# ==============================================================================
# GAIN SEMANTIC FUNNEL  (additive enrichment - keyword -> embedding -> LLM)
#
# Does NOT touch the existing layers. Reads the newest enriched file, adds
# semantic + LLM columns, writes GAIN_EVIDENCE_ENRICHED_[date]_SEM.csv (which
# crossref/Power BI then auto-pick up as newest). All original columns kept.
#
# Funnel:
#   1. KEYWORD pass  - already done by GAIN_ENRICH_EVIDENCE.R (we start from it)
#   2. EMBEDDING     - embed each survivor, cosine to the 413 GAIN examples
#                      (seed positives). Fast (~0.1s each), cached.
#   3. LLM EXTRACT   - extract_evidence() ONLY on gated survivors (~100s each).
#                      Gate = not suppressed + has a displacement signal +
#                      similarity >= GATE_SIM, per-domain capped, total capped to
#                      MAX_EXTRACTS (the "modest, ~3 hour" budget).
#
# Idempotent & resumable: every extract + embed is content-hash cached, so a
# rerun never recalls the model and an interrupted run just continues.
#
#   Run order:  ENRICH -> (this) -> CROSSREF -> POWERBI
# ==============================================================================

suppressMessages({ library(tidyverse); library(rlang) })
source("shared/GAIN_OLLAMA_HELPERS.R")

stamp <- format(Sys.Date(), "%Y%m%d")
GATE_SIM        <- as.numeric(Sys.getenv("GAIN_GATE_SIM", "0.60"))  # cosine cutoff
MAX_EXTRACTS    <- as.integer(Sys.getenv("GAIN_MAX_EXTRACTS", "110")) # ~3h @ ~100s
MAX_PER_DOMAIN  <- as.integer(Sys.getenv("GAIN_MAX_PER_DOMAIN", "25"))
# READ-EVERYTHING mode: when GAIN_READ_ALL=1, the AI reads EVERY non-suppressed
# candidate that has readable text - the similarity gate and keyword-signal floor
# are bypassed, so novel inclusion that doesn't resemble the known GAIN examples
# still gets read. Low-value docs simply come back with low llm_relevance, so the
# evidence is not polluted. Default 0 = unchanged behaviour.
READ_ALL        <- Sys.getenv("GAIN_READ_ALL", "0") == "1"
EVIDENCE_CACHE  <- "evidence_cache"   # where ENRICH cached fetched documents

if (!ollama_available()) {
  stop("Ollama not reachable / models missing. Start Ollama (qwen2.5:7b + bge-m3) and retry.")
}

pick <- function(df, col, default = NA) if (col %in% names(df)) df[[col]] else default

# ------------------------------------------------------------------------------
# Load newest enriched (skip our own _SEM output so reruns re-read the base)
# ------------------------------------------------------------------------------
# read the most recently WRITTEN base enrich (exclude our own _SEM output), by mtime
enr_cand <- list.files(pattern = "^GAIN_EVIDENCE_ENRICHED_\\d+(_LLM)?\\.csv$")
enr_f <- if (length(enr_cand)) enr_cand[which.max(file.info(enr_cand)$mtime)] else NA
if (is.na(enr_f)) stop("No GAIN_EVIDENCE_ENRICHED_*.csv - run GAIN_ENRICH_EVIDENCE.R first.")
enr <- read_csv(enr_f, show_col_types = FALSE)
message(paste("Funnel input:", enr_f, "-", nrow(enr), "records"))

# ------------------------------------------------------------------------------
# 1. SEED POSITIVES - embed the 413 confirmed GAIN examples (cached)
# ------------------------------------------------------------------------------
message("Embedding GAIN examples as seed positives...")
gex <- read_csv("analysis_ready_group_roster.csv", show_col_types = FALSE) %>%
  filter(!is.na(PRO03)) %>%
  transmute(
    gain_index = index,
    seed_text = pmap_chr(list(PRO03, morganization, mcountry,
      coalesce(PRO07.A, 0) == 1, coalesce(PRO07.B, 0) == 1, coalesce(PRO07.C, 0) == 1,
      PRO08_label), gain_seed_text)   # shared builder -> cache shared with crossref
  )
seed_vecs <- map(gex$seed_text, embed)
keep_seed <- !map_lgl(seed_vecs, is.null)
seed_vecs <- seed_vecs[keep_seed]; seed_idx <- gex$gain_index[keep_seed]
seed_mat  <- do.call(rbind, seed_vecs)                  # n_seed x dim
seed_norm <- seed_mat / sqrt(rowSums(seed_mat^2))
message(paste("  seed vectors:", nrow(seed_mat)))

# ------------------------------------------------------------------------------
# 2. EMBED survivors + nearest-seed cosine (fast, cached)
# ------------------------------------------------------------------------------
# record text for embedding (shared builder -> cache shared with crossref)
rec_text <- function(i) record_embed_text(
  pick(enr, "title")[i], pick(enr, "extract_summary")[i],
  pick(enr, "context_refugee")[i], pick(enr, "context_idp")[i],
  pick(enr, "context_stateless")[i], pick(enr, "english_working_summary")[i])

message("Embedding survivors + nearest-GAIN-example similarity...")
sim <- rep(NA_real_, nrow(enr)); near <- rep(NA, nrow(enr))
for (i in seq_len(nrow(enr))) {
  v <- embed(rec_text(i))
  if (is.null(v)) next
  cs <- as.numeric(seed_norm %*% (v / sqrt(sum(v^2))))
  sim[i] <- max(cs); near[i] <- seed_idx[which.max(cs)]
  if (i %% 50 == 0) message(sprintf("  embedded %d/%d", i, nrow(enr)))
}
enr$sem_similarity   <- round(sim, 4)
enr$nearest_gain_index <- near

# ------------------------------------------------------------------------------
# 3. GATE -> the records that earn an LLM extract
# ------------------------------------------------------------------------------
cat_l   <- str_sub(coalesce(pick(enr, "outreach_category"), "?"), 1, 1)
has_sig <- coalesce(pick(enr, "core_strong"), pick(enr, "core_mentions"), 0) > 0 |
           coalesce(pick(enr, "has_inclusion"), FALSE) |
           coalesce(pick(enr, "mentions_egriss"), 0) > 0
priority_blend <- coalesce(pick(enr, "relevance_score"), 0) + 100 * coalesce(sim, 0)

gate_df <- tibble(.row = seq_len(nrow(enr)),
                  domain = coalesce(pick(enr, "producer"), pick(enr, "country"), "?"),
                  sim = sim, blend = priority_blend,
                  eligible = cat_l != "E" & !is.na(sim) &
                             (READ_ALL | (has_sig & sim >= GATE_SIM)))

gated <- gate_df %>% filter(eligible) %>%
  arrange(desc(blend)) %>%
  group_by(domain) %>% slice_head(n = MAX_PER_DOMAIN) %>% ungroup() %>%
  arrange(desc(blend)) %>% slice_head(n = MAX_EXTRACTS)
message(sprintf("Gate: %d eligible -> %d selected for LLM extract (sim>=%.2f, <=%d/domain, <=%d total)",
                sum(gate_df$eligible), nrow(gated), GATE_SIM, MAX_PER_DOMAIN, MAX_EXTRACTS))

# ------------------------------------------------------------------------------
# 4. LLM EXTRACT on the gated set (full cached doc text if available)
# ------------------------------------------------------------------------------
doc_text <- function(url, fallback) {
  f <- file.path(EVIDENCE_CACHE, paste0(rlang::hash(url), ".rds"))
  if (file.exists(f)) {
    d <- tryCatch(readRDS(f), error = function(e) NULL)
    # v4: smart_excerpt (from the sourced helpers) selects keyword-dense windows
    # from the FULL document instead of its first N characters - the methodology
    # section of a long report reaches the model wherever it sits in the text.
    if (!is.null(d) && length(d$pages) > 0)
      return(smart_excerpt(paste(d$pages, collapse = " ")))
  }
  fallback
}

# initialise llm_* columns (NA for non-gated rows)
llm_cols <- c("llm_relevance","llm_counted","llm_implementation_status","llm_population",
              "llm_organization","llm_instrument_or_title","llm_year","llm_lead_type",
              "llm_is_humanitarian","llm_recommendation_referenced","llm_quote",
              "llm_confidence","llm_extracted")
for (c in llm_cols) enr[[c]] <- NA
enr$llm_extracted <- FALSE

message(paste("Running extract_evidence on", nrow(gated), "records (cached; resumable)..."))
for (k in seq_len(nrow(gated))) {
  i <- gated$.row[k]
  txt <- doc_text(enr$url[i], rec_text(i))
  e <- extract_evidence(txt, title = pick(enr, "title")[i], country = enr$country[i])
  if (is.null(e)) next
  enr$llm_relevance[i] <- e$relevance_score
  enr$llm_counted[i] <- e$counted
  enr$llm_implementation_status[i] <- e$implementation_status
  enr$llm_population[i] <- e$population
  enr$llm_organization[i] <- e$organization
  enr$llm_instrument_or_title[i] <- e$instrument_or_title
  enr$llm_year[i] <- e$year
  enr$llm_lead_type[i] <- e$lead_type
  enr$llm_is_humanitarian[i] <- e$is_humanitarian_data
  enr$llm_recommendation_referenced[i] <- e$recommendation_referenced
  enr$llm_quote[i] <- e$evidence_quote
  enr$llm_confidence[i] <- e$confidence
  enr$llm_extracted[i] <- TRUE
  if (k %% 10 == 0) message(sprintf("  extracted %d/%d", k, nrow(gated)))
}

# ------------------------------------------------------------------------------
# 5. Compare LLM vs rule-based lead + humanitarian (the "compare results" ask)
# ------------------------------------------------------------------------------
rule_lead <- str_replace(coalesce(pick(enr, "candidate_lead_type"), ""),
                         "^likely ", "")            # "likely country-led" -> "country-led"
enr$lead_agreement <- case_when(
  is.na(enr$llm_lead_type)                          ~ NA_character_,
  enr$llm_lead_type == rule_lead                    ~ "agree",
  rule_lead %in% c("country-led","partner-led") &
    enr$llm_lead_type %in% c("country-led","partner-led") ~ "DISAGREE",
  TRUE                                              ~ "one-unclear")
enr$humanitarian_agreement <- case_when(
  is.na(enr$llm_is_humanitarian)                    ~ NA_character_,
  enr$llm_is_humanitarian == coalesce(pick(enr,"is_humanitarian"), FALSE) ~ "agree",
  TRUE                                              ~ "DISAGREE")

out_f <- paste0("GAIN_EVIDENCE_ENRICHED_", stamp, "_SEM.csv")
write_excel_csv(enr, out_f)

message(paste0("\nWritten: ", out_f))
message(sprintf("  embedded: %d | LLM-extracted: %d | mean sim of extracted: %.2f",
                sum(!is.na(enr$sem_similarity)), sum(enr$llm_extracted, na.rm = TRUE),
                mean(enr$sem_similarity[enr$llm_extracted], na.rm = TRUE)))
if (sum(enr$llm_extracted, na.rm = TRUE) > 0) {
  message("  lead agreement (LLM vs rule): ",
          paste(names(table(enr$lead_agreement)), table(enr$lead_agreement), sep="=", collapse=" "))
  message("  LLM-confirmed counted=TRUE: ", sum(enr$llm_counted == TRUE, na.rm = TRUE))
}
message("Next: source('GAIN_PHASE5_CROSSREF.R') then GAIN_POWERBI_EXPORT.R")
