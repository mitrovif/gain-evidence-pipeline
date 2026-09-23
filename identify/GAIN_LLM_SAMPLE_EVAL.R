# ==============================================================================
# GAIN LLM SAMPLE EVAL  (v2 vs v3 on a stratified sample - run before the full
# re-extraction to validate the fixes and time the real thing)
#
# Picks ~15 documents that were LLM-scored under the v2 regime (6000-char
# truncation, num_ctx 2048, 300s timeout) and re-scores them fresh under v3
# (20000 chars, num_ctx 8192, 600s + retry). Includes the Canada resettlement
# articles whose methodology sat past the old cut-off - the cases that exposed
# the bug. Writes one row per document AS IT FINISHES to GAIN_LLM_SAMPLE_EVAL.csv
# so partial results are usable, and prints a v2-vs-v3 comparison at the end.
#
# Results are cached under the v3 keys, so NOTHING here is wasted - the full
# re-run later gets these documents for free.
# ==============================================================================

suppressMessages({ library(tidyverse) })
source("shared/GAIN_COMMON.R")
source("shared/GAIN_OLLAMA_HELPERS.R")
if (!ollama_available()) stop("Ollama is not reachable - start it first.")

OUT <- "GAIN_LLM_SAMPLE_EVAL.csv"

# ---- the v2 results to compare against ---------------------------------------
sem_f <- newest_file("^GAIN_EVIDENCE_ENRICHED_\\d{8}_SEM\\.csv$")
if (is.na(sem_f)) stop("No _SEM file found.")
sem <- read_csv(sem_f, show_col_types = FALSE)
message("v2 baseline: ", sem_f, " (", sum(sem$llm_extracted, na.rm = TRUE), " v2-extracted rows)")

ex <- sem %>% filter(llm_extracted %in% TRUE) %>%
  mutate(v2_rel = suppressWarnings(as.numeric(llm_relevance)))

# ---- stratified sample --------------------------------------------------------
canada <- ex %>% filter(country == "Canada",
                        str_detect(str_to_lower(coalesce(title, "")), "refugee|resettl"))
strata <- bind_rows(
  ex %>% filter(v2_rel >= 70) %>% slice_sample(n = 3),
  ex %>% filter(v2_rel >= 40, v2_rel < 70) %>% slice_sample(n = 3),
  ex %>% filter(v2_rel < 40) %>% slice_sample(n = 3),
  ex %>% filter(llm_counted %in% TRUE)  %>% slice_sample(n = 2),
  ex %>% filter(llm_counted %in% FALSE) %>% slice_sample(n = 2))
# plus 2 that were gated-quality but got NO v2 verdict (the silent-failure class)
failed <- sem %>% filter(!llm_extracted %in% TRUE,
                         suppressWarnings(as.numeric(sem_similarity)) >= 0.60) %>%
  arrange(desc(suppressWarnings(as.numeric(sem_similarity)))) %>% slice_head(n = 2) %>%
  mutate(v2_rel = NA_real_)

smp <- bind_rows(canada, strata, failed) %>% distinct(url, .keep_all = TRUE)
message("Sample: ", nrow(smp), " documents (incl. ", nrow(canada), " Canada bug-cases, ",
        nrow(failed), " v2 silent-failures)")

# ---- doc text exactly as the funnel builds it (v4: smart excerpting) -----------
doc_text <- function(url) {
  f <- file.path("evidence_cache", paste0(rlang::hash(url), ".rds"))
  if (!file.exists(f)) return(NA_character_)
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || length(d$pages) == 0) return(NA_character_)
  smart_excerpt(paste(d$pages, collapse = " "))
}

# ---- run, appending each result as it lands ------------------------------------
if (file.exists(OUT)) file.remove(OUT)
for (i in seq_len(nrow(smp))) {
  r <- smp[i, ]
  txt <- doc_text(r$url)
  if (is.na(txt)) { message("[", i, "] no cached text - skipped: ", substr(r$title, 1, 50)); next }
  t0 <- Sys.time()
  e  <- extract_evidence(txt, title = r$title, country = r$country)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  row <- tibble(
    country = r$country, title = substr(coalesce(r$title, ""), 1, 70),
    doc_chars = nchar(txt),
    v2_relevance = r$v2_rel,
    v2_counted   = as.logical(r$llm_counted),
    v2_confidence = as.character(r$llm_confidence),
    v3_relevance = if (is.null(e)) NA else e$relevance_score,
    v3_counted   = if (is.null(e)) NA else e$counted,
    v3_implementation = if (is.null(e)) NA else e$implementation_status,
    v3_confidence = if (is.null(e)) NA else e$confidence,
    v3_quote = if (is.null(e)) "" else substr(e$evidence_quote, 1, 120),
    status = if (is.null(e)) "FAILED" else "ok",
    secs = secs)
  write_csv(row, OUT, append = file.exists(OUT))
  message(sprintf("[%d/%d] %s | %s | v2=%s -> v3=%s | counted %s->%s | %ss",
                  i, nrow(smp), r$country, substr(r$title, 1, 40),
                  coalesce(as.character(r$v2_rel), "NA"),
                  coalesce(as.character(row$v3_relevance), "FAIL"),
                  coalesce(as.character(row$v2_counted), "NA"),
                  coalesce(as.character(row$v3_counted), "NA"), secs))
}

# ---- summary -------------------------------------------------------------------
res <- read_csv(OUT, show_col_types = FALSE)
message("\n==================== SAMPLE EVAL (v2 -> v3) ====================")
message("documents: ", nrow(res), " | failures: ", sum(res$status != "ok"),
        " | median secs/doc: ", round(median(res$secs, na.rm = TRUE)))
ok <- res %>% filter(status == "ok", !is.na(v2_relevance))
if (nrow(ok)) {
  message("relevance shift (v3 - v2): median ",
          round(median(ok$v3_relevance - ok$v2_relevance, na.rm = TRUE)),
          " | counted flips FALSE->TRUE: ",
          sum(!ok$v2_counted %in% TRUE & ok$v3_counted %in% TRUE, na.rm = TRUE),
          " | TRUE->FALSE: ",
          sum(ok$v2_counted %in% TRUE & !ok$v3_counted %in% TRUE, na.rm = TRUE))
}
message("Full table: ", OUT)
