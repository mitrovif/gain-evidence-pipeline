# ==============================================================================
# GAIN PHASE 5 - CROSS-REFERENCING ENGINE
#
# Joins four data sources:
#   1. Web evidence        (output of NSO crawler, e.g. GAIN_CLEAN_DEDUP_*.csv)
#   2. GAIN responses      (analysis_ready_main_roster.csv)
#   3. GAIN projects       (analysis_ready_group_roster.csv)
#   4. Contact list        (GAIN Data Collection 2025_Email Link to 2024.xlsx)
#
# Outputs (CSV):
#   A. evidence_flagged_[date].csv      - each evidence row: IN_GAIN / POSSIBLE / NEW
#   B. country_priority_matrix_[date].csv - respondent status x evidence
#   C. contact_gaps_[date].csv          - priority countries missing contacts
#   D. suggested_respondents_[date].csv - ranked contacts per target NSO by title fit
# ==============================================================================

library(tidyverse)
library(readxl)
library(stringr)

# Shared helpers: safe_write() + harmonize_country() now live in GAIN_COMMON.R
# (single source of truth - the outreach scripts use the SAME country mappings,
# which is what keeps the roster/contacts/log joins consistent).
source("shared/GAIN_COMMON.R")

# ------------------------------------------------------------------------------
# CONFIG - adjust filenames here
# ------------------------------------------------------------------------------
# Auto-find the most recently WRITTEN enriched file (by mtime, not filename) so a
# fresh enrich is never shadowed by an older _SEM/_LLM variant that sorts later.
EVIDENCE_FILE <- {
  cand <- list.files(pattern = "^GAIN_EVIDENCE_ENRICHED_.*\\.csv$")
  sem  <- cand[grepl("_SEM\\.csv$", cand)]    # carries llm_*/sem_similarity columns
  base <- cand[!grepl("_SEM\\.csv$", cand)]
  # PREFER the newest _SEM file: it has the LLM fields. Falling back to a base
  # enrich (no llm_*) would silently drop every llm_/sem_similarity column and
  # make them all NA in the dashboard. Only use base if no _SEM exists at all.
  if (length(sem) > 0)        sem[which.max(file.info(sem)$mtime)]
  else if (length(base) > 0)  base[which.max(file.info(base)$mtime)]
  else                        "GAIN_CLEAN_DEDUP_20260610.csv"
}
MAIN_ROSTER     <- "analysis_ready_main_roster.csv"
GROUP_ROSTER    <- "analysis_ready_group_roster.csv"
CONTACT_FILE    <- "GAIN Data Collection 2025_Email Link to 2024.xlsx"
CURRENT_CYCLE   <- 2024   # "active" = responded in this year or later
YEAR_TOLERANCE  <- 1      # evidence year vs project year window

stamp <- format(Sys.Date(), "%Y%m%d")

# harmonize_country() comes from GAIN_COMMON.R (sourced above). Extend the
# mapping table THERE as new spellings appear in the unmatched report below -
# the outreach scripts share it, so a fix here-and-only-here is no longer possible.

# Multi-country / regional datasets ("Austria, Cyprus, Germany...and 2 more",
# "Europe and Central Asia") cannot be matched to one GAIN respondent country.
REGION_NAMES <- c("Europe and Central Asia", "Sub-Saharan Africa",
                  "Middle East and North Africa", "East Asia and Pacific",
                  "Latin America and the Caribbean", "South Asia", "World")
is_multi_country <- function(x) {
  str_detect(coalesce(x, ""), ",|\\.\\.\\.and \\d+ more") | x %in% REGION_NAMES
}

# ==============================================================================
# LOAD DATA
# ==============================================================================
message("Loading inputs...")

main_roster <- read_csv(MAIN_ROSTER, show_col_types = FALSE) %>%
  mutate(mcountry = harmonize_country(mcountry))

group_roster <- read_csv(GROUP_ROSTER, show_col_types = FALSE) %>%
  mutate(mcountry = harmonize_country(mcountry))

contacts <- read_excel(CONTACT_FILE, sheet = "Sample Survey") %>%
  rename_with(~ str_replace_all(.x, "[ ()]+", "_")) %>%
  mutate(Country = harmonize_country(Country))

has_evidence <- file.exists(EVIDENCE_FILE)
if (has_evidence) {
  evidence <- read_csv(EVIDENCE_FILE, show_col_types = FALSE)
  # Enriched-pipeline format (lowercase columns): map to the names used here,
  # keeping ALL enrichment columns (lead type, outreach category, contacts...)
  if ("country" %in% names(evidence) && !"Country" %in% names(evidence)) {
    evidence <- evidence %>%
      mutate(Country          = country,
             Report_Title     = title,
             Populations      = populations,
             Publication_Date = as.character(year),
             Found_On_Page    = url)
  }
  evidence <- evidence %>% mutate(Country = harmonize_country(Country))
  # Suppressed records (category E: false positives/spam) are excluded from
  # all outreach analysis but remain in the enriched CSV for audit.
  if ("outreach_category" %in% names(evidence)) {
    n0 <- nrow(evidence)
    evidence <- evidence %>%
      filter(!str_starts(coalesce(outreach_category, ""), "E"))
    message(paste("  Suppressed (category E) records excluded:", n0 - nrow(evidence)))
  }
  message(paste("  Evidence rows:", nrow(evidence), "from", EVIDENCE_FILE))
  if (!"llm_relevance" %in% names(evidence))
    message("  WARNING: this evidence file has NO llm_* columns -> the dashboard's\n",
            "  AI fields (llm_*, sem_similarity) will be NA. Run the semantic funnel\n",
            "  (DO_LLM=TRUE in RUN_ALL.R) to produce a _SEM file, then re-run this.")
  else
    message(paste("  LLM fields present:",
                  sum(!is.na(evidence$llm_relevance)), "of", nrow(evidence),
                  "rows have an llm_relevance value"))
} else {
  message(paste("  NOTE:", EVIDENCE_FILE, "not found - Modules A/B run in status-only mode"))
  evidence <- tibble(Country = character(), Report_Title = character(),
                     Populations = character(), Publication_Date = character(),
                     Found_On_Page = character())
}

# ==============================================================================
# RESPONDENT STATUS - per country, NSOs only (LOC01 == 1)
# ==============================================================================
message("Building respondent status...")

nso_responses <- main_roster %>%
  filter(LOC01 == 1, !is.na(mcountry))

respondent_status <- nso_responses %>%
  group_by(mcountry) %>%
  summarise(
    first_response = min(year, na.rm = TRUE),
    last_response  = max(year, na.rm = TRUE),
    n_responses    = n(),
    .groups = "drop"
  ) %>%
  mutate(status = if_else(last_response >= CURRENT_CYCLE, "ACTIVE", "LAPSED"))

# Countries in contact list but never in roster = NEVER responded
never_responded <- contacts %>%
  filter(!is.na(Country), !Country %in% respondent_status$mcountry,
         Country != "INST") %>%
  distinct(Country) %>%
  mutate(status = "NEVER", first_response = NA, last_response = NA, n_responses = 0)

status_all <- bind_rows(
  respondent_status,
  never_responded %>% rename(mcountry = Country)
)

message(paste("  ACTIVE:", sum(status_all$status == "ACTIVE"),
              "| LAPSED:", sum(status_all$status == "LAPSED"),
              "| NEVER:",  sum(status_all$status == "NEVER")))

# ==============================================================================
# MODULE A - FLAG EVIDENCE AGAINST GAIN PROJECTS
# Match: country + population overlap + year window
# ==============================================================================
message("\nModule A: flagging evidence against GAIN projects...")

# GAIN project table: one row per project with populations and year span
gain_projects <- group_roster %>%
  transmute(
    mcountry,
    project_title = PRO03,
    start_year    = suppressWarnings(as.numeric(PRO04_year)),
    end_year      = suppressWarnings(as.numeric(PRO05_year)),
    has_refugees  = coalesce(PRO07.A, 0) == 1,
    has_idps      = coalesce(PRO07.B, 0) == 1,
    has_stateless = coalesce(PRO07.C, 0) == 1
  ) %>%
  filter(!is.na(mcountry))

flag_one <- function(country, pops, pub_date) {
  cand <- gain_projects %>% filter(mcountry == country)
  if (nrow(cand) == 0) return("NEW")

  pops_l <- str_to_lower(coalesce(pops, ""))
  ev_ref <- str_detect(pops_l, "refugee")
  ev_idp <- str_detect(pops_l, "displaced|idp")
  ev_sta <- str_detect(pops_l, "stateless")

  ev_year <- suppressWarnings(as.numeric(str_extract(coalesce(pub_date, ""), "20\\d{2}")))

  pop_match <- (ev_ref & cand$has_refugees) |
               (ev_idp & cand$has_idps) |
               (ev_sta & cand$has_stateless)

  year_match <- if (is.na(ev_year)) {
    rep(TRUE, nrow(cand))   # no date on evidence -> don't penalize
  } else {
    s <- coalesce(cand$start_year, ev_year)
    e <- coalesce(cand$end_year, s)
    ev_year >= (s - YEAR_TOLERANCE) & ev_year <= (e + YEAR_TOLERANCE)
  }

  if (any(pop_match & year_match)) "IN_GAIN"
  else if (any(pop_match)) "POSSIBLE"
  else "POSSIBLE"   # country reports to GAIN but different population: review
}

if (has_evidence && nrow(evidence) > 0) {
  evidence_flagged <- evidence %>%
    mutate(gain_flag = pmap_chr(
      list(Country, Populations, Publication_Date), flag_one
    )) %>%
    left_join(status_all %>% select(mcountry, respondent_status = status),
              by = c("Country" = "mcountry")) %>%
    mutate(respondent_status = replace_na(respondent_status, "NEVER"),
           # regional/multi-country datasets need per-country review, not a flag
           gain_flag = if_else(is_multi_country(Country),
                               "MULTI-COUNTRY dataset - review per country",
                               gain_flag))

  # Where the country already has an ACTIVE GAIN respondent, route strong
  # candidates through the existing GAIN focal point instead of cold outreach.
  if ("recommended_outreach_route" %in% names(evidence_flagged)) {
    evidence_flagged <- evidence_flagged %>%
      mutate(recommended_outreach_route = if_else(
        respondent_status == "ACTIVE" &
          str_detect(coalesce(outreach_category, ""), "^[AB]") &
          recommended_outreach_route %in% c("NSO direct",
                                            "partner-supported follow-up"),
        "existing GAIN focal point",
        recommended_outreach_route))
  }

  # ============================================================================
  # MODULE A2 - EXAMPLE-LEVEL GAIN MATCH (additive; needs Ollama + funnel output)
  # Embedding nearest-neighbour to GAIN examples IN THE SAME COUNTRY + qwen
  # adjudication on near matches -> already_in_gain / new_example_existing_country
  # / new_country, with matched_gain_id + confidence. Uncertain rows -> review_queue.
  # Skips cleanly (columns simply absent) if Ollama is down, so the country-level
  # gain_flag and the rest of Phase 5 are never affected.
  # ============================================================================
  ex_ok <- file.exists("shared/GAIN_OLLAMA_HELPERS.R") &&
           Sys.getenv("GAIN_SKIP_EXAMPLE_MATCH") != "1"   # RUN_ALL sets this when DO_LLM=FALSE
  if (ex_ok) { suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R")); ex_ok <- ollama_available() }
  if (ex_ok) {
    message("\nModule A2: example-level GAIN matching...")
    EX_HIGH_SIM <- as.numeric(Sys.getenv("GAIN_EX_HIGH_SIM", "0.85"))
    EX_BAND_LOW <- as.numeric(Sys.getenv("GAIN_EX_BAND_LOW", "0.62"))
    MAX_ADJ     <- as.integer(Sys.getenv("GAIN_MAX_ADJUDICATIONS", "200"))

    gex <- group_roster %>% filter(!is.na(PRO03)) %>%
      transmute(gain_index = index, gain_pindex2 = pindex2, ex_country = mcountry, ex_org = morganization,
        ex_title = PRO03, ex_year = as.character(PRO04_year),
        ex_pop = str_squish(paste(if_else(coalesce(PRO07.A,0)==1,"refugees",""),
          if_else(coalesce(PRO07.B,0)==1,"idps",""),
          if_else(coalesce(PRO07.C,0)==1,"stateless",""))),
        seed_text = pmap_chr(list(PRO03, morganization, mcountry,
          coalesce(PRO07.A,0)==1, coalesce(PRO07.B,0)==1, coalesce(PRO07.C,0)==1,
          PRO08_label), gain_seed_text))
    gex$vec <- map(gex$seed_text, embed)
    gex <- gex %>% filter(!map_lgl(vec, is.null))
    message(paste("  GAIN examples embedded:", nrow(gex)))

    pk <- function(col) if (col %in% names(evidence_flagged)) evidence_flagged[[col]] else rep(NA, nrow(evidence_flagged))
    n <- nrow(evidence_flagged)
    mtype <- rep(NA_character_, n); mid <- rep(NA_character_, n)
    mconf <- rep(NA_character_, n); mreason <- rep(NA_character_, n)
    mpindex <- rep(NA_character_, n); mtitle <- rep(NA_character_, n)   # matched GAIN example id + title
    adj_used <- 0
    for (i in seq_len(n)) {
      if (isTRUE(is_multi_country(evidence_flagged$Country[i]))) {
        mtype[i] <- "multi-country - review per country"; next }
      ctry <- evidence_flagged$Country[i]
      same <- gex %>% filter(ex_country == ctry)
      if (nrow(same) == 0) { mtype[i] <- "new_country"; mconf[i] <- "high"; next }
      av <- embed(record_embed_text(pk("title")[i], pk("extract_summary")[i],
        pk("context_refugee")[i], pk("context_idp")[i], pk("context_stateless")[i],
        pk("english_working_summary")[i]))
      if (is.null(av)) { mtype[i] <- "new_example_existing_country"; mconf[i] <- "low"
        mreason[i] <- "no artifact embedding"; next }
      sims <- map_dbl(same$vec, ~ cosine_sim(av, .x))
      bi <- which.max(sims); bs <- sims[bi]; bex <- same[bi, ]
      if (bs >= EX_BAND_LOW && adj_used < MAX_ADJ) {
        # all fields forced to character: the SEM file may read llm_year as <dbl>,
        # which would clash with the <chr> fallback inside coalesce (vctrs error).
        artifact <- list(country = as.character(ctry),
          organization = coalesce(as.character(pk("llm_organization")[i]), as.character(pk("producer")[i])),
          instrument_or_title = coalesce(as.character(pk("llm_instrument_or_title")[i]), as.character(evidence_flagged$Report_Title[i])),
          year = coalesce(as.character(pk("llm_year")[i]), as.character(evidence_flagged$Publication_Date[i])),
          population = coalesce(as.character(pk("llm_population")[i]), as.character(evidence_flagged$Populations[i])))
        example <- list(country = bex$ex_country, organization = bex$ex_org,
                        title = bex$ex_title, year = bex$ex_year, population = bex$ex_pop)
        adj <- adjudicate_match(artifact, example); adj_used <- adj_used + 1
        if (isTRUE(adj$same_example)) {
          mtype[i] <- "already_in_gain"; mid[i] <- as.character(bex$gain_index)
          mpindex[i] <- as.character(bex$gain_pindex2); mtitle[i] <- as.character(bex$ex_title)
          mconf[i] <- adj$confidence; mreason[i] <- adj$reason
        } else {
          mtype[i] <- "new_example_existing_country"
          mconf[i] <- adj$confidence; mreason[i] <- adj$reason
        }
      } else {
        mtype[i] <- "new_example_existing_country"; mconf[i] <- "low"
        mreason[i] <- if (bs < EX_BAND_LOW) sprintf("nearest example sim %.2f below band", bs)
                      else "adjudication cap reached"
      }
      if (i %% 50 == 0) message(sprintf("  matched %d/%d (adjudications: %d)", i, n, adj_used))
    }
    evidence_flagged$gain_match_type       <- mtype
    evidence_flagged$matched_gain_id       <- mid
    evidence_flagged$matched_gain_pindex2  <- mpindex
    evidence_flagged$matched_gain_title    <- mtitle
    evidence_flagged$gain_match_confidence <- mconf
    evidence_flagged$gain_match_reason     <- mreason
    message(sprintf("  already_in_gain: %d | new_example_existing_country: %d | new_country: %d (adjudications used: %d)",
      sum(mtype == "already_in_gain", na.rm = TRUE),
      sum(mtype == "new_example_existing_country", na.rm = TRUE),
      sum(mtype == "new_country", na.rm = TRUE), adj_used))
    # embedding failures are indistinguishable from genuine "doesn't match"
    # results downstream unless a reviewer reads gain_match_reason - surface
    # the count so a bad/flaky Ollama run doesn't silently masquerade as data
    n_embed_fail <- sum(mreason == "no artifact embedding", na.rm = TRUE)
    if (n_embed_fail > 0)
      message("  NOTE: ", n_embed_fail, " record(s) got no artifact embedding ",
              "(Ollama hiccup or empty text) and were defaulted to low-confidence ",
              "new_example_existing_country - check gain_match_reason before trusting these.")

    # review queue: new examples in existing countries + low-confidence matches
    rq <- evidence_flagged %>%
      filter(gain_match_type == "new_example_existing_country" |
             (gain_match_type == "already_in_gain" & gain_match_confidence == "low")) %>%
      select(any_of(c("Country","Report_Title","Found_On_Page","gain_match_type",
        "matched_gain_id","gain_match_confidence","gain_match_reason",
        "outreach_category","candidate_lead_type","relevance_score","sem_similarity",
        "llm_relevance","llm_counted","recommended_primary_contact")))
    safe_write(rq, paste0("review_queue_", stamp, ".csv"))
    message(paste("  review_queue rows:", nrow(rq), "-> review_queue_", stamp, ".csv"))
  } else {
    why <- if (Sys.getenv("GAIN_SKIP_EXAMPLE_MATCH") == "1") "DO_LLM off"
           else if (!file.exists("shared/GAIN_OLLAMA_HELPERS.R")) "helper file missing"
           else "Ollama not reachable"
    message(sprintf("\nModule A2 skipped (%s) - example-level columns not added.", why))
  }

  safe_write(evidence_flagged, paste0("evidence_flagged_", stamp, ".csv"))
  message(paste("  IN_GAIN:",  sum(evidence_flagged$gain_flag == "IN_GAIN"),
                "| POSSIBLE:", sum(evidence_flagged$gain_flag == "POSSIBLE"),
                "| NEW:",      sum(evidence_flagged$gain_flag == "NEW")))
} else {
  evidence_flagged <- tibble()
}

# ==============================================================================
# MODULE B - COUNTRY PRIORITY MATRIX
# ==============================================================================
message("\nModule B: building priority matrix...")

first_non_na <- function(x) { y <- x[!is.na(x)]; if (length(y) == 0) NA_character_ else y[1] }

evidence_summary <- if (nrow(evidence_flagged) > 0) {
  # make sure enrichment columns exist even with a legacy evidence file
  for (col in c("outreach_category", "candidate_lead_type",
                "recommended_primary_contact", "recommended_primary_contact_type",
                "contact_confidence", "contact_validation_needed",
                "recommended_outreach_route")) {
    if (!col %in% names(evidence_flagged)) evidence_flagged[[col]] <- NA
  }
  evidence_flagged %>%
    group_by(Country) %>%
    summarise(
      n_evidence     = n(),
      n_new_evidence = sum(gain_flag == "NEW"),
      n_strong_nso_candidates = sum(str_starts(coalesce(as.character(outreach_category), ""), "A")),
      n_partner_or_unclear    = sum(str_starts(coalesce(as.character(outreach_category), ""), "B")),
      n_manual_review         = sum(str_starts(coalesce(as.character(outreach_category), ""), "D")),
      web_discovered_contact      = first_non_na(as.character(recommended_primary_contact)),
      web_discovered_contact_type = first_non_na(as.character(recommended_primary_contact_type)),
      web_contact_confidence      = first_non_na(as.character(contact_confidence)),
      example_release = first(Report_Title),
      example_url     = first(Found_On_Page),
      .groups = "drop"
    )
} else {
  tibble(Country = character(), n_evidence = integer(),
         n_new_evidence = integer(), n_strong_nso_candidates = integer(),
         n_partner_or_unclear = integer(), n_manual_review = integer(),
         web_discovered_contact = character(),
         web_discovered_contact_type = character(),
         web_contact_confidence = character(),
         example_release = character(), example_url = character())
}

priority_matrix <- status_all %>%
  left_join(evidence_summary, by = c("mcountry" = "Country")) %>%
  mutate(
    n_evidence     = replace_na(n_evidence, 0L),
    n_new_evidence = replace_na(n_new_evidence, 0L),
    n_strong_nso_candidates = replace_na(n_strong_nso_candidates, 0L),
    n_partner_or_unclear    = replace_na(n_partner_or_unclear, 0L),
    n_manual_review         = replace_na(n_manual_review, 0L),
    priority = case_when(
      status == "NEVER"  & n_new_evidence > 0 ~ "P1: Cold outreach WITH evidence hook",
      status == "LAPSED" & n_new_evidence > 0 ~ "P2: Re-engage WITH evidence hook",
      status == "ACTIVE" & n_new_evidence > 0 ~ "P3: Follow-up - evidence not yet reported",
      status == "LAPSED"                      ~ "P4: Re-engage (generic)",
      status == "NEVER"                       ~ "P5: Cold outreach (generic)",
      TRUE                                    ~ "P6: Active - no action"
    )
  ) %>%
  arrange(priority, desc(n_strong_nso_candidates), desc(n_new_evidence))

safe_write(priority_matrix, paste0("country_priority_matrix_", stamp, ".csv"))
message("  Priority breakdown:")
print(count(priority_matrix, priority))

# ==============================================================================
# MODULE C - CONTACT GAPS
# ==============================================================================
message("\nModule C: contact gap analysis...")

contact_coverage <- contacts %>%
  filter(!is.na(Country), Country != "INST") %>%
  group_by(Country) %>%
  summarise(
    n_contacts        = n(),
    n_with_position   = sum(!is.na(Position)),
    n_with_email      = sum(!is.na(Emails_List)),
    .groups = "drop"
  )

contact_gaps <- priority_matrix %>%
  filter(str_starts(priority, "P1|P2|P3")) %>%
  left_join(contact_coverage, by = c("mcountry" = "Country")) %>%
  mutate(across(c(n_contacts, n_with_position, n_with_email),
                ~ replace_na(.x, 0L)),
         gap = case_when(
           n_contacts == 0 & !is.na(web_discovered_contact) ~
             "No GAIN contact - web-discovered NSO contact available (validate before use)",
           n_contacts == 0      ~ "NO CONTACT AT ALL",
           n_with_email == 0 & !is.na(web_discovered_contact) ~
             "GAIN contacts lack email - web-discovered contact available (validate before use)",
           n_with_email == 0    ~ "Contacts but no email",
           n_with_position == 0 ~ "Contacts but no position info",
           TRUE                 ~ "Covered"
         )) %>%
  select(mcountry, status, priority, n_evidence, n_new_evidence,
         n_strong_nso_candidates, n_partner_or_unclear, n_manual_review,
         n_contacts, n_with_position, gap,
         web_discovered_contact, web_discovered_contact_type,
         web_contact_confidence, example_release)

safe_write(contact_gaps, paste0("contact_gaps_", stamp, ".csv"))
message("  Gap breakdown among P1-P3 countries:")
print(count(contact_gaps, gap))

# ==============================================================================
# MODULE D - SUGGESTED RESPONDENTS (title-based scoring)
# Weights derived from observed GAIN respondent titles (n=296):
#   pop/social/demographic stats director (52), migration (42),
#   statistician (21), DG (14), intl cooperation (10), census (10)
# ==============================================================================
message("\nModule D: ranking candidate respondents by title fit...")

title_score <- function(position) {
  if (is.na(position)) return(0)
  p <- str_to_lower(position)
  score <- 0
  if (str_detect(p, "(director|head|chief|deputy)") &
      str_detect(p, "(population|demograph|social stat)")) score <- score + 52
  if (str_detect(p, "migration"))                          score <- score + 42
  if (str_detect(p, "statistician"))                       score <- score + 21
  if (str_detect(p, "director general|president|chief executive")) score <- score + 14
  if (str_detect(p, "international|cooperation|partnership"))      score <- score + 10
  if (str_detect(p, "census"))                             score <- score + 10
  if (str_detect(p, "governance|peace|security"))          score <- score + 5
  if (str_detect(p, "information management|data"))        score <- score + 5
  score
}

target_countries <- priority_matrix %>%
  filter(str_starts(priority, "P1|P2")) %>%
  pull(mcountry)

suggested <- contacts %>%
  filter(Country %in% target_countries) %>%
  mutate(title_fit = map_dbl(Position, title_score)) %>%
  group_by(Country) %>%
  arrange(desc(title_fit), .by_group = TRUE) %>%
  mutate(rank_in_country = row_number()) %>%
  ungroup() %>%
  filter(rank_in_country <= 3) %>%   # top 3 candidates per country
  select(Country, rank_in_country, First_Name_Proper_, Last_Name_Proper_,
         Position, Emails_List, title_fit, Category_of_Respondent) %>%
  left_join(priority_matrix %>%
              select(mcountry, priority, example_release,
                     web_discovered_contact, web_discovered_contact_type,
                     web_contact_confidence),
            by = c("Country" = "mcountry"))

safe_write(suggested, paste0("suggested_respondents_", stamp, ".csv"))
message(paste("  Candidate suggestions written for",
              n_distinct(suggested$Country), "target countries"))

# Countries where we have NO candidate with title_fit > 0:
no_fit <- setdiff(target_countries, suggested %>% filter(title_fit > 0) %>% pull(Country))
if (length(no_fit) > 0) {
  message("\n  Countries needing NEW contact hunting (no title-matched contact):")
  message(paste("   ", paste(no_fit, collapse = ", ")))
  message("  -> Target profile to search on NSO staff/structure pages:")
  message("     1. Director/Head of Population, Social or Demographic Statistics")
  message("     2. Head of Migration Statistics unit")
}

# ==============================================================================
# UNMATCHED COUNTRY NAMES - extend harmonize_country() with these
# ==============================================================================
if (has_evidence && nrow(evidence) > 0) {
  unmatched <- setdiff(evidence$Country,
                       c(status_all$mcountry, contacts$Country))
  multi <- unmatched[is_multi_country(unmatched)]
  unmatched <- unmatched[!is_multi_country(unmatched)]
  if (length(unmatched) > 0) {
    message("\nNOTE - single-country evidence not in GAIN roster/contact list")
    message("(either a spelling to add to harmonize_country(), or a country")
    message("that has never engaged with GAIN - check before outreach):")
    message(paste("  ", paste(unmatched, collapse = ", ")))
  }
  if (length(multi) > 0) {
    message(paste0("\n", length(multi),
                   " multi-country/regional datasets flagged 'MULTI-COUNTRY' for per-country review."))
  }
}

message("\nDone. Outputs:")
message(paste0("  evidence_flagged_", stamp, ".csv"))
message(paste0("  country_priority_matrix_", stamp, ".csv"))
message(paste0("  contact_gaps_", stamp, ".csv"))
message(paste0("  suggested_respondents_", stamp, ".csv"))
