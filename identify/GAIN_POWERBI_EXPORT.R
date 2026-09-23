# ==============================================================================
# GAIN POWER BI EXPORT
#
# Flattens the pipeline outputs into clean, dashboard-ready tables in a
# powerbi_export/ subfolder. Power BI connects to that ONE folder.
#
# Inputs (newest of each, auto-found):
#   GAIN_EVIDENCE_ENRICHED_*.csv   (candidate records - the main fact table)
#   country_priority_matrix_*.csv  (per-country outreach status)
#   LAYER3_inventory_stats_*.csv   (per-domain processing overview)
#
# Outputs (UTF-8 BOM, Excel/Power BI friendly):
#   powerbi_export/WEB_GAIN_fact_candidates.csv   one row per candidate record
#   powerbi_export/WEB_GAIN_dim_country.csv       one row per country (summary + status)
#   powerbi_export/WEB_GAIN_fact_contacts.csv     one row per suggested outreach contact
#   powerbi_export/WEB_GAIN_processing_status.csv per-domain crawl/search status
#   powerbi_export/WEB_GAIN_README.txt           how to connect Power BI + suggested visuals
# ==============================================================================

library(tidyverse)
library(stringr)

OUT <- "powerbi_export"
dir.create(OUT, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# GEO-HARMONISATION for the Power BI map. Adds three columns to a table:
#   map_country        clean single-country name (NA for multi-country/regions)
#   iso3               ISO-3166 alpha-3 (NA for the same rows; NA if countrycode
#                      is not installed - map_country alone still geocodes by name)
#   is_single_country  TRUE only for one recognised country
# Multi-country lists ("...and 16 more", 3+ comma-separated names) and region
# labels are not places and are left blank so they never break the map.
# ------------------------------------------------------------------------------
GEO_NAME_FIXES <- c(
  "Congo, Dem. Rep."            = "Democratic Republic of the Congo",
  "Moldova (the Republic of)"   = "Moldova",
  "Federal Republic of Somalia" = "Somalia",
  "Slovak Republic"             = "Slovakia",
  "Yemen, Rep."                 = "Yemen",
  "Kyrgyz Republic"             = "Kyrgyzstan",
  "State of Palestine"          = "Palestine"
)
GEO_REGIONS <- c("Europe and Central Asia")  # extend if new regions appear

harmonize_geo <- function(df, col = "country") {
  raw <- as.character(df[[col]])
  is_multi <- str_detect(coalesce(raw, ""), "more$") |
              raw %in% GEO_REGIONS |
              str_count(coalesce(raw, ""), ",") >= 2          # 3+ names = a list
  clean <- ifelse(raw %in% names(GEO_NAME_FIXES), GEO_NAME_FIXES[raw], raw)
  map_country <- ifelse(is_multi | is.na(raw), NA_character_, clean)
  iso3 <- rep(NA_character_, length(map_country))
  ok <- !is.na(map_country)
  if (any(ok) && requireNamespace("countrycode", quietly = TRUE)) {
    iso3[ok] <- countrycode::countrycode(as.character(map_country[ok]), "country.name",
                                          "iso3c", warn = FALSE)
  }
  df$map_country       <- map_country
  df$iso3              <- iso3
  df$is_single_country <- !is_multi & !is.na(raw)
  df
}

# newest by modification time (not filename) so a fresh file is never shadowed
# by an older variant whose name sorts later (e.g. a stale _SEM)
newest <- function(pat) { f <- list.files(pattern = pat); if (length(f)) f[which.max(file.info(f)$mtime)] else NA }

# Write that tolerates transient OneDrive/Excel file locks: retries a few times,
# and if still locked falls back to a timestamped name so the run never fails.
safe_write <- function(df, path) {
  for (attempt in 1:4) {
    okw <- tryCatch({ write_excel_csv(df, path); TRUE },
                    error = function(e) FALSE)
    if (okw) return(invisible(path))
    Sys.sleep(2)   # let OneDrive/Excel release the handle
  }
  alt <- sub("\\.csv$", paste0("_", format(Sys.time(), "%H%M%S"), ".csv"), path)
  write_excel_csv(df, alt)
  message("  (", basename(path), " was locked - wrote ", basename(alt), " instead)")
  invisible(alt)
}

# ------------------------------------------------------------------------------
# LOCKED SCHEMAS - the column NAMES and ORDER of each output table are fixed here
# so an existing Power BI dashboard refreshes cleanly every run, regardless of
# whether the LLM layer ran. New analytical columns are APPENDED AT THE END so
# existing columns keep their names and positions. To add a column later, append
# its name to the relevant list (never reorder or remove existing ones).
# ------------------------------------------------------------------------------
FACT_SCHEMA <- c(
  # --- stable base (original dashboard columns - DO NOT reorder) ---
  "data_source","country","title","dashboard_title","url","outreach_category","category_letter",
  "evidence_nature","is_humanitarian","instrument_includes_displacement","mentions_inclusion",
  "pop_stat_cooccurrence","gain_flag","in_gain_already","gain_respondent_status",
  "candidate_lead_type","relevance_score","doc_language","doc_type","year","pub_date_guess",
  "source_layer","trust","populations","mentions_refugee","mentions_idp","mentions_stateless",
  "mentions_egriss","outreach_priority","recommended_outreach_route","recommended_primary_contact",
  "recommended_primary_contact_type","contact_confidence","contact_validation_needed",
  "overall_comment","is_actionable","is_suppressed","is_official_actionable",
  "map_country","iso3","is_single_country",
  # --- new analytical columns (appended at end) ---
  "gain_match_type","matched_gain_id","gain_match_confidence","gain_match_reason",
  "sem_similarity","llm_relevance","llm_counted","llm_implementation_status","llm_lead_type",
  "llm_is_humanitarian","llm_quote","llm_confidence","lead_agreement","is_llm_reviewed")

DIM_SCHEMA <- c("data_source","country","map_country","iso3","is_single_country",
  "records","actionable","official_actionable","humanitarian_records",
  "strong_nso_A","partner_unclear_B","evidence_lead_C","manual_review_D","suppressed_E",
  "in_gain_already","not_yet_in_gain","top_score","has_outreach_contact",
  "status","priority","n_responses","web_contact_confidence",
  # --- LLM/example-level country rollups (appended) ---
  "llm_confirmed_inclusion","new_to_gain_examples","already_in_gain_examples")

CONTACTS_SCHEMA <- c("data_source","country","title","email","contact_type",
  "contact_source","rank","map_country","iso3","is_single_country")

# guarantee every schema column exists (NA if absent) and output EXACTLY the
# schema columns in fixed order (extras dropped with a note so count is stable)
enforce_schema <- function(df, schema) {
  for (col in setdiff(schema, names(df))) df[[col]] <- NA
  extra <- setdiff(names(df), schema)
  if (length(extra))
    message("  note: dropping ", length(extra), " column(s) not in locked schema: ",
            paste(extra, collapse = ", "))
  df[, schema, drop = FALSE]
}

# Prefer evidence_flagged_*.csv (crossref output): it has every enriched column
# PLUS gain_flag (in GAIN already?) and respondent_status. Fall back to enriched.
flag_f <- newest("^evidence_flagged_.*\\.csv$")
enr_f  <- newest("^GAIN_EVIDENCE_ENRICHED_.*\\.csv$")
if (!is.na(flag_f)) {
  enr <- read_csv(flag_f, show_col_types = FALSE)
  message(paste("Source:", flag_f, "-", nrow(enr), "records (incl. GAIN match status)"))
} else if (!is.na(enr_f)) {
  enr <- read_csv(enr_f, show_col_types = FALSE)
  message(paste("Source:", enr_f, "-", nrow(enr),
                "records (run GAIN_PHASE5_CROSSREF.R to add GAIN match status)"))
} else {
  stop("No evidence_flagged_*.csv or GAIN_EVIDENCE_ENRICHED_*.csv found.")
}

# ------------------------------------------------------------------------------
# 1. FACT TABLE - one row per candidate, only the columns a reviewer/dashboard needs
# ------------------------------------------------------------------------------
pick <- function(df, col, default = NA) if (col %in% names(df)) df[[col]] else default

gain_flag_raw <- pick(enr, "gain_flag")
# Source tag so these rows are identifiable when fed into the bigger dashboard
DATA_SOURCE <- "GAIN Web Scraping"

fact <- tibble(
  data_source        = DATA_SOURCE,
  country            = enr$country,
  title              = pick(enr, "title"),
  # title prefixed for easy identification inside a combined dashboard
  dashboard_title    = paste0("[GAIN Web Scraping] ", coalesce(pick(enr, "title"), enr$url)),
  url                = enr$url,
  outreach_category  = pick(enr, "outreach_category"),
  category_letter    = str_sub(coalesce(pick(enr, "outreach_category"), "?"), 1, 1),
  evidence_nature    = pick(enr, "evidence_nature"),
  is_humanitarian    = pick(enr, "is_humanitarian"),
  # GAIN-defining signal: does the instrument actually capture displacement?
  instrument_includes_displacement = pick(enr, "has_inclusion"),
  mentions_inclusion = pick(enr, "mentions_inclusion"),
  pop_stat_cooccurrence = pick(enr, "cooccur_pop_stat"),
  # --- already in GAIN? (from crossref) -------------------------------------
  gain_flag          = gain_flag_raw,
  in_gain_already    = case_when(
    is.na(gain_flag_raw)                  ~ "unknown (crossref not run)",
    gain_flag_raw == "IN_GAIN"            ~ "Yes - matches a reported GAIN project",
    gain_flag_raw == "POSSIBLE"           ~ "Possibly - country reports to GAIN",
    gain_flag_raw == "NEW"                ~ "No - not yet reported to GAIN",
    str_starts(gain_flag_raw, "MULTI")    ~ "Multi-country - review per country",
    TRUE                                  ~ gain_flag_raw),
  gain_respondent_status = pick(enr, "respondent_status"),
  # --- example-level match + LLM/semantic enrichment (from crossref + funnel) --
  gain_match_type        = pick(enr, "gain_match_type"),        # already_in_gain / new_example_existing_country / new_country
  matched_gain_id        = pick(enr, "matched_gain_id"),        # GAIN example index when matched
  gain_match_confidence  = pick(enr, "gain_match_confidence"),
  gain_match_reason      = pick(enr, "gain_match_reason"),
  sem_similarity         = pick(enr, "sem_similarity"),         # cosine to nearest GAIN example
  llm_relevance          = pick(enr, "llm_relevance"),
  llm_counted            = pick(enr, "llm_counted"),
  llm_implementation_status = pick(enr, "llm_implementation_status"),
  llm_lead_type          = pick(enr, "llm_lead_type"),
  llm_is_humanitarian    = pick(enr, "llm_is_humanitarian"),
  llm_quote              = pick(enr, "llm_quote"),
  llm_confidence         = pick(enr, "llm_confidence"),
  lead_agreement         = pick(enr, "lead_agreement"),         # LLM vs rule lead comparison
  candidate_lead_type = pick(enr, "candidate_lead_type"),
  relevance_score    = pick(enr, "relevance_score"),
  doc_language       = pick(enr, "doc_language"),
  doc_type           = pick(enr, "doc_type"),
  year               = pick(enr, "year"),
  pub_date_guess     = pick(enr, "pub_date_guess"),
  source_layer       = pick(enr, "source_layer"),
  trust              = pick(enr, "trust"),
  populations        = pick(enr, "populations"),
  mentions_refugee   = pick(enr, "mentions_refugee"),
  mentions_idp       = pick(enr, "mentions_idp"),
  mentions_stateless = pick(enr, "mentions_stateless"),
  mentions_egriss    = pick(enr, "mentions_egriss"),
  outreach_priority  = pick(enr, "outreach_priority"),
  recommended_outreach_route   = pick(enr, "recommended_outreach_route"),
  recommended_primary_contact  = pick(enr, "recommended_primary_contact"),
  recommended_primary_contact_type = pick(enr, "recommended_primary_contact_type"),
  contact_confidence = pick(enr, "contact_confidence"),
  contact_validation_needed = pick(enr, "contact_validation_needed"),
  overall_comment    = pick(enr, "overall_comment")
) %>%
  mutate(is_actionable = category_letter %in% c("A", "B", "D"),
         is_suppressed = category_letter == "E",
         # official-statistics leads exclude humanitarian/operational data
         is_official_actionable = is_actionable & !coalesce(is_humanitarian, FALSE),
         # TRUE for rows the LLM actually reviewed (the gated subset). Lets the
         # dashboard separate "LLM-checked" from "not checked" instead of blanks.
         is_llm_reviewed = !is.na(llm_relevance))

fact <- harmonize_geo(fact)
fact <- enforce_schema(fact, FACT_SCHEMA)   # lock column names + order
safe_write(fact, file.path(OUT, "WEB_GAIN_fact_candidates.csv"))

# ------------------------------------------------------------------------------
# 2. DIM COUNTRY - ONE row per clean single country (unique on map_country) so
# fact_candidates[map_country] -> dim_country[map_country] relates cleanly.
# Grouping on map_country merges spelling variants ("Somalia" + "Federal Republic
# of Somalia" -> one Somalia row). Multi-country/regional rows are not real
# places and are excluded from the country dimension (they stay in fact_candidates
# with is_single_country = FALSE for per-country review). `country` is kept and
# set equal to map_country so existing references still resolve.
# ------------------------------------------------------------------------------
dim_country <- fact %>%
  filter(is_single_country) %>%
  group_by(map_country, iso3) %>%
  summarise(
    data_source          = DATA_SOURCE,
    records              = n(),
    actionable           = sum(is_actionable),
    official_actionable  = sum(is_official_actionable),
    humanitarian_records = sum(coalesce(is_humanitarian, FALSE)),
    strong_nso_A         = sum(category_letter == "A"),
    partner_unclear_B    = sum(category_letter == "B"),
    evidence_lead_C      = sum(category_letter == "C"),
    manual_review_D      = sum(category_letter == "D"),
    suppressed_E         = sum(category_letter == "E"),
    in_gain_already      = sum(gain_flag == "IN_GAIN", na.rm = TRUE),
    not_yet_in_gain      = sum(gain_flag == "NEW", na.rm = TRUE),
    top_score            = max(relevance_score, na.rm = TRUE),
    has_outreach_contact = any(!is.na(recommended_primary_contact)),
    # --- LLM / example-level rollups (all -> 0 when the LLM layer did not run) ---
    llm_confirmed_inclusion  = sum(coalesce(as.logical(llm_counted), FALSE)),
    new_to_gain_examples     = sum(gain_match_type %in%
                                     c("new_country","new_example_existing_country"), na.rm = TRUE),
    already_in_gain_examples = sum(gain_match_type == "already_in_gain", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(country = map_country, is_single_country = TRUE)

pm_f <- newest("^country_priority_matrix_.*\\.csv$")
if (!is.na(pm_f)) {
  pm <- read_csv(pm_f, show_col_types = FALSE)
  cc <- intersect(c("mcountry", "status", "priority", "n_responses",
                    "web_contact_confidence"), names(pm))
  if ("mcountry" %in% names(pm)) {
    # join the priority matrix on the clean country name
    dim_country <- dim_country %>%
      left_join(pm %>% select(all_of(cc)) %>% rename(country = mcountry),
                by = "country")
  }
}
dim_country <- enforce_schema(dim_country, DIM_SCHEMA)   # lock column names + order
safe_write(dim_country, file.path(OUT, "WEB_GAIN_dim_country.csv"))

# ------------------------------------------------------------------------------
# 3. FACT CONTACTS - one row per suggested outreach contact (primary + secondary)
# ------------------------------------------------------------------------------
contacts <- bind_rows(
  tibble(data_source = DATA_SOURCE, country = fact$country, title = fact$title,
         email = pick(enr, "recommended_primary_contact"),
         contact_type = pick(enr, "recommended_primary_contact_type"),
         contact_source = pick(enr, "recommended_primary_contact_source"),
         rank = "primary"),
  tibble(data_source = DATA_SOURCE, country = fact$country, title = fact$title,
         email = pick(enr, "recommended_secondary_contact"),
         contact_type = pick(enr, "recommended_secondary_contact_type"),
         contact_source = pick(enr, "recommended_secondary_contact_source"),
         rank = "secondary")
) %>% filter(!is.na(email)) %>% distinct(country, email, .keep_all = TRUE)
contacts <- harmonize_geo(contacts)
contacts <- enforce_schema(contacts, CONTACTS_SCHEMA)   # lock column names + order
safe_write(contacts, file.path(OUT, "WEB_GAIN_fact_contacts.csv"))

# ------------------------------------------------------------------------------
# 4. PROCESSING STATUS - per-domain crawl/search overview
# ------------------------------------------------------------------------------
ov_f <- newest("^LAYER3_inventory_stats_.*\\.csv$")
if (!is.na(ov_f)) {
  read_csv(ov_f, show_col_types = FALSE) %>%
    mutate(data_source = DATA_SOURCE, .before = 1) %>%
    safe_write(file.path(OUT, "WEB_GAIN_processing_status.csv"))
}

# ------------------------------------------------------------------------------
# README
# ------------------------------------------------------------------------------
readme <- c(
  "GAIN evidence - Power BI data pack",
  "==================================",
  "",
  "Connect Power BI Desktop to this folder:",
  "  Home > Get data > Folder. For each CSV set File Origin = 65001: Unicode (UTF-8).",
  "",
  "Tables:",
  "  WEB_GAIN_fact_candidates    one row per candidate record (the main table)",
  "  WEB_GAIN_dim_country        one row per country (rollup + GAIN respondent status)",
  "  WEB_GAIN_fact_contacts      one row per suggested outreach contact",
  "  WEB_GAIN_processing_status  per-domain crawl/search status",
  "",
  "Geographic columns (on every table):",
  "  map_country        clean single-country name for the map; blank for",
  "                     multi-country lists and regions",
  "  iso3               ISO-3166 alpha-3 code; blank for the same rows",
  "  is_single_country  TRUE for one recognised country, FALSE for lists/regions",
  "",
  "Model:",
  "  dim_country is ONE row per clean single country (unique on map_country).",
  "  Relate fact_candidates[map_country] -> dim_country[map_country] (many-to-one).",
  "  (fact_candidates[country] is the RAW name and includes multi-country lists -",
  "   do not relate on it; use map_country, which is in BOTH tables.)",
  "  If you want slicers on fact_candidates to also filter the map, set this one",
  "  relationship to filter BOTH directions.",
  "",
  "Before building visuals:",
  "  - Mark map_country and iso3 as Country/Region (Column tools > Data category).",
  "  - Mark url as Web URL so it is clickable.",
  "  - Always use map_country, never raw country, so spelling variants do not split.",
  "",
  "Visuals (field wells):",
  "",
  "1. Map - where the real leads are",
  "   Table dim_country",
  "   Location: map_country        Bubble size: official_actionable (Sum)",
  "   Tooltips: not_yet_in_gain, records, top_score, status",
  "   Visual filter: is_single_country = TRUE AND official_actionable > 0",
  "",
  "2. Stacked bar - candidates by category",
  "   Table fact_candidates",
  "   Axis: category_letter   Legend: evidence_nature   Values: Count of url",
  "",
  "3. Matrix - new vs already reported",
  "   Table fact_candidates",
  "   Rows: map_country   Columns: in_gain_already   Values: Count of url",
  "   Optional filter: is_official_actionable = TRUE",
  "   Sort by the \"No - not yet reported to GAIN\" column to surface new evidence.",
  "",
  "4. Cards - Count of url, one card each with a visual-level filter:",
  "   Total actionable:   is_actionable = TRUE",
  "   Official worklist:  is_official_actionable = TRUE",
  "   New to GAIN:        in_gain_already = \"No - not yet reported to GAIN\"",
  "   Strong NSO leads:   category_letter = A",
  "",
  "5. Detail table - the A/B worklist",
  "   Table fact_candidates",
  "   Columns: map_country, title, relevance_score, candidate_lead_type,",
  "            evidence_nature, in_gain_already, overall_comment,",
  "            recommended_primary_contact, url",
  "   Filter: category_letter is A or B.   Sort: relevance_score descending.",
  "",
  "6. Slicers:",
  "   evidence_nature, candidate_lead_type, outreach_priority,",
  "   gain_respondent_status, doc_language, year, in_gain_already",
  "",
  "Optional measures (create on fact_candidates):",
  "  Official leads = CALCULATE(COUNTROWS(fact_candidates),",
  "                             fact_candidates[is_official_actionable] = TRUE())",
  "  New to GAIN    = CALCULATE(COUNTROWS(fact_candidates),",
  "                   fact_candidates[in_gain_already] = \"No - not yet reported to GAIN\")",
  "  Use \"Official leads\" as the map bubble size instead of dim_country[official_actionable]",
  "  if you want fact-side slicers to drive the map.",
  "",
  "CAUTION (keep the framing): every row is a POSSIBLE candidate requiring manual",
  "review and confirmation from the NSO or relevant respondent. Nothing here is a",
  "confirmed GAIN example; relevance_score is a screening priority only.",
  "",
  "Refresh: re-run the pipeline, re-run GAIN_POWERBI_EXPORT.R, then click Refresh",
  "in Power BI. Same folder, same file names, no re-mapping needed."
)
writeLines(readme, file.path(OUT, "WEB_GAIN_README.txt"))

message("\nPower BI pack written to ", normalizePath(OUT, winslash = "/"))
message(paste0("  fact_candidates.csv  (", nrow(fact), " rows)"))
message(paste0("  dim_country.csv      (", nrow(dim_country), " countries)"))
message(paste0("  fact_contacts.csv    (", nrow(contacts), " contacts)"))
message("  processing_status.csv + _README.txt")
message("\nOpen Power BI Desktop -> Get data -> Folder -> point at powerbi_export/")
