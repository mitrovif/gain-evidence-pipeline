# ==============================================================================
# GAIN REFERENCE FILE - LAYER 1: STRUCTURED CATALOG HARVEST
#
# Harvests displacement-related survey metadata from open catalog APIs:
#   - IHSN central survey catalog        (catalog.ihsn.org)
#   - UNHCR Microdata Library            (microdata.unhcr.org)
#   - World Bank Microdata Library       (microdata.worldbank.org)
#   - ReliefWeb reports API              (api.reliefweb.int)
#
# These are curated, structured records: title, country, years, producer.
# Free, no API keys, no scraping. This is the highest-quality layer.
#
# Output: LAYER1_catalog_records_[date].csv
# ==============================================================================

library(tidyverse)
library(httr2)
library(jsonlite)

stamp <- format(Sys.Date(), "%Y%m%d")

# Catalog search keywords. English core + inclusion/statelessness-proxy terms +
# French/Spanish/Arabic refugee+IDP terms (NADA/ReliefWeb hold non-English
# metadata). naturalisation & birth registration are statelessness proxies.
KEYWORDS <- c(
  # English core
  "refugee", "internally displaced", "IDP", "stateless", "asylum",
  "forced displacement", "EGRISS",
  # inclusion / related signals
  "returnee", "durable solutions", "host community",
  "naturalisation", "naturalization", "birth registration",
  # French
  "réfugié", "déplacés internes", "apatride", "demandeur d'asile", "retournés",
  # Spanish
  "refugiados", "desplazados internos", "apátrida", "retornados",
  # Arabic (refugee / IDP / stateless)
  "لاجئين", "النازحين", "عديمي الجنسية")

# Microdata/catalog records restricted to recent years only (user decision:
# GAIN evidence should come mostly from NSO websites; catalogs are support)
MIN_YEAR <- 2024
MAX_YEAR <- 2026

# DHS and MICS survey cycles run for years; their records get a 2022+ window
# (user decision 11 Jun 2026) while all other catalogs stay 2024+.
MIN_YEAR_DHS_MICS <- 2022
DHS_MICS_PAT <- "\\bmics\\b|multiple indicator cluster|demographic and health|\\bdhs\\b|malaria indicator"

# ------------------------------------------------------------------------------
# NADA catalog search (IHSN / UNHCR / World Bank all run the same API)
# ------------------------------------------------------------------------------
search_nada <- function(base_url, keyword, source_name, max_pages = 10,
                        from_year = MIN_YEAR) {
  all_rows <- list()
  ps <- 100
  for (page in 0:(max_pages - 1)) {
    res <- tryCatch({
      request(paste0(base_url, "/index.php/api/catalog/search")) %>%
        req_user_agent("EGRISS-GAIN-research/1.0 (statistics inventory; egriss.org)") %>%
        req_headers(Accept = "application/json") %>%
        req_url_query(sk = keyword, ps = ps, page = page + 1,
                      from = from_year, to = MAX_YEAR) %>%
        req_timeout(30) %>%
        req_retry(max_tries = 3) %>%
        req_perform() %>%
        resp_body_json()
    }, error = function(e) {
      message(paste("      [", source_name, "] request failed:",
                    str_sub(conditionMessage(e), 1, 80)))
      NULL
    })

    rows <- res$result$rows
    if (is.null(rows) || length(rows) == 0) break

    parsed <- map_dfr(rows, function(r) {
      tibble(
        source      = source_name,
        keyword     = keyword,
        idno        = as.character(r$idno %||% NA),
        title       = as.character(r$title %||% NA),
        country     = as.character(r$nation %||% NA),
        year_start  = as.character(r$year_start %||% NA),
        year_end    = as.character(r$year_end %||% NA),
        producer    = as.character(r$authoring_entity %||% r$repo_title %||% NA),
        url         = paste0(base_url, "/index.php/catalog/", r$id %||% "")
      )
    })
    all_rows[[page + 1]] <- parsed
    if (length(rows) < ps) break
    Sys.sleep(0.5)
  }
  bind_rows(all_rows)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

catalogs <- tribble(
  ~base_url,                          ~source_name,
  "https://catalog.ihsn.org",         "IHSN",
  "https://microdata.unhcr.org",      "UNHCR Microdata",
  "https://microdata.worldbank.org",  "World Bank Microdata"
)

message("Layer 1a: NADA catalogs (IHSN, UNHCR, World Bank)...")

nada_results <- pmap_dfr(catalogs, function(base_url, source_name) {
  map_dfr(KEYWORDS, function(kw) {
    message(paste("  ", source_name, "-", kw))
    out <- search_nada(base_url, kw, source_name)
    message(paste("     ", nrow(out), "records"))
    out
  })
})

# ------------------------------------------------------------------------------
# MICS records via the same NADA catalogs (2022+ window)
# ------------------------------------------------------------------------------
message("\nLayer 1a-bis: MICS surveys via NADA catalogs (2022+)...")

mics_results <- pmap_dfr(catalogs, function(base_url, source_name) {
  map_dfr(c("Multiple Indicator Cluster Survey", "MICS"), function(kw) {
    message(paste("  ", source_name, "-", kw))
    out <- search_nada(base_url, kw, source_name, from_year = MIN_YEAR_DHS_MICS)
    message(paste("     ", nrow(out), "records"))
    out
  })
})

# ------------------------------------------------------------------------------
# DHS Program survey list API (free, no key; covers DHS / MIS / AIS surveys)
# ------------------------------------------------------------------------------
message("\nLayer 1a-ter: DHS Program survey API (2022+)...")

search_dhs <- function(min_year = MIN_YEAR_DHS_MICS) {
  res <- tryCatch({
    request("https://api.dhsprogram.com/rest/dhs/surveys") %>%
      req_url_query(f = "json", surveyYearStart = min_year) %>%
      req_user_agent("EGRISS-GAIN-research/1.0 (statistics inventory; egriss.org)") %>%
      req_timeout(40) %>%
      req_retry(max_tries = 3) %>%
      req_perform() %>%
      resp_body_json()
  }, error = function(e) {
    message(paste("   DHS API failed:", str_sub(conditionMessage(e), 1, 80)))
    NULL
  })
  rows <- res$Data
  if (is.null(rows) || length(rows) == 0) return(tibble())
  map_dfr(rows, function(r) {
    tibble(
      source     = "DHS Program",
      keyword    = "dhs catalog",
      idno       = as.character(r$SurveyId %||% NA),
      title      = str_squish(paste(r$CountryName %||% "", r$SurveyType %||% "DHS",
                                    r$SurveyYearLabel %||% r$SurveyYear %||% "")),
      country    = as.character(r$CountryName %||% NA),
      year_start = as.character(r$SurveyYear %||% NA),
      year_end   = NA_character_,
      producer   = "The DHS Program (USAID) with national implementing agency",
      url        = if (!is.null(r$SurveyNum))
        paste0("https://dhsprogram.com/methodology/survey/survey-display-",
               r$SurveyNum, ".cfm") else "https://dhsprogram.com"
    )
  })
}
dhs_results <- search_dhs()
message(paste("   ", nrow(dhs_results), "DHS records"))

# ------------------------------------------------------------------------------
# ReliefWeb API - reports tagged to NSOs / statistical sources
# ------------------------------------------------------------------------------
message("\nLayer 1b: ReliefWeb API...")

search_reliefweb <- function(keyword, limit = 200) {
  res <- tryCatch({
    request("https://api.reliefweb.int/v1/reports") %>%
      req_url_query(appname = "gain-egriss") %>%
      req_body_json(list(
        query = list(
          value = paste0('"', keyword, '" AND statistics'),
          operator = "AND"
        ),
        filter = list(
          field = "date.created",
          value = list(from = paste0(MIN_YEAR, "-01-01T00:00:00+00:00"))
        ),
        fields = list(
          include = list("title", "primary_country.name",
                         "date.created", "source.name", "url")
        ),
        limit = limit
      )) %>%
      req_timeout(30) %>%
      req_perform() %>%
      resp_body_json()
  }, error = function(e) NULL)

  if (is.null(res) || is.null(res$data) || length(res$data) == 0) return(tibble())

  map_dfr(res$data, function(d) {
    f <- d$fields
    src <- f$source
    producer_txt <- if (is.null(src)) {
      NA_character_
    } else if (!is.null(src$name)) {
      as.character(src$name)
    } else {
      paste(map_chr(src, ~ as.character(.x$name %||% "")), collapse = "; ")
    }
    tibble(
      source     = "ReliefWeb",
      keyword    = keyword,
      idno       = as.character(d$id %||% NA),
      title      = as.character(f$title %||% NA),
      country    = as.character(f$primary_country$name %||% NA),
      year_start = substr(as.character(f$date$created %||% ""), 1, 4),
      year_end   = NA_character_,
      producer   = producer_txt,
      url        = as.character(f$url %||% NA)
    )
  })
}

rw_results <- map_dfr(KEYWORDS, function(kw) {
  message(paste("  ReliefWeb -", kw))
  out <- search_reliefweb(kw)
  message(paste("     ", nrow(out), "records"))
  Sys.sleep(1)
  out
})

# Keep only ReliefWeb records where the producer looks statistical
if (nrow(rw_results) > 0 && "producer" %in% names(rw_results)) {
  rw_results <- rw_results %>%
    filter(str_detect(str_to_lower(coalesce(producer, "")),
                      "statisti|census|bureau|institut|nso|government"))
  message(paste("  ReliefWeb records kept after NSO filter:", nrow(rw_results)))
} else {
  message("  ReliefWeb returned 0 records - continuing with NADA results only")
  rw_results <- tibble()
}

# ------------------------------------------------------------------------------
# COMBINE, DEDUPLICATE, FILTER TO NSO PRODUCERS
# ------------------------------------------------------------------------------
message("\nCombining and deduplicating...")

combined <- bind_rows(nada_results, mics_results, dhs_results, rw_results) %>%
  filter(!is.na(title))

if (nrow(combined) == 0) {
  stop("No records from any catalog - check internet connection / firewall and re-run.")
}

combined <- combined %>%
  # one row per unique record (same survey found via multiple keywords)
  group_by(source, idno) %>%
  summarise(
    across(c(title, country, year_start, year_end, producer, url), first),
    keywords_matched = paste(unique(keyword), collapse = "; "),
    .groups = "drop"
  ) %>%
  # population classification from title + matched keywords
  mutate(
    txt = str_to_lower(paste(title, keywords_matched)),
    pop_refugees  = str_detect(txt, "refugee|asylum"),
    pop_idps      = str_detect(txt, "internally displaced|idp|displacement"),
    pop_stateless = str_detect(txt, "stateless"),
    # crude NSO flag: producer mentions a statistical agency
    likely_nso = str_detect(str_to_lower(coalesce(producer, "")),
                            "statisti|census bureau|bureau of stat|institut.*stat")
  ) %>%
  select(-txt) %>%
  arrange(country, desc(year_start))

# Hard recency filter: 2024-2026, except DHS/MICS records which get 2022+
n_before_year <- nrow(combined)
combined <- combined %>%
  mutate(
    best_year = suppressWarnings(
      pmax(as.numeric(year_start), as.numeric(year_end), na.rm = TRUE)),
    year_floor = if_else(
      str_detect(str_to_lower(paste(title, keywords_matched, source)), DHS_MICS_PAT),
      MIN_YEAR_DHS_MICS, MIN_YEAR)
  ) %>%
  filter(is.finite(best_year), best_year >= year_floor, best_year <= MAX_YEAR) %>%
  select(-best_year, -year_floor)
message(paste0("Recency filter (", MIN_YEAR, "+ general, ", MIN_YEAR_DHS_MICS,
               "+ for DHS/MICS): dropped ", n_before_year - nrow(combined),
               " older/undated records, kept ", nrow(combined)))

# ------------------------------------------------------------------------------
# DISPLACEMENT-RELEVANCE FILTER (user decision 15 Jun 2026):
# general DHS/MICS/census records are NOT auto-added. A catalog record is kept
# only if its title or matched keywords name a displacement population
# (refugee / asylum / IDP / internally displaced / stateless / forcibly
# displaced / returnee / EGRISS). This stops standard DHS rounds and routine
# surveys padding the dataset; only displacement-relevant ones survive.
DISPLACEMENT_PAT <- paste0(
  "refugee|asylum|asile|asilo|réfugi|refugiad|",
  "internally displaced|\\bidps?\\b|displac|déplac|desplaz|deslocad|",
  "stateless|apatrid|apátrid|forcibly displaced|forced displacement|",
  "returnee|retourn|retornad|egriss|نازح|لاجئ|اللجوء|عديمي الجنسية")
n_before_rel <- nrow(combined)
combined <- combined %>%
  filter(str_detect(str_to_lower(paste(coalesce(title, ""),
                                       coalesce(keywords_matched, ""))),
                    DISPLACEMENT_PAT))
message(paste0("Displacement-relevance filter: dropped ",
               n_before_rel - nrow(combined),
               " non-displacement records (general DHS/MICS/census etc.), kept ",
               nrow(combined)))

# UTF-8 with BOM so Excel renders all scripts correctly
write_excel_csv(combined, paste0("LAYER1_catalog_records_", stamp, ".csv"))

message(paste("\nTotal unique catalog records:", nrow(combined)))
message(paste("Flagged as NSO-produced:", sum(combined$likely_nso, na.rm = TRUE)))
message(paste("Countries covered:", n_distinct(combined$country)))
message(paste("\nOutput: LAYER1_catalog_records_", stamp, ".csv"))
