# ==============================================================================
# GAIN LAYER 1 REFINEMENT
# Input:  LAYER1_catalog_records_[date].csv  (raw harvest)
# Output: LAYER1_refined_[date].csv          (tiered, deduplicated)
#
# Tiers:
#   TIER1_DEDICATED  - displacement survey by title (refugee/IDP/stateless study)
#   TIER2_INCLUSION  - national survey/census that mentions displacement in
#                      documentation = candidate evidence of inclusion in
#                      national instruments (verify before use)
# ==============================================================================

library(tidyverse)

# auto-find the newest catalog harvest in this folder
INPUT <- sort(list.files(pattern = "^LAYER1_catalog_records_.*\\.csv$")) %>% tail(1)
if (length(INPUT) == 0) stop("No LAYER1_catalog_records_*.csv found - run GAIN_LAYER1_CATALOGS first.")
stamp <- format(Sys.Date(), "%Y%m%d")

# Catalog/microdata records restricted to 2024-2026 (GAIN = recent,
# country-led examples; NSO websites are the primary evidence source)
MIN_YEAR <- 2024
MAX_YEAR <- 2026
MIN_YEAR_DHS_MICS <- 2022   # DHS/MICS cycles run long - wider window
DHS_MICS_PAT <- "\\bmics\\b|multiple indicator cluster|demographic and health|\\bdhs\\b|malaria indicator"

raw <- read_csv(INPUT, show_col_types = FALSE)
message(paste("Raw records:", nrow(raw)))

# ------------------------------------------------------------------------------
# 1. Tiering by title
# ------------------------------------------------------------------------------
title_pattern <- paste0(
  "refugee|asylum|displac|\\bidp\\b|stateless|forcibly|",
  "r\u00e9fugi|d\u00e9plac|apatrid|desplaz|refugiad|ap\u00e1trid|",
  "\u043b\u0430\u0436\u0435\u043d|\u0431\u0435\u0436\u0435\u043d|\u043f\u0435\u0440\u0435\u043c\u0435\u0449"
)

# national-instrument signal for Tier 2
national_pattern <- "census|labour force|lfs|demographic and health|dhs|household budget|living standards|lsms|mics|population and housing|welfare|socio-?economic"

refined <- raw %>%
  mutate(
    title_l = str_to_lower(coalesce(title, "")),
    producer_l = str_to_lower(coalesce(producer, "")),
    tier = case_when(
      str_detect(title_l, title_pattern) ~ "TIER1_DEDICATED",
      str_detect(title_l, national_pattern) ~ "TIER2_INCLUSION",
      TRUE ~ "TIER3_REVIEW"
    )
  )

# ------------------------------------------------------------------------------
# 2. Producer classification - catch joint NSO production
# ------------------------------------------------------------------------------
nso_pattern <- "statisti|census bureau|bureau of stat|institut.*stat|nso\\b|dane\\b|inegi|insee|pcbs|knbs|ubos|zimstat|instat"
io_pattern  <- "unhcr|wfp|unicef|world bank|iom\\b|undp|reach|jips|idmc|ilo\\b"

refined <- refined %>%
  mutate(
    producer_type = case_when(
      str_detect(producer_l, nso_pattern) & str_detect(producer_l, io_pattern) ~ "NSO + Int. org (joint)",
      str_detect(producer_l, nso_pattern) ~ "NSO",
      str_detect(producer_l, io_pattern)  ~ "International org",
      str_detect(producer_l, "ministr|government|national")  ~ "Other government",
      TRUE ~ "Other/unknown"
    )
  )

# ------------------------------------------------------------------------------
# 3. Year sanity + cross-catalog dedup (IHSN mirrors much of World Bank)
# ------------------------------------------------------------------------------
n_before_year <- nrow(refined)
refined <- refined %>%
  mutate(
    year_start = suppressWarnings(as.numeric(year_start)),
    year_start = if_else(year_start < 1990 | year_start > 2026, NA_real_, year_start),
    best_year  = suppressWarnings(
      pmax(year_start, as.numeric(year_end), na.rm = TRUE)),
    year_floor = if_else(
      str_detect(str_to_lower(paste(coalesce(title, ""), coalesce(source, ""))),
                 DHS_MICS_PAT),
      MIN_YEAR_DHS_MICS, MIN_YEAR)
  ) %>%
  filter(is.finite(best_year), best_year >= year_floor, best_year <= MAX_YEAR) %>%
  select(-best_year, -year_floor)
message(paste0("Recency filter ", MIN_YEAR, "-", MAX_YEAR, ": dropped ",
               n_before_year - nrow(refined), " older/undated records"))

refined <- refined %>%
  mutate(norm_title = title_l %>%
           str_remove_all("[^a-z0-9 ]") %>% str_squish()) %>%
  group_by(norm_title, country) %>%
  arrange(source) %>%          # IHSN before World Bank alphabetically; keep one
  mutate(also_in = paste(unique(source), collapse = " + ")) %>%
  slice(1) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 4. Population flags from TITLE only (raw flags were over-inclusive)
# ------------------------------------------------------------------------------
refined <- refined %>%
  mutate(
    pop_refugees  = str_detect(title_l, "refugee|asylum|r\u00e9fugi|refugiad"),
    pop_idps      = str_detect(title_l, "displac|\\bidp\\b|d\u00e9plac|desplaz"),
    pop_stateless = str_detect(title_l, "stateless|apatrid|ap\u00e1trid"),
    populations = pmap_chr(list(pop_refugees, pop_idps, pop_stateless),
      function(r, i, s) {
        p <- c(if (r) "refugees", if (i) "idps", if (s) "stateless")
        if (length(p) == 0) "none in title (tier2/3)" else paste(p, collapse = "; ")
      })
  ) %>%
  select(tier, country, year_start, title, producer, producer_type,
         populations, source, also_in, url, idno) %>%
  arrange(tier, country, desc(year_start))

write_excel_csv(refined, paste0("LAYER1_refined_", stamp, ".csv"))  # UTF-8 BOM for Excel

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------
message("\n================== REFINED ==================")
message(paste("After dedup:", nrow(refined)))
print(count(refined, tier))
message("\nTier 1 by producer type:")
print(refined %>% filter(tier == "TIER1_DEDICATED") %>% count(producer_type))
message("\nTier 1 countries:")
t1 <- refined %>% filter(tier == "TIER1_DEDICATED")
message(paste(" ", n_distinct(t1$country), "countries,",
              sum(t1$year_start >= 2015, na.rm = TRUE), "records from 2015+"))
message(paste("\nOutput: LAYER1_refined_", stamp, ".csv"))
message("\nUse: TIER1 -> direct candidates for GAIN evidence + Phase 5 crossref")
message("     TIER2 -> inclusion-in-national-instruments candidates (verify)")
message("     TIER3 -> low priority review pile")
