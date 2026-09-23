# ==============================================================================
# GAIN SDMX DISPLACEMENT CONTEXT  (UNICEF Data Warehouse - public, no API key)
#
# Inspired by github.com/jpazvd/unicefstats-mcp, but pulls the DISPLACEMENT
# indicators that the curated child-stats MCP deliberately excludes, straight
# from the UNICEF SDMX REST API (https://sdmx.data.unicef.org).
#
# IMPORTANT - what this is and is NOT:
#   These figures are PARTNER-sourced aggregates (IDPs -> IDMC, refugees ->
#   UNHCR, asylum -> UNHCR). They are NOT NSO official statistics, so they are
#   NOT GAIN "evidence". They are a displacement-CONTEXT layer:
#     1. how many refugees / IDPs / asylum-seekers each country has (latest year)
#        -> prioritise and contextualise the NSO search + the Power BI dashboard
#     2. a flag for the rare rows whose DATA_SOURCE is actually an NSO/census
#        -> those ARE candidate GAIN-relevant official statistics (written out
#           separately for review).
#
# Additive, cached (sdmx_cache/), idempotent. No keys. Outputs:
#   displacement_context_[date].csv          one row per country (the context table)
#   powerbi_export/WEB_GAIN_displacement_context.csv   same, keyed for Power BI
#   sdmx_nso_sourced_displacement_[date].csv  rows where the source looks like an NSO
# ==============================================================================

suppressMessages({ library(tidyverse); library(httr2) })
stamp <- format(Sys.Date(), "%Y%m%d")
SDMX_BASE  <- "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest"
START_YEAR <- 2020
CACHE_DIR  <- "sdmx_cache"; dir.create(CACHE_DIR, showWarnings = FALSE)
has_cc <- requireNamespace("countrycode", quietly = TRUE)

# the displacement indicators (confirmed present in CL_UNICEF_INDICATOR)
INDICATORS <- tribble(
  ~code,                    ~field,            ~label,
  "MG_RFGS",                "refugees",        "Refugees (hosted)",
  "MG_INTERNAL_DISP_PERS",  "idps",            "Internally displaced persons",
  "MG_NEW_INTERNAL_DISP",   "new_idps",        "New internal displacements",
  "MG_ASYLM",               "asylum_seekers",  "Asylum seekers"
)

# fetch one indicator for ALL areas, latest values from START_YEAR (cached)
fetch_indicator <- function(code) {
  cache <- file.path(CACHE_DIR, paste0(code, "_", stamp, ".rds"))
  if (file.exists(cache)) return(readRDS(cache))
  url <- paste0(SDMX_BASE, "/data/UNICEF,GLOBAL_DATAFLOW,1.0/.", code,
                "?format=csv&startPeriod=", START_YEAR)
  body <- tryCatch(
    request(url) %>% req_user_agent("EGRISS-GAIN-research (statistics inventory)") %>%
      req_timeout(90) %>% req_perform() %>% resp_body_string(),
    error = function(e) "")
  d <- tryCatch(read_csv(I(body), show_col_types = FALSE), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) { saveRDS(tibble(), cache); return(tibble()) }
  out <- d %>%
    transmute(iso3 = REF_AREA, indicator = code,
              year = suppressWarnings(as.integer(TIME_PERIOD)),
              value = suppressWarnings(as.numeric(OBS_VALUE)),
              source = if ("DATA_SOURCE" %in% names(d)) DATA_SOURCE else NA_character_,
              source_link = if ("SOURCE_LINK" %in% names(d)) SOURCE_LINK else NA_character_) %>%
    filter(str_detect(iso3, "^[A-Z]{3}$"))    # real countries only (drop regions/aggregates)
  saveRDS(out, cache)
  out
}

message("Fetching UNICEF SDMX displacement indicators (no key; cached)...")
raw <- map_dfr(INDICATORS$code, function(cd) {
  message("  ", cd); r <- fetch_indicator(cd); message("    rows: ", nrow(r)); r
})
if (nrow(raw) == 0) stop("No SDMX data returned - check connectivity to sdmx.data.unicef.org")

# latest year per country x indicator
latest <- raw %>% group_by(iso3, indicator) %>% slice_max(year, n = 1, with_ties = FALSE) %>% ungroup()

# NSO-sourced detection (these would be GAIN-relevant official statistics)
NSO_SRC_PAT <- "statisti|census|bureau of stat|national institut|nso\\b|enquête|recensement"
PARTNER_SRC_PAT <- "unhcr|idmc|internal displacement|refugee agency|\\biom\\b|un desa|population division"
latest <- latest %>%
  mutate(source_type = case_when(
    str_detect(str_to_lower(coalesce(source, "")), NSO_SRC_PAT) ~ "NSO/official",
    str_detect(str_to_lower(coalesce(source, "")), PARTNER_SRC_PAT) ~ "partner (UNHCR/IDMC/UN)",
    TRUE ~ "other/unknown"))

# country context table: one row per country, wide (separate pivots = robust to
# indicators that returned no data)
lj <- latest %>% left_join(INDICATORS, by = c("indicator" = "code"))
wide_val <- lj %>% select(iso3, field, value) %>% pivot_wider(names_from = field, values_from = value)
wide_yr  <- lj %>% select(iso3, field, year)  %>% pivot_wider(names_from = field, values_from = year,  names_glue = "{field}_year")
wide_src <- lj %>% select(iso3, field, source) %>% pivot_wider(names_from = field, values_from = source, names_glue = "{field}_source")
ctx <- wide_val %>% left_join(wide_yr, by = "iso3") %>% left_join(wide_src, by = "iso3")

# guarantee every indicator field exists (NA if that indicator had no data)
for (f in INDICATORS$field) if (!f %in% names(ctx)) ctx[[f]] <- NA_real_

ctx <- ctx %>%
  mutate(country = if (has_cc) countrycode::countrycode(iso3, "iso3c", "country.name", warn = FALSE) else iso3,
         data_source = "UNICEF SDMX (UNHCR/IDMC/UN DESA)",
         any_nso_sourced = iso3 %in% (latest %>% filter(source_type == "NSO/official") %>% pull(iso3)),
         displaced_total = rowSums(across(any_of(c("refugees","idps","asylum_seekers")),
                                          ~ coalesce(.x, 0)))) %>%
  relocate(country, iso3, data_source) %>%
  arrange(desc(displaced_total))

write_excel_csv(ctx, paste0("displacement_context_", stamp, ".csv"))

# Power BI dimension (key on iso3 / country to relate to dim_country)
if (dir.exists("powerbi_export"))
  write_excel_csv(ctx, file.path("powerbi_export", "WEB_GAIN_displacement_context.csv"))

# NSO-sourced rows = candidate GAIN-relevant official displacement statistics
nso_rows <- latest %>% filter(source_type == "NSO/official") %>%
  left_join(INDICATORS, by = c("indicator" = "code")) %>%
  transmute(iso3,
            country = if (has_cc) countrycode::countrycode(iso3, "iso3c", "country.name", warn = FALSE) else iso3,
            indicator = label, year, value, source, source_link)
write_excel_csv(nso_rows, paste0("sdmx_nso_sourced_displacement_", stamp, ".csv"))

message("\n==================== SDMX DISPLACEMENT CONTEXT ====================")
message(paste("Countries with displacement data:", nrow(ctx)))
message(paste("  with refugees figure:", sum(!is.na(ctx$refugees)),
              "| with IDPs:", sum(!is.na(ctx$idps)),
              "| with asylum seekers:", sum(!is.na(ctx$asylum_seekers))))
message(paste("NSO/official-sourced observations (candidate GAIN evidence):", nrow(nso_rows)))
message(paste0("\nOutputs: displacement_context_", stamp, ".csv",
               " | WEB_GAIN_displacement_context.csv | sdmx_nso_sourced_displacement_", stamp, ".csv"))
message("Top 8 by displaced population:")
print(ctx %>% transmute(country, refugees, idps, asylum_seekers) %>% head(8) %>% as.data.frame(), row.names = FALSE)
