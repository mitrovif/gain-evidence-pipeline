# ==============================================================================
# GAIN LAYER 1.5 - STRUCTURED STATISTICAL API DISCOVERY  (prototype)
#
# Discovers official displacement statistics by QUERYING structured statistical
# APIs instead of scraping HTML. Returns clean, producer-known, language-agnostic
# records in the same MASTER schema, so they flow into merge/enrich unchanged.
#
# This prototype covers EUROSTAT (one SDMX/JSON-stat endpoint = ~30 EU/EEA
# countries; the national statistical authorities report these series, so they
# are country-led official statistics - genuine GAIN-relevant evidence, not
# partner context). PxWeb (Nordic NSOs) is stubbed for the next iteration.
#
# Why this is better than scraping for these countries: precise titles + the
# PRODUCER for free, no per-language keywords, no Spain/Brazil-style HTML noise.
# Honest limit: high-displacement NSOs (Sudan, Chad, DRC...) have no such API ->
# they keep the web-scrape path. This routes each country to its best interface.
#
# Output: structured_discovery_[date].csv  (master schema)
#         NSO_Full_Registry_routed.csv      (registry + discovery_interface col)
# No API key. Cached in struct_cache/. Additive.
# ==============================================================================

suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
stamp <- format(Sys.Date(), "%Y%m%d")
CACHE <- "struct_cache"; dir.create(CACHE, showWarnings = FALSE)
EUROSTAT <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data"

`%||%` <- function(a,b) if (is.null(a)||length(a)==0||all(is.na(a))) b else a

# GAIN-relevant Eurostat dataflows -> population group
EUROSTAT_FLOWS <- tribble(
  ~flow,             ~populations, ~kind,
  "migr_asyappctza", "refugees",   "Asylum applicants",
  "migr_asydcfsta",  "refugees",   "Decisions on asylum applications",
  "migr_asyunaa",    "refugees",   "Asylum applicants - unaccompanied minors",
  "migr_resfas",     "refugees",   "Resettled persons",
  "migr_pop1ctz",    "stateless",  "Population by citizenship (incl. stateless)"
)

# Eurostat geo codes that are NOT single countries (drop these)
EU_AGG <- c("EU","EU27_2020","EU28","EU27_2007","EA","EA19","EA20","EFTA","TOTAL")
geo_fixes <- c(EL="Greece", UK="United Kingdom", DE="Germany", FR="France")

fetch_flow_geos <- function(flow) {
  cache <- file.path(CACHE, paste0("estat_", flow, "_", stamp, ".rds"))
  if (file.exists(cache)) return(readRDS(cache))
  url <- paste0(EUROSTAT, "/", flow, "?format=JSON&lastTimePeriod=1")
  body <- tryCatch(request(url) %>% req_user_agent("EGRISS-GAIN-research") %>%
                     req_timeout(60) %>% req_perform() %>% resp_body_string(),
                   error = function(e) "")
  j <- tryCatch(fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j) || is.null(j$dimension$geo)) { saveRDS(NULL, cache); return(NULL) }
  geos <- names(j$dimension$geo$category$label)
  glab <- unlist(j$dimension$geo$category$label)
  yr   <- names(j$dimension$time$category$label %||% list("NA" = NA))[1]
  out <- list(label = j$label %||% flow, source = j$source %||% "ESTAT",
              year = yr, geos = geos, glab = glab)
  saveRDS(out, cache); out
}

message("Querying Eurostat structured endpoints (no key; cached)...")
records <- map_dfr(seq_len(nrow(EUROSTAT_FLOWS)), function(i) {
  f <- EUROSTAT_FLOWS[i,]
  message("  ", f$flow)
  g <- fetch_flow_geos(f$flow)
  if (is.null(g)) return(tibble())
  keep <- !g$geos %in% EU_AGG & str_detect(g$geos, "^[A-Z]{2}$")
  cc <- g$geos[keep]
  cname <- coalesce(geo_fixes[cc], g$glab[cc])
  tibble(
    country     = unname(cname),
    title       = paste0(f$kind, " - ", g$label, " (", g$year, ")"),
    url         = paste0("https://ec.europa.eu/eurostat/databrowser/view/", f$flow, "/default/table?geo=", cc),
    populations = f$populations,
    year        = g$year,
    producer    = paste0("Eurostat / national statistical authority (", f$source, ")"),
    source_layer= "STRUCT:Eurostat",
    trust       = "HIGH (structured API)"
  )
})
message(paste("  Eurostat structured records:", nrow(records)))

write_excel_csv(records, paste0("structured_discovery_", stamp, ".csv"))

# ------------------------------------------------------------------------------
# Add a discovery_interface column to the registry (routing)
# ------------------------------------------------------------------------------
if (file.exists("NSO_Full_Registry.csv")) {
  reg <- read_csv("NSO_Full_Registry.csv", show_col_types = FALSE)
  eurostat_countries <- unique(records$country)
  pxweb_countries <- c("Sweden","Finland","Norway","Denmark","Iceland")  # known PxWeb NSOs
  reg <- reg %>% mutate(discovery_interface = case_when(
    country %in% eurostat_countries ~ "sdmx (Eurostat)",
    country %in% pxweb_countries     ~ "pxweb (next)",
    TRUE                             ~ "scrape"))
  write_excel_csv(reg, "NSO_Full_Registry_routed.csv")
  message(paste0("\nRouting written to NSO_Full_Registry_routed.csv:"))
  print(as.data.frame(count(reg, discovery_interface)))
}

message("\n==================== STRUCTURED DISCOVERY ====================")
message(paste("Records:", nrow(records), "| countries:", n_distinct(records$country),
              "| populations:", paste(unique(records$populations), collapse=", ")))
message("Sample:")
print(records %>% transmute(country, title = str_sub(title, 1, 50), populations) %>% head(8) %>% as.data.frame(), row.names = FALSE)
message(paste0("\nOutputs: structured_discovery_", stamp, ".csv (master schema) + NSO_Full_Registry_routed.csv"))
