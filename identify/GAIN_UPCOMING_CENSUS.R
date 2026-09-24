# ==============================================================================
# GAIN UPCOMING CENSUSES  (ask about inclusion BEFORE the census, not after)
#
# The rest of identification finds inclusion that has already been published.
# A census is the one instrument whose date is known years ahead, and GAIN
# records PLANNED inclusion too - so an office planning a 2026-2028 census can
# be asked now whether refugees, IDPs and stateless people will be identified.
#
# Sources (all public, read through their data interfaces - no page scraping
# beyond the UNSD table):
#   1. UNSD "Census dates for all countries" (2020 and 2030 rounds), the most
#      current list (page states its last update) -> census date per country
#   2. UNFPA Global Census Tracker (ArcGIS layer census_joined behind
#      experience.arcgis.com/experience/6c3954186a17429b84fe518af72aa674):
#      pilot census, data-collection method, previous census. NOTE: last
#      edited Feb 2023 - used for method/pilot, NOT as the date of record.
#   3. UNHCR Refugee Data Finder API: refugees, asylum-seekers, IDPs, stateless
#      and others of concern hosted per country (latest year available)
#   4. GAIN roster: is there already a census example for the country?
#   5. Contact workbook (GAIN_CONFIG.R CONTACT_XLSX): is there an NSO contact?
#
# Output: upcoming_censuses_[date].csv and powerbi_export/WEB_GAIN_upcoming_censuses.csv
#   action:
#     "ask now: planned census"       census between this year and +ASK_WINDOW (default 3)
#     "later: census after window"    planned further out (listed, not asked yet)
#     "ask: recent census, not in GAIN" census 2022-2025, no GAIN census example
#     "in GAIN: ask for update"       census example already in GAIN, newer census
#     "context"                       everything else
#   priority: high when the country hosts >= DISPLACED_MIN displaced/stateless people
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite); library(xml2); library(rvest) })
source("shared/GAIN_COMMON.R")
suppressMessages(source("shared/GAIN_CONFIG.R"))

DISPLACED_MIN <- as.numeric(Sys.getenv("GAIN_CENSUS_DISPLACED_MIN", "10000"))
ASK_WINDOW    <- as.integer(Sys.getenv("GAIN_CENSUS_ASK_WINDOW", "3"))   # "ask now" = census this year .. +3
THIS_YEAR     <- as.integer(format(Sys.Date(), "%Y"))
UA            <- "EGRISS GAIN research (UNHCR)"
get_json <- function(u) request(u) |> req_user_agent(UA) |> req_timeout(60) |>
  req_retry(max_tries = 3) |> req_perform() |> resp_body_json(simplifyVector = TRUE)

# ---- 1. UNSD census dates -----------------------------------------------------
unsd_url <- "https://unstats.un.org/unsd/demographic-social/census/censusdates/"
pg <- request(unsd_url) |> req_user_agent(UA) |> req_timeout(60) |> req_perform() |>
  resp_body_string() |> read_html()
unsd_updated <- str_extract(html_text(pg), "Last update:\\s*[0-9]{1,2} [A-Za-z]+ [0-9]{4}")
rows <- html_elements(pg, "div.row")
unsd <- map_dfr(rows, function(r) {
  cells <- html_elements(r, xpath = "./div[contains(@class,'col-md-2')]")
  if (length(cells) != 6) return(NULL)
  txt <- map_chr(cells, function(cl) {
    xml_remove(html_elements(cl, "div.headline"))      # drop the column headings
    str_squish(html_text(cl)) })
  if (!nzchar(txt[1]) || str_detect(txt[1], "^(AFRICA|AMERICA|ASIA|EUROPE|OCEANIA)")) return(NULL)
  tibble(unsd_country = txt[1], r2010 = txt[4], r2020 = txt[5], r2030 = txt[6])
})
last_year <- function(x) { y <- as.integer(str_extract_all(coalesce(x, ""), "(19|20)[0-9]{2}")[[1]]); if (length(y)) max(y) else NA_integer_ }
unsd <- unsd %>% mutate(
  # UNSD names carry footnote markers: "Germany (8) (18)", "Belgium (4)"
  country   = harmonize_country(str_squish(str_remove_all(unsd_country, "\\(\\d+\\)"))),
  y2020     = map_int(r2020, last_year),
  y2030     = map_int(r2030, last_year),
  census_year = coalesce(y2030, y2020),
  census_date_unsd = if_else(!is.na(y2030), r2030, r2020)) %>%
  distinct(country, .keep_all = TRUE)
message(sprintf("UNSD: %d countries (%s)", nrow(unsd), coalesce(unsd_updated, "update date not found")))

# ---- 2. UNFPA Global Census Tracker (ArcGIS FeatureServer) -------------------
unfpa_layer <- "https://services5.arcgis.com/aQMqya7Haac8J82d/arcgis/rest/services/cesus_tracker_test/FeatureServer/0"
uf <- get_json(paste0(unfpa_layer, "/query?where=1%3D1&outFields=*&returnGeometry=false&f=json"))
ufa <- as_tibble(uf$features$attributes)
unfpa <- ufa %>%
  transmute(iso3 = ISO3CD, country = harmonize_country(coalesce(Country, ROMNAM)),
            unfpa_census_date = Actual_census_date, unfpa_census_year = Actual_census_year,
            unfpa_pilot = Date_of_pilot_census, unfpa_method = Data_collection_method,
            unfpa_previous = Previous_census, unfpa_covid = Impact_of_covid_19) %>%
  filter(!is.na(iso3)) %>% distinct(iso3, .keep_all = TRUE)
lyr <- get_json(paste0(unfpa_layer, "?f=json"))
unfpa_edited <- if (!is.null(lyr$editingInfo$lastEditDate))
  format(as.POSIXct(lyr$editingInfo$lastEditDate / 1000, origin = "1970-01-01"), "%d %b %Y") else NA
message(sprintf("UNFPA tracker: %d rows (layer last edited %s)", nrow(unfpa), unfpa_edited))

# ---- 3. UNHCR population hosted (latest year with data) -----------------------
unhcr <- NULL
for (yr in c(THIS_YEAR, THIS_YEAR - 1, THIS_YEAR - 2)) {
  u <- sprintf("https://api.unhcr.org/population/v1/population/?limit=1000&dataset=population&yearFrom=%d&yearTo=%d&coa_all=true", yr, yr)
  j <- tryCatch(get_json(u), error = function(e) NULL)
  if (!is.null(j) && length(j$items) && NROW(j$items) > 50) { unhcr <- as_tibble(j$items) %>% mutate(unhcr_year = yr); break }
}
num <- function(x) suppressWarnings(as.numeric(x))
unhcr_names <- unhcr %>% transmute(country = harmonize_country(coa_name), iso3 = coa_iso)
unhcr <- unhcr %>% transmute(iso3 = coa_iso, unhcr_year,
  refugees = num(refugees), asylum_seekers = num(asylum_seekers), idps = num(idps),
  stateless = num(stateless), others = num(ooc)) %>%
  mutate(displaced_total = rowSums(across(c(refugees, asylum_seekers, idps, stateless, others)), na.rm = TRUE))
message(sprintf("UNHCR: %d countries (year %s)", nrow(unhcr), unhcr$unhcr_year[1]))

# ---- 4. GAIN census examples + 5. NSO contacts --------------------------------
g <- suppressMessages(read_csv("analysis_ready_group_roster.csv", show_col_types = FALSE)) %>%
  mutate(country = harmonize_country(mcountry))
gain_census <- g %>%
  filter(str_detect(toupper(coalesce(PRO08, "")), "CENSUS") |
         str_detect(tolower(coalesce(PRO03, "")), "census|recensement|censo|rgph|nphc")) %>%
  group_by(country) %>%
  summarise(gain_census_examples = n(), gain_census_titles = paste(unique(str_sub(PRO03, 1, 60)), collapse = " | "),
            gain_census_last_year = suppressWarnings(max(as.integer(PRO04_year), na.rm = TRUE)), .groups = "drop") %>%
  mutate(gain_census_last_year = if_else(is.finite(gain_census_last_year), gain_census_last_year, NA_integer_))
gain_any <- g %>% count(country, name = "gain_examples_total")

nso_contact <- tibble(country = character(), nso_contact = logical())
if (!is.na(CONTACT_XLSX)) {
  s <- openxlsx::read.xlsx(CONTACT_XLSX, "Sample Survey 2026", sep.names = " ")
  b <- tryCatch(openxlsx::read.xlsx(CONTACT_XLSX, "Bounced Email Log", sep.names = " "), error = function(e) NULL)
  em <- grep("^Email", names(s), value = TRUE)[1]
  bounced <- if (is.null(b)) character() else b[["Bounced Email"]]
  s <- s %>% filter(!is.na(.data[[em]]), !(.data[[em]] %in% bounced))
  nso_contact <- s %>% mutate(country = harmonize_country(Country)) %>%
    filter(str_detect(tolower(NEW_ORG_NAME), "statist|\\bstats\\b|census|population commission|bureau of stat|instituto nacional|institut national|planning")) %>%
    distinct(country) %>% mutate(nso_contact = TRUE)
}

# ---- join ----------------------------------------------------------------------
# name -> ISO3 from EVERY spelling the two coded sources use (UNFPA Country and
# ROMNAM, UNHCR coa_name), harmonised the same way as the UNSD names
iso_by_name <- bind_rows(
    ufa %>% transmute(country = harmonize_country(Country), iso3 = ISO3CD),
    ufa %>% transmute(country = harmonize_country(ROMNAM),  iso3 = ISO3CD),
    unhcr_names) %>%
  filter(!is.na(country), nzchar(country), !is.na(iso3)) %>% distinct(country, .keep_all = TRUE)
out <- unsd %>%
  left_join(iso_by_name, by = "country") %>%
  left_join(unfpa %>% select(-country), by = "iso3") %>%
  left_join(unhcr, by = "iso3") %>%
  left_join(gain_census, by = "country") %>%
  left_join(gain_any, by = "country") %>%
  left_join(nso_contact, by = "country") %>%
  mutate(nso_contact = coalesce(nso_contact, FALSE),
         gain_census_examples = coalesce(gain_census_examples, 0L),
         gain_examples_total = coalesce(gain_examples_total, 0L),
         displaced_total = coalesce(displaced_total, 0),
         action = case_when(
           !is.na(census_year) & census_year >= THIS_YEAR &
             census_year <= THIS_YEAR + ASK_WINDOW                                            ~ "ask now: planned census",
           !is.na(census_year) & census_year > THIS_YEAR + ASK_WINDOW                         ~ "later: census after window",
           !is.na(census_year) & census_year >= 2022 & gain_census_examples == 0             ~ "ask: recent census, not in GAIN",
           gain_census_examples > 0 & !is.na(census_year) &
             census_year > coalesce(gain_census_last_year, 0L)                                ~ "in GAIN: ask for update",
           TRUE                                                                              ~ "context"),
         askable = !action %in% c("context", "later: census after window"),
         priority = if_else(askable & displaced_total >= DISPLACED_MIN, "high",
                            if_else(askable, "normal", "-"))) %>% select(-askable) %>%
  arrange(desc(priority == "high"), action, desc(displaced_total)) %>%
  select(country, iso3, action, priority, census_year, census_date_unsd, unfpa_census_date,
         unfpa_pilot, unfpa_method, unfpa_previous, displaced_total, refugees, asylum_seekers,
         idps, stateless, unhcr_year, gain_census_examples, gain_census_titles,
         gain_census_last_year, gain_examples_total, nso_contact)

unmatched <- out %>% filter(is.na(iso3)) %>% pull(country)
if (length(unmatched)) message("No ISO3 match (no UNFPA/UNHCR data joined) for ", length(unmatched), ": ",
                               paste(head(unmatched, 25), collapse = ", "))

stamp <- format(Sys.Date(), "%Y%m%d")
attr_line <- sprintf("Sources: UNSD census dates (%s); UNFPA Global Census Tracker (layer edited %s); UNHCR population data %s.",
                     coalesce(unsd_updated, "n/a"), unfpa_edited, out$unhcr_year[!is.na(out$unhcr_year)][1])
readr::write_excel_csv(out, sprintf("upcoming_censuses_%s.csv", stamp))
dir.create("powerbi_export", showWarnings = FALSE)
readr::write_excel_csv(out, "powerbi_export/WEB_GAIN_upcoming_censuses.csv")
writeLines(attr_line, "powerbi_export/WEB_GAIN_upcoming_censuses_SOURCES.txt")

message("\n", attr_line)
message("\n==== action x priority ====")
print(count(out, action, priority))
message("\n==== top 'ask now' (planned census, most displaced people hosted) ====")
print(out %>% filter(action == "ask now: planned census") %>% head(15) %>%
        transmute(country, census_year, census = str_sub(census_date_unsd, 1, 30),
                  displaced = format(displaced_total, big.mark = ","), in_gain = gain_census_examples, nso_contact))
message("wrote upcoming_censuses_", stamp, ".csv + powerbi_export/WEB_GAIN_upcoming_censuses.csv")
