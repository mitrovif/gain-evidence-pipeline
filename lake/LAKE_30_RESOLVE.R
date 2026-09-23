# ==============================================================================
# GAIN DATA LAKE - RESOLVE the dataset shortlist to ACCESS STATUS + PORTAL.
#
# The IHSN API does not expose access status cleanly, but access is reliably set by
# the portal each survey family lives on. We classify each shortlisted survey by its
# real download portal and whether it is download-now (free/public-use) or apply
# (licensed), and give the exact site to fetch it from. Output: lake_dataset_resolved.csv.
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
sh <- suppressMessages(read_csv(file.path(LAKE,"lake_dataset_shortlist.csv"), show_col_types=FALSE))

out <- sh %>% mutate(
  access_status = case_when(
    type=="MICS"                   ~ "1 DOWNLOAD NOW - free (register once at MICS portal, request the survey)",
    type=="DHS"                    ~ "1 DOWNLOAD NOW - free account (register at DHS, short data request ~1 day)",
    type=="living conditions/LSMS" ~ "2 MOSTLY PUBLIC-USE - accept terms & download; some licensed (apply)",
    type=="labour force"           ~ "2 MOSTLY PUBLIC-USE - WB/NSO, accept terms; some on national site only",
    type=="FDP socio-economic"     ~ "3 APPLY - licensed request (intended-use form, committee review)",
    TRUE                           ~ "4 CHECK record - follow 'Get Microdata' to the host repository"),
  portal = case_when(
    type=="MICS"                   ~ "https://mics.unicef.org/surveys",
    type=="DHS"                    ~ "https://dhsprogram.com/data/available-datasets.cfm",
    type=="living conditions/LSMS" ~ "https://microdata.worldbank.org",
    type=="labour force"           ~ "https://microdata.worldbank.org  (or national statistics office)",
    type=="FDP socio-economic"     ~ "https://microdata.unhcr.org",
    TRUE                           ~ "https://catalog.ihsn.org")) %>%
  arrange(access_status, gain_country) %>%
  transmute(access_status, gain_country, type, survey, year, portal, ihsn_record=catalog)
readr::write_excel_csv(out, file.path(LAKE,"lake_dataset_resolved.csv"))

message(sprintf("==== dataset list resolved: %d surveys ====\n", nrow(out)))
message("by access status (what you do to get it):")
print(out %>% count(access_status) %>% as.data.frame(), right=FALSE)
message(sprintf("\n=> DOWNLOAD-NOW (free): %d  |  public-use (accept terms): %d  |  apply (licensed): %d  |  check: %d",
        sum(str_detect(out$access_status,"^1")), sum(str_detect(out$access_status,"^2")),
        sum(str_detect(out$access_status,"^3")), sum(str_detect(out$access_status,"^4"))))
