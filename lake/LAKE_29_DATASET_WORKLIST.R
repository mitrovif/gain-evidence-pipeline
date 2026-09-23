# ==============================================================================
# GAIN DATA LAKE - DATASET-FIRST worklist.
#
# User priority: DATASETS first (reports only as fallback). This ranks the 565
# socio-economic surveys by how easily the microdata can actually be pulled, spells
# out the download route per survey, and keeps a focused "latest per country+type"
# shortlist so the download run is tractable. Output: lake_dataset_worklist.csv
# (full) + lake_dataset_shortlist.csv (latest per country x type).
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
s <- suppressMessages(read_csv(file.path(LAKE,"lake_socioeconomic_microdata.csv"), show_col_types=FALSE))

route <- function(type, source){ case_when(
  type=="MICS"                    ~ "UNICEF MICS portal (mics.unicef.org) - register once, then free microdata download",
  type=="DHS"                     ~ "DHS Program (dhsprogram.com) - free account + short request, approved in ~1 day",
  type=="living conditions/LSMS"  ~ "World Bank Microdata (microdata.worldbank.org) - Public-Use: accept terms & download; Licensed: apply",
  type=="labour force"            ~ "ILO/NSO or World Bank Microdata - often Public-Use or on the national statistics office site",
  type=="FDP socio-economic"      ~ "UNHCR Microdata (microdata.unhcr.org) / World Bank - usually Licensed: apply with intended use",
  TRUE                            ~ "IHSN catalog record - follow 'Get Microdata' to the hosting repository") }
ease <- function(type){ case_when(type=="MICS"~1L, type=="living conditions/LSMS"~2L, type=="labour force"~3L, type=="DHS"~4L, type=="FDP socio-economic"~5L, TRUE~6L) }
yr <- function(x) suppressWarnings(as.integer(str_extract(as.character(x),"(19|20)\\d{2}")))

work <- s %>% mutate(download_route=route(type,source), ease_rank=ease(type), yr=coalesce(yr(year),yr(survey),0L)) %>%
  arrange(ease_rank, gain_country, desc(yr)) %>%
  transmute(ease_rank, gain_country, type, source, survey, year, download_route, catalog)
readr::write_excel_csv(work, file.path(LAKE,"lake_dataset_worklist.csv"))

# focused shortlist: the most recent survey per country x type (the one worth pulling first)
short <- work %>% group_by(gain_country, type) %>% arrange(desc(coalesce(yr(year),0L))) %>% slice(1) %>% ungroup() %>%
  arrange(ease_rank, gain_country)
readr::write_excel_csv(short, file.path(LAKE,"lake_dataset_shortlist.csv"))

message(sprintf("==== DATASET-FIRST worklist ====\nfull: %d surveys | shortlist (latest per country x type): %d\n", nrow(work), nrow(short)))
message("shortlist by ease/type (what to pull, easiest first):")
print(short %>% count(ease_rank, type, name="datasets") %>% arrange(ease_rank) %>% as.data.frame(), right=FALSE)
message(sprintf("\ncountries covered: %d", n_distinct(short$gain_country)))
message("\ntop of the shortlist (MICS first - free download):")
print(short %>% filter(type=="MICS") %>% transmute(country=gain_country, survey=substr(survey,1,40), year, catalog) %>% head(10) %>% as.data.frame(), right=FALSE)
