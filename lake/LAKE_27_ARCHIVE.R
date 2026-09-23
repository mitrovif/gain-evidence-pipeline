# ==============================================================================
# GAIN DATA LAKE - the GAIN EXAMPLES ARCHIVE.
#
# One entry per example reported to GAIN (all rounds), consolidating: the record,
# what we hold (downloaded data / report / nothing), the acquisition route to use
# (online download / extract-from-report / reach-out), the confirmed dataset where
# found, and SDG/identification tags. This is the durable archive that feeds the
# SDG-indicator and progress-measure workstreams. Output: gain_archive_index.csv.
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"; STORE <- file.path(LAKE,"store")
rd <- function(f) if (file.exists(file.path(LAKE,f))) suppressMessages(read_csv(file.path(LAKE,f), show_col_types=FALSE)) else tibble()
ro <- rd("lake_roster.csv"); fr <- rd("lake_frame.csv")

# what we hold per example. Distinguish RELIABLE holdings from the unverified NADA
# search harvest (microlib_/nada_) that the title-audit found was mostly mis-matched.
datare  <- "^(dataset|drill|table|api)_.*[.](csv|xlsx?|zip|json)$|ecadefi|asylwesen|_csv[.]zip"   # genuine data mined from the example's own link
docre   <- "^(source[.]|text[.]txt)|[.]pdf$"                                                       # the example's own report/page (Tier-3 source)
candre  <- "^(microlib|nada)_"                                                                     # unverified search-matched candidate, NOT proof of data
held <- function(id){ d<-file.path(STORE,id); if(!dir.exists(d)) return(list(data=FALSE,doc=FALSE,cand=FALSE,files="")); fs<-list.files(d)
  list(data=any(grepl(datare,fs)), doc=any(grepl(docre,fs)), cand=any(grepl(candre,fs)), files=paste(head(fs,12),collapse="; ")) }

# confirmed datasets found in the finding flow (reliable, user-verified)
confirmed <- tribble(~example_id, ~confirmed_dataset, ~access,
  "ex299","Uganda RHCS 2018 (WB 3867)","open - downloaded",
  "ex269","Nigeria IDP Profile NE 2018 (WB 3410)","open - downloaded",
  "ex262","Honduras IDP Profiling 2018 (UNHCR 566)","licensed - applied",
  "ex103","Ethiopia SESRE 2023 (WB 6251)","licensed",
  "ex183","Ethiopia SESRE 2023 (WB 6251)","licensed",
  "ex200","Ukraine Intentions Survey #2 (UNHCR 783)","licensed")

arch <- ro %>%
  transmute(example_id, gain_pindex2, year, lead_type, country, organisation, title, populations,
            recommendations, phase, description, has_link, link_results, all_urls, is_microdata_capable) %>%
  left_join(fr %>% select(example_id, sdg_domains, id_framework, data_access, host_signal), by="example_id") %>%
  left_join(confirmed, by="example_id") %>%
  mutate(h = map(example_id, held), has_data=map_lgl(h,"data"), has_doc=map_lgl(h,"doc"),
         has_cand=map_lgl(h,"cand"), files_held=map_chr(h,"files"),
         unverified_candidate = has_cand & !has_data,
         route = case_when(
           !is.na(confirmed_dataset) ~ "CONFIRMED DATASET",
           has_data ~ "1-data in hand (download/archive)",
           has_doc  ~ "3-extract from report (doc in hand)",
           has_link ~ "1/3-fetch then extract (link, not yet pulled)",
           TRUE ~ "2-reach out (no online source)")) %>%
  select(-h)
readr::write_excel_csv(arch %>% select(-all_urls, -description), file.path(LAKE,"gain_archive_index.csv"))
readr::write_excel_csv(arch, file.path(LAKE,"gain_archive_full.csv"))

message(sprintf("==== GAIN examples archive: %d examples (all rounds) ====", nrow(arch)))
message("\nacquisition route (what to do for each):"); print(arch %>% count(route, sort=TRUE))
message(sprintf("\nheld now: genuine data %d | reports/pages %d | unverified NADA candidate only %d | nothing %d",
        sum(arch$has_data), sum(arch$has_doc & !arch$has_data), sum(arch$unverified_candidate & !arch$has_doc),
        sum(!arch$has_data & !arch$has_doc & !arch$has_cand)))
message(sprintf("confirmed datasets: %d | with an SDG tag: %d | with IRRS/IRIS/IROSS: %d",
        sum(!is.na(arch$confirmed_dataset)), sum(nzchar(coalesce(arch$sdg_domains,""))), sum(nzchar(coalesce(arch$id_framework,"")))))
message("\nwrote gain_archive_index.csv + gain_archive_full.csv")
