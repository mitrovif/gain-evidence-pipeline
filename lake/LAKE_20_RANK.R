# ==============================================================================
# GAIN DATA LAKE - rank all examples by EASE OF DOWNLOAD, pick the easiest 150.
#
# Consolidates every signal (in-hand files, API-connectable, data-file links,
# reachability, web/drill matches + the title audit) into one ease tier per
# example, so we can hand back the N easiest-to-download sources, datasets first.
# ==============================================================================
suppressMessages({ library(tidyverse) })
L <- "data_lake"; STORE <- file.path(L,"store")
rd <- function(f) if (file.exists(file.path(L,f))) suppressMessages(read_csv(file.path(L,f), show_col_types=FALSE)) else tibble()
ro <- rd("lake_roster.csv"); plan <- rd("lake_download_plan.csv") %>% distinct(example_id,.keep_all=TRUE)
dl <- rd("lake_dataset_links.csv"); api <- rd("lake_api_connectable.csv"); web <- rd("lake_websearch_found.csv")
drill <- rd("lake_drill.csv"); au <- rd("lake_match_audit.csv"); bp <- rd("lake_browser_pull.csv")
hu <- function(x) !is.na(x) & nzchar(x)

datare <- "^(dataset|drill|table|api|microlib)_.*[.](csv|xlsx?|zip|json|xml)$|ecadefi|asylwesen|_csv[.]zip"
in_hand <- function(id){ d<-file.path(STORE,id); if(!dir.exists(d)) return(FALSE); any(grepl(datare, list.files(d))) }
has_datafile_link <- dl %>% filter(kind %in% c("csv","xlsx","xls","json","zip")) %>% distinct(example_id) %>% pull(example_id)
audit_v <- au %>% group_by(example_id) %>% summarise(av=ifelse(any(verdict=="CONFIRMED"),"CONFIRMED",ifelse(any(verdict=="SUSPECT"),"SUSPECT","MISMATCH")), .groups="drop")

x <- ro %>% select(example_id, year, country, organisation, title, lead_type, is_microdata_capable, populations) %>%
  left_join(plan %>% select(example_id, action, reachability, source_type), by="example_id") %>%
  left_join(api %>% distinct(example_id) %>% mutate(api=TRUE), by="example_id") %>%
  left_join(web %>% filter(verdict=="MATCH", hu(found_url)) %>% distinct(example_id) %>% mutate(webok=TRUE), by="example_id") %>%
  left_join(drill %>% filter(verdict=="MATCH", hu(drilled_url)) %>% distinct(example_id) %>% mutate(drillok=TRUE), by="example_id") %>%
  left_join(audit_v, by="example_id") %>%
  mutate(inhand = map_lgl(example_id, in_hand), has_file_link = example_id %in% has_datafile_link,
         ease_tier = case_when(
           inhand ~ 1L,
           !is.na(api) ~ 2L,
           has_file_link ~ 3L,
           reachability=="live" & source_type %in% c("web_page") & !is.na(drillok) ~ 4L,
           reachability=="live" ~ 5L,
           !is.na(webok) ~ 5L,
           reachability=="blocked" ~ 6L,
           TRUE ~ 9L),
         method = recode(as.character(ease_tier), `1`="already in hand", `2`="NSO/Eurostat API pull",
           `3`="direct data-file link", `4`="page with dataset (fetch+extract)", `5`="scrape live page/office site",
           `6`="needs browser (blocked)", `9`="no open source (gated/no-link/activity/forthcoming)"),
         reliability = coalesce(av, ifelse(source_type=="none","-", "roster-own")))

rank <- x %>% filter(ease_tier <= 6) %>%
  arrange(ease_tier, desc(is_microdata_capable), match(reliability, c("CONFIRMED","roster-own","SUSPECT","MISMATCH","-"))) %>%
  mutate(rank = row_number())
top150 <- head(rank, 150)
readr::write_excel_csv(rank %>% transmute(rank, example_id, country, organisation, title=substr(title,1,60),
  is_microdata_capable, ease_tier, method, reliability), file.path(L,"lake_download_ranked.csv"))
readr::write_excel_csv(head(rank,150) %>% transmute(rank, example_id, country, organisation, title=substr(title,1,60),
  is_microdata_capable, ease_tier, method, reliability), file.path(L,"lake_easiest_150.csv"))

message(sprintf("downloadable examples (tier<=6): %d of %d roster", nrow(rank), nrow(x)))
message("\n=== by ease tier (all downloadable) ===")
print(rank %>% count(ease_tier, method))
message(sprintf("\n=== the easiest %d (what we can realistically get in hand) ===", nrow(top150)))
print(top150 %>% count(ease_tier, method))
message(sprintf("of the top %d, microdata/dataset-type: %d | tables/aggregate: %d",
        nrow(top150), sum(top150$is_microdata_capable), sum(!top150$is_microdata_capable)))
message(sprintf("\nSHORTFALL vs 150: %d (only %d examples have ANY open download path)", max(0,150-nrow(rank)), nrow(rank)))
