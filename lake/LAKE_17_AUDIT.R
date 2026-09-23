# ==============================================================================
# GAIN DATA LAKE - QA: strict TITLE-LEVEL audit of every matched attachment.
#
# Earlier verification passed on topic+country, which lets a different survey by
# the same agency/country through (FDS Cameroon matched to a COVID assessment;
# a livelihood study matched to cash-monitoring). This re-checks EVERY matched
# route (microlib, web-search, drill, API-connect, recovery) at the survey-NAME
# level: is the attached source the SAME specific statistical activity the GAIN
# example names? CONFIRMED / SUSPECT / MISMATCH. Roster-OWN links are not re-judged
# (the reporting office declared them).
# ==============================================================================
suppressMessages({ library(tidyverse) })
source("shared/GAIN_CONFIG.R"); source("shared/GAIN_COMMON.R"); suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("LM Studio not reachable.")
LAKE <- "data_lake"
rd <- function(f) if (file.exists(file.path(LAKE,f))) suppressMessages(read_csv(file.path(LAKE,f), show_col_types=FALSE)) else tibble()
ro <- rd("lake_roster.csv")
hu <- function(x) !is.na(x) & nzchar(x)

web<-rd("lake_websearch_found.csv"); drill<-rd("lake_drill.csv"); api<-rd("lake_api_connectable.csv")
rec<-rd("lake_recovery_verified.csv"); mic<-rd("lake_microlib_found.csv")
cand <- bind_rows(
  if(nrow(mic))  mic %>% filter(verdict=="MATCH") %>% transmute(example_id, route="microlib", attached=study_title) else tibble(),
  if(nrow(web))  web %>% filter(verdict=="MATCH", hu(found_url)) %>% transmute(example_id, route="web-search", attached=paste(found_title, found_url)) else tibble(),
  if(nrow(drill))drill%>% filter(verdict=="MATCH", hu(drilled_url)) %>% transmute(example_id, route="drill", attached=paste(anchor, drilled_url)) else tibble(),
  if(nrow(api))  api %>% transmute(example_id, route="api-connect", attached=paste(dataset_label, dataset_code)) else tibble(),
  if(nrow(rec))  rec %>% filter(kept) %>% transmute(example_id, route="recovery", attached=recovered_url) else tibble()) %>%
  left_join(ro %>% select(example_id, g_title=title, g_org=organisation, g_country=country, g_pop=populations), by="example_id") %>%
  distinct(example_id, route, .keep_all=TRUE)

audit1 <- function(r){
  raw <- .ollama_generate(paste0(
    "Decide if an ATTACHED data source is the SAME SPECIFIC statistical activity that a GAIN example names, ",
    "or merely the same topic/country. A different survey/census/report by the same agency or country is a MISMATCH, ",
    "even if the population and theme overlap. Match the SPECIFIC named instrument.\n\n",
    "GAIN EXAMPLE:\n  title: ", r$g_title, "\n  organisation: ", r$g_org, "\n  country: ", r$g_country, "\n  populations: ", r$g_pop,
    "\n\nATTACHED SOURCE: ", substr(r$attached,1,200),
    "\n\nAnswer two lines:\nVERDICT: CONFIRMED (same specific instrument) or SUSPECT (topically close, unclear) or MISMATCH (different instrument)\nREASON: one short sentence."),
    timeout=100, json=FALSE)
  v <- toupper(str_match(coalesce(raw,""),"VERDICT[:\\s]*\\s*(CONFIRMED|SUSPECT|MISMATCH)")[,2])
  list(verdict=coalesce(v,"SUSPECT"), reason=substr(str_squish(sub(".*REASON[:\\s]*","",coalesce(raw,""))),1,150))
}
message(sprintf("title-level audit of %d matched attachments ...", nrow(cand)))
res <- pmap(cand, function(...) audit1(list(...)))
cand <- cand %>% mutate(verdict=map_chr(res,"verdict"), reason=map_chr(res,"reason"))
readr::write_excel_csv(cand %>% transmute(example_id, route, verdict, reason,
  gain_example=substr(g_title,1,50), attached=substr(attached,1,60)), file.path(LAKE,"lake_match_audit.csv"))

message("\n==== title-level audit ====")
print(cand %>% count(route, verdict) %>% pivot_wider(names_from=verdict, values_from=n, values_fill=0))
message(sprintf("\nCONFIRMED %d | SUSPECT %d | MISMATCH %d  (of %d matched attachments)",
        sum(cand$verdict=="CONFIRMED"), sum(cand$verdict=="SUSPECT"), sum(cand$verdict=="MISMATCH"), nrow(cand)))
message("\nsample MISMATCH / SUSPECT:")
print(cand %>% filter(verdict!="CONFIRMED") %>% transmute(example_id, route, verdict, gain=substr(g_title,1,34), attached=substr(attached,1,34)) %>% head(12) %>% as.data.frame(), right=FALSE)
