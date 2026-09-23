# ==============================================================================
# GAIN DATA LAKE - FILL the review sheet: my best determination per example, so
# the user just clicks to confirm/download. Every candidate is kept for override.
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
cand <- suppressMessages(read_csv(file.path(LAKE,"lake_candidates.csv"), show_col_types=FALSE))

# ---- classify the EXAMPLE: is it actually a survey/dataset, or something else? ----
status_of <- function(title, desc){ t <- tolower(paste(title, desc))
  case_when(
    str_detect(t, "planned to be impl|under revision|will aim|to be implemented|planning to implement|will be (conducted|collected)|foundations for implementing") ~ "forthcoming (not yet collected)",
    str_detect(t, "repatriation|resettlement|launched by|voluntary return|pilot .*project|resettlement city") ~ "operation/event (no microdata)",
    str_detect(t, "^i use|i tend to|geospatial analytics|using .*analytics|accessibility analytics") ~ "method note (no dataset)",
    str_detect(t, "workshop|training|webinar|e-learning|academy|guidance|toolkit|expert group|technical working group|guidelines on|support to|towards greater inclusion|articulation between|south.?south|coordination|update of statistic|development of (new )?statistic|extension of publications|building the evidence|data-driven approaches") ~ "activity/project (may USE a survey, not a dataset itself)",
    TRUE ~ "survey/dataset") }

# distinctive-token overlap for the best-pick heuristic
sw <- c("the","of","and","for","de","la","in","on","to","a","survey","study","data","report","national","project","joint","center","funded","household","assessment","socio","economic")
tk <- function(s){ x<-str_split(str_squish(gsub("[^a-z0-9 ]"," ",tolower(coalesce(s,""))))," ")[[1]]; setdiff(x[nchar(x)>3],sw) }
yr <- function(s) suppressWarnings(as.integer(str_extract(s,"(19|20)\\d{2}")))

ex <- ro %>% select(example_id, gtitle=title, gdesc=description, country) %>%
  mutate(my_status = status_of(gtitle, gdesc))

pick <- cand %>% left_join(ex %>% select(example_id, gtitle, my_status), by="example_id") %>%
  filter(my_status=="survey/dataset") %>%
  mutate(sim = map2_dbl(map(gtitle,tk), map(candidate,tk), ~ if(!length(.x)||!length(.y)) 0 else length(intersect(.x,.y))/length(union(.x,.y)),),
         yr_gap = abs(coalesce(yr(gtitle),9999) - coalesce(suppressWarnings(as.integer(year)),0))) %>%
  group_by(example_id) %>% arrange(desc(sim), yr_gap) %>% slice(1) %>% ungroup() %>%
  mutate(my_confidence = case_when(sim>=0.2 ~ "med (verify)", sim>=0.08 ~ "low (verify)", TRUE ~ "guess (likely wrong)"))

filled <- ex %>%
  left_join(pick %>% select(example_id, my_pick=candidate, my_pick_id=cand_link, my_confidence, sim), by="example_id") %>%
  mutate(my_pick_id = sub(".*catalog/","",coalesce(my_pick_id,"")),
         recommendation = case_when(
           my_status!="survey/dataset" ~ paste0("SKIP - ", my_status),
           is.na(my_pick) ~ "no IHSN candidate found - reach out / other source",
           TRUE ~ paste0("try: ", substr(my_pick,1,44), " [", my_pick_id, "] (", my_confidence, ")"))) %>%
  filter(example_id %in% ro$example_id[ro$is_microdata_capable]) %>%
  transmute(example_id, gain_example=substr(gtitle,1,52), my_status, recommendation, my_pick_id, click_confirm="")
readr::write_excel_csv(filled, file.path(LAKE,"lake_review_filled.csv"))

message(sprintf("FILLED review sheet -> lake_review_filled.csv (%d microdata-capable examples)\n", nrow(filled)))
message("by my status:"); print(filled %>% mutate(s=sub(" .*","",my_status)) %>% count(my_status, sort=TRUE))
message(sprintf("\nexamples with a candidate to try: %d | skip (not a dataset): %d | no candidate: %d",
        sum(str_detect(filled$recommendation,"^try")), sum(str_detect(filled$recommendation,"^SKIP")), sum(str_detect(filled$recommendation,"no IHSN"))))
message("\n=== examples with a candidate to try (click these) ===")
print(filled %>% filter(str_detect(recommendation,"^try")) %>% transmute(example_id, gain=substr(gain_example,1,38), rec=substr(recommendation,1,60)) %>% as.data.frame(), right=FALSE)
