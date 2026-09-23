# ==============================================================================
# GAIN DATA LAKE - human-review sheet: pair each IHSN candidate with the GAIN
# example I think it is, + my confidence, for the user to confirm.
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
ih <- suppressMessages(read_csv(file.path(LAKE,"lake_ihsn_datasets.csv"), show_col_types=FALSE))

stop_w <- c("the","of","and","for","de","la","in","on","a","to","survey","study","assessment","household","data","report","project","national","population")
tok <- function(s){ t <- str_split(str_squish(gsub("[^a-z0-9 ]"," ",tolower(coalesce(s,"")))), " ")[[1]]; setdiff(t[nchar(t)>2], stop_w) }
jac <- function(a,b){ if(!length(a)||!length(b)) return(0); round(length(intersect(a,b))/length(union(a,b)),2) }
typ <- function(s){ s<-tolower(s); case_when(str_detect(s,"census|censo|recens")~"census", str_detect(s,"\\bfds\\b|forced displacement survey")~"fds",
  str_detect(s,"profil")~"profiling", str_detect(s,"dhs|mics|demographic")~"dhs/mics", str_detect(s,"nutrition")~"nutrition",
  str_detect(s,"needs assessment|msna")~"needs-assessment", str_detect(s,"phone|high frequency")~"phone-survey",
  str_detect(s,"budget")~"budget", str_detect(s,"survey|enqu")~"survey", TRUE~"other") }

rev <- ih %>% filter(verdict=="CONFIRMED") %>%
  left_join(ro %>% select(example_id, gtitle_full=title), by="example_id") %>%
  mutate(sim = map2_dbl(map(gtitle_full, tok), map(ihsn_title, tok), jac),
         gtype = typ(gtitle_full), itype = typ(ihsn_title), type_ok = gtype==itype | gtype %in% c("survey","other"),
         my_read = case_when(sim>=0.25 & type_ok ~ "LIKELY", (sim>=0.12 | type_ok) ~ "UNSURE", TRUE ~ "UNLIKELY (probably wrong instrument)")) %>%
  arrange(match(my_read,c("LIKELY","UNSURE","UNLIKELY (probably wrong instrument)")), desc(sim)) %>%
  transmute(gain_example=example_id, country, gain_survey=substr(gtitle_full,1,50),
            ihsn_candidate=substr(ihsn_title,1,50), my_read, similarity=sim, ihsn_link=catalog, your_confirm="")
readr::write_excel_csv(rev, file.path(LAKE,"lake_ihsn_review.csv"))
# examples with no candidate
none <- ih %>% filter(verdict!="CONFIRMED") %>% left_join(ro%>%select(example_id,t=title),by="example_id")

message(sprintf("review sheet: %d candidate pairs (%d with no candidate) -> lake_ihsn_review.csv\n", nrow(rev), nrow(none)))
message("my read of the pairs:"); print(count(rev, my_read))
message("\n=== pairs to confirm (best first) ===")
print(rev %>% select(gain_example, country, gain_survey, ihsn_candidate, my_read) %>% as.data.frame(), right=FALSE)
