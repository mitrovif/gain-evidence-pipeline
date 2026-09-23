# ==============================================================================
# GAIN DATA LAKE - match UNHCR + WB(displacement) microdata studies to the GAIN
# examples they are the data FOR. User rule: take UNHCR studies ONLY if they
# correspond to a GAIN example. Conservative: rank confidence by country + title-
# token overlap + year; present for human confirmation. Output: gain_microdata_matches.csv
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
un <- suppressMessages(read_csv(file.path(LAKE,"unhcr_gain.csv"), show_col_types=FALSE)) %>%
  transmute(source="UNHCR", idno, s_country=gain_country, s_title=title, s_year=suppressWarnings(as.integer(year_start)),
            dl=download_api, page)
wb <- suppressMessages(read_csv(file.path(LAKE,"wb_displacement.csv"), show_col_types=FALSE)) %>%
  transmute(source="WB", idno, s_country=gain_country, s_title=title, s_year=suppressWarnings(as.integer(year_start)),
            dl=dl_files_api, page=sprintf("https://microdata.worldbank.org/index.php/catalog/%s", idno))
pool <- bind_rows(un, wb)

STOP <- c("the","of","and","for","de","la","in","on","to","a","survey","study","data","report","national",
          "household","assessment","socio","economic","socioeconomic","monitoring","phone","high","frequency",
          "wave","round","community","based","2020","2021","2022","2023","2024","2019","2018","refugee","refugees","idp","idps")
tok <- function(s){ x<-str_split(str_squish(gsub("[^a-z0-9 ]"," ",tolower(coalesce(s,""))))," ")[[1]]; setdiff(x[nchar(x)>3], STOP) }
yr  <- function(s) suppressWarnings(as.integer(str_extract(as.character(s),"(19|20)\\d{2}")))
# exact-ish country compare (avoid Niger/Nigeria): compare full lowercased names
ceq <- function(a,b){ a<-tolower(coalesce(a,"")); b<-tolower(coalesce(b,"")); a==b | startsWith(a,b) | startsWith(b,a) }

ex <- ro %>% filter(is_microdata_capable, nzchar(coalesce(country,""))) %>%
  transmute(example_id, e_country=country, e_title=title, e_year=coalesce(yr(title), yr(description), as.integer(year)),
            e_tok=map(paste(title, populations), tok))

best <- map_dfr(seq_len(nrow(ex)), function(i){ e<-ex[i,]
  cand <- pool %>% filter(map_lgl(s_country, ~ceq(.x, e$e_country[[1]])))
  if(!nrow(cand)) return(tibble(example_id=e$example_id, matched=NA))
  cand %>% mutate(
      sim = map_dbl(map(s_title, tok), ~{ a<-e$e_tok[[1]]; b<-.x; if(!length(a)||!length(b)) 0 else length(intersect(a,b))/length(union(a,b)) }),
      ygap = ifelse(is.na(e$e_year[[1]])|is.na(s_year), 9, abs(e$e_year[[1]]-s_year))) %>%
    arrange(desc(sim), ygap) %>% slice(1) %>%
    transmute(example_id=e$example_id, source, idno, s_title, s_year, sim, ygap, dl, page) })

out <- ex %>% select(example_id, e_country, e_title, e_year) %>%
  left_join(best, by="example_id") %>%
  mutate(confidence = case_when(is.na(idno) ~ "no match",
                                sim>=0.30 | (sim>=0.18 & ygap<=1) ~ "STRONG",
                                sim>=0.15 | (sim>=0.08 & ygap<=1) ~ "MEDIUM",
                                TRUE ~ "weak")) %>%
  arrange(match(confidence,c("STRONG","MEDIUM","weak","no match")), desc(sim)) %>%
  transmute(example_id, gain_example=substr(e_title,1,46), country=e_country, source, matched_study=substr(s_title,1,46),
            idno, confidence, sim=round(sim,2), download_api=dl, catalog_page=page)
readr::write_excel_csv(out, file.path(LAKE,"gain_microdata_matches.csv"))

message(sprintf("GAIN examples (microdata-capable) matched to UNHCR/WB microdata: %d\n", nrow(out)))
message("by confidence:"); print(count(out, confidence) %>% as.data.frame(), right=FALSE)
message("\n=== STRONG matches (data behind these GAIN examples) ===")
print(out %>% filter(confidence=="STRONG") %>% transmute(example_id, country, gain_example=substr(gain_example,1,34), source, idno) %>% as.data.frame(), right=FALSE)
