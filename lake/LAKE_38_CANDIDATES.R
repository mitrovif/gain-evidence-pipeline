# ==============================================================================
# GAIN DATA LAKE - per microdata-capable GAIN example, list the TOP candidate
# UNHCR/WB microdata studies in that country for HUMAN confirmation (title-match is
# unreliable, so we present options, not a single auto-pick). Confirmed matches are
# flagged. Output: gain_microdata_candidates.csv
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
un <- suppressMessages(read_csv(file.path(LAKE,"unhcr_gain.csv"), show_col_types=FALSE)) %>%
  transmute(source="UNHCR", idno, s_country=gain_country, s_title=title, s_year=suppressWarnings(as.integer(year_start)), page)
wb <- suppressMessages(read_csv(file.path(LAKE,"wb_displacement.csv"), show_col_types=FALSE)) %>%
  transmute(source="WB", idno, s_country=gain_country, s_title=title, s_year=suppressWarnings(as.integer(year_start)),
            page=sprintf("https://microdata.worldbank.org/index.php/catalog/%s", idno))
pool <- bind_rows(un, wb)

confirmed <- tibble(example_id=c("ex299","ex269","ex103","ex183","ex180","ex262","ex200"),
  confirmed_idno=c("UGA_2018_RHCS_v01_M","NGA_2018_*IDP","(WB) SESRE 2023","(WB) SESRE 2023","(WB) SESRE 2023","(UNHCR) Honduras IDP","(UNHCR) Ukraine Intentions"))

STOP <- c("the","of","and","for","de","la","survey","study","data","report","national","household","assessment",
          "socio","economic","monitoring","phone","high","frequency","wave","round","community","based","refugee","refugees","idp","idps","internally","displaced","persons","displacement")
tok <- function(s){ x<-str_split(str_squish(gsub("[^a-z0-9 ]"," ",tolower(coalesce(s,""))))," ")[[1]]; setdiff(x[nchar(x)>3], STOP) }
yr  <- function(s) suppressWarnings(as.integer(str_extract(as.character(s),"(19|20)[0-9]{2}")))
ceq <- function(a,b){ a<-tolower(coalesce(a,"")); b<-tolower(coalesce(b,"")); a==b | startsWith(a,b) | startsWith(b,a) }

ex <- ro %>% filter(is_microdata_capable, nzchar(coalesce(country,""))) %>%
  transmute(example_id, e_country=country, e_title=title, e_year=coalesce(yr(title), yr(description), as.integer(year)),
            e_tok=map(paste(title, populations), tok))

cand <- map_dfr(seq_len(nrow(ex)), function(i){ e<-ex[i,]
  cc <- pool %>% filter(map_lgl(s_country, ~ceq(.x, e$e_country[[1]])))
  if(!nrow(cc)) return(tibble())
  cc %>% mutate(sim=map_dbl(map(s_title,tok), ~{a<-e$e_tok[[1]]; b<-.x; if(!length(a)||!length(b)) 0 else length(intersect(a,b))/length(union(a,b))}),
                ygap=ifelse(is.na(e$e_year[[1]])|is.na(s_year),9,abs(e$e_year[[1]]-s_year))) %>%
    arrange(desc(sim), ygap) %>% group_by(source) %>% slice(1:3) %>% ungroup() %>%
    transmute(example_id=e$example_id, gain_example=substr(e$e_title,1,44), country=e$e_country[[1]],
              cand_source=source, candidate=substr(s_title,1,50), cand_idno=idno, cand_year=s_year, sim=round(sim,2), cand_page=page) })

out <- cand %>% left_join(confirmed, by="example_id") %>%
  mutate(is_confirmed = !is.na(confirmed_idno)) %>%
  arrange(desc(is_confirmed), example_id, desc(sim))
readr::write_excel_csv(out, file.path(LAKE,"gain_microdata_candidates.csv"))
message(sprintf("candidates for %d microdata-capable examples -> gain_microdata_candidates.csv (%d rows)", n_distinct(out$example_id), nrow(out)))
message(sprintf("examples with >=1 candidate: %d | confirmed already: %d", n_distinct(out$example_id), n_distinct(out$example_id[out$is_confirmed])))
