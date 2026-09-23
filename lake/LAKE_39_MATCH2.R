# ==============================================================================
# GAIN DATA LAKE - matcher v2. Match GAIN examples to portal studies by SURVEY
# PROGRAMME / ACRONYM + country (not generic tokens), and flag STOCK vs SOCIO-
# ECONOMIC. Rules: use any agency's study only if it is the data behind a GAIN
# example; if no programme match -> "reach out / not published" (no substitutes);
# stock-of-displaced deprioritised. Output: gain_matches_v2.csv
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
un <- suppressMessages(read_csv(file.path(LAKE,"unhcr_gain.csv"), show_col_types=FALSE)) %>%
  transmute(source="UNHCR", idno, s_country=gain_country, s_title=title, s_year=suppressWarnings(as.integer(year_start)), page)
wb <- suppressMessages(read_csv(file.path(LAKE,"wb_displacement.csv"), show_col_types=FALSE)) %>%
  transmute(source="WB", idno, s_country=gain_country, s_title=title, s_year=suppressWarnings(as.integer(year_start)),
            page=sprintf("https://microdata.worldbank.org/index.php/catalog/%s", idno))
pool <- bind_rows(un, wb) %>% mutate(txt=tolower(paste(idno, s_title)))

# programme signatures: code -> regex (matched on both example text and candidate text)
PROG <- tribble(~code, ~rx,
  "FDS","forced displacement survey|\\bfds\\b",
  "SESRE","socio.?economic survey of refugee|\\bsesre\\b",
  "VASYR","vulnerabilit.*syrian|\\bvasyr\\b",
  "RHCS","refugee and host communit|\\brhcs\\b|host and refugee",
  "HFPS","high.?frequency phone|\\bhfps\\b|phone survey",
  "CBPS","cox.?s bazar panel|\\bcbps\\b",
  "RMS","results monitoring survey|\\brms\\b",
  "LMS","labou?r market survey|\\blms\\b",
  "MSNA","multi.?sector.*needs assessment|\\bm?sna\\b|jmsna",
  "IDPPROF","profil.*(idp|internally displaced|solution)|idp.*profil|durable solution.*analys",
  "INTENTIONS","intention",
  "LIVELIHOOD","livelihood",
  "DTM","displacement tracking|\\bdtm\\b|site assessment|site and village",
  "CENSUS","\\bcensus\\b|housing and population",
  "MICS","multiple indicator cluster|\\bmics\\b",
  "DHS","demographic and health survey|\\bdhs\\b",
  "LSMS","living (standards|conditions)|\\blsms\\b|integrated household",
  "LFS","labou?r force survey|\\blfs\\b",
  "VULN","vulnerability assessment|\\bvaf\\b",
  "PROTMON","protection monitoring|\\bcbpm\\b|border monitor")
codes_of <- function(txt){ txt<-tolower(coalesce(txt,"")); PROG$code[map_lgl(PROG$rx, ~str_detect(txt,.x))] }
STOCK_CODES <- c("DTM","CENSUS","PROTMON")   # stock-of-displaced / counts, not socio-economic
ceq <- function(a,b){ a<-tolower(coalesce(a,"")); b<-tolower(coalesce(b,"")); a==b | startsWith(a,b) | startsWith(b,a) }
yr  <- function(s) suppressWarnings(as.integer(str_extract(as.character(s),"(19|20)[0-9]{2}")))

pool <- pool %>% mutate(cand_codes = map(txt, codes_of))
ex <- ro %>% filter(is_microdata_capable, nzchar(coalesce(country,""))) %>%
  transmute(example_id, e_country=country, e_title=title,
            e_year=coalesce(yr(title), yr(description), as.integer(year)),
            e_codes=map(paste(title, description), codes_of))

res <- map_dfr(seq_len(nrow(ex)), function(i){ e<-ex[i,]
  cc <- pool %>% filter(map_lgl(s_country, ~ceq(.x, e$e_country[[1]])))
  ecodes <- e$e_codes[[1]]
  if(!nrow(cc) || !length(ecodes)) return(tibble(example_id=e$example_id, match_idno=NA_character_, prog=NA_character_, verdict="no programme match -> reach out / not published"))
  cc <- cc %>% mutate(shared = map(cand_codes, ~intersect(.x, ecodes)),
                      nshared = map_int(shared, length),
                      ygap = ifelse(is.na(e$e_year[[1]])|is.na(s_year),9,abs(e$e_year[[1]]-s_year))) %>%
    filter(nshared>0) %>% arrange(desc(nshared), ygap)
  if(!nrow(cc)) return(tibble(example_id=e$example_id, match_idno=NA_character_, prog=NA_character_, verdict="no programme match -> reach out / not published"))
  top <- cc %>% slice(1)
  prog <- top$shared[[1]][1]
  is_stock <- all(top$shared[[1]] %in% STOCK_CODES)
  tibble(example_id=e$example_id, source=top$source, match_idno=top$idno, match_title=substr(top$s_title,1,50),
         match_year=top$s_year, prog=paste(top$shared[[1]],collapse=","),
         kind=ifelse(is_stock,"STOCK (deprioritise)","socio-economic"),
         ygap=top$ygap, page=top$page,
         verdict=ifelse(top$ygap<=2, "programme match", "programme match (different year - check)")) })

out <- ex %>% select(example_id, country=e_country, gain_example=e_title, e_year) %>%
  left_join(res, by="example_id") %>%
  mutate(gain_example=substr(gain_example,1,46)) %>%
  arrange(is.na(match_idno), desc(kind=="socio-economic"))
readr::write_excel_csv(out, file.path(LAKE,"gain_matches_v2.csv"))
message(sprintf("matched by programme: %d of %d microdata-capable examples", sum(!is.na(out$match_idno)), nrow(out)))
message("verdicts:"); print(count(out, verdict) %>% as.data.frame(), right=FALSE)
message("kind:"); print(count(out, kind) %>% as.data.frame(), right=FALSE)
