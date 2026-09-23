# ==============================================================================
# GAIN DATA LAKE - candidates-per-example sheet (human picks; no LLM guessing).
#
# The LLM auto-pick keeps mis-attaching (Belgium example got the Ukraine survey).
# IHSN search itself is good, so for each microdata-capable example we return the
# top IHSN candidates + catalog links and let the user pick the correct one.
# Two query variants per example (country+type+pop, and country+distinctive terms).
# Output: lake_candidates.csv (long: example -> ranked candidates + links).
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
source("shared/GAIN_COMMON.R")
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
ACT <- "workshop|training|webinar|e-?learn|academy|guidance|toolkit|expert group|capacity|methodolog|coordinat|strateg|\\bcourse\\b|geospatial analytics|note on"
found <- c("ex299","ex269","ex262","ex103","ex183","ex200")   # already secured
mc <- ro %>% filter(is_microdata_capable, !str_detect(tolower(title), ACT), nzchar(coalesce(country,"")), !example_id %in% found)
message(sprintf("examples needing candidates: %d", nrow(mc)))

GET <- function(u) tryCatch(request(u)|>req_timeout(30)|>req_headers(`User-Agent`="Mozilla/5.0 EGRISS",Accept="application/json,*/*")|>req_options(followlocation=TRUE,ssl_verifypeer=0)|>req_error(is_error=\(x)FALSE)|>req_perform(), error=function(e) NULL)
sb <- function(rp) tryCatch(resp_body_string(rp), error=function(e) "")
BASE <- "https://catalog.ihsn.org"
srch <- function(q){ rp<-GET(sprintf("%s/index.php/api/catalog/search?sk=%s&ps=6",BASE,URLencode(q)))
  j<-tryCatch(fromJSON(sb(rp)),error=function(e)NULL); r<-j$result$rows; if(is.null(r)||!length(r)) return(tibble())
  d<-as_tibble(r); getc<-function(nm) if(nm %in% names(d)) as.character(d[[nm]]) else rep("",nrow(d))
  tibble(id=getc("id"), title=getc("title"), nation=getc("nation"), year=getc("year_start")) %>% head(6) }
stopw <- c("the","of","and","for","de","la","project","survey","study","data","report","national","joint","center","funded","household")
q1 <- function(title,country,pop){ t<-tolower(paste(title,pop))
  typ<-if(str_detect(t,"census|censo|recens"))"census" else if(str_detect(t,"survey|enqu|encuesta|assessment|profiling|monitoring|insights"))"survey" else ""
  popk<-if(str_detect(t,"refugee|réfugi|refugiad"))"refugee" else if(str_detect(t,"idp|displaced|déplac|desplaz"))"displaced" else if(str_detect(t,"stateless|apatri"))"stateless" else ""
  str_squish(paste(country,popk,typ)) }
q2 <- function(title,country){ toks<-str_split(str_squish(gsub("[^a-z ]"," ",tolower(title)))," ")[[1]]; toks<-setdiff(toks[nchar(toks)>3],stopw)
  str_squish(paste(paste(head(toks,3),collapse=" "), country)) }

out <- list()
for (i in seq_len(nrow(mc))) { r <- mc[i,]
  cw <- substr(tolower(str_split(r$country," ")[[1]][1]),1,5)
  cand <- bind_rows(srch(q1(r$title,r$country,r$populations)), srch(q2(r$title,r$country))) %>%
    filter(str_detect(tolower(nation), fixed(cw))) %>% distinct(id, .keep_all=TRUE) %>% head(5)
  if (nrow(cand)) out[[length(out)+1]] <- cand %>% mutate(example_id=r$example_id, gain_title=substr(r$title,1,50),
    cand_rank=row_number(), cand_link=sprintf("%s/index.php/catalog/%s",BASE,id))
  message(sprintf("  [%d/%d] %s %s -> %d candidates", i, nrow(mc), r$example_id, substr(r$country,1,12), nrow(cand)))
  Sys.sleep(0.2)
}
res <- bind_rows(out) %>% transmute(example_id, gain_title, cand_rank, candidate=substr(title,1,52), year, cand_link, your_pick="")
readr::write_excel_csv(res, file.path(LAKE,"lake_candidates.csv"))
message(sprintf("\nwrote lake_candidates.csv: %d candidates across %d examples (%d examples with >=1 candidate)",
        nrow(res), n_distinct(mc$example_id), n_distinct(res$example_id)))
message(sprintf("examples with NO IHSN candidate: %d", nrow(mc) - n_distinct(res$example_id)))
