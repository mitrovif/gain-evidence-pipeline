# ==============================================================================
# GAIN DATA LAKE - find real DATASETS via IHSN (whose catalog search actually works).
#
# The UNHCR/WB NADA search is broken; IHSN (catalog.ihsn.org) honors queries and
# aggregates open national census/survey microdata. For each microdata-capable
# example we search IHSN by survey name + country, STRICT-verify at the instrument
# level, and record confirmed datasets + their access type (open vs licensed) so we
# can download the open ones. This is the reliable route to real datasets.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
source("shared/GAIN_CONFIG.R"); source("shared/GAIN_COMMON.R"); suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("LM Studio not reachable.")
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
ACT <- "workshop|training|webinar|e-?learn|academy|guidance|toolkit|expert group|capacity|methodolog|coordinat|strateg|\\bcourse\\b|geospatial analytics|note on"
mc <- ro %>% filter(is_microdata_capable, !str_detect(tolower(title), ACT), nzchar(coalesce(country,"")))
message(sprintf("microdata-capable, non-activity, with country: %d", nrow(mc)))

GET <- function(u) tryCatch(request(u)|>req_timeout(35)|>req_headers(`User-Agent`="Mozilla/5.0 EGRISS",Accept="application/json,*/*")|>req_options(followlocation=TRUE,ssl_verifypeer=0)|>req_error(is_error=\(x)FALSE)|>req_perform(), error=function(e) NULL)
sb <- function(rp) tryCatch(resp_body_string(rp), error=function(e) "")
BASE <- "https://catalog.ihsn.org"
DACC <- c(`1`="OPEN/direct", `2`="Public Use File", `3`="Licensed", `4`="Data enclave", `5`="External repo", `6`="Data request")
# SHORT high-signal query (long titles return nothing in NADA search): country + type + population
q_of <- function(title, country, pop){
  t <- tolower(paste(title, pop))
  typ <- if(str_detect(t,"census|recens|censo")) "census" else if(str_detect(t,"survey|enqu|encuesta|assessment|profiling|monitoring")) "survey" else ""
  popk <- if(str_detect(t,"refugee|réfugi|refugiad")) "refugee" else if(str_detect(t,"idp|displaced|déplac|desplaz")) "displaced" else if(str_detect(t,"stateless|apatri")) "stateless" else ""
  str_squish(paste(country, popk, typ)) }
search <- function(q){ rp<-GET(sprintf("%s/index.php/api/catalog/search?sk=%s&ps=8",BASE,URLencode(q)))
  j<-tryCatch(fromJSON(sb(rp)),error=function(e)NULL); r<-j$result$rows
  if(is.null(r)||!length(r)) return(tibble())
  d <- as_tibble(r); getc <- function(nm) if(nm %in% names(d)) as.character(d[[nm]]) else rep("", nrow(d))
  tibble(id=getc("id"), title=getc("title"), nation=getc("nation"), year=getc("year_start"), dclass=getc("data_class_id")) %>% head(8) }
verify <- function(r, cand){
  raw <- .ollama_generate(paste0(
    "A GAIN example names a SPECIFIC statistical activity. Pick the candidate study that IS that same specific survey/census ",
    "(same country and instrument), NOT a different one on the same topic.\n\n",
    "GAIN EXAMPLE: ", r$title, " | country: ", r$country, " | populations: ", r$populations, "\n\nCANDIDATES:\n",
    paste(sprintf("%d) %s | %s | %s", seq_len(nrow(cand)), cand$title, cand$nation, cand$year), collapse="\n"),
    "\n\nAnswer:\nBEST: number, or 0 if none is that specific instrument\nVERDICT: CONFIRMED or NONE\nREASON: one sentence."), timeout=100, json=FALSE)
  list(best=coalesce(as.integer(str_match(coalesce(raw,""),"BEST[:\\s]*\\s*(\\d+)")[,2]),0L),
       verdict=coalesce(toupper(str_match(coalesce(raw,""),"VERDICT[:\\s]*\\s*(CONFIRMED|NONE)")[,2]),"NONE")) }

MAN <- file.path(LAKE,"lake_ihsn_datasets.csv"); if (file.exists(MAN)) file.remove(MAN)
for (i in seq_len(nrow(mc))) { r <- mc[i,]
  cand <- tryCatch(search(q_of(r$title, r$country, r$populations)), error=function(e) tibble())
  # keep only same-country candidates (match on the country's first word)
  if (nrow(cand)) { cw <- tolower(str_split(r$country," ")[[1]][1])
    cand <- cand %>% filter(str_detect(tolower(nation), fixed(substr(cw,1,5)))) }
  study<-""; stitle<-""; access<-""; verdict<-if(nrow(cand))"NONE" else "NO RESULTS"
  if (nrow(cand)) { v<-verify(r, cand)
    if(v$best>=1 && v$best<=nrow(cand) && v$verdict=="CONFIRMED"){ verdict<-"CONFIRMED"
      study<-cand$id[v$best]; stitle<-cand$title[v$best]; access<-coalesce(DACC[cand$dclass[v$best]], cand$dclass[v$best]) } }
  readr::write_csv(tibble(example_id=r$example_id, country=r$country, gain_title=substr(r$title,1,46),
    verdict=verdict, study=study, ihsn_title=substr(stitle,1,46), access=coalesce(access,""),
    catalog=if(nzchar(study)) sprintf("%s/index.php/catalog/%s", BASE, study) else ""), MAN, append=file.exists(MAN))
  message(sprintf("  [%d/%d] %s %s -> %s %s [%s]", i, nrow(mc), r$example_id, substr(r$country,1,12), verdict, study, coalesce(access,"")))
  Sys.sleep(0.25)
}
m <- suppressMessages(read_csv(MAN, show_col_types=FALSE))
message("\n==== IHSN dataset hunt ====")
print(m %>% count(verdict))
message(sprintf("CONFIRMED datasets: %d | of which OPEN/PUF (downloadable): %d",
        sum(m$verdict=="CONFIRMED"), sum(m$verdict=="CONFIRMED" & grepl("OPEN|Public", m$access))))
print(m %>% filter(verdict=="CONFIRMED") %>% count(access))
