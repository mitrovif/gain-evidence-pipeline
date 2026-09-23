# ==============================================================================
# GAIN DATA LAKE - re-match microdata studies by SPECIFIC SURVEY NAME (strict).
#
# The topic+country match was wrong for all 6 microdata examples. Re-do it at the
# instrument level: search BOTH agency libraries (UNHCR + World Bank; JDC studies
# live on the WB site) by the example's actual survey name, and CONFIRM only if a
# candidate study IS that named survey (not a different survey on the same topic).
# Examples that are activities or not-yet-collected are excluded up front.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
source("shared/GAIN_CONFIG.R"); source("shared/GAIN_COMMON.R"); suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("LM Studio not reachable.")
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))

# targets = the microdata examples that name a REAL, collected survey (ex071 is an
# activity, ex357/HEAT is not yet collected -> excluded with a reason).
excluded <- tribble(~example_id, ~reason,
  "ex071", "not a dataset (respondent describes using geospatial data as a method)",
  "ex357", "survey not yet collected (HEAT questionnaire under revision)")
targets <- tibble(example_id=c("ex072","ex078","ex188","ex382"),
  query=c("Social Cohesion forced displacement returnee Afghanistan",
          "livelihood refugees host communities labour market Malaysia",
          "Forced Displacement Survey Cameroon socioeconomic refugees host",
          "socio-economic survey refugees migrants Libya")) %>%
  left_join(ro %>% select(example_id, g_title=title, country, org=organisation, populations), by="example_id")

GET <- function(u) tryCatch(request(u)|>req_timeout(35)|>req_headers(`User-Agent`="Mozilla/5.0 EGRISS",Accept="application/json,*/*")|>req_options(followlocation=TRUE,ssl_verifypeer=0)|>req_error(is_error=\(x)FALSE)|>req_perform(), error=function(e) NULL)
sb <- function(rp) tryCatch(resp_body_string(rp), error=function(e) "")
nada_search <- function(base, q){ rp<-GET(sprintf("%s/index.php/api/catalog/search?sk=%s&ps=10", base, URLencode(q)))
  j<-tryCatch(fromJSON(sb(rp)),error=function(e)NULL); rows<-j$result$rows
  if(is.null(rows)||!length(rows)) return(tibble())
  as_tibble(rows) %>% transmute(id=as.character(id), title=as.character(title%||%""), nation=as.character(nation%||%""), year=as.character(year_start%||%"")) %>% head(10) }

verify <- function(t, cand){
  raw <- .ollama_generate(paste0(
    "A GAIN example names a SPECIFIC statistical survey. Below are candidate studies from the agency microdata libraries. ",
    "Pick the candidate that IS that same specific survey (same instrument/name), NOT a different survey on the same topic or country.\n\n",
    "GAIN EXAMPLE survey: ", t$g_title, "\n  country: ", t$country, " | populations: ", t$populations,
    "\n\nCANDIDATES:\n", paste(sprintf("%d) [%s] %s | %s | %s", seq_len(nrow(cand)), cand$src, cand$title, cand$nation, cand$year), collapse="\n"),
    "\n\nAnswer:\nBEST: number of the candidate that is the SAME survey, or 0 if none is that specific survey\n",
    "VERDICT: CONFIRMED or NONE\nREASON: one short sentence."), timeout=110, json=FALSE)
  list(best=coalesce(as.integer(str_match(coalesce(raw,""),"BEST[:\\s]*\\s*(\\d+)")[,2]),0L),
       verdict=coalesce(toupper(str_match(coalesce(raw,""),"VERDICT[:\\s]*\\s*(CONFIRMED|NONE)")[,2]),"NONE"),
       reason=substr(str_squish(sub(".*REASON[:\\s]*","",coalesce(raw,""))),1,150)) }

out <- list()
for (i in seq_len(nrow(targets))) { t <- targets[i,]
  cand <- bind_rows(
    nada_search("https://microdata.unhcr.org", t$query) %>% mutate(base="https://microdata.unhcr.org", src="UNHCR"),
    nada_search("https://microdata.worldbank.org", t$query) %>% mutate(base="https://microdata.worldbank.org", src="WorldBank"))
  study<-""; stitle<-""; base<-""; verdict<-if(nrow(cand))"NONE" else "NO RESULTS"; reason<-""
  if (nrow(cand)){ v<-verify(t, cand)
    verdict<-v$verdict; reason<-v$reason
    if(v$best>=1 && v$best<=nrow(cand) && v$verdict=="CONFIRMED"){ study<-cand$id[v$best]; stitle<-cand$title[v$best]; base<-cand$base[v$best] } }
  out[[i]] <- tibble(example_id=t$example_id, gain_survey=substr(t$g_title,1,50), verdict, study, base,
                     matched_study=substr(stitle,1,50), catalog=if(nzchar(study)) sprintf("%s/index.php/catalog/%s/get-microdata",base,study) else "", reason)
  message(sprintf("  %s -> %s %s | %s", t$example_id, verdict, study, substr(stitle,1,44)))
}
res <- bind_rows(out)
readr::write_excel_csv(res, file.path(LAKE,"lake_rematch_microdata.csv"))
message("\n==== re-match by survey name ====")
print(res %>% count(verdict))
message(sprintf("CONFIRMED microdata studies to apply for: %d | excluded up front (activity/not-collected): %d", sum(res$verdict=="CONFIRMED"), nrow(excluded)))
print(res %>% filter(verdict=="CONFIRMED") %>% transmute(example_id, matched_study, catalog) %>% as.data.frame(), right=FALSE)
