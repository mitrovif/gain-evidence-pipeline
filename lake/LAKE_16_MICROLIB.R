# ==============================================================================
# GAIN DATA LAKE - step 13: self-serve the institution-led examples from the
# agencies' own curated MICRODATA LIBRARIES (NADA), instead of reaching out.
#
# UNHCR (microdata.unhcr.org) and the World Bank (microdata.worldbank.org) run
# public NADA catalogues. For each institution-led reach-out example on these
# platforms, we SEARCH the catalogue by topic+country, LLM-verify the best study
# is the example's dataset, and pull its DDI variable inventory (the microdata
# files are open or request-per-study, but the catalogue entry + variables are
# open). Moves matched examples out of the reach-out list into "self-serve".
# Extendable to IHSN (catalog.ihsn.org), ILO, IOM DTM, UNDP HDR.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite); library(xml2) })
source("shared/GAIN_CONFIG.R"); source("shared/GAIN_COMMON.R"); suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("LM Studio not reachable.")
LAKE <- "data_lake"; STORE <- file.path(LAKE, "store")
ex0 <- suppressMessages(read_csv(file.path(LAKE, "lake_reachout_examples.csv"), show_col_types = FALSE)) %>% filter(lead_type == "institution-led")
# MANY institution-led "examples" are ACTIVITIES (workshops, training, guidance,
# methodological/coordination work), not datasets - they have no data to find.
ACTIVITY <- "workshop|training|webinar|e-?learn|academy|guidance|toolkit|expert group|capacity|methodolog|coordinat|strateg|\\bcourse\\b|quality assurance|global trends|alignment|promoting|supporting|transfer learning|roadmap|framework|note on|estimates|modelling|webinar|pledge|advocacy|e-learning"
o <- tolower(coalesce(ex0$organisation, ""))
ex <- ex0 %>% mutate(base = case_when(
  str_detect(o, "high commissioner for refugees|unhcr") ~ "https://microdata.unhcr.org",
  str_detect(o, "world bank|joint data|jdc")            ~ "https://microdata.worldbank.org",
  TRUE ~ NA_character_),
  is_activity = str_detect(tolower(title), ACTIVITY)) %>%
  # exclude ACTIVITIES (no data) and require a country (a microdata study is country-specific)
  filter(!is.na(base), !is_activity, nzchar(coalesce(country, "")))
message(sprintf("NADA self-serve candidates (dataset-type, with country): %d of %d institution-led | activities excluded: %d",
        nrow(ex), nrow(ex0), sum(str_detect(tolower(ex0$title), ACTIVITY))))

MAN <- file.path(LAKE, "lake_microlib_found.csv"); if (file.exists(MAN)) file.remove(MAN)
GET <- function(u) tryCatch(request(u) |> req_timeout(40) |> req_headers(`User-Agent`="Mozilla/5.0 EGRISS", Accept="application/json,*/*") |>
    req_options(followlocation=TRUE, ssl_verifypeer=0) |> req_error(is_error=\(x) FALSE) |> req_perform(), error=function(e) NULL)
sbody <- function(rp) tryCatch(resp_body_string(rp), error=function(e) "")
parse_ddi <- function(path) { x <- tryCatch(read_xml(path), error=function(e) NULL); if (is.null(x)) return(tibble())
  xml_ns_strip(x); v <- xml_find_all(x, "//var"); if (!length(v)) return(tibble())
  tibble(name=xml_attr(v,"name"), label=xml_text(xml_find_first(v,".//labl")), question=xml_text(xml_find_first(v,".//qstnLit"))) }

nada_search <- function(base, kw) {
  rp <- GET(sprintf("%s/index.php/api/catalog/search?sk=%s&ps=8", base, URLencode(kw)))
  j <- tryCatch(fromJSON(sbody(rp)), error=function(e) NULL); rows <- j$result$rows
  if (is.null(rows) || !length(rows)) return(tibble())
  as_tibble(rows) %>% transmute(id = as.character(id), idno = as.character(idno %||% ""),
    title = as.character(title %||% ""), nation = as.character(nation %||% ""),
    year = as.character(year_start %||% "")) %>% head(8) }
kw_of <- function(r) { p <- tolower(coalesce(r$populations,"")); k <- c(r$country)
  if (str_detect(p,"refugee")) k <- c(k,"refugee"); if (str_detect(p,"idp")) k <- c(k,"displaced")
  if (str_detect(p,"stateless")) k <- c(k,"stateless"); str_squish(paste(k, collapse=" ")) }
verify <- function(r, cand) {
  raw <- .ollama_generate(paste0(
    "A GAIN example, and candidate microdata studies from ", r$organisation, "'s data library. Which candidate is the ",
    "SAME study/dataset the example describes (same country, population, and survey/census)?\n\n",
    "EXAMPLE: title=", r$title, " | country=", r$country, " | populations=", r$populations, "\n\nCANDIDATES:\n",
    paste(sprintf("%d) %s | %s | %s", seq_len(nrow(cand)), cand$title, cand$nation, cand$year), collapse="\n"),
    "\n\nAnswer:\nBEST: number, or 0 if none matches\nVERDICT: MATCH or MISMATCH\nREASON: one sentence."), timeout=100, json=FALSE)
  list(best=coalesce(as.integer(str_match(coalesce(raw,""),"BEST[:\\s]*\\s*(\\d+)")[,2]),0L),
       verdict=coalesce(toupper(str_match(coalesce(raw,""),"VERDICT[:\\s]*\\s*(MATCH|MISMATCH)")[,2]),"MISMATCH")) }

ccountry <- function(x) str_squish(tolower(gsub("[[:punct:]]", " ", coalesce(x, ""))))
for (i in seq_len(nrow(ex))) {
  r <- ex[i,]; cand <- nada_search(r$base, kw_of(r))
  # HARD country filter: a country-specific study can only match the same country
  if (nrow(cand)) { ec <- ccountry(r$country)
    cand <- cand %>% filter(str_detect(ccountry(nation), fixed(ec)) | str_detect(ec, fixed(ccountry(nation))) | !nzchar(str_squish(nation))) }
  study <- ""; title <- ""; nv <- 0L; verdict <- if (nrow(cand)) "MISMATCH" else "NO RESULTS"
  if (nrow(cand)) { v <- verify(r, cand)
    if (v$best >= 1 && v$best <= nrow(cand) && v$verdict == "MATCH") { verdict <- "MATCH"
      study <- cand$id[v$best]; title <- cand$title[v$best]
      d <- file.path(STORE, r$example_id); dir.create(d, showWarnings=FALSE)
      rp <- GET(sprintf("%s/index.php/metadata/export/%s/ddi", r$base, study))
      if (!is.null(rp) && resp_status(rp) < 400 && length(resp_body_raw(rp)) > 500) {
        p <- file.path(d, sprintf("microlib_%s_ddi.xml", study)); writeBin(resp_body_raw(rp), p)
        vv <- parse_ddi(p); if (nrow(vv)) { readr::write_excel_csv(vv, file.path(d, sprintf("microlib_%s_variables.csv", study))); nv <- nrow(vv) } } } }
  readr::write_csv(tibble(example_id=r$example_id, organisation=r$organisation, base=r$base,
    verdict=verdict, study=study, study_title=substr(title,1,70), n_variables=nv,
    catalog_url=if(nzchar(study)) sprintf("%s/index.php/catalog/%s", r$base, study) else ""), MAN, append=file.exists(MAN))
  message(sprintf("  [%d/%d] %s %s -> %s %s (%d vars)", i, nrow(ex), r$example_id, substr(r$organisation,1,10), verdict, study, nv))
  Sys.sleep(0.2)
}
m <- suppressMessages(read_csv(MAN, show_col_types=FALSE))
message(sprintf("\n==== microdata-library self-serve ====\nMATCH (found in the agency library): %d of %d | with variable inventory: %d",
        sum(m$verdict=="MATCH"), nrow(m), sum(m$n_variables>0, na.rm=TRUE)))
print(m %>% count(verdict))
