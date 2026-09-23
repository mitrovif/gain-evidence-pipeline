# ==============================================================================
# GAIN DATA LAKE - step 8: connect via NSO STATISTICAL APIs (user's steer).
#
# Many country-led examples with no usable link are actually available through the
# office's statistical API. We search the API catalogue by the example's topic,
# then LLM-VERIFY each candidate (same gate as LAKE_09) so only confirmed datasets
# are marked API-connectable. Covered now: Eurostat SDMX (all EU countries) and
# SSB/PxWeb (Norway); registry is easily extended (SCB, StatFin, CSO PxStat...).
# Output: lake_api_connectable.csv (example -> dataset code, verified).
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2) })
source("shared/GAIN_CONFIG.R"); source("shared/GAIN_COMMON.R"); suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("LM Studio not reachable.")
LAKE <- "data_lake"
ts <- suppressMessages(read_csv(file.path(LAKE, "lake_to_source.csv"), show_col_types = FALSE)) %>% filter(lead_type == "country-led")
GET <- function(u) tryCatch(request(u) |> req_timeout(40) |> req_headers(`User-Agent`="Mozilla/5.0 EGRISS") |>
    req_options(followlocation=TRUE, ssl_verifypeer=0) |> req_error(is_error=\(x) FALSE) |> req_perform(), error=function(e) NULL)
sbody <- function(rp) tryCatch(resp_body_string(rp), error=function(e) "")

# ---- registry: which API serves each country ---------------------------------
EU <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Czech Republic","Denmark","Estonia","Finland",
        "France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
        "Netherlands (Kingdom of the)","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden")
api_for <- function(country) {
  c0 <- harmonize_country(country)
  if (c0 == "Norway") "SSB" else if (c0 %in% harmonize_country(EU)) "Eurostat" else NA_character_
}

# topic keywords from the example's populations + title
topic_kw <- function(pops, title) {
  k <- c(); p <- tolower(coalesce(pops, "")); t <- tolower(coalesce(title, ""))
  if (grepl("refugee", p) || grepl("refugee|asylum", t)) k <- c(k, "asylum", "refugee")
  if (grepl("idp", p) || grepl("internally displaced", t)) k <- c(k, "displaced")
  if (grepl("stateless", p) || grepl("stateless|nationalit", t)) k <- c(k, "stateless", "citizenship")
  if (!length(k)) k <- c("migration", "foreign")
  unique(k)
}

# ---- Eurostat: cached table-of-contents, keyword search ----------------------
TOC <- file.path(LAKE, "eurostat_toc.rds")
toc <- if (file.exists(TOC)) readRDS(TOC) else {
  rp <- GET("https://ec.europa.eu/eurostat/api/dissemination/catalogue/toc/txt")
  lines <- str_split(sbody(rp), "\n")[[1]]
  d <- str_match(lines, '^\\s*"([^"]+)"\\s+"([a-z0-9_]+)"\\s+"dataset"')
  t <- tibble(label = d[,2], code = d[,3]) %>% filter(!is.na(code)); saveRDS(t, TOC); t }
eurostat_search <- function(kw) {
  pat <- paste0("\\b(", paste(kw, collapse="|"), ")")
  toc %>% filter(str_detect(tolower(label), pat)) %>% distinct(code, .keep_all=TRUE) %>% head(6) }

ssb_search <- function(kw) {
  rp <- GET(sprintf("https://data.ssb.no/api/v0/en/table/?query=%s", URLencode(paste(kw, collapse=" "))))
  j <- tryCatch(jsonlite::fromJSON(sbody(rp)), error=function(e) NULL)
  if (is.null(j) || !length(j)) return(tibble(code=character(), label=character()))
  as_tibble(j) %>% transmute(code = id, label = title) %>% head(6) }

verify <- function(r, label, code, system) {
  raw <- .ollama_generate(paste0(
    "A GAIN statistical example and a CANDIDATE dataset from the national statistical API. Can the candidate dataset ",
    "provide the statistics this example is about (same population group and topic)?\n\n",
    "EXAMPLE: org=", r$organisation, " | title=", r$title, " | populations=", r$populations,
    "\nCANDIDATE (", system, "): ", label, " [", code, "]\n\n",
    "Answer two lines:\nVERDICT: MATCH or PARTIAL or MISMATCH\nREASON: one short sentence."), timeout=90, json=FALSE)
  v <- toupper(str_match(coalesce(raw,""), "VERDICT[:\\s]*\\s*(MATCH|PARTIAL|MISMATCH)")[,2])
  list(verdict = coalesce(v, "MISMATCH"), reason = substr(str_squish(sub(".*REASON[:\\s]*","",coalesce(raw,""))),1,140))
}

# ---- run: search + verify per country-led example ----------------------------
ts <- ts %>% mutate(system = map_chr(country, api_for))
elig <- ts %>% filter(!is.na(system))
message(sprintf("country-led examples with a known NSO API: %d (Eurostat %d, SSB %d)",
        nrow(elig), sum(elig$system=="Eurostat"), sum(elig$system=="SSB")))
out <- list()
for (i in seq_len(nrow(elig))) {
  r <- elig[i,]; kw <- topic_kw(r$populations, r$title)
  cands <- if (r$system == "Eurostat") eurostat_search(kw) else ssb_search(kw)
  if (!nrow(cands)) next
  best <- NULL
  for (j in seq_len(nrow(cands))) { v <- verify(r, cands$label[j], cands$code[j], r$system)
    if (v$verdict %in% c("MATCH","PARTIAL")) { best <- tibble(example_id=r$example_id, country=r$country, system=r$system,
      dataset_code=cands$code[j], dataset_label=cands$label[j], verdict=v$verdict, reason=v$reason,
      api_url = if (r$system=="Eurostat") sprintf("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/%s?format=JSON", cands$code[j])
                else sprintf("https://data.ssb.no/api/v0/en/table/%s", cands$code[j])); break } }
  if (!is.null(best)) { out[[length(out)+1]] <- best; message(sprintf("  [%d/%d] %s %s -> %s [%s] %s", i, nrow(elig), r$example_id, substr(r$country,1,14), best$dataset_code, best$verdict, substr(best$dataset_label,1,40))) }
  Sys.sleep(0.1)
}
conn <- bind_rows(out)
readr::write_excel_csv(conn, file.path(LAKE, "lake_api_connectable.csv"))

message(sprintf("\n==== API-connectable ====\nverified connectable datasets: %d examples (of %d eligible, %d country-led to-source)",
        n_distinct(conn$example_id), nrow(elig), nrow(ts)))
if (nrow(conn)) print(conn %>% count(system, verdict))
message(sprintf("still need web-search / reach-out: %d country-led", nrow(ts) - n_distinct(conn$example_id)))
