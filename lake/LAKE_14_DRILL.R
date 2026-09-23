# ==============================================================================
# GAIN DATA LAKE - step 11: DRILL the PARTIAL portals to the exact dataset.
#
# Web-search located the right office but often at a portal/landing page (PARTIAL).
# This step fetches that page, finds the best SAME-SITE link matching the example's
# topic (refugees/IDPs/stateless + title terms), LLM-verifies it is the specific
# dataset/output, and downloads it if it is a direct data file. Turns "office
# located" into "specific dataset located / in hand".
# Output: lake_drill.csv ; data files -> store/<id>/drill_*.<ext>.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2) })
source("shared/GAIN_CONFIG.R"); source("shared/GAIN_COMMON.R"); suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("LM Studio not reachable.")
LAKE <- "data_lake"; STORE <- file.path(LAKE, "store")
ws <- suppressMessages(read_csv(file.path(LAKE, "lake_websearch_found.csv"), show_col_types = FALSE)) %>%
  filter(verdict == "PARTIAL", !is.na(found_url), nzchar(found_url))
roster <- suppressMessages(read_csv(file.path(LAKE, "lake_roster.csv"), show_col_types = FALSE))
MAN <- file.path(LAKE, "lake_drill.csv")
done <- if (file.exists(MAN)) suppressMessages(read_csv(MAN, show_col_types = FALSE))$example_id else character(0)
ws <- ws %>% filter(!example_id %in% done)
message(sprintf("drilling %d PARTIAL portals ...", nrow(ws)))

GET <- function(u) tryCatch(request(u) |> req_timeout(30) |>
  req_headers(`User-Agent`="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/124 Safari/537.36",
              Accept="text/html,*/*", `Accept-Language`="en") |>
  req_options(followlocation=TRUE, ssl_verifypeer=0) |> req_error(is_error=\(x) FALSE) |> req_perform(), error=function(e) NULL)
reg_dom <- function(h) { p <- str_split(tolower(coalesce(h,"")), "\\.")[[1]]; n <- length(p); if (n <= 2) paste(p, collapse=".") else paste(tail(p, 2), collapse=".") }
host_of <- function(u) tolower(coalesce(str_match(coalesce(u,""), "^https?://([^/]+)")[,2], ""))
resolve <- function(href, base) { href <- trimws(href)
  if (!nzchar(href) || grepl("^(mailto:|javascript:|#|tel:)", href)) return(NA_character_)
  if (grepl("^https?://", href)) return(href)
  m <- str_match(base, "^(https?://[^/]+)(/.*)?$"); root <- m[2]; path <- coalesce(m[3], "/")
  if (grepl("^//", href)) return(paste0(sub("^(https?):.*","\\1",base), ":", href))
  if (grepl("^/", href)) return(paste0(root, href))
  paste0(root, sub("[^/]*$", "", path), href) }
DATEXT <- "[.](csv|xlsx?|json|zip|px|sdmx|pdf)([?#]|$)"
DATAWORD <- "dataset|table|statistic|/data|census|survey|microdata|download|indicator|publication|report|bulletin|profile"

toks <- function(s) { t <- str_split(str_squish(gsub("[^a-z0-9 ]"," ", tolower(coalesce(s,"")))), " ")[[1]]; t[nchar(t) > 3] }
verify <- function(r, cand) {
  raw <- .ollama_generate(paste0(
    "A GAIN example, and candidate links from the office's OWN website. Pick the link that is the SPECIFIC dataset/output ",
    "the example describes (the survey/census/table/report itself, or its data file), not a generic section or homepage.\n\n",
    "EXAMPLE: org=", r$organisation, " | title=", r$title, " | populations=", r$populations, "\n\nLINKS:\n",
    paste(sprintf("%d) %s  <%s>", seq_len(nrow(cand)), substr(cand$anchor,1,70), cand$url), collapse="\n"),
    "\n\nAnswer:\nBEST: number of the specific-dataset link, or 0 if none is specific enough\n",
    "VERDICT: MATCH (specific dataset/output) or MISMATCH\nREASON: one short sentence."), timeout=100, json=FALSE)
  list(best = coalesce(as.integer(str_match(coalesce(raw,""),"BEST[:\\s]*\\s*(\\d+)")[,2]), 0L),
       verdict = coalesce(toupper(str_match(coalesce(raw,""),"VERDICT[:\\s]*\\s*(MATCH|MISMATCH)")[,2]), "MISMATCH"),
       reason = substr(str_squish(sub(".*REASON[:\\s]*","",coalesce(raw,""))),1,140)) }

for (i in seq_len(nrow(ws))) {
  r <- ws[i,]; ex <- r$example_id; rr <- roster %>% filter(example_id == ex) %>% slice(1)
  portal <- r$found_url; rp <- GET(portal)
  drilled <- ""; anchor <- ""; verdict <- "NO PAGE"; reason <- ""; saved <- ""
  if (!is.null(rp) && resp_status(rp) < 400) {
    h <- tryCatch(resp_body_string(rp), error=function(e) "")
    a <- str_match_all(h, '<a[^>]*href="([^"]+)"[^>]*>(.*?)</a>')[[1]]
    if (length(a)) {
      cand <- tibble(url = map_chr(a[,2], resolve, base = portal), anchor = str_squish(gsub("<[^>]+>","", a[,3]))) %>%
        filter(!is.na(url)) %>% mutate(host = host_of(url)) %>%
        filter(reg_dom(host) == reg_dom(host_of(portal))) %>%          # stay on the office's own site
        distinct(url, .keep_all = TRUE)
      kw <- unique(c(toks(rr$title), unlist(str_split(rr$populations, ";"))))
      score <- function(u, an) { s <- tolower(paste(u, an))
        sum(vapply(kw, function(k) str_detect(s, fixed(k)), logical(1))) +
        2*str_detect(s, DATEXT) + str_detect(s, DATAWORD) }
      cand <- cand %>% mutate(sc = map2_dbl(url, anchor, score)) %>% filter(sc > 0 | str_detect(tolower(url), DATEXT)) %>%
        arrange(desc(sc)) %>% head(8)
      if (nrow(cand)) { v <- verify(rr, cand)
        verdict <- v$verdict; reason <- v$reason
        if (v$best >= 1 && v$best <= nrow(cand) && v$verdict == "MATCH") {
          drilled <- cand$url[v$best]; anchor <- cand$anchor[v$best]
          if (str_detect(tolower(drilled), DATEXT)) {                  # direct data file -> grab it
            d <- file.path(STORE, ex); dir.create(d, showWarnings=FALSE)
            fp <- GET(drilled)
            if (!is.null(fp) && resp_status(fp) < 400) { ext <- str_match(tolower(drilled), DATEXT)[,2]
              body <- resp_body_raw(fp); if (length(body) > 200 && length(body)/1048576 < 60) {
                saved <- file.path(d, sprintf("drill_%02d.%s", i, ext)); writeBin(body, saved) } } } } }
    }
  }
  readr::write_csv(tibble(example_id=ex, country=r$country, organisation=r$organisation, portal_url=portal,
    drilled_url=drilled, anchor=substr(anchor,1,80), verdict=verdict, reason=reason,
    saved=if(nzchar(saved)) basename(saved) else ""), MAN, append=file.exists(MAN))
  message(sprintf("  [%d/%d] %s %s -> %s %s", i, nrow(ws), ex, substr(r$country,1,14), verdict, substr(drilled,1,48)))
  Sys.sleep(0.4)
}

m <- suppressMessages(read_csv(MAN, show_col_types=FALSE))
message(sprintf("\n==== drill-down ====\nportals drilled to a specific dataset (MATCH): %d of %d | data files pulled: %d",
        sum(m$verdict=="MATCH", na.rm=TRUE), nrow(m), sum(nzchar(coalesce(m$saved,"")))))
print(m %>% count(verdict))
