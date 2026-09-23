# ==============================================================================
# GAIN DATA LAKE - step 10: WEB-SEARCH + verify the remaining country-led examples.
#
# Most offices in the backlog have no open API, so we search the web for each
# example's dataset (DuckDuckGo HTML, no key), read the result titles/snippets, and
# LLM-VERIFY the best candidate against the roster example (same gate as LAKE_09/10).
# Only MATCH/PARTIAL are recorded as found. Microdata-capable examples go first.
# Resumable via manifest. Output: lake_websearch_found.csv.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2) })
source("shared/GAIN_CONFIG.R"); source("shared/GAIN_COMMON.R"); suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("LM Studio not reachable.")
LAKE <- "data_lake"
conn <- suppressMessages(read_csv(file.path(LAKE, "lake_api_connectable.csv"), show_col_types = FALSE))
ts <- suppressMessages(read_csv(file.path(LAKE, "lake_to_source.csv"), show_col_types = FALSE)) %>%
  filter(lead_type == "country-led", !example_id %in% conn$example_id) %>%
  arrange(desc(is_microdata_capable), country)
MAN <- file.path(LAKE, "lake_websearch_found.csv")
done <- if (file.exists(MAN)) suppressMessages(read_csv(MAN, show_col_types = FALSE))$example_id else character(0)
ts <- ts %>% filter(!example_id %in% done)
CAP <- suppressWarnings(as.integer(Sys.getenv("LAKE_WS_CAP", "999")))
ts <- head(ts, CAP)
message(sprintf("web-searching %d country-led examples (microdata-capable first) ...", nrow(ts)))

# national statistical office domain per country (to tell the office's OWN output
# from generic global aggregates) + a blocklist of generic aggregator/news hosts.
nso_dom <- suppressMessages(read_csv("NSO_Full_Registry.csv", show_col_types = FALSE)) %>%
  transmute(country = harmonize_country(country), dom = tolower(domain)) %>% filter(nzchar(dom)) %>% distinct(country, .keep_all = TRUE)
dom_of <- function(u) sub("^www\\d*\\.", "", tolower(coalesce(str_match(coalesce(u,""), "^https?://([^/]+)")[,2], "")))
AGG <- paste0("humdata\\.org|reliefweb|data\\.unhcr\\.org|unhcr\\.org|worldbank\\.org/(data|indicator|en)|",
  "ourworldindata|healthdata\\.org|ghdx|wikipedia|statista|knoema|macrotrends|tradingeconomics|populationstatistics\\.github|",
  "reuters|aljazeera|ansa\\.it|bbc\\.|apnews|egrisstats\\.org|jointdatacenter\\.org|\\biom\\.int|ecoi\\.net|refworld")

GET <- function(u) tryCatch(request(u) |> req_timeout(25) |>
  req_headers(`User-Agent`="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/124 Safari/537.36",
              Accept="text/html,*/*", `Accept-Language`="en") |>
  req_options(followlocation=TRUE, ssl_verifypeer=0) |> req_error(is_error=\(x) FALSE) |> req_perform(), error=function(e) NULL)
sbody <- function(rp) tryCatch(resp_body_string(rp), error=function(e) "")

ddg <- function(query) {
  h <- sbody(GET(paste0("https://html.duckduckgo.com/html/?q=", URLencode(query))))
  a <- str_match_all(h, 'result__a[^>]*href="([^"]+)"[^>]*>(.*?)</a>')[[1]]
  if (!length(a)) return(tibble(url=character(), title=character(), snippet=character()))
  urls <- vapply(a[,2], function(u) { m <- str_match(u, "uddg=([^&]+)"); if (is.na(m[2])) u else utils::URLdecode(m[2]) }, character(1))
  titles <- str_squish(gsub("<[^>]+>", "", a[,3]))
  sn <- str_squish(gsub("<[^>]+>", "", str_match_all(h, 'result__snippet[^>]*>(.*?)</a>')[[1]][,2]))
  n <- min(length(urls), 5); sn <- c(sn, rep("", n))[seq_len(n)]
  tibble(url = urls[seq_len(n)], title = titles[seq_len(n)], snippet = sn) %>%
    filter(!str_detect(tolower(url), "facebook|twitter|linkedin|youtube|instagram|pinterest|reddit"))
}
topic <- function(pops, title) {
  p <- tolower(coalesce(pops,"")); k <- c()
  if (str_detect(p,"refugee")) k <- c(k,"refugees"); if (str_detect(p,"idp")) k <- c(k,"internally displaced")
  if (str_detect(p,"stateless")) k <- c(k,"stateless"); if (!length(k)) k <- "forcibly displaced"
  paste(k, collapse=" ")
}
verify <- function(r, cand) {
  tag <- ifelse(cand$is_agg, " [GENERIC AGGREGATOR/NEWS - not the office's own output]",
         ifelse(cand$on_nso, " [on the office's own website]", ""))
  raw <- .ollama_generate(paste0(
    "You match a GAIN statistical example to its OWN dataset among web results. STRICT rule: a result counts only if it ",
    "is the SPECIFIC statistical output the example describes, PUBLISHED BY (or jointly by) the example's own ",
    "organisation - its official website, or its named study in a data repository. A generic global/regional aggregate ",
    "(UNHCR global refugee statistics, a World Bank indicator, a humanitarian data portal, Wikipedia), a news article, or ",
    "a different survey is a MISMATCH, even if the topic is the same.\n\n",
    "EXAMPLE:\n  organisation: ", r$organisation, "\n  country: ", r$country, "\n  title: ", r$title,
    "\n  populations: ", r$populations, "\n\nRESULTS:\n",
    paste(sprintf("%d) %s | %s%s | %s", seq_len(nrow(cand)), cand$title, cand$url, tag, substr(cand$snippet,1,110)), collapse="\n"),
    "\n\nAnswer three lines:\nBEST: number of the result that is the office's OWN output, or 0 if none qualifies\n",
    "VERDICT: MATCH (office's own specific output) or PARTIAL (clearly the office, but a portal/landing page) or MISMATCH\n",
    "REASON: one short sentence naming the publisher."),
    timeout=110, json=FALSE)
  bn <- as.integer(str_match(coalesce(raw,""), "BEST[:\\s]*\\s*(\\d+)")[,2])
  v  <- toupper(str_match(coalesce(raw,""), "VERDICT[:\\s]*\\s*(MATCH|PARTIAL|MISMATCH)")[,2])
  list(best=coalesce(bn, 0L), verdict=coalesce(v,"MISMATCH"), reason=substr(str_squish(sub(".*REASON[:\\s]*","",coalesce(raw,""))),1,140))
}

for (i in seq_len(nrow(ts))) {
  r <- ts[i,]
  q <- paste(r$organisation, topic(r$populations, r$title), "statistics data survey", r$country)
  cand <- ddg(q); Sys.sleep(1.4)
  found_url <- ""; title <- ""; verdict <- "NO RESULTS"; reason <- ""
  if (nrow(cand)) {
    nd <- nso_dom$dom[match(harmonize_country(r$country), nso_dom$country)]
    cand <- cand %>% mutate(host = map_chr(url, dom_of), is_agg = str_detect(host, AGG),
                            on_nso = !is.na(nd) & nzchar(nd) & str_detect(host, fixed(nd)))
    v <- verify(r, cand); verdict <- v$verdict; reason <- v$reason
    b <- v$best
    if (b >= 1 && b <= nrow(cand) && v$verdict %in% c("MATCH","PARTIAL")) {
      if (isTRUE(cand$is_agg[b])) { verdict <- "MISMATCH"; reason <- paste("generic aggregator rejected;", reason) }
      else { found_url <- cand$url[b]; title <- cand$title[b] } } }
  readr::write_csv(tibble(example_id=r$example_id, country=r$country, organisation=r$organisation,
    title=substr(r$title,1,60), is_microdata_capable=r$is_microdata_capable,
    found_url=found_url, found_title=substr(title,1,80), verdict=verdict, reason=reason), MAN, append=file.exists(MAN))
  message(sprintf("  [%d/%d] %s %s -> %s %s", i, nrow(ts), r$example_id, substr(r$country,1,14), verdict, substr(found_url,1,46)))
}

m <- suppressMessages(read_csv(MAN, show_col_types=FALSE)) %>% mutate(has_url = !is.na(found_url) & nzchar(found_url))
message(sprintf("\n==== web-search ====\nresolved (office's own output found): %d of %d searched", sum(m$has_url), nrow(m)))
print(m %>% count(verdict))
message(sprintf("microdata-capable resolved: %d", sum(m$has_url & m$is_microdata_capable, na.rm=TRUE)))
