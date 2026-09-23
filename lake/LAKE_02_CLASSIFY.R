# ==============================================================================
# GAIN DATA LAKE - step 1b: classify each example's source + build the download
# plan and the DATA-ACCESS reach-out list.
#
# For the 119 examples that carry a link: what kind of source is it, and can we
# pull it programmatically (AUTO), must we scrape/parse it (SCRAPE), or is it
# behind access control (GATED)? A light HEAD check flags dead links.
# For the ~294 with NO followable link: the reporting office is known
# (organisation), so they go straight to the data-access reach-out list.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2) })
LAKE <- "data_lake"
roster <- suppressMessages(read_csv(file.path(LAKE, "lake_roster.csv"), show_col_types = FALSE))

# ---- source-type + method from the URL ---------------------------------------
classify_url <- function(u) {
  x <- tolower(coalesce(u, "")); if (!nzchar(x)) return(c(type = "none", method = "reach_out", tier = "NO_LINK"))
  pick <- function(type, method, tier) c(type = type, method = method, tier = tier)
  if (str_detect(x, "microdata\\.unhcr\\.org"))         return(pick("unhcr_microdata", "ddi_api",   "AUTO_META"))  # metadata open, data may be gated
  if (str_detect(x, "microdata\\.worldbank\\.org"))     return(pick("wb_microdata",    "ddi_api",   "AUTO_META"))
  if (str_detect(x, "catalog\\.ihsn\\.org|/nada|/index.php/catalog")) return(pick("ihsn_nada", "ddi_api", "AUTO_META"))
  if (str_detect(x, "ec\\.europa\\.eu|eurostat|/sdmx|sdmx|/estat"))   return(pick("eurostat_sdmx", "sdmx_api",  "AUTO"))
  if (str_detect(x, "pxweb|/px/|statbank|/webapi/|statfin|px-x-|/pxweb/")) return(pick("pxweb", "pxweb_api", "AUTO"))
  if (str_detect(x, "json-?stat|/api/v\\d|/api/dataset|data\\.un\\.org|/rest/data")) return(pick("api_other", "http_api", "AUTO"))
  if (str_detect(x, "\\.(csv|xlsx?|zip|json|px|sdmx|xml)(\\?|$)"))    return(pick("data_file",     "http_file", "AUTO"))
  if (str_detect(x, "\\.pdf(\\?|$)"))                                 return(pick("pdf",           "pdf_extract","SCRAPE"))
  pick("web_page", "scrape_html", "SCRAPE")
}

linked <- roster %>% filter(has_link) %>% mutate(url = coalesce(link_results, str_extract(all_urls, "\\S+")))
cl <- t(vapply(linked$url, classify_url, character(3)))
linked <- bind_cols(linked, as_tibble(cl))

# ---- reachability via GET with browser-like headers (cached, resumable) ------
# HEAD is unreliable (many NSO servers reject it, or bot-block with 403). A ranged
# GET with real headers tells blocked (403/429 = exists, needs a browser) apart
# from gone (404/410) apart from a dead host (DNS/timeout).
CACHE <- file.path(LAKE, "lake_reach_cache.rds")
cache <- if (file.exists(CACHE)) readRDS(CACHE) else list()
check <- function(u) {
  if (!is.null(cache[[u]])) return(cache[[u]])
  res <- tryCatch({
    rp <- request(u) |> req_timeout(15) |>
      req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124 Safari/537.36",
                  Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                  `Accept-Language` = "en,fr;q=0.8", Range = "bytes=0-4095") |>
      req_options(followlocation = TRUE, ssl_verifypeer = 0) |> req_error(is_error = \(x) FALSE) |> req_perform()
    list(status = resp_status(rp), ctype = resp_header(rp, "content-type") %||% "", err = "")
  }, error = function(e) { m <- conditionMessage(e)
    list(status = NA_integer_, ctype = "", err = if (grepl("resolve|host|DNS", m, ignore.case=TRUE)) "dns" else "timeout") })
  cache[[u]] <<- res; res
}
message(sprintf("GET-checking %d links (browser headers) ...", nrow(linked)))
hc <- map(linked$url, check); saveRDS(cache, CACHE)
linked <- linked %>% mutate(http_status = map_int(hc, "status"), err = map_chr(hc, "err"),
  content_type = map_chr(hc, ~ substr(.x$ctype, 1, 40)),
  reachability = case_when(
    !is.na(http_status) & http_status < 400 ~ "live",
    http_status %in% c(401,403,429) ~ "blocked",     # exists, bot-refused -> needs a real browser
    http_status %in% c(404,410) ~ "gone",
    err == "dns" ~ "dead_host",
    TRUE ~ "unreachable"))

# ---- download plan: action = how we will actually get the data ---------------
plan <- linked %>%
  mutate(action = case_when(
    reachability == "live" & tier %in% c("AUTO","AUTO_META") ~ tier,   # pull programmatically
    reachability == "live"                                   ~ "SCRAPE",         # fetch + parse the page/pdf
    reachability == "blocked"                                ~ "SCRAPE_BROWSER",  # exists; needs browser/stronger client
    reachability %in% c("gone","dead_host","unreachable")    ~ "RECOVER")) %>%    # try Wayback / cross-match next
  transmute(example_id, year, country, organisation, lead_type, populations, recommendations,
            is_microdata_capable, host_signal, url, source_type = type, method,
            reachability, http_status, action, title = substr(title, 1, 80))
readr::write_excel_csv(plan, file.path(LAKE, "lake_download_plan.csv"))

# examples with NO link at all -> straight to the recovery step (LAKE_03 will try
# to find them by cross-match / search before concluding a data request is needed).
nolink <- roster %>% filter(!has_link) %>%
  transmute(example_id, year, country, organisation, lead_type, populations, recommendations,
            is_microdata_capable, host_signal, url = NA_character_, source_type = "none", method = "recover",
            reachability = "no_link", http_status = NA_integer_, action = "RECOVER", title = substr(title, 1, 80))
readr::write_excel_csv(bind_rows(plan, nolink), file.path(LAKE, "lake_download_plan.csv"))

# ---- summary -----------------------------------------------------------------
message("\n==== reachability of the 119 linked examples ====")
print(plan %>% count(reachability, action) %>% arrange(desc(n)))
message(sprintf("\nlive-AUTO: %d | live-SCRAPE: %d | BLOCKED (needs browser): %d | RECOVER (gone/dead): %d | NO_LINK: %d",
        sum(plan$action %in% c("AUTO","AUTO_META")), sum(plan$action=="SCRAPE"),
        sum(plan$action=="SCRAPE_BROWSER"), sum(plan$action=="RECOVER"), nrow(nolink)))
message(sprintf("wrote %s/lake_download_plan.csv (%d rows)", LAKE, nrow(plan)+nrow(nolink)))
