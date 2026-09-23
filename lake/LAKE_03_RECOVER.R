# ==============================================================================
# GAIN DATA LAKE - step 1c: RECOVER sources that are gone/missing, and QUEUE the
# ones that exist but are bot-blocked.
#
# The link audit (LAKE_02) found most "dead" links are actually 403/429 BLOCKED
# (the page exists, a headless client is refused) not gone. Those go to a
# browser-scrape queue. Only the genuinely gone / dead-host / no-link records
# need recovery, which we try three ways before concluding a data request:
#   (1) Wayback Machine  - archived snapshot of the exact URL
#   (2) scrape cross-match - our own crawl already fetched 1140 live NSO pages;
#       match this example (country + org + title) to one on that office's site
#   (3) still nothing -> data-access reach-out list (office is named in the roster)
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
suppressMessages(source("shared/GAIN_COMMON.R"))
LAKE <- "data_lake"
plan   <- suppressMessages(read_csv(file.path(LAKE, "lake_download_plan.csv"), show_col_types = FALSE))
roster <- suppressMessages(read_csv(file.path(LAKE, "lake_roster.csv"), show_col_types = FALSE))

.ORG_STOP <- c("the","of","and","for","de","la","le","les","du","des","und","der","van","el","los","las","office","bureau","agency","department","ministry","national","statistics","statistical","institute")
toks <- function(s) { x <- gsub("[^a-z0-9 ]"," ", tolower(coalesce(as.character(s),""))); t <- str_split(str_squish(x)," ")[[1]]
  setdiff(unique(t[nchar(t) > 2]), .ORG_STOP) }
jacc <- function(a, b) { if (!length(a) || !length(b)) return(0); length(intersect(a,b)) / length(union(a,b)) }
host_of <- function(u) sub("^www\\d*\\.","", tolower(coalesce(str_match(coalesce(u,""),"^https?://([^/]+)")[,2],"")))

# ---- blocked-but-exists -> browser-scrape queue ------------------------------
browserq <- plan %>% filter(action == "SCRAPE_BROWSER") %>%
  transmute(example_id, country, organisation, url, http_status, populations, recommendations, title)
readr::write_excel_csv(browserq, file.path(LAKE, "lake_browser_queue.csv"))
message(sprintf("browser-scrape queue (blocked but exists): %d links", nrow(browserq)))

recov <- plan %>% filter(action == "RECOVER")

# ---- (1) Wayback Machine for gone/dead-host links (lenient JSON) --------------
CACHE <- file.path(LAKE, "lake_wayback_cache.rds"); wb <- if (file.exists(CACHE)) readRDS(CACHE) else list()
wayback <- function(u, yr) {
  if (is.na(u) || !nzchar(u)) return(NA_character_)
  k <- paste(u, yr); if (!is.null(wb[[k]])) return(wb[[k]])
  ts <- if (is.na(yr)) "" else paste0(yr, "0601")
  res <- tryCatch({
    rp <- request("https://archive.org/wayback/available") |> req_url_query(url = u, timestamp = ts) |>
      req_timeout(15) |> req_error(is_error = \(x) FALSE) |> req_perform()
    j <- tryCatch(fromJSON(resp_body_string(rp)), error = function(e) NULL)
    snap <- j$archived_snapshots$closest
    if (!is.null(snap) && isTRUE(as.logical(snap$available))) snap$url else NA_character_
  }, error = function(e) NA_character_)
  wb[[k]] <<- res; res
}
wu <- recov %>% filter(!is.na(url) & nzchar(url))
message(sprintf("Wayback-checking %d gone/dead-host links ...", nrow(wu)))
wmap <- setNames(map2_chr(wu$url, wu$year, wayback), wu$example_id); saveRDS(wb, CACHE)
recov$wayback_url <- unname(wmap[as.character(recov$example_id)])

# ---- (2) cross-match to the live scrape index (FINAL evidence) ---------------
fin <- tail(sort(list.files(".", "^GAIN_EVIDENCE_FINAL_.*\\.csv$")), 1)
ev <- suppressMessages(read_csv(fin, show_col_types = FALSE)) %>%
  transmute(country = harmonize_country(as.character(country)),
            org = coalesce(as.character(llm_organization), as.character(producer)),
            title = coalesce(as.character(llm_instrument_or_title), as.character(title)),
            url = coalesce(as.character(url), as.character(Found_On_Page)), doc_type = as.character(doc_type)) %>%
  filter(!is.na(url), nzchar(url), !doc_type %in% "unreachable") %>% mutate(ttok = map(title, toks))
ev_by_ctry <- split(ev, ev$country)
xmatch <- function(ctry, org, ttl) {
  cand <- ev_by_ctry[[harmonize_country(ctry)]]; if (is.null(cand) || !nrow(cand)) return(c(url = NA, score = "0"))
  ot <- toks(org); tt <- toks(ttl)
  sc <- vapply(seq_len(nrow(cand)), function(i) 0.6*jacc(tt, cand$ttok[[i]]) + 0.4*jacc(ot, toks(cand$org[i])), numeric(1))
  if (max(sc) >= 0.20) c(url = cand$url[which.max(sc)], score = sprintf("%.2f", max(sc))) else c(url = NA, score = sprintf("%.2f", max(sc))) }
mm <- pmap(list(recov$country, recov$organisation, recov$title), function(c,o,t) xmatch(c,o,t))
recov$scrape_url <- map_chr(mm, "url"); recov$scrape_score <- map_chr(mm, "score")

recov <- recov %>% mutate(recovered_url = coalesce(wayback_url, scrape_url),
  recovery = case_when(!is.na(wayback_url) ~ "wayback", !is.na(scrape_url) ~ "scrape_crossmatch", TRUE ~ "none"))
readr::write_excel_csv(recov %>% select(example_id, country, organisation, title, reachability,
                       wayback_url, scrape_url, scrape_score, recovered_url, recovery),
                       file.path(LAKE, "lake_recovery.csv"))

# ---- final data-access reach-out list: only still-unrecovered ----------------
still <- recov %>% filter(recovery == "none")
reach <- still %>% group_by(country, organisation, lead_type) %>%
  summarise(n_examples = n(), populations = paste(sort(unique(unlist(str_split(populations, ";")))), collapse = ";"),
            frameworks = paste(sort(unique(recommendations[nzchar(recommendations)])), collapse = "; "),
            example_ids = paste(head(example_id, 15), collapse = ";"), .groups = "drop") %>% arrange(desc(n_examples))
readr::write_excel_csv(reach, file.path(LAKE, "lake_access_reachout.csv"))

message("\n==== recovery of gone / no-link examples ====")
print(recov %>% count(recovery))
message(sprintf("\nBROWSER queue: %d | recovered (wayback/crossmatch): %d | still need a data request: %d examples across %d offices",
        nrow(browserq), sum(recov$recovery != "none"), nrow(still), nrow(reach)))
