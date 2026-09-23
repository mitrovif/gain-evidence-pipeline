# ==============================================================================
# GAIN COVERAGE GAPS  (which countries is the scrape leaving dark, and why?)
#
# Crosses three things - the registry priorities, the Layer 3 inventory stats,
# and the actual evidence found by ANY layer - and writes GAIN_COVERAGE_GAPS.csv:
# every registry country with no/low evidence, why (no inventory? site dead?),
# and a suggested action. For domains with ZERO inventory it also does a quick
# liveness check (one HEAD request each): the most common cause of a dark NSO
# is simply that the domain in the registry has died or moved.
#
# Output: GAIN_COVERAGE_GAPS.csv (stable name - re-run overwrites).
# ==============================================================================

suppressMessages({ library(tidyverse); library(httr2) })
source("shared/GAIN_COMMON.R")

reg  <- read_csv("NSO_Full_Registry.csv", show_col_types = FALSE)
inv_f <- newest_file("^LAYER3_inventory_stats_.*\\.csv$")
ev_f  <- newest_file("^evidence_flagged_.*\\.csv$")
if (is.na(inv_f) || is.na(ev_f)) stop("Need LAYER3_inventory_stats_* and evidence_flagged_* files.")
inv <- read_csv(inv_f, show_col_types = FALSE)
ev  <- read_csv(ev_f,  show_col_types = FALSE)
cc  <- if ("Country" %in% names(ev)) "Country" else "country"
evc <- ev %>% count(.data[[cc]], name = "evidence_rows") %>% rename(country = 1)

gaps <- reg %>%
  select(country, domain, priority) %>%
  left_join(inv %>% select(domain, total_inventory, keyword_matches, coverage), by = "domain") %>%
  left_join(evc, by = "country") %>%
  mutate(evidence_rows   = coalesce(evidence_rows, 0L),
         total_inventory = coalesce(total_inventory, 0)) %>%
  filter(evidence_rows <= 2) %>%                      # the low/no-evidence set
  arrange(priority, evidence_rows, desc(total_inventory))

# liveness check ONLY for the zero-inventory domains (one HEAD request each)
check_alive <- function(domain) {
  for (proto in c("https://", "https://www.")) {
    r <- tryCatch(request(paste0(proto, domain)) %>%
                    req_method("HEAD") %>%
                    req_user_agent("EGRISS-GAIN-research (coverage check)") %>%
                    req_options(followlocation = TRUE) %>%
                    req_timeout(15) %>%
                    req_error(is_error = function(x) FALSE) %>%
                    req_perform(),
                  error = function(e) NULL)
    if (!is.null(r)) return(list(status = resp_status(r), final_url = resp_url(r)))
  }
  list(status = NA_integer_, final_url = NA_character_)
}

dark <- gaps %>% filter(total_inventory == 0)
message("Liveness-checking ", nrow(dark), " zero-inventory domain(s)...")
alive <- map(dark$domain, function(d) { Sys.sleep(0.5); check_alive(d) })
dark_diag <- dark %>%
  mutate(http_status = map_int(alive, ~ .x$status %||% NA_integer_),
         final_url   = map_chr(alive, ~ .x$final_url %||% NA_character_))

gaps <- gaps %>%
  left_join(dark_diag %>% select(domain, http_status, final_url), by = "domain") %>%
  mutate(
    domain_moved = !is.na(final_url) &
      !str_detect(coalesce(final_url, ""), fixed(domain)),
    suggested_action = case_when(
      total_inventory == 0 & is.na(http_status) ~
        "domain DEAD or unreachable - find the NSO's current website, update NSO_Full_Registry.csv",
      total_inventory == 0 & domain_moved ~
        paste0("domain REDIRECTS elsewhere (", final_url, ") - update registry to the new domain"),
      total_inventory == 0 & coalesce(http_status, 0) >= 400 ~
        paste0("site returns HTTP ", http_status, " - verify the URL / find the new one"),
      total_inventory == 0 ~
        "site alive but no sitemap and not in Common Crawl - manual publications-page check",
      keyword_matches == 0 ~
        "pages inventoried but nothing keyword-matched - possibly no online displacement stats, or non-covered language",
      TRUE ~ "some inventory + matches but little survived scoring - check the enriched report for this country"
    )) %>%
  select(country, domain, priority, total_inventory, keyword_matches,
         evidence_rows, http_status, final_url, suggested_action)

write_excel_csv(gaps, "GAIN_COVERAGE_GAPS.csv")
message("\n==================== COVERAGE GAPS ====================")
message(nrow(gaps), " low/no-evidence countries -> GAIN_COVERAGE_GAPS.csv")
message("\nPriority 1-2 headlines:")
print(as.data.frame(gaps %>% filter(coalesce(priority, 3) <= 2) %>%
        select(country, priority, evidence_rows, suggested_action) %>% head(15)),
      row.names = FALSE)
