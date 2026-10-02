# ==============================================================================
# GAIN OFFICE DEEP CHECK  (offices with many leads: is it really new?)
#
# Why: Norway (24 leads, 15 GAIN examples) and the Netherlands (35 leads, 2 GAIN
# examples) produce long lead lists that are mostly (a) the same few statistics
# in many releases and languages and (b) products the office already reported.
# The general matcher misses many because titles are Norwegian/Dutch and the
# page-language field said "English". This step looks at ONE office at a time:
#   1. drop definition/glossary pages (e.g. "Refugee | CBS")
#   2. translate every title to English (local model, cached)
#   3. ask the local model, per lead, whether it is the same product as - or an
#      edition of - one of THAT office's GAIN examples, using title, URL path,
#      year and the AI's summary; deterministic URL overlap is added as evidence
#   4. group the remaining leads into products (one model call per office)
#   5. keep what was published in 2025-2026 (reviewer decision, Oct 2026);
#      undated leads are listed separately for a person to check
# Output: review/GAIN_OFFICE_CHECK_<date>.xlsx  (one sheet per office: products
#         with proposed action + a sheet of every lead) and a CSV twin.
# Countries: GAIN_DEEPCHECK_COUNTRIES="Norway;Netherlands (Kingdom of the)"
# Needs LM Studio running. Cached: re-runs only pay for new leads.
# CAUTION (Oct 2026 test on Norway/Netherlands): the local 7B model is NOT reliable
# for the "same product?" judgement - v1 over-matched, v2 under-matched (it called
# the unaccompanied-minors monitor 'different'). Use this output as a translated,
# grouped worklist; the final same-product call is made by a stronger model or a
# person (review/GAIN_OFFICE_CHECK_NO_NL_20261002.xlsx was made that way).
# ==============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(httr2); library(jsonlite); library(rlang); library(openxlsx) })
source("shared/GAIN_COMMON.R")
suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("LM Studio not reachable - start the server first.")

COUNTRIES <- str_split(Sys.getenv("GAIN_DEEPCHECK_COUNTRIES", "Norway;Netherlands (Kingdom of the)"), ";")[[1]]
RECENT    <- as.integer(str_split(Sys.getenv("GAIN_DEEPCHECK_YEARS", "2025;2026"), ";")[[1]])
DC_VERSION <- "deepcheck-v2"   # v2: shortlist by embedding, then pairwise yes/no (v1 list-of-15 prompt over-matched)

fin <- suppressMessages(read_csv(tail(sort(list.files(".", "^GAIN_EVIDENCE_FINAL_.*\\.csv$")), 1),
                                 show_col_types = FALSE, guess_max = 5000))
g   <- suppressMessages(read_csv("analysis_ready_group_roster.csv", show_col_types = FALSE)) %>%
  mutate(country = harmonize_country(mcountry))
chr <- function(x) coalesce(as.character(x), "")

# year of the lead: AI year, then publication year, then a date written in the URL
url_year <- function(u) {
  y <- str_match(u, "/(20[12][0-9])[/-]")[, 2]
  m <- str_match(u, "/(2[0-6])(0[1-9]|1[0-2])[0-3][0-9][a-z]?[/-]")[, 2]
  coalesce(y, if_else(is.na(m), NA_character_, paste0("20", m)))
}
GLOSSARY <- regex(paste0("/begrippen|/methoden/begrippen|/methods/definitions|/onze-diensten/methoden/begrippen|",
                         "/en-gb/our-services/methods/definitions|/metadata/(begrep|definisjon)|/variabler/"), ignore_case = TRUE)
GLOSSARY_TITLE <- regex("^(refugee|asylum seeker|asylum request|invited refugee|vluchteling.*|asylum seeker with a protected status|temporary protection directive)(\\s*[|-]\\s*cbs)?$",
                        ignore_case = TRUE)
url_path <- function(u) str_remove(str_remove(tolower(chr(u)), "^https?://[^/]+"), "[?#].*$")

# one FOUND item vs ONE GAIN example: same product / newer edition / different
ask_pair <- function(lead, e) {
  prompt <- paste0(
    "Is the FOUND item the same product as the GAIN EXAMPLE that this statistical office already reported? ",
    "Same product = the same statistic, survey, register output, report series or database (another table, ",
    "release, language version or year of it still counts). Different = a different statistic or topic, even ",
    "if both are about refugees or migrants. Be strict: shared population words alone are NOT enough.\n\n",
    "FOUND: ", lead$title_en, " (original: ", lead$title, ")\n  url path: ", lead$path,
    "\n  year: ", coalesce(as.character(lead$year), "unknown"), "\n  summary: ", str_trunc(chr(lead$summary), 300),
    "\n\nGAIN EXAMPLE: ", e$PRO03, " (", e$yrs, ")", if (nzchar(e$desc)) paste0("\n  description: ", e$desc) else "",
    "\n\nReturn JSON only: {\"verdict\": \"same|edition|different\", \"confidence\": \"high|medium|low\", ",
    "\"reason\": \"<max 15 words>\"}. edition = same product but a newer release than the example's years.")
  key <- hash(paste(DC_VERSION, OLLAMA_GEN_MODEL, prompt))
  c0 <- .cache_get("deepcheck", key); if (!is.null(c0)) return(c0)
  raw <- .ollama_generate(prompt, timeout = 120, json = TRUE)
  obj <- tryCatch(fromJSON(regmatches(raw, regexpr("(?s)\\{.*\\}", raw, perl = TRUE))), error = function(e) NULL)
  out <- list(verdict = tolower(chr(obj$verdict %||% "different")), confidence = tolower(chr(obj$confidence %||% "low")),
              reason = chr(obj$reason %||% ""))
  .cache_put("deepcheck", key, out); out
}
ask_same_product <- function(lead, ex, ex_vec) {
  v <- embed(paste(lead$title_en, str_trunc(chr(lead$summary), 300)))
  sims <- if (is.null(v)) rep(0, nrow(ex)) else vapply(ex_vec, function(w) if (is.null(w)) 0 else cosine_sim(v, w), numeric(1))
  top <- head(order(-sims), 3)
  best <- list(match = 0L, likelihood = "low", edition = FALSE, reason = sprintf("closest GAIN example sim %.2f: different", max(sims)))
  rank <- function(r) if (r$verdict %in% c("same", "edition")) match(r$confidence, c("high", "medium", "low")) else 9
  for (j in top) {
    r <- ask_pair(lead, ex[j, ])
    if (rank(r) < 9 && (best$match == 0L || rank(r) < match(best$likelihood, c("high", "medium", "low"))))
      best <- list(match = j, likelihood = r$confidence, edition = r$verdict == "edition",
                   reason = sprintf("%s (sim %.2f): %s", r$verdict, sims[j], r$reason))
  }
  best
}

group_products <- function(country, leads) {
  items <- paste0(leads$lead_id, ": ", leads$title_en, " [", coalesce(as.character(leads$year), "?"), "] ", leads$path, collapse = "\n")
  prompt <- paste0(
    "Group these publications from the statistical office of ", country, " into PRODUCTS. A product is one ",
    "statistic, survey, report series, register output or database; its tables, releases, editions, ",
    "news items and language versions belong to the same product. Give each product a short English name.\n\n",
    items, "\n\nReturn JSON only: {\"products\": [{\"name\": \"...\", \"ids\": [\"L01\", ...]}, ...]}. ",
    "Every id must appear exactly once.")
  key <- hash(paste(DC_VERSION, "group", OLLAMA_GEN_MODEL, prompt))
  c0 <- .cache_get("deepcheck", key); if (!is.null(c0)) return(c0)
  raw <- .ollama_generate(prompt, timeout = 300, json = TRUE)
  obj <- tryCatch(fromJSON(regmatches(raw, regexpr("(?s)\\{.*\\}", raw, perl = TRUE)), simplifyVector = FALSE),
                  error = function(e) NULL)
  map <- tibble(lead_id = character(), product = character())
  if (!is.null(obj$products)) for (p in obj$products)
    map <- bind_rows(map, tibble(lead_id = as.character(unlist(p$ids)), product = chr(p$name)))
  map <- map %>% filter(lead_id %in% leads$lead_id) %>% distinct(lead_id, .keep_all = TRUE)
  .cache_put("deepcheck", key, map); map
}

wb <- createWorkbook(); all_leads <- list(); summary_rows <- list()
for (ct in COUNTRIES) {
  leads <- fin %>% filter(is_series_primary, route == "NSO (direct)",
                          harmonize_country(disp_country) == harmonize_country(ct),
                          final_tier %in% c("reach out", "review then reach out", "review - possible GAIN match")) %>%
    transmute(country = disp_country, title = chr(disp_instrument), url = chr(disp_url), tier = final_tier,
              score = final_score, summary = coalesce(llm_quote, english_working_summary, extract_summary, ""),
              year = suppressWarnings(as.integer(coalesce(str_extract(chr(llm_year), "(19|20)[0-9]{2}"),
                                                          str_extract(chr(pub_year), "(19|20)[0-9]{2}"), url_year(chr(disp_url)))))) %>%
    distinct(url, .keep_all = TRUE) %>%
    mutate(lead_id = sprintf("L%02d", row_number()), path = url_path(url),
           glossary = str_detect(url, GLOSSARY) | str_detect(str_squish(title), GLOSSARY_TITLE))
  ex <- g %>% filter(country == harmonize_country(ct), !is.na(PRO03)) %>%
    transmute(pindex2, index, PRO03, desc = str_trunc(str_squish(chr(PRO13)), 160),
              yrs = paste0(chr(PRO04_year), "-", if_else(chr(PRO05_year) == "9999", "ongoing", chr(PRO05_year))),
              link = chr(PRO16))
  message(sprintf("\n## %s: %d leads (%d definition pages), %d GAIN examples", ct, nrow(leads), sum(leads$glossary), nrow(ex)))

  leads$title_en <- vapply(leads$title, function(t) translate_text(t, "English"), character(1))
  ex_vec <- lapply(paste(ex$PRO03, ex$desc), embed)
  res <- map(seq_len(nrow(leads)), function(i) {
    if (leads$glossary[i] || !nrow(ex)) return(list(match = 0L, likelihood = "low", edition = FALSE, reason = if (leads$glossary[i]) "definition page" else "office has no GAIN example"))
    message(sprintf("  [%s] %s", leads$lead_id[i], str_trunc(leads$title_en[i], 70)))
    ask_same_product(leads[i, ], ex, ex_vec)
  })
  leads <- leads %>% mutate(
    gain_match_no   = map_int(res, ~ coalesce(.x$match, 0L)),
    gain_likelihood = map_chr(res, ~ .x$likelihood),
    gain_edition    = map_lgl(res, ~ isTRUE(.x$edition)),
    gain_reason     = map_chr(res, ~ .x$reason),
    gain_example    = if_else(gain_match_no >= 1 & gain_match_no <= nrow(ex),
                              paste0(ex$pindex2[pmax(1, pmin(gain_match_no, nrow(ex)))], " | ",
                                     str_trunc(ex$PRO03[pmax(1, pmin(gain_match_no, nrow(ex)))], 70)), ""),
    # deterministic evidence: the GAIN example's own link shares the URL path
    url_overlap = map_lgl(seq_len(n()), function(i) {
      if (!nzchar(path[i]) || !nrow(ex)) return(FALSE)
      p <- str_split(path[i], "/")[[1]]; p <- p[nchar(p) > 3]
      any(vapply(paste(ex$link, ex$PRO03), function(l) length(p) > 0 && sum(str_detect(tolower(l), fixed(p))) >= 2, logical(1))) }),
    gain_likelihood = if_else(url_overlap & gain_likelihood == "low", "medium", gain_likelihood),
    recent = !is.na(year) & year %in% RECENT)

  keep <- leads %>% filter(!glossary)
  grp <- if (nrow(keep)) group_products(ct, keep) else tibble(lead_id = character(), product = character())
  leads <- leads %>% left_join(grp, by = "lead_id") %>%
    mutate(product = case_when(glossary ~ "(definition pages)", is.na(product) | !nzchar(product) ~ str_trunc(title_en, 60),
                               TRUE ~ product))
  lk <- function(x) match(x, c("high", "medium", "low"))
  prod <- leads %>% filter(!glossary) %>% group_by(product) %>%
    summarise(leads = n(), newest_year = suppressWarnings(max(year, na.rm = TRUE)),
              recent_leads = sum(recent), undated = sum(is.na(year)),
              in_gain = c("high", "medium", "low")[suppressWarnings(min(c(lk(gain_likelihood), 3)))],
              edition = any(gain_edition & lk(gain_likelihood) <= 2),
              gain_example = first(gain_example[order(lk(gain_likelihood))]),
              best_tier = first(tier[order(-score)]),
              example_titles = paste(head(unique(str_trunc(title, 60)), 3), collapse = " || "),
              links = paste(head(url, 3), collapse = " "), .groups = "drop") %>%
    mutate(newest_year = if_else(is.finite(newest_year), as.integer(newest_year), NA_integer_),
           proposed = case_when(
             in_gain == "high" & !edition                   ~ "already in GAIN - do not ask",
             in_gain %in% c("high", "medium") & edition & recent_leads > 0 ~ "ask to UPDATE the GAIN example",
             in_gain == "medium"                            ~ "check: may already be in GAIN",
             recent_leads > 0                               ~ "ask: new example (2025-2026)",
             undated > 0                                    ~ "undated - check by hand",
             TRUE                                           ~ "older than 2025 - do not ask"),
           `YOUR decision` = "", `YOUR note` = "") %>%
    arrange(factor(proposed, levels = c("ask: new example (2025-2026)", "ask to UPDATE the GAIN example",
             "check: may already be in GAIN", "undated - check by hand", "already in GAIN - do not ask",
             "older than 2025 - do not ask")), desc(leads))
  sh <- str_trunc(str_remove(ct, " \\(.*"), 25)
  addWorksheet(wb, sh); writeData(wb, sh, prod)
  addStyle(wb, sh, createStyle(textDecoration = "bold", fgFill = "#DCE6F1", wrapText = TRUE), rows = 1, cols = seq_along(prod), gridExpand = TRUE)
  setColWidths(wb, sh, cols = seq_along(prod), widths = "auto"); freezePane(wb, sh, firstRow = TRUE)
  all_leads[[ct]] <- leads
  summary_rows[[ct]] <- prod %>% count(proposed) %>% mutate(country = ct)
  message("  products: ", nrow(prod)); print(count(prod, proposed))
}
L <- bind_rows(all_leads) %>% select(country, lead_id, product, title, title_en, year, recent, glossary, tier, score,
                                     gain_likelihood, gain_edition, gain_example, gain_reason, url_overlap, url)
addWorksheet(wb, "all leads"); writeData(wb, "all leads", L)
setColWidths(wb, "all leads", cols = seq_along(L), widths = "auto")
dir.create("review", showWarnings = FALSE)
out <- sprintf("review/GAIN_OFFICE_CHECK_%s.xlsx", format(Sys.Date(), "%Y%m%d"))
saveWorkbook(wb, out, overwrite = TRUE); write_excel_csv(L, sub("xlsx$", "csv", out))
message("\nwrote ", out)
