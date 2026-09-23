# ==============================================================================
# GAIN SDG DOCS HARVEST  (workstream: SDG indicators for refugees/IDPs/stateless)
#
# Stage 1 of the SDG documentation database: DOWNLOAD the underlying documents
# (PDF reports, findings, datasets, report webpages) for
#   (a) documented GAIN survey examples - respondent-provided links in the
#       group roster (PRO13 / PRO14A / PRO16), and
#   (b) potential GAIN examples - the discovered candidates in evidence_flagged
# into sdg_docs/downloads/, and build sdg_docs/SDG_DOCS_MANIFEST.csv - one row
# per document with country / populations / source / production-method guess
# and EMPTY sdg_indicator columns to be filled in stage 2 (LLM classification
# against the priority-indicator paper).
#
# Resumable: already-downloaded URLs (status ok in the manifest) are skipped.
# Polite: 1s pause between real downloads; 35 MB per-file cap.
# Test mode: Sys.setenv(GAIN_SDG_LIMIT = "25") downloads only the first 25.
#
# All outputs live under sdg_docs/ - this workstream touches nothing else.
# ==============================================================================

suppressMessages({ library(tidyverse); library(httr2) })
source("shared/GAIN_COMMON.R")

DIR      <- "sdg_docs"
DL_GAIN  <- file.path(DIR, "downloads", "gain")
DL_CAND  <- file.path(DIR, "downloads", "candidates")
MANIFEST <- file.path(DIR, "SDG_DOCS_MANIFEST.csv")
for (d in c(DL_GAIN, DL_CAND)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
LIMIT      <- suppressWarnings(as.integer(Sys.getenv("GAIN_SDG_LIMIT", "0")))  # 0 = all
MAX_MB     <- 35

# ------------------------------------------------------------------------------
# 1. SOURCE LIST
# ------------------------------------------------------------------------------
is_url <- function(x) grepl("^https?://", coalesce(as.character(x), ""))

gr <- read_csv("analysis_ready_group_roster.csv", show_col_types = FALSE)
gain_src <- map_dfr(c("PRO13", "PRO14A", "PRO16"), function(col) {
  tibble(url = as.character(gr[[col]]),
         country = gr$mcountry, title = gr$PRO03,
         organization = gr$morganization, year = as.character(gr$PRO04_year),
         populations = str_squish(paste(
           if_else(coalesce(gr$PRO07.A, 0) == 1, "refugees", ""),
           if_else(coalesce(gr$PRO07.B, 0) == 1, "idps", ""),
           if_else(coalesce(gr$PRO07.C, 0) == 1, "stateless", ""))),
         link_field = col)
}) %>%
  filter(is_url(url)) %>%
  mutate(source = "gain_survey", match_type = "already_in_gain")

ev_f <- newest_file("^evidence_flagged_.*\\.csv$")
ev <- read_csv(ev_f, show_col_types = FALSE)
cand_src <- tibble(
  url = coalesce(ev$Found_On_Page, ev$url),
  country = coalesce(ev$Country, ev$country),
  title = coalesce(ev$Report_Title, ev$title),
  organization = as.character(ev$producer %||% NA),
  year = as.character(coalesce(ev$year, ev$Publication_Date)),
  populations = as.character(coalesce(ev$populations, ev$Populations)),
  link_field = "discovered",
  source = "candidate",
  match_type = as.character(ev$gain_match_type %||% NA)) %>%
  filter(is_url(url),
         !str_starts(coalesce(ev$outreach_category, ""), "E"))   # skip suppressed noise

src <- bind_rows(gain_src, cand_src) %>%
  distinct(source, url, .keep_all = TRUE) %>%
  mutate(doc_id = paste0(substr(source, 1, 4), "_", substr(map_chr(url, rlang::hash), 1, 10)))
message("Sources: ", sum(src$source == "gain_survey"), " GAIN survey links + ",
        sum(src$source == "candidate"), " candidate links = ", nrow(src))

# production-method GUESS from the title (transparent heuristic; stage 2 refines)
guess_method <- function(title) {
  t <- str_to_lower(coalesce(title, ""))
  case_when(
    str_detect(t, "census|recensement|censo|zensus|volkszählung|перепис") ~ "census",
    str_detect(t, "register|registre|registro|administrative|admin data|civil registration") ~ "admin data",
    str_detect(t, "survey|enquête|encuesta|dhs|mics|lfs|labour force|household budget|inquérito") ~ "survey",
    TRUE ~ "unknown")
}

# ------------------------------------------------------------------------------
# 2. DOWNLOAD (resumable via the manifest)
# ------------------------------------------------------------------------------
done <- if (file.exists(MANIFEST)) {
  read_csv(MANIFEST, show_col_types = FALSE) %>% filter(status == "ok") %>% pull(url)
} else character(0)
todo <- src %>% filter(!url %in% done)
if (LIMIT > 0) todo <- head(todo, LIMIT)
message(length(done), " already downloaded | ", nrow(todo), " to fetch",
        if (LIMIT > 0) paste0(" (LIMIT ", LIMIT, ")") else "")

ext_for <- function(ctype, url) {
  ct <- str_to_lower(coalesce(ctype, ""))
  if (str_detect(ct, "pdf"))  return("pdf")
  if (str_detect(ct, "spreadsheet|excel")) return("xlsx")
  if (str_detect(ct, "csv"))  return("csv")
  if (str_detect(ct, "zip"))  return("zip")
  if (str_detect(ct, "word")) return("docx")
  if (str_detect(ct, "html")) return("html")
  e <- str_to_lower(tools::file_ext(str_remove(url, "\\?.*$")))
  if (e %in% c("pdf", "xlsx", "xls", "csv", "zip", "docx", "doc")) e else "html"
}

for (i in seq_len(nrow(todo))) {
  r <- todo[i, ]
  res <- tryCatch({
    resp <- request(r$url) %>%
      req_user_agent("EGRISS-GAIN-research (SDG documentation; egriss.org)") %>%
      req_timeout(60) %>% req_error(is_error = function(x) FALSE) %>% req_perform()
    list(status = resp_status(resp),
         ctype  = resp_header(resp, "content-type") %||% "",
         body   = resp_body_raw(resp))
  }, error = function(e) list(status = NA_integer_, ctype = "", body = raw(0)))

  ok <- !is.na(res$status) && res$status < 400 && length(res$body) > 0 &&
        length(res$body) / 1e6 <= MAX_MB
  ext <- ext_for(res$ctype, r$url)
  dest_dir <- if (r$source == "gain_survey") DL_GAIN else DL_CAND
  fname <- if (ok) file.path(dest_dir, paste0(r$doc_id, ".", ext)) else NA_character_
  if (ok) writeBin(res$body, fname)

  row <- tibble(doc_id = r$doc_id, source = r$source, country = r$country,
    title = substr(coalesce(r$title, ""), 1, 150), organization = r$organization,
    year = r$year, populations = r$populations, match_type = r$match_type,
    link_field = r$link_field, url = r$url,
    file = coalesce(fname, ""), content_type = substr(res$ctype, 1, 60),
    size_kb = if (ok) round(length(res$body) / 1024) else NA_real_,
    http_status = res$status,
    status = if (ok) "ok" else if (is.na(res$status)) "unreachable"
             else if (length(res$body) / 1e6 > MAX_MB) "too_large" else "http_error",
    production_method_guess = guess_method(r$title),
    sdg_indicators = "",          # stage 2: filled against the priority-indicator paper
    sdg_notes = "",
    downloaded_at = as.character(Sys.Date()))
  write_csv(row, MANIFEST, append = file.exists(MANIFEST))
  if (i %% 25 == 0) message("  [", i, "/", nrow(todo), "] ...")
  Sys.sleep(1)
}

m <- read_csv(MANIFEST, show_col_types = FALSE)
message("\n==================== SDG DOCS HARVEST ====================")
message("manifest rows: ", nrow(m), " | ok: ", sum(m$status == "ok"),
        " | failed: ", sum(m$status != "ok"))
message("by source: ", paste(names(table(m$source)), table(m$source), sep = "=", collapse = " | "))
message("by file type: ", paste(names(table(tools::file_ext(m$file[m$status == "ok"]))),
        table(tools::file_ext(m$file[m$status == "ok"])), sep = "=", collapse = " | "))
message("total size: ", round(sum(m$size_kb, na.rm = TRUE) / 1024), " MB")
message("\nDatabase seed: ", MANIFEST)
