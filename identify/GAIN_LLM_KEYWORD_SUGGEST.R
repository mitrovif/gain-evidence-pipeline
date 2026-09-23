# ==============================================================================
# GAIN LLM KEYWORD + GROUNDING SUGGESTER
#
# Reads the actual EGRISS recommendation documents (IRRS/IRIS/IROSS) and uses
# the LOCAL LLM (qwen2.5, via GAIN_OLLAMA_HELPERS.R) to:
#   1. Draft a short "grounding cheat-sheet" - save it as
#      GAIN_RECOMMENDATION_GROUNDING.txt to have GAIN_OLLAMA_HELPERS.R prepend
#      it to every extract_evidence call (see the comment there).
#   2. Suggest NEW search keywords/phrases (by language + population group) that
#      the recommendations use but our current keyword_dict (in
#      GAIN_LAYER2_SEARCH_API_5.R) might be missing.
#
# HUMAN REVIEW REQUIRED for both outputs - nothing here is auto-applied to the
# search keyword dictionary or the grounding file. Past experience on this
# project (the Spain/Brazil false-positive floods from overly broad bare terms
# like "migration"/"nationality") showed why keyword breadth needs a precision
# check before going live - this script proposes, you decide.
#
# HOW TO USE:
#   1. Put the recommendation documents (PDF, DOCX, or TXT) in this folder, or
#      point RECOMMENDATION_FILES below at their exact paths.
#   2. Make sure Ollama is running (qwen2.5:7b).
#   3. source("identify/GAIN_LLM_KEYWORD_SUGGEST.R")
#   4. Review keyword_suggestions_[date].csv and
#      GAIN_RECOMMENDATION_GROUNDING_DRAFT.txt before using either.
# ==============================================================================

suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
source("shared/GAIN_OLLAMA_HELPERS.R")
stamp <- format(Sys.Date(), "%Y%m%d")
# Root-caused, confirmed by direct timing: the first attempt (CHUNK_CHARS=
# 25000) had every one of 52 calls come back as NA after running for its FULL
# allotted time - not a parsing failure, a genuine timeout (measured at
# exactly 300.1s, i.e. the OLD hardcoded req_timeout(300) in .ollama_generate).
# A follow-up "fix" that doubled num_ctx to 16384 made it WORSE (still timed
# out at 300s) because a larger context window means more compute per token on
# this CPU-bound setup - the opposite of what a timeout problem needs. The
# actual fix: keep num_ctx at its normal default, use a genuinely generous
# timeout (this script passes 900s explicitly, see below), and keep chunks at
# a size independently confirmed to complete fast (an 8000-char test chunk
# returned a valid, well-formed response with no timeout at all).
KW_TIMEOUT_SECS <- 900

# ------------------------------------------------------------------------------
# CONFIG - point this at your recommendation documents. Leave as "auto" to have
# the script look for common filename patterns in this folder.
# ------------------------------------------------------------------------------
# Folder holding the three recommendation PDFs: set GAIN_RECS_DIR in .Renviron.
RECS_DIR <- Sys.getenv("GAIN_RECS_DIR", ".")
RECOMMENDATION_FILES <- file.path(RECS_DIR, c(
  "International-Recommendations-on-Refugee-Statistics.pdf",
  "The-International-Recommendations-on-IDP-Statistics.pdf",
  "International_Recommendations_on_Statelessness_Statistics_Jan_2023_final.pdf"))
CHUNK_CHARS <- 8000              # per-chunk budget - independently confirmed fast/reliable at
# this size on this hardware. Larger chunks reduce the NUMBER of LLM calls but
# each one gets meaningfully slower on CPU, and once a call exceeds the
# timeout it produces NOTHING (not partial credit) - so smaller, reliable
# chunks that all actually complete beat fewer chunks that mostly time out.

if (identical(RECOMMENDATION_FILES, "auto")) {
  RECOMMENDATION_FILES <- list.files(
    pattern = "(?i)(IRRS|IRIS|IROSS|EGRISS).*recommendation.*\\.(pdf|docx|txt)$|(?i)recommendation.*(IRRS|IRIS|IROSS)",
    full.names = TRUE)
}
if (length(RECOMMENDATION_FILES) == 0) {
  stop(
"No recommendation documents found. Put the IRRS/IRIS/IROSS PDF/DOCX/TXT files in\n",
"this folder (or set RECOMMENDATION_FILES at the top of this script to their exact\n",
"paths), then re-run. This script never guesses or downloads them for you.")
}
message("Reading recommendation document(s):\n  ", paste(RECOMMENDATION_FILES, collapse = "\n  "))

# ------------------------------------------------------------------------------
# Text extraction per file type
#
# SAFETY: on Windows, pdftools/poppler (and some other compiled libraries) can
# CRASH THE WHOLE R SESSION - not raise a catchable error - when a file's full
# path exceeds ~260 characters (the legacy MAX_PATH limit). This project's own
# folder path is already ~190 characters deep before any filename, so a long
# recommendation-document filename can realistically push a path over that
# limit. To make this safe regardless of where it's run from, every file is
# first copied to a short path (R's session tempdir, near the drive root) and
# read FROM THERE. This cost nothing (a fast local copy) and eliminates an
# entire class of otherwise-silent crash.
# ------------------------------------------------------------------------------
.safe_short_path <- function(path) {
  short_dir <- file.path(tempdir(), "gain_kw")
  dir.create(short_dir, showWarnings = FALSE, recursive = TRUE)
  short_path <- file.path(short_dir, paste0(rlang::hash(path), ".", tools::file_ext(path)))
  if (!file.exists(short_path)) file.copy(path, short_path, overwrite = TRUE)
  short_path
}

read_any <- function(path) {
  path <- .safe_short_path(path)
  ext <- tolower(tools::file_ext(path))
  if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools not installed")
    paste(pdftools::pdf_text(path), collapse = "\n")
  } else if (ext == "docx") {
    if (!requireNamespace("officer", quietly = TRUE)) stop("officer not installed")
    paste(officer::docx_summary(officer::read_docx(path))$text, collapse = "\n")
  } else {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
}

full_text <- map_chr(RECOMMENDATION_FILES, read_any) %>% paste(collapse = "\n\n")
full_text <- str_squish(full_text)
message("Total extracted text: ", nchar(full_text), " chars")

# split into chunks on paragraph-ish boundaries, respecting CHUNK_CHARS
chunk_text <- function(txt, size) {
  n <- ceiling(nchar(txt) / size)
  map_chr(seq_len(n), ~ substr(txt, (.x - 1) * size + 1, .x * size))
}
chunks <- chunk_text(full_text, CHUNK_CHARS)
message("Split into ", length(chunks), " chunk(s) of ~", CHUNK_CHARS, " chars each")

# ------------------------------------------------------------------------------
# Per-chunk LLM call: keyword suggestions + a short thematic summary (used to
# assemble the grounding cheat-sheet afterwards)
# ------------------------------------------------------------------------------
LANGS_SUPPORTED <- c("en","fr","es","ar","ru","tr","pt","sw","uk","de","nl","it","zh")

# FROZEN at the literal "v3": this cache key used to borrow the EXTRACT
# prompt's PROMPT_VERSION, which meant bumping that (e.g. v3 -> v4 for the
# extraction improvements) would have silently invalidated every cached chunk
# of THIS long-running job on its next resume - hours of re-computation for a
# prompt this script doesn't even use. The keyword prompt has its own life;
# bump KW_PROMPT_VERSION only when THIS file's prompt text changes.
KW_PROMPT_VERSION <- "v3"

suggest_from_chunk <- function(chunk, idx) {
  key <- rlang::hash(paste("kwsuggest", KW_PROMPT_VERSION, chunk))
  cache <- file.path("ollama_cache", paste0("kwsuggest_", substr(key, 1, 16), ".rds"))
  if (file.exists(cache)) return(readRDS(cache))

  prompt <- paste0(
"You are helping refine a web-search keyword list for the EGRISS GAIN exercise, which ",
"finds official statistics that include refugees, IDPs, returnees, or stateless people. ",
"Below is an excerpt from an actual EGRISS recommendation document (IRRS/IRIS/IROSS).\n\n",
"EXCERPT:\n\"\"\"\n", chunk, "\n\"\"\"\n\n",
"From this excerpt ONLY, extract:\n",
"1. A 2-4 sentence SUMMARY of the inclusion mechanisms or definitions it discusses ",
"(for a grounding note used elsewhere - be concrete, not generic).\n",
"2. UP TO 8 distinctive search-worthy PHRASES this text uses for displacement/statelessness ",
"statistical inclusion that a keyword-based web search might currently miss (e.g. specific ",
"technical terms, named modules, disaggregation categories - NOT generic words like ",
"'refugee' or 'statistics' alone, which are already covered). Only propose phrases that are ",
"SPECIFIC enough to be useful in a search query without causing false positives.\n\n",
"Return ONLY JSON:\n",
'{"summary": "", "keywords": [{"phrase": "", "population": "refugees|idps|stateless|returnees|general", "rationale": ""}]}')

  raw <- .ollama_generate(prompt, timeout = KW_TIMEOUT_SECS)
  out <- list(chunk_idx = idx, summary = "", keywords = tibble())
  if (!is.na(raw)) {
    j <- regmatches(raw, regexpr("(?s)\\{.*\\}", raw, perl = TRUE))
    obj <- tryCatch(fromJSON(if (length(j)) j else raw), error = function(e) NULL)
    if (!is.null(obj)) {
      out$summary <- as.character(obj$summary %||% "")
      if (!is.null(obj$keywords) && is.data.frame(obj$keywords) && nrow(obj$keywords) > 0)
        out$keywords <- as_tibble(obj$keywords) %>% mutate(chunk_idx = idx)
    }
  }
  saveRDS(out, cache); out
}

message("Asking qwen2.5 to read each chunk (cached; resumable)...")
results <- map(seq_along(chunks), ~ suggest_from_chunk(chunks[.x], .x))

# ------------------------------------------------------------------------------
# OUTPUT 1: keyword suggestions for human review (never auto-merged)
# ------------------------------------------------------------------------------
suggestions <- map_dfr(results, "keywords")
if (nrow(suggestions) > 0) {
  suggestions <- suggestions %>%
    distinct(phrase, .keep_all = TRUE) %>%
    transmute(phrase, population = coalesce(population, "general"),
              rationale, source_chunk = chunk_idx,
              already_in_keyword_dict = NA,   # fill in manually after checking GAIN_LAYER2_SEARCH_API_5.R
              add_to_lang = "en",             # which keyword_dict language row to add this under (edit as needed)
              approved = "")                  # leave blank; set "yes"/"no" as you review
  write_excel_csv(suggestions, paste0("keyword_suggestions_", stamp, ".csv"))
  message("\nKeyword suggestions written: ", nrow(suggestions),
          " -> keyword_suggestions_", stamp, ".csv")
  message("REVIEW before adding any to keyword_dict in GAIN_LAYER2_SEARCH_API_5.R - ",
          "past broad additions (bare 'migration'/'nationality') caused false-positive floods.")
} else {
  message("No keyword suggestions extracted (check Ollama is running and the document read cleanly).")
}

# ------------------------------------------------------------------------------
# OUTPUT 2: a draft grounding cheat-sheet, condensed from the per-chunk summaries
# ------------------------------------------------------------------------------
summaries <- map_chr(results, "summary")
summaries <- summaries[nzchar(summaries)]
if (length(summaries) > 0) {
  condense_prompt <- paste0(
"Combine these notes (each summarising part of the EGRISS IRRS/IRIS/IROSS recommendations) ",
"into ONE dense reference paragraph (max 1800 characters) covering: the key definitions and ",
"the range of inclusion mechanisms these recommendations recognise (counting, disaggregation, ",
"registers, sampling frames, dedicated modules, etc.). Write it as a reference note for someone ",
"assessing documents - factual, no filler, no repetition of 'the recommendations state'.\n\n",
paste(summaries, collapse = "\n\n"))
  # json = FALSE: this asks for a PROSE paragraph; JSON-forcing returned "{}"
  draft <- .ollama_generate(condense_prompt, timeout = KW_TIMEOUT_SECS, json = FALSE)
  draft_txt <- if (!is.na(draft)) str_squish(draft) else paste(summaries, collapse = " ")
  writeLines(substr(draft_txt, 1, 2200), "GAIN_RECOMMENDATION_GROUNDING_DRAFT.txt")
  message("\nDraft grounding note written: GAIN_RECOMMENDATION_GROUNDING_DRAFT.txt (",
          nchar(draft_txt), " chars)")
  message("REVIEW it, then rename/copy to GAIN_RECOMMENDATION_GROUNDING.txt to activate it ",
          "(GAIN_OLLAMA_HELPERS.R will start prepending it to every extract_evidence call).")
}

message("\n==================== DONE ====================")
message("Nothing was auto-applied. Review both output files before using them.")
