# ==============================================================================
# GAIN CALIBRATE  (check the pipeline's judgements against a human reviewer)
#
# Two modes, picked automatically:
#   1. No review sheet yet  -> draws a stratified sample of N candidates
#      (default 20) and writes review/GAIN_REVIEW_<N>.xlsx for a person to fill.
#      The pipeline's own answers are kept in a hidden "key" sheet so the
#      reviewer is not steered by them.
#   2. Sheet exists and is filled -> compares reviewer vs pipeline on
#        A) GAIN match   : already in GAIN / new edition / new
#        B) relevance    : ask for it / maybe / no
#      prints agreement tables and writes review/GAIN_REVIEW_<N>_RESULT.md.
#
# To start a new round (bigger sample): Sys.setenv(GAIN_REVIEW_N = "50")
# Input : newest GAIN_EVIDENCE_FINAL_*.csv (GAIN_FINALIZE.R output, which carries
#         the match_v2_* columns and the tiers)
# Name a new round without overwriting the old one: Sys.setenv(GAIN_REVIEW_ROUND = "b")
# ==============================================================================
suppressMessages({ library(tidyverse); library(openxlsx) })

N      <- as.integer(Sys.getenv("GAIN_REVIEW_N", "20"))
SEED   <- 20260923L
dir.create("review", showWarnings = FALSE)
ROUND <- Sys.getenv("GAIN_REVIEW_ROUND", "")
sheet_path <- file.path("review", sprintf("GAIN_REVIEW_%d%s.xlsx", N, ROUND))

MATCH_CHOICES <- c("already in GAIN (same product)", "new edition of a GAIN example",
                   "new - not in GAIN", "can't tell")
REL_CHOICES   <- c("yes - ask for it", "maybe", "no - not relevant", "can't tell")

# pipeline answer -> the reviewer's 3-way vocabulary
pipe_match <- function(cat) case_when(
  cat == "same product"                         ~ "already in GAIN (same product)",
  cat == "new edition"                          ~ "new edition of a GAIN example",
  cat == "new output of GAIN example"           ~ "new - not in GAIN",
  cat %in% c("check - likely same", "check - other language") ~ "check (undecided)",
  TRUE                                          ~ "new - not in GAIN")
pipe_rel <- function(tier) case_when(
  tier %in% c("reach out", "review then reach out", "review - possible GAIN match") ~ "yes - ask for it",
  tier %in% c("watch / context", "unsure - check questionnaire",
              "could not read - check by hand")                  ~ "maybe",
  tier %in% c("low", "historic - context only", "junk - login/redirect page") ~ "no - not relevant",
  TRUE                                                           ~ "not scored")

# ------------------------------------------------------------------------------
# MODE 1 - make the sheet
# ------------------------------------------------------------------------------
if (!file.exists(sheet_path)) {
  mf <- tail(sort(list.files(".", "^GAIN_EVIDENCE_FINAL_.*\\.csv$")), 1)
  stopifnot("run GAIN_FINALIZE.R first" = length(mf) == 1)
  d <- suppressMessages(read_csv(mf, show_col_types = FALSE, guess_max = 5000)) %>%
    mutate(row_id = row_number())
  # already-reviewed rows (any earlier round) are never drawn again
  seen <- unlist(lapply(setdiff(list.files("review", "^GAIN_REVIEW_.*\\.xlsx$", full.names = TRUE), sheet_path),
    function(f) tryCatch(read.xlsx(f, "review")$link, error = function(e) NULL)))
  d <- d %>% filter(!url %in% seen, coalesce(is_series_primary, TRUE))
  refresh_urls <- unlist(lapply(list.files(".", "^LAYER2_search_hits_since.*\\.csv$"),
    function(f) suppressMessages(read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")))$url))
  FOCUS <- Sys.getenv("GAIN_REVIEW_FOCUS", if (nzchar(ROUND)) "feedback" else "categories")
  strata <- if (FOCUS == "feedback") list(
    # round 2: aimed at what the round-1 reviewer flagged (see GAIN_FINALIZE.R rules)
    list("questionnaire decided",  function(x) !is.na(x$questionnaire_verdict) &
           x$questionnaire_verdict %in% c("displacement questions found", "no displacement question"), 4),
    list("still unsure (web page)", function(x) x$final_tier == "unsure - check questionnaire", 4),
    list("new output / edition",   function(x) x$match_v2_category %in% c("new output of GAIN example", "new edition"), 4),
    list("agency route",           function(x) x$route == "international agency" &
           x$final_tier %in% c("reach out", "review then reach out"), 2),
    list("regional (Eurostat)",    function(x) x$route == "regional body", 2),
    list("new since June",         function(x) x$url %in% refresh_urls &
           x$final_tier %in% c("reach out", "review then reach out", "unsure - check questionnaire"), 4))
  else list(
    list("same product", function(x) x$match_v2_category == "same product", 4),
    list("new edition", function(x) x$match_v2_category == "new edition", 2),
    list("check - likely same", function(x) x$match_v2_category == "check - likely same", 2),
    list("check - other language", function(x) x$match_v2_category == "check - other language", 3),
    list("same org, other product", function(x) x$match_v2_category == "same org, other product", 4),
    list("other producer", function(x) x$match_v2_category == "other producer", 3),
    list("new country", function(x) x$match_v2_category == "new country", 2))
  set.seed(SEED)
  taken <- integer(0)
  s <- map_dfr(strata, function(st) {
    pool <- d %>% filter(st[[2]](d), !row_id %in% taken)
    got <- pool %>% slice_sample(n = min(st[[3]], nrow(pool))) %>% mutate(stratum = st[[1]])
    taken <<- c(taken, got$row_id)
    got
  }) %>% slice_sample(prop = 1)            # shuffle so strata are not grouped

  view <- s %>% transmute(
    review_id = sprintf("R%02d", row_number()),
    country   = Country,
    candidate = coalesce(llm_instrument_or_title, title),
    producer  = coalesce(llm_organization, producer),
    year      = coalesce(as.character(llm_year), as.character(pub_year)),
    evidence  = str_sub(coalesce(llm_quote, english_working_summary, extract_summary, ""), 1, 400),
    link      = url,
    closest_GAIN_example = if_else(is.na(match_v2_pindex2), "(no GAIN example in this country)",
                             sprintf("%s | %s | %s | %s", match_v2_pindex2, match_v2_gain_title,
                                     match_v2_gain_org, match_v2_gain_years)),
    other_GAIN_examples_same_country = match_v2_top3,
    questionnaire = coalesce(questionnaire_hits, ""),
    `YOUR: is it already in GAIN?` = "",
    `YOUR: relevant to ask for?`   = "",
    `YOUR: notes` = "")
  key <- s %>% transmute(review_id = view$review_id, row_id, stratum, match_v2_category, route,
                         pipeline_match = pipe_match(match_v2_category), match_v2_checks,
                         final_tier, final_score, final_confidence, llm_relevance, relevance_score)

  wb <- createWorkbook()
  addWorksheet(wb, "review")
  writeData(wb, "review", view)
  hs <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1", wrapText = TRUE, valign = "top")
  ys <- createStyle(fgFill = "#FFF2CC", wrapText = TRUE, valign = "top")
  addStyle(wb, "review", hs, rows = 1, cols = seq_along(view), gridExpand = TRUE)
  addStyle(wb, "review", createStyle(wrapText = TRUE, valign = "top"),
           rows = 2:(nrow(view) + 1), cols = 1:10, gridExpand = TRUE)
  addStyle(wb, "review", ys, rows = 1:(nrow(view) + 1), cols = 11:13, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, "review", cols = seq_along(view),
               widths = c(7, 14, 40, 22, 7, 60, 30, 45, 45, 40, 26, 20, 30))
  freezePane(wb, "review", firstActiveRow = 2, firstActiveCol = 3)
  addWorksheet(wb, "lists", visible = FALSE)
  writeData(wb, "lists", tibble(match = MATCH_CHOICES, rel = REL_CHOICES))
  dataValidation(wb, "review", col = 11, rows = 2:(nrow(view) + 1), type = "list",
                 value = "lists!$A$2:$A$5")
  dataValidation(wb, "review", col = 12, rows = 2:(nrow(view) + 1), type = "list",
                 value = "lists!$B$2:$B$5")
  addWorksheet(wb, "key", visible = FALSE)
  writeData(wb, "key", key)
  addWorksheet(wb, "how to fill")
  writeData(wb, "how to fill", tibble(`How to fill (about 30-40 min for 20 rows)` = c(
    "For each row, open the link if needed and fill the two yellow dropdowns:",
    "1) Is it already in GAIN? Compare with 'closest_GAIN_example' and the other GAIN examples of that country.",
    "   - already in GAIN = the SAME product (same survey/census/register/report series, same round)",
    "   - new edition = the same product but a LATER round/edition than the one in GAIN",
    "   - new = a different product (even if the same NSO already has other examples in GAIN)",
    "2) Relevant to ask for? Would you want this submitted to the GAIN 2026 round?",
    "Use 'can't tell' freely and add a short note - notes are the most useful part.",
    "The pipeline's own answers are hidden on purpose; run GAIN_CALIBRATE.R again when done.")))
  saveWorkbook(wb, sheet_path, overwrite = TRUE)
  message("Review sheet written: ", sheet_path, " (", nrow(view), " rows)")
  message("Fill the yellow columns, save, then source('GAIN_CALIBRATE.R') again to score.")
  print(count(key, stratum))
} else {
# ------------------------------------------------------------------------------
# MODE 2 - score the filled sheet
# ------------------------------------------------------------------------------
  v <- read.xlsx(sheet_path, sheet = "review", sep.names = " ")
  k <- read.xlsx(sheet_path, sheet = "key")
  x <- v %>% select(review_id, country, candidate,
                    human_match = `YOUR: is it already in GAIN?`,
                    human_rel = `YOUR: relevant to ask for?`, notes = `YOUR: notes`) %>%
    left_join(k, by = "review_id") %>%
    mutate(pipeline_rel = pipe_rel(final_tier))
  filled <- x %>% filter(coalesce(human_match, "") != "" | coalesce(human_rel, "") != "")
  if (!nrow(filled)) stop("No answers yet in ", sheet_path, " - fill the yellow columns first.")
  message(sprintf("Scoring %d of %d rows answered", nrow(filled), nrow(x)))

  m <- filled %>% filter(!coalesce(human_match, "") %in% c("", "can't tell"))
  r <- filled %>% filter(!coalesce(human_rel, "") %in% c("", "can't tell"))
  m_ok <- m %>% filter(pipeline_match != "check (undecided)") %>%
    summarise(n = n(), agree = sum(pipeline_match == human_match))
  # relevance: "yes" vs everything else is what drives outreach
  r_ok <- r %>% filter(pipeline_rel != "not scored") %>%
    summarise(n = n(), agree = sum(pipeline_rel == human_rel),
              yes_precision = sum(pipeline_rel == "yes - ask for it" & human_rel == "yes - ask for it") /
                              max(1, sum(pipeline_rel == "yes - ask for it")),
              yes_recall    = sum(pipeline_rel == "yes - ask for it" & human_rel == "yes - ask for it") /
                              max(1, sum(human_rel == "yes - ask for it")))
  cat("\n==== A) GAIN match: pipeline (rows) x reviewer (cols) ====\n")
  print(table(pipeline = m$pipeline_match, reviewer = m$human_match))
  cat(sprintf("decided rows agreeing: %d / %d\n", m_ok$agree, m_ok$n))
  cat("\n==== B) relevance: pipeline (rows) x reviewer (cols) ====\n")
  print(table(pipeline = r$pipeline_rel, reviewer = r$human_rel))
  cat(sprintf("agree %d / %d | of pipeline 'yes', reviewer also yes: %.0f%% | of reviewer 'yes', pipeline caught: %.0f%%\n",
              r_ok$agree, r_ok$n, 100 * r_ok$yes_precision, 100 * r_ok$yes_recall))
  dis <- filled %>% filter((pipeline_match != human_match & !human_match %in% c("", "can't tell") &
                             pipeline_match != "check (undecided)") |
                           (pipeline_rel != human_rel & !human_rel %in% c("", "can't tell")))
  cat("\n==== disagreements (these tell us what to fix) ====\n")
  for (i in seq_len(nrow(dis))) cat(sprintf("%s %s | %s\n   match: pipe=%s / you=%s | rel: pipe=%s (score %s) / you=%s\n   checks: %s\n   note: %s\n",
    dis$review_id[i], dis$country[i], str_sub(dis$candidate[i], 1, 70), dis$pipeline_match[i], dis$human_match[i],
    dis$pipeline_rel[i], dis$final_score[i], dis$human_rel[i], dis$match_v2_checks[i], coalesce(dis$notes[i], "")))
  out_md <- sub("\\.xlsx$", "_RESULT.md", sheet_path)
  writeLines(c(sprintf("# GAIN calibration - %s (%d rows answered)", Sys.Date(), nrow(filled)),
    sprintf("- GAIN match agreement (decided rows): %d / %d", m_ok$agree, m_ok$n),
    sprintf("- Relevance agreement: %d / %d; pipeline-yes confirmed %.0f%%; reviewer-yes caught %.0f%%",
            r_ok$agree, r_ok$n, 100 * r_ok$yes_precision, 100 * r_ok$yes_recall),
    sprintf("- Disagreements: %d (listed in console output)", nrow(dis))), out_md)
  message("\nwrote ", out_md)
}
