# ==============================================================================
# GAIN SDG DATAPOINT REVIEW SHEET  (human QA of the extracted values)
#
# Builds a prioritised Excel workbook from SDG_DATAPOINTS.csv so a reviewer can
# confirm/reject each extracted value fastest-payoff first. Priority is driven by
# the automatic gate signals:
#   P1 - REJECT LIKELY : value_in_evidence == FALSE (the value is not in the
#                        source text) OR value is the prompt's example "9.2" and
#                        not verifiable -> echo / fabrication the gate flagged.
#   P2 - CHECK PROXY   : match demoted (measure not named in the evidence) -
#                        may be a valid proxy for a related indicator.
#   P3 - QUICK CONFIRM : clean exact/comparable/related match with the value
#                        present in the evidence - the keepers, spot-check.
#
# Reviewer fills the `decision` column (keep / reject / fix) and `reviewer_note`.
# Output: sdg_docs/SDG_DATAPOINTS_REVIEW.xlsx
# ==============================================================================

suppressMessages({ library(tidyverse); library(openxlsx) })
setwd_ok <- TRUE
dp <- read_csv("sdg_docs/SDG_DATAPOINTS.csv", show_col_types = FALSE)

ECHO_VALUE <- "9.2"   # the classifier prompt's example value - see PROMPT note

rev <- dp %>%
  mutate(
    match_short = if_else(str_detect(match_type, "demoted"),
                          "related (demoted)", match_type),
    value_missing = !coalesce(value_in_evidence, FALSE),
    is_echo_value = str_squish(as.character(value)) == ECHO_VALUE,
    review_priority = case_when(
      value_missing                         ~ 1L,
      is_echo_value                         ~ 1L,
      str_detect(match_type, "demoted")     ~ 2L,
      TRUE                                  ~ 3L),
    review_reason = case_when(
      value_missing & is_echo_value ~ "P1 REJECT: value not in source AND equals prompt example (9.2) - echo/fabrication",
      value_missing                 ~ "P1 REJECT: value does not appear in the source text",
      is_echo_value                 ~ "P1 CHECK: equals prompt example value (9.2) - confirm genuine vs echo",
      str_detect(match_type,"demoted") ~ "P2 CHECK: measure not named in evidence - valid proxy for this SDG?",
      match_type %in% c("exact","comparable") ~ "P3 CONFIRM: high-value match, value present - spot-check",
      TRUE                          ~ "P3 CONFIRM: related proxy, value present - spot-check"),
    decision = "", reviewer_note = "") %>%
  arrange(review_priority, desc(is_priority), sdg_code, country) %>%
  transmute(review_priority, review_reason, decision, reviewer_note,
            country, sdg_code, is_priority, match_type = match_short,
            population, population_detail,
            value, unit, year, measure_as_stated,
            evidence, page, value_in_evidence,
            source, production_method, instrument, doc_id, url)

# ---- workbook -----------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Review")
writeData(wb, "Review", rev, withFilter = TRUE)
freezePane(wb, "Review", firstActiveRow = 2)
setColWidths(wb, "Review", cols = 1:ncol(rev),
             widths = c(8, 62, 10, 22, 14, 8, 8, 20, 14, 20, 8, 6, 6, 30, 60, 6, 10, 12, 14, 26, 16, 40))
# priority colour bands
p1 <- createStyle(fgFill = "#F8CBAD"); p2 <- createStyle(fgFill = "#FFE699"); p3 <- createStyle(fgFill = "#C6E0B4")
hdr <- createStyle(textDecoration = "bold", fgFill = "#404040", fontColour = "white", wrapText = TRUE)
addStyle(wb, "Review", hdr, rows = 1, cols = 1:ncol(rev), gridExpand = TRUE)
for (pr in 1:3) {
  rows <- which(rev$review_priority == pr) + 1
  if (length(rows)) addStyle(wb, "Review", get(paste0("p", pr)), rows = rows, cols = 1,
                             gridExpand = TRUE, stack = TRUE)
}
addStyle(wb, "Review", createStyle(wrapText = TRUE, valign = "top"),
         rows = 2:(nrow(rev)+1), cols = c(2, 15), gridExpand = TRUE, stack = TRUE)

# ---- summary sheet ------------------------------------------------------------
addWorksheet(wb, "Summary")
summ <- rev %>% count(review_priority, review_reason, name = "rows") %>% arrange(review_priority)
writeData(wb, "Summary", tibble(metric = "total datapoints", value = as.character(nrow(rev))))
writeData(wb, "Summary", summ, startRow = 3)
writeData(wb, "Summary", tibble(
  note = c("", "P1 rows are the ones to look at first - the gate suspects the value.",
           "The classifier prompt uses 9.2 as its worked example; genuine 9.2 values",
           "exist (e.g. Slovenia refugee unemployment) so 9.2 is CHECK, not auto-reject.",
           "Fill 'decision' = keep / reject / fix, and 'reviewer_note'.")),
  startRow = 3 + nrow(summ) + 2)

out <- "sdg_docs/SDG_DATAPOINTS_REVIEW.xlsx"
saveWorkbook(wb, out, overwrite = TRUE)

message("Wrote ", out)
message("priority breakdown:")
print(as.data.frame(count(rev, review_priority, name = "rows")), row.names = FALSE)
message("P1 (reject/echo): ", sum(rev$review_priority==1),
        " | P2 (proxy): ", sum(rev$review_priority==2),
        " | P3 (confirm): ", sum(rev$review_priority==3))
