# ==============================================================================
# GAIN QA - "already in GAIN" matches review sheet
#
# Builds a reviewer workbook of every candidate the crossref judged to already
# be in GAIN, showing the FOUND artifact next to the GAIN example it maps to
# (pindex2 + title + reason + confidence), lowest-confidence first, with blank
# decision/notes columns to tick through. Lets a methodologist catch bad matches
# (e.g. a protection-seeker statistic mis-matched to an employment study).
#
# Input : newest GAIN_EVIDENCE_FINAL_*.csv (or evidence_flagged_*.csv)
# Output: GAIN_QA_INGAIN_matches_[date].xlsx
# ==============================================================================
suppressMessages({ library(tidyverse); library(openxlsx) })

f <- tail(sort(c(list.files(".", "^GAIN_EVIDENCE_FINAL_.*\\.csv$"),
                 list.files(".", "^evidence_flagged_.*\\.csv$"))), 1)
message("QA from: ", f)
d <- suppressMessages(read_csv(f, show_col_types = FALSE))
pk <- function(c) if (c %in% names(d)) d[[c]] else rep(NA, nrow(d))

conf_rank <- c(low = 1, medium = 2, high = 3)
qa <- tibble(
  found_country    = as.character(pk("country")),
  found_instrument = coalesce(as.character(pk("llm_instrument_or_title")), as.character(pk("title"))),
  found_url        = coalesce(as.character(pk("url")), as.character(pk("Found_On_Page"))),
  matched_pindex2  = as.character(pk("matched_gain_pindex2")),
  gain_example     = as.character(pk("matched_gain_title")),
  match_confidence = tolower(as.character(pk("gain_match_confidence"))),
  match_reason     = as.character(pk("gain_match_reason")),
  gain_match_type  = as.character(pk("gain_match_type"))
) %>%
  filter(gain_match_type == "already_in_gain") %>%
  mutate(reviewer_decision = "", reviewer_notes = "",
         .cr = coalesce(conf_rank[match_confidence], 0L)) %>%
  arrange(.cr, found_country) %>% select(-.cr, -gain_match_type)

today <- format(Sys.Date(), "%Y%m%d")
out <- sprintf("GAIN_QA_INGAIN_matches_%s.xlsx", today)
wb <- createWorkbook()
addWorksheet(wb, "in_gain_matches")
writeDataTable(wb, 1, qa)
setColWidths(wb, 1, cols = 1:ncol(qa), widths = c(16, 40, 34, 12, 40, 12, 40, 16, 18))
# highlight low-confidence rows for the eye
low_rows <- which(qa$match_confidence == "low") + 1
if (length(low_rows))
  addStyle(wb, 1, createStyle(fgFill = "#FDE0DC"), rows = low_rows, cols = 1:ncol(qa),
           gridExpand = TRUE, stack = TRUE)
freezePane(wb, 1, firstRow = TRUE)
saveWorkbook(wb, out, overwrite = TRUE)
message(sprintf("wrote %s  (%d matches; %d low-confidence highlighted)",
        out, nrow(qa), sum(qa$match_confidence == "low")))
