# ==============================================================================
# GAIN OUTREACH LOG  (the living campaign tracker)
#
# Builds/updates ONE persistent file - GAIN_OUTREACH_LOG.csv - that records every
# contact we plan to email and how the outreach is going. Power BI reads it.
#
# IDEMPOTENT: re-running seeds NEW rows (status = draft_ready) but PRESERVES every
# human-edited field (status, dates, bounced, responded, comments...) on rows that
# already exist. So you can edit the log freely and re-run without losing notes.
#
# Status lifecycle (you update these as you go, or the Outlook-sync script does):
#   draft_ready -> sent -> (bounced | responded) -> (submitted | declined | no_response)
# ==============================================================================

suppressMessages({ library(tidyverse) })
LOG <- "GAIN_OUTREACH_LOG.csv"
TRACK_COLS <- c("status","date_sent","date_bounced","date_responded",
                "bounced","responded","outcome","response_summary","comments",
                "follow_up_date","owner")   # human/sync-owned fields (preserved)

newest <- function(pat) { f <- list.files(pattern = pat); f <- f[endsWith(f, ".csv")]
  if (length(f)) f[which.max(file.info(f)$mtime)] else NA }

# ---- current campaign (from the emails we generated) -------------------------
em_f <- newest("outreach_emails_")
if (is.na(em_f)) stop("No outreach_emails_*.csv - run GAIN_OUTREACH_TARGETS.R first.")
em <- read_csv(em_f, show_col_types = FALSE)

# attach GAIN respondent status (ACTIVE/LAPSED/NEVER) per country, if available
ev_f <- newest("evidence_flagged_")
resp <- tibble(country = character(), gain_respondent_status = character())
if (!is.na(ev_f)) {
  ev <- read_csv(ev_f, show_col_types = FALSE)
  rcol <- intersect(c("gain_respondent_status","respondent_status"), names(ev))[1]
  ccol <- intersect(c("Country","country"), names(ev))[1]
  if (!is.na(rcol) && !is.na(ccol))
    resp <- ev %>% transmute(country = .data[[ccol]], gain_respondent_status = .data[[rcol]]) %>%
      filter(!is.na(gain_respondent_status)) %>% distinct(country, .keep_all = TRUE)
}

campaign <- em %>%
  left_join(resp, by = "country") %>%
  transmute(key = paste(country, to_email, sep = "|"),
            country, organization, tier, n_examples,
            to_name, to_email, cc_email,
            gain_respondent_status = coalesce(gain_respondent_status, "unknown"),
            subject)

# ---- merge with the existing log (preserve human-tracked fields) -------------
# Preserved by COUNTRY, not by the stored key: the log holds exactly one row per
# country (the campaign groups per-country), and the country name is the one
# identifier that survives a contact reassignment. Joining on key (country|email)
# silently orphaned all human notes whenever the contact changed - including the
# supported workflow of a human editing to_email in the log to reassign, since
# nobody updates the stored key column by hand. The key column is still written
# (recomputed fresh each run) for the drafts script's already-sent check.
if (file.exists(LOG)) {
  old <- read_csv(LOG, show_col_types = FALSE) %>%
    mutate(across(everything(), as.character)) %>%
    distinct(country, .keep_all = TRUE)
  keep <- old %>% select(any_of(c("country", TRACK_COLS)))
  out <- campaign %>% left_join(keep, by = "country")
  message(sprintf("Updating existing log: %d countries kept their notes, %d new",
                  sum(campaign$country %in% old$country),
                  sum(!campaign$country %in% old$country)))
} else {
  out <- campaign
  message("Creating new log: ", nrow(out), " contacts")
}

# defaults for new / empty tracking cells
def <- function(x, d) ifelse(is.na(x) | x == "", d, x)
for (c in TRACK_COLS) if (!c %in% names(out)) out[[c]] <- NA_character_
out <- out %>% mutate(
  status         = def(status, "draft_ready"),
  bounced        = def(bounced, "FALSE"),
  responded      = def(responded, "FALSE"),
  outcome        = def(outcome, ""),
  response_summary = coalesce(response_summary, ""),
  comments       = coalesce(comments, ""),
  owner          = coalesce(owner, ""),
  date_sent = coalesce(date_sent, ""), date_bounced = coalesce(date_bounced, ""),
  date_responded = coalesce(date_responded, ""), follow_up_date = coalesce(follow_up_date, "")
) %>%
  select(key, country, organization, gain_respondent_status, tier, n_examples,
         to_name, to_email, cc_email, all_of(TRACK_COLS), subject) %>%
  arrange(factor(tier, levels = c("sure_bet","needs_review","low_bet")), country)

write_excel_csv(out, LOG)
if (dir.exists("powerbi_export"))
  write_excel_csv(out, file.path("powerbi_export", "WEB_GAIN_outreach_log.csv"))

message("\n==================== OUTREACH LOG ====================")
message("Contacts in log: ", nrow(out))
message("by status: ", paste(names(table(out$status)), table(out$status), sep="=", collapse=" | "))
message("not yet sent (draft_ready): ", sum(out$status == "draft_ready"))
message("\nFile: ", LOG, "  (edit status/comments freely; re-run anytime, notes are kept)")
message("Power BI: powerbi_export/WEB_GAIN_outreach_log.csv")
