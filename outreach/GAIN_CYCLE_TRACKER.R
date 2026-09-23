# ==============================================================================
# GAIN CYCLE TRACKER  (stages 1-5: identify -> qualify -> contact -> reach -> track)
#
# Joins the EVIDENCE side (what we found that should be in GAIN) to the OUTREACH
# side (who we have, and how the contact is going) into ONE spine - so every
# identified example can be followed from "found online" to "they replied".
# One row per identified example. Power BI reads WEB_GAIN_cycle_tracker.csv.
#
# Stages 6-9 (hold-for-launch / launch wave / submission / conversion) are left as
# blank placeholder columns to be filled later.
#
# Pure add-on: reads outreach_targets_*.csv (+ GAIN_OUTREACH_LOG.csv). Nothing
# upstream changes. Re-run any time.
# ==============================================================================

suppressMessages({ library(tidyverse) })
stamp <- format(Sys.Date(), "%Y%m%d")
newest <- function(pat) { f <- list.files(pattern = pat); f <- f[endsWith(f, ".csv")]
  if (length(f)) f[which.max(file.info(f)$mtime)] else NA }

tf <- newest("outreach_targets_")
if (is.na(tf)) stop("No outreach_targets_*.csv - run GAIN_OUTREACH_TARGETS.R first.")
tg <- read_csv(tf, show_col_types = FALSE)

# outreach status per country (stage 4-5), from the living log if present
log_status <- tibble(country = character())
if (file.exists("GAIN_OUTREACH_LOG.csv")) {
  lg <- read_csv("GAIN_OUTREACH_LOG.csv", show_col_types = FALSE) %>% mutate(across(everything(), as.character))
  log_status <- lg %>% group_by(country) %>%
    summarise(outreach_status = first(status), outcome = first(outcome),
              responded = first(responded), bounced = first(bounced),
              date_sent = first(date_sent), date_responded = first(date_responded),
              response_summary = first(response_summary), tier = first(tier),
              gain_respondent_status = first(gain_respondent_status), .groups = "drop")
}

trk <- tg %>%
  left_join(log_status, by = "country") %>%
  mutate(
    # GAIN side (stages 1-2): identified + whether it should be in GAIN
    should_be_in_gain = match_type %in% c("new_country", "new_example_existing_country"),
    new_country       = match_type == "new_country",
    # outreach side (stages 3-5)
    has_contact       = coalesce(has_contact, FALSE),
    outreach_status   = coalesce(outreach_status, if_else(has_contact, "not_started", "no_contact")),
    responded_flag    = coalesce(responded, "FALSE") %in% c("TRUE","true"),
    bounced_flag      = coalesce(bounced, "FALSE")  %in% c("TRUE","true"),
    # the furthest stage this example has reached (1-5)
    stage = case_when(
      responded_flag                               ~ "5_responded",
      bounced_flag                                 ~ "4_bounced",
      outreach_status == "sent"                    ~ "4_contacted",
      outreach_status %in% c("draft_ready")        ~ "3_contact_ready",
      has_contact                                  ~ "3_contact_ready",
      TRUE                                         ~ "2_no_contact"),
    # placeholders for stages 6-9 (to be filled later)
    pending_launch = NA, invited_at_launch = NA, submitted = NA, submitted_date = NA
  ) %>%
  transmute(
    country, example_title, year, url, populations, relevance_score,
    is_strong, match_type, should_be_in_gain, new_country, evidence_quote,
    has_contact, contact_name, contact_position, contact_org, contact_email,
    tier, outreach_status, outcome, responded = responded_flag, bounced = bounced_flag,
    date_sent, date_responded, response_summary, gain_respondent_status,
    stage, pending_launch, invited_at_launch, submitted, submitted_date) %>%
  arrange(stage, country, desc(relevance_score))

write_excel_csv(trk, paste0("GAIN_CYCLE_TRACKER_", stamp, ".csv"))
write_excel_csv(trk, "GAIN_CYCLE_TRACKER.csv")                       # stable name
if (dir.exists("powerbi_export"))
  write_excel_csv(trk, file.path("powerbi_export", "WEB_GAIN_cycle_tracker.csv"))

# ---- the 1-5 funnel (by example, and by country) ----------------------------
order5 <- c("2_no_contact","3_contact_ready","4_contacted","4_bounced","5_responded")
ex_funnel <- trk %>% count(stage) %>% arrange(factor(stage, levels = order5))
ct_funnel <- trk %>% distinct(country, stage) %>% count(stage) %>% arrange(factor(stage, levels = order5))

message("\n==================== CYCLE TRACKER (stages 1-5) ====================")
message("Identified examples that should be in GAIN: ", nrow(trk),
        " across ", n_distinct(trk$country), " countries")
message("\nFunnel by EXAMPLE:")
print(as.data.frame(ex_funnel), row.names = FALSE)
message("\nFunnel by COUNTRY (furthest stage reached):")
print(as.data.frame(ct_funnel), row.names = FALSE)
message("\nKey drop-offs to action:")
message("  examples with NO contact (stage 3 gap): ", sum(trk$stage == "2_no_contact"),
        "  -> see outreach_contact_gaps_*.csv")
message("  have a contact but not yet contacted: ", sum(trk$stage == "3_contact_ready"))
message("\nOutputs: GAIN_CYCLE_TRACKER.csv (+ dated) | powerbi_export/WEB_GAIN_cycle_tracker.csv")
