# ==============================================================================
# GAIN OUTREACH FOLLOW-UP  (reminder drafts for non-responders)
#
# Reads GAIN_OUTREACH_LOG.csv and builds a short reminder for every contact that
# was SENT more than FOLLOWUP_DAYS ago and has NOT responded or bounced. Writes
# outreach_followups_[date].csv in the same shape as outreach_emails, so you can
# create the drafts with GAIN_CREATE_OUTLOOK_DRAFTS.R (set SOURCE <- "followups").
#
# Drafts only - never sends. Most replies come from the second touch.
# ==============================================================================

suppressMessages({ library(tidyverse) })
stamp <- format(Sys.Date(), "%Y%m%d")

source("shared/GAIN_CONFIG.R")    # SENDER_NAME, SENDER_TITLE, GAIN_SURVEY_LINK - one shared place
FOLLOWUP_DAYS <- 14        # remind contacts sent at least this many days ago

LOG <- "GAIN_OUTREACH_LOG.csv"
if (!file.exists(LOG)) stop("No ", LOG, " - run GAIN_OUTREACH_LOG.R first.")
log <- read_csv(LOG, show_col_types = FALSE) %>% mutate(across(everything(), as.character))

cutoff <- Sys.Date() - FOLLOWUP_DAYS
due <- log %>%
  mutate(ds = suppressWarnings(as.Date(date_sent))) %>%
  filter(status == "sent",
         !(coalesce(responded, "") %in% c("TRUE", "true")),
         !(coalesce(bounced, "")  %in% c("TRUE", "true")),
         !is.na(ds), ds <= cutoff,
         coalesce(follow_up_date, "") == "" | suppressWarnings(as.Date(follow_up_date)) <= Sys.Date())

if (nrow(due) == 0) {
  message("No contacts are due for a follow-up (sent >= ", FOLLOWUP_DAYS, " days ago, no reply).")
} else {
  reminder <- function(first, country, date_sent) paste0(
    "Dear ", coalesce(first, "colleague"), ",\n\n",
    "I am following up on my email of ", date_sent, " regarding displacement statistics ",
    "from ", country, " that may be relevant for the GAIN survey. We would be grateful for ",
    "your thoughts whenever convenient, and we are happy to share the details again.\n\n",
    "GAIN survey: ", GAIN_SURVEY_LINK, "\n\n",
    "Thank you,\n", SENDER_NAME, "\n", SENDER_TITLE)

  followups <- due %>%
    transmute(
      tier = coalesce(tier, "needs_review"), language = "en", country,
      to_name, to_email, cc_email = coalesce(cc_email, ""),
      organization = coalesce(organization, ""), n_examples = coalesce(n_examples, ""),
      subject = paste0("RE: GAIN survey: displacement statistics from ", country),
      body = pmap_chr(list(str_split_fixed(to_name, " ", 2)[, 1], country, date_sent), reminder))

  write_excel_csv(followups, paste0("outreach_followups_", stamp, ".csv"))
  message("Follow-up reminders written: ", nrow(followups),
          "  ->  outreach_followups_", stamp, ".csv")
  message("by tier: ", paste(names(table(followups$tier)), table(followups$tier), sep = "=", collapse = " | "))
  message("\nTo create the reminder drafts: set SOURCE <- \"followups\" in ",
          "GAIN_CREATE_OUTLOOK_DRAFTS.R and run it.")
}
