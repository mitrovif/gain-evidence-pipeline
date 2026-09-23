# ==============================================================================
# GAIN EXPORT PA EMAILS  (feed the Power Automate batch-send flow)
#
# Converts the newest outreach_emails_*.csv into outreach_emails_pa.xlsx with a
# named table ("Emails") that Power Automate's "List rows present in a table"
# can read. See POWER_AUTOMATE_OUTREACH_SPEC.md, Flow 4.
#
# Safety, same rules as the Outlook drafts script:
#   * contacts whose log status is already sent / bounced / responded are
#     EXCLUDED, so a re-run of the flow can never double-email an NSO
#   * choose which tiers to export below (start with sure_bet)
#   * body_html has line breaks converted for the flow's Send-an-email action
# ==============================================================================

suppressMessages({ library(tidyverse); library(openxlsx) })

EXPORT_TIERS <- c("sure_bet", "needs_review", "low_bet")   # trim to e.g. "sure_bet" for a first wave

ef <- list.files(pattern = "outreach_emails_"); ef <- ef[endsWith(ef, ".csv")]
if (length(ef) == 0) stop("No outreach_emails_*.csv - run GAIN_OUTREACH_TARGETS.R first.")
src <- ef[which.max(file.info(ef)$mtime)]
em <- read_csv(src, show_col_types = FALSE) %>%
  filter(tier %in% EXPORT_TIERS, nzchar(coalesce(to_email, "")))

if (file.exists("GAIN_OUTREACH_LOG.csv")) {
  lg <- read_csv("GAIN_OUTREACH_LOG.csv", show_col_types = FALSE)
  done <- lg$country[lg$status %in% c("sent", "bounced", "responded")]
  n0 <- nrow(em); em <- em %>% filter(!country %in% done)
  if (n0 > nrow(em)) message(n0 - nrow(em), " contact(s) already sent/bounced/responded - excluded")
}
if (nrow(em) == 0) stop("Nothing to export for the selected tiers/filters.")

out <- em %>% transmute(
  tier, language, country, to_name, to_email,
  cc_email = coalesce(cc_email, ""), subject,
  body,                                              # plain text (audit / drafts)
  body_html = str_replace_all(body, "\n", "<br>"))   # what the flow's Send action uses

wb <- createWorkbook()
addWorksheet(wb, "Emails")
writeDataTable(wb, "Emails", as.data.frame(out), tableName = "Emails")
saveWorkbook(wb, "outreach_emails_pa.xlsx", overwrite = TRUE)
message("Wrote outreach_emails_pa.xlsx (table 'Emails'): ", nrow(out),
        " emails from ", src)
message("by tier: ", paste(names(table(out$tier)), table(out$tier), sep = "=", collapse = " | "))
message("\nPoint the Power Automate batch-send flow at this file (Flow 4 in the spec).")
