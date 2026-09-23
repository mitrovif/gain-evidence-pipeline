# ==============================================================================
# GAIN WORKSTREAM 2 - REACH OUT  (contact NSOs about identified examples)
#
# SEPARATE from identification. It consumes the identification output
# (GAIN_EVIDENCE_FINAL_*.csv -> final_tier == "reach out"), connects each example
# to an NSO contact ADDRESS via the contact workbook, drafts emails (never sends),
# tracks replies, and produces the outreach dashboard tables.
#
# Prerequisite: run RUN_IDENTIFY.R first. Also needs the contact workbook found
# by CONTACT_XLSX in GAIN_CONFIG.R (fix that path for the current cycle's file).
#
# Windows/Outlook-only steps (Outlook sync, Outlook drafts) are skipped
# automatically on macOS/Linux. Dashboard tables -> powerbi_export/reachout/.
# ==============================================================================

is_windows <- Sys.info()[["sysname"]] == "Windows"

if (!length(list.files(pattern = "^GAIN_EVIDENCE_FINAL_.*\\.csv$")) &&
    !length(list.files(pattern = "^evidence_flagged_.*\\.csv$")))
  stop("No identification output found (GAIN_EVIDENCE_FINAL_* / evidence_flagged_*). Run RUN_IDENTIFY.R first.")

run_step <- function(label, file, win_only = FALSE) {
  if (win_only && !is_windows) {
    message("\n-- SKIP (Windows/Outlook only): ", label); return(invisible(NA)) }
  if (!file.exists(file)) { message("\n-- SKIP (missing ", file, "): ", label); return(invisible(NA)) }
  message("\n========================================================")
  message("STEP: ", label, "  ->  ", file)
  message("========================================================")
  t0 <- Sys.time()
  ok <- tryCatch({ source(file, local = new.env()); TRUE },
                 error = function(e) { message("  !! FAILED: ", conditionMessage(e)); FALSE })
  message(sprintf("  [%s] %s in %.1f min", if (ok) "done" else "FAILED",
                  label, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  ok
}

message("GAIN reach-out workstream")
run_step("Targets   -> connect examples to contacts + draft emails", "outreach/GAIN_OUTREACH_TARGETS.R")
run_step("Log       -> living campaign tracker (idempotent)",        "outreach/GAIN_OUTREACH_LOG.R")
run_step("Outlook sync -> Sent/Bounced/Responded into the log",      "outreach/GAIN_OUTREACH_SYNC.R", win_only = TRUE)
run_step("Follow-up -> reminder drafts for non-responders",          "outreach/GAIN_OUTREACH_FOLLOWUP.R")
run_step("Triage    -> AI-classify captured replies (LM Studio)",    "outreach/GAIN_OUTREACH_TRIAGE.R")
run_step("PA export -> outreach_emails_pa.xlsx (Power Automate)",     "outreach/GAIN_EXPORT_PA_EMAILS.R")
run_step("Outlook drafts -> create drafts in Outlook (review, send)", "outreach/GAIN_CREATE_OUTLOOK_DRAFTS.R", win_only = TRUE)
run_step("Cycle tracker -> found->contacted->replied spine",         "outreach/GAIN_CYCLE_TRACKER.R")

local({
  sub <- file.path("powerbi_export", "reachout")
  dir.create(sub, recursive = TRUE, showWarnings = FALSE)
  tabs <- c("WEB_GAIN_outreach.csv", "WEB_GAIN_outreach_log.csv", "WEB_GAIN_cycle_tracker.csv")
  for (t in tabs) { s <- file.path("powerbi_export", t)
    if (file.exists(s)) file.copy(s, file.path(sub, t), overwrite = TRUE) }
  message("\nreach-out dashboard tables -> ", sub, "/")
})
message("\nREACH-OUT WORKSTREAM DONE")
