# ==============================================================================
# GAIN - CREATE OUTLOOK DRAFTS FROM THE OUTREACH EMAILS
#
# Reads the newest outreach_emails_*.csv and creates one DRAFT per row in your
# Outlook (desktop). It NEVER sends - every message lands in your Drafts folder
# for you to review and send. Each draft is tagged with its tier as an Outlook
# colour Category (sure_bet / needs_review / low_bet) so you can sort them.
#
# Requirements: Windows + Outlook desktop installed and signed in.
# Run AFTER GAIN_OUTREACH_TARGETS.R. Safe to re-run (it will create duplicates,
# so clear old GAIN drafts first if you re-run).
# ==============================================================================

# Which tiers to turn into drafts. Edit this line, then source the file.
#   "sure_bet"      = strongest evidence (LLM-confirmed / category A / score >=70)
#   "needs_review"  = solid but check first (category B/C / score >=45)
#   "low_bet"       = weak; review carefully, you may not send these
DRAFT_TIERS <- "sure_bet,needs_review,low_bet"

# Which set of drafts to create: "emails" (initial outreach) or "followups" (reminders)
SOURCE <- "emails"

# ---- locate the newest source file -----------------------------------------
pat <- if (SOURCE == "followups") "outreach_followups_" else "outreach_emails_"
ef <- list.files(pattern = pat); ef <- ef[endsWith(ef, ".csv")]
if (length(ef) == 0) stop("No ", pat, "*.csv found - run the matching script first.")
csv <- normalizePath(ef[which.max(file.info(ef)$mtime)])
message("Drafts will be created from: ", basename(csv))

# how many will be created (preview)
em <- utils::read.csv(csv, stringsAsFactors = FALSE)
want <- strsplit(DRAFT_TIERS, ",")[[1]]
em <- em[em$tier %in% want & nzchar(em$to_email), ]

# Skip contacts whose log status shows we've already sent/heard from them -
# without this, re-running after some emails were already sent would silently
# regenerate drafts for those same contacts (real risk of double-emailing an
# NSO). Set SKIP_ALREADY_CONTACTED <- FALSE below to override.
SKIP_ALREADY_CONTACTED <- TRUE
if (SKIP_ALREADY_CONTACTED && file.exists("GAIN_OUTREACH_LOG.csv")) {
  lg <- utils::read.csv("GAIN_OUTREACH_LOG.csv", stringsAsFactors = FALSE)
  already <- lg$key[lg$status %in% c("sent", "bounced", "responded")]
  em_key <- paste(em$country, em$to_email, sep = "|")
  n_skip <- sum(em_key %in% already)
  if (n_skip > 0) {
    message(n_skip, " contact(s) already sent/bounced/responded per ",
            "GAIN_OUTREACH_LOG.csv - skipping (set SKIP_ALREADY_CONTACTED <- FALSE to override).")
    em <- em[!(em_key %in% already), ]
  }
}

n_to_make <- nrow(em)
message(sprintf("Tiers selected: %s -> %d drafts to create", DRAFT_TIERS, n_to_make))
if (n_to_make == 0) stop("Nothing to create for the selected tiers/filters.")

# write the FILTERED set to a temp CSV so PowerShell only sees what should be
# drafted - the original outreach_emails_*.csv on disk is left untouched for audit
csv <- file.path(tempdir(), "gain_drafts_filtered.csv")
utils::write.csv(em, csv, row.names = FALSE, fileEncoding = "UTF-8")

# ---- write a PowerShell script that drives Outlook COM, then run it --------
# (CSV path + tiers are embedded as literals so there are no arg-quoting issues)
ps_path <- file.path(tempdir(), "gain_make_drafts.ps1")
ps <- c(
  "$ErrorActionPreference = 'Stop'",
  paste0("$CsvPath = '", gsub("'", "''", csv), "'"),
  paste0("$Tiers   = '", DRAFT_TIERS, "'"),
  "try { $ol = New-Object -ComObject Outlook.Application }",
  "catch { Write-Output ('ERROR: could not start Outlook. Is the desktop app installed/open? ' + $_); exit 1 }",
  "$wanted = $Tiers.Split(',')",
  "$rows = Import-Csv -Path $CsvPath -Encoding UTF8",
  "$n = 0",
  "foreach ($r in $rows) {",
  "  if ($wanted -notcontains $r.tier) { continue }",
  "  if ([string]::IsNullOrWhiteSpace($r.to_email)) { continue }",
  "  $m = $ol.CreateItem(0)",                       # 0 = olMailItem
  "  $m.To = $r.to_email",
  "  if ($r.PSObject.Properties.Name -contains 'cc_email' -and -not [string]::IsNullOrWhiteSpace($r.cc_email)) { $m.CC = $r.cc_email }",
  "  $m.Subject = $r.subject",
  "  $m.Body = ($r.body -replace \"`n\", \"`r`n\")", # proper Windows line breaks
  "  if (-not [string]::IsNullOrWhiteSpace($r.tier)) { $m.Categories = $r.tier }",
  "  $m.Save()",                                     # save to Drafts (does NOT send)
  "  $n++",
  "}",
  "Write-Output ('Created ' + $n + ' Outlook drafts (tiers: ' + $Tiers + '). Review them in your Drafts folder.')"
)
writeLines(ps, ps_path)

out <- system2("powershell",
               c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", ps_path),
               stdout = TRUE, stderr = TRUE)
cat(out, sep = "\n")

if (any(grepl("^ERROR", out)))
  message("\nIf Outlook COM failed: make sure the Outlook DESKTOP app (not just web) is ",
          "installed and open, then re-run. New Outlook (store version) does not expose COM - ",
          "use classic Outlook, or fall back to mail-merge from the CSV.")
