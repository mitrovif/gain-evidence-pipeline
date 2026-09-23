# ==============================================================================
# GAIN OUTREACH SYNC  (Outlook -> the outreach log)
#
# Reads your Outlook and updates GAIN_OUTREACH_LOG.csv automatically:
#   * SENT      - emails in Sent Items with our subject  -> status = sent
#   * BOUNCED   - non-delivery reports (NDRs) in the Inbox -> status = bounced
#                 (+ appended to GAIN_bounced_master.csv so future runs exclude them)
#   * RESPONDED - replies in the "GAIN Outreach" folder    -> status = responded
#
# Matching is by the COUNTRY in the subject line ("...from <Country>"), which both
# sent items, NDRs ("Undeliverable: ...from <Country>") and replies ("RE: ...from
# <Country>") preserve - so we never have to resolve email addresses to match.
#
# IDEMPOTENT: only advances the lifecycle (draft_ready -> sent -> responded;
# bounced overrides) and fills blank date/summary cells. Your manual comments are
# never touched. Safe to run on a schedule (e.g. weekly).
#
# Requirements: Windows + CLASSIC Outlook desktop (the new store Outlook has no COM).
# One-time setup: create an Outlook folder named "GAIN Outreach" and a rule moving
# messages whose subject contains "GAIN survey: displacement statistics" into it.
# ==============================================================================

suppressMessages({ library(tidyverse) })

DAYS_BACK    <- 120                                   # how far back to scan
SUBJECT_KEY  <- "GAIN survey: displacement statistics"
REPLY_FOLDER <- "GAIN Outreach"                       # Outlook folder replies are routed to
LOG          <- "GAIN_OUTREACH_LOG.csv"
BOUNCE_MASTER<- "GAIN_bounced_master.csv"
today <- as.character(Sys.Date())

if (!file.exists(LOG)) stop("No ", LOG, " - run GAIN_OUTREACH_LOG.R first.")

# ---- PA MODE: Power Automate event workbook takes precedence over COM --------
# If pa_events.xlsx exists (see POWER_AUTOMATE_OUTREACH_SPEC.md), your Power
# Automate flows are feeding Replies/Bounces/Sent rows into it and we read
# THAT instead of scanning Outlook via COM. Benefits: works with NEW Outlook
# (no COM), works headless, and bounce recipients come from the flow rather
# than regex-guessing inside NDR bodies. Delete/rename the workbook to fall
# back to the COM scan.
PA_EVENTS <- "pa_events.xlsx"
PA_MODE   <- file.exists(PA_EVENTS)
if (PA_MODE) message("PA MODE: reading ", PA_EVENTS, " (Power Automate events) - Outlook COM skipped")

# ---- 1. pull sent / bounced / replied from Outlook via PowerShell COM --------
if (!PA_MODE) {
tmp <- tempdir()
sent_csv <- file.path(tmp, "gain_sent.csv")
bnc_csv  <- file.path(tmp, "gain_bounces.csv")
rep_csv  <- file.path(tmp, "gain_replies.csv")
unlink(c(sent_csv, bnc_csv, rep_csv))
litq <- function(x) gsub("'", "''", x)   # escape for a single-quoted PS literal

ps <- c(
  "$ErrorActionPreference='Stop'",
  paste0("$SubjectKey='", litq(SUBJECT_KEY), "'"),
  paste0("$DaysBack=", DAYS_BACK),
  paste0("$ReplyFolder='", litq(REPLY_FOLDER), "'"),
  paste0("$sentPath='", litq(normalizePath(sent_csv, mustWork = FALSE)), "'"),
  paste0("$bncPath='",  litq(normalizePath(bnc_csv,  mustWork = FALSE)), "'"),
  paste0("$repPath='",  litq(normalizePath(rep_csv,  mustWork = FALSE)), "'"),
  "try { $ol=New-Object -ComObject Outlook.Application; $ns=$ol.GetNamespace('MAPI') }",
  "catch { Write-Output ('ERROR: Outlook COM failed - '+$_); exit 1 }",
  "$since=(Get-Date).AddDays(-$DaysBack)",
  "$dfilt=\"[ReceivedTime] >= '\"+$since.ToString('MM/dd/yyyy')+\"'\"",
  "$sfilt=\"[SentOn] >= '\"+$since.ToString('MM/dd/yyyy')+\"'\"",
  "function CountryOf($s){ if($s -match 'from (.+?)\\s*$'){ return $matches[1].Trim() } return '' }",
  # SENT
  "$sent=@()",
  "try { $sf=$ns.GetDefaultFolder(5); $items=$sf.Items.Restrict($sfilt)",
  "  foreach($it in $items){ try{ if($it.Subject -like ('*'+$SubjectKey+'*')){",
  "    $sent+=[pscustomobject]@{country=(CountryOf $it.Subject); date=$it.SentOn.ToString('yyyy-MM-dd')} } }catch{} } }catch{}",
  "$sent | Export-Csv -Path $sentPath -NoTypeInformation -Encoding UTF8",
  # BOUNCES (NDRs in inbox)
  "$bnc=@()",
  "try { $inb=$ns.GetDefaultFolder(6); $items=$inb.Items.Restrict($dfilt)",
  "  foreach($it in $items){ try{",
  "    $isNDR=($it.MessageClass -like 'REPORT*NDR*') -or ($it.Subject -like 'Undeliverable*') -or ($it.Subject -like 'Delivery*fail*')",
  "    if($isNDR -and ($it.Subject -like ('*'+$SubjectKey+'*'))){",
  # NDR bodies often contain SEVERAL email-shaped strings (postmaster,
  # mailer-daemon, the sender's own quoted address in boilerplate text) -
  # '-match' only returns the FIRST one, which is frequently the WRONG address
  # and would wrongly flag a still-valid contact as bounced. Capture every
  # candidate instead; the R side only marks bounced when a contact's own
  # email is actually among them (never assumes "first = correct").
  "      $ms=[regex]::Matches($it.Body,'[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}')",
  "      $ems=($ms | ForEach-Object { $_.Value } | Select-Object -Unique) -join ';'",
  "      $bnc+=[pscustomobject]@{country=(CountryOf $it.Subject); candidate_emails=$ems; date=$it.ReceivedTime.ToString('yyyy-MM-dd')} } }catch{} } }catch{}",
  "$bnc | Export-Csv -Path $bncPath -NoTypeInformation -Encoding UTF8",
  # REPLIES (in the GAIN Outreach folder if it exists, else Inbox)
  "$rep=@()",
  "try { $inb=$ns.GetDefaultFolder(6); $folder=$inb",
  "  try{ $f=$inb.Folders.Item($ReplyFolder); if($f){ $folder=$f } }catch{}",
  "  $items=$folder.Items.Restrict($dfilt)",
  "  foreach($it in $items){ try{",
  "    if(($it.Subject -like ('*'+$SubjectKey+'*')) -and ($it.Subject -notlike 'Undeliverable*')){",
  "      $from=''; try{ $from=$it.SenderEmailAddress }catch{}",
  "      $snip=''; try{ $snip=($it.Body.Substring(0,[Math]::Min(200,$it.Body.Length)) -replace '\\s+',' ') }catch{}",
  "      $rep+=[pscustomobject]@{country=(CountryOf $it.Subject); from_email=$from; date=$it.ReceivedTime.ToString('yyyy-MM-dd'); snippet=$snip} } }catch{} } }catch{}",
  "$rep | Export-Csv -Path $repPath -NoTypeInformation -Encoding UTF8",
  "Write-Output ('OK sent='+$sent.Count+' bounces='+$bnc.Count+' replies='+$rep.Count)"
)
ps_path <- file.path(tmp, "gain_outlook_scan.ps1")
writeLines(ps, ps_path)
out <- system2("powershell", c("-NoProfile","-ExecutionPolicy","Bypass","-File", ps_path),
               stdout = TRUE, stderr = TRUE)
cat(out, sep = "\n")
if (any(grepl("^ERROR", out))) {
  message("\nOutlook could not be read. Use CLASSIC Outlook desktop (open + signed in). ",
          "You can still update statuses manually in ", LOG, ".")
}
}  # end of the COM branch (skipped entirely in PA MODE)

# ---- 2. gather the events (PA workbook if present, else the COM CSVs) --------
read_safe <- function(f) if (file.exists(f) && file.info(f)$size > 3)
  suppressWarnings(read_csv(f, show_col_types = FALSE)) else tibble()

if (PA_MODE) {
  suppressMessages(library(readxl))
  country_of <- function(s) {   # same rule as the PS CountryOf: country = tail of the subject
    m <- str_match(coalesce(as.character(s), ""), "from (.+?)\\s*$")[, 2]
    str_squish(coalesce(m, ""))
  }
  col_of <- function(d, c) if (c %in% names(d)) as.character(d[[c]]) else rep("", nrow(d))
  pa_sheet <- function(name) {
    d <- tryCatch(read_excel(PA_EVENTS, sheet = name), error = function(e) tibble())
    # defensive: only rows carrying our campaign subject key (flows should
    # already filter, but a mis-configured flow must not corrupt the log)
    if (nrow(d) && "subject" %in% names(d))
      d[str_detect(coalesce(as.character(d$subject), ""), fixed(SUBJECT_KEY)), ]
    else d[0, ]
  }
  s0 <- pa_sheet("Sent"); b0 <- pa_sheet("Bounces"); r0 <- pa_sheet("Replies")
  sent <- if (nrow(s0)) tibble(country = country_of(s0$subject),
                               date = col_of(s0, "sent_date")) else tibble()
  # Bounce candidates: use failed_email if the flow filled it; otherwise pull
  # every email-shaped string out of the NDR body preview (Power Automate has
  # no regex, so that extraction lives HERE). Downstream matching only marks a
  # contact bounced when the contact's OWN address is among the candidates -
  # postmaster/mailer-daemon noise in the preview can never hurt anyone.
  bncs <- if (nrow(b0)) {
    fe <- col_of(b0, "failed_email")
    bp <- col_of(b0, "body_preview")
    cand <- vapply(seq_len(nrow(b0)), function(i) {
      if (nzchar(fe[i])) return(fe[i])
      paste(unlist(str_extract_all(bp[i],
        "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}")), collapse = ";")
    }, character(1))
    tibble(country = country_of(b0$subject), candidate_emails = cand,
           date = col_of(b0, "received_date"))
  } else tibble()
  reps <- if (nrow(r0)) tibble(country = country_of(r0$subject),
                               from_email = col_of(r0, "from_email"),
                               date = col_of(r0, "received_date"),
                               snippet = col_of(r0, "snippet")) else tibble()
  message(sprintf("PA events (subject-key filtered): %d sent | %d bounces | %d replies",
                  nrow(sent), nrow(bncs), nrow(reps)))
} else {
  sent <- read_safe(sent_csv); bncs <- read_safe(bnc_csv); reps <- read_safe(rep_csv)
}

log <- read_csv(LOG, show_col_types = FALSE) %>% mutate(across(everything(), as.character))
norm <- function(x) str_squish(str_to_lower(coalesce(as.character(x), "")))
blank <- function(x) is.na(x) | x == ""

n_sent <- n_bnc <- n_rep <- 0
bounced_matched_emails <- character(0)
# SENT -> status sent (only from draft_ready)
if (nrow(sent) && "country" %in% names(sent)) {
  hit <- norm(log$country) %in% norm(sent$country) & log$status %in% c("draft_ready", "")
  log$date_sent[hit & blank(log$date_sent)] <- today
  log$status[hit] <- "sent"; n_sent <- sum(hit)
}
# BOUNCED -> only when a contact's OWN email is among the candidate addresses
# found in that NDR (an NDR body often contains several email-shaped strings -
# postmaster, mailer-daemon, the sender's own quoted address - and taking
# "the first one found" would wrongly bounce a perfectly good contact).
if (nrow(bncs) && "candidate_emails" %in% names(bncs)) {
  cand_sets <- map(coalesce(bncs$candidate_emails, ""), ~ norm(str_split(.x, ";")[[1]]))
  hit <- map_lgl(seq_len(nrow(log)), function(i) {
    e <- norm(log$to_email[i])
    nzchar(e) && any(map_lgl(cand_sets, ~ e %in% .x))
  })
  log$bounced[hit] <- "TRUE"; log$status[hit] <- "bounced"
  log$date_bounced[hit & blank(log$date_bounced)] <- today; n_bnc <- sum(hit)
  # only the ACTUAL matched contact emails go to the master exclusion list -
  # never the raw NDR candidate_emails (which can include postmaster/
  # mailer-daemon addresses that must never be treated as "a contact bounced")
  bounced_matched_emails <- unique(log$to_email[hit])
}
# RESPONDED -> by country (not those already bounced), capture snippet
if (nrow(reps) && "country" %in% names(reps)) {
  for (i in seq_len(nrow(reps))) {
    m <- norm(log$country) == norm(reps$country[i]) & log$status != "bounced"
    if (!any(m)) next
    log$responded[m] <- "TRUE"
    log$status[m] <- "responded"
    log$date_responded[m & blank(log$date_responded)] <- today
    sn <- substr(coalesce(reps$snippet[i], ""), 1, 200)
    log$response_summary[m & blank(log$response_summary)] <- sn
    n_rep <- n_rep + sum(m)
  }
}

write_excel_csv(log, LOG)
if (dir.exists("powerbi_export"))
  write_excel_csv(log, file.path("powerbi_export", "WEB_GAIN_outreach_log.csv"))

# master bounce list (future GAIN_OUTREACH_TARGETS runs can exclude these)
if (length(bounced_matched_emails) > 0) {
  newb <- tibble(email = bounced_matched_emails, country = NA_character_, date_logged = today)
  master <- if (file.exists(BOUNCE_MASTER)) read_csv(BOUNCE_MASTER, show_col_types = FALSE) else tibble()
  master <- bind_rows(master, newb) %>% distinct(email, .keep_all = TRUE)
  write_excel_csv(master, BOUNCE_MASTER)
}

message("\n==================== OUTREACH SYNC ====================")
message(sprintf("Log rows updated -> sent: %d | bounced: %d | responded: %d", n_sent, n_bnc, n_rep))
message("by status now: ", paste(names(table(log$status)), table(log$status), sep = "=", collapse = " | "))
message("Log: ", LOG, "  |  Power BI: powerbi_export/WEB_GAIN_outreach_log.csv")
if (file.exists(BOUNCE_MASTER)) message("Bounce master: ", BOUNCE_MASTER, " (add to GAIN_OUTREACH_TARGETS exclusions)")
