# Power Automate x GAIN outreach - flow specifications

Date: 2 Jul 2026. You build the flows in Power Automate; the R side is ALREADY
wired to work with them (tested). This doc is the contract between the two.

## Why this is worth building
Today the tracking loop (GAIN_OUTREACH_SYNC.R) scans your mailbox through
Outlook COM - which needs CLASSIC desktop Outlook, a logged-in session, and
regex-guessing inside bounce messages. With Power Automate the mailbox events
are captured in the cloud, the moment they happen, with no Outlook version
dependency - and the R pipeline just reads a workbook.

## The architecture (one picture)

```
  your mailbox (cloud)
     |  PA Flow 1: reply arrives    -> add row to pa_events.xlsx [Replies] + move to folder
     |  PA Flow 2: bounce arrives   -> add row to pa_events.xlsx [Bounces]
     |  PA Flow 3: sent scan (opt.) -> add row to pa_events.xlsx [Sent]
     |  PA Flow 4: batch send (opt.)-> reads outreach_emails_pa.xlsx, sends w/ approval,
     v                                 logs each send to [Sent]
  pa_events.xlsx  (in this R script folder, OneDrive-synced)
     |
     v
  GAIN_OUTREACH_SYNC.R  ->  GAIN_OUTREACH_LOG.csv  ->  Power BI / cycle tracker
```

**The R side is done and tested:** as soon as a file named `pa_events.xlsx`
exists in this folder, the sync script reads it INSTEAD of scanning Outlook
("PA MODE" - it says so when it runs). Delete/rename the file to fall back to
the COM scan. Verified end-to-end with synthetic events: sent -> status "sent",
reply -> "responded" with the snippet captured, bounces only match a contact's
own address (postmaster noise in the NDR preview cannot hurt anyone).

## Setup step 0 - the events workbook
Copy `pa_events_template.xlsx` (already generated, in this folder) to
`pa_events.xlsx` **when your first flow is connected and working**. It contains
the three named tables the flows write to (Power Automate's "Add a row into a
table" requires real Excel tables):

| Sheet / table | Columns |
|---|---|
| `Replies` | received_date, from_email, from_name, subject, snippet |
| `Bounces` | received_date, failed_email, subject, body_preview |
| `Sent`    | sent_date, to_email, subject |

Rules the flows must respect:
- **subject must carry the original subject line** - the R side extracts the
  country from `"... from <Country>"` at the end of it. RE:/Undeliverable:
  prefixes are fine.
- Dates as text `yyyy-MM-dd` (expression: `formatDateTime(utcNow(),'yyyy-MM-dd')`).
- Append-only; never edit or delete rows (R filters and dedupes on its side).

## Flow 1 - Reply capture  (BUILD THIS FIRST - highest value, lowest risk)
- **Trigger:** "When a new email arrives (V3)" - folder: Inbox
- **Condition:** Subject contains `GAIN survey: displacement statistics`
  (this also catches `RE:` / localized reply prefixes)
- **Actions:**
  1. Excel Online (Business) > "Add a row into a table" -> pa_events.xlsx, table `Replies`:
     - received_date: `formatDateTime(utcNow(),'yyyy-MM-dd')`
     - from_email: trigger `From`
     - from_name: trigger `Subject`-adjacent name field if available, else leave blank
     - subject: trigger `Subject`
     - snippet: trigger `Body preview` (plain text - do NOT use Body, it is HTML)
  2. "Move email (V2)" -> folder `GAIN Outreach` (create it once in Outlook)
  3. Optional: mobile/Teams notification "GAIN reply from <Subject>"

## Flow 2 - Bounce capture
- **Trigger:** same as Flow 1
- **Condition (all):** Subject starts with `Undeliverable` (add an OR-branch for
  `Delivery has failed` if your tenant produces those) AND Subject contains
  `GAIN survey: displacement statistics`
- **Action:** Add a row into table `Bounces`:
  - received_date: `formatDateTime(utcNow(),'yyyy-MM-dd')`
  - failed_email: leave BLANK (PA has no regex - R extracts candidate addresses
    from body_preview and only marks a contact bounced if their own address is
    among them)
  - subject: trigger `Subject`
  - body_preview: trigger `Body preview`
- Order note: if Flow 1 and 2 both fire on an NDR, add "does not start with
  Undeliverable" to Flow 1's condition.

## Flow 3 - Sent capture (optional)
Only needed if you send manually from Outlook (Flow 4 logs its own sends).
- **Trigger:** Recurrence, daily
- **Action:** "Get emails (V3)" - folder: Sent Items, search: `"GAIN survey: displacement statistics"`,
  received in the last 1 day -> Apply to each -> Add a row into `Sent`
  (sent_date, To, Subject).
Simplest alternative: skip this flow and keep marking sends via the Outlook
drafts workflow - the sync's COM branch or a manual status edit covers it.

## Flow 4 - Approved batch send (the "help reaching out" flow)
This replaces hand-sending 95 drafts. Handle with care - it actually emails NSOs.
- **Prepare in R first:** `source("GAIN_EXPORT_PA_EMAILS.R")` -> writes
  `outreach_emails_pa.xlsx` (table `Emails`). It EXCLUDES anyone already
  sent/bounced/responded per the log, so re-runs can't double-send. Set
  `EXPORT_TIERS <- "sure_bet"` inside it for a first wave.
- **Trigger:** "Manually trigger a flow" (never scheduled!)
- **Steps:**
  1. "List rows present in a table" -> outreach_emails_pa.xlsx, table `Emails`
  2. "Start and wait for an approval" -> "Send N GAIN outreach emails?" (you
     approve once per batch)
  3. If approved -> Apply to each row:
     - "Send an email (V2)": To = `to_email`, CC = `cc_email`,
       Subject = `subject`, Body = `body_html` (the column with <br> breaks,
       prepared by R - keep the editor in HTML mode)
     - Add a row into pa_events.xlsx `Sent` (sent_date, to_email, subject)
     - Delay: 30 seconds (politeness/throttle; avoids tenant send-rate limits)
- After the batch: run `GAIN_OUTREACH_SYNC.R` -> statuses flip to `sent`.

## Flow 5 - Weekly rhythm nudge (optional, 5 minutes to build)
Recurrence (Monday 09:00) -> send yourself a Teams/email reminder: "GAIN weekly
loop: run SYNC -> TRIAGE -> FOLLOWUP -> CYCLE_TRACKER". The R side of the loop
stays deliberately human-triggered.

## Rollout order
1. Flow 1 (replies) -> copy template to pa_events.xlsx -> run SYNC, confirm
   "PA MODE" appears and a test reply lands in the log.
2. Flow 2 (bounces).
3. Flow 4 (batch send) - only once SENDER_NAME + GAIN_SURVEY_LINK are real in
   GAIN_CONFIG.R and the v3 LLM re-run has refreshed the tiers.
4. Flows 3/5 if you feel the need.

## Safety rules (non-negotiable)
- Flow 4 is manual-trigger + approval-gated + throttled; everything else only
  READS the mailbox or moves messages.
- The workbook is append-only; the R log (GAIN_OUTREACH_LOG.csv) remains the
  single source of truth, and human edits in it always win.
- The subject line is the campaign's tracking key - never change its
  "GAIN survey: displacement statistics from <Country>" format without
  updating SUBJECT_KEY in GAIN_OUTREACH_SYNC.R and these flows together.
