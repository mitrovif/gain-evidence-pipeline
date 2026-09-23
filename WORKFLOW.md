# GAIN Web Scraping - how to run everything (the one page to read)

Updated: 23 Sep 2026 (code split into shared/ identify/ outreach/ sdg/ lake/; data stays in this folder). All documentation lives in `docs/`; superseded outputs in
`archive/`; this file is the map.

## Before anything
1. Open **GAIN_WebScraping.Rproj** (never run from Downloads - that caused two
   real incidents).
2. Set your name + the survey link once, in **shared/GAIN_CONFIG.R**.

## A. The DATA pipeline (find + score evidence)
Full refresh (scrape + LLM, hours):
```r
# in RUN_IDENTIFY.R set DO_SCRAPE / DO_LLM, then:
source("RUN_IDENTIFY.R")
```
Targeted re-runs (much more common):
```r
source("identify/GAIN_SEMANTIC_FUNNEL.R")    # LLM re-extract (Ollama up; cached)
source("identify/GAIN_PHASE5_CROSSREF.R")    # GAIN match + evidence_flagged
source("identify/GAIN_POWERBI_EXPORT.R")     # refresh the Power BI pack
```
Diagnostics: `source("identify/GAIN_COVERAGE_GAPS.R")` - which countries are dark and
why (dead domains, no sitemap, nothing matched), with suggested actions.

## B. The OUTREACH loop (contact NSOs about new examples)
Initial build:
```r
source("outreach/GAIN_OUTREACH_TARGETS.R")   # tiered bilingual drafts (sticky contacts)
source("outreach/GAIN_OUTREACH_LOG.R")       # the living tracker (notes preserved by country)
source("outreach/GAIN_CYCLE_TRACKER.R")      # identify->contact->reply funnel
source("outreach/GAIN_CREATE_OUTLOOK_DRAFTS.R")  # drafts in Outlook (never sends)
# or for Power Automate sending: source("outreach/GAIN_EXPORT_PA_EMAILS.R")
```
Weekly, after emails are out:
```r
source("outreach/GAIN_OUTREACH_SYNC.R")      # sent/bounced/replied (PA workbook or Outlook COM)
source("outreach/GAIN_OUTREACH_TRIAGE.R")    # LLM sorts replies (Ollama up)
source("outreach/GAIN_OUTREACH_FOLLOWUP.R")  # reminders for non-responders
source("outreach/GAIN_CYCLE_TRACKER.R")      # refresh the funnel
```
Power Automate flows: see `docs/POWER_AUTOMATE_OUTREACH_SPEC.md` (the R side is
already wired - drop `pa_events.xlsx` in this folder and SYNC switches to it).

## C. Housekeeping
```r
source("shared/GAIN_TIDY_FOLDER.R")                                      # preview
Sys.setenv(GAIN_TIDY_APPLY = "1"); source("shared/GAIN_TIDY_FOLDER.R")   # archive old outputs
```

## Shared building blocks (never copy - source these)
| File | Holds |
|---|---|
| shared/GAIN_CONFIG.R | sender, survey link, deadline, contact-workbook path |
| shared/GAIN_COMMON.R | harmonize_country(), newest_file(), safe_write() |
| shared/GAIN_OLLAMA_HELPERS.R | all local-LLM calls (extract/embed/adjudicate, caching, logging) |

## Where things are
| Place | Contents |
|---|---|
| `docs/` | READMEs, data dictionary (md+docx), Power BI guide, PA spec, review notes |
| `archive/` | superseded dated outputs (moved, never deleted) |
| `powerbi_export/` | the pack Power BI is connected to - do not hand-edit |
| caches (`evidence_cache/` `ollama_cache/` `l3_cache/` ...) | resumability - safe to leave alone |

## Cautious framing (always)
Everything found is a **possible candidate requiring manual review and NSO
confirmation** - never a confirmed GAIN example. EGRISS frameworks are
**recommendations** (IRRS / IRIS / IROSS), never "standards".
