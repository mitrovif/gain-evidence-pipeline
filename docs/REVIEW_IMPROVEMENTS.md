# Whole-project review - improvements assessment

Date: 2 Jul 2026. A full pass over everything built this cycle: the scraping/LLM
pipeline, the outreach toolkit, the Power BI pack, and the documentation - plus
the incidents we hit along the way and what they teach us.

---

## What is genuinely solid (no action needed)
- The layered pipeline design: additive, cached, resumable at every stage - it
  survived multiple crashes and interruptions without losing work.
- The locked Power BI schema (append-only columns) - refresh has been stable
  since the mentions_idp incident.
- The LLM guardrails: annotate-only, temperature 0, content-hash caching,
  decision log, review queue. The funnel keeps LLM cost bounded.
- The cautious framing discipline (candidates, never confirmed examples;
  recommendations, never standards) is consistent across code, emails and docs.
- The outreach chain end-to-end: tiers, bilingual drafts, CC + named greetings,
  drafts-only safety, idempotent log, bounce protection after the NDR fix.

---

## 1. CRITICAL - the dashboard's LLM values are still the OLD (buggy) ones
The Canada investigation fixed three real defects in code (6000-char truncation
in two places, num_ctx defaulting to 2048, rule score uncapped) and bumped
PROMPT_VERSION to v3 so the fixes actually take effect. **But no LLM run has
happened since.** Everything live today - evidence_flagged_20260629.csv, the
Power BI fact table, sem_similarity, llm_counted, AND the outreach tiers
(sure_bet was defined partly by llm_counted) - was computed under the old
truncated regime. Canada-class under-scores are still in the data being used to
prioritise emails.

**Action (before sending any tiered emails):** with Ollama up,
```r
source("GAIN_SEMANTIC_FUNNEL.R")     # re-extracts under v3 (~3-5h; embeddings cached)
source("GAIN_PHASE5_CROSSREF.R")     # example-level match
source("GAIN_POWERBI_EXPORT.R")      # refresh the pack
source("GAIN_OUTREACH_TARGETS.R")    # re-tier the emails on corrected llm_counted
source("GAIN_OUTREACH_LOG.R"); source("GAIN_CYCLE_TRACKER.R")
```
Also set SENDER_NAME + GAIN_SURVEY_LINK first (still placeholders).

> **Status update (2 Jul 2026):** items 2, 3 and 4 are DONE and verified -
> sticky contact assignment (plus the log now preserves notes by country, so a
> human reassigning to_email in the log no longer orphans them), GAIN_COMMON.R
> + GAIN_CONFIG.R consolidation, and the cache-hit sleep fix. Items 1, 5 and 6
> remain open; item 1 (re-run the LLM chain under v3) is still the priority.

## 2. DONE - outreach log key instability (flagged in review, still open)
GAIN_OUTREACH_LOG.csv keys rows on country|to_email. If a re-run of TARGETS
picks a different "best contact" for a country (ranking ties break on Excel row
order), the old row's human notes are silently orphaned and a fresh draft_ready
row appears. Once the team starts writing real notes, this becomes data loss.
**Fix: sticky assignment** - if a country already has a log row, TARGETS should
reuse that contact instead of re-ranking. ~20 lines.

## 3. HIGH - one shared helpers/config file instead of drift-prone copies
Two concrete duplications already bit or nearly bit us:
- `harmonize_country()` exists as separate literal copies in GAIN_PHASE5_CROSSREF.R
  and GAIN_OUTREACH_TARGETS.R. A spelling fix added to one will silently miss
  the other, and country joins are the backbone of the whole outreach chain.
- SENDER_NAME / GAIN_SURVEY_LINK live in both TARGETS and FOLLOWUP (currently
  protected only by a warning).
**Fix:** a small `GAIN_COMMON.R` (harmonize_country, newest(), safe_write) and
`GAIN_CONFIG.R` (sender, links, deadline, thresholds), sourced everywhere. ~1h.

## 4. MEDIUM - easy re-run speed win: don't sleep on cache hits
GAIN_ENRICH_EVIDENCE.R sleeps 0.8s per record even when the document came from
evidence_cache (no network call). On the current 1,224-record master that is
~16 minutes of pure sleep per re-run. The merge script already solved this
(fetch_title returns a fetched flag; sleeps only on real fetches) - apply the
same pattern to fetch_document. ~10 lines.

## 5. MEDIUM - version control and the two-folder problem
This project has no git repo; the informal mirror in Downloads caused two real
incidents (the all-NA dashboard; the 262-vs-1174 funnel). OneDrive gives some
file history but no diffs/rollback discipline.
**Fix:** `git init` in the R script folder, commit the scripts (data/caches in
.gitignore), and stop treating Downloads as a working copy - keep it as a
one-way backup only, or drop it. ~30 min, prevents the most damaging class of
incident we actually experienced.

## 6. MEDIUM - documentation drift
The docs were excellent when written but the toolkit outgrew them:
- README_OUTPUTS.md and the data dictionary (md + docx) don't cover the newer
  Power BI tables: WEB_GAIN_outreach, WEB_GAIN_outreach_log,
  WEB_GAIN_cycle_tracker, WEB_GAIN_displacement_context.
- No single runbook for the outreach cadence (initial send flow vs the weekly
  sync -> triage -> follow-up loop) - it exists only in chat history.
**Fix:** one WORKFLOW.md runbook + a dictionary refresh. ~1h.

## 7. LOW - housekeeping
- ~1,400 orphaned v2 extract cache files in ollama_cache/ (dead weight since
  the v3 bump; harmless, deletable).
- MAX_PATH: the project folder is ~190 chars deep; cache paths reach ~242 of
  the 260 limit. The keyword script now has a short-path guard; the rest is OK
  today but avoid deeper nesting / longer filenames (or enable Windows
  LongPathsEnabled).
- GAIN_bounced_master.csv writes country as NA since the NDR fix (cosmetic).
- Native-speaker review of the FR/ES/AR/RU/ZH templates still pending.

## 8. PARKED (deliberately, by your decision - listed so they aren't lost)
- Stages 6-9 of the cycle: hold-for-launch, launch-wave email, conversion
  matcher, suppression list (waiting on GAIN survey timing).
- Keyword candidates from the direct document read: "habitual residence",
  "statelessness determination procedure" (agreed useful, not yet wired);
  birth registration was handled the safe way (scoring-only co-occurrence).
- Structured discovery extensions: PxWeb (Nordics), OECD/ILOSTAT SDMX;
  UNICEF SDMX refugees/asylum for the context table (IDPs only today).
- Power BI outreach funnel/map page design.
- LLM keyword/grounding run: in progress (~8h compute left, resumable);
  review keyword_suggestions_*.csv and the grounding draft when done, then
  activate GAIN_RECOMMENDATION_GROUNDING.txt.

---

## Recommended order
1. **Re-run the LLM chain under v3** (item 1) - everything downstream of the
   tiers depends on it; do this before any emails go out.
2. Sticky contact assignment in the log (item 2) - before the team starts
   writing notes into the log.
3. GAIN_COMMON.R + GAIN_CONFIG.R consolidation (item 3) and the sleep fix
   (item 4) - one short session, permanent payoff.
4. git init + retire the Downloads mirror (item 5).
5. Runbook + dictionary refresh (item 6) once the v3 numbers are in.
6. Review the keyword-suggestion output when the run finishes; wire approved
   terms + grounding file; consider a small A/B on scores before/after
   grounding to see if it actually helps the model.
