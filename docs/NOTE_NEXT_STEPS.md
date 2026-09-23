# Note — what I would do next, and how

Author: pipeline build session · Date: 22 Jun 2026
Audience: GAIN / EGRISS team (non-technical readable)

This note sets out the recommended next moves for the GAIN web-scraping +
displacement-statistics search, why each matters, and how I'd do it. It is a
plan, not a change — nothing here runs until you say so.

---

## Where we are today (one paragraph)
The pipeline finds candidate evidence of displaced-population inclusion in
official statistics across 172 NSO websites + catalogues, scores and tags it,
cross-references it against the GAIN database, and exports a Power BI pack
(`WEB_GAIN_*`). The current run holds **340 candidates / 78 countries**, on a
**locked schema** so your dashboard refreshes cleanly. A local-LLM layer (Ollama:
qwen2.5 + bge-m3) is **built and tested but not yet run for real** — it adds
example-level GAIN matching, an inclusion read, and a review queue. A new
**UNICEF SDMX displacement-context layer** (IDPs per country) is in place. See
`README_OUTPUTS.md` and `README_OLLAMA_LLM.md` for the full picture.

---

## Priority 1 — Complete the displacement-context layer (refugees + asylum)
**Why.** Right now the context table carries IDPs (from IDMC) but not refugees or
asylum-seekers — and refugees are the single most GAIN-central number. Without
them the "where is displacement largest" view is half-blind (e.g. major refugee-
hosting countries like Jordan, Lebanon, Uganda look empty).

**How.** Refugee/asylum series are not in the UNICEF `GLOBAL_DATAFLOW` I queried;
they sit in a dedicated dataflow. I would: (a) query the UNICEF SDMX API's
structure to confirm the dataflow/key that holds `MG_RFGS` / `MG_ASYLM` data,
(b) add it as a second fetch target in `GAIN_SDMX_DISPLACEMENT.R` (same cached,
no-key pattern), (c) populate `refugees`, `asylum_seekers` in the context table.
Effort: ~1–2 hours, low risk, fully additive. Note honestly: these remain
UNHCR-sourced *context*, not NSO evidence.

---

## Priority 2 — Let real displacement magnitude drive search priority
**Why.** The 172-domain `NSO_Full_Registry.csv` priorities (1=high … 3=low) are
hand-assigned. Some are debatable. Real IDP/refugee magnitude is an objective
signal of where displaced people actually are — and therefore where NSO
inclusion matters most and our crawl should dig hardest.

**How.** Join the SDMX `displaced_total` per country into the registry and derive
a data-driven priority (e.g. top quartile of displaced population → priority 1).
I'd keep it a **suggestion, not an override**: write a `priority_suggested`
column next to your manual `priority`, show both, and only adopt it where you
agree. Effort: ~1 hour. Risk: low — it informs, it doesn't silently change the
crawl. This turns a subjective list into an evidence-based one.

---

## Priority 3 — Run the LLM layer for real (it's built, never executed at scale)
**Why.** The example-level matching is the core fix for the cross-reference
problem: the old country-level logic claims 222 candidates are "already in GAIN"
when it really just means "this country has *some* matching example". The LLM
layer decides, per artifact, whether it is genuinely the same GAIN example, a
*new* example in an existing country, or a new country — and routes the
uncertain ones to a review queue. Until it runs, those columns are empty.

**How.** With Ollama up: set `DO_LLM <- TRUE` in `RUN_ALL.R` and run once. Budget
~3 h for the funnel extracts + ~1.5 h for the example-match adjudications; every
call is cached, so it is a one-time cost and resumable. Then in Power BI switch
the "already in GAIN?" visuals from `in_gain_already` (country-level, over-claims)
to `gain_match_type` (example-level, accurate). I'd run it on the current 340
records first, eyeball ~20 results + the `review_queue`, then tune the two
thresholds (`GAIN_GATE_SIM`, `GAIN_MAX_ADJUDICATIONS`) before trusting it broadly.

---

## Priority 4 — Establish the human review loop
**Why.** GAIN is self-reported; this pipeline supports screening, it does not
replace NSO confirmation. The value is realised only when a person works the
shortlist. Everything is already framed cautiously ("possible candidate",
"requires confirmation") for exactly this.

**How.** Treat three files as the weekly worklist: the **category A/B** rows in
`fact_candidates` (strong NSO follow-up), `review_queue_*.csv` (new examples +
low-confidence matches), and `contact_gaps_*.csv` (priority countries missing a
contact). A reviewer confirms/rejects each; confirmations feed the GAIN survey
follow-up, rejections can be captured in `MANUAL_OVERRIDES.csv` so the pipeline
learns not to resurface them. Effort: process, not code.

---

## Things I would explicitly NOT do (and why)
- **Don't treat SDMX/partner figures as GAIN evidence.** IDMC/UNHCR counts are
  context. Folding them into the evidence base would flood it with non-NSO data.
- **Don't auto-confirm `already_in_gain`.** Even with high LLM confidence it stays
  a *suggestion* a human approves — the determinism guardrail we built on purpose.
- **Don't add more keyword breadth without a precision check.** Past breadth
  additions (bare `migration`, `nationality`, `asilo`) caused the Spain/Brazil
  floods. New terms get the strong/weak split + a before/after count first.

---

## Recommended sequence
1. **Refugees + asylum into the context layer** (P1) — completes the picture, low risk.
2. **Suggested priority from displacement magnitude** (P2) — review, then adopt where you agree.
3. **One real LLM run on the current data** (P3) — see the example-level matches + review queue.
4. **Stand up the weekly review loop** (P4) — A/B list + review_queue + contact_gaps.
5. Re-run end-to-end, refresh Power BI (locked schema → no re-mapping).

Each step is additive, cached, and reversible. I'd do them in this order because
P1 and P2 are quick objective wins, P3 is the high-value step that needs a few
hours of machine time, and P4 is what actually turns the dataset into outreach.

---

## Open questions for you
- Refugee/asylum context: pull from UNICEF SDMX (consistent, one source) or
  straight from UNHCR's own API (more authoritative, another integration)?
- Priority: happy for displacement magnitude to *suggest* registry priority, or
  keep priorities fully manual?
- LLM run: do it now on 340 records, or wait until after a fresh full scrape so
  the first (cached) pass covers the larger set?
