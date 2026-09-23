# Power BI restructure — using the Ollama / LLM fields

Date: 23 Jun 2026. After the two crash fixes (crossref types + Power BI
countrycode guard) the pack carries the full LLM layer. This note: (1) how to
finish the run, (2) what's now included, (3) how to restructure the report.

---

## 1. Finish the run (on the machine that has the overnight outputs)
Your overnight run succeeded through the LLM step (102 extracted, 44 counted=TRUE,
`GAIN_EVIDENCE_ENRICHED_20260623_SEM.csv`) but the last two steps crashed. Both
are now fixed. Re-run only the last two steps:

```r
# Ollama must be running for the example-level match (adjudications weren't cached)
Sys.setenv(GAIN_SKIP_EXAMPLE_MATCH = "0", GAIN_MAX_ADJUDICATIONS = "450")
source("GAIN_PHASE5_CROSSREF.R")   # writes a fresh evidence_flagged_* with llm_* + gain_match_*
source("GAIN_POWERBI_EXPORT.R")    # rebuilds the WEB_GAIN_* pack
```
- LLM **extracts are already cached**, so this is fast; only the example-level
  adjudications run (~10–20 min with Ollama up).
- **No Ollama?** Run `Sys.setenv(GAIN_SKIP_EXAMPLE_MATCH = "1")` first: you still
  get every `llm_*` field in the dashboard; only the example-level `gain_match_*`
  stays blank. Re-run later with Ollama up to fill it.

Then in Power BI: **Refresh** (same folder, same locked schema → no re-mapping).

## 2. What is now included (verified)
The locked `fact_candidates` schema carries all 13 LLM/example columns, and the
funnel writes those exact names, so they populate after the re-run:

| Field | Meaning |
|---|---|
| `gain_match_type` | **example-level**: already_in_gain / new_example_existing_country / new_country |
| `matched_gain_id`, `gain_match_confidence`, `gain_match_reason` | which GAIN example matched + why |
| `llm_counted` | **did the LLM confirm a displaced group was actually included?** (TRUE/FALSE) |
| `llm_relevance` | LLM relevance score (second opinion vs `relevance_score`) |
| `llm_implementation_status` | implemented / planned / unclear |
| `llm_lead_type` + `lead_agreement` | LLM lead call, and whether it agrees with the rule one |
| `llm_is_humanitarian` | official vs humanitarian (LLM second opinion) |
| `llm_quote` | the verbatim evidence sentence — great for reviewers |
| `llm_confidence`, `sem_similarity` | confidence + semantic closeness to GAIN examples |

New **country-level rollups** in `dim_country` (for the map/bars):
`llm_confirmed_inclusion`, `new_to_gain_examples`, `already_in_gain_examples`.

## 3. Recommended report structure (3 pages)

### Page 1 — Executive overview (refine your current page)
KPI cards (swap the country-level ones for the example-level / LLM ones):
- **Total candidates** = COUNTROWS(fact_candidates)
- **Official-statistics worklist** = COUNTROWS filtered `is_official_actionable = TRUE`
- **LLM-confirmed inclusion** = `CALCULATE(COUNTROWS(fact), fact[llm_counted] = TRUE)`  ← NEW
- **New to GAIN (verified)** = `CALCULATE(COUNTROWS(fact), fact[gain_match_type] IN {"new_country","new_example_existing_country"})`  ← replaces the country-level "New to GAIN"
- **Strong NSO leads** = category A count
- Map: `map_country`, legend = `gain_match_type`, size = `relevance_score`.

### Page 2 — GAIN match (the example-level fix — the headline new capability)
The old country-level flag over-claimed "already in GAIN". Use `gain_match_type`:
- Slicer: `gain_match_type`.
- Cards: already_in_gain / new_example_existing_country / new_country counts
  (the dim rollups `already_in_gain_examples`, `new_to_gain_examples`).
- Table = the **review queue**: filter `gain_match_type = "new_example_existing_country"`
  OR (`already_in_gain` AND `gain_match_confidence = "low"`); columns Country,
  Title, `gain_match_reason`, `matched_gain_id`, `gain_match_confidence`. This is
  the human worklist — what is genuinely new vs already covered.

### Page 3 — LLM evidence quality & triage
- Primary filter: `llm_counted = TRUE` ("inclusion actually confirmed").
- **Lead disagreements** table: `lead_agreement = "DISAGREE"` → a human checks the
  lead call. Measure: `CALCULATE(COUNTROWS(fact), fact[lead_agreement]="DISAGREE")`.
- `llm_relevance` vs `relevance_score` scatter (LLM vs rule — outliers = review).
- `llm_is_humanitarian` vs `is_humanitarian` split (official vs humanitarian).
- `llm_implementation_status` slicer (implemented / planned / unclear).
- Put `llm_quote` in the table/tooltip — the one-sentence evidence reviewers need.
- `llm_confidence` and `sem_similarity` as confidence slicers.

### Useful slicers to add globally
`gain_match_type`, `llm_counted`, `llm_implementation_status`, `lead_agreement`,
`gain_match_confidence`, `llm_confidence`, plus your existing Type-of-data / Lead /
Priority / Language / Year.

## 4. Two honesty caveats for this run
- **Web search (Layer 2 / Exa) contributed almost nothing this run** — the log
  shows Google keys invalid and only **1** Layer-2 record; Exa did not run (the
  `GAIN_SKIP_GOOGLE=1` switch / synced Exa code wasn't active). To add the Exa
  hits, re-run Layer 2 with `Sys.setenv(GAIN_SKIP_GOOGLE="1", GAIN_EXA_MAX="1000")`
  then re-run merge → enrich → crossref → Power BI. So the current evidence is
  mostly catalog (Layer 1) + sitemaps/Common-Crawl (Layer 3) + structured Eurostat.
- **Example-level `gain_match_*` only fills when Ollama is up** for the crossref
  re-run; otherwise use `GAIN_SKIP_EXAMPLE_MATCH=1` and the `llm_*` fields still load.

## 5. Always-on framing
Every row is a **possible candidate requiring manual review + NSO confirmation**.
`llm_counted`/`gain_match_type` are **screening aids**, not confirmed GAIN status.
EGRISS frameworks are **recommendations** (IRRS/IRIS/IROSS), never "standards".
