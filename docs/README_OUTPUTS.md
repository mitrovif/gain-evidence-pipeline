# GAIN Web Scraping — What the pipeline produces & what's in Power BI

Last verified: data run of 18–19 Jun 2026 (340 candidate records).

This file answers two questions: **what every output file is**, and **exactly what is
in the Power BI pack** (every column, and whether it is populated on a normal run vs
only after the local-LLM run).

---

## 1. The pipeline at a glance

```
Layers 1–3 (scrape)  ─►  Merge  ─►  Enrich  ─►  [Semantic funnel*]  ─►  Crossref  ─►  Power BI
  catalogs+search+web    master    scoring &     embeddings + LLM       GAIN match     WEB_GAIN_*
                         reference  tagging       extract (DO_LLM)       + review_queue   tables
```
`*` the funnel and the example-level match only run when `DO_LLM = TRUE` in RUN_ALL.R.

Run everything with: open **GAIN_WebScraping.Rproj**, then `source("RUN_ALL.R")`.

---

## 2. Every output file (newest of each, dated)

| File | What it is | Made by |
|---|---|---|
| `GAIN_MASTER_REFERENCE_*.csv` | One row per discovered URL (country, title, url, populations, year, producer, trust). The de-duplicated evidence backbone. | Merge |
| `GAIN_EVIDENCE_ENRICHED_*.csv` | The master + all scoring/tagging: mention counts, inclusion signal, scores+caps, outreach category A–E, lead type, contacts, plain-English comment. | Enrich |
| `GAIN_EVIDENCE_ENRICHED_*_SEM.csv` | The enriched file **plus** semantic + LLM columns (only exists after a `DO_LLM` run). | Semantic funnel |
| `GAIN_EVIDENCE_REPORT_*.html` | Human-readable review report — open in a browser; one card per candidate, highlighted terms, contacts. | Enrich |
| `evidence_flagged_*.csv` | Every candidate + GAIN cross-reference: country-level `gain_flag`, and (after `DO_LLM`) the example-level `gain_match_*`. Feeds Power BI. | Crossref |
| `country_priority_matrix_*.csv` | Per-country outreach priority (P1–P6) and GAIN respondent status. | Crossref |
| `contact_gaps_*.csv` | Priority countries missing a usable contact. | Crossref |
| `suggested_respondents_*.csv` | Ranked candidate respondents per target NSO. | Crossref |
| `review_queue_*.csv` | (After `DO_LLM`) new examples in existing GAIN countries + low-confidence matches — the manual-review pile. | Crossref |
| `LAYER3_inventory_stats_*.csv` | Per-domain crawl/search processing status. | Layer 3 / Enrich |
| `ollama_decisions_log.csv` | (After `DO_LLM`) one row per LLM decision (extract & adjudication) — full audit trail. | Ollama helpers |
| `powerbi_export/WEB_GAIN_*.csv` | The dashboard pack (see §3). | Power BI export |

Caches (never delete unless you want to re-fetch): `evidence_cache/` (fetched docs),
`l3_cache/` (site inventories), `ollama_cache/` (LLM results + embeddings). These make
re-runs fast and the whole pipeline resumable.

---

## 3. The Power BI pack — `powerbi_export/`

Connect Power BI: **Get data ▸ Folder ▸ point at `powerbi_export/`**, set each CSV to
**File Origin = 65001: Unicode (UTF-8)**. Four tables:

| Table | Grain | Rows (this run) |
|---|---|---|
| `WEB_GAIN_fact_candidates.csv` | one row per discovered candidate | 340 |
| `WEB_GAIN_dim_country.csv` | one row per clean single country | 78 |
| `WEB_GAIN_fact_contacts.csv` | one row per suggested contact | 56 |
| `WEB_GAIN_processing_status.csv` | one row per domain crawled | — |

**Model:** relate `fact_candidates[map_country] → dim_country[map_country]` (many-to-one).
Use `map_country` for the map, never raw `country`. Full visual guidance is in
`powerbi_export/WEB_GAIN_README.txt`.

### Schema is LOCKED
The column **names and order are fixed** (the export enforces this every run), so the
dashboard refreshes without breaking. New analytical fields are appended at the END;
existing columns never move or get renamed. To add a column later, append it to the
schema list in `GAIN_POWERBI_EXPORT.R` (never reorder).

### fact_candidates — all 54 columns

**A. Identity / source** (always populated)
`data_source` (= "GAIN Web Scraping"), `country`, `title`, `dashboard_title`
(title prefixed `[GAIN Web Scraping]`), `url`, `source_layer`, `trust`, `doc_language`,
`doc_type`, `year`, `pub_date_guess`.

**B. Geography for the map** (always populated)
`map_country` (clean single-country name; blank for multi-country/regional rows),
`iso3`, `is_single_country`.

**C. Rule-based classification — the PRIMARY columns** (always populated, deterministic, auditable)
`outreach_category` / `category_letter` (A strong NSO follow-up / B partner-or-unclear /
C evidence lead only / D manual review / E suppress), `evidence_nature`
(official statistics vs humanitarian/operational), `is_humanitarian`,
`candidate_lead_type` (likely country-led / partner-led / unclear), `relevance_score`,
`instrument_includes_displacement`, `mentions_inclusion`, `pop_stat_cooccurrence`,
`populations`, `mentions_refugee`, `mentions_idp`, `mentions_stateless`, `mentions_egriss`,
`is_actionable`, `is_suppressed`, `is_official_actionable`, `outreach_priority`,
`overall_comment` (plain-English summary).

**D. Outreach contact** (always populated where a contact was found)
`recommended_outreach_route`, `recommended_primary_contact`,
`recommended_primary_contact_type`, `contact_confidence`, `contact_validation_needed`.

**E. GAIN cross-reference**
`gain_flag` (country-level: IN_GAIN / POSSIBLE / NEW / MULTI-COUNTRY — always populated),
`in_gain_already` (friendly version of `gain_flag` — always populated),
`gain_respondent_status` (ACTIVE / LAPSED / NEVER — always populated).

**F. Example-level match + LLM enrichment** — *populated ONLY after a `DO_LLM = TRUE` run;
blank otherwise (the columns always exist, so the schema never changes):*
`gain_match_type` (already_in_gain / new_example_existing_country / new_country),
`matched_gain_id` (the GAIN example index when matched), `gain_match_confidence`,
`gain_match_reason`, `sem_similarity` (cosine to nearest GAIN example),
`llm_relevance`, `llm_counted`, `llm_implementation_status` (implemented/planned/unclear),
`llm_lead_type`, `llm_is_humanitarian`, `llm_quote` (verbatim evidence sentence),
`llm_confidence`, `lead_agreement` (does the LLM lead call agree with the rule one).

> **Is everything in Power BI now?** Yes — all 54 columns are present in the file. The
> rule-based columns (groups A–E) are filled on every run. The LLM/example columns
> (group F) are present but **empty until you run with `DO_LLM = TRUE`** (Ollama up).
> See README_OLLAMA_LLM.md for how that layer works.

### Which columns to use for what (recommended)
- **Already in GAIN?** Default `in_gain_already` (country-level). After an LLM run, switch
  to **`gain_match_type`** (example-level — far more accurate; the country-level flag
  over-claims).
- **Lead institution:** `candidate_lead_type` (primary). Filter `lead_agreement = DISAGREE`
  to find rows where the LLM disagrees and a human should check.
- **Priority/relevance:** `relevance_score` (primary, deterministic). `llm_relevance` is a
  second opinion on the gated band only.
- **Official vs humanitarian:** `is_humanitarian` (primary); `llm_is_humanitarian` to compare.
- **Map bubble size:** `official_actionable` from `dim_country` (real, official leads).

---

## 4. Refreshing the dashboard
Re-run the pipeline, then `source("GAIN_POWERBI_EXPORT.R")`, then click **Refresh** in
Power BI. Same folder, same file names, **same locked schema** → no re-mapping needed.
If a `WEB_GAIN_*` file is open in Excel/Power BI during a run, the export writes a
timestamped copy instead of failing — so close them before refreshing to keep the clean
canonical names.

---

## 4c. Structured-API discovery + displacement context (added Jun 2026)
Two new no-key, cached sources now feed the pipeline (see `NOTE_SEARCH_ARCHITECTURE.md`):

- **Structured discovery** (`GAIN_LAYER1_STRUCTURED.R` → `structured_discovery_*.csv`):
  queries **Eurostat** for official asylum / decisions / resettlement /
  unaccompanied-minors / stateless statistics — **175 clean records / 44 European
  countries**, producer known, zero HTML noise. They flow into the merge as
  `source_layer = STRUCT:Eurostat` and now appear in Power BI. They classify as
  **"D. Manual review needed"** by design: they prove a country *produces*
  displacement statistics, but Eurostat asylum data is administrative — not proof
  of EGRISS-style inclusion — so a reviewer confirms them (filter
  `source_layer = STRUCT:Eurostat` to batch). `NSO_Full_Registry_routed.csv` adds a
  `discovery_interface` column routing each country to API-vs-scrape.
- **Displacement context** (`GAIN_SDMX_DISPLACEMENT.R` →
  `WEB_GAIN_displacement_context.csv`): per-country **IDP magnitudes** (UNICEF SDMX /
  IDMC-sourced) — *context, not evidence*. Relate it to `dim_country` on `iso3`.
  Separate table — does **not** touch the locked 54-column fact schema.

Power BI pack after these: **fact_candidates 581 rows / 108 countries** (L1 catalog
221, STRUCT 175, L3 web 145, L2 search 40); schema still 54 cols, `mentions_idp` at
position 26 — refresh-safe.

---

## 5. Cautious framing (always)
Every row is a **possible candidate** that **requires manual review** and **confirmation
from the NSO or relevant respondent**. Nothing here is a confirmed GAIN example;
`relevance_score` and the LLM fields are **screening priority only**. EGRISS frameworks
are **recommendations** (IRRS / IRIS / IROSS), never "standards".
