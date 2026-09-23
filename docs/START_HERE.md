# START HERE - GAIN Reference File Pipeline
## The only 6 scripts you need (everything older is obsolete)

> **RUN FROM ANY MACHINE (12 Jun 2026).** This folder is fully self-contained and
> SharePoint-synced: scripts, registry, all data, the document cache AND the API
> keys (`.Renviron` in this folder) travel together.
>
> On any machine that syncs this folder:
> 1. Double-click **GAIN_WebScraping.Rproj** (opens RStudio in this folder,
>    keys load automatically) - or start R here any other way.
> 2. Run the script you need, e.g. `source("GAIN_LAYER2_SEARCH_API_5.R")`.
>    No setwd, no Sys.setenv, no per-machine setup.
>
> Rules of the road:
> - **One machine at a time.** Progress/cache files sync via OneDrive; running on
>   two machines simultaneously creates sync conflicts.
> - **Let OneDrive finish syncing** before starting on a different machine.
> - The keys are readable by anyone with access to this SharePoint library -
>   they are low-sensitivity search keys, but rotate them if the library is shared
>   more widely.
> - The pipeline is incremental everywhere: completed queries, fetched documents,
>   contact crawls and site inventories are never re-downloaded.

> **Update 12 Jun 2026 (v8) - higher recall, fully incremental:**
> - **More key terms everywhere**: 24 supplementary Layer 2 query groups in the 8 core
>   languages (refugee camp, asylum applications, UNHCR/HCR/ACNUR/УВКБ, persons of
>   concern, displaced persons/households, forced migration, returnees, conflict-affected,
>   citizenship/nationality status...), ~25 new URL keywords in Layer 3 (fluechtling,
>   uchodz, izbeglic, unhcr, nationality...), expanded news anchor-text terms, and the
>   same terms added to the ENRICH scoring patterns so the new hits score properly.
> - **Nothing is re-scraped**: new terms are NEW progress keys, so the 50 countries
>   already searched run ONLY the ~198 incremental top-up queries; Layer 3 now caches
>   each domain's full inventory for 30 days in l3_cache/ (keyword changes re-match
>   cached lists instantly - only new/stale domains are downloaded); merge title
>   fetches are now cached too; documents and contacts were already cached.
> - Query budget: 1498 total | ~1244 remaining (mostly the 122 never-searched domains)
>   = about 3 days at 400 free queries/day once the 4 keys are in .Renviron.
> - One-time note: the first Layer 3 run after this update fetches all 172 domains once
>   to seed the cache (the old run predates caching); after that, everything is incremental.
>
> **Update 12 Jun 2026 (v7) - DHS/MICS, encoding fix, news crawl, friendlier review:**
> - **DHS + MICS added to Layer 1** (DHS via its public API, MICS via the NADA
>   catalogs), with a 2022+ window for those two sources (long survey cycles);
>   all other catalogs stay 2024+.
> - **Excel encoding fixed**: every pipeline CSV now written with UTF-8 BOM
>   (Ukrainian/Arabic/accents render correctly when double-clicked into Excel),
>   plus an automatic mojibake REPAIR for already-garbled text
>   ("TÃ¼rkiye" -> "Türkiye", "Ð¿Ñ–Ð´Ð¿Ñ€..." -> "підпр...") applied in MERGE and
>   ENRICH - re-running those two scripts cleans existing data, no re-harvest.
> - **NSO news/events/publications pages crawled** (Layer 3): link TEXT matched
>   in all pipeline languages, found items flow in as source "news-section" and
>   their link text becomes the record title (fewer title fetches too).
> - **Friendlier review output**: new `overall_comment` column - one cautious
>   plain-English paragraph per record (what it is, what was found, who leads,
>   suggested contact, next step) shown on every report card; `doc_language` and
>   `pub_date_guess` columns for sorting; English working summary + original
>   excerpt now for EVERY non-English record (not just Arabic); excerpts longer
>   (400 chars) and more (up to 5); `top_candidates` per country added to the
>   processing overview.
> - The Ukrainian "weird signs" rows were enterprise-statistics false positives
>   in mojibake form - both issues fixed; they disappear on the next MERGE+ENRICH run.

> **Expansion to 172 NSO websites (11 Jun 2026):** `NSO_Full_Registry.csv` now holds
> all 172 domains in 4 engine groups and is picked up automatically by Layers 2/3 and
> ENRICH. To activate groups 2-4 you need 3 new Google engines + 3 new API keys —
> follow **SETUP_ADDITIONAL_ENGINES.md** (15-20 min of clicking, paste-ready site
> lists included). All 3 engines are created (IDs in ~/.Renviron);
> only the 3 extra API keys remain (replace the PASTE_ placeholders in .Renviron).
> Keyword dictionary covers 40 languages incl. every local language of the new
> sites. ~641 new queries ≈ 2 days at 400 free queries/day with 4 keys.

> **Updated 11 Jun 2026 (v6) - outreach, Arabic, false positives, key rotation:**
> - **Outreach contacts:** ENRICH now crawls public NSO contact pages (contact/about/
>   staff/departments/microdata/data-request/...) and adds 13 outreach fields per record
>   (recommended primary/secondary contact + type + source, confidence, validation flag,
>   contact gap + reason, route, priority, notes). Evidence-source emails are kept separate
>   from outreach contacts; catalogue/partner emails are never assumed to be NSO contacts.
> - **Outreach categories A-E** per record (A strong NSO follow-up / B partner-led or
>   unclear / C evidence lead only / D manual review / E suppress) and
>   **candidate_lead_type** (likely country-led / likely partner-led / unclear / n.a.).
> - **Arabic:** full Arabic term list (population + statistical-source terms), UTF-8 BOM
>   CSVs so Excel renders Arabic, RTL-safe HTML report, and an automated English term
>   gloss + original Arabic excerpt for each Arabic-language candidate (clearly labelled
>   as a gloss requiring manual review - not a translation).
> - **Ukraine/Russia false positives:** "idp" must be a standalone token in URLs
>   (pidpryiemstv/enterprise pages excluded in Layer 3 and suppressed in ENRICH unless the
>   text contains a valid displacement term: ВПО, ВПЛ, внутрішньо переміщені особи, etc.).
> - **Score caps:** no relevant terms -> max 20; generic migration only -> max 30;
>   URL-inventory-only without text -> max 45; casino/betting/spam -> Exclude.
>   Optional MANUAL_OVERRIDES.csv (url, override_relevant, note) lifts caps after review.
> - **Key rotation (Layer 2):** reads GOOGLE_CSE_KEY + SEARCH_API_KEY_1..4 from env vars,
>   rotates on quota/failure, logs the slot name (never the key) in LAYER2_progress.csv,
>   stops gracefully and reports unprocessed domains. Resumable as before.
> - **~150-site expansion:** drop NSO_Full_Registry.csv (country, domain, languages,
>   cse_group) in this folder - Layers 2/3 and ENRICH pick it up automatically.
> - **Processing overview:** ENRICH updates LAYER3_inventory_stats_*.csv IN PLACE with
>   processing/inventory/extraction/contact-search status, candidate counts, failure
>   reason and next_action per domain; same table appears at the top of the HTML report.
> - **Cautious GAIN framing everywhere:** all notes/summaries say "possible candidate",
>   "may be relevant to GAIN", "requires manual review/NSO confirmation" - nothing is ever
>   labelled a confirmed GAIN example. Crossref re-routes strong candidates in countries
>   with an ACTIVE respondent to the existing GAIN focal point.
> - No new output files: all improvements live inside the existing outputs
>   (ENRICHED CSV, HTML report, inventory stats, evidence_flagged, priority matrix,
>   contact_gaps, suggested_respondents).
> - Functional tests: test_gain_v6_funcs.R (re-run any time; safe, no network).
>
> **Updated 11 Jun 2026 - rebalanced toward NSO websites (country-led examples):**
> - Microdata libraries (IHSN / UNHCR / World Bank / ReliefWeb) now restricted to
>   **2024-2026 records only** (filtered at the API and again in REFINE + MERGE,
>   so re-running REFINE on your existing 20260610 CSV is enough - no re-harvest needed)
> - Layer 2 + 3 NSO-website keywords expanded (asylum seekers, forcibly displaced,
>   durable solutions, returnees, geçici koruma, etc.) and **EGRISS / IRRS / IROSS /
>   "international recommendations" added** as search terms
> - MERGE now enforces **>= 60% of master-file records from NSO websites / NSO-led
>   efforts**; catalog/int-org records capped at 40% (highest trust + most recent kept)
> - ENRICH counts EGRISS mentions (x10 score weight) and boosts NSO-website records +20
> - Layer 2 quota note: the EGRISS query adds 1 query per country (~50 extra; total ~250)

Put all 6 .R files in: <GAIN_ROOT>/EGRISS Database Integration/GAIN Web Scarping/R script
Always start R with: setwd("<GAIN_ROOT>/EGRISS Database Integration/GAIN Web Scarping/R script")

| # | File | What it does | Status |
|---|------|--------------|--------|
| 1 | GAIN_LAYER1_CATALOGS.R | Harvests IHSN / UNHCR / World Bank / ReliefWeb survey catalogs | DONE (you have LAYER1_catalog_records_20260610.csv) |
| 2 | GAIN_LAYER1_REFINE.R | Tiers + dedups the Layer 1 harvest | DONE (you have LAYER1_refined_20260610.csv) |
| 3 | GAIN_LAYER2_SEARCH_API.R | Google search across 50 NSO domains, 2024+ only | BLOCKED on Google API enable (see below) |
| 4 | GAIN_LAYER3_SITEMAPS_CC.R | Sitemap + Common Crawl URL inventories, 50 domains | READY - can run any time, no keys |
| 5 | GAIN_MERGE_MASTER_REFERENCE.R | Merges Layers 1-3 into GAIN_MASTER_REFERENCE.csv | Run after 3 and 4 |
| 6 | GAIN_PHASE5_CROSSREF.R | Cross-references master file with GAIN database, contacts, response status | Run last (also works standalone) |

---

## STEP 0 - Unblock Google (one time, 3 minutes)

1. Open: https://console.developers.google.com/apis/api/customsearch.googleapis.com/overview?project=127684829430
2. Check top-right: signed in with the SAME Google account that made the API key
3. Click the blue ENABLE button
4. Wait 3-5 minutes

Verify in R (expect 200):

    library(httr2)
    resp <- tryCatch(
      request("https://www.googleapis.com/customsearch/v1") |>
        req_url_query(key = Sys.getenv("GOOGLE_CSE_KEY"),
                      cx = Sys.getenv("GOOGLE_CSE_CX"),
                      q = "refugee", siteSearch = "ubos.org",
                      siteSearchFilter = "i", num = 5) |>
        req_perform(),
      error = function(e) e$resp)
    resp_status(resp)

If 403 persists and the page would not let you click Enable, your work
Google account may block it - make the key under a personal account instead.

---

## STEP 1 - Run Layer 2 (after Step 0 returns 200)

    setwd("<GAIN_ROOT>/EGRISS Database Integration/GAIN Web Scarping/R script")
    Sys.setenv(GOOGLE_CSE_KEY = "AIza...your key")
    Sys.setenv(GOOGLE_CSE_CX  = "<CSE_ID>")
    source("GAIN_LAYER2_SEARCH_API.R")

- ~198 queries total; free tier = 100/day
- It stops at quota and RESUMES automatically: just re-run the same line tomorrow
- Output: LAYER2_search_hits_[date].csv

## STEP 2 - Run Layer 3 (independent of Google; run today if you want)

    source("GAIN_LAYER3_SITEMAPS_CC.R")

- No keys needed. Takes ~30-60 min for 50 domains
- Outputs: LAYER3_url_inventory_[date].csv + LAYER3_inventory_stats_[date].csv

## STEP 3 - Merge into the master reference file

    source("GAIN_MERGE_MASTER_REFERENCE.R")

- Auto-finds the latest LAYER1_refined / LAYER2 / LAYER3 CSVs in the folder
- Output: GAIN_MASTER_REFERENCE_[date].csv

## STEP 4 - Cross-reference with GAIN

Needs in the same folder:
- GAIN_MASTER_REFERENCE_[date].csv  (set as EVIDENCE_FILE at top of script)
- analysis_ready_main_roster.csv
- analysis_ready_group_roster.csv
- GAIN Data Collection 2025_Email Link to 2024.xlsx

    source("GAIN_PHASE5_CROSSREF.R")

Outputs:
- evidence_flagged_[date].csv        IN_GAIN / POSSIBLE / NEW per release
- country_priority_matrix_[date].csv P1-P6 outreach tiers
- contact_gaps_[date].csv            where contacts are missing
- suggested_respondents_[date].csv   top 3 candidate respondents per country

---

## Files you can DELETE from Downloads (obsolete generations)

GAIN_NSO_Audit_Refined.R, GAIN_NSO_Audit_Enhanced_Metadata.R,
GAIN_NSO_Audit_Enhanced_FIXED.R, GAIN_NSO_IMPROVED_MULTI_LEVEL.R,
GAIN_NSO_AUTOCRAWL.R, GAIN_NSO_AUTOCRAWL_CORRECTED.R,
GAIN_NSO_DEEP_SEARCH.R, GAIN_INTELLIGENT_DEDUP.R,
GAIN_RULE_BASED_DEDUP.R, GAIN_OLLAMA_DEDUP.R, ANALYZE_AUDIT_RESULTS.R,
CHECK_CSV_STRUCTURE.R

Keep: the 6 scripts above + your data CSVs/XLSX.
