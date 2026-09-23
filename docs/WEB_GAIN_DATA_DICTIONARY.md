# WEB_GAIN Power BI — Data Dictionary (plain language)

What every column means, in simple terms. Four tables:
`fact_candidates` (one row per found document), `dim_country` (one row per
country), `fact_contacts` (one row per suggested contact), `processing_status`
(one row per website crawled).

> Golden rule: every row is a **possible candidate that still needs a human to
> review it and confirm with the NSO**. Nothing here is a confirmed GAIN example.
> Scores and AI fields are **screening aids only**. EGRISS frameworks are
> **recommendations** (IRRS / IRIS / IROSS), never "standards".

Fields tagged **(AI)** only fill in after the local-LLM (Ollama) run; they are
blank otherwise.

---

## TABLE 1 — `fact_candidates` (the main table: one row per document found)

### A. What and where the document is
| Column | Plain meaning |
|---|---|
| `data_source` | Always "GAIN Web Scraping" — marks the row as coming from this tool. |
| `country` | The country the document is about (raw, as found). May list several countries. |
| `title` | The document's title. |
| `dashboard_title` | Same title, prefixed `[GAIN Web Scraping]` so it's clear where it came from. |
| `url` | Direct web link to the document — click to open the source. |
| `source_layer` | How we found it: `L1:` = official catalogue, `L2:` = search engine, `L3:` = website crawl, `STRUCT:Eurostat` = structured statistics API. |
| `trust` | How reliable the find is: HIGH (curated catalogue / structured API / indexed page) vs MEDIUM (URL inventory) vs LOW (unreachable). |
| `doc_language` | Language the page/document is written in. |
| `doc_type` | Format: webpage, PDF, etc. |
| `year` | The year in the title/URL (the data/publication year, best guess). |
| `pub_date_guess` | Our best guess of the publication date. |

### B. The verdict — how the document was classified
| Column | Plain meaning |
|---|---|
| `outreach_category` | The headline bucket (full text). One of A–E (see next row). |
| `category_letter` | A = strong NSO follow-up candidate · B = strong but partner-led/unclear lead · C = evidence lead only · D = needs manual review · E = suppress (noise). |
| `relevance_score` | 0–100 priority score (rule-based). Higher = more likely to be real displacement-in-official-statistics evidence. **Screening priority, not proof.** |
| `evidence_nature` | Is this **official statistics** or **humanitarian/operational data**? |
| `is_humanitarian` | TRUE if the data is humanitarian/operational rather than national official statistics. |
| `candidate_lead_type` | Who likely led it: country-led (NSO), partner-led, or unclear. |
| `overall_comment` | A one-paragraph plain-English summary of why this row was flagged. **Read this first.** |
| `is_actionable` | TRUE if it's worth acting on (categories A/B/D). |
| `is_official_actionable` | TRUE if actionable **and** official statistics (excludes humanitarian) — the cleanest leads. |
| `is_suppressed` | TRUE if filtered out as noise (category E). |
| `outreach_priority` | Suggested priority for follow-up. |

### C. The signals we detected (the "why")
| Column | Plain meaning |
|---|---|
| `instrument_includes_displacement` | TRUE if the survey/census itself appears to include a displaced group. |
| `mentions_inclusion` | Count of inclusion-type wording (disaggregated by, refugee module, host community, etc.). |
| `pop_stat_cooccurrence` | TRUE if a displaced-population word sits **near** a statistics word on the page (a strong signal). |
| `populations` | Which displaced groups are named: refugees, idps, stateless. |
| `mentions_refugee` / `mentions_idp` / `mentions_stateless` | How many times each group is mentioned in the document. |
| `mentions_egriss` | How many times EGRISS / IRRS / IRIS / IROSS are mentioned. |

### D. Already in GAIN? (country-level — the older, rougher check)
| Column | Plain meaning |
|---|---|
| `gain_flag` | Country-level match: IN_GAIN / POSSIBLE / NEW / MULTI-COUNTRY. **Tends to over-claim "already in GAIN"** — prefer `gain_match_type` (AI) below. |
| `in_gain_already` | Friendly version of `gain_flag`. |
| `gain_respondent_status` | Has this country engaged with GAIN before: ACTIVE / LAPSED / NEVER. |

### E. Suggested contact for outreach
| Column | Plain meaning |
|---|---|
| `recommended_outreach_route` | Suggested way to reach the producer. |
| `recommended_primary_contact` | Best email/contact found on the NSO site. |
| `recommended_primary_contact_type` | What kind of contact (general, named officer, etc.). |
| `contact_confidence` | How confident we are in that contact. |
| `contact_validation_needed` | TRUE if the contact should be double-checked before use. |

### F. Geography (for the map)
| Column | Plain meaning |
|---|---|
| `map_country` | Clean single-country name for the map. **Blank for multi-country/regional rows.** Use this on maps, not `country`. |
| `iso3` | 3-letter country code (e.g. KEN, COL). |
| `is_single_country` | TRUE = one country (mappable); FALSE = covers several countries. |

### G. AI fields — the local LLM (Ollama) layer **(all AI; blank until the LLM run)**
| Column | Plain meaning |
|---|---|
| `gain_match_type` | **Example-level** match (the accurate one): `already_in_gain` / `new_example_existing_country` / `new_country`. Use this instead of `gain_flag`. |
| `matched_gain_id` | If already in GAIN, which GAIN example it matched. |
| `gain_match_confidence` | How sure the AI is about that match (high/medium/low). |
| `gain_match_reason` | One line explaining the AI's match decision. |
| `llm_counted` | **Did the AI confirm a displaced group was actually included/counted?** TRUE/FALSE — the single most useful inclusion signal. |
| `llm_relevance` | The AI's own 0–100 relevance score (a second opinion on `relevance_score`). |
| `llm_implementation_status` | implemented / planned / unclear. |
| `llm_lead_type` | The AI's read of who led it (country-led / partner-led / unclear). |
| `lead_agreement` | Does the AI's lead call agree with the rule-based one? `agree` / `DISAGREE`. Filter DISAGREE to find rows a human should check. |
| `llm_is_humanitarian` | The AI's read of official vs humanitarian (compare with `is_humanitarian`). |
| `llm_quote` | The **exact sentence** from the document that is the evidence — great for quick review. |
| `llm_confidence` | How confident the AI is overall. |
| `sem_similarity` | 0–1 score of how close this document is in meaning to known GAIN examples. Higher = more similar. |
| `is_llm_reviewed` | TRUE/FALSE — **was this row actually checked by the AI?** Only the most relevant ~110 rows are LLM-reviewed; the rest are FALSE (and their `llm_*` fields are blank **by design**, not by error). Use as a slicer so blank `llm_*` cells don't confuse readers, and as the honest denominator (e.g. "44 confirmed out of the LLM-reviewed rows"). |

---

## TABLE 2 — `dim_country` (one row per single country; for maps & country bars)
| Column | Plain meaning |
|---|---|
| `data_source` | "GAIN Web Scraping". |
| `country` / `map_country` | Clean country name (use `map_country` to relate to the fact table and the map). |
| `iso3` | 3-letter country code. |
| `is_single_country` | Always TRUE here (this table is only real single countries). |
| `records` | How many candidate documents found for this country. |
| `actionable` | How many are worth acting on (A/B/D). |
| `official_actionable` | How many are actionable **and** official statistics — the best map measure. |
| `humanitarian_records` | How many are humanitarian/operational rather than official. |
| `strong_nso_A` / `partner_unclear_B` / `evidence_lead_C` / `manual_review_D` / `suppressed_E` | Count of documents in each category A–E for this country. |
| `in_gain_already` | Count flagged as already in GAIN (country-level). |
| `not_yet_in_gain` | Count flagged as new to GAIN (country-level). |
| `top_score` | The highest `relevance_score` among this country's documents. |
| `has_outreach_contact` | TRUE if at least one usable contact was found. |
| `status` | The country's GAIN engagement status (from the priority matrix). |
| `priority` | Outreach priority for this country. |
| `n_responses` | Number of GAIN survey responses on record for the country. |
| `web_contact_confidence` | Confidence in the web-found contact for this country. |
| `llm_confirmed_inclusion` **(AI)** | How many documents the AI confirmed actually included a displaced group (`llm_counted` = TRUE). |
| `new_to_gain_examples` **(AI)** | How many documents are **example-level** new to GAIN (new example or new country). |
| `already_in_gain_examples` **(AI)** | How many documents the AI matched to an existing GAIN example. |

---

## TABLE 3 — `fact_contacts` (one row per suggested outreach contact)
| Column | Plain meaning |
|---|---|
| `data_source` | "GAIN Web Scraping". |
| `country` / `map_country` / `iso3` / `is_single_country` | Which country the contact is for (and map keys). |
| `title` | The document this contact is associated with. |
| `email` | The contact email found. |
| `contact_type` | Kind of contact (general inbox, named officer, etc.). |
| `contact_source` | Where the contact was found (which page). |
| `rank` | `primary` (best) or `secondary` (backup) contact. |

---

## TABLE 4 — `processing_status` (one row per website crawled; coverage tracking)
| Column | Plain meaning |
|---|---|
| `data_source` | "GAIN Web Scraping". |
| `country` / `domain` | The NSO and its website address. |
| `processing_status` | Whether the site was processed successfully. |
| `inventory_status` | Whether we could list the site's pages (sitemap/Common Crawl). |
| `text_extraction_status` | Whether we could read page/PDF text. |
| `contact_search_status` | Whether we searched the site for contacts. |
| `records_found` | Total pages/URLs inventoried for the site. |
| `candidate_records_found` | How many matched displacement keywords. |
| `high_priority_candidates` | How many strong candidates. |
| `manual_review_needed` | How many need a human look. |
| `top_candidates` | The best finds for the site. |
| `failure_reason` | If the site couldn't be processed, why (timeout, blocked, no sitemap…). |
| `last_attempt_date` | When the site was last crawled. |
| `next_action` | Suggested next step (e.g. manual check) for sites with no coverage. |
| `sitemap_urls` | Pages found via the site's sitemap. |
| `commoncrawl_urls` | Pages found via the Common Crawl web archive. |
| `total_inventory` | Total pages found (sitemap + Common Crawl). |
| `keyword_matches` | How many of those pages matched displacement keywords. |
| `coverage` | Overall coverage rating for the site (e.g. GOOD / PARTIAL / NONE). |
| `l2_queries_done` | How many search-engine queries were completed for this site. |

---

## Quick "which field do I use?" cheat sheet
- **Is it real evidence?** Start with `overall_comment`, then `relevance_score`, then (AI) `llm_counted`.
- **Already in GAIN?** Use **`gain_match_type`** (AI). Fall back to `in_gain_already` only if the LLM hasn't run.
- **Who leads it?** `candidate_lead_type`; check `lead_agreement = DISAGREE` for rows to review.
- **Official vs humanitarian?** `is_humanitarian` (and compare `llm_is_humanitarian`).
- **For the map?** `map_country` + `official_actionable` (size).
- **Who to contact?** `recommended_primary_contact` (+ the `fact_contacts` table).
- **The evidence in one line?** `llm_quote` (AI).
