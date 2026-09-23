# ==============================================================================
# GAIN WORKSTREAM 1 - IDENTIFICATION  (find + score + AI-read + GAIN-match)
#
# This is the identification engine only. It produces the combined verdict and
# the reach-out shortlist. The other two workstreams are SEPARATE run files that
# consume this one's output:
#   * RUN_REACHOUT.R - contact NSOs about final_tier == "reach out"
#   * RUN_SDG.R      - retrieve documents + produce SDG indicator values
# Dashboard tables for this workstream are copied to powerbi_export/identification/.
#
# HOW TO USE
#   1. Open GAIN_WebScraping.Rproj (so R starts in this folder + loads the keys)
#   2. source("RUN_IDENTIFY.R")   (code lives in identify/ + shared/; data stays in this folder)
#
# Modes (toggle below):
#   DO_SCRAPE = TRUE  -> full refresh incl. new web/catalog data (slow, hours)
#   DO_SCRAPE = FALSE -> just rebuild outputs from existing data with the latest
#                        scoring/tagging logic (fast, minutes - everything cached)
#
#   DO_LLM = TRUE  -> local Ollama enrichment: the semantic funnel (embedding +
#                    extract_evidence) AND the example-level GAIN match in the
#                    crossref. Needs Ollama up (qwen2.5:7b + bge-m3). First pass
#                    is slow (~3-5 h) then fully cached; everything uncertain
#                    lands in review_queue.csv.
#   DO_LLM = FALSE -> skip all local-LLM steps (the example-level match too).
#
# Each step is wrapped so one failure does not lose the others. Re-run any time;
# all layers are resumable and cached, so nothing is re-done unnecessarily.
# ==============================================================================

DO_SCRAPE <- TRUE     # set FALSE to skip Layers 1-3 (re-score existing data only)
DO_LLM    <- FALSE    # set TRUE for the local Ollama semantic + example-level layer

# --- LLM run knobs (only used when DO_LLM = TRUE; safe to leave as-is) ---------
Sys.setenv(GAIN_MAX_EXTRACTS      = "110")   # funnel: ~3 h first pass
Sys.setenv(GAIN_GATE_SIM          = "0.60")  # funnel: embedding-similarity cutoff
Sys.setenv(GAIN_MAX_ADJUDICATIONS = "450")   # crossref: example-match decisions (~+1.5 h)

# sanity: are we in the right folder?
if (!file.exists("identify/GAIN_ENRICH_EVIDENCE.R")) {
  stop("Run this from the GAIN 'R script' folder (open GAIN_WebScraping.Rproj first).")
}
# when LLM is off, tell the crossref to skip its example-level (Ollama) module
Sys.setenv(GAIN_SKIP_EXAMPLE_MATCH = if (DO_LLM) "0" else "1")

run_step <- function(label, file) {
  message("\n========================================================")
  message(paste("STEP:", label, "  ->", file))
  message("========================================================")
  t0 <- Sys.time()
  ok <- tryCatch({ source(file, local = new.env()); TRUE },
                 error = function(e) { message("  !! FAILED: ", conditionMessage(e)); FALSE })
  message(sprintf("  [%s] %s in %.1f min", if (ok) "done" else "FAILED",
                  label, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  ok
}

message("GAIN pipeline run - mode: ",
        if (DO_SCRAPE) "FULL REFRESH (with scraping)" else "RE-SCORE ONLY (no scraping)")

if (DO_SCRAPE) {
  # --- gather new evidence (network; slow; resumable) ---
  run_step("Layer 1a  catalogs (IHSN/UNHCR/WB/ReliefWeb + DHS/MICS)", "identify/GAIN_LAYER1_CATALOGS_5.R")
  run_step("Layer 1b  refine / tier",                                 "identify/GAIN_LAYER1_REFINE_5.R")
  run_step("Layer 2   search engines (Brave/Google, resumable)",      "identify/GAIN_LAYER2_SEARCH_API_5.R")
  run_step("Layer 3   sitemaps + Common Crawl + news + MICS/DHS",     "identify/GAIN_LAYER3_SITEMAPS_CC.R")
  # Layer 1.5: structured statistical APIs (Eurostat SDMX, no key). Clean,
  # producer-known official statistics for countries that expose an API; flows
  # into the merge as STRUCT:* records. See NOTE_SEARCH_ARCHITECTURE.md.
  if (file.exists("identify/GAIN_LAYER1_STRUCTURED.R"))
    run_step("Layer 1.5 structured APIs (Eurostat SDMX, no-key)",     "identify/GAIN_LAYER1_STRUCTURED.R")
}

# --- build the master + evidence outputs (fast; cached) ---
run_step("Merge     -> GAIN_MASTER_REFERENCE", "identify/GAIN_MERGE_MASTER_REFERENCE.R")
run_step("Enrich    -> evidence + report + scoring/tagging", "identify/GAIN_ENRICH_EVIDENCE.R")

if (DO_LLM) {
  # local Ollama: semantic funnel (embedding + extract_evidence on gated survivors).
  # The example-level GAIN match runs inside the crossref step below.
  if (file.exists("shared/GAIN_OLLAMA_HELPERS.R")) {
    suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
    if (!ollama_available())
      message("\n  NOTE: DO_LLM=TRUE but Ollama is not reachable - LLM steps will no-op/skip.")
  }
  run_step("Semantic funnel -> embeddings + LLM extract (gated)", "identify/GAIN_SEMANTIC_FUNNEL.R")
}

# --- cross-reference with GAIN (+ example-level match when DO_LLM) + dashboard ---
run_step("Crossref  -> evidence_flagged / review_queue / priority / contacts", "identify/GAIN_PHASE5_CROSSREF.R")
run_step("Finalize  -> combined verdict + reach-out shortlist + dashboard table", "identify/GAIN_FINALIZE.R")
run_step("Power BI  -> powerbi_export/ pack",                   "identify/GAIN_POWERBI_EXPORT.R")
# Displacement-CONTEXT dimension (UNICEF SDMX: IDP magnitudes per country, IDMC-
# sourced). Context only, not evidence; relates to dim_country in Power BI.
if (file.exists("identify/GAIN_SDMX_DISPLACEMENT.R"))
  run_step("SDMX context -> WEB_GAIN_displacement_context",      "identify/GAIN_SDMX_DISPLACEMENT.R")

message("\n========================================================")
message("PIPELINE COMPLETE. Newest outputs in this folder:")
message("  GAIN_EVIDENCE_REPORT_*.html   (open in a browser to review)")
message("  GAIN_EVIDENCE_ENRICHED_*.csv  (full data backbone)")
if (DO_LLM) {
  message("  GAIN_EVIDENCE_ENRICHED_*_SEM.csv (with sem_similarity + llm_* fields)")
  message("  review_queue_*.csv            (new examples + low-confidence matches to check)")
}
message("  evidence_flagged_* / country_priority_matrix_* / contact_gaps_* / suggested_respondents_*")
message("  powerbi_export/               (connect Power BI to this folder)")
if (DO_LLM) message("  ollama_decisions_log.csv      (every LLM classification decision)")
message("========================================================")

# --- collect THIS workstream's dashboard tables into its own subfolder ---------
# (copies, not moves: the canonical files stay in powerbi_export/ so nothing
# else breaks; Power BI can point at the per-workstream folder for clean reporting)
local({
  sub <- file.path("powerbi_export", "identification")
  dir.create(sub, recursive = TRUE, showWarnings = FALSE)
  tabs <- c("WEB_GAIN_fact_candidates.csv", "WEB_GAIN_dim_country.csv",
            "WEB_GAIN_fact_contacts.csv", "WEB_GAIN_processing_status.csv",
            "WEB_GAIN_final.csv", "WEB_GAIN_displacement_context.csv")
  for (t in tabs) { s <- file.path("powerbi_export", t)
    if (file.exists(s)) file.copy(s, file.path(sub, t), overwrite = TRUE) }
  message("  identification dashboard tables -> ", sub, "/")
})
