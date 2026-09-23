# ==============================================================================
# GAIN WORKSTREAM 3 - SDG DATA RETRIEVAL & PRODUCTION
#
# SEPARATE from identification. Goes beyond "is the group included?" to the
# actual numbers: it downloads the underlying documents/datasets for identified
# examples (GAIN respondent links AND the discovered candidates), has the AI
# extract reported SDG indicator VALUES by population group, maps the gaps, and
# where microdata allows produces demonstration tabulations.
#
# Based on identification (GAIN examples or otherwise): reads the GAIN group
# roster links + evidence_flagged_*.csv. Run RUN_IDENTIFY.R first for the
# candidate side (GAIN-example side works without it).
#
# Notes:
#   * Harvest is network-heavy; classification uses the local LLM (LM Studio).
#   * Pilot tabulations need microdata you download MANUALLY under each study's
#     access terms - those steps skip cleanly if the data files are absent.
#   * GAIN_SDGDOCS_REGATE.R is a one-off cleanup, not part of the routine run.
# Dashboard table -> powerbi_export/sdg/.
# ==============================================================================

if (!length(list.files(pattern = "^evidence_flagged_.*\\.csv$")))
  message("NOTE: no evidence_flagged_* found - harvesting GAIN-example links only. Run RUN_IDENTIFY.R for the candidate side.")

# make the local-LLM helpers available to the classifier step
if (file.exists("shared/GAIN_OLLAMA_HELPERS.R")) suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))

run_step <- function(label, file) {
  if (!file.exists(file)) { message("\n-- SKIP (missing ", file, "): ", label); return(invisible(NA)) }
  message("\n========================================================")
  message("STEP: ", label, "  ->  ", file)
  message("========================================================")
  t0 <- Sys.time()
  ok <- tryCatch({ source(file, local = new.env()); TRUE },
                 error = function(e) { message("  !! FAILED: ", conditionMessage(e)); FALSE })
  message(sprintf("  [%s] %s in %.1f min", if (ok) "done" else "FAILED",
                  label, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  ok
}

message("GAIN SDG data-retrieval & production workstream")
run_step("Harvest   -> download docs/datasets for examples + candidates", "sdg/GAIN_SDGDOCS_HARVEST.R")
run_step("Classify  -> AI extracts SDG indicator datapoints (LM Studio)", "sdg/GAIN_SDGDOCS_CLASSIFY.R")
run_step("Gap matrix-> where are the gaps, which a tabulation could fill", "sdg/GAIN_SDGDOCS_GAP_MATRIX.R")
run_step("DDI fetch -> which microdata could support a tabulation",        "sdg/GAIN_SDGDOCS_DDI_FETCH.R")
run_step("Pilot     -> demonstration estimates (Cameroon; manual data)",   "sdg/GAIN_SDGDOCS_PILOT_TABULATE.R")
run_step("Pilot NGA -> demonstration estimates (Nigeria; manual data)",    "sdg/GAIN_SDGDOCS_PILOT_TABULATE_NGA.R")
run_step("Review    -> human QA sheet of extracted values",                "sdg/GAIN_SDGDOCS_REVIEW_SHEET.R")
run_step("Dashboard -> the one publishable table of SDG values",           "sdg/GAIN_SDGDOCS_DASHBOARD.R")

local({
  sub <- file.path("powerbi_export", "sdg")
  dir.create(sub, recursive = TRUE, showWarnings = FALSE)
  tabs <- c("WEB_GAIN_sdg_values.csv", "WEB_GAIN_sdg_gap_matrix.csv")
  for (t in tabs) { s <- file.path("powerbi_export", t)
    if (file.exists(s)) file.copy(s, file.path(sub, t), overwrite = TRUE) }
  message("\nSDG dashboard table -> ", sub, "/")
})
message("\nSDG WORKSTREAM DONE")
