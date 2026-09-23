# ==============================================================================
# GAIN TIDY FOLDER  (housekeeping - run any time the root gets cluttered)
#
# Every pipeline run writes dated output files (evidence_flagged_YYYYMMDD.csv,
# GAIN_EVIDENCE_ENRICHED_YYYYMMDD.csv, ...). Only the NEWEST of each family is
# ever read - every script picks its input by newest modification time - so the
# older generations just pile up in the root. This script MOVES (never deletes)
# the superseded ones into archive/, keeping the newest KEEP_N of each family
# in the root.
#
# SAFE BY CONSTRUCTION:
#   * moves only files matching the explicit dated-family patterns below,
#     plus the explicit OBSOLETE list - everything else is untouched
#   * scripts read inputs with non-recursive list.files(), so files in
#     archive/ are invisible to the pipeline (that is the point)
#   * never touches: caches, powerbi_export/, scripts, docs, stable-name data
#     (GAIN_OUTREACH_LOG.csv, LAYER2_progress.csv, rosters, registries, xlsx)
#   * DRY RUN by default - set TIDY_APPLY <- TRUE to actually move files
# ==============================================================================

# dry-run by default; apply with:  Sys.setenv(GAIN_TIDY_APPLY = "1"); source("shared/GAIN_TIDY_FOLDER.R")
TIDY_APPLY <- identical(Sys.getenv("GAIN_TIDY_APPLY"), "1")
KEEP_N     <- 1       # newest N of each dated family stay in the root
ARCHIVE    <- "archive"

# dated output families ("newest wins" applies within each pattern separately;
# note base ENRICHED and _SEM are SEPARATE families - the crossref needs the
# newest of EACH, so both survivors stay in the root)
FAMILIES <- c(
  "^GAIN_MASTER_REFERENCE_\\d{8}\\.csv$",
  "^GAIN_EVIDENCE_ENRICHED_\\d{8}\\.csv$",
  "^GAIN_EVIDENCE_ENRICHED_\\d{8}_SEM\\.csv$",
  "^GAIN_EVIDENCE_REPORT_\\d{8}\\.html$",
  "^LAYER1_catalog_records_\\d{8}\\.csv$",
  "^LAYER1_refined_\\d{8}\\.csv$",
  "^LAYER2_search_hits_\\d{8}\\.csv$",
  "^LAYER3_url_inventory_\\d{8}\\.csv$",
  "^LAYER3_inventory_stats_\\d{8}\\.csv$",
  "^evidence_flagged_\\d{8}(_\\d{6})?\\.csv$",     # incl. _HHMMSS lock-fallback copies
  "^country_priority_matrix_\\d{8}\\.csv$",
  "^contact_gaps_\\d{8}\\.csv$",
  "^suggested_respondents_\\d{8}\\.csv$",
  "^review_queue_\\d{8}\\.csv$",
  "^outreach_targets_\\d{8}\\.csv$",
  "^outreach_emails_\\d{8}\\.csv$",
  "^outreach_contact_gaps_\\d{8}\\.csv$",
  "^outreach_followups_\\d{8}\\.csv$",
  "^GAIN_CYCLE_TRACKER_\\d{8}\\.csv$",             # stable GAIN_CYCLE_TRACKER.csv untouched
  "^displacement_context_\\d{8}\\.csv$",
  "^structured_discovery_\\d{8}\\.csv$",
  "^sdmx_nso_sourced_displacement_\\d{8}\\.csv$",
  "^keyword_suggestions_\\d{8}\\.csv$",
  "^ollama_decisions_log_\\d{14}\\.csv$"           # rotated-aside old decision logs
)

# retired scripts / stale one-off logs - archived regardless of date
# (verified referenced by NOTHING: RUN_ALL sources the funnel, not LLM_SCREEN)
OBSOLETE <- c("GAIN_LLM_SCREEN.R", "test_gain_v6_funcs.R", "tmp_key_smoke.R",
              "RERUN_log.txt", "RERUN2_log.txt", "RUN_ALL_log.txt")

# ------------------------------------------------------------------------------
if (!file.exists("GAIN_WebScraping.Rproj"))
  stop("Run this from the R script project folder (open GAIN_WebScraping.Rproj first).")

to_move <- character(0)
for (pat in FAMILIES) {
  f <- list.files(pattern = pat)
  if (length(f) <= KEEP_N) next
  keep <- f[order(file.info(f)$mtime, decreasing = TRUE)][seq_len(KEEP_N)]
  to_move <- c(to_move, setdiff(f, keep))
}
to_move <- c(to_move, OBSOLETE[file.exists(OBSOLETE)])
to_move <- unique(to_move)

if (length(to_move) == 0) {
  message("Nothing to tidy - the root only holds current files.")
} else {
  sizes <- file.info(to_move)$size
  message(sprintf("%s %d file(s), %.1f MB total, -> %s/",
                  if (TIDY_APPLY) "Moving" else "WOULD move", length(to_move),
                  sum(sizes, na.rm = TRUE) / 1e6, ARCHIVE))
  for (f in to_move[order(to_move)]) message("  ", f)
  if (TIDY_APPLY) {
    dir.create(ARCHIVE, showWarnings = FALSE)
    ok <- vapply(to_move, function(f) file.rename(f, file.path(ARCHIVE, f)), logical(1))
    if (any(!ok)) message("COULD NOT move (locked? open in Excel?): ",
                          paste(to_move[!ok], collapse = ", "))
    message(sprintf("Done: %d moved, %d failed. Root now has %d files.",
                    sum(ok), sum(!ok), length(list.files(all.files = FALSE)) -
                      length(list.dirs(recursive = FALSE))))
  } else {
    message("\nDry run only. Set TIDY_APPLY <- TRUE at the top and re-run to apply.")
  }
}
