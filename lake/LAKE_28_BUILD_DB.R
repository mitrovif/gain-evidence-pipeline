# ==============================================================================
# GAIN DATA LAKE - BUILD THE DATABASE.
#
# The durable archive of everything reported to GAIN. Each example is referenced
# by its GAIN submission id (pindex2) plus a within-submission number, because one
# pindex2 (one organisation's questionnaire response) bundles several examples.
#   ref = <pindex2>_<nn>     e.g. 20211143_01
# Builds gain_database/ : one folder per example holding a human-readable record
# and every data file / report we have secured, plus a master index keyed by ref.
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"; STORE <- file.path(LAKE,"store"); DB <- file.path(LAKE,"gain_database")
dir.create(DB, showWarnings=FALSE, recursive=TRUE)
arch <- suppressMessages(read_csv(file.path(LAKE,"gain_archive_full.csv"), show_col_types=FALSE))

# ---- reference key: pindex2 + within-submission sequence (stable, by example_id order) ----
arch <- arch %>% group_by(gain_pindex2) %>% arrange(example_id, .by_group=TRUE) %>%
  mutate(sub_n = row_number(), ref = sprintf("%s_%02d", gain_pindex2, sub_n)) %>% ungroup() %>%
  arrange(ref)

esc <- function(x) ifelse(is.na(x),"",as.character(x))
n_files <- 0L
for (i in seq_len(nrow(arch))) {
  a <- arch[i,]; d <- file.path(DB, a$ref); dir.create(d, showWarnings=FALSE)
  # copy every secured file from the working store into the archive folder
  src <- file.path(STORE, a$example_id)
  if (dir.exists(src)) { fs <- list.files(src, full.names=TRUE, recursive=FALSE)
    for (f in fs) if (file.info(f)$isdir %in% c(FALSE,NA)) { file.copy(f, file.path(d, basename(f)), overwrite=TRUE); n_files <- n_files+1L } }
  held <- list.files(d)
  # human-readable record
  rec <- c(
    sprintf("# GAIN example %s", a$ref),
    "",
    sprintf("- **GAIN submission (pindex2):** %s", esc(a$gain_pindex2)),
    sprintf("- **Reference:** %s   (internal id %s)", a$ref, a$example_id),
    sprintf("- **Round / year:** %s", esc(a$year)),
    sprintf("- **Country:** %s", esc(a$country)),
    sprintf("- **Organisation:** %s", esc(a$organisation)),
    sprintf("- **Title:** %s", esc(a$title)),
    sprintf("- **Populations:** %s", esc(a$populations)),
    sprintf("- **Recommendations:** %s", esc(a$recommendations)),
    sprintf("- **Identification framework:** %s", esc(a$id_framework)),
    sprintf("- **SDG domains:** %s", esc(a$sdg_domains)),
    sprintf("- **Host-community signal:** %s", esc(a$host_signal)),
    sprintf("- **Phase:** %s", esc(a$phase)),
    sprintf("- **Acquisition route:** %s", esc(a$route)),
    sprintf("- **Confirmed dataset:** %s", esc(a$confirmed_dataset)),
    sprintf("- **Unverified NADA candidate attached (needs checking):** %s", esc(a$unverified_candidate)),
    "",
    "## Description", esc(a$description), "",
    "## Source links", esc(a$all_urls), "",
    "## Files held in this record",
    if (length(held)) paste0("- ", held) else "- (none yet - see acquisition route)")
  writeLines(rec, file.path(d, "record.md"))
}

# ---- master index keyed by ref ----
idx <- arch %>% mutate(n_files = map_int(ref, ~length(list.files(file.path(DB,.x))) - 1L)) %>%
  transmute(ref, gain_pindex2, example_id, year, country, organisation,
            title=substr(title,1,80), populations, recommendations, id_framework, sdg_domains,
            host_signal, route, confirmed_dataset, unverified_candidate, n_files, folder=file.path("gain_database",ref))
readr::write_excel_csv(idx, file.path(DB,"gain_index.csv"))
readr::write_excel_csv(idx, file.path(LAKE,"gain_index.csv"))

writeLines(c(
  "# GAIN DATABASE",
  "",
  "Durable archive of every example reported to the GAIN survey (all rounds).",
  "",
  "- One folder per example, named `<pindex2>_<nn>` where **pindex2** is the GAIN",
  "  submission id and `nn` is the example's number within that submission.",
  "- Each folder holds `record.md` (the full GAIN record) plus any data files and",
  "  reports secured for it.",
  "- `gain_index.csv` is the master table, keyed by `ref` (= folder name).",
  "",
  "Acquisition routes: CONFIRMED dataset / data in hand / extract-from-report /",
  "fetch-then-extract / reach-out. Where we already have data or access, no",
  "reach-out is needed."), file.path(DB,"README.md"))

message(sprintf("==== GAIN DATABASE built: %d example records under %s ====", nrow(arch), DB))
message(sprintf("files copied into the archive: %d", n_files))
message(sprintf("records with >=1 data/report file: %d | metadata-only (pending): %d",
        sum(idx$n_files>0), sum(idx$n_files==0)))
message(sprintf("distinct pindex2 submissions: %d", n_distinct(arch$gain_pindex2)))
message("\nby acquisition route:"); print(count(idx, route, sort=TRUE))
message("\nsample refs:"); print(head(idx %>% select(ref, country, title, n_files, route), 6) %>% as.data.frame(), right=FALSE)
