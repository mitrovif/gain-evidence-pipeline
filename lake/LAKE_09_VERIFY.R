# ==============================================================================
# GAIN DATA LAKE - step 7: VERIFY the scrape-recovered dataset attachments.
#
# 36 examples had no usable roster link, so the recovery step guessed a URL by
# cross-matching to the old web crawl. That mis-attaches datasets (a census stapled
# to a displacement survey, one UNHCR study reused for three examples). This step
# asks the local LLM, per example, whether the recovered resource is REALLY the same
# statistical activity as the GAIN roster example. Keep MATCH only; everything else
# (rejected recoveries + roster examples with no link) goes to a to-source list
# classified as "search public availability" vs "reach out to the reporting office".
#
# Roster-OWN links are trusted (the reporting office declared them) and not re-judged.
# ==============================================================================
suppressMessages({ library(tidyverse) })
source("shared/GAIN_CONFIG.R"); source("shared/GAIN_COMMON.R"); suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("LM Studio not reachable.")
LAKE <- "data_lake"; STORE <- file.path(LAKE, "store")
roster <- suppressMessages(read_csv(file.path(LAKE, "lake_roster.csv"), show_col_types = FALSE))
rec    <- suppressMessages(read_csv(file.path(LAKE, "lake_recovery.csv"), show_col_types = FALSE)) %>% filter(recovery != "none")
api    <- suppressMessages(read_csv(file.path(LAKE, "lake_api_manifest.csv"), show_col_types = FALSE)) %>% filter(status == "variables")

# candidate description = harvested microdata study title (best) else the page's text head
cand_desc <- function(ex, url) {
  h <- api %>% filter(example_id == ex)
  if (nrow(h)) return(sprintf("microdata study titled '%s' hosted on %s", h$title[1], h$host[1]))
  tf <- file.path(STORE, ex, "text.txt")
  snip <- if (file.exists(tf)) str_squish(substr(paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = " "), 1, 700)) else ""
  sprintf("web page at %s. Page text begins: %s", sub("^https?://([^/]+).*", "\\1", url), substr(snip, 1, 500))
}

verify_one <- function(ex, url) {
  r <- roster %>% filter(example_id == ex) %>% slice(1)
  prompt <- paste0(
    "A GAIN statistical example (reported by an office) is described, and a CANDIDATE data source we found for it. ",
    "Decide if the candidate is the SAME statistical activity as the example: same producing organisation, same ",
    "survey/census/report, same population focus. A different survey by the same agency, or a generic census when the ",
    "example is a specific survey, is a MISMATCH.\n\n",
    "GAIN EXAMPLE:\n  organisation: ", r$organisation, "\n  title: ", r$title,
    "\n  populations: ", r$populations, "\n  year: ", coalesce(as.character(r$year), "?"),
    "\n\nCANDIDATE SOURCE:\n  ", cand_desc(ex, url),
    "\n\nAnswer in exactly two lines:\nVERDICT: MATCH or MISMATCH or UNSURE\nREASON: one short sentence.")
  raw <- .ollama_generate(prompt, timeout = 120, json = FALSE)
  v <- toupper(str_match(coalesce(raw, ""), "VERDICT[:\\s]*\\s*(MATCH|MISMATCH|UNSURE)")[, 2])
  reason <- str_squish(sub(".*REASON[:\\s]*", "", coalesce(raw, "")))
  list(verdict = coalesce(v, "UNSURE"), reason = substr(reason, 1, 160))
}

message(sprintf("LLM-verifying %d recovered attachments ...", nrow(rec)))
res <- map2(rec$example_id, rec$recovered_url, verify_one)
rec <- rec %>% mutate(verdict = map_chr(res, "verdict"), reason = map_chr(res, "reason"),
                      kept = verdict == "MATCH")
readr::write_excel_csv(rec %>% select(example_id, country, organisation, title, recovered_url, scrape_score, verdict, reason, kept),
                       file.path(LAKE, "lake_recovery_verified.csv"))
message("\nverification verdicts:"); print(count(rec, verdict))

# ---- dataset provenance per fetched example ----------------------------------
man <- suppressMessages(read_csv(file.path(LAKE, "lake_store_manifest.csv"), show_col_types = FALSE)) %>% filter(nzchar(saved))
prov <- man %>% distinct(example_id) %>% mutate(
  provenance = case_when(
    example_id %in% rec$example_id[rec$kept]  ~ "recovered + LLM-verified",
    example_id %in% rec$example_id[!rec$kept] ~ "recovered - REJECTED (removed)",
    TRUE ~ "roster own link (trusted)"))
readr::write_excel_csv(prov, file.path(LAKE, "lake_dataset_provenance.csv"))
trusted_ids <- prov$example_id[prov$provenance != "recovered - REJECTED (removed)"]

# ---- corrected microdata-computable view -------------------------------------
enr <- suppressMessages(read_csv(file.path(LAKE, "lake_frame_enriched.csv"), show_col_types = FALSE))
micro <- enr %>% filter(str_detect(id_capability_enriched, "microdata")) %>%
  mutate(status = ifelse(example_id %in% trusted_ids, "KEPT (trusted/verified)", "DROPPED (bad attachment)"))
message("\ncorrected microdata-computable examples:"); print(micro %>% count(status))

# ---- to-source list: examples with NO trusted dataset ------------------------
have_trusted <- union(trusted_ids, character(0))
to_source <- roster %>% filter(!example_id %in% have_trusted) %>%
  left_join(rec %>% select(example_id, verdict), by = "example_id") %>%
  mutate(roster_link = ifelse(has_link, "roster link present but not usable/dead", "no link in GAIN record"),
         suggested_action = case_when(
           lead_type == "country-led" ~ "search public availability (NSO site / statbank), then reach out",
           TRUE ~ "reach out to reporting office (likely gated microdata)")) %>%
  transmute(example_id, year, country, organisation, lead_type, title, populations,
            recommendations, is_microdata_capable, recovered_verdict = coalesce(verdict, NA),
            roster_link, suggested_action) %>% arrange(desc(is_microdata_capable), country)
readr::write_excel_csv(to_source, file.path(LAKE, "lake_to_source.csv"))

message(sprintf("\n==== corrected lake ===="))
message(sprintf("recovered attachments: %d MATCH kept, %d dropped", sum(rec$kept), sum(!rec$kept)))
message(sprintf("examples with a TRUSTED dataset (roster-own + verified): %d", length(trusted_ids)))
message(sprintf("examples to source (search public / reach out): %d", nrow(to_source)))
print(to_source %>% count(suggested_action))
