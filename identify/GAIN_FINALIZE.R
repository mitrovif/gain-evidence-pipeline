# ==============================================================================
# GAIN FINALIZE  (one combined verdict from the two judgements)
#
# The web-scraping keyword score and the AI (LLM) read disagree often
# (correlation ~0.4; when they differ it is ~88:1 "AI caught what keyword
# missed"). This collapses them into ONE verdict per candidate so there is a
# single thing to sort, dashboard and reach out on - no more "which score?".
#
#   final_score (0-100)  AI-LED: the AI's relevance where it read the document;
#                        keyword score only as fallback for un-read rows.
#   final_tier           the shortlist / outreach indicator (see bands below).
#   final_confidence     high/medium/low - LOW when the two signals disagree
#                        (those deserve a human glance before acting).
#   final_reason         one line, incl. the matched GAIN pindex2 when in-GAIN.
#
# Inputs : newest evidence_flagged_*.csv (carries llm_*, keyword score, and the
#          GAIN match incl. matched_gain_pindex2 / matched_gain_title).
# Outputs: GAIN_EVIDENCE_FINAL_[date].csv          (full, with final_* columns)
#          powerbi_export/WEB_GAIN_final.csv        (dashboard table)
#          GAIN_REACHOUT_SHORTLIST_[date].csv       ("reach out" tier, deduped)
# Additive: reads a crossref output, writes new files. Nothing upstream changes.
# ==============================================================================
suppressMessages({ library(tidyverse) })

REACH_MIN <- as.numeric(Sys.getenv("GAIN_REACH_MIN", "70"))   # focused bar

ef_file <- tail(sort(list.files(".", "^evidence_flagged_.*\\.csv$")), 1)
stopifnot(length(ef_file) == 1)
message("Finalizing from: ", ef_file)
d <- suppressMessages(read_csv(ef_file, show_col_types = FALSE))

pk  <- function(col) if (col %in% names(d)) d[[col]] else rep(NA, nrow(d))
num <- function(x) suppressWarnings(as.numeric(x))
tru <- function(x) tolower(as.character(x)) %in% c("true", "1", "yes")
first_words <- function(x, k = 4) vapply(strsplit(str_squish(coalesce(as.character(x), "")), " "),
  function(w) paste(head(w, k), collapse = " "), character(1))

kw      <- num(pk("relevance_score"))
llm     <- num(pk("llm_relevance"))
read    <- tru(pk("llm_extracted"))
counted <- tru(pk("llm_counted"))
lead    <- tolower(coalesce(as.character(pk("llm_lead_type")), "unclear"))
humanit <- tru(pk("llm_is_humanitarian"))
gm      <- as.character(pk("gain_match_type"))
gconf   <- tolower(coalesce(as.character(pk("gain_match_confidence")), ""))
pidx    <- as.character(pk("matched_gain_pindex2"))
gtitle  <- as.character(pk("matched_gain_title"))

# ---- final_score : AI-led ----------------------------------------------------
final_score <- ifelse(read, llm, kw)
final_score <- pmax(0, pmin(100, round(coalesce(final_score, 0))))

# ---- agreement / confidence --------------------------------------------------
disagree <- read & ((coalesce(kw, 0) < 40 & coalesce(llm, 0) >= 70) |
                    (coalesce(kw, 0) >= 70 & coalesce(llm, 0) < 40))
llmconf  <- tolower(coalesce(as.character(pk("llm_confidence")), "low"))
final_confidence <- ifelse(!read, "unverified (keyword only)",
                    ifelse(disagree, "low - signals disagree",
                    ifelse(llmconf %in% c("high", "medium", "low"), llmconf, "low")))

# ---- final_tier : the shortlist / outreach indicator -------------------------
in_gain_solid <- gm == "already_in_gain" & gconf %in% c("high", "medium")
in_gain_weak  <- gm == "already_in_gain" & gconf == "low"
reach <- read & counted & final_score >= REACH_MIN & lead == "country-led" & !humanit

final_tier <- ifelse(!read, "not assessed",
              ifelse(in_gain_solid, "already in GAIN",
              ifelse(in_gain_weak, "review - possible GAIN match",
              ifelse(reach, "reach out",
              ifelse(counted & final_score >= 50, "review then reach out",
              ifelse(final_score >= 40, "watch / context", "low"))))))

# ---- final_reason ------------------------------------------------------------
kwnote <- ifelse(disagree, " (keyword under-rated)", "")
final_reason <- case_when(
  in_gain_solid | in_gain_weak ~ sprintf("matches GAIN pindex2 %s - %s (%s conf): %s",
      coalesce(pidx, "?"), substr(coalesce(gtitle, ""), 1, 60), gconf,
      substr(coalesce(as.character(pk("gain_match_reason")), ""), 1, 80)),
  final_tier == "reach out" ~ sprintf("AI: counted (%s), country-led, rel %d%s",
      substr(coalesce(as.character(pk("llm_population")), ""), 1, 30), final_score, kwnote),
  read ~ sprintf("AI rel %d, %s%s%s", final_score,
      ifelse(counted, "counted", "not counted"),
      ifelse(humanit, ", humanitarian-data", ""), kwnote),
  TRUE ~ "not AI-read - keyword score only")

# ---- carry everything onto the frame (full-length, aligned) ------------------
org_disp  <- coalesce(as.character(pk("llm_organization")), as.character(pk("producer")))
inst_disp <- coalesce(as.character(pk("llm_instrument_or_title")), as.character(pk("title")))
series_key <- str_squish(tolower(paste(coalesce(as.character(pk("country")), ""),
                 coalesce(org_disp, ""), first_words(inst_disp, 4))))

out <- d %>% mutate(
  final_score = final_score, final_tier = final_tier,
  final_confidence = final_confidence, final_reason = final_reason,
  final_series_key = series_key,
  disp_country = as.character(pk("country")), disp_org = org_disp,
  disp_instrument = inst_disp, disp_year = as.character(pk("llm_year")),
  disp_population = as.character(pk("llm_population")),
  disp_quote = as.character(pk("llm_quote")), disp_url = as.character(pk("url")),
  disp_counted = counted, disp_lead = lead, disp_humanit = humanit,
  disp_gm = gm, disp_pidx = pidx, disp_gtitle = gtitle)

out <- out %>% group_by(final_series_key) %>%
  mutate(is_series_primary = row_number() == which.max(final_score)) %>% ungroup()

today <- format(Sys.Date(), "%Y%m%d")
readr::write_excel_csv(out, sprintf("GAIN_EVIDENCE_FINAL_%s.csv", today))

# ---- dashboard table ---------------------------------------------------------
dash <- out %>% transmute(
  country = disp_country, organization = disp_org, instrument = disp_instrument,
  year = disp_year, final_score, final_tier, final_confidence,
  counted = disp_counted, lead_type = disp_lead, humanitarian_flag = disp_humanit,
  gain_match_type = disp_gm, matched_gain_pindex2 = disp_pidx,
  matched_gain_title = disp_gtitle, final_reason, is_series_primary, url = disp_url)
dir.create("powerbi_export", showWarnings = FALSE)
readr::write_excel_csv(dash, "powerbi_export/WEB_GAIN_final.csv")

# ---- reach-out shortlist -----------------------------------------------------
short <- out %>% filter(final_tier == "reach out") %>%
  transmute(country = disp_country, organization = disp_org, instrument = disp_instrument,
            year = disp_year, final_score, final_confidence, population = disp_population,
            evidence_quote = disp_quote, url = disp_url,
            is_series_primary, final_series_key) %>%
  arrange(desc(final_score))
readr::write_excel_csv(short, sprintf("GAIN_REACHOUT_SHORTLIST_%s.csv", today))

# ---- report ------------------------------------------------------------------
message("\n==== final_tier counts ====")
print(out %>% count(final_tier, sort = TRUE))
message(sprintf("\nreach-out shortlist: %d rows (%d one-per-instrument)",
        nrow(short), sum(short$is_series_primary, na.rm = TRUE)))
message("wrote: GAIN_EVIDENCE_FINAL_", today, ".csv | powerbi_export/WEB_GAIN_final.csv | GAIN_REACHOUT_SHORTLIST_", today, ".csv")
