# ==============================================================================
# GAIN FINALIZE  (one verdict per candidate: tier + route + GAIN match + SDG flag)
#
# The web-scraping keyword score and the AI (LLM) read disagree often
# (correlation ~0.4), so the AI leads and the keyword score is only a fallback.
#
#   final_score (0-100)  AI relevance where it read the document; keyword
#                        score only for rows the AI did not read.
#   final_tier           what to DO with it (bands below).
#   route                WHO to ask:  NSO (direct)  |  regional body (Eurostat,
#                        UN regional commissions, ...)  |  international agency
#                        (UNHCR, IOM, WB/JDC, ...)  |  other / non-statistical.
#                        NSOs are contacted directly; for the others we send a
#                        list and ask them to reach out on our behalf.
#   final_confidence     high/medium/low - LOW when the two signals disagree.
#   final_reason         one line of why.
#   use_for_sdg          data source the SDG workstream should look at
#                        (independent of the tier - a census already in GAIN can
#                        still feed SDG indicators).
#
# Tiers (in order of precedence):
#   junk - login/redirect page      URL is a sign-in / SSO page, not a document
#   already in GAIN                 same product as a GAIN example (GAIN_MATCH_V2)
#   historic - context only         reference year before HISTORIC_BEFORE
#   could not read - check by hand  document text was not retrievable and the
#                                   AI saw only a snippet: a low score means
#                                   "unknown", not "irrelevant"
#   reach out                       AI: displaced people counted, score >= REACH_MIN
#   (questionnaire check: displacement/statelessness question found -> reach out)
#   review then reach out           counted, score >= 50; or a new edition / new
#                                   output of a GAIN example (relevant by definition)
#   unsure - check questionnaire    the AI's default 40 / not counted: it found no
#                                   evidence either way (often a catalogue page -
#                                   the questionnaire decides)
#   watch / context                 score >= 40
#   low
#
# Rules from the Sep 2026 reviewer calibration (review/GAIN_REVIEW_20.xlsx):
#   - a report/table set built on a GAIN survey (new output) is a NEW example
#   - a new edition of a GAIN example is worth asking for (update)
#   - Eurostat tables are ONE regional example per dataset, not a country one
#   - data older than HISTORIC_BEFORE is not asked for
#   - UNHCR operational surveys (RMS/MSNA/PDM) are agency work, not NSO work
#
# Inputs : newest GAIN_MATCH_V2_*.csv (crossref output + match_v2_* columns);
#          falls back to evidence_flagged_*.csv (old example-level match only)
# Outputs: GAIN_EVIDENCE_FINAL_[date].csv          (full, with final_* columns)
#          powerbi_export/WEB_GAIN_final.csv        (dashboard table)
#          GAIN_REACHOUT_SHORTLIST_[date].csv       (reach out + review then reach out)
#          GAIN_SDG_HANDOFF_[date].csv              (use_for_sdg rows, for RUN_SDG)
# ==============================================================================
suppressMessages({ library(tidyverse) })
source("shared/GAIN_COMMON.R")

REACH_MIN       <- as.numeric(Sys.getenv("GAIN_REACH_MIN", "70"))
HISTORIC_BEFORE <- as.integer(Sys.getenv("GAIN_HISTORIC_BEFORE", "2020"))

ef_file <- tail(sort(list.files(".", "^evidence_flagged_.*\\.csv$")), 1)
mv_file <- tail(sort(list.files(".", "^GAIN_MATCH_V2_.*\\.csv$")), 1)
stamp_of <- function(f) str_extract(f, "\\d{8}")
use_v2 <- length(mv_file) == 1 && length(ef_file) == 1 && stamp_of(mv_file) >= stamp_of(ef_file)
in_file <- if (use_v2) mv_file else ef_file
stopifnot(length(in_file) == 1)
if (!use_v2) message("NOTE: no GAIN_MATCH_V2 file as new as ", ef_file,
                     " - run identify/GAIN_MATCH_V2.R first; using the old example-level match.")
message("Finalizing from: ", in_file)
d <- suppressMessages(read_csv(in_file, show_col_types = FALSE, guess_max = 5000))

pk  <- function(col) if (col %in% names(d)) d[[col]] else rep(NA, nrow(d))
num <- function(x) suppressWarnings(as.numeric(x))
tru <- function(x) tolower(as.character(x)) %in% c("true", "1", "yes")
chr <- function(x) coalesce(as.character(x), "")
low <- function(x) tolower(chr(x))
first_words <- function(x, k = 4) vapply(strsplit(str_squish(chr(x)), " "),
  function(w) paste(head(w, k), collapse = " "), character(1))

kw      <- num(pk("relevance_score"))
llm     <- num(pk("llm_relevance"))
read    <- tru(pk("llm_extracted"))
counted <- tru(pk("llm_counted"))
got_txt <- tru(pk("text_extracted"))
lead    <- low(pk("llm_lead_type")); lead[lead == ""] <- "unclear"
humanit <- tru(pk("llm_is_humanitarian"))
url     <- chr(pk("url"))
org_s   <- coalesce(as.character(pk("llm_organization")), as.character(pk("producer")))
title_s <- coalesce(as.character(pk("llm_instrument_or_title")), as.character(pk("title")))
year_n  <- num(coalesce(str_extract(chr(pk("llm_year")), "(19|20)[0-9]{2}"),
                        str_extract(chr(pk("pub_year")), "(19|20)[0-9]{2}")))

# ---- GAIN match: v2 categories when available, else the old crossref -------
v2cat <- if (use_v2) chr(pk("match_v2_category")) else rep("", nrow(d))
gm    <- chr(pk("gain_match_type"))
gconf <- low(pk("gain_match_confidence"))
in_gain <- if (use_v2) v2cat == "same product" else gm == "already_in_gain" & gconf %in% c("high", "medium")
maybe_in_gain <- if (use_v2) str_starts(v2cat, "check") else gm == "already_in_gain" & gconf == "low"
pidx   <- if (use_v2) chr(pk("match_v2_pindex2")) else chr(pk("matched_gain_pindex2"))
gidx   <- if (use_v2) chr(pk("match_v2_gain_index")) else chr(pk("matched_gain_id"))
gtitle <- if (use_v2) chr(pk("match_v2_gain_title")) else chr(pk("matched_gain_title"))
eurostat <- if (use_v2) v2cat == "regional (Eurostat)" | chr(pk("match_v2_eurostat_dataset")) != "" else
            str_detect(low(org_s), "eurostat") | str_detect(url, "ec\\.europa\\.eu/eurostat")

# ---- route: who do we ask? ---------------------------------------------------
REGIONAL_PAT <- paste0("eurostat|\\bescap\\b|\\bescwa\\b|\\beclac\\b|\\bcepal\\b|\\bunece\\b|",
  "economic commission for (africa|europe|latin)|\\buneca\\b|ecowas|cedeao|east african community|",
  "\\beac\\b|african union|afristat|gcc.?stat|sesric|pacific community|\\bspc\\b|\\bsiap\\b|asean|caricom|oecd")
AGENCY_PAT <- paste0("unhcr|acnur|un refugee agency|high commissioner for refugees|\\biom\\b|\\boim\\b|",
  "displacement tracking|\\bdtm\\b|world bank|joint data cent|\\bjdc\\b|\\bjips\\b|\\bidmc\\b|unicef|unfpa|",
  "undp|\\bwfp\\b|\\breach\\b|impact initiatives|\\bhdx\\b|\\bocha\\b|\\bnrc\\b|\\bdrc\\b danish")
OPERATIONAL_PAT <- "results monitoring survey|\\brms\\b|\\bmsna\\b|multi.?sector needs|post.?distribution|\\bpdm\\b|protection monitoring"
NSO_PAT <- "statisti|estad[ií]st|census|\\bine\\b|\\binsee\\b|\\bistat\\b|bureau of stat|national population commission|high commission for planning"
o <- low(org_s); t <- low(title_s)
nso_site <- on_nso_site(url, as.character(pk("country")))
route <- case_when(
  eurostat | str_detect(o, REGIONAL_PAT)                  ~ "regional body",
  nso_site                                                ~ "NSO (direct)",
  str_detect(t, OPERATIONAL_PAT) | str_detect(o, AGENCY_PAT) ~ "international agency",
  str_detect(o, NSO_PAT) | lead == "country-led"          ~ "NSO (direct)",
  lead == "partner-led"                                   ~ "international agency",
  TRUE                                                    ~ "other / non-statistical")

# ---- final_score : AI-led ----------------------------------------------------
final_score <- ifelse(read, llm, kw)
final_score <- pmax(0, pmin(100, round(coalesce(final_score, 0))))

# ---- agreement / confidence --------------------------------------------------
disagree <- read & ((coalesce(kw, 0) < 40 & coalesce(llm, 0) >= 70) |
                    (coalesce(kw, 0) >= 70 & coalesce(llm, 0) < 40))
llmconf  <- low(pk("llm_confidence")); llmconf[llmconf == ""] <- "low"
final_confidence <- ifelse(!read, "unverified (keyword only)",
                    ifelse(disagree, "low - signals disagree",
                    ifelse(llmconf %in% c("high", "medium", "low"), llmconf, "low")))

# ---- questionnaire check (identify/GAIN_QUESTIONNAIRE_CHECK.R), if run --------
qc_file <- tail(sort(list.files(".", "^questionnaire_check_.*\\.csv$")), 1)
qv <- rep(NA_character_, nrow(d)); qhits <- rep(NA_character_, nrow(d))
qvars <- rep(NA_character_, nrow(d)); qpops <- rep(NA_character_, nrow(d))
if (length(qc_file)) {
  qc <- suppressMessages(read_csv(qc_file, show_col_types = FALSE)) %>% distinct(url, .keep_all = TRUE)
  j <- match(url, qc$url)
  qv <- qc$questionnaire_verdict[j]; qhits <- qc$hit_examples[j]
  if ("hit_vars" %in% names(qc)) { qvars <- qc$hit_vars[j]; qpops <- qc$populations[j] }
  message("Questionnaire check: ", qc_file, " (", sum(!is.na(qv)), " candidates covered)")
}
q_yes <- coalesce(str_detect(qv, "questions found"), FALSE)
q_no  <- coalesce(qv == "no displacement question", FALSE)

# ---- final_tier ----------------------------------------------------------------
junk       <- str_detect(url, "(?i)samlrequest|/sso\\b|/sso\\?|/login|/signin|/idp/profile|/oauth|/auth/")
historic   <- !is.na(year_n) & year_n < HISTORIC_BEFORE
unreadable <- !got_txt & final_score < 50                 # AI saw a snippet only
unsure     <- read & !counted & !is.na(llm) & llm == 40   # the AI's "no evidence either way"
operational <- str_detect(t, OPERATIONAL_PAT)
reach <- read & counted & final_score >= REACH_MIN & !(operational & route == "NSO (direct)")

final_tier <- case_when(
  junk                                 ~ "junk - login/redirect page",
  in_gain                              ~ "already in GAIN",
  historic                             ~ "historic - context only",
  maybe_in_gain & final_score >= 50    ~ "review - possible GAIN match",
  !read                                ~ "not assessed",
  reach                                ~ "reach out",
  counted & final_score >= 50          ~ "review then reach out",
  # its parent is already in GAIN, so it is relevant by definition (reviewer: R08
  # Canada BDIM, a new edition the AI scored 45) - never below "review then reach out"
  v2cat %in% c("new edition", "new output of GAIN example") ~ "review then reach out",
  # the questionnaire overrides the AI's catalogue-page guess. Reviewer (Sep 2026):
  # a DHS forced-displacement question (v175 answer "Forced displacement") or a
  # stateless answer option is ENOUGH TO ASK -> reach out
  q_yes                                ~ "reach out",
  q_no & final_score < 70              ~ "low",
  unreadable                           ~ "could not read - check by hand",
  unsure                               ~ "unsure - check questionnaire",
  final_score >= 40                    ~ "watch / context",
  TRUE                                 ~ "low")

# what kind of ask (only meaningful for the reach-out tiers)
ask_type <- case_when(
  v2cat == "new edition"                ~ "update GAIN example (new edition)",
  v2cat == "new output of GAIN example" ~ "new example (output of a GAIN example)",
  q_yes & !counted                      ~ paste0("new example (questionnaire identifies ", coalesce(qpops, "displacement"), ")"),
  route == "regional body" & eurostat   ~ "regional example (Eurostat)",
  TRUE                                  ~ "new example")

# ---- SDG hand-off flag -------------------------------------------------------
SDG_SOURCE_PAT <- paste0("census|recensement|censo|\\bdhs\\b|demographic and health|\\bmics\\b|",
  "multiple indicator|labou?r force|\\blfs\\b|household (budget|income|survey)|integrated (household|survey)|",
  "living standards|\\blsms\\b|forced displacement survey|\\bfds\\b|register|registre|registro")
use_for_sdg <- !junk & !historic & (counted | in_gain | q_yes) & str_detect(t, SDG_SOURCE_PAT)

# ---- final_reason ------------------------------------------------------------
kwnote <- ifelse(disagree, " (keyword under-rated)", "")
final_reason <- case_when(
  junk ~ "sign-in / redirect URL, no document",
  in_gain | maybe_in_gain ~ sprintf("%s GAIN %s (example %s) - %s", if_else(in_gain, "matches", "may match"),
      coalesce(na_if(pidx, ""), "?"), coalesce(na_if(gidx, ""), "?"), substr(gtitle, 1, 60)),
  historic ~ sprintf("reference year %d is before %d", year_n, HISTORIC_BEFORE),
  final_tier == "could not read - check by hand" ~ "document text not retrievable; AI saw a snippet only",
  q_yes & !counted ~ paste0("questionnaire identifies ", coalesce(qpops, "displacement"), " (", coalesce(qvars, ""), "): ",
                            substr(coalesce(qhits, ""), 1, 100)),
  q_no & final_tier == "low" ~ "questionnaire read: no displacement question",
  final_tier == "unsure - check questionnaire" ~ "AI found no evidence either way (default 40) - check the questionnaire/variables",
  final_tier == "reach out" ~ sprintf("AI: counted (%s), rel %d, %s%s",
      substr(chr(pk("llm_population")), 1, 30), final_score, route, kwnote),
  read ~ sprintf("AI rel %d, %s%s, %s%s", final_score,
      ifelse(counted, "counted", "not counted"), ifelse(humanit, ", humanitarian-data", ""), route, kwnote),
  TRUE ~ "not AI-read - keyword score only")

# ---- one row per product: Eurostat per DATASET, others per country+org+title --
ds <- if (use_v2) chr(pk("match_v2_eurostat_dataset")) else rep("", nrow(d))
disp_country <- if_else(eurostat, "EU (regional - Eurostat)", as.character(pk("country")))
series_key <- if_else(eurostat & ds != "", paste0("eurostat:", tolower(ds)),
  str_squish(tolower(paste(chr(pk("country")), chr(org_s), first_words(title_s, 4)))))

out <- d %>% mutate(
  final_score = final_score, final_tier = final_tier, route = route, ask_type = ask_type,
  final_confidence = final_confidence, final_reason = final_reason,
  use_for_sdg = use_for_sdg, final_series_key = series_key,
  questionnaire_verdict = qv, questionnaire_hits = qhits,
  questionnaire_vars = qvars, questionnaire_populations = qpops,
  disp_country = disp_country, disp_org = org_s, disp_instrument = title_s,
  disp_year = as.character(pk("llm_year")),
  disp_population = as.character(pk("llm_population")),
  disp_quote = as.character(pk("llm_quote")), disp_url = url,
  disp_counted = counted, disp_lead = lead, disp_humanit = humanit,
  disp_gm = if (use_v2) v2cat else gm, disp_pidx = pidx, disp_gidx = gidx, disp_gtitle = gtitle,
  disp_checks = if (use_v2) chr(pk("match_v2_checks")) else "")

out <- out %>% group_by(final_series_key) %>%
  mutate(is_series_primary = row_number() == which.max(final_score)) %>% ungroup()

today <- format(Sys.Date(), "%Y%m%d")
readr::write_excel_csv(out, sprintf("GAIN_EVIDENCE_FINAL_%s.csv", today))

# ---- dashboard table ---------------------------------------------------------
dash <- out %>% transmute(
  country = disp_country, organization = disp_org, instrument = disp_instrument,
  year = disp_year, final_score, final_tier, route, ask_type, final_confidence,
  counted = disp_counted, lead_type = disp_lead, humanitarian_flag = disp_humanit,
  gain_match_type = disp_gm, matched_gain_pindex2 = disp_pidx, matched_gain_index = disp_gidx,
  matched_gain_title = disp_gtitle, gain_match_checks = disp_checks,
  use_for_sdg, questionnaire_verdict, final_reason, is_series_primary, url = disp_url)
dir.create("powerbi_export", showWarnings = FALSE)
readr::write_excel_csv(dash, "powerbi_export/WEB_GAIN_final.csv")

# ---- reach-out shortlist (the hand-off to the outreach workstream) ------------
short <- out %>% filter(final_tier %in% c("reach out", "review then reach out"), is_series_primary) %>%
  transmute(country = disp_country, organization = disp_org, instrument = disp_instrument,
            year = disp_year, final_tier, route, ask_type, final_score, final_confidence,
            population = disp_population, evidence_quote = disp_quote, url = disp_url,
            matched_gain_pindex2 = disp_pidx, matched_gain_title = disp_gtitle,
            use_for_sdg, final_series_key) %>%
  arrange(route, desc(final_tier == "reach out"), desc(final_score))
readr::write_excel_csv(short, sprintf("GAIN_REACHOUT_SHORTLIST_%s.csv", today))

# ---- SDG hand-off --------------------------------------------------------------
sdg <- out %>% filter(use_for_sdg, is_series_primary) %>%
  transmute(country = disp_country, organization = disp_org, instrument = disp_instrument,
            year = disp_year, final_tier, in_gain = final_tier == "already in GAIN",
            # the variables to use for our own tabulation (reviewer: e.g. Lesotho
            # DHS v175 - analyse it ourselves for SDG disaggregation)
            populations = questionnaire_populations, variables = questionnaire_vars,
            matched_gain_pindex2 = disp_pidx, url = disp_url)
readr::write_excel_csv(sdg, sprintf("GAIN_SDG_HANDOFF_%s.csv", today))

# ---- report ------------------------------------------------------------------
message("\n==== final_tier (one row per product) ====")
print(out %>% filter(is_series_primary) %>% count(final_tier, sort = TRUE))
message("\n==== shortlist by route ====")
print(count(short, route, final_tier))
message(sprintf("\nshortlist: %d products | SDG hand-off: %d", nrow(short), nrow(sdg)))
message("wrote: GAIN_EVIDENCE_FINAL_", today, ".csv | powerbi_export/WEB_GAIN_final.csv | ",
        "GAIN_REACHOUT_SHORTLIST_", today, ".csv | GAIN_SDG_HANDOFF_", today, ".csv")
