# ==============================================================================
# GAIN SDG PILOT TABULATION  (stage 5 - demonstration estimates from microdata)
#
# Pilot dataset: Cameroon Socio-Economic Profiling Survey 2025
#                (UNHCR_CMR_2025_PROFILING_v2.1, catalogue 1467)
# Pilot indicators (from the DDI variable map, sdg_docs/pilot/*_variable_map.csv):
#   7.1.1  access to electricity   <- AEQ0  "access to electricity from the public grid"
#   6.1.1  drinking water (basic)  <- AAQ1  "main source of drinking water"
# Group variable: HOUSEHOLD_STATUS_SR (IDP / refugee / host ... as categorised
# by the survey itself) | Weight: WEIGHT_FINAL ("weighting raking ratio")
#
# HOW TO USE
#   1. Log in to https://microdata.unhcr.org/index.php/catalog/1467 and download
#      the study's data files under its access conditions (2 files: Main
#      household file + Members file; any of Stata/SPSS/CSV formats is fine).
#   2. Drop them (zips are fine) into sdg_docs/pilot/data/
#   3. source("sdg/GAIN_SDGDOCS_PILOT_TABULATE.R")
#
# OUTPUT: sdg_docs/SDG_DEMONSTRATION_TABULATIONS.csv - same shape as
# SDG_DATAPOINTS.csv but source = "demonstration tabulation".
#
# FRAMING (non-negotiable): these are DEMONSTRATION ESTIMATES showing that the
# survey CAN produce these indicators disaggregated by displacement status.
# They are NOT official statistics - the country owns those - and they are the
# feasibility evidence for the GAIN outreach ask ("please produce and report").
# Small cells (unweighted n < 30) are suppressed. Estimates are weighted with
# WEIGHT_FINAL; without documented strata/PSU the SEs are indicative only.
# ==============================================================================

suppressMessages({ library(tidyverse); library(haven); library(survey) })
DIR  <- "sdg_docs/pilot/data"
OUT  <- "sdg_docs/SDG_DEMONSTRATION_TABULATIONS.csv"
MIN_N <- 30    # unweighted minimum cell size (disclosure + reliability)

VAR_WEIGHT <- "WEIGHT_FINAL"
VAR_GROUP  <- "HOUSEHOLD_STATUS_SR"
INDICATORS <- tribble(
  ~sdg_code, ~match_type,  ~measure,                                   ~var,   ~positive_pattern,
  "7.1.1",   "exact",      "Household access to electricity (grid)",   "AEQ0", "^(yes|oui|1)",
  "6.1.1",   "related",    "Main drinking water source is improved",   "AAQ1",
    "piped|robinet|tap|borehole|forage|protected|protégé|rain|bottled|bouteille|kiosk|stand")

# ---- 1. find and read the household file --------------------------------------
if (!dir.exists(DIR) || length(list.files(DIR)) == 0)
  stop("No files in ", DIR, " - download the two Cameroon files from the catalogue ",
       "(see header) and drop them there, then re-run.")
# zips are extracted to a SHORT path: this project folder is ~190 characters
# deep, and the zip's internal folder + filenames pushed extraction past the
# Windows 260-char MAX_PATH limit (unzip died mid-file). C:/temp is safe.
EXTRACT <- "C:/temp/gain_pilot_extract"
dir.create(EXTRACT, showWarnings = FALSE, recursive = TRUE)
for (z in list.files(DIR, pattern = "\\.zip$", full.names = TRUE))
  unzip(z, exdir = EXTRACT, overwrite = TRUE)
cands <- c(list.files(DIR,     pattern = "\\.(dta|sav|csv)$", full.names = TRUE, recursive = TRUE),
           list.files(EXTRACT, pattern = "\\.(dta|sav|csv)$", full.names = TRUE, recursive = TRUE))
if (length(cands) == 0) stop("No .dta/.sav/.csv found in ", DIR, " or the zip(s)")

read_any <- function(f) switch(tolower(tools::file_ext(f)),
  dta = haven::read_dta(f), sav = haven::read_sav(f),
  csv = readr::read_csv(f, show_col_types = FALSE))
find_household_file <- function() {
  for (f in cands) {
    d <- tryCatch(read_any(f), error = function(e) NULL)
    if (!is.null(d) &&
        all(c(VAR_WEIGHT, VAR_GROUP) %in% toupper(names(d)) |
            c(VAR_WEIGHT, VAR_GROUP) %in% names(d))) {
      names(d) <- toupper(names(d))
      message("household file: ", basename(f), " (", nrow(d), " rows)")
      return(d)
    }
  }
  stop("None of the files contain both ", VAR_WEIGHT, " and ", VAR_GROUP,
       " - check the variable map in sdg_docs/pilot/ and adjust the VAR_* constants.")
}
hh <- find_household_file()

# ---- 2. clean the pieces -------------------------------------------------------
as_label <- function(x) {
  if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x)) else as.character(x)
}
hh <- hh %>%
  mutate(.w = as.numeric(.data[[VAR_WEIGHT]]),
         .g = str_squish(as_label(.data[[VAR_GROUP]]))) %>%
  filter(!is.na(.w), .w > 0, nzchar(coalesce(.g, "")))
message("groups found: ", paste(names(table(hh$.g)), table(hh$.g), sep = "=", collapse = " | "))

# ---- 3. tabulate each indicator by group (+ total), design-weighted ------------
if (file.exists(OUT)) file.remove(OUT)
for (i in seq_len(nrow(INDICATORS))) {
  ind <- INDICATORS[i, ]
  v <- toupper(ind$var)
  if (!v %in% names(hh)) { message("skip ", ind$sdg_code, ": variable ", v, " not in file"); next }
  d <- hh %>%
    mutate(.x = as.integer(str_detect(str_to_lower(as_label(.data[[v]])),
                                      ind$positive_pattern))) %>%
    filter(!is.na(.x))
  des <- svydesign(ids = ~1, weights = ~.w, data = d)   # weights only: SEs indicative
  tab <- bind_rows(
    tibble(group = "total", n = nrow(d),
           est = as.numeric(svymean(~.x, des)) * 100,
           se  = as.numeric(SE(svymean(~.x, des))) * 100),
    map_dfr(unique(d$.g), function(g) {
      dg <- subset(des, .g == g)
      tibble(group = g, n = sum(d$.g == g),
             est = as.numeric(svymean(~.x, dg)) * 100,
             se  = as.numeric(SE(svymean(~.x, dg))) * 100)
    }))
  tab <- tab %>% mutate(suppressed = n < MIN_N)
  message("\n", ind$sdg_code, " ", ind$measure, " (positive = '", ind$positive_pattern, "'):")
  print(as.data.frame(tab %>% mutate(across(c(est, se), ~ round(.x, 1)))), row.names = FALSE)
  write_csv(tab %>% filter(!suppressed) %>% transmute(
    country = "Cameroon", sdg_code = ind$sdg_code, match_type = ind$match_type,
    measure_as_stated = ind$measure,
    population = case_when(str_detect(str_to_lower(group), "refug") ~ "refugees",
                           str_detect(str_to_lower(group), "non-displac|non-déplac|host") ~ "host community",
                           str_detect(str_to_lower(group), "idp|displac") ~ "idps",
                           group == "total" ~ "total", TRUE ~ "total"),
    population_detail = if_else(group == "total", "", group),
    value = as.character(round(est, 1)), unit = "%", year = "2025",
    se = round(se, 2), unweighted_n = n,
    production_method = "survey",
    instrument = "Socio-Economic Profiling Survey 2025 (UNHCR_CMR_2025_PROFILING_v2.1)",
    source = "demonstration tabulation",
    note = "NOT official statistics - feasibility demonstration; weights only, SEs indicative",
    verified = ""), OUT, append = file.exists(OUT))
}
# ---- 4. person-level indicators from the individual/members file ---------------
# The individual file carries its own WEIGHT_FINAL and PERSON_STATUS_SR.
# CIQ2 is anonymised into AGE BANDS (1=0-4, 2=5-11, 3=12-17, 4=18-24, 5=25-49,
# 6=50-59, 7=60+), so age filters run on band codes and cut-points follow the
# bands (school age 5-17; adults 18+ because no band splits at 15).
#   16.9.1 comparable - under-5s holding a birth certificate (CIQ39_2) or birth
#          declaration (CIQ39_3): document POSSESSION, a proxy for registration
#   8.5.2  related    - employment-to-population ratio 18+ (CIQ52 worked >=1h,
#          or CIQ54 has job but absent); the survey lacks the job-search items
#          needed for a true ILO unemployment rate
#   4.1.2  related    - enrolled in formal education (CIQ44), ages 5-17
ind_f <- cands[grepl("ind_EN|individual", basename(cands), ignore.case = TRUE) &
               grepl("\\.sav$", cands, ignore.case = TRUE)][1]
if (!is.na(ind_f)) {
  pp <- haven::read_sav(ind_f)
  names(pp) <- toupper(names(pp))
  message("\nindividual file: ", basename(ind_f), " (", nrow(pp), " persons)")
  pp <- pp %>%
    mutate(.w    = as.numeric(WEIGHT_FINAL),
           .g    = str_squish(as_label(PERSON_STATUS_SR)),
           .band = as.numeric(CIQ2)) %>%   # band code, NOT years (see above)
    filter(!is.na(.w), .w > 0, nzchar(coalesce(.g, "")))
  PERSON_IND <- list(
    list(sdg = "16.9.1", match = "comparable", bands = 1,
         measure = "Children under 5 holding a birth certificate or birth declaration",
         make = function(d) as.integer(d$CIQ39_2 == 1 | d$CIQ39_3 == 1)),
    list(sdg = "8.5.2",  match = "related",    bands = 4:7,
         measure = "Employment-to-population ratio, 18+ (worked >=1h or job but absent)",
         make = function(d) as.integer(d$CIQ52 == 1 | (!is.na(d$CIQ54) & d$CIQ54 == 1))),
    list(sdg = "4.1.2",  match = "related",    bands = 2:3,
         measure = "Enrolled in formal education, ages 5-17",
         make = function(d) as.integer(d$CIQ44 == 1)))
  for (ind in PERSON_IND) {
    d <- pp %>% filter(.band %in% ind$bands)
    d$.x <- ind$make(d)
    d <- d %>% filter(!is.na(.x))
    if (nrow(d) < MIN_N) { message("skip ", ind$sdg, ": only ", nrow(d), " persons in age range"); next }
    des <- svydesign(ids = ~1, weights = ~.w, data = d)
    tab <- bind_rows(
      tibble(group = "total", n = nrow(d),
             est = as.numeric(svymean(~.x, des)) * 100,
             se  = as.numeric(SE(svymean(~.x, des))) * 100),
      map_dfr(unique(d$.g), function(g) {
        dg <- subset(des, .g == g)
        tibble(group = g, n = sum(d$.g == g),
               est = as.numeric(svymean(~.x, dg)) * 100,
               se  = as.numeric(SE(svymean(~.x, dg))) * 100)
      })) %>% mutate(suppressed = n < MIN_N)
    message("\n", ind$sdg, " ", ind$measure, ":")
    print(as.data.frame(tab %>% mutate(across(c(est, se), ~ round(.x, 1)))), row.names = FALSE)
    write_csv(tab %>% filter(!suppressed) %>% transmute(
      country = "Cameroon", sdg_code = ind$sdg, match_type = ind$match,
      measure_as_stated = ind$measure,
      population = case_when(str_detect(str_to_lower(group), "refug") ~ "refugees",
                             str_detect(str_to_lower(group), "non-displac|non-déplac|host") ~ "host community",
                             str_detect(str_to_lower(group), "idp|displaced person") ~ "idps",
                             group == "total" ~ "total", TRUE ~ "total"),
      population_detail = if_else(group == "total", "", group),
      value = as.character(round(est, 1)), unit = "%", year = "2025",
      se = round(se, 2), unweighted_n = n,
      production_method = "survey",
      instrument = "Socio-Economic Profiling Survey 2025 (UNHCR_CMR_2025_PROFILING_v2.1)",
      source = "demonstration tabulation",
      note = "NOT official statistics - feasibility demonstration; weights only, SEs indicative",
      verified = ""), OUT, append = file.exists(OUT))
  }
} else message("\n(no individual-level file found - person-level indicators skipped)")

message("\nOutput: ", OUT, "  (demonstration estimates - review before any use)")
