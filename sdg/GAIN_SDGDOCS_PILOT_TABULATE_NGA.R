# ==============================================================================
# GAIN SDG PILOT TABULATION - NIGERIA  (stage 5 - demonstration estimates)
#
# Pilot dataset: Nigeria General Household Survey, Panel 2023-2024, Wave 5
#                (World Bank LSMS-ISA, NGA_2023_GHSP-W5_v01_M, catalogue 6410)
#
# KEY FINDING (documented, not a bug): unlike the Cameroon profiling survey, the
# Nigeria GHS Panel is a GENERAL national household survey and carries NO
# displacement-status variable. The only displacement marker in the entire
# instrument is s1q23 == "11. FLED PROBLEM AREAS / INTERNALLY DISPLACED PERSONS
# / CRISIS" as a reason a person JOINED the household - and only 12 individuals
# in the whole 28,256-person roster fall in it (below the n>=30 floor, and it
# captures only IDPs who recently joined a host household, not displacement
# status of the population). Therefore Nigeria CANNOT support a
# displacement-disaggregated tabulation. That gap IS the demonstration: it is
# exactly what the GAIN recommendations ask national surveys to close.
#
# What this script DOES produce: the NATIONAL (overall-population) SDG value the
# survey can generate today, to sit in the same dataset as the displacement cuts
# from surveys (e.g. Cameroon) that do carry the identifier - so a dashboard can
# show national context beside the displacement-specific figures.
#
# Indicator: 6.1.1 (proxy: population using an improved drinking-water source,
#            the WHO/JMP "improved" class; SDG 6.1.1 proper = "safely managed",
#            which needs availability+quality this variable alone cannot give ->
#            match_type = "related").
#
# OUTPUT: appends to sdg_docs/SDG_DEMONSTRATION_TABULATIONS.csv
#         source = "demonstration tabulation", NOT official statistics.
# ==============================================================================

suppressMessages({ library(tidyverse); library(survey) })
EXTRACT <- "C:/temp/gain_pilot_extract/NGA"
OUT     <- "sdg_docs/SDG_DEMONSTRATION_TABULATIONS.csv"
MIN_N   <- 30

if (!dir.exists(EXTRACT)) {
  Z <- "sdg_docs/pilot/data/NGA_2023_GHSP-W5_v01_M_CSV.zip"
  if (!file.exists(Z)) stop("Nigeria zip not found at ", Z)
  dir.create(EXTRACT, recursive = TRUE, showWarnings = FALSE)
  unzip(Z, exdir = EXTRACT)      # short path: avoids Windows 260-char MAX_PATH
}
rd <- function(p) suppressMessages(readr::read_csv(file.path(EXTRACT, p),
                                                   show_col_types = FALSE, guess_max = 5000))

# ---- 1. household frame: weight + design vars (post-harvest cover file) --------
cover <- rd("Post Harvest Wave 5/Household/secta_harvestw5.csv")
names(cover) <- tolower(names(cover))
wt_var  <- intersect(c("wt_cross_wave5", "wt_wave5"), names(cover))[1]
if (is.na(wt_var)) stop("no cross-sectional weight in cover file")
psu_var <- intersect(c("ea", "cluster"), names(cover))[1]     # NA -> weights-only
str_var <- intersect(c("strata"), names(cover))[1]
cat("weight:", wt_var, "| psu:", psu_var %||% "(none)", "| strata:", str_var %||% "(none)", "\n")

frame <- cover %>%
  transmute(hhid = as.character(hhid),
            .w   = as.numeric(.data[[wt_var]]),
            .psu = if (!is.na(psu_var)) as.character(.data[[psu_var]]) else NA_character_,
            .str = if (!is.na(str_var)) as.character(.data[[str_var]]) else NA_character_) %>%
  filter(!is.na(.w), .w > 0)

# ---- 2. displacement-marker audit (documents WHY there is no disaggregation) ---
rost <- rd("Post Harvest Wave 5/Household/sect1_harvestw5.csv")
names(rost) <- tolower(names(rost))
n_idp <- if ("s1q23" %in% names(rost))
  sum(grepl("INTERNALLY DISPLACED|FLED PROBLEM", rost$s1q23, ignore.case = TRUE), na.rm = TRUE) else 0
cat("displacement marker (s1q23 == 'fled/IDP'):", n_idp, "persons",
    if (n_idp < MIN_N) "-> below floor, no disaggregation possible\n" else "\n")

# ---- 3. indicator 6.1.1: improved drinking-water source (household level) ------
water <- rd("Post Harvest Wave 5/Household/sect9_harvestw5.csv")
names(water) <- tolower(names(water))
# WHO/JMP improved sources; guard "UNPROTECTED"/"SURFACE" as unimproved FIRST.
classify_water <- function(x) {
  x <- toupper(as.character(x))
  imp <- rep(NA_integer_, length(x))
  imp[grepl("UNPROTECTED|SURFACE WATER", x)] <- 0L
  imp[is.na(imp) & grepl("PIPED|TAP|STANDPIPE|BOREHOLE|TUBE WELL|PROTECTED|RAIN|TANKER|CART|BOTTLED|SACHET|KIOSK|PROTECTED SPRING", x)] <- 1L
  imp
}
d <- frame %>%
  inner_join(water %>% transmute(hhid = as.character(hhid), src = s9q27), by = "hhid") %>%
  mutate(.x = classify_water(src)) %>%
  filter(!is.na(.x))
cat("water: matched", nrow(d), "households to weights\n")

use_design <- all(!is.na(d$.psu)) && all(!is.na(d$.str)) &&
              dplyr::n_distinct(d$.psu) > 1 && dplyr::n_distinct(d$.str) > 1
des <- if (use_design) {
  options(survey.lonely.psu = "adjust")
  svydesign(ids = ~.psu, strata = ~.str, weights = ~.w, data = d, nest = TRUE)
} else svydesign(ids = ~1, weights = ~.w, data = d)
cat("design:", if (use_design) "stratified cluster (real SEs)" else "weights-only (SEs indicative)", "\n")

est <- as.numeric(svymean(~.x, des)) * 100
se  <- as.numeric(SE(svymean(~.x, des))) * 100
cat(sprintf("\n6.1.1 improved drinking water (national, total pop): %.1f%% (SE %.2f, n=%d)\n",
            est, se, nrow(d)))

# ---- 4. write in the shared demonstration schema ------------------------------
row <- tibble(
  country = "Nigeria", sdg_code = "6.1.1", match_type = "related",
  measure_as_stated = "Population using an improved drinking-water source",
  population = "total", population_detail = "",
  value = as.character(round(est, 1)), unit = "%", year = "2023",
  se = round(se, 2), unweighted_n = nrow(d),
  production_method = "survey",
  instrument = "General Household Survey Panel 2023-2024 Wave 5 (NGA_2023_GHSP-W5_v01_M)",
  source = "demonstration tabulation",
  note = paste0("NOT official statistics - feasibility demonstration; NATIONAL total only. ",
                "GHS Panel carries no displacement-status variable (only s1q23 'fled/IDP' reason-for-joining, n=",
                n_idp, "), so it cannot disaggregate by displacement - the gap the GAIN recommendations address."),
  verified = "")
write_csv(row, OUT, append = file.exists(OUT))
message("\nAppended Nigeria national datapoint to ", OUT)
