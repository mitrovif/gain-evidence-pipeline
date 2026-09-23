# ==============================================================================
# GAIN COMMON HELPERS  (single source of truth - source() this, don't copy)
#
# Why this file exists: harmonize_country() used to live as separate literal
# copies in GAIN_PHASE5_CROSSREF.R and GAIN_OUTREACH_TARGETS.R, and the copies
# had already drifted ("Moldova (the Republic of)" was only in one of them).
# Country-name matching is the join key for the whole outreach chain - the
# GAIN roster, the contact list, the outreach log and the cycle tracker all
# meet on it - so a one-sided spelling fix silently breaks matching elsewhere.
#
# Requires tidyverse (stringr + dplyr) to be loaded by the sourcing script.
# ==============================================================================

if (!"package:dplyr" %in% search())
  stop("GAIN_COMMON.R needs tidyverse loaded first - add library(tidyverse) before source().")

# ------------------------------------------------------------------------------
# Country name harmonization. Crawler registry, GAIN roster, contact list and
# catalogue producers all use different spellings; this maps them to the GAIN
# roster's canonical names. Extend HERE (only here) as mismatches appear in the
# crossref's unmatched-countries report.
# ------------------------------------------------------------------------------
harmonize_country <- function(x) {
  x <- str_trim(str_remove_all(coalesce(x, ""), "\\*"))
  recode(x,
    "Turkey"                        = "Türkiye",
    "Netherlands"                   = "Netherlands (Kingdom of the)",
    "Tanzania"                      = "United Republic of Tanzania",
    "Russia"                        = "Russian Federation",
    "Ivory Coast"                   = "Côte d'Ivoire",
    "Cote d'Ivoire"                 = "Côte d'Ivoire",
    "Palestine"                     = "State of Palestine",
    "Moldova"                       = "Republic of Moldova",
    "Bolivia"                       = "Bolivia (Plurinational State of)",
    "Venezuela"                     = "Venezuela (Bolivarian Republic of)",
    "Iran"                          = "Iran (Islamic Republic of)",
    "Syria"                         = "Syrian Arab Republic",
    "Vietnam"                       = "Viet Nam",
    "Laos"                          = "Lao People's Democratic Republic",
    "South Korea"                   = "Republic of Korea",
    "DR Congo"                      = "Democratic Republic of the Congo",
    "DRC"                           = "Democratic Republic of the Congo",
    # World Bank / catalogue spellings seen in harvested evidence:
    "Congo, Dem. Rep."              = "Democratic Republic of the Congo",
    "Congo, Rep."                   = "Congo",
    "Yemen, Rep."                   = "Yemen",
    "Egypt, Arab Rep."              = "Egypt",
    "Iran, Islamic Rep."            = "Iran (Islamic Republic of)",
    "Kyrgyz Republic"               = "Kyrgyzstan",
    "Slovak Republic"               = "Slovakia",
    "Czech Republic"                = "Czechia",
    "Moldova (the Republic of)"     = "Republic of Moldova",
    "Federal Republic of Somalia"   = "Somalia",
    "United States"                 = "United States of America",
    "Venezuela, RB"                 = "Venezuela (Bolivarian Republic of)",
    "Gambia, The"                   = "Gambia",
    "Lao PDR"                       = "Lao People's Democratic Republic",
    "Korea, Rep."                   = "Republic of Korea",
    .default = x
  )
}

# ------------------------------------------------------------------------------
# Newest file matching a pattern, by MODIFICATION TIME (never by filename sort,
# which let a stale _SEM file shadow fresh data once). NA if none found.
# ------------------------------------------------------------------------------
newest_file <- function(pattern) {
  f <- list.files(pattern = pattern)
  if (length(f)) f[which.max(file.info(f)$mtime)] else NA_character_
}

# ------------------------------------------------------------------------------
# Write that tolerates transient OneDrive/Excel file locks: retries, then falls
# back to a timestamped name so a run never dies on a locked CSV.
# ------------------------------------------------------------------------------
safe_write <- function(df, path) {
  for (attempt in 1:4) {
    if (tryCatch({ write_excel_csv(df, path); TRUE }, error = function(e) FALSE))
      return(invisible(path))
    Sys.sleep(2)
  }
  alt <- sub("\\.csv$", paste0("_", format(Sys.time(), "%H%M%S"), ".csv"), path)
  write_excel_csv(df, alt)
  message("  (", basename(path), " was locked - wrote ", basename(alt), " instead)")
  invisible(alt)
}
