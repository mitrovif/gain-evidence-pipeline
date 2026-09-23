# ==============================================================================
# GAIN SDG DASHBOARD VALUES  (workstream stage 6 - the one publishable table)
#
# Unions every SDG VALUE we hold into a single long table for a dashboard/report:
#   * published values   - extracted from harvested documents (NSO reports,
#                          GAIN examples, statistical yearbooks) by the classifier
#   * demonstration values - estimates WE produced from microdata (stage 5),
#                          clearly flagged NOT official statistics
#
# One row per country x indicator x population, so a dashboard can put the
# OVERALL-population (national/total) value next to the refugee / IDP / stateless
# value for the same indicator - the comparison the whole exercise is for.
#
# Every SDG the classifier found is included (not only the priority set);
# is_priority flags whether it is one of the IRRS/IRIS/IROSS priority indicators.
#
# Output: powerbi_export/WEB_GAIN_sdg_values.csv  (+ a copy in sdg_docs/)
# Re-runnable any time - reads whatever the classification has produced so far.
# ==============================================================================

suppressMessages({ library(tidyverse) })
source("shared/GAIN_COMMON.R")
DIR <- "sdg_docs"

tax <- read_csv(file.path(DIR, "sdg_priority_indicators.csv"), show_col_types = FALSE)
priority_codes <- tax$code
tax_lookup <- tax %>% distinct(code, .keep_all = TRUE) %>%
  select(sdg_code = code, sdg_title = title, thematic_group)

norm_pop <- function(p) {
  p <- str_to_lower(coalesce(p, ""))
  case_when(
    str_detect(p, "refug")                    ~ "refugees",
    str_detect(p, "idp|internally displaced") ~ "idps",
    str_detect(p, "stateless")                ~ "stateless",
    str_detect(p, "return")                   ~ "returnees",
    str_detect(p, "host")                     ~ "host community",
    str_detect(p, "total|overall|national|all") | p == "" ~ "total",
    TRUE                                      ~ p)
}

# ---- published values ---------------------------------------------------------
dp_f <- file.path(DIR, "SDG_DATAPOINTS.csv")
pub <- if (file.exists(dp_f)) {
  read_csv(dp_f, show_col_types = FALSE) %>%
    transmute(
      country = harmonize_country(country),
      sdg_code, match_type,
      measure_as_stated,
      population = norm_pop(population), population_detail = coalesce(population_detail, ""),
      value = as.character(value), unit = coalesce(unit, ""), year = as.character(year),
      value_type = "published",
      production_method = coalesce(production_method, ""),
      instrument = coalesce(instrument, title),
      reference = str_squish(paste0(coalesce(doc_id, ""),
                                    if_else(!is.na(page) & page != "", paste0(" p.", page), ""))),
      source_url = coalesce(url, ""),
      se = NA_real_, unweighted_n = NA_integer_,
      evidence = coalesce(evidence, ""),
      note = "",
      verified = coalesce(as.character(verified), ""))
} else tibble()

# ---- demonstration values -----------------------------------------------------
dm_f <- file.path(DIR, "SDG_DEMONSTRATION_TABULATIONS.csv")
dem <- if (file.exists(dm_f)) {
  read_csv(dm_f, show_col_types = FALSE) %>%
    transmute(
      country = harmonize_country(country),
      sdg_code, match_type,
      measure_as_stated,
      population = norm_pop(population), population_detail = coalesce(population_detail, ""),
      value = as.character(value), unit = coalesce(unit, ""), year = as.character(year),
      value_type = "demonstration tabulation",
      production_method = coalesce(production_method, "survey"),
      instrument = coalesce(instrument, ""),
      reference = coalesce(instrument, ""),
      source_url = "",
      se = suppressWarnings(as.numeric(se)),
      unweighted_n = suppressWarnings(as.integer(unweighted_n)),
      evidence = "",
      note = coalesce(note, ""),
      verified = coalesce(as.character(verified), ""))
} else tibble()

# ---- union + priority flag + indicator titles ---------------------------------
vals <- bind_rows(pub, dem) %>%
  mutate(is_priority = sdg_code %in% priority_codes) %>%
  left_join(tax_lookup, by = "sdg_code") %>%
  # for non-priority codes we have no curated title; keep the measure as a stand-in
  mutate(sdg_title = coalesce(sdg_title, measure_as_stated),
         thematic_group = coalesce(thematic_group, "(non-priority SDG)")) %>%
  relocate(country, sdg_code, sdg_title, thematic_group, is_priority,
           population, population_detail, match_type, measure_as_stated,
           value, unit, year, value_type) %>%
  arrange(desc(is_priority), sdg_code, country,
          factor(population, levels = c("total", "refugees", "idps", "stateless",
                                        "returnees", "host community")))

out_main <- file.path(DIR, "SDG_DASHBOARD_VALUES.csv")
write_excel_csv(vals, out_main)
if (dir.exists("powerbi_export"))
  write_excel_csv(vals, file.path("powerbi_export", "WEB_GAIN_sdg_values.csv"))

# ---- console summary ----------------------------------------------------------
message("==================== SDG DASHBOARD VALUES ====================")
message("total value rows: ", nrow(vals),
        "  (published: ", sum(vals$value_type == "published"),
        " | demonstration: ", sum(vals$value_type == "demonstration tabulation"), ")")
message("countries: ", n_distinct(vals$country),
        " | indicators: ", n_distinct(vals$sdg_code),
        " | priority-indicator rows: ", sum(vals$is_priority))
message("\nby population:")
print(as.data.frame(count(vals, population, sort = TRUE)), row.names = FALSE)
message("\nindicators where we have BOTH a total and a displacement-group value ",
        "(the side-by-side comparison the dashboard is for):")
comp <- vals %>%
  mutate(grp = if_else(population == "total", "total", "displacement")) %>%
  distinct(country, sdg_code, grp) %>%
  count(country, sdg_code) %>% filter(n > 1)
if (nrow(comp)) print(as.data.frame(comp), row.names = FALSE) else
  message("  (none yet - grows as classification + tabulations accumulate)")
message("\nOutput: ", out_main,
        if (dir.exists("powerbi_export")) " (+ powerbi_export/WEB_GAIN_sdg_values.csv)" else "")
