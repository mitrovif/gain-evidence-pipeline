# ==============================================================================
# GAIN SDG GAP MATRIX  (workstream stage 4 - where are the gaps, and which of
# them could a tabulation fill?)
#
# One row per country x priority indicator x population group, answering:
#   * has_published_value  - did the classification find a published value for
#                            this group (exact/comparable match)?
#   * has_related_value    - only a related measure found (worth review)
#   * n_candidate_docs     - harvested documents for this country that cover
#                            the group (report material)
#   * n_microdata_pages    - harvested microdata-catalogue entries for this
#                            country/group: the TABULATION candidates
#   * tabulation_candidate - no published value + microdata exists -> the case
#                            for a demonstration tabulation (stage 5)
#
# Re-run any time - it reads whatever the (still running) classification has
# produced so far. Output: sdg_docs/SDG_GAP_MATRIX.csv (+ Power BI copy).
# ==============================================================================

suppressMessages({ library(tidyverse) })
source("shared/GAIN_COMMON.R")

DIR <- "sdg_docs"
tax <- read_csv(file.path(DIR, "sdg_priority_indicators.csv"), show_col_types = FALSE)
man <- read_csv(file.path(DIR, "SDG_DOCS_MANIFEST.csv"), show_col_types = FALSE) %>%
  filter(status == "ok") %>%
  mutate(country = harmonize_country(country),
         is_catalog = str_detect(coalesce(url, ""), "microdata|catalog"))
dp_f <- file.path(DIR, "SDG_DATAPOINTS.csv")
dp <- if (file.exists(dp_f)) read_csv(dp_f, show_col_types = FALSE) %>%
        mutate(country = harmonize_country(country)) else tibble()

# stage-5 demonstration tabulations: estimates WE produced from microdata to
# fill a gap. Folded in below so a candidate cell that was actually demonstrated
# is marked as such (closing the loop harvest -> gap -> tabulate).
demo_f <- file.path(DIR, "SDG_DEMONSTRATION_TABULATIONS.csv")
demo <- if (file.exists(demo_f)) read_csv(demo_f, show_col_types = FALSE) %>%
          mutate(country = harmonize_country(country)) else tibble()

GROUPS <- c("refugees", "idps", "stateless")
grid <- expand_grid(country = sort(unique(man$country)),
                    sdg_code = tax$code, population = GROUPS)

# published values per country x code x group (from the live classification)
pub <- if (nrow(dp) > 0) {
  dp %>% filter(sdg_code %in% tax$code) %>%
    mutate(strict = match_type %in% c("exact", "comparable")) %>%
    group_by(country, sdg_code, population) %>%
    summarise(has_published_value = any(strict),
              has_related_value = any(!strict), .groups = "drop")
} else tibble(country = character(), sdg_code = character(), population = character(),
              has_published_value = logical(), has_related_value = logical())

# demonstration values we produced, per country x code x displacement group
dem <- if (nrow(demo) > 0) {
  demo %>% filter(sdg_code %in% tax$code, population %in% GROUPS) %>%
    group_by(country, sdg_code, population) %>%
    summarise(has_demonstration_value = TRUE,
              demonstration_value = paste0(first(value), first(unit)),
              .groups = "drop")
} else tibble(country = character(), sdg_code = character(), population = character(),
              has_demonstration_value = logical(), demonstration_value = character())

# candidate material per country x group (from the harvest manifest)
cand <- map_dfr(GROUPS, function(g)
  man %>% filter(str_detect(coalesce(populations, ""), g)) %>%
    group_by(country) %>%
    summarise(population = g,
              n_candidate_docs = n(),
              n_microdata_pages = sum(is_catalog),
              example_docs = paste(head(doc_id, 3), collapse = ";"), .groups = "drop"))

gap <- grid %>%
  left_join(pub,  by = c("country", "sdg_code", "population")) %>%
  left_join(dem,  by = c("country", "sdg_code", "population")) %>%
  left_join(cand, by = c("country", "population")) %>%
  mutate(across(c(has_published_value, has_related_value, has_demonstration_value),
                ~ coalesce(.x, FALSE)),
         across(c(n_candidate_docs, n_microdata_pages), ~ coalesce(.x, 0L)),
         example_docs = coalesce(example_docs, ""),
         demonstration_value = coalesce(demonstration_value, ""),
         tabulation_candidate = !has_published_value & n_microdata_pages > 0,
         # one status per cell, most-resolved wins
         cell_status = case_when(
           has_published_value     ~ "published",
           has_demonstration_value ~ "demonstrated",
           tabulation_candidate    ~ "tabulation candidate",
           has_related_value       ~ "related only (review)",
           TRUE                    ~ "gap")) %>%
  # keep only rows where the country has ANY material for that group - a
  # country with zero docs about stateless people is not a meaningful "gap"
  filter(n_candidate_docs > 0)

write_excel_csv(gap, file.path(DIR, "SDG_GAP_MATRIX.csv"))
if (dir.exists("powerbi_export"))
  write_excel_csv(gap, file.path("powerbi_export", "WEB_GAIN_sdg_gap_matrix.csv"))

message("==================== SDG GAP MATRIX ====================")
message("rows (country x indicator x group with material): ", nrow(gap))
message("cell status breakdown:")
print(as.data.frame(count(gap, cell_status, sort = TRUE)), row.names = FALSE)
message("demonstration values folded in: ", sum(gap$has_demonstration_value))
message("TABULATION CANDIDATES (no published value, microdata exists): ",
        sum(gap$tabulation_candidate))
message("\ncountries with the most tabulation-candidate cells:")
print(as.data.frame(gap %>% filter(tabulation_candidate) %>%
        count(country, sort = TRUE) %>% head(10)), row.names = FALSE)
message("\nNote: the classification is still running - re-run this script any time ",
        "for an updated matrix.")
