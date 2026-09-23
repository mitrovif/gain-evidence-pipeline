# ==============================================================================
# GAIN DATA LAKE - step 2: analysis frame + coverage/gap matrix.
#
# For each GAIN example, what could its data support, for the two priority lenses
# (identification & EGRISS categories, and SDG)? This frame is seeded from roster
# metadata now (recommendations, data source, populations, description) and is
# meant to be ENRICHED per dataset after download (real variable inventories).
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
roster <- suppressMessages(read_csv(file.path(LAKE, "lake_roster.csv"), show_col_types = FALSE))
plan   <- suppressMessages(read_csv(file.path(LAKE, "lake_download_plan.csv"), show_col_types = FALSE)) %>%
  distinct(example_id, .keep_all = TRUE)

# SDG theme detection from title + description (seed; refined after download)
SDG <- list(
  "SDG1 poverty"        = "poverty|poor|income|livelihood|destitut|deprivation",
  "SDG3 health"         = "health|mortalit|disease|vaccin|maternal|nutrition|mental",
  "SDG4 education"      = "education|school|enrol|literacy|learning|pupil|student",
  "SDG5 gender"         = "gender|women|girls|female|gender-based",
  "SDG6 WASH"           = "water|sanitation|hygiene|\\bwash\\b|drinking water",
  "SDG8 work"           = "employ|labour|labor|\\bwork\\b|\\bjob|unemploy|wage|informal sector",
  "SDG10 inclusion"     = "inequalit|inclusion|discrimination|integration|social protection",
  "SDG11 housing"       = "housing|shelter|settlement|\\burban|\\bcamp|accommodation|slum",
  "SDG16 legal identity"= "birth registration|legal identity|civil registration|documentation|statelessness|nationality|asylum")

frame <- roster %>% left_join(plan %>% select(example_id, reachability, action), by = "example_id") %>%
  mutate(
    text = tolower(paste(coalesce(title,""), coalesce(description,""))),
    # ---- data access status (from LAKE_02/03) ----
    data_access = case_when(
      action %in% c("AUTO","AUTO_META") ~ "downloadable (API)",
      action == "SCRAPE"                ~ "downloadable (scrape)",
      action == "SCRAPE_BROWSER"        ~ "needs browser scrape",
      example_id %in% (suppressMessages(read_csv(file.path(LAKE,"lake_recovery.csv"), show_col_types=FALSE)) %>%
                       filter(recovery != "none") %>% pull(example_id)) ~ "recovered (re-check)",
      TRUE ~ "data request needed"),
    # ---- unit / granularity ----
    granularity = case_when(
      str_detect(source_tool, "survey|census") ~ "microdata (survey/census)",
      str_detect(source_tool, "administrative|register|integration") ~ "admin/register data",
      nzchar(source_tool) ~ "aggregate/other", TRUE ~ "unknown"),
    # ---- LENS A: identification & EGRISS categories ----
    id_framework = str_squish(paste(ifelse(irrs,"IRRS",""), ifelse(iris,"IRIS",""), ifelse(iross,"IROSS",""))),
    id_capability = case_when(
      has_identification_rec & is_microdata_capable ~ "computable (framework + microdata)",
      has_identification_rec                        ~ "reported only (framework, aggregate)",
      is_microdata_capable                          ~ "possible (microdata, framework unstated)",
      TRUE ~ "unclear"),
    # ---- LENS B: SDG ----
    sdg_domains = map_chr(text, function(t) paste(names(SDG)[map_lgl(SDG, ~ str_detect(t, .x))], collapse = "; ")),
    sdg_any = nzchar(sdg_domains),
    sdg_disaggregatable = sdg_any & is_microdata_capable,
    # ---- host communities ----
    host_comparison = host_signal & is_microdata_capable)

readr::write_excel_csv(
  frame %>% select(example_id, year, country, organisation, lead_type, title, populations,
                   granularity, data_access, id_framework, id_capability,
                   sdg_domains, sdg_disaggregatable, host_signal, host_comparison),
  file.path(LAKE, "lake_frame.csv"))

# ---- coverage / gap matrix ---------------------------------------------------
cov <- frame %>% summarise(
  `examples (total)` = n(),
  `identification: computable` = sum(id_capability == "computable (framework + microdata)"),
  `identification: reported only` = sum(id_capability == "reported only (framework, aggregate)"),
  `SDG: any theme` = sum(sdg_any),
  `SDG: disaggregatable by group` = sum(sdg_disaggregatable),
  `host-community comparison` = sum(host_comparison),
  `microdata-capable` = sum(is_microdata_capable)) %>% pivot_longer(everything(), names_to="capability", values_to="n_examples")
readr::write_excel_csv(cov, file.path(LAKE, "lake_coverage.csv"))

# access x lens crosstab: of the analytically-useful examples, how many are we able to GET?
gap <- frame %>% mutate(useful = has_identification_rec | sdg_any | host_comparison) %>%
  filter(useful) %>% count(data_access, id_capability) %>% arrange(desc(n))
readr::write_excel_csv(gap, file.path(LAKE, "lake_gap_matrix.csv"))

message("==== analysis frame (413 examples) ====")
print(cov, n = 20)
message("\n==== data access x identification capability (analytically-useful examples) ====")
print(frame %>% mutate(useful = has_identification_rec | sdg_any | host_comparison) %>% filter(useful) %>% count(data_access))
message(sprintf("\nwrote %s/lake_frame.csv, lake_coverage.csv, lake_gap_matrix.csv", LAKE))
