# ==============================================================================
# GAIN DATA LAKE - step 12: consolidated DATA-ACCESS reach-out list.
#
# Across every route (trusted roster links, verified recoveries, NSO APIs, web
# search, portal drill, browser recon) some examples now have a data path and some
# do not. This builds the reach-out list = roster examples with NO usable data
# route, classified by why, grouped by reporting office, prioritised by analytical
# value (microdata-capable + identification/SDG). Distinct from the SURVEY outreach:
# same offices, different ask ("please share the data / microdata for example X").
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
rd <- function(f) if (file.exists(file.path(LAKE, f))) suppressMessages(read_csv(file.path(LAKE, f), show_col_types = FALSE)) else tibble()
roster <- suppressMessages(read_csv(file.path(LAKE, "lake_roster.csv"), show_col_types = FALSE))
frame  <- suppressMessages(read_csv(file.path(LAKE, "lake_frame.csv"), show_col_types = FALSE))

prov  <- rd("lake_dataset_provenance.csv")
api   <- rd("lake_api_connectable.csv")
web   <- rd("lake_websearch_found.csv")
drill <- rd("lake_drill.csv")
bpull <- rd("lake_browser_pull.csv")
micro <- rd("lake_microlib_found.csv")            # agency microdata-library self-serve

has_url <- function(x) !is.na(x) & nzchar(x)
resolved <- unique(c(
  if (nrow(prov))  prov$example_id[prov$provenance != "recovered - REJECTED (removed)"] else character(0),
  if (nrow(api))   api$example_id else character(0),
  if (nrow(web))   web$example_id[web$verdict == "MATCH" & has_url(web$found_url)] else character(0),
  if (nrow(drill)) drill$example_id[drill$verdict == "MATCH" & has_url(drill$drilled_url)] else character(0),
  if (nrow(micro)) micro$example_id[micro$verdict == "MATCH"] else character(0),
  if (nrow(bpull)) bpull$example_id else character(0), "ex174"))
# many GAIN "examples" are ACTIVITIES (workshops/training/guidance/methodological
# work), not datasets - nothing to download OR request; they leave the reach-out list.
ACTIVITY <- "workshop|training|webinar|e-?learn|academy|guidance|toolkit|expert group|capacity|methodolog|coordinat|strateg|\\bcourse\\b|quality assurance|global trends|alignment|promoting|supporting|transfer learning|roadmap|\\bframework\\b|note on|advocacy|pledge|data academy"

# portal hint: office located by web-search/drill even if the exact dataset was not reached
portal <- bind_rows(
  if (nrow(web))   web %>% transmute(example_id, hint = found_url) else tibble(),
  if (nrow(drill)) drill %>% transmute(example_id, hint = portal_url) else tibble()) %>%
  filter(has_url(hint)) %>% group_by(example_id) %>% summarise(portal_hint = first(hint), .groups = "drop")

# browser-explored, confirmed NOT publicly available (from the blocked-batch recon)
not_public <- tibble(example_id = c("ex037","ex039","ex136","ex171"),
  note = c("belstat.gov.by navigation denied", "WFP joint assessment not on CAPMAS site",
           "ICASEES: RGPH-4 census still being finalized", "INSTAD site is a JS app, no accessible data"))

unresolved <- roster %>% filter(!example_id %in% resolved) %>% mutate(is_activity = str_detect(tolower(title), ACTIVITY))
activities <- unresolved %>% filter(is_activity)          # out of scope: no data by nature
readr::write_excel_csv(activities %>% transmute(example_id, year, country, organisation, lead_type, title),
                       file.path(LAKE, "lake_no_dataset_activities.csv"))
ex <- unresolved %>% filter(!is_activity) %>%
  left_join(frame %>% select(example_id, id_framework, sdg_domains), by = "example_id") %>%
  left_join(portal, by = "example_id") %>%
  left_join(not_public, by = "example_id") %>%
  mutate(
    reason = case_when(
      !is.na(note) ~ note,
      has_url(portal_hint) ~ "office portal located, specific dataset not reachable (blocked/forthcoming)",
      lead_type == "institution-led" ~ "institution-led; data held by the reporting body",
      TRUE ~ "no usable data link found; request from the reporting office"),
    request = ifelse(is_microdata_capable, "microdata + documentation (DDI)", "the dataset / tables behind this example"),
    priority = (is_microdata_capable) * 2 + (nzchar(coalesce(id_framework,"")) | nzchar(coalesce(sdg_domains,""))) * 1)
readr::write_excel_csv(
  ex %>% transmute(example_id, year, country, organisation, lead_type, title, populations,
                   recommendations, is_microdata_capable, priority, request, reason,
                   portal_hint = coalesce(portal_hint, "")),
  file.path(LAKE, "lake_reachout_examples.csv"))

# grouped by reporting office (one ask per office)
office <- ex %>% group_by(country, organisation, lead_type) %>%
  summarise(n_examples = n(),
            priority = max(priority),
            microdata_examples = sum(is_microdata_capable, na.rm = TRUE),
            populations = paste(sort(unique(unlist(str_split(populations, ";")))), collapse = ";"),
            frameworks = paste(sort(unique(recommendations[nzchar(coalesce(recommendations,""))])), collapse = "; "),
            portal = first(na.omit(portal_hint[has_url(portal_hint)])),
            example_ids = paste(example_id, collapse = ";"),
            reasons = paste(sort(unique(reason)), collapse = " | "), .groups = "drop") %>%
  arrange(desc(priority), desc(microdata_examples), desc(n_examples))
readr::write_excel_csv(office, file.path(LAKE, "lake_reachout_offices.csv"))

message(sprintf("==== DATA-ACCESS reach-out list ===="))
message(sprintf("resolved (a data route exists): %d of %d roster examples", length(intersect(resolved, roster$example_id)), nrow(roster)))
message(sprintf("no dataset - ACTIVITIES (workshops/training/guidance, out of scope): %d", nrow(activities)))
message(sprintf("REACH-OUT: %d examples across %d offices", nrow(ex), nrow(office)))
message("\nby lead type:"); print(count(ex, lead_type))
message("\nby reason:"); print(ex %>% count(reason, sort = TRUE))
message(sprintf("\nhigh-priority offices (microdata + identification/SDG): %d", sum(office$priority == 3)))
message("\ntop 12 offices to approach first:")
print(office %>% filter(priority == 3) %>% transmute(country = substr(country,1,18), organisation = substr(organisation,1,34),
      n_examples, microdata_examples) %>% head(12) %>% as.data.frame(), right = FALSE)
message(sprintf("\nwrote lake_reachout_offices.csv (%d offices) and lake_reachout_examples.csv (%d examples)", nrow(office), nrow(ex)))
