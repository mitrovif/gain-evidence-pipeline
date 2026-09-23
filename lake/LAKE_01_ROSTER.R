# ==============================================================================
# GAIN DATA LAKE - step 1a: canonical roster of examples ALREADY IN GAIN
#
# Spine = analysis_ready_group_roster.csv (413 examples reported to GAIN, all
# rounds 2021-2025). This is NOT the web-scraper's *potential* candidates; it is
# what countries and institutions actually reported. We normalise the coded
# columns into a clean roster the lake is built on.
#
# Column map (from the "EGRISS Sec Examples Collected" questionnaire):
#   PRO03 title | PRO04 year started | PRO05 year finished | PRO06/phase phase |
#   PRO07 populations | PRO08_label data source/tool | recommendations IRRS/IRIS/IROSS |
#   PRO13 description | PRO14A link to RESULTS | PRO16 link to QUESTIONNAIRE |
#   g_conled 1=country-led 2=institution-led | mcountry | morganization
# ==============================================================================
suppressMessages({ library(tidyverse) })
source("shared/GAIN_COMMON.R")
LAKE <- "data_lake"; dir.create(LAKE, showWarnings = FALSE, recursive = TRUE)
ROSTER_IN <- "analysis_ready_group_roster.csv"
stopifnot(file.exists(ROSTER_IN))
r <- suppressMessages(read_csv(ROSTER_IN, show_col_types = FALSE))

g <- function(col) if (col %in% names(r)) as.character(r[[col]]) else rep(NA_character_, nrow(r))
urls_in <- function(s) str_extract_all(coalesce(s, ""), "https?://[^\\s,;\"'<>\\]\\)]+")
# pool every URL-bearing field; PRO14A is the primary "link to results"
url_fields <- c("PRO14A", "PRO16", "PRO13", "PRO03", "PRO02A")
all_urls_list <- pmap(map(url_fields, g), function(...) unique(unlist(urls_in(c(...)))))
n_url_vec <- lengths(all_urls_list)
primary  <- map_chr(urls_in(g("PRO14A")), ~ if (length(.x)) .x[[1]] else NA_character_)

POP <- function(x) {                         # PRO07 tokens -> tidy population set
  toks <- str_split(toupper(coalesce(x, "")), "\\s+")[[1]]; toks <- toks[nzchar(toks)]
  map <- c(REFUGEES="refugees", IDPS="idps", STATELESSNESS="stateless", OTHER="other")
  paste(unique(unname(map[toks[toks %in% names(map)]])), collapse = ";")
}
host_kw <- "host communit|host population|receiving communit|non-displaced|host and refugee|réfugiés et hôtes|comunidad de acogida|host household"
lead_lbl <- c(`1` = "country-led", `2` = "institution-led", `3` = "other")

roster <- tibble(
  example_id  = sprintf("ex%03d", seq_len(nrow(r))),          # guaranteed-unique lake key
  gain_pindex2 = na_if(g("pindex2"), ""),                     # GAIN reference (NOT unique)
  year        = suppressWarnings(as.integer(g("year"))),
  lead_type   = unname(coalesce(lead_lbl[g("g_conled")], "unknown")),
  country     = str_squish(coalesce(g("mcountry"), "")),
  organisation= str_squish(coalesce(g("morganization"), "")),
  title       = str_squish(coalesce(g("PRO03"), "")),
  populations = map_chr(g("PRO07"), POP),
  source_tool = str_squish(tolower(coalesce(g("PRO08_label"), ""))),
  recommendations   = str_squish(toupper(coalesce(g("recommendations"), ""))),
  n_recommendations = suppressWarnings(as.integer(g("count_recommendations"))),
  phase       = str_squish(tolower(coalesce(na_if(g("phase"), ""), g("PRO06")))),
  description = str_squish(coalesce(g("PRO13"), "")),
  link_results       = primary,
  link_questionnaire = map_chr(urls_in(g("PRO16")), ~ if (length(.x)) .x[[1]] else NA_character_),
  all_urls    = map_chr(all_urls_list, ~ paste(.x, collapse = " ")),
  n_urls      = n_url_vec,
  egriss_region = str_squish(coalesce(g("egriss_region"), "")),
  unhcr_region  = str_squish(coalesce(g("unhcr_region"), ""))) %>%
  mutate(
    country = ifelse(nzchar(country), harmonize_country(country), country),
    # identification framework signalled by the recommendation family
    irrs = str_detect(recommendations, "IRRS"), iris = str_detect(recommendations, "IRIS"),
    iross = str_detect(recommendations, "IROSS"),
    has_identification_rec = irrs | iris | iross,
    # data nature -> can we in principle compute indicators / cross-tabs?
    is_microdata_capable = str_detect(source_tool, "survey|census|administrative|integration|register"),
    host_signal = str_detect(tolower(paste(title, description)), host_kw) | str_detect(populations, "other"),
    has_link = n_urls > 0)

readr::write_excel_csv(roster, file.path(LAKE, "lake_roster.csv"))
message(sprintf("lake_roster.csv: %d examples | %d with a link | %d microdata-capable | %d with IRRS/IRIS/IROSS | %d host-signal",
        nrow(roster), sum(roster$has_link), sum(roster$is_microdata_capable), sum(roster$has_identification_rec), sum(roster$host_signal)))
print(roster %>% count(year, has_link) %>% pivot_wider(names_from = has_link, values_from = n, names_prefix = "link_"))
