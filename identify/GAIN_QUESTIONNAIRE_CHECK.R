# ==============================================================================
# GAIN QUESTIONNAIRE CHECK  (does the survey actually ASK about displacement?)
#
# Why: the reviewer calibration (Sep 2026) showed the AI scoring catalogue
# records a flat 40 / "not counted" - e.g. Nigeria DHS 2024, which has a forced-
# displacement question, and Lesotho DHS 2023-24, which has none. The AI only
# saw the catalogue page. The questionnaire decides, so read it.
#
# For every candidate whose URL is a NADA catalogue entry (IHSN, World Bank,
# UNHCR microdata libraries: .../index.php/catalog/<id>), this fetches the DDI
# metadata (variable names, labels, question text, study abstract) and searches
# it for displacement concepts in several languages.
#
# Output: questionnaire_check_[date].csv  (url, ddi_found, n_var_hits,
#         hit_examples, abstract_hit). GAIN_FINALIZE.R reads the newest one:
#           variable hits      -> "reach out" (reviewer: a DHS forced-displacement
#                                 question is enough to ask) + SDG hand-off with
#                                 the variable names, for our own analysis
#           DDI read, no hits  -> "low - no displacement question"
#           no DDI             -> unchanged
# Cached per study in ddi_cache/ (polite: one request per study, ever).
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(xml2) })

CACHE <- "ddi_cache"; dir.create(CACHE, showWarnings = FALSE)

src <- tail(sort(list.files(".", "^GAIN_MATCH_V2_.*\\.csv$")), 1)
if (!length(src)) src <- tail(sort(list.files(".", "^evidence_flagged_.*\\.csv$")), 1)
stopifnot(length(src) == 1)
d <- suppressMessages(read_csv(src, show_col_types = FALSE, guess_max = 5000))

# displacement concepts, searched in variable labels, question text AND answer
# options. Only clear terms count: the decisive evidence is often an ANSWER
# option, e.g. Nigeria DHS 2024 v175 "Reason for moving to current place of
# residence" -> "... | Forced displacement | Other". Vaguer words (conflict,
# insecurity, nationality, camp) are left out: "food insecurity" is not displacement.
DISP_PAT <- regex(paste0(
  "refug|asylum|displac|\\bidps?\\b|forced to (leave|move|flee)|\\bfled\\b|\\bflee\\b|returnee|",
  "r[eé]fugi|d[eé]plac|\\basile\\b|desplazad|refugiad|",
  "\u0644\u0627\u062c\u0626|\u0646\u0627\u0632\u062d"), ignore_case = TRUE)
# statelessness (reviewer, Sep 2026: "if there is data on statelessness we would be
# interested") - usually an ANSWER option of a nationality/citizenship question
STATELESS_PAT <- regex(paste0(
  "stateless|apatrid|no nationality|without (a )?nationality|undetermined nationality|",
  "nationality (unknown|undetermined|not determined)|no citizenship|without citizenship|",
  "sans nationalit|nationalit[eé] ind[eé]termin|sin nacionalidad|nacionalidad indeterminada|",
  "\u0628\u062f\u0648\u0646 \u062c\u0646\u0633\u064a\u0629"), ignore_case = TRUE)

cat_rows <- d %>%
  transmute(url = as.character(url)) %>% distinct() %>%
  mutate(base = str_match(url, "^(https?://[^/]+)(/[^?#]*)?/index\\.php/catalog/([0-9]+)")[, 2],
         path = coalesce(str_match(url, "^https?://[^/]+(/[^?#]*)?/index\\.php/catalog/[0-9]+")[, 2], ""),
         cat_id = str_match(url, "/index\\.php/catalog/([0-9]+)")[, 2]) %>%
  filter(!is.na(cat_id))
message(sprintf("Catalogue candidates: %d (from %s)", nrow(cat_rows), src))

get_ddi <- function(base, path, id) {
  host <- str_remove(base, "^https?://")
  f <- file.path(CACHE, paste0(str_replace_all(host, "[^A-Za-z0-9]", "_"), "_", id, ".xml"))
  if (file.exists(f)) { x <- readLines(f, warn = FALSE, encoding = "UTF-8"); return(if (length(x)) paste(x, collapse = "\n") else NA_character_) }
  u <- paste0(base, path, "/index.php/metadata/export/", id, "/ddi")
  txt <- tryCatch(request(u) |> req_timeout(60) |> req_user_agent("EGRISS GAIN research (UNHCR)") |>
                    req_retry(max_tries = 2) |> req_perform() |> resp_body_string(),
                  error = function(e) NA_character_)
  if (is.na(txt) || nchar(txt) < 500 || !str_detect(txt, "<codeBook|<ddi:codeBook")) txt <- ""
  writeLines(txt, f, useBytes = TRUE); Sys.sleep(1)
  if (nzchar(txt)) txt else NA_character_
}

check_one <- function(base, path, id) {
  ddi <- get_ddi(base, path, id)
  if (is.na(ddi)) return(tibble(ddi_found = FALSE, n_var = NA_integer_, n_var_hits = NA_integer_,
                                populations = NA_character_, hit_vars = NA_character_,
                                hit_examples = NA_character_, abstract_hit = NA))
  x <- tryCatch(read_xml(ddi), error = function(e) NULL)
  if (is.null(x)) return(tibble(ddi_found = FALSE, n_var = NA_integer_, n_var_hits = NA_integer_,
                                populations = NA_character_, hit_vars = NA_character_,
                                hit_examples = NA_character_, abstract_hit = NA))
  xml_ns_strip(x)
  vars <- xml_find_all(x, ".//var")
  vtxt <- vapply(vars, function(v) str_squish(paste(
    xml_attr(v, "name"), xml_text(xml_find_first(v, "./labl")),
    coalesce(xml_text(xml_find_first(v, ".//qstnLit")), ""),
    "[", paste(xml_text(xml_find_all(v, ".//catgry/labl")), collapse = " | "), "]")), character(1))
  disp <- str_detect(vtxt, DISP_PAT)
  stl  <- str_detect(vtxt, STATELESS_PAT)
  strong <- disp | stl
  abst <- paste(xml_text(xml_find_all(x, ".//stdyDscr//abstract | .//stdyDscr//universe | .//stdyDscr//titl")), collapse = " ")
  tibble(ddi_found = TRUE, n_var = length(vars), n_var_hits = sum(strong),
         populations = paste(c(if (any(disp)) "displacement", if (any(stl)) "statelessness"), collapse = " + "),
         hit_vars = paste(head(xml_attr(vars[strong], "name"), 10), collapse = ", "),
         hit_examples = paste(head(str_sub(vtxt[strong], 1, 160), 5), collapse = " || "),
         abstract_hit = str_detect(abst, DISP_PAT) | str_detect(abst, STATELESS_PAT))
}

res <- cat_rows %>% mutate(r = pmap(list(base, path, cat_id), check_one)) %>% unnest(r) %>%
  mutate(questionnaire_verdict = case_when(
    !ddi_found                      ~ "no DDI available",
    n_var_hits > 0                  ~ "displacement/statelessness questions found",
    n_var == 0 & abstract_hit       ~ "no variable list; abstract mentions displacement",
    n_var == 0                      ~ "no variable list",
    TRUE                            ~ "no displacement question"))

out <- sprintf("questionnaire_check_%s.csv", format(Sys.Date(), "%Y%m%d"))
readr::write_excel_csv(res %>% select(-base, -path), out)
message("\n==== questionnaire verdicts ====")
print(count(res, questionnaire_verdict, sort = TRUE))
message("wrote ", out)
