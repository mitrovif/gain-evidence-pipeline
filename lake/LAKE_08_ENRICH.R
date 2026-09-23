# ==============================================================================
# GAIN DATA LAKE - step 6: frame ENRICHMENT -> indicator-capability matrix.
#
# For every downloaded example, read its evidence and decide what it can actually
# produce, for identification/EGRISS categories and SDGs. Evidence is ranked by
# strength:  microdata variable (DDI)  >  table/data column  >  text mention.
#   - microdata var  = the study carries the variable  -> COMPUTABLE
#   - table column   = an aggregate table has it        -> aggregate-computable
#   - text mention   = the report discusses it          -> evidenced (topic only)
# Bilingual patterns (EN/FR/ES) because the corpus is multilingual.
# Output: lake_indicator_matrix.csv (long), lake_frame_enriched.csv (per example),
#         lake_indicator_coverage.csv (how many examples reach each indicator).
# ==============================================================================
suppressMessages({ library(tidyverse); library(readxl) })
LAKE <- "data_lake"; STORE <- file.path(LAKE, "store")
roster <- suppressMessages(read_csv(file.path(LAKE, "lake_roster.csv"), show_col_types = FALSE))
frame  <- suppressMessages(read_csv(file.path(LAKE, "lake_frame.csv"), show_col_types = FALSE))

# ---- concept dictionary (bilingual regex) ------------------------------------
ID <- list(
  country_of_birth   = "country of birth|place of birth|pays de naissance|lieu de naissance|pa[ií]s de nacimiento|born abroad|foreign.?born",
  citizenship        = "citizenship|nationalit|certificat de nationalit|ciudadan|nacionalidad",
  legal_status       = "legal status|residence (permit|status)|permis de s[eé]jour|titre de s[eé]jour|asylum (status|seeker)|refugee status|statut de r[eé]fugi[eé]|estat[uo]s (legal|migratori)|regulariz|documentation status",
  displacement_reason= "reason for (leaving|migrat|displac|moving|flight)|raison (du|de) (d[eé]part|migrat)|motivo (de|del) (desplaz|migrac)|cause of displacement|forced to (leave|flee)|reason.*fled|why.*(left|fled)",
  refugee            = "refugee|r[eé]fugi[eé]|refugiad",
  asylum_seeker      = "asylum|asile|asilo|demandeur d.asile|solicitante de asilo",
  idp                = "internally displaced|\\bidps?\\b|d[eé]plac[eé].*int[eé]rieur|desplazad.*intern|personnes d[eé]plac[eé]es intern",
  stateless          = "stateless|apatrid|sans nationalit|statelessness",
  returnee           = "returnee|returned (refugee|migrant|idp)|return migra|rapatri|retornad|migrant.*retour",
  host_community     = "host (communit|population|household|countr)|communaut[eé] d.accueil|comunidad de acogida|receiving (communit|population)|non.?displaced",
  arrival_duration   = "year of arrival|arrival (year|date)|when did.*arrive|ann[eé]e d.arriv[eé]e|a[nñ]o de llegada|duration of (stay|residence)|length of (stay|residence)|time since arrival")
SDG <- list(
  "SDG1 poverty"        = "poverty|income|livelihood|pauvret[eé]|revenu|pobreza|ingreso|destitut|poor household",
  "SDG3 health"         = "\\bhealth\\b|sant[eé]|salud|mortalit|vaccin|maternal|nutrition|malnutri|morbidit",
  "SDG4 education"      = "education|\\bschool\\b|enrol|literacy|scolaris|\\b[eé]cole\\b|[eé]duca|escuela|alfabet|attending.*school|out of school",
  "SDG5 gender"         = "\\bgender\\b|\\bsex\\b|\\bwomen\\b|\\bgirls\\b|female|genre|sexe|femmes|g[eé]nero|mujer|gender.based violence|\\bgbv\\b",
  "SDG6 WASH"           = "\\bwater\\b|sanitation|hygiene|\\bwash\\b|drinking water|assainissement|eau potable|\\bagua\\b|saneamiento|latrine|\\btoilet",
  "SDG8 work"           = "employ|labour|labor|unemploy|\\bwage|occupation|emploi|travail|ch[oô]mage|empleo|trabajo|desemple|informal sector",
  "SDG10 inclusion"     = "social protection|social inclusion|discrimination|integration|protection sociale|inclusi[oó]n|assistance program",
  "SDG11 housing"       = "housing|shelter|settlement|accommodation|logement|h[eé]bergement|vivienda|alojamiento|\\bcamp\\b|\\bslum\\b|dwelling|tenure",
  "SDG16 legal identity"= "birth registration|civil registration|legal identity|identity document|enregistrement des naissances|acte de naissance|registro de nacimiento|pi[eè]ce d.identit[eé]|certificat de nationalit|nationality certificate")

# ---- gather each example's evidence -------------------------------------------
txt_of <- function(d) { f <- file.path(d, "text.txt"); if (file.exists(f)) tolower(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " ")) else "" }
vars_of <- function(d) { vs <- list.files(d, pattern = "nada_.*_variables\\.csv$", full.names = TRUE); if (!length(vs)) return("")
  tolower(paste(map_chr(vs, function(f) { x <- tryCatch(suppressMessages(read_csv(f, show_col_types = FALSE)), error = function(e) NULL)
    if (is.null(x)) "" else paste(coalesce(x$name,""), coalesce(x$label,""), coalesce(x$question,""), collapse = " ") }), collapse = " ")) }
cols_of <- function(d) { cols <- character(0)
  for (f in list.files(d, pattern = "^(table|dataset)_.*\\.csv$", full.names = TRUE))
    cols <- c(cols, tryCatch(names(suppressMessages(read_csv(f, n_max = 0, show_col_types = FALSE))), error = function(e) character(0)))
  for (f in list.files(d, pattern = "^dataset_.*\\.xlsx?$", full.names = TRUE))
    cols <- c(cols, tryCatch(names(suppressMessages(read_excel(f, n_max = 0))), error = function(e) character(0)))
  tolower(paste(cols, collapse = " | ")) }

detect <- function(patterns, micro, cols, text) {
  map_chr(patterns, function(p) {
    if (str_detect(micro, p)) "microdata" else if (str_detect(cols, p)) "table" else if (str_detect(text, p)) "text" else "none" }) }

ids <- roster$example_id[dir.exists(file.path(STORE, roster$example_id))]
long <- list()
for (ex in ids) {
  d <- file.path(STORE, ex); micro <- vars_of(d); cols <- cols_of(d); text <- txt_of(d)
  if (!nzchar(micro) && !nzchar(cols) && !nzchar(text)) next
  idl  <- detect(ID,  micro, cols, text); sdgl <- detect(SDG, micro, cols, text)
  long[[length(long)+1]] <- bind_rows(
    tibble(example_id = ex, group = "identification", concept = names(ID),  level = idl),
    tibble(example_id = ex, group = "sdg",            concept = names(SDG), level = sdgl)) %>% filter(level != "none")
}
mat <- bind_rows(long)
readr::write_excel_csv(mat, file.path(LAKE, "lake_indicator_matrix.csv"))

# ---- per-example enriched frame ----------------------------------------------
rank_lvl <- c(none = 0, text = 1, table = 2, microdata = 3)
best <- function(v) names(rank_lvl)[which.max(c(0, rank_lvl[v]))]
enr <- mat %>% group_by(example_id) %>% summarise(
  id_best   = { v <- level[group=="identification"]; if (length(v)) names(which.max(rank_lvl[unique(v)])) else "none" },
  id_microdata_vars = sum(group=="identification" & level=="microdata"),
  egriss_categories = paste(sort(unique(concept[group=="identification" & concept %in% c("refugee","asylum_seeker","idp","stateless","returnee","host_community")])), collapse=";"),
  host_evidenced    = any(concept=="host_community"),
  sdg_computable = paste(sort(unique(concept[group=="sdg" & level %in% c("microdata","table")])), collapse="; "),
  sdg_evidenced  = paste(sort(unique(concept[group=="sdg"])), collapse="; "),
  n_sdg_computable = n_distinct(concept[group=="sdg" & level %in% c("microdata","table")]),
  .groups="drop")
enriched <- frame %>% left_join(enr, by = "example_id") %>%
  mutate(across(c(id_microdata_vars, n_sdg_computable), ~replace_na(., 0L)),
         id_best = replace_na(id_best, "none"),
         id_capability_enriched = case_when(
           id_microdata_vars >= 3 ~ "computable (microdata)",
           id_best == "microdata" ~ "partial (microdata)",
           id_best == "table"     ~ "aggregate table",
           id_best == "text"      ~ "text evidence only",
           TRUE ~ "none in downloaded data"))
readr::write_excel_csv(enriched %>% select(example_id, year, country, organisation, title, data_access,
  id_capability_enriched, id_microdata_vars, egriss_categories, host_evidenced,
  n_sdg_computable, sdg_computable, sdg_evidenced), file.path(LAKE, "lake_frame_enriched.csv"))

# ---- coverage summary ---------------------------------------------------------
cov <- mat %>% mutate(strong = level %in% c("microdata","table")) %>%
  group_by(group, concept) %>% summarise(examples_any = n_distinct(example_id),
    examples_computable = n_distinct(example_id[strong]), .groups="drop") %>% arrange(group, desc(examples_computable))
readr::write_excel_csv(cov, file.path(LAKE, "lake_indicator_coverage.csv"))

message(sprintf("==== frame enrichment: %d examples with downloaded evidence ====", n_distinct(mat$example_id)))
message("\nidentification capability (enriched):"); print(count(enriched, id_capability_enriched, sort=TRUE))
message("\nidentification concept coverage (examples that can COMPUTE it, i.e. microdata/table):")
print(cov %>% filter(group=="identification") %>% select(concept, examples_computable, examples_any))
message("\nSDG concept coverage:")
print(cov %>% filter(group=="sdg") %>% select(concept, examples_computable, examples_any))
message(sprintf("\nwrote lake_indicator_matrix.csv, lake_frame_enriched.csv, lake_indicator_coverage.csv"))
