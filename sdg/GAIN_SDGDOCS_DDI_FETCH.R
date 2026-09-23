# ==============================================================================
# GAIN SDG DDI FETCH  (workstream stage 5 prep - which flagged datasets could
# actually support a demonstration tabulation?)
#
# For every microdata-catalogue entry in the harvest manifest (NADA catalogues:
# UNHCR / IHSN / World Bank), this fetches the study metadata JSON and the full
# DDI XML variable dictionary - WITHOUT downloading any data - and answers:
#   * does the dataset have a DISPLACEMENT/STATELESSNESS identifier variable?
#     (the prerequisite for tabulating "for refugees/IDPs/stateless")
#   * does it have survey WEIGHT variables? (prerequisite for valid estimates)
#   * which priority-indicator INPUTS do its variables cover (birth
#     registration, electricity, labour status, anthropometry, water, ...)?
#   * what is the declared ACCESS policy? (licensed vs public use - respected)
#
# Output: sdg_docs/SDG_MICRODATA_INVENTORY.csv - joined with SDG_GAP_MATRIX.csv
# this yields the stage-5 pilot shortlist. Cached (sdg_docs/ddi_cache/), polite,
# resumable. Test mode: Sys.setenv(GAIN_DDI_LIMIT = "5").
# ==============================================================================

suppressMessages({ library(tidyverse); library(httr2); library(xml2); library(jsonlite) })
source("shared/GAIN_COMMON.R")

DIR   <- "sdg_docs"
CACHE <- file.path(DIR, "ddi_cache"); dir.create(CACHE, showWarnings = FALSE)
OUT   <- file.path(DIR, "SDG_MICRODATA_INVENTORY.csv")
LIMIT <- suppressWarnings(as.integer(Sys.getenv("GAIN_DDI_LIMIT", "0")))

# indicator-input variable signals (searched in variable names + labels)
INPUT_SIGNALS <- c(
  "16.9.1 birth registration" = "birth.{0,20}regist|regist.{0,20}birth",
  "7.1.1 electricity"         = "electric",
  "8.5.2/8.3.1 labour"        = "unemploy|employ|labou?r force|informal",
  "2.2.1 anthropometry"       = "stunt|height|anthropom|nutrition",
  "6.1.1 drinking water"      = "drinking water|water source",
  "1.2.1 poverty/consumption" = "poverty|consumption|expenditure",
  "4.1.1 learning"            = "literacy|numeracy|reading|mathematic",
  "11.1.1 housing"            = "housing|dwelling|slum",
  "1.4.2 tenure"              = "tenure|land right|land ownership",
  "16.1.4 safety"             = "feel.{0,10}safe|safety",
  "16.b.1 discrimination"     = "discriminat|harass",
  "3.1.2 birth attendance"    = "birth attend|skilled.{0,15}(birth|delivery)|delivery assist")
DISP_PAT   <- "refugee|displac|\\bidp|asylum|stateless|nationality|citizen|migrat"
WEIGHT_PAT <- "weight|\\bwgt|\\bpond"

# ---- catalogue entries from the manifest --------------------------------------
man <- read_csv(file.path(DIR, "SDG_DOCS_MANIFEST.csv"), show_col_types = FALSE) %>%
  filter(status == "ok", str_detect(coalesce(url, ""), "/catalog/[0-9]+")) %>%
  mutate(base = str_match(url, "^(https?://[^/]+)")[, 2],
         cat_id = str_match(url, "/catalog/([0-9]+)")[, 2]) %>%
  filter(!is.na(base), !is.na(cat_id)) %>%
  group_by(base, cat_id) %>%
  summarise(countries = paste(unique(country), collapse = ";"),
            title_hint = first(title), url = first(url),
            populations = paste(unique(na.omit(populations)), collapse = ";"),
            .groups = "drop")
done <- if (file.exists(OUT)) read_csv(OUT, show_col_types = FALSE) %>%
          mutate(k = paste(base, cat_id)) %>% pull(k) else character(0)
todo <- man %>% filter(!paste(base, cat_id) %in% done)
if (LIMIT > 0) todo <- head(todo, LIMIT)
message(nrow(man), " catalogue entries | ", length(done), " done | ", nrow(todo), " to fetch",
        if (LIMIT > 0) paste0(" (LIMIT ", LIMIT, ")") else "")

get_txt <- function(url) {
  r <- tryCatch(request(url) %>% req_user_agent("EGRISS-GAIN-research (SDG documentation)") %>%
                  req_timeout(45) %>% req_error(is_error = function(x) FALSE) %>% req_perform(),
                error = function(e) NULL)
  if (is.null(r) || resp_status(r) >= 400) return(NA_character_)
  tryCatch(resp_body_string(r), error = function(e) NA_character_)
}

for (i in seq_len(nrow(todo))) {
  r <- todo[i, ]
  ck <- file.path(CACHE, paste0(rlang::hash(paste(r$base, r$cat_id)), ".rds"))
  if (file.exists(ck)) { dat <- readRDS(ck) } else {
    study_json <- get_txt(paste0(r$base, "/index.php/api/catalog/", r$cat_id, "?id_format=id"))
    Sys.sleep(1)
    ddi_xml <- get_txt(paste0(r$base, "/index.php/metadata/export/", r$cat_id, "/ddi"))
    Sys.sleep(1)
    dat <- list(study = study_json, ddi = ddi_xml)
    saveRDS(dat, ck)
  }
  idno <- title <- access <- nation <- NA_character_
  if (!is.na(dat$study)) {
    j <- tryCatch(fromJSON(dat$study, simplifyVector = FALSE), error = function(e) NULL)
    ds <- j$dataset
    if (!is.null(ds)) {
      idno <- as.character(ds$idno %||% NA); title <- as.character(ds$title %||% NA)
      nation <- as.character(ds$nation %||% NA)
      access <- as.character(ds$data_access_type %||% ds$dataset_access %||% NA)
    }
  }
  n_vars <- 0L; has_w <- FALSE; has_d <- FALSE; hits <- character(0)
  if (!is.na(dat$ddi) && nchar(dat$ddi) > 500) {
    x <- tryCatch(read_xml(dat$ddi), error = function(e) NULL)
    if (!is.null(x)) {
      vars <- xml_find_all(x, ".//*[local-name()='var']")
      n_vars <- length(vars)
      lab <- str_to_lower(paste(xml_attr(vars, "name"),
        vapply(vars, function(v) paste(xml_text(
          xml_find_all(v, ".//*[local-name()='labl']")), collapse = " "), character(1))))
      lab_all <- paste(lab, collapse = " || ")
      has_w <- str_detect(lab_all, WEIGHT_PAT)
      has_d <- str_detect(lab_all, DISP_PAT)
      hits <- names(INPUT_SIGNALS)[vapply(INPUT_SIGNALS, function(p)
        str_detect(lab_all, regex(p, ignore_case = TRUE)), logical(1))]
    }
  }
  write_csv(tibble(base = r$base, cat_id = r$cat_id, idno = idno,
                   title = coalesce(title, r$title_hint), countries = r$countries,
                   nation = nation, populations = r$populations,
                   access_type = access, n_vars = n_vars,
                   has_weight_var = has_w, has_displacement_var = has_d,
                   indicator_inputs = paste(hits, collapse = " | "),
                   n_indicator_inputs = length(hits), url = r$url,
                   ddi_available = !is.na(dat$ddi) && nchar(dat$ddi) > 500),
            OUT, append = file.exists(OUT))
  if (i %% 20 == 0) message("  [", i, "/", nrow(todo), "]")
}

inv <- read_csv(OUT, show_col_types = FALSE)
message("\n==================== MICRODATA INVENTORY ====================")
message("datasets: ", nrow(inv), " | DDI dictionaries readable: ", sum(inv$ddi_available),
        " | with displacement variable: ", sum(inv$has_displacement_var),
        " | with weights: ", sum(inv$has_weight_var))
message("PILOT-READY (DDI + displacement var + weights + >=1 indicator input): ",
        sum(inv$ddi_available & inv$has_displacement_var & inv$has_weight_var &
            inv$n_indicator_inputs > 0))
message("access types: ", paste(names(table(inv$access_type)), table(inv$access_type),
                                sep = "=", collapse = " | "))
message("Output: ", OUT)
