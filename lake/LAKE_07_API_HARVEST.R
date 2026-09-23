# ==============================================================================
# GAIN DATA LAKE - step 5: harvest the DATASETS behind the portal/API links.
#
# The portal links resolve to three systems; the analytically decisive one is the
# NADA microdata catalogues (UNHCR / IHSN / World Bank), which expose the full
# DDI metadata + VARIABLE INVENTORY per study. Working endpoints (probed):
#   * DDI XML export : <host>/index.php/metadata/export/<id>/ddi   -> all variables
#   * study metadata : <host>/index.php/api/catalog/search?id=<id> -> idno/title/access
# The DDI variable list is what upgrades the frame to "computable" for
# identification & SDG. SSB's old JSON-stat API is 410-gone (needs the new PxWeb
# v2 POST API) and is only 5 examples -> recorded, not harvested here.
# Output per example in data_lake/store/<id>/ ; index lake_api_manifest.csv.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite); library(xml2) })
LAKE <- "data_lake"; STORE <- file.path(LAKE, "store")
l <- suppressMessages(read_csv(file.path(LAKE, "lake_dataset_links.csv"), show_col_types = FALSE)) %>% filter(kind == "portal")
asset <- "[.](css|js|png|jpe?g|svg|gif|ico|woff2?|ttf)([?#]|$)|/themes/|/static/|/assets/|/css/|/js/"
l <- l %>% filter(!str_detect(tolower(dataset_url), asset)) %>% mutate(host = tolower(str_match(dataset_url, "^https?://([^/]+)")[,2]))

nada <- l %>% filter(str_detect(host, "microdata.unhcr.org|catalog.ihsn.org|microdata.worldbank.org"),
                     str_detect(dataset_url, "/catalog/\\d+")) %>%
  mutate(base = paste0("https://", host), study = str_match(dataset_url, "/catalog/(\\d+)")[,2]) %>%
  distinct(base, study, .keep_all = TRUE)
ssb <- l %>% filter(str_detect(dataset_url, "ssb\\.no/.*sq/\\d+")) %>% mutate(sq = str_match(dataset_url,"/sq/(\\d+)")[,2]) %>% distinct(sq, .keep_all=TRUE)
message(sprintf("targets -> NADA studies: %d (%d examples) | SSB sq (deferred): %d", nrow(nada), n_distinct(nada$example_id), nrow(ssb)))

MAN <- file.path(LAKE, "lake_api_manifest.csv"); if (file.exists(MAN)) file.remove(MAN)
GET <- function(u) tryCatch(request(u) |> req_timeout(45) |> req_headers(`User-Agent`="Mozilla/5.0 EGRISS-GAIN-lake") |>
    req_options(followlocation = TRUE, ssl_verifypeer = 0) |> req_error(is_error = \(x) FALSE) |> req_perform(), error = function(e) NULL)
sbody <- function(rp) tryCatch(resp_body_string(rp), error = function(e) "")

parse_ddi <- function(path) {
  x <- tryCatch(read_xml(path), error = function(e) NULL); if (is.null(x)) return(tibble())
  xml_ns_strip(x); vars <- xml_find_all(x, "//var"); if (!length(vars)) return(tibble())
  tibble(name  = xml_attr(vars, "name"),
         label = xml_text(xml_find_first(vars, ".//labl")),
         question = xml_text(xml_find_first(vars, ".//qstnLit"))) %>% filter(!is.na(name) | nzchar(label))
}

n_ddi <- 0; n_meta <- 0
for (i in seq_len(nrow(nada))) {
  ex <- nada$example_id[i]; base <- nada$base[i]; id <- nada$study[i]
  d <- file.path(STORE, ex); dir.create(d, showWarnings = FALSE)
  # study metadata via search-by-id
  idno <- NA_character_; title <- NA_character_; access <- NA_character_; kind <- NA_character_
  rp <- GET(sprintf("%s/index.php/api/catalog/search?id=%s", base, id))
  if (!is.null(rp) && resp_status(rp) < 400) { j <- tryCatch(fromJSON(sbody(rp)), error = function(e) NULL)
    rows <- j$result$rows
    if (!is.null(rows) && length(rows)) { r1 <- if (is.data.frame(rows)) rows[1,] else as_tibble(rows)[1,]
      idno <- as.character(r1$idno %||% NA); title <- as.character(r1$title %||% NA)
      kind <- as.character(r1$type %||% NA); n_meta <- n_meta + 1 } }
  # DDI export -> variable inventory
  nv <- 0L; ddi_path <- ""
  rp <- GET(sprintf("%s/index.php/metadata/export/%s/ddi", base, id))
  if (!is.null(rp) && resp_status(rp) < 400 && length(resp_body_raw(rp)) > 500) {
    ddi_path <- file.path(d, sprintf("nada_%s_ddi.xml", id)); writeBin(resp_body_raw(rp), ddi_path)
    v <- parse_ddi(ddi_path)
    if (nrow(v)) { readr::write_excel_csv(v, file.path(d, sprintf("nada_%s_variables.csv", id))); nv <- nrow(v); n_ddi <- n_ddi + 1 }
  }
  readr::write_csv(tibble(example_id = ex, system = "NADA", host = nada$host[i], study = id, idno = idno,
    title = substr(coalesce(title, ""), 1, 80), kind = kind, n_variables = nv,
    ddi = basename(ddi_path), status = if (nv > 0) "variables" else if (!is.na(idno)) "meta_only" else "miss"),
    MAN, append = file.exists(MAN))
  message(sprintf("  [%d/%d] %s study %s | %d vars | %s", i, nrow(nada), ex, id, nv, substr(coalesce(title,""),1,42)))
  Sys.sleep(0.3)
}
# record SSB targets as deferred (needs PxWeb v2 POST API)
for (i in seq_len(nrow(ssb))) readr::write_csv(tibble(example_id = ssb$example_id[i], system = "SSB", host = "ssb.no",
  study = ssb$sq[i], idno = NA, title = "", kind = "statbank saved query", n_variables = NA_integer_, ddi = "",
  status = "deferred (PxWeb v2)"), MAN, append = file.exists(MAN))

man <- suppressMessages(read_csv(MAN, show_col_types = FALSE))
message(sprintf("\n==== API harvest ====\nNADA: %d DDI variable inventories, %d study metadata | total variables: %d",
        n_ddi, n_meta, sum(man$n_variables, na.rm = TRUE)))
message(sprintf("examples with a microdata variable inventory: %d", n_distinct(man$example_id[man$status == "variables"])))
print(man %>% count(system, status))
