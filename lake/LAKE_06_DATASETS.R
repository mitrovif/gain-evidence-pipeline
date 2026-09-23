# ==============================================================================
# GAIN DATA LAKE - step 4: get the actual DATASETS behind the fetched pages.
#
# The step-3 fetch mostly grabbed doorway PAGES (101 html) not datasets. But those
# pages link to the real data: direct files (csv/xlsx/json), dataset portals/APIs
# (statbank/PxWeb/SDMX/microdata), and embedded HTML tables. This step:
#   (a) parses every stored page for dataset links (resolved to absolute URLs),
#   (b) saves embedded HTML tables as table_*.csv,
#   (c) downloads the direct data files (top few per example) as dataset_*.<ext>,
#   (d) records portal/API links for the API-harvest step.
# Output per example lands in data_lake/store/<id>/ ; index in lake_datasets.csv.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2) })
have_rvest <- requireNamespace("rvest", quietly = TRUE)
have_xml2  <- requireNamespace("xml2",  quietly = TRUE)
LAKE <- "data_lake"; STORE <- file.path(LAKE, "store")
man <- suppressMessages(read_csv(file.path(LAKE, "lake_store_manifest.csv"), show_col_types = FALSE)) %>%
  filter(kind == "html", nzchar(saved))
MAX_FILES_PER_EX <- 3; MAX_MB <- 60

# ---- URL resolver (relative href -> absolute against the page URL) ------------
resolve <- function(href, base) {
  href <- trimws(href); if (!nzchar(href) || grepl("^(mailto:|javascript:|#|tel:)", href)) return(NA_character_)
  if (grepl("^https?://", href)) return(href)
  m <- str_match(base, "^(https?://[^/]+)(/.*)?$"); root <- m[2]; path <- coalesce(m[3], "/")
  if (grepl("^//", href)) return(paste0(sub("^(https?):.*", "\\1", base), ":", href))
  if (grepl("^/", href)) return(paste0(root, href))
  dir <- sub("[^/]*$", "", path); paste0(root, dir, href)
}
DATRE  <- "[.](csv|xlsx?|json|zip|px|sdmx|xml)([?#]|$)"
PORTRE <- "statbank|pxweb|/px/|/sdmx|sdmx|/api/|microdata|/nada|/catalog|dataset|opendata|/webapi|json-stat|estat|eurostat"
kind_of <- function(u) { x <- tolower(u)
  if (str_detect(x, DATRE)) str_match(x, DATRE)[2] else if (str_detect(x, PORTRE)) "portal" else NA_character_ }

# ---- parse each page: dataset links + embedded tables ------------------------
links <- list(); tables_saved <- 0
for (i in seq_len(nrow(man))) {
  id <- man$example_id[i]; base <- man$url[i]; d <- file.path(STORE, id)
  f <- file.path(d, "source.html"); if (!file.exists(f)) next
  h <- tryCatch(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), error = function(e) "")
  hrefs <- unlist(str_extract_all(h, 'href=["\']([^"\']+)'))
  hrefs <- sub('^href=["\']', "", hrefs)
  abs <- unique(na.omit(map_chr(hrefs, resolve, base = base)))
  k <- map_chr(abs, kind_of); keep <- !is.na(k)
  if (any(keep)) links[[length(links)+1]] <- tibble(example_id = id, page = base, dataset_url = abs[keep], kind = k[keep])
  # embedded tables -> CSV
  if (have_rvest) {
    tt <- tryCatch(rvest::html_table(rvest::read_html(f), fill = TRUE), error = function(e) list())
    tt <- Filter(function(x) nrow(x) >= 2 && ncol(x) >= 2, tt)
    for (j in seq_along(tt)) { readr::write_excel_csv(tt[[j]], file.path(d, sprintf("table_%02d.csv", j))); tables_saved <- tables_saved + 1 }
  }
}
linkdf <- bind_rows(links) %>% distinct(example_id, dataset_url, .keep_all = TRUE) %>%
  mutate(rank = case_when(kind %in% c("csv","xlsx","xls","json") ~ 1, kind %in% c("zip","px","sdmx","xml") ~ 2, TRUE ~ 3)) %>%
  arrange(example_id, rank)
readr::write_excel_csv(linkdf, file.path(LAKE, "lake_dataset_links.csv"))
message(sprintf("dataset links found: %d (%d direct files, %d portal/API) across %d examples | tables saved: %d",
        nrow(linkdf), sum(linkdf$rank == 1), sum(linkdf$kind == "portal"), n_distinct(linkdf$example_id), tables_saved))

# ---- download the direct data files (top few per example) --------------------
files <- linkdf %>% filter(rank <= 2) %>% group_by(example_id) %>% slice_head(n = MAX_FILES_PER_EX) %>% ungroup()
DM <- file.path(LAKE, "lake_dataset_manifest.csv")
done <- if (file.exists(DM)) suppressMessages(read_csv(DM, show_col_types = FALSE))$dataset_url else character(0)
files <- files %>% filter(!dataset_url %in% done)
message(sprintf("downloading %d direct data files ...", nrow(files)))
seqn <- ave(seq_len(nrow(files)), files$example_id, FUN = seq_along)
for (i in seq_len(nrow(files))) {
  id <- files$example_id[i]; u <- files$dataset_url[i]; d <- file.path(STORE, id)
  row <- tryCatch({
    rp <- request(u) |> req_timeout(60) |>
      req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/124 Safari/537.36",
                  Accept = "*/*", `Accept-Language` = "en") |>
      req_options(followlocation = TRUE, ssl_verifypeer = 0) |> req_error(is_error = \(x) FALSE) |> req_perform()
    st <- resp_status(rp); body <- resp_body_raw(rp); mb <- length(body)/1048576
    if (st < 400 && mb <= MAX_MB && length(body) > 0) {
      p <- file.path(d, sprintf("dataset_%02d.%s", seqn[i], files$kind[i])); writeBin(body, p)
      tibble(example_id=id, dataset_url=u, kind=files$kind[i], http_status=st, bytes=length(body), saved=p, note="")
    } else tibble(example_id=id, dataset_url=u, kind=files$kind[i], http_status=st, bytes=length(body), saved="",
                  note=if (mb>MAX_MB) sprintf("oversize %.0fMB", mb) else "http/empty")
  }, error = function(e) tibble(example_id=id, dataset_url=u, kind=files$kind[i], http_status=NA_integer_, bytes=0, saved="", note=paste("ERR:", substr(conditionMessage(e),1,50))))
  readr::write_csv(row, DM, append = file.exists(DM)); Sys.sleep(0.2)
}

# ---- summary: which examples now have a real dataset -------------------------
dm <- if (file.exists(DM)) suppressMessages(read_csv(DM, show_col_types = FALSE)) else tibble()
tbl_ex <- unique(sub("/table.*", "", list.files(STORE, pattern = "^table_.*csv$", recursive = TRUE)))
have_file  <- if (nrow(dm)) unique(dm$example_id[nzchar(dm$saved)]) else character(0)
have_portal<- unique(linkdf$example_id[linkdf$kind == "portal"])
have_any <- union(union(have_file, tbl_ex), have_portal)
message(sprintf("\n==== datasets ===="))
message(sprintf("examples with a downloaded data FILE: %d | with an extracted TABLE: %d | with a portal/API link: %d",
        length(have_file), length(tbl_ex), length(have_portal)))
message(sprintf("examples with a real dataset reachable (file/table/portal): %d of %d fetched pages", length(have_any), nrow(man)))
if (nrow(dm)) print(dm %>% filter(nzchar(saved)) %>% count(kind, sort = TRUE))
