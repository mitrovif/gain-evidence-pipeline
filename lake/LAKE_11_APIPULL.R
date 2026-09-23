# ==============================================================================
# GAIN DATA LAKE - step 9: PULL the verified API-connectable datasets into the lake.
#
# For each example confirmed in LAKE_10, fetch the actual data via the office API,
# filtered to that country (and recent years) so we store the country's own slice,
# not the whole EU cube. Eurostat is pulled as SDMX-CSV (tidy). Saved analysis-ready
# to data_lake/store/<id>/api_<code>.csv ; index lake_api_data_manifest.csv.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2) })
source("shared/GAIN_COMMON.R")
LAKE <- "data_lake"; STORE <- file.path(LAKE, "store")
conn <- suppressMessages(read_csv(file.path(LAKE, "lake_api_connectable.csv"), show_col_types = FALSE))

# country -> Eurostat geo code (EL for Greece, UK excluded)
ISO2 <- c(Austria="AT", Belgium="BE", Bulgaria="BG", Croatia="HR", Cyprus="CY", Czechia="CZ",
  Denmark="DK", Estonia="EE", Finland="FI", France="FR", Germany="DE", Greece="EL", Hungary="HU",
  Ireland="IE", Italy="IT", Latvia="LV", Lithuania="LT", Luxembourg="LU", Malta="MT", Netherlands="NL",
  Poland="PL", Portugal="PT", Romania="RO", Slovakia="SK", Slovenia="SI", Spain="ES", Sweden="SE")
geo_of <- function(country) unname(ISO2[harmonize_country(country)])

GET <- function(u) tryCatch(request(u) |> req_timeout(90) |> req_headers(`User-Agent`="Mozilla/5.0 EGRISS") |>
    req_options(followlocation=TRUE, ssl_verifypeer=0) |> req_error(is_error=\(x) FALSE) |> req_perform(), error=function(e) NULL)

MAN <- file.path(LAKE, "lake_api_data_manifest.csv"); if (file.exists(MAN)) file.remove(MAN)
pull_eurostat <- function(code, geo) {
  # SDMX-CSV, country slice, recent years
  base <- sprintf("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/%s/?format=SDMX-CSV&startPeriod=2016", code)
  u <- if (!is.na(geo)) paste0(base, "&geo=", geo) else base
  rp <- GET(u); if (is.null(rp) || resp_status(rp) >= 400) return(list(ok=FALSE, note=sprintf("http %s", if(is.null(rp)) "ERR" else resp_status(rp)), url=u))
  txt <- tryCatch(resp_body_string(rp), error=function(e) "")
  if (!grepl("OBS_VALUE|obs_value", txt) || nchar(txt) < 50) return(list(ok=FALSE, note="not CSV / empty", url=u))
  list(ok=TRUE, txt=txt, url=u)
}

for (i in seq_len(nrow(conn))) {
  r <- conn[i,]; d <- file.path(STORE, r$example_id); dir.create(d, showWarnings=FALSE)
  geo <- geo_of(r$country)
  res <- if (r$system == "Eurostat") pull_eurostat(r$dataset_code, geo) else list(ok=FALSE, note="system not implemented", url=r$api_url)
  saved <- ""; nrows <- 0L
  if (isTRUE(res$ok)) {
    p <- file.path(d, sprintf("api_%s.csv", r$dataset_code)); writeLines(res$txt, p); saved <- p
    nrows <- tryCatch(nrow(suppressMessages(readr::read_csv(p, show_col_types=FALSE))), error=function(e) NA_integer_)
  }
  readr::write_csv(tibble(example_id=r$example_id, country=r$country, system=r$system, dataset_code=r$dataset_code,
    geo=coalesce(geo,""), verdict=r$verdict, rows=nrows, saved=basename(saved),
    note=if(isTRUE(res$ok)) "" else res$note, url=res$url), MAN, append=file.exists(MAN))
  message(sprintf("  [%d/%d] %s %s %s -> %s", i, nrow(conn), r$example_id, substr(r$country,1,12), r$dataset_code,
                  if(isTRUE(res$ok)) sprintf("%s rows", nrows) else res$note))
  Sys.sleep(0.3)
}

m <- suppressMessages(read_csv(MAN, show_col_types=FALSE))
ok <- m %>% filter(nzchar(saved))
message(sprintf("\n==== API data pull ====\nsaved %d of %d datasets (%d total rows) into the lake",
        nrow(ok), nrow(m), sum(ok$rows, na.rm=TRUE)))
if (nrow(ok)) print(ok %>% transmute(example_id, country=substr(country,1,14), dataset_code, rows) %>% as.data.frame(), right=FALSE)
if (any(!nzchar(m$saved))) { message("\nnot pulled:"); print(m %>% filter(!nzchar(saved)) %>% select(example_id, dataset_code, note)) }
