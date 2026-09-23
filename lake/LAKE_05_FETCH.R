# ==============================================================================
# GAIN DATA LAKE - step 3: FETCH the obtainable-now sources into the lake store.
#
# Downloads every example whose data we can get without a request: live SCRAPE
# pages/PDFs, the 2 API/file sources, and the 36 links recovered by cross-match.
# Saves raw bytes to data_lake/store/<example_id>/ plus an extracted .txt layer
# (HTML visible text / PDF text) for the frame-enrichment step. Resumable via a
# manifest; browser headers; per-file size cap; polite pacing.
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2) })
have_pdf <- requireNamespace("pdftools", quietly = TRUE)
LAKE <- "data_lake"; STORE <- file.path(LAKE, "store"); dir.create(STORE, recursive = TRUE, showWarnings = FALSE)
MANIFEST <- file.path(LAKE, "lake_store_manifest.csv")
MAX_MB <- 60

plan <- suppressMessages(read_csv(file.path(LAKE, "lake_download_plan.csv"), show_col_types = FALSE))
rec  <- suppressMessages(read_csv(file.path(LAKE, "lake_recovery.csv"), show_col_types = FALSE))

live  <- plan %>% filter(action %in% c("AUTO", "AUTO_META", "SCRAPE")) %>%
  transmute(example_id, country, organisation, source_type, fetch_url = url, how = action)
recd  <- rec %>% filter(recovery != "none", !is.na(recovered_url)) %>%
  transmute(example_id, country, organisation, fetch_url = recovered_url, how = "RECOVERED", source_type = "web_page")
tgt <- bind_rows(live, recd) %>% filter(!is.na(fetch_url), nzchar(fetch_url)) %>%
  distinct(example_id, .keep_all = TRUE)

done <- if (file.exists(MANIFEST)) suppressMessages(read_csv(MANIFEST, show_col_types = FALSE))$example_id else character(0)
todo <- tgt %>% filter(!example_id %in% done)
message(sprintf("fetch targets: %d (%d already in store, %d to fetch)", nrow(tgt), length(done), nrow(todo)))

ext_of <- function(url, ctype) {
  u <- tolower(url); c <- tolower(coalesce(ctype, ""))
  if (grepl("\\.pdf($|\\?)", u) || grepl("pdf", c)) return("pdf")
  if (grepl("\\.xlsx($|\\?)", u) || grepl("spreadsheet", c)) return("xlsx")
  if (grepl("\\.xls($|\\?)", u)) return("xls")
  if (grepl("\\.csv($|\\?)", u) || grepl("text/csv", c)) return("csv")
  if (grepl("\\.zip($|\\?)", u) || grepl("zip", c)) return("zip")
  if (grepl("json", c) || grepl("\\.json($|\\?)", u)) return("json")
  if (grepl("xml|sdmx", c) || grepl("\\.xml($|\\?)", u)) return("xml")
  "html"
}
html_text <- function(raw) {
  s <- rawToChar(raw[seq_len(min(length(raw), 3e6))]); Encoding(s) <- "UTF-8"
  s <- gsub("(?is)<script.*?</script>|<style.*?</style>", " ", s, perl = TRUE)
  s <- gsub("<[^>]+>", " ", s); s <- gsub("&nbsp;", " ", s, fixed = TRUE); s <- gsub("&amp;", "&", s, fixed = TRUE)
  str_squish(s)
}
fetch_one <- function(id, url) {
  d <- file.path(STORE, id); dir.create(d, showWarnings = FALSE)
  out <- tryCatch({
    rp <- request(url) |> req_timeout(45) |>
      req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124 Safari/537.36",
                  Accept = "text/html,application/xhtml+xml,application/pdf,*/*;q=0.8", `Accept-Language` = "en,fr;q=0.8") |>
      req_options(followlocation = TRUE, ssl_verifypeer = 0) |> req_error(is_error = \(x) FALSE) |> req_perform()
    st <- resp_status(rp); ct <- resp_header(rp, "content-type") %||% ""; body <- resp_body_raw(rp)
    mb <- length(body) / 1048576
    if (st >= 400) return(tibble(example_id=id, url=url, http_status=st, content_type=substr(ct,1,40), bytes=length(body), saved="", kind="", note="http error"))
    if (mb > MAX_MB) return(tibble(example_id=id, url=url, http_status=st, content_type=substr(ct,1,40), bytes=length(body), saved="", kind="oversize", note=sprintf("skipped %.0fMB > %dMB", mb, MAX_MB)))
    ext <- ext_of(url, ct); path <- file.path(d, paste0("source.", ext)); writeBin(body, path)
    txt <- ""
    if (ext == "html") { txt <- html_text(body) }
    else if (ext == "pdf" && have_pdf) { txt <- tryCatch(str_squish(paste(pdftools::pdf_text(path), collapse=" ")), error=function(e) "") }
    if (nzchar(txt)) writeLines(substr(txt, 1, 200000), file.path(d, "text.txt"))
    tibble(example_id=id, url=url, http_status=st, content_type=substr(ct,1,40), bytes=length(body),
           saved=path, kind=ext, note=if (nzchar(txt)) sprintf("text %d chars", nchar(txt)) else "")
  }, error = function(e) tibble(example_id=id, url=url, http_status=NA_integer_, content_type="", bytes=0, saved="", kind="", note=paste("ERR:", substr(conditionMessage(e),1,60))))
  out
}

n <- nrow(todo)
for (i in seq_len(n)) {
  row <- fetch_one(todo$example_id[i], todo$fetch_url[i])
  readr::write_csv(row, MANIFEST, append = file.exists(MANIFEST))
  message(sprintf("  [%d/%d] %s %s %s | %s", i, n, todo$example_id[i],
                  ifelse(is.na(row$http_status),"ERR",row$http_status), row$kind, substr(todo$fetch_url[i],1,64)))
  Sys.sleep(0.25)
}

m <- suppressMessages(read_csv(MANIFEST, show_col_types = FALSE))
ok <- m %>% filter(nzchar(saved))
message(sprintf("\n==== fetch done: %d saved of %d attempted ====", nrow(ok), nrow(m)))
print(ok %>% count(kind, sort = TRUE))
message(sprintf("with extracted text: %d | total stored: %.1f MB", sum(str_detect(m$note,"text \\d")), sum(m$bytes)/1048576))
