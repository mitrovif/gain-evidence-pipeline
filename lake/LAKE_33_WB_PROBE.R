# ==============================================================================
# GAIN DATA LAKE - probe WB download API for every matched study: real access type,
# file count, total size, and direct download links. NO downloading here - this just
# tells us what is open (pullable now) vs licensed. Output: wb_download_manifest.csv
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
LAKE <- "data_lake"
g <- suppressMessages(read_csv(file.path(LAKE,"wb_socio_gain.csv"), show_col_types=FALSE))
GET <- function(u) tryCatch(request(u)|>req_timeout(30)|>req_headers(`User-Agent`="Mozilla/5.0 EGRISS")|>req_options(followlocation=TRUE,ssl_verifypeer=0)|>req_error(is_error=\(x)FALSE)|>req_perform(), error=function(e) NULL)

probe1 <- function(idno){
  rp <- GET(sprintf("https://microdata.worldbank.org/index.php/api/downloads/%s/files?type=data", idno))
  txt <- tryCatch(resp_body_string(rp), error=function(e) "")
  j  <- tryCatch(fromJSON(txt), error=function(e) NULL)
  if (is.null(j) || is.null(j$status) || !isTRUE(j$status=="success") || is.null(j$files) || !length(j$files))
    return(tibble(idno=idno, access="none/gated", n_files=0L, mb=0, dl=""))
  f <- as_tibble(j$files)
  acc <- if("data_access_type" %in% names(f)) paste(unique(na.omit(as.character(f$data_access_type))),collapse="/") else ""
  mb  <- if("file_size_bytes" %in% names(f)) round(sum(suppressWarnings(as.numeric(f$file_size_bytes)),na.rm=TRUE)/1e6,1) else NA_real_
  # links can be a data.frame column or a list-column depending on the response
  dl <- ""
  if ("links" %in% names(f)) { lk <- f[["links"]]
    dl <- tryCatch({
      if (is.data.frame(lk) && "download" %in% names(lk)) paste(na.omit(as.character(lk$download)), collapse=" | ")
      else if (is.list(lk)) paste(na.omit(map_chr(lk, function(x) tryCatch(as.character(x[["download"]])[1], error=function(e) NA_character_))), collapse=" | ")
      else "" }, error=function(e) "") }
  tibble(idno=idno, access=ifelse(nzchar(acc),acc,"open?"), n_files=nrow(f), mb=mb, dl=dl)
}
probe <- function(idno) tryCatch(probe1(idno), error=function(e) tibble(idno=idno, access="error", n_files=0L, mb=0, dl=""))
rows <- vector("list", nrow(g))
for (i in seq_len(nrow(g))) { rows[[i]] <- probe(g$idno[i])
  if (i %% 100 == 0) { message(sprintf("  probed %d/%d", i, nrow(g)))
    readr::write_excel_csv(bind_rows(rows[seq_len(i)]), file.path(LAKE,"wb_probe_partial.csv")) }
  Sys.sleep(0.03) }
man <- bind_rows(rows)
out <- g %>% select(gain_country, idno, title, year_start) %>% left_join(man, by="idno") %>%
  mutate(downloadable = access %in% c("open","direct","public","open?") | (n_files>0 & nzchar(dl))) %>%
  arrange(desc(downloadable), gain_country, desc(year_start))
readr::write_excel_csv(out, file.path(LAKE,"wb_download_manifest.csv"))

message(sprintf("\n==== WB download manifest: %d studies ====", nrow(out)))
message("by access:"); print(count(out, access, sort=TRUE) %>% as.data.frame(), right=FALSE)
message(sprintf("\nDOWNLOADABLE NOW: %d studies | total ~%.1f GB | across %d countries",
        sum(out$downloadable), sum(out$mb[out$downloadable],na.rm=TRUE)/1000, n_distinct(out$gain_country[out$downloadable])))
