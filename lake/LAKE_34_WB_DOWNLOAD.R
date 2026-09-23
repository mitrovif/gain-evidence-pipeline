# ==============================================================================
# GAIN DATA LAKE - download every downloadable WB study into a systematic store.
#   data_lake/downloads/worldbank/<ISO3>/<IDNO>/<files>  (+ files.json manifest)
# Resume-safe: skips files already on disk. Logs every file. Re-fetches fresh
# download links per study (they are stable but we take them live). Output:
# wb_download_log.csv
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
LAKE <- "data_lake"; ROOT <- file.path(LAKE,"downloads","worldbank")
dir.create(ROOT, showWarnings=FALSE, recursive=TRUE)
man <- suppressMessages(read_csv(file.path(LAKE,"wb_download_manifest.csv"), show_col_types=FALSE)) %>% filter(downloadable)
message(sprintf("studies to download: %d (~%.1f GB)", nrow(man), sum(man$mb,na.rm=TRUE)/1000))

GET <- function(u) tryCatch(request(u)|>req_timeout(40)|>req_headers(`User-Agent`="Mozilla/5.0 EGRISS")|>req_options(followlocation=TRUE,ssl_verifypeer=0)|>req_error(is_error=\(x)FALSE)|>req_perform(), error=function(e) NULL)
files_of <- function(idno){ rp<-GET(sprintf("https://microdata.worldbank.org/index.php/api/downloads/%s/files?type=data", idno))
  j<-tryCatch(fromJSON(resp_body_string(rp)),error=function(e)NULL); if(is.null(j$files)||!length(j$files)) return(tibble())
  f<-as_tibble(j$files); n<-nrow(f); lk<-f[["links"]]
  dl <- if(is.data.frame(lk) && "download" %in% names(lk)) as.character(lk$download)
        else if(is.list(lk)) map_chr(lk, function(x) tryCatch(as.character(x[["download"]])[1], error=function(e) NA_character_))
        else rep(NA_character_, n)
  dl <- as.character(dl); length(dl) <- n
  fn <- as.character(if("filename" %in% names(f)) f$filename else rep(NA,n)); length(fn) <- n
  bytes <- suppressWarnings(as.numeric(if("file_size_bytes" %in% names(f)) f$file_size_bytes else rep(NA,n))); length(bytes) <- n
  tibble(filename=fn, url=dl, bytes=bytes) %>% filter(!is.na(url), nzchar(url)) }
dl_file <- function(url, dest){ rp<-tryCatch(request(url)|>req_timeout(600)|>req_headers(`User-Agent`="Mozilla/5.0 EGRISS")|>req_options(followlocation=TRUE,ssl_verifypeer=0)|>req_error(is_error=\(x)FALSE)|>req_perform(path=dest), error=function(e) NULL)
  if(is.null(rp)||!file.exists(dest)||file.info(dest)$size<200) return(FALSE); TRUE }

log <- list()
for (i in seq_len(nrow(man))) {
  idno<-man$idno[i]; iso3<-str_extract(idno,"^[A-Z]{3}"); if(is.na(iso3)) iso3<-"XXX"
  d<-file.path(ROOT, iso3, idno); dir.create(d, showWarnings=FALSE, recursive=TRUE)
  ff<-files_of(idno); if(!nrow(ff)){ log[[length(log)+1]]<-tibble(idno=idno,filename=NA,status="no-files",bytes=0); next }
  writeLines(jsonlite::toJSON(ff, auto_unbox=TRUE), file.path(d,"files.json"))
  for (k in seq_len(nrow(ff))) { dest<-file.path(d, ff$filename[k])
    if (file.exists(dest) && file.info(dest)$size>200) { log[[length(log)+1]]<-tibble(idno=idno,filename=ff$filename[k],status="exists",bytes=file.info(dest)$size); next }
    ok<-dl_file(ff$url[k], dest)
    log[[length(log)+1]]<-tibble(idno=idno,filename=ff$filename[k],status=ifelse(ok,"downloaded","FAILED"),bytes=ifelse(ok&&file.exists(dest),file.info(dest)$size,0)) }
  if (i %% 25 == 0) { message(sprintf("  %d/%d studies", i, nrow(man))); readr::write_excel_csv(bind_rows(log), file.path(LAKE,"wb_download_log.csv")) }
}
lg <- bind_rows(log); readr::write_excel_csv(lg, file.path(LAKE,"wb_download_log.csv"))
message(sprintf("\n==== WB download complete ====\nfiles: %d downloaded, %d already present, %d failed | %.1f GB on disk | %d studies",
        sum(lg$status=="downloaded"), sum(lg$status=="exists"), sum(lg$status=="FAILED"),
        sum(lg$bytes,na.rm=TRUE)/1e9, n_distinct(lg$idno)))
