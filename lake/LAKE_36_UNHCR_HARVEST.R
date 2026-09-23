# ==============================================================================
# GAIN DATA LAKE - harvest the UNHCR Microdata Library (microdata.unhcr.org, same
# NADA API as WB). All UNHCR studies are forced-displacement data by mandate. We
# page the full catalogue, match to GAIN countries, and record each study's IDNO +
# download-files API so the user (UNHCR staff) can pull them. Output: unhcr_gain.csv
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
gain_countries <- ro %>% filter(nzchar(coalesce(country,""))) %>% distinct(country) %>% pull(country)
GET <- function(u) tryCatch(request(u)|>req_timeout(40)|>req_headers(`User-Agent`="Mozilla/5.0 EGRISS")|>req_options(followlocation=TRUE,ssl_verifypeer=0)|>req_error(is_error=\(x)FALSE)|>req_perform(), error=function(e) NULL)
page1 <- function(pg, ps=200){ rp<-GET(sprintf("https://microdata.unhcr.org/index.php/api/catalog/search?ps=%d&page=%d",ps,pg))
  j<-tryCatch(fromJSON(resp_body_string(rp)),error=function(e)NULL); r<-j$result$rows
  if(is.null(r)||!length(r)) return(tibble()); as_tibble(r) %>% mutate(across(everything(), as.character)) }

message("harvesting UNHCR catalogue ...")
all <- list(); pg <- 1
repeat { d <- page1(pg); if(!nrow(d)) break
  all[[pg]] <- d %>% select(any_of(c("idno","title","nation","year_start","year_end","repositoryid","data_class_id","total_downloads")))
  message(sprintf("  page %d (cum %d)", pg, sum(map_int(all,nrow)))); pg <- pg+1; if(pg>12) break; Sys.sleep(0.1) }
cat_u <- bind_rows(all) %>% distinct(idno, .keep_all=TRUE)
readr::write_excel_csv(cat_u, file.path(LAKE,"unhcr_catalog.csv"))

gain_for <- function(nation){ n<-tolower(coalesce(nation,"")); hit<-gain_countries[map_lgl(gain_countries, ~ str_detect(n, fixed(tolower(substr(.x,1,6)))))]; if(length(hit)) hit[1] else NA_character_ }
g <- cat_u %>% mutate(gain_country = map_chr(nation, gain_for)) %>% filter(!is.na(gain_country)) %>%
  mutate(download_api = sprintf("https://microdata.unhcr.org/index.php/api/downloads/%s/files?type=data", idno),
         page = sprintf("https://microdata.unhcr.org/index.php/catalog/%s", idno)) %>%
  select(gain_country, idno, title, year_start, download_api, page) %>% arrange(gain_country, desc(year_start))
readr::write_excel_csv(g, file.path(LAKE,"unhcr_gain.csv"))

message(sprintf("\n==== UNHCR Microdata: %d studies total | %d in GAIN countries (%d countries) ====",
        nrow(cat_u), nrow(g), n_distinct(g$gain_country)))
message("top GAIN countries:"); print(count(g, gain_country, sort=TRUE) %>% head(15) %>% as.data.frame(), right=FALSE)
