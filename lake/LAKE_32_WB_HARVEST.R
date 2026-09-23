# ==============================================================================
# GAIN DATA LAKE - harvest the FULL World Bank Microdata catalogue, match to GAIN
# countries + socio-economic survey families, and record each study's real IDNO
# (which the WB download API accepts). The free-text search is broken but pagination
# works, so we page through all ~7,100 studies. Output: wb_catalog.csv + wb_socio_gain.csv
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
gain_countries <- ro %>% filter(nzchar(coalesce(country,""))) %>% distinct(country) %>% pull(country)

GET <- function(u) tryCatch(request(u)|>req_timeout(40)|>req_headers(`User-Agent`="Mozilla/5.0 EGRISS")|>req_options(followlocation=TRUE,ssl_verifypeer=0)|>req_error(is_error=\(x)FALSE)|>req_perform(), error=function(e) NULL)
page1 <- function(pg, ps=200){ rp<-GET(sprintf("https://microdata.worldbank.org/index.php/api/catalog/search?ps=%d&page=%d&sort_by=year_start&sort_order=desc",ps,pg))
  j<-tryCatch(fromJSON(resp_body_string(rp)),error=function(e)NULL); r<-j$result$rows
  if(is.null(r)||!length(r)) return(tibble()); as_tibble(r) %>% mutate(across(everything(), as.character)) }

message("harvesting WB catalogue ...")
all <- list(); pg <- 1
repeat { d <- page1(pg); if(!nrow(d)) break
  all[[pg]] <- d %>% select(any_of(c("idno","title","nation","year_start","year_end","repositoryid","data_class_id","varcount","total_downloads")))
  message(sprintf("  page %d -> %d (cum %d)", pg, nrow(d), sum(map_int(all,nrow)))); pg <- pg+1; if(pg>60) break; Sys.sleep(0.1) }
cat_wb <- bind_rows(all) %>% distinct(idno, .keep_all=TRUE)
readr::write_excel_csv(cat_wb, file.path(LAKE,"wb_catalog.csv"))
message(sprintf("WB catalogue harvested: %d studies", nrow(cat_wb)))

# match to GAIN countries + socio-economic families
SOCIO <- "socio.?econ|living cond|household (budget|income|expenditure|survey)|lsms|labour force|labor force|welfare|poverty|integrated household|monitoring|high frequency|panel|profiling|forced displacement|refugee|vulnerability|multiple indicator|\\bmics\\b|demographic and health|\\bdhs\\b|standard of living|socioeconomic"
STOCK <- "population and housing census|housing census|census of population"
cmatch <- function(nation){ n<-tolower(coalesce(nation,"")); any(map_lgl(gain_countries, ~ str_detect(n, fixed(tolower(substr(.x,1,6)))))) }
gain_for <- function(nation){ n<-tolower(coalesce(nation,"")); hit<-gain_countries[map_lgl(gain_countries, ~ str_detect(n, fixed(tolower(substr(.x,1,6)))))]; if(length(hit)) hit[1] else NA_character_ }

socio <- cat_wb %>% filter(str_detect(tolower(title), SOCIO), !str_detect(tolower(title), STOCK)) %>%
  mutate(in_gain = map_lgl(nation, cmatch)) %>% filter(in_gain) %>%
  mutate(gain_country = map_chr(nation, gain_for),
         iso3 = str_extract(idno,"^[A-Z]{3}"),
         download_api = sprintf("https://microdata.worldbank.org/index.php/api/downloads/%s/files?type=data", idno)) %>%
  select(gain_country, nation, idno, title, year_start, data_class_id, varcount, download_api) %>%
  arrange(gain_country, desc(year_start))
readr::write_excel_csv(socio, file.path(LAKE,"wb_socio_gain.csv"))

message(sprintf("\nsocio-economic WB studies in GAIN countries: %d across %d countries", nrow(socio), n_distinct(socio$gain_country)))
message("by NADA access class (data_class_id):"); print(count(socio, data_class_id, sort=TRUE) %>% as.data.frame(), right=FALSE)
message("\nsample:"); print(socio %>% transmute(gain_country, idno, title=substr(title,1,40), year_start, data_class_id) %>% head(10) %>% as.data.frame(), right=FALSE)
