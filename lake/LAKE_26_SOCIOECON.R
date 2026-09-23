# ==============================================================================
# GAIN DATA LAKE - socio-economic MICRODATA acquisition list.
#
# User focus: download socio-economic microdata (living conditions, poverty,
# employment, health, education) - MICS, World Bank surveys, NSO household surveys
# - NOT stock/population counts. For each GAIN country we search IHSN (the one
# NADA search that works, and it federates World Bank + national catalogues) for
# the socio-economic survey families, classify, drop pure-stock, and list them
# with catalog links so they can be downloaded (public-use direct, else request).
# ==============================================================================
suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
source("shared/GAIN_COMMON.R")
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))
countries <- ro %>% filter(nzchar(coalesce(country,""))) %>% count(country, sort=TRUE) %>%
  filter(!country %in% c("NA")) %>% pull(country)
message(sprintf("GAIN countries to sweep: %d", length(countries)))

GET <- function(u) tryCatch(request(u)|>req_timeout(30)|>req_headers(`User-Agent`="Mozilla/5.0 EGRISS",Accept="application/json,*/*")|>req_options(followlocation=TRUE,ssl_verifypeer=0)|>req_error(is_error=\(x)FALSE)|>req_perform(), error=function(e) NULL)
sb <- function(rp) tryCatch(resp_body_string(rp), error=function(e) "")
BASE <- "https://catalog.ihsn.org"
srch <- function(q){ rp<-GET(sprintf("%s/index.php/api/catalog/search?sk=%s&ps=10",BASE,URLencode(q)))
  j<-tryCatch(fromJSON(sb(rp)),error=function(e)NULL); r<-j$result$rows; if(is.null(r)||!length(r)) return(tibble())
  d<-as_tibble(r); g<-function(nm) if(nm %in% names(d)) as.character(d[[nm]]) else rep("",nrow(d))
  tibble(id=g("id"), title=g("title"), nation=g("nation"), year=g("year_start"), repo=g("repositoryid")) %>% head(10) }

# socio-economic survey families (what produces the indicators the user wants)
SOCIO <- "socio.?econ|living cond|household (budget|income|expenditure|survey)|lsms|labour force|labor force|multiple indicator|\\bmics\\b|demographic and health|\\bdhs\\b|welfare|poverty|integrated household|standard of living|monitoring survey|high frequency|panel survey|profiling|forced displacement|refugee.*(survey|assessment)|vulnerability assessment"
STOCK <- "population.*census$|housing census|civil registration|vital statistic|register of|administrative records only"

results <- list()
for (i in seq_along(countries)) { c0 <- countries[i]; cw <- substr(tolower(str_split(c0," ")[[1]][1]),1,5)
  q <- c(paste(c0,"socioeconomic refugees"), paste(c0,"forced displacement survey"),
         paste(c0,"MICS"), paste(c0,"living conditions household survey"))
  cand <- map_dfr(q, srch) %>%
    filter(str_detect(tolower(nation), fixed(cw))) %>% distinct(id, .keep_all=TRUE) %>%
    filter(str_detect(tolower(title), SOCIO), !str_detect(tolower(title), STOCK))
  if (nrow(cand)) results[[length(results)+1]] <- cand %>% mutate(gain_country=c0)
  message(sprintf("  [%d/%d] %s -> %d socio-economic surveys", i, length(countries), substr(c0,1,16), nrow(cand)))
  Sys.sleep(0.15)
}
res <- bind_rows(results) %>%
  mutate(source = case_when(str_detect(tolower(repo),"unhcr")~"UNHCR", str_detect(tolower(repo),"wb|world")~"World Bank",
                            str_detect(tolower(title),"mics|multiple indicator")~"UNICEF MICS", TRUE~"national/other"),
         type = case_when(str_detect(tolower(title),"mics|multiple indicator")~"MICS",
           str_detect(tolower(title),"forced displacement|refugee|profiling|vulnerability|socio.?econ")~"FDP socio-economic",
           str_detect(tolower(title),"lsms|living cond|household (budget|income|expenditure)|welfare|poverty|integrated household")~"living conditions/LSMS",
           str_detect(tolower(title),"labour force|labor force")~"labour force",
           str_detect(tolower(title),"demographic and health|dhs")~"DHS", TRUE~"other"),
         catalog=sprintf("%s/index.php/catalog/%s",BASE,id)) %>%
  transmute(gain_country, source, type, survey=substr(title,1,54), year, catalog) %>%
  arrange(gain_country, type)
readr::write_excel_csv(res, file.path(LAKE,"lake_socioeconomic_microdata.csv"))

message(sprintf("\n==== socio-economic microdata catalogue ====\n%d surveys across %d GAIN countries", nrow(res), n_distinct(res$gain_country)))
message("\nby source:"); print(count(res, source, sort=TRUE))
message("\nby type:"); print(count(res, type, sort=TRUE))
