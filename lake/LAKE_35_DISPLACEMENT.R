# ==============================================================================
# GAIN DATA LAKE - narrow the WB socio-economic set to FORCED-DISPLACEMENT studies
# only (refugees / IDPs / returnees / asylum / Venezuelan & other migrants / host
# communities), which is what the GAIN/EGRISS workstreams actually need. Flags what
# is already downloaded vs still to get, plus access route. Output: wb_displacement.csv
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"; STORE <- file.path(LAKE,"downloads","worldbank")
g <- suppressMessages(read_csv(file.path(LAKE,"wb_socio_gain.csv"), show_col_types=FALSE))
m <- suppressMessages(read_csv(file.path(LAKE,"wb_download_manifest.csv"), show_col_types=FALSE)) %>% select(idno, access)

# displacement markers in title or idno
DISP <- paste0("refugee|\\bidp\\b|internally displaced|displac|returnee|\\breturn\\b|asylum|",
  "forced displ|\\bfdp\\b|venezuel|\\bmigrant|migration|rohingya|host communit|",
  "protection monitoring|post.?return|\\bprms\\b|\\bcbpm\\b|border monitor|profiling|",
  "resettlement|stateless|vulnerabilit|humanitarian|\\bhfps-?(idp|ref)")
# things that merely say 'migration' as internal migration - keep only if other displacement signal; light touch here
disp <- g %>% mutate(txt = tolower(paste(title, idno))) %>%
  filter(str_detect(txt, DISP)) %>%
  left_join(m, by="idno") %>%
  mutate(access = coalesce(access, "gated/no-api"))

# in-hand = has a valid zip folder
withzip <- list.files(STORE, pattern="\\.zip$", recursive=TRUE, full.names=TRUE)
inhand <- unique(basename(dirname(withzip))); inhand <- inhand[grepl("^[A-Z]{3}_", inhand)]

out <- disp %>% mutate(
  in_hand = idno %in% inhand,
  route = case_when(in_hand ~ "IN HAND",
                    access %in% c("direct","open") ~ "download now (no login)",
                    access=="public" ~ "public-use (your login)",
                    access %in% c("licensed","remote") ~ "licensed (apply)",
                    TRUE ~ "gated - try UNHCR portal / login"),
  dl_files_api = sprintf("https://microdata.worldbank.org/index.php/api/downloads/%s/files?type=data", idno)) %>%
  arrange(desc(in_hand), gain_country, desc(year_start)) %>%
  select(gain_country, idno, title, year_start, access, route, in_hand, dl_files_api)
readr::write_excel_csv(out, file.path(LAKE,"wb_displacement.csv"))

message(sprintf("==== WB forced-displacement studies in GAIN countries: %d (of %d socio-economic) ====", nrow(out), nrow(g)))
message(sprintf("across %d countries | already in hand: %d | to get: %d", n_distinct(out$gain_country), sum(out$in_hand), sum(!out$in_hand)))
message("\nby route (what to do):"); print(count(out, route, sort=TRUE) %>% as.data.frame(), right=FALSE)
message("\ntop countries by # displacement studies:"); print(count(out, gain_country, sort=TRUE) %>% head(12) %>% as.data.frame(), right=FALSE)
