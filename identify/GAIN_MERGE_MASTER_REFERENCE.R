# ==============================================================================
# GAIN REFERENCE FILE - MERGE LAYERS INTO MASTER
#
# Combines:
#   Layer 1 (catalog records)   - already structured, highest trust
#   Layer 2 (search engine hits)- title + snippet + URL, high trust
#   Layer 3 (URL inventory)     - URL only, needs title fetch (Layer 4)
#
# Layer 4 (this script, part B): fetches <title> for Layer 3 URLs that
# are not already covered by Layers 1-2, to complete the record.
#
# Output: GAIN_MASTER_REFERENCE_[date].csv
#   country | title | url | populations | year | source_layer | trust
# ==============================================================================

library(tidyverse)
library(httr2)
library(rvest)

stamp <- format(Sys.Date(), "%Y%m%d")

# --- Composition targets ------------------------------------------------------
# GAIN is mostly about country-led examples: at least 60% of the master file
# must come from NSO websites / NSO-led efforts. Catalog (microdata library)
# records are capped at 40% and restricted to 2024-2026.
TARGET_NSO_SHARE <- 0.60
MIN_YEAR <- 2024
MAX_YEAR <- 2026

# Adjust to your actual filenames
# Layers 1 and 3: ALL dated files are combined (newest first, so a record's most
# recent metadata wins on duplicate URLs). A re-harvest can return FEWER records
# (e.g. NSO sitemaps that are down that day) - reading only the newest file would
# silently drop what earlier runs found.
read_all_newest_first <- function(files) {
  if (!length(files)) return(NULL)
  map_dfr(rev(sort(files)), ~ read_csv(.x, show_col_types = FALSE,
                                       col_types = cols(.default = col_character()))) %>%
    distinct(url, .keep_all = TRUE) %>% type_convert(col_types = cols())
}
L1_ALL <- list.files(pattern = "^LAYER1_catalog_records_.*\\.csv$")
L1 <- if (length(L1_ALL)) sort(L1_ALL) %>% last() else character(0)
# Layer 2 = ALL search-hit files (the original harvest + any "since" refresh
# runs), de-duplicated by URL below; older files win so first-seen is kept.
L2_ALL <- list.files(pattern = "^LAYER2_search_hits_.*\\.csv$") %>% sort()
L2 <- if (length(L2_ALL)) L2_ALL[1] else character(0)
L3_ALL <- list.files(pattern = "^LAYER3_url_inventory_.*\\.csv$")
L3 <- if (length(L3_ALL)) sort(L3_ALL) %>% last() else character(0)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ------------------------------------------------------------------------------
# MOJIBAKE REPAIR: fixes UTF-8 text that was decoded as Windows-1252 somewhere
# upstream ("TÃ¼rkiye" -> "Türkiye", "Ð¿Ñ–Ð´Ð¿Ñ€..." -> "підпр..."). Applied to
# all text columns read from the layer files; harmless on clean text.
# ------------------------------------------------------------------------------
repair_mojibake <- function(x) {
  fix_one <- function(s) {
    if (is.na(s)) return(s)
    cur <- s
    for (i in 1:2) {   # handles single and double mis-encoding
      # mojibake signature: marker letter immediately followed by another
      # non-ASCII char (real words are safe: "Sao"/"ANO" have ASCII after)
      if (!str_detect(cur, "[ÃÐÑÒÂ][^\\x01-\\x7F]")) break
      b <- tryCatch(iconv(cur, from = "UTF-8", to = "windows-1252", toRaw = TRUE)[[1]],
                    error = function(e) NULL)
      if (is.null(b) || length(b) == 0 || any(is.na(b))) break
      cand <- rawToChar(b)
      Encoding(cand) <- "UTF-8"
      if (!validUTF8(cand)) break
      cur <- cand
    }
    cur
  }
  vapply(as.character(x), fix_one, character(1), USE.NAMES = FALSE)
}

norm_pop <- function(txt) {
  t <- str_to_lower(coalesce(txt, ""))
  pops <- c()
  if (str_detect(t, paste0(
    "refugee|asylum|refugi|\\basil[eo]\\b|لاجئ|اللجوء|беженц|біженц|mülteci|sığınmacı|wakimbizi|",
    "flücht|vluchteling|rifugiat|uchodź|menekült|uprchlí|utečen|πρόσφυγ|refugiaț|",
    "бежанц|izbeglic|izbjeglic|refugjat|бегалц|begunc|پناهند|פליטים|flykting|flyktning|",
    "flygtning|pakolai|pagulas|bēgļ|pabėgėl|难民|難民|난민|ผู้ลี้ภัย|tị nạn|pengungsi|pelarian")))
    pops <- c(pops, "refugees")
  if (str_detect(t, paste0(
    "displac|\\bidps?\\b|deplac|desplaz|deslocad|نازح|النزوح|перемещ|переміщ|yerinden|",
    "vertrieben|ontheemd|sfollat|przesiedl|wysiedl|vysídlen|εκτοπισμέν|strămutat|",
    "разселени|raseljen|zhvendosur|раселени|razseljen|آوارگ|עקורים|fördrivna|fordrevne|",
    "流离失所|避難民|실향민|ผู้พลัดถิ่น|di tản|",
    "forced migration|migration forcée|migración forzada|вынужденная миграция|",
    "вимушена міграція|returnees?\\b|retourné|retornad|возвращенц")))
    pops <- c(pops, "idps")
  if (str_detect(t, paste0(
    "stateless|apatrid|عديمي|الجنسية|апатрид|без гражданства|без громадянства|vatansız|",
    "staatenlos|staatloos|apolid|bezpaństwow|hontalan|ανιθαγεν|apatriz|без гражданство|",
    "državljanstva|shtetësi|državjanstvo|无国籍|無国籍|무국적|ไร้สัญชาติ")))
    pops <- c(pops, "stateless")
  paste(pops, collapse = "; ")
}

records <- list()

# ---- Layer 1 ----
if (length(L1_ALL) > 0) {
  message(paste("Layer 1 files:", paste(sort(L1_ALL), collapse = ", ")))
  l1 <- read_all_newest_first(L1_ALL) %>%
    mutate(across(any_of(c("title", "producer", "country")), repair_mojibake))
  # microdata/catalog records: 2024-2026 only; DHS/MICS get 2022+ (long cycles)
  n_l1_raw <- nrow(l1)
  l1 <- l1 %>%
    mutate(
      best_year = suppressWarnings(
        pmax(as.numeric(year_start), as.numeric(year_end), na.rm = TRUE)),
      year_floor = if_else(
        str_detect(str_to_lower(paste(coalesce(title, ""), coalesce(source, ""))),
                   "\\bmics\\b|multiple indicator cluster|demographic and health|\\bdhs\\b|malaria indicator"),
        2022, MIN_YEAR)
    ) %>%
    filter(is.finite(best_year), best_year >= year_floor, best_year <= MAX_YEAR) %>%
    select(-best_year, -year_floor)
  # displacement-relevance: drop general DHS/MICS/census etc. (keep only records
  # whose title/keywords name a displacement population) - cleans existing data
  disp_pat <- paste0("refugee|asylum|asile|asilo|réfugi|refugiad|",
    "internally displaced|\\bidps?\\b|displac|déplac|desplaz|deslocad|",
    "stateless|apatrid|apátrid|forcibly displaced|forced displacement|",
    "returnee|retourn|retornad|egriss|نازح|لاجئ|اللجوء|عديمي الجنسية")
  kw_col <- if ("keywords_matched" %in% names(l1)) l1$keywords_matched else ""
  l1 <- l1 %>% filter(str_detect(
    str_to_lower(paste(coalesce(title, ""), coalesce(kw_col, ""))), disp_pat))
  message(paste0("Layer 1 recency + displacement filter: ", n_l1_raw, " -> ",
                 nrow(l1), " records (", MIN_YEAR, "+ general, 2022+ DHS/MICS, ",
                 "displacement-relevant only)"))
  records$l1 <- l1 %>% transmute(
    country, title, url,
    populations = map_chr(paste(title, keywords_matched), norm_pop),
    year = as.character(coalesce(year_start, year_end)),
    producer = producer,
    source_layer = paste0("L1:", source),
    trust = "HIGH (curated catalog)"
  )
  message(paste("Layer 1:", nrow(l1), "records"))
}

# ---- Layer 2 ----
if (length(L2_ALL) > 0) {
  l2 <- map_dfr(L2_ALL, ~ read_csv(.x, show_col_types = FALSE,
                                   col_types = cols(.default = col_character()))) %>%
    distinct(url, .keep_all = TRUE) %>%
    mutate(across(any_of(c("title", "snippet")), repair_mojibake))
  message(paste("Layer 2 files:", paste(L2_ALL, collapse = ", ")))
  records$l2 <- l2 %>% transmute(
    country, title, url,
    populations = map_chr(paste(title, snippet, population), norm_pop),
    year = str_extract(paste(title, snippet), "20\\d{2}"),
    producer = domain,
    source_layer = paste0("L2:", engine),
    trust = "HIGH (indexed page)"
  )
  message(paste("Layer 2:", nrow(l2), "records"))
}

# ---- Layer 1.5: Structured statistical APIs (Eurostat SDMX, etc.) ----
# Already in master schema (country/title/url/populations/year/producer/
# source_layer/trust); produced by GAIN_LAYER1_STRUCTURED.R. Clean, no-noise,
# producer-known official statistics for countries that expose an API.
ST <- list.files(pattern = "^structured_discovery_.*\\.csv$")
ST <- if (length(ST)) ST[which.max(file.info(ST)$mtime)] else NA_character_
if (!is.na(ST)) {
  st <- read_csv(ST, show_col_types = FALSE)
  needed <- c("country", "title", "url", "populations", "year",
              "producer", "source_layer", "trust")
  if (all(needed %in% names(st))) {
    records$struct <- st %>%
      mutate(across(any_of(c("title", "producer", "country")), repair_mojibake)) %>%
      transmute(country, title, url, populations,
                year = as.character(year), producer, source_layer, trust)
    message(paste("Layer 1.5 (structured APIs):", nrow(st), "records from", ST))
  } else {
    message("Layer 1.5: structured_discovery file found but schema mismatch - skipped")
  }
}

# ---- Layer 3 + 4: fetch titles for URLs not already covered ----
if (length(L3_ALL) > 0) {
  message(paste("Layer 3 files:", paste(sort(L3_ALL), collapse = ", ")))
  l3 <- read_all_newest_first(L3_ALL) %>%
    mutate(across(any_of(c("link_text")), repair_mojibake))
  if (!"link_text" %in% names(l3)) l3$link_text <- NA_character_
  covered_urls <- c(records$l1$url %||% character(0),
                    records$l2$url %||% character(0),
                    records$struct$url %||% character(0))
  todo <- l3 %>% filter(!url %in% covered_urls)

  # titles are cached per URL so re-running the merge never re-fetches a page
  dir.create("evidence_cache", showWarnings = FALSE)
  title_cache_file <- function(u)
    file.path("evidence_cache", paste0("title_", rlang::hash(u), ".rds"))

  # how many genuinely need a network fetch (no link_text AND not yet cached)
  needs_net <- todo %>%
    filter(is.na(link_text) | nchar(coalesce(link_text, "")) <= 12) %>%
    pull(url)
  n_uncached <- sum(!file.exists(map_chr(needs_net, title_cache_file)))
  message(paste0("Layer 3: ", nrow(l3), " URLs, ", nrow(todo), " candidates; ",
                 n_uncached, " titles to fetch (",
                 length(needs_net) - n_uncached, " already cached, ",
                 nrow(todo) - length(needs_net), " use their link text)"))

  # returns list(title, fetched): fetched=TRUE only on a real network call,
  # so the politeness sleep below never fires on a cache hit
  fetch_title <- function(u) {
    cache <- title_cache_file(u)
    if (file.exists(cache)) return(list(title = readRDS(cache), fetched = FALSE))
    if (str_detect(u, "\\.pdf$")) return(list(title = basename(u), fetched = FALSE))
    out <- tryCatch({
      resp <- request(u) %>%
        req_user_agent("EGRISS-GAIN-research") %>%
        req_timeout(20) %>% req_perform()
      page <- read_html(resp_body_string(resp))
      str_trim(html_text(html_element(page, "title")) %||% basename(u))
    }, error = function(e) NA_character_)
    if (!is.na(out)) saveRDS(out, cache)   # failures retried next run
    list(title = out, fetched = TRUE)
  }

  if (nrow(todo) > 0) {
    todo <- todo %>%
      mutate(title = map2_chr(url, link_text, function(u, lt) {
        # news-section links already carry their link text as a title
        if (!is.na(lt) && nchar(lt) > 12) return(lt)
        r <- fetch_title(u)
        if (r$fetched) Sys.sleep(0.8)   # pause ONLY on real network calls
        r$title
      }),
      title = repair_mojibake(title))
    records$l3 <- todo %>% transmute(
      country, title = coalesce(title, basename(url)), url,
      populations = map_chr(paste(title, matched_keyword), norm_pop),
      year = str_extract(paste(title, url), "20\\d{2}"),
      producer = domain,
      source_layer = paste0("L3:", source),
      trust = if_else(is.na(title), "LOW (unreachable)", "MEDIUM (URL inventory)")
    )
  }
}

# ---- Merge + dedup ----
master <- bind_rows(records) %>%
  filter(!is.na(url)) %>%
  # repair Common-Crawl-encoded query strings so links open in a browser
  # (& was stored as %26, = as %3D -> malformed query -> 404 / wrong page)
  mutate(url = url %>%
           str_replace_all("%26", "&") %>%
           str_replace_all("%3[Dd]", "=") %>%
           str_replace_all("&amp;", "&")) %>%
  mutate(norm_title = str_to_lower(str_remove_all(coalesce(title, ""), "[^a-z0-9 ]"))) %>%
  # same release found by multiple layers: keep highest-trust version
  arrange(country, norm_title, trust) %>%
  distinct(url, .keep_all = TRUE) %>%
  group_by(country, norm_title) %>%
  slice(1) %>%
  ungroup() %>%
  select(-norm_title) %>%
  filter(populations != "" |
         trust == "HIGH (curated catalog)" |
         str_detect(str_to_lower(coalesce(title, "")), "egriss")) %>%
  arrange(country, desc(year))

# ---- Enforce NSO-led composition (>= TARGET_NSO_SHARE from NSO sources) ----
# NSO-led = found on an NSO website (Layers 2/3) or catalog record whose
# producer is a statistical office. Everything else (microdata libraries,
# ReliefWeb, int. org products) is capped so it never exceeds 40%.
nso_producer_pattern <-
  "statisti|census bureau|bureau of stat|institut.*stat|nso\\b|dane\\b|inegi|pcbs|knbs|ubos|zimstat|instat"

master <- master %>%
  mutate(nso_led = (str_starts(source_layer, "L2:") |
                    str_starts(source_layer, "L3:") |
                    str_starts(source_layer, "STRUCT:") |   # structured API = national official stats
                    str_detect(str_to_lower(coalesce(producer, "")),
                               nso_producer_pattern)) &
                   # global partner catalogues (MICS/DHS sites) are not NSO-led
                   !str_detect(str_to_lower(coalesce(producer, "")),
                               "mics\\.unicef|dhsprogram") &
                   !str_starts(coalesce(country, ""), fixed("Global (")))

n_nso   <- sum(master$nso_led)
n_other <- sum(!master$nso_led)
max_other <- floor(n_nso * (1 - TARGET_NSO_SHARE) / TARGET_NSO_SHARE)

if (n_nso == 0) {
  warning("No NSO-led records found (have Layers 2/3 been run?). ",
          "Skipping the 60% composition cap so the file is not emptied.")
} else if (n_other > max_other) {
  message(paste0("Composition cap: trimming non-NSO records ", n_other,
                 " -> ", max_other, " (keeping highest trust, most recent)"))
  kept_other <- master %>%
    filter(!nso_led) %>%
    mutate(trust_rank = case_when(str_starts(trust, "HIGH")   ~ 1,
                                  str_starts(trust, "MEDIUM") ~ 2,
                                  TRUE                        ~ 3)) %>%
    arrange(trust_rank, desc(year)) %>%
    slice_head(n = max_other) %>%
    select(-trust_rank)
  master <- bind_rows(filter(master, nso_led), kept_other) %>%
    arrange(country, desc(year))
}

# UTF-8 with BOM so Excel renders all scripts correctly
write_excel_csv(master, paste0("GAIN_MASTER_REFERENCE_", stamp, ".csv"))

message("\n==================== MASTER REFERENCE ====================")
message(paste("Total records:", nrow(master)))
message(paste("Countries:", n_distinct(master$country)))
message(sprintf("NSO-led share: %.0f%% (%d of %d records) - target >= %.0f%%",
                100 * mean(master$nso_led), sum(master$nso_led), nrow(master),
                100 * TARGET_NSO_SHARE))
print(count(master, source_layer, trust))
message(paste("\nOutput: GAIN_MASTER_REFERENCE_", stamp, ".csv"))
message("This file feeds directly into GAIN_PHASE5_CROSSREF.R as EVIDENCE_FILE.")
