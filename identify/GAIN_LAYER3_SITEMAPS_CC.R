# ==============================================================================
# GAIN REFERENCE FILE - LAYER 3: FULL URL INVENTORIES
#
# Gets the COMPLETE list of URLs on each NSO domain, two free ways:
#   A. sitemap.xml      - the site's own declared page list
#   B. Common Crawl     - every URL ever captured by the CC web archive,
#                         queried via index API (never touches the NSO server)
#
# Then filters URL paths/filenames by multilingual keywords.
# This catches what link-crawling misses (e.g. the Kenya KIHBS page).
#
# Output: LAYER3_url_inventory_[date].csv  (keyword-matched URLs per country)
#         LAYER3_inventory_stats_[date].csv (coverage per domain)
# ==============================================================================

library(tidyverse)
library(httr2)
library(xml2)
library(rvest)   # for the news/events/publications link crawl

stamp <- format(Sys.Date(), "%Y%m%d")

# Same 50 domains as Layer 2
nso_registry <- tribble(
  ~country,               ~domain,
  "Kenya",                "knbs.or.ke",
  "Uganda",               "ubos.org",
  "Tanzania",             "nbs.go.tz",
  "Ethiopia",             "statsethiopia.gov.et",
  "South Africa",         "statssa.gov.za",
  "Zimbabwe",             "zimstat.co.zw",
  "Zambia",               "zamstats.gov.zm",
  "Nigeria",              "nigerianstat.gov.ng",
  "Ghana",                "statsghana.gov.gh",
  "Senegal",              "ansd.sn",
  "Jordan",               "dos.gov.jo",
  "Lebanon",              "cas.gov.lb",
  "Iraq",                 "cosit.gov.iq",
  "Türkiye",              "tuik.gov.tr",
  "Bangladesh",           "bbs.gov.bd",
  "Pakistan",             "pbs.gov.pk",
  "Colombia",             "dane.gov.co",
  "Mexico",               "inegi.org.mx",
  "Sudan",                "cbs.gov.sd",
  "South Sudan",          "nbs.gov.ss",
  "Somalia",              "nbs.gov.so",
  "DR Congo",             "ins-rdc.org",
  "Chad",                 "inseed.td",
  "Cameroon",             "ins-cameroun.cm",
  "Mali",                 "instat-mali.org",
  "Burkina Faso",         "insd.bf",
  "Niger",                "stat-niger.org",
  "Mozambique",           "ine.gov.mz",
  "Rwanda",               "statistics.gov.rw",
  "Burundi",              "insbu.bi",
  "Egypt",                "capmas.gov.eg",
  "Morocco",              "hcp.ma",
  "Tunisia",              "ins.tn",
  "Côte d'Ivoire",        "ins.ci",
  "State of Palestine",   "pcbs.gov.ps",
  "Afghanistan",          "nsia.gov.af",
  "Syria",                "cbssyr.sy",
  "Armenia",              "armstat.am",
  "Azerbaijan",           "stat.gov.az",
  "Georgia",              "geostat.ge",
  "Ukraine",              "ukrstat.gov.ua",
  "Moldova",              "statistica.md",
  "Kazakhstan",           "stat.gov.kz",
  "Kyrgyzstan",           "stat.kg",
  "Nepal",                "nsonepal.gov.np",
  "Sri Lanka",            "statistics.gov.lk",
  "Philippines",          "psa.gov.ph",
  "Indonesia",            "bps.go.id",
  "Peru",                 "inei.gob.pe",
  "Ecuador",              "ecuadorencifras.gob.ec"
)
# To scale to the remaining ~150 statistical websites: drop NSO_Full_Registry.csv
# (columns: country, domain) in this folder and it is picked up automatically.
if (file.exists("NSO_Full_Registry.csv")) {
  full_reg <- read_csv("NSO_Full_Registry.csv", show_col_types = FALSE)
  if (all(c("country", "domain") %in% names(full_reg))) {
    nso_registry <- full_reg %>% select(country, domain, any_of("priority"))
    message(paste("Using NSO_Full_Registry.csv:", nrow(nso_registry), "domains",
                  if ("priority" %in% names(nso_registry)) "(with priority)" else "(no priority col -> all treated as 2)"))
  }
}

# Global partner catalogue websites (MICS + DHS), crawled the same way but
# labelled distinctly so downstream they are treated as partner sources, not NSO
# (user decision 15 Jun 2026). Only their displacement-relevant pages survive
# the keyword filter, so standard health-survey pages are excluded.
partner_registry <- tribble(
  ~country,            ~domain,
  "Global (MICS)",     "mics.unicef.org",
  "Global (DHS)",      "dhsprogram.com"
)
nso_registry <- bind_rows(nso_registry, partner_registry)
message(paste("Plus", nrow(partner_registry), "global partner catalogues (MICS, DHS)"))

# ------------------------------------------------------------------------------
# URL-level keywords, split STRONG vs WEAK (diagnosed 15 Jun 2026 against the
# real L3 inventory). Three leaks were killing precision on mega-sites:
#   * "migration"/"migracion"  -> routine residential-mobility demography (1019 hits,
#       US/NZ/NL), not forced displacement. Demoted to WEAK; bare form dropped.
#   * "idp" matching "&idp=..." -> INE (Spain) URL query params (891 hits). The
#       token now requires PATH-separator boundaries, so "&idp=" never matches.
#   * "asile"/"asilo"           -> match inside "brasileiro" (Brazil, 78 hits) and
#       mean 'care home' in ES/PT. Dropped from the URL filter entirely.
#
# STRONG = specific displacement terms. A strong hit is always a candidate.
# WEAK   = related-but-noisy. A weak-only hit is kept only for priority 1/2
#          domains and is flagged as a review-tier match, never auto-candidate.
# ------------------------------------------------------------------------------
URL_STRONG <- c(
  "refugee", "refugie", "refugiad", "refugiado", "asylum",
  # idp/idps only as a PATH token (/idp/ /idp- _idps. ...), never &idp= or idp:1234
  "(^|[/_.-])idps?([/_.-]|$)",
  "displaced", "displacement", "deplace", "desplazado", "deslocad",
  "stateless", "apatride", "apatrida",
  "forcibly", "forced-displacement", "forced-migration", "migration-forcee",
  "egriss", "irrs", "iross",
  "kihbs", "unhcr", "acnur", "persons-of-concern", "displaced-person",
  "wakimbizi", "multeci", "siginmaci", "gecici-koruma", "yerinden", "vatansiz",
  "bezhen", "pereselen", "fluechtling", "fluchtling", "vluchteling", "rifugiat",
  "sfollat", "uchodz", "przesiedl", "menekult", "uprchli", "utecen", "vysidlen",
  "begunc", "izbeglic", "izbjeglic", "raseljen", "refugiat", "stramutat",
  "pakolai", "pagulas"
)
URL_WEAK <- c(
  "migration", "migracion",          # mostly internal/residential mobility
  "returnee", "retourn", "retornad",
  "durable-solution", "solutions-durables",
  "host-communit"
  # NOTE: bare "asile"/"asilo" and "nationality"/"citizenship" removed entirely -
  # they matched 'brasileiro', care-homes, and 'population by nationality' pages.
)
url_pattern_strong <- paste(URL_STRONG, collapse = "|")
url_pattern_weak   <- paste(URL_WEAK,   collapse = "|")
url_pattern        <- paste(c(URL_STRONG, URL_WEAK), collapse = "|")  # any match

# normalise the ES/EN (and other 2-letter) language path segment so a bilingual
# site's twin URLs (/es/... and /en/...) collapse to one record
norm_lang_path <- function(u) {
  str_replace_all(str_to_lower(u),
                  "/(es|en|ca|eu|gl|fr|pt|de|it|nl|ru|uk|ar)/", "/LANG/")
}

MAX_MATCHES_PER_DOMAIN <- 150   # no single mega-site may dominate the inventory

# spam / unrelated commercial URLs are excluded outright
SPAM_URL_PAT <- "casino|bett?ing|gambl|jackpot|slots?|poker|bonus|viagra|porn|escort|hack"

`%||%` <- function(a, b) if (is.null(a)) b else a

RECENCY_CUTOFF <- as.Date("2024-01-01")   # keep URLs modified/dated 2024+ where known

# ------------------------------------------------------------------------------
# A. SITEMAP HARVEST (handles sitemap index files recursively, captures lastmod)
# ------------------------------------------------------------------------------
fetch_sitemap_urls <- function(domain, max_sitemaps = 20) {
  candidates <- c(
    paste0("https://", domain, "/sitemap.xml"),
    paste0("https://www.", domain, "/sitemap.xml"),
    paste0("https://", domain, "/sitemap_index.xml"),
    paste0("https://", domain, "/wp-sitemap.xml")
  )

  read_one <- function(u) {
    tryCatch({
      resp <- request(u) %>%
        req_user_agent("EGRISS-GAIN-research (statistics inventory)") %>%
        req_timeout(30) %>% req_perform()
      read_xml(resp_body_string(resp))
    }, error = function(e) NULL)
  }

  out <- tibble(url = character(0), lastmod = character(0))
  queue <- candidates
  seen <- character(0)
  n_processed <- 0

  while (length(queue) > 0 && n_processed < max_sitemaps) {
    u <- queue[1]; queue <- queue[-1]
    if (u %in% seen) next
    seen <- c(seen, u)
    doc <- read_one(u)
    if (is.null(doc)) next
    n_processed <- n_processed + 1

    ns <- xml_ns_strip(doc)
    is_index <- length(xml_find_all(ns, ".//sitemap")) > 0
    if (is_index) {
      queue <- c(queue, xml_text(xml_find_all(ns, ".//sitemap/loc")))
    } else {
      nodes <- xml_find_all(ns, ".//url")
      if (length(nodes) > 0) {
        out <- bind_rows(out, tibble(
          url = xml_text(xml_find_first(nodes, "./loc")) %>% as.character(),
          lastmod = xml_text(xml_find_first(nodes, "./lastmod")) %>% as.character()
        ))
      } else {
        locs <- xml_text(xml_find_all(ns, ".//loc"))
        if (length(locs) > 0) out <- bind_rows(out, tibble(url = locs, lastmod = NA))
      }
    }
    Sys.sleep(0.5)
  }
  distinct(out, url, .keep_all = TRUE)
}

# ------------------------------------------------------------------------------
# B. COMMON CRAWL INDEX (latest crawl; free, no key)
# ------------------------------------------------------------------------------
get_latest_cc_index <- function() {
  res <- tryCatch({
    request("https://index.commoncrawl.org/collinfo.json") %>%
      req_timeout(30) %>% req_perform() %>% resp_body_json()
  }, error = function(e) NULL)
  if (is.null(res)) return("CC-MAIN-2026-18")  # fallback guess; update if needed
  res[[1]]$id
}

fetch_commoncrawl_urls <- function(domain, cc_index, max_lines = 50000) {
  res <- tryCatch({
    request(paste0("https://index.commoncrawl.org/", cc_index, "-index")) %>%
      req_url_query(url = paste0("*.", domain, "/*"), output = "json",
                    fl = "url", limit = max_lines) %>%
      req_timeout(120) %>%
      req_perform()
  }, error = function(e) NULL)
  if (is.null(res)) return(character(0))

  lines <- str_split(resp_body_string(res), "\n")[[1]]
  lines <- lines[lines != ""]
  urls <- map_chr(lines, function(l) {
    tryCatch(jsonlite::fromJSON(l)$url %||% NA_character_,
             error = function(e) NA_character_)
  })
  unique(na.omit(urls))
}

# ------------------------------------------------------------------------------
# C. NEWS / EVENTS / PUBLICATIONS SECTIONS (shallow crawl, anchor-TEXT matching)
# News articles and event pages rarely carry keywords in the URL; matching the
# link text catches announcements of displacement-related surveys and releases.
# ------------------------------------------------------------------------------
NEWS_PATHS <- c(
  "news", "press", "press-releases", "press-room", "media", "media-center",
  "events", "publications", "publication", "reports", "releases",
  "actualites", "communiques", "noticias", "publicaciones", "publicacoes",
  "haberler", "duyurular", "novosti", "novini", "novyny"
)

# multilingual content terms for anchor text (key terms across pipeline languages)
CONTENT_PATTERN <- paste0(
  # asile/asilo dropped (match 'brasileiro' / care-home); idp needs a real word
  # boundary (space/path), never '&idp=' query params
  "refugee|réfugié|refugiado|asylum|displac|déplacé|desplazad|deslocad|",
  "stateless|apatrid|(^|[\\s/_.-])idps?([\\s/_.-]|$)|لاجئ|نازح|اللجوء|الجنسية|беженц|біженц|переміщ|",
  "перемещ|разселени|раселени|izbeglic|izbjeglic|raseljen|mülteci|sığınmacı|",
  "wakimbizi|flücht|vertrieben|uchodź|przesiedl|menekült|uprchlí|utečen|",
  "πρόσφυγ|εκτοπισμέν|refugiaț|strămutat|begunc|refugjat|zhvendosur|",
  "难民|流离失所|難民|避難民|난민|실향민|ผู้ลี้ภัย|tị nạn|pengungsi|pelarian|egriss|",
  # 12 Jun 2026 recall expansion for anchor text:
  "unhcr|acnur|\\bhcr\\b|увкб|المفوضية|refugee camp|camp de réfugiés|",
  "asylum application|demande[s]? d'asile|solicitudes de asilo|طلبات اللجوء|",
  "forced migration|migration forcée|migración forzada|вынужденная миграция|",
  "вимушена міграція|الهجرة القسرية|zorunlu göç|returnee|retourné|retornad|",
  "возвращенц|العائدين|durable solution|solutions durables|",
  # statelessness-specific only - bare nationality/citizenship terms flooded
  # results with routine demographic pages ("población por nacionalidad")
  "sin nacionalidad|sans nationalité|sem nacionalidade|без громадянства|без гражданства"
)

fetch_news_links <- function(domain) {
  out <- list(); tried <- 0; ok <- 0
  for (p in NEWS_PATHS) {
    if (tried >= 14 || ok >= 6) break
    u <- paste0("https://", domain, "/", p)
    tried <- tried + 1
    links <- tryCatch({
      resp <- request(u) %>%
        req_user_agent("EGRISS-GAIN-research (statistics inventory)") %>%
        req_timeout(20) %>% req_perform()
      if (!str_detect(resp_header(resp, "content-type") %||% "", "html")) NULL else {
        html <- read_html(resp_body_string(resp))
        a <- html_elements(html, "a")
        tibble(url = html_attr(a, "href"), link_text = str_squish(html_text(a)))
      }
    }, error = function(e) NULL)
    Sys.sleep(0.5)
    if (is.null(links) || nrow(links) == 0) next
    ok <- ok + 1
    # NOTE: no keyword filter here - raw candidate links are cached so that
    # keyword changes re-match instantly without re-crawling (filter in loop)
    links <- links %>%
      filter(!is.na(url), nchar(coalesce(link_text, "")) > 12) %>%
      mutate(url = if_else(str_starts(url, "http"), url,
                           paste0("https://", domain,
                                  str_replace(paste0("/", url), "^//+", "/")))) %>%
      filter(str_detect(url, fixed(domain))) %>%
      filter(!str_detect(str_to_lower(url), SPAM_URL_PAT)) %>%
      distinct(url, .keep_all = TRUE)
    if (nrow(links) > 0) out[[p]] <- links
  }
  bind_rows(out)
}

# ------------------------------------------------------------------------------
# PER-DOMAIN INVENTORY CACHE - makes re-runs INCREMENTAL:
# sitemap/Common Crawl/news fetches are saved per domain and reused for
# CACHE_MAX_AGE_DAYS. Changing keywords only re-matches the cached URL lists
# (instant); only new or stale domains are actually downloaded again.
# ------------------------------------------------------------------------------
CACHE_DIR_L3 <- "l3_cache"
dir.create(CACHE_DIR_L3, showWarnings = FALSE)
CACHE_MAX_AGE_DAYS <- 30

load_or_fetch_inventory <- function(domain, cc_index) {
  cache_file <- file.path(CACHE_DIR_L3,
                          paste0(str_replace_all(domain, "[^a-z0-9]", "_"), ".rds"))
  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    age <- as.numeric(difftime(Sys.time(), cached$fetched_at, units = "days"))
    if (age <= CACHE_MAX_AGE_DAYS) {
      message(paste0("    [cache] inventory reused (", round(age, 1), " days old)"))
      return(cached)
    }
  }
  sm <- fetch_sitemap_urls(domain)
  message(paste("    sitemap URLs:", nrow(sm)))
  cc_urls <- fetch_commoncrawl_urls(domain, cc_index)
  message(paste("    Common Crawl URLs:", length(cc_urls)))
  news <- fetch_news_links(domain)
  out <- list(sm = sm, cc_urls = cc_urls, news = news, fetched_at = Sys.time())
  saveRDS(out, cache_file)
  Sys.sleep(2)  # be polite between freshly fetched domains
  out
}

# ------------------------------------------------------------------------------
# MAIN LOOP
# ------------------------------------------------------------------------------
cc_index <- get_latest_cc_index()
message(paste("Using Common Crawl index:", cc_index, "\n"))

all_matches <- list()
stats <- list()

for (i in seq_len(nrow(nso_registry))) {
  row <- nso_registry[i, ]
  message(paste0("[", i, "/", nrow(nso_registry), "] ", row$country,
                 " (", row$domain, ")"))

  inv <- load_or_fetch_inventory(row$domain, cc_index)
  sm <- inv$sm
  sm_urls <- sm$url
  cc_urls <- inv$cc_urls
  # keyword matching happens HERE, on cached raw links - new keywords apply
  # instantly without re-crawling
  news <- inv$news
  if (nrow(news) > 0) {
    news <- news %>%
      filter(str_detect(str_to_lower(paste(link_text, url)), CONTENT_PATTERN))
  }
  message(paste0("    inventory: ", length(sm_urls), " sitemap + ",
                 length(cc_urls), " CC URLs; news links matched: ", nrow(news)))

  inventory <- unique(c(sm_urls, cc_urls))

  # priority of this domain (1 high .. 3 low); default 2 if column absent
  dom_priority <- if ("priority" %in% names(row) && !is.na(row$priority))
    as.integer(row$priority) else 2L

  inv_l <- str_to_lower(inventory)
  hit_strong <- str_detect(inv_l, url_pattern_strong)
  hit_weak   <- str_detect(inv_l, url_pattern_weak)
  # priority-3 domains (Spain, US, most EU) require a STRONG token; a weak-only
  # match there is noise (residential migration etc.) and is discarded
  keep <- if (dom_priority >= 3) hit_strong else (hit_strong | hit_weak)
  keep <- keep & !str_detect(inv_l, SPAM_URL_PAT)
  matches <- inventory[keep]
  match_is_strong <- hit_strong[keep]
  message(paste0("    matched URLs: ", length(matches), " (strong: ",
                 sum(match_is_strong), ", weak: ", sum(!match_is_strong),
                 ", domain priority ", dom_priority, ")"))

  if (length(matches) > 0 || nrow(news) > 0) {
    url_df <- tibble(
      country = row$country,
      domain  = row$domain,
      url     = matches,
      source  = case_when(matches %in% sm_urls & matches %in% cc_urls ~ "both",
                          matches %in% sm_urls ~ "sitemap",
                          TRUE ~ "commoncrawl"),
      matched_keyword = str_extract(str_to_lower(matches),
                                    paste(url_pattern_strong, url_pattern_weak, sep = "|")),
      match_strength = if_else(match_is_strong, "strong", "weak (review)"),
      is_pdf  = str_detect(str_to_lower(matches), "\\.pdf"),
      link_text = NA_character_
    )
    news_df <- if (nrow(news) > 0) {
      news %>%
        filter(!url %in% url_df$url) %>%
        transmute(
          country = row$country,
          domain  = row$domain,
          url,
          source  = "news-section",
          matched_keyword = str_extract(str_to_lower(paste(link_text, url)),
                                        CONTENT_PATTERN),
          match_strength = "strong",   # anchor-text match = meaningful signal
          is_pdf  = str_detect(str_to_lower(url), "\\.pdf"),
          link_text
        )
    } else tibble()
    match_df <- bind_rows(url_df, news_df) %>%
      left_join(sm, by = "url") %>%
      mutate(
        lastmod_date = suppressWarnings(as.Date(substr(lastmod, 1, 10))),
        url_year = suppressWarnings(as.numeric(str_extract(url, "20(2[0-9])"))),
        recency = case_when(
          !is.na(lastmod_date) & lastmod_date >= RECENCY_CUTOFF ~ "RECENT (lastmod)",
          !is.na(url_year) & url_year >= 2024                   ~ "RECENT (url year)",
          !is.na(lastmod_date) | !is.na(url_year)               ~ "OLDER",
          TRUE                                                  ~ "UNKNOWN"
        )
      ) %>%
      # collapse ES/EN (and other language) twin URLs to one record
      mutate(.langkey = norm_lang_path(url)) %>%
      arrange(match_strength, desc(str_starts(recency, "RECENT"))) %>%
      distinct(.langkey, .keep_all = TRUE) %>%
      select(-.langkey) %>%
      # per-domain cap: strong + most-recent first, so one mega-site can't dominate
      arrange(match_strength == "weak (review)", desc(str_starts(recency, "RECENT"))) %>%
      head(MAX_MATCHES_PER_DOMAIN)
    all_matches[[row$country]] <- match_df
    if (nrow(match_df) >= MAX_MATCHES_PER_DOMAIN)
      message(paste("    capped at", MAX_MATCHES_PER_DOMAIN, "matches for this domain"))
    message(paste("    kept after dedup/cap:", nrow(match_df),
                  "| RECENT (2024+):", sum(str_starts(match_df$recency, "RECENT"))))
  }

  stats[[row$country]] <- tibble(
    country = row$country, domain = row$domain,
    sitemap_urls = length(sm_urls),
    commoncrawl_urls = length(cc_urls),
    news_links = nrow(news),
    total_inventory = length(inventory),
    keyword_matches = length(matches),
    coverage = case_when(
      length(inventory) == 0 ~ "NONE - manual review needed",
      length(sm_urls) == 0   ~ "CC only",
      length(cc_urls) == 0   ~ "sitemap only",
      TRUE                   ~ "full"
    )
  )
  # (politeness sleep happens inside load_or_fetch_inventory, only on real fetches)
}

matched_df <- bind_rows(all_matches)
stats_df <- bind_rows(stats)

# UTF-8 with BOM so Excel renders all scripts correctly
write_excel_csv(matched_df, paste0("LAYER3_url_inventory_", stamp, ".csv"))
write_excel_csv(stats_df, paste0("LAYER3_inventory_stats_", stamp, ".csv"))

message("\n================ COVERAGE ================")
print(stats_df)
message(paste("\nTotal keyword-matched URLs:", nrow(matched_df)))
message(paste("Outputs: LAYER3_url_inventory_", stamp, ".csv  +  stats file"))
message("\nCountries with coverage = NONE need Layer 4 direct crawl or manual check.")
