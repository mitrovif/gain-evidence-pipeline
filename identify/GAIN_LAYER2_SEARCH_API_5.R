# ==============================================================================
# GAIN REFERENCE FILE - LAYER 2: SEARCH ENGINE HARVEST
#
# Instead of crawling NSO sites, query search engine indexes with
# site-restricted, multilingual queries:  site:knbs.or.ke refugee
#
# Engines supported (tried in this order per query, falling back on failure):
#   A. Google Programmable Search (JSON API) - 100 free queries/day, then $5/1000.
#   C. Exa neural search (exa.ai) - EXA_API_KEY in .Renviron. includeDomains =
#      site-restriction; startPublishedDate = recency. Strong PDF + semantic
#      coverage; use when Google is unavailable.
#   B. Brave Search API - free tier 2000/month. Last-resort backup.
#
# Flags:
#   GAIN_SKIP_GOOGLE=1  -> bypass Google entirely (go straight to Exa -> Brave).
#                          Use when Google keys are dead/disabled.
#   GAIN_RETRY_BRAVE=1  -> re-run previously Brave-answered queries (now through
#                          Exa first). Combine the two to upgrade weak Brave hits.
#
# Setup (one time):
#   Google: 1. console.cloud.google.com -> enable "Custom Search API" -> API key
#           2. programmablesearchengine.google.com -> create engine,
#              "Search the entire web" ON -> copy the cx ID
#   Sys.setenv(GOOGLE_CSE_KEY = "...", GOOGLE_CSE_CX = "...")
#   Brave:  api.search.brave.com -> free key
#   Sys.setenv(BRAVE_API_KEY = "...")
#
# Output: LAYER2_search_hits_[date].csv
# Resumable: progress saved after every country; re-running skips done work.
# ==============================================================================

library(tidyverse)
library(httr2)
library(jsonlite)

stamp <- format(Sys.Date(), "%Y%m%d")
PROGRESS_FILE <- "LAYER2_progress.csv"

# ------------------------------------------------------------------------------
# SEARCH API KEY POOL
# Keys are never hard-coded: they are read from environment variables.
# GOOGLE_CSE_KEY is the legacy slot; SEARCH_API_KEY_1..4 are additional slots.
# When a slot hits its quota or fails, the run rotates to the next slot.
# The progress log records WHICH SLOT was used - never the key value itself.
# ------------------------------------------------------------------------------
KEY_SLOTS <- c("GOOGLE_CSE_KEY", "SEARCH_API_KEY_1", "SEARCH_API_KEY_2",
               "SEARCH_API_KEY_3", "SEARCH_API_KEY_4")

# Machine-independent key loading: the keys travel with the project folder
# (.Renviron next to this script), so any machine syncing the folder can run.
# Search order: working directory -> user home(s) -> legacy fixed path.
if (all(map_chr(c(KEY_SLOTS, "BRAVE_API_KEY"), Sys.getenv) == "")) {
  for (renv in c(file.path(getwd(), ".Renviron"),
                 path.expand("~/.Renviron"),
                 file.path(Sys.getenv("USERPROFILE"), ".Renviron")
                 )) {
    if (file.exists(renv)) {
      readRenviron(renv)
      message("Loaded keys from ", renv)
      break
    }
  }
}
key_vals <- map_chr(KEY_SLOTS, Sys.getenv)
key_pool <- KEY_SLOTS[key_vals != "" & !str_starts(key_vals, "PASTE")]  # skip .Renviron placeholders
current_slot <- 1
message(paste("Search key slots available:", length(key_pool),
              if (length(key_pool) > 0) paste0("(", paste(key_pool, collapse = ", "), ")") else ""))

# ------------------------------------------------------------------------------
# NSO REGISTRY - extend to full list; language drives which keywords are used
# cse_group routes each country to a Google engine (max 50 domains per engine,
# per Google's Jan 2026 policy). Test registry fits in one engine = group 1.
# ------------------------------------------------------------------------------
nso_registry <- tribble(
  ~country,               ~domain,                  ~languages, ~cse_group,
  # --- original 18 ---
  "Kenya",                "knbs.or.ke",             "en",       1,
  "Uganda",               "ubos.org",               "en",       1,
  "Tanzania",             "nbs.go.tz",              "en,sw",    1,
  "Ethiopia",             "statsethiopia.gov.et",   "en",       1,
  "South Africa",         "statssa.gov.za",         "en",       1,
  "Zimbabwe",             "zimstat.co.zw",          "en",       1,
  "Zambia",               "zamstats.gov.zm",        "en",       1,
  "Nigeria",              "nigerianstat.gov.ng",    "en",       1,
  "Ghana",                "statsghana.gov.gh",      "en",       1,
  "Senegal",              "ansd.sn",                "fr",       1,
  "Jordan",               "dos.gov.jo",             "ar,en",    1,
  "Lebanon",              "cas.gov.lb",             "ar,en,fr", 1,
  "Iraq",                 "cosit.gov.iq",           "ar,en",    1,
  "Türkiye",              "tuik.gov.tr",            "tr,en",    1,
  "Bangladesh",           "bbs.gov.bd",             "en",       1,
  "Pakistan",             "pbs.gov.pk",             "en",       1,
  "Colombia",             "dane.gov.co",            "es",       1,
  "Mexico",               "inegi.org.mx",           "es",       1,
  # --- expansion to 50 (displacement-priority) ---
  "Sudan",                "cbs.gov.sd",             "ar,en",    1,
  "South Sudan",          "nbs.gov.ss",             "en",       1,
  "Somalia",              "nbs.gov.so",             "en",       1,
  "DR Congo",             "ins-rdc.org",            "fr",       1,
  "Chad",                 "inseed.td",              "fr",       1,
  "Cameroon",             "ins-cameroun.cm",        "fr,en",    1,
  "Mali",                 "instat-mali.org",        "fr",       1,
  "Burkina Faso",         "insd.bf",                "fr",       1,
  "Niger",                "stat-niger.org",         "fr",       1,
  "Mozambique",           "ine.gov.mz",             "pt",       1,
  "Rwanda",               "statistics.gov.rw",      "en",       1,
  "Burundi",              "insbu.bi",               "fr",       1,
  "Egypt",                "capmas.gov.eg",          "ar",       1,
  "Morocco",              "hcp.ma",                 "fr,ar",    1,
  "Tunisia",              "ins.tn",                 "fr,ar",    1,
  "Côte d'Ivoire",        "ins.ci",                 "fr",       1,
  "State of Palestine",   "pcbs.gov.ps",            "ar,en",    1,
  "Afghanistan",          "nsia.gov.af",            "en",       1,
  "Syria",                "cbssyr.sy",              "ar",       1,
  "Armenia",              "armstat.am",             "en,ru",    1,
  "Azerbaijan",           "stat.gov.az",            "en,ru",    1,
  "Georgia",              "geostat.ge",             "en",       1,
  "Ukraine",              "ukrstat.gov.ua",         "en,ru",    1,
  "Moldova",              "statistica.md",          "en,ru",    1,
  "Kazakhstan",           "stat.gov.kz",            "ru,en",    1,
  "Kyrgyzstan",           "stat.kg",                "ru",       1,
  "Nepal",                "nsonepal.gov.np",        "en",       1,
  "Sri Lanka",            "statistics.gov.lk",      "en",       1,
  "Philippines",          "psa.gov.ph",             "en",       1,
  "Indonesia",            "bps.go.id",              "en",       1,
  "Peru",                 "inei.gob.pe",            "es",       1,
  "Ecuador",              "ecuadorencifras.gob.ec", "es",       1
)
# To scale to the remaining ~150 statistical websites: drop a file named
# NSO_Full_Registry.csv (columns: country, domain, languages, cse_group) in this
# folder and it is picked up automatically. cse_group 1..4, max 50 domains each.
if (file.exists("NSO_Full_Registry.csv")) {
  full_reg <- read_csv("NSO_Full_Registry.csv", show_col_types = FALSE)
  if (all(c("country", "domain") %in% names(full_reg))) {
    if (!"languages" %in% names(full_reg)) full_reg$languages <- "en"
    if (!"cse_group" %in% names(full_reg)) full_reg$cse_group <- 1
    nso_registry <- full_reg %>% select(country, domain, languages, cse_group)
    message(paste("Using NSO_Full_Registry.csv:", nrow(nso_registry), "domains"))
  }
}

# Map each cse_group to its engine ID. With one engine, only group 1 is set.
GOOGLE_CX_BY_GROUP <- c(
  "1" = Sys.getenv("GOOGLE_CSE_CX"),    # your current engine
  "2" = Sys.getenv("GOOGLE_CSE_CX2"),   # create when scaling past 50 domains
  "3" = Sys.getenv("GOOGLE_CSE_CX3"),
  "4" = Sys.getenv("GOOGLE_CSE_CX4")
)

# ------------------------------------------------------------------------------
# HELPER: print paste-ready site patterns for the engine's "Sites to search"
# Run print_cse_site_list(2) etc. and paste the block into the Google panel.
#
# Google's pattern rules: "*.domain" is used for domains the NSO owns
# (incl. registrations under public suffixes like gov.xx / go.xx / org.xx);
# NSOs hosted as a subdomain of a shared government portal (govmu.org,
# admin.ch, fgov.be, ...) get the entire-site form "host/*" instead.
# Bare public-suffix patterns (*.com, *.gov.bh) are never emitted.
# ------------------------------------------------------------------------------
print_cse_site_list <- function(group = 1) {
  doms <- nso_registry %>% filter(cse_group == group) %>% pull(domain)
  # common public-suffix second levels: domain right after them is registrable
  psl2 <- "^[^.]+\\.(gov|go|govt|gob|gub|gc|org|or|com|co|net|ac|edu|rks-gov)\\."
  pattern <- if_else(
    str_count(doms, "\\.") >= 2 & !str_detect(doms, psl2),
    paste0(doms, "/*"),    # subdomain of a shared portal -> entire site
    paste0("*.", doms)     # own domain -> entire domain
  )
  cat(pattern, sep = "\n")
  invisible(pattern)
}

# ------------------------------------------------------------------------------
# MULTILINGUAL KEYWORD DICTIONARY
# One query per language per population group keeps query count manageable:
#   queries per country = n_languages x 3 population groups
# ------------------------------------------------------------------------------
keyword_dict <- tribble(
  ~lang, ~population,  ~query,
  "en",  "refugees",   "refugee OR refugees OR asylum OR \"asylum seekers\" OR \"forcibly displaced\"",
  "en",  "idps",       "\"internally displaced\" OR IDP OR IDPs OR displacement OR \"durable solutions\" OR returnees",
  "en",  "stateless",  "stateless OR statelessness OR \"without nationality\"",
  "fr",  "refugees",   "réfugiés OR réfugié OR asile OR \"demandeurs d'asile\" OR \"déplacés de force\"",
  "fr",  "idps",       "\"déplacés internes\" OR déplacées OR déplacement OR \"solutions durables\" OR retournés",
  "fr",  "stateless",  "apatride OR apatrides OR apatridie",
  "es",  "refugees",   "refugiados OR refugiado OR asilo OR \"solicitantes de asilo\"",
  "es",  "idps",       "\"desplazados internos\" OR desplazamiento OR \"desplazamiento forzado\" OR retornados",
  "es",  "stateless",  "apátrida OR apátridas OR apatridia",
  "ar",  "refugees",   "لاجئين OR اللاجئين OR لجوء OR \"طالبي اللجوء\"",
  "ar",  "idps",       "نازحين OR النازحين OR نزوح OR \"النزوح الداخلي\"",
  "ar",  "stateless",  "\"عديمي الجنسية\" OR \"انعدام الجنسية\"",
  "ru",  "refugees",   "беженцы OR беженцев OR \"лица, ищущие убежище\"",
  "ru",  "idps",       "\"внутренне перемещенные\" OR переселенцы OR \"вынужденные переселенцы\"",
  "ru",  "stateless",  "апатриды OR \"без гражданства\"",
  "tr",  "refugees",   "mülteci OR mülteciler OR sığınmacı OR \"geçici koruma\" OR \"uluslararası koruma\"",
  "tr",  "idps",       "\"yerinden edilmiş\" OR \"iç göç\" OR \"zorla yerinden\"",
  "tr",  "stateless",  "vatansız OR vatansızlık",
  "pt",  "refugees",   "refugiados OR refugiado OR asilo OR \"deslocação forçada\"",
  "pt",  "idps",       "\"deslocados internos\" OR deslocamento OR retornados",
  "pt",  "stateless",  "apátrida OR apátridas",
  "sw",  "refugees",   "wakimbizi OR ukimbizi",
  "sw",  "idps",       "wakimbizi wa ndani OR waliohamishwa",
  "sw",  "stateless",  "\"wasio na uraia\"",
  # --- languages of the group 2-4 NSO websites (+ uk/id for group 1) ---
  "uk",  "refugees",   "біженці OR біженців OR притулок",
  "uk",  "idps",       "\"внутрішньо переміщені\" OR ВПО OR переміщених",
  "uk",  "stateless",  "\"без громадянства\" OR апатриди",
  "id",  "refugees",   "pengungsi OR \"pencari suaka\"",
  "id",  "idps",       "\"pengungsi internal\" OR \"pengungsian dalam negeri\"",
  "id",  "stateless",  "\"tanpa kewarganegaraan\"",
  "fa",  "refugees",   "پناهندگان OR پناهنده OR پناهجویان",
  "fa",  "idps",       "آوارگان OR \"آوارگان داخلی\" OR بیجاشدگان",
  "fa",  "stateless",  "\"بدون تابعیت\" OR \"بی تابعیتی\"",
  "he",  "refugees",   "פליטים OR \"מבקשי מקלט\"",
  "he",  "idps",       "עקורים OR \"עקורים פנימיים\"",
  "he",  "stateless",  "\"חסרי אזרחות\"",
  "mn",  "refugees",   "дүрвэгсэд OR дүрвэгч OR орогнол",
  "mn",  "idps",       "\"дотоодын дүрвэгсэд\"",
  "mn",  "stateless",  "харьяалалгүй",
  "bcs", "refugees",   "izbeglice OR izbjeglice OR azil OR \"tražioci azila\"",
  "bcs", "idps",       "\"interno raseljena lica\" OR raseljeni OR raseljenih",
  "bcs", "stateless",  "apatridi OR \"bez državljanstva\"",
  "sq",  "refugees",   "refugjatët OR refugjatë OR azil",
  "sq",  "idps",       "\"të zhvendosur\" OR zhvendosje",
  "sq",  "stateless",  "\"pa shtetësi\" OR apatridë",
  "mk",  "refugees",   "бегалци OR бегалците OR азил",
  "mk",  "idps",       "\"внатрешно раселени\" OR раселени",
  "mk",  "stateless",  "\"без државјанство\" OR апатриди",
  "sl",  "refugees",   "begunci OR beguncev OR azil",
  "sl",  "idps",       "\"notranje razseljene\" OR razseljeni",
  "sl",  "stateless",  "apatridi OR \"brez državljanstva\"",
  "bg",  "refugees",   "бежанци OR бежанците OR убежище",
  "bg",  "idps",       "\"вътрешно разселени\" OR разселени",
  "bg",  "stateless",  "\"без гражданство\" OR апатриди",
  "ro",  "refugees",   "refugiați OR refugiaților OR azil",
  "ro",  "idps",       "\"persoane strămutate\" OR strămutați OR strămutare",
  "ro",  "stateless",  "apatrizi OR apatridie",
  "hu",  "refugees",   "menekültek OR menekült OR menedékkérők",
  "hu",  "idps",       "\"belső menekültek\" OR \"lakóhelyüket elhagyni kényszerült\"",
  "hu",  "stateless",  "hontalanok OR hontalanság",
  "pl",  "refugees",   "uchodźcy OR uchodźców OR azyl",
  "pl",  "idps",       "\"przesiedleńcy wewnętrzni\" OR przesiedleńcy OR wysiedleni",
  "pl",  "stateless",  "bezpaństwowcy OR bezpaństwowość",
  "cs",  "refugees",   "uprchlíci OR uprchlíků OR azyl",
  "cs",  "idps",       "\"vnitřně vysídlené\" OR vysídlenci",
  "cs",  "stateless",  "\"bez státní příslušnosti\" OR apatridé",
  "sk",  "refugees",   "utečenci OR utečencov OR azyl",
  "sk",  "idps",       "\"vnútorne vysídlené\" OR vysídlenci",
  "sk",  "stateless",  "\"bez štátnej príslušnosti\"",
  "el",  "refugees",   "πρόσφυγες OR προσφύγων OR άσυλο",
  "el",  "idps",       "\"εσωτερικά εκτοπισμένοι\" OR εκτοπισμένοι",
  "el",  "stateless",  "ανιθαγενείς OR ανιθαγένεια",
  "lv",  "refugees",   "bēgļi OR \"patvēruma meklētāji\"",
  "lv",  "idps",       "\"iekšzemē pārvietotie\"",
  "lv",  "stateless",  "bezvalstnieki OR bezvalstniecība",
  "lt",  "refugees",   "pabėgėliai OR \"prieglobsčio prašytojai\"",
  "lt",  "idps",       "\"šalies viduje perkelti\"",
  "lt",  "stateless",  "\"be pilietybės\" OR bepilietybė",
  "et",  "refugees",   "pagulased OR varjupaigataotlejad",
  "et",  "idps",       "\"riigisiseselt ümberasustatud\"",
  "et",  "stateless",  "kodakondsuseta",
  "fi",  "refugees",   "pakolaiset OR turvapaikanhakijat",
  "fi",  "idps",       "\"maan sisäiset pakolaiset\" OR \"siirtymään joutuneet\"",
  "fi",  "stateless",  "kansalaisuudettomat OR kansalaisuudettomuus",
  "sv",  "refugees",   "flyktingar OR asylsökande OR asyl",
  "sv",  "idps",       "internflyktingar OR fördrivna",
  "sv",  "stateless",  "statslösa OR statslöshet",
  "no",  "refugees",   "flyktninger OR asylsøkere OR asyl",
  "no",  "idps",       "\"internt fordrevne\" OR fordrevne",
  "no",  "stateless",  "statsløse OR statsløshet",
  "da",  "refugees",   "flygtninge OR asylansøgere OR asyl",
  "da",  "idps",       "\"internt fordrevne\" OR fordrevne",
  "da",  "stateless",  "statsløse OR statsløshed",
  "de",  "refugees",   "Flüchtlinge OR Geflüchtete OR Asylbewerber OR Asyl",
  "de",  "idps",       "Binnenvertriebene OR Vertriebene OR Vertreibung",
  "de",  "stateless",  "Staatenlose OR Staatenlosigkeit",
  "nl",  "refugees",   "vluchtelingen OR asielzoekers OR asiel",
  "nl",  "idps",       "\"intern ontheemden\" OR ontheemden",
  "nl",  "stateless",  "staatlozen OR staatloosheid OR staatloos",
  "it",  "refugees",   "rifugiati OR \"richiedenti asilo\" OR asilo",
  "it",  "idps",       "sfollati OR \"sfollati interni\"",
  "it",  "stateless",  "apolidi OR apolidia",
  "zh",  "refugees",   "难民 OR 寻求庇护者 OR 庇护",
  "zh",  "idps",       "境内流离失所者 OR 流离失所",
  "zh",  "stateless",  "无国籍",
  "ja",  "refugees",   "難民 OR 庇護申請者",
  "ja",  "idps",       "国内避難民 OR 避難民",
  "ja",  "stateless",  "無国籍",
  "ko",  "refugees",   "난민 OR 비호신청자",
  "ko",  "idps",       "국내실향민 OR 실향민",
  "ko",  "stateless",  "무국적 OR 무국적자",
  "th",  "refugees",   "ผู้ลี้ภัย OR ผู้ขอลี้ภัย",
  "th",  "idps",       "ผู้พลัดถิ่นภายในประเทศ OR ผู้พลัดถิ่น",
  "th",  "stateless",  "คนไร้สัญชาติ OR ไร้สัญชาติ",
  "vi",  "refugees",   "\"người tị nạn\" OR \"xin tị nạn\"",
  "vi",  "idps",       "\"di tản trong nước\" OR \"người di tản\"",
  "vi",  "stateless",  "\"không quốc tịch\"",
  "ms",  "refugees",   "pelarian OR \"pencari suaka\"",
  "ms",  "idps",       "\"pemindahan dalaman\" OR \"orang pelarian dalaman\"",
  "ms",  "stateless",  "\"tanpa kewarganegaraan\"",
  # --- SUPPLEMENTARY groups (added 12 Jun 2026 for higher recall) -------------
  # New population values = new progress keys, so countries already searched
  # run ONLY these new queries on the next run - nothing is re-scraped.
  "en",  "refugees_plus",  "\"refugee camp\" OR \"asylum applications\" OR \"persons of concern\" OR \"refugee statistics\" OR UNHCR",
  "en",  "idps_plus",      "\"displaced persons\" OR \"displaced households\" OR \"forced migration\" OR returnees OR \"conflict-affected\"",
  "en",  "stateless_plus", "\"without citizenship\" OR \"citizenship status\" OR \"nationality data\" OR \"legal identity\"",
  "fr",  "refugees_plus",  "\"camp de réfugiés\" OR \"demandes d'asile\" OR HCR OR \"statistiques sur les réfugiés\"",
  "fr",  "idps_plus",      "\"personnes déplacées\" OR \"ménages déplacés\" OR \"migration forcée\" OR retournés",
  "fr",  "stateless_plus", "\"sans nationalité\" OR \"nationalité indéterminée\" OR \"identité juridique\"",
  "es",  "refugees_plus",  "\"campamento de refugiados\" OR \"solicitudes de asilo\" OR ACNUR OR \"estadísticas de refugiados\"",
  "es",  "idps_plus",      "\"personas desplazadas\" OR \"hogares desplazados\" OR \"migración forzada\" OR retornados",
  "es",  "stateless_plus", "\"sin nacionalidad\" OR \"nacionalidad indeterminada\" OR \"identidad legal\"",
  "ar",  "refugees_plus",  "\"مخيمات اللاجئين\" OR \"طلبات اللجوء\" OR المفوضية OR \"إحصاءات اللاجئين\"",
  "ar",  "idps_plus",      "المهجرين OR \"الأسر النازحة\" OR \"الهجرة القسرية\" OR العائدين OR \"الحلول الدائمة\"",
  "ar",  "stateless_plus", "\"بدون جنسية\" OR \"مكتومي القيد\" OR \"الهوية القانونية\"",
  "ru",  "refugees_plus",  "\"лагеря беженцев\" OR \"ходатайства об убежище\" OR УВКБ OR \"статистика беженцев\"",
  "ru",  "idps_plus",      "\"перемещенные лица\" OR \"вынужденная миграция\" OR возвращенцы OR \"долгосрочные решения\"",
  "ru",  "stateless_plus", "\"лица без гражданства\" OR \"правовая идентичность\"",
  "pt",  "refugees_plus",  "\"campo de refugiados\" OR \"pedidos de asilo\" OR ACNUR",
  "pt",  "idps_plus",      "\"pessoas deslocadas\" OR \"migração forçada\" OR retornados OR \"soluções duradouras\"",
  "pt",  "stateless_plus", "\"sem nacionalidade\" OR apatridia",
  "tr",  "refugees_plus",  "\"mülteci kampı\" OR \"sığınma başvuruları\" OR BMMYK",
  "tr",  "idps_plus",      "\"zorunlu göç\" OR \"geri dönenler\" OR \"yerinden edilmiş kişiler\"",
  "tr",  "stateless_plus", "\"vatandaşlığı olmayan\" OR vatansızlık",
  "uk",  "refugees_plus",  "\"табори біженців\" OR \"заяви про притулок\" OR УВКБ",
  "uk",  "idps_plus",      "\"переміщені особи\" OR \"вимушена міграція\" OR \"повернення ВПО\"",
  "uk",  "stateless_plus", "\"особи без громадянства\" OR \"правова ідентичність\""
)

# EGRISS / international recommendations query - run once per country
# (acronyms are language-independent; catches country-led implementation pages)
EGRISS_QUERY <- paste0(
  'EGRISS OR IRRS OR IROSS OR ',
  # IRIS = International Recommendations on IDP Statistics. The bare acronym is
  # ambiguous (eye/flower/products), so pair it with a displacement/IDP term in
  # the query - the search engine returns pages where both co-occur.
  '(IRIS AND (IDP OR "internally displaced" OR displacement OR statistics)) OR ',
  '"international recommendations on refugee statistics" OR ',
  '"international recommendations on internally displaced persons statistics" OR ',
  '"international recommendations on IDP statistics"'
)

# Inclusion-signal query (per reviewer): proves a displaced group was actually
# captured, not just that an instrument exists. One extra query per country.
INCLUSION_QUERY <- paste0(
  '"disaggregated by" (refugee OR IDP OR displacement OR nationality) OR ',
  '"refugee module" OR "IDP module" OR "displacement status" OR ',
  '"host community" OR returnees OR oversampling'
)

# ------------------------------------------------------------------------------
# RECENCY: restrict results to recent publications
# Google: dateRestrict (y2 = past 2 years). Brave: freshness date range.
# ------------------------------------------------------------------------------
# Months elapsed since Jan 2024, so coverage always starts at 2024-01
# (the old "y2" setting silently missed Jan-Jun 2024 when run in mid-2026)
months_since_2024 <- max(1, (as.integer(format(Sys.Date(), "%Y")) - 2024) * 12 +
                            as.integer(format(Sys.Date(), "%m")))
RECENCY_GOOGLE <- paste0("m", months_since_2024)
RECENCY_BRAVE  <- "2024-01-01to2026-12-31"

# ------------------------------------------------------------------------------
# ENGINE A: Google Programmable Search
# ------------------------------------------------------------------------------
# Returns tibble of hits, "QUOTA" (limit reached), "BADKEY" (invalid key),
# or NULL (transient error / not configured). Never prints the key value.
search_google <- function(query, domain, cx, api_key) {
  key <- api_key
  if (is.na(key) || key == "" || is.na(cx) || cx == "") return(NULL)

  res <- tryCatch({
    request("https://www.googleapis.com/customsearch/v1") %>%
      req_url_query(key = key, cx = cx,
                    q = query,
                    siteSearch = domain,
                    siteSearchFilter = "i",   # include only this domain
                    dateRestrict = RECENCY_GOOGLE,
                    sort = "date",
                    num = 10) %>%
      req_timeout(30) %>%
      req_perform() %>%
      resp_body_json()
  }, error = function(e) {
    body <- tryCatch(resp_body_string(e$resp), error = function(e2) "")
    if (grepl("SERVICE_DISABLED|accessNotConfigured|does not have the access to Custom Search|PERMISSION_DENIED",
              body)) {
      message("    Custom Search API is NOT ENABLED on this key's project.\n",
              "    Fix: https://console.cloud.google.com/apis/library/customsearch.googleapis.com\n",
              "    (select the right project top-left, click Enable, wait ~3 min)")
      return("BADKEY")   # rotate to the next slot instead of stopping the run
    }
    if (grepl("API key not valid|API_KEY_INVALID", body)) {
      return("BADKEY")   # rotate to the next key slot instead of stopping
    }
    if (grepl("RATE_LIMIT_EXCEEDED|rateLimitExceeded|dailyLimitExceeded|Quota exceeded", body)) {
      return("QUOTA")
    }
    message(paste("    Google error:", str_sub(body, 1, 200)))
    NULL
  })

  if (identical(res, "QUOTA")) return("QUOTA")
  if (identical(res, "BADKEY")) return("BADKEY")
  if (is.null(res$items)) return(tibble())

  map_dfr(res$items, function(it) {
    tibble(
      engine  = "google",
      title   = as.character(it$title %||% NA),
      url     = as.character(it$link %||% NA),
      snippet = as.character(it$snippet %||% NA)
    )
  })
}

# ------------------------------------------------------------------------------
# ENGINE B: Brave Search (backup / free tier)
# ------------------------------------------------------------------------------
search_brave <- function(query, domain) {
  key <- Sys.getenv("BRAVE_API_KEY")
  if (key == "") return(NULL)

  res <- tryCatch({
    request("https://api.search.brave.com/res/v1/web/search") %>%
      req_headers("X-Subscription-Token" = key) %>%
      req_url_query(q = paste0("site:", domain, " ", query),
                    freshness = RECENCY_BRAVE,
                    count = 10) %>%
      req_timeout(30) %>%
      req_perform() %>%
      resp_body_json()
  }, error = function(e) NULL)

  results <- res$web$results
  if (is.null(results)) return(tibble())

  map_dfr(results, function(it) {
    tibble(
      engine  = "brave",
      title   = as.character(it$title %||% NA),
      url     = as.character(it$url %||% NA),
      snippet = as.character(it$description %||% NA)
    )
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------------------------
# ENGINE C: Exa (neural search API, exa.ai) - supplements/replaces Google
# Set EXA_API_KEY in .Renviron. includeDomains gives the same site-restriction
# as site:, startPublishedDate gives recency. Returns tibble of hits, "QUOTA",
# "BADKEY", or NULL. Never prints the key value.
# ------------------------------------------------------------------------------
EXA_START_DATE <- "2024-01-01T00:00:00.000Z"   # recency floor (matches Brave)
search_exa <- function(query, domain) {
  key <- Sys.getenv("EXA_API_KEY")
  if (key == "") return(NULL)

  res <- tryCatch({
    request("https://api.exa.ai/search") %>%
      req_headers("x-api-key" = key, "Content-Type" = "application/json") %>%
      req_body_json(list(
        query              = query,
        type               = "auto",      # Exa picks neural vs keyword
        numResults         = 10,
        includeDomains     = list(domain), # same effect as site:domain
        startPublishedDate = EXA_START_DATE,
        contents           = list(text = list(maxCharacters = 400))
      )) %>%
      req_timeout(40) %>%
      req_perform() %>%
      resp_body_json()
  }, error = function(e) {
    body <- tryCatch(resp_body_string(e$resp), error = function(e2) "")
    if (grepl("401|unauthor|invalid api key|invalid.*key", body, ignore.case = TRUE))
      return("BADKEY")
    if (grepl("429|rate.?limit|quota|insufficient", body, ignore.case = TRUE))
      return("QUOTA")
    message(paste("    Exa error:", str_sub(body, 1, 200)))
    NULL
  })

  if (identical(res, "QUOTA") || identical(res, "BADKEY")) return(res)
  if (is.null(res$results)) return(tibble())

  map_dfr(res$results, function(it) {
    tibble(
      engine  = "exa",
      title   = as.character(it$title %||% NA),
      url     = as.character(it$url %||% NA),
      snippet = as.character(it$text %||% it$summary %||% NA)
    )
  })
}

# ------------------------------------------------------------------------------
# MAIN LOOP - resumable
# ------------------------------------------------------------------------------
done <- if (file.exists(PROGRESS_FILE)) {
  read_csv(PROGRESS_FILE, show_col_types = FALSE) %>%
    # force these to character so bind_rows with new rows never type-clashes
    # (readr may auto-parse attempt_date as <date>; new rows write <character>)
    mutate(across(any_of(c("attempt_date", "key_slot")), as.character))
} else {
  tibble(country = character(), population = character(), lang = character())
}

# Brave's index of NSO sites is weaker than Google's. Once Google keys work,
# re-run ONLY the Brave-answered queries on Google (everything else stays done):
#   Sys.setenv(GAIN_RETRY_BRAVE = "1"); source("identify/GAIN_LAYER2_SEARCH_API_5.R")
# Results are deduplicated by URL, so the overlap is harmless.
if (Sys.getenv("GAIN_RETRY_BRAVE") == "1" && "key_slot" %in% names(done)) {
  n0 <- nrow(done)
  done <- done %>% filter(is.na(key_slot) | key_slot != "BRAVE")
  message(paste("GAIN_RETRY_BRAVE=1:", n0 - nrow(done),
                "Brave-answered queries will be retried on Google"))
}

results_file <- paste0("LAYER2_search_hits_", stamp, ".csv")
quota_hit <- FALSE

# When Google is unavailable/broken, set GAIN_SKIP_GOOGLE=1 to bypass it entirely
# and go straight to Exa (then Brave) - avoids wasting time on dead Google calls.
SKIP_GOOGLE <- Sys.getenv("GAIN_SKIP_GOOGLE") == "1"
# Hard cap on Exa API calls per run (each ~1 credit). Default 1000; resumable -
# a capped run stops gracefully and the next run continues where it left off.
EXA_MAX <- suppressWarnings(as.integer(Sys.getenv("GAIN_EXA_MAX", unset = "1000")))
if (is.na(EXA_MAX) || EXA_MAX < 0) EXA_MAX <- 1000L
exa_calls <- 0L
if (SKIP_GOOGLE) message("GAIN_SKIP_GOOGLE=1: skipping Google, using Exa -> Brave")
if (Sys.getenv("EXA_API_KEY") != "")
  message("Exa search: ENABLED (cap ", EXA_MAX, " queries this run)")

for (i in seq_len(nrow(nso_registry))) {
  if (quota_hit) break
  row <- nso_registry[i, ]
  langs <- str_split(row$languages, ",")[[1]] %>% str_trim()
  queries <- keyword_dict %>%
    filter(lang %in% langs) %>%
    # one EGRISS + one inclusion query per country (acronyms are universal)
    bind_rows(tibble(lang = langs[1], population = "egriss", query = EGRISS_QUERY),
              tibble(lang = langs[1], population = "inclusion", query = INCLUSION_QUERY))

  for (j in seq_len(nrow(queries))) {
    q <- queries[j, ]

    already <- done %>%
      filter(country == row$country, population == q$population, lang == q$lang)
    if (nrow(already) > 0) next

    message(paste0("[", row$country, "] ", q$lang, "/", q$population))

    cx_for_country <- GOOGLE_CX_BY_GROUP[as.character(row$cse_group)]

    # --- try Google key slots in order, rotating on quota/failure -------------
    hits <- NULL
    used_slot <- NA_character_
    while (!SKIP_GOOGLE && current_slot <= length(key_pool)) {
      slot_name <- key_pool[current_slot]
      res <- search_google(q$query, row$domain, cx_for_country,
                           Sys.getenv(slot_name))
      if (identical(res, "QUOTA") || identical(res, "BADKEY") || is.null(res)) {
        reason <- if (identical(res, "QUOTA")) "quota reached"
                  else if (identical(res, "BADKEY")) "key invalid"
                  else "request failed"
        message(paste0("    key slot '", slot_name, "': ", reason,
                       " - rotating to next slot"))
        current_slot <- current_slot + 1
      } else {
        hits <- res
        used_slot <- slot_name
        break
      }
    }

    # --- Google unavailable: fall back to Exa (neural), then Brave -----------
    if (is.null(hits) && Sys.getenv("EXA_API_KEY") != "") {
      if (exa_calls >= EXA_MAX) {
        message(sprintf("  Exa cap reached (%d queries this run) - stopping. ",
                        EXA_MAX),
                "Progress saved; re-run to continue the next ", EXA_MAX, ".")
        quota_hit <- TRUE
        break
      }
      ex <- search_exa(q$query, row$domain)
      exa_calls <- exa_calls + 1L
      if (!is.null(ex) && !identical(ex, "QUOTA") && !identical(ex, "BADKEY")) {
        hits <- ex
        used_slot <- "EXA"
      } else if (identical(ex, "QUOTA")) {
        message("    Exa: quota/credits exhausted")
      } else if (identical(ex, "BADKEY")) {
        message("    Exa: key invalid (check EXA_API_KEY)")
      }
    }
    # --- last resort: Brave; else stop gracefully -----------------------------
    if (is.null(hits)) {
      hits <- search_brave(q$query, row$domain)
      if (!is.null(hits)) {
        used_slot <- "BRAVE"
      } else {
        message("  All engines unavailable (Google/Exa/Brave) - no working key.")
        message("  Stopping gracefully - progress is saved; re-run to resume.")
        quota_hit <- TRUE
        break
      }
    }

    if (nrow(hits) > 0) {
      hits <- hits %>%
        mutate(country = row$country, domain = row$domain,
               population = q$population, lang = q$lang,
               retrieved = as.character(Sys.Date())) %>%
        # drop hits that escaped the site restriction
        filter(str_detect(url, fixed(row$domain)))
      write_csv(hits, results_file, append = file.exists(results_file))
      message(paste("   ", nrow(hits), "hits"))
    } else {
      message("    0 hits")
    }

    done <- bind_rows(done, tibble(country = row$country,
                                   population = q$population, lang = q$lang,
                                   key_slot = used_slot,           # slot name only, never the key
                                   attempt_date = as.character(Sys.Date())))
    write_csv(done, PROGRESS_FILE)
    Sys.sleep(1.2)
  }
}

# ------------------------------------------------------------------------------
# REMAINING-WORK REPORT: which domains/queries are still unprocessed
# ------------------------------------------------------------------------------
expected <- pmap_dfr(nso_registry, function(country, domain, languages, cse_group, ...) {
  ctry <- country
  langs <- str_trim(str_split(languages, ",")[[1]])
  bind_rows(
    keyword_dict %>% filter(lang %in% langs) %>%
      transmute(country = ctry, population, lang),
    tibble(country = ctry, population = "egriss", lang = langs[1])
  )
})
remaining <- anti_join(expected, done, by = c("country", "population", "lang"))
if (nrow(remaining) > 0) {
  rem_countries <- unique(remaining$country)
  message(paste0("\nREMAINING: ", nrow(remaining), " queries across ",
                 length(rem_countries), " domains not yet processed:"))
  message(paste("  ", paste(rem_countries, collapse = ", ")))
  message("Re-run this script (with fresh quota or additional SEARCH_API_KEY_N slots) to continue.")
} else {
  message("\nAll registry domains fully processed.")
}
if (length(key_pool) > 0 && nrow(done) > 0 && "key_slot" %in% names(done)) {
  message("Key slot usage (slot names only):")
  print(count(filter(done, !is.na(key_slot)), key_slot))
}

if (file.exists(results_file)) {
  final <- read_csv(results_file, show_col_types = FALSE) %>%
    distinct(url, .keep_all = TRUE)
  write_excel_csv(final, results_file)   # UTF-8 BOM so Excel renders all scripts
  message(paste("\nTotal unique URLs found:", nrow(final),
                "across", n_distinct(final$country), "countries"))
} else {
  message("\nNo results yet.")
}
message(paste("Output:", results_file))
message("Resumable: re-run this script any time; completed queries are skipped.")
