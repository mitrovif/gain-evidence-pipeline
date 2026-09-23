# ==============================================================================
# GAIN EVIDENCE ENRICHMENT + REVIEW REPORT  (v6 - outreach edition)
#
# Input : newest GAIN_MASTER_REFERENCE_*.csv
# Output: GAIN_EVIDENCE_ENRICHED_[date].csv   (data backbone, UTF-8 BOM for Excel)
#         GAIN_EVIDENCE_REPORT_[date].html    (user-friendly review report)
#         LAYER3_inventory_stats_[date].csv   (UPDATED in place with processing
#                                              overview columns - no new file)
#
# What this version adds (kept inside the existing output structure):
#   - Public NSO contact-page discovery + outreach contact recommendation
#   - Candidate lead type (likely country-led / likely partner-led / unclear / n.a.)
#   - Outreach category A-E and outreach route/priority/notes
#   - Expanded Arabic keyword coverage + automated English term gloss for review
#   - Ukrainian/Russian IDP false-positive suppression (pidpr / enterprise pages)
#   - Relevance score caps so no-term records cannot score high
#   - Cautious GAIN framing throughout: everything is a POSSIBLE candidate that
#     requires manual review and NSO confirmation - never a confirmed example.
#
# Requires: install.packages("pdftools")   (one time)
# ==============================================================================

library(tidyverse)
library(httr2)
library(rvest)
library(pdftools)
library(rlang)

stamp <- format(Sys.Date(), "%Y%m%d")
CACHE_DIR <- "evidence_cache"
dir.create(CACHE_DIR, showWarnings = FALSE)

MAX_PDF_MB    <- 25
MAX_RECORDS   <- Inf     # set e.g. 30 for a test run
SNIPPET_CHARS <- 400     # context window around each mention
MAX_SNIPS     <- 5       # snippets per term group

# Optional manual override file: columns url, override_relevant (TRUE/FALSE), note
# Lets a reviewer confirm relevance so the automatic score caps are lifted.
OVERRIDE_FILE <- "MANUAL_OVERRIDES.csv"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ------------------------------------------------------------------------------
# MOJIBAKE REPAIR (same as in MERGE): fixes UTF-8 read as Windows-1252
# ("TÃ¼rkiye" -> "Türkiye"); harmless on clean text.
# ------------------------------------------------------------------------------
repair_mojibake <- function(x) {
  fix_one <- function(s) {
    if (is.na(s)) return(s)
    cur <- s
    for (i in 1:2) {
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

# ------------------------------------------------------------------------------
# LANGUAGE DETECTION (best-guess heuristic: script ranges + stopword scoring)
# ------------------------------------------------------------------------------
detect_language <- function(txt) {
  t <- coalesce(txt, "")
  if (nchar(t) < 20) return(NA_character_)
  if (str_detect(t, "\\p{Hangul}"))   return("Korean")
  if (str_detect(t, "[\\p{Hiragana}\\p{Katakana}]")) return("Japanese")
  if (str_detect(t, "\\p{Han}"))      return("Chinese")
  if (str_detect(t, "\\p{Thai}"))     return("Thai")
  if (str_detect(t, "\\p{Hebrew}"))   return("Hebrew")
  if (str_detect(t, "\\p{Arabic}"))
    return(if (str_detect(t, "[پگچژ]")) "Persian" else "Arabic")
  if (str_detect(t, "\\p{Greek}"))    return("Greek")
  if (str_detect(t, "\\p{Cyrillic}")) {
    if (str_detect(t, "[іїєґ]")) return("Ukrainian")
    if (str_detect(t, "[ыэъё]")) return("Russian")
    return("Cyrillic (undetermined)")
  }
  tl <- str_to_lower(t)
  scores <- c(
    English    = sum(str_count(tl, "\\b(the|and|of|for|with|from)\\b")),
    French     = sum(str_count(tl, "\\b(les|des|une|est|dans|pour|aux)\\b")),
    # Portuguese vs Spanish: use the markers each language does NOT share -
    # PT has ã/õ/"ção"/não/são; ES has ñ/¿¡/según; "como/del" alone is ambiguous
    Spanish    = sum(str_count(tl, "\\b(los|las|del|según|número|población|para)\\b")) +
                 2 * str_count(tl, "ñ") + 2 * str_count(t, "[¿¡]"),
    Portuguese = sum(str_count(tl, "\\b(dos|das|não|são|uma|reúne|informações|domicílios)\\b")) +
                 str_count(tl, "ção|ões") + 2 * str_count(tl, "[ãõ]"),
    German     = sum(str_count(tl, "\\b(der|die|das|und|für|von|nicht)\\b")),
    Turkish    = sum(str_count(tl, "\\b(ve|bir|için|ile|bu)\\b")) + str_count(tl, "[ığş]"),
    Dutch      = sum(str_count(tl, "\\b(het|van|een|niet|voor)\\b")),
    Italian    = sum(str_count(tl, "\\b(della|che|per|con|sono|degli)\\b")),
    Polish     = str_count(tl, "[ąęłżź]") + sum(str_count(tl, "\\b(się|jest|oraz)\\b")),
    Indonesian = sum(str_count(tl, "\\b(yang|dan|untuk|dari|dengan)\\b"))
  )
  if (max(scores) < 3) return("Latin script (undetermined)")
  names(which.max(scores))
}

# ------------------------------------------------------------------------------
# PUBLICATION DATE GUESS (ISO date in URL/text, or "12 March 2025" style)
# ------------------------------------------------------------------------------
MONTHS_PAT <- paste0(
  "january|february|march|april|may|june|july|august|september|october|november|december|",
  "janvier|février|mars|avril|juin|juillet|août|septembre|octobre|novembre|décembre|",
  "enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre")

extract_pub_date <- function(pages, url, title) {
  txt <- str_sub(paste(c(coalesce(title, ""), head(pages, 2)), collapse = " "), 1, 30000)
  iso <- str_extract(paste(coalesce(url, ""), txt), "20\\d{2}-[01]\\d-[0-3]\\d")
  if (!is.na(iso)) return(iso)
  dmy <- str_extract(str_to_lower(txt),
                     paste0("[0-3]?\\d\\s+(", MONTHS_PAT, ")\\s+20\\d{2}"))
  if (!is.na(dmy)) return(dmy)
  str_extract(str_to_lower(txt), paste0("(", MONTHS_PAT, ")\\s+20\\d{2}"))
}

# ------------------------------------------------------------------------------
# NSO registry (country -> domain -> languages). Used for contact discovery and
# the processing overview. If NSO_Full_Registry.csv exists (the ~150-domain
# expansion file, columns country/domain/languages), it replaces this table.
# ------------------------------------------------------------------------------
nso_registry <- tribble(
  ~country,               ~domain,                  ~languages,
  "Kenya",                "knbs.or.ke",             "en",
  "Uganda",               "ubos.org",               "en",
  "Tanzania",             "nbs.go.tz",              "en,sw",
  "Ethiopia",             "statsethiopia.gov.et",   "en",
  "South Africa",         "statssa.gov.za",         "en",
  "Zimbabwe",             "zimstat.co.zw",          "en",
  "Zambia",               "zamstats.gov.zm",        "en",
  "Nigeria",              "nigerianstat.gov.ng",    "en",
  "Ghana",                "statsghana.gov.gh",      "en",
  "Senegal",              "ansd.sn",                "fr",
  "Jordan",               "dos.gov.jo",             "ar,en",
  "Lebanon",              "cas.gov.lb",             "ar,en,fr",
  "Iraq",                 "cosit.gov.iq",           "ar,en",
  "Türkiye",              "tuik.gov.tr",            "tr,en",
  "Bangladesh",           "bbs.gov.bd",             "en",
  "Pakistan",             "pbs.gov.pk",             "en",
  "Colombia",             "dane.gov.co",            "es",
  "Mexico",               "inegi.org.mx",           "es",
  "Sudan",                "cbs.gov.sd",             "ar,en",
  "South Sudan",          "nbs.gov.ss",             "en",
  "Somalia",              "nbs.gov.so",             "en",
  "DR Congo",             "ins-rdc.org",            "fr",
  "Chad",                 "inseed.td",              "fr",
  "Cameroon",             "ins-cameroun.cm",        "fr,en",
  "Mali",                 "instat-mali.org",        "fr",
  "Burkina Faso",         "insd.bf",                "fr",
  "Niger",                "stat-niger.org",         "fr",
  "Mozambique",           "ine.gov.mz",             "pt",
  "Rwanda",               "statistics.gov.rw",      "en",
  "Burundi",              "insbu.bi",               "fr",
  "Egypt",                "capmas.gov.eg",          "ar",
  "Morocco",              "hcp.ma",                 "fr,ar",
  "Tunisia",              "ins.tn",                 "fr,ar",
  "Côte d'Ivoire",        "ins.ci",                 "fr",
  "State of Palestine",   "pcbs.gov.ps",            "ar,en",
  "Afghanistan",          "nsia.gov.af",            "en",
  "Syria",                "cbssyr.sy",              "ar",
  "Armenia",              "armstat.am",             "en,ru",
  "Azerbaijan",           "stat.gov.az",            "en,ru",
  "Georgia",              "geostat.ge",             "en",
  "Ukraine",              "ukrstat.gov.ua",         "en,ru",
  "Moldova",              "statistica.md",          "en,ru",
  "Kazakhstan",           "stat.gov.kz",            "ru,en",
  "Kyrgyzstan",           "stat.kg",                "ru",
  "Nepal",                "nsonepal.gov.np",        "en",
  "Sri Lanka",            "statistics.gov.lk",      "en",
  "Philippines",          "psa.gov.ph",             "en",
  "Indonesia",            "bps.go.id",              "en",
  "Peru",                 "inei.gob.pe",            "es",
  "Ecuador",              "ecuadorencifras.gob.ec", "es"
)
if (file.exists("NSO_Full_Registry.csv")) {
  full_reg <- read_csv("NSO_Full_Registry.csv", show_col_types = FALSE)
  if (all(c("country", "domain") %in% names(full_reg))) {
    if (!"languages" %in% names(full_reg)) full_reg$languages <- "en"
    nso_registry <- full_reg %>% select(country, domain, languages)
    message(paste("Using NSO_Full_Registry.csv:", nrow(nso_registry), "domains"))
  }
}

# ------------------------------------------------------------------------------
# Load master reference
# ------------------------------------------------------------------------------
master_file <- list.files(pattern = "^GAIN_MASTER_REFERENCE_.*\\.csv$") %>%
  sort() %>% last()
if (is.na(master_file)) stop("No GAIN_MASTER_REFERENCE_*.csv found in this folder.")

master <- read_csv(master_file, show_col_types = FALSE) %>%
  filter(!is.na(url)) %>%
  distinct(url, .keep_all = TRUE) %>%
  mutate(across(any_of(c("title", "producer")), repair_mojibake))

if (is.finite(MAX_RECORDS)) master <- head(master, MAX_RECORDS)
message(paste("Enriching", nrow(master), "records from", master_file, "\n"))

overrides <- if (file.exists(OVERRIDE_FILE)) {
  read_csv(OVERRIDE_FILE, show_col_types = FALSE)
} else {
  tibble(url = character(), override_relevant = logical(), note = character())
}

# ------------------------------------------------------------------------------
# Multilingual term patterns (en/fr/es/pt/ar/ru/uk/tr/sw)
# Arabic terms per the GAIN review specification.
# ------------------------------------------------------------------------------
PAT <- list(
  refugee = paste0(
    # \basil[eo]\b is bounded so 'asile' no longer matches inside 'brasileiro'
    "refugee[s]?|asylum[- ]seeker[s]?|asylum|réfugié|refugiad[oa]s?|\\basile\\b|\\basilo\\b|",
    "لاجئ|لاجئون|اللاجئين|طالب لجوء|طالبي اللجوء|طالبو اللجوء|اللجوء|الحماية الدولية|",
    "беженц|беженец|біженц|притулок|mülteci|sığınmacı|geçici koruma|wakimbizi|",
    # group 2-4 site languages
    "flüchtling|geflüchtete|asylbewerber|vluchteling|asielzoeker|rifugiat|richiedenti asilo|",
    "uchodź|menekült|menedékkér|uprchlí|utečen|πρόσφυγ|άσυλο|refugiaț|убежище|бежанц|",
    "izbeglic|izbjeglic|refugjat|бегалц|begunc|پناهند|پناهجو|פליטים|מבקשי מקלט|",
    "flykting|flyktning|flygtning|asylsøk|asylansøg|asylsökande|pakolai|turvapaikan|",
    "pagulas|varjupaiga|bēgļ|patvēruma|pabėgėl|prieglobsč|",
    "难民|寻求庇护|難民|난민|ผู้ลี้ภัย|tị nạn|pengungsi|pencari suaka|pelarian|дүрвэгс|",
    # recall expansion 12 Jun 2026
    "refugee camp|camp de réfugiés|campamento de refugiados|persons of concern|",
    "asylum application|demande[s]? d'asile|solicitudes de asilo|طلبات اللجوء|مخيمات اللاجئين"
  ),
  idp = paste0(
    # (?![:=0-9]) stops \bidp\b matching URL fragments like 'idp:1254735976595'
    "internally displaced|\\bidps?\\b(?![:=0-9])|displacement|déplacé|desplazad[oa]s?|deslocad[oa]s?|",
    "نازح|نازحون|النازحين|نازح داخلي|نازحون داخلياً|النزوح الداخلي|المشردون داخلياً|",
    "внутренне перемещ|вынужденн\\S* перемещ|вынужденные переселенцы|\\bвпл\\b|",
    "внутрішньо переміщені|внутрішнє переміщення|\\bвпо\\b|yerinden edilm|",
    # group 2-4 site languages
    "binnenvertrieben|vertriebene|ontheemd|sfollat|przesiedl|wysiedl|belső menekült|",
    "vysídlen|εκτοπισμέν|strămutat|разселени|raseljen|zhvendosur|раселени|razseljen|",
    "آوارگ|بیجاشدگ|עקורים|internflykting|fördrivna|fordrevne|maan sisäiset pakolaiset|",
    "ümberasustatud|iekšzemē pārvietot|viduje perkelt|",
    "流离失所|国内避難民|避難民|실향민|ผู้พลัดถิ่น|di tản|pengungsi internal|",
    "pemindahan dalaman|дотоодын дүрвэгс|",
    # recall expansion 12 Jun 2026
    "forced migration|migration forcée|migración forzada|migração forçada|",
    "вынужденная миграция|вимушена міграція|الهجرة القسرية|zorunlu göç|",
    "returnees?\\b|retourné|retornad|возвращенц|العائدين|",
    "displaced (persons?|households?|populations?)"
  ),
  stateless = paste0(
    "stateless|statelessness|nationality status|undetermined nationality|",
    "without nationality|apatride|apátrida|",
    "عديم الجنسية|عديمو الجنسية|انعدام الجنسية|بلا جنسية|",
    "الجنسية غير محددة|الجنسية غير معروفة|حالة الجنسية|",
    "апатрид|без гражданства|без громадянства|vatansız|",
    # group 2-4 site languages
    "staatenlos|staatloos|staatloz|apolid|bezpaństwow|hontalan|",
    "bez státní příslušnosti|bez štátnej príslušnosti|ανιθαγεν|apatriz|без гражданство|",
    "bez državljanstva|pa shtetësi|без државјанство|brez državljanstva|",
    "بدون تابعیت|بی تابعیت|חסרי אזרחות|statslös|statsløs|kansalaisuudet|kodakondsuseta|",
    "bezvalstniek|be pilietybės|无国籍|無国籍|무국적|ไร้สัญชาติ|không quốc tịch|",
    "tanpa kewarganegaraan|харьяалалгүй|",
    # recall expansion 12 Jun 2026
    "citizenship status|without citizenship|undetermined citizenship|legal identity"
  ),
  egriss = paste0(
    "egriss|\\birrs\\b|\\biross\\b|",
    # IRIS (International Recommendations on IDP Statistics) - anchored so it does
    # NOT match the eye/flower/messaging products: must co-occur with a
    # displacement/statistics/IDP term within 40 chars on either side.
    "\\biris\\b(?=.{0,40}(displac|statistic|\\bidp))|",
    "(?<=(displac|statistic|\\bidp).{0,40})\\biris\\b|",
    "international recommendations on (refugee|internally displaced|idp|stateless)|",
    "recommandations internationales sur|recomendaciones internacionales sobre"
  ),
  # statistical activity / source-type signal (incl. Arabic per specification)
  statactivity = paste0(
    "survey|census|questionnaire|microdata|administrative data|civil registration|",
    "statistical (report|register|yearbook|bulletin)|sampling frame|metadata|methodolog|",
    "enquête|recensement|encuesta|censo|levantamento|",
    "مسح|تعداد|استبيان|إحصاءات|تقرير إحصائي|بيانات وصفية|بيانات إدارية|",
    "سجل|سجلات|بيانات دقيقة|منهجية|",
    "перепис|перепись|обстеження|обследование|anket|sayım|nüfus sayımı|",
    # group 2-4 site languages (census/survey terms)
    "zensus|volkszählung|erhebung|mikrozensus|censimento|indagine|volkstelling|",
    "spis powszechny|népszámlálás|sčítání|sčítanie|απογραφή|recensământ|преброяване|",
    "popis stanovništva|regjistrimi|попис|tổng điều tra|",
    "普查|国勢調査|총조사|สำมะโน|sensus|survei|banci|тооллого"
  ),
  # generic migration terms (alone these cap the score at 30)
  migration = "migration|migrant[s]?|migración|миграц|міграц|مهاجر|الهجرة|göç\\b",
  # INCLUSION SIGNAL - language that proves a displaced group was actually
  # captured/disaggregated, not just that an instrument exists. This is the
  # distinction GAIN cares about: a census disaggregating by displacement status
  # is evidence; a census merely existing is not.
  inclusion = paste0(
    "disaggregat|désagrég|desagregad|",
    "displacement status|statut de déplacement|condición de desplazamiento|",
    "refugee module|idp module|migration module|module (réfugié|déplacé|migration)|",
    "módulo (de )?(refugiad|desplazad|migración)|",
    "identifier variable|variable d'identification|variable de identificación|",
    "additional question on|question additionnelle|pregunta adicional sobre|",
    "oversampl|suréchantillon|sobremuestre|",
    "host communit|communauté[s]? d'accueil|comunidad(es)? de acogida|host[- ]country|",
    "refugee[- ]hosting|zone d'accueil des réfugiés|área[s]? de acogida|",
    "returnee[s]?|retourné|retornad|",
    "by (refugee|idp|displacement|migrant|nationality) status|",
    "broken down by (refugee|idp|displacement|migrant|nationality)"
  ),
  # BIRTH REGISTRATION - a corroborating signal for statelessness statistics
  # (IROSS links birth registration coverage to statelessness risk/prevention).
  # NEVER used as a standalone/search signal: on its own it is extremely common,
  # generic child-health/vital-statistics language (MICS/DHS indicators, civil
  # registration coverage rates) with no connection to statelessness in most
  # uses - exactly the bare-broad-term pattern that caused false-positive
  # floods before. Scored ONLY via co-occurrence with a stateless/nationality
  # term nearby (see birth_reg_stateless_cooccur below), never added by itself.
  birthreg = paste0(
    "birth registration|registration of birth|unregistered birth|",
    "enregistrement des naissances|naissances non enregistrées|",
    "registro de nacimiento|inscripción de nacimientos|",
    "تسجيل المواليد|",
    "регистрация рождения|реєстрація народжен"
  )
)

# 'asilo'/'asile' in Portuguese/Spanish also mean a care/retirement home and
# appear in site navigation menus (e.g. IBGE "Brasil em Síntese" pages). They
# count as refugee mentions for transparency, but a record whose ONLY refugee
# signal is this ambiguous word - with no stronger term and not in the title -
# is treated as a weak lead, not an outreach candidate.
PAT_AMBIG_ASILO <- "\\basil[eo]\\b"

# Ukrainian/Russian enterprise & industry false-positive signals
# ("pidpryiemstv"/"підприємств" pages match the substring 'idp' in URLs)
FP_ENTERPRISE_PAT <- paste0(
  "підприєм|пiдприєм|предприят|промислов|промышлен|виробництв|производств|",
  "\\bpidpr|enterprise statistics|industrial production"
)

# spam / unrelated commercial content -> Exclude
SPAM_PAT <- "casino|bett?ing|gambl|jackpot|slots?\\b|poker|\\bbonus(es)?\\b|viagra|porn|escort|\\bhack(ed|ing|er)?\\b"

TITLE_WORDS <- "director|directeur|directora|deputy|head of|chief|commissioner|statistician|coordinator|manager|secretary general|directeur général|director general"

# ------------------------------------------------------------------------------
# Arabic glossary: term -> neutral English gloss (used for the working summary)
# ------------------------------------------------------------------------------
AR_GLOSSARY <- c(
  "لاجئ"               = "refugee",
  "لاجئون"             = "refugees",
  "اللاجئين"           = "refugees",
  "طالب لجوء"          = "asylum seeker",
  "طالبي اللجوء"       = "asylum seekers",
  "طالبو اللجوء"       = "asylum seekers",
  "اللجوء"             = "asylum",
  "الحماية الدولية"    = "international protection",
  "نازح"               = "internally displaced person",
  "نازحون"             = "internally displaced persons",
  "النازحين"           = "internally displaced persons",
  "نازح داخلي"         = "internally displaced person",
  "نازحون داخلياً"     = "internally displaced persons",
  "النزوح الداخلي"     = "internal displacement",
  "المشردون داخلياً"   = "internally displaced persons",
  "عديم الجنسية"       = "stateless person",
  "عديمو الجنسية"      = "stateless persons",
  "انعدام الجنسية"     = "statelessness",
  "بلا جنسية"          = "without nationality",
  "الجنسية غير محددة"  = "undetermined nationality",
  "الجنسية غير معروفة" = "unknown nationality",
  "حالة الجنسية"       = "nationality status",
  "مسح"                = "survey",
  "تعداد"              = "census",
  "استبيان"            = "questionnaire",
  "إحصاءات"            = "statistics",
  "تقرير إحصائي"       = "statistical report",
  "بيانات وصفية"       = "metadata",
  "بيانات إدارية"      = "administrative data",
  "سجل"                = "register",
  "سجلات"              = "registers",
  "بيانات دقيقة"       = "microdata",
  "منهجية"             = "methodology"
)

has_arabic <- function(x) str_detect(coalesce(x, ""), "\\p{Arabic}")

# Automated term gloss for Arabic records: neutral, clearly NOT a translation
make_arabic_gloss <- function(full_text, populations) {
  if (!has_arabic(full_text)) return(NA_character_)
  found <- AR_GLOSSARY[map_lgl(names(AR_GLOSSARY),
                               ~ str_detect(full_text, fixed(.x)))]
  terms_txt <- if (length(found) > 0) {
    paste0(found, " (", names(found), ")", collapse = "; ")
  } else "none from the review glossary"
  paste0(
    "Arabic-language record - automated term gloss, not a full translation. ",
    "Detected terms: ", terms_txt, ". ",
    "Evidence suggests this may be relevant to GAIN",
    if (!is.na(populations) && populations != "")
      paste0(" (possible population groups: ", populations, ")") else "",
    ". Possible candidate only - requires manual review by an Arabic reader ",
    "and confirmation from the NSO or relevant respondent."
  )
}

extract_arabic_excerpt <- function(pages, contexts) {
  ar_ctx <- contexts[!is.na(contexts) & map_lgl(contexts, has_arabic)]
  if (length(ar_ctx) > 0) return(str_sub(ar_ctx[1], 1, 400))
  txt <- paste(pages, collapse = " ")
  m <- str_extract(txt, "[\\p{Arabic}][\\p{Arabic}\\s[:punct:]0-9]{40,300}")
  if (is.na(m)) NA_character_ else str_squish(m)
}

# Original-language excerpt for ANY non-English record (Arabic, Ukrainian,
# Portuguese, ...) so the reviewer always sees what was actually matched
extract_original_excerpt <- function(pages, contexts, lang) {
  if (is.na(lang) || lang %in% c("English", "Latin script (undetermined)"))
    return(NA_character_)
  if (lang == "Arabic") return(extract_arabic_excerpt(pages, contexts))
  ctx <- contexts[!is.na(contexts)]
  if (length(ctx) > 0) return(str_sub(ctx[1], 1, 400))
  txt <- str_squish(paste(pages, collapse = " "))
  if (nchar(txt) == 0) NA_character_ else str_sub(txt, 1, 300)
}

# English working summary for ANY non-English record. Arabic gets the
# term-level gloss; other languages get a neutral counts-based summary.
# Always cautious: a possible candidate requiring manual review.
make_working_summary <- function(full_text, populations, lang,
                                 m_ref, m_idp, m_sta, m_egr, m_act) {
  if (is.na(lang) || lang %in% c("English", "Latin script (undetermined)"))
    return(NA_character_)
  if (lang == "Arabic") return(make_arabic_gloss(full_text, populations))
  counts <- c(
    if (m_ref > 0) paste0(m_ref, " refugee/asylum term mention(s)"),
    if (m_idp > 0) paste0(m_idp, " internal-displacement term mention(s)"),
    if (m_sta > 0) paste0(m_sta, " statelessness/nationality-status mention(s)"),
    if (m_egr > 0) paste0(m_egr, " EGRISS/international-recommendations mention(s)"),
    if (m_act > 0) paste0(m_act, " census/survey/statistical-activity term(s)")
  )
  paste0(lang, "-language record - automated screening summary, not a translation. ",
         if (length(counts) > 0)
           paste0("Detected: ", paste(counts, collapse = ", "), ". ")
         else "No GAIN-relevant terms were detected in the extracted text. ",
         "Possible candidate only - requires manual review by a ", lang,
         " reader and confirmation from the NSO or relevant respondent.")
}

# ------------------------------------------------------------------------------
# Fetch with cache -> list(doc_type, pages = chr vector of page/section texts)
# ------------------------------------------------------------------------------
fetch_document <- function(url) {
  key <- rlang::hash(url)
  cache_rds <- file.path(CACHE_DIR, paste0(key, ".rds"))
  # `fetched` tells the caller whether a REAL network call happened, so the
  # politeness sleep fires only then (same pattern as the merge's fetch_title).
  # Previously the loop slept 0.8s per record unconditionally - on a fully
  # cached 1,224-record re-run that was ~16 minutes of pure sleep. The flag is
  # added AFTER saveRDS, so the on-disk cache format is unchanged.
  if (file.exists(cache_rds)) {
    out <- readRDS(cache_rds)
    out$fetched <- FALSE
    return(out)
  }

  out <- tryCatch({
    resp <- request(url) %>%
      req_user_agent("EGRISS-GAIN-research (statistics inventory; egriss.org)") %>%
      req_timeout(40) %>%
      req_perform()

    ctype <- resp_header(resp, "content-type") %||% ""
    body  <- resp_body_raw(resp)

    if (str_detect(str_to_lower(url), "\\.pdf($|\\?)") ||
        str_detect(str_to_lower(ctype), "pdf")) {
      if (length(body) / 1e6 > MAX_PDF_MB) {
        list(doc_type = "PDF (too large, skipped)", pages = character(0))
      } else {
        tmp <- tempfile(fileext = ".pdf")
        writeBin(body, tmp)
        pages <- tryCatch(pdf_text(tmp), error = function(e) character(0))
        unlink(tmp)
        if (length(pages) == 0 || all(str_squish(paste(pages, collapse = "")) == "")) {
          list(doc_type = "PDF (no text layer)", pages = character(0))
        } else {
          list(doc_type = "PDF", pages = pages)
        }
      }
    } else {
      html <- read_html(rawToChar(body))
      xml2::xml_remove(html_elements(html, "script, style, nav, footer, noscript"))
      txt <- html %>% html_element("body") %>% html_text2()
      list(doc_type = "webpage", pages = c(txt %||% ""))
    }
  }, error = function(e) list(doc_type = "unreachable", pages = character(0)))

  saveRDS(out, cache_rds)
  out$fetched <- TRUE     # real network call - caller should pause politely
  out
}

# ------------------------------------------------------------------------------
# PUBLIC CONTACT DISCOVERY on NSO websites
# Only public, professional contact pages are inspected; nothing private is
# scraped or inferred. Results cached per domain in evidence_cache/.
# ------------------------------------------------------------------------------
CONTACT_PATHS <- c(
  "contact", "contact-us", "contactus", "contacts", "about", "about-us",
  "staff", "directory", "departments", "structure", "organisation",
  "statistics-information-service", "microdata", "data-request",
  "survey-department", "census", "population-statistics", "migration-statistics",
  "international-cooperation", "dissemination", "public-relations", "press"
)

EMAIL_RX <- "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
SPAM_EMAIL_RX <- paste0(SPAM_PAT, "|example\\.|sentry|wixpress|noreply|no-reply|\\.png$|\\.jpg$")

discover_domain_contacts <- function(domain) {
  cache <- file.path(CACHE_DIR,
                     paste0("contacts_", str_replace_all(domain, "[^a-z0-9]", "_"), ".rds"))
  if (file.exists(cache)) return(readRDS(cache))

  urls <- unique(c(
    paste0("https://", domain, "/"),
    paste0("https://", domain, "/", CONTACT_PATHS),
    paste0("https://www.", domain, "/", head(CONTACT_PATHS, 6))
  ))

  found <- list(); ok_pages <- 0; tried <- 0
  for (u in urls) {
    if (ok_pages >= 6 || tried >= 16) break
    tried <- tried + 1
    txt <- tryCatch({
      resp <- request(u) %>%
        req_user_agent("EGRISS-GAIN-research (statistics inventory; egriss.org)") %>%
        req_timeout(15) %>% req_perform()
      if (!str_detect(resp_header(resp, "content-type") %||% "", "html")) {
        NA_character_
      } else {
        html <- read_html(resp_body_string(resp))
        mailtos <- html_attr(html_elements(html, "a[href^='mailto:']"), "href")
        paste(html_text2(html_element(html, "body")) %||% "",
              paste(mailtos, collapse = " "))
      }
    }, error = function(e) NA_character_)
    Sys.sleep(0.6)
    if (is.na(txt)) next
    ok_pages <- ok_pages + 1
    emails <- unique(str_extract_all(txt, EMAIL_RX)[[1]])
    emails <- emails[!str_detect(str_to_lower(emails), SPAM_EMAIL_RX)]
    if (length(emails) > 0) {
      found[[u]] <- tibble(email = emails, found_on = u)
    }
  }
  out <- bind_rows(found)
  if (nrow(out) > 0) out <- distinct(out, email, .keep_all = TRUE)
  saveRDS(out, cache)
  out
}

# Contact-type classification (per GAIN outreach specification)
GENERIC_LOCALS <- "^(info|contact|contacts|stat|stats|statistics|data|press|media|pr|communication|webmaster|admin|office|secretariat|library|publication|diffusion|dissemination|general|enquir|inquir|helpdesk|service)"
DEPT_LOCALS    <- "census|survey|population|demograph|migration|social|methodol|coop|international|microdata|dissemination|nada"

classify_contact <- function(email, nso_domain) {
  e     <- str_to_lower(coalesce(email, ""))
  if (e == "" || str_detect(e, SPAM_EMAIL_RX)) return("unknown")
  edom  <- str_extract(e, "(?<=@)[a-z0-9.-]+")
  local <- str_extract(e, "^[^@]+")
  base  <- str_remove(str_to_lower(coalesce(nso_domain, "zzz-none")), "^www\\.")

  if (!is.na(edom) && str_detect(edom, fixed(base))) {
    if (str_detect(local, DEPT_LOCALS))         return("NSO department contact")
    if (str_detect(local, GENERIC_LOCALS))      return("NSO general institutional contact")
    return("NSO named staff contact")
  }
  if (str_detect(edom %||% "", "unhcr|worldbank|iom\\.int|wfp\\.org|unicef|undp|un\\.org|ilo\\.org|unfpa"))
    return("international organization contact")
  if (str_detect(edom %||% "", "afristat|paris21|uneca|escwa|asean|cepal|eclac|afdb|ssco"))
    return("regional organization contact")
  if (str_detect(edom %||% "", "ihsn|datafirst|dhsprogram|surveys\\.worldbank|microdata"))
    return("data catalogue contact")
  if (str_detect(edom %||% "", "\\.gov\\.|\\.go\\.|\\.gouv\\.|\\.gob\\."))
    return("national statistical system actor")
  "partner contact"
}

# preference order when picking an OUTREACH contact (NSO first, per spec)
CONTACT_RANK <- c(
  "NSO department contact"            = 1,
  "NSO general institutional contact" = 2,
  "NSO named staff contact"           = 3,   # usable, but must be validated
  "national statistical system actor" = 4,
  "international organization contact" = 5,
  "regional organization contact"     = 6,
  "partner contact"                   = 7,
  "data catalogue contact"            = 8,
  "publication metadata contact"      = 9,
  "unknown"                           = 10
)

# ------------------------------------------------------------------------------
# Extraction helpers
# ------------------------------------------------------------------------------
# Strip web navigation noise before keyword matching: query-string fragments
# like 'mga=cid:1254...~idp:1254...~secc:...' otherwise inflate IDP/keyword counts
# (Spain INE internal links), and long digit runs / URL params are never evidence.
strip_web_noise <- function(txt) {
  txt %>%
    str_replace_all("mga=[^\\s]+", " ") %>%               # INE mga link blocks
    str_replace_all("[A-Za-z_]+[:=]\\d{3,}", " ") %>%      # param:bignum / param=bignum
    str_replace_all("https?://[^\\s]+", " ") %>%           # raw URLs in text
    str_replace_all("[?&][A-Za-z_]+=", " ")                # query params
}

count_mentions <- function(pages, pattern) {
  sum(str_count(strip_web_noise(str_to_lower(pages)), pattern))
}

# Proximity / co-occurrence: does a match of patA fall within `window` characters
# of a match of patB anywhere in the (de-noised) text? Rewards a population term
# sitting NEAR a statistical-activity term, vs the two being on opposite ends of
# a busy homepage (refugee in a news ticker, census in the footer).
cooccurs_within <- function(pages, patA, patB, window = 250) {
  t <- strip_web_noise(str_to_lower(paste(pages, collapse = " ")))
  a <- str_locate_all(t, patA)[[1]]; if (nrow(a) == 0) return(FALSE)
  b <- str_locate_all(t, patB)[[1]]; if (nrow(b) == 0) return(FALSE)
  am <- a[, 1]; bm <- b[, 1]
  # cap the cross-product so a huge page can't blow up memory
  if (length(am) > 400) am <- am[seq_len(400)]
  if (length(bm) > 400) bm <- bm[seq_len(400)]
  any(outer(am, bm, function(x, y) abs(x - y) <= window))
}

extract_contexts <- function(pages, pattern, is_pdf, max_snips = MAX_SNIPS) {
  pages <- strip_web_noise(pages)   # match/display on de-noised text
  snips <- character(0)
  for (p in seq_along(pages)) {
    locs <- str_locate_all(str_to_lower(pages[p]), pattern)[[1]]
    if (nrow(locs) == 0) next
    for (k in seq_len(min(nrow(locs), max_snips))) {
      s <- max(1, locs[k, 1] - SNIPPET_CHARS / 2)
      e <- min(nchar(pages[p]), locs[k, 2] + SNIPPET_CHARS / 2)
      snip <- str_squish(str_sub(pages[p], s, e))
      tag <- if (is_pdf) paste0("p.", p, ": ") else ""
      snips <- c(snips, paste0(tag, "...", snip, "..."))
      if (length(snips) >= max_snips) return(paste(snips, collapse = " | "))
    }
  }
  if (length(snips) == 0) NA_character_ else paste(snips, collapse = " | ")
}

extract_summary <- function(pages) {
  txt <- paste(head(pages, 2), collapse = " ")
  paras <- str_split(txt, "\n\n|\\.\\s{2,}")[[1]] %>% str_squish()
  good <- paras[nchar(paras) > 80 & !str_detect(paras, "^(\\{|<|function|var )")]
  if (length(good) == 0) return(NA_character_)
  str_sub(good[1], 1, 400)
}

extract_emails <- function(pages) {
  e <- str_extract_all(paste(pages, collapse = " "), EMAIL_RX)[[1]] %>%
    unique() %>%
    discard(~ str_detect(str_to_lower(.x), SPAM_EMAIL_RX))
  if (length(e) == 0) NA_character_ else paste(head(e, 6), collapse = "; ")
}

extract_name_titles <- function(pages) {
  txt <- paste(pages, collapse = "\n")
  hits <- str_extract_all(txt, regex(paste0(
    "((Mr|Mrs|Ms|Dr|Prof)\\.?\\s+)?[A-Z][a-zà-ÿ]+(\\s+[A-Z][a-zà-ÿ]+){1,2}[,\\s\\-]{1,3}(", TITLE_WORDS, ")[^.\\n]{0,50}",
    "|(", TITLE_WORDS, ")[^.\\n]{0,40}[:\\-]\\s*[A-Z][a-zà-ÿ]+(\\s+[A-Z][a-zà-ÿ]+){1,2}"
  ), ignore_case = FALSE))[[1]] %>%
    str_squish() %>% unique()
  hits <- hits[nchar(hits) > 15 & nchar(hits) < 120]
  if (length(hits) == 0) NA_character_ else paste(head(hits, 5), collapse = " | ")
}

# ------------------------------------------------------------------------------
# STEP 1: contact discovery for every country present in the master file
# ------------------------------------------------------------------------------
message("Step 1: public contact discovery on NSO websites...")

target_domains <- nso_registry %>% filter(country %in% unique(master$country))

contact_pool <- map_dfr(seq_len(nrow(target_domains)), function(i) {
  row <- target_domains[i, ]
  message(sprintf("  [%d/%d] %s (%s)", i, nrow(target_domains), row$country, row$domain))
  found <- discover_domain_contacts(row$domain)
  if (nrow(found) == 0) return(tibble())
  found %>% mutate(
    country = row$country,
    contact_type = map_chr(email, classify_contact, nso_domain = row$domain),
    contact_source = "NSO contact page crawl"
  )
})
if (nrow(contact_pool) == 0) {
  contact_pool <- tibble(country = character(), email = character(),
                         found_on = character(), contact_type = character(),
                         contact_source = character())
}
message(paste("  Contacts discovered on NSO pages:", nrow(contact_pool)))

# ------------------------------------------------------------------------------
# STEP 2: per-record document enrichment (mentions, contexts, Arabic gloss)
# ------------------------------------------------------------------------------
message("\nStep 2: document enrichment...")

enriched <- map_dfr(seq_len(nrow(master)), function(i) {
  row <- master[i, ]
  message(sprintf("[%d/%d] %s | %s", i, nrow(master), row$country,
                  str_sub(coalesce(row$title, row$url), 1, 60)))

  # Structured-API records (Layer 1.5, e.g. Eurostat): their value is in the
  # metadata (official title names the population, producer is the NSO). The
  # underlying URL is a JS data-browser table with no useful prose, so we do NOT
  # scrape it - we synthesize the document text from the title/producer/
  # populations so the mention + co-occurrence logic registers them correctly.
  is_struct <- str_starts(coalesce(row$source_layer, ""), "STRUCT:")
  if (is_struct) {
    doc <- list(doc_type = "structured (API)",
                pages = c(paste(row$title, "-", row$populations,
                                "official statistics from", row$producer)))
  } else {
    doc <- fetch_document(row$url)
  }
  is_pdf <- str_starts(doc$doc_type, "PDF")
  if (!is_struct && isTRUE(doc$fetched)) Sys.sleep(0.8)   # pause ONLY on real network fetches

  m_ref <- count_mentions(doc$pages, PAT$refugee)
  m_idp <- count_mentions(doc$pages, PAT$idp)
  m_sta <- count_mentions(doc$pages, PAT$stateless)
  m_egr <- count_mentions(doc$pages, PAT$egriss)
  m_act <- count_mentions(doc$pages, PAT$statactivity)
  m_mig <- count_mentions(doc$pages, PAT$migration)
  m_fp  <- count_mentions(doc$pages, FP_ENTERPRISE_PAT)
  m_inc <- count_mentions(doc$pages, PAT$inclusion)   # displacement-inclusion language
  # ambiguous 'asilo'/'asile' occurrences; strong refugee count excludes them
  m_asilo    <- count_mentions(doc$pages, PAT_AMBIG_ASILO)
  m_ref_strong <- max(0, m_ref - m_asilo)
  # co-occurrence: a population term sitting NEAR a statistical-activity term
  pop_pat <- paste(PAT$refugee, PAT$idp, PAT$stateless, sep = "|")
  cooccur_pop_stat <- cooccurs_within(doc$pages, pop_pat, PAT$statactivity)
  # birth registration: corroborating signal only - scored ONLY when it
  # co-occurs with an actual stateless/nationality term nearby (never alone,
  # since bare "birth registration" is common generic vital-statistics language)
  m_birthreg <- count_mentions(doc$pages, PAT$birthreg)
  cooccur_birthreg_stateless <- cooccurs_within(doc$pages, PAT$birthreg, PAT$stateless)

  title_l <- str_to_lower(coalesce(row$title, ""))
  in_title <- str_detect(title_l, PAT$refugee) | str_detect(title_l, PAT$idp) |
              str_detect(title_l, PAT$stateless)
  in_title_inclusion <- str_detect(title_l, PAT$inclusion)

  ctx_ref <- extract_contexts(doc$pages, PAT$refugee, is_pdf)
  ctx_idp <- extract_contexts(doc$pages, PAT$idp, is_pdf)
  ctx_sta <- extract_contexts(doc$pages, PAT$stateless, is_pdf)
  ctx_egr <- extract_contexts(doc$pages, PAT$egriss, is_pdf)

  full_text <- paste(c(row$title, head(doc$pages, 5)), collapse = " ")
  lang <- detect_language(full_text)

  tibble(
    row,
    doc_type = doc$doc_type,
    n_pages  = length(doc$pages),
    text_extracted     = length(doc$pages) > 0,
    mentions_refugee   = m_ref,
    mentions_refugee_strong = m_ref_strong,
    mentions_idp       = m_idp,
    mentions_stateless = m_sta,
    mentions_egriss    = m_egr,
    mentions_statactivity = m_act,
    mentions_migration = m_mig,
    mentions_inclusion = m_inc,
    mentions_enterprise_fp = m_fp,
    in_title_match     = in_title,
    in_title_inclusion = in_title_inclusion,
    cooccur_pop_stat   = cooccur_pop_stat,
    mentions_birth_reg = m_birthreg,
    cooccur_birthreg_stateless = cooccur_birthreg_stateless,
    context_refugee    = ctx_ref,
    context_idp        = ctx_idp,
    context_stateless  = ctx_sta,
    context_egriss     = ctx_egr,
    extract_summary    = extract_summary(doc$pages),
    emails_found       = extract_emails(doc$pages),   # evidence-source contacts
    staff_candidates   = extract_name_titles(doc$pages),
    contains_arabic    = has_arabic(full_text),
    doc_language       = lang,
    pub_date_guess     = extract_pub_date(doc$pages, row$url, row$title),
    original_excerpt   = extract_original_excerpt(doc$pages,
                           c(ctx_ref, ctx_idp, ctx_sta, ctx_egr), lang),
    english_working_summary = make_working_summary(full_text, row$populations,
                                lang, m_ref, m_idp, m_sta, m_egr, m_act)
  )
})

# ------------------------------------------------------------------------------
# STEP 3: false positives, score caps, lead type, outreach category (vectorized)
# ------------------------------------------------------------------------------
message("\nStep 3: scoring, lead type and outreach classification...")

PARTNER_PRODUCER_PAT <- "unhcr|world bank|\\biom\\b|wfp|unicef|\\bdhs\\b|jips|datafirst|ihsn|undp|reach|idmc|\\bilo\\b"
NSO_PRODUCER_PAT     <- "statisti|census bureau|bureau of stat|institut.*stat|nso\\b|dane\\b|inegi|pcbs|knbs|ubos|zimstat|instat|capmas|\\bbps\\b|\\bbbs\\b|\\bpsa\\b|\\bine\\b|\\bins\\b|geostat|armstat|destatis|istat|\\bcbs\\b|stats? ?(sa|south africa)|zamstats|zamstat"

# --- Evidence nature: official statistics vs humanitarian/operational data ----
# GAIN is about NSO/official statistics. UNHCR/WFP/IOM operational M&E surveys
# (RMS, protection monitoring, MSNA, PDM, vulnerability assessments...) are
# humanitarian data, NOT official statistics, unless an NSO is involved.
HUMANITARIAN_PRODUCER_PAT <- paste0(
  "unhcr|refugee agency|haut commissariat|\\bwfp\\b|world food|\\biom\\b|",
  "migration agency|unicef|\\breach\\b|impact initiative|\\bnrc\\b|",
  "norwegian refugee|danish refugee|\\bdrc\\b|\\bjips\\b|\\bacted\\b|",
  "save the children|mercy corps|ipsos|samuel hall|ground truth")
HUMANITARIAN_TITLE_PAT <- paste0(
  "results monitoring survey|\\brms\\b|protection monitoring|protection profiling|",
  "multi-?sector(al)? needs|\\bmsna\\b|post-?distribution monitoring|\\bpdm\\b|",
  "vulnerability assessment|socio-?economic profiling|community-?based protection|",
  "rapid household assessment|cash-?based intervention|standardi[sz]ed expanded nutrition|",
  "\\bsens\\b|mixed movements|intentions (survey|and perspectives)|",
  "joint post.?distribution|needs assessment|displacement tracking|\\bdtm\\b|",
  "high frequency phone|listening to|monitoring survey")
# titles that are clearly official statistical instruments (often NSO-led even
# when a partner co-funds: census, LFS, DHS, MICS, HBS, LSMS, national surveys)
OFFICIAL_TITLE_PAT <- paste0(
  "census|population and housing|labour force|labor force|\\blfs\\b|",
  "demographic and health|\\bdhs\\b|multiple indicator cluster|\\bmics\\b|",
  "indicadores múltiplos|indicateurs multiples|household budget|",
  "living standards|\\blsms\\b|integrated household|socio-?economic survey|",
  "statistical yearbook|\\bhies\\b|national (household |)survey|administrative (data|records)")

enriched <- enriched %>%
  mutate(
    core_mentions = mentions_refugee + mentions_idp + mentions_stateless,
    # "strong" core ignores the ambiguous PT/ES 'asilo' word (care home vs asylum)
    core_strong   = mentions_refugee_strong + mentions_idp + mentions_stateless,
    # weak lead: only evidence is the ambiguous 'asilo'/'asile' in body text,
    # nothing stronger and not in the title -> not an outreach candidate
    ambiguous_asilo_only = core_strong == 0 & mentions_refugee > 0 &
      !in_title_match & mentions_egriss == 0,
    # INCLUSION signal: the language that proves a displaced group was actually
    # captured/disaggregated. For GAIN this is the deciding distinction.
    has_inclusion = mentions_inclusion > 0 | in_title_inclusion,
    # A-tier bar: a displacement term in the TITLE (dedicated instrument), OR
    # 2+ mentions PLUS evidence the population is actually included/related
    # (inclusion language, or a population term sitting near a stat-activity term).
    # "A census disaggregating by displacement status is evidence; a census
    # merely existing is not." Stat-activity must be present either way.
    strong_candidate = mentions_statactivity > 0 & (
        in_title_match
        | (core_strong >= 2 & (has_inclusion | cooccur_pop_stat))
      ),
    # NSO-website signal EXCLUDES global partner catalogues (MICS/DHS sites) -
    # those are partner-led, never country-led, even though crawled via L3
    is_partner_site = str_detect(str_to_lower(coalesce(producer, "")),
                                 "mics\\.unicef|dhsprogram") |
                      str_starts(coalesce(country, ""), fixed("Global (")),
    nso_led_site  = (str_starts(coalesce(source_layer, ""), "L2:") |
                     str_starts(coalesce(source_layer, ""), "L3:")) &
                    !is_partner_site,
    l3_only       = str_starts(coalesce(source_layer, ""), "L3:"),
    url_title_l   = str_to_lower(paste(coalesce(url, ""), coalesce(title, ""))),

    # spam / unrelated commercial -> Exclude
    spam_flag = str_detect(url_title_l, SPAM_PAT),

    # UA/RU IDP false positive: 'idp' only as URL fragment (e.g. pidpryiemstv),
    # no valid displacement term in extracted text, enterprise terms present
    idp_url_fragment_only = str_detect(url_title_l, "idp") &
      !str_detect(url_title_l, "(^|[^a-z])idps?([^a-z]|$)"),
    idp_false_positive = text_extracted & mentions_idp == 0 &
      str_detect(str_to_lower(coalesce(populations, "")), "idp") &
      (idp_url_fragment_only | mentions_enterprise_fp > 0),
    false_positive_flag = idp_false_positive |
      (text_extracted & core_mentions == 0 & mentions_enterprise_fp > 0 &
         coalesce(populations, "") != ""),

    manual_override = url %in% (overrides %>% filter(override_relevant) %>% pull(url)),

    # graded recency: for the 2026 cycle, freshness is itself a ranking signal
    # (as.character guards against `year` being read as numeric -> coalesce clash)
    pub_year = suppressWarnings(as.numeric(str_extract(
      paste(coalesce(as.character(pub_date_guess), ""),
            coalesce(as.character(year), "")), "20\\d{2}"))),
    recency_points = case_when(pub_year >= 2026 ~ 12,
                               pub_year == 2025 ~ 6,
                               TRUE             ~ 0),

    # ---- relevance score with caps ------------------------------------------
    relevance_raw = core_mentions +
      mentions_egriss * 10 +
      if_else(has_inclusion, 15, 0) +          # actual displacement-inclusion language
      if_else(cooccur_pop_stat, 15, 0) +       # population term NEAR a stat-activity term
      # birth registration NEVER scores on its own (mentions_birth_reg is not
      # used here at all) - only when it co-occurs with a stateless/nationality
      # term nearby, corroborating evidence already found some other way
      if_else(cooccur_birthreg_stateless, 10, 0) +
      if_else(nso_led_site, 20, 0) +
      if_else(in_title_match, 25, 0) +
      if_else(str_detect(coalesce(trust, ""), "HIGH"), 15, 0) +
      if_else(str_starts(doc_type, "PDF"), 10, 0) +
      recency_points,

    relevance_score = case_when(
      spam_flag | false_positive_flag          ~ 0,
      manual_override                          ~ relevance_raw,
      # only evidence is the ambiguous 'asilo'/'asile' word -> cap 25
      ambiguous_asilo_only                     ~ pmin(relevance_raw, 25),
      # no STRONG population/EGRISS terms in extracted text -> cap 20
      text_extracted & core_strong == 0 & mentions_egriss == 0 &
        mentions_migration == 0                ~ pmin(relevance_raw, 20),
      # only generic migration terms -> cap 30
      text_extracted & core_strong == 0 & mentions_egriss == 0 &
        mentions_migration > 0                 ~ pmin(relevance_raw, 30),
      # URL-inventory-only record where full text could not be extracted -> cap 45
      l3_only & !text_extracted                ~ pmin(relevance_raw, 45),
      # fallback: relevance_raw is unbounded (core_mentions is a raw keyword
      # count), so a long, keyword-dense document (e.g. a 60k-char StatCan
      # article) could otherwise score past 100 - always clamp to the 0-100 scale.
      TRUE                                     ~ pmin(relevance_raw, 100)
    ),

    # ---- lead type & evidence nature ------------------------------------------
    producer_l  = str_to_lower(coalesce(producer, "")),
    title_l_nat = str_to_lower(coalesce(title, "")),
    # MICS / DHS / census / LFS / HBS / LSMS are NSO-published official statistics
    # (country-led), even when found on the UNICEF/DHS catalogue and co-funded.
    is_official_instrument = str_detect(title_l_nat, OFFICIAL_TITLE_PAT),
    # UNHCR/WFP/IOM rapid & operational surveys (RMS, protection monitoring,
    # MSNA, PDM, profiling...) - partner-led unless an NSO is explicitly named.
    is_humanitarian_signal = str_detect(producer_l, HUMANITARIAN_PRODUCER_PAT) |
                             str_detect(title_l_nat, HUMANITARIAN_TITLE_PAT),
    nso_involved = nso_led_site |
      str_detect(producer_l, NSO_PRODUCER_PAT) |
      str_detect(title_l_nat, "statistic|census bureau|stats office|national bureau"),

    # official instrument WITH no displacement-inclusion signal: the instrument
    # exists but there's no evidence it captured a displaced group -> review pile,
    # not a candidate (most DHS/MICS/census rounds don't disaggregate displacement)
    official_no_inclusion = is_official_instrument & !has_inclusion &
                            !in_title_match & core_strong == 0,
    candidate_lead_type = case_when(
      spam_flag | false_positive_flag                       ~ "not applicable",
      ambiguous_asilo_only                                  ~ "unclear",
      # official instrument that DOES show inclusion (or names the population in
      # its title) = country-led (NSO-published), even when UNICEF/USAID support.
      is_official_instrument & (has_inclusion | in_title_match | core_strong > 0) ~ "likely country-led",
      # official instrument with no displacement signal at all -> needs review
      is_official_instrument                                ~ "unclear",
      nso_led_site & core_strong > 0 &
        mentions_statactivity > 0                           ~ "likely country-led",
      nso_involved & core_strong > 0                        ~ "likely country-led",
      # humanitarian/operational survey with no NSO named -> partner-led
      is_humanitarian_signal & !nso_involved                ~ "likely partner-led",
      str_starts(coalesce(source_layer, ""), "L1:") &
        str_detect(producer_l, PARTNER_PRODUCER_PAT) &
        !nso_involved                                       ~ "likely partner-led",
      TRUE                                                  ~ "unclear"
    ),

    evidence_nature = case_when(
      spam_flag | false_positive_flag                       ~ "not applicable",
      # official instruments (census/DHS/MICS/LFS/LSMS...) -> official statistics
      is_official_instrument                                ~ "official statistics",
      nso_involved                                          ~ "official statistics",
      is_humanitarian_signal                                ~ "humanitarian/operational data",
      TRUE                                                  ~ "unclear"
    ),
    is_humanitarian = evidence_nature == "humanitarian/operational data",

    needs_manual_check = doc_type %in%
        c("unreachable", "PDF (no text layer)", "PDF (too large, skipped)") |
        (core_mentions == 0 & doc_type == "webpage")
  )

# ---- attach outreach contacts per country ------------------------------------
# Evidence-source contacts (emails found inside documents) are kept separate;
# only emails on the country's own NSO domain are promoted into the outreach pool.
record_nso_emails <- enriched %>%
  filter(!is.na(emails_found)) %>%
  left_join(nso_registry, by = "country") %>%
  mutate(email = str_split(emails_found, ";\\s*")) %>%
  select(country, domain, email) %>%
  unnest(email) %>%
  mutate(contact_type = map2_chr(email, domain, classify_contact),
         contact_source = "evidence document") %>%
  filter(str_starts(contact_type, "NSO") |
         contact_type == "national statistical system actor") %>%
  distinct(country, email, .keep_all = TRUE)

outreach_pool <- bind_rows(
  contact_pool %>% select(country, email, contact_type, contact_source),
  record_nso_emails %>% select(country, email, contact_type, contact_source)
) %>%
  filter(contact_type != "unknown") %>%
  mutate(rank = CONTACT_RANK[contact_type]) %>%
  arrange(country, rank) %>%
  distinct(country, email, .keep_all = TRUE)

pick_contacts <- function(ctry, lead_type) {
  pool <- outreach_pool %>% filter(country == ctry)
  nso  <- pool %>% filter(rank <= 4)
  prt  <- pool %>% filter(rank >= 5)

  # partner contacts may lead ONLY when the candidate appears partner-led
  ordered <- if (identical(lead_type, "likely partner-led") && nrow(prt) > 0) {
    bind_rows(prt, nso)
  } else {
    bind_rows(nso, prt)
  }
  p1 <- if (nrow(ordered) >= 1) ordered[1, ] else NULL
  p2 <- if (nrow(ordered) >= 2) ordered[2, ] else NULL

  tibble(
    recommended_primary_contact        = p1$email %||% NA_character_,
    recommended_primary_contact_type   = p1$contact_type %||% NA_character_,
    recommended_primary_contact_source = p1$contact_source %||% NA_character_,
    recommended_secondary_contact        = p2$email %||% NA_character_,
    recommended_secondary_contact_type   = p2$contact_type %||% NA_character_,
    recommended_secondary_contact_source = p2$contact_source %||% NA_character_,
    has_nso_contact = nrow(nso) > 0
  )
}

contact_cols <- map2_dfr(enriched$country, enriched$candidate_lead_type, pick_contacts)
enriched <- bind_cols(enriched, contact_cols)

enriched <- enriched %>%
  mutate(
    contact_confidence = case_when(
      is.na(recommended_primary_contact)                       ~ "none",
      recommended_primary_contact_type %in%
        c("NSO general institutional contact",
          "NSO department contact") &
        recommended_primary_contact_source ==
          "NSO contact page crawl"                             ~ "high",
      str_starts(recommended_primary_contact_type, "NSO") |
        recommended_primary_contact_type ==
          "national statistical system actor"                  ~ "medium",
      TRUE                                                     ~ "low"
    ),
    contact_validation_needed =
      coalesce(recommended_primary_contact_type, "") == "NSO named staff contact" |
      contact_confidence %in% c("low", "none"),
    contact_gap = !has_nso_contact,
    contact_gap_reason = case_when(
      has_nso_contact                          ~ NA_character_,
      !is.na(recommended_primary_contact)      ~ "only partner/catalogue contact available - not confirmed as correct NSO contact",
      TRUE                                     ~ "no suitable public contact found on NSO website"
    ),

    # ---- outreach category A-E -----------------------------------------------
    outreach_category = case_when(
      spam_flag                                              ~ "E. Suppress",
      false_positive_flag                                    ~ "E. Suppress",
      # populations tags inherited from search queries / URL keywords are NOT
      # trusted: if the extracted text itself contains no STRONG relevant terms,
      # suppress. Curated catalog records are exempt (metadata is trusted).
      text_extracted & core_strong == 0 & mentions_egriss == 0 &
        mentions_migration == 0 & !manual_override &
        !is_official_instrument &
        coalesce(trust, "") != "HIGH (curated catalog)"      ~ "E. Suppress",
      # official instrument (DHS/MICS/census) with no displacement-inclusion
      # signal: keep for review (TIER2 landscape), but it is NOT a candidate
      official_no_inclusion & !manual_override               ~ "C. Evidence lead only",
      # only ambiguous 'asilo'/'asile' word -> evidence lead only, no outreach
      ambiguous_asilo_only & !manual_override                ~ "C. Evidence lead only",
      # A / B now require a STRONG candidate (term in title or 2+ mentions).
      # Country-led includes NSO-published MICS/DHS/census, not just NSO-site finds.
      (nso_led_site | candidate_lead_type == "likely country-led") &
        strong_candidate & has_nso_contact                   ~ "A. Strong NSO follow-up candidate",
      (nso_led_site | candidate_lead_type == "likely country-led") &
        strong_candidate                                     ~ "D. Manual review needed",  # A-quality evidence, contact gap
      strong_candidate &
        candidate_lead_type %in%
          c("likely partner-led", "unclear")                 ~ "B. Strong candidate but partner-led or unclear lead",
      !text_extracted                                        ~ "D. Manual review needed",
      # single incidental displacement mention (not title, <2x) -> manual review
      core_strong > 0                                        ~ "D. Manual review needed",
      # no displacement terms: EGRISS or migration only -> landscape context
      mentions_egriss > 0 | mentions_migration > 0           ~ "C. Evidence lead only",
      TRUE                                                   ~ "D. Manual review needed"
    ),

    # ---- outreach route, priority, notes --------------------------------------
    # NOTE: GAIN_PHASE5_CROSSREF.R upgrades the route to "existing GAIN focal
    # point" for countries with an ACTIVE GAIN respondent.
    recommended_outreach_route = case_when(
      str_starts(outreach_category, "E")                     ~ "do not follow up yet",
      str_starts(outreach_category, "C")                     ~ "do not follow up yet",
      str_starts(outreach_category, "A")                     ~ "NSO direct",
      str_starts(outreach_category, "B") &
        candidate_lead_type == "likely partner-led" &
        !is.na(recommended_primary_contact)                  ~ "partner-supported follow-up",
      str_starts(outreach_category, "B") & has_nso_contact   ~ "NSO direct",
      str_starts(outreach_category, "B")                     ~ "manual contact search needed",
      contact_gap                                            ~ "manual contact search needed",
      TRUE                                                   ~ "do not follow up yet"
    ),
    outreach_priority = case_when(
      str_starts(outreach_category, "E") ~ "none",
      # humanitarian/operational data with no NSO involved: deprioritise -
      # not official statistics, less likely to be a GAIN focus
      is_humanitarian                    ~ "low",
      str_starts(outreach_category, "A") ~ "high",
      str_starts(outreach_category, "B") ~ "medium",
      str_starts(outreach_category, "D") ~ "medium",
      str_starts(outreach_category, "C") ~ "low",
      TRUE                               ~ "none"
    ),
    outreach_notes = case_when(
      str_starts(outreach_category, "E") ~
        "Suppressed: apparent false positive, spam or no GAIN-relevant evidence. Do not contact.",
      str_starts(outreach_category, "A") ~ paste0(
        "Possible candidate. The source appears to be published on the official NSO website and ",
        "evidence suggests a relevant population group and a statistical activity. ",
        "May be relevant to GAIN; requires confirmation from the NSO or relevant respondent ",
        "before any conclusion is drawn.",
        if_else(contact_validation_needed,
                " Recommended contact requires manual validation before outreach.", "")),
      str_starts(outreach_category, "B") ~ paste0(
        "Possible candidate, but the lead institution appears to be a partner or cannot be ",
        "confirmed from the available evidence. Requires manual review to establish the NSO role ",
        "before follow-up."),
      str_starts(outreach_category, "C") ~ paste0(
        "Evidence lead only: useful for understanding the statistical landscape, ",
        "not yet suitable for NSO outreach."),
      TRUE ~ paste0(
        "Requires manual review: the contact, lead institution, population group or source type ",
        "is unclear from the automated evidence. ",
        if_else(contact_gap, coalesce(contact_gap_reason, ""), ""))
    ),

    # ---- user-friendly overall comment (one plain-English paragraph) ----------
    findings_txt = pmap_chr(
      list(mentions_refugee, mentions_idp, mentions_stateless,
           mentions_egriss, mentions_statactivity, mentions_inclusion),
      function(r, i, s, e, a, inc) {
        parts <- c(if (r > 0) paste0("refugee/asylum terms (", r, ")"),
                   if (i > 0) paste0("internal-displacement terms (", i, ")"),
                   if (s > 0) paste0("statelessness/nationality terms (", s, ")"),
                   if (e > 0) paste0("EGRISS/IRIS mentions (", e, ")"),
                   if (a > 0) paste0("census/survey terms (", a, ")"),
                   if (inc > 0) paste0("inclusion/disaggregation terms (", inc, ")"))
        if (length(parts) == 0) "" else paste(parts, collapse = ", ")
      }),
    source_described = case_when(
      str_starts(coalesce(source_layer, ""), "L1:DHS")  ~ "the DHS Program survey catalogue",
      str_starts(coalesce(source_layer, ""), "L1:")     ~ paste0("the ", str_remove(source_layer, "^L1:"), " catalogue"),
      str_starts(coalesce(source_layer, ""), "L2:")     ~ "a keyword search of the NSO website",
      coalesce(source_layer, "") == "L3:news-section"   ~ "the NSO news/events/publications pages",
      str_starts(coalesce(source_layer, ""), "L3:")     ~ "the NSO website page inventory",
      TRUE                                              ~ "automated collection"
    ),
    finding_sentence = case_when(
      spam_flag           ~ "Excluded: the URL or title points to unrelated commercial content.",
      false_positive_flag ~ "Suppressed: the apparent match comes from an unrelated term (e.g. enterprise statistics), not displacement.",
      !text_extracted     ~ "The text could not be extracted automatically - open the link and check by hand.",
      ambiguous_asilo_only ~ "Weak signal: the only match is the word 'asilo'/'asile', which in Portuguese/Spanish also means a care home and often appears in site menus - likely not displacement evidence.",
      findings_txt == "" & mentions_migration > 0 ~ "Only generic migration terms were found; no specific refugee, IDP or statelessness content was detected.",
      findings_txt == ""  ~ "No population-group terms were detected in the extracted text.",
      TRUE                ~ paste0("Automated screening found ", findings_txt, ".")
    ),
    lead_sentence = case_when(
      candidate_lead_type == "likely country-led" ~ " The example appears likely country-led (published on the NSO's own website).",
      candidate_lead_type == "likely partner-led" ~ " The example appears likely partner-led; the NSO role needs confirmation.",
      candidate_lead_type == "unclear"            ~ " The lead institution is unclear from the available evidence.",
      TRUE                                        ~ ""
    ),
    nature_sentence = if_else(is_humanitarian,
      " This appears to be humanitarian/operational data (not official statistics) with no NSO involvement - lower GAIN priority unless an NSO role can be confirmed.", ""),
    inclusion_sentence = case_when(
      has_inclusion ~ " Evidence suggests a displaced group may be captured or disaggregated (inclusion language found) - this is the GAIN-relevant signal; verify the exact variable/table.",
      official_no_inclusion ~ " The instrument exists but NO displacement-inclusion signal was detected - manually check whether a displaced group is actually captured before treating it as a GAIN example.",
      TRUE ~ ""),
    contact_sentence = case_when(
      str_starts(outreach_category, "E")        ~ "",
      !is.na(recommended_primary_contact)       ~ paste0(
        " Suggested contact: ", recommended_primary_contact, " (",
        recommended_primary_contact_type,
        if_else(contact_validation_needed, "; validate before use", ""), ")."),
      contact_gap                               ~ paste0(
        " Contact gap: ", coalesce(contact_gap_reason, "no contact identified"), "."),
      TRUE                                      ~ ""
    ),
    next_step = case_when(
      str_starts(outreach_category, "A") ~ "Manual review, then possible NSO follow-up.",
      str_starts(outreach_category, "B") ~ "Manual review to clarify the lead institution before any follow-up.",
      str_starts(outreach_category, "C") ~ "Keep as landscape context; no outreach for now.",
      str_starts(outreach_category, "D") ~ "Manual review needed.",
      TRUE                               ~ "No action - suppressed."
    ),
    overall_comment = paste0(
      coalesce(doc_language, "Unknown-language"), " ",
      if_else(str_starts(coalesce(doc_type, ""), "PDF"), "PDF document", "web page"),
      if_else(!is.na(pub_date_guess), paste0(" (", pub_date_guess, ")"),
              if_else(!is.na(year), paste0(" (", year, ")"), "")),
      " found via ", source_described, ". ",
      finding_sentence, lead_sentence, nature_sentence, inclusion_sentence,
      contact_sentence, " Next step: ", next_step
    )
  ) %>%
  select(-url_title_l, -producer_l, -idp_url_fragment_only, -title_l_nat,
         -findings_txt, -source_described, -finding_sentence,
         -lead_sentence, -nature_sentence, -inclusion_sentence,
         -contact_sentence, -next_step) %>%
  arrange(desc(relevance_score))

# UTF-8 with BOM so Excel renders Arabic correctly
write_excel_csv(enriched, paste0("GAIN_EVIDENCE_ENRICHED_", stamp, ".csv"))
message(paste0("\nCSV written: GAIN_EVIDENCE_ENRICHED_", stamp, ".csv"))

# ------------------------------------------------------------------------------
# STEP 4: per-country processing overview - UPDATES the existing
# LAYER3_inventory_stats_*.csv in place (no new output file)
# ------------------------------------------------------------------------------
message("\nStep 4: processing overview...")

stats_file <- list.files(pattern = "^LAYER3_inventory_stats_.*\\.csv$") %>%
  sort() %>% last()
l3_stats <- if (!is.na(stats_file)) {
  read_csv(stats_file, show_col_types = FALSE) %>%
    select(any_of(c("country", "domain", "sitemap_urls", "commoncrawl_urls",
                    "total_inventory", "keyword_matches", "coverage")))
} else tibble(country = character(), domain = character(), coverage = character())

l2_done <- if (file.exists("LAYER2_progress.csv")) {
  read_csv("LAYER2_progress.csv", show_col_types = FALSE) %>% count(country, name = "l2_queries_done")
} else tibble(country = character(), l2_queries_done = integer())

# top 3 candidates per country (the "executive view")
top_cands <- enriched %>%
  filter(!str_starts(outreach_category, "E")) %>%
  group_by(country) %>%
  slice_max(relevance_score, n = 3, with_ties = FALSE) %>%
  summarise(top_candidates = paste0(
    "[", str_sub(outreach_category, 1, 1), "] ",
    str_sub(coalesce(title, url), 1, 70), " (score ", relevance_score, ")",
    collapse = " | "), .groups = "drop")

country_results <- enriched %>%
  group_by(country) %>%
  summarise(
    records_found            = n(),
    candidate_records_found  = sum(!str_starts(outreach_category, "E")),
    high_priority_candidates = sum(str_starts(outreach_category, "A")),
    manual_review_needed     = sum(str_starts(outreach_category, "D")),
    n_extraction_failed      = sum(!text_extracted),
    has_any_nso_contact      = any(has_nso_contact),
    .groups = "drop"
  )

overview <- nso_registry %>%
  select(country, domain, languages) %>%
  full_join(l3_stats, by = c("country", "domain")) %>%
  left_join(l2_done, by = "country") %>%
  left_join(country_results, by = "country") %>%
  left_join(top_cands, by = "country") %>%
  mutate(
    expected_l2_queries = 3 * (str_count(languages, ",") + 1) + 1,
    across(c(records_found, candidate_records_found, high_priority_candidates,
             manual_review_needed, n_extraction_failed, l2_queries_done),
           ~ replace_na(.x, 0L)),
    inventory_status = coalesce(coverage, "not yet inventoried"),
    processing_status = case_when(
      l2_queries_done == 0 & is.na(coverage)                         ~ "not started",
      coalesce(coverage, "") == "NONE - manual review needed" &
        l2_queries_done == 0                                         ~ "failed",
      !is.na(expected_l2_queries) & l2_queries_done > 0 &
        l2_queries_done < expected_l2_queries                        ~ "in progress",
      TRUE                                                           ~ "completed"
    ),
    text_extraction_status = case_when(
      records_found == 0                        ~ "no records to extract",
      n_extraction_failed == 0                  ~ "all extracted",
      n_extraction_failed < records_found       ~ paste0(n_extraction_failed, " of ",
                                                         records_found, " failed"),
      TRUE                                      ~ "extraction failed"
    ),
    contact_search_status = case_when(
      isTRUE(has_any_nso_contact)               ~ "NSO contact found",
      records_found > 0                         ~ "no NSO contact found - gap",
      TRUE                                      ~ "not searched"
    ),
    failure_reason = case_when(
      processing_status == "failed"             ~ "website access failed / no inventory and no search coverage",
      text_extraction_status == "extraction failed" ~ "text extraction failed for all records",
      TRUE                                      ~ NA_character_
    ),
    last_attempt_date = if_else(processing_status %in% c("completed", "in progress", "failed"),
                                as.character(Sys.Date()), NA_character_),
    next_action = case_when(
      high_priority_candidates > 0 &
        isTRUE(has_any_nso_contact)             ~ "candidate ready for possible follow-up (manual review first)",
      manual_review_needed > 0                  ~ "candidate found but manual review needed",
      records_found > 0 & !isTRUE(has_any_nso_contact) ~ "contact gap - manual contact search",
      processing_status == "failed"             ~ "failed website access - retry or manual check",
      processing_status == "in progress"        ~ "continue search (resumable)",
      processing_status == "not started"        ~ "not yet processed",
      records_found == 0 &
        coalesce(total_inventory, 0) > 0        ~ "no evidence detected",
      TRUE                                      ~ "insufficient automated coverage - manual check"
    )
  ) %>%
  select(country, domain, processing_status, inventory_status,
         text_extraction_status, contact_search_status,
         records_found, candidate_records_found, high_priority_candidates,
         manual_review_needed, top_candidates, failure_reason,
         last_attempt_date, next_action,
         any_of(c("sitemap_urls", "commoncrawl_urls", "news_links",
                  "total_inventory", "keyword_matches", "coverage",
                  "l2_queries_done")))

overview_file <- if (!is.na(stats_file)) stats_file else
  paste0("LAYER3_inventory_stats_", stamp, ".csv")
write_excel_csv(overview, overview_file)
message(paste("Processing overview written into:", overview_file))

# ==============================================================================
# HTML REVIEW REPORT
# ==============================================================================
esc <- function(x) {
  x <- coalesce(as.character(x), "")
  x %>% str_replace_all("&", "&amp;") %>% str_replace_all("<", "&lt;") %>%
        str_replace_all(">", "&gt;")
}

highlight <- function(x) {
  x <- esc(x)
  for (p in PAT) x <- str_replace_all(x, regex(paste0("(", p, ")"), ignore_case = TRUE),
                                      "<mark>\\1</mark>")
  x
}

badge <- function(txt, color) sprintf(
  "<span style='background:%s;color:#fff;border-radius:10px;padding:2px 9px;font-size:12px;margin-right:6px;white-space:nowrap'>%s</span>",
  color, esc(txt))

CAT_COLORS <- c("A" = "#1e8449", "B" = "#b9770e", "C" = "#5d6d7e",
                "D" = "#884ea0", "E" = "#922b21")

record_card <- function(r) {
  ctx <- c(r$context_refugee, r$context_idp, r$context_stateless,
           r$context_egriss) %>% na.omit()
  # dir='auto' so Arabic snippets render right-to-left correctly
  ctx_html <- if (length(ctx) > 0) {
    paste0("<div dir='auto' style='background:#f6f8fa;border-left:4px solid #3b71b9;padding:8px 12px;margin:8px 0;font-size:14px'>",
           paste(map_chr(ctx, highlight), collapse = "<br><br>"), "</div>")
  } else ""

  gloss_html <- if (!is.na(r$english_working_summary)) {
    paste0(
      "<div style='background:#eef7ee;border-left:4px solid #1e8449;padding:8px 12px;margin:8px 0;font-size:14px'>",
      if (!is.na(r$original_excerpt))
        paste0("<div dir='auto' style='margin-bottom:6px;color:#333'>",
               esc(r$original_excerpt), "</div>") else "",
      "<b>English working summary (automated):</b> ",
      esc(r$english_working_summary), "</div>")
  } else ""

  # outreach contact block (separate from evidence-source contacts)
  outreach_html <- if (!is.na(r$recommended_primary_contact)) {
    paste0(
      "<div style='background:#eef3fb;border-left:4px solid #1a4d8f;padding:8px 12px;margin:8px 0;font-size:14px'>",
      "<b>Suggested outreach contact:</b> ", esc(r$recommended_primary_contact),
      " <i>(", esc(r$recommended_primary_contact_type), "; ",
      esc(r$recommended_primary_contact_source), ")</i>",
      if (!is.na(r$recommended_secondary_contact))
        paste0("<br><b>Secondary:</b> ", esc(r$recommended_secondary_contact),
               " <i>(", esc(r$recommended_secondary_contact_type), ")</i>") else "",
      "<br><b>Confidence:</b> ", esc(r$contact_confidence),
      if (isTRUE(r$contact_validation_needed))
        " - <b style='color:#922b21'>manual validation needed before outreach</b>" else "",
      "<br><b>Route:</b> ", esc(r$recommended_outreach_route),
      " | <b>Priority:</b> ", esc(r$outreach_priority),
      "</div>")
  } else if (isTRUE(r$contact_gap) && !str_starts(r$outreach_category, "E")) {
    paste0("<div style='background:#fdf2f2;border-left:4px solid #922b21;padding:8px 12px;margin:8px 0;font-size:13px'>",
           "<b>Contact gap:</b> ", esc(coalesce(r$contact_gap_reason, "no contact identified")),
           "</div>")
  } else ""

  contacts <- c()
  if (!is.na(r$emails_found))
    contacts <- c(contacts, paste0("<b>Emails on source page (evidence-source, not necessarily outreach contacts):</b> ",
                                   esc(r$emails_found)))
  if (!is.na(r$staff_candidates))
    contacts <- c(contacts, paste0("<b>Possible staff mentioned:</b> ", esc(r$staff_candidates)))
  contacts_html <- if (length(contacts) > 0) {
    paste0("<div style='background:#fff8e6;border-left:4px solid #e6a700;padding:8px 12px;margin:8px 0;font-size:14px'>",
           paste(contacts, collapse = "<br>"), "</div>")
  } else ""

  summ <- if (!is.na(r$extract_summary))
    paste0("<p dir='auto' style='color:#444;font-size:14px;margin:6px 0'>", esc(r$extract_summary), "</p>") else ""

  # user-friendly overall comment, shown prominently on every card
  notes <- paste0("<p style='color:#333;font-size:14px;background:#fafafa;",
                  "border-left:4px solid #95a5a6;padding:8px 12px;margin:8px 0'>",
                  esc(r$overall_comment), "</p>")

  warn <- if (isTRUE(r$needs_manual_check)) badge("NEEDS MANUAL CHECK", "#c0392b") else ""
  cat_letter <- str_sub(r$outreach_category, 1, 1)

  paste0(
    "<div class='card' style='border:1px solid #ddd;border-radius:8px;padding:14px 18px;margin:10px 0;background:#fff'>",
    "<a dir='auto' href='", esc(r$url), "' target='_blank' style='font-size:16px;font-weight:600;color:#1a4d8f;text-decoration:none'>",
    esc(coalesce(r$title, r$url)), "</a>",
    "<div style='margin:7px 0'>",
    badge(r$outreach_category, CAT_COLORS[cat_letter] %||% "#7f8c8d"),
    badge(paste("lead:", r$candidate_lead_type), "#34495e"),
    if (isTRUE(r$is_humanitarian)) badge("humanitarian data (not official)", "#d35400") else "",
    if (!is.na(r$doc_language)) badge(r$doc_language, "#16a085") else "",
    badge(r$doc_type, if (str_starts(coalesce(r$doc_type, ""), "PDF")) "#8e44ad" else "#2e86c1"),
    badge(paste("score", r$relevance_score), "#27ae60"),
    if (!is.na(r$year)) badge(r$year, "#7f8c8d") else "",
    if (!is.na(r$trust)) badge(r$trust, "#95a5a6") else "",
    if (!is.na(r$source_layer)) badge(r$source_layer, "#b3b3b3") else "",
    warn,
    "</div>",
    notes, summ, ctx_html, gloss_html, outreach_html, contacts_html,
    "<div style='font-size:12px;color:#888'>refugee: ", r$mentions_refugee,
    " &nbsp;|&nbsp; IDP: ", r$mentions_idp,
    " &nbsp;|&nbsp; stateless: ", r$mentions_stateless,
    " &nbsp;|&nbsp; EGRISS: ", r$mentions_egriss,
    " &nbsp;|&nbsp; statistical activity: ", r$mentions_statactivity,
    if (!is.na(r$pub_date_guess)) paste0(" &nbsp;|&nbsp; date: ", esc(r$pub_date_guess)) else "",
    "</div>",
    "</div>")
}

# Suppressed records go to a collapsed section at the bottom
active   <- enriched %>% filter(!str_starts(outreach_category, "E"))
suppressed <- enriched %>% filter(str_starts(outreach_category, "E"))

country_sections <- active %>%
  group_by(country) %>%
  group_map(function(g, key) {
    cards <- paste(map_chr(seq_len(nrow(g)), ~ record_card(g[.x, ])), collapse = "\n")
    paste0(
      "<details open><summary style='font-size:20px;font-weight:700;cursor:pointer;",
      "padding:10px 0;color:#1a1a2e'>", esc(key$country),
      " <span style='font-weight:400;color:#777;font-size:14px'>(", nrow(g),
      " possible candidates, top score ", max(g$relevance_score), ")</span></summary>",
      cards, "</details>")
  }) %>% unlist() %>% paste(collapse = "\n")

suppressed_section <- if (nrow(suppressed) > 0) {
  cards <- paste(map_chr(seq_len(nrow(suppressed)), ~ record_card(suppressed[.x, ])),
                 collapse = "\n")
  paste0("<details><summary style='font-size:18px;font-weight:700;cursor:pointer;",
         "padding:10px 0;color:#922b21'>Suppressed records (", nrow(suppressed),
         ") - false positives / spam / no GAIN-relevant evidence</summary>",
         cards, "</details>")
} else ""

# processing overview table for the report header
ov <- overview %>% arrange(desc(high_priority_candidates), desc(candidate_records_found))
ov_rows <- paste(map_chr(seq_len(nrow(ov)), function(i) {
  r <- ov[i, ]
  paste0("<tr><td>", esc(r$country), "</td><td>", esc(r$domain), "</td><td>",
         esc(r$processing_status), "</td><td>", r$records_found, "</td><td>",
         r$candidate_records_found, "</td><td>", r$high_priority_candidates,
         "</td><td>", r$manual_review_needed, "</td><td>",
         esc(r$contact_search_status), "</td><td>", esc(r$next_action), "</td></tr>")
}), collapse = "\n")

overview_html <- paste0(
  "<details><summary style='font-size:18px;font-weight:700;cursor:pointer;padding:10px 0'>",
  "Processing overview by country (", nrow(ov), " domains)</summary>",
  "<table style='border-collapse:collapse;font-size:13px;width:100%'>",
  "<tr style='background:#1a1a2e;color:#fff'>",
  "<th style='padding:6px'>Country</th><th>Domain</th><th>Status</th><th>Records</th>",
  "<th>Candidates</th><th>High priority</th><th>Manual review</th><th>Contact search</th><th>Next action</th></tr>",
  ov_rows, "</table>",
  "<style>td{border:1px solid #ddd;padding:5px}</style></details>")

cat_summary <- enriched %>% count(outreach_category) %>%
  mutate(txt = paste0(outreach_category, ": ", n)) %>% pull(txt) %>% paste(collapse = " | ")

html <- paste0(
  "<!DOCTYPE html><html><head><meta charset='utf-8'>",
  "<title>GAIN Evidence Review - ", stamp, "</title></head>",
  "<body style='font-family:Segoe UI,Arial,sans-serif;background:#f0f2f5;",
  "max-width:1100px;margin:0 auto;padding:24px'>",
  "<h1 style='color:#1a1a2e'>GAIN Evidence Review - possible candidates for manual screening</h1>",
  "<p style='color:#555'>", nrow(enriched), " records | ", n_distinct(enriched$country),
  " countries | ", sum(str_starts(enriched$doc_type, "PDF")), " PDFs | generated ",
  format(Sys.Date(), "%d %b %Y"), "</p>",
  "<p style='color:#555'><b>", cat_summary, "</b></p>",
  "<p style='background:#fff8e6;border-left:4px solid #e6a700;padding:10px 14px;color:#555;font-size:14px'>",
  "<b>How to read this report.</b> Every record here is a <b>possible candidate</b> that ",
  "<b>may be relevant to GAIN</b>. Evidence was collected automatically and ",
  "<b>requires manual review</b>; inclusion in the GAIN evidence base ",
  "<b>requires confirmation from the NSO or relevant respondent</b> through the GAIN Survey. ",
  "Nothing in this report is a confirmed GAIN example, and scores indicate screening priority only. ",
  "Suggested contacts are public professional contacts and named contacts must be validated ",
  "manually before any outreach.</p>",
  overview_html,
  "<input id='q' placeholder='Type to filter (country, title, keyword)...' ",
  "style='width:100%;padding:10px;font-size:15px;margin:12px 0;border:1px solid #ccc;border-radius:6px' ",
  "onkeyup='f()'>",
  "<script>function f(){var q=document.getElementById('q').value.toLowerCase();",
  "document.querySelectorAll('.card').forEach(function(c){",
  "c.style.display=c.innerText.toLowerCase().includes(q)?'':'none'})}</script>",
  country_sections, suppressed_section, "</body></html>")

report_file <- paste0("GAIN_EVIDENCE_REPORT_", stamp, ".html")
writeLines(html, report_file, useBytes = TRUE)

message(paste("HTML report written:", report_file))
message("Open it by double-clicking the file - works in any browser, shareable as-is.")
message(sprintf("\nSummary: %d records | %s", nrow(enriched), cat_summary))
message(sprintf("Contacts: %d records with an outreach contact suggestion | %d with a contact gap",
                sum(!is.na(enriched$recommended_primary_contact)),
                sum(enriched$contact_gap & !str_starts(enriched$outreach_category, "E"))))
