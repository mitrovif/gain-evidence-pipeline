# ==============================================================================
# GAIN MATCH v2  (product-level match of each candidate against GAIN examples)
#
# Why: the old crossref matched on COUNTRY + population + year window only
# (gain_flag "IN_GAIN" on 540/1,140 rows), and its AI step compared a candidate
# with just the ONE most similar GAIN example, seeing only title/org/year/pop.
# Result: e.g. Uganda "Refugee Health Status Report" -> "DHS", Hungary
# "asylum seekers table" -> "Demographic yearbook 2022".
#
# What this does instead - for every candidate, against EVERY GAIN example in
# the same country, it checks four things separately and says which agree:
#   ORG   same producer?          (NSO<->NSO in one country counts as same;
#                                  agency acronyms/aliases; name-token overlap)
#   TYPE  same kind of product?   census / survey / admin-register /
#                                  data-integration / publication / guidance
#   NAME  same product name?      acronym match (FDS, DHS, UMIS, LFS ...) or
#                                  distinctive-word overlap (generic words such
#                                  as "refugees", "statistics" do not count)
#   YEAR  same round?             candidate year inside the GAIN example's
#                                  PRO04-PRO05 span (+-1), or a later edition
# and assigns ONE category:
#   same product            -> already in GAIN (don't ask again)
#   new edition             -> same product, later round (ask to UPDATE it)
#   check - likely same     -> name+type agree, producer differs (joint/partner)
#   check - other language  -> same org+type, titles in different languages
#   same org, other product -> producer is in GAIN, THIS product is not (new)
#   other producer          -> country is in GAIN, producer is not (new)
#   new country             -> country has no GAIN example at all (new)
#   multi-country           -> regional dataset, review per country
#
# The two "check" categories are what the AI step (or a human) should look at;
# everything else is decided by explicit, visible rules.
#
# Input : newest GAIN_EVIDENCE_FINAL_*.csv (or evidence_flagged_*.csv)
#         analysis_ready_group_roster.csv  (the GAIN examples)
# Output: GAIN_MATCH_V2_[date].csv  (one row per candidate + match_v2_* columns)
# Additive: reads, writes a new file. Nothing upstream changes.
# ==============================================================================
suppressMessages({ library(tidyverse) })
source("shared/GAIN_COMMON.R")

YEAR_TOL  <- 1L
THIS_YEAR <- as.integer(format(Sys.Date(), "%Y"))

in_file <- tail(sort(list.files(".", "^GAIN_EVIDENCE_FINAL_.*\\.csv$")), 1)
if (!length(in_file)) in_file <- tail(sort(list.files(".", "^evidence_flagged_.*\\.csv$")), 1)
stopifnot(length(in_file) == 1)
message("Matching candidates from: ", in_file)
d <- suppressMessages(read_csv(in_file, show_col_types = FALSE, guess_max = 5000))
g <- suppressMessages(read_csv("analysis_ready_group_roster.csv", show_col_types = FALSE)) %>%
  mutate(mcountry = harmonize_country(mcountry))

pk <- function(df, col) if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))

# ------------------------------------------------------------------------------
# text helpers
# ------------------------------------------------------------------------------
fold <- function(x) {                       # lower-case, strip accents + punctuation
  x <- iconv(coalesce(as.character(x), ""), "UTF-8", "ASCII//TRANSLIT", sub = "")
  str_squish(str_replace_all(tolower(x), "[^a-z0-9 ]", " "))
}
# words that say nothing about WHICH product it is (every candidate has them)
GENERIC <- c(
  "the","of","and","in","for","on","a","an","to","by","with","from","at","its","their",
  "de","des","du","la","le","les","et","en","el","los","las","del","y","por","para","der","die",
  "das","und","von","im","zur","zum","og","i","av","for","til","med","om","van","het","en",
  "refugee","refugees","asylum","seeker","seekers","displaced","displacement","idp","idps",
  "stateless","statelessness","migrant","migrants","migration","immigrant","immigrants",
  "statistics","statistical","statistic","data","report","reports","survey","surveys",
  "population","national","persons","people","people's","analysis","study","project",
  "inclusion","including","included","forced","forcibly","international","protection",
  "republic","country","government","office","bureau","institute","annual","new","final",
  "results","estimates","information","table","tables","indicators","indicator","key",
  "refugies","refugiados","fluchtlinge","schutzsuchende","flyktninger","asile","asilo",
  "enquete","encuesta","statistik","statistique","estadistica","estadisticas","rapport","informe",
  "populations","world","bank","group","pdr","drc","quarterly","program","programme","round",
  "panel","wave","edition","part","volume","publication","published","press","release")
# words from the candidate's own country name (and common demonyms) are not
# product names either: "Thailand ... Census" vs "Thailand ... Survey"
country_words <- function(ctry) {
  w <- str_split(fold(ctry), " ")[[1]]
  extra <- c(ukraine = "ukrainian ukrainians", norway = "norwegian", germany = "german deutschland",
             lao = "laos", netherlands = "dutch", sweden = "swedish", switzerland = "swiss schweiz suisse",
             canada = "canadian", italy = "italian italia", france = "french", spain = "spanish espana",
             colombia = "colombian", venezuela = "venezuelan venezuelans", syrian = "syria syrians",
             afghanistan = "afghan afghans", sudan = "sudanese", somalia = "somali")
  unique(c(w, unlist(str_split(extra[names(extra) %in% w], " "))))
}
content_tokens <- function(x, ctry = "") {
  w <- str_split(fold(x), " ")[[1]]
  w <- w[nchar(w) >= 3 & !w %in% GENERIC & !w %in% country_words(ctry) & !str_detect(w, "^\\d+$")]
  unique(w)
}
# spelled-out product names -> one canonical acronym, in any of the languages
# seen, so "Forced displacement survey for Zambia" meets "FDS 2025".
PRODUCT_NAMES <- c(
  FDS  = "forced(ly)? displace(ment|d) survey|forcibly displaced survey",
  DHS  = "demographic (and|&) health survey|enquete demographique et de sante|encuesta de demografia y salud",
  MICS = "multiple indicator cluster",
  LFS  = "labou?r force survey|enquete (sur l )?emploi|encuesta de (poblacion activa|fuerza de trabajo)",
  HFPS = "high frequency (phone )?survey",
  RMS  = "results monitoring survey",
  SEA  = "socio ?economic assessment",
  MSNA = "multi ?sector(al)? needs assessment",
  MIS  = "malaria indicator survey",
  IMDB = "longitudinal immigration database|base de donnees longitudinales sur l immigration",
  DEP  = "demographic estimates program|programme des estimations demographiques",
  LTIM = "long term international migration",
  RIS  = "report on integration and (society|communities)|integratie en samenleving",
  PHC  = "population and housing census|recensement general de la population|censo (nacional )?de poblacion")
product_codes <- function(x) {
  t <- fold(x)
  names(PRODUCT_NAMES)[vapply(PRODUCT_NAMES, function(p) str_detect(t, p), logical(1))]
}
# acronyms: 2-7 capitals (optionally with digits) in the ORIGINAL text, minus
# country/agency ones that don't identify a product
NOT_PRODUCT_ACR <- c("UN","UNHCR","IOM","WB","JDC","JIPS","IDMC","UNICEF","UNFPA","UNDP","WFP",
  "EU","USA","UK","NSO","INE","ONS","SSB","CBS","KSH","DANE","INEGI","ISTAT","UBOS","PCBS",
  "IDP","IDPS","NA","II","III","IV","PDF","COVID","SDG","SDGS","EGRISS","IRRS","IRIS","IROSS","GIZ","PDR","DRC","CAR","RB","JSON","HTML","CSV")
acronyms <- function(x) {
  a <- str_extract_all(coalesce(as.character(x), ""), "\\b[A-Z][A-Z0-9]{1,6}\\b")[[1]]
  a <- unique(a[!str_detect(a, "^\\d") & !a %in% NOT_PRODUCT_ACR])
  a <- recode(a, UMIS = "MIS", UDHS = "DHS", BDIM = "IMDB", PED = "DEP", RGPH = "PHC", NPHC = "PHC",
              HFS = "HFPS", ENPE = "LFS")
  unique(c(a, product_codes(x)))
}

# ------------------------------------------------------------------------------
# product type
# ------------------------------------------------------------------------------
type_of <- function(text, gain_code = NA) {
  gc <- toupper(coalesce(as.character(gain_code), ""))
  if (str_detect(gc, "CENSUS")) return("census")
  if (str_detect(gc, "SURVEY")) return("survey")
  if (str_detect(gc, "ADMINISTRATIVE")) return("admin")
  if (str_detect(gc, "INTEGRATION")) return("integration")
  if (str_detect(gc, "GUIDANCE|WORKSHOP|TRAINING")) return("guidance")
  t <- fold(text)
  case_when(
    str_detect(t, "census|recensement|censo|zensus|folketelling|nphc|rgph") ~ "census",
    str_detect(t, "linkage|linked|matching|longitudinal|integrated data|data integration|imdb|bdim") ~ "integration",
    str_detect(t, "survey|enquete|encuesta|erhebung|umfrage|dhs|mics|lfs|labour force|household|profiling|msna|assessment|panel|poll|interview") ~ "survey",
    str_detect(t, "register|registre|registro|registerbas|administrative|permit|application|residence|asylum decision|database|base de donnees|benefit claim|records") ~ "admin",
    str_detect(t, "guidance|toolkit|training|workshop|manual|methodolog|e learning") ~ "guidance",
    str_detect(t, "yearbook|monitor|press release|bulletin|estimates|report|brief|analysis|study|publication|statistik|statistics") ~ "publication",
    TRUE ~ "unknown")
}
type_compatible <- function(a, b) {
  if (a == "unknown" || b == "unknown") return(TRUE)
  if (a == b) return(TRUE)
  # an NSO publication/report is usually BUILT ON admin or survey data
  if ("publication" %in% c(a, b) && any(c(a, b) %in% c("admin", "integration", "survey", "census"))) return(TRUE)
  if (all(c(a, b) %in% c("admin", "integration"))) return(TRUE)
  FALSE
}

# ------------------------------------------------------------------------------
# producer
# ------------------------------------------------------------------------------
AGENCY_ALIASES <- list(
  UNHCR = "unhcr|un refugee agency|high commissioner for refugees|acnur|hcr\\b",
  JDC   = "joint data cent|\\bjdc\\b",
  WB    = "world bank|banque mondiale|banco mundial|\\bwb\\b",
  IOM   = "\\biom\\b|\\boim\\b|organization for migration|displacement tracking|\\bdtm\\b",
  JIPS  = "\\bjips\\b|joint idp profiling",
  IDMC  = "\\bidmc\\b|internal displacement monitoring",
  UNICEF= "unicef", UNFPA = "unfpa", UNDP = "undp", WFP = "\\bwfp\\b|world food",
  ECOWAS= "ecowas|cedeao", IDB = "inter american development bank|\\bidb\\b|\\bbid\\b",
  EUROSTAT = "eurostat", OECD = "\\boecd\\b|\\bocde\\b")
NSO_PAT <- paste0("statisti|estadist|statisti|census|bureau of stat|office for national stat|",
  "\\bine\\b|\\binsee\\b|\\bistat\\b|\\bksh\\b|\\bssb\\b|\\bcbs\\b|\\bdane\\b|\\binegi\\b|\\bubos\\b|",
  "\\bpcbs\\b|\\bknbs\\b|\\bons\\b|destatis|statbel|\\bscb\\b|\\bcso\\b|\\bnbs\\b|\\bbps\\b|",
  "high commission for planning|haut commissariat au plan|zamstats|\\binstad\\b|\\bins\\b|",
  "department of statistics|central agency for public mobilization|capmas|belstat|geostat|armstat")
org_class <- function(x) {
  t <- fold(x)
  ag <- names(AGENCY_ALIASES)[vapply(AGENCY_ALIASES, function(p) str_detect(t, p), logical(1))]
  # an NSO string names no international agency (a joint "UNHCR + NSO" product
  # still matches on the agency side)
  list(agencies = ag, nso = str_detect(t, NSO_PAT) && length(ag) == 0,
       tokens = content_tokens(x))
}
same_org <- function(ca, ga) {
  if (length(ca$agencies) && length(ga$agencies) && length(intersect(ca$agencies, ga$agencies))) return(TRUE)
  if (isTRUE(ca$nso) && isTRUE(ga$nso)) return(TRUE)        # one NSO per country
  ov <- intersect(ca$tokens, ga$tokens)
  length(ov) >= 2 || (length(ov) == 1 && min(length(ca$tokens), length(ga$tokens)) == 1)
}

# ------------------------------------------------------------------------------
# GAIN example cards (one per example)
# ------------------------------------------------------------------------------
yr <- function(x) suppressWarnings(as.integer(x))
gcards <- g %>% filter(!is.na(PRO03), !is.na(mcountry)) %>%
  transmute(pindex2, country = mcountry, org = morganization, title = PRO03,
            desc = str_sub(coalesce(PRO13, ""), 1, 300),
            gtype_code = coalesce(PRO08, PRO08_label),
            y0 = yr(PRO04_year), y1 = yr(PRO05_year),
            pop = str_squish(paste(if_else(coalesce(PRO07.A, 0) == 1, "refugees", ""),
                                   if_else(coalesce(PRO07.B, 0) == 1, "IDPs", ""),
                                   if_else(coalesce(PRO07.C, 0) == 1, "stateless", "")))) %>%
  mutate(y1 = if_else(!is.na(y1) & y1 >= 9000, THIS_YEAR, y1),   # 9999 = ongoing
         gtype = pmap_chr(list(title, desc, gtype_code), function(t, ds, cd) {
           x <- type_of(t, cd); if (x == "unknown") type_of(ds) else x }),
         gorg = map(org, org_class),
         gtok = map2(title, country, content_tokens),
         gacr = map(title, acronyms))
message("GAIN examples: ", nrow(gcards), " in ", n_distinct(gcards$country), " countries")

# ------------------------------------------------------------------------------
# compare one candidate with one GAIN example
# ------------------------------------------------------------------------------
pair_check <- function(c_org, c_tok, c_acr, c_type, c_year, c_lang, e) {
  org_ok  <- same_org(c_org, e$gorg[[1]])
  acr_hit <- intersect(c_acr, e$gacr[[1]])
  ov      <- intersect(c_tok, e$gtok[[1]])
  jac     <- if (length(union(c_tok, e$gtok[[1]]))) length(ov) / length(union(c_tok, e$gtok[[1]])) else 0
  name_strong <- length(acr_hit) > 0 || jac >= 0.5 || length(ov) >= 3
  name_mid    <- !name_strong && length(ov) >= 2
  type_ok <- type_compatible(c_type, e$gtype)
  type_exact <- c_type == e$gtype && c_type != "unknown"
  y_in    <- !is.na(c_year) && !is.na(e$y0) &&
             c_year >= e$y0 - YEAR_TOL && c_year <= coalesce(e$y1, e$y0) + YEAR_TOL
  y_later <- !is.na(c_year) && !is.na(e$y0) && c_year > coalesce(e$y1, e$y0) + YEAR_TOL
  y_unk   <- is.na(c_year) || is.na(e$y0)
  same_name <- name_strong || (name_mid && type_exact)
  non_en    <- !is.na(c_lang) && !str_detect(tolower(c_lang), "^en|english")

  cat <- if (org_ok && same_name && type_ok && (y_in || y_unk)) "same product"
    else if (org_ok && same_name && type_ok && y_later)          "new edition"
    else if (!org_ok && same_name && type_ok)                    "check - likely same"
    else if (org_ok && type_ok && non_en && !name_mid)           "check - other language"
    else if (org_ok)                                             "same org, other product"
    else                                                         "other producer"
  rank <- match(cat, c("same product", "new edition", "check - likely same",
                       "check - other language", "same org, other product", "other producer"))
  mk <- function(ok) if (ok) "✓" else "✗"
  checks <- sprintf("org %s | type %s (%s vs %s) | name %s%s | year %s",
    mk(org_ok), mk(type_ok), c_type, e$gtype,
    if (same_name) "✓" else if (name_mid) "~" else "✗",
    if (length(acr_hit)) paste0(" [", paste(acr_hit, collapse = ","), "]")
      else if (length(ov)) paste0(" [", paste(head(ov, 4), collapse = ","), "]") else "",
    if (y_unk) "?" else sprintf("%s vs %s-%s%s", c_year, e$y0, coalesce(e$y1, e$y0),
      if (y_in) " ✓" else if (y_later) " later" else " ✗"))
  list(cat = cat, rank = rank, score = rank * 10 - jac * 5 - length(acr_hit) * 3, checks = checks)
}

# ------------------------------------------------------------------------------
# run over all candidates
# ------------------------------------------------------------------------------
is_multi <- function(x) str_detect(coalesce(x, ""), ",|and \\d+ more|region|europe and|africa$|americas")

cand <- tibble(
  country = harmonize_country(coalesce(pk(d, "Country"), pk(d, "country"))),
  org     = coalesce(pk(d, "llm_organization"), pk(d, "producer")),
  title   = coalesce(pk(d, "llm_instrument_or_title"), pk(d, "title"), pk(d, "Report_Title")),
  year    = yr(coalesce(str_extract(pk(d, "llm_year"), "(19|20)\\d{2}"),
                        str_extract(pk(d, "pub_year"), "(19|20)\\d{2}"),
                        str_extract(pk(d, "year"), "(19|20)\\d{2}"))),
  lang    = pk(d, "doc_language"),
  summary = coalesce(pk(d, "english_working_summary"), pk(d, "extract_summary"), ""))

n <- nrow(cand)
res <- vector("list", n)
for (i in seq_len(n)) {
  ci <- cand[i, ]
  if (is_multi(ci$country)) { res[[i]] <- list(cat = "multi-country"); next }
  ex <- gcards %>% filter(country == ci$country)
  if (!nrow(ex)) { res[[i]] <- list(cat = "new country"); next }
  c_org  <- org_class(ci$org)
  c_tok  <- content_tokens(ci$title, ci$country)
  c_acr  <- acronyms(ci$title)
  c_type <- type_of(ci$title)
  if (c_type == "unknown") c_type <- type_of(str_sub(ci$summary, 1, 300))
  pcs <- lapply(seq_len(nrow(ex)), function(j)
    pair_check(c_org, c_tok, c_acr, c_type, ci$year, ci$lang, ex[j, ]))
  sc  <- vapply(pcs, `[[`, numeric(1), "score")
  ord <- order(sc)
  b   <- ord[1]; e <- ex[b, ]
  top3 <- paste(vapply(head(ord, 3), function(j) sprintf("%s: %s (%s)",
            ex$pindex2[j], str_sub(ex$title[j], 1, 50), pcs[[j]]$cat), character(1)), collapse = " || ")
  res[[i]] <- list(cat = pcs[[b]]$cat, pindex2 = as.character(e$pindex2), gtitle = e$title,
    gorg = e$org, gtype = e$gtype, gyears = sprintf("%s-%s", e$y0, coalesce(e$y1, e$y0)),
    checks = pcs[[b]]$checks, ctype = c_type, n_ex = nrow(ex), top3 = top3)
  if (i %% 200 == 0) message(sprintf("  matched %d/%d", i, n))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
g1 <- function(k) vapply(res, function(r) as.character(r[[k]] %||% NA_character_), character(1))

out <- d %>% mutate(
  match_v2_category   = g1("cat"),
  match_v2_pindex2    = if_else(g1("cat") %in% c("new country", "multi-country"), NA_character_, g1("pindex2")),
  match_v2_gain_title = g1("gtitle"),
  match_v2_gain_org   = g1("gorg"),
  match_v2_gain_type  = g1("gtype"),
  match_v2_gain_years = g1("gyears"),
  match_v2_cand_type  = g1("ctype"),
  match_v2_checks     = g1("checks"),
  match_v2_n_gain_examples_in_country = g1("n_ex"),
  match_v2_top3       = g1("top3"))

today <- format(Sys.Date(), "%Y%m%d")
readr::write_excel_csv(out, sprintf("GAIN_MATCH_V2_%s.csv", today))

message("\n==== match_v2_category ====")
print(count(out, match_v2_category, sort = TRUE))
if ("gain_flag" %in% names(out)) {
  message("\n==== old country-level gain_flag  x  new category ====")
  print(table(old = out$gain_flag, new = out$match_v2_category))
}
if ("gain_match_type" %in% names(out)) {
  message("\n==== old example-level match  x  new category ====")
  print(table(old = out$gain_match_type, new = out$match_v2_category))
}
message("wrote GAIN_MATCH_V2_", today, ".csv")
