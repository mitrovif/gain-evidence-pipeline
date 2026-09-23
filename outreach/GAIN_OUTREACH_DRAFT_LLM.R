# ==============================================================================
# GAIN OUTREACH - relationship-aware email generator  (v11, one email per ORG)
#
# The scrape supplies WHAT we found; the sample file supplies WHO they are. Every
# recipient here is a KNOWN contact (sample file + 2025 acknowledgements), not a
# cold lead. Two LLM jobs only: (3) reword a VERIFIED facts object into an opener,
# (4) one hedged sentence about the material. Everything else is assembled in code.
#   sections keyed to the spec: 1 joins | 2 To/Cc + salutation | 3 opener facts |
#   4 body sentence | 5 fixed blocks | 6 config | 7 filters/holds | 8 style | 9 output
# ==============================================================================
suppressMessages({ library(tidyverse); library(readxl); library(jsonlite) })
source("shared/GAIN_CONFIG.R"); source("shared/GAIN_COMMON.R")
suppressMessages(source("shared/GAIN_OLLAMA_HELPERS.R"))
if (!ollama_available()) stop("Local LLM (LM Studio) not reachable - start the server first.")
if (is.na(CONTACT_XLSX)) stop("Contact workbook not found - fix CONTACT_XLSX in GAIN_CONFIG.R.")
stopifnot(is.numeric(MINUTES_PER_EXAMPLE), is.numeric(SURVEY_YEAR), is.numeric(N_FOCAL_POINTS), is.numeric(N_ORGS))
ACK_DOCX <- file.path(GAIN_ROOT, "EGRISS GAIN Survey 2025", "11 Reporting", "01 GAIN Tables",
                      "01 GAIN Table Acknowlegment", "Acknowledgments GAIN - 2025.docx")
EMAIL_MAX <- suppressWarnings(as.integer(Sys.getenv("GAIN_EMAIL_MAX", "100000")))
ONLY_CTRY <- Sys.getenv("GAIN_EMAIL_COUNTRY", "")
message(sprintf("Survey object: GAIN %d | dates %s | %d min/example | social proof %d focal points / %d orgs",
                SURVEY_YEAR, if (nzchar(SURVEY_OPEN)) paste(SURVEY_OPEN, "to", SURVEY_CLOSE) else "TBC (blank)",
                MINUTES_PER_EXAMPLE, N_FOCAL_POINTS, N_ORGS))

# ---- style / text helpers (section 8) ----------------------------------------
nd <- function(s) gsub("[[:space:]]+", " ", gsub("\\s*[\u2014\u2013]\\s*", ", ", coalesce(as.character(s), "")))
propercase <- function(s) gsub("(^|[ '-])([[:lower:]])", "\\1\\U\\2", str_squish(coalesce(as.character(s), "")), perl = TRUE)
.FOLD <- c("à"="a","á"="a","â"="a","ä"="a","å"="a","ã"="a","ç"="c","è"="e","é"="e","ê"="e","ë"="e",
           "ì"="i","í"="i","î"="i","ï"="i","ñ"="n","ò"="o","ó"="o","ô"="o","ö"="o","õ"="o","ø"="o",
           "ù"="u","ú"="u","û"="u","ü"="u","ý"="y","ÿ"="y","ř"="r","š"="s","ž"="z","č"="c","ć"="c","ę"="e","ł"="l","ń"="n","ū"="u","ī"="i")
fold <- function(s) { s <- tolower(coalesce(as.character(s), "")); for (k in names(.FOLD)) s <- gsub(k, .FOLD[[k]], s, fixed = TRUE)
  str_squish(gsub("[^a-z ]", " ", s)) }

# ---- org / domain matching (section 1) ---------------------------------------
.ORG_STOP <- c("the","of","and","for","de","la","le","les","du","des","und","der","van","el","los","las","office","bureau","agency","department","ministry")
norm_tokens <- function(s) { x <- gsub("[^a-z0-9 ]"," ", tolower(coalesce(as.character(s),""))); t <- str_split(str_squish(x)," ")[[1]]
  setdiff(unique(t[nchar(t)>1]), .ORG_STOP) }
STAT_TOK <- c("statistic","statistics","statistical","statistik","statistique","estadistica","census","nso","insee","instat","destatis","ssb","cbs","ine","scb","statbel","dosm")
# returns list(idx, score, weak) matching prod org -> one of `cand`
org_match <- function(prod, cand, is_nso = FALSE) {
  pt <- norm_tokens(prod); if (!length(pt)) return(list(idx = NA_integer_, score = 0, weak = TRUE))
  sc <- vapply(cand, function(c) { ct <- norm_tokens(c); if (!length(ct)) return(0)
    j <- length(intersect(pt, ct)) / length(union(pt, ct)); if (is_nso && any(ct %in% STAT_TOK)) j <- j + 0.30; j }, numeric(1))
  i <- which.max(sc)
  if (max(sc) >= 0.30) list(idx = i, score = max(sc), weak = max(sc) < 0.34) else list(idx = NA_integer_, score = max(sc), weak = TRUE)
}
domain_of <- function(url) { h <- tolower(coalesce(str_match(coalesce(url,""),"^https?://([^/]+)")[,2],""))
  sub(":\\d+$","",sub("^(www\\d*|blog|data|stats?|statistics|statbank|portal|old|new|en|fr|es|ec|epp|apps?|opendata)\\.","", h)) }
AGG_DOMAINS <- "europa\\.eu|eurostat|\\bun\\.org|unhcr\\.org|unstats|worldbank\\.org|oecd\\.org|who\\.int|iom\\.int|ilo\\.org|imf\\.org|ihsn\\.org"
on_own_domain <- function(dom, own) { ok <- nzchar(dom) & nzchar(own); res <- rep(FALSE, length(dom)); idx <- which(ok)
  if (length(idx)) res[idx] <- mapply(function(d,o) grepl(o,d,fixed=TRUE) || grepl(d,o,fixed=TRUE), dom[idx], own[idx]); res }
decaps <- function(s) vapply(s, function(x) { if (is.na(x) || !nzchar(x)) return(x)
  w <- strsplit(x, " ", fixed = TRUE)[[1]]; w <- ifelse(grepl("^[[:upper:]][[:upper:][:digit:]\\-]{5,}$", w), tolower(w), w)
  x2 <- paste(w, collapse = " "); paste0(toupper(substr(x2,1,1)), substr(x2,2,nchar(x2))) }, character(1), USE.NAMES = FALSE)
is_generic_inbox <- function(email) grepl("^(info|contact|contacts|enquir|enquiries|statistics?|admin|office|mail|general|press|media|comms|communication)@", tolower(coalesce(email,"")))

# section 8: real office names, not the ALLCAPS workbook label. Canonical short names
# for the offices in scope; otherwise title-case the raw name but keep acronyms upper.
NSO_NAME <- c("albania"="INSTAT (Albania)", "canada"="Statistics Canada", "germany"="Destatis",
  "netherlands"="Statistics Netherlands (CBS)", "norway"="Statistics Norway (SSB)",
  "poland"="Statistics Poland (GUS)", "sweden"="Statistics Sweden (SCB)",
  "uganda"="Uganda Bureau of Statistics (UBOS)", "latvia"="Central Statistical Bureau of Latvia",
  "spain"="INE (Instituto Nacional de Estadistica)", "malaysia"="Department of Statistics Malaysia (DOSM)",
  "\\blao"="Lao Statistics Bureau (LSB)", "belgium"="Statbel", "france"="INSEE",
  "ireland"="Central Statistics Office (CSO)", "italy"="Istat",
  "rwanda"="National Institute of Statistics of Rwanda (NISR)",
  "serbia"="Statistical Office of the Republic of Serbia",
  "switzerland"="Swiss Federal Statistical Office (FSO)", "thailand"="National Statistical Office of Thailand")
titlecase_org <- function(x) { x <- str_squish(coalesce(x, "")); if (!nzchar(x)) return(x)
  w <- str_split(x, " ")[[1]]
  w <- ifelse(grepl("^\\(?[[:upper:]]{2,}\\)?[.,]?$", w), w, str_to_title(tolower(w)))
  paste(w, collapse = " ") }
org_display <- function(country, raw) {
  k <- names(NSO_NAME)[vapply(names(NSO_NAME), function(kk) grepl(kk, tolower(country)), logical(1))]
  if (length(k)) unname(NSO_NAME[k[1]]) else titlecase_org(raw) }

# ---- HTML helpers ------------------------------------------------------------
html_esc <- function(s) { s <- coalesce(as.character(s),""); s <- gsub("&","&amp;",s,fixed=TRUE); gsub(">","&gt;",gsub("<","&lt;",s,fixed=TRUE),fixed=TRUE) }
alink <- function(url, text) ifelse(nzchar(coalesce(url,"")), sprintf('<a href="%s">%s</a>', html_esc(url), html_esc(text)), html_esc(text))
p_ <- function(...) paste0("<p>", ..., "</p>")
EMAIL_FONT <- 'font-family:Aptos,Calibri,"Segoe UI",Helvetica,Arial,sans-serif;font-size:11pt;line-height:1.45'   # section 8

# ==============================================================================
# ACKNOWLEDGEMENTS (section 1): parse docx -> set of folded "first last" names
# ==============================================================================
ack_names <- local({
  z <- tryCatch({ con <- unz(ACK_DOCX, "word/document.xml"); on.exit(close(con))
                  paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "") }, error = function(e) "")
  if (!nzchar(z)) { message("WARNING: acknowledgements docx unreadable - no 'acknowledged' facts"); return(character(0)) }
  paras <- strsplit(z, "</w:p>")[[1]]
  txt <- vapply(paras, function(p) { m <- regmatches(p, gregexpr("<w:t[^>]*>[^<]*</w:t>", p))[[1]]
    paste(gsub("<[^>]+>", "", m), collapse = "") }, character(1))
  txt <- trimws(txt); keep <- nzchar(txt) & grepl(",", txt) & !grepl("ACKNOWLEDG", txt, ignore.case = TRUE)
  unique(fold(sub(",.*$", "", txt[keep])))                         # person name = before first comma
})
message(sprintf("acknowledgements: %d named individuals loaded", length(ack_names)))

# ==============================================================================
# EVENT GAZETTEER + FACT PRECEDENCE (section 3)
# ==============================================================================
# category token -> event display, city, when, recency order (1 = most recent)
MEET <- list(
  "Workshop attendees, Kenya"                  = list(event="the EGRISS validation meeting and training in Nairobi", city="Nairobi", when="March 2026",   ord=1),
  "All Members Meeting 2025, Poland"           = list(event="the All Members Meeting in Warsaw",                      city="Warsaw",  when="October 2025", ord=2),
  "4th African School on Migration Statistics" = list(event="the 4th African School on Migration Statistics in Abidjan", city="Abidjan", when="May 2025",  ord=3),
  "Workshop attendees, Cairo"                  = list(event="the UN ESCWA regional workshop in Cairo",                city="Cairo",   when="November 2024",ord=4),
  "Expert Meeting, Geneva"                     = list(event="the UNECE-EGRISS regional workshop in Geneva",           city="Geneva",  when="May 2024",     ord=5),
  "Workshop attendees, Bangkok"                = list(event="the Asia-Pacific regional workshop in Bangkok",          city="Bangkok", when="March 2024",   ord=6))
GAZ_CITIES <- unname(vapply(MEET, function(m) m$city, character(1)))
GAZ_YEARS  <- c("2023","2024","2025","2026")

# From one contact's Categories-of-Respondents tokens + acknowledged flag, choose
# up to TWO facts (primary by precedence; secondary prefers an in-person meeting).
select_facts <- function(cats, acked, org) {
  has <- function(p) any(grepl(p, cats, fixed = TRUE))
  not_reported <- has("Members who have not participated in 2023 GAIN Survey") &&
                  !has("GAIN 2025") && !has("GAIN 2024") && !has("GAIN 2023")
  best_meet <- { ms <- names(MEET)[vapply(names(MEET), has, logical(1))]
                 if (length(ms)) MEET[[ms[order(vapply(ms, function(m) MEET[[m]]$ord, numeric(1)))][1]]] else NULL }
  # (rank, type, template key, fact list element)
  pool <- list()
  add <- function(rank, type, tmpl, fact) pool[[length(pool)+1]] <<- list(rank=rank, type=type, tmpl=tmpl, fact=fact, meeting=(type=="meeting"))
  if (acked)            add(1, "acknowledged", "acknowledged", list(type="acknowledged", round="2025"))
  if (has("GAIN 2025")) add(2, "reported",     "gain2025",     list(type="reported", round="2025"))
  if (!is.null(best_meet)) add(3, "meeting",   "meeting",      list(type="meeting", event=best_meet$event, city=best_meet$city, when=best_meet$when))
  if (has("GAIN 2024")) add(4, "reported",     "gain2024",     list(type="reported", round="2024"))
  if (has("GAIN 2023")) add(5, "reported",     "gain2023",     list(type="reported", round="2023"))
  if (has("GAIN 2025 (Additional Focal Points)")) add(6, "addl", "addl", list(type="additional_focal_point", round="2025"))
  if (has("Inclusion Pledge") || has("GRF"))      add(7, "pledge", "pledge", list(type="inclusion_pledge_focal_point"))
  if (has("E-Learning"))                          add(8, "elearning", "elearning", list(type="completed_elearning"))
  if (has("EGRISS Active Member") || has("Active Member")) add(9, "member", if (not_reported) "member_never" else "member", list(type="egriss_member"))
  if (!length(pool)) return(list(rank=10, tmpl="none", facts=list(), city_allow=character(0), year_allow=character(0)))
  ord <- order(vapply(pool, function(x) x$rank, numeric(1)))
  pool <- pool[ord]
  primary <- pool[[1]]
  # secondary: ONLY an in-person meeting (the most personal fact, and it cannot be
  # merged into a wrong year). Two reporting rounds as facts led the model to fuse
  # them ("both rounds in 2025"), so a non-meeting secondary is not used.
  sec <- NULL
  meet_i <- which(vapply(pool, function(x) x$meeting, logical(1)))
  if (length(meet_i) && !primary$meeting) sec <- pool[[meet_i[1]]]
  chosen <- if (is.null(sec)) list(primary) else list(primary, sec)
  facts <- lapply(chosen, function(x) x$fact)
  city_allow <- unlist(lapply(facts, function(f) if (!is.null(f$city)) f$city))
  year_allow <- unlist(lapply(facts, function(f) if (!is.null(f$round)) f$round else if (!is.null(f$when)) str_extract(f$when, "\\d{4}")))
  list(rank = primary$rank, tmpl = primary$tmpl, facts = facts,
       city_allow = if (is.null(city_allow)) character(0) else unique(city_allow),
       year_allow = if (is.null(year_allow)) character(0) else unique(year_allow))
}

# English fallback opener (section 3c). Uses the literal token @@ORG@@ where the
# organisation name goes, so the name survives translation intact (it is substituted
# AFTER translate_text). Returns the placeholdered English master.
fallback_opener <- function(tmpl, facts) {
  m <- Find(function(f) identical(f$type, "meeting"), facts)
  switch(tmpl,
    acknowledged  = "Thank you for reporting to GAIN last year. Your name is in the acknowledgements for the 2025 round.",
    gain2025      = "Thank you for reporting to GAIN last year.",
    gain2024      = "Thank you for reporting to GAIN in 2024.",
    gain2023      = "You reported to GAIN in 2023, and we would like to have @@ORG@@ in the record again this year.",
    addl          = "You were listed as an additional focal point for GAIN in 2025.",
    meeting       = if (!is.null(m)) sprintf("It was good to see @@ORG@@ represented at %s, %s.", m$event, m$when) else "@@ORG@@ is an active member of EGRISS.",
    pledge        = "As a focal point for the Statistical Inclusion Pledge, you will know this work already.",
    elearning     = "Thank you for completing the EGRISS e-learning course.",
    member        = "@@ORG@@ is an active member of EGRISS.",
    member_never  = "@@ORG@@ is an active member of EGRISS, though our records show the office has not yet reported to GAIN. That is the gap we are hoping to close this year.",
    "Given your office's work in this field, we thought this might be of interest.")
}

# ==============================================================================
# 1 · WHAT WE FOUND - scrape examples, filtered (section 7)
# ==============================================================================
nso <- suppressMessages(read_csv("NSO_Full_Registry.csv", show_col_types = FALSE)) %>%
  transmute(country = harmonize_country(country), nso_domain = domain) %>% distinct(country, .keep_all = TRUE)
fin <- tail(sort(list.files(".", "^GAIN_EVIDENCE_FINAL_.*\\.csv$")), 1)
d <- suppressMessages(read_csv(fin, show_col_types = FALSE))
pk <- function(c) if (c %in% names(d)) d[[c]] else rep(NA, nrow(d))
ex0 <- tibble(country = harmonize_country(as.character(pk("country"))),
  prod_org = str_squish(coalesce(as.character(pk("llm_organization")), as.character(pk("producer")))),
  title = decaps(coalesce(as.character(pk("llm_instrument_or_title")), as.character(pk("title")))),
  year = as.character(pk("llm_year")), quote = as.character(pk("llm_quote")),
  url = coalesce(as.character(pk("url")), as.character(pk("Found_On_Page"))),
  tier = as.character(pk("final_tier")), conf = tolower(coalesce(as.character(pk("final_confidence")), ""))) %>%
  filter(tier == "reach out", !is.na(country), nzchar(coalesce(prod_org, ""))) %>%
  left_join(nso, by = "country") %>%
  mutate(domain = map_chr(url, domain_of), own_dom = coalesce(nso_domain, ""),
         title_key = str_squish(tolower(coalesce(title, "")))) %>%
  group_by(title_key) %>% mutate(n_org_title = n_distinct(domain[nzchar(domain)])) %>% ungroup() %>%
  mutate(excl_reason = case_when(
    nzchar(title_key) & n_org_title >= 3          ~ "aggregator title (shared by 3+ offices)",
    str_detect(domain, AGG_DOMAINS)               ~ "aggregator / supranational domain",
    nzchar(own_dom) & nzchar(domain) & !on_own_domain(domain, own_dom) ~ "source URL not on office own domain",
    TRUE ~ ""))
if (nzchar(ONLY_CTRY)) { cc <- harmonize_country(str_trim(str_split(ONLY_CTRY, ",")[[1]])); ex0 <- ex0 %>% filter(country %in% cc) }
ex_review <- ex0 %>% filter(nzchar(excl_reason)) %>% transmute(country, organisation = prod_org, title, year, url, reason = excl_reason)
ex <- ex0 %>% filter(!nzchar(excl_reason)) %>% mutate(is_nso = on_own_domain(domain, own_dom))
message(sprintf("examples: %d kept, %d routed to review (aggregator/off-domain)", nrow(ex), nrow(ex_review)))

# ==============================================================================
# 1 · WHO THEY ARE - sample file contacts, bounced suppression
# ==============================================================================
truthy <- function(x) tolower(as.character(x)) %in% c("true","yes","1","y","member")
bounced <- tryCatch(suppressMessages(read_excel(CONTACT_XLSX, sheet = "Bounced Email Log")) %>%
  transmute(email = tolower(str_trim(`Bounced Email`))) %>% filter(nzchar(email)) %>% pull(email),
  error = function(e) character(0))
con <- suppressMessages(read_excel(CONTACT_XLSX, sheet = "Sample Survey 2026")) %>%
  transmute(country = harmonize_country(Country), org_id = as.character(`NEW_ORG_ID`), org = `NEW_ORG_NAME`,
            first = propercase(`First Name`), surname = propercase(`Last Name`),
            name = str_squish(paste(propercase(`First Name`), propercase(`Last Name`))),
            priority = suppressWarnings(as.integer(Priority)),
            email = tolower(str_trim(str_split_fixed(coalesce(`Email (combined)`,""),"[;,]",2)[,1])),
            cats = coalesce(`Categories of Respondents`, ""), egriss = truthy(`Is EGRISS Member`)) %>%
  filter(!is.na(email), nzchar(email), !is.na(org), !is.na(org_id)) %>%
  filter(!(email %in% bounced)) %>%
  mutate(acked = fold(name) %in% ack_names,
         name_check = mapply(function(e, s) { fs <- fold(s); nzchar(fs) && !grepl(fs, fold(e), fixed = TRUE) },
                             email, surname))
# distinct organisations present in the sample (for org-name -> NEW_ORG_ID matching)
orgs <- con %>% distinct(country, org_id, org)
message(sprintf("contacts: %d usable (bounced dropped: %d) across %d organisations",
                nrow(con), length(bounced), n_distinct(con$org_id)))

# ==============================================================================
# 1 · JOIN: match each example's org -> a sample NEW_ORG_ID within the country
# ==============================================================================
match_org <- function(country, prod_org, is_nso) {
  oc <- orgs %>% filter(country == !!country)
  if (!nrow(oc)) return(list(org_id = NA_character_, weak = TRUE))
  m <- org_match(prod_org, oc$org, is_nso = is_nso)
  if (is.na(m$idx)) list(org_id = NA_character_, weak = TRUE)
  else list(org_id = oc$org_id[m$idx], weak = m$weak)
}
exj <- ex %>% mutate(mm = pmap(list(country, prod_org, is_nso), match_org),
                     org_id = map_chr(mm, "org_id"), weak_match = map_lgl(mm, "weak")) %>% select(-mm)
matched <- exj %>% filter(!is.na(org_id))
unmatched <- exj %>% filter(is.na(org_id))                     # org not in sample -> HOLD (section 7)

# ==============================================================================
# 5 · fixed blocks (English masters, dash-free), localised via translate_text
# ==============================================================================
blocks_en <- function() {
  ns <- if (nzchar(SURVEY_OPEN))
      sprintf("If you confirm, we will send the GAIN questionnaire when the round opens, %s to %s. Describing one example takes about %d minutes, in %s.",
              SURVEY_OPEN, SURVEY_CLOSE, MINUTES_PER_EXAMPLE, SURVEY_LANGUAGES)
    else
      sprintf("If you confirm, we will send the GAIN questionnaire when the round opens later this year. Describing one example takes about %d minutes, in %s.",
              MINUTES_PER_EXAMPLE, SURVEY_LANGUAGES)
  reward <- sprintf(paste("In the 2025 round, %d focal points in %d organisations reported. What they report becomes",
                          "public evidence in the GAIN Tables, travels as case studies to countries facing similar",
                          "challenges, reaches the UN Statistical Commission, and guides where EGRISS directs technical support."),
                    N_FOCAL_POINTS, N_ORGS)
  lapply(list(
    found    = "What we found:",
    scope    = paste("GAIN covers statistical activities, including censuses, surveys, use of administrative data,",
                     "workshops and reports, that focus on or include refugees, internally displaced or stateless persons."),
    ask      = paste("We may well have this wrong. Could you reply and tell us whether these are yours, whether they fit,",
                     "and whether we have missed anything? One line is enough. There is no form at this stage."),
    forward  = "If this sits with a colleague, please point us to them, or forward this on.",
    nextstep = ns, reward = reward,
    sponsor  = GAIN_SPONSOR, datause = GAIN_DATA_USE, optout = GAIN_OPTOUT,
    moreabout = "More about the GAIN Survey",
    more_note = "more items found on your website; we can send you the full list on request."), nd)
}
.block_cache <- new.env()
blocks_for <- function(lang) { key <- if (nzchar(lang)) lang else "English"
  if (!is.null(.block_cache[[key]])) return(.block_cache[[key]])
  b <- blocks_en(); if (key != "English") b <- lapply(b, function(x) translate_text(x, key))
  .block_cache[[key]] <- b; b }

# ---- language, salutation (section 2), subject (section 8) --------------------
un_lang_of <- function(country) { x <- tolower(coalesce(as.character(country), ""))
  ar <- "egypt|saudi|iraq|jordan|lebanon|syria|sudan|algeria|morocco|maroc|tunisia|libya|yemen|emirates|qatar|kuwait|bahrain|\\boman\\b|palestin|maurit|somal|djibouti|comoros"
  fr <- "france|belgi|luxembourg|monaco|\\bcongo|ivoire|\\bsenegal|\\bmali\\b|\\bniger\\b|burkina|cameroon|cameroun|\\bchad\\b|tchad|\\bbenin|\\btogo|\\bgabon|central african|centrafric|madagascar|\\bhaiti|rwanda|burundi|seychelles"
  es <- "spain|espa|mexic|argentin|colombia|\\bperu|\\bchile|venezuela|ecuador|bolivia|paraguay|uruguay|guatemala|honduras|salvador|nicaragua|costa rica|\\bpanam|\\bcuba|dominican|equatorial guinea"
  ru <- "russia|belarus|kazakh|kyrgyz"; zh <- "\\bchina\\b"
  if (grepl(ar,x)) "Arabic" else if (grepl(fr,x)) "French" else if (grepl(es,x)) "Spanish" else
  if (grepl(ru,x)) "Russian" else if (grepl(zh,x)) "Chinese" else "" }
# honorific TOKEN (literal, for the sender to resolve; never a prose slash form)
HON  <- list(English="[Ms/Mr]", French="[Mme/M.]", Spanish="[Sra./Sr.]", Arabic="[السيد/ة]", Russian="[Г-н/Г-жа]", Chinese="[先生/女士]")
NEU  <- list(English="Dear Sir or Madam,", French="Madame, Monsieur,", Spanish="Señoras y señores:",
             Arabic="السيدات والسادة،", Russian="Уважаемые дамы и господа,", Chinese="尊敬的女士/先生：")
salutation_of <- function(lang, surname) {
  L <- if (lang %in% names(HON)) lang else "English"
  if (!nzchar(surname)) return(list(text = NEU[[L]], honorific = FALSE))
  h <- HON[[L]]
  txt <- switch(L,
    English = sprintf("Dear %s %s,", h, surname),
    French  = sprintf("Bonjour %s %s,", h, surname),
    Spanish = sprintf("%s %s:", h, surname),
    Arabic  = sprintf("%s %s،", h, surname),
    Russian = sprintf("Уважаемый(ая) %s %s,", h, surname),
    Chinese = sprintf("尊敬的%s%s：", h, surname))
  list(text = txt, honorific = TRUE)
}
CCLAUSE <- list(English = c("I have copied ", " and ", "."),
                French  = c("J'ai mis en copie ", " et ", "."),
                Spanish = c("Pongo en copia a ", " y ", "."))
cc_clause <- function(lang, names_v) {
  names_v <- names_v[nzchar(names_v)]; if (!length(names_v)) return("")
  parts <- if (lang %in% names(CCLAUSE)) CCLAUSE[[lang]] else CCLAUSE[["English"]]
  joined <- if (length(names_v) == 1) names_v
            else if (length(names_v) == 2) paste0(names_v[1], parts[2], names_v[2])
            else paste0(paste(head(names_v, -1), collapse = ", "), parts[2], tail(names_v, 1))
  paste0(parts[1], joined, parts[3])
}
subject_of <- function(lang, ref) {
  ref <- nd(str_squish(gsub('["\u201c\u201d\u00ab\u00bb\u300c\u300d]', "", coalesce(ref, ""))))
  ref <- sub("[[:space:]]*[.\u2026]+$", "", ref)
  if (!nzchar(ref)) ref <- "refugee and displacement statistics"
  if (lang == "Chinese") sprintf("GAIN %d\uff1a%s", SURVEY_YEAR, ref) else sprintf("GAIN %d: %s", SURVEY_YEAR, ref)
}

# ---- one language block (HTML) -----------------------------------------------
LIST_MAX <- 10L
lang_block <- function(lang, sal, opener, cc, body, ex_df, b) {
  extra <- max(0L, nrow(ex_df) - LIST_MAX); shown <- head(ex_df, LIST_MAX)
  titles <- nd(gsub("\\((\\d{4})\\)\\s*\\(\\1\\)", "(\\1)", coalesce(shown$title, ""))); yrs <- coalesce(shown$year, "")
  has_year <- mapply(function(t, y) nzchar(y) && grepl(paste0("(", y, ")"), t, fixed = TRUE), titles, yrs)
  yr_suffix <- ifelse(nzchar(yrs) & !has_year, paste0(" (", html_esc(yrs), ")"), "")
  ex_items <- paste(sprintf("<li>%s%s</li>", alink(coalesce(shown$url, ""), titles), yr_suffix), collapse = "")
  if (extra > 0) ex_items <- paste0(ex_items, sprintf("<li><em>+%d %s</em></li>", extra, html_esc(b$more_note)))
  opener_p <- if (nzchar(opener)) p_(html_esc(opener), if (nzchar(cc)) paste0(" ", html_esc(cc)) else "") else if (nzchar(cc)) p_(html_esc(cc)) else ""
  legit <- p_(sub("EGRISS", alink(GAIN_EGRISS_LINK, "EGRISS"), html_esc(b$sponsor), fixed = TRUE), " ",
              alink(GAIN_PAGE_URL, b$moreabout), ". ", html_esc(b$datause), " ", html_esc(b$optout))
  sig <- p_(html_esc(SENDER_NAME), "<br>", html_esc(SENDER_TITLE), "<br>EGRISS Secretariat, hosted by UNHCR",
            "<br>", html_esc(SENDER_PHONE), "<br>", html_esc(SENDER_EMAIL))
  paste0(p_(html_esc(sal)), opener_p, if (nzchar(body)) p_(html_esc(body)) else "",
    "<p><b>", html_esc(b$found), "</b></p><ul>", ex_items, "</ul>",
    p_(html_esc(b$scope)), p_(html_esc(b$ask)), p_(html_esc(b$forward)),
    p_(html_esc(b$nextstep)), p_(html_esc(b$reward)), legit, sig)
}

# ==============================================================================
# 2/3/4 · draft ONE email per organisation
# ==============================================================================
rows <- list(); hold <- list(); fb_log <- list()
org_ids <- matched %>% distinct(country, org_id) %>% head(EMAIL_MAX)
for (k in seq_len(nrow(org_ids))) {
  ctry <- org_ids$country[k]; oid <- org_ids$org_id[k]
  cex <- matched %>% filter(country == ctry, org_id == oid) %>% distinct(title, .keep_all = TRUE)
  cex_top <- head(cex, 4)
  weak <- any(matched$weak_match[matched$org_id == oid & matched$country == ctry])
  lowconf <- mean(cex$conf == "low", na.rm = TRUE) > 0.5

  contacts <- con %>% filter(org_id == oid)
  if (!nrow(contacts)) next
  org_disp <- org_display(ctry, contacts$org[1])
  # rank each contact by its primary fact precedence; To = best, Cc = the rest
  fx <- lapply(seq_len(nrow(contacts)), function(i) select_facts(contacts$cats[i], contacts$acked[i], org_disp))
  rank <- vapply(fx, function(x) x$rank, numeric(1))
  ord  <- order(rank, coalesce(contacts$priority, 99L), seq_len(nrow(contacts)))
  contacts <- contacts[ord, ]; fx <- fx[ord]
  to <- contacts[1, ]; to_fx <- fx[[1]]; ccs <- if (nrow(contacts) > 1) contacts[-1, ] else contacts[0, ]

  lang <- un_lang_of(ctry); b <- blocks_for(lang); b_en <- blocks_for("English")

  # SECTION 3: opener from verified facts (LLM in English, validated) with template
  # fallback. The English opener carries no org name (the prompt forbids restating it),
  # so its translation is clean; a fallback keeps the org as @@ORG@@ until after translation.
  # No name in the JSON: the salutation carries the name, and a 7B model given a
  # name will restate it ("We acknowledge Jan Eberle's ..."). Facts only.
  facts_json <- toJSON(list(facts = to_fx$facts), auto_unbox = TRUE)
  facts_txt <- paste(vapply(to_fx$facts, function(f) paste(names(f), unlist(f), sep="=", collapse=", "), character(1)), collapse=" | ")
  op <- draft_opener(facts_json, facts_txt, org_disp, "English",
                     allow_years = to_fx$year_allow, allow_places = to_fx$city_allow,
                     gaz_years = GAZ_YEARS, gaz_places = GAZ_CITIES)
  fell <- op$fell_back || !nzchar(op$text)
  sub_org <- function(s) gsub("@@ORG@@", org_disp, s, fixed = TRUE)
  if (fell) {
    tmpl_ph <- fallback_opener(to_fx$tmpl, to_fx$facts)              # English master, org = @@ORG@@
    open_en  <- nd(sub_org(tmpl_ph))
    open_loc <- if (nzchar(lang)) nd(sub_org(translate_text(tmpl_ph, lang))) else ""
    fb_log[[length(fb_log)+1]] <- tibble(country=ctry, organisation=org_disp, to=to$name,
                                         template=to_fx$tmpl, facts=facts_txt)
  } else {
    open_en  <- nd(op$text)
    open_loc <- if (nzchar(lang)) nd(translate_text(open_en, lang)) else ""
  }

  # SECTION 4: body sentence about the material only
  em <- draft_outreach_email(country=ctry, organization=org_disp, examples=cex_top, translate_to=lang)
  bilingual <- nzchar(lang) && nzchar(em$para_local)

  # SECTION 2: salutation + cc clause
  sal_en <- salutation_of("English", to$surname)
  cc_en  <- cc_clause("English", ccs$name)
  en_block <- lang_block("English", sal_en$text, open_en, cc_en, em$para_en, cex, b_en)
  if (bilingual) {
    sal_loc <- salutation_of(lang, to$surname); cc_loc <- cc_clause(lang, ccs$name)
    loc_block <- lang_block(lang, sal_loc$text, open_loc, cc_loc, em$para_local, cex, b)
    email_html <- paste0(loc_block, "<hr style=\"border:none;border-top:1px solid #ccc;margin:22px 0\">", en_block)
    subj_lang <- lang
  } else { email_html <- en_block; subj_lang <- "English" }
  email_html <- sprintf('<div style="%s">%s</div>', EMAIL_FONT, email_html)   # section 8 typography

  subj <- subject_of(subj_lang, subject_referent(cex_top, org_disp, if (bilingual) lang else "English"))

  # SECTION 9: flags
  flags <- c(if (to_fx$tmpl == "none") "[NO RELATIONSHIP FOUND]",
             if (isTRUE(weak)) "[WEAK CONTACT MATCH]",
             if (isTRUE(sal_en$honorific)) "[Ms/Mr UNRESOLVED]",
             if (any(c(to$name_check, ccs$name_check))) "[NAME CHECK]",
             if (isTRUE(lowconf)) "[WEAK SCOPE MATCH]",
             if (isTRUE(fell)) "[OPENER FELL BACK TO TEMPLATE]",
             if (isTRUE(em$flagged)) "[BODY SENTENCE NEEDS CHECK]",
             if (nrow(cex) > LIST_MAX) sprintf("[LONG LIST - %d EXAMPLES]", nrow(cex)),
             if (bilingual) "[NEEDS NATIVE CHECK]")
  rows[[length(rows)+1]] <- tibble(country=ctry, institution=org_disp, n_examples=nrow(cex),
    to_name=to$name, to_email=to$email, cc_names=paste(ccs$name, collapse="; "), cc_emails=paste(ccs$email, collapse="; "),
    primary_fact=to_fx$tmpl, subject=subj, subject_language=subj_lang, language_sent=em$local_language,
    flags=paste(flags, collapse=" "), email_html=email_html, status=em$status,
    replied="", example_status="", nominated_focal_point="", nominated_address="")
  message(sprintf("  [%d/%d] %s | %s | To %s (%s) | +%d cc | %d ex | %s",
                  k, nrow(org_ids), ctry, substr(org_disp,1,24), to$name, to_fx$tmpl, nrow(ccs), nrow(cex), if(nzchar(lang)) lang else "en"))
}

# ---- HOLD: orgs whose scraped producer is not in the sample (section 7) ------
if (nrow(unmatched)) {
  hold <- unmatched %>% group_by(country, prod_org) %>%
    summarise(n_examples = n(), website = { w <- domain[nzchar(domain)]; if (length(w)) w[1] else "" }, .groups = "drop") %>%
    # split any concatenated multi-org record on strong delimiters
    mutate(prod_org = str_split(prod_org, "\\s*(?:/|;|\\||\\+|\\n| & )\\s*")) %>% unnest(prod_org) %>%
    mutate(prod_org = str_squish(prod_org)) %>% filter(nzchar(prod_org)) %>%
    transmute(country, institution = prod_org, n_examples, website,
              suggested_contact_page = ifelse(nzchar(website), paste0("https://", website), ""), reason = "no verified address")
}
out <- bind_rows(rows); holds <- if (is.data.frame(hold)) hold else bind_rows(hold); fb <- bind_rows(fb_log)
today <- format(Sys.Date(), "%Y%m%d")

# section 2 assertion: no address appears in more than one draft
if (nrow(out)) { addr <- tolower(str_squish(unlist(str_split(paste(out$to_email, out$cc_emails, sep=";"), ";"))))
  addr <- addr[nzchar(addr)]; dup <- unique(addr[duplicated(addr)])
  if (length(dup)) warning(sprintf("address in >1 draft: %s", paste(dup, collapse=", "))); stopifnot(length(dup) == 0) }

readr::write_excel_csv(out, sprintf("outreach_emails_llm_%s.csv", today))
if (nrow(holds)) readr::write_excel_csv(holds, sprintf("outreach_HOLD_%s.csv", today))
if (nrow(ex_review)) readr::write_excel_csv(ex_review, sprintf("outreach_review_excluded_%s.csv", today))
if (nrow(fb)) readr::write_excel_csv(fb, sprintf("outreach_opener_fallback_log_%s.csv", today))
samp <- out %>% group_by(language_sent) %>% slice(1) %>% ungroup()
readr::write_excel_csv(samp %>% select(language_sent, country, institution, subject, email_html),
                       sprintf("outreach_samples_by_language_%s.csv", today))

fb_rate <- if (nrow(out)) round(nrow(fb) / nrow(out), 2) else 0
message(sprintf("\n==== GAIN %d outreach ====", SURVEY_YEAR))
message(sprintf("organisations drafted : %d", nrow(out)))
message(sprintf("on HOLD (no address)  : %d", nrow(holds)))
message(sprintf("examples excluded     : %d", nrow(ex_review)))
message(sprintf("opener fallback rate  : %.0f%% (%d of %d)%s", 100*fb_rate, nrow(fb), nrow(out),
                if (fb_rate > 0.2) "  <-- >1 in 5: check the opener prompt, not the model" else ""))
