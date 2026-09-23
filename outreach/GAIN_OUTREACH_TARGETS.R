# ==============================================================================
# GAIN OUTREACH TARGETS + EMAIL DRAFTS
#
# Joins your NSO contact list to the NOT-YET-IN-GAIN examples and produces:
#   outreach_targets_[date].csv   every new example + its assigned contact (full)
#   outreach_emails_[date].csv     one ready-to-edit draft email per country/contact
#   outreach_contact_gaps_[date].csv  new-example countries with NO contact (hunt)
#   powerbi_export/WEB_GAIN_outreach.csv   per-country outreach coverage for Power BI
#
# It DRAFTS emails only - it never sends anything. Review/edit, then mail-merge.
# Pure add-on: reads evidence_flagged_*.csv (crossref output) + your contact xlsx.
# Nothing in the working pipeline is changed.
# ==============================================================================

suppressMessages({ library(tidyverse); library(readxl) })
source("shared/GAIN_CONFIG.R")   # SENDER_NAME, GAIN_SURVEY_LINK, deadline, CONTACT_XLSX (shared)
source("shared/GAIN_COMMON.R")   # harmonize_country() etc. (shared - do not redefine locally)
stamp <- format(Sys.Date(), "%Y%m%d")

# ------------------------------------------------------------------------------
# Script-specific knobs (campaign-wide settings live in GAIN_CONFIG.R)
# ------------------------------------------------------------------------------
MAX_EXAMPLES_PER_EMAIL <- 6    # feature the top N per country; the rest are in the targets file
EMAIL_MIN_SCORE        <- 45   # only FEATURE examples this strong (or category A/B/C) in an email
MAX_CC                 <- 3    # also CC up to this many other contacts at the SAME NSO
SHOW_QUOTES            <- TRUE  # show the verbatim evidence quote under an example (when available)
GREETING_STYLE         <- "first"  # "first" (Dear Jane,) or "full" (Dear Jane Doe,)

if (is.na(CONTACT_XLSX))
  stop("Contact workbook not found - copy it into this folder or fix CONTACT_XLSX in GAIN_CONFIG.R.")

# clean scraped titles: strip HTML, en/em dashes -> hyphen, collapse, truncate
clean_title <- function(x) {
  x <- coalesce(x, "")
  x <- str_replace_all(x, "<[^>]+>", " ")     # complete HTML tags
  x <- str_replace_all(x, "<[^>]*$", " ")     # trailing unclosed tag (e.g. truncated <a href=...)
  x <- str_replace_all(x, "&[a-zA-Z#0-9]+;", " ")
  x <- str_replace_all(x, "[–—]", "-")        # en/em dash -> hyphen
  str_trunc(str_squish(x), 110)
}

# CONTACT_XLSX comes from GAIN_CONFIG.R; harmonize_country() from GAIN_COMMON.R
# (the local copy this script used to carry had already drifted from the
# crossref's - it was missing "Moldova (the Republic of)" - which is exactly
# why the shared version exists).

# ------------------------------------------------------------------------------
# 1. NEW examples (not yet in GAIN) from the crossref output
# ------------------------------------------------------------------------------
# Prefer the FINALIZED file (carries final_tier = the reach-out decision); fall
# back to the raw crossref output if finalize has not been run yet.
fin_f <- list.files(pattern = "GAIN_EVIDENCE_FINAL_"); fin_f <- fin_f[endsWith(fin_f, ".csv")]
ef_f  <- list.files(pattern = "evidence_flagged_");    ef_f  <- ef_f[endsWith(ef_f, ".csv")]
src_f <- if (length(fin_f)) fin_f[which.max(file.info(fin_f)$mtime)] else ef_f[which.max(file.info(ef_f)$mtime)]
message("outreach source: ", src_f)
ev   <- read_csv(src_f, show_col_types = FALSE)
gc   <- function(c, d=NA) if (c %in% names(ev)) ev[[c]] else rep(d, nrow(ev))

new_ex <- tibble(
  country_h  = harmonize_country(if ("Country" %in% names(ev)) ev$Country else ev$country),
  title      = clean_title(coalesce(gc("Report_Title"), gc("title"))),
  url        = coalesce(gc("Found_On_Page"), gc("url")),
  year       = as.character(coalesce(gc("year"), gc("Publication_Date"))),
  quote      = clean_title(gc("llm_quote")),
  score      = suppressWarnings(as.numeric(gc("relevance_score"))),
  category   = gc("outreach_category"),
  lead       = gc("candidate_lead_type"),
  match_type = gc("gain_match_type"),
  match_why  = gc("gain_match_reason"),
  final_tier = gc("final_tier"),
  llm_counted = gc("llm_counted"),
  populations = coalesce(gc("populations"), gc("Populations"))
) %>%
  # Drive outreach off the combined verdict when it exists: only the "reach out"
  # tier (AI-confirmed, country-led, not-in-GAIN). Fall back to the old
  # match-type filter if the finalize step has not produced final_tier yet.
  filter(if (any(!is.na(final_tier))) coalesce(final_tier == "reach out", FALSE)
         else match_type %in% c("new_example_existing_country", "new_country")) %>%
  mutate(score = coalesce(score, 0),
         llm_counted = suppressWarnings(as.logical(llm_counted)),
         cat_letter = str_sub(coalesce(category, "?"), 1, 1),
         is_strong  = cat_letter %in% c("A","B","C") | score >= EMAIL_MIN_SCORE)
message(sprintf("New examples: %d across %d countries (from %s)",
                nrow(new_ex), n_distinct(new_ex$country_h), ef_f))

# ------------------------------------------------------------------------------
# 2. Contacts (+ bounce exclusion + personalized-intro mapping)
# ------------------------------------------------------------------------------
con <- read_excel(CONTACT_XLSX, sheet = "Sample Survey 2026") %>%
  transmute(
    first = `First Name`, last = `Last Name`,
    country_h = harmonize_country(Country),
    org = `NEW_ORG_NAME`, position = Position,
    email = str_trim(str_split_fixed(coalesce(`Email (combined)`, ""), "[;,]", 2)[,1]),
    priority = suppressWarnings(as.numeric(Priority)),
    top_cat = `Top Category`,
    is_member = `Is EGRISS Member`) %>%
  filter(!is.na(email), email != "")

bounced <- tryCatch(read_excel(CONTACT_XLSX, sheet = "Bounced Email Log")$`Bounced Email`,
                    error = function(e) character(0))
# also exclude addresses that bounced in our own campaign (from GAIN_OUTREACH_SYNC)
bmaster <- if (file.exists("GAIN_bounced_master.csv"))
  tryCatch(read_csv("GAIN_bounced_master.csv", show_col_types = FALSE)$email, error = function(e) character(0)) else character(0)
all_bounced <- str_to_lower(c(coalesce(bounced, ""), coalesce(bmaster, "")))
con <- con %>% filter(!str_to_lower(email) %in% all_bounced)
message(sprintf("Contacts: %d usable (after %d bounced excluded), %d countries",
                nrow(con), length(bounced), n_distinct(con$country_h)))

# personalized intro line per Top Category (lowercase start; flows into the email)
intro_for <- function(tc) {
  t <- str_to_lower(coalesce(tc, ""))
  case_when(
    str_detect(t, "e-learning")                    ~ "Following your completion of the EGRISS e-learning course",
    str_detect(t, "gain 20")                        ~ "Following your participation in the GAIN survey",
    str_detect(t, "cairo")                          ~ "Following the UNESCWA regional workshop in Cairo",
    str_detect(t, "kenya|nairobi")                  ~ "Following the validation meeting in Nairobi",
    str_detect(t, "bangkok|asia")                   ~ "Following the Asia-Pacific regional workshop in Bangkok",
    str_detect(t, "african school|migration stat")  ~ "Following the African School on Migration Statistics in Abidjan",
    str_detect(t, "geneva")                         ~ "Following the UNECE-EGRISS regional workshop in Geneva",
    str_detect(t, "poland|warsaw|all members")      ~ "Following the All Members Meeting in Warsaw",
    str_detect(t, "pledge|grf")                     ~ "As a focal point for the GRF Statistical Inclusion Pledge",
    str_detect(t, "member|focal|hlsc")              ~ "As an EGRISS member",
    TRUE                                            ~ "Given your work in official statistics")
}

# ------------------------------------------------------------------------------
# 3. Pick the best contact per country: stats/migration role > EGRISS member > priority
# ------------------------------------------------------------------------------
ROLE_PAT <- "statist|population|demograph|migration|census|social statistics|vital|director general|chief statist"
con_ranked <- con %>%
  mutate(role_fit = as.integer(str_detect(str_to_lower(coalesce(position, "")), ROLE_PAT)),
         member_fit = as.integer(str_to_lower(coalesce(is_member, "")) == "yes")) %>%
  group_by(country_h) %>%
  arrange(desc(role_fit), desc(member_fit), priority, .by_group = TRUE) %>%
  mutate(rank_in_country = row_number()) %>% ungroup()

primary <- con_ranked %>% filter(rank_in_country == 1)

# ------------------------------------------------------------------------------
# STICKY ASSIGNMENT: once a country has a contact in GAIN_OUTREACH_LOG.csv, keep
# using that contact on every re-run. Without this, ranking ties (broken only by
# the contact workbook's row order) could pick a DIFFERENT "best contact" next
# run - and since the log keys rows on country|email, the old row's human notes
# (status, comments, response_summary...) would be silently orphaned. A sticky
# contact only changes if it becomes unavailable (bounced / removed from the
# workbook), in which case we fall back to the freshly ranked pick and say so.
# ------------------------------------------------------------------------------
if (file.exists("GAIN_OUTREACH_LOG.csv")) {
  prev <- read_csv("GAIN_OUTREACH_LOG.csv", show_col_types = FALSE) %>%
    transmute(country_h = harmonize_country(country),
              prev_email = str_to_lower(coalesce(to_email, ""))) %>%
    filter(nzchar(prev_email)) %>%
    distinct(country_h, .keep_all = TRUE)
  sticky <- con_ranked %>%
    mutate(.email_l = str_to_lower(email)) %>%
    inner_join(prev, by = "country_h") %>%
    filter(.email_l == prev_email) %>%
    group_by(country_h) %>% slice(1) %>% ungroup() %>%
    select(-.email_l, -prev_email)
  if (nrow(sticky) > 0) {
    primary <- bind_rows(sticky,
                         primary %>% filter(!country_h %in% sticky$country_h))
    message("Sticky assignment: ", nrow(sticky),
            " countries keep their previously logged contact")
  }
  lost <- prev %>% filter(!country_h %in% sticky$country_h,
                          country_h %in% con_ranked$country_h)
  if (nrow(lost) > 0)
    message("  NOTE: ", nrow(lost), " previously logged contact(s) no longer usable ",
            "(bounced or removed from the workbook) - re-ranked fresh: ",
            paste(head(lost$country_h, 5), collapse = ", "),
            if (nrow(lost) > 5) " ..." else "")
}

# CC list: other contacts at the SAME organisation (reach the whole NSO team),
# top-ranked, capped at MAX_CC. Same-org only so we don't mix unrelated offices.
prim_key <- primary %>% transmute(country_h, p_org = org, p_email = email)
cc_tbl <- con_ranked %>%
  inner_join(prim_key, by = "country_h") %>%
  # require the org to ACTUALLY match (both present, equal after normalising
  # case/whitespace). Previously `is.na(org) | is.na(p_org)` treated a MISSING
  # org field as "same organisation", which could CC a contact at an entirely
  # unrelated institution for that country - a real risk of emailing the wrong
  # office. Missing org data now means "don't CC" (safe default) rather than
  # "assume same org".
  mutate(org_norm = str_to_lower(str_squish(coalesce(org, ""))),
         p_org_norm = str_to_lower(str_squish(coalesce(p_org, "")))) %>%
  filter(rank_in_country > 1, email != p_email,
         nzchar(org_norm), nzchar(p_org_norm), org_norm == p_org_norm) %>%
  distinct(country_h, email, .keep_all = TRUE) %>%   # same email listed twice -> one CC, not two
  group_by(country_h) %>% arrange(rank_in_country, .by_group = TRUE) %>%
  slice_head(n = MAX_CC) %>%
  summarise(cc_email = paste(email, collapse = "; "),
            cc_first = paste(coalesce(first, ""), collapse = "; "),
            cc_full  = paste(str_squish(paste(coalesce(first, ""), coalesce(last, ""))), collapse = "; "),
            .groups = "drop")

# country -> primary UN language (Arabic/Chinese/French/Russian/Spanish), from the
# curated NSO registry's `languages` field (first token). Non-UN primary -> English.
UN_LANGS <- c("ar", "zh", "fr", "ru", "es")
lang_of <- c()
if (file.exists("NSO_Full_Registry.csv")) {
  rg <- read_csv("NSO_Full_Registry.csv", show_col_types = FALSE)
  if (all(c("country", "languages") %in% names(rg))) {
    rl <- rg %>%
      transmute(country_h = harmonize_country(country),
                first = str_trim(str_split_fixed(coalesce(languages, "en"), ",", 2)[, 1])) %>%
      mutate(main = if_else(first %in% UN_LANGS, first, "en")) %>%
      distinct(country_h, .keep_all = TRUE)
    lang_of <- setNames(rl$main, rl$country_h)
  }
}
lang_for <- function(ctry) { l <- unname(lang_of[ctry]); if (length(l) == 0 || is.na(l)) "en" else l }

# country -> GAIN respondent status (ACTIVE/LAPSED/NEVER) for a status-aware ask
resp_of <- c()
.rcol <- intersect(c("gain_respondent_status", "respondent_status"), names(ev))[1]
if (!is.na(.rcol)) {
  rd <- ev %>% transmute(c = if ("Country" %in% names(ev)) Country else country, s = .data[[.rcol]]) %>%
    filter(!is.na(s)) %>% distinct(c, .keep_all = TRUE)
  resp_of <- setNames(rd$s, rd$c)
}
status_key_of <- function(s) {
  s <- toupper(coalesce(s, ""))
  if (grepl("ACTIVE", s)) "active" else if (grepl("LAPS", s)) "lapsed" else if (grepl("NEVER", s)) "never" else ""
}
# readable population phrase from the "refugees; idps; stateless" tokens
pops_phrase_en <- function(pop_vec) {
  toks <- str_trim(unique(unlist(str_split(coalesce(pop_vec, ""), ";"))))
  map <- c(refugees = "refugees", idps = "internally displaced people", stateless = "stateless people")
  lab <- unname(map[toks[nzchar(toks)]]); lab <- lab[!is.na(lab)]
  if (length(lab) == 0) return("")
  if (length(lab) == 1) return(lab)
  paste0(paste(head(lab, -1), collapse = ", "), " and ", tail(lab, 1))
}
ST_EN <- list(
  active = "Thank you for your office's past contributions to the GAIN survey.",
  lapsed = "We would value re-engaging your office with the GAIN survey.",
  never  = "We would be glad to see your office take part in the GAIN survey.")
# status-aware sentence in each UN language (active / lapsed / never)
ST_NATIVE <- list(
  fr = list(active="Nous vous remercions pour les contributions passées de votre institution à l'enquête GAIN.",
            lapsed="Nous serions heureux de renouer avec votre institution dans le cadre de l'enquête GAIN.",
            never ="Nous serions ravis que votre institution participe à l'enquête GAIN."),
  es = list(active="Le agradecemos las contribuciones anteriores de su oficina a la encuesta GAIN.",
            lapsed="Nos gustaría retomar el contacto con su oficina en relación con la encuesta GAIN.",
            never ="Nos complacería que su oficina participara en la encuesta GAIN."),
  ru = list(active="Благодарим вас за прежний вклад вашего ведомства в обследование GAIN.",
            lapsed="Мы были бы рады возобновить взаимодействие с вашим ведомством в рамках обследования GAIN.",
            never ="Мы были бы рады участию вашего ведомства в обследовании GAIN."),
  ar = list(active="نشكركم على مساهمات مكتبكم السابقة في استبيان GAIN.",
            lapsed="يسعدنا تجديد التعاون مع مكتبكم في إطار استبيان GAIN.",
            never ="يسعدنا أن يشارك مكتبكم في استبيان GAIN."),
  zh = list(active="感谢贵机构此前对 GAIN 调查的贡献。",
            lapsed="我们很高兴能与贵机构重新就 GAIN 调查开展合作。",
            never ="我们很高兴邀请贵机构参与 GAIN 调查。"))

# ------------------------------------------------------------------------------
# 4. OUTREACH TARGETS - every new example + its country's primary contact
# ------------------------------------------------------------------------------
targets <- new_ex %>%
  left_join(primary, by = "country_h") %>%
  arrange(country_h, desc(score)) %>%
  transmute(country = country_h, example_title = title, year, url,
            relevance_score = score, is_strong, cat_letter, llm_counted, populations,
            match_type, match_why, evidence_quote = quote,
            contact_name = str_squish(paste(coalesce(first,""), coalesce(last,""))),
            contact_first = coalesce(first, ""),  # kept separately: a first name that
            # itself contains a space (e.g. "Ana Maria") would otherwise get cut down
            # to "Ana" by re-splitting contact_name on the first space in the greeting
            contact_position = position, contact_org = org, contact_email = email,
            contact_is_member = is_member, has_contact = !is.na(email))
write_excel_csv(targets, paste0("outreach_targets_", stamp, ".csv"))

# ------------------------------------------------------------------------------
# 5. EMAIL DRAFTS - one per country/contact, direct tone, no en dashes
# ------------------------------------------------------------------------------
# join first names into a greeting: "A", "A and B", or "A, B and C" (conj per language)
format_names <- function(v, conj = "and") {
  v <- str_squish(v[nzchar(coalesce(v, ""))])
  if (length(v) == 0) return("colleague")
  if (length(v) == 1) return(v)
  if (conj == "、") return(paste(v, collapse = "、"))   # Chinese: no spaces
  paste0(paste(head(v, -1), collapse = ", "), " ", conj, " ", tail(v, 1))
}

make_bullets <- function(ex_df) {
  q <- if (SHOW_QUOTES && "evidence_quote" %in% names(ex_df)) coalesce(ex_df$evidence_quote, "") else rep("", nrow(ex_df))
  ex_df %>% mutate(
    .q = q,
    line = paste0("- ", coalesce(example_title, "(untitled)"),
      if_else(!is.na(year) & year != "NA", paste0(" (", year, ")"), ""),
      if_else(!is.na(url), paste0(": ", url), ""),
      if_else(nzchar(.q), paste0("\n    \"", .q, "\""), ""))) %>%
    pull(line) %>% paste(collapse = "\n")
}

# cycle-deadline line per language (only shown if GAIN_DEADLINE is set)
DL <- list(en = "The %s GAIN cycle closes on %s.",
           fr = "Le cycle GAIN %s se clôture le %s.",
           es = "El ciclo GAIN %s se cierra el %s.",
           ru = "Цикл GAIN %s завершается %s.",
           ar = "تنتهي دورة GAIN %s في %s.",
           zh = "%s 年 GAIN 调查周期将于 %s 截止。")
deadline_line <- function(lang) if (nzchar(GAIN_DEADLINE)) paste0(" ", sprintf(DL[[lang]], GAIN_CYCLE, GAIN_DEADLINE)) else ""

build_body <- function(names_vec, intro, country, org, ex_df, n_more, status_key = "", pops = "") {  # English block
  bullets <- make_bullets(ex_df)
  more <- if (n_more > 0) paste0("\nWe have ", n_more, " further example(s) we can share.") else ""
  org_phrase  <- if (!is.na(org) && org != "") paste0(" from ", org) else " from your office"
  pops_clause <- if (nzchar(pops)) paste0(", including work on ", pops, ",") else ""
  status_line <- if (nzchar(status_key)) paste0(" ", ST_EN[[status_key]]) else ""
  paste0(
    "Dear ", format_names(names_vec), ",\n\n",
    intro, ", I am writing from the EGRISS Secretariat.", status_line, "\n\n",
    "We are mapping how national statistical offices include refugees, IDPs and stateless ",
    "people in official statistics. We found work", org_phrase, pops_clause, " that does not yet appear in ",
    "the GAIN survey:\n\n", bullets, more, "\n\n",
    "Could you confirm whether these reflect inclusion work by your office, and report them in ",
    "the GAIN survey so ", country, " is counted in the global picture? It takes only a few minutes.",
    deadline_line("en"), "\n\n",
    "GAIN survey: ", GAIN_SURVEY_LINK, "\n\n",
    "Thank you,\n", SENDER_NAME, "\n", SENDER_TITLE)
}

# Bilingual templates for the UN languages. NOTE: professional drafts - have a
# native speaker review before sending. The native block goes first, English below.
TPL <- list(
  fr = list(gp="Bonjour ", gs=",", and="et",
    intro="Je vous écris au nom du Secrétariat EGRISS.",
    p1="Nous étudions la manière dont les instituts nationaux de statistique incluent les réfugiés, les personnes déplacées internes et les apatrides dans les statistiques officielles. Nous avons identifié des travaux qui ne figurent pas encore dans l'enquête GAIN :",
    more="Nous disposons de %d autre(s) exemple(s) que nous pouvons partager.",
    p2="Pourriez-vous confirmer si ces éléments reflètent un travail d'inclusion de votre institution, et les signaler dans l'enquête GAIN afin que %s soit prise en compte au niveau mondial ? Cela ne prend que quelques minutes.",
    survey="Enquête GAIN :", thanks="Avec mes remerciements,"),
  es = list(gp="Estimados/as ", gs=":", and="y",
    intro="Le escribo en nombre de la Secretaría de EGRISS.",
    p1="Estamos analizando cómo las oficinas nacionales de estadística incluyen a las personas refugiadas, desplazadas internas y apátridas en las estadísticas oficiales. Hemos identificado trabajos que aún no figuran en la encuesta GAIN:",
    more="Disponemos de %d ejemplo(s) más que podemos compartir.",
    p2="¿Podría confirmar si reflejan un trabajo de inclusión de su oficina y comunicarlos en la encuesta GAIN para que %s quede reflejado en el panorama mundial? Solo lleva unos minutos.",
    survey="Encuesta GAIN:", thanks="Muchas gracias,"),
  ru = list(gp="Здравствуйте, ", gs="!", and="и",
    intro="Пишу вам от имени Секретариата EGRISS.",
    p1="Мы изучаем, как национальные статистические службы учитывают беженцев, внутренне перемещённых лиц и лиц без гражданства в официальной статистике. Мы обнаружили материалы, которые пока не отражены в обследовании GAIN:",
    more="У нас есть ещё %d пример(ов), которыми мы можем поделиться.",
    p2="Не могли бы вы подтвердить, отражают ли они работу вашего ведомства, и сообщить о них в обследовании GAIN, чтобы %s была учтена в общей картине? Это займёт всего несколько минут.",
    survey="Обследование GAIN:", thanks="С уважением,"),
  ar = list(gp="تحية طيبة ", gs="،", and="و",
    intro="أكتب إليكم باسم أمانة EGRISS.",
    p1="نقوم بمسح كيفية إدراج المكاتب الإحصائية الوطنية للاجئين والنازحين داخلياً وعديمي الجنسية في الإحصاءات الرسمية. وقد وجدنا أعمالاً لم تظهر بعد في استبيان GAIN:",
    more="لدينا %d مثال إضافي يمكننا مشاركته.",
    p2="هل يمكنكم تأكيد ما إذا كانت تعكس عمل مكتبكم في الإدماج، والإبلاغ عنها في استبيان GAIN حتى تُحتسب %s في الصورة العالمية؟ لن يستغرق الأمر سوى بضع دقائق.",
    survey="استبيان GAIN:", thanks="مع خالص الشكر،"),
  zh = list(gp="", gs="，您好：", and="、",
    intro="我谨代表 EGRISS 秘书处与您联系。",
    p1="我们正在梳理各国国家统计机构如何将难民、境内流离失所者和无国籍人纳入官方统计。我们发现了一些尚未出现在 GAIN 调查中的相关工作：",
    more="我们还有 %d 个示例可以分享。",
    p2="能否请您确认这些是否反映了贵机构的纳入工作，并在 GAIN 调查中报告，以便 %s 纳入全球统计？只需几分钟。",
    survey="GAIN 调查：", thanks="谨致谢意，"))

build_native <- function(lang, names_vec, country, ex_df, n_more, status_key = "") {
  t <- TPL[[lang]]
  bullets <- make_bullets(ex_df)
  more <- if (n_more > 0) paste0("\n", sprintf(t$more, n_more)) else ""
  status_line <- if (nzchar(status_key)) paste0(" ", ST_NATIVE[[lang]][[status_key]]) else ""
  paste0(t$gp, format_names(names_vec, t$and), t$gs, "\n\n",
         t$intro, status_line, "\n\n", t$p1, "\n\n", bullets, more, "\n\n",
         sprintf(t$p2, country), deadline_line(lang), "\n\n", t$survey, " ", GAIN_SURVEY_LINK, "\n\n",
         t$thanks, "\n", SENDER_NAME, "\n", SENDER_TITLE)
}

emails <- targets %>%
  filter(has_contact) %>%
  group_by(country, contact_name, contact_first, contact_email, contact_org) %>%
  group_modify(function(g, key) {
    g <- g %>% arrange(desc(is_strong), desc(relevance_score))
    top <- head(g, MAX_EXAMPLES_PER_EMAIL)
    n_more <- nrow(g) - nrow(top)
    best <- max(g$relevance_score, na.rm = TRUE)
    # tier from the BEST evidence in this email: LLM-confirmed or category A or high
    # score = sure bet; category B/C or mid score = needs review; else low bet.
    tier <- case_when(
      any(g$llm_counted %in% TRUE) | any(g$cat_letter == "A", na.rm = TRUE) | best >= 70 ~ "sure_bet",
      any(g$cat_letter %in% c("B", "C"), na.rm = TRUE) | best >= EMAIL_MIN_SCORE          ~ "needs_review",
      TRUE                                                                                ~ "low_bet")
    first <- if (nzchar(coalesce(key$contact_first, ""))) key$contact_first
             else str_split_fixed(key$contact_name, " ", 2)[,1]  # fallback if contact_first is blank
    idx <- match(key$country, cc_tbl$country_h)
    cc_first <- cc_tbl$cc_first[idx]; cc_full <- cc_tbl$cc_full[idx]
    cc_vec_f <- if (length(cc_first) == 1 && !is.na(cc_first)) str_split(cc_first, ";")[[1]] else character(0)
    cc_vec_F <- if (length(cc_full) == 1 && !is.na(cc_full))  str_split(cc_full, ";")[[1]]  else character(0)
    names_vec <- if (GREETING_STYLE == "full") c(key$contact_name, cc_vec_F) else c(first, cc_vec_f)
    tc <- primary$top_cat[match(key$country, primary$country_h)]
    lang <- lang_for(key$country)
    skey <- status_key_of(resp_of[key$country])             # active / lapsed / never / ""
    pops <- pops_phrase_en(top$populations)                  # e.g. "refugees and IDPs"
    eng <- build_body(names_vec, intro_for(tc), key$country, key$contact_org, top, n_more, skey, pops)
    # UN-language country: native first, English below; otherwise English only
    body <- if (lang == "en") eng else paste0(
      build_native(lang, names_vec, key$country, top, n_more, skey),
      "\n\n----- English version below -----\n\n", eng)
    tibble(
      tier = tier, language = lang,
      n_examples = nrow(g),
      subject = paste0("GAIN survey: displacement statistics from ", key$country),
      body = str_replace_all(body, "[–—]", "-"))
  }) %>% ungroup() %>%
  left_join(cc_tbl, by = c("country" = "country_h")) %>%
  mutate(tier = factor(tier, levels = c("sure_bet", "needs_review", "low_bet")),
         cc_email = coalesce(cc_email, "")) %>%
  arrange(tier, country) %>%
  transmute(tier, language, country, to_name = contact_name, to_email = contact_email,
            cc_email, organization = contact_org, n_examples, subject, body)
write_excel_csv(emails, paste0("outreach_emails_", stamp, ".csv"))

# ------------------------------------------------------------------------------
# 6. CONTACT GAPS + Power BI coverage table
# ------------------------------------------------------------------------------
gaps <- targets %>% filter(!has_contact) %>%
  count(country, name = "new_examples") %>% arrange(desc(new_examples))
write_excel_csv(gaps, paste0("outreach_contact_gaps_", stamp, ".csv"))

coverage <- targets %>%
  group_by(country) %>%
  summarise(new_examples = n(), has_contact = any(has_contact),
            contact_name = first(contact_name), contact_email = first(contact_email),
            contact_org = first(contact_org), .groups = "drop") %>%
  arrange(desc(new_examples))
if (dir.exists("powerbi_export"))
  write_excel_csv(coverage, file.path("powerbi_export", "WEB_GAIN_outreach.csv"))

message("\n==================== OUTREACH ====================")
message(sprintf("New examples targeted: %d | with a contact: %d | missing a contact: %d",
                nrow(targets), sum(targets$has_contact), sum(!targets$has_contact)))
message(sprintf("Draft emails written: %d (one per country/contact)", nrow(emails)))
message("  by tier: ", paste(names(table(emails$tier)), table(emails$tier), sep="=", collapse=" | "))
message("  emails that also CC same-NSO colleagues: ", sum(nzchar(emails$cc_email)))
message("  by language: ", paste(names(table(emails$language)), table(emails$language), sep="=", collapse=" | "))
message(sprintf("Countries needing a contact (gap): %d", nrow(gaps)))
message(paste0("\nOutputs: outreach_targets_", stamp, ".csv | outreach_emails_", stamp,
               ".csv | outreach_contact_gaps_", stamp, ".csv | powerbi_export/WEB_GAIN_outreach.csv"))
message("\nNEXT: open outreach_emails_*.csv, edit SENDER_NAME + GAIN_SURVEY_LINK at the top of")
message("this script (or in the file), review the drafts, then mail-merge to send.")
