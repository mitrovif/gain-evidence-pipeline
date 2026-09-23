# ==============================================================================
# GAIN SDG DOCS CLASSIFY  (workstream stage 3 - extract SDG indicator DATAPOINTS)
#
# For every harvested document (sdg_docs/SDG_DOCS_MANIFEST.csv, status ok), the
# local LLM reads a keyword-dense excerpt and extracts REPORTED VALUES for SDG
# indicators - ALL SDG indicators, with `is_priority` marking the ones on the
# EGRISS Methodological Paper 3 priority list (sdg_docs/sdg_priority_indicators.csv).
#
#   one datapoint = country x indicator x population group x value
#   population        coarse group: total | refugees | idps | stateless |
#                     returnees | host community
#   population_detail as stated in the document, following the statistical
#                     frameworks of the recommendations where detail exists
#                     (e.g. "asylum seekers", "returned refugees", "IDPs in
#                     camps", "persons of undetermined nationality")
#   match_type        exact      = the measure IS that SDG indicator
#                     comparable = a close variant (e.g. 2.2.2-style siblings)
#                     related    = same topic, different measure (e.g. an
#                                  employment rate recorded against 8.5.2)
#   page              located DETERMINISTICALLY in the archived document (PDFs)
#
# QUALITY GATES (the LLM proposes, deterministic checks dispose):
#   1. value gate   - the value must appear in the model's own verbatim evidence
#                     quote (not merely somewhere in the excerpt); kills
#                     hallucinated numbers and echoed prompt-example values
#   2. anchor gate  - an "exact" claim on a PRIORITY indicator must have
#                     evidence naming the measure (anchor_terms in the
#                     taxonomy); failing claims are DEMOTED to "related", not
#                     dropped (caught employment-rate mapped to unemployment)
#   3. code gate    - indicator codes must look like real SDG codes (N.N.N)
#   4. human        - `verified` column: every value is checked against the
#                     archived document before publication
#
# Outputs (sdg_docs/): SDG_DOCS_CLASSIFIED.csv (per document) and
# SDG_DATAPOINTS.csv (per value - the dashboard dataset).
# Values recorded AS PUBLISHED; not comparable across countries/methods.
# Test mode: Sys.setenv(GAIN_SDGCLASS_LIMIT = "5"). Needs Ollama (qwen2.5:7b).
# ==============================================================================

suppressMessages({ library(tidyverse); library(jsonlite) })
source("shared/GAIN_COMMON.R")
source("shared/GAIN_OLLAMA_HELPERS.R")
if (!ollama_available()) stop("Ollama is not reachable - start it first.")

DIR      <- "sdg_docs"
MANIFEST <- file.path(DIR, "SDG_DOCS_MANIFEST.csv")
TAXONOMY <- file.path(DIR, "sdg_priority_indicators.csv")
OUT_DOCS <- file.path(DIR, "SDG_DOCS_CLASSIFIED.csv")
OUT_DP   <- file.path(DIR, "SDG_DATAPOINTS.csv")
SDGCLASS_VERSION <- "v6"   # v6: documents do NOT need to mention the SDGs at all -
#   a reported measure that matches an SDG indicator's DESCRIPTION is recorded
#   (the mapping to a code is ours, labelled honestly via match_type), and the
#   new measure_as_stated field keeps the document's own name for the measure
#   ("employment rate", "share of registered births") for the metadata review.
# v5: all SDGs + is_priority; population_detail; match_type; page lookup.
# v4: anchor-term gate. v3: value-integrity gate. v2: datapoint schema.
LIMIT <- suppressWarnings(as.integer(Sys.getenv("GAIN_SDGCLASS_LIMIT", "0")))
POPS_ALLOWED <- c("total", "refugees", "idps", "stateless", "returnees", "host community")
SDG_CODE_PAT <- "^[0-9]{1,2}\\.[0-9a-z]{1,2}\\.[0-9]{1,2}$"

tax <- read_csv(TAXONOMY, show_col_types = FALSE)
# indicator block for the prompt: code = title, plus the OFFICIAL UN metadata
# definition where we have it (sdg_docs/indicator_metadata/) - sharper matching
ind_block <- paste(sprintf("  %s = %s%s", tax$code, substr(tax$title, 1, 100),
  ifelse(nzchar(coalesce(tax$definition, "")),
         paste0(" [definition: ", substr(tax$definition, 1, 150), "]"), "")),
  collapse = "\n")
tax_sig <- rlang::hash(paste(ind_block, "v6"))

man <- read_csv(MANIFEST, show_col_types = FALSE) %>% filter(status == "ok")
done_ids <- if (file.exists(OUT_DOCS)) unique(read_csv(OUT_DOCS, show_col_types = FALSE)$doc_id) else character(0)
todo <- man %>% filter(!doc_id %in% done_ids)
# PRIORITIZED QUEUE - the docs most likely to hold priority-indicator values run
# first, so early results are the valuable ones and the run can be stopped at
# any point (everything is cached/resumable):
#   1. GAIN-survey documents (the documented examples - core of the database)
#   2. PDFs (actual statistical reports rather than landing pages)
#   3. candidates whose title names an instrument (census/survey/admin guess)
#   4. everything else
todo <- todo %>%
  mutate(.prio = case_when(
    source == "gain_survey" ~ 1L,
    str_ends(coalesce(file, ""), ".pdf") ~ 2L,
    coalesce(production_method_guess, "unknown") != "unknown" ~ 3L,
    TRUE ~ 4L)) %>%
  arrange(.prio, country)
message("queue: ", paste(names(table(todo$.prio)), table(todo$.prio),
                         sep = "=", collapse = " | "),
        "  (1=gain_survey 2=pdf 3=named-instrument 4=other)")
todo <- todo %>% select(-.prio)
if (LIMIT > 0) todo <- head(todo, LIMIT)
message(nrow(man), " harvested docs | ", length(done_ids), " already classified | ",
        nrow(todo), " to do", if (LIMIT > 0) paste0(" (LIMIT ", LIMIT, ")") else "")

# ---- document text as PAGES (enables the page lookup); cache first, then file
doc_pages_of <- function(row) {
  f <- file.path("evidence_cache", paste0(rlang::hash(row$url), ".rds"))
  if (file.exists(f)) {
    d <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(d) && length(d$pages) > 0) return(as.character(d$pages))
  }
  ext <- tolower(tools::file_ext(coalesce(row$file, "")))
  if (ext == "pdf")
    return(tryCatch(pdftools::pdf_text(row$file), error = function(e) character(0)))
  if (ext %in% c("html", "htm"))
    return(tryCatch({
      h <- xml2::read_html(row$file)
      xml2::xml_remove(rvest::html_elements(h, "script, style, nav, footer"))
      rvest::html_text2(h)
    }, error = function(e) character(0)))
  character(0)
}

.strip <- function(x) gsub("[[:space:]]", "", .coal(x, ""))
# value normaliser: strip spaces AND unify decimal separators (9,2 == 9.2) and
# case, so the value gate matches genuine European-format figures but is not
# fooled by formatting. Used only for value<->text comparisons.
.normv <- function(x) gsub(",", ".", tolower(gsub("[[:space:]]", "", .coal(x, ""))))

# deterministic page lookup: first page containing the evidence quote (stripped),
# falling back to the first page containing the value string; NA for single-blob
# webpages (no meaningful page concept)
page_of <- function(pages, evidence, value) {
  if (length(pages) <= 1) return(NA_integer_)
  ps <- vapply(pages, .strip, character(1))
  ev <- .strip(evidence)
  if (nzchar(ev)) { hit <- which(vapply(ps, function(p) grepl(ev, p, fixed = TRUE), logical(1)))
                    if (length(hit)) return(hit[1]) }
  v <- .strip(value)
  if (nzchar(v)) { hit <- which(vapply(ps, function(p) grepl(v, p, fixed = TRUE), logical(1)))
                   if (length(hit)) return(hit[1]) }
  NA_integer_
}

classify_doc <- function(txt, title, country, populations) {
  excerpt <- smart_excerpt(txt)
  key <- rlang::hash(paste("sdgclass", SDGCLASS_VERSION, OLLAMA_GEN_MODEL,
                           tax_sig, .coal(title, ""), excerpt))
  cached <- .cache_get("sdgclass", key)
  if (!is.null(cached)) return(cached)
  prompt <- paste0(
"You extract SDG indicator DATA VALUES from official statistical documents for the EGRISS ",
"SDG documentation database, which records how SDG indicators are produced for refugees, ",
"IDPs (internally displaced persons) and stateless persons. Judge ONLY from the text; be ",
"literal; never invent, compute or convert values.\n\n",
"PRIORITY SDG INDICATORS (other SDG indicators may also be recorded):\n", ind_block, "\n\n",
"DOCUMENT (country hint: ", .coal(country, "unknown"),
"; populations hint: ", .coal(populations, "unknown"), "):\n",
"TITLE: ", .coal(title, ""), "\n", excerpt, "\n\n",
"Extract every REPORTED VALUE of a measure matching an SDG indicator (any SDG indicator, ",
"not only the priority list) for the overall population (population=\"total\") and/or ",
"refugees / idps / stateless / returnees / host community. THE DOCUMENT DOES NOT NEED TO ",
"MENTION THE SDGs: if a reported measure matches an SDG indicator by DESCRIPTION (e.g. a ",
"share of births registered, a proportion in inadequate housing), record it and map it to ",
"the closest indicator code with an honest match_type. A datapoint needs an actual value ",
"printed in the text. STRICT RULES: copy each value EXACTLY as printed; the evidence quote ",
"MUST contain that exact value; measure_as_stated = the measure's name AS THE DOCUMENT ",
"CALLS IT (e.g. \"employment rate\"); population_detail = the group exactly as the document ",
"describes it (e.g. \"asylum seekers\", \"returned refugees\", \"Ukraine refugees\", ",
"\"IDPs in camps\", \"persons of undetermined nationality\"); match_type = \"exact\" ONLY ",
"if the measure IS that SDG indicator, \"comparable\" for a close variant, \"related\" for ",
"same-topic-different-measure (e.g. an employment rate is only RELATED to unemployment-",
"rate 8.5.2). Return ONE JSON object, no prose:\n",
'{"populations_covered": [],           // subset of ["refugees","idps","stateless"]\n',
' "production_method": "unclear",      // "census" | "survey" | "admin data" | "mixed" | "unclear"\n',
' "instrument": "",                    // the named census/survey/register, if stated\n',
' "datapoints": [{"code": "8.5.2", "match_type": "exact", "measure_as_stated": "",\n',
'                 "population": "refugees", "population_detail": "", "value": "NN.N",\n',
'                 "unit": "%", "year": "2023", "evidence": ""}],  // value = the number EXACTLY as printed; evidence <= 25 words verbatim and MUST contain that number\n',
' "confidence": "low"}                 // low | medium | high')
  raw <- .ollama_generate(prompt, timeout = 600)
  if (is.na(raw)) raw <- .ollama_generate(prompt, timeout = 600)
  out <- list(populations = "", method = "unclear", instrument = "",
              dp = tibble(), confidence = "low", status = "no-response")
  if (!is.na(raw)) {
    j <- regmatches(raw, regexpr("(?s)\\{.*\\}", raw, perl = TRUE))
    obj <- tryCatch(fromJSON(if (length(j)) j else raw), error = function(e) NULL)
    if (!is.null(obj)) {
      m <- tolower(as.character(obj$production_method %||% "unclear"))
      if (!m %in% c("census", "survey", "admin data", "mixed", "unclear")) m <- "unclear"
      dp <- obj$datapoints
      dp <- if (!is.null(dp) && is.data.frame(dp) && nrow(dp) > 0) {
        d <- as_tibble(dp) %>% mutate(across(everything(), as.character))
        for (c in c("code","match_type","measure_as_stated","population","population_detail",
                    "value","unit","year","evidence"))
          if (!c %in% names(d)) d[[c]] <- ""
        d %>%
          mutate(population = tolower(coalesce(population, "")),
                 match_type = tolower(coalesce(match_type, "related"))) %>%
          # code gate: must look like a real SDG indicator code
          filter(grepl(SDG_CODE_PAT, code), population %in% POPS_ALLOWED,
                 nzchar(coalesce(value, ""))) %>%
          mutate(match_type = if_else(match_type %in% c("exact","comparable","related"),
                                      match_type, "related")) %>%
          transmute(sdg_code = code, match_type,
                    measure_as_stated = substr(coalesce(measure_as_stated, ""), 1, 90),
                    population,
                    population_detail = substr(coalesce(population_detail, ""), 1, 80),
                    value = substr(value, 1, 30), unit = substr(coalesce(unit, ""), 1, 20),
                    year = substr(coalesce(year, ""), 1, 12),
                    evidence = substr(coalesce(evidence, ""), 1, 200))
      } else tibble()
      # VALUE GATE (deterministic, two conditions):
      #  (a) the value appears somewhere in the document excerpt, AND
      #  (b) the value appears in the model's OWN evidence quote.
      # (b) is the strong test: a fabricated or echoed value (e.g. the prompt's
      # example figure) rarely lands inside a verbatim quote, whereas a common
      # substring like "9.2" is easy to find *somewhere* in a big excerpt. An
      # earlier excerpt-only gate let ~40% of rows through on that loophole.
      if (nrow(dp) > 0) {
        tnorm <- .normv(excerpt)
        in_excerpt <- vapply(dp$value, function(v) nzchar(.normv(v)) &&
                         grepl(.normv(v), tnorm, fixed = TRUE), logical(1))
        in_evidence <- vapply(seq_len(nrow(dp)), function(i) nzchar(.normv(dp$value[i])) &&
                         grepl(.normv(dp$value[i]), .normv(dp$evidence[i]), fixed = TRUE), logical(1))
        keep <- in_excerpt & in_evidence
        if (sum(!keep) > 0)
          message("    value gate: ", sum(!keep), " datapoint(s) REJECTED (value not in evidence quote)")
        dp <- dp[keep, , drop = FALSE]
      }
      # ANCHOR GATE: an "exact" claim on a PRIORITY indicator must have evidence
      # naming the measure; failing claims are DEMOTED to "related" (kept, honest)
      if (nrow(dp) > 0) {
        anch <- setNames(tax$anchor_terms, tax$code)
        dp$is_priority <- dp$sdg_code %in% tax$code
        dp$anchor_checked <- FALSE
        for (i in seq_len(nrow(dp))) {
          if (dp$match_type[i] == "exact" && dp$is_priority[i]) {
            a <- anch[[dp$sdg_code[i]]]
            if (!is.na(a) && nzchar(a)) {
              dp$anchor_checked[i] <- TRUE
              if (!grepl(a, dp$evidence[i], ignore.case = TRUE, perl = TRUE)) {
                dp$match_type[i] <- "related (demoted: evidence does not name the measure)"
                message("    anchor gate: 1 'exact' claim on ", dp$sdg_code[i], " demoted to related")
              }
            }
          }
        }
        dp$value_in_evidence <- TRUE   # now enforced by the value gate above
      }
      conf <- tolower(as.character(obj$confidence %||% "low"))
      out <- list(
        populations = paste(intersect(tolower(unlist(obj$populations_covered)),
                                      c("refugees", "idps", "stateless")), collapse = ";"),
        method = m, instrument = substr(as.character(obj$instrument %||% ""), 1, 120),
        dp = dp, confidence = if (conf %in% c("low", "medium", "high")) conf else "low",
        status = "ok")
    } else out$status <- "invalid-json"
  }
  if (out$status == "ok") .cache_put("sdgclass", key, out)
  out
}

for (i in seq_len(nrow(todo))) {
  r <- todo[i, ]
  pages <- doc_pages_of(r)
  txt <- paste(pages, collapse = " ")
  if (nchar(str_squish(txt)) < 200) {
    write_csv(tibble(doc_id = r$doc_id, source = r$source, country = r$country,
                     title = r$title, url = r$url, indicators_found = "",
                     n_datapoints = 0L, populations_covered = "", production_method = "",
                     instrument = "", confidence = "", class_status = "no-text"),
              OUT_DOCS, append = file.exists(OUT_DOCS))
    next
  }
  t0 <- Sys.time()
  cl <- classify_doc(txt, r$title, r$country, r$populations)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  write_csv(tibble(doc_id = r$doc_id, source = r$source, country = r$country,
                   title = r$title, url = r$url,
                   indicators_found = paste(unique(cl$dp$sdg_code), collapse = ";"),
                   n_datapoints = nrow(cl$dp), populations_covered = cl$populations,
                   production_method = cl$method, instrument = cl$instrument,
                   confidence = cl$confidence, class_status = cl$status),
            OUT_DOCS, append = file.exists(OUT_DOCS))
  if (nrow(cl$dp) > 0) {
    cl$dp$page <- vapply(seq_len(nrow(cl$dp)), function(k)
      page_of(pages, cl$dp$evidence[k], cl$dp$value[k]), integer(1))
    write_csv(bind_cols(tibble(doc_id = r$doc_id, country = r$country,
                               source = r$source, title = r$title, url = r$url,
                               production_method = cl$method, instrument = cl$instrument),
                        cl$dp) %>% mutate(confidence = cl$confidence, verified = ""),
              OUT_DP, append = file.exists(OUT_DP))
  }
  message(sprintf("[%d/%d] %s | %s | %d datapoint(s) | %s | %ss",
                  i, nrow(todo), r$country, substr(r$title, 1, 40),
                  nrow(cl$dp), cl$method, secs))
}

docs <- read_csv(OUT_DOCS, show_col_types = FALSE)
message("\n==================== SDG DATAPOINT EXTRACTION ====================")
message("documents: ", n_distinct(docs$doc_id),
        " | with datapoints: ", sum(docs$n_datapoints > 0, na.rm = TRUE))
if (file.exists(OUT_DP)) {
  dp <- read_csv(OUT_DP, show_col_types = FALSE)
  message("datapoints: ", nrow(dp),
          " | priority: ", sum(dp$is_priority, na.rm = TRUE),
          " | exact: ", sum(dp$match_type == "exact", na.rm = TRUE),
          " | by population: ",
          paste(names(table(dp$population)), table(dp$population), sep = "=", collapse = " | "))
}
message("Outputs: ", OUT_DOCS, " + ", OUT_DP, "  (verify every value before publishing)")
