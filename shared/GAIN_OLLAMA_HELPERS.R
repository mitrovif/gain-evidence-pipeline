# ==============================================================================
# GAIN OLLAMA HELPERS  (local LLM enrichment - additive, cached, resumable)
#
# Two cached helpers used by the semantic funnel and the example-level crossref.
# Nothing here touches the existing pipeline layers; source() this file from a
# script that needs it.
#
#   extract_evidence(text, title, country) -> validated list:
#       relevance_score (0-100), counted (bool), population, organization,
#       instrument_or_title, year, recommendation_referenced, evidence_quote
#       (<=20 words), confidence (low|medium|high)
#     Model: qwen2.5:7b, temperature 0, JSON-forced, schema-validated.
#
#   embed(text) -> numeric vector (bge-m3 multilingual embedding)
#
# Both cache by content hash to ./ollama_cache/ so a rerun NEVER recalls the
# model (idempotent). Every extract decision is appended to
# ollama_decisions_log.csv. Requires Ollama at http://localhost:11434.
#
# EGRISS note: its frameworks are RECOMMENDATIONS (IRRS / IRIS / IROSS), never
# "standards". The prompt enforces this wording.
# ==============================================================================

suppressMessages({
  library(httr2)
  library(jsonlite)
  library(rlang)
})

# ------------------------------------------------------------------------------
# BACKEND SELECTION: Ollama (original, Windows) OR LM Studio (OpenAI-compatible,
# e.g. this Mac). Set GAIN_LLM_BACKEND = "ollama" | "lmstudio" | "auto".
# Default "auto" probes LM Studio (:1234) first, then Ollama (:11434). The
# public API (extract_evidence/adjudicate_match/embed/.ollama_generate and the
# OLLAMA_* names below) is UNCHANGED, so every caller keeps working on either
# backend. Model ids for LM Studio are read live from /v1/models, so you need
# not hard-code LM Studio's exact id (e.g. "qwen2.5-7b-instruct"). Override
# anything with env: GAIN_LLM_MODEL, GAIN_EMB_MODEL, OLLAMA_URL, LMSTUDIO_URL.
# NOTE (LM Studio): context length is a LOAD-TIME setting in the LM Studio UI,
# not an API parameter - load Qwen with an 8k+ context window so it matches the
# MAX_DOC_CHARS budget this pipeline assumes (num_ctx below is Ollama-only).
# ------------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

GAIN_LLM_BACKEND <- tolower(Sys.getenv("GAIN_LLM_BACKEND", "auto"))
OLLAMA_URL       <- Sys.getenv("OLLAMA_URL",   "http://localhost:11434")
LMSTUDIO_URL     <- Sys.getenv("LMSTUDIO_URL", "http://localhost:1234")

.probe <- function(url, path, timeout = 2) tryCatch({
  request(paste0(url, path)) %>% req_timeout(timeout) %>% req_perform(); TRUE
}, error = function(e) FALSE)

LLM_BACKEND <- if (GAIN_LLM_BACKEND %in% c("lmstudio", "openai")) "lmstudio" else
               if (GAIN_LLM_BACKEND == "ollama")                  "ollama"   else
               if (.probe(LMSTUDIO_URL, "/v1/models"))            "lmstudio" else
               if (.probe(OLLAMA_URL,   "/api/tags"))             "ollama"   else "lmstudio"

.lmstudio_model_ids <- function() tryCatch({
  resp <- request(paste0(LMSTUDIO_URL, "/v1/models")) %>%
    req_timeout(3) %>% req_perform() %>% resp_body_json()
  vapply(resp$data, function(m) m$id %||% "", character(1))
}, error = function(e) character(0))

# Heuristic: which loaded model is an embedding model vs a chat model.
.is_embed_id <- function(id) grepl("embed|bge|nomic|gte|\\be5\\b|mxbai|minilm",
                                   id, ignore.case = TRUE)

# Resolve generation + embedding model names for the ACTIVE backend. Env wins;
# otherwise LM Studio auto-picks from /v1/models (prefer a Qwen chat model for
# generation; first embedding-type model for embed()). Empty embed model ""
# means "no embedding model available" and embed() degrades gracefully.
.gen_env <- Sys.getenv("GAIN_LLM_MODEL", "")
.emb_env <- Sys.getenv("GAIN_EMB_MODEL", "")
if (LLM_BACKEND == "lmstudio") {
  .ids     <- .lmstudio_model_ids()
  .gen_ids <- .ids[!.is_embed_id(.ids)]
  .emb_ids <- .ids[ .is_embed_id(.ids)]
  # Prefer a Qwen chat model; among Qwens prefer a non-vision (text) build if
  # both a text and a -vl (vision-language) variant happen to be loaded.
  .qwen    <- .gen_ids[grepl("qwen", .gen_ids, ignore.case = TRUE)]
  .qwen    <- c(.qwen[!grepl("-vl|\\bvl\\b", .qwen, ignore.case = TRUE)], .qwen)
  OLLAMA_GEN_MODEL <- if (nzchar(.gen_env)) .gen_env else
                      c(.qwen, .gen_ids, "qwen2.5-7b-instruct")[1]
  # Prefer bge (multilingual) over other embedding models (e.g. English nomic)
  # when several are loaded - matches the original bge-m3 pipeline.
  .bge     <- .emb_ids[grepl("bge", .emb_ids, ignore.case = TRUE)]
  OLLAMA_EMB_MODEL <- if (nzchar(.emb_env)) .emb_env else c(.bge, .emb_ids, "")[1]
} else {
  OLLAMA_GEN_MODEL <- if (nzchar(.gen_env)) .gen_env else "qwen2.5:7b"
  OLLAMA_EMB_MODEL <- if (nzchar(.emb_env)) .emb_env else "bge-m3:latest"
}
.embed_warned <- FALSE   # one-shot guard for the "no embedding model" message

OLLAMA_CACHE_DIR  <- "ollama_cache"
OLLAMA_LOG_FILE   <- "ollama_decisions_log.csv"
PROMPT_VERSION    <- "v4"          # bump to invalidate the extract cache
EMB_VERSION       <- "v1"          # bump to invalidate the embed cache
ADJ_PROMPT_VERSION <- "adj-v1"     # adjudicate_match now has ITS OWN version -
#   extract-prompt bumps used to needlessly invalidate every cached adjudication
# v4: (a) SMART EXCERPTING - the document slice sent to the model is built from
#     windows AROUND keyword hits instead of the first N characters. The v3
#     sample eval proved first-N is the wrong strategy: the Canada BVOR study's
#     methodology (refugees disaggregated by admission category via the linked
#     IMDB register) starts at char ~19,000 and stayed effectively invisible
#     even with a 20,000-char budget.
#     (b) ANALYTICAL-PRODUCTS clause in the prompt - NSO research built ON
#     register/survey data that identifies displaced people IS implemented
#     inclusion (same Canada case: v2 and v3 both wrongly said counted=FALSE).
#     (c) the extract cache key now includes the grounding file's content, so
#     activating/editing GAIN_RECOMMENDATION_GROUNDING.txt invalidates exactly
#     what it should (previously it would silently NOT apply to cached docs).
# v3: MAX_DOC_CHARS 6000->20000, num_ctx 2048->8192, 600s timeout + retry.
dir.create(OLLAMA_CACHE_DIR, showWarnings = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
# standalone NA/NULL coalesce so this module needs no tidyverse
.coal <- function(x, d) if (is.null(x) || length(x) == 0 || all(is.na(x))) d else x

# ------------------------------------------------------------------------------
# Reachability self-check (cheap; localhost only)
# ------------------------------------------------------------------------------
# Name kept as ollama_available() for backward compatibility with every caller;
# it now reports on whichever backend is active.
ollama_available <- function() {
  if (LLM_BACKEND == "lmstudio") {
    ids <- .lmstudio_model_ids()
    if (!length(ids)) {
      message("  LM Studio not serving models on ", LMSTUDIO_URL,
              " - start the local server (Developer tab) and load a model.")
      return(FALSE)
    }
    if (!nzchar(OLLAMA_EMB_MODEL))
      message("  LM Studio up; generation='", OLLAMA_GEN_MODEL,
              "'. No embedding model loaded - embed()/semantic funnel disabled ",
              "until you load one (e.g. bge-m3) or set GAIN_EMB_MODEL.")
    TRUE   # generation is available; embeddings handled separately in embed()
  } else {
    tryCatch({
      tags <- request(paste0(OLLAMA_URL, "/api/tags")) %>%
        req_timeout(5) %>% req_perform() %>% resp_body_json()
      have <- vapply(tags$models, function(m) m$name, character(1))
      ok <- any(grepl(OLLAMA_GEN_MODEL, have, fixed = TRUE)) &&
            any(grepl(sub(":.*$", "", OLLAMA_EMB_MODEL), have))
      if (!ok) message("  Ollama up but expected models missing. Have: ",
                       paste(have, collapse = ", "))
      ok
    }, error = function(e) FALSE)
  }
}

# ------------------------------------------------------------------------------
# Content-hash disk cache
# ------------------------------------------------------------------------------
.cache_path <- function(kind, key) file.path(OLLAMA_CACHE_DIR, paste0(kind, "_", key, ".rds"))
# an unreadable cache file (OneDrive online-only placeholder that fails to
# download, or a truncated write) is a cache MISS, not a fatal error - one bad
# file stopped a whole read-all run in Sep 2026
.cache_get  <- function(kind, key) {
  p <- .cache_path(kind, key)
  if (!file.exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) { message("  (cache unreadable, recomputing: ", basename(p), ")"); NULL })
}
.cache_put  <- function(kind, key, val) saveRDS(val, .cache_path(kind, key))

# ------------------------------------------------------------------------------
# Decision log (append-only; one row per extract_evidence call)
# ------------------------------------------------------------------------------
.log_decision <- function(row) {
  row$ts <- as.character(Sys.time())
  line <- as.data.frame(row, stringsAsFactors = FALSE)
  # if an existing log has a different column set (e.g. after a schema/version
  # change), rotate it aside so appends never misalign columns
  if (file.exists(OLLAMA_LOG_FILE)) {
    old_hdr <- tryCatch(strsplit(readLines(OLLAMA_LOG_FILE, n = 1), ",")[[1]],
                        error = function(e) character(0))
    old_hdr <- gsub('^"|"$', "", old_hdr)
    if (!identical(old_hdr, names(line))) {
      file.rename(OLLAMA_LOG_FILE,
                  sub("\\.csv$", paste0("_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv"),
                      OLLAMA_LOG_FILE))
    }
  }
  write.table(line, OLLAMA_LOG_FILE, sep = ",", row.names = FALSE,
              col.names = !file.exists(OLLAMA_LOG_FILE), append = file.exists(OLLAMA_LOG_FILE),
              qmethod = "double")
}

# ------------------------------------------------------------------------------
# Low-level call to qwen (JSON-forced, temperature 0)
# ------------------------------------------------------------------------------
OLLAMA_NUM_CTX <- as.integer(Sys.getenv("GAIN_LLM_NUM_CTX", "8192"))
# Ollama defaults num_ctx to 2048 tokens unless told otherwise. Our prompt
# instructions alone run ~500-600 tokens, so with the old 6000-char (~1500
# token) document budget we were already brushing that ceiling - Ollama would
# silently drop the OLDEST context (never an error), which is a second,
# invisible truncation on top of the R-side substr() cut. Setting num_ctx
# explicitly makes the real budget match what MAX_DOC_CHARS below assumes.
.ollama_generate <- function(prompt, model = OLLAMA_GEN_MODEL, timeout = 300, json = TRUE) {
  prompt <- iconv(prompt, "UTF-8", "UTF-8", sub = "")   # never send invalid UTF-8 to jsonlite/HTTP
  # json = FALSE for PROSE tasks. Forcing format="json" on a prose request makes
  # the model return an empty "{}" - which silently produced a 3-character
  # grounding draft after a multi-day keyword run. Structured extraction keeps
  # the default TRUE.
  # timeout is a parameter (not hardcoded) so a caller with unusually large
  # prompts (e.g. GAIN_LLM_KEYWORD_SUGGEST.R's ~15000-char chunks) can request
  # more time WITHOUT changing the 300s default used by extract_evidence/
  # adjudicate_match elsewhere. Root-caused bug: a batch of large-chunk calls
  # was silently returning NA - not because of bad output, but because the
  # model genuinely took longer than 300s to finish and req_timeout killed the
  # request first; tryCatch below then quietly swallowed that as NA, which
  # looked identical to a parsing failure until timed directly (300.1s, i.e.
  # exactly the old hardcoded limit).

  if (LLM_BACKEND == "lmstudio") {
    # OpenAI-compatible chat endpoint. json=TRUE -> ask for a JSON object via
    # response_format. Some older LM Studio builds reject response_format with
    # an HTTP 4xx; in that case we retry WITHOUT it (the prompts already demand
    # "return ONE JSON object" and the validators regex-extract the object, so
    # this stays reliable). We do NOT retry on timeouts/connection errors here -
    # that would double the wait and resurrect the silent-slow failure mode;
    # NA is returned so callers' own retry/timeout logic handles it.
    .lm_chat <- function(use_rf) {
      body <- list(model = model,
                   messages = list(list(role = "user", content = prompt)),
                   temperature = 0, stream = FALSE)
      if (isTRUE(json) && use_rf) body$response_format <- list(type = "json_object")
      resp <- request(paste0(LMSTUDIO_URL, "/v1/chat/completions")) %>%
        req_body_json(body) %>%
        req_timeout(timeout) %>% req_perform() %>% resp_body_json()  # cold load can be slow
      resp$choices[[1]]$message$content %||% NA_character_
    }
    return(tryCatch(.lm_chat(TRUE), error = function(e) {
      if (isTRUE(json) &&
          (inherits(e, "httr2_http_400") || inherits(e, "httr2_http_422") ||
           grepl("response_format|400|422", conditionMessage(e), ignore.case = TRUE)))
        tryCatch(.lm_chat(FALSE), error = function(e2) NA_character_)
      else NA_character_
    }))
  }

  # ---- Ollama backend (original) ----
  body <- list(model = model, prompt = prompt, stream = FALSE,
               options = list(temperature = 0, num_ctx = OLLAMA_NUM_CTX))
  if (isTRUE(json)) body$format <- "json"
  tryCatch({
    resp <- request(paste0(OLLAMA_URL, "/api/generate")) %>%
      req_body_json(body) %>%
      req_timeout(timeout) %>% req_perform() %>% resp_body_json()   # cold load can be slow
    resp$response %||% NA_character_
  }, error = function(e) NA_character_)
}

# ------------------------------------------------------------------------------
# extract_evidence: strict-JSON evidence extraction, schema-validated
# ------------------------------------------------------------------------------
.EXTRACT_SCHEMA_KEYS <- c("relevance_score", "counted", "implementation_status",
  "population", "organization", "lead_type", "is_humanitarian_data",
  "instrument_or_title", "year", "recommendation_referenced",
  "evidence_quote", "confidence")

# Document-body character budget for the LLM prompt. History: 6000 (v2, far
# too short) -> 20000 (v3) -> 12000 (v4). The v3 sample eval showed ~5 min per
# document at 20000 chars of PREFIX text, most of it navigation/front-matter.
# With smart_excerpt() below selecting keyword-dense windows, 12000 chars of
# RELEVANT text beats 20000 chars of prefix on both quality and speed.
MAX_DOC_CHARS <- as.integer(Sys.getenv("GAIN_LLM_MAX_DOC_CHARS", "12000"))

# ------------------------------------------------------------------------------
# SMART EXCERPTING: build the document slice from windows AROUND keyword hits
# rather than the first N characters. Rationale (proven by the v3 sample eval):
# in long official documents the section that PROVES inclusion - methodology,
# data sources, disaggregation tables - routinely sits tens of thousands of
# characters in, after contents pages and boilerplate. A prefix slice never
# reaches it; keyword-anchored windows find it wherever it lives.
# Compact multilingual pattern: population terms + inclusion/statistical terms.
# Base R only (this module deliberately has no tidyverse dependency).
# ------------------------------------------------------------------------------
# TWO TIERS of anchors. Population terms ("refugee"...) appear hundreds of
# times in exactly the documents we care about, so windows around them flood
# the budget with generic text (verified on the real Canada article: a naive
# single-pattern version STILL missed the IMDB methodology). METHOD terms -
# registers, disaggregation, record linkage, sampling frames - are rare and
# diagnostic: they mark the sentences that PROVE statistical inclusion.
.EXCERPT_METHOD_PAT <- paste0(
  "disaggregat|désagrég|desagregad|sampling frame|census|enumerat|",
  "population register|administrative (data|records|registers?)|longitudinal|",
  "record linkage|linked (data|records)|microdata|data sources?|methodolog|",
  "landing records?|tax (data|records|information)|",
  "recensement|encuesta|módulo|module|vital (statistics|registration)")
.EXCERPT_POP_PAT <- paste0(
  "refugee|asylum|réfugi|refugiad|internally displaced|\\bidps?\\b|displac|",
  "déplac|desplaz|stateless|apatrid|returnee|لاجئ|نازح|беженц|біженц|",
  "перемещ|переміщ|mülteci|vluchteling|flücht|flykting|flyktning")

# Block-scoring design: split the document into fixed 1500-char blocks, score
# each block by what it contains, keep the top-scoring blocks (plus the lead)
# up to the budget, reassembled in document order. Chosen over window-merging
# after testing on the real Canada article: merged keyword windows chain into
# giant segments in keyword-dense documents and the selection becomes too
# coarse to guarantee the methodology block survives. Fixed blocks make the
# granularity - and therefore the guarantee - predictable.
smart_excerpt <- function(text, budget = MAX_DOC_CHARS) {
  text <- .coal(text, "")
  # Drop invalid UTF-8 byte sequences (common in PDF-extracted text) so the
  # perl/ignore.case regex below never errors with "invalid UTF-8" - which
  # otherwise aborts the whole funnel mid-run. iconv UTF-8->UTF-8 with sub=""
  # is a no-op on already-valid text, so cache-neutral for clean documents.
  text <- iconv(text, "UTF-8", "UTF-8", sub = "")
  n <- nchar(text)
  if (n <= budget) return(text)
  bs <- 1500L
  nb <- as.integer(ceiling(n / bs))
  starts <- (seq_len(nb) - 1L) * bs + 1L
  pos_of <- function(pat) {
    p <- gregexpr(pat, text, ignore.case = TRUE, perl = TRUE)[[1]]
    if (p[1] == -1) integer(0) else as.integer(p)
  }
  hm <- pos_of(.EXCERPT_METHOD_PAT)   # rare, diagnostic
  hp <- pos_of(.EXCERPT_POP_PAT)      # common
  if (!length(hm) && !length(hp)) return(substr(text, 1, budget))
  blk <- function(p) pmin(nb, (p - 1L) %/% bs + 1L)
  cm <- tabulate(blk(hm), nb)         # method hits per block
  cp <- tabulate(blk(hp), nb)         # population hits per block
  # method counts double, and a block where method AND population co-occur
  # (the proof sentences: "refugees ... disaggregated ... register") gets a bonus
  score <- 2 * cm + cp + ifelse(cm > 0 & cp > 0, 4, 0)
  score[1] <- score[1] + 1000         # document lead: always kept for context
  k <- max(2L, as.integer(floor(budget / bs)))
  sel <- order(-score)[seq_len(min(k, nb))]
  sel <- sort(sel[score[sel] > 0])
  runs <- split(sel, cumsum(c(1, diff(sel) != 1)))   # adjacent blocks join up
  paste(vapply(runs, function(r) {
    substr(text, starts[r[1]], min(n, starts[r[length(r)]] + bs - 1L))
  }, character(1)), collapse = "\n[...]\n")
}

# ------------------------------------------------------------------------------
# RECOMMENDATION GROUNDING (optional): a short condensed excerpt of the actual
# EGRISS recommendations (IRRS/IRIS/IROSS definitions + inclusion mechanisms),
# prepended to every extract_evidence prompt so the LLM judges against the real
# framework text, not just our paraphrase of it. Backward-compatible no-op: if
# the grounding file doesn't exist, this returns "" and nothing changes.
#
# TO USE: put a short (~1500-2500 char) plain-text excerpt in a file named
# GAIN_RECOMMENDATION_GROUNDING.txt in this folder. Keep it SHORT - this is
# prepended to every single LLM call, so a long file slows every extraction
# and eats into the MAX_DOC_CHARS budget for the actual document. A distilled
# cheat-sheet (key definitions + the range of inclusion mechanisms the
# recommendations recognise) is far more useful here than the full text.
# GAIN_LLM_KEYWORD_SUGGEST.R can help you draft this excerpt from the full
# recommendation documents.
# ------------------------------------------------------------------------------
GROUNDING_FILE <- "GAIN_RECOMMENDATION_GROUNDING.txt"
.recommendation_grounding <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- if (file.exists(GROUNDING_FILE)) {
        txt <- paste(readLines(GROUNDING_FILE, warn = FALSE), collapse = "\n")
        paste0("REFERENCE - excerpt from the actual EGRISS recommendations ",
               "(use this to judge borderline cases; do not quote it back):\n",
               substr(txt, 1, 2500), "\n\n")
      } else ""
    }
    cached
  }
})

.build_extract_prompt <- function(text, title, country) {
  body <- paste(.coal(title, ""), "\n", smart_excerpt(.coal(text, "")))
  paste0(
.recommendation_grounding(),
"You assess statistical documents for the EGRISS GAIN exercise, which tracks whether ",
"forcibly displaced people (refugees, asylum-seekers, IDPs, returnees) and stateless ",
"people are INCLUDED in OFFICIAL statistics. Judge ONLY from the text; be literal and ",
"cautious and never assert a confirmed example. EGRISS frameworks are RECOMMENDATIONS ",
"(IRRS = refugees, IRIS = IDPs, IROSS = statelessness), NEVER 'standards'.\n\n",
"INCLUSION IS BROAD: counting/enumerating, disaggregating by displacement or nationality ",
"status, adding the group to a sampling frame, covering them in a census, survey, register ",
"or administrative-data source, or a dedicated module - ALL count. Host-community coverage ",
"alongside a displaced group also counts. Both PLANNED and IMPLEMENTED inclusion are relevant.\n\n",
"ANALYTICAL PRODUCTS COUNT: a study, article or statistical report that ANALYSES displaced ",
"or stateless people using census, survey, register or linked administrative data (for ",
"example, outcomes of refugees disaggregated by admission category from a longitudinal ",
"immigration database) IS evidence of IMPLEMENTED inclusion - the underlying official data ",
"system identifies and disaggregates the group. Set counted=true and score by the bands below.\n\n",
"SCORE BANDS (be moderately conservative, but do NOT give 0 to anything plausibly related; ",
"reserve 0 only for documents with no plausible connection to displaced/stateless statistics):\n",
"  80-100  a displaced/stateless group is concretely included in an official statistical\n",
"          activity (enumerated, disaggregated, dedicated module, in a frame/register), implemented\n",
"  60-79   included but limited (e.g. covered without disaggregation), implemented or clearly underway\n",
"  40-59   inclusion PLANNED/intended, or described at a general/landscape level (still relevant)\n",
"  20-39   displacement/statelessness topic is present and plausibly relevant, but inclusion in\n",
"          official statistics is unclear from the text\n",
"  0-19    clearly unrelated to including displaced/stateless people in official statistics\n\n",
"FLAG HUMANITARIAN DATA: set is_humanitarian_data=true when the producer is a humanitarian actor ",
"(UNHCR, WFP, IOM, UNICEF, REACH, NRC, etc.) and the product is operational/M&E data (results ",
"monitoring, MSNA, protection monitoring, post-distribution, vulnerability assessment) rather than ",
"an official NSO statistical product. These are lower GAIN priority unless an NSO is involved.\n\n",
"COUNTRY (hint): ", .coal(country, "unknown"), "\n",
"DOCUMENT:\n", body, "\n\n",
"Return ONE JSON object, no prose, with EXACTLY these keys:\n",
'{\n',
'  "relevance_score": 0,                  // 0-100 per the bands above\n',
'  "counted": false,                       // true if a displaced/stateless group is actually included (counted/disaggregated/in a frame/register/module)\n',
'  "implementation_status": "unclear",     // "implemented" | "planned" | "unclear"\n',
'  "population": [],                       // any of "refugees","idps","stateless","returnees" actually covered\n',
'  "organization": "",                     // the producing body, as named in the text\n',
'  "lead_type": "unclear",                 // "country-led" (NSO/national stat system) | "partner-led" (international/humanitarian org) | "unclear"\n',
'  "is_humanitarian_data": false,          // true if humanitarian operational/M&E data (see above)\n',
'  "instrument_or_title": "",              // the survey/census/register/report this is\n',
'  "year": "",                             // 4-digit reference year, or ""\n',
'  "recommendation_referenced": "",        // EGRISS/IRRS/IRIS/IROSS if cited, else ""\n',
'  "evidence_quote": "",                   // <=20 words, verbatim, showing the inclusion; else ""\n',
'  "confidence": "low"                     // low | medium | high\n',
'}')
}

.validate_extract <- function(txt) {
  if (is.na(txt)) return(NULL)
  j <- regmatches(txt, regexpr("(?s)\\{.*\\}", txt, perl = TRUE))
  obj <- tryCatch(fromJSON(if (length(j)) j else txt), error = function(e) NULL)
  if (is.null(obj) || !is.list(obj) || !all(.EXTRACT_SCHEMA_KEYS %in% names(obj))) return(NULL)
  as_bool <- function(x) isTRUE(x) || identical(tolower(as.character(x %||% "")), "true")
  conf <- tolower(as.character(obj$confidence %||% "low"))
  if (!conf %in% c("low", "medium", "high")) conf <- "low"
  score <- suppressWarnings(as.numeric(obj$relevance_score %||% 0))
  if (is.na(score)) score <- 0
  impl <- tolower(as.character(obj$implementation_status %||% "unclear"))
  if (!impl %in% c("implemented", "planned", "unclear")) impl <- "unclear"
  lead <- tolower(as.character(obj$lead_type %||% "unclear"))
  if (!lead %in% c("country-led", "partner-led", "unclear")) lead <- "unclear"
  list(
    relevance_score = max(0, min(100, score)),
    counted = as_bool(obj$counted),
    implementation_status = impl,
    population = paste(unlist(obj$population) %||% character(0), collapse = "; "),
    organization = as.character(obj$organization %||% ""),
    lead_type = lead,
    is_humanitarian_data = as_bool(obj$is_humanitarian_data),
    instrument_or_title = as.character(obj$instrument_or_title %||% ""),
    year = (regmatches(as.character(obj$year %||% ""),
                       regexpr("20\\d{2}", as.character(obj$year %||% ""))) %||% "")[1] %||% "",
    recommendation_referenced = as.character(obj$recommendation_referenced %||% ""),
    evidence_quote = substr(as.character(obj$evidence_quote %||% ""), 1, 300),
    confidence = conf
  )
}

# The extract cache key includes the grounding file's CONTENT: activating or
# editing GAIN_RECOMMENDATION_GROUNDING.txt changes what the model is told, so
# it must invalidate cached results. (Without this, grounding would silently
# never apply to any already-cached document.) Note the grounding is memoised
# per session - scripts run fresh via Rscript, so this is always current there.
.extract_cache_key <- function(title, text)
  rlang::hash(paste(PROMPT_VERSION, OLLAMA_GEN_MODEL,
                    rlang::hash(.recommendation_grounding()),
                    .coal(title, ""), .coal(text, "")))

extract_evidence <- function(text, title = "", country = "") {
  key <- .extract_cache_key(title, text)
  cached <- .cache_get("extract", key)
  if (!is.null(cached)) { .log_decision(c(cached$log, cache = "hit")); return(cached$val) }

  # Extract calls get a LONGER timeout than the 300s default and ONE retry.
  # Why: the v2 decision log showed 18.6% of extract calls silently returning
  # NA - mostly requests killed by the old fixed 300s timeout (cold model
  # loads, long documents). v3 raised the document budget 6000->20000 chars,
  # which makes calls SLOWER, so without this the silent-failure rate would
  # have gone UP on the re-run. The retry also covers transient Ollama hiccups
  # (the second attempt hits a warm model, which is typically much faster).
  EXTRACT_TIMEOUT <- as.integer(Sys.getenv("GAIN_LLM_TIMEOUT", "600"))
  prompt <- .build_extract_prompt(text, title, country)
  raw <- .ollama_generate(prompt, timeout = EXTRACT_TIMEOUT)
  if (is.na(raw)) raw <- .ollama_generate(prompt, timeout = EXTRACT_TIMEOUT)  # one retry
  val <- .validate_extract(raw)
  # split the failure modes in the log: "no-response" = timeout/network (fix:
  # raise GAIN_LLM_TIMEOUT), "invalid-json" = model returned unparseable output
  # (fix: prompt/schema) - previously merged, which hid the timeout epidemic
  status <- if (!is.null(val)) "ok" else if (is.na(raw)) "no-response" else "invalid-json"
  log <- list(title = substr(.coal(title, ""), 1, 80),
              country = .coal(country, ""), status = status,
              relevance = if (is.null(val)) NA else val$relevance_score,
              counted = if (is.null(val)) NA else val$counted,
              implementation_status = if (is.null(val)) NA else val$implementation_status,
              lead_type = if (is.null(val)) NA else val$lead_type,
              is_humanitarian_data = if (is.null(val)) NA else val$is_humanitarian_data,
              confidence = if (is.null(val)) NA else val$confidence)
  if (!is.null(val)) .cache_put("extract", key, list(val = val, log = log))  # cache only valid
  .log_decision(c(log, cache = "miss"))
  val
}

# ------------------------------------------------------------------------------
# embed: bge-m3 multilingual embedding (tries /api/embed then /api/embeddings)
# ------------------------------------------------------------------------------
embed <- function(text) {
  text <- .coal(text, "")
  text <- iconv(text, "UTF-8", "UTF-8", sub = "")   # strip invalid UTF-8 before hashing/HTTP
  if (nchar(trimws(text)) == 0) return(NULL)
  # No embedding model available (typical on LM Studio if only a chat model is
  # loaded): degrade gracefully to NULL with a single clear message, rather
  # than hammering a non-existent endpoint. Callers (funnel/crossref) already
  # treat NULL embeddings as "skip the semantic step".
  if (!nzchar(OLLAMA_EMB_MODEL)) {
    if (!isTRUE(.embed_warned)) {
      message("embed(): no embedding model configured for backend '", LLM_BACKEND,
              "'. Load one (e.g. bge-m3 / nomic-embed-text) or set GAIN_EMB_MODEL. ",
              "Returning NULL; the semantic funnel/crossref will be skipped.")
      .embed_warned <<- TRUE
    }
    return(NULL)
  }
  # EMB_VERSION guards against silently reusing stale embeddings if the
  # truncation budget or preprocessing here ever changes (same class of bug
  # just fixed for extract_evidence via PROMPT_VERSION).
  key <- rlang::hash(paste(EMB_VERSION, OLLAMA_EMB_MODEL, substr(text, 1, 8000)))
  cached <- .cache_get("embed", key)
  if (!is.null(cached)) return(cached)

  if (LLM_BACKEND == "lmstudio") {
    vec <- tryCatch({
      resp <- request(paste0(LMSTUDIO_URL, "/v1/embeddings")) %>%
        req_body_json(list(model = OLLAMA_EMB_MODEL, input = substr(text, 1, 8000))) %>%
        req_timeout(60) %>% req_perform() %>% resp_body_json()
      as.numeric(resp$data[[1]]$embedding)
    }, error = function(e) NULL)
  } else {
    vec <- tryCatch({
      resp <- request(paste0(OLLAMA_URL, "/api/embed")) %>%
        req_body_json(list(model = OLLAMA_EMB_MODEL, input = substr(text, 1, 8000))) %>%
        req_timeout(60) %>% req_perform() %>% resp_body_json()
      as.numeric(resp$embeddings[[1]])
    }, error = function(e) NULL)
    if (is.null(vec) || length(vec) == 0) {       # older Ollama: /api/embeddings
      vec <- tryCatch({
        resp <- request(paste0(OLLAMA_URL, "/api/embeddings")) %>%
          req_body_json(list(model = OLLAMA_EMB_MODEL, prompt = substr(text, 1, 8000))) %>%
          req_timeout(60) %>% req_perform() %>% resp_body_json()
        as.numeric(resp$embedding)
      }, error = function(e) NULL)
    }
  }
  if (!is.null(vec) && length(vec) > 0) .cache_put("embed", key, vec)
  vec
}

# ------------------------------------------------------------------------------
# Shared text builders - used by BOTH the funnel and the crossref so embed()
# cache keys are identical across scripts (no recompute).
# ------------------------------------------------------------------------------
gain_seed_text <- function(pro03, morg, country, has_r, has_i, has_s, pro08) {
  trimws(gsub("\\s+", " ", paste(
    .coal(pro03, ""), .coal(morg, ""), .coal(country, ""),
    if (isTRUE(has_r)) "refugees" else "",
    if (isTRUE(has_i)) "idps" else "",
    if (isTRUE(has_s)) "stateless" else "",
    .coal(pro08, ""))))
}
record_embed_text <- function(title, summary = "", ctx_ref = "", ctx_idp = "",
                              ctx_sta = "", working = "") {
  substr(trimws(gsub("\\s+", " ", paste(.coal(title, ""), .coal(summary, ""),
    .coal(ctx_ref, ""), .coal(ctx_idp, ""), .coal(ctx_sta, ""), .coal(working, "")))),
    1, 4000)
}

# ------------------------------------------------------------------------------
# adjudicate_match: does a discovered artifact == a known GAIN example?
# qwen2.5, JSON-forced, cached by the pair, logged.
# ------------------------------------------------------------------------------
adjudicate_match <- function(artifact, example) {
  key <- rlang::hash(paste("adj", ADJ_PROMPT_VERSION, OLLAMA_GEN_MODEL,
    paste(unlist(artifact), collapse = "|"), paste(unlist(example), collapse = "|")))
  cached <- .cache_get("adjudicate", key)
  if (!is.null(cached)) return(cached)

  prompt <- paste0(
"Decide whether a DISCOVERED statistical artifact and a KNOWN GAIN example describe the ",
"SAME statistical activity/example. Same = same country AND same producing body AND the ",
"same instrument/project, allowing for wording or translation differences and a +/-1 year ",
"gap. Different instruments by the same office are NOT the same example. Judge literally.\n\n",
"DISCOVERED ARTIFACT:\n",
"  country: ", .coal(artifact$country, ""), "\n  organization: ", .coal(artifact$organization, ""),
"\n  instrument/title: ", .coal(artifact$instrument_or_title, ""), "\n  year: ", .coal(artifact$year, ""),
"\n  population: ", .coal(artifact$population, ""), "\n\n",
"KNOWN GAIN EXAMPLE:\n",
"  country: ", .coal(example$country, ""), "\n  organization: ", .coal(example$organization, ""),
"\n  title: ", .coal(example$title, ""), "\n  year: ", .coal(example$year, ""),
"\n  population: ", .coal(example$population, ""), "\n\n",
"Return ONE JSON object, no prose:\n",
'{ "same_example": false, "confidence": "low", "reason": "" }   // confidence: low|medium|high; reason <=20 words')

  raw <- .ollama_generate(prompt)
  obj <- NULL
  if (!is.na(raw)) {
    j <- regmatches(raw, regexpr("(?s)\\{.*\\}", raw, perl = TRUE))
    obj <- tryCatch(fromJSON(if (length(j)) j else raw), error = function(e) NULL)
  }
  same <- isTRUE(obj$same_example) || identical(tolower(as.character(obj$same_example %||% "")), "true")
  conf <- tolower(as.character(obj$confidence %||% "low"))
  if (!conf %in% c("low", "medium", "high")) conf <- "low"
  out <- list(same_example = if (is.null(obj)) NA else same,
              confidence = if (is.null(obj)) "low" else conf,
              reason = substr(as.character(obj$reason %||% ""), 1, 200),
              status = if (is.null(obj)) "invalid/no-response" else "ok")
  if (!is.null(obj)) .cache_put("adjudicate", key, out)
  .log_decision(list(title = substr(.coal(artifact$instrument_or_title, ""), 1, 60),
                     country = .coal(artifact$country, ""), status = paste0("adj:", out$status),
                     relevance = NA, counted = NA, implementation_status = NA,
                     lead_type = NA, is_humanitarian_data = NA,
                     confidence = out$confidence, cache = "miss"))
  out
}

# cosine similarity helper (used by the funnel / crossref)
cosine_sim <- function(a, b) {
  if (is.null(a) || is.null(b) || length(a) != length(b)) return(NA_real_)
  d <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (d == 0) NA_real_ else sum(a * b) / d
}

# ------------------------------------------------------------------------------
# draft_outreach_email: LLM-drafted, per-NSO survey invitation, grounded in
# Dillman's Tailored Design (social exchange) + an optional local guidance file.
# English + local-language versions. Cached by content (idempotent, reviewable).
# Drop a text (a Calgaro/other web-survey paper, distilled) into
# GAIN_EMAIL_GROUNDING.txt and it is prepended to every draft prompt.
# ------------------------------------------------------------------------------
EMAIL_PROMPT_VERSION <- "email-v12"  # v12: opener no name/org/meta/future; body French elision + no invented provenance; no dashes
GAIN_EMAIL_GROUNDING_FILE <- "shared/prompts/GAIN_EMAIL_GROUNDING.txt"
# STYLE (section 8): no em/en dashes anywhere. Replace with a comma; collapse spaces.
.no_dash <- function(s) { s <- gsub("\\s*[\u2014\u2013]\\s*", ", ", .coal(s, "")); gsub("[[:space:]]+", " ", s) }
.email_grounding <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) cached <<- if (file.exists(GAIN_EMAIL_GROUNDING_FILE)) {
        paste0("ADDITIONAL DRAFTING GUIDANCE (an expert source the user supplied - ",
               "follow it alongside Dillman):\n",
               substr(paste(readLines(GAIN_EMAIL_GROUNDING_FILE, warn = FALSE), collapse = "\n"),
                      1, 3500), "\n\n")
      } else ""
    cached
  }
})

draft_outreach_email <- function(country, organization, recipients, examples,
                                 respondent_status = "", survey_link = "", sender_name = "",
                                 deadline = "", translate_to = "", connection = "", next_round = "") {
  # SECTION 4 (v11): returns ONE general, HEDGED sentence (20-35 words) about the
  # material only. It does NOT itemise/name/describe individual outputs (the linked
  # list below does that), asserts nothing about ownership/scope, invents no
  # provenance, and NEVER mentions the recipient, a prior round, a workshop or
  # membership (that is the opener's job). Validated + one stricter retry.
  titles <- .coal(examples$title, "")
  ex_block <- paste(sprintf("- %s (%s): %s", titles, .coal(examples$year, ""),
                    substr(.coal(examples$quote, ""), 1, 120)), collapse = "\n")
  key <- rlang::hash(paste(EMAIL_PROMPT_VERSION, OLLAMA_GEN_MODEL, .email_grounding(),
    country, organization, ex_block, translate_to))
  cached <- .cache_get("email", key); if (!is.null(cached)) return(cached)

  lang_instr <- if (nzchar(translate_to))
    paste0("Write the sentence in ", translate_to, " (formal register: Spanish MUST use 'usted' not 'tú'; ",
           "French MUST use correct elision, e.g. \"d'Ukraine\" not \"du Ukraine\"), then a line containing exactly ",
           "--- , then a faithful English rendering of the same sentence.")
  else "Write the sentence in English."

  mk_prompt <- function(extra) paste0(
   .email_grounding(),
   "You draft ONE sentence (20-35 words MAX) for a formal email from the EGRISS Secretariat (hosted by ",
   "UNHCR) to an institution. You are given the organisation name, country, and 1-3 candidate statistical ",
   "outputs found by automated search of the organisation's public website. A bulleted, linked list of ",
   "those outputs appears immediately BELOW your sentence, so you must NOT itemise them.\n\n",
   "Write ONE sentence that:\n",
   " - says in GENERAL terms what the material appears to be (e.g. statistics on refugees, internally ",
   "displaced or stateless persons) and why it may fit the GAIN survey\n",
   " - does NOT name, list, quote, or describe any individual output, and contains NO colon-led list\n",
   " - introduces NO fact absent from the input: no data sources, NO provenance (do NOT say where the ",
   "data come from), no methodology, no figures, no dates\n",
   " - NEVER mentions the recipient, a prior survey round, a workshop or EGRISS membership (you do not have that information)\n",
   " - is hedged ('appears', 'may be'), formal register, no praise, no exclamation marks, no dashes\n",
   " - contains NO greeting, sign-off, hyperlink, recipient name, or the request itself\n\n",
   "Output the single sentence only. No preamble, no quotation marks, no list, no heading.\n",
   extra, lang_instr, "\n\n",
   "ORGANISATION: ", organization, "\nCOUNTRY: ", country, "\nCANDIDATE OUTPUTS (for context only - do NOT copy their titles):\n", ex_block)

  parse_out <- function(raw) {
    pe <- ""; pl <- ""
    if (!is.na(raw)) { txt <- sub('^"(.*)"$', "\\1", trimws(raw))
      if (nzchar(translate_to) && grepl("(?m)^[[:space:]]*---[[:space:]]*$", txt, perl = TRUE)) {
        parts <- strsplit(txt, "(?m)^[[:space:]]*---[[:space:]]*$", perl = TRUE)[[1]]
        pl <- trimws(parts[1]); pe <- trimws(paste(parts[-1], collapse = "\n"))
      } else pe <- txt }
    list(pe = .no_dash(pe), pl = .no_dash(pl))
  }
  bad <- function(pe) {
    if (!nzchar(pe)) return(TRUE)
    nw <- length(str_split(str_squish(pe), " ")[[1]])
    if (nw > 38) return(TRUE)                                   # too long
    if (grepl(":", pe)) return(TRUE)                            # colon-led list
    if (!grepl("[.!?][\"'»]?\\s*$", pe)) return(TRUE)      # truncated token / no end punctuation
    if (grepl("official register|administrative register|as documented in|according to|sourced from|data come from|drawn from the", pe, ignore.case = TRUE)) return(TRUE)  # invented provenance
    if (any(nzchar(titles) & vapply(titles, function(t) nchar(t) > 12 && grepl(t, pe, fixed = TRUE), logical(1)))) return(TRUE)  # copied a title
    FALSE
  }
  o <- parse_out(.ollama_generate(mk_prompt(""), timeout = 300, json = FALSE))
  if (bad(o$pe)) o <- parse_out(.ollama_generate(mk_prompt(
    "YOUR PREVIOUS ATTEMPT FAILED: it was too long, listed items, or copied a title. Write ONE short, GENERAL "
    ), timeout = 300, json = FALSE))   # one stricter retry (temp 0, but the prompt differs)

  out <- list(para_en = o$pe, para_local = o$pl,
              local_language = if (nzchar(translate_to)) translate_to else "English",
              flagged = bad(o$pe), status = if (nzchar(o$pe)) "ok" else "failed")
  if (nzchar(o$pe)) .cache_put("email", key, out)
  out
}

# ------------------------------------------------------------------------------
# draft_opener (SECTION 3b/3c): rewords a VERIFIED facts object into the opening
# 1-2 sentences. The model may only reword the facts given; a code validator
# (bad_open) rejects any hallucinated year/city/event/round or praise word, and
# the caller falls back to a fixed template. Returns list(text, fell_back).
#   facts_txt : human-readable lines of the selected (<=2) facts
#   allow_years / allow_places : tokens permitted to appear (from THIS contact's facts)
#   gaz_years / gaz_places     : the FULL gazetteer (anything here but not allowed -> reject)
# ------------------------------------------------------------------------------
draft_opener <- function(facts_json, facts_txt, organization, lang,
                         allow_years, allow_places, gaz_years, gaz_places) {
  key <- rlang::hash(paste("open4", EMAIL_PROMPT_VERSION, OLLAMA_GEN_MODEL, facts_json, organization, lang))
  cached <- .cache_get("email", key); if (!is.null(cached)) return(cached)
  prompt <- paste0(
    "You are given a JSON object of VERIFIED facts about a person our team already knows. ",
    "Write the opening one or two sentences of a formal email that acknowledge these facts naturally.\n\n",
    "Absolute rules:\n",
    " - Use ONLY the facts in the object. You know nothing else about this person.\n",
    " - Write in the FIRST PERSON PLURAL (we). Do NOT restate the recipient's name or the organisation name, ",
    "and do NOT write a salutation.\n",
    " - Every fact is in the PAST. Never imply a future meeting or event, and never invent a date.\n",
    " - Do not add a year, city, event, job title, survey round or any other detail not in the object.\n",
    " - Do NOT refer to 'facts', 'records', 'data', 'the object', 'information provided' or 'verified'; ",
    "just say the thing warmly and plainly.\n",
    " - Do not invent warmth you cannot support. If the only fact is EGRISS membership, do not write ",
    "'it was good to meet you'.\n",
    " - 15 to 40 words. Formal, warm, plain. No praise, no exclamation marks, no dashes.\n",
    " - Do not mention the material we found; that comes later.\n",
    " - Write in this language: ", if (nzchar(lang)) lang else "English", ".\n\n",
    "Output the sentences only.\n\nFACTS (JSON):\n", facts_json)
  raw <- .ollama_generate(prompt, timeout = 240, json = FALSE)
  txt <- if (is.na(raw)) "" else .no_dash(sub('^"(.*)"$', "\\1", trimws(raw)))

  bad_open <- function(s) {
    if (!nzchar(s)) return(TRUE)
    if (length(str_split(str_squish(s), " ")[[1]]) > 45) return(TRUE)
    if (grepl("congratulat|delighted|excited|valuable partner|honou?red|privilege|thrilled|felicit|encantad|complac|enchant", s, ignore.case = TRUE)) return(TRUE)  # strong praise (warmth is allowed - the spec wants warm)
    if (grepl("\\b(verified|the facts|information (you )?provided|as recorded|the object)\\b", s, ignore.case = TRUE)) return(TRUE)  # meta leak
    if (grepl("re-?engagement|ongoing commitment|renewed commitment|recommit|continued partnership", s, ignore.case = TRUE)) return(TRUE)  # invented relationship framing
    yrs <- unique(str_extract_all(s, "\\b(19|20)\\d{2}\\b")[[1]])
    if (any(!yrs %in% allow_years)) return(TRUE)                       # a year not in the facts
    off <- setdiff(gaz_places, allow_places)
    if (length(off) && any(vapply(off, function(p) grepl(p, s, ignore.case = TRUE), logical(1)))) return(TRUE)
    if (nzchar(organization) && lengths(regmatches(s, gregexpr(organization, s, fixed = TRUE))) > 1) return(TRUE)
    FALSE
  }
  fell <- bad_open(txt)
  out <- list(text = if (fell) "" else txt, fell_back = fell)
  .cache_put("email", key, out); out
}

# subject_referent: a SHORT natural referent (3-7 words) in the body language for
# the subject line (F5). No quotes, no colon, no title. Cached.
subject_referent <- function(examples, organization, lang) {
  ex <- paste(head(.coal(examples$title, ""), 3), collapse = "; ")
  key <- rlang::hash(paste("ref1", OLLAMA_GEN_MODEL, ex, lang))
  c0 <- .cache_get("email", key); if (!is.null(c0)) return(c0)
  raw <- .ollama_generate(paste0(
    "Write a SHORT natural referent of 3-7 words describing the common THEME of these statistical outputs, ",
    "in ", if (nzchar(lang)) lang else "English", ", plain language, for an email subject line (e.g. ",
    "'your work on refugee economic outcomes'). Do NOT copy a title, use no quotation marks, no colon. ",
    "Output only the phrase.\n\nOUTPUTS:\n", ex), timeout = 120, json = FALSE)
  out <- if (is.na(raw)) "" else .no_dash(str_squish(gsub('["“”:]', "", sub("^[^:]*:\\s*", "", trimws(raw)))))
  .cache_put("email", key, out); out
}

# translate_text: faithful translation of a short fixed notice into a target
# language (for the localized email footer). Cached; returns the input unchanged
# if target is empty or the call fails.
translate_text <- function(text, target_lang) {
  text <- .coal(text, ""); if (!nzchar(text) || !nzchar(target_lang)) return(text)
  key <- rlang::hash(paste("tr2", OLLAMA_GEN_MODEL, target_lang, text))
  c0 <- .cache_get("translate", key); if (!is.null(c0)) return(c0)
  hygiene <- if (grepl("span", target_lang, ignore.case = TRUE))
      " Use the formal 'usted' throughout (NEVER 'tú' or 'vosotros'), as this addresses a national statistical office."
    else if (grepl("french|fran", target_lang, ignore.case = TRUE))
      " Use the formal 'vous' and correct elision (e.g. 'd'Ukraine', not 'du Ukraine')."
    else ""
  raw <- .ollama_generate(paste0(
    "Translate the text below into ", target_lang, ", faithfully and naturally, in a register ",
    "appropriate for an official statistical audience.", hygiene, " Return ONLY the translation - no quotes, ",
    "no notes, no preamble.\n\nTEXT:\n", text), timeout = 120, json = FALSE)
  out <- if (is.na(raw)) text else sub("^\\s*(translation|traduction|traducci[oó]n)\\s*:?\\s*", "",
                                       trimws(raw), ignore.case = TRUE)
  out <- .no_dash(out)                                   # STYLE (section 8): no dashes
  .cache_put("translate", key, out); out
}

if (identical(environment(), globalenv()) && !exists(".GAIN_OLLAMA_SOURCED")) {
  .GAIN_OLLAMA_SOURCED <- TRUE
  message("GAIN LLM helpers loaded. Backend: ", LLM_BACKEND,
          " | url: ", if (LLM_BACKEND == "lmstudio") LMSTUDIO_URL else OLLAMA_URL,
          " | gen: ", OLLAMA_GEN_MODEL,
          " | emb: ", if (nzchar(OLLAMA_EMB_MODEL)) OLLAMA_EMB_MODEL else "(none)")
  message("  Reachable: ", ollama_available())
}
