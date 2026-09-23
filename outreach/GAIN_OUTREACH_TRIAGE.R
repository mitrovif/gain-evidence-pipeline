# ==============================================================================
# GAIN OUTREACH TRIAGE  (classify replies with the local LLM)
#
# Reads GAIN_OUTREACH_LOG.csv and, for every row that has a reply captured
# (responded = TRUE, response_summary filled), asks the LOCAL Ollama model to
# sort the reply into an OUTCOME so the dashboard shows who is interested - not
# just who replied.
#
# HOW THE LLM WORKS HERE:
#   * Everything runs on your machine via Ollama (http://localhost:11434). No
#     reply text leaves the computer.
#   * Model qwen2.5:7b reads the reply snippet and returns strict JSON:
#       {"outcome": one of [interested, will_submit, declined, needs_info,
#                           out_of_office, unclear], "reason": short}
#   * temperature 0 (deterministic) and cached by content hash (ollama_cache/),
#     so the same reply is never re-classified and a re-run is instant.
#   * out_of_office auto-replies are NOT counted as a response: the row is reset
#     to status=sent with a follow_up_date in a week.
#   * If Ollama is not running, the script no-ops (outcomes stay blank) - nothing
#     breaks, and you can still set outcome by hand in the log.
# ==============================================================================

suppressMessages({ library(tidyverse); library(httr2); library(jsonlite) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
OUTCOMES <- c("interested","will_submit","declined","needs_info","out_of_office","unclear")
LOG <- "GAIN_OUTREACH_LOG.csv"
dir.create("ollama_cache", showWarnings = FALSE)

# Backend: Ollama (original) or LM Studio (OpenAI-compatible, e.g. this Mac).
# Auto-detect; override with GAIN_LLM_BACKEND / GAIN_LLM_MODEL / *_URL. Reply
# text still never leaves the machine - both backends are localhost.
BACKEND    <- tolower(Sys.getenv("GAIN_LLM_BACKEND", "auto"))
OLLAMA_URL <- Sys.getenv("OLLAMA_URL",   "http://localhost:11434")
LMSTU_URL  <- Sys.getenv("LMSTUDIO_URL", "http://localhost:1234")
.probe <- function(u, p) tryCatch({ request(paste0(u, p)) %>% req_timeout(2) %>%
                                      req_perform(); TRUE }, error = function(e) FALSE)
BACKEND <- if (BACKEND %in% c("lmstudio","openai")) "lmstudio" else
           if (BACKEND == "ollama")                 "ollama"   else
           if (.probe(LMSTU_URL, "/v1/models"))     "lmstudio" else
           if (.probe(OLLAMA_URL, "/api/tags"))     "ollama"   else "lmstudio"

.lm_ids <- function() tryCatch({
  resp <- request(paste0(LMSTU_URL, "/v1/models")) %>% req_timeout(3) %>%
            req_perform() %>% resp_body_json()
  vapply(resp$data, function(m) m$id %||% "", character(1))
}, error = function(e) character(0))

MODEL <- Sys.getenv("GAIN_LLM_MODEL", "")
if (!nzchar(MODEL)) {
  if (BACKEND == "lmstudio") {
    ids <- .lm_ids()
    qw  <- ids[grepl("qwen", ids, ignore.case = TRUE) &
               !grepl("embed|bge", ids, ignore.case = TRUE)]
    MODEL <- c(qw, ids, "qwen2.5-7b-instruct")[1]
  } else MODEL <- "qwen2.5:7b"
}

# LM Studio chat call. Sends response_format=json_object; if an older build
# rejects it (HTTP 4xx) we retry without it - the prompt already demands JSON.
.lm_generate <- function(prompt, timeout = 120) {
  call <- function(use_rf) {
    body <- list(model = MODEL, messages = list(list(role = "user", content = prompt)),
                 temperature = 0, stream = FALSE)
    if (use_rf) body$response_format <- list(type = "json_object")
    resp <- request(paste0(LMSTU_URL, "/v1/chat/completions")) %>%
      req_body_json(body) %>% req_timeout(timeout) %>% req_perform() %>% resp_body_json()
    resp$choices[[1]]$message$content
  }
  tryCatch(call(TRUE), error = function(e)
    if (inherits(e, "httr2_http_400") || inherits(e, "httr2_http_422"))
      tryCatch(call(FALSE), error = function(e2) NULL) else NULL)
}

# Name kept for the call site below; checks whichever backend is active.
ollama_up <- function() {
  if (BACKEND == "lmstudio") length(.lm_ids()) > 0
  else isTRUE(tryCatch(request(paste0(OLLAMA_URL, "/api/tags")) %>% req_timeout(5) %>%
                         req_perform() %>% resp_status(), error = function(e) NA) == 200)
}

classify_reply <- function(text) {
  text <- str_squish(coalesce(text, ""))
  if (!nzchar(text)) return(list(outcome = "unclear", reason = ""))
  cache <- file.path("ollama_cache", paste0("triage_", substr(rlang::hash(text), 1, 16), ".rds"))
  if (file.exists(cache)) return(readRDS(cache))
  prompt <- paste0(
    "You sort replies from national statistics offices about contributing examples to the ",
    "EGRISS GAIN survey on including displaced and stateless people in official statistics.\n",
    "Reply:\n\"\"\"\n", substr(text, 1, 1500), "\n\"\"\"\n",
    "Classify the sender's intent. Return ONLY JSON: ",
    "{\"outcome\":\"<one of: interested, will_submit, declined, needs_info, out_of_office, unclear>\",",
    "\"reason\":\"<5 to 12 words>\"}")
  resp_text <- if (BACKEND == "lmstudio") .lm_generate(prompt, 120) else {
    res <- tryCatch(
      request(paste0(OLLAMA_URL, "/api/generate")) %>%
        req_body_json(list(model = MODEL, prompt = prompt, format = "json", stream = FALSE,
                           options = list(temperature = 0))) %>%
        req_timeout(120) %>% req_perform() %>% resp_body_json(),
      error = function(e) NULL)
    if (!is.null(res)) res$response else NULL
  }
  out <- list(outcome = "unclear", reason = "")
  if (!is.null(resp_text)) {
    j <- tryCatch(fromJSON(resp_text), error = function(e) NULL)
    oc <- tolower(as.character(j$outcome %||% "unclear"))
    if (!oc %in% OUTCOMES) oc <- "unclear"
    out <- list(outcome = oc, reason = as.character(j$reason %||% ""))
  }
  saveRDS(out, cache); out
}

if (!file.exists(LOG)) stop("No ", LOG, " - run GAIN_OUTREACH_LOG.R / GAIN_OUTREACH_SYNC.R first.")
log <- read_csv(LOG, show_col_types = FALSE) %>% mutate(across(everything(), as.character))

todo <- which(log$responded %in% c("TRUE","true") &
              nzchar(coalesce(log$response_summary, "")) &
              coalesce(log$outcome, "") == "")
if (!ollama_up()) {
  message("Local LLM backend '", BACKEND, "' is not reachable - skipping LLM triage ",
          "(outcomes left blank). Start ",
          if (BACKEND == "lmstudio") paste0("LM Studio's server at ", LMSTU_URL,
                                             " and load ", MODEL)
          else paste0("Ollama (", MODEL, ")"), ", then re-run.")
} else if (length(todo) == 0) {
  message("No replies awaiting triage (need responded=TRUE + a response_summary + blank outcome).")
} else {
  message("Triaging ", length(todo), " replies with ", MODEL, " (cached; local)...")
  for (i in todo) {
    r <- classify_reply(log$response_summary[i])
    log$outcome[i] <- r$outcome
    log$comments[i] <- str_squish(paste(coalesce(log$comments[i], ""),
                                        if (nzchar(r$reason)) paste0("[LLM: ", r$reason, "]") else ""))
    if (r$outcome == "out_of_office") {   # auto-reply: not a real response
      log$responded[i] <- "FALSE"; log$status[i] <- "sent"
      log$date_responded[i] <- ""; log$response_summary[i] <- ""
      log$follow_up_date[i] <- as.character(Sys.Date() + 7)
    }
  }
  write_excel_csv(log, LOG)
  if (dir.exists("powerbi_export"))
    write_excel_csv(log, file.path("powerbi_export", "WEB_GAIN_outreach_log.csv"))
  message("Done. Outcome breakdown:")
  print(as.data.frame(table(outcome = log$outcome[nzchar(coalesce(log$outcome, ""))])))
  message("Log + powerbi_export/WEB_GAIN_outreach_log.csv updated.")
}
