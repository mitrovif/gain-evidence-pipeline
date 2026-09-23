# ==============================================================================
# GAIN OUTREACH CONFIGURATION  (edit HERE, once - every outreach script reads it)
#
# These values used to be duplicated at the top of GAIN_OUTREACH_TARGETS.R and
# GAIN_OUTREACH_FOLLOWUP.R, so editing one script but not the other could send
# follow-ups with a literal "[YOUR NAME]" in them. Now there is one place.
# ==============================================================================

# --- sender / signature (A6: a cold gov contact needs a verifiable human + phone) --
SENDER_NAME   <- "Filip Mitrović"                 # full name (signature)
SENDER_TITLE  <- "[your role, e.g. Statistician]"  # role/title - EDIT before sending
SENDER_PHONE  <- "[+41 XX XXX XX XX]"             # institutional phone - EDIT before sending
SENDER_EMAIL  <- "[name]@unhcr.org"               # institutional @unhcr.org address - EDIT before sending

# --- survey object (section 6: the ONLY place dates/burden/counts live) -----------
# Confirmed 2026-07-31: inviting to the GAIN 2026 round; exact open/close dates are
# not yet set, so they stay blank (the next-step sentence reads "when the round opens
# later this year"). No literal date/duration/count appears in prose - all come from here.
SURVEY_YEAR         <- 2026L
SURVEY_OPEN         <- ""                          # TBC - leave blank until the official dates are set
SURVEY_CLOSE        <- ""                          # TBC
MINUTES_PER_EXAMPLE <- 10L
SURVEY_LANGUAGES    <- "English, French, Spanish, Russian or Arabic"
# Social proof (section 5, item 10): from the 2025 acknowledgements (94 people, 81 orgs).
# The 413 figure on the data-use page is CUMULATIVE 2021-2025 and is NOT used in prose.
N_FOCAL_POINTS      <- 94L
N_ORGS              <- 81L

# --- links -----------------------------------------------------------------------
GAIN_SURVEY_LINK <- "https://egrisstats.org/implementation/gain-survey/"  # the GAIN page
GAIN_PAGE_LINK   <- GAIN_SURVEY_LINK
GAIN_PAGE_URL    <- GAIN_SURVEY_LINK
GAIN_INFO_LINK   <- GAIN_SURVEY_LINK
GAIN_EGRISS_LINK <- "https://egrisstats.org"       # EGRISS site (letterhead link)
DATA_USE_URL     <- NULL                            # data-use page not yet live: omit the link, keep the sentence
GAIN_DEADLINE    <- ""                             # kept for the legacy template scripts
GAIN_CYCLE       <- "2026"

# --- fixed content blocks (EDIT to match actual EGRISS/GAIN wording & policy) ------
GAIN_SPONSOR  <- paste("The GAIN Survey is run by the EGRISS Secretariat, hosted by UNHCR and reporting",
                       "to the UN Statistical Commission.")
GAIN_DATA_USE <- paste("Your reply is used only to build the GAIN evidence base and would be attributed to",
                       "your office; we will check the wording with you before anything is published.")
GAIN_OPTOUT   <- "If you would prefer not to hear from us about this, just reply and we will not write again."
GAIN_ABOUT    <- paste("The Global Annual Inclusion (GAIN) Survey, run by the EGRISS Secretariat, records",
                       "statistical activities that include refugees, internally displaced and stateless persons",
                       "in national statistics.")

# --- where the rest of the GAIN SharePoint library is ------------------------------
# GAIN_ROOT = the "Integration & GAIN Survey" folder. Worked out from this
# folder's position (".../EGRISS Database Integration/GAIN Web Scarping/R script"),
# so the same code runs on the Mac and on Windows with no personal paths in it.
# Override with GAIN_ROOT=... in .Renviron if the folder ever moves.
GAIN_ROOT <- Sys.getenv("GAIN_ROOT", normalizePath(file.path(getwd(), "..", "..", ".."),
                                                   winslash = "/", mustWork = FALSE))
if (!dir.exists(file.path(GAIN_ROOT, "EGRISS GAIN Survey 2026")))
  message("*** NOTE: GAIN_ROOT (", GAIN_ROOT, ") has no 'EGRISS GAIN Survey 2026' folder - ",
          "set GAIN_ROOT in .Renviron. ***")

# Contact workbook: looked for in the working directory first, then in the
# current cycle's Sample Files folder. Update the cycle folder for a new round.
CONTACT_XLSX <- {
  cands <- c(list.files(pattern = "GAIN Data Collection .*Sample File.*xlsx$", full.names = TRUE),
             file.path(GAIN_ROOT, "EGRISS GAIN Survey 2026", "06 Sample Files",
                       "GAIN Data Collection 2026 - Sample File.xlsx"))
  hit <- cands[file.exists(cands)]
  if (length(hit) == 0) NA_character_ else hit[1]   # scripts that need it check + stop
}

if (SENDER_NAME == "[YOUR NAME]" || GAIN_SURVEY_LINK == "[GAIN SURVEY LINK]")
  message("*** WARNING: SENDER_NAME and/or GAIN_SURVEY_LINK is still a placeholder ",
          "in GAIN_CONFIG.R - every draft will literally say \"", SENDER_NAME,
          "\" / \"", GAIN_SURVEY_LINK, "\". Edit GAIN_CONFIG.R before sending. ***")
