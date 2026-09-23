# ==============================================================================
# ONE-OFF: re-apply the strengthened value gate to the ALREADY-extracted
# datapoints (no LLM re-run). Drops rows whose value does not appear in the
# model's own evidence quote - the echoes/fabrications the old excerpt-only gate
# let through. Backs up the raw file first. Idempotent.
# ==============================================================================
suppressMessages(library(tidyverse))
RAW <- "sdg_docs/SDG_DATAPOINTS.csv"
BAK <- "sdg_docs/SDG_DATAPOINTS_raw_backup.csv"

.normv <- function(x) gsub(",", ".", tolower(gsub("[[:space:]]", "", coalesce(as.character(x), ""))))

dp <- read_csv(RAW, show_col_types = FALSE)
if (!file.exists(BAK)) { write_csv(dp, BAK); message("backed up raw -> ", BAK) }

dp2 <- dp %>%
  mutate(.in_ev = map2_lgl(value, evidence,
                           ~ nzchar(.normv(.x)) && str_detect(.normv(.y), fixed(.normv(.x)))))
kept <- dp2 %>% filter(.in_ev) %>% mutate(value_in_evidence = TRUE) %>% select(-.in_ev)
dropped <- dp2 %>% filter(!.in_ev)

write_csv(kept, RAW)
message("value re-gate: ", nrow(dp), " -> ", nrow(kept),
        " kept  (", nrow(dropped), " dropped: value not in evidence quote)")
message("\ndropped by indicator:")
print(as.data.frame(count(dropped, sdg_code, sort = TRUE)), row.names = FALSE)
message("\nkept by match_type:")
print(as.data.frame(count(kept, match_type, sort = TRUE)), row.names = FALSE)
message("\nkept: rows still carrying the old example value 9.2 (human-review, may be genuine):")
message("  ", sum(.normv(kept$value) == "9.2"))
