# ==============================================================================
# GAIN DATA LAKE - build the REACH-OUT list: microdata-capable GAIN examples whose
# EXACT data we could NOT obtain online (forthcoming, office-held, not yet released,
# or no portal copy). These get a data-request + GAIN-round-announcement email to
# the reporting office. Output: gain_reachout_list.csv
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
ro <- suppressMessages(read_csv(file.path(LAKE,"lake_roster.csv"), show_col_types=FALSE))

# what we SECURED or REQUESTED (exact matches), so we do NOT reach out for these
secured   <- c("ex246","ex295","ex299")               # in hand
requested <- c("ex191","ex200","ex103","ex183")       # request/application submitted
done <- c(secured, requested)

# microdata-capable examples that still need the data from the office
reach <- ro %>% filter(is_microdata_capable, !example_id %in% done, nzchar(coalesce(country,""))) %>%
  mutate(reason = case_when(
    str_detect(tolower(paste(title,description)), "plan|forthcoming|will |2026|under revision|mapping|launched|design") ~ "forthcoming / not yet collected",
    str_detect(tolower(organisation), "who|world health") ~ "office-held (WHO)",
    str_detect(tolower(organisation), "jips|joint idp profiling") ~ "JIPS exercise - not on portal",
    str_detect(tolower(organisation), "instituto nacional|national statistic|bureau of statistic|national institute of statist") ~ "NSO-held - request from office/site",
    TRUE ~ "exact study not online - request from office")) %>%
  transmute(example_id, country, organisation=substr(organisation,1,40), title=substr(title,1,50), year, reason) %>%
  arrange(country)
readr::write_excel_csv(reach, file.path(LAKE,"gain_reachout_list.csv"))

message(sprintf("REACH-OUT list: %d microdata-capable examples need the data from the office", nrow(reach)))
message(sprintf("(secured %d + requested %d already handled)\n", length(secured), length(requested)))
message("by reason:"); print(count(reach, reason, sort=TRUE) %>% as.data.frame(), right=FALSE)
message(sprintf("\ndistinct organisations to contact: %d", n_distinct(reach$organisation)))
