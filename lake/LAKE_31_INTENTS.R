# ==============================================================================
# GAIN DATA LAKE - pre-write licensed-dataset APPLICATIONS (tier 3).
#
# For each licensed socio-economic dataset (FDP + any licensed), produce a ready-
# to-paste intended-use application using the settled EGRISS/GAIN wording, tailored
# to the survey. The user just logs in and submits. Output: lake_dataset_intents.csv
# and a readable lake_dataset_intents.md.
# ==============================================================================
suppressMessages({ library(tidyverse) })
LAKE <- "data_lake"
r <- suppressMessages(read_csv(file.path(LAKE,"lake_dataset_resolved.csv"), show_col_types=FALSE))
lic <- r %>% filter(str_detect(access_status,"^3"))

ORG <- "EGRISS Secretariat, hosted by UNHCR"
intent <- function(survey, country){ paste0(
"Requesting organisation: ", ORG, "\n",
"Intended use of the data:\n",
"The Expert Group on Refugee, IDP and Statelessness Statistics (EGRISS) is compiling a ",
"cross-country evidence base to support statistical inclusion of forcibly displaced and ",
"stateless populations, in line with the International Recommendations on Refugee (IRRS), ",
"IDP (IRIS) and Statelessness (IROSS) Statistics. We will use the ", survey, " (", country, ") ",
"to produce aggregate, non-disclosive statistics - poverty, employment, education, health and ",
"living-conditions indicators disaggregated by displacement status - feeding SDG indicator ",
"estimation and progress measurement under the GAIN initiative.\n",
"Outputs: aggregate indicator tables and methodological notes only. No attempt will be made to ",
"re-identify individuals or households; microdata will not be redistributed and will be stored ",
"securely and used solely for the stated statistical purpose.\n",
"Expected completion: 31 December 2026.") }

lic <- lic %>% mutate(application_text = map2_chr(survey, gain_country, intent))
readr::write_excel_csv(lic %>% select(gain_country, type, survey, year, portal, ihsn_record, application_text),
                       file.path(LAKE,"lake_dataset_intents.csv"))
md <- lic %>% mutate(block = sprintf("## %s - %s (%s)\nPortal: %s\nIHSN: %s\n\n%s\n\n---\n",
                     gain_country, survey, year, portal, ihsn_record, application_text)) %>% pull(block)
writeLines(c("# Licensed socio-economic datasets - ready-to-paste applications", "",
             sprintf("%d datasets. Log in to the portal, start the data request, paste the text.", nrow(lic)), "",
             md), file.path(LAKE,"lake_dataset_intents.md"))
message(sprintf("wrote %d licensed-dataset applications -> lake_dataset_intents.csv / .md", nrow(lic)))
print(lic %>% count(portal) %>% as.data.frame(), right=FALSE)
