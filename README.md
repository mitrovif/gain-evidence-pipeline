# GAIN evidence pipeline

R code used by the EGRISS Secretariat to find **possible new examples** for the
Global Annual Inclusion (GAIN) Survey: statistical activities that include
refugees, internally displaced and stateless persons in official statistics.
It also prepares the follow-up with the institutions that produced them.

Everything the pipeline finds is a **possible candidate that needs manual review
and confirmation by the producing institution**. It is never a confirmed GAIN
example. The EGRISS frameworks (IRRS, IRIS, IROSS) are *recommendations*, not
standards.

This repository holds **code and documentation only**. Contact lists, survey
responses, outputs and API keys are kept out of it (see `.gitignore`).

## Workstreams

| Folder | Run file | What it does |
|---|---|---|
| `identify/` | `RUN_IDENTIFY.R` | Search NSO websites, data catalogues, search engines and Eurostat; score candidates with a local LLM; match them against existing GAIN examples; export the Power BI tables and the prioritised list |
| `outreach/` | `RUN_REACHOUT.R` | Take the prioritised list, connect it to contacts, write email **drafts** (never sends), track replies |
| `sdg/` | `RUN_SDG.R` | Retrieve documents and produce SDG indicator values disaggregated by displacement status |
| `lake/` | (step scripts `LAKE_01`…`LAKE_40`) | Recover the data behind examples already in GAIN |
| `shared/` | | Config (`GAIN_CONFIG.R`), common helpers, local-LLM helpers, prompts |

The only link between identification and outreach is the hand-off file written by
`identify/GAIN_FINALIZE.R`.

### Matching against GAIN (`identify/GAIN_MATCH_V2.R`)
Each candidate is compared with **every** GAIN example in the same country on
four separate checks: producer, product type (census / survey / admin register /
data integration / publication), product name (acronyms and distinctive words,
across languages) and year. It is then placed in one category: *same product*,
*new edition*, *check*, *same organisation, other product*, *other producer*,
*new country* or *multi-country*. `identify/GAIN_CALIBRATE.R` builds a small
review sheet, then measures how often the pipeline agrees with a human reviewer.

## Running it

The scripts run **from this folder**, which is where the data lives: open
`GAIN_WebScraping.Rproj`, or `cd` here. The code sits in the subfolders above.

```r
source("RUN_IDENTIFY.R")    # set DO_SCRAPE / DO_LLM at the top first
source("RUN_REACHOUT.R")
```

On macOS, run scripts from the terminal with `LC_ALL=en_US.UTF-8 Rscript …`.
Without it, the multilingual text handling fails under the default C locale.

### Requirements
- R ≥ 4.3 with `tidyverse`, `httr2`, `jsonlite`, `rlang`, `pdftools`, `openxlsx`, `readxl`, `survey`
- A local LLM server for the AI steps: **LM Studio** (default, `http://localhost:1234`)
  with `qwen2.5-7b-instruct` and `text-embedding-bge-m3` loaded, or Ollama.
  The backend is detected automatically.
- API keys for the search layer in a local `.Renviron` (copy `.Renviron.example`)
- Outlook-specific steps (sync, creating drafts) run on Windows only; they are
  skipped automatically elsewhere.

More detail: [`WORKFLOW.md`](WORKFLOW.md) and [`docs/`](docs/).
