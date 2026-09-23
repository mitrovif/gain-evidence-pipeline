# Expanding the GAIN search to 172 NSO websites
## Setting up 3 additional Google engines + 3 additional API keys

You currently have **one engine** (`GOOGLE_CSE_CX = <CSE_ID>`) covering the
first 50 NSO domains — Google's maximum per engine. The remaining 122 domains are
covered by creating **three more engines** (one per group of <=50 sites) and
**three more API keys** (each in its own Google Cloud project, so each brings its
own free 100 queries/day).

Everything else is already wired up:

- `NSO_Full_Registry.csv` (in this folder) holds all 172 domains in groups 1-4.
  Layers 2, 3 and the ENRICH script pick it up **automatically** — no script edits.
- Layer 2 reads `GOOGLE_CSE_CX2/3/4` for the new engines and rotates across
  `GOOGLE_CSE_KEY` + `SEARCH_API_KEY_1/2/3/4` when a key hits its quota.
- Progress is saved per query, so you can stop/resume any day.

**Two different things, don't mix them up:**

| Thing | What it is | Limit | Env variable |
|---|---|---|---|
| Engine (cx ID) | WHICH sites can be searched | max 50 sites per engine | GOOGLE_CSE_CX, _CX2, _CX3, _CX4 |
| API key | HOW MANY queries per day | 100/day free **per Google Cloud project** | GOOGLE_CSE_KEY, SEARCH_API_KEY_1..3 |

Any key works with any engine. Keys made in the **same** project share one quota —
that is why each new key needs its **own project**.

---

## STEP 1 - Create 3 new search engines (~5 min each)

For each of groups 2, 3, 4:

1. Open https://programmablesearchengine.google.com -> **Add**
2. Name it e.g. `GAIN NSO Group 2`
3. Under **"Search specific sites or pages"**, paste the site list for that group
   (below). Do NOT enable "Search the entire web".
4. Click **Create**, then copy the **Search engine ID** (the cx string).

**Site pattern format (Google's rules).** The lists below already follow them:
- `*.example.com` = entire domain incl. subdomains - used for NSOs that own their
  domain (e.g. `*.knbs.or.ke`, `*.stats.gov.sa`).
- `host.parent.tld/*` = entire site - used for NSOs hosted as a subdomain of a
  shared government portal, where `*.` would wrongly target the parent domain
  (e.g. `statsmauritius.govmu.org/*`, `bfs.admin.ch/*`, `statbel.fgov.be/*`).
- Patterns that are bare public suffixes (`*.com`, `*.gov.bh`) are not allowed -
  none of the lists contain any.
- Max **50 distinct domains per engine**: each list below is exactly one group of
  <=50 domains. Never paste two groups into the same engine.

**DONE (11 Jun 2026)** - the three engines exist and their IDs are already in
`~/.Renviron`:
- Group 1 (existing): `GOOGLE_CSE_CX  = <CSE_ID>`
- Group 2: `GOOGLE_CSE_CX2 = <CSE_ID>`
- Group 3: `GOOGLE_CSE_CX3 = <CSE_ID>`
- Group 4: `GOOGLE_CSE_CX4 = <CSE_ID>`

### Group 2 site list (paste as-is — Africa, MENA, Central Asia, Balkans)

```
*.ons.dz
*.bsc.ly
*.ansade.mr
*.gbosdata.org
*.stat-guinee.org
*.stat-guinebissau.com
*.statistics.sl
*.lisgis.gov.lr
*.inseed.tg
*.instad.bj
*.ine.cv
*.icasees.org
*.ins-congo.cg
*.inege.gq
*.ine.gov.ao
*.nsa.org.na
*.statsbots.org.bw
*.bos.gov.ls
*.cso.gov.sz
*.nsomalawi.mw
*.instat.mg
statsmauritius.govmu.org/*
*.nbs.gov.sc
*.inseed.km
*.instad.dj
*.ine.st
*.stats.gov.sa
*.fcsc.gov.ae
*.psa.gov.qa
*.csb.gov.kw
*.data.gov.bh
*.ncsi.gov.om
*.cso-yemen.org
*.amar.org.ir
*.cbs.gov.il
*.stat.tj
*.stat.gov.tm
*.stat.uz
*.nso.mn
*.belstat.gov.by
*.stat.gov.rs
*.bhas.gov.ba
*.stat.gov.mk
*.instat.gov.al
*.monstat.org
*.ask.rks-gov.net
*.dzs.hr
*.stat.si
*.nsi.bg
*.insse.ro
```

### Group 3 site list (paste as-is — Europe + Americas)

```
*.ksh.hu
*.stat.gov.pl
*.czso.cz
*.statistics.sk
*.statistics.gr
*.cystat.gov.cy
*.nso.gov.mt
*.stat.gov.lv
*.stat.gov.lt
*.stat.ee
*.stat.fi
*.scb.se
*.ssb.no
*.dst.dk
*.statice.is
*.cso.ie
*.ons.gov.uk
*.destatis.de
*.insee.fr
*.cbs.nl
statbel.fgov.be/*
statistiques.public.lu/*
bfs.admin.ch/*
*.statistik.at
*.istat.it
*.ine.es
*.ine.pt
*.ibge.gov.br
*.indec.gob.ar
*.ine.gob.cl
*.ine.gob.bo
*.ine.gov.py
*.ine.gub.uy
*.ine.gob.ve
*.statisticsguyana.gov.gy
*.statistics-suriname.org
*.inec.gob.pa
*.inec.cr
*.inide.gob.ni
*.ine.hn
onec.bcr.gob.sv/*
*.ine.gob.gt
*.sib.org.bz
*.one.gob.do
*.ihsi.ht
*.statinja.gov.jm
*.cso.gov.tt
*.onei.gob.cu
*.statcan.gc.ca
*.census.gov
```

### Group 4 site list (paste as-is — Asia-Pacific)

```
*.mospi.gov.in
*.stats.gov.cn
*.csostat.gov.mm
*.nso.go.th
*.gso.gov.vn
*.nis.gov.kh
*.lsb.gov.la
*.dosm.gov.my
*.singstat.gov.sg
*.statistics.gov.tl
*.nso.gov.pg
*.statsfiji.gov.fj
*.statistics.gov.sb
*.vnso.gov.vu
*.sbs.gov.ws
*.tongastats.gov.to
*.statisticsmaldives.gov.mv
*.nsb.gov.bt
*.kostat.go.kr
*.stat.go.jp
*.abs.gov.au
*.stats.govt.nz
```

---

## STEP 2 - Create 3 additional API keys (~5 min each)

Repeat this **three times**, once per NEW project:

1. Open https://console.cloud.google.com -> project dropdown (top bar) ->
   **New project** -> name it e.g. `gain-search-2` -> Create -> switch to it.
2. **APIs & Services -> Library** -> search **"Custom Search API"** -> **Enable**.
   (This is the step that was blocked before — do it per project, wait ~3 min.)
3. **APIs & Services -> Credentials -> Create credentials -> API key** -> copy it.

This gives you keys for `SEARCH_API_KEY_1`, `SEARCH_API_KEY_2`, `SEARCH_API_KEY_3`.
Your existing key stays in `GOOGLE_CSE_KEY`. Total free quota: **400 queries/day**.

If your work Google account blocks project creation, use a personal account —
keys and engines do not need to be in the same account.

---

## STEP 3 - Store the keys (one time, no keys in any script)

The file `~/.Renviron` already exists with the four engine IDs
filled in and **placeholders for the four API keys**. Open it in R:

    file.edit("~/.Renviron")

Replace the four `PASTE_...` placeholders with your real keys, save, then
RESTART R. Until you replace them, Layer 2 simply ignores the placeholder slots
(it skips any value starting with "PASTE"), so nothing breaks - it just has no
quota to use.

The progress log records only slot NAMES (e.g. `SEARCH_API_KEY_2`), never key values.

---

## STEP 4 - Verify, then run

Quick verification that the registry and engines are wired (expect 172):

    setwd("<GAIN_ROOT>/EGRISS Database Integration/GAIN Web Scarping/R script")
    nrow(readr::read_csv("NSO_Full_Registry.csv", show_col_types = FALSE))

Then simply:

    source("GAIN_LAYER2_SEARCH_API_5.R")     # search: auto-rotates keys, resumable
    source("GAIN_LAYER3_SITEMAPS_CC.R")      # sitemaps/Common Crawl for all 172 domains
    source("GAIN_MERGE_MASTER_REFERENCE.R")  # rebuild master reference
    source("GAIN_ENRICH_EVIDENCE.R")         # enrich + contacts + report
    source("GAIN_PHASE5_CROSSREF.R")         # cross-reference with GAIN

What to expect for Layer 2:
- The keyword dictionary now covers **40 languages** - every local language used
  on the group 2-4 websites (German, Dutch, Italian, Polish, Hungarian, Czech,
  Slovak, Greek, Romanian, Bulgarian, Serbian/Croatian/Bosnian, Albanian,
  Macedonian, Slovenian, Ukrainian, Persian, Hebrew, Nordic + Baltic languages,
  Chinese, Japanese, Korean, Thai, Vietnamese, Indonesian, Malay, Mongolian),
  each searched alongside English where the NSO publishes in both.
- Total query budget: **895 queries** for all 172 domains; group 1's original
  English queries are already done, so about **641 remain** -> roughly
  **2 days** at 400 free queries/day (resumes automatically across days).
- When all keys hit quota it stops gracefully, prints exactly which countries
  remain, and resumes from there next run.
- Layer 3 needs no keys but will take ~2-3 hours for 172 domains (run overnight).

---

## Notes and cautions

- **Verify flagged domains first.** In `NSO_Full_Registry.csv`, rows with a `note`
  saying "verify" are domains I could not state with full confidence (small or
  recently reorganised NSOs: Libya, Liberia, CAR, Congo, Eswatini, Comoros,
  Djibouti, Qatar, Bahrain, Yemen, Tajikistan, Slovakia, El Salvador, Venezuela,
  Haiti, Myanmar, Timor-Leste, Guinea-Bissau). Open each in a browser before
  running; fix the domain in the CSV if needed (it is just a text file). A wrong
  domain wastes ~4 queries and crawl time but breaks nothing.
- The CSV is the single source of truth now: edit it to add/remove/regroup
  domains. Keep each cse_group at <=50 domains and make sure the group's engine
  contains the same sites.
- If you later add a 5th group you would need one more engine variable in
  `GAIN_LAYER2_SEARCH_API_5.R` (`GOOGLE_CSE_CX5`) - currently 4 groups = 200
  domains max, which covers the 172 here with room to spare.
- Country names in the registry feed the GAIN cross-reference; if a new name
  doesn't match the GAIN roster, add it to `harmonize_country()` in
  `GAIN_PHASE5_CROSSREF.R` (the script warns you about unmatched names).
