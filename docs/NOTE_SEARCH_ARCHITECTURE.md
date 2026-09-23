# Note — can structured-statistics tools (like the UNICEF MCP) improve HOW we search?

Author: pipeline build session · Date: 22 Jun 2026
Question addressed: not "use UNICEF data to enrich results", but "does the
MCP/SDMX pattern point to a better way to DISCOVER and HARVEST official
statistics, and are there tools that fit our structure and improve the SCRAPE
itself — not just the post-scrape refinement?"

---

## 1. The reframe — it's the interface, not the data
`unicefstats-mcp` matters less as a dataset and more as a **signpost**: official
statistics have a machine-readable distribution layer (SDMX and peers). Our
pipeline currently leans on the part that ignores that layer — **Layer 3, the
HTML crawl** — and that is exactly where our pain has been: the Spain/Brazil
floods, encoding mojibake, `&idp=` false positives, per-domain caps. Those are
inherent to scraping rendered web pages.

A structured statistical API returns instead: clean **title, producer, country,
year, dimensions (incl. population breakdowns)**, language-agnostic **codes**,
and the **producer identity** — the very thing our `evidence_nature` logic tries
to reverse-engineer after scraping. So the strategic move is to **shift discovery
weight from HTML scraping toward structured APIs where they exist**.

## 2. The honest paradox (the crux — do not skip)
Structured APIs are **strongest for high-capacity NSOs** (EU, OECD, Nordics) —
which are mostly your **low** displacement priority. The **high-displacement
NSOs** (Sudan, Chad, DR Congo, South Sudan, Afghanistan) frequently expose **no
API** → those you must still scrape. **APIs help most where we need it least.**
Conclusion: not "replace scraping", but **route each country to its best
available interface**.

## 3. The ecosystem that fits our structure (and the R access for each)
All free, mostly no-key, and all return structured metadata we can map straight
into our existing `master` schema (country/title/url/producer/year/populations).

| Tool / standard | Who exposes it | Why it helps DISCOVERY | R access |
|---|---|---|---|
| **SDMX REST** | UNICEF, Eurostat, OECD, ILOSTAT, IMF, World Bank, ECB, FAO, UNSD-SDG, UNESCO-UIS, and a growing set of NSOs running **.Stat Suite** | One query language; producer + dimensions in the metadata; multilingual codes | `rsdmx`, `readsdmx`, or raw `httr2` (what we already did for UNICEF) |
| **PxWeb API** | Statistics Sweden/Finland/Norway/Denmark, Ireland CSO (PxStat), and ~20 more NSOs | Queryable table API; clean titles + dimensions | `pxweb` |
| **NADA / IHSN catalog API** | IHSN, UNHCR Microdata, World Bank Microdata, many national microdata catalogues | Survey-level metadata incl. producer + populations | we already use this (Layer 1) |
| **CKAN / DKAN open-data APIs** | Many NSO/government open-data portals (`/api/3/action/package_search`) | Full-text + faceted dataset search, structured results | raw `httr2` |
| **Org statistics APIs** | World Bank (`wbstats`), Eurostat (`eurostat`), ILOSTAT (`Rilostat`), data.un.org | Indicator + country + year, official | dedicated R packages |
| **Displacement-specific** | **UNHCR Refugee Statistics API**, **IOM DTM API**, **IDMC GIDD**, **JIPS**, HDX (`data.humdata.org`, HXL-tagged) | The actual displacement series + who collected them | raw `httr2` |
| **Discovery meta-search** | Google Dataset Search (schema.org/Dataset), national data-portal sitemaps | Finds datasets across portals without per-site crawling | `httr2` + JSON-LD parse |

**On MCP specifically:** MCP is a wrapper that lets an *LLM agent* call these
tools interactively. For our deterministic R pipeline we don't need the MCP
layer — we call the **underlying APIs directly** (as we did for UNICEF SDMX).
MCP only becomes relevant if we later want an LLM agent to drive discovery
conversationally. So: adopt the **APIs/standards**, not the MCP wrappers.

## 4. How this maps onto our layers (proposed change)
Add a new **"Layer 1.5 — Structured Statistical API discovery"** that runs BEFORE
the web crawl and feeds the same `master` schema:

```
per country ->  has an SDMX/PxWeb/.Stat/CKAN endpoint?
                 │
        YES ─────┤  query it for displacement-relevant series  ─►  clean records
                 │  (precise, producer known, multilingual, low noise)
        NO  ─────┘  fall back to Layer 2/3 web scrape (today's path)
```

Benefits that directly answer "do a better job at scraping":
- **Higher precision at the source** — no Spain-style floods; the producer field
  removes most `evidence_nature` guesswork.
- **Multilingual for free** — codes, not page text, so no per-language keyword upkeep.
- **Less crawl load** — query one endpoint instead of crawling thousands of URLs.
- **Better Layer-3 targeting** — even where we still scrape, the API tells us which
  instruments exist, so the crawl looks for *those* rather than guessing.

## 5. What the APIs still cannot do (why it stays hybrid)
The GAIN-defining signal — *does this census/survey actually INCLUDE or
DISAGGREGATE by displacement status?* — is usually **not** in the structured
metadata. A PxWeb table titled "Population by region" won't say in its metadata
that it oversampled IDPs. So:
- **APIs** = breadth + precision of *discovery* + *producer identity*.
- **Scrape + LLM** = the *inclusion / disaggregation* read (our funnel +
  `extract_evidence`), which no warehouse exposes.
This is the same complementarity we already use; the APIs just make the
discovery half cleaner and the producer call automatic.

## 6. Recommendation — a concrete, low-risk first step
Prototype a **generic structured-discovery harvester** on the two interfaces with
the widest NSO coverage and the cleanest R support:
1. **SDMX** (`.Stat` NSOs + the big orgs) and **PxWeb** (Nordics/Ireland/others) —
   query each registry-listed NSO that has an endpoint for displacement-relevant
   dataflows/tables, emit `master`-schema records with producer filled in.
2. Add the registry a `discovery_interface` column (`sdmx` / `pxweb` / `ckan` /
   `scrape`) so each country is routed to its best source; default `scrape`.
3. Measure: for the ~30–40 countries with an API, compare API-discovered records
   vs the current scrape — precision and producer-accuracy should jump, noise drop.

Effort: ~1–2 days for the SDMX + PxWeb prototype across the high-capacity NSOs.
Risk: low and additive — it feeds the same `master`; nothing downstream changes.
The high-displacement, no-API NSOs keep their current scrape path unchanged.

## 6b. Prototype results (built & run, 22 Jun 2026) — `GAIN_LAYER1_STRUCTURED.R`
A working Eurostat structured-discovery harvester is in place (no key, cached in
`struct_cache/`). One endpoint, five GAIN-relevant dataflows (asylum applicants,
asylum decisions, unaccompanied minors, resettled persons, population incl.
stateless):

- **175 clean records across 44 European countries** — a consistent 5 per country
  (asylum × 3, resettlement, stateless), every one with `producer = national
  statistical authority (ESTAT)` and a direct data-browser URL. Zero HTML noise.
- **It recovers countries the scrape missed entirely.** In the current enriched
  file Spain = **0** scrape records and Germany = **0** (lost to the false-positive
  suppression) — structured discovery gives each **5** genuine official series.
- **It replaces noise with signal where the scrape floods.** Netherlands = **139**
  scrape records → **5** clean structured ones for the same displacement topics.
- **Routing written:** `NSO_Full_Registry_routed.csv` adds a `discovery_interface`
  column — **41** registry countries → `sdmx (Eurostat)`, the rest → `scrape`
  (Nordics flagged `pxweb (next)`).

Output: `structured_discovery_20260622.csv` (master schema, ready to merge) +
`NSO_Full_Registry_routed.csv`. **Not yet wired into the merge** — it's a
standalone proof; folding it in is the next explicit step.

Honest caveat confirmed: these are EU/EEA countries — mostly low displacement
priority. The high-displacement NSOs still have no API and keep the scrape path.
The win here is **precision + recovery for the European bloc at near-zero cost**,
and a reusable pattern for the next endpoints (OECD/ILOSTAT SDMX, PxWeb Nordics).

## 7. One-line answer
Yes — the MCP/SDMX pattern points to a structurally better **discovery** layer
(structured statistical APIs), which would cut Layer-3 noise and hand us the
producer identity for free. But because the highest-displacement NSOs are the
least likely to expose an API, the right design is **route-by-interface
(API where it exists, scrape where it doesn't) + keep the LLM for the inclusion
signal no API carries** — not a wholesale replacement of scraping.
