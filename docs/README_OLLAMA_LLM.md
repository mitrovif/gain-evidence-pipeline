# How the local LLM (Ollama) is used in the GAIN pipeline

This explains the **optional** local-AI layer: what it is, what it does, where it plugs
in, and the guardrails. It is **off by default** and never changes the deterministic
(rule-based) results — it only *adds* a second opinion and the example-level match.

---

## 1. What is running, and where

Everything runs **locally on your machine** via **Ollama** at `http://localhost:11434`.
**No data leaves the computer** for the LLM (the only external calls in the whole pipeline
are the search APIs and the public stats websites). Two local models:

| Model | Role |
|---|---|
| `qwen2.5:7b` | Reads text and returns structured JSON: classifies evidence and decides whether two records are the same GAIN example. |
| `bge-m3:latest` | Turns text into a 1024-number "embedding" (a meaning fingerprint) so we can measure how *similar* two texts are, in any language. |

It is **opt-in**: nothing calls the models unless you set `DO_LLM <- TRUE` in `RUN_ALL.R`
(and Ollama is running). If Ollama is down, every LLM step skips cleanly and the
rule-based pipeline runs exactly as before.

All of this lives in one helper file: **`GAIN_OLLAMA_HELPERS.R`**.

---

## 2. The three things the LLM does

### a) `extract_evidence(text)` — read a document, return structured facts
Feeds the document text to `qwen2.5` and gets back a strict JSON record:
`relevance_score` (0–100), `counted` (was a displaced/stateless group actually
included?), `implementation_status` (implemented / planned / unclear), `population`,
`organization`, `lead_type` (country-led / partner-led / unclear),
`is_humanitarian_data` (flagged), `instrument_or_title`, `year`,
`recommendation_referenced` (EGRISS / IRRS / IRIS / IROSS), a `evidence_quote`
(≤20 words, verbatim), and `confidence`.

This is what catches inclusion that the keyword rules miss — e.g. a census that adds
refugees to its sampling frame using wording our dictionary doesn't list, or a
non-English page. It is the **slow** call (~100 seconds per document on CPU).

### b) `embed(text)` — measure meaning similarity
Uses `bge-m3` to convert text to a vector. Two texts about "refugees counted in the
census" score ~0.74 similarity; "refugees" vs "coastal hotel occupancy" scores ~0.36.
Fast (~0.1 s). Multilingual, so a Spanish page and an English GAIN example still compare.

### c) `adjudicate_match(artifact, example)` — are these the SAME example?
Given a discovered artifact and a known GAIN example (country, organization, instrument,
year, population), `qwen2.5` decides `same_example: true/false` with a reason. It
correctly says a *census* and a *labour-force survey by the same office are NOT the same
example* — the discrimination the old country-level matcher could not make.

---

## 3. How they plug into the pipeline (the "funnel")

The LLM is expensive, so it is used as a **funnel**, never on everything:

```
ALL candidates
   │  (1) KEYWORD pass  — already done by the enrich step (free)
   ▼
~survivors
   │  (2) EMBEDDING     — embed each survivor, cosine-similarity to the 413 confirmed
   │                      GAIN examples (the "seed positives"). Fast.
   ▼
gated set (~110)        — keep only: not suppressed + has a displacement signal +
   │                      similarity ≥ 0.60, capped per domain, capped to ~110 total
   │  (3) LLM EXTRACT   — extract_evidence() ONLY on this gated set (~3 h first pass)
   ▼
enriched + llm_* columns  ──►  written to GAIN_EVIDENCE_ENRICHED_*_SEM.csv
```
This runs in **`GAIN_SEMANTIC_FUNNEL.R`**.

Then the **example-level GAIN match** (inside `GAIN_PHASE5_CROSSREF.R`, "Module A2"):
for each artifact it finds the nearest GAIN example **in the same country** (embeddings),
and runs `adjudicate_match` only on the near matches, to assign:
`already_in_gain` (with `matched_gain_id`), `new_example_existing_country`, or
`new_country`. Anything uncertain or low-confidence → **`review_queue.csv`** (never
trusted at scale).

---

## 4. The classification rules (the prompt, version v2)

`extract_evidence` is told to be **moderately conservative** with these score bands:
- **80–100** group concretely included & implemented (enumerated, disaggregated, in a
  frame/register, dedicated module)
- **60–79** included but limited
- **40–59** inclusion **planned**, or described at a general level (still relevant)
- **20–39** topic present, inclusion unclear
- **0–19** clearly unrelated (0 reserved for no plausible connection)

Other rules baked in: **inclusion is broad** (counting, disaggregating, sampling-frame
addition, registers, admin data, modules; refugees / asylum-seekers / IDPs / returnees /
stateless / host-community all count); **planned and implemented both count**;
**humanitarian operational data is flagged** (`is_humanitarian_data`); the LLM also gives
its own **lead assessment** so we can compare it to the rule-based one; and EGRISS
frameworks are **recommendations, never "standards"**.

To change these rules, edit the prompt in `GAIN_OLLAMA_HELPERS.R` and bump
`PROMPT_VERSION` (e.g. "v2" → "v3"). Bumping the version means only changed records are
re-read; everything else stays cached.

---

## 5. Guardrails — why this is safe to trust

- **Annotate-only:** the LLM **never overwrites** a rule-based column. It only adds the
  `llm_*` / `gain_match_*` / `sem_*` columns. Your deterministic score and category are
  untouched, so the pipeline stays auditable.
- **Schema-validated:** every JSON response is parsed and checked; malformed responses are
  discarded (the record just isn't annotated), never silently mis-stored.
- **Temperature 0:** deterministic outputs — same input gives the same answer.
- **Cached by content hash:** every extract, embedding, and adjudication is saved to
  `ollama_cache/`. A re-run **never re-calls the model** for something already done →
  idempotent and resumable (an interrupted run just continues).
- **Logged:** every decision is appended to `ollama_decisions_log.csv` (title, country,
  score, counted, lead, humanitarian flag, confidence, cache hit/miss) — a full audit trail.
- **Review queue:** low-confidence and "new example" matches are routed to
  `review_queue.csv` for a human, not trusted automatically.

---

## 6. How to run it

1. Start Ollama and make sure both models are present:
   `ollama run qwen2.5:7b`  and  `ollama pull bge-m3`
   (verify in R: `source("GAIN_OLLAMA_HELPERS.R"); ollama_available()` → `TRUE`)
2. In `RUN_ALL.R` set **`DO_LLM <- TRUE`**, then `source("RUN_ALL.R")`.
3. First pass is slow: ~3 h for the funnel extracts + ~1.5 h for the example-match
   adjudications. **You will know it ran** because of that runtime and because the
   `llm_*` / `gain_match_*` columns fill in. If it finishes in 20 minutes, `DO_LLM` is
   off or Ollama isn't reachable.

Tunable knobs (set in `RUN_ALL.R` or as environment variables):
`GAIN_MAX_EXTRACTS` (110), `GAIN_GATE_SIM` (0.60), `GAIN_MAX_ADJUDICATIONS` (450),
`GAIN_EX_BAND_LOW` (0.62), `GAIN_EX_HIGH_SIM` (0.85). Because everything is cached,
raising a cap later only costs the time of the *new* items.

---

## 7. One-line summary
The local LLM is an **optional, cached, logged second-opinion layer**: embeddings find
the records most worth reading, `qwen2.5` reads them and judges whether a displaced group
is actually included and whether the artifact is already a known GAIN example — and it
*annotates* the deterministic results rather than replacing them, with everything
uncertain sent to a human review queue.
