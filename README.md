# HStat : Shiny Statistical Analysis Application

[![R-CMD-check](https://github.com/houphouet/Hstat/actions/workflows/R.yml/badge.svg)](https://github.com/houphouet/Hstat/actions/workflows/R.yml)

HStat is an interactive web application built with R Shiny that enables a
complete data analysis pipeline, from data import to advanced multivariate
analyses without writing a single line of code.

---

## Prerequisites

- **R** ≥ 4.4.0
- **RStudio** (recommended) or any other R environment
- Internet connection for automatic package installation (first run only)

---

## Installation

Install directly from GitHub using `remotes`:

```r
# install.packages("remotes")
remotes::install_github("houphouet/Hstat")
```

## Launch the application

```r
library(HStat)
run_hstat()
```

This opens the app in your default web browser. Required packages are
installed automatically if needed on first run.

---

## Large datasets (out-of-memory engine)

HStat handles datasets far beyond available RAM. Files under the
out-of-memory threshold (500 MB by default) are loaded in memory with
`data.table::fread`. Above it, CSV/Parquet/DuckDB files are **never loaded
into RAM**: DuckDB queries them on disk, exact statistics are computed by
SQL on the *full* dataset, and interactive analyses run on a reproducible
random sample (100 000 rows by default, adjustable up to 10 million in the
UI). Row counts above 2^31 (2.1 billion rows) are supported.

Environment variables (all optional):

| Variable | Default | Purpose |
|---|---|---|
| `HSTAT_MAX_UPLOAD_MB` | `102400` (100 GB) | Max upload size |
| `HSTAT_BIGDATA_THRESHOLD_MB` | `500` | Out-of-memory switch threshold |
| `HSTAT_SAMPLE_SIZE` | `100000` | Working sample size |
| `HSTAT_DUCKDB_MEMORY` | *(unset)* | DuckDB RAM cap, e.g. `8GB` (spills to disk beyond) |
| `HSTAT_PLOT_MAX_POINTS` | `100000` | Max points drawn on scatterplots |
| `HSTAT_DIST_MAX_N` | `5000` | Cap for O(n²) distance-matrix analyses |
| `HSTAT_KENDALL_MAX_N` | `20000` | Cap for Kendall correlation |
| `HSTAT_IMPUTE_MAX_N` | `100000` | Cap for kNN/missForest imputation |
| `HSTAT_ML_MAX_N` | `200000` | Training-set cap for ML/DL models |

---

## Qualitative coding workbench (CAQDAS)

Under **Analyses qualitatives → Codage / thématisation**, HStat provides a
MAXQDA-style coding workbench:

- **Coding** — read one open-ended answer at a time, select a word, sentence
  or paragraph with the mouse, then drag-and-drop it onto a code (clicking the
  code works too). The passage gets a coloured label; overlapping codes are
  rendered as a gradient of their colours.
- **Retrieval and cross-tabulation** — click a code to list every excerpt it
  covers, and cross the coded text with respondent profiles (for instance, only
  the price complaints made by the "Moins de 25 ans" group).
- **Visualisation and reports** — word clouds, a concept map of code
  co-occurrences (the MAXMaps equivalent, laid out by classical MDS), cross
  matrices, and an Excel workbook gathering codebook, excerpts and matrices.
  The whole coding project can be saved to `.rds` and reloaded later.
- **Coding assistant — free, local and offline.** Three engines, none of which
  requires an account or a paid subscription:

  | Engine | Needs | Network | Cost |
  |---|---|---|---|
  | **Local model** *(default)* | Ollama or any OpenAI-compatible inference server (llama.cpp, LM Studio, vLLM, Jan) running on your machine | none once the model is downloaded | free |
  | **Automatic thematisation** | nothing at all — it runs inside R | none, ever | free |
  | Claude API | an Anthropic API key | required | paid |

  The assistant proposes a codebook from your corpus, then pre-codes the
  answers. Suggestions are tagged and can be reviewed or dropped in one click.

  The **local model** engine talks to `http://127.0.0.1:11434` (Ollama) or
  `http://127.0.0.1:8080` (OpenAI-compatible) — install Ollama once, run
  `ollama pull qwen2.5`, and everything afterwards works with no Internet
  connection. Survey responses never leave the machine. Installed models are
  discovered automatically and listed in a dropdown.

  The **automatic thematisation** engine uses no language model at all: corpus
  terms are clustered by hierarchical clustering on their co-occurrence across
  answers (cosine distance, Ward's method), so words appearing in the same
  answers form a theme. Each theme comes with a **keyword dictionary** you can
  edit, which then pre-codes the *whole* corpus — accent- and case-insensitive,
  labelling the sentence carrying the keyword — instantly and deterministically.

  Whatever the engine, an excerpt proposed by a model is located in the real
  text before any label is placed: a quote the model invented is discarded and
  counted, never applied to the wrong passage.

---

## Result interpretation & decision support

A dedicated tab — **Interprétation & aide à la décision** — turns raw output
into something you can paste into a report, and tells you which analysis your
data actually call for. It never chooses or runs an analysis: the method stays
your decision, and your responsibility.

**Continuous integration.** `.github/workflows/tests.yml` runs on every push and
pull request: a dependency-free syntax pass over all R files plus a version
consistency check, then the full `testthat` suite. Packages are installed from
an explicit list rather than from `DESCRIPTION`'s 107 Imports — the suite only
sources four files, and installing everything would make CI take an hour
without testing anything more.

**Reproducibility journal.** Every analysis you run is recorded, and the
**Journal & reproductibilité** tab turns the session into an executable **R
script**: data loading, then each analysis in the order you ran it, with its
parameters. Steps whose settings were purely interactive are flagged
`NON RECONSTITUÉ` and documented in a comment rather than guessed — a script
that silently differed from what the app computed would be worse than no script
at all. The generated script is verified to parse and to run.

**Automatic report.** The **Rapport** tab assembles everything the session
produced into one document you can hand over as it stands: dataset summary,
data-quality findings, every analysis with its parameters and tables, the
figures, the written interpretation, the recommended analyses, and the R script
as an appendix. Pick the sections you want and one of three formats —
**HTML**, **Word (.docx)** or **PDF**. HTML is assembled in R itself and is
therefore always available, figures embedded as base64 so the file stays a
single, mailable document; Word and PDF go through pandoc and LaTeX, and when
those are missing the app **says so, points at the fix, and falls back to
HTML** rather than failing with a technical message. The report computes
nothing — it typesets what you already obtained.

**Data health check.** A dedicated tab reports what is wrong with the dataset
before you analyse it: missing-value rates, constant and quasi-constant
variables, numbers stored as text, rare or too-numerous categories, extreme
values, near-perfect redundancy between variables, duplicate rows, and too few
observations for the number of variables. Every finding carries a **severity**
— *bloquant* / *important* / *à surveiller* — and a **concrete suggestion**,
not just a percentage. Deterministic, offline, and sampled above 20 000 rows so
it stays instant on large files.

**Automatic capture — every module, without exception.** Exploration, cleaning, filtering,
descriptive statistics, visualisation, correlations, statistical tests, post-hoc
comparisons, the 14 multivariate analyses, qualitative analyses, time series,
machine learning, deep learning, power analysis and efficacy thresholds — all 15
families drop their results into a shared slot. A module only claims the context
when it has actually done something: filtering stays silent until a filter
really removes observations. No module knows about the
assistant, and the assistant knows about no module — a new analysis is picked
up for free as long as it feeds the same slots.

**Guidance where the analysis ends.** A recommendation you only get if you
think to change tabs helps nobody. As soon as an analysis produces a result, a
banner at the bottom of that tab announces what your data profile calls for,
with a one-click link to the full interpretation — and a notification says the
same thing. All twelve analysis tabs carry one.

**Interpretation.** Two paths, both available:

- *Automatic reading* — deterministic, offline, always available. It re-reads
  your tables, states every p-value and its significance at your chosen alpha,
  summarises the data profile, and lists the analyses your data call for.
  Nothing is generated: these numbers are read, not written.
- *Model-written interpretation* — the local model (or Claude) turns the same
  material into a **Lecture des résultats / Ce que cela signifie / Précautions
  et limites / Analyse recommandée** write-up, in scientific, plain-language or
  detailed register, downloadable as Markdown or plain text. The prompt forbids
  inventing any figure, re-running anything, or telling the user they were
  wrong. If the model is unreachable, it falls back to the automatic reading.

**Analysis recommendation — no language model involved.** Recommendations come
from classical statistical rules applied to a profile of your variables:
variable types, group sizes and balance, per-group normality (Shapiro-Wilk),
homogeneity of variance (Bartlett or Levene), pairing. A statistical test
should not be suggested by text generation, so it isn't. Every recommendation
comes with the reason behind it, and the profile tab shows the numbers the
recommendation rests on.

Normality is tested **within each group**, never on the pooled variable — two
perfectly normal but well-separated groups form a bimodal mixture that
Shapiro-Wilk rejects (p ≈ 1e-6 where each group gives p ≈ 0.8), which would
steer you away from ANOVA exactly when it fits.

The verdict tells you whether the analysis you ran is among those your data
call for — phrased as information, never as a reprimand, since a research
question or a field constraint can justify a choice the rules don't know
about. Descriptive analyses are treated as a preliminary step and get a "what
next" suggestion rather than a verdict.

---

## Project structure

```
.
├── app.R
├── DESCRIPTION
├── Hstat.Rproj
├── inst
│   ├── app
│   │   ├── app.R
│   │   ├── app_server.R
│   │   ├── mod_ai.R
│   │   ├── HStat.R
│   │   ├── mod_clean.R
│   │   ├── mod_coding.R
│   │   ├── mod_descriptive.R
│   │   ├── mod_design.R
│   │   ├── mod_dl.R
│   │   ├── mod_explore.R
│   │   ├── mod_filter.R
│   │   ├── mod_ml.R
│   │   ├── mod_qualitative.R
│   │   ├── mod_tests.R
│   │   ├── mod_threshold.R
│   │   ├── mod_timeseries.R
│   │   ├── mod_viz.R
│   │   ├── Utils.R
│   │   ├── UX.R
│   │   └── www
│   │       ├── fonts
│   │       │   ├── archivo-latin-400-normal.woff2
│   │       │   ├── archivo-latin-500-normal.woff2
│   │       │   ├── archivo-latin-600-normal.woff2
│   │       │   ├── archivo-latin-700-normal.woff2
│   │       │   ├── Archivo-LICENSE.txt
│   │       │   ├── ibm-plex-mono-latin-400-normal.woff2
│   │       │   ├── ibm-plex-mono-latin-500-normal.woff2
│   │       │   ├── ibm-plex-mono-latin-600-normal.woff2
│   │       │   ├── ibm-plex-sans-latin-400-normal.woff2
│   │       │   ├── ibm-plex-sans-latin-500-normal.woff2
│   │       │   ├── ibm-plex-sans-latin-600-normal.woff2
│   │       │   ├── ibm-plex-sans-latin-700-normal.woff2
│   │       │   ├── inter-latin-400-normal.woff2
│   │       │   ├── inter-latin-500-normal.woff2
│   │       │   ├── inter-latin-600-normal.woff2
│   │       │   ├── inter-latin-700-normal.woff2
│   │       │   ├── Inter-LICENSE.txt
│   │       │   ├── newsreader-latin-400-italic.woff2
│   │       │   ├── newsreader-latin-400-normal.woff2
│   │       │   ├── newsreader-latin-500-normal.woff2
│   │       │   ├── newsreader-latin-600-normal.woff2
│   │       │   └── Newsreader-LICENSE.txt
│   │       ├── hstat-theme.css
│   │       └── Sortable.min.js
│   └── CITATION
├── man
│   └── run_hstat.Rd
├── NAMESPACE
├── R
│   ├── run_hstat.R
│   └── zzz.R
├── README.md
└── tests
    ├── test-hstat.R
    ├── testthat
    │   └── test-hstat.R
    └── testthat.R
```
---

## How to cite / Comment citer

If HStat is useful for your work, please cite it. In R:

```r
citation("HStat")
```

Or use one of the following:

**Text**
> KOUADIO, Houphouet (2026). HStat: Application Shiny interactive pour l'analyse statistique. Version 0.14.0. https://github.com/houphouet/hstat

**BibTeX**
```bibtex
@Manual{hstat,
  title  = {HStat: Application Shiny interactive pour l'analyse statistique},
  author = {Houphouet KOUADIO},
  year   = {2026},
  note   = {Version 0.14.0},
  url    = {https://github.com/houphouet/hstat},
}
```

The application also has a **"Citer HStat"** tab offering the citation in Text,
BibTeX, RIS, APA, Vancouver and Markdown styles, with copy and download buttons.

---

## License

This project is licensed under the GPL-3.0 License.

---

## Author

**Houphouet KOUADIO**
ORCID: [0000-0002-8238-1091](https://orcid.org/0000-0002-8238-1091)

Development started on **17 September 2025** (développement débuté le 17 septembre 2025).

---

*HStat is developed to make statistical analysis accessible without any
programming barrier.*
