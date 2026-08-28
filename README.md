# HStat — statistical analysis in your browser, without writing code

[![tests](https://github.com/houphouet/hstat/actions/workflows/tests.yml/badge.svg)](https://github.com/houphouet/hstat/actions/workflows/tests.yml)

HStat is an R Shiny application that takes a dataset from import to publication:
cleaning, descriptive statistics, tests, multivariate analyses, machine
learning, qualitative coding — then a report you can hand in. Everything runs
through the interface. The whole app is bilingual (**French / English**) and
works **offline**.

---

## Install and run

```r
# install.packages("remotes")
remotes::install_github("houphouet/hstat")

library(HStat)
run_hstat()
```

The app opens in your browser. Missing packages are installed on first run.
Requires **R ≥ 4.4**.

---

## What you can do

| Step | Tabs |
|---|---|
| **Load** | CSV, Excel (several sheets at once), Parquet, DuckDB, SPSS, Stata |
| **Prepare** | Overview, data health check, cleaning, filtering, recoding |
| **Describe** | Descriptive statistics, 25+ chart types, correlations |
| **Test** | t-test, ANOVA (incl. repeated measures), non-parametric tests, chi-square, post-hoc comparisons, tests against a reference value |
| **Explore** | PCA, CA, MCA, FAMD, clustering, HCPC, DFA, PLS, SEM/CFA, MANOVA/PERMANOVA — 14 multivariate analyses |
| **Model** | Machine learning, deep learning, time series and forecasting |
| **Qualitative** | Frequency and cross-tabulation, Likert scales, text analysis, CAQDAS coding workbench |
| **Plan** | Experimental designs, sample size and statistical power |
| **Dose** | Product dose and active-ingredient rate per hectare, spray mix, tank loads, and serial dilutions (working solutions) |
| **DL50 / CL50** | Dose-mortality probit regression (Henry line), natural mortality by Abbott or EM, lethal doses with standard error and tolerance standard deviation, Fieller intervals, potency ratio (resistance ratio), trial comparison and merging, native WIN DL file import/export |
| **Report** | Interpretation, reproducibility journal, automatic report (HTML / Word / PDF) |

Every analysis states its **assumptions**, gives an **interpretation in plain
language**, and exports its tables (CSV, Excel) and its figures (PNG, JPEG,
TIFF, SVG, PDF, EPS) at a resolution you choose.

---

## Six things worth knowing

### 1. It tells you which analysis your data call for

A dedicated tab reads your variables — types, group sizes and balance,
per-group normality, homogeneity of variance, pairing — and recommends the
analysis those rules point to, **with the reason**. No language model is
involved: a statistical test should not be suggested by text generation.

It never runs the analysis for you. The method stays your decision, and your
responsibility. And it refuses to advise on a variable it cannot analyse: an
empty column is reported as *blocking*, by name, rather than typed as binary
and handed a chi-square test.

### 2. Your data are never translated

Switching to English must not turn the `Oui` in your table into `Yes`. For a
statistics tool that is the worst possible defect, because nothing breaks — it
just quietly rewrites what you came to read.

So the server declares your column names and category labels to the browser,
and nothing on that list is ever translated. Inside a table cell, only
sentences are (over 25 characters): a data value is almost never a whole
sentence, an interpretation always is.

### 3. Bigger-than-RAM files

Under 500 MB a file is read into memory. Above it, CSV / Parquet / DuckDB files
are **never loaded**: DuckDB queries them on disk, exact statistics are computed
by SQL on the *full* dataset, and interactive analyses run on a reproducible
random sample. Files with more than 2.1 billion rows are supported.

### 4. A qualitative coding workbench (CAQDAS)

Read one open-ended answer at a time, select a passage, drop it on a code. Codes
nest, and a parent counts its whole branch. Memos attach to a code, a document,
a segment or nothing. Then: retrieval, cross-tabulation, complex queries
(*A AND B* / *A EXCEPT B*, with the scope stated alongside the count),
concordance (KWIC), document portraits, intercoder agreement (Cohen's kappa),
word clouds and concept maps.

A coding assistant proposes a codebook and pre-codes the answers. Its default
engine is a **local model** (Ollama or any OpenAI-compatible server) — free, and
your survey responses never leave your machine. A second engine needs no model
at all: it clusters corpus terms by co-occurrence, entirely inside R.

### 5. The session survives a locked screen

When the machine sleeps, the browser drops its connection and a Shiny app
normally dies with it. HStat keeps the session alive, shows a banner saying the
work is **not lost**, and reconnects on its own — when you unlock the machine,
return to the tab, or the network comes back. **Only you close the
application.**

### 6. Everything you did is written down

Each analysis is recorded as you run it. Two tabs turn that into deliverables:

- **Reproducibility journal** — the session as an executable R script. Steps
  whose settings were purely interactive are flagged `NON RECONSTITUE` in a
  comment rather than guessed: a script that silently differed from what the app
  computed would be worse than no script. The generated script is tested to
  parse and to run.
- **Automatic report** — dataset summary, data-quality findings, every analysis
  with its tables and figures, the interpretation, and the script as an
  appendix. HTML is assembled in R and therefore always available (figures
  embedded, so the file stays mailable); Word and PDF go through pandoc, and
  when it is missing the app says so and falls back to HTML rather than failing.
  Figures are drawn for print: **1000 dpi minimum**.

---

## Errors you can act on

R speaks English, and it speaks to statisticians: *"data are essentially
constant"*, *"system is computationally singular"*. HStat translates the errors
it shows into French sentences that name the **cause** and then the **gesture**
— which variable to change, which filter to check, which package to install,
which analysis to use instead. The original R message is kept in parentheses:
it is what you would paste when asking for help.

---

## Configuration

All optional, all environment variables:

| Variable | Default | Purpose |
|---|---|---|
| `HSTAT_MAX_UPLOAD_MB` | `102400` (100 GB) | Maximum upload size |
| `HSTAT_BIGDATA_THRESHOLD_MB` | `500` | Switch to the out-of-memory engine |
| `HSTAT_SAMPLE_SIZE` | `100000` | Working sample size |
| `HSTAT_DUCKDB_MEMORY` | *(unset)* | DuckDB RAM cap, e.g. `8GB` |
| `HSTAT_PLOT_MAX_POINTS` | `100000` | Maximum points drawn on a scatterplot |
| `HSTAT_DIST_MAX_N` | `5000` | Cap for O(n²) distance-matrix analyses |
| `HSTAT_KENDALL_MAX_N` | `20000` | Cap for Kendall correlation |
| `HSTAT_IMPUTE_MAX_N` | `100000` | Cap for kNN / missForest imputation |
| `HSTAT_ML_MAX_N` | `200000` | Training-set cap for ML and DL models |

---

## Contributing

Conventions, design decisions and the defects behind them are recorded in
[`CLAUDE.md`](CLAUDE.md) — read it before changing anything.

Run the test suite from the repository root:

```r
testthat::test_dir("tests/testthat")
```

CI runs on every push: a dependency-free syntax pass over all R files, a version
consistency check, then the full suite.

---

## Project structure

```
.
├── .github
│   └── workflows
│       └── tests.yml               # CI: dependency-free syntax pass, then the testthat suite
├── .Rbuildignore                   # keeps working files (.claude, CLAUDE.md) out of the tarball
├── .gitignore                      # keeps R run artefacts and build tarballs out of the repository
├── app.R                           # deployment bridge to inst/app (shinyAppDir — never setwd)
├── CLAUDE.md                       # repository conventions
├── DESCRIPTION                     # single source of truth for the version number
├── Hstat.Rproj
├── inst
│   ├── app
│   │   ├── app.R                   # standard Shiny entry point; serves www/
│   │   ├── app_server.R            # server(): shared state and multivariate analyses
│   │   ├── HStat.R                 # bridge, UX.R, app_server.R, then shinyApp(ui, server)
│   │   ├── i18n
│   │   │   └── fr-en.csv           # translation pairs; adding a language is a CSV edit
│   │   ├── Utils.R                 # bridge to R/utils.R + start-up side effects
│   │   ├── UX.R                    # ui: every tab
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
│   │       ├── hstat-i18n.js       # FR/EN toggle applied to the rendered text
│   │       ├── hstat-session.js    # session persistence: reconnect banner, keep-alive
│   │       ├── hstat-theme.css
│   │       └── Sortable.min.js     # drag-and-drop for the coding workbench
│   └── CITATION
├── man
│   └── run_hstat.Rd
├── NAMESPACE
├── R
│   ├── _disable_autoload.R         # stops Shiny sourcing R/ into the app environment
│   ├── mod_ai.R                    # inference engine, decision support, reproducibility journal
│   ├── mod_clean.R
│   ├── mod_coding.R                # CAQDAS coding workbench
│   ├── mod_descriptive.R
│   ├── mod_design.R
│   ├── mod_dl.R
│   ├── mod_dl50.R                  # DL50/CL50 : régression probit dose-mortalité
│   ├── mod_dosage.R                # doses à l'hectare et solutions filles
│   ├── mod_explore.R
│   ├── mod_filter.R
│   ├── mod_ml.R
│   ├── mod_qualitative.R
│   ├── mod_report.R                # automatic report (HTML / Word / PDF)
│   ├── mod_tests.R
│   ├── mod_threshold.R
│   ├── mod_timeseries.R
│   ├── mod_viz.R                   # modules: no source() order left to hold
│   ├── run_hstat.R                 # run_hstat(): installs missing packages, then launches
│   ├── utils.R                     # shared engine: definitions only, no side effect at load
│   └── zzz.R                       # startup message with the citation
├── README.md
├── tests
│   ├── testthat
│   │   └── test-hstat.R            # the reference suite
│   └── testthat.R                  # entry point used by R CMD check
└── tools
    ├── mutation.R                  # mutation bench: does an assertion actually bite?
    └── paste2trf.R                 # paste0("texte ", x) -> trf("texte %s", x)
```
---

## How to cite / Comment citer

If HStat is useful for your work, please cite it. In R:

```r
citation("HStat")
```

Or use one of the following:

**Text**
> KOUADIO, Houphouet & Claude Code (2026). HStat: Application Shiny interactive pour l'analyse statistique. Version 0.71.0. https://github.com/houphouet/hstat

**BibTeX**
```bibtex
@Manual{hstat,
  title  = {HStat: Application Shiny interactive pour l'analyse statistique},
  author = {Houphouet KOUADIO and {Claude Code}},
  year   = {2026},
  note   = {Version 0.71.0},
  url    = {https://github.com/houphouet/hstat},
}
```

The application also carries a **"Citer HStat"** tab offering the citation in
Text, BibTeX, RIS, APA, Vancouver and Markdown styles, with copy and download
buttons.

---

## License

GPL-3.0.

---

## Authors

**Houphouet KOUADIO** — ORCID: [0000-0002-8238-1091](https://orcid.org/0000-0002-8238-1091)

**Claude Code** — AI pair programmer (Anthropic). Contributed the package
conversion, the test suite, the bilingual layer and a large share of the defect
fixes recorded in `CLAUDE.md`.

Development started on **17 September 2025** (développement débuté le
17 septembre 2025).
