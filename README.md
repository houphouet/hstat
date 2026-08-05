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
- **Optional AI assistant** — with an Anthropic API key (typed in the UI or
  read from `ANTHROPIC_API_KEY`), Claude can propose a codebook from the corpus
  and pre-code the answers. Suggestions are tagged "IA" and can be reviewed or
  dropped in one click. Without a key, every other feature works unchanged.

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
> KOUADIO, Houphouet (2026). HStat: Application Shiny interactive pour l'analyse statistique. Version 0.8.0. https://github.com/houphouet/hstat

**BibTeX**
```bibtex
@Manual{hstat,
  title  = {HStat: Application Shiny interactive pour l'analyse statistique},
  author = {Houphouet KOUADIO},
  year   = {2026},
  note   = {Version 0.8.0},
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
