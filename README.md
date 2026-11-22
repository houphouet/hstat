# HStat — statistical analysis in your browser, without writing code

[![tests](https://github.com/houphouet/hstat/actions/workflows/tests.yml/badge.svg)](https://github.com/houphouet/hstat/actions/workflows/tests.yml)
![](https://hstatistique.goatcounter.com/count?p=%2Fgithub&t=GitHub%20README)

HStat is an R Shiny application that takes a dataset from import to publication:
cleaning, descriptive statistics, tests, multivariate analyses, machine
learning, qualitative coding, and the tables and figures you publish.
Everything runs through the interface. The whole app is bilingual
(**French / English**) and works **offline**.

---

## Install and run

```r
# install.packages("remotes")
remotes::install_github("houphouet/hstat")

library(HStat)
run_hstat()
```

---

## R packages

Installing HStat installs the packages it cannot start without. The others are
optional: each one serves an analysis or an interface element, `run_hstat()`
installs the missing ones on first run, and an analysis whose package is
absent says so and names an alternative.

**Required** (`Imports`): `cluster`, `data.table`, `DBI`, `dplyr`, `DT`,
`emmeans`, `ggplot2`, `haven`, `htmltools`, `magrittr`, `MASS`, `multcomp`,
`multcompView`, `openxlsx`, `plotly`, `pROC`, `psych`, `readxl`, `rlang`,
`scales`, `shiny`, `shinydashboard`, `shinyjs`, `stats`, `svglite`, `tidyr`,
`vegan`.

**Optional** (`Suggests`):

| Used for | Packages |
|---|---|
| Interface and figures | `colourpicker`, `shinycssloaders`, `shinyWidgets`, `shinyalert`, `sortable`, `ggtext`, `ggrepel`, `patchwork`, `treemapify`, `RColorBrewer`, `corrplot`, `tibble`, `base64enc`, `zip` |
| Data and missing values | `duckdb`, `VIM`, `mice`, `missForest`, `bestNormalize` |
| Tests and mixed models | `afex`, `agricolae`, `ARTool`, `car`, `EMT`, `FSA`, `PMCMRplus`, `rcompanion`, `pwr`, `glmmTMB`, `lme4`, `lmerTest`, `lmtest`, `heplots` |
| Multivariate analysis and clustering | `FactoMineR`, `factoextra`, `clustMixType`, `klaR`, `poLCA`, `mclust`, `dbscan`, `lavaan`, `pls` |
| Machine learning and deep learning | `randomForest`, `xgboost`, `e1071`, `kknn`, `glmnet`, `rpart`, `nnet`, `neuralnet`, `torch` |
| Time series and epidemiology | `forecast`, `prophet`, `dlm`, `dlnm`, `survival` |
| AI services, network and start-up | `httr`, `curl`, `jsonlite`, `later` |
| Test suite | `testthat` |

---

## What you can do

| Step | Tabs |
|---|---|
| **Load** | CSV, Excel (several sheets at once), Parquet, DuckDB, SPSS, Stata — or a dataset built into R, by name |
| **Prepare** | Overview, data health check, cleaning, filtering, recoding |
| **Describe** | Descriptive statistics, 14 chart types, correlations |
| **Test** | t-test (Student *and* Welch, each under its own name), ANOVA (Fisher, Welch, repeated measures), non-parametric tests, chi-square, post-hoc comparisons protected by the omnibus test, tests against a reference value |
| **Explore** | PCA, CA, MCA, FAMD, clustering, HCPC, DFA, PLS, SEM/CFA, MANOVA/PERMANOVA — 14 multivariate analyses |
| **Genetic selection** | Varieties, lines, parents, crosses, hybrids and breeds across sites and years: variance components and heritabilities, genetic advance and selective accuracy, combining ability (GCA/SCA) from a Griffing diallel or a Kempthorne line × tester design, Baker ratio, heterosis, multi-trait ranking of crosses, AMMI (ASV, YSI, WAAS), GGE biplot (which-won-where, mean vs stability, environments), Eberhart-Russell, Wricke, Shukla and Lin-Binns stability, MGIDI, MTSI and GYT biplot, environmental covariates and factorial regression, genotypic correlations and path analysis, and a final list of what improves or depreciates the target trait |
| **Model** | Machine learning, deep learning, time series and forecasting |
| **Qualitative** | Frequency and cross-tabulation, Likert scales, text analysis, CAQDAS coding workbench |
| **Agronomy** | Yield and yield gain against an untreated control, efficacy thresholds (Abbott's formula), harvest-loss and relative-yield indicators |
| **Ecology** | Alpha and beta diversity: some forty indices, richness estimators, rarefaction, Baselga partition, published reading grids with their source |
| **Epidemiology** | DLNM (exposure–lag–response), incidence rates, adjusted OR *and* RR, survival (Kaplan-Meier, Cox), case-crossover, standardisation and SMR, diagnostic tests and ROC, impact measures |
| **Plan** | Experimental designs, sample size and statistical power |
| **Dose** | Product dose and active-ingredient rate per hectare, spray mix, tank loads, and serial dilutions (working solutions) |
| **DL50 / CL50** | Dose-mortality probit regression (Henry line), natural mortality by Abbott or EM, lethal doses with standard error and tolerance standard deviation, Fieller intervals, potency ratio (resistance ratio), trial comparison and merging, native WIN DL file import/export |

Every analysis states its **assumptions**, gives an **interpretation in plain
language**, and exports its tables (CSV, Excel) and its figures (PNG, JPEG,
TIFF, SVG, PDF, EPS) at a resolution you choose.

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
├── .claude
│   └── agents                      # project subagents
│       ├── hstat-design.md
│       ├── hstat-performance.md
│       ├── hstat-securite.md
│       └── hstat-telechargements.md
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
│   │       ├── hstat-a11y.js       # accessibility: icon labels, field labels, skip link, back-to-top button
│   │       ├── hstat-decimale.js   # comma/point decimal separator asked before each download
│   │       ├── hstat-favicon.svg   # tab icon; without it, a 404 on every visit
│   │       ├── hstat-i18n.js       # FR/EN toggle applied to the rendered text
│   │       ├── hstat-menu.js       # sidebar sections that unfold on hover and stay open on click
│   │       ├── hstat-session.js    # session persistence: reconnect banner, keep-alive
│   │       ├── hstat-theme.css
│   │       ├── lib
│   │       │   └── nouislider          # noUiSlider 7.0.10, the jQuery plugin DT expects (Debian ships v15)
│   │       │       ├── jquery.nouislider.min.css
│   │       │       └── jquery.nouislider.min.js
│   │       └── Sortable.min.js     # drag-and-drop for the coding workbench
│   └── CITATION
├── man
│   └── run_hstat.Rd
├── NAMESPACE
├── R
│   ├── _disable_autoload.R         # stops Shiny sourcing R/ into the app environment
│   ├── mod_ai.R                    # shared inference engine (LLM providers)
│   ├── mod_clean.R
│   ├── mod_coding.R                # CAQDAS coding workbench
│   ├── mod_descriptive.R
│   ├── mod_design.R
│   ├── mod_diversity.R             # indices de diversité écologique (α, β, seuils)
│   ├── mod_dl.R
│   ├── mod_dl50.R                  # DL50/CL50 : régression probit dose-mortalité
│   ├── mod_dosage.R                # doses à l'hectare, solutions filles, solution mère
│   ├── mod_epidemio.R              # épidémiologie : DLNM, taux, risque, survie, SMR, ROC
│   ├── mod_explore.R
│   ├── mod_filter.R
│   ├── mod_genetique.R             # sélection génétique : Griffing, AMMI, GGE, MGIDI, MTSI, GYT
│   ├── mod_ml.R
│   ├── mod_qualitative.R
│   ├── mod_tests.R
│   ├── mod_threshold.R
│   ├── mod_timeseries.R
│   ├── mod_viz.R                   # modules: no source() order left to hold
│   ├── mod_yield.R                 # yield and yield gain against an untreated control
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
> KOUADIO, Houphouet & Claude Code (2026). HStat: Application Shiny interactive pour l'analyse statistique. Version 1.62.0. https://github.com/houphouet/hstat

**BibTeX**
```bibtex
@Manual{hstat,
  title  = {HStat: Application Shiny interactive pour l'analyse statistique},
  author = {Houphouet KOUADIO and {Claude Code}},
  year   = {2026},
  note   = {Version 1.62.0},
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
