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

## Bilingual (French / English), offline

A **FR / EN** toggle sits in the header. The choice survives a reload.

The mechanism is deliberately unusual, and the reason is arithmetic: the app
holds around **9 700 user-facing strings across 24 R files**. Wrapping each one
in a translation call would touch every line of code and take weeks. So the
translation is applied to the **rendered text**, in the browser, from a
dictionary embedded in the page — the interface built by `UX.R`, the module
UIs, notifications and rendered tables all pass through the same filter without
a single call site being modified. A `MutationObserver` catches whatever Shiny
renders later.

Three properties follow:

- **Offline, always.** The dictionary ships inside the page. No request, ever.
  No translation API: it would need a connection, cost money per use, and
  mistranslate statistical vocabulary.
- **Light.** A CSV of pairs, ~48 KB of JSON in the page — less than one of the
  bundled fonts. Templates filled in by R never travel: translated before they
  exist, they could never match anything in the page. Terms spelled the same in both languages are recorded as a
  translation decision but never sent, since replacing them would change
  nothing on screen.
- **Graceful.** The key *is* the French string, so anything not yet translated
  stays in French rather than showing a technical identifier. Partial coverage
  degrades quietly; it never breaks a screen.

Switching back to French restores the exact original text, kept on each node —
not a reverse lookup, which would lose accents and collapse distinct terms.

Translations live in `inst/app/i18n/fr-en.csv`: two columns, `fr` and `en`.
Adding a language pair is a CSV edit, not a code change. `hstat_i18n_coverage()`
reports what is covered and names what is missing.

### Your data is never translated

This is the rule everything else bends around. A column of your file holding
`Oui` / `Non` — or `Total`, `Normal`, `Moyenne` — matches an interface label
**word for word**. Early on, switching to English turned the `Oui` in the
preview into `Yes`: the app was rewriting the data you had come to read. For a
statistics tool that is the worst possible defect, because nothing breaks — it
just quietly lies.

Two barriers now stand in the way, one exact and one heuristic:

- **The terms of your file are declared.** The server sends the browser the
  column names and the categories of your qualitative variables. Nothing on
  that list is ever translated, anywhere on the page — including in a table
  added to the app later, which would have escaped any hand-placed annotation.
  The price is accepted: if one of your columns is called `Total`, the
  interface label *Total* stops being translated too.
- **Inside a table cell, only sentences are translated** (over 25 characters).
  A data value is almost never a whole sentence, whereas an interpretation
  always is.

The same rule holds for the sentences R composes itself. `trf()` translates the
**template** — `"%s: %d value(s) changed"` — and then fills it in, so the
arguments, which are precisely where your variable names and values live, pass
through without ever being read. Here the guarantee comes from the construction
rather than from a precaution.

**Current coverage: every tab and box title, every widget label, the verdicts,
the error messages, and the 228 composed sentences of every module** — metrics,
recommendation engine, statistical tests, qualitative analyses, experimental
design, machine learning, time series, cleaning and reports.

One rule shapes the dictionary: **an ambiguous word never goes in on its own.**
*Moyenne* means *medium* for an effect size and *mean* in statistics; a single
key would corrupt the other sense. The nuance is carried by the whole sentence
instead — four complete sentences rather than a template and an adjective. A
word chosen by the code that *isn't* ambiguous (*équiprobables*, *blocs égaux*)
is opted in explicitly at the call site. `trf()` never translates its arguments
by itself; that is what protects your values.

---

## Excel workbooks: several sheets, one dataset

A survey workbook usually carries **one sheet per year, per site or per wave**.
Reading only the first one throws the rest of the data away, and copying each
sheet into its own file just to be able to combine them is manual work the app
can do.

Select the workbook and HStat lists its sheets. Tick the ones you want, and it
**looks at their structure to tell you how to combine them** rather than
leaving you to guess:

- same columns everywhere → **stack them**, one row per observation, each row
  keeping the name of the sheet it came from. With *number extracted*, a sheet
  named `2024` yields `2024` as a **numeric** column — a real year variable you
  can analyse, not a label;
- different columns with something in common → **join on a key**, so a
  reference sheet (site → region) enriches every observation.

Empty or unreadable sheets are skipped and **named**, so one malformed sheet in
a workbook of twelve doesn't block the other eleven. The result replaces the
working dataset. Under the hood this is the same merge engine used for multiple
files — sheets have no reason to obey a different logic.

The multi-file merge section works the same way: **a workbook dropped there is
unfolded into its sheets**, so one workbook with three sheets is enough to feed
a merge — two separate files are no longer required. The key selectors list the
columns of each sheet, so joins are configured exactly as they are between
files.

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
- **A hierarchical code system.** Codes nest: *Prix > Trop cher > Livraison
  tardive*. A parent shows both its own segment count and the **cumulative
  count for its whole branch** — "how many segments mention price, sub-codes
  included?" is the question a hierarchy exists to answer, and a total that
  ignored sub-codes would read zero on a heavily documented parent. The same
  label is allowed under two different parents (*Prix > Qualité* and *Service >
  Qualité* are two different things) but refused twice under the same one.
  Deleting a code lifts its sub-codes one level rather than silently taking
  their coding with them, and a code can never become its own descendant — its
  branch would detach from the tree and vanish from view.
- **Memos** — on a code (why it exists, where its boundary runs), on a document
  (what makes this interview atypical), on a segment, or free-standing (the
  hypothesis taking shape). This is what turns coding into analysis, and what a
  reviewer asks for when they want to understand how you got there. Full-text
  search ignores case and accents; memos already held in the codebook are
  carried over automatically, so no older project loses them.
- **Retrieval and cross-tabulation** — click a code to list every excerpt it
  covers, and cross the coded text with respondent profiles (for instance, only
  the price complaints made by the "Moins de 25 ans" group).
- **Complex coding query** — cross two sets of codes: *A AND B*, *A EXCEPT B*,
  *A OR B*. The **scope changes the answer**, so it is stated alongside the
  count: *same document* (both themes coexist in one respondent's answer, even
  ten lines apart), *same passage* (one excerpt carries both labels), or
  *within N characters* (the ideas follow one another without overlapping). On
  the same corpus the three scopes returned 23, 16 and 3 excerpts — reporting a
  figure without saying which one was used would be meaningless.
- **Concordance (KWIC)** — every occurrence of a word with its left and right
  context. This is the tool that *precedes* coding: you see how a word is
  actually used before deciding which code it deserves. The search pattern is
  escaped by default, so typing `prix (cher)` searches for that text instead of
  raising an unmatched-parenthesis error.
- **Document portrait (codeline)** — one document as a band where each code
  occupies the stretch of text it labels, so the order of the discourse is
  visible at a glance. Positions are given as a **percentage of the document**,
  not in characters, so answers of very different lengths stay comparable.
- **Intercoder agreement** — percentage agreement and Cohen's kappa between two
  coders. The unit compared is the *document × code* pair: two coders never cut
  at the same boundaries, and comparing segments would require an arbitrary
  overlap threshold that moves the result more than the real disagreement does.
  Kappa is undefined when both coders label everything (or nothing) the same
  way — the expected chance agreement is already 1 — so the verdict has a
  fourth state, *indeterminable*, and the percentage agreement is shown instead
  of a `NaN`.
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

Figures are drawn **for print, not for the screen**: **1000 dpi minimum**, and
that is a floor, not a default — a lower value passed anywhere is raised back
to it. Journals typically ask for 300–600 dpi; at 150 dpi a figure looks sharp
on screen and comes out blurred on paper, and you only find out once the
document has been submitted. 1200 and 2400 dpi are offered for stricter
requirements. The cost is measured, not assumed: a scatterplot at 9 × 5.5 in
weighs 0.36 MB at 1000 dpi and takes about two seconds. Only the on-screen
preview is exempt — it checks the layout, it is not what you print.

**It refuses to advise on a variable it cannot analyse.** A column with no
observed value at all has no type — and the engine used to type it as *binary*
(the set of its distinct non-missing values being empty, hence of size ≤ 2) and
recommend a chi-square test of independence on it. Confidently recommending an
impossible analysis is worse than recommending nothing: it is what a user
follows without suspicion. Such variables are now reported as **blocking**, by
name, pointing at the cleaning tab.

**Errors you can act on.** R speaks English, and it speaks to statisticians:
*"data are essentially constant"*, *"incorrect number of dimensions"*,
*"system is computationally singular"*. HStat translates the errors it surfaces
into French sentences that name the **cause** and then the **gesture** — which
variable to change, which filter to check, which package to install, which
analysis to use instead. The original R message is kept in parentheses, never
dropped: it is what you would paste when asking for help. A repo-wide test
fails if any new code puts a raw R message in front of the user.

**The session survives a locked screen.** When the computer sleeps or locks,
the browser suspends its connection and Shiny's websocket drops — the app used
to grey out and the session was gone, data and analyses with it. HStat now
keeps the session alive server-side (`allowReconnect`), replaces Shiny's grey
"Disconnected from the server" veil with a French banner saying the work is
**not lost**, and reconnects on its own — immediately when you unlock the
machine, come back to the tab, or the network returns. Closing the tab with
data loaded asks for confirmation. **Only you close the application.**

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
├── .github
│   └── workflows
│       └── tests.yml               # CI: dependency-free syntax pass, then the testthat suite
├── app.R                           # deployment bridge to inst/app (shinyAppDir — never setwd)
├── CLAUDE.md                       # repository conventions
├── DESCRIPTION                     # single source of truth for the version number
├── Hstat.Rproj
├── inst
│   ├── app
│   │   ├── app.R                   # standard Shiny entry point; serves www/
│   │   ├── app_server.R            # server(): shared state and multivariate analyses
│   │   ├── HStat.R                 # sources the modules in order, then shinyApp(ui, server)
│   │   ├── i18n
│   │   │   └── fr-en.csv           # translation pairs; adding a language is a CSV edit
│   │   ├── mod_ai.R                # inference engine, decision support, reproducibility journal
│   │   ├── mod_clean.R
│   │   ├── mod_coding.R            # CAQDAS coding workbench
│   │   ├── mod_descriptive.R
│   │   ├── mod_design.R
│   │   ├── mod_dl.R
│   │   ├── mod_explore.R
│   │   ├── mod_filter.R
│   │   ├── mod_ml.R
│   │   ├── mod_qualitative.R
│   │   ├── mod_report.R            # automatic report (HTML / Word / PDF)
│   │   ├── mod_tests.R
│   │   ├── mod_threshold.R
│   │   ├── mod_timeseries.R
│   │   ├── mod_viz.R
│   │   ├── Utils.R                 # shared computation helpers; sourced first
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
│   ├── run_hstat.R                 # run_hstat(): installs missing packages, then launches
│   └── zzz.R                       # startup message with the citation
├── README.md
└── tests
    ├── testthat
    │   └── test-hstat.R            # the reference suite
    └── testthat.R                  # entry point used by R CMD check
```
---

## How to cite / Comment citer

If HStat is useful for your work, please cite it. In R:

```r
citation("HStat")
```

Or use one of the following:

**Text**
> KOUADIO, Houphouet (2026). HStat: Application Shiny interactive pour l'analyse statistique. Version 0.35.4. https://github.com/houphouet/hstat

**BibTeX**
```bibtex
@Manual{hstat,
  title  = {HStat: Application Shiny interactive pour l'analyse statistique},
  author = {Houphouet KOUADIO},
  year   = {2026},
  note   = {Version 0.35.4},
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
