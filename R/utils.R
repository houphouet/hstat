# HStat -- socle partage : fonctions de calcul et utilitaires
# CE FICHIER NE FAIT QUE DEFINIR.

#' Socle partage de HStat
#'
#' Definitions communes a toute l'application : calcul, mise en forme, export.
#'
#' RIEN N'EST EXPORTE ICI, et ce n'est pas un oubli. Le pont
#' (`inst/app/Utils.R`) recopie les objets depuis l'ESPACE DE NOMS
#' (`ls(asNamespace("HStat"), all.names = TRUE)`), ce qui atteint aussi bien
#' les aides internes `.hstat_*` -- les exports n'y changent rien. Un
#' `exportPattern(".")` ne servait donc a rien, et coutait cher : `R CMD check`
#' reclame une fiche de documentation par objet exporte, soit ~240 fiches
#' impossibles a tenir a jour.
#'
#' @keywords internal
"_PACKAGE"

# Boutons d'export DT avec noms de fichiers propres

.hstat_dt_buttons <- function(fname = "HStat_export") {
  fname <- gsub("[^A-Za-z0-9_.-]+", "_", as.character(fname))
  # Le separateur decimal se lit AU CLIC, dans le navigateur.
  corps <- function(mode) list(format = list(body = DT::JS(sprintf(
    "function (d) { return window.hstatDecimaleCellule ? window.hstatDecimaleCellule(d, '%s') : d; }",
    mode))))
  list(
    list(extend = "copy",  title = NULL, exportOptions = corps("texte")),
    # Avec la virgule, le champ se separe par un point-virgule : la convention
    # du CSV francais, sans laquelle « 1,5 » ferait deux colonnes.
    list(extend = "csv",   filename = fname, title = NULL, exportOptions = corps("texte"),
         action = DT::JS(paste(
           "function (e, dt, button, config) {",
           "config.fieldSeparator = (window.hstatDecimale && window.hstatDecimale() === ',') ? ';' : ',';",
           "$.fn.dataTable.ext.buttons.csvHtml5.action.call(this, e, dt, button, config); }"))),
    list(extend = "excel", filename = fname, title = NULL, exportOptions = corps("excel"))
  )
}

install_and_load <- function(packages) {
  installed_packages <- rownames(utils::installed.packages())
  to_install <- packages[!packages %in% installed_packages]

  if (length(to_install) > 0) {
    # On tente directement l'installation : install.packages() gere lui-meme
    # l'absence de reseau.
    repos <- getOption("repos")
    if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@") ||
        is.na(repos["CRAN"]))
      repos <- c(CRAN = "https://cloud.r-project.org")
    ok <- tryCatch({
      utils::install.packages(to_install, repos = repos)
      TRUE
    }, error = function(e) FALSE, warning = function(e) FALSE)

    still_missing <- to_install[!to_install %in% rownames(utils::installed.packages())]
    if (length(still_missing) > 0) {
      message("\n", strrep("=", 70),
              "\n  HStat -- certains paquets n'ont pas pu être installés",
              "\n  Manquants : ", paste(still_missing, collapse = ", "),
              "\n  Vérifiez votre connexion Internet, puis relancez l'application.",
              "\n  (Installation manuelle : install.packages(c(...)) )",
              "\n", strrep("=", 70), "\n")
    }
  }

  # Chargement : on n'interrompt pas l'application pour un package optionnel manquant
  missing_after <- character(0)
  for (pkg in packages) {
    # suppressMessages est indispensable.
    ok <- suppressWarnings(suppressMessages(suppressPackageStartupMessages(
      requireNamespace(pkg, quietly = TRUE))))
    if (ok) {
      suppressWarnings(suppressMessages(suppressPackageStartupMessages(
        library(pkg, character.only = TRUE))))
    } else {
      missing_after <- c(missing_after, pkg)
    }
  }
  if (length(missing_after) > 0)
    message("HStat : packages indisponibles (certaines fonctions seront limitées) : ",
            paste(missing_after, collapse = ", "))
}

required_packages <- c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets", "shinyalert", "DT",
  "shinycssloaders", "RColorBrewer", "colourpicker", "ggrepel", "openxlsx",
  "zip", "haven", "base64enc", "dplyr", "scales", "ggplot2", "sortable",
  "tibble", "plotly", "tidyr", "corrplot", "car", "agricolae", "pwr", "factoextra",
  "FactoMineR", "MASS", "cluster", "psych", "lmtest", "multcomp", "FSA",
  "treemapify", "ggtext", "stats", "emmeans", "PMCMRplus", "multcompView",
  "rcompanion", "EMT", "bestNormalize", "lme4", "lmerTest", "afex", "ARTool",
  "glmmTMB", "vegan", "heplots", "data.table", "patchwork", "lavaan", "pls",
  "klaR", "poLCA", "clustMixType", "nnet", "DBI", "duckdb", "htmltools",
  "magrittr", "readxl", "rlang", "svglite", "mice", "missForest",
  "VIM"
)

# -- Packages de modelisation predictive (series temporelles / ML / DL) -------
# CES PACKAGES NE DOIVENT JAMAIS ETRE ATTACHES PAR library().
hstat_model_packages <- c(
  "forecast", "glmnet", "rpart", "randomForest", "xgboost", "e1071",
  "kknn", "pROC", "dbscan", "mclust", "neuralnet",
  "dlm", "dlnm",          # modeles lineaires dynamiques & retards distribues
  "prophet", "torch",     # inclus (installes automatiquement)
  # Analyses multivariees etendues.
  "lavaan", "pls", "klaR", "poLCA", "clustMixType", "nnet"
)

hstat_load_model_packages <- function(packages = hstat_model_packages) {
  installed <- rownames(utils::installed.packages())
  to_install <- packages[!packages %in% installed]
  if (length(to_install) > 0) {
    repos <- getOption("repos")
    if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@") ||
        is.na(repos["CRAN"]))
      repos <- c(CRAN = "https://cloud.r-project.org")
    tryCatch(utils::install.packages(to_install, repos = repos),
             error = function(e) NULL, warning = function(w) NULL)
  }
  missing_after <- character(0)
  for (pkg in packages) {
    ok <- suppressWarnings(suppressMessages(suppressPackageStartupMessages(
      requireNamespace(pkg, quietly = TRUE))))
    if (!ok) missing_after <- c(missing_after, pkg)
  }
  # torch : ses bibliotheques natives (libtorch, ~600 Mo) ne sont JAMAIS
  # telechargees au demarrage -- cela bloquait l'apparition de l'interface.
  if (!"torch" %in% missing_after) {
    backend_ok <- tryCatch(torch::torch_is_installed(), error = function(e) FALSE)
    if (!isTRUE(backend_ok))
      message("HStat : les bibliothèques natives de torch ne sont pas encore ",
              "installées. Le module Deep Learning propose un bouton pour les ",
              "télécharger (~600 Mo, une seule fois) ; les modèles neuralnet ",
              "sont disponibles immédiatement.")
  }
  if (length(missing_after) > 0)
    message("HStat : packages de modélisation indisponibles ",
            "(les modèles correspondants seront limités) : ",
            paste(missing_after, collapse = ", "))
  invisible(missing_after)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Gardes pour packages d'UI optionnels

.hstat_has <- function(pkg) isTRUE(requireNamespace(pkg, quietly = TRUE))

.hstat_num1 <- function(x, default) {
  if (is.null(x) || length(x) == 0) return(default)
  v <- suppressWarnings(as.numeric(x[1]))
  if (length(v) == 0 || is.na(v) || !is.finite(v)) return(default)
  v
}

# FACTEUR D'INFLATION DE LA VARIANCE, d'un seul calcul.
hstat_vif <- function(df) {
  df <- remove_zero_var_cols(as.data.frame(df))
  p <- ncol(df)
  if (p == 0) return(stats::setNames(numeric(0), character(0)))
  if (p == 1) return(stats::setNames(Inf, names(df)))
  cc <- df[stats::complete.cases(df), , drop = FALSE]
  par_lm <- function() vapply(seq_len(p), function(i) {
    y <- df[[i]]; x <- remove_zero_var_cols(df[, -i, drop = FALSE])
    if (ncol(x) == 0) return(Inf)
    r2 <- tryCatch(suppressWarnings(summary(stats::lm(y ~ ., data = x))$r.squared),
                   error = function(e) NA_real_)
    if (is.na(r2) || r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1))
  v <- tryCatch({
    R <- stats::cor(cc)
    if (anyNA(R) || rcond(R) < 1e-12) NULL else diag(solve(R))
  }, error = function(e) NULL)
  if (is.null(v)) v <- par_lm()
  v[!is.finite(v) | v > 1e12] <- Inf
  stats::setNames(unname(v), names(df))
}

remove_zero_var_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(df)
  keep <- sapply(df, function(x) {
    if (!is.numeric(x)) return(TRUE)
    sd_val <- stats::sd(x, na.rm = TRUE)
    !is.na(sd_val) && sd_val > 0
  })
  df[, keep, drop = FALSE]
}

safe_cor <- function(df, use = "pairwise.complete.obs") {
  if (is.null(df) || !is.data.frame(df) || !ncol(df)) return(NULL)
  # `sapply()` sur un tableau SANS COLONNE rend `list()`, et `df[, list()]`
  # leve « invalid subscript type 'list' ».
  df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
  df <- remove_zero_var_cols(df)
  if (is.null(df) || ncol(df) < 2) return(NULL)
  tryCatch(suppressWarnings(stats::cor(df, use = use)), error = function(e) NULL)
}

# Tests de corrélation (Pearson / Kendall / Spearman) sur toutes les paires de
# variables numériques. 
hstat_correlation_tests <- function(df, vars, method = "pearson",
                                     alternative = "two.sided",
                                     conf.level = 0.95,
                                     p.adjust.method = "holm",
                                     target = NULL) {
  if (is.null(df) || length(vars) < 2) return(NULL)
  df <- df[, intersect(vars, names(df)), drop = FALSE]
  df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
  vars <- names(df)
  if (length(vars) < 2) return(NULL)

  combos <- utils::combn(vars, 2, simplify = FALSE)
  # Mode "une variable contre les autres".
  if (!is.null(target) && nzchar(target) && target %in% vars) {
    combos <- Filter(function(pr) target %in% pr, combos)
    combos <- lapply(combos, function(pr) c(target, setdiff(pr, target)))
  }
  rows <- lapply(combos, function(pr) {
    x <- df[[pr[1]]]; y <- df[[pr[2]]]
    ok <- stats::complete.cases(x, y)
    xx <- x[ok]; yy <- y[ok]; n <- length(xx)

    res <- if (n >= 3) tryCatch(
      suppressWarnings(stats::cor.test(
        xx, yy, method = method, alternative = alternative,
        conf.level = conf.level, exact = FALSE)),
      error = function(e) NULL) else NULL

    if (is.null(res)) {
      return(data.frame(
        Variable_X = pr[1], Variable_Y = pr[2], N = n, Methode = method,
        Coefficient = NA_real_, Statistique = NA_real_, ddl = NA_real_,
        p_value = NA_real_, IC_bas = NA_real_, IC_haut = NA_real_,
        R2 = NA_real_, Force = "\u2014", Sens = "\u2014",
        stringsAsFactors = FALSE))
    }

    est  <- unname(res$estimate)
    stat <- unname(res$statistic)
    ddl  <- if (!is.null(res$parameter)) unname(res$parameter) else NA_real_
    ci   <- if (!is.null(res$conf.int)) res$conf.int else c(NA_real_, NA_real_)
    abs_r <- abs(est)
    force <- if (is.na(abs_r)) "\u2014"
             else if (abs_r < .1) "N\u00e9gligeable"
             else if (abs_r < .3) "Faible"
             else if (abs_r < .5) "Mod\u00e9r\u00e9e"
             else if (abs_r < .7) "Forte"
             else "Tr\u00e8s forte"
    sens <- if (is.na(est)) "\u2014" else if (est >= 0) "Positif" else "N\u00e9gatif"

    data.frame(
      Variable_X = pr[1], Variable_Y = pr[2], N = n, Methode = method,
      Coefficient = round(est, 4), Statistique = round(stat, 4), ddl = ddl,
      p_value = res$p.value,
      IC_bas = round(ci[1], 4), IC_haut = round(ci[2], 4),
      R2 = if (method == "pearson") round(est^2, 4) else NA_real_,
      Force = force, Sens = sens, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  out$p_ajuste <- stats::p.adjust(out$p_value, method = p.adjust.method)
  out$Significatif <- ifelse(is.na(out$p_ajuste), "\u2014",
                       ifelse(out$p_ajuste < .001, "*** (p<.001)",
                       ifelse(out$p_ajuste < .01,  "** (p<.01)",
                       ifelse(out$p_ajuste < .05,  "* (p<.05)", "ns"))))
  out$p_value  <- signif(out$p_value, 4)
  out$p_ajuste <- signif(out$p_ajuste, 4)
  rownames(out) <- NULL
  out
}

is_categorical <- function(x) {
  is.factor(x) || is.character(x) || inherits(x, "Date") || inherits(x, "POSIXt")
}

get_categorical_cols <- function(df) {
  names(df)[vapply(df, is_categorical, logical(1))]
}

get_all_factor_candidates <- function(df, max_numeric_levels = 30) {
  nms <- names(df)
  keep <- sapply(nms, function(col) {
    x <- df[[col]]
    if (is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
    if (inherits(x, "Date") || inherits(x, "POSIXt"))    return(TRUE)
    if (is.numeric(x)) return(length(unique(stats::na.omit(x))) <= max_numeric_levels)
    FALSE
  })
  nms[keep]
}

# Echappement HTML.

# Le dictionnaire est cherche depuis le dossier de l'application, depuis la
# racine du depot, et en remontant.
hstat_i18n_path <- function() {
  rel <- c(file.path("i18n", "fr-en.csv"),
           file.path("inst", "app", "i18n", "fr-en.csv"))
  prefixes <- c(".", "..", file.path("..", ".."), file.path("..", "..", ".."))
  cands <- c(as.vector(outer(prefixes, rel, file.path)),
             system.file("app", "i18n", "fr-en.csv", package = "HStat"))
  hit <- cands[nzchar(cands) & file.exists(cands)]
  if (length(hit)) normalizePath(hit[1]) else NA_character_
}

# Le dictionnaire est lu UNE fois et garde en memoire : il est relu a chaque
# ouverture de session sinon, pour un fichier qui ne change pas.
.hstat_i18n_cache <- new.env(parent = emptyenv())

hstat_i18n_load <- function(path = hstat_i18n_path(), force = FALSE) {
  # Une cle vide leverait « attempt to use zero-length variable name ».
  cle <- if (length(path) != 1L || is.na(path) || !nzchar(path)) "(aucun)" else path
  if (!isTRUE(force) && !is.null(.hstat_i18n_cache[[cle]]))
    return(.hstat_i18n_cache[[cle]])
  vide <- data.frame(fr = character(0), en = character(0),
                     stringsAsFactors = FALSE)
  d <- if (identical(cle, "(aucun)")) vide else tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8",
                    colClasses = "character"),
    error = function(e) vide)
  if (!all(c("fr", "en") %in% names(d))) d <- vide
  d$fr <- trimws(as.character(d$fr)); d$en <- trimws(as.character(d$en))
  # Une entree vide ne sert a rien ; une entree en double rendrait la
  # traduction imprevisible.
  d <- d[nzchar(d$fr) & nzchar(d$en), , drop = FALSE]
  d <- d[!duplicated(d$fr), , drop = FALSE]
  .hstat_i18n_cache[[cle]] <- d
  d
}

# Dictionnaire pret a l'emploi : francais -> langue demandee.
hstat_i18n_dict <- function(lang = "fr", path = hstat_i18n_path()) {
  if (!identical(lang, "en")) return(stats::setNames(character(0), character(0)))
  d <- hstat_i18n_load(path)
  stats::setNames(d$en, d$fr)
}

# Traduction cote R, pour le texte que le serveur compose lui-meme (noms de
# fichiers telecharges, en-tetes de rapport).
tr <- function(x, lang = hstat_langue_session(), dict = NULL) {
  if (is.null(x) || !length(x)) return(x)
  if (!identical(lang, "en")) return(x)
  if (is.null(dict)) dict <- hstat_i18n_dict(lang)
  if (!length(dict)) return(x)
  out <- as.character(x)
  # LA RECHERCHE SE FAIT SUR LA CHAINE ELAGUEE, et l'espacement d'origine est
  # rendu autour de la traduction.
  net <- trimws(out)
  hit <- match(net, names(dict))
  ok <- !is.na(hit)
  if (any(ok)) {
    avant <- sub("^([[:space:]]*).*$", "\\1", out[ok])
    # L'espacement FINAL se prend par mesure, pas par expression reguliere : un
    # `sub` gourmand rendrait la chaine entiere.
    apres <- substring(out[ok], nchar(sub("[[:space:]]+$", "", out[ok])) + 1L)
    out[ok] <- paste0(avant, unname(dict[hit[ok]]), apres)
  }
  out
}

# Echappement d'une chaine JSON.
.hstat_json_esc <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  gsub("\t", "\\t", s, fixed = TRUE)
}

# Dictionnaire serialise pour le navigateur.
hstat_i18n_json <- function(lang = "en", path = hstat_i18n_path()) {
  d <- hstat_i18n_dict(lang, path)
  # Les termes identiques dans les deux langues sont retires ICI.
  d <- d[names(d) != unname(d)]
  # LES GABARITS NE PARTENT PAS AU NAVIGATEUR.
  d <- d[!grepl(HSTAT_I18N_MARQUEUR, names(d))]
  if (!length(d)) return("{}")
  esc <- .hstat_json_esc
  paste0("{", paste(sprintf("\"%s\":\"%s\"", esc(names(d)), esc(unname(d))),
                    collapse = ","), "}")
}

.hstat_i18n_ressource <- new.env(parent = emptyenv())

# Echappe tout caractere hors ASCII en \uXXXX (paires de substitution
# comprises).
.hstat_i18n_ascii <- function(s) {
  cp <- utf8ToInt(enc2utf8(s))
  if (!length(cp) || !any(cp > 127L)) return(s)
  ch <- strsplit(s, "")[[1]]
  # Au-dela du plan multilingue de base, \uXXXX ne suffit pas : JavaScript
  # attend une paire de substitution.
  h <- cp > 0xFFFFL
  if (any(h)) {
    v <- cp[h] - 0x10000L
    ch[h] <- sprintf("\\u%04x\\u%04x",
                     0xD800L + v %/% 0x400L, 0xDC00L + v %% 0x400L)
  }
  b <- cp > 127L & !h
  ch[b] <- sprintf("\\u%04x", cp[b])
  paste(ch, collapse = "")
}

# Ecrit le dictionnaire dans un fichier servi par l'application et rend son
# adresse estampillee.
hstat_i18n_asset <- function(lang = "en") {
  cle <- paste0("src.", lang)
  if (!is.null(.hstat_i18n_ressource[[cle]])) return(.hstat_i18n_ressource[[cle]])
  src <- tryCatch({
    nom <- paste0("hstat-i18n-", lang, ".js")
    dir <- file.path(tempdir(), "hstat-dict")
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    con <- file(file.path(dir, nom), open = "wb")
    on.exit(close(con), add = TRUE)
    writeBin(charToRaw(hstat_i18n_payload(lang)), con)
    shiny::addResourcePath("hstat-dict", normalizePath(dir, mustWork = TRUE))
    hstat_asset(paste0("hstat-dict/", nom))
  }, error = function(e) NA_character_)
  .hstat_i18n_ressource[[cle]] <- src
  src
}

# Le contenu du fichier, et la seule forme sous laquelle le dictionnaire entre
# dans le navigateur.
hstat_i18n_payload <- function(lang = "en") {
  paste0("window.HSTAT_I18N = ", .hstat_i18n_ascii(hstat_i18n_json(lang)), ";")
}

# La balise a poser dans l'en-tete, AVANT hstat-i18n.js qui lit
# `window.HSTAT_I18N` des son chargement.
.hstat_js_chaine <- function(x) {
  x <- as.character(x)[1]
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  gsub("[\r\n<]", "", x)
}

hstat_i18n_script <- function(lang = "en") {
  src <- hstat_i18n_asset(lang)
  # LE DICTIONNAIRE NE PART PLUS AVEC LA PAGE.
  if (!is.na(src))
    return(shiny::tags$script(shiny::HTML(sprintf(
      "window.HSTAT_I18N_SRC = \"%s\";", .hstat_js_chaine(src)))))
  # Repli : le dictionnaire redevient incorpore.
  shiny::tags$script(shiny::HTML(hstat_i18n_payload(lang)))
}

# Etat de la traduction : combien de chaines de l'interface sont couvertes, et
# lesquelles manquent.
hstat_i18n_coverage <- function(chaines, path = hstat_i18n_path()) {
  d <- hstat_i18n_load(path)
  chaines <- unique(trimws(as.character(chaines)))
  chaines <- chaines[nzchar(chaines)]
  traduites <- intersect(chaines, d$fr)
  list(total = length(chaines), traduites = length(traduites),
       manquantes = setdiff(chaines, d$fr),
       taux = if (!length(chaines)) 1 else length(traduites) / length(chaines))
}

# ETAT INITIAL DE LA SESSION : UNE SEULE SOURCE DE VERITE
# La reinitialisation remettait a NULL une liste de champs ENUMEREE A LA MAIN,
# distincte de celle qui cree `reactiveValues`.
hstat_valeurs_initiales <- function() {
  list(
    data = NULL, cleanData = NULL, filteredData = NULL, descStats = NULL,
    normResults = NULL, leveneResults = NULL, testResults = NULL,
    lastKruskal = NULL, multiResults = NULL,
    multiGroups = NULL, currentPlot = NULL, residualsNorm = NULL,
    leveneResid = NULL, multiNormResults = NULL, multiLeveneResults = NULL,
    pcaResult = NULL, clusterResult = NULL, currentModel = NULL,
    testInterpretation = NULL, cahResult = NULL, currentInteractivePlot = NULL,
    cahClusters = NULL, testResultsDF = NULL,
    multiResultsMain = NULL, multiResultsInteraction = NULL,
    normalityResults = NULL, homogeneityResults = NULL,
    currentVarIndex = 1, currentValidationVar = 1,
    allTestResults = list(), modelsList = list(),
    normalityResultsPerVar = list(), homogeneityResultsPerVar = list(),
    currentDiagVar = 1, currentResidVar = 1,
    customXOrder = NULL, posthocXLabels = NULL, posthocXStyles = NULL,
    y2Vars = NULL, dualAxisActive = FALSE,
    y2VarsActive = NULL, y2RangeForAxis = NULL, y2UnifiedColorMap = NULL,
    transformationLog = list(),
    chiSqResults = NULL, chiSqFreqData = NULL, chiSqPostHocData = NULL,
    chiSqPlotObj = NULL,
    chiSqPGlobal = NULL,
    # Moteur de donnees (memoire / hors-memoire DuckDB)
    dbCon = NULL, dbTable = NULL, dataMode = "memory",
    fullNrow = NULL, fullNcol = NULL, fullNA = NULL, isSampled = FALSE,
    sourceSize = NULL,
    # Chemin du fichier NEUTRALISE par la derniere reinitialisation.
    fichierNeutralise = NULL,
    resetSignal = 0
  )
}

# REMISE A ZERO DE L'ETAT DE SESSION -- une seule definition
# Deux gestes remettent l'application a zero : le bouton « Reinitialiser », et
# le CHARGEMENT D'UN NOUVEAU JEU DE DONNEES.
hstat_vider_valeurs <- function(rv, sauf = character(0)) {
  noms <- setdiff(names(shiny::isolate(shiny::reactiveValuesToList(rv))), sauf)
  for (nm in noms) {
    v <- shiny::isolate(rv[[nm]])
    if (inherits(v, "reactiveVal")) v(NULL) else rv[[nm]] <- NULL
  }
  invisible(noms)
}

# Ce qu'une remise a zero ne vide PAS : les essais de la DL50 ne dependent pas
# du jeu de travail (saisie, collage, fichier WIN DL).
HSTAT_VALEURS_PERSISTANTES <- c("dl50Essais")

# Une session merite d'etre deposee si elle porte des donnees OU des essais
# de DL50 : ces derniers se saisissent sans aucun fichier charge.
hstat_session_a_garder <- function(values) {
  if (!is.null(shiny::isolate(values$data))) return(TRUE)
  e <- shiny::isolate(values$dl50Essais)
  if (inherits(e, "reactiveVal")) e <- shiny::isolate(e())
  length(e) > 0
}

hstat_reinitialiser_valeurs <- function(values) {
  init <- hstat_valeurs_initiales()
  # LE COMPTEUR DE REMISE A ZERO EST MONOTONE.
  sig <- (shiny::isolate(values$resetSignal) %||% 0) + 1
  # Tout est vide d'abord -- y compris les champs crees en cours de session par
  # un module, qui ne figurent pas dans la liste initiale.
  hstat_vider_valeurs(values, sauf = HSTAT_VALEURS_PERSISTANTES)
  for (nm in names(init)) values[[nm]] <- init[[nm]]
  values$resetSignal <- sig
  invisible(sig)
}

# GARDE DE SESSION : UNE VEILLE NE FERME PAS L'APPLICATION
# Verrouiller l'ordinateur ou le mettre en veille coupe le websocket de Shiny.
# 1. LA CLE EST UNE CAPACITE.
# 2. LA REPRISE EST A USAGE UNIQUE.
# 3. LE DEPOT EST BORNE, en duree ET en nombre.
# 4. UN `reactiveVal` SE REPREND EN L'APPELANT.
HSTAT_SESSION_GARDE_HEURES <- 24
HSTAT_SESSION_GARDE_MAX    <- 20L
HSTAT_SESSION_CLE_MOTIF    <- "^[A-Za-z0-9_-]{32,64}$"

.hstat_garde <- new.env(parent = emptyenv())

hstat_session_cle_valide <- function(cle) {
  is.character(cle) && length(cle) == 1L && !is.na(cle) &&
    grepl(HSTAT_SESSION_CLE_MOTIF, cle)
}

.hstat_garde_heures <- function() {
  h <- suppressWarnings(as.numeric(getOption("hstat.session.garde.heures",
                                             HSTAT_SESSION_GARDE_HEURES))[1])
  if (is.na(h) || h < 0) HSTAT_SESSION_GARDE_HEURES else h
}

# Retire une entree ET libere ce qu'elle tient.
.hstat_garde_retirer <- function(cle, liberer = TRUE) {
  e <- .hstat_garde[[cle]]
  rm(list = cle, envir = .hstat_garde)
  if (liberer && is.function(e$liberer)) try(e$liberer(), silent = TRUE)
  invisible(e)
}

hstat_session_garde_purger <- function(maintenant = Sys.time()) {
  limite <- maintenant - .hstat_garde_heures() * 3600
  for (cle in ls(.hstat_garde, all.names = TRUE)) {
    if (.hstat_garde[[cle]]$quand < limite) .hstat_garde_retirer(cle)
  }
  invisible(NULL)
}

hstat_session_deposer <- function(cle, etat, liberer = NULL,
                                  maintenant = Sys.time()) {
  if (!hstat_session_cle_valide(cle)) return(invisible(FALSE))
  hstat_session_garde_purger(maintenant)
  # Une cle deja deposee est remplacee : seule la derniere coupure compte.
  if (exists(cle, envir = .hstat_garde, inherits = FALSE))
    .hstat_garde_retirer(cle)
  assign(cle, list(etat = etat, liberer = liberer, quand = maintenant),
         envir = .hstat_garde)
  cles <- ls(.hstat_garde, all.names = TRUE)
  if (length(cles) > HSTAT_SESSION_GARDE_MAX) {
    quand <- vapply(cles, function(k) as.numeric(.hstat_garde[[k]]$quand), 0)
    for (k in cles[order(quand)][seq_len(length(cles) - HSTAT_SESSION_GARDE_MAX)])
      .hstat_garde_retirer(k)
  }
  invisible(TRUE)
}

# Rend l'etat depose sous `cle` et le RETIRE du depot, sans le liberer : la
# connexion DuckDB passe a la session qui le reprend.
hstat_session_reprendre <- function(cle, maintenant = Sys.time()) {
  if (!hstat_session_cle_valide(cle)) return(NULL)
  hstat_session_garde_purger(maintenant)
  if (!exists(cle, envir = .hstat_garde, inherits = FALSE)) return(NULL)
  .hstat_garde_retirer(cle, liberer = FALSE)$etat
}

# Cliche d'un `reactiveValues` : une liste de REFERENCES (rien n'est copie).
hstat_session_cliche <- function(rv, sauf = character(0)) {
  l <- shiny::isolate(shiny::reactiveValuesToList(rv))
  l <- l[setdiff(names(l), sauf)]
  lapply(l, function(v)
    if (inherits(v, "reactiveVal")) structure(list(shiny::isolate(v())),
                                              class = "hstat_rv_valeur")
    else v)
}

hstat_session_restaurer <- function(rv, cliche, sauf = character(0)) {
  if (!is.list(cliche)) return(invisible(character(0)))
  noms <- setdiff(names(cliche), c(sauf, ""))
  for (nm in noms) {
    v <- cliche[[nm]]
    if (inherits(v, "hstat_rv_valeur")) {
      cible <- shiny::isolate(rv[[nm]])
      if (inherits(cible, "reactiveVal")) cible(v[[1]])
      else rv[[nm]] <- shiny::reactiveVal(v[[1]])
    } else {
      rv[[nm]] <- v
    }
  }
  invisible(noms)
}

# Retrait des accents : UNE seule definition
hstat_sans_accents <- function(x) {
  chartr("\u00e0\u00e2\u00e4\u00e3\u00e9\u00e8\u00ea\u00eb\u00ee\u00ef\u00f4\u00f6\u00f5\u00f9\u00fb\u00fc\u00e7",
         "aaaaeeeeiiooouuuc", as.character(x))
}

hstat_html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

# GRAPHIQUES INTERACTIFS : RETIRER LE POLYFILL OBSOLETE DE PLOTLY
# plotly attache une dependance « typedarray », polyfill destine aux
# navigateurs depourvus de tableaux types (IE9).
HSTAT_PLOTLY_INTERDITS <- list(bar = c("mode"), pie = c("mode"),
                               treemap = c("mode"), heatmap = c("mode"))

.hstat_plotly_attrs <- function(p) {
  d <- p$x$data
  if (!length(d)) return(p)
  p$x$data <- lapply(d, function(tr) {
    ty <- tr$type
    if (is.null(ty) || !nzchar(ty) || is.null(HSTAT_PLOTLY_INTERDITS[[ty]]))
      return(tr)
    for (a in HSTAT_PLOTLY_INTERDITS[[ty]]) tr[[a]] <- NULL
    tr
  })
  p
}

# `geom_violin(drop = FALSE)` n'est connu que depuis ggplot2 3.5.0 : le passer
# a une version anterieure fait avertir a chaque trace.
hstat_violin_drop <- function() {
  ok <- tryCatch("drop" %in% ggplot2::StatYdensity$parameters(TRUE),
                 error = function(e) FALSE)
  if (isTRUE(ok)) list(drop = FALSE) else list()
}

HSTAT_PLOTLY_LAYOUT_TOLERES <- "don't have these attributes: '(boxmode|violinmode)'(\\n|$)"

hstat_plotly_clean <- function(p) {
  if (is.null(p)) return(p)
  p <- tryCatch(.hstat_plotly_attrs(p), error = function(e) p)
  # `boxmode` est un attribut de mise en page propre aux boites.
  b <- tryCatch(withCallingHandlers(
    plotly::plotly_build(p),
    warning = function(w) {
      if (grepl(HSTAT_PLOTLY_LAYOUT_TOLERES, conditionMessage(w)))
        invokeRestart("muffleWarning")
    }), error = function(e) NULL)
  if (is.null(b)) return(p)
  if (!is.null(b$dependencies))
    b$dependencies <- Filter(function(d) !identical(d$name, "typedarray"),
                             b$dependencies)
  tryCatch(.hstat_plotly_alleger(b), error = function(e) b)
}

# -- Un nuage interactif ne porte pas un million de points --------------------
# La conversion et l'envoi sont LINEAIRES en nombre de points.
HSTAT_PLOTLY_POINTS_MAX <- 20000L

.hstat_plotly_trace_points <- function(tr) {
  ty <- tr$type %||% "scatter"
  if (!ty %in% c("scatter", "scattergl")) return(FALSE)
  mo <- tr$mode %||% "markers"
  grepl("markers", mo, fixed = TRUE) && !grepl("lines", mo, fixed = TRUE)
}

.hstat_plotly_sous <- function(v, idx, n) {
  if (is.atomic(v) && length(v) == n) return(v[idx])
  if (is.list(v) && !is.data.frame(v) && length(v) == n &&
      is.null(names(v))) return(v[idx])
  if (is.list(v) && !is.null(names(v)))
    for (k in names(v)) if (!is.null(v[[k]]))
      v[[k]] <- .hstat_plotly_sous(v[[k]], idx, n)
  v
}

.hstat_plotly_alleger <- function(b, max_points = HSTAT_PLOTLY_POINTS_MAX) {
  tr <- b$x$data
  if (!length(tr)) return(b)
  pts <- vapply(tr, .hstat_plotly_trace_points, logical(1))
  n <- vapply(tr, function(t) length(t$x %||% t$y), numeric(1))
  total <- sum(n[pts])
  if (!any(pts) || total <= max_points) return(b)
  # Chaque trace garde sa part : un groupe rare ne doit pas disparaitre au
  # profit d'un groupe abondant.
  garde <- 0
  for (i in which(pts)) {
    cible <- max(1L, floor(n[i] * max_points / total))
    if (n[i] <= cible) { garde <- garde + n[i]; next }
    idx <- unique(round(seq(1, n[i], length.out = cible)))
    nom <- names(tr[[i]])
    for (k in nom) if (!k %in% c("type", "mode", "name", "legendgroup",
                                 "showlegend", "xaxis", "yaxis"))
      tr[[i]][[k]] <- .hstat_plotly_sous(tr[[i]][[k]], idx, n[i])
    garde <- garde + length(idx)
  }
  b$x$data <- tr
  note <- trf("Affichage interactif allégé : %s points sur %s. Les calculs et l'export d'image portent sur toutes les observations.",
              format(garde, big.mark = " "), format(total, big.mark = " "))
  b$x$layout$annotations <- c(b$x$layout$annotations, list(list(
    text = hstat_html_escape(note), showarrow = FALSE, xref = "paper",
    yref = "paper", x = 0, y = 1, xanchor = "left", yanchor = "bottom",
    font = list(size = 11, color = "#5f6b7a"))))
  attr(b, "hstat_allege") <- c(garde = garde, total = total)
  b
}

# PAQUET ABSENT : LE DIRE, ET PROPOSER UNE VOIE DE REPLI
# « Package 'klaR' indisponible.

HSTAT_PKG_REPLI <- list(
  klaR = paste("En attendant, une ACM sur les mêmes variables suivie d'une CAH",
               "sur les coordonnées factorielles donne une classification",
               "d'individus décrits par des variables qualitatives. Les deux",
               "analyses sont disponibles dans cet onglet."),
  poLCA = paste("En attendant, une ACM suivie d'une CAH dégage des profils",
                "comparables. Elle ne fournit pas les probabilités",
                "d'appartenance ni les critères AIC/BIC, mais elle répond à la",
                "même question : quels groupes d'individus se ressemblent ?"),
  clustMixType = paste("En attendant, une AFDM (analyse factorielle de données",
                       "mixtes) suivie d'une CAH traite également un mélange de",
                       "variables numériques et qualitatives. Les deux analyses",
                       "sont disponibles dans cet onglet."))

hstat_pkg_manquant <- function(pkg, analyse = NULL) {
  trf("%s%scette analyse ne peut donc pas être lancée. %sHStat. %s", if (!is.null(analyse)) paste0(analyse, " : ") else "", trf("le paquet R « %s » n'est pas installé sur cette machine, ",      pkg), sprintf("Pour l'ajouter : install.packages(\"%s\"), puis relancez ",      pkg), HSTAT_PKG_REPLI[[pkg]] %||% "")
}

# QUALITE D'UNE PARTITION Ces fonctions vivent ici, et non dans l'observateur
# qui les appelle, pour une raison precise.

# Pseudo-R2 d'une partition k-modes : part de la dissimilarite totale expliquee
# par la partition.
hstat_kmodes_pseudo_r2 <- function(withindiff, data) {
  d_tot <- sum(vapply(as.data.frame(data), function(col) {
    tb <- table(col)
    if (!length(tb)) 0 else sum(tb) - max(tb)
  }, numeric(1)))
  tot <- sum(withindiff)
  r2 <- if (is.finite(d_tot) && d_tot > 0) 1 - tot / d_tot else NA_real_
  list(dissimilarite_intra = tot, dissimilarite_totale = d_tot,
       pseudo_r2 = r2, verdict = hstat_seuil_verdict(r2, 0.50, 0.30))
}

# Entropie relative d'une classification latente : 1 = chaque individu affecte
# sans ambiguite, 0 = affectations indiscernables.
hstat_lca_entropie <- function(posterior) {
  p <- as.matrix(posterior)
  if (!nrow(p) || ncol(p) < 2)
    return(list(entropie = NA_real_, entropie_relative = NA_real_,
                verdict = "indeterminable"))
  # Le plancher evite log(0) sans deplacer l'entropie de facon sensible.
  ent <- -sum(p * log(p + 1e-12))
  rel <- 1 - ent / (nrow(p) * log(ncol(p)))
  list(entropie = ent, entropie_relative = rel,
       verdict = hstat_seuil_verdict(rel, 0.80, 0.60))
}

# Equilibre des effectifs : une classe a 1 % n'est pas interpretable, quelle
# que soit la qualite du reste.
hstat_part_equilibre <- function(sizes, seuil = 0.05) {
  # Les modules fournissent tantot des effectifs (`km$size`), tantot une
  # `table()`.
  if (is.factor(sizes) || is.character(sizes))
    sizes <- suppressWarnings(as.numeric(as.character(sizes)))
  sizes <- suppressWarnings(as.numeric(sizes))
  sizes <- sizes[is.finite(sizes)]
  n <- sum(sizes)
  if (!length(sizes) || !is.finite(n) || n <= 0)
    return(list(part_min = NA_real_, verdict = "indeterminable"))
  part <- min(sizes) / n
  list(part_min = part, effectifs = as.numeric(sizes),
       verdict = if (!is.finite(part)) "indeterminable"
                 else if (part >= seuil) "ok" else "warn")
}

# Pastille de condition d'une analyse multivariee : verte, orange ou rouge.
.mv_badge <- function(ok, warn, label) {
  st <- function(fond) paste0("display:inline-block;background:", fond,
                              ";color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;")
  if (ok)        shiny::div(style = st("#27ae60"), shiny::icon("check"), label)
  else if (warn) shiny::div(style = st("#f39c12"), shiny::icon("exclamation-triangle"), label)
  else           shiny::div(style = st("#e74c3c"), shiny::icon("times-circle"), label)
}

# Verdict a trois niveaux sur une statistique bornee.
hstat_seuil_verdict <- function(x, seuil_ok, seuil_warn) {
  if (length(x) != 1L || !is.finite(x)) return("indeterminable")
  if (x >= seuil_ok) "ok" else if (x >= seuil_warn) "warn" else "err"
}

# TRADUCTION DES ERREURS R
# L'interface est en francais ; les erreurs de R, non.
# 1. Chaque traduction enonce la CAUSE puis le GESTE.
# 2. Le message R d'origine n'est jamais supprime, il est mis entre
#    parentheses.

HSTAT_ERR_FR <- list(
  list("data are essentially constant|essentiellement constant",
       paste("La variable ne varie pas : toutes ses valeurs sont identiques (ou",
             "presque). Aucun test ne peut comparer ce qui ne varie pas.",
             "Choisissez une autre variable, ou vérifiez que le filtre actif",
             "n'a pas réduit vos données à un seul cas de figure.")),
  list("all 'x' values are identical|values are identical",
       paste("Toutes les observations portent la même valeur. Le test n'a rien",
             "à comparer. Vérifiez la variable choisie et les filtres actifs.")),
  list("sample size must be between 3 and 5000",
       paste("Le test de Shapiro-Wilk exige entre 3 et 5000 observations.",
             "Au-delà, utilisez un graphique quantile-quantile plutôt qu'un",
             "test : sur de tels effectifs, il rejetterait le moindre écart.")),
  list("not enough .?(x|y|finite)?.? observations|not enough observations",
       paste("Effectif insuffisant pour ce test. Vérifiez le nombre",
             "d'observations non manquantes dans chaque groupe : un groupe vide",
             "ou réduit à une seule observation suffit à bloquer le calcul.")),
  list("grouping factor must have exactly 2 levels",
       paste("Ce test compare exactement deux groupes, or le facteur choisi",
             "n'en distingue pas deux. Pour plus de deux groupes, utilisez",
             "l'ANOVA — ou Kruskal-Wallis si la normalité n'est pas acquise.")),
  list("not enough 'x' observations|need at least 2 groups|at least two groups",
       paste("Il faut au moins deux groupes comportant des observations, et",
             "l'un d'eux est vide après retrait des valeurs manquantes.",
             "Vérifiez les effectifs par groupe, et les filtres actifs.")),
  list("contrasts can be applied only to factors with 2 or more levels",
       paste("Une des variables explicatives ne prend qu'une seule modalité",
             "dans les données analysées : elle n'apporte aucune information au",
             "modèle. Retirez-la, ou vérifiez les filtres actifs.")),
  list("incorrect number of dimensions",
       paste("Le résultat ne comporte qu'un seul axe factoriel : il ne peut pas",
             "être représente dans un plan. C'est le cas courant d'une AFC",
             "croisant une variable binaire (sexe, oui/non, avant/après).",
             "Croisez des variables comportant davantage de modalités.")),
  list("exactly singular|computationally singular|singular matrix|matrice singuli",
       paste("La matrice n'est pas inversible : au moins deux variables sont",
             "redondantes (l'une se déduit des autres), ou il y a moins",
             "d'observations que de variables. Retirez une des variables",
             "corrélées, ou augmentez l'effectif.")),
  list("non-conformable arg",
       paste("Les dimensions des tableaux combines ne correspondent pas.",
             "Vérifiez que toutes les variables retenues portent bien sur les",
             "mêmes observations.")),
  list("missing value where TRUE/FALSE needed",
       paste("Une statistique n'a pas pu être calculée (elle vaut NA) et une",
             "décision en dépendait. C'est le signe de données dégénérées :",
             "vérifiez la variance et l'effectif de chaque groupe, ainsi que",
             "le taux de valeurs manquantes.")),
  list("0 \\(non-NA\\) cases|no complète élément|complète\\.cases",
       paste("Aucune observation ne renseigne toutes les variables choisies à",
             "la fois. Retirez la variable la plus lacunaire, ou traitez les",
             "valeurs manquantes dans l'onglet Nettoyage.")),
  list("NA/NaN/Inf in foreign function call|infinité or missing values",
       paste("Les données contiennent des valeurs manquantes ou infinies que ce",
             "calcul n'accepte pas. Traitez-les dans l'onglet Nettoyage",
             "(imputation ou retrait) avant de relancer.")),
  list("undefined columns selected|subscript out of bounds",
       paste("Une variable attendue est absente du jeu de données. Elle a sans",
             "doute été renommée ou retirée depuis le choix : resélectionnez",
             "vos variables.")),
  list("there is no package called",
       paste("Un paquet R nécessaire à cette analyse n'est pas installé.",
             "Installez-le, puis relancez l'application ; cette analyse restera",
             "indisponible en attendant, les autres continuent de fonctionner.")),
  list("could not find function",
       paste("Une fonction attendue est introuvable : le paquet qui la fournit",
             "n'est pas installé ou n'a pas pu être charge. Installez-le, puis",
             "relancez l'application.")),
  list("cannot open file|No such file or directory|impossible d'ouvrir",
       paste("Le fichier n'a pas pu être ouvert. Vérifiez le chemin, que le",
             "fichier n'a pas été déplacé, et vos droits d'accès.")),
  list("arguments imply differing number of rows|replacement has .* rows",
       paste("Les colonnes assemblées n'ont pas le même nombre de lignes.",
             "Vérifiez que les jeux de données fusionnes portent bien sur les",
             "mêmes observations.")),
  list("approximation may be incorrect|approximation incorrecte",
       paste("Certains effectifs théoriques sont inférieurs à 5 :",
             "l'approximation du khi-deux devient douteuse. Utilisez le test",
             "exact de Fisher, ou regroupez les modalités les moins",
             "frequentes.")),
  list("figure margins too large",
       paste("La zone de trace est trop petite pour le graphique demandé.",
             "Agrandissez la fenêtre, ou réduisez la taille des étiquettes.")),
  list("argument \"name\" is missing",
       paste("Erreur interne d'affichage. Signalez-la : elle vient du code de",
             "l'application, pas de vos données.")),
  list("must be numeric|not numeric|doit être numérique",
       paste("Ce calcul attend une variable numérique et a reçu du texte ou une",
             "catégorie. Convertissez la variable dans l'onglet Nettoyage, ou",
             "choisissez une variable numérique.")),
  list("system is exactly singular|did not converge|ne converge pas",
       paste("Le modèle n'a pas convergé. Les groupes sont probablement",
             "parfaitement sépares, ou l'effectif est trop faible pour le",
             "nombre de paramètres estimes. Simplifiez le modèle.")))

# Traduit une erreur R en francais actionnable.
hstat_langue_session <- function() {
  d <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (is.null(d) || is.null(d$userData)) return("fr")
  rv <- tryCatch(d$userData$langue_rv, error = function(e) NULL)
  if (is.function(rv) &&
      !is.null(tryCatch(shiny::getCurrentOutputInfo(d), error = function(e) NULL)))
    tryCatch(rv(), error = function(e) NULL)
  l <- tryCatch(d$userData$langue, error = function(e) NULL)
  if (identical(l, "en")) "en" else "fr"
}

# Separateur decimal des fichiers telecharges
# Un utilisateur francophone colle un CSV dans un tableur regle a la francaise
# et lit « 1.5 » comme une date ou du texte.

.hstat_decimale <- function(dec) {
  if (identical(as.character(dec)[1], ",")) "," else "."
}

hstat_decimale_session <- function() {
  d <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (is.null(d) || is.null(d$userData)) return(".")
  .hstat_decimale(tryCatch(d$userData$decimale, error = function(e) NULL))
}

# Un nombre ECRIT dans un texte : « 2.073 ± 0.658 », « RR=1.23 », « p < 0.001
# ».
HSTAT_DECIMALE_MOTIF <- "(?<![\\p{L}\\p{N}_.])(\\d+)\\.(\\d+)(?!\\d|\\.\\d)"

hstat_decimale_texte <- function(x, dec = hstat_decimale_session()) {
  if (!identical(.hstat_decimale(dec), ",") || !length(x)) return(x)
  if (is.factor(x)) {
    lv <- hstat_decimale_texte(levels(x), dec)
    # Deux niveaux qui deviendraient identiques (« 0.5 » et « 0,5 ») seraient
    # FUSIONNES par `levels<-` : une modalite disparaitrait du fichier.
    if (!anyDuplicated(lv)) levels(x) <- lv
    return(x)
  }
  if (!is.character(x)) return(x)
  x[] <- gsub(HSTAT_DECIMALE_MOTIF, "\\1,\\2", x, perl = TRUE)
  x
}

# Les colonnes de TEXTE d'un tableau.
hstat_decimale_tableau <- function(x, dec = hstat_decimale_session()) {
  if (!identical(.hstat_decimale(dec), ",") || is.null(x)) return(x)
  if (is.matrix(x) && is.character(x)) return(hstat_decimale_texte(x, dec))
  if (!is.data.frame(x)) return(x)
  for (j in seq_along(x)) x[[j]] <- hstat_decimale_texte(x[[j]], dec)
  x
}

# Un CLASSEUR Excel avec la virgule : les nombres y sont ECRITS avec elle.
hstat_decimale_classeur <- function(x, dec = hstat_decimale_session()) {
  if (!identical(.hstat_decimale(dec), ",") || is.null(x)) return(x)
  if (is.matrix(x) && !is.character(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
  x <- hstat_decimale_tableau(x, dec)
  if (!is.data.frame(x)) return(x)
  for (j in seq_along(x)) {
    v <- x[[j]]
    if (is.numeric(v) && !inherits(v, c("Date", "POSIXt", "difftime"))) {
      s <- as.character(v)
      s[!is.finite(v)] <- NA_character_
      x[[j]] <- sub(".", ",", s, fixed = TRUE)
    }
  }
  x
}

# LE SEUL ecrivain de CSV.
hstat_ecrire_csv <- function(x, file, row.names = FALSE, fileEncoding = "UTF-8",
                             dec = hstat_decimale_session()) {
  dec <- .hstat_decimale(dec)
  if (!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
  x <- hstat_decimale_tableau(x, dec)
  # `col.names = NA` quand les noms de lignes sont ecrits.
  cn <- if (isTRUE(row.names) || is.character(row.names)) NA else TRUE
  args <- list(x, file, sep = if (dec == ",") ";" else ",", dec = dec,
               qmethod = "double", row.names = row.names, col.names = cn)
  # `fileEncoding` ne vaut que pour un NOM de fichier ; une connexion porte
  # deja le sien.
  if (is.character(file)) args$fileEncoding <- fileEncoding
  do.call(utils::write.table, args)
}

# Ce que les figures ecrivent : les graduations, les valeurs portees, les
# etiquettes, les equations.
.hstat_decimale_expr <- function(e) {
  if (is.character(e)) return(hstat_decimale_texte(e, ","))
  if (is.numeric(e) && length(e) == 1L && is.finite(e) && e != round(e))
    return(format(e, decimal.mark = ","))
  if (is.expression(e))
    return(as.expression(lapply(e, .hstat_decimale_expr)))
  if (is.call(e) && length(e) > 1L)
    for (i in seq.int(2L, length(e))) {
      a <- e[[i]]
      if (!missing(a) && !is.null(a)) e[[i]] <- .hstat_decimale_expr(a)
    }
  e
}

.hstat_decimale_grob_texte <- function(g) {
  if (is.null(g) || !is.list(g)) return(g)
  for (champ in c("label", "lab")) {
    v <- g[[champ]]
    if (is.null(v)) next
    if (is.character(v) || is.factor(v)) g[[champ]] <- hstat_decimale_texte(v, ",")
    else if (is.expression(v) || is.call(v)) g[[champ]] <- .hstat_decimale_expr(v)
    else if (is.list(v)) g[[champ]] <- lapply(v, .hstat_decimale_expr)
  }
  if (is.list(g$grobs)) g$grobs <- lapply(g$grobs, .hstat_decimale_grob_texte)
  if (length(g$children)) {
    cl <- class(g$children)
    g$children[] <- lapply(g$children, .hstat_decimale_grob_texte)
    class(g$children) <- cl
  }
  # CERTAINES GRILLES N'ECRIVENT LEUR TEXTE QU'AU TRACE.
  if (inherits(g, "grob") && !inherits(g, "hstat_decimale_differee") &&
      !is.null(utils::getS3method("makeContent", class(g)[1], optional = TRUE,
                                   envir = asNamespace("grid"))))
    class(g) <- c("hstat_decimale_differee", class(g))
  g
}

# Enregistree aupres de grid au moment de l'emploi.
.hstat_decimale_differee_contenu <- function(x) {
  class(x) <- setdiff(class(x), "hstat_decimale_differee")
  .hstat_decimale_grob_texte(grid::makeContent(x))
}

# La grille d'une figure, texte converti.
.hstat_decimale_grob <- function(plot) {
  g <- if (inherits(plot, "patchwork") && requireNamespace("patchwork", quietly = TRUE))
         patchwork::patchworkGrob(plot)
       else if (inherits(plot, "ggplot")) ggplot2::ggplotGrob(plot)
       else if (inherits(plot, "grob")) plot
       else return(NULL)
  registerS3method("makeContent", "hstat_decimale_differee",
                   .hstat_decimale_differee_contenu, envir = asNamespace("grid"))
  .hstat_decimale_grob_texte(g)
}

# LA REPONSE DU MODELE EST DU TEXTE AFFICHE, ET AUCUN DICTIONNAIRE NE PEUT LA
# TRADUIRE.
hstat_ai_consigne_langue <- function(lang = hstat_langue_session()) {
  if (identical(lang, "en")) "in English" else "en fran\u00e7ais"
}

# `lang` : ces messages sont COMPOSES ici, phrase par phrase, et n'existent
# donc pas comme chaine entiere dans le dictionnaire du navigateur.
hstat_err_fr <- function(e, contexte = NULL, lang = hstat_langue_session()) {
  msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)[1]
  msg <- trimws(paste(msg, collapse = " "))
  if (!length(msg) || !nzchar(msg)) msg <- tr("erreur sans message", lang)
  # Le francais met une espace avant les deux-points, l'anglais non.
  dp <- if (identical(lang, "en")) ": " else " : "
  prefixe <- if (!is.null(contexte) && nzchar(contexte))
    paste0(contexte, dp) else ""
  # UN REFUS DE L'APPLICATION EST DEJA REDIGE.
  if (inherits(e, "hstat_refus")) return(paste0(prefixe, msg))
  for (r in HSTAT_ERR_FR) {
    if (grepl(r[[1]], msg, ignore.case = TRUE, perl = TRUE))
      # Le message d'origine reste entre parentheses : c'est ce qu'un
      # utilisateur copiera pour demander de l'aide.
      return(sprintf("%s%s (%s%s%s)", prefixe, tr(r[[2]], lang),
                     tr("message R", lang), dp, msg))
  }
  # Rien de connu : on ne masque pas, on annonce.
  sprintf("%s%s%s%s", prefixe,
          tr("L'analyse a échoué. Message renvoyé par R (non traduit)", lang),
          dp, msg)
}

# Erreurs HTTP des services d'IA : la cause, puis le geste.
hstat_ai_err_http <- function(code, message = "", label = "l'API") {
  code <- suppressWarnings(as.integer(code[1]))
  msg  <- if (length(message)) as.character(message)[1] else ""
  if (is.na(msg)) msg <- ""
  bas  <- tolower(msg)
  a <- function(motifs)
    any(vapply(motifs, function(m) grepl(m, bas, fixed = TRUE), logical(1)))

  cause <-
    if (a(c("credit balance", "insufficient_quota", "insufficient quota",
            "exceeded your current quota", "purchase credits", "billing")))
      "credit"
    else if (a(c("invalid x-api-key", "invalid api key", "incorrect api key",
                 "api key not valid", "invalid_api_key", "authentication_error")))
      "cle"
    else if (a(c("permission", "does not have access", "not allowed")))
      "droit"
    else if (a(c("model not found", "unknown model", "is not supported",
                 "does not exist")))
      "modele"
    else if (a(c("rate limit", "too many requests", "overloaded")))
      "cadence"
    else if (a(c("too long", "maximum context", "exceeds the maximum",
                 "request too large", "too large")))
      "taille"
    else if (isTRUE(code == 401L)) "cle"
    else if (isTRUE(code == 402L)) "credit"
    else if (isTRUE(code == 403L)) "droit"
    else if (isTRUE(code == 404L)) "modele"
    else if (isTRUE(code == 413L)) "taille"
    else if (isTRUE(code == 429L)) "cadence"
    else if (isTRUE(!is.na(code) && code >= 500L)) "panne"
    else "inconnue"

  phrase <- switch(
    cause,
    "credit"  = trf("Le compte d'API de %s n'a plus de crédit : la clé est valide, c'est la facturation qui bloque. Un abonnement grand public (Claude Pro ou Max, ChatGPT Plus...) ne donne aucun crédit d'API — l'API se paie à part, par avance, sur la console du fournisseur.", label),
    "cle"     = trf("%s refuse la clé d'API. Vérifiez qu'elle est recopiée en entier, sans espace ni retour à la ligne, et qu'elle appartient bien à ce service.", label),
    "droit"   = trf("La clé est acceptée par %s mais n'a pas le droit d'appeler ce modèle. Vérifiez les autorisations du compte, ou choisissez un autre modèle.", label),
    "modele"  = trf("%s ne connaît pas le modèle demandé. Corrigez son nom dans le champ « Modèle » des réglages du moteur.", label),
    "cadence" = trf("%s refuse la requête pour cause de cadence : trop d'appels en peu de temps, ou service momentanément saturé. Réessayez dans un instant.", label),
    "taille"  = trf("La requête est trop longue pour %s. Réduisez le nombre de résultats interprétés, ou choisissez un modèle à plus grande fenêtre.", label),
    "panne"   = trf("%s est en panne ou en maintenance. Réessayez plus tard ; l'analyse locale reste disponible.", label),
    trf("%s a refusé la requête.", label))

  # L'assemblage ne passe PAS par `trf()`.
  if (!nzchar(msg))
    return(if (is.na(code)) phrase else sprintf("%s (HTTP %d)", phrase, code))
  if (is.na(code)) return(sprintf("%s (%s)", phrase, substr(msg, 1, 300)))
  sprintf("%s (HTTP %d \u2014 %s)", phrase, code, substr(msg, 1, 300))
}

# FactoMineR reduit ses coordonnees a un VECTEUR des que le resultat ne
# comporte qu'un seul axe.
hstat_coord_mat <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.null(dim(x)))
    return(matrix(x, ncol = 1L, dimnames = list(names(x), "Dim 1")))
  as.matrix(x)
}

# UN POINT QUI DISPARAIT SE NOMME.
hstat_coord_incompletes <- function(coord, axes = c(1L, 2L)) {
  m <- hstat_coord_mat(coord)
  if (is.null(m) || !NROW(m) || !NCOL(m)) return(character(0))
  ax <- unique(as.integer(axes))
  ax <- ax[!is.na(ax) & ax >= 1L & ax <= NCOL(m)]
  if (!length(ax)) return(character(0))
  sub <- m[, ax, drop = FALSE]
  mauvais <- apply(sub, 1L, function(r) any(!is.finite(r)))
  noms <- rownames(m)
  if (is.null(noms)) noms <- as.character(seq_len(NROW(m)))
  noms[mauvais]
}

# ACP : QUI PORTE LA COULEUR SUR UN BIPLOT, ET CE QU'UNE VARIABLE N'APPREND PAS

# UN BIPLOT PORTE DEUX FAMILLES, UNE SEULE PEUT PORTER LE DEGRADE.
HSTAT_PCA_COLOR_CIBLE <- c(contrib = "ind", cos2 = "var", sat = "var")

# Cible d'une metrique.
hstat_pca_cible_couleur <- function(metrique = "contrib") {
  id <- as.character(metrique)[1]
  if (is.na(id) || !nzchar(id)) return("ind")
  cible <- HSTAT_PCA_COLOR_CIBLE[id]
  if (is.na(cible)) "ind" else unname(cible)
}

# QUALITE DE REPRESENTATION D'UNE VARIABLE SUR UN SOUS-ESPACE.
# 1. LE SOUS-ESPACE FAIT PARTIE DU CHIFFRE.
# 2. UNE QUALITE INCALCULABLE RESTE `NA`, JAMAIS ZERO.
# 3. LE TRI MET LES INCALCULABLES EN TETE.
hstat_pca_qualite_var <- function(res, axes = c(1L, 2L)) {
  vide <- data.frame(Variable = character(0), Qualite = numeric(0),
                     Qualite_pct = numeric(0), Contribution = numeric(0),
                     stringsAsFactors = FALSE)
  cos2 <- hstat_coord_mat(res$var$cos2)
  if (is.null(cos2) || !NROW(cos2) || !NCOL(cos2)) {
    attr(vide, "axes") <- integer(0)
    return(vide)
  }
  ax <- unique(as.integer(axes))
  ax <- ax[!is.na(ax) & ax >= 1L & ax <= NCOL(cos2)]
  if (!length(ax)) ax <- seq_len(NCOL(cos2))

  noms <- rownames(cos2)
  if (is.null(noms)) noms <- as.character(seq_len(NROW(cos2)))

  .somme <- function(m) {
    apply(m[, ax, drop = FALSE], 1L, function(r) {
      fini <- is.finite(r)
      if (!any(fini)) NA_real_ else sum(r[fini])
    })
  }
  qual <- .somme(cos2)

  contrib <- hstat_coord_mat(res$var$contrib)
  ctr <- if (is.null(contrib) || NCOL(contrib) < max(ax)) rep(NA_real_, length(qual))
         else .somme(contrib)

  out <- data.frame(Variable = noms, Qualite = unname(qual),
                    Qualite_pct = unname(round(100 * qual, 1)),
                    Contribution = unname(ctr),
                    stringsAsFactors = FALSE)
  out <- out[order(out$Qualite, na.last = FALSE), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "axes") <- ax
  out
}

# Variables dont la qualite tombe sous le seuil, dans l'ordre du tableau (les
# moins informatives d'abord).
hstat_pca_var_faibles <- function(qual, seuil = 0.30) {
  if (!NROW(qual)) return(character(0))
  s <- suppressWarnings(as.numeric(seuil)[1])
  if (!is.finite(s)) s <- 0.30
  garde <- is.na(qual$Qualite) | qual$Qualite < s
  as.character(qual$Variable[garde])
}

# Verdict a TROIS etats sur une p-value : significatif / non significatif /
# indeterminable.
hstat_p_verdict <- function(p, alpha = 0.05) {
  p <- suppressWarnings(as.numeric(p)[1])
  if (!is.finite(p)) return("indeterminable")
  if (p < alpha) "significatif" else "non significatif"
}

# Une seule écriture des p-values dans toute l'application.
hstat_p_txt <- function(p, digits = 4) {
  p <- suppressWarnings(as.numeric(p))
  vapply(p, function(x) {
    if (!is.finite(x)) return(NA_character_)
    if (x < 2.2e-16) return("< 2.2e-16")
    if (x < 1e-4) return(formatC(x, format = "e", digits = 2))
    formatC(x, format = "f", digits = digits)
  }, character(1))
}

# Une seule échelle de significativité, et traduite.
hstat_p_niveau <- function(p) {
  p <- suppressWarnings(as.numeric(p)[1])
  if (!is.finite(p)) return(tr("Non calculable"))
  if (p < 0.001) return(tr("Hautement significatif (p < 0.001)"))
  if (p < 0.01)  return(tr("Très significatif (p < 0.01)"))
  if (p < 0.05)  return(tr("Significatif (p < 0.05)"))
  tr("Non significatif (p >= 0.05)")
}

interpret_test_results <- function(test_type, p_value, test_object = NULL) {
  if (is.na(p_value)) return(tr("Résultat non disponible", hstat_langue_session()))
  # `trf()` ne traduit JAMAIS ses arguments -- c'est ce qui protège les données
  # de l'utilisateur.
  significance <- tr(if (p_value < 0.05) "significative" else "non significative",
                     hstat_langue_session())
  switch(test_type,
         "t.test"           = trf("Le test t montre une différence %s entre les groupes (p = %s)", significance, round(p_value, 8)),
         "wilcox.test"      = trf("Le test de Wilcoxon montre une différence %s entre les groupes (p = %s)", significance, round(p_value, 8)),
         "anova"            = trf("L'ANOVA montre une différence %s entre les groupes (p = %s)", significance, round(p_value, 8)),
         "friedman"         = trf("Le test de Friedman montre une différence %s entre les périodes (p = %s)", significance, round(p_value, 8)),
         "durbin"           = trf("Le test de Durbin montre une différence %s entre les périodes (p = %s)", significance, round(p_value, 8)),
         "kruskal.test"     = trf("Le test de Kruskal-Wallis montre une différence %s entre les groupes (p = %s)", significance, round(p_value, 8)),
         "scheirerRayHare"  = trf("Le test de Scheirer-Ray-Hare montre une différence %s entre les groupes (p = %s)", significance, round(p_value, 8)),
         "manova"           = trf("La MANOVA montre une différence multivariée %s entre les groupes (p = %s)", significance, round(p_value, 8)),
         "permanova"        = trf("La PERMANOVA montre une différence multivariée %s entre les groupes (p = %s)", significance, round(p_value, 8)),
         "chisq.test"       = trf("Le test du chi² montre une association %s entre les variables (p = %s)", significance, round(p_value, 8)),
         "cor.test"         = trf("La corrélation est %s (p = %s)", significance, round(p_value, 8)),
         trf("Le test %s montre un résultat %s (p = %s)", test_type, significance, round(p_value, 8))
  )
}

# AJUSTEMENT ROBUSTE D'UN GLM (non-convergence / séparation)
# glm.fit() émet « l'algorithme n'a pas convergé » dans deux situations : - le
# budget d'itérations par défaut (25) est trop court.

# Diagnostic textuel d'un objet glm : NULL si tout va bien, sinon un message
# expliquant la cause probable et la conduite à tenir.
hstat_glm_note <- function(fit, warns = character(0), maxit = 100) {
  if (is.null(fit)) return(NULL)
  converged <- isTRUE(fit$converged)
  fv <- tryCatch(stats::fitted(fit), error = function(e) numeric(0))
  binom <- tryCatch(identical(fit$family$family, "binomial"),
                    error = function(e) FALSE)
  # Probabilités ajustées collées à 0 ou 1 : signature de la séparation.
  extreme <- binom && length(fv) > 0 &&
    any(fv < 1e-8 | fv > 1 - 1e-8, na.rm = TRUE)
  co <- tryCatch(stats::coef(fit), error = function(e) numeric(0))
  huge <- length(co) > 0 && any(abs(co) > 30, na.rm = TRUE)
  if (converged && !extreme && !huge && length(warns) == 0) return(NULL)
  msg <- character(0)
  if (!converged)
    msg <- c(msg, trf("L'estimation n'a pas convergé en %d itérations.", maxit))
  if (extreme || huge)
    msg <- c(msg, paste0(
      "Séparation (quasi-)complète détectée : un ou plusieurs prédicteurs ",
      "séparent parfaitement les modalités de la réponse. Les coefficients et ",
      "les rapports de cotes divergent ; leurs écarts-types et p-values ne sont ",
      "pas interprétables."))
  if (length(msg) == 0) msg <- unique(warns)
  msg <- c(msg, paste0(
    "Pistes : retirer ou regrouper le prédicteur responsable, fusionner les ",
    "modalités à faible effectif, ou passer à une régression pénalisée ",
    "(Ridge / Lasso), stable dans ce cas."))
  paste(msg, collapse = " ")
}

# TAILLE DES LABELS EN POINTS TYPOGRAPHIQUES (analyses multivariées)
# Toutes les analyses multivariées exposent deux réglages de taille de label,
# bornés à [12, 24] pt.
HSTAT_GG_POINT_SIZE <- 1.5    # geom_point(size = )
HSTAT_GG_LINEWIDTH  <- 0.5    # geom_line/segment(linewidth = )
HSTAT_GG_BASE_SIZE  <- 11     # theme_grey(base_size = ), en points
HSTAT_GG_LABEL_PT   <- 11     # geom_text(size = 3.88 mm) ~ 11 pt

# Le minimum descend SOUS la taille de ggplot2.
HSTAT_LBL_PT_MIN     <- 8
HSTAT_LBL_PT_MAX     <- 24
HSTAT_LBL_PT_DEFAULT <- HSTAT_GG_LABEL_PT
.HSTAT_PT_PER_MM     <- 72.27 / 25.4   # identique à ggplot2::.pt

# Borne une taille saisie à l'intervalle autorisé, en retombant sur la valeur
# par défaut si l'entrée est absente ou invalide.
hstat_lbl_pt <- function(pt, default = HSTAT_LBL_PT_DEFAULT) {
  v <- suppressWarnings(as.numeric(pt))[1]
  if (is.na(v) || !is.finite(v)) v <- default
  min(max(v, HSTAT_LBL_PT_MIN), HSTAT_LBL_PT_MAX)
}

# Points -> unité de taille ggplot2 (mm).
hstat_lbl_pt2gg <- function(pt, default = HSTAT_LBL_PT_DEFAULT) {
  hstat_lbl_pt(pt, default) / .HSTAT_PT_PER_MM
}

# Points -> facteur cex (graphiques base R et fviz_dend), où cex = 1 correspond
# à la police de référence de 12 pt.
hstat_lbl_pt2cex <- function(pt, default = HSTAT_LBL_PT_DEFAULT) {
  hstat_lbl_pt(pt, default) / 12
}

# Curseur standard « taille des labels », toujours gradué en points.
hstat_lbl_slider <- function(id, label, value = HSTAT_LBL_PT_DEFAULT) {
  shiny::sliderInput(id, label, min = HSTAT_LBL_PT_MIN, max = HSTAT_LBL_PT_MAX,
              value = value, step = 1, post = " pt")
}

# Impose la taille des calques de texte d'un ggplot déjà construit (factoextra
# n'expose qu'un seul argument `labelsize`, commun aux individus et aux
# variables).
hstat_noircir_etiquettes <- function(p, colour = "#1a1a1a",
                                     n_cible = NA_integer_, n_autre = NA_integer_) {
  if (is.null(p) || is.null(p$layers)) return(p)
  nc <- suppressWarnings(as.integer(n_cible))
  nc <- unique(nc[!is.na(nc) & nc > 0])
  n_autre <- suppressWarnings(as.integer(n_autre)[1])
  if (!is.na(n_autre)) nc <- nc[nc != n_autre]
  cible_seule <- length(nc) > 0

  for (i in seq_along(p$layers)) {
    if (!any(grepl("Text|Label", class(p$layers[[i]]$geom)))) next
    if (cible_seule) {
      nr <- tryCatch(
        if (is.data.frame(p$layers[[i]]$data)) nrow(p$layers[[i]]$data) else NA_integer_,
        error = function(e) NA_integer_)
      if (is.na(nr) || !(nr %in% nc)) next
    }
    p$layers[[i]]$aes_params$colour   <- colour
    p$layers[[i]]$aes_params$fontface <- "bold"
    # Neutralise un mapping de couleur eventuel sur le texte : sans cela
    # l'aes_params est ecrase au trace par le degrade mappe.
    if (!is.null(p$layers[[i]]$mapping)) p$layers[[i]]$mapping$colour <- NULL
  }
  p
}

hstat_apply_label_sizes <- function(p, size_default, size_var = NULL,
                                    n_var = NA_integer_, n_ind = NA_integer_) {
  if (is.null(p) || is.null(p$layers)) return(p)
  n_var <- suppressWarnings(as.integer(n_var))
  n_var <- unique(n_var[!is.na(n_var) & n_var > 0])
  if (!is.na(n_ind)) n_var <- n_var[n_var != n_ind]   # comptes ambigus écartés
  use_var <- !is.null(size_var) && length(n_var) > 0
  for (i in seq_along(p$layers)) {
    if (!any(grepl("Text|Label", class(p$layers[[i]]$geom)))) next
    nr <- tryCatch(
      if (is.data.frame(p$layers[[i]]$data)) nrow(p$layers[[i]]$data)
      else NA_integer_,
      error = function(e) NA_integer_)
    sz <- if (use_var && !is.na(nr) && nr %in% n_var) size_var else size_default
    if (!is.null(sz) && is.finite(sz)) p$layers[[i]]$aes_params$size <- sz
  }
  p
}

# Ajuste un glm en silence et renvoie list(fit = <glm>, note = <message|NULL>).
hstat_glm_fit <- function(formula, data, family = stats::binomial(),
                          maxit = 100, ...) {
  warns <- character(0)
  fit <- withCallingHandlers(
    stats::glm(formula, data = data, family = family,
               control = stats::glm.control(maxit = maxit), ...),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  list(fit = fit, note = hstat_glm_note(fit, warns, maxit))
}

# UN TEST QUI NE REJETTE PAS NE DEMONTRE RIEN.
interpret_normality <- function(p_value) {
  if (is.na(p_value)) return(tr("Résultat non disponible"))
  if (p_value >= 0.05) {
    trf("Normalité non rejetée (p = %s >= 0.05) : les données sont compatibles avec une loi normale.", round(p_value, 8))
  } else {
    trf("La distribution n'est pas normale (p = %s < 0.05)", round(p_value, 8))
  }
}

interpret_homogeneity <- function(p_value) {
  if (is.na(p_value)) return(tr("Résultat non disponible"))
  if (p_value >= 0.05) {
    trf("Homogénéité des variances non rejetée (p = %s >= 0.05) : les variances sont compatibles entre groupes.", round(p_value, 8))
  } else {
    trf("Les variances ne sont pas homogènes (p = %s < 0.05)", round(p_value, 8))
  }
}

interpret_normality_resid <- function(p_value) {
  if (is.na(p_value)) return(tr("Test non applicable"))
  if (p_value > 0.05) {
    tr("Normalité des résidus non rejetée (p > 0.05) : les données sont compatibles avec les conditions des tests paramétriques.")
  } else {
    tr("Les résidus ne suivent pas une distribution normale (p < 0.05). Considérez l'utilisation de tests non-paramétriques.")
  }
}

interpret_homogeneity_resid <- function(p_value) {
  if (is.na(p_value)) return(tr("Test non applicable"))
  if (p_value > 0.05) {
    tr("Homogénéité des variances des résidus non rejetée (p > 0.05) : les données sont compatibles avec les conditions des tests paramétriques.")
  } else {
    tr("Les variances ne sont pas homogènes (p < 0.05). Utilisez des tests robustes à l'hétérogénéité des variances.")
  }
}

filter_complete_cross <- function(df, A, B, reqA = TRUE, reqB = FALSE) {
  df <- df[, , drop = FALSE]
  if (!is.factor(df[[A]])) df[[A]] <- factor(df[[A]])
  if (!is.factor(df[[B]])) df[[B]] <- factor(df[[B]])
  changed <- TRUE
  while (changed) {
    tab <- table(droplevels(df[[A]]), droplevels(df[[B]]))
    keepA <- rownames(tab)[apply(tab > 0, 1, all)]
    keepB <- colnames(tab)[apply(tab > 0, 2, all)]
    old_n <- nrow(df)
    if (reqA) df <- df[df[[A]] %in% keepA, , drop = FALSE]
    if (reqB) df <- df[df[[B]] %in% keepB, , drop = FALSE]
    df <- droplevels(df)
    changed <- nrow(df) < old_n
  }
  return(df)
}

filter_complete_cross_n <- function(df, factors) {
  if (length(factors) < 2) return(df)
  dfx <- df
  for (f in factors) if (!is.factor(dfx[[f]])) dfx[[f]] <- factor(dfx[[f]])
  changed <- TRUE
  while (changed) {
    cnt <- dfx %>% dplyr::count(dplyr::across(dplyr::all_of(factors)), name = "n", .drop = FALSE)
    full <- tidyr::complete(cnt, tidyr::nesting(!!!rlang::syms(factors)))
    miss <- full[is.na(full$n), , drop = FALSE]
    if (nrow(miss) == 0) break
    levels_to_drop <- lapply(factors, function(f) unique(miss[[f]]))
    names(levels_to_drop) <- factors
    old_n <- nrow(dfx)
    for (f in factors) {
      dfx <- dfx[!(dfx[[f]] %in% levels_to_drop[[f]]), , drop = FALSE]
    }
    dfx <- droplevels(dfx)
    changed <- nrow(dfx) < old_n
    if (nrow(dfx) == 0) break
  }
  dfx
}

# COEFFICIENT DE VARIATION : deux divisions par zero, et un signe qui ne veut
# rien dire.
calc_cv <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (!isTRUE(is.finite(s)) || !isTRUE(is.finite(m)) || m == 0) return(NA_real_)
  s / abs(m) * 100
}

#' Retourne le libellé lisible d'une méthode de transformation
#' @param method character : code de la méthode
#' @return character : libellé affiché
get_transformation_label <- function(method) {
  switch(method,
         "log"        = "log(x) — Logarithme naturel",
         "log1p"      = "log(x+1) — Log avec zéros",
         "log10"      = "log10(x) — Log base 10",
         "sqrt"       = "sqrt(x) — Racine carrée",
         "cuberoot"   = "x^(1/3) — Racine cubique",
         "boxcox"     = "Box-Cox (λ optimal)",
         "yeojohnson" = "Yeo-Johnson (bestNormalize)",
         "arcsin"     = "asin(sqrt(x)) — Arcsinus",
         "logit"      = "log(p/(1-p)) — Logit",
         method
  )
}

#' Retourne le code mathématique affiché de la transformation
#' @param method character
get_transformation_formula <- function(method) {
  switch(method,
         "log"        = "log(x)",
         "log1p"      = "log(x + 1)",
         "log10"      = "log10(x)",
         "sqrt"       = "sqrt(x)",
         "cuberoot"   = "x^(1/3)",
         "boxcox"     = "(x^λ - 1) / λ",
         "yeojohnson" = "Yeo-Johnson(x)",
         "arcsin"     = "asin(sqrt(x))",
         "logit"      = "log(x / (1-x))",
         method
  )
}

#' Applique une transformation à un vecteur numérique
#'
#' @param x        numeric vector — données originales (peut contenir des NA)
#' @param method   character — code de la méthode de transformation
#' @return         numeric vector de même longueur que x, avec attributs supplémentaires :
#'                   - attr(result, "lambda")    : λ Box-Cox estimé (si method = "boxcox")
#'                   - attr(result, "yj_object") : objet Yeo-Johnson (si method = "yeojohnson")
#'
apply_variable_transformation <- function(x, method) {
  x_nona <- x[!is.na(x)]
  
  result <- switch(method,
                   
                   # 1. Logarithme naturel
                   "log" = {
                     if (any(x_nona <= 0))
                       stop("log(x) requiert des valeurs strictement positives (x > 0).\n",
                            "Valeurs problématiques : ", sum(x_nona <= 0, na.rm = TRUE), " observation(s) <= 0.\n",
                            "Conseil : utilisez log(x+1) si vous avez des zéros.")
                     log(x)
                   },
                   
                   # 2. log(x+1)
                   "log1p" = {
                     if (any(x_nona < 0))
                       stop("log(x+1) requiert des valeurs >= 0 (x >= 0).\n",
                            "Valeurs négatives détectées : ", sum(x_nona < 0, na.rm = TRUE), " observation(s).")
                     log1p(x)
                   },
                   
                   # 3. Log base 10
                   "log10" = {
                     if (any(x_nona <= 0))
                       stop("log10(x) requiert des valeurs strictement positives (x > 0).\n",
                            "Valeurs problématiques : ", sum(x_nona <= 0, na.rm = TRUE), " observation(s) <= 0.")
                     log10(x)
                   },
                   
                   # 4. Racine carrée
                   "sqrt" = {
                     if (any(x_nona < 0))
                       stop("sqrt(x) requiert des valeurs >= 0.\n",
                            "Valeurs négatives : ", sum(x_nona < 0, na.rm = TRUE), " observation(s).\n",
                            "Conseil : utilisez x^(1/3) si vous avez des valeurs négatives.")
                     sqrt(x)
                   },
                   
                   # 5. Racine cubique
                   "cuberoot" = {
                     sign(x) * abs(x)^(1/3)
                   },
                   
                   # 6. Box-Cox
                   "boxcox" = {
                     if (any(x_nona <= 0))
                       stop("Box-Cox requiert des valeurs strictement positives (x > 0).\n",
                            sum(x_nona <= 0, na.rm = TRUE), " valeur(s) <= 0 détectée(s).\n",
                            "Conseil : si vous avez des zéros, utilisez Yeo-Johnson.")
                     bc_fit  <- MASS::boxcox(x_nona ~ 1, plotit = FALSE, lambda = seq(-3, 3, by = 0.01))
                     lambda  <- bc_fit$x[which.max(bc_fit$y)]
                     x_trans <- rep(NA_real_, length(x))
                     if (abs(lambda) < 1e-6) {
                       x_trans[!is.na(x)] <- log(x_nona)
                     } else {
                       x_trans[!is.na(x)] <- (x_nona^lambda - 1) / lambda
                     }
                     attr(x_trans, "lambda") <- round(lambda, 4)
                     x_trans
                   },
                   
                   # 7. Yeo-Johnson
                   "yeojohnson" = {
                     yj_obj  <- bestNormalize::yeojohnson(x_nona, standardize = FALSE)
                     x_trans <- rep(NA_real_, length(x))
                     x_trans[!is.na(x)] <- stats::predict(yj_obj, newdata = x_nona)
                     attr(x_trans, "yj_object") <- yj_obj
                     attr(x_trans, "lambda")    <- round(yj_obj$lambda, 4)
                     x_trans
                   },
                   
                   # 8. Arcsinus
                   "arcsin" = {
                     if (any(x_nona < 0 | x_nona > 1))
                       stop("asin(sqrt(x)) requiert des valeurs entre 0 et 1 (proportions/pourcentages en décimal).\n",
                            sum(x_nona < 0 | x_nona > 1, na.rm = TRUE), " valeur(s) hors [0,1].\n",
                            "Si vos données sont en %, divisez par 100 avant la transformation.")
                     asin(sqrt(x))
                   },
                   
                   # 9. Logit
                   "logit" = {
                     if (any(x_nona <= 0 | x_nona >= 1))
                       stop("logit requiert des valeurs strictement entre 0 et 1 (0 et 1 exclus).\n",
                            sum(x_nona <= 0 | x_nona >= 1, na.rm = TRUE), " valeur(s) hors ]0,1[.\n",
                            "Conseil : si vos données incluent 0 ou 1, appliquez une correction : (x*(n-1)+0.5)/n.")
                     log(x / (1 - x))
                   },
                   
                   stop("Méthode de transformation inconnue : '", method, "'")
  )
  
  return(result)
}

#' Retro-transforme des valeurs vers l'échelle originale (pour l'affichage des moyennes PostHoc)
#'
#' @param x         numeric vector — valeurs sur l'échelle transformée
#' @param method    character — code de la méthode
#' @param lambda    numeric — λ Box-Cox ou Yeo-Johnson (si disponible)
#' @param yj_object objet yeojohnson de bestNormalize (pour inversion exacte)
#' @return          numeric vector sur l'échelle originale
#'
back_transform_values <- function(x, method, lambda = NULL, yj_object = NULL) {
  tryCatch({
    switch(method,
           "log"      = exp(x),
           "log1p"    = expm1(x),
           "log10"    = 10^x,
           "sqrt"     = x^2,
           "cuberoot" = x^3,
           "arcsin"   = sin(x)^2,
           "logit"    = exp(x) / (1 + exp(x)),
           "boxcox" = {
             if (is.null(lambda)) return(x)
             if (abs(lambda) < 1e-6) exp(x) else (lambda * x + 1)^(1 / lambda)
           },
           "yeojohnson" = {
             if (!is.null(yj_object)) {
               stats::predict(yj_object, newdata = x, inverse = TRUE)
             } else {
               x  # fallback si l'objet n'est pas disponible
             }
           },
           x  # default : pas de retro-transformation
    )
  }, error = function(e) x)
}

#' Vérifie si une transformation est applicable sur un vecteur
#' Retourne une liste : list(ok = TRUE/FALSE, message = "...")
#'
check_transformation_feasibility <- function(x, method) {
  x_nona <- x[!is.na(x)]
  n <- length(x_nona)
  if (n == 0) return(list(ok = FALSE, message = "Aucune valeur non-NA disponible."))
  
  issues <- switch(method,
                   "log"     = if (any(x_nona <= 0)) trf("%s valeur(s) <= 0 détectée(s)", sum(x_nona <= 0)) else NULL,
                   "log1p"   = if (any(x_nona < 0))  trf("%s valeur(s) < 0 détectée(s)", sum(x_nona < 0))else NULL,
                   "log10"   = if (any(x_nona <= 0)) trf("%s valeur(s) <= 0 détectée(s)", sum(x_nona <= 0)) else NULL,
                   "sqrt"    = if (any(x_nona < 0))  trf("%s valeur(s) < 0 détectée(s)", sum(x_nona < 0))else NULL,
                   "cuberoot" = NULL,  # toujours applicable
                   "boxcox"  = if (any(x_nona <= 0)) trf("%s valeur(s) <= 0 (Box-Cox nécessite x > 0)", sum(x_nona <= 0))else NULL,
                   "yeojohnson" = NULL,  # toujours applicable
                   "arcsin"  = if (any(x_nona < 0 | x_nona > 1)) trf("%s valeur(s) hors [0,1]", sum(x_nona < 0 | x_nona > 1)) else NULL,
                   "logit"   = if (any(x_nona <= 0 | x_nona >= 1)) trf("%s valeur(s) hors ]0,1[", sum(x_nona <= 0 | x_nona >= 1)) else NULL,
                   NULL
  )
  
  if (!is.null(issues)) {
    list(ok = FALSE, message = issues)
  } else {
    list(ok = TRUE, message = paste0("Applicable (n = ", n, ")"))
  }
}

# -- Noms de mois et de jours : DECLARES, jamais lus dans la locale ----------
# `%B`, `%b`, `%A` et `%a` lisent LC_TIME.
HSTAT_MOIS <- list(
  fr = c("janvier", "f\u00e9vrier", "mars", "avril", "mai", "juin",
         "juillet", "ao\u00fbt", "septembre", "octobre", "novembre", "d\u00e9cembre"),
  en = c("January", "February", "March", "April", "May", "June",
         "July", "August", "September", "October", "November", "December"))

# Abreviations d'usage.
HSTAT_MOIS_ABR <- list(
  fr = c("janv.", "f\u00e9vr.", "mars", "avr.", "mai", "juin",
         "juil.", "ao\u00fbt", "sept.", "oct.", "nov.", "d\u00e9c."),
  en = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Les noms de mois que les lecteurs reconnaissent, deplies d'accents et en
# minuscules, DU PLUS LONG AU PLUS COURT : « juillet » avant « juil.
.hstat_mois_variantes <- function() {
  noms <- c(HSTAT_MOIS[["fr"]], HSTAT_MOIS[["en"]],
            HSTAT_MOIS_ABR[["fr"]], HSTAT_MOIS_ABR[["en"]],
            sub("\\.$", "", HSTAT_MOIS_ABR[["fr"]]), "fév")
  num <- c(rep(1:12, 5L), 2L)
  noms <- hstat_sans_accents(tolower(noms))
  garde <- !duplicated(noms)
  noms <- noms[garde]; num <- num[garde]
  ord <- order(nchar(noms), decreasing = TRUE)
  list(noms = noms[ord], num = num[ord])
}

# Indexes par `%u` : 1 = lundi.
HSTAT_JOURS <- list(
  fr = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"),
  en = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
HSTAT_JOURS_ABR <- list(
  fr = c("lun.", "mar.", "mer.", "jeu.", "ven.", "sam.", "dim."),
  en = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

.hstat_lang2 <- function(lang) if (identical(lang, "en")) "en" else "fr"

# Remplace `%B`, `%b`, `%A`, `%a` par le texte fourni, en laissant TOUT le
# reste intact -- `%%` compris.
.hstat_pct_sub <- function(fmt, repl) {
  ch <- strsplit(fmt, "")[[1]]
  n <- length(ch); out <- character(n); k <- 0L; i <- 1L
  while (i <= n) {
    if (identical(ch[i], "%") && i < n) {
      code <- ch[i + 1L]
      k <- k + 1L
      out[k] <- if (code %in% names(repl)) repl[[code]] else paste0("%", code)
      i <- i + 2L
    } else {
      k <- k + 1L; out[k] <- ch[i]; i <- i + 1L
    }
  }
  paste(out[seq_len(k)], collapse = "")
}

# Formate une date SANS dependre de LC_TIME.
hstat_date_fmt <- function(x, fmt, lang = hstat_langue_session()) {
  x <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (!length(x)) return(character(0))
  if (is.null(fmt) || !nzchar(fmt)) fmt <- "%Y-%m-%d"
  # Rien a faire si le format ne nomme ni mois ni jour : `format()` suffit, et
  # les codes chiffres ne dependent d'aucune locale.
  if (!grepl("%[BbAa]", fmt)) return(format(x, fmt))
  l <- .hstat_lang2(lang)
  mo <- HSTAT_MOIS[[l]]; moa <- HSTAT_MOIS_ABR[[l]]
  jo <- HSTAT_JOURS[[l]]; joa <- HSTAT_JOURS_ABR[[l]]
  vapply(seq_along(x), function(i) {
    d <- x[i]
    if (is.na(d)) return(NA_character_)
    m <- as.integer(format(d, "%m"))
    w <- as.integer(format(d, "%u"))
    format(d, .hstat_pct_sub(fmt, c(B = mo[m], b = moa[m], A = jo[w], a = joa[w])))
  }, character(1))
}

# LA LECTURE SOUFFRE DU MEME MAL QUE L'ECRITURE.
hstat_date_parse <- function(x, fmt, lang = hstat_langue_session()) {
  if (is.null(fmt) || !nzchar(fmt)) fmt <- "%Y-%m-%d"
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  if (!grepl("%[Bb]", fmt)) return(as.Date(x, format = fmt))
  mv <- .hstat_mois_variantes()
  y <- hstat_sans_accents(tolower(x))
  for (i in seq_along(mv$noms))
    y <- sub(mv$noms[i], sprintf("%02d", mv$num[i]), y, fixed = TRUE)
  as.Date(y, format = .hstat_pct_sub(fmt, c(B = "%m", b = "%m")))
}

# TOUTES LES DATES DOIVENT ENTRER, QUELLE QUE SOIT LA LANGUE DE L'APPLICATION
# 1. LE SILENCE.
# 2. LA VALEUR PLAUSIBLE ET FAUSSE.
# 3. LE FORMAT ABSENT DE LA LISTE.

# Catalogue des formats SOURCE.
HSTAT_DATE_FORMATS_SRC <- c(
  "AAAA-MM-JJ (ISO 8601)"            = "%Y-%m-%d",
  "AAAA/MM/JJ"                       = "%Y/%m/%d",
  "JJ/MM/AAAA (France)"              = "%d/%m/%Y",
  "JJ-MM-AAAA"                       = "%d-%m-%Y",
  "JJ.MM.AAAA"                       = "%d.%m.%Y",
  "MM/JJ/AAAA (US)"                  = "%m/%d/%Y",
  "MM-JJ-AAAA"                       = "%m-%d-%Y",
  "JJ Mois AAAA (25 mars 2024)"      = "%d %B %Y",
  "JJ-Mois-AAAA (25-mars-2024)"      = "%d-%b-%Y",
  "Mois JJ, AAAA (March 25, 2024)"   = "%B %d, %Y",
  "Mois JJ AAAA (March 25 2024)"     = "%B %d %Y")

# Valeur du selecteur qui demande le balayage plutot qu'un format impose.
HSTAT_DATE_AUTO <- "auto"

# Normalisation employee par le controle d'aller-retour ci-dessous.
.hstat_date_norm <- function(x) {
  y <- hstat_sans_accents(tolower(as.character(x)))
  mv <- .hstat_mois_variantes()
  for (i in seq_along(mv$noms))
    y <- sub(mv$noms[i], as.character(mv$num[i]), y, fixed = TRUE)
  y <- gsub("(^|[^0-9])0+([0-9])", "\\1\\2", y)
  gsub("[[:space:]]+", " ", trimws(y))
}

# UN FORMAT N'EST RETENU QUE S'IL SE RELIT.
.hstat_date_coherent <- function(src, d, fmt) {
  ok <- !is.na(d) & !is.na(src) & nzchar(trimws(as.character(src)))
  if (!any(ok)) return(logical(length(d)))
  cible <- .hstat_date_norm(src)
  res <- rep(FALSE, length(d))
  for (lg in c("fr", "en")) {
    rendu <- rep(NA_character_, length(d))
    rendu[ok] <- tryCatch(hstat_date_fmt(d[ok], fmt, lang = lg),
                          error = function(e) NA_character_)
    res <- res | (!is.na(rendu) & .hstat_date_norm(rendu) == cible)
  }
  res & ok
}

# Lecture d'une colonne de dates, format decide plutot que devine.
hstat_date_auto <- function(x, fmt = NULL, seuil = 0.8) {
  n <- length(x)
  vide <- list(dates = as.Date(rep(NA_real_, n), origin = "1970-01-01"),
               format = NA_character_, auto = FALSE, ambigu = character(0),
               n_ok = 0L, n = n)
  if (!n) return(vide)
  if (inherits(x, c("Date", "POSIXct", "POSIXlt")))
    return(list(dates = as.Date(x), format = NA_character_, auto = FALSE,
                ambigu = character(0), n_ok = sum(!is.na(x)), n = n))
  src <- as.character(x)
  utile <- !is.na(src) & nzchar(trimws(src))
  n_utile <- sum(utile)
  if (!n_utile) return(vide)

  essai <- function(f) {
    d <- suppressWarnings(tryCatch(hstat_date_parse(src, f),
                                   error = function(e) NULL))
    if (is.null(d) || length(d) != n) return(NULL)
    list(dates = d, ok = sum(.hstat_date_coherent(src, d, f)))
  }

  impose <- !is.null(fmt) && nzchar(fmt) && !identical(fmt, HSTAT_DATE_AUTO)
  if (impose) {
    e <- essai(fmt)
    if (!is.null(e) && e$ok >= seuil * n_utile)
      return(list(dates = e$dates, format = fmt, auto = FALSE,
                  ambigu = character(0), ok = NULL, n_ok = e$ok, n = n_utile))
  }

  cands <- unname(HSTAT_DATE_FORMATS_SRC)
  if (impose) cands <- unique(c(fmt, cands))
  res <- lapply(cands, essai)
  names(res) <- cands
  res <- res[!vapply(res, is.null, logical(1))]
  if (!length(res)) return(vide)
  scores <- vapply(res, function(r) r$ok, integer(1))
  best <- max(scores)
  if (best == 0L) return(vide)
  gagnants <- names(scores)[scores == best]
  retenu <- gagnants[1]
  # Ne sont ambigus que les formats qui relisent AUTANT et rendent AUTRE CHOSE :
  # deux ecritures d'une meme date ne posent aucune question.
  ambigu <- gagnants[vapply(gagnants, function(g)
    !identical(res[[g]]$dates, res[[retenu]]$dates), logical(1))]
  list(dates = res[[retenu]]$dates, format = retenu,
       auto = !identical(retenu, fmt),
       ambigu = if (length(ambigu)) c(retenu, ambigu) else character(0),
       n_ok = best, n = n_utile)
}

# Libelle lisible d'un format, pour les messages.
hstat_date_fmt_label <- function(fmt) {
  if (is.null(fmt) || !length(fmt) || is.na(fmt[1])) return("")
  i <- match(fmt[1], HSTAT_DATE_FORMATS_SRC)
  if (is.na(i)) fmt[1] else names(HSTAT_DATE_FORMATS_SRC)[i]
}

VIZ_DATE_FORMATS_VALID <- c(
  "%d-%m-%Y", "%m-%d-%Y", "%Y-%m-%d", "%Y-%d-%m",
  "%d/%m/%Y", "%m/%d/%Y",
  "%d-%m",    "%m-%d",    "%m-%Y",    "%Y-%m",
  "%d-%b-%Y", "%b-%Y",    "%d-%b",    "%b-%d",    "%Y-%b-%d",
  "%d %B %Y", "%B %Y",    "%d %B",    "%B %d",    "%Y %B"
)

viz_valid_date_fmt <- function(fmt) {
  !is.null(fmt) && nzchar(trimws(fmt)) && fmt %in% VIZ_DATE_FORMATS_VALID
}

# Conversion numerique tolerante : decimales a virgule (format FR), espaces
# (separateurs de milliers, espaces insecables).
hstat_as_numeric_fr <- function(x) {
  if (is.numeric(x)) return(x)
  ch <- as.character(x)
  ch <- gsub("[\u00a0\u202f[:space:]]", "", ch)
  ch <- gsub(",", ".", ch, fixed = TRUE)
  out <- suppressWarnings(as.numeric(ch))
  n_src <- sum(!is.na(x) & nzchar(trimws(as.character(x))))
  if (n_src == 0) return(NULL)
  if (sum(!is.na(out)) / n_src < 0.9) return(NULL)
  out
}

viz_detect_x_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("date")
  if (is.factor(x))    return("factor")
  if (is.numeric(x))   return("numeric")
  if (is.character(x))
    return(if (length(unique(x)) < length(x) / 2) "categorical" else "text")
  "text"
}

# Styles d'ecriture proposes partout ou un texte de graphique se met en forme.

# « ancien = nouveau », une ligne par element.
hstat_legende_etiquettes <- function(txt) {
  if (is.null(txt) || !length(txt)) return(character(0))
  # Borne : 200 lignes de libelles bornes, c'est-a-dire plus qu'aucune legende
  # n'en porte, et rien de ce qu'un message forge pourrait faire mettre en page.
  txt <- hstat_texte_client(paste(as.character(txt), collapse = "\n"),
                            max = 200L * (HSTAT_TEXTE_CLIENT_MAX + 4L))
  l <- utils::head(unlist(strsplit(txt, "\r?\n")), 200L)
  l <- l[grepl("=", l, fixed = TRUE)]
  if (!length(l)) return(character(0))
  pos <- regexpr("=", l, fixed = TRUE)
  anc <- trimws(substr(l, 1, pos - 1))
  nou <- trimws(substr(l, pos + 1, nchar(l)))
  ok  <- nzchar(anc) & nzchar(nou)
  stats::setNames(substr(nou[ok], 1L, HSTAT_TEXTE_CLIENT_MAX), anc[ok])
}

# Le renommage passe par les LIBELLES de l'echelle, jamais par les donnees.
.hstat_legende_libelles <- function(ancien, ren) {
  force(ancien); force(ren)
  function(x) {
    base <- if (is.function(ancien)) ancien(x)
            else if (is.null(ancien) || inherits(ancien, "waiver")) x
            else if (!is.null(names(ancien)) && all(as.character(x) %in% names(ancien)))
              unname(ancien[as.character(x)])
            else if (length(ancien) == length(x)) ancien
            else x
    base <- as.character(base)
    cle <- as.character(x)
    i <- match(cle, names(ren))
    base[!is.na(i)] <- unname(ren[i[!is.na(i)]])
    base
  }
}

# Applique les reglages de legende a un ggplot.
hstat_legende_appliquer <- function(p, titre = NULL, masquer_titre = FALSE,
                                    etiquettes = character(0),
                                    taille_titre = NA, taille_texte = NA) {
  if (!inherits(p, "ggplot")) return(p)
  titre <- if (is.null(titre)) "" else trimws(as.character(titre)[1])
  if (is.na(titre)) titre <- ""
  if (nzchar(titre) || length(etiquettes)) {
    b <- tryCatch(suppressWarnings(suppressMessages(ggplot2::ggplot_build(p))),
                  error = function(e) NULL)
    if (!is.null(b)) {
      pos <- c("x", "y", "xmin", "xmax", "ymin", "ymax", "xend", "yend",
               "xintercept", "yintercept", "lower", "upper", "middle")
      for (s in b$plot$scales$scales) {
        if (any(s$aesthetics %in% pos)) next
        cl <- s$clone()
        if (nzchar(titre)) cl$name <- titre
        if (length(etiquettes) && isTRUE(cl$is_discrete()))
          cl$labels <- .hstat_legende_libelles(s$labels, etiquettes)
        p <- suppressMessages(p + cl)
      }
    }
  }
  th <- list()
  if (isTRUE(masquer_titre)) th$legend.title <- ggplot2::element_blank()
  else if (is.finite(suppressWarnings(as.numeric(taille_titre))[1]))
    th$legend.title <- ggplot2::element_text(size = as.numeric(taille_titre)[1])
  if (is.finite(suppressWarnings(as.numeric(taille_texte))[1]))
    th$legend.text <- ggplot2::element_text(size = as.numeric(taille_texte)[1])
  if (length(th)) p <- p + do.call(ggplot2::theme, th)
  p
}

# Les reglages, declares une fois.
hstat_legende_ui <- function(prefix) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8, shiny::textInput(paste0(prefix, "_legtitre"), "Titre de la légende",
                                        placeholder = "Titre automatique")),
      shiny::column(4, shiny::div(style = "margin-top:24px;",
        shiny::checkboxInput(paste0(prefix, "_legsanstitre"), "Masquer le titre", FALSE)))),
    shiny::textAreaInput(paste0(prefix, "_legetiq"), "Renommer les éléments de la légende",
                         rows = 3, placeholder = "Colonne = Couleur des yeux\nLigne = Couleur des cheveux"),
    shiny::tags$small(style = "color:#6b7280;",
      "Une ligne par élément : le libellé affiché, un signe égal, le nouveau libellé. Les couleurs et les formes ne changent pas."),
    shiny::fluidRow(
      shiny::column(6, shiny::numericInput(paste0(prefix, "_legtitresz"), "Taille du titre (pt)",
                                           value = NULL, min = 6, max = 30, step = 1)),
      shiny::column(6, shiny::numericInput(paste0(prefix, "_legtextesz"), "Taille des éléments (pt)",
                                           value = NULL, min = 6, max = 30, step = 1))),
    shiny::tags$small(style = "color:#6b7280;", "Taille vide : celle du thème."),
    shiny::checkboxInput(paste0(prefix, "_centre"), "Centrer le graphique sur l'origine (0, 0)", FALSE))
}

# Une carte factorielle se lit par rapport a son origine : c'est le centre de
# gravite des profils.
hstat_centrer_origine <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  sc <- tryCatch(ggplot2::layer_scales(p), error = function(e) NULL)
  if (is.null(sc)) return(p)
  demi <- function(s) {
    if (is.null(s) || !inherits(s, "ScaleContinuous")) return(NA_real_)
    r <- suppressWarnings(as.numeric(s$range$range))
    r <- r[is.finite(r)]
    # 10 % au-dela du point le plus eloigne : son etiquette tient dans le cadre.
    if (!length(r)) NA_real_ else 1.1 * max(abs(r))
  }
  mx <- demi(sc$x); my <- demi(sc$y)
  lim <- list()
  if (is.finite(mx) && mx > 0) lim$x <- c(-mx, mx)
  if (is.finite(my) && my > 0) lim$y <- c(-my, my)
  if (!length(lim)) return(p)
  p + do.call(ggplot2::expand_limits, lim)
}

# Lit les reglages ci-dessus et les applique.
hstat_legende_depuis <- function(p, lire) {
  borne <- function(v) {
    v <- suppressWarnings(as.numeric(v))[1]
    if (length(v) && is.finite(v)) min(max(v, 6), 30) else NA
  }
  p <- hstat_legende_appliquer(p,
    titre         = lire("legtitre"),
    masquer_titre = isTRUE(lire("legsanstitre")),
    etiquettes    = hstat_legende_etiquettes(lire("legetiq")),
    taille_titre  = borne(lire("legtitresz")),
    taille_texte  = borne(lire("legtextesz")))
  if (isTRUE(lire("centre"))) p <- hstat_centrer_origine(p)
  p
}

# Reglages de forme communs aux analyses multivariees historiques (ACP, HCPC,
# AFD).
hstat_mv_forme_ui <- function(prefix, titre = "Apparence du graphique") {
  .hstat_opt_section(
    titre, "brush", "#8e44ad", "#f7f0fb",
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(paste0(prefix, "_theme"), "Thème",
        choices = c(HSTAT_THEMES_MV, HSTAT_THEMES_GG),
        selected = "gg")),
      shiny::column(6, shiny::textInput(paste0(prefix, "_subtitle"), "Sous-titre",
                                        placeholder = "Optionnel"))),
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(paste0(prefix, "_legendpos"), "Légende",
        choices = c("Par défaut (ggplot2)" = "gg", "À droite" = "right",
                    "À gauche" = "left", "En haut" = "top", "En bas" = "bottom",
                    "Masquée" = "none"), selected = "gg")),
      shiny::column(6, shiny::selectInput(paste0(prefix, "_grid"), "Grille",
        choices = c("Par défaut (ggplot2)" = "gg", "Principale seule" = "majeure",
                    "Aucune" = "sans"), selected = "gg"))),
    hstat_legende_ui(prefix))
}

# Les teintes des cartes d'options se DECLARENT, elles ne se choisissent pas
# carte par carte.
HSTAT_OPT_TEINTES <- list(
  bleu     = c("#1f6fb2", "#eaf3fb"),
  orange   = c("#b4500b", "#fdf0e6"),
  vert     = c("#1b7a43", "#e9f7ef"),
  violet   = c("#7d3c98", "#f4ecfa"),
  rouge    = c("#b03a2e", "#fbeceb"),
  sarcelle = c("#0f766e", "#e6f4f1"),
  ardoise  = c("#4d5d6e", "#eef2f5"),
  ocre     = c("#8a6200", "#fdf6e3"),
  magenta  = c("#a8326e", "#fbeaf3"),
  indigo   = c("#3f51b5", "#ecedf8")
)

# Une carte d'options a la teinte du catalogue.
.hstat_opt_carte <- function(titre, icone, teinte, ...) {
  if (!is.character(teinte) || length(teinte) != 1L ||
      !teinte %in% names(HSTAT_OPT_TEINTES)) {
    stop(sprintf("Teinte de carte inconnue : %s", paste(teinte, collapse = ", ")),
         call. = FALSE)
  }
  tc <- HSTAT_OPT_TEINTES[[teinte]]
  .hstat_opt_section(titre, icone, tc[1], tc[2], ...)
}

# Contraste WCAG entre deux couleurs hexadecimales (1 a 21).
hstat_contraste <- function(a, b) {
  lum <- function(h) {
    v <- grDevices::col2rgb(h)[, 1] / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v)
  }
  l <- sort(c(lum(a), lum(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

# Une couleur de texte assombrie juste assez pour se lire sur son fond (WCAG
# AA, 4,5:1).
hstat_couleur_lisible <- function(couleur, fond = "#ffffff", ratio = 4.5) {
  ok <- tryCatch({ grDevices::col2rgb(couleur); grDevices::col2rgb(fond); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(couleur)
  v <- grDevices::col2rgb(couleur)[, 1]
  for (k in 0:20) {
    c2 <- grDevices::rgb(t(round(v * (1 - k * 0.05))), maxColorValue = 255)
    if (hstat_contraste(c2, fond) >= ratio) return(tolower(c2))
  }
  "#000000"
}

# Le titre est un h4 : il suit le h3 de la boite, sans sauter de niveau.
.hstat_opt_section <- function(titre, icone, couleur, fond, ...) {
  shiny::div(
    style = sprintf(paste0("background:%s;border-left:4px solid %s;border-radius:6px;",
                           "padding:14px 16px;margin-bottom:14px;"), fond, couleur),
    shiny::h4(shiny::icon(icone), " ", titre,
              style = sprintf(paste0("font-weight:700;color:%s;margin:0 0 12px 0;",
                                     "text-transform:uppercase;letter-spacing:.4px;font-size:12px;"),
                              hstat_couleur_lisible(couleur, fond))),
    ...)
}

# Le vocabulaire de mise en forme que TOUS les panneaux « Options du
# graphique » doivent porter, quel que soit le module.

# LE KIT SE PREND PAR FAMILLE, PAS EN BLOC.
HSTAT_PLOT_EXTRAS_FAMILLES <- c("police", "axe", "cles", "marges",
                                "styles", "bornes", "angles", "pas",
                                "legende")

# LE DEFAUT NE S'ELARGIT PAS QUAND LE CATALOGUE S'ELARGIT, et c'est une mesure
# qui l'a impose.
HSTAT_PLOT_EXTRAS_DEFAUT <- c("police", "axe", "cles", "marges")

# La correspondance famille -> reglages n'est ecrite qu'ici ; le test la relit
# plutot que de la recopier.
HSTAT_PLOT_EXTRAS_PAR_FAMILLE <- list(
  police = "PoliceBase",
  axe    = c("AxisLine", "AxisLineCouleur", "AxisLineEpaisseur"),
  cles   = "LegendeCles",
  marges = c("MargeHaut", "MargeBas", "MargeGauche", "MargeDroite"),
  # LES SUFFIXES DE `styles` ET `bornes` SONT CEUX QUE `hstat_plot_opts_ui`
  # DECLARAIT DEJA CHEZ ELLE.
  styles = c("StTitre", "StAxes", "StGrad"),
  bornes = c("Xmin", "Xmax", "Ymin", "Ymax"),
  angles = c("AngleX", "AngleY"),
  pas    = c("PasX", "PasY"),
  legende = c("LegendeTitre", "LegendeTitreTaille", "LegendeTaille"))

HSTAT_PLOT_EXTRAS <- unlist(HSTAT_PLOT_EXTRAS_PAR_FAMILLE, use.names = FALSE)

# Le titre de la carte ne nomme AUCUNE des familles : il doit rester vrai quel
# que soit le sous-ensemble retenu.
hstat_plot_extras_ui <- function(ns, prefix,
                                 familles = HSTAT_PLOT_EXTRAS_DEFAUT) {
  familles <- intersect(as.character(familles), HSTAT_PLOT_EXTRAS_FAMILLES)
  if (!length(familles)) return(NULL)
  id <- function(x) ns(paste0(prefix, x))
  els <- list()

  # LA POLICE DE BASE EST CELLE DU THEME, pas un reglage de plus.
  if ("police" %in% familles)
    els <- c(els, list(
      shiny::sliderInput(id("PoliceBase"), "Taille de police de base",
                         min = 6, max = 24, value = 11, step = 1, ticks = FALSE)))

  # Decoche, le reglage ne pose RIEN.
  if ("axe" %in% familles)
    els <- c(els, list(
      shiny::checkboxInput(id("AxisLine"), "Tracer les axes X et Y", value = FALSE),
      shiny::conditionalPanel(
        ns = ns, condition = sprintf("input.%sAxisLine == true", prefix),
        shiny::fluidRow(
          shiny::column(6, colourInput(id("AxisLineCouleur"), "Couleur des axes",
                                       value = "#000000")),
          shiny::column(6, shiny::sliderInput(id("AxisLineEpaisseur"),
                                              "Épaisseur des axes",
                                              min = 0.1, max = 3, value = 1,
                                              step = 0.1, ticks = FALSE))))))

  if ("cles" %in% familles)
    els <- c(els, list(
      shiny::sliderInput(id("LegendeCles"), "Taille des clés de légende",
                         min = 0.4, max = 3, value = 1.2, step = 0.1,
                         ticks = FALSE)))

  if ("styles" %in% familles)
    els <- c(els, list(
      shiny::fluidRow(
        shiny::column(4, shiny::selectInput(id("StTitre"), "Style du titre",
                                            choices = HSTAT_FONT_STYLES,
                                            selected = "bold")),
        shiny::column(4, shiny::selectInput(id("StAxes"), "Style des titres d'axes",
                                            choices = HSTAT_FONT_STYLES,
                                            selected = "plain")),
        shiny::column(4, shiny::selectInput(id("StGrad"), "Style des graduations",
                                            choices = HSTAT_FONT_STYLES,
                                            selected = "plain")))))

  # L'INCLINAISON EST UN ANGLE, PAS UN OUI/NON, et elle commande aussi le
  # CALAGE.
  if ("angles" %in% familles)
    els <- c(els, list(
      shiny::fluidRow(
        shiny::column(6, shiny::sliderInput(id("AngleX"),
                                            "Inclinaison des graduations X (°)",
                                            min = 0, max = 90, value = 0,
                                            step = 5, ticks = FALSE)),
        shiny::column(6, shiny::sliderInput(id("AngleY"),
                                            "Inclinaison des graduations Y (°)",
                                            min = 0, max = 90, value = 0,
                                            step = 5, ticks = FALSE)))))

  # LES BORNES RECADRENT, ELLES NE FILTRENT PAS : `coord_cartesian` masque ce
  # qui sort du cadre au lieu de le retirer des donnees.
  if ("bornes" %in% familles)
    els <- c(els, list(
      shiny::fluidRow(
        shiny::column(3, shiny::numericInput(id("Xmin"), "X minimum", value = NULL)),
        shiny::column(3, shiny::numericInput(id("Xmax"), "X maximum", value = NULL)),
        shiny::column(3, shiny::numericInput(id("Ymin"), "Y minimum", value = NULL)),
        shiny::column(3, shiny::numericInput(id("Ymax"), "Y maximum", value = NULL))),
      shiny::tags$small(style = "color:#6b7280;",
        tr("Bornes vides = automatiques. Elles recadrent la figure ; aucune observation n'est retirée du calcul."))))

  if ("legende" %in% familles)
    els <- c(els, list(
      shiny::fluidRow(
        shiny::column(4, shiny::textInput(id("LegendeTitre"),
                                          "Titre de la légende", value = "")),
        shiny::column(4, shiny::numericInput(id("LegendeTitreTaille"),
                                             "Taille du titre de légende",
                                             value = 11, min = 4, max = 30, step = 1)),
        shiny::column(4, shiny::numericInput(id("LegendeTaille"),
                                             "Taille du texte de légende",
                                             value = 10, min = 4, max = 30, step = 1)))))

  # UN PAS VIDE LAISSE LES GRADUATIONS AUTOMATIQUES.
  if ("pas" %in% familles)
    els <- c(els, list(
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(id("PasX"), "Pas des graduations X",
                                             value = NULL, min = 0)),
        shiny::column(6, shiny::numericInput(id("PasY"), "Pas des graduations Y",
                                             value = NULL, min = 0))),
      shiny::tags$small(style = "color:#6b7280;",
        tr("Pas vide = graduations automatiques. Le pas ne s'applique qu'à un axe numérique."))))

  if ("marges" %in% familles)
    els <- c(els, list(
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(id("MargeHaut"), "Marge haut (pt)",
                                             value = 5, min = 0, step = 1)),
        shiny::column(6, shiny::numericInput(id("MargeBas"), "Marge bas (pt)",
                                             value = 5, min = 0, step = 1))),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(id("MargeGauche"), "Marge gauche (pt)",
                                             value = 5, min = 0, step = 1)),
        shiny::column(6, shiny::numericInput(id("MargeDroite"), "Marge droite (pt)",
                                             value = 5, min = 0, step = 1))),
      shiny::tags$small(style = "color:#5f6b7a;font-style:italic;",
                        "Les marges se comptent en points, comme les tailles de texte.")))

  do.call(.hstat_opt_section,
          c(list("Mise en forme générale", "vector-square",
                 HSTAT_OPT_TEINTES$ardoise[1], HSTAT_OPT_TEINTES$ardoise[2]),
            els))
}

# La lecture rend une liste NOMMEE, jamais les entrees une par une.
hstat_plot_extras_lire <- function(input, prefix,
                                   familles = HSTAT_PLOT_EXTRAS_DEFAUT) {
  familles <- intersect(as.character(familles), HSTAT_PLOT_EXTRAS_FAMILLES)
  g <- function(x, defaut) {
    v <- input[[paste0(prefix, x)]]
    if (is.null(v) || (length(v) == 1L && is.na(v))) defaut else v
  }
  n <- function(x, defaut) {
    v <- suppressWarnings(as.numeric(g(x, defaut))[1])
    if (!isTRUE(is.finite(v))) defaut else v
  }
  # `nv()` rend NA sur une saisie vide, la ou `n()` retombe sur un defaut.
  nv <- function(x) {
    v <- suppressWarnings(as.numeric(input[[paste0(prefix, x)]])[1])
    if (isTRUE(is.finite(v))) v else NA_real_
  }
  st <- function(x, defaut) {
    v <- g(x, defaut)
    if (length(v) != 1L || !nzchar(as.character(v)[1])) defaut else as.character(v)[1]
  }
  list(familles = familles,
       police   = n("PoliceBase", 11),
       axe      = isTRUE(g("AxisLine", FALSE)),
       axe_col  = as.character(g("AxisLineCouleur", "#000000"))[1],
       axe_ep   = n("AxisLineEpaisseur", 1),
       cles     = n("LegendeCles", 1.2),
       marges   = c(haut = n("MargeHaut", 5), droite = n("MargeDroite", 5),
                    bas  = n("MargeBas", 5),  gauche = n("MargeGauche", 5)),
       st_titre = st("StTitre", "bold"),
       st_axes  = st("StAxes", "plain"),
       st_grad  = st("StGrad", "plain"),
       angle_x  = n("AngleX", 0),
       angle_y  = n("AngleY", 0),
       bornes   = c(xmin = nv("Xmin"), xmax = nv("Xmax"),
                    ymin = nv("Ymin"), ymax = nv("Ymax")),
       pas      = c(x = nv("PasX"), y = nv("PasY")),
       leg_titre      = as.character(g("LegendeTitre", ""))[1],
       leg_titre_taille = n("LegendeTitreTaille", 11),
       leg_taille       = n("LegendeTaille", 10))
}

# `police` ne passe PAS par ici.
hstat_plot_extras_theme <- function(o, titre = TRUE) {
  f  <- as.character(o$familles %||% HSTAT_PLOT_EXTRAS_FAMILLES)
  th <- ggplot2::theme()
  if ("cles" %in% f)
    th <- th + ggplot2::theme(
      legend.key.height = ggplot2::unit(o$cles, "lines"),
      legend.key.width  = ggplot2::unit(o$cles * 1.25, "lines"))
  if ("marges" %in% f) {
    m  <- o$marges
    th <- th + ggplot2::theme(
      plot.margin = ggplot2::margin(t = m[["haut"]], r = m[["droite"]],
                                    b = m[["bas"]], l = m[["gauche"]], unit = "pt"))
  }
  # LE STYLE NE TOUCHE QUE LA FACE.
  if ("styles" %in% f) {
    if (isTRUE(titre))
      th <- th + ggplot2::theme(
        plot.title = ggplot2::element_text(face = o$st_titre))
    th <- th + ggplot2::theme(
      axis.title = ggplot2::element_text(face = o$st_axes),
      axis.text  = ggplot2::element_text(face = o$st_grad))
  }

  # L'ANGLE COMMANDE LE CALAGE, et les deux se posent ensemble : une etiquette
  # penchee finit sous sa graduation, une etiquette droite se centre.
  if ("angles" %in% f) {
    ax <- o$angle_x; ay <- o$angle_y
    th <- th + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = ax, hjust = if (ax > 0) 1 else 0.5,
        vjust = if (ax > 0) 1 else 0.5),
      # Cote Y le calage ne depend pas de l'angle : l'etiquette reste collee a
      # son axe (`hjust = 1`) et centree sur sa graduation, penchee ou non.
      axis.text.y = ggplot2::element_text(angle = ay, hjust = 1, vjust = 0.5))
  }

  if ("legende" %in% f)
    th <- th + ggplot2::theme(
      legend.title = ggplot2::element_text(size = o$leg_titre_taille),
      legend.text  = ggplot2::element_text(size = o$leg_taille))

  if (!("axe" %in% f) || !isTRUE(o$axe)) return(th)
  th + ggplot2::theme(
    axis.line   = ggplot2::element_line(colour = o$axe_col, linewidth = o$axe_ep),
    axis.line.x = ggplot2::element_line(colour = o$axe_col, linewidth = o$axe_ep),
    axis.line.y = ggplot2::element_line(colour = o$axe_col, linewidth = o$axe_ep))
}

# BORNES ET PAS : ce sont des ECHELLES, pas un theme

# Un axe DISCRET n'a pas de pas.
.hstat_extras_axe_continu <- function(g, aes) {
  sc <- tryCatch(g$scales$get_scales(aes), error = function(e) NULL)
  if (inherits(sc, "Scale")) return(!isTRUE(sc$is_discrete()))

  # LE MAPPAGE S'EVALUE, IL NE SE LIT PAS AU NOM.
  d <- g$data
  if (!is.data.frame(d)) return(FALSE)
  m <- g$mapping[[aes]]
  if (is.null(m)) for (l in g$layers) {
    if (!is.null(l$mapping[[aes]])) { m <- l$mapping[[aes]]; break }
  }
  if (is.null(m)) return(FALSE)
  # Une quosure HERITE de « formula » : `m[[2]]` y marche encore, mais rlang
  # avertit a chaque appel que l'acces est deprecie.
  ex <- tryCatch(
    if (requireNamespace("rlang", quietly = TRUE) && rlang::is_quosure(m))
      rlang::quo_get_expr(m)
    else if (inherits(m, "formula")) m[[length(m)]] else m,
    error = function(e) NULL)
  if (is.null(ex)) return(FALSE)
  # `baseenv()` en environnement englobant : le mappage ne doit pas resoudre un
  # nom quelconque de l'espace global au passage.
  v <- tryCatch(eval(ex, c(as.list(d), list(.data = d)), baseenv()),
                error = function(e) NULL, warning = function(w) NULL)
  is.numeric(v) || inherits(v, c("Date", "POSIXct", "POSIXlt"))
}

.hstat_extras_pas <- function(g, aes, pas) {
  if (!isTRUE(is.finite(pas)) || pas <= 0) return(g)
  if (!.hstat_extras_axe_continu(g, aes)) return(g)
  brk <- function(lim) {
    if (!all(is.finite(lim))) return(ggplot2::waiver())
    # Alignee sur le pas, PUIS bornee au cadre.
    b <- hstat_graduations(min(lim), max(lim) + pas, pas)
    if (is.null(b)) return(ggplot2::waiver())
    b[b >= min(lim) & b <= max(lim)]
  }
  # DEUX ECHELLES NE S'AJOUTENT PAS.
  sc <- tryCatch(g$scales$get_scales(aes), error = function(e) NULL)
  if (inherits(sc, "Scale")) { sc$breaks <- brk; return(g) }
  g + (if (identical(aes, "x")) ggplot2::scale_x_continuous(breaks = brk)
       else ggplot2::scale_y_continuous(breaks = brk))
}

hstat_plot_extras_scales <- function(g, o) {
  if (is.null(g) || !inherits(g, "ggplot")) return(g)
  f <- as.character(o$familles %||% HSTAT_PLOT_EXTRAS_FAMILLES)

  # LE TITRE DE LEGENDE EST UN LIBELLE, pas un theme : il se pose par `labs()`.
  if ("legende" %in% f && nzchar(o$leg_titre %||% ""))
    g <- g + ggplot2::labs(colour = o$leg_titre, fill = o$leg_titre)

  if ("pas" %in% f) {
    g <- .hstat_extras_pas(g, "x", o$pas[["x"]])
    g <- .hstat_extras_pas(g, "y", o$pas[["y"]])
  }

  # LA COORD NE SE POSE QUE SI UNE BORNE EST REMPLIE : en poser une d'office
  # remplacerait celle qu'un constructeur a choisie.
  if (!("bornes" %in% f)) return(g)
  b <- o$bornes
  xl <- c(b[["xmin"]], b[["xmax"]]); yl <- c(b[["ymin"]], b[["ymax"]])
  if (!any(is.finite(c(xl, yl)))) return(g)
  g + ggplot2::coord_cartesian(
    xlim = if (any(is.finite(xl))) xl else NULL,
    ylim = if (any(is.finite(yl))) yl else NULL)
}

# LA LIGNE ZERO EST UNE SEULE LIGNE, DONC UN SEUL REGLAGE
HSTAT_AXE_ZERO <- c("Aucune ligne au zéro"                = "aucune",
                    "Ligne de référence en pointillés"    = "reference",
                    "Axe X posé sur la ligne zéro"        = "axe")

# Ce que « axe » fait, et pourquoi chacune des trois pieces est necessaire :
# 1. LA COUCHE.
# 2. L'ANCIEN TRAIT S'EFFACE.
# 3. L'EXPANSION DU BAS DISPARAIT QUAND RIEN N'EST NEGATIF.
hstat_axe_zero <- function(mode = "reference", negatifs = FALSE,
                           couleur = "#000000", epaisseur = 1,
                           couleur_ref = "#5f6b7a") {
  mode <- as.character(mode %||% "reference")[1]
  if (!isTRUE(mode %in% HSTAT_AXE_ZERO)) mode <- "reference"
  res <- list(mode = mode, couches = list(), expand = ggplot2::waiver())
  if (identical(mode, "aucune")) return(res)
  if (identical(mode, "reference")) {
    res$couches <- list(ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                            colour = couleur_ref, linewidth = 0.5))
    return(res)
  }
  ep <- suppressWarnings(as.numeric(epaisseur)[1])
  if (!isTRUE(is.finite(ep)) || ep <= 0) ep <- 1
  col <- as.character(couleur %||% "#000000")[1]
  if (!isTRUE(nzchar(col))) col <- "#000000"
  res$couches <- list(
    ggplot2::geom_hline(yintercept = 0, colour = col, linewidth = ep),
    # Le trait doit etre DANS le cadre : sans cela le reglage rendrait un axe
    # invisible sur une serie qui n'atteint jamais zero.
    ggplot2::expand_limits(y = 0),
    ggplot2::theme(axis.line.x        = ggplot2::element_blank(),
                   axis.line.x.bottom = ggplot2::element_blank(),
                   axis.line.x.top    = ggplot2::element_blank()))
  res$expand <- if (isTRUE(negatifs)) ggplot2::waiver()
                else ggplot2::expansion(mult = c(0, 0.05))
  res
}

HSTAT_FONT_STYLES <- c("Normal" = "plain", "Gras" = "bold",
                       "Italique" = "italic", "Gras + italique" = "bold.italic")

# Themes proposes a l'utilisateur, avec le libelle affiche.
HSTAT_PALETTES_QUALI <- c("Set1 - vive" = "Set1", "Set2 - douce" = "Set2",
                          "Dark2 - soutenue" = "Dark2",
                          "Accent - contrastée" = "Accent",
                          "Paired - par paires" = "Paired",
                          "Set3 - large (12)" = "Set3",
                          "Pastel" = "Pastel1")

# LA PALETTE PAR DEFAUT DE ggplot2 -- teintes reparties sur le cercle HCL.
HSTAT_PALETTE_GG <- c("ggplot2 (défaut) - une couleur par modalité" = "ggplot2")

# UN NOM DE COULEUR SEUL EST UNE VALEUR DE DONNEES PLAUSIBLE.
HSTAT_PALETTES_DEGRADE <- c("Dégradé de bleus" = "Blues",
                            "Dégradé de verts" = "Greens",
                            "Dégradé de rouges" = "Reds",
                            "Dégradé de violets" = "Purples",
                            "Dégradé bleu-vert" = "YlGnBu",
                            "Dégradé mauve" = "BuPu",
                            "Spectral (froid -> chaud)" = "Spectral",
                            "Rouge-jaune-bleu" = "RdYlBu")

# -- LA PALETTE NE SE POSE QU'A UN ENDROIT -------------------------------------

# Liste de choix commune.
hstat_palettes_choix <- function(degrades = TRUE, autres = TRUE) {
  ch <- list("Palette par défaut" = HSTAT_PALETTE_GG,
             "Teintes vives (groupes distincts)" = HSTAT_PALETTES_QUALI)
  if (isTRUE(degrades))
    ch[["Dégradés (valeurs ordonnées)"]] <- HSTAT_PALETTES_DEGRADE
  if (isTRUE(autres))
    ch[["Accessibles et monochromes"]] <- HSTAT_PALETTES_AUTRES
  ch
}

# Palettes hors RColorBrewer, declarees ICI et nulle part ailleurs : cinq
# modules en tenaient chacun leur copie (viridis, gris, monochromes).
HSTAT_PALETTES_AUTRES <- c("Viridis (lisible par les daltoniens)" = "viridis",
                           "Niveaux de gris" = "grey",
                           "Monochrome bleu" = "mono_blue",
                           "Monochrome vert" = "mono_green",
                           "Monochrome rouge" = "mono_red",
                           "Monochrome violet" = "mono_purple",
                           "Monochrome orange" = "mono_orange",
                           "Monochrome noir" = "mono_black")
HSTAT_PALETTES_MONO <- c(mono_blue = "#2E86AB", mono_green = "#06A77D",
                         mono_red = "#D62828", mono_purple = "#6A4C93",
                         mono_orange = "#F77F00", mono_black = "#2C3E50")

# n COULEURS POUR N'IMPORTE QUELLE PALETTE DU CATALOGUE.
hstat_palette_couleurs <- function(pal, n) {
  n <- suppressWarnings(as.integer(n)[1])
  if (is.na(n) || n <= 0L) return(character(0))
  pal <- as.character(pal %||% "")[1]
  if (is.na(pal)) pal <- ""
  if (pal %in% c(HSTAT_PALETTES_QUALI, HSTAT_PALETTES_DEGRADE) &&
      requireNamespace("RColorBrewer", quietly = TRUE)) {
    mx <- RColorBrewer::brewer.pal.info[pal, "maxcolors"]
    if (n <= mx) return(RColorBrewer::brewer.pal(max(3L, n), pal)[seq_len(n)])
    return(grDevices::colorRampPalette(RColorBrewer::brewer.pal(mx, pal))(n))
  }
  if (identical(pal, "viridis")) return(grDevices::hcl.colors(n, "viridis"))
  if (identical(pal, "grey")) return(grDevices::grey.colors(n, start = 0.3, end = 0.9))
  if (pal %in% names(HSTAT_PALETTES_MONO))
    return(grDevices::colorRampPalette(c("#FFFFFF", HSTAT_PALETTES_MONO[[pal]]))(n + 1L)[-1L])
  scales::hue_pal()(n)
}

# Rend les echelles a ajouter au graphique, ou NULL quand il n'y a rien a poser.
hstat_scales_palette <- function(pal, fill = TRUE, colour = TRUE, ...) {
  pal <- as.character(pal %||% "")[1]
  if (is.na(pal) || !nzchar(pal)) return(NULL)
  out <- list()
  if (identical(pal, unname(HSTAT_PALETTE_GG))) {
    if (isTRUE(fill))   out <- c(out, list(ggplot2::scale_fill_hue(...)))
    if (isTRUE(colour)) out <- c(out, list(ggplot2::scale_colour_hue(...)))
    return(out)
  }
  if (!pal %in% c(HSTAT_PALETTES_QUALI, HSTAT_PALETTES_DEGRADE,
                  HSTAT_PALETTES_AUTRES)) return(NULL)
  # Une echelle discrete dont la palette RECOIT le nombre de modalites.
  echelle <- function(aes) {
    args <- list(aesthetics = aes,
                 palette = function(n) hstat_palette_couleurs(pal, n), ...)
    if (utils::packageVersion("ggplot2") < "3.5.0") args$scale_name <- "hstat"
    do.call(ggplot2::discrete_scale, args)
  }
  if (isTRUE(fill))   out <- c(out, list(echelle("fill")))
  if (isTRUE(colour)) out <- c(out, list(echelle("colour")))
  out
}

# Alignements horizontaux d'un titre.
hstat_ellipse_ok <- function(groupes, x, y, min_n = 3L) {
  vide <- list(ok = FALSE, groupes = character(0), faibles = character(0),
               motif = "Aucun groupe exploitable.")
  if (is.null(groupes) || is.null(x) || is.null(y)) return(vide)
  g <- as.character(groupes)
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  n <- min(length(g), length(x), length(y))
  if (n < min_n) return(vide)
  g <- g[seq_len(n)]; x <- x[seq_len(n)]; y <- y[seq_len(n)]
  garde <- !is.na(g) & is.finite(x) & is.finite(y)
  g <- g[garde]; x <- x[garde]; y <- y[garde]
  if (!length(g)) return(vide)

  bons <- character(0); faibles <- character(0)
  for (lv in unique(g)) {
    i <- g == lv
    xi <- x[i]; yi <- y[i]
    assez <- length(xi) >= min_n &&
             stats::sd(xi) > 1e-9 && stats::sd(yi) > 1e-9 &&
             abs(suppressWarnings(stats::cor(xi, yi))) < 1 - 1e-9
    if (isTRUE(assez)) bons <- c(bons, lv) else faibles <- c(faibles, lv)
  }
  list(ok = length(bons) > 0 && !length(faibles),
       groupes = bons, faibles = faibles,
       motif = if (!length(faibles)) NULL
               else trf("Ellipses non tracées : %s. Un groupe doit compter au moins %d individus non alignés et de coordonnées variables.",
                        paste(faibles, collapse = ", "), min_n))
}

# Groupe passe a factoextra par `habillage` : une premiere valeur "none" y
# vaut « pas de groupe », la modalite garde donc son nom a un blanc pres.
hstat_habillage <- function(x) {
  g <- factor(as.character(x))
  levels(g)[levels(g) == "none"] <- "none "
  g
}

# Deux ellipses par groupe, qui ne disent pas la meme chose : celle de la
# MOYENNE (ou se trouve le centre du groupe) et celle de la DISPERSION (ou se
# trouvent ses individus : elle en couvre environ la part du niveau).
HSTAT_ELLIPSE_TYPES <- c(
  "Dispersion des individus (ellipse normale)" = "dispersion",
  "Confiance de la moyenne du groupe"          = "confiance")

# Contours des ellipses de chaque groupe : "confiance" est celle que trace
# factoextra sous ellipse.type = "confidence", "dispersion" celle de
# ellipse.type = "norm" (ggplot2::stat_ellipse).
hstat_ellipses_confiance <- function(x, y, g, groupes = NULL, niveau = 0.95,
                                     n_points = 100L, type = "confiance") {
  type <- if (identical(type, "dispersion")) "dispersion" else "confiance"
  g <- as.character(g)
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  garde <- !is.na(g) & is.finite(x) & is.finite(y)
  g <- g[garde]; x <- x[garde]; y <- y[garde]
  if (is.null(groupes)) groupes <- unique(g)
  niveau <- suppressWarnings(as.numeric(niveau)[1])
  if (!is.finite(niveau) || niveau <= 0 || niveau >= 1) niveau <- 0.95
  t <- seq(0, 2 * pi, length.out = n_points)
  cercle <- cbind(cos(t), sin(t))
  morceaux <- lapply(groupes, function(lv) {
    i <- g == lv
    if (sum(i) < 3L) return(NULL)
    m <- cbind(x[i], y[i])
    n <- nrow(m)
    s <- if (type == "dispersion") stats::cov(m) else stats::cov(m) / n
    rayon <- if (type == "dispersion") sqrt(2 * stats::qf(niveau, 2, n - 1))
             else sqrt(stats::qchisq(niveau, df = 2))
    r <- tryCatch(chol(s), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    pts <- sweep(rayon * cercle %*% r, 2, colMeans(m), "+")
    data.frame(x = pts[, 1], y = pts[, 2], groupe = lv, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, morceaux)
  if (is.null(res)) res <- data.frame(x = numeric(0), y = numeric(0), groupe = character(0))
  res
}

# TELECHARGEMENT D'IMAGE : NE JAMAIS RENVOYER UNE PAGE HTML
# Un `content =` de downloadHandler qui leve une erreur -- ou qui se termine
# sans avoir ECRIT le fichier.
HSTAT_IMG_MIME <- c(
  png = "image/png", jpeg = "image/jpeg", jpg = "image/jpeg",
  tiff = "image/tiff", tif = "image/tiff", bmp = "image/bmp",
  svg = "image/svg+xml", pdf = "application/pdf",
  eps = "application/postscript")

# Format normalise (minuscules, alias resolus, repli sur PNG).
hstat_img_fmt <- function(x, defaut = "png") {
  f <- tolower(trimws(as.character(x)[1] %||% ""))
  f <- switch(f, "jpg" = "jpeg", "tif" = "tiff", "htm" = defaut, "html" = defaut, f)
  if (!nzchar(f) || !(f %in% names(HSTAT_IMG_MIME))) defaut else f
}

# Type MIME a annoncer au navigateur.
hstat_img_mime <- function(fmt) unname(HSTAT_IMG_MIME[hstat_img_fmt(fmt)])

# LES DEUX REGLAGES D'EXPORT NE SE DECLARENT QU'ICI
HSTAT_TIFF_COMPRESSION <- c("Aucune" = "none", "LZW" = "lzw", "ZIP" = "zip")

HSTAT_FORMATS_IMG <- c(
  "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "BMP" = "bmp",
  "PDF (vectoriel)" = "pdf", "SVG (vectoriel)" = "svg",
  "EPS (vectoriel)" = "eps")

# Selecteur de format d'export.
hstat_format_input <- function(id, label = "Format", selected = "png",
                               width = NULL) {
  # Le separateur decimal n'est PAS pose ici.
  shiny::selectInput(id, label, choices = HSTAT_FORMATS_IMG,
                     selected = selected, width = width)
}

# Champ de resolution.
hstat_dpi_input <- function(id, label = "DPI", valeur = 300, min = 72,
                            step = 50, width = NULL) {
  shiny::numericInput(id, label, value = valeur, min = min,
                      max = HSTAT_DPI_MAX, step = step, width = width)
}

# Ouvre le peripherique graphique du format demande.
.hstat_img_device <- function(file, fmt, width, height, dpi,
                              qualite = 95, compression = "lzw") {
  px_w <- max(1, round(width * dpi)); px_h <- max(1, round(height * dpi))
  q <- suppressWarnings(as.integer(qualite)[1])
  if (!isTRUE(is.finite(q)) || q < 1 || q > 100) q <- 95L
  cmp <- if (is.character(compression) && nzchar(compression[1])) compression[1] else "lzw"
  switch(fmt,
    pdf  = grDevices::pdf(file, width = width, height = height),
    eps  = grDevices::postscript(file, width = width, height = height,
                                 paper = "special", horizontal = FALSE),
    svg  = if (requireNamespace("svglite", quietly = TRUE))
             svglite::svglite(file, width = width, height = height)
           else grDevices::svg(file, width = width, height = height),
    jpeg = grDevices::jpeg(file, width = px_w, height = px_h, res = dpi,
                           quality = q, type = "cairo"),
    tiff = grDevices::tiff(file, width = px_w, height = px_h, res = dpi,
                           type = "cairo", compression = cmp),
    bmp  = grDevices::bmp(file, width = px_w, height = px_h, res = dpi,
                          type = "cairo"),
    grDevices::png(file, width = px_w, height = px_h, res = dpi, type = "cairo"))
  invisible(TRUE)
}

# Image de secours : un fichier valide du bon format, portant le motif.
hstat_image_secours <- function(file, fmt = "png", message = NULL,
                                width = 10, height = 7.5, dpi = 150) {
  fmt <- hstat_img_fmt(fmt)
  msg <- message %||% "Graphique indisponible."
  tryCatch({
    .hstat_img_device(file, fmt, width, height, max(72, dpi))
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::text(0.5, 0.5, paste(strwrap(msg, 60), collapse = "\n"),
                   cex = 1.1, col = "#c0392b")
    TRUE
  }, error = function(e) FALSE)
}

# Ecrit `plot` dans `file`, au format demande, ET GARANTIT qu'un fichier valide
# existe au retour.
HSTAT_IMG_POUCES_MIN <- 1
HSTAT_IMG_POUCES_MAX <- 200

.hstat_img_pouces <- function(v, defaut) {
  v <- suppressWarnings(as.numeric(v)[1])
  if (!isTRUE(is.finite(v)) || v <= 0) return(defaut)
  min(max(v, HSTAT_IMG_POUCES_MIN), HSTAT_IMG_POUCES_MAX)
}

hstat_ecrire_image <- function(file, plot, fmt = "png", width = 10, height = 7.5,
                               dpi = 300, echec = NULL, secours = TRUE,
                               qualite = 95, compression = "lzw",
                               decimale = hstat_decimale_session()) {
  fmt <- hstat_img_fmt(fmt)
  decimale <- .hstat_decimale(decimale)
  # La taille aussi est bornee, pas seulement la resolution : voir
  # `.hstat_img_pouces()`.
  width <- .hstat_img_pouces(width, 10); height <- .hstat_img_pouces(height, 7.5)
  # LE PLAFOND EST ICI, chez l'ecrivain commun : vingt exports en heritent sans
  # que chacun ait a y penser, et aucun ne peut l'oublier.
  if (!fmt %in% c("pdf", "svg", "eps")) {
    eff <- hstat_dpi_effectif(width, height, dpi)
    dpi <- eff$dpi
  }
  # LE PERIPHERIQUE EST FERME AVANT TOUT LE RESTE, d'ou la fonction anonyme.
  ok <- tryCatch({
    if (is.null(plot)) stop("Aucun graphique à exporter.")
    (function() {
      .hstat_img_device(file, fmt, width, height, dpi, qualite, compression)
      on.exit(grDevices::dev.off(), add = TRUE)
      if (!identical(decimale, ",")) {
        if (is.function(plot)) plot() else print(plot)
        return(invisible(NULL))
      }
      # Une figure de grille (ggplot, patchwork) est convertie APRES sa
      # construction, sur son texte : graduations, valeurs portees, equations.
      g <- if (is.function(plot)) NULL else .hstat_decimale_grob(plot)
      if (!is.null(g)) {
        grid::grid.newpage()
        grid::grid.draw(g)
      } else {
        ancien <- options(OutDec = ",")
        on.exit(options(ancien), add = TRUE)
        if (is.function(plot)) plot() else print(plot)
      }
    })()
    TRUE
  }, error = function(e) {
    if (isTRUE(secours))
      hstat_image_secours(file, fmt,
        echec %||% hstat_err_fr(e, "Export du graphique"), width, height, dpi)
    FALSE
  })
  # Un fichier absent ou vide serait servi en HTML : dernier filet.
  if (!file.exists(file) || file.size(file) == 0) {
    if (!isTRUE(secours)) {
      unlink(file)
      return(FALSE)
    }
    hstat_image_secours(file, fmt, echec %||% "Graphique indisponible.",
                        width, height, dpi)
  }
  isTRUE(ok)
}

# Habillage d'une barre : opacite, et contour seulement s'il est demande.
hstat_barre_style <- function(alpha = 0.8, contour = FALSE,
                              couleur = "#2c3e50", epaisseur = 0.5) {
  a <- suppressWarnings(as.numeric(alpha)[1])
  if (!isTRUE(is.finite(a)) || a <= 0 || a > 1) a <- 0.8
  out <- list(alpha = a)
  if (isTRUE(contour)) {
    e <- suppressWarnings(as.numeric(epaisseur)[1])
    if (!isTRUE(is.finite(e)) || e <= 0) e <- 0.5
    out$colour <- if (is.character(couleur) && nzchar(couleur[1])) couleur[1] else "#2c3e50"
    out$linewidth <- e
  }
  out
}

# Ou poser l'etiquette de valeur d'une barre : ordonnee ET calage vertical,
# ensemble, parce qu'ils ne se choisissent pas separement.
HSTAT_ALIGN_TITRE <- c("Gauche" = "0", "Centré" = "0.5", "Droite" = "1")

hstat_axe_titre <- function(size = 12, face = "plain", align = "0.5",
                            axe = c("x", "y"), retour = TRUE,
                            colour = NULL, marge = 6) {
  axe <- match.arg(axe)
  h <- suppressWarnings(as.numeric(align)[1])
  if (!isTRUE(is.finite(h))) h <- 0
  marges <- if (axe == "y") ggplot2::margin(r = marge) else ggplot2::margin(t = marge)
  # LE REPLI D'UN PAQUET ABSENT NE PEUT PAS ETRE CE PAQUET.
  if (!requireNamespace("ggtext", quietly = TRUE))
    return(ggplot2::element_text(size = size, face = face, hjust = h,
                                 colour = colour, margin = marges))
  if (!isTRUE(retour))
    return(ggtext::element_markdown(size = size, face = face, hjust = h,
                                    colour = colour, margin = marges))
  ggtext::element_textbox_simple(
    size = size, face = face, colour = colour,
    halign = h, hjust = 0.5,
    width = grid::unit(1, "npc"),
    orientation = if (axe == "y") "left-rotated" else "upright",
    margin = marges)
}

# Etendue d'un axe : les donnees, ET tout ce qu'on trace par-dessus.
hstat_axe_titre_ui <- function(ns, prefix, retour = TRUE, align = "0.5") {
  shiny::tagList(
    shiny::checkboxInput(ns(paste0(prefix, "TitreRetour")),
                  "Titres d'axe sur plusieurs lignes si trop longs", retour),
    shiny::selectInput(ns(paste0(prefix, "TitreAlign")), "Alignement des titres d'axe :",
                choices = HSTAT_ALIGN_TITRE, selected = align))
}

# Lecture des deux reglages ci-dessus, rendue prete pour `theme()`.
hstat_axe_titre_lire <- function(input, prefix, size = 12, face = "plain",
                                 axe = c("x", "y"), colour = NULL) {
  hstat_axe_titre(size = size, face = face,
                  align = input[[paste0(prefix, "TitreAlign")]] %||% "0.5",
                  axe = match.arg(axe),
                  retour = isTRUE(input[[paste0(prefix, "TitreRetour")]] %||% TRUE),
                  colour = colour)
}

# Bandeau "portee des données".
.hstat_scope_banner <- function(exact = FALSE) {
  shiny::conditionalPanel(
    condition = "output.hstatBigData == true",
    shiny::div(class = "callout callout-warning", style = "margin-bottom:16px;",
      shiny::tags$p(style = "margin:0; font-size:13px;",
        shiny::icon("database"), " ",
        # Le texte vit dans SON element : le traducteur remplace un element
        # dont le balisage entier est une entree du dictionnaire.
        shiny::span(
          if (exact)
            shiny::HTML("<b>Mode hors-mémoire.</b> Cette analyse s'exécute sur l'échantillon de travail ; l'option <b>« calculer sur le jeu complet »</b> ci-dessous fournit un résultat exact lorsque c'est applicable.")
          else
            shiny::HTML("<b>Mode hors-mémoire.</b> Cette analyse ajuste un modèle et s'exécute donc sur l'<b>échantillon de travail</b>. Pour gagner en fidélité, agrandissez l'échantillon dans l'onglet « Chargement » → « Échantillon de travail ».")))
    )
  )
}

hstat_etendue_axe <- function(valeurs, reperes = numeric(0)) {
  v <- c(as.numeric(valeurs), as.numeric(reperes))
  v <- v[is.finite(v)]
  if (!length(v)) return(c(0, 1))
  r <- range(v)
  if (r[1] == r[2]) r + c(-0.5, 0.5) else r
}

# UN PAS DE GRADUATION VIENT DU NAVIGATEUR, ET IL FIXE UN NOMBRE DE TRAITS.
HSTAT_GRADUATIONS_MAX <- 100L

hstat_graduations <- function(debut, fin, pas, aligner = TRUE,
                              max = HSTAT_GRADUATIONS_MAX) {
  num <- function(v) suppressWarnings(as.numeric(v)[1])
  debut <- num(debut); fin <- num(fin); pas <- num(pas)
  if (!isTRUE(is.finite(debut)) || !isTRUE(is.finite(fin)) ||
      !isTRUE(is.finite(pas)) || pas <= 0) return(NULL)
  if (fin < debut) { t <- debut; debut <- fin; fin <- t }
  n <- (fin - debut) / pas
  if (!is.finite(n)) return(NULL)
  if (n > max) {
    k <- n / max
    m <- 10^floor(log10(k))
    f <- k / m
    pas <- pas * m * (if (f <= 1) 1 else if (f <= 2) 2 else if (f <= 5) 5 else 10)
  }
  d <- if (isTRUE(aligner)) floor(debut / pas) * pas else debut
  b <- seq(d, fin, by = pas)
  attr(b, "pas") <- pas
  b
}

hstat_valeur_pos <- function(y, position = c("dessus", "dedans", "pied")) {
  position <- match.arg(position)
  y <- suppressWarnings(as.numeric(y))
  neg <- !is.na(y) & y < 0
  if (identical(position, "pied"))
    return(list(y = rep(0, length(y)), vjust = ifelse(neg, 1.4, -0.5)))
  list(y = y,
       vjust = if (identical(position, "dessus")) ifelse(neg, 1.4, -0.5)
               else ifelse(neg, -0.4, 1.4))
}

HSTAT_ALIGNEMENTS <- c("Centré" = "0.5", "Gauche" = "0", "Droite" = "1")

HSTAT_THEMES_GG <- c("Minimal" = "minimal", "Classique" = "classic",
                     "Noir et blanc" = "bw", "Clair" = "light",
                     "Gris" = "gray", "Sombre" = "dark",
                     "Traits fins" = "linedraw", "Sans décor" = "void")

# Les deux themes propres aux graphiques multivaries vivent ICI, avec les
# autres.
HSTAT_THEMES_MV <- c("Par défaut (ggplot2)" = "gg",
                     "HStat (titre centré, légende en bas)" = "hstat")

viz_get_theme <- function(theme_name = "minimal", base_size = 12) {
  switch(theme_name,
         "gg"       = ggplot2::theme_grey(base_size = base_size),
         "hstat"    = ggplot2::theme_minimal(base_size = base_size) +
           ggplot2::theme(
             plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold",
                                                   size = base_size + 2, colour = "#2c3e50"),
             plot.subtitle = ggplot2::element_text(hjust = 0.5, colour = "#555555",
                                                   size = base_size - 1),
             axis.text     = ggplot2::element_text(size = base_size - 1),
             legend.text   = ggplot2::element_text(size = base_size - 1),
             legend.title  = ggplot2::element_text(size = base_size, face = "bold"),
             legend.position  = "bottom",
             panel.grid.minor = ggplot2::element_blank()),
         "minimal"  = ggplot2::theme_minimal( base_size = base_size),
         "classic"  = ggplot2::theme_classic( base_size = base_size),
         "bw"       = ggplot2::theme_bw(      base_size = base_size),
         "light"    = ggplot2::theme_light(   base_size = base_size),
         "gray"     = ggplot2::theme_gray(    base_size = base_size),
         "dark"     = ggplot2::theme_dark(    base_size = base_size),
         "void"     = ggplot2::theme_void(    base_size = base_size),
         "linedraw" = ggplot2::theme_linedraw(base_size = base_size),
         ggplot2::theme_minimal(base_size = base_size)
  )
}

viz_get_x_scale <- function(x_col, disp_fmt = "%d-%m-%Y", label_map = NULL, custom_ord = NULL) {
  if (inherits(x_col, "AsIs")) {
    cls <- class(x_col)
    cls <- cls[cls != "AsIs"]
    x_col <- unclass(x_col)
    class(x_col) <- cls
  }
  x_is_date    <- inherits(x_col, c("Date", "POSIXct", "POSIXlt"))
  x_is_numeric <- is.numeric(x_col) && !x_is_date
  has_lm <- !is.null(label_map) && length(label_map) > 0 &&
    any(as.character(label_map) != names(label_map))
  
  if (x_is_date) {
    all_x <- sort(unique(x_col))
    if (!is.null(custom_ord) && length(custom_ord) > 0) {
      co_dates <- tryCatch(as.Date(custom_ord), error = function(e) NULL)
      if (!is.null(co_dates)) {
        valid_ord <- co_dates[!is.na(co_dates) & co_dates %in% all_x]
        if (length(valid_ord) > 0) all_x <- valid_ord
      }
    }
    labels_vec <- sapply(as.character(all_x), function(v) {
      if (has_lm && v %in% names(label_map) && as.character(label_map[[v]]) != v)
        as.character(label_map[[v]])
      else
        # `hstat_date_fmt` et non `format`.
        tryCatch(hstat_date_fmt(as.Date(v), disp_fmt), error = function(e) v)
    }, USE.NAMES = FALSE)
    return(ggplot2::scale_x_date(
      breaks = all_x, labels = labels_vec,
      guide  = ggplot2::guide_axis(check.overlap = TRUE)
    ))
  } else if (x_is_numeric) {
    all_x <- sort(unique(x_col))
    if (!is.null(custom_ord) && length(custom_ord) > 0) {
      co_num <- suppressWarnings(as.numeric(custom_ord))
      valid  <- co_num[!is.na(co_num) & co_num %in% all_x]
      if (length(valid) > 0) all_x <- valid
    }
    if (has_lm) {
      labels_vec <- sapply(as.character(all_x), function(v) {
        if (v %in% names(label_map) && as.character(label_map[[v]]) != v)
          as.character(label_map[[v]])
        else v
      }, USE.NAMES = FALSE)
    } else {
      labels_vec <- ggplot2::waiver()
    }
    return(ggplot2::scale_x_continuous(
      breaks = all_x, labels = labels_vec,
      guide  = ggplot2::guide_axis(check.overlap = TRUE)
    ))
  } else {
    if (is.factor(x_col)) {
      all_x <- levels(x_col)
    } else {
      all_x <- sort(unique(as.character(x_col)))
    }
    if (!is.null(custom_ord) && length(custom_ord) > 0) {
      ord_valid <- custom_ord[custom_ord %in% all_x]
      if (length(ord_valid) > 0) all_x <- ord_valid
    }
    # UNE DATE RESTE UNE DATE MEME PRESENTEE EN DISCRET.
    lab_disc <- vapply(all_x, function(v) {
      if (has_lm && v %in% names(label_map) &&
          as.character(label_map[[v]]) != v) return(as.character(label_map[[v]]))
      if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", v)) return(v)
      dt <- tryCatch(as.Date(v, format = "%Y-%m-%d"), error = function(e) NA)
      if (length(dt) != 1L || is.na(dt)) return(v)
      tryCatch(hstat_date_fmt(dt, disp_fmt), error = function(e) v)
    }, character(1), USE.NAMES = FALSE)
    return(ggplot2::scale_x_discrete(limits = all_x, labels = lab_disc,
                                     drop = FALSE))
  }
}

viz_label_params <- function(size = 3, color = "#333333", bold = FALSE,
                             italic = FALSE, digits = 2, position = "above") {
  fontface <- if (bold && italic) "bold.italic"
  else if (bold)   "bold"
  else if (italic) "italic"
  else             "plain"
  vjust <- switch(position,
                  "above"  = -0.5, "below"  =  1.5,
                  "center" =  0.5, "right"  =  0.5, "left" = 0.5, -0.5)
  hjust <- switch(position, "right" = -0.2, "left" = 1.2, 0.5)
  plotly_pos <- switch(position,
                       "above"  = "top center",    "below"  = "bottom center",
                       "center" = "middle center", "right"  = "middle right",
                       "left"   = "middle left",   "top center")
  list(size = size, color = color, fontface = fontface,
       digits = digits, vjust = vjust, hjust = hjust,
       plotly_textpos = plotly_pos)
}

# MANOVA — Fonctions utilitaires (paramétrique + non paramétrique) -
# Vérification des prérequis multivariés (n, p, completeness) - Normalité
# multivariée (Mardia via psych) - Homogénéité des matrices de covariance
# (Box's M via heplots) - Homogénéité multivariée des dispersions (PERMDISP via
# vegan::betadisper) - Mise en forme des résultats parametric (4 statistiques)
# - PERMANOVA (vegan::adonis2) + pairwise PERMANOVA - Décomposition univariée
# post-hoc (ANOVA / Kruskal) avec ajustement

#' Vérifie que les données sont utilisables pour une MANOVA
#' @param df         data.frame
#' @param response   character — variables réponses (>= 2)
#' @param factors    character — facteurs (>= 1)
#' @return           list(ok, message, df_clean, n, p, k)
check_manova_data <- function(df, response, factors) {
  if (length(response) < 2)
    return(list(ok = FALSE, message = "MANOVA nécessite au moins 2 variables réponses."))
  if (length(factors) < 1)
    return(list(ok = FALSE, message = "MANOVA nécessite au moins 1 facteur."))
  
  keep <- c(response, factors)
  df2  <- df[, keep, drop = FALSE]
  
  for (v in response) {
    if (!is.numeric(df2[[v]])) df2[[v]] <- suppressWarnings(as.numeric(df2[[v]]))
  }
  
  for (f in factors) {
    if (!is.factor(df2[[f]])) {
      df2[[f]] <- tryCatch({
        if (inherits(df2[[f]], c("Date","POSIXct","POSIXlt")))
          factor(format(df2[[f]], "%Y-%m-%d"))
        else
          factor(as.character(df2[[f]]))
      }, error = function(e) factor(df2[[f]]))
    }
    df2[[f]] <- droplevels(df2[[f]])
  }
  
  df2 <- df2[stats::complete.cases(df2), , drop = FALSE]
  
  n <- nrow(df2)
  p <- length(response)
  if (n < (p + 3))
    return(list(ok = FALSE,
                message = trf("Trop peu d'observations complètes (n=%s) pour %s variables réponses.", n, p)))
  
  # Variance non nulle pour chaque réponse globalement et par groupe
  zero_var <- vapply(response, function(v) {
    stats::sd(df2[[v]], na.rm = TRUE) == 0 ||
      is.na(stats::sd(df2[[v]], na.rm = TRUE))
  }, logical(1))
  if (any(zero_var))
    return(list(ok = FALSE,
                message = trf("Variance nulle pour : %s", paste(response[zero_var], collapse = ", "))))
  
  # Au moins 2 niveaux par facteur, et chaque cellule >= 2 obs
  for (f in factors) {
    if (nlevels(df2[[f]]) < 2)
      return(list(ok = FALSE,
                  message = trf("Le facteur '%s' a moins de 2 niveaux après nettoyage.", f)))
  }
  
  list(ok = TRUE, message = "Données valides",
       df_clean = df2, n = n, p = p, k = length(factors))
}

#' Test de normalité multivariée de Mardia (skewness + kurtosis)
#' @param Y matrix/data.frame numérique
#' @return list(skewness, p.skewness, kurtosis, p.kurtosis, n, p, conclusion)
multivariate_normality_mardia <- function(Y) {
  Y <- as.matrix(Y)
  Y <- Y[stats::complete.cases(Y), , drop = FALSE]
  n <- nrow(Y); p <- ncol(Y)
  
  if (n < 8 || p < 2) {
    return(list(method = "Mardia",
                skewness = NA_real_, p.skewness = NA_real_,
                kurtosis = NA_real_, p.kurtosis = NA_real_,
                n = n, p = p,
                conclusion = "Échantillon trop petit pour Mardia (n < 8 ou p < 2)"))
  }
  
  res <- tryCatch(suppressWarnings(psych::mardia(Y, plot = FALSE)),
                  error = function(e) NULL)
  if (is.null(res))
    return(list(method = "Mardia",
                skewness = NA_real_, p.skewness = NA_real_,
                kurtosis = NA_real_, p.kurtosis = NA_real_,
                n = n, p = p,
                conclusion = "Test de Mardia indisponible"))
  
  ok_skew <- isTRUE(res$p.skew >= 0.05)
  ok_kurt <- isTRUE(res$p.kurt >= 0.05)
  concl <- if (ok_skew && ok_kurt)
    "Normalité multivariée plausible (Mardia : p.skew >= 0.05 et p.kurt >= 0.05)"
  else if (!ok_skew && !ok_kurt)
    "Violation de normalité multivariée (skewness ET kurtosis significatifs)"
  else if (!ok_skew)
    "Violation par asymétrie multivariée (Mardia p.skew < 0.05)"
  else
    "Violation par aplatissement multivarié (Mardia p.kurt < 0.05)"
  
  list(method = "Mardia",
       skewness   = as.numeric(res$skew),
       p.skewness = as.numeric(res$p.skew),
       kurtosis   = as.numeric(res$kurtosis),
       p.kurtosis = as.numeric(res$p.kurt),
       n = n, p = p,
       conclusion = concl)
}

#' Test de Box's M (homogénéité des matrices de covariance entre groupes)
#' @param Y matrix/data.frame numérique
#' @param group facteur (1 seul facteur)
#' @return list(chi2, df, p.value, conclusion)
box_m_test <- function(Y, group) {
  Y <- as.matrix(Y); group <- as.factor(group)
  ok <- stats::complete.cases(Y) & !is.na(group)
  Y <- Y[ok, , drop = FALSE]; group <- droplevels(group[ok])
  
  if (nlevels(group) < 2 || nrow(Y) < 5 || ncol(Y) < 2)
    return(list(chi2 = NA_real_, df = NA_real_, p.value = NA_real_,
                conclusion = "Test impossible (< 2 groupes / trop peu d'obs / p < 2)"))
  
  min_per_group <- min(table(group))
  if (min_per_group < ncol(Y) + 1)
    return(list(chi2 = NA_real_, df = NA_real_, p.value = NA_real_,
                conclusion = trf("Groupes trop petits pour Box's M (min n=%s < p+1=%s)", min_per_group, ncol(Y) + 1)))
  
  # Vérifier que chaque matrice de covariance de groupe est de rang plein.
  p <- ncol(Y)
  rank_ok <- vapply(split(seq_len(nrow(Y)), group), function(idx) {
    sub <- Y[idx, , drop = FALSE]
    cov_sub <- tryCatch(stats::cov(sub), error = function(e) NULL)
    if (is.null(cov_sub) || any(!is.finite(cov_sub))) return(FALSE)
    rg <- tryCatch(qr(cov_sub)$rank, error = function(e) NA_integer_)
    !is.na(rg) && rg >= p
  }, logical(1))
  
  if (any(!rank_ok))
    return(list(chi2 = NA_real_, df = NA_real_, p.value = NA_real_,
                conclusion = "Matrice de covariance singulière dans au moins un groupe (variables colinéaires ou n trop petit) -- Box's M non applicable"))
  
  res <- tryCatch(
    withCallingHandlers(
      heplots::boxM(Y, group),
      warning = function(w) {
        if (grepl("NaN|log|det", conditionMessage(w), ignore.case = TRUE))
          invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  if (is.null(res) || is.na(res$p.value))
    return(list(chi2 = NA_real_, df = NA_real_, p.value = NA_real_,
                conclusion = "Test de Box's M indisponible (matrices mal conditionnées)"))
  
  concl <- if (isTRUE(res$p.value >= 0.05))
    "Homogénéité des matrices de covariance OK (Box's M p >= 0.05)"
  else
    "Violation d'homogénéité (Box's M p < 0.05) — privilégier Pillai (robuste)"
  
  list(chi2 = unname(res$statistic),
       df = unname(res$parameter),
       p.value = unname(res$p.value),
       conclusion = concl)
}

#' PERMDISP — homogénéité multivariée des dispersions (vegan::betadisper)
#' Équivalent multivarié non paramétrique du test de Levene
#' @param Y matrix de réponses
#' @param group facteur
#' @param dist_method "euclidean" (par défaut) ou autre
#' @return list(F, df1, df2, p.value, conclusion)
permdisp_test <- function(Y, group, dist_method = "euclidean") {
  Y <- as.matrix(Y); group <- as.factor(group)
  ok <- stats::complete.cases(Y) & !is.na(group)
  Y <- Y[ok, , drop = FALSE]; group <- droplevels(group[ok])
  
  if (nlevels(group) < 2 || nrow(Y) < 5)
    return(list(F = NA_real_, df1 = NA_real_, df2 = NA_real_,
                p.value = NA_real_, conclusion = "Test impossible"))
  
  cap <- hstat_cap_Y_group(Y, group, what = "Homogénéité des dispersions (betadisper)")
  Y <- cap$Y; group <- cap$group
  res <- tryCatch({
    d  <- vegan::vegdist(Y, method = dist_method)
    bd <- vegan::betadisper(d, group)
    pa <- vegan::permutest(bd, permutations = 999)
    list(F   = pa$tab[1, "F"],
         df1 = pa$tab[1, "Df"],
         df2 = pa$tab[2, "Df"],
         p   = pa$tab[1, "Pr(>F)"])
  }, error = function(e) NULL)
  
  if (is.null(res))
    return(list(F = NA_real_, df1 = NA_real_, df2 = NA_real_,
                p.value = NA_real_, conclusion = "PERMDISP indisponible"))
  
  concl <- if (isTRUE(res$p >= 0.05))
    "Dispersions multivariées homogènes (PERMDISP p >= 0.05)"
  else
    "Dispersions multivariées hétérogènes (PERMDISP p < 0.05) — interpréter PERMANOVA avec prudence"
  
  list(F = res$F, df1 = res$df1, df2 = res$df2,
       p.value = res$p, conclusion = concl)
}

#' Format les 4 statistiques MANOVA en data.frame "wide"
#' @param fit modèle manova
#' @return data.frame avec colonnes Effet, ddl_num, ddl_den + 4 blocs (stat/F/p)
manova_format_all_stats <- function(fit) {
  pillai <- summary(fit, test = "Pillai")$stats
  wilks  <- summary(fit, test = "Wilks")$stats
  hotell <- summary(fit, test = "Hotelling-Lawley")$stats
  roy    <- summary(fit, test = "Roy")$stats
  
  effects <- rownames(pillai)
  effects <- effects[effects != "Residuals"]
  
  do.call(rbind, lapply(effects, function(eff) {
    data.frame(
      Effet         = eff,
      ddl_num       = pillai[eff, "Df"],
      ddl_den       = pillai["Residuals", "Df"],
      Pillai        = pillai[eff, "Pillai"],
      F_Pillai      = pillai[eff, "approx F"],
      p_Pillai      = pillai[eff, "Pr(>F)"],
      Wilks         = wilks[eff,  "Wilks"],
      F_Wilks       = wilks[eff,  "approx F"],
      p_Wilks       = wilks[eff,  "Pr(>F)"],
      Hotelling     = hotell[eff, "Hotelling-Lawley"],
      F_Hotelling   = hotell[eff, "approx F"],
      p_Hotelling   = hotell[eff, "Pr(>F)"],
      Roy           = roy[eff,    "Roy"],
      F_Roy         = roy[eff,    "approx F"],
      p_Roy         = roy[eff,    "Pr(>F)"],
      stringsAsFactors = FALSE
    )
  }))
}

#' Tailles d'effet multivariées (eta² partiel) à partir de Wilks
#' Formule : eta²_partiel = 1 - Wilks^(1/s)  où s = min(p, ddl_effet)
#' @param df data.frame produit par manova_format_all_stats
#' @param p nombre de réponses
manova_effect_sizes <- function(df, p) {
  s <- pmin(p, df$ddl_num)
  # s = 0 -- un degré de liberté dégénéré -- donnerait Wilks^(1/0), soit
  # Wilks^Inf, donc eta² = 1.
  s[!is.finite(s) | s <= 0] <- NA_real_
  df$eta2_partial <- 1 - df$Wilks^(1 / s)
  df$eta2_pillai  <- df$Pillai / s   # eta² partiel basé sur Pillai
  df
}

#' Interprétation textuelle d'un effet MANOVA paramétrique
#' @param p_pillai  p-value de Pillai
#' @param eta2      eta² partiel (optionnel)
interpret_manova_effect <- function(p_pillai, eta2 = NA) {
  if (is.na(p_pillai)) return("Résultat non disponible")
  sig <- if (p_pillai < 0.05) "significatif" else "non significatif"
  base <- trf("Effet multivarié %s (Pillai, p = %s)", sig, round(p_pillai, 6))
  if (!is.na(eta2)) {
    # « faible » vaut *small* pour une taille d'effet et *weak* pour une force
    # d'association.
    base <- paste0(base,
      if (eta2 < 0.01) trf(" — taille d'effet négligeable (eta² = %s)", round(eta2, 3))
      else if (eta2 < 0.06) trf(" — taille d'effet faible (eta² = %s)", round(eta2, 3))
      else if (eta2 < 0.14) trf(" — taille d'effet modérée (eta² = %s)", round(eta2, 3))
      else trf(" — taille d'effet importante (eta² = %s)", round(eta2, 3)))
  }
  base
}

interpret_permanova_effect <- function(p_value, R2 = NA) {
  if (is.na(p_value)) return("Résultat non disponible")
  sig <- if (p_value < 0.05) "significatif" else "non significatif"
  base <- trf("Effet multivarié %s (PERMANOVA, p = %s)", sig, round(p_value, 6))
  if (!is.na(R2)) {
    base <- paste0(base,
      if (R2 < 0.01) trf(" — R² = %s (taille d'effet négligeable)", round(R2, 3))
      else if (R2 < 0.06) trf(" — R² = %s (taille d'effet faible)", round(R2, 3))
      else if (R2 < 0.14) trf(" — R² = %s (taille d'effet modérée)", round(R2, 3))
      else trf(" — R² = %s (taille d'effet importante)", round(R2, 3)))
  }
  base
}

#' PERMANOVA pairwise sur les niveaux d'un facteur
#' Implémentation manuelle (adonis2 sur chaque paire) avec correction de p-values
#' @param Y matrice de réponses
#' @param group facteur (1 seul)
#' @param permutations nombre de permutations
#' @param dist_method  méthode de distance (par défaut "euclidean")
#' @param p_adjust     "bonferroni" (défaut), "holm", "BH", "fdr"...
#' @return data.frame avec colonnes : Niveau1, Niveau2, n1, n2, F, R2, p_value, p_adj
pairwise_permanova <- function(Y, group, permutations = 999,
                               dist_method = "euclidean",
                               p_adjust = "bonferroni") {
  Y <- as.matrix(Y); group <- as.factor(group)
  ok <- stats::complete.cases(Y) & !is.na(group)
  Y <- Y[ok, , drop = FALSE]; group <- droplevels(group[ok])
  
  levs  <- levels(group)
  pairs <- utils::combn(levs, 2, simplify = FALSE)
  
  out <- do.call(rbind, lapply(pairs, function(pr) {
    idx  <- group %in% pr
    Yp   <- Y[idx, , drop = FALSE]
    gp   <- droplevels(group[idx])
    # DEUX OBSERVATIONS AU MOINS DANS CHAQUE GROUPE.
    if (length(unique(gp)) < 2 || nrow(Yp) < 4 || min(table(gp)) < 2) {
      return(data.frame(Niveau1 = pr[1], Niveau2 = pr[2],
                        n1 = sum(group == pr[1]),
                        n2 = sum(group == pr[2]),
                        F  = NA_real_, R2 = NA_real_, p_value = NA_real_))
    }
    capp <- hstat_cap_Y_group(Yp, gp, what = "PERMANOVA par paires")
    Yp <- capp$Y; gp <- capp$group
    res <- tryCatch({
      d  <- vegan::vegdist(Yp, method = dist_method)
      a  <- vegan::adonis2(d ~ gp, permutations = permutations, by = "terms")
      data.frame(Niveau1 = pr[1], Niveau2 = pr[2],
                 n1 = sum(gp == pr[1]), n2 = sum(gp == pr[2]),
                 F  = a$F[1], R2 = a$R2[1], p_value = a$`Pr(>F)`[1])
    }, error = function(e) {
      data.frame(Niveau1 = pr[1], Niveau2 = pr[2],
                 n1 = sum(group == pr[1]), n2 = sum(group == pr[2]),
                 F = NA_real_, R2 = NA_real_, p_value = NA_real_)
    })
    res
  }))
  
  out$p_adj <- stats::p.adjust(out$p_value, method = p_adjust)
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}

#' Box's M par facteur (applique box_m_test à chaque facteur d'un design)
#' @return data.frame : Facteur, Chi2, ddl, p_value, Conclusion (NULL si aucun)
boxm_per_factor <- function(Y, df, factors) {
  rows <- lapply(factors, function(f) {
    bm <- box_m_test(Y, df[[f]])
    data.frame(
      Facteur    = f,
      Chi2       = bm$chi2,
      ddl        = bm$df,
      p_value    = bm$p.value,
      Conclusion = bm$conclusion,
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

#' PERMDISP par facteur (applique permdisp_test à chaque facteur d'un design)
#' @return data.frame : Facteur, F_stat, ddl1, ddl2, p_value, Conclusion (NULL si aucun)
permdisp_per_factor <- function(Y, df, factors, dist_method = "euclidean") {
  rows <- lapply(factors, function(f) {
    pd <- permdisp_test(Y, df[[f]], dist_method = dist_method)
    data.frame(
      Facteur    = f,
      F_stat     = pd$F,
      ddl1       = pd$df1,
      ddl2       = pd$df2,
      p_value    = pd$p.value,
      Conclusion = pd$conclusion,
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

# LE TEST GLOBAL D'ABORD : un post-hoc n'est pas un test independant.
HSTAT_POSTHOC_WELCH <- c("games")

# LES DEUX TESTS DE WELCH
# Welch ne suppose pas les variances egales.
HSTAT_TESTS_WELCH <- c("t_welch", "anova_welch")

# Nettoie et verifie un couple (reponse numerique, facteur) avant tout test a
# deux echantillons ou plus.
.hstat_deux_ech <- function(df, var, fvar, min_groupes = 2L, max_groupes = Inf) {
  ko <- function(m) list(ok = FALSE, message = m)
  if (!is.data.frame(df) || !NROW(df)) return(ko(tr("Aucune donnée : chargez un jeu de données.")))
  if (!all(c(var, fvar) %in% names(df)))
    return(ko(tr("Variable réponse ou facteur introuvable dans les données.")))
  y <- suppressWarnings(as.numeric(df[[var]]))
  g <- droplevels(factor(as.character(df[[fvar]])))
  ok <- is.finite(y) & !is.na(g)
  y <- y[ok]; g <- droplevels(g[ok])
  k <- nlevels(g)
  if (k < min_groupes)
    return(ko(trf("« %s » ne distingue que %d groupe(s) exploitable(s) : il en faut au moins %d.",
                  fvar, k, min_groupes)))
  if (k > max_groupes)
    return(ko(trf("« %s » compte %d modalités (%s). Ce test en compare exactement %d ; au-delà, passez à l'ANOVA.",
                  fvar, k, paste(utils::head(levels(g), 5), collapse = ", "), max_groupes)))
  # UN GROUPE A UNE SEULE OBSERVATION N'A PAS DE VARIANCE, et c'est justement
  # la variance par groupe que Welch estime separement.
  n_g <- as.integer(table(g))
  if (any(n_g < 2L))
    return(ko(trf("Chaque groupe doit compter au moins deux observations ; %s n'en a qu'une.",
                  paste(levels(g)[n_g < 2L], collapse = ", "))))
  list(ok = TRUE, y = y, g = g, k = k, n = n_g)
}

# Test t a deux echantillons.
hstat_t_deux <- function(df, var, fvar, var_equal = FALSE, conf_level = 0.95) {
  nom <- if (isTRUE(var_equal)) "Test t de Student" else "Test t de Welch"
  vide <- list(p = NA_real_, statistique = NA_real_, ddl = NA_real_,
               test = nom, message = NULL, ic = c(NA_real_, NA_real_),
               moyennes = NULL)
  pre <- .hstat_deux_ech(df, var, fvar, min_groupes = 2L, max_groupes = 2L)
  if (!isTRUE(pre$ok)) { vide$message <- pre$message; return(vide) }
  r <- tryCatch(
    stats::t.test(pre$y ~ pre$g, var.equal = isTRUE(var_equal),
                  conf.level = conf_level),
    error = function(e) NULL)
  if (is.null(r) || !isTRUE(is.finite(r$p.value))) {
    vide$message <- tr("Test t non calculable sur ces données.")
    return(vide)
  }
  list(p = unname(r$p.value), statistique = unname(r$statistic),
       # LE DDL DE WELCH EST FRACTIONNAIRE : on ne l'arrondit pas.
       ddl = unname(r$parameter), test = nom, message = NULL,
       ic = unname(r$conf.int), moyennes = unname(r$estimate),
       niveaux = levels(pre$g), n = pre$n)
}

# ANOVA de Welch a un facteur.
hstat_anova_welch <- function(df, var, fvar) {
  vide <- list(p = NA_real_, statistique = NA_real_, ddl1 = NA_real_,
               ddl2 = NA_real_, test = "ANOVA de Welch", message = NULL)
  pre <- .hstat_deux_ech(df, var, fvar, min_groupes = 2L)
  if (!isTRUE(pre$ok)) { vide$message <- pre$message; return(vide) }
  r <- tryCatch(stats::oneway.test(pre$y ~ pre$g, var.equal = FALSE),
                error = function(e) NULL)
  if (is.null(r) || !isTRUE(is.finite(r$p.value))) {
    vide$message <- tr("ANOVA de Welch non calculable sur ces données.")
    return(vide)
  }
  list(p = unname(r$p.value), statistique = unname(r$statistic),
       ddl1 = unname(r$parameter[1]), ddl2 = unname(r$parameter[2]),
       test = "ANOVA de Welch", message = NULL,
       niveaux = levels(pre$g), n = pre$n)
}

# Ecrit un degre de liberte : entier quand il l'est, deux decimales sinon.
hstat_ddl_fmt <- function(x, dec = 2L, max_dec = 6L) {
  if (!length(x) || !isTRUE(is.finite(x[1]))) return(NA_character_)
  x <- x[1]
  if (isTRUE(all.equal(x, round(x)))) return(format(round(x)))
  # UN DDL FRACTIONNAIRE QUI S'AFFICHE « 11.00 » SE LIT COMME UN ENTIER.
  for (d in seq.int(dec, max_dec)) {
    v <- round(x, d)
    if (!isTRUE(all.equal(v, round(v)))) return(format(v, nsmall = d))
  }
  format(round(x, max_dec), nsmall = max_dec)
}

hstat_omnibus_variances <- function(methode) {
  if (length(methode) != 1L || is.na(methode)) return("egales")
  if (as.character(methode) %in% HSTAT_POSTHOC_WELCH) "inegales" else "egales"
}

hstat_omnibus <- function(df, var, fvar, parametric = TRUE,
                          variances = c("egales", "inegales")) {
  variances <- match.arg(variances)
  vide <- list(p = NA_real_, test = NA_character_, statistique = NA_real_,
               ddl = NA_character_, message = NULL)
  if (!is.data.frame(df) || !NROW(df)) return(vide)
  if (!all(c(var, fvar) %in% names(df))) return(vide)
  y <- suppressWarnings(as.numeric(df[[var]]))
  g <- droplevels(factor(as.character(df[[fvar]])))
  ok <- is.finite(y) & !is.na(g)
  y <- y[ok]; g <- droplevels(g[ok])
  if (nlevels(g) < 2 || length(y) < 3) {
    vide$message <- tr("Test global non calculable : moins de deux groupes exploitables.")
    return(vide)
  }
  if (isTRUE(parametric) && identical(variances, "inegales")) {
    # ANOVA DE WELCH : la reference des methodes a variances inegales.
    r <- tryCatch({
      w <- stats::oneway.test(y ~ g, var.equal = FALSE)
      list(p = unname(w$p.value), test = "ANOVA de Welch",
           statistique = unname(w$statistic),
           ddl = paste(signif(unname(w$parameter[1]), 4), ",",
                       signif(unname(w$parameter[2]), 4)),
           message = NULL)
    }, error = function(e) NULL)
  } else if (isTRUE(parametric)) {
    r <- tryCatch({
      s <- summary(stats::aov(y ~ g))[[1]]
      list(p = s[["Pr(>F)"]][1], test = "ANOVA", statistique = s[["F value"]][1],
           ddl = paste(s[["Df"]][1], ",", s[["Df"]][2]), message = NULL)
    }, error = function(e) NULL)
  } else {
    r <- tryCatch({
      k <- stats::kruskal.test(y ~ g)
      list(p = unname(k$p.value), test = "Kruskal-Wallis",
           statistique = unname(k$statistic), ddl = as.character(unname(k$parameter)),
           message = NULL)
    }, error = function(e) NULL)
  }
  if (is.null(r) || !isTRUE(is.finite(r$p))) {
    vide$message <- tr("Test global non calculable sur ces données.")
    return(vide)
  }
  r
}

# Applique la protection : sans test global significatif, TOUTES les modalites
# partagent la meme lettre.
hstat_cld_proteger <- function(lettres, p_omnibus, seuil = 0.05,
                               test = NULL) {
  lettres <- as.character(lettres)
  if (!length(lettres)) return(lettres)
  if (!isTRUE(is.finite(p_omnibus))) {
    attr(lettres, "protege") <- FALSE
    attr(lettres, "message") <- tr("Test global non calculable : les lettres sont laissées telles quelles.")
    return(lettres)
  }
  if (p_omnibus <= seuil) {
    attr(lettres, "protege") <- FALSE
    return(lettres)
  }
  aplat <- rep("a", length(lettres))
  names(aplat) <- names(lettres)
  attr(aplat, "protege") <- TRUE
  attr(aplat, "message") <- trf(
    "%s non significatif (p = %s) : aucune différence entre modalités, toutes reçoivent la même lettre. Décochez « post-hoc protégé » pour voir les comparaisons par paires malgré tout.",
    test %||% tr("Test global"), format(signif(p_omnibus, 4), scientific = FALSE))
  aplat
}

# UNE DEMI-MATRICE DE PMCMRplus NE PORTE PAS TOUS LES NIVEAUX.
hstat_pmat_demi <- function(p, niveaux) {
  niveaux <- as.character(niveaux)
  n <- length(niveaux)
  out <- matrix(1, n, n, dimnames = list(niveaux, niveaux))
  if (is.null(p) || !length(p)) return(out)
  p <- as.matrix(p)
  rn <- rownames(p); cn <- colnames(p)
  if (is.null(rn) || is.null(cn)) return(out)
  manquants <- character(0)
  for (r in rn) for (cc in cn) {
    v <- p[r, cc]
    if (!isTRUE(is.finite(v))) next
    if (!(r %in% niveaux) || !(cc %in% niveaux)) {
      manquants <- c(manquants, paste(r, cc, sep = " - ")); next
    }
    out[r, cc] <- v
    out[cc, r] <- v
  }
  diag(out) <- 1
  # CE QUI N'A PAS PU ETRE RATTACHE EST NOMME.
  if (length(manquants)) attr(out, "non_resolues") <- unique(manquants)
  out
}

# UNE ETIQUETTE DE COMPARAISON NE SE DECOUPE PAS SUR SON SEPARATEUR.
hstat_paire_niveaux <- function(etiquette, niveaux, sep = "-") {
  et <- trimws(as.character(etiquette)[1])
  niveaux <- as.character(niveaux)
  if (is.na(et) || !nzchar(et) || !length(niveaux)) return(NULL)
  pos <- gregexpr(sep, et, fixed = TRUE)[[1]]
  if (pos[1] == -1L) return(NULL)
  trouves <- list()
  for (k in pos) {
    gauche <- trimws(substr(et, 1L, k - 1L))
    droite <- trimws(substr(et, k + nchar(sep), nchar(et)))
    if (gauche %in% niveaux && droite %in% niveaux)
      trouves[[length(trouves) + 1L]] <- c(gauche, droite)
  }
  # ZERO coupe valide : l'etiquette ne parle pas de ces niveaux.
  if (length(trouves) != 1L) return(NULL)
  trouves[[1L]]
}

# Matrice de p-values a partir d'etiquettes de comparaison.
hstat_pmat_comparaisons <- function(etiquettes, p, niveaux, sep = "-") {
  niveaux <- as.character(niveaux)
  pmat <- matrix(1, length(niveaux), length(niveaux),
                 dimnames = list(niveaux, niveaux))
  non_resolues <- character(0)
  for (i in seq_along(etiquettes)) {
    paire <- hstat_paire_niveaux(etiquettes[i], niveaux, sep)
    if (is.null(paire)) {
      non_resolues <- c(non_resolues, as.character(etiquettes[i]))
      next
    }
    pmat[paire[1], paire[2]] <- p[i]
    pmat[paire[2], paire[1]] <- p[i]
  }
  diag(pmat) <- 1
  if (length(non_resolues)) attr(pmat, "non_resolues") <- non_resolues
  pmat
}

#' Construit la matrice de p-values à partir d'un data.frame de paires
#' (utilisé pour générer les lettres CLD via multcompView)
#' @param pairs_df data.frame avec colonnes Niveau1, Niveau2, p_adj
#' @param levels   vecteur des niveaux du facteur
#' @return matrix carrée de p-values (diagonale = 1)
build_pvalue_matrix <- function(pairs_df, levels) {
  pmat <- matrix(NA_real_, length(levels), length(levels),
                 dimnames = list(levels, levels))
  for (i in seq_len(nrow(pairs_df))) {
    a <- pairs_df$Niveau1[i]; b <- pairs_df$Niveau2[i]
    pmat[a, b] <- pairs_df$p_adj[i]
    pmat[b, a] <- pairs_df$p_adj[i]
  }
  diag(pmat) <- 1
  pmat
}

#' PostHoc univarie par variable reponse (decomposition d'un posthoc multivarie)
#'
#' Pour chaque variable reponse, lance un posthoc adapte :
#' - methode parametrique  -> ANOVA + Tukey HSD
#' - methode non parametrique -> Kruskal-Wallis + Dunn (Bonferroni)
#' puis derive les lettres CLD propres a cette variable.
#'
#' @param df       data.frame nettoye
#' @param response variables reponses (vecteur)
#' @param factor_name un seul facteur
#' @param parametric TRUE = ANOVA/Tukey, FALSE = Kruskal/Dunn
#' @param digits   decimales d'affichage
#' @return data.frame long : Variable, Niveau, N, Moyenne, Ecart_type, Erreur_type,
#'         Groupes, Moyenne_pm_SD, Moyenne_pm_SE
build_letters_per_variable <- function(df, response, factor_name,
                                       parametric = TRUE, digits = 3) {
  if (length(factor_name) != 1) return(NULL)
  fvar <- factor_name
  df <- df[stats::complete.cases(df[, c(response, fvar), drop = FALSE]), , drop = FALSE]
  if (!is.factor(df[[fvar]])) df[[fvar]] <- factor(as.character(df[[fvar]]))
  df[[fvar]] <- droplevels(df[[fvar]])
  levs <- levels(df[[fvar]])
  if (length(levs) < 2) return(NULL)
  
  fmt_val <- function(m, s) ifelse(is.na(m) | is.na(s), "NA",
                                   paste0(formatC(m, digits = digits, format = "f"),
                                          " \u00b1 ",
                                          formatC(s, digits = digits, format = "f")))
  
  rows <- list()
  for (v in response) {
    y  <- df[[v]]
    g  <- df[[fvar]]
    n_per   <- as.numeric(table(g))[match(levs, levels(g))]
    means   <- vapply(levs, function(lv) mean(y[g == lv], na.rm = TRUE), numeric(1))
    sds     <- vapply(levs, function(lv) stats::sd(y[g == lv], na.rm = TRUE), numeric(1))
    ses     <- sds / sqrt(pmax(n_per, 1))
    
    letters_v <- tryCatch({
      if (parametric) {
        fit  <- stats::aov(y ~ g)
        tuk  <- stats::TukeyHSD(fit)[[1]]
        pv   <- tuk[, "p adj"]
        nm   <- rownames(tuk)
        pmat <- hstat_pmat_comparaisons(nm, pv, levs)
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      } else {
        kt <- stats::kruskal.test(y ~ g)
        dn <- FSA::dunnTest(y, g, method = "bonferroni")$res
        pmat <- hstat_pmat_comparaisons(dn$Comparison, dn$P.adj, levs)
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      }
    }, error = function(e) stats::setNames(rep("a", length(levs)), levs))
    
    rows[[v]] <- data.frame(
      Variable      = v,
      Niveau        = levs,
      N             = n_per,
      Moyenne       = means,
      Ecart_type    = sds,
      Erreur_type   = ses,
      Groupes       = as.character(letters_v),
      Moyenne_pm_SD = paste0(fmt_val(means, sds), " ", as.character(letters_v)),
      Moyenne_pm_SE = paste0(fmt_val(means, ses), " ", as.character(letters_v)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out
}

#' PostHoc par variable sur les cellules d'une interaction (facteurs croises)
#'
#' Quand une interaction est presente, compare les combinaisons de niveaux
#' (ex. periode1.zoneA, periode1.zoneB, periode2.zoneA...) afin d'apprecier
#' simultanement l'effet du facteur fixe et du facteur evalue.
#'
#' @param df        data.frame nettoye
#' @param response  variables reponses
#' @param factors   vecteur de >= 2 facteurs (croises)
#' @param parametric TRUE = ANOVA/Tukey, FALSE = Kruskal/Dunn
#' @param digits    decimales d'affichage
#' @return data.frame long : Variable, Cellule, <facteurs>, N, Moyenne,
#'         Ecart_type, Erreur_type, Groupes, Moyenne_pm_SD, Moyenne_pm_SE
build_letters_interaction <- function(df, response, factors,
                                      parametric = TRUE, digits = 3) {
  if (length(factors) < 2) return(NULL)
  df <- df[stats::complete.cases(df[, c(response, factors), drop = FALSE]), , drop = FALSE]
  for (f in factors) {
    if (!is.factor(df[[f]])) df[[f]] <- factor(as.character(df[[f]]))
    df[[f]] <- droplevels(df[[f]])
  }
  
  # Facteur croise : une cellule par combinaison de niveaux
  cell <- interaction(df[factors], sep = " . ", drop = TRUE)
  cell <- droplevels(cell)
  levs <- levels(cell)
  if (length(levs) < 2) return(NULL)
  
  cell_map <- unique(data.frame(
    Cellule = as.character(cell),
    df[factors],
    stringsAsFactors = FALSE
  ))
  cell_map <- cell_map[match(levs, cell_map$Cellule), , drop = FALSE]
  
  fmt_val <- function(m, s) ifelse(is.na(m) | is.na(s), "NA",
                                   paste0(formatC(m, digits = digits, format = "f"),
                                          " \u00b1 ",
                                          formatC(s, digits = digits, format = "f")))
  
  rows <- list()
  for (v in response) {
    y <- df[[v]]
    n_per <- as.numeric(table(cell))[match(levs, levels(cell))]
    means <- vapply(levs, function(lv) mean(y[cell == lv], na.rm = TRUE), numeric(1))
    sds   <- vapply(levs, function(lv) stats::sd(y[cell == lv], na.rm = TRUE), numeric(1))
    ses   <- sds / sqrt(pmax(n_per, 1))
    
    letters_v <- tryCatch({
      if (parametric) {
        fit  <- stats::aov(y ~ cell)
        tuk  <- stats::TukeyHSD(fit)[[1]]
        pv   <- tuk[, "p adj"]; nm <- rownames(tuk)
        pmat <- hstat_pmat_comparaisons(nm, pv, levs)
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      } else {
        dn <- FSA::dunnTest(y, cell, method = "bonferroni")$res
        pmat <- hstat_pmat_comparaisons(dn$Comparison, dn$P.adj, levs)
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      }
    }, error = function(e) stats::setNames(rep("a", length(levs)), levs))
    
    block <- data.frame(
      Variable      = v,
      Cellule       = levs,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    for (f in factors) block[[f]] <- cell_map[[f]]
    block$N             <- n_per
    block$Moyenne       <- means
    block$Ecart_type    <- sds
    block$Erreur_type   <- ses
    block$Groupes       <- as.character(letters_v)
    block$Moyenne_pm_SD <- paste0(fmt_val(means, sds), " ", as.character(letters_v))
    block$Moyenne_pm_SE <- paste0(fmt_val(means, ses), " ", as.character(letters_v))
    rows[[v]] <- block
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out
}

#' Distance de Mahalanobis et detection d'outliers multivaries
#' Un point est considere comme outlier si D2 depasse le quantile chi2(p) a alpha.
#' @return list(d2, threshold, n_outliers, idx_outliers, conclusion)
detect_multivariate_outliers <- function(Y, alpha = 0.001) {
  Y <- as.matrix(Y)
  Y <- Y[stats::complete.cases(Y), , drop = FALSE]
  n <- nrow(Y); p <- ncol(Y)
  if (n < p + 2)
    return(list(d2 = NULL, threshold = NA, n_outliers = NA,
                idx_outliers = integer(0),
                conclusion = "Échantillon trop petit pour Mahalanobis"))
  
  centre <- colMeans(Y)
  cov_mat <- tryCatch(stats::cov(Y), error = function(e) NULL)
  # LE RANG, PAS LE DETERMINANT.
  rang <- tryCatch(qr(cov_mat)$rank, error = function(e) NA_integer_)
  if (is.null(cov_mat) || any(is.na(cov_mat)) ||
      is.na(rang) || rang < p) {
    return(list(d2 = NULL, threshold = NA, n_outliers = NA,
                idx_outliers = integer(0),
                conclusion = "Matrice de covariance singulière -- impossible de calculer Mahalanobis"))
  }
  
  d2 <- stats::mahalanobis(Y, centre, cov_mat)
  threshold <- stats::qchisq(1 - alpha, df = p)
  idx <- which(d2 > threshold)
  pct <- round(100 * length(idx) / n, 1)
  
  concl <- if (length(idx) == 0)
    trf("Aucun outlier multivarié détecté (seuil chi2(%s) à alpha = %s).", p, alpha)
  else if (pct < 5)
    trf("%s outlier(s) multivarié(s) détecté(s) (%s%% des observations). Inspectez-les avant d'analyser.", length(idx), pct)
  else
    trf("%s outliers (%s%% des observations) -- proportion élevée, vérifiez la qualité des données.", length(idx), pct)
  
  list(d2 = d2, threshold = threshold, n_outliers = length(idx),
       idx_outliers = idx, conclusion = concl, alpha = alpha)
}

#' Assistant decisionnel : recommande MANOVA parametrique ou PERMANOVA
#'
#' Combine les resultats de Mardia (normalite multivariee), Box's M
#' (homogeneite des covariances) et PERMDISP (homogeneite des dispersions
#' multivariees) pour produire une recommandation argumentee.
#'
#' - Mardia OK + Box's M OK   -> MANOVA parametrique (Wilks, plus puissant)
#' - Mardia OK + Box's M KO   -> MANOVA parametrique avec Pillai (robuste)
#' - Mardia KO + n grand      -> MANOVA Pillai (theoreme central limite)
#' - Mardia KO + n petit      -> PERMANOVA (aucune hypothese distributionnelle)
#' - PERMDISP KO (dispersions inegales) -> alerte sur l'interpretation
#'
#' @param mardia  output de multivariate_normality_mardia
#' @param boxm    output de boxm_per_factor (data.frame)
#' @param permdisp output de permdisp_per_factor (data.frame)
#' @param n       nombre d'observations
#' @return list(test_recommande, statistique_recommandee, score, justifications, alertes, niveau_confiance)
recommend_manova_test <- function(mardia, boxm, permdisp, n) {
  justifications <- character()
  alertes        <- character()
  score_param    <- 0L  # positif = parametrique recommande, negatif = non parametrique
  
  # 1. Normalite multivariee (Mardia)
  mardia_ok <- !is.null(mardia) &&
    !is.na(mardia$p.skewness) && !is.na(mardia$p.kurtosis) &&
    mardia$p.skewness >= 0.05 && mardia$p.kurtosis >= 0.05
  if (mardia_ok) {
    justifications <- c(justifications,
                        trf("Normalité multivariée respectée (Mardia : p.skew = %s, p.kurt = %s).", round(mardia$p.skewness, 3), round(mardia$p.kurtosis, 3)))
    score_param <- score_param + 2L
  } else {
    if (!is.null(mardia) && (isTRUE(mardia$p.skewness < 0.05) || isTRUE(mardia$p.kurtosis < 0.05))) {
      justifications <- c(justifications,
                          "Violation de la normalité multivariée (Mardia significatif).")
      # Mais si n est grand, le theoreme central limite protege
      if (n >= 50) {
        justifications <- c(justifications,
                            trf("Toutefois, n = %s >= 50 : la MANOVA reste robuste par le théorème central limite (préférer la statistique de Pillai).", n))
        score_param <- score_param + 0L
      } else {
        justifications <- c(justifications,
                            trf("Et n = %s < 50 : PERMANOVA est plus sure (pas d'hypothèse distributionnelle).", n))
        score_param <- score_param - 2L
      }
    }
  }
  
  # 2. Homogeneite des matrices de covariance (Box's M)
  boxm_violations <- if (!is.null(boxm)) sum(grepl("Violation", boxm$Conclusion), na.rm = TRUE) else 0
  if (!is.null(boxm) && boxm_violations == 0) {
    justifications <- c(justifications,
                        "Homogénéité des matrices de covariance respectée (Box's M non significatif).")
    score_param <- score_param + 1L
  } else if (boxm_violations > 0) {
    justifications <- c(justifications,
                        trf("Violation d'homogénéité des covariances sur %s facteur(s) (Box's M significatif). La statistique de Pillai est recommandée car plus robuste à cette violation.", boxm_violations))
    score_param <- score_param + 0L  # neutre car Pillai compense
  }
  
  # 3. Homogeneite multivariee des dispersions (PERMDISP)
  permdisp_violations <- if (!is.null(permdisp)) sum(grepl("heterogenes|hétérogènes", permdisp$Conclusion), na.rm = TRUE) else 0
  if (permdisp_violations > 0) {
    alertes <- c(alertes,
                 trf("PERMDISP signale des dispersions multivariées inégales sur %s facteur(s). Une PERMANOVA significative pourrait refléter une différence de dispersion plutôt qu'une différence de localisation. À interpréter avec prudence.", permdisp_violations))
  }
  
  if (score_param >= 2) {
    test_rec  <- "MANOVA paramétrique"
    stat_rec  <- "Wilks (puissance maximale)"
    confiance <- "élevée"
  } else if (score_param >= 0) {
    test_rec  <- "MANOVA paramétrique"
    stat_rec  <- "Pillai (robuste aux violations)"
    confiance <- if (score_param == 0) "modérée" else "élevée"
  } else {
    test_rec  <- "PERMANOVA"
    stat_rec  <- "Pseudo-F par permutations (999 permutations recommandées)"
    confiance <- "élevée"
  }
  
  list(
    test_recommande         = test_rec,
    statistique_recommandee = stat_rec,
    score                   = score_param,
    niveau_confiance        = confiance,
    justifications          = justifications,
    alertes                 = alertes
  )
}

#' Test d'effets simples multivaries (MANOVA conditionnelles)
#'
#' Quand l'interaction A:B est significative, decompose en testant l'effet de A
#' separement dans chaque niveau de B (et vice versa).
#'
#' @param df       data.frame nettoye
#' @param response variables reponses
#' @param fixed    facteur fixe (on conditionne dessus)
#' @param tested   facteur teste dans chaque niveau de `fixed`
#' @return data.frame : Niveau_fixe, Effet_teste, ddl_num, ddl_den, Pillai, F, p_value, Significatif
manova_simple_effects <- function(df, response, fixed, tested) {
  df <- df[stats::complete.cases(df[, c(response, fixed, tested), drop = FALSE]), , drop = FALSE]
  if (!is.factor(df[[fixed]]))  df[[fixed]]  <- factor(as.character(df[[fixed]]))
  if (!is.factor(df[[tested]])) df[[tested]] <- factor(as.character(df[[tested]]))
  
  results <- list()
  skipped  <- character()
  for (lev in levels(df[[fixed]])) {
    sub <- df[df[[fixed]] == lev, , drop = FALSE]
    sub[[tested]] <- droplevels(sub[[tested]])
    if (nlevels(sub[[tested]]) < 2) next
    n_sub <- nrow(sub); p <- length(response)
    if (n_sub < p + nlevels(sub[[tested]]) + 1) {
      skipped <- c(skipped, lev); next
    }
    
    # Verifier le rang de la matrice des reponses : variables colineaires -> MANOVA impossible
    Ysub <- as.matrix(sub[, response, drop = FALSE])
    rg <- tryCatch(qr(scale(Ysub, center = TRUE, scale = FALSE))$rank,
                   error = function(e) NA_integer_)
    if (is.na(rg) || rg < p) {
      skipped <- c(skipped, lev); next
    }
    
    fml <- stats::as.formula(paste0(
      "cbind(", paste(sapply(response, function(x) paste0("`", x, "`")), collapse = ", "),
      ") ~ `", tested, "`"
    ))
    fit <- tryCatch(stats::manova(fml, data = sub), error = function(e) NULL)
    if (is.null(fit)) { skipped <- c(skipped, lev); next }
    
    s <- tryCatch(summary(fit, test = "Pillai")$stats, error = function(e) NULL)
    if (is.null(s)) { skipped <- c(skipped, lev); next }
    eff_row <- which(rownames(s) == tested)
    if (length(eff_row) == 0) eff_row <- 1
    results[[lev]] <- data.frame(
      Niveau_fixe = paste0(fixed, " = ", lev),
      Effet_teste = tested,
      ddl_num     = s[eff_row, "Df"],
      ddl_den     = s["Residuals", "Df"],
      Pillai      = s[eff_row, "Pillai"],
      F_stat      = s[eff_row, "approx F"],
      p_value     = s[eff_row, "Pr(>F)"],
      stringsAsFactors = FALSE
    )
  }
  if (length(results) == 0) {
    if (length(skipped) > 0)
      attr(results, "skip_reason") <-
        "Variables réponses colinéaires ou sous-groupes trop petits : effets simples MANOVA non calculables."
    return(NULL)
  }
  out <- do.call(rbind, results); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = "bonferroni")
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  # CE QUI EST ECARTE EST NOMME, MEME QUAND LE RESTE ABOUTIT.
  if (length(skipped) > 0) {
    attr(out, "niveaux_ecartes") <- skipped
    attr(out, "message") <- trf(
      "%s niveau(x) écarté(s) : %s. Réponses colinéaires ou sous-groupe trop petit. La correction de Bonferroni ne porte que sur les %s test(s) restant(s).",
      length(skipped), paste(skipped, collapse = ", "), nrow(out))
  }
  out
}

#' Effets simples PERMANOVA (analogue non parametrique)
#' @return data.frame analogue a manova_simple_effects
permanova_simple_effects <- function(df, response, fixed, tested,
                                     permutations = 999, dist_method = "euclidean") {
  df <- df[stats::complete.cases(df[, c(response, fixed, tested), drop = FALSE]), , drop = FALSE]
  if (!is.factor(df[[fixed]]))  df[[fixed]]  <- factor(as.character(df[[fixed]]))
  if (!is.factor(df[[tested]])) df[[tested]] <- factor(as.character(df[[tested]]))
  
  results <- list()
  skipped <- character()
  for (lev in levels(df[[fixed]])) {
    sub <- df[df[[fixed]] == lev, , drop = FALSE]
    sub[[tested]] <- droplevels(sub[[tested]])
    if (nlevels(sub[[tested]]) < 2 || nrow(sub) < 4) { skipped <- c(skipped, lev); next }
    
    sub <- hstat_cap_df_rows(sub, what = "PERMANOVA stratifiée")
    Y <- as.matrix(sub[, response, drop = FALSE])
    d <- tryCatch(vegan::vegdist(Y, method = dist_method), error = function(e) NULL)
    if (is.null(d)) { skipped <- c(skipped, lev); next }
    fml <- stats::as.formula(paste0("d ~ `", tested, "`"))
    ad <- tryCatch(
      vegan::adonis2(fml, data = sub, permutations = permutations, by = "terms"),
      error = function(e) NULL
    )
    if (is.null(ad)) { skipped <- c(skipped, lev); next }
    
    results[[lev]] <- data.frame(
      Niveau_fixe  = paste0(fixed, " = ", lev),
      Effet_teste  = tested,
      ddl          = ad$Df[1],
      R2           = ad$R2[1],
      F_pseudo     = ad$F[1],
      p_value      = ad$`Pr(>F)`[1],
      stringsAsFactors = FALSE
    )
  }
  if (length(results) == 0) return(NULL)
  out <- do.call(rbind, results); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = "bonferroni")
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  # Meme regle que les effets simples MANOVA : un niveau ecarte est nomme, et
  # l'on dit sur combien de tests porte reellement la correction.
  if (length(skipped) > 0) {
    attr(out, "niveaux_ecartes") <- skipped
    attr(out, "message") <- trf(
      "%s niveau(x) écarté(s) : %s. Moins de deux modalités testées ou trop peu d'observations. La correction de Bonferroni ne porte que sur les %s test(s) restant(s).",
      length(skipped), paste(skipped, collapse = ", "), nrow(out))
  }
  out
}

#' Identifie les predicteurs categoriels dans un modele lm/glm
#' @param model modele lm ou glm ajuste
#' @return character vector des noms des predicteurs categoriels
identify_categorical_predictors <- function(model) {
  if (is.null(model) || is.null(model$model)) return(character(0))
  mf <- model$model
  preds <- setdiff(names(mf), names(mf)[1])
  preds[vapply(preds, function(p) {
    v <- mf[[p]]
    is.factor(v) || is.character(v) || is.logical(v)
  }, logical(1))]
}

#' Comparaisons par paires sur un predicteur categoriel (emmeans-based)
#' @param model    modele lm ou glm
#' @param predictor nom du predicteur categoriel
#' @param adjust   methode d'ajustement ("tukey", "bonferroni", "sidak", "holm", "fdr", "none")
#' @return data.frame : Comparaison, Estimate, SE, ddl, t/z, p_value, p_adj, Significatif
lm_pairwise_emmeans <- function(model, predictor, adjust = "tukey") {
  if (!requireNamespace("emmeans", quietly = TRUE))
    return(NULL)
  em <- tryCatch(
    emmeans::emmeans(model, specs = predictor),
    error = function(e) NULL
  )
  if (is.null(em)) return(NULL)
  pr <- tryCatch(
    graphics::pairs(em, adjust = adjust),
    error = function(e) NULL
  )
  if (is.null(pr)) return(NULL)
  
  df_out <- as.data.frame(pr)
  rename_map <- c(contrast    = "Comparaison",
                  estimate    = "Différence",
                  t.ratio     = "t",
                  z.ratio     = "z",
                  p.value     = "p_value")
  for (old in names(rename_map)) {
    if (old %in% names(df_out)) names(df_out)[names(df_out) == old] <- rename_map[[old]]
  }
  if ("p_value" %in% names(df_out)) {
    df_out$p_adj        <- df_out$p_value
    df_out$Significatif <- ifelse(is.na(df_out$p_adj), "NA",
                                  ifelse(df_out$p_adj < 0.05, "Oui", "Non"))
  }
  df_out
}

#' Lettres de groupes (CLD) pour un predicteur categoriel
#' @param model    modele lm ou glm
#' @param predictor nom du predicteur categoriel
#' @param adjust   methode d'ajustement
#' @return data.frame : Niveau, emmean, SE, ddl, Groupes
lm_cld_letters <- function(model, predictor, adjust = "tukey", digits = 3) {
  if (!requireNamespace("emmeans", quietly = TRUE) ||
      !requireNamespace("multcomp", quietly = TRUE))
    return(NULL)
  em <- tryCatch(emmeans::emmeans(model, specs = predictor),
                 error = function(e) NULL)
  if (is.null(em)) return(NULL)
  # `multcomp::cld` SUFFIT : c'est la generique, et emmeans enregistre sa
  # methode pour les objets `emmGrid`.
  cld <- tryCatch(
    multcomp::cld(em, adjust = adjust, Letters = letters, decreasing = TRUE),
    error = function(e) NULL)
  if (is.null(cld)) return(NULL)
  
  df_out <- as.data.frame(cld)
  if (predictor %in% names(df_out))
    names(df_out)[names(df_out) == predictor] <- "Niveau"
  if (".group" %in% names(df_out))
    names(df_out)[names(df_out) == ".group"] <- "Groupes"
  if ("group" %in% names(df_out))
    names(df_out)[names(df_out) == "group"] <- "Groupes"
  if ("Groupes" %in% names(df_out))
    df_out$Groupes <- trimws(df_out$Groupes)
  
  # Moyennes observees par niveau (depuis les donnees du modele) + SD / SE
  mf <- tryCatch(model$model, error = function(e) NULL)
  if (!is.null(mf) && predictor %in% names(mf)) {
    resp_name <- names(mf)[1]
    y <- mf[[resp_name]]
    g <- factor(mf[[predictor]])
    fmt <- function(m, s) ifelse(is.na(m) | is.na(s), "NA",
                                 paste0(formatC(m, digits = digits, format = "f"),
                                        " \u00b1 ",
                                        formatC(s, digits = digits, format = "f")))
    stats_g <- sapply(as.character(df_out$Niveau), function(lv) {
      yy <- y[g == lv]
      n  <- sum(!is.na(yy))
      m  <- mean(yy, na.rm = TRUE)
      sdv <- stats::sd(yy, na.rm = TRUE)
      sev <- if (n > 0) sdv / sqrt(n) else NA_real_
      c(m = m, sd = sdv, se = sev)
    })
    grp <- df_out$Groupes
    df_out$`Moyenne_pm_SD` <- paste0(fmt(stats_g["m", ], stats_g["sd", ]), " ", grp)
    df_out$`Moyenne_pm_SE` <- paste0(fmt(stats_g["m", ], stats_g["se", ]), " ", grp)
  }
  df_out
}

# Helpers transverses : arrondi global et coloration des groupes posthoc

#' Arrondit toutes les colonnes numeriques d'un data.frame selon les reglages.
#' Centralise la logique d'arrondi pour toutes les analyses (maintenable).
#' @param df       data.frame a arrondir
#' @param round_on TRUE si l'arrondi est active (input$testsRoundResults)
#' @param decimals nombre de decimales (input$testsDecimals)
#' @return data.frame avec colonnes numeriques arrondies (inchange si round_on FALSE)
# Nombre de decimales D'AFFICHAGE : la valeur choisie quand l'arrondi est
# actif, et `NA` quand il ne l'est pas.
hstat_dec_affichage <- function(round_on, decimals = 2) {
  if (!isTRUE(round_on)) return(NA_integer_)
  if (is.null(decimals) || !length(decimals) || is.na(decimals[1])) return(2L)
  max(0L, as.integer(decimals[1]))
}

# « moyenne ± dispersion groupe », a une precision d'affichage donnee.
.hstat_pm <- function(m, s, groupes = NULL, dec = NA_integer_) {
  m <- suppressWarnings(as.numeric(m))
  s <- suppressWarnings(as.numeric(s))
  dec <- suppressWarnings(as.integer(dec[1]))
  # Sans arrondi demande, on rend le nombre tel qu'il est.
  mef <- if (is.na(dec)) {
    # ELEMENT PAR ELEMENT.
    function(x) vapply(x, function(v)
      format(v, trim = TRUE, digits = 15, scientific = FALSE), character(1),
      USE.NAMES = FALSE)
  } else {
    function(x) formatC(round(x, dec), digits = max(0L, dec), format = "f")
  }
  out <- ifelse(is.na(m) | is.na(s), "NA",
                paste0(mef(m), " \u00b1 ", mef(s)))
  if (!is.null(groupes)) out <- paste0(out, " ", as.character(groupes))
  out
}

# SYNTHESE POST-HOC « comme dans un article ».
HSTAT_POSTHOC_SYMBOLES <- c("ANOVA" = "F", "ANOVA de Welch" = "W",
                            "Kruskal-Wallis" = "H")

# LE COEFFICIENT DE VARIATION D'UNE COLONNE est celui de l'essai : l'ecart-type
# INTRA-GROUPE mis en commun, rapporte a la moyenne generale.
HSTAT_SYNTH_CV <- "CV (%)"

.hstat_synth_cv <- function(sv) {
  m <- suppressWarnings(as.numeric(sv$Moyenne))
  n <- suppressWarnings(as.numeric(sv$N %||% rep(NA_real_, length(m))))
  s <- if ("Ecart_type" %in% names(sv)) suppressWarnings(as.numeric(sv$Ecart_type)) else
    rep(NA_real_, length(m))
  if ("Erreur_type" %in% names(sv)) {
    es <- suppressWarnings(as.numeric(sv$Erreur_type))
    manque <- !is.finite(s) & is.finite(es) & is.finite(n)
    s[manque] <- es[manque] * sqrt(n[manque])
  }
  ok <- is.finite(m) & is.finite(s) & is.finite(n) & n > 1
  if (!any(ok)) return(NA_real_)
  ddl <- sum(n[ok] - 1)
  mg <- sum(n[ok] * m[ok]) / sum(n[ok])
  if (!is.finite(mg) || mg == 0 || ddl <= 0) return(NA_real_)
  sqrt(sum((n[ok] - 1) * s[ok]^2) / ddl) / abs(mg) * 100
}

.hstat_synth_cv_txt <- function(sv, dec) {
  cv <- .hstat_synth_cv(sv)
  if (is.finite(cv)) formatC(round(cv, dec), format = "f", digits = dec) else ""
}

.hstat_synth_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  ifelse(!is.finite(p), "",
         ifelse(p < 0.001, "< 0.001", formatC(round(p, 3), format = "f", digits = 3)))
}

# Une table par facteur.
hstat_posthoc_synthese <- function(d, dispersion = c("ecart_type", "erreur_type"),
                                   dec = NA_integer_) {
  dispersion <- match.arg(dispersion)
  col_disp <- if (dispersion == "ecart_type") "Ecart_type" else "Erreur_type"
  if (!is.data.frame(d) || !NROW(d) || !all(c("Type", "Facteur", "Variable",
      "Moyenne", col_disp) %in% names(d))) return(list())
  d <- d[!is.na(d$Type) & d$Type %in% c("main", "simple_effect"), , drop = FALSE]
  if (!NROW(d)) return(list())
  d <- d[order(d$Type != "main"), , drop = FALSE]
  dec_stat <- if (is.na(suppressWarnings(as.integer(dec[1])))) 2L else
    max(0L, as.integer(dec[1]))
  out <- list()
  for (f in unique(as.character(d$Facteur))) {
    sub <- d[as.character(d$Facteur) == f, , drop = FALSE]
    # Un effet simple range ses modalites sous le nom du facteur TESTE, ecrit
    # avant « | » dans son libelle.
    colf <- if (identical(sub$Type[1], "simple_effect"))
      trimws(sub(" \\| .*$", "", f)) else f
    # Les modalites se lisent sur `hstat_modalite` quand elle existe.
    colf <- if ("hstat_modalite" %in% names(sub) &&
                any(!is.na(sub$hstat_modalite))) "hstat_modalite" else colf
    if (!colf %in% names(sub)) next
    niv  <- unique(as.character(sub[[colf]]))
    niv  <- niv[!is.na(niv)]
    vars <- unique(as.character(sub$Variable))
    if (!length(niv) || !length(vars)) next
    # Les quatre lignes du test : etiquette commune, sauf quand les colonnes ne
    # portent pas le meme test -- chaque case dit alors son symbole.
    sym <- vapply(vars, function(v) {
      sv <- sub[as.character(sub$Variable) == v, , drop = FALSE]
      if ("Symbole" %in% names(sv) && !is.na(sv$Symbole[1])) return(as.character(sv$Symbole[1]))
      t <- if ("Test_global" %in% names(sv)) sv$Test_global[1] else NA
      if (is.null(t) || is.na(t)) NA_character_ else
        unname(HSTAT_POSTHOC_SYMBOLES[as.character(t)])
    }, character(1))
    sym <- unname(sym)
    sym_uniq <- unique(sym[!is.na(sym)])
    lab_stat <- if (length(sym_uniq) == 1L) sym_uniq else
      if (length(sym_uniq)) paste(sym_uniq, collapse = " / ") else tr("Statistique")
    tab <- data.frame(x = c(niv, HSTAT_SYNTH_CV, tr("ddl"), lab_stat, "N", "P"),
                      stringsAsFactors = FALSE, check.names = FALSE)
    names(tab) <- f
    # Le groupe de chaque case, a part.
    groupes <- matrix("", length(niv), length(vars))
    for (k in seq_along(vars)) {
      v <- vars[k]
      sv <- sub[as.character(sub$Variable) == v, , drop = FALSE]
      grp <- if ("groups" %in% names(sv)) as.character(sv$groups) else NULL
      if (!is.null(grp)) grp[is.na(grp)] <- ""
      cases <- trimws(.hstat_pm(sv$Moyenne, sv[[col_disp]], grp, dec))
      # Une valeur sans dispersion -- la reference d'un test a une norme -- se
      # lit seule : « 12 » et non « NA ».
      seule <- !is.na(suppressWarnings(as.numeric(sv$Moyenne))) &
        is.na(suppressWarnings(as.numeric(sv[[col_disp]])))
      if (any(seule))
        cases[seule] <- trimws(paste(sub(" \u00b1 .*$", "",
          .hstat_pm(sv$Moyenne[seule], 0, NULL, dec)),
          if (is.null(grp)) "" else grp[seule]))
      cellule <- cases[match(niv, as.character(sv[[colf]]))]
      cellule[is.na(cellule)] <- ""
      if (!is.null(grp)) {
        gk <- grp[match(niv, as.character(sv[[colf]]))]
        gk[is.na(gk)] <- ""
        groupes[, k] <- trimws(gk)
      }
      stat <- suppressWarnings(as.numeric(sv$Statistique_globale[1] %||% NA))
      stat_txt <- if (is.finite(stat)) formatC(round(stat, dec_stat),
                                               format = "f", digits = dec_stat) else ""
      if (nzchar(stat_txt) && length(sym_uniq) > 1L && !is.na(sym[k]))
        stat_txt <- paste(sym[k], "=", stat_txt)
      ddl <- as.character(sv$ddl_global[1] %||% NA)
      ddl <- if (is.na(ddl)) "" else gsub("\\s*,\\s*", ", ", trimws(ddl))
      # N est l'effectif DU TEST : la somme des groupes, sauf quand l'appelant
      # le pose (des paires comptent une fois, pas deux).
      n_t <- if ("N_test" %in% names(sv)) suppressWarnings(as.numeric(sv$N_test[1])) else NA
      n <- if (isTRUE(is.finite(n_t))) n_t else
        sum(suppressWarnings(as.numeric(sv$N)), na.rm = TRUE)
      tab[[v]] <- c(cellule, .hstat_synth_cv_txt(sv, dec_stat), ddl, stat_txt,
                    if (n > 0) format(n) else "", .hstat_synth_p(sv$p_global[1]))
    }
    attr(tab, "groupes") <- groupes
    out[[f]] <- tab
  }
  out
}

# LES TESTS QUI COMPARENT DEUX MODALITES, UNE VALEUR A UNE NORME, OU DEUX
# MESURES APPARIEES prennent la meme synthese.
.hstat_lettres_deux <- function(m, p) {
  m <- suppressWarnings(as.numeric(m))
  if (length(m) != 2L || any(!is.finite(m))) return(rep(NA_character_, length(m)))
  switch(hstat_p_verdict(p),
    "significatif" = if (m[1] >= m[2]) c("a", "b") else c("b", "a"),
    "non significatif" = c("a", "a"),
    rep(NA_character_, 2L))
}

# Un bloc au format des resultats post-hoc : `hstat_posthoc_synthese()` le lit
# sans savoir d'ou il vient.
.hstat_synth_bloc <- function(facteur, lignes, m, s, n, var, stat, ddl, p,
                              symbole, n_test = NA_real_, lettres = TRUE) {
  m <- suppressWarnings(as.numeric(m)); s <- suppressWarnings(as.numeric(s))
  n <- suppressWarnings(as.numeric(n))
  # Les modalites vont dans `hstat_modalite`, jamais dans une colonne nommee
  # d'apres le facteur.
  b <- data.frame(hstat_modalite = as.character(lignes), stringsAsFactors = FALSE)
  b$Moyenne <- m
  b$Ecart_type <- s
  b$Erreur_type <- ifelse(is.finite(n) & n > 1, s / sqrt(n), NA_real_)
  b$N <- n
  b$groups <- if (isTRUE(lettres)) .hstat_lettres_deux(m, p) else NA_character_
  b$Variable <- var
  b$Facteur <- facteur
  b$Type <- "main"
  b$Symbole <- symbole
  b$p_global <- suppressWarnings(as.numeric(p[1]))
  b$Statistique_globale <- suppressWarnings(as.numeric(stat[1]))
  b$ddl_global <- if (is.null(ddl) || !length(ddl) || is.na(ddl[1])) NA_character_ else as.character(ddl[1])
  b$N_test <- suppressWarnings(as.numeric(n_test[1]))
  b
}

# Empile des blocs qui ne portent pas tous la meme colonne de modalites.
.hstat_synth_empiler <- function(blocs) {
  blocs <- Filter(function(b) is.data.frame(b) && NROW(b), blocs)
  if (!length(blocs)) return(NULL)
  as.data.frame(dplyr::bind_rows(blocs))
}

# Modalites lues comme le font les tests : une date par son ecriture ISO.
.hstat_synth_niveaux <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) x <- format(x, "%Y-%m-%d")
  if (is.factor(x)) return(droplevels(x))
  if (is.numeric(x)) {
    u <- sort(unique(x[!is.na(x)]))
    return(factor(as.character(x), levels = unique(as.character(u))))
  }
  factor(as.character(x))
}

# Tests a groupes (t, Wilcoxon-Mann-Whitney, ANOVA, Welch, Kruskal-Wallis) :
# une ligne par modalite, le test lu dans le tableau de resultats.
hstat_synthese_groupes <- function(df, vars, fvars, res, symbole,
                                   cas_complets = character(0)) {
  if (!is.data.frame(df) || !is.data.frame(res)) return(NULL)
  # Les lignes que le modele a vues : un facteur manquant les ecarte de l'ANOVA
  # a plusieurs facteurs, il doit les ecarter de N aussi.
  garde <- rep(TRUE, NROW(df))
  for (cc in intersect(cas_complets, names(df))) garde <- garde & !is.na(df[[cc]])
  blocs <- list()
  for (f in intersect(fvars, names(df))) {
    g <- .hstat_synth_niveaux(df[[f]])
    for (v in intersect(vars, names(df))) {
      fac_res <- gsub("`", "", trimws(as.character(res$Facteur)), fixed = TRUE)
      r <- res[as.character(res$Variable) == v & fac_res == f, , drop = FALSE]
      if (!NROW(r)) next
      y <- suppressWarnings(as.numeric(df[[v]]))
      ok <- is.finite(y) & !is.na(g) & garde
      if (!any(ok)) next
      gg <- droplevels(g[ok]); yy <- y[ok]
      niv <- levels(gg)
      # Un seul passage groupe par statistique, pas un sous-ensemble par niveau.
      n <- as.numeric(tabulate(gg, nbins = length(niv)))
      m <- as.numeric(tapply(yy, gg, mean))
      sdv <- as.numeric(tapply(yy, gg, stats::sd))
      sdv[n < 2] <- NA_real_
      blocs[[length(blocs) + 1L]] <- .hstat_synth_bloc(
        f, niv, m, sdv, n, v, r$Statistique[1], r$ddl[1], r$p_value[1], symbole,
        lettres = length(niv) == 2L)
    }
  }
  .hstat_synth_empiler(blocs)
}

# Comparaison a une norme : l'echantillon, puis la valeur de reference.
hstat_synthese_reference <- function(x, var, reference, stat, ddl, p, symbole,
                                     lettres = TRUE, libelle_ref = tr("Valeur de référence")) {
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (length(x) < 1L) return(NULL)
  .hstat_synth_bloc(tr("Comparaison à la norme"), c(tr("Échantillon observé"), libelle_ref),
                    c(mean(x), reference),
                    c(if (length(x) > 1) stats::sd(x) else NA_real_, NA_real_),
                    c(length(x), NA_real_), var, stat, ddl, p, symbole,
                    n_test = length(x), lettres = lettres)
}

# Deux mesures appariees : une ligne par condition, N compte les PAIRES.
hstat_synthese_apparie <- function(a, b, noms, var, stat, ddl, p, symbole,
                                   lettres = TRUE) {
  a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
  ok <- is.finite(a) & is.finite(b); a <- a[ok]; b <- b[ok]
  if (length(a) < 1L) return(NULL)
  sdx <- function(z) if (length(z) > 1) stats::sd(z) else NA_real_
  .hstat_synth_bloc(tr("Mesure appariée"), noms, c(mean(a), mean(b)), c(sdx(a), sdx(b)),
                    c(length(a), length(b)), var, stat, ddl, p, symbole,
                    n_test = length(a), lettres = lettres)
}

# Une couleur par groupe de lettres, la meme regle que la colonne « groups » du
# tableau des effets principaux.
hstat_groupes_couleurs <- function(g, en_plus = NULL) {
  net <- function(x) { x <- unique(trimws(as.character(x))); x[!is.na(x) & nzchar(x)] }
  g <- net(g)
  out <- if (length(g)) stats::setNames(grDevices::rainbow(length(g), alpha = 0.3), g) else character(0)
  e <- setdiff(net(en_plus), g)
  if (length(e))
    out <- c(out, stats::setNames(grDevices::rainbow(length(e), s = 0.45, alpha = 0.3), e))
  out
}

# Les tables de synthese en HTML statique.
hstat_synthese_html <- function(tabs, couleurs = NULL) {
  if (!length(tabs)) return(shiny::helpText(tr("Aucun résultat à résumer.")))
  if (is.null(couleurs))
    couleurs <- hstat_groupes_couleurs(unlist(lapply(tabs, function(t)
      as.vector(attr(t, "groupes")))))
  shiny::tagList(lapply(tabs, function(t) {
    n_niv <- nrow(t) - 5L
    grp <- attr(t, "groupes")
    entete <- shiny::tags$tr(lapply(names(t), function(n) shiny::tags$th(n)))
    lignes <- lapply(seq_len(nrow(t)), function(i) {
      cls <- if (i == n_niv + 1L) "hstat-synthese-test" else NULL
      shiny::tags$tr(class = cls, lapply(seq_along(t), function(j) {
        cell <- t[i, j]
        if (j == 1L) return(shiny::tags$td(class = "hstat-synthese-modalite", cell))
        g <- if (i <= n_niv && is.matrix(grp) && ncol(grp) >= j - 1L)
          grp[i, j - 1L] else ""
        coul <- if (nzchar(g) && g %in% names(couleurs)) couleurs[[g]] else NA
        shiny::tags$td(cell, class = if (!is.na(coul)) "hstat-synthese-groupe",
                       style = if (!is.na(coul)) paste0("background-color:", coul, ";"))
      }))
    })
    shiny::tags$table(class = "table hstat-synthese",
      shiny::tags$thead(entete), shiny::tags$tbody(lignes))
  }))
}

# Plusieurs tables dans UNE feuille : chacune sous une ligne qui porte son
# intitule, separees d'une ligne vide.
.hstat_synth_empiler_feuille <- function(tabs) {
  if (length(tabs) == 1L) return(tabs[[1]])
  vars <- unique(unlist(lapply(tabs, function(t) names(t)[-1])))
  cols <- c(tr("Facteur / modalité"), vars)
  vide <- function(n) as.data.frame(matrix("", n, length(cols),
    dimnames = list(NULL, cols)), stringsAsFactors = FALSE, check.names = FALSE)
  blocs <- lapply(seq_along(tabs), function(i) {
    t <- tabs[[i]]
    titre <- vide(1L); titre[1, 1] <- names(t)[1]
    corps <- vide(nrow(t)); corps[, 1] <- t[[1]]
    for (v in names(t)[-1]) corps[[v]] <- t[[v]]
    rbind(titre, corps, if (i < length(tabs)) vide(1L))
  })
  do.call(rbind, blocs)
}

# Les feuilles du classeur : une par dispersion, toutes les tables dedans.
hstat_posthoc_synthese_feuilles <- function(d, dec = NA_integer_) {
  et <- hstat_posthoc_synthese(d, "ecart_type", dec)
  es <- hstat_posthoc_synthese(d, "erreur_type", dec)
  if (!length(et)) return(list())
  list(Moyenne_ET_groupe = .hstat_synth_empiler_feuille(et),
       Moyenne_ES_groupe = .hstat_synth_empiler_feuille(es))
}

round_numeric_df <- function(df, round_on, decimals = 2) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  dec <- hstat_dec_affichage(round_on, decimals)
  
  # Noms possibles des colonnes texte (avant ou apres renommage d'affichage).
  sd_names <- c("Moyenne_pm_SD", "Moyenne\u00b1Ecart_type",
                "Moyenne \u00b1 Écart-type groupe")
  se_names <- c("Moyenne_pm_SE", "Moyenne\u00b1Erreur_type",
                "Moyenne \u00b1 Erreur-type groupe")
  has_col  <- function(nms) nms[nms %in% names(df)][1]
  sd_col   <- has_col(sd_names)
  se_col   <- has_col(se_names)
  
  # Reconstruction des colonnes texte a partir des colonnes numeriques sources,
  # afin que "Moyenne ± Écart-type" affiche EXACTEMENT la valeur de "Moyenne".
  fmt_pair <- function(m, s, grp) .hstat_pm(m, s, grp, dec)
  # La colonne des lettres de groupes s'appelle « Groupes » ici et « groups »
  # dans le post-hoc.
  grp_col <- has_col(c("Groupes", "groups"))
  grp_vec <- if (!is.na(grp_col)) as.character(df[[grp_col]]) else NULL
  
  if (!is.na(sd_col) && all(c("Moyenne", "Ecart_type") %in% names(df))) {
    df[[sd_col]] <- fmt_pair(df$Moyenne, df$Ecart_type, grp_vec)
  }
  if (!is.na(se_col) && all(c("Moyenne", "Erreur_type") %in% names(df))) {
    df[[se_col]] <- fmt_pair(df$Moyenne, df$Erreur_type, grp_vec)
  }
  
  # Colonnes numeriques : arrondi seulement si l'utilisateur l'a demande.
  if (isTRUE(round_on)) {
    num <- vapply(df, is.numeric, logical(1))
    if (any(num))
      df[, num] <- lapply(df[, num, drop = FALSE], function(x) round(x, dec))
    
    # Colonnes texte "± " restantes (sans colonnes numeriques sources) :
    # re-arrondir les nombres presents dans la chaine.
    is_pm_col <- function(values) {
      v <- values[!is.na(values)]
      length(v) > 0 && all(grepl("\u00b1", v))
    }
    handled <- c(sd_col, se_col)
    for (cn in names(df)) {
      if (cn %in% handled) next
      if (is.character(df[[cn]]) && is_pm_col(df[[cn]])) {
        df[[cn]] <- vapply(df[[cn]], function(s) {
          if (is.na(s)) return(NA_character_)
          m <- gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", s, perl = TRUE)
          regmatches(s, m) <- list(vapply(regmatches(s, m)[[1]], function(num) {
            val <- suppressWarnings(as.numeric(num))
            if (is.na(val)) num
            else formatC(round(val, dec), digits = dec, format = "f")
          }, character(1)))
          s
        }, character(1), USE.NAMES = FALSE)
      }
    }
  }
  df
}

#' Palette de couleurs distinctes pour les lettres de groupes CLD.
#' @param n nombre de couleurs souhaitees
#' @return vecteur de couleurs hexadecimales
group_color_palette <- function(n) {
  base <- c("#E8F5E9", "#FFF3E0", "#E3F2FD", "#F3E5F5", "#FCE4EC",
            "#E0F7FA", "#FFF9C4", "#EFEBE9", "#F1F8E9", "#E1F5FE",
            "#FBE9E7", "#EDE7F6")
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::hcl.colors(n, palette = "Pastel 1")
}

#' Applique une couleur de fond distincte par lettre de groupe a une colonne
#' d'un datatable DT. Chaque groupe unique recoit sa propre couleur.
#' @param dt  objet datatable DT
#' @param df  data.frame source (pour extraire les niveaux de groupe)
#' @param col nom de la colonne contenant les lettres de groupes
#' @return datatable DT avec coloration appliquee
color_groups_dt <- function(dt, df, col = "Groupes") {
  if (!col %in% names(df)) return(dt)
  grps <- sort(unique(stats::na.omit(as.character(df[[col]]))))
  if (length(grps) == 0) return(dt)
  cols <- group_color_palette(length(grps))
  DT::formatStyle(dt, col,
                  backgroundColor = DT::styleEqual(grps, cols),
                  fontWeight = "bold", textAlign = "center")
}

#  Moteur de donnees HStat -- chargement en memoire (fread) + hors-memoire (DuckDB)

# -- Parametres globaux (modifiables).
HSTAT_BIGDATA_THRESHOLD <- {
  v <- suppressWarnings(as.numeric(Sys.getenv("HSTAT_BIGDATA_THRESHOLD_MB", "500")))
  if (!is.finite(v) || v < 50) v <- 500
  v * 1024^2
}
# Taille de l'echantillon de travail en mode hors-memoire.
HSTAT_SAMPLE_SIZE <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_SAMPLE_SIZE", "100000")))
  if (!is.finite(v) || v < 1000) 100000L else v
}

# -- Gros volumes : garde-fous pour 1 000 000+ lignes -------------------------
# Nombre maximal de points traces sur un nuage de points.
HSTAT_PLOT_MAX_POINTS <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_PLOT_MAX_POINTS", "100000")))
  if (!is.finite(v) || v < 1000) 100000L else v
}

# Echantillonne un data.frame pour l'AFFICHAGE graphique uniquement.
hstat_sample_rows <- function(df, n = HSTAT_PLOT_MAX_POINTS, notify = TRUE) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) <= n) return(df)
  n0 <- nrow(df)
  old_seed <- if (exists(".Random.seed", envir = globalenv()))
    get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(20260712L)
  idx <- sort(sample.int(n0, n))
  out <- df[idx, , drop = FALSE]
  attr(out, "hstat_sampled_from") <- n0
  if (isTRUE(notify) && !is.null(shiny::getDefaultReactiveDomain())) {
    shiny::showNotification(
      sprintf(paste0("Nuage de points : affichage d'un échantillon de %s points ",
                     "sur %s lignes (les calculs statistiques utilisent toutes ",
                     "les lignes)."),
              format(n, big.mark = " "), format(n0, big.mark = " ")),
      type = "message", duration = 8)
  }
  out
}

# Test de Shapiro-Wilk robuste aux gros echantillons : shapiro.test() refuse n
# > 5000 (limite de R).
hstat_shapiro <- function(x) {
  x <- x[is.finite(as.numeric(x))]
  n <- length(x)
  if (n < 3) stop("Shapiro-Wilk : au moins 3 valeurs valides sont requises.")
  if (n <= 5000) return(stats::shapiro.test(x))
  old_seed <- if (exists(".Random.seed", envir = globalenv()))
    get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(20260712L)
  res <- stats::shapiro.test(sample(x, 5000L))
  res$method <- trf(
    "Shapiro-Wilk normality test (sous-échantillon aléatoire de 5000 valeurs sur %s)",
    format(n, big.mark = " "))
  res
}

# SHAPIRO-WILK PAR GROUPE, SUR LES DONNEES BRUTES ET SUR LES RESIDUS
# Le test global sur la variable brute melange les groupes.
.hstat_shapiro_ligne <- function(x, test, variable, facteur, groupe) {
  x <- x[is.finite(x)]
  n <- length(x)
  base <- data.frame(Test = test, Variable = variable, Facteur = facteur,
                     Groupe = groupe, n = n, Statistique = NA_real_,
                     ddl = NA_character_, p_value = NA_real_,
                     Interpretation = NA_character_, stringsAsFactors = FALSE)
  if (n < 3) {
    base$Interpretation <- tr("Moins de 3 valeurs dans ce groupe : Shapiro-Wilk n'est pas calculable.")
    return(base)
  }
  if (diff(range(x)) < 1e-12) {
    base$Interpretation <- tr("Toutes les valeurs du groupe sont identiques : la normalité n'est pas testable.")
    return(base)
  }
  r <- tryCatch(hstat_shapiro(x), error = function(e) e)
  if (inherits(r, "error")) { base$Interpretation <- hstat_err_fr(r); return(base) }
  base$Statistique <- round(unname(r$statistic), 4)
  base$p_value <- r$p.value
  base$Interpretation <- interpret_test_results("shapiro", r$p.value)
  base
}

hstat_shapiro_groupes <- function(d, variables, facteurs,
                                  sources = c("brutes", "residus")) {
  sources <- intersect(sources, c("brutes", "residus"))
  facteurs <- intersect(facteurs, names(d))
  variables <- intersect(variables, names(d))
  if (!length(facteurs) || !length(variables) || !length(sources)) return(NULL)
  fac <- lapply(facteurs, function(f) {
    v <- d[[f]]
    if (inherits(v, c("Date", "POSIXct", "POSIXlt"))) v <- format(v, "%Y-%m-%d")
    droplevels(factor(v))
  })
  names(fac) <- facteurs
  lignes <- list()
  for (var in variables) {
    y <- suppressWarnings(as.numeric(d[[var]]))
    # Le residu exige une cellule definie : une ligne a qui manque un facteur
    # n'en a pas, et sort du calcul des moyennes de cellule.
    ok_cell <- Reduce(`&`, lapply(fac, function(v) !is.na(v)), is.finite(y))
    res <- rep(NA_real_, length(y))
    if (any(ok_cell)) {
      cell <- interaction(lapply(fac, function(v) v[ok_cell]), drop = TRUE)
      res[ok_cell] <- y[ok_cell] - stats::ave(y[ok_cell], cell, FUN = mean)
    }
    if ("residus" %in% sources)
      lignes[[length(lignes) + 1L]] <- .hstat_shapiro_ligne(
        res, tr("Normalité des résidus (Shapiro-Wilk)"), var,
        paste(facteurs, collapse = " × "), tr("Ensemble (résidus regroupés)"))
    for (f in facteurs) {
      for (niv in levels(fac[[f]])) {
        dans <- !is.na(fac[[f]]) & fac[[f]] == niv
        if ("brutes" %in% sources)
          lignes[[length(lignes) + 1L]] <- .hstat_shapiro_ligne(
            y[dans], tr("Normalité par groupe, données brutes (Shapiro-Wilk)"),
            var, f, niv)
        if ("residus" %in% sources) {
          l <- .hstat_shapiro_ligne(
            res[dans], tr("Normalité par groupe, résidus (Shapiro-Wilk)"),
            var, f, niv)
          if (length(facteurs) == 1L && !is.na(l$p_value))
            l$Interpretation <- paste(l$Interpretation,
              tr("Avec un seul facteur, le résidu d'un groupe est la valeur centrée sur sa moyenne : W est le même que sur les données brutes."))
          lignes[[length(lignes) + 1L]] <- l
        }
      }
    }
  }
  out <- do.call(rbind, lignes)
  rownames(out) <- NULL
  out
}

# COMPARAISON A UNE VALEUR DE REFERENCE (NORME)

# Vocabulaire commun aux sorties.
.hstat_ref_alt_label <- function(alternative) {
  switch(alternative,
         "two.sided" = "bilatéral (différent de la référence)",
         "greater"   = "unilatéral (supérieur à la référence)",
         "less"      = "unilatéral (inférieur à la référence)",
         alternative)
}

# Interprétation rédigée d'un test de conformité.
.hstat_ref_interp <- function(label, p, estimate, mu, alternative,
                              unit = "", digits = 4) {
  if (is.na(p)) return("Résultat non disponible")
  sig <- p < 0.05
  sens <- if (alternative == "two.sided") {
    if (estimate > mu) "supérieure" else "inférieure"
  } else if (alternative == "greater") "supérieure" else "inférieure"
  if (sig)
    sprintf(paste0("%s : l'écart à la référence (%s) est significatif ",
                   "(p = %s). La valeur observée (%s%s) est %s à la norme."),
            label, format(mu), hstat_p_txt(p),
            format(round(estimate, digits)), unit, sens)
  else
    sprintf(paste0("%s : aucun écart significatif à la référence (%s) ",
                   "(p = %s). Les données sont compatibles avec la norme."),
            label, format(mu), hstat_p_txt(p))
}

# Enveloppe de sortie commune.
.hstat_ref_out <- function(test, statistic, parameter, p.value, estimate,
                           reference, conf.int, effect, effect_label,
                           alternative, n, interpretation, note = NA_character_) {
  list(test = test, statistic = statistic, parameter = parameter,
       p.value = p.value, estimate = estimate, reference = reference,
       conf.low = conf.int[1], conf.high = conf.int[2],
       effect = effect, effect_label = effect_label,
       alternative = alternative, n = n,
       interpretation = interpretation, note = note)
}

# Famille quantitative
HSTAT_WILCOX_IC_MAX <- 20000L

hstat_ref_test <- function(x, mu = 0, method = "ttest",
                           alternative = c("two.sided", "greater", "less"),
                           conf.level = 0.95, sigma = NULL, margin = NULL) {
  alternative <- match.arg(alternative)
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  n <- length(x)
  if (!is.finite(mu)) stop("La valeur de référence doit être un nombre.")
  if (n < 2) stop("Au moins 2 valeurs valides sont requises.")
  if (!conf.level > 0 || !conf.level < 1)
    stop("Le niveau de confiance doit être compris strictement entre 0 et 1.")
  m <- mean(x); s <- stats::sd(x)

  if (method == "ttest") {
    tr <- stats::t.test(x, mu = mu, alternative = alternative,
                        conf.level = conf.level)
    # d de Cohen a un echantillon : ecart a la norme en ecarts-types.
    d <- if (s > 0) (m - mu) / s else NA_real_
    return(.hstat_ref_out(
      "Test t (1 échantillon)", unname(tr$statistic), unname(tr$parameter),
      tr$p.value, m, mu, tr$conf.int, d, "d de Cohen", alternative, n,
      .hstat_ref_interp("Test t à un échantillon", tr$p.value, m, mu, alternative)))
  }

  if (method == "ztest") {
    if (is.null(sigma) || !is.finite(sigma) || sigma <= 0)
      stop("Le test z exige un écart-type de référence strictement positif.")
    se <- sigma / sqrt(n)
    z  <- (m - mu) / se
    p  <- switch(alternative,
                 "two.sided" = 2 * stats::pnorm(-abs(z)),
                 "greater"   = stats::pnorm(z, lower.tail = FALSE),
                 "less"      = stats::pnorm(z))
    q  <- stats::qnorm(1 - (1 - conf.level) / 2)
    ci <- switch(alternative,
                 "two.sided" = c(m - q * se, m + q * se),
                 "greater"   = c(m - stats::qnorm(conf.level) * se, Inf),
                 "less"      = c(-Inf, m + stats::qnorm(conf.level) * se))
    return(.hstat_ref_out(
      "Test z (1 échantillon, sigma connu)", z, NA_real_, p, m, mu, ci,
      (m - mu) / sigma, "d de Cohen", alternative, n,
      .hstat_ref_interp("Test z à un échantillon", p, m, mu, alternative),
      trf("Écart-type de référence sigma = %s.", format(sigma))))
  }

  if (method == "wilcoxon") {
    # L'INTERVALLE (pseudo-mediane de Hodges-Lehmann) se cherche par `uniroot`
    # en recalculant les rangs a chaque pas.
    avec_ic <- n <= HSTAT_WILCOX_IC_MAX
    tr <- suppressWarnings(stats::wilcox.test(
      x, mu = mu, alternative = alternative, conf.level = conf.level,
      conf.int = avec_ic, exact = FALSE, correct = TRUE))
    med <- stats::median(x)
    # r = Z / sqrt(n), Z reconstitue depuis la p-value bilaterale.
    z_eq <- stats::qnorm(min(max(tr$p.value / 2, 1e-300), 0.5), lower.tail = FALSE)
    r <- z_eq / sqrt(n)
    ci <- if (is.null(tr$conf.int)) c(NA_real_, NA_real_) else tr$conf.int
    return(.hstat_ref_out(
      "Wilcoxon signé (1 échantillon)", unname(tr$statistic), NA_real_,
      tr$p.value, med, mu, ci, r, "r (Z/racine(n))", alternative, n,
      .hstat_ref_interp("Test des rangs signés de Wilcoxon", tr$p.value, med, mu,
                        alternative),
      if (avec_ic)
        "Compare la médiane à la norme ; suppose une distribution symétrique."
      else
        trf("Compare la médiane à la norme ; suppose une distribution symétrique. Intervalle de confiance non calculé au-delà de %d valeurs (son calcul croît avec l'effectif) : la p-value porte sur toutes les valeurs.",
            HSTAT_WILCOX_IC_MAX)))
  }

  if (method == "sign") {
    d0 <- x[x != mu]
    k  <- sum(d0 > mu); nn <- length(d0)
    if (nn < 1) stop("Toutes les valeurs sont égales à la référence.")
    tr <- stats::binom.test(k, nn, p = 0.5, alternative = alternative,
                            conf.level = conf.level)
    med <- stats::median(x)
    return(.hstat_ref_out(
      "Test du signe (1 échantillon)", k, NA_real_, tr$p.value, med, mu,
      c(NA_real_, NA_real_), unname(tr$estimate), "Proportion au-dessus de la norme",
      alternative, n,
      .hstat_ref_interp("Test du signe", tr$p.value, med, mu, alternative),
      sprintf(paste0("%d valeurs au-dessus de la norme sur %d non nulles ",
                     "(%d ex aequo écartés). Aucune hypothèse de symétrie."),
              k, nn, n - nn)))
  }

  if (method == "variance") {
    if (is.null(sigma) || !is.finite(sigma) || sigma <= 0)
      stop("Le test de conformité d'une variance exige un écart-type de référence positif.")
    dfree <- n - 1
    chi2  <- dfree * s^2 / sigma^2
    p <- switch(alternative,
                "two.sided" = 2 * min(stats::pchisq(chi2, dfree),
                                      stats::pchisq(chi2, dfree, lower.tail = FALSE)),
                "greater"   = stats::pchisq(chi2, dfree, lower.tail = FALSE),
                "less"      = stats::pchisq(chi2, dfree))
    p <- min(p, 1)
    a <- (1 - conf.level) / 2
    ci <- c(dfree * s^2 / stats::qchisq(1 - a, dfree),
            dfree * s^2 / stats::qchisq(a, dfree))
    return(.hstat_ref_out(
      "Chi² de conformité (variance)", chi2, dfree, p, s^2, sigma^2, ci,
      s^2 / sigma^2, "Rapport de variances", alternative, n,
      .hstat_ref_interp("Test du Chi² de conformité d'une variance", p, s^2,
                        sigma^2, alternative),
      "Test sensible à la non-normalité : vérifier la normalité au préalable."))
  }

  if (method == "tost") {
    if (is.null(margin) || !is.finite(margin) || margin <= 0)
      stop("Le test d'équivalence exige une marge d'équivalence strictement positive.")
    # Deux tests unilateraux : H0 = |moyenne - norme| >= marge.
    lo <- stats::t.test(x, mu = mu - margin, alternative = "greater")
    hi <- stats::t.test(x, mu = mu + margin, alternative = "less")
    p  <- max(lo$p.value, hi$p.value)
    # Intervalle de confiance a 1 - 2*alpha : convention TOST.
    ci <- stats::t.test(x, conf.level = 1 - 2 * (1 - conf.level))$conf.int
    interp <- if (p < 0.05)
      sprintf(paste0("Équivalence démontrée (p = %s) : la moyenne observée (%s) ",
                     "est comprise dans la marge de +/- %s autour de la norme (%s)."),
              hstat_p_txt(p), format(round(m, 4)), format(margin),
              format(mu))
    else
      sprintf(paste0("Équivalence NON démontrée (p = %s) : on ne peut pas ",
                     "conclure que la moyenne observée (%s) reste dans la marge ",
                     "de +/- %s autour de la norme (%s)."),
              hstat_p_txt(p), format(round(m, 4)), format(margin),
              format(mu))
    return(.hstat_ref_out(
      "TOST (équivalence à la norme)",
      # Statistique du test unilateral CONTRAIGNANT (celui de plus grande
      # p-value : TOST conclut sur le maximum des deux).
      if (isTRUE(lo$p.value >= hi$p.value)) unname(lo$statistic) else unname(hi$statistic),
      unname(lo$parameter), p, m, mu, ci, (m - mu) / s, "d de Cohen",
      "two.sided", n, interp,
      paste0("Hypothèse nulle inversée : un p < 0,05 conclut à l'équivalence. ",
             "Intervalle affiché au niveau 1 - 2 alpha (convention TOST).")))
  }

  stop("Méthode de comparaison à une référence inconnue : ", method)
}

# Famille categorielle
# successes / n : effectif observe et effectif total.
hstat_ref_prop_test <- function(successes, n, p0 = 0.5, method = "binom",
                                alternative = c("two.sided", "greater", "less"),
                                conf.level = 0.95) {
  alternative <- match.arg(alternative)
  successes <- as.numeric(successes)[1]; n <- as.numeric(n)[1]
  if (!is.finite(successes) || !is.finite(n) || n <= 0 || successes < 0)
    stop("Effectifs invalides pour un test de conformité.")
  if (method != "poisson" && (successes > n))
    stop("L'effectif observé ne peut pas dépasser l'effectif total.")

  if (method == "poisson") {
    if (!is.finite(p0) || p0 <= 0)
      stop("Le taux de référence doit être strictement positif.")
    tr <- stats::poisson.test(round(successes), T = n, r = p0,
                              alternative = alternative, conf.level = conf.level)
    rate <- successes / n
    return(.hstat_ref_out(
      "Test de Poisson (taux vs référence)", round(successes), NA_real_, tr$p.value,
      rate, p0, tr$conf.int, rate / p0, "Rapport de taux", alternative, n,
      .hstat_ref_interp("Test de conformité d'un taux", tr$p.value, rate, p0,
                        alternative),
      "Suppose des événements indépendants et un taux constant."))
  }

  if (!is.finite(p0) || p0 <= 0 || p0 >= 1)
    stop("La proportion de référence doit être comprise strictement entre 0 et 1.")

  if (method == "binom") {
    tr <- stats::binom.test(round(successes), round(n), p = p0,
                            alternative = alternative, conf.level = conf.level)
    ph <- successes / n
    # h de Cohen : taille d'effet pour un ecart de proportions.
    h <- 2 * asin(sqrt(ph)) - 2 * asin(sqrt(p0))
    return(.hstat_ref_out(
      "Test binomial exact (proportion)", round(successes), NA_real_, tr$p.value,
      ph, p0, tr$conf.int, h, "h de Cohen", alternative, round(n),
      .hstat_ref_interp("Test binomial exact", tr$p.value, ph, p0, alternative),
      "Exact : valable quel que soit l'effectif."))
  }

  if (method == "prop") {
    tr <- suppressWarnings(stats::prop.test(
      round(successes), round(n), p = p0, alternative = alternative,
      conf.level = conf.level, correct = TRUE))
    ph <- successes / n
    h <- 2 * asin(sqrt(ph)) - 2 * asin(sqrt(p0))
    small <- n * p0 < 5 || n * (1 - p0) < 5
    return(.hstat_ref_out(
      "Chi² de conformité (proportion)", unname(tr$statistic),
      unname(tr$parameter), tr$p.value, ph, p0, tr$conf.int, h, "h de Cohen",
      alternative, round(n),
      .hstat_ref_interp("Test du Chi² de conformité d'une proportion",
                        tr$p.value, ph, p0, alternative),
      if (small)
        "Approximation normale peu fiable ici (n*p0 < 5) : préférer le test binomial exact."
      else "Approximation normale avec correction de continuité."))
  }

  stop("Méthode de conformité d'une proportion inconnue : ", method)
}

# Convertit une sortie hstat_ref_* en ligne du tableau de résultats de l'onglet
# « Tests statistiques » (mêmes colonnes que les autres tests).
hstat_ref_result_row <- function(res, variable, reference_label = NULL) {
  data.frame(
    Test        = res$test,
    Variable    = variable,
    Facteur     = if (is.null(reference_label))
                    trf("Référence = %s", format(res$reference))
                  else reference_label,
    Statistique = if (is.na(res$statistic)) NA_real_ else round(res$statistic, 4),
    ddl         = if (is.na(res$parameter)) NA_real_ else round(res$parameter, 2),
    p_value     = res$p.value,
    Interpretation = res$interpretation,
    stringsAsFactors = FALSE)
}

# TESTS APPARIES : un test a un echantillon sur les DIFFERENCES
# Deux mesures prises sur la MEME unite (avant / apres, gauche / droite, deux
# methodes sur le meme echantillon) ne sont pas deux groupes.
HSTAT_APPARIE_METHODES <- c(t = "ttest", wilcoxon = "wilcoxon", signe = "sign")

.hstat_apparie_nom <- function(methode) switch(methode,
  t        = tr("Test t apparié (Student)"),
  wilcoxon = tr("Wilcoxon apparié (rangs signés)"),
  signe    = tr("Test du signe apparié"),
  methode)

# -- Construire les paires
# Deux formes de
# fichier, et aucune ne se devine.
hstat_paires_large <- function(a, b) {
  a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
  if (length(a) != length(b))
    stop(hstat_refus(tr("Les deux colonnes n'ont pas le même nombre de lignes.")))
  ok <- is.finite(a) & is.finite(b)
  list(a = a[ok], b = b[ok], ecartees = sum(!ok), doublons = 0L,
       incomplets = character(0))
}

hstat_paires_longue <- function(y, groupe, sujet) {
  y <- suppressWarnings(as.numeric(y))
  g <- if (is.factor(groupe)) droplevels(groupe) else factor(as.character(groupe))
  s <- as.character(sujet)
  ok <- is.finite(y) & !is.na(g) & !is.na(s) & nzchar(s)
  y <- y[ok]; g <- droplevels(g[ok]); s <- s[ok]
  niv <- levels(g)
  if (length(niv) != 2L)
    stop(hstat_refus(trf("Un test apparié compare exactement deux conditions : le facteur en compte %d (%s). Pour plus de deux mesures par sujet, utilisez l'ANOVA à mesures répétées ou Friedman.",
                         length(niv), paste(utils::head(niv, 5), collapse = ", "))))
  cle <- paste(s, g, sep = "\r")
  nb <- table(cle)
  doublons <- sum(nb > 1L)
  moy <- tapply(y, list(s, g), mean)
  complets <- stats::complete.cases(moy)
  incomplets <- rownames(moy)[!complets]
  if (sum(complets) == 0L)
    stop(hstat_refus(tr("Aucun sujet n'est mesuré dans les deux conditions : vérifiez la colonne d'identifiant.")))
  list(a = unname(moy[complets, 1]), b = unname(moy[complets, 2]),
       niveaux = niv, sujets = rownames(moy)[complets],
       ecartees = sum(!ok) , doublons = doublons, incomplets = incomplets)
}

# -- Mesures repetees non parametriques : Friedman et Durbin.
hstat_rm_blocs <- function(y, groupe, sujet, complets = TRUE) {
  y <- suppressWarnings(as.numeric(y))
  g <- if (is.factor(groupe)) droplevels(groupe) else factor(as.character(groupe))
  s <- as.character(sujet)
  ok <- is.finite(y) & !is.na(g) & !is.na(s) & nzchar(s)
  y <- y[ok]; g <- droplevels(g[ok]); s <- s[ok]
  niv <- levels(g)
  if (length(niv) < 2L)
    stop(hstat_refus(trf("Le facteur intra-sujet ne compte qu'une modalité (%s) : il faut au moins deux périodes à comparer.",
                         paste(niv, collapse = ", "))))
  nb <- table(paste(s, g, sep = "\r"))
  doublons <- sum(nb > 1L)
  moy <- tapply(y, list(s, g), mean)
  presents <- rowSums(!is.na(moy))
  garde <- if (complets) presents == length(niv) else presents >= 2L
  ecartes <- rownames(moy)[!garde]
  if (sum(garde) < 2L)
    stop(hstat_refus(if (complets)
      trf("Moins de deux sujets sont mesurés à toutes les périodes (%d sur %d) : Friedman ne peut pas être calculé. Vérifiez la colonne « Sujet », ou choisissez Durbin, fait pour les blocs incomplets.",
          sum(garde), nrow(moy))
    else trf("Moins de deux sujets sont mesurés à au moins deux périodes (%d sur %d) : Durbin ne peut pas être calculé. Vérifiez la colonne « Sujet ».",
             sum(garde), nrow(moy))))
  m <- moy[garde, , drop = FALSE]
  # Durbin suppose un plan en blocs incomplets EQUILIBRE : meme nombre de
  # periodes par sujet, meme nombre de sujets par periode.
  if (!complets) {
    k <- rowSums(!is.na(m)); r <- colSums(!is.na(m))
    if (length(unique(k)) > 1L || length(unique(r)) > 1L)
      stop(hstat_refus(trf("Durbin exige un plan équilibré : chaque sujet mesuré au même nombre de périodes (ici %s) et chaque période au même nombre de sujets (ici %s). Choisissez Friedman, qui écarte les sujets incomplets, ou ART.",
                           paste(sort(unique(k)), collapse = ", "),
                           paste(sort(unique(r)), collapse = ", "))))
  }
  long <- data.frame(y = as.vector(m),
                     groupe = factor(rep(colnames(m), each = nrow(m)), levels = niv),
                     sujet = factor(rep(rownames(m), times = ncol(m))),
                     stringsAsFactors = FALSE)
  long <- long[!is.na(long$y), , drop = FALSE]
  list(y = long$y, groupe = long$groupe, sujet = long$sujet,
       niveaux = niv, n_sujets = sum(garde), doublons = doublons,
       ecartes = ecartes, lignes_ecartees = sum(!ok))
}

# Les notes de preparation, en phrases entieres.
hstat_rm_blocs_notes <- function(b) {
  n <- character(0)
  if (b$doublons > 0L)
    n <- c(n, trf("%d combinaison(s) sujet × période portaient plusieurs mesures : ramenées à leur moyenne.",
                  b$doublons))
  if (length(b$ecartes) > 0L)
    n <- c(n, trf("%d sujet(s) écarté(s), faute de mesures aux périodes requises : %s.",
                  length(b$ecartes), paste(utils::head(b$ecartes, 8), collapse = ", ")))
  n
}

# Le test lui-meme ne leve jamais : un echec rend une ligne qui DIT pourquoi.
hstat_rm_np <- function(y, groupe, sujet, methode = c("friedman", "durbin")) {
  methode <- match.arg(methode)
  nom <- if (methode == "friedman") "Friedman" else "Durbin"
  b <- tryCatch(hstat_rm_blocs(y, groupe, sujet, complets = methode == "friedman"),
                error = function(e) e)
  if (inherits(b, "error"))
    return(list(test = nom, statistic = NA_real_, ddl = NA_real_, p.value = NA_real_,
                n_sujets = NA_integer_, blocs = NULL, notes = character(0),
                motif = hstat_err_fr(b)))
  res <- tryCatch(
    if (methode == "friedman") stats::friedman.test(y = b$y, groups = b$groupe, blocks = b$sujet)
    else PMCMRplus::durbinTest(y = b$y, groups = b$groupe, blocks = b$sujet),
    error = function(e) e)
  if (inherits(res, "error"))
    return(list(test = nom, statistic = NA_real_, ddl = length(b$niveaux) - 1,
                p.value = NA_real_, n_sujets = b$n_sujets, blocs = b,
                notes = hstat_rm_blocs_notes(b), motif = hstat_err_fr(res)))
  ddl <- if (!is.null(res$parameter)) unname(res$parameter)[1] else length(b$niveaux) - 1
  list(test = nom, statistic = unname(res$statistic)[1], ddl = ddl,
       p.value = unname(res$p.value)[1], n_sujets = b$n_sujets, blocs = b,
       notes = hstat_rm_blocs_notes(b), motif = NA_character_)
}

# -- Le test ----------------------------------------------------------------------
# Rend une liste a plat, du meme esprit que `.hstat_ref_out()`, et ne leve
# jamais sur une donnee degeneree : des differences toutes egales rendent la
# statistique incalculable (ecart-type nul), ce qui se DIT plutot que de faire
# tomber la sortie -- la regle « ne jamais brancher sur une statistique non
# calculable ».
hstat_test_apparie <- function(a, b, methode = c("t", "wilcoxon", "signe"),
                               alternative = c("two.sided", "greater", "less"),
                               conf.level = 0.95, noms = c("Mesure 1", "Mesure 2")) {
  methode <- match.arg(methode); alternative <- match.arg(alternative)
  a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
  ok <- is.finite(a) & is.finite(b)
  a <- a[ok]; b <- b[ok]
  n <- length(a)
  if (n < 2L)
    stop(hstat_refus(trf("Un test apparié demande au moins 2 paires complètes ; %d disponible(s).", n)))
  d <- b - a
  nom <- .hstat_apparie_nom(methode)
  base <- list(test = nom, n = n, moyenne_a = mean(a), moyenne_b = mean(b),
               mediane_a = stats::median(a), mediane_b = stats::median(b),
               alternative = alternative, noms = noms)
  # Differences toutes egales : t (ecart-type nul) et Wilcoxon (rangs tous ex
  # aequo) n'ont pas de statistique.
  degenere <- if (methode == "signe") isTRUE(all(d == 0))
              else isTRUE(all(abs(d - d[1]) < 1e-12))
  res <- if (degenere) NULL else tryCatch(
    hstat_ref_test(d, mu = 0, method = HSTAT_APPARIE_METHODES[[methode]],
                   alternative = alternative, conf.level = conf.level),
    error = function(e) e)
  if (is.null(res) || inherits(res, "error")) {
    motif <- if (degenere && methode == "signe")
      tr("Toutes les différences sont nulles : le test du signe n'a aucune paire à compter.")
    else if (degenere)
      trf("Toutes les différences valent %s : la statistique n'est pas calculable. Les deux mesures diffèrent d'une constante sur chaque paire.",
          format(signif(d[1], 6)))
    else hstat_err_fr(res)
    return(c(base, list(statistic = NA_real_, parameter = NA_real_, p.value = NA_real_,
                        estimate = if (methode == "t") mean(d) else stats::median(d),
                        conf.low = NA_real_, conf.high = NA_real_,
                        effect = NA_real_, effect_label = NA_character_,
                        verdict = "indeterminable", interpretation = motif)))
  }
  verdict <- hstat_p_verdict(res$p.value)
  mesure <- if (methode == "t") tr("la différence moyenne") else tr("la différence médiane")
  # « p = < 2.2e-16 » se lirait mal : le signe fait partie du texte.
  pf <- hstat_p_txt(res$p.value)
  ptxt <- if (startsWith(pf, "<")) paste("p", pf) else paste("p =", pf)
  interp <- switch(verdict,
    # Deux phrases entieres et non un adjectif en argument.
    "significatif" = trf(if (res$estimate > 0)
                           "%s : %s (%s − %s) vaut %s et diffère significativement de 0 (%s). La mesure « %s » est supérieure à la mesure « %s »."
                         else
                           "%s : %s (%s − %s) vaut %s et diffère significativement de 0 (%s). La mesure « %s » est inférieure à la mesure « %s ».",
                         nom, mesure, noms[2], noms[1], format(signif(res$estimate, 4)),
                         ptxt, noms[2], noms[1]),
    "non significatif" = trf("%s : %s (%s − %s) vaut %s, sans différence significative entre les deux mesures (%s).",
                             nom, mesure, noms[2], noms[1], format(signif(res$estimate, 4)),
                             ptxt),
    tr("Résultat non disponible"))
  c(base, list(statistic = unname(res$statistic), parameter = unname(res$parameter),
               p.value = res$p.value, estimate = res$estimate,
               conf.low = res$conf.low, conf.high = res$conf.high,
               effect = res$effect,
               effect_label = switch(methode,
                 t        = tr("dz de Cohen (différence / écart-type des différences)"),
                 wilcoxon = tr("r (Z / racine de n)"),
                 signe    = tr("Proportion de différences positives")),
               verdict = verdict, interpretation = interp))
}

# Taille max des analyses a matrice de distances (dist, vegdist, silhouette,
# cophenetique).
HSTAT_DIST_MAX_N <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_DIST_MAX_N", "5000")))
  if (!is.finite(v) || v < 100) 5000L else v
}
# Taille max pour la correlation de Kendall (algorithme en O(n^2)).
HSTAT_KENDALL_MAX_N <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_KENDALL_MAX_N", "20000")))
  if (!is.finite(v) || v < 100) 20000L else v
}
# Taille max pour les imputations couteuses (kNN, missForest).
HSTAT_IMPUTE_MAX_N <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_IMPUTE_MAX_N", "100000")))
  if (!is.finite(v) || v < 1000) 100000L else v
}

# Notification (si session Shiny active) qu'une analyse a ete calculee sur un
# echantillon.
hstat_bigdata_note <- function(what, n_used, n_total) {
  if (is.null(shiny::getDefaultReactiveDomain())) return(invisible(NULL))
  shiny::showNotification(
    trf("%s : calcul sur un échantillon aléatoire de %s lignes (sur %s).",
            what, format(n_used, big.mark = " "), format(n_total, big.mark = " ")),
    type = "message", duration = 8)
  invisible(NULL)
}

# Indices d'un echantillon reproductible (seed locale, restauree ensuite).
hstat_cap_indices <- function(n0, max_n = HSTAT_DIST_MAX_N) {
  if (n0 <= max_n) return(seq_len(n0))
  old_seed <- if (exists(".Random.seed", envir = globalenv()))
    get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(20260712L)
  sort(sample.int(n0, max_n))
}

# Plafonne les lignes d'un data.frame/matrice (avec notification).
hstat_cap_df_rows <- function(df, max_n = HSTAT_DIST_MAX_N, what = "Analyse") {
  n0 <- nrow(df)
  if (is.null(n0) || n0 <= max_n) return(df)
  idx <- hstat_cap_indices(n0, max_n)
  hstat_bigdata_note(what, length(idx), n0)
  df[idx, , drop = FALSE]
}

# Plafonne conjointement une matrice de reponses Y et son facteur de groupes.
hstat_cap_Y_group <- function(Y, group, max_n = HSTAT_DIST_MAX_N,
                              what = "Analyse multivariée (distances)") {
  n0 <- nrow(Y)
  if (is.null(n0) || n0 <= max_n) return(list(Y = Y, group = group))
  idx <- hstat_cap_indices(n0, max_n)
  hstat_bigdata_note(what, length(idx), n0)
  list(Y = Y[idx, , drop = FALSE], group = droplevels(factor(group[idx])))
}

# Silhouette moyenne, robuste aux gros n.
hstat_silhouette_mean <- function(coords, clusters, max_n = HSTAT_DIST_MAX_N) {
  coords <- as.matrix(coords)
  cl <- as.integer(as.factor(clusters))
  n0 <- nrow(coords)
  if (n0 > max_n) {
    idx <- hstat_cap_indices(n0, max_n)
    coords <- coords[idx, , drop = FALSE]
    cl <- cl[idx]
    hstat_bigdata_note("Silhouette", length(idx), n0)
  }
  if (length(unique(cl)) < 2) return(NA_real_)
  sil <- tryCatch(cluster::silhouette(cl, stats::dist(coords)),
                  error = function(e) NULL)
  if (is.null(sil)) return(NA_real_)
  mean(sil[, 3])
}

# Correlation cophenetique, robuste aux gros n : exacte si possible, sinon
# estimee en re-ajustant une CAH sur un echantillon (meme methode).
hstat_cophenetic_corr <- function(coords, tree, hc_method = "ward.D2",
                                  max_n = HSTAT_DIST_MAX_N) {
  coords <- as.matrix(coords)
  n0 <- nrow(coords)
  if (n0 <= max_n) {
    ct <- tryCatch(stats::cophenetic(tree), error = function(e) NULL)
    if (!is.null(ct) && isTRUE(attr(ct, "Size") == n0)) {
      return(tryCatch(stats::cor(stats::dist(coords), ct),
                      error = function(e) NA_real_))
    }
  }
  idx <- hstat_cap_indices(n0, max_n)
  if (n0 > max_n) hstat_bigdata_note("Corrélation cophénétique", length(idx), n0)
  tryCatch({
    d  <- stats::dist(coords[idx, , drop = FALSE])
    hc <- stats::hclust(d, method = hc_method)
    stats::cor(d, stats::cophenetic(hc))
  }, error = function(e) NA_real_)
}

# Proportion de paires concordantes entre deux partitions (indice de Rand
# simple), vectorisee.
hstat_pair_agreement <- function(a, b) {
  sa <- outer(a, a, "==")
  sb <- outer(b, b, "==")
  ut <- upper.tri(sa)
  mean(sa[ut] == sb[ut])
}

# Test de correlation de Kendall robuste aux gros n : tau est en O(n^2)
# (plusieurs heures au-dela de ~100 000).
hstat_kendall_test <- function(x, y, max_n = HSTAT_KENDALL_MAX_N) {
  n0 <- length(x)
  if (n0 > max_n) {
    idx <- hstat_cap_indices(n0, max_n)
    x <- x[idx]; y <- y[idx]
    hstat_bigdata_note("Corrélation de Kendall", length(idx), n0)
  }
  stats::cor.test(x, y, method = "kendall")
}

# Valeur numerique finie ou valeur par defaut.
hstat_finite <- function(x, default) {
  x <- suppressWarnings(as.numeric(if (is.null(x)) default else x))
  if (length(x) != 1 || !is.finite(x)) default else x
}

# -- Filtre par valeur : la saisie est un TEXTE, jamais un motif.
HSTAT_FILTRE_VALEURS_MAX <- 500L
hstat_filtre_valeurs <- function(x, valeurs, exact = TRUE, casse = FALSE) {
  x <- as.character(x)
  valeurs <- unique(valeurs[!is.na(valeurs) & nzchar(valeurs)])
  plafond <- NULL
  if (!exact && length(valeurs) > HSTAT_FILTRE_VALEURS_MAX) {
    plafond <- trf("%d valeurs au-delà des %d premières ont été ignorées : une recherche « contient » se limite à %d valeurs.",
                   length(valeurs) - HSTAT_FILTRE_VALEURS_MAX,
                   HSTAT_FILTRE_VALEURS_MAX, HSTAT_FILTRE_VALEURS_MAX)
    valeurs <- valeurs[seq_len(HSTAT_FILTRE_VALEURS_MAX)]
  }
  if (!casse) { x <- tolower(x); valeurs <- tolower(valeurs) }
  m <- if (exact) x %in% valeurs
       else Reduce(`|`, lapply(valeurs, function(v) grepl(v, x, fixed = TRUE)),
                   logical(length(x)))
  m[is.na(x)] <- FALSE
  if (!is.null(plafond)) attr(m, "plafond") <- plafond
  m
}

# -- Lire un .rds venu de l'exterieur -----------------------------------------
# LA DESERIALISATION DE R N'EST PAS INOFFENSIVE AVANT R 4.4.0.
hstat_rds_autorise <- function(version = getRversion())
  version >= "4.4.0" || isTRUE(getOption("hstat.rds.autoriser"))

hstat_lire_rds <- function(path, version = getRversion()) {
  if (!hstat_rds_autorise(version))
    stop(hstat_refus(trf("Les fichiers .rds ne sont lus qu'à partir de R 4.4.0 (version installée : %s) : avant cette version, la lecture d'un fichier .rds peut exécuter du code qu'il contient. Exportez vos données en CSV ou en Excel, ou mettez R à jour. Pour une installation personnelle qui ne lit que vos propres fichiers : options(hstat.rds.autoriser = TRUE).",
                         as.character(version))))
  readRDS(path)
}

# -- Lire un classeur Excel venu de l'exterieur.
HSTAT_EXCEL_DECOMP_MAX  <- 500e6
HSTAT_EXCEL_RATIO_MAX   <- 100
HSTAT_EXCEL_RATIO_SEUIL <- 50e6

hstat_excel_controle <- function(path, decomp_max = HSTAT_EXCEL_DECOMP_MAX,
                                 ratio_max = HSTAT_EXCEL_RATIO_MAX,
                                 ratio_seuil = HSTAT_EXCEL_RATIO_SEUIL) {
  taille <- suppressWarnings(file.size(path))
  if (!isTRUE(is.finite(taille)) || taille <= 0) return(NULL)
  sig <- tryCatch(readBin(path, "raw", 4L), error = function(e) raw(0))
  if (!identical(sig, as.raw(c(0x50, 0x4b, 0x03, 0x04)))) return(NULL)
  tdm <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
  if (is.null(tdm))
    return(tr("Ce classeur Excel est illisible : l'archive qui le compose est endommagée."))
  decomp <- sum(as.numeric(tdm$Length), na.rm = TRUE)
  if (decomp > decomp_max)
    return(trf("Ce classeur occuperait %s Mo une fois décompressé (plafond : %s Mo) : sa lecture figerait l'application pour tous ses utilisateurs. Enregistrez-le en CSV, que l'application lit sans le charger entièrement.",
               format(round(decomp / 1e6), big.mark = " "),
               format(round(decomp_max / 1e6), big.mark = " ")))
  if (decomp > ratio_seuil && decomp / taille > ratio_max)
    return(trf("Ce classeur se décompresse d'un facteur %s (un classeur ordinaire : 5 à 20) : il n'est pas lu. Enregistrez vos données en CSV.",
               format(round(decomp / taille))))
  NULL
}

# La SEULE porte vers readxl::read_excel() pour un fichier venu de l'exterieur.
hstat_lire_excel <- function(path, sheet = 1, ...) {
  motif <- hstat_excel_controle(path)
  if (!is.null(motif)) stop(hstat_refus(motif))
  as.data.frame(readxl::read_excel(path, sheet = sheet, ...))
}

# -- Un refus de l'application, distinct d'une erreur de R --------------------
# `stop(hstat_refus(msg))`.
hstat_refus <- function(msg)
  structure(class = c("hstat_refus", "error", "condition"),
            list(message = msg, call = NULL))

# -- Table de contingence d'une AFC : une ligne n'est pas toujours UN individu
HSTAT_EFFECTIF_NOMS <- c("freq", "frequence", "effectif", "effectifs", "n",
                         "count", "counts", "nombre", "nb", "poids", "weight")

# Propose la colonne d'effectifs d'apres son nom, jamais d'apres ses valeurs.
hstat_effectif_propose <- function(d) {
  if (!is.data.frame(d) || !ncol(d)) return(NULL)
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  cle <- tolower(hstat_sans_accents(num))
  hit <- num[cle %in% HSTAT_EFFECTIF_NOMS]
  if (length(hit)) hit[1] else NULL
}

hstat_table_contingence <- function(d, ligne, colonne, poids = NULL) {
  if (!is.null(poids) && (!nzchar(poids) || identical(poids, "__aucun__"))) poids <- NULL
  r <- d[[ligne]]; k <- d[[colonne]]
  if (is.null(r) || is.null(k))
    return(list(ok = FALSE, msg = tr("Variable absente des données.")))
  w <- if (is.null(poids)) rep(1, nrow(d)) else suppressWarnings(as.numeric(d[[poids]]))
  if (is.null(w) || length(w) != nrow(d))
    return(list(ok = FALSE, msg = trf("Colonne d'effectifs « %s » absente des données.", poids)))
  ok <- !is.na(r) & !is.na(k) & is.finite(w)
  neg <- ok & w < 0
  ok <- ok & w >= 0
  ecartees <- sum(!ok)
  tab <- tapply(w[ok], list(factor(r[ok]), factor(k[ok])), sum)
  tab[is.na(tab)] <- 0
  tab <- as.table(tab)
  names(dimnames(tab)) <- c(ligne, colonne)
  notes <- character(0)
  if (!is.null(poids)) notes <- c(notes, trf(
    "Effectifs lus dans la colonne « %s » : chaque ligne compte pour sa valeur, pas pour un individu.", poids))
  if (sum(neg)) notes <- c(notes, trf("%d ligne(s) d'effectif négatif écartée(s).", sum(neg)))
  if (ecartees > sum(neg)) notes <- c(notes, trf("%d ligne(s) incomplète(s) écartée(s).", ecartees - sum(neg)))
  list(ok = TRUE, table = tab, poids = poids, notes = notes)
}

# -- Une table de contingence DEJA CONSTRUITE (format large).
hstat_table_contingence_large <- function(d, etiquettes = "__lignes__", colonnes) {
  colonnes <- intersect(as.character(colonnes), names(d))
  if (!is.null(etiquettes) && !identical(etiquettes, "__lignes__"))
    colonnes <- setdiff(colonnes, etiquettes)
  if (length(colonnes) < 2)
    return(list(ok = FALSE, msg = tr("Choisissez au moins deux colonnes d'effectifs : une par modalité de la variable-colonne.")))
  non_num <- colonnes[!vapply(d[colonnes], is.numeric, logical(1))]
  if (length(non_num))
    return(list(ok = FALSE, msg = trf("Colonne(s) non numérique(s) : %s. Les cases d'une table de contingence sont des effectifs.",
                                      paste(non_num, collapse = ", "))))
  m <- as.matrix(d[colonnes])
  neg <- colonnes[colSums(m < 0, na.rm = TRUE) > 0]
  if (length(neg))
    return(list(ok = FALSE, msg = trf("Effectif négatif dans : %s. Une table de contingence ne compte que des effectifs positifs ou nuls.",
                                      paste(neg, collapse = ", "))))
  if (is.null(etiquettes) || identical(etiquettes, "__lignes__")) {
    lab <- rownames(d); nom <- tr("Lignes")
  } else {
    if (is.null(d[[etiquettes]]))
      return(list(ok = FALSE, msg = tr("Variable absente des données.")))
    lab <- as.character(d[[etiquettes]]); nom <- etiquettes
  }
  lab[is.na(lab) | !nzchar(trimws(lab))] <- tr("(sans nom)")
  notes <- character(0)
  vides <- sum(is.na(m))
  if (vides) notes <- c(notes, trf("%d case(s) vide(s) comptée(s) comme zéro.", vides))
  m[is.na(m)] <- 0
  if (anyDuplicated(lab)) {
    notes <- c(notes, trf("%d ligne(s) de même étiquette additionnée(s).", sum(duplicated(lab))))
    m <- rowsum(m, factor(lab, levels = unique(lab)))
  } else rownames(m) <- lab
  tab <- as.table(m)
  names(dimnames(tab)) <- c(nom, tr("Colonnes"))
  notes <- c(notes, trf("Table de contingence lue telle quelle : %d ligne(s) x %d colonne(s).",
                        nrow(tab), ncol(tab)))
  list(ok = TRUE, table = tab, poids = NULL, notes = notes, ligne = nom, colonne = tr("Colonnes"))
}

hstat_garder_choix <- function(courant, choix, defaut = NULL) {
  v <- unname(as.character(unlist(choix, use.names = FALSE)))
  k <- intersect(as.character(courant), v)
  if (length(k)) return(k)
  d <- intersect(as.character(defaut), v)
  if (length(d)) d else NULL
}

# Un TEXTE venu du navigateur, borne en longueur.
HSTAT_TEXTE_CLIENT_MAX <- 500L
# SELECTION DE LIGNES (« 1-5, 8, 10 a 12 »)
# UNE SEULE DEFINITION.
hstat_lignes_selection <- function(selection_text, max_rows) {
  if (is.null(selection_text) || nchar(trimws(selection_text)) == 0) {
    stop("Veuillez entrer une sélection de lignes valide.")
  }
  
  text <- trimws(selection_text)
  text <- gsub("\\s+", " ", text)  
  
  # Remplacer différents formats de plage par un format uniforme.
  text <- gsub("(?i)(?<=[0-9])\\s*(?:to|[a\u00e0])\\s*(?=[0-9])", "-", text, perl = TRUE)
  
  parts <- strsplit(text, ",")[[1]]
  parts <- trimws(parts)
  
  # UN MASQUE, PAS UNE ACCUMULATION, et pour une raison mesuree.
  vus <- logical(max_rows)
  
  for (part in parts) {
    if (grepl("-", part)) {
      # C'est une plage (ex: "1-10" ou "20-30")
      range_parts <- strsplit(part, "-")[[1]]
      range_parts <- trimws(range_parts)
      
      if (length(range_parts) != 2) {
        stop(trf("Format de plage invalide : %s", part))
      }
      
      start <- as.numeric(range_parts[1])
      end <- as.numeric(range_parts[2])
      
      if (is.na(start) || is.na(end)) {
        stop(paste("Plage invalide :", part))
      }
      
      if (start > end) {
        stop(trf("La ligne de début doit être <= ligne de fin dans : %s", part))
      }
      
      if (start < 1 || end > max_rows) {
        stop(paste("Plage hors limites :", part, "(min: 1, max:", max_rows, ")"))
      }
      
      vus[start:end] <- TRUE
      
    } else {
      row_num <- as.numeric(part)
      
      if (is.na(row_num)) {
        stop(trf("Numéro de ligne invalide : %s", part))
      }
      
      if (row_num < 1 || row_num > max_rows) {
        stop(paste("Ligne hors limites :", row_num, "(min: 1, max:", max_rows, ")"))
      }
      
      vus[row_num] <- TRUE
    }
  }
  
  # `which()` rend les indices deja tries et sans doublon.
  return(which(vus))
}

hstat_texte_client <- function(x, max = HSTAT_TEXTE_CLIENT_MAX) {
  v <- if (is.null(x) || !length(x)) "" else as.character(x)[1]
  if (is.na(v)) v <- ""
  if (is.na(nchar(v, type = "chars", allowNA = TRUE))) return("")
  substr(v, 1L, max)
}

# Une valeur venue du navigateur, bornee des deux cotes : un plafond d'interface
# ne protege rien.
hstat_borne_client <- function(x, defaut, min, max) {

  v <- hstat_finite(x, defaut)
  min(max(v, min), max)
}

# -- Un indice de ligne ou de colonne venu du navigateur ----------------------
# UN entier, dans [1 ; n], ou NA.
hstat_indice_client <- function(v, n) {
  v <- suppressWarnings(as.numeric(v))
  if (length(v) != 1L || !is.finite(v) || v != round(v) || v < 1 || v > n)
    return(NA_integer_)
  as.integer(v)
}

# Lignes d'un tableau de saisie editable (DL50, rendement, dilutions).
HSTAT_SAISIE_MAX <- 1000L

# -- Detection du type de fichier ---------------------------------------------
hstat_file_kind <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "txt", "tsv"))      return("csv")
  if (ext %in% c("xlsx", "xls"))            return("excel")
  if (ext == "parquet")                     return("parquet")
  if (ext %in% c("duckdb", "ddb"))          return("duckdb")
  if (ext == "sav")                         return("sav")
  if (ext == "dta")                         return("dta")
  if (ext == "rds")                         return("rds")
  "inconnu"
}

# -- Taille du fichier (octets) -----------------------------------------------
hstat_file_size <- function(path) {
  s <- tryCatch(file.info(path)$size, error = function(e) NA_real_)
  if (is.na(s)) 0 else s
}
hstat_format_size <- function(bytes) {
  if (is.na(bytes) || bytes <= 0) return("0 o")
  u <- c("o", "Ko", "Mo", "Go", "To")
  i <- min(floor(log(bytes, 1024)), length(u) - 1)
  paste0(round(bytes / 1024^i, 1), " ", u[i + 1])
}

# -- Chemin compatible SQL (slash avant, apostrophes echappees) ---------------
hstat_sql_path <- function(path) {
  gsub("'", "''", gsub("\\\\", "/", path))
}

# Ce qui fait d'une chaine un GABARIT : un marqueur de sprintf.
HSTAT_I18N_KO_MAX <- 700

HSTAT_I18N_MARQUEUR <- "%[-0-9.]*[sdfgeix%]"

# -- Phrase COMPOSEE : on traduit le gabarit, jamais les arguments.
trf <- function(fmt, ..., lang = hstat_langue_session()) {
  g <- tr(fmt, lang)
  # [1] La traduction.
  out <- tryCatch(sprintf(g, ...), error = function(e) NULL)
  if (!is.null(out)) return(out)
  # [2] Le francais.
  out <- tryCatch(sprintf(fmt, ...), error = function(e) NULL)
  if (!is.null(out)) return(out)
  # [3] UN ARGUMENT FAUTIF PASSE DONC LES DEUX, et c'est le trou que [2]
  # laissait : le repli rejouait le MEME sprintf avec les MEMES valeurs.
  souple <- c(d = "%s", i = "%s")
  for (tpl in c(g, fmt)) {
    out <- tryCatch(sprintf(.hstat_pct_sub(tpl, souple), ...),
                    error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  # [4] Restent les fautes de NOMBRE d'arguments, qu'aucune substitution ne
  # repare.
  fmt
}

# LES TERMES DU FICHIER DE L'UTILISATEUR NE SE TRADUISENT JAMAIS
# La regle de longueur dans les cellules (LONGUEUR_CELLULE, cote navigateur)
# est une heuristique.
hstat_i18n_termes_donnees <- function(df, max_modalites = 200L,
                                      max_termes = 3000L, max_nchar = 60L) {
  if (!is.data.frame(df) || !ncol(df)) return(character(0))
  termes <- names(df)
  for (cn in names(df)) {
    if (length(termes) >= max_termes) break
    x <- df[[cn]]
    if (!(is.character(x) || is.factor(x) || is.logical(x))) next
    v <- unique(trimws(as.character(x)))
    v <- v[!is.na(v) & nzchar(v) & nchar(v) <= max_nchar]
    # Une colonne a trop de modalites est du texte libre, pas une variable
    # qualitative : ses valeurs ne risquent pas de coincider avec un libelle.
    if (length(v) > max_modalites) next
    termes <- c(termes, v)
  }
  termes <- unique(trimws(termes))
  termes <- termes[!is.na(termes) & nzchar(termes)]
  utils::head(termes, max_termes)
}

# JSON pret a envoyer au navigateur.
hstat_i18n_termes_json <- function(df, ...) {
  t <- tryCatch(hstat_i18n_termes_donnees(df, ...), error = function(e) character(0))
  if (!length(t)) return("[]")
  # Meme echappement que hstat_i18n_json().
  esc <- .hstat_json_esc
  paste0("[", paste(sprintf("\"%s\"", esc(t)), collapse = ","), "]")
}

# SEUILS D'EFFICACITE : COMPARAISON DE CHAQUE MODALITE AU TEMOIN
# Formule d'Abbott, celle qu'emploient l'agronomie, la phytopharmacie et
# l'entomologie pour dire de combien un traitement reduit ce que l'on mesure
# par rapport a un temoin non traite.
# 1. LE TEMOIN VAUT ZERO PAR DEFINITION, ET ON L'ECRIT.
# 2. UN TEMOIN NUL REND L'EFFICACITE INDEFINIE POUR TOUT LE MONDE.
# 3. UNE EFFICACITE NEGATIVE EST UN RESULTAT, PAS UNE ERREUR.
# 4. LE GROUPEMENT FACULTATIF EST CE QUI REND LA SUITE POSSIBLE.
HSTAT_EFF_AGG <- c("Moyenne" = "moyenne", "Médiane" = "mediane", "Somme" = "somme")

hstat_efficacite <- function(df, var_modalite, vars_reponse, temoin,
                             agg = c("moyenne", "mediane", "somme"),
                             var_repetition = NULL,
                             mode = c("cumul", "par_repetition"),
                             var_groupe = NULL) {
  agg <- match.arg(agg); mode <- match.arg(mode)
  # `var_groupe` est l'ancien argument.
  if (is.null(var_repetition) && !is.null(var_groupe)) {
    var_repetition <- var_groupe
    mode <- "par_repetition"
  }
  vide <- data.frame(Modalite = character(0), Variable = character(0),
                     Repetitions = integer(0), N = integer(0),
                     Valeur = numeric(0), Temoin = numeric(0),
                     Efficacite = numeric(0),
                     check.names = FALSE, stringsAsFactors = FALSE)
  msg <- function(x, m) { attr(x, "message") <- m; x }

  if (!is.data.frame(df) || !NROW(df))
    return(msg(vide, "Aucune donnée : chargez un jeu de données."))
  if (is.null(var_modalite) || !nzchar(var_modalite[1]) ||
      !(var_modalite[1] %in% names(df)))
    return(msg(vide, "Choisissez la variable qui porte les traitements."))
  vars_reponse <- intersect(as.character(vars_reponse), names(df))
  if (!length(vars_reponse))
    return(msg(vide, "Choisissez au moins une variable à mesurer."))

  modal <- trimws(as.character(df[[var_modalite[1]]]))
  modal[is.na(modal) | !nzchar(modal)] <- NA_character_
  temoin <- trimws(as.character(temoin)[1])
  if (is.na(temoin) || !nzchar(temoin) || !(temoin %in% modal))
    return(msg(vide, trf("Le témoin choisi n'existe pas dans « %s » : choisissez une modalité présente.", var_modalite[1])))

  a_rep <- !is.null(var_repetition) && nzchar(var_repetition[1]) &&
           var_repetition[1] %in% names(df)
  # SILENCE.
  if (!is.null(var_repetition) && nzchar(var_repetition[1]) && !a_rep)
    # `sprintf(paste0(...))` composait ce refus MORCEAU PAR MORCEAU.
    return(msg(vide, trf("La variable de répétition « %s » est introuvable dans les données : choisissez-en une autre.",
                         var_repetition[1])))
  rep_v <- if (a_rep) trimws(as.character(df[[var_repetition[1]]]))
           else rep("", NROW(df))
  rep_v[is.na(rep_v)] <- "(manquant)"

  # DEUX FACONS DE TENIR COMPTE DES REPETITIONS, ET ELLES NE REPONDENT PAS A LA
  # MEME QUESTION.
  grp <- if (identical(mode, "par_repetition")) rep_v else rep("", NROW(df))

  resume <- function(x) {
    x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
    if (!length(x)) return(c(n = 0L, v = NA_real_))
    c(n = length(x),
      v = switch(agg, moyenne = mean(x), mediane = stats::median(x), somme = sum(x)))
  }

  niveaux <- sort(unique(modal[!is.na(modal)]))
  lignes <- list(); alertes <- character(0); groupes_sans_temoin <- character(0)
  for (g in sort(unique(grp))) {
    dans_g <- grp == g
    for (v in vars_reponse) {
      y <- df[[v]]
      dans_temoin <- dans_g & !is.na(modal) & modal == temoin
      ref <- resume(y[dans_temoin])
      # DEUX CAUSES DIFFERENTES, DEUX MESSAGES.
      if (!any(dans_temoin))
        groupes_sans_temoin <- c(groupes_sans_temoin, g)
      else if (!is.finite(ref[["v"]]))
        alertes <- c(alertes, trf("témoin sans valeur mesurable pour « %s »", v))
      else if (ref[["v"]] == 0)
        alertes <- c(alertes, trf("témoin nul pour « %s » : l'efficacité n'est pas définissable", v))
      for (m in niveaux) {
        dans_m <- dans_g & !is.na(modal) & modal == m
        r <- resume(y[dans_m])
        # Nombre de repetitions REELLEMENT mesurees pour cette modalite : c'est
        # lui qui decide si une somme est comparable a une autre.
        nrep <- if (a_rep)
          length(unique(rep_v[dans_m & is.finite(suppressWarnings(as.numeric(y)))]))
          else NA_integer_
        # Decision 1 : le temoin ne se compare pas a lui-meme.
        eff <- if (identical(m, temoin)) 0
               # Decision 2 : jamais de division par zero silencieuse.
               else if (!is.finite(ref[["v"]]) || ref[["v"]] == 0 || !is.finite(r[["v"]])) NA_real_
               else (ref[["v"]] - r[["v"]]) * 100 / ref[["v"]]
        lignes[[length(lignes) + 1L]] <- data.frame(
          Groupe = g, Modalite = m, Variable = v,
          Repetitions = as.integer(nrep),
          N = as.integer(r[["n"]]),
          Valeur = unname(r[["v"]]), Temoin = unname(ref[["v"]]),
          Efficacite = eff,
          check.names = FALSE, stringsAsFactors = FALSE)
      }
    }
  }
  out <- do.call(rbind, lignes)
  # La colonne de groupe ne sert a rien quand il n'y a pas de groupement : la
  # garder vide ferait croire a une information absente.
  if (all(!nzchar(out$Groupe))) out$Groupe <- NULL
  # Une seule variable mesuree : la colonne « Variable » n'apprend rien.
  if (length(vars_reponse) == 1L) out$Variable <- NULL
  # Sans variable de repetition declaree, la colonne ne porte que des NA.
  if (all(is.na(out$Repetitions))) out$Repetitions <- NULL

  # LA SOMME N'EST COMPARABLE QUE SI LES REPETITIONS SONT EQUILIBREES.
  if (a_rep && identical(agg, "somme") && !is.null(out$Repetitions)) {
    nr <- out$Repetitions[!is.na(out$Repetitions) & out$Repetitions > 0]
    if (length(unique(nr)) > 1)
      alertes <- c(sprintf(paste0("répétitions inégales (%s) : une SOMME n'est pas ",
                                  "comparable entre modalités inégalement répétées, ",
                                  "choisissez la moyenne"),
                           paste(sort(unique(nr)), collapse = " / ")),
                   alertes)
  }
  rownames(out) <- NULL
  attr(out, "temoin") <- temoin
  attr(out, "agg") <- agg
  attr(out, "mode") <- mode
  attr(out, "repetition") <- if (a_rep) var_repetition[1] else NA_character_
  gst <- unique(groupes_sans_temoin)
  if (length(gst))
    alertes <- c(sprintf(paste0("le témoin « %s » est absent de %s groupe(s) (%s) : ",
                                "vérifiez la variable de groupement, un groupe sans ",
                                "témoin n'a rien à quoi se comparer"),
                         temoin, length(gst),
                         paste(utils::head(gst, 4), collapse = ", ")),
                 alertes)
  attr(out, "groupes_sans_temoin") <- gst
  msg(out, if (length(alertes))
    paste0("Attention : ", paste(unique(alertes), collapse = " ; "), ".")
    else trf("%s modalité(s) comparée(s) au témoin « %s » (%s).",
                 length(niveaux), temoin, agg))
}

# Modalites d'une colonne, temoin exclu : « une fois le temoin choisi, les
# autres modalites passent dans une variable ».
HSTAT_EFF_PREFIXE <- "Efficacite_"

hstat_eff_large <- function(res) {
  if (is.null(res) || !is.data.frame(res) || !NROW(res)) return(res)
  garder <- attributes(res)[setdiff(names(attributes(res)),
                                    c("names", "class", "row.names", "dim", "dimnames"))]
  # Sans colonne « Variable », une seule mesure a ete calculee : le tableau est
  # deja large, la colonne « Efficacite » suffit.
  if (!("Variable" %in% names(res)) || !("Efficacite" %in% names(res))) return(res)

  cles <- intersect(c("Groupe", "Modalite"), names(res))
  if (!length(cles)) return(res)

  vars <- unique(as.character(res$Variable))
  base <- unique(res[, cles, drop = FALSE])
  rownames(base) <- NULL
  ident <- function(d) do.call(paste, c(lapply(cles, function(k) as.character(d[[k]])),
                                        list(sep = "\r")))
  cle_base <- ident(base)

  for (v in vars) {
    sous <- res[as.character(res$Variable) == v, , drop = FALSE]
    base[[paste0(HSTAT_EFF_PREFIXE, v)]] <-
      sous$Efficacite[match(cle_base, ident(sous))]
  }
  # Le nombre de repetitions ne depend pas de la variable mesuree quand il est
  # constant ; on le garde alors, il documente le plan.
  if ("Repetitions" %in% names(res)) {
    rp <- res$Repetitions[match(cle_base, ident(res))]
    if (!all(is.na(rp))) base$Repetitions <- rp
  }
  for (a in names(garder)) attr(base, a) <- garder[[a]]
  attr(base, "variables") <- vars
  attr(base, "colonnes_efficacite") <- paste0(HSTAT_EFF_PREFIXE, vars)
  base
}

hstat_eff_modalites <- function(df, var_modalite, temoin = NULL) {
  if (!is.data.frame(df) || is.null(var_modalite) || !length(var_modalite) ||
      !(var_modalite[1] %in% names(df))) return(character(0))
  m <- trimws(as.character(df[[var_modalite[1]]]))
  m <- sort(unique(m[!is.na(m) & nzchar(m)]))
  if (is.null(temoin) || !length(temoin)) return(m)
  setdiff(m, trimws(as.character(temoin)[1]))
}

# RENDEMENT ET GAIN DE RENDEMENT
# Rendement (kg/ha) = masse recoltee (kg) / surface (ha) Gain (%) = (rendement
# du traitement - rendement du non traite) / rendement du non traite x 100
# 1. LES UNITES SE CONVERTISSENT PAR UNE BASE COMMUNE, jamais de proche en
#    proche.
# 2. UNE SURFACE NULLE REND LE RENDEMENT INDEFINI, PAS INFINI.
# 3. LE RENDEMENT GLOBAL N'EST PAS LA MOYENNE DES RENDEMENTS.
# 4. LE NON TRAITE VAUT ZERO DE GAIN PAR DEFINITION, ET ON L'ECRIT.

# Facteur de chaque unite vers l'unite de BASE (le kilogramme).
HSTAT_RDT_MASSE <- c(
  "milligramme (mg)"   = 1e-6,
  "gramme (g)"         = 1e-3,
  "kilogramme (kg)"    = 1,
  "quintal (100 kg)"   = 100,
  "tonne (1000 kg)"    = 1000,
  "livre (lb)"         = 0.45359237,
  "once (oz)"          = 0.028349523125)

# Facteur de chaque unite vers l'unite de BASE (le metre carre).
HSTAT_RDT_SURFACE <- c(
  "centimètre carré (cm²)" = 1e-4,
  "mètre carré (m²)"       = 1,
  "are (100 m²)"           = 100,
  "hectare (ha)"           = 10000,
  "kilomètre carré (km²)"  = 1e6,
  "acre"                   = 4046.8564224)

# LE SYMBOLE SE DECLARE, IL NE SE DEVINE PAS.
HSTAT_RDT_MASSE_SYM <- c(
  "milligramme (mg)" = "mg", "gramme (g)" = "g", "kilogramme (kg)" = "kg",
  "quintal (100 kg)" = "q", "tonne (1000 kg)" = "t", "livre (lb)" = "lb",
  "once (oz)" = "oz")

HSTAT_RDT_SURFACE_SYM <- c(
  "centimètre carré (cm²)" = "cm²", "mètre carré (m²)" = "m²",
  "are (100 m²)" = "a", "hectare (ha)" = "ha",
  "kilomètre carré (km²)" = "km²", "acre" = "acre")

# Symboles associes a une table de facteurs.
.hstat_rdt_sym <- function(table) {
  if (identical(names(table), names(HSTAT_RDT_MASSE))) HSTAT_RDT_MASSE_SYM
  else HSTAT_RDT_SURFACE_SYM
}

# Facteur d'une unite, ou NA quand elle est inconnue.
hstat_rdt_facteur <- function(unite, table) {
  u <- trimws(as.character(unite)[1])
  if (is.na(u) || !nzchar(u)) return(NA_real_)
  if (u %in% names(table)) return(unname(table[[u]]))
  sym <- .hstat_rdt_sym(table)
  i <- match(u, unname(sym))
  if (!is.na(i) && i <= length(table)) return(unname(table[[i]]))
  j <- match(tolower(u), tolower(names(table)))
  if (!is.na(j)) return(unname(table[[j]]))
  NA_real_
}

# Rendement, dans l'unite demandee.
hstat_rendement <- function(masse, surface,
                            unite_masse = "kilogramme (kg)",
                            unite_surface = "hectare (ha)",
                            sortie_masse = "kilogramme (kg)",
                            sortie_surface = "hectare (ha)") {
  masse   <- suppressWarnings(as.numeric(masse))
  surface <- suppressWarnings(as.numeric(surface))
  n <- max(length(masse), length(surface))
  masse   <- rep_len(masse, n)
  surface <- rep_len(surface, n)

  fm  <- hstat_rdt_facteur(unite_masse,   HSTAT_RDT_MASSE)
  fs  <- hstat_rdt_facteur(unite_surface, HSTAT_RDT_SURFACE)
  fom <- hstat_rdt_facteur(sortie_masse,   HSTAT_RDT_MASSE)
  fos <- hstat_rdt_facteur(sortie_surface, HSTAT_RDT_SURFACE)
  vide <- rep(NA_real_, n)
  if (anyNA(c(fm, fs, fom, fos)) || fom == 0 || fos == 0) {
    attr(vide, "invalides") <- 0L
    attr(vide, "message") <- "Unité inconnue : choisissez une unité proposée."
    return(vide)
  }

  # UNE SURFACE NULLE, NEGATIVE OU MANQUANTE N'EST PAS UNE SURFACE.
  ok <- is.finite(surface) & surface > 0
  out <- vide
  out[ok] <- (masse[ok] * fm) / (surface[ok] * fs) * fos / fom
  attr(out, "invalides") <- sum(!ok)
  out
}

# Libelle de l'unite de rendement, tel qu'il s'ecrit sur un axe.
hstat_rdt_unite_libelle <- function(sortie_masse = "kilogramme (kg)",
                                    sortie_surface = "hectare (ha)") {
  court <- function(x, sym) {
    x <- trimws(as.character(x)[1])
    if (is.na(x) || !nzchar(x)) return("?")
    if (x %in% names(sym)) return(unname(sym[[x]]))
    if (x %in% unname(sym)) return(x)
    x
  }
  paste0(court(sortie_masse, HSTAT_RDT_MASSE_SYM), "/",
         court(sortie_surface, HSTAT_RDT_SURFACE_SYM))
}

# Rendement par modalite : le global ET le moyen.
hstat_rdt_table <- function(df, var_modalite, var_masse, var_surface,
                            unite_masse = "kilogramme (kg)",
                            unite_surface = "hectare (ha)",
                            sortie_masse = "kilogramme (kg)",
                            sortie_surface = "hectare (ha)",
                            var_repetition = NULL) {
  vide <- data.frame(Modalite = character(0), N = integer(0),
                     Repetitions = integer(0),
                     Masse_totale = numeric(0), Surface_totale = numeric(0),
                     Rendement_global = numeric(0), Rendement_moyen = numeric(0),
                     Rendement_somme = numeric(0),
                     Ecart_type = numeric(0), Erreur_type = numeric(0),
                     check.names = FALSE, stringsAsFactors = FALSE)
  msg <- function(x, m) { attr(x, "message") <- m; x }

  if (!is.data.frame(df) || !NROW(df))
    return(msg(vide, "Aucune donnée : chargez un jeu de données."))
  # `trf()` ne traduit JAMAIS ses arguments.
  manque <- function(nm) is.null(nm) || !length(nm) || !nzchar(nm[1]) ||
                         !(nm[1] %in% names(df))
  if (manque(var_modalite))
    return(msg(vide, "Choisissez la variable qui porte les traitements."))
  if (manque(var_masse))
    return(msg(vide, "Choisissez la variable qui porte la masse récoltée."))
  if (manque(var_surface))
    return(msg(vide, "Choisissez la variable qui porte la surface."))

  modal <- trimws(as.character(df[[var_modalite[1]]]))
  modal[is.na(modal) | !nzchar(modal)] <- NA_character_

  rdt <- hstat_rendement(df[[var_masse[1]]], df[[var_surface[1]]],
                         unite_masse, unite_surface, sortie_masse, sortie_surface)
  if (!is.null(attr(rdt, "message"))) return(msg(vide, attr(rdt, "message")))
  invalides <- attr(rdt, "invalides") %||% 0L

  fm  <- hstat_rdt_facteur(unite_masse,   HSTAT_RDT_MASSE)
  fs  <- hstat_rdt_facteur(unite_surface, HSTAT_RDT_SURFACE)
  fom <- hstat_rdt_facteur(sortie_masse,   HSTAT_RDT_MASSE)
  fos <- hstat_rdt_facteur(sortie_surface, HSTAT_RDT_SURFACE)
  masse_out   <- suppressWarnings(as.numeric(df[[var_masse[1]]]))   * fm / fom
  surface_out <- suppressWarnings(as.numeric(df[[var_surface[1]]])) * fs / fos

  a_rep <- !is.null(var_repetition) && length(var_repetition) &&
           nzchar(var_repetition[1]) && var_repetition[1] %in% names(df)
  # SILENCE dans le module des efficacites : meme piege ici, meme refus.
  if (!is.null(var_repetition) && length(var_repetition) &&
      nzchar(var_repetition[1]) && !a_rep)
    return(msg(vide, trf("La variable de répétition « %s » est introuvable dans les données : choisissez-en une autre.",
                         var_repetition[1])))
  rep_v <- if (a_rep) trimws(as.character(df[[var_repetition[1]]])) else rep("", NROW(df))

  niveaux <- sort(unique(modal[!is.na(modal)]))
  if (!length(niveaux))
    return(msg(vide, trf("La variable « %s » ne porte aucune modalité exploitable.",
                         var_modalite[1])))

  lignes <- lapply(niveaux, function(m) {
    i <- which(!is.na(modal) & modal == m)
    r <- rdt[i]
    mt <- sum(masse_out[i][is.finite(masse_out[i])], na.rm = TRUE)
    st <- sum(surface_out[i][is.finite(surface_out[i]) & surface_out[i] > 0], na.rm = TRUE)
    ok <- is.finite(r)
    n_ok <- sum(ok)
    # LE RENDEMENT GLOBAL RAPPORTE LA MASSE TOTALE A LA SURFACE TOTALE.
    data.frame(
      Modalite = m,
      N = length(i),
      Repetitions = if (a_rep) length(unique(rep_v[i])) else NA_integer_,
      Masse_totale = mt,
      Surface_totale = st,
      Rendement_global = if (is.finite(st) && st > 0) mt / st else NA_real_,
      Rendement_moyen = if (n_ok) mean(r[ok]) else NA_real_,
      # LA SOMME DES RENDEMENTS DE CHAQUE REPETITION.
      Rendement_somme = if (n_ok) sum(r[ok]) else NA_real_,
      Ecart_type = if (n_ok > 1) stats::sd(r[ok]) else NA_real_,
      Erreur_type = if (n_ok > 1) stats::sd(r[ok]) / sqrt(n_ok) else NA_real_,
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, lignes)
  rownames(out) <- NULL
  if (!a_rep) out$Repetitions <- NULL

  attr(out, "unite") <- hstat_rdt_unite_libelle(sortie_masse, sortie_surface)
  # Les unites de sortie voyagent AVEC le tableau.
  attr(out, "sortie_masse") <- sortie_masse
  attr(out, "sortie_surface") <- sortie_surface
  attr(out, "invalides") <- invalides
  # LE DETAIL PAR LIGNE VOYAGE AVEC LE TABLEAU.
  attr(out, "detail") <- data.frame(
    Modalite   = modal,
    Repetition = if (a_rep) rep_v else NA_character_,
    Rendement  = as.numeric(rdt),
    check.names = FALSE, stringsAsFactors = FALSE)
  alertes <- character(0)
  if (invalides > 0)
    alertes <- c(alertes, trf("%d ligne(s) écartée(s) : surface nulle, négative ou manquante",
                              invalides))
  # LE GLOBAL ET LE MOYEN DIVERGENT QUAND LES SURFACES DIFFERENT.
  n_lignes <- vapply(niveaux, function(m) sum(!is.na(modal) & modal == m & is.finite(rdt)),
                     integer(1))
  if (length(unique(n_lignes)) > 1)
    alertes <- c(alertes, trf("le nombre de lignes exploitables varie d'une modalité à l'autre (%s) : la SOMME des rendements n'est pas comparable entre modalités, la moyenne et le rendement global le restent",
                              paste(range(n_lignes), collapse = " à ")))
  ecart <- suppressWarnings(max(abs(out$Rendement_global - out$Rendement_moyen) /
                                 pmax(abs(out$Rendement_global), 1e-12), na.rm = TRUE))
  if (isTRUE(is.finite(ecart)) && ecart > 0.01)
    alertes <- c(alertes, "les surfaces varient d'une répétition à l'autre : le rendement global (masse totale / surface totale) et le rendement moyen ne coïncident pas")
  msg(out, if (length(alertes))
    paste0("Attention : ", paste(alertes, collapse = " ; "), ".")
    else trf("%d modalité(s), rendement en %s.", nrow(out), attr(out, "unite")))
}

# LE RENDEMENT EST PARFOIS DEJA CALCULE, et l'exiger en masse + surface est
# alors une friction pure.
# 1. UNE COLONNE DE SURFACES.
# 2. DES SURFACES DECLAREES EGALES.
hstat_rdt_table_prete <- function(df, var_modalite, var_rendement,
                                  sortie_masse = "kilogramme (kg)",
                                  sortie_surface = "hectare (ha)",
                                  var_repetition = NULL,
                                  var_surface = NULL,
                                  surfaces_egales = FALSE) {
  vide <- data.frame(Modalite = character(0), N = integer(0),
                     Repetitions = integer(0),
                     Rendement_moyen = numeric(0), Rendement_somme = numeric(0),
                     Ecart_type = numeric(0), Erreur_type = numeric(0),
                     check.names = FALSE, stringsAsFactors = FALSE)
  msg <- function(x, m) { attr(x, "message") <- m; x }

  if (!is.data.frame(df) || !NROW(df))
    return(msg(vide, "Aucune donnée : chargez un jeu de données."))
  manque <- function(nm) is.null(nm) || !length(nm) || !nzchar(nm[1]) ||
                         !(nm[1] %in% names(df))
  if (manque(var_modalite))
    return(msg(vide, "Choisissez la variable qui porte les traitements."))
  if (manque(var_rendement))
    return(msg(vide, "Choisissez la variable qui porte le rendement déjà calculé."))

  modal <- trimws(as.character(df[[var_modalite[1]]]))
  modal[is.na(modal) | !nzchar(modal)] <- NA_character_
  rdt <- suppressWarnings(as.numeric(df[[var_rendement[1]]]))
  if (!any(is.finite(rdt)))
    return(msg(vide, trf("La variable « %s » ne porte aucun rendement exploitable : elle doit être numérique.",
                         var_rendement[1])))
  invalides <- sum(!is.finite(rdt) & !is.na(modal))

  a_rep <- !is.null(var_repetition) && length(var_repetition) &&
           nzchar(var_repetition[1]) && var_repetition[1] %in% names(df)
  if (!is.null(var_repetition) && length(var_repetition) &&
      nzchar(var_repetition[1]) && !a_rep)
    return(msg(vide, trf("La variable de répétition « %s » est introuvable dans les données : choisissez-en une autre.",
                         var_repetition[1])))
  rep_v <- if (a_rep) trimws(as.character(df[[var_repetition[1]]])) else rep("", NROW(df))

  niveaux <- sort(unique(modal[!is.na(modal)]))
  if (!length(niveaux))
    return(msg(vide, trf("La variable « %s » ne porte aucune modalité exploitable.",
                         var_modalite[1])))

  # LA SURFACE, QUAND ELLE EST DONNEE, PONDERE LE GLOBAL.
  a_surf <- !is.null(var_surface) && length(var_surface) && nzchar(var_surface[1]) &&
            var_surface[1] %in% names(df)
  if (!is.null(var_surface) && length(var_surface) && nzchar(var_surface[1]) && !a_surf)
    return(msg(vide, trf("La variable de surface « %s » est introuvable dans les données : choisissez-en une autre.",
                         var_surface[1])))
  surf <- if (a_surf) suppressWarnings(as.numeric(df[[var_surface[1]]])) else rep(NA_real_, NROW(df))
  if (a_surf && !any(is.finite(surf) & surf > 0))
    return(msg(vide, trf("La variable « %s » ne porte aucune surface exploitable : elle doit être numérique et strictement positive.",
                         var_surface[1])))
  # UNE SURFACE NULLE OU NEGATIVE NE PONDERE RIEN : elle est ecartee et comptee,
  # comme dans le chemin masse/surface.
  surf_ko <- if (a_surf) sum(!is.na(modal) & is.finite(rdt) & !(is.finite(surf) & surf > 0)) else 0L
  glob <- a_surf || isTRUE(surfaces_egales)

  lignes <- lapply(niveaux, function(m) {
    i <- which(!is.na(modal) & modal == m)
    r <- rdt[i]
    ok <- is.finite(r)
    n_ok <- sum(ok)
    data.frame(
      Modalite = m,
      N = length(i),
      Repetitions = if (a_rep) length(unique(rep_v[i])) else NA_integer_,
      # `NULL` DANS UN `data.frame()` N'OMET PAS LA COLONNE.
      Rendement_global = if (!glob) NA_real_
        else if (a_surf) {
          s_i <- surf[i]
          u <- ok & is.finite(s_i) & s_i > 0
          if (any(u)) sum(r[u] * s_i[u]) / sum(s_i[u]) else NA_real_
        } else if (n_ok) mean(r[ok]) else NA_real_,
      Rendement_moyen = if (n_ok) mean(r[ok]) else NA_real_,
      Rendement_somme = if (n_ok) sum(r[ok]) else NA_real_,
      Ecart_type = if (n_ok > 1) stats::sd(r[ok]) else NA_real_,
      Erreur_type = if (n_ok > 1) stats::sd(r[ok]) / sqrt(n_ok) else NA_real_,
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, lignes)
  rownames(out) <- NULL
  if (!a_rep) out$Repetitions <- NULL
  if (!glob) out$Rendement_global <- NULL

  attr(out, "unite") <- hstat_rdt_unite_libelle(sortie_masse, sortie_surface)
  attr(out, "sortie_masse") <- sortie_masse
  attr(out, "sortie_surface") <- sortie_surface
  attr(out, "invalides") <- invalides
  # LE DETAIL PAR LIGNE VOYAGE AVEC LE TABLEAU.
  attr(out, "detail") <- data.frame(
    Modalite   = modal,
    Repetition = if (a_rep) rep_v else NA_character_,
    Rendement  = as.numeric(rdt),
    check.names = FALSE, stringsAsFactors = FALSE)
  attr(out, "prete") <- TRUE
  attr(out, "global") <- if (!glob) "aucun" else if (a_surf) "pondere" else "surfaces_egales"
  alertes <- character(0)
  if (invalides > 0)
    alertes <- c(alertes, trf("%d ligne(s) écartée(s) : rendement illisible ou manquant",
                              invalides))
  # MEME PIEGE QUE LE TABLEAU CALCULE.
  n_lignes <- vapply(niveaux, function(m) sum(!is.na(modal) & modal == m & is.finite(rdt)),
                     integer(1))
  if (length(unique(n_lignes)) > 1)
    alertes <- c(alertes, trf("le nombre de lignes exploitables varie d'une modalité à l'autre (%s) : la SOMME des rendements n'est pas comparable entre modalités, la moyenne le reste",
                              paste(range(n_lignes), collapse = " à ")))
  if (a_surf && surf_ko > 0)
    alertes <- c(alertes, trf("%d ligne(s) sans surface exploitable : elles ne pondèrent pas le rendement global",
                              surf_ko))
  note <- if (a_surf)
      trf("%d modalité(s), rendement lu tel quel en %s. Le rendement global est pondéré par « %s ».",
          nrow(out), attr(out, "unite"), var_surface[1])
    else if (isTRUE(surfaces_egales))
      trf("%d modalité(s), rendement lu tel quel en %s. Surfaces déclarées égales : le rendement global vaut alors exactement la moyenne — c'est la même pondération, pas un second calcul.",
          nrow(out), attr(out, "unite"))
    else
      trf("%d modalité(s), rendement lu tel quel en %s. Sans surface ni déclaration d'égalité, le rendement global n'est pas calculable : il pondère par la surface.",
          nrow(out), attr(out, "unite"))
  msg(out, if (length(alertes))
    paste0("Attention : ", paste(alertes, collapse = " ; "), ".")
    else note)
}

# Insere une colonne juste apres une autre, en conservant l'ordre du reste.
.hstat_col_apres <- function(d, apres, nom, valeurs) {
  d[[nom]] <- valeurs
  nm <- setdiff(names(d), nom)
  i  <- match(apres, nm)
  if (is.na(i)) return(d)
  d[, append(nm, nom, after = i), drop = FALSE]
}

# Colonnes d'un tableau de rendement qui PORTENT UNE UNITE, donc les seules
# qui se convertissent.
HSTAT_RDT_COLS_CONV <- c("Rendement_global", "Rendement_moyen",
                         "Rendement_somme", "Ecart_type", "Erreur_type")

# Convertit un rendement d'un couple (masse, surface) vers un autre.
hstat_rdt_convertir_rendement <- function(x, de_masse, de_surface,
                                          vers_masse, vers_surface) {
  x   <- suppressWarnings(as.numeric(x))
  fdm <- hstat_rdt_facteur(de_masse,     HSTAT_RDT_MASSE)
  fds <- hstat_rdt_facteur(de_surface,   HSTAT_RDT_SURFACE)
  fvm <- hstat_rdt_facteur(vers_masse,   HSTAT_RDT_MASSE)
  fvs <- hstat_rdt_facteur(vers_surface, HSTAT_RDT_SURFACE)
  if (anyNA(c(fdm, fds, fvm, fvs)) || fvm == 0 || fds == 0)
    return(rep(NA_real_, length(x)))
  x * fdm / fvm * fvs / fds
}

# Ajoute au tableau de rendement les colonnes exprimees dans une AUTRE unite.
hstat_rdt_convertir_table <- function(res, vers_masse = NULL, vers_surface = NULL) {
  if (!is.data.frame(res) || !NROW(res)) return(res)
  # Une conversion non demandee n'est pas une erreur : c'est une OPTION, et le
  # defaut reste l'unite deja choisie.
  if (is.null(vers_masse) || !length(vers_masse) || !nzchar(vers_masse[1]) ||
      is.null(vers_surface) || !length(vers_surface) || !nzchar(vers_surface[1]))
    return(res)

  de_m <- attr(res, "sortie_masse")   %||% "kilogramme (kg)"
  de_s <- attr(res, "sortie_surface") %||% "hectare (ha)"
  u_conv <- hstat_rdt_unite_libelle(vers_masse[1], vers_surface[1])
  # Convertir vers l'unite qu'on a deja n'ajoute rien : la colonne serait le
  # doublon exact de son originale.
  if (identical(u_conv, attr(res, "unite"))) return(res)

  essai <- hstat_rdt_convertir_rendement(1, de_m, de_s, vers_masse[1], vers_surface[1])
  # Une unite inconnue ne doit pas vider les colonnes en silence : on ne
  # convertit pas, plutot que de multiplier par NA.
  if (!isTRUE(is.finite(essai))) return(res)

  garder <- attributes(res)[setdiff(names(attributes(res)),
                                    c("names", "class", "row.names"))]
  for (k in intersect(HSTAT_RDT_COLS_CONV, names(res)))
    res <- .hstat_col_apres(res, k, paste0(k, "_conv"),
                            hstat_rdt_convertir_rendement(res[[k]], de_m, de_s,
                                                          vers_masse[1], vers_surface[1]))
  # Les deux unites en colonnes : un CSV exporte doit se lire sans le tableau
  # de bord qui l'a produit.
  res$Unite <- attr(res, "unite") %||% hstat_rdt_unite_libelle(de_m, de_s)
  res$Unite_conv <- u_conv
  for (a in names(garder)) attr(res, a) <- garder[[a]]
  attr(res, "unite_conv") <- u_conv
  attr(res, "colonnes_conv") <- paste0(intersect(HSTAT_RDT_COLS_CONV, names(res)), "_conv")
  res
}

# Gain de rendement par rapport au programme non traite.
hstat_rdt_gain <- function(rendements, modalites = names(rendements),
                           non_traite = NULL) {
  r <- suppressWarnings(as.numeric(rendements))
  m <- trimws(as.character(modalites))
  if (!length(r) || length(m) != length(r)) {
    out <- rep(NA_real_, length(r))
    attr(out, "message") <- "Aucun rendement à comparer."
    return(out)
  }
  nt <- trimws(as.character(non_traite)[1])
  if (is.na(nt) || !nzchar(nt) || !(nt %in% m)) {
    out <- rep(NA_real_, length(r))
    attr(out, "message") <- "Choisissez le programme non traité parmi les modalités présentes."
    return(out)
  }
  ref <- r[match(nt, m)][1]
  if (!isTRUE(is.finite(ref))) {
    out <- rep(NA_real_, length(r))
    attr(out, "message") <- trf("Le rendement du programme non traité « %s » n'est pas calculable : le gain ne l'est pour aucune modalité.", nt)
    return(out)
  }
  if (ref == 0) {
    # DIVISER PAR ZERO RENDRAIT `Inf`, qui passerait pour un gain colossal.
    out <- rep(NA_real_, length(r))
    attr(out, "message") <- trf("Le rendement du programme non traité « %s » est nul : le gain est indéfini (division par zéro).", nt)
    return(out)
  }
  out <- (r - ref) / ref * 100
  # LE NON TRAITE VAUT ZERO PAR DEFINITION, et on l'ecrit.
  out[m == nt] <- 0
  attr(out, "reference") <- ref
  attr(out, "non_traite") <- nt
  attr(out, "message") <- trf("Gains calculés par rapport à « %s » (rendement de référence : %s).",
                              nt, format(signif(ref, 6), scientific = FALSE))
  out
}

# PERTES DE RECOLTE : CHAQUE MODALITE COMPAREE A UNE PROTECTION DE REFERENCE
# Deux formules, et ce sont les deux faces d'un meme nombre :

# Mesures de rendement sur lesquelles une perte se calcule.
HSTAT_RDT_MESURES_PERTE <- list(
  Rendement_global = c(col = "global",
    perte   = "Perte de rendement global vs « %s » (%%)",
    relatif = "Rendement relatif global vs « %s » (%%)"),
  Rendement_moyen = c(col = "moyen",
    perte   = "Perte de rendement moyen vs « %s » (%%)",
    relatif = "Rendement relatif moyen vs « %s » (%%)"),
  Rendement_somme = c(col = "somme",
    perte   = "Perte de rendement cumulé vs « %s » (%%)",
    relatif = "Rendement relatif cumulé vs « %s » (%%)"))

# UN POURCENTAGE EST SANS DIMENSION : ces colonnes ne se convertissent jamais.
HSTAT_RDT_PREFIXES_PCT <- c("Gain_", "Perte_", "Relatif_")

# Un nom de modalite vient du fichier : il porte des accents, des espaces, des
# parentheses.
.hstat_rdt_slug <- function(x) {
  s <- hstat_sans_accents(as.character(x))
  s <- gsub("[^A-Za-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  s[is.na(s) | !nzchar(s)] <- "ref"
  # « T 1 » et « T-1 » donnent le meme abrege.
  make.unique(s, sep = "_")
}

# Valeur de la modalite de reference, et le motif quand elle n'est pas
# utilisable.
.hstat_rdt_ref_valeur <- function(r, m, reference) {
  nom <- trimws(as.character(reference)[1])
  if (is.na(nom) || !nzchar(nom) || !(nom %in% m))
    return(list(ok = FALSE, nom = nom, message =
      "Choisissez la modalité de référence parmi les modalités présentes."))
  v <- r[match(nom, m)][1]
  if (!isTRUE(is.finite(v)))
    return(list(ok = FALSE, nom = nom, message = trf(
      "Le rendement de la référence « %s » n'est pas calculable : la perte ne l'est pour aucune modalité.", nom)))
  # DIVISER PAR ZERO RENDRAIT `Inf`, qui passerait pour une perte colossale.
  if (v == 0)
    return(list(ok = FALSE, nom = nom, message = trf(
      "Le rendement de la référence « %s » est nul : la perte est indéfinie (division par zéro).", nom)))
  list(ok = TRUE, nom = nom, valeur = v, message = "")
}

.hstat_rdt_ecart <- function(rendements, modalites, reference, quoi) {
  r <- suppressWarnings(as.numeric(rendements))
  m <- trimws(as.character(modalites))
  vide <- function(motif) {
    out <- rep(NA_real_, length(r)); attr(out, "message") <- motif; out
  }
  if (!length(r) || length(m) != length(r))
    return(vide("Aucun rendement à comparer."))
  g <- .hstat_rdt_ref_valeur(r, m, reference)
  if (!isTRUE(g$ok)) return(vide(g$message))
  ref <- g$valeur
  perte <- identical(quoi, "perte")
  out <- if (perte) (ref - r) / ref * 100 else r / ref * 100
  # LA REFERENCE VAUT 0 % DE PERTE ET 100 % DE RENDEMENT RELATIF PAR
  # DEFINITION.
  out[m == g$nom] <- if (perte) 0 else 100
  attr(out, "reference") <- ref
  attr(out, "reference_nom") <- g$nom
  attr(out, "message") <- trf(
    if (perte) "Pertes de rendement calculées par rapport à « %s » (rendement de référence : %s)."
    else       "Rendements relatifs calculés par rapport à « %s » (rendement de référence : %s).",
    g$nom, format(signif(ref, 6), scientific = FALSE))
  out
}

# Perte de rendement (%) de chaque modalite par rapport a `reference`.
hstat_rdt_perte <- function(rendements, modalites = names(rendements),
                            reference = NULL) {
  .hstat_rdt_ecart(rendements, modalites, reference, "perte")
}

# Rendement relatif (%) de chaque modalite par rapport a `reference`.
hstat_rdt_relatif <- function(rendements, modalites = names(rendements),
                              reference = NULL) {
  .hstat_rdt_ecart(rendements, modalites, reference, "relatif")
}

# Ajoute au tableau de rendement, pour CHAQUE reference demandee, la perte et
# le rendement relatif de chaque mesure disponible.

# Pertes calculees DANS chaque repetition, contre la reference de la meme
# repetition.
hstat_rdt_pertes_rep <- function(detail, reference) {
  vide <- data.frame(Modalite = character(0), Repetition = character(0),
                     Reference = character(0), Rendement = numeric(0),
                     Rendement_reference = numeric(0),
                     Perte = numeric(0), Relatif = numeric(0),
                     check.names = FALSE, stringsAsFactors = FALSE)
  msg <- function(x, m) { attr(x, "message") <- m; x }
  if (!is.data.frame(detail) || !NROW(detail))
    return(msg(vide, "Aucun rendement détaillé : la perte par répétition n'est pas calculable."))
  ref <- trimws(as.character(reference)[1])
  if (is.na(ref) || !nzchar(ref))
    return(msg(vide, "Choisissez la modalité de référence parmi les modalités présentes."))

  d <- detail
  d$Modalite   <- trimws(as.character(d$Modalite))
  d$Repetition <- trimws(as.character(d$Repetition))
  d$Rendement  <- suppressWarnings(as.numeric(d$Rendement))
  d <- d[!is.na(d$Modalite) & nzchar(d$Modalite), , drop = FALSE]
  # UNE REPETITION NON DECLAREE N'EN EST PAS UNE.
  if (!NROW(d) || all(is.na(d$Repetition)) || !any(nzchar(d$Repetition %||% "")))
    return(msg(vide, "Déclarez la variable de répétition : sans elle, la perte ne peut pas être calculée à l'intérieur de chaque répétition."))
  if (!(ref %in% d$Modalite))
    return(msg(vide, trf("La référence « %s » est absente des données détaillées.", ref)))

  blocs <- unique(d$Repetition[!is.na(d$Repetition) & nzchar(d$Repetition)])
  sans_ref <- character(0); ref_nulle <- character(0)
  out <- list()
  for (b in blocs) {
    i  <- which(d$Repetition == b)
    db <- d[i, , drop = FALSE]
    # La reference peut etre mesuree plusieurs fois dans un meme bloc : on
    # prend sa MOYENNE, seule valeur definie quand il y en a plusieurs.
    vr <- db$Rendement[db$Modalite == ref]
    vr <- vr[is.finite(vr)]
    # UN BLOC SANS REFERENCE EST UN DEFAUT DE PLAN, PAS DE MESURE, et il se
    # nomme.
    if (!length(vr)) { sans_ref <- c(sans_ref, b); next }
    r0 <- mean(vr)
    if (r0 == 0) { ref_nulle <- c(ref_nulle, b); next }
    perte   <- (r0 - db$Rendement) / r0 * 100
    relatif <- db$Rendement / r0 * 100
    # La reference vaut 0 % de perte et 100 % de rendement relatif DANS SON
    # PROPRE BLOC, par definition.
    perte[db$Modalite == ref]   <- 0
    relatif[db$Modalite == ref] <- 100
    out[[length(out) + 1L]] <- data.frame(
      Modalite = db$Modalite, Repetition = b, Reference = ref,
      Rendement = db$Rendement, Rendement_reference = r0,
      Perte = perte, Relatif = relatif,
      check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!length(out)) {
    m <- if (length(sans_ref))
      trf("Aucune répétition ne porte la référence « %s » : la perte par répétition n'est calculable nulle part.", ref)
    else trf("Le rendement de la référence « %s » est nul dans toutes les répétitions : la perte est indéfinie (division par zéro).", ref)
    return(msg(vide, m))
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  alertes <- character(0)
  if (length(sans_ref))
    alertes <- c(alertes, trf("%d répétition(s) sans la référence « %s », écartée(s) : %s.",
                              length(sans_ref), ref, paste(sans_ref, collapse = ", ")))
  if (length(ref_nulle))
    alertes <- c(alertes, trf("%d répétition(s) où la référence « %s » a un rendement nul, écartée(s) : %s.",
                              length(ref_nulle), ref, paste(ref_nulle, collapse = ", ")))
  attr(res, "reference") <- ref
  attr(res, "repetitions_ecartees") <- c(sans_ref, ref_nulle)
  attr(res, "message") <- if (length(alertes)) paste(alertes, collapse = " ") else
    trf("Pertes calculées dans chacune des %d répétitions, contre « %s » de la même répétition.",
        length(unique(res$Repetition)), ref)
  res
}

# Resume par modalite des pertes calculees par repetition : moyenne,
# dispersion et nombre de repetitions reellement comparables.
hstat_rdt_pertes_rep_resume <- function(rep_tab) {
  vide <- data.frame(Modalite = character(0), n = integer(0), Perte = numeric(0),
                     ET = numeric(0), ES = numeric(0), Relatif = numeric(0),
                     check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.data.frame(rep_tab) || !NROW(rep_tab)) return(vide)
  mods <- unique(rep_tab$Modalite)
  lignes <- lapply(mods, function(m) {
    x <- rep_tab$Perte[rep_tab$Modalite == m]
    y <- rep_tab$Relatif[rep_tab$Modalite == m]
    x <- x[is.finite(x)]; y <- y[is.finite(y)]
    n <- length(x)
    # UN ECART-TYPE SUR UNE SEULE VALEUR N'EXISTE PAS : `sd()` rend `NA`, et
    # c'est ce qu'il faut afficher.
    et <- if (n >= 2L) stats::sd(x) else NA_real_
    data.frame(Modalite = m, n = n,
               Perte = if (n) mean(x) else NA_real_,
               ET = et,
               ES = if (n >= 2L) et / sqrt(n) else NA_real_,
               Relatif = if (length(y)) mean(y) else NA_real_,
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, lignes)
  rownames(res) <- NULL
  res
}

# Colonnes du resume « par repetition ».
HSTAT_RDT_PERTE_REP <- list(
  Perte   = c(col = "Perte_rep",   lib = "Perte de rendement moyenne par répétition vs « %s » (%%)"),
  ET      = c(col = "Perte_rep_ET", lib = "Écart-type des pertes par répétition vs « %s » (%%)"),
  ES      = c(col = "Perte_rep_ES", lib = "Erreur-type des pertes par répétition vs « %s » (%%)"),
  n       = c(col = "Perte_rep_n",  lib = "Nombre de répétitions comparables à « %s »"),
  Relatif = c(col = "Relatif_rep",  lib = "Rendement relatif moyen par répétition vs « %s » (%%)"))

hstat_rdt_pertes_table <- function(res, references = NULL,
                                   mode = c("cumul", "par_repetition")) {
  mode <- match.arg(mode)
  if (!is.data.frame(res) || !NROW(res)) return(res)
  refs <- trimws(as.character(references %||% character(0)))
  refs <- unique(refs[!is.na(refs) & nzchar(refs)])
  if (!length(refs)) return(res)

  mods    <- as.character(res$Modalite)
  mesures <- intersect(names(HSTAT_RDT_MESURES_PERTE), names(res))
  if (!length(mesures)) return(res)

  # Les attributs ne survivent pas a l'ajout de colonnes : on les remet.
  garder <- attributes(res)[setdiff(names(attributes(res)),
                                    c("names", "class", "row.names"))]
  slugs <- .hstat_rdt_slug(refs)
  libelles <- character(0)
  messages <- character(0)
  # UNE REFERENCE ABSENTE SE NOMME.
  absentes <- refs[!(refs %in% mods)]

  detail <- attr(res, "detail")
  reps   <- list()

  for (i in seq_along(refs)) {
    if (!(refs[i] %in% mods)) next

    if (identical(mode, "par_repetition")) {
      # LE MODE EST UN AIGUILLAGE, PAS UN AJOUT.
      rt <- hstat_rdt_pertes_rep(detail, refs[i])
      messages <- c(messages, attr(rt, "message"))
      if (!NROW(rt)) next
      reps[[refs[i]]] <- rt
      som <- hstat_rdt_pertes_rep_resume(rt)
      j <- match(mods, som$Modalite)
      for (champ in names(HSTAT_RDT_PERTE_REP)) {
        spec <- HSTAT_RDT_PERTE_REP[[champ]]
        cn <- paste0(unname(spec[["col"]]), "_vs_", slugs[i])
        res[[cn]] <- som[[champ]][j]
        libelles[cn] <- trf(unname(spec[["lib"]]), refs[i])
      }
      next
    }

    for (k in mesures) {
      spec  <- HSTAT_RDT_MESURES_PERTE[[k]]
      court <- unname(spec[["col"]])
      p  <- hstat_rdt_perte(res[[k]],   mods, refs[i])
      q  <- hstat_rdt_relatif(res[[k]], mods, refs[i])
      cp <- paste0("Perte_",    court, "_vs_", slugs[i])
      cr <- paste0("Relatif_",  court, "_vs_", slugs[i])
      res[[cp]] <- as.numeric(p)
      res[[cr]] <- as.numeric(q)
      libelles[cp] <- trf(unname(spec[["perte"]]),   refs[i])
      libelles[cr] <- trf(unname(spec[["relatif"]]), refs[i])
      if (identical(k, mesures[1]))
        messages <- c(messages, attr(p, "message"))
    }
  }

  for (a in names(garder)) attr(res, a) <- garder[[a]]
  attr(res, "references_perte")  <- refs[refs %in% mods]
  attr(res, "libelles_perte")    <- libelles
  attr(res, "message_perte")     <- paste(messages, collapse = " ")
  attr(res, "mode_perte")        <- mode
  # Le detail part AVEC le tableau : c'est lui qui rend les pertes analysables
  # ailleurs (ANOVA, comparaisons multiples) et exportables.
  attr(res, "pertes_repetitions") <- if (length(reps)) reps else NULL
  attr(res, "references_absentes") <- absentes
  res
}
# Tableau complet : rendements ET gains, global et moyen.
hstat_rdt_complet <- function(df, var_modalite, var_masse, var_surface,
                              non_traite,
                              unite_masse = "kilogramme (kg)",
                              unite_surface = "hectare (ha)",
                              sortie_masse = "kilogramme (kg)",
                              sortie_surface = "hectare (ha)",
                              var_repetition = NULL,
                              conv_masse = NULL, conv_surface = NULL,
                              var_rendement = NULL,
                              rdt_var_surface = NULL,
                              rdt_surfaces_egales = FALSE,
                              references = NULL,
                              mode_perte = c("cumul", "par_repetition")) {
  mode_perte <- match.arg(mode_perte)
  # Le rendement DEJA CALCULE court-circuite la pesee : meme tableau, memes
  # gains, memes graphiques.
  prete <- !is.null(var_rendement) && length(var_rendement) && nzchar(var_rendement[1])
  res <- if (prete)
    hstat_rdt_table_prete(df, var_modalite, var_rendement,
                          sortie_masse, sortie_surface, var_repetition,
                          rdt_var_surface, rdt_surfaces_egales)
  else
    hstat_rdt_table(df, var_modalite, var_masse, var_surface,
                    unite_masse, unite_surface, sortie_masse,
                    sortie_surface, var_repetition)
  if (!NROW(res)) return(res)
  g_moy  <- hstat_rdt_gain(res$Rendement_moyen,  res$Modalite, non_traite)
  g_som  <- hstat_rdt_gain(res$Rendement_somme,  res$Modalite, non_traite)
  g_glob <- if ("Rendement_global" %in% names(res))
    hstat_rdt_gain(res$Rendement_global, res$Modalite, non_traite) else NULL
  if (!is.null(g_glob)) res$Gain_global <- as.numeric(g_glob)
  res$Gain_moyen  <- as.numeric(g_moy)
  res$Gain_somme  <- as.numeric(g_som)
  # Les pertes viennent avec les gains, et AVANT la conversion : ce sont des
  # pourcentages, donc sans dimension.
  res <- hstat_rdt_pertes_table(res, references, mode_perte)
  # La conversion vient APRES les gains.
  res <- hstat_rdt_convertir_table(res, conv_masse, conv_surface)
  garder_p <- attributes(res)[grep("^(references_perte|libelles_perte|message_perte|references_absentes|mode_perte|pertes_repetitions)$",
                                  names(attributes(res)))]
  attr(res, "non_traite") <- non_traite
  attr(res, "message_gain") <- attr(g_glob %||% g_moy, "message")
  for (a in names(garder_p)) attr(res, a) <- garder_p[[a]]
  res
}

# SAISIE DE LA RECOLTE : COLLAGE DEPUIS UN TABLEUR
# Trois colonnes attendues -- modalite, masse, surface -- et une quatrieme
# facultative pour la repetition.
# 1. LA PREMIERE COLONNE EST DU TEXTE.
# 2. LA VIRGULE NE PEUT PAS ETRE A LA FOIS SEPARATEUR ET DECIMALE.
hstat_rdt_coller <- function(txt) {
  echec <- function(motif) list(ok = FALSE, message = motif)
  vide <- is.null(txt) || !length(txt) || !nzchar(trimws(paste(txt, collapse = "")))
  if (vide)
    return(echec(tr("Rien à coller : copiez au moins trois colonnes (modalité, masse, surface) depuis votre tableur.")))

  lignes <- trimws(unlist(strsplit(paste(txt, collapse = "\n"), "[\r\n]+")))
  lignes <- lignes[nzchar(lignes)]
  if (!length(lignes))
    return(echec(tr("Rien à coller : copiez au moins trois colonnes (modalité, masse, surface) depuis votre tableur.")))

  franc <- any(grepl("[\t;]", lignes))
  motif <- if (franc) "[\t;]+" else "[\t;,]+"
  champs <- function(li) {
    v <- trimws(strsplit(li, motif)[[1]])
    v[!is.na(v)]
  }
  nombre <- function(x) {
    x <- trimws(as.character(x))
    if (franc) x <- gsub(",", ".", x, fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }

  # L'EN-TETE SE RECONNAIT SUR LES COLONNES QUI DOIVENT ETRE NUMERIQUES, pas
  # sur la ligne entiere.
  entete <- function(li) {
    v <- champs(li)
    length(v) >= 3 && !any(is.finite(nombre(v[2:3])))
  }
  # ET L'EN-TETE ECARTE SE DIT.
  saute <- entete(lignes[1])
  if (saute) lignes <- lignes[-1]
  if (!length(lignes))
    return(echec(tr("Le collage ne contient que des en-têtes : il manque les lignes de données.")))

  # Le collage voyage entier vers le navigateur une fois range dans le tableau :
  # sans borne, cent mille lignes le figeaient (HSTAT_SAISIE_MAX).
  if (length(lignes) > HSTAT_SAISIE_MAX)
    return(echec(trf("Le collage compte %d lignes : le tableau de saisie en accepte %d au plus.",
                     length(lignes), HSTAT_SAISIE_MAX)))
  mat <- lapply(lignes, champs)
  larg <- vapply(mat, length, integer(1))
  if (any(larg < 3L))
    return(echec(trf("Ligne(s) à moins de trois colonnes : %s. Il faut la modalité, la masse récoltée et la surface.",
                     paste(which(larg < 3L), collapse = ", "))))

  pris <- function(i) vapply(mat, function(v) if (length(v) >= i) v[i] else NA_character_,
                             character(1))
  d <- data.frame(
    Modalite = trimws(pris(1)),
    Masse    = nombre(pris(2)),
    Surface  = nombre(pris(3)),
    Repetition = if (any(larg >= 4L)) trimws(pris(4)) else NA_character_,
    stringsAsFactors = FALSE)

  mauvaises <- which(!nzchar(d$Modalite) | !is.finite(d$Masse) | !is.finite(d$Surface))
  if (length(mauvaises))
    return(echec(trf("Valeur illisible ligne(s) %s : la modalité doit être renseignée, la masse et la surface doivent être des nombres.",
                     paste(mauvaises, collapse = ", "))))
  # Une repetition vide partout ne merite pas sa colonne : elle ferait croire a
  # une information absente.
  if (all(is.na(d$Repetition) | !nzchar(d$Repetition))) d$Repetition <- NA_character_

  list(ok = TRUE, table = d, lignes = nrow(d), entete = saute,
       decimale = if (franc) "," else ".")
}

# Table de saisie vide : `n` lignes prêtes à recevoir une récolte.
hstat_rdt_saisie_vide <- function(n = 6L) {
  n <- as.integer(n)[1]
  if (!isTRUE(is.finite(n)) || n < 0L) n <- 0L
  data.frame(Modalite = rep(NA_character_, n),
             Masse = rep(NA_real_, n),
             Surface = rep(NA_real_, n),
             Repetition = rep(NA_character_, n),
             stringsAsFactors = FALSE)
}

# Ce qui, dans une table de saisie, est REELLEMENT exploitable.
hstat_rdt_saisie_propre <- function(d) {
  if (!is.data.frame(d) || !NROW(d)) return(hstat_rdt_saisie_vide(0L))
  for (k in c("Modalite", "Repetition"))
    if (k %in% names(d)) d[[k]] <- trimws(as.character(d[[k]]))
  for (k in c("Masse", "Surface"))
    if (k %in% names(d)) d[[k]] <- suppressWarnings(as.numeric(d[[k]]))
  ok <- !is.na(d$Modalite) & nzchar(d$Modalite) &
        is.finite(d$Masse) & is.finite(d$Surface)
  out <- d[ok, , drop = FALSE]
  rownames(out) <- NULL
  if ("Repetition" %in% names(out) &&
      all(is.na(out$Repetition) | !nzchar(out$Repetition)))
    out$Repetition <- NULL
  out
}

# VARIABLES ENTIEREMENT NULLES
# Une colonne dont TOUTES les valeurs observees valent zero ne porte aucune
# information.
# 1. LES MANQUANTS NE COMPTENT PAS COMME DES ZEROS.
# 2. UNE COLONNE ENTIEREMENT VIDE N'EST PAS UNE COLONNE DE ZEROS.
# 3. LES BOOLEENS SONT ECARTES.
hstat_vars_zero <- function(df, seuil = 1) {
  vide <- data.frame(Variable = character(0), Type = character(0),
                     Observations = integer(0), Zeros = integer(0),
                     `Zeros (%)` = numeric(0), Manquants = integer(0),
                     Constat = character(0),
                     check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.data.frame(df) || !ncol(df)) {
    attr(vide, "vides") <- character(0)
    return(vide)
  }
  seuil <- suppressWarnings(as.numeric(seuil)[1])
  if (!is.finite(seuil)) seuil <- 1

  lignes <- list(); vides <- character(0)
  for (cn in names(df)) {
    x <- df[[cn]]
    # Decision 2 AVANT la 3, et sur le TEXTE VIDE autant que sur NA.
    plat <- if (is.character(x) || is.factor(x)) trimws(as.character(x)) else x
    # `&` EST VECTORISE : SES DEUX COTES SONT EVALUES.
    vide_txt <- if (is.character(plat)) !nzchar(plat) else FALSE
    if (all(is.na(plat) | vide_txt)) {
      vides <- c(vides, cn); next
    }
    # Decision 3 : un booleen renseigne est une reponse, pas une mesure.
    if (is.logical(x)) next
    num <- if (is.numeric(x)) as.numeric(x) else hstat_as_numeric_fr(x)
    if (is.null(num)) next                       # colonne non numerisable
    n_na <- sum(is.na(num))
    obs <- num[!is.na(num)]
    if (!length(obs)) { vides <- c(vides, cn); next }
    n_zero <- sum(obs == 0)
    part <- n_zero / length(obs)
    if (part < seuil) next
    lignes[[length(lignes) + 1L]] <- data.frame(
      Variable = cn,
      Type = if (is.numeric(x)) "numérique"
             else if (is.character(x) || is.factor(x)) "texte" else class(x)[1],
      Observations = length(obs),
      Zeros = n_zero,
      `Zeros (%)` = round(part * 100, 2),
      Manquants = n_na,
      Constat = if (part >= 1 && n_na == 0)
        "Toutes les valeurs sont nulles : la variable n'apporte aucune information."
      else if (part >= 1)
        sprintf(paste0("Toutes les valeurs observées sont nulles ; %s valeur(s) ",
                       "manquante(s). Vérifiez si le zéro signifie ici ",
                       "« non mesuré »."), n_na)
      else
        trf("%s %% des valeurs observées sont nulles.", round(part * 100, 2)),
      check.names = FALSE, stringsAsFactors = FALSE)
  }
  res <- if (length(lignes)) do.call(rbind, lignes) else vide
  attr(res, "vides") <- vides
  res
}

# -- Saisie directe des valeurs d'une variable.
hstat_zero_valeurs_parse <- function(txt, n) {
  n <- suppressWarnings(as.integer(n)[1])
  if (!is.finite(n) || n < 1)
    return(list(ok = FALSE, valeurs = NULL,
                message = "Aucune ligne à remplir : chargez des données."))
  if (is.null(txt) || !length(txt)) txt <- ""
  brut <- unlist(strsplit(paste(txt, collapse = "\n"), "[\n;]"))
  brut <- trimws(brut)
  # Une saisie se termine souvent par un retour a la ligne : la case vide
  # finale ne doit pas etre comptee comme une valeur manquante de plus.
  while (length(brut) && !nzchar(brut[length(brut)])) brut <- brut[-length(brut)]
  if (!length(brut))
    return(list(ok = FALSE, valeurs = NULL,
                message = "Saisie vide : entrez une valeur par ligne."))
  if (length(brut) != n)
    return(list(ok = FALSE, valeurs = NULL,
                message = sprintf(paste0("%s valeur(s) saisie(s) pour %s ",
                                         "observation(s) : ajustez la liste ",
                                         "pour qu'elles correspondent une à une."),
                                  length(brut), n)))
  manquant <- !nzchar(brut) | toupper(brut) %in% c("NA", "N/A")
  num <- suppressWarnings(as.numeric(gsub(",", ".", brut, fixed = TRUE)))
  mauvais <- which(!manquant & is.na(num))
  if (length(mauvais))
    return(list(ok = FALSE, valeurs = NULL,
                message = sprintf(paste0("Valeur non numérique en position %s ",
                                         "(« %s ») : corrigez-la ou écrivez NA."),
                                  mauvais[1], brut[mauvais[1]])))
  num[manquant] <- NA_real_
  list(ok = TRUE, valeurs = num, message = trf("%s valeur(s) prise(s) en compte.", n))
}

# -- Creation de classes d'intervalles (discretisation).
hstat_cut_intervals <- function(x, method = c("width", "quantile", "manual"),
                                n_classes = 4, breaks_manual = NULL,
                                labels_custom = NULL,
                                interval_style = c("std_last_closed", "all_left_closed",
                                                   "mixed_open", "all_right_closed", "all_closed"),
                                right = FALSE, dig = 3) {
  interval_style <- match.arg(interval_style)
  method <- match.arg(method)
  xnum <- if (is.numeric(x)) as.numeric(x) else hstat_as_numeric_fr(x)
  if (is.null(xnum))
    return(list(ok = FALSE, msg = "La variable n'est pas numérique (conversion impossible)."))
  xv <- xnum[is.finite(xnum)]
  if (length(xv) < 2)
    return(list(ok = FALSE, msg = "Trop peu de valeurs numériques valides."))
  rng <- range(xv)

  if (method == "width") {
    if (!is.finite(n_classes) || n_classes < 2)
      return(list(ok = FALSE, msg = "Il faut au moins 2 classes."))
    if (rng[1] == rng[2])
      return(list(ok = FALSE, msg = "Variable constante : impossible de créer des classes."))
    breaks <- seq(rng[1], rng[2], length.out = n_classes + 1)
  } else if (method == "quantile") {
    if (!is.finite(n_classes) || n_classes < 2)
      return(list(ok = FALSE, msg = "Il faut au moins 2 classes."))
    breaks <- unique(stats::quantile(xv, probs = seq(0, 1, length.out = n_classes + 1),
                                     names = FALSE, type = 7))
    if (length(breaks) < 3)
      return(list(ok = FALSE, msg = paste0(
        "Quantiles non distincts (valeurs trop concentrées) : réduisez le nombre ",
        "de classes ou utilisez la méthode « largeur égale ».")))
  } else {
    breaks <- suppressWarnings(sort(unique(as.numeric(breaks_manual))))
    breaks <- breaks[is.finite(breaks)]
    if (length(breaks) < 2)
      return(list(ok = FALSE, msg = "Fournissez au moins deux bornes numériques distinctes."))
  }

  nB <- length(breaks)            # nombre de bornes ; classes = nB - 1
  fmt <- function(v) formatC(signif(v, max(dig, 3)), format = "g", big.mark = " ")
  lo <- breaks[-nB]; hi <- breaks[-1]

  # Etiquettes automatiques selon la convention choisie ("[a ; b[" etc.)
  auto_labels <- switch(interval_style,
    "std_last_closed" = {
      lab <- sprintf("[%s ; %s[", fmt(lo), fmt(hi))
      lab[length(lab)] <- sprintf("[%s ; %s]", fmt(lo[length(lo)]), fmt(hi[length(hi)]))
      lab
    },
    "all_left_closed" = sprintf("[%s ; %s[", fmt(lo), fmt(hi)),
    "mixed_open" = {
      lab <- sprintf("]%s ; %s[", fmt(lo), fmt(hi))       # milieu : ouvert des 2 cotes
      lab[1] <- sprintf("[%s ; %s[", fmt(lo[1]), fmt(hi[1]))               # 1re : fermee gauche
      lab[length(lab)] <- sprintf("]%s ; %s]", fmt(lo[length(lo)]), fmt(hi[length(hi)]))
      lab
    },
    "all_right_closed" = {
      lab <- sprintf("]%s ; %s]", fmt(lo), fmt(hi))       # toutes fermees a droite
      lab[1] <- sprintf("[%s ; %s]", fmt(lo[1]), fmt(hi[1]))
      lab
    },
    "all_closed" = sprintf("[%s ; %s]", fmt(lo), fmt(hi))) # toutes fermees des 2 cotes

  labels <- auto_labels
  if (!is.null(labels_custom) && length(labels_custom) > 0) {
    labels_custom <- trimws(as.character(labels_custom))
    labels_custom <- labels_custom[nzchar(labels_custom)]
    if (length(labels_custom) != nB - 1)
      return(list(ok = FALSE, msg = trf(
        "Nombre d'étiquettes (%d) différent du nombre de classes (%d).",
        length(labels_custom), nB - 1)))
    labels <- labels_custom
  }

  # Binning coherent avec la convention.
  cut_breaks <- breaks
  # all_right_closed : bornes fermees a droite (right = TRUE), 1re classe fermee
  # a gauche via include.lowest.
  use_right <- (interval_style == "all_right_closed")
  # include.lowest ferme la borne extreme du cote "ouvert" : - std_last_closed
  # / all_closed.
  incl_low <- interval_style %in% c("std_last_closed", "all_closed", "all_right_closed")
  if (interval_style %in% c("all_left_closed", "mixed_open")) {
    span <- diff(range(breaks)); eps <- if (span > 0) span * 1e-9 else 1e-9
    cut_breaks[nB] <- breaks[nB] + eps
  }
  f <- cut(xnum, breaks = cut_breaks, labels = labels, right = use_right,
           include.lowest = incl_low, ordered_result = TRUE)
  n_na_created <- sum(is.na(f) & !is.na(xnum))
  counts <- as.data.frame(table(Classe = f, useNA = "no"))
  names(counts) <- c("Classe", "Effectif")
  counts$Pourcentage <- round(100 * counts$Effectif / max(sum(counts$Effectif), 1), 1)

  list(ok = TRUE, factor = f, breaks = breaks, counts = counts,
       n_na_created = n_na_created,
       msg = if (n_na_created > 0) trf(
         "%d valeur(s) hors bornes -> NA (élargissez les bornes si nécessaire).",
         n_na_created) else NULL)
}

# Echappement d'un identifiant SQL (nom de colonne issu du fichier utilisateur)
# pour DuckDB.
hstat_sql_ident <- function(x) sprintf('"%s"', gsub('"', '""', x))

# UN NOM DE TELECHARGEMENT PART DANS UN EN-TETE HTTP, PAS SUR UN DISQUE.
hstat_nom_fichier <- function(x, defaut = "fichier", max = 80L) {
  s <- as.character(x)[1]
  if (is.na(s)) s <- ""
  s <- gsub("[[:cntrl:]]", "", s)                 # retours a la ligne, tabulations
  s <- gsub('[/\\\\:*?"<>|]', "_", s)               # separateurs et caracteres refuses
  s <- gsub("[.][.]+", ".", s)                    # « .. » ne remonte nulle part
  s <- trimws(s)
  s <- substr(s, 1L, max)
  if (!nzchar(s) || s %in% c(".", "..")) defaut else s
}

# VERSION DU PAQUET -- source unique de verite
# Le numero de version ne vit qu'a UN seul endroit : le champ Version: de
# DESCRIPTION.

# Localise DESCRIPTION, que l'on tourne depuis les sources ou depuis
# l'installe.
.hstat_description_path <- function() {
  cands <- c(
    tryCatch(system.file("DESCRIPTION", package = "HStat"), error = function(e) ""),
    "DESCRIPTION",
    file.path("..", "..", "DESCRIPTION"),
    file.path("..", "DESCRIPTION"),
    file.path("..", "..", "..", "DESCRIPTION")
  )
  cands <- cands[nzchar(cands) & file.exists(cands)]
  if (length(cands)) cands[1] else NA_character_
}

# Lit un champ de DESCRIPTION ; NA_character_ si absent ou illisible.
.hstat_description_field <- function(field) {
  p <- .hstat_description_path()
  if (is.na(p)) return(NA_character_)
  v <- tryCatch(read.dcf(p, fields = field)[1, 1],
                error = function(e) NA_character_)
  if (is.null(v) || is.na(v) || !nzchar(v)) NA_character_ else as.character(v)
}

# Version courante du paquet : paquet installe, puis DESCRIPTION, puis repli.
hstat_version <- function(fallback = "0.0.0") {
  v <- suppressWarnings(tryCatch(as.character(utils::packageVersion("HStat")),
                                 error = function(e) NA_character_))
  if (length(v) && !is.na(v) && nzchar(v)) return(v)
  v <- .hstat_description_field("Version")
  if (!is.na(v)) return(v)
  fallback
}

# Fichier statique estampille de la version : « hstat-theme.css?v=0.36.0 ».
hstat_asset <- function(fichier) paste0(fichier, "?v=", hstat_version())

# Annee a citer : date de construction du paquet, puis champ Date: de
# DESCRIPTION, puis annee courante.
hstat_pkg_year <- function() {
  y <- suppressWarnings(tryCatch(
    sub("-.*", "", as.character(utils::packageDate("HStat"))),
    error = function(e) NA_character_))
  if (length(y) && !is.null(y) && !is.na(y) && nzchar(y)) return(y)
  d <- .hstat_description_field("Date")
  if (!is.na(d)) return(sub("-.*", "", d))
  format(Sys.Date(), "%Y")
}

# Les auteurs a citer : DESCRIPTION est la source unique
hstat_pkg_auteurs <- function() {
  champ <- suppressWarnings(tryCatch(
    utils::packageDescription("HStat", fields = "Authors@R"),
    error = function(e) NA_character_))
  if (is.null(champ) || length(champ) != 1L || is.na(champ) || !nzchar(champ))
    champ <- .hstat_description_field("Authors@R")
  if (is.na(champ)) return(NULL)
  # `Authors@R` est du CODE R, et c'est ainsi que R lui-meme le lit
  # (`citation()` fait de meme).
  ppl <- tryCatch(eval(parse(text = champ),
                       envir = new.env(parent = asNamespace("utils"))),
                  error = function(e) NULL)
  if (!inherits(ppl, "person") || !length(ppl)) return(NULL)
  # Seuls les AUTEURS sont cites.
  roles <- vapply(ppl, function(p) paste(p$role, collapse = " "), character(1))
  garde <- grepl("\\baut\\b|\\bcre\\b", roles)
  if (!any(garde)) return(ppl)
  ppl[garde]
}

# Les quatre formes de nom employees par les six styles de citation.
HSTAT_CIT_FORMES <- c("nom_prenom", "prenom_nom", "nom_initiales",
                      "nom_initiales_nu")

# Rendu d'UN auteur.
.hstat_auteur_nom <- function(p, forme = HSTAT_CIT_FORMES) {
  forme <- match.arg(forme, HSTAT_CIT_FORMES)
  fam <- paste(p$family, collapse = " ")
  giv <- paste(p$given,  collapse = " ")
  if (!nzchar(fam)) return(giv)
  if (!nzchar(giv)) return(fam)
  mots <- strsplit(giv, "[[:space:]]+")[[1]]
  ini  <- paste(vapply(mots, function(m)
    paste0(substr(strsplit(m, "-")[[1]], 1, 1), ".", collapse = "-"),
    character(1), USE.NAMES = FALSE), collapse = " ")
  switch(forme,
    nom_prenom       = paste0(fam, ", ", giv),
    prenom_nom       = paste0(giv, " ", fam),
    nom_initiales    = paste0(fam, ", ", ini),
    # Vancouver colle les initiales, sans point ni trait d'union : « KOUADIO H ».
    nom_initiales_nu = paste0(fam, " ", gsub("[^[:alnum:]]", "", ini)))
}

# BibTeX REORDONNE les noms qu'il croit composes : « Claude Code » ressortirait
# « Code, C.
.hstat_auteur_bibtex <- function(p) {
  fam <- paste(p$family, collapse = " ")
  giv <- paste(p$given,  collapse = " ")
  if (!nzchar(fam)) return(paste0("{", giv, "}"))
  if (!nzchar(giv)) return(paste0("{", fam, "}"))
  paste0(giv, " ", fam)
}

# Assemblage de la liste.
.hstat_auteurs_liste <- function(ppl, forme, sep = ", ", dernier = sep) {
  if (is.null(ppl) || !length(ppl)) return("")
  noms <- vapply(ppl, .hstat_auteur_nom, character(1), forme = forme)
  n <- length(noms)
  if (n == 1L) return(noms)
  paste0(paste(noms[-n], collapse = sep), dernier, noms[n])
}

# APA termine la liste d'auteurs par un point.
.hstat_point <- function(x) if (grepl("[.]$", x)) x else paste0(x, ".")

# Genere la citation du package HStat dans differents styles.
hstat_citation <- function(style = c("text", "bibtex", "ris", "apa", "vancouver", "markdown")) {
  style <- match.arg(style)
  vers <- hstat_version()
  year <- hstat_pkg_year()
  ppl  <- hstat_pkg_auteurs()
  title <- "HStat : Application Shiny interactive pour l'analyse statistique"
  url   <- "https://github.com/houphouet/hstat"

  # DESCRIPTION illisible : la citation garde sa FORME et le manque se voit,
  # comme le « 0.0.0 » volontairement invalide de `hstat_version()`.
  if (is.null(ppl) || !length(ppl))
    ppl <- utils::person(family = tr("Auteurs indisponibles"))

  switch(style,
    "text" = sprintf(
      "%s (%s). %s. Version %s. %s",
      .hstat_auteurs_liste(ppl, "nom_prenom", ", ", " & "),
      year, title, vers, url),

    "apa" = sprintf(
      "%s (%s). %s (Version %s) [Logiciel R]. %s",
      .hstat_point(.hstat_auteurs_liste(ppl, "nom_initiales", ", ", ", & ")),
      year, title, vers, url),

    "vancouver" = sprintf(
      "%s. %s [Logiciel R]. Version %s. %s; %s.",
      .hstat_auteurs_liste(ppl, "nom_initiales_nu", ", ", ", "),
      title, vers, year, url),

    "markdown" = sprintf(
      "%s (%s). *%s*. Version %s. [%s](%s)",
      .hstat_auteurs_liste(ppl, "nom_prenom", ", ", " & "),
      year, title, vers, url, url),

    "bibtex" = paste(
      "@Manual{hstat,",
      sprintf("  title  = {%s},", title),
      sprintf("  author = {%s},", paste(
        vapply(ppl, .hstat_auteur_bibtex, character(1)), collapse = " and ")),
      sprintf("  year   = {%s},", year),
      sprintf("  note   = {Version %s},", vers),
      sprintf("  url    = {%s},", url),
      "}", sep = "\n"),

    # Une ligne AU par auteur : c'est la forme que les gestionnaires de
    # references attendent, une seule ligne les rendrait comme un auteur unique
    # au nom a rallonge.
    "ris" = paste(c(
      "TY  - COMP",
      sprintf("AU  - %s",
              vapply(ppl, .hstat_auteur_nom, character(1), forme = "nom_prenom")),
      sprintf("PY  - %s", year),
      sprintf("TI  - %s", title),
      sprintf("ET  - Version %s", vers),
      sprintf("UR  - %s", url),
      "ER  - "), collapse = "\n")
  )
}

# -- Capuchons de moustache pour l'EXPORT des boxplots.
hstat_add_whisker_caps <- function(p, cap_width = 0.3) {
  if (!inherits(p, "ggplot") || length(p$layers) == 0) return(p)
  is_box <- vapply(p$layers, function(l) inherits(l$geom, "GeomBoxplot"), logical(1))
  has_caps <- any(vapply(p$layers, function(l)
    inherits(l$geom, "GeomErrorbar") && inherits(l$stat, "StatBoxplot"),
    logical(1)))
  if (!any(is_box) || has_caps) return(p)
  new_layers <- list()
  for (i in seq_along(p$layers)) {
    l <- p$layers[[i]]
    if (is_box[i]) {
      m <- l$mapping
      cap_pos <- l$position
      if (!is.null(m) && !is.null(m$fill)) {
        # Vraies boites multiples par x (fill au niveau de la COUCHE).
        if (is.null(m$group)) {
          xq <- if (!is.null(m$x)) m$x else p$mapping$x
          m$group <- if (!is.null(xq)) {
            rlang::inject(ggplot2::aes(group = interaction(!!xq, !!m$fill)))$group
          } else m$fill
        }
        m$fill <- NULL
        cap_pos <- ggplot2::position_dodge(width = 0.75)
        l$position <- ggplot2::position_dodge(width = 0.75)
      }
      # fill au niveau du plot ou pas de fill : une boite par x, centree ->
      # capuchon etroit centre, position de la couche inchangee.
      lay_data <- if (inherits(l$data, "waiver")) NULL else l$data
      cap <- tryCatch(
        ggplot2::layer(stat = "boxplot", geom = "errorbar",
                       data = lay_data, mapping = m,
                       position = cap_pos,
                       show.legend = FALSE,
                       inherit.aes = isTRUE(l$inherit.aes) || is.null(l$inherit.aes),
                       params = list(width = cap_width, na.rm = TRUE)),
        error = function(e) NULL)
      if (!is.null(cap)) new_layers <- c(new_layers, list(cap))
    }
    new_layers <- c(new_layers, list(l))
  }
  p$layers <- new_layers
  p
}

# -- Disponibilite des moteurs ------------------------------------------------
hstat_has_duckdb <- function()
  requireNamespace("duckdb", quietly = TRUE) && requireNamespace("DBI", quietly = TRUE)
hstat_has_datatable <- function()
  requireNamespace("data.table", quietly = TRUE)

# -- Lecture rapide d'un CSV en memoire (fread, repli read.csv) ---------------
# Detecte si le fichier est de l'UTF-8 valide.
hstat_detect_encoding <- function(path) {
  enc <- tryCatch({
    bytes <- readBin(path, "raw", n = 262144L)   # 256 Ko suffisent
    if (length(bytes) == 0) return("UTF-8")
    s <- rawToChar(bytes)
    Encoding(s) <- "bytes"
    test <- suppressWarnings(iconv(rawToChar(bytes), "UTF-8", "UTF-8"))
    if (is.na(test)) "Latin-1" else "UTF-8"
  }, error = function(e) "Latin-1")
  enc
}

hstat_read_csv_mem <- function(path, header = TRUE, sep = ",") {
  enc <- hstat_detect_encoding(path)
  df <- if (hstat_has_datatable()) {
    # On declare l'encodage detecte a fread : il convertit nativement (rapide)
    # et ne bloque pas.
    fenc <- if (identical(enc, "Latin-1")) "Latin-1" else "UTF-8"
    as.data.frame(data.table::fread(path, header = header, sep = sep,
                            data.table = FALSE, check.names = FALSE,
                            showProgress = FALSE, encoding = fenc))
  } else {
    fe <- if (identical(enc, "Latin-1")) "latin1" else "UTF-8"
    utils::read.csv(path, header = header, sep = sep, check.names = FALSE,
             stringsAsFactors = FALSE, fileEncoding = fe)
  }
  # Filet de securite : garantit des chaines UTF-8 valides en sortie.
  hstat_df_to_utf8(df)
}

# -- Assainissement d'encodage : garantit des chaines UTF-8 valides.
hstat_to_utf8 <- function(x) {
  if (is.character(x)) {
    bad <- is.na(iconv(x, "UTF-8", "UTF-8"))
    if (any(bad %in% TRUE)) {
      conv <- iconv(x[bad %in% TRUE], "latin1", "UTF-8")
      x[bad %in% TRUE] <- conv
    }
    still_bad <- is.na(iconv(x, "UTF-8", "UTF-8"))
    if (any(still_bad %in% TRUE)) {
      x[still_bad %in% TRUE] <- iconv(x[still_bad %in% TRUE], "UTF-8", "UTF-8", sub = "")
    }
    Encoding(x) <- "UTF-8"
    return(x)
  }
  if (is.factor(x)) { levels(x) <- hstat_to_utf8(levels(x)); return(x) }
  x
}

# Assainit un data.frame entier : noms de colonnes + colonnes texte/facteur.
hstat_df_to_utf8 <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  nm <- hstat_to_utf8(names(df))
  nm[is.na(nm) | !nzchar(nm)] <- "V"
  names(df) <- make.unique(nm, sep = "_")
  for (j in seq_along(df)) {
    if (is.character(df[[j]]) || is.factor(df[[j]])) df[[j]] <- hstat_to_utf8(df[[j]])
  }
  df
}

# -- Lecture generique en memoire pour la FUSION de fichiers.
hstat_read_any_mem <- function(path, sep = ",", name = NULL) {
  ext <- tolower(tools::file_ext(path))
  df <- tryCatch({
    if (ext %in% c("csv", "txt", "tsv")) {
      s <- if (ext == "tsv") "\t" else sep
      hstat_read_csv_mem(path, header = TRUE, sep = s)
    } else if (ext %in% c("xlsx", "xls")) {
      hstat_lire_excel(path, sheet = 1)
    } else if (ext == "rds") {
      as.data.frame(hstat_lire_rds(path))
    } else if (ext == "sav") {
      as.data.frame(haven::read_sav(path))
    } else if (ext == "dta") {
      as.data.frame(haven::read_dta(path))
    } else {
      hstat_read_csv_mem(path, header = TRUE, sep = sep)
    }
  }, error = function(e) NULL)
  # Assainit l'encodage pour eviter les "invalid multibyte string" lors des
  # traitements ulterieurs (nchar, strsplit, affichage, fusion...).
  hstat_df_to_utf8(df)
}

# -- Moteur de fusion de plusieurs data.frames --------------------------------
# type : "inner"/"left"/"right"/"full" (jointures), "rows" (UNION), "cols" (cbind).

# Noms des feuilles d'un classeur.
hstat_excel_sheets <- function(path) {
  if (is.null(path) || !length(path) || !nzchar(path[1]) || !file.exists(path[1]))
    return(character(0))
  if (!(tolower(tools::file_ext(path[1])) %in% c("xlsx", "xls")))
    return(character(0))
  s <- tryCatch(readxl::excel_sheets(path[1]), error = function(e) character(0))
  s <- as.character(s)
  s[!is.na(s) & nzchar(s)]
}

# Lit les feuilles demandees.
hstat_excel_read_sheets <- function(path, sheets = NULL) {
  dispo <- hstat_excel_sheets(path)
  if (!length(dispo))
    return(list(frames = list(), names = character(0), ignorees = character(0),
                msg = "Ce fichier n'est pas un classeur Excel lisible."))
  choisies <- if (is.null(sheets) || !length(sheets)) dispo
              else intersect(as.character(sheets), dispo)
  if (!length(choisies))
    return(list(frames = list(), names = character(0), ignorees = character(0),
                msg = "Aucune des feuilles demandées n'existe dans ce classeur."))
  # Le controle de decompression porte sur l'ARCHIVE, pas sur une feuille : il
  # se fait une fois, avant la boucle.
  motif <- hstat_excel_controle(path)
  if (!is.null(motif))
    return(list(frames = list(), names = character(0), ignorees = character(0),
                msg = motif))
  frames <- list(); noms <- character(0); ignorees <- character(0)
  for (s in choisies) {
    d <- tryCatch(hstat_lire_excel(path, sheet = s),
                  error = function(e) NULL)
    if (is.null(d) || !nrow(d) || !ncol(d)) { ignorees <- c(ignorees, s); next }
    frames[[length(frames) + 1L]] <- d
    noms <- c(noms, s)
  }
  msg <- if (!length(frames)) "Aucune feuille exploitable dans ce classeur."
         else sprintf("%d feuille(s) lue(s)%s.", length(frames),
                      if (length(ignorees))
                        trf(", %d écartée(s) car vide(s) ou illisible(s) : %s",
                                length(ignorees), paste(ignorees, collapse = ", "))
                      else "")
  list(frames = frames, names = noms, ignorees = ignorees, msg = msg)
}

# Compare la structure des feuilles.
hstat_excel_compat <- function(frames, names_ = NULL) {
  if (!length(frames))
    return(list(identiques = FALSE, communes = character(0),
                suggestion = "rows", msg = "Aucune feuille à comparer."))
  cols <- lapply(frames, names)
  communes <- Reduce(intersect, cols)
  toutes <- unique(unlist(cols))
  identiques <- length(communes) == length(toutes) &&
                all(vapply(cols, function(c0) length(c0) == length(toutes), logical(1)))
  msg <- if (identiques)
    sprintf(paste("Les %d feuilles portent exactement les mêmes %d colonnes :",
                  "l'empilement les met bout à bout, une ligne par observation."),
            length(frames), length(toutes))
  else if (length(communes))
    sprintf(paste("Les feuilles ont %d colonne(s) en commun (%s) et %d colonne(s)",
                  "propres. Une jointure par clé rapproche les lignes qui se",
                  "correspondent ; l'empilement les mettrait bout à bout en",
                  "laissant des vides."),
            length(communes), paste(utils::head(communes, 6), collapse = ", "),
            length(setdiff(toutes, communes)))
  else
    paste("Les feuilles n'ont AUCUNE colonne en commun : ni jointure ni",
          "empilement n'a de sens en l'état. Vérifiez que la première ligne de",
          "chaque feuille porte bien les en-têtes.")
  list(identiques = identiques, communes = communes,
       suggestion = if (identiques) "rows" else if (length(communes)) "inner" else "rows",
       msg = msg)
}

hstat_merge_frames <- function(frames, type = "inner",
                               key_left = NULL, key_right = NULL,
                               add_source = TRUE, source_names = NULL,
                               source_col = ".source", source_mode = "name") {
  out <- tryCatch(
    .hstat_merge_frames_impl(frames, type, key_left, key_right, add_source,
                             source_names, source_col, source_mode),
    error = function(e) list(ok = FALSE, data = NULL,
      msg = hstat_err_fr(e, "Fusion")))
  out
}

.hstat_merge_frames_impl <- function(frames, type = "inner",
                               key_left = NULL, key_right = NULL,
                               add_source = TRUE, source_names = NULL,
                               source_col = ".source", source_mode = "name") {
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) < 2)
    return(list(ok = FALSE, data = NULL, msg = "Au moins deux fichiers valides sont requis."))
  if (is.null(source_names) || length(source_names) != length(frames))
    source_names <- paste0("fichier", seq_along(frames))

  # Normalise les cles : accepte un vecteur (cle composite) ou une chaine unique.
  norm_keys <- function(k) {
    if (is.null(k)) return(character(0))
    k <- unlist(strsplit(as.character(k), "[,;]"))
    k <- trimws(k); k[nzchar(k)]
  }
  kl <- norm_keys(key_left)
  kr <- norm_keys(key_right)
  if (length(kr) == 0) kr <- kl
  if (length(kl) > 0 && length(kr) > 0 && length(kl) != length(kr))
    return(list(ok = FALSE, data = NULL,
                msg = "Les clés gauche et droite doivent avoir le même nombre de colonnes."))

  # Empilement de lignes (UNION)
  if (type %in% c("rows", "union_distinct")) {
    # Valeur d'origine inscrite dans la colonne source.
    scol <- if (is.null(source_col) || !nzchar(trimws(source_col))) ".source" else trimws(source_col)
    src_values <- source_names
    if (identical(source_mode, "number")) {
      src_values <- vapply(source_names, function(nm) {
        m <- regmatches(nm, regexpr("[0-9]+", nm))
        if (length(m) && nzchar(m)) suppressWarnings(as.character(as.numeric(m))) else nm
      }, character(1))
    }
    all_cols <- unique(unlist(lapply(frames, names)))
    norm <- lapply(seq_along(frames), function(i) {
      d <- frames[[i]]
      for (cc in setdiff(all_cols, names(d))) d[[cc]] <- NA
      d <- d[, all_cols, drop = FALSE]
      if (isTRUE(add_source) && type == "rows") d[[scol]] <- src_values[i]
      d
    })
    # rbind robuste : si des types de colonnes diffèrent entre fichiers (p. ex.
    # numérique vs texte sous le même nom), on harmonise en caractère pour éviter
    # une erreur qui ferait planter l'empilement.
    out <- tryCatch(do.call(rbind, norm), error = function(e) {
      norm2 <- lapply(norm, function(d) { d[] <- lapply(d, as.character); d })
      do.call(rbind, norm2)
    })
    # Si la colonne source est numérique (mode "number"), la convertir en numérique.
    if (isTRUE(add_source) && type == "rows" && identical(source_mode, "number") &&
        scol %in% names(out)) {
      num_try <- suppressWarnings(as.numeric(out[[scol]]))
      if (!any(is.na(num_try) & !is.na(out[[scol]]))) out[[scol]] <- num_try
    }
    if (type == "union_distinct") {
      out <- out[!duplicated(out), , drop = FALSE]
      return(list(ok = TRUE, data = out,
        msg = trf("Union distincte de %d fichiers : %d lignes uniques, %d colonnes.",
                      length(frames), nrow(out), ncol(out))))
    }
    return(list(ok = TRUE, data = out,
                msg = trf("Empilement de %d fichiers : %d lignes, %d colonnes.",
                              length(frames), nrow(out), ncol(out))))
  }

  # Juxtaposition de colonnes
  if (type == "cols") {
    n <- max(vapply(frames, nrow, integer(1)))
    norm <- lapply(seq_along(frames), function(i) {
      d <- frames[[i]]
      if (nrow(d) < n) {
        pad <- d[rep(NA_integer_, n - nrow(d)), , drop = FALSE]
        d <- rbind(d, pad)
      }
      names(d) <- paste0(names(d), "_", source_names[i])
      d
    })
    out <- do.call(cbind, norm)
    names(out) <- make.unique(names(out))
    return(list(ok = TRUE, data = out,
                msg = trf("Juxtaposition de %d fichiers : %d lignes, %d colonnes.",
                              length(frames), nrow(out), ncol(out))))
  }

  # Opérations ENSEMBLISTES sur les lignes (mêmes colonnes)
  if (type %in% c("intersect", "setdiff", "setdiff_right")) {
    a <- frames[[1]]; b <- frames[[2]]
    common <- intersect(names(a), names(b))
    if (length(common) == 0)
      return(list(ok = FALSE, data = NULL, msg = "Aucune colonne commune entre les deux fichiers."))
    a2 <- a[, common, drop = FALSE]; b2 <- b[, common, drop = FALSE]
    ka <- do.call(paste, c(a2, sep = "\r")); kb <- do.call(paste, c(b2, sep = "\r"))
    out <- switch(type,
      "intersect"     = a2[ka %in% kb, , drop = FALSE],
      "setdiff"       = a2[!(ka %in% kb), , drop = FALSE],
      "setdiff_right" = b2[!(kb %in% ka), , drop = FALSE])
    out <- out[!duplicated(out), , drop = FALSE]
    lbl <- switch(type, "intersect" = "Intersection",
                  "setdiff" = "Différence (1er sauf 2e)", "setdiff_right" = "Différence (2e sauf 1er)")
    return(list(ok = TRUE, data = out,
      msg = trf("%s sur colonnes communes : %d lignes, %d colonnes.", lbl, nrow(out), ncol(out))))
  }

  # Jointure CROISÉE (produit cartésien, sans clé)
  if (type == "cross") {
    # Garde-fou mémoire : le produit cartésien peut exploser.
    total_rows <- prod(vapply(frames, nrow, numeric(1)))
    if (!is.finite(total_rows) || total_rows > 5e6)
      return(list(ok = FALSE, data = NULL,
        msg = trf("Jointure croisée refusée : le produit cartésien générerait ~%.0f lignes (> 5 000 000). Réduisez le nombre ou la taille des fichiers.", total_rows)))
    acc <- frames[[1]]
    for (i in 2:length(frames)) {
      d2 <- frames[[i]]
      names(d2) <- ifelse(names(d2) %in% names(acc),
                          paste0(names(d2), "_", source_names[i]), names(d2))
      acc <- merge(acc, d2, by = character(0), all = TRUE)
    }
    return(list(ok = TRUE, data = acc,
      msg = trf("Jointure croisée (produit cartésien) de %d fichiers : %d lignes, %d colonnes.",
                    length(frames), nrow(acc), ncol(acc))))
  }

  # Au-delà : toutes les options nécessitent une clé.
  if (length(kl) == 0)
    return(list(ok = FALSE, data = NULL, msg = "Choisissez au moins une colonne clé du 1er fichier."))
  acc <- frames[[1]]
  if (!all(kl %in% names(acc)))
    return(list(ok = FALSE, data = NULL,
                msg = trf("Clé(s) « %s » absente(s) du 1er fichier.", paste(setdiff(kl, names(acc)), collapse = ", "))))

  # SEMI / ANTI jointures (filtrent le 1er ou le 2e, sans ajouter de colonnes)
  if (type %in% c("semi", "anti", "anti_right")) {
    b <- frames[[2]]
    kr2 <- if (all(kr %in% names(b))) kr else kl
    if (!all(kr2 %in% names(b)))
      return(list(ok = FALSE, data = NULL,
                  msg = trf("Clé(s) « %s » absente(s) du 2e fichier.", paste(setdiff(kr2, names(b)), collapse = ", "))))
    ka <- do.call(paste, c(acc[, kl, drop = FALSE], sep = "\r"))
    kb <- do.call(paste, c(b[, kr2, drop = FALSE], sep = "\r"))
    out <- switch(type,
      "semi"       = acc[ka %in% kb, , drop = FALSE],
      "anti"       = acc[!(ka %in% kb), , drop = FALSE],
      "anti_right" = b[!(kb %in% ka), , drop = FALSE])
    lbl <- switch(type, "semi" = "Semi-jointure (1er avec correspondance)",
                  "anti" = "Anti-jointure (1er sans correspondance)",
                  "anti_right" = "Anti-jointure (2e sans correspondance)")
    return(list(ok = TRUE, data = out,
      msg = trf("%s sur « %s » : %d lignes, %d colonnes.",
                    lbl, paste(kl, collapse = "+"), nrow(out), ncol(out))))
  }

  # MISE À JOUR / COALESCE : complète/remplace les valeurs du 1er par le 2e
  if (type %in% c("update", "patch")) {
    b <- frames[[2]]
    kr2 <- if (all(kr %in% names(b))) kr else kl
    if (!all(kr2 %in% names(b)))
      return(list(ok = FALSE, data = NULL,
                  msg = trf("Clé(s) « %s » absente(s) du 2e fichier.", paste(setdiff(kr2, names(b)), collapse = ", "))))
    ka <- do.call(paste, c(acc[, kl, drop = FALSE], sep = "\r"))
    kb <- do.call(paste, c(b[, kr2, drop = FALSE], sep = "\r"))
    idx <- match(ka, kb)               # ligne du 2e correspondant a chaque ligne du 1er
    shared <- setdiff(intersect(names(acc), names(b)), kl)
    n_upd <- 0L
    for (cc in shared) {
      newv <- b[[cc]][idx]
      if (type == "patch") {           # patch : ne remplit que les NA du 1er
        repl <- is.na(acc[[cc]]) & !is.na(newv)
      } else {                          # update : remplace si une valeur existe dans le 2e
        repl <- !is.na(newv)
      }
      acc[[cc]][repl] <- newv[repl]; n_upd <- n_upd + sum(repl, na.rm = TRUE)
    }
    lbl <- if (type == "patch") "Complétion des valeurs manquantes" else "Mise à jour des valeurs"
    return(list(ok = TRUE, data = acc,
      msg = trf("%s : %d valeur(s) modifiée(s) sur %d colonne(s) partagée(s).",
                    lbl, n_upd, length(shared))))
  }

  # Jointures classiques (inner/left/right/full), clés composites, enchaînées
  if (!type %in% c("inner", "left", "right", "full"))
    return(list(ok = FALSE, data = NULL, msg = trf("Type de fusion inconnu : %s", type)))
  for (i in 2:length(frames)) {
    d2 <- frames[[i]]
    kr_i <- if (length(kr) == length(kl) && all(kr %in% names(d2))) kr else kl
    if (!all(kr_i %in% names(d2)))
      return(list(ok = FALSE, data = NULL,
                  msg = trf("Clé(s) « %s » absente(s) du fichier %d.", paste(setdiff(kr_i, names(d2)), collapse = ", "), i)))
    acc <- merge(acc, d2, by.x = kl, by.y = kr_i,
                 all.x = type %in% c("left", "full"),
                 all.y = type %in% c("right", "full"),
                 suffixes = c("", paste0("_", source_names[i])))
  }
  list(ok = TRUE, data = acc,
       msg = trf("Jointure %s de %d fichiers sur « %s » : %d lignes, %d colonnes.",
                     type, length(frames), paste(kl, collapse = "+"), nrow(acc), ncol(acc)))
}

# -- Ouverture d'une connexion DuckDB en memoire ------------------------------
hstat_duckdb_connect <- function() {
  if (!hstat_has_duckdb()) stop("Le package 'duckdb' est requis pour le mode hors-mémoire.")
  con <- DBI::dbConnect(duckdb::duckdb())
  # Reglages "tres gros volumes" : - temp_directory.
  tryCatch({
    tmp <- file.path(tempdir(), "hstat_duckdb_spill")
    dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
    DBI::dbExecute(con, sprintf("PRAGMA temp_directory='%s'",
                                gsub("'", "''", normalizePath(tmp, winslash = "/"))))
    DBI::dbExecute(con, "SET preserve_insertion_order=false")
    mem <- Sys.getenv("HSTAT_DUCKDB_MEMORY", "")
    if (nzchar(mem) && grepl("^[0-9]+(\\.[0-9]+)?\\s*(KB|MB|GB|TB|KiB|MiB|GiB|TiB)$",
                             mem, ignore.case = TRUE))
      DBI::dbExecute(con, sprintf("SET memory_limit='%s'", mem))
  }, error = function(e) NULL)  # reglages facultatifs : ne jamais bloquer la connexion
  con
}

# -- Enregistrement d'une source dans DuckDB sous forme de VUE ----------------
# Aucune donnee n'est materialisee.
hstat_duckdb_register <- function(con, path, kind, header = TRUE, sep = ",") {
  p   <- hstat_sql_path(path)
  tbl <- "hstat_source"
  DBI::dbExecute(con, sprintf("DROP VIEW IF EXISTS %s", hstat_sql_ident(tbl)))
  if (kind == "csv") {
    delim <- if (identical(sep, "\t")) "\\t" else sep
    sql <- sprintf(
      "CREATE VIEW %s AS SELECT * FROM read_csv_auto('%s', header=%s, delim='%s', sample_size=-1)",
      hstat_sql_ident(tbl), p, if (isTRUE(header)) "true" else "false",
      gsub("'", "''", delim))  # delim echappe (securite)
  } else if (kind == "parquet") {
    sql <- sprintf("CREATE VIEW %s AS SELECT * FROM read_parquet('%s')", hstat_sql_ident(tbl), p)
  } else {
    stop("Type non pris en charge par DuckDB : ", kind)
  }
  DBI::dbExecute(con, sql)
  tbl
}

# -- Connexion a un fichier DuckDB natif --------------------------------------
HSTAT_DUCKDB_FICHIER_CONFIG <- list(enable_external_access = "false")
hstat_duckdb_open_file <- function(path) {
  if (!hstat_has_duckdb()) stop("Le package 'duckdb' est requis.")
  # Un fichier .duckdb televerse n'est pas qu'un conteneur de tables.
  con <- DBI::dbConnect(duckdb::duckdb(config = HSTAT_DUCKDB_FICHIER_CONFIG),
                        dbdir = path, read_only = TRUE)
  tbls <- DBI::dbListTables(con)
  if (length(tbls) == 0) { DBI::dbDisconnect(con, shutdown = TRUE); stop("Aucune table dans ce fichier DuckDB.") }
  list(con = con, table = tbls[1], tables = tbls)
}

# -- Metadonnees d'une table/vue DuckDB ---------------------------------------
hstat_duckdb_nrow <- function(con, tbl) {
  as.numeric(DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", hstat_sql_ident(tbl)))$n[1])
}
hstat_duckdb_colnames <- function(con, tbl) {
  DBI::dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 0", hstat_sql_ident(tbl))) |> names()
}
hstat_duckdb_na_total <- function(con, tbl) {
  cols <- hstat_duckdb_colnames(con, tbl)
  if (length(cols) == 0) return(0)
  expr <- paste(sprintf('SUM(CASE WHEN %s IS NULL THEN 1 ELSE 0 END)',
                        hstat_sql_ident(cols)),
                collapse = " + ")
  as.numeric(DBI::dbGetQuery(con, sprintf("SELECT (%s) AS na FROM %s", expr, hstat_sql_ident(tbl)))$na[1])
}

# -- Echantillon representatif (reservoir sampling DuckDB) --------------------
hstat_duckdb_sample <- function(con, tbl, n = HSTAT_SAMPLE_SIZE) {
  total <- hstat_duckdb_nrow(con, tbl)
  if (total <= n) {
    df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", hstat_sql_ident(tbl)))
  } else {
    # %.0f et non %d : n reste < 2^31 mais la table, elle, peut depasser 2 147
    # 483 647 lignes (COUNT(*) revient en numeric).
    df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s USING SAMPLE %.0f ROWS",
                                       hstat_sql_ident(tbl), floor(as.numeric(n))))
  }
  as.data.frame(df)
}

# -- Materialisation complete d'une source DuckDB (petits fichiers) -----------
hstat_duckdb_collect <- function(con, tbl) {
  as.data.frame(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", hstat_sql_ident(tbl))))
}

# -- Fermeture propre d'une connexion DuckDB ----------------------------------
hstat_duckdb_close <- function(con) {
  if (!is.null(con)) tryCatch(DBI::dbDisconnect(con, shutdown = TRUE),
                              error = function(e) NULL)
}

# -- Chargeur unifie ----------------------------------------------------------
# Retourne une liste.
hstat_load_data <- function(path, kind, header = TRUE, sep = ",",
                            sheet = 1,
                            threshold = HSTAT_BIGDATA_THRESHOLD,
                            sample_size = HSTAT_SAMPLE_SIZE) {
  size <- hstat_file_size(path)

  # Formats toujours charges en memoire
  if (kind == "excel") {
    df <- hstat_lire_excel(path, sheet = sheet %||% 1)
  } else if (kind == "sav") {
    df <- as.data.frame(haven::read_sav(path))
  } else if (kind == "dta") {
    df <- as.data.frame(haven::read_dta(path))
  } else if (kind == "rds") {
    df <- as.data.frame(hstat_lire_rds(path))

  # CSV : memoire si petit, DuckDB si volumineux
  } else if (kind == "csv") {
    if (size <= threshold || !hstat_has_duckdb()) {
      df <- hstat_read_csv_mem(path, header = header, sep = sep)
    } else {
      con <- hstat_duckdb_connect()
      tbl <- hstat_duckdb_register(con, path, "csv", header = header, sep = sep)
      total <- hstat_duckdb_nrow(con, tbl)
      smp   <- hstat_duckdb_sample(con, tbl, sample_size)
      return(list(data = smp, mode = "duckdb", con = con, table = tbl,
                  full_nrow = total, full_ncol = ncol(smp),
                  full_na = NA_real_, is_sampled = total > nrow(smp),
                  kind = kind, size = size))
    }

  # Parquet : DuckDB (materialise si petit, vue si volumineux)
  } else if (kind == "parquet") {
    if (!hstat_has_duckdb()) stop("Le package 'duckdb' est requis pour lire le Parquet.")
    con <- hstat_duckdb_connect()
    tbl <- hstat_duckdb_register(con, path, "parquet")
    total <- hstat_duckdb_nrow(con, tbl)
    if (size <= threshold) {
      df <- hstat_duckdb_collect(con, tbl)
      hstat_duckdb_close(con)
    } else {
      smp <- hstat_duckdb_sample(con, tbl, sample_size)
      return(list(data = smp, mode = "duckdb", con = con, table = tbl,
                  full_nrow = total, full_ncol = ncol(smp),
                  full_na = NA_real_, is_sampled = total > nrow(smp),
                  kind = kind, size = size))
    }

  # DuckDB natif
  } else if (kind == "duckdb") {
    op <- hstat_duckdb_open_file(path)
    total <- hstat_duckdb_nrow(op$con, op$table)
    smp   <- hstat_duckdb_sample(op$con, op$table, sample_size)
    return(list(data = smp, mode = "duckdb", con = op$con, table = op$table,
                full_nrow = total, full_ncol = ncol(smp),
                full_na = NA_real_, is_sampled = total > nrow(smp),
                kind = kind, size = size))

  } else {
    stop("Format de fichier non pris en charge.")
  }

  # Sortie mode memoire
  # Assainissement final.
  df <- hstat_df_to_utf8(df)
  list(data = df, mode = "memory", con = NULL, table = NULL,
       full_nrow = nrow(df), full_ncol = ncol(df),
       full_na = sum(is.na(df)), is_sampled = FALSE,
       kind = kind, size = size)
}

# -- Jeux de donnees integres a R ---------------------------------------------
# 1. LE NOM DU JEU N'EST PAS LE NOM DU FICHIER DE DONNEES.
# 2. LA PLUPART DE CES JEUX NE SONT PAS DES `data.frame`.
# 3. CE QUI NE SE CONVERTIT PAS EST NOMME.

# Colonne qui recueille les noms de lignes.
HSTAT_DONNEES_R_COL <- "Nom"

# Le catalogue ne depend pas de la session : les paquets installes sont une
# propriete du PROCESSUS.
.hstat_dr_cat <- new.env(parent = emptyenv())

# -- Catalogue des jeux de donnees disponibles --------------------------------
# Rend un data.frame.
hstat_donnees_r_catalogue <- function(packages = NULL, rafraichir = FALSE) {
  if (is.null(packages)) {
    packages <- tryCatch(rownames(utils::installed.packages()),
                         error = function(e) "datasets")
    if (!length(packages)) packages <- "datasets"
  }
  packages <- unique(as.character(packages))
  cle <- paste0("cat::", paste(sort(packages), collapse = "|"))
  if (!rafraichir && exists(cle, envir = .hstat_dr_cat, inherits = FALSE))
    return(get(cle, envir = .hstat_dr_cat, inherits = FALSE))
  morceaux <- list()
  for (p in packages) {
    r <- tryCatch(suppressWarnings(utils::data(package = p)$results),
                  error = function(e) NULL)
    if (is.null(r) || !NROW(r)) next
    it <- as.character(r[, "Item"])
    # « alias (fichier) » : voir le piege 1 ci-dessus.
    alias <- sub("[[:space:]].*$", "", it)
    fichier <- ifelse(grepl("(", it, fixed = TRUE),
                      sub("^.*\\(([^)]*)\\).*$", "\\1", it), alias)
    morceaux[[length(morceaux) + 1L]] <- data.frame(
      Nom = alias, Paquet = as.character(r[, "Package"]),
      Fichier = fichier, Titre = as.character(r[, "Title"]),
      stringsAsFactors = FALSE)
  }
  out <- if (length(morceaux)) do.call(rbind, morceaux) else
    data.frame(Nom = character(0), Paquet = character(0),
               Fichier = character(0), Titre = character(0),
               stringsAsFactors = FALSE)
  if (nrow(out))
    out <- out[order(out$Paquet != "datasets", tolower(out$Nom), out$Paquet), ,
               drop = FALSE]
  rownames(out) <- NULL
  assign(cle, out, envir = .hstat_dr_cat)
  out
}

# Noms de lignes REELS : `as.character(seq_len(n))` est ce que R rend pour un
# tableau qui n'en porte pas.
.hstat_dr_noms_lignes <- function(x) {
  rn <- rownames(x)
  if (is.null(rn) || !length(rn)) return(NULL)
  if (identical(rn, as.character(seq_len(length(rn))))) return(NULL)
  rn
}

# `make.unique` et non le nom nu.
.hstat_dr_avec_noms <- function(df, noms, col = HSTAT_DONNEES_R_COL) {
  if (is.null(noms) || !length(noms)) return(df)
  cand <- make.unique(c(names(df), col))
  nm <- cand[length(cand)]
  out <- cbind(stats::setNames(
    data.frame(as.character(noms), stringsAsFactors = FALSE), nm), df)
  rownames(out) <- NULL
  out
}

# -- Conversion en tableau d'observations -------------------------------------
# Rend `list(data, classe, note, msg)`.
hstat_donnees_r_convertir <- function(x, nom = "donnees") {
  cl <- paste(class(x), collapse = "/")
  rien <- function(motif) list(data = NULL, classe = cl, note = NULL, msg = motif)
  if (is.null(x)) return(rien(tr("objet vide")))

  # serie temporelle
  # AVANT le test
  # `is.matrix`.
  if (inherits(x, "ts")) {
    tt <- as.numeric(stats::time(x))
    fr <- stats::frequency(x)
    val <- if (is.matrix(x)) as.data.frame(unclass(x)) else
      stats::setNames(data.frame(as.numeric(x)), nom)
    if (is.matrix(x) && is.null(colnames(x)))
      names(val) <- paste0(nom, "_", seq_along(val))
    cal <- data.frame(Temps = tt)
    # L'annee et le rang dans le cycle sont EXACTS, pas devines : ils se
    # deduisent de la frequence declaree par la serie.
    if (fr > 1) {
      cal$Annee <- floor(tt)
      cal$Periode <- as.integer(stats::cycle(x))
    }
    out <- cbind(cal, val)
    rownames(out) <- NULL
    return(list(data = out, classe = cl, msg = NULL,
      note = trf("série temporelle (fréquence %s) dépliée en colonnes de temps et de valeurs", fr)))
  }

  # data.frame
  if (is.data.frame(x)) {
    rn <- .hstat_dr_noms_lignes(x)
    d <- as.data.frame(x)
    rownames(d) <- NULL
    d <- .hstat_dr_avec_noms(d, rn)
    return(list(data = d, classe = cl, msg = NULL,
      note = if (is.null(rn)) NULL else
        trf("noms de lignes déplacés dans la colonne « %s »", HSTAT_DONNEES_R_COL)))
  }

  # table de contingence, ou tableau a plus de deux dimensions
  if (inherits(x, "table") || (is.array(x) && length(dim(x)) > 2L)) {
    d <- as.data.frame(if (inherits(x, "table")) x else as.table(x),
                       stringsAsFactors = FALSE)
    return(list(data = d, classe = cl, msg = NULL,
      note = tr("table de contingence dépliée en format long (une ligne par croisement, effectif en colonne « Freq »)")))
  }

  # matrice de distances
  if (inherits(x, "dist")) {
    m <- as.matrix(x)
    d <- .hstat_dr_avec_noms(as.data.frame(m), .hstat_dr_noms_lignes(m))
    return(list(data = d, classe = cl, msg = NULL,
      note = tr("matrice de distances dépliée en tableau carré")))
  }

  # matrice
  if (is.matrix(x)) {
    d <- as.data.frame(unclass(x))
    if (is.null(colnames(x))) names(d) <- paste0(nom, "_", seq_along(d))
    d <- .hstat_dr_avec_noms(d, .hstat_dr_noms_lignes(x))
    return(list(data = d, classe = cl, msg = NULL,
      note = tr("matrice convertie en tableau")))
  }

  # liste
  if (is.list(x)) {
    lg <- vapply(x, function(e) if (is.atomic(e)) length(e) else -1L, integer(1))
    if (length(lg) && all(lg > 0L) && length(unique(lg)) == 1L) {
      d <- as.data.frame(x, stringsAsFactors = FALSE)
      rownames(d) <- NULL
      return(list(data = d, classe = cl, msg = NULL,
        note = tr("liste convertie en tableau")))
    }
    det <- paste(sprintf("%s : %s",
                         if (is.null(names(x))) seq_along(x) else names(x),
                         ifelse(lg < 0L, tr("non atomique"), lg)),
                 collapse = ", ")
    return(rien(trf("« %s » est une liste de composantes de longueurs différentes (%s) : ce n'est pas un tableau d'observations. Choisissez un autre jeu de données.",
                    nom, det)))
  }

  # vecteur
  if (is.atomic(x) && length(x)) {
    d <- stats::setNames(data.frame(x, stringsAsFactors = FALSE), nom)
    d <- .hstat_dr_avec_noms(d, names(x))
    return(list(data = d, classe = cl, msg = NULL,
      note = trf("vecteur converti en colonne « %s »", nom)))
  }

  # PAS DE REPLI PAR `as.matrix()` SUR TOUT OBJET A DEUX DIMENSIONS, et c'est
  # une mesure qui l'a decide.
  rien(trf("objet de classe « %s » : HStat ne sait pas en faire un tableau d'observations.", cl))
}

# `as.character(x)[1] %||% ""` NE SUFFIT PAS : `%||%` ne rattrape que `NULL`,
# et `as.character(NULL)[1]` vaut `NA_character_`.
.hstat_dr_texte <- function(x) {
  v <- tryCatch(as.character(x), error = function(e) character(0))
  if (!length(v) || is.na(v[1])) return("")
  trimws(v[1])
}

# -- « Vouliez-vous dire »
# Un refus qui ne propose
# rien laisse chercher.
.hstat_dr_proches <- function(nom, noms, n = 6L) {
  if (!length(noms) || !nzchar(nom)) return(character(0))
  # RECHERCHE LITTERALE, et la casse est retiree des DEUX cotes plutot que
  # passee en `ignore.case`.
  dedans <- noms[grepl(tolower(nom), tolower(noms), fixed = TRUE)]
  dedans <- dedans[order(nchar(dedans), tolower(dedans))]
  d <- tryCatch(as.vector(utils::adist(nom, noms, ignore.case = TRUE)),
                error = function(e) rep(NA_integer_, length(noms)))
  # Le plafond suit la longueur de la saisie : une tolerance fixe accepterait
  # n'importe quoi sur un nom de trois lettres.
  seuil <- max(1L, ceiling(nchar(nom) / 3))
  ok <- !is.na(d) & d <= seuil
  # LES DEUX FAMILLES SE CLASSENT ENSEMBLE, par distance d'edition.
  cand <- unique(c(dedans, noms[ok]))
  pos <- match(cand, noms)
  utils::head(cand[order(d[pos], pos)], n)
}

# -- Resolution d'un nom ------------------------------------------------------
# Rend `list(nom, paquet, fichier, suggestions)`, ou NULL si le nom est
# inconnu.
.hstat_dr_resoudre <- function(nom, cat) {
  nom <- .hstat_dr_texte(nom)
  paquet <- NA_character_
  # « paquet::jeu » : la seule facon de lever une ambiguite sans deviner.
  if (grepl("::", nom, fixed = TRUE)) {
    bouts <- strsplit(nom, "::", fixed = TRUE)[[1]]
    paquet <- trimws(bouts[1]); nom <- trimws(paste(bouts[-1], collapse = "::"))
  }
  if (!nzchar(nom)) return(NULL)
  # `which()` ET NON UN MASQUE LOGIQUE.
  sel <- cat[which(cat$Nom == nom), , drop = FALSE]
  if (!nrow(sel)) sel <- cat[which(tolower(cat$Nom) == tolower(nom)), , drop = FALSE]
  if (!is.na(paquet) && nzchar(paquet))
    sel <- sel[which(sel$Paquet == paquet | tolower(sel$Paquet) == tolower(paquet)), ,
               drop = FALSE]
  if (!nrow(sel))
    return(list(nom = NULL, suggestions = .hstat_dr_proches(nom, cat$Nom)))
  
  # Le catalogue est trie `datasets` d'abord : la premiere ligne est donc la
  # bonne, et les autres sont NOMMEES plutot que passees sous silence.
  list(nom = sel$Nom[1], paquet = sel$Paquet[1], fichier = sel$Fichier[1],
       autres = if (nrow(sel) > 1) sel$Paquet[-1] else character(0),
       titre = sel$Titre[1], suggestions = character(0))
}

# -- Chargement d'un jeu integre ----------------------------------------------
# MEME FORME QUE `hstat_load_data()`.
hstat_donnees_r <- function(nom, packages = NULL, catalogue = NULL) {
  ko <- function(msg, sug = character(0))
    list(ok = FALSE, msg = msg, suggestions = sug, data = NULL)
  brut <- .hstat_dr_texte(nom)
  if (!nzchar(brut))
    return(ko(tr("Écrivez le nom d'un jeu de données intégré à R, par exemple « iris ».")))
  cat_df <- if (is.null(catalogue)) hstat_donnees_r_catalogue(packages) else catalogue
  r <- .hstat_dr_resoudre(brut, cat_df)
  if (is.null(r) || is.null(r$nom)) {
    sug <- if (is.null(r)) character(0) else r$suggestions
    msg <- if (length(sug))
      trf("Aucun jeu de données nommé « %s ». Vouliez-vous dire : %s ?",
          brut, paste(sug, collapse = ", "))
    else
      trf("Aucun jeu de données nommé « %s » parmi les %s jeux des paquets installés. Consultez le catalogue ci-dessous.",
          brut, nrow(cat_df))
    return(ko(msg, sug))
  }
  # `parent = globalenv()`, ET SURTOUT PAS `emptyenv()`.
  e <- new.env(parent = globalenv())
  charge <- tryCatch({
    suppressWarnings(utils::data(list = r$fichier, package = r$paquet, envir = e))
    TRUE
  }, error = function(err) FALSE)
  objets <- ls(e, all.names = TRUE)
  if (!charge || !length(objets))
    return(ko(trf("Le jeu « %s » est annoncé par le paquet %s mais n'a pas pu être chargé. Vérifiez l'installation de ce paquet.",
                  r$nom, r$paquet)))
  # L'objet porte l'ALIAS.
  note_obj <- NULL
  if (r$nom %in% objets) {
    x <- get(r$nom, envir = e, inherits = FALSE)
  } else if (length(objets) == 1L) {
    x <- get(objets[1], envir = e, inherits = FALSE)
    note_obj <- trf("objet « %s » du fichier de données « %s »", objets[1], r$fichier)
  } else {
    return(ko(trf("Le fichier de données « %s » crée %s objets (%s) et aucun ne s'appelle « %s ». Écrivez directement le nom de l'objet voulu.",
                  r$fichier, length(objets), paste(objets, collapse = ", "), r$nom)))
  }
  conv <- hstat_donnees_r_convertir(x, r$nom)
  if (is.null(conv$data) || !nrow(conv$data))
    return(ko(conv$msg %||% trf("Le jeu « %s » ne donne aucune observation.", r$nom)))
  d <- hstat_df_to_utf8(as.data.frame(conv$data))
  notes <- c(note_obj, conv$note,
             if (length(r$autres))
               trf("« %s » existe aussi dans : %s. Écrivez « paquet::%s » pour en choisir un autre.",
                   r$nom, paste(unique(r$autres), collapse = ", "), r$nom))
  list(ok = TRUE, msg = NULL,
       data = d, mode = "memory", con = NULL, table = NULL,
       full_nrow = nrow(d), full_ncol = ncol(d),
       full_na = sum(is.na(d)), is_sampled = FALSE,
       kind = "rdata",
       # La taille n'a pas de sens ici : rien n'est lu sur disque.
       size = NA_real_,
       nom = r$nom, paquet = r$paquet, titre = r$titre,
       classe = conv$classe, notes = notes,
       suggestions = character(0))
}

#  Statistiques exactes sur le jeu COMPLET via DuckDB (mode hors-memoire)

# -- Construit les expressions SQL d'agregation pour une colonne numerique ----
# Retourne un vecteur nomme statistique -> expression SQL.
.hstat_sql_stat_exprs <- function(col, stats_sel) {
  q <- hstat_sql_ident(col)
  all_ex <- c(
    mean   = sprintf("AVG(%s)", q),
    median = sprintf("MEDIAN(%s)", q),
    sd     = sprintf("STDDEV_SAMP(%s)", q),
    var    = sprintf("VAR_SAMP(%s)", q),
    cv     = sprintf("(CASE WHEN AVG(%s)=0 THEN NULL ELSE 100.0*STDDEV_SAMP(%s)/ABS(AVG(%s)) END)",
                     q, q, q),
    min    = sprintf("MIN(%s)", q),
    max    = sprintf("MAX(%s)", q),
    q1     = sprintf("QUANTILE_CONT(%s, 0.25)", q),
    q3     = sprintf("QUANTILE_CONT(%s, 0.75)", q)
  )
  all_ex[stats_sel[stats_sel %in% names(all_ex)]]
}

# -- Statistiques descriptives GLOBALES exactes sur le jeu complet ------------
# Sortie identique a make_summ_global.
hstat_duckdb_describe_global <- function(con, tbl, num_vars, stats_sel) {
  rows <- lapply(num_vars, function(v) {
    ex <- .hstat_sql_stat_exprs(v, stats_sel)
    if (length(ex) == 0) return(NULL)
    sel <- paste(sprintf("%s AS %s", ex, names(ex)), collapse = ", ")
    r <- DBI::dbGetQuery(con, sprintf("SELECT %s FROM %s", sel, hstat_sql_ident(tbl)))
    data.frame(Facteurs = "Global", Variable = v, r, check.names = FALSE)
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# -- Statistiques descriptives GROUPEES exactes sur le jeu complet -----------
# Sortie identique a make_summ_grouped.
hstat_duckdb_describe_grouped <- function(con, tbl, group_vars, num_vars, stats_sel) {
  gcols <- paste(hstat_sql_ident(group_vars), collapse = ", ")
  rows <- lapply(num_vars, function(v) {
    ex <- .hstat_sql_stat_exprs(v, stats_sel)
    if (length(ex) == 0) return(NULL)
    sel <- paste(sprintf("%s AS %s", ex, names(ex)), collapse = ", ")
    r <- DBI::dbGetQuery(con, sprintf(
      "SELECT %s, %s FROM %s GROUP BY %s ORDER BY %s",
      gcols, sel, hstat_sql_ident(tbl), gcols, gcols))
    r$Variable <- v
    r[, c(group_vars, "Variable", names(ex)), drop = FALSE]
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

# -- Table de contingence exacte sur le jeu complet --------------------------
hstat_duckdb_crosstab <- function(con, tbl, row_var, col_var) {
  r <- DBI::dbGetQuery(con, sprintf(
    'SELECT %s AS rv, %s AS cv, COUNT(*) AS n FROM %s GROUP BY 1, 2',
    hstat_sql_ident(row_var), hstat_sql_ident(col_var), hstat_sql_ident(tbl)))
  if (nrow(r) == 0) return(NULL)
  stats::xtabs(n ~ rv + cv, data = r)
}

# -- Matrice de correlation exacte sur le jeu complet ------------------------
hstat_duckdb_cor <- function(con, tbl, num_vars) {
  k <- length(num_vars)
  if (k < 2) return(NULL)
  m <- diag(1, k); dimnames(m) <- list(num_vars, num_vars)
  for (i in 1:(k-1)) for (j in (i+1):k) {
    val <- tryCatch(DBI::dbGetQuery(con, sprintf(
      'SELECT CORR(%s,%s) AS r FROM %s',
      hstat_sql_ident(num_vars[i]), hstat_sql_ident(num_vars[j]), hstat_sql_ident(tbl)))$r[1],
      error = function(e) NA_real_)
    m[i, j] <- m[j, i] <- val
  }
  m
}

# Graine par defaut de l'application (modifiable par l'utilisateur dans l'UI).
HSTAT_DEFAULT_SEED <- 123L

# Applique une graine de maniere sure juste avant une operation aleatoire.
hstat_set_seed <- function(seed = NULL) {
  s <- suppressWarnings(as.integer(seed))
  if (length(s) == 0 || is.na(s)) s <- HSTAT_DEFAULT_SEED
  set.seed(s)
  invisible(s)
}

# Cache memoire pour les agregations DuckDB (evite de relancer une requete SQL
# identique sur un tres gros fichier).

.hstat_cache <- new.env(parent = emptyenv())

# LE CACHE EST DANS LE PAQUET, LES SESSIONS SONT DANS LE PROCESSUS.
.hstat_session_id <- function() {
  d <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (is.null(d)) return("hors-session")
  tok <- tryCatch(d$token, error = function(e) NULL)
  if (is.null(tok) || !nzchar(as.character(tok)[1])) "hors-session"
  else as.character(tok)[1]
}

# Vide le cache DE LA SESSION COURANTE (a appeler au chargement d'un nouveau
# fichier, ET a la fermeture de la session).
hstat_cache_clear <- function(id = NULL) {
  jeton <- if (is.null(id) || !nzchar(as.character(id)[1])) .hstat_session_id()
           else as.character(id)[1]
  prefixe <- paste0(jeton, "::")
  cles <- ls(.hstat_cache, all.names = TRUE)
  cles <- cles[startsWith(cles, prefixe)]
  if (length(cles)) rm(list = cles, envir = .hstat_cache)
  invisible(NULL)
}

# Memoise le resultat de 'fn()' sous une cle.
hstat_cache_get <- function(key, fn) {
  if (exists(key, envir = .hstat_cache, inherits = FALSE))
    return(get(key, envir = .hstat_cache, inherits = FALSE))
  val <- fn()
  assign(key, val, envir = .hstat_cache)
  val
}

# Construit une cle de cache stable a partir d'elements (table + parametres).
hstat_cache_key <- function(...) {
  parts <- vapply(list(...), function(x) paste(as.character(x), collapse = "|"),
                  character(1))
  # L'IDENTIFIANT DE SESSION VIENT EN TETE, et il n'est pas decoratif.
  paste(c(.hstat_session_id(), parts), collapse = "::")
}

# Protege les noms de colonnes contenant des caracteres speciaux dans une
# formule de calcul, et convertit mean()/sum() en rowMeans()/rowSums() sur
# plusieurs colonnes.
auto_quote_colnames <- function(formula_str, col_names) {
  # CE QUI EST CITE SORT DU JEU.
  jetons <- character(0)
  poser <- function(txt) {
    jetons[[length(jetons) + 1L]] <<- txt
    paste0("\001", length(jetons), "\002")
  }
  # 1. Ce que l'utilisateur a DEJA cite lui-meme : on n'y touche plus.
  repeat {
    m <- regexpr("`[^`]*`", formula_str)
    if (m == -1L) break
    regmatches(formula_str, m) <- poser(regmatches(formula_str, m))
  }
  # 2. Les noms a citer, du plus long au plus court, chacun mis a l'abri.
  cols_sorted <- col_names[order(nchar(col_names), decreasing = TRUE)]
  for (col in cols_sorted) {
    if (!nzchar(col) || !grepl("[-/ +*^()%$@!?]|^[0-9]", col, perl = TRUE)) next
    while (grepl(col, formula_str, fixed = TRUE))
      formula_str <- sub(col, poser(paste0("`", col, "`")), formula_str,
                         fixed = TRUE)
  }
  formula_str <- gsub("\\bmean\\s*\\(\\s*c\\s*\\(([^)]+)\\)\\s*\\)",
    "rowMeans(cbind(\\1), na.rm=TRUE)", formula_str, perl = TRUE)
  formula_str <- gsub("\\bmean\\s*\\(([^)]+,[^)]+)\\)",
    "rowMeans(cbind(\\1), na.rm=TRUE)", formula_str, perl = TRUE)
  formula_str <- gsub("\\bsum\\s*\\(\\s*c\\s*\\(([^)]+)\\)\\s*\\)",
    "rowSums(cbind(\\1), na.rm=TRUE)", formula_str, perl = TRUE)
  formula_str <- gsub("\\bsum\\s*\\(([^)]+,[^)]+)\\)",
    "rowSums(cbind(\\1), na.rm=TRUE)", formula_str, perl = TRUE)
  # 3. Restitution, dans l'ordre inverse de la pose.
  for (i in rev(seq_along(jetons)))
    formula_str <- gsub(paste0("\001", i, "\002"), jetons[[i]],
                        formula_str, fixed = TRUE)
  formula_str
}

# -- Evaluation SECURISEE des formules du calculateur de variables.
# 1) l'expression est analysee (AST) : seules les fonctions de la liste blanche
#    HSTAT_FORMULA_FUNS sont autorisees ;
# 2) l'evaluation se fait dans un environnement clos (parent = emptyenv()) ne
#    contenant que les colonnes du jeu de donnees et ces fonctions.
HSTAT_FORMULA_FUNS <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "(",
  "==", "!=", "<", "<=", ">", ">=", "&", "|", "!",
  "ifelse", "is.na", "c", "cbind",
  "log", "log2", "log10", "log1p", "exp", "sqrt", "abs", "sign",
  "round", "floor", "ceiling", "trunc", "signif",
  "min", "max", "pmin", "pmax", "mean", "sum", "cumsum", "cumprod",
  "sd", "median", "var", "mad", "scale", "rank",
  "rowMeans", "rowSums",
  "paste", "paste0", "as.numeric", "as.character", "as.integer", "as.factor",
  "toupper", "tolower", "trimws", "nchar", "substr",
  "sin", "cos", "tan"
)

# Verifie recursivement que l'AST ne contient que des appels autorises.
hstat_check_formula_ast <- function(expr) {
  if (is.call(expr)) {
    fn <- expr[[1]]
    if (!(is.name(fn) && as.character(fn) %in% HSTAT_FORMULA_FUNS)) {
      stop(trf(
        "Fonction non autorisée dans la formule : « %s ». Seuls les opérateurs arithmétiques/logiques et les fonctions statistiques usuelles (log, sqrt, ifelse, rowMeans, rowSums, mean, sum...) sont permis.",
        paste(deparse(fn), collapse = " ")), call. = FALSE)
    }
    for (i in seq_along(expr)[-1]) hstat_check_formula_ast(expr[[i]])
  } else if (!(is.name(expr) || is.atomic(expr) || is.null(expr))) {
    stop("Élément non autorisé dans la formule.", call. = FALSE)
  }
  invisible(TRUE)
}

# -- Cout d'une formule : la liste blanche ne borne que ce qu'on appelle -----
# Le bac a sable empeche d'EXECUTER autre chose.
HSTAT_FORMULA_MAX_CHARS <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_FORMULA_MAX_CHARS", "10000")))
  if (!is.finite(v) || v < 100) 10000L else v
}

# Evalue une formule validee sur un data.frame, sans acces au reste de R.
hstat_safe_eval <- function(formula_str, data) {
  n <- sum(nchar(as.character(formula_str), type = "bytes"))
  if (!is.finite(n) || n > HSTAT_FORMULA_MAX_CHARS)
    stop(trf("Formule trop longue : %s caractères pour un maximum de %s. Une formule de cette taille fige l'application pour toutes les sessions ; décomposez-la en plusieurs variables calculées.",
             format(n, big.mark = " "),
             format(HSTAT_FORMULA_MAX_CHARS, big.mark = " ")), call. = FALSE)
  exprs <- tryCatch(parse(text = formula_str, keep.source = FALSE),
                    error = function(e) stop("Formule invalide : ",
                                             conditionMessage(e), call. = FALSE))
  if (length(exprs) != 1)
    stop("La formule doit contenir une seule expression.", call. = FALSE)
  expr <- exprs[[1]]
  hstat_check_formula_ast(expr)
  funs <- lapply(HSTAT_FORMULA_FUNS, function(nm) {
    tryCatch(get(nm, envir = baseenv()),
             error = function(e) tryCatch(getExportedValue("stats", nm),
                                          error = function(e2) NULL))
  })
  names(funs) <- HSTAT_FORMULA_FUNS
  funs <- Filter(Negate(is.null), funs)
  enclos <- list2env(funs, parent = emptyenv())
  eval(expr, envir = as.list(data), enclos = enclos)
}

# Modelisation predictive (series temporelles / ML / DL) : infrastructure

# Plafond de lignes pour l'entrainement des modeles ML/DL (certains algorithmes
# comme SVM ou randomForest deviennent impraticables au-dela).
HSTAT_ML_MAX_N <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_ML_MAX_N", "200000")))
  if (!is.finite(v) || v < 1000) 200000L else v
}

# -- Interpretation automatique d'une valeur de R2 ----------------------------
.hstat_interp_r2 <- function(r2) {
  if (!is.finite(r2)) return("Non calculable sur ces données.")
  if (r2 >= 0.9) "Excellent : le modèle explique la quasi-totalité de la variance."
  else if (r2 >= 0.7) "Bon : le modèle capture l'essentiel de la structure des données."
  else if (r2 >= 0.5) "Moyen : pouvoir explicatif réel mais une part importante reste inexpliquée."
  else if (r2 >= 0.3) "Faible : le modèle n'explique qu'une part limitée de la variance."
  else "Très faible : le modèle explique peu ; revoir les variables ou le type de modèle."
}

.hstat_interp_mape <- function(m) {
  if (!is.finite(m)) return("MAPE non calculable (valeurs observées nulles).")
  if (m < 10) "Excellente précision (erreur relative moyenne < 10 %)."
  else if (m < 20) "Bonne précision (erreur relative moyenne < 20 %)."
  else if (m < 50) "Précision moyenne : prévisions indicatives."
  else "Précision faible : prévisions peu fiables en l'état."
}

# -- Metriques de REGRESSION avec interpretation -------------------------------
# Retourne un data.frame : Metrique, Valeur, Interpretation.
hstat_metrics_reg <- function(obs, pred) {
  obs <- as.numeric(obs); pred <- as.numeric(pred)
  ok <- is.finite(obs) & is.finite(pred)
  obs <- obs[ok]; pred <- pred[ok]
  if (length(obs) < 2)
    return(data.frame(Metrique = "Erreur", Valeur = NA_real_,
                      Interpretation = "Pas assez d'observations valides.",
                      stringsAsFactors = FALSE))
  err  <- obs - pred
  rmse <- sqrt(mean(err^2))
  mae  <- mean(abs(err))
  mape <- if (all(obs == 0)) NA_real_ else mean(abs(err[obs != 0] / obs[obs != 0])) * 100
  ss_res <- sum(err^2); ss_tot <- sum((obs - mean(obs))^2)
  r2   <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  sc   <- stats::sd(obs)
  data.frame(
    Metrique = c("RMSE", "MAE", "MAPE (%)", "R2"),
    Valeur   = round(c(rmse, mae, mape, r2), 4),
    Seuils = c(
      "Pas de seuil universel : à comparer à l'écart-type de la cible (excellent si RMSE << sigma) et entre modèles",
      "Pas de seuil universel : exprimée dans l'unité de la cible ; plus petite = meilleure",
      "< 10 % excellent ; 10-20 % bon ; 20-50 % moyen ; > 50 % faible",
      "< 0,3 tres faible ; 0,3-0,5 faible ; 0,5-0,7 moyen ; 0,7-0,9 bon ; >= 0,9 excellent"),
    Interpretation = c(
      trf("Erreur quadratique moyenne : %s unité(s) de la variable cible (écart-type observé : %s). Pénalise fortement les grosses erreurs.",
              format(round(rmse, 3), big.mark = " "), format(round(sc, 3), big.mark = " ")),
      trf("En moyenne, la prédiction s'écarte de %s unité(s) de la valeur réelle.",
              format(round(mae, 3), big.mark = " ")),
      .hstat_interp_mape(mape),
      .hstat_interp_r2(r2)),
    stringsAsFactors = FALSE)
}

# -- Matrice de confusion en data.frame ----------------------------------------
hstat_confusion_df <- function(obs, pred) {
  obs  <- factor(obs); pred <- factor(pred, levels = levels(obs))
  as.data.frame.matrix(table(Observe = obs, Predit = pred))
}

# -- Metriques de CLASSIFICATION avec interpretation
# prob : matrice/vecteur de probabilites (facultatif, pour l'AUC binaire).
hstat_metrics_cls <- function(obs, pred, prob = NULL) {
  obs  <- factor(obs); pred <- factor(pred, levels = levels(obs))
  ok   <- !is.na(obs) & !is.na(pred)
  obs  <- obs[ok]; pred <- pred[ok]
  if (length(obs) < 2)
    return(data.frame(Metrique = "Erreur", Valeur = NA_real_,
                      Interpretation = "Pas assez d'observations valides.",
                      stringsAsFactors = FALSE))
  tab <- table(obs, pred)
  acc <- sum(diag(tab)) / sum(tab)
  # Kappa de Cohen
  pe  <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
  kap <- if (pe < 1) (acc - pe) / (1 - pe) else NA_real_
  # Precision / rappel / F1 par classe puis moyenne macro
  prec <- diag(tab) / pmax(colSums(tab), 1)
  rec  <- diag(tab) / pmax(rowSums(tab), 1)
  f1   <- ifelse(prec + rec > 0, 2 * prec * rec / (prec + rec), 0)
  # AUC pour le cas binaire si probabilites fournies
  auc <- NA_real_
  if (!is.null(prob) && nlevels(obs) == 2 &&
      requireNamespace("pROC", quietly = TRUE)) {
    p <- if (is.matrix(prob) || is.data.frame(prob)) as.numeric(prob[ok, ncol(prob)]) else as.numeric(prob)[ok]
    auc <- tryCatch(as.numeric(pROC::auc(pROC::roc(obs, p, quiet = TRUE, levels = levels(obs), direction = "<"))),
                    error = function(e) NA_real_)
  }
  maj <- max(prop.table(table(obs)))
  vals <- c(acc, kap, mean(prec, na.rm = TRUE), mean(rec, na.rm = TRUE),
            mean(f1, na.rm = TRUE), auc)
  data.frame(
    Metrique = c("Exactitude (accuracy)", "Kappa de Cohen", "Précision (macro)",
                 "Rappel / sensibilité (macro)", "F1-score (macro)", "AUC (ROC)"),
    Valeur   = round(vals, 4),
    Seuils = c(
      "À comparer à la part de la classe majoritaire : nettement au-dessus = modèle informatif",
      "< 0,2 negligeable ; 0,2-0,4 faible ; 0,4-0,6 modere ; 0,6-0,8 substantiel ; > 0,8 quasi parfait (Landis & Koch)",
      ">= 0,9 excellente ; 0,7-0,9 bonne ; 0,5-0,7 moyenne ; < 0,5 faible",
      ">= 0,9 excellent ; 0,7-0,9 bon ; 0,5-0,7 moyen ; < 0,5 faible",
      ">= 0,9 excellent ; 0,8-0,9 bon ; 0,6-0,8 moyen ; < 0,6 faible",
      "0,5 = hasard ; 0,7-0,8 acceptable ; 0,8-0,9 bonne ; >= 0,9 excellente discrimination"),
    Interpretation = c(
      trf("%.1f %% des observations sont bien classées (classe majoritaire seule : %.1f %% ; le modèle %s).",
              100 * acc, 100 * maj,
              if (is.finite(acc) && acc > maj) tr("fait mieux que ce niveau de référence")
              else tr("ne dépasse pas ce niveau de référence")),
      if (!is.finite(kap)) "Non calculable." else if (kap >= 0.8) "Accord quasi parfait au-delà du hasard."
      else if (kap >= 0.6) "Accord substantiel au-delà du hasard."
      else if (kap >= 0.4) "Accord modéré au-delà du hasard."
      else if (kap >= 0.2) "Accord faible : à peine mieux que le hasard."
      else "Accord négligeable : équivalent au hasard.",
      "Parmi les prédictions d'une classe, part réellement correcte (moyenne des classes).",
      "Parmi les cas réels d'une classe, part correctement retrouvée (moyenne des classes).",
      "Compromis précision/rappel (1 = parfait). Robuste aux classes déséquilibrées.",
      if (!is.finite(auc)) "AUC calculée uniquement en classification binaire (avec probabilités)."
      else if (auc >= 0.9) "Discrimination excellente entre les deux classes."
      else if (auc >= 0.8) "Bonne discrimination."
      else if (auc >= 0.7) "Discrimination acceptable."
      else "Discrimination faible (0.5 = hasard)."),
    stringsAsFactors = FALSE)
}

# -- Interpretation globale automatique d'un modele -----------------------------
hstat_model_interpretation <- function(task, metrics_df, model_label,
                                       n_train, n_test, notes = NULL) {
  get_v <- function(m) {
    i <- match(m, metrics_df$Metrique)
    if (is.na(i)) NA_real_ else metrics_df$Valeur[i]
  }
  head_txt <- trf(
    "Le modèle %s a été entraîné sur %s observation(s) puis évalué sur %s observation(s) de test jamais vues pendant l'entraînement : les métriques ci-dessus reflètent donc sa capacité de généralisation, pas sa mémoire.",
    model_label, format(n_train, big.mark = " "), format(n_test, big.mark = " "))
  core <- if (identical(task, "regression")) {
    r2 <- get_v("R2"); mape <- get_v("MAPE (%)")
    paste0(.hstat_interp_r2(r2), " ", .hstat_interp_mape(mape))
  } else {
    acc <- get_v("Exactitude (accuracy)"); f1 <- get_v("F1-score (macro)")
    trf("Avec %.1f %% de bonnes classifications et un F1 macro de %.2f, %s",
            100 * acc, f1,
            if (is.finite(f1) && f1 >= 0.8) tr("le modèle est opérationnel pour la prédiction.")
            else if (is.finite(f1) && f1 >= 0.6) tr("le modèle est utilisable mais perfectible (plus de données, autres variables, réglages).")
            else tr("le modèle n'est pas encore fiable : enrichir les variables ou changer d'algorithme."))
  }
  paste(c(head_txt, core, notes), collapse = " ")
}

# Le cadre carre des analyses multivariees : les dimensions d'origine de
# ggplot2

# Le peripherique que R ouvre par defaut.
HSTAT_CARRE_RES <- 120
HSTAT_CARRE_PX  <- 840   # 7 pouces a HSTAT_CARRE_RES

# LA HAUTEUR N'EST PAS FIXEE DANS L'INTERFACE, ELLE EST CALCULEE AU SERVEUR.
hstat_carre_ui <- function(id, spinner = NULL, largeur = HSTAT_CARRE_PX) {
  sortie <- shiny::plotOutput(id, height = "auto")
  if (!is.null(spinner)) sortie <- withSpinner(sortie, color = spinner)
  shiny::div(style = paste0("max-width:", largeur,
                            "px; margin:0 auto; width:100%;"), sortie)
}

# La fonction a passer en `height =` d'un `renderPlot`.
hstat_carre_hauteur <- function(session, id, defaut = HSTAT_CARRE_PX) {
  force(session); force(id); force(defaut)
  function() {
    l <- session$clientData[[paste0("output_", id, "_width")]]
    if (is.null(l) || !is.numeric(l) || !is.finite(l) || l < 1) defaut else l
  }
}

# Export universel de graphiques : PNG/JPG/TIFF/BMP/PDF/SVG, DPI jusqu'a 20 000

# Bloc UI reutilisable : format, dimensions, DPI (max 20 000) + bouton.
hstat_export_plot_ui <- function(ns, prefix, width = 10, height = 6,
                                 theme = TRUE) {
  shiny::tagList(
    if (isTRUE(theme))
      shiny::fluidRow(shiny::column(6, shiny::selectInput(ns(paste0(prefix, "Theme")), "Thème",
                                     choices = HSTAT_THEMES_GG,
                                     selected = "minimal"))),
    shiny::fluidRow(
      shiny::column(3, hstat_format_input(ns(paste0(prefix, "Fmt")), "Format")),
      shiny::column(3, shiny::numericInput(ns(paste0(prefix, "W")), "Largeur (pouces)",
                             value = width, min = 3, max = 30, step = 0.5)),
      shiny::column(3, shiny::numericInput(ns(paste0(prefix, "H")), "Hauteur (pouces)",
                             value = height, min = 3, max = 30, step = 0.5)),
      shiny::column(3, hstat_dpi_input(ns(paste0(prefix, "Dpi")), "DPI (max 20 000)"))),
    # REGLAGES PROPRES AU FORMAT.
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'jpeg'", ns(paste0(prefix, "Fmt"))),
      fluidRow(column(6, sliderInput(ns(paste0(prefix, "Qual")), "Qualité JPEG",
                                     min = 50, max = 100, value = 95, step = 5)))),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tiff'", ns(paste0(prefix, "Fmt"))),
      fluidRow(column(6, selectInput(ns(paste0(prefix, "Comp")), "Compression TIFF",
                                     choices = HSTAT_TIFF_COMPRESSION,
                                     selected = "lzw")))),
    shiny::tags$small(style = "color:#6b7280;",
      "PDF et SVG sont vectoriels (résolution infinie, DPI sans objet). ",
      "Pour les formats matriciels, au-delà d'un certain DPI les dimensions physiques ",
      "sont automatiquement réduites afin de garder une image ouvrable (plafond de sécurité en pixels)."),
    shiny::div(style = "margin-top:8px;",
        shiny::downloadButton(ns(paste0(prefix, "Dl")), "Télécharger le graphique",
                       class = "btn-success"))
  )
}

# Theme choisi dans le bloc d'export, pret a etre ajoute a un ggplot.
hstat_export_theme <- function(input, prefix, base_size = 12) {
  v <- input[[paste0(prefix, "Theme")]]
  if (is.null(v) || !nzchar(v)) return(NULL)
  viz_get_theme(v, base_size = base_size)
}

# Handler d'export associe. plot_fun() doit renvoyer un ggplot (ou NULL).
hstat_export_plot_handler <- function(input, prefix, plot_fun, fname = "graphique") {
  reglages <- function() {
    fmt <- hstat_img_fmt(input[[paste0(prefix, "Fmt")]] %||% "png")
    w   <- hstat_finite(input[[paste0(prefix, "W")]], 10);  w <- max(3, min(30, w))
    h   <- hstat_finite(input[[paste0(prefix, "H")]], 6);   h <- max(3, min(30, h))
    dpi <- hstat_finite(input[[paste0(prefix, "Dpi")]], 300)
    qual <- hstat_finite(input[[paste0(prefix, "Qual")]], 95)
    list(fmt = fmt, w = w, h = h, dpi = max(72, min(HSTAT_DPI_MAX, dpi)),
         qualite = max(50, min(100, qual)),
         compression = input[[paste0(prefix, "Comp")]] %||% "lzw")
  }
  shiny::downloadHandler(
    filename = function() {
      fmt <- reglages()$fmt
      ext <- if (identical(fmt, "jpeg")) "jpg" else fmt
      paste0(fname, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      r <- reglages()
      # La taille demandee est RESPECTEE : c'est la resolution qui plie si le
      # matriciel ne peut pas suivre.
      eff <- hstat_dpi_effectif(r$w, r$h, r$dpi)
      if (isTRUE(eff$plafonne))
        shiny::showNotification(eff$note, type = "warning", duration = 10)
      g <- tryCatch(plot_fun(), error = function(e) NULL)
      hstat_ecrire_image(file, g, r$fmt, r$w, r$h, r$dpi,
        echec = "Aucun graphique à exporter : lancez d'abord l'analyse.",
        qualite = r$qualite, compression = r$compression)
    })
}

# NB : hstat_finite() est defini plus haut dans ce fichier.

# Personnalisation d'apparence reutilisable pour les graphiques de modelisation

hstat_plot_opts_ui <- function(ns, prefix) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::textInput(ns(paste0(prefix, "Title")), "Titre", value = "")),
      shiny::column(6, shiny::textInput(ns(paste0(prefix, "Sub")), "Sous-titre", value = ""))),
    shiny::fluidRow(
      shiny::column(6, shiny::textInput(ns(paste0(prefix, "Xlab")), "Titre de l'axe X", value = "")),
      shiny::column(6, shiny::textInput(ns(paste0(prefix, "Ylab")), "Titre de l'axe Y", value = ""))),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(ns(paste0(prefix, "Theme")), "Thème",
               choices = HSTAT_THEMES_GG, selected = "minimal")),
      shiny::column(4, shiny::numericInput(ns(paste0(prefix, "Base")), "Taille du texte",
                             value = 13, min = 7, max = 30, step = 1)),
      shiny::column(4, shiny::selectInput(ns(paste0(prefix, "Legend")), "Légende",
               choices = c("Droite" = "right", "Gauche" = "left", "Haut" = "top",
                           "Bas" = "bottom", "Masquée" = "none"),
               selected = "right"))),
    shiny::fluidRow(
      shiny::column(4, if (requireNamespace("colourpicker", quietly = TRUE))
                  colourpicker::colourInput(ns(paste0(prefix, "Col")),
                    "Couleur principale", value = "#2c7fb8")
                else shiny::textInput(ns(paste0(prefix, "Col")),
                    "Couleur principale (hex)", value = "#2c7fb8")),
      shiny::column(4, shiny::numericInput(ns(paste0(prefix, "Lwd")), "Épaisseur des lignes",
                             value = 0.9, min = 0.2, max = 4, step = 0.1)),
      shiny::column(4, shiny::numericInput(ns(paste0(prefix, "Rot")), "Rotation des labels X (°)",
                             value = 0, min = 0, max = 90, step = 15))),
    # UNE CORRECTION FAITE ICI PROFITE AUX TROIS MODULES qui passent par ce kit
    # (apprentissage, apprentissage profond, series temporelles).
    hstat_plot_extras_ui(ns, prefix,
                         familles = c("axe", "cles", "marges", "styles", "bornes"))
  )
}

# Applique les options ci-dessus a un ggplot.
hstat_apply_plot_opts <- function(g, input, prefix) {
  if (is.null(g)) return(g)
  # UN SEUL CHOISISSEUR DE THEME.
  base <- hstat_finite(input[[paste0(prefix, "Base")]], 13)
  g <- g + viz_get_theme(input[[paste0(prefix, "Theme")]] %||% "minimal",
                         base_size = max(7, min(30, base)))
  lab <- function(x) { x <- input[[paste0(prefix, x)]]; if (is.null(x) || !nzchar(x)) NULL else x }
  if (!is.null(lab("Title"))) g <- g + ggplot2::labs(title = lab("Title"))
  if (!is.null(lab("Sub")))   g <- g + ggplot2::labs(subtitle = lab("Sub"))
  if (!is.null(lab("Xlab")))  g <- g + ggplot2::labs(x = lab("Xlab"))
  if (!is.null(lab("Ylab")))  g <- g + ggplot2::labs(y = lab("Ylab"))
  rot <- hstat_finite(input[[paste0(prefix, "Rot")]], 0)
  g <- g + ggplot2::theme(
    legend.position = input[[paste0(prefix, "Legend")]] %||% "right",
    axis.text.x = ggplot2::element_text(angle = rot,
                                        hjust = if (rot > 0) 1 else 0.5))
  # LE KIT SE POSE APRES LE THEME : un theme complet remplace tout ce qui le
  # precede, et `viz_get_theme()` ci-dessus en est un.
  ex <- hstat_plot_extras_lire(
    input, prefix, familles = c("axe", "cles", "marges", "styles", "bornes"))
  g <- g + hstat_plot_extras_theme(ex)
  hstat_plot_extras_scales(g, ex)
}

hstat_plot_opt <- function(input, prefix, what, default) {
  v <- input[[paste0(prefix, what)]]
  if (is.null(v) || (is.character(v) && !nzchar(v))) default else v
}

# Export de tableaux (CSV / Excel) et simulateur de predictions

hstat_export_table_ui <- function(ns, prefix) {
  shiny::div(style = "margin-top:6px;",
      shiny::downloadButton(ns(paste0(prefix, "Csv")), "CSV", class = "btn-sm"),
      shiny::downloadButton(ns(paste0(prefix, "Xlsx")), "Excel", class = "btn-sm"))
}

# Graine du LSTM.
HSTAT_LSTM_SEED <- 123L

# UNE METRIQUE SANS SON CONTEXTE NE SE RELIT PAS.
# 1. UNE ENTREE VIDE DISPARAIT, elle ne s'ecrit pas « NA ».
# 2. UN VECTEUR SE REPLIE EN UNE CELLULE (« a, b, c »).
# 3. LA VERSION ET LA DATE FERMENT LE TABLEAU.
hstat_details_techniques <- function(..., version = TRUE, date = TRUE) {
  elems <- list(...)
  noms  <- names(elems)
  if (is.null(noms)) noms <- rep("", length(elems))

  fmt <- function(v) {
    if (is.null(v) || !length(v)) return(NA_character_)
    if (is.factor(v)) v <- as.character(v)
    if (is.numeric(v)) v <- format(v, trim = TRUE, scientific = FALSE)
    v <- trimws(as.character(v))
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) NA_character_ else paste(v, collapse = ", ")
  }
  val <- vapply(elems, fmt, character(1))
  ok  <- nzchar(noms) & !is.na(val)

  # NOMS DE COLONNES SANS ACCENT, et ce n'est pas un oubli.
  out <- data.frame(noms[ok], val[ok], stringsAsFactors = FALSE)
  names(out) <- c("Detail", "Valeur")

  ligne <- function(k, v) stats::setNames(
    data.frame(k, v, stringsAsFactors = FALSE), names(out))
  if (isTRUE(version)) out <- rbind(out, ligne("Version HStat", hstat_version()))
  if (isTRUE(date))
    out <- rbind(out, ligne("Date d'export",
                            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  rownames(out) <- NULL
  out
}

hstat_export_table_handlers <- function(output, prefix, data_fun, fname = "resultats",
                                        details_fun = NULL) {
  # Un tableau seul est une liste d'un element : meme chemin d'ecriture que les
  # exports a plusieurs feuilles, donc meme garantie.
  tables <- function() {
    d <- data_fun()
    if (is.null(d)) return(NULL)
    out <- stats::setNames(list(as.data.frame(d)), fname)
    if (!is.null(details_fun)) {
      # UN DETAIL QUI ECHOUE N'EMPORTE PAS L'EXPORT.
      det <- tryCatch(details_fun(), error = function(e) NULL)
      if (!is.null(det) && NROW(det))
        out[["details_techniques"]] <- as.data.frame(det)
    }
    out
  }
  output[[paste0(prefix, "Csv")]]  <- hstat_csv_handler(tables, fname)
  output[[paste0(prefix, "Xlsx")]] <- hstat_classeur_handler(tables, fname)
}

# UN SEUL ECRIVAIN DE TABLEAUX, comme il n'y a qu'un ecrivain d'image
# Vingt telechargements de tableaux montaient chacun leur classeur.

HSTAT_MIME_XLSX <-
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

# Nom de feuille accepte par Excel : 31 caracteres, sans []:*?/\\ .
hstat_feuille_nom <- function(x, defaut = "Feuille") {
  s <- gsub("[^A-Za-z0-9_ .-]", "_", as.character(x)[1] %||% "")
  s <- substr(trimws(s), 1, 31)
  if (!nzchar(s)) defaut else s
}

# Liste nommee de tableaux -> classeur d'une feuille par element.
hstat_ecrire_classeur <- function(file, tables, dec = hstat_decimale_session()) {
  wb <- openxlsx::createWorkbook()
  vus <- character(0)
  for (i in seq_along(tables)) {
    nm <- hstat_feuille_nom(names(tables)[i] %||% "", paste0("Feuille", i))
    # Deux tableaux dont les noms se ressemblent a 31 caracteres pres donnent
    # le meme nom de feuille, et `addWorksheet()` refuse le doublon.
    if (nm %in% vus) nm <- hstat_feuille_nom(paste0(substr(nm, 1, 27), "_", i))
    vus <- c(vus, nm)
    openxlsx::addWorksheet(wb, nm)
    t <- as.data.frame(tables[[i]])
    openxlsx::writeData(wb, nm, hstat_decimale_classeur(t, dec))
    # Colonnes a la largeur de leur contenu : un extrait de texte code ou un
    # libelle long se lit sans avoir a elargir chaque colonne a la main.
    if (ncol(t)) openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(t)), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  length(tables)
}

# Liste nommee de tableaux -> archive ZIP d'un CSV par element.
hstat_ecrire_csv_zip <- function(file, tables, dec = hstat_decimale_session()) {
  dossier <- file.path(tempdir(), paste0("hstat_csv_", as.integer(Sys.time())))
  dir.create(dossier, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  noms <- character(0)
  for (i in seq_along(tables)) {
    nm <- hstat_feuille_nom(names(tables)[i] %||% "", paste0("tableau", i))
    f  <- paste0(nm, ".csv")
    hstat_ecrire_csv(tables[[i]], file.path(dossier, f), dec = dec)
    noms <- c(noms, f)
  }
  # `-q` : sans lui, chaque archive ecrit sa liste de fichiers dans la console
  # du serveur, a chaque telechargement.
  utils::zip(file, file.path(dossier, noms), flags = "-jq")
  noms
}

# Ecarte les tableaux absents ou vides d'une liste nommee.
hstat_tables_non_vides <- function(tables) {
  garde <- vapply(tables, function(d) {
    !is.null(d) && (is.data.frame(d) || is.matrix(d)) && NROW(d) > 0
  }, logical(1))
  tables[garde]
}

# Ce qu'on ecrit quand il n'y a rien a ecrire : un fichier valide qui le dit.
.hstat_tables_secours <- function(motif) {
  list(Info = data.frame(Message = motif %||% "Aucun résultat à exporter.",
                         stringsAsFactors = FALSE))
}

.hstat_tables_ou_motif <- function(tables_fun, libelle) {
  motif <- NULL
  tb <- tryCatch(tables_fun(), error = function(e) {
    motif <<- hstat_err_fr(e, libelle)
    NULL
  })
  if (is.data.frame(tb)) tb <- list("Résultats" = tb)
  if (is.null(tb) || !length(tb))
    tb <- .hstat_tables_secours(motif %||% "Aucun résultat à exporter : lancez d'abord l'analyse.")
  tb
}

# UN `content =` QUI LEVE OU N'ECRIT RIEN FAIT RENVOYER A SHINY SA PAGE
# D'ERREUR HTML, que le navigateur enregistre sous le nom demande.
hstat_contenu_sur <- function(content, motif_vide = NULL) {
  force(content)
  function(file) {
    err <- tryCatch({ content(file); NULL }, error = function(e) e)
    if (is.null(err) && file.exists(file) && isTRUE(file.size(file) > 0))
      return(invisible(file))
    motif <- if (is.null(err) || !nzchar(conditionMessage(err)))
      motif_vide %||% tr("Rien à exporter pour l'instant : lancez d'abord l'analyse.")
    else if (inherits(err, "validation")) conditionMessage(err)
    else hstat_err_fr(err)
    .hstat_fichier_motif(file, motif)
    invisible(file)
  }
}

# Un fichier valide du format de `file`, qui ne porte que le motif.
.hstat_fichier_motif <- function(file, motif) {
  ext <- tolower(tools::file_ext(file))
  tab <- data.frame(Message = motif, stringsAsFactors = FALSE)
  if (ext == "csv") return(hstat_ecrire_csv(tab, file))
  if (ext == "xlsx") return(hstat_ecrire_classeur(file, list(Info = tab)))
  if (ext %in% names(HSTAT_FORMATS_IMG_EXT))
    return(hstat_image_secours(file, HSTAT_FORMATS_IMG_EXT[[ext]], motif))
  if (ext == "zip") {
    d <- tempfile("hstat_motif")
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    f <- file.path(d, "message.txt")
    writeLines(enc2utf8(motif), f, useBytes = TRUE)
    return(utils::zip(file, f, flags = "-jq"))
  }
  writeLines(enc2utf8(motif), file, useBytes = TRUE)
}

# Extension de fichier -> format de `hstat_ecrire_image()`.
HSTAT_FORMATS_IMG_EXT <- c(png = "png", jpg = "jpeg", jpeg = "jpeg",
                           tif = "tiff", tiff = "tiff", bmp = "bmp",
                           svg = "svg", pdf = "pdf", eps = "eps")

# Telechargement d'un classeur Excel a partir d'une liste nommee de tableaux.
hstat_classeur_handler <- function(tables_fun, fname = "resultats",
                                   libelle = "Export Excel") {
  shiny::downloadHandler(
    filename = function() paste0(fname, "_", Sys.Date(), ".xlsx"),
    contentType = HSTAT_MIME_XLSX,
    content = function(file)
      hstat_ecrire_classeur(file, .hstat_tables_ou_motif(tables_fun, libelle)))
}

# Telechargement CSV : un seul tableau sort en `.csv`, plusieurs en `.zip`.
hstat_csv_handler <- function(tables_fun, fname = "resultats",
                              libelle = "Export CSV") {
  multiple <- function() length(.hstat_tables_ou_motif(tables_fun, libelle)) > 1
  # PAS de `contentType` : Shiny ne l'EVALUE PAS quand c'est une fonction, il
  # le passe tel quel dans l'en-tete HTTP (`download$contentType %||%
  # getContentType(filename)`).
  shiny::downloadHandler(
    filename = function()
      paste0(fname, "_", Sys.Date(), if (multiple()) ".zip" else ".csv"),
    content = function(file) {
      tb <- .hstat_tables_ou_motif(tables_fun, libelle)
      if (length(tb) > 1) hstat_ecrire_csv_zip(file, tb)
      else hstat_ecrire_csv(tb[[1]], file)
    })
}

# Les deux telechargements d'un meme resultat, declares d'un seul appel.
hstat_export_tables_handlers <- function(output, prefix, tables_fun,
                                         fname = "resultats", libelle = NULL) {
  lib <- libelle %||% fname
  output[[paste0(prefix, "Xlsx")]] <-
    hstat_classeur_handler(tables_fun, fname, paste("Export Excel", lib))
  output[[paste0(prefix, "Csv")]] <-
    hstat_csv_handler(tables_fun, fname, paste("Export CSV", lib))
}

# Formulaire dynamique : un champ par predicteur (numerique -> valeur mediane,
# facteur/caractere -> liste des modalites observees).
hstat_sim_inputs_ui <- function(ns, df, vars, prefix) {
  if (is.null(df) || length(vars) == 0) return(NULL)
  ctrls <- lapply(vars, function(v) {
    x <- df[[v]]
    if (is.numeric(x)) {
      md <- suppressWarnings(stats::median(x, na.rm = TRUE))
      shiny::numericInput(ns(paste0(prefix, "_", v)), v,
                   value = round(hstat_finite(md, 0), 4))
    } else {
      lv <- sort(unique(as.character(x[!is.na(x)])))
      if (length(lv) == 0) lv <- ""
      shiny::selectInput(ns(paste0(prefix, "_", v)), v, choices = lv)
    }
  })
  do.call(shiny::tagList, lapply(seq_along(ctrls), function(i)
    shiny::column(4, ctrls[[i]])))
}

# Recompose une ligne de donnees typee a partir du formulaire.
hstat_sim_collect <- function(input, df, vars, prefix) {
  vals <- lapply(vars, function(v) {
    raw <- input[[paste0(prefix, "_", v)]]
    x <- df[[v]]
    if (is.numeric(x)) hstat_finite(raw, stats::median(x, na.rm = TRUE))
    else {
      lv <- levels(factor(x))
      factor(as.character(raw %||% lv[1]), levels = lv)
    }
  })
  names(vals) <- vars
  as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
}

# Aligne un nouveau jeu de donnees (simulation par import) sur les types et
# niveaux de facteurs du jeu d'entrainement.
hstat_align_newdata <- function(newdf, ref, vars) {
  miss <- setdiff(vars, names(newdf))
  if (length(miss) > 0)
    return(list(data = NULL,
                warn = trf("Colonnes manquantes dans le fichier importé : %s", paste(miss, collapse = ", "))))
  out <- newdf[, vars, drop = FALSE]
  warns <- character(0)
  for (v in vars) {
    if (is.numeric(ref[[v]])) {
      out[[v]] <- suppressWarnings(as.numeric(out[[v]]))
    } else {
      lv <- levels(factor(ref[[v]]))
      bad <- setdiff(unique(as.character(out[[v]])), c(lv, NA))
      if (length(bad) > 0)
        warns <- c(warns, trf("%s : modalités inconnues ignorées (%s)", v, paste(utils::head(bad, 5), collapse = ", ")))
      out[[v]] <- factor(as.character(out[[v]]), levels = lv)
    }
  }
  list(data = out, warn = if (length(warns)) paste(warns, collapse = " ; ") else NULL)
}

# Fiches des modeles : principe, objectif, conditions d'application

hstat_model_doc <- function(id) {
  d <- list(
    # Series temporelles
    naive  = c("Naïf (dernière valeur)",
      "Chaque prévision reprend la dernière valeur observée.",
      "Servir de niveau de référence : tout modèle utile doit faire mieux.",
      "Aucune ; pertinent quand la série est une marche aléatoire sans tendance ni saison."),
    snaive = c("Naïf saisonnier",
      "Chaque prévision reprend la valeur observée à la même période de la saison précédente.",
      "Référence pour les séries saisonnières.",
      "Fréquence saisonnière > 1 et au moins 2 saisons complètes."),
    meanf  = c("Moyenne historique",
      "Toutes les prévisions valent la moyenne de la série.",
      "Référence pour les séries stationnaires autour d'un niveau.",
      "Série sans tendance ni saisonnalité marquées."),
    drift  = c("Marche aléatoire avec dérive",
      "Prolonge la droite reliant la première et la dernière observation.",
      "Capturer une tendance moyenne simple.",
      "Tendance approximativement linéaire ; pas de saisonnalité."),
    ses    = c("Lissage exponentiel simple (SES)",
      "Moyenne pondérée des observations, avec des poids décroissant exponentiellement vers le passe.",
      "Prévoir le niveau d'une série sans tendance ni saison.",
      "Série stationnaire en tendance et en saison ; au moins ~10 observations."),
    holt   = c("Holt (tendance)",
      "Étend le SES avec une composante de tendance lissée.",
      "Prévoir une série avec tendance persistante.",
      "Tendance approximativement linéaire ; pas de saisonnalité ; attention aux horizons longs (tendance extrapolée sans fin)."),
    holtd  = c("Holt amorti",
      "Comme Holt, mais la tendance s'atténue progressivement vers un plateau.",
      "Prévisions long terme plus prudentes que Holt.",
      "Tendance qui a des raisons de ralentir ; pas de saisonnalité."),
    hwadd  = c("Holt-Winters additif",
      "Lissage du niveau, de la tendance et d'une saisonnalité d'amplitude constante.",
      "Prévoir une série saisonnière dont les oscillations gardent la même ampleur.",
      "Fréquence > 1, au moins 2 saisons complètes, amplitude saisonnière stable."),
    hwmul  = c("Holt-Winters multiplicatif",
      "Comme l'additif, mais la saisonnalité est proportionnelle au niveau.",
      "Prévoir une série dont les oscillations grandissent avec le niveau.",
      "Fréquence > 1, 2 saisons complètes, valeurs strictement positives."),
    ets    = c("ÉTS (sélection automatique)",
      "Famille des lissages exponentiels ; la meilleure combinaison Erreur/Tendance/Saison est choisie par vraisemblance (AICc).",
      "Obtenir automatiquement le meilleur lissage exponentiel sans réglage manuel.",
      "Au moins ~16 observations ; saisonnalités très longues (> 24) mal gérées (prendre TBATS)."),
    arima  = c("ARIMA automatique",
      "Modélise la série par ses propres retards (AR), la différenciation (I) et les erreurs passées (MA) ; les ordres sont choisis automatiquement.",
      "Capturer l'autocorrelation, la tendance stochastique et la saisonnalité.",
      "Série rendue stationnaire par différenciation ; résidus à vérifier (Ljung-Box) ; au moins ~30 observations."),
    sarima = c("SARIMA manuel",
      "ARIMA avec ordres saisonniers (P,D,Q) fixes par l'utilisateur.",
      "Contrôler finement la structure quand l'automatique ne convient pas.",
      "Bien lire l'ACF/PACF des résidus pour choisir les ordres ; fréquence > 1 pour la partie saisonnière."),
    tbats  = c("TBATS",
      "Combinaison de transformations Box-Cox, tendance amortie, erreurs ARMA et saisonnalités trigonométriques multiples.",
      "Séries à saisonnalités complexes ou multiples (ex. journalière + hebdomadaire).",
      "Séries suffisamment longues ; calcul plus lent que ÉTS/ARIMA."),
    theta  = c("Méthode Thêta",
      "Décompose la série en droites de courbure modifiée puis les recombine (équivalent à un SES avec dérive).",
      "Prévision robuste et rapide, très performante dans les compétitions (M3).",
      "Série de préférence désaisonnalisée ou peu saisonnière ; peu de réglages."),
    stlf   = c("STL + ÉTS",
      "Décompose la série (tendance / saison / reste) par régression locale STL puis prévoit le reste par ÉTS et rajoute la saison.",
      "Séries saisonnières au motif stable, avec robustesse aux valeurs atypiques.",
      "Fréquence > 1 et au moins 2 saisons complètes."),
    nnetar = c("NNAR (réseau de neurones autoregressif)",
      "Perceptron à une couche cachée nourri par les retards de la série (et les retards saisonniers).",
      "Capturer des dynamiques non linéaires ignorées par ARIMA/ÉTS.",
      "Séries assez longues ; pas d'intervalles de prévision analytiques ; risque de surapprentissage sur séries courtes."),
    dlmts  = c("DLM (modèle linéaire dynamique)",
      "Modèle espace d'états gaussien (niveau, tendance, saison) dont les composantes évoluent dans le temps ; estimation par maximum de vraisemblance et filtre de Kalman.",
      "Suivre des composantes qui changent au fil du temps et fournir une incertitude cohérente.",
      "Série approximativement gaussienne ; au moins ~30 observations ; l'optimisation peut échouer sur séries très courtes."),
    dlnm   = c("DLNM (retards distribues non linéaires)",
      "Régression ou la réponse dépend d'une exposition via une surface exposition-retard (splines croisées) : l'effet peut être non linéaire ET étale dans le temps.",
      "Quantifier et exploiter l'effet retardé d'une exposition (température, pollution...) sur un résultat, usage classique en épidémiologie environnementale.",
      "Choisir une variable d'exposition et un décalage maximal ; série nettement plus longue que le décalage ; famille quasi-Poisson automatique pour les comptages ; les prévisions exigent des expositions futures."),
    prophet = c("Prophet",
      "Modèle additif décomposable : tendance par morceaux + saisonnalités de Fourier + jours fériés, estime par optimisation bayésienne approchée.",
      "Séries d'activité (journalières/hebdomadaires) avec changements de tendance et effets calendaires.",
      "Nécessite une colonne de dates ; au moins plusieurs mois d'historique ; moins adapté aux séries très courtes ou hautement autocorrelees."),
    # Machine learning supervise
    lmglm = c("Modèle linéaire / logistique",
      "Combinaison linéaire des variables ; en classification, la probabilité passe par une fonction logistique.",
      "Référence interprétable : effets marginaux lisibles (coefficients).",
      "Relations approximativement linéaires ; peu de colinéarité ; résidus homoscédastiques (régression)."),
    glmnet = c("Ridge / Lasso / Elastic-Net",
      "Modèle linéaire pénalise : la pénalité rétrécit les coefficients (Ridge) ou en annule (Lasso), dosée par validation croisée.",
      "Stabiliser le modèle avec beaucoup de variables corrélées et sélectionner les plus utiles.",
      "Variables standardisées en interne ; efficace quand p est grand ou les prédicteurs corrélés."),
    rpart = c("Arbre de décision",
      "Partitionne récursivement les données par des règles de seuil maximisant la pureté des feuilles.",
      "Règles de décision lisibles et non linéaires.",
      "Sensible aux petites variations des données ; élaguer (cp) pour éviter le surapprentissage."),
    rf = c("Foret aléatoire",
      "Moyenne de centaines d'arbres construits sur des échantillons bootstrap et des sous-ensembles de variables.",
      "Excellente performance par défaut, robuste au bruit, importance des variables.",
      "Peu d'hypothèses ; coûteuse sur très gros volumes ; extrapole mal hors du domaine observe."),
    xgb = c("Gradient boosting (xgboost)",
      "Ajoute séquentiellement de petits arbres corrigeant les erreurs résiduelles des précédents.",
      "État de l'art sur données tabulaires quand il est bien règle.",
      "Sensible aux hyperparamètres (profondeur, taux d'apprentissage, itérations) ; activer la recherche automatique."),
    svm = c("SVM (machine à vecteurs de support)",
      "Cherche la frontière de marge maximale, rendue non linéaire par un noyau (radial, polynomial...).",
      "Frontières complexes sur échantillons petits à moyens.",
      "Variables à standardiser (fait en interne par e1071) ; coût élevé au-delà de ~10 000 lignes ; régler C (et le noyau)."),
    knn = c("k plus proches voisins",
      "Prédit par vote (ou moyenne) des k observations les plus proches.",
      "Méthode locale sans hypothèse de forme.",
      "Sensible à l'échelle des variables et à la dimension ; choisir k (impair en binaire) ; coûteux en prédiction sur gros volumes."),
    nb = c("Naïve Bayes",
      "Applique la règle de Bayes en supposant les variables indépendantes conditionnellement à la classe.",
      "Classifieur rapide et étonnamment efficace, notamment sur variables catégorielles.",
      "Classification uniquement ; l'hypothèse d'indépendance doit rester raisonnable."),
    nnet = c("Réseau de neurones (1 couche)",
      "Perceptron à une couche cachée : combinaisons non linéaires apprises des variables.",
      "Capturer des interactions non linéaires simples.",
      "Variables standardisées (fait en interne) ; régler taille et decay ; risque de minima locaux."),
    # Clustering
    kmeans = c("k-means",
      "Alterne affectation de chaque point au centre le plus proche et recalcul des centres.",
      "Partitionner rapidement en k groupes compacts.",
      "k fixe à l'avance ; groupes sphériques de tailles comparables ; standardiser les variables."),
    hclust = c("Classification hiérarchique (CAH)",
      "Fusionne progressivement les paires de groupes les plus proches (Ward) en un dendrogramme.",
      "Explorer la structure à plusieurs niveaux de regroupement.",
      "Matrice de distances en O(n^2) : réserver aux effectifs modérés ; standardiser."),
    pam = c("PAM (k-médoïdes)",
      "Comme k-means mais les centres sont des observations réelles (médoïdes), avec une distance quelconque.",
      "Clustering robuste aux valeurs atypiques.",
      "Plus coûteux que k-means ; k fixe à l'avance."),
    dbscan = c("DBSCAN",
      "Regroupe les points densément connectes ; les points isoles deviennent du bruit.",
      "Trouver des groupes de forme quelconque sans fixer k, et isoler les anomalies.",
      "Régler eps et minPts (sensibles) ; difficile si les densités varient beaucoup ; standardiser."),
    mclust = c("Mélanges gaussiens (mclust)",
      "Modèle probabiliste : les données proviennent d'un mélange de lois normales estimé par EM ; choix du modèle par BIC.",
      "Clustering souple (appartenance probabiliste) et formes elliptiques.",
      "Hypothèse de normalité par composante ; effectifs suffisants par groupe."),
    # Deep learning
    dl_neuralnet = c("MLP (neuralnet)",
      "Perceptron multi-couches entraîne par retropropagation résiliente (rprop), 100 % R.",
      "Réseau profond simple, disponible sans aucune installation supplémentaire.",
      "Prédicteurs standardises (fait automatiquement) ; peut ne pas converger : réduire les couches ou augmenter les itérations."),
    dl_torch = c("MLP (torch)",
      "Perceptron multi-couches (ReLU) entraîne par Adam et mini-lots via libtorch, avec courbe de perte par époque.",
      "Architectures plus profondes, contrôle fin (époques, taux d'apprentissage, lots).",
      "Bibliothèques natives à télécharger une fois (~600 Mo, bouton dédié) ; surveiller la courbe de perte (surapprentissage)."),
    lstm = c("LSTM (torch)",
      "Réseau récurrent à mémoire longue : apprend à prédire chaque valeur à partir d'une fenêtre glissante du passe.",
      "Prévision de séquences aux dépendances longues et non linéaires.",
      "Séries longues (>> fenêtre) ; prévisions futures récursives dont l'incertitude croit avec l'horizon ; torch requis.")
  )
  x <- d[[id]]
  if (is.null(x)) return(NULL)
  list(nom = x[1], principe = x[2], objectif = x[3], conditions = x[4])
}

# Bloc UI pret a l'emploi pour afficher la fiche d'un modele.
hstat_model_doc_ui <- function(id) {
  f <- hstat_model_doc(id)
  if (is.null(f)) return(NULL)
  shiny::div(class = "callout callout-info", style = "margin-top:8px;",
      shiny::tags$p(shiny::icon("book"), shiny::strong(trf(" Fiche du modèle — %s", f$nom))),
      shiny::tags$p(shiny::strong("Principe : "), f$principe),
      shiny::tags$p(shiny::strong("Objectif : "), f$objectif),
      shiny::tags$p(shiny::strong("Conditions d'application : "), f$conditions))
}

# EXPORT D'IMAGE : les pixels saisis sont une MISE EN PAGE, pas la sortie
# `ggsave` raisonne en pouces : le fichier fait pouces x DPI.

# PIXELS ET RESOLUTION : DEUX MODELES, ET IL FAUT DIRE LEQUEL
# Quand les champs de largeur/hauteur sont RECALCULES a chaque changement de
# DPI -- c'est le cas des analyses multivariees.

# Nouvelle taille en pixels apres un changement de resolution, a taille
# physique constante.
hstat_px_apres_dpi <- function(px, dpi_avant, dpi_apres, max_px = HSTAT_EXPORT_MAX_PX) {
  n <- function(x) suppressWarnings(as.numeric(x)[1])
  px <- n(px); a <- n(dpi_avant); b <- n(dpi_apres)
  if (!isTRUE(is.finite(px)) || px <= 0) return(NULL)
  if (!isTRUE(is.finite(a)) || a <= 0 || !isTRUE(is.finite(b)) || b <= 0) return(NULL)
  round(min(px * b / a, max_px))
}

# Pouces correspondant a une taille en pixels rendue a une resolution donnee.
hstat_px_en_pouces <- function(px, dpi, defaut = 8, max_in = 200) {
  n <- function(x) suppressWarnings(as.numeric(x)[1])
  px <- n(px); d <- n(dpi)
  if (!isTRUE(is.finite(px)) || px <= 0 ||
      !isTRUE(is.finite(d))  || d  <= 0) return(defaut)
  max(1, min(px / d, max_in))
}

# Pixels a produire pour une taille physique et une resolution donnees.
hstat_px_pour_dpi <- function(pouces, dpi, max_px = HSTAT_EXPORT_MAX_PX) {
  n <- function(x) suppressWarnings(as.numeric(x)[1])
  po <- n(pouces); d <- n(dpi)
  if (!isTRUE(is.finite(po)) || po <= 0) return(NULL)
  if (!isTRUE(is.finite(d))  || d  <= 0) return(NULL)
  round(min(po * d, max_px))
}

# Emplacement de la note « ce que le fichier contiendra », posee sous les trois
# champs d'export.
hstat_mv_dim_note_ui <- function(prefix) {
  shiny::uiOutput(paste0(prefix, "_dimnote"))
}

HSTAT_EXPORT_REF_DPI <- 96

# Borne de securite : au-dela, ggsave tente d'allouer un bitmap que la machine
# ne peut pas tenir et l'export echoue au lieu de rendre une image un peu moins
# fine.
HSTAT_EXPORT_MAX_PX <- 20000L

# Plafond propre a l'export de l'onglet Visualisation, ou la taille physique
# est fixe et seul le DPI varie.
HSTAT_DPI_MAX <- 20000L

# Cote maximal, en pixels, d'une image MATRICIELLE.
HSTAT_RASTER_MAX_PX <- 20000L

# Resolution reellement applicable a une taille physique DONNEE.
hstat_dpi_effectif <- function(w_in, h_in, dpi, max_px = HSTAT_RASTER_MAX_PX) {
  n1 <- function(x, d) { x <- suppressWarnings(as.numeric(x)[1])
                         if (length(x) == 0 || is.na(x) || x <= 0) d else x }
  w <- n1(w_in, 10); h <- n1(h_in, 7)
  dem <- max(72, min(HSTAT_DPI_MAX, n1(dpi, 300)))
  cote <- max(w, h)
  eff <- if (cote * dem > max_px) max(72, floor(max_px / cote)) else dem
  list(dpi = eff, demande = dem, plafonne = eff < dem,
       note = if (eff < dem)
         trf("Résolution ramenée de %s à %s DPI : au-delà, l'image dépasse %s pixels de côté et l'export échoue. La taille de la figure, elle, est inchangée. Pour une finesse illimitée, choisissez SVG ou PDF.",
             round(dem), round(eff), format(max_px, big.mark = " ")) else NULL)
}

# Dimensions d'export de l'onglet Visualisation, EN UN SEUL ENDROIT.
hstat_viz_export_dims <- function(dpi, base_w = 12, base_h = 8,
                                  max_px = HSTAT_RASTER_MAX_PX) {
  d <- suppressWarnings(as.numeric(dpi)[1])
  if (!isTRUE(is.finite(d)) || d <= 0) d <- 300
  eff <- hstat_dpi_effectif(base_w, base_h, d, max_px)
  list(dpi = eff$dpi, width = base_w, height = base_h,
       px_w = round(base_w * eff$dpi), px_h = round(base_h * eff$dpi),
       plafonne = eff$plafonne, note = eff$note)
}

hstat_export_dims <- function(width_px, height_px, dpi,
                              ref = HSTAT_EXPORT_REF_DPI,
                              max_in = 200, max_px = HSTAT_EXPORT_MAX_PX) {
  num1 <- function(x, defaut) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (length(x) == 0 || is.na(x) || x <= 0) defaut else x
  }
  width_px  <- num1(width_px, 1200)
  height_px <- num1(height_px, 800)
  dpi_dem   <- max(72, num1(dpi, 300))

  w_in <- max(1, min(width_px  / ref, max_in))
  h_in <- max(1, min(height_px / ref, max_in))

  dpi_eff <- dpi_dem
  cote <- max(w_in, h_in)
  if (cote * dpi_eff > max_px) dpi_eff <- max(72, floor(max_px / cote))

  note <- if (dpi_eff < dpi_dem)
    trf("Résolution ramenée de %s à %s DPI : au-delà, l'image dépasse %s pixels de côté et l'export échoue. Passez au format SVG ou PDF, dont la résolution est illimitée.",
        round(dpi_dem), round(dpi_eff), format(max_px, big.mark = " ")) else NULL

  list(width_in = w_in, height_in = h_in, dpi = dpi_eff,
       width_out = round(w_in * dpi_eff), height_out = round(h_in * dpi_eff),
       note = note)
}

# Etiquette mise en forme pour plotly.
hstat_etiquettes_x_style <- function(etiquettes, styles = "plain") {
  etiquettes <- as.character(etiquettes)
  if (!length(etiquettes)) return(list())
  styles <- rep_len(as.character(styles %||% "plain"), length(etiquettes))
  styles[is.na(styles)] <- "plain"
  lapply(seq_along(etiquettes), function(i) {
    lab <- etiquettes[i]
    switch(styles[i],
           "bold"        = bquote(bold(.(lab))),
           "italic"      = bquote(italic(.(lab))),
           "bolditalic"  = bquote(bolditalic(.(lab))),
           "bold.italic" = bquote(bolditalic(.(lab))),
           lab)
  })
}

hstat_html_style_label <- function(x, style = "plain") {
  x <- as.character(x)
  style <- rep_len(as.character(style %||% "plain"), length(x))
  style[is.na(style)] <- "plain"
  vapply(seq_along(x), function(i) {
    s <- hstat_html_escape(x[i])
    switch(style[i],
           "bold"       = paste0("<b>", s, "</b>"),
           "italic"     = paste0("<i>", s, "</i>"),
           "bolditalic" = paste0("<b><i>", s, "</i></b>"),
           "bold.italic" = paste0("<b><i>", s, "</i></b>"),
           s)
  }, character(1))
}

# LE RANG, PAS LE DETERMINANT -- et il faut NOMMER ce qui est redondant.
hstat_colineaires <- function(data, tol = 1e-7) {
  vide <- list(rang = 0L, p = 0L, redondantes = character(0))
  if (is.null(data)) return(vide)
  X <- as.data.frame(data)
  X <- X[, vapply(X, is.numeric, logical(1)), drop = FALSE]
  if (!NCOL(X) || !NROW(X)) return(vide)
  X <- X[stats::complete.cases(X), , drop = FALSE]
  if (NROW(X) < 2L) return(list(rang = 0L, p = NCOL(X), redondantes = names(X)))
  # Une colonne d'ecart-type nul n'est pas « redondante », elle est vide de
  # variation : `scale()` y rendrait des NaN et le rang deviendrait NA.
  et <- vapply(X, function(x) stats::sd(x), numeric(1))
  cst <- names(X)[!is.finite(et) | et <= 0]
  Xv <- X[, setdiff(names(X), cst), drop = FALSE]
  if (!NCOL(Xv)) return(list(rang = 0L, p = NCOL(X), redondantes = names(X)))
  Z <- scale(Xv)
  q <- tryCatch(qr(Z, tol = tol), error = function(e) NULL)
  if (is.null(q)) return(list(rang = NA_integer_, p = NCOL(X), redondantes = cst))
  rang <- q$rank
  sup <- if (rang < NCOL(Xv)) colnames(Xv)[q$pivot[seq.int(rang + 1L, NCOL(Xv))]]
         else character(0)
  list(rang = as.integer(rang), p = NCOL(X),
       redondantes = unique(c(cst, sup)))
}

# UNE SELECTION SURVIT AU JEU DE DONNEES QU'ELLE DESIGNE.
hstat_cols_absentes <- function(data, cols) {
  if (is.null(data) || is.null(cols)) return(character(0))
  cols <- as.character(cols)
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  if (!length(cols)) return(character(0))
  setdiff(cols, names(data))
}

# Le pendant a l'usage : vrai quand TOUTES les colonnes demandees sont la.
hstat_cols_pretes <- function(data, cols) {
  length(hstat_cols_absentes(data, cols)) == 0L
}

# Une agregation numerique n'a de sens que sur une variable numerique.
hstat_y_agregeable <- function(data, y_var) {
  if (is.null(data) || is.null(y_var)) return(FALSE)
  y_var <- as.character(y_var)[1]
  if (is.na(y_var) || !nzchar(y_var) || !y_var %in% names(data)) return(FALSE)
  x <- data[[y_var]]
  is.numeric(x) || is.logical(x) || inherits(x, c("Date", "POSIXct", "POSIXlt"))
}

# Renommer les modalites AFFICHEES sur l'axe X, sans toucher aux donnees.
hstat_etiquettes_x <- function(niveaux, etiquettes = NULL) {
  niveaux <- as.character(niveaux)
  out <- niveaux
  names(out) <- niveaux
  if (is.null(etiquettes) || !length(etiquettes) || is.null(names(etiquettes)))
    return(out)
  et <- as.character(etiquettes)
  names(et) <- names(etiquettes)
  garde <- !is.na(names(et)) & nzchar(names(et)) & !is.na(et) & nzchar(et)
  et <- et[garde]
  com <- intersect(niveaux, names(et))
  if (!length(com)) return(out)
  out[com] <- unname(et[com])
  dup <- unique(out[duplicated(out)])
  if (length(dup)) {
    fautifs <- names(out)[out %in% dup]
    out[fautifs] <- fautifs
    attr(out, "collisions") <- sort(dup)
  }
  out
}

# Variables Y retenues pour un graphique multi-courbes.
hstat_y_multi_valides <- function(data, y_vars, x_var = NULL) {
  vide <- list(gardees = character(0), ecartees = character(0), motif = NULL)
  if (is.null(data) || !length(y_vars)) return(vide)
  cand <- unique(as.character(y_vars))
  cand <- cand[cand %in% names(data)]
  if (!length(cand)) return(vide)

  ecart_x <- intersect(cand, as.character(x_var %||% character(0)))
  cand    <- setdiff(cand, ecart_x)
  if (!length(cand))
    return(list(gardees = character(0), ecartees = ecart_x,
                motif = "axe X"))

  num <- cand[vapply(cand, function(v) is.numeric(data[[v]]), logical(1))]
  if (length(num) && length(num) < length(cand))
    return(list(gardees = num, ecartees = c(ecart_x, setdiff(cand, num)),
                motif = "type non quantitatif"))
  if (!length(num)) {
    cl   <- vapply(cand, function(v) class(data[[v]])[1], character(1))
    gard <- cand[cl == cl[1]]
    if (length(gard) < length(cand))
      return(list(gardees = gard, ecartees = c(ecart_x, setdiff(cand, gard)),
                  motif = "type incompatible"))
  }
  list(gardees = cand, ecartees = ecart_x,
       motif = if (length(ecart_x)) "axe X" else NULL)
}

# Replis des paquets d'interface optionnels
# DEFINIR ces replis appartient au paquet.
layout <- function(p, ...) {
  if (isTRUE(requireNamespace("plotly", quietly = TRUE))) plotly::layout(p, ...) else p
}

# Dependances web declarees mais introuvables
# `shiny::sliderInput()` attache la dependance « strftime » en annoncant
# `strftime-min.js` -- alors que le paquet livre `strftime.min.js`.
.hstat_nouislider_casse <- function(f = system.file("htmlwidgets/lib/nouislider/jquery.nouislider.min.js",
                                                   package = "DT")) {
  if (!nzchar(f) || !file.exists(f)) return(FALSE)
  txt <- tryCatch(readChar(f, file.info(f)$size, useBytes = TRUE), error = function(e) "")
  nzchar(txt) && !grepl("fn.noUiSlider", txt, fixed = TRUE, useBytes = TRUE)
}

.hstat_nouislider_dep <- function() {
  htmltools::htmlDependency("nouislider", "7.0.10", src = c(href = "lib/nouislider"),
                            script = "jquery.nouislider.min.js",
                            stylesheet = "jquery.nouislider.min.css")
}

hstat_reparer_deps <- function(ui, nouislider_casse = .hstat_nouislider_casse()) {
  if (!requireNamespace("htmltools", quietly = TRUE)) return(ui)
  if (isTRUE(nouislider_casse))
    ui <- htmltools::attachDependencies(ui, list(.hstat_nouislider_dep()), append = TRUE)
  deps <- tryCatch(htmltools::findDependencies(ui), error = function(e) NULL)
  if (!length(deps)) return(ui)
  corrigees <- list()
  for (d in deps) {
    rep <- d$src$file
    if (is.null(rep) || !length(d$script)) next
    base <- if (!is.null(d$package)) system.file(rep, package = d$package) else rep
    if (!nzchar(base) || !dir.exists(base)) next
    js <- unlist(d$script)
    if (all(file.exists(file.path(base, js)))) next
    # Le fichier annonce est absent : on cherche le SEUL .js du dossier.
    reels <- list.files(base, pattern = "[.]js$")
    if (length(reels) != 1L) next
    d$script <- reels
    d$version <- paste0(d$version, ".1")   # gagne a la resolution
    corrigees[[length(corrigees) + 1L]] <- d
  }
  if (!length(corrigees)) return(ui)
  htmltools::attachDependencies(ui, corrigees, append = TRUE)
}

# EN-TETES DE SECURITE DE LA PAGE, ce que l'interface permet sans se casser.
HSTAT_ENTETES_SECURITE <- list(
  `Content-Security-Policy` = "object-src 'none'; base-uri 'self'; form-action 'self'",
  `X-Content-Type-Options`  = "nosniff",
  `Referrer-Policy`         = "strict-origin-when-cross-origin")

# La page se serialise a la demande, une fois, et ses erreurs se levent
# L'interface est un objet construit une fois au demarrage, et elle rend le
# meme HTML a chaque requete.
hstat_ui_en_cache <- function(ui, prechauffer = TRUE) {
  if (is.function(ui)) return(ui)
  rendre <- tryCatch(get("renderPage", envir = asNamespace("shiny")),
                     error = function(e) NULL)
  if (!is.function(rendre)) return(ui)
  cache <- new.env(parent = emptyenv())
  cle_courante <- function()
    if (isTRUE(shiny::getShinyOption("testmode", default = FALSE))) "test" else "normal"
  serialiser <- function(cle) {
    page <- cache[[cle]]
    if (!is.null(page)) return(page)
    html <- tryCatch(
      rendre(ui, 0, identical(cle, "test")),
      error = function(e) {
        msg <- trf("HStat : sérialisation de l'interface impossible : %s",
                   conditionMessage(e))
        message(msg)
        stop(structure(class = c("hstat_serialisation", "error", "condition"),
                       list(message = msg, call = NULL, parent = e)))
      })
    page <- list(html = html,
                 etag = paste0('"', rlang::hash(html), '"'))
    assign(cle, page, envir = cache)
    page
  }
  if (isTRUE(prechauffer) && requireNamespace("later", quietly = TRUE))
    later::later(function() tryCatch(serialiser("normal"), error = function(e) NULL))
  f <- function(req) {
    page <- serialiser(cle_courante())
    entetes <- c(list(ETag = page$etag, `Cache-Control` = "no-cache"),
                 HSTAT_ENTETES_SECURITE)
    if (identical(req$HTTP_IF_NONE_MATCH, page$etag))
      return(shiny::httpResponse(304L, content = "", headers = entetes))
    shiny::httpResponse(200L, content = page$html, headers = entetes)
  }
  attr(f, "hstat_serialiser") <- serialiser
  f
}

# La premiere session ne paie plus la compilation de toute l'application
HSTAT_PRECHAUFFAGE_ECART <- 1

hstat_prechauffer_session <- function(server, differer = TRUE) {
  if (!is.function(server)) return(server)
  courant <- server
  # Une VRAIE session ouverte avant la session fictive rend celle-ci inutile.
  en_fictive <- FALSE
  vraie_session <- FALSE
  enveloppe <- function(input, output, session) {
    if (!en_fictive) vraie_session <<- TRUE
    courant(input = input, output = output, session = session)
  }
  compiler <- function() {
    cf <- tryCatch(compiler::cmpfun(server), error = function(e) {
      message(trf("HStat : compilation du serveur impossible : %s", conditionMessage(e)))
      NULL
    })
    if (is.function(cf)) courant <<- cf
  }
  session_fictive <- function() {
    # Les gestionnaires vivent DANS le rappel.
    if (vraie_session) return(invisible(FALSE))
    t0 <- proc.time()[["elapsed"]]
    en_fictive <<- TRUE
    on.exit(en_fictive <<- FALSE, add = TRUE)
    ok <- tryCatch({
      suppressWarnings(suppressMessages(shiny::testServer(enveloppe, {})))
      TRUE
    }, error = function(e) {
      message(trf("HStat : préchauffage de la session impossible : %s",
                  conditionMessage(e)))
      FALSE
    })
    if (ok) message(trf("HStat : session préchauffée en %.1f s.",
                        proc.time()[["elapsed"]] - t0))
  }
  attr(enveloppe, "hstat_etapes") <- list(compiler = compiler, session = session_fictive)
  if (isFALSE(getOption("hstat.prechauffage", TRUE)) || !isTRUE(differer) ||
      !requireNamespace("later", quietly = TRUE)) return(enveloppe)
  # UN DELAI ENTRE LES ETAPES, PAS ZERO.
  later::later(function() {
    compiler()
    later::later(session_fictive, delay = HSTAT_PRECHAUFFAGE_ECART)
  }, delay = HSTAT_PRECHAUFFAGE_ECART)
  enveloppe
}

hstat_installer_replis_ui <- function(envir = globalenv()) {
  has <- function(p) isTRUE(requireNamespace(p, quietly = TRUE))
  # UN AIGUILLAGE, PAS UN REPLI CONDITIONNEL.
  poser <- function(nom, valeur) assign(nom, valeur, envir = envir)

  # -- shinycssloaders ---------------------------------------------------------
  poser("withSpinner", if (has("shinycssloaders")) shinycssloaders::withSpinner
        else function(ui_element, ...) ui_element)

  # -- plotly ------------------------------------------------------------------
  poser("plotlyOutput", if (has("plotly")) plotly::plotlyOutput
        else function(outputId, width = "100%", height = "400px", ...)
          shiny::plotOutput(outputId, width = width, height = height))
  poser("ggplotly", if (has("plotly")) plotly::ggplotly else function(p, ...) p)
  # `layout` et `config` : aucun usage en graphisme de base dans l'application
  # (verifie), ils ne peuvent donc pas masquer `graphics::layout`.
  poser("layout", layout)
  poser("config", if (has("plotly")) plotly::config else function(p, ...) p)

  # Le nettoyage du polyfill obsolete est pose sur `renderPlotly` LUI-MEME
  # plutot qu'a chacun des appels.
  poser("renderPlotly", if (has("plotly"))
    function(expr, env = parent.frame(), quoted = FALSE) {
      fn <- shiny::exprToFunction(expr, env, quoted)
      rf <- plotly::renderPlotly(hstat_plotly_clean(fn()),
                                 env = environment(), quoted = FALSE)
      # La serialisation refait `plotly_build()`, donc reverifie la mise en
      # page.
      w <- function(...) withCallingHandlers(rf(...), warning = function(x) {
        if (grepl(HSTAT_PLOTLY_LAYOUT_TOLERES, conditionMessage(x)))
          invokeRestart("muffleWarning")
      })
      attributes(w) <- attributes(rf)
      w
    } else function(expr, ...) {
      q <- substitute(expr); pf <- parent.frame()
      shiny::renderPlot({
        val <- eval(q, envir = pf)
        if (inherits(val, c("ggplot", "gg"))) print(val)
        else if (inherits(val, "grob")) grid::grid.draw(val)
        else val
      })
    })

  # Ne PAS ecraser un vrai pipe : un repli naif `rhs(lhs)` perdrait les
  # arguments (`p %>% layout(x = 1)` deviendrait `layout(p)`).
  if (!exists("%>%", envir = envir, inherits = TRUE))
    poser("%>%", if (has("magrittr")) magrittr::`%>%` else function(lhs, rhs) {
      rc <- substitute(rhs)
      if (is.call(rc)) {
        nouveau <- as.call(c(rc[[1]], substitute(lhs), as.list(rc)[-1]))
        eval(nouveau, parent.frame())
      } else rhs(lhs)
    })

  # -- ggtext
  poser("element_markdown", if (has("ggtext")) ggtext::element_markdown
        else function(family = NULL, face = NULL, size = NULL, colour = NULL,
                      fill = NULL, box.colour = NULL, linetype = NULL,
                      linewidth = NULL, hjust = NULL, vjust = NULL,
                      halign = NULL, valign = NULL, angle = NULL,
                      lineheight = NULL, margin = NULL, color = NULL, ...) {
          # `halign`/`valign` alignent les LIGNES entre elles chez gridtext ;
          # `element_text` n'a que `hjust`/`vjust`, qui deplacent le bloc
          # entier.
          if (is.null(hjust) && !is.null(halign)) hjust <- halign
          if (is.null(vjust) && !is.null(valign)) vjust <- valign
          ggplot2::element_text(family = family, face = face, size = size,
                                colour = colour %||% color, hjust = hjust,
                                vjust = vjust, angle = angle,
                                lineheight = lineheight, margin = margin)
        })

  # -- colourpicker ------------------------------------------------------------
  poser("colourInput", if (has("colourpicker")) colourpicker::colourInput
        else function(inputId, label, value = "#000000", ...)
          shiny::textInput(inputId, label, value = value))
  # LE METTEUR A JOUR VA AVEC LE CHAMP.
  poser("updateColourInput",
        if (has("colourpicker")) colourpicker::updateColourInput
        else function(session, inputId, label = NULL, value = NULL, ...)
          shiny::updateTextInput(session, inputId, label = label, value = value))

  # -- shinyWidgets ------------------------------------------------------------
  poser("pickerInput", if (has("shinyWidgets")) shinyWidgets::pickerInput
        else function(inputId, label = NULL, choices, selected = NULL,
                      multiple = FALSE, options = NULL, choicesOpt = NULL, ...)
          shiny::selectInput(inputId, label, choices = choices,
                             selected = selected, multiple = multiple))
  poser("radioGroupButtons", if (has("shinyWidgets")) shinyWidgets::radioGroupButtons
        else function(inputId, label = NULL, choices = NULL, selected = NULL, ...)
          shiny::radioButtons(inputId, label, choices = choices,
                              selected = selected %||% NULL, inline = TRUE))
  poser("updatePickerInput", if (has("shinyWidgets")) shinyWidgets::updatePickerInput
        else function(session, inputId, ..., choices = NULL, selected = NULL)
          shiny::updateSelectInput(session, inputId, choices = choices,
                                   selected = selected))

  # -- sortable ----------------------------------------------------------------
  poser("rank_list", if (has("sortable")) sortable::rank_list
        else function(text = NULL, labels = NULL, input_id, ...)
          shiny::selectInput(input_id, label = text, choices = labels,
                             selected = labels, multiple = TRUE))
  invisible(TRUE)
}

# DIVERSITE ECOLOGIQUE : matrice sites x especes, indices alpha et beta
# Toutes les statistiques vivent ICI et non dans le module : c'est la regle du
# depot, et elle a une raison pratique.

# -- LA BASE DU LOGARITHME N'EST PAS UN DETAIL D'AFFICHAGE
# `vegan::diversity()` calcule en logarithme NATUREL.
HSTAT_DIV_BASES <- c("bits (log2)" = "2", "nats (ln)" = "e", "décibels (log10)" = "10")

.hstat_div_log <- function(x, base = "2") {
  b <- as.character(base %||% "2")[1]
  if (identical(b, "e")) log(x) else log(x, base = as.numeric(b))
}

# Conversion d'une entropie d'une base a l'autre : H_b2 = H_b1 * ln(b1)/ln(b2).
hstat_div_convertir <- function(h, de = "2", vers = "2") {
  lb <- function(b) if (identical(as.character(b), "e")) 1 else log(as.numeric(b))
  h * lb(de) / lb(vers)
}

# -- Matrice sites x especes.
.hstat_div_cle <- function(data, cols) {
  parts <- lapply(cols, function(k) {
    v <- data[[k]]
    # Une date doit entrer par son ECRITURE, pas par son nombre de jours depuis
    # 1970.
    if (inherits(v, c("Date", "POSIXct", "POSIXlt"))) format(v) else as.character(v)
  })
  parts <- lapply(parts, function(x) { x[is.na(x)] <- ""; trimws(x) })
  trimws(do.call(paste, c(parts, sep = " | ")))
}

# LES STADES D'UNE MEME ESPECE NE SONT PAS DEUX ESPECES
# Un fichier de comptages entomologiques porte le stade en prefixe.
hstat_div_stades <- function(cols) {
  cols <- as.character(cols)
  # `[A-Za-z]+` ne peut pas franchir le souligne.
  mm <- regmatches(cols, regexec("^([A-Za-z]+)_(.+)$", cols))
  stade  <- vapply(mm, function(x) if (length(x) == 3L) x[2] else NA_character_, character(1))
  espece <- vapply(mm, function(x) if (length(x) == 3L) x[3] else NA_character_, character(1))
  # Sans prefixe, la colonne EST l'espece.
  sans <- is.na(espece)
  espece[sans] <- cols[sans]
  multi <- character(0)
  for (e in unique(espece[!sans])) {
    k <- which(espece == e & !sans)
    if (length(unique(stade[k])) >= 2L) multi <- c(multi, e)
  }
  # Ce qui n'a pas au moins deux stades garde son nom de colonne.
  final <- cols
  keep <- espece %in% multi
  final[keep] <- espece[keep]
  list(colonne = cols, stade = stade, espece = final,
       multi = multi, n_multi = length(multi))
}

# -- Bornes de la matrice releves x especes ---------------------------------
# La matrice est DENSE.
HSTAT_DIV_CELLULES_MAX <- 2e6

# Les analyses PAR PAIRE (diversite beta, partition de Baselga) rendent n x n
# valeurs, ou n (n - 1) / 2 lignes.
HSTAT_DIV_RELEVES_MAX <- 1000L

.hstat_div_releves_max <- function(n) {
  if (n > HSTAT_DIV_RELEVES_MAX)
    stop(hstat_refus(trf("%d relevés : les comparaisons par paire sont limitées à %d relevés (%s paires). Regroupez les relevés (par traitement ou par date) pour comparer des unités moins nombreuses.",
                         as.integer(n), HSTAT_DIV_RELEVES_MAX,
                         format(HSTAT_DIV_RELEVES_MAX * (HSTAT_DIV_RELEVES_MAX - 1) / 2,
                                big.mark = " ", scientific = FALSE))))
  invisible(TRUE)
}

# Le meme motif, rendu comme texte.
.hstat_div_trop_de_releves <- function(n)
  tryCatch({ .hstat_div_releves_max(n); "" }, error = function(e) conditionMessage(e))

.hstat_div_cellules_max <- function(n_releves, n_especes) {
  if (as.numeric(n_releves) * as.numeric(n_especes) > HSTAT_DIV_CELLULES_MAX)
    stop(hstat_refus(trf("Le tableau relevés × espèces compterait %d relevés et %d espèces, soit plus de %s cases. Une colonne qui porte une valeur différente à chaque ligne a sans doute été choisie comme relevé ou comme espèce : vérifiez ces deux colonnes.",
                         as.integer(n_releves), as.integer(n_especes),
                         format(HSTAT_DIV_CELLULES_MAX, big.mark = " ", scientific = FALSE))))
  invisible(TRUE)
}

hstat_div_matrice <- function(data, format = c("long", "large"),
                              var_site = NULL, var_espece = NULL,
                              var_abondance = NULL, var_especes = NULL,
                              grouper_stades = FALSE) {
  format <- match.arg(format)
  if (!is.data.frame(data) || !nrow(data))
    stop("Aucune donnée à agréger.", call. = FALSE)

  msg <- character(0)
  if (identical(format, "large")) {
    cols <- intersect(as.character(var_especes %||% character(0)), names(data))
    if (length(cols) < 1L)
      stop("Sélectionnez au moins une colonne d'espèce.", call. = FALSE)
    m <- as.matrix(data[, cols, drop = FALSE])
    storage.mode(m) <- "double"
    sites <- intersect(as.character(var_site %||% character(0)), names(data))
    lignes <- if (length(sites)) .hstat_div_cle(data, sites)
              else paste0("Relevé ", seq_len(nrow(data)))
    # Deux lignes de meme nom de releve sont AGREGEES, pas empilees.
    if (anyDuplicated(lignes)) {
      m <- rowsum(m, group = lignes, reorder = FALSE)
      msg <- c(msg, trf("%d relevé(s) apparaissaient sur plusieurs lignes : leurs effectifs ont été additionnés.",
                        sum(duplicated(lignes))))
      lignes <- rownames(m)
    }
    rownames(m) <- lignes
  } else {
    if (is.null(var_espece) || !var_espece %in% names(data))
      stop("Sélectionnez la colonne des espèces.", call. = FALSE)
    esp <- as.character(data[[var_espece]])
    sites <- intersect(as.character(var_site %||% character(0)), names(data))
    sit <- if (length(sites)) .hstat_div_cle(data, sites) else rep("Ensemble", nrow(data))
    if (is.null(var_abondance) || !nzchar(var_abondance %||% "") ||
        !var_abondance %in% names(data)) {
      # CHAQUE LIGNE EST UN INDIVIDU.
      ab <- rep(1, length(esp))
      msg <- c(msg, tr("Aucune colonne d'effectif : chaque ligne est comptée pour un individu."))
    } else {
      ab <- hstat_as_numeric_fr(data[[var_abondance]])
      if (is.null(ab)) ab <- suppressWarnings(as.numeric(data[[var_abondance]]))
    }
    ok <- !is.na(esp) & nzchar(esp) & !is.na(sit) & !is.na(ab)
    if (sum(!ok))
      msg <- c(msg, trf("%d ligne(s) écartée(s) : espèce, relevé ou effectif manquant.", sum(!ok)))
    if (!any(ok)) stop("Aucune ligne exploitable après nettoyage.", call. = FALSE)
    neg <- ok & ab < 0
    if (any(neg)) {
      msg <- c(msg, trf("%d effectif(s) négatif(s) écarté(s) : un effectif ne peut pas l'être.", sum(neg)))
      ok <- ok & !neg
    }
    .hstat_div_cellules_max(length(unique(sit[ok])), length(unique(esp[ok])))
    m <- tapply(ab[ok], list(sit[ok], esp[ok]), sum)
    m[is.na(m)] <- 0
    m <- as.matrix(m)
  }

  m[!is.finite(m)] <- 0

  # LE REGROUPEMENT SE FAIT ICI, sur la matrice montee, et non dans chacune des
  # deux branches.
  st <- hstat_div_stades(colnames(m))
  if (st$n_multi) {
    if (isTRUE(grouper_stades)) {
      m <- t(rowsum(t(m), group = st$espece, reorder = FALSE))
      msg <- c(msg, trf("Stades regroupés : %d espèce(s) observée(s) à plusieurs stades ont été additionnées (%s).",
                        st$n_multi, paste(st$multi, collapse = ", ")))
    } else {
      # UNE RICHESSE GONFLEE SANS UN MOT est le defaut qu'on corrige : le cas
      # est nomme meme quand on ne regroupe pas.
      msg <- c(msg, trf("Attention : %d espèce(s) apparaissent à plusieurs stades (%s) et comptent ici pour autant d'espèces distinctes, ce qui gonfle la richesse. Cochez « Regrouper les stades » pour les additionner.",
                        st$n_multi, paste(st$multi, collapse = ", ")))
    }
  }
  # Une espece absente de TOUS les releves n'apporte rien et fausse la richesse
  # si elle reste en colonne.
  vides_esp <- colSums(m) <= 0
  if (any(vides_esp)) {
    msg <- c(msg, trf("%d espèce(s) absente(s) de tous les relevés, écartée(s).", sum(vides_esp)))
    m <- m[, !vides_esp, drop = FALSE]
  }
  vides_sit <- rowSums(m) <= 0
  if (any(vides_sit)) {
    msg <- c(msg, trf("%d relevé(s) sans aucun individu, écarté(s).", sum(vides_sit)))
    m <- m[!vides_sit, , drop = FALSE]
  }
  if (!nrow(m) || !ncol(m))
    stop("La matrice est vide après nettoyage.", call. = FALSE)
  attr(m, "message") <- if (length(msg)) paste(msg, collapse = " ") else NULL
  m
}

# -- Estimateurs de richesse --------------------------------------------------
# Un inventaire ne voit jamais toutes les especes presentes.
hstat_div_richesse <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x) & x > 0]
  S <- length(x); N <- sum(x)
  ent <- abs(x - round(x)) < 1e-9
  # LES ESTIMATEURS COMPTENT DES INDIVIDUS, PAS DES BIOMASSES.
  entier <- all(ent)
  F1 <- if (entier) sum(x == 1) else NA_real_
  F2 <- if (entier) sum(x == 2) else NA_real_

  chao1 <- chao1_bc <- chao1_var <- chao1_inf <- chao1_sup <- NA_real_
  ace <- jack1 <- jack2 <- boot <- couverture <- NA_real_
  if (entier && S > 0) {
    # Chao (1984) : S + F1^2 / (2 F2).
    chao1    <- if (F2 > 0) S + F1^2 / (2 * F2) else NA_real_
    chao1_bc <- S + F1 * (F1 - 1) / (2 * (F2 + 1))
    if (F2 > 0) {
      r <- F1 / F2
      chao1_var <- F2 * (r^4 / 4 + r^3 + r^2 / 2)
    } else if (F1 > 0) {
      chao1_var <- F1 * (F1 - 1) / 2 + F1 * (2 * F1 - 1)^2 / 4 - F1^4 / (4 * chao1_bc)
    }
    # Intervalle LOG-NORMAL (Chao 1987) et non symetrique.
    est <- if (is.finite(chao1)) chao1 else chao1_bc
    if (is.finite(chao1_var) && chao1_var > 0 && est > S) {
      K <- exp(1.96 * sqrt(log(1 + chao1_var / (est - S)^2)))
      chao1_inf <- S + (est - S) / K
      chao1_sup <- S + (est - S) * K
    } else if (is.finite(est)) {
      chao1_inf <- chao1_sup <- est
    }

    # Couverture de Good (1953) : part des individus appartenant a des especes
    # deja vues plus d'une fois.
    if (N > 0) couverture <- 1 - F1 / N

    # ACE -- Chao & Lee (1992).
    rares <- x[x <= 10]; abondantes <- x[x > 10]
    S_rare <- length(rares); S_abond <- length(abondantes)
    N_rare <- sum(rares)
    if (S_rare > 0 && N_rare > 0 && F1 < N_rare) {
      C_ace <- 1 - F1 / N_rare
      somme <- sum(vapply(1:10, function(i) i * (i - 1) * sum(x == i), numeric(1)))
      g2 <- max(0, (S_rare / C_ace) * somme / (N_rare * (N_rare - 1)) - 1)
      ace <- S_abond + S_rare / C_ace + F1 / C_ace * g2
    }

    # Jackknife d'ordre 1 et 2 -- Burnham & Overton (1978, 1979).
    jack1 <- S + F1 * (N - 1) / N
    if (N > 1) jack2 <- S + F1 * (2 * N - 3) / N - F2 * (N - 2)^2 / (N * (N - 1))
    # Bootstrap -- Smith & van Belle (1984).
    p <- x / N
    boot <- S + sum((1 - p)^N)
  }

  data.frame(
    Individus = N, Sobs = S,
    Singletons = F1, Doubletons = F2,
    Chao1 = chao1, Chao1_corrige = chao1_bc,
    Chao1_IC_inf = chao1_inf, Chao1_IC_sup = chao1_sup,
    ACE = ace, Jackknife1 = jack1, Jackknife2 = jack2, Bootstrap = boot,
    Couverture_Good = couverture,
    Completude = if (is.finite(chao1_bc) && chao1_bc > 0) S / chao1_bc else NA_real_,
    stringsAsFactors = FALSE)
}

# -- Indices de diversite et d'equitabilite, releve par releve.
hstat_div_indices <- function(x, base = "2") {
  x <- as.numeric(x); x <- x[is.finite(x) & x > 0]
  S <- length(x); N <- sum(x)
  vide <- data.frame(Individus = N, Richesse_S = S)
  if (S == 0 || N <= 0) {
    for (k in c("Shannon_H", "Shannon_Hmax", "Pielou_J", "Simpson_D",
                "Simpson_1_D", "Simpson_inverse", "Simpson_E", "Heip_E",
                "Sheldon_E", "Camargo_E", "Smith_Wilson_Evar", "Brillouin_HB",
                "Brillouin_E", "McIntosh_U", "McIntosh_D", "McIntosh_E",
                "Berger_Parker_d", "Berger_Parker_inverse", "Margalef_DMg",
                "Menhinick_DMn", "Fisher_alpha", "Hill_N0", "Hill_N1",
                "Hill_N2", "Hill_Ninf"))
      vide[[k]] <- NA_real_
    return(vide)
  }
  p <- x / N

  H     <- -sum(p * .hstat_div_log(p, base))
  Hmax  <- .hstat_div_log(S, base)
  # UNE SEULE ESPECE : H' = 0 ET Hmax = 0, donc J = 0/0.
  J     <- if (S > 1) H / Hmax else NA_real_

  D     <- sum(p^2)                       # Simpson (1949), concentration
  D1    <- 1 - D                          # diversite de Gini-Simpson
  Dinv  <- if (D > 0) 1 / D else NA_real_ # Simpson inverse = Hill N2
  E_D   <- if (S > 1 && D > 0) Dinv / S else NA_real_

  # Heip (1974) : equitabilite bornee a 0 pour une seule espece dominante, et
  # moins sensible a la richesse que J.
  Hnat  <- hstat_div_convertir(H, base, "e")
  E_hp  <- if (S > 1) (exp(Hnat) - 1) / (S - 1) else NA_real_
  E_sh  <- exp(Hnat) / S                                    # Sheldon (1969)
  E_cam <- if (S > 1)                                       # Camargo (1993)
    1 - sum(outer(p, p, function(a, b) abs(a - b))[upper.tri(diag(S))]) / S else NA_real_
  # Smith & Wilson (1996) : Evar, variance des log-abondances, bornee [0 ; 1].
  E_var <- if (S > 1) {
    lx <- log(x)
    1 - 2 / pi * atan(sum((lx - mean(lx))^2) / S)
  } else NA_real_

  # Brillouin (1956) : l'entropie de la collection FINIE, pas de la population.
  HB <- if (all(abs(x - round(x)) < 1e-9))
    (lgamma(N + 1) - sum(lgamma(round(x) + 1))) / N else NA_real_
  HB <- if (is.finite(HB)) hstat_div_convertir(HB / log(2), "2", base) else NA_real_
  HBmax <- if (is.finite(HB) && S > 0) {
    n <- N %/% S; r <- N - S * n
    v <- (lgamma(N + 1) - (S - r) * lgamma(n + 1) - r * lgamma(n + 2)) / N
    hstat_div_convertir(v / log(2), "2", base)
  } else NA_real_
  E_B <- if (is.finite(HB) && is.finite(HBmax) && HBmax > 0) HB / HBmax else NA_real_

  U    <- sqrt(sum(x^2))                                    # McIntosh (1967)
  D_mc <- if (N > 0) (N - U) / (N - sqrt(N)) else NA_real_
  E_mc <- if (S > 1 && N > 0)
    (N - U) / (N - N / sqrt(S)) else NA_real_

  d_bp <- max(p)                                            # Berger & Parker (1970)

  Mg <- if (N > 1) (S - 1) / log(N) else NA_real_           # Margalef (1958)
  Mn <- S / sqrt(N)                                         # Menhinick (1964)

  data.frame(
    Individus = N, Richesse_S = S,
    Shannon_H = H, Shannon_Hmax = Hmax, Pielou_J = J,
    Simpson_D = D, Simpson_1_D = D1, Simpson_inverse = Dinv, Simpson_E = E_D,
    Heip_E = E_hp, Sheldon_E = E_sh, Camargo_E = E_cam, Smith_Wilson_Evar = E_var,
    Brillouin_HB = HB, Brillouin_E = E_B,
    McIntosh_U = U, McIntosh_D = D_mc, McIntosh_E = E_mc,
    Berger_Parker_d = d_bp, Berger_Parker_inverse = if (d_bp > 0) 1 / d_bp else NA_real_,
    Margalef_DMg = Mg, Menhinick_DMn = Mn,
    Fisher_alpha = hstat_div_fisher(S, N),
    Hill_N0 = S,
    Hill_N1 = exp(Hnat),
    Hill_N2 = Dinv,
    Hill_Ninf = if (d_bp > 0) 1 / d_bp else NA_real_,
    stringsAsFactors = FALSE)
}

# -- Alpha de Fisher ----------------------------------------------------------
# Fisher, Corbet & Williams (1943).
hstat_div_fisher <- function(S, N) {
  S <- as.numeric(S)[1]; N <- as.numeric(N)[1]
  if (!is.finite(S) || !is.finite(N) || S < 1 || N <= S) return(NA_real_)
  f <- function(a) a * log(1 + N / a) - S
  lo <- 1e-8; hi <- 1
  # L'alpha d'un peuplement tres divers monte vite.
  k <- 0L
  while (f(hi) < 0 && k < 200L) { hi <- hi * 2; k <- k + 1L }
  if (f(hi) < 0) return(NA_real_)
  if (f(lo) > 0) return(NA_real_)
  for (i in 1:200) {
    mid <- (lo + hi) / 2
    if (f(mid) < 0) lo <- mid else hi <- mid
  }
  (lo + hi) / 2
}

# -- Nombres de Hill : une seule famille, trois indices connus.
hstat_div_hill <- function(x, q = c(0, 1, 2, Inf)) {
  x <- as.numeric(x); x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(stats::setNames(rep(NA_real_, length(q)), paste0("q=", q)))
  p <- x / sum(x)
  v <- vapply(q, function(qq) {
    if (!is.finite(qq)) return(1 / max(p))
    if (abs(qq - 1) < 1e-9) return(exp(-sum(p * log(p))))
    sum(p^qq)^(1 / (1 - qq))
  }, numeric(1))
  stats::setNames(v, paste0("q=", q))
}

# -- Profil de Renyi ----------------------------------------------------------
# Renyi (1961) : H_a = ln(sum p^a) / (1 - a).
hstat_div_renyi <- function(x, alphas = c(0, 0.25, 0.5, 1, 2, 4, 8, Inf)) {
  x <- as.numeric(x); x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(stats::setNames(rep(NA_real_, length(alphas)), as.character(alphas)))
  p <- x / sum(x)
  v <- vapply(alphas, function(a) {
    if (!is.finite(a)) return(-log(max(p)))
    if (abs(a - 1) < 1e-9) return(-sum(p * log(p)))
    log(sum(p^a)) / (1 - a)
  }, numeric(1))
  stats::setNames(v, as.character(alphas))
}

# -- Entropie de Tsallis (HCDT) -----------------------------------------------
# Tsallis (1988) : (1 - sum p^q) / (q - 1).
hstat_div_tsallis <- function(x, q = c(0, 1, 2)) {
  x <- as.numeric(x); x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(stats::setNames(rep(NA_real_, length(q)), paste0("q=", q)))
  p <- x / sum(x)
  v <- vapply(q, function(qq) {
    if (abs(qq - 1) < 1e-9) return(-sum(p * log(p)))
    (1 - sum(p^qq)) / (qq - 1)
  }, numeric(1))
  stats::setNames(v, paste0("q=", q))
}

# -- Abondances : absolue, relative, frequence d'occurrence.
hstat_div_abondance <- function(m) {
  m <- as.matrix(m)
  tot <- sum(m)
  ab  <- colSums(m)
  data.frame(
    Espece = colnames(m),
    Abondance = as.numeric(ab),
    Abondance_relative_pct = if (tot > 0) as.numeric(ab / tot * 100) else NA_real_,
    Occurrences = as.integer(colSums(m > 0)),
    Frequence_pct = if (nrow(m) > 0) as.numeric(colSums(m > 0) / nrow(m) * 100) else NA_real_,
    Rang = rank(-as.numeric(ab), ties.method = "min"),
    stringsAsFactors = FALSE, row.names = NULL)[order(-ab), , drop = FALSE]
}

# Abondance relative DANS chaque releve : c'est la ligne qui somme a 100, et
# c'est la forme qu'un tableau de vegetation publie.
hstat_div_abondance_site <- function(m) {
  m <- as.matrix(m)
  tot <- rowSums(m)
  rel <- sweep(m, 1, ifelse(tot > 0, tot, NA_real_), "/") * 100
  data.frame(Releve = rownames(m), as.data.frame(rel, check.names = FALSE),
             check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
}

# DIVERSITE BETA : ce que deux releves ont en commun, et ce qui les separe
# TOUS LES COEFFICIENTS SE CALCULENT SUR a, b, c -- et c'est ce qui permet de
# les poser une seule fois.
HSTAT_DIV_BETA_BINAIRES <- c(
  "Jaccard"              = "jaccard",
  "Sørensen (Dice)"      = "sorensen",
  "Ochiai"               = "ochiai",
  "Kulczynski"           = "kulczynski",
  "Simpson (turnover)"   = "simpson",
  "Whittaker"            = "whittaker",
  "Russell-Rao"          = "russell",
  "Sokal-Michener"       = "sokal")

HSTAT_DIV_BETA_ABONDANCE <- c(
  "Bray-Curtis"          = "bray",
  "Morisita-Horn"        = "horn",
  "Morisita"             = "morisita",
  "Canberra"             = "canberra",
  "Euclidienne"          = "euclidean",
  "Manhattan"            = "manhattan",
  "Chao-Jaccard"         = "chao_jaccard",
  "Chao-Sørensen"        = "chao_sorensen")

.hstat_div_abc <- function(u, v) {
  pu <- u > 0; pv <- v > 0
  list(a = sum(pu & pv), b = sum(pu & !pv), c = sum(!pu & pv),
       d = sum(!pu & !pv))
}

# Similarite d'une PAIRE, pour un coefficient binaire.
hstat_div_sim_binaire <- function(u, v, methode = "jaccard") {
  k <- .hstat_div_abc(u, v); a <- k$a; b <- k$b; cc <- k$c; d <- k$d
  s <- switch(as.character(methode),
    jaccard    = if (a + b + cc > 0) a / (a + b + cc) else NA_real_,
    sorensen   = if (2 * a + b + cc > 0) 2 * a / (2 * a + b + cc) else NA_real_,
    ochiai     = if ((a + b) > 0 && (a + cc) > 0) a / sqrt((a + b) * (a + cc)) else NA_real_,
    kulczynski = if ((a + b) > 0 && (a + cc) > 0) (a / (a + b) + a / (a + cc)) / 2 else NA_real_,
    # Simpson (1943) : rapporte au PLUS PETIT des deux releves.
    simpson    = if (a + min(b, cc) > 0) a / (a + min(b, cc)) else NA_real_,
    # Whittaker (1960) est une DISSIMILARITE par construction : beta_w =
    # S/alpha - 1.
    whittaker  = if (2 * a + b + cc > 0) 1 - (b + cc) / (2 * a + b + cc) else NA_real_,
    russell    = if (a + b + cc + d > 0) a / (a + b + cc + d) else NA_real_,
    sokal      = if (a + b + cc + d > 0) (a + d) / (a + b + cc + d) else NA_real_,
    NA_real_)
  s
}

# Similarite d'une paire pour un coefficient d'ABONDANCE.
hstat_div_sim_abondance <- function(u, v, methode = "bray") {
  u <- as.numeric(u); v <- as.numeric(v)
  su <- sum(u); sv <- sum(v)
  s <- switch(as.character(methode),
    bray = if (su + sv > 0) 1 - sum(abs(u - v)) / (su + sv) else NA_real_,
    # Morisita-Horn (Horn 1966).
    horn = {
      if (su > 0 && sv > 0) {
        pu <- u / su; pv <- v / sv
        den <- sum(pu^2) + sum(pv^2)
        if (den > 0) 2 * sum(pu * pv) / den else NA_real_
      } else NA_real_
    },
    # Morisita (1959) : la forme d'origine, definie sur des EFFECTIFS entiers.
    morisita = {
      if (su > 1 && sv > 1 && all(abs(c(u, v) - round(c(u, v))) < 1e-9)) {
        lu <- sum(u * (u - 1)) / (su * (su - 1))
        lv <- sum(v * (v - 1)) / (sv * (sv - 1))
        if (lu + lv > 0) 2 * sum(u * v) / ((lu + lv) * su * sv) else NA_real_
      } else NA_real_
    },
    canberra = {
      k <- (u + v) > 0
      if (any(k)) 1 - mean(abs(u[k] - v[k]) / (u[k] + v[k])) else NA_real_
    },
    euclidean = NA_real_, manhattan = NA_real_,
    # Chao, Chazdon, Colwell & Shen (2005) : corrigent le biais des especes non
    # detectees.
    chao_jaccard = .hstat_div_chao_sim(u, v, "jaccard"),
    chao_sorensen = .hstat_div_chao_sim(u, v, "sorensen"),
    NA_real_)
  s
}

.hstat_div_chao_sim <- function(u, v, forme = "jaccard") {
  u <- as.numeric(u); v <- as.numeric(v)
  if (!all(abs(c(u, v) - round(c(u, v))) < 1e-9)) return(NA_real_)
  n <- sum(u); m <- sum(v)
  if (n <= 1 || m <= 1) return(NA_real_)
  part <- u > 0 & v > 0
  if (!any(part)) return(0)
  U <- sum(u[part]) / n
  V <- sum(v[part]) / m
  f1p <- sum(u[part] == 1); f2p <- max(1, sum(u[part] == 2))
  g1p <- sum(v[part] == 1); g2p <- max(1, sum(v[part] == 2))
  U <- U + (m - 1) / m * f1p / (2 * f2p) * sum(v[part & u == 1]) / n
  V <- V + (n - 1) / n * g1p / (2 * g2p) * sum(u[part & v == 1]) / m
  U <- min(1, U); V <- min(1, V)
  if (U <= 0 || V <= 0) return(0)
  if (identical(forme, "jaccard")) U * V / (U + V - U * V) else 2 * U * V / (U + V)
}

# -- Matrice de (dis)similarite entre tous les releves.
.hstat_div_abc_mat <- function(m) {
  P <- (m > 0) * 1
  A <- tcrossprod(P)
  S <- rowSums(P)
  B <- S - A                       # B[i, j] : especes de i absentes de j
  list(a = A, b = B, c = t(B), d = ncol(m) - A - B - t(B))
}

# Division qui rend NA quand le denominateur ne l'autorise pas.
.hstat_div_quot <- function(num, den, ok = den > 0) {
  out <- num / den
  out[!ok] <- NA_real_
  out
}

.hstat_div_beta_vect <- function(m, methode) {
  if (methode %in% HSTAT_DIV_BETA_BINAIRES) {
    k <- .hstat_div_abc_mat(m); a <- k$a; b <- k$b; cc <- k$c; d <- k$d
    bmin <- pmin(b, cc)
    return(switch(methode,
      jaccard    = .hstat_div_quot(a, a + b + cc),
      sorensen   = .hstat_div_quot(2 * a, 2 * a + b + cc),
      ochiai     = .hstat_div_quot(a, sqrt((a + b) * (a + cc)),
                                   (a + b) > 0 & (a + cc) > 0),
      kulczynski = {
        o <- (a / (a + b) + a / (a + cc)) / 2
        o[!((a + b) > 0 & (a + cc) > 0)] <- NA_real_
        o
      },
      simpson    = .hstat_div_quot(a, a + bmin),
      whittaker  = {
        o <- 1 - (b + cc) / (2 * a + b + cc)
        o[!(2 * a + b + cc > 0)] <- NA_real_
        o
      },
      russell    = .hstat_div_quot(a, a + b + cc + d),
      sokal      = .hstat_div_quot(a + d, a + b + cc + d),
      NULL))
  }
  su <- rowSums(m)
  switch(methode,
    euclidean = as.matrix(stats::dist(m, method = "euclidean")),
    manhattan = as.matrix(stats::dist(m, method = "manhattan")),
    bray = {
      den <- outer(su, su, "+")
      .hstat_div_quot(den - as.matrix(stats::dist(m, method = "manhattan")), den)
    },
    horn = {
      vu <- su > 0
      pu <- m / ifelse(vu, su, 1)
      q  <- rowSums(pu^2)
      den <- outer(q, q, "+")
      .hstat_div_quot(2 * tcrossprod(pu), den, outer(vu, vu, "&") & den > 0)
    },
    morisita = {
      entier <- apply(abs(m - round(m)) < 1e-9, 1, all)
      vu <- su > 1 & entier
      lu <- ifelse(vu, rowSums(m * (m - 1)) / (su * (su - 1)), NA_real_)
      ll <- outer(lu, lu, "+")
      ok <- outer(vu, vu, "&") & !is.na(ll) & ll > 0
      ok[is.na(ok)] <- FALSE
      .hstat_div_quot(2 * tcrossprod(m), ll * outer(su, su), ok)
    },
    canberra      = .hstat_div_par_ligne(m, .hstat_div_canberra_ligne),
    chao_jaccard  = .hstat_div_chao_mat(m, "jaccard"),
    chao_sorensen = .hstat_div_chao_mat(m, "sorensen"),
    NULL)
}

# Une ligne contre TOUTES les suivantes, d'un coup : n appels au lieu de n (n -
# 1) / 2.
.hstat_div_par_ligne <- function(m, f) {
  n <- nrow(m)
  out <- matrix(NA_real_, n, n)
  for (i in seq_len(n - 1L)) {
    j <- (i + 1L):n
    s <- f(m[i, ], m[j, , drop = FALSE])
    out[i, j] <- s; out[j, i] <- s
  }
  out
}

.hstat_div_canberra_ligne <- function(u, V) {
  U <- matrix(u, nrow(V), ncol(V), byrow = TRUE)
  k <- (U + V) > 0
  r <- abs(U - V) / (U + V)
  r[!k] <- 0
  nk <- rowSums(k)
  out <- 1 - rowSums(r) / nk
  out[nk == 0] <- NA_real_
  out
}

# La forme MATRICIELLE de `.hstat_div_chao_sim()`, qui reste la definition.
.hstat_div_chao_mat <- function(m, forme = "jaccard") {
  P  <- (m > 0) * 1
  E1 <- (m == 1) * 1
  E2 <- (m == 2) * 1
  n  <- rowSums(m)
  ent <- apply(abs(m - round(m)) < 1e-9, 1, all)
  k  <- nrow(m)
  Ni <- matrix(n, k, k); Nj <- t(Ni)
  A    <- tcrossprod(P)
  Usum <- tcrossprod(m, P)
  F1 <- tcrossprod(E1, P); F2 <- pmax(tcrossprod(E2, P), 1)
  SV1 <- tcrossprod(E1, m)
  U  <- Usum / Ni
  Vv <- t(Usum) / Nj
  U  <- U  + (Nj - 1) / Nj * F1 / (2 * F2) * SV1 / Ni
  Vv <- Vv + (Ni - 1) / Ni * t(F1) / (2 * t(F2)) * t(SV1) / Nj
  U <- pmin(U, 1); Vv <- pmin(Vv, 1)
  out <- if (identical(forme, "jaccard")) U * Vv / (U + Vv - U * Vv) else 2 * U * Vv / (U + Vv)
  na <- !outer(ent, ent, "&") | Ni <= 1 | Nj <= 1
  out[!na & (U <= 0 | Vv <= 0)] <- 0
  out[!na & A == 0] <- 0
  out[na] <- NA_real_
  out
}

hstat_div_beta <- function(m, methode = "jaccard", sortie = c("similarite", "dissimilarite")) {
  sortie <- match.arg(sortie)
  m <- as.matrix(m)
  n <- nrow(m)
  if (n < 2) stop("Au moins deux relevés sont nécessaires pour la diversité bêta.", call. = FALSE)
  .hstat_div_releves_max(n)
  binaire <- methode %in% HSTAT_DIV_BETA_BINAIRES
  # Les distances metriques n'ont pas de similarite bornee a 1 : les rendre en
  # « similarite » exigerait une normalisation arbitraire.
  metrique <- methode %in% c("euclidean", "manhattan")
  out <- .hstat_div_beta_vect(m, methode)
  if (is.null(out)) {
    out <- matrix(NA_real_, n, n)
    for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
      s <- if (binaire) hstat_div_sim_binaire(m[i, ], m[j, ], methode)
           else hstat_div_sim_abondance(m[i, ], m[j, ], methode)
      out[i, j] <- s; out[j, i] <- s
    }
  }
  out <- unname(out)
  storage.mode(out) <- "double"
  diag(out) <- if (metrique) 0 else 1
  dimnames(out) <- list(rownames(m), rownames(m))
  if (identical(sortie, "dissimilarite") && !metrique) out <- 1 - out
  attr(out, "methode")  <- methode
  attr(out, "metrique") <- metrique
  attr(out, "sortie")   <- if (metrique) "distance" else sortie
  out
}

# -- Partition de Baselga : remplacement contre emboitement -------------------
# Baselga (2010).
hstat_div_baselga <- function(m) {
  m <- as.matrix(m); n <- nrow(m)
  if (n < 2) stop("Au moins deux relevés sont nécessaires.", call. = FALSE)
  .hstat_div_releves_max(n)
  k <- .hstat_div_abc_mat(m)
  ij <- which(upper.tri(k$a), arr.ind = TRUE)
  ij <- ij[order(ij[, 1], ij[, 2]), , drop = FALSE]
  a <- k$a[ij]; b <- k$b[ij]; cc <- k$c[ij]
  bmin <- pmin(b, cc)
  sor <- .hstat_div_quot(b + cc, 2 * a + b + cc)
  sim <- .hstat_div_quot(bmin, bmin + a)
  jac <- .hstat_div_quot(b + cc, a + b + cc)
  jtu <- .hstat_div_quot(2 * bmin, 2 * bmin + a)
  rn <- rownames(m) %||% rep(NA_character_, n)
  data.frame(
    Releve_1 = rn[ij[, 1]], Releve_2 = rn[ij[, 2]],
    Communes_a = as.integer(a), Exclusives_1_b = as.integer(b),
    Exclusives_2_c = as.integer(cc),
    Sorensen_total = sor, Simpson_turnover = sim, Sorensen_emboitement = sor - sim,
    Jaccard_total = jac, Jaccard_turnover = jtu, Jaccard_emboitement = jac - jtu,
    stringsAsFactors = FALSE)
}

# -- Partition de Whittaker : alpha, beta, gamma ------------------------------
# Whittaker (1960).
hstat_div_whittaker <- function(m, base = "2") {
  m <- as.matrix(m)
  alpha_S <- mean(rowSums(m > 0))
  gamma_S <- sum(colSums(m) > 0)
  ind <- do.call(rbind, lapply(seq_len(nrow(m)), function(i) hstat_div_indices(m[i, ], base)))
  pool <- hstat_div_indices(colSums(m), base)
  data.frame(
    Mesure = c("Richesse (S)", "Shannon (H')", "Simpson inverse (1/D)"),
    Alpha_moyen = c(alpha_S, mean(ind$Shannon_H, na.rm = TRUE),
                    mean(ind$Simpson_inverse, na.rm = TRUE)),
    Gamma_total = c(gamma_S, pool$Shannon_H, pool$Simpson_inverse),
    Beta_additif = c(gamma_S - alpha_S,
                     pool$Shannon_H - mean(ind$Shannon_H, na.rm = TRUE),
                     pool$Simpson_inverse - mean(ind$Simpson_inverse, na.rm = TRUE)),
    # `alpha_S` vaut NaN des que la matrice n'a aucune ligne :
    # `mean(numeric(0))`.
    Beta_multiplicatif = c(
      if (isTRUE(alpha_S > 0)) gamma_S / alpha_S else NA_real_,
      NA_real_,
      if (isTRUE(mean(ind$Simpson_inverse, na.rm = TRUE) > 0))
        pool$Simpson_inverse / mean(ind$Simpson_inverse, na.rm = TRUE) else NA_real_),
    Beta_Whittaker = c(if (isTRUE(alpha_S > 0)) gamma_S / alpha_S - 1 else NA_real_,
                       NA_real_, NA_real_),
    stringsAsFactors = FALSE)
}

# -- Rarefaction : comparer a effort egal -------------------------------------
# Sanders (1968), corrige par Hurlbert (1971).
hstat_div_rarefaction <- function(x, n) {
  x <- as.numeric(x); x <- x[is.finite(x) & x > 0]
  if (!length(x) || !all(abs(x - round(x)) < 1e-9)) return(NA_real_)
  N <- sum(x); n <- min(n, N)
  if (n < 1) return(NA_real_)
  termes <- vapply(x, function(xi) {
    if (N - xi < n) return(0)
    exp(lchoose(N - xi, n) - lchoose(N, n))
  }, numeric(1))
  sum(1 - termes)
}

# Courbe de rarefaction d'un releve, de 1 individu a son effectif total.
hstat_div_courbe_rarefaction <- function(x, pas = NULL) {
  x <- as.numeric(x); x <- x[is.finite(x) & x > 0]
  N <- sum(x)
  if (!is.finite(N) || N < 1) return(NULL)
  pas <- pas %||% max(1, floor(N / 50))
  ns  <- unique(c(seq(1, N, by = pas), N))
  data.frame(Individus = ns,
             Especes = vapply(ns, function(k) hstat_div_rarefaction(x, k), numeric(1)),
             stringsAsFactors = FALSE)
}

# -- Courbe d'accumulation des especes ----------------------------------------
# Combien d'especes de plus par releve supplementaire.
hstat_div_accumulation <- function(m, permutations = 100L, graine = 123L) {
  m <- as.matrix(m); n <- nrow(m)
  if (n < 1) return(NULL)
  hstat_set_seed(graine)
  acc <- matrix(NA_real_, permutations, n)
  # Une espece est vue a partir du premier releve qui la porte : le cumul de
  # presence est un maximum cumule par colonne.
  pres <- m > 0
  for (p in seq_len(permutations)) {
    ordre <- sample.int(n)
    vu <- apply(pres[ordre, , drop = FALSE], 2, cummax)
    if (!is.matrix(vu)) vu <- matrix(vu, nrow = n)
    acc[p, ] <- rowSums(vu)
  }
  data.frame(Releves = seq_len(n),
             Especes = colMeans(acc),
             Ecart_type = apply(acc, 2, stats::sd),
             IC_inf = apply(acc, 2, stats::quantile, 0.025),
             IC_sup = apply(acc, 2, stats::quantile, 0.975),
             stringsAsFactors = FALSE)
}

# SEUILS DE LECTURE : qui les a poses, et sur quelle base
# DEUX ORIGINES, ET LES CONFONDRE SERAIT MALHONNETE.
HSTAT_DIV_SEUILS <- list(

  Shannon_H = list(
    indice = "Indice de Shannon (H')", base = "2",
    bornes = c(1, 2, 3, 4),
    etats  = c("err", "err", "warn", "ok", "ok"),
    libelles = c("Très faible", "Faible", "Moyenne", "Élevée", "Très élevée"),
    interpretations = c(
      "Peuplement très déséquilibré : une ou deux espèces concentrent presque tous les individus.",
      "Diversité faible : le peuplement est dominé par un petit nombre d'espèces.",
      "Diversité moyenne : plusieurs espèces se partagent le peuplement.",
      "Diversité élevée : les individus se répartissent sur de nombreuses espèces.",
      "Diversité très élevée : peuplement riche et équilibré."),
    auteur = "Frontier (1983)",
    reference = paste("FRONTIER S. (1983) — Stratégies d'échantillonnage en écologie.",
                      "Masson, Paris. Grille reprise par Frontier & Pichod-Viale (1991)",
                      "et Dajoz (2006). L'indice est de SHANNON C.E. (1948),",
                      "A mathematical theory of communication, Bell System Tech. J. 27 : 379-423."),
    origine = "usage"),

  Shannon_H_nats = list(
    indice = "Indice de Shannon (H', en nats)", base = "e",
    bornes = c(1.5, 3.5, 4.5),
    etats  = c("warn", "ok", "ok", "warn"),
    libelles = c("Sous l'étendue usuelle", "Étendue usuelle", "Étendue usuelle haute",
                 "Exceptionnellement élevée"),
    interpretations = c(
      "En deçà de l'étendue habituellement observée : peuplement peu diversifié ou échantillon trop petit.",
      "Dans l'étendue habituelle des peuplements naturels.",
      "Dans le haut de l'étendue habituelle.",
      "Au-delà de ce qui s'observe habituellement : vérifiez la base du logarithme et la taille de l'échantillon."),
    auteur = "Magurran (2004)",
    reference = paste("MAGURRAN A.E. (2004) — Measuring Biological Diversity.",
                      "Blackwell, Oxford, p. 107 : les valeurs de H' « se situent",
                      "habituellement entre 1,5 et 3,5 et ne dépassent que rarement 4,5 »",
                      "(logarithme naturel). C'est un CADRAGE D'ORDRE DE GRANDEUR observé,",
                      "non une grille de qualité."),
    origine = "primaire"),

  Pielou_J = list(
    indice = "Équitabilité de Pielou (J)", base = NA,
    bornes = c(0.5, 0.65, 0.8),
    etats  = c("err", "warn", "warn", "ok"),
    libelles = c("Déséquilibré", "Déséquilibre modéré", "Tendance à l'équilibre", "Équilibré"),
    interpretations = c(
      "Une ou quelques espèces dominent nettement : les individus sont très inégalement répartis.",
      "Répartition inégale : la dominance reste marquée.",
      "Répartition assez régulière des individus entre les espèces.",
      "Peuplement équilibré : les espèces ont des abondances comparables."),
    auteur = "Pielou (1966) pour l'indice ; grille d'usage (Daget 1976, Barbault 1981)",
    reference = paste("PIELOU E.C. (1966) — The measurement of diversity in different types",
                      "of biological collections. J. Theor. Biol. 13 : 131-144. L'indice est",
                      "défini comme J = H'/H'max et tend vers 0 quand une espèce domine,",
                      "vers 1 quand les abondances sont égales — cela relève de la DÉFINITION.",
                      "Les bornes 0,5 / 0,65 / 0,8 relèvent de l'usage en écologie francophone",
                      "(DAGET J., 1976, Les modèles mathématiques en écologie, Masson ;",
                      "BARBAULT R., 1981, Écologie des populations et des peuplements, Masson)."),
    origine = "usage"),

  Simpson_1_D = list(
    indice = "Diversité de Simpson (1 − D)", base = NA,
    bornes = c(0.3, 0.6, 0.8),
    etats  = c("err", "warn", "ok", "ok"),
    libelles = c("Faible", "Moyenne", "Élevée", "Très élevée"),
    interpretations = c(
      "Deux individus tirés au hasard ont moins de 3 chances sur 10 d'appartenir à des espèces différentes.",
      "Deux individus tirés au hasard ont 3 à 6 chances sur 10 d'appartenir à des espèces différentes.",
      "Deux individus tirés au hasard ont 6 à 8 chances sur 10 d'appartenir à des espèces différentes.",
      "Deux individus tirés au hasard appartiennent presque toujours à des espèces différentes."),
    auteur = "Simpson (1949) pour l'indice ; grille d'usage",
    reference = paste("SIMPSON E.H. (1949) — Measurement of diversity. Nature 163 : 688.",
                      "1 − D est la PROBABILITÉ que deux individus tirés au hasard",
                      "appartiennent à deux espèces différentes : sa lecture est directe et",
                      "ne demande aucun seuil. Les bornes 0,3 / 0,6 / 0,8 sont une commodité",
                      "de lecture d'usage, sans source primaire unique."),
    origine = "usage"),

  Berger_Parker_d = list(
    indice = "Dominance de Berger-Parker (d)", base = NA,
    bornes = c(0.25, 0.5, 0.75),
    etats  = c("ok", "warn", "err", "err"),
    libelles = c("Pas de dominance marquée", "Dominance modérée",
                 "Dominance forte", "Dominance très forte"),
    interpretations = c(
      "L'espèce la plus abondante représente moins du quart des individus.",
      "L'espèce la plus abondante représente le quart à la moitié des individus.",
      "L'espèce la plus abondante représente plus de la moitié des individus.",
      "L'espèce la plus abondante représente plus des trois quarts des individus : peuplement quasi monospécifique."),
    auteur = "Berger & Parker (1970)",
    reference = paste("BERGER W.H. & PARKER F.L. (1970) — Diversity of planktonic",
                      "foraminifera in deep-sea sediments. Science 168 : 1345-1347.",
                      "d est la PART de l'espèce la plus abondante : le seuil de 0,5",
                      "(« une espèce détient plus de la moitié du peuplement ») découle",
                      "de la définition ; les bornes 0,25 et 0,75 sont d'usage."),
    origine = "usage"),

  Couverture_Good = list(
    indice = "Couverture de Good (C)", base = NA,
    bornes = c(0.8, 0.9, 0.95),
    etats  = c("err", "warn", "warn", "ok"),
    libelles = c("Inventaire très incomplet", "Inventaire incomplet",
                 "Inventaire acceptable", "Inventaire quasi complet"),
    interpretations = c(
      "Plus de 20 % des individus appartiennent à des espèces vues une seule fois : l'échantillonnage est nettement insuffisant.",
      "10 à 20 % des individus appartiennent à des espèces vues une seule fois : poursuivez l'échantillonnage.",
      "5 à 10 % des individus appartiennent à des espèces vues une seule fois.",
      "Moins de 5 % des individus appartiennent à des espèces vues une seule fois : l'inventaire peut être considéré comme complet."),
    auteur = "Good (1953) pour l'estimateur ; seuil de 0,95 d'usage",
    reference = paste("GOOD I.J. (1953) — The population frequencies of species and the",
                      "estimation of population parameters. Biometrika 40 : 237-264.",
                      "C = 1 − F1/N. Le seuil de 95 % au-delà duquel un inventaire est",
                      "tenu pour complet est une pratique répandue (écologie microbienne,",
                      "inventaires entomologiques), non une borne publiée par Good."),
    origine = "usage"),

  Completude = list(
    indice = "Complétude de l'inventaire (Sobs / Chao1)", base = NA,
    bornes = c(0.5, 0.7, 0.85),
    etats  = c("err", "err", "warn", "ok"),
    libelles = c("Très incomplet", "Incomplet", "Partiel", "Satisfaisant"),
    interpretations = c(
      "Moins de la moitié des espèces estimées présentes ont été observées.",
      "La moitié aux deux tiers des espèces estimées ont été observées : l'effort est insuffisant.",
      "70 à 85 % des espèces estimées ont été observées.",
      "Plus de 85 % des espèces estimées ont été observées : l'inventaire couvre l'essentiel du peuplement."),
    auteur = "Chao (1984) pour l'estimateur ; grille d'usage",
    reference = paste("CHAO A. (1984) — Nonparametric estimation of the number of classes",
                      "in a population. Scand. J. Stat. 11 : 265-270. Le rapport",
                      "Sobs/Chao1 mesure la part du peuplement effectivement observée ;",
                      "les bornes retenues relèvent de la pratique des inventaires."),
    origine = "usage"),

  Jaccard_similarite = list(
    indice = "Similarité de Jaccard", base = NA,
    bornes = c(0.25, 0.5, 0.75),
    etats  = c("err", "warn", "ok", "ok"),
    libelles = c("Très dissemblables", "Dissemblables", "Semblables", "Très semblables"),
    interpretations = c(
      "Moins du quart des espèces sont communes aux deux relevés.",
      "Un quart à la moitié des espèces sont communes.",
      "La moitié aux trois quarts des espèces sont communes.",
      "Plus des trois quarts des espèces sont communes : les deux relevés décrivent le même peuplement."),
    auteur = "Jaccard (1901, 1912) pour l'indice ; grille d'usage",
    reference = paste("JACCARD P. (1901) — Distribution de la flore alpine dans le bassin",
                      "des Dranses. Bull. Soc. Vaud. Sci. Nat. 37 : 241-272 ; JACCARD P.",
                      "(1912) — The distribution of the flora in the alpine zone.",
                      "New Phytol. 11 : 37-50. L'indice vaut a/(a+b+c). Les bornes de",
                      "lecture par quarts sont une convention, non une borne publiée."),
    origine = "usage"),

  Sorensen_similarite = list(
    indice = "Similarité de Sørensen", base = NA,
    bornes = c(0.33, 0.5, 0.75),
    etats  = c("err", "warn", "ok", "ok"),
    libelles = c("Très dissemblables", "Dissemblables", "Semblables", "Très semblables"),
    interpretations = c(
      "Les deux relevés partagent peu d'espèces.",
      "Les deux relevés partagent une minorité d'espèces.",
      "Les deux relevés partagent la moitié à trois quarts de leurs espèces.",
      "Les deux relevés partagent l'essentiel de leurs espèces."),
    auteur = "Sørensen (1948), indice proposé indépendamment par Dice (1945) ; grille d'usage",
    reference = paste("SØRENSEN T. (1948) — A method of establishing groups of equal",
                      "amplitude in plant sociology. Biol. Skr. 5 : 1-34 ; DICE L.R. (1945)",
                      "— Measures of the amount of ecologic association between species.",
                      "Ecology 26 : 297-302. L'indice vaut 2a/(2a+b+c). Le seuil de 50 %",
                      "employé en phytosociologie pour rattacher deux relevés au même",
                      "groupement est une convention de la discipline."),
    origine = "usage"),

  Margalef_DMg = list(
    indice = "Richesse de Margalef (DMg)", base = NA,
    bornes = c(2.5, 4),
    etats  = c("err", "warn", "ok"),
    libelles = c("Faible", "Moyenne", "Élevée"),
    interpretations = c(
      "Faible richesse spécifique rapportée à l'effort d'échantillonnage.",
      "Richesse spécifique moyenne rapportée à l'effort d'échantillonnage.",
      "Richesse spécifique élevée rapportée à l'effort d'échantillonnage."),
    auteur = "Margalef (1958) pour l'indice ; grille d'usage",
    reference = paste("MARGALEF R. (1958) — Information theory in ecology.",
                      "General Systems 3 : 36-71. DMg = (S − 1)/ln(N) corrige la richesse",
                      "par l'effort. Les bornes 2,5 et 4 sont d'usage en écologie",
                      "aquatique et benthique, sans source primaire unique."),
    origine = "usage"),

  Simpson_E = list(
    indice = "Équitabilité de Simpson (E1/D)", base = NA,
    bornes = c(0.4, 0.6, 0.8),
    etats  = c("err", "warn", "ok", "ok"),
    libelles = c("Faible", "Moyenne", "Bonne", "Très bonne"),
    interpretations = c(
      "Le nombre d'espèces effectivement équivalentes est bien inférieur à la richesse observée.",
      "Une partie des espèces observées pèse peu dans le peuplement.",
      "Les espèces observées contribuent de façon assez régulière.",
      "Les espèces observées ont des poids proches : peuplement très régulier."),
    auteur = "Smith & Wilson (1996) pour la formulation E1/D ; grille d'usage",
    reference = paste("SMITH B. & WILSON J.B. (1996) — A consumer's guide to evenness",
                      "indices. Oikos 76 : 70-82, qui recense et compare les indices",
                      "d'équitabilité, dont E1/D = (1/D)/S. Les bornes relèvent de l'usage."),
    origine = "usage")
)

# -- Le verdict : jamais de branchement sur une valeur non calculable ---------
# Un indice rend NA des que le releve est degenere.
hstat_div_verdict <- function(indice, valeur, base = "2") {
  g <- HSTAT_DIV_SEUILS[[as.character(indice)[1]]]
  if (is.null(g))
    return(list(etat = "indeterminable", libelle = NA_character_,
                interpretation = NA_character_, auteur = NA_character_,
                reference = NA_character_, origine = NA_character_,
                grille = NA_character_, valeur = NA_real_))
  v <- suppressWarnings(as.numeric(valeur)[1])
  # LA CONVERSION DE BASE EST LE COEUR DU MECANISME.
  if (!is.na(g$base) && !identical(as.character(base), as.character(g$base)))
    v <- hstat_div_convertir(v, base, g$base)
  grille <- .hstat_div_grille_texte(g)
  if (!is.finite(v))
    return(list(etat = "indeterminable",
                libelle = "Indéterminable",
                interpretation = "L'indice n'est pas calculable sur ce relevé (une seule espèce, aucun individu, ou effectifs non entiers).",
                auteur = g$auteur, reference = g$reference, origine = g$origine,
                grille = grille, valeur = NA_real_))
  k <- sum(v >= g$bornes) + 1L
  list(etat = g$etats[k], libelle = g$libelles[k],
       interpretation = g$interpretations[k],
       auteur = g$auteur, reference = g$reference, origine = g$origine,
       grille = grille, valeur = v)
}

.hstat_div_grille_texte <- function(g) {
  n <- length(g$libelles)
  b <- g$bornes
  parts <- character(n)
  for (i in seq_len(n)) {
    parts[i] <- if (i == 1L) sprintf("< %s", .hstat_div_num(b[1]))
    else if (i == n) sprintf("≥ %s", .hstat_div_num(b[n - 1]))
    else sprintf("%s – %s", .hstat_div_num(b[i - 1]), .hstat_div_num(b[i]))
    parts[i] <- paste0(parts[i], " : ", g$libelles[i])
  }
  paste(parts, collapse = "  |  ")
}

.hstat_div_num <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return("—")
  sub("[.]?0+$", "", formatC(x, format = "f", digits = 3))
}

# -- Tableau d'interpretation complet.
hstat_div_interpreter <- function(indices, base = "2", cles = NULL) {
  cles <- cles %||% names(HSTAT_DIV_SEUILS)
  cles <- intersect(cles, names(HSTAT_DIV_SEUILS))
  lignes <- lapply(cles, function(k) {
    # La cle « Shannon_H_nats » lit la MEME colonne que « Shannon_H » : ce sont
    # deux lectures d'une seule valeur, pas deux valeurs.
    col <- if (identical(k, "Shannon_H_nats")) "Shannon_H" else k
    val <- if (!is.null(indices[[col]])) indices[[col]][1] else NA_real_
    v <- hstat_div_verdict(k, val, base)
    data.frame(
      Indice = HSTAT_DIV_SEUILS[[k]]$indice,
      Valeur = v$valeur,
      Classe = v$libelle %||% NA_character_,
      Interpretation = v$interpretation %||% NA_character_,
      Grille = v$grille, Auteur = v$auteur,
      Origine = if (identical(v$origine, "primaire"))
        "Seuils publiés par l'auteur" else "Convention d'usage",
      Reference = v$reference,
      stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, lignes)
  res[!is.na(res$Indice), , drop = FALSE]
}

# UN COMPTAGE N'A PAS DE DECIMALES
# `DT::formatRound()` applique le meme nombre de decimales a toutes les
# colonnes qu'on lui nomme.
hstat_cols_entieres <- function(df, tol = 1e-9) {
  if (is.null(df) || !length(df)) return(logical(0))
  vapply(df, function(v) {
    if (!is.numeric(v)) return(FALSE)
    o <- v[is.finite(v)]
    length(o) > 0L && all(abs(o - round(o)) < tol)
  }, logical(1))
}

# Pose les deux formats sur un `DT::datatable` : zero decimale sur les colonnes
# de comptage, `digits` sur les autres.
hstat_dt_arrondi <- function(d, df, digits = 4) {
  num    <- vapply(df, is.numeric, logical(1))
  entier <- hstat_cols_entieres(df)
  if (any(num & !entier)) d <- DT::formatRound(d, names(df)[num & !entier], digits)
  if (any(entier))        d <- DT::formatRound(d, names(df)[entier], 0)
  d
}

# EPIDEMIOLOGIE -- les modeles que la discipline emploie, et leurs pieges
# Regle du depot appliquee a la lettre : TOUT le calcul vit ici, le module ne
# fait que choisir, afficher et mettre en forme.

HSTAT_EPI_ANALYSES <- list(
  dlnm = c(
    "DLNM — exposition, retard et réponse",
    "Surface exposition-retard par splines croisées : l'effet peut être non linéaire ET étalé dans le temps (vagues de chaleur, pollution).",
    "Quantifier l'effet différé d'une exposition environnementale sur un décompte sanitaire.",
    "Série longue devant le décalage ; un dénominateur (offset) si le décompte dépend d'une population."),
  taux = c(
    "Taux d'incidence (Poisson / binomiale négative)",
    "Modèle de comptage avec temps-personne en offset : les coefficients sont des rapports de taux d'incidence (IRR).",
    "Comparer des taux entre groupes en tenant compte du temps d'observation.",
    "Un décompte d'événements et un temps-personne strictement positif."),
  risque = c(
    "Risque : OR, RR ajustés et leur écart",
    "Logistique (OR), log-binomiale et Poisson robuste (RR) sur les mêmes données : l'écart OR/RR se mesure au lieu de se supposer.",
    "Estimer un risque ajusté sans confondre cote et risque quand l'issue est fréquente.",
    "Issue binaire 0/1 ; au moins un facteur d'exposition."),
  survie = c(
    "Survie : Kaplan-Meier, log-rank et Cox",
    "Courbes de survie, test du log-rank et modèle à risques proportionnels, avec vérification de l'hypothèse PH (Schoenfeld).",
    "Analyser un délai de survenue en présence de censure.",
    "Une durée de suivi, un indicateur d'événement (1 = survenu, 0 = censuré)."),
  cascroise = c(
    "Cas-croisé (case-crossover) stratifié sur le temps",
    "Chaque cas est son propre témoin : les jours-témoins sont pris dans la même strate (année x mois x jour de semaine), et le modèle est une logistique conditionnelle.",
    "Effet aigu d'une exposition sans avoir à modéliser la saison ni les facteurs propres au sujet.",
    "Une date, un décompte de cas et une exposition datée."),
  smr = c(
    "Standardisation et SMR / SIR",
    "Standardisation directe (taux comparables) et indirecte (rapport observé/attendu), l'intervalle du SMR étant exact (Poisson).",
    "Comparer la mortalité ou l'incidence de populations de structures d'âge différentes.",
    "Des effectifs, des événements par strate et des taux de référence."),
  diagnostic = c(
    "Test diagnostique : Se, Sp, VPP, VPN, ROC",
    "Sensibilité, spécificité, valeurs prédictives, rapports de vraisemblance, indice de Youden et aire sous la courbe ROC.",
    "Évaluer un test de dépistage ou un score de prédiction.",
    "Un résultat de test (binaire ou continu) et un état de référence binaire."),
  impact = c(
    "Mesures d'impact : RA, FAE, FAP, NNT",
    "Risque attribuable, fraction attribuable chez les exposés et en population, nombre de sujets à traiter.",
    "Traduire une association en nombre de cas évitables — ce que demande une décision de santé publique.",
    "Un tableau 2x2 exposition x issue, et la prévalence de l'exposition pour la FAP.")
)

# UN INTERVALLE DE COMPTAGE EST EXACT, PAS NORMAL
# L'approximation normale sur un decompte d'evenements rend une borne basse
# NEGATIVE des que le compte est petit.
hstat_epi_ic_poisson <- function(x, conf = 0.95) {
  x <- suppressWarnings(as.numeric(x))
  conf <- suppressWarnings(as.numeric(conf)[1])
  if (!isTRUE(is.finite(conf)) || conf <= 0 || conf >= 1) conf <- 0.95
  a <- (1 - conf) / 2
  bas <- ifelse(!is.finite(x) | x < 0, NA_real_,
                ifelse(x == 0, 0, stats::qchisq(a, 2 * x) / 2))
  haut <- ifelse(!is.finite(x) | x < 0, NA_real_,
                 stats::qchisq(1 - a, 2 * (x + 1)) / 2)
  list(bas = bas, haut = haut, conf = conf)
}

# DLNM -- surface exposition / retard / reponse
# Ce n'est PAS le DLNM de l'onglet Series temporelles, qui est une methode de
# PREVISION.

HSTAT_EPI_DLNM_FAMILLES <- c(
  "Automatique (AIC, puis surdispersion)" = "auto",
  "Poisson"                              = "poisson",
  "Quasi-Poisson (surdispersion)"        = "quasipoisson",
  "Binomiale négative"                   = "nb")

# LE CHOIX AUTOMATIQUE SE FAIT EN DEUX TEMPS, ET CE N'EST PAS UN DETOUR
# « Automatique » ne comparait que Poisson et la binomiale negative par AIC,
# puis, sous Poisson surdisperse, se contentait de CONSEILLER quasi-Poisson --
# un reglage que l'utilisateur devait aller changer a la main, sur une analyse
# qu'il croyait deja ajustee au mieux.
# 1. AIC entre Poisson et binomiale negative -- quel AJUSTEMENT est meilleur ;
# 2. si Poisson l'emporte mais que les residus sont surdisperses, on passe a
#    quasi-Poisson.
HSTAT_EPI_DISP_SEUIL <- 1.5

# La surdispersion se MESURE : somme des residus de Pearson au carre rapportee
# au degre de liberte residuel.
.hstat_epi_disp <- function(fit) {
  tryCatch({
    rp <- stats::residuals(fit, type = "pearson")
    sum(rp^2, na.rm = TRUE) / stats::df.residual(fit)
  }, error = function(e) NA_real_)
}

#' Quelle loi retenir, d'apres les deux AIC et la surdispersion mesurée.
#'
#' La REGLE vit ici, hors du bloc d'ajustement : dans le corps de
#' `hstat_epi_dlnm()` elle n'est atteignable qu'en faisant echouer `glm.nb`,
#' donc intestable en pratique. C'est la regle du depot -- une statistique
#' posee dans un bloc d'ajustement n'est pas testable.
#'
#' Rend une liste : `famille`, et `motif` (ce qui a decide), jamais `NULL`.
hstat_epi_famille_auto <- function(aic_poisson, aic_nb, dispersion,
                                   seuil = HSTAT_EPI_DISP_SEUIL,
                                   nb_dispo = TRUE) {
  ap <- suppressWarnings(as.numeric(aic_poisson)[1])
  an <- suppressWarnings(as.numeric(aic_nb)[1])
  dp <- suppressWarnings(as.numeric(dispersion)[1])
  # UN AIC ABSENT N'EST PAS UN AIC PERDANT.
  if (isTRUE(nb_dispo) && isTRUE(is.finite(an)) &&
      (!isTRUE(is.finite(ap)) || an < ap))
    return(list(famille = "nb", motif = "aic"))
  # La binomiale negative n'a pas gagne.
  if (isTRUE(is.finite(dp)) && dp > seuil)
    return(list(famille = "quasipoisson", motif = "dispersion"))
  list(famille = "poisson", motif = "aic")
}

# Les percentiles auxquels la surface est lue.
HSTAT_EPI_DLNM_PCT <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)

# LES NOEUDS SONT DES PERCENTILES, ET DEUX PERCENTILES PEUVENT COINCIDER
# Sur une exposition peu variable -- une temperature de saison seche, une serie
# tronquee -- P25 et P50 tombent sur la meme valeur.
.hstat_epi_noeuds <- function(x, probs) {
  x <- x[is.finite(x)]
  if (length(x) < 4L) return(list(noeuds = numeric(0), message = NULL))
  k <- unname(stats::quantile(x, probs = probs, na.rm = TRUE))
  k <- unique(k)
  # Un noeud pose sur une borne ne partage rien : `ns()` le refuse.
  k <- k[k > min(x) & k < max(x)]
  msg <- if (length(k) < length(probs))
    trf("Nœuds de la variable d'exposition : %d demandé(s), %d retenu(s) — des percentiles coïncident sur cette série, la souplesse du modèle est donc plus faible que demandée.",
        length(probs), length(k)) else NULL
  list(noeuds = k, message = msg)
}

# LE DENOMINATEUR NE S'INVENTE PAS

# UNE DATE COMPLETE SE DECOMPOSE, ET C'EST DEMANDE, JAMAIS FAIT D'OFFICE
# Le chemin inverse du couple mois + annee.
HSTAT_EPI_PARTIES <- c(
  "Année (entier)"                          = "annee",
  "Mois (facteur 01-12)"                    = "mois",
  "Jour du mois (entier)"                   = "jour",
  "Jour de la semaine (facteur, lundi en tête)" = "sem")

# Suffixe de colonne, stable : c'est lui que relisent l'export et les autres
# onglets.
HSTAT_EPI_PARTIES_SUFFIXE <- c(annee = "_annee", mois = "_mois",
                               jour = "_jour", sem = "_jour_sem")

#' Décompose une colonne de date complète en année, mois, jour et jour de
#' semaine.
#'
#' Rend toujours une liste : `data` (le tableau, colonnes AJOUTEES), `ajoutees`
#' (les noms réellement créés), `n_ok`, et `message`.
hstat_epi_date_parts <- function(data, var_date,
                                 parties = c("annee", "mois", "jour"),
                                 lang = hstat_langue_session()) {
  vide <- list(data = data, ajoutees = character(0), n_ok = 0L, message = NULL)
  if (!is.data.frame(data) || !NROW(data)) return(vide)
  if (!isTRUE(nzchar(var_date %||% "")) || !var_date %in% names(data)) {
    vide$message <- tr("Choisissez d'abord la colonne de date complète à décomposer.")
    return(vide)
  }
  parties <- intersect(as.character(parties %||% character(0)),
                       unname(HSTAT_EPI_PARTIES))
  if (!length(parties)) {
    vide$message <- tr("Choisissez au moins un élément à extraire (année, mois, jour).")
    return(vide)
  }

  da <- hstat_date_auto(data[[var_date]])
  d  <- da$dates
  n_ok <- sum(!is.na(d))
  if (!n_ok) {
    vide$message <- trf("La colonne « %s » n'a pu être lue comme une date : rien n'a été extrait.",
                        var_date)
    return(vide)
  }
  msg <- character(0)
  if (isTRUE(da$auto) && isTRUE(nzchar(da$format %||% "")))
    msg <- c(msg, trf("Format de date reconnu automatiquement : %s.", da$format))
  # UNE LIGNE ILLISIBLE NE DISPARAIT PAS EN SILENCE.
  if (n_ok < length(d))
    msg <- c(msg, trf("%d ligne(s) sur %d n'ont pas pu être lues comme une date : les colonnes extraites y valent NA.",
                      length(d) - n_ok, length(d)))

  jours <- HSTAT_JOURS[[.hstat_lang2(lang)]]
  valeur <- function(p) switch(p,
    annee = as.integer(format(d, "%Y")),
    jour  = as.integer(format(d, "%d")),
    # Facteur NON ordonne, niveaux en ordre chronologique : voir l'en-tete.
    mois  = factor(format(d, "%m"), levels = sprintf("%02d", 1:12)),
    sem   = factor(jours[as.integer(format(d, "%u"))], levels = jours))

  ajout <- character(0)
  for (p in parties) {
    base <- paste0(var_date, HSTAT_EPI_PARTIES_SUFFIXE[[p]])
    # LA COLONNE D'ORIGINE EST CONSERVEE, et une homonyme aussi.
    nom <- make.unique(c(names(data), base))[length(names(data)) + 1L]
    if (!identical(nom, base))
      msg <- c(msg, trf("La colonne « %s » existe déjà : l'extraction est déposée sous « %s ».",
                        base, nom))
    data[[nom]] <- valeur(p)
    ajout <- c(ajout, nom)
  }
  msg <- c(msg, trf("%d colonne(s) ajoutée(s) : %s. La colonne de date d'origine est conservée.",
                    length(ajout), paste(ajout, collapse = ", ")))
  list(data = data, ajoutees = ajout, n_ok = n_ok,
       message = paste(msg, collapse = " "))
}

# UNE DATE SE COMPOSE PARFOIS DE DEUX COLONNES
# Beaucoup de fichiers de suivi mensuel ne portent AUCUNE colonne de date : ils
# portent « Mois » en toutes lettres et « Annee » a cote.
hstat_epi_mois_num <- function(x) {
  if (is.null(x)) return(integer(0))
  n <- suppressWarnings(as.numeric(x))
  ok <- is.finite(n) & n >= 1 & n <= 12 & abs(n - round(n)) < 1e-8
  out <- ifelse(ok, as.integer(round(n)), NA_integer_)
  reste <- which(is.na(out) & !is.na(x) & nzchar(trimws(as.character(x))))
  if (length(reste)) {
    cle <- hstat_sans_accents(tolower(trimws(as.character(x)[reste])))
    mv <- .hstat_mois_variantes()
    # Correspondance EXACTE, jamais par prefixe.
    out[reste] <- mv$num[match(cle, mv$noms)]
  }
  as.integer(out)
}

#' Compose une date a partir d'une colonne de mois et d'une colonne d'annee.
#'
#' Rend toujours une liste : `dates`, `n_ok`, `ecartes` (les libelles de mois
#' non reconnus, NOMMES) et `message`.
hstat_epi_date_mois_annee <- function(mois, annee) {
  n <- max(length(mois), length(annee))
  vide <- list(dates = as.Date(rep(NA_real_, n), origin = "1970-01-01"),
               n_ok = 0L, ecartes = character(0), message = NULL)
  if (!n) return(vide)
  m <- hstat_epi_mois_num(mois)
  a <- suppressWarnings(as.numeric(annee))
  msg <- character(0)

  # UNE ANNEE SUR DEUX CHIFFRES EST AMBIGUE, et la trancher en silence ferait
  # lire « 05 » comme l'an 5 ou comme 2005 selon la convention choisie -- deux
  # series distantes de deux millenaires, toutes deux plausibles en tableau.
  deux <- is.finite(a) & a >= 0 & a < 100
  if (any(deux, na.rm = TRUE))
    msg <- c(msg, tr("La colonne d'année porte des valeurs à deux chiffres : elles ne sont pas interprétées, écrivez l'année en entier (2005 et non 05)."))
  ok_a <- is.finite(a) & a >= 100 & abs(a - round(a)) < 1e-8

  inconnus <- unique(as.character(mois)[is.na(m) & !is.na(mois) &
                                          nzchar(trimws(as.character(mois)))])
  if (length(inconnus))
    msg <- c(msg, trf("Mois non reconnu(s) : %s. Les noms français et anglais, leurs abréviations et les numéros 1 à 12 sont acceptés.",
                      paste(utils::head(inconnus, 8), collapse = ", ")))

  ok <- !is.na(m) & ok_a
  d <- as.Date(rep(NA_real_, n), origin = "1970-01-01")
  if (any(ok))
    d[ok] <- as.Date(sprintf("%04d-%02d-01", as.integer(round(a[ok])), m[ok]))
  if (any(ok))
    msg <- c(msg, tr("Date composée depuis le mois et l'année : le jour est fixé au premier du mois, une donnée mensuelle n'en portant pas."))

  list(dates = d, n_ok = sum(ok), ecartes = inconnus,
       message = if (length(msg)) paste(msg, collapse = " ") else NULL)
}

# LE TEMPS SE RESOUT A UN SEUL ENDROIT
# Trois sources possibles -- une colonne de date, un couple mois + annee, ou
# rien (l'ordre des lignes).
hstat_epi_temps <- function(data, var_temps = NULL, var_mois = NULL,
                            var_annee = NULL) {
  n <- NROW(data)
  msg <- character(0)
  a_col <- function(v) isTRUE(nzchar(v %||% "")) && v %in% names(data)

  if (a_col(var_mois) && a_col(var_annee)) {
    r <- hstat_epi_date_mois_annee(data[[var_mois]], data[[var_annee]])
    if (!is.null(r$message)) msg <- c(msg, r$message)
    if (r$n_ok > 0)
      return(list(dates = r$dates, source = "mois_annee",
                  message = if (length(msg)) paste(msg, collapse = " ") else NULL))
    msg <- c(msg, tr("Aucune date n'a pu être composée depuis le mois et l'année : les retards sont comptés sur l'ordre des lignes."))
    return(list(dates = NULL, source = "rang",
                message = paste(msg, collapse = " ")))
  }
  # UN SEUL DES DEUX CHAMPS NE SUFFIT PAS, et le taire laisserait croire que la
  # date a ete composee alors que le module est retombe sur l'ordre des lignes.
  if (a_col(var_mois) || a_col(var_annee))
    msg <- c(msg, tr("Le mois et l'année se déclarent ensemble : un seul des deux ne compose aucune date."))

  if (a_col(var_temps)) {
    da <- hstat_date_auto(data[[var_temps]])
    if (any(!is.na(da$dates))) {
      if (isTRUE(da$auto) && isTRUE(nzchar(da$format %||% "")))
        msg <- c(msg, trf("Format de date reconnu automatiquement : %s.", da$format))
      if (length(da$ambigu) > 1L)
        msg <- c(msg, trf("Plusieurs formats de date relisent cette colonne (%s) : déclarez-le dans l'onglet Nettoyage si la lecture retenue n'est pas la bonne.",
                          paste(da$ambigu, collapse = ", ")))
      return(list(dates = da$dates, source = "colonne",
                  message = if (length(msg)) paste(msg, collapse = " ") else NULL))
    }
    # Une colonne numerique de rang (1, 2, 3...) est un temps legitime : on la
    # garde telle quelle plutot que d'exiger une date.
    num <- suppressWarnings(as.numeric(data[[var_temps]]))
    if (any(is.finite(num)))
      return(list(dates = NULL, rang = num, source = "numerique",
                  message = if (length(msg)) paste(msg, collapse = " ") else NULL))
    msg <- c(msg, tr("La colonne de temps n'a pu être lue ni comme une date ni comme un nombre : les retards sont comptés sur l'ordre des lignes."))
  } else {
    msg <- c(msg, tr("Aucune colonne de temps déclarée : les retards sont comptés sur l'ordre des lignes du fichier."))
  }
  list(dates = NULL, source = "rang",
       message = if (length(msg)) paste(msg, collapse = " ") else NULL)
}

# L'usage courant ecrit `offset = log(N + 1)` pour eviter log(0).
.hstat_epi_offset <- function(off) {
  if (is.null(off)) return(list(log_off = NULL, garde = NULL, ecartes = 0L))
  m <- if (is.data.frame(off) || is.matrix(off)) as.data.frame(off)
       else data.frame(x = off)
  if (!NCOL(m)) return(list(log_off = NULL, garde = NULL, ecartes = 0L))
  o <- vapply(m, function(v) suppressWarnings(as.numeric(v)), numeric(NROW(m)))
  o <- matrix(o, nrow = NROW(m))
  ok <- apply(is.finite(o) & o > 0, 1, all)
  lg <- rowSums(log(pmax(o, .Machine$double.xmin)))
  list(log_off = ifelse(ok, lg, NA_real_), garde = ok, ecartes = sum(!ok))
}

# Saison et tendance : des harmoniques de Fourier pour le cycle, une spline du
# temps pour la derive longue.
.hstat_epi_saison <- function(temps, periode, harmoniques) {
  h <- as.integer(max(0L, min(6L, harmoniques)))
  if (h < 1L || !isTRUE(is.finite(periode)) || periode <= 1) return(NULL)
  out <- lapply(seq_len(h), function(k) {
    cbind(sin(2 * pi * k * temps / periode), cos(2 * pi * k * temps / periode))
  })
  m <- do.call(cbind, out)
  colnames(m) <- as.vector(rbind(paste0("sin", seq_len(h)), paste0("cos", seq_len(h))))
  m
}

# UNE SEULE EXPOSITION NE SUFFIT JAMAIS
.hstat_epi_cb <- function(x, L, pct_noeuds, nk_lag, nom) {
  # ZERO NOEUD DEMANDE N'EST PAS ZERO NOEUD SUBI.
  lineaire <- !length(pct_noeuds)
  nd <- if (lineaire) list(noeuds = numeric(0), message = NULL)
        else .hstat_epi_noeuds(x, pct_noeuds)
  if (!lineaire && !length(nd$noeuds))
    return(list(ok = FALSE,
                message = trf("« %s » ne varie pas assez pour poser un nœud : aucune surface n'y est estimable.", nom)))
  cb <- tryCatch(
    dlnm::crossbasis(x, lag = L,
                     argvar = if (lineaire) list(fun = "lin")
                              else list(fun = "ns", knots = nd$noeuds),
                     arglag = list(fun = "ns",
                                   knots = if (L >= 2L)
                                     dlnm::logknots(L, nk = max(1L, min(as.integer(nk_lag), L - 1L)))
                                   else NULL)),
    error = function(e) e)
  if (inherits(cb, "error"))
    return(list(ok = FALSE,
                message = trf("« %s » : surface impossible (%s)", nom, hstat_err_fr(cb))))
  list(ok = TRUE, cb = cb, noeuds = nd$noeuds,
       message = if (!is.null(nd$message)) paste0(nom, " — ", nd$message) else NULL)
}

# DEUX EXPOSITIONS TRES CORRELEES SE PARTAGENT L'EFFET, ET AUCUNE NE RESSORT
# Temperature maximale et humidite relative correlent souvent a -0,8 ; en
# saison seche, temperature et pluviometrie tout autant.
hstat_epi_collinearite <- function(data, vars, seuil = 0.7) {
  vars <- intersect(as.character(vars %||% character(0)), names(data))
  if (length(vars) < 2L) return(NULL)
  m <- vapply(vars, function(v) suppressWarnings(as.numeric(data[[v]])),
              numeric(nrow(data)))
  cm <- suppressWarnings(stats::cor(m, use = "pairwise.complete.obs"))
  pa <- utils::combn(seq_along(vars), 2)
  r <- vapply(seq_len(ncol(pa)), function(k) cm[pa[1, k], pa[2, k]], numeric(1))
  d <- data.frame(Variable_1 = vars[pa[1, ]], Variable_2 = vars[pa[2, ]],
                  Correlation = round(r, 4), check.names = FALSE,
                  stringsAsFactors = FALSE)
  d <- d[order(-abs(d$Correlation)), , drop = FALSE]
  fortes <- d[is.finite(d$Correlation) & abs(d$Correlation) >= seuil, , drop = FALSE]
  msg <- if (nrow(fortes))
    trf("Expositions fortement corrélées (|r| ≥ %.2f) : %s. Leur effet se partage de façon instable : les intervalles s'élargissent et les deux peuvent ressortir « non significatives » alors que leur effet conjoint est net. Retirez-en une, ou interprétez-les ensemble.",
        seuil, paste(sprintf("%s/%s (r = %.2f)", fortes$Variable_1, fortes$Variable_2,
                             fortes$Correlation), collapse = " ; ")) else NULL
  structure(d, message = msg, fortes = nrow(fortes))
}

#' Ajuste une surface exposition-retard-reponse.
#'
#' Rend TOUJOURS une liste ; `ok = FALSE` porte le motif. Faire lever la
#' fonction ferait tomber la sortie Shiny entiere, la ou un motif se lit.
hstat_epi_dlnm <- function(data, var_y, var_expo, var_offset = NULL,
                           var_temps = NULL, lag_max = 5L,
                           pct_noeuds = c(0.25, 0.50, 0.75),
                           nk_lag = 2L, reference = NULL,
                           famille = "auto", periode = 12,
                           harmoniques = 2L, df_tendance = 3L,
                           conf = 0.95, vars_ajust_cb = NULL,
                           vars_ajust = NULL, var_mois = NULL,
                           var_annee = NULL, proxy_a = 1, proxy_b = 0) {
  msg <- character(0)
  ko <- function(m) list(ok = FALSE, message = m)

  if (!requireNamespace("dlnm", quietly = TRUE))
    return(ko(hstat_pkg_manquant("dlnm", tr("l'analyse exposition-retard"))))
  if (is.null(data) || !nrow(data)) return(ko(tr("Aucune donnée.")))
  for (v in c(var_y, var_expo))
    if (!isTRUE(nzchar(v %||% "")) || !v %in% names(data))
      return(ko(trf("Colonne introuvable : %s", as.character(v %||% "(non choisie)"))))
  if (identical(var_y, var_expo))
    return(ko(tr("L'issue et l'exposition doivent être deux colonnes différentes.")))

  # UNE EXPOSITION D'AJUSTEMENT NE PEUT ETRE NI L'ISSUE NI L'EXPOSITION
  # PRINCIPALE.
  vars_ajust_cb <- setdiff(intersect(as.character(vars_ajust_cb %||% character(0)),
                                     names(data)), c(var_y, var_expo))
  vars_ajust <- setdiff(intersect(as.character(vars_ajust %||% character(0)),
                                  names(data)), c(var_y, var_expo, vars_ajust_cb))

  y <- suppressWarnings(as.numeric(data[[var_y]]))
  x <- suppressWarnings(as.numeric(data[[var_expo]]))
  # LA MESURE D'AMBIANCE SE RAMENE A L'EXPOSITION SUBIE AVANT TOUT LE RESTE.
  px <- hstat_epi_proxy(x, proxy_a, proxy_b, nom = var_expo)
  x <- px$x
  if (!is.null(px$message)) msg <- c(msg, px$message)
  n <- length(y)

  # UN DECALAGE NEGATIF OU ILLISIBLE EST UNE SAISIE, PAS UNE DEMANDE.
  Lb <- suppressWarnings(as.numeric(lag_max)[1])
  if (isTRUE(is.finite(Lb)) && Lb < 0)
    return(ko(trf("Décalage maximal négatif (%s) : il compte des périodes, il ne peut pas être négatif.",
                  format(Lb))))
  if (!isTRUE(is.finite(Lb))) {
    Lb <- 5
    msg <- c(msg, tr("Décalage maximal illisible : 5 périodes retenues."))
  }
  L <- as.integer(max(0L, min(60L, Lb)))
  # LA SERIE DOIT ETRE NETTEMENT PLUS LONGUE QUE LE DECALAGE.
  if (n < L + 12L)
    return(ko(trf("Série trop courte : %d observations pour un décalage de %d. Il en faut au moins %d (réduisez le décalage).",
                  n, L, L + 12L)))

  # Le temps : une colonne declaree, ou le rang.
  tt <- seq_len(n)
  rt <- hstat_epi_temps(data, var_temps, var_mois, var_annee)
  if (!is.null(rt$message)) msg <- c(msg, rt$message)
  brut <- if (!is.null(rt$dates)) suppressWarnings(as.numeric(rt$dates)) else rt$rang
  if (!is.null(brut) && !all(is.na(brut))) {
    if (is.unsorted(brut, na.rm = TRUE))
      msg <- c(msg, tr("Les lignes ne sont pas triées par date : un retard se compte sur des observations consécutives, triez le fichier avant l'analyse."))
    pas <- stats::median(diff(sort(unique(brut))), na.rm = TRUE)
    tt <- if (isTRUE(is.finite(pas)) && pas > 0)
      (brut - min(brut, na.rm = TRUE)) / pas + 1 else as.numeric(brut)
  }

  # La surface principale, puis celles d'ajustement.
  pr <- .hstat_epi_cb(x, L, pct_noeuds, nk_lag, var_expo)
  if (!isTRUE(pr$ok)) return(ko(pr$message))
  if (!is.null(pr$message)) msg <- c(msg, pr$message)
  cbs <- list(); cbs[[var_expo]] <- pr$cb
  ecartees <- character(0)
  for (v in vars_ajust_cb) {
    a <- .hstat_epi_cb(suppressWarnings(as.numeric(data[[v]])), L, pct_noeuds, nk_lag, v)
    if (!isTRUE(a$ok)) { ecartees <- c(ecartees, v); msg <- c(msg, a$message); next }
    if (!is.null(a$message)) msg <- c(msg, a$message)
    cbs[[v]] <- a$cb
  }
  vars_ajust_cb <- setdiff(vars_ajust_cb, ecartees)

  # La collinearite se mesure sur TOUTES les expositions retenues, la principale
  # comprise : c'est entre elles que l'effet se partage.
  coll <- hstat_epi_collinearite(data, c(var_expo, vars_ajust_cb))
  if (!is.null(coll) && !is.null(attr(coll, "message")))
    msg <- c(msg, attr(coll, "message"))

  vo <- intersect(as.character(var_offset %||% character(0)), names(data))
  vo <- vo[nzchar(vo)]
  of <- .hstat_epi_offset(if (length(vo)) data[, vo, drop = FALSE] else NULL)
  if (of$ecartes > 0)
    msg <- c(msg, trf("%d observation(s) écartée(s) : dénominateur nul, négatif ou manquant. Un mois sans dénominateur n'informe aucun taux — il n'est pas remplacé par 1, ce qui fabriquerait le taux de ce mois.",
                      of$ecartes))

  sais <- .hstat_epi_saison(tt, periode, harmoniques)
  dft <- as.integer(max(0L, min(20L, suppressWarnings(as.numeric(df_tendance)[1]))))
  if (!isTRUE(is.finite(dft))) dft <- 3L
  tend <- if (dft >= 1L && n > dft + 2L)
    tryCatch(splines::ns(tt, df = dft), error = function(e) NULL) else NULL
  if (dft >= 1L && is.null(tend))
    msg <- c(msg, tr("Tendance longue non estimable : la spline du temps a été retirée."))

  dd <- data.frame(.y = y)
  if (!is.null(sais)) dd <- cbind(dd, as.data.frame(sais))
  if (!is.null(tend)) {
    tn <- as.data.frame(unclass(tend))
    names(tn) <- paste0("tend", seq_len(ncol(tn)))
    dd <- cbind(dd, tn)
  }
  if (length(vars_ajust)) {
    av <- data[, vars_ajust, drop = FALSE]
    names(av) <- paste0("aj", seq_along(vars_ajust))
    dd <- cbind(dd, av)
    ajn <- stats::setNames(vars_ajust, names(av))
  } else ajn <- character(0)
  if (!is.null(of$log_off)) dd$.logoff <- of$log_off

  # LA GARDE PAR OBSERVATION, PAS PAR MODELE : une ligne sans denominateur ne
  # doit pas emporter la serie entiere.
  garde <- stats::complete.cases(dd) & is.finite(y)
  if (!is.null(of$garde)) garde <- garde & of$garde
  if (sum(garde) < L + 12L)
    return(ko(trf("Après retrait des lignes incomplètes il ne reste que %d observation(s) : trop peu pour un décalage de %d.",
                  sum(garde), L)))

  # Les surfaces entrent dans la formule sous des noms STABLES (`cb1`, `cb2`…)
  # et non sous le nom de la colonne.
  cbn <- sprintf("cb%0*d", nchar(length(cbs)), seq_along(cbs))
  env <- new.env(parent = parent.frame())
  for (k in seq_along(cbs)) assign(cbn[k], cbs[[k]], envir = env)
  # L'OFFSET ENTRE PAR LA FORMULE, JAMAIS PAR L'ARGUMENT `offset =`.
  pred_noms <- setdiff(names(dd), c(".y", ".logoff"))
  fml <- stats::as.formula(paste(".y ~", paste(cbn, collapse = " + "),
                                 if (length(pred_noms)) paste("+", paste(pred_noms, collapse = " + ")) else "",
                                 if (!is.null(of$log_off)) "+ offset(.logoff)" else ""))
  environment(fml) <- env

  compte <- all(y[garde] >= 0, na.rm = TRUE) &&
    all(abs(y[garde] - round(y[garde])) < 1e-8, na.rm = TRUE)
  if (!compte)
    msg <- c(msg, tr("L'issue n'est pas un décompte d'événements (valeurs non entières ou négatives) : les rapports de risque se lisent alors avec prudence."))

  ajuster <- function(fam) {
    a <- list(formula = fml, data = dd, subset = garde, na.action = stats::na.exclude)
    tryCatch({
      if (identical(fam, "nb")) {
        if (!requireNamespace("MASS", quietly = TRUE)) return(NULL)
        suppressWarnings(do.call(MASS::glm.nb, a))
      } else {
        a$family <- switch(fam, poisson = stats::poisson(),
                           quasipoisson = stats::quasipoisson(),
                           stats::poisson())
        suppressWarnings(do.call(stats::glm, a))
      }
    }, error = function(e) NULL)
  }

  fam_dem <- if (famille %in% c("auto", "poisson", "quasipoisson", "nb")) famille else "auto"
  if (identical(fam_dem, "auto")) {
    m_p <- ajuster("poisson"); m_n <- ajuster("nb")
    # L'AIC N'EST PAS DEFINI POUR UN QUASI-MODELE.
    a_p <- if (!is.null(m_p)) stats::AIC(m_p) else NA_real_
    a_n <- if (!is.null(m_n)) suppressWarnings(stats::AIC(m_n)) else NA_real_
    d_p <- if (!is.null(m_p)) .hstat_epi_disp(m_p) else NA_real_
    ch <- hstat_epi_famille_auto(a_p, a_n, d_p, nb_dispo = !is.null(m_n))
    fam_ret <- ch$famille
    fit <- switch(fam_ret, nb = m_n, quasipoisson = ajuster("quasipoisson"), m_p)
    # SI LE REPLI NE S'AJUSTE PAS, on garde Poisson plutot que de rendre
    # `NULL`.
    if (is.null(fit)) { fit <- m_p; fam_ret <- "poisson" }
    if (isTRUE(is.finite(a_p)) && isTRUE(is.finite(a_n)))
      msg <- c(msg, trf("Choix automatique par AIC : Poisson %.1f, binomiale négative %.1f.", a_p, a_n))
    if (identical(fam_ret, "quasipoisson"))
      msg <- c(msg, trf("Surdispersion mesurée (%.2f) : quasi-Poisson retenue plutôt que Poisson, dont les intervalles seraient trop étroits.", d_p))
  } else {
    fit <- ajuster(fam_dem); fam_ret <- fam_dem
  }
  if (is.null(fit)) return(ko(tr("L'ajustement du modèle a échoué : essayez un décalage plus court ou moins de nœuds.")))

  # LA SURDISPERSION SE MESURE, ELLE NE SE SUPPOSE PAS.
  disp <- .hstat_epi_disp(fit)
  if (identical(fam_ret, "poisson") && isTRUE(is.finite(disp)) && disp > HSTAT_EPI_DISP_SEUIL)
    msg <- c(msg, trf("Surdispersion détectée (variance/moyenne des résidus = %.2f) : les intervalles de la loi de Poisson sont trop étroits. Choisissez quasi-Poisson ou la binomiale négative.", disp))

  # UNE REFERENCE HORS DE L'ETENDUE OBSERVEE REND DES RR DE 0 OU D'INF.
  ref <- suppressWarnings(as.numeric(reference)[1])
  bornes <- range(x, na.rm = TRUE)
  if (isTRUE(is.finite(ref)) && (ref < bornes[1] || ref > bornes[2])) {
    ref0 <- ref
    ref <- min(max(ref, bornes[1]), bornes[2])
    msg <- c(msg, trf("Référence %s hors de l'étendue observée [%s ; %s] : ramenée à %s. Au-delà, la surface extrapole sans borne et le rapport de risque part vers 0 ou l'infini.",
                      format(ref0, digits = 4), format(bornes[1], digits = 4),
                      format(bornes[2], digits = 4), format(ref, digits = 4)))
  }
  if (!isTRUE(is.finite(ref))) ref <- unname(stats::quantile(x, 0.50, na.rm = TRUE))

  # LA PREDICTION SE FAIT SURFACE PAR SURFACE.
  faire_pred <- function(k) {
    xv <- if (k == 1L) x else suppressWarnings(as.numeric(data[[names(cbs)[k]]]))
    cen <- if (k == 1L) ref else unname(stats::quantile(xv, 0.50, na.rm = TRUE))
    at <- seq(min(xv, na.rm = TRUE), max(xv, na.rm = TRUE), length.out = 100)
    ap <- as.call(list(quote(dlnm::crosspred), basis = as.name(cbn[k]),
                       model = fit, at = at, cen = cen, cumul = TRUE,
                       ci.level = conf))
    tryCatch(eval(ap, envir = env), error = function(e) NULL)
  }
  pred <- faire_pred(1L)
  if (is.null(pred))
    return(ko(tr("Prédiction impossible sur l'exposition principale : réduisez le nombre de nœuds ou le décalage.")))
  preds_aj <- list()
  for (k in seq_along(cbs)[-1]) {
    p <- faire_pred(k)
    if (!is.null(p)) preds_aj[[names(cbs)[k]]] <- p
  }

  list(ok = TRUE, pred = pred, preds_ajust = preds_aj, model = fit, cb = cbs[[1]],
       cbs = cbs, cb_noms = stats::setNames(cbn, names(cbs)),
       famille = fam_ret, reference = ref, lag_max = L, noeuds = pr$noeuds,
       expo = x, var_expo = var_expo, vars_ajust_cb = vars_ajust_cb,
       proxy = list(a = px$a, b = px$b, applique = px$applique),
       vars_ajust = ajn, collinearite = coll,
       dispersion = disp, n_utilisees = sum(garde), conf = conf,
       aic = tryCatch(suppressWarnings(stats::AIC(fit)), error = function(e) NA_real_),
       dev_expl = tryCatch(100 * (1 - fit$deviance / fit$null.deviance),
                           error = function(e) NA_real_),
       percentiles = stats::quantile(x, HSTAT_EPI_DLNM_PCT, na.rm = TRUE),
       message = if (length(msg)) paste(msg, collapse = " ") else NULL)
}

# TOUS LES SCENARIOS : chaque exposition tour a tour, et la comparaison
# Un essai porte rarement une seule question.
hstat_epi_dlnm_multi <- function(data, vars_y, vars_expo, mutuel = TRUE,
                                 proxy_a = NULL, proxy_b = NULL, ...) {
  vars_y <- intersect(as.character(vars_y %||% character(0)), names(data))
  if (!length(vars_y))
    return(list(ok = FALSE, message = tr("Choisissez au moins une issue à analyser.")))
  vars_expo <- intersect(as.character(vars_expo %||% character(0)), names(data))
  vars_expo <- setdiff(vars_expo, vars_y)
  if (!length(vars_expo))
    return(list(ok = FALSE, message = tr("Choisissez au moins une variable d'exposition.")))
  plusieurs <- length(vars_y) > 1L
  res <- list(); echecs <- character(0)
  for (y in vars_y) for (v in vars_expo) {
    cle <- if (plusieurs) sprintf("%s \u2014 %s", y, v) else v
    autres <- if (isTRUE(mutuel)) setdiff(vars_expo, v) else character(0)
    # LE FACTEUR DE TRANSFERT EST PROPRE A CHAQUE EXPOSITION.
    pa <- suppressWarnings(as.numeric(proxy_a[[v]] %||% 1)[1])
    pb <- suppressWarnings(as.numeric(proxy_b[[v]] %||% 0)[1])
    r <- tryCatch(hstat_epi_dlnm(data, y, v, vars_ajust_cb = autres,
                                 proxy_a = pa, proxy_b = pb, ...),
                  error = function(e) list(ok = FALSE, message = hstat_err_fr(e)))
    if (isTRUE(r$ok)) { r$var_y <- y; r$cle <- cle; res[[cle]] <- r }
    else echecs <- c(echecs, sprintf("%s : %s", cle,
      r$message %||% tr("L'ajustement n'a pas abouti.")))
  }
  if (!length(res))
    return(list(ok = FALSE,
                message = paste(c(tr("Aucune exposition n'a pu être analysée."), echecs), collapse = " ")))
  list(ok = TRUE, resultats = res, mutuel = isTRUE(mutuel),
       vars_y = vars_y, plusieurs_issues = plusieurs,
       echecs = echecs,
       comparaison = hstat_epi_dlnm_comparaison(res),
       message = if (length(echecs))
         trf("%d exposition(s) écartée(s) : %s", length(echecs), paste(echecs, collapse = " ; "))
         else NULL)
}

# Le tableau qui met les expositions cote a cote : effet cumule au 90e et au
# 10e percentile, reference la mediane de CHAQUE exposition.
hstat_epi_dlnm_comparaison <- function(res_list) {
  if (!length(res_list)) return(NULL)
  cles <- names(res_list)
  if (is.null(cles) || !all(nzchar(cles)))
    cles <- vapply(seq_along(res_list), function(i)
      res_list[[i]]$var_expo %||% as.character(i), character(1))
  out <- lapply(seq_along(cles), function(i) {
    v <- cles[[i]]
    r <- res_list[[i]]
    if (!isTRUE(r$ok)) return(NULL)
    tb <- hstat_epi_dlnm_rr(r, c(0.10, 0.90))
    if (is.null(tb) || !nrow(tb)) return(NULL)
    w <- hstat_epi_dlnm_wald(r)
    data.frame(
      Issue = r$var_y %||% NA_character_,
      Exposition = r$var_expo %||% v, Famille = r$famille,
      Reference = round(r$reference, 4),
      RR_P10 = tb$RR[1], IC_P10 = sprintf("[%.3f ; %.3f]", tb$IC_bas[1], tb$IC_haut[1]),
      RR_P90 = tb$RR[2], IC_P90 = sprintf("[%.3f ; %.3f]", tb$IC_bas[2], tb$IC_haut[2]),
      Test_global_p = if (!is.null(w)) round(w$p, 6) else NA_real_,
      Verdict = if (!is.null(w)) w$verdict else tr("indéterminable"),
      AIC = round(r$aic, 2), check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(NULL)
  d <- do.call(rbind, out)
  # UNE COLONNE CONSTANTE N'APPREND RIEN : avec une seule issue, « Issue »
  # repeterait le meme nom sur chaque ligne et ferait croire a un choix.
  if (!is.null(d) && length(unique(d$Issue)) < 2L) d$Issue <- NULL
  d
}

# UN RETARD SE DESIGNE PAR SA VALEUR, JAMAIS PAR SON RANG DE COLONNE
# `crosspred(..., bylag = 0.5)` rend une colonne tous les DEMI-retards :
# colonne 1 = lag 0, colonne 2 = lag 0,5, colonne 3 = lag 1...
.hstat_epi_lag_grille <- function(pred) {
  g <- suppressWarnings(as.numeric(sub("^lag", "", colnames(pred$matRRfit))))
  if (length(g) == ncol(pred$matRRfit) && all(is.finite(g))) return(g)
  # Repli : la grille se reconstruit sur l'etendue et le nombre de colonnes.
  seq(pred$lag[1], pred$lag[2], length.out = ncol(pred$matRRfit))
}

.hstat_epi_col_lag <- function(pred, lag) {
  g <- .hstat_epi_lag_grille(pred)
  i <- which.min(abs(g - lag))
  # UN RETARD DEMANDE QUI N'EXISTE PAS DANS LA GRILLE N'EST PAS RENDU DE
  # TRAVERS : il est rendu absent.
  if (!length(i) || !isTRUE(abs(g[i] - lag) < 1e-6)) return(NA_integer_)
  i
}

# Effets CUMULES sur tous les retards, lus aux percentiles de l'exposition.
hstat_epi_dlnm_rr <- function(res, probs = HSTAT_EPI_DLNM_PCT) {
  if (!isTRUE(res$ok)) return(NULL)
  p <- res$pred
  q <- stats::quantile(res$expo, probs = probs, na.rm = TRUE)
  idx <- vapply(q, function(v) which.min(abs(p$predvar - v)), integer(1))
  verdict <- vapply(seq_along(idx), function(k) {
    lo <- p$allRRlow[idx[k]]; hi <- p$allRRhigh[idx[k]]
    # NE JAMAIS BRANCHER SUR UNE STATISTIQUE NON CALCULABLE : un intervalle
    # incalculable ne prouve rien, ni dans un sens ni dans l'autre.
    if (!isTRUE(is.finite(lo)) || !isTRUE(is.finite(hi))) tr("indéterminable")
    else if (lo > 1) tr("risque accru")
    else if (hi < 1) tr("risque réduit")
    else tr("non significatif")
  }, character(1))
  data.frame(
    Percentile = paste0("P", formatC(probs * 100, format = "fg")),
    Exposition = unname(round(q, 4)),
    RR         = unname(round(p$allRRfit[idx], 4)),
    IC_bas     = unname(round(p$allRRlow[idx], 4)),
    IC_haut    = unname(round(p$allRRhigh[idx], 4)),
    Verdict    = verdict,
    check.names = FALSE, stringsAsFactors = FALSE)
}

# Effets PAR RETARD a une exposition donnee.
hstat_epi_dlnm_lags <- function(res, at = NULL, prob = 0.90) {
  if (!isTRUE(res$ok)) return(NULL)
  p <- res$pred
  v <- suppressWarnings(as.numeric(at)[1])
  if (!isTRUE(is.finite(v))) v <- unname(stats::quantile(res$expo, prob, na.rm = TRUE))
  i <- which.min(abs(p$predvar - v))
  lags <- seq(0, res$lag_max, by = 1)
  cols <- vapply(lags, function(l) .hstat_epi_col_lag(p, l), integer(1))
  ok <- !is.na(cols)
  if (!any(ok)) return(NULL)
  fit <- p$matRRfit[i, cols[ok]]; lo <- p$matRRlow[i, cols[ok]]; hi <- p$matRRhigh[i, cols[ok]]
  verdict <- vapply(seq_along(fit), function(k) {
    if (!isTRUE(is.finite(lo[k])) || !isTRUE(is.finite(hi[k]))) tr("indéterminable")
    else if (lo[k] > 1) tr("risque accru")
    else if (hi[k] < 1) tr("risque réduit")
    else tr("non significatif")
  }, character(1))
  structure(data.frame(
    Retard  = lags[ok], RR = unname(round(fit, 4)),
    IC_bas  = unname(round(lo, 4)), IC_haut = unname(round(hi, 4)),
    Verdict = verdict, check.names = FALSE, stringsAsFactors = FALSE),
    exposition = unname(p$predvar[i]))
}

# LE TEST GLOBAL SE FAIT SUR LES COEFFICIENTS, PAS PAR UN REAJUSTEMENT
# `lmtest::waldtest(fit, .
hstat_epi_dlnm_wald <- function(res, exposition = NULL) {
  if (!isTRUE(res$ok)) return(NULL)
  b <- stats::coef(res$model)
  V <- tryCatch(stats::vcov(res$model), error = function(e) NULL)
  if (is.null(V)) return(NULL)
  nom <- res$cb_noms[[exposition %||% res$var_expo %||% names(res$cb_noms)[1]]]
  if (is.null(nom)) return(NULL)
  idx <- grep(paste0("^", nom, "v"), names(b))
  idx <- idx[is.finite(b[idx])]
  if (!length(idx)) return(NULL)
  bb <- b[idx]; VV <- V[idx, idx, drop = FALSE]
  # Une surface sur-parametree rend une matrice singuliere : on refuse plutot
  # que de rendre un khi-deux tire d'une inversion approchee.
  W <- tryCatch(as.numeric(t(bb) %*% solve(VV) %*% bb), error = function(e) NA_real_)
  ddl <- length(bb)
  p <- if (isTRUE(is.finite(W)) && W >= 0) stats::pchisq(W, ddl, lower.tail = FALSE) else NA_real_
  list(chi2 = W, ddl = ddl, p = p, verdict = hstat_p_verdict(p))
}

# Diagnostics du modele : ce que le script d'origine trace en quatre panneaux.
hstat_epi_dlnm_diag <- function(res, dates = NULL) {
  if (!isTRUE(res$ok)) return(NULL)
  r <- stats::residuals(res$model, type = "deviance")
  f <- stats::fitted(res$model)
  n <- max(length(r), length(f))
  d <- if (!is.null(dates) && length(dates) == n) dates else seq_len(n)
  ok <- is.finite(r) & is.finite(f)
  data.frame(Ajuste = as.numeric(f)[ok], Residu = as.numeric(r)[ok],
             Temps = d[ok], check.names = FALSE, stringsAsFactors = FALSE)
}

# VARIANCE ROBUSTE (SANDWICH) -- ecrite ici, pas empruntee
# Le paquet `sandwich` est optionnel ; la variance robuste, elle, ne l'est pas :
# c'est elle qui rend le RR de la Poisson robuste utilisable.
hstat_epi_vcov_robuste <- function(fit) {
  X <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (is.null(X)) return(NULL)
  B <- tryCatch(summary(fit)$cov.unscaled, error = function(e) NULL)
  if (is.null(B)) return(NULL)
  r <- as.numeric(stats::residuals(fit, type = "response"))
  w <- as.numeric(stats::weights(fit, type = "prior"))
  if (!length(w) || all(is.na(w))) w <- rep(1, length(r))
  u <- X * (w * r)
  ok <- stats::complete.cases(u)
  M <- crossprod(u[ok, , drop = FALSE])
  V <- tryCatch(B %*% M %*% B, error = function(e) NULL)
  if (is.null(V)) return(NULL)
  dimnames(V) <- dimnames(B)
  V
}

# Un tableau de coefficients exponentiels (OR, RR, IRR, HR) avec son
# intervalle.
.hstat_epi_coefs <- function(fit, V = NULL, conf = 0.95, exposant = TRUE,
                             etiquette = "RR", retirer = "\\(Intercept\\)") {
  b <- stats::coef(fit)
  if (is.null(V)) V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(V)) return(NULL)
  se <- sqrt(diag(V))
  keep <- rep(TRUE, length(b))
  if (nzchar(retirer)) keep <- !grepl(retirer, names(b))
  b <- b[keep]; se <- se[names(b)]
  z <- suppressWarnings(stats::qnorm(1 - (1 - conf) / 2))
  lo <- b - z * se; hi <- b + z * se
  p <- suppressWarnings(2 * stats::pnorm(-abs(b / se)))
  est <- if (exposant) exp(b) else b
  l <- if (exposant) exp(lo) else lo
  h <- if (exposant) exp(hi) else hi
  out <- data.frame(Terme = names(b), Estimation = round(unname(est), 4),
                    IC_bas = round(unname(l), 4), IC_haut = round(unname(h), 4),
                    p = round(unname(p), 6),
                    Verdict = vapply(p, function(v) hstat_p_verdict(v), character(1)),
                    check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[2] <- etiquette
  out
}

# TAUX D'INCIDENCE -- le temps-personne est l'offset, jamais un predicteur
# Mettre le temps-personne en variable explicative estimerait un coefficient la
# ou la theorie en impose un (le taux est un rapport).
hstat_epi_taux <- function(data, var_y, vars_x, var_temps_personne = NULL,
                           famille = "auto", conf = 0.95, pour = 1000) {
  ko <- function(m) list(ok = FALSE, message = m)
  if (is.null(data) || !nrow(data)) return(ko(tr("Aucune donnée.")))
  vars_x <- intersect(as.character(vars_x %||% character(0)), names(data))
  if (!isTRUE(nzchar(var_y %||% "")) || !var_y %in% names(data))
    return(ko(tr("Choisissez la colonne du nombre d'événements.")))
  if (!length(vars_x)) return(ko(tr("Choisissez au moins un facteur.")))

  y <- suppressWarnings(as.numeric(data[[var_y]]))
  if (any(y < 0, na.rm = TRUE))
    return(ko(tr("Le nombre d'événements ne peut pas être négatif.")))
  d <- data.frame(.y = y, data[, vars_x, drop = FALSE], check.names = FALSE)
  msg <- character(0)

  lo <- NULL
  if (isTRUE(nzchar(var_temps_personne %||% "")) && var_temps_personne %in% names(data)) {
    o <- .hstat_epi_offset(data[[var_temps_personne]])
    lo <- o$log_off
    if (o$ecartes > 0)
      msg <- c(msg, trf("%d ligne(s) écartée(s) : temps-personne nul, négatif ou manquant — un taux n'y est pas défini.", o$ecartes))
  } else {
    msg <- c(msg, tr("Aucun temps-personne déclaré : les coefficients comparent des NOMBRES d'événements, pas des taux. Ils ne sont comparables qu'à durées d'observation égales."))
  }
  if (!is.null(lo)) d$.logoff <- lo
  garde <- stats::complete.cases(d)
  if (sum(garde) < length(vars_x) + 2L)
    return(ko(tr("Trop peu d'observations complètes pour ajuster le modèle.")))

  # L'offset passe par la FORMULE, pour la meme raison que dans le DLNM.
  fml <- stats::as.formula(paste(".y ~", paste(sprintf("`%s`", vars_x), collapse = " + "),
                                 if (!is.null(lo)) "+ offset(.logoff)" else ""))
  aj <- function(fam) {
    a <- list(formula = fml, data = d, subset = garde)
    tryCatch({
      if (identical(fam, "nb")) {
        if (!requireNamespace("MASS", quietly = TRUE)) return(NULL)
        suppressWarnings(do.call(MASS::glm.nb, a))
      } else {
        a$family <- switch(fam, quasipoisson = stats::quasipoisson(), stats::poisson())
        suppressWarnings(do.call(stats::glm, a))
      }
    }, error = function(e) NULL)
  }

  if (identical(famille, "auto")) {
    mp <- aj("poisson"); mn <- aj("nb")
    ap <- if (!is.null(mp)) stats::AIC(mp) else NA_real_
    an <- if (!is.null(mn)) suppressWarnings(stats::AIC(mn)) else NA_real_
    if (isTRUE(is.finite(an)) && (!isTRUE(is.finite(ap)) || an < ap)) {
      fit <- mn; fam <- "nb" } else { fit <- mp; fam <- "poisson" }
    if (isTRUE(is.finite(ap)) && isTRUE(is.finite(an)))
      msg <- c(msg, trf("Choix automatique par AIC : Poisson %.1f, binomiale négative %.1f.", ap, an))
  } else { fit <- aj(famille); fam <- famille }
  if (is.null(fit)) return(ko(tr("L'ajustement du modèle a échoué.")))

  disp <- tryCatch(sum(stats::residuals(fit, type = "pearson")^2, na.rm = TRUE) /
                     stats::df.residual(fit), error = function(e) NA_real_)
  if (identical(fam, "poisson") && isTRUE(is.finite(disp)) && disp > 1.5)
    msg <- c(msg, trf("Surdispersion (%.2f) : sous Poisson les intervalles sont trop étroits et des effets ressortent significatifs à tort. Choisissez quasi-Poisson ou la binomiale négative.", disp))

  list(ok = TRUE, model = fit, famille = fam, dispersion = disp,
       coefs = .hstat_epi_coefs(fit, conf = conf, etiquette = "IRR"),
       aic = tryCatch(suppressWarnings(stats::AIC(fit)), error = function(e) NA_real_),
       n = sum(garde), pour = pour, conf = conf,
       message = if (length(msg)) paste(msg, collapse = " ") else NULL)
}

# OR ET RR NE SONT PAS INTERCHANGEABLES, ET L'ECART SE MESURE
# C'est l'erreur la plus repandue de la litterature epidemiologique.
hstat_epi_risque <- function(data, var_y, vars_x, cas = NULL, conf = 0.95) {
  ko <- function(m) list(ok = FALSE, message = m)
  if (is.null(data) || !nrow(data)) return(ko(tr("Aucune donnée.")))
  vars_x <- intersect(as.character(vars_x %||% character(0)), names(data))
  if (!isTRUE(nzchar(var_y %||% "")) || !var_y %in% names(data))
    return(ko(tr("Choisissez la colonne de l'issue.")))
  if (!length(vars_x)) return(ko(tr("Choisissez au moins un facteur d'exposition.")))

  yb <- hstat_epi_binaire(data[[var_y]], cas)
  if (!isTRUE(yb$ok)) return(ko(yb$message))
  d <- data.frame(.y = yb$y, data[, vars_x, drop = FALSE], check.names = FALSE)
  garde <- stats::complete.cases(d)
  if (sum(garde) < length(vars_x) + 2L)
    return(ko(tr("Trop peu d'observations complètes.")))
  d <- d[garde, , drop = FALSE]
  msg <- character(0)
  if (length(yb$ecartes) && yb$ecartes > 0)
    msg <- c(msg, trf("%d ligne(s) écartée(s) : issue ni « cas » ni « non-cas ».", yb$ecartes))

  prev <- mean(d$.y, na.rm = TRUE)
  if (!isTRUE(prev > 0) || !isTRUE(prev < 1))
    return(ko(tr("L'issue ne prend qu'une seule valeur : aucun risque n'est estimable.")))

  fml <- stats::as.formula(paste(".y ~", paste(sprintf("`%s`", vars_x), collapse = " + ")))
  m_log <- tryCatch(suppressWarnings(stats::glm(fml, data = d, family = stats::binomial())),
                    error = function(e) NULL)
  if (is.null(m_log)) return(ko(tr("La régression logistique n'a pas pu être ajustée.")))

  # LA LOG-BINOMIALE NE CONVERGE PAS TOUJOURS, ET C'EST SA FAIBLESSE CONNUE.
  m_lb <- tryCatch(suppressWarnings(
    stats::glm(fml, data = d, family = stats::binomial(link = "log"),
               start = c(log(prev), rep(0, length(stats::coef(m_log)) - 1L)))),
    error = function(e) NULL, warning = function(w) NULL)
  lb_ok <- !is.null(m_lb) && isTRUE(m_lb$converged)

  # POISSON ROBUSTE (Zou, 2004) : la Poisson sur une issue binaire rend le bon
  # RR mais une variance TROP GRANDE.
  m_pr <- tryCatch(suppressWarnings(stats::glm(fml, data = d, family = stats::poisson())),
                   error = function(e) NULL)
  V_pr <- if (!is.null(m_pr)) hstat_epi_vcov_robuste(m_pr) else NULL

  if (!lb_ok)
    msg <- c(msg, tr("La log-binomiale n'a pas convergé (contrainte de probabilité ≤ 1) : le risque relatif affiché vient de la Poisson à variance robuste, qui estime le même RR."))

  or <- .hstat_epi_coefs(m_log, conf = conf, etiquette = "OR")
  rr_lb <- if (lb_ok) .hstat_epi_coefs(m_lb, conf = conf, etiquette = "RR") else NULL
  rr_pr <- if (!is.null(m_pr) && !is.null(V_pr))
    .hstat_epi_coefs(m_pr, V = V_pr, conf = conf, etiquette = "RR") else NULL

  # L'ecart OR/RR, terme a terme.
  ecart <- NULL
  rr_ref <- if (!is.null(rr_lb)) rr_lb else rr_pr
  if (!is.null(rr_ref)) {
    k <- match(or$Terme, rr_ref$Terme)
    ecart <- data.frame(
      Terme = or$Terme, OR = or$OR, RR = rr_ref$RR[k],
      Surestimation_pct = round(100 * (or$OR - rr_ref$RR[k]) / rr_ref$RR[k], 1),
      check.names = FALSE, stringsAsFactors = FALSE)
  }

  if (prev >= 0.10)
    msg <- c(msg, trf("Issue fréquente (%.1f %% de cas) : l'odds ratio s'écarte nettement du risque relatif. C'est le RR qu'il faut publier sous le mot « risque ».", 100 * prev))
  else
    msg <- c(msg, trf("Issue rare (%.1f %% de cas) : l'odds ratio approche correctement le risque relatif.", 100 * prev))

  list(ok = TRUE, prevalence = prev, n = nrow(d), conf = conf,
       or = or, rr = rr_ref, rr_source = if (lb_ok) "log-binomiale" else "Poisson robuste",
       rr_logbin = rr_lb, rr_poisson = rr_pr, ecart = ecart,
       model = m_log, message = paste(msg, collapse = " "))
}

# UNE ISSUE BINAIRE SE DECLARE, ELLE NE SE DEVINE PAS
# Prendre « la premiere modalite par ordre alphabetique » comme cas donnerait
# des OR parfaitement plausibles et INVERSES.
hstat_epi_binaire <- function(x, cas = NULL) {
  if (is.null(x)) return(list(ok = FALSE, message = tr("Colonne absente.")))
  if (is.logical(x)) return(list(ok = TRUE, y = as.integer(x), ecartes = sum(is.na(x)),
                                 cas = "TRUE"))
  if (isTRUE(nzchar(cas %||% ""))) {
    v <- as.character(x)
    y <- ifelse(is.na(v), NA_integer_, as.integer(v == cas))
    if (!any(y == 1L, na.rm = TRUE))
      return(list(ok = FALSE, message = trf("Aucune observation ne porte la modalité « %s » déclarée comme cas.", cas)))
    return(list(ok = TRUE, y = y, ecartes = sum(is.na(y)), cas = cas))
  }
  n <- suppressWarnings(as.numeric(x))
  u <- sort(unique(n[is.finite(n)]))
  if (length(u) == 2L && all(u %in% c(0, 1)))
    return(list(ok = TRUE, y = as.integer(n), ecartes = sum(!is.finite(n)), cas = "1"))
  list(ok = FALSE, message = tr("L'issue n'est pas codée 0/1 : déclarez explicitement la modalité qui représente le cas."))
}

# SURVIE -- Kaplan-Meier, log-rank, Cox, et l'hypothese qu'on oublie
# Au-dela de ce nombre de valeurs distinctes, une variable numerique n'est pas
# un facteur de groupement pour Kaplan-Meier et le log-rank.
HSTAT_EPI_KM_GROUPES_MAX <- 10L

hstat_epi_survie <- function(data, var_duree, var_event, vars_x = NULL,
                             event_cas = NULL, conf = 0.95) {
  ko <- function(m) list(ok = FALSE, message = m)
  if (!requireNamespace("survival", quietly = TRUE))
    return(ko(hstat_pkg_manquant("survival", tr("l'analyse de survie"))))
  if (is.null(data) || !nrow(data)) return(ko(tr("Aucune donnée.")))
  for (v in c(var_duree, var_event))
    if (!isTRUE(nzchar(v %||% "")) || !v %in% names(data))
      return(ko(tr("Choisissez la durée de suivi et l'indicateur d'événement.")))

  tps <- suppressWarnings(as.numeric(data[[var_duree]]))
  ev <- hstat_epi_binaire(data[[var_event]], event_cas)
  if (!isTRUE(ev$ok)) return(ko(ev$message))
  msg <- character(0)

  # UNE DUREE NEGATIVE OU NULLE N'EST PAS UN SUIVI.
  bad <- !is.finite(tps) | tps <= 0
  if (any(bad, na.rm = TRUE))
    msg <- c(msg, trf("%d ligne(s) écartée(s) : durée de suivi nulle, négative ou manquante.", sum(bad, na.rm = TRUE)))

  vars_x <- intersect(as.character(vars_x %||% character(0)), names(data))
  d <- data.frame(.t = tps, .e = ev$y, check.names = FALSE)
  if (length(vars_x)) d <- cbind(d, data[, vars_x, drop = FALSE])
  garde <- stats::complete.cases(d) & !bad
  if (sum(garde) < 5L) return(ko(tr("Trop peu d'observations exploitables.")))
  d <- d[garde, , drop = FALSE]
  if (!any(d$.e == 1L)) return(ko(tr("Aucun événement observé : toutes les observations sont censurées, aucune survie n'est estimable.")))

  srv <- survival::Surv(d$.t, d$.e)
  grp <- if (length(vars_x)) vars_x[1] else NULL
  # UN FACTEUR CONTINU NE FAIT PAS DES GROUPES.
  if (!is.null(grp) && is.numeric(d[[grp]]) &&
      length(unique(d[[grp]])) > HSTAT_EPI_KM_GROUPES_MAX) {
    msg <- c(msg, trf("« %s » est numérique et compte plus de %d valeurs : elle n'est pas un facteur de groupement. Elle entre au modèle de Cox ; la courbe de Kaplan-Meier et le log-rank portent sur l'ensemble. Pour comparer des groupes, découpez-la en classes.",
                      grp, HSTAT_EPI_KM_GROUPES_MAX))
    grp <- NULL
  }
  fml_km <- if (is.null(grp)) stats::as.formula("srv ~ 1") else
    stats::as.formula(sprintf("srv ~ `%s`", grp))
  km <- tryCatch(survival::survfit(fml_km, data = d, conf.int = conf),
                 error = function(e) NULL)

  # LOG-RANK : il n'existe que s'il y a au moins deux groupes A COMPARER.
  lr <- NULL
  if (!is.null(grp) && length(unique(stats::na.omit(d[[grp]]))) >= 2L) {
    sd <- tryCatch(survival::survdiff(fml_km, data = d), error = function(e) NULL)
    if (!is.null(sd)) {
      p <- stats::pchisq(sd$chisq, length(sd$n) - 1L, lower.tail = FALSE)
      lr <- list(chi2 = sd$chisq, ddl = length(sd$n) - 1L, p = p,
                 verdict = hstat_p_verdict(p))
    }
  } else if (!is.null(grp)) {
    msg <- c(msg, tr("Le facteur de groupement ne compte qu'une seule modalité : le test du log-rank est sans objet."))
  }

  cox <- ph <- NULL
  if (length(vars_x)) {
    fml_cox <- stats::as.formula(paste("srv ~", paste(sprintf("`%s`", vars_x), collapse = " + ")))
    cx <- tryCatch(survival::coxph(fml_cox, data = d), error = function(e) NULL)
    if (!is.null(cx)) {
      cox <- .hstat_epi_coefs(cx, conf = conf, etiquette = "HR", retirer = "")
      # L'HYPOTHESE DE RISQUES PROPORTIONNELS N'EST PAS UN DETAIL.
      z <- tryCatch(survival::cox.zph(cx), error = function(e) NULL)
      if (!is.null(z)) {
        tb <- as.data.frame(z$table)
        ph <- data.frame(Terme = rownames(tb), chi2 = round(tb$chisq, 4),
                         ddl = tb$df, p = round(tb$p, 6),
                         Verdict = vapply(tb$p, function(v) hstat_p_verdict(v), character(1)),
                         check.names = FALSE, stringsAsFactors = FALSE)
        pg <- tb$p[rownames(tb) == "GLOBAL"]
        if (length(pg) && isTRUE(is.finite(pg)) && pg < 0.05)
          msg <- c(msg, tr("L'hypothèse de risques proportionnels est rejetée : le rapport de risques n'est pas constant dans le temps, et le chiffre unique du modèle de Cox en est une moyenne. Stratifiez, ou modélisez l'interaction avec le temps."))
      }
      cox_model <- cx
    }
  }

  list(ok = TRUE, km = km, logrank = lr, cox = cox, ph = ph, n = nrow(d),
       evenements = sum(d$.e == 1L), groupe = grp, conf = conf,
       mediane = tryCatch(as.data.frame(summary(km)$table), error = function(e) NULL),
       donnees = d, message = if (length(msg)) paste(msg, collapse = " ") else NULL)
}

# CAS-CROISE STRATIFIE SUR LE TEMPS
# Chaque cas est son propre temoin : tout ce qui ne varie pas a l'interieur
# d'une strate.
hstat_epi_cas_croise <- function(data, var_date, var_cas, var_expo,
                                 vars_ajust = NULL, conf = 0.95) {
  ko <- function(m) list(ok = FALSE, message = m)
  if (is.null(data) || !nrow(data)) return(ko(tr("Aucune donnée.")))
  for (v in c(var_date, var_cas, var_expo))
    if (!isTRUE(nzchar(v %||% "")) || !v %in% names(data))
      return(ko(tr("Choisissez la date, le nombre de cas et l'exposition.")))

  # Meme lecteur que le DLNM, et pour la meme raison.
  dt <- data[[var_date]]
  dd <- hstat_date_auto(dt)$dates
  if (all(is.na(dd)))
    return(ko(tr("La colonne de dates n'a pas pu être lue : vérifiez son format.")))
  y <- suppressWarnings(as.numeric(data[[var_cas]]))
  x <- suppressWarnings(as.numeric(data[[var_expo]]))

  # LE PLAN EXIGE UN PAS JOURNALIER.
  pas <- suppressWarnings(stats::median(diff(sort(unique(as.numeric(dd)))), na.rm = TRUE))
  if (!isTRUE(is.finite(pas)) || pas > 2)
    return(ko(trf("Le cas-croisé stratifié sur le temps suppose un pas journalier ; le fichier a un pas de %s jour(s). Utilisez le DLNM pour des données mensuelles.",
                  formatC(pas, format = "fg"))))

  strate <- paste(format(dd, "%Y"), format(dd, "%m"), format(dd, "%u"), sep = "-")
  vars_ajust <- intersect(as.character(vars_ajust %||% character(0)), names(data))
  d <- data.frame(.y = y, .x = x, .s = factor(strate), check.names = FALSE)
  if (length(vars_ajust)) d <- cbind(d, data[, vars_ajust, drop = FALSE])
  garde <- stats::complete.cases(d)
  d <- d[garde, , drop = FALSE]
  msg <- character(0)

  # UNE STRATE A UNE SEULE LIGNE N'APPORTE RIEN et consomme un parametre : elle
  # est retiree, et comptee.
  n_par_strate <- table(d$.s)
  seules <- names(n_par_strate)[n_par_strate < 2L]
  if (length(seules)) {
    d <- d[!d$.s %in% seules, , drop = FALSE]
    msg <- c(msg, trf("%d strate(s) à une seule journée retirée(s) : sans jour-témoin, elles ne portent aucune comparaison.", length(seules)))
  }
  d$.s <- droplevels(d$.s)
  if (!nrow(d) || nlevels(d$.s) < 2L)
    return(ko(tr("Aucune strate ne contient de jour-témoin exploitable.")))

  # POISSON CONDITIONNELLE par effets fixes de strate : c'est l'equivalent
  # exact du plan pour des comptages agreges.
  fml <- stats::as.formula(paste(".y ~ .x + .s",
    if (length(vars_ajust)) paste("+", paste(sprintf("`%s`", vars_ajust), collapse = " + ")) else ""))
  fit <- tryCatch(suppressWarnings(stats::glm(fml, data = d, family = stats::quasipoisson())),
                  error = function(e) NULL)
  if (is.null(fit)) return(ko(tr("L'ajustement du modèle conditionnel a échoué.")))

  co <- .hstat_epi_coefs(fit, conf = conf, etiquette = "RR", retirer = "^(\\(Intercept\\)|\\.s)")
  if (!is.null(co) && nrow(co)) co$Terme[co$Terme == ".x"] <- var_expo

  list(ok = TRUE, model = fit, coefs = co, n = nrow(d), strates = nlevels(d$.s),
       conf = conf, message = if (length(msg)) paste(msg, collapse = " ") else NULL)
}

# STANDARDISATION ET SMR
# Deux populations n'ont pas la meme structure d'age : comparer leurs taux
# bruts, c'est comparer leurs pyramides des ages.
hstat_epi_smr <- function(data, var_evenements, var_population,
                          var_taux_ref = NULL, var_strate = NULL,
                          var_groupe = NULL, conf = 0.95, pour = 100000) {
  ko <- function(m) list(ok = FALSE, message = m)
  if (is.null(data) || !nrow(data)) return(ko(tr("Aucune donnée.")))
  for (v in c(var_evenements, var_population))
    if (!isTRUE(nzchar(v %||% "")) || !v %in% names(data))
      return(ko(tr("Choisissez la colonne des événements et celle de la population.")))

  o <- suppressWarnings(as.numeric(data[[var_evenements]]))
  pop <- suppressWarnings(as.numeric(data[[var_population]]))
  msg <- character(0)
  bad <- !is.finite(pop) | pop <= 0 | !is.finite(o) | o < 0
  if (any(bad))
    msg <- c(msg, trf("%d ligne(s) écartée(s) : population nulle ou négative, ou événements manquants.", sum(bad)))
  keep <- !bad
  if (!any(keep)) return(ko(tr("Aucune ligne exploitable.")))

  grp <- if (isTRUE(nzchar(var_groupe %||% "")) && var_groupe %in% names(data))
    as.character(data[[var_groupe]]) else rep(tr("Toutes strates confondues"), nrow(data))

  ref <- NULL
  if (isTRUE(nzchar(var_taux_ref %||% "")) && var_taux_ref %in% names(data))
    ref <- suppressWarnings(as.numeric(data[[var_taux_ref]]))

  # SANS TAUX DE REFERENCE, ON NE LES INVENTE PAS : on les DERIVE de l'ensemble
  # du fichier, strate par strate, et on le dit.
  st <- if (isTRUE(nzchar(var_strate %||% "")) && var_strate %in% names(data))
    as.character(data[[var_strate]]) else rep("_", nrow(data))
  if (is.null(ref)) {
    agg_o <- tapply(o[keep], st[keep], sum, na.rm = TRUE)
    agg_p <- tapply(pop[keep], st[keep], sum, na.rm = TRUE)
    tx <- agg_o / agg_p
    ref <- unname(tx[match(st, names(tx))])
    msg <- c(msg, tr("Aucun taux de référence déclaré : les taux de l'ensemble du fichier servent de référence (standardisation interne). Le SMR moyen vaut alors 1 par construction — ce n'est pas une comparaison à une population externe."))
  }

  # UNE LIGNE SANS TAUX DE REFERENCE EST ECARTEE, pas seulement ses attendus.
  sans_ref <- keep & (!is.finite(ref) | ref < 0)
  if (any(sans_ref)) {
    msg <- c(msg, trf("%d ligne(s) écartée(s) : taux de référence manquant ou négatif.", sum(sans_ref)))
    keep <- keep & !sans_ref
    if (!any(keep)) return(ko(tr("Aucune ligne exploitable.")))
  }

  att <- pop * ref
  g <- factor(grp[keep], levels = unique(grp[keep]))
  O <- tapply(o[keep], g, sum, na.rm = TRUE)
  E <- tapply(att[keep], g, sum, na.rm = TRUE)
  P <- tapply(pop[keep], g, sum, na.rm = TRUE)
  ic <- hstat_epi_ic_poisson(as.numeric(O), conf)

  smr <- as.numeric(O) / as.numeric(E)
  verdict <- vapply(seq_along(smr), function(k) {
    lo <- ic$bas[k] / E[k]; hi <- ic$haut[k] / E[k]
    if (!isTRUE(is.finite(lo)) || !isTRUE(is.finite(hi))) tr("indéterminable")
    else if (lo > 1) tr("excès")
    else if (hi < 1) tr("déficit")
    else tr("non significatif")
  }, character(1))

  tab <- data.frame(
    Groupe = levels(g), Observes = as.numeric(O),
    Attendus = round(as.numeric(E), 2), Population = as.numeric(P),
    SMR = round(smr, 4),
    IC_bas = round(as.numeric(ic$bas) / as.numeric(E), 4),
    IC_haut = round(as.numeric(ic$haut) / as.numeric(E), 4),
    Verdict = verdict, check.names = FALSE, stringsAsFactors = FALSE)

  # STANDARDISATION DIRECTE : la population de reference est la somme des
  # strates de tout le fichier -- la « population type » implicite.
  directe <- NULL
  if (length(unique(st[keep])) > 1L) {
    w <- tapply(pop[keep], st[keep], sum, na.rm = TRUE)
    w <- w / sum(w, na.rm = TRUE)
    directe <- do.call(rbind, lapply(levels(g), function(gg) {
      sel <- keep & grp == gg
      # LA GARDE EST PAR GROUPE, pas autour de la boucle : un groupe sans
      # certaines strates ne doit pas emporter le tableau des autres.
      tx_g <- tryCatch({
        a <- tapply(o[sel], st[sel], sum, na.rm = TRUE)
        b <- tapply(pop[sel], st[sel], sum, na.rm = TRUE)
        a / b
      }, error = function(e) NULL)
      if (is.null(tx_g)) return(NULL)
      k <- intersect(names(w), names(tx_g))
      if (!length(k)) return(NULL)
      ww <- w[k] / sum(w[k])
      data.frame(Groupe = gg, Strates_couvertes = length(k),
                 Taux_standardise = round(sum(ww * tx_g[k], na.rm = TRUE) * pour, 4),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }))
  }

  list(ok = TRUE, smr = tab, directe = directe, pour = pour, conf = conf,
       message = if (length(msg)) paste(msg, collapse = " ") else NULL)
}

# TEST DIAGNOSTIQUE -- et la valeur predictive depend de la prevalence
# Sensibilite et specificite sont des proprietes DU TEST ; les valeurs
# predictives sont des proprietes du test DANS UNE POPULATION.
hstat_epi_diagnostic <- function(data, var_test, var_reference,
                                 test_positif = NULL, ref_positif = NULL,
                                 seuil = NULL, conf = 0.95, prevalence = NULL) {
  ko <- function(m) list(ok = FALSE, message = m)
  if (is.null(data) || !nrow(data)) return(ko(tr("Aucune donnée.")))
  for (v in c(var_test, var_reference))
    if (!isTRUE(nzchar(v %||% "")) || !v %in% names(data))
      return(ko(tr("Choisissez le résultat du test et l'état de référence.")))

  rb <- hstat_epi_binaire(data[[var_reference]], ref_positif)
  if (!isTRUE(rb$ok)) return(ko(rb$message))
  brut <- data[[var_test]]
  msg <- character(0)

  num <- suppressWarnings(as.numeric(brut))
  continu <- is.numeric(brut) && length(unique(stats::na.omit(num))) > 2L
  if (continu) {
    s <- suppressWarnings(as.numeric(seuil)[1])
    if (!isTRUE(is.finite(s))) {
      s <- stats::median(num, na.rm = TRUE)
      msg <- c(msg, trf("Aucun seuil déclaré : la médiane (%.4g) sert de seuil. Un seuil se choisit sur la question posée, il ne se devine pas.", s))
    }
    tb <- list(ok = TRUE, y = as.integer(num >= s), seuil = s)
  } else {
    tb <- hstat_epi_binaire(brut, test_positif)
    if (!isTRUE(tb$ok)) return(ko(tb$message))
  }

  ok <- !is.na(tb$y) & !is.na(rb$y)
  tp <- sum(tb$y == 1L & rb$y == 1L, na.rm = TRUE)
  fp <- sum(tb$y == 1L & rb$y == 0L, na.rm = TRUE)
  fn <- sum(tb$y == 0L & rb$y == 1L, na.rm = TRUE)
  tn <- sum(tb$y == 0L & rb$y == 0L, na.rm = TRUE)
  if ((tp + fn) == 0 || (tn + fp) == 0)
    return(ko(tr("L'état de référence ne prend qu'une seule valeur : ni sensibilité ni spécificité ne sont estimables.")))

  ic_p <- function(k, n) {
    if (!isTRUE(n > 0)) return(c(NA_real_, NA_real_))
    # WILSON, pas Wald : l'intervalle de Wald sort de [0 ; 1] des que la
    # proportion approche une borne.
    z <- stats::qnorm(1 - (1 - conf) / 2); ph <- k / n
    d <- 1 + z^2 / n
    c <- (ph + z^2 / (2 * n)) / d
    h <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2)) / d
    c(max(0, c - h), min(1, c + h))
  }
  se <- tp / (tp + fn); sp <- tn / (tn + fp)
  prev_obs <- (tp + fn) / (tp + fn + tn + fp)
  prev <- suppressWarnings(as.numeric(prevalence)[1])
  if (!isTRUE(is.finite(prev)) || prev <= 0 || prev >= 1) prev <- prev_obs
  else msg <- c(msg, trf("Valeurs prédictives recalculées pour une prévalence déclarée de %.3g %% (la prévalence observée dans le fichier est de %.3g %%).",
                         100 * prev, 100 * prev_obs))

  # LES VALEURS PREDICTIVES PASSENT PAR BAYES, jamais par les effectifs du
  # tableau.
  vpp <- se * prev / (se * prev + (1 - sp) * (1 - prev))
  vpn <- sp * (1 - prev) / (sp * (1 - prev) + (1 - se) * prev)
  rvp <- if (isTRUE((1 - sp) > 0)) se / (1 - sp) else NA_real_
  rvn <- if (isTRUE(sp > 0)) (1 - se) / sp else NA_real_

  ligne <- function(lab, val, ic = NULL) data.frame(
    Mesure = lab, Valeur = round(val, 4),
    IC_bas = if (is.null(ic)) NA_real_ else round(ic[1], 4),
    IC_haut = if (is.null(ic)) NA_real_ else round(ic[2], 4),
    check.names = FALSE, stringsAsFactors = FALSE)

  tab <- rbind(
    ligne(tr("Sensibilité"), se, ic_p(tp, tp + fn)),
    ligne(tr("Spécificité"), sp, ic_p(tn, tn + fp)),
    ligne(tr("Valeur prédictive positive"), vpp),
    ligne(tr("Valeur prédictive négative"), vpn),
    ligne(tr("Rapport de vraisemblance positif"), rvp),
    ligne(tr("Rapport de vraisemblance négatif"), rvn),
    ligne(tr("Indice de Youden"), se + sp - 1),
    ligne(tr("Exactitude (accuracy)"), (tp + tn) / (tp + tn + fp + fn)))

  if (prev < 0.05)
    msg <- c(msg, trf("Prévalence faible (%.2g %%) : même un test très spécifique rend une majorité de faux positifs. VPP = %.1f %%.",
                      100 * prev, 100 * vpp))

  list(ok = TRUE, table = tab, matrice = matrix(c(tp, fn, fp, tn), 2, 2,
         dimnames = list(c(tr("Test +"), tr("Test −")),
                        c(tr("Malades (référence)"), tr("Sains (référence)")))),
       prevalence = prev, prevalence_observee = prev_obs, continu = continu,
       seuil = if (continu) tb$seuil else NULL, conf = conf,
       roc = if (continu) hstat_epi_roc(num[ok], rb$y[ok]) else NULL,
       message = if (length(msg)) paste(msg, collapse = " ") else NULL)
}

# ROC : l'aire EST la statistique de Mann-Whitney, et c'est ce qui la teste
# UNE MESURE D'AMBIANCE N'EST PAS L'EXPOSITION D'UNE PERSONNE
HSTAT_EPI_PROXY_MSG <- paste(
  "Exposition ramenée à l'échelle individuelle (personnelle = a × ambiance + b).",
  "L'axe, la référence et les valeurs des tableaux sont désormais des expositions subies ;",
  "le RR lu à un percentile donné, lui, ne change pas — la transformation est monotone.")

#' Applique la relation de transfert ambiance -> individu.
#'
#' Rend toujours une liste : `x` (la série transformée), `applique` et
#' `message`. Un facteur nul ou négatif n'est pas appliqué : il écraserait la
#' série sur une constante ou en inverserait l'ordre, donc le sens de l'effet.
hstat_epi_proxy <- function(x, a = 1, b = 0, nom = NULL) {
  av <- suppressWarnings(as.numeric(a)[1]); bv <- suppressWarnings(as.numeric(b)[1])
  if (!isTRUE(is.finite(av))) av <- 1
  if (!isTRUE(is.finite(bv))) bv <- 0
  if (av <= 0)
    return(list(x = x, applique = FALSE, a = 1, b = 0,
                message = trf("« %s » : le facteur de transfert doit être strictement positif — un facteur nul écraserait la série, un facteur négatif inverserait le sens de l'effet. La mesure d'ambiance est gardée telle quelle.",
                              nom %||% "")))
  if (isTRUE(all.equal(av, 1)) && isTRUE(all.equal(bv, 0)))
    return(list(x = x, applique = FALSE, a = 1, b = 0, message = NULL))
  list(x = av * x + bv, applique = TRUE, a = av, b = bv,
       message = paste(trf("« %s » : a = %.3g, b = %.3g.", nom %||% "", av, bv),
                       tr(HSTAT_EPI_PROXY_MSG)))
}

# LA SOUPLESSE DE LA SURFACE EST UN REGLAGE, PAS UNE CONSTANTE
# Elle valait `c(0.25, 0.50, 0.75)` en dur -- trois noeuds, donc quatre degres
# de liberte sur l'exposition, multiplies par ceux du retard.
HSTAT_EPI_NOEUDS_EXPO <- list(
  lin = list(label = "Linéaire (1 df) — le plus parcimonieux", probs = numeric(0)),
  k1  = list(label = "1 nœud, médiane (2 df)",               probs = 0.50),
  k2  = list(label = "2 nœuds, 33e et 66e (3 df)",           probs = c(1/3, 2/3)),
  k3  = list(label = "3 nœuds, quartiles (4 df)",            probs = c(0.25, 0.50, 0.75)),
  k4  = list(label = "4 nœuds, 10e à 90e (5 df)",            probs = c(0.10, 1/3, 2/3, 0.90)))

# La liste de choix DERIVE du catalogue : deux listes recopiees finiraient par
# diverger, et c'est la copie oubliee qui ment.
hstat_epi_noeuds_choix <- function() {
  stats::setNames(names(HSTAT_EPI_NOEUDS_EXPO),
                  vapply(HSTAT_EPI_NOEUDS_EXPO, function(e) tr(e$label), character(1)))
}

#' Percentiles de nœuds d'un code du catalogue.
#'
#' Un code inconnu retombe sur le défaut (quartiles) plutôt que sur le linéaire :
#' une faute de frappe ne doit pas changer la forme du modèle en silence.
hstat_epi_noeuds_probs <- function(code) {
  e <- HSTAT_EPI_NOEUDS_EXPO[[as.character(code %||% "")[1] %||% ""]]
  if (is.null(e)) HSTAT_EPI_NOEUDS_EXPO$k3$probs else e$probs
}

# UNE LIGNE DE REPERE SANS ETIQUETTE OBLIGE A SE SOUVENIR DE CE QU'ELLE MARQUE
# Le trait pointille de la reference porte une valeur -- la mediane de
# l'exposition, ou le RR neutre -- et rien ne la nomme sur la figure.
HSTAT_EPI_REPERE_POS <- c("En haut" = "haut", "Au milieu" = "milieu",
                          "En bas" = "bas")
HSTAT_EPI_REPERE_COTE <- c("À droite du trait" = "droite",
                           "À gauche du trait" = "gauche")

#' Couches d'une ligne de repère, avec son étiquette facultative.
#'
#' @param sens "v" (trait vertical, valeur en abscisse) ou "h".
#' @param etendue étendue de l'AUTRE axe ; requise pour la position « milieu ».
hstat_ligne_repere <- function(valeur, sens = "v", etiquette = NULL,
                               position = "haut", cote = "droite",
                               couleur = "#4575b4", type = "dotted",
                               epaisseur = 0.5, taille = 3.5, style = "plain",
                               etendue = NULL) {
  if (!isTRUE(is.finite(valeur))) return(list())
  vert <- !identical(sens, "h")
  cou <- if (vert)
    ggplot2::geom_vline(xintercept = valeur, linetype = type,
                        colour = couleur, linewidth = epaisseur)
  else
    ggplot2::geom_hline(yintercept = valeur, linetype = type,
                        colour = couleur, linewidth = epaisseur)
  lab <- as.character(etiquette %||% "")[1]
  if (!isTRUE(nzchar(lab))) return(list(cou))

  # « milieu » sans etendue ne peut pas etre place.
  mid <- if (length(etendue) == 2L && all(is.finite(etendue)))
    mean(range(etendue)) else NULL
  if (identical(position, "milieu") && is.null(mid)) position <- "haut"

  dec <- if (identical(cote, "gauche")) 1.2 else -0.2   # hjust, le long du trait
  a <- switch(position,
    milieu = list(o = mid,  just = 0.5),
    bas    = list(o = -Inf, just = -0.4),
    list(o = Inf, just = 1.4))                          # « haut » par defaut

  arg <- list(geom = "text", label = lab, colour = couleur, size = taille,
              fontface = style, angle = if (vert) 90 else 0)
  if (vert) {
    arg$x <- valeur; arg$y <- a$o
    arg$hjust <- a$just; arg$vjust <- dec
  } else {
    arg$y <- valeur; arg$x <- a$o
    arg$hjust <- a$just; arg$vjust <- dec
  }
  list(cou, do.call(ggplot2::annotate, arg))
}

# UN INTERVALLE ENORME N'EST PAS UN DEFAUT D'AFFICHAGE : C'EST UN BUDGET
HSTAT_EPI_BUDGET_SEUILS <- c(confortable = 10, juste = 5)

hstat_epi_dlnm_budget <- function(res) {
  if (is.null(res) || !isTRUE(res$ok) || is.null(res$model)) return(NULL)
  cf <- tryCatch(stats::coef(res$model), error = function(e) NULL)
  if (!length(cf)) return(NULL)
  nm <- names(cf)
  cbn <- res$cb_noms %||% character(0)

  # Chaque surface est reconnue par le SYMBOLE sous lequel elle est entree dans
  # la formule (`cb1`, `cb2`...), pas par le nom de la colonne.
  fam <- rep(tr("Autres termes du modèle"), length(nm))
  fam[nm == "(Intercept)"] <- tr("Constante")
  etiq <- names(cbn); if (is.null(etiq)) etiq <- cbn
  etiq[!nzchar(etiq %||% "")] <- cbn[!nzchar(etiq %||% "")]
  for (k in seq_along(cbn)) {
    sel <- startsWith(nm, paste0(cbn[k], "v"))
    if (any(sel)) fam[sel] <- trf("Surface « %s » (exposition × retard)", etiq[k])
  }
  fam[grepl("^(sin|cos)[0-9]+$", nm)] <- tr("Saison (harmoniques)")
  fam[grepl("^tend[0-9]+$", nm)]      <- tr("Tendance longue")

  tb <- as.data.frame(table(Terme = factor(fam, levels = unique(fam))),
                      stringsAsFactors = FALSE)
  names(tb)[2] <- "Parametres"
  n_obs <- res$n_utilisees %||% NA_integer_
  n_par <- length(cf)
  ratio <- if (isTRUE(is.finite(n_obs)) && n_par > 0) n_obs / n_par else NA_real_

  verdict <- if (!isTRUE(is.finite(ratio))) "indeterminable"
    else if (ratio >= HSTAT_EPI_BUDGET_SEUILS[["confortable"]]) "confortable"
    else if (ratio >= HSTAT_EPI_BUDGET_SEUILS[["juste"]]) "juste" else "insuffisant"

  # Le cout REEL de chaque levier, sur ce modele-ci.
  df1 <- tryCatch(as.integer(attr(res$cbs[[1]], "df")), error = function(e) NULL)
  n_surf <- length(cbn)
  levier <- character(0)
  if (length(df1) == 2L && all(is.finite(df1))) {
    if (df1[1] > 1L) levier <- c(levier, trf(
      "retirer un nœud à l'exposition économise %d paramètre(s)", df1[2] * n_surf))
    if (df1[2] > 1L) levier <- c(levier, trf(
      "retirer un nœud au retard en économise %d", df1[1] * n_surf))
  }
  # L'AJUSTEMENT MUTUEL COUTE UNE SURFACE ENTIERE PAR EXPOSITION EN PLUS.
  if (n_surf > 1L) {
    cout_autres <- sum(vapply(cbn[-1], function(s) sum(startsWith(nm, paste0(s, "v"))),
                              integer(1)))
    if (cout_autres > 0L) levier <- c(levier, trf(
      "ne pas ajuster les expositions les unes sur les autres en économise %d",
      cout_autres))
  }

  msg <- switch(verdict,
    confortable = trf("%d observation(s) pour %d paramètre(s) estimé(s), soit %.1f par paramètre : l'estimation est confortable.",
                      n_obs, n_par, ratio),
    juste = trf("%d observation(s) pour %d paramètre(s) estimé(s), soit %.1f par paramètre : c'est juste — la convention demande au moins 10, et les intervalles s'élargissent en dessous.",
                n_obs, n_par, ratio),
    insuffisant = trf("%d observation(s) pour %d paramètre(s) estimé(s), soit %.1f par paramètre : c'est INSUFFISANT. Des intervalles très larges (un RR de 20 avec une borne haute à 350) n'ont pas d'autre cause — ni le tracé, ni le niveau de confiance.",
                      n_obs, n_par, ratio),
    trf("%d paramètre(s) estimé(s) ; le nombre d'observations n'est pas connu.", n_par))
  if (!identical(verdict, "confortable") && length(levier))
    msg <- paste0(msg, " ", trf("Sur ce modèle : %s.", paste(levier, collapse = " ; ")),
                  " ",
                  tr("Le retard est déjà lissé par une spline : il n'y a pas de version « continue » à activer, seulement des nœuds à retirer."))

  structure(tb, n_obs = n_obs, n_par = n_par, ratio = ratio,
            verdict = verdict, message = msg)
}

# PLUSIEURS EXPOSITIONS SUR UNE MEME FIGURE
HSTAT_EPI_MULTI_MODES <- c(
  "Facettes (une échelle par exposition)" = "facettes",
  "Superposées sur les percentiles"       = "percentiles")

# Les figures qui acceptent PLUSIEURS expositions.
HSTAT_EPI_MULTI_FIG <- c("cumul", "retard", "carte", "coupes")

# La valeur du selecteur qui demande TOUTES les expositions.
HSTAT_EPI_TOUTES <- "__toutes__"

# LES DEUX LISTES DISAIENT LA MEME CHOSE, ET ELLES DISENT L'INVERSE.
HSTAT_EPI_MULTI_FACETTES <- "carte"

# Les figures dont l'axe est le PERCENTILE quoi qu'on demande.
HSTAT_EPI_MULTI_PCT <- "coupes"

#' Percentile de chaque valeur d'exposition dans sa propre distribution.
#'
#' C'est ce qui rend deux expositions comparables sur un même axe : le
#' percentile est sans dimension. `stats::ecdf` est la définition exacte.
.hstat_epi_pct <- function(valeurs, obs) {
  o <- suppressWarnings(as.numeric(obs))
  o <- o[is.finite(o)]
  if (!length(o)) return(rep(NA_real_, length(valeurs)))
  100 * stats::ecdf(o)(valeurs)
}

#' Construit la figure DLNM de PLUSIEURS expositions à la fois.
#'
#' Rend `NULL` quand la figure ne s'y prête pas — l'appelant retombe alors sur
#' l'exposition choisie, et le dit.
hstat_epi_figure_multi <- function(figure, res_list, mode = "facettes",
                                   titre = NULL, o = NULL) {
  if (!length(res_list) || !figure %in% HSTAT_EPI_MULTI_FIG) return(NULL)
  ok <- vapply(res_list, function(r) isTRUE(r$ok), logical(1))
  res_list <- res_list[ok]
  if (length(res_list) < 2L) return(NULL)
  # UNE CARTE NE SE SUPERPOSE PAS : on force les facettes plutot que de rendre
  # une image ou seule la derniere exposition se voit.
  if (figure %in% HSTAT_EPI_MULTI_FACETTES) mode <- "facettes"
  # Redondant avec la branche « coupes », qui pose son axe en percentiles quoi
  # qu'on lui passe.
  if (figure %in% HSTAT_EPI_MULTI_PCT) mode <- "percentiles"
  mode <- if (identical(mode, "percentiles")) "percentiles" else "facettes"
  noms <- names(res_list)

  # Effet cumule : une courbe exposition-reponse par exposition
  if (identical(figure, "cumul")) {
    d <- do.call(rbind, lapply(noms, function(v) {
      p <- res_list[[v]]$pred
      data.frame(x = p$predvar,
                 pct = .hstat_epi_pct(p$predvar, res_list[[v]]$expo),
                 fit = p$allRRfit, lo = p$allRRlow, hi = p$allRRhigh,
                 Exposition = v, stringsAsFactors = FALSE)
    }))
    d$Exposition <- factor(d$Exposition, levels = noms)
    pct <- identical(mode, "percentiles")
    d$ax <- if (pct) d$pct else d$x
    # LE CADRE SE BORNE SUR LES CENTILES DE L'INTERVALLE, comme la figure a une
    # exposition.
    hi <- stats::quantile(d$hi, 0.98, na.rm = TRUE)
    lo <- max(0, stats::quantile(d$lo, 0.02, na.rm = TRUE))
    g <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["ax"]], colour = .data[["Exposition"]],
                                         fill = .data[["Exposition"]])) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lo"]], ymax = .data[["hi"]]),
                           alpha = 0.12, colour = NA) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
      ggplot2::geom_line(ggplot2::aes(y = .data[["fit"]]), linewidth = 1) +
      ggplot2::coord_cartesian(ylim = c(lo, hi)) +
      # LE REPERE NE SE POSE QU'EN PERCENTILES.
      (if (pct) .hstat_epi_repere_couche(o, res_list[[1]]$reference,
                                         etendue = c(lo, hi),
                                         expo = res_list[[1]]$expo) else NULL) +
      ggplot2::labs(
        x = if (pct) tr("Percentile de l'exposition (%)") else tr("Exposition"),
        y = tr("RR cumulé"), colour = tr("Variable d'influence"),
        fill = tr("Variable d'influence"),
        title = titre %||% tr("Effet cumulé — toutes les variables d'influence"),
        subtitle = if (pct)
          tr("Axe en percentiles : sans dimension, donc comparable d'une variable à l'autre.")
        else tr("Un panneau par variable : les unités diffèrent, les échelles aussi."))
    # `free_x` N'EST PAS UN CONFORT.
    if (!pct) g <- g + ggplot2::facet_wrap(~ Exposition,
                                           scales = hstat_epi_facet_scales(o$facettes))
    return(g)
  }

  # Structure temporelle : le RETARD est une grandeur commune
  if (identical(figure, "retard")) {
    d <- do.call(rbind, lapply(noms, function(v) {
      r <- res_list[[v]]; p <- r$pred
      lagg <- .hstat_epi_lag_grille(p)
      q <- stats::quantile(r$expo, 0.90, na.rm = TRUE)
      i <- which.min(abs(p$predvar - q))
      data.frame(lag = lagg, fit = p$matRRfit[i, ], lo = p$matRRlow[i, ],
                 hi = p$matRRhigh[i, ], Exposition = v, stringsAsFactors = FALSE)
    }))
    d$Exposition <- factor(d$Exposition, levels = noms)
    g <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["lag"]], colour = .data[["Exposition"]],
                                         fill = .data[["Exposition"]])) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lo"]], ymax = .data[["hi"]]),
                           alpha = 0.12, colour = NA) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
      ggplot2::geom_line(ggplot2::aes(y = .data[["fit"]]), linewidth = 1) +
      ggplot2::labs(x = tr("Retard"), y = "RR", colour = tr("Variable d'influence"),
                    fill = tr("Variable d'influence"),
                    title = titre %||% tr("Structure temporelle au 90e percentile"),
                    subtitle = tr("Le retard est la même grandeur pour toutes : les courbes se superposent sans transformation."))
    if (identical(mode, "facettes"))
      g <- g + ggplot2::facet_wrap(~ Exposition,
                                   scales = hstat_epi_facet_scales(o$facettes))
    return(g)
  }

  # Carte de chaleur : facettes, chacune avec son echelle d'exposition
  if (identical(figure, "carte")) {
    d <- do.call(rbind, lapply(noms, function(v) {
      p <- res_list[[v]]$pred
      e <- expand.grid(lag = .hstat_epi_lag_grille(p), x = p$predvar)
      e$RR <- as.vector(t(p$matRRfit))
      e$Exposition <- v
      e
    }))
    d$Exposition <- factor(d$Exposition, levels = noms)
    # `geom_tile` ET NON `geom_raster` : deux expositions ont deux grilles de
    # valeurs, donc un pas horizontal inegal une fois empilees.
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data[["x"]], y = .data[["lag"]],
                                           fill = .data[["RR"]])) +
      ggplot2::geom_tile() +
      hstat_epi_carte_fill(o$carte_palette) +
      ggplot2::facet_wrap(~ Exposition,
                          scales = hstat_epi_facet_scales(o$facettes)) +
      ggplot2::labs(x = tr("Exposition"), y = tr("Retard"), fill = "RR",
                    title = titre %||% tr("Surfaces exposition × retard")))
  }

  # Coupes par retard : exposition en couleur, retard en facette
  if (identical(figure, "coupes")) {
    d <- do.call(rbind, lapply(noms, function(v) {
      r <- res_list[[v]]; p <- r$pred
      lags <- seq(0, r$lag_max, by = 1)
      cols <- vapply(lags, function(l) .hstat_epi_col_lag(p, l), integer(1))
      k <- which(!is.na(cols))
      if (!length(k)) return(NULL)
      do.call(rbind, lapply(k, function(j) data.frame(
        pct = .hstat_epi_pct(p$predvar, r$expo), fit = p$matRRfit[, cols[j]],
        lag = lags[j], Exposition = v, stringsAsFactors = FALSE)))
    }))
    if (is.null(d) || !NROW(d)) return(NULL)
    d$Exposition <- factor(d$Exposition, levels = noms)
    d$lag <- factor(trf("Retard %d", d$lag),
                    levels = trf("Retard %d", sort(unique(d$lag))))
    # L'AXE EST LE PERCENTILE, et il le faut : les facettes portent ici le
    # RETARD, donc un meme panneau recoit toutes les expositions.
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data[["pct"]], y = .data[["fit"]],
                                           colour = .data[["Exposition"]])) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
      .hstat_epi_repere_couche(o, res_list[[1]]$reference,
                               etendue = range(d$fit, na.rm = TRUE),
                               expo = res_list[[1]]$expo) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::facet_wrap(~ lag, scales = hstat_epi_facet_scales(o$facettes)) +
      ggplot2::labs(x = tr("Percentile de l'exposition (%)"), y = "RR",
                    colour = tr("Variable d'influence"),
                    title = titre %||% tr("Coupes par retard — toutes les variables")))
  }
  NULL
}

# AUC = P(score d'un malade > score d'un sain), ex aequo comptes pour moitie.
hstat_epi_roc <- function(score, etat) {
  s <- suppressWarnings(as.numeric(score)); e <- as.integer(etat)
  ok <- is.finite(s) & e %in% c(0L, 1L)
  s <- s[ok]; e <- e[ok]
  if (!length(s) || !any(e == 1L) || !any(e == 0L)) return(NULL)
  seuils <- sort(unique(s), decreasing = TRUE)
  seuils <- c(Inf, seuils)
  np <- sum(e == 1L); nn <- sum(e == 0L)
  se <- vapply(seuils, function(k) sum(s >= k & e == 1L) / np, numeric(1))
  sp <- vapply(seuils, function(k) sum(s < k & e == 0L) / nn, numeric(1))
  courbe <- data.frame(Seuil = seuils, Sensibilite = se, Specificite = sp,
                       Un_moins_Sp = 1 - sp, check.names = FALSE)
  courbe <- courbe[order(courbe$Un_moins_Sp, courbe$Sensibilite), , drop = FALSE]
  aire <- sum(diff(courbe$Un_moins_Sp) *
                (utils::head(courbe$Sensibilite, -1) + courbe$Sensibilite[-1]) / 2)
  # Le rang : meme quantite, autre chemin.
  r <- rank(s)
  auc_rang <- (sum(r[e == 1L]) - np * (np + 1) / 2) / (np * nn)
  j <- which.max(courbe$Sensibilite + courbe$Specificite - 1)
  list(courbe = courbe, auc = aire, auc_rang = auc_rang,
       youden = list(seuil = courbe$Seuil[j], se = courbe$Sensibilite[j],
                     sp = courbe$Specificite[j]),
       n_positifs = np, n_negatifs = nn)
}

# MESURES D'IMPACT -- ce qu'une decision de sante publique demande vraiment
# Un RR de 2 ne dit pas s'il faut agir.
hstat_epi_impact <- function(a, b, c, d, conf = 0.95, prevalence_expo = NULL) {
  v <- suppressWarnings(as.numeric(c(a, b, c, d)))
  if (any(!is.finite(v)) || any(v < 0))
    return(list(ok = FALSE, message = tr("Les quatre effectifs du tableau 2x2 doivent être des entiers positifs ou nuls.")))
  a <- v[1]; b <- v[2]; c <- v[3]; d <- v[4]
  msg <- character(0)

  # LA GARDE DES GROUPES VIDES PRECEDE LA CORRECTION DE HALDANE.
  if (!isTRUE(a + b > 0) || !isTRUE(c + d > 0))
    return(list(ok = FALSE, message = tr("Un des deux groupes est vide : aucun risque n'est comparable.")))

  # UNE CASE NULLE REND LE RR INDEFINI ou infini.
  corrige <- FALSE
  if (any(c(a, b, c, d) == 0)) {
    a <- a + 0.5; b <- b + 0.5; c <- c + 0.5; d <- d + 0.5
    corrige <- TRUE
    msg <- c(msg, tr("Au moins une case du tableau est nulle : correction de Haldane (+0,5) appliquée. Les intervalles sont plus larges que ceux des effectifs bruts."))
  }
  n1 <- a + b; n0 <- c + d

  r1 <- a / n1; r0 <- c / n0
  rr <- if (isTRUE(r0 > 0)) r1 / r0 else NA_real_
  ra <- r1 - r0
  fae <- if (isTRUE(r1 > 0) && isTRUE(is.finite(rr))) (rr - 1) / rr else NA_real_
  pe <- suppressWarnings(as.numeric(prevalence_expo)[1])
  if (!isTRUE(is.finite(pe)) || pe < 0 || pe > 1) {
    pe <- n1 / (n1 + n0)
    msg <- c(msg, trf("Prévalence de l'exposition prise sur le fichier (%.1f %%). Sur un plan cas-témoins elle est FIXÉE par l'échantillonnage : déclarez la prévalence en population, sinon la fraction attribuable en population ne décrit aucune population réelle.",
                      100 * pe))
  }
  fap <- if (isTRUE(is.finite(rr))) pe * (rr - 1) / (1 + pe * (rr - 1)) else NA_real_
  nnt <- if (isTRUE(is.finite(ra)) && abs(ra) > 0) 1 / abs(ra) else NA_real_

  z <- stats::qnorm(1 - (1 - conf) / 2)
  se_ra <- sqrt(r1 * (1 - r1) / n1 + r0 * (1 - r0) / n0)
  se_lrr <- if (isTRUE(a > 0) && isTRUE(c > 0)) sqrt(1 / a - 1 / n1 + 1 / c - 1 / n0) else NA_real_

  ligne <- function(lab, val, lo = NA_real_, hi = NA_real_, unite = "",
                    cle = "")
    data.frame(Mesure = lab, Valeur = round(val, 4), IC_bas = round(lo, 4),
               IC_haut = round(hi, 4), Unite = unite, .cle = cle,
               check.names = FALSE, stringsAsFactors = FALSE)

  tab <- rbind(
    ligne(tr("Risque chez les exposés"), r1, unite = tr("proportion"), cle = "prop"),
    ligne(tr("Risque chez les non exposés"), r0, unite = tr("proportion"), cle = "prop"),
    ligne(tr("Risque relatif (RR)"), rr,
          if (isTRUE(is.finite(se_lrr))) exp(log(rr) - z * se_lrr) else NA_real_,
          if (isTRUE(is.finite(se_lrr))) exp(log(rr) + z * se_lrr) else NA_real_),
    ligne(tr("Risque attribuable (RA)"), ra, ra - z * se_ra, ra + z * se_ra,
          tr("proportion"), cle = "prop"),
    ligne(tr("Fraction attribuable chez les exposés (FAE)"), fae, unite = tr("proportion"), cle = "prop"),
    ligne(tr("Fraction attribuable en population (FAP)"), fap, unite = tr("proportion"), cle = "prop"),
    ligne(if (isTRUE(ra < 0)) tr("Nombre de sujets à traiter (NST)")
          else tr("Nombre de sujets à exposer pour un cas (NSE)"), nnt,
          unite = tr("nombre de sujets"), cle = "nnt"))

  # UN RISQUE ATTRIBUABLE NEGATIF EST UN RESULTAT : l'exposition PROTEGE.
  if (isTRUE(ra < 0))
    msg <- c(msg, tr("Le risque attribuable est négatif : dans ces données l'exposition est associée à MOINS de cas. Les fractions attribuables se lisent alors comme une fraction évitée."))

  list(ok = TRUE, table = tab, corrige = corrige, conf = conf,
       prevalence_expo = pe, message = if (length(msg)) paste(msg, collapse = " ") else NULL)
}

# FIGURES -- le catalogue est declare une fois, le selecteur en derive
HSTAT_EPI_FIGURES <- list(
  dlnm = c("Effet cumulé (tous retards)" = "cumul",
           "Carte de chaleur exposition × retard" = "carte",
           "Structure temporelle (par retard)" = "retard",
           "Coupes par retard" = "coupes",
           "Surface 3D" = "surface3d",
           "Diagnostics du modèle" = "diag"),
  taux       = c("Forêt des IRR" = "foret"),
  risque     = c("Forêt : OR contre RR" = "foret"),
  survie     = c("Courbes de Kaplan-Meier" = "km", "Forêt des HR" = "foret"),
  cascroise  = c("Forêt des RR" = "foret"),
  smr        = c("SMR par groupe" = "smr"),
  diagnostic = c("Courbe ROC" = "roc"),
  impact     = c("Mesures d'impact" = "impact")
)

# LES FIGURES TRACEES EN GRAPHIQUES DE BASE N'OBEISSENT PAS AU KIT GGPLOT.
HSTAT_EPI_FIGURES_BASE <- c("surface3d", "diag")

HSTAT_EPI_FACET_SCALES <- c(
  "Axe X libre (unités différentes)"        = "free_x",
  "Échelles fixes (RR comparables)"         = "fixed",
  "Axe Y libre (formes comparables)"        = "free_y",
  "Les deux libres (chaque panneau seul)"   = "free")

# Un nom inconnu retombe sur le defaut, jamais sur une valeur que `facet_wrap`
# refuserait : elle leve, et l'erreur emporte la figure entiere.
hstat_epi_facet_scales <- function(code) {
  v <- as.character(code %||% "")[1]
  if (v %in% HSTAT_EPI_FACET_SCALES) v else "free_x"
}

# LA CARTE CHANGE DE COULEURS, JAMAIS DE NATURE
HSTAT_EPI_CARTE_PALETTES <- list(
  thermique  = list(label = "Thermique (bleu → rouge) — température",
                    low = "#4575b4", mid = "#ffffbf", high = "#a50026"),
  pluvio     = list(label = "Pluviométrie (brun → vert-bleu)",
                    low = "#8c510a", mid = "#f6e8c3", high = "#01665e"),
  humidite   = list(label = "Humidité (orange → violet)",
                    low = "#b35806", mid = "#f7f7f7", high = "#542788"),
  pollution  = list(label = "Pollution (vert → magenta)",
                    low = "#1b7837", mid = "#f7f7f7", high = "#762a83"),
  neutre     = list(label = "Neutre (violet → vert)",
                    low = "#762a83", mid = "#f7f7f7", high = "#1b7837"),
  gris       = list(label = "Sobre (impression noir et blanc)",
                    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b"))

hstat_epi_carte_choix <- function() {
  stats::setNames(names(HSTAT_EPI_CARTE_PALETTES),
                  vapply(HSTAT_EPI_CARTE_PALETTES, function(x) x$label, character(1)))
}

# Un nom inconnu rend le triplet thermique plutot que `NULL` : une echelle
# absente laisserait ggplot poser la sienne, qui est SEQUENTIELLE.
hstat_epi_carte_palette <- function(code) {
  v <- as.character(code %||% "")[1]
  HSTAT_EPI_CARTE_PALETTES[[if (v %in% names(HSTAT_EPI_CARTE_PALETTES)) v else "thermique"]]
}

# L'echelle de remplissage de la carte, posee une seule fois.
hstat_epi_carte_fill <- function(code) {
  pal <- hstat_epi_carte_palette(code)
  ggplot2::scale_fill_gradient2(low = pal$low, mid = pal$mid, high = pal$high,
                                midpoint = 1)
}

.hstat_epi_gg_foret <- function(df, etiquette, titre, ref = 1, o = NULL) {
  df <- df[is.finite(df[[etiquette]]), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df$Terme <- factor(df$Terme, levels = rev(df$Terme))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[etiquette]], y = .data[["Terme"]])) +
    ggplot2::geom_vline(xintercept = ref, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data[["IC_bas"]], xmax = .data[["IC_haut"]]),
                            height = 0.18, colour = "#2c3e50") +
    ggplot2::geom_point(size = 2.6, colour = "#d73027") +
    ggplot2::labs(x = etiquette, y = NULL, title = titre)
  # L'ECHELLE EST LOGARITHMIQUE PARCE QUE LA MESURE EST UN RAPPORT.
  if (all(df$IC_bas > 0, na.rm = TRUE)) p <- p + ggplot2::scale_x_log10()
  p
}

#' Construit une figure du module d'epidemiologie.
#'
#' Rend un objet ggplot, OU une fonction sans argument pour les figures tracees
#' en graphiques de base (`hstat_ecrire_image` accepte les deux).
# LE REPERE DE REFERENCE SE POSE PARTOUT OU L'AXE PORTE L'EXPOSITION
HSTAT_EPI_PCT_MARQUEURS <- c(10, 25, 75, 90)

# Les choix offerts.
HSTAT_EPI_PCT_CHOIX <- c(1, 2.5, 5, 10, 25, 50, 75, 90, 95, 97.5, 99)

# Les percentiles retenus, bornes et ordonnes.
hstat_epi_pct_valides <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  v <- v[is.finite(v) & v > 0 & v < 100]
  if (!length(v)) return(numeric(0))
  sort(unique(v))
}

# Les marqueurs : un point et son etiquette par percentile demande.
.hstat_epi_marqueurs <- function(o, res, decimales = 2L, taille = 3) {
  pc <- hstat_epi_pct_valides((o %||% list())$percentiles)
  if (!length(pc) || is.null(res$pred)) return(NULL)
  q <- stats::quantile(res$expo, pc / 100, na.rm = TRUE)
  i <- vapply(q, function(v) which.min(abs(res$pred$predvar - v)), integer(1))
  dec <- max(0L, min(6L, as.integer(decimales %||% 2L)))
  d <- data.frame(
    x = res$pred$predvar[i], y = res$pred$allRRfit[i],
    lab = sprintf("P%s\nRR=%s", formatC(pc, format = "fg"),
                  formatC(round(res$pred$allRRfit[i], dec), format = "f", digits = dec)),
    stringsAsFactors = FALSE)
  d <- d[is.finite(d$x) & is.finite(d$y), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  list(
    ggplot2::geom_point(data = d, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
                        inherit.aes = FALSE, colour = "#d73027", size = 2.2),
    ggplot2::geom_text(data = d,
                       ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["lab"]]),
                       inherit.aes = FALSE, colour = "#d73027", size = taille,
                       vjust = -0.4, lineheight = 0.9))
}

.hstat_epi_repere_couche <- function(o, ref, etendue = NULL, expo = NULL) {
  rp <- (o %||% list())$repere %||% list()
  v <- suppressWarnings(as.numeric(ref)[1])
  if (!isTRUE(is.finite(v))) return(NULL)
  # L'axe est en percentiles des que l'appelant fournit la serie d'exposition.
  pos_x <- if (!is.null(expo)) .hstat_epi_pct(v, expo) else v
  # « NOMMER LA LIGNE » NE COMMANDE QUE LE NOM.
  t0 <- as.character(rp$texte %||% "")[1]
  lab <- if (!isTRUE(rp$montrer %||% TRUE)) NULL
         else if (nzchar(t0)) t0 else trf("Référence : %.3g", v)
  hstat_ligne_repere(pos_x, sens = "v", etiquette = lab,
                     position = rp$position %||% "haut",
                     cote = rp$cote %||% "droite",
                     epaisseur = rp$epaisseur %||% 0.5,
                     taille = rp$taille %||% 3.5,
                     style = rp$style %||% "plain",
                     etendue = etendue)
}

# FENETRES DE RETARD : « lag 0-1 », « lag 2-4 », et leurs noms
# INTERVALLE de retards.
.hstat_epi_W <- function(cb, x, lags, ref) {
  av <- attributes(cb)$argvar; al <- attributes(cb)$arglag
  bv <- function(v) as.vector(do.call(dlnm::onebasis, c(list(x = v), av)))
  bl <- function(l) as.vector(do.call(dlnm::onebasis, c(list(x = l), al)))
  a <- bv(x) - bv(ref)
  Reduce(`+`, lapply(lags, function(l) {
    b <- bl(l)
    rep(a, each = length(b)) * rep(b, times = length(a))
  }))
}

# Les fenetres usuelles de l'epidemiologie environnementale, nommees.
HSTAT_EPI_FENETRES <- list(
  list(nom = "Court terme",  de = 0L, a = 1L),
  list(nom = "Différé",      de = 2L, a = 4L),
  list(nom = "Moyen terme",  de = 5L, a = 8L),
  list(nom = "Long terme",   de = 9L, a = 12L))

# La saisie libre : « 0-1 court terme ; 2-4 différé ; 5 ».
hstat_epi_fenetres_parse <- function(txt, lag_max = Inf) {
  z <- as.character(txt %||% "")[1]
  mor <- trimws(unlist(strsplit(z, "[;\n]+")))
  mor <- mor[nzchar(mor)]
  out <- list(); rejets <- character(0)
  for (m in mor) {
    g <- regmatches(m, regexec("^\\s*([0-9]+)\\s*(?:[-–—a]\\s*([0-9]+))?\\s*(.*)$", m))[[1]]
    if (!length(g)) { rejets <- c(rejets, m); next }
    de <- suppressWarnings(as.integer(g[2]))
    a  <- if (nzchar(g[3])) suppressWarnings(as.integer(g[3])) else de
    nom <- trimws(g[4])
    if (!isTRUE(is.finite(de)) || !isTRUE(is.finite(a))) { rejets <- c(rejets, m); next }
    # LES BORNES SE REMETTENT DANS L'ORDRE plutot que de rendre une fenetre
    # vide.
    if (a < de) { t0 <- de; de <- a; a <- t0 }
    if (de > lag_max) { rejets <- c(rejets, m); next }
    a <- min(a, lag_max)
    out[[length(out) + 1L]] <- list(
      nom = if (nzchar(nom)) nom else NA_character_, de = de, a = a)
  }
  attr(out, "rejets") <- rejets
  out
}

# L'ecriture d'une fenetre, declaree une fois : le tableau, le graphique et
# l'export la lisent, ils ne peuvent donc pas diverger.
hstat_epi_fenetre_libelle <- function(f) {
  base <- if (f$de == f$a) trf("Lag %d", f$de) else trf("Lag %d-%d", f$de, f$a)
  if (!is.na(f$nom) && nzchar(f$nom)) sprintf("%s (%s)", base, f$nom) else base
}

#' RR cumulé sur chaque fenêtre de retard, à un percentile d'exposition donné.
#'
#' Rend un data.frame : fenêtre, bornes, RR, IC, verdict. `NULL` si le modèle
#' n'expose pas sa base croisée.
hstat_epi_dlnm_fenetres <- function(res, fenetres = HSTAT_EPI_FENETRES,
                                    prob = 0.90, conf = NULL) {
  if (!isTRUE(res$ok) || is.null(res$cb) || is.null(res$model)) return(NULL)
  if (!requireNamespace("dlnm", quietly = TRUE)) return(NULL)
  cf <- res$conf %||% 0.95
  if (isTRUE(is.finite(suppressWarnings(as.numeric(conf)[1])))) cf <- as.numeric(conf)[1]
  zq <- stats::qnorm(1 - (1 - cf) / 2)
  nm <- names(stats::coef(res$model))
  # `^nom` SUIVI DE `v`, comme le test de Wald : sans lui, `^cb1` prend aussi
  # les coefficients d'une surface `cb10`.
  idx <- grep(paste0("^", res$cb_noms[[1]], "v"), nm)
  if (!length(idx)) return(NULL)
  b <- stats::coef(res$model)[idx]
  V <- stats::vcov(res$model)[idx, idx, drop = FALSE]
  # L'EXPOSITION SE CALE SUR LA GRILLE DE PREDICTION, comme le fait
  # `hstat_epi_dlnm_rr()`.
  q <- unname(stats::quantile(res$expo, prob, na.rm = TRUE))
  x <- res$pred$predvar[which.min(abs(res$pred$predvar - q))]
  lignes <- lapply(fenetres, function(f) {
    lags <- seq(f$de, f$a)
    lags <- lags[lags >= 0 & lags <= res$lag_max]
    if (!length(lags)) return(NULL)
    w <- tryCatch(.hstat_epi_W(res$cb, x, lags, res$reference), error = function(e) NULL)
    if (is.null(w) || length(w) != length(b)) return(NULL)
    est <- sum(w * b)
    va  <- drop(t(w) %*% V %*% w)
    se  <- if (isTRUE(is.finite(va)) && va >= 0) sqrt(va) else NA_real_
    lo <- exp(est - zq * se); hi <- exp(est + zq * se)
    data.frame(
      Fenetre = hstat_epi_fenetre_libelle(list(nom = f$nom, de = min(lags), a = max(lags))),
      Lag_de = min(lags), Lag_a = max(lags),
      RR = round(exp(est), 4),
      IC_bas = round(lo, 4), IC_haut = round(hi, 4),
      # NE JAMAIS BRANCHER SUR UNE STATISTIQUE NON CALCULABLE : un intervalle
      # incalculable ne prouve rien, ni dans un sens ni dans l'autre.
      Verdict = if (!isTRUE(is.finite(lo)) || !isTRUE(is.finite(hi)))
                  tr("indéterminable")
                else if (lo > 1) tr("risque accru")
                else if (hi < 1) tr("risque réduit")
                else tr("non significatif"),
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  lignes <- lignes[!vapply(lignes, is.null, logical(1))]
  if (!length(lignes)) return(NULL)
  d <- do.call(rbind, lignes)
  attr(d, "exposition") <- x
  attr(d, "percentile") <- prob
  d
}

hstat_epi_figure <- function(analyse, figure, res, o = NULL, titre = NULL) {
  if (is.null(res) || !isTRUE(res$ok)) return(NULL)
  # LE KIT DE MISE EN FORME NE SE POSE QUE S'IL A ETE LU.
  th <- function(p) {
    if (is.null(p)) return(NULL)
    base <- if (isTRUE(is.finite(suppressWarnings(as.numeric(o$police)[1]))))
      as.numeric(o$police)[1] else 11
    p <- p + viz_get_theme(o$theme %||% "minimal", base_size = base)
    if (is.list(o) && !is.null(o$familles)) p <- p + hstat_plot_extras_theme(o)
    p
  }

  if (identical(analyse, "dlnm")) {
    p <- res$pred
    lagg <- .hstat_epi_lag_grille(p)

    if (identical(figure, "cumul")) {
      d <- data.frame(x = p$predvar, fit = p$allRRfit,
                      lo = p$allRRlow, hi = p$allRRhigh)
      # UN INTERVALLE QUI EXPLOSE AUX EXTREMES ECRASE LA COURBE CENTRALE.
      hi <- stats::quantile(d$hi, 0.98, na.rm = TRUE)
      lo <- max(0, stats::quantile(d$lo, 0.02, na.rm = TRUE))
      # L'ETIQUETTE SE POSE A COTE DU TRAIT, et son etendue de placement est
      # celle du CADRE (`lo`, `hi`), pas celle des donnees.
      return(th(ggplot2::ggplot(d, ggplot2::aes(x = .data[["x"]])) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lo"]], ymax = .data[["hi"]]),
                             fill = "#d73027", alpha = 0.15) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
        .hstat_epi_repere_couche(o, res$reference, etendue = c(lo, hi)) +
        ggplot2::geom_line(ggplot2::aes(y = .data[["fit"]]), colour = "#d73027", linewidth = 1) +
        # LES MARQUEURS SE POSENT APRES LA LIGNE : un point sous la courbe
        # disparaitrait derriere elle.
        .hstat_epi_marqueurs(o, res, decimales = o$pct_decimales %||% 2L,
                             taille = o$pct_taille %||% 3) +
        ggplot2::coord_cartesian(ylim = c(lo, hi)) +
        ggplot2::labs(x = tr("Exposition"), y = tr("RR cumulé"),
                      title = titre %||% tr("Effet cumulé sur tous les retards"),
                      subtitle = trf("Référence : %.3g", res$reference))))
    }

    if (identical(figure, "carte")) {
      d <- expand.grid(lag = lagg, x = p$predvar)
      d$RR <- as.vector(t(p$matRRfit))
      return(th(ggplot2::ggplot(d, ggplot2::aes(x = .data[["x"]], y = .data[["lag"]],
                                                fill = .data[["RR"]])) +
        ggplot2::geom_raster(interpolate = TRUE) +
        # L'ECHELLE RESTE DIVERGENTE ET CENTREE SUR 1 quelle que soit la
        # palette.
        hstat_epi_carte_fill(o$carte_palette) +
        .hstat_epi_repere_couche(o, res$reference, etendue = range(lagg, na.rm = TRUE)) +
        ggplot2::labs(x = tr("Exposition"), y = tr("Retard"), fill = "RR",
                      title = titre %||% tr("Surface exposition × retard"))))
    }

    if (identical(figure, "retard")) {
      q <- stats::quantile(res$expo, c(0.10, 0.90), na.rm = TRUE)
      d <- do.call(rbind, lapply(seq_along(q), function(k) {
        i <- which.min(abs(p$predvar - q[k]))
        data.frame(lag = lagg, fit = p$matRRfit[i, ], lo = p$matRRlow[i, ],
                   hi = p$matRRhigh[i, ],
                   serie = sprintf("P%s (%.3g)", c("10", "90")[k], p$predvar[i]))
      }))
      return(th(ggplot2::ggplot(d, ggplot2::aes(x = .data[["lag"]], colour = .data[["serie"]],
                                                fill = .data[["serie"]])) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lo"]], ymax = .data[["hi"]]),
                             alpha = 0.15, colour = NA) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
        ggplot2::geom_line(ggplot2::aes(y = .data[["fit"]]), linewidth = 1) +
        ggplot2::scale_colour_manual(values = c("#4575b4", "#d73027")) +
        ggplot2::scale_fill_manual(values = c("#4575b4", "#d73027")) +
        ggplot2::labs(x = tr("Retard"), y = "RR", colour = NULL, fill = NULL,
                      title = titre %||% tr("Structure temporelle de l'effet"))))
    }

    if (identical(figure, "coupes")) {
      lags <- seq(0, res$lag_max, by = 1)
      cols <- vapply(lags, function(l) .hstat_epi_col_lag(p, l), integer(1))
      ok <- !is.na(cols)
      d <- do.call(rbind, lapply(which(ok), function(k) {
        data.frame(x = p$predvar, fit = p$matRRfit[, cols[k]],
                   lo = p$matRRlow[, cols[k]], hi = p$matRRhigh[, cols[k]],
                   lag = factor(trf("Retard %d", lags[k]),
                                levels = trf("Retard %d", lags[ok])))
      }))
      return(th(ggplot2::ggplot(d, ggplot2::aes(x = .data[["x"]])) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lo"]], ymax = .data[["hi"]]),
                             fill = "#d73027", alpha = 0.15) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
        # LE REPERE MANQUAIT ICI, et c'est la figure qui en a le plus besoin.
        .hstat_epi_repere_couche(o, res$reference,
                                 etendue = range(c(d$lo, d$hi), na.rm = TRUE)) +
        ggplot2::geom_line(ggplot2::aes(y = .data[["fit"]]), colour = "#d73027") +
        ggplot2::facet_wrap(~ lag, scales = hstat_epi_facet_scales(o$facettes)) +
        ggplot2::labs(x = tr("Exposition"), y = "RR",
                      title = titre %||% tr("Coupes par retard"))))
    }

    if (identical(figure, "surface3d")) {
      pal <- grDevices::colorRampPalette(c("#313695", "#4575b4", "#74add1", "#e0f3f8",
        "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#a50026"))(100)
      return(function() {
        graphics::par(mar = c(1, 1, 3, 1))
        graphics::plot(p, xlab = tr("Exposition"), zlab = "RR", theta = 200,
                       phi = 30, lphi = 30, col = pal, border = NA,
                       main = titre %||% tr("Surface exposition-retard"))
      })
    }

    if (identical(figure, "diag")) {
      dg <- hstat_epi_dlnm_diag(res)
      if (is.null(dg) || !nrow(dg)) return(NULL)
      return(function() {
        graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2.5, 1.5))
        graphics::plot(dg$Ajuste, dg$Residu, pch = 16, cex = 0.7, col = "grey40",
                       xlab = tr("Valeurs ajustées"), ylab = tr("Résidus de déviance"),
                       main = tr("Résidus contre ajustés"))
        graphics::abline(h = 0, col = "#d73027", lty = 2, lwd = 2)
        # `lowess` leve « delta doit etre fini et > 0 » sur des non-finis.
        if (length(unique(dg$Ajuste)) > 2L)
          graphics::lines(stats::lowess(dg$Ajuste, dg$Residu), col = "#4575b4", lwd = 2)
        stats::qqnorm(dg$Residu, pch = 16, cex = 0.7, col = "grey40",
                      main = tr("Droite de Henry des résidus"))
        stats::qqline(dg$Residu, col = "#d73027", lwd = 2)
        stats::acf(dg$Residu, lag.max = min(24L, nrow(dg) - 1L),
                   main = tr("Autocorrélation des résidus"), na.action = stats::na.pass)
        graphics::plot(dg$Temps, dg$Residu, type = "l", col = "grey40",
                       xlab = tr("Temps de suivi"), ylab = tr("Résidus de déviance"),
                       main = tr("Résidus dans le temps"))
        graphics::abline(h = 0, col = "#d73027", lty = 2, lwd = 2)
      })
    }
    return(NULL)
  }

  if (identical(analyse, "taux"))
    return(th(.hstat_epi_gg_foret(res$coefs, "IRR",
      titre %||% tr("Rapports de taux d'incidence"))))

  if (identical(analyse, "cascroise"))
    return(th(.hstat_epi_gg_foret(res$coefs, "RR",
      titre %||% tr("Effet de l'exposition (cas-croisé)"))))

  if (identical(analyse, "risque")) {
    if (is.null(res$or)) return(NULL)
    d <- rbind(
      data.frame(Terme = res$or$Terme, Mesure = "OR", val = res$or$OR,
                 IC_bas = res$or$IC_bas, IC_haut = res$or$IC_haut),
      if (!is.null(res$rr)) data.frame(Terme = res$rr$Terme, Mesure = "RR",
        val = res$rr$RR, IC_bas = res$rr$IC_bas, IC_haut = res$rr$IC_haut))
    d <- d[is.finite(d$val), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    d$Terme <- factor(d$Terme, levels = rev(unique(d$Terme)))
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["val"]], y = .data[["Terme"]],
                                         colour = .data[["Mesure"]])) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data[["IC_bas"]], xmax = .data[["IC_haut"]]),
                              height = 0.18, position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_point(size = 2.6, position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::scale_colour_manual(values = c(OR = "#d73027", RR = "#4575b4")) +
      ggplot2::labs(x = tr("Rapport"), y = NULL, colour = NULL,
                    title = titre %||% tr("Odds ratio contre risque relatif"),
                    subtitle = trf("Issue présente chez %.1f %% des observations", 100 * res$prevalence))
    if (all(d$IC_bas > 0, na.rm = TRUE)) p <- p + ggplot2::scale_x_log10()
    return(th(p))
  }

  if (identical(analyse, "survie")) {
    if (identical(figure, "foret"))
      return(th(.hstat_epi_gg_foret(res$cox, "HR", titre %||% tr("Rapports de risques (Cox)"))))
    if (is.null(res$km)) return(NULL)
    k <- res$km
    d <- data.frame(temps = k$time, surv = k$surv, lo = k$lower, hi = k$upper)
    d$groupe <- if (!is.null(k$strata)) rep(sub("^[^=]*=", "", names(k$strata)), k$strata)
                else tr("Tous sujets confondus")
    return(th(ggplot2::ggplot(d, ggplot2::aes(x = .data[["temps"]], y = .data[["surv"]],
                                              colour = .data[["groupe"]],
                                              fill = .data[["groupe"]])) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lo"]], ymax = .data[["hi"]]),
                           alpha = 0.12, colour = NA) +
      # `geom_step` ET NON `geom_line` : la survie de Kaplan-Meier est une
      # fonction en ESCALIER, constante entre deux evenements.
      ggplot2::geom_step(linewidth = 0.9) +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::labs(x = tr("Temps de suivi"), y = tr("Probabilité de survie"),
                    colour = NULL, fill = NULL,
                    title = titre %||% tr("Courbes de survie de Kaplan-Meier"))))
  }

  if (identical(analyse, "smr")) {
    d <- res$smr
    d <- d[is.finite(d$SMR), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    return(th(ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(.data[["Groupe"]], .data[["SMR"]]),
                                              y = .data[["SMR"]])) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[["IC_bas"]], ymax = .data[["IC_haut"]]),
                             width = 0.18, colour = "#2c3e50") +
      ggplot2::geom_point(size = 3, colour = "#d73027") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = tr("SMR (observés / attendus)"),
                    title = titre %||% tr("Rapport standardisé de mortalité"))))
  }

  if (identical(analyse, "diagnostic")) {
    if (is.null(res$roc)) return(NULL)
    c0 <- res$roc$courbe
    return(th(ggplot2::ggplot(c0, ggplot2::aes(x = .data[["Un_moins_Sp"]],
                                               y = .data[["Sensibilite"]])) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
      ggplot2::geom_line(colour = "#d73027", linewidth = 1) +
      # LE CADRE EST CARRE PARCE QUE L'AIRE EN DEPEND.
      ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(x = tr("1 − spécificité"), y = tr("Sensibilité"),
                    title = titre %||% tr("Courbe ROC"),
                    subtitle = trf("Aire sous la courbe : %.3f", res$roc$auc))))
  }

  if (identical(analyse, "impact")) {
    d <- res$table[res$table$.cle == "prop", , drop = FALSE]
    d <- d[is.finite(d$Valeur), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    d$Mesure <- factor(d$Mesure, levels = rev(d$Mesure))
    return(th(ggplot2::ggplot(d, ggplot2::aes(x = .data[["Valeur"]], y = .data[["Mesure"]])) +
      ggplot2::geom_vline(xintercept = 0, colour = "grey40") +
      ggplot2::geom_col(fill = "#d73027", alpha = 0.8) +
      ggplot2::labs(x = tr("Proportion (0 à 1)"), y = NULL,
                    title = titre %||% tr("Mesures d'impact"))))
  }
  NULL
}

# Valeurs manquantes : le seuil de chaque methode
# Visualisation : une variable lue d'ensemble, sans le detail des niveaux de X
HSTAT_VIZ_X_REGROUPABLE <- c("box", "violin", "bar", "scatter")

hstat_viz_x_regroupe <- function(coche, type) {
  isTRUE(coche) && length(type) == 1L && type %in% HSTAT_VIZ_X_REGROUPABLE
}

# Une methode d'imputation n'est pas bonne ou mauvaise dans l'absolu : elle
# l'est jusqu'a une certaine proportion de valeurs manquantes.
HSTAT_NA_METHODES <- data.frame(
  code = c("remove", "mean", "median", "mode", "knn", "mice", "rf", "value"),
  libelle = c(
    "Supprimer les lignes contenant des NA (moins de 5 % de NA)",
    "Remplacer par la moyenne, numériques (5 à 15 % de NA)",
    "Remplacer par la médiane, numériques (5 à 15 % de NA)",
    "Remplacer par le mode, catégorielles (5 à 15 % de NA)",
    "Imputation par k plus proches voisins, KNN (5 à 20 % de NA)",
    "Imputation multiple par équations chaînées, MICE/PMM (5 à 30 % de NA)",
    "Imputation par forêts aléatoires, missForest (5 à 30 % de NA)",
    "Remplacer par une valeur spécifique (quand le NA a un sens connu)"),
  nom = c(
    "Suppression des lignes", "Remplacement par la moyenne",
    "Remplacement par la médiane", "Remplacement par le mode",
    "KNN (k plus proches voisins)", "MICE/PMM (imputation multiple)",
    "missForest (forêts aléatoires)", "Valeur spécifique"),
  seuil = c(
    "Moins de 5 %", "5 à 15 %", "5 à 15 %", "5 à 15 %",
    "5 à 20 %", "5 à 30 %", "5 à 30 %", "Selon le sens du NA"),
  usage = c(
    "La perte de lignes reste faible ; au-delà, l'échantillon s'appauvrit et se biaise.",
    "Rapide, mais réduit la variance : au-delà de 15 %, préférer une imputation multiple.",
    "Comme la moyenne, et robuste aux valeurs extrêmes.",
    "Pour une variable catégorielle, jusqu'à 15 % de NA.",
    "Exploite la ressemblance entre observations ; se dégrade quand les voisins sont eux-mêmes incomplets.",
    "Imputation multiple, recommandée pour des NA non négligeables (MAR) ; au-delà de 30 %, résultats exploratoires.",
    "Non paramétrique, gère le mélange numérique / catégoriel ; éprouvé jusqu'à 30 %.",
    "Quand le manquant signifie une valeur connue (absence = 0, par exemple)."),
  max = c(5, 15, 15, 15, 20, 30, 30, NA),
  stringsAsFactors = FALSE)

# Au-dela de ce pourcentage, aucune methode n'est sure : retirer la variable
# merite d'etre envisage.
HSTAT_NA_SEUIL_RETRAIT <- 30

# Choix du bouton radio, derives de la table : le seuil est ECRIT dans le
# libelle, la ou l'on choisit.
hstat_na_choix <- function() {
  stats::setNames(HSTAT_NA_METHODES$code, HSTAT_NA_METHODES$libelle)
}

# Adequation d'une methode au pourcentage de NA observe.
hstat_na_adequation <- function(methode, pct) {
  i <- match(methode, HSTAT_NA_METHODES$code)
  if (is.na(i) || length(pct) != 1L || !is.finite(pct)) return(NA_character_)
  m <- HSTAT_NA_METHODES$max[i]
  if (is.na(m)) return("sans_seuil")
  if (pct <= m) "adaptee" else "au-dela"
}

# Recommandation tiree des memes seuils.
hstat_na_recommandation <- function(pct, categorielle = FALSE) {
  if (length(pct) != 1L || !is.finite(pct) || pct <= 0)
    return(list(txt = "Aucune valeur manquante : aucun traitement nécessaire.", m = NULL))
  if (pct < 5)
    return(list(txt = "Moins de 5 % de NA : la suppression des lignes est acceptable, ou une imputation simple (médiane).", m = "median"))
  if (pct <= 20 && isTRUE(categorielle))
    return(list(txt = "NA modérés avec variables catégorielles : missForest (gère le mixte, jusqu'à 30 %) ou MICE/PMM sont recommandés.", m = "rf"))
  if (pct <= HSTAT_NA_SEUIL_RETRAIT)
    return(list(txt = "NA non négligeables (mécanisme MAR plausible) : l'imputation multiple MICE/PMM est recommandée, jusqu'à 30 % de NA.", m = "mice"))
  list(txt = "Plus de 30 % de NA sur au moins une variable : aucune méthode n'est sûre, envisagez de retirer la variable ; sinon MICE/PMM avec prudence.", m = "mice")
}

# Diagramme volcan
# Un volcan croise l'AMPLEUR d'un effet (abscisse : log2 du rapport, ou
# difference) et sa SIGNIFICATIVITE (ordonnee : -log10 de la p-value).
# 1. UNE P-VALUE NULLE N'A PAS DE LOGARITHME.
# 2. CE QUI N'EST PAS UNE P-VALUE EST ECARTE ET COMPTE : manquante, negative,
#    superieure a 1.
# 3. UN RAPPORT N'EST PAS UN LOG2.
HSTAT_VOLCAN_CLASSES <- c(hausse = "Hausse significative",
                          baisse = "Baisse significative",
                          ns = "Non significatif")

# -- Arbre CART : un predicteur qualitatif a trop de modalites.
HSTAT_CART_NIVEAUX_MAX <- 20L

hstat_cart_niveaux_bloquants <- function(d, cible, preds,
                                         max_niveaux = HSTAT_CART_NIVEAUX_MAX) {
  y <- d[[cible]]
  if (!is.factor(y) || nlevels(droplevels(y)) < 3L) return(integer(0))
  k <- vapply(preds, function(v) {
    x <- d[[v]]
    if (is.factor(x) || is.character(x)) length(unique(x[!is.na(x)])) else 0L
  }, integer(1))
  k[k > max_niveaux]
}

HSTAT_VOLCAN_LABELS_MAX <- 50L

hstat_volcan_donnees <- function(d, x_var, p_var, seuil_fc = 1, seuil_p = 0.05,
                                 x_mode = c("log2", "rapport"),
                                 p_mode = c("brute", "log10"),
                                 label_var = NULL, n_labels = 10L) {
  x_mode <- match.arg(x_mode); p_mode <- match.arg(p_mode)
  if (!x_var %in% names(d) || !p_var %in% names(d))
    stop(tr("Les variables du volcan sont absentes des données."), call. = FALSE)
  seuil_fc <- suppressWarnings(as.numeric(seuil_fc))[1]
  seuil_p  <- suppressWarnings(as.numeric(seuil_p))[1]
  if (!is.finite(seuil_fc) || seuil_fc < 0) seuil_fc <- 1
  if (!is.finite(seuil_p) || seuil_p <= 0 || seuil_p >= 1) seuil_p <- 0.05
  xv <- suppressWarnings(as.numeric(d[[x_var]]))
  pv <- suppressWarnings(as.numeric(d[[p_var]]))
  n0 <- nrow(d)
  ecart_x <- 0L; ecart_p <- 0L; nuls <- 0L
  if (x_mode == "rapport") {
    bad <- !is.finite(xv) | xv <= 0
    ecart_x <- sum(bad & !is.na(xv))
    xv[bad] <- NA_real_
    xv <- log2(xv)
  }
  if (p_mode == "brute") {
    bad <- !is.finite(pv) | pv < 0 | pv > 1
    ecart_p <- sum(bad & !is.na(pv))
    pv[bad] <- NA_real_
    pos <- pv[is.finite(pv) & pv > 0]
    zero <- which(is.finite(pv) & pv == 0)
    nuls <- length(zero)
    if (nuls) pv[zero] <- if (length(pos)) min(pos) else NA_real_
    yv <- -log10(pv)
  } else {
    bad <- !is.finite(pv) | pv < 0
    ecart_p <- sum(bad & !is.na(pv))
    pv[bad] <- NA_real_
    yv <- pv
  }
  garde <- is.finite(xv) & is.finite(yv)
  out <- d[garde, , drop = FALSE]
  out$.x <- xv[garde]
  out$.y <- yv[garde]
  y_seuil <- -log10(seuil_p)
  sig <- out$.y > y_seuil
  cl <- ifelse(sig & out$.x > 0 & out$.x >= seuil_fc, "hausse",
        ifelse(sig & out$.x < 0 & out$.x <= -seuil_fc, "baisse", "ns"))
  out$.classe <- factor(cl, levels = names(HSTAT_VOLCAN_CLASSES))
  # Les etiquettes vont aux points significatifs les plus hauts : ce sont ceux
  # qu'on cherche du regard.
  out$.label <- NA_character_
  # Le curseur s'arrete a 50 ; la valeur vient pourtant du navigateur.
  n_labels <- min(suppressWarnings(as.integer(n_labels))[1], HSTAT_VOLCAN_LABELS_MAX)
  if (!is.null(label_var) && label_var %in% names(out) && is.finite(n_labels) &&
      n_labels > 0) {
    idx <- which(out$.classe != "ns")
    idx <- idx[order(-out$.y[idx], -abs(out$.x[idx]))]
    idx <- utils::head(idx, n_labels)
    out$.label[idx] <- as.character(out[[label_var]][idx])
  }
  attr(out, "seuils") <- c(fc = seuil_fc, p = seuil_p, y = y_seuil)
  attr(out, "bilan") <- c(lues = n0, gardees = nrow(out),
                          ecartees_x = ecart_x, ecartees_p = ecart_p,
                          p_nulles = nuls,
                          hausse = sum(out$.classe == "hausse"),
                          baisse = sum(out$.classe == "baisse"),
                          ns = sum(out$.classe == "ns"))
  rownames(out) <- NULL
  out
}

# ggrepel n'a pas de traduction plotly : `ggplotly` laisse tomber la couche, et
# le rendu interactif perdait les noms des points.
hstat_sans_repel <- function(p) {
  y <- if (is.data.frame(p$data) && ".y" %in% names(p$data)) p$data$.y else numeric(0)
  y <- y[is.finite(y)]
  dy <- if (length(y) > 1L) diff(range(y)) * 0.035 else 0
  for (i in seq_along(p$layers)) {
    l <- p$layers[[i]]
    if (inherits(l$geom, c("GeomTextRepel", "GeomLabelRepel")))
      p$layers[[i]] <- ggplot2::geom_text(
        data = l$data, mapping = l$mapping,
        position = ggplot2::position_nudge(y = dy),
        size = l$aes_params$size %||% 3.5, show.legend = FALSE)
  }
  p
}

# Figure : points par classe, lignes de seuil pointillees, etiquettes.
hstat_volcan_plot <- function(vd, couleurs = c(hausse = "#d7301f",
                                               baisse = "#2b8cbe",
                                               ns = "grey65"),
                              taille = 2, alpha = 0.8, montrer_seuils = TRUE,
                              taille_label = 3.5) {
  s <- attr(vd, "seuils")
  b <- attr(vd, "bilan")
  lib <- vapply(names(HSTAT_VOLCAN_CLASSES), function(k)
    sprintf("%s (n = %d)", tr(HSTAT_VOLCAN_CLASSES[[k]]), as.integer(b[[k]])),
    character(1))
  couleurs <- unname(couleurs[names(HSTAT_VOLCAN_CLASSES)])
  # La couleur porte le LIBELLE et non le code.
  vd$.classe_lib <- factor(unname(lib[as.character(vd$.classe)]), levels = unname(lib))
  p <- ggplot2::ggplot(vd, ggplot2::aes(x = .data$.x, y = .data$.y,
                                        colour = .data$.classe_lib)) +
    ggplot2::geom_point(size = taille, alpha = alpha, na.rm = TRUE) +
    ggplot2::scale_colour_manual(name = NULL, values = stats::setNames(couleurs, lib),
                                 breaks = unname(lib), drop = FALSE)
  if (isTRUE(montrer_seuils) && !is.null(s)) {
    p <- p + ggplot2::geom_hline(yintercept = s[["y"]], linetype = "dashed",
                                 colour = "grey40", linewidth = 0.4)
    if (s[["fc"]] > 0)
      p <- p + ggplot2::geom_vline(xintercept = c(-s[["fc"]], s[["fc"]]),
                                   linetype = "dashed", colour = "grey40",
                                   linewidth = 0.4)
  }
  lab <- vd[!is.na(vd$.label), , drop = FALSE]
  if (nrow(lab)) {
    p <- p + if (requireNamespace("ggrepel", quietly = TRUE))
      ggrepel::geom_text_repel(data = lab, ggplot2::aes(label = .data$.label),
                               size = taille_label, show.legend = FALSE,
                               max.overlaps = Inf, min.segment.length = 0)
    else ggplot2::geom_text(data = lab, ggplot2::aes(label = .data$.label),
                            size = taille_label, vjust = -0.7,
                            show.legend = FALSE)
  }
  p
}

# Numeros de lignes des valeurs identifiees (manquantes, aberrantes)
# Le numero rendu est le NOM DE LIGNE du jeu de travail, celui qu'affiche
# l'apercu des donnees nettoyees (`rownames = TRUE`).

# Suite d'entiers ecrite en plages : 3, 5-8, 12.
hstat_lignes_compactes <- function(x, max = 100L) {
  x <- as.character(x)
  if (!length(x)) return("")
  couper <- function(m) {
    if (length(m) > max) c(m[seq_len(max)], sprintf("\u2026 (+%d)", length(m) - max)) else m
  }
  n <- suppressWarnings(as.integer(x))
  if (anyNA(n) || any(as.character(n) != x)) return(paste(couper(x), collapse = ", "))
  n <- sort(unique(n))
  coupe <- c(0L, which(diff(n) != 1L), length(n))
  morceaux <- vapply(seq_len(length(coupe) - 1L), function(i) {
    a <- n[coupe[i] + 1L]; b <- n[coupe[i + 1L]]
    if (a == b) as.character(a) else paste0(a, "-", b)
  }, character(1))
  paste(couper(morceaux), collapse = ", ")
}

.hstat_noms_lignes <- function(d) {
  rn <- rownames(d)
  if (is.null(rn) || length(rn) != nrow(d)) rn <- as.character(seq_len(nrow(d)))
  rn
}

# Une ligne par variable portant au moins une valeur manquante : effectif et
# numeros de lignes.
hstat_na_lignes <- function(d, vars = names(d)) {
  vars <- intersect(vars, names(d))
  rn <- .hstat_noms_lignes(d)
  res <- lapply(vars, function(v) {
    i <- which(is.na(d[[v]]))
    if (!length(i)) return(NULL)
    data.frame(Variable = v, `NA (n)` = length(i),
               Lignes = hstat_lignes_compactes(rn[i]),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, res)
  if (is.null(res))
    res <- data.frame(Variable = character(0), `NA (n)` = integer(0),
                      Lignes = character(0), check.names = FALSE,
                      stringsAsFactors = FALSE)
  res
}

# Bornes (basse, haute) de detection des valeurs aberrantes.
hstat_outlier_bornes <- function(x, method = "iqr", iqr_k = 1.5, z_thr = 3) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(c(NA_real_, NA_real_))
  if (identical(method, "iqr")) {
    q <- stats::quantile(x, c(.25, .75), names = FALSE)
    iqrv <- q[2] - q[1]
    c(q[1] - iqr_k * iqrv, q[2] + iqr_k * iqrv)
  } else if (identical(method, "zscore")) {
    m <- mean(x); s <- stats::sd(x)
    c(m - z_thr * s, m + z_thr * s)
  } else {  # mad
    med <- stats::median(x); madv <- stats::mad(x)
    if (madv == 0) madv <- 1e-9
    c(med - z_thr * madv, med + z_thr * madv)
  }
}

# Une ligne par valeur aberrante : la ligne, la variable, la valeur et le cote
# franchi.
hstat_outlier_lignes <- function(d, vars, method = "iqr", iqr_k = 1.5, z_thr = 3) {
  vars <- intersect(vars, names(d))
  rn <- .hstat_noms_lignes(d)
  res <- lapply(vars, function(v) {
    x <- d[[v]]
    if (!is.numeric(x)) return(NULL)
    b <- hstat_outlier_bornes(x, method, iqr_k, z_thr)
    if (anyNA(b)) return(NULL)
    i <- which(is.finite(x) & (x < b[1] | x > b[2]))
    if (!length(i)) return(NULL)
    data.frame(Ligne = rn[i], Variable = v, Valeur = x[i],
               `Côté` = ifelse(x[i] < b[1], tr("sous la borne basse"),
                               tr("au-dessus de la borne haute")),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, res)
  if (is.null(res))
    res <- data.frame(Ligne = character(0), Variable = character(0),
                      Valeur = numeric(0), `Côté` = character(0),
                      check.names = FALSE, stringsAsFactors = FALSE)
  rownames(res) <- NULL
  res
}

# « Tout sélectionner » / « Tout désélectionner » : une couleur chacun
# Quatorze boutons dans quatre modules portaient chacun leur couleur -- vert,
# rouge, orange ou gris selon le module.
hstat_btn_tout <- function(inputId, label = "Tout sélectionner", ..., class = NULL) {
  shiny::actionButton(inputId, label, ...,
                      class = paste(c("hstat-btn-tout", class), collapse = " "))
}

hstat_btn_aucun <- function(inputId, label = "Tout désélectionner", ..., class = NULL) {
  shiny::actionButton(inputId, label, ...,
                      class = paste(c("hstat-btn-aucun", class), collapse = " "))
}

# DOUBLONS : reperer, puis traiter (supprimer, marquer, modifier)
# Deux lignes sont des doublons quand elles portent les MEMES valeurs sur les
# colonnes de comparaison.
# 1. DEUX NA SONT EGAUX.
# 2. LA LIGNE SE NOMME PAR SON NOM DE LIGNE, jamais par sa position.
# 3. LA CASSE ET LES ESPACES NE S'IGNORENT QUE SUR DEMANDE.
.hstat_doublons_cle <- function(d, cles, ignorer_casse = FALSE, ignorer_espaces = FALSE) {
  parts <- lapply(cles, function(cn) {
    x <- d[[cn]]
    v <- if (inherits(x, c("Date", "POSIXt"))) format(x)
         else if (is.numeric(x)) format(x, digits = 15, trim = TRUE)
         else as.character(x)
    if (is.character(x) || is.factor(x)) {
      if (isTRUE(ignorer_espaces)) v <- gsub("[[:space:]]+", " ", trimws(v))
      if (isTRUE(ignorer_casse)) v <- tolower(v)
    }
    v[is.na(x)] <- "\u0001NA"
    v
  })
  if (!length(parts)) return(character(NROW(d)))
  do.call(paste, c(parts, sep = "\u001f"))
}

hstat_doublons <- function(d, cles = NULL, ignorer_casse = FALSE, ignorer_espaces = FALSE) {
  vide <- function(cles) structure(
    data.frame(Groupe = integer(0), Ligne = character(0), stringsAsFactors = FALSE),
    n_groupes = 0L, n_lignes = 0L, n_superflues = 0L, cles = cles)
  if (is.null(d) || !NROW(d) || !NCOL(d)) return(vide(character(0)))
  cles <- intersect(as.character(cles %||% character(0)), names(d))
  if (!length(cles)) cles <- names(d)
  k <- .hstat_doublons_cle(d, cles, ignorer_casse, ignorer_espaces)
  dup <- duplicated(k) | duplicated(k, fromLast = TRUE)
  if (!any(dup)) return(vide(cles))
  g <- match(k, unique(k[dup]))
  i <- which(dup)
  i <- i[order(g[i], i)]
  res <- data.frame(Groupe = g[i], Ligne = rownames(d)[i],
                    d[i, , drop = FALSE], check.names = FALSE,
                    stringsAsFactors = FALSE)
  rownames(res) <- NULL
  structure(res, n_groupes = length(unique(g[i])), n_lignes = length(i),
            n_superflues = length(i) - length(unique(g[i])), cles = cles)
}

# Les lignes SUPERFLUES : toutes sauf une par groupe -- la premiere ou la
# derniere occurrence, au choix.
hstat_doublons_superflus <- function(d, cles = NULL, garder = c("premier", "dernier"),
                                     ignorer_casse = FALSE, ignorer_espaces = FALSE) {
  garder <- match.arg(garder)
  if (is.null(d) || !NROW(d) || !NCOL(d)) return(character(0))
  cles <- intersect(as.character(cles %||% character(0)), names(d))
  if (!length(cles)) cles <- names(d)
  k <- .hstat_doublons_cle(d, cles, ignorer_casse, ignorer_espaces)
  rownames(d)[duplicated(k, fromLast = identical(garder, "dernier"))]
}

# UNE VALEUR SAISIE PREND LE TYPE DE SA COLONNE.
hstat_valeur_saisie <- function(col, valeur) {
  v <- trimws(as.character(valeur %||% "")[1])
  if (is.na(v) || !nzchar(v) || toupper(v) == "NA")
    return(list(ok = TRUE, valeur = col[NA_integer_][1]))
  if (is.logical(col)) {
    b <- switch(tolower(v), "true" = , "vrai" = , "oui" = , "1" = TRUE,
                "false" = , "faux" = , "non" = , "0" = FALSE, NA)
    if (is.na(b)) return(list(ok = FALSE, texte = trf("« %s » n'est pas une valeur logique (vrai / faux).", v)))
    return(list(ok = TRUE, valeur = b))
  }
  if (is.numeric(col)) {
    n <- suppressWarnings(as.numeric(gsub(",", ".", v, fixed = TRUE)))
    if (!is.finite(n)) return(list(ok = FALSE, texte = trf("« %s » n'est pas un nombre.", v)))
    if (is.integer(col) && n == round(n) && abs(n) < .Machine$integer.max) n <- as.integer(n)
    return(list(ok = TRUE, valeur = n))
  }
  if (inherits(col, "Date")) {
    dt <- hstat_date_auto(v)$dates
    if (!length(dt) || is.na(dt[1])) return(list(ok = FALSE, texte = trf("« %s » n'est pas une date lisible.", v)))
    return(list(ok = TRUE, valeur = as.Date(dt[1])))
  }
  if (inherits(col, "POSIXt")) {
    dt <- suppressWarnings(tryCatch(as.POSIXct(v, tz = attr(col, "tzone") %||% ""),
                                    error = function(e) NA))
    if (!length(dt) || is.na(dt[1])) return(list(ok = FALSE, texte = trf("« %s » n'est pas une date lisible.", v)))
    return(list(ok = TRUE, valeur = dt[1]))
  }
  list(ok = TRUE, valeur = v)
}

# Ecrit la valeur dans la colonne.
hstat_ecrire_cellule <- function(d, ligne, colonne, valeur) {
  if (is.factor(d[[colonne]]) && !is.na(valeur) && !(valeur %in% levels(d[[colonne]])))
    levels(d[[colonne]]) <- c(levels(d[[colonne]]), valeur)
  d[ligne, colonne] <- valeur
  d
}

# ANOVA a mesures repetees a plusieurs facteurs : termes, strates, post-hoc
# Le modele portait deja le plan factoriel complet (`A * B`), mais trois choses
# manquaient pour lire un plan a plusieurs facteurs.
# 1. CHOISIR LES INTERACTIONS.
# 2. LES STRATES D'ERREUR.
# 3. LES EFFETS SIMPLES.

HSTAT_RM_INTERACTIONS <- c(
  "Factoriel complet (toutes les interactions)" = "complet",
  "Interactions d'ordre 2 seulement"            = "ordre2",
  "Effets principaux seuls (sans interaction)"  = "aucune")

# Partie fixe de la formule.
hstat_rm_termes <- function(facteurs, interactions = "complet") {
  bt <- paste0("`", facteurs, "`")
  if (length(bt) < 2) return(bt)
  switch(as.character(interactions %||% "complet")[1],
         aucune = paste(bt, collapse = " + "),
         ordre2 = if (length(bt) == 2) paste(bt, collapse = " * ")
                  else paste0("(", paste(bt, collapse = " + "), ")^2"),
         paste(bt, collapse = " * "))
}

# Partie aleatoire : le sujet, puis une strate par combinaison STRICTE de
# facteurs intra (sujet x A, sujet x B, sujet x A x B a trois facteurs...),
# exactement les strates d'erreur de l'ANOVA a mesures repetees classique.
hstat_rm_aleatoire <- function(sujet, within, d) {
  s <- paste0("`", sujet, "`")
  termes <- paste0("(1 | ", s, ")")
  k <- length(within)
  if (k >= 2) {
    for (taille in seq_len(k - 1)) {
      for (cmb in utils::combn(within, taille, simplify = FALSE)) {
        niv <- nlevels(interaction(d[c(sujet, cmb)], drop = TRUE))
        if (niv < nrow(d))
          termes <- c(termes, paste0("(1 | ", paste(c(s, paste0("`", cmb, "`")),
                                                    collapse = ":"), ")"))
      }
    }
  }
  paste(termes, collapse = " + ")
}

# Comparaisons par paires : effets principaux, puis effets simples de chaque
# facteur a chaque niveau de chacun des autres.
hstat_rm_posthoc <- function(modele, within, between = character(0),
                             interactions = "complet", adj = "holm",
                             alpha = 0.05, variable = "", p_effets = NULL,
                             roles = c(intra = "Intra", inter = "Inter")) {
  facteurs <- c(within, between)
  role <- function(f) if (f %in% within) roles[["intra"]] else roles[["inter"]]
  lignes <- list()
  ajoute <- function(ph, f, role_txt, condition) {
    if (is.null(ph) || nrow(ph) == 0) return()
    pcol <- if ("p.value" %in% colnames(ph)) "p.value" else utils::tail(colnames(ph), 1)
    ecol <- if ("estimate" %in% colnames(ph)) "estimate" else colnames(ph)[2]
    p <- ph[[pcol]]
    lignes[[length(lignes) + 1L]] <<- data.frame(
      Variable = variable, Role = role_txt, Facteur = f,
      Condition = condition, Comparaison = as.character(ph[[1]]),
      Estimation = round(ph[[ecol]], 4), p_value = round(p, 5),
      Significatif = ifelse(!is.na(p) & p < alpha, "Oui", "n.s."),
      stringsAsFactors = FALSE)
  }
  emm_paires <- function(...) tryCatch(suppressMessages(as.data.frame(
    emmeans::contrast(emmeans::emmeans(modele, ...), method = "pairwise",
                      adjust = adj))), error = function(e) NULL)
  for (f in facteurs) ajoute(emm_paires(specs = f), f, role(f), "")

  avec_inter <- length(facteurs) >= 2 &&
    !identical(as.character(interactions %||% "complet")[1], "aucune")
  if (avec_inter) {
    for (f1 in facteurs) for (f2 in setdiff(facteurs, f1)) {
      ph <- emm_paires(specs = f1, by = f2)
      if (is.null(ph) || !f2 %in% colnames(ph)) next
      for (niv in unique(as.character(ph[[f2]]))) {
        sel <- ph[as.character(ph[[f2]]) == niv, , drop = FALSE]
        ajoute(sel, f1, "Effet simple", paste0(f2, " = ", niv))
      }
    }
  }
  res <- if (length(lignes)) do.call(rbind, lignes) else NULL

  # Les interactions significatives se nomment.
  notes <- character(0)
  if (length(p_effets)) {
    noms <- gsub("`", "", names(p_effets))
    for (i in seq_along(p_effets)) {
      parts <- strsplit(noms[i], ":", fixed = TRUE)[[1]]
      p <- p_effets[[i]]
      if (length(parts) >= 2 && is.finite(p) && p < alpha)
        notes <- c(notes, trf(
          "L'interaction %s est significative (p = %s) : l'effet de chaque facteur dépend du niveau des autres. Lisez les effets simples ; les comparaisons d'un facteur seul moyennent sur les autres.",
          paste(parts, collapse = " × "), hstat_p_txt(p)))
    }
  }
  attr(res, "notes") <- notes
  res
}

# Moyennes ajustees par combinaison des deux premiers facteurs, pour le
# graphique d'interaction.
hstat_rm_moyennes <- function(modele, f1, f2, variable = "") {
  em <- tryCatch(suppressMessages(as.data.frame(
    emmeans::emmeans(modele, specs = c(f1, f2)))), error = function(e) NULL)
  if (is.null(em) || nrow(em) == 0) return(NULL)
  lo <- intersect(c("lower.CL", "asymp.LCL"), colnames(em))[1]
  hi <- intersect(c("upper.CL", "asymp.UCL"), colnames(em))[1]
  data.frame(Variable = variable, X = em[[f1]], Groupe = em[[f2]],
             Moyenne = em$emmean,
             Bas = if (!is.na(lo)) em[[lo]] else NA_real_,
             Haut = if (!is.na(hi)) em[[hi]] else NA_real_,
             stringsAsFactors = FALSE)
}

hstat_rm_graphique_interaction <- function(d, f1, f2, theme = NULL) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  dodge <- ggplot2::position_dodge(width = 0.15)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$X, y = .data$Moyenne,
                                       colour = .data$Groupe, group = .data$Groupe)) +
    ggplot2::geom_line(position = dodge, linewidth = 0.8) +
    ggplot2::geom_point(position = dodge, size = 2.4)
  if (any(is.finite(d$Bas)))
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$Bas, ymax = .data$Haut),
                                    width = 0.12, position = dodge)
  if (length(unique(d$Variable)) > 1)
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$Variable), scales = "free_y")
  p + ggplot2::labs(x = f1, y = tr("Moyenne ajustée (IC 95 %)"), colour = f2,
                    title = trf("Interaction %s × %s", f1, f2)) +
    (theme %||% ggplot2::theme_minimal())
}

# Barres groupees et courbes a barres d'erreur, dans les couleurs de ggplot2

# Les couleurs par defaut de ggplot2 : celles que `scale_colour_hue()` pose
# d'elle-meme.
hstat_couleurs_series <- function(n) {
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1L || is.na(n) || n < 1L) return(character(0))
  scales::hue_pal()(n)
}

# Ce que portent les barres d'erreur.
HSTAT_VIZ_ERREURS <- c(
  "Aucune"                                    = "aucune",
  "Erreur-type (moyenne ± SE)"           = "se",
  "Écart-type (moyenne ± SD)"       = "sd",
  "Intervalle de confiance à 95 %"       = "ic95"
)

# Moyenne et barre d'erreur par abscisse (et par serie, et par facette).
hstat_viz_resume_erreur <- function(d, x, y, groupes = NULL, type = "se") {
  type <- if (length(type) == 1L && type %in% HSTAT_VIZ_ERREURS) type else "se"
  groupes <- intersect(setdiff(unique(groupes), c(x, y)), names(d))
  cles <- c(x, groupes)
  d <- d[!is.na(d[[x]]), , drop = FALSE]
  v <- suppressWarnings(as.numeric(d[[y]]))
  ok <- is.finite(v)
  d <- d[ok, cles, drop = FALSE]
  d$.v <- v[ok]
  if (!nrow(d)) {
    vide <- d[0, cles, drop = FALSE]
    vide[[y]] <- numeric(0); vide$.bas <- numeric(0)
    vide$.haut <- numeric(0); vide$.n <- integer(0)
    return(vide)
  }
  r <- d %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(cles))) %>%
    dplyr::summarise(.moy = mean(.data$.v), .et = stats::sd(.data$.v),
                     .n = dplyr::n(), .groups = "drop") %>%
    as.data.frame(stringsAsFactors = FALSE)
  demi <- switch(type,
    aucune = rep(NA_real_, nrow(r)),
    sd     = r$.et,
    se     = r$.et / sqrt(r$.n),
    ic95   = suppressWarnings(stats::qt(0.975, pmax(r$.n - 1, 1))) * r$.et / sqrt(r$.n))
  demi[r$.n < 2] <- NA_real_
  r[[y]]  <- r$.moy
  r$.bas  <- r$.moy - demi
  r$.haut <- r$.moy + demi
  r[, c(cles, y, ".bas", ".haut", ".n"), drop = FALSE]
}

# Largeur d'une barre d'erreur, en unites de l'axe X.
hstat_largeur_erreur <- function(x, part = 0.3) {
  part <- suppressWarnings(as.numeric(part))
  if (length(part) != 1L || !is.finite(part) || part <= 0) part <- 0.3
  if (is.factor(x) || is.character(x) || is.logical(x)) return(part)
  u <- sort(unique(suppressWarnings(as.numeric(x))))
  u <- u[is.finite(u)]
  if (length(u) < 2L) return(part)
  part * min(diff(u))
}

# Les barres de plusieurs series portees sur l'axe secondaire se posent en UNE
# couche, decalees cote a cote.
hstat_barres_groupees <- function(traces, x, y = ".y2_scaled",
                                  serie = ".curve_label", alpha = 1) {
  traces <- Filter(function(t) is.data.frame(t) && nrow(t) > 0 &&
                     all(c(x, y, serie) %in% names(t)), traces)
  if (!length(traces)) return(NULL)
  d <- do.call(rbind, lapply(traces, function(t) t[, c(x, y, serie), drop = FALSE]))
  d[[serie]] <- factor(d[[serie]], levels = unique(names(traces) %||% d[[serie]]))
  ggplot2::geom_col(
    data = d, inherit.aes = FALSE,
    ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[serie]]),
    position = ggplot2::position_dodge(), alpha = alpha,
    # NA et non TRUE : TRUE forcerait la couche dans TOUTES les legendes, et
    # chaque cle de courbe porterait un carre gris de barre.
    na.rm = TRUE, show.legend = NA)
}

# Vit dans le socle et non dans `mod_dosage.R`.
hstat_ma_input <- function(id, label, selected = "", saisies = character(0),
                           placeholder = NULL) {
  selected <- trimws(as.character(selected %||% ""))[1]
  if (is.na(selected)) selected <- ""
  choix <- hstat_ma_choix(c(saisies, selected))
  shiny::selectizeInput(id, label, choices = c("", choix), selected = selected,
    # `delimiter` : par defaut la virgule, qui couperait « 2,4-D (amine salt) »
    # en deux entrees a la creation.
    options = list(create = TRUE, createOnBlur = TRUE, persist = FALSE,
                   delimiter = "\u001f",
                   maxOptions = 500, placeholder = placeholder %||% "",
                   render = I("{option_create: function(d, e) { return '<div class=\"create\">+ <strong>' + e(d.input) + '</strong></div>'; }}")))
}

# Statistiques de diffusion : le site, le choix memorise, et le compteur de la
# page. Un refus (option ou choix memorise) retire le compteur de la page.
HSTAT_STATS_SITE <- "https://hstatistique.goatcounter.com"

.hstat_stats_fichier <- function(nom = "statistiques.txt") {
  d <- tryCatch(tools::R_user_dir("HStat", "config"), error = function(e) "")
  if (!is.character(d) || length(d) != 1L || !nzchar(d)) return("")
  file.path(d, nom)
}

.hstat_stats_lire <- function(fichier) {
  if (!nzchar(fichier) || !file.exists(fichier)) return(NA)
  v <- tryCatch(trimws(readLines(fichier, n = 1L, warn = FALSE)),
                error = function(e) character(0))
  if (length(v) == 1L && v %in% c("oui", "non")) identical(v, "oui") else NA
}

# Le compteur GoatCounter de la page : une ouverture de l'application, sous le
# chemin /application/<version>. allow_local : sans lui, le script ignore une
# page servie depuis cette machine, le cas ordinaire.
hstat_stats_script <- function(version = hstat_version(),
                               site = getOption("hstat.statistiques.site", HSTAT_STATS_SITE),
                               fichier = .hstat_stats_fichier()) {
  if (isFALSE(getOption("hstat.statistiques"))) return(NULL)
  if (isFALSE(.hstat_stats_lire(fichier))) return(NULL)
  if (!is.character(site) || length(site) != 1L ||
      !grepl("^https://[A-Za-z0-9.-]+$", site)) return(NULL)
  version <- gsub("[^0-9.]", "", as.character(version))
  shiny::tags$script(
    `data-goatcounter` = paste0(site, "/count"),
    `data-goatcounter-settings` = sprintf(
      '{"allow_local": true, "path": "/application/%s", "title": "HStat %s", "referrer": ""}',
      version, version),
    async = NA, src = "https://gc.zgo.at/count.js")
}
