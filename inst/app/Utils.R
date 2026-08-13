
options(encoding = "UTF-8")

# Limite de taille des fichiers televerses (defaut Shiny = 5 Mo).
# Shiny ecrit l'upload en continu sur disque (pas en RAM) : la seule vraie
# contrainte est l'espace disque et la patience reseau. Defaut : 100 Go,
# configurable via HSTAT_MAX_UPLOAD_MB. Reduire cette valeur en cas de
# deploiement multi-utilisateurs.
.hstat_max_mb <- suppressWarnings(as.numeric(Sys.getenv("HSTAT_MAX_UPLOAD_MB", "102400")))
if (!is.finite(.hstat_max_mb) || .hstat_max_mb <= 0) .hstat_max_mb <- 102400
options(shiny.maxRequestSize = .hstat_max_mb * 1024^2)

# --- Qualite d'affichage des graphiques --------------------------------------

options(shiny.plot.res = 96)

# --- Boutons d'export DT avec noms de fichiers propres -----------------------

.hstat_dt_buttons <- function(fname = "HStat_export") {
  fname <- gsub("[^A-Za-z0-9_.-]+", "_", as.character(fname))
  list(
    list(extend = "copy",  title = NULL),
    list(extend = "csv",   filename = fname, title = NULL),
    list(extend = "excel", filename = fname, title = NULL)
  )
}

# --- Locale UTF-8 robuste ----------------------------------------------------

local({
  candidates <- if (.Platform$OS.type == "windows") {
    c("French_France.utf8", "French_France.1252", "C.UTF-8", "en_US.UTF-8")
  } else {
    c("fr_FR.UTF-8", "fr_FR.utf8", "en_US.UTF-8", "C.UTF-8", "C.utf8")
  }
  for (loc in candidates) {
    ok <- tryCatch(
      suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "",
      error = function(e) FALSE)
    if (isTRUE(ok)) break
  }
})



install_and_load <- function(packages) {
  installed_packages <- rownames(installed.packages())
  to_install <- packages[!packages %in% installed_packages]

  if (length(to_install) > 0) {
    # On tente directement l'installation : install.packages() gere lui-meme
    # l'absence de reseau. (L'ancienne detection "en ligne" via url() traitait
    # tout avertissement SSL/proxy comme hors-ligne et n'installait alors RIEN,
    # ce qui provoquait ensuite une UI incomplete -> "No UI defined".)
    repos <- getOption("repos")
    if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@") ||
        is.na(repos["CRAN"]))
      repos <- c(CRAN = "https://cloud.r-project.org")
    ok <- tryCatch({
      install.packages(to_install, repos = repos)
      TRUE
    }, error = function(e) FALSE, warning = function(e) FALSE)

    still_missing <- to_install[!to_install %in% rownames(installed.packages())]
    if (length(still_missing) > 0) {
      message("\n", strrep("=", 70),
              "\n  HStat -- certains paquets n'ont pas pu etre installes",
              "\n  Manquants : ", paste(still_missing, collapse = ", "),
              "\n  Verifiez votre connexion Internet, puis relancez l'application.",
              "\n  (Installation manuelle : install.packages(c(...)) )",
              "\n", strrep("=", 70), "\n")
    }
  }

  # Chargement : on n'interrompt pas l'application pour un package optionnel manquant
  missing_after <- character(0)
  for (pkg in packages) {
    # suppressMessages est indispensable : les messages "Registered S3
    # method(s) overwritten by ..." ne sont PAS des messages de demarrage
    # de package ; suppressPackageStartupMessages ne les masque donc pas.
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
    message("HStat : packages indisponibles (certaines fonctions seront limitees) : ",
            paste(missing_after, collapse = ", "))
}

required_packages <- c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets", "shinyalert", "DT", "shinycssloaders",
  "RColorBrewer", "colourpicker", "ggrepel",  "openxlsx", "zip", "rmarkdown", "haven", "base64enc",
  "dplyr", "knitr", "stringr", "scales", "ggplot2", "ggdendro", "reshape2", "sortable",
  "tibble", "plotrix", "plotly",  "qqplotr", "tidyr",  "report", "see", "corrplot",
  "car", "agricolae", "pwr", "forcats", "bslib", "factoextra",  "FactoMineR","questionr",  "digest",
  "MASS", "cluster", "GGally", "psych", "nortest", "lmtest", "multcomp","FSA", "treemapify", "ggtext",
  "stats",  "emmeans", "performance","purrr", "PMCMRplus","multcompView", "rcompanion", "EMT",
  "bestNormalize","lme4", "lmerTest", "afex", "ARTool", "glmmTMB", "vegan", "heplots", "data.table",
  "patchwork", "lavaan", "pls", "klaR", "poLCA", "clustMixType", "nnet", "DBI", "duckdb",
  "DescTools", "epitools", "htmltools", "magrittr", "readxl", "rlang", "svglite", "writexl",
  "mice", "missForest", "VIM"
)


install_and_load(required_packages)

# -- Packages de modelisation predictive (series temporelles / ML / DL) -------
# CES PACKAGES NE DOIVENT JAMAIS ETRE ATTACHES PAR library() : plusieurs
# d'entre eux masquent des fonctions vitales de l'application une fois sur le
# chemin de recherche (mclust::em masque shiny::em et casse toute l'interface
# avec "l'argument modelName est manquant" ; xgboost::slice masque
# dplyr::slice ; randomForest::margin masque ggplot2::margin ; pROC::var/cov
# masquent stats::var/cov...). Tous les appels du code HStat sont prefixe s
# par :: ; il suffit donc d'installer puis de charger leur espace de noms
# (loadNamespace), ce qui enregistre aussi leurs methodes S3 (predict, etc.)
# sans rien masquer.
hstat_model_packages <- c(
  "forecast", "glmnet", "rpart", "randomForest", "xgboost", "e1071",
  "kknn", "pROC", "dbscan", "mclust", "neuralnet",
  "dlm", "dlnm",          # modeles lineaires dynamiques & retards distribues
  "prophet", "torch"      # inclus (installes automatiquement)
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
  # L'installation se fait a la demande, via un bouton dedie du module
  # Deep Learning (avec barre de progression), ou manuellement par
  # torch::install_torch(). Ici on se contente d'informer.
  if (!"torch" %in% missing_after) {
    backend_ok <- tryCatch(torch::torch_is_installed(), error = function(e) FALSE)
    if (!isTRUE(backend_ok))
      message("HStat : les bibliotheques natives de torch ne sont pas encore ",
              "installees. Le module Deep Learning propose un bouton pour les ",
              "telecharger (~600 Mo, une seule fois) ; les modeles neuralnet ",
              "sont disponibles immediatement.")
  }
  if (length(missing_after) > 0)
    message("HStat : packages de modelisation indisponibles ",
            "(les modeles correspondants seront limites) : ",
            paste(missing_after, collapse = ", "))
  invisible(missing_after)
}
hstat_load_model_packages()

# -- Blindage contre la pollution du chemin de recherche -----------------------
# L'application ne controle pas l'environnement R de l'utilisateur : un
# .Rprofile, un workspace RStudio restaure ou un library() anterieur peut
# attacher des packages dont les exports masquent des fonctions vitales.
# Cas reel observe : mclust attache dans la session -> mclust::em(modelName,
# data, parameters) masque shiny::em() et la construction de l'interface
# echoue avec "l'argument modelName est manquant". De meme,
# randomForest::margin masquerait ggplot2::margin dans tous les themes.
# Ces liaisons globales explicites sont resolues AVANT le chemin de
# recherche par tout le code de l'application (source(local = FALSE)) :
# elles rendent l'interface immunisee, quel que soit l'etat de la session.
em     <- shiny::em
margin <- ggplot2::margin

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# --- Gardes pour packages d'UI optionnels -----------------------------------

.hstat_has <- function(pkg) isTRUE(requireNamespace(pkg, quietly = TRUE))

# shinycssloaders::withSpinner -> renvoie l'output tel quel si absent
if (!exists("withSpinner") && !.hstat_has("shinycssloaders")) {
  withSpinner <- function(ui_element, ...) ui_element
}

# plotly : sorties/rendu -> repli sur plotOutput/renderPlot statiques.

if (!.hstat_has("plotly")) {
  if (!exists("plotlyOutput"))
    plotlyOutput <- function(outputId, width = "100%", height = "400px", ...)
      shiny::plotOutput(outputId, width = width, height = height)
  if (!exists("ggplotly")) ggplotly <- function(p, ...) p
  # layout()/config() en no-op : renvoient l'objet graphique tel quel.

  layout <- function(p, ...) p
  config <- function(p, ...) p
  # Ne PAS redefinir %>% si un vrai pipe (magrittr via dplyr) existe deja : un repli
  # naif `rhs(lhs)` perdrait les arguments (p %>% layout(x=1) deviendrait layout(p)).
  if (!exists("%>%")) {
    if (.hstat_has("magrittr")) {
      `%>%` <- magrittr::`%>%`
    } else {
      # Repli minimal correct : insere lhs comme 1er argument de l'appel rhs.
      `%>%` <- function(lhs, rhs) {
        rc <- substitute(rhs)
        if (is.call(rc)) {
          new <- as.call(c(rc[[1]], substitute(lhs), as.list(rc)[-1]))
          eval(new, parent.frame())
        } else rhs(lhs)
      }
    }
  }
  if (!exists("renderPlotly"))
    renderPlotly <- function(expr, ...) {
      q <- substitute(expr)
      pf <- parent.frame()
      shiny::renderPlot({
        val <- eval(q, envir = pf)
        if (inherits(val, c("ggplot", "gg"))) print(val)
        else if (inherits(val, "grob")) grid::grid.draw(val)
        else val
      })
    }
}

# colourpicker::colourInput -> textInput de repli 
if (!.hstat_has("colourpicker")) {
  colourInput <- function(inputId, label, value = "#000000", ...)
    shiny::textInput(inputId, label, value = value)
}

# shinyWidgets : pickerInput / radioGroupButtons -> equivalents shiny de base
if (!.hstat_has("shinyWidgets")) {
  if (!exists("pickerInput"))
    pickerInput <- function(inputId, label = NULL, choices, selected = NULL,
                            multiple = FALSE, options = NULL, choicesOpt = NULL, ...)
      shiny::selectInput(inputId, label, choices = choices, selected = selected,
                         multiple = multiple)
  if (!exists("radioGroupButtons"))
    radioGroupButtons <- function(inputId, label = NULL, choices = NULL,
                                  selected = NULL, ...)
      shiny::radioButtons(inputId, label, choices = choices,
                          selected = selected %||% NULL, inline = TRUE)
  if (!exists("updatePickerInput"))
    updatePickerInput <- function(session, inputId, ..., choices = NULL,
                                  selected = NULL)
      shiny::updateSelectInput(session, inputId, choices = choices,
                               selected = selected)
}

# sortable::rank_list -> selecteur multiple ordonne de repli
if (!.hstat_has("sortable") && !exists("rank_list")) {
  rank_list <- function(text = NULL, labels = NULL, input_id, ...)
    shiny::selectInput(input_id, label = text, choices = labels,
                       selected = labels, multiple = TRUE)
}

.hstat_num1 <- function(x, default) {
  if (is.null(x) || length(x) == 0) return(default)
  v <- suppressWarnings(as.numeric(x[1]))
  if (length(v) == 0 || is.na(v) || !is.finite(v)) return(default)
  v
}

remove_zero_var_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(df)
  keep <- sapply(df, function(x) {
    if (!is.numeric(x)) return(TRUE)
    sd_val <- sd(x, na.rm = TRUE)
    !is.na(sd_val) && sd_val > 0
  })
  df[, keep, drop = FALSE]
}

safe_cor <- function(df, use = "pairwise.complete.obs") {
  if (is.null(df)) return(NULL)
  df <- df[, sapply(df, is.numeric), drop = FALSE]
  df <- remove_zero_var_cols(df)
  if (is.null(df) || ncol(df) < 2) return(NULL)
  tryCatch(suppressWarnings(cor(df, use = use)), error = function(e) NULL)
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
  # Mode "une variable contre les autres" : ne garder que les paires impliquant la
  # variable cible (target), et la placer systematiquement en Variable_X.
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
  names(df)[sapply(df, is_categorical)]
}

get_all_factor_candidates <- function(df, max_numeric_levels = 30) {
  nms <- names(df)
  keep <- sapply(nms, function(col) {
    x <- df[[col]]
    if (is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
    if (inherits(x, "Date") || inherits(x, "POSIXt"))    return(TRUE)
    if (is.numeric(x)) return(length(unique(na.omit(x))) <= max_numeric_levels)
    FALSE
  })
  nms[keep]
}

# Echappement HTML. Pose ici plutot que dans un module : le texte des
# repondants (atelier de codage), les reponses d'un modele de langue et les
# titres de rapport passent tous par la meme porte avant d'entrer dans le DOM.
# ===========================================================================
# BILINGUE (francais / anglais)
# ---------------------------------------------------------------------------
# Trois exigences, dans cet ordre :
#   HORS LIGNE   aucun appel reseau, jamais. Une API de traduction exigerait
#                une connexion, couterait a l'usage, et traduirait mal le
#                vocabulaire statistique — « test de l'etendue studentisee »
#                ne survit pas a un traducteur automatique.
#   LEGER        un CSV de paires, charge une fois. Quelques dizaines de Ko,
#                moins qu'une seule des polices embarquees.
#   COMPLET      l'application compte ~9 700 chaines destinees a l'utilisateur,
#                reparties sur 24 fichiers. Les envelopper une a une dans un
#                appel de traduction serait un chantier de plusieurs semaines
#                et casserait chaque ligne de code au passage.
#
# D'ou le choix : la traduction est appliquee AU TEXTE AFFICHE, dans le
# navigateur, a partir d'un dictionnaire embarque dans la page. L'interface
# construite par UX.R, les modules, les notifications et les tableaux rendus
# passent tous par le meme filtre, sans qu'aucun appel de code soit modifie.
#
# LA CLE EST LA CHAINE FRANCAISE ELLE-MEME. Consequence directe et voulue :
# une chaine absente du dictionnaire reste en francais au lieu d'afficher un
# identifiant technique. Une traduction incomplete degrade donc doucement,
# elle ne casse rien.
# ===========================================================================

HSTAT_LANGUES <- c("Francais" = "fr", "English" = "en")

# Le dictionnaire est cherche depuis le dossier de l'application, depuis la
# racine du depot, et en remontant : la suite de tests tourne depuis
# `tests/testthat/`, ou aucun des deux premiers chemins n'existe.
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
  # Une cle vide leverait « attempt to use zero-length variable name » : le
  # dictionnaire introuvable est un cas normal (paquet non installe, execution
  # depuis un dossier quelconque), il ne doit pas faire tomber le demarrage.
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
  # traduction imprevisible. En revanche « Exploration » se dit de la meme
  # facon dans les deux langues : c'est une DECISION de traduction, pas un
  # dechet, et la couverture doit la compter. Ce sont les seules a ne pas etre
  # envoyees au navigateur (voir hstat_i18n_json), ou elles ne feraient rien.
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
# fichiers telecharges, en-tetes de rapport). Une chaine inconnue ressort
# INCHANGEE : c'est la regle de degradation douce.
tr <- function(x, lang = "fr", dict = NULL) {
  if (is.null(x) || !length(x)) return(x)
  if (!identical(lang, "en")) return(x)
  if (is.null(dict)) dict <- hstat_i18n_dict(lang)
  if (!length(dict)) return(x)
  out <- as.character(x)
  hit <- match(out, names(dict))
  out[!is.na(hit)] <- unname(dict[hit[!is.na(hit)]])
  out
}

# Dictionnaire serialise pour le navigateur. Sans jsonlite (Suggests), on
# construit le JSON a la main — quelques echappements, et aucune dependance
# imposee pour une fonctionnalite d'interface.
hstat_i18n_json <- function(lang = "en", path = hstat_i18n_path()) {
  d <- hstat_i18n_dict(lang, path)
  # Les termes identiques dans les deux langues sont retires ICI : les envoyer
  # alourdirait la page pour un remplacement qui ne change rien a l'ecran.
  d <- d[names(d) != unname(d)]
  # LES GABARITS NE PARTENT PAS AU NAVIGATEUR. Une phrase composee par trf()
  # est traduite dans R, avant d'exister ; sa forme a marqueurs (« %s », « %d »)
  # n'apparait jamais telle quelle dans le DOM et ne pourrait donc jamais y
  # etre trouvee. L'envoyer alourdirait la page pour rien -- 30,8 Ko contre
  # 19,7 -- et la legerete est une promesse du bilingue.
  # Le motif vise les marqueurs de sprintf, pas le caractere « % » seul : le
  # libelle d'interface « % colonne » doit continuer de partir.
  d <- d[!grepl(HSTAT_I18N_MARQUEUR, names(d))]
  if (!length(d)) return("{}")
  esc <- function(s) {
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    s <- gsub("\"", "\\\"", s, fixed = TRUE)
    s <- gsub("\r", "\\r", s, fixed = TRUE)
    s <- gsub("\n", "\\n", s, fixed = TRUE)
    gsub("\t", "\\t", s, fixed = TRUE)
  }
  paste0("{", paste(sprintf("\"%s\":\"%s\"", esc(names(d)), esc(unname(d))),
                    collapse = ","), "}")
}

# Etat de la traduction : combien de chaines de l'interface sont couvertes, et
# lesquelles manquent. Sert au suivi du chantier, et a un test qui empeche la
# couverture de reculer en silence quand du texte est ajoute.
hstat_i18n_coverage <- function(chaines, path = hstat_i18n_path()) {
  d <- hstat_i18n_load(path)
  chaines <- unique(trimws(as.character(chaines)))
  chaines <- chaines[nzchar(chaines)]
  traduites <- intersect(chaines, d$fr)
  list(total = length(chaines), traduites = length(traduites),
       manquantes = setdiff(chaines, d$fr),
       taux = if (!length(chaines)) 1 else length(traduites) / length(chaines))
}

# ===========================================================================
# ETAT INITIAL DE LA SESSION : UNE SEULE SOURCE DE VERITE
# ---------------------------------------------------------------------------
# La reinitialisation remettait a NULL une liste de champs ENUMEREE A LA MAIN,
# distincte de celle qui cree `reactiveValues`. Les deux listes ont derive :
# tout champ ajoute depuis (aiContext, aiHistory, cahClusters, y2Vars…)
# survivait a la reinitialisation, et l'utilisateur retrouvait des restes de sa
# session precedente.
#
# Les deux usages partagent desormais CETTE liste. Un champ ajoute ici est
# cree au demarrage et efface a la reinitialisation, sans qu'on ait a y penser.
# ===========================================================================
hstat_valeurs_initiales <- function() {
  list(
    data = NULL, cleanData = NULL, filteredData = NULL, descStats = NULL,
    normResults = NULL, leveneResults = NULL, testResults = NULL,
    anovaModel = NULL, lastKruskal = NULL, multiResults = NULL,
    multiGroups = NULL, currentPlot = NULL, residualsNorm = NULL,
    leveneResid = NULL, multiNormResults = NULL, multiLeveneResults = NULL,
    pcaResult = NULL, clusterResult = NULL, currentModel = NULL,
    testInterpretation = NULL, cahResult = NULL, currentInteractivePlot = NULL,
    cahClusters = NULL, testResultsDF = NULL,
    multiResultsMain = NULL, multiResultsInteraction = NULL,
    normalityResults = NULL, homogeneityResults = NULL,
    currentVarIndex = 1, currentValidationVar = 1,
    allTestResults = list(), allPostHocResults = list(), modelsList = list(),
    normalityResultsPerVar = list(), homogeneityResultsPerVar = list(),
    currentDiagVar = 1, currentResidVar = 1,
    customXOrder = NULL, y2Vars = NULL, dualAxisActive = FALSE,
    y2VarsActive = NULL, y2RangeForAxis = NULL, y2UnifiedColorMap = NULL,
    postHocSyncTrigger = NULL,
    transformationLog = list(),
    chiSqResults = NULL, chiSqFreqData = NULL, chiSqPostHocData = NULL,
    chiSqPlotObj = NULL, chiSqRawObs = NULL, chiSqModalites = NULL,
    chiSqPGlobal = NULL,
    # ---- Moteur de donnees (memoire / hors-memoire DuckDB) ----
    dbCon = NULL, dbTable = NULL, dataMode = "memory",
    fullNrow = NULL, fullNcol = NULL, fullNA = NULL, isSampled = FALSE,
    sourceKind = NULL, sourceSize = NULL,
    # ---- Aide a la decision ----
    aiContext = NULL, aiHistory = NULL,
    # Chemin du fichier NEUTRALISE par la derniere reinitialisation.
    # `shinyjs::reset("file")` remet le widget a blanc mais `input$file` garde
    # sa valeur : sans ce temoin, la feuille Excel choisie et le bloc de
    # combinaison de feuilles survivaient a la reinitialisation.
    fichierNeutralise = NULL,
    resetSignal = 0
  )
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

# ===========================================================================
# GRAPHIQUES INTERACTIFS : RETIRER LE POLYFILL OBSOLETE DE PLOTLY
# ---------------------------------------------------------------------------
# plotly attache une dependance « typedarray », polyfill destine aux
# navigateurs depourvus de tableaux types (IE9). Son code reference `GLOBAL`,
# une variable de Node.js qui n'existe pas dans un navigateur : il leve donc
# une ReferenceError des qu'il est evalue, sur CHAQUE page portant un
# graphique interactif. Rien ne casse — les tableaux types sont natifs partout
# depuis 2012 — mais la console d'un utilisateur qui l'ouvre est polluee par
# une erreur permanente, et une erreur permanente masque les vraies.
#
# `plotly_build()` est appele ici parce que la dependance n'est attachee qu'a
# la construction ; le rendu l'appellerait de toute facon, le cout ne fait que
# changer de place.
# ===========================================================================
hstat_plotly_clean <- function(p) {
  if (is.null(p)) return(p)
  b <- tryCatch(plotly::plotly_build(p), error = function(e) NULL)
  if (is.null(b)) return(p)
  if (!is.null(b$dependencies))
    b$dependencies <- Filter(function(d) !identical(d$name, "typedarray"),
                             b$dependencies)
  b
}

# Le nettoyage est pose sur `renderPlotly` lui-meme plutot qu'a chacun des
# appels : les corps de ces sorties comportent plusieurs `return()`, et un
# habillage de l'expression serait purement et simplement saute par le premier
# d'entre eux. `exprToFunction` transforme le bloc en fonction — le `return()`
# en sort alors normalement, et la valeur passe bien par le nettoyage.
if (.hstat_has("plotly")) {
  renderPlotly <- function(expr, env = parent.frame(), quoted = FALSE) {
    fn <- shiny::exprToFunction(expr, env, quoted)
    plotly::renderPlotly(hstat_plotly_clean(fn()),
                         env = environment(), quoted = FALSE)
  }
}

# ===========================================================================
# PAQUET ABSENT : LE DIRE, ET PROPOSER UNE VOIE DE REPLI
# ---------------------------------------------------------------------------
# « Package 'klaR' indisponible. » est une impasse : l'utilisateur ne sait ni
# comment l'installer, ni quoi faire en attendant. Or il y a presque toujours
# une analyse voisine, deja disponible, qui repond a la meme question — moins
# bien, mais tout de suite.
#
# Ces trois classifications reposent sur des paquets non installes par defaut.
# Elles restent donc les seules analyses de l'application qui ne peuvent etre
# ni executees ni testees sur une machine minimale, d'ou ce soin particulier.
# ===========================================================================

HSTAT_PKG_REPLI <- list(
  klaR = paste("En attendant, une ACM sur les memes variables suivie d'une CAH",
               "sur les coordonnees factorielles donne une classification",
               "d'individus decrits par des variables qualitatives. Les deux",
               "analyses sont disponibles dans cet onglet."),
  poLCA = paste("En attendant, une ACM suivie d'une CAH degage des profils",
                "comparables. Elle ne fournit pas les probabilites",
                "d'appartenance ni les criteres AIC/BIC, mais elle repond a la",
                "meme question : quels groupes d'individus se ressemblent ?"),
  clustMixType = paste("En attendant, une AFDM (analyse factorielle de donnees",
                       "mixtes) suivie d'une CAH traite egalement un melange de",
                       "variables numeriques et qualitatives. Les deux analyses",
                       "sont disponibles dans cet onglet."))

hstat_pkg_manquant <- function(pkg, analyse = NULL) {
  paste0(
    if (!is.null(analyse)) paste0(analyse, " : ") else "",
    sprintf("le paquet R « %s » n'est pas installe sur cette machine, ", pkg),
    "cette analyse ne peut donc pas etre lancee. ",
    sprintf("Pour l'ajouter : install.packages(\"%s\"), puis relancez ", pkg),
    "HStat. ",
    HSTAT_PKG_REPLI[[pkg]] %||% "")
}

# ---------------------------------------------------------------------------
# QUALITE D'UNE PARTITION
# Ces fonctions vivent ici, et non dans l'observateur qui les appelle, pour une
# raison precise : klaR, poLCA et clustMixType sont absents de beaucoup de
# machines, la CI comprise. Sortir le calcul du paquet le rend testable sur des
# valeurs posees a la main — c'est la seule facon de garder ces analyses sous
# controle sans pouvoir les executer.
# ---------------------------------------------------------------------------

# Pseudo-R2 d'une partition k-modes : part de la dissimilarite totale expliquee
# par la partition. `d_tot` est la dissimilarite d'une partition a un seul
# groupe (chaque variable ramenee a son mode).
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
  # `table()`. Un appelant qui passerait le VECTEUR d'affectation plutot que sa
  # table — confusion facile — envoyait ici un facteur, sur lequel `sum()`
  # echoue (« 'sum' not meaningful for factors ») et faisait tomber toute la
  # sortie. La fonction promet un verdict, pas une erreur : on convertit ce qui
  # se convertit, et on rend « indeterminable » sur le reste.
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

# Verdict a trois niveaux sur une statistique bornee. Renvoie « indeterminable »
# et non une erreur quand la statistique n'a pas pu etre calculee : brancher sur
# un NA ferait tomber toute la sortie (cf. hstat_p_verdict).
hstat_seuil_verdict <- function(x, seuil_ok, seuil_warn) {
  if (length(x) != 1L || !is.finite(x)) return("indeterminable")
  if (x >= seuil_ok) "ok" else if (x >= seuil_warn) "warn" else "err"
}

# ===========================================================================
# TRADUCTION DES ERREURS R
# ---------------------------------------------------------------------------
# L'interface est en francais ; les erreurs de R, non. « data are essentially
# constant » ou « incorrect number of dimensions » ne disent rien a un
# utilisateur, et surtout ne disent pas QUOI FAIRE — c'est la seule chose qui
# l'interesse a ce moment-la.
#
# Deux principes :
#   1. Chaque traduction enonce la CAUSE puis le GESTE. « Variance nulle » ne
#      suffit pas ; « toutes les valeurs sont identiques, choisissez une autre
#      variable » se suit.
#   2. Le message R d'origine n'est jamais supprime, il est mis entre
#      parentheses. Sans lui, un utilisateur qui demande de l'aide n'a plus
#      rien a montrer, et une erreur mal traduite devient indebuggable.
#
# Les motifs sont des expressions regulieres, testees dans l'ordre : le plus
# specifique d'abord. Les messages de R dependant de la locale, on accepte les
# deux formulations quand elles different.
# ===========================================================================

HSTAT_ERR_FR <- list(
  list("data are essentially constant|essentiellement constant",
       paste("La variable ne varie pas : toutes ses valeurs sont identiques (ou",
             "presque). Aucun test ne peut comparer ce qui ne varie pas.",
             "Choisissez une autre variable, ou verifiez que le filtre actif",
             "n'a pas reduit vos donnees a un seul cas de figure.")),
  list("all 'x' values are identical|values are identical",
       paste("Toutes les observations portent la meme valeur. Le test n'a rien",
             "a comparer. Verifiez la variable choisie et les filtres actifs.")),
  list("sample size must be between 3 and 5000",
       paste("Le test de Shapiro-Wilk exige entre 3 et 5000 observations.",
             "Au-dela, utilisez un graphique quantile-quantile plutot qu'un",
             "test : sur de tels effectifs, il rejetterait le moindre ecart.")),
  list("not enough .?(x|y|finite)?.? observations|not enough observations",
       paste("Effectif insuffisant pour ce test. Verifiez le nombre",
             "d'observations non manquantes dans chaque groupe : un groupe vide",
             "ou reduit a une seule observation suffit a bloquer le calcul.")),
  list("grouping factor must have exactly 2 levels",
       paste("Ce test compare exactement deux groupes, or le facteur choisi",
             "n'en distingue pas deux. Pour plus de deux groupes, utilisez",
             "l'ANOVA — ou Kruskal-Wallis si la normalite n'est pas acquise.")),
  list("not enough 'x' observations|need at least 2 groups|at least two groups",
       paste("Il faut au moins deux groupes comportant des observations, et",
             "l'un d'eux est vide apres retrait des valeurs manquantes.",
             "Verifiez les effectifs par groupe, et les filtres actifs.")),
  list("contrasts can be applied only to factors with 2 or more levels",
       paste("Une des variables explicatives ne prend qu'une seule modalite",
             "dans les donnees analysees : elle n'apporte aucune information au",
             "modele. Retirez-la, ou verifiez les filtres actifs.")),
  list("incorrect number of dimensions",
       paste("Le resultat ne comporte qu'un seul axe factoriel : il ne peut pas",
             "etre represente dans un plan. C'est le cas courant d'une AFC",
             "croisant une variable binaire (sexe, oui/non, avant/apres).",
             "Croisez des variables comportant davantage de modalites.")),
  list("exactly singular|computationally singular|singular matrix|matrice singuli",
       paste("La matrice n'est pas inversible : au moins deux variables sont",
             "redondantes (l'une se deduit des autres), ou il y a moins",
             "d'observations que de variables. Retirez une des variables",
             "correlees, ou augmentez l'effectif.")),
  list("non-conformable arg",
       paste("Les dimensions des tableaux combines ne correspondent pas.",
             "Verifiez que toutes les variables retenues portent bien sur les",
             "memes observations.")),
  list("missing value where TRUE/FALSE needed",
       paste("Une statistique n'a pas pu etre calculee (elle vaut NA) et une",
             "decision en dependait. C'est le signe de donnees degenerees :",
             "verifiez la variance et l'effectif de chaque groupe, ainsi que",
             "le taux de valeurs manquantes.")),
  list("0 \\(non-NA\\) cases|no complete element|complete\\.cases",
       paste("Aucune observation ne renseigne toutes les variables choisies a",
             "la fois. Retirez la variable la plus lacunaire, ou traitez les",
             "valeurs manquantes dans l'onglet Nettoyage.")),
  list("NA/NaN/Inf in foreign function call|infinite or missing values",
       paste("Les donnees contiennent des valeurs manquantes ou infinies que ce",
             "calcul n'accepte pas. Traitez-les dans l'onglet Nettoyage",
             "(imputation ou retrait) avant de relancer.")),
  list("undefined columns selected|subscript out of bounds",
       paste("Une variable attendue est absente du jeu de donnees. Elle a sans",
             "doute ete renommee ou retiree depuis le choix : reselectionnez",
             "vos variables.")),
  list("there is no package called",
       paste("Un paquet R necessaire a cette analyse n'est pas installe.",
             "Installez-le, puis relancez l'application ; cette analyse restera",
             "indisponible en attendant, les autres continuent de fonctionner.")),
  list("could not find function",
       paste("Une fonction attendue est introuvable : le paquet qui la fournit",
             "n'est pas installe ou n'a pas pu etre charge. Installez-le, puis",
             "relancez l'application.")),
  list("cannot open file|No such file or directory|impossible d'ouvrir",
       paste("Le fichier n'a pas pu etre ouvert. Verifiez le chemin, que le",
             "fichier n'a pas ete deplace, et vos droits d'acces.")),
  list("arguments imply differing number of rows|replacement has .* rows",
       paste("Les colonnes assemblees n'ont pas le meme nombre de lignes.",
             "Verifiez que les jeux de donnees fusionnes portent bien sur les",
             "memes observations.")),
  list("approximation may be incorrect|approximation incorrecte",
       paste("Certains effectifs theoriques sont inferieurs a 5 :",
             "l'approximation du khi-deux devient douteuse. Utilisez le test",
             "exact de Fisher, ou regroupez les modalites les moins",
             "frequentes.")),
  list("figure margins too large",
       paste("La zone de trace est trop petite pour le graphique demande.",
             "Agrandissez la fenetre, ou reduisez la taille des etiquettes.")),
  list("argument \"name\" is missing",
       paste("Erreur interne d'affichage. Signalez-la : elle vient du code de",
             "l'application, pas de vos donnees.")),
  list("must be numeric|not numeric|doit etre numerique",
       paste("Ce calcul attend une variable numerique et a recu du texte ou une",
             "categorie. Convertissez la variable dans l'onglet Nettoyage, ou",
             "choisissez une variable numerique.")),
  list("system is exactly singular|did not converge|ne converge pas",
       paste("Le modele n'a pas converge. Les groupes sont probablement",
             "parfaitement separes, ou l'effectif est trop faible pour le",
             "nombre de parametres estimes. Simplifiez le modele.")))

# Traduit une erreur R en francais actionnable. `e` accepte une condition ou
# une chaine. `contexte` prefixe le message (« Test t : … ») quand l'appelant
# sait de quelle analyse il s'agit.
# Langue de la session courante. Elle est portee par `session$userData` et non
# par une option globale : sur un serveur partage, une option ferait basculer
# la langue de TOUS les utilisateurs des que l'un d'eux change la sienne.
# Hors Shiny (tests, scripts), le francais s'applique.
hstat_langue_session <- function() {
  d <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (is.null(d) || is.null(d$userData)) return("fr")
  l <- tryCatch(d$userData$langue, error = function(e) NULL)
  if (identical(l, "en")) "en" else "fr"
}

# `lang` : ces messages sont COMPOSES ici, phrase par phrase, et n'existent
# donc pas comme chaine entiere dans le dictionnaire du navigateur — celui-ci
# ne remplace que des correspondances completes. La traduction se fait ainsi
# cote serveur, mais elle puise dans LE MEME fichier CSV que le reste : une
# seule source de verite pour les traductions.
hstat_err_fr <- function(e, contexte = NULL, lang = hstat_langue_session()) {
  msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)[1]
  msg <- trimws(paste(msg, collapse = " "))
  if (!length(msg) || !nzchar(msg)) msg <- tr("erreur sans message", lang)
  # Le francais met une espace avant les deux-points, l'anglais non. Garder la
  # ponctuation francaise dans une phrase anglaise trahirait la traduction.
  dp <- if (identical(lang, "en")) ": " else " : "
  prefixe <- if (!is.null(contexte) && nzchar(contexte))
    paste0(contexte, dp) else ""
  for (r in HSTAT_ERR_FR) {
    if (grepl(r[[1]], msg, ignore.case = TRUE, perl = TRUE))
      # Le message d'origine reste entre parentheses : c'est ce qu'un
      # utilisateur copiera pour demander de l'aide.
      return(sprintf("%s%s (%s%s%s)", prefixe, tr(r[[2]], lang),
                     tr("message R", lang), dp, msg))
  }
  # Rien de connu : on ne masque pas, on annonce. Presenter un message anglais
  # comme une phrase francaise serait pire que de dire qu'il ne l'est pas.
  sprintf("%s%s%s%s", prefixe,
          tr("L'analyse a echoue. Message renvoye par R (non traduit)", lang),
          dp, msg)
}

# FactoMineR reduit ses coordonnees a un VECTEUR des que le resultat ne comporte
# qu'un seul axe. C'est le cas d'une AFC croisant une variable BINAIRE avec une
# autre (une table 3x2 ne porte qu'une dimension), situation tres courante en
# enquete : sexe, oui/non, avant/apres. `ncol()` vaut alors NULL et l'indexation
# `[, 1:2]` echoue sur « incorrect number of dimensions ». On restitue
# systematiquement une matrice avant tout usage.
hstat_coord_mat <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.null(dim(x)))
    return(matrix(x, ncol = 1L, dimnames = list(names(x), "Dim 1")))
  as.matrix(x)
}

# Verdict a TROIS etats sur une p-value : significatif / non significatif /
# indeterminable. Un test statistique rend NA ou NaN des que ses donnees sont
# degenerees (variance nulle, matrice singuliere, effectifs vides) ; brancher
# alors directement dessus — `if (p < 0.05)` — leve « missing value where
# TRUE/FALSE needed » et fait tomber la sortie entiere au lieu de signaler
# poliment que le test n'a pas pu conclure.
hstat_p_verdict <- function(p, alpha = 0.05) {
  p <- suppressWarnings(as.numeric(p)[1])
  if (!is.finite(p)) return("indeterminable")
  if (p < alpha) "significatif" else "non significatif"
}

interpret_p_value <- function(p_value) {
  if (is.na(p_value)) {
    return("NA")
  } else if (p_value < 0.001) {
    return("Hautement significatif (p < 0.001)")
  } else if (p_value < 0.01) {
    return("Très significatif (p < 0.01)")
  } else if (p_value < 0.05) {
    return("Significatif (p < 0.05)")
  } else {
    return("Non significatif (p >= 0.05)")
  }
}

interpret_test_results <- function(test_type, p_value, test_object = NULL) {
  if (is.na(p_value)) return("Résultat non disponible")
  significance <- ifelse(p_value < 0.05, "significative", "non significative")
  switch(test_type,
         "t.test"           = paste0("Le test t montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "wilcox.test"      = paste0("Le test de Wilcoxon montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "anova"            = paste0("L'ANOVA montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "kruskal.test"     = paste0("Le test de Kruskal-Wallis montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "scheirerRayHare"  = paste0("Le test de Scheirer-Ray-Hare montre une différence ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "manova"           = paste0("La MANOVA montre une différence multivariée ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "permanova"        = paste0("La PERMANOVA montre une différence multivariée ", significance, " entre les groupes (p = ", round(p_value, 8), ")"),
         "chisq.test"       = paste0("Le test du chi² montre une association ", significance, " entre les variables (p = ", round(p_value, 8), ")"),
         "cor.test"         = paste0("La corrélation est ", significance, " (p = ", round(p_value, 8), ")"),
         paste0("Le test ", test_type, " montre un résultat ", significance, " (p = ", round(p_value, 8), ")")
  )
}

# =============================================================================
#  AJUSTEMENT ROBUSTE D'UN GLM (non-convergence / séparation)
# -----------------------------------------------------------------------------
#  glm.fit() émet « l'algorithme n'a pas convergé » dans deux situations :
#   - le budget d'itérations par défaut (25) est trop court ;
#   - surtout : les données sont (quasi-)séparées, c'est-à-dire qu'une
#     combinaison des prédicteurs classe parfaitement les deux modalités de Y.
#     Les coefficients divergent alors vers l'infini et l'algorithme tourne
#     jusqu'à la limite d'itérations sans jamais se stabiliser.
#  Ces fonctions augmentent le budget d'itérations, capturent l'avertissement au
#  lieu de le laisser filer dans la console, et renvoient un diagnostic lisible
#  affichable dans l'interface.
# =============================================================================

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

# =============================================================================
#  TAILLE DES LABELS EN POINTS TYPOGRAPHIQUES (analyses multivariées)
# -----------------------------------------------------------------------------
#  Toutes les analyses multivariées exposent deux réglages de taille de label,
#  bornés à [12, 24] pt : un pour les INDIVIDUS, un pour les VARIABLES (ou
#  modalités). ggplot2 exprime la taille du texte en millimètres — aussi bien
#  dans geom_text(size = ) que dans l'argument labelsize de factoextra — d'où la
#  conversion par le facteur .pt = 72,27 / 25,4.
# =============================================================================
HSTAT_LBL_PT_MIN     <- 12
HSTAT_LBL_PT_MAX     <- 24
HSTAT_LBL_PT_DEFAULT <- 12
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
  sliderInput(id, label, min = HSTAT_LBL_PT_MIN, max = HSTAT_LBL_PT_MAX,
              value = value, step = 1, post = " pt")
}

# Impose la taille des calques de texte d'un ggplot déjà construit (factoextra
# n'expose qu'un seul argument `labelsize`, commun aux individus et aux
# variables). `size_default` s'applique à tous les calques de texte ; si
# `size_var` et `n_var` sont fournis, les calques dont les données comptent
# exactement `n_var` lignes — les labels de variables / modalités — reçoivent
# `size_var`. `n_var` accepte plusieurs effectifs candidats (variables
# quantitatives, modalités, groupes), les méthodes factorielles n'exposant pas
# toutes leurs coordonnées au même endroit. Ce repérage par effectif n'est
# fiable que si individus et variables sont en nombres différents, d'où le
# garde-fou sur `n_ind`.
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

interpret_normality <- function(p_value) {
  if (is.na(p_value)) return("Résultat non disponible")
  if (p_value >= 0.05) {
    return(paste0("La distribution est normale (p = ", round(p_value, 8), " >= 0.05)"))
  } else {
    return(paste0("La distribution n'est pas normale (p = ", round(p_value, 8), " < 0.05)"))
  }
}

interpret_homogeneity <- function(p_value) {
  if (is.na(p_value)) return("Résultat non disponible")
  if (p_value >= 0.05) {
    return(paste0("Les variances sont homogènes (p = ", round(p_value, 8), " >= 0.05)"))
  } else {
    return(paste0("Les variances ne sont pas homogènes (p = ", round(p_value, 8), " < 0.05)"))
  }
}

interpret_normality_resid <- function(p_value) {
  if (is.na(p_value)) return("Test non applicable")
  if (p_value > 0.05) {
    return("Les résidus suivent une distribution normale (p > 0.05). Les conditions pour les tests paramétriques sont respectées.")
  } else {
    return("Les résidus ne suivent pas une distribution normale (p < 0.05). Considérez l'utilisation de tests non-paramétriques.")
  }
}

interpret_homogeneity_resid <- function(p_value) {
  if (is.na(p_value)) return("Test non applicable")
  if (p_value > 0.05) {
    return("Les variances sont homogènes (p > 0.05). Les conditions pour les tests paramétriques sont respectées.")
  } else {
    return("Les variances ne sont pas homogènes (p < 0.05). Utilisez des tests robustes à l'hétérogénéité des variances.")
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

calc_cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100



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

#' Retourne les conditions d'application d'une transformation
#' @param method character
get_transformation_condition <- function(method) {
  switch(method,
         "log"        = "x > 0 (strictement positif)",
         "log1p"      = "x >= 0 (positif ou nul)",
         "log10"      = "x > 0 (strictement positif)",
         "sqrt"       = "x >= 0 (positif ou nul)",
         "cuberoot"   = "Toutes valeurs (accepte les négatifs)",
         "boxcox"     = "x > 0 (strictement positif) — λ estimé par MV",
         "yeojohnson" = "Toutes valeurs (accepte les négatifs)",
         "arcsin"     = "0 <= x <= 1 (proportions)",
         "logit"      = "0 < x < 1 (taux stricts)",
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
                   
                   # ── 1. Logarithme naturel ──────────────────────────────────────────────────
                   "log" = {
                     if (any(x_nona <= 0))
                       stop("log(x) requiert des valeurs strictement positives (x > 0).\n",
                            "Valeurs problématiques : ", sum(x_nona <= 0, na.rm = TRUE), " observation(s) <= 0.\n",
                            "Conseil : utilisez log(x+1) si vous avez des zéros.")
                     log(x)
                   },
                   
                   # ── 2. log(x+1) ───────────────────────────────────────────────────────────
                   "log1p" = {
                     if (any(x_nona < 0))
                       stop("log(x+1) requiert des valeurs >= 0 (x >= 0).\n",
                            "Valeurs négatives détectées : ", sum(x_nona < 0, na.rm = TRUE), " observation(s).")
                     log1p(x)
                   },
                   
                   # ── 3. Log base 10 ────────────────────────────────────────────────────────
                   "log10" = {
                     if (any(x_nona <= 0))
                       stop("log10(x) requiert des valeurs strictement positives (x > 0).\n",
                            "Valeurs problématiques : ", sum(x_nona <= 0, na.rm = TRUE), " observation(s) <= 0.")
                     log10(x)
                   },
                   
                   # ── 4. Racine carrée ──────────────────────────────────────────────────────
                   "sqrt" = {
                     if (any(x_nona < 0))
                       stop("sqrt(x) requiert des valeurs >= 0.\n",
                            "Valeurs négatives : ", sum(x_nona < 0, na.rm = TRUE), " observation(s).\n",
                            "Conseil : utilisez x^(1/3) si vous avez des valeurs négatives.")
                     sqrt(x)
                   },
                   
                   # ── 5. Racine cubique ─────────────────────────────────────────────────────
                   "cuberoot" = {
                     sign(x) * abs(x)^(1/3)
                   },
                   
                   # ── 6. Box-Cox ────────────────────────────────────────────────────────────
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
                   
                   # ── 7. Yeo-Johnson ────────────────────────────────────────────────────────
                   "yeojohnson" = {
                     yj_obj  <- bestNormalize::yeojohnson(x_nona, standardize = FALSE)
                     x_trans <- rep(NA_real_, length(x))
                     x_trans[!is.na(x)] <- predict(yj_obj, newdata = x_nona)
                     attr(x_trans, "yj_object") <- yj_obj
                     attr(x_trans, "lambda")    <- round(yj_obj$lambda, 4)
                     x_trans
                   },
                   
                   # ── 8. Arcsinus ───────────────────────────────────────────────────────────
                   "arcsin" = {
                     if (any(x_nona < 0 | x_nona > 1))
                       stop("asin(sqrt(x)) requiert des valeurs entre 0 et 1 (proportions/pourcentages en décimal).\n",
                            sum(x_nona < 0 | x_nona > 1, na.rm = TRUE), " valeur(s) hors [0,1].\n",
                            "Si vos données sont en %, divisez par 100 avant la transformation.")
                     asin(sqrt(x))
                   },
                   
                   # ── 9. Logit ──────────────────────────────────────────────────────────────
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
               predict(yj_object, newdata = x, inverse = TRUE)
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
                   "log"     = if (any(x_nona <= 0)) paste(sum(x_nona <= 0), "valeur(s) <= 0 détectée(s)") else NULL,
                   "log1p"   = if (any(x_nona < 0))  paste(sum(x_nona < 0),  "valeur(s) < 0 détectée(s)")  else NULL,
                   "log10"   = if (any(x_nona <= 0)) paste(sum(x_nona <= 0), "valeur(s) <= 0 détectée(s)") else NULL,
                   "sqrt"    = if (any(x_nona < 0))  paste(sum(x_nona < 0),  "valeur(s) < 0 détectée(s)")  else NULL,
                   "cuberoot" = NULL,  # toujours applicable
                   "boxcox"  = if (any(x_nona <= 0)) paste(sum(x_nona <= 0), "valeur(s) <= 0 (Box-Cox nécessite x > 0)") else NULL,
                   "yeojohnson" = NULL,  # toujours applicable
                   "arcsin"  = if (any(x_nona < 0 | x_nona > 1)) paste(sum(x_nona < 0 | x_nona > 1), "valeur(s) hors [0,1]") else NULL,
                   "logit"   = if (any(x_nona <= 0 | x_nona >= 1)) paste(sum(x_nona <= 0 | x_nona >= 1), "valeur(s) hors ]0,1[") else NULL,
                   NULL
  )
  
  if (!is.null(issues)) {
    list(ok = FALSE, message = issues)
  } else {
    list(ok = TRUE, message = paste0("Applicable (n = ", n, ")"))
  }
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
# (separateurs de milliers, espaces insecables). Retourne NULL si moins de 90 %
# des valeurs non manquantes sont convertibles (colonne reellement textuelle).
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

viz_get_theme <- function(theme_name = "minimal", base_size = 12) {
  switch(theme_name,
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
        tryCatch(format(as.Date(v), disp_fmt), error = function(e) v)
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
    return(ggplot2::scale_x_discrete(limits = all_x, drop = FALSE))
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

#  MANOVA — Fonctions utilitaires (paramétrique + non paramétrique)
#    - Vérification des prérequis multivariés (n, p, completeness)
#    - Normalité multivariée (Mardia via psych)
#    - Homogénéité des matrices de covariance (Box's M via heplots)
#    - Homogénéité multivariée des dispersions (PERMDISP via vegan::betadisper)
#    - Mise en forme des résultats parametric (4 statistiques)
#    - PERMANOVA (vegan::adonis2) + pairwise PERMANOVA
#    - Décomposition univariée post-hoc (ANOVA / Kruskal) avec ajustement

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
                message = paste0("Trop peu d'observations complètes (n=", n,
                                 ") pour ", p, " variables réponses.")))
  
  # Variance non nulle pour chaque réponse globalement et par groupe
  zero_var <- vapply(response, function(v) {
    stats::sd(df2[[v]], na.rm = TRUE) == 0 ||
      is.na(stats::sd(df2[[v]], na.rm = TRUE))
  }, logical(1))
  if (any(zero_var))
    return(list(ok = FALSE,
                message = paste0("Variance nulle pour : ",
                                 paste(response[zero_var], collapse = ", "))))
  
  # Au moins 2 niveaux par facteur, et chaque cellule >= 2 obs
  for (f in factors) {
    if (nlevels(df2[[f]]) < 2)
      return(list(ok = FALSE,
                  message = paste0("Le facteur '", f, "' a moins de 2 niveaux après nettoyage.")))
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
                conclusion = paste0("Groupes trop petits pour Box's M (min n=",
                                    min_per_group, " < p+1=", ncol(Y) + 1, ")")))
  
  # Vérifier que chaque matrice de covariance de groupe est de rang plein.
  # On teste le RANG (via qr) et non le déterminant brut : le déterminant
  # dépend de l'échelle des variables et peut être légitimement très petit.
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
  
  cap <- hstat_cap_Y_group(Y, group, what = "Homogeneite des dispersions (betadisper)")
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
  base <- paste0("Effet multivarié ", sig, " (Pillai, p = ", round(p_pillai, 6), ")")
  if (!is.na(eta2)) {
    mag <- if (eta2 < 0.01) "négligeable"
    else if (eta2 < 0.06) "faible"
    else if (eta2 < 0.14) "modéré"
    else "important"
    base <- paste0(base, " — taille d'effet ", mag, " (eta² = ", round(eta2, 3), ")")
  }
  base
}


interpret_permanova_effect <- function(p_value, R2 = NA) {
  if (is.na(p_value)) return("Résultat non disponible")
  sig <- if (p_value < 0.05) "significatif" else "non significatif"
  base <- paste0("Effet multivarié ", sig, " (PERMANOVA, p = ", round(p_value, 6), ")")
  if (!is.na(R2)) {
    mag <- if (R2 < 0.01) "négligeable"
    else if (R2 < 0.06) "faible"
    else if (R2 < 0.14) "modéré"
    else "important"
    base <- paste0(base, " — R² = ", round(R2, 3), " (", mag, ")")
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
    if (length(unique(gp)) < 2 || nrow(Yp) < 4) {
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


#' Décomposition univariée d'une MANOVA paramétrique
#' Lance une ANOVA sur chaque Y (avec mêmes facteurs/interaction)
#' Applique un ajustement Bonferroni cross-réponses sur les p-values
#' @param df data.frame (déjà nettoyé)
#' @param response character — variables réponses
#' @param factors  character — facteurs
#' @param interaction logical
#' @param p_adjust "bonferroni" (défaut)
#' @return data.frame : Reponse, Effet, ddl, F, p_value, p_adj, eta2_partial, Significatif
manova_univariate_followup <- function(df, response, factors, interaction = FALSE,
                                       p_adjust = "bonferroni") {
  rhs <- paste(sapply(factors, function(x) paste0("`", x, "`")),
               collapse = ifelse(isTRUE(interaction), "*", "+"))
  res_all <- list()
  for (v in response) {
    fml <- stats::as.formula(paste0("`", v, "` ~ ", rhs))
    fit <- tryCatch(stats::aov(fml, data = df), error = function(e) NULL)
    if (is.null(fit)) next
    tab <- summary(fit)[[1]]
    eff <- rownames(tab); eff <- trimws(eff)
    is_resid <- eff == "Residuals"
    if (!any(is_resid)) next
    ss_resid <- tab[is_resid, "Sum Sq"]
    df_resid <- tab[is_resid, "Df"]
    
    for (i in which(!is_resid)) {
      eta2 <- tab[i, "Sum Sq"] / (tab[i, "Sum Sq"] + ss_resid)
      res_all[[paste(v, eff[i], sep = "_")]] <- data.frame(
        Reponse  = v,
        Effet    = eff[i],
        ddl      = paste0(tab[i, "Df"], ", ", df_resid),
        F_stat   = tab[i, "F value"],
        p_value  = tab[i, "Pr(>F)"],
        eta2_partial = eta2,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(res_all) == 0) return(NULL)
  out <- do.call(rbind, res_all); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = p_adjust)
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


#' Décomposition univariée non paramétrique (Kruskal-Wallis) pour PERMANOVA
#' Lance KW sur chaque Y x facteur (effets simples, un facteur à la fois)
#' Ajustement Bonferroni cross-réponses x facteurs
manova_univariate_followup_np <- function(df, response, factors,
                                          p_adjust = "bonferroni") {
  res_all <- list()
  for (v in response) {
    for (f in factors) {
      fml <- stats::as.formula(paste0("`", v, "` ~ `", f, "`"))
      kw  <- tryCatch(stats::kruskal.test(fml, data = df), error = function(e) NULL)
      if (is.null(kw)) next
      n   <- nrow(df[stats::complete.cases(df[, c(v, f)]), , drop = FALSE])
      eta2_kw <- (kw$statistic - length(unique(df[[f]])) + 1) / (n - length(unique(df[[f]])))
      eta2_kw <- max(0, as.numeric(eta2_kw))
      res_all[[paste(v, f, sep = "_")]] <- data.frame(
        Reponse  = v,
        Facteur  = f,
        H_stat   = as.numeric(kw$statistic),
        ddl      = as.numeric(kw$parameter),
        p_value  = kw$p.value,
        eta2_KW  = eta2_kw,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(res_all) == 0) return(NULL)
  out <- do.call(rbind, res_all); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = p_adjust)
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


#' Comparaisons par paires sur les niveaux d'un facteur — univarié paramétrique
#' Tukey HSD appliqué à chaque variable réponse, ajustement Bonferroni cross-réponses
#' @param df       data.frame nettoyé
#' @param response character — variables réponses
#' @param factor   character — UN facteur
#' @return data.frame : Reponse, Comparaison, Diff, IC_inf, IC_sup, p_value, p_adj
manova_pairwise_univariate <- function(df, response, factor_name,
                                       p_adjust = "bonferroni") {
  if (length(factor_name) != 1) return(NULL)
  fvar <- factor_name
  res_all <- list()
  for (v in response) {
    fml <- stats::as.formula(paste0("`", v, "` ~ `", fvar, "`"))
    fit <- tryCatch(stats::aov(fml, data = df), error = function(e) NULL)
    if (is.null(fit)) next
    tk <- tryCatch(stats::TukeyHSD(fit, fvar), error = function(e) NULL)
    if (is.null(tk)) next
    tab <- tk[[1]]
    res_all[[v]] <- data.frame(
      Reponse     = v,
      Comparaison = rownames(tab),
      Diff        = tab[, "diff"],
      IC_inf      = tab[, "lwr"],
      IC_sup      = tab[, "upr"],
      p_value     = tab[, "p adj"],
      stringsAsFactors = FALSE
    )
  }
  if (length(res_all) == 0) return(NULL)
  out <- do.call(rbind, res_all); rownames(out) <- NULL
  out$p_adj        <- stats::p.adjust(out$p_value, method = p_adjust)
  out$Significatif <- ifelse(is.na(out$p_adj), "NA",
                             ifelse(out$p_adj < 0.05, "Oui", "Non"))
  out
}


manova_pairwise_univariate_np <- function(df, response, factor_name,
                                          p_adjust = "bonferroni") {
  if (length(factor_name) != 1) return(NULL)
  fvar <- factor_name
  res_all <- list()
  for (v in response) {
    sub <- df[stats::complete.cases(df[, c(v, fvar)]), c(v, fvar), drop = FALSE]
    if (nrow(sub) < 4 || nlevels(droplevels(as.factor(sub[[fvar]]))) < 2) next
    sub[[fvar]] <- droplevels(as.factor(sub[[fvar]]))
    dn <- tryCatch(
      FSA::dunnTest(sub[[v]], sub[[fvar]], method = "bonferroni"),
      error = function(e) NULL
    )
    if (is.null(dn)) next
    tab <- dn$res
    res_all[[v]] <- data.frame(
      Reponse     = v,
      Comparaison = as.character(tab$Comparison),
      Z_stat      = as.numeric(tab$Z),
      p_value     = as.numeric(tab$P.unadj),
      p_dunn_bonf = as.numeric(tab$P.adj),
      stringsAsFactors = FALSE
    )
  }
  if (length(res_all) == 0) return(NULL)
  out <- do.call(rbind, res_all); rownames(out) <- NULL
  # Ajustement cross-réponses (en plus du Dunn intra-réponse déjà bonferroni)
  out$p_adj        <- stats::p.adjust(out$p_value, method = p_adjust)
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


#' Construit le data.frame des lettres CLD pour un facteur
#' Si Y est fourni, ajoute des colonnes Moyenne ± Ecart-type et Moyenne ± Erreur-type
#' (calculees sur la norme multivariee : sqrt(sum(Yi^2)) pour chaque observation).
#' @param pairs_df output de pairwise_permanova
#' @param group    facteur
#' @param Y        (optionnel) matrice des reponses pour calculer moyennes par groupe
#' @param digits   nombre de decimales pour le formatage (defaut : 3)
#' @return data.frame : Niveau, N, Groupes [, Moyenne_pm_SD, Moyenne_pm_SE]
build_letters_df <- function(pairs_df, group, Y = NULL, digits = 3) {
  group <- as.factor(group)
  levs  <- levels(group)
  if (is.null(pairs_df) || nrow(pairs_df) == 0) return(NULL)
  pmat <- build_pvalue_matrix(pairs_df, levs)
  cld <- tryCatch(multcompView::multcompLetters(pmat, threshold = 0.05)$Letters,
                  error = function(e) stats::setNames(rep("a", length(levs)), levs))
  n_per <- as.numeric(table(group))[match(levs, names(table(group)))]
  out <- data.frame(
    Niveau   = levs,
    N        = n_per,
    Groupes  = as.character(cld[levs]),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(Y)) {
    Y <- as.matrix(Y)
    score <- sqrt(rowSums(Y^2))
    means <- vapply(levs, function(lv) mean(score[group == lv], na.rm = TRUE), numeric(1))
    sds   <- vapply(levs, function(lv) stats::sd(score[group == lv], na.rm = TRUE), numeric(1))
    ses   <- sds / sqrt(pmax(n_per, 1))
    fmt   <- function(m, s) ifelse(is.na(m) | is.na(s), "NA",
                                   paste0(formatC(m, digits = digits, format = "f"),
                                          " \u00b1 ",
                                          formatC(s, digits = digits, format = "f")))
    out$`Moyenne_pm_SD` <- paste0(fmt(means, sds), " ", out$Groupes)
    out$`Moyenne_pm_SE` <- paste0(fmt(means, ses), " ", out$Groupes)
  }
  out
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
        pmat <- matrix(1, length(levs), length(levs), dimnames = list(levs, levs))
        for (i in seq_along(nm)) {
          pair <- strsplit(nm[i], "-", fixed = TRUE)[[1]]
          if (length(pair) == 2 && all(pair %in% levs)) {
            pmat[pair[1], pair[2]] <- pv[i]
            pmat[pair[2], pair[1]] <- pv[i]
          }
        }
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      } else {
        kt <- stats::kruskal.test(y ~ g)
        dn <- FSA::dunnTest(y, g, method = "bonferroni")$res
        pmat <- matrix(1, length(levs), length(levs), dimnames = list(levs, levs))
        for (i in seq_len(nrow(dn))) {
          pair <- trimws(strsplit(as.character(dn$Comparison[i]), "-", fixed = TRUE)[[1]])
          if (length(pair) == 2 && all(pair %in% levs)) {
            pmat[pair[1], pair[2]] <- dn$P.adj[i]
            pmat[pair[2], pair[1]] <- dn$P.adj[i]
          }
        }
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
        pmat <- matrix(1, length(levs), length(levs), dimnames = list(levs, levs))
        for (i in seq_along(nm)) {
          pair <- strsplit(nm[i], "-", fixed = TRUE)[[1]]
          if (length(pair) == 2 && all(pair %in% levs)) {
            pmat[pair[1], pair[2]] <- pv[i]; pmat[pair[2], pair[1]] <- pv[i]
          }
        }
        multcompView::multcompLetters(pmat, threshold = 0.05)$Letters[levs]
      } else {
        dn <- FSA::dunnTest(y, cell, method = "bonferroni")$res
        pmat <- matrix(1, length(levs), length(levs), dimnames = list(levs, levs))
        for (i in seq_len(nrow(dn))) {
          pair <- trimws(strsplit(as.character(dn$Comparison[i]), "-", fixed = TRUE)[[1]])
          if (length(pair) == 2 && all(pair %in% levs)) {
            pmat[pair[1], pair[2]] <- dn$P.adj[i]; pmat[pair[2], pair[1]] <- dn$P.adj[i]
          }
        }
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
                conclusion = "Echantillon trop petit pour Mahalanobis"))
  
  centre <- colMeans(Y)
  cov_mat <- tryCatch(stats::cov(Y), error = function(e) NULL)
  if (is.null(cov_mat) || any(is.na(cov_mat)) ||
      tryCatch(det(cov_mat) < .Machine$double.eps, error = function(e) TRUE)) {
    return(list(d2 = NULL, threshold = NA, n_outliers = NA,
                idx_outliers = integer(0),
                conclusion = "Matrice de covariance singuliere -- impossible de calculer Mahalanobis"))
  }
  
  d2 <- stats::mahalanobis(Y, centre, cov_mat)
  threshold <- stats::qchisq(1 - alpha, df = p)
  idx <- which(d2 > threshold)
  pct <- round(100 * length(idx) / n, 1)
  
  concl <- if (length(idx) == 0)
    paste0("Aucun outlier multivarie détecté (seuil chi2(", p, ") a alpha = ", alpha, ").")
  else if (pct < 5)
    paste0(length(idx), " outlier(s) multivarie(s) détecté(s) (", pct,
           "% des observations). Inspectez-les avant d'analyser.")
  else
    paste0(length(idx), " outliers (", pct,
           "% des observations) -- proportion élevée, verifiez la qualité des données.")
  
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
                        paste0("Normalite multivariee respectee (Mardia : p.skew = ",
                               round(mardia$p.skewness, 3), ", p.kurt = ",
                               round(mardia$p.kurtosis, 3), ")."))
    score_param <- score_param + 2L
  } else {
    if (!is.null(mardia) && (isTRUE(mardia$p.skewness < 0.05) || isTRUE(mardia$p.kurtosis < 0.05))) {
      justifications <- c(justifications,
                          "Violation de la normalité multivariee (Mardia significatif).")
      # Mais si n est grand, le theoreme central limite protege
      if (n >= 50) {
        justifications <- c(justifications,
                            paste0("Toutefois, n = ", n, " >= 50 : la MANOVA reste robuste par le ",
                                   "theoreme central limite (preferer la statistique de Pillai)."))
        score_param <- score_param + 0L
      } else {
        justifications <- c(justifications,
                            paste0("Et n = ", n, " < 50 : PERMANOVA est plus sure (pas d'hypothese ",
                                   "distributionnelle)."))
        score_param <- score_param - 2L
      }
    }
  }
  
  # 2. Homogeneite des matrices de covariance (Box's M)
  boxm_violations <- if (!is.null(boxm)) sum(grepl("Violation", boxm$Conclusion), na.rm = TRUE) else 0
  if (!is.null(boxm) && boxm_violations == 0) {
    justifications <- c(justifications,
                        "Homogeneite des matrices de covariance respectee (Box's M non significatif).")
    score_param <- score_param + 1L
  } else if (boxm_violations > 0) {
    justifications <- c(justifications,
                        paste0("Violation d'homogénéité des covariances sur ", boxm_violations,
                               " facteur(s) (Box's M significatif). La statistique de Pillai est ",
                               "recommandee car plus robuste a cette violation."))
    score_param <- score_param + 0L  # neutre car Pillai compense
  }
  
  # 3. Homogeneite multivariee des dispersions (PERMDISP)
  permdisp_violations <- if (!is.null(permdisp)) sum(grepl("heterogenes|hétérogènes", permdisp$Conclusion), na.rm = TRUE) else 0
  if (permdisp_violations > 0) {
    alertes <- c(alertes,
                 paste0("PERMDISP signale des dispersions multivariees inegales sur ",
                        permdisp_violations, " facteur(s). Une PERMANOVA significative pourrait ",
                        "refleter une difference de dispersion plutot qu'une difference de localisation. ",
                        "A interpreter avec prudence."))
  }
  
  if (score_param >= 2) {
    test_rec  <- "MANOVA parametrique"
    stat_rec  <- "Wilks (puissance maximale)"
    confiance <- "élevée"
  } else if (score_param >= 0) {
    test_rec  <- "MANOVA parametrique"
    stat_rec  <- "Pillai (robuste aux violations)"
    confiance <- if (score_param == 0) "modérée" else "élevée"
  } else {
    test_rec  <- "PERMANOVA"
    stat_rec  <- "Pseudo-F par permutations (999 permutations recommandees)"
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
  for (lev in levels(df[[fixed]])) {
    sub <- df[df[[fixed]] == lev, , drop = FALSE]
    sub[[tested]] <- droplevels(sub[[tested]])
    if (nlevels(sub[[tested]]) < 2 || nrow(sub) < 4) next
    
    sub <- hstat_cap_df_rows(sub, what = "PERMANOVA stratifiee")
    Y <- as.matrix(sub[, response, drop = FALSE])
    d <- tryCatch(vegan::vegdist(Y, method = dist_method), error = function(e) NULL)
    if (is.null(d)) next
    fml <- stats::as.formula(paste0("d ~ `", tested, "`"))
    ad <- tryCatch(
      vegan::adonis2(fml, data = sub, permutations = permutations, by = "terms"),
      error = function(e) NULL
    )
    if (is.null(ad)) next
    
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
  out
}


#' Resume l'etat actuel de l'analyse multivariee pour la frise de workflow
#' @return liste de booleens decrivant chaque etape du workflow
workflow_state <- function(values) {
  detect_interaction <- function(df, effet_col, p_col) {
    if (is.null(df)) return(FALSE)
    inter <- grepl(":", df[[effet_col]])
    any(inter) && any(df[[p_col]][inter] < 0.05, na.rm = TRUE)
  }
  list(
    has_data        = !is.null(values$filteredData),
    has_diagnostic  = !is.null(values$manovaMardia) || !is.null(values$manovaBoxM),
    has_test        = !is.null(values$manovaParamResults) || !is.null(values$manovaPermanovaResults),
    has_posthoc     = !is.null(values$manovaMultiPostHoc),
    is_param        = !is.null(values$manovaParamResults),
    is_nonparam     = !is.null(values$manovaPermanovaResults),
    has_interaction = detect_interaction(values$manovaParamResults, "Effet", "p_Pillai") ||
      detect_interaction(values$manovaPermanovaResults, "Effet", "p_value")
  )
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
                  estimate    = "Difference",
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
  cld <- tryCatch(
    multcomp::cld(em, adjust = adjust, Letters = letters, decreasing = TRUE),
    error = function(e) {
      tryCatch(emmeans::cld(em, adjust = adjust, Letters = letters, decreasing = TRUE),
               error = function(e2) NULL)
    }
  )
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


#' Test de Type II (ou Type III) pour un modele lm/glm
#' @param model modele
#' @param type 2 ou 3
#' @return data.frame avec colonnes : Predicteur, Chi2|F, ddl, p_value
lm_anova_table <- function(model, type = 2) {
  if (!requireNamespace("car", quietly = TRUE)) {
    res <- tryCatch(stats::anova(model), error = function(e) return(NULL))
    if (is.null(res)) return(NULL)
    df_out <- as.data.frame(res)
    df_out$Predicteur <- rownames(df_out)
    return(df_out)
  }
  res <- tryCatch(car::Anova(model, type = type), error = function(e) NULL)
  if (is.null(res)) return(NULL)
  df_out <- as.data.frame(res)
  df_out$Predicteur <- rownames(df_out)
  df_out <- df_out[df_out$Predicteur != "Residuals", , drop = FALSE]
  pcol <- intersect(c("Pr(>F)", "Pr(>Chisq)"), names(df_out))
  if (length(pcol) > 0) {
    df_out$Significatif <- ifelse(is.na(df_out[[pcol[1]]]), "NA",
                                  ifelse(df_out[[pcol[1]]] < 0.05, "Oui", "Non"))
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
round_numeric_df <- function(df, round_on, decimals = 2) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  
  # Nombre de decimales : si l'arrondi est actif, valeur choisie ; sinon
  # une precision d'affichage par defaut (3) garantissant la coherence
  # entre colonnes numeriques et colonnes texte "Moyenne ± ...".
  dec <- if (isTRUE(round_on)) {
    if (is.null(decimals) || is.na(decimals)) 2L else as.integer(decimals)
  } else {
    3L
  }
  
  # Noms possibles des colonnes texte (avant ou apres renommage d'affichage)
  sd_names <- c("Moyenne_pm_SD", "Moyenne \u00b1 Écart-type groupe")
  se_names <- c("Moyenne_pm_SE", "Moyenne \u00b1 Erreur-type groupe")
  has_col  <- function(nms) nms[nms %in% names(df)][1]
  sd_col   <- has_col(sd_names)
  se_col   <- has_col(se_names)
  
  # Reconstruction des colonnes texte a partir des colonnes numeriques sources,
  # afin que "Moyenne ± Écart-type" affiche EXACTEMENT la valeur de "Moyenne".
  fmt_pair <- function(m, s, grp) {
    out <- ifelse(is.na(m) | is.na(s), "NA",
                  paste0(formatC(round(m, dec), digits = dec, format = "f"),
                         " \u00b1 ",
                         formatC(round(s, dec), digits = dec, format = "f")))
    if (!is.null(grp)) out <- paste0(out, " ", grp)
    out
  }
  grp_vec <- if ("Groupes" %in% names(df)) as.character(df$Groupes) else NULL
  
  if (!is.na(sd_col) && all(c("Moyenne", "Écart_type") %in% names(df))) {
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

# -- Parametres globaux (modifiables) -----------------------------------------
# Au-dela de ce seuil, les CSV/Parquet basculent en mode hors-memoire (DuckDB).
# Configurable via HSTAT_BIGDATA_THRESHOLD_MB (defaut : 500 Mo).
HSTAT_BIGDATA_THRESHOLD <- {
  v <- suppressWarnings(as.numeric(Sys.getenv("HSTAT_BIGDATA_THRESHOLD_MB", "500")))
  if (!is.finite(v) || v < 50) v <- 500
  v * 1024^2
}
# Taille de l'echantillon de travail en mode hors-memoire.
# Configurable via HSTAT_SAMPLE_SIZE (defaut : 100 000 lignes).
HSTAT_SAMPLE_SIZE <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_SAMPLE_SIZE", "100000")))
  if (!is.finite(v) || v < 1000) 100000L else v
}

# -- Gros volumes : garde-fous pour 1 000 000+ lignes -------------------------
# Nombre maximal de points traces sur un nuage de points. Au-dela, un
# echantillon aleatoire est affiche (les statistiques, elles, restent
# calculees sur TOUTES les lignes). Configurable via HSTAT_PLOT_MAX_POINTS.
HSTAT_PLOT_MAX_POINTS <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_PLOT_MAX_POINTS", "100000")))
  if (!is.finite(v) || v < 1000) 100000L else v
}

# Echantillonne un data.frame pour l'AFFICHAGE graphique uniquement.
# Echantillon reproductible (seed locale) pour que le graphique ne change pas
# a chaque re-rendu. L'attribut 'hstat_sampled_from' garde le n d'origine.
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
      sprintf(paste0("Nuage de points : affichage d'un echantillon de %s points ",
                     "sur %s lignes (les calculs statistiques utilisent toutes ",
                     "les lignes)."),
              format(n, big.mark = " "), format(n0, big.mark = " ")),
      type = "message", duration = 8)
  }
  out
}

# Test de Shapiro-Wilk robuste aux gros echantillons : shapiro.test() refuse
# n > 5000 (limite de R). Au-dela, le test est applique a un sous-echantillon
# aleatoire de 5000 valeurs (pratique standard) et la methode l'indique.
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
    "Shapiro-Wilk normality test (sous-echantillon aleatoire de 5000 valeurs sur %s)",
    format(n, big.mark = " "))
  res
}

# =============================================================================
#  COMPARAISON A UNE VALEUR DE REFERENCE (NORME)
# -----------------------------------------------------------------------------
#  Tests a UN echantillon : au lieu de comparer deux groupes entre eux, on
#  confronte une serie de mesures a une valeur cible connue d'avance (norme
#  reglementaire, valeur theorique, objectif, historique...).
#
#  Famille quantitative — hstat_ref_test() :
#    "ttest"    test t a un echantillon (moyenne vs mu0, ecart-type estime)
#    "ztest"    test z a un echantillon (moyenne vs mu0, ecart-type connu)
#    "wilcoxon" test des rangs signes de Wilcoxon (mediane vs mu0)
#    "sign"     test du signe (mediane vs mu0, aucune hypothese de symetrie)
#    "variance" test du Chi2 de conformite d'une variance (s2 vs sigma0^2)
#    "tost"     test d'equivalence TOST (la moyenne est-elle DANS +/- marge
#               autour de la norme ?) — l'hypothese nulle y est l'ecart, pas
#               l'egalite : un p petit conclut a l'equivalence.
#
#  Famille categorielle — hstat_ref_prop_test() :
#    "binom"    test binomial exact (proportion vs p0)
#    "prop"     test du Chi2 / z de conformite d'une proportion (approx. normale)
#    "poisson"  test de conformite d'un taux d'evenements (lambda vs lambda0)
#
#  Toutes renvoient une liste normalisee : statistique, ddl, p_value, estimation,
#  intervalle de confiance, taille d'effet et interpretation redigee.
# =============================================================================

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
            label, format(mu), format.pval(p, digits = 3),
            format(round(estimate, digits)), unit, sens)
  else
    sprintf(paste0("%s : aucun écart significatif à la référence (%s) ",
                   "(p = %s). Les données sont compatibles avec la norme."),
            label, format(mu), format.pval(p, digits = 3))
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

# --- Famille quantitative ----------------------------------------------------
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
    tr <- suppressWarnings(stats::wilcox.test(
      x, mu = mu, alternative = alternative, conf.level = conf.level,
      conf.int = TRUE, exact = FALSE, correct = TRUE))
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
      "Compare la médiane à la norme ; suppose une distribution symétrique."))
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
              format.pval(p, digits = 3), format(round(m, 4)), format(margin),
              format(mu))
    else
      sprintf(paste0("Équivalence NON démontrée (p = %s) : on ne peut pas ",
                     "conclure que la moyenne observée (%s) reste dans la marge ",
                     "de +/- %s autour de la norme (%s)."),
              format.pval(p, digits = 3), format(round(m, 4)), format(margin),
              format(mu))
    return(.hstat_ref_out(
      "TOST (équivalence à la norme)",
      # Statistique du test unilateral CONTRAIGNANT (celui de plus grande
      # p-value : TOST conclut sur le maximum des deux). `isTRUE()` plutot
      # qu'une comparaison nue : une p-value non calculable ne doit pas lever
      # « missing value where TRUE/FALSE needed » au moment de l'affichage.
      if (isTRUE(lo$p.value >= hi$p.value)) unname(lo$statistic) else unname(hi$statistic),
      unname(lo$parameter), p, m, mu, ci, (m - mu) / s, "d de Cohen",
      "two.sided", n, interp,
      paste0("Hypothèse nulle inversée : un p < 0,05 conclut à l'équivalence. ",
             "Intervalle affiché au niveau 1 - 2 alpha (convention TOST).")))
  }

  stop("Méthode de comparaison à une référence inconnue : ", method)
}

# --- Famille categorielle ----------------------------------------------------
#  successes / n : effectif observe et effectif total. Pour "poisson", n est la
#  taille de la population (ou le temps d'exposition) et p0 le taux attendu par
#  unite.
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
                    paste("Référence =", format(res$reference))
                  else reference_label,
    Statistique = if (is.na(res$statistic)) NA_real_ else round(res$statistic, 4),
    ddl         = if (is.na(res$parameter)) NA_real_ else round(res$parameter, 2),
    p_value     = res$p.value,
    Interpretation = res$interpretation,
    stringsAsFactors = FALSE)
}

# Taille max des analyses a matrice de distances (dist, vegdist, silhouette,
# cophenetique) : au-dela, echantillonnage (une matrice de distances est en
# O(n^2) : 1 000 000 de lignes = ~4 To de RAM). Configurable par variable
# d'environnement.
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
    sprintf("%s : calcul sur un echantillon aleatoire de %s lignes (sur %s).",
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
                              what = "Analyse multivariee (distances)") {
  n0 <- nrow(Y)
  if (is.null(n0) || n0 <= max_n) return(list(Y = Y, group = group))
  idx <- hstat_cap_indices(n0, max_n)
  hstat_bigdata_note(what, length(idx), n0)
  list(Y = Y[idx, , drop = FALSE], group = droplevels(factor(group[idx])))
}

# Silhouette moyenne, robuste aux gros n : calcul vectorise (cluster::silhouette)
# sur un echantillon si necessaire (la version exacte est en O(n^2)).
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
  if (n0 > max_n) hstat_bigdata_note("Correlation cophenetique", length(idx), n0)
  tryCatch({
    d  <- stats::dist(coords[idx, , drop = FALSE])
    hc <- stats::hclust(d, method = hc_method)
    stats::cor(d, stats::cophenetic(hc))
  }, error = function(e) NA_real_)
}

# Proportion de paires concordantes entre deux partitions (indice de Rand
# simple), vectorisee : remplace combn() + apply(), impraticables des 10 000
# lignes (n(n-1)/2 paires).
hstat_pair_agreement <- function(a, b) {
  sa <- outer(a, a, "==")
  sb <- outer(b, b, "==")
  ut <- upper.tri(sa)
  mean(sa[ut] == sb[ut])
}

# Test de correlation de Kendall robuste aux gros n : tau est en O(n^2)
# (plusieurs heures au-dela de ~100 000) ; au-dela du seuil, calcul sur un
# echantillon aleatoire conjoint.
hstat_kendall_test <- function(x, y, max_n = HSTAT_KENDALL_MAX_N) {
  n0 <- length(x)
  if (n0 > max_n) {
    idx <- hstat_cap_indices(n0, max_n)
    x <- x[idx]; y <- y[idx]
    hstat_bigdata_note("Correlation de Kendall", length(idx), n0)
  }
  stats::cor.test(x, y, method = "kendall")
}

# Valeur numerique finie ou valeur par defaut. Contrairement a %||%, protege
# aussi contre NA, NaN et Inf (un numericInput vide renvoie NA, pas NULL, et
# un calcul de puissance peut legitimement renvoyer n = Inf).
hstat_finite <- function(x, default) {
  x <- suppressWarnings(as.numeric(if (is.null(x)) default else x))
  if (length(x) != 1 || !is.finite(x)) default else x
}

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
#
# Le motif ne tolere PAS l'indicateur d'espace (« % d »), et c'est deliberé :
# « 100 % de valeurs manquantes » n'est pas un gabarit, c'est un libellé où le
# pour-cent est suivi du mot « de ». Un motif plus permissif y voyait un
# marqueur, ecartait la phrase du dictionnaire du navigateur et la faisait
# passer pour une traduction fautive. Une seule definition, partagee par le
# filtre et par le test, pour que les deux ne divergent pas.
HSTAT_I18N_MARQUEUR <- "%[-0-9.]*[sdfgeix%]"

# -- Phrase COMPOSEE : on traduit le gabarit, jamais les arguments ------------
# Les ~217 phrases construites par sprintf() n'existent nulle part dans le DOM
# comme chaine entiere : le traducteur du navigateur, qui ne remplace que des
# correspondances completes, ne peut rien en faire. Elles restaient donc en
# francais quelle que soit la langue choisie.
#
# LE GABARIT EST LA CLE. « %s : %d valeur(s) modifiee(s) » entre au dictionnaire
# avec ses marqueurs intacts ; seule cette armature est traduite.
#
# CONSEQUENCE VOULUE : LES ARGUMENTS NE SONT JAMAIS TRADUITS. Ce sont eux qui
# portent les donnees de l'utilisateur -- un nom de variable, une modalite, un
# effectif. Ils traversent la traduction sans etre lus. C'est la meme regle que
# cote navigateur, obtenue ici par construction plutot que par precaution.
#
# Degradation douce : un gabarit absent du dictionnaire ressort en francais,
# correctement rempli, au lieu de disparaitre ou d'afficher une cle technique.
trf <- function(fmt, ..., lang = hstat_langue_session()) {
  g <- tr(fmt, lang)
  # Une traduction fautive peut avoir perdu un marqueur : sprintf leverait
  # alors « too few arguments » et ferait tomber toute la sortie pour une
  # simple erreur de dictionnaire. On retombe sur le francais, qui marche.
  out <- tryCatch(sprintf(g, ...), error = function(e) NULL)
  if (is.null(out)) sprintf(fmt, ...) else out
}

# =============================================================================
#  LES TERMES DU FICHIER DE L'UTILISATEUR NE SE TRADUISENT JAMAIS
# -----------------------------------------------------------------------------
#  La regle de longueur dans les cellules (LONGUEUR_CELLULE, cote navigateur)
#  est une heuristique : elle protege « Oui » mais laisserait passer un
#  en-tete de colonne, et un nouveau tableau ajoute demain echapperait a toute
#  annotation posee a la main.
#
#  On procede donc a l'envers : le serveur ENVOIE au navigateur la liste des
#  termes qui viennent du fichier -- noms de colonnes et modalites des
#  variables qualitatives. Le traducteur ne touche jamais un texte qui figure
#  dans cette liste, ou qu'il apparaisse. Un tableau ajoute plus tard est
#  protege sans qu'on y pense.
#
#  Le prix est assume : si une colonne s'appelle « Total », le libelle
#  d'interface « Total » cesse d'etre traduit lui aussi. C'est la degradation
#  douce de la conception ; alterer une donnee n'en est pas une.
#
#  La liste est BORNEE. Une colonne de texte libre porte autant de modalites
#  que de lignes ; l'envoyer entiere ferait grossir la page sans rien proteger
#  d'utile (une phrase entiere ne coincide pas avec un libelle d'interface).
#  D'ou `max_modalites` par colonne et `max_termes` au total.
# =============================================================================
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

# JSON pret a envoyer au navigateur. Rend "[]" plutot qu'une erreur : cette
# valeur alimente un message Shiny, ou une erreur couperait la bascule.
hstat_i18n_termes_json <- function(df, ...) {
  t <- tryCatch(hstat_i18n_termes_donnees(df, ...), error = function(e) character(0))
  if (!length(t)) return("[]")
  # Meme echappement que hstat_i18n_json(). `fixed = TRUE` est indispensable :
  # une barre oblique inverse seule n'est pas une expression reguliere valide
  # (« Trailing backslash »), et le terme vient du fichier de l'utilisateur.
  esc <- function(s) {
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    s <- gsub("\"", "\\\"", s, fixed = TRUE)
    s <- gsub("\r", "\\r", s, fixed = TRUE)
    s <- gsub("\n", "\\n", s, fixed = TRUE)
    gsub("\t", "\\t", s, fixed = TRUE)
  }
  paste0("[", paste(sprintf("\"%s\"", esc(t)), collapse = ","), "]")
}

# =============================================================================
#  SEUILS D'EFFICACITE : COMPARAISON DE CHAQUE MODALITE AU TEMOIN
# -----------------------------------------------------------------------------
#  Formule d'Abbott, celle qu'emploient l'agronomie, la phytopharmacie et
#  l'entomologie pour dire de combien un traitement reduit ce que l'on mesure
#  par rapport a un temoin non traite :
#
#      efficacite (%) = (temoin - traitement) x 100 / temoin
#
#  QUATRE DECISIONS, CHACUNE TESTEE
#
#  1. LE TEMOIN VAUT ZERO PAR DEFINITION, ET ON L'ECRIT. La formule donnerait
#     bien 0 pour lui... sauf si sa valeur est nulle, ou elle rend NaN (0/0).
#     On pose donc 0 explicitement : le temoin ne se compare pas a lui-meme.
#
#  2. UN TEMOIN NUL REND L'EFFICACITE INDEFINIE POUR TOUT LE MONDE. Diviser
#     par zero produirait des Inf silencieux qui ressortiraient en graphique
#     comme des barres demesurees. On rend NA et on le DIT (attribut `message`).
#
#  3. UNE EFFICACITE NEGATIVE EST UN RESULTAT, PAS UNE ERREUR. Elle signifie
#     que la modalite fait moins bien que le temoin. La borner a zero
#     masquerait precisement ce qu'il faut voir.
#
#  4. LE GROUPEMENT FACULTATIF EST CE QUI REND LA SUITE POSSIBLE. Sans lui, il
#     n'y a qu'une ligne par modalite et plus rien a tester. En calculant
#     l'efficacite DANS chaque repetition (bloc, essai, site), on obtient une
#     vraie variable, analysable ensuite par ANOVA ou comparaisons multiples.
#
#  `agg` : "moyenne" (defaut), "mediane" ou "somme" -- la valeur resumee de
#  chaque modalite avant comparaison.
# =============================================================================
HSTAT_EFF_AGG <- c("Moyenne" = "moyenne", "Mediane" = "mediane", "Somme" = "somme")

hstat_efficacite <- function(df, var_modalite, vars_reponse, temoin,
                             agg = c("moyenne", "mediane", "somme"),
                             var_repetition = NULL,
                             mode = c("cumul", "par_repetition"),
                             var_groupe = NULL) {
  agg <- match.arg(agg); mode <- match.arg(mode)
  # `var_groupe` est l'ancien argument. Il decoupait le calcul par groupe : on
  # lui GARDE ce sens, sinon un appel existant changerait silencieusement de
  # resultat -- 4 lignes la ou il en rendait 12. La repetition se declare
  # desormais par `var_repetition`, avec un `mode` qui dit ce qu'on en fait.
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
    return(msg(vide, "Aucune donnee : chargez un jeu de donnees."))
  if (is.null(var_modalite) || !nzchar(var_modalite[1]) ||
      !(var_modalite[1] %in% names(df)))
    return(msg(vide, "Choisissez la variable qui porte les traitements."))
  vars_reponse <- intersect(as.character(vars_reponse), names(df))
  if (!length(vars_reponse))
    return(msg(vide, "Choisissez au moins une variable a mesurer."))

  modal <- trimws(as.character(df[[var_modalite[1]]]))
  modal[is.na(modal) | !nzchar(modal)] <- NA_character_
  temoin <- trimws(as.character(temoin)[1])
  if (is.na(temoin) || !nzchar(temoin) || !(temoin %in% modal))
    return(msg(vide, paste0("Le temoin choisi n'existe pas dans « ",
                            var_modalite[1], " » : choisissez une modalite presente.")))

  a_rep <- !is.null(var_repetition) && nzchar(var_repetition[1]) &&
           var_repetition[1] %in% names(df)
  # Une variable de repetition demandee mais introuvable etait ignoree EN
  # SILENCE : l'utilisateur croyait ses repetitions prises en compte alors que
  # le calcul les melangeait. On refuse plutot que de rendre un chiffre faux.
  if (!is.null(var_repetition) && nzchar(var_repetition[1]) && !a_rep)
    return(msg(vide, sprintf(paste0("La variable de repetition « %s » est introuvable ",
                                    "dans les donnees : choisissez-en une autre."),
                             var_repetition[1])))
  rep_v <- if (a_rep) trimws(as.character(df[[var_repetition[1]]]))
           else rep("", NROW(df))
  rep_v[is.na(rep_v)] <- "(manquant)"

  # DEUX FACONS DE TENIR COMPTE DES REPETITIONS, ET ELLES NE REPONDENT PAS A LA
  # MEME QUESTION.
  #   "cumul"          -> les repetitions sont MISES EN COMMUN : la moyenne (ou
  #                       la somme) de la modalite porte sur toutes ses
  #                       repetitions, et l'on obtient UNE efficacite par
  #                       modalite. C'est le chiffre que l'on publie.
  #   "par_repetition" -> l'efficacite est calculee DANS chaque repetition, ce
  #                       qui donne autant de valeurs que de repetitions --
  #                       donc une variable, analysable par ANOVA ou
  #                       comparaisons multiples.
  # Le decoupage n'a lieu que dans le second cas.
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
      # DEUX CAUSES DIFFERENTES, DEUX MESSAGES. « Sans valeur mesurable »
      # couvrait aussi le cas ou le temoin est simplement ABSENT du groupe --
      # un defaut de plan, pas de mesure. Constate en groupant par une colonne
      # qui compte une modalite par ligne : l'utilisateur lisait un message qui
      # ne nommait pas sa vraie erreur.
      # `<-` et NON `<<-` : la boucle `for` ne cree pas de cadre, on est dans le
      # corps de la fonction. `<<-` y ecrirait dans l'environnement ENGLOBANT et
      # sauterait la variable locale, qui resterait vide -- le miroir exact du
      # defaut corrige dans mod_tests.R, ou c'est `<<-` qu'il fallait.
      if (!any(dans_temoin))
        groupes_sans_temoin <- c(groupes_sans_temoin, g)
      else if (!is.finite(ref[["v"]]))
        alertes <- c(alertes, sprintf("temoin sans valeur mesurable pour « %s »", v))
      else if (ref[["v"]] == 0)
        alertes <- c(alertes, sprintf("temoin nul pour « %s » : l'efficacite n'est pas definissable", v))
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

  # LA SOMME N'EST COMPARABLE QUE SI LES REPETITIONS SONT EQUILIBREES. Avec un
  # nombre de repetitions inegal, la modalite la plus repetee accumule
  # mecaniquement davantage et ressort artificiellement « moins efficace » --
  # un artefact de plan pris pour un resultat. La moyenne, elle, n'en souffre
  # pas. On le dit plutot que de laisser publier le chiffre.
  if (a_rep && identical(agg, "somme") && !is.null(out$Repetitions)) {
    nr <- out$Repetitions[!is.na(out$Repetitions) & out$Repetitions > 0]
    if (length(unique(nr)) > 1)
      alertes <- c(sprintf(paste0("repetitions inegales (%s) : une SOMME n'est pas ",
                                  "comparable entre modalites inegalement repetees, ",
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
    alertes <- c(sprintf(paste0("le temoin « %s » est absent de %s groupe(s) (%s) : ",
                                "verifiez la variable de groupement, un groupe sans ",
                                "temoin n'a rien a quoi se comparer"),
                         temoin, length(gst),
                         paste(utils::head(gst, 4), collapse = ", ")),
                 alertes)
  attr(out, "groupes_sans_temoin") <- gst
  msg(out, if (length(alertes))
    paste0("Attention : ", paste(unique(alertes), collapse = " ; "), ".")
    else sprintf("%s modalite(s) comparee(s) au temoin « %s » (%s).",
                 length(niveaux), temoin, agg))
}

# Modalites d'une colonne, temoin exclu : « une fois le temoin choisi, les
# autres modalites passent dans une variable ». Sert a l'affichage et a la
# verification, et rend character(0) plutot qu'une erreur sur une entree vide.
hstat_eff_modalites <- function(df, var_modalite, temoin = NULL) {
  if (!is.data.frame(df) || is.null(var_modalite) || !length(var_modalite) ||
      !(var_modalite[1] %in% names(df))) return(character(0))
  m <- trimws(as.character(df[[var_modalite[1]]]))
  m <- sort(unique(m[!is.na(m) & nzchar(m)]))
  if (is.null(temoin) || !length(temoin)) return(m)
  setdiff(m, trimws(as.character(temoin)[1]))
}

# =============================================================================
#  VARIABLES ENTIEREMENT NULLES
# -----------------------------------------------------------------------------
#  Une colonne dont TOUTES les valeurs observees valent zero ne porte aucune
#  information : variance nulle, correlation indefinie, aucun test possible.
#  Elle vient presque toujours d'un export ou d'un questionnaire ou le zero
#  signifie « non mesure » plutot que « mesure a zero » -- d'ou les deux gestes
#  offerts a l'utilisateur : corriger les valeurs, ou retirer la variable.
#
#  TROIS DECISIONS, CHACUNE TESTEE
#
#  1. LES MANQUANTS NE COMPTENT PAS COMME DES ZEROS. Une colonne 0/0/NA/0 est
#     nulle sur ce qu'elle montre ; la colonne des manquants le dit a cote, au
#     lieu de melanger « mesure a zero » et « pas de mesure ».
#
#  2. UNE COLONNE ENTIEREMENT VIDE N'EST PAS UNE COLONNE DE ZEROS. Sans
#     observation, il n'y a rien a comparer a zero. L'annoncer comme nulle
#     serait faux, et la taire serait trompeur : elle ressort dans l'attribut
#     "vides", que l'interface nomme separement.
#
#  3. LES BOOLEENS SONT ECARTES. `FALSE` vaut bien 0 en arithmetique, mais une
#     colonne de « non » est une reponse, pas une mesure a zero ; la ranger ici
#     ferait proposer d'en « corriger les valeurs ».
#
#  Le texte lu comme nombre passe par hstat_as_numeric_fr() : un CSV importe
#  livre couramment des "0" en chaine, et la virgule decimale francaise doit
#  etre comprise ("0,0").
# =============================================================================
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
    # Decision 2 AVANT la 3, et sur le TEXTE VIDE autant que sur NA. Une
    # colonne sans valeur arrive tantot typee LOGIQUE (lecteurs de CSV), tantot
    # remplie de chaines vides (Excel, exports SPSS). Tester `is.na()` seul en
    # laissait passer la moitie, et le test des booleens place avant faisait
    # disparaitre l'autre : dans les deux cas, en silence.
    plat <- if (is.character(x) || is.factor(x)) trimws(as.character(x)) else x
    if (all(is.na(plat) | (is.character(plat) & !nzchar(plat)))) {
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
      Type = if (is.numeric(x)) "numerique" else class(x)[1],
      Observations = length(obs),
      Zeros = n_zero,
      `Zeros (%)` = round(part * 100, 2),
      Manquants = n_na,
      Constat = if (part >= 1 && n_na == 0)
        "Toutes les valeurs sont nulles : la variable n'apporte aucune information."
      else if (part >= 1)
        sprintf(paste0("Toutes les valeurs observees sont nulles ; %s valeur(s) ",
                       "manquante(s). Verifiez si le zero signifie ici ",
                       "« non mesure »."), n_na)
      else
        trf("%s %% des valeurs observees sont nulles.", round(part * 100, 2)),
      check.names = FALSE, stringsAsFactors = FALSE)
  }
  res <- if (length(lignes)) do.call(rbind, lignes) else vide
  attr(res, "vides") <- vides
  res
}

# -- Saisie directe des valeurs d'une variable --------------------------------
# « Editer les valeurs » d'une colonne entierement nulle, c'est le plus souvent
# ressaisir les vraies mesures.
#
# LA VIRGULE EST UNE DECIMALE, PAS UN SEPARATEUR. Elle ne peut pas etre les
# deux : dans une application francaise, « 2,5 » est la facon normale d'ecrire
# deux et demi, et la traiter en separateur en faisait DEUX valeurs -- donc un
# decompte faux, donc un refus incomprehensible. Les separateurs sont le retour
# a la ligne et le point-virgule, exactement la convention du CSV francais, qui
# existe pour cette raison.
#
# Le decompte est VERIFIE : une liste plus courte ou plus longue que la colonne
# decalerait silencieusement toutes les observations, ce qui est pire que de
# refuser la saisie. "NA" et une entree vide valent manquant.
# Rend list(ok, valeurs, message).
hstat_zero_valeurs_parse <- function(txt, n) {
  n <- suppressWarnings(as.integer(n)[1])
  if (!is.finite(n) || n < 1)
    return(list(ok = FALSE, valeurs = NULL,
                message = "Aucune ligne a remplir : chargez des donnees."))
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
                                         "pour qu'elles correspondent une a une."),
                                  length(brut), n)))
  manquant <- !nzchar(brut) | toupper(brut) %in% c("NA", "N/A")
  num <- suppressWarnings(as.numeric(gsub(",", ".", brut, fixed = TRUE)))
  mauvais <- which(!manquant & is.na(num))
  if (length(mauvais))
    return(list(ok = FALSE, valeurs = NULL,
                message = sprintf(paste0("Valeur non numerique en position %s ",
                                         "(« %s ») : corrigez-la ou ecrivez NA."),
                                  mauvais[1], brut[mauvais[1]])))
  num[manquant] <- NA_real_
  list(ok = TRUE, valeurs = num, message = trf("%s valeur(s) prise(s) en compte.", n))
}

# -- Creation de classes d'intervalles (discretisation) -----------------------
# Transforme une variable numerique en classes (ex. classes d'age), sous forme
# de facteur ORDONNE. Methodes : largeur egale, effectifs egaux (quantiles),
# bornes personnalisees. Etiquettes automatiques ("[0 ; 3[") ou fournies.
# Retourne list(ok, factor, breaks, counts, msg, n_na_created).
# interval_style : convention de bornes des classes
#  "std_last_closed"  -> [a;b[ , [b;c[ , ... , [y;z]   (dernière fermée des 2 côtés)
#  "all_left_closed"  -> [a;b[ , [b;c[ , ... , [y;z[   (toutes fermées-gauche/ouvertes-droite)
#  "mixed_open"       -> [a;b[ , ]b;c[ , ... , ]y;z]   (1re fermée-gauche, milieu ouvert 2 côtés, dernière ouverte-gauche/fermée-droite)
#  "all_right_closed" -> [a;b] , ]b;c] , ... , ]y;z]   (1re fermée 2 côtés, autres ouvertes-gauche/fermées-droite ; ancienne convention)
#  "all_closed"       -> [a;b] , [b;c] , ... , [y;z]   (toutes fermées des 2 côtés)
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
      lab[length(lab)] <- sprintf("]%s ; %s]", fmt(lo[length(lo)]), fmt(hi[length(hi)])) # derniere : fermee droite
      lab
    },
    "all_right_closed" = {
      lab <- sprintf("]%s ; %s]", fmt(lo), fmt(hi))       # toutes fermees a droite
      lab[1] <- sprintf("[%s ; %s]", fmt(lo[1]), fmt(hi[1]))               # 1re : fermee des 2 cotes
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

  # Binning coherent avec la convention. Toutes les conventions demandees sont
  # fermees a GAUCHE (right = FALSE). Pour capturer la valeur maximale :
  #  - std_last_closed : include.lowest = TRUE ferme la derniere classe a droite.
  #  - all_left_closed / mixed_open : la derniere classe reste ouverte a droite,
  #    on etend donc la borne haute d'un epsilon pour ne perdre aucune valeur
  #    (la valeur max reste affichee dans "[... ; max[" resp. "]... ; max]").
  cut_breaks <- breaks
  # all_right_closed : bornes fermees a droite (right = TRUE), 1re classe fermee
  # a gauche via include.lowest.
  use_right <- (interval_style == "all_right_closed")
  # include.lowest ferme la borne extreme du cote "ouvert" :
  #  - std_last_closed / all_closed : ferme la derniere classe a droite
  #  - all_right_closed : ferme la premiere classe a gauche
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
# pour DuckDB : doublement des guillemets internes puis encadrement par "...".
hstat_sql_ident <- function(x) sprintf('"%s"', gsub('"', '""', x))

# =============================================================================
#  VERSION DU PAQUET -- source unique de verite
# -----------------------------------------------------------------------------
#  Le numero de version ne vit qu'a UN seul endroit : le champ Version: de
#  DESCRIPTION. Tout ce qui l'affiche (citation, en-tetes, exports) doit le lire
#  ici, jamais le recopier.
#
#  Deux situations a couvrir :
#   - HStat installe comme paquet : utils::packageVersion() suffit ;
#   - application lancee depuis les SOURCES (runApp("inst/app"), deploiement
#     shinyapps.io du dossier, source("HStat.R")...) : le paquet n'est alors pas
#     installe, packageVersion() echoue, et il faut lire DESCRIPTION sur disque.
#  Sans ce second cas, la citation restait figee sur un numero code en dur.
# =============================================================================

# Localise DESCRIPTION, que l'on tourne depuis les sources ou depuis l'installe.
# Les chemins relatifs couvrent les repertoires de travail usuels : racine du
# paquet, inst/app/ (repertoire courant quand l'application tourne), inst/.
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
# packageVersion() leve une erreur et packageDate() un AVERTISSEMENT quand le
# paquet n'est pas installe : on neutralise les deux, sinon l'onglet de citation
# deverse un avertissement a chaque rendu.
hstat_version <- function(fallback = "0.0.0") {
  v <- suppressWarnings(tryCatch(as.character(utils::packageVersion("HStat")),
                                 error = function(e) NA_character_))
  if (length(v) && !is.na(v) && nzchar(v)) return(v)
  v <- .hstat_description_field("Version")
  if (!is.na(v)) return(v)
  fallback
}

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

# Genere la citation du package HStat dans differents styles.
# Version et annee suivent automatiquement DESCRIPTION (cf. hstat_version()).
hstat_citation <- function(style = c("text", "bibtex", "ris", "apa", "vancouver", "markdown")) {
  style <- match.arg(style)
  vers <- hstat_version()
  year <- hstat_pkg_year()
  author_last <- "KOUADIO"; author_first <- "Houphouet"; author_initial <- "H"
  title <- "HStat : Application Shiny interactive pour l'analyse statistique"
  url   <- "https://github.com/houphouet/hstat"
  orcid <- "0000-0002-8238-1091"

  switch(style,
    "text" = sprintf(
      "%s, %s (%s). %s. Version %s. %s",
      author_last, author_first, year, title, vers, url),

    "apa" = sprintf(
      "%s, %s. (%s). %s (Version %s) [Logiciel R]. %s",
      author_last, substr(author_first, 1, 1), year, title, vers, url),

    "vancouver" = sprintf(
      "%s %s. %s [Logiciel R]. Version %s. %s; %s.",
      author_last, author_initial, title, vers, year, url),

    "markdown" = sprintf(
      "%s, %s (%s). *%s*. Version %s. [%s](%s)",
      author_last, author_first, year, title, vers, url, url),

    "bibtex" = paste(
      "@Manual{hstat,",
      sprintf("  title  = {%s},", title),
      sprintf("  author = {%s %s},", author_first, author_last),
      sprintf("  year   = {%s},", year),
      sprintf("  note   = {Version %s},", vers),
      sprintf("  url    = {%s},", url),
      "}", sep = "\n"),

    "ris" = paste(
      "TY  - COMP",
      sprintf("AU  - %s, %s", author_last, author_first),
      sprintf("PY  - %s", year),
      sprintf("TI  - %s", title),
      sprintf("ET  - Version %s", vers),
      sprintf("UR  - %s", url),
      "ER  - ", sep = "\n")
  )
}

# -- Capuchons de moustache pour l'EXPORT des boxplots ------------------------
# A l'ecran, les boxplots sont rendus par plotly (ggplotly), qui dessine ses
# propres capuchons. A l'export, ggsave utilise le ggplot brut ou geom_boxplot
# n'en dessine pas. Cette fonction insere, juste avant chaque couche boxplot,
# une couche stat_boxplot(geom = "errorbar") reprenant les memes donnees, le
# meme mapping (fill converti en group : l'errorbar ignore fill et perdrait le
# "dodge") et la meme position -> capuchons alignes boite par boite.
# Idempotente : ne fait rien si des capuchons sont deja presents.
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
        # Vraies boites multiples par x (fill au niveau de la COUCHE) : pour
        # des capuchons etroits ET alignes, les deux couches doivent partager
        # un position_dodge de largeur FIXE (dodge2 emballerait le capuchon
        # etroit a une position differente de sa boite). On harmonise donc la
        # position de la couche boxplot elle-meme.
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
# Detecte si le fichier est de l'UTF-8 valide ; sinon on suppose du Latin-1/CP1252
# (cas le plus frequent pour les CSV Excel francais). Lecture d'un echantillon
# d'octets seulement, donc rapide.
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
    # On declare l'encodage detecte a fread : il convertit nativement (rapide) et
    # ne bloque pas. "Latin-1" couvre les CSV Excel/Windows francais.
    fenc <- if (identical(enc, "Latin-1")) "Latin-1" else "UTF-8"
    as.data.frame(data.table::fread(path, header = header, sep = sep,
                            data.table = FALSE, check.names = FALSE,
                            showProgress = FALSE, encoding = fenc))
  } else {
    fe <- if (identical(enc, "Latin-1")) "latin1" else "UTF-8"
    read.csv(path, header = header, sep = sep, check.names = FALSE,
             stringsAsFactors = FALSE, fileEncoding = fe)
  }
  # Filet de securite : garantit des chaines UTF-8 valides en sortie.
  hstat_df_to_utf8(df)
}

# -- Lecture des EN-TETES uniquement (pour peupler les menus de fusion) --------
# Ne lit que les noms de colonnes, sans charger les donnees, afin d'eviter de
# saturer la memoire quand l'utilisateur selectionne plusieurs gros fichiers.
hstat_read_header_mem <- function(path, sep = ",") {
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    if (ext %in% c("csv", "txt", "tsv")) {
      s <- if (ext == "tsv") "\t" else sep
      # Lecture ultra-defensive de la 1re ligne. On lit en BINAIRE puis on convertit,
      # ce qui evite a la fois les plantages et la TRONCATURE des fichiers Latin-1
      # (une connexion UTF-8 s'arreterait au 1er octet invalide).
      first <- tryCatch({
        bytes <- readBin(path, "raw", n = 65536L)
        nl <- which(bytes == as.raw(0x0A))[1]          # 1re fin de ligne
        if (!is.na(nl)) bytes <- bytes[seq_len(nl - 1L)]
        line <- rawToChar(bytes[bytes != as.raw(0x0D)]) # retire les \r
        hstat_to_utf8(line)
      }, error = function(e) "")
      if (length(first) == 0 || !nzchar(first)) return(NULL)
      first <- sub("^\ufeff", "", first)
      parts <- strsplit(first, s, fixed = TRUE)[[1]]
      parts <- trimws(gsub('^"|"$', '', parts))
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) return(NULL)
      make.unique(hstat_to_utf8(parts))
    } else if (ext %in% c("xlsx", "xls")) {
      names(as.data.frame(readxl::read_excel(path, sheet = 1, n_max = 1)))
    } else if (ext == "rds") {
      names(as.data.frame(readRDS(path)))
    } else if (ext == "sav") {
      names(as.data.frame(haven::read_sav(path, n_max = 1)))
    } else if (ext == "dta") {
      names(as.data.frame(haven::read_dta(path, n_max = 1)))
    } else {
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      first <- readLines(con, n = 1, warn = FALSE)
      if (length(first) == 0) return(NULL)
      first <- sub("^\ufeff", "", first)
      parts <- trimws(gsub('^"|"$', '', strsplit(first, sep, fixed = TRUE)[[1]]))
      make.unique(parts[nzchar(parts)])
    }
  }, error = function(e) NULL)
}

# -- Assainissement d'encodage : garantit des chaines UTF-8 valides -----------
# Les CSV generes sous Windows/Excel en francais sont souvent en Latin-1/CP1252.
# Lus comme UTF-8, ils contiennent des octets invalides qui font planter nchar(),
# strsplit(), etc. ("invalid multibyte string"). Cette fonction convertit toute
# chaine en UTF-8 valide (essai Latin-1 -> UTF-8, puis substitution des octets
# restants), de facon vectorisee et sure.
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
# Garantit aussi des noms de colonnes UNIQUES (make.unique) : des en-tetes
# dupliques (CSV mal formes, fusions cote a cote) provoquaient sinon des erreurs
# ggplot "data must be uniquely named but has duplicate columns".
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

# -- Lecture generique en memoire pour la FUSION de fichiers -------------------
# Lit un fichier (CSV/TXT/TSV/Excel/RDS/SPSS/Stata) en data.frame complet.
hstat_read_any_mem <- function(path, sep = ",", name = NULL) {
  ext <- tolower(tools::file_ext(path))
  df <- tryCatch({
    if (ext %in% c("csv", "txt", "tsv")) {
      s <- if (ext == "tsv") "\t" else sep
      hstat_read_csv_mem(path, header = TRUE, sep = s)
    } else if (ext %in% c("xlsx", "xls")) {
      as.data.frame(readxl::read_excel(path, sheet = 1))
    } else if (ext == "rds") {
      as.data.frame(readRDS(path))
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
# key_left / key_right : noms de colonnes cle (jointures). add_source : colonne
# d'origine pour l'empilement. Renvoie list(ok, data, msg).
# ===========================================================================
# CLASSEUR EXCEL : LES FEUILLES SE TRAITENT COMME DES FICHIERS
# ---------------------------------------------------------------------------
# Un classeur d'enquete porte tres souvent une feuille par annee, par site ou
# par vague. Ne lire que la premiere revient a jeter le reste des donnees, et
# recopier chaque feuille dans un fichier separe pour pouvoir les fusionner est
# un travail manuel que l'application peut faire.
#
# On ne construit PAS de moteur de fusion parallele : les feuilles sont lues en
# une liste de tableaux et passees a `hstat_merge_frames()`, celui-la meme qui
# sert deja a fusionner plusieurs fichiers. Toutes ses jointures — empilement,
# jointure par cle, intersection… — deviennent donc disponibles sur les
# feuilles sans une ligne de logique supplementaire.
# ===========================================================================

# Noms des feuilles d'un classeur. Rend character(0) — jamais une erreur — pour
# tout ce qui n'est pas un classeur lisible : l'appelant est une sortie Shiny,
# une erreur y ferait tomber tout le panneau de chargement.
hstat_excel_sheets <- function(path) {
  if (is.null(path) || !length(path) || !nzchar(path[1]) || !file.exists(path[1]))
    return(character(0))
  if (!(tolower(tools::file_ext(path[1])) %in% c("xlsx", "xls")))
    return(character(0))
  s <- tryCatch(readxl::excel_sheets(path[1]), error = function(e) character(0))
  s <- as.character(s)
  s[!is.na(s) & nzchar(s)]
}

# Lit les feuilles demandees. Une feuille vide ou illisible est ECARTEE et
# nommee dans `ignorees` plutot que de faire echouer l'ensemble : sur un
# classeur de douze feuilles, une seule mal formee ne doit pas tout bloquer.
hstat_excel_read_sheets <- function(path, sheets = NULL) {
  dispo <- hstat_excel_sheets(path)
  if (!length(dispo))
    return(list(frames = list(), names = character(0), ignorees = character(0),
                msg = "Ce fichier n'est pas un classeur Excel lisible."))
  choisies <- if (is.null(sheets) || !length(sheets)) dispo
              else intersect(as.character(sheets), dispo)
  if (!length(choisies))
    return(list(frames = list(), names = character(0), ignorees = character(0),
                msg = "Aucune des feuilles demandees n'existe dans ce classeur."))
  frames <- list(); noms <- character(0); ignorees <- character(0)
  for (s in choisies) {
    d <- tryCatch(as.data.frame(readxl::read_excel(path, sheet = s),
                                stringsAsFactors = FALSE),
                  error = function(e) NULL)
    if (is.null(d) || !nrow(d) || !ncol(d)) { ignorees <- c(ignorees, s); next }
    frames[[length(frames) + 1L]] <- d
    noms <- c(noms, s)
  }
  msg <- if (!length(frames)) "Aucune feuille exploitable dans ce classeur."
         else sprintf("%d feuille(s) lue(s)%s.", length(frames),
                      if (length(ignorees))
                        sprintf(", %d ecartee(s) car vide(s) ou illisible(s) : %s",
                                length(ignorees), paste(ignorees, collapse = ", "))
                      else "")
  list(frames = frames, names = noms, ignorees = ignorees, msg = msg)
}

# Compare la structure des feuilles. C'est ce qui permet de CONSEILLER une
# fusion plutot que de laisser l'utilisateur deviner : des feuilles aux memes
# colonnes s'empilent, des feuilles differentes se joignent par une cle.
hstat_excel_compat <- function(frames, names_ = NULL) {
  if (!length(frames))
    return(list(identiques = FALSE, communes = character(0),
                suggestion = "rows", msg = "Aucune feuille a comparer."))
  cols <- lapply(frames, names)
  communes <- Reduce(intersect, cols)
  toutes <- unique(unlist(cols))
  identiques <- length(communes) == length(toutes) &&
                all(vapply(cols, function(c0) length(c0) == length(toutes), logical(1)))
  msg <- if (identiques)
    sprintf(paste("Les %d feuilles portent exactement les memes %d colonnes :",
                  "l'empilement les met bout a bout, une ligne par observation."),
            length(frames), length(toutes))
  else if (length(communes))
    sprintf(paste("Les feuilles ont %d colonne(s) en commun (%s) et %d colonne(s)",
                  "propres. Une jointure par cle rapproche les lignes qui se",
                  "correspondent ; l'empilement les mettrait bout a bout en",
                  "laissant des vides."),
            length(communes), paste(utils::head(communes, 6), collapse = ", "),
            length(setdiff(toutes, communes)))
  else
    paste("Les feuilles n'ont AUCUNE colonne en commun : ni jointure ni",
          "empilement n'a de sens en l'etat. Verifiez que la premiere ligne de",
          "chaque feuille porte bien les en-tetes.")
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

  # ---- Empilement de lignes (UNION) ----
  if (type %in% c("rows", "union_distinct")) {
    # Valeur d'origine inscrite dans la colonne source : nom du fichier, ou nombre
    # extrait du nom (utile pour une colonne "année" depuis 2021.csv, 2022.csv...).
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
        msg = sprintf("Union distincte de %d fichiers : %d lignes uniques, %d colonnes.",
                      length(frames), nrow(out), ncol(out))))
    }
    return(list(ok = TRUE, data = out,
                msg = sprintf("Empilement de %d fichiers : %d lignes, %d colonnes.",
                              length(frames), nrow(out), ncol(out))))
  }

  # ---- Juxtaposition de colonnes ----
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
                msg = sprintf("Juxtaposition de %d fichiers : %d lignes, %d colonnes.",
                              length(frames), nrow(out), ncol(out))))
  }

  # ---- Opérations ENSEMBLISTES sur les lignes (mêmes colonnes) ----
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
      msg = sprintf("%s sur colonnes communes : %d lignes, %d colonnes.", lbl, nrow(out), ncol(out))))
  }

  # ---- Jointure CROISÉE (produit cartésien, sans clé) ----
  if (type == "cross") {
    # Garde-fou mémoire : le produit cartésien peut exploser. On refuse au-delà
    # d'un seuil pour éviter de saturer la mémoire et de faire planter l'app.
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

  # ---- SEMI / ANTI jointures (filtrent le 1er ou le 2e, sans ajouter de colonnes) ----
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
      msg = sprintf("%s sur « %s » : %d lignes, %d colonnes.",
                    lbl, paste(kl, collapse = "+"), nrow(out), ncol(out))))
  }

  # ---- MISE À JOUR / COALESCE : complète/remplace les valeurs du 1er par le 2e ----
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

  # ---- Jointures classiques (inner/left/right/full), clés composites, enchaînées ----
  if (!type %in% c("inner", "left", "right", "full"))
    return(list(ok = FALSE, data = NULL, msg = sprintf("Type de fusion inconnu : %s", type)))
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
       msg = sprintf("Jointure %s de %d fichiers sur « %s » : %d lignes, %d colonnes.",
                     type, length(frames), paste(kl, collapse = "+"), nrow(acc), ncol(acc)))
}

# -- Ouverture d'une connexion DuckDB en memoire ------------------------------
hstat_duckdb_connect <- function() {
  if (!hstat_has_duckdb()) stop("Le package 'duckdb' est requis pour le mode hors-memoire.")
  con <- DBI::dbConnect(duckdb::duckdb())
  # Reglages "tres gros volumes" :
  # - temp_directory : DuckDB deborde sur disque au lieu d'echouer en RAM
  #   (tris, agregations et echantillonnages sur des milliards de lignes) ;
  # - preserve_insertion_order=false : reduit fortement la memoire des scans ;
  # - memory_limit optionnel via HSTAT_DUCKDB_MEMORY (ex. "8GB").
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
# Aucune donnee n'est materialisee : DuckDB interroge le fichier sur disque.
# Retourne le nom de la vue creee.
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
hstat_duckdb_open_file <- function(path) {
  if (!hstat_has_duckdb()) stop("Le package 'duckdb' est requis.")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
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
    # %.0f et non %d : n reste < 2^31 mais la table, elle, peut depasser
    # 2 147 483 647 lignes (COUNT(*) revient en numeric) ; aucun passage
    # par as.integer() sur des comptages issus de DuckDB.
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
# Retourne une liste : data (data.frame de travail), mode ("memory"/"duckdb"),
# con (connexion DuckDB ou NULL), table, full_nrow, full_ncol, full_na,
hstat_load_data <- function(path, kind, header = TRUE, sep = ",",
                            sheet = 1,
                            threshold = HSTAT_BIGDATA_THRESHOLD,
                            sample_size = HSTAT_SAMPLE_SIZE) {
  size <- hstat_file_size(path)

  # --- Formats toujours charges en memoire ---------------------------------
  if (kind == "excel") {
    df <- as.data.frame(readxl::read_excel(path = path, sheet = sheet %||% 1))
  } else if (kind == "sav") {
    df <- as.data.frame(haven::read_sav(path))
  } else if (kind == "dta") {
    df <- as.data.frame(haven::read_dta(path))
  } else if (kind == "rds") {
    df <- as.data.frame(readRDS(path))

  # --- CSV : memoire si petit, DuckDB si volumineux ------------------------
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

  # --- Parquet : DuckDB (materialise si petit, vue si volumineux) ----------
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

  # --- DuckDB natif --------------------------------------------------------
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

  # --- Sortie mode memoire -------------------------------------------------
  # Assainissement final : garantit des chaines UTF-8 valides quel que soit le
  # format (Excel/SPSS/Stata/RDS peuvent aussi contenir du non-UTF-8), ce qui
  # evite les erreurs "invalid multibyte string" en aval (nchar, affichage...).
  df <- hstat_df_to_utf8(df)
  list(data = df, mode = "memory", con = NULL, table = NULL,
       full_nrow = nrow(df), full_ncol = ncol(df),
       full_na = sum(is.na(df)), is_sampled = FALSE,
       kind = kind, size = size)
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
# Sortie identique a make_summ_global : Facteurs, Variable, <stats...>
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
# Sortie identique a make_summ_grouped : <group_vars...>, Variable, <stats...>
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

# -- Comptage exact de lignes apres filtre SQL (optionnel) -------------------
hstat_duckdb_count <- function(con, tbl, where = NULL) {
  sql <- sprintf("SELECT COUNT(*) AS n FROM %s", hstat_sql_ident(tbl))
  if (!is.null(where) && nzchar(where)) sql <- paste0(sql, " WHERE ", where)
  as.numeric(DBI::dbGetQuery(con, sql)$n[1])
}



# Graine par defaut de l'application (modifiable par l'utilisateur dans l'UI).
HSTAT_DEFAULT_SEED <- 123L

# Applique une graine de maniere sure juste avant une operation aleatoire.
# 'seed' provient generalement de input$globalSeed ; si NULL/NA, on retombe
# sur la graine par defaut afin de garantir un comportement reproductible.
hstat_set_seed <- function(seed = NULL) {
  s <- suppressWarnings(as.integer(seed))
  if (length(s) == 0 || is.na(s)) s <- HSTAT_DEFAULT_SEED
  set.seed(s)
  invisible(s)
}


#  Cache memoire pour les agregations DuckDB (evite de relancer une requete
#  SQL identique sur un tres gros fichier). Cache simple cle -> valeur, vide
#  a chaque nouveau chargement de donnees via hstat_cache_clear().

.hstat_cache <- new.env(parent = emptyenv())

# Vide le cache (a appeler au chargement d'un nouveau fichier).
hstat_cache_clear <- function() {
  rm(list = ls(.hstat_cache, all.names = TRUE), envir = .hstat_cache)
  invisible(NULL)
}

# Memoise le resultat de 'fn()' sous une cle. Si la cle existe deja, renvoie
# la valeur en cache sans relancer le calcul.
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
  paste(parts, collapse = "::")
}

# Protege les noms de colonnes contenant des caracteres speciaux dans une formule
# de calcul, et convertit mean()/sum() en rowMeans()/rowSums() sur plusieurs
# colonnes. Defini globalement (utilise par mod_clean et par le serveur).
auto_quote_colnames <- function(formula_str, col_names) {
  cols_sorted <- col_names[order(nchar(col_names), decreasing = TRUE)]
  for (col in cols_sorted) {
    if (grepl("[-/ +*^()%$@!?]|^[0-9]", col, perl = TRUE)) {
      backtick_col <- paste0("`", col, "`")
      if (!grepl(backtick_col, formula_str, fixed = TRUE)) {
        formula_str <- gsub(col, backtick_col, formula_str, fixed = TRUE)
      }
    }
  }
  formula_str <- gsub("\\bmean\\s*\\(\\s*c\\s*\\(([^)]+)\\)\\s*\\)",
    "rowMeans(cbind(\\1), na.rm=TRUE)", formula_str, perl = TRUE)
  formula_str <- gsub("\\bmean\\s*\\(([^)]+,[^)]+)\\)",
    "rowMeans(cbind(\\1), na.rm=TRUE)", formula_str, perl = TRUE)
  formula_str <- gsub("\\bsum\\s*\\(\\s*c\\s*\\(([^)]+)\\)\\s*\\)",
    "rowSums(cbind(\\1), na.rm=TRUE)", formula_str, perl = TRUE)
  formula_str <- gsub("\\bsum\\s*\\(([^)]+,[^)]+)\\)",
    "rowSums(cbind(\\1), na.rm=TRUE)", formula_str, perl = TRUE)
  formula_str
}

# -- Evaluation SECURISEE des formules du calculateur de variables ------------
# Empeche l'execution de code arbitraire via le champ formule (system(),
# file.remove(), source(), install.packages(), etc.) :
#   1) l'expression est analysee (AST) : seules les fonctions de la liste
#      blanche HSTAT_FORMULA_FUNS sont autorisees ;
#   2) l'evaluation se fait dans un environnement clos (parent = emptyenv())
#      ne contenant que les colonnes du jeu de donnees et ces fonctions.
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

# Evalue une formule validee sur un data.frame, sans acces au reste de R.
hstat_safe_eval <- function(formula_str, data) {
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


# ==============================================================================
#  Modelisation predictive (series temporelles / ML / DL) : infrastructure
# ==============================================================================

# Plafond de lignes pour l'entrainement des modeles ML/DL (certains algorithmes
# comme SVM ou randomForest deviennent impraticables au-dela). Configurable via
# HSTAT_ML_MAX_N. Les predictions, elles, ne sont jamais plafonnees.
HSTAT_ML_MAX_N <- {
  v <- suppressWarnings(as.integer(Sys.getenv("HSTAT_ML_MAX_N", "200000")))
  if (!is.finite(v) || v < 1000) 200000L else v
}

# -- Interpretation automatique d'une valeur de R2 ----------------------------
.hstat_interp_r2 <- function(r2) {
  if (!is.finite(r2)) return("Non calculable sur ces donnees.")
  if (r2 >= 0.9) "Excellent : le modele explique la quasi-totalite de la variance."
  else if (r2 >= 0.7) "Bon : le modele capture l'essentiel de la structure des donnees."
  else if (r2 >= 0.5) "Moyen : pouvoir explicatif reel mais une part importante reste inexpliquee."
  else if (r2 >= 0.3) "Faible : le modele n'explique qu'une part limitee de la variance."
  else "Tres faible : le modele explique peu ; revoir les variables ou le type de modele."
}

.hstat_interp_mape <- function(m) {
  if (!is.finite(m)) return("MAPE non calculable (valeurs observees nulles).")
  if (m < 10) "Excellente precision (erreur relative moyenne < 10 %)."
  else if (m < 20) "Bonne precision (erreur relative moyenne < 20 %)."
  else if (m < 50) "Precision moyenne : previsions indicatives."
  else "Precision faible : previsions peu fiables en l'etat."
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
      "Pas de seuil universel : a comparer a l'ecart-type de la cible (excellent si RMSE << sigma) et entre modeles",
      "Pas de seuil universel : exprimee dans l'unite de la cible ; plus petite = meilleure",
      "< 10 % excellent ; 10-20 % bon ; 20-50 % moyen ; > 50 % faible",
      "< 0,3 tres faible ; 0,3-0,5 faible ; 0,5-0,7 moyen ; 0,7-0,9 bon ; >= 0,9 excellent"),
    Interpretation = c(
      trf("Erreur quadratique moyenne : %s unite(s) de la variable cible (ecart-type observe : %s). Penalise fortement les grosses erreurs.",
              format(round(rmse, 3), big.mark = " "), format(round(sc, 3), big.mark = " ")),
      trf("En moyenne, la prediction s'ecarte de %s unite(s) de la valeur reelle.",
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

# -- Metriques de CLASSIFICATION avec interpretation ----------------------------
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
    Metrique = c("Exactitude (accuracy)", "Kappa de Cohen", "Precision (macro)",
                 "Rappel / sensibilite (macro)", "F1-score (macro)", "AUC (ROC)"),
    Valeur   = round(vals, 4),
    Seuils = c(
      "A comparer a la part de la classe majoritaire : nettement au-dessus = modele informatif",
      "< 0,2 negligeable ; 0,2-0,4 faible ; 0,4-0,6 modere ; 0,6-0,8 substantiel ; > 0,8 quasi parfait (Landis & Koch)",
      ">= 0,9 excellente ; 0,7-0,9 bonne ; 0,5-0,7 moyenne ; < 0,5 faible",
      ">= 0,9 excellent ; 0,7-0,9 bon ; 0,5-0,7 moyen ; < 0,5 faible",
      ">= 0,9 excellent ; 0,8-0,9 bon ; 0,6-0,8 moyen ; < 0,6 faible",
      "0,5 = hasard ; 0,7-0,8 acceptable ; 0,8-0,9 bonne ; >= 0,9 excellente discrimination"),
    Interpretation = c(
      trf("%.1f %% des observations sont bien classees (classe majoritaire seule : %.1f %% ; le modele %s).",
              100 * acc, 100 * maj,
              if (is.finite(acc) && acc > maj) "fait mieux que ce niveau de reference"
              else "ne depasse pas ce niveau de reference"),
      if (!is.finite(kap)) "Non calculable." else if (kap >= 0.8) "Accord quasi parfait au-dela du hasard."
      else if (kap >= 0.6) "Accord substantiel au-dela du hasard."
      else if (kap >= 0.4) "Accord modere au-dela du hasard."
      else if (kap >= 0.2) "Accord faible : a peine mieux que le hasard."
      else "Accord negligeable : equivalent au hasard.",
      "Parmi les predictions d'une classe, part reellement correcte (moyenne des classes).",
      "Parmi les cas reels d'une classe, part correctement retrouvee (moyenne des classes).",
      "Compromis precision/rappel (1 = parfait). Robuste aux classes desequilibrees.",
      if (!is.finite(auc)) "AUC calculee uniquement en classification binaire (avec probabilites)."
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
    "Le modele %s a ete entraine sur %s observation(s) puis evalue sur %s observation(s) de test jamais vues pendant l'entrainement : les metriques ci-dessus refletent donc sa capacite de generalisation, pas sa memoire.",
    model_label, format(n_train, big.mark = " "), format(n_test, big.mark = " "))
  core <- if (identical(task, "regression")) {
    r2 <- get_v("R2"); mape <- get_v("MAPE (%)")
    paste0(.hstat_interp_r2(r2), " ", .hstat_interp_mape(mape))
  } else {
    acc <- get_v("Exactitude (accuracy)"); f1 <- get_v("F1-score (macro)")
    sprintf("Avec %.1f %% de bonnes classifications et un F1 macro de %.2f, %s",
            100 * acc, f1,
            if (is.finite(f1) && f1 >= 0.8) "le modele est operationnel pour la prediction."
            else if (is.finite(f1) && f1 >= 0.6) "le modele est utilisable mais perfectible (plus de donnees, autres variables, reglages)."
            else "le modele n'est pas encore fiable : enrichir les variables ou changer d'algorithme.")
  }
  paste(c(head_txt, core, notes), collapse = " ")
}

# ==============================================================================
#  Export universel de graphiques : PNG/JPG/TIFF/BMP/PDF/SVG, DPI jusqu'a 20 000
# ==============================================================================

# Bloc UI reutilisable : format, dimensions, DPI (max 20 000) + bouton.
hstat_export_plot_ui <- function(ns, prefix, width = 10, height = 6) {
  tagList(
    fluidRow(
      column(3, selectInput(ns(paste0(prefix, "Fmt")), "Format",
               choices = c("PNG" = "png", "JPG" = "jpeg", "TIFF" = "tiff",
                           "BMP" = "bmp", "PDF (vectoriel)" = "pdf",
                           "SVG (vectoriel)" = "svg"), selected = "png")),
      column(3, numericInput(ns(paste0(prefix, "W")), "Largeur (pouces)",
                             value = width, min = 3, max = 30, step = 0.5)),
      column(3, numericInput(ns(paste0(prefix, "H")), "Hauteur (pouces)",
                             value = height, min = 3, max = 30, step = 0.5)),
      column(3, numericInput(ns(paste0(prefix, "Dpi")), "DPI (max 20 000)",
                             value = 300, min = 72, max = 20000, step = 50))),
    tags$small(style = "color:#6b7280;",
      "PDF et SVG sont vectoriels (resolution infinie, DPI sans objet). ",
      "Pour les formats matriciels, au-dela d'un certain DPI les dimensions physiques ",
      "sont automatiquement reduites afin de garder une image ouvrable (plafond de securite en pixels)."),
    div(style = "margin-top:8px;",
        downloadButton(ns(paste0(prefix, "Dl")), "Télécharger le graphique",
                       class = "btn-success"))
  )
}

# Handler d'export associe. plot_fun() doit renvoyer un ggplot (ou NULL).
# Garde-fou : plafonne chaque cote a 16 000 px (une image 16 000 x 16 000 en
# RGBA occupe deja ~1 Go en memoire de trace) en reduisant la taille physique,
# jamais le DPI demande (le fichier conserve la metadonnee DPI voulue).
hstat_export_plot_handler <- function(input, prefix, plot_fun, fname = "graphique") {
  downloadHandler(
    filename = function() {
      fmt <- input[[paste0(prefix, "Fmt")]] %||% "png"
      ext <- if (identical(fmt, "jpeg")) "jpg" else fmt
      paste0(fname, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      g <- plot_fun()
      if (is.null(g)) stop("Aucun graphique a exporter : lancez d'abord l'analyse.")
      fmt <- input[[paste0(prefix, "Fmt")]] %||% "png"
      w   <- hstat_finite(input[[paste0(prefix, "W")]], 10);  w <- max(3, min(30, w))
      h   <- hstat_finite(input[[paste0(prefix, "H")]], 6);   h <- max(3, min(30, h))
      dpi <- hstat_finite(input[[paste0(prefix, "Dpi")]], 300)
      dpi <- max(72, min(20000, dpi))
      if (fmt %in% c("pdf", "svg")) {
        dev <- if (identical(fmt, "pdf")) grDevices::cairo_pdf else "svg"
        ggplot2::ggsave(file, g, width = w, height = h, device = dev, limitsize = FALSE)
      } else {
        max_px <- 16000
        scale  <- min(1, max_px / (w * dpi), max_px / (h * dpi))
        args <- list(filename = file, plot = g, width = w * scale,
                     height = h * scale, dpi = dpi, device = fmt, limitsize = FALSE)
        if (identical(fmt, "tiff")) args$compression <- "lzw"
        if (identical(fmt, "jpeg")) args$quality <- 95
        do.call(ggplot2::ggsave, args)
      }
    })
}

# NB : hstat_finite() est defini plus haut dans ce fichier.

# ==============================================================================
#  Personnalisation d'apparence reutilisable pour les graphiques de modelisation
# ==============================================================================

hstat_plot_opts_ui <- function(ns, prefix) {
  tagList(
    fluidRow(
      column(6, textInput(ns(paste0(prefix, "Title")), "Titre", value = "")),
      column(6, textInput(ns(paste0(prefix, "Sub")), "Sous-titre", value = ""))),
    fluidRow(
      column(6, textInput(ns(paste0(prefix, "Xlab")), "Titre de l'axe X", value = "")),
      column(6, textInput(ns(paste0(prefix, "Ylab")), "Titre de l'axe Y", value = ""))),
    fluidRow(
      column(4, selectInput(ns(paste0(prefix, "Theme")), "Thème",
               choices = c("Minimal" = "minimal", "Classique" = "classic",
                           "Noir & blanc" = "bw", "Clair" = "light",
                           "Sombre" = "dark"), selected = "minimal")),
      column(4, numericInput(ns(paste0(prefix, "Base")), "Taille du texte",
                             value = 13, min = 7, max = 30, step = 1)),
      column(4, selectInput(ns(paste0(prefix, "Legend")), "Légende",
               choices = c("Droite" = "right", "Gauche" = "left", "Haut" = "top",
                           "Bas" = "bottom", "Masquée" = "none"),
               selected = "right"))),
    fluidRow(
      column(4, if (requireNamespace("colourpicker", quietly = TRUE))
                  colourpicker::colourInput(ns(paste0(prefix, "Col")),
                    "Couleur principale", value = "#2c7fb8")
                else textInput(ns(paste0(prefix, "Col")),
                    "Couleur principale (hex)", value = "#2c7fb8")),
      column(4, numericInput(ns(paste0(prefix, "Lwd")), "Épaisseur des lignes",
                             value = 0.9, min = 0.2, max = 4, step = 0.1)),
      column(4, numericInput(ns(paste0(prefix, "Rot")), "Rotation des labels X (°)",
                             value = 0, min = 0, max = 90, step = 15)))
  )
}

# Applique les options ci-dessus a un ggplot. La couleur/epaisseur sont lues par
# les fonctions de trace via hstat_plot_opt(input, prefix, "Col"/"Lwd").
hstat_apply_plot_opts <- function(g, input, prefix) {
  if (is.null(g)) return(g)
  th <- switch(input[[paste0(prefix, "Theme")]] %||% "minimal",
               classic = ggplot2::theme_classic, bw = ggplot2::theme_bw,
               light = ggplot2::theme_light, dark = ggplot2::theme_dark,
               ggplot2::theme_minimal)
  base <- hstat_finite(input[[paste0(prefix, "Base")]], 13)
  g <- g + th(base_size = max(7, min(30, base)))
  lab <- function(x) { x <- input[[paste0(prefix, x)]]; if (is.null(x) || !nzchar(x)) NULL else x }
  if (!is.null(lab("Title"))) g <- g + ggplot2::labs(title = lab("Title"))
  if (!is.null(lab("Sub")))   g <- g + ggplot2::labs(subtitle = lab("Sub"))
  if (!is.null(lab("Xlab")))  g <- g + ggplot2::labs(x = lab("Xlab"))
  if (!is.null(lab("Ylab")))  g <- g + ggplot2::labs(y = lab("Ylab"))
  rot <- hstat_finite(input[[paste0(prefix, "Rot")]], 0)
  g + ggplot2::theme(
    legend.position = input[[paste0(prefix, "Legend")]] %||% "right",
    axis.text.x = ggplot2::element_text(angle = rot,
                                        hjust = if (rot > 0) 1 else 0.5))
}

hstat_plot_opt <- function(input, prefix, what, default) {
  v <- input[[paste0(prefix, what)]]
  if (is.null(v) || (is.character(v) && !nzchar(v))) default else v
}

# ==============================================================================
#  Export de tableaux (CSV / Excel) et simulateur de predictions
# ==============================================================================

hstat_export_table_ui <- function(ns, prefix) {
  div(style = "margin-top:6px;",
      downloadButton(ns(paste0(prefix, "Csv")), "CSV", class = "btn-sm"),
      downloadButton(ns(paste0(prefix, "Xlsx")), "Excel", class = "btn-sm"))
}

hstat_export_table_handlers <- function(output, prefix, data_fun, fname = "resultats") {
  output[[paste0(prefix, "Csv")]] <- downloadHandler(
    filename = function() paste0(fname, "_", Sys.Date(), ".csv"),
    content  = function(file) {
      d <- data_fun(); if (is.null(d)) stop("Aucun resultat a exporter.")
      utils::write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  output[[paste0(prefix, "Xlsx")]] <- downloadHandler(
    filename = function() paste0(fname, "_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      d <- data_fun(); if (is.null(d)) stop("Aucun resultat a exporter.")
      writexl::write_xlsx(as.data.frame(d), file)
    })
}

# Formulaire dynamique : un champ par predicteur (numerique -> valeur mediane,
# facteur/caractere -> liste des modalites observees).
hstat_sim_inputs_ui <- function(ns, df, vars, prefix) {
  if (is.null(df) || length(vars) == 0) return(NULL)
  ctrls <- lapply(vars, function(v) {
    x <- df[[v]]
    if (is.numeric(x)) {
      md <- suppressWarnings(stats::median(x, na.rm = TRUE))
      numericInput(ns(paste0(prefix, "_", v)), v,
                   value = round(hstat_finite(md, 0), 4))
    } else {
      lv <- sort(unique(as.character(x[!is.na(x)])))
      if (length(lv) == 0) lv <- ""
      selectInput(ns(paste0(prefix, "_", v)), v, choices = lv)
    }
  })
  do.call(tagList, lapply(seq_along(ctrls), function(i)
    column(4, ctrls[[i]])))
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
# niveaux de facteurs du jeu d'entrainement. Retourne list(data, warn).
hstat_align_newdata <- function(newdf, ref, vars) {
  miss <- setdiff(vars, names(newdf))
  if (length(miss) > 0)
    return(list(data = NULL,
                warn = paste0("Colonnes manquantes dans le fichier importe : ",
                              paste(miss, collapse = ", "))))
  out <- newdf[, vars, drop = FALSE]
  warns <- character(0)
  for (v in vars) {
    if (is.numeric(ref[[v]])) {
      out[[v]] <- suppressWarnings(as.numeric(out[[v]]))
    } else {
      lv <- levels(factor(ref[[v]]))
      bad <- setdiff(unique(as.character(out[[v]])), c(lv, NA))
      if (length(bad) > 0)
        warns <- c(warns, paste0(v, " : modalites inconnues ignorees (",
                                 paste(utils::head(bad, 5), collapse = ", "), ")"))
      out[[v]] <- factor(as.character(out[[v]]), levels = lv)
    }
  }
  list(data = out, warn = if (length(warns)) paste(warns, collapse = " ; ") else NULL)
}


# ==============================================================================
#  Fiches des modeles : principe, objectif, conditions d'application
# ==============================================================================

hstat_model_doc <- function(id) {
  d <- list(
    # ---- Series temporelles ----
    naive  = c("Naif (derniere valeur)",
      "Chaque prevision reprend la derniere valeur observee.",
      "Servir de niveau de reference : tout modele utile doit faire mieux.",
      "Aucune ; pertinent quand la serie est une marche aleatoire sans tendance ni saison."),
    snaive = c("Naif saisonnier",
      "Chaque prevision reprend la valeur observee a la meme periode de la saison precedente.",
      "Reference pour les series saisonnieres.",
      "Frequence saisonniere > 1 et au moins 2 saisons completes."),
    meanf  = c("Moyenne historique",
      "Toutes les previsions valent la moyenne de la serie.",
      "Reference pour les series stationnaires autour d'un niveau.",
      "Serie sans tendance ni saisonnalite marquees."),
    drift  = c("Marche aleatoire avec derive",
      "Prolonge la droite reliant la premiere et la derniere observation.",
      "Capturer une tendance moyenne simple.",
      "Tendance approximativement lineaire ; pas de saisonnalite."),
    ses    = c("Lissage exponentiel simple (SES)",
      "Moyenne ponderee des observations, avec des poids decroissant exponentiellement vers le passe.",
      "Prevoir le niveau d'une serie sans tendance ni saison.",
      "Serie stationnaire en tendance et en saison ; au moins ~10 observations."),
    holt   = c("Holt (tendance)",
      "Etend le SES avec une composante de tendance lissee.",
      "Prevoir une serie avec tendance persistante.",
      "Tendance approximativement lineaire ; pas de saisonnalite ; attention aux horizons longs (tendance extrapolee sans fin)."),
    holtd  = c("Holt amorti",
      "Comme Holt, mais la tendance s'attenue progressivement vers un plateau.",
      "Previsions long terme plus prudentes que Holt.",
      "Tendance qui a des raisons de ralentir ; pas de saisonnalite."),
    hwadd  = c("Holt-Winters additif",
      "Lissage du niveau, de la tendance et d'une saisonnalite d'amplitude constante.",
      "Prevoir une serie saisonniere dont les oscillations gardent la meme ampleur.",
      "Frequence > 1, au moins 2 saisons completes, amplitude saisonniere stable."),
    hwmul  = c("Holt-Winters multiplicatif",
      "Comme l'additif, mais la saisonnalite est proportionnelle au niveau.",
      "Prevoir une serie dont les oscillations grandissent avec le niveau.",
      "Frequence > 1, 2 saisons completes, valeurs strictement positives."),
    ets    = c("ETS (selection automatique)",
      "Famille des lissages exponentiels ; la meilleure combinaison Erreur/Tendance/Saison est choisie par vraisemblance (AICc).",
      "Obtenir automatiquement le meilleur lissage exponentiel sans reglage manuel.",
      "Au moins ~16 observations ; saisonnalites tres longues (> 24) mal gerees (prendre TBATS)."),
    arima  = c("ARIMA automatique",
      "Modelise la serie par ses propres retards (AR), la differenciation (I) et les erreurs passees (MA) ; les ordres sont choisis automatiquement.",
      "Capturer l'autocorrelation, la tendance stochastique et la saisonnalite.",
      "Serie rendue stationnaire par differenciation ; residus a verifier (Ljung-Box) ; au moins ~30 observations."),
    sarima = c("SARIMA manuel",
      "ARIMA avec ordres saisonniers (P,D,Q) fixes par l'utilisateur.",
      "Controler finement la structure quand l'automatique ne convient pas.",
      "Bien lire l'ACF/PACF des residus pour choisir les ordres ; frequence > 1 pour la partie saisonniere."),
    tbats  = c("TBATS",
      "Combinaison de transformations Box-Cox, tendance amortie, erreurs ARMA et saisonnalites trigonometriques multiples.",
      "Series a saisonnalites complexes ou multiples (ex. journaliere + hebdomadaire).",
      "Series suffisamment longues ; calcul plus lent que ETS/ARIMA."),
    theta  = c("Methode Theta",
      "Decompose la serie en droites de courbure modifiee puis les recombine (equivalent a un SES avec derive).",
      "Prevision robuste et rapide, tres performante dans les competitions (M3).",
      "Serie de preference desaisonnalisee ou peu saisonniere ; peu de reglages."),
    stlf   = c("STL + ETS",
      "Decompose la serie (tendance / saison / reste) par regression locale STL puis prevoit le reste par ETS et rajoute la saison.",
      "Series saisonnieres au motif stable, avec robustesse aux valeurs atypiques.",
      "Frequence > 1 et au moins 2 saisons completes."),
    nnetar = c("NNAR (reseau de neurones autoregressif)",
      "Perceptron a une couche cachee nourri par les retards de la serie (et les retards saisonniers).",
      "Capturer des dynamiques non lineaires ignorees par ARIMA/ETS.",
      "Series assez longues ; pas d'intervalles de prevision analytiques ; risque de surapprentissage sur series courtes."),
    dlmts  = c("DLM (modele lineaire dynamique)",
      "Modele espace d'etats gaussien (niveau, tendance, saison) dont les composantes evoluent dans le temps ; estimation par maximum de vraisemblance et filtre de Kalman.",
      "Suivre des composantes qui changent au fil du temps et fournir une incertitude coherente.",
      "Serie approximativement gaussienne ; au moins ~30 observations ; l'optimisation peut echouer sur series tres courtes."),
    dlnm   = c("DLNM (retards distribues non lineaires)",
      "Regression ou la reponse depend d'une exposition via une surface exposition-retard (splines croisees) : l'effet peut etre non lineaire ET etale dans le temps.",
      "Quantifier et exploiter l'effet retarde d'une exposition (temperature, pollution...) sur un resultat, usage classique en epidemiologie environnementale.",
      "Choisir une variable d'exposition et un decalage maximal ; serie nettement plus longue que le decalage ; famille quasi-Poisson automatique pour les comptages ; les previsions exigent des expositions futures."),
    prophet = c("Prophet",
      "Modele additif decomposable : tendance par morceaux + saisonnalites de Fourier + jours feries, estime par optimisation bayesienne approchee.",
      "Series d'activite (journalieres/hebdomadaires) avec changements de tendance et effets calendaires.",
      "Necessite une colonne de dates ; au moins plusieurs mois d'historique ; moins adapte aux series tres courtes ou hautement autocorrelees."),
    # ---- Machine learning supervise ----
    lmglm = c("Modele lineaire / logistique",
      "Combinaison lineaire des variables ; en classification, la probabilite passe par une fonction logistique.",
      "Reference interpretable : effets marginaux lisibles (coefficients).",
      "Relations approximativement lineaires ; peu de colinearite ; residus homoscedastiques (regression)."),
    glmnet = c("Ridge / Lasso / Elastic-Net",
      "Modele lineaire penalise : la penalite retrecit les coefficients (Ridge) ou en annule (Lasso), dosee par validation croisee.",
      "Stabiliser le modele avec beaucoup de variables correlees et selectionner les plus utiles.",
      "Variables standardisees en interne ; efficace quand p est grand ou les predicteurs correles."),
    rpart = c("Arbre de decision",
      "Partitionne recursivement les donnees par des regles de seuil maximisant la purete des feuilles.",
      "Regles de decision lisibles et non lineaires.",
      "Sensible aux petites variations des donnees ; elaguer (cp) pour eviter le surapprentissage."),
    rf = c("Foret aleatoire",
      "Moyenne de centaines d'arbres construits sur des echantillons bootstrap et des sous-ensembles de variables.",
      "Excellente performance par defaut, robuste au bruit, importance des variables.",
      "Peu d'hypotheses ; couteuse sur tres gros volumes ; extrapole mal hors du domaine observe."),
    xgb = c("Gradient boosting (xgboost)",
      "Ajoute sequentiellement de petits arbres corrigeant les erreurs residuelles des precedents.",
      "Etat de l'art sur donnees tabulaires quand il est bien regle.",
      "Sensible aux hyperparametres (profondeur, taux d'apprentissage, iterations) ; activer la recherche automatique."),
    svm = c("SVM (machine a vecteurs de support)",
      "Cherche la frontiere de marge maximale, rendue non lineaire par un noyau (radial, polynomial...).",
      "Frontieres complexes sur echantillons petits a moyens.",
      "Variables a standardiser (fait en interne par e1071) ; cout eleve au-dela de ~10 000 lignes ; regler C (et le noyau)."),
    knn = c("k plus proches voisins",
      "Predit par vote (ou moyenne) des k observations les plus proches.",
      "Methode locale sans hypothese de forme.",
      "Sensible a l'echelle des variables et a la dimension ; choisir k (impair en binaire) ; couteux en prediction sur gros volumes."),
    nb = c("Naive Bayes",
      "Applique la regle de Bayes en supposant les variables independantes conditionnellement a la classe.",
      "Classifieur rapide et etonnamment efficace, notamment sur variables categorielles.",
      "Classification uniquement ; l'hypothese d'independance doit rester raisonnable."),
    nnet = c("Reseau de neurones (1 couche)",
      "Perceptron a une couche cachee : combinaisons non lineaires apprises des variables.",
      "Capturer des interactions non lineaires simples.",
      "Variables standardisees (fait en interne) ; regler taille et decay ; risque de minima locaux."),
    # ---- Clustering ----
    kmeans = c("k-means",
      "Alterne affectation de chaque point au centre le plus proche et recalcul des centres.",
      "Partitionner rapidement en k groupes compacts.",
      "k fixe a l'avance ; groupes spheriques de tailles comparables ; standardiser les variables."),
    hclust = c("Classification hierarchique (CAH)",
      "Fusionne progressivement les paires de groupes les plus proches (Ward) en un dendrogramme.",
      "Explorer la structure a plusieurs niveaux de regroupement.",
      "Matrice de distances en O(n^2) : reserver aux effectifs moderes ; standardiser."),
    pam = c("PAM (k-medoides)",
      "Comme k-means mais les centres sont des observations reelles (medoides), avec une distance quelconque.",
      "Clustering robuste aux valeurs atypiques.",
      "Plus couteux que k-means ; k fixe a l'avance."),
    dbscan = c("DBSCAN",
      "Regroupe les points densement connectes ; les points isoles deviennent du bruit.",
      "Trouver des groupes de forme quelconque sans fixer k, et isoler les anomalies.",
      "Regler eps et minPts (sensibles) ; difficile si les densites varient beaucoup ; standardiser."),
    mclust = c("Melanges gaussiens (mclust)",
      "Modele probabiliste : les donnees proviennent d'un melange de lois normales estime par EM ; choix du modele par BIC.",
      "Clustering souple (appartenance probabiliste) et formes elliptiques.",
      "Hypothese de normalite par composante ; effectifs suffisants par groupe."),
    # ---- Deep learning ----
    dl_neuralnet = c("MLP (neuralnet)",
      "Perceptron multi-couches entraine par retropropagation resiliente (rprop), 100 % R.",
      "Reseau profond simple, disponible sans aucune installation supplementaire.",
      "Predicteurs standardises (fait automatiquement) ; peut ne pas converger : reduire les couches ou augmenter les iterations."),
    dl_torch = c("MLP (torch)",
      "Perceptron multi-couches (ReLU) entraine par Adam et mini-lots via libtorch, avec courbe de perte par epoque.",
      "Architectures plus profondes, controle fin (epoques, taux d'apprentissage, lots).",
      "Bibliotheques natives a telecharger une fois (~600 Mo, bouton dedie) ; surveiller la courbe de perte (surapprentissage)."),
    lstm = c("LSTM (torch)",
      "Reseau recurrent a memoire longue : apprend a predire chaque valeur a partir d'une fenetre glissante du passe.",
      "Prevision de sequences aux dependances longues et non lineaires.",
      "Series longues (>> fenetre) ; previsions futures recursives dont l'incertitude croit avec l'horizon ; torch requis.")
  )
  x <- d[[id]]
  if (is.null(x)) return(NULL)
  list(nom = x[1], principe = x[2], objectif = x[3], conditions = x[4])
}

# Bloc UI pret a l'emploi pour afficher la fiche d'un modele.
hstat_model_doc_ui <- function(id) {
  f <- hstat_model_doc(id)
  if (is.null(f)) return(NULL)
  div(class = "callout callout-info", style = "margin-top:8px;",
      tags$p(icon("book"), strong(sprintf(" Fiche du modele — %s", f$nom))),
      tags$p(strong("Principe : "), f$principe),
      tags$p(strong("Objectif : "), f$objectif),
      tags$p(strong("Conditions d'application : "), f$conditions))
}

# ---------------------------------------------------------------------------
# EXPORT D'IMAGE : les pixels saisis sont une MISE EN PAGE, pas la sortie
# ---------------------------------------------------------------------------
# `ggsave` raisonne en pouces : le fichier fait pouces x DPI. Diviser les pixels
# demandes par le DPI (1200 / 300 = 4 pouces) rendait bien 1200 px de large,
# mais sur une toile de QUATRE pouces : onze etiquettes de traitement s'y
# ecrasent, et le texte, dimensionne en points, y occupe une place enorme.
# Pire, le defaut s'aggravait dans le sens ou l'utilisateur cherchait a
# l'eviter -- 1200 px a 600 DPI faisaient 2 pouces, donc demander plus de
# qualite retrecissait la figure et la rendait illisible. Constate a l'ecran.
#
# Les pixels saisis sont donc lus a la resolution de reference de l'ecran
# (96 ppp, la convention CSS) : ils fixent la MISE EN PAGE, celle que
# l'utilisateur voit. Le DPI multiplie ensuite la finesse du rendu. 1200 x 800
# a 300 DPI donne une figure de 12,5 x 8,33 pouces rendue en 3750 x 2500 px.
HSTAT_EXPORT_REF_DPI <- 96

# Borne de securite : au-dela, ggsave tente d'allouer un bitmap que la machine
# ne peut pas tenir et l'export echoue au lieu de rendre une image un peu moins
# fine. On abaisse le DPI et on le DIT -- un export silencieusement degrade
# serait pire que le refus.
HSTAT_EXPORT_MAX_PX <- 20000L

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

# Etiquette mise en forme pour plotly. Le graphique porte ses etiquettes en
# plotmath (bold(), italic()) : ggsave les rend, mais ggplotly les DEPARSE et
# l'axe affichait `bold("2SP(0,5)&2PV")` en toutes lettres. Signale a l'ecran.
# plotly comprend un sous-ensemble de HTML ; le texte est echappe avant d'y
# entrer, sinon un « & » ou un « < » dans un nom de traitement casserait la
# balise.
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
           s)
  }, character(1))
}

# Variables Y retenues pour un graphique multi-courbes. `pivot_longer` empile
# les Y dans UNE colonne : elles doivent donc partager un type. Sur un fichier
# de suivi, selectionner la date en X *et* en Y levait
# « Can't combine `Semaine` <date> and `ch_Hel` <double> » -- erreur qui
# tombait dans l'observateur et emportait tout le graphique. Signale a l'ecran.
#
# La variable d'axe X est ecartee (l'empiler comme mesure n'a pas de sens), et
# en cas de types melanges le quantitatif l'emporte : c'est ce qu'une courbe
# represente. Ce qui est ecarte est NOMME, jamais retire en silence.
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
