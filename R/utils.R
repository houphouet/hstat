# =============================================================================
#  HStat -- socle partage : fonctions de calcul et utilitaires
# -----------------------------------------------------------------------------
#  CE FICHIER NE FAIT QUE DEFINIR. Aucune expression de premier niveau n'agit
#  ici : ni `options()`, ni installation de paquet, ni definition conditionnelle.
#  La raison est structurelle -- dans un paquet R, le code de premier niveau est
#  evalue A L'INSTALLATION, pas au chargement. Un `if (!requireNamespace(...))`
#  place ici figerait sur la machine de construction une decision qui appartient
#  a la machine d'execution.
#
#  Ces effets de bord vivent donc dans `inst/app/Utils.R`, devenu un PONT : il
#  charge ce socle, puis fait ce qui doit l'etre au demarrage de l'application.
#
#  Consequence voulue : ce fichier est chargeable seul, par `pkgload::load_all()`
#  comme par `sys.source()`, et les tests n'ont plus besoin de demarrer quoi que
#  ce soit pour l'atteindre.
#
# =============================================================================

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


# --- Boutons d'export DT avec noms de fichiers propres -----------------------

.hstat_dt_buttons <- function(fname = "HStat_export") {
  fname <- gsub("[^A-Za-z0-9_.-]+", "_", as.character(fname))
  list(
    list(extend = "copy",  title = NULL),
    list(extend = "csv",   filename = fname, title = NULL),
    list(extend = "excel", filename = fname, title = NULL)
  )
}



install_and_load <- function(packages) {
  installed_packages <- rownames(utils::installed.packages())
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
    message("HStat : packages indisponibles (certaines fonctions seront limitées) : ",
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
  "prophet", "torch",     # inclus (installes automatiquement)
  # Analyses multivariees etendues. Ils etaient installes DEPUIS LE CORPS DU
  # SERVEUR, donc a chaque nouvelle session : sur un poste hors ligne, chaque
  # ouverture attendait l'expiration de la requete CRAN, et sur un serveur
  # partage, plusieurs sessions pouvaient ecrire en meme temps dans la
  # bibliotheque. L'installation appartient au demarrage, une fois pour toutes.
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
  # L'installation se fait a la demande, via un bouton dedie du module
  # Deep Learning (avec barre de progression), ou manuellement par
  # torch::install_torch(). Ici on se contente d'informer.
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

# --- Gardes pour packages d'UI optionnels -----------------------------------

.hstat_has <- function(pkg) isTRUE(requireNamespace(pkg, quietly = TRUE))

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
    sd_val <- stats::sd(x, na.rm = TRUE)
    !is.na(sd_val) && sd_val > 0
  })
  df[, keep, drop = FALSE]
}

safe_cor <- function(df, use = "pairwise.complete.obs") {
  if (is.null(df)) return(NULL)
  df <- df[, sapply(df, is.numeric), drop = FALSE]
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
    if (is.numeric(x)) return(length(unique(stats::na.omit(x))) <= max_numeric_levels)
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

HSTAT_LANGUES <- c("Français" = "fr", "English" = "en")

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
  # LA RECHERCHE SE FAIT SUR LA CHAINE ELAGUEE, et l'espacement d'origine est
  # rendu autour de la traduction.
  #
  # `hstat_i18n_load()` applique `trimws()` a ses cles -- une entree de CSV ne
  # doit pas dependre d'un blanc invisible. Consequence non voulue : toute
  # chaine BORDEE D'ESPACES devenait intraduisible, sans un mot. Trente-huit
  # gabarits etaient dans ce cas -- ce sont des fragments de phrase assembles
  # par `paste0`, ou l'espace separe deux morceaux et fait donc partie de la
  # mise en forme, jamais du texte. Ils restaient en francais au milieu d'une
  # interface anglaise, et le dictionnaire les contenait pourtant.
  #
  # L'espacement est de la PRESENTATION : on le retire pour chercher, on le
  # remet pour rendre.
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

# ---------------------------------------------------------------------------
# Retrait des accents : UNE seule definition
# ---------------------------------------------------------------------------
# Le meme `chartr` etait recopie sept fois (nettoyage de texte, tokenisation,
# noms de fichiers, recherche dans les memos, reconnaissance des familles
# d'analyse). Une copie oubliee, c'est un rapprochement qui echoue en silence :
# « energie » ne rejoint pas « energie », et le mot compte pour deux.
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
# Second nettoyage, meme famille : un attribut qu'un type de trace REFUSE.
# Une trace « bar » n'a pas de `mode` -- c'est un attribut des nuages de
# points. Quand la conversion en depose un, `plotly_build()` avertit puis le
# jette : le graphique est correct, mais l'avertissement revient a chaque
# rendu et finit par masquer ceux qui comptent. On retire donc l'attribut
# AVANT la construction, ce qui supprime la cause au lieu de taire le
# symptome -- un `suppressWarnings()` global etoufferait aussi les vrais.
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

hstat_plotly_clean <- function(p) {
  if (is.null(p)) return(p)
  p <- tryCatch(.hstat_plotly_attrs(p), error = function(e) p)
  b <- tryCatch(plotly::plotly_build(p), error = function(e) NULL)
  if (is.null(b)) return(p)
  if (!is.null(b$dependencies))
    b$dependencies <- Filter(function(d) !identical(d$name, "typedarray"),
                             b$dependencies)
  b
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
          tr("L'analyse a échoué. Message renvoyé par R (non traduit)", lang),
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
         trf("Le test %s montre un résultat %s (p = %s)", test_type, significance, round(p_value, 8))
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
# Les valeurs par defaut de ggplot2 lui-meme. Un graphique doit s'afficher
# D'ABORD tel que ggplot2 le dessine : c'est la reference que tout le monde
# connait, celle des manuels et des exemples. Les reglages de l'application
# partent donc de la, et l'utilisateur s'en ecarte s'il le souhaite -- l'inverse
# (des tailles maison imposees d'entree) oblige a defaire avant de faire.
HSTAT_GG_POINT_SIZE <- 1.5    # geom_point(size = )
HSTAT_GG_LINEWIDTH  <- 0.5    # geom_line/segment(linewidth = )
HSTAT_GG_BASE_SIZE  <- 11     # theme_grey(base_size = ), en points
HSTAT_GG_LABEL_PT   <- 11     # geom_text(size = 3.88 mm) ~ 11 pt

# Le minimum descend SOUS la taille de ggplot2. Un curseur qui commence au
# defaut ne permet que d'agrandir, or c'est le plus souvent l'inverse qu'il
# faut : sur un nuage de plusieurs dizaines d'individus, les etiquettes se
# recouvrent a 11 pt et il n'y avait aucun moyen de les reduire. 8 pt reste
# lisible sur une figure exportee a 300 DPI.
#
# Le DEFAUT, lui, ne bouge pas : c'est toujours celui de ggplot2, l'etat
# d'origine qu'on doit pouvoir retrouver sans le chercher.
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
    return(trf("La distribution est normale (p = %s >= 0.05)", round(p_value, 8)))
  } else {
    return(trf("La distribution n'est pas normale (p = %s < 0.05)", round(p_value, 8)))
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

calc_cv <- function(x) stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100



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
                     x_trans[!is.na(x)] <- stats::predict(yj_obj, newdata = x_nona)
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

# Styles d'ecriture proposes partout ou un texte de graphique se met en forme.
# Declares une fois : la meme liste etait recopiee treize fois dans le panneau
# d'options, et une correction n'en touchait qu'une.
# Carte de reglages du panneau « Options du graphique ». Un panneau de quarante
# controles gris se lit mal : chaque famille porte donc sa couleur, la meme sur
# le liseré, l'icone et le titre. La teinte de fond reste tres pale -- ce sont
# des reglages, pas des alertes.
# Reglages de forme communs aux analyses multivariees historiques (ACP, HCPC,
# AFD). Elles avaient deja titre et libelles d'axes ; le theme, le sous-titre,
# la position de la legende et la grille manquaient a toutes.
hstat_mv_forme_ui <- function(prefix, titre = "Apparence du graphique") {
  .hstat_opt_section(
    titre, "brush", "#8e44ad", "#f7f0fb",
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(paste0(prefix, "_theme"), "Thème",
        choices = c("Par défaut (ggplot2)" = "gg",
                    "HStat (minimal)" = "hstat", HSTAT_THEMES_GG),
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
                    "Aucune" = "sans"), selected = "gg"))))
}

.hstat_opt_section <- function(titre, icone, couleur, fond, ...) {
  shiny::div(
    style = sprintf(paste0("background:%s;border-left:4px solid %s;border-radius:6px;",
                           "padding:14px 16px;margin-bottom:14px;"), fond, couleur),
    shiny::h6(shiny::icon(icone), " ", titre,
              style = sprintf(paste0("font-weight:700;color:%s;margin:0 0 12px 0;",
                                     "text-transform:uppercase;letter-spacing:.4px;font-size:12px;"),
                              couleur)),
    ...)
}

HSTAT_FONT_STYLES <- c("Normal" = "plain", "Gras" = "bold",
                       "Italique" = "italic", "Gras + italique" = "bold.italic")

# Themes proposes a l'utilisateur, avec le libelle affiche. Les valeurs sont
# celles que viz_get_theme() sait rendre -- une entree de plus ici sans le
# switch correspondant retomberait en silence sur « minimal ».
# Palettes proposees pour colorer les groupes. Les valeurs sont des noms de
# palettes RColorBrewer : scale_fill_brewer() leve une erreur sur un nom
# inconnu, et le graphique tomberait entierement pour une faute de frappe dans
# une liste de choix. Un test verifie chaque nom contre le catalogue.
# Les qualitatives d'abord : ce sont elles qui conviennent a des groupes sans
# ordre naturel, et c'est le cas le plus frequent.
HSTAT_PALETTES_QUALI <- c("Set1 - vive" = "Set1", "Set2 - douce" = "Set2",
                          "Dark2 - soutenue" = "Dark2",
                          "Accent - contrastée" = "Accent",
                          "Paired - par paires" = "Paired",
                          "Set3 - large (12)" = "Set3",
                          "Pastel" = "Pastel1")

HSTAT_PALETTES_DEGRADE <- c("Bleus" = "Blues", "Verts" = "Greens",
                            "Rouges" = "Reds", "Violets" = "Purples",
                            "Bleu-vert" = "YlGnBu", "Mauve" = "BuPu",
                            "Spectral (froid -> chaud)" = "Spectral",
                            "Rouge-jaune-bleu" = "RdYlBu")

# Alignements horizontaux d'un titre. Les valeurs sont les `hjust` de ggplot,
# transmises en CHAINE par selectInput : le lecteur doit les convertir.
# =============================================================================
#  ELLIPSES DE CONCENTRATION : TOUS LES GROUPES NE PEUVENT PAS EN PORTER
# -----------------------------------------------------------------------------
#  Une ellipse de confiance suppose une covariance INVERSIBLE dans le groupe.
#  Trois cas la rendent impossible, et tous se rencontrent :
#
#    - moins de trois individus dans le groupe ;
#    - une coordonnee constante (variance nulle sur un axe) ;
#    - des points parfaitement alignes (correlation de +/-1), la covariance est
#      alors singuliere.
#
#  ggpubr calcule alors un facteur d'echelle NA et s'arrete sur
#  « Computation failed in stat_conf_ellipse() ... valeur manquante la ou
#  TRUE/FALSE est requis ». Le message ne nomme ni le groupe ni la cause.
#  Signale a l'ecran.
#
#  On verifie donc AVANT de demander les ellipses, et l'on NOMME les groupes qui
#  ne peuvent pas en porter.
# =============================================================================
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

# =============================================================================
#  TELECHARGEMENT D'IMAGE : NE JAMAIS RENVOYER UNE PAGE HTML
# -----------------------------------------------------------------------------
#  Un `content =` de downloadHandler qui leve une erreur -- ou qui se termine
#  sans avoir ECRIT le fichier -- fait renvoyer a Shiny sa page d'erreur HTML.
#  Le navigateur l'enregistre sous le nom demande : on croit tenir un PNG, on
#  ouvre du HTML. Signale a l'ecran.
#
#  D'ou la regle : tout chemin de sortie ecrit un fichier VALIDE du format
#  demande. Quand le graphique est indisponible, l'image porte le motif -- une
#  image qui explique vaut mieux qu'un fichier qu'aucun logiciel n'ouvre.
# =============================================================================
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

# Type MIME a annoncer au navigateur. Sans lui, un contenu inattendu est servi
# en text/html et enregistre comme tel.
hstat_img_mime <- function(fmt) unname(HSTAT_IMG_MIME[hstat_img_fmt(fmt)])

# ---------------------------------------------------------------------------
#  LES DEUX REGLAGES D'EXPORT NE SE DECLARENT QU'ICI
# ---------------------------------------------------------------------------
#  Le format et le DPI etaient reecrits a la main a chaque export : dix-sept
#  listes de formats et vingt champs de DPI, qui avaient DIVERGE.
#
#  Les neuf exports des analyses multivariees n'offraient que quatre formats
#  sur les sept que l'ecrivain sait produire, sans libelle ; deux modules
#  ecrivaient `20000` en clair la ou les autres lisaient `HSTAT_DPI_MAX` -- une
#  montee du plafond en aurait laisse deux en arriere, comme c'etait deja
#  arrive.
#
#  La liste des formats est DERIVEE de ce que l'ecrivain sait ecrire : offrir
#  un format qu'il ignore ferait retomber `hstat_img_fmt()` sur PNG, et
#  l'utilisateur recevrait un PNG portant l'extension demandee.
# Compressions TIFF proposees. Declarees ici, comme les formats : la liste
# etait ecrite en dur dans le seul onglet Visualisation.
HSTAT_TIFF_COMPRESSION <- c("Aucune" = "none", "LZW" = "lzw", "ZIP" = "zip")

HSTAT_FORMATS_IMG <- c(
  "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "BMP" = "bmp",
  "PDF (vectoriel)" = "pdf", "SVG (vectoriel)" = "svg",
  "EPS (vectoriel)" = "eps")

# Selecteur de format d'export. Pas d'argument `choices` : c'est precisement
# ce qui permettait a chaque appel d'inventer sa propre liste.
hstat_format_input <- function(id, label = "Format", selected = "png",
                               width = NULL) {
  shiny::selectInput(id, label, choices = HSTAT_FORMATS_IMG,
                     selected = selected, width = width)
}

# Champ de resolution. Pas d'argument `max` : le plafond est celui de
# l'application, il ne se negocie pas au point d'appel.
hstat_dpi_input <- function(id, label = "DPI", valeur = 300, min = 72,
                            step = 50, width = NULL) {
  shiny::numericInput(id, label, value = valeur, min = min,
                      max = HSTAT_DPI_MAX, step = step, width = width)
}

# Ouvre le peripherique graphique du format demande.
#
# `qualite` (JPEG) et `compression` (TIFF) sont des reglages de FORMAT : ils
# appartiennent donc a l'ecrivain, pas aux modules. Les laisser dehors obligeait
# le seul module qui les propose a ouvrir son propre peripherique -- et a se
# priver de tout ce que l'ecrivain garantit.
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
# existe au retour. `plot` accepte un ggplot, un objet imprimable, ou une
# FONCTION (graphiques base R, qui se tracent au lieu de se renvoyer).
#
#  `secours = FALSE` supprime le filet : aucun fichier n'est laisse quand le
#  trace echoue. C'est ce qu'il faut pour le RAPPORT, ou une figure devenue
#  indessinable doit DISPARAITRE du document -- une image portant un message
#  d'erreur au milieu d'un rapport remis serait pire que son absence. Partout
#  ailleurs (telechargement direct), le filet reste indispensable : sans lui,
#  Shiny renvoie sa page d'erreur HTML sous le nom `.png` demande.
hstat_ecrire_image <- function(file, plot, fmt = "png", width = 10, height = 7.5,
                               dpi = 300, echec = NULL, secours = TRUE,
                               qualite = 95, compression = "lzw") {
  fmt <- hstat_img_fmt(fmt)
  # LE PLAFOND EST ICI, chez l'ecrivain commun : vingt exports en heritent sans
  # que chacun ait a y penser, et aucun ne peut l'oublier.
  #
  # Il porte sur la RESOLUTION, jamais sur la taille : la figure garde la
  # largeur et la hauteur demandees, quel que soit le DPI. Au-dela du cote
  # maximal d'un bitmap, le peripherique echoue et l'utilisateur n'obtient
  # aucun fichier ; mieux vaut une finesse plafonnee qu'un export perdu.
  #
  # Les formats VECTORIELS n'ont pas cette limite : leur resolution est
  # infinie et le DPI n'y veut rien dire. On ne les plafonne donc pas.
  if (!fmt %in% c("pdf", "svg", "eps")) {
    eff <- hstat_dpi_effectif(width, height, dpi)
    dpi <- eff$dpi
  }
  # LE PERIPHERIQUE EST FERME AVANT TOUT LE RESTE, d'ou la fonction anonyme :
  # `on.exit()` s'accroche a un CADRE DE FONCTION, et le bloc d'un `tryCatch`
  # n'en cree pas -- la fermeture etait donc repoussee a la sortie de
  # `hstat_ecrire_image()`, soit APRES le gestionnaire d'erreur et APRES le
  # controle final. Deux consequences, toutes deux verifiees :
  #
  #   * le controle lisait un fichier encore vide (rien n'est ecrit tant que le
  #     peripherique n'est pas ferme) et croyait l'export perdu ;
  #   * sur erreur, l'image de secours etait tracee sur un SECOND peripherique,
  #     puis ecrasee par la fermeture du premier -- l'utilisateur recevait une
  #     image vide au lieu du motif.
  ok <- tryCatch({
    if (is.null(plot)) stop("Aucun graphique à exporter.")
    (function() {
      .hstat_img_device(file, fmt, width, height, dpi, qualite, compression)
      on.exit(grDevices::dev.off(), add = TRUE)
      if (is.function(plot)) plot() else print(plot)
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
# `colour = NA` n'est pas equivalent a l'absence d'argument -- il efface le
# contour que certaines geometries dessinent d'elles-memes -- d'ou une LISTE
# d'arguments montee a la demande plutot qu'un appel fige.
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
#
# La regle depend du SIGNE. Une efficacite negative -- la modalite fait moins
# bien que le temoin, c'est un resultat, pas une erreur -- descend sous l'axe :
# un `vjust` fige ecrirait son etiquette du mauvais cote de la barre, tantot
# dedans quand on la voulait dehors, tantot par-dessus le zero.
#
#   "dessus" : au bout de la barre, a l'exterieur
#   "dedans" : au bout de la barre, a l'interieur
#   "pied"   : au pied de la barre (y = 0), donc toujours visible meme quand la
#              barre sort du cadre limite par l'axe
# Debut d'une suite de graduations alignee sur le pas demande.
#
# `seq(borne, ...)` partant de la borne brute place les graduations n'importe
# ou : avec un minimum a -37 et un pas de 20, on obtient -37, -17, 3, 23... et
# ZERO N'EST PAS GRADUE. Or c'est la seule graduation qui compte quand des
# efficacites sont negatives -- c'est elle qui separe le gain de la perte.
# ---------------------------------------------------------------------------
#  Titre d'axe : revenir a la ligne, et se caler ou l'utilisateur veut
# ---------------------------------------------------------------------------
#  UN TITRE PLUS LONG QUE SON AXE DEBORDE. `element_text()` et
#  `element_markdown()` ne reviennent JAMAIS a la ligne : le titre sort du
#  cadre, se fait rogner a l'export, et sur un nom de variable un peu explicite
#  ("Rendement moyen par parcelle en tonnes par hectare") c'est le cas normal,
#  pas le cas rare.
#
#  `element_textbox_simple()` (ggtext) enveloppe le texte dans une boite de la
#  largeur de l'axe : le retour a la ligne se fait tout seul, a la largeur
#  REELLE, et non a un nombre de caracteres devine.
#
#  `halign` cale le texte DANS la boite -- c'est ce que l'utilisateur appelle
#  centrer ou justifier. `hjust` de `element_text()` ne fait pas la meme chose :
#  il deplace un texte d'un seul tenant, il ne peut pas caler des lignes entre
#  elles.
#
#  Repli sur `element_markdown()` quand ggtext est absent ou quand le retour a
#  la ligne n'est pas demande : le style (gras, italique, taille) survit dans
#  les deux cas.
#  PAS D'ENTREE « JUSTIFIE » : gridtext ne sait pas repartir le texte entre les
#  marges. L'offrir donnerait un reglage que l'image ignore -- le defaut meme
#  que ce depot traque ailleurs (« un reglage declare mais masque n'existe
#  pas »). Trois calages, tous les trois reels.
HSTAT_ALIGN_TITRE <- c("Gauche" = "0", "Centré" = "0.5", "Droite" = "1")

hstat_axe_titre <- function(size = 12, face = "plain", align = "0.5",
                            axe = c("x", "y"), retour = TRUE,
                            colour = NULL, marge = 6) {
  axe <- match.arg(axe)
  h <- suppressWarnings(as.numeric(align)[1])
  if (!isTRUE(is.finite(h))) h <- 0
  marges <- if (axe == "y") ggplot2::margin(r = marge) else ggplot2::margin(t = marge)
  if (!isTRUE(retour) || !requireNamespace("ggtext", quietly = TRUE))
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
#
# UNE LIGNE DE REFERENCE HORS DU CADRE N'EXISTE PAS. Avec des limites
# automatiques, ggplot entraine bien son echelle sur les couches -- un
# `geom_hline` etend la plage. Mais les GRADUATIONS calculees a la main
# (`seq(min, max, pas)`) s'arretent, elles, a l'etendue des donnees : la ligne
# est tracee et aucune graduation ne dit a quelle hauteur elle passe.
#
# `reperes` recoit donc les valeurs qu'on veut voir tenir dans le cadre :
# valeur de seuil, zero, borne de reference. Les non finies sont ignorees --
# un champ vide ne doit pas faire disparaitre l'etendue.
# Les deux controles qui vont avec `hstat_axe_titre()`. Un helper plutot que
# deux widgets recopies : c'est ce qui garantit que les modules offrent les
# MEMES elements -- la recopie derive, on l'a vu sur les themes.
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

# Bandeau "portee des données" : affiche sur chaque onglet d'analyse en mode
# hors-memoire pour rappeler que l'analyse porte sur un echantillon, avec acces
# rapide au reglage de l'echantillon. 'exact' = TRUE si l'onglet propose en plus
# un calcul exact sur le jeu complet.
.hstat_scope_banner <- function(exact = FALSE) {
  shiny::conditionalPanel(
    condition = "output.hstatBigData == true",
    shiny::div(class = "callout callout-warning", style = "margin-bottom:16px;",
      shiny::tags$p(style = "margin:0; font-size:13px;",
        shiny::icon("database"),
        if (exact)
          shiny::HTML(" <b>Mode hors-mémoire.</b> Cette analyse s'exécute sur l'échantillon de travail ; l'option <b>« calculer sur le jeu complet »</b> ci-dessous fournit un résultat exact lorsque c'est applicable.")
        else
          shiny::HTML(" <b>Mode hors-mémoire.</b> Cette analyse ajuste un modèle et s'exécute donc sur l'<b>échantillon de travail</b>. Pour gagner en fidélité, agrandissez l'échantillon dans l'onglet « Chargement » &rarr; « Échantillon de travail »."))
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

hstat_pas_debut <- function(borne, pas) {
  if (!isTRUE(is.finite(borne)) || !isTRUE(is.finite(pas)) || pas <= 0) return(borne)
  floor(borne / pas) * pas
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
                message = trf("Variance nulle pour : %s", paste(response[zero_var], collapse = ", "))))
  
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
                conclusion = trf("Groupes trop petits pour Box's M (min n=%s < p+1=%s)", min_per_group, ncol(Y) + 1)))
  
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
  # Wilks^Inf, donc eta² = 1 : la taille d'effet MAXIMALE, produite par une
  # statistique non calculable, et que `interpret_manova_effect()` qualifierait
  # d'« important ». Pillai / 0 rendrait `Inf` dans le tableau de résultats.
  # Le cas ne se présente pas par `manova_format_all_stats()`, qui écarte la
  # ligne « Residuals » et ne rend donc que des effets à ddl >= 1 ; la garde
  # est là parce qu'un chiffre faux et péremptoire est le pire des retours.
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
  base <- trf("Effet multivarié %s (PERMANOVA, p = %s)", sig, round(p_value, 6))
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
                conclusion = "Échantillon trop petit pour Mahalanobis"))
  
  centre <- colMeans(Y)
  cov_mat <- tryCatch(stats::cov(Y), error = function(e) NULL)
  if (is.null(cov_mat) || any(is.na(cov_mat)) ||
      tryCatch(det(cov_mat) < .Machine$double.eps, error = function(e) TRUE)) {
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
                            paste0("Toutefois, n = ", n, " >= 50 : la MANOVA reste robuste par le ",
                                   "théorème central limite (préférer la statistique de Pillai)."))
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
                        paste0("Violation d'homogénéité des covariances sur ", boxm_violations,
                               " facteur(s) (Box's M significatif). La statistique de Pillai est ",
                               "recommandée car plus robuste à cette violation."))
    score_param <- score_param + 0L  # neutre car Pillai compense
  }
  
  # 3. Homogeneite multivariee des dispersions (PERMDISP)
  permdisp_violations <- if (!is.null(permdisp)) sum(grepl("heterogenes|hétérogènes", permdisp$Conclusion), na.rm = TRUE) else 0
  if (permdisp_violations > 0) {
    alertes <- c(alertes,
                 paste0("PERMDISP signale des dispersions multivariées inégales sur ",
                        permdisp_violations, " facteur(s). Une PERMANOVA significative pourrait ",
                        "refléter une différence de dispersion plutôt qu'une différence de localisation. ",
                        "À interpréter avec prudence."))
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
    
    sub <- hstat_cap_df_rows(sub, what = "PERMANOVA stratifiée")
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
  # methode pour les objets `emmGrid`. Le repli qui suivait appelait
  # `emmeans::cld` -- qui N'EXISTE PAS (« 'cld' is not an exported object »).
  # Enferme dans un `tryCatch`, il rendait NULL au lieu de lever : un repli
  # mort, qui donnait l'illusion d'un filet de securite. Signale par
  # `R CMD check`, jamais par l'execution.
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
      sprintf(paste0("Nuage de points : affichage d'un échantillon de %s points ",
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
    "Shapiro-Wilk normality test (sous-échantillon aléatoire de 5000 valeurs sur %s)",
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
  if (n0 > max_n) hstat_bigdata_note("Corrélation cophénétique", length(idx), n0)
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
    hstat_bigdata_note("Corrélation de Kendall", length(idx), n0)
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
# Plafond du dictionnaire EMBARQUE dans la page, en kilo-octets.
#
# Il tenait en dur dans DEUX tests, avec la meme valeur recopiee : deux chiffres
# identiques qui ne se parlent pas finissent par diverger, et l'un des deux ment.
#
# 120 Ko et non 60 : la couverture des libelles atteignables est passee de 64 %
# a 100 %, ce qui porte le dictionnaire de ~53 a 79 Ko -- 26 Ko une fois
# compresse, ce qui est la taille reellement transmise. C'est le prix d'une
# interface entierement bilingue, et il reste tres inferieur au moindre paquet
# de scripts d'une page web ordinaire. Le plafond garde ce qu'il gardait : que
# la croissance reste VUE, et decidee.
#
# 400 Ko et non 120 : la couverture ne portait que sur les ONGLETS et les
# LIBELLES DE WIDGETS -- 100 % de ceux-la, mais 28,5 % du texte reellement
# affiche. Messages, verdicts, aides et interpretations restaient en francais
# au milieu d'une interface anglaise. Les traduire tous multiplie le
# dictionnaire par trois.
#
# Le chiffre est MESURE, pas estime : a 2 678 entrees le fichier pese 153 Ko
# bruts pour 50 Ko compresses (facteur 3,1), et c'est la taille compressee qui
# transite -- Shiny sert la page en gzip. Le plafond a 400 Ko brut laisse la
# place de finir la traduction en restant sous ~130 Ko sur le reseau, soit le
# poids d'une seule image moyenne pour une application entierement bilingue
# hors ligne.
#
# Il reste un VRAI plafond : un gabarit qui echapperait au filtre, ou une liste
# de modalites partie par erreur au navigateur, le ferait sauter. C'est ce
# qu'il garde -- pas la petitesse pour elle-meme.
HSTAT_I18N_KO_MAX <- 400

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
HSTAT_EFF_AGG <- c("Moyenne" = "moyenne", "Médiane" = "mediane", "Somme" = "somme")

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
    return(msg(vide, paste0("Le témoin choisi n'existe pas dans « ",
                            var_modalite[1], " » : choisissez une modalité présente.")))

  a_rep <- !is.null(var_repetition) && nzchar(var_repetition[1]) &&
           var_repetition[1] %in% names(df)
  # Une variable de repetition demandee mais introuvable etait ignoree EN
  # SILENCE : l'utilisateur croyait ses repetitions prises en compte alors que
  # le calcul les melangeait. On refuse plutot que de rendre un chiffre faux.
  if (!is.null(var_repetition) && nzchar(var_repetition[1]) && !a_rep)
    return(msg(vide, sprintf(paste0("La variable de répétition « %s » est introuvable ",
                                    "dans les données : choisissez-en une autre."),
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

  # LA SOMME N'EST COMPARABLE QUE SI LES REPETITIONS SONT EQUILIBREES. Avec un
  # nombre de repetitions inegal, la modalite la plus repetee accumule
  # mecaniquement davantage et ressort artificiellement « moins efficace » --
  # un artefact de plan pris pour un resultat. La moyenne, elle, n'en souffre
  # pas. On le dit plutot que de laisser publier le chiffre.
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
# autres modalites passent dans une variable ». Sert a l'affichage et a la
# verification, et rend character(0) plutot qu'une erreur sur une entree vide.
# =============================================================================
#  DU LONG AU LARGE : UNE COLONNE PAR VARIABLE MESUREE
# -----------------------------------------------------------------------------
#  hstat_efficacite() empile les variables mesurees : quinze variables sur onze
#  modalites font 165 lignes, toutes portant la meme colonne « Efficacite ». Le
#  selecteur « Variable Y » n'avait donc qu'un seul choix, et le graphique
#  superposait quinze series sur les memes onze positions -- illisible, et
#  surtout faux : on croyait lire une variable, on en lisait quinze.
#
#  Le tableau large donne UNE colonne d'efficacite PAR variable mesuree, nommee
#  d'apres elle. Le selecteur Y liste alors les variables de l'utilisateur, qui
#  choisit celle qu'il veut representer.
#
#  Le prefixe « Efficacite_ » est deliberé : une colonne nommee comme la
#  variable d'origine contiendrait des pourcentages et non la mesure, et se
#  confondrait avec elle des qu'on relit le tableau ou qu'on le reinjecte dans
#  l'application.
# =============================================================================
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

# Fichier statique estampille de la version : « hstat-theme.css?v=0.36.0 ».
#
# Sans cette estampille, le navigateur garde en cache une feuille de style
# servie sous un nom INCHANGE : l'application est mise a jour sur le serveur,
# et l'utilisateur continue de voir l'ancienne mise en page sans qu'aucun
# message ne le lui dise. Le cas s'est presente sur telephone, ou l'on ne
# sait meme pas comment forcer un rechargement.
#
# L'estampille change a chaque montee de version -- c'est-a-dire a chaque
# modification, la regle du depot -- donc le fichier est retelecharge
# exactement quand il le faut, et mis en cache le reste du temps.
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
    utils::read.csv(path, header = header, sep = sep, check.names = FALSE,
             stringsAsFactors = FALSE, fileEncoding = fe)
  }
  # Filet de securite : garantit des chaines UTF-8 valides en sortie.
  hstat_df_to_utf8(df)
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
                msg = "Aucune des feuilles demandées n'existe dans ce classeur."))
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
                        trf(", %d écartée(s) car vide(s) ou illisible(s) : %s",
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
        msg = trf("Union distincte de %d fichiers : %d lignes uniques, %d colonnes.",
                      length(frames), nrow(out), ncol(out))))
    }
    return(list(ok = TRUE, data = out,
                msg = trf("Empilement de %d fichiers : %d lignes, %d colonnes.",
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
                msg = trf("Juxtaposition de %d fichiers : %d lignes, %d colonnes.",
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
      msg = trf("%s sur colonnes communes : %d lignes, %d colonnes.", lbl, nrow(out), ncol(out))))
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
      msg = trf("%s sur « %s » : %d lignes, %d colonnes.",
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
  # CE QUI EST CITE SORT DU JEU. Le tri par longueur ne suffisait pas : quand un
  # nom est le PREFIXE d'un autre et que les deux demandent des accents graves,
  # le court se reinserait DANS les accents du long. « A-1 » et « A-1-bis »
  # donnaient ``A-1`-bis`, et R refuse de l'analyser (« attempt to use
  # zero-length variable name »). Le garde-fou qui existait cherchait la forme
  # citee exacte -- « `A-1` » ne figure pas dans « `A-1-bis` », il ne se
  # declenchait donc jamais. Des noms comme « Rdt-2023 » et
  # « Rdt-2023-corrige » suffisent : le calculateur de variables tombait alors
  # sur un message que personne ne peut relier a ses colonnes.
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
              if (is.finite(acc) && acc > maj) "fait mieux que ce niveau de référence"
              else "ne dépasse pas ce niveau de référence"),
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
            if (is.finite(f1) && f1 >= 0.8) "le modèle est opérationnel pour la prédiction."
            else if (is.finite(f1) && f1 >= 0.6) "le modèle est utilisable mais perfectible (plus de données, autres variables, réglages)."
            else "le modèle n'est pas encore fiable : enrichir les variables ou changer d'algorithme.")
  }
  paste(c(head_txt, core, notes), collapse = " ")
}

# ==============================================================================
#  Export universel de graphiques : PNG/JPG/TIFF/BMP/PDF/SVG, DPI jusqu'a 20 000
# ==============================================================================

# Bloc UI reutilisable : format, dimensions, DPI (max 20 000) + bouton.
# `theme = FALSE` pour les modules qui portent DEJA un selecteur de theme
# (mod_dl, mod_ml, mod_timeseries, via `hstat_plot_opts_ui()`) : deux
# selecteurs pour un meme graphique, c'est un reglage qui en contredit un
# autre. Partout ailleurs il vient AVEC l'export -- ainsi un bloc ajoute
# demain herite du theme sans qu'on y pense, comme il herite deja du format et
# du DPI. Quatre graphiques (descriptif, plan experimental, distribution,
# valeurs manquantes) n'offraient AUCUN choix de theme.
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
    # REGLAGES PROPRES AU FORMAT. L'ecrivain commun (`hstat_ecrire_image`) sait
    # depuis toujours honorer `qualite` et `compression` ; seul l'onglet
    # Visualisation les DEMANDAIT. Les dix-sept autres blocs d'export ecrivaient
    # donc leurs JPEG et leurs TIFF avec les valeurs par defaut, sans que
    # l'utilisateur puisse y toucher -- une compression TIFF ne se choisit pas
    # par hasard quand la figure part chez un editeur.
    #
    # Ils ne s'affichent que pour le format concerne : proposer une qualite JPEG
    # devant un PDF ferait douter de ce que le reglage touche.
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
#
# Rend `NULL` quand le bloc n'offre pas de selecteur (`theme = FALSE`) : ajouter
# `NULL` a un ggplot est sans effet, l'appelant n'a donc pas a se garder.
hstat_export_theme <- function(input, prefix, base_size = 12) {
  v <- input[[paste0(prefix, "Theme")]]
  if (is.null(v) || !nzchar(v)) return(NULL)
  viz_get_theme(v, base_size = base_size)
}

# Handler d'export associe. plot_fun() doit renvoyer un ggplot (ou NULL).
#
# L'ECRITURE N'EST PAS REFAITE ICI : elle passe par `hstat_ecrire_image()`,
# l'ecrivain commun. Ce qui reste au handler, c'est de LIRE les reglages du
# prefixe et de nommer le fichier -- son seul travail propre.
#
# Ce que ce branchement corrige au passage : `stop()` sur un graphique absent
# ne laissait AUCUN fichier, et Shiny renvoyait alors sa page d'erreur HTML,
# que le navigateur enregistrait sous le nom `.png` demande. Treize exports
# etaient dans ce cas -- on croyait tenir une image, on ouvrait du HTML.
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
      # matriciel ne peut pas suivre. L'utilisateur doit le savoir, d'ou
      # l'annonce -- le plafonnement lui-meme vit chez l'ecrivain commun.
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

# ==============================================================================
#  Personnalisation d'apparence reutilisable pour les graphiques de modelisation
# ==============================================================================

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
                             value = 0, min = 0, max = 90, step = 15)))
  )
}

# Applique les options ci-dessus a un ggplot. La couleur/epaisseur sont lues par
# les fonctions de trace via hstat_plot_opt(input, prefix, "Col"/"Lwd").
hstat_apply_plot_opts <- function(g, input, prefix) {
  if (is.null(g)) return(g)
  # UN SEUL CHOISISSEUR DE THEME. Ce `switch` etait un SECOND, et il avait
  # derive : il connaissait cinq themes sur les huit du catalogue, si bien que
  # « Gris », « Traits fins » et « Sans decor » retombaient EN SILENCE sur
  # « Minimal ». L'utilisateur changeait le reglage et l'image ne bougeait pas
  # -- treize blocs d'export etaient concernes. C'est exactement le defaut que
  # `HSTAT_THEMES_GG` et `viz_get_theme()` existent pour empecher.
  base <- hstat_finite(input[[paste0(prefix, "Base")]], 13)
  g <- g + viz_get_theme(input[[paste0(prefix, "Theme")]] %||% "minimal",
                         base_size = max(7, min(30, base)))
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
  shiny::div(style = "margin-top:6px;",
      shiny::downloadButton(ns(paste0(prefix, "Csv")), "CSV", class = "btn-sm"),
      shiny::downloadButton(ns(paste0(prefix, "Xlsx")), "Excel", class = "btn-sm"))
}

hstat_export_table_handlers <- function(output, prefix, data_fun, fname = "resultats") {
  # Un tableau seul est une liste d'un element : meme chemin d'ecriture que les
  # exports a plusieurs feuilles, donc meme garantie.
  tables <- function() {
    d <- data_fun()
    if (is.null(d)) NULL else stats::setNames(list(as.data.frame(d)), fname)
  }
  output[[paste0(prefix, "Csv")]]  <- hstat_csv_handler(tables, fname)
  output[[paste0(prefix, "Xlsx")]] <- hstat_classeur_handler(tables, fname)
}

# ---------------------------------------------------------------------------
#  UN SEUL ECRIVAIN DE TABLEAUX, comme il n'y a qu'un ecrivain d'image
# ---------------------------------------------------------------------------
#  Vingt telechargements de tableaux montaient chacun leur classeur : meme
#  boucle sur les feuilles, meme archive ZIP de CSV, meme `tryCatch`, meme
#  notification. La seule chose qui leur appartenait vraiment, c'est la LISTE
#  NOMMEE de tableaux a ecrire.
#
#  Ils partageaient aussi le meme defaut que les images : un `req()` ou un
#  `return(NULL)` sans avoir ecrit le fichier fait renvoyer a Shiny sa page
#  d'erreur HTML, que le navigateur enregistre en `.xlsx`. Excel refuse alors
#  de l'ouvrir, sans dire pourquoi. Tout chemin ecrit donc un classeur valide,
#  portant le motif s'il n'y a rien a exporter.

HSTAT_MIME_XLSX <-
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

# Nom de feuille accepte par Excel : 31 caracteres, sans []:*?/\\ .
# `addWorksheet()` LEVE sur un nom trop long -- et le nom vient parfois d'une
# variable de l'utilisateur.
hstat_feuille_nom <- function(x, defaut = "Feuille") {
  s <- gsub("[^A-Za-z0-9_ .-]", "_", as.character(x)[1] %||% "")
  s <- substr(trimws(s), 1, 31)
  if (!nzchar(s)) defaut else s
}

# Liste nommee de tableaux -> classeur d'une feuille par element.
hstat_ecrire_classeur <- function(file, tables) {
  wb <- openxlsx::createWorkbook()
  vus <- character(0)
  for (i in seq_along(tables)) {
    nm <- hstat_feuille_nom(names(tables)[i] %||% "", paste0("Feuille", i))
    # Deux tableaux dont les noms se ressemblent a 31 caracteres pres donnent
    # le meme nom de feuille, et `addWorksheet()` refuse le doublon.
    if (nm %in% vus) nm <- hstat_feuille_nom(paste0(substr(nm, 1, 27), "_", i))
    vus <- c(vus, nm)
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, as.data.frame(tables[[i]]))
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  length(tables)
}

# Liste nommee de tableaux -> archive ZIP d'un CSV par element.
hstat_ecrire_csv_zip <- function(file, tables) {
  dossier <- file.path(tempdir(), paste0("hstat_csv_", as.integer(Sys.time())))
  dir.create(dossier, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  noms <- character(0)
  for (i in seq_along(tables)) {
    nm <- hstat_feuille_nom(names(tables)[i] %||% "", paste0("tableau", i))
    f  <- paste0(nm, ".csv")
    utils::write.csv(as.data.frame(tables[[i]]), file.path(dossier, f),
                     row.names = FALSE, fileEncoding = "UTF-8")
    noms <- c(noms, f)
  }
  # `-q` : sans lui, chaque archive ecrit sa liste de fichiers dans la console
  # du serveur, a chaque telechargement.
  utils::zip(file, file.path(dossier, noms), flags = "-jq")
  noms
}

# Ecarte les tableaux absents ou vides d'une liste nommee. Une feuille vide
# n'apporte rien, et laisse croire a une information manquante -- c'est la meme
# regle que pour les colonnes du module d'efficacite.
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
# Proposer une archive quand il n'y a qu'un tableau serait un detour ; en
# livrer un seul quand il y en a plusieurs en perdrait.
hstat_csv_handler <- function(tables_fun, fname = "resultats",
                              libelle = "Export CSV") {
  multiple <- function() length(.hstat_tables_ou_motif(tables_fun, libelle)) > 1
  # PAS de `contentType` : Shiny ne l'EVALUE PAS quand c'est une fonction, il le
  # passe tel quel dans l'en-tete HTTP (`download$contentType %||%
  # getContentType(filename)`). Or le type depend ici du nombre de tableaux.
  # L'omettre laisse Shiny le deduire de l'extension, qui est deja juste.
  shiny::downloadHandler(
    filename = function()
      paste0(fname, "_", Sys.Date(), if (multiple()) ".zip" else ".csv"),
    content = function(file) {
      tb <- .hstat_tables_ou_motif(tables_fun, libelle)
      if (length(tb) > 1) hstat_ecrire_csv_zip(file, tb)
      else utils::write.csv(as.data.frame(tb[[1]]), file, row.names = FALSE,
                            fileEncoding = "UTF-8")
    })
}

# Les deux telechargements d'un meme resultat, declares d'un seul appel.
# Les identifiants suivent le prefixe (`<prefixe>Xlsx`, `<prefixe>Csv`) : c'est
# le meme contrat que pour les graphiques.
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
# niveaux de facteurs du jeu d'entrainement. Retourne list(data, warn).
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


# ==============================================================================
#  Fiches des modeles : principe, objectif, conditions d'application
# ==============================================================================

hstat_model_doc <- function(id) {
  d <- list(
    # ---- Series temporelles ----
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
    # ---- Machine learning supervise ----
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
    # ---- Clustering ----
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
    # ---- Deep learning ----
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
      shiny::tags$p(shiny::icon("book"), shiny::strong(sprintf(" Fiche du modèle — %s", f$nom))),
      shiny::tags$p(shiny::strong("Principe : "), f$principe),
      shiny::tags$p(shiny::strong("Objectif : "), f$objectif),
      shiny::tags$p(shiny::strong("Conditions d'application : "), f$conditions))
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

# =============================================================================
#  PIXELS ET RESOLUTION : DEUX MODELES, ET IL FAUT DIRE LEQUEL
# -----------------------------------------------------------------------------
#  Quand les champs de largeur/hauteur sont RECALCULES a chaque changement de
#  DPI -- c'est le cas des analyses multivariees -- ils affichent les pixels
#  reellement produits, et la taille physique est l'invariant :
#
#      pouces = pixels / DPI          (constant)
#      pixels = pouces x DPI          (recalcule a chaque changement de DPI)
#
#  Le champ dit alors exactement ce que le fichier contiendra. C'est pour cela
#  que la division par le DPI, faute a l'endroit ou l'utilisateur saisit des
#  pixels a la main (voir hstat_export_dims), est ici la bonne operation :
#  personne ne saisit ces pixels, ils sont derives.
# =============================================================================

# Nouvelle taille en pixels apres un changement de resolution, a taille
# physique constante. Le DPI precedent est indispensable : sans lui on ne peut
# pas savoir a quelle taille physique les pixels actuels correspondent.
hstat_px_apres_dpi <- function(px, dpi_avant, dpi_apres, max_px = HSTAT_EXPORT_MAX_PX) {
  n <- function(x) suppressWarnings(as.numeric(x)[1])
  px <- n(px); a <- n(dpi_avant); b <- n(dpi_apres)
  if (!isTRUE(is.finite(px)) || px <= 0) return(NULL)
  if (!isTRUE(is.finite(a)) || a <= 0 || !isTRUE(is.finite(b)) || b <= 0) return(NULL)
  round(min(px * b / a, max_px))
}

# Pouces correspondant a une taille en pixels rendue a une resolution donnee.
# Bornee : une valeur absurde ferait echouer l'export au lieu de rendre une
# image un peu differente de celle qu'on attendait.
hstat_px_en_pouces <- function(px, dpi, defaut = 8, max_in = 200) {
  n <- function(x) suppressWarnings(as.numeric(x)[1])
  px <- n(px); d <- n(dpi)
  if (!isTRUE(is.finite(px)) || px <= 0 ||
      !isTRUE(is.finite(d))  || d  <= 0) return(defaut)
  max(1, min(px / d, max_in))
}

# Pixels a produire pour une taille physique et une resolution donnees --
# l'operation inverse de la precedente, et celle qui doit piloter les champs.
#
# Recalculer les pixels a partir des PIXELS PRECEDENTS et de l'ANCIEN DPI
# (px x nouveau / ancien) donne le meme resultat quand tout va bien, mais
# suppose deux etats que rien ne garantit : que le serveur ait retenu l'ancien
# DPI, et que le navigateur ait deja renvoye les pixels ecrits au changement
# d'avant. Un panneau reconstruit remet les champs a leur valeur d'origine sans
# que l'ancien DPI bouge ; deux changements plus rapproches que l'aller-retour
# font repartir le calcul de pixels perimes. Dans les deux cas la taille cesse
# de suivre la resolution -- le defaut signale.
#
# La taille physique, elle, ne depend d'aucun des deux.
hstat_px_pour_dpi <- function(pouces, dpi, max_px = HSTAT_EXPORT_MAX_PX) {
  n <- function(x) suppressWarnings(as.numeric(x)[1])
  po <- n(pouces); d <- n(dpi)
  if (!isTRUE(is.finite(po)) || po <= 0) return(NULL)
  if (!isTRUE(is.finite(d))  || d  <= 0) return(NULL)
  round(min(po * d, max_px))
}

# Emplacement de la note « ce que le fichier contiendra », posee sous les trois
# champs d'export. L'utilisateur ne voyait nulle part le lien entre la
# resolution et la taille produite : il ne restait qu'a le croire.
hstat_mv_dim_note_ui <- function(prefix) {
  shiny::uiOutput(paste0(prefix, "_dimnote"))
}

HSTAT_EXPORT_REF_DPI <- 96

# Borne de securite : au-dela, ggsave tente d'allouer un bitmap que la machine
# ne peut pas tenir et l'export echoue au lieu de rendre une image un peu moins
# fine. On abaisse le DPI et on le DIT -- un export silencieusement degrade
# serait pire que le refus.
HSTAT_EXPORT_MAX_PX <- 20000L

# Plafond propre a l'export de l'onglet Visualisation, ou la taille physique
# est fixe et seul le DPI varie : au-dela, `ggsave` echoue sur l'allocation du
# bitmap et l'utilisateur n'obtient AUCUN fichier.
# Plafond du champ DPI, partout dans l'application. Un seul chiffre : neuf
# champs plafonnaient a 1200 ou 2000 sans raison, et l'utilisateur ne pouvait
# pas demander mieux la ou il en avait besoin.
HSTAT_DPI_MAX <- 20000L

# Cote maximal, en pixels, d'une image MATRICIELLE. Au-dela, le peripherique
# graphique echoue sur l'allocation du bitmap et l'utilisateur n'obtient aucun
# fichier. C'est une limite du format, pas un choix.
HSTAT_RASTER_MAX_PX <- 20000L

# Resolution reellement applicable a une taille physique DONNEE.
#
# Regle unique de toute l'application : monter le DPI ne change JAMAIS la
# largeur, la hauteur ni la mise en page. Deux exports faisaient l'inverse --
# ils multipliaient les pouces par un facteur de reduction, si bien que
# demander plus de finesse rendait l'image plus petite sur le papier.
#
# Quand le matriciel ne peut plus suivre, c'est donc la RESOLUTION qui est
# ramenee, jamais la taille ; et elle est annoncee, parce qu'un export
# silencieusement degrade est pire qu'un refus. Le vecteur (PDF, SVG) n'a pas
# cette limite : sa resolution est infinie et le DPI n'y veut rien dire.
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
#
# Le calcul vivait en deux exemplaires : dans le telechargement, et dans le
# panneau qui ANNONCE a l'utilisateur ce que le fichier contiendra. Deux copies
# d'une meme regle finissent par diverger -- corriger l'une sans l'autre ferait
# annoncer 6 x 4 pouces pour un fichier de 12 x 8.
#
# La taille physique est FIXE : le DPI ne multiplie que la finesse. Un escalier
# la reduisait a mesure que le DPI montait (12 x 8 jusqu'a 600 DPI, 6 x 4
# au-dela de 5000) : demander plus de finesse rendait l'image plus PETITE sur
# le papier. Seul le plafond de pixels peut encore la reduire, la ou l'export
# echouerait sinon.
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

# ---------------------------------------------------------------------------
#  Replis des paquets d'interface optionnels
# ---------------------------------------------------------------------------
#  DEFINIR ces replis appartient au paquet ; DECIDER de les installer appartient
#  au demarrage, car la reponse depend de ce qui est installe sur la machine
#  d'EXECUTION -- pas sur celle qui a construit le paquet.
#
#  Le bloc est evalue TEL QUEL dans `envir` : reecrire chaque `nom <- function`
#  en `assign()` etait tentant, mais une premiere tentative a coupe une
#  definition dont le corps tenait sur la ligne suivante. `eval(quote({...}))`
#  ne transforme rien, donc ne peut rien casser.
#
#  Consequence directe, et c'est la raison d'etre du deplacement : un module se
#  teste desormais seul (`shiny::testServer`) apres un simple appel a cette
#  fonction. Tant que ces replis vivaient dans le pont, tester
#  `mod_tests_server` echouait sur « could not find function updatePickerInput ».
# `layout` EST DEFINI ICI, au premier niveau, et pas seulement pose dans
# l'environnement global par `hstat_installer_replis_ui()`.
#
# C'est le seul aiguillage dont le nom existe aussi dans un paquet DE BASE
# (`graphics::layout`). Le compilateur d'octets, qui ne voit que le paquet,
# resolvait donc `layout(showlegend = FALSE)` en `graphics::layout` et rendait
# sept avertissements « unused argument » a l'installation. A l'execution
# l'environnement global l'emportait -- l'avertissement etait faux, mais il
# signalait une vraie fragilite : le code du paquet appele SANS l'application
# (un module sous `testServer`, par exemple) serait bien tombe sur
# `graphics::layout`. Les dix autres aiguillages n'ont pas d'homonyme de base :
# le compilateur n'a rien a quoi les resoudre, et se tait.
#
# La decision est prise A CHAQUE APPEL, et non une fois pour toutes : c'est la
# machine d'execution qui sait si plotly est la, pas celle de construction.
layout <- function(p, ...) {
  if (isTRUE(requireNamespace("plotly", quietly = TRUE))) plotly::layout(p, ...) else p
}

# ---------------------------------------------------------------------------
# Dependances web declarees mais introuvables
# ---------------------------------------------------------------------------
# `shiny::sliderInput()` attache la dependance « strftime » en annoncant
# `strftime-min.js` -- alors que le paquet livre `strftime.min.js`. Le
# navigateur recoit un 404 sur CHAQUE page, et une erreur permanente en
# console masque les vraies. Meme famille que le polyfill `typedarray` de
# plotly, meme remede : on repare la declaration au lieu de la subir.
#
# `resolveDependencies()` garde, a nom egal, la version la plus haute : il
# suffit donc d'attacher la version corrigee au sommet de l'interface pour
# qu'elle l'emporte sur celles posees par chaque curseur.
hstat_reparer_deps <- function(ui) {
  if (!requireNamespace("htmltools", quietly = TRUE)) return(ui)
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

hstat_installer_replis_ui <- function(envir = globalenv()) {
  has <- function(p) isTRUE(requireNamespace(p, quietly = TRUE))
  # UN AIGUILLAGE, PAS UN REPLI CONDITIONNEL. Ces noms sont desormais TOUJOURS
  # definis : soit ils pointent sur la fonction du paquet, soit sur un
  # equivalent de base. La difference n'est pas cosmetique -- l'ancienne forme
  # ne definissait le repli que si le paquet etait ABSENT, si bien qu'un paquet
  # INSTALLE MAIS NON ATTACHE ne donnait ni l'un ni l'autre. Constate en
  # integration continue : « could not find function updatePickerInput », alors
  # que shinyWidgets etait bien installe sur la machine.
  #
  # Consequence voulue : l'application ne depend plus de ce que `library()` a
  # attache. C'est ce qui rend un module testable seul.
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
  # `layout` n'est pas refabrique ici : il est defini au premier niveau (voir
  # ci-dessus) et seulement RECOPIE. Deux definitions du meme aiguillage
  # finiraient par diverger.
  poser("layout", layout)
  poser("config", if (has("plotly")) plotly::config else function(p, ...) p)

  # Le nettoyage du polyfill obsolete est pose sur `renderPlotly` LUI-MEME
  # plutot qu'a chacun des appels : leurs corps comportent plusieurs `return()`,
  # qu'un habillage de l'expression sauterait purement et simplement.
  # `exprToFunction` transforme le bloc en fonction -- le `return()` en sort
  # alors normalement, et la valeur passe bien par le nettoyage.
  poser("renderPlotly", if (has("plotly"))
    function(expr, env = parent.frame(), quoted = FALSE) {
      fn <- shiny::exprToFunction(expr, env, quoted)
      plotly::renderPlotly(hstat_plotly_clean(fn()),
                           env = environment(), quoted = FALSE)
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

  # -- colourpicker ------------------------------------------------------------
  poser("colourInput", if (has("colourpicker")) colourpicker::colourInput
        else function(inputId, label, value = "#000000", ...)
          shiny::textInput(inputId, label, value = value))

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
