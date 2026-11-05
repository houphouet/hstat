#' Launch the HStat Shiny Application
#'
#' Starts the HStat interactive statistical analysis application. On first
#' launch, any missing package dependencies are installed automatically (an
#' internet connection is required the first time only).
#'
#' In an interactive session, HStat asks where to open: in the web browser or
#' in the RStudio Shiny window. The answer can be kept as the default, stored
#' in \code{tools::R_user_dir("HStat", "config")}; HStat then opens there
#' without asking. \code{run_hstat(ouverture = "demander")} forgets the
#' stored default and asks again.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'   (e.g. \code{port}). Passing \code{launch.browser} bypasses the choice
#'   of where to open.
#' @param ouverture Where to open the app. \code{NULL} (default) uses the
#'   stored default, or asks when there is none. \code{"navigateur"} opens in
#'   the web browser and \code{"shiny"} in the RStudio Shiny window, this time
#'   only. \code{"demander"} forgets the stored default and asks again.
#' @param mise_a_jour What to do when a newer HStat is published on GitHub.
#'   \code{"auto"} (default) installs it and relaunches the new version,
#'   \code{"demander"} asks first, \code{"non"} skips the check. The default
#'   can be set with \code{options(hstat.mise_a_jour = "non")}. The check
#'   runs in interactive sessions only, and gives up silently after a few
#'   seconds without network.
#' @param install_missing Logical; if \code{TRUE} (default) missing package
#'   dependencies are installed before the app is launched. Set to
#'   \code{FALSE} to skip installation (e.g. on a managed server where
#'   packages are provisioned separately).
#'
#' @return No return value. Called for its side effect of launching the app.
#'
#' @examples
#' if (interactive()) {
#'   run_hstat()
#' }
#'
#' @export
run_hstat <- function(..., ouverture = NULL, mise_a_jour = NULL,
                      install_missing = TRUE) {
  # --- Mise a jour depuis GitHub, AVANT tout le reste ----------------------
  # Une version installee pendant que l'ancienne reste chargee rend la base
  # paresseuse de l'ancienne illisible (« lazy-load database is corrupt ») :
  # apres l'installation on ne touche plus a rien de l'ancien espace de noms,
  # on le decharge et on rappelle la NOUVELLE run_hstat().
  if (isTRUE(.hstat_maj_verifier(mise_a_jour))) {
    relance <- tryCatch({
      base::unloadNamespace("HStat")
      f <- base::getExportedValue("HStat", "run_hstat")
      base::attachNamespace("HStat")
      f
    }, error = function(e) NULL)
    if (is.null(relance)) {
      base::message("HStat a \u00e9t\u00e9 mis \u00e0 jour. Red\u00e9marrez R ",
                    "(Session > Restart R) puis relancez run_hstat().")
      return(invisible(NULL))
    }
    return(relance(..., ouverture = ouverture, mise_a_jour = "non",
                   install_missing = install_missing))
  }

  app_dir <- system.file("app", package = "HStat")

  if (app_dir == "") {
    stop(
      "Could not find the HStat app directory. ",
      "Try re-installing the package with remotes::install_github('houphouet/hstat').",
      call. = FALSE
    )
  }

  app_file <- file.path(app_dir, "app.R")
  if (!file.exists(app_file)) {
    stop("Could not find app.R inside the installed package (expected at : ",
         app_file, ").", call. = FALSE)
  }

  # --- Installation prealable de TOUTES les dependances -------------------
  # C'est indispensable : HStat.R construit l'interface (dashboardPage,
  # dropdownMenu, pickerInput, ...) des le sourcage. Si un seul de ces
  # paquets manque, la construction de `ui` echoue, shinyApp() n'est jamais
  # atteint, et Shiny affiche le trompeur "No UI defined".
  if (isTRUE(install_missing)) {
    deps <- .hstat_required_packages()
    missing_pkgs <- deps[!vapply(deps, function(p)
      suppressWarnings(suppressMessages(requireNamespace(p, quietly = TRUE))),
      logical(1))]
    if (length(missing_pkgs) > 0) {
      message("HStat : installation de ", length(missing_pkgs),
              " paquet(s) manquant(s)...\n  ",
              paste(missing_pkgs, collapse = ", "))
      tryCatch(
        utils::install.packages(missing_pkgs),
        error = function(e)
          message("Certains paquets n'ont pas pu être installés : ",
                  conditionMessage(e)))
    }
    # Verifier les paquets STRICTEMENT necessaires a l'interface
    ui_critical <- c("shiny", "shinydashboard", "shinyWidgets", "shinyjs",
                     "DT", "ggplot2", "plotly", "dplyr", "shinycssloaders")
    still <- ui_critical[!vapply(ui_critical, function(p)
      suppressWarnings(suppressMessages(requireNamespace(p, quietly = TRUE))),
      logical(1))]
    if (length(still) > 0) {
      stop(
        "HStat ne peut pas démarrer : ces paquets d'interface sont ",
        "manquants et n'ont pas pu être installés :\n  ",
        paste(still, collapse = ", "),
        "\n\nInstallez-les manuellement puis relancez run_hstat() :\n  ",
        "install.packages(c(",
        paste(sprintf('\"%s\"', still), collapse = ", "), "))",
        call. = FALSE)
    }
  }

  # Ou ouvrir l'application. Un `launch.browser` passe explicitement
  # l'emporte : c'est une demande, on ne la redemande pas.
  args <- list(...)
  if (!"launch.browser" %in% names(args)) {
    lanceur <- .hstat_ouverture_lanceur(.hstat_ouverture_resoudre(ouverture))
    if (!is.null(lanceur)) args$launch.browser <- lanceur
  }

  # runApp() sur un DOSSIER sert automatiquement le sous-dossier www/
  # (feuille de style + polices) et detecte app.R.
  do.call(shiny::runApp, c(list(app_dir), args))
}

## ---------------------------------------------------------------------------
##  Ou ouvrir l'application : navigateur ou fenetre Shiny de RStudio
## ---------------------------------------------------------------------------
##  La question se pose une fois, et la reponse peut devenir le defaut. Elle
##  n'est ECRITE sur disque que si l'utilisateur le demande (choix 3 ou 4) :
##  rien n'est depose dans son dossier de configuration sans son accord.
##  `run_hstat(ouverture = "demander")` oublie le defaut et repose la question.
##
##  Hors session interactive on ne demande rien et on ne choisit rien : Shiny
##  garde alors son propre comportement (aucune ouverture sous Rscript).

HSTAT_OUVERTURES <- c(navigateur = "Navigateur web",
                      shiny      = "Fen\u00eatre Shiny de RStudio")

.hstat_ouverture_fichier <- function() {
  d <- tryCatch(tools::R_user_dir("HStat", "config"), error = function(e) "")
  if (!is.character(d) || length(d) != 1L || !nzchar(d)) return("")
  file.path(d, "ouverture.txt")
}

# Un fichier illisible ou porteur d'une valeur inconnue ne vaut pas un choix :
# on redemande plutot que d'ouvrir la ou personne ne l'a demande.
.hstat_ouverture_lire <- function(fichier = .hstat_ouverture_fichier()) {
  if (!nzchar(fichier) || !file.exists(fichier)) return(NA_character_)
  v <- tryCatch(trimws(readLines(fichier, n = 1L, warn = FALSE)),
                error = function(e) character(0))
  if (length(v) == 1L && v %in% names(HSTAT_OUVERTURES)) v else NA_character_
}

.hstat_ouverture_ecrire <- function(mode, fichier = .hstat_ouverture_fichier()) {
  if (!nzchar(fichier) || !mode %in% names(HSTAT_OUVERTURES)) return(FALSE)
  isTRUE(tryCatch({
    dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
    writeLines(mode, fichier)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE))
}

.hstat_ouverture_oublier <- function(fichier = .hstat_ouverture_fichier()) {
  if (nzchar(fichier) && file.exists(fichier)) unlink(fichier)
  invisible(NULL)
}

# Rend list(mode, memoriser), ou NULL si l'utilisateur annule (0).
.hstat_ouverture_demander <- function(choix = utils::menu) {
  i <- choix(c("Navigateur web (cette fois)",
               "Fen\u00eatre Shiny de RStudio (cette fois)",
               "Navigateur web, et par d\u00e9faut d\u00e9sormais",
               "Fen\u00eatre Shiny de RStudio, et par d\u00e9faut d\u00e9sormais"),
             title = "O\u00f9 ouvrir HStat ?")
  if (!is.numeric(i) || length(i) != 1L || is.na(i) || !i %in% 1:4) return(NULL)
  list(mode = names(HSTAT_OUVERTURES)[(i - 1L) %% 2L + 1L], memoriser = i > 2L)
}

# NA = ne rien imposer (Shiny garde son comportement).
.hstat_ouverture_resoudre <- function(ouverture = NULL, interactif = interactive(),
                                      demander = .hstat_ouverture_demander,
                                      fichier = .hstat_ouverture_fichier()) {
  if (!is.null(ouverture)) {
    ouverture <- match.arg(ouverture, c(names(HSTAT_OUVERTURES), "demander"))
    if (ouverture != "demander") return(ouverture)
    .hstat_ouverture_oublier(fichier)
  } else {
    m <- .hstat_ouverture_lire(fichier)
    if (!is.na(m)) return(m)
  }
  if (!isTRUE(interactif)) return(NA_character_)
  r <- demander()
  if (is.null(r)) return(NA_character_)
  if (isTRUE(r$memoriser)) {
    if (.hstat_ouverture_ecrire(r$mode, fichier))
      message("HStat s'ouvrira d\u00e9sormais dans : ", HSTAT_OUVERTURES[[r$mode]],
              ". Pour changer : run_hstat(ouverture = \"demander\")")
    else
      message("Le choix n'a pas pu \u00eatre m\u00e9moris\u00e9 : il vaut pour cette fois.")
  }
  r$mode
}

# Les lanceurs que RStudio installe lui-meme : ce sont eux qu'emploie son
# bouton « Run App », et ils savent traverser le mandataire de RStudio Server,
# ce que `browseURL()` sur une adresse locale ne sait pas faire.
.hstat_rstudio_fonction <- function(nom) {
  e <- tryCatch(as.environment("tools:rstudio"), error = function(e) NULL)
  if (is.null(e)) return(NULL)
  get0(nom, envir = e, mode = "function", inherits = FALSE)
}

.hstat_ouverture_lanceur <- function(mode, trouver = .hstat_rstudio_fonction) {
  if (length(mode) != 1L || is.na(mode)) return(NULL)
  if (identical(mode, "shiny")) {
    f <- trouver(".rs.invokeShinyWindowViewer")
    if (!is.null(f)) return(f)
    message("La fen\u00eatre Shiny n'existe que dans RStudio : ",
            "ouverture dans le navigateur.")
  }
  trouver(".rs.invokeShinyWindowExternal") %||%
    function(url) utils::browseURL(url)
}


# Liste des dependances requises, lue depuis inst/app/Utils.R (source unique de
# verite) pour rester synchronisee avec l'application. Repli sur une liste
# minimale si l'extraction echoue.
.hstat_required_packages <- function() {
  utils_file <- system.file("app", "Utils.R", package = "HStat")
  fallback <- c("shiny", "shinydashboard", "shinyWidgets", "shinyjs", "DT",
                "shinycssloaders", "ggplot2", "plotly", "dplyr", "tidyr",
                "DT", "colourpicker", "sortable", "ggrepel", "scales")
  if (!nzchar(utils_file) || !file.exists(utils_file)) return(fallback)
  txt <- tryCatch(paste(readLines(utils_file, warn = FALSE, encoding = "UTF-8"),
                        collapse = "\n"),
                  error = function(e) "")
  m <- regmatches(txt, regexpr("required_packages\\s*<-\\s*c\\((?:[^()]|\\([^()]*\\))*\\)",
                               txt, perl = TRUE))
  if (length(m) == 0) return(fallback)
  pkgs <- regmatches(m, gregexpr('"([^"]+)"', m))[[1]]
  pkgs <- gsub('"', "", pkgs)
  pkgs <- setdiff(unique(pkgs), c("stats", "utils", "methods", "grDevices",
                                  "graphics", "tools", "grid", "base"))
  if (length(pkgs) < 5) fallback else pkgs
}

## ---------------------------------------------------------------------------
##  Mise a jour depuis GitHub
## ---------------------------------------------------------------------------
##  La reference est le champ `Version:` du DESCRIPTION de la branche
##  principale : c'est la source unique du numero (regle du depot), et elle
##  monte a chaque modification. On ne met a jour que vers une version
##  STRICTEMENT plus recente : une installation locale en avance (developpement)
##  n'est jamais ramenee en arriere.
##
##  Hors session interactive on ne verifie rien : sur un serveur, une
##  installation au demarrage changerait le code sous les autres sessions.

HSTAT_DEPOT <- "houphouet/hstat"
HSTAT_MAJ_URL <- "https://raw.githubusercontent.com/houphouet/hstat/main/DESCRIPTION"
HSTAT_MAJ_MODES <- c("auto", "demander", "non")
HSTAT_MAJ_DELAI <- 5

# Le numero publie, ou NULL : sans reseau, un depot injoignable ou un fichier
# illisible ne doivent ni lever ni bloquer le lancement.
.hstat_version_distante <- function(url = HSTAT_MAJ_URL, delai = HSTAT_MAJ_DELAI) {
  ancien <- options(timeout = delai)
  on.exit(options(ancien), add = TRUE)
  txt <- tryCatch(suppressWarnings(readLines(url, warn = FALSE)),
                  error = function(e) NULL)
  .hstat_version_lire(txt)
}

.hstat_version_lire <- function(txt) {
  if (!is.character(txt) || !length(txt)) return(NULL)
  v <- sub("^Version:[[:space:]]*", "", grep("^Version:", txt, value = TRUE)[1])
  if (is.na(v)) return(NULL)
  tryCatch(package_version(trimws(v)), error = function(e) NULL)
}

.hstat_version_locale <- function() {
  tryCatch(utils::packageVersion("HStat"), error = function(e) NULL)
}

# `upgrade = "never"` : la mise a jour de HStat ne doit pas relancer celle de
# cent dependances sans qu'on l'ait demande ; celles qui manquent sont
# installees ensuite par run_hstat() comme au premier lancement.
.hstat_maj_installer <- function(depot = HSTAT_DEPOT) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    ok <- tryCatch({ utils::install.packages("remotes"); TRUE },
                   error = function(e) FALSE)
    if (!ok || !requireNamespace("remotes", quietly = TRUE)) return(FALSE)
  }
  isTRUE(tryCatch({
    remotes::install_github(depot, upgrade = "never", quiet = TRUE)
    TRUE
  }, error = function(e) {
    message("La mise \u00e0 jour de HStat a \u00e9chou\u00e9 : ", conditionMessage(e))
    FALSE
  }))
}

.hstat_maj_demander <- function(locale, distante, choix = utils::menu) {
  i <- choix(c("Mettre \u00e0 jour maintenant", "Pas cette fois"),
             title = sprintf("HStat %s est disponible (install\u00e9e : %s).",
                             distante, locale))
  identical(as.integer(i), 1L)
}

# TRUE si une nouvelle version vient d'etre installee.
.hstat_maj_verifier <- function(mode = NULL, interactif = interactive(),
                                distante = .hstat_version_distante,
                                locale = .hstat_version_locale,
                                installer = .hstat_maj_installer,
                                demander = .hstat_maj_demander) {
  mode <- mode %||% getOption("hstat.mise_a_jour", "auto")
  if (!is.character(mode) || length(mode) != 1L || !mode %in% HSTAT_MAJ_MODES)
    mode <- "auto"
  if (mode == "non" || !isTRUE(interactif)) return(FALSE)
  loc <- locale()
  if (is.null(loc)) return(FALSE)
  dis <- distante()
  if (is.null(dis) || dis <= loc) return(FALSE)
  if (mode == "demander" && !isTRUE(demander(loc, dis))) return(FALSE)
  message(sprintf("HStat : mise \u00e0 jour %s -> %s...", loc, dis))
  ok <- installer()
  if (ok) message(sprintf("HStat %s install\u00e9e.", dis))
  ok
}
