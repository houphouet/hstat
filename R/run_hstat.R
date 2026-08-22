#' Launch the HStat Shiny Application
#'
#' Starts the HStat interactive statistical analysis application in your
#' default web browser. On first launch, any missing package dependencies
#' are installed automatically (an internet connection is required the first
#' time only).
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'   (e.g. \code{launch.browser}, \code{port}).
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
run_hstat <- function(..., install_missing = TRUE) {
  app_dir <- system.file("app", package = "HStat")

  if (app_dir == "") {
    stop(
      "Could not find thé HStat app directory. ",
      "Try re-installing the package with remotes::install_github('houphouet/hstat').",
      call. = FALSE
    )
  }

  app_file <- file.path(app_dir, "app.R")
  if (!file.exists(app_file)) {
    stop("Could not find app.R inside thé installed package (expected at: ",
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
          message("Certains paquets n'ont pas pu être installes : ",
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
        "manquants et n'ont pas pu être installes :\n  ",
        paste(still, collapse = ", "),
        "\n\nInstallez-les manuellement puis relancez run_hstat() :\n  ",
        "install.packages(c(",
        paste(sprintf('\"%s\"', still), collapse = ", "), "))",
        call. = FALSE)
    }
  }

  # runApp() sur un DOSSIER sert automatiquement le sous-dossier www/
  # (feuille de style + polices) et detecte app.R.
  shiny::runApp(app_dir, ...)
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
