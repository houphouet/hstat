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
#' HStat can send an anonymous launch signal so that its spread can be
#' followed: the HStat version, the operating system family and the R version.
#' The country is deduced by the receiving service (GoatCounter) from the
#' connection, whose address is not stored. No identifier, file name, data or
#' result ever leaves the machine. HStat asks once, in an interactive
#' session; nothing is sent without that agreement, at most once a day.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'   (e.g. \code{port}). Passing \code{launch.browser} bypasses the choice
#'   of where to open.
#' @param ouverture Where to open the app. \code{NULL} (default) uses the
#'   stored default, or asks when there is none. \code{"navigateur"} opens in
#'   the web browser and \code{"shiny"} in the RStudio Shiny window, this time
#'   only. \code{"demander"} forgets the stored default and asks again.
#' @param statistiques \code{NULL} (default) uses the stored answer, or asks
#'   once in an interactive session. \code{TRUE} or \code{FALSE} records a
#'   new answer. \code{options(hstat.statistiques = FALSE)} turns the signal
#'   off whatever the stored answer.
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
run_hstat <- function(..., ouverture = NULL, statistiques = NULL,
                      install_missing = TRUE) {
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
  # dropdownMenu, pickerInput, ...) des le sourcage.
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

  if (.hstat_stats_consentement(statistiques)) .hstat_stats_envoyer()

  # runApp() sur un DOSSIER sert automatiquement le sous-dossier www/
  # (feuille de style + polices) et detecte app.R.
  do.call(shiny::runApp, c(list(app_dir), args))
}

# ---------------------------------------------------------------------------
# Ou ouvrir l'application : navigateur ou fenetre Shiny de RStudio
# ---------------------------------------------------------------------------
# La question se pose une fois, et la reponse peut devenir le defaut.

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


# ---------------------------------------------------------------------------
# Statistiques de diffusion, anonymes et consenties
# ---------------------------------------------------------------------------
# Un signal par machine et par jour au plus : version de HStat, famille du
# systeme, version de R. Le pays est deduit par GoatCounter, qui ne stocke pas
# l'adresse. Rien n'est envoye sans accord : la politique du CRAN l'interdit.

HSTAT_STATS_SITE <- "https://hstat.goatcounter.com"

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

.hstat_stats_ecrire <- function(accord, fichier) {
  if (!nzchar(fichier)) return(FALSE)
  isTRUE(tryCatch({
    dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
    writeLines(if (isTRUE(accord)) "oui" else "non", fichier)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE))
}

.hstat_stats_demander <- function(choix = utils::menu) {
  message("HStat peut envoyer, au plus une fois par jour, un signal anonyme de ",
          "lancement : version de HStat, syst\u00e8me et version de R. Le pays ",
          "est d\u00e9duit par le service de statistiques, qui ne conserve pas ",
          "l'adresse. Aucune donn\u00e9e, aucun r\u00e9sultat, aucun identifiant.")
  i <- choix(c("Oui, aider \u00e0 suivre la diffusion de HStat",
               "Non, ne rien envoyer"),
             title = "Statistiques anonymes de diffusion")
  if (!is.numeric(i) || length(i) != 1L || is.na(i) || !i %in% 1:2) return(NA)
  i == 1L
}

# Vrai seulement sur un accord explicite. L'option l'emporte sur tout ; une
# question annulee (0) ne vaut ni oui ni non : elle reviendra au lancement suivant.
.hstat_stats_consentement <- function(statistiques = NULL,
                                      interactif = interactive(),
                                      demander = .hstat_stats_demander,
                                      fichier = .hstat_stats_fichier()) {
  opt <- getOption("hstat.statistiques")
  if (isFALSE(opt)) return(FALSE)
  if (!is.null(statistiques)) {
    accord <- isTRUE(statistiques)
    .hstat_stats_ecrire(accord, fichier)
    message(if (accord) "Statistiques anonymes activ\u00e9es." else
              "Statistiques anonymes d\u00e9sactiv\u00e9es.",
            " Pour changer : run_hstat(statistiques = ", !accord, ")")
    return(accord)
  }
  if (isTRUE(opt)) return(TRUE)
  m <- .hstat_stats_lire(fichier)
  if (!is.na(m)) return(m)
  if (!isTRUE(interactif)) return(FALSE)
  r <- demander()
  if (is.na(r)) return(FALSE)
  .hstat_stats_ecrire(r, fichier)
  message("R\u00e9ponse m\u00e9moris\u00e9e. Pour changer : run_hstat(statistiques = ",
          !r, ")")
  r
}

.hstat_stats_systeme <- function(sys = Sys.info()[["sysname"]]) {
  s <- tolower(sys %||% "")
  if (grepl("windows", s)) "windows" else if (grepl("darwin", s)) "macos"
  else if (grepl("linux", s)) "linux" else "autre"
}

# L'adresse ne porte que des valeurs du paquet et du systeme : rien de saisi.
.hstat_stats_url <- function(version = hstat_version(),
                             systeme = .hstat_stats_systeme(),
                             site = getOption("hstat.statistiques.site",
                                              HSTAT_STATS_SITE)) {
  if (!is.character(site) || length(site) != 1L ||
      !grepl("^https://[A-Za-z0-9.-]+$", site)) return(NA_character_)
  version <- gsub("[^0-9.]", "", as.character(version))
  chemin <- sprintf("/lancement/%s/%s", version, systeme)
  sprintf("%s/count?p=%s&t=%s&rnd=%s", site,
          utils::URLencode(chemin, reserved = TRUE),
          utils::URLencode(paste("HStat", version), reserved = TRUE),
          paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))
}

# Une fois par jour et par machine : relancer HStat dix fois ne fait pas dix
# installations. L'envoi ne retarde jamais le lancement : il part en tache de
# fond (curl multi, relance par later) et tout echec est muet.
.hstat_stats_envoyer <- function(jour = format(Sys.Date()),
                                 fichier = .hstat_stats_fichier("statistiques_dernier.txt"),
                                 url = .hstat_stats_url()) {
  if (identical(Sys.getenv("TESTTHAT"), "true") || is.na(url)) return(invisible(FALSE))
  if (!requireNamespace("curl", quietly = TRUE) ||
      !requireNamespace("later", quietly = TRUE)) return(invisible(FALSE))
  if (nzchar(fichier) && file.exists(fichier) &&
      identical(tryCatch(readLines(fichier, n = 1L, warn = FALSE),
                         error = function(e) ""), jour)) return(invisible(FALSE))
  tryCatch({
    h <- curl::new_handle(timeout = 10, connecttimeout = 5, followlocation = FALSE,
                          useragent = sprintf("HStat/%s (R %s; %s)",
                                              hstat_version(),
                                              paste(R.version$major, R.version$minor, sep = "."),
                                              .hstat_stats_systeme()))
    pool <- curl::new_pool()
    curl::curl_fetch_multi(url, handle = h, pool = pool,
      done = function(r) if (r$status_code < 400 && nzchar(fichier)) {
        dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
        try(writeLines(jour, fichier), silent = TRUE)
      },
      fail = function(e) NULL)
    tours <- 0L
    pousser <- function() {
      tours <<- tours + 1L
      reste <- tryCatch(curl::multi_run(timeout = 0, pool = pool)$pending,
                        error = function(e) 0L)
      if (reste > 0L && tours < 60L) later::later(pousser, 0.25)
    }
    later::later(pousser, 0)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}


# Liste des dependances requises, lue depuis inst/app/Utils.R (source unique de
# verite) pour rester synchronisee avec l'application.
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
