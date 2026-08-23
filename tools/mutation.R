# =============================================================================
#  BANC DE MUTATION -- outil de developpement, pas du code d'application
# -----------------------------------------------------------------------------
#  Une assertion qui FIGE une tolerance ressemble exactement a une assertion qui
#  GARDE une regle. La relecture ne les distingue pas ; une regression
#  deliberee, si. On abime le code, on relance les tests, et on regarde si la
#  suite s'en apercoit. Une mutation qui PASSE est un trou dans les assertions.
#
#  C'est ainsi qu'a ete trouve le refus de pente negative qui refusait a tort :
#  le desactiver ne faisait echouer aucune assertion, ce qui a mene a regarder
#  ce qu'il gardait.
#
#  Usage, depuis la racine du depot :
#
#      Rscript tools/mutation.R                 # toute la suite
#      Rscript tools/mutation.R "DL50|probit"   # les tests dont le nom colle
#
#  Le filtre est ce qui rend la methode praticable : une passe ciblee prend une
#  vingtaine de secondes la ou la suite entiere en prend trois minutes, et il en
#  faut une par mutation.
#
#  Le banc SOURCE la vraie suite -- il ne reimplemente rien. Une premiere
#  version qui refaisait le chargement rapportait 24 echecs sur du code sain :
#  un banc de mesure qui ment est pire que pas de banc.
# =============================================================================

suppressMessages(library(testthat))

args   <- commandArgs(trailingOnly = TRUE)
motif  <- if (length(args) >= 1 && nzchar(args[1])) args[1] else NULL
fichier <- if (length(args) >= 2) args[2] else "tests/testthat/test-hstat.R"

if (!file.exists(fichier))
  stop("Suite introuvable : ", fichier, " (lancer depuis la racine du depot)")

# `test_that` est SUBSTITUE avant de sourcer la suite : le preambule du fichier
# s'execute normalement -- c'est lui qui definit les aides -- et seuls les tests
# retenus sont reellement joues.
vrai_test_that <- testthat::test_that
filtre <- function(desc, code) {
  if (!is.null(motif) && !grepl(motif, desc, perl = TRUE)) return(invisible(TRUE))
  invisible(tryCatch(withCallingHandlers(
      { vrai_test_that(desc, code); TRUE },
      expectation_failure = function(e) invokeRestart("muffleCondition")),
    error = function(e) FALSE))
}

rapporteur <- testthat::SilentReporter$new()
testthat::with_reporter(rapporteur, {
  assign("test_that", filtre, envir = globalenv())
  suppressWarnings(suppressMessages(source(fichier, local = FALSE, echo = FALSE)))
})

res <- rapporteur$expectations()
mauvais <- Filter(function(x)
  inherits(x, "expectation_failure") || inherits(x, "expectation_error"), res)

cat("ASSERTIONS:", length(res), " ECHECS:", length(mauvais), "\n")
if (length(mauvais)) {
  msgs <- unique(vapply(mauvais, function(x)
    sub("\n.*", "", conditionMessage(x)), character(1)))
  cat("  ", paste(utils::head(msgs, 8), collapse = "\n   "), "\n")
}
