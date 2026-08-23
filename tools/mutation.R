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

args    <- commandArgs(trailingOnly = TRUE)
fichier <- if (length(args) >= 2) args[2] else "tests/testthat/test-hstat.R"

if (!file.exists(fichier))
  stop("Suite introuvable : ", fichier, " (lancer depuis la racine du depot)")

# ---------------------------------------------------------------------------
#  L'ETAT DU BANC VIT DANS SON PROPRE ENVIRONNEMENT, PAS DANS `globalenv()`.
#
#  Ce n'est pas une precaution de style. `testthat::test_that()` evalue le corps
#  du test dans le CADRE DE SON APPELANT -- ici le filtre -- dont l'environnement
#  englobant etait `globalenv()`. Le banc y rangeait son motif sous le nom
#  `motif` ; un test qui ecrit `motif <- "..."` (le balayage des p-values le
#  fait) l'ECRASAIT en cours de passe. Le filtre se mettait alors a comparer les
#  descriptions suivantes a une expression reguliere de code R, qui ne colle a
#  aucune : tous les tests d'apres etaient SAUTES EN SILENCE.
#
#  Une passe filtree pouvait donc rendre « 0 echec » sans avoir joue le test qui
#  gardait la regle -- exactement le resultat qu'une mutation non detectee
#  produit. Le banc annoncait un trou dans les assertions la ou il n'y avait
#  qu'un trou dans le banc. Constate trois fois avant d'etre compris.
#
#  Le filtre se referme desormais sur `.banc`, un environnement que le code
#  teste ne peut pas nommer, et le decompte des tests RETENUS est affiche.
# ---------------------------------------------------------------------------
.banc <- new.env(parent = emptyenv())
.banc$motif   <- if (length(args) >= 1 && nzchar(args[1])) args[1] else NULL
.banc$vrai    <- testthat::test_that
.banc$retenus <- 0L
.banc$vus     <- 0L

# `test_that` est SUBSTITUE avant de sourcer la suite : le preambule du fichier
# s'execute normalement -- c'est lui qui definit les aides -- et seuls les tests
# retenus sont reellement joues.
filtre <- local(function(desc, code) {
  .banc$vus <- .banc$vus + 1L
  if (!is.null(.banc$motif) && !grepl(.banc$motif, desc, perl = TRUE))
    return(invisible(TRUE))
  .banc$retenus <- .banc$retenus + 1L
  invisible(tryCatch(withCallingHandlers(
      { .banc$vrai(desc, code); TRUE },
      expectation_failure = function(e) invokeRestart("muffleCondition")),
    error = function(e) FALSE))
}, list2env(list(.banc = .banc), parent = globalenv()))

rapporteur <- testthat::SilentReporter$new()
testthat::with_reporter(rapporteur, {
  assign("test_that", filtre, envir = globalenv())
  suppressWarnings(suppressMessages(source(fichier, local = FALSE, echo = FALSE)))
})

# UN FILTRE QUI NE RETIENT RIEN EST UNE ERREUR, PAS UN RESULTAT. Sans ce
# controle, une faute de frappe dans le motif rend « 0 echec » -- lu comme une
# mutation non detectee, donc comme un trou dans les assertions.
if (!is.null(.banc$motif) && .banc$retenus == 0L)
  stop("Le filtre \"", .banc$motif, "\" ne retient aucun des ",
       .banc$vus, " tests de la suite : verifier le motif.")

res <- rapporteur$expectations()
mauvais <- Filter(function(x)
  inherits(x, "expectation_failure") || inherits(x, "expectation_error"), res)

cat("TESTS:", .banc$retenus, "/", .banc$vus,
    " ASSERTIONS:", length(res), " ECHECS:", length(mauvais), "\n")
if (length(mauvais)) {
  msgs <- unique(vapply(mauvais, function(x)
    sub("\n.*", "", conditionMessage(x)), character(1)))
  cat("  ", paste(utils::head(msgs, 8), collapse = "\n   "), "\n")
}
