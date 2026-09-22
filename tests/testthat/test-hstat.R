# =============================================================================
#  HStat -- Tests automatises (testthat)
#
#  Comment lancer, depuis la RACINE du depot :
#    1. Installer testthat :  install.packages("testthat")
#    2. testthat::test_dir("tests/testthat")
#    ou pour ce seul fichier :
#         testthat::test_file("tests/testthat/test-hstat.R")
#
#  Viser "tests/testthat" et non "tests" : testthat traite tout fichier dont le
#  nom commence par "test" comme une suite, y compris tests/testthat.R, dont le
#  library(HStat) echoue tant que le paquet n'est pas installe.
#  R CMD check, lui, passe bien par tests/testthat.R apres installation.
#
#  Ces tests couvrent les FONCTIONS DE CALCUL et utilitaires de Utils.R :
#  detection de format, formatage, moteur de donnees (chemin memoire),
#  agregations SQL (si duckdb dispo), graine reproductible.
#  Ils ne lancent pas l'application Shiny (pas de UI/serveur).
# =============================================================================

library(testthat)

# `shiny` est ATTACHE explicitement. Le socle (`R/utils.R`) appelle des
# fonctions de Shiny sans prefixe -- il vient de l'application, ou Shiny est
# attache. Tant que les tests sourcaient l'ancien `Utils.R`, l'attachement
# arrivait par effet de bord de `install_and_load()` ; le socle etant desormais
# inerte, cet effet a disparu et un test dependait donc de l'ORDRE d'execution
# des autres. Le dire ici vaut mieux que de le subir.
if (isTRUE(requireNamespace("shiny", quietly = TRUE)))
  suppressMessages(library(shiny))

# -- Charger le code du paquet (sans demarrer l'app) -------------------------
# Le socle et TOUS les modules vivent dans `R/`. On les source dans un
# environnement isole : ce sont des definitions, rien ne s'execute.
#
# Le dossier est balaye, les fichiers ne sont plus nommes un a un. C'est le
# gain de la migration, et il vaut pour les tests comme pour l'application :
# avant, quatre modules etaient charges par leur nom (mod_ai,
# mod_coding, mod_design) et un cinquieme qui aurait porte une fonction de
# calcul serait reste invisible -- les tests le concernant auraient echoue sur
# « could not find function », loin de la cause.
#
# install_and_load() est neutralise : il tenterait d'installer des paquets.
local({
  socle_cands <- c(
    file.path("R", "utils.R"),                              # racine du depot
    file.path("..", "R", "utils.R"),                        # depuis tests/
    file.path("..", "..", "R", "utils.R"),                  # depuis tests/testthat/
    file.path("..", "..", "..", "R", "utils.R"))
  socle_path <- socle_cands[file.exists(socle_cands)][1]

  e <- new.env()
  assign("install_and_load", function(...) invisible(NULL), envir = e)
  if (!is.na(socle_path)) {
    # `utils.R` d'abord, puis le reste par ordre alphabetique. L'ordre n'a plus
    # d'importance de fond -- aucun module n'agit au chargement -- mais le fixer
    # rend un echec reproductible. `run_hstat.R`, `zzz.R` et
    # `_disable_autoload.R` appartiennent au paquet, pas a l'application.
    dossier <- dirname(socle_path)
    a_part <- c("utils.R", "run_hstat.R", "zzz.R", "_disable_autoload.R")
    autres <- setdiff(basename(list.files(dossier, pattern = "[.][Rr]$")), a_part)
    for (f in c("utils.R", sort(autres)))
      suppressWarnings(suppressMessages(
        sys.source(file.path(dossier, f), envir = e, keep.source = FALSE)))
  } else if (isTRUE(requireNamespace("HStat", quietly = TRUE))) {
    # Paquet installe (R CMD check) : les definitions viennent de l'espace de
    # noms, y compris les aides internes que les tests exercent directement.
    ns <- asNamespace("HStat")
    for (nm in setdiff(ls(ns, all.names = TRUE),
                       c(".__NAMESPACE__.", ".__S3MethodsTable__.", ".packageName")))
      assign(nm, get(nm, envir = ns), envir = e)
  } else {
    stop("Impossible de localiser le socle (R/utils.R) depuis ", getwd())
  }
  # Exporter TOUTES les fonctions (y compris cachees, ex. .hstat_sql_stat_exprs)
  for (nm in ls(e, all.names = TRUE))
    assign(nm, get(nm, envir = e), envir = globalenv())
})

# -- Poser les aiguillages, UNE FOIS, comme le fait le pont au demarrage ------
# Les modules appellent `withSpinner`, `pickerInput`, `element_markdown`... sans
# prefixe : ce sont des AIGUILLAGES, definis soit vers le paquet optionnel soit
# vers un equivalent de base. Sans eux, tout test qui construit une interface ou
# trace un graphique de module leve « could not find function ».
#
# Sept tests les posaient chacun de leur cote. Comme ils ecrivent dans
# `globalenv()`, le PREMIER a s'executer servait tous les suivants -- donc un
# test dependait de l'ordre des autres, exactement ce que l'amorce ci-dessus
# dit d'eviter pour l'attachement de shiny. On le pose ici, au meme endroit et
# pour la meme raison.
if (exists("hstat_installer_replis_ui"))
  suppressMessages(hstat_installer_replis_ui())

# -- Racine du depot (pour les tests portant sur app.R et R/) ----------------
# Renvoie NA quand les tests tournent depuis un paquet installe, ou app.R et le
# dossier R/ n'existent plus : les tests concernes s'y skippent d'eux-memes.
.hstat_repo_root <- function(requis = TRUE) {
  cands <- c(".", "..", file.path("..", ".."), file.path("..", "..", ".."))
  hit <- cands[file.exists(file.path(cands, "DESCRIPTION")) &
               dir.exists(file.path(cands, "inst", "app"))]
  if (length(hit)) return(normalizePath(hit[1]))
  # HORS DU DEPOT -- typiquement sous `R CMD check`, qui travaille sur le
  # paquet INSTALLE : `inst/app/` y est aplati en `app/` et `R/` a disparu.
  # Les tests qui BALAIENT LES SOURCES n'ont alors rien a lire.
  #
  # Le saut est pose ICI, pas dans chacun d'eux. Vingt et un des quatre-vingts
  # tests concernes n'avaient pas de garde : sous `R CMD check` ils ne
  # trouvaient aucun fichier, concluaient a l'absence de ce qu'ils cherchaient,
  # et ECHOUAIENT -- 48 echecs pour une seule cause, et pas une seule vraie.
  # Garder la liste a jour a la main l'aurait fait deriver au premier test
  # ajoute ; le localisateur, lui, est traverse par tous.
  if (isTRUE(requis))
    testthat::skip("hors du depot : les sources ne sont pas disponibles")
  NA_character_
}

# -- Chemin du SOCLE ---------------------------------------------------------
# Les definitions ont quitte `inst/app/Utils.R` pour `R/utils.R`, ou elles
# forment le code du paquet. Les tests qui lisent une definition doivent donc
# viser le socle ; ceux qui lisent un effet de bord de demarrage visent le pont.
# Un seul localisateur, sinon la distinction se perdra au prochain test ajoute.
.hstat_socle_path <- function() {
  root <- .hstat_repo_root()
  if (is.na(root)) return(NA_character_)
  p <- file.path(root, "R", "utils.R")
  if (file.exists(p)) p else NA_character_
}

# Lignes de code d'un fichier R, COMMENTAIRES RETIRES par l'analyseur de R.
# Une heuristique (« tout ce qui suit un # ») se signalerait elle-meme sur les
# commentaires qui documentent le defaut recherche, et un faux positif permanent
# finit toujours par faire desactiver le test.
#
# Ce decoupage etait recopie a l'identique dans deux balayages ; il n'existe
# desormais qu'ici -- c'est la meme regle que celle appliquee a l'application.
# -- Toutes les sources de l'application -------------------------------------
# La migration vers le paquet deplace les modules un a un : le socle et les
# modules migres vivent dans `R/`, les autres encore dans `inst/app/`. Un
# balayage qui n'enumererait qu'un seul dossier CESSERAIT DE VOIR le code migre
# -- et passerait au vert en ne regardant plus rien. Tous les balayages passent
# donc par ici.
.hstat_sources_app <- function() {
  root <- .hstat_repo_root()
  if (is.na(root)) return(character(0))
  # `list.files()` et non `.hstat_sources_app()` : la substitution mecanique qui a
  # recale les balayages avait touche CE CORPS, et la fonction s'appelait
  # elle-meme -- « C stack usage is too close to the limit ». Meme piege que
  # celui documente pour les reactifs de l'application.
  c(list.files(file.path(root, "inst", "app"), pattern = "[.]R$", full.names = TRUE),
    file.path(root, "R", "utils.R"),
    list.files(file.path(root, "R"), pattern = "^mod_.*[.]R$", full.names = TRUE))
}

# -- Tous les noms DEFINIS dans un fichier ------------------------------------
# A n'importe quelle profondeur, parametres formels compris. Se limiter au
# premier niveau (`ex[[i]][[2]]`) laissait passer les aides internes des
# modules -- `id <- function(s) ns(...)` dans le corps d'une fonction d'UI --
# et un balayage les prenait alors pour des fonctions de paquet.
.hstat_noms_definis <- function(f) {
  ex <- tryCatch(parse(f), error = function(e) NULL)
  if (is.null(ex)) return(character(0))
  acc <- character(0)
  vide <- function(l, i) identical(l[[i]], quote(expr = ))
  rec <- function(x) {
    if (!is.call(x)) return(invisible())
    tete <- x[[1]]
    if (is.name(tete)) {
      nm <- as.character(tete)
      if (nm %in% c("<-", "=", "<<-") && is.name(x[[2]]))
        acc <<- c(acc, as.character(x[[2]]))
      if (nm == "function") acc <<- c(acc, names(as.list(x[[2]])))
    }
    l <- as.list(x)
    for (i in seq_along(l)) if (!vide(l, i)) rec(l[[i]])
  }
  for (e in ex) rec(e)
  unique(acc)
}

# -- Chemin d'un fichier de MODULE -------------------------------------------
# La migration vers le paquet se fait module par module : `mod_tests.R` vit
# desormais dans `R/`, les autres encore dans `inst/app/`. Les tests qui lisent
# un module doivent donc le CHERCHER, pas presumer de son dossier -- sinon
# chaque migration casserait une poignee de tests sans rapport avec elle.
.hstat_module_path <- function(nom) {
  root <- .hstat_repo_root()
  if (is.na(root)) return(NA_character_)
  cands <- c(file.path(root, "R", nom),
             file.path(root, "R", tolower(nom)),
             file.path(root, "inst", "app", nom))
  hit <- cands[file.exists(cands)]
  if (length(hit)) hit[1] else NA_character_
}

.hstat_code_lignes <- function(f) {
  lignes <- readLines(f, warn = FALSE, encoding = "UTF-8")
  pd <- tryCatch(utils::getParseData(parse(f, keep.source = TRUE)),
                 error = function(e) NULL)
  if (!is.null(pd)) {
    com <- pd[pd$token == "COMMENT", , drop = FALSE]
    for (i in seq_len(nrow(com))) {
      l <- com$line1[i]
      if (l >= 1 && l <= length(lignes))
        lignes[l] <- substr(lignes[l], 1, max(0, com$col1[i] - 1))
    }
  }
  lignes
}

# Le chargement separe de `mod_qualitative.R` a disparu : le balayage de `R/`
# ci-dessus le prend comme les autres. Il cherchait le fichier a cote du
# repertoire courant -- un chemin qui n'a plus de sens depuis que les modules
# sont dans le paquet, et qui aurait laisse les hstat_q_* introuvables sans dire
# pourquoi.


# =============================================================================
# --------------------------------------------------------------------------
#  Détection du type de fichier
# --------------------------------------------------------------------------
# =============================================================================

test_that("hstat_file_kind reconnait les extensions courantes", {
  expect_equal(hstat_file_kind("data.csv"),      "csv")
  expect_equal(hstat_file_kind("data.CSV"),      "csv")   # insensible a la casse
  expect_equal(hstat_file_kind("data.txt"),      "csv")
  expect_equal(hstat_file_kind("data.tsv"),      "csv")
  expect_equal(hstat_file_kind("classeur.xlsx"), "excel")
  expect_equal(hstat_file_kind("classeur.xls"),  "excel")
  expect_equal(hstat_file_kind("tab.parquet"),   "parquet")
  expect_equal(hstat_file_kind("base.duckdb"),   "duckdb")
  expect_equal(hstat_file_kind("enq.sav"),       "sav")
  expect_equal(hstat_file_kind("enq.dta"),       "dta")
  expect_equal(hstat_file_kind("obj.rds"),       "rds")
})

test_that("hstat_file_kind renvoie 'inconnu' pour le reste", {
  expect_equal(hstat_file_kind("image.png"), "inconnu")
  expect_equal(hstat_file_kind("archive.zip"), "inconnu")
  expect_equal(hstat_file_kind("sansextension"), "inconnu")
})


# =============================================================================
# --------------------------------------------------------------------------
#  Formatage des tailles de fichier
# --------------------------------------------------------------------------
# =============================================================================

test_that("hstat_format_size formate correctement les ordres de grandeur", {
  expect_equal(hstat_format_size(0),          "0 o")
  expect_equal(hstat_format_size(-5),         "0 o")     # garde-fou
  expect_equal(hstat_format_size(NA),         "0 o")
  expect_match(hstat_format_size(1024),       "Ko")
  expect_match(hstat_format_size(1048576),    "Mo")
  expect_match(hstat_format_size(1073741824), "Go")
})

test_that("hstat_format_size donne une valeur numérique plausible", {
  expect_equal(hstat_format_size(1536), "1.5 Ko")   # 1.5 * 1024
})


# =============================================================================
# --------------------------------------------------------------------------
#  Chemin SQL (echappement)
# --------------------------------------------------------------------------
# =============================================================================

test_that("hstat_sql_path normalise les antislash et echappe les apostrophes", {
  expect_equal(hstat_sql_path("C:\\data\\f.csv"), "C:/data/f.csv")
  expect_equal(hstat_sql_path("a'b.csv"),         "a''b.csv")
  expect_equal(hstat_sql_path("/home/u/f.csv"),   "/home/u/f.csv")
})


# =============================================================================
# --------------------------------------------------------------------------
#  Graine reproductible
# --------------------------------------------------------------------------
# =============================================================================

test_that("hstat_set_seed rend les tirages reproductibles", {
  hstat_set_seed(42); a <- runif(5)
  hstat_set_seed(42); b <- runif(5)
  expect_identical(a, b)
})

test_that("hstat_set_seed retombe sur la graine par défaut si entree invalide", {
  s1 <- hstat_set_seed(NULL)
  expect_equal(s1, HSTAT_DEFAULT_SEED)
  s2 <- hstat_set_seed(NA)
  expect_equal(s2, HSTAT_DEFAULT_SEED)
})

test_that("deux graines differentes produisent des tirages differents", {
  hstat_set_seed(1); a <- runif(10)
  hstat_set_seed(2); b <- runif(10)
  expect_false(isTRUE(all.equal(a, b)))
})


# =============================================================================
# --------------------------------------------------------------------------
#  Chargeur de données -- chemin en memoire (CSV)
# --------------------------------------------------------------------------
# =============================================================================

test_that("hstat_load_data lit un CSV en memoire et renseigne les metadonnees", {
  set.seed(1)
  df <- data.frame(
    g = sample(c("A", "B", "C"), 200, replace = TRUE),
    x = rnorm(200),
    y = runif(200)
  )
  df$x[c(3, 50, 120)] <- NA
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  res <- hstat_load_data(tmp, kind = "csv", header = TRUE, sep = ",")

  expect_equal(res$mode, "memory")
  expect_true(is.data.frame(res$data))
  expect_equal(res$full_nrow, 200)
  expect_equal(res$full_ncol, 3)
  expect_equal(res$full_na, 3)          # 3 NA inseres
  expect_false(res$is_sampled)
  expect_null(res$con)
})

test_that("le seuil hors-memoire n'affecte pas un petit CSV", {
  df  <- data.frame(a = 1:50, b = letters[1:50 %% 26 + 1])
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  res <- hstat_load_data(tmp, kind = "csv", threshold = 1e12)
  expect_equal(res$mode, "memory")
  expect_equal(res$full_nrow, 50)
})


# =============================================================================
# --------------------------------------------------------------------------
#  Expressions SQL d'agregation
# --------------------------------------------------------------------------
# =============================================================================

test_that(".hstat_sql_stat_exprs génère les expressions attendues", {
  ex <- .hstat_sql_stat_exprs("Rendement", c("mean", "sd", "min", "max"))
  expect_true(all(c("mean", "sd", "min", "max") %in% names(ex)))
  expect_match(ex[["mean"]], "AVG")
  expect_match(ex[["sd"]],   "STDDEV_SAMP")
  expect_match(ex[["min"]],  "MIN")
  expect_match(ex[["max"]],  "MAX")
  # Les noms de colonnes sont entre guillemets doubles (securite SQL)
  expect_match(ex[["mean"]], '"Rendement"')
})

test_that(".hstat_sql_stat_exprs ne retourne que les stats demandees", {
  ex <- .hstat_sql_stat_exprs("x", c("median"))
  expect_equal(names(ex), "median")
  expect_match(ex[["median"]], "MEDIAN")
})


# =============================================================================
# --------------------------------------------------------------------------
#  Agregations exactes via DuckDB (si disponible)
# --------------------------------------------------------------------------
# =============================================================================

test_that("describe_global DuckDB == calcul de référence R", {
  skip_if_not(hstat_has_duckdb(), "duckdb non installe")

  set.seed(7)
  df <- data.frame(grp = sample(c("A", "B"), 5000, replace = TRUE),
                   x = rnorm(5000, 10, 5))
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  con <- hstat_duckdb_connect()
  on.exit(hstat_duckdb_close(con), add = TRUE)
  tbl <- hstat_duckdb_register(con, tmp, "csv", header = TRUE, sep = ",")

  out <- hstat_duckdb_describe_global(con, tbl, "x", c("mean", "sd", "min", "max"))

  expect_equal(out$mean[1], mean(df$x),  tolerance = 1e-6)
  expect_equal(out$sd[1],   sd(df$x),    tolerance = 1e-6)
  expect_equal(out$min[1],  min(df$x),   tolerance = 1e-6)
  expect_equal(out$max[1],  max(df$x),   tolerance = 1e-6)
})

test_that("crosstab DuckDB == table() de référence", {
  skip_if_not(hstat_has_duckdb(), "duckdb non installe")

  set.seed(3)
  df <- data.frame(r = sample(c("A", "B", "C"), 4000, replace = TRUE),
                   c = sample(c("X", "Y"), 4000, replace = TRUE))
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  con <- hstat_duckdb_connect()
  on.exit(hstat_duckdb_close(con), add = TRUE)
  tbl <- hstat_duckdb_register(con, tmp, "csv", header = TRUE, sep = ",")

  ct_duck <- hstat_duckdb_crosstab(con, tbl, "r", "c")
  ct_ref  <- table(df$r, df$c)

  # Memes effectifs (apres alignement des dimensions)
  expect_equal(sum(ct_duck), sum(ct_ref))
  expect_equal(as.numeric(ct_duck["A", "X"]),
               as.numeric(ct_ref["A", "X"]))
})

test_that("corrélation DuckDB == cor() de référence (Pearson)", {
  skip_if_not(hstat_has_duckdb(), "duckdb non installe")

  set.seed(11)
  n <- 5000
  df <- data.frame(a = rnorm(n))
  df$b <- df$a * 0.7 + rnorm(n, 0, 0.5)
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  con <- hstat_duckdb_connect()
  on.exit(hstat_duckdb_close(con), add = TRUE)
  tbl <- hstat_duckdb_register(con, tmp, "csv", header = TRUE, sep = ",")

  m <- hstat_duckdb_cor(con, tbl, c("a", "b"))
  expect_equal(m["a", "b"], cor(df$a, df$b), tolerance = 1e-6)
  expect_equal(diag(m), c(a = 1, b = 1))
})

test_that("le sous-echantillonnage DuckDB respecte la taille demandee", {
  skip_if_not(hstat_has_duckdb(), "duckdb non installe")

  df  <- data.frame(x = 1:20000)
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)

  con <- hstat_duckdb_connect()
  on.exit(hstat_duckdb_close(con), add = TRUE)
  tbl <- hstat_duckdb_register(con, tmp, "csv", header = TRUE, sep = ",")

  hstat_set_seed(1)
  smp <- hstat_duckdb_sample(con, tbl, n = 1000)
  expect_lte(nrow(smp), 1000)
  expect_gt(nrow(smp), 0)

  # Si l'echantillon demande depasse le total, on recupere tout
  smp_all <- hstat_duckdb_sample(con, tbl, n = 99999)
  expect_equal(nrow(smp_all), 20000)
})

# ---- Tests du module d'analyses qualitatives -----------------------------
test_that("Détection de type qualitatif", {
  expect_equal(hstat_q_detect_type(c("Homme","Femme","Homme")), "nominale")
  expect_equal(hstat_q_detect_type(factor(c("Bas","Moyen","Haut"), ordered=TRUE)), "ordinale")
})

test_that("l'échelle accentuée est reconnue, la coïncidence de mot ne l'est pas", {
  # LES ACCENTS SONT DEPLIES DES DEUX COTES. Les motifs l'etaient, pas les
  # modalites : une echelle ecrite « Élevé / Énormément » -- soit le francais
  # correct -- ne rencontrait aucun motif et passait pour NOMINALE. Elle
  # perdait alors son ordre, donc la mediane, les quartiles et tout test de
  # tendance. La regression est silencieuse : rien ne signale un type qui
  # aurait pu etre meilleur.
  expect_equal(hstat_q_detect_type(rep(c("Nul", "Élevé", "Énormément"), 4)),
               "ordinale")

  # LE SEUIL EST DE DEUX MOTS-CLES, et c'est ce qui rend la detection sure.
  # Le depliage des accents rapproche « Élève » (l'ecolier) de « eleve »
  # (le niveau) : une colonne de PROFESSIONS rencontre donc un motif, et un
  # seul. A un seul mot-cle, elle serait declaree ordinale et l'application
  # ordonnerait des metiers.
  expect_equal(hstat_q_detect_type(
                 rep(c("Agriculteur", "Commerçant", "Fonctionnaire", "Élève"), 4)),
               "nominale")
  expect_equal(hstat_q_detect_type(rep(c("Rouge", "Vert", "Bleu"), 4)), "nominale")
})

test_that("une case nulle est corrigée, jamais laissée produire un OR de zéro", {
  # Sans la correction de Haldane-Anscombe (+0,5), une case a zero donne
  # 1/0 = Inf sous la racine : l'OR tombe a 0 et sa borne HAUTE devient NaN.
  # Un intervalle a moitie indefini est pire qu'un refus -- il s'affiche.
  r <- hstat_q_or_rr_2x2(0, 12, 9, 11)
  expect_true(r$corrected)
  expect_true(all(is.finite(c(r$or, r$or_lo, r$or_hi))))
  expect_gt(r$or, 0)
  expect_true(all(is.finite(c(r$rr, r$rr_lo, r$rr_hi))))
  # `corrected` doit dire ce qui a REELLEMENT ete fait : il annoncait la
  # correction meme quand elle n'etait plus appliquee.
  expect_equal(unname(r$cells), c(0.5, 12.5, 9.5, 11.5))
  # Aucune case nulle : rien n'est ajoute, et on ne l'annonce pas.
  s <- hstat_q_or_rr_2x2(5, 12, 9, 11)
  expect_false(s$corrected)
  expect_equal(unname(s$cells), c(5, 12, 9, 11))
})

test_that("Analyse nominale univariee", {
  set.seed(1); x <- sample(c("A","B","C"), 60, replace=TRUE)
  r <- hstat_q_nominal_univariate(x, "Var")
  expect_true(r$ok)
  expect_true(any(grepl("Shannon", r$metrics$Metrique)))
  expect_true("Tableau de fréquences" %in% names(r$tables))
})

test_that("Analyse nominale bivariee produit V de Cramér", {
  set.seed(2); x <- sample(c("A","B"), 80, replace=TRUE); y <- sample(c("X","Y","Z"), 80, replace=TRUE)
  r <- hstat_q_nominal_bivariate(x, y)
  expect_true(r$ok)
  expect_true(any(grepl("Cramér", r$metrics$Metrique)))
})

test_that("Choix multiples : formats binaire et séparé", {
  set.seed(3)
  dfb <- as.data.frame(matrix(rbinom(50*3,1,.5), ncol=3)); names(dfb) <- c("O1","O2","O3")
  rb <- hstat_q_multiple_choice(dfb, cols=c("O1","O2","O3"))
  expect_true(rb$ok)
  dfs <- data.frame(c = c("A;B","B;C","A","A;B;C"), stringsAsFactors=FALSE)
  rs <- hstat_q_multiple_choice(dfs, sep_col="c")
  expect_true(rs$ok)
})

test_that("Échelle de Likert et alpha de Cronbach", {
  set.seed(4); lat <- sample(1:5, 100, replace=TRUE)
  items <- as.data.frame(lapply(1:4, function(i) pmax(1,pmin(5, lat + sample(-1:1,100,replace=TRUE)))))
  names(items) <- paste0("Q",1:4)
  r <- hstat_q_likert_scale(items, levels_order=1:5)
  expect_true(r$ok)
  alpha_row <- r$metrics$Valeur[r$metrics$Metrique=="Alpha de Cronbach"]
  expect_true(as.numeric(alpha_row) > 0.7)  # items correles -> alpha eleve
})

test_that("Analyse textuelle et thématique", {
  set.seed(5)
  txt <- sample(c("le service est rapide et efficace personnel competent",
                  "prix trop cher pour la qualite vraiment decevant",
                  "produit de bonne qualite je recommande vivement"), 60, replace=TRUE)
  r <- hstat_q_text_analysis(txt, n_topics=2)
  expect_true(r$ok)
  expect_true("Fréquences des mots" %in% names(r$tables))
  expect_true(any(grepl("TF-IDF|TFIDF", names(r$tables))))
})


# =============================================================================
# --------------------------------------------------------------------------
#  Sécurité -- évaluateur de formules et identifiants SQL
# --------------------------------------------------------------------------
# =============================================================================

test_that("hstat_safe_eval évalue les formules légitimes", {
  d <- data.frame(`Var A` = c(1, 2, 3), B = c(10, 20, 30), check.names = FALSE)
  expect_equal(hstat_safe_eval("`Var A` + B * 2", d), c(21, 42, 63))
  expect_equal(hstat_safe_eval("ifelse(B > 15, 1, 0)", d), c(0, 1, 1))
  expect_equal(hstat_safe_eval("rowMeans(cbind(`Var A`, B), na.rm = TRUE)", d),
               c(5.5, 11, 16.5))
  expect_equal(hstat_safe_eval("log(B)", d), log(c(10, 20, 30)))
})

test_that("hstat_safe_eval bloque le code arbitraire (RCE)", {
  d <- data.frame(B = 1:3)
  expect_error(hstat_safe_eval('system("id")', d))
  expect_error(hstat_safe_eval('base::system("id")', d))
  expect_error(hstat_safe_eval('file.remove("x")', d))
  expect_error(hstat_safe_eval('eval(parse(text = "1+1"))', d))
  expect_error(hstat_safe_eval('(function(x) x)(1)', d))
  expect_error(hstat_safe_eval('assign("x", 1)', d))
  expect_error(hstat_safe_eval('1 + 1; system("id")', d))
  expect_error(hstat_safe_eval('get("system")("id")', d))
  expect_error(hstat_safe_eval('do.call("system", list("id"))', d))

  # Vecteurs d'evasion supplementaires, tous verifies un par un. Chacun est une
  # facon differente d'atteindre une fonction : par le nom cite, par l'espace
  # de noms, par l'environnement, par le detour d'une fonction d'ordre
  # superieur, ou par une structure de controle.
  for (f in c('`system`("id")',            # nom entre accents graves
              'base:::system("id")',       # espace de noms interne
              '.GlobalEnv$q',              # `$` sur un environnement
              'globalenv()[["q"]]',        # `[[` sur un environnement
              'match.fun("system")("id")', # resolution differee
              'Recall()',                  # rappel de l appelant
              'sapply(1, get)',            # fonction passee en valeur
              'lapply(c(1), function(i) i)',
              'Reduce(`+`, c(B))',
              'quote(system("id"))',       # expression non evaluee
              '~system("id")',             # formule
              'structure(1, class = "x")', # fabrique d objet
              'attr(B, "x")',
              'body(mean)',
              'while (TRUE) 1',            # structures de controle
              'for (i in 1:2) i',
              '{ system("id") }',
              'x <- 1'))
    expect_error(hstat_safe_eval(f, d), info = f)
})

test_that("la liste blanche des formules ne laisse entrer aucune porte", {
  # LE RISQUE N'EST PAS LE CODE D'AUJOURD'HUI, C'EST L'AJOUT DE DEMAIN. Le bac
  # a sable tient parce que `HSTAT_FORMULA_FUNS` ne contient que des fonctions
  # closes sur leurs arguments. Y glisser une seule fonction d'ordre superieur
  # -- `sapply`, `do.call`, `Reduce` -- ou une fonction qui resout un nom
  # -- `get`, `match.fun`, `eval` -- rouvrirait l'execution de code arbitraire
  # depuis un simple champ de texte, et rien ne le signalerait.
  portes <- c("eval", "evalq", "parse", "str2lang", "str2expression", "quote",
              "bquote", "substitute", "get", "get0", "mget", "match.fun",
              "assign", "do.call", "Recall", "Reduce", "Filter", "Map",
              "lapply", "sapply", "vapply", "mapply", "apply", "outer",
              "system", "system2", "shell", "source", "sys.function",
              "environment", "globalenv", "emptyenv", "baseenv", "new.env",
              "as.environment", "list2env", "attach", "library", "require",
              "loadNamespace", "asNamespace", "getExportedValue",
              "file", "url", "readRDS", "saveRDS", "load", "save",
              "readLines", "writeLines", "unlink", "file.remove",
              "install.packages", "download.file", "Sys.setenv", "Sys.getenv",
              "body", "formals", "args", "structure", "attr", "attributes",
              "class", "oldClass", "unclass", "on.exit", "trace", "debug",
              "$", "[[", "[", "@", "::", ":::", "<-", "<<-", "=", "->",
              "function", "if", "for", "while", "repeat", "{", "return")
  for (f in portes)
    expect_false(f %in% HSTAT_FORMULA_FUNS, info = f)

  # Et l'evaluation reste CLOSE : l'environnement des fonctions autorisees a
  # `emptyenv()` pour parent, si bien qu'un nom absent des donnees et de la
  # liste blanche ne peut pas etre resolu par le chemin de recherche.
  d <- data.frame(B = 1:3)
  expect_error(hstat_safe_eval("pi", d))
  expect_error(hstat_safe_eval("T", d))
  expect_error(hstat_safe_eval("letters", d))
  # Les fonctions autorisees, elles, restent atteignables.
  expect_equal(hstat_safe_eval("sqrt(B)", d), sqrt(1:3))
})

test_that("la détection d'outliers juge la singularité sur le rang, pas le déterminant", {
  # LE DEFAUT, MESURE. Le determinant d'une covariance est homogene a la
  # p-ieme puissance d'une unite : cinq variables mesurees en microgrammes le
  # font tomber a 8e-60 alors que le rang reste plein. Le seuil
  # `det < .Machine$double.eps` criait donc a la singularite sur des donnees
  # parfaitement inversibles, et la detection s'arretait -- en accusant les
  # donnees. C'est la meme correction que dans `box_m_test()` ; elle manquait
  # ici, dans la fonction voisine.
  set.seed(1)
  n <- 60L; p <- 5L
  Y <- matrix(stats::rnorm(n * p), n, p)
  Y[1, ] <- Y[1, ] + 12                      # un outlier franc

  usuel <- detect_multivariate_outliers(Y)
  micro <- detect_multivariate_outliers(Y * 1e-6)

  # LA DISTANCE DE MAHALANOBIS EST INVARIANTE PAR CHANGEMENT D'ECHELLE : les
  # deux appels portent sur les memes observations, ils doivent donner le meme
  # resultat. C'est ce qui rend le defaut indiscutable.
  expect_equal(length(usuel$idx_outliers), 1L)
  expect_identical(micro$idx_outliers, usuel$idx_outliers)
  expect_equal(micro$d2, usuel$d2)
  expect_false(grepl("singuli", micro$conclusion))

  # Le determinant, lui, s'effondre : c'est bien lui qui mentait, pas le rang.
  expect_lt(det(stats::cov(Y * 1e-6)), .Machine$double.eps)
  expect_equal(qr(stats::cov(Y * 1e-6))$rank, p)

  # ET UNE VRAIE SINGULARITE RESTE REFUSEE. Sans cette moitie, le test
  # passerait aussi sur une fonction qui aurait simplement retire le garde-fou.
  Z <- cbind(Y, Y[, 1])
  expect_match(detect_multivariate_outliers(Z)$conclusion, "singuli")
  expect_equal(length(detect_multivariate_outliers(Z)$idx_outliers), 0L)

  # Un echantillon trop petit se dit, il ne se calcule pas.
  expect_match(detect_multivariate_outliers(Y[1:3, ])$conclusion, "trop petit")
})

test_that("un nom de colonne n'entre jamais tel quel dans une notification HTML", {
  # UNE COLONNE « Rendement <2023> » PERD SON MILLESIME. Les chevrons sont lus
  # comme une balise : la notification annonce un nom qui n'est pas celui de
  # l'utilisateur, et rien ne le signale. Le « & » d'un « Masse & surface »
  # ressort de meme corrompu.
  expect_equal(hstat_html_escape("Rendement <2023>"), "Rendement &lt;2023&gt;")
  expect_equal(hstat_html_escape("Masse & surface"), "Masse &amp; surface")
  expect_equal(hstat_html_escape(c("a<b", "c>d")), c("a&lt;b", "c&gt;d"))

  # Les deux sites qui composent une notification a partir de noms de colonnes
  # passent par l'echappement. Ils sont nommes : ce sont les seuls du depot ou
  # un nom venu du fichier rejoint du balisage.
  chemin <- .hstat_module_path("mod_tests.R")
  skip_if_not(file.exists(chemin))
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("hstat_html_escape(errors)", txt, fixed = TRUE))
  expect_true(grepl("hstat_html_escape(added)", txt, fixed = TRUE))
  expect_false(grepl('paste(errors, collapse = "<br>")', txt, fixed = TRUE))
  expect_false(grepl('paste0("<b>", added, "</b>"', txt, fixed = TRUE))
})

test_that("aucun déclencheur ne guette une entrée que personne ne déclare", {
  # 463 LIGNES ETAIENT INATTEIGNABLES. Un `observeEvent(input$X)` dont aucune
  # interface ne declare `X` ne se declenche JAMAIS : le code se lit comme une
  # fonctionnalite vivante, il ne s'execute pas, et rien ne le signale. Neuf
  # declencheurs etaient dans ce cas -- dont le chi-deux d'adequation entier
  # (analyse, post-hoc, graphique, exports) et un post-hoc LM en double.
  #
  # Le risque n'est pas le poids : c'est de corriger la copie morte en croyant
  # corriger l'analyse. C'est la lecon deja tiree de `createPlotDownloadHandler`.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  fs <- c(list.files(file.path(root, "R"), "^mod_.*\\.R$", full.names = TRUE),
          file.path(root, "inst", "app", c("app_server.R", "UX.R")))
  fs <- fs[file.exists(fs)]
  skip_if(!length(fs))
  txt <- paste(vapply(fs, function(f)
    paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), ""), collapse = "\n")

  pull <- function(re) unique(unlist(regmatches(txt, gregexpr(re, txt, perl = TRUE))))
  # `.` NE FRANCHIT PAS UN RETOUR A LA LIGNE, et un identifiant se pose souvent
  # sur la ligne SUIVANT `actionButton(`. Un decoupage par `^.*\\("` laissait
  # donc la correspondance entiere et signalait comme orphelins trois boutons
  # parfaitement declares. On extrait la DERNIERE chaine citee, quelle que soit
  # la mise en page.
  cap  <- function(v, pre, suf) {
    if (!length(v)) return(character(0))
    unique(vapply(v, function(x) {
      q <- regmatches(x, gregexpr('"[A-Za-z0-9_.]+"', x))[[1]]
      if (!length(q)) NA_character_ else gsub('"', "", q[length(q)])
    }, character(1), USE.NAMES = FALSE))
  }

  # Tout ce qui DECLARE un identifiant : ns("x"), un widget, une mise a jour,
  # ou une construction JavaScript par prefixe (le renommage de colonne).
  dec <- c(
    cap(pull('ns\\("[A-Za-z0-9_.]+"\\)'), '^ns\\("', '"\\)$'),
    cap(pull(paste0('(?:selectInput|selectizeInput|numericInput|textInput|textAreaInput|',
                    'checkboxInput|checkboxGroupInput|radioButtons|radioGroupButtons|',
                    'sliderInput|actionButton|actionLink|fileInput|dateInput|colourInput|',
                    'pickerInput|downloadButton|downloadLink|tabsetPanel|rank_list)',
                    '\\(\\s*"[A-Za-z0-9_.]+"')), '^.*\\("', '"$'),
    cap(pull('update[A-Za-z]*\\(\\s*session\\s*,\\s*"[A-Za-z0-9_.]+"'), '^.*,\\s*"', '"$'),
    # Le renommage de colonne se pose depuis le JavaScript de DT, en guillemets
    # SIMPLES et prefixe a la main : `Shiny.setInputValue(nsId + 'renameCol...')`.
    # C'est une declaration valide, elle ne ressemble simplement a aucune autre.
    unique(gsub(".*'([A-Za-z0-9_.]+)'.*", "\\1",
                pull("nsId \\+ '[A-Za-z0-9_.]+'"))))

  trig <- unique(sub('^.*input\\$', "",
    pull('(?:observeEvent|eventReactive)\\(\\s*(?:shiny::)?input\\$[A-Za-z0-9_.]+')))
  # Formes construites a l'execution, hors de portee d'un balayage textuel :
  # les entrees que DT fabrique, les prefixes du multivarie, et les deux aides
  # qui posent elles-memes leur bouton (`id()` de mod_ai, `torch_install_ui`).
  dyn <- "(_cell_edit|_rows_selected|_columns_selected|_cells_selected|_search|_state)$|^mv_|^hstat_"
  connus <- c("ping", "torchInstall", "torchInstall2")
  orphelins <- setdiff(trig[!grepl(dyn, trig)], c(dec, connus))
  expect_equal(sort(orphelins), character(0))
})

test_that("une sortie de module est toujours namespacée", {
  # CINQ PANNEAUX D'INTERPRETATION NE S'AFFICHAIENT JAMAIS. Le serveur les
  # calculait -- diagnostic du modele, QQ-plot, normalite, homogeneite,
  # autocorrelation des residus -- mais l'interface les posait sans `ns()` :
  # elle demandait « qqPlotInterpretation » quand le module publie
  # « tests-qqPlotInterpretation ». Rien ne leve, la place reste vide.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  fs <- list.files(file.path(root, "R"), "^mod_.*\\.R$", full.names = TRUE)
  skip_if(!length(fs))
  fautifs <- character(0)
  sorties <- paste0("(uiOutput|htmlOutput|textOutput|verbatimTextOutput|plotOutput|",
                    "DTOutput|dataTableOutput|plotlyOutput|imageOutput|tableOutput)")
  for (f in fs) {
    src <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    m <- regmatches(src, gregexpr(paste0(sorties, '\\(\\s*"[A-Za-z0-9_.]+"'),
                                  src, perl = TRUE))[[1]]
    if (length(m))
      fautifs <- c(fautifs, sprintf("%s : %s", basename(f), m))
  }
  expect_equal(fautifs, character(0))
})

test_that("hstat_sql_ident neutralise les guillemets dans les identifiants", {
  expect_equal(hstat_sql_ident("Rendement"), '"Rendement"')
  expect_equal(hstat_sql_ident('col" ; DROP TABLE x --'),
               '"col"" ; DROP TABLE x --"')
})


# =============================================================================
# --------------------------------------------------------------------------
#  Visualisation -- conversion numérique FR et capuchons de moustache
# --------------------------------------------------------------------------
# =============================================================================

test_that("hstat_as_numeric_fr convertit les formats français", {
  expect_equal(hstat_as_numeric_fr(c("12,5", "3,25", "1 250,75")),
               c(12.5, 3.25, 1250.75))
  expect_equal(hstat_as_numeric_fr(c("2005", "2010", NA)), c(2005, 2010, NA))
  x <- c(1.5, 2.5); expect_identical(hstat_as_numeric_fr(x), x)
  expect_null(hstat_as_numeric_fr(c("ABENGOUROU", "Divo", "MAN")))
  expect_null(hstat_as_numeric_fr(c("", NA_character_)))
  # tolérance : > 10 % de valeurs non convertibles -> NULL
  expect_null(hstat_as_numeric_fr(c("1", "2", "x", "y")))
  expect_equal(hstat_as_numeric_fr(c(rep("1,5", 19), "abc"))[1], 1.5)
})

test_that("hstat_add_whisker_caps insère des capuchons alignés", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  set.seed(3)
  d <- data.frame(x = rep(c("A", "B"), each = 40),
                  loc = rep(c("g1", "g2", "g3", "g4"), 20), y = rnorm(80))
  align_ok <- function(p2, n) {
    b <- ggplot_build(p2); eb <- b$data[[1]]; bx <- b$data[[2]]
    nrow(eb) == n && max(abs(sort(eb$x) - sort(bx$x))) < 1e-9 &&
      max(abs(sort(eb$ymin) - sort(bx$ymin))) < 1e-9 &&
      max(abs(sort(eb$ymax) - sort(bx$ymax))) < 1e-9
  }
  # boxplot groupé (fill au niveau couche) : 8 boîtes "dodgées"
  p1 <- hstat_add_whisker_caps(ggplot(d, aes(x, y)) +
                                 geom_boxplot(aes(fill = loc), alpha = .7))
  expect_true(inherits(p1$layers[[1]]$geom, "GeomErrorbar"))
  expect_true(align_ok(p1, 8))
  # fill au niveau plot (cas post-hoc)
  p2 <- hstat_add_whisker_caps(ggplot(d, aes(x, y, fill = x)) +
                                 geom_boxplot(alpha = .7))
  expect_true(align_ok(p2, 2))
  # idempotence et non-boxplot inchangé
  expect_length(hstat_add_whisker_caps(p1)$layers, 2)
  expect_length(hstat_add_whisker_caps(ggplot(d, aes(y, y)) + geom_line())$layers, 1)
})


# =============================================================================
# --------------------------------------------------------------------------
#  Types de variables -- facteurs ordinaux
# --------------------------------------------------------------------------
# =============================================================================

test_that("la conversion en facteur ordinal respecte l'ordre défini", {
  convert_ordered <- function(x, lv_user) {
    vals_chr <- as.character(x)
    uniq <- unique(vals_chr[!is.na(vals_chr)])
    lv <- lv_user[lv_user %in% uniq]
    if (length(lv) == 0) lv <- sort(uniq)
    lv <- c(lv, sort(setdiff(uniq, lv)))
    factor(vals_chr, levels = lv, ordered = TRUE)
  }
  x <- c("0-3 ans", "4-15 ans", "+15 ans", "0-3 ans", NA)
  f <- convert_ordered(x, c("0-3 ans", "4-15 ans", "+15 ans"))
  expect_true(is.ordered(f))
  expect_identical(levels(f), c("0-3 ans", "4-15 ans", "+15 ans"))
  expect_true(f[1] < f[2] && f[2] < f[3])
  # ordre partiel : modalités restantes ajoutées en fin, rien n'est perdu
  f2 <- convert_ordered(x, "0-3 ans")
  expect_identical(levels(f2), c("0-3 ans", "+15 ans", "4-15 ans"))
  expect_equal(sum(is.na(f2)), 1)
  # nominal explicite : la classe ordered doit disparaître
  f3 <- factor(as.character(f), ordered = FALSE)
  expect_false(is.ordered(f3))
  expect_true(is.ordered(as.factor(f)))  # justification du correctif
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- sorties console et IC du V de Cramér
# --------------------------------------------------------------------------
# =============================================================================

test_that("les tests bivariés reproduisent la présentation console R (Titanic)", {
  skip_if_not_installed("ggplot2")
  x <- c(rep("0", 81), rep("1", 233), rep("0", 468), rep("1", 109))
  y <- c(rep("female", 314), rep("male", 577))
  res <- hstat_q_nominal_bivariate(x, y, "Survived", "Sex")
  expect_true(isTRUE(res$ok))
  txt <- paste(res$console, collapse = "\n")
  expect_true(grepl("Yates", txt))            # khi-deux avec correction
  expect_true(grepl("260.7", txt))            # X-squared = 260.72
  expect_true(grepl("193.4747", txt))         # effectifs attendus
  expect_true(grepl("-8.08617", txt))         # résidus de Pearson
  expect_true(grepl("Fisher's Exact Test", txt))
  expect_true(grepl("Cramer V", txt))
  expect_true("Effectifs théoriques (attendus)" %in% names(res$tables))
})

test_that("OR / RR au format epitools avec les valeurs de référence", {
  x <- c(rep("0", 81), rep("1", 233), rep("0", 468), rep("1", 109))
  y <- c(rep("female", 314), rep("male", 577))
  orr <- hstat_q_or_rr_analysis(y, x, "Sex", "Survived", y_issue = "1")
  t2 <- paste(orr$console, collapse = "\n")
  expect_true(grepl("odds ratio with 95% C.I.", t2))
  expect_true(grepl("risk ratio with 95% C.I.", t2))
  expect_true(grepl("0.2545801", t2))         # RR identique à epitools
  expect_true(grepl("0.2123854", t2))         # IC bas identique
  expect_true(grepl("6.46392e-60", t2))       # p Fisher identique
})

test_that("l'IC du V de Cramér encadre l'estimation", {
  chi2 <- 260.717; n <- 891
  V <- sqrt(chi2 / n)
  ci <- hstat_q_cramer_ci(chi2, 1, n, 2)
  expect_true(ci[1] < V && V < ci[2])
  expect_true(ci[1] > 0)
  expect_equal(hstat_q_cramer_ci(NA, 1, 10, 2), c(NA_real_, NA_real_))
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- OR/RR toutes modalités et interprétation
# --------------------------------------------------------------------------
# =============================================================================

test_that("le mode 'toutes les modalités' croise chaque X avec chaque Y", {
  set.seed(7)
  x <- sample(c("0", "1"), 600, replace = TRUE)
  y <- sample(c("A", "B", "C"), 600, replace = TRUE)
  res <- hstat_q_or_rr_analysis(x, y, "X", "Y", all_pairs = TRUE)
  expect_true(isTRUE(res$ok))
  tbl <- res$tables[["OR / RR par paire"]]
  expect_equal(nrow(tbl), 2 * 3)              # 2 modalités X x 3 modalités Y
  expect_true("Interpretation" %in% names(tbl))
  expect_true(all(nzchar(tbl$Interpretation)))
  # un bloc console OR + RR par modalité de Y
  expect_equal(sum(grepl("== ODDS RATIO", res$console)), 3)
  expect_equal(sum(grepl("== RISQUE RELATIF", res$console)), 3)
  # synthèse d'interprétation présente
  expect_true(any(grepl("combinaison", res$interpretation)))
})

test_that("le mode 'une issue' et le 2x2 strict restent inchangés", {
  set.seed(8)
  x <- sample(c("0", "1"), 400, replace = TRUE)
  y3 <- sample(c("A", "B", "C"), 400, replace = TRUE)
  r1 <- hstat_q_or_rr_analysis(x, y3, "X", "Y", y_issue = "A", all_pairs = FALSE)
  expect_equal(nrow(r1$tables[["OR / RR par paire"]]), 2)     # 2 X vs issue A
  y2 <- sample(c("H", "F"), 400, replace = TRUE)
  r2 <- hstat_q_or_rr_analysis(x, y2, "X", "Sexe")
  expect_equal(nrow(r2$tables[["OR / RR par paire"]]), 1)     # 2x2 strict
})

test_that("le niveau de confiance annoncé est celui qui a été calculé", {
  # LE CURSEUR VA DE 0,80 A 0,99. La phrase d'interpretation ecrivait « IC95% »
  # en dur : a 99 %, elle annoncait donc 95 % a cote de bornes plus larges,
  # pendant que le tableau des mesures affichait « IC99% » sur les MEMES
  # bornes. C'est la phrase que l'utilisateur recopie dans son rapport.
  x <- c(rep("1", 100), rep("0", 100))
  y <- c(rep("oui", 90), rep("non", 10), rep("oui", 10), rep("non", 90))
  bornes <- list()
  for (cf in c(0.80, 0.95, 0.99)) {
    r <- hstat_q_or_rr_analysis(x, y, "Expo", "Issue", conf = cf)
    phrase <- r$interpretation[grepl("odds ratio", r$interpretation)][1]
    expect_true(grepl(sprintf("IC%d%%", round(100 * cf)), phrase, fixed = TRUE),
                info = paste("conf =", cf, "->", phrase))
    expect_true(any(grepl(sprintf("IC%d%%", round(100 * cf)),
                          r$metrics$Metrique, fixed = TRUE)),
                info = paste("tableau des mesures, conf =", cf))
    bornes[[as.character(cf)]] <- as.numeric(
      regmatches(phrase, gregexpr("[0-9]+\\.[0-9]+", phrase))[[1]])
  }
  # Et l'etiquette suit bien un calcul REEL : un intervalle a 99 % est plus
  # large qu'a 95 %, lui-meme plus large qu'a 80 %.
  larg <- vapply(bornes, function(b) diff(range(utils::tail(b, 2))), numeric(1))
  expect_true(larg[["0.8"]] < larg[["0.95"]] && larg[["0.95"]] < larg[["0.99"]])
})

test_that("l'interprétation par ligne détecte le sens de l'association", {
  # X fortement associé à Y : X=1 -> presque toujours Y=oui
  x <- c(rep("1", 100), rep("0", 100))
  y <- c(rep("oui", 90), rep("non", 10), rep("oui", 10), rep("non", 90))
  res <- hstat_q_or_rr_analysis(x, y, "Expo", "Issue", all_pairs = TRUE)
  tbl <- res$tables[["OR / RR par paire"]]
  # au moins une association significative détectée
  sig <- !(tbl$OR_IC_bas <= 1 & tbl$OR_IC_haut >= 1)
  expect_true(any(sig))
  expect_true(any(grepl("significatif", tbl$Interpretation)))
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- texte (NLP) et modes
# --------------------------------------------------------------------------
# =============================================================================

test_that("le stemmer français regroupe les formes fléchies sans sur-découper", {
  st <- hstat_q_stem_fr(c("moustiquaire", "moustiquaires", "enfant", "enfants",
                          "saison", "saisons", "dorment", "dormir"))
  expect_equal(st[1], st[2])         # moustiquaire(s)
  expect_equal(st[3], st[4])         # enfant(s)
  expect_equal(st[3], "enfant")      # PAS "enf" (sur-découpage évité)
  expect_equal(st[5], st[6])         # saison(s)
})

test_that("le pipeline NLP produit toutes les étapes et respecte les options", {
  skip_if_not_installed("ggplot2")
  ph <- c("Les moustiquaires protègent les enfants contre le paludisme.",
          "Il fait chaud, les moustiques donnent le paludisme aux enfants.",
          "Le dispensaire donne des moustiquaires en saison des pluies.",
          "Les enfants dorment sous une moustiquaire chaque nuit.",
          "Sans moustiquaire, les enfants attrapent le paludisme souvent.",
          "Ma famille dort sous moustiquaire depuis deux ans.")
  r <- hstat_q_text_analysis(ph, "texte", stem = TRUE)
  expect_true(isTRUE(r$ok))
  expect_true("Étapes du pipeline NLP" %in% names(r$tables))
  expect_true("Scores TF-IDF" %in% names(r$tables))
  # "qu" (mot outil) doit être filtré par les stopwords étendus
  expect_false("qu" %in% r$tables[["Fréquences des mots"]]$Mot)
  # stopwords personnalisés
  r2 <- hstat_q_text_analysis(ph, "texte", extra_stopwords = c("paludisme"))
  expect_false("paludisme" %in% r2$tables[["Fréquences des mots"]]$Mot)
  # chiffres retirés par défaut
  r3 <- hstat_q_text_analysis(c("test 123 456", "test 789 mot", "mot test valeur"),
                              "t", min_char = 2)
  expect_false(any(grepl("[0-9]", r3$tables[["Fréquences des mots"]]$Mot)))
})

test_that("le mode signale l'unimodalité et la multimodalité", {
  r1 <- hstat_q_nominal_univariate(c(rep("A", 10), rep("B", 5)), "V")
  m1 <- r1$metrics
  expect_equal(m1$Valeur[m1$Metrique == "Mode(s)"], "A")
  expect_equal(m1$Valeur[m1$Metrique == "Nature de la distribution"], "Unimodale")
  r2 <- hstat_q_nominal_univariate(c(rep("A", 5), rep("B", 5), rep("C", 2)), "V")
  m2 <- r2$metrics
  expect_equal(m2$Valeur[m2$Metrique == "Nature de la distribution"], "Bimodale")
  expect_true(grepl("A", m2$Valeur[m2$Metrique == "Mode(s)"]))
  expect_true(grepl("B", m2$Valeur[m2$Metrique == "Mode(s)"]))
})

test_that("hstat_q_apply_palette re-colore sans casser le graphique", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  r <- hstat_q_nominal_univariate(c(rep("A", 6), rep("B", 3), rep("C", 1)), "V")
  pbar <- r$plotfns[["Diagramme en barres"]]()
  for (pal in c("blues", "greens", "viridis", "spectral", "greys", "custom")) {
    p2 <- hstat_q_apply_palette(pbar, pal, "#123456", "#abcdef")
    expect_s3_class(p2, "ggplot")
    expect_silent(ggplot2::ggplot_build(p2))
  }
  # 'default' laisse le graphique inchangé
  expect_identical(hstat_q_apply_palette(pbar, "default"), pbar)
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- couleurs par défaut de ggplot
# --------------------------------------------------------------------------
# =============================================================================

test_that("les graphiques utilisent les échelles par défaut de ggplot", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  ph <- rep(c("moustiquaire enfants paludisme excellent",
              "moustiques chaud probleme difficile",
              "dispensaire sante dormir nuit"), 4)
  rt <- hstat_q_text_analysis(ph, "t", stem = TRUE)
  # aucune échelle fill/colour déclarée => défauts ggplot
  no_custom_scale <- function(p)
    !any(vapply(p$scales$scales,
                function(sc) any(c("fill", "colour") %in% sc$aesthetics), logical(1)))
  expect_true(no_custom_scale(rt$plotfns[["Sentiments"]]()))
  expect_true(no_custom_scale(rt$plotfns[["Mots fréquents"]]()))
  expect_true(no_custom_scale(rt$plotfns[["Nuage de mots"]]()))
  r <- hstat_q_nominal_univariate(c(rep("A", 6), rep("B", 3)), "V")
  expect_true(no_custom_scale(r$plotfns[["Diagramme en barres"]]()))
  # la personnalisation reste possible par-dessus
  p2 <- hstat_q_apply_palette(rt$plotfns[["Sentiments"]](), "viridis")
  expect_false(no_custom_scale(p2))
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- Chi²/Multinomial et tableaux croisés intégrés
# --------------------------------------------------------------------------
# =============================================================================

test_that("le Chi² d'ajustement reproduit chisq.test et interprète ses métriques", {
  x <- c(rep("A", 60), rep("B", 25), rep("C", 15))
  r <- hstat_q_gof_analysis(x, "V", method = "chisq")
  expect_true(isTRUE(r$ok))
  ref <- suppressWarnings(chisq.test(c(60, 25, 15)))
  expect_equal(as.numeric(r$metrics$Valeur[r$metrics$Metrique == "Khi-deux"]),
               unname(round(ref$statistic, 3)))
  expect_true("Interpretation" %in% names(r$metrics))
  expect_true(all(nzchar(r$metrics$Interpretation)))
  expect_true(any(grepl("Chi-squared", r$console)))
  # proportions personnalisées conformes -> p ~ 1
  r2 <- hstat_q_gof_analysis(x, "V", expected_props = c(0.6, 0.25, 0.15))
  p2 <- r2$metrics$Valeur[r2$metrics$Metrique == "p-value (Khi-deux)"]
  expect_true(as.numeric(gsub("[^0-9.e-]", "", p2)) > 0.5)
  # nombre de proportions incorrect -> message clair
  r3 <- hstat_q_gof_analysis(x, "V", expected_props = c(0.5, 0.5))
  expect_false(isTRUE(r3$ok))
})

test_that("le test multinomial exact fournit une p-value et sa sortie console", {
  xs <- c(rep("A", 9), rep("B", 3), rep("C", 2))
  set.seed(11)
  r <- hstat_q_gof_analysis(xs, "V", method = "multinomial", B = 2000)
  expect_true(isTRUE(r$ok))
  expect_true("p-value (Multinomial exact)" %in% r$metrics$Metrique)
  p <- as.numeric(gsub("[^0-9.e-]", "",
        r$metrics$Valeur[r$metrics$Metrique == "p-value (Multinomial exact)"]))
  expect_true(p > 0 && p < 1)
  expect_true(any(grepl("multinomial exact", r$console)))
})

test_that("les tableaux croisés intégrés fournissent profils et métriques interprétées", {
  set.seed(2)
  x <- sample(c("H", "F"), 200, TRUE)
  y <- sample(c("Oui", "Non", "NSP"), 200, TRUE)
  r <- hstat_q_nominal_bivariate(x, y, "Sexe", "Reponse")
  expect_true(all(c("Table de contingence", "Profils ligne (%)",
                    "Profils colonne (%)", "Pourcentages du total (%)")
                  %in% names(r$tables)))
  expect_true("Interpretation" %in% names(r$metrics))
  expect_true(all(nzchar(r$metrics$Interpretation)))
  expect_true("Barres groupées (effectifs)" %in% names(r$plotfns))
  pc <- r$tables[["Profils colonne (%)"]]
  expect_true(all(abs(colSums(pc[, -1]) - 100) < 0.5))
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- croisement d'une variable avec elle-même
# --------------------------------------------------------------------------
# =============================================================================

test_that("les graphiques croisés fonctionnent même si les variables ont le même nom", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  set.seed(1)
  x <- sample(c("A", "B", "C"), 120, TRUE)
  y <- sample(c("Oui", "Non"), 120, TRUE)
  # Cas qui provoquait l'erreur "duplicate columns" : xname == yname
  r <- hstat_q_nominal_bivariate(x, x, "V", "V")
  expect_true(isTRUE(r$ok))
  for (pn in names(r$plotfns)) {
    p <- r$plotfns[[pn]]()
    if (!is.null(p)) expect_silent(ggplot2::ggplot_build(p))
  }
  # Les axes de la carte des résidus restent corrects (x = Y, y = X)
  r2 <- hstat_q_nominal_bivariate(x, y, "GG", "RR")
  ph <- r2$plotfns[["Carte des résidus"]]()
  expect_equal(ph$labels$x, "RR")
  expect_equal(ph$labels$y, "GG")
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- exportation des résultats
# --------------------------------------------------------------------------
# =============================================================================

test_that(".safe_name produit des noms de fichiers propres", {
  expect_equal(.safe_name("Profils colonne (%)"), "profils_colonne")
  expect_equal(.safe_name("Tranche d'\u00e2ge / R\u00e9ponse"), "tranche_d_age_reponse")
  expect_equal(.safe_name(""), "tableau")
})

test_that(".write_xlsx écrit un classeur multi-feuilles valide", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE))
  set.seed(1)
  r <- hstat_q_nominal_bivariate(sample(c("A","B"), 80, TRUE),
                                 sample(c("X","Y","Z"), 80, TRUE), "G", "R")
  sheets <- c(list("Métriques" = r$metrics), r$tables)
  f <- tempfile(fileext = ".xlsx")
  .write_xlsx(sheets, f)
  expect_true(file.exists(f) && file.size(f) > 3000)
  sn <- openxlsx::getSheetNames(f)
  expect_equal(length(sn), length(sheets))
  expect_true(all(nchar(sn) <= 31))
})

test_that("l'export image fonctionne dans tous les formats", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  set.seed(2)
  r <- hstat_q_nominal_univariate(sample(c("A","B","C"), 60, TRUE), "V")
  p <- r$plotfns[["Diagramme en barres"]]()
  for (fmt in c("png", "pdf", "svg")) {
    ff <- tempfile(fileext = paste0(".", fmt))
    args <- list(filename = ff, plot = p, width = 8, height = 5,
                 units = "in", device = fmt)
    if (fmt == "png") { args$dpi <- 200; args$bg <- "white" }
    do.call(ggplot2::ggsave, args)
    expect_true(file.exists(ff) && file.size(ff) > 500)
  }
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- mise en forme interactive du graphique
# --------------------------------------------------------------------------
# =============================================================================

test_that("hstat_q_apply_style applique titres, tailles, rotation et style", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  set.seed(1)
  r <- hstat_q_nominal_univariate(sample(c("A", "B", "C"), 90, TRUE), "V")
  p0 <- r$plotfns[["Diagramme en barres"]]()
  opts <- list(title = "Mon titre", xlab = "Cat", ylab = "Eff", legend = "Grp",
               title_size = 22, axis_text_size = 8, x_rotation = 90,
               axis_title_bold = TRUE, axis_title_italic = TRUE,
               show_grid = FALSE, black_axes = TRUE, black_ticks = TRUE)
  p <- hstat_q_apply_style(p0, opts)
  expect_equal(p$labels$title, "Mon titre")
  expect_equal(p$labels$x, "Cat")
  expect_equal(p$theme$plot.title$size, 22)
  expect_equal(p$theme$axis.text.x$angle, 90)
  expect_equal(p$theme$axis.title.x$face, "bold.italic")
  expect_s3_class(p$theme$panel.grid.major, "element_blank")
  expect_s3_class(p$theme$axis.line, "element_line")
  # champs vides -> ne remplace pas les labels existants
  p2 <- hstat_q_apply_style(p0, list())
  expect_silent(ggplot2::ggplot_build(p2))
})

test_that("palette et style se composent sur tous les graphiques", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  set.seed(2)
  r <- hstat_q_nominal_bivariate(sample(c("A", "B"), 80, TRUE),
                                 sample(c("X", "Y", "Z"), 80, TRUE), "G", "R")
  for (pn in names(r$plotfns)) {
    p <- hstat_q_apply_style(
      hstat_q_apply_palette(r$plotfns[[pn]](), "viridis"),
      list(title = "T", x_rotation = 30, black_ticks = TRUE))
    expect_silent(ggplot2::ggplot_build(p))
  }
})


# =============================================================================
# --------------------------------------------------------------------------
#  Sécurité -- v25 : tables SQL, délimiteur, limite d'upload
# --------------------------------------------------------------------------
# =============================================================================

test_that("un nom de table piégé est neutralisé dans les requêtes DuckDB", {
  evil <- 'x" ; DROP TABLE users --'
  q <- sprintf("SELECT COUNT(*) AS n FROM %s", hstat_sql_ident(evil))
  # le contenu hostile reste ENTRE guillemets doublés : simple identifiant
  expect_true(grepl('FROM "x"" ; DROP TABLE users --"', q, fixed = TRUE))
  expect_equal(hstat_sql_ident("hstat_source"), '"hstat_source"')
})

test_that("le délimiteur CSV est échappé avant interpolation SQL", {
  delim <- "\'),; ATTACH \'"
  esc <- gsub("\'", "\'\'", delim)
  # plus aucune apostrophe isolée : impossible de clore la chaîne SQL
  expect_false(grepl("(^|[^\'])\'([^\']|$)", esc))
})

test_that("la limite d'upload est configurable et bornée", {
  old <- Sys.getenv("HSTAT_MAX_UPLOAD_MB", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("HSTAT_MAX_UPLOAD_MB")
          else Sys.setenv(HSTAT_MAX_UPLOAD_MB = old))
  Sys.setenv(HSTAT_MAX_UPLOAD_MB = "512")
  v <- suppressWarnings(as.numeric(Sys.getenv("HSTAT_MAX_UPLOAD_MB", "2048")))
  if (!is.finite(v) || v <= 0) v <- 2048
  expect_equal(v, 512)
  Sys.setenv(HSTAT_MAX_UPLOAD_MB = "abc")
  v2 <- suppressWarnings(as.numeric(Sys.getenv("HSTAT_MAX_UPLOAD_MB", "2048")))
  if (!is.finite(v2) || v2 <= 0) v2 <- 2048
  expect_equal(v2, 2048)
})


# =============================================================================
# --------------------------------------------------------------------------
#  Nettoyage -- classes d'intervalles (discrétisation)
# --------------------------------------------------------------------------
# =============================================================================

test_that("les trois méthodes de découpage produisent des facteurs ordonnés", {
  set.seed(1)
  ages <- c(round(runif(80, 0, 60)), NA)
  r1 <- hstat_cut_intervals(ages, "width", n_classes = 4)
  expect_true(isTRUE(r1$ok) && is.ordered(r1$factor) && nlevels(r1$factor) == 4)
  r2 <- hstat_cut_intervals(ages, "quantile", n_classes = 4)
  expect_true(isTRUE(r2$ok))
  expect_lte(max(r2$counts$Effectif) - min(r2$counts$Effectif), 3)
  r3 <- hstat_cut_intervals(ages, "manual", breaks_manual = c(0, 3, 15, 100),
                            labels_custom = c("0-3 ans", "4-15 ans", "+15 ans"))
  expect_identical(levels(r3$factor), c("0-3 ans", "4-15 ans", "+15 ans"))
  i_lo <- which(ages < 3)[1]; i_hi <- which(ages > 20)[1]
  expect_true(r3$factor[i_lo] < r3$factor[i_hi])   # comparaison ordinale valide
})

test_that("les erreurs de paramétrage donnent des messages clairs", {
  x <- 1:50
  expect_false(hstat_cut_intervals(x, "manual", breaks_manual = 5)$ok)
  expect_false(hstat_cut_intervals(x, "manual", c(0, 10, 20),
                                   labels_custom = "une_seule")$ok)
  expect_false(hstat_cut_intervals(rep(5, 30), "width", 4)$ok)
  expect_false(hstat_cut_intervals(c("A", "B"), "width", 2)$ok)
  expect_false(hstat_cut_intervals(c(rep(1, 50), 2), "quantile", 5)$ok)
  # hors bornes -> NA signalés
  r <- hstat_cut_intervals(1:100, "manual", breaks_manual = c(10, 30))
  expect_gt(r$n_na_created, 0)
  expect_true(grepl("hors bornes", r$msg))
})

test_that("texte au format français et intervalles fermés à droite", {
  r <- hstat_cut_intervals(c("10,5", "20,2", "30,8", "40,1", "15,3", "25,7"),
                           "width", 2)
  expect_true(isTRUE(r$ok))
  r2 <- hstat_cut_intervals(1:60, "width", 3)
  expect_true(grepl("^\\[", levels(r2$factor)[1]))   # 1re classe fermée à gauche
})

test_that("les trois conventions de bornes produisent les bonnes étiquettes", {
  x <- c(0, 1, 2, 3, 4, 10, 14, 15, 20, 50, 99, 100)
  b <- c(0, 3, 15, 100)
  r2 <- hstat_cut_intervals(x, "manual", breaks_manual = b, interval_style = "std_last_closed")
  expect_identical(levels(r2$factor), c("[0 ; 3[", "[3 ; 15[", "[15 ; 100]"))
  expect_equal(r2$n_na_created, 0)
  r3 <- hstat_cut_intervals(x, "manual", breaks_manual = b, interval_style = "all_left_closed")
  expect_identical(levels(r3$factor), c("[0 ; 3[", "[3 ; 15[", "[15 ; 100["))
  expect_equal(r3$n_na_created, 0)   # max capturé malgré la borne ouverte
  r1 <- hstat_cut_intervals(x, "manual", breaks_manual = b, interval_style = "mixed_open")
  expect_identical(levels(r1$factor), c("[0 ; 3[", "]3 ; 15[", "]15 ; 100]"))
  expect_equal(r1$n_na_created, 0)
  # aucune valeur perdue quelle que soit la convention
  for (r in list(r1, r2, r3)) expect_equal(sum(r$counts$Effectif), length(x))
  # ancienne convention : fermées à droite
  r4 <- hstat_cut_intervals(x, "manual", breaks_manual = b, interval_style = "all_right_closed")
  expect_identical(levels(r4$factor), c("[0 ; 3]", "]3 ; 15]", "]15 ; 100]"))
  expect_equal(r4$n_na_created, 0)
  expect_equal(as.character(r4$factor[which(x == 3)]), "[0 ; 3]")   # 3 dans la 1re (fermée à droite)
  # toutes fermées des deux côtés
  r5 <- hstat_cut_intervals(x, "manual", breaks_manual = b, interval_style = "all_closed")
  expect_identical(levels(r5$factor), c("[0 ; 3]", "[3 ; 15]", "[15 ; 100]"))
  expect_equal(r5$n_na_created, 0)
  # les cinq conventions ne perdent aucune valeur
  for (st in c("std_last_closed", "all_left_closed", "mixed_open",
               "all_right_closed", "all_closed")) {
    r <- hstat_cut_intervals(x, "manual", breaks_manual = b, interval_style = st)
    expect_equal(sum(r$counts$Effectif), length(x))
  }
  # étiquettes personnalisées prioritaires sur la convention
  rc <- hstat_cut_intervals(x, "manual", breaks_manual = b, interval_style = "mixed_open",
                            labels_custom = c("0-3", "4-15", "+15"))
  expect_identical(levels(rc$factor), c("0-3", "4-15", "+15"))
})


# =============================================================================
# --------------------------------------------------------------------------
#  Post-hoc paramétrique -- ajustement des p-values
# --------------------------------------------------------------------------
# =============================================================================

test_that("un ajustement plus strict rend les groupes au moins aussi homogènes", {
  skip_if_not_installed("emmeans")
  suppressMessages(library(emmeans))
  set.seed(7)
  d <- data.frame(
    y = c(rnorm(15, 50, 8), rnorm(15, 45, 8), rnorm(15, 43, 8), rnorm(15, 40, 8)),
    g = factor(rep(c("A", "B", "C", "D"), each = 15)))
  m <- aov(y ~ g, data = d)
  p_none <- summary(pairs(emmeans(m, ~ g), adjust = "none"))$p.value
  for (adj in c("bonferroni", "holm", "BH", "hochberg")) {
    p_adj <- summary(pairs(emmeans(m, ~ g), adjust = adj))$p.value
    # chaque p ajustée >= p brute -> moins de différences significatives
    # -> regroupements en lettres au moins aussi larges (groupes plus homogènes)
    expect_true(all(p_adj >= p_none - 1e-9),
                info = paste("ajustement", adj, "doit être >= brut"))
  }
})

test_that("p.adjust reproduit les méthodes proposées à l'utilisateur", {
  p <- c(0.01, 0.02, 0.04, 0.20)
  # Bonferroni = p * m (borné à 1)
  expect_equal(p.adjust(p, "bonferroni"), pmin(p * length(p), 1))
  # toutes les méthodes de la liste existent
  for (m in c("holm", "bonferroni", "BH", "BY", "hochberg", "hommel")) {
    expect_length(p.adjust(p, m), length(p))
  }
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- post-hoc du Chi² d'ajustement (déplacé)
# --------------------------------------------------------------------------
# =============================================================================

test_that("le Chi² d'ajustement fournit un post-hoc par paires et des lettres de groupes", {
  x <- c(rep("A", 70), rep("B", 25), rep("C", 5))
  r <- hstat_q_gof_analysis(x, "V", method = "chisq", posthoc_adjust = "bonferroni")
  expect_true(isTRUE(r$ok))
  expect_true("Post-hoc : comparaisons par paires" %in% names(r$tables))
  ph <- r$tables[["Post-hoc : comparaisons par paires"]]
  expect_equal(nrow(ph), 3)   # A-B, A-C, B-C
  expect_true(all(c("Comparaison", "Khi2", "p_brute", "p_ajustee",
                    "Significatif", "Ajustement") %in% names(ph)))
  # lettres de groupes présentes (si multcompView dispo)
  skip_if_not_installed("multcompView")
  gof <- r$tables[["Observé vs attendu"]]
  expect_true("Groupe" %in% names(gof))
  expect_true("Groupes homogènes" %in% names(r$plotfns))
  expect_true(any(grepl("groupe.* homogène", r$interpretation)))
})

test_that("l'ajustement post-hoc du Chi² d'ajustement est monotone", {
  x <- c(rep("A", 60), rep("B", 30), rep("C", 10))
  r_none <- hstat_q_gof_analysis(x, "V", posthoc_adjust = "none")
  r_bonf <- hstat_q_gof_analysis(x, "V", posthoc_adjust = "bonferroni")
  p_none <- r_none$tables[["Post-hoc : comparaisons par paires"]]$p_ajustee
  p_bonf <- r_bonf$tables[["Post-hoc : comparaisons par paires"]]$p_ajustee
  expect_true(all(p_bonf >= p_none - 1e-9))
})

test_that("le post-hoc est aussi disponible avec le multinomial exact", {
  x <- c(rep("X", 9), rep("Y", 3), rep("Z", 2))
  set.seed(3)
  r <- hstat_q_gof_analysis(x, "V", method = "multinomial", B = 1500,
                            posthoc_adjust = "holm")
  expect_true("Post-hoc : comparaisons par paires" %in% names(r$tables))
})


# =============================================================================
# --------------------------------------------------------------------------
#  Analyses qualitatives -- proportions, corrélation interprétée, rangs/médianes
# --------------------------------------------------------------------------
# =============================================================================

test_that("les tableaux croisés incluent un graphique de proportions", {
  skip_if_not_installed("ggplot2")
  set.seed(1)
  r <- hstat_q_nominal_bivariate(sample(c("H","F"), 150, TRUE),
                                 sample(c("A","B","C"), 150, TRUE), "Sexe", "Choix")
  expect_true("Proportions (% par groupe)" %in% names(r$plotfns))
  expect_silent(ggplot2::ggplot_build(r$plotfns[["Proportions (% par groupe)"]]()))
})

test_that("la corrélation ordinale est interprétée (significative, non signif, p-value)", {
  ord <- c("Faible", "Moyen", "Fort")
  set.seed(9)
  # non significatif (aléatoire)
  x <- sample(ord, 60, TRUE); y <- sample(1:3, 60, TRUE)
  r <- hstat_q_ordinal_compare(x, y, levels_order = ord, second_ordinal = TRUE,
                               xname = "A", gname = "B")
  expect_true(any(grepl("SIGNIFICATIVE|NON significative", r$interpretation)))
  expect_true(any(grepl("^p =|p <", unlist(r$interpretation))))   # p-value interprétée
})

test_that("la comparaison de groupes fournit rangs, médianes et test de Mood", {
  ord <- c("Faible", "Moyen", "Fort")
  set.seed(2)
  grp <- rep(c("G1","G2","G3"), each = 30)
  val <- c(sample(ord, 30, TRUE, c(.6,.3,.1)),
           sample(ord, 30, TRUE, c(.3,.4,.3)),
           sample(ord, 30, TRUE, c(.1,.3,.6)))
  r <- hstat_q_ordinal_compare(val, grp, levels_order = ord,
                               xname = "Satisfaction", gname = "Groupe")
  expect_true("Médianes et rangs par groupe" %in% names(r$tables))
  md <- r$tables[["Médianes et rangs par groupe"]]
  expect_true(all(c("Mediane_rang", "Rang_moyen") %in% names(md)))
  expect_true("Interpretation" %in% names(r$metrics))
  expect_true(any(grepl("médiane", r$metrics$Metrique, ignore.case = TRUE)))
})

test_that("le test d'adéquation stratifié teste Y dans chaque groupe de X", {
  set.seed(1)
  x <- rep(c("Nord", "Sud"), each = 60)
  y <- c(sample(c("A","B","C"), 60, TRUE, c(.6,.3,.1)),
         sample(c("A","B","C"), 60, TRUE, c(.2,.3,.5)))
  r <- hstat_q_gof_stratified(y, x, "Pref", "Region", method = "chisq")
  expect_true(isTRUE(r$ok))
  expect_true("Synthèse par groupe (X)" %in% names(r$tables))
  expect_equal(nrow(r$tables[["Synthèse par groupe (X)"]]), 2)
  expect_true("Table croisée X x Y" %in% names(r$tables))
})


# =============================================================================
# --------------------------------------------------------------------------
#  Citation du package
# --------------------------------------------------------------------------
# =============================================================================

test_that("hstat_citation produit les 6 styles valides", {
  for (st in c("text", "apa", "vancouver", "markdown", "bibtex", "ris")) {
    v <- suppressWarnings(hstat_citation(st))
    expect_type(v, "character")
    expect_length(v, 1)
    expect_true(nzchar(v))
    expect_true(grepl("KOUADIO|houphouet", v, ignore.case = TRUE))
  }
})

test_that("le BibTeX est bien formé et le RIS structuré", {
  bt <- suppressWarnings(hstat_citation("bibtex"))
  expect_true(grepl("^@Manual\\{hstat,", bt))
  expect_equal(lengths(regmatches(bt, gregexpr("\\{", bt))),
               lengths(regmatches(bt, gregexpr("\\}", bt))))
  ris <- suppressWarnings(hstat_citation("ris"))
  expect_true(grepl("^TY  - COMP", ris))
  expect_true(grepl("ER  - $", ris))
})

test_that("un style de citation inconnu est rejeté", {
  expect_error(hstat_citation("inconnu"))
})


# =============================================================================
#  v0.5.0 -- Modelisation predictive : helpers de metriques, alignement,
#  simulation et garde-fou d'export haute resolution
# =============================================================================

test_that("hstat_metrics_reg calcule et interprete les 4 metriques", {
  set.seed(1)
  obs <- rnorm(200, 50, 10); pred <- obs + rnorm(200, 0, 3)
  m <- hstat_metrics_reg(obs, pred)
  expect_setequal(m$Metrique, c("RMSE", "MAE", "MAPE (%)", "R2"))
  expect_gt(m$Valeur[m$Metrique == "R2"], 0.85)
  expect_true(all(nzchar(m$Interpretation)))
  # Robustesse : NA et effectif minimal
  expect_s3_class(hstat_metrics_reg(c(1, NA, 3), c(1, 2, 3)), "data.frame")
})

test_that("les quatre metriques valent ce qu'elles annoncent, pas seulement leur nom", {
  # LES NOMS ETAIENT VERIFIES, LES VALEURS NON. Remplacer RMSE par MAE dans le
  # calcul ne faisait echouer aucune assertion : le tableau gardait ses quatre
  # lignes, ses quatre libelles et ses quatre interpretations non vides, et
  # affichait un chiffre faux sous le bon nom. Meme famille que le temoin nul
  # qui rendait -Inf avec son alerte affichee a cote. Trouve par mutation.
  obs  <- c(10, 20, 30, 40)
  pred <- c(12, 18, 33, 36)          # erreurs : -2, +2, -3, +4
  m <- hstat_metrics_reg(obs, pred)
  v <- function(nom) m$Valeur[m$Metrique == nom]

  expect_equal(v("RMSE"), round(sqrt(mean(c(2, 2, 3, 4)^2)), 4))   # 2,8723
  expect_equal(v("MAE"),  round(mean(c(2, 2, 3, 4)), 4))           # 2,75
  expect_equal(v("MAPE (%)"), 12.5)
  expect_equal(v("R2"), 0.934)

  # RMSE ET MAE NE SONT PAS INTERCHANGEABLES : la premiere penalise les grosses
  # erreurs, et lui vaut donc strictement plus des que les erreurs sont
  # inegales. C'est l'assertion qui distingue les deux formules.
  expect_gt(v("RMSE"), v("MAE"))
  # A erreurs EGALES, les deux coincident -- sinon on figerait un ecart plutot
  # qu'une regle.
  e <- hstat_metrics_reg(c(0, 10, 20), c(3, 13, 23))
  expect_equal(e$Valeur[e$Metrique == "RMSE"], e$Valeur[e$Metrique == "MAE"])

  # Un R2 parfait, et une prediction sans erreur.
  p <- hstat_metrics_reg(obs, obs)
  expect_equal(p$Valeur[p$Metrique == "R2"], 1)
  expect_equal(p$Valeur[p$Metrique == "RMSE"], 0)
})

test_that("les echelons d'interpretation changent bien de palier", {
  # Un seuil deplace ne se voit pas : le texte reste une phrase francaise
  # plausible, et « toutes les interpretations sont non vides » passe toujours.
  # On verifie donc de part et d'autre de CHAQUE frontiere annoncee.
  # R2 : 0,3 / 0,5 / 0,7 / 0,9
  expect_match(.hstat_interp_r2(0.90), "Excellent")
  expect_match(.hstat_interp_r2(0.89), "Bon")
  expect_match(.hstat_interp_r2(0.70), "Bon")
  expect_match(.hstat_interp_r2(0.69), "Moyen")
  expect_match(.hstat_interp_r2(0.50), "Moyen")
  expect_match(.hstat_interp_r2(0.49), "Faible")
  expect_match(.hstat_interp_r2(0.30), "Faible")
  expect_match(.hstat_interp_r2(0.29), "Très faible")
  expect_match(.hstat_interp_r2(NaN),  "Non calculable")

  # MAPE : 10 / 20 / 50, bornes STRICTES dans l'autre sens.
  expect_match(.hstat_interp_mape(9.99), "Excellente")
  expect_match(.hstat_interp_mape(10),   "Bonne")
  expect_match(.hstat_interp_mape(19.99), "Bonne")
  expect_match(.hstat_interp_mape(20),   "moyenne")
  expect_match(.hstat_interp_mape(49.99), "moyenne")
  expect_match(.hstat_interp_mape(50),   "faible")
  expect_match(.hstat_interp_mape(NA_real_), "non calculable")
})

test_that("hstat_metrics_cls gere binaire, multiclasse et matrice de confusion", {
  set.seed(2)
  y  <- factor(sample(c("A", "B"), 300, TRUE))
  py <- y; flip <- sample(300, 45)
  py[flip] <- factor(ifelse(y[flip] == "A", "B", "A"), levels = levels(y))
  mc <- hstat_metrics_cls(y, py)
  expect_equal(mc$Valeur[mc$Metrique == "Exactitude (accuracy)"], 0.85,
               tolerance = 1e-6)
  expect_true(all(nzchar(mc$Interpretation)))
  y3 <- factor(sample(c("X", "Y", "Z"), 300, TRUE))
  mc3 <- hstat_metrics_cls(y3, y3)
  expect_equal(mc3$Valeur[mc3$Metrique == "Exactitude (accuracy)"], 1)
  cm <- hstat_confusion_df(y, py)
  expect_equal(sum(as.matrix(cm)), 300)
})

test_that("precision, rappel, F1 et kappa valent chacun leur propre formule", {
  # MEME FAMILLE QUE LES METRIQUES DE REGRESSION : les libelles etaient
  # verifies, les valeurs non. Echanger precision et rappel, remplacer la
  # moyenne HARMONIQUE du F1 par une moyenne arithmetique, ou retirer la garde
  # de kappa ne faisait echouer aucune assertion. Trouve par mutation.
  #
  # La matrice est volontairement ASYMETRIQUE -- sur une matrice equilibree,
  # precision et rappel coincident et l'echange ne se verrait pas.
  #        pred
  #   obs   A  B      colonnes : 13 et 7
  #     A   8  2      lignes   : 10 et 10
  #     B   5  5
  obs  <- factor(c(rep("A", 10), rep("B", 10)))
  pred <- factor(c(rep("A", 8), rep("B", 2), rep("A", 5), rep("B", 5)))
  m <- hstat_metrics_cls(obs, pred)
  v <- function(nom) m$Valeur[m$Metrique == nom]

  # PRECISION = diagonale / COLONNE (parmi les predits A, combien le sont).
  prec <- mean(c(8 / 13, 5 / 7))
  # RAPPEL = diagonale / LIGNE (parmi les vrais A, combien sont retrouves).
  rec  <- mean(c(8 / 10, 5 / 10))
  expect_equal(v("Précision (macro)"), round(prec, 4))
  expect_equal(v("Rappel / sensibilité (macro)"), round(rec, 4))
  # Et les deux DIFFERENT ici : c'est ce qui rend l'echange visible.
  expect_false(isTRUE(all.equal(v("Précision (macro)"),
                                v("Rappel / sensibilité (macro)"))))

  # F1 = moyenne HARMONIQUE par classe, puis moyenne des classes. La moyenne
  # arithmetique donnerait 0,6574 : plausible, et faux.
  f1 <- mean(c(2 * (8/13) * (8/10) / ((8/13) + (8/10)),
               2 * (5/7)  * (5/10) / ((5/7)  + (5/10))))
  expect_equal(v("F1-score (macro)"), round(f1, 4))

  expect_equal(v("Exactitude (accuracy)"), 0.65)
  # kappa = (acc - pe) / (1 - pe), pe = (10*13 + 10*7) / 20^2 = 0,5
  expect_equal(v("Kappa de Cohen"), round((0.65 - 0.5) / 0.5, 4))

  # KAPPA N'EST PAS TOUJOURS DEFINI. Si les deux cotes ne portent qu'une seule
  # classe, l'accord attendu par hasard vaut 1, le denominateur s'annule et la
  # division rend NaN. Meme regle que pour l'accord inter-codeurs : on rend NA,
  # et l'exactitude, elle, reste calculable.
  u <- hstat_metrics_cls(factor(rep("A", 6)), factor(rep("A", 6)))
  k <- u$Valeur[u$Metrique == "Kappa de Cohen"]
  expect_true(is.na(k))
  expect_false(is.nan(k))
  expect_equal(u$Valeur[u$Metrique == "Exactitude (accuracy)"], 1)
})

test_that("hstat_model_interpretation produit un texte substantiel", {
  m <- hstat_metrics_reg(1:50, (1:50) + rnorm(50, 0, 2))
  txt <- hstat_model_interpretation("regression", m, "test", 100, 50)
  expect_true(is.character(txt) && nchar(txt) > 80)
  expect_match(txt, "généralisation")
})

test_that("hstat_align_newdata convertit types, niveaux et colonnes manquantes", {
  ref <- data.frame(a = rnorm(10),
                    b = factor(rep(c("u", "v"), 5)))
  nd  <- data.frame(a = c("1.5", "2.5"), b = c("u", "w"))
  al  <- hstat_align_newdata(nd, ref, c("a", "b"))
  expect_true(is.numeric(al$data$a))
  expect_true(is.factor(al$data$b))
  expect_true(is.na(al$data$b[2]))        # modalite inconnue -> NA
  expect_false(is.null(al$warn))
  bad <- hstat_align_newdata(data.frame(a = 1), ref, c("a", "b"))
  expect_null(bad$data)
  expect_match(bad$warn, "manquantes")
})

test_that("le garde-fou d'export plafonne les pixels sans toucher au DPI", {
  max_px <- 16000
  for (dpi in c(300, 5000, 20000)) {
    w <- 10; h <- 6
    scale <- min(1, max_px / (w * dpi), max_px / (h * dpi))
    expect_lte(w * scale * dpi, max_px + 1e-6)
    expect_lte(h * scale * dpi, max_px + 1e-6)
    expect_gt(scale, 0)
  }
})

test_that("HSTAT_ML_MAX_N est defini et raisonnable", {
  expect_true(is.integer(HSTAT_ML_MAX_N) || is.numeric(HSTAT_ML_MAX_N))
  expect_gte(HSTAT_ML_MAX_N, 1000)
})


# =============================================================================
#  v0.6.0 -- Fiches modeles, seuils des metriques
# =============================================================================

test_that("hstat_model_doc couvre tous les modeles et fournit les 4 champs", {
  ids <- c("naive","snaive","meanf","drift","ses","holt","holtd","hwadd","hwmul",
           "ets","arima","sarima","tbats","theta","stlf","nnetar","dlmts","dlnm",
           "prophet","lmglm","glmnet","rpart","rf","xgb","svm","knn","nb","nnet",
           "kmeans","hclust","pam","dbscan","mclust","dl_neuralnet","dl_torch","lstm")
  for (id in ids) {
    f <- hstat_model_doc(id)
    expect_false(is.null(f), info = id)
    expect_true(all(c("nom","principe","objectif","conditions") %in% names(f)),
                info = id)
    # Le NOM peut legitimement etre court ("TBATS", "Prophet", "DBSCAN",
    # "k-means") : on exige seulement qu'il soit renseigne. Ce sont les trois
    # champs redactionnels qui doivent etre substantiels.
    expect_true(nzchar(f$nom), info = id)
    expect_true(all(nchar(unlist(f[c("principe","objectif","conditions")])) > 10),
                info = id)
  }
  expect_null(hstat_model_doc("modele_inexistant"))
})

test_that("les tableaux de metriques exposent une colonne Seuils renseignee", {
  m <- hstat_metrics_reg(1:50, (1:50) + rnorm(50))
  expect_true("Seuils" %in% names(m))
  expect_true(all(nzchar(m$Seuils)))
  y <- factor(rep(c("A","B"), 25))
  mc <- hstat_metrics_cls(y, y)
  expect_true("Seuils" %in% names(mc))
  expect_true(all(nzchar(mc$Seuils)))
  expect_match(mc$Seuils[mc$Metrique == "Kappa de Cohen"], "Landis")
})

# =============================================================================
#  v0.7.0 -- Comparaison a une valeur de reference & tailles de labels
# =============================================================================

test_that("le test t a un echantillon reproduit stats::t.test", {
  set.seed(42)
  x <- rnorm(40, mean = 5.4, sd = 1.1)
  r <- hstat_ref_test(x, mu = 5, method = "ttest")
  b <- stats::t.test(x, mu = 5)
  expect_equal(r$p.value, b$p.value)
  expect_equal(r$statistic, unname(b$statistic))
  expect_equal(r$parameter, unname(b$parameter))
  expect_equal(r$estimate, mean(x))
  expect_equal(r$reference, 5)
  expect_equal(r$effect, (mean(x) - 5) / stats::sd(x))   # d de Cohen
  expect_match(r$interpretation, "référence")
})

test_that("les alternatives unilaterales sont transmises aux tests", {
  set.seed(7)
  x <- rnorm(30, mean = 12, sd = 2)
  for (alt in c("two.sided", "greater", "less")) {
    r <- hstat_ref_test(x, mu = 10, method = "ttest", alternative = alt)
    expect_equal(r$p.value, stats::t.test(x, mu = 10, alternative = alt)$p.value)
    expect_identical(r$alternative, alt)
  }
  # Un ecart positif est plus significatif en "greater" qu'en "less".
  expect_lt(hstat_ref_test(x, mu = 10, method = "ttest", alternative = "greater")$p.value,
            hstat_ref_test(x, mu = 10, method = "ttest", alternative = "less")$p.value)
})

test_that("le test z utilise l'ecart-type de reference fourni", {
  set.seed(3)
  x <- rnorm(25, mean = 102, sd = 5)
  r <- hstat_ref_test(x, mu = 100, method = "ztest", sigma = 5)
  expect_equal(r$statistic, (mean(x) - 100) / (5 / sqrt(25)))
  expect_equal(r$p.value, 2 * stats::pnorm(-abs(r$statistic)))
  # Sans ecart-type de reference, le test doit refuser de s'executer.
  expect_error(hstat_ref_test(x, mu = 100, method = "ztest"), "écart-type")
  expect_error(hstat_ref_test(x, mu = 100, method = "ztest", sigma = -1), "écart-type")
})

test_that("le Chi2 de conformite d'une variance suit la loi attendue", {
  set.seed(11)
  x <- rnorm(31, mean = 0, sd = 3)
  r <- hstat_ref_test(x, mu = 0, method = "variance", sigma = 3)
  expect_equal(r$statistic, 30 * stats::var(x) / 9)
  expect_equal(r$parameter, 30)
  expect_equal(r$estimate, stats::var(x))
  expect_true(r$conf.low <= stats::var(x) && stats::var(x) <= r$conf.high)
  expect_error(hstat_ref_test(x, mu = 0, method = "variance"), "écart-type")
})

test_that("Wilcoxon signe et test du signe comparent la mediane a la norme", {
  set.seed(19)
  x <- c(rnorm(24, mean = 8), 40, 45)   # queue lourde : la mediane reste robuste
  w <- hstat_ref_test(x, mu = 8, method = "wilcoxon")
  s <- hstat_ref_test(x, mu = 8, method = "sign")
  expect_equal(w$estimate, stats::median(x))
  expect_equal(s$estimate, stats::median(x))
  expect_equal(s$p.value,
               stats::binom.test(sum(x > 8), sum(x != 8), 0.5)$p.value)
  expect_true(is.na(s$parameter))   # « ddl » n'a pas de sens pour un test exact
})

test_that("le TOST conclut a l'equivalence quand la marge est large", {
  set.seed(23)
  x <- rnorm(60, mean = 100.2, sd = 2)
  large <- hstat_ref_test(x, mu = 100, method = "tost", margin = 3)
  etroit <- hstat_ref_test(x, mu = 100, method = "tost", margin = 0.05)
  expect_lt(large$p.value, 0.05)      # equivalence demontree
  expect_gt(etroit$p.value, 0.05)     # equivalence non demontree
  expect_match(large$interpretation, "Équivalence démontrée")
  expect_match(etroit$interpretation, "NON démontrée")
  expect_error(hstat_ref_test(x, mu = 100, method = "tost"), "marge")
})

test_that("les tests de conformite d'une proportion reproduisent binom/prop.test", {
  b <- hstat_ref_prop_test(42, 100, p0 = 0.5, method = "binom")
  expect_equal(b$p.value, stats::binom.test(42, 100, 0.5)$p.value)
  expect_equal(b$estimate, 0.42)
  p <- hstat_ref_prop_test(42, 100, p0 = 0.5, method = "prop")
  expect_equal(p$p.value,
               suppressWarnings(stats::prop.test(42, 100, p = 0.5))$p.value)
  # h de Cohen : nul quand la proportion observee vaut la reference.
  expect_equal(hstat_ref_prop_test(50, 100, p0 = 0.5, method = "binom")$effect, 0)
  # Un effectif faible declenche l'avertissement sur l'approximation normale.
  expect_match(hstat_ref_prop_test(1, 20, p0 = 0.02, method = "prop")$note, "exact")
  expect_error(hstat_ref_prop_test(120, 100, p0 = 0.5), "dépasser")
  expect_error(hstat_ref_prop_test(5, 100, p0 = 1.5), "entre 0 et 1")
})

test_that("le test de Poisson compare un taux d'evenements a la norme", {
  r <- hstat_ref_prop_test(15, 100, p0 = 0.10, method = "poisson")
  expect_equal(r$p.value, stats::poisson.test(15, T = 100, r = 0.10)$p.value)
  expect_equal(r$estimate, 0.15)
  expect_equal(r$effect, 1.5)          # rapport de taux
  expect_error(hstat_ref_prop_test(15, 100, p0 = 0, method = "poisson"), "positif")
})

test_that("hstat_ref_result_row produit les colonnes du tableau de resultats", {
  set.seed(5)
  r <- hstat_ref_test(rnorm(20, 3), mu = 2, method = "ttest")
  row <- hstat_ref_result_row(r, "teneur")
  expect_identical(names(row),
    c("Test","Variable","Facteur","Statistique","ddl","p_value","Interpretation"))
  expect_equal(nrow(row), 1)
  expect_identical(row$Variable, "teneur")
  expect_match(row$Facteur, "Référence")
})

test_that("les entrees invalides d'un test de conformite sont rejetees", {
  expect_error(hstat_ref_test(c(1, NA, Inf), mu = 0), "2 valeurs")
  expect_error(hstat_ref_test(rnorm(10), mu = NA), "nombre")
  expect_error(hstat_ref_test(rnorm(10), mu = 0, method = "inconnu"), "inconnue")
  expect_error(hstat_ref_test(rnorm(10), mu = 0, conf.level = 1.4), "confiance")
  # Les valeurs non finies sont ecartees sans faire echouer le test.
  r <- hstat_ref_test(c(rnorm(15, 5), NA, NaN), mu = 5, method = "ttest")
  expect_equal(r$n, 15)
})

test_that("la taille des labels part du defaut de ggplot2 et se convertit", {
  # Le plancher etait a 12 pt, puis a 11 : le curseur commencait alors au
  # defaut et ne permettait plus que d'AGRANDIR. Or sur un nuage de plusieurs
  # dizaines d'individus, ce sont des etiquettes plus PETITES qu'il faut.
  expect_equal(HSTAT_LBL_PT_MIN, 8)
  expect_lt(HSTAT_LBL_PT_MIN, HSTAT_GG_LABEL_PT)
  # Le defaut reste celui de ggplot2 : l'etat d'origine doit se retrouver sans
  # le chercher, et il doit rester atteignable par le curseur.
  expect_equal(HSTAT_LBL_PT_DEFAULT, HSTAT_GG_LABEL_PT)
  expect_gte(HSTAT_LBL_PT_DEFAULT, HSTAT_LBL_PT_MIN)
  expect_lte(HSTAT_LBL_PT_DEFAULT, HSTAT_LBL_PT_MAX)
  expect_equal(HSTAT_LBL_PT_MAX, 24)
  # Bornage des saisies hors domaine, absentes ou invalides.
  expect_equal(hstat_lbl_pt(3), HSTAT_LBL_PT_MIN)
  expect_equal(hstat_lbl_pt(8), 8)          # le plancher demande est atteignable
  expect_equal(hstat_lbl_pt(99), 24)
  expect_equal(hstat_lbl_pt(NULL), HSTAT_LBL_PT_DEFAULT)
  expect_equal(hstat_lbl_pt(NA), HSTAT_LBL_PT_DEFAULT)
  expect_equal(hstat_lbl_pt("18"), 18)
  # 1 pt = 1/72,27 pouce ; ggplot2 exprime la taille en mm.
  expect_equal(hstat_lbl_pt2gg(12), 12 / (72.27 / 25.4))
  expect_equal(hstat_lbl_pt2gg(24), 2 * hstat_lbl_pt2gg(12))
  expect_equal(hstat_lbl_pt2cex(12), 1)
  expect_equal(hstat_lbl_pt2cex(24), 2)
})

test_that("hstat_apply_label_sizes distingue les labels d'individus et de variables", {
  mk <- function(nr, cls) list(geom = structure(list(), class = c(cls, "Geom")),
                               data = data.frame(a = seq_len(nr)),
                               aes_params = list())
  sizes <- function(p) vapply(p$layers, function(l) {
    if (is.null(l$aes_params$size)) NA_real_ else l$aes_params$size
  }, numeric(1))
  p <- list(layers = list(mk(50, "GeomTextRepel"), mk(4, "GeomText"),
                          mk(50, "GeomPoint")))
  r <- hstat_apply_label_sizes(p, 4.2, 8.4, n_var = 4, n_ind = 50)
  expect_equal(sizes(r), c(4.2, 8.4, NA))          # points non touches
  # Individus et variables en nombres egaux : impossible de les distinguer,
  # tout le texte prend la taille des individus.
  r2 <- hstat_apply_label_sizes(p, 4.2, 8.4, n_var = 50, n_ind = 50)
  expect_equal(sizes(r2), c(4.2, 4.2, NA))
  # Une seule taille fournie : elle s'applique a tous les calques de texte.
  expect_equal(sizes(hstat_apply_label_sizes(p, 8.4)), c(8.4, 8.4, NA))
  # Plusieurs effectifs candidats (quanti / quali / groupes) : chacun compte,
  # et ceux qui coincident avec le nombre d'individus sont ecartes.
  p3 <- list(layers = list(mk(50, "GeomTextRepel"), mk(4, "GeomText"),
                           mk(7, "GeomText")))
  expect_equal(sizes(hstat_apply_label_sizes(p3, 4.2, 8.4,
                                             n_var = c(4, 7, 50), n_ind = 50)),
               c(4.2, 8.4, 8.4))
  expect_equal(sizes(hstat_apply_label_sizes(p3, 4.2, 8.4,
                                             n_var = c(NA, 0), n_ind = 50)),
               c(4.2, 4.2, 4.2))
  expect_null(hstat_apply_label_sizes(NULL, 4))
})

test_that("hstat_glm_note detecte non-convergence et separation", {
  # Modele sain : aucun diagnostic.
  set.seed(31)
  d <- data.frame(y = rbinom(80, 1, 0.5), x = rnorm(80))
  ok <- hstat_glm_fit(y ~ x, data = d)
  expect_true(inherits(ok$fit, "glm"))
  expect_null(ok$note)
  # Separation complete : x classe parfaitement y.
  ds <- data.frame(y = c(rep(0, 20), rep(1, 20)), x = c(rnorm(20, -8), rnorm(20, 8)))
  sep <- hstat_glm_fit(y ~ x, data = ds)
  expect_false(is.null(sep$note))
  expect_match(sep$note, "Séparation")
  expect_match(sep$note, "pénalisée")
})

test_that("la version citee suit DESCRIPTION et n'est jamais codee en dur", {
  desc <- .hstat_description_path()
  skip_if(is.na(desc), "DESCRIPTION introuvable depuis le repertoire de test")
  attendue <- unname(read.dcf(desc, fields = "Version")[1, 1])
  expect_true(nzchar(attendue))
  expect_identical(hstat_version(), attendue)
  # La version doit apparaitre telle quelle dans TOUS les styles de citation :
  # c'est ce qui garantit qu'aucun style ne retombe sur un numero fige.
  for (st in c("text", "apa", "vancouver", "markdown", "bibtex", "ris")) {
    cit <- hstat_citation(st)
    expect_true(nzchar(cit), info = st)
    expect_true(grepl(attendue, cit, fixed = TRUE), info = st)
  }
  # Le repli ne sert que si rien n'est trouvable : il ne doit jamais primer sur
  # DESCRIPTION, et ne doit pas etre un numero de version plausible.
  expect_false(identical(hstat_version(), "0.0.0"))
  expect_identical(hstat_version(fallback = "sentinelle"), attendue)
})

test_that("la resolution de version ne fuit ni erreur ni avertissement", {
  # packageVersion() leve une erreur et packageDate() un avertissement quand le
  # paquet n'est pas installe : les deux doivent rester silencieux.
  expect_silent(hstat_version())
  expect_silent(hstat_pkg_year())
  expect_silent(hstat_citation("text"))
  # L'annee est toujours une annee a 4 chiffres exploitable.
  expect_match(hstat_pkg_year(), "^[0-9]{4}$")
})

# =============================================================================
#  v0.7.4 -- Point d'entree de deploiement (app.R a la racine)
# =============================================================================

test_that("app.R sert bien www/ une fois deploye depuis la racine", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "racine du depot introuvable depuis le repertoire de test")
  app_r <- file.path(root, "app.R")
  skip_if_not(file.exists(app_r), "app.R absent (paquet installe)")
  # On inspecte le CODE seul : les commentaires de app.R expliquent justement
  # pourquoi setwd() est proscrit, et les inclure ferait echouer le test a tort.
  src <- paste(sub("#.*$", "", readLines(app_r, warn = FALSE)), collapse = "\n")

  # Shiny resout le dossier de l'application AVANT d'evaluer app.R : un setwd()
  # arrive trop tard et laisse www/ introuvable. L'app repondait alors 404 sur
  # hstat-theme.css, Sortable.min.js et les polices une fois deployee.
  expect_false(grepl("setwd\\s*\\(", src),
               info = "app.R ne doit pas utiliser setwd() : www/ ne serait plus servi")
  expect_true(grepl("shinyAppDir\\s*\\(", src),
              info = "app.R doit declarer le dossier de l'app via shinyAppDir()")

  # Les ressources statiques doivent exister la ou shinyAppDir() les cherchera.
  www <- file.path(root, "inst", "app", "www")
  expect_true(dir.exists(www))
  for (f in c("hstat-theme.css", "Sortable.min.js"))
    expect_true(file.exists(file.path(www, f)), info = f)
  expect_true(length(list.files(file.path(www, "fonts"), pattern = "\\.woff2$")) > 0)
})

test_that("Shiny ne source pas le dossier R/ du paquet dans l'application", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "racine du depot introuvable depuis le repertoire de test")
  skip_if_not(dir.exists(file.path(root, "R")), "dossier R/ absent (paquet installe)")
  # shiny::loadSupport() s'arrete si R/_disable_autoload.R existe ; sans lui,
  # run_hstat() etait injecte dans l'environnement de l'application.
  expect_true(file.exists(file.path(root, "R", "_disable_autoload.R")))
})

test_that("la version du README suit celle de DESCRIPTION", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "racine du depot introuvable depuis le repertoire de test")
  readme <- file.path(root, "README.md")
  skip_if_not(file.exists(readme), "README.md absent (paquet installe)")
  attendue <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])
  txt <- readLines(readme, warn = FALSE)

  # Le README est du markdown statique : il ne peut pas lire DESCRIPTION, son
  # numero doit donc y etre ecrit a la main. Ce test transforme cette etape
  # manuelle en garde-fou -- le README de citation etait reste bloque sur 0.6.0
  # alors que le paquet etait en 0.7.4.
  citees <- unique(unlist(regmatches(txt, gregexpr("Version [0-9]+\\.[0-9]+\\.[0-9]+", txt))))
  citees <- sub("^Version ", "", citees)
  expect_true(length(citees) > 0,
              info = "aucune version citee dans README.md : le bloc de citation a-t-il disparu ?")
  expect_identical(sort(citees), attendue,
                   info = paste0("README.md cite ", paste(citees, collapse = ", "),
                                 " alors que DESCRIPTION est en ", attendue))
})

# =============================================================================
#  ATELIER DE CODAGE QUALITATIF (CAQDAS) -- mod_coding.R
# =============================================================================

test_that("livre de codes : ajout, unicite, couleurs et suppression", {
  cb <- hstat_code_new_codebook()
  expect_equal(nrow(cb), 0L)

  cb <- hstat_code_add(cb, "Prix trop eleve")
  cb <- hstat_code_add(cb, "Satisfaction")
  expect_equal(nrow(cb), 2L)
  expect_equal(cb$code_id, c("prix_trop_eleve", "satisfaction"))
  # Deux codes ne doivent jamais partager une couleur tant que la palette suffit
  expect_equal(length(unique(cb$color)), 2L)

  # Libelle deja present (a la casse pres) : refuse en silence
  expect_equal(nrow(hstat_code_add(cb, "satisfaction")), 2L)
  # Libelle vide : refuse aussi
  expect_equal(nrow(hstat_code_add(cb, "   ")), 2L)

  # Couleur imposee
  cb <- hstat_code_add(cb, "Delais", color = "#123456")
  expect_equal(cb$color[cb$code_id == "delais"], "#123456")

  cb <- hstat_code_update(cb, "delais", label = "Delais de livraison", memo = "retards")
  expect_equal(hstat_code_label(cb, "delais"), "Delais de livraison")
  expect_equal(cb$memo[cb$code_id == "delais"], "retards")

  cb <- hstat_code_remove(cb, "delais")
  expect_equal(nrow(cb), 2L)
  # Un code inconnu retombe sur son identifiant plutot que sur NA
  expect_equal(hstat_code_label(cb, "inconnu"), "inconnu")
  expect_equal(hstat_code_color(cb, "inconnu"), "#95a5a6")
})

test_that("hstat_code_slug produit des identifiants uniques et sans accents", {
  expect_equal(hstat_code_slug("Prix tres eleve"), "prix_tres_eleve")
  expect_equal(hstat_code_slug("Qualite / Securite"), "qualite_securite")
  expect_equal(hstat_code_slug("Prix", existing = c("prix")), "prix_2")
  expect_equal(hstat_code_slug("Prix", existing = c("prix", "prix_2")), "prix_3")
  expect_equal(hstat_code_slug("!!!"), "code")
})

test_that("segments : ajout, dedoublonnage, suppression et effectifs", {
  cb <- hstat_code_add(hstat_code_add(hstat_code_new_codebook(), "Prix"), "Delai")
  sg <- hstat_code_new_segments()

  sg <- hstat_seg_add(sg, "D00001", "prix", 0, 10, "trop cher!!")
  expect_equal(nrow(sg), 1L)
  # Meme document, meme code, memes bornes : depot en double ignore
  sg <- hstat_seg_add(sg, "D00001", "prix", 0, 10, "trop cher!!")
  expect_equal(nrow(sg), 1L)
  # Selection vide ou inversee : refusee
  sg <- hstat_seg_add(sg, "D00001", "prix", 5, 5, "")
  sg <- hstat_seg_add(sg, "D00001", "prix", 9, 3, "")
  expect_equal(nrow(sg), 1L)

  sg <- hstat_seg_add(sg, "D00002", "prix", 2, 8, "cher")
  sg <- hstat_seg_add(sg, "D00002", "delai", 10, 20, "trop long")
  expect_equal(nrow(sg), 3L)

  cnt <- hstat_code_counts(cb, sg)
  expect_equal(cnt$n_seg[cnt$code_id == "prix"], 2L)
  expect_equal(cnt$n_doc[cnt$code_id == "prix"], 2L)
  expect_equal(cnt$n_seg[cnt$code_id == "delai"], 1L)

  expect_equal(nrow(hstat_seg_for_doc(sg, "D00002")), 2L)
  expect_equal(nrow(hstat_seg_remove(sg, sg$seg_id[1])), 2L)
  # Supprimer un code emporte ses etiquettes : pas de segment orphelin
  expect_equal(nrow(hstat_seg_drop_code(sg, "prix")), 1L)
})

test_that("hstat_code_highlight_html balise le bon passage et echappe le HTML", {
  cb <- hstat_code_add(hstat_code_new_codebook(), "Prix", color = "#e74c3c")
  txt <- "Le prix est trop eleve"
  # "prix" occupe les positions 3 a 7 (bornes JS : debut a 0, fin exclue)
  sg <- hstat_seg_add(hstat_code_new_segments(), "D1", "prix", 3, 7, "prix")

  h <- hstat_code_highlight_html(txt, sg, cb)
  expect_true(grepl("<mark", h, fixed = TRUE))
  expect_true(grepl(">prix</mark>", h, fixed = TRUE))
  expect_true(grepl("231,76,60", h, fixed = TRUE))   # #e74c3c en rgba
  # Le texte hors segment reste intact
  expect_true(grepl("Le ", h, fixed = TRUE))
  expect_true(grepl(" est trop eleve", h, fixed = TRUE))

  # Sans segment : simple echappement, aucune balise <mark>
  expect_false(grepl("<mark", hstat_code_highlight_html(txt, NULL, cb), fixed = TRUE))

  # Le texte du repondant ne doit jamais pouvoir injecter du HTML
  h2 <- hstat_code_highlight_html("a <script>x</script> b",
                                  hstat_code_new_segments(), cb)
  expect_false(grepl("<script>", h2, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", h2, fixed = TRUE))

  # Texte vide
  expect_true(grepl("vide", hstat_code_highlight_html("", NULL, cb)))
})

test_that("hstat_code_highlight_html gere les chevauchements", {
  cb <- hstat_code_add(hstat_code_add(hstat_code_new_codebook(), "A", color = "#e74c3c"),
                       "B", color = "#2980b9")
  txt <- "0123456789"
  sg <- hstat_seg_add(hstat_code_new_segments(), "D1", "a", 0, 6, "012345")
  sg <- hstat_seg_add(sg, "D1", "b", 4, 10, "456789")

  h <- hstat_code_highlight_html(txt, sg, cb)
  # Le texte affiche doit rester exactement le texte d'origine, balises otees
  expect_equal(gsub("<[^>]*>", "", h), txt)
  # La zone commune (4-6) recoit un degrade des deux couleurs
  expect_true(grepl("linear-gradient", h, fixed = TRUE))
  expect_true(grepl("A + B", h, fixed = TRUE))

  # Bornes hors du texte : ramenees dans les limites, sans erreur
  sg2 <- hstat_seg_add(hstat_code_new_segments(), "D1", "a", 5, 999, "x")
  expect_equal(gsub("<[^>]*>", "", hstat_code_highlight_html(txt, sg2, cb)), txt)
})

test_that("hstat_code_docs ne retient que les lignes non vides", {
  df <- data.frame(rep = c("trop cher", "", NA, "service parfait"),
                   age = c("<25", "25-40", "<25", ">40"),
                   stringsAsFactors = FALSE)
  d <- hstat_code_docs(df, "rep")
  expect_equal(nrow(d), 2L)
  expect_equal(d$row, c(1L, 4L))
  expect_equal(d$text, c("trop cher", "service parfait"))
  # Le profil suit les documents retenus, dans le meme ordre
  p <- hstat_code_profile(df, d, "age")
  expect_equal(p$age, c("<25", ">40"))
  # Colonne inexistante : tableau vide plutot qu'erreur
  expect_equal(nrow(hstat_code_docs(df, "absente")), 0L)
})

test_that("hstat_code_retrieve filtre par code, par profil et par mot-cle", {
  df <- data.frame(rep = c("c'est trop cher", "service parfait", "prix excessif"),
                   age = c("<25", ">40", "<25"), stringsAsFactors = FALSE)
  d  <- hstat_code_docs(df, "rep")
  pr <- hstat_code_profile(df, d, "age")
  cb <- hstat_code_add(hstat_code_add(hstat_code_new_codebook(), "Prix"), "Service")
  sg <- hstat_seg_add(hstat_code_new_segments(), d$doc_id[1], "prix", 7, 15, "trop cher")
  sg <- hstat_seg_add(sg, d$doc_id[2], "service", 0, 15, "service parfait")
  sg <- hstat_seg_add(sg, d$doc_id[3], "prix", 0, 14, "prix excessif")

  expect_equal(nrow(hstat_code_retrieve(sg, cb, d)), 3L)
  expect_equal(nrow(hstat_code_retrieve(sg, cb, d, code_ids = "prix")), 2L)

  # « Les critiques sur le prix emises par les moins de 25 ans »
  r <- hstat_code_retrieve(sg, cb, d, code_ids = "prix", profile = pr,
                           filter_var = "age", filter_levels = "<25")
  expect_equal(nrow(r), 2L)
  expect_true(all(r$age == "<25"))
  expect_true("Extrait" %in% names(r))

  r2 <- hstat_code_retrieve(sg, cb, d, profile = pr, filter_var = "age",
                            filter_levels = ">40")
  expect_equal(nrow(r2), 1L)
  expect_equal(r2$Code, "Service")

  expect_equal(nrow(hstat_code_retrieve(sg, cb, d, search = "excessif")), 1L)
  expect_equal(nrow(hstat_code_retrieve(hstat_code_new_segments(), cb, d)), 0L)
})

test_that("hstat_code_matrix croise les codes et les profils", {
  df <- data.frame(rep = c("a", "b", "c"), age = c("<25", "<25", ">40"),
                   stringsAsFactors = FALSE)
  d  <- hstat_code_docs(df, "rep")
  pr <- hstat_code_profile(df, d, "age")
  cb <- hstat_code_add(hstat_code_add(hstat_code_new_codebook(), "Prix"), "Service")
  sg <- hstat_seg_add(hstat_code_new_segments(), d$doc_id[1], "prix", 0, 1, "a")
  sg <- hstat_seg_add(sg, d$doc_id[1], "prix", 0, 1, "a")   # doublon : ignore
  sg <- hstat_seg_add(sg, d$doc_id[2], "prix", 0, 1, "b")
  sg <- hstat_seg_add(sg, d$doc_id[3], "service", 0, 1, "c")

  m <- hstat_code_matrix(sg, cb, pr, "age", count = "segments")
  expect_equal(m$Code, c("Prix", "Service"))
  expect_equal(m[["<25"]], c(2L, 0L))
  expect_equal(m[[">40"]], c(0L, 1L))
  expect_equal(m$Total, c(2, 1))

  # Sans variable de profil : simple colonne d'effectifs
  m0 <- hstat_code_matrix(sg, cb, pr, "")
  expect_equal(names(m0), c("Code", "Total"))
  expect_equal(m0$Total, c(2L, 1L))

  # Comptage par repondant : deux segments dans le meme document pesent 1
  sg2 <- hstat_seg_add(sg, d$doc_id[1], "prix", 5, 9, "zzzz")
  expect_equal(hstat_code_matrix(sg2, cb, pr, "age", count = "segments")[["<25"]][1], 3L)
  expect_equal(hstat_code_matrix(sg2, cb, pr, "age", count = "documents")[["<25"]][1], 2L)

  expect_null(hstat_code_matrix(sg, hstat_code_new_codebook(), pr, "age"))
})

test_that("hstat_code_cooccurrence distingue meme reponse et meme passage", {
  cb <- hstat_code_add(hstat_code_add(hstat_code_new_codebook(), "A"), "B")
  # Deux codes dans la meme reponse, mais sur des passages disjoints
  sg <- hstat_seg_add(hstat_code_new_segments(), "D1", "a", 0, 5, "xxxxx")
  sg <- hstat_seg_add(sg, "D1", "b", 20, 25, "yyyyy")

  m_doc <- hstat_code_cooccurrence(sg, cb, mode = "document")
  expect_equal(m_doc["A", "B"], 1L)
  m_ov <- hstat_code_cooccurrence(sg, cb, mode = "overlap")
  expect_equal(m_ov["A", "B"], 0L)

  # Passages qui se recouvrent
  sg2 <- hstat_seg_add(hstat_code_new_segments(), "D1", "a", 0, 10, "x")
  sg2 <- hstat_seg_add(sg2, "D1", "b", 5, 15, "y")
  expect_equal(hstat_code_cooccurrence(sg2, cb, mode = "overlap")["A", "B"], 1L)
  # Matrice symetrique, diagonale nulle
  m2 <- hstat_code_cooccurrence(sg2, cb, mode = "overlap")
  expect_equal(m2["A", "B"], m2["B", "A"])
  expect_equal(unname(diag(m2)), c(0L, 0L))

  # Moins de deux codes : rien a croiser
  expect_null(hstat_code_cooccurrence(sg, hstat_code_add(hstat_code_new_codebook(), "A")))
})

test_that("la mise en page du nuage de mots ne superpose aucun mot", {
  set.seed(1)
  w <- c("prix", "cher", "service", "qualite", "delai", "accueil", "attente")
  f <- c(30, 25, 20, 12, 8, 5, 3)
  lay <- hstat_code_cloud_layout(w, f, max_words = 10, min_size = 4, max_size = 14)
  expect_true(nrow(lay) >= 5)
  expect_true(all(is.finite(lay$x)) && all(is.finite(lay$y)))
  # Le mot le plus frequent est le plus gros et occupe le centre
  expect_equal(lay$word[1], "prix")
  expect_equal(lay$size[1], max(lay$size))
  expect_equal(lay$x[1], 0)

  # Aucun recouvrement des boites englobantes
  bw <- nchar(lay$word) * lay$size * 0.62
  bh <- lay$size * 1.35
  for (i in seq_len(nrow(lay) - 1L)) for (j in (i + 1L):nrow(lay)) {
    ov <- abs(lay$x[i] - lay$x[j]) < (bw[i] + bw[j]) / 2 &&
          abs(lay$y[i] - lay$y[j]) < (bh[i] + bh[j]) / 2
    expect_false(ov, info = sprintf("« %s » chevauche « %s »", lay$word[i], lay$word[j]))
  }

  # Plafond du nombre de mots respecte
  expect_true(nrow(hstat_code_cloud_layout(w, f, max_words = 3)) <= 3)
  expect_null(hstat_code_cloud_layout(character(0), numeric(0)))
})

test_that("la carte conceptuelle place autant de noeuds que de codes", {
  cb <- hstat_code_new_codebook()
  for (l in c("Prix", "Service", "Delai", "Qualite")) cb <- hstat_code_add(cb, l)
  sg <- hstat_code_new_segments()
  sg <- hstat_seg_add(sg, "D1", "prix", 0, 5, "a")
  sg <- hstat_seg_add(sg, "D1", "service", 6, 9, "b")
  sg <- hstat_seg_add(sg, "D2", "prix", 0, 5, "a")
  sg <- hstat_seg_add(sg, "D2", "delai", 6, 9, "c")
  sg <- hstat_seg_add(sg, "D3", "qualite", 0, 5, "d")

  m <- hstat_code_cooccurrence(sg, cb)
  cnt <- hstat_code_counts(cb, sg)
  lay <- hstat_code_map_layout(m, stats::setNames(cnt$n_seg, cnt$label))
  expect_equal(nrow(lay), 4L)
  expect_true(all(is.finite(lay$x)) && all(is.finite(lay$y)))
  expect_setequal(lay$label, cb$label)

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- hstat_code_map_plot(m, cb, cnt)
    expect_s3_class(p, "ggplot")
  }
  # Aucune cooccurrence : repli sur la disposition circulaire, sans erreur
  vide <- hstat_code_cooccurrence(hstat_code_new_segments(), cb)
  expect_equal(nrow(hstat_code_map_layout(vide, stats::setNames(rep(1, 4), cb$label))), 4L)
})

test_that("le moteur hors ligne est toujours disponible, sans cle ni reseau", {
  old <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA)
  Sys.unsetenv("ANTHROPIC_API_KEY")
  on.exit(if (!is.na(old)) Sys.setenv(ANTHROPIC_API_KEY = old), add = TRUE)

  # C'est la garantie centrale : le moteur « auto » ne depend de rien.
  st <- hstat_ai_status("auto")
  expect_true(st$ok)
  expect_true(grepl("sans réseau", st$message, fixed = TRUE))
})

test_that("le moteur par defaut est gratuit, jamais une API payante", {
  # Le premier choix propose et le defaut des deux fonctions doivent rester la
  # thematisation automatique : gratuite, hors ligne, sans cle. Une
  # fonctionnalite facturee a l'usage ne doit jamais devenir le chemin par
  # defaut d'un utilisateur qui n'a rien demande.
  expect_equal(unname(HSTAT_AI_ENGINES[1]), "auto")
  expect_equal(formals(hstat_ai_call)$engine, "auto")
  expect_equal(formals(hstat_ai_status)$engine, "auto")

  # Les moteurs gratuits viennent AVANT les payants dans la liste de choix.
  paye <- vapply(HSTAT_AI_FOURNISSEURS, function(f) isTRUE(f$paye), logical(1))
  expect_false(any(paye[seq_len(sum(!paye))]))

  # Les services demandes sont tous proposes, Claude compris et sans privilege.
  for (id in c("claude", "chatgpt", "deepseek", "gemini", "copilot", "kimi"))
    expect_true(id %in% HSTAT_AI_ENGINES, label = paste("fournisseur :", id))

  # Ollama a ete retire : ni moteur, ni protocole, ni constructeur de corps.
  #
  # Le balayage porte sur le CODE, commentaires retires par l'analyseur de R :
  # ce test s'est signale lui-meme sur le commentaire qui documente le retrait
  # et qui cite les points d'entree disparus. Un faux positif permanent finit
  # toujours par faire desactiver le test.
  sans_com <- function(f) {
    paste(.hstat_code_lignes(f), collapse = "\n")
  }
  src <- sans_com(.hstat_module_path("mod_ai.R"))
  ui  <- sans_com(.hstat_module_path("mod_coding.R"))
  expect_false(grepl("api/tags", src, fixed = TRUE))
  expect_false(grepl(".hstat_ai_call_ollama", src, fixed = TRUE))
  expect_false(grepl("HSTAT_AI_BACKENDS", paste(src, ui), fixed = TRUE))
  expect_false(grepl("ollama pull", ui, fixed = TRUE))
  expect_false(exists("hstat_ai_ollama_models"))
})

test_that("chaque fournisseur est complet et son protocole implemente", {
  for (id in names(HSTAT_AI_FOURNISSEURS)) {
    f <- HSTAT_AI_FOURNISSEURS[[id]]
    expect_true(nzchar(f$label), label = paste("libelle :", id))
    expect_true(f$protocole %in% c("auto", "openai", "anthropic", "gemini"),
                label = paste("protocole :", id))
    # Un service en ligne se reconnait a sa variable d'environnement : il doit
    # alors dire OU obtenir la cle, sinon le message n'est pas actionnable.
    if (isTRUE(f$paye)) {
      expect_true(nzchar(f$cle_env), label = paste("variable :", id))
      expect_true(nzchar(f$cle_url %||% ""), label = paste("ou obtenir la cle :", id))
      expect_true(nzchar(f$modele), label = paste("modele par defaut :", id))
      expect_true(grepl("^https://", f$url), label = paste("adresse https :", id))
    }
    # L'adresse d'un moteur local reste sur la machine de l'utilisateur.
    if (identical(id, "local"))
      expect_true(grepl("^http://127\\.0\\.0\\.1:", f$url))
  }
  # Chaque cle d'environnement est propre au service : une cle OpenAI ne doit
  # pas servir a appeler DeepSeek.
  env <- vapply(HSTAT_AI_FOURNISSEURS, function(f) f$cle_env %||% "", character(1))
  env <- env[nzchar(env)]
  expect_equal(length(env), length(unique(env)))

  # Et aucune variable d'environnement AMBIANTE : GITHUB_TOKEN existe sur
  # quantite de postes et dans toutes les integrations continues. La lire
  # d'office enverrait un jeton chez un tiers sans acte de l'utilisateur --
  # constate a l'ecran, le moteur s'annoncait « disponible » tout seul.
  expect_false("GITHUB_TOKEN" %in% env)
})

test_that("clé d'API : celle du service, jamais celle d'un autre", {
  for (v in c("ANTHROPIC_API_KEY", "OPENAI_API_KEY", "DEEPSEEK_API_KEY",
              "GEMINI_API_KEY", "GITHUB_MODELS_TOKEN", "MOONSHOT_API_KEY"))
    Sys.unsetenv(v)

  expect_equal(hstat_ai_key("claude", NULL), "")
  expect_equal(hstat_ai_key("claude", "  sk-test  "), "sk-test")
  expect_false(hstat_ai_available(NULL))

  st <- hstat_ai_status("claude")
  expect_false(st$ok)
  expect_true(grepl("clé d'API", st$message, fixed = TRUE))
  expect_true(grepl("ANTHROPIC_API_KEY", st$message, fixed = TRUE))
  # Le message oriente vers le moteur gratuit
  expect_true(grepl("gratuite et hors ligne", st$message, fixed = TRUE))

  # Aucun appel reseau ne doit partir sans cle, quel que soit le service
  for (id in c("claude", "chatgpt", "gemini")) {
    r <- hstat_ai_call("bonjour", engine = id, api_key = NULL, timeout = 3)
    expect_false(isTRUE(r$ok), label = paste("sans cle :", id))
    expect_true(nzchar(r$error), label = paste("message :", id))
  }

  # La variable d'environnement du service est lue, et elle seule.
  Sys.setenv(OPENAI_API_KEY = "sk-openai")
  expect_equal(hstat_ai_key("chatgpt", NULL), "sk-openai")
  expect_equal(hstat_ai_key("deepseek", NULL), "")     # pas de fuite d'un service a l'autre
  expect_equal(hstat_ai_key("auto", NULL), "")
  Sys.unsetenv("OPENAI_API_KEY")
})

# -- UNE CLE D'AMBIANCE NE SUIT PAS UNE ADRESSE CHANGEE ----------------------
# Deux decisions justes se combinaient mal : l'adresse du service est un champ
# TEXTE LIBRE (le test voisin le garde, et c'est voulu), et le champ de cle
# INVITE a rester vide pour que la variable d'environnement serve. Les deux se
# resolvaient independamment.
#
# Sur un deploiement partage -- ceux que le README prevoit -- l'exploitant pose
# la cle dans l'environnement du serveur. N'importe quel visiteur choisissait
# alors le moteur, laissait la cle vide, remplacait l'adresse par la sienne, et
# recevait la cle du serveur dans l'en-tete `x-api-key`. Constate avec un
# serveur de capture : la cle partait en clair.
#
# L'assertion doit distinguer les DEUX cotes, sinon elle ne garde rien : une
# fonction qui refuserait TOUJOURS le repli passerait la moitie « adresse
# changee » et casserait le cas ordinaire, qui est justement celui que le repli
# sert.
test_that("la cle d'environnement ne part pas a une adresse changee", {
  Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-cle-du-serveur")
  on.exit(Sys.unsetenv("ANTHROPIC_API_KEY"), add = TRUE)

  # 1. L'ATTAQUE : adresse changee, champ de cle vide -> rien ne part.
  expect_equal(hstat_ai_key("claude", NULL, "http://127.0.0.1:8099"), "")
  expect_equal(hstat_ai_key("claude", NULL, "https://attaquant.example"), "")

  # 2. LE CAS ORDINAIRE, qui ne doit pas casser : adresse du fournisseur.
  expect_equal(hstat_ai_key("claude", NULL), "sk-ant-cle-du-serveur")
  expect_equal(hstat_ai_key("claude", NULL, NULL), "sk-ant-cle-du-serveur")
  # Un champ vide, ou une barre finale, ne sont PAS un changement d'adresse :
  # `hstat_ai_url()` les resout tous deux sur celle du fournisseur.
  expect_equal(hstat_ai_key("claude", NULL, "   "), "sk-ant-cle-du-serveur")
  expect_equal(hstat_ai_key("claude", NULL, "https://api.anthropic.com/"),
               "sk-ant-cle-du-serveur")

  # 3. UNE CLE SAISIE part toujours ou son proprietaire l'envoie : c'est la
  #    sienne. Le pouvoir de deplacer l'adresse reste entier.
  expect_equal(hstat_ai_key("claude", "sk-a-moi", "http://127.0.0.1:8099"),
               "sk-a-moi")

  # 4. Le diagnostic le DIT, il ne se contente pas de laisser l'appel echouer :
  #    un service annonce « disponible » qui ne peut pas partir serait pire.
  st <- hstat_ai_status("claude", url = "http://127.0.0.1:8099")
  expect_false(isTRUE(st$ok))
  expect_true(grepl("clé d'API", st$message, fixed = TRUE))

  # 5. Et l'appel complet ne part pas non plus -- c'est le chemin reellement
  #    emprunte, celui par lequel la cle fuyait.
  r <- hstat_ai_call("test", engine = "claude",
                     url = "http://127.0.0.1:8099", api_key = NULL, timeout = 3)
  expect_false(isTRUE(r$ok))

  # L'aide qui porte la regle se lit seule, dans les deux sens.
  expect_true(hstat_ai_url_attendue("claude", NULL))
  expect_true(hstat_ai_url_attendue("claude", "https://api.anthropic.com"))
  expect_false(hstat_ai_url_attendue("claude", "http://127.0.0.1:8099"))
})

test_that("adresses et modeles par defaut, modifiables", {
  # 127.0.0.1 et non localhost : on veut que ce soit visiblement la machine
  # de l'utilisateur, et rien d'autre.
  expect_true(grepl("^http://127\\.0\\.0\\.1:8080$", hstat_ai_url("local")))
  expect_equal(hstat_ai_url("local", "http://192.168.1.5:8080/"),
               "http://192.168.1.5:8080")
  expect_equal(hstat_ai_url("local", "   "), "http://127.0.0.1:8080")
  # Une adresse saisie l'emporte toujours : un service qui demenage ne doit pas
  # obliger a rouvrir le code.
  expect_equal(hstat_ai_url("chatgpt", "https://proxy.interne/v1"),
               "https://proxy.interne/v1")
  # Idem pour le modele.
  expect_equal(hstat_ai_modele("chatgpt"), HSTAT_AI_FOURNISSEURS$chatgpt$modele)
  expect_equal(hstat_ai_modele("chatgpt", "gpt-4o-mini"), "gpt-4o-mini")
  expect_equal(hstat_ai_modele("chatgpt", "   "), HSTAT_AI_FOURNISSEURS$chatgpt$modele)
  # Un identifiant inconnu retombe sur "auto", jamais sur une API payante.
  expect_equal(hstat_ai_fournisseur("service-inexistant")$protocole, "auto")
  expect_equal(hstat_ai_fournisseur(NULL)$protocole, "auto")
})

test_that("serveur local injoignable : message actionnable, jamais d'erreur", {
  skip_if_not(requireNamespace("httr", quietly = TRUE))
  # Port volontairement ferme
  st <- hstat_ai_status("local", "http://127.0.0.1:9")
  expect_false(st$ok)
  expect_true(grepl("Aucun modèle joignable", st$message, fixed = TRUE))
  expect_equal(hstat_ai_models("local", "http://127.0.0.1:9", timeout = 2),
               character(0))

  r <- hstat_ai_call("bonjour", engine = "local", url = "http://127.0.0.1:9",
                     model = "inexistant", timeout = 3)
  expect_false(r$ok)
  expect_true(nzchar(r$error))
})

test_that("les reglages affiches suivent le moteur choisi", {
  skip_if_not_installed("shiny")
  # L'interface derive de la table : sept `conditionalPanel` ecrits a la main
  # devenaient faux des qu'on ajoutait une ligne, ce qui est precisement ce
  # qu'on veut pouvoir faire.
  ids <- function(engine, prefixe = "") {
    h <- as.character(hstat_ai_reglages_ui(identity, engine, prefixe))
    regmatches(h, gregexpr('id="[^"]+"', h))[[1]]
  }

  # « auto » ne demande rien : ni cle, ni adresse, ni modele.
  expect_length(ids("auto"), 0)

  # Un serveur local : adresse et modele, mais AUCUN champ de cle -- il n'en
  # faut pas, et en demander une laisserait croire le contraire.
  loc <- ids("local")
  expect_true(any(grepl('id="url"', loc, fixed = TRUE)))
  expect_true(any(grepl('id="model"', loc, fixed = TRUE)))
  expect_false(any(grepl('id="key"', loc, fixed = TRUE)))

  # Chaque service en ligne : cle, adresse, modele.
  for (id in c("claude", "chatgpt", "deepseek", "gemini", "copilot", "kimi")) {
    x <- ids(id)
    for (champ in c("key", "url", "model"))
      expect_true(any(grepl(sprintf('id="%s"', champ), x, fixed = TRUE)),
                  label = paste(id, champ))
  }

  # Le prefixe est indispensable : l'atelier de codage nomme ses champs
  # `ai_url`, `ai_model`, `ai_key`, et ce sont ces noms que son serveur lit.
  a <- ids("chatgpt", "ai_")
  for (champ in c("ai_key", "ai_url", "ai_model"))
    expect_true(any(grepl(sprintf('id="%s"', champ), a, fixed = TRUE)), label = champ)

  # La valeur par defaut affichee est bien celle du fournisseur.
  h <- as.character(hstat_ai_reglages_ui(identity, "kimi"))
  expect_true(grepl(HSTAT_AI_FOURNISSEURS$kimi$url, h, fixed = TRUE))
  expect_true(grepl(HSTAT_AI_FOURNISSEURS$kimi$modele, h, fixed = TRUE))
  # Et la variable d'environnement est NOMMEE : sans elle, l'utilisateur ne
  # sait pas comment eviter de ressaisir sa cle a chaque session.
  expect_true(grepl(HSTAT_AI_FOURNISSEURS$kimi$cle_env, h, fixed = TRUE))
})

test_that("le moteur auto ne pretend pas rediger un texte", {
  # « auto » n'est pas un modele de langue : le lui demander doit rendre un
  # message qui dit quoi faire, pas partir en reseau ni tomber en erreur.
  r <- hstat_ai_call("bonjour", engine = "auto")
  expect_false(r$ok)
  expect_true(grepl("thématisation automatique", r$error, ignore.case = TRUE))
})

test_that("le modele Claude declare est bien claude-opus-5", {
  expect_equal(HSTAT_AI_MODEL, "claude-opus-5")
})

test_that("hstat_code_auto_codebook degage des themes sans modele ni reseau", {
  set.seed(3)
  # Chaque registre partage un terme pivot d'une reponse a l'autre : c'est
  # exactement la structure de cooccurrence sur laquelle s'appuie la methode.
  prix <- rep(c("Le prix est vraiment trop eleve.",
                "Un prix excessif, beaucoup trop cher.",
                "Le prix annonce reste cher et excessif."), 6)
  serv <- rep(c("L accueil du personnel est chaleureux.",
                "Un accueil competent et vraiment disponible.",
                "L accueil reste chaleureux et disponible."), 6)
  cb <- hstat_code_auto_codebook(c(prix, serv), n_codes = 2, min_char = 4)

  expect_false(is.null(cb))
  expect_equal(nrow(cb), 2L)
  expect_setequal(names(cb), c("label", "memo", "keywords"))
  expect_true(all(nzchar(cb$label)))
  expect_false(any(duplicated(tolower(cb$label))))
  expect_true(all(nzchar(cb$keywords)))

  # Les deux registres lexicaux doivent se retrouver dans des themes distincts
  kw <- lapply(seq_len(2), function(i) trimws(strsplit(cb$keywords[i], ";")[[1]]))
  th_prix <- which(vapply(kw, function(k) "prix" %in% k, logical(1)))
  th_serv <- which(vapply(kw, function(k) "personnel" %in% k, logical(1)))
  expect_length(th_prix, 1L)
  expect_length(th_serv, 1L)
  expect_false(identical(th_prix, th_serv))

  # Corpus trop court : refus explicite plutot qu'erreur
  expect_null(hstat_code_auto_codebook(c("a", "b")))
  expect_null(hstat_code_auto_codebook(character(0)))
})

test_that(".hstat_code_sentences decoupe sans rogner de lettre", {
  t <- "Le service est parfait. En revanche, le prix est trop eleve ! Je ne reviendrai pas."
  b <- .hstat_code_sentences(t)
  expect_equal(nrow(b), 3L)
  got <- vapply(seq_len(3), function(i) substr(t, b[i, 1] + 1, b[i, 2]), character(1))
  expect_equal(got, c("Le service est parfait",
                      "En revanche, le prix est trop eleve",
                      "Je ne reviendrai pas"))
  # Sans ponctuation finale, le texte entier forme une phrase
  expect_equal(substr("un seul bloc", 1, .hstat_code_sentences("un seul bloc")[1, 2]),
               "un seul bloc")
  expect_null(.hstat_code_sentences(""))
  expect_null(.hstat_code_sentences("..."))
})

test_that("le codage par dictionnaire pose des bornes exactes", {
  df <- data.frame(avis = c("Le prix est trop eleve. Mais l accueil est parfait.",
                            "Rien a signaler.",
                            "TARIF excessif et PRIX abusif !"),
                   stringsAsFactors = FALSE)
  d  <- hstat_code_docs(df, "avis")
  cb <- hstat_code_add(hstat_code_new_codebook(), "Prix", keywords = "prix; tarif")
  cb <- hstat_code_add(cb, "Accueil", keywords = "accueil")

  sg <- hstat_code_lexical_apply(d, cb, scope = "phrase")
  expect_true(nrow(sg) >= 3)
  expect_true(all(sg$source == "auto"))
  # Toute borne doit redonner exactement le texte stocke
  for (i in seq_len(nrow(sg))) {
    txt <- d$text[d$doc_id == sg$doc_id[i]]
    expect_equal(substr(txt, sg$start[i] + 1, sg$end[i]), sg$text[i])
  }
  # La phrase entiere est etiquetee, pas le seul mot-cle
  expect_true(any(grepl("Le prix est trop eleve", sg$text, fixed = TRUE)))
  # La reponse sans mot-cle n'est pas codee
  expect_false(d$doc_id[2] %in% sg$doc_id)
  # Casse ignoree : « TARIF » et « PRIX » sont dans la meme phrase, donc un
  # seul segment pour le code Prix (le doublon de bornes est ecarte)
  expect_equal(sum(sg$doc_id == d$doc_id[3] & sg$code_id == "prix"), 1L)

  # Portee « mot » : le segment se limite au mot-cle
  sm <- hstat_code_lexical_apply(d, cb, scope = "mot")
  expect_true(all(tolower(sm$text) %in% c("prix", "tarif", "accueil")))
  expect_true(nrow(sm) > nrow(sg))
})

test_that("le dictionnaire ignore les accents sans decaler les positions", {
  df <- data.frame(avis = "Le tarif est vraiment \u00e9lev\u00e9 et la qualit\u00e9 m\u00e9diocre.",
                   stringsAsFactors = FALSE)
  d  <- hstat_code_docs(df, "avis")
  # Mot-cle sans accent, texte avec accents
  cb <- hstat_code_add(hstat_code_new_codebook(), "Qualite", keywords = "qualite; eleve")
  sg <- hstat_code_lexical_apply(d, cb, scope = "mot")
  expect_equal(nrow(sg), 2L)
  # Les positions doivent pointer sur les formes ACCENTUEES du texte d'origine
  expect_setequal(sg$text, c("\u00e9lev\u00e9", "qualit\u00e9"))
  for (i in seq_len(nrow(sg)))
    expect_equal(substr(d$text, sg$start[i] + 1, sg$end[i]), sg$text[i])
})

test_that("mots-cles : lecture, ecriture et migration d'un ancien projet", {
  cb <- hstat_code_add(hstat_code_new_codebook(), "Prix",
                       keywords = "prix; tarif ; cher")
  expect_equal(hstat_code_keywords_of(cb, "prix"), c("prix", "tarif", "cher"))
  expect_equal(hstat_code_keywords_of(cb, "inconnu"), character(0))

  cb2 <- hstat_code_update(cb, "prix", keywords = "cout")
  expect_equal(hstat_code_keywords_of(cb2, "prix"), "cout")

  # Livre de codes enregistre par une version anterieure a la colonne keywords
  ancien <- data.frame(code_id = "prix", label = "Prix", color = "#e74c3c",
                       memo = "", created = "2026-01-01 00:00:00",
                       stringsAsFactors = FALSE)
  mig <- hstat_code_migrate_codebook(ancien)
  expect_true("keywords" %in% names(mig))
  expect_equal(mig$keywords, "")
  expect_equal(mig$label, "Prix")
  expect_equal(nrow(hstat_code_migrate_codebook(NULL)), 0L)
})

test_that("les codes sans mots-cles sont comptes, pas silencieusement ignores", {
  df <- data.frame(avis = "Le prix est eleve.", stringsAsFactors = FALSE)
  d  <- hstat_code_docs(df, "avis")
  cb <- hstat_code_add(hstat_code_new_codebook(), "Prix", keywords = "prix")
  cb <- hstat_code_add(cb, "Delai")            # sans dictionnaire
  sg <- hstat_code_lexical_apply(d, cb)
  expect_equal(attr(sg, "codes_sans_mots_cles"), 1L)
  expect_true(all(sg$code_id == "prix"))
})

test_that("hstat_ai_extract_json tolere le texte et les blocs markdown", {
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  j1 <- hstat_ai_extract_json('Voici le resultat :\n```json\n{"codes":[{"label":"Prix"}]}\n```\nVoila.')
  expect_equal(j1$codes[[1]]$label, "Prix")
  j2 <- hstat_ai_extract_json('{"codes":[{"label":"A"},{"label":"B"}]}')
  expect_equal(length(j2$codes), 2L)
  expect_null(hstat_ai_extract_json("aucun json ici"))
  expect_null(hstat_ai_extract_json(""))
})

test_that("hstat_ai_parse_codebook extrait libelles et memos", {
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  p <- hstat_ai_extract_json(
    '{"codes":[{"label":"Prix trop eleve","memo":"cout juge excessif"},
               {"label":"Accueil","memo":""},
               {"label":"","memo":"ignore"}]}')
  cb <- hstat_ai_parse_codebook(p)
  expect_equal(nrow(cb), 2L)
  expect_equal(cb$label, c("Prix trop eleve", "Accueil"))
  expect_equal(cb$memo[1], "cout juge excessif")
  expect_null(hstat_ai_parse_codebook(NULL))
})

test_that("hstat_code_locate_quote retrouve l'extrait dans le texte", {
  txt <- "Le service est correct mais le prix est vraiment trop eleve."
  p <- hstat_code_locate_quote(txt, "le prix est vraiment trop eleve")
  expect_false(is.null(p))
  expect_equal(substr(txt, p[["start"]] + 1, p[["end"]]), "le prix est vraiment trop eleve")

  # Casse differente
  p2 <- hstat_code_locate_quote(txt, "LE PRIX EST VRAIMENT TROP ELEVE")
  expect_false(is.null(p2))
  expect_equal(p2[["start"]], p[["start"]])

  # Espaces normalises par le modele
  p3 <- hstat_code_locate_quote(txt, "le   prix    est vraiment")
  expect_false(is.null(p3))

  # Extrait invente : rien n'est pose sur le texte
  expect_null(hstat_code_locate_quote(txt, "la livraison a ete tres rapide"))
  expect_null(hstat_code_locate_quote(txt, ""))
})

test_that("hstat_ai_parse_autocode n'accepte que les extraits reellement presents", {
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  df <- data.frame(rep = c("le prix est trop eleve", "service impeccable"),
                   stringsAsFactors = FALSE)
  d  <- hstat_code_docs(df, "rep")
  cb <- hstat_code_add(hstat_code_add(hstat_code_new_codebook(), "Prix"), "Service")

  p <- hstat_ai_extract_json(sprintf(
    '{"codages":[{"doc":"%s","code":"Prix","extrait":"le prix est trop eleve"},
                 {"doc":"%s","code":"Service","extrait":"un extrait totalement invente"},
                 {"doc":"%s","code":"Code inconnu","extrait":"service impeccable"}]}',
    d$doc_id[1], d$doc_id[2], d$doc_id[2]))
  sg <- hstat_ai_parse_autocode(p, d, cb)

  expect_equal(nrow(sg), 1L)
  expect_equal(sg$code_id, "prix")
  expect_equal(sg$source, "IA")
  expect_equal(sg$text, "le prix est trop eleve")
  expect_equal(attr(sg, "non_localises"), 2L)
})

test_that("HStat.R ne source plus aucun module : l'ordre n'est plus a tenir", {
  # C'ETAIT LA CONTRAINTE A LEVER. `mod_qualitative_ui()` appelle
  # `mod_coding_ui()`, et un test verifiait que `HStat.R` sourcait le second en
  # premier. La contrainte etait reelle, mais elle ne se voyait pas : rien
  # n'empechait un module ajoute plus tard de se glisser au mauvais rang, et
  # l'erreur (« could not find function mod_coding_ui ») serait tombee au
  # demarrage, loin de sa cause.
  #
  # Les modules etant dans `R/`, ils sont charges sans ordre. Le test garde
  # donc l'inverse de ce qu'il gardait : qu'AUCUNE ligne de source() de module
  # ne revienne dans `HStat.R`. En reintroduire une remettrait la contrainte
  # sans remettre le garde-fou.
  root <- .hstat_repo_root()
  skip_if(is.na(root), "hors depot")
  h <- .hstat_code_lignes(file.path(root, "inst", "app", "HStat.R"))
  expect_length(grep('source\\("mod_[^"]*\\.R"', h), 0L)
  # Les deux seuls fichiers qui AGISSENT au chargement restent sources.
  expect_length(grep('source\\("UX\\.R"', h), 1L)
  expect_length(grep('source\\("app_server\\.R"', h), 1L)
  # Et tous les modules sont bien du cote paquet.
  expect_length(list.files(file.path(root, "inst", "app"), pattern = "^mod_"), 0L)
  expect_gte(length(list.files(file.path(root, "R"), pattern = "^mod_.*[.]R$")), 15L)
})

test_that("le corps des requetes locales a la forme attendue par les serveurs", {
  # Ces champs sont le contrat avec Ollama et avec les serveurs compatibles
  # OpenAI : un renommage silencieux casserait l'assistant sans erreur visible.
  o <- .hstat_ai_body_openai("code ce corpus", system = "sys", model = "gpt-4o",
                             max_tokens = 2048L, json = TRUE)
  expect_equal(o$model, "gpt-4o")
  expect_false(o$stream)                    # sinon la reponse arrive par morceaux
  expect_equal(o$max_tokens, 2048L)
  expect_equal(o$temperature, 0.2)          # thematisation stable, pas creative
  expect_equal(o$response_format$type, "json_object")
  expect_equal(vapply(o$messages, function(m) m$role, character(1)),
               c("system", "user"))
  expect_equal(o$messages[[2]]$content, "code ce corpus")
  # Sans consigne systeme, un seul message
  expect_equal(vapply(.hstat_ai_body_openai("x", system = NULL, model = "m")$messages,
                      function(m) m$role, character(1)), "user")
  # Le rejeu apres un refus du serveur passe par la meme fonction sans le champ
  expect_null(.hstat_ai_body_openai("x", model = "m", json = FALSE)$response_format)

  # Gemini ne parle ni le protocole d'OpenAI ni celui d'Anthropic : la consigne
  # systeme y est un champ a part (`systemInstruction`), et le texte est range
  # en `parts`. C'est la piece qui casse en silence si un champ est renomme.
  g <- .hstat_ai_body_gemini("code ce corpus", system = "sys", json = TRUE)
  expect_equal(g$contents[[1]]$parts[[1]]$text, "code ce corpus")
  expect_equal(g$contents[[1]]$role, "user")
  expect_equal(g$systemInstruction$parts[[1]]$text, "sys")
  expect_equal(g$generationConfig$temperature, 0.2)
  expect_equal(g$generationConfig$responseMimeType, "application/json")
  expect_null(.hstat_ai_body_gemini("x", system = NULL)$systemInstruction)
  expect_null(.hstat_ai_body_gemini("x", json = FALSE)$generationConfig$responseMimeType)

  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  # `messages` doit rester un TABLEAU JSON, meme avec un seul message
  j <- jsonlite::toJSON(.hstat_ai_body_openai("x", model = "m"), auto_unbox = TRUE)
  expect_true(grepl('"messages":[{', j, fixed = TRUE))
  expect_true(grepl('"stream":false', j, fixed = TRUE))
  # Idem pour `contents` et `parts` chez Gemini
  jg <- jsonlite::toJSON(.hstat_ai_body_gemini("x"), auto_unbox = TRUE)
  expect_true(grepl('"contents":[{', jg, fixed = TRUE))
  expect_true(grepl('"parts":[{', jg, fixed = TRUE))
})

# =============================================================================
#  AIDE A LA DECISION -- mod_ai.R
# =============================================================================








test_that("l'invite demande la reponse dans la langue de la session", {
  # LA REPONSE DU MODELE EST DU TEXTE AFFICHE QU'AUCUN DICTIONNAIRE NE PEUT
  # TRADUIRE : elle n'existe pas quand la page se construit, et le traducteur
  # du navigateur ne remplace que des correspondances connues. Trois invites
  # imposaient « en francais » en dur -- un utilisateur anglophone recevait une
  # interpretation ENTIERE en francais, sans un mot d'avertissement. C'est la
  # plus grosse trace de francais qui restait, et la seule qu'une mesure de
  # couverture du dictionnaire ne peut pas voir.
  # L'invite d'interpretation a disparu avec son onglet ; celle du LIVRE DE
  # CODES reste, et la regle vaut d'autant plus pour elle : ses libelles
  # deviennent des DONNEES du projet de codage, ils doivent suivre la langue de
  # qui code.
  cb_fr <- hstat_ai_codebook_prompt(c("trop cher", "service lent"), lang = "fr")
  cb_en <- hstat_ai_codebook_prompt(c("trop cher", "service lent"), lang = "en")
  expect_true(grepl("en français", cb_fr, fixed = TRUE))
  expect_true(grepl("in English", cb_en, fixed = TRUE))
  expect_false(grepl("en français", cb_en, fixed = TRUE))

  # Hors session Shiny, la valeur par defaut reste le francais : la fonction
  # est pure et testable, et un appel existant ne change pas de comportement.
  expect_equal(hstat_ai_consigne_langue("fr"), "en français")
  expect_equal(hstat_ai_consigne_langue(), "en français")
})


test_that("le moteur d'inference est charge avec le paquet, sans rang a tenir", {
  # Ce test verifiait que `HStat.R` sourcait `mod_ai.R` avant les quatre modules
  # qui appellent `hstat_ai_*`. La dependance existe toujours -- elle est juste
  # devenue sans objet : dans un paquet, les definitions sont toutes en place
  # avant qu'aucune ne soit appelee.
  #
  # Ce qui reste a garder, c'est que le moteur soit bien DU COTE PAQUET. L'y
  # oublier ferait retomber la question de l'ordre par la fenetre.
  root <- .hstat_repo_root()
  skip_if(is.na(root), "hors depot")
  expect_true(file.exists(file.path(root, "R", "mod_ai.R")))
  expect_false(file.exists(file.path(root, "inst", "app", "mod_ai.R")))
  # Ce qui vit encore dans ce fichier, c'est le moteur d'inference lui-meme,
  # celui dont l'atelier de codage se sert.
  expect_true(is.function(hstat_ai_call))
  expect_true(is.function(hstat_ai_status))
})





test_that("les intervalles de prevision perdent leur classe `ts` avant ggplot", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # LE PIEGE, reproduit sans `forecast` : `as.matrix()` sur une serie MULTIPLE
  # ne retire pas la classe `ts` des colonnes extraites.
  m <- stats::ts(matrix(1:20, ncol = 2), frequency = 4)
  expect_true(inherits(as.matrix(m)[, 1], "ts"))
  expect_false(inherits(as.numeric(as.matrix(m)[, 1]), "ts"))

  # Consequence dans l'application : ggplot ne sait pas choisir d'echelle pour
  # ce type et le signalait a CHAQUE trace de prevision (« Don't know how to
  # automatically pick scale for object of type <ts> »). Mesure faite dans le
  # journal du serveur : 1 avertissement avant correction, 0 apres.
  l <- .hstat_code_lignes(.hstat_module_path("mod_timeseries.R"))
  poses <- grep("fdf\\$(lo|hi)[0-9]+ *<-", l, value = TRUE)
  expect_gt(length(poses), 0L)
  expect_true(all(grepl("as.numeric", poses)))
})

test_that("aucune installation de paquet ne part du corps du serveur", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # `install.packages()` etait appele DANS `server` : sur un poste hors ligne,
  # chaque ouverture de l'application attendait l'expiration de la requete CRAN
  # (mesure : sept tentatives pour sept sessions d'essai) ; sur un serveur
  # partage, la bibliotheque est le plus souvent en lecture seule, et deux
  # sessions simultanees pouvaient y ecrire ensemble.
  #
  # L'installation appartient au demarrage : `install_and_load()` et
  # `hstat_load_model_packages()` (Utils.R), appeles une fois au source.
  # Le socle est ecarte : `install_and_load()` y vit, et c'est son domicile
  # legitime. Ce que le test interdit, c'est une installation lancee par le CODE
  # DE L'APPLICATION -- interface, serveur, modules.
  fichiers <- setdiff(.hstat_sources_app(), file.path(root, "R", "utils.R"))
  for (chemin in fichiers) {
    f <- basename(chemin)
    # Le motif vise un APPEL, pas une mention : plusieurs messages indiquent a
    # l'utilisateur la commande d'installation, et les compter ferait echouer le
    # test sur une phrase d'aide. On passe donc par l'analyseur de R, qui
    # distingue un appel de fonction d'une chaine de caracteres.
    pd <- utils::getParseData(parse(chemin, keep.source = TRUE))
    appels <- pd$text[pd$token == "SYMBOL_FUNCTION_CALL"]
    expect_false("install.packages" %in% appels, info = f)
  }
  # Et la liste de demarrage porte bien les paquets qui y ont ete deplaces.
  # Elle vit dans le SOCLE (`R/utils.R`) : c'est une definition. Ce qui reste au
  # pont, c'est l'APPEL qui l'installe.
  u <- paste(.hstat_code_lignes(file.path(root, "R", "utils.R")), collapse = "\n")
  i <- regexpr("hstat_model_packages <- c\\(", u)
  bloc <- substr(u, i, i + 900L)
  for (p in c("lavaan", "pls", "klaR", "poLCA", "clustMixType", "nnet"))
    expect_true(grepl(paste0('"', p, '"'), bloc), info = p)
})

test_that("une date est un facteur de periode valide, et il est chronologique", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # UNE PERIODE REPETEE EST PRESQUE TOUJOURS UNE DATE. Le selecteur ne retenait
  # que facteurs, chaines et numeriques a peu de modalites : une colonne `Date`
  # n'est aucun des trois et n'apparaissait donc jamais dans la liste -- alors
  # que l'exemple affiche sous le champ annonce « date ».
  l <- .hstat_code_lignes(.hstat_module_path("mod_tests.R"))
  i <- grep("fac_cols <- names\\(df\\)\\[sapply", l)
  expect_gt(length(i), 0L)
  fenetre <- paste(l[max(1L, i[1] - 6L):(i[1] + 3L)], collapse = " ")
  expect_true(grepl("Date", fenetre, fixed = TRUE))
  expect_true(grepl("POSIXct", fenetre, fixed = TRUE))

  # LE PIEGE : l'ordre des niveaux. `factor()` sur une `Date` classe sur la
  # valeur sous-jacente, donc chronologiquement -- ce qui est indispensable a
  # des mesures repetees et aux contrastes post-hoc.
  d <- as.Date(c("2026-04-05", "2026-03-19", "2026-03-05"))
  expect_equal(levels(factor(d)), c("2026-03-05", "2026-03-19", "2026-04-05"))

  # La forme fautive, pour memoire : passer d'abord par une chaine au format
  # francais fait trier par ordre ALPHABETIQUE. Les dates doivent traverser un
  # changement de mois pour que l'ecart se voie -- a l'interieur d'un meme mois,
  # les deux ordres coincident, et un exemple mal choisi ferait croire que le
  # piege n'existe pas.
  fr <- format(d, "%d/%m/%Y")
  expect_equal(levels(factor(fr)),
               c("05/03/2026", "05/04/2026", "19/03/2026"))   # le 5 avril AVANT le 19 mars
  expect_false(identical(levels(factor(fr)),
                         format(sort(unique(d)), "%d/%m/%Y")))
})

test_that("une randomisation ne tire jamais dans 1:x par accident", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # `sample(x, n)` PIOCHE DANS 1:x QUAND x EST UN SEUL NOMBRE. Un plan a un
  # seul traitement code numeriquement verrait apparaitre un traitement qui
  # n'existe pas -- silencieusement, dans un plan d'experience.
  expect_true(all(sample(c(7), 1) %in% 1:7))          # le piege, tel quel
  expect_equal(c(7)[sample.int(1)], 7)                # la forme employee

  l <- .hstat_code_lignes(.hstat_module_path("mod_design.R"))
  fautifs <- grep("sample\\([a-zA-Z_][A-Za-z0-9_.]*, *[a-zA-Z_]", l, value = TRUE)
  expect_equal(fautifs, character(0))
})

test_that("aucun identifiant n'est declare deux fois dans la page", {
  skip_if_not_installed("shinydashboard")
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # DEUX ELEMENTS, UN SEUL IDENTIFIANT : les deux boutons « Diagnostiquer mes
  # données » portaient `runManovaDiagnostic`. Cliquer marchait -- la liaison de
  # Shiny lit l'id de l'element clique -- mais `updateActionButton()` ou
  # `shinyjs::disable()` n'en auraient atteint qu'un, et le HTML est invalide.
  #
  # La mesure porte sur la PAGE RENDUE, pas sur le source : un identifiant
  # ecrit deux fois dans deux branches d'interface qui ne s'affichent jamais
  # ensemble n'est pas un doublon, et le compter le ferait echouer a tort.
  app <- file.path(root, "inst", "app")
  e <- new.env(parent = globalenv())
  ok <- tryCatch({
    suppressMessages(suppressWarnings({
      old <- setwd(app); on.exit(setwd(old), add = TRUE)
      # Le dossier est BALAYE, les modules ne sont plus nommes un a un : une
      # liste de noms se serait videe a mesure des migrations, et le test
      # aurait fini par construire une interface amputee -- donc sans doublon,
      # donc au vert, en ne regardant plus rien.
      socle <- file.path(root, "R")
      for (f in c(file.path(socle, "utils.R"),
                  list.files(socle, pattern = "^mod_.*[.]R$", full.names = TRUE)))
        try(sys.source(f, e), silent = TRUE)
      hstat_installer_replis_ui(e)
      try(sys.source("UX.R", e), silent = TRUE)
    }))
    exists("ui", envir = e)
  }, error = function(err) FALSE)
  skip_if_not(isTRUE(ok), "interface non constructible dans cet environnement")

  html <- paste(as.character(htmltools::renderTags(get("ui", e))$html), collapse = "\n")
  # L'espace avant `id=` est indispensable : sans lui, `data-grid="true"` est lu
  # comme un identifiant valant « true », et la mesure annonce 108 doublons.
  ids <- gsub("^ id=\"|\"$", "", regmatches(html, gregexpr(" id=\"[^\"]+\"", html))[[1]])

  # LES ANCRES D'ONGLET DE SHINY SONT ECARTEES, et ce n'est pas un renoncement.
  # `tabsetPanel()` numerote ses onglets `tab-<entier au hasard>-<n>`, l'entier
  # etant tire entre 1000 et 10000 A CHAQUE CONSTRUCTION. L'application en rend
  # trente-quatre : la probabilite qu'au moins deux partagent le meme tirage est
  # de 6,1 % PAR RENDU. Ce test echouait donc environ une fois sur seize, sur un
  # code parfaitement correct -- constate en integration continue, jamais en
  # local.
  #
  # Verifie : passer un `id` explicite a `tabsetPanel()` NE CHANGE PAS ces
  # ancres (l'id nomme la liaison d'entree, pas les cibles). Le tirage est
  # interne a Shiny, l'application ne peut pas s'en prevenir.
  #
  # Ce que le test cherche -- les identifiants que L'APPLICATION declare deux
  # fois, comme les 108 boutons homonymes qu'il a trouves -- reste entierement
  # couvert : aucun d'eux ne porte cette forme.
  ids <- ids[!grepl("^tab-[0-9]+-[0-9]+$", ids)]
  compte <- table(ids)
  expect_gt(length(ids), 500L)
  expect_equal(names(compte)[compte > 1], character(0))
})


# =============================================================================
#  ROBUSTESSE AUX STATISTIQUES NON CALCULABLES
# =============================================================================

test_that("hstat_p_verdict distingue trois etats, dont l'indeterminable", {
  expect_equal(hstat_p_verdict(0.001), "significatif")
  expect_equal(hstat_p_verdict(0.049), "significatif")
  expect_equal(hstat_p_verdict(0.05), "non significatif")   # borne stricte
  expect_equal(hstat_p_verdict(0.9), "non significatif")

  # Le troisieme etat est la raison d'etre de la fonction : un test qui n'a pas
  # pu conclure ne doit pas etre lu comme « non significatif ».
  expect_equal(hstat_p_verdict(NA), "indeterminable")
  expect_equal(hstat_p_verdict(NaN), "indeterminable")
  expect_equal(hstat_p_verdict(NULL), "indeterminable")
  expect_equal(hstat_p_verdict(Inf), "indeterminable")
  expect_equal(hstat_p_verdict(character(0)), "indeterminable")
  expect_equal(hstat_p_verdict("abc"), "indeterminable")

  # Seuil ajustable
  expect_equal(hstat_p_verdict(0.03, alpha = 0.01), "non significatif")
  expect_equal(hstat_p_verdict(0.003, alpha = 0.01), "significatif")
})

test_that("le verdict rend l'ancien piege impossible", {
  # C'est exactement le cas qui faisait tomber le diagnostic de Ljung-Box :
  # des residus de variance nulle donnent p = NaN, et `if (p > 0.05)` levait
  # « missing value where TRUE/FALSE needed ».
  res <- rep(0, 40)
  lb <- suppressWarnings(stats::Box.test(res, lag = 8, type = "Ljung-Box"))
  expect_true(is.nan(lb$p.value))
  expect_error(if (lb$p.value > 0.05) TRUE else FALSE)          # l'ancien code
  expect_equal(hstat_p_verdict(lb$p.value), "indeterminable")   # le nouveau
  # Et le verdict se consomme sans jamais brancher sur un NA
  expect_silent(switch(hstat_p_verdict(lb$p.value),
                       "significatif" = "a", "non significatif" = "b", "c"))
})

test_that("aucune condition ne branche directement sur une p-value non gardee", {
  # `.hstat_sources_app()` et NON `list.files(inst/app)` : les vingt et un
  # modules ont demenage dans `R/`, et trois balayages -- celui-ci, celui des
  # coordonnees FactoMineR et celui des messages R bruts -- etaient restes
  # pointes sur `inst/app/`. Ils lisaient CINQ fichiers au lieu de vingt-trois,
  # donc plus aucun module. Verifie par mutation : les trois defauts glisses
  # ensemble dans `R/mod_tests.R` passaient tous les trois.
  skip_if(is.na(.hstat_repo_root()))
  motif <- "if\\s*\\([^)]*\\$p\\.value\\s*[<>]"
  trouve <- character(0)
  for (f in .hstat_sources_app()) {
    l <- readLines(f, warn = FALSE)
    l <- l[!grepl("^\\s*#", l)]
    hit <- grep(motif, l, value = TRUE)
    # `isTRUE(...)` et `is.finite(...)` sont des gardes acceptables
    hit <- hit[!grepl("isTRUE|isFALSE|is\\.finite|is\\.na", hit)]
    if (length(hit)) trouve <- c(trouve, paste0(basename(f), " : ", hit))
  }
  expect_equal(trouve, character(0),
               info = paste("conditions non gardees :", paste(trouve, collapse = " | ")))
})

test_that("un temoin nul rend NA, jamais un infini", {
  # CLAUDE.md pose la regle : « diviser par zero produirait des Inf silencieux,
  # qui ressortiraient en graphique comme des barres demesurees. On rend NA et
  # on le dit. » Le MESSAGE etait verifie, les VALEURS ne l'etaient pas --
  # retirer le garde-fou ne faisait echouer aucune assertion de la suite
  # entiere, et les efficacites sortaient a -Inf avec l'alerte toujours
  # affichee a cote. Trouve par mutation.
  d <- data.frame(Mod = c("T", "A", "B"), Y = c(0, 5, 12))
  r <- hstat_efficacite(d, "Mod", "Y", "T")

  # 1. AUCUN INFINI, nulle part. C'est l'assertion qui manquait.
  expect_false(any(is.infinite(r$Efficacite)))
  # 2. Les modalites comparees au temoin nul valent NA -- pas zero, pas -Inf :
  #    l'efficacite n'est pas definissable, et NA le dit.
  expect_true(all(is.na(r$Efficacite[r$Modalite != "T"])))
  # 3. Le temoin, lui, garde son zero par definition.
  expect_equal(r$Efficacite[r$Modalite == "T"], 0)
  # 4. Et le message accompagne les valeurs, il ne les remplace pas.
  expect_true(grepl("nul", attr(r, "message"), fixed = TRUE))

  # Le meme essai avec un temoin NON nul reste normal : le garde-fou ne doit
  # pas se declencher a tort.
  d2 <- data.frame(Mod = c("T", "A", "B"), Y = c(20, 5, 12))
  r2 <- hstat_efficacite(d2, "Mod", "Y", "T")
  expect_true(all(is.finite(r2$Efficacite)))
  expect_equal(r2$Efficacite[r2$Modalite == "A"], 75)
})

test_that("hstat_coord_mat protege des coordonnees reduites a un vecteur", {
  # Cas nominal : une matrice reste une matrice
  m <- matrix(1:6, ncol = 3, dimnames = list(c("a","b"), paste("Dim", 1:3)))
  expect_identical(dim(hstat_coord_mat(m)), c(2L, 3L))
  expect_equal(colnames(hstat_coord_mat(m)), colnames(m))

  # Cas du bug : un seul axe -> FactoMineR rend un vecteur nu
  v <- c(a = 0.3, b = -0.2, c = 0.1)
  out <- hstat_coord_mat(v)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(3L, 1L))
  expect_equal(rownames(out), c("a","b","c"))
  expect_equal(unname(out[, 1]), unname(v))
  # LA COLONNE EST NOMMEE, et cela se verifie. Sans la garde, `as.matrix()` rend
  # bien une matrice 3x1 aux bonnes lignes -- la mutation SURVIVAIT -- mais sa
  # colonne n'a PAS de nom, la ou la garde la baptise « Dim 1 ». C'est ce nom
  # qui etiquette l'axe du graphique ; la branche « matrice » de ce test le
  # verifiait deja, la branche « vecteur » l'avait oublie.
  expect_equal(colnames(out), "Dim 1")
  # ... et l'indexation qui echouait passe desormais
  expect_error(v[, 1:min(2, ncol(v)), drop = FALSE])                    # avant
  expect_silent(out[, 1:min(2, ncol(out)), drop = FALSE])               # apres

  expect_null(hstat_coord_mat(NULL))
  # Un data.frame de coordonnees est accepte aussi
  expect_true(is.matrix(hstat_coord_mat(data.frame(x = 1:3, y = 4:6))))
})

test_that("une AFC croisant une variable binaire ne casse plus", {
  skip_if_not(requireNamespace("FactoMineR", quietly = TRUE))
  set.seed(4)
  # Table 3x2 : une seule dimension. Cas tres courant (sexe, oui/non...).
  d <- data.frame(groupe = sample(c("Temoin","A","B"), 90, TRUE),
                  sexe = sample(c("F","H"), 90, TRUE), stringsAsFactors = FALSE)
  tab <- table(d$groupe, d$sexe)
  ca <- FactoMineR::CA(tab, graph = FALSE)

  # FactoMineR reduit bien les coordonnees des lignes a un vecteur ici :
  # c'est la cause exacte de « incorrect number of dimensions ».
  expect_null(dim(ca$row$coord))

  row_co <- hstat_coord_mat(ca$row$coord)
  col_co <- hstat_coord_mat(ca$col$coord)
  rc <- as.data.frame(row_co[, 1:min(2, ncol(row_co)), drop = FALSE])
  if (ncol(rc) < 2) rc$D2 <- 0
  names(rc)[1:2] <- c("Dim1", "Dim2")
  expect_equal(nrow(rc), 3L)
  expect_equal(names(rc)[1:2], c("Dim1", "Dim2"))
  expect_equal(rownames(row_co), rownames(tab))

  cc <- as.data.frame(col_co[, 1:min(2, ncol(col_co)), drop = FALSE])
  if (ncol(cc) < 2) cc$D2 <- 0
  expect_equal(nrow(cc), 2L)
})

test_that("aucune coordonnee FactoMineR n'est indexee sans passer par le garde-fou", {
  skip_if(is.na(.hstat_repo_root()))
  fautes <- character(0)
  for (f in .hstat_sources_app()) {
    l <- readLines(f, warn = FALSE)
    l <- l[!grepl("^\\s*#", l)]
    # `<objet>$<champ>$coord[` sans hstat_coord_mat() sur la meme ligne
    hit <- grep("\\$coord\\[", l, value = TRUE)
    hit <- hit[!grepl("hstat_coord_mat", hit)]
    # les acces a des axes explicitement choisis par l'utilisateur (axis_x/axis_y)
    # portent deja leur propre validation en amont
    hit <- hit[!grepl("axis_x|axis_y", hit)]
    if (length(hit)) fautes <- c(fautes, paste0(basename(f), " : ", trimws(hit)))
  }
  expect_equal(fautes, character(0),
               info = paste("indexations non protegees :", paste(fautes, collapse = " | ")))
})

# =============================================================================
#  DIAGNOSTIC DE QUALITE DES DONNEES
# =============================================================================






# =============================================================================
#  JOURNAL DE REPRODUCTIBILITE
# =============================================================================


















# ===========================================================================
# TRADUCTION DES ERREURS R (hstat_err_fr)
# ===========================================================================

test_that("les erreurs R courantes deviennent des consignes en francais", {
  cas <- list(
    # message R                                        # mot attendu dans la traduction
    list("data are essentially constant",              "ne varie pas"),
    list("not enough 'y' observations",                "Effectif insuffisant"),
    list("grouping factor must have exactly 2 levels", "deux groupes"),
    list("incorrect number of dimensions",             "un seul axe"),
    list("system is computationally singular: reciprocal condition number",
                                                       "redondantes"),
    list("missing value where TRUE/FALSE needed",      "dégénérées"),
    list("0 (non-NA) cases",                           "Aucune observation"),
    list("NA/NaN/Inf in foreign function call (arg 1)", "manquantes ou infinies"),
    list("undefined columns selected",                 "absente du jeu de données"),
    list("there is no package called 'poLCA'",         "pas installé"),
    list("contrasts can be applied only to factors with 2 or more levels",
                                                       "une seule modalité"),
    list("sample size must be between 3 and 5000",     "Shapiro-Wilk"),
    list("figure margins too large",                   "trop petite"))
  for (c0 in cas) {
    tr <- hstat_err_fr(simpleError(c0[[1]]))
    expect_true(grepl(c0[[2]], tr, fixed = TRUE),
                info = paste0(c0[[1]], " -> ", tr))
    # Le message d'origine survit : c'est ce qu'un utilisateur copiera pour
    # demander de l'aide, et sans lui une traduction fautive est indebuggable.
    expect_true(grepl(c0[[1]], tr, fixed = TRUE), info = c0[[1]])
    expect_true(grepl("message R :", tr, fixed = TRUE))
  }
})

test_that("chaque traduction dit quoi faire, pas seulement ce qui s'est passe", {
  # Une traduction qui se contente de nommer la panne ne sert a rien. On exige
  # au moins un verbe d'action dans chacune.
  gestes <- paste("Choisissez|V\u00e9rifiez|Retirez|Convertissez|Installez|Traitez",
                  "|Simplifiez|Croisez|Augmentez|Agrandissez|R\u00e9duisez|Utilisez",
                  "|res\u00e9lectionnez|Signalez|Installez", sep = "")
  for (r in HSTAT_ERR_FR)
    expect_true(grepl(gestes, r[[2]], perl = TRUE, ignore.case = TRUE),
                info = substr(r[[2]], 1, 70))
  # Et chaque motif doit etre une expression reguliere valide
  for (r in HSTAT_ERR_FR)
    expect_silent(grepl(r[[1]], "test", perl = TRUE))
})

test_that("une erreur inconnue est annoncee comme non traduite, pas maquillee", {
  tr <- hstat_err_fr(simpleError("une panne totalement inedite"))
  expect_true(grepl("non traduit", tr, fixed = TRUE))
  expect_true(grepl("une panne totalement inedite", tr, fixed = TRUE))
  # Le contexte prefixe le message quand l'appelant le connait
  expect_true(grepl("^Test t : ", hstat_err_fr(simpleError("boum"), "Test t")))
  # Une chaine nue est acceptee au meme titre qu'une condition
  expect_equal(hstat_err_fr("data are essentially constant"),
               hstat_err_fr(simpleError("data are essentially constant")))
  # Une erreur sans message ne produit pas une phrase tronquee
  expect_true(grepl("erreur sans message", hstat_err_fr(simpleError("")),
                    fixed = TRUE))
})

test_that("aucun message R brut n'est affiche a l'utilisateur", {
  skip_if(is.na(.hstat_repo_root()))
  fautes <- character(0)
  for (f in .hstat_sources_app()) {
    if (basename(f) %in% c("HStat.R")) next   # secours de demarrage, hors Shiny
    l <- readLines(f, warn = FALSE, encoding = "UTF-8")
    l <- l[!grepl("^\\s*#", l)]
    # conditionMessage() dans une notification ou une validation : le message
    # anglais de R arriverait tel quel dans une interface francaise.
    hit <- grep("(showNotification|validate\\(need)\\(.*(conditionMessage|e\\$message)",
                l, value = TRUE)
    hit <- hit[!grepl("hstat_err_fr", hit)]
    # Les colonnes « Interpretation » des tableaux de resultats sont lues comme
    # une phrase : un message anglais y est encore plus depayse qu'ailleurs.
    hit <- c(hit, grep("Interpretation = paste\\(\"Erreur", l, value = TRUE))
    if (length(hit)) fautes <- c(fautes, paste0(basename(f), " : ", trimws(hit)))
  }
  expect_equal(fautes, character(0),
    info = paste("Passer par hstat_err_fr() : l'interface est en francais.\n",
                 paste(fautes, collapse = "\n")))
})


# ===========================================================================
# PERSISTANCE DE LA SESSION
# ---------------------------------------------------------------------------
# Un verrouillage d'ecran ne doit pas fermer l'application. Ces tests gardent
# les trois pieces du mecanisme : l'autorisation cote serveur, le script cote
# navigateur, et le fait que le voile gris de Shiny soit bien neutralise.
# ===========================================================================

test_that("le serveur autorise la reprise de session", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("allowReconnect", src, fixed = TRUE))
  # « force » et non TRUE : sans lui, la reprise n'est active que derriere un
  # serveur qui la gere, alors que HStat tourne le plus souvent en local.
  expect_true(grepl('allowReconnect\\("force"\\)', src))
  # Le signal de maintien envoye par le navigateur est bien recu
  expect_true(grepl("input\\$hstat_keepalive", src))
})

test_that("le script de persistance est present et branche", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  js <- file.path(root, "inst", "app", "www", "hstat-session.js")
  expect_true(file.exists(js))
  src <- paste(readLines(js, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Les quatre reprises : deconnexion, retour de visibilite (deverrouillage),
  # retour du focus, retour du reseau.
  for (ev in c("shiny:disconnected", "shiny:connected", "visibilitychange",
               "online"))
    expect_true(grepl(ev, src, fixed = TRUE), info = ev)
  expect_true(grepl("reconnect()", src, fixed = TRUE))
  expect_true(grepl("hstat_keepalive", src, fixed = TRUE))
  expect_true(grepl("beforeunload", src, fixed = TRUE))
  # Le maintien ne doit pas declencher d'analyse : signal, pas entree.
  expect_true(grepl('priority: "event"', src, fixed = TRUE))

  ux <- paste(readLines(file.path(root, "inst", "app", "UX.R"),
                        warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("hstat-session.js", ux, fixed = TRUE))
  # Le voile gris de Shiny est masque : sinon il recouvrirait le bandeau
  # francais et l'utilisateur croirait l'application morte.
  expect_true(grepl("shiny-disconnected-overlay", ux, fixed = TRUE))
})

test_that("le bandeau de reprise parle francais et rassure", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- paste(readLines(file.path(root, "inst", "app", "www", "hstat-session.js"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Le message doit dire que rien n'est perdu : c'est la seule chose que
  # l'utilisateur veut savoir a cet instant.
  expect_true(grepl("Connexion interrompue", src, fixed = TRUE))
  expect_true(grepl("n'est pas perdu", src, fixed = TRUE))
  expect_true(grepl("Connexion retablie", src, fixed = TRUE))
  # Et lui laisser la main plutot que de le faire attendre
  expect_true(grepl("Reprendre maintenant", src, fixed = TRUE))
  # Aucun libelle anglais dans ce qui s'affiche. Le voile de Shiny est cite
  # dans l'en-tete du fichier pour expliquer ce qu'on remplace : on ne cherche
  # donc l'anglais que dans les appels d'affichage.
  affichages <- regmatches(src, gregexpr('afficher\\([^;]+', src))[[1]]
  expect_true(length(affichages) >= 2L)
  for (a in affichages)
    expect_false(grepl("Disconnected|Please refresh|reload the page", a),
                 info = substr(a, 1, 60))
})


# ===========================================================================
# CLASSIFICATIONS SUR PAQUETS OPTIONNELS (k-modes, LCA, k-prototypes)
# ---------------------------------------------------------------------------
# klaR, poLCA et clustMixType ne sont pas installes par defaut. Les analyses
# elles-memes ne sont donc pas executables ici — mais leur STATISTIQUE DE
# QUALITE l'est, parce qu'elle a ete sortie du paquet. C'est le seul moyen de
# garder ces trois analyses sous controle sans pouvoir les lancer.
# ===========================================================================

test_that("le pseudo-R2 k-modes vaut ce qu'il doit valoir", {
  # 6 individus, 2 variables binaires, partition parfaite en 2 groupes.
  d <- data.frame(a = c("x","x","x","y","y","y"),
                  b = c("p","p","p","q","q","q"), stringsAsFactors = FALSE)
  # Dissimilarite totale : pour chaque variable, n - effectif du mode = 6 - 3.
  q <- hstat_kmodes_pseudo_r2(withindiff = c(0, 0), data = d)
  expect_equal(q$dissimilarite_totale, 6)
  expect_equal(q$dissimilarite_intra, 0)
  expect_equal(q$pseudo_r2, 1)
  expect_equal(q$verdict, "ok")

  # Partition qui n'explique rien : dissimilarite intra = dissimilarite totale
  q0 <- hstat_kmodes_pseudo_r2(withindiff = c(3, 3), data = d)
  expect_equal(q0$pseudo_r2, 0)
  expect_equal(q0$verdict, "err")

  # Cas degenere : variables constantes -> aucune dissimilarite a expliquer.
  # Le pseudo-R2 n'existe pas ; il ne doit pas valoir 0 ni faire tomber la
  # sortie, il doit se declarer indeterminable.
  cst <- data.frame(a = rep("x", 6), b = rep("p", 6), stringsAsFactors = FALSE)
  qc <- hstat_kmodes_pseudo_r2(0, cst)
  expect_equal(qc$dissimilarite_totale, 0)
  expect_true(is.na(qc$pseudo_r2))
  expect_equal(qc$verdict, "indeterminable")
})

test_that("l'entropie relative distingue une classification nette d'une confuse", {
  # Affectations sans ambiguite -> entropie relative = 1
  nette <- rbind(c(1, 0), c(1, 0), c(0, 1), c(0, 1))
  e1 <- hstat_lca_entropie(nette)
  expect_equal(e1$entropie_relative, 1, tolerance = 1e-6)
  expect_equal(e1$verdict, "ok")

  # Affectations indiscernables (50/50) -> entropie relative = 0
  floue <- matrix(0.5, nrow = 4, ncol = 2)
  e0 <- hstat_lca_entropie(floue)
  expect_equal(e0$entropie_relative, 0, tolerance = 1e-6)
  expect_equal(e0$verdict, "err")

  # Une seule classe : l'entropie n'est pas definie (log(1) = 0 au denominateur)
  e_una <- hstat_lca_entropie(matrix(1, nrow = 4, ncol = 1))
  expect_equal(e_una$verdict, "indeterminable")
  expect_equal(hstat_lca_entropie(matrix(numeric(0), 0, 2))$verdict,
               "indeterminable")
})

test_that("l'equilibre d'une partition signale les classes minoritaires", {
  expect_equal(hstat_part_equilibre(c(50, 50))$verdict, "ok")
  expect_equal(hstat_part_equilibre(c(50, 50))$part_min, 0.5)
  # 4 % : sous le seuil de 5 %, la classe n'est pas interpretable
  expect_equal(hstat_part_equilibre(c(96, 4))$verdict, "warn")
  expect_equal(hstat_part_equilibre(c(95, 5))$verdict, "ok")
  # Un cluster vide
  expect_equal(hstat_part_equilibre(c(100, 0))$verdict, "warn")
  # Aucun effectif : indeterminable, jamais une division par zero
  expect_equal(hstat_part_equilibre(integer(0))$verdict, "indeterminable")
  expect_equal(hstat_part_equilibre(c(0, 0))$verdict, "indeterminable")
  # Accepte une table() comme les modules la fournissent (2 et 1 sur 3 : la
  # plus petite pese 33 %, largement au-dessus du seuil)
  eq <- hstat_part_equilibre(table(c("a","a","b")))
  expect_equal(eq$verdict, "ok")
  expect_equal(eq$part_min, 1/3)
})

test_that("hstat_seuil_verdict ne branche jamais sur une valeur non calculable", {
  expect_equal(hstat_seuil_verdict(0.9, 0.8, 0.6), "ok")
  expect_equal(hstat_seuil_verdict(0.7, 0.8, 0.6), "warn")
  expect_equal(hstat_seuil_verdict(0.1, 0.8, 0.6), "err")
  for (x in list(NA, NA_real_, NaN, Inf, -Inf, NULL, character(0), c(1, 2)))
    expect_equal(hstat_seuil_verdict(x, 0.8, 0.6), "indeterminable")
})

test_that("un paquet absent donne une consigne, pas une impasse", {
  for (pkg in c("klaR", "poLCA", "clustMixType")) {
    m <- hstat_pkg_manquant(pkg, "Analyse X")
    expect_true(grepl("Analyse X", m, fixed = TRUE))
    expect_true(grepl(pkg, m, fixed = TRUE))
    # La commande d'installation, telle quelle
    expect_true(grepl(sprintf('install.packages("%s")', pkg), m, fixed = TRUE))
    # Et une voie de repli disponible SANS ce paquet
    expect_true(grepl("En attendant", m, fixed = TRUE))
    expect_true(grepl("ACM|AFDM", m))
  }
  # Un paquet sans repli declare reste explicite sur l'installation
  m <- hstat_pkg_manquant("truc")
  expect_true(grepl('install.packages("truc")', m, fixed = TRUE))
})

test_that("aucune des trois analyses ne renvoie encore un message d'impasse", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (pkg in c("klaR", "poLCA", "clustMixType")) {
    expect_false(grepl(sprintf("error = \"Package '%s' indisponible.\"", pkg),
                       src, fixed = TRUE), info = pkg)
    expect_true(grepl(sprintf('hstat_pkg_manquant("%s"', pkg), src, fixed = TRUE),
                info = pkg)
  }
})


# ===========================================================================
# RESOLUTION DES FIGURES DU RAPPORT
# ---------------------------------------------------------------------------
# Un rapport part chez un relecteur ou un imprimeur : a 150 dpi une figure est
# nette a l'ecran et floue sur papier, et le defaut ne se voit qu'une fois le
# document remis. D'ou un PLANCHER, verifie sur les pixels reellement produits
# et non sur l'argument passe.
# ===========================================================================

# Dimensions lues dans l'en-tete IHDR du PNG. Ecrit a la main plutot que confie
# a un paquet : `png` n'est pas garanti present, et l'en-tete PNG tient en
# quatre octets par dimension a des positions fixes.
.hstat_png_dims <- function(f) {
  r <- readBin(f, "raw", 33L)
  if (length(r) < 24L) return(c(largeur = NA_integer_, hauteur = NA_integer_))
  ent <- function(i) sum(as.integer(r[i:(i + 3L)]) * c(2^24, 2^16, 2^8, 1))
  c(largeur = ent(17L), hauteur = ent(21L))
}

.hstat_hist_figure <- function() list(
  list(module = "Visualisation", title = "Nuage", time = Sys.time(),
       meta = list(), tables = list(),
       plot = function() ggplot2::ggplot(data.frame(x = 1:8, y = (1:8)^2),
                                         ggplot2::aes(x, y)) + ggplot2::geom_point()))









# ===========================================================================
# AUDIT : DONNEES QUI CASSENT LA MISE EN FORME
# ---------------------------------------------------------------------------
# Cinq defauts trouves en soumettant les fonctions pures a des donnees
# degenerees. Aucun ne levait d'erreur visible : ils produisaient un document
# faux, ou un script que R refusait d'executer. Ce sont les pires.
# ===========================================================================





test_that("un nom de colonne prefixe d'un autre ne casse plus la formule", {
  # LE TRI PAR LONGUEUR NE SUFFISAIT PAS. Quand un nom est le PREFIXE d'un
  # autre et que les deux demandent des accents graves, le court se reinserait
  # DANS les accents du long : « A-1 » et « A-1-bis » donnaient ``A-1`-bis`,
  # que R refuse d'analyser (« attempt to use zero-length variable name »).
  # Le garde-fou qui existait cherchait la forme citee EXACTE -- « `A-1` » ne
  # figure pas dans « `A-1-bis` », il ne se declenchait donc jamais.
  # Des noms comme « Rdt-2023 » / « Rdt-2023-corrige » suffisent a le produire.
  analysable <- function(txt)
    !inherits(tryCatch(parse(text = txt), error = function(e) e), "error")

  f <- auto_quote_colnames("A-1-bis + A-1", c("A-1", "A-1-bis"))
  expect_equal(f, "`A-1-bis` + `A-1`")
  expect_true(analysable(f))
  # L'ordre de DECLARATION des colonnes ne doit rien changer.
  expect_equal(auto_quote_colnames("A-1-bis + A-1", c("A-1-bis", "A-1")), f)
  expect_true(analysable(
    auto_quote_colnames("Rdt-2023-corrige / Rdt-2023",
                        c("Rdt-2023", "Rdt-2023-corrige"))))

  # Ce qui marchait doit continuer a marcher.
  expect_equal(auto_quote_colnames("a + b", c("a", "b")), "a + b")
  expect_equal(auto_quote_colnames("2024 + 1", c("2024")), "`2024` + 1")
  # Un nom SANS caractere special n'est pas cite -- meme a cote d'un nom cite.
  expect_equal(auto_quote_colnames("Rdt-t/ha + Rdt", c("Rdt", "Rdt-t/ha")),
               "`Rdt-t/ha` + Rdt")
  # Une citation posee par l'utilisateur n'est pas redoublee.
  expect_equal(auto_quote_colnames("`Rdt-t/ha` * 2", c("Rdt-t/ha")),
               "`Rdt-t/ha` * 2")
  # mean() et sum() deviennent des operations LIGNE A LIGNE : `mean(a, b)` en R
  # ignorerait purement et simplement `b` (c'est l'argument `trim`).
  expect_equal(auto_quote_colnames("mean(c(a, b))", c("a", "b")),
               "rowMeans(cbind(a, b), na.rm=TRUE)")
  expect_equal(auto_quote_colnames("sum(a, b)", c("a", "b")),
               "rowSums(cbind(a, b), na.rm=TRUE)")
  # Et la reecriture survit a la citation : les deux passes se composent.
  g <- auto_quote_colnames("mean(c(A-1, A-1-bis))", c("A-1", "A-1-bis"))
  expect_true(analysable(g))
  expect_match(g, "rowMeans", fixed = TRUE)
  expect_match(g, "`A-1-bis`", fixed = TRUE)
})

test_that("la matrice de p-values est symetrique et diagonale a 1", {
  pd <- data.frame(Niveau1 = c("A", "A", "B"), Niveau2 = c("B", "C", "C"),
                   p_adj = c(0.01, 0.20, 0.75), stringsAsFactors = FALSE)
  m <- build_pvalue_matrix(pd, c("A", "B", "C"))
  # Une comparaison n'a pas de sens : A vs B et B vs A sont le meme test.
  expect_equal(m, t(m))
  expect_equal(m["A", "B"], 0.01)
  expect_equal(m["C", "A"], 0.20)
  # La diagonale vaut 1, pas NA : un niveau compare a lui-meme n'est jamais
  # different, et un NA sur la diagonale ferait echouer les lettres de groupes.
  expect_equal(unname(diag(m)), rep(1, 3))
  # Un niveau sans aucune paire reste present, en NA hors diagonale.
  m4 <- build_pvalue_matrix(pd, c("A", "B", "C", "D"))
  expect_equal(dim(m4), c(4L, 4L))
  expect_true(all(is.na(m4["D", c("A", "B", "C")])))
  expect_equal(unname(m4["D", "D"]), 1)
})

test_that("chaque transformation revient exactement sur ses pas", {
  # L'ALLER-RETOUR EST L'INVARIANT FORT de cette famille. Les comparaisons
  # post-hoc affichent des moyennes RETRO-TRANSFORMEES : une inverse fausse ne
  # leve pas, elle rend des nombres dans la bonne unite et du mauvais ordre de
  # grandeur. Ni `apply_variable_transformation()` ni `back_transform_values()`
  # n'etaient appelees par un test.
  cas <- list(
    log      = c(1, 2, 5, 10),
    log1p    = c(0, 1, 4, 9),
    log10    = c(1, 10, 100),
    sqrt     = c(0, 1, 4, 9),
    cuberoot = c(-8, -1, 0, 1, 8),
    arcsin   = c(0, 0.25, 0.5, 1),
    logit    = c(0.1, 0.5, 0.9))
  for (m in names(cas)) {
    v <- cas[[m]]
    t <- as.numeric(apply_variable_transformation(v, m))
    expect_equal(back_transform_values(t, m), v, tolerance = 1e-10,
                 info = paste("aller-retour :", m))
  }

  # Quelques valeurs POSEES, pour que l'aller-retour ne puisse pas etre
  # satisfait par deux fonctions fausses qui s'annulent.
  expect_equal(as.numeric(apply_variable_transformation(c(1, 10, 100), "log10")),
               c(0, 1, 2))
  expect_equal(as.numeric(apply_variable_transformation(c(0, 1, 4), "sqrt")),
               c(0, 1, 2))
  expect_equal(as.numeric(apply_variable_transformation(0.5, "logit")), 0)

  # LA RACINE CUBIQUE DOIT ACCEPTER LES NEGATIFS -- c'est sa raison d'etre,
  # et le message de `sqrt` y renvoie explicitement. `x^(1/3)` nu rendrait NaN.
  cr <- apply_variable_transformation(c(-8, 8), "cuberoot")
  expect_equal(as.numeric(cr), c(-2, 2))
  expect_false(any(is.nan(cr)))

  # Les NA traversent sans etre comptes comme des valeurs fautives.
  expect_true(is.na(apply_variable_transformation(c(1, NA, 4), "sqrt")[2]))
  # Une methode inconnue est refusee, pas appliquee au hasard.
  expect_error(apply_variable_transformation(1:3, "racine_quatrieme"))
  # Retro-transformer par une methode inconnue rend l'entree inchangee.
  expect_equal(back_transform_values(c(1, 2), "inconnue"), c(1, 2))
})

test_that("le controle de faisabilite dit exactement ce que l'application fera", {
  # DEUX LISTES DE CONDITIONS QUI DOIVENT S'ACCORDER : le controle annonce, et
  # l'application leve. Elles vivent dans deux `switch()` distincts, donc elles
  # peuvent deriver -- et une derive laisserait soit un bouton actif qui fait
  # tomber la sortie, soit un refus incomprehensible sur des donnees valides.
  meth <- c("log", "log1p", "log10", "sqrt", "cuberoot", "arcsin", "logit")
  cas <- list(negatifs   = c(-1, 2, 3),
              zeros      = c(0, 1, 2),
              hors_01    = c(0.5, 1.5),
              bornes_01  = c(0, 0.5, 1),
              positifs   = c(1, 2, 3))
  for (nm in names(cas)) for (m in meth) {
    ok   <- isTRUE(check_transformation_feasibility(cas[[nm]], m)$ok)
    leve <- inherits(try(apply_variable_transformation(cas[[nm]], m),
                         silent = TRUE), "try-error")
    expect_false(ok == leve, info = paste(nm, "/", m,
                 ": controle =", ok, ", application leve =", leve))
  }
  # Un vecteur entierement manquant est refuse, et pour cette raison-la.
  vide <- check_transformation_feasibility(c(NA_real_, NA_real_), "log")
  expect_false(vide$ok)
  expect_match(vide$message, "non-NA")
  # Un refus NOMME le nombre d'observations fautives : « 2 valeurs <= 0 » se
  # corrige, « impossible » ne se corrige pas.
  expect_match(check_transformation_feasibility(c(-1, 0, 5), "log")$message, "^2 ")
  # Et une acceptation annonce l'effectif retenu.
  expect_match(check_transformation_feasibility(c(1, 2, NA), "log")$message, "n = 2")
})

test_that("Box's M refuse poliment ce qu'il ne peut pas tester", {
  # QUATRE GARDE-FOUS, et aucun n'etait teste. Ils protegent l'appel a
  # `heplots::boxM()`, qui leve ou rend des NaN sur des donnees degenerees --
  # et l'onglet MANOVA tombe avec lui. Ils rendent tous NA et une PHRASE :
  # « Test impossible » se lit, un NA nu ne se lit pas.
  set.seed(11)
  Y <- matrix(rnorm(60), ncol = 3)
  g <- factor(rep(c("a", "b"), each = 10))
  nul <- function(r) is.na(r$chi2) && is.na(r$df) && is.na(r$p.value)

  # 1. Moins de deux groupes, trop peu d'observations, moins de deux colonnes.
  for (cas in list(list(Y, factor(rep("a", 20))),
                   list(Y[1:4, ], factor(rep(c("a", "b"), each = 2))),
                   list(Y[, 1, drop = FALSE], g))) {
    r <- do.call(box_m_test, cas)
    expect_true(nul(r))
    expect_match(r$conclusion, "impossible")
  }

  # 2. Un groupe plus petit que p+1 : sa covariance ne peut pas etre estimee.
  #    Le message NOMME les deux nombres -- sans eux, l'utilisateur ne sait pas
  #    de combien il manque.
  r <- box_m_test(Y[1:8, ], factor(c(rep("a", 6), rep("b", 2))))
  expect_true(nul(r))
  expect_match(r$conclusion, "min n=2")
  expect_match(r$conclusion, "p\\+1=4")

  # 3. Colonnes colineaires : covariance singuliere.
  col <- box_m_test(cbind(Y[, 1], Y[, 1] * 2, Y[, 2]), g)
  expect_true(nul(col))
  expect_match(col$conclusion, "singuli")

  # 4. LE RANG, PAS LE DETERMINANT. C'est la decision documentee dans le corps
  #    de la fonction, et elle se verifie : a l'echelle 1e-6, le determinant de
  #    la covariance vaut ~2e-37 -- tout seuil sur le determinant crierait a la
  #    singularite -- alors que le rang reste plein. Des variables mesurees en
  #    microgrammes ne sont pas colineaires pour autant.
  petit <- Y * 1e-6
  expect_lt(det(stats::cov(petit[1:10, ])), 1e-30)     # le piege
  expect_equal(qr(stats::cov(petit[1:10, ]))$rank, 3)  # la realite
  expect_false(grepl("singuli", box_m_test(petit, g)$conclusion))
})

test_that("PERMDISP, Mardia et la silhouette refusent aussi sans faire tomber la sortie", {
  set.seed(12)
  Y <- matrix(rnorm(60), ncol = 3)

  # PERMDISP : moins de deux groupes, ou moins de cinq observations.
  for (cas in list(list(Y, factor(rep("a", 20))),
                   list(Y[1:4, ], factor(rep(c("a", "b"), each = 2))))) {
    r <- do.call(permdisp_test, cas)
    expect_true(is.na(r$F) && is.na(r$p.value))
    expect_match(r$conclusion, "impossible")
  }

  # Mardia exige n >= 8 ET p >= 2 : l'asymetrie et l'aplatissement
  # multivaries n'ont pas de sens sur une seule variable.
  m6 <- multivariate_normality_mardia(Y[1:6, ])
  expect_true(is.na(m6$skewness) && is.na(m6$kurtosis))
  expect_match(m6$conclusion, "trop petit")
  expect_equal(m6$n, 6L); expect_equal(m6$p, 3L)
  expect_match(multivariate_normality_mardia(Y[, 1, drop = FALSE])$conclusion,
               "trop petit")

  # La silhouette n'existe pas avec un seul groupe : il n'y a aucun voisin
  # auquel se comparer. NA, et surtout pas zero -- zero se lirait comme
  # « partition indifferente », ce qui est un resultat.
  sil <- hstat_silhouette_mean(Y, rep(1, 20))
  expect_true(is.na(sil))
  expect_false(isTRUE(sil == 0))
})

test_that("la taille d'effet MANOVA ne s'invente pas un effet total", {
  # eta² partiel = 1 - Wilks^(1/s), s = min(p, ddl du numerateur).
  # Valeurs posees a la main : Wilks = 0,5 et s = 2 donnent 1 - sqrt(0,5).
  d <- data.frame(Wilks = c(0.5, 0.9, 1.0), Pillai = c(0.5, 0.1, 0.0),
                  ddl_num = c(2, 2, 2))
  r <- manova_effect_sizes(d, p = 3)
  expect_equal(r$eta2_partial[1], 1 - sqrt(0.5))
  expect_equal(r$eta2_pillai[1], 0.25)
  # Wilks = 1 : aucune variance expliquee, eta² nul. La borne basse.
  expect_equal(r$eta2_partial[3], 0)
  # s = min(p, ddl) : avec p = 1, s vaut 1 quel que soit le ddl.
  expect_equal(manova_effect_sizes(d, p = 1)$eta2_partial[1], 0.5)

  # UN DEGRE DE LIBERTE DEGENERE NE DOIT PAS PRODUIRE UN EFFET MAXIMAL.
  # s = 0 donne Wilks^(1/0) = Wilks^Inf = 0, donc eta² = 1 -- la taille
  # d'effet la plus forte possible, tiree d'une statistique non calculable,
  # et qu'`interpret_manova_effect()` qualifierait d'« important ». Et
  # Pillai / 0 rend Inf. NA se voit ; 1,00 se croit.
  z <- manova_effect_sizes(
         data.frame(Wilks = 0.5, Pillai = 0.5, ddl_num = 0), p = 3)
  expect_true(is.na(z$eta2_partial))
  expect_true(is.na(z$eta2_pillai))
  expect_false(any(is.infinite(c(z$eta2_partial, z$eta2_pillai))))
  # Un ddl manquant se comporte pareil.
  na <- manova_effect_sizes(
          data.frame(Wilks = 0.5, Pillai = 0.5, ddl_num = NA_real_), p = 3)
  expect_true(is.na(na$eta2_partial))
})

test_that("l'interpretation d'un effet MANOVA change de palier aux bons seuils", {
  # Seuils de Cohen : 0,01 / 0,06 / 0,14. Un palier deplace laisse une phrase
  # parfaitement lisible -- seule la paire (juste avant / juste apres) le voit.
  expect_match(interpret_manova_effect(0.01, 0.009), "négligeable")
  expect_match(interpret_manova_effect(0.01, 0.010), "faible")
  expect_match(interpret_manova_effect(0.01, 0.059), "faible")
  expect_match(interpret_manova_effect(0.01, 0.060), "modéré")
  expect_match(interpret_manova_effect(0.01, 0.139), "modéré")
  expect_match(interpret_manova_effect(0.01, 0.140), "important")
  # La significativite est portee a part de la taille d'effet.
  expect_match(interpret_manova_effect(0.049), "significatif")
  expect_match(interpret_manova_effect(0.050), "non significatif")
  # Et une p-value absente ne fait pas tomber la ligne.
  expect_match(interpret_manova_effect(NA), "non disponible")
})

test_that("l'accord entre partitions ne depend pas du nom des groupes", {
  # C'EST TOUT L'INTERET DE L'INDICE DE RAND, et la raison pour laquelle il
  # est employe ici : les etiquettes de classe sont ARBITRAIRES -- un k-means
  # relance rend les memes groupes sous d'autres numeros. Comparer les
  # etiquettes une a une donnerait un accord effondre sur deux partitions
  # identiques. L'indice compare des PAIRES : deux individus sont-ils
  # ensemble des deux cotes, oui ou non.
  #
  # La fonction alimente le chiffre de stabilite par bootstrap de la CAH
  # (app_server.R) et n'etait couverte par aucun test.
  a <- c(1, 1, 2, 2)
  expect_equal(hstat_pair_agreement(a, a), 1)
  expect_equal(hstat_pair_agreement(a, c(2, 2, 1, 1)), 1)        # renumerotees
  expect_equal(hstat_pair_agreement(a, c("x", "x", "y", "y")), 1) # renommees

  # Partition croisee : sur 6 paires, 2 s'accordent.
  #   (1,4) et (2,3) : separes des deux cotes. Les quatre autres divergent.
  expect_equal(hstat_pair_agreement(a, c(1, 2, 1, 2)), 2 / 6)
  # Tout dans un seul groupe : les 3 paires reunies chez `a` s'accordent...
  expect_equal(hstat_pair_agreement(a, rep(1, 4)), 2 / 6)
  # ... et l'accord n'est jamais hors de [0 ; 1].
  for (b in list(a, c(2, 2, 1, 1), c(1, 2, 1, 2), rep(1, 4), 1:4)) {
    r <- hstat_pair_agreement(a, b)
    expect_true(is.finite(r) && r >= 0 && r <= 1)
  }
})

test_that("hstat_part_equilibre rend un verdict, jamais une erreur", {
  # Passer le vecteur d'affectation au lieu de sa table est une confusion
  # facile ; elle faisait tomber toute la sortie de l'analyse.
  expect_equal(hstat_part_equilibre(factor(c("a", "a", "b")))$verdict,
               "indeterminable")
  expect_equal(hstat_part_equilibre(c("a", "b"))$verdict, "indeterminable")
  expect_equal(hstat_part_equilibre(list())$verdict, "indeterminable")
  expect_equal(hstat_part_equilibre(c(NA, NaN, Inf))$verdict, "indeterminable")
  # Des effectifs en texte restent exploitables
  expect_equal(hstat_part_equilibre(c("96", "4"))$verdict, "warn")
  # Le comportement nominal est intact
  expect_equal(hstat_part_equilibre(c(50, 50))$verdict, "ok")
  expect_equal(hstat_part_equilibre(table(rep(c("a", "b"), c(96, 4))))$verdict,
               "warn")
})


# ===========================================================================
# AUDIT : NE RIEN CONSEILLER SUR UNE VARIABLE VIDE
# ---------------------------------------------------------------------------
# Le plus grave defaut de cette passe : le moteur de recommandation conseillait
# un chi-deux d'independance sur une colonne entierement vide. Conseiller avec
# aplomb une analyse impossible est pire que ne rien conseiller — c'est ce
# qu'un utilisateur suit sans se mefier.
# ===========================================================================






# ===========================================================================
# AUDIT : LE POLYFILL OBSOLETE DE PLOTLY
# ---------------------------------------------------------------------------
# plotly attache un polyfill « typedarray » destine aux navigateurs sans
# tableaux types (IE9). Son code reference `GLOBAL`, variable de Node.js
# inexistante dans un navigateur : une ReferenceError etait levee sur chaque
# page portant un graphique interactif. Rien ne cassait, mais une erreur
# permanente en console masque les vraies.
# ===========================================================================

test_that("le polyfill typedarray est retire des graphiques interactifs", {
  skip_if_not_installed("plotly")
  p <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers")
  avant <- vapply(plotly::plotly_build(p)$dependencies,
                  function(d) d$name, character(1))
  # Le polyfill est bien la avant nettoyage : sans cela le test ne prouve rien.
  skip_if_not("typedarray" %in% avant,
              "cette version de plotly n'attache plus typedarray")

  b <- hstat_plotly_clean(p)
  apres <- vapply(b$dependencies, function(d) d$name, character(1))
  expect_false("typedarray" %in% apres)
  # Et plotly lui-meme doit rester : retirer trop casserait tout affichage.
  expect_true(any(grepl("plotly", apres)))
  expect_true(inherits(b, "plotly"))
  # Idempotent, et tolerant a l'absence d'objet
  expect_false("typedarray" %in%
    vapply(hstat_plotly_clean(b)$dependencies, function(d) d$name, character(1)))
  expect_null(hstat_plotly_clean(NULL))
})

test_that("le nettoyage est pose sur renderPlotly, pas sur chaque appel", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # Le repli `renderPlotly` a rejoint le socle : sa DEFINITION appartient au
  # paquet, seule la decision de l'installer reste au pont.
  skip_if(is.na(.hstat_socle_path()))
  src <- paste(readLines(.hstat_socle_path(), warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  # Habiller chaque appel ne marcherait pas : leurs corps comportent des
  # `return()` qui sauteraient le nettoyage. C'est `exprToFunction` qui rend
  # l'interception correcte.
  # `renderPlotly` est desormais pose par un AIGUILLAGE (`poser(...)`) plutot
  # que par une affectation directe : ce qui compte n'a pas change -- le
  # nettoyage passe par `exprToFunction`, pas par un habillage de l'expression.
  expect_true(grepl("poser(\"renderPlotly\"", src, fixed = TRUE))
  expect_true(grepl("exprToFunction", src, fixed = TRUE))
  expect_true(grepl("hstat_plotly_clean(fn())", src, fixed = TRUE))
})


# ===========================================================================
# L'ARBORESCENCE DU README DOIT DECRIRE LE DEPOT REEL
# ---------------------------------------------------------------------------
# La section « Project structure » est du markdown statique : rien ne la met a
# jour quand un fichier arrive ou disparait. Elle avait derive — cinq fichiers
# reels manquaient (dont le workflow de CI et hstat-session.js)
# et un fichier inexistant y figurait (tests/test-hstat.R). Une documentation
# qui invente un fichier est pire qu'une documentation absente : on le cherche.
# ===========================================================================

# Reconstitue les chemins decrits par l'arbre du README. Un noeud est un
# FICHIER s'il n'est le prefixe d'aucun autre : inutile de deviner d'apres
# l'extension, l'arbre porte deja l'information.
.hstat_readme_arbre <- function(readme) {
  txt <- paste(readLines(readme, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  bloc <- strsplit(strsplit(txt, "## Project structure", fixed = TRUE)[[1]][2],
                   "```", fixed = TRUE)[[1]][2]
  lignes <- strsplit(bloc, "\n", fixed = TRUE)[[1]]
  pile <- character(0); chemins <- character(0)
  for (l in lignes) {
    if (!nzchar(trimws(l)) || identical(trimws(l), ".")) next
    m <- regmatches(l, regexec("^([\\s│├└─]*)(?:├──|└──)\\s+(\\S+)", l, perl = TRUE))[[1]]
    if (length(m) < 3) next
    prof <- nchar(m[2]) %/% 4L
    length(pile) <- max(length(pile), prof + 1L)
    pile[prof + 1L] <- m[3]
    chemins <- c(chemins, paste(pile[seq_len(prof + 1L)], collapse = "/"))
  }
  chemins[!vapply(chemins, function(c0)
    any(startsWith(chemins, paste0(c0, "/"))), logical(1))]
}

test_that("l'arborescence du README decrit exactement le depot", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  skip_if(!nzchar(Sys.which("git")), "git indisponible")
  suivis <- suppressWarnings(system2("git", c("-C", shQuote(root), "ls-files"),
                                     stdout = TRUE, stderr = FALSE))
  skip_if(!length(suivis) || !any(grepl("DESCRIPTION", suivis, fixed = TRUE)),
          "depot git non lisible depuis les tests")

  decrits <- .hstat_readme_arbre(file.path(root, "README.md"))
  # Le parseur doit avoir vu quelque chose : sans ce garde-fou, un arbre
  # illisible donnerait deux ensembles vides et le test passerait a tort.
  expect_gt(length(decrits), 40L)

  absents  <- setdiff(suivis, decrits)
  inventes <- setdiff(decrits, suivis)
  expect_equal(absents, character(0),
    info = paste("Fichiers du depot absents du README :\n  ",
                 paste(absents, collapse = "\n   ")))
  expect_equal(inventes, character(0),
    info = paste("Fichiers listes par le README mais inexistants :\n  ",
                 paste(inventes, collapse = "\n   ")))
})


# ===========================================================================
# SYSTEME DE CODES HIERARCHIQUE (equivalent du code system de MAXQDA)
# ---------------------------------------------------------------------------
# Un livre de codes plat oblige a encoder la hierarchie dans les libelles
# (« Prix - trop cher »), ce qui interdit toute agregation : on ne peut plus
# demander « combien de segments parlent du prix, tous sous-codes confondus ».
# ===========================================================================

.hstat_cb_essai <- function() {
  cb <- hstat_code_add(hstat_code_new_codebook(), "Prix")
  p  <- cb$code_id[cb$label == "Prix"]
  cb <- hstat_code_add(cb, "Trop cher", parent_id = p)
  tc <- cb$code_id[cb$label == "Trop cher"]
  cb <- hstat_code_add(cb, "Livraison tardive", parent_id = tc)
  cb <- hstat_code_add(cb, "Service")
  cb
}

test_that("un code peut avoir un parent, et l'arbre s'affiche dans l'ordre", {
  cb <- .hstat_cb_essai()
  tr <- hstat_code_tree(cb)
  expect_equal(nrow(tr), nrow(cb))
  # Chaque code suit son parent, et la profondeur sert a l'indentation
  expect_equal(tr$profondeur[tr$code_id == cb$code_id[cb$label == "Prix"]], 0L)
  expect_equal(tr$profondeur[tr$code_id == cb$code_id[cb$label == "Trop cher"]], 1L)
  expect_equal(tr$profondeur[tr$code_id == cb$code_id[cb$label == "Livraison tardive"]], 2L)
  expect_true("Prix > Trop cher > Livraison tardive" %in% tr$chemin)
  # Un enfant est toujours liste apres son parent
  expect_lt(which(tr$code_id == cb$code_id[cb$label == "Prix"]),
            which(tr$code_id == cb$code_id[cb$label == "Trop cher"]))
})

test_that("le meme libelle est permis sous deux parents, interdit sous le meme", {
  cb <- .hstat_cb_essai()
  p <- cb$code_id[cb$label == "Prix"]; s <- cb$code_id[cb$label == "Service"]
  cb <- hstat_code_add(cb, "Qualite", parent_id = p)
  n <- nrow(cb)
  # « Prix > Qualite » et « Service > Qualite » sont deux codes legitimes
  cb <- hstat_code_add(cb, "Qualite", parent_id = s)
  expect_equal(nrow(cb), n + 1L)
  # Deux fois le meme libelle sous le meme parent rendrait la restitution ambigue
  expect_equal(nrow(hstat_code_add(cb, "Qualite", parent_id = s)), n + 1L)
})

test_that("les effectifs sont cumules sur toute la branche", {
  cb <- .hstat_cb_essai()
  p  <- cb$code_id[cb$label == "Prix"]
  tc <- cb$code_id[cb$label == "Trop cher"]
  lt <- cb$code_id[cb$label == "Livraison tardive"]
  seg <- data.frame(seg_id = paste0("s", 1:4),
                    doc_id = c("d1", "d1", "d2", "d3"),
                    code_id = c(tc, tc, lt, p),
                    start = 1, end = 2, text = "x", source = "m", created = "",
                    stringsAsFactors = FALSE)
  cn <- hstat_code_counts(cb, seg)
  ligne <- function(id) cn[cn$code_id == id, ]
  # Le code parent porte 1 segment en propre, 4 sur la branche
  expect_equal(ligne(p)$n_seg, 1L)
  expect_equal(ligne(p)$n_seg_cumul, 4L)
  expect_equal(ligne(p)$n_doc_cumul, 3L)
  expect_equal(ligne(tc)$n_seg_cumul, 3L)
  expect_equal(ligne(lt)$n_seg_cumul, 1L)
  # Un code sans descendance : cumul = effectif propre
  s <- cb$code_id[cb$label == "Service"]
  expect_equal(ligne(s)$n_seg_cumul, ligne(s)$n_seg)
})

test_that("supprimer un code ne fait pas disparaitre le codage de ses enfants", {
  cb <- .hstat_cb_essai()
  p <- cb$code_id[cb$label == "Prix"]
  # Par defaut les sous-codes REMONTENT au parent du code supprime
  ap <- hstat_code_remove(cb, p)
  expect_false(p %in% ap$code_id)
  expect_true("Trop cher" %in% ap$label)
  expect_equal(ap$parent_id[ap$label == "Trop cher"], "")
  # Le petit-enfant garde son propre parent
  expect_equal(ap$parent_id[ap$label == "Livraison tardive"],
               ap$code_id[ap$label == "Trop cher"])
  # Sur demande explicite, la branche entiere part
  br <- hstat_code_remove(cb, p, avec_descendants = TRUE)
  expect_equal(sort(br$label), "Service")
})

test_that("un code ne peut pas devenir son propre descendant", {
  cb <- .hstat_cb_essai()
  p  <- cb$code_id[cb$label == "Prix"]
  lt <- cb$code_id[cb$label == "Livraison tardive"]
  # Deplacer Prix sous son propre petit-enfant detacherait la branche
  ap <- hstat_code_update(cb, p, parent_id = lt)
  expect_equal(ap$parent_id[ap$code_id == p], "")
  expect_equal(ap$parent_id[ap$code_id == p], cb$parent_id[cb$code_id == p])
  # Ni sous lui-meme
  expect_equal(hstat_code_update(cb, p, parent_id = p)$parent_id[
                 cb$code_id == p], "")
  # Un deplacement legitime, lui, passe
  s <- cb$code_id[cb$label == "Service"]
  expect_equal(hstat_code_update(cb, s, parent_id = p)$parent_id[
                 cb$code_id == s], p)
})

test_that("un livre de codes ancien ou abime est repare, jamais refuse", {
  # Projet enregistre avant la hierarchie : la colonne est recreee
  vieux <- data.frame(code_id = "a", label = "A", color = "#fff", memo = "",
                      keywords = "", created = "", stringsAsFactors = FALSE)
  m <- hstat_code_migrate_codebook(vieux)
  expect_true("parent_id" %in% names(m))
  expect_equal(m$parent_id, "")
  expect_equal(names(m), HSTAT_CODE_COLS)

  # Parent introuvable : le code remonte a la racine plutot que de disparaitre
  casse <- data.frame(code_id = c("a", "b"), label = c("A", "B"),
                      color = "#fff", memo = "", keywords = "",
                      parent_id = c("", "fantome"), created = "",
                      stringsAsFactors = FALSE)
  expect_equal(hstat_code_migrate_codebook(casse)$parent_id, c("", ""))

  # Cycle (fichier edite a la main) : l'affichage de l'arbre boucleait
  cycle <- data.frame(code_id = c("a", "b"), label = c("A", "B"),
                      color = "#fff", memo = "", keywords = "",
                      parent_id = c("b", "a"), created = "",
                      stringsAsFactors = FALSE)
  repare <- hstat_code_migrate_codebook(cycle)
  expect_equal(repare$parent_id, c("", ""))
  expect_equal(nrow(hstat_code_tree(repare)), 2L)
})

test_that("ancetres et descendants se lisent dans les deux sens", {
  cb <- .hstat_cb_essai()
  p  <- cb$code_id[cb$label == "Prix"]
  tc <- cb$code_id[cb$label == "Trop cher"]
  lt <- cb$code_id[cb$label == "Livraison tardive"]
  expect_equal(hstat_code_ancestors(cb, lt), c(tc, p))
  expect_equal(hstat_code_ancestors(cb, p), character(0))
  expect_setequal(hstat_code_descendants(cb, p), c(tc, lt))
  expect_setequal(hstat_code_descendants(cb, p, inclus = TRUE), c(p, tc, lt))
  expect_equal(hstat_code_descendants(cb, cb$code_id[cb$label == "Service"]),
               character(0))
})


# ===========================================================================
# MEMOS (equivalent des memos de MAXQDA)
# ---------------------------------------------------------------------------
# C'est la piece qui transforme un codage en analyse : pourquoi ce code existe,
# ou passe sa frontiere, ce qu'un entretien a d'atypique, l'hypothese qui se
# dessine. C'est aussi ce qu'un relecteur demande pour comprendre le chemin.
# ===========================================================================

test_that("un memo se pose sur un code, un document, un segment ou rien", {
  m <- hstat_memo_new()
  expect_equal(nrow(m), 0L)
  expect_equal(names(m), HSTAT_MEMO_COLS)
  m <- hstat_memo_add(m, "code", "prix", "Frontiere", "Ne code pas le SAV.")
  m <- hstat_memo_add(m, "document", "d3", "", "Ancien salarie.")
  m <- hstat_memo_add(m, "segment", "s12", "", "Contredit le debut.")
  m <- hstat_memo_add(m, "libre", "", "Hypothese", "Les jeunes critiquent le prix.")
  expect_equal(nrow(m), 4L)
  expect_equal(nrow(hstat_memo_for(m, "code")), 1L)
  expect_equal(hstat_memo_for(m, "document", "d3")$texte, "Ancien salarie.")
  expect_equal(nrow(hstat_memo_for(m, "document", "inexistant")), 0L)
  # Un memo libre ne s'accroche a rien
  expect_equal(hstat_memo_for(m, "libre")$cible_id, "")
})

test_that("un memo vide n'est pas cree, un titre absent est deduit", {
  m <- hstat_memo_add(hstat_memo_new(), "libre", "", "", "")
  expect_equal(nrow(m), 0L)
  expect_equal(nrow(hstat_memo_add(m, "libre", "", "", "   ")), 0L)
  # Titre deduit du debut du texte : la liste reste lisible
  long <- paste(rep("mot", 40), collapse = " ")
  m2 <- hstat_memo_add(hstat_memo_new(), "libre", "", "", long)
  expect_true(nzchar(m2$titre))
  expect_lte(nchar(m2$titre), 64L)
  expect_true(grepl("\\.\\.\\.$", m2$titre))
  # Un titre seul suffit a creer le memo
  expect_equal(nrow(hstat_memo_add(hstat_memo_new(), "libre", "", "Idee", "")), 1L)
})

test_that("un type de cible inconnu bascule en memo libre, jamais orphelin", {
  m <- hstat_memo_add(hstat_memo_new(), "n'importe quoi", "x", "T", "texte")
  expect_equal(m$cible_type, "libre")
  expect_equal(m$cible_id, "")
  # Idem a la relecture d'un fichier abime
  abime <- data.frame(memo_id = "m1", cible_type = "inconnu", cible_id = "z",
                      titre = "T", texte = "t", auteur = "", created = "",
                      modified = "", stringsAsFactors = FALSE)
  expect_equal(hstat_memo_migrate(abime)$cible_type, "libre")
  # Fichier ancien sans certaines colonnes
  vieux <- data.frame(memo_id = "m1", texte = "t", stringsAsFactors = FALSE)
  expect_equal(names(hstat_memo_migrate(vieux)), HSTAT_MEMO_COLS)
})

test_that("la recherche de memos ignore la casse et les accents", {
  m <- hstat_memo_add(hstat_memo_new(), "libre", "", "Hypothèse",
                      "Les critiques viennent des plus jeunes.")
  expect_equal(nrow(hstat_memo_search(m, "hypothese")), 1L)
  expect_equal(nrow(hstat_memo_search(m, "HYPOTHÈSE")), 1L)
  expect_equal(nrow(hstat_memo_search(m, "jeunes")), 1L)     # dans le corps
  expect_equal(nrow(hstat_memo_search(m, "introuvable")), 0L)
  # Une recherche vide ne filtre rien
  expect_equal(nrow(hstat_memo_search(m, "")), 1L)
})

test_that("modification et suppression d'un memo", {
  m <- hstat_memo_add(hstat_memo_new(), "code", "prix", "Avant", "texte")
  id <- m$memo_id[1]
  m2 <- hstat_memo_update(m, id, titre = "Apres", texte = "nouveau")
  expect_equal(m2$titre, "Apres")
  expect_equal(m2$texte, "nouveau")
  expect_equal(m2$created, m$created)      # la creation ne bouge pas
  # Un identifiant inconnu ne casse rien
  expect_equal(hstat_memo_update(m, "fantome", titre = "X")$titre, "Avant")
  expect_equal(nrow(hstat_memo_remove(m2, id)), 0L)
  expect_equal(nrow(hstat_memo_remove(m2, "fantome")), 1L)
})

test_that("les memos deja portes par le livre de codes sont repris sans doublon", {
  cb <- hstat_code_add(hstat_code_new_codebook(), "Service",
                       memo = "Tout ce qui touche a l'accueil.")
  m <- hstat_memo_sync_codes(hstat_memo_new(), cb)
  expect_equal(nrow(m), 1L)
  expect_equal(m$cible_type, "code")
  expect_equal(m$titre, "Service")
  # Rejouer la reprise ne duplique pas
  expect_equal(nrow(hstat_memo_sync_codes(m, cb)), 1L)
  # Un code sans memo n'en fabrique pas
  cb2 <- hstat_code_add(cb, "Prix")
  expect_equal(nrow(hstat_memo_sync_codes(hstat_memo_new(), cb2)), 1L)
})

test_that("le resume des memos couvre les quatre cibles", {
  m <- hstat_memo_add(hstat_memo_new(), "code", "a", "T", "t")
  m <- hstat_memo_add(m, "code", "b", "T", "t")
  r <- hstat_memo_resume(m)
  expect_equal(nrow(r), length(HSTAT_MEMO_CIBLES))
  expect_equal(r$Memos[r$Cible == "Code"], 2L)
  expect_equal(r$Memos[r$Cible == "Document"], 0L)
  expect_equal(sum(hstat_memo_resume(hstat_memo_new())$Memos), 0L)
})


# ===========================================================================
# CLASSEUR EXCEL : COMBINER PLUSIEURS FEUILLES
# ---------------------------------------------------------------------------
# Un classeur d'enquete porte souvent une feuille par annee, par site ou par
# vague. Ne lire que la premiere revient a jeter le reste des donnees. Les
# feuilles sont donc lues en une liste de tableaux et passees au moteur de
# fusion qui sert deja aux fichiers multiples — pas de logique parallele.
# ===========================================================================

.hstat_classeur_essai <- function() {
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(
    "2023" = data.frame(id = 1:3, site = c("A", "B", "A"), val = c(1, 2, 3)),
    "2024" = data.frame(id = 4:6, site = c("A", "C", "B"), val = c(4, 5, 6)),
    "Notes" = data.frame(),
    "Referentiel" = data.frame(site = c("A", "B", "C"),
                               region = c("Nord", "Sud", "Est"))), f)
  f
}

test_that("les feuilles d'un classeur sont listees, sans jamais lever d'erreur", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  f <- .hstat_classeur_essai()
  on.exit(unlink(f), add = TRUE)
  expect_equal(hstat_excel_sheets(f), c("2023", "2024", "Notes", "Referentiel"))
  # La fonction alimente une sortie Shiny : une erreur y ferait tomber tout le
  # panneau de chargement. Tout ce qui n'est pas un classeur rend character(0).
  expect_equal(hstat_excel_sheets(tempfile(fileext = ".csv")), character(0))
  expect_equal(hstat_excel_sheets("/introuvable.xlsx"), character(0))
  expect_equal(hstat_excel_sheets(NULL), character(0))
  expect_equal(hstat_excel_sheets(""), character(0))
  expect_equal(hstat_excel_sheets(character(0)), character(0))
})

test_that("une feuille vide est ecartee et nommee, sans bloquer les autres", {
  skip_if_not_installed("writexl")
  f <- .hstat_classeur_essai()
  on.exit(unlink(f), add = TRUE)
  r <- hstat_excel_read_sheets(f, c("2023", "2024", "Notes"))
  expect_length(r$frames, 2L)
  expect_equal(r$names, c("2023", "2024"))
  expect_equal(r$ignorees, "Notes")
  # Sur un classeur de douze feuilles, une seule mal formee ne doit pas tout
  # bloquer — mais l'utilisateur doit savoir laquelle a saute.
  expect_true(grepl("Notes", r$msg, fixed = TRUE))
  expect_true(grepl("écartée", r$msg, fixed = TRUE))

  # Sans precision, toutes les feuilles sont lues
  expect_equal(hstat_excel_read_sheets(f)$names, c("2023", "2024", "Referentiel"))
  # Feuille inexistante : message clair, pas d'erreur
  expect_length(hstat_excel_read_sheets(f, "Fantome")$frames, 0L)
  expect_length(hstat_excel_read_sheets(tempfile(fileext = ".csv"))$frames, 0L)
})

test_that("le diagnostic conseille l'empilement ou la jointure selon la structure", {
  skip_if_not_installed("writexl")
  f <- .hstat_classeur_essai()
  on.exit(unlink(f), add = TRUE)

  # Memes colonnes : on empile
  meme <- hstat_excel_read_sheets(f, c("2023", "2024"))
  a <- hstat_excel_compat(meme$frames, meme$names)
  expect_true(a$identiques)
  expect_equal(a$suggestion, "rows")
  expect_true(grepl("empilement", a$msg, fixed = TRUE))

  # Structures differentes : on joint par une cle
  mixte <- hstat_excel_read_sheets(f, c("2023", "Referentiel"))
  b <- hstat_excel_compat(mixte$frames, mixte$names)
  expect_false(b$identiques)
  expect_equal(b$communes, "site")
  expect_equal(b$suggestion, "inner")
  expect_true(grepl("jointure", b$msg, ignore.case = TRUE))

  # Aucune colonne commune : ni l'un ni l'autre n'a de sens, et on le dit
  c0 <- hstat_excel_compat(list(data.frame(a = 1), data.frame(b = 2)))
  expect_true(grepl("AUCUNE colonne en commun", c0$msg, fixed = TRUE))
  expect_true(grepl("en-têtes", c0$msg, fixed = TRUE))
  # Aucune feuille : pas d'erreur
  expect_false(hstat_excel_compat(list())$identiques)
})

test_that("les feuilles s'empilent avec leur origine, exploitable en analyse", {
  skip_if_not_installed("writexl")
  f <- .hstat_classeur_essai()
  on.exit(unlink(f), add = TRUE)
  r <- hstat_excel_read_sheets(f, c("2023", "2024"))
  m <- hstat_merge_frames(r$frames, type = "rows", add_source = TRUE,
                          source_names = r$names, source_col = "annee",
                          source_mode = "number")
  expect_true(m$ok)
  expect_equal(nrow(m$data), 6L)
  expect_true("annee" %in% names(m$data))
  # « Nombre extrait » doit donner une vraie variable numerique d'annee, sinon
  # elle ne servirait a rien dans une analyse.
  expect_true(is.numeric(m$data$annee))
  expect_setequal(unique(m$data$annee), c(2023, 2024))

  # En mode « nom », la colonne reste le libelle de la feuille
  m2 <- hstat_merge_frames(r$frames, type = "rows", add_source = TRUE,
                           source_names = r$names, source_col = "feuille",
                           source_mode = "name")
  expect_setequal(unique(m2$data$feuille), c("2023", "2024"))
})

test_that("des feuilles de structures differentes se joignent par une cle", {
  skip_if_not_installed("writexl")
  f <- .hstat_classeur_essai()
  on.exit(unlink(f), add = TRUE)
  r <- hstat_excel_read_sheets(f, c("2023", "Referentiel"))
  j <- hstat_merge_frames(r$frames, type = "inner", key_left = "site",
                          source_names = r$names)
  expect_true(j$ok)
  expect_true("region" %in% names(j$data))
  expect_equal(nrow(j$data), 3L)          # les 3 lignes de 2023 ont un site connu
  # Chaque ligne recoit bien la region de son site
  expect_equal(j$data$region[j$data$site == "A"][1], "Nord")
})


# ===========================================================================
# BILINGUE (francais / anglais)
# ---------------------------------------------------------------------------
# La cle est la chaine FRANCAISE elle-meme. Consequence voulue : une chaine
# absente du dictionnaire reste en francais au lieu d'afficher un identifiant
# technique. Une traduction incomplete degrade doucement, elle ne casse rien.
# ===========================================================================

test_that("le dictionnaire se charge et rejette ce qui ne sert a rien", {
  d <- hstat_i18n_load()
  expect_true(is.data.frame(d))
  expect_equal(names(d), c("fr", "en"))
  expect_gt(nrow(d), 100L)
  # Aucune entree vide, aucune traduction identique a la source, aucun doublon
  expect_true(all(nzchar(d$fr)) && all(nzchar(d$en)))
  expect_false(any(duplicated(d$fr)))
  # « Exploration » se dit de la meme facon dans les deux langues : l'entree
  # est une DECISION de traduction et compte dans la couverture, mais elle
  # n'est pas envoyee au navigateur ou elle ne ferait rien.
  identiques <- d$fr[d$fr == d$en]
  expect_gt(length(identiques), 0L)
  j <- hstat_i18n_json("en")
  for (x in utils::head(identiques, 5))
    expect_false(grepl(sprintf('"%s":', x), j, fixed = TRUE), info = x)

  # Fichier absent ou illisible : dictionnaire vide, jamais une erreur — le
  # bilingue est un confort, son absence ne doit pas empecher de demarrer.
  vide <- hstat_i18n_load(path = NA_character_, force = TRUE)
  expect_equal(nrow(vide), 0L)
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  writeLines(c("colonne1,colonne2", "a,b"), f)
  expect_equal(nrow(hstat_i18n_load(f, force = TRUE)), 0L)
})

test_that("tr() traduit, et laisse le francais quand il ne sait pas", {
  expect_equal(tr("Tests statistiques", "en"), "Statistical tests")
  expect_equal(tr("Chargement", "en"), "Loading")
  # La regle de degradation douce : inconnu -> inchange
  expect_equal(tr("Chaine absente du dictionnaire", "en"),
               "Chaine absente du dictionnaire")
  # En francais, rien n'est touche
  expect_equal(tr("Tests statistiques", "fr"), "Tests statistiques")
  # Vectorise : chaque element est traite independamment
  expect_equal(tr(c("Charger", "Inconnu", "Resultat inconnu"), "en"),
               c("Load", "Inconnu", "Resultat inconnu"))
  # Entrees degenerees
  expect_null(tr(NULL, "en"))
  expect_equal(tr(character(0), "en"), character(0))
  expect_equal(tr(NA_character_, "en"), NA_character_)
})

test_that("le dictionnaire envoye au navigateur est un JSON valide", {
  j <- hstat_i18n_json("en")
  expect_true(startsWith(j, "{") && endsWith(j, "}"))
  expect_gt(nchar(j), 1000L)
  # Le francais n'embarque rien : la page reste legere par defaut
  expect_equal(hstat_i18n_json("fr"), "{}")

  # Les guillemets et antislashs doivent etre echappes, sinon le JSON casse et
  # la page entiere perd son script.
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  utils::write.csv(data.frame(
    fr = c('Dire "oui"', "Chemin C:\\dossier", "Sur\ndeux lignes"),
    en = c('Say "yes"', "Path C:\\folder", "On\ntwo lines"),
    stringsAsFactors = FALSE), f, row.names = FALSE, fileEncoding = "UTF-8")
  j2 <- hstat_i18n_json("en", f)
  expect_false(grepl('[^\\\\]"oui"', j2))          # le guillemet est echappe
  expect_true(grepl('\\\\"oui\\\\"', j2))
  expect_true(grepl("\\\\\\\\dossier", j2))        # l'antislash aussi
  expect_false(grepl("\n", j2, fixed = TRUE))      # plus de saut de ligne brut
  skip_if_not_installed("jsonlite")
  expect_silent(jsonlite::fromJSON(j2))
  expect_equal(unname(jsonlite::fromJSON(j2)[["Dire \"oui\""]]), "Say \"yes\"")
})

test_that("la couverture se mesure et nomme ce qui manque", {
  cv <- hstat_i18n_coverage(c("Chargement", "Tests statistiques", "Zzz inconnu"))
  expect_equal(cv$total, 3L)
  expect_equal(cv$traduites, 2L)
  expect_equal(cv$manquantes, "Zzz inconnu")
  expect_equal(cv$taux, 2/3)
  # Aucune chaine : pas de division par zero
  expect_equal(hstat_i18n_coverage(character(0))$taux, 1)
})

test_that("la navigation entiere est traduite", {
  # Le menu lateral est ce que l'utilisateur voit en permanence : s'il reste en
  # francais, l'application ne parait pas bilingue quoi qu'on traduise ailleurs.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  ux <- readLines(file.path(root, "inst", "app", "UX.R"), warn = FALSE,
                  encoding = "UTF-8")
  menus <- grep("menuItem\\(", ux, value = TRUE)
  libelles <- unlist(lapply(menus, function(l) {
    m <- regmatches(l, regexpr('"[^"]+"', l))
    if (length(m)) gsub('"', "", m) else NULL
  }))
  expect_gt(length(libelles), 10L)
  cv <- hstat_i18n_coverage(libelles)
  expect_equal(cv$manquantes, character(0),
               info = paste("Entrees de menu non traduites :",
                            paste(cv$manquantes, collapse = ", ")))
})

test_that("la bascule cote navigateur est presente et branchee", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  js <- file.path(root, "inst", "app", "www", "hstat-i18n.js")
  expect_true(file.exists(js))
  src <- paste(readLines(js, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Le contenu rendu APRES la bascule (notifications, tableaux, sorties) doit
  # etre rattrape : sans observateur, seule l'interface initiale serait traduite.
  expect_true(grepl("MutationObserver", src, fixed = TRUE))
  # Le retour au francais restitue le texte d'origine plutot que de retraduire
  # en sens inverse, ce qui perdrait accents et doublons de sens.
  expect_true(grepl("__hstatFr", src, fixed = TRUE))
  # Le choix survit a un rechargement
  expect_true(grepl("localStorage", src, fixed = TRUE))
  # Les donnees de l'utilisateur sont protegeables
  expect_true(grepl("data-hstat-notranslate", src, fixed = TRUE))

  ux <- paste(readLines(file.path(root, "inst", "app", "UX.R"), warn = FALSE,
                        encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("hstat-i18n.js", ux, fixed = TRUE))
  expect_true(grepl("hstatLangEn", ux, fixed = TRUE))

  # Les deux assertions qui suivent portent sur le CODE, commentaires
  # retires. Ecrites sur le texte brut elles se signalaient elles-memes :
  # le commentaire qui explique l'ordre nomme forcement les deux balises,
  # et il est place AVANT la ligne qu'il documente. Un balayage qui crie au
  # loup finit desactive -- la meme raison qui fait passer les autres par
  # l'analyseur.
  code <- paste(.hstat_code_lignes(file.path(root, "inst", "app", "UX.R")),
                collapse = "\n")
  # Le dictionnaire passe par le socle, jamais recopie dans UX.R : deux
  # facons de le poser finiraient par diverger.
  expect_true(grepl("hstat_i18n_script(", code, fixed = TRUE))
  expect_false(grepl("window.HSTAT_I18N", code, fixed = TRUE))
  # ET IL DOIT PRECEDER hstat-i18n.js. La raison a change avec le chargement
  # a la demande, l'ordre non : la balise ne porte plus le dictionnaire mais
  # son ADRESSE (`window.HSTAT_I18N_SRC`), et le traducteur la lit des son
  # chargement. Posee apres, l'adresse serait la et le traducteur n'aurait
  # rien a aller chercher : la bascule resterait sans effet, sans qu'aucune
  # erreur ne le dise.
  expect_lt(regexpr("hstat_i18n_script(", code, fixed = TRUE),
            regexpr("hstat-i18n.js", code, fixed = TRUE))
})


# ===========================================================================
# LE TRADUCTEUR NE DOIT PAS TOUCHER AUX DONNEES DE L'UTILISATEUR
# ---------------------------------------------------------------------------
# Constate a l'ecran, et c'est le pire defaut possible pour un outil
# statistique : une colonne valant « Oui »/« Non » dans le fichier charge
# s'affichait « Yes »/« No » une fois l'anglais choisi. L'application
# reecrivait les donnees que l'utilisateur etait venu lire.
#
# Second defaut du meme passage : le remplacement se faisait avec une CHAINE,
# or String.replace interprete « $& » et « $\u0060 » comme des references au
# texte trouve. Une traduction contenant ces suites ressortait corrompue.
#
# Les deux se prouvent en EXECUTANT le traducteur, pas en cherchant une chaine
# dans le fichier : un test textuel passerait encore si le code changeait de
# forme en gardant le defaut. Le banc d'essai ci-dessous fournit le minimum de
# DOM necessaire (arbre de noeuds, TreeWalker, attributs) et a ete verifie
# comme ECHOUANT sur la version d'avant correction.
# ===========================================================================

.hstat_node <- function() {
  for (cmd in c("node", "nodejs", "/opt/node22/bin/node")) {
    ok <- tryCatch(system2(cmd, "--version", stdout = TRUE, stderr = TRUE),
                   error = function(e) NULL, warning = function(w) NULL)
    if (!is.null(ok) && length(ok)) return(cmd)
  }
  NA_character_
}

test_that("la bascule laisse intactes les donnees de l'utilisateur", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  node <- .hstat_node()
  skip_if(is.na(node), "node absent : le traducteur ne peut pas etre execute")

  banc <- tempfile(fileext = ".js")
  writeLines(r"---(// Banc d'essai minimal : assez de DOM pour executer hstat-i18n.js pour de vrai.
var fs = require("fs"), vm = require("vm");
var SRC = process.argv[2];

function El(tag) {
  return { nodeType: 1, tagName: tag, childNodes: [], parentNode: null, A: {},
           hasAttribute: function (n) { return this.A[n] !== undefined; },
           getAttribute: function (n) { return this.A[n]; },
           setAttribute: function (n, v) { this.A[n] = v; },
           classList: { add: function () {}, remove: function () {} } };
}
function Txt(v) { return { nodeType: 3, nodeValue: v, parentNode: null }; }
function add(p, c) { c.parentNode = p; p.childNodes.push(c); return c; }
function tous(n, out) {
  out = out || [];
  (n.childNodes || []).forEach(function (c) { out.push(c); tous(c, out); });
  return out;
}
function equiper(el) {
  el.querySelectorAll = function () {
    return tous(this).filter(function (n) { return n.nodeType === 1; });
  };
  return el;
}

var html = equiper(El("HTML"));
var body = equiper(El("BODY"));
add(html, body);

// --- interface : un libelle de menu hors tableau
var menu = add(body, equiper(El("SPAN")));
var tMenu = add(menu, Txt("Chargement"));

// --- un tableau : en-tete (libelle) + cellules (DONNEES DE L'UTILISATEUR)
var table = add(body, equiper(El("TABLE")));
var th = add(table, equiper(El("TH")));
var tTh = add(th, Txt("Chargement"));
// en-tete portant un NOM DE COLONNE du fichier de l'utilisateur
var thCol = add(table, equiper(El("TH")));
var tThCol = add(thCol, Txt("Total"));
// libelle d'interface identique, hors tableau
var spTot = add(body, equiper(El("SPAN")));
var tTot = add(spTot, Txt("Total"));
var td = add(table, equiper(El("TD")));
var tTd = add(td, Txt("Oui"));
var tdLong = add(table, equiper(El("TD")));
var tLong = add(tdLong, Txt("Effectifs attendus >= 5 dans au moins 80 % des cases."));

// --- le piege du remplacement : une traduction contenant « $& »
var tdDollar = add(body, equiper(El("SPAN")));
var tDollar = add(tdDollar, Txt("Prix"));

// --- une indication telle que SHINY LA SERIALISE : un seul noeud de texte,
// mais chaque argument sur sa propre ligne, avec l'indentation du serialiseur.
var aide = add(body, equiper(El("SPAN")));
var tAide = add(aide, Txt("\n  Une indication coupee en trois\n   morceaux par le\n   serialiseur.\n"));

// --- un exemple de saisie porte par un attribut, avec de VRAIS retours a la
// ligne : c'est la forme normale d'un `placeholder` de zone de texte.
var zone = add(body, equiper(El("TEXTAREA")));
zone.setAttribute("placeholder", "Exemples :\n1 a 10\n1,3,5,10 a 15");

var ctx = {
  console: console, setTimeout: function () { return 0; }, clearTimeout: function () {},
  localStorage: { getItem: function () { return null; }, setItem: function () {} },
  NodeFilter: { SHOW_TEXT: 4 },
  document: {
    readyState: "complete", body: body, documentElement: html,
    getElementById: function () { return null; },
    addEventListener: function () {},
    createTreeWalker: function (racine) {
      var l = tous(racine).filter(function (n) { return n.nodeType === 3; }), i = -1;
      return { nextNode: function () { return ++i < l.length ? l[i] : null; } };
    }
  }
};
ctx.window = ctx;
ctx.window.HSTAT_I18N = {
  "Chargement": "Loading", "Oui": "Yes",
  "Effectifs attendus >= 5 dans au moins 80 % des cases.": "Expected counts >= 5 in at least 80% of cells.",
  "Prix": "$& remplace", "Total": "Total (translated)",
  // La cle est PROPRE : c'est la phrase que l'utilisateur lit, pas ce que le
  // serialiseur a ecrit.
  "Une indication coupee en trois morceaux par le serialiseur.":
    "A hint split into three pieces by the serialiser.",
  // La cle porte ses retours a la ligne, la traduction aussi.
  "Exemples :\n1 a 10\n1,3,5,10 a 15": "Examples:\n1 to 10\n1,3,5,10 to 15"
};
// Termes venant du fichier charge : le serveur les annonce au traducteur.
if (process.argv[3] === "avec-termes") {
  ctx.window.__termes = ["Total", "Oui"];
}
vm.createContext(ctx);
// Shiny simule : juste ce qu'il faut pour recevoir la liste des termes.
var handlers = {};
ctx.window.Shiny = {
  addCustomMessageHandler: function (n, f) { handlers[n] = f; },
  setInputValue: function () {}
};
vm.runInContext(fs.readFileSync(SRC, "utf8"), ctx);
if (ctx.window.__termes && handlers["hstat-termes-donnees"])
  handlers["hstat-termes-donnees"](JSON.stringify(ctx.window.__termes));

ctx.window.hstatSetLangue("en");
var apres = {
  menu: tMenu.nodeValue, th: tTh.nodeValue, cellule: tTd.nodeValue,
  th_colonne: tThCol.nodeValue, libelle_total: tTot.nodeValue,
  cellule_longue: tLong.nodeValue, dollar: tDollar.nodeValue,
  aide: tAide.nodeValue, exemple: zone.getAttribute("placeholder")
};
ctx.window.hstatSetLangue("fr");
apres.retour_menu = tMenu.nodeValue;
apres.retour_cellule = tTd.nodeValue;
apres.retour_dollar = tDollar.nodeValue;
apres.retour_aide = tAide.nodeValue;
apres.retour_exemple = zone.getAttribute("placeholder");
process.stdout.write(JSON.stringify(apres));
)---", banc, useBytes = TRUE)
  js <- file.path(root, "inst", "app", "www", "hstat-i18n.js")

  lancer <- function(...) {
    s <- suppressWarnings(system2(node, c(shQuote(banc), shQuote(js), ...),
                                  stdout = TRUE, stderr = TRUE))
    if (!length(s)) return(NULL)
    tryCatch(jsonlite::fromJSON(paste(s, collapse = "")), error = function(e) NULL)
  }
  res <- lancer()
  skip_if(is.null(res), "le banc d'essai n'a rien produit d'exploitable")
  # Deuxieme passe : le serveur a annonce les termes du fichier de
  # l'utilisateur (« Total » est ici un NOM DE COLONNE).
  avec <- lancer("avec-termes")
  skip_if(is.null(avec), "le banc d'essai n'a rien produit d'exploitable")

  # 1. UNE VALEUR DE DONNEES DANS UNE CELLULE RESTE CE QU'ELLE EST.
  expect_equal(res$cellule, "Oui")
  # 2. Un libelle d'interface hors tableau se traduit.
  expect_equal(res$menu, "Loading")
  # 3. Un EN-TETE est un libelle, pas une donnee : il se traduit.
  expect_equal(res$th, "Loading")
  # 4. Une interpretation, elle, tient dans une cellule et doit passer : une
  #    valeur de donnees n'est presque jamais une phrase entiere.
  expect_equal(res$cellule_longue,
               "Expected counts >= 5 in at least 80% of cells.")
  # 5. « $& » dans une traduction sort tel quel, il n'est pas interprete.
  expect_equal(res$dollar, "$& remplace")
  # 6. Le retour au francais restitue le texte d'origine, exactement.
  expect_equal(res$retour_menu, "Chargement")
  expect_equal(res$retour_cellule, "Oui")
  expect_equal(res$retour_dollar, "Prix")

  # 7. LE TROU QUE LA REGLE DE LONGUEUR LAISSAIT OUVERT. Un <th> est un
  #    libelle... sauf quand c'est le NOM D'UNE COLONNE du fichier charge.
  #    Sans la liste des termes, il etait traduit.
  expect_equal(res$th_colonne, "Total (translated)")
  # 8. Avec la liste, le nom de colonne est intact — et le libelle d'interface
  #    homonyme cesse d'etre traduit lui aussi : c'est le prix assume, la
  #    degradation douce. Alterer une donnee n'en serait pas une.
  expect_equal(avec$th_colonne, "Total")
  expect_equal(avec$libelle_total, "Total")
  # 10. UNE INDICATION SERIALISEE PAR SHINY SE TRADUIT. Shiny ecrit chaque
  #     argument d'une balise sur sa propre ligne : `helpText("a", " b")` rend
  #     UN noeud de texte, mais dont la valeur porte retours a la ligne et
  #     indentation. Aucune cle propre ne pouvait correspondre, et les
  #     quarante-neuf indications sous les controles restaient en francais quoi
  #     qu'on mette au dictionnaire. On cherche donc sur les blancs normalises
  #     -- ce que le navigateur AFFICHE, pas ce que le serialiseur a ecrit.
  expect_equal(trimws(res$aide),
               "A hint split into three pieces by the serialiser.")
  #     Les blancs de BORDURE sont conserves : les retirer collerait le texte
  #     a l'element voisin.
  expect_true(startsWith(res$aide, "\n  "))
  #     Et le retour au francais restitue la valeur d'origine, blancs compris.
  expect_equal(res$retour_aide,
               "\n  Une indication coupee en trois\n   morceaux par le\n   serialiseur.\n")
  # 11. UN ATTRIBUT PORTE DE VRAIS RETOURS A LA LIGNE. Le `placeholder` d'une
  #     zone de texte montre une saisie sur plusieurs lignes ; la valeur brute
  #     ne collait a aucune cle, et ces exemples restaient en francais dans la
  #     version anglaise. La recherche passe par les blancs normalises, mais la
  #     valeur POSEE est la traduction telle quelle -- avec ses propres retours
  #     a la ligne, sans quoi l'exemple tiendrait sur une seule ligne.
  expect_equal(res$exemple, "Examples:\n1 to 10\n1,3,5,10 to 15")
  expect_equal(res$retour_exemple, "Exemples :\n1 a 10\n1,3,5,10 a 15")

  # 9. Le reste de l'interface continue de se traduire normalement.
  expect_equal(avec$menu, "Loading")
  expect_equal(avec$cellule, "Oui")
  expect_equal(avec$dollar, "$& remplace")
})

# -- UNE COLONNE NOMMEE « constructor » RESTE « constructor » -----------------
# `DICT` sort de JSON.parse : c'est un objet ORDINAIRE, donc porteur
# d'Object.prototype. `DICT["constructor"]` y rend la fonction Object, qui
# n'est pas `undefined` -- le noeud de texte etait alors remplace par
# « function Object() { [native code] } ». Mesure avant correction, dictionnaire
# reduit a {"Rendement": "Yield"} : les CINQ noms ci-dessous ressortaient en
# code JavaScript, dans un en-tete de tableau comme dans un `placeholder`.
#
# C'est le defaut que ce depot tient pour le pire : l'application reecrivait
# les donnees que l'utilisateur etait venu lire. Les deux protections en place
# ne le couvrent pas -- la liste des termes de donnees est BORNEE
# (max_termes 3000, max_modalites 200), et la regle de longueur en cellule ne
# protege qu'un <td>, jamais un <th> ni un attribut.
#
# Le test EXECUTE le traducteur sous node plutot que de chercher une chaine
# dans le fichier : un test textuel passerait encore si le code changeait de
# forme en gardant le defaut. Et il verifie les DEUX cotes -- les noms rendus
# intacts, ET une traduction ordinaire qui marche toujours : une fonction qui
# ne traduirait plus rien du tout passerait la premiere moitie.
test_that("un nom de methode d'Object n'est pas pris pour une traduction", {
  node <- .hstat_node()
  skip_if(is.na(node), "node absent : le traducteur ne peut pas etre execute")
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  skip_if_not_installed("jsonlite")

  banc <- tempfile(fileext = ".js")
  writeLines(r"---(var fs = require("fs"), vm = require("vm");
var SRC = process.argv[2];
function El(t) {
  return { nodeType: 1, tagName: t, childNodes: [], parentNode: null, A: {},
           hasAttribute: function (n) { return this.A[n] !== undefined; },
           getAttribute: function (n) { return this.A[n]; },
           setAttribute: function (n, v) { this.A[n] = v; },
           classList: { add: function () {}, remove: function () {} } };
}
function Txt(v) { return { nodeType: 3, nodeValue: v, parentNode: null }; }
function add(p, c) { c.parentNode = p; p.childNodes.push(c); return c; }
function tous(n, o) { o = o || [];
  (n.childNodes || []).forEach(function (c) { o.push(c); tous(c, o); }); return o; }
function eq(e) { e.querySelectorAll = function () {
  return tous(this).filter(function (n) { return n.nodeType === 1; }); }; return e; }

var html = eq(El("HTML")), body = eq(El("BODY")); add(html, body);
// Des EN-TETES de tableau portant des noms de colonnes du fichier. Un <th> est
// un libelle : ni la regle de longueur en cellule ni la liste des termes ne le
// protegent ici.
var table = eq(add(body, eq(El("TABLE"))));
var noms = ["constructor", "toString", "valueOf", "hasOwnProperty",
            "isPrototypeOf", "Rendement"];
var txt = {};
noms.forEach(function (n) { var e = add(table, eq(El("TH"))); txt[n] = add(e, Txt(n)); });
// Et un attribut, l'autre chemin de lecture du dictionnaire.
var zone = add(body, eq(El("TEXTAREA")));
zone.setAttribute("placeholder", "toString");

var ctx = {
  console: console, setTimeout: function () { return 0; }, clearTimeout: function () {},
  localStorage: { getItem: function () { return null; }, setItem: function () {} },
  NodeFilter: { SHOW_TEXT: 4 },
  document: {
    readyState: "complete", body: body, documentElement: html,
    getElementById: function () { return null; },
    addEventListener: function () {},
    createTreeWalker: function (racine) {
      var l = tous(racine).filter(function (n) { return n.nodeType === 3; }), i = -1;
      return { nextNode: function () { return ++i < l.length ? l[i] : null; } };
    }
  }
};
ctx.window = ctx;
// Le dictionnaire ne contient QUE « Rendement » : tout autre remplacement ne
// peut venir que de la chaine de prototypes.
ctx.window.HSTAT_I18N = { "Rendement": "Yield" };
vm.createContext(ctx);
ctx.window.Shiny = { addCustomMessageHandler: function () {}, setInputValue: function () {} };
vm.runInContext(fs.readFileSync(SRC, "utf8"), ctx);
ctx.window.hstatSetLangue("en");
var out = {};
noms.forEach(function (n) { out[n] = String(txt[n].nodeValue); });
out.attribut = String(zone.getAttribute("placeholder"));
process.stdout.write(JSON.stringify(out));
)---", banc, useBytes = TRUE)

  js <- file.path(root, "inst", "app", "www", "hstat-i18n.js")
  sortie <- suppressWarnings(system2(node, c(shQuote(banc), shQuote(js)),
                                     stdout = TRUE, stderr = TRUE))
  res <- tryCatch(jsonlite::fromJSON(paste(sortie, collapse = "")),
                  error = function(e) NULL)
  skip_if(is.null(res), "le banc d'essai n'a rien produit d'exploitable")

  # 1. Les noms de methodes d'Object ressortent INTACTS, en texte comme en
  #    attribut. Avant correction : « function Object() { [native code] } ».
  for (n in c("constructor", "toString", "valueOf", "hasOwnProperty",
              "isPrototypeOf"))
    expect_equal(res[[n]], n, info = n)
  expect_equal(res$attribut, "toString")

  # 2. ET LA TRADUCTION MARCHE TOUJOURS. Sans cette moitie, un traducteur qui
  #    ne remplacerait plus rien passerait le test.
  expect_equal(res$Rendement, "Yield")
})

# -- UNE COULEUR DE CODE N'EST PAS UNE CHAINE LIBRE ---------------------------
# Le surligneur du corpus monte son `<mark>` par `sprintf` et le rend par
# `HTML()` : ce qui y entre n'est plus echappe par personne. Le texte du
# document passait bien par `.hstat_code_esc()` -- la COULEUR y allait brute.
# Une couleur valant `#ff0000" onmouseover="alert(1)` sortait de l'attribut
# `style` et deposait un gestionnaire d'evenement vivant sur l'element.
#
# Le vecteur realiste n'est pas la saisie, c'est le PROJET RECHARGE : un livre
# de codes arrive par `.rds` televerse, ou la couleur est une donnee comme une
# autre. On ouvre le projet d'un collegue, et le balisage part avec.
#
# Le test verifie les DEUX cotes -- l'injection refusee ET une couleur
# legitime toujours rendue : une fonction qui remplacerait TOUTE couleur par
# le gris passerait la premiere moitie sans rien garder.
test_that("une couleur de code n'echappe pas de l'attribut style", {
  cb <- hstat_code_add(hstat_code_new_codebook(), "Prix")
  sg <- hstat_seg_add(hstat_code_new_segments(), "d1", cb$code_id[1], 0L, 4L, "t")

  for (hostile in c("#ff0000\" onmouseover=\"alert(1)",
                    "red;\"></mark><img src=x onerror=alert(1)>",
                    "</style><script>alert(1)</script>")) {
    cb$color[1] <- hostile
    h <- hstat_code_highlight_html("trop cher pour ce que c'est", sg, cb)
    expect_false(grepl("onmouseover", h, fixed = TRUE), info = hostile)
    expect_false(grepl("<img", h, fixed = TRUE), info = hostile)
    expect_false(grepl("<script", h, fixed = TRUE), info = hostile)
    # Le `<mark>` reste bien forme : on ne casse pas l'affichage pour se
    # proteger, on remplace la couleur par le repli.
    expect_true(grepl("border-bottom:2px solid #cccccc;", h, fixed = TRUE),
                info = hostile)
  }

  # ET UNE COULEUR LEGITIME PASSE TOUJOURS, avec ou sans croisillon.
  for (bonne in c("#e74c3c", "e74c3c")) {
    cb$color[1] <- bonne
    h <- hstat_code_highlight_html("trop cher", sg, cb)
    expect_true(grepl("border-bottom:2px solid #e74c3c;", h, fixed = TRUE),
                info = bonne)
  }

  # Le texte du document, lui, etait deja echappe : on le garde sous garde.
  cb$color[1] <- "#e74c3c"
  expect_false(grepl("<script",
                     hstat_code_highlight_html("<script>x</script>", sg, cb),
                     fixed = TRUE))

  # L'aide se lit seule, dans les deux sens.
  expect_equal(.hstat_code_hex("#e74c3c"), "#e74c3c")
  expect_equal(.hstat_code_hex("e74c3c"), "#e74c3c")
  expect_equal(.hstat_code_hex("rouge"), "#cccccc")
  expect_equal(.hstat_code_hex(NA), "#cccccc")
  # Et la conversion en rgba, qui la traverse desormais, n'a pas change de
  # resultat sur une couleur valide -- le croisillon est bien retire.
  expect_equal(.hstat_code_rgba("#e74c3c", 0.35), "rgba(231,76,60,0.35)")
  expect_equal(.hstat_code_rgba("e74c3c", 0.35), "rgba(231,76,60,0.35)")
})

# -- L'EXTENSION D'UN EXPORT PASSE PAR LE NORMALISEUR -------------------------
# `hstat_img_fmt()` normalise le format (jpg -> jpeg, html -> png) ET sert a
# composer le NOM du fichier : c'est son extension que Shiny traduit en type
# MIME. Le contenu, lui, est deja normalise par l'ecrivain commun -- ce sont
# donc les deux qui doivent se rejoindre, sinon l'extension annonce un format
# que le fichier ne porte pas.
#
# Dix-neuf exports sur vingt et un le faisaient ; deux lisaient `input$...`
# BRUT. Le balayage passe par l'analyseur et ne regarde que les entrees dont le
# nom parle de format -- un axe numerique ou un nom de tableau n'a rien a
# normaliser, et un balayage qui crie au loup finit desactive.
test_that("aucun nom de fichier ne compose une extension de format non normalisee", {
  fichiers <- .hstat_sources_app()
  skip_if(!length(fichiers), "sources indisponibles")

  symboles <- function(e) {
    if (is.name(e)) return(as.character(e))
    if (is.call(e) || is.pairlist(e) || is.expression(e))
      return(unlist(lapply(as.list(e), symboles)))
    character(0)
  }
  # Les entrees LUES dans l'expression, avec leur nom : `input$xFormat`.
  entrees <- function(e) {
    out <- character(0)
    rec <- function(n) {
      if (!is.call(n)) return(invisible())
      if (identical(paste(deparse(n[[1]]), collapse = ""), "$") &&
          length(n) == 3 && identical(as.character(n[[2]])[1], "input"))
        out <<- c(out, as.character(n[[3]])[1])
      for (i in seq_along(n)[-1]) {
        a <- tryCatch(n[[i]], error = function(z) NULL)
        if (missing(a)) next
        if (!is.null(a) && !identical(a, quote(expr = ))) rec(a)
      }
    }
    rec(e); out
  }

  fautifs <- character(0)
  for (f in fichiers) {
    p <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(p)) next
    visite <- function(e) {
      if (!is.call(e)) return(invisible())
      fn <- paste(deparse(e[[1]]), collapse = "")
      if (grepl("(^|::)downloadHandler$", fn)) {
        a <- as.list(e)[-1]
        fa <- if (!is.null(a$filename)) a$filename else if (length(a)) a[[1]] else NULL
        if (!is.null(fa)) {
          # Une entree qui parle de format doit traverser le normaliseur.
          fmt <- grep("(?i)(format|fmt)$", entrees(fa), value = TRUE, perl = TRUE)
          if (length(fmt) && !("hstat_img_fmt" %in% symboles(fa)))
            fautifs <<- c(fautifs,
                          paste0(basename(f), " : input$", paste(fmt, collapse = ", input$")))
        }
      }
      for (i in seq_along(e)[-1]) {
        a <- tryCatch(e[[i]], error = function(z) NULL)
        if (missing(a)) next
        if (!is.null(a) && !identical(a, quote(expr = ))) visite(a)
      }
    }
    for (i in seq_along(p)) visite(p[[i]])
  }
  expect_identical(unique(fautifs), character(0))
})

test_that("une classe hstat-* posee dans le code est definie dans le style", {
  # LE DEFAUT QUE CE TEST GARDE, constate a l'audit. La classe de l'encadre
  # d'interpretation etait posee sur DIX-SEPT divs de mod_tests.R et definie
  # NULLE PART. Le code construisait donc un encadre qui ne s'affichait pas :
  # l'interpretation sortait au fil du texte, sans rien qui la distingue des
  # resultats bruts qu'elle commente. Rien ne casse, rien ne previent -- c'est
  # exactement le genre de defaut qu'une relecture ne voit pas.
  #
  # Le controle ne porte QUE sur le prefixe du projet : « btn », « box »,
  # « col-md-6 » viennent de Bootstrap et de shinydashboard, les exiger dans
  # notre feuille de style ferait echouer le test sur du code parfaitement sain.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  css_f <- file.path(root, "inst", "app", "www", "hstat-theme.css")
  skip_if(!file.exists(css_f))

  src <- unlist(lapply(.hstat_sources_app(), readLines, warn = FALSE))
  m <- regmatches(src, gregexpr("class\\s*=\\s*[\"'][^\"']*[\"']", src))
  cls <- unlist(lapply(unlist(m), function(x)
    strsplit(trimws(gsub("^class\\s*=\\s*[\"']|[\"']$", "", x)), "\\s+")[[1]]))
  cls <- unique(cls[grepl("^hstat-", cls)])
  skip_if(!length(cls))

  css <- paste(readLines(css_f, warn = FALSE), collapse = "\n")
  # Les modules qui portent leur propre bloc de style (mod_coding) comptent
  # aussi : la definition n'a pas a vivre dans la feuille commune.
  inline <- paste(src[grepl("\\{", src)], collapse = "\n")
  defini <- function(c) {
    motif <- paste0("\\.", c, "([^a-zA-Z0-9_-]|$)")
    grepl(motif, css) || grepl(motif, inline)
  }
  absentes <- cls[!vapply(cls, defini, logical(1))]
  expect_equal(absentes, character(0),
               info = paste("classes sans regle de style :",
                            paste(absentes, collapse = ", ")))
})

test_that("une phrase coupee par une balise se traduit quand meme", {
  # LE DEFAUT QUE CE TEST GARDE. Le traducteur remplace des NOEUDS DE TEXTE.
  # Une phrase mise en forme -- « <b>Toutes fermees</b> — <code>[a ; b]</code> »
  # -- n'existe nulle part comme noeud entier : le DOM la coupe en morceaux.
  # Les 73 entrees du dictionnaire de cette forme, qui sont precisement les
  # encadres d'aide, ne s'appliquaient JAMAIS et restaient en francais.
  #
  # Deux proprietes sont verifiees ici, et la premiere ne se voit pas :
  #  - la cle du dictionnaire est comparee APRES etre passee par l'analyseur du
  #    navigateur, parce que `innerHTML` est renormalise a la lecture. Le banc
  #    ecrit donc la cle avec des guillemets SIMPLES et l'element avec des
  #    guillemets DOUBLES : sans normalisation, aucun des deux ne colle ;
  #  - le retour au francais restitue le balisage d'origine.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  node <- .hstat_node()
  skip_if(is.na(node), "node absent : le traducteur ne peut pas etre execute")

  banc <- tempfile(fileext = ".js")
  writeLines(r"---(var fs = require("fs"), vm = require("vm");
var SRC = process.argv[2];

function Txt(v) { return { nodeType: 3, nodeValue: v, parentNode: null }; }
function add(p, c) { c.parentNode = p; p.childNodes.push(c); return c; }
function tous(n, out) {
  out = out || [];
  (n.childNodes || []).forEach(function (c) { out.push(c); tous(c, out); });
  return out;
}
// Le navigateur RENORMALISE le balisage : minuscules et guillemets doubles.
// Le banc en fait autant, sinon il testerait autre chose que la realite.
function norm(s) {
  return String(s).replace(/<\/?[a-zA-Z][^>]*>/g, function (b) {
    return b.replace(/([a-zA-Z-]+)='([^']*)'/g, '$1="$2"')
            .replace(/^<(\/?)([a-zA-Z][a-zA-Z0-9]*)/,
                     function (m, sl, t) { return "<" + sl + t.toLowerCase(); });
  });
}
function attrs(el) {
  var s = "";
  for (var k in el.A) s += " " + k + '="' + el.A[k] + '"';
  return s;
}
function serialiser(n) {
  var out = "";
  (n.childNodes || []).forEach(function (c) {
    if (c.nodeType === 3) { out += c.nodeValue; return; }
    var t = c.tagName.toLowerCase();
    out += "<" + t + attrs(c) + ">" + serialiser(c) + "</" + t + ">";
  });
  return out;
}
function El(tag) {
  var el = { nodeType: 1, tagName: tag, childNodes: [], parentNode: null, A: {},
             hasAttribute: function (n) { return this.A[n] !== undefined; },
             getAttribute: function (n) { return this.A[n]; },
             setAttribute: function (n, v) { this.A[n] = v; },
             classList: { add: function () {}, remove: function () {} } };
  Object.defineProperty(el, "children", { get: function () {
    return this.childNodes.filter(function (c) { return c.nodeType === 1; }); } });
  Object.defineProperty(el, "innerHTML", {
    get: function () {
      return this.__html !== undefined ? norm(this.__html) : norm(serialiser(this));
    },
    set: function (v) { this.__html = v; this.childNodes = [Txt(String(v))]; }
  });
  el.querySelectorAll = function () {
    return tous(this).filter(function (n) { return n.nodeType === 1; });
  };
  return el;
}

var html = El("HTML"), body = El("BODY");
add(html, body);

// L'encadre d'aide : trois noeuds de texte, dont un dans un <code> que le
// traducteur ignore par principe. Aucun d'eux n'est traduisible seul.
//
// LES RETOURS A LA LIGNE SONT CEUX DE SHINY. `div("a", strong("b"), "c")`
// serialise chaque enfant sur sa propre ligne, avec indentation : le balisage
// reel est truffe de blancs que la cle du CSV, ecrite d'un trait, n'a pas.
var boite = add(body, El("DIV"));
add(boite, Txt("\n  "));
var gras = add(boite, El("B")); gras.A["class"] = "t";
add(gras, Txt("Toutes fermees"));
add(boite, Txt("\n   — \n  "));
var code = add(boite, El("CODE"));
add(code, Txt("[a ; b]"));
add(boite, Txt("\n"));

var ctx = {
  console: console, setTimeout: function () { return 0; }, clearTimeout: function () {},
  localStorage: { getItem: function () { return null; }, setItem: function () {} },
  NodeFilter: { SHOW_TEXT: 4 },
  document: {
    readyState: "complete", body: body, documentElement: html,
    getElementById: function () { return null; },
    addEventListener: function () {},
    createElement: function (t) { return El(t.toUpperCase()); },
    createTreeWalker: function (racine) {
      var l = tous(racine).filter(function (n) { return n.nodeType === 3; }), i = -1;
      return { nextNode: function () { return ++i < l.length ? l[i] : null; } };
    }
  }
};
ctx.window = ctx;
// LA CLE PORTE DES GUILLEMETS SIMPLES, comme dans le CSV et dans le code R.
ctx.window.HSTAT_I18N = {
  "<b class='t'>Toutes fermees</b> — <code>[a ; b]</code>":
    "<b class='t'>All closed</b> — <code>[a, b]</code>",
  "Toutes fermees": "NE DOIT PAS SERVIR"
};
vm.createContext(ctx);
ctx.window.Shiny = { addCustomMessageHandler: function () {}, setInputValue: function () {} };
vm.runInContext(fs.readFileSync(SRC, "utf8"), ctx);

var avant = boite.innerHTML;
ctx.window.hstatSetLangue("en");
var apres = boite.innerHTML;
ctx.window.hstatSetLangue("fr");
process.stdout.write(JSON.stringify(
  { avant: avant, apres: apres, retour: boite.innerHTML }));
)---", banc, useBytes = TRUE)

  js <- file.path(root, "inst", "app", "www", "hstat-i18n.js")
  s <- suppressWarnings(system2(node, c(shQuote(banc), shQuote(js)),
                                stdout = TRUE, stderr = TRUE))
  res <- if (length(s))
    tryCatch(jsonlite::fromJSON(paste(s, collapse = "")), error = function(e) NULL)
  skip_if(is.null(res), "le banc d'essai n'a rien produit d'exploitable")

  # 1. La phrase mise en forme est bien coupee : aucun noeud ne la porte.
  expect_true(grepl("<b", res$avant, fixed = TRUE))
  # 2. ELLE SE TRADUIT MALGRE TOUT, balisage compris.
  expect_true(grepl("All closed", res$apres, fixed = TRUE))
  # 3. Y COMPRIS CE QUI EST DANS <code>, que la passe sur les noeuds de texte
  #    ignore par principe : c'est le remplacement en bloc qui l'emporte.
  expect_true(grepl("[a, b]", res$apres, fixed = TRUE))
  expect_false(grepl("[a ; b]", res$apres, fixed = TRUE))
  # 4. Le retour au francais restitue le balisage d'origine, exactement.
  expect_equal(res$retour, res$avant)
})

test_that("une phrase composee traduit son gabarit, jamais ses arguments", {
  # LES ARGUMENTS PORTENT LES DONNEES DE L'UTILISATEUR : un nom de variable,
  # une modalite, un effectif. Ils traversent la traduction sans etre lus.
  # C'est la meme regle que cote navigateur, obtenue ici PAR CONSTRUCTION
  # plutot que par precaution : trf() ne traduit que l'armature.
  f <- "%s : %d valeur(s) modifiee(s) sur %d colonne(s) partagee(s)."
  fr <- trf("%s : %d valeur(s) modifiée(s) sur %d colonne(s) partagée(s).",
            "Ma_Variable", 3, 2, lang = "fr")
  en <- trf("%s : %d valeur(s) modifiée(s) sur %d colonne(s) partagée(s).",
            "Ma_Variable", 3, 2, lang = "en")
  expect_false(identical(fr, en))                       # le gabarit est traduit
  expect_true(grepl("Ma_Variable", fr, fixed = TRUE))   # l'argument est intact
  expect_true(grepl("Ma_Variable", en, fixed = TRUE))
  expect_true(grepl("3", en, fixed = TRUE))

  # Une valeur de donnees qui coincide mot pour mot avec un libelle
  # d'interface passe elle aussi telle quelle.
  t <- trf("Corrélation : variable(s) non numérique(s) ignorée(s) : %s.",
           "Oui, Non, Total", lang = "en")
  expect_true(grepl("Oui, Non, Total", t, fixed = TRUE))

  # Degradation douce : un gabarit absent du dictionnaire ressort en francais,
  # CORRECTEMENT REMPLI, au lieu de disparaitre ou d'afficher une cle.
  inconnu <- trf("Gabarit inexistant portant %s.", "une valeur", lang = "en")
  expect_equal(inconnu, "Gabarit inexistant portant une valeur.")

  # Une traduction fautive peut avoir perdu un marqueur : sprintf leverait
  # « too few arguments » et ferait tomber toute la sortie pour une erreur de
  # dictionnaire. On retombe sur le francais, qui marche.
  expect_silent(x <- trf("Deux marqueurs %s et %d.", "a", 2, lang = "en"))
  expect_true(grepl("a", x, fixed = TRUE))
})

test_that("les libelles de widgets et les titres d'onglets sont traduits", {
  # C'est la surface que l'utilisateur LIT en premier : les libelles poses sur
  # les widgets et les titres d'onglets. Ce test empeche la couverture de
  # reculer en silence quand un module ajoute un controle.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  fichiers <- .hstat_sources_app()
  widgets <- c("selectInput", "selectizeInput", "textInput", "textAreaInput",
               "numericInput", "radioButtons", "checkboxInput",
               "checkboxGroupInput", "sliderInput", "actionButton",
               "downloadButton", "fileInput", "dateInput", "dateRangeInput")
  lab <- character(0); ong <- character(0)
  for (f in fichiers) {
    src <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    for (w in widgets) {
      p <- sprintf('%s\\(\\s*(?:ns\\()?\\s*"[^"]*"\\)?\\s*,\\s*"([^"]{3,90})"', w)
      m <- regmatches(src, gregexpr(p, src, perl = TRUE))[[1]]
      if (length(m)) lab <- c(lab, sub('.*,\\s*"([^"]*)"$', "\\1", m))
    }
    m <- regmatches(src, gregexpr(
      'tabPanel\\(\\s*(?:shiny::)?(?:tagList\\(\\s*(?:shiny::)?icon\\("[^"]*"\\)\\s*,\\s*)?"([^"]{2,60})"',
      src, perl = TRUE))[[1]]
    if (length(m)) ong <- c(ong, sub('.*"([^"]*)"$', "\\1", m))
    # Troisieme forme, oubliee au premier balayage : `title = tagList(icon(..),
    # " Titre")`. Elle porte 76 titres de boites — le module de nettoyage
    # affichait encore « Supprimer Variable » en anglais.
    m <- regmatches(src, gregexpr(
      'title\\s*=\\s*(?:shiny::)?tagList\\(\\s*(?:shiny::)?icon\\("[^"]*"\\)\\s*,\\s*"\\s?([^"]{2,60})"',
      src, perl = TRUE))[[1]]
    if (length(m)) ong <- c(ong, trimws(sub('.*"\\s?([^"]*)"$', "\\1", m)))
  }
  nettoie <- function(x) {
    x <- unique(trimws(x))
    # Les chaines portant un echappement \\uXXXX litteral dans la source R sont
    # un artefact de l'extraction textuelle : a l'ecran, R affiche le caractere
    # (« α »), pas la sequence. Les compter ferait echouer le test sur une
    # difference qui n'existe pas pour l'utilisateur.
    x[nzchar(x) & !grepl("^[a-z_]+$", x) & !grepl("\\\\u[0-9a-f]{4}", x)]
  }
  lab <- nettoie(lab); ong <- nettoie(ong)

  cv_ong <- hstat_i18n_coverage(ong)
  expect_equal(cv_ong$manquantes, character(0),
               info = paste("Titres d'onglets non traduits :",
                            paste(cv_ong$manquantes, collapse = " | ")))

  cv_lab <- hstat_i18n_coverage(lab)
  expect_equal(cv_lab$manquantes, character(0),
               info = paste("Libelles de widgets non traduits :",
                            paste(utils::head(cv_lab$manquantes, 20), collapse = " | ")))
})

test_that("tr() suit la langue de la session, sans qu'on ait a la lui passer", {
  # LE DEFAUT QUE CE TEST GARDE. `tr()` avait pour defaut `lang = "fr"` : les
  # deux cent cinquante appels `tr("...")` du depot -- pas un ne passe `lang` --
  # rendaient donc le francais QUOI QU'IL ARRIVE. La fonction existait, le
  # dictionnaire la servait, et les tests la couvraient... en lui passant
  # `lang` explicitement, ce qui masquait exactement le defaut. Un defaut qui
  # neutralise sa propre fonction ne se voit qu'a l'usage.
  faux <- list(userData = list(langue = "en"))
  expect_equal(shiny::withReactiveDomain(faux, tr("Chargement")), "Loading")
  # Hors session, ou en francais, rien ne change.
  expect_equal(tr("Chargement"), "Chargement")
  expect_equal(shiny::withReactiveDomain(list(userData = list(langue = "fr")),
                                         tr("Chargement")), "Chargement")
  # Meme regle que `trf()` et `hstat_err_fr()` : la langue vient de la SESSION,
  # jamais d'une option globale -- sur un serveur partage, une option ferait
  # basculer la langue de tous les utilisateurs des que l'un change la sienne.
  expect_identical(deparse(formals(tr)$lang), deparse(formals(trf)$lang))
})

test_that("une alerte qui melange texte et valeurs passe par un gabarit", {
  # LE DEFAUT QUE CE TEST GARDE, constate a l'ecran en anglais.
  #
  # LES ENFANTS TEXTE ADJACENTS NE FONT QU'UN SEUL NOEUD. Le navigateur fond
  # toute suite de caracteres en UN noeud de texte : dans
  # `tagList(icon(...), " Formule incorrecte : le resultat a ", n, " valeur(s)")`
  # le traducteur ne voit pas trois morceaux, il voit
  # « Formule incorrecte : le resultat a 3 valeur(s) » -- une chaine qui depend
  # des donnees et qu'aucune cle ne peut couvrir. Mettre les morceaux au
  # dictionnaire ne sert donc a RIEN : ils n'existent nulle part comme noeud.
  # Seul un gabarit `trf()` traduit ce cas.
  #
  # Une BALISE, elle, coupe le noeud : `tagList(icon(...), " texte fixe")` reste
  # traduisible par le dictionnaire, et c'est la forme la plus courante.
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")
  ALERTE <- c("showNotification", "showModal", "shinyalert", "modalDialog")
  DEJA   <- c("tr", "trf", "hstat_err_fr", "hstat_pkg_manquant")
  BORNE  <- "\u0001"                 # marque une balise : elle coupe le noeud
  frcs <- function(x) grepl("[\u00e0-\u00ff\u00c0-\u00dd]", x) ||
    grepl("\\b(le|la|les|des|une|un|du|de|et|pour|dans|sur|avec|est|sont|pas|vos|vous|aucun|aucune|ce|cette|qui|que|ne|au|aux)\\b",
          tolower(x), perl = TRUE)
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    ex <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(ex)) next
    unite <- function(n) {
      if (is.character(n) && length(n) == 1L) return(n)
      if (!is.call(n)) return("%s")
      nm <- sub("^(shiny|htmltools)::", "", paste(deparse(n[[1]]), collapse = ""))
      if (nm %in% DEJA) return("")            # deja traduit : neutre
      if (nm %in% c("paste", "paste0")) {
        a <- as.list(n)[-1]; nz <- names(a); if (is.null(nz)) nz <- rep("", length(a))
        # `paste` colle avec une espace, `paste0` sans : confondre les deux
        # souderait des mots et ferait passer pour absentes des phrases
        # presentes.
        sep <- if (nm == "paste") {
          j <- which(nz == "sep")
          if (length(j) && is.character(a[[j]])) a[[j]] else " "
        } else ""
        return(paste(vapply(a[nz == ""], unite, character(1)), collapse = sep))
      }
      if (nm == "tagList") {
        a <- as.list(n)[-1]; nz <- names(a); if (is.null(nz)) nz <- rep("", length(a))
        return(paste(vapply(a[nz == ""], unite, character(1)), collapse = ""))
      }
      if (grepl("^(tags[$]|icon$|br$|HTML$|strong$|b$|em$|span$|div$|p$|small$|a$|h[1-6]$|code$|pre$)", nm))
        return(BORNE)
      "%s"
    }
    v <- function(n) {
      if (is.call(n)) {
        nm <- sub("^(shiny|shinyalert)::", "", paste(deparse(n[[1]]), collapse = ""))
        if (nm %in% ALERTE) {
          a <- as.list(n)[-1]; nz <- names(a); if (is.null(nz)) nz <- rep("", length(a))
          for (x in a[nz %in% c("", "ui", "text", "title")])
            for (bout in strsplit(unite(x), BORNE, fixed = TRUE)[[1]]) {
              g <- gsub("\\s+", " ", trimws(bout))
              # ON NE VERIFIE PAS QUE LE GABARIT EST AU DICTIONNAIRE, mais
              # qu'il PASSE PAR `trf()` : `unite()` rend la chaine vide pour un
              # appel deja traduit. Un `paste()` qui reconstitue par hasard une
              # cle existante resterait sinon accepte alors qu'il n'est jamais
              # traduit a l'execution -- le defaut exact qu'on cherche.
              if (nchar(g) >= 6 && frcs(g) && grepl("%s", g, fixed = TRUE))
                fautifs <<- c(fautifs, paste(basename(f), "|", g))
            }
        }
        for (i in seq_along(n)) tryCatch(v(n[[i]]), error = function(e) NULL)
      } else if (is.pairlist(n) || is.expression(n)) {
        for (i in seq_along(n)) tryCatch(v(n[[i]]), error = function(e) NULL)
      }
    }
    for (k in seq_along(ex)) tryCatch(v(ex[[k]]), error = function(e) NULL)
  }
  expect_equal(unique(fautifs), character(0),
               info = paste("Alertes sans gabarit :",
                            paste(utils::head(unique(fautifs), 10), collapse = " || ")))
})

test_that("une syntaxe de plage montree a l'utilisateur est une syntaxe acceptee", {
  # Les exemples affiches sous les champs de selection de lignes sont traduits :
  # « 1 a 10 » devient « 1 to 10 ». Un analyseur qui ne connait que « a »
  # enseignerait alors a l'utilisateur anglophone une syntaxe qu'il refuse --
  # une traduction qui casse la fonctionnalite qu'elle decrit.
  #
  # La fonction est definie DANS le corps du module : on va la chercher dans
  # l'arbre syntaxique plutot que de decouper le fichier au juge.
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")
  extraire <- function(fichier, nom) {
    trouve <- NULL
    v <- function(n) {
      if (!is.null(trouve) || !is.call(n)) return(invisible())
      if (identical(paste(deparse(n[[1]]), collapse = ""), "<-") &&
          is.name(n[[2]]) && identical(as.character(n[[2]]), nom)) {
        trouve <<- n[[3]]; return(invisible())
      }
      for (i in seq_along(n)) tryCatch(v(n[[i]]), error = function(e) NULL)
    }
    for (k in parse(fichier, keep.source = FALSE)) v(k)
    trouve
  }
  for (f in c(file.path("R", "mod_filter.R"), file.path("R", "mod_clean.R"))) {
    chemin <- file.path(root, f)
    skip_if_not(file.exists(chemin), f)
    def <- extraire(chemin, "parseRowSelection")
    expect_false(is.null(def), info = f)
    fn <- eval(def, envir = globalenv())
    expect_equal(sort(fn("1 \u00e0 10", 20)), 1:10, info = f)
    expect_equal(sort(fn("1 to 10", 20)), 1:10, info = f)
    expect_equal(sort(fn("1,3,5,10 to 12", 20)), c(1, 3, 5, 10, 11, 12), info = f)
  }
})

# -- LE COUT NE DEPEND PAS DU NOMBRE DE PLAGES -------------------------------
# `all_rows <- c(all_rows, start:end)` dans une boucle recopie le vecteur
# ENTIER a chaque tour : le cout etait QUADRATIQUE en nombre de plages, alors
# que le resultat est borne par `max_rows`. Et chaque plage est
# individuellement VALIDE -- « 1-200000 » sur un fichier de 200 000 lignes --
# donc aucune garde ne se declenchait. Mesure avant correction : 800 plages,
# soit 7 Ko de saisie, tenaient le processus 237 s pour rendre exactement ce
# qu'UNE plage rend en 0,1 s. Shiny sert toutes les sessions depuis un seul
# processus R : ce gel est celui de tout le monde.
#
# ON MESURE UN COMPTE, JAMAIS UNE DUREE -- la regle que ce depot s'est donnee
# apres trois assertions de temps fausses. Le compte est le nombre d'OCTETS
# ALLOUES, que `Rprofmem` rend et qui ne depend pas de la machine. Il
# discrimine sans ambiguite : mesure sur 20 000 lignes, le rapport entre les
# deux codes passe de 12 (n=20) a 203 (n=400) -- l'un croit avec n, l'autre
# non. Le PIC de memoire, lui, ne distingue rien (rapport 3,3) : le ramasse-
# miettes reprend les copies intermediaires au fur et a mesure. Une assertion
# posee dessus n'aurait rien garde.
test_that("le selecteur de lignes ne paie pas le carre du nombre de plages", {
  skip_if(!capabilities("profmem"),
          "R sans profilage memoire : le compte d'octets est indisponible")
  root <- .hstat_repo_root()
  skip_if(is.na(root))

  extraire <- function(fichier, nom) {
    trouve <- NULL
    v <- function(n) {
      if (!is.call(n)) return(invisible())
      if (identical(paste(deparse(n[[1]]), collapse = ""), "<-") &&
          is.name(n[[2]]) && identical(as.character(n[[2]]), nom)) {
        trouve <<- n[[3]]; return(invisible())
      }
      for (i in seq_along(n)) tryCatch(v(n[[i]]), error = function(e) NULL)
    }
    for (k in parse(fichier, keep.source = FALSE)) v(k)
    trouve
  }
  octets <- function(f, txt, mx) {
    tf <- tempfile(); on.exit(unlink(tf), add = TRUE)
    utils::Rprofmem(tf, threshold = 1000)
    invisible(f(txt, mx))
    utils::Rprofmem(NULL)
    l <- readLines(tf, warn = FALSE)
    sum(as.numeric(sub("^([0-9]+).*", "\\1", l[grepl("^[0-9]+ :", l)])),
        na.rm = TRUE)
  }

  MX <- 20000L
  for (f in c(file.path("R", "mod_filter.R"), file.path("R", "mod_clean.R"))) {
    chemin <- file.path(root, f)
    skip_if_not(file.exists(chemin), f)
    fn <- eval(extraire(chemin, "parseRowSelection"), envir = globalenv())

    peu     <- paste(rep(paste0("1-", MX),  10), collapse = ",")
    beaucoup <- paste(rep(paste0("1-", MX), 100), collapse = ",")

    # Le RESULTAT est le meme quel que soit le nombre de plages : c'est ce qui
    # rend le surcout entierement gratuit.
    expect_equal(fn(peu, MX), seq_len(MX), info = f)
    expect_equal(fn(beaucoup, MX), seq_len(MX), info = f)

    # Dix fois plus de plages ne doit pas couter cent fois plus. On laisse une
    # marge large (20 pour un facteur 10 attendu) : c'est la difference d'ORDRE
    # qu'on garde, pas un chiffre. Avant correction le rapport valait ~50 ici,
    # et il montait avec n.
    a <- octets(fn, peu, MX)
    b <- octets(fn, beaucoup, MX)
    expect_gt(a, 0)
    expect_lt(b, 20 * a, label = paste("octets alloues,", f))
  }
})

test_that("un mot ambigu n'entre pas seul au dictionnaire", {
  # « moyenne » vaut *medium* pour une taille d'effet mais *mean* en
  # statistique. La cle du dictionnaire etant la chaine francaise elle-meme,
  # une entree pour l'un corromprait l'autre — exactement le defaut que la
  # restitution du texte d'origine sert deja a eviter dans l'autre sens.
  # La nuance est donc portee par la PHRASE ENTIERE.
  for (mot in c("moyenne", "grande", "petite", "moyen", "grand", "petit"))
    expect_equal(tr(mot, "en"), mot, info = mot)

  p <- "Taille d'effet : moyenne (repères : 0,1 petite ; 0,3 moyenne ; 0,5 grande)."
  expect_false(identical(tr(p, "en"), p))
  expect_true(grepl("medium", tr(p, "en"), fixed = TRUE))
  expect_true(grepl("medium departure",
                    trf("Taille d'effet w de Cohen = %.3f : écart moyen.", 0.35,
                        lang = "en"), fixed = TRUE))

  # Un mot NON ambigu choisi par le code passe, lui, explicitement par tr() :
  # ce n'est pas une donnee de l'utilisateur.
  expect_false(identical(tr("équiprobables", "en"), "équiprobables"))
})

test_that("chaque gabarit du dictionnaire porte les memes marqueurs dans les deux langues", {
  # Un marqueur perdu ou reordonne ferait lever « too few arguments » a
  # sprintf. trf() retombe alors sur le francais, mais la traduction serait
  # morte en silence : ce test la rend visible.
  d <- hstat_i18n_load()
  # MEME definition que le filtre du dictionnaire (HSTAT_I18N_MARQUEUR) :
  # deux motifs distincts finiraient par diverger, et l'un des deux mentirait.
  mk <- function(s) {
    m <- regmatches(s, gregexpr(HSTAT_I18N_MARQUEUR, s))[[1]]
    m[m != "%%"]
  }
  fautifs <- character(0)
  for (i in seq_len(nrow(d))) {
    a <- mk(d$fr[i]); b <- mk(d$en[i])
    if (!identical(a, b))
      fautifs <- c(fautifs, substr(d$fr[i], 1, 50))
  }
  expect_equal(fautifs, character(0),
               info = paste("Marqueurs divergents :", paste(fautifs, collapse = " | ")))
})

test_that("une chaine bordee d'espaces se traduit, et garde son espacement", {
  # `hstat_i18n_load()` applique `trimws()` a ses cles -- une entree de CSV ne
  # doit pas dependre d'un blanc invisible. Consequence NON VOULUE : toute
  # chaine bordee d'espaces devenait intraduisible, sans un mot. Trente-huit
  # gabarits etaient dans ce cas ; ce sont des fragments assembles par
  # `paste0`, ou l'espace separe deux morceaux et releve de la mise en forme,
  # jamais du texte. Ils restaient en francais au milieu d'une interface
  # anglaise, et le dictionnaire les contenait pourtant.
  d <- c("Aucune donnée" = "No data", "Chargement" = "Loading")
  expect_equal(tr(" Aucune donnée ", "en", d), " No data ")
  expect_equal(tr("Aucune donnée", "en", d), "No data")
  expect_equal(tr("  Chargement", "en", d), "  Loading")
  expect_equal(tr("Chargement\n", "en", d), "Loading\n")
  # L'espacement est RENDU A L'IDENTIQUE, jamais normalise.
  expect_equal(tr("\t Chargement  ", "en", d), "\t Loading  ")
  # Une chaine inconnue ressort intacte, espaces compris.
  expect_equal(tr(" Inconnu ", "en", d), " Inconnu ")
  # Et le francais n'est jamais touche.
  expect_equal(tr(" Aucune donnée ", "fr", d), " Aucune donnée ")

  # `trf()` passe par `tr()` : le gabarit borde d'espaces en beneficie aussi.
  d2 <- c("%d cas prédits (moyenne = %s)." = "%d cases predicted (mean = %s).")
  expect_equal(trf(" %d cas prédits (moyenne = %s).", 12L, "3,4", lang = "en"),
               " 12 cases predicted (mean = 3,4).")
})

test_that("le kit de mise en forme declare, lit ET applique chacun de ses reglages", {
  # UNE LISTE RECOPIEE FINIT PAR DIVERGER, et c'est la copie oubliee qui ment :
  # la lecon des formats d'image, des champs de DPI, des themes et des palettes,
  # appliquee cette fois aux reglages de mise en forme que `mod_viz` portait
  # seul. Le test exige les TROIS conditions, comme celui du panneau post-hoc :
  # declare dans l'interface, lu par la lecture, et employe par le theme.
  skip_if_not_installed("shinydashboard")
  suppressMessages(hstat_installer_replis_ui())
  # LE DEFAUT NE S'ELARGIT PAS QUAND LE CATALOGUE S'ELARGIT : un module qui
  # appelle le kit sans nommer de familles demande le kit TEL QU'IL ETAIT.
  # Ajouter cinq familles au defaut avait donne quatre identifiants en double
  # dans la page -- `mod_yield` les declarait deja chez lui.
  expect_equal(HSTAT_PLOT_EXTRAS_DEFAUT, c("police", "axe", "cles", "marges"))
  expect_true(all(HSTAT_PLOT_EXTRAS_DEFAUT %in% HSTAT_PLOT_EXTRAS_FAMILLES))
  expect_gt(length(HSTAT_PLOT_EXTRAS_FAMILLES), length(HSTAT_PLOT_EXTRAS_DEFAUT))

  # 1. DECLARE -- et prefixe : un widget hors du prefixe de son module
  # n'existe pour personne, le defaut le plus silencieux du depot.
  h <- paste(as.character(hstat_plot_extras_ui(shiny::NS("m"), "pfx",
               familles = HSTAT_PLOT_EXTRAS_FAMILLES)), collapse = "")
  for (x in HSTAT_PLOT_EXTRAS)
    expect_true(grepl(sprintf('id="m-pfx%s"', x), h, fixed = TRUE), label = x)

  # 2. LU -- les defauts tiennent quand rien n'est saisi (le cas du premier
  # rendu, avant que le navigateur ait renvoye quoi que ce soit).
  o <- hstat_plot_extras_lire(list(), "pfx")
  expect_equal(o$police, 11); expect_false(o$axe); expect_equal(o$cles, 1.2)
  expect_equal(unname(o$marges), rep(5, 4))
  # Une saisie videe en cours de frappe rend NA : elle ne doit pas faire tomber
  # le graphique ni redimensionner la figure sous les doigts.
  expect_equal(hstat_plot_extras_lire(list(pfxPoliceBase = NA), "pfx")$police, 11)
  expect_equal(hstat_plot_extras_lire(list(pfxMargeHaut = "x"), "pfx")$marges[["haut"]], 5)

  i <- list(pfxPoliceBase = 14, pfxAxisLine = TRUE, pfxAxisLineCouleur = "#123456",
            pfxAxisLineEpaisseur = 2, pfxLegendeCles = 2, pfxMargeHaut = 20,
            pfxMargeBas = 3, pfxMargeGauche = 30, pfxMargeDroite = 4)
  o2 <- hstat_plot_extras_lire(i, "pfx")
  expect_equal(o2$police, 14); expect_true(o2$axe)
  expect_equal(unname(o2$marges), c(20, 4, 3, 30))   # haut, droite, bas, gauche

  # 3. APPLIQUE -- on lit les VALEURS du theme construit, jamais le nom des
  # reglages : un kit qui rendrait un theme vide passerait un controle qui ne
  # verifierait que la presence de la fonction.
  th <- hstat_plot_extras_theme(o2)
  expect_identical(th$axis.line$colour, "#123456")
  expect_equal(th$axis.line$linewidth, 2)
  expect_equal(as.numeric(th$legend.key.height), 2)
  expect_equal(as.numeric(th$plot.margin), c(20, 4, 3, 30))

  # Le trait d'axe ne s'impose ni ne s'efface : decoche, le kit ne pose RIEN,
  # la ou un `element_blank()` effacerait les axes d'un theme qui les trace.
  th0 <- hstat_plot_extras_theme(hstat_plot_extras_lire(list(), "pfx"))
  expect_null(th0$axis.line)

  # 4. LE KIT SE PREND PAR FAMILLE. Trois modules portaient deja une partie de
  # ce vocabulaire, ecrite a la main et gardee par des tests : leur poser le kit
  # entier declarerait DEUX fois le meme reglage, et c'est le second, invisible,
  # qui finirait par mentir.
  expect_setequal(names(HSTAT_PLOT_EXTRAS_PAR_FAMILLE), HSTAT_PLOT_EXTRAS_FAMILLES)
  # La liste plate DERIVE de la carte : les comparer serait une tautologie. Ce
  # qui se garde, c'est que les familles soient DISJOINTES -- un reglage range
  # dans deux d'entre elles serait declare deux fois par un module qui prend
  # les deux, et c'est le doublon silencieux qu'on vient de corriger.
  expect_equal(anyDuplicated(HSTAT_PLOT_EXTRAS), 0L)
  expect_true(all(vapply(HSTAT_PLOT_EXTRAS_PAR_FAMILLE, length, 0L) > 0L))

  # Ce qu'un module ne declare pas n'est ni affiche...
  hm <- paste(as.character(hstat_plot_extras_ui(shiny::NS("m"), "pfx",
                                                familles = "marges")), collapse = "")
  for (x in HSTAT_PLOT_EXTRAS_PAR_FAMILLE$marges)
    expect_true(grepl(sprintf('id="m-pfx%s"', x), hm, fixed = TRUE), label = x)
  for (x in c(HSTAT_PLOT_EXTRAS_PAR_FAMILLE$axe, HSTAT_PLOT_EXTRAS_PAR_FAMILLE$cles,
              HSTAT_PLOT_EXTRAS_PAR_FAMILLE$police))
    expect_false(grepl(sprintf('id="m-pfx%s"', x), hm, fixed = TRUE), label = x)

  # ...NI APPLIQUE. C'est la moitie qui compte : un module qui garde son propre
  # reglage de cles verrait sinon le kit lui imposer sa valeur par defaut, et le
  # sien cesserait d'agir sans un mot.
  om <- hstat_plot_extras_lire(i, "pfx", familles = "marges")
  thm <- hstat_plot_extras_theme(om)
  expect_equal(as.numeric(thm$plot.margin), c(20, 4, 3, 30))
  expect_null(thm$legend.key.height)
  expect_null(thm$axis.line)

  # Et le trait d'axe demande n'arrive pas si la famille n'est pas prise.
  expect_null(hstat_plot_extras_theme(
    hstat_plot_extras_lire(i, "pfx", familles = "cles"))$axis.line)
  expect_equal(as.numeric(hstat_plot_extras_theme(
    hstat_plot_extras_lire(i, "pfx", familles = "cles"))$legend.key.height), 2)

  # Le titre de la carte ne nomme AUCUNE famille : il reste vrai quel que soit
  # le sous-ensemble. « Cadre, marges et police » annoncerait deux reglages
  # absents d'un module qui ne prend que les marges.
  expect_false(grepl("police", hm, fixed = TRUE))
  # Aucune famille demandee : rien du tout, plutot qu'une carte vide.
  expect_null(hstat_plot_extras_ui(shiny::NS("m"), "pfx", familles = character(0)))
})

test_that("chaque module porte les quatre familles, et une seule fois", {
  # LA DEMANDE, MESUREE SUR CE QUI EST RENDU. « Les fonctionnalites de Options
  # du graphique du module visualisation doivent toutes etre presentes dans les
  # autres » : on ne verifie donc pas que le kit est APPELE -- un appel peut
  # etre place hors du chemin de rendu -- mais que le widget existe dans la page
  # que le module construit.
  #
  # « Une seule fois » est l'autre moitie, et c'est celle qui coute : deux
  # reglages pour un meme trait, c'est un utilisateur qui en change un pendant
  # que le graphique lit l'autre. C'est cette assertion qui interdit de poser le
  # kit entier sur un module qui porte deja une partie du vocabulaire.
  skip_if_not_installed("shinydashboard")
  root <- .hstat_repo_root(); skip_if(is.na(root))
  suppressMessages(hstat_installer_replis_ui())

  # Les motifs portent sur les IDENTIFIANTS rendus, pas sur une liste de noms
  # recopiee module par module : un module renomme le sien sans que ce test
  # cesse de le voir.
  motifs <- c(
    police = "policebase|fontbase|basefontsize|basesize|fontaxis|obase$",
    axe    = "^[a-z0-9_]*(show)?axisline",
    cles   = "keysize|legendecles",
    marges = "marge(haut|bas|gauche|droite)$|plotmargin(top|right|bottom|left)$")

  # `mod_tests_ui` n'a pas de panneau de mise en forme : les options du
  # graphique post-hoc vivent dans `mod_posthoc_ui`, qui est donc l'interface
  # a mesurer.
  # LE COMPTE ATTENDU EST LE NOMBRE DE PANNEAUX DE MISE EN FORME, pas 1 :
  # `mod_explore` en porte DEUX, un par graphique (distribution et valeurs
  # manquantes), et ses deux jeux de reglages sont legitimes -- ils habillent
  # deux figures differentes. C'est le test qui l'a etabli, pas une hypothese.
  uis <- c(mod_viz_ui = 1L, mod_posthoc_ui = 1L, mod_threshold_ui = 1L,
           mod_yield_ui = 1L, mod_dl50_ui = 1L, mod_descriptive_ui = 1L,
           mod_explore_ui = 2L, mod_design_ui = 1L, mod_qualitative_ui = 1L,
           mod_ml_ui = 1L, mod_dl_ui = 1L, mod_timeseries_ui = 1L)

  for (nm in names(uis)) {
    fn <- tryCatch(get(nm), error = function(e) NULL)
    if (is.null(fn)) { expect_true(FALSE, label = paste("UI absente :", nm)); next }
    h <- paste(as.character(fn(nm)), collapse = "")
    ids <- unique(regmatches(
      h, gregexpr(sprintf('(?<=id=")%s-[A-Za-z0-9_]+', nm), h, perl = TRUE))[[1]])
    ids <- sub(paste0("^", nm, "-"), "", ids)

    for (fam in names(motifs)) {
      # `mod_qualitative` est la seule exception, et elle est motivee : ses
      # quinze constructeurs posent chacun leur theme complet, si bien qu'un
      # curseur de police pose apres coup serait un reglage que l'image ignore.
      if (nm == "mod_qualitative_ui" && fam == "police") next
      # Le trait d'axe se declare en un widget (case a cocher) ou en trois
      # (case, couleur, epaisseur) ; les marges en quatre. Ce qui doit rester
      # unique, c'est le reglage MAITRE de la famille.
      maitres <- switch(
        fam,
        marges = grep("(haut|top)$", ids[grepl(motifs[[fam]], tolower(ids))],
                      ignore.case = TRUE, value = TRUE),
        axe    = grep("(couleur|epaisseur|color|width)$",
                      ids[grepl(motifs[[fam]], tolower(ids))],
                      ignore.case = TRUE, value = TRUE, invert = TRUE),
        ids[grepl(motifs[[fam]], tolower(ids))])
      expect_length(maitres, uis[[nm]])
      if (length(maitres) != uis[[nm]])
        message(sprintf("%s / %s -> %s", nm, fam, paste(maitres, collapse = ", ")))
    }
  }
})

test_that("un module qui declare la police l'emploie vraiment", {
  # LA POLICE DE BASE PART AU THEME, pas au kit : l'appliquer apres coup ne
  # toucherait que ce que le theme vient de fixer. C'est la seule des neuf
  # entrees dont l'emploi ne se voit pas dans `hstat_plot_extras_theme()` -- il
  # faut donc l'exiger DANS le module, sans quoi elle serait « declaree, lue,
  # mais jamais utilisee », le plus trompeur des trois defauts.
  root <- .hstat_repo_root(); skip_if(is.na(root))
  for (m in c("mod_yield.R", "mod_tests.R", "mod_threshold.R",
              "mod_descriptive.R", "mod_explore.R")) {
    txt <- paste(readLines(.hstat_module_path(m), warn = FALSE,
                           encoding = "UTF-8"), collapse = "\n")
    expect_true(grepl("base_size = extras$police", txt, fixed = TRUE), label = m)
    expect_true(grepl("hstat_plot_extras_theme(", txt, fixed = TRUE), label = m)
  }
  # Le rendement etait le premier adoptant : sa taille de police figee a 12 a
  # bien disparu.
  expect_false(grepl("base_size = 12)",
                     paste(readLines(.hstat_module_path("mod_yield.R"),
                                     warn = FALSE, encoding = "UTF-8"),
                           collapse = "\n"), fixed = TRUE))
})

test_that("une erreur d'API dit la cause, et surtout laquelle", {
  # LE CODE MENT, LE MESSAGE DIT LA VERITE. Le manque de credit est un 400 chez
  # Anthropic et un 429 chez OpenAI -- ce dernier partage donc son code avec le
  # depassement de cadence, qui appelle pourtant le geste INVERSE : attendre
  # plutot que payer. Reconnaitre sur le code seul rendrait donc, une fois sur
  # deux, le conseil contraire a celui qu'il faut suivre.
  cred <- hstat_ai_err_http(
    400, "Your credit balance is too low to access the Anthropic API. Please go to Plans & Billing to upgrade or purchase credits.",
    "Claude (Anthropic)")
  expect_true(grepl("crédit", cred, fixed = TRUE))
  # Le point qui compte pour l'utilisateur : sa cle n'est PAS en cause. Sans
  # cela il cherche une faute de frappe dans une cle parfaitement valide.
  expect_true(grepl("la clé est valide", cred, fixed = TRUE))
  # Et le message d'origine survit : c'est ce qu'on copie pour demander de
  # l'aide, et ce qui rend une reconnaissance fautive debuggable.
  expect_true(grepl("credit balance is too low", cred, fixed = TRUE))
  expect_true(grepl("HTTP 400", cred, fixed = TRUE))

  # Meme cause, autre fournisseur, autre code : c'est bien le message qui
  # tranche.
  quota <- hstat_ai_err_http(429, "You exceeded your current quota, please check your plan and billing details.", "ChatGPT (OpenAI)")
  expect_true(grepl("crédit", quota, fixed = TRUE))
  # Meme code, message different : le conseil doit changer.
  cadence <- hstat_ai_err_http(429, "Rate limit reached for requests", "ChatGPT (OpenAI)")
  expect_false(grepl("crédit", cadence, fixed = TRUE))
  expect_true(grepl("cadence", cadence, fixed = TRUE))
  # Les deux 429 ne rendent donc PAS la meme phrase : sans cette derniere
  # assertion, une fonction qui ignorerait le message passerait les deux
  # precedentes des qu'elle dirait « crédit » partout.
  expect_false(identical(quota, cadence))

  # Une cle refusee n'est pas un defaut de credit, et se repare autrement.
  cle <- hstat_ai_err_http(401, "invalid x-api-key", "Claude (Anthropic)")
  expect_true(grepl("clé d'API", cle, fixed = TRUE))
  expect_false(grepl("crédit", cle, fixed = TRUE))

  # Repli par le CODE quand le message ne dit rien : chacun a son geste.
  expect_true(grepl("modèle", hstat_ai_err_http(404, "", "X"), fixed = TRUE))
  expect_true(grepl("panne", hstat_ai_err_http(503, "", "X"), fixed = TRUE))
  expect_true(grepl("trop longue", hstat_ai_err_http(413, "", "X"), fixed = TRUE))

  # Un code inconnu n'est pas maquille : on ne promet pas une cause qu'on
  # ignore, on rend la phrase neutre et le message brut.
  inc <- hstat_ai_err_http(418, "je suis une théière", "Serveur local")
  expect_true(grepl("a refusé la requête", inc, fixed = TRUE))
  expect_true(grepl("théière", inc, fixed = TRUE))

  # Cas degeneres : ni code ni message ne doivent faire tomber la sortie --
  # c'est une fonction qui ne sert QUE sur le chemin d'erreur.
  expect_true(nzchar(hstat_ai_err_http(NA, "", "X")))
  expect_true(nzchar(hstat_ai_err_http("pas un nombre", NULL, "X")))
  expect_false(grepl("HTTP NA", hstat_ai_err_http(NA, "", "X"), fixed = TRUE))
})

test_that("les trois protocoles d'IA passent par le meme traducteur", {
  # Ils rendaient chacun le message du fournisseur tel quel, en anglais : trois
  # copies du meme defaut, qu'une correction faite a un seul endroit laisserait
  # diverger.
  root <- .hstat_repo_root(); skip_if(is.na(root))
  txt <- paste(readLines(.hstat_module_path("mod_ai.R"), warn = FALSE,
                         encoding = "UTF-8"), collapse = "\n")
  expect_equal(length(gregexpr("hstat_ai_err_http(", txt, fixed = TRUE)[[1]]), 3L)
  # Et les deux gabarits d'erreur brute ont bien disparu du code.
  expect_false(grepl("Erreur API (HTTP %d)", txt, fixed = TRUE))
  expect_false(grepl("Erreur de %s (HTTP %d)", txt, fixed = TRUE))
})


test_that("toute chaine passee a tr()/trf() est au dictionnaire", {
  root <- .hstat_repo_root()
  # Ces chaines sont traduites DANS R : leur absence du dictionnaire n'est
  # rattrapee par rien cote navigateur. La couverture doit donc y etre entiere.
  dic <- hstat_i18n_load()
  vide <- function(l, i) identical(l[[i]], quote(expr = ))
  acc <- character(0)
  for (f in .hstat_sources_app()) for (ex in parse(f)) {
    rec <- function(x) {
      if (!is.call(x)) return(invisible())
      nm <- if (is.name(x[[1]])) as.character(x[[1]]) else ""
      if (nm %in% c("tr", "trf") && length(x) >= 2 && is.character(x[[2]]))
        acc <<- c(acc, x[[2]])
      l <- as.list(x)
      for (i in seq_along(l)) if (!vide(l, i)) rec(l[[i]])
    }
    rec(ex)
  }
  acc <- unique(trimws(acc))          # comme `tr()`, qui cherche sur l'elague
  expect_gt(length(acc), 200L)
  expect_equal(setdiff(acc, dic$fr), character(0))
})

test_that("les gabarits ne partent pas au navigateur", {
  # Une phrase composee est traduite DANS R, avant d'exister. Sa forme a
  # marqueurs n'apparait jamais telle quelle dans le DOM : l'envoyer
  # alourdirait la page sans rien pouvoir y remplacer.
  j <- hstat_i18n_json("en")
  expect_false(grepl("%s", j, fixed = TRUE))
  expect_false(grepl("%d", j, fixed = TRUE))
  # Mais l'interface simple, elle, part toujours.
  expect_true(grepl("Chargement", j, fixed = TRUE))
  # Le poids embarque reste la promesse de legerete. Le plafond est declare
  # DANS LE SOCLE : il vivait en dur ici ET dans un second test, meme valeur
  # recopiee -- deux chiffres qui ne se parlent pas finissent par diverger.
  expect_lt(nchar(j) / 1024, HSTAT_I18N_KO_MAX)

  # Le motif vise les MARQUEURS de sprintf, pas le caractere « % » seul : un
  # libelle d'interface comme « % colonne » doit continuer de partir.
  motif <- HSTAT_I18N_MARQUEUR
  for (x in c("% colonne", "% ligne", "Taux de 50 % atteint",
              "100 % de valeurs manquantes"))
    expect_false(grepl(motif, x), info = x)
  for (x in c("%s : %d valeur(s)", "%.1f %% des observations", "%d groupes"))
    expect_true(grepl(motif, x), info = x)
})


test_that("les termes du fichier de l'utilisateur sont recenses et bornes", {
  d <- data.frame(Total = c(1, 2), Reponse = c("Oui", "Non"),
                  Normal = c("Moyenne", "Total"), stringsAsFactors = FALSE)
  t <- hstat_i18n_termes_donnees(d)
  # Noms de colonnes ET modalites qualitatives : les deux coincident avec des
  # libelles d'interface, les deux doivent etre proteges.
  for (x in c("Total", "Reponse", "Normal", "Oui", "Non", "Moyenne"))
    expect_true(x %in% t, info = x)

  # Une colonne de TEXTE LIBRE n'est pas une variable qualitative : envoyer ses
  # milliers de modalites alourdirait la page sans rien proteger d'utile.
  libre <- data.frame(txt = paste("reponse libre numero", 1:500),
                      stringsAsFactors = FALSE)
  expect_equal(hstat_i18n_termes_donnees(libre, max_modalites = 200L), "txt")

  # La liste totale est bornee.
  gros <- as.data.frame(matrix("", nrow = 1, ncol = 5000), stringsAsFactors = FALSE)
  expect_lte(length(hstat_i18n_termes_donnees(gros, max_termes = 3000L)), 3000L)

  # Entrees degenerees : jamais d'erreur, une liste vide.
  for (x in list(NULL, data.frame(), "texte", 42))
    expect_silent(hstat_i18n_termes_donnees(x))
})

test_that("les termes sont encodes en JSON sans casser sur la ponctuation", {
  # Le terme vient du fichier : il peut contenir guillemet, barre oblique
  # inverse, tabulation. `fixed = TRUE` est indispensable — une barre seule
  # n'est pas une expression reguliere valide (« Trailing backslash »).
  h <- data.frame(a = c('il dit "oui"', "c:\\chemin", "avec\ttab"),
                  stringsAsFactors = FALSE)
  j <- hstat_i18n_termes_json(h)
  expect_true(grepl('\\\\"oui', j))
  expect_equal(hstat_i18n_termes_json(NULL), "[]")
  expect_equal(hstat_i18n_termes_json(data.frame()), "[]")

  root <- .hstat_repo_root()
  skip_if(is.na(root))
  a <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("hstat-termes-donnees", a, fixed = TRUE))
  js <- paste(readLines(file.path(root, "inst", "app", "www", "hstat-i18n.js"),
                        warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("hstat-termes-donnees", js, fixed = TRUE))
})


# ===========================================================================
# REINITIALISATION COMPLETE
# ---------------------------------------------------------------------------
# La remise a zero effacait une liste de champs ENUMEREE A LA MAIN, distincte
# de celle qui cree `reactiveValues`. Les deux ont derive : tout champ ajoute
# depuis survivait a la reinitialisation, et l'utilisateur retrouvait des
# restes de sa session precedente.
# ===========================================================================

test_that("l'etat initial est decrit a un seul endroit", {
  init <- hstat_valeurs_initiales()
  expect_true(is.list(init))
  expect_gt(length(init), 50L)
  expect_true(all(nzchar(names(init))))
  expect_false(any(duplicated(names(init))))
  # Les champs structurants doivent y figurer, sinon ils ne seraient ni crees
  # au demarrage ni effaces a la reinitialisation.
  for (nm in c("data", "cleanData", "filteredData",
               "dbCon", "dataMode", "resetSignal", "fichierNeutralise"))
    expect_true(nm %in% names(init), info = nm)
  # Les valeurs par defaut qui ne sont pas NULL sont celles qu'on attend
  expect_equal(init$dataMode, "memory")
  expect_equal(init$resetSignal, 0)
  expect_false(init$isSampled)
  expect_equal(init$allTestResults, list())
})

test_that("creation et reinitialisation partagent la meme liste", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # `reactiveValues` est construit DEPUIS la liste, pas recopie a cote
  expect_true(grepl("do.call(shiny::reactiveValues, hstat_valeurs_initiales())",
                    src, fixed = TRUE))
  # La remise a zero elle-meme vit dans le socle, en UN exemplaire : deux
  # gestes l'appellent (le bouton, et le chargement de nouvelles donnees), et
  # deux copies finiraient par diverger -- l'une des deux oublierait un champ.
  expect_true(grepl("hstat_reinitialiser_valeurs(values)", src, fixed = TRUE))
  socle <- paste(readLines(.hstat_socle_path(), warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
  expect_true(grepl("init <- hstat_valeurs_initiales()", socle, fixed = TRUE))
  expect_true(grepl("for (nm in names(init)) values[[nm]] <- init[[nm]]",
                    socle, fixed = TRUE))
  # Et vide en plus ce qu'un module aurait cree en cours de session -- y
  # compris les `reactiveVal`, que `hstat_vider_valeurs()` remet a NULL EN LES
  # APPELANT plutot qu'en les detruisant.
  expect_true(grepl("hstat_vider_valeurs(values)", socle, fixed = TRUE))
  expect_true(grepl("setdiff(names(shiny::isolate(shiny::reactiveValuesToList(rv))), sauf)",
                    socle, fixed = TRUE))
  # L'ancienne enumeration a la main a bien disparu
  expect_false(grepl("values$chiSqPGlobal     <- NULL", src, fixed = TRUE))
})

test_that("charger de nouvelles donnees remet l'etat de session a zero", {
  # LE DEFAUT QUE CE TEST GARDE, signale a l'ecran : apres le chargement d'un
  # SECOND fichier, les anciennes variables restaient proposees et les
  # resultats du premier restaient affiches. Quarante-huit champs crees par les
  # modules portent des noms de colonnes et des niveaux de facteur
  # (`yVarNames`, `selected_y_vars`, `x_label_levels`, `customXLevels`...) :
  # aucun n'etait efface. On lisait donc des tableaux et des recommandations
  # portant sur des colonnes qui n'existent plus -- pire qu'un ecran vide,
  # parce que ca ressemble a un resultat.
  v <- do.call(shiny::reactiveValues, hstat_valeurs_initiales())
  shiny::isolate({
    v$data <- data.frame(a = 1); v$cleanData <- v$data; v$filteredData <- v$data
    v$descStats <- "resultats du fichier precedent"
    v$transformationLog <- list(x = 1)
    v$customXOrder <- c("A", "B")
    v$allTestResults <- list(t = 1)
    # Champs CREES PAR UN MODULE : ils ne figurent pas dans la liste initiale,
    # et ce sont eux qui portent les noms de variables.
    v$yVarNames <- c("ancienne_variable")
    v$selected_y_vars <- c("ancienne_variable")
    v$manovaOutliers <- 1:3

    expect_equal(hstat_reinitialiser_valeurs(v), 1L)

    for (nm in c("data", "cleanData", "filteredData", "descStats",
                 "customXOrder", "yVarNames", "selected_y_vars", "manovaOutliers"))
      expect_null(v[[nm]], info = nm)
    # Les valeurs par defaut qui ne sont pas NULL reviennent, elles aussi.
    expect_equal(v$transformationLog, list())
    expect_equal(v$allTestResults, list())
    expect_equal(v$dataMode, "memory")

    # LE COMPTEUR EST MONOTONE. Les modules l'observent pour vider leurs
    # propres controles, et `observeEvent` ne reagit qu'a un CHANGEMENT : le
    # remettre a son initiale (0) puis l'incrementer rendrait toujours 1, et le
    # deuxieme chargement ne signalerait plus rien.
    expect_equal(hstat_reinitialiser_valeurs(v), 2L)
    expect_equal(v$resetSignal, 2L)
    expect_equal(hstat_reinitialiser_valeurs(v), 3L)
  })
})

test_that("un reactiveVal range dans values survit a la remise a zero", {
  # LE DEFAUT QUE CE TEST GARDE, introduit par la remise a zero elle-meme.
  # `values$customXLevels` EST une fonction -- un `reactiveVal` --, et le reste
  # du code l'appelle comme telle : `values$customXLevels()`. Lui affecter NULL
  # DETRUIT l'objet au lieu de le vider ; l'appel suivant leve « attempt to
  # apply non-function » et tout le graphique post-hoc tombe, sur un geste
  # aussi banal que charger un autre fichier.
  v <- do.call(shiny::reactiveValues, hstat_valeurs_initiales())
  shiny::isolate({
    v$customXLevels <- shiny::reactiveVal(NULL)
    v$customXLevels(c("B", "A"))
    v$descStats <- "resultats du fichier precedent"

    hstat_reinitialiser_valeurs(v)

    expect_true(is.function(v$customXLevels))
    expect_true(inherits(v$customXLevels, "reactiveVal"))
    expect_null(v$customXLevels())          # vide, mais VIVANT
    expect_null(v$descStats)
    # Et il reste utilisable : c'est tout l'objet de la nuance.
    v$customXLevels(c("A", "B"))
    expect_equal(v$customXLevels(), c("A", "B"))
  })

  # `hstat_vider_valeurs()` sait aussi epargner ce qu'on lui nomme.
  w <- shiny::reactiveValues(a = 1, b = 2)
  shiny::isolate({
    hstat_vider_valeurs(w, sauf = "b")
    expect_null(w$a)
    expect_equal(w$b, 2)
  })
})

test_that("les modules a etat propre ecoutent le signal de remise a zero", {
  # Un module qui garde son etat dans SES PROPRES `reactiveVal` echappe a la
  # remise a zero de `values` : le tableau d'efficacites et les quatre messages
  # de nettoyage decrivaient encore le fichier PRECEDENT apres un nouveau
  # chargement. `resetSignal` est le seul canal qui les atteint.
  root <- .hstat_repo_root(); skip_if(is.na(root))
  attendu <- list("mod_threshold.R" = "eff_res(NULL)",
                  "mod_clean.R"     = "rename_msg(NULL)")
  for (f in names(attendu)) {
    chemin <- .hstat_module_path(f)
    skip_if_not(file.exists(chemin), f)
    txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_true(grepl("observeEvent(values$resetSignal", txt, fixed = TRUE), label = f)
    expect_true(grepl(attendu[[f]], txt, fixed = TRUE), label = paste(f, attendu[[f]]))
  }
})

test_that("les trois portes d'entree des donnees remettent l'etat a zero", {
  # Un quatrieme chemin d'import ajoute demain sans cet appel reintroduirait le
  # defaut en silence : les anciennes variables reviendraient, sans erreur.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- readLines(file.path(root, "inst", "app", "app_server.R"),
                   warn = FALSE, encoding = "UTF-8")
  ex <- parse(text = paste(src, collapse = "\n"), keep.source = FALSE)
  portes <- c("input$loadData", "input$applySheetMerge", "input$applyMerge")
  vus <- character(0)
  v <- function(n) {
    if (is.call(n)) {
      nm <- sub("^shiny::", "", paste(deparse(n[[1]]), collapse = ""))
      if (nm == "observeEvent" && length(n) >= 3) {
        dec <- paste(deparse(n[[2]]), collapse = "")
        if (dec %in% portes) {
          corps <- paste(deparse(n[[3]]), collapse = " ")
          if (grepl(".hstat_purger_session", corps, fixed = TRUE))
            vus <<- c(vus, dec)
        }
      }
      for (i in seq_along(n)) tryCatch(v(n[[i]]), error = function(e) NULL)
    } else if (is.pairlist(n) || is.expression(n)) {
      for (i in seq_along(n)) tryCatch(v(n[[i]]), error = function(e) NULL)
    }
  }
  for (k in seq_along(ex)) v(ex[[k]])
  expect_equal(sort(unique(vus)), sort(portes))
  # Et le purgeur fait bien les deux : l'etat partage ET les resultats
  # multivaries, qui vivent dans leur propre `reactiveValues`.
  src1 <- paste(src, collapse = "\n")
  expect_true(grepl("hstat_reinitialiser_valeurs(values)", src1, fixed = TRUE))
  expect_true(grepl("hstat_vider_valeurs(mv_res)", src1, fixed = TRUE))
})

test_that("le fichier reste neutralise apres la reinitialisation", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # shinyjs::reset() remet le WIDGET a blanc mais input$file garde sa valeur :
  # sans temoin, la feuille Excel et le bloc de combinaison survivaient.
  expect_true(grepl("values$fichierNeutralise <- shiny::isolate(input$file$datapath)",
                    src, fixed = TRUE))
  expect_true(grepl("fichier_actif <- shiny::reactive", src, fixed = TRUE))
  # Tout ce qui derive du fichier passe par cette porte, et une seule
  lignes <- strsplit(src, "\n", fixed = TRUE)[[1]]
  lignes <- lignes[!grepl("^\\s*#", lignes)]
  brut <- grep("input\\$file", lignes, value = TRUE)
  brut <- brut[!grepl("fichierNeutralise|f <- input\\$file", brut)]
  expect_equal(brut, character(0),
    info = paste("Acces direct a input$file, qui ignore la neutralisation :\n  ",
                 paste(brut, collapse = "\n   ")))
})

test_that("la reinitialisation ne depend pas d'un paquet optionnel", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # shinyalert est optionnel : sans repli, shinyalert() levait une erreur
  # avalee par l'observateur et le bouton ne faisait RIEN, en silence.
  expect_true(grepl('requireNamespace("shinyalert"', src, fixed = TRUE))
  expect_true(grepl("shiny::showModal(shiny::modalDialog(", src, fixed = TRUE))
  expect_true(grepl("resetConfirm", src, fixed = TRUE))
  # Les deux chemins de confirmation appellent la MEME remise a zero
  expect_true(grepl(".hstat_reinitialiser <- function()", src, fixed = TRUE))
  expect_gte(lengths(gregexpr(".hstat_reinitialiser()", src, fixed = TRUE)), 2L)
})


# ===========================================================================
# MESSAGES D'ERREUR BILINGUES
# ---------------------------------------------------------------------------
# Ces messages sont COMPOSES en R, phrase par phrase : ils n'existent pas comme
# chaine entiere dans le dictionnaire du navigateur, qui ne remplace que des
# correspondances completes. La traduction se fait donc cote serveur — mais
# elle puise dans LE MEME fichier CSV, une seule source de verite.
# ===========================================================================

test_that("les 23 explications d'erreur sont traduites", {
  textes <- vapply(HSTAT_ERR_FR, function(r) r[[2]], character(1))
  cv <- hstat_i18n_coverage(textes)
  expect_equal(cv$manquantes, character(0),
    info = paste("Explications non traduites :\n  ",
                 paste(substr(cv$manquantes, 1, 60), collapse = "\n   ")))
  expect_equal(cv$traduites, length(textes))
})

test_that("hstat_err_fr rend l'anglais quand la langue est l'anglais", {
  fr <- hstat_err_fr(simpleError("data are essentially constant"), "Test t", "fr")
  en <- hstat_err_fr(simpleError("data are essentially constant"), "t-test", "en")
  expect_true(grepl("La variable ne varie pas", fr, fixed = TRUE))
  expect_true(grepl("The variable does not vary", en, fixed = TRUE))
  # Le message R d'origine survit dans les deux langues : c'est ce qu'un
  # utilisateur copiera pour demander de l'aide.
  expect_true(grepl("data are essentially constant", fr, fixed = TRUE))
  expect_true(grepl("data are essentially constant", en, fixed = TRUE))
  # L'encadrement suit la langue, sinon la phrase serait mi-anglaise
  expect_true(grepl("message R :", fr, fixed = TRUE))
  expect_true(grepl("R message:", en, fixed = TRUE))
  expect_false(grepl("message R :", en, fixed = TRUE))
})

test_that("une erreur inconnue s'annonce comme non traduite dans les deux langues", {
  fr <- hstat_err_fr(simpleError("panne inedite"), NULL, "fr")
  en <- hstat_err_fr(simpleError("panne inedite"), NULL, "en")
  expect_true(grepl("non traduit", fr, fixed = TRUE))
  # Ponctuation francaise en francais, anglaise en anglais
  expect_true(grepl("(non traduit) : panne", fr, fixed = TRUE))
  expect_true(grepl("(untranslated): panne", en, fixed = TRUE))
  expect_true(grepl("untranslated", en, fixed = TRUE))
  expect_true(grepl("panne inedite", en, fixed = TRUE))
  # Une erreur sans message reste explicite
  expect_true(grepl("error with no message",
                    hstat_err_fr(simpleError(""), NULL, "en"), fixed = TRUE))
})

test_that("la langue est propre a la session, jamais globale", {
  # Hors Shiny, le francais s'applique — et rien ne plante.
  expect_equal(hstat_langue_session(), "fr")
  # Le defaut de hstat_err_fr lit la session : aucun des ~70 points d'appel
  # n'a besoin de passer la langue.
  expect_true(grepl("hstat_langue_session()",
                    paste(deparse(args(hstat_err_fr)), collapse = " "),
                    fixed = TRUE))

  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # `hstat_langue_session()` est une definition : elle vit dans le socle.
  skip_if(is.na(.hstat_socle_path()))
  u <- paste(readLines(.hstat_socle_path(), warn = FALSE,
                       encoding = "UTF-8"), collapse = "\n")
  # `session$userData` et non une option globale : sur un serveur partage, une
  # option ferait basculer la langue de TOUS les utilisateurs a la fois.
  expect_true(grepl("d$userData$langue", u, fixed = TRUE))
  expect_false(grepl('getOption("hstat.langue"', u, fixed = TRUE))

  a <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("session$userData$langue <-", a, fixed = TRUE))
  expect_true(grepl("input$hstat_langue", a, fixed = TRUE))

  js <- paste(readLines(file.path(root, "inst", "app", "www", "hstat-i18n.js"),
                        warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("hstat_langue", js, fixed = TRUE))
  # Shiny n'est pas pret quand ce fichier s'execute : sans reessai, la langue
  # choisie avant la connexion ne parviendrait jamais au serveur.
  expect_true(grepl("setTimeout(envoyer", js, fixed = TRUE))
})


# ===========================================================================
# UNE ERREUR CAPTUREE DOIT ARRIVER JUSQU'AU TABLEAU
# ---------------------------------------------------------------------------
# Six analyses (normalite, homogeneite, t-test, Wilcoxon, Kruskal-Wallis,
# Scheirer-Ray-Hare) construisaient soigneusement une ligne de resultat portant
# `hstat_err_fr(e)` — puis la JETAIENT. Dans
#
#     results_list <- list()
#     for (var in vars) tryCatch({ ... }, error = function(e) {
#       results_list[[var]] <- data.frame(...)      # <- affectation LOCALE
#     })
#
# le `<-` cree une copie dans le cadre du gestionnaire ; la liste de
# l'observateur n'est pas touchee. Consequence a l'ecran : la variable en echec
# DISPARAIT du tableau sans un mot, et si c'etait la seule, l'utilisateur ne
# recoit qu'un « Aucun resultat genere » qui masque la vraie cause (« variance
# nulle : toutes les valeurs sont identiques, choisissez une autre variable »).
#
# Tout le travail de traduction des messages d'erreur etait annule a l'endroit
# meme ou il devait servir. Le lapsus est atteste : ligne 2832, un `<<-`
# correct precede de deux lignes le `<-` fautif.
#
# `gdf[[fvar]] <- ...` dans les gestionnaires de comparaisons multiples n'est
# PAS le meme cas : `gdf` y est cree dans le corps du gestionnaire, qui le
# RENVOIE. D'ou la regle du balayage : n'est fautive qu'une affectation a un
# nom que le gestionnaire n'a pas lui-meme defini.
# ===========================================================================

# ===========================================================================
# ATELIER DE CODAGE : REQUETE COMBINEE, CONCORDANCIER, PORTRAIT, ACCORD
# ---------------------------------------------------------------------------
# Les quatre analyses que MAXQDA propose et qui manquaient encore. Chacune a
# un piege propre, et les trois premiers ont ete constates ici meme.
# ===========================================================================

.hstat_corpus_test <- function() {
  docs <- data.frame(
    doc_id = c("d1", "d2", "d3"), row = 1:3,
    text = c("Le prix est trop cher mais la qualite du service est bonne.",
             "Le service est lent. Vraiment lent.",
             "Le prix me convient tout a fait."),
    stringsAsFactors = FALSE)
  cb <- hstat_code_add(hstat_code_add(hstat_code_new_codebook(), "Prix"), "Service")
  idP <- cb$code_id[cb$label == "Prix"]; idS <- cb$code_id[cb$label == "Service"]
  s <- hstat_code_new_segments()
  s <- hstat_seg_add(s, "d1", idP,  3, 20, "prix est trop cher", "alice")
  s <- hstat_seg_add(s, "d1", idS, 25, 50, "qualite du service", "alice")
  s <- hstat_seg_add(s, "d2", idS,  3, 20, "service est lent",   "alice")
  s <- hstat_seg_add(s, "d3", idP,  3, 15, "prix me convient",   "alice")
  list(docs = docs, cb = cb, seg = s, P = idP, S = idS)
}

test_that("la portee change le sens de la requete, et elle est annoncee", {
  x <- .hstat_corpus_test()
  # « Meme document » : Prix et Service coexistent chez d1, meme a distance.
  d <- hstat_code_query(x$seg, x$cb, x$docs, x$P, x$S, "et", "document")
  expect_equal(unique(d$doc_id), "d1")
  expect_equal(attr(d, "portee"), "document")

  # « Meme passage » : aucun extrait ne porte les deux etiquettes ici.
  o <- hstat_code_query(x$seg, x$cb, x$docs, x$P, x$S, "et", "overlap")
  expect_equal(nrow(o), 0L)

  # « A proximite » : 20 et 25 sont distants de 5 caracteres.
  expect_equal(nrow(hstat_code_query(x$seg, x$cb, x$docs, x$P, x$S,
                                     "et", "proximite", distance = 10)), 1L)
  expect_equal(nrow(hstat_code_query(x$seg, x$cb, x$docs, x$P, x$S,
                                     "et", "proximite", distance = 1)), 0L)

  # Trois portees, trois effectifs pour la MEME question : d'ou l'attribut.
  expect_false(identical(nrow(d), nrow(o)))
})

test_that("SAUF retranche, OU reunit, et OU n'a pas de portee", {
  x <- .hstat_corpus_test()
  sauf <- hstat_code_query(x$seg, x$cb, x$docs, x$P, x$S, "sauf", "document")
  expect_equal(unique(sauf$doc_id), "d3")

  ou <- hstat_code_query(x$seg, x$cb, x$docs, x$P, x$S, "ou")
  expect_equal(nrow(ou), nrow(x$seg))
  # « OU » ne croise rien : afficher une portee laisserait croire le contraire.
  expect_true(is.na(attr(ou, "portee")))

  # Sans second ensemble : « ET » ne peut rien confirmer, « SAUF » rien retirer.
  expect_equal(nrow(hstat_code_query(x$seg, x$cb, x$docs, x$P, NULL, "et")), 0L)
  expect_equal(nrow(hstat_code_query(x$seg, x$cb, x$docs, x$P, NULL, "sauf")), 2L)
})

test_that("le concordancier ne casse pas sur la ponctuation de l'utilisateur", {
  d <- data.frame(doc_id = "d1", row = 1L,
                  text = "Le prix (trop cher) reste un probleme. Le prix monte.",
                  stringsAsFactors = FALSE)
  # Par defaut le motif est ECHAPPE : taper une parenthese ne doit ni lever
  # « unmatched parenthesis » ni chercher un groupe de capture.
  k <- hstat_code_kwic(d, "prix (trop cher)")
  expect_equal(nrow(k), 1L)
  expect_equal(k$Motif, "prix (trop cher)")

  expect_equal(nrow(hstat_code_kwic(d, "prix")), 2L)

  # Une expression reguliere INVALIDE rend zero ligne, elle ne fait pas
  # tomber le panneau.
  expect_silent(bad <- hstat_code_kwic(d, "prix (", regex = TRUE))
  expect_equal(nrow(bad), 0L)

  # En mode regex, le motif est bien interprete.
  expect_equal(nrow(hstat_code_kwic(d, "pri[xz]", regex = TRUE)), 2L)

  # Casse
  expect_equal(nrow(hstat_code_kwic(d, "PRIX")), 2L)
  expect_equal(nrow(hstat_code_kwic(d, "PRIX", casse = TRUE)), 0L)

  # Entrees vides : jamais d'erreur, un tableau vide.
  for (m in list("", "   ", NA_character_, NULL))
    expect_equal(nrow(hstat_code_kwic(d, m)), 0L)
  expect_equal(nrow(hstat_code_kwic(NULL, "prix")), 0L)
})

test_that("le portrait du document est en pourcentage, pas en caracteres", {
  x <- .hstat_corpus_test()
  cl <- hstat_code_codeline(x$seg, x$cb, x$docs, "d1")
  expect_equal(nrow(cl), 2L)
  expect_true(all(cl$debut_pct >= 0 & cl$fin_pct <= 100))
  # C'est tout l'interet : deux reponses de longueurs differentes deviennent
  # comparables. Le meme segment sur un document deux fois plus court occupe
  # deux fois plus de place.
  court <- x$docs; court$text[1] <- substr(court$text[1], 1, 30)
  cl2 <- hstat_code_codeline(x$seg, x$cb, x$docs, "d1")
  cl3 <- hstat_code_codeline(x$seg, x$cb, court, "d1")
  expect_gt(cl3$fin_pct[1], cl2$fin_pct[1])

  # Un document introuvable ou vide ne doit pas produire d'Inf silencieux.
  vide <- x$docs; vide$text[1] <- ""
  cv <- hstat_code_codeline(x$seg, x$cb, vide, "d1")
  expect_true(all(is.finite(cv$debut_pct)))
  # « FINI » NE SUFFIT PAS : 100 / 0 vaut Inf, que pmin(100, .) ramene a 100.
  # Le segment ressortait alors comme occupant TOUT le document -- un resultat
  # faux et parfaitement fini, que la seule verification de finitude laissait
  # passer. Sur un document sans texte il n'y a rien a mettre a l'echelle : la
  # position reste a zero.
  expect_true(all(cv$debut_pct == 0 & cv$fin_pct == 0))
  # Et un segment demarrant a zero donnerait 0 * Inf, donc NaN.
  s0 <- hstat_seg_add(x$seg, "d1", x$P, 0, 4, "", "alice")
  expect_false(any(is.nan(hstat_code_codeline(s0, x$cb, vide, "d1")$debut_pct)))
  expect_equal(nrow(hstat_code_codeline(x$seg, x$cb, x$docs, "inconnu")), 0L)
})

test_that("deux codeurs peuvent etiqueter le meme passage", {
  # LE CODEUR FAIT PARTIE DE L'IDENTITE DU SEGMENT. Sans lui dans le test de
  # doublon, l'accord PARFAIT — le cas le plus courant — voyait le second
  # codage silencieusement ecarte, et l'accord portait sur un corpus ampute.
  cb <- hstat_code_add(hstat_code_new_codebook(), "Prix")
  s <- hstat_code_new_segments()
  s <- hstat_seg_add(s, "d1", cb$code_id[1], 1, 5, "x", "alice")
  s <- hstat_seg_add(s, "d1", cb$code_id[1], 1, 5, "x", "alice")  # meme codeur
  expect_equal(nrow(s), 1L)
  s <- hstat_seg_add(s, "d1", cb$code_id[1], 1, 5, "x", "bob")    # autre codeur
  expect_equal(nrow(s), 2L)
})

test_that("l'accord inter-codeurs rend un verdict, jamais un NaN branchable", {
  x <- .hstat_corpus_test()
  s <- x$seg
  s <- hstat_seg_add(s, "d1", x$P,  4, 21, "prix",    "bob")   # accord
  s <- hstat_seg_add(s, "d2", x$S,  2, 19, "service", "bob")   # accord
  s <- hstat_seg_add(s, "d2", x$P,  2, 19, "faux",    "bob")   # desaccord

  a <- hstat_code_accord(s, x$cb, "alice", "bob")
  # Documents communs : d1 et d2 (d3 n'a ete vu que par alice). Quatre unites,
  # deux accords — (d1,Prix) et (d2,Service) — et deux desaccords : bob n'a
  # pas pose Service sur d1, et il a pose Prix sur d2.
  expect_equal(a$n_unites, 4L)
  expect_equal(a$accord, 0.5)
  expect_true(is.finite(a$kappa))
  expect_true(a$verdict %in% c("excellent", "acceptable", "faible"))

  # ACCORD PARFAIT : pe vaut 1, kappa se derobe. Brancher sur un NaN leverait
  # « missing value where TRUE/FALSE needed » ; on rend `indeterminable` et le
  # pourcentage d'accord, qui lui reste lisible.
  cb <- hstat_code_add(hstat_code_new_codebook(), "Prix")
  p <- hstat_code_new_segments()
  p <- hstat_seg_add(p, "d1", cb$code_id[1], 1, 5, "x", "alice")
  p <- hstat_seg_add(p, "d1", cb$code_id[1], 1, 5, "x", "bob")
  ap <- hstat_code_accord(p, cb, "alice", "bob")
  expect_equal(ap$accord, 1)
  expect_equal(ap$verdict, "indeterminable")
  expect_true(grepl("hasard", ap$message))
  expect_false(is.finite(ap$kappa))

  # Deux fois le meme codeur, ou un codeur absent : refus explicite.
  expect_equal(hstat_code_accord(s, x$cb, "alice", "alice")$verdict, "indeterminable")
  expect_equal(hstat_code_accord(s, x$cb, "alice", "zoe")$n_unites, 0L)

  # Seuls les documents que LES DEUX ont vus comptent : sinon l'absence de
  # codage d'un document jamais ouvert passerait pour un desaccord.
  solo <- hstat_seg_add(x$seg, "d9", x$P, 1, 5, "y", "bob")
  expect_equal(hstat_code_accord(solo, x$cb, "alice", "bob")$n_unites, 0L)
})

test_that("les quatre analyses encaissent un atelier vide", {
  cb <- hstat_code_new_codebook(); s <- hstat_code_new_segments()
  d <- data.frame(doc_id = character(0), row = integer(0), text = character(0),
                  stringsAsFactors = FALSE)
  expect_equal(nrow(hstat_code_query(s, cb, d, NULL, NULL)), 0L)
  expect_equal(nrow(hstat_code_kwic(d, "x")), 0L)
  expect_equal(nrow(hstat_code_codeline(s, cb, d, "d1")), 0L)
  expect_equal(hstat_code_accord(s, cb, "a", "b")$verdict, "indeterminable")
  expect_silent(hstat_code_codeline_plot(NULL))
})

test_that("l'atelier expose bien les quatre analyses dans l'interface", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  m <- paste(readLines(.hstat_module_path("mod_coding.R"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (f in c("hstat_code_query", "hstat_code_kwic", "hstat_code_codeline",
              "hstat_code_accord"))
    expect_true(grepl(paste0(f, "("), m, fixed = TRUE), info = f)
  for (t in c("Requete combinee", "Concordancier", "Portrait du document",
              "Accord inter-codeurs"))
    expect_true(grepl(t, m, fixed = TRUE), info = t)
})


test_that("le tableau des valeurs aberrantes a une sortie dediee", {
  # `renderTable()` appele depuis un renderUI ne produit qu'un conteneur vide,
  # jamais alimente : le tableau des bornes ne s'affichait JAMAIS, et la note
  # en dessous expliquait des « Bornes basse/haute » absentes de l'ecran.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  m <- paste(readLines(.hstat_module_path("mod_clean.R"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl('tableOutput(ns("outlierTable"))', m, fixed = TRUE))
  expect_true(grepl("output$outlierTable <- shiny::renderTable", m, fixed = TRUE))
})

test_that("le post-hoc rend les vraies moyennes, pas des valeurs tronquees", {
  # LE DEFAUT QUE CE TEST GARDE, et il rendait un REGLAGE MENTEUR. L'onglet
  # porte « Arrondir les résultats numériques », décoché par défaut, et
  # promet : « Si décoché, les valeurs s'affichent sans arrondi ». Or les
  # tableaux de comparaisons multiples arrondissaient `Moyenne`, `Ecart_type`,
  # `Erreur_type` et `CV` A DEUX DECIMALES AU MOMENT DU CALCUL, sur trois
  # sites. La valeur était détruite avant d'atteindre l'affichage : décocher
  # la case ne pouvait plus rien restituer, et l'export comme le rapport ne
  # portaient qu'une moyenne à deux décimales.
  #
  # L'arrondi appartient à l'AFFICHAGE (`round_numeric_df`), jamais au calcul.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  m <- readLines(.hstat_module_path("mod_tests.R"), warn = FALSE,
                 encoding = "UTF-8")
  fautifs <- grep("(Moyenne|Ecart_type|Erreur_type|CV|P_interaction)[[:space:]]*=[[:space:]]*round\\(",
                  m, value = TRUE)
  expect_equal(fautifs, character(0),
               info = paste("arrondi au calcul :", paste(fautifs, collapse = " | ")))
  # La regle de decimales ne se recopie pas non plus : elle etait ecrite en
  # toutes lettres a deux endroits de l'affichage, en plus du socle.
  expect_equal(grep("is.null(input$multiDecimals)", m, fixed = TRUE), integer(0))

  # 1. « PAS D'ARRONDI » N'EST PAS « ARRONDI A TROIS DECIMALES ». La case est
  #    decochee par defaut et promet des valeurs sans arrondi : la regle rend
  #    donc NA, que le formateur lit comme « rends le nombre tel qu'il est ».
  #    Une precision par defaut faisait afficher deux precisions differentes
  #    pour le meme nombre, et la plus visible -- celle qu'on recopie dans un
  #    rapport -- etait la tronquee.
  expect_true(is.na(hstat_dec_affichage(FALSE)))
  expect_true(is.na(hstat_dec_affichage(FALSE, 7)))  # decoche : le reglage ne s'applique pas
  expect_equal(hstat_dec_affichage(TRUE, NULL), 2L)
  expect_equal(hstat_dec_affichage(TRUE, NA), 2L)
  expect_equal(hstat_dec_affichage(TRUE, 5), 5L)

  # 2. SANS ARRONDI DEMANDE, RIEN N'EST ARRONDI -- ni les colonnes numeriques,
  #    ni la chaine.
  d <- data.frame(Moyenne = c(12.3456789, 1000.5),
                  Ecart_type = c(1.23456789, 2.25),
                  Erreur_type = c(0.61728, 1.125),
                  Groupes = c("a", "b"),
                  Moyenne_pm_SD = NA_character_, stringsAsFactors = FALSE)
  sans <- round_numeric_df(d, FALSE)
  expect_equal(sans$Moyenne, d$Moyenne)
  expect_equal(sans$Ecart_type, d$Ecart_type)
  expect_equal(sans$Erreur_type, d$Erreur_type)
  expect_equal(sans$Moyenne_pm_SD[1], "12.3456789 ± 1.23456789 a")

  # 3. AVEC ARRONDI, la precision demandee s'applique aux deux a la fois.
  avec <- round_numeric_df(d, TRUE, 4)
  expect_equal(avec$Moyenne[1], 12.3457)
  expect_equal(avec$Moyenne_pm_SD[1], "12.3457 ± 1.2346 a")

  # 4. CHAQUE NOMBRE PORTE SES PROPRES CHIFFRES. `format()` sur un VECTEUR
  #    aligne tout le monde sur la meme longueur : a cote de 12,3456789
  #    l'entier 3 ressortait « 3.000000000 » et un ecart-type nul
  #    « 0.00000000 ». Ce n'est pas de la precision, c'est du bruit -- et il
  #    fait croire a une mesure au neuvieme chiffre.
  ligne <- .hstat_pm(c(12.3456789, 3), c(1.2345, 0), NULL)
  expect_equal(ligne, c("12.3456789 ± 1.2345", "3 ± 0"))

  # 5. Le formateur tient les cas degeneres : un ecart-type manquant (groupe a
  #    une seule observation) ne doit pas rendre « 12.35 ± NA » a moitie forme.
  expect_equal(.hstat_pm(1.5, NA, NULL, 2), "NA")
  expect_equal(.hstat_pm(NA, 1.5, NULL, 2), "NA")
  expect_equal(.hstat_pm(1.5, 0.25, NULL, 0), "2 ± 0")

  # 6. LE CHOIX D'ARRONDI DOIT ATTEINDRE LES CHAINES DU POST-HOC. Constate a
  #    l'ecran : case cochee a 2 decimales, la colonne « Moyenne » affichait
  #    0.03 et « Moyenne±Ecart_type », sur la MEME LIGNE,
  #    « 0.02556235 ± 0.16167049669281 ». Deux causes :
  #
  #      - le post-hoc nomme ses colonnes « Moyenne±Ecart_type », pas
  #        « Moyenne_pm_SD » : la reconstruction ne se declenchait jamais ;
  #      - la lettre de groupe y vit sous « groups » et non « Groupes », si
  #        bien qu'une reconstruction naive l'aurait perdue -- or c'est
  #        justement ce que le lecteur vient chercher.
  ph <- data.frame(
    Moyenne = c(0.02556235, 0.053072775),
    Ecart_type = c(0.16167049669281, 0.150358300574377),
    Erreur_type = c(0.02556235, 0.0237737347463618),
    groups = c("ef", "bc"),
    `Moyenne±Ecart_type` = "composee au calcul",
    `Moyenne±Erreur_type` = "composee au calcul",
    check.names = FALSE, stringsAsFactors = FALSE)

  r2 <- round_numeric_df(ph, TRUE, 2)
  expect_equal(r2$`Moyenne±Ecart_type`, c("0.03 ± 0.16 ef", "0.05 ± 0.15 bc"))
  expect_equal(r2$`Moyenne±Erreur_type`, c("0.03 ± 0.03 ef", "0.05 ± 0.02 bc"))
  # La chaine dit EXACTEMENT ce que dit la colonne numerique de la meme ligne.
  expect_true(all(mapply(function(ch, m)
    startsWith(ch, formatC(m, digits = 2, format = "f")),
    r2$`Moyenne±Ecart_type`, r2$Moyenne)))

  r4 <- round_numeric_df(ph, TRUE, 4)
  expect_equal(r4$`Moyenne±Ecart_type`[1], "0.0256 ± 0.1617 ef")
  # Decoche : la chaine repart en pleine precision, lettre comprise.
  expect_equal(round_numeric_df(ph, FALSE)$`Moyenne±Ecart_type`[1],
               "0.02556235 ± 0.16167049669281 ef")
})

test_that("les diagnostics d'ANOVA ne font pas tomber toute la sortie", {
  # Le tryCatch y enveloppe TOUTE la boucle : une seule variable a residus
  # constants emporterait l'ANOVA de toutes les autres. Shapiro leve « all 'x'
  # values are identical », et leveneTest « contrasts can be applied only to
  # factors with 2 or more levels ».
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  m <- paste(readLines(.hstat_module_path("mod_tests.R"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("stats::sd(residuals_data) > 1e-10", m, fixed = TRUE))
  expect_true(grepl("length(unique(stats::na.omit(fitted_factor))) >= 2", m, fixed = TRUE))

  # Les deux echecs sont reels : on le verifie plutot que de le supposer.
  expect_error(stats::shapiro.test(rep(3, 10)))
  expect_error(car::leveneTest(r ~ g, data = data.frame(r = rnorm(10),
                                                        g = factor(rep("A", 10)))))
})

# ===========================================================================
# UN REACTIF NE S'APPELLE PAS LUI-MEME
# ---------------------------------------------------------------------------
# Constate en ajoutant le selecteur de source du module de seuils : une
# substitution mecanique avait remplace `values$filteredData` par
# `source_data()` DANS LE CORPS de `source_data` lui-meme. R s'arrete alors sur
# « C stack usage is too close to the limit » et l'application ne demarre plus
# du tout -- une panne totale, pour une ligne.
#
# Le defaut ne se voit ni a l'analyse syntaxique ni a la lecture rapide : le
# code est parfaitement valide. Seul un balayage le rattrape.
# ===========================================================================

test_that("aucun reactif ne s'appelle lui-meme", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # Les COMMENTAIRES sont retires par l'analyseur de R, pas par une heuristique :
  # ce test s'est signale lui-meme sur le commentaire qui documente la
  # correction (« PAS `source_data()` »). Un balayage textuel naif aurait
  # produit un faux positif permanent, et on aurait fini par le desactiver.
  sans_commentaires <- function(f) {
    paste(.hstat_code_lignes(f), collapse = "\n")
  }

  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    txt <- sans_commentaires(f)
    car <- strsplit(txt, "")[[1]]
    debuts <- gregexpr("([A-Za-z_.][\\w.]*)\\s*<-\\s*(?:shiny::)?reactive\\(\\s*\\{",
                       txt, perl = TRUE)[[1]]
    if (debuts[1] == -1) next
    lg <- attr(debuts, "match.length")
    for (k in seq_along(debuts)) {
      d <- debuts[k]
      nom <- sub("\\s*<-.*$", "", substr(txt, d, d + lg[k] - 1))
      # fin du corps, par equilibrage d'accolades
      i <- d + lg[k] - 1L; prof <- 0L; fin <- NA_integer_
      while (i <= length(car)) {
        if (car[i] == "{") prof <- prof + 1L
        else if (car[i] == "}") {
          prof <- prof - 1L
          if (prof == 0L) { fin <- i; break }
        }
        i <- i + 1L
      }
      if (is.na(fin)) next
      corps <- substr(txt, d + lg[k], fin)
      if (grepl(paste0("\\b", nom, "\\s*\\("), corps, perl = TRUE))
        fautifs <- c(fautifs, paste0(basename(f), " : ", nom))
    }
  }
  expect_equal(fautifs, character(0),
               info = paste("Reactif recursif (l'application ne demarrerait pas) :",
                            paste(fautifs, collapse = " | ")))
})


# ===========================================================================
# SEUILS D'EFFICACITE : CHAQUE MODALITE COMPAREE AU TEMOIN
# ---------------------------------------------------------------------------
# Formule d'Abbott : efficacite (%) = (temoin - traitement) x 100 / temoin.
# Quatre decisions, et chacune se trompe dans un sens couteux si on l'omet.
# ===========================================================================

.hstat_essai <- function() data.frame(
  bloc = rep(c("B1", "B2", "B3"), each = 4),
  trt  = rep(c("Temoin", "T1", "T2", "T3"), 3),
  degats = c(100, 40, 20, 10,  120, 48, 30, 12,  80, 32, 16, 8),
  rdt    = c( 10, 20, 30, 40,   12, 22, 33, 44,  11, 21, 31, 41),
  stringsAsFactors = FALSE)

# ===========================================================================
#  RENDEMENT ET GAIN DE RENDEMENT
# ===========================================================================

test_that("le rendement suit la formule, quelle que soit l'unite de saisie", {
  # Rendement (kg/ha) = masse (kg) / surface (ha). Une tonne sur un hectare
  # fait mille kilogrammes a l'hectare, quelle que soit la facon de l'ecrire.
  expect_equal(as.numeric(hstat_rendement(1000, 1)), 1000)
  expect_equal(as.numeric(hstat_rendement(1, 1, "tonne (1000 kg)", "hectare (ha)")), 1000)
  # 1 000 000 g = 1000 kg ; 100 ares = 1 ha.
  expect_equal(as.numeric(hstat_rendement(1e6, 100, "gramme (g)", "are (100 m²)")), 1000)
  # Et la sortie se choisit independamment de l'entree.
  expect_equal(as.numeric(hstat_rendement(1000, 1, "kilogramme (kg)", "hectare (ha)",
                                          "tonne (1000 kg)", "hectare (ha)")), 1)
  expect_equal(as.numeric(hstat_rendement(1000, 1, "kilogramme (kg)", "hectare (ha)",
                                          "quintal (100 kg)", "hectare (ha)")), 10)

  # LE SYMBOLE SE DECLARE, IL NE SE DEVINE PAS. Le lire entre les parentheses
  # du libelle donnait « 1000 kg/ha » au lieu de « t/ha » : la parenthese porte
  # la definition, pas l'abreviation.
  expect_equal(hstat_rdt_unite_libelle(), "kg/ha")
  expect_equal(hstat_rdt_unite_libelle("tonne (1000 kg)", "hectare (ha)"), "t/ha")
  expect_equal(hstat_rdt_unite_libelle("quintal (100 kg)", "are (100 m²)"), "q/a")

  # Le symbole est accepte a l'entree comme le libelle complet : un appel ecrit
  # a la main ne doit pas echouer pour une raison que rien n'affiche.
  expect_equal(as.numeric(hstat_rendement(1000, 1, "kg", "ha")), 1000)
  expect_equal(hstat_rdt_facteur("t", HSTAT_RDT_MASSE), 1000)
  expect_true(is.na(hstat_rdt_facteur("stone", HSTAT_RDT_MASSE)))
})

test_that("une surface nulle rend le rendement indefini, jamais infini", {
  # LE DEFAUT QUE CE TEST GARDE. La division rendrait `Inf` : une valeur
  # d'apparence normale dans un tableau, et une barre demesuree dans un
  # graphique -- qui ecraserait toutes les autres a l'echelle.
  r <- hstat_rendement(c(100, 100, 100, 100), c(1, 0, -1, NA))
  expect_false(any(is.infinite(r)))
  expect_equal(as.numeric(r[1]), 100)
  expect_true(all(is.na(r[2:4])))
  # Les lignes ecartees sont COMPTEES : les taire ferait croire a un jeu
  # complet.
  expect_equal(attr(r, "invalides"), 3L)

  # Une unite inconnue ne fabrique pas un chiffre au hasard.
  u <- hstat_rendement(1000, 1, "stone")
  expect_true(all(is.na(u)))
  expect_true(nzchar(attr(u, "message")))
})

test_that("le rendement global et le rendement moyen coincident a surfaces egales", {
  d <- data.frame(
    Traitement = rep(c("T0", "T1"), each = 3),
    Masse = c(900, 1000, 1100, 1500, 1600, 1700),
    Surface = rep(1, 6), Bloc = rep(c("B1", "B2", "B3"), 2),
    stringsAsFactors = FALSE)
  r <- hstat_rdt_table(d, "Traitement", "Masse", "Surface", var_repetition = "Bloc")
  expect_equal(r$Rendement_global, r$Rendement_moyen)
  expect_equal(r$Rendement_global, c(1000, 1600))
  expect_equal(r$Repetitions, c(3L, 3L))
  expect_false(grepl("^Attention", attr(r, "message")))

  # A SURFACES INEGALES ILS DIVERGENT, et le module le dit. Le global pondere
  # chaque repetition par sa surface, la moyenne les traite a egalite : aucun
  # n'est faux, mais les confondre l'est.
  d2 <- d; d2$Surface <- c(1, 2, 3, 1, 1, 1)
  r2 <- hstat_rdt_table(d2, "Traitement", "Masse", "Surface")
  expect_false(isTRUE(all.equal(r2$Rendement_global[1], r2$Rendement_moyen[1])))
  expect_match(attr(r2, "message"), "surfaces varient")
})

test_that("le gain suit la formule et vaut zero pour le programme non traite", {
  g <- hstat_rdt_gain(c(1000, 1600, 2000), c("T0", "T1", "T2"), "T0")
  expect_equal(as.numeric(g), c(0, 60, 100))
  # LE NON TRAITE VAUT ZERO PAR DEFINITION, et on l'ecrit plutot que de s'en
  # remettre a l'arrondi de la division.
  expect_identical(as.numeric(g[1]), 0)
  expect_equal(attr(g, "reference"), 1000)

  # Un gain negatif est un RESULTAT : la modalite fait moins bien que le
  # non traite. Le borner a zero masquerait ce qu'il faut voir.
  gn <- hstat_rdt_gain(c(1000, 400), c("T0", "T1"), "T0")
  expect_equal(as.numeric(gn[2]), -60)
})

test_that("un programme non traite a rendement nul rend le gain indefini, pas infini", {
  # Division par zero : `Inf` passerait pour un gain colossal.
  g <- hstat_rdt_gain(c(0, 100, 200), c("T0", "T1", "T2"), "T0")
  expect_true(all(is.na(g)))
  expect_false(any(is.infinite(g)))
  expect_match(attr(g, "message"), "nul")

  # Reference introuvable, reference non calculable : on refuse en le disant.
  expect_match(attr(hstat_rdt_gain(c(1, 2), c("A", "B"), "T0"), "message"),
               "non trait")
  expect_true(all(is.na(hstat_rdt_gain(c(NA, 2), c("T0", "B"), "T0"))))
})

test_that("hstat_rdt_complet rend les deux rendements et les deux gains", {
  d <- data.frame(
    Traitement = rep(c("T0", "T1", "T2"), each = 2),
    Masse = c(900, 1100, 1400, 1600, 1900, 2100),
    Surface = rep(1, 6), stringsAsFactors = FALSE)
  r <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0")
  for (col in c("Rendement_global", "Rendement_moyen", "Gain_global", "Gain_moyen"))
    expect_true(col %in% names(r), info = col)
  expect_equal(r$Rendement_global, c(1000, 1500, 2000))
  expect_equal(r$Gain_global, c(0, 50, 100))
  expect_equal(r$Gain_moyen, r$Gain_global)      # surfaces egales
  expect_equal(attr(r, "unite"), "kg/ha")

  # L'unite de sortie traverse tout le calcul, gains compris (qui n'en
  # dependent pas : un pourcentage est sans unite).
  rt <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0",
                          sortie_masse = "tonne (1000 kg)")
  expect_equal(rt$Rendement_global, c(1, 1.5, 2))
  expect_equal(rt$Gain_global, c(0, 50, 100))
  expect_equal(attr(rt, "unite"), "t/ha")
})

test_that("la conversion du rendement ajoute, elle ne remplace pas", {
  d <- data.frame(Traitement = rep(c("T0", "T1"), each = 2),
                  Masse = c(900, 1100, 1500, 1700), Surface = rep(1, 4),
                  stringsAsFactors = FALSE)
  r0 <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0")
  r  <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0",
                          conv_masse = "tonne (1000 kg)", conv_surface = "hectare (ha)")

  # LA VALEUR D'ORIGINE SURVIT. L'utilisateur qui a demandé des kg/ha doit
  # continuer de les voir : la valeur convertie sert a comparer avec un bareme,
  # l'originale au controle, et l'une sans l'autre oblige a refaire le calcul.
  expect_equal(r$Rendement_global, r0$Rendement_global)
  expect_equal(r$Rendement_moyen, r0$Rendement_moyen)
  expect_true("Rendement_global_conv" %in% names(r))
  expect_equal(r$Rendement_global_conv, r$Rendement_global / 1000)
  # La colonne convertie se pose JUSTE APRES son originale : l'oeil apparie les
  # deux sans traverser le tableau.
  expect_equal(which(names(r) == "Rendement_global_conv"),
               which(names(r) == "Rendement_global") + 1L)
  # Les deux unites sont dans le tableau : un CSV exporte se lit sans le
  # tableau de bord qui l'a produit.
  expect_equal(unique(r$Unite), "kg/ha")
  expect_equal(unique(r$Unite_conv), "t/ha")
  expect_equal(attr(r, "unite_conv"), "t/ha")

  # UN GAIN NE SE CONVERTIT PAS : c'est un pourcentage, donc sans dimension.
  # Le convertir serait une faute de categorie -- le meme rapport rendrait des
  # pourcentages multiplies par mille.
  expect_false("Gain_global_conv" %in% names(r))
  expect_false("Gain_moyen_conv" %in% names(r))
  expect_equal(r$Gain_global, r0$Gain_global)

  # La conversion est une OPTION : non demandee, unite identique, ou unite
  # inconnue, le tableau ressort tel quel plutot que vide.
  for (arg in list(list(NULL, NULL), list("", ""),
                   list("kilogramme (kg)", "hectare (ha)"),
                   list("stone", "hectare (ha)"))) {
    x <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0",
                           conv_masse = arg[[1]], conv_surface = arg[[2]])
    expect_false("Rendement_global_conv" %in% names(x),
                 info = paste(arg, collapse = "/"))
    expect_equal(x$Rendement_global, r0$Rendement_global)
  }

  # La conversion part de l'unite de SORTIE, pas de celle de saisie.
  q <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0",
                         sortie_masse = "quintal (100 kg)",
                         sortie_surface = "are (100 m²)",
                         conv_masse = "tonne (1000 kg)", conv_surface = "hectare (ha)")
  expect_equal(q$Rendement_global, c(0.1, 0.16))
  expect_equal(q$Rendement_global_conv, c(1, 1.6))
})

test_that("le rendement cumulé est la somme des répétitions, et son piège est dit", {
  d <- data.frame(Traitement = rep(c("T0", "T1"), each = 3),
                  Masse = c(900, 1000, 1100, 1500, 1600, 1700),
                  Surface = rep(1, 6), stringsAsFactors = FALSE)
  r <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0")
  expect_true("Rendement_somme" %in% names(r))
  expect_equal(r$Rendement_somme, c(3000, 4800))
  expect_equal(r$Rendement_somme, r$Rendement_moyen * r$N)
  expect_true("Gain_somme" %in% names(r))
  # A nombre de lignes egal, les trois gains coincident.
  expect_equal(r$Gain_somme, r$Gain_moyen)
  expect_equal(r$Gain_somme, r$Gain_global)
  expect_false(grepl("SOMME", attr(r, "message")))

  # LA SOMME N'EST COMPARABLE QU'A NOMBRE DE LIGNES EGAL. La modalite la plus
  # repetee accumule mecaniquement davantage et ressort artificiellement plus
  # productive : un artefact de plan pris pour un resultat.
  d2 <- d[-c(2, 3), ]
  r2 <- hstat_rdt_complet(d2, "Traitement", "Masse", "Surface", "T0")
  expect_equal(round(r2$Gain_moyen[2], 2), 77.78)   # juste
  expect_equal(round(r2$Gain_somme[2], 2), 433.33)  # artefact
  expect_match(attr(r2, "message"), "SOMME")
  # La moyenne et le global, eux, restent comparables : c'est ce que dit
  # l'alerte, et c'est ce qui rend l'avertissement utile plutot qu'alarmiste.
  expect_equal(r2$Gain_moyen, r2$Gain_global)

  # La somme porte une unite, donc elle se convertit ; son gain, non.
  rc <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0",
                          conv_masse = "tonne (1000 kg)", conv_surface = "hectare (ha)")
  expect_equal(rc$Rendement_somme_conv, rc$Rendement_somme / 1000)
  expect_false("Gain_somme_conv" %in% names(rc))
})

test_that("un rendement et un gain negatifs traversent le calcul intacts", {
  # Une modalite qui fait moins bien que le non traite donne un gain NEGATIF :
  # c'est un resultat. Le borner a zero masquerait ce qu'il faut voir.
  d <- data.frame(Traitement = rep(c("T0", "T1", "T2"), each = 2),
                  Masse = c(1000, 1000, 400, 400, -50, -50),
                  Surface = rep(1, 6), stringsAsFactors = FALSE)
  r <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0")
  expect_equal(r$Rendement_global, c(1000, 400, -50))
  expect_equal(r$Gain_global, c(0, -60, -105))
  expect_true(any(r$Rendement_global < 0))
  expect_true(any(r$Gain_global < 0))
  # Rien n'est ecrete, rien n'est mis a NA.
  expect_false(anyNA(r$Gain_global))
  expect_equal(min(r$Gain_moyen), -105)
})

test_that("un gain nul est une valeur, pas une absence", {
  # LE DEFAUT QUE CE TEST GARDE. Le temoin a un gain de 0 PAR DEFINITION : sur
  # un graphique en barres, une hauteur nulle ne dessine rien, et la reference
  # de toute l'analyse disparaissait -- ce qui se lit « pas de resultat » et
  # non « egal a la reference ».
  d <- data.frame(Traitement = rep(c("T0", "T1"), each = 2),
                  Masse = c(1000, 1000, 1000, 1000), Surface = rep(1, 4),
                  stringsAsFactors = FALSE)
  r <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", "T0")
  # Tous les gains valent zero : aucun n'est NA, et le tableau garde ses lignes.
  expect_equal(r$Gain_global, c(0, 0))
  expect_false(anyNA(r$Gain_global))
  expect_equal(nrow(r), 2L)
  expect_true(all(is.finite(r$Gain_moyen)))

  # `which.max` sur des gains tous nuls designe bien une modalite : c'est
  # `any(is.finite(...))` qui doit decider, jamais une comparaison a zero.
  expect_true(any(is.finite(r$Gain_moyen)))
  expect_equal(r$Modalite[which.max(r$Gain_moyen)], "T0")

  # Le module dessine un repere pour les valeurs exactement nulles, et le
  # temoin se choisit -- il ne se devine pas.
  chemin <- .hstat_module_path("mod_yield.R")
  skip_if_not(file.exists(chemin))
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("zeros <- d[is.finite(d$.val) & d$.val == 0, , drop = FALSE]",
                    txt, fixed = TRUE))
  expect_true(grepl("geom_point(data = zeros", txt, fixed = TRUE))
  # Prendre la premiere modalite par ordre alphabetique donnerait un gain a
  # toutes les autres sans que personne ait designe la reference : les chiffres
  # seraient alors faux ET vraisemblables.
  expect_true(grepl('choices = c("(à choisir)" = "", m), selected = ""',
                    txt, fixed = TRUE))
  # Et l'axe atteint zero quand des valeurs sont negatives, sinon la base de
  # comparaison sort du champ.
  expect_true(grepl("expand_limits(y = 0)", txt, fixed = TRUE))
})

test_that("le module de rendement refuse clairement ce qu'il ne peut pas faire", {
  d <- data.frame(T = c("A", "B"), M = c(1, 2), S = c(1, 1), stringsAsFactors = FALSE)
  att <- function(x) attr(x, "message")
  expect_match(att(hstat_rdt_table(NULL, "T", "M", "S")), "Aucune donn")
  expect_match(att(hstat_rdt_table(d, "", "M", "S")), "traitements")
  expect_match(att(hstat_rdt_table(d, "T", "Absente", "S")), "masse")
  expect_match(att(hstat_rdt_table(d, "T", "M", "Absente")), "surface")
  # Une variable de repetition demandee mais introuvable etait ignoree EN
  # SILENCE dans le module des efficacites : meme piege ici, meme refus.
  expect_match(att(hstat_rdt_table(d, "T", "M", "S", var_repetition = "Zzz")),
               "introuvable")
  # Chaque refus est une phrase COMPLETE au dictionnaire : `trf()` ne traduit
  # jamais ses arguments, un gabarit « Choisissez la variable %s » rendrait une
  # phrase a moitie anglaise.
  dico <- trimws(hstat_i18n_load()$fr)
  skip_if(!length(dico), "dictionnaire introuvable")
  for (m in c("Choisissez la variable qui porte les traitements.",
              "Choisissez la variable qui porte la masse récoltée.",
              "Choisissez la variable qui porte la surface."))
    expect_true(m %in% dico, info = m)
})

test_that("les unites de rendement sont coherentes entre facteurs et symboles", {
  # Une unite ajoutee d'un cote et pas de l'autre ferait tomber le libelle
  # d'axe sur le nom complet, ou pire : chercher un facteur inexistant.
  expect_identical(names(HSTAT_RDT_MASSE), names(HSTAT_RDT_MASSE_SYM))
  expect_identical(names(HSTAT_RDT_SURFACE), names(HSTAT_RDT_SURFACE_SYM))
  expect_true(all(HSTAT_RDT_MASSE > 0))
  expect_true(all(HSTAT_RDT_SURFACE > 0))
  # Les unites de BASE valent 1 : tout le reste s'y rapporte.
  expect_equal(unname(HSTAT_RDT_MASSE[["kilogramme (kg)"]]), 1)
  expect_equal(unname(HSTAT_RDT_SURFACE[["mètre carré (m²)"]]), 1)
  # Aucun symbole en double : deux unites de meme symbole rendraient le premier
  # facteur trouve, en silence.
  expect_false(any(duplicated(HSTAT_RDT_MASSE_SYM)))
  expect_false(any(duplicated(HSTAT_RDT_SURFACE_SYM)))
  # Les conversions connues, verifiees a la main.
  expect_equal(unname(HSTAT_RDT_SURFACE[["hectare (ha)"]]), 10000)
  expect_equal(unname(HSTAT_RDT_MASSE[["quintal (100 kg)"]]), 100)
})

test_that("le module de rendement est cable de bout en bout", {
  # Un module qui n'est ni au menu, ni dans les onglets, ni appele par le
  # serveur existe dans le depot et nulle part a l'ecran.
  root <- .hstat_repo_root(); skip_if(is.na(root))
  ux  <- paste(readLines(file.path(root, "inst", "app", "UX.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  srv <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl('tabName = "yield"', ux, fixed = TRUE))
  expect_true(grepl('mod_yield_ui("yield")', ux, fixed = TRUE))
  expect_true(grepl('mod_yield_server("yield", values)', srv, fixed = TRUE))
  expect_true(file.exists(.hstat_module_path("mod_yield.R")))
})

test_that("chaque option du graphique de rendement est declaree, lue ET utilisee", {
  # TROIS CONDITIONS, PAS DEUX. Un reglage absent de l'interface est
  # inatteignable ; un reglage que le reactif ne LIT pas se change sans que
  # l'image bouge ; et un reglage lu mais jamais UTILISE ne fait rien non plus
  # -- c'est le cas le plus trompeur, puisque le code a l'air branche.
  chemin <- .hstat_module_path("mod_yield.R")
  skip_if_not(file.exists(chemin))
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ex <- parse(chemin, keep.source = FALSE)
  corps <- NULL
  v <- function(n) {
    if (!is.null(corps) || !is.call(n)) return(invisible())
    if (identical(paste(deparse(n[[1]]), collapse = ""), "<-") && is.name(n[[2]]) &&
        identical(as.character(n[[2]]), "graphique")) { corps <<- n[[3]]; return(invisible()) }
    for (i in seq_along(n)) tryCatch(v(n[[i]]), error = function(e) NULL)
  }
  for (k in seq_along(ex)) v(ex[[k]])
  expect_false(is.null(corps))
  b <- paste(deparse(corps, width.cutoff = 500), collapse = "\n")

  reglages <- c("yieldPlotType", "yieldPalette", "yieldCouleurUnique", "yieldTheme",
                "yieldAlpha", "yieldLargeur", "yieldPointSize", "yieldContour",
                "yieldContourCouleur", "yieldContourEpaisseur",
                "yieldTitre", "yieldSousTitre", "yieldLabelX", "yieldLabelY",
                "yieldLegendeTitre", "yieldTitrePos", "yieldSousTitrePos",
                "yieldTitreStyle", "yieldSousTitreStyle", "yieldAxisTitleStyle",
                "yieldAxisTextXStyle", "yieldAxisTextYStyle",
                "yieldTitreSize", "yieldSousTitreSize", "yieldAxisTitleSize",
                "yieldAxisTextSize",
                "yieldValeurs", "yieldValeursDec", "yieldValeursSize",
                "yieldValeursCouleur", "yieldValeursStyle", "yieldValeursPos",
                "yieldAngleX", "yieldAngleY", "yieldOrdre", "yieldLimites",
                "yieldYMin", "yieldYMax", "yieldPasY", "yieldGrilleMaj",
                "yieldGrilleMin", "yieldZeroMode", "yieldMasquer",
                "yieldErreurs", "yieldErreurType",
                "yieldLegendePos", "yieldLegendeTitreSize", "yieldLegendeTexteSize")
  for (r in reglages) {
    expect_true(grepl(sprintf('ns("%s")', r), txt, fixed = TRUE),
                label = paste("declare dans l'interface :", r))
    expect_true(grepl(sprintf("input$%s", r), b, fixed = TRUE),
                label = paste("lu par le reactif du graphique :", r))
  }
  # Les reglages d'EXPORT sont lus par le telechargement, pas par le graphique.
  for (r in c("yieldExportW", "yieldExportH", "yieldExportFormat", "yieldExportDPI")) {
    expect_true(grepl(sprintf('ns("%s")', r), txt, fixed = TRUE), label = r)
    expect_true(grepl(sprintf("input$%s", r), txt, fixed = TRUE), label = r)
  }
  # Un reglage declare mais masque n'existe pas.
  expect_false(grepl("display *: *none", txt))
})

test_that("l'export du rendement offre tous les formats et la resolution demandee", {
  chemin <- .hstat_module_path("mod_yield.R")
  skip_if_not(file.exists(chemin))
  lignes <- readLines(chemin, warn = FALSE, encoding = "UTF-8")
  txt <- paste(lignes, collapse = "\n")
  # Le selecteur de format ne recopie pas sa propre liste : c'est ce qui
  # laissait chaque module en inventer une plus courte.
  expect_true(grepl("hstat_format_input(ns(\"yieldExportFormat\")", txt, fixed = TRUE))
  # Sept formats, dont trois vectoriels.
  expect_gte(length(HSTAT_FORMATS_IMG), 7L)
  # Le plancher est a 300 DPI : en dessous, une figure est nette a l'ecran et
  # floue sur papier, et le defaut ne se voit qu'une fois le document remis.
  i <- grep('ns("yieldExportDPI")', lignes, fixed = TRUE)
  expect_length(i, 1L)
  m <- paste(lignes[i:(i + 1)], collapse = " ")
  expect_true(grepl("min = 300", m, fixed = TRUE))
  # Le plafond est celui de l'application, il ne se negocie pas au point
  # d'appel : `hstat_dpi_input()` n'accepte pas d'argument `max`.
  expect_false(grepl("max = ", m, fixed = TRUE))
  expect_equal(HSTAT_DPI_MAX, 20000L)
  # L'ecriture passe par le chemin commun, qui garantit un fichier valide.
  expect_true(grepl("hstat_ecrire_image(file, graphique(), fmt", txt, fixed = TRUE))
  expect_true(grepl("hstat_export_dims(input$yieldExportW", txt, fixed = TRUE))
})

test_that("l'efficacite suit la formule, sur toutes les modalites", {
  d <- .hstat_essai()
  r <- hstat_efficacite(d, "trt", "degats", "Temoin")
  expect_equal(nrow(r), 4L)                       # la boucle couvre TOUT
  expect_equal(sort(r$Modalite), c("T1", "T2", "T3", "Temoin"))

  # temoin = 100 ; T1 = 40  ->  (100 - 40) * 100 / 100 = 60
  expect_equal(r$Efficacite[r$Modalite == "T1"], 60)
  expect_equal(r$Efficacite[r$Modalite == "T2"], 78)
  expect_equal(r$Efficacite[r$Modalite == "T3"], 90)

  # LE TEMOIN VAUT ZERO PAR DEFINITION : il ne se compare pas a lui-meme.
  expect_equal(r$Efficacite[r$Modalite == "Temoin"], 0)

  # Les autres modalites, une fois le temoin choisi.
  expect_equal(hstat_eff_modalites(d, "trt", "Temoin"), c("T1", "T2", "T3"))
  expect_equal(length(hstat_eff_modalites(d, "trt")), 4L)
})

test_that("un temoin nul ne produit pas d'Inf silencieux", {
  # Diviser par zero donnerait des Inf qui ressortiraient en graphique comme
  # des barres demesurees, sans que rien ne signale l'anomalie.
  d <- .hstat_essai(); d$degats[d$trt == "Temoin"] <- 0
  r <- hstat_efficacite(d, "trt", "degats", "Temoin")
  expect_true(all(is.na(r$Efficacite[r$Modalite != "Temoin"])))
  expect_false(any(is.infinite(r$Efficacite)))
  # Et on le DIT.
  expect_true(grepl("témoin nul", attr(r, "message")))
  # Le temoin, lui, reste a 0 : c'est une definition, pas un calcul.
  expect_equal(r$Efficacite[r$Modalite == "Temoin"], 0)
})

test_that("une efficacite negative est un resultat, pas une erreur", {
  # Elle signifie que la modalite fait MOINS BIEN que le temoin. La borner a
  # zero masquerait precisement ce qu'il faut voir.
  d <- .hstat_essai(); d$degats[d$trt == "T1"] <- 200
  r <- hstat_efficacite(d, "trt", "degats", "Temoin")
  expect_lt(r$Efficacite[r$Modalite == "T1"], 0)
})

test_that("un groupe sans temoin est nomme, pas confondu avec une mesure manquante", {
  # DEUX CAUSES DIFFERENTES, DEUX MESSAGES. « Temoin sans valeur mesurable »
  # couvrait aussi le cas ou le temoin est simplement ABSENT du groupe -- un
  # defaut de PLAN, pas de mesure. Constate en groupant par une colonne qui
  # compte une modalite par ligne : l'utilisateur lisait un message qui ne
  # nommait pas sa vraie erreur.
  d <- data.frame(g = c("A", "A", "B", "B"), trt = c("Tem", "T1", "T1", "T2"),
                  y = c(10, 5, 4, 3), stringsAsFactors = FALSE)
  r <- hstat_efficacite(d, "trt", "y", "Tem", var_groupe = "g")
  expect_equal(attr(r, "groupes_sans_temoin"), "B")
  m <- attr(r, "message")
  expect_true(grepl("absent de 1 groupe", m))
  expect_true(grepl("\\bB\\b", m))
  expect_true(grepl("vérifiez", m))            # cause PUIS geste

  # Un plan sain ne declenche rien.
  ok <- data.frame(g = c("A", "A", "B", "B"), trt = c("Tem", "T1", "Tem", "T1"),
                   y = c(10, 5, 8, 4), stringsAsFactors = FALSE)
  r2 <- hstat_efficacite(ok, "trt", "y", "Tem", var_groupe = "g")
  expect_equal(length(attr(r2, "groupes_sans_temoin")), 0L)
  expect_false(grepl("absent", attr(r2, "message")))

  # `<-` ET NON `<<-` : la boucle `for` ne cree pas de cadre. `<<-` ecrirait
  # dans l'environnement ENGLOBANT et la liste resterait vide -- le miroir
  # exact du defaut corrige dans mod_tests.R, ou c'est `<<-` qu'il fallait.
  # Ce test echoue si l'operateur repart de travers.
  expect_gt(length(attr(r, "groupes_sans_temoin")), 0L)
})

test_that("le groupement rend l'efficacite analysable", {
  # Sans groupement il n'y a qu'une ligne par modalite, donc plus rien a
  # tester. Par bloc, on obtient une vraie variable.
  d <- .hstat_essai()
  sans <- hstat_efficacite(d, "trt", "degats", "Temoin")
  avec <- hstat_efficacite(d, "trt", "degats", "Temoin",
                           var_repetition = "bloc", mode = "par_repetition")
  expect_equal(nrow(sans), 4L)
  expect_equal(nrow(avec), 12L)                   # 3 blocs x 4 modalites
  expect_true("Groupe" %in% names(avec))
  expect_false("Groupe" %in% names(sans))         # colonne vide = information absente
  # Dans chaque bloc, le temoin vaut toujours zero.
  expect_true(all(avec$Efficacite[avec$Modalite == "Temoin"] == 0))
})

test_that("les deux modes de repetition ne repondent pas a la meme question", {
  d <- .hstat_essai()
  # « En commun » : les repetitions sont mises ensemble, une ligne par modalite.
  # C'est le chiffre que l'on publie.
  c1 <- hstat_efficacite(d, "trt", "degats", "Temoin", var_repetition = "bloc")
  expect_equal(nrow(c1), 4L)
  expect_equal(attr(c1, "mode"), "cumul")
  expect_equal(unique(c1$Repetitions), 3L)      # 3 blocs par modalite

  # « Par repetition » : autant de valeurs que de repetitions, donc une
  # variable analysable ensuite.
  c2 <- hstat_efficacite(d, "trt", "degats", "Temoin", var_repetition = "bloc",
                         mode = "par_repetition")
  expect_equal(nrow(c2), 12L)
  expect_true("Groupe" %in% names(c2))

  # A REPETITIONS EQUILIBREES, moyenne et somme donnent la MEME efficacite :
  # le rapport est invariant par changement d'echelle.
  moy <- hstat_efficacite(d, "trt", "degats", "Temoin", agg = "moyenne",
                          var_repetition = "bloc")
  som <- hstat_efficacite(d, "trt", "degats", "Temoin", agg = "somme",
                          var_repetition = "bloc")
  expect_equal(moy$Efficacite, som$Efficacite)
  expect_false(grepl("inégales", attr(som, "message")))
})

test_that("une somme sur des repetitions inegales est signalee", {
  # LE PIEGE. Avec un nombre de repetitions inegal, la modalite la plus
  # repetee accumule mecaniquement davantage et ressort artificiellement
  # « moins efficace » : un artefact de plan pris pour un resultat.
  d <- .hstat_essai()
  di <- d[!(d$trt == "T1" & d$bloc %in% c("B2", "B3")), ]   # T1 : 1 repetition

  som <- hstat_efficacite(di, "trt", "degats", "Temoin", agg = "somme",
                          var_repetition = "bloc")
  moy <- hstat_efficacite(di, "trt", "degats", "Temoin", agg = "moyenne",
                          var_repetition = "bloc")
  eff_som <- som$Efficacite[som$Modalite == "T1"]
  eff_moy <- moy$Efficacite[moy$Modalite == "T1"]

  # L'ecart est massif, et c'est bien la SOMME qui ment. Temoin : 3 blocs,
  # moyenne 100 et somme 300. T1 : un seul bloc, valeur 40.
  #   moyenne -> (100 - 40) / 100 = 60 %   (juste)
  #   somme   -> (300 - 40) / 300 = 86,7 % (artefact du desequilibre)
  expect_equal(round(eff_moy), 60)
  expect_equal(round(eff_som, 1), 86.7)
  expect_gt(eff_som, eff_moy + 20)

  # La somme le dit ; la moyenne n'a rien a signaler.
  expect_true(grepl("inégales", attr(som, "message")))
  expect_true(grepl("choisissez la moyenne", attr(som, "message")))
  expect_false(grepl("inégales", attr(moy, "message")))

  # Le decompte des repetitions est visible dans le tableau : c'est lui qui
  # permet a l'utilisateur de verifier le desequilibre par lui-meme.
  expect_equal(som$Repetitions[som$Modalite == "T1"], 1L)
  expect_equal(som$Repetitions[som$Modalite == "T2"], 3L)
})

test_that("une variable de repetition introuvable est refusee, pas ignoree", {
  # Elle etait ignoree EN SILENCE : l'utilisateur croyait ses repetitions
  # prises en compte alors que le calcul les melangeait. Un chiffre faux rendu
  # sans un mot est pire qu'un refus.
  d <- .hstat_essai()
  r <- hstat_efficacite(d, "trt", "degats", "Temoin", var_repetition = "zzz")
  expect_equal(nrow(r), 0L)
  expect_true(grepl("introuvable", attr(r, "message")))
  expect_true(grepl("choisissez", attr(r, "message")))
  # Ne pas declarer de repetition reste legitime.
  expect_gt(nrow(hstat_efficacite(d, "trt", "degats", "Temoin")), 0L)
  expect_gt(nrow(hstat_efficacite(d, "trt", "degats", "Temoin", var_repetition = "")), 0L)
})

test_that("l'ancien argument var_groupe garde son sens", {
  # Il decoupait le calcul par groupe. Lui donner le nouveau sens ferait passer
  # un appel existant de 12 lignes a 4, en silence.
  d <- .hstat_essai()
  o <- hstat_efficacite(d, "trt", "degats", "Temoin", var_groupe = "bloc")
  expect_equal(nrow(o), 12L)
  expect_equal(attr(o, "mode"), "par_repetition")
})

test_that("sans repetition declaree, la colonne ne s'affiche pas", {
  # Une colonne de NA ferait croire a une information absente.
  r <- hstat_efficacite(.hstat_essai(), "trt", "degats", "Temoin")
  expect_false("Repetitions" %in% names(r))
})

test_that("plusieurs variables mesurees, et les trois resumes", {
  d <- .hstat_essai()
  r <- hstat_efficacite(d, "trt", c("degats", "rdt"), "Temoin")
  expect_equal(nrow(r), 8L)
  expect_true("Variable" %in% names(r))
  # Une seule variable : la colonne n'apprend rien, elle disparait.
  expect_false("Variable" %in% names(hstat_efficacite(d, "trt", "degats", "Temoin")))

  for (a in c("moyenne", "mediane", "somme")) {
    x <- hstat_efficacite(d, "trt", "degats", "Temoin", agg = a)
    expect_equal(nrow(x), 4L, info = a)
    expect_equal(x$Efficacite[x$Modalite == "Temoin"], 0, info = a)
  }
  # La somme conserve le rapport quand les effectifs sont equilibres.
  expect_equal(hstat_efficacite(d, "trt", "degats", "Temoin", agg = "somme")$Efficacite,
               hstat_efficacite(d, "trt", "degats", "Temoin", agg = "moyenne")$Efficacite)
})

test_that("hstat_efficacite refuse clairement ce qu'elle ne peut pas faire", {
  d <- .hstat_essai()
  attendu <- function(x, motif) {
    expect_equal(nrow(x), 0L)
    expect_true(grepl(motif, attr(x, "message")), info = attr(x, "message"))
  }
  attendu(hstat_efficacite(NULL, "trt", "degats", "Temoin"), "Aucune donnée")
  attendu(hstat_efficacite(data.frame(), "trt", "degats", "Temoin"), "Aucune donnée")
  attendu(hstat_efficacite(d, "zzz", "degats", "Temoin"), "traitements")
  attendu(hstat_efficacite(d, "trt", "zzz", "Temoin"), "au moins une variable")
  attendu(hstat_efficacite(d, "trt", "degats", "Inexistant"), "n'existe pas")
  # Chaque refus nomme le geste a faire : c'est la regle des messages d'erreur.
  for (x in list(hstat_efficacite(d, "zzz", "degats", "Temoin"),
                 hstat_efficacite(d, "trt", "degats", "Inexistant")))
    expect_true(grepl("[Cc]hoisissez", attr(x, "message")))

  expect_equal(hstat_eff_modalites(NULL, "trt"), character(0))
  expect_equal(hstat_eff_modalites(d, "zzz"), character(0))
})

test_that("le module de seuils expose bien le calcul depuis un temoin", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  m <- paste(readLines(.hstat_module_path("mod_threshold.R"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("hstat_efficacite(", m, fixed = TRUE))
  expect_true(grepl("hstat_eff_modalites(", m, fixed = TRUE))
  expect_true(grepl("Calcul depuis un t", m, fixed = TRUE))
  # Le tableau doit pouvoir devenir le jeu de travail : c'est ce qui le rend
  # utilisable par les autres onglets.
  expect_true(grepl("effUseAsData", m, fixed = TRUE))
  expect_true(grepl("values$filteredData <- x", m, fixed = TRUE))
})


# ===========================================================================
# VARIABLES A VALEURS NULLES
# ---------------------------------------------------------------------------
# Une colonne dont toutes les valeurs observees valent zero a une variance
# nulle : ni correlation, ni test. Elle vient presque toujours d'un export ou
# le zero signifie « non mesure », d'ou les deux gestes offerts — corriger les
# valeurs, ou retirer la variable.
#
# Les trois cas limites ci-dessous ont ete constates a l'ecran pendant la mise
# au point, et chacun se trompait dans le sens SILENCIEUX : la colonne
# disparaissait du diagnostic au lieu d'y figurer avec le bon libelle.
# ===========================================================================

test_that("seules les colonnes reellement nulles sont listees", {
  d <- data.frame(
    tout_zero  = c(0, 0, 0, 0),
    zero_et_na = c(0, NA, 0, 0),
    texte_zero = c("0", "0,0", "0", "0"),   # un CSV livre couramment des "0"
    presque    = c(0, 0, 0, 5),
    normale    = c(1, 2, 3, 4),
    mot        = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE)
  z <- hstat_vars_zero(d)
  expect_equal(sort(z$Variable), c("texte_zero", "tout_zero", "zero_et_na"))

  # Les manquants ne comptent pas comme des zeros : la colonne les annonce a
  # cote, au lieu de melanger « mesure a zero » et « pas de mesure ».
  l <- z[z$Variable == "zero_et_na", ]
  expect_equal(l$Observations, 3L)
  expect_equal(l$Zeros, 3L)
  expect_equal(l$Manquants, 1L)
  expect_true(grepl("non mesuré", l$Constat))

  # La virgule decimale francaise est comprise.
  expect_equal(z[z$Variable == "texte_zero", ]$Zeros, 4L)
})

test_that("le seuil ouvre la liste aux variables quasi nulles", {
  d <- data.frame(presque = c(0, 0, 0, 5), tout = c(0, 0, 0, 0))
  expect_equal(hstat_vars_zero(d, seuil = 1)$Variable, "tout")
  expect_equal(sort(hstat_vars_zero(d, seuil = 0.7)$Variable), c("presque", "tout"))
  # Un seuil illisible ne doit pas faire tomber le diagnostic : on revient au
  # cas strict plutot que d'echouer.
  expect_equal(hstat_vars_zero(d, seuil = "abc")$Variable, "tout")
})

test_that("une colonne vide n'est pas une colonne de zeros, et elle est nommee", {
  # Les trois formes sous lesquelles une colonne sans valeur se presente :
  # typee LOGIQUE par les lecteurs de CSV, numerique tout-NA, ou remplie de
  # chaines vides (Excel, exports SPSS). Les trois etaient perdues.
  d <- data.frame(vide_logique = as.logical(c(NA, NA, NA)),
                  vide_num     = as.numeric(c(NA, NA, NA)),
                  vide_txt     = c("", " ", NA),
                  zero         = c(0, 0, 0),
                  stringsAsFactors = FALSE)
  z <- hstat_vars_zero(d)
  expect_equal(z$Variable, "zero")
  expect_equal(sort(attr(z, "vides")),
               c("vide_logique", "vide_num", "vide_txt"))
})

test_that("un booleen renseigne n'est pas une mesure a zero", {
  # FALSE vaut bien 0 en arithmetique, mais une colonne de « non » est une
  # reponse : la ranger ici ferait proposer d'en « corriger les valeurs ».
  d <- data.frame(drapeau = c(FALSE, FALSE, FALSE), zero = c(0, 0, 0))
  expect_equal(hstat_vars_zero(d)$Variable, "zero")
})

test_that("hstat_vars_zero encaisse les entrees degenerees", {
  for (x in list(data.frame(), NULL, "texte", 42))
    expect_silent(z <- hstat_vars_zero(x))
  expect_equal(nrow(hstat_vars_zero(data.frame())), 0L)
  expect_equal(attr(hstat_vars_zero(NULL), "vides"), character(0))
})

test_that("la saisie des valeurs refuse tout decompte qui decalerait les lignes", {
  # Une liste plus courte ou plus longue que la colonne decalerait
  # silencieusement toutes les observations : c'est pire que de refuser.
  ok <- hstat_zero_valeurs_parse("1\n2\n3\n4", 4)
  expect_true(ok$ok)
  expect_equal(ok$valeurs, c(1, 2, 3, 4))

  court <- hstat_zero_valeurs_parse("1;2;3", 4)
  expect_false(court$ok)
  expect_true(grepl("3 valeur", court$message))
  expect_true(grepl("ajustez", court$message))       # cause PUIS geste

  expect_false(hstat_zero_valeurs_parse("1\n2\n3\n4\n5", 4)$ok)

  # Un retour a la ligne final ne compte pas pour une valeur de plus.
  expect_true(hstat_zero_valeurs_parse("1\n2\n3\n4\n", 4)$ok)

  # LA VIRGULE EST UNE DECIMALE, PAS UN SEPARATEUR. Elle ne peut pas etre les
  # deux : « 2,5 » est la facon francaise d'ecrire deux et demi, et la traiter
  # en separateur en faisait deux valeurs — donc un decompte faux, donc un
  # refus incomprehensible. Separateurs : retour a la ligne et point-virgule.
  na <- hstat_zero_valeurs_parse("1; 2,5; NA; 4", 4)
  expect_true(na$ok)
  expect_equal(na$valeurs, c(1, 2.5, NA, 4))
  expect_equal(hstat_zero_valeurs_parse("0,5\n1,25", 2)$valeurs, c(0.5, 1.25))

  mauvais <- hstat_zero_valeurs_parse("1\n2\nabc\n4", 4)
  expect_false(mauvais$ok)
  expect_true(grepl("position 3", mauvais$message))
  expect_true(grepl("abc", mauvais$message))

  expect_false(hstat_zero_valeurs_parse("", 4)$ok)
  expect_false(hstat_zero_valeurs_parse("1", 0)$ok)
  expect_false(hstat_zero_valeurs_parse(NULL, 3)$ok)
})

test_that("le module de nettoyage porte bien l'etape des variables nulles", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  m <- paste(readLines(.hstat_module_path("mod_clean.R"),
                       warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("hstat_vars_zero", m, fixed = TRUE))
  expect_true(grepl("hstat_zero_valeurs_parse", m, fixed = TRUE))
  # Les quatre gestes offerts a l'utilisateur
  for (a in c('"na"', '"valeur"', '"saisie"', '"supprimer"'))
    expect_true(grepl(a, m, fixed = TRUE), info = a)
  # `transformationLog` est un registre TYPE, relu champ par champ pour
  # inverser les transformations : y deposer une phrase casserait son
  # affichage. Le geste passe par le registre d'analyses.
  expect_false(grepl("transformationLog <- c(", m, fixed = TRUE))
  expect_true(grepl("Variables à valeurs nulles", m, fixed = TRUE))
})


test_that("le <<- est bien ce qui distingue les deux cas", {
  # La semantique en cause, rendue executable : sans `<<-`, la ligne d'erreur
  # n'existe tout simplement pas dans la liste de l'appelant.
  collecte <- function(operateur) {
    acc <- list()
    for (v in c("bonne", "degeneree")) {
      tryCatch({
        if (v == "degeneree") stop("variance nulle")
        acc[[v]] <- "ok"
      }, error = function(e) {
        if (identical(operateur, "local")) acc[[v]] <- "erreur traduite"
        else acc[[v]] <<- "erreur traduite"
      })
    }
    acc
  }
  expect_equal(names(collecte("local")), "bonne")          # la ligne est perdue
  expect_equal(names(collecte("englobant")), c("bonne", "degeneree"))
})

test_that("aucun gestionnaire d'erreur ne jette la ligne qu'il vient de batir", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  fichiers <- .hstat_sources_app()
  fautifs <- character(0)

  for (f in fichiers) {
    src <- readLines(f, warn = FALSE, encoding = "UTF-8")
    txt <- paste(src, collapse = "\n")
    debuts <- gregexpr("(error|warning)\\s*=\\s*function\\s*\\([^)]*\\)\\s*\\{",
                       txt)[[1]]
    if (debuts[1] == -1) next
    car <- strsplit(txt, "")[[1]]
    for (d in debuts) {
      # fin du corps du gestionnaire, par equilibrage d'accolades
      i <- d + attr(debuts, "match.length")[which(debuts == d)] - 1L
      prof <- 0L; fin <- NA_integer_
      while (i <= length(car)) {
        if (car[i] == "{") prof <- prof + 1L
        else if (car[i] == "}") {
          prof <- prof - 1L
          if (prof == 0L) { fin <- i; break }
        }
        i <- i + 1L
      }
      if (is.na(fin)) next
      corps <- substr(txt, d, fin)
      cibles <- regmatches(corps, gregexpr(
        "(?m)^\\s*[A-Za-z_.][\\w.]*\\s*\\[\\[[^]]*\\]\\]\\s*<-(?!-)", corps,
        perl = TRUE))[[1]]
      for (cible in cibles) {
        nom <- sub("^\\s*([A-Za-z_.][\\w.]*).*$", "\\1", cible, perl = TRUE)
        # Defini DANS le gestionnaire (donc local et renvoye) : cas legitime.
        pose <- grepl(paste0("(?m)^\\s*", nom, "\\s*<-[^-]"), corps, perl = TRUE)
        # `values` et consorts sont des objets a REFERENCE (reactiveValues) :
        # y ecrire depuis un gestionnaire a bien un effet au dehors.
        if (!pose && !nom %in% c("values", "session", "input"))
          fautifs <- c(fautifs, paste0(basename(f), " : ", nom, "[[...]] <-"))
      }
    }
  }

  expect_equal(fautifs, character(0),
               info = paste("Affectation perdue dans un gestionnaire (utiliser <<-) :",
                            paste(unique(fautifs), collapse = " | ")))
})


# ===========================================================================
# INTERPRETATIONS TRADUITES
# ---------------------------------------------------------------------------
# Les phrases FIGEES passent par le dictionnaire, comme le reste de
# l'interface. Les phrases COMPOSEES (sprintf avec un effectif, un nom de
# variable) n'y entrent pas : le traducteur ne remplace que des chaines
# entieres. Elles demandent une reecriture par gabarit — chantier distinct,
# non entrepris ici, et ce test dit ou l'on en est plutot que de le masquer.
# ===========================================================================

test_that("les analyses recommandees sont nommees en anglais", {
  for (a in c("Test t de Student (deux échantillons)", "ANOVA à un facteur",
              "Corrélation de Pearson", "Régression linéaire",
              "Test du chi-deux d'indépendance", "Comparaisons post-hoc",
              "Aucune analyse possible en l'état")) {
    t <- tr(a, "en")
    expect_false(identical(t, a), info = a)
    expect_true(nzchar(t))
  }
  expect_equal(tr("ANOVA à un facteur", "en"), "One-way ANOVA")
  expect_equal(tr("Test du chi-deux d'indépendance", "en"),
               "Chi-square test of independence")
})

test_that("les conditions et alternatives des recommandations sont traduites", {
  for (x in c("Indépendance des observations ; normalité dans chaque groupe.",
              "Effectifs attendus >= 5 dans au moins 80 % des cases.",
              "Test exact de Fisher.",
              "Corrélation de Spearman, qui ne suppose que la monotonie."))
    expect_false(identical(tr(x, "en"), x), info = substr(x, 1, 40))
  # La ponctuation anglaise ne garde pas l'espace avant le point-virgule
  expect_false(grepl(" ;", tr("Indépendance des observations ; normalité dans chaque groupe.",
                              "en"), fixed = TRUE))
})

test_that("les suggestions de qualite les plus frequentes sont traduites", {
  for (x in c("Ces valeurs font échouer la plupart des calculs. Les remplacer ou les retirer dans l'onglet Nettoyage.",
              "Variable quasi vide : l'exclure des analyses, ou retrouver la source des données manquantes.",
              "Convertir en numerique (onglet Nettoyage). En l'etat, moyennes, correlations et tests quantitatifs sont impossibles."))
    expect_false(identical(tr(x, "en"), x), info = substr(x, 1, 40))
})

test_that("le dictionnaire couvre desormais plus que la seule navigation", {
  d <- hstat_i18n_load()
  expect_gt(nrow(d), 300L)
  # Les trois familles doivent y etre representees : interface, messages
  # d'erreur, interpretations.
  expect_true("Tests statistiques" %in% d$fr)                 # interface
  expect_true("message R" %in% d$fr)                          # erreurs
  expect_true("ANOVA à un facteur" %in% d$fr)                 # interpretations
  # Meme plafond, meme source : voir HSTAT_I18N_KO_MAX dans le socle.
  expect_lt(nchar(hstat_i18n_json("en")) / 1024, HSTAT_I18N_KO_MAX)
})

test_that("les sous-ensembles de lignes du module graphique gardent leur tableau", {
  # Le jeu prepare par plotData() ne contient QUE les colonnes utiles : une
  # seule quand X et Y designent la meme variable, ou quand l'agregation a
  # renomme Y. Un `df[cond, ]` sans drop = FALSE le ramenait alors a un
  # vecteur, et ggplot rendait « dim(data) must return an <integer> of
  # length 2 » -- message que personne ne peut relier a son choix de
  # variables. Constate a l'ecran.
  d1 <- data.frame(a = c(1, NA, 3))
  expect_false(is.data.frame(d1[!is.na(d1$a), ]))              # le piege
  expect_true(is.data.frame(d1[!is.na(d1$a), , drop = FALSE])) # le remede

  src <- readLines(.hstat_module_path("mod_viz.R"),
                   warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]
  # Aucune indexation de lignes de mod_viz.R ne doit laisser la simplification
  # par defaut : un `[..., ]` nu est signale, y compris sur une matrice ou la
  # chute vers un vecteur est un piege plus grand encore.
  fautifs <- grep(",\\s*\\]", code, value = TRUE)
  expect_equal(length(fautifs), 0L,
               info = paste(trimws(fautifs), collapse = "\n"))

  # Et le garde-fou nomme le cas restant plutot que de laisser passer le
  # message de ggplot.
  expect_true(any(grepl("is.data.frame(data)", code, fixed = TRUE)))
})

test_that("les pixels d'export fixent la mise en page, le DPI la finesse", {
  d <- hstat_export_dims(1200, 800, 300)
  # 1200 px lus a 96 ppp = 12,5 pouces -- et non 4 pouces (1200 / 300)
  expect_equal(d$width_in, 12.5)
  expect_equal(round(d$height_in, 2), 8.33)
  expect_equal(d$dpi, 300)
  expect_equal(d$width_out, 3750)

  # Le defaut corrige : monter le DPI RETRECISSAIT la figure, donc demander
  # plus de qualite la rendait moins lisible. La mise en page doit desormais
  # etre insensible au DPI, et les pixels produits croitre avec lui.
  for (dpi in c(72, 150, 300, 600, 1200)) {
    x <- hstat_export_dims(1200, 800, dpi)
    expect_equal(x$width_in, 12.5, info = paste("dpi", dpi))
  }
  expect_gt(hstat_export_dims(1200, 800, 600)$width_out,
            hstat_export_dims(1200, 800, 300)$width_out)

  # Au-dela du bitmap tenable, le DPI est abaisse ET annonce
  gros <- hstat_export_dims(4000, 3000, 20000)
  expect_lt(gros$dpi, 20000)
  expect_lte(max(gros$width_out, gros$height_out), HSTAT_EXPORT_MAX_PX)
  expect_true(is.character(gros$note) && nzchar(gros$note))
  expect_null(hstat_export_dims(1200, 800, 300)$note)

  # Saisie vide ou absurde : on retombe sur des valeurs utilisables plutot que
  # de faire echouer le telechargement
  for (mauvais in list(NULL, NA, "", 0, -5))
    expect_gte(hstat_export_dims(mauvais, mauvais, mauvais)$width_in, 1)
})

test_that("ggsave rend bien les pixels annonces", {
  skip_if_not_installed("ggplot2")
  d <- hstat_export_dims(1200, 800, 150)
  f <- tempfile(fileext = ".png")
  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  ok <- tryCatch({
    suppressWarnings(ggplot2::ggsave(f, p, width = d$width_in,
                                     height = d$height_in, dpi = d$dpi))
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(ok, "ggsave indisponible dans cet environnement")
  # Les pixels REELLEMENT produits, lus dans l'en-tete du PNG : un ggsave qui
  # ignorerait la resolution passerait autrement inapercu.
  px <- .hstat_png_dims(f)
  expect_equal(unname(px[["largeur"]]), d$width_out)
  expect_equal(unname(px[["hauteur"]]), d$height_out)
})

test_that("le style des etiquettes survit a la conversion plotly", {
  # plotmath ne survit pas a ggplotly : l'axe affichait bold(\"...\") en
  # toutes lettres. plotly comprend un sous-ensemble de HTML.
  expect_equal(hstat_html_style_label("A", "bold"), "<b>A</b>")
  expect_equal(hstat_html_style_label("A", "italic"), "<i>A</i>")
  expect_equal(hstat_html_style_label("A", "bolditalic"), "<b><i>A</i></b>")
  expect_equal(hstat_html_style_label("A", "plain"), "A")
  # Un nom de traitement porte des caracteres qui casseraient la balise
  expect_false(grepl("&(?!amp;)", hstat_html_style_label("2SP(0,5)&2PV", "bold"),
                     perl = TRUE))
  expect_true(grepl("<b>", hstat_html_style_label("2SP(0,5)&2PV", "bold"), fixed = TRUE))
  # Vectorise, style par niveau
  expect_equal(hstat_html_style_label(c("A", "B"), c("bold", "plain")),
               c("<b>A</b>", "B"))
})

test_that("les Y d'un multi-courbes partagent un type, et l'axe X en est exclu", {
  d <- data.frame(Semaine = as.Date("2026-01-01") + 0:4,
                  ch_Hel = as.numeric(1:5), ch_Bis = as.numeric(5:1),
                  note = letters[1:5], stringsAsFactors = FALSE)

  # Le cas signale : la date choisie en X ET en Y levait
  # « Can't combine `Semaine` <date> and `ch_Hel` <double> »
  r <- hstat_y_multi_valides(d, c("Semaine", "ch_Hel", "ch_Bis"), "Semaine")
  expect_setequal(r$gardees, c("ch_Hel", "ch_Bis"))
  expect_equal(r$ecartees, "Semaine")
  # et le pivot qui echouait passe desormais
  expect_silent(tidyr::pivot_longer(d[, c("Semaine", r$gardees)],
                                    cols = dplyr::any_of(r$gardees),
                                    names_to = "Variable", values_to = "Value"))

  # Types melanges : le quantitatif l'emporte, le reste est NOMME
  r2 <- hstat_y_multi_valides(d, c("ch_Hel", "note"), "Semaine")
  expect_equal(r2$gardees, "ch_Hel")
  expect_equal(r2$ecartees, "note")
  expect_true(nzchar(r2$motif))

  # Homogenes : rien n'est retire
  expect_setequal(hstat_y_multi_valides(d, c("ch_Hel", "ch_Bis"), "Semaine")$gardees,
                  c("ch_Hel", "ch_Bis"))
  # Une seule variable, qui est l'axe X : plus rien a tracer, dit clairement
  expect_length(hstat_y_multi_valides(d, "Semaine", "Semaine")$gardees, 0L)
  # Colonne inexistante ou entree vide
  expect_length(hstat_y_multi_valides(d, "absente", "Semaine")$gardees, 0L)
  expect_length(hstat_y_multi_valides(d, character(0), "Semaine")$gardees, 0L)
})

test_that("chaque variable mesuree recoit sa colonne d'efficacite", {
  set.seed(3)
  d <- data.frame(
    Modalite = rep(c("Temoin", "A", "B"), each = 6),
    Bloc     = rep(1:2, 9),
    v1 = c(runif(6, 8, 10), runif(6, 4, 6),  runif(6, 2, 4)),
    v2 = c(runif(6, 20, 22), runif(6, 15, 17), runif(6, 10, 12)),
    stringsAsFactors = FALSE)

  long <- hstat_efficacite(d, "Modalite", c("v1", "v2"), "Temoin")
  expect_equal(nrow(long), 6L)                 # 3 modalites x 2 variables
  large <- hstat_eff_large(long)

  # Une ligne par modalite, une colonne d'efficacite par variable mesuree :
  # sans cela le selecteur « Variable Y » n'avait qu'un seul choix et le
  # graphique superposait toutes les variables sur les memes positions.
  expect_equal(nrow(large), 3L)
  expect_true(all(c("Efficacite_v1", "Efficacite_v2") %in% names(large)))
  expect_equal(attr(large, "colonnes_efficacite"),
               c("Efficacite_v1", "Efficacite_v2"))

  # Les valeurs sont celles du tableau detaille, pas un recalcul
  for (v in c("v1", "v2")) {
    ref <- long$Efficacite[long$Variable == v][order(long$Modalite[long$Variable == v])]
    obt <- large[[paste0("Efficacite_", v)]][order(large$Modalite)]
    expect_equal(obt, ref, info = v)
  }
  # Le temoin vaut zero pour chaque variable
  expect_true(all(unlist(large[large$Modalite == "Temoin",
                               c("Efficacite_v1", "Efficacite_v2")]) == 0))
  # Les attributs du calcul survivent : l'interface les affiche
  expect_equal(attr(large, "temoin"), "Temoin")
  expect_equal(attr(large, "agg"), "moyenne")

  # Le prefixe distingue le pourcentage de la mesure d'origine : une colonne
  # nommee « v1 » contiendrait des pourcentages et se confondrait avec elle.
  expect_false("v1" %in% names(large))

  # Une seule variable : le tableau est deja large, il n'est pas touche
  seul <- hstat_efficacite(d, "Modalite", "v1", "Temoin")
  expect_identical(names(hstat_eff_large(seul)), names(seul))

  # Par repetition : la cle de groupe est conservee, une ligne par couple
  parrep <- hstat_efficacite(d, "Modalite", c("v1", "v2"), "Temoin",
                             var_repetition = "Bloc", mode = "par_repetition")
  lp <- hstat_eff_large(parrep)
  expect_true("Groupe" %in% names(lp))
  expect_equal(nrow(lp), 6L)                   # 3 modalites x 2 blocs
  expect_true(all(c("Efficacite_v1", "Efficacite_v2") %in% names(lp)))

  # Entrees degenerees : rien ne casse
  expect_null(hstat_eff_large(NULL))
  expect_equal(nrow(hstat_eff_large(long[0, , drop = FALSE])), 0L)
})

test_that("le panneau d'options post-hoc expose toutes ses mises en forme", {
  root <- .hstat_repo_root()
  src  <- readLines(.hstat_module_path("mod_tests.R"), warn = FALSE)
  txt  <- paste(src, collapse = "\n")

  # Treize reglages etaient declares dans un bloc `display:none` : ils
  # existaient dans le code, agissaient sur le graphique, et l'utilisateur ne
  # pouvait pas les atteindre. Signale a l'ecran.
  expect_false(grepl("display *: *none", txt))

  # Tout reglage lu par le serveur doit exister dans l'interface -- sinon il
  # retombe en silence sur sa valeur par defaut.
  reglages <- c("plotWidth", "plotHeight", "xAxisMin", "xAxisMax",
                "subtitleSize", "subtitleFontStyle", "subtitlePosition",
                "axisTextXFontStyle", "axisTextYFontStyle",
                "legendTitleFontStyle", "legendTextFontStyle",
                "legendKeySize", "posthocTheme")
  for (r in reglages) {
    expect_true(grepl(sprintf('ns("%s")', r), txt, fixed = TRUE),
                label = paste("declare dans l'interface :", r))
    expect_true(grepl(sprintf("input$%s", r), txt, fixed = TRUE),
                label = paste("lu par le serveur :", r))
  }
})

test_that("chaque option du graphique post-hoc est declaree, lue ET utilisee", {
  # TROIS CONDITIONS, PAS DEUX. Un reglage absent de l'interface est
  # inatteignable ; un reglage que le reactif ne LIT pas se change sans que
  # l'image bouge ; et un reglage lu mais jamais UTILISE ne fait rien non plus
  # -- c'est le cas le plus trompeur, puisque le code a l'air branche.
  # `legendKeySize` etait exactement dans ce cas avant sa correction.
  root <- .hstat_repo_root(); skip_if(is.na(root))
  chemin <- .hstat_module_path("mod_tests.R")
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ex <- parse(chemin, keep.source = FALSE)
  corps <- NULL
  v <- function(n) {
    if (!is.null(corps) || !is.call(n)) return(invisible())
    if (identical(paste(deparse(n[[1]]), collapse = ""), "<-") && is.name(n[[2]]) &&
        identical(as.character(n[[2]]), "create_posthoc_plot")) {
      corps <<- n[[3]]; return(invisible())
    }
    for (i in seq_along(n)) tryCatch(v(n[[i]]), error = function(e) NULL)
  }
  for (k in seq_along(ex)) v(ex[[k]])
  expect_false(is.null(corps))
  b <- paste(deparse(corps, width.cutoff = 500), collapse = "\n")

  paires <- c(
    xLabelAngle = "x_label_angle", yLabelAngle = "y_label_angle",
    titlePosition = "title_position", legendPosition = "legend_position",
    plotAlpha = "plot_alpha", showBorder = "show_border",
    borderColor = "border_color", borderWidth = "border_width",
    geomWidth = "geom_width", pointSize = "point_size",
    errorBarWidth = "error_bar_width", errorBarColor = "error_bar_color",
    letterColor = "letter_color", showMeanValues = "show_mean_values",
    meanValueDecimals = "mean_value_decimals", meanValueColor = "mean_value_color",
    meanValueFontStyle = "mean_value_font_style",
    showGridMajor = "show_grid_major", showGridMinor = "show_grid_minor",
    showAxisLines = "show_axis_lines", axisLineColor = "axis_line_color",
    axisLineWidth = "axis_line_width", axisTicksMarks = "axis_ticks_marks")
  for (nm in names(paires)) {
    loc <- paires[[nm]]
    expect_true(grepl(sprintf('ns("%s")', nm), txt, fixed = TRUE),
                label = paste("declare dans l'interface :", nm))
    expect_true(grepl(sprintf("input$%s", nm), b, fixed = TRUE),
                label = paste("lu par le reactif du graphique :", nm))
    # Deux occurrences au moins : l'affectation, et au moins un usage.
    n_u <- if (grepl(loc, b, fixed = TRUE))
      length(gregexpr(paste0("\\b", loc, "\\b"), b, perl = TRUE)[[1]]) else 0L
    expect_gte(n_u, 2L, label = paste("utilise par le graphique :", nm))
  }

  # Les valeurs figees que ces reglages remplacent ont bien disparu.
  expect_false(grepl("alpha = 0.7", txt, fixed = TRUE))
  expect_false(grepl('color = "red")', txt, fixed = TRUE))
  expect_false(grepl('width = 0.2, color = "black"', txt, fixed = TRUE))
  expect_false(grepl("round(Moyenne, 2)", txt, fixed = TRUE))
})

test_that("hstat_etiquettes_x_style rend du plotmath, et laisse « plain » en texte", {
  # LE STYLE PAR NIVEAU SE POSE EN PLOTMATH, seule forme que ggsave rende dans
  # un fichier. Mais « plain » doit rester une CHAINE : `plain()` de plotmath
  # rend le texte dans la police mathematique, ou l'espace disparait et la
  # parenthese se decale -- un nom de traitement en ressortirait deforme alors
  # que l'utilisateur n'a demande aucun style.
  r <- hstat_etiquettes_x_style(c("T0", "T1", "T2", "T3"),
                                c("plain", "bold", "italic", "bold.italic"))
  expect_length(r, 4L)
  expect_true(is.character(r[[1]]))
  expect_identical(r[[1]], "T0")
  # Les trois autres sont des APPELS plotmath, et c'est la fonction appelee
  # qu'il faut verifier : `is.call` seul passerait sur n'importe quel appel.
  expect_true(is.call(r[[2]])); expect_identical(as.character(r[[2]][[1]]), "bold")
  expect_true(is.call(r[[3]])); expect_identical(as.character(r[[3]][[1]]), "italic")
  expect_true(is.call(r[[4]])); expect_identical(as.character(r[[4]][[1]]), "bolditalic")
  # Le texte porte est bien celui du niveau, pas celui d'un voisin.
  expect_identical(r[[2]][[2]], "T1")
  expect_identical(r[[4]][[2]], "T3")

  # « bolditalic » et « bold.italic » nomment le meme style : deux orthographes
  # pour un seul rendu, dont l'une retomberait sinon en silence sur « plain ».
  expect_identical(as.character(hstat_etiquettes_x_style("A", "bolditalic")[[1]][[1]]),
                   "bolditalic")
  # Un style inconnu ne fait pas tomber le graphique : il ne style pas.
  expect_identical(hstat_etiquettes_x_style("A", "souligne")[[1]], "A")
  # Le recyclage : un seul style pour tous les niveaux.
  expect_length(hstat_etiquettes_x_style(c("A", "B", "C"), "bold"), 3L)
  expect_identical(hstat_etiquettes_x_style(character(0), "bold"), list())
})

test_that("hstat_html_style_label connait les deux orthographes du gras italique", {
  # Le meme style porte deux noms dans le depot : « bold.italic » dans
  # HSTAT_FONT_STYLES (le nom de ggplot2), « bolditalic » dans le plotmath. Le
  # rendu interactif lit le PREMIER : ne connaitre que le second rendait le
  # niveau en texte nu, sans un mot, alors que l'export sortait bien en gras
  # italique -- deux images differentes pour un meme reglage.
  expect_identical(hstat_html_style_label("T1", "bold.italic"), "<b><i>T1</i></b>")
  expect_identical(hstat_html_style_label("T1", "bolditalic"), "<b><i>T1</i></b>")
  expect_identical(hstat_html_style_label("T1", "bold"), "<b>T1</b>")
  expect_identical(hstat_html_style_label("T1", "plain"), "T1")
  # Le texte reste echappe : un « & » dans un nom de traitement casserait la
  # balise que plotly interprete.
  expect_false(grepl("&amp;", hstat_html_style_label("2SP&2PV", "plain"), fixed = TRUE) &&
               grepl("<", hstat_html_style_label("2SP&2PV", "plain"), fixed = TRUE))
  expect_true(grepl("&", hstat_html_style_label("2SP&2PV", "bold"), fixed = TRUE))
})

test_that("le chi2 d'adequation vit dans la colonne des tests parametriques", {
  skip_if_not_installed("shinydashboard")
  # Il vivait dans une COLONNE A LUI, posee apres la rangee des trois familles
  # de tests : il retombait donc sous la premiere colonne (les reglages), pas
  # sous les tests parametriques. La mesure porte sur l'ARBRE de l'interface,
  # pas sur l'ordre des lignes du fichier : une accolade deplacee changerait
  # l'imbrication sans changer l'ordre.
  # Les aiguillages d'interface (`withSpinner`, `plotlyOutput`...) vivent dans
  # l'environnement global : sans eux, l'UI du module ne se construit pas.
  suppressMessages(hstat_installer_replis_ui())
  ui <- mod_tests_ui("tests")
  colonnes <- list()
  ids_sous <- function(x) {
    if (inherits(x, "shiny.tag")) {
      out <- character(0)
      if (!is.null(x$attribs$id)) out <- c(out, as.character(x$attribs$id))
      for (k in x$children) out <- c(out, ids_sous(k))
      cls <- x$attribs$class
      if (!is.null(cls) && any(grepl("col-sm-", as.character(cls), fixed = TRUE)))
        colonnes[[length(colonnes) + 1L]] <<- out
      return(out)
    }
    if (is.list(x)) {
      out <- character(0)
      for (k in x) out <- c(out, ids_sous(k))
      return(out)
    }
    character(0)
  }
  tous <- ids_sous(ui)
  expect_true(all(c("tests-testANOVA", "tests-runChiSqTest",
                    "tests-testKruskal") %in% tous))
  # UNE colonne porte les tests parametriques ET le chi2, sans porter les
  # non parametriques : c'est la definition de « sous les tests parametriques ».
  # Sans la seconde condition, la boite entiere (elle aussi une colonne, de
  # largeur 12) satisferait la premiere quelle que soit la disposition.
  ok <- vapply(colonnes, function(s)
    all(c("tests-testANOVA", "tests-runChiSqTest") %in% s) &&
      !("tests-testKruskal" %in% s), logical(1))
  expect_true(any(ok))
})

test_that("l'etiquette et le style de l'axe X passent par une seule echelle", {
  # DEUX `scale_x_discrete()` NE S'AJOUTENT PAS : le second REMPLACE le premier
  # en avertissant. Poser le style dans une echelle et les noms choisis dans
  # une autre effacerait donc l'un des deux, selon l'ordre -- et l'utilisateur
  # verrait son reglage rester sans effet.
  root <- .hstat_repo_root(); skip_if(is.na(root))
  chemin <- .hstat_module_path("mod_tests.R")
  ex <- parse(chemin, keep.source = FALSE)
  corps <- NULL
  v <- function(n) {
    if (!is.null(corps) || !is.call(n)) return(invisible())
    if (identical(paste(deparse(n[[1]]), collapse = ""), "<-") && is.name(n[[2]]) &&
        identical(as.character(n[[2]]), "create_posthoc_plot")) {
      corps <<- n[[3]]; return(invisible())
    }
    for (i in seq_along(n)) tryCatch(v(n[[i]]), error = function(e) NULL)
  }
  for (k in seq_along(ex)) v(ex[[k]])
  expect_false(is.null(corps))
  b <- paste(deparse(corps, width.cutoff = 500), collapse = "\n")
  expect_equal(length(gregexpr("scale_x_discrete", b, fixed = TRUE)[[1]]), 1L)
  # Et cette echelle passe bien par l'aide de style : sans elle, le gras
  # demande par niveau ne serait jamais pose.
  expect_true(grepl("hstat_etiquettes_x_style", b, fixed = TRUE))

  # Le rendu interactif rejoue les memes styles en HTML : ggplotly DEPARSE le
  # plotmath, l'axe afficherait `bold("T1")` en toutes lettres.
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("hstat_html_style_label", txt, fixed = TRUE))
  expect_true(grepl('attr(p, "hstat_x_styles")', txt, fixed = TRUE))
})

test_that("le bouton LMM impose la gaussienne, quels que soient les selecteurs", {
  # UN MODELE LINEAIRE MIXTE EST LE MIXTE GAUSSIEN A LIEN IDENTITE. Il vivait
  # derriere « Modèle (généralisé) mixte » : il fallait savoir que la famille
  # par defaut le donnait, et un selecteur laisse sur « Poisson » d'une analyse
  # precedente rendait autre chose sous le nom qu'on venait de demander.
  #
  # Le test porte donc sur ce que le bouton AJUSTE, pas sur sa presence : les
  # selecteurs de famille et de lien sont poses a des valeurs FAUSSES pour un
  # LMM, et le modele doit sortir gaussien a lien identite quand meme.
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("lme4")
  set.seed(42)
  bloc <- rep(paste0("B", 1:4), each = 15)
  trt  <- rep(rep(c("T0", "T1", "T2", "T3", "T4"), each = 3), 4)
  eb <- stats::setNames(stats::rnorm(4, 0, 1.5), paste0("B", 1:4))
  et <- c(T0 = 0, T1 = 1.2, T2 = 2, T3 = 2.4, T4 = 0.6)
  df <- data.frame(Bloc = bloc, Traitement = trt,
                   Rendement = 5 + eb[bloc] + et[trt] + stats::rnorm(60, 0, 0.8),
                   stringsAsFactors = FALSE)
  suppressMessages(hstat_installer_replis_ui())
  vals <- do.call(shiny::reactiveValues, hstat_valeurs_initiales())
  vals$data <- df; vals$filteredData <- df

  shiny::testServer(mod_tests_server, args = list(values = vals), {
    session$setInputs(responseVar = "Rendement", factorVar = "Traitement",
                      glmmEngine = "lme4",
                      glmmFamily = "poisson",   # faux pour un LMM, exprès
                      glmmLink   = "log",       # faux pour un LMM, exprès
                      glmmRandom = "Bloc", glmmRandomCustom = "",
                      interaction = FALSE)
    session$setInputs(testLMM = 1)
    r <- values$testResultsDF
    expect_false(is.null(r))
    expect_true(nrow(r) >= 2L)
    # C'est la VALEUR qui est verifiee, pas le libelle : un modele de Poisson
    # etiquete « LMM » passerait un controle qui ne lirait que la colonne Test.
    m <- values$currentModel
    expect_identical(stats::family(m)$family, "gaussian")
    expect_identical(stats::family(m)$link, "identity")
    # L'effet aleatoire demande est bien dans le modele ajuste.
    expect_true(grepl("(1 | Bloc)", paste(deparse(stats::formula(m)), collapse = ""),
                      fixed = TRUE))
    expect_setequal(unique(r$Test), "LMM")
    # lmerTest fournit des ddl de Satterthwaite : sans eux il n'y a pas de
    # p-value, et une ligne de LMM sans p-value ne se lit pas.
    skip_if_not_installed("lmerTest")
    expect_true(any(is.finite(r$p_value)))
    expect_true(any(is.finite(suppressWarnings(as.numeric(r$ddl)))))
  })
})

test_that("les deux modeles mixtes partagent un seul ajustement", {
  # Deux copies du meme ajustement divergeraient a la premiere correction --
  # c'est la lecon des deux tests t, ou `.run_ttest(var_equal)` porte les deux.
  root <- .hstat_repo_root(); skip_if(is.na(root))
  chemin <- .hstat_module_path("mod_tests.R")
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl(".run_mixed <- function(", txt, fixed = TRUE))
  expect_true(grepl("input$testGLMM, .run_mixed(lineaire = FALSE)", txt, fixed = TRUE))
  expect_true(grepl("input$testLMM,  .run_mixed(lineaire = TRUE)", txt, fixed = TRUE))
  # Les deux boutons existent dans l'interface, et un seul appel a lmer y
  # repond : un second `lme4::lmer(` hors de l'ANOVA a mesures repetees
  # signalerait la copie que ce test existe pour empecher.
  expect_true(grepl('ns("testLMM")', txt, fixed = TRUE))
  expect_true(grepl('ns("testGLMM")', txt, fixed = TRUE))
})

test_that("la note du post-hoc protege a bien disparu de l'interface", {
  # Retiree a la demande : elle tenait dix lignes sous une case a cocher, et
  # detaillait un comportement (quelles methodes separent sous une ANOVA non
  # significative) qui releve de la documentation, pas du panneau de reglages.
  # La CASE, elle, reste : c'est elle qui protege les lettres.
  chemin <- .hstat_module_path("mod_tests.R")
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Sans diff", txt, fixed = TRUE))
  expect_false(grepl("Waller-Duncan s", txt, fixed = TRUE))
  expect_true(grepl('ns("multiProtege")', txt, fixed = TRUE))
  # Et sa cle quitte le dictionnaire avec elle : une entree qui ne peut plus
  # rien traduire se lit comme une traduction manquante.
  root <- .hstat_repo_root(); skip_if(is.na(root))
  csv <- file.path(root, "inst", "app", "i18n", "fr-en.csv")
  skip_if_not(file.exists(csv))
  dico <- paste(readLines(csv, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Waller-Duncan s", dico, fixed = TRUE))
})

test_that("le trait des axes ne s'impose ni ne s'efface", {
  # Decoche, le reglage ne doit RIEN poser : un theme qui trace ses axes de
  # lui-meme (« classique ») garde les siens, la ou un `element_blank()` les
  # effacerait sans que personne l'ait demande.
  chemin <- .hstat_module_path("mod_tests.R")
  lignes <- readLines(chemin, warn = FALSE, encoding = "UTF-8")
  txt <- paste(lignes, collapse = "\n")
  expect_true(grepl("axis.line", txt, fixed = TRUE))
  expect_false(grepl("axis.line = ggplot2::element_blank()", txt, fixed = TRUE))
  # La couleur par defaut est le NOIR : c'est le reglage attendu d'une figure
  # publiee, et le demander a chaque fois serait une friction inutile.
  i <- grep('ns("axisLineColor")', lignes, fixed = TRUE)
  expect_length(i, 1L)
  expect_true(grepl('value = "#000000"', lignes[i], fixed = TRUE))
  # L'epaisseur va jusqu'a 3 : « gras » n'est pas un oui/non, c'est un trait
  # dont on choisit la force.
  j <- grep('ns("axisLineWidth")', lignes, fixed = TRUE)
  expect_length(j, 1L)
  m <- paste(lignes[j:(j + 1)], collapse = " ")
  expect_true(grepl("max = 3", m, fixed = TRUE))
})

test_that("l'inclinaison des libelles X couvre tout l'intervalle 0-90 degres", {
  # La case « inclines a 45 degres » ne laissait le choix qu'entre 0 et 45 :
  # onze noms de traitement se chevauchent encore a 45, et une etiquette
  # courte n'a aucune raison d'etre penchee.
  chemin <- .hstat_module_path("mod_tests.R")
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("rotateXLabels", txt, fixed = TRUE))
  # L'appel tient sur deux lignes : on prend la declaration ET la suivante,
  # plutot qu'une regex gourmande qui s'arreterait sur le `)` de `ns(...)`.
  lignes <- readLines(chemin, warn = FALSE, encoding = "UTF-8")
  for (nm in c("xLabelAngle", "yLabelAngle")) {
    i <- grep(sprintf('ns("%s")', nm), lignes, fixed = TRUE)
    expect_length(i, 1L)
    m <- paste(lignes[i:(i + 1)], collapse = " ")
    expect_true(grepl("sliderInput", m, fixed = TRUE), label = nm)
    expect_true(grepl("min = 0", m, fixed = TRUE), label = nm)
    expect_true(grepl("max = 90", m, fixed = TRUE), label = nm)
  }
})

test_that("les options du graphique post-hoc forment une boite a part entiere", {
  # Elles vivaient repliees DANS l'onglet « Graphique », au bas de la colonne
  # de resultats : quarante reglages dans une colonne etroite, qu'il fallait
  # deplier a chaque fois.
  chemin <- .hstat_module_path("mod_tests.R")
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Une vraie boite de tableau de bord, sur toute la largeur.
  lignes <- readLines(chemin, warn = FALSE, encoding = "UTF-8")
  i <- grep(" Options du graphique", lignes, fixed = TRUE)
  expect_length(i, 1L)
  bloc <- paste(lignes[max(1, i - 4):(i + 4)], collapse = "\n")
  expect_true(grepl("shinydashboard::box(", bloc, fixed = TRUE))
  expect_true(grepl("width = 12", bloc, fixed = TRUE))
  expect_true(grepl("collapsible = TRUE", bloc, fixed = TRUE))
  # Et elle vient APRES la configuration de l'analyse
  expect_gt(i, grep("Configuration de l'analyse", lignes)[1])
  # L'ancien panneau replie a bien disparu
  expect_false(grepl("graphOptionsPanel", txt, fixed = TRUE))
})

test_that("chaque palette proposee existe vraiment chez RColorBrewer", {
  # scale_fill_brewer() leve une erreur sur un nom inconnu : une faute de
  # frappe dans la liste de choix ferait tomber TOUT le graphique, et
  # seulement pour l'utilisateur qui aurait choisi cette entree.
  skip_if_not_installed("RColorBrewer")
  connues <- rownames(RColorBrewer::brewer.pal.info)
  for (p in c(unname(HSTAT_PALETTES_QUALI), unname(HSTAT_PALETTES_DEGRADE)))
    expect_true(p %in% connues, label = p)
  # Les qualitatives sont bien qualitatives : un degrade sur des groupes sans
  # ordre naturel suggere une progression qui n'existe pas.
  for (p in unname(HSTAT_PALETTES_QUALI))
    expect_equal(as.character(RColorBrewer::brewer.pal.info[p, "category"]),
                 "qual", info = p)
  expect_false(any(unname(HSTAT_PALETTES_DEGRADE) %in% unname(HSTAT_PALETTES_QUALI)))
})

test_that("les themes proposes sont tous rendus par viz_get_theme", {
  # Une entree de plus dans la liste sans le `switch` correspondant
  # retomberait en silence sur « minimal » : l'utilisateur choisirait un theme
  # et n'en verrait aucun changement.
  skip_if_not_installed("ggplot2")
  ref <- viz_get_theme("minimal")
  for (v in setdiff(unname(HSTAT_THEMES_GG), "minimal"))
    expect_false(identical(viz_get_theme(v), ref), label = v)
  expect_true(all(nzchar(names(HSTAT_THEMES_GG))))
  # Les styles d'ecriture sont ceux que ggplot accepte pour `face`
  expect_setequal(unname(HSTAT_FONT_STYLES),
                  c("plain", "bold", "italic", "bold.italic"))
})

test_that("l'habillage d'une barre ne fabrique pas de contour qu'on n'a pas demande", {
  s <- hstat_barre_style()
  expect_equal(s$alpha, 0.8)
  # `colour = NA` n'est pas equivalent a l'absence d'argument : il EFFACE le
  # contour. Sans contour demande, la cle ne doit donc pas exister du tout.
  expect_null(s$colour)
  expect_null(s$linewidth)

  c1 <- hstat_barre_style(0.5, TRUE, "#000000", 1.2)
  expect_equal(c1$alpha, 0.5)
  expect_equal(c1$colour, "#000000")
  expect_equal(c1$linewidth, 1.2)

  # Saisies impossibles : on retombe sur des valeurs utilisables plutot que de
  # faire tomber le graphique entier
  for (mauvais in list(NULL, NA, "", 0, -1, 5))
    expect_equal(hstat_barre_style(mauvais)$alpha, 0.8)
  expect_equal(hstat_barre_style(0.7, TRUE, "#123456", 0)$linewidth, 0.5)
  expect_equal(hstat_barre_style(0.7, TRUE, "", 1)$colour, "#2c3e50")
})

test_that("l'etiquette de valeur se place selon le signe de l'efficacite", {
  y <- c(60, -20, 0)

  # Une efficacite negative -- la modalite fait moins bien que le temoin --
  # descend sous l'axe : un vjust fige ecrirait son etiquette du mauvais cote.
  d <- hstat_valeur_pos(y, "dessus")
  expect_equal(d$y, y)
  expect_lt(d$vjust[1], 0)          # barre positive : au-dessus du sommet
  expect_gt(d$vjust[2], 0)          # barre negative : sous le creux
  expect_equal(d$vjust[1], d$vjust[3])   # zero se traite comme un positif

  dd <- hstat_valeur_pos(y, "dedans")
  expect_equal(dd$y, y)
  expect_gt(dd$vjust[1], 0)         # a l'interieur, donc de l'autre cote
  expect_lt(dd$vjust[2], 0)

  # Au pied : l'ordonnee est ZERO, pas la valeur -- c'est ce qui garde
  # l'etiquette visible quand la barre sort du cadre fixe par l'axe.
  pd <- hstat_valeur_pos(y, "pied")
  expect_equal(pd$y, c(0, 0, 0))
  expect_equal(pd$vjust, d$vjust)

  # Valeurs manquantes : pas d'erreur, et le NA est traite comme un positif
  expect_length(hstat_valeur_pos(c(NA, 1), "dessus")$vjust, 2L)
  expect_error(hstat_valeur_pos(y, "ailleurs"))
})

test_that("le module des seuils expose une mise en forme complete", {
  root <- .hstat_repo_root()
  txt  <- paste(readLines(.hstat_module_path("mod_threshold.R"),
                          warn = FALSE), collapse = "\n")

  # Les familles de mise en forme qui manquaient : theme, sous-titre, style et
  # position du titre, style des graduations, valeurs portees sur les barres,
  # opacite et contour, pas des graduations Y, etiquette du seuil, taille du
  # texte de legende distincte de celle du titre.
  reglages <- c("thresholdTheme", "thresholdSubtitle", "thresholdTitleStyle",
                "thresholdTitlePosition", "thresholdSubtitleStyle",
                "thresholdSubtitlePosition", "thresholdAxisTextXStyle",
                "thresholdAxisTextYStyle", "thresholdShowValues",
                "thresholdValueDigits", "thresholdValueSize",
                "thresholdValueStyle", "thresholdValuePosition",
                "thresholdValueColor", "thresholdBarAlpha", "thresholdBarBorder",
                "thresholdBarBorderColor", "thresholdBarBorderWidth",
                "thresholdLegendTextSize", "thresholdValueLabelSize",
                "thresholdYBreakStep", "thresholdShowLabel",
                "thresholdLabelPos", "thresholdLabelStyle")
  for (r in reglages) {
    expect_true(grepl(sprintf('ns("%s")', r), txt, fixed = TRUE),
                label = paste("declare :", r))
    expect_true(grepl(sprintf("input$%s", r), txt, fixed = TRUE),
                label = paste("lu :", r))
    # Un reglage que le reactif du graphique n'observe pas ne redessine rien :
    # l'utilisateur le change et l'image ne bouge pas.
    expect_true(grepl(sprintf("input$%s\n", r), txt, fixed = TRUE) ||
                grepl(sprintf("input$%s\r\n", r), txt, fixed = TRUE),
                label = paste("observe par le reactif :", r))
  }
  expect_false(grepl("display *: *none", txt))
  # L'opacite etait figee a 0,8 aux six endroits ou les barres sont tracees
  expect_false(grepl("alpha = 0.8", txt, fixed = TRUE))
})

test_that("le sous-titre est remis dans le titre plotly", {
  # ggplotly laisse simplement TOMBER le sous-titre. Sans reinjection,
  # l'utilisateur en saisissait un, ne voyait rien a l'ecran, et le retrouvait
  # dans le fichier telecharge -- le pire des deux mondes.
  txt <- paste(readLines(.hstat_module_path("mod_threshold.R"), warn = FALSE),
               collapse = "\n")
  i_gg  <- regexpr("ggplotly(", txt, fixed = TRUE)
  i_sub <- regexpr("<br><sup>", txt, fixed = TRUE)
  expect_gt(i_sub, 0)
  expect_gt(i_sub, i_gg)          # apres la conversion, pas avant
  # Le texte vient de l'utilisateur : il est echappe avant d'entrer dans la
  # balise, comme les etiquettes d'axe.
  expect_true(grepl("hstat_html_escape(sous)", txt, fixed = TRUE))
})

test_that("les alignements de titre sont des hjust valides", {
  v <- suppressWarnings(as.numeric(HSTAT_ALIGNEMENTS))
  expect_false(any(is.na(v)))
  expect_true(all(v >= 0 & v <= 1))
  expect_true("0.5" %in% unname(HSTAT_ALIGNEMENTS))
})

test_that("les pixels d'export suivent la resolution a taille physique constante", {
  # Modele des analyses multivariees : les champs de largeur/hauteur sont
  # RECALCULES a chaque changement de DPI, ils affichent donc les pixels
  # reellement produits. pouces = pixels / DPI est alors exact.
  expect_equal(hstat_px_apres_dpi(3000, 300, 600), 6000)
  expect_equal(hstat_px_apres_dpi(3000, 300, 150), 1500)
  expect_equal(hstat_px_apres_dpi(3000, 300, 300), 3000)

  # La taille physique est l'invariant : elle ne bouge pas d'un DPI a l'autre.
  for (dpi in c(72, 150, 300, 600)) {
    px <- hstat_px_apres_dpi(3000, 300, dpi)
    expect_equal(round(hstat_px_en_pouces(px, dpi), 6), 10)
  }
  # Bornage : un bitmap demesure ferait echouer l'export
  expect_lte(hstat_px_apres_dpi(19000, 72, 1200), HSTAT_EXPORT_MAX_PX)
  # Entrees inutilisables : on ne propose rien plutot qu'un chiffre invente
  for (x in list(NULL, NA, 0, -1, ""))
    expect_null(hstat_px_apres_dpi(x, 300, 600))
  expect_null(hstat_px_apres_dpi(3000, 0, 600))
  expect_null(hstat_px_apres_dpi(3000, 300, NA))

  # Pouces : repli explicite plutot qu'une valeur absurde
  expect_equal(hstat_px_en_pouces(3000, 300), 10)
  expect_equal(hstat_px_en_pouces(NULL, 300, defaut = 8), 8)
  expect_equal(hstat_px_en_pouces(3000, NA, defaut = 8), 8)
  expect_gte(hstat_px_en_pouces(10, 300), 1)          # jamais moins d'un pouce
})

test_that("les pixels se recalculent depuis la taille physique, pas depuis les precedents", {
  # La taille physique est l'etat ; les pixels n'en sont que l'affichage.
  expect_equal(hstat_px_pour_dpi(10, 300), 3000)
  expect_equal(hstat_px_pour_dpi(10, 600), 6000)
  expect_equal(hstat_px_pour_dpi(7.5, 1200), 9000)

  # Aller-retour : la taille physique survit a n'importe quelle resolution.
  for (dpi in c(72, 96, 150, 300, 600, 1200)) {
    px <- hstat_px_pour_dpi(10, dpi)
    expect_equal(round(hstat_px_en_pouces(px, dpi), 6), 10)
  }

  # Et surtout : DEUX changements d'affilee donnent le meme resultat qu'un
  # seul. C'est ce que la chaine « pixels precedents x neuf / ancien » ne
  # garantissait pas -- il lui fallait l'aller-retour du navigateur entre les
  # deux, faute de quoi elle repartait de pixels perimes.
  direct <- hstat_px_pour_dpi(10, 1200)
  perime <- hstat_px_apres_dpi(hstat_px_apres_dpi(3000, 300, 600), 600, 1200)
  expect_equal(direct, perime)                      # chemin nominal : identiques
  chaine_decrochee <- hstat_px_apres_dpi(3000, 600, 1200)   # pixels pas encore revenus
  expect_false(identical(direct, chaine_decrochee)) # la chaine, elle, se trompe
  expect_equal(hstat_px_pour_dpi(10, 1200), direct) # la taille physique, jamais

  # Bornage et entrees inutilisables
  expect_lte(hstat_px_pour_dpi(100, 1200), HSTAT_EXPORT_MAX_PX)
  for (x in list(NULL, NA, 0, -1, "")) expect_null(hstat_px_pour_dpi(x, 300))
  expect_null(hstat_px_pour_dpi(10, 0))
  expect_null(hstat_px_pour_dpi(10, NA))
})

test_that("la liaison DPI part de la taille physique et l'annonce", {
  srv <- paste(readLines(file.path(.hstat_repo_root(), "inst", "app", "app_server.R"),
                         warn = FALSE), collapse = "\n")
  ux  <- paste(readLines(file.path(.hstat_repo_root(), "inst", "app", "UX.R"),
                         warn = FALSE), collapse = "\n")

  # L'ancre en pouces remplace le chainage : le DPI precedent ne doit plus
  # servir a recalculer quoi que ce soit.
  expect_true(grepl(".mv_pouces", srv, fixed = TRUE))
  expect_false(grepl(".mv_dpi_prec", srv, fixed = TRUE))
  expect_true(grepl("hstat_px_pour_dpi", srv, fixed = TRUE))

  # Nos propres ecritures ne doivent pas redefinir la taille physique : sans
  # ce garde-fou, un echo arrive en retard divise d'anciens pixels par la
  # resolution deja changee, et la figure retrecit a chaque cran.
  expect_true(grepl(".mv_ecrit", srv, fixed = TRUE))

  # L'export lit la taille physique retenue, pas les champs : le fichier fait
  # pouces x DPI meme si l'affichage n'a pas suivi.
  expect_true(grepl("mv_pouces_export", srv, fixed = TRUE))

  # Et la note dit ce que le fichier contiendra, dans les 23 blocs d'export.
  historiques <- c("pcaPlot", "pcaScree", "pcaParallel", "pcaCTR",
                   "hcpcCluster", "hcpcDend", "hcpcHeights", "afdInd", "afdVar")
  for (p in historiques)
    expect_true(grepl(sprintf('hstat_mv_dim_note_ui("%s")', p), ux, fixed = TRUE),
                label = paste("note de taille :", p))
  expect_true(grepl("hstat_mv_dim_note_ui(prefix)", srv, fixed = TRUE))
  expect_true(grepl('output[[paste0(pfx, "_dimnote")]]', srv, fixed = TRUE))
})

test_that("les graphiques multivaries partent des reglages de ggplot2", {
  # « S'afficher initialement avec les configurations d'origine de ggplot2 » :
  # les valeurs de depart doivent etre celles de ggplot2, pas des tailles
  # maison qu'il faudrait defaire.
  expect_equal(HSTAT_GG_POINT_SIZE, 1.5)     # geom_point
  expect_equal(HSTAT_GG_LINEWIDTH, 0.5)      # geom_line / segment
  expect_equal(HSTAT_GG_BASE_SIZE, 11)       # theme_grey(base_size = )
  expect_equal(HSTAT_GG_LABEL_PT, 11)        # geom_text ~ 3,88 mm

  root <- .hstat_repo_root()
  srv  <- paste(readLines(file.path(root, "inst", "app", "app_server.R"), warn = FALSE),
                collapse = "\n")
  ux   <- paste(readLines(file.path(root, "inst", "app", "UX.R"), warn = FALSE),
                collapse = "\n")

  # Le theme etait impose en dur a chaque graphique factoriel : le rendu
  # d'origine de ggplot2 etait alors inatteignable. (Les commentaires en
  # parlent encore, on ne balaie que le code.)
  code <- readLines(file.path(root, "inst", "app", "app_server.R"), warn = FALSE)
  code <- paste(code[!grepl("^\\s*#", code)], collapse = "\n")
  expect_false(grepl("ggtheme = theme_minimal()", code, fixed = TRUE))
  expect_true(grepl("mv_ggtheme(", code, fixed = TRUE))

  # Aucune taille maison ne subsiste comme valeur de depart
  for (motif in c("value = 2.4", "value = 0.7,"))
    expect_false(grepl(motif, srv, fixed = TRUE), label = motif)

  # Et le helper de telechargement mort a disparu : il portait un calcul de
  # dimensions different de celui reellement employe.
  expect_false(grepl("createPlotDownloadHandler <- function", srv, fixed = TRUE))
  expect_false(grepl("calculate_dimensions_from_dpi", srv, fixed = TRUE))
  expect_false(grepl("calculate_dimensions_from_dpi", ux, fixed = TRUE))
})

test_that("chaque export multivarie a sa liaison DPI et ses reglages de forme", {
  root <- .hstat_repo_root()
  srv  <- paste(readLines(file.path(root, "inst", "app", "app_server.R"), warn = FALSE),
                collapse = "\n")
  ux   <- paste(readLines(file.path(root, "inst", "app", "UX.R"), warn = FALSE),
                collapse = "\n")
  tout <- paste(srv, ux)

  generiques <- paste0("mv_", c("kmeans","efa","cfa","mtmm","pls","regmult","afc","mca",
                                "kmodes","lca","logit","famd","mfa","kproto"))
  historiques <- c("pcaPlot", "pcaScree", "pcaParallel", "pcaCTR",
                   "hcpcCluster", "hcpcDend", "hcpcHeights", "afdInd", "afdVar")

  # « Sans exception » : la liste qui alimente l'observateur doit contenir les
  # 23 exports. Une analyse ajoutee sans y figurer se comporterait autrement
  # que ses voisines, et c'est precisement ce qu'on ne veut plus.
  for (p in c(generiques, historiques))
    expect_true(grepl(sprintf('"%s"', p), srv, fixed = TRUE), label = paste("liee au DPI :", p))
  expect_true(grepl("MV_EXPORTS_DPI", srv, fixed = TRUE))
  expect_true(grepl("mv_lier_dpi", srv, fixed = TRUE))

  # Les analyses historiques recoivent le bloc de forme manquant (theme,
  # sous-titre, legende, grille).
  for (p in historiques)
    expect_true(grepl(sprintf('hstat_mv_forme_ui("%s"', p), ux, fixed = TRUE),
                label = paste("forme :", p))

  # Les generiques passent par la boite commune, qui porte les memes reglages.
  expect_true(grepl("mv_forme_box(prefix)", srv, fixed = TRUE))
  for (suffixe in c("_title", "_subtitle", "_xlab", "_ylab", "_theme",
                    "_legendpos", "_grid", "_width", "_height", "_dpi"))
    expect_true(grepl(sprintf('paste0(prefix, "%s")', suffixe), srv, fixed = TRUE),
                label = suffixe)

  # La largeur et la hauteur demandees doivent etre LUES : l'export sortait
  # toujours carre, neuf pouces de cote, quels que soient les champs.
  expect_false(grepl("size_in <- 9", srv, fixed = TRUE))
  expect_true(grepl("hstat_px_en_pouces", srv, fixed = TRUE))
})

test_that("un telechargement d'image ecrit toujours un fichier valide", {
  skip_if_not_installed("ggplot2")
  # Un `content =` qui leve, ou qui se termine sans avoir ecrit, fait renvoyer
  # a Shiny sa page d'erreur HTML : le navigateur l'enregistre sous le nom
  # demande, et l'on croit tenir un PNG. Signale a l'ecran.
  entete <- function(f, n = 8) readBin(f, "raw", n)
  est_png <- function(f) identical(as.integer(entete(f)[1:4]), c(137L, 80L, 78L, 71L))
  est_pdf <- function(f) identical(rawToChar(entete(f, 4)), "%PDF")

  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  f1 <- tempfile(fileext = ".png")
  expect_true(hstat_ecrire_image(f1, p, "png", 6, 4, 100))
  expect_true(est_png(f1))

  # Graphique absent : un fichier VALIDE portant le motif, jamais du HTML
  f2 <- tempfile(fileext = ".png")
  expect_false(hstat_ecrire_image(f2, NULL, "png", 6, 4, 100))
  expect_true(est_png(f2))
  expect_gt(file.size(f2), 1000)

  # Le trace qui leve en cours de route laisse malgre tout un fichier lisible
  f3 <- tempfile(fileext = ".pdf")
  expect_false(hstat_ecrire_image(f3, function() stop("boum"), "pdf", 6, 4, 100))
  expect_true(est_pdf(f3))

  # Le format demande est respecte, alias compris
  f4 <- tempfile(fileext = ".jpg")
  expect_true(hstat_ecrire_image(f4, p, "jpg", 5, 4, 100))
  expect_equal(as.integer(entete(f4, 2)), c(255L, 216L))     # marqueur JPEG

  # Normalisation : c'est l'extension qui decide du type MIME servi par Shiny
  expect_equal(hstat_img_fmt("JPG"), "jpeg")
  expect_equal(hstat_img_fmt("TIF"), "tiff")
  expect_equal(hstat_img_fmt("html"), "png")   # jamais du HTML
  expect_equal(hstat_img_fmt(NULL), "png")
  expect_equal(hstat_img_mime("svg"), "image/svg+xml")
  expect_equal(hstat_img_mime("inconnu"), "image/png")
})

test_that("les ellipses ne sont demandees que sur des groupes qui peuvent en porter", {
  set.seed(4)
  # Trois causes d'echec de stat_conf_ellipse(), toutes rencontrees :
  x <- c(stats::rnorm(12), stats::rnorm(12, 4))
  y <- c(stats::rnorm(12), stats::rnorm(12, 4))
  g <- rep(c("A", "B"), each = 12)
  expect_true(hstat_ellipse_ok(g, x, y)$ok)
  expect_null(hstat_ellipse_ok(g, x, y)$motif)

  # 1. groupe trop petit
  g2 <- c(rep("A", 22), rep("B", 2))
  r2 <- hstat_ellipse_ok(g2, x, y)
  expect_false(r2$ok)
  expect_equal(r2$faibles, "B")
  expect_true(grepl("B", r2$motif, fixed = TRUE))     # le groupe est NOMME

  # 2. coordonnee constante
  r3 <- hstat_ellipse_ok(g, c(stats::rnorm(12), rep(1, 12)),
                            c(stats::rnorm(12), rep(2, 12)))
  expect_false(r3$ok)
  expect_equal(r3$faibles, "B")

  # 3. points parfaitement alignes : covariance singuliere
  r4 <- hstat_ellipse_ok(g, c(stats::rnorm(12), 1:12),
                            c(stats::rnorm(12), 2 * (1:12)))
  expect_false(r4$ok)
  expect_equal(r4$faibles, "B")

  # Les groupes sains restent utilisables : on n'ecarte que ce qui echouerait
  expect_equal(r2$groupes, "A")

  # Entrees degenerees : pas d'erreur, pas d'ellipse
  expect_false(hstat_ellipse_ok(NULL, NULL, NULL)$ok)
  expect_false(hstat_ellipse_ok("A", 1, 1)$ok)
  expect_false(hstat_ellipse_ok(g, rep(NA_real_, 24), y)$ok)
})

test_that("aucun telechargement d'image ne peut se terminer sans ecrire", {
  root <- .hstat_repo_root()
  # `calculate_dimensions_from_dpi()` etait appelee dans mod_descriptive.R
  # alors qu'elle vivait dans le corps de `server` : jamais visible depuis un
  # module. L'appel levait, et le « PNG » telecharge etait la page d'erreur
  # HTML de Shiny.
  src <- unlist(lapply(.hstat_sources_app(), readLines, warn = FALSE))
  code <- src[!grepl("^\\s*#", src)]
  expect_false(any(grepl("calculate_dimensions_from_dpi", code, fixed = TRUE)))
  # Le chemin d'ecriture garanti est bien celui employe
  expect_true(sum(grepl("hstat_ecrire_image", code, fixed = TRUE)) >= 15)
})

test_that("un seul endroit dans l'application ouvre un peripherique graphique", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # L'INVARIANT : `.hstat_img_device()` est le SEUL ouvreur de peripherique, et
  # `ggsave` n'est plus appele nulle part. Tout ce que l'ecrivain commun
  # garantit -- plafond de resolution, format valide, image de secours portant
  # le motif -- ne profite qu'aux exports qui passent par lui ; un module qui
  # ouvre son propre peripherique se prive de tout, en silence.
  #
  # Douze ecritures brutes vivaient hors de l'ecrivain : deux dans le rapport,
  # cinq dans les tests statistiques, une dans le module qualitatif, une dans la
  # visualisation, et le kit d'export partage lui-meme.
  ouvreurs <- c("ggsave(", "grDevices::png(", "grDevices::jpeg(",
                "grDevices::tiff(", "grDevices::bmp(", "grDevices::pdf(",
                "grDevices::postscript(", "grDevices::svg(", "cairo_pdf",
                "svglite::svglite(")
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    lignes <- .hstat_code_lignes(f)
    # Fenetre autorisee : le corps de `.hstat_img_device` dans Utils.R.
    permis <- integer(0)
    deb <- grep(".hstat_img_device <- function", lignes, fixed = TRUE)
    if (length(deb)) permis <- seq(deb[1], min(length(lignes), deb[1] + 40L))
    for (m in ouvreurs) {
      hit <- setdiff(grep(m, lignes, fixed = TRUE), permis)
      if (length(hit))
        fautifs <- c(fautifs, sprintf("%s:%d (%s)", basename(f), hit, m))
    }
  }
  expect_equal(fautifs, character(0))
})

test_that("aucun bouton de telechargement n'est branche dans le vide", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # C'EST LE PRIX DU CONTRAT PAR PREFIXE. Tant que chaque export declarait son
  # `output$<identifiant>`, une faute de frappe se voyait a la lecture. Avec un
  # prefixe, l'identifiant est CONSTRUIT (`<prefixe>Xlsx`) : une lettre de
  # travers debranche le bouton en silence -- il reste affiche, il ne fait
  # rien, et rien dans le code ne le signale.
  #
  # Le test refait donc la construction des deux cotes et compare.
  src <- paste(vapply(.hstat_sources_app(),
                      function(f) paste(.hstat_code_lignes(f), collapse = "\n"),
                      character(1)), collapse = "\n")
  ext <- function(motif) {
    m <- regmatches(src, gregexpr(motif, src, perl = TRUE))[[1]]
    if (!length(m)) character(0) else unique(sub(motif, "\\1", m, perl = TRUE))
  }
  tb_ui <- ext('hstat_export_table_ui\\(ns, "([A-Za-z0-9_.]+)"')
  boutons <- unique(c(
    ext('download(?:Button|Link)\\(ns\\("([A-Za-z0-9_.]+)"'),
    ext('download(?:Button|Link)\\("([A-Za-z0-9_.]+)"'),
    paste0(ext('hstat_export_plot_ui\\(ns, "([A-Za-z0-9_.]+)"'), "Dl"),
    paste0(tb_ui, "Csv"), paste0(tb_ui, "Xlsx")))
  kit_tb <- ext('hstat_export_tables?_handlers\\(output, "([A-Za-z0-9_.]+)"')
  producteurs <- unique(c(
    ext('output\\$([A-Za-z0-9_.]+) *<-'),
    ext('output\\[\\["([A-Za-z0-9_.]+)"\\]\\] *<-'),
    # L'AFFECTATION FAIT PARTIE DU MOTIF, ET C'EST TOUT LE SUJET.
    # `hstat_export_plot_handler()` REND un `downloadHandler` : appele nu, il
    # le construit puis le jette, et le bouton reste affiche sans rien
    # produire. Un motif qui s'arretait a l'appel comptait donc un producteur
    # la ou il n'y en avait aucun -- exactement le defaut qu'il devait voir,
    # et il l'a laisse passer sur `epiP`. Le `output$X <-` est exige.
    paste0(ext('output\\$[A-Za-z0-9_.]+ *<- *hstat_export_plot_handler\\(input, "([A-Za-z0-9_.]+)"'), "Dl"),
    paste0(kit_tb, "Csv"), paste0(kit_tb, "Xlsx")))

  # Les identifiants construits en boucle (`paste0("mv_", key, ...)`) ne sont
  # litteraux d'aucun cote : ils sortent des deux listes a la fois, donc le
  # test ne les invente pas -- il ne les couvre simplement pas.
  expect_gt(length(boutons), 100L)
  expect_equal(setdiff(boutons, producteurs), character(0))
})

test_that("les tableaux exportes passent par un ecrivain unique", {
  skip_if_not_installed("openxlsx")
  # Meme regle que pour les images : tout chemin ecrit un fichier VALIDE. Un
  # `req()` ou un `return(NULL)` sans ecriture fait renvoyer a Shiny sa page
  # d'erreur HTML, qu'Excel refuse ensuite d'ouvrir sans dire pourquoi.
  tb <- list("Valeurs propres" = data.frame(axe = 1:3, val = c(2.1, 1.2, 0.7)),
             "Un nom de feuille beaucoup trop long pour Excel" = data.frame(x = 1:2))
  f <- tempfile(fileext = ".xlsx")
  expect_equal(hstat_ecrire_classeur(f, tb), 2L)
  # Le nom de feuille vient parfois d'une VARIABLE DE L'UTILISATEUR :
  # `addWorksheet()` leve au-dela de 31 caracteres et sur []:*?/\\ .
  noms <- openxlsx::getSheetNames(f)
  expect_true(all(nchar(noms) <= 31))
  expect_false(any(grepl("[\\[\\]:*?/\\\\]", noms, perl = TRUE)))
  expect_equal(hstat_feuille_nom("a[b]:c*d?e/f"), "a_b__c_d_e_f")
  expect_equal(hstat_feuille_nom(""), "Feuille")

  # Deux noms identiques une fois tronques ne doivent pas faire echouer
  # l'export entier : `addWorksheet()` refuse le doublon.
  long <- paste0("Comparaisons multiples par variable ", c("A", "B"))
  f2 <- tempfile(fileext = ".xlsx")
  expect_equal(hstat_ecrire_classeur(f2, stats::setNames(
    list(data.frame(x = 1), data.frame(x = 2)), long)), 2L)
  expect_equal(length(openxlsx::getSheetNames(f2)), 2L)

  # Une liste vide donne un classeur qui PORTE LE MOTIF, jamais rien.
  vide <- .hstat_tables_ou_motif(function() NULL, "Test")
  expect_equal(names(vide), "Info")
  f3 <- tempfile(fileext = ".xlsx")
  expect_equal(hstat_ecrire_classeur(f3, vide), 1L)
  expect_gt(file.size(f3), 0)

  # Les tableaux absents ou vides sont ecartes : une feuille vide fait croire
  # a une information manquante.
  expect_equal(names(hstat_tables_non_vides(
    list(a = data.frame(x = 1), b = NULL, c = data.frame(x = numeric(0))))), "a")
})

test_that("un prefixe declare bien les deux sorties attendues", {
  skip_if_not_installed("shiny")
  # Le balayage du depot verifie que les NOMS concordent ; celui-ci verifie que
  # la mecanique les produit vraiment.
  #
  # Le faux `output` est un ENVIRONNEMENT, pas une liste : le `output` de Shiny
  # est un objet a reference, et une liste passee a une fonction y serait copiee
  # -- le test ne verrait alors jamais ce que le kit a depose.
  faux <- new.env()
  hstat_export_tables_handlers(faux, "monExport",
                               function() list(A = data.frame(x = 1)), "essai")
  expect_setequal(ls(faux), c("monExportXlsx", "monExportCsv"))
  expect_true(all(vapply(ls(faux), function(n) is.function(faux[[n]]), logical(1))))

  faux2 <- new.env()
  hstat_export_table_handlers(faux2, "autre", function() data.frame(x = 1), "essai")
  expect_setequal(ls(faux2), c("autreCsv", "autreXlsx"))

  # L'extension suit le nombre de tableaux : un `.zip` contenant un CSV nu ne
  # s'ouvre pas comme l'utilisateur s'y attend, et livrer un seul CSV quand il y
  # a plusieurs tableaux en perdrait. C'est la longueur de la liste qui tranche.
  expect_equal(length(.hstat_tables_ou_motif(
    function() list(A = data.frame(x = 1)), "essai")), 1L)
  expect_equal(length(.hstat_tables_ou_motif(
    function() list(A = data.frame(x = 1), B = data.frame(y = 2)), "essai")), 2L)
  f3 <- tempfile(fileext = ".zip")
  hstat_ecrire_csv_zip(f3, list(A = data.frame(x = 1), B = data.frame(y = 2)))
  expect_equal(length(utils::unzip(f3, list = TRUE)$Name), 2L)
})

test_that("le catalogue de formats ne promet que ce que l'ecrivain sait ecrire", {
  skip_if_not_installed("ggplot2")
  # Offrir un format que l'ecrivain ignore ne leve rien : `hstat_img_fmt()`
  # retombe sur PNG, et l'utilisateur recoit un PNG portant l'extension
  # demandee. Le catalogue est donc verifie contre la table de l'ecrivain...
  expect_true(all(HSTAT_FORMATS_IMG %in% names(HSTAT_IMG_MIME)))
  expect_equal(unname(vapply(HSTAT_FORMATS_IMG, hstat_img_fmt, character(1))),
               unname(HSTAT_FORMATS_IMG))

  # ... et contre la realite : chaque format annonce produit un fichier dont
  # les premiers octets sont bien ceux de ce format.
  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  signature <- list(
    png  = as.raw(c(0x89, 0x50, 0x4e, 0x47)),
    jpeg = as.raw(c(0xff, 0xd8, 0xff)),
    bmp  = charToRaw("BM"),
    pdf  = charToRaw("%PDF"))
  for (fmt in unname(HSTAT_FORMATS_IMG)) {
    f <- tempfile(fileext = paste0(".", fmt))
    ok <- suppressWarnings(hstat_ecrire_image(f, p, fmt, 4, 3, 72))
    expect_true(ok, info = fmt)
    expect_gt(file.size(f), 0)
    if (!is.null(signature[[fmt]])) {
      tete <- readBin(f, "raw", length(signature[[fmt]]))
      expect_identical(tete, signature[[fmt]], info = fmt)
    } else {
      # TIFF (II*/MM*), SVG et EPS : formats texte ou a boutisme variable.
      expect_gt(file.size(f), 100)
    }
  }
})

test_that("aucun module du paquet n'appelle un autre paquet sans prefixe", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  mods <- list.files(file.path(root, "R"), pattern = "^mod_.*[.]R$",
                     full.names = TRUE)
  expect_gte(length(mods), 15L)   # tous migres : plus aucun dans inst/app/

  # L'INVARIANT DU CODE DE PAQUET : tout appel a une fonction d'un autre paquet
  # est qualifie (`DT::renderDT`), ou passe par un AIGUILLAGE de l'application.
  # Sans cela le module depend de ce que `library()` a attache -- et un paquet
  # installe mais NON attache le fait tomber, ce qui s'est produit en
  # integration continue.
  #
  # Le test BALAIE le dossier : un module ajoute demain est couvert sans qu'on
  # y pense, ce qui est exactement ce qu'une liste de noms ne fait pas.
  # LA LISTE DES AIGUILLAGES SE DERIVE, ELLE NE SE RECOPIE PAS. Elle etait
  # tenue a la main ici, et elle avait deja deux noms de retard
  # (`element_markdown`, `updateColourInput`) : une liste recopiee finit par
  # diverger, et c'est la copie oubliee qui ment. Or `hstat_installer_replis_ui()`
  # POSE exactement ces noms -- on les lui demande.
  #
  # L'enjeu n'est pas cosmetique : un aiguillage absent de la liste fait
  # echouer ce test sur du code sain, et le remede evident (qualifier l'appel)
  # serait precisement le defaut que l'aiguillage existe pour eviter. Constate
  # sur `updateColourInput`, que `shinyjs` RE-EXPORTE -- le meme piege que la
  # note de NAMESPACE documente deja pour `colourInput`.
  #
  # `%>%` reste nomme a part : il est pose au premier niveau du socle, pas par
  # l'installateur (un repli naif du pipe perdrait les arguments nommes).
  aig_env <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(hstat_installer_replis_ui(aig_env)))
  aiguillages <- c(ls(aig_env, all.names = TRUE), "%>%")
  expect_gte(length(aiguillages), 12L)   # une liste vide ne garderait rien
  socle <- new.env()
  suppressWarnings(suppressMessages(
    sys.source(.hstat_socle_path(), envir = socle, keep.source = FALSE)))
  bases <- unlist(lapply(c("base", "stats", "utils", "graphics", "grDevices",
                           "methods", "tools", "parallel", "compiler"),
                         function(p) ls(asNamespace(p), all.names = TRUE)))
  maison <- unlist(lapply(.hstat_sources_app(), function(f) {
    ex <- tryCatch(parse(f), error = function(e) NULL)
    if (is.null(ex)) return(character(0))
    unlist(lapply(ex, function(e)
      if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "<<-", "="))
        as.character(e[[2]])[1] else NULL))
  }))
  d <- read.dcf(file.path(root, "DESCRIPTION"))
  imports <- sub("\\s*\\(.*", "", trimws(unlist(strsplit(d[1, "Imports"], ","))))
  imports <- imports[vapply(imports, function(p)
    isTRUE(requireNamespace(p, quietly = TRUE)), logical(1))]
  exports <- lapply(imports, getNamespaceExports)

  fautifs <- character(0)
  for (mod in mods) {
    pd <- utils::getParseData(parse(mod, keep.source = TRUE))
    a <- pd[pd$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]
    if (!nrow(a)) next
    # Deja « pris » : precede de `::`, `:::`, `$` ou `@`. Le cas `$` n'est pas
    # theorique : `tags$code(...)` est etiquete comme un appel, et le qualifier
    # produirait `tags$shiny::code(...)`, que R refuse d'analyser.
    pris <- pd[pd$token %in% c("NS_GET", "NS_GET_INT", "'$'", "'@'"), , drop = FALSE]
    garde <- !vapply(seq_len(nrow(a)), function(i)
      any(pris$line1 == a$line1[i] & pris$col2 == a$col1[i] - 1L), logical(1))
    restants <- setdiff(unique(a$text[garde]),
                        c(bases, ls(socle, all.names = TRUE), maison, aiguillages,
                          .hstat_noms_definis(mod)))
    if (!length(restants)) next
    # Ce qui reste doit etre local au fichier (aides internes, variables portant
    # une fonction) : aucun ne doit appartenir a un paquet declare en Imports.
    for (i in seq_along(imports)) {
      h <- intersect(restants, exports[[i]])
      if (length(h))
        fautifs <- c(fautifs, paste0(basename(mod), " : ", imports[i], "::", h))
    }
  }
  expect_equal(fautifs, character(0))
})

test_that("aucun appel qualifie ne recouvre une fonction locale", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # LE DEFAUT PROPRE AUX QUALIFICATIONS EN MASSE, constate trois fois.
  # `hstat_ai_reglages_ui()` definit chez elle `id <- function(s) ns(...)` ;
  # le balayage n'a vu qu'un appel inconnu, l'a trouve exporte par dplyr, et a
  # ecrit `dplyr::id("url")`. L'interface ne levait rien AU CHARGEMENT -- le
  # defaut n'apparait qu'a l'affichage de l'onglet, sur « id() was deprecated
  # in dplyr 0.5.0 and is now defunct ». Deux autres : `VIM::prepare()` et
  # `mclust::sim()`, tous deux des reactifs du module.
  #
  # Le critere est la PORTEE, pas la coincidence de nom : `httr::timeout(timeout)`
  # ou `grDevices::rgb(rgb[1], ...)` sont justes -- la locale y porte une valeur,
  # pas une fonction. Seule une liaison qui porte une FONCTION recouvre un appel.
  est_fn <- function(x)
    is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) == "function"
  vide <- function(l, i) identical(l[[i]], quote(expr = ))
  fabriques <- c("function", "reactive", "eventReactive", "reactiveVal",
                 "debounce", "throttle", "renderPlot", "Negate", "Vectorize")
  porte_fonction <- function(x) {
    if (!is.call(x)) return(FALSE)
    tete <- x[[1]]
    if (is.call(tete) && is.name(tete[[1]]) &&
        as.character(tete[[1]]) %in% c("::", ":::")) tete <- tete[[3]]
    is.name(tete) && as.character(tete) %in% fabriques
  }
  # Noms lies dans un corps, SANS descendre dans les fonctions imbriquees :
  # c'est la portee de R, et l'ignorer attribuerait a `mod_viz_server` les
  # locales de ses cent reactifs.
  liaisons <- function(x) {
    acc <- character(0)
    rec <- function(y) {
      if (!is.call(y) || est_fn(y)) return(invisible())
      tete <- y[[1]]
      if (is.name(tete) && as.character(tete) %in% c("<-", "=", "<<-") &&
          is.name(y[[2]]) && porte_fonction(y[[3]]))
        acc <<- c(acc, as.character(y[[2]]))
      l <- as.list(y)
      for (i in seq_along(l)) if (!vide(l, i)) rec(l[[i]])
    }
    rec(x); unique(acc)
  }
  trouves <- character(0)
  visiter <- function(x, pile, fichier) {
    if (!is.call(x)) return(invisible())
    if (est_fn(x)) {
      fm <- as.list(x[[2]])
      pile <- c(pile, liaisons(x[[3]]))
      for (i in seq_along(fm)) if (!vide(fm, i)) visiter(fm[[i]], pile, fichier)
      visiter(x[[3]], pile, fichier)
      return(invisible())
    }
    tete <- x[[1]]
    if (is.call(tete) && is.name(tete[[1]]) &&
        as.character(tete[[1]]) %in% c("::", ":::") &&
        as.character(tete[[3]]) %in% pile)
      trouves <<- c(trouves, sprintf("%s : %s::%s", fichier,
                                     as.character(tete[[2]]), as.character(tete[[3]])))
    l <- as.list(x)
    for (i in seq_along(l)) if (!vide(l, i)) visiter(l[[i]], pile, fichier)
  }
  for (f in .hstat_sources_app())
    for (e in parse(f)) visiter(e, character(0), basename(f))
  expect_equal(unique(trouves), character(0))
})

test_that("aucune borne d'axe ni valeur de repere n'est bridee au positif", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # « Valeur du seuil (%) » portait `min = 0, max = 100`. On ne pouvait donc pas
  # poser de repere sur une efficacite NEGATIVE -- la modalite fait moins bien
  # que le temoin, c'est le resultat que l'on cherche justement a lire -- ni
  # au-dela de 100. Un repere qu'on ne peut pas saisir est un repere qui
  # n'existe pas.
  #
  # Meme regle pour les bornes d'axe : elles doivent aller aussi loin dans le
  # negatif que dans le positif.
  champs <- c("thresholdValue", "thresholdYMin", "thresholdYMax",
              "yAxisMin", "yAxisMax", "xAxisMin", "xAxisMax", "refValue")
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    for (e in parse(f)) {
      rec <- function(x) {
        if (!is.call(x)) return(invisible())
        tete <- x[[1]]
        nom <- if (is.call(tete) && is.name(tete[[1]]) &&
                   as.character(tete[[1]]) %in% c("::", ":::"))
                 as.character(tete[[3]])
               else if (is.name(tete)) as.character(tete) else ""
        if (nom == "numericInput") {
          l <- as.list(x)
          id <- if (length(l) > 1 && is.call(l[[2]])) l[[2]] else NULL
          cible <- ""
          if (!is.null(id) && length(id) > 1 && is.character(id[[2]]))
            cible <- id[[2]]
          if (cible %in% champs && "min" %in% names(l))
            fautifs <<- c(fautifs, sprintf("%s : %s a une borne min",
                                           basename(f), cible))
        }
        l <- as.list(x)
        for (i in seq_along(l))
          if (!identical(l[[i]], quote(expr = ))) rec(l[[i]])
      }
      rec(e)
    }
  }
  expect_equal(unique(fautifs), character(0))
})

test_that("l'etendue d'un axe inclut ce qu'on trace par-dessus", {
  # UNE LIGNE DE REFERENCE HORS DU CADRE N'EXISTE PAS. Avec des limites
  # automatiques, ggplot entraine son echelle sur les couches ; mais les
  # GRADUATIONS calculees a la main (`seq(min, max, pas)`) s'arretent, elles, a
  # l'etendue des donnees. La ligne etait tracee et aucune graduation ne disait
  # a quelle hauteur elle passait.
  expect_equal(hstat_etendue_axe(c(-60, 45)), c(-60, 45))
  expect_equal(hstat_etendue_axe(c(-60, 45), c(0, 80)), c(-60, 80))  # le seuil rentre
  expect_equal(hstat_etendue_axe(c(-60, 45), c(-90)), c(-90, 45))    # et vers le bas
  # Un champ vide ne doit pas faire disparaitre l'etendue.
  expect_equal(hstat_etendue_axe(c(-60, 45), c(NA, NaN)), c(-60, 45))
  # Etendue nulle : un axe de hauteur zero ne se trace pas.
  expect_equal(hstat_etendue_axe(c(10, 10)), c(9.5, 10.5))
  # Rien de finit : on rend un cadre par defaut plutot que `c(Inf, -Inf)`.
  expect_equal(hstat_etendue_axe(c(NA, Inf)), c(0, 1))
})

test_that("les reglages propres au format valent pour TOUS les exports", {
  skip_if_not_installed("ggplot2")
  # `hstat_ecrire_image()` sait depuis toujours honorer `qualite` (JPEG) et
  # `compression` (TIFF) ; seul l'onglet Visualisation les DEMANDAIT. Les
  # dix-sept autres blocs ecrivaient donc avec les valeurs par defaut, sans
  # recours -- or une compression TIFF ne se choisit pas par hasard quand la
  # figure part chez un editeur.
  root <- .hstat_repo_root()
  socle <- .hstat_code_lignes(.hstat_socle_path())
  i <- grep("^hstat_export_plot_ui <- function", socle)
  corps <- paste(socle[i:(i + 40)], collapse = "\n")
  expect_true(grepl("Qual", corps, fixed = TRUE))
  expect_true(grepl("HSTAT_TIFF_COMPRESSION", corps, fixed = TRUE))
  # Ils ne s'affichent que pour le format concerne, et la condition vise bien le
  # champ FORMAT -- l'avoir fait pointer sur le champ de compression lui-meme
  # aurait rendu le panneau invisible pour toujours.
  expect_equal(length(gregexpr('paste0(prefix, "Fmt")', corps, fixed = TRUE)[[1]]), 3L)

  # ET ILS AGISSENT : un reglage qui ne change pas le fichier serait pire que
  # son absence. On mesure les octets REELLEMENT produits.
  p <- ggplot2::ggplot(data.frame(x = 1:200, y = sin(1:200 / 9)),
                       ggplot2::aes(x, y)) + ggplot2::geom_point(size = 3)
  taille <- function(...) {
    f <- tempfile(); on.exit(unlink(f), add = TRUE)
    suppressWarnings(hstat_ecrire_image(f, p, ...)); file.size(f)
  }
  expect_lt(taille("jpeg", 6, 4, 150, qualite = 50),
            taille("jpeg", 6, 4, 150, qualite = 95))
  expect_lt(taille("tiff", 6, 4, 150, compression = "lzw"),
            taille("tiff", 6, 4, 150, compression = "none"))
})

test_that("chaque graphique exportable offre un choix de theme", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # Quatre graphiques -- descriptif, plan experimental, distribution, valeurs
  # manquantes -- n'offraient AUCUN choix de theme, alors que les treize autres
  # en avaient un. Le theme rejoint donc le format et le DPI : il se declare au
  # KIT (`hstat_export_plot_ui()`), et un bloc ajoute demain en herite sans
  # qu'on y pense.
  #
  # `theme = FALSE` reste legitime pour les modules qui portent deja un
  # selecteur global (`hstat_plot_opts_ui()`) : deux selecteurs pour un meme
  # graphique, c'est un reglage qui en contredit un autre.
  manquants <- character(0)
  for (f in .hstat_sources_app()) {
    l <- .hstat_code_lignes(f)
    blocs <- grep("hstat_export_plot_ui\\(ns, \"", l, value = TRUE)
    if (!length(blocs)) next
    sans <- grep("theme\\s*=\\s*FALSE", blocs, value = TRUE)
    if (length(sans) && !any(grepl("hstat_plot_opts_ui\\(", l)))
      manquants <- c(manquants, sprintf("%s : %d bloc(s) sans theme et sans hstat_plot_opts_ui",
                                        basename(f), length(sans)))
  }
  expect_equal(manquants, character(0))

  # Le kit declare bien le selecteur, et depuis le catalogue.
  socle <- .hstat_code_lignes(.hstat_socle_path())
  i <- grep("^hstat_export_plot_ui <- function", socle)
  expect_length(i, 1L)
  corps <- paste(socle[i:(i + 25)], collapse = "\n")
  expect_true(grepl("HSTAT_THEMES_GG", corps, fixed = TRUE))
  expect_true(grepl("Theme", corps, fixed = TRUE))
})

test_that("un seul choisisseur de theme : viz_get_theme", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # `hstat_apply_plot_opts()` portait un SECOND `switch` sur le nom du theme, et
  # il avait derive : cinq themes connus sur les huit du catalogue. « Gris »,
  # « Traits fins » et « Sans decor » retombaient EN SILENCE sur « Minimal » --
  # l'utilisateur changeait le reglage et l'image ne bougeait pas.
  #
  # Le balayage cherche tout `switch` dont les etiquettes sont des noms de
  # theme, hors de `viz_get_theme()` elle-meme.
  noms <- unname(HSTAT_THEMES_GG)
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    for (e in parse(f)) {
      nom_def <- if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") &&
                     is.name(e[[2]])) as.character(e[[2]]) else ""
      if (identical(nom_def, "viz_get_theme")) next
      rec <- function(x) {
        if (!is.call(x)) return(invisible())
        if (is.name(x[[1]]) && as.character(x[[1]]) == "switch") {
          et <- names(as.list(x))
          et <- if (is.null(et)) character(0) else et[nzchar(et)]
          if (length(intersect(et, noms)) >= 3L)
            fautifs <<- c(fautifs, sprintf("%s : %s()", basename(f), nom_def))
        }
        l <- as.list(x)
        for (i in seq_along(l))
          if (!identical(l[[i]], quote(expr = ))) rec(l[[i]])
      }
      rec(e)
    }
  }
  expect_equal(unique(fautifs), character(0))

  # Et les huit themes du catalogue sont tous rendus par `viz_get_theme()`.
  for (th in unname(HSTAT_THEMES_GG)) {
    g <- viz_get_theme(th, base_size = 12)
    expect_s3_class(g, "theme")
  }
  # Un theme du catalogue absent du `switch` retombe sur la branche par defaut,
  # qui rend EXACTEMENT `theme_minimal(base_size)` : l'objet complet est alors
  # identique a celui de « minimal ». C'est donc l'objet COMPLET qu'on compare.
  #
  # Premiere version de ce test : elle comparait deux proprietes choisies a la
  # main (`panel.background`, `panel.grid.major`). Elle passait en local et
  # ECHOUAIT en integration continue sur « void » -- selon la version de
  # ggplot2, ces deux proprietes-la coincident avec celles de `theme_minimal`
  # sans que les themes soient pour autant les memes. Un test ne doit pas
  # dependre de la propriete par laquelle deux themes se distinguent.
  ref <- viz_get_theme("minimal", base_size = 12)
  autres <- setdiff(unname(HSTAT_THEMES_GG), "minimal")
  pareils <- autres[vapply(autres,
    function(th) identical(viz_get_theme(th, 12), ref), logical(1))]
  expect_equal(pareils, character(0))
  # Et le contre-exemple : un nom absent du catalogue DOIT retomber sur minimal.
  expect_identical(viz_get_theme("theme_inexistant", 12), ref)
})

test_that("tout appel qualifie designe un objet qui existe", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # SIX APPELS VERS DES FONCTIONS INEXISTANTES, tous invisibles a la lecture :
  # `glmmTMB::gaussian()`, `binomial()`, `poisson()`, `Gamma()`,
  # `inverse.gaussian()` -- glmmTMB porte ces noms dans son espace de noms sans
  # les EXPORTER, si bien qu'un balayage par `getNamespaceExports()` pouvait les
  # croire siens. Ce sont les familles de `stats`. Le selecteur de loi du GLMM
  # levait « 'gaussian' is not an exported object ».
  #
  # Et `emmeans::cld`, qui n'existe pas non plus : enferme dans un `tryCatch`,
  # il rendait NULL au lieu de lever -- un repli mort qui donnait l'illusion
  # d'un filet de securite.
  #
  # Trouve par `R CMD check`, jamais par l'execution ni par la lecture.
  vide <- function(l, i) identical(l[[i]], quote(expr = ))
  paires <- list()
  for (f in .hstat_sources_app()) {
    for (e in parse(f)) {
      rec <- function(x) {
        if (!is.call(x)) return(invisible())
        if (is.name(x[[1]]) && as.character(x[[1]]) %in% c("::", ":::") &&
            length(x) == 3L && is.name(x[[2]]) && is.name(x[[3]]))
          paires[[length(paires) + 1L]] <<- c(basename(f), as.character(x[[2]]),
                                              as.character(x[[3]]))
        l <- as.list(x)
        for (i in seq_along(l)) if (!vide(l, i)) rec(l[[i]])
      }
      rec(e)
    }
  }
  expect_gt(length(paires), 5000L)          # la qualification est bien en place
  vus <- unique(vapply(paires, function(p) paste(p[2], p[3], sep = "::"), character(1)))
  fautifs <- character(0)
  for (v in vus) {
    pk <- sub("::.*", "", v); ob <- sub(".*::", "", v)
    # Un paquet absent de la machine n'est pas une faute : on ne peut rien en
    # dire, et le dire quand meme ferait echouer le test sur l'ENVIRONNEMENT.
    if (!isTRUE(requireNamespace(pk, quietly = TRUE))) next
    if (!(ob %in% getNamespaceExports(pk))) fautifs <- c(fautifs, v)
  }
  expect_equal(fautifs, character(0))
})

test_that("aucun appel ne nomme un argument que la fonction n'accepte pas", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # `hstat_q_apply_palette()` a pour parametres `col_low` / `col_high` ;
  # `mod_coding.R` l'appelait avec `low =` / `high =`. R leve « unused
  # arguments » A L'APPEL : le nuage de mots tombait entierement des qu'une
  # palette autre que « default » etait choisie -- donc jamais au chargement,
  # jamais a la lecture, seulement sous les doigts de l'utilisateur.
  #
  # Trouve par le compilateur d'octets a l'installation du paquet ; c'est un
  # benefice direct de la conversion, et ce test le rend permanent.
  fichiers <- .hstat_sources_app()
  formels <- list()
  for (f in fichiers) for (e in parse(f)) {
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=", "<<-") &&
        is.name(e[[2]]) && is.call(e[[3]]) && is.name(e[[3]][[1]]) &&
        as.character(e[[3]][[1]]) == "function")
      formels[[as.character(e[[2]])]] <- names(as.list(e[[3]][[2]]))
  }
  expect_gte(length(formels), 300L)
  vide <- function(l, i) identical(l[[i]], quote(expr = ))
  trouves <- character(0)
  visiter <- function(x, fichier) {
    if (!is.call(x)) return(invisible())
    tete <- x[[1]]
    if (is.name(tete)) {
      fm <- formels[[as.character(tete)]]
      # `...` absorbe tout : la fonction ne peut pas se plaindre.
      if (!is.null(fm) && !("..." %in% fm)) {
        nommes <- names(as.list(x))
        nommes <- if (is.null(nommes)) character(0) else nommes[-1]
        inconnus <- setdiff(nommes[nzchar(nommes)], fm)
        if (length(inconnus))
          trouves <<- c(trouves, sprintf("%s : %s(%s)", fichier,
                                         as.character(tete),
                                         paste(inconnus, collapse = ", ")))
      }
    }
    l <- as.list(x)
    for (i in seq_along(l)) if (!vide(l, i)) visiter(l[[i]], fichier)
  }
  for (f in fichiers) for (e in parse(f)) visiter(e, basename(f))
  expect_equal(unique(trouves), character(0))
})

test_that("aucun importFrom ne nomme un aiguillage", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # Un nom importe vit dans `imports:HStat`, CHERCHE AVANT l'environnement
  # global ou `hstat_installer_replis_ui()` pose les aiguillages : l'aiguillage
  # ne peut alors jamais gagner. `importFrom(shinyjs, colourInput)` -- shinyjs
  # RE-EXPORTE un ersatz devenu caduc -- faisait tomber TOUTE la construction
  # de l'interface sur « colourInput() has been moved to the 'colourpicker'
  # package ».
  #
  # Le defaut n'existait QUE dans le paquet installe : depuis les sources il
  # n'y a pas d'environnement d'imports. Aucun parcours de l'application depuis
  # le depot ne pouvait le voir.
  aiguillages <- c("withSpinner", "plotlyOutput", "ggplotly", "layout", "config",
                   "renderPlotly", "colourInput", "pickerInput",
                   "radioGroupButtons", "updatePickerInput", "rank_list")
  n <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  imp <- grep("^importFrom\\(", n, value = TRUE)
  noms <- sub("^importFrom\\([^,]+,\\s*([^)]+)\\)$", "\\1", imp)
  expect_equal(intersect(trimws(noms), aiguillages), character(0))
})

test_that("un nom masque par un paquet d'interface est qualifie", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # `box` EST DANS graphics. Un balayage qui tient les paquets de base pour
  # « connus » le laisse donc non qualifie -- et sans `library(shinydashboard)`
  # attache, `box(title = ..., status = ...)` appelle `graphics::box` et leve
  # « plot.new has not been called yet ». L'interface entiere ne se construit
  # plus, pour un nom de trois lettres.
  #
  # C'est l'exact envers du piege precedent : la, un nom de paquet recouvrait
  # une fonction locale ; ici, un nom de base recouvre une fonction de paquet.
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    a <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "box", , drop = FALSE]
    if (!nrow(a)) next
    ns <- pd[pd$token %in% c("NS_GET", "NS_GET_INT"), , drop = FALSE]
    nu <- !vapply(seq_len(nrow(a)), function(i)
      any(ns$line1 == a$line1[i] & ns$col2 == a$col1[i] - 1L), logical(1))
    if (any(nu))
      fautifs <- c(fautifs, sprintf("%s:%d box()", basename(f), a$line1[nu]))
  }
  expect_equal(fautifs, character(0))
})

test_that("le pronom .data n'est jamais qualifie", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # LE CONTRE-EXEMPLE DE LA QUALIFICATION. `.data` est bien exporte par
  # ggplot2, mais ce n'est pas une fonction : c'est un PRONOM, remplace par le
  # masque de donnees au moment de l'evaluation. Ecrit `ggplot2::.data[[x]]`,
  # il est evalue tout de suite et leve « Can't subset `.data` outside of a
  # data mask context » -- donc tout graphique bati sur un nom de colonne
  # variable, ce qui est le cas general ici.
  #
  # Un nom exporte par un paquet n'est donc pas qualifiable pour autant.
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    l <- .hstat_code_lignes(f)
    j <- grep("::[.]data", l)
    if (length(j)) fautifs <- c(fautifs, sprintf("%s:%d", basename(f), j))
  }
  expect_equal(fautifs, character(0))
})

test_that("une fonction passee en argument est qualifiee comme un appel", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # `do.call(tagList, els)` n'est PAS un appel a `tagList` : le jeton est un
  # simple SYMBOL, la qualification des appels ne le voit pas, et le nom resout
  # par le chemin de recherche. Meme chose pour `tags$div(...)` -- `tags` est un
  # objet de shiny, pas une fonction.
  noms <- c("tags", "tagList", "reactiveValues", "geom_col")
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    s <- pd[pd$token == "SYMBOL" & pd$text %in% noms, , drop = FALSE]
    if (!nrow(s)) next
    pris <- pd[pd$token %in% c("NS_GET", "NS_GET_INT", "'$'", "'@'"), , drop = FALSE]
    nu <- !vapply(seq_len(nrow(s)), function(i)
      any(pris$line1 == s$line1[i] & pris$col2 == s$col1[i] - 1L), logical(1))
    if (any(nu))
      fautifs <- c(fautifs, sprintf("%s:%d %s", basename(f), s$line1[nu], s$text[nu]))
  }
  expect_equal(fautifs, character(0))
})

test_that("l'interface se construit sans qu'aucun paquet soit attache", {
  skip_if_not_installed("shiny")
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # LA MESURE DE CE QUE LA QUALIFICATION APPORTE. `UX.R` ne dependait plus que
  # de `library(shinydashboard)`, pose par `install_and_load()` au demarrage :
  # hors de l'application, l'interface ne se construisait pas, et le test des
  # identifiants dupliques se SKIPPAIT depuis toujours -- un test saute
  # ressemble a un test qui passe.
  e <- new.env(parent = globalenv())
  ok <- tryCatch({
    suppressMessages(suppressWarnings({
      socle <- file.path(root, "R")
      for (f in c(file.path(socle, "utils.R"),
                  list.files(socle, pattern = "^mod_.*[.]R$", full.names = TRUE)))
        sys.source(f, e)
      hstat_installer_replis_ui(e)
      old <- setwd(file.path(root, "inst", "app")); on.exit(setwd(old), add = TRUE)
      sys.source("UX.R", e)
    }))
    TRUE
  }, error = function(err) conditionMessage(err))
  expect_true(isTRUE(ok), info = if (!isTRUE(ok)) ok else "")
  expect_true(exists("ui", envir = e))
})

test_that("un nom optionnel passe par son aiguillage, jamais par le paquet", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # LE DEFAUT QUE L'AIGUILLAGE EXISTE POUR EVITER, POSE A LA MAIN. Treize
  # appels ecrivaient `shinycssloaders::withSpinner(...)`,
  # `colourpicker::colourInput(...)` ou `shinyWidgets::pickerInput(...)` en
  # dur. Ces paquets sont OPTIONNELS : absents, l'appel leve, l'interface du
  # module ne se construit pas, et `HStat.R` remplace TOUTE l'application par
  # sa page de secours -- pour un indicateur d'attente manquant.
  #
  # Le repli existait pourtant, sous le meme nom, a un prefixe pres.
  # `ggtext` s'y est ajoute : VINGT-QUATRE appels ecrivaient
  # `ggtext::element_markdown(...)` en dur. Le paquet est optionnel, et son
  # absence emportait le graphique entier -- dans sept modules. Meme defaut que
  # les treize precedents, a ceci pres qu'il frappait au TRACE et non a la
  # construction de l'interface : plus tard, et un onglet a la fois.
  optionnels <- c(shinycssloaders = "withSpinner",
                  colourpicker    = "colourInput",
                  shinyWidgets    = "pickerInput",
                  shinyWidgets    = "radioGroupButtons",
                  shinyWidgets    = "updatePickerInput",
                  sortable        = "rank_list",
                  plotly          = "plotlyOutput",
                  plotly          = "renderPlotly",
                  plotly          = "ggplotly",
                  ggtext          = "element_markdown")
  # LE BALAYAGE PASSE PAR L'ANALYSEUR, PAS PAR LE TEXTE DE LA LIGNE.
  # Cherche a la ligne, il signalait `UX.R:461` -- un commentaire JAVASCRIPT
  # dans une chaine R, que `.hstat_code_lignes()` ne peut pas retirer puisque
  # ce n'est pas un commentaire R. Un balayage qui crie au loup finit
  # desactive ; celui-ci ne voit que de VRAIS `pkg::nom`, commentaires et
  # chaines exclus par construction.
  #
  # Il est aussi plus strict que la version textuelle : celle-ci exigeait la
  # parenthese ouvrante et manquait donc `sapply(x, plotly::ggplotly)`, ou le
  # nom est passe en VALEUR. Un aiguillage contourne de cette facon serait tout
  # aussi mort.
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    pd <- tryCatch(utils::getParseData(parse(f, keep.source = TRUE)),
                   error = function(e) NULL)
    if (is.null(pd)) next
    o <- pd[order(pd$line1, pd$col1), , drop = FALSE]
    k <- which(o$token == "SYMBOL_PACKAGE")
    for (i in k) {
      if (i + 2L > nrow(o) || !identical(o$token[i + 1L], "NS_GET")) next
      pkg <- o$text[i]; nom <- o$text[i + 2L]
      if (any(names(optionnels) == pkg & optionnels == nom))
        fautifs <- c(fautifs, sprintf("%s:%d %s::%s", basename(f), o$line1[i], pkg, nom))
    }
  }
  # `R/utils.R` est le seul endroit legitime : c'est lui qui POSE les
  # aiguillages, il doit bien nommer le paquet vers lequel ils pointent.
  fautifs <- fautifs[!startsWith(fautifs, "utils.R")]
  expect_equal(fautifs, character(0))
})

test_that("le serveur du module de tests s'execute seul, hors application", {
  skip_if_not_installed("shiny")
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  mod <- .hstat_module_path("mod_tests.R")
  skip_if(is.na(mod))

  # VOICI CE QUE LA MIGRATION FAIT GAGNER. Jusqu'ici, un module n'existait que
  # comme effet de bord d'un `source()` sequentiel : les tests ne pouvaient
  # verifier que des balayages de texte -- « le code RESSEMBLE-t-il a ce qu'il
  # faut ». Le module etant desormais dans le paquet, `shiny::testServer()`
  # l'EXECUTE et l'on observe ce qu'il ecrit reellement.
  suppressWarnings(suppressMessages(sys.source(mod, envir = globalenv())))
  skip_if_not(is.function(mod_tests_server))
  # PLUS AUCUN `library()` ICI, et c'est la mesure du progres. Le module
  # qualifie ses appels (`DT::renderDT`, `ggplot2::aes`...) et les noms
  # optionnels passent par les aiguillages : il ne depend plus de ce que
  # `library()` a attache. La version precedente de ce test devait reproduire
  # le contrat de demarrage en attachant six paquets, faute de quoi il tombait
  # en integration continue sur « could not find function updatePickerInput ».
  suppressMessages(hstat_installer_replis_ui())

  set.seed(1)
  d <- data.frame(
    score = c(stats::rnorm(20, 10), stats::rnorm(20, 13), stats::rnorm(20, 16)),
    groupe = rep(c("A", "B", "C"), each = 20), stringsAsFactors = FALSE)
  v <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d)

  shiny::testServer(mod_tests_server, args = list(values = v), {
    # `flushReact()` apres chaque saisie : les versions recentes de Shiny ne
    # vident pas la file reactive au meme moment que les anciennes, et le test
    # observait alors un etat encore vide -- vert ici, rouge en CI.
    vider <- function() try(session$flushReact(), silent = TRUE)
    session$setInputs(responseVar = "score", factorVar = "groupe"); vider()

    # 1. La normalite depose bien un tableau de resultats -- c'est ce tableau
    #    qui declenche la capture « Tests statistiques ».
    session$setInputs(testNormalityRaw = 1); vider()
    # Si le module n'a rien produit, c'est l'environnement qui manque quelque
    # chose (paquet optionnel) : on le DIT, au lieu d'echouer sur un defaut qui
    # n'existe pas -- et au lieu de passer en silence.
    skip_if(is.null(v$testResultsDF),
            "le module n'a produit aucun resultat dans cet environnement")
    expect_gt(NROW(v$testResultsDF), 0L)

    # 2. L'ANOVA ecrit a son tour, et nomme le type de test retenu.
    session$setInputs(testANOVA = 1); vider()
    expect_false(is.null(v$testResultsDF))
    expect_true(nzchar(v$currentTestType %||% ""))

    # 3. Le t de Student sur TROIS groupes doit REFUSER sans rien casser : le
    #    test en compare exactement deux. Le tableau precedent survit.
    avant <- v$testResultsDF
    session$setInputs(testT = 1); vider()
    expect_false(is.null(v$testResultsDF))
    expect_identical(v$testResultsDF, avant)
  })
})

test_that("le socle ne fait rien : il ne fait que definir", {
  socle <- .hstat_socle_path()
  skip_if(is.na(socle))
  # C'EST L'INVARIANT DU PAQUET. Dans un paquet R, le code de premier niveau est
  # evalue A L'INSTALLATION, pas au chargement : un `options()` y serait pose sur
  # la machine de construction, et un `if (!.hstat_has("plotly")) ...` y figerait
  # une decision qui appartient a la machine d'execution.
  ex <- parse(socle)
  agit <- vapply(ex, function(e)
    !(is.call(e) && as.character(e[[1]])[1] %in% c("<-", "<<-", "=")), logical(1))
  # `"_PACKAGE"` est la seule expression non-affectation toleree : c'est le
  # support de la documentation roxygen du paquet, il ne s'evalue pas.
  agit <- agit & !vapply(ex, function(e) identical(e, "_PACKAGE"), logical(1))
  coupables <- vapply(ex[agit], function(e)
    substr(gsub("\\s+", " ", paste(deparse(e), collapse = " ")), 1, 70), character(1))
  expect_equal(unname(coupables), character(0))
})

test_that("le pont ne definit rien d'autre que son chargeur", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # Le pont porte les effets de bord du demarrage, pas les definitions. Une
  # fonction utilitaire qui reapparaitrait ici serait une SECONDE SOURCE DE
  # VERITE : celle que l'application verrait, sans que le paquet ni les tests en
  # sachent rien.
  ex <- parse(file.path(root, "inst", "app", "Utils.R"))
  noms <- unlist(lapply(ex, function(e) {
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "<<-", "="))
      as.character(e[[2]])[1] else NULL
  }))
  # Trois exceptions, chacune motivee : le chargeur lui-meme (il ne peut pas
  # venir d'ailleurs), la taille d'upload (lue dans l'environnement au
  # demarrage) et les deux alias anti-masquage (ils protegent l'environnement
  # de l'application, pas le paquet).
  expect_setequal(noms, c(".hstat_charger_socle", ".hstat_max_mb", "em", "margin"))
})

test_that("le socle se charge seul et rend l'application complete", {
  socle <- .hstat_socle_path()
  skip_if(is.na(socle))
  # LE GAIN ANNONCE, mesure : le socle est chargeable sans demarrer quoi que ce
  # soit. C'est ce qui rendra les modules testables un a un.
  e <- new.env()
  suppressWarnings(suppressMessages(sys.source(socle, envir = e, keep.source = FALSE)))
  expect_gt(length(ls(e, all.names = TRUE)), 200L)
  essentiels <- c("hstat_ecrire_image", "hstat_dpi_effectif", "hstat_valeurs_initiales",
                  "hstat_err_fr", "trf", "tr", "%||%", "hstat_p_verdict",
                  "hstat_efficacite", "hstat_export_plot_handler", "install_and_load",
                  "required_packages", "hstat_model_packages", "HSTAT_FORMATS_IMG",
                  ".hstat_num1", "hstat_feuille_nom", "viz_get_theme", "hstat_i18n_path")
  expect_equal(setdiff(essentiels, ls(e, all.names = TRUE)), character(0))
  # Et il fonctionne hors application : la version se lit sur le disque.
  expect_match(get("hstat_version", envir = e)(), "^[0-9]+[.][0-9]+")
})

test_that("le pont prefere les sources au paquet installe", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # Sur un poste ou HStat est AUSSI installe, charger le paquet ferait travailler
  # l'application sur une version anterieure a celle qu'on edite -- defaut
  # particulierement penible parce qu'il ne se voit pas. L'ordre compte donc :
  # `R/utils.R` d'abord, l'espace de noms ensuite.
  l <- .hstat_code_lignes(file.path(root, "inst", "app", "Utils.R"))
  i_src <- grep('file.path(rel, "R", "utils.R")', l, fixed = TRUE)
  i_pkg <- grep('requireNamespace("HStat"', l, fixed = TRUE)
  expect_length(i_src, 1L)
  expect_length(i_pkg, 1L)
  expect_lt(i_src, i_pkg)
})

test_that("le pont atteint le socle sans dependre des exports", {
  root <- .hstat_repo_root()
  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  # CE QUI COMPTE N'EST PAS L'EXPORT, C'EST LA RECOPIE. Le pont prend les
  # objets dans l'ESPACE DE NOMS -- `ls(asNamespace("HStat"), all.names = TRUE)`
  # atteint aussi bien les aides `.hstat_*`, exportees ou non. Le paquet
  # portait donc un `exportPattern(".")` qui ne servait a rien et coutait une
  # fiche de documentation par objet : ~240 fiches reclamees par `R CMD check`.
  #
  # Les DIRECTIVES seules comptent : la version precedente de ce test cherchait
  # `exportPattern(".")` dans le fichier entier, commentaires compris. Elle
  # aurait continue de passer sur un simple commentaire -- ce qui s'est
  # exactement produit au moment de retirer la directive.
  directives <- grep("^\\s*#", ns, value = TRUE, invert = TRUE)
  expect_false(any(grepl("exportPattern", directives, fixed = TRUE)))
  expect_true(any(grepl("export(run_hstat)", directives, fixed = TRUE)))

  # Et le pont recopie bien depuis l'espace de noms, pas depuis les exports.
  pont <- .hstat_code_lignes(file.path(root, "inst", "app", "Utils.R"))
  expect_true(any(grepl("asNamespace(\"HStat\")", pont, fixed = TRUE)))
  expect_true(any(grepl("all.names = TRUE", pont, fixed = TRUE)))
})

test_that("le format et le DPI ne se declarent qu'au catalogue", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # Dix-sept listes de formats et vingt champs de DPI etaient ecrits a la main,
  # et ils avaient DIVERGE : les neuf exports des analyses multivariees
  # n'offraient que quatre formats sur sept, et deux modules ecrivaient `20000`
  # en clair la ou les autres lisaient `HSTAT_DPI_MAX` -- une montee du plafond
  # en aurait laisse deux en arriere, ce qui etait deja arrive.
  faits <- character(0)
  for (f in .hstat_sources_app()) {
    lignes <- .hstat_code_lignes(f)
    # Une liste de formats d'image ecrite a la main.
    hit <- grep('choices *= *c\\( *"(PNG|png)"', lignes)
    if (length(hit))
      faits <- c(faits, sprintf("%s:%d liste de formats ecrite a la main",
                                basename(f), hit))
    # Un plafond de DPI pose ailleurs qu'au catalogue. Le motif vise le CHAMP
    # DE DPI, pas le nombre 20 000 : les champs de largeur et de hauteur en
    # PIXELS portent le meme plafond sans etre des resolutions, et les compter
    # ferait echouer le test sur une coincidence de chiffre.
    permis <- integer(0)
    deb <- grep("hstat_dpi_input <- function", lignes, fixed = TRUE)
    if (length(deb)) permis <- seq(deb[1], min(length(lignes), deb[1] + 6L))
    hit <- setdiff(grep("max *= *(20000|HSTAT_DPI_MAX)", lignes), permis)
    hit <- hit[vapply(hit, function(i) {
      # Remonter jusqu'a la TETE de l'appel : c'est elle qui porte
      # l'identifiant et le libelle, donc la nature du champ. Se contenter des
      # lignes voisines confondait un champ de pixels avec le champ de DPI
      # declare juste au-dessus.
      j <- rev(grep("numericInput\\(", lignes[max(1L, i - 3L):i]))
      if (!length(j)) return(TRUE)
      tete <- lignes[max(1L, i - 3L) + j[1] - 1L]
      grepl("[Dd][Pp][Ii]|[Rr]ésolution|[Rr]esolution", tete)
    }, logical(1))]
    if (length(hit))
      faits <- c(faits, sprintf("%s:%d plafond de DPI hors catalogue",
                                basename(f), hit))
  }
  expect_equal(faits, character(0))
})

test_that("l'ecrivain commun ferme son peripherique avant de conclure", {
  skip_if_not_installed("ggplot2")
  # LE DEFAUT : `on.exit()` s'accroche a un cadre de FONCTION, et le bloc d'un
  # `tryCatch` n'en cree pas. La fermeture etait donc repoussee a la sortie de
  # `hstat_ecrire_image()` -- apres le gestionnaire d'erreur et apres le
  # controle final, qui lisait un fichier encore vide.
  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  # 1. Sans filet, un export REUSSI rend bien son fichier (le controle final
  #    le declarait perdu et le supprimait).
  f1 <- tempfile(fileext = ".png")
  expect_true(hstat_ecrire_image(f1, p, "png", 6, 4, 100, secours = FALSE))
  expect_gt(file.size(f1), 0)

  # 2. Sans filet, un echec ne laisse AUCUN fichier : c'est ce que le rapport
  #    attend, une figure indessinable doit disparaitre du document.
  f2 <- tempfile(fileext = ".png")
  expect_false(hstat_ecrire_image(f2, function() stop("boum"), "png", 6, 4, 100,
                                  secours = FALSE))
  expect_false(file.exists(f2))

  # 3. Avec filet, l'image de secours SURVIT. Elle etait tracee sur un second
  #    peripherique, puis ecrasee par la fermeture du premier : l'utilisateur
  #    recevait une image vide au lieu du motif. On le mesure en octets, une
  #    image blanche de meme taille pesant une fraction de celle qui porte du
  #    texte.
  f3 <- tempfile(fileext = ".png"); f4 <- tempfile(fileext = ".png")
  expect_false(hstat_ecrire_image(f3, function() stop("boum"), "png", 6, 4, 100,
                                  echec = "Motif attendu, lisible dans l'image."))
  hstat_ecrire_image(f4, function() { graphics::par(mar = c(0, 0, 0, 0));
                                      graphics::plot.new() }, "png", 6, 4, 100)
  expect_gt(file.size(f3), file.size(f4) * 1.5)
})

test_that("les reglages de format sont ceux de l'ecrivain, et ils agissent", {
  skip_if_not_installed("ggplot2")
  # La qualite JPEG et la compression TIFF etaient portees par le seul module
  # qui les propose, donc par son propre appel a `ggsave`. Passees a l'ecrivain,
  # elles doivent AGIR -- un reglage deplace sans effet est pire qu'absent.
  p <- ggplot2::ggplot(data.frame(x = runif(400), y = runif(400)),
                       ggplot2::aes(x, y)) + ggplot2::geom_point()
  bas <- tempfile(fileext = ".jpg"); haut <- tempfile(fileext = ".jpg")
  hstat_ecrire_image(bas,  p, "jpeg", 6, 4, 100, qualite = 5)
  hstat_ecrire_image(haut, p, "jpeg", 6, 4, 100, qualite = 100)
  expect_lt(file.size(bas), file.size(haut))

  # Une valeur aberrante ne fait pas tomber l'export : elle retombe sur 95.
  ab <- tempfile(fileext = ".jpg")
  expect_true(hstat_ecrire_image(ab, p, "jpeg", 6, 4, 100, qualite = "n'importe quoi"))
  expect_gt(file.size(ab), 0)
})

test_that("la mise en page s'adapte aux petits ecrans sans rien couper", {
  root <- .hstat_repo_root()
  css  <- paste(readLines(file.path(root, "inst", "app", "www", "hstat-theme.css"),
                          warn = FALSE), collapse = "\n")
  ux   <- paste(readLines(file.path(root, "inst", "app", "UX.R"), warn = FALSE),
                collapse = "\n")

  # LE DEFAUT : l'escamotage de la barre laterale etait code en dur a 230 px
  # (la valeur d'AdminLTE) alors que HStat declare une barre de 300 px. Les
  # 70 px de difference restaient poses SUR le contenu, dont le bord gauche
  # etait coupe sur tous les onglets. Aucun pixel en dur ne doit revenir : la
  # largeur de la barre peut changer, l'escamotage doit suivre.
  expect_false(grepl("translate(-230px", css, fixed = TRUE))
  expect_false(grepl("translate(-230px", ux,  fixed = TRUE))
  expect_true(grepl("translate(-100%, 0)", css, fixed = TRUE))

  # La largeur declaree dans R reste la seule source : la feuille de style ne
  # doit pas la recopier pour deplacer la barre.
  expect_true(grepl("dashboardSidebar(", ux, fixed = TRUE))
  expect_false(grepl("translate(-300px", css, fixed = TRUE))

  # Les regles responsive vivent dans la feuille de theme, pas dispersees dans
  # l'interface : deux endroits finissent par se contredire, et c'est
  # exactement ce qui s'etait produit.
  expect_false(grepl("@media", ux, fixed = TRUE))
  expect_true(grepl("@media (max-width: 767px)", css, fixed = TRUE))
  expect_true(grepl("@media (max-width: 991px)", css, fixed = TRUE))

  # Ce qui est plus large que l'ecran doit DEFILER dans son conteneur, jamais
  # etre coupe : `.wrapper { overflow: hidden }` fait disparaitre des colonnes
  # entieres sans que rien ne le signale.
  for (regle in c(".dataTables_wrapper", ".hstat-table-scroll"))
    expect_true(grepl(regle, css, fixed = TRUE), label = regle)
  expect_true(grepl("overflow-x: auto", css, fixed = TRUE))

  # Le catalogue multivarie s'empile en BLOCS sous 1100 px. En colonne flex,
  # `align-items: flex-start` reduit les deux colonnes a la largeur de leur
  # contenu : les fiches tombaient a 24 px de large sur telephone.
  i_media <- regexpr("@media (max-width: 1100px)", css, fixed = TRUE)
  expect_gt(i_media, 0)
  bloc <- substr(css, i_media, i_media + 700)
  expect_true(grepl(".mv-layout { display: block; }", bloc, fixed = TRUE))

  # 16 px sur les champs : en dessous, Safari iOS zoome des qu'on y touche et
  # la page reste zoomee -- l'interface se retrouve coupee sans qu'on ait rien
  # demande.
  expect_true(grepl("input, select, textarea, .form-control { font-size: 16px; }",
                    css, fixed = TRUE))

  # Et la page declare bien qu'elle se rend a la largeur de l'appareil : sans
  # cette balise, un telephone rend la page a 980 px et la reduit.
  expect_true(grepl("width=device-width", ux, fixed = TRUE))
})

test_that("les fichiers statiques portent la version, sinon le cache ment", {
  # Servie sous un nom INCHANGE, la feuille de style reste en cache : le
  # serveur est mis a jour et l'utilisateur voit toujours l'ancienne mise en
  # page, sans qu'aucun message ne le lui dise. Constate sur telephone, ou
  # l'on ne sait meme pas comment forcer un rechargement.
  expect_equal(hstat_asset("hstat-theme.css"),
               paste0("hstat-theme.css?v=", hstat_version()))
  expect_true(grepl("?v=", hstat_asset("x.js"), fixed = TRUE))

  ux <- paste(readLines(file.path(.hstat_repo_root(), "inst", "app", "UX.R"),
                        warn = FALSE), collapse = "\n")
  # Aucun appel direct ne doit subsister : c'est celui qu'on oublie qui garde
  # l'ancien fichier. On vise la forme NON estampillee (`href = "..."`,
  # `src = "..."`) et non le nom du fichier, qui figure aussi -- legitimement --
  # a l'interieur de hstat_asset().
  for (f in c("hstat-theme.css", "hstat-session.js", "hstat-i18n.js")) {
    for (attr in c("href", "src"))
      expect_false(grepl(sprintf('%s = "%s"', attr, f), ux, fixed = TRUE),
                   label = paste("appel direct :", attr, f))
    expect_true(grepl(sprintf('hstat_asset("%s")', f), ux, fixed = TRUE),
                label = paste("estampille :", f))
  }

  # L'estampille doit etre la VERSION, qui monte a chaque modification : une
  # valeur figee ne ferait jamais retelecharger, un horodatage ferait
  # retelecharger a chaque demarrage.
  expect_false(grepl("?v=1\"", ux, fixed = TRUE))
})

test_that("la barre laterale est soit entiere, soit absente -- jamais entre les deux", {
  root <- .hstat_repo_root()
  css  <- paste(readLines(file.path(root, "inst", "app", "www", "hstat-theme.css"),
                          warn = FALSE), collapse = "\n")
  ux   <- paste(readLines(file.path(root, "inst", "app", "UX.R"), warn = FALSE),
                collapse = "\n")

  # DEUX mecanismes independants. `transform` seul ne suffit pas : AdminLTE, la
  # CSS que shinydashboard injecte pour width = 300, et le mode replie en
  # posent chacun un, a des valeurs differentes. Qu'un seul l'emporte, et la
  # barre revient A MOITIE sur le contenu -- signale a l'ecran deux fois, dans
  # un contexte que la mesure locale ne reproduisait pas. `left` ne depend
  # d'aucun transform.
  expect_true(grepl("left: -100% !important", css, fixed = TRUE))
  expect_true(grepl("translate(-100%, 0) !important", css, fixed = TRUE))

  # L'etat REPLIE doit valoir a toute largeur : AdminLTE l'escamote de 230 px
  # en dur, la largeur de SA barre, pas de celle de HStat.
  i <- regexpr(".sidebar-collapse .main-sidebar", css, fixed = TRUE)
  expect_gt(i, 0)
  # ... et hors de toute media query : le repli ne connait pas de largeur.
  avant <- substr(css, 1, i)
  expect_equal(length(gregexpr("@media", avant, fixed = TRUE)[[1]]),
               length(gregexpr("\\}\\s*\\n\\}", avant)[[1]]),
               info = "la regle de repli ne doit pas etre enfermee dans une media query")

  # La variante « mini » d'AdminLTE laisse un rail de 50 px : le menu de HStat
  # n'a pas d'icones seules, un rail y poserait des puces muettes sur le texte.
  expect_true(grepl(".sidebar-mini.sidebar-collapse", css, fixed = TRUE))

  # Sur telephone, le menu est un TIROIR : il se referme des qu'on choisit une
  # entree ou qu'on touche le contenu. Sinon il reste ouvert par-dessus les
  # resultats, et le bouton qui le refermerait est lui-meme recouvert.
  expect_true(grepl("sidebar-open", ux, fixed = TRUE))
  expect_true(grepl("$(document).on('click', '.sidebar-menu a'", ux, fixed = TRUE))
  expect_true(grepl(".content-wrapper', fermer)", ux, fixed = TRUE))
  # Les evenements passent par jQuery : addEventListener ne les voit jamais.
  expect_false(grepl("addEventListener('click', fermer", ux, fixed = TRUE))
})

test_that("les dispositifs de malherbologie sont complets et bien branches", {
  cat_mh <- hstat_malherbo_catalog()
  expect_length(cat_mh, 11)

  plans <- hstat_design_catalog()
  for (id in names(cat_mh)) {
    d <- cat_mh[[id]]
    # Chaque entree doit tout porter : sans le modele et le piege, le catalogue
    # n'est qu'une liste de noms, et c'est justement ce qu'un experimentateur
    # n'a pas besoin qu'on lui donne.
    for (champ in c("label", "base", "r", "facteurs", "but", "mesures",
                    "modele", "analyse", "piege", "couleur"))
      expect_true(!is.null(d[[champ]]) && length(d[[champ]]) >= 1,
                  label = paste(id, champ))
    # Le dispositif figure dans le catalogue commun...
    expect_true(id %in% plans, label = paste("au catalogue :", id))
    # ... et repose sur un plan qui existe VRAIMENT.
    expect_true(d$base %in% plans, label = paste("plan de base :", id))
    expect_equal(hstat_design_base(id), d$base)
    expect_gte(d$r, 3)          # moins de 3 repetitions ne donne pas d'erreur estimable

    # Les modalites sont saisies dans un champ SEPARE PAR DES VIRGULES : une
    # virgule dans un libelle scinderait silencieusement la modalite en deux.
    for (f in d$facteurs)
      expect_false(any(grepl(",", f, fixed = TRUE)),
                  label = paste("virgule dans une modalite :", id))
    expect_true(all(nzchar(names(d$facteurs))), label = paste("noms de facteurs :", id))
  }

  # Les plans classiques n'ont pas bouge, et un type ordinaire reste lui-meme.
  for (id in c("crd", "fisher", "lsd", "factorial", "split", "strip"))
    expect_equal(hstat_design_base(id), id)
  expect_equal(hstat_design_base(NULL), NULL)

  # LE TEMOIN. C'est l'invariant de la specialite : sans lui, la mesure n'a
  # pas de reference et le dispositif ne repond pas a sa propre question.
  #  - serie additive : le temoin sans adventice porte le rendement potentiel ;
  #  - dose-reponse   : la dose nulle ancre la courbe a l'origine ;
  #  - efficacite     : il en faut DEUX, enherbe pour l'efficacite, propre pour
  #                     la selectivite -- ils ne se remplacent pas.
  expect_true(any(grepl("^A0 ", cat_mh$mh_serie_additive$facteurs[[1]])))
  expect_true(any(grepl("^A0 ", cat_mh$mh_densite_croisee$facteurs$Densite_adventice)))
  expect_true(any(grepl("^D0 ", cat_mh$mh_dose_reponse$facteurs$Dose)))
  expect_true(any(grepl("^T0 ", cat_mh$mh_desherbage$facteurs$Strategie)))
  eff <- cat_mh$mh_efficacite$facteurs$Traitement
  expect_true(any(grepl("enherbé", eff)))    # denominateur de l'efficacite
  expect_true(any(grepl("propre",  eff)))    # reference de rendement et de selectivite

  # Periode critique : les DEUX temoins permanents. Le propre porte le
  # denominateur de toute perte de rendement, l'enherbe en borne le maximum ;
  # sans eux, ni l'une ni l'autre des deux courbes n'est ancree.
  pc <- cat_mh$mh_periode_critique$facteurs$Duree
  expect_true(any(grepl("^PT ", pc)))
  expect_true(any(grepl("^ET ", pc)))
  expect_true(any(grepl("^E", pc)) && any(grepl("^P", pc)))   # les deux series

  # Date et frequence : le temoin non desherbe et le temoin propre encadrent
  # la reponse ; sans le propre, « aussi bien que le propre » ne se teste pas.
  cal <- cat_mh$mh_date_frequence$facteurs$Calendrier
  expect_true(any(grepl("^T0 ", cal)))
  expect_true(any(grepl("^TP ", cal)))

  # Parcelles appariees et bandes traitees : le temoin non traite.
  expect_true(any(grepl("^T0 ", cat_mh$mh_paires$facteurs$Traitement)))
  expect_true(any(grepl("^T0 ", cat_mh$mh_bandes_traitees$facteurs$Traitement)))

  # Les deux dispositifs qui reposent sur un plan DEJA present n'en creent pas
  # un nouveau : ils lui apportent le contenu de la specialite.
  expect_equal(cat_mh$mh_paires$base, "paired")
  expect_equal(cat_mh$mh_bandes_croisees$base, "strip")
  # Le strip-plot exige deux facteurs : le preset doit les fournir.
  expect_length(cat_mh$mh_bandes_croisees$facteurs, 2L)

  # Serie substitutive : les DEUX peuplements purs. Ce sont les denominateurs
  # de RYc et de RYa -- sans eux, aucun rendement relatif ne se calcule, et RYT
  # n'existe pas. C'est l'exigence la plus stricte du lot.
  sub <- cat_mh$mh_serie_substitutive$facteurs$Proportion
  expect_true(any(grepl("100% coton \\+ 0% adventice", sub)))
  expect_true(any(grepl("0% coton \\+ 100% adventice", sub)))
  expect_true(grepl("RYT", cat_mh$mh_serie_substitutive$modele, fixed = TRUE))

  # Une dose-reponse doit ENCADRER la reponse : dose nulle et dose saturante.
  # Sans les deux, ED90 n'est pas estime mais extrapole.
  expect_gte(length(cat_mh$mh_dose_reponse$facteurs$Dose), 6)
  expect_true(any(grepl("4xR", cat_mh$mh_dose_reponse$facteurs$Dose, fixed = TRUE)))

  # Les conseils d'analyse passent par le catalogue, pas par le switch generique.
  a <- hstat_design_analysis("mh_dose_reponse", 1)
  expect_equal(a$modele, cat_mh$mh_dose_reponse$modele)
  expect_true(grepl("extrapole", a$analyse, fixed = TRUE))
  # ... et le switch generique repond toujours pour les plans classiques.
  expect_equal(hstat_design_analysis("fisher", 1)$modele, "y ~ Traitement + block")
})

test_that("un dispositif de malherbologie ne fait pas un moteur de plan de plus", {
  src <- paste(readLines(.hstat_module_path("mod_design.R"),
                         warn = FALSE), collapse = "\n")
  # La structure de traitements est PRE-REMPLIE ; la randomisation, la carte et
  # l'export restent ceux du moteur commun. Deux moteurs divergeraient a la
  # premiere correction -- c'est la lecon deja tiree des feuilles Excel.
  expect_true(grepl("type <- hstat_design_base(type)", src, fixed = TRUE))
  expect_equal(length(gregexpr("hstat_agri_design <- function", src, fixed = TRUE)[[1]]), 1)
  # Les modalites du catalogue ne doivent pas etre ecrasees par le generateur
  # automatique « lettre + debut..fin ».
  expect_true(grepl("if (!is.null(hstat_malherbo_catalog()[[t]])) return()", src, fixed = TRUE))
})

test_that("l'export de l'onglet Visualisation ne retrecit plus la figure", {
  viz <- paste(readLines(.hstat_module_path("mod_viz.R"),
                         warn = FALSE), collapse = "\n")

  # UN ESCALIER reduisait la taille physique a mesure que le DPI montait :
  # 12 x 8 pouces jusqu'a 600 DPI, mais 6 x 4 au-dela de 5000. Demander plus de
  # finesse rendait l'image plus PETITE sur le papier -- le defaut s'aggravait
  # dans le sens ou l'utilisateur cherchait a l'eviter. Le meme escalier avait
  # deja ete retire des analyses multivariees.
  expect_false(grepl("if (dpi <= 600)", viz, fixed = TRUE))
  expect_false(grepl("dpi <= 2400", viz, fixed = TRUE))

  # La qualite JPEG et la compression TIFF etaient declarees dans l'interface
  # et LUES nulle part : deux reglages que l'utilisateur deplacait sans effet.
  expect_true(grepl("input$jpegQuality", viz, fixed = TRUE))
  expect_true(grepl("input$tiffCompression", viz, fixed = TRUE))

  # Le calcul ne doit exister qu'a UN endroit : le telechargement et le panneau
  # qui l'annonce le partagent. Deux copies divergent -- ici, l'annonce aurait
  # dit 6 x 4 pouces pour un fichier de 12 x 8.
  expect_gte(length(gregexpr("hstat_viz_export_dims", viz, fixed = TRUE)[[1]]), 2)
  d1 <- hstat_viz_export_dims(300); d2 <- hstat_viz_export_dims(1200)
  expect_equal(d1$width, d2$width)              # meme mise en page
  expect_equal(d2$px_w / d1$px_w, 4)            # quatre fois plus de pixels
  expect_false(d2$plafonne)
  expect_true(hstat_viz_export_dims(2400)$plafonne)
  # Entrees inutilisables : un repli, jamais une erreur.
  expect_equal(hstat_viz_export_dims(NULL)$dpi, 300)
  expect_equal(hstat_viz_export_dims(NA)$dpi, 300)


  skip_if_not_installed("ggplot2")
  # L'INVARIANT, mesure sur les pixels reellement produits (en-tete IHDR) :
  # monter le DPI ne change pas la mise en page et augmente les pixels.
  ihdr <- function(f) {
    b <- readBin(f, "raw", 33)
    lire <- function(i) sum(as.integer(b[i:(i + 3)]) * c(16777216L, 65536L, 256L, 1L))
    c(l = lire(17), h = lire(21))
  }
  p <- ggplot2::ggplot(data.frame(x = 1:9, y = (1:9)^2), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  px <- lapply(c(150, 300), function(dpi) {
    w <- 12; h <- 8
    eff <- hstat_dpi_effectif(w, h, dpi)
    f <- tempfile(fileext = ".png"); on.exit(unlink(f), add = TRUE)
    suppressMessages(ggplot2::ggsave(f, p, device = "png", width = w,
                                     height = h, units = "in", dpi = eff$dpi,
                                     bg = "white", limitsize = FALSE))
    c(ihdr(f), pouces_l = w)
  })
  expect_equal(px[[1]][["pouces_l"]], px[[2]][["pouces_l"]])   # mise en page constante
  expect_gt(px[[2]][["l"]], px[[1]][["l"]])                    # et plus de pixels
  expect_equal(px[[2]][["l"]] / px[[1]][["l"]], 2, tolerance = 0.02)

  # Le plafond ne joue qu'aux resolutions extremes, la ou ggsave echouerait sur
  # l'allocation du bitmap -- pas des 600 DPI comme l'escalier.
  expect_false(hstat_viz_export_dims(1200)$plafonne)
  expect_true(hstat_viz_export_dims(2400)$plafonne)
})

test_that("le code mort retire ne revient pas", {
  root <- .hstat_repo_root()
  # Le socle compte parmi les sources balayees : le code mort peut aussi y
  # revenir, et c'est meme la qu'il vivait.
  fs <- c(.hstat_sources_app(),
          list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
  src <- paste(unlist(lapply(fs, readLines, warn = FALSE)), collapse = "\n")

  # UNE SECTION CHI-DEUX ENTIERE etait calculee sans etre affichee : vingt
  # sorties et trois telechargements dont le nom n'apparaissait qu'une fois dans
  # tout le depot. Le chi-deux reellement accessible vit ailleurs dans le meme
  # fichier ; le risque etait de corriger la copie morte en croyant corriger
  # l'analyse -- la lecon deja tiree de createPlotDownloadHandler.
  #
  # LA REGLE A SURVECU, LA LISTE NON. Le chi-deux d'adequation a depuis recu
  # l'interface qui lui manquait : `chiSqPostHocTable` est de nouveau la, mais
  # AFFICHEE cette fois. Figer des noms aurait interdit de reparer le defaut ;
  # ce qu'il faut garder, c'est la regle -- une sortie que rien n'affiche ne
  # doit pas exister. On la verifie donc directement, ce qui couvre aussi les
  # sorties qu'on ajouterait demain.
  for (n in c("chiSqGlobalResult", "chiSqPlotMultiple",
              "chiSqVarCatSelect", "downloadChiSqPHPlot"))
    expect_false(grepl(paste0("output$", n), src, fixed = TRUE), label = n)

  sorties <- unique(gsub("^output[$]", "",
    unlist(regmatches(src, gregexpr("output[$]chiSq[A-Za-z0-9_]*", src)))))
  posees <- unique(unlist(regmatches(src,
    gregexpr('ns\\("(chiSq[A-Za-z0-9_]*)"\\)', src, perl = TRUE))))
  posees <- gsub('^ns\\("|"\\)$', "", posees)
  expect_equal(sort(setdiff(sorties, posees)), character(0))

  # ... et le chi-deux VIVANT est toujours la.
  expect_true(grepl("chisq.test(", src, fixed = TRUE))

  # Douze fonctions globales n'etaient ni appelees, ni passees en valeur, ni
  # utilisees comme argument par defaut, ni testees.
  for (n in c(".hstat_palette_colors", "build_letters_df", "hstat_duckdb_count",
              "lm_anova_table", "workflow_state", "manova_univariate_followup"))
    expect_false(grepl(n, src, fixed = TRUE), label = n)

  # Garde-fou inverse, et c'est le plus important : ces cinq-la ETAIENT vivantes,
  # mais seulement passees en VALEUR (mapply, sapply, breaks =) ou en argument
  # par defaut. Un balayage qui ne cherche que « nom( » les croit mortes et les
  # supprime -- ce qui est arrive, et a casse quatre modules.
  for (n in c("is_categorical", "hstat_i18n_path", "interpret_manova_effect",
              "interpret_permanova_effect", ".hstat_code_breaks3"))
    expect_true(grepl(n, src, fixed = TRUE), label = paste("toujours definie :", n))
})

test_that("monter le DPI ne change ni la taille ni la mise en page, nulle part", {
  # LA REGLE, demandee explicitement : l'augmentation de la resolution ne doit
  # affecter ni la qualite, ni la longueur, ni la hauteur, ni la largeur.
  # Deux exports faisaient l'inverse -- ils multipliaient les pouces par un
  # facteur de reduction, si bien que demander plus de finesse rendait l'image
  # plus PETITE sur le papier.
  for (d in c(72, 300, 600, 1200, 2000, 5000, HSTAT_DPI_MAX)) {
    v <- hstat_viz_export_dims(d)
    expect_equal(v$width, 12)            # la taille physique ne bouge jamais
    expect_equal(v$height, 8)
  }

  # ... et la finesse ne DECROIT jamais quand on demande davantage.
  px <- vapply(c(72, 300, 600, 1200, 2000, 5000, HSTAT_DPI_MAX),
               function(d) hstat_viz_export_dims(d)$px_w, numeric(1))
  expect_false(is.unsorted(px))

  # Le plafond porte sur la RESOLUTION, jamais sur la taille : au-dela du cote
  # maximal d'un bitmap le peripherique echoue et l'utilisateur n'obtient rien.
  e <- hstat_dpi_effectif(12, 8, HSTAT_DPI_MAX)
  expect_lt(e$dpi, e$demande)
  expect_true(e$plafonne)
  expect_true(nzchar(e$note))
  expect_lte(12 * e$dpi, HSTAT_RASTER_MAX_PX)
  # ... et il est ANNONCE : un export silencieusement degrade est pire qu'un refus.
  expect_true(grepl("inchang", e$note))

  # En dessous du plafond, la resolution demandee est rendue telle quelle.
  expect_equal(hstat_dpi_effectif(12, 8, 1200)$dpi, 1200)
  expect_false(hstat_dpi_effectif(12, 8, 1200)$plafonne)
  # Une petite figure va donc bien plus haut qu'une grande : c'est le cote en
  # pixels qui compte, pas le DPI.
  expect_gt(hstat_dpi_effectif(4, 3, 5000)$dpi, hstat_dpi_effectif(12, 8, 5000)$dpi)

  # LE VECTORIEL n'est pas plafonne : sa resolution est infinie et le DPI n'y
  # veut rien dire. C'est la reponse a « je veux 20 000 DPI sans rien perdre ».
  skip_if(is.na(.hstat_socle_path()))
  ecr <- paste(readLines(.hstat_socle_path(), warn = FALSE), collapse = "\n")
  i <- regexpr("hstat_ecrire_image <- function", ecr, fixed = TRUE)
  corps <- substr(ecr, i, i + 1400)
  expect_true(grepl('!fmt %in% c("pdf", "svg", "eps")', corps, fixed = TRUE))
  expect_true(grepl("hstat_dpi_effectif", corps, fixed = TRUE))
})

test_that("le plafond du champ DPI est le meme partout", {
  root <- .hstat_repo_root()
  expect_equal(HSTAT_DPI_MAX, 20000L)
  # Neuf champs plafonnaient a 1200 ou 2000 sans raison : l'utilisateur ne
  # pouvait pas demander mieux la ou il en avait besoin. Un seul chiffre,
  # declare une fois.
  for (f in c(file.path(root, "inst", "app", "UX.R"),
              file.path(root, "inst", "app", "app_server.R"),
              .hstat_module_path("mod_descriptive.R"))) {
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")
    dpi_max <- regmatches(src, gregexpr("[Dd]pi\"[^)]*max = [0-9]+", src))[[1]]
    expect_length(dpi_max, 0)
  }
})

test_that("aucun mot francais affiche ne perd ses accents", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # Le francais SANS ACCENT est une faute, et elle etait partout : « modele »,
  # « donnees », « temoin », « parametres »... Le correcteur (hunspell,
  # dictionnaire fr_FR) les a tous trouves ; ce test garde le resultat sans
  # exiger hunspell, en balayant la liste exacte des mots corriges.
  #
  # Il ne porte QUE sur le TEXTE AFFICHE. Trois familles sont ecartees PAR
  # CONSTRUCTION, jamais par une liste d'exceptions :
  #
  #  1. un identifiant (« Modalite », « hstat-termes-donnees ») ne porte pas
  #     d'espace -- un libelle francais en porte toujours ;
  #  2. du code engendre par le journal de reproductibilite, du CSS ou du
  #     JavaScript : accentuer « text-decoration » ou « event » casse la page.
  #     Le premier balayage l'avait fait, trois fois ;
  #  3. le premier argument d'une fonction d'expression reguliere : c'est un
  #     MOTIF, compare a du texte deja deplie par `hstat_sans_accents()`.
  fautifs <- character(0)
  mots <- c("modele", "donnees", "temoin", "etiquette", "methode", "normalite",
            "densite", "reponse", "lineaire", "resultat", "reference",
            "modalite", "decision", "parametrique", "aleatoire", "qualite",
            "telecharger", "selectionnez", "apercu", "portee", "precedente",
            "deplacer", "operateur", "definition", "sensibilite", "detail",
            "interpreter", "regression", "serie", "deja", "categorielle",
            "numerique", "efficacite", "repetition", "verifiez", "echec",
            "eloignes", "representatifs", "parametres", "probleme")
  motif <- paste0("\\b(", paste(mots, collapse = "|"), ")\\b")
  REGEX <- c("grepl", "sub", "gsub", "grep", "regexpr", "gregexpr", "regmatches",
             "strsplit", "startsWith", "endsWith", "switch")
  CODE <- paste0("function\\s*\\(|=>|document[.]|window[.]|Shiny[.]|<-|::|",
                 "[a-z-]+\\s*:\\s*[^;]+;|priority|classList|\\$\\(")
  vide <- function(l, i) identical(l[[i]], quote(expr = ))
  for (f in .hstat_sources_app()) {
    ex <- parse(f, keep.source = TRUE)
    visiter <- function(x) {
      if (is.character(x) && length(x) == 1L && !is.na(x)) {
        if (!grepl("[[:space:]]", x)) return(invisible())
        if (grepl(CODE, x, perl = TRUE)) return(invisible())
        if (grepl(motif, x, ignore.case = TRUE))
          fautifs <<- c(fautifs, sprintf("%s : %s", basename(f), substr(x, 1, 60)))
        return(invisible())
      }
      if (!is.call(x)) return(invisible())
      tete <- x[[1]]
      nm <- if (is.name(tete)) as.character(tete) else ""
      l <- as.list(x)
      deb <- if (nm %in% REGEX && length(l) >= 3) 3L else 2L
      for (i in seq_along(l)) if (i >= deb && !vide(l, i)) visiter(l[[i]])
      invisible()
    }
    for (e in ex) visiter(e)
  }
  expect_equal(fautifs, character(0))
})

test_that("un motif compare a du texte deplie reste sans accent", {
  # `hstat_sans_accents()` retire les accents du texte AVANT comparaison :
  # un motif accentue ne peut alors JAMAIS correspondre. Le journal de
  # reproductibilite est reste muet sur la regression lineaire pour cette
  # raison exacte -- sans erreur, sans avertissement, sans code.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    ex <- parse(f, keep.source = TRUE)
    deplies <- character(0)
    reperer <- function(x) {
      if (!is.call(x)) return(invisible())
      if (identical(x[[1]], quote(`<-`)) && length(x) == 3L && is.name(x[[2]]) &&
          grepl("hstat_sans_accents", paste(deparse(x[[3]]), collapse = " "),
                fixed = TRUE))
        deplies <<- c(deplies, as.character(x[[2]]))
      l <- as.list(x)
      for (i in seq_along(l))
        if (!identical(l[[i]], quote(expr = ))) reperer(l[[i]])
      invisible()
    }
    for (e in ex) reperer(e)
    if (!length(deplies)) next
    verifier <- function(x) {
      if (!is.call(x)) return(invisible())
      nm <- if (is.name(x[[1]])) as.character(x[[1]]) else ""
      if (nm %in% c("grepl", "sub", "gsub", "grep", "regexpr") &&
          length(x) >= 3 && is.character(x[[2]]) && is.name(x[[3]]) &&
          as.character(x[[3]]) %in% deplies &&
          any(utf8ToInt(x[[2]]) > 127L))
        fautifs <<- c(fautifs, sprintf("%s : motif accentue %s",
                                       basename(f), x[[2]]))
      l <- as.list(x)
      for (i in seq_along(l))
        if (!identical(l[[i]], quote(expr = ))) verifier(l[[i]])
      invisible()
    }
    for (e in ex) verifier(e)
  }
  expect_equal(fautifs, character(0))
})

test_that("une colonne creee et une colonne lue portent le meme nom", {
  # `Ecart_type` etait cree sans accent et relu sous « Écart_type » a quatorze
  # endroits : la colonne existait, la lecture rendait NULL, et la sortie
  # partait sans un mot. Le balayage rapproche les chaines accentuees des
  # SYMBOLES du meme fichier.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    chn <- unique(gsub('^"|"$', "", pd$text[pd$token == "STR_CONST"]))
    chn <- chn[grepl("^[A-Za-z_.][A-Za-z0-9_.]*$",
                     iconv(chn, "UTF-8", "ASCII//TRANSLIT")) &
               grepl("[^\\x01-\\x7f]", chn, perl = TRUE)]
    sym <- unique(pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")])
    for (s in chn) {
      nu <- iconv(s, "UTF-8", "ASCII//TRANSLIT")
      if (!is.na(nu) && nu != s && nu %in% sym)
        fautifs <- c(fautifs, sprintf("%s : chaine %s mais symbole %s",
                                      basename(f), s, nu))
    }
  }
  # Les couples legitimes : un nom de colonne ASCII et son LIBELLE accentue,
  # verifies un par un (« Modalite » la colonne, « Modalité » l'etiquette).
  # « Répétition » vient de la grille de saisie du rendement : la colonne
  # s'appelle `Repetition` (un nom de colonne ne s'accentue pas), l'etiquette
  # affichee par `tr()` porte l'accent. Verifie : aucune lecture accentuee
  # (`$Répétition`, `[["Répétition"]]`) n'existe dans le depot.
  # « Espèce » et « Relevé » viennent du module de diversite : les colonnes
  # s'appellent `Espece` et `Releve` (un nom de colonne ne s'accentue pas), les
  # chaines accentuees ne sont QUE des titres d'axe passes a `labs()`. Verifie :
  # les cinq occurrences sont toutes des arguments de `ggplot2::labs()`, aucune
  # lecture (`$Relevé`, `[["Espèce"]]`) n'existe dans le depot.
  # « Sensibilité » et « Spécificité » viennent du module d'epidemiologie : la
  # courbe ROC porte les colonnes `Sensibilite` et `Specificite`, et les chaines
  # accentuees sont le LIBELLE de la ligne du tableau de mesures et le titre
  # d'axe de la figure. Verifie : aucune lecture accentuee (`$Sensibilité`,
  # `[["Spécificité"]]`) n'existe dans le depot -- la figure lit bien
  # `.data[["Sensibilite"]]`.
  connus <- c("Observé", "Prédit", "Modalité", "Résidu", "Thème", "Fréquence",
              "Méthode", "Interprétation", "Métrique", "Unité", "Répétition",
              "Espèce", "Relevé", "Sensibilité", "Spécificité")
  fautifs <- fautifs[!grepl(paste0("chaine (", paste(connus, collapse = "|"),
                                   ") "), fautifs)]
  expect_equal(fautifs, character(0))
})

test_that("la traduction anglaise est coherente et typographiee en anglais", {
  d <- hstat_i18n_load()
  skip_if(is.null(d) || !nrow(d))

  # 1. Les marqueurs de sprintf survivent : une traduction qui en perd un
  #    ferait tomber toute la sortie sur « too few arguments ».
  marq <- function(x) vapply(regmatches(x, gregexpr("%[-0-9.]*[sdfgeix]", x)),
                             paste, character(1), collapse = "")
  expect_equal(marq(d$fr), marq(d$en))

  # 2. La ponctuation francaise ne passe pas la frontiere : l'anglais ne met
  #    pas d'espace avant « : ; ! ? », ni avant le pour-cent.
  expect_equal(d$en[grepl("[^ [:punct:]] [:;!?][^\")]", d$en)], character(0))
  expect_equal(d$en[grepl("[0-9] %[^%s]", d$en)], character(0))

  # 3. Les deux-points annoncent un champ : ils ne disparaissent pas a la
  #    traduction. « Seuil : » rendu « Threshold » perd le signe.
  fr2p <- grepl("[[:space:]]*:[[:space:]]*$", d$fr)
  expect_equal(d$en[fr2p & !grepl(":[[:space:]]*$", d$en)], character(0))

  # 4. Un terme, un mot. « graphique » etait rendu tantot « chart », tantot
  #    « plot » : le lecteur croit lire deux notions.
  g <- d$en[grepl("graphique", d$fr, ignore.case = TRUE)]
  expect_equal(g[grepl("\\bcharts?\\b", g, ignore.case = TRUE)], character(0))
  # « repetition » n'est pas le mot du plan d'experience : c'est « replicate ».
  r <- d$en[grepl("r[ée]p[ée]tition", d$fr, ignore.case = TRUE)]
  expect_equal(r[grepl("\\brepetitions?\\b", r, ignore.case = TRUE)], character(0))
  # « jeu de donnees » : un seul mot anglais.
  j <- d$en[grepl("jeu de donn[ée]es", d$fr, ignore.case = TRUE)]
  expect_equal(j[grepl("\\bdata sets?\\b", j, ignore.case = TRUE)], character(0))
})

test_that("un attribut interdit par le type de trace est retire avant la construction", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("ggplot2")
  # Une trace « bar » n'a pas de `mode` : c'est un attribut des nuages de
  # points. Quand la conversion en depose un, `plotly_build()` avertit puis le
  # jette -- le graphique est juste, mais l'avertissement revient a CHAQUE
  # rendu et finit par masquer ceux qui comptent. Meme famille que le polyfill
  # `typedarray`, meme remede : on supprime la cause, pas le symptome.
  d <- data.frame(x = factor(c("a", "b")), y = c(1, 2))
  g <- plotly::ggplotly(ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_col())
  g$x$data[[1]]$mode <- "markers"

  capte <- function(expr) {
    w <- character(0)
    withCallingHandlers(invisible(force(expr)),
      warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
    w[grepl("attributes", w, fixed = TRUE)]
  }
  # Le defaut existe bel et bien sans le nettoyage : sans cette moitie, le
  # test passerait meme si `hstat_plotly_clean()` ne faisait rien.
  expect_gt(length(capte(plotly::plotly_build(g))), 0L)
  expect_equal(capte(hstat_plotly_clean(g)), character(0))

  # Un `mode` legitime sur un nuage de points n'est PAS touche.
  s <- plotly::ggplotly(ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point())
  s$x$data[[1]]$mode <- "markers"
  expect_equal(.hstat_plotly_attrs(s)$x$data[[1]]$mode, "markers")
})

test_that("une efficacite indeterminable est nommee, pas escamotee", {
  # La formule d'Abbott rend NA des que le temoin vaut zero. La barre
  # disparait alors du graphique, l'axe garde sa place vide, et le seul signal
  # partait dans la console de R. L'utilisateur voyait un trou.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- paste(readLines(file.path(root, "R", "mod_threshold.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Le decompte porte sur les valeurs NON FINIES, pas seulement sur celles qui
  # sortent des bornes : c'est ce qui manquait.
  expect_true(grepl("absentes <- !is.finite(plot_data$Efficacy)", src, fixed = TRUE))
  expect_true(grepl("seuilSansValeur", src, fixed = TRUE))
  # Le message nomme les modalites concernees : « une valeur manquante »
  # n'aide pas a la retrouver dans un tableau de onze traitements.
  expect_true(grepl("plot_data$Treatment[absentes]", src, fixed = TRUE))

  # Et l'avertissement de ggplot devient redondant : chaque `geom_col` du
  # module porte `na.rm = TRUE`, sinon la console repete ce que l'interface
  # vient de dire.
  cols <- gregexpr("geom_col, c\\(list\\(.{0,180}", src, perl = TRUE)
  args <- regmatches(src, cols)[[1]]
  expect_gt(length(args), 0L)
  expect_true(all(grepl("na.rm = TRUE", args, fixed = TRUE)),
              info = paste(args[!grepl("na.rm = TRUE", args, fixed = TRUE)],
                           collapse = "\n"))
})

# =============================================================================
#  DOSES ET DILUTIONS
# =============================================================================

test_that("dose et grammage sont exactement reciproques", {
  # Le cas de terrain : 250 mL/ha d'un produit a 400 g/L, bouillie de 60 L/ha.
  # Les deux sens doivent rendre le MEME tableau -- sinon l'un des deux ment,
  # et rien a l'ecran ne dirait lequel.
  a <- hstat_dose_bilan("dose", 250, "mL/ha", 400, "g/L",
                        volume_bouillie = 60, superficie = 2, volume_cuve = 15)
  b <- hstat_dose_bilan("grammage", 100, "g/ha", 400, "g/L",
                        volume_bouillie = 60, superficie = 2, volume_cuve = 15)
  expect_equal(a$Valeur, b$Valeur)

  v <- stats::setNames(a$Valeur, c("dose", "grammage", "conc_bouillie", "dose_bouillie",
                                   "prod_total", "ma_total", "eau_total",
                                   "par_cuve", "nb_cuves", "surf_cuve"))
  expect_equal(unname(v[["grammage"]]), 100)          # 0,25 L x 400 g/L
  expect_equal(unname(v[["conc_bouillie"]]), 100 / 60)
  expect_equal(unname(v[["dose_bouillie"]]), 250 / 60)
  expect_equal(unname(v[["prod_total"]]), 0.5)        # 0,25 L/ha x 2 ha
  expect_equal(unname(v[["ma_total"]]), 200)
  expect_equal(unname(v[["eau_total"]]), 120)
  expect_equal(unname(v[["surf_cuve"]]), 0.25)        # 15 L / 60 L/ha
  expect_equal(unname(v[["par_cuve"]]), 62.5)         # 250 mL/ha x 0,25 ha
  expect_equal(unname(v[["nb_cuves"]]), 8)

  # CHAQUE ligne porte sa formule : un chiffre de dose qu'on ne peut pas
  # refaire a la main sera recalcule, et c'est le recalcul qui fera foi.
  expect_equal(sum(nzchar(a$Formule)), nrow(a))
})

test_that("une unite inconnue ne vaut jamais 1 par defaut", {
  # Un facteur muet rendrait un resultat mille fois trop grand sans le moindre
  # signe : c'est la faute la plus couteuse que ce module puisse commettre.
  expect_true(is.na(hstat_conc_vers_ref(400, "g/hL")))
  expect_true(is.na(hstat_dose_vers_ref(250, "mL")))
  expect_true(is.na(hstat_dose_depuis_ref(1, "inconnue")))
  # Et les conversions connues sont justes.
  expect_equal(hstat_conc_vers_ref(5, "%"), 50)       # 5 g/100 mL = 50 g/L
  expect_equal(hstat_conc_vers_ref(1000, "ppm"), 1)
  expect_equal(hstat_dose_vers_ref(250, "mL/ha"), 0.25)
})

test_that("le bilan de dose refuse ce qu'il ne peut pas calculer, en le nommant", {
  for (cas in list(list(400 * 0, "g/L"), list(NA, "g/L"), list(400, "g/hL"))) {
    r <- hstat_dose_bilan("dose", 250, "mL/ha", cas[[1]], cas[[2]])
    expect_equal(nrow(r), 0L)
    expect_true(nzchar(attr(r, "message")))
    expect_true(grepl("[Rr]enseignez|[Vv]érifiez|[Cc]hoisissez", attr(r, "message")))
  }
  r <- hstat_dose_bilan("dose", 0, "mL/ha", 400, "g/L")
  expect_equal(nrow(r), 0L)
  expect_true(grepl("saisissez", attr(r, "message"), fixed = TRUE))
})

test_that("un nombre affiche ne ment ni sur zero ni sur ses entiers", {
  # DEUX DEFAUTS SILENCIEUX, tous deux constates sur les solutions filles.
  #
  # 1. LE RETRAIT DES ZEROS MANGEAIT LES ENTIERS. L'ancien motif
  #    `\\.?0+$` ne demandait pas de point : a zero decimale, « 1 000 »
  #    ressortait « 1 ». Le reglage n'allait jamais a zero, le defaut dormait --
  #    et le rendre reglable le reveillait.
  expect_equal(hstat_fmt_nb(1000, 0), "1 000")
  expect_equal(hstat_fmt_nb(1000, 2), "1 000")
  expect_equal(hstat_fmt_nb(1.23, 4), "1.23")
  expect_equal(hstat_fmt_nb(0, 2), "0")

  # 2. UNE CONCENTRATION NON NULLE S'AFFICHAIT « 0 ». Sur une gamme au 1/10
  #    depuis 100 g/L, la 7e fille vaut 1e-5 : a quatre decimales l'ecran
  #    annoncait une solution VIDE. Dans un contexte de paillasse c'est le pire
  #    defaut possible -- on prepare a partir du chiffre lu.
  expect_false(identical(hstat_fmt_nb(1e-5, 4), "0"))
  expect_true(grepl("e-0?5", hstat_fmt_nb(1e-5, 4)))
  # Assez de decimales : l'ecriture decimale revient, la scientifique s'efface.
  expect_equal(hstat_fmt_nb(1e-5, 10), "0.00001")
  # Zero reste zero : la bascule ne vise QUE les valeurs non nulles.
  expect_equal(hstat_fmt_nb(0, 0), "0")
  expect_true(is.na(hstat_fmt_nb(NA, 3)))
})

test_that("le microlitre est une unite de volume, et les volumes se convertissent", {
  # Le microlitre manquait, et c'est l'unite de la paillasse : une gamme au
  # 1/10 depuis 100 mL descend a 0,01 mL des le quatrieme etage -- illisible,
  # alors que 10 uL se pipette.
  expect_true("µL" %in% names(HSTAT_VOL_UNITES))
  expect_equal(unname(HSTAT_VOL_UNITES[["µL"]]), 1e-6)
  # Les facteurs sont des LITRES : mille microlitres font un millilitre.
  expect_equal(HSTAT_VOL_UNITES[["mL"]] / HSTAT_VOL_UNITES[["µL"]], 1000)

  d <- data.frame(
    Produit = "A", Matiere_active = "ma1", Concentration_mere = 100,
    Unite = "g/L", Coefficient = 10, Nb_filles = 3, Volume_final = 100,
    Unite_volume = "mL", stringsAsFactors = FALSE)
  r <- hstat_dilution_calcul(d)

  c_ul <- hstat_dilution_convertir(r, "µL")

  # 1. LA CONVERSION AJOUTE, ELLE NE REMPLACE PAS. Une premiere version
  #    ecrasait la valeur saisie : l'utilisateur qui avait tape « 100 mL » ne
  #    le retrouvait plus nulle part et ne pouvait plus verifier son entree.
  #    Devant une paillasse, la valeur convertie sert au geste et la valeur
  #    saisie sert au controle -- il faut les DEUX.
  expect_equal(c_ul$Volume_final, r$Volume_final)
  expect_equal(c_ul$Volume_a_prelever, r$Volume_a_prelever)
  expect_equal(c_ul$Unite_volume, r$Unite_volume)

  # 2. La valeur convertie est la, dans sa propre colonne, avec son unite.
  expect_equal(c_ul$Volume_final_conv, r$Volume_final * 1000)
  expect_equal(c_ul$Volume_a_prelever_conv, r$Volume_a_prelever * 1000)
  expect_equal(c_ul$Volume_eau_a_ajouter_conv, r$Volume_eau_a_ajouter * 1000)
  expect_equal(unique(c_ul$Unite_volume_conv), "µL")

  # 3. CHAQUE COLONNE CONVERTIE SUIT IMMEDIATEMENT SON ORIGINALE : deux
  #    colonnes de volume separees par la moitie du tableau ne se comparent
  #    pas, et c'est la comparaison qui est demandee.
  nm <- names(c_ul)
  for (k in c("Volume_final", "Volume_a_prelever", "Volume_eau_a_ajouter",
              "Volume_mere_requis", "Unite_volume"))
    expect_equal(match(paste0(k, "_conv"), nm), match(k, nm) + 1L, info = k)
  # Rien n'est perdu au passage : les colonnes d'origine sont toutes la.
  expect_true(all(names(r) %in% nm))

  # L'option est une OPTION : sans unite, rien ne bouge.
  expect_equal(hstat_dilution_convertir(r, ""), r)
  expect_equal(hstat_dilution_convertir(r, NULL), r)
  # Une unite inconnue ne doit pas VIDER la colonne en divisant par NA.
  expect_equal(hstat_dilution_convertir(r, "gallon"), r)
})

test_that("la concentration de depart est facultative et fixe le premier etage", {
  base <- data.frame(
    Produit = "A", Matiere_active = "ma1", Concentration_mere = 100,
    Unite = "g/L", Coefficient = 10, Nb_filles = 4, Volume_final = 100,
    Unite_volume = "mL", stringsAsFactors = FALSE)

  # 1. ABSENTE, LE CALCUL EST EXACTEMENT CELUI D'AVANT -- c'est ce qui rend
  #    l'ajout sans risque pour un tableau deja saisi.
  r0 <- hstat_dilution_calcul(base)
  expect_equal(r0$Concentration_fille, 100 / 10^(1:4))
  expect_equal(r0$Volume_a_prelever, 100 / 10^(1:4))
  # La colonne DISPARAIT quand personne ne s'en sert : une colonne vide fait
  # croire a une information absente.
  expect_false("Concentration_depart" %in% names(r0))

  # 2. RENSEIGNEE, la gamme commence a cette valeur puis se divise par le
  #    coefficient. Donner 25 au lieu des 10 par defaut decale toute la suite.
  b1 <- base; b1$Concentration_depart <- 25
  r1 <- hstat_dilution_calcul(b1)
  expect_true("Concentration_depart" %in% names(r1))
  expect_equal(r1$Concentration_fille, 25 / 10^(0:3))
  # La conservation du solute tient : c'est elle qui garantit le prelevement.
  expect_equal(r1$Concentration_mere * r1$Volume_a_prelever,
               r1$Concentration_fille * r1$Volume_final)
  # Le rang 1 vient de la MERE, les suivants de la fille precedente.
  expect_equal(r1$Concentration_precedente[1], 100)
  expect_equal(r1$Concentration_precedente[-1], r1$Concentration_fille[-4])

  # 3. UNE DILUTION NE CONCENTRE PAS.
  b2 <- base; b2$Concentration_depart <- 200
  expect_equal(nrow(hstat_dilution_calcul(b2)), 0L)
  expect_true(grepl("ne concentre pas",
                    attr(hstat_dilution_calcul(b2), "message"), fixed = TRUE))
  b3 <- base; b3$Concentration_depart <- 0
  expect_true(grepl("nulle ou négative",
                    attr(hstat_dilution_calcul(b3), "message"), fixed = TRUE))

  # 4. UN FLACON, UN GESTE. Une dilution divise TOUTES les matieres actives par
  #    le meme nombre : deux concentrations de depart qui impliqueraient deux
  #    coefficients decriraient deux gestes sur le meme flacon.
  duo <- data.frame(
    Produit = "B", Matiere_active = c("a", "b"),
    Concentration_mere = c(100, 50), Unite = "g/L",
    Coefficient = 10, Nb_filles = 2, Volume_final = 100,
    Unite_volume = "mL", stringsAsFactors = FALSE)
  faux <- duo; faux$Concentration_depart <- c(25, 25)      # rapports 4 et 2
  expect_equal(nrow(hstat_dilution_calcul(faux)), 0L)
  expect_true(grepl("contradictoires",
                    attr(hstat_dilution_calcul(faux), "message"), fixed = TRUE))

  # Dans le MEME rapport, c'est un geste unique : accepte.
  bon <- duo; bon$Concentration_depart <- c(25, 12.5)
  rb <- hstat_dilution_calcul(bon)
  expect_equal(nrow(rb), 4L)
  expect_setequal(rb$Concentration_fille[rb$Rang == 1], c(25, 12.5))
  # La tolerance est relative et lache : une saisie arrondie passe.
  arr <- duo; arr$Concentration_mere <- c(33.3, 10)
  arr$Concentration_depart <- c(3.33, 1)
  expect_equal(nrow(hstat_dilution_calcul(arr)), 4L)
})

test_that("les solutions filles suivent la conservation du solute", {
  d <- data.frame(
    Produit            = c("A", "B", "B"),
    Matiere_active     = c("ma1", "ma1", "ma2"),
    Concentration_mere = c(50, 25, 20),
    Unite              = "g/L",
    Coefficient        = c(10, 5, 5),
    Volume_final       = c(1, 2, 2),
    Unite_volume       = "L", stringsAsFactors = FALSE)
  r <- hstat_dilution_calcul(d)
  expect_equal(nrow(r), 3L)

  # C_mere x V_preleve = C_fille x V_final, pour chaque ligne.
  for (i in seq_len(nrow(r))) {
    vp <- r$Volume_final[i] / r$Coefficient[i]
    expect_equal(r$Concentration_mere[i] * vp,
                 r$Concentration_fille[i] * r$Volume_final[i])
  }
  a <- r[r$Produit == "A", ]
  expect_equal(a$Concentration_fille, 5)
  expect_equal(a$Volume_a_prelever, 0.1)
  expect_equal(a$Volume_eau_a_ajouter, 0.9)

  # Un produit a DEUX matieres actives : chacune a sa concentration fille, et
  # la solution porte leur somme -- c'est la question posee.
  bb <- r[r$Produit == "B", ]
  expect_setequal(bb$Concentration_fille, c(5, 4))
  expect_equal(sum(bb$Concentration_totale_g_L, na.rm = TRUE), 9)

  # Le volume a prelever appartient au PRODUIT, pas a la matiere active : le
  # repeter ferait croire qu'il faut prelever deux fois.
  expect_equal(sum(!is.na(bb$Volume_a_prelever)), 1L)
  expect_equal(sum(bb$Volume_a_prelever, na.rm = TRUE), 0.4)
  expect_equal(sum(bb$Volume_eau_a_ajouter, na.rm = TRUE), 1.6)

  expect_true(length(attr(r, "formules")) >= 3L)
})

test_that("les concentrations filles sont additionnees dans une unite commune", {
  # Sommer des pour-cent et des mg/L donnerait un total qui ne veut rien dire.
  d <- data.frame(
    Produit            = "P", Matiere_active = c("a", "b"),
    Concentration_mere = c(1, 5000),        # 1 % = 10 g/L ; 5000 mg/L = 5 g/L
    Unite              = c("%", "mg/L"),
    Coefficient        = 10, Volume_final = 1, Unite_volume = "L",
    stringsAsFactors = FALSE)
  r <- hstat_dilution_calcul(d)
  # filles : 0,1 % = 1 g/L, et 500 mg/L = 0,5 g/L -> 1,5 g/L au total
  expect_equal(sum(r$Concentration_totale_g_L, na.rm = TRUE), 1.5)
})

test_that("la dilution refuse une saisie contradictoire en nommant le produit", {
  base <- data.frame(
    Produit = c("Alpha", "Alpha"), Matiere_active = c("a", "b"),
    Concentration_mere = c(10, 20), Unite = "g/L",
    Coefficient = c(10, 10), Volume_final = c(1, 1), Unite_volume = "L",
    stringsAsFactors = FALSE)

  # Coefficient inferieur a 1 : c'est une CONCENTRATION, presque toujours une
  # inversion de saisie (0,1 pour 10).
  k <- base; k$Coefficient <- c(0.1, 0.1)
  r <- hstat_dilution_calcul(k)
  expect_equal(nrow(r), 0L)
  expect_true(grepl("Alpha", attr(r, "message"), fixed = TRUE))

  # Deux coefficients sous le meme nom decrivent deux preparations.
  k2 <- base; k2$Coefficient <- c(10, 8)
  r2 <- hstat_dilution_calcul(k2)
  expect_equal(nrow(r2), 0L)
  expect_true(grepl("Alpha", attr(r2, "message"), fixed = TRUE))

  # Une ligne sans nom de produit ne peut etre rattachee a aucune preparation.
  k3 <- base; k3$Produit <- c("Alpha", "")
  r3 <- hstat_dilution_calcul(k3)
  expect_equal(nrow(r3), 0L)
  expect_true(grepl("nommez-les", attr(r3, "message"), fixed = TRUE))

  # Une ligne ENTIEREMENT vide est un reste de saisie : elle se retire sans
  # un mot, sinon un tableau pre-rempli refuserait de calculer.
  k4 <- rbind(base, hstat_dilution_table_vide(2)[HSTAT_DILUTION_COLS])
  r4 <- hstat_dilution_calcul(k4)
  expect_equal(nrow(r4), 2L)
})

test_that("la gamme de filles se prend TOUJOURS dans la solution mere", {
  # Le prelevement se fait dans la mere a chaque etage : une erreur de
  # pipetage ne se propage donc pas d'une fille a la suivante. La gamme reste
  # geometrique -- C_n = C_mere / k^n -- mais Vi se lit sur la MERE, jamais
  # sur la fille precedente. Confondre les deux donne des volumes justes au
  # premier etage et faux partout ensuite, ce qui ne se voit pas.
  d <- data.frame(
    Produit = "P", Matiere_active = c("a", "b"),
    Concentration_mere = c(400, 50), Unite = "g/L",
    Coefficient = 10, Nb_filles = 4,
    Volume_final = 100, Unite_volume = "mL", stringsAsFactors = FALSE)
  r <- hstat_dilution_calcul(d)
  expect_equal(nrow(r), 8L)                       # 4 filles x 2 matieres actives
  expect_equal(sort(unique(r$Rang)), 1:4)

  a <- r[r$Matiere_active == "a", ]
  a <- a[order(a$Rang), ]
  expect_equal(a$Concentration_fille, 400 / 10^(1:4))
  # Chaque fille est la precedente divisee par le coefficient.
  expect_equal(a$Concentration_precedente, c(400, a$Concentration_fille[1:3]))

  # Vi x C_mere = Vf x Cf, sur CHAQUE fille, avec Ci = concentration MERE.
  p1 <- r[!is.na(r$Volume_a_prelever), ]
  for (i in seq_len(nrow(p1)))
    expect_equal(p1$Volume_a_prelever[i] * p1$Concentration_mere[i],
                 p1$Volume_final[i] * p1$Concentration_fille[i])
  # Donc Vi = Vf / k^n, et non Vf / k a tous les etages.
  expect_equal(sort(p1$Volume_a_prelever, decreasing = TRUE), 100 / 10^(1:4))

  # L'eau ajoutee est la difference Vf - Vi, a chaque fois.
  expect_equal(p1$Volume_eau_a_ajouter, p1$Volume_final - p1$Volume_a_prelever)

  # Ce que la mere doit fournir : la somme de tous les prelevements, puisqu'ils
  # viennent tous d'elle. Une seule ligne le porte -- c'est un total.
  expect_equal(sum(!is.na(r$Volume_mere_requis)), 1L)
  expect_equal(sum(r$Volume_mere_requis, na.rm = TRUE), sum(100 / 10^(1:4)))

  # Sans Nb_filles, le contrat d'avant la gamme tient : une fille par ligne.
  d2 <- d[, setdiff(names(d), "Nb_filles")]
  expect_equal(nrow(hstat_dilution_calcul(d2)), 2L)
})

test_that("un prelevement impipetable est nomme, pas rendu tel quel", {
  # 100 mL au 1/10 sur six etages demandent 0,0001 mL au dernier : le calcul
  # tient, la paillasse non. Un chiffre que personne ne peut mesurer serait
  # applique quand meme, faute d'un mot pour dire qu'il ne se mesure pas.
  d <- data.frame(
    Produit = "P", Matiere_active = "a", Concentration_mere = 400,
    Unite = "g/L", Coefficient = 10, Nb_filles = 6,
    Volume_final = 100, Unite_volume = "mL", stringsAsFactors = FALSE)
  r <- hstat_dilution_calcul(d)
  expect_equal(nrow(r), 6L)                       # le calcul n'est PAS bloque
  av <- attr(r, "avertissement")
  expect_true(!is.null(av) && nzchar(av))
  expect_true(grepl("P", av, fixed = TRUE))
  expect_true(grepl("augmentez|réduisez", av))

  # Une gamme realisable ne declenche rien : un avertissement permanent finit
  # par ne plus etre lu.
  d2 <- d; d2$Nb_filles <- 3
  expect_null(attr(hstat_dilution_calcul(d2), "avertissement"))

  # L'unite de volume compte : 100 L au meme rang restent pipetables.
  d3 <- d; d3$Unite_volume <- "L"
  expect_null(attr(hstat_dilution_calcul(d3), "avertissement"))
})

test_that("le nombre de filles est un entier borne, et il appartient au produit", {
  base <- data.frame(
    Produit = "P", Matiere_active = c("a", "b"),
    Concentration_mere = c(10, 20), Unite = "g/L",
    Coefficient = 10, Nb_filles = 3, Volume_final = 1, Unite_volume = "L",
    stringsAsFactors = FALSE)
  for (n in list(c(2.5, 2.5), c(0, 0), c(HSTAT_DILUTION_NB_MAX + 1, HSTAT_DILUTION_NB_MAX + 1))) {
    k <- base; k$Nb_filles <- n
    r <- hstat_dilution_calcul(k)
    expect_equal(nrow(r), 0L)
    expect_true(grepl("P", attr(r, "message"), fixed = TRUE))
  }
  # Deux nombres de filles sous le meme nom decrivent deux preparations.
  k <- base; k$Nb_filles <- c(3, 4)
  r <- hstat_dilution_calcul(k)
  expect_equal(nrow(r), 0L)
  expect_true(grepl("P", attr(r, "message"), fixed = TRUE))
})

# =============================================================================
#  DL50 / CL50 -- REGRESSION PROBIT DOSE-MORTALITE
# -----------------------------------------------------------------------------
#  Les valeurs de reference viennent d'un fichier de resultats produit par WIN
#  DL lui-meme (CL94AC1.PRN, CIRAD) : 7 doses, 25 insectes par dose, temoin
#  25/0. Elles sont inscrites ici en dur -- un test qui dependrait d'une
#  archive televersee ne tournerait pas en integration continue, et c'est
#  precisement ce test-la qui doit tourner a chaque modification du noyau.
# =============================================================================

.hstat_dl50_essai_ref <- function()
  hstat_dl50_essai(c(0.00063, 0.00125, 0.0025, 0.005, 0.01, 0.02, 0.03),
                   rep(25, 7), c(5, 7, 9, 11, 14, 18, 20),
                   temoin_n = 25, temoin_morts = 0,
                   titre = "C. leucotreta reference cyfluthrine 94")

test_that("l'ajustement probit reproduit les resultats de WIN DL", {
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  expect_true(isTRUE(f$ok))

  # Parametres de la droite de Henry et mortalite naturelle.
  expect_equal(f$a, 2.19760, tolerance = 1e-4)
  expect_equal(f$b, 0.97319, tolerance = 1e-4)
  expect_equal(f$c, 0, tolerance = 1e-6)

  # Les deux log-vraisemblances, dans la convention du logiciel : SANS les
  # coefficients binomiaux. R en rendrait -12.55876 -- l'ecart est exactement
  # sum(log(choose(n, x))), et il ne depend pas des parametres.
  expect_equal(f$ll0, -105.63592, tolerance = 1e-4)
  expect_equal(f$ll1, -105.29973, tolerance = 1e-5)
  ecart <- sum(log(choose(25, c(5, 7, 9, 11, 14, 18, 20))))
  expect_equal(f$ll1 + ecart,
               sum(stats::dbinom(c(5, 7, 9, 11, 14, 18, 20), 25,
                                 c(5, 7, 9, 11, 14, 18, 20) / 25, log = TRUE)))

  # Ajustement : le Chi-2 est la DEVIANCE, pas celui de Pearson (qui vaudrait
  # 0.668). Le degre de liberte est celui du manuel : nombre de doses - 2.
  expect_equal(f$chi2, 0.672, tolerance = 1e-3)
  expect_equal(f$ddl, 5L)
  expect_equal(f$p_chi2, 0.9845, tolerance = 1e-3)
  expect_false(f$heterogene)

  # Les six termes de variance, issus de l'inversion de la matrice
  # d'information de FISHER a TROIS parametres.
  expect_equal(sqrt(f$Vh[1, 1]), 7.67643e-01, tolerance = 1e-4)
  expect_equal(sqrt(f$Vh[2, 2]), 5.96156e-01, tolerance = 1e-4)
  expect_equal(sqrt(f$Vh[3, 3]), 4.38675e-01, tolerance = 1e-4)
  expect_equal(f$Vh[1, 2], 4.37167e-01, tolerance = 1e-4)
  expect_equal(f$Vh[1, 3], 2.79222e-01, tolerance = 1e-4)
  expect_equal(abs(f$Vh[2, 3]), 2.49109e-01, tolerance = 1e-4)

  dl <- hstat_dl50_doses_letales(f)
  expect_equal(dl$Seuil, c(90, 50, 10))
  # LES TOLERANCES SONT SERREES A DESSEIN. Elles valaient 1e-3 sur les doses
  # letales et 1e-2 sur leurs bornes tant que HStat inversait la normale
  # EXACTEMENT alors que WIN DL emploie l'approximation de HASTINGS : l'ecart
  # etait de 1,8e-4 en probit, soit 2e-4 en relatif sur la DL90. Depuis que
  # `.hstat_dl50_qnorm()` reprend l'approximation du logiciel, tout est retrouve
  # a 5e-5 pres -- c'est-a-dire aux six chiffres que WIN DL imprime.
  #
  # Relacher ces tolerances laisserait revenir `stats::qnorm` sans que rien ne
  # le signale, et le module cesserait d'etre comparable au logiciel qu'il
  # existe pour reproduire.
  expect_equal(dl$Log_dose, c(-9.41099e-01, -2.25814e+00, -3.57517e+00), tolerance = 3e-5)
  # Ce que WIN DL imprime sous le nom « Ecart-type » est l'ERREUR-TYPE de
  # l'estimation : c'est elle qui fonde les intervalles de confiance, et elle
  # diminue quand on teste plus d'individus.
  expect_equal(dl$Erreur_type, c(2.92820e-01, 6.71583e-01, 1.45538e+00), tolerance = 2e-4)
  expect_equal(dl$Dose, c(1.14525e-01, 5.51905e-03, 2.65967e-04), tolerance = 2e-4)
  expect_equal(dl$Limite_inf, c(3.05474e-02, 2.66417e-04, 3.73502e-07), tolerance = 2e-4)
  expect_equal(dl$Limite_sup, c(4.29366e-01, 1.14332e-01, 1.89393e-01), tolerance = 2e-4)

  # Le probit corrige de chaque dose, tel que le logiciel l'imprime -- compare
  # en ECART ABSOLU : le .PRN n'en donne que quatre decimales, une comparaison
  # relative se briserait sur la derniere, qui n'est qu'un arrondi.
  expect_lt(max(abs(f$table$Probit_corrige -
    c(-0.8415, -0.5825, -0.3580, -0.1507, 0.1506, 0.5825, 0.8415))), 1e-3)
})

test_that("l'inverse normale est celle de WIN DL, l'approximation de Hastings", {
  # Le manuel du logiciel l'ecrit : « algorithme d'approximation polynomiale de
  # la distribution normale inverse de HASTINGS ». C'est la formule 26.2.23
  # d'Abramowitz & Stegun, d'erreur bornee par 4,5e-4 -- et cette erreur EST
  # visible a la precision d'impression du logiciel.
  expect_equal(.hstat_dl50_qnorm(0.5), 0, tolerance = 1e-6)
  expect_equal(.hstat_dl50_qnorm(0.9), 1.2817288, tolerance = 1e-6)
  expect_equal(.hstat_dl50_qnorm(0.1), -1.2817288, tolerance = 1e-6)
  # Symetrique, et croissante.
  expect_equal(.hstat_dl50_qnorm(0.3), -.hstat_dl50_qnorm(0.7), tolerance = 1e-12)
  pp <- seq(0.001, 0.999, by = 0.001)
  expect_true(all(diff(.hstat_dl50_qnorm(pp)) > 0))
  # Elle reste dans la borne d'erreur annoncee par Abramowitz & Stegun.
  expect_lt(max(abs(.hstat_dl50_qnorm(pp) - stats::qnorm(pp))), 4.5e-4)
  # ET ELLE EST DIFFERENTE de la normale exacte la ou cela compte : c'est
  # precisement cet ecart qui rapproche la DL90 de la valeur publiee.
  expect_gt(abs(.hstat_dl50_qnorm(0.9) - stats::qnorm(0.9)), 1e-4)

  # LES VALEURS EXTREMES SONT AFFECTEES, pas calculees : le manuel pose
  # « 6 pour 100 % de mortalite et -6 pour 0 % ». La borne a 1e-12 employee
  # auparavant rendait +/- 7,0345, un nombre qui ne dependait que d'elle.
  expect_equal(hstat_dl50_probit(0), -6)
  expect_equal(hstat_dl50_probit(1), 6)
  expect_equal(hstat_dl50_probit(-0.2), -6)
  expect_equal(hstat_dl50_probit(0.5), 0, tolerance = 1e-6)
  expect_true(is.na(hstat_dl50_probit(NA)))

  # Le quantile de CONFIANCE, lui, reste exact -- et c'est mesure, pas suppose.
  # Les bornes publiees de la DL50 rendent t = 1,95999 ; la normale exacte donne
  # 1,959964 et Hastings 1,960395. C'est la premiere qui colle.
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  expect_equal(f$t, stats::qnorm(0.975), tolerance = 1e-9)
})

test_that("les essais livres avec WIN DL sont reproduits", {
  # Le logiciel est livre avec six essais d'exemple qui portent, en fin de
  # fichier, LES RESULTATS QU'IL A CALCULES : terme constant, pente, DL50 et
  # ses bornes. Ils couvrent deux cas que le fichier de reference ne couvre
  # pas -- un essai dont une dose tue TOUT (CL94AEN) et un essai dont le
  # TEMOIN COMPTE DES MORTS (CL97CY03), donc la correction d'Abbott.
  #
  # Ces resultats-la datent du moteur MS-DOS : leurs intervalles sont ceux de
  # la DELTA-METHODE seule, Fieller n'ayant ete ajoute qu'avec la version
  # Windows. On confronte donc les estimations ponctuelles et l'ERREUR-TYPE,
  # qui ne dependent pas de ce choix.
  ref <- list(
    list(nom = "CL94ACY", d = c(0.02, 0.01, 0.005, 0.0025, 0.00125, 0.00063),
         n = rep(25, 6), x = c(18, 14, 11, 9, 7, 5), n0 = 25, x0 = 0,
         a = 2.01915, b = 0.90698, dl = 0.00594, lo = 0.00336, hi = 0.01051),
    list(nom = "CL95DEL", d = c(0.00012, 0.00025, 0.0005, 0.001, 0.00215),
         n = rep(25, 5), x = c(10, 12, 16, 20, 22), n0 = 25, x0 = 0,
         a = 4.36366, b = 1.19857, dl = 0.00023, lo = 0.00014, hi = 0.0004,
         # Bornes stockees a DEUX chiffres significatifs : l'erreur-type qu'on
         # en deduirait porterait leur arrondi, pas le calcul. On ne la
         # confronte pas ici -- une tolerance assez large pour l'absorber ne
         # verifierait plus rien.
         se_verifiable = FALSE),
    list(nom = "CL94AEN", d = c(10, 4, 2, 1, 0.5, 0.25),
         n = rep(30, 6), x = c(30, 28, 22, 15, 10, 4), n0 = 25, x0 = 0,
         a = 0.11299, b = 2.12526, dl = 0.88477, lo = 0.69046, hi = 1.13378),
    list(nom = "CL97CY03", d = c(0.08476, 0.05053, 0.03423, 0.0163, 0.00848),
         n = rep(90, 5), x = c(82, 75, 62, 43, 30), n0 = 90, x0 = 4,
         a = 3.28559, b = 1.87104, dl = 0.01754, lo = 0.01456, hi = 0.02113))

  for (r in ref) {
    f <- hstat_dl50_ajuste(hstat_dl50_essai(r$d, r$n, r$x, r$n0, r$x0), "abbott")
    expect_true(isTRUE(f$ok), info = r$nom)
    expect_equal(f$a, r$a, tolerance = 3e-4, info = r$nom)
    expect_equal(f$b, r$b, tolerance = 3e-4, info = r$nom)
    dl <- hstat_dl50_doses_letales(f, 50)
    # Les DL50 stockees ne portent que deux a trois chiffres significatifs.
    expect_equal(dl$Dose[1], r$dl, tolerance = 6e-3, info = r$nom)

    # L'ERREUR-TYPE EST LA VERIFICATION QUI COMPTE. Les bornes stockees sont
    # symetriques en log -- donc issues de la delta-methode -- et l'ecart-type
    # s'en deduit : (log hi - log lo) / (2 t). Il doit valoir celui de HStat.
    # C'est cette quantite, et elle seule, qui distingue l'inversion a DEUX
    # parametres de l'inversion a TROIS : sous Abbott, `c` est declaree, et le
    # logiciel ne lui fait pas payer d'incertitude -- son manuel le dit
    # (« ce que la formule d'ABBOTT ne fait pas »), ses chiffres le confirment.
    if (!identical(r$se_verifiable, FALSE)) {
      se_ref <- (log10(r$hi) - log10(r$lo)) / (2 * f$t)
      expect_equal(dl$Erreur_type[1], se_ref, tolerance = 5e-3, info = r$nom)
    }
  }

  # ET L'INVERSION A TROIS PARAMETRES EN EST LOIN. Sur l'essai a temoin non nul,
  # elle gonfle l'erreur-type d'un facteur qui se compte, pas qui se discute.
  f <- hstat_dl50_ajuste(hstat_dl50_essai(
    c(0.08476, 0.05053, 0.03423, 0.0163, 0.00848), rep(90, 5),
    c(82, 75, 62, 43, 30), 90, 4), "abbott")
  I <- .hstat_dl50_fisher(log10(f$essai$doses$dose), f$essai$doses$n, f$a, f$b, f$c)
  expect_gt(solve(I)[2, 2] / f$V[2, 2], 5)
})

test_that("erreur-type et ecart-type ne mesurent pas la meme chose", {
  # Les confondre est l'erreur classique du bioessai, et elle change la
  # conclusion : l'erreur-type mesure la PRECISION DE L'ESTIMATION et diminue
  # quand on teste plus d'individus ; l'ecart-type mesure la DISPERSION DES
  # SENSIBILITES dans la population et ne diminue pas.
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  dl <- hstat_dl50_doses_letales(f)

  # L'ecart-type des tolerances vaut 1/b : il est le MEME pour toutes les
  # doses letales, parce qu'il est une propriete de la pente, pas du seuil.
  expect_equal(unique(dl$Ecart_type), 1 / abs(f$b))
  expect_equal(length(unique(dl$Ecart_type)), 1L)
  # L'erreur-type, elle, change d'un seuil a l'autre. Elle n'est PAS minimale a
  # la DL50 -- on le croit, et c'est faux : var(m) est un polynome du second
  # degre en m, minimal en m* = -Vab/Vbb, qui ne coincide avec la DL50 que si
  # la covariance de a et b l'y met. Sur l'essai de reference, la DL90 est plus
  # precise que la DL50.
  expect_gt(length(unique(dl$Erreur_type)), 1L)
  m_etoile <- -f$Vh[1, 2] / f$Vh[2, 2]
  expect_equal(which.min(dl$Erreur_type),
               which.min(abs(dl$Log_dose - m_etoile)))
  expect_gt(abs(dl$Log_dose[dl$Seuil == 50] - m_etoile), 0.5)

  # La verification qui les separe : 10^(log DL50 +/- 1/b) rend exactement la
  # DL84 et la DL16 -- l'ecart-type decrit la courbe, pas l'essai.
  m50 <- dl$Log_dose[dl$Seuil == 50]
  s <- dl$Ecart_type[1]
  attendu <- hstat_dl50_doses_letales(f, seuils = c(pnorm(1) * 100, pnorm(-1) * 100))
  # L'identite est EXACTE dans le modele, et retrouvee ici a la precision de
  # l'inverse normale employee : `1/b` est calcule exactement, le seuil passe
  # par l'approximation de Hastings, celle de WIN DL. L'ecart residuel est de
  # 7e-5 en relatif -- c'est le prix, mesure, de la conformite au logiciel.
  expect_equal(10^(m50 + s), attendu$Dose[1], tolerance = 1e-3)
  expect_equal(10^(m50 - s), attendu$Dose[2], tolerance = 1e-3)

  # Et l'erreur-type SEULE diminue quand on double les effectifs : c'est ce qui
  # distingue une estimation plus precise d'une population plus homogene.
  e2 <- hstat_dl50_essai(c(0.00063, 0.00125, 0.0025, 0.005, 0.01, 0.02, 0.03),
                         rep(250, 7), c(50, 70, 90, 110, 140, 180, 200), 250, 0)
  d2 <- hstat_dl50_doses_letales(hstat_dl50_ajuste(e2, "em"))
  expect_lt(d2$Erreur_type[d2$Seuil == 50], dl$Erreur_type[dl$Seuil == 50])
  expect_equal(d2$Ecart_type[1], dl$Ecart_type[1], tolerance = 0.05)

  # Les deux colonnes « ± » encadrent la dose, et celle de l'erreur-type est
  # la plus etroite des deux ici (l'essai est petit mais la pente est faible).
  expect_true(all(nzchar(dl$DL_ecart_type)))
  expect_true(all(nzchar(dl$DL_erreur_type)))
  expect_true(all(grepl("–", dl$DL_ecart_type, fixed = TRUE)))
})

test_that("le tableau des parametres porte toutes les statistiques", {
  # Un chiffre lu dans un paragraphe ne se recopie pas dans un rapport et ne
  # s'exporte pas : le tableau doit les porter tous.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  mod <- paste(readLines(file.path(root, "R", "mod_dl50.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (cle in c("Terme constant (a)", "Pente (b)", "Mortalité naturelle (c)",
                "Écart-type des tolérances (1/b)", "Covariance a-b (Vab)",
                "Log-vraisemblance du modèle (H0)", "Log-vraisemblance saturée (H1)",
                "Chi-2 d'ajustement", "Degrés de liberté",
                "Probabilité de dépassement du Chi-2", "Facteur d'hétérogénéité",
                "Quantile employé pour les intervalles", "Risque α",
                "Nombre de doses", "Itérations"))
    expect_true(grepl(cle, mod, fixed = TRUE), info = cle)
})

test_that("chaque reglage du graphique est declare, lu et observe", {
  # Un reglage que le reactif n'observe pas se change sans que l'image bouge.
  # Le balayage exige les trois : declare dans l'interface, lu dans le reactif
  # d'options, et pris en compte par la fonction de trace.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- paste(readLines(file.path(root, "R", "mod_dl50.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui <- regmatches(src, gregexpr('ns\\("(g[A-Za-z0-9]+)"\\)', src, perl = TRUE))[[1]]
  ui <- unique(sub('.*ns\\("([^"]+)"\\).*', "\\1", ui))
  # UNE SORTIE N'EST PAS UN REGLAGE, et elle se reconnait a son constructeur.
  # La liste etait tenue a la main : elle a derive des qu'une note et un titre
  # dynamique se sont ajoutes sous le graphique, et le test a signale deux
  # sorties comme des reglages morts. On la derive donc de la source -- un
  # affichage ajoute demain sort tout seul du decompte.
  sorties <- regmatches(src, gregexpr(
    '(?:uiOutput|textOutput|verbatimTextOutput|plotOutput|DTOutput|downloadButton)\\(\\s*ns\\("(g[A-Za-z0-9]*|graphe)"\\)',
    src, perl = TRUE))[[1]]
  sorties <- unique(sub('.*ns\\("([^"]+)"\\).*', "\\1", sorties))
  expect_true(all(c("graphe", "gNote", "gTaille") %in% sorties))
  # Les reglages d'EXPORT sont bien des entrees, mais ils ne passent pas par le
  # reactif de trace : ils ont leur propre chemin, celui du telechargement.
  ui <- setdiff(ui, c(sorties, "gFmt", "gLargeur", "gHauteur", "gDpi",
                      "gQualite", "gCompression", "gTous"))
  # Le reactif d'options est le SEUL endroit ou les reglages sont lus : c'est
  # donc lui qu'on balaie. Chercher « input$<id> » ne suffirait pas -- la
  # plupart passent par `nb("<id>", defaut)`, donc par `input[[id]]`.
  deb <- regexpr("graphe_opt <- shiny::reactive({", src, fixed = TRUE)
  expect_gt(deb, 0)
  # La fin se cherche APRES le debut, et sur une chaine que rien d'autre ne
  # contient : « graphe <- shiny::reactive({ » est un morceau de
  # « fits_graphe <- shiny::reactive({ », et le decoupage rendait un bloc VIDE
  # -- un test qui ne balaie rien passe toujours, sauf quand il tombe a
  # l'envers.
  reste <- substr(src, deb, nchar(src))
  fin <- regexpr("\n    graphe <- shiny::reactive({", reste, fixed = TRUE)
  expect_gt(fin, 0)
  corps <- substr(reste, 1, fin)
  expect_gt(nchar(corps), 1000)
  lus <- vapply(ui, function(id)
    grepl(paste0('"', id, '"'), corps, fixed = TRUE) ||
    grepl(paste0("input$", id), corps, fixed = TRUE), logical(1))
  expect_equal(ui[!lus], character(0),
               info = paste("Réglages déclarés mais jamais lus par le réactif :",
                            paste(ui[!lus], collapse = ", ")))
  # Chaque cle de la liste d'options a une valeur par defaut declaree, et la
  # fonction de trace s'en sert : une cle absente du defaut serait NULL.
  expect_true(all(names(HSTAT_DL50_OPT_DEFAUT) %in% names(.hstat_dl50_opt())))
  o <- .hstat_dl50_opt(list(titre = "Essai", point_taille = 9))
  expect_equal(o$titre, "Essai")
  expect_equal(o$point_taille, 9)
  expect_equal(o$theme, HSTAT_DL50_OPT_DEFAUT$theme)
})

test_that("le graphique se trace avec les reglages, et les limites sont en doses", {
  skip_if_not_installed("ggplot2")
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  p <- hstat_dl50_graphique(list(f))
  expect_s3_class(p, "ggplot")
  # Les limites de l'axe des doses se saisissent EN DOSES : personne ne
  # raisonne en log10 devant un plan d'essai.
  p2 <- hstat_dl50_graphique(list(f), list(x_min = 0.001, x_max = 0.1))
  expect_s3_class(p2, "ggplot")
  expect_equal(p2$coordinates$limits$x, log10(c(0.001, 0.1)), tolerance = 1e-9)
  # Une fenetre vide ne rend pas un graphique faux : elle ne rend rien.
  expect_null(hstat_dl50_graphique(list(f), list(x_min = 1, x_max = 0.001)))
  expect_null(hstat_dl50_graphique(list()))
  # Chaque famille de reglages se pose sans lever.
  for (o in list(list(points = FALSE, droite = FALSE, bande = FALSE),
                 list(courbe = TRUE, reperes = FALSE, axe2 = FALSE),
                 list(grille = FALSE, legende_pos = "none", theme = "classic"),
                 list(point_forme = "17", droite_type = "dashed",
                      titre = "T", sous_titre = "S"),
                 # L'INCLINAISON VIENT DESORMAIS DU KIT, et pour les deux axes :
                 # `gGradXAngle` a disparu au profit de `gAngleX`/`gAngleY`.
                 list(extras = list(familles = c("angles", "pas"),
                                    angle_x = 45, angle_y = 30,
                                    pas = c(x = NA_real_, y = NA_real_)))))
    expect_s3_class(hstat_dl50_graphique(list(f), o), "ggplot")
})

test_that("la palette par defaut de ggplot2 ne plafonne pas au nombre d'essais", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("RColorBrewer")

  # 1. LA SENTINELLE N'EST PAS UN NOM RColorBrewer, et ne doit pas rejoindre la
  #    liste des qualitatives : un test verifie que chacune existe chez Brewer,
  #    et l'y glisser ferait tomber le graphique de qui l'aurait choisie.
  expect_false(unname(HSTAT_PALETTE_GG) %in%
                 rownames(RColorBrewer::brewer.pal.info))
  expect_false(unname(HSTAT_PALETTE_GG) %in%
                 c(unname(HSTAT_PALETTES_QUALI), unname(HSTAT_PALETTES_DEGRADE)))

  # 2. CE QUE LA PALETTE APPORTE, mesure sur les couleurs REELLEMENT RENDUES et
  #    non sur l'argument passe. Les qualitatives de Brewer plafonnent -- Set2,
  #    Dark2 et Accent a 8 couleurs, Set1 et Pastel1 a 9 -- et au-dela ggplot
  #    AVERTIT puis rend les series surnumeraires en gris. Sur un reglage nomme
  #    « plusieurs essais », c'est le cas qu'on rencontre pour de vrai.
  rendues <- function(sc, n) {
    d <- data.frame(x = seq_len(n), y = seq_len(n),
                    g = factor(paste0("essai", seq_len(n))))
    g <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point() + sc
    unique(suppressWarnings(ggplot2::ggplot_build(g))$data[[1]]$colour)
  }
  brewer12 <- suppressWarnings(
    rendues(ggplot2::scale_colour_brewer(palette = "Set1"), 12))
  hue12 <- rendues(ggplot2::scale_colour_hue(), 12)
  expect_true(any(is.na(brewer12) | brewer12 == "grey50"))
  expect_equal(length(hue12), 12L)
  expect_false(any(is.na(hue12) | hue12 == "grey50"))

  # 3. LE TRACE LA POSE VRAIMENT. Passee a `scale_*_brewer()`, la sentinelle
  #    serait un nom inconnu : le trace retomberait sur Set1 SANS RIEN DIRE.
  #    On compare donc les COULEURS que l'echelle du graphique engendre --
  #    « hue » et « brewer » partagent leur classe (`ScaleDiscrete`), le nom de
  #    classe ne les distingue pas.
  f1 <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  f2 <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "abbott")
  couleurs_du_trace <- function(pal, n = 5L) {
    p <- hstat_dl50_graphique(list(f1, f2), list(palette = pal))
    expect_s3_class(p, "ggplot")
    for (s in p$scales$scales)
      if (identical(s$aesthetics[1], "colour")) return(s$palette(n))
    character(0)
  }
  attendu_hue <- ggplot2::scale_colour_hue()$palette(5L)
  attendu_set1 <- ggplot2::scale_colour_brewer(palette = "Set1")$palette(5L)
  expect_equal(couleurs_du_trace(unname(HSTAT_PALETTE_GG)), attendu_hue)
  expect_equal(couleurs_du_trace("Set1"), attendu_set1)
  # Les deux echelles different bien : sans cela les deux egalites ci-dessus
  # seraient vraies pour la meme raison, et le test ne garderait rien.
  expect_false(identical(attendu_hue, attendu_set1))

  # 4. Un seul essai : pas d'echelle de groupes, la couleur unique s'applique.
  expect_s3_class(hstat_dl50_graphique(list(f1),
                    list(palette = unname(HSTAT_PALETTE_GG))), "ggplot")
})

test_that("la courbe dose-reponse porte la mortalite observee, bornee par 0 et 100", {
  skip_if_not_installed("ggplot2")
  # Un essai avec une dose a 0 mort et une dose ou tout meurt : c'est le cas
  # normal d'un bioessai bien concu, qui ENCADRE la reponse.
  d <- c(0.1, 0.2, 0.5, 1, 2, 5)
  f <- hstat_dl50_ajuste(
    hstat_dl50_essai(d, rep(30, 6), c(0, 4, 12, 20, 27, 30), 30, 3), "em")
  expect_true(isTRUE(f$ok))

  couche <- function(ty, champ) {
    b <- ggplot2::ggplot_build(hstat_dl50_graphique(list(f), list(type = ty)))
    k <- which(vapply(b$data, function(z) champ %in% names(z), logical(1)))[1]
    list(d = b$data[[k]], y = b$layout$panel_params[[1]]$y.range)
  }

  # 1. LES DOSES EXTREMES. Une mortalite corrigee de 0 % ou de 100 % n'a pas de
  #    probit : `hstat_dl50_probit()` la ramene a +/- 7,03, une valeur qui ne
  #    mesure rien -- elle depend de l'epsilon de troncature. Elle est donc
  #    ecartee de la droite de Henry, et NOMMEE. La courbe dose-reponse, elle,
  #    les porte toutes les six.
  pr <- couche("probit", "shape"); rp <- couche("reponse", "shape")
  expect_equal(nrow(pr$d), 4L)
  expect_equal(nrow(rp$d), 6L)
  ec <- attr(hstat_dl50_graphique(list(f), list(type = "probit")), "ecartes")
  expect_true(length(ec) == 1L && grepl("0.1", ec, fixed = TRUE) &&
              grepl("5", ec, fixed = TRUE))
  expect_null(attr(hstat_dl50_graphique(list(f), list(type = "reponse")), "ecartes"))

  # 2. L'AXE N'EST PLUS ETIRE PAR DEUX ARTEFACTS. Avant, l'etendue se calculait
  #    sur les points tronques et allait de -7,7 a +7,7 : les quatre points
  #    reels tenaient dans 17 % de la hauteur.
  expect_lt(diff(pr$y), 8)
  expect_equal(rp$y, c(-5, 105), tolerance = 1e-6)

  # 3. LA BANDE RESTE DANS [0 ; 100]. Elle est construite sur le probit puis
  #    transportee par F, qui est monotone. La batir directement sur le
  #    pourcentage la ferait sortir du cadre aux extremes, la ou l'on lit.
  rb <- couche("reponse", "ymin")$d
  expect_gte(min(rb$ymin), 0)
  expect_lte(max(rb$ymax), 100)
  # Et elle ne descend jamais sous la mortalite naturelle : la courbe part de c.
  expect_gte(min(rb$ymin), 100 * f$c - 1e-8)

  # 4. LA DL50 N'EST PAS A 50 % DE MORTALITE OBSERVEE. Elle est definie sur la
  #    mortalite CORRIGEE ; le repere doit donc passer la ou la courbe ajustee
  #    coupe la DL50, soit c + (1 - c)/2. Avec un temoin nul les deux
  #    coincident -- l'erreur serait invisible sur les essais les plus propres.
  dl50 <- hstat_dl50_doses_letales(f, 50)$Dose[1]
  attendu <- 100 * hstat_dl50_mortalite(f, dl50)$Mortalite
  # Tolerance a la precision de l'inverse normale de WIN DL : la DL50 passe par
  # l'approximation de Hastings, la mortalite attendue par `pnorm` exact.
  expect_equal(attendu, 100 * (f$c + (1 - f$c) * 0.5), tolerance = 1e-4)
  expect_gt(attendu, 50)   # le temoin meurt : le repere est AU-DESSUS de 50 %

  # 5. Les bornes de l'axe se saisissent en pourcentage dans les deux cas.
  b <- ggplot2::ggplot_build(hstat_dl50_graphique(list(f),
         list(type = "reponse", y_min = 20, y_max = 80)))
  expect_equal(b$plot$coordinates$limits$y, c(20, 80), tolerance = 1e-9)
})

test_that("la matrice d'information est assemblee sur les doses seules", {
  # C'est la convention de WIN DL, et elle ne se devine pas. Le lot temoin
  # apporte pourtant de l'information sur c -- mais sa contribution vaut
  # n0/(c(1-c)), donc INFINIE des que c = 0, ce qui rendrait ET(c) = 0. Le
  # logiciel affiche 0.4387 sur un essai ou c vaut exactement zero.
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  expect_gt(sqrt(f$V[3, 3]), 0.4)
  # Reprendre le choix inverse (temoin inclus) ferait tendre ET(c) vers zero :
  # on le verifie en ajoutant a la main la contribution du temoin.
  z <- log10(f$essai$doses$dose)
  I <- solve(f$V)
  I[3, 3] <- I[3, 3] + f$essai$n0 / (1e-8 * (1 - 1e-8))
  expect_lt(sqrt(solve(I)[3, 3]), 1e-3)
})

test_that("une mortalite naturelle DECLAREE n'est pas un parametre estime", {
  # C'est la faute la plus couteuse que ce module ait portee, et elle etait
  # entierement silencieuse. Sous « Abbott » et « mortalite nulle », c est
  # DECLAREE -- lue sur le temoin, ou posee a zero. La matrice d'information a
  # trois lignes etait pourtant inversee comme si c avait ete estimee : le bloc
  # (a, b) payait alors une incertitude sur c que l'hypothese exclut.
  ref <- .hstat_dl50_essai_ref()
  d <- ref$doses$dose; n <- ref$doses$n; x <- ref$doses$x

  # LA VERIFICATION QUI TRANCHE : a c = 0, le modele est EXACTEMENT un GLM
  # binomial a lien probit sur le log10 de la dose. `glm()` est la reference
  # universelle, et les deux doivent coincider -- pas approcher.
  f0 <- hstat_dl50_ajuste(ref, "nulle")
  g <- stats::glm(cbind(x, n - x) ~ log10(d), family = stats::binomial(link = "probit"))
  expect_equal(unname(c(f0$a, f0$b)), unname(stats::coef(g)), tolerance = 1e-5)
  expect_equal(unname(f0$V[1:2, 1:2]), unname(stats::vcov(g)), tolerance = 1e-5)

  # Et l'erreur-type de la DL50 rejoint celle de MASS::dose.p, qui applique la
  # delta-methode au meme ajustement.
  if (requireNamespace("MASS", quietly = TRUE)) {
    dp <- MASS::dose.p(g, p = 0.5)
    dl0 <- hstat_dl50_doses_letales(f0, 50)
    expect_equal(dl0$Log_dose[1], as.numeric(dp), tolerance = 1e-6)
    expect_equal(dl0$Erreur_type[1], as.numeric(attr(dp, "SE")), tolerance = 1e-6)
  }

  # L'ampleur de la faute, pour qu'elle ne revienne pas sans se voir : inverser
  # les trois lignes rendait var(a) = 0,589 au lieu de 0,184, et une
  # erreur-type de DL50 6,5 fois trop grande.
  I <- .hstat_dl50_fisher(log10(d), n, f0$a, f0$b, 0)
  expect_gt(solve(I)[1, 1] / f0$V[1, 1], 3)

  # c n'etant pas estimee, son ecart-type et ses covariances N'EXISTENT PAS.
  # `NA` dit « sans objet » ; zero dirait « connue exactement », ce qui n'est
  # pas la question posee.
  expect_true(is.na(f0$V[3, 3]) && is.na(f0$V[1, 3]) && is.na(f0$V[2, 3]))
  expect_true(all(is.finite(f0$V[1:2, 1:2])))

  # Abbott : c fixee a la valeur du temoin, meme regle. On la controle contre
  # le hessien OBSERVE de la log-vraisemblance a c fixee -- information
  # attendue contre observee, elles ne coincident qu'a la taille d'echantillon
  # pres, d'ou la tolerance large ; ce qui est verifie ici, c'est la DIMENSION.
  ab <- hstat_dl50_ajuste(hstat_dl50_essai(d, n, x, 25, 3), "abbott")
  Iab <- .hstat_dl50_fisher(log10(d), n, ab$a, ab$b, ab$c)
  expect_equal(unname(ab$V[1:2, 1:2]), unname(solve(Iab[1:2, 1:2])), tolerance = 1e-9)
  expect_gt(solve(Iab)[1, 1] / ab$V[1, 1], 2)

  # L'INCOHERENCE ETAIT INTERNE : `npar` vaut deja 2 pour ces methodes -- c'est
  # lui qui decide si le Chi-2 garde un degre de liberte residuel. Le meme
  # ajustement comptait deux parametres pour le test et trois pour les
  # variances.
  expect_equal(f0$npar, 2L)
  expect_equal(hstat_dl50_ajuste(ref, "em")$npar, 3L)

  # EM : c EST estimee, les trois lignes restent -- et c'est le cas verifie
  # contre WIN DL, qui ne doit pas bouger d'un chiffre.
  fem <- hstat_dl50_ajuste(ref, "em")
  expect_true(all(is.finite(fem$V)))
  expect_equal(sqrt(fem$V[3, 3]), 4.38675e-01, tolerance = 1e-4)
})


test_that("la saisie en pourcentage arrondit, et le dit", {
  # Beaucoup d'operateurs notent « 40 % » plutot que « 12 sur 30 ». La
  # conversion est triviale ; ce qui ne l'est pas, c'est que le modele binomial
  # a besoin d'un ENTIER.

  # 1. LE CAS EXACT ne signale rien.
  r <- hstat_dl50_pct_vers_morts(c(30, 30), c(40, 50))
  expect_equal(as.numeric(r), c(12, 15))
  expect_equal(attr(r, "arrondies"), integer(0))

  # 2. L'ARRONDI CHANGE LE POURCENTAGE, et il est nomme. 40 % de 7 font 2,8 :
  #    on enregistre 3, soit 42,857 %.
  r <- hstat_dl50_pct_vers_morts(7, 40)
  expect_equal(as.numeric(r), 3)
  expect_equal(attr(r, "arrondies"), 1L)
  expect_equal(attr(r, "ecart"), 100 * 3 / 7 - 40, tolerance = 1e-9)

  # 3. DEUX POURCENTAGES DIFFERENTS DONNENT LE MEME EFFECTIF. C'est la raison
  #    de fond de l'avertissement : une saisie plus fine que l'essai ne
  #    l'autorise promet une precision qui n'existe pas.
  expect_equal(as.numeric(hstat_dl50_pct_vers_morts(7, 40)),
               as.numeric(hstat_dl50_pct_vers_morts(7, 43)))

  # 4. LES MOITIES VONT VERS LE HAUT. `round()` arrondit au PAIR en R :
  #    `round(2.5)` vaut 2. « 50 % de 5 individus » rendrait donc 2, ce que
  #    personne n'attend -- et le defaut serait invisible sauf sur les
  #    effectifs impairs.
  expect_equal(as.numeric(hstat_dl50_pct_vers_morts(5, 50)), 3)
  expect_equal(as.numeric(hstat_dl50_pct_vers_morts(5, 30)), 2)
  expect_false(identical(as.numeric(hstat_dl50_pct_vers_morts(5, 50)),
                         as.numeric(round(5 * 0.5))))

  # 5. LES BORNES SONT TENUES : jamais moins de zero mort, jamais plus que
  #    l'effectif -- une saisie a 120 % est une faute de frappe, pas un essai.
  expect_equal(as.numeric(hstat_dl50_pct_vers_morts(20, c(-5, 0, 100, 120))),
               c(0, 0, 20, 20))

  # 6. UN POURCENTAGE SANS EFFECTIF NE DONNE RIEN, et les lignes sont comptees.
  r <- hstat_dl50_pct_vers_morts(c(NA, 0, 25), c(50, 50, 50))
  expect_true(all(is.na(as.numeric(r)[1:2])))
  expect_equal(as.numeric(r)[3], 12.5 + 0.5)
  expect_equal(attr(r, "sans_effectif"), c(1L, 2L))

  # 7. LE CHEMIN INVERSE est une simple lecture, et il ne perd rien.
  expect_equal(hstat_dl50_morts_vers_pct(c(30, 7, NA, 0), c(12, 3, 5, 5)),
               c(40, 100 * 3 / 7, NA, NA))
  n <- c(25, 30, 90); x <- c(5, 12, 43)
  expect_equal(as.numeric(hstat_dl50_pct_vers_morts(
                 n, hstat_dl50_morts_vers_pct(n, x))), x)

  # Et l'ajustement ne voit aucune difference : ce sont les memes effectifs.
  d <- c(0.00063, 0.00125, 0.0025, 0.005, 0.01, 0.02, 0.03)
  xx <- c(5, 7, 9, 11, 14, 18, 20)
  pct <- hstat_dl50_morts_vers_pct(rep(25, 7), xx)
  f1 <- hstat_dl50_ajuste(hstat_dl50_essai(d, rep(25, 7), xx, 25, 0), "em")
  f2 <- hstat_dl50_ajuste(hstat_dl50_essai(
    d, rep(25, 7), as.numeric(hstat_dl50_pct_vers_morts(rep(25, 7), pct)),
    25, 0), "em")
  expect_equal(f1$a, f2$a); expect_equal(f1$b, f2$b)
})

test_that("le plafond d'iterations est un reglage, borne et respecte", {
  ref <- .hstat_dl50_essai_ref()
  # Le defaut vaut cent, et il suffit tres largement : l'essai de reference
  # converge en trois boucles.
  expect_equal(HSTAT_DL50_ITMAX, 100L)
  f <- hstat_dl50_ajuste(ref, "em")
  expect_equal(f$itmax, 100L)
  expect_lt(f$iterations, 10L)
  expect_true(f$converge)

  # UN PLAFOND TROP BAS ARRETE LE CALCUL, ET LE DIT. Sans quoi on publierait un
  # ajustement interrompu comme s'il avait converge.
  f1 <- hstat_dl50_ajuste(ref, "em", itmax = 1)
  expect_equal(f1$itmax, 1L)
  expect_equal(f1$iterations, 1L)
  expect_false(f1$converge)

  # Le releve donne le meme resultat que le defaut : le plafond ne change rien
  # tant qu'il n'est pas atteint.
  f2 <- hstat_dl50_ajuste(ref, "em", itmax = 1000)
  expect_equal(f2$a, f$a); expect_equal(f2$b, f$b)

  # LES SAISIES ABERRANTES SONT BORNEES, pas propagees : un champ numerique
  # accepte le vide, le zero, le negatif et le texte.
  for (v in list(NA, 0, -5, "", "abc", NULL))
    expect_equal(hstat_dl50_ajuste(ref, "em", itmax = v)$itmax, HSTAT_DL50_ITMAX)
  expect_equal(hstat_dl50_ajuste(ref, "em", itmax = 1e9)$itmax, HSTAT_DL50_ITMAX_MAX)

  # Le plafond vaut AUSSI pour Newton-Raphson quand c'est lui l'ajustement --
  # Abbott, mortalite nulle. Emboite dans l'EM il garde les 50 du manuel.
  expect_equal(HSTAT_DL50_ITMAX_NR, 50L)
  fa <- hstat_dl50_ajuste(ref, "nulle", itmax = 2)
  expect_lte(fa$iterations, 2L)
})

test_that("la table de saisie est rafraichie par son proxy, sans exception", {
  # J'AI CRU DEUX FOIS A UN DEFAUT QUI N'EXISTE PAS. Une cellule fraichement
  # modifiee semblait s'afficher VIDE, et j'ai construit un contournement :
  # sauter la mise a jour du proxy quand le changement naissait dans la table.
  #
  # `innerText` d'une cellule en cours d'edition rend la chaine vide, parce que
  # DT y a place son editeur `<input type="number">` et que le texte d'un champ
  # de saisie n'est pas du texte de noeud. La MESURE etait fausse, pas
  # l'affichage : des que le focus quitte la cellule, elle montre la valeur.
  #
  # Le contournement apportait, lui, une vraie regression : en pourcentage la
  # cellule aurait garde le chiffre TAPE (« 50 ») alors que l'arrondi range 4
  # morts sur 7, soit 57,14 %. L'ecran aurait cesse de dire la verite pour
  # eviter un defaut inexistant. Ce test barre la route au retour du drapeau.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  mod <- paste(readLines(file.path(root, "R", "mod_dl50.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  prox <- regmatches(mod, regexpr(
    "(?s)shiny::observeEvent\\(saisie_affichee\\(\\).*?ignoreInit = TRUE\\)", mod, perl = TRUE))
  expect_equal(length(prox), 1L)
  expect_true(grepl("DT::replaceData(proxy_saisie", prox, fixed = TRUE))
  # AUCUNE SORTIE ANTICIPEE : le rafraichissement vaut pour tous les
  # changements, y compris ceux nes dans la table.
  expect_false(grepl("return()", prox, fixed = TRUE))
  expect_false(grepl("depuis_table", mod, fixed = TRUE))
})

test_that("un seuil de dose letale se filtre dans la fonction, pas chez l'appelant", {
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")

  # UNE DOSE LETALE A 0 % OU A 100 % N'EXISTE PAS : l'inverse normale y vaut
  # l'infini. La fonction rendait une ligne de `NaN` -- un tableau de resultats
  # qui affiche NaN sans rien dire.
  for (s in list(0, 100, 150, -10, NA)) {
    d <- hstat_dl50_doses_letales(f, s)
    expect_equal(nrow(d), 0L, info = paste(s))
    expect_true(nzchar(attr(d, "message") %||% ""), info = paste(s))
  }

  # UNE LISTE VIDE LEVAIT « invalid argument to unary operator » : le champ des
  # seuils qu'on efface pour le retaper faisait tomber tout le tableau.
  # L'interface filtrait avant d'appeler, mais elle n'est pas le seul appelant.
  expect_silent(d0 <- hstat_dl50_doses_letales(f, numeric(0)))
  expect_equal(nrow(d0), 0L)
  # Les colonnes restent celles du tableau plein : un appelant qui les nomme ne
  # doit pas casser sur un resultat vide.
  expect_true(all(c("Seuil", "Dose", "Erreur_type", "Limite_inf", "Intervalle")
                  %in% names(d0)))

  # LES SEUILS VALIDES SURVIVENT, LES AUTRES SONT NOMMES.
  d <- hstat_dl50_doses_letales(f, c(10, 0, 50, 120, 90))
  expect_equal(nrow(d), 3L)
  expect_equal(sort(d$Seuil), c(10, 50, 90))
  expect_true(grepl("0", attr(d, "ecartes"), fixed = TRUE))
  expect_true(grepl("120", attr(d, "ecartes"), fixed = TRUE))
})

test_that("un essai de reference hors bornes est refuse, pas remplace", {
  A <- .hstat_dl50_essai_ref()
  B <- hstat_dl50_essai(c(0.001, 0.002, 0.005, 0.01, 0.02), rep(30, 5),
                        c(4, 9, 15, 22, 27), 30, 1)
  # C'EST LE DEFAUT LE PLUS DIFFICILE A VOIR : rien n'est vide, rien ne leve.
  # L'indice retombait sur le premier essai, et demander le rapport par rapport
  # a l'essai 9 sur un jeu qui en compte deux rendait le tableau du premier,
  # sa colonne « Reference » cochee sur lui. Un resultat plausible, et pas
  # celui qu'on avait demande.
  for (rf in list(0, 9, NA, -1, "x")) {
    r <- hstat_dl50_puissance(list(A = A, B = B), reference = rf)
    expect_equal(nrow(r), 0L, info = paste(rf))
    expect_true(nzchar(attr(r, "message") %||% ""), info = paste(rf))
  }
  # Les references valides passent, et designent bien l'essai demande.
  r1 <- hstat_dl50_puissance(list(A = A, B = B), reference = 1)
  r2 <- hstat_dl50_puissance(list(A = A, B = B), reference = 2)
  expect_true(r1$Reference[1]); expect_true(r2$Reference[2])
  expect_equal(r1$Rapport[2], 1 / r2$Rapport[1], tolerance = 1e-6)
})

test_that("une palette inconnue ne fait pas avertir le graphique", {
  skip_if_not_installed("ggplot2")
  # Une palette absente de RColorBrewer fait AVERTIR ggplot a chaque trace et
  # rend un graphique gris. L'interface n'offre que des noms valides, mais la
  # fonction est publique -- et un avertissement par trace s'accumule dans la
  # console d'un serveur partage.
  f1 <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  f2 <- hstat_dl50_ajuste(hstat_dl50_essai(c(0.001, 0.002, 0.005, 0.01, 0.02),
                                           rep(30, 5), c(4, 9, 15, 22, 27), 30, 1), "em")
  expect_silent(p <- ggplot2::ggplot_build(
    hstat_dl50_graphique(list(f1, f2), list(palette = "ZZZ"))))
  # Et une palette valide reste employee telle quelle.
  expect_silent(ggplot2::ggplot_build(
    hstat_dl50_graphique(list(f1, f2), list(palette = "Dark2"))))
})

test_that("les chiffres significatifs affiches sont un reglage borne", {
  # Ils ne changent QUE l'affichage -- les exports gardent la precision
  # complete, arrondir une donnee exportee la ferait diverger du calcul.
  expect_equal(HSTAT_DL50_CHIFFRES, 5L)
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  mod <- paste(readLines(file.path(root, "R", "mod_dl50.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Chaque table d'affichage suit le reglage : aucune ne garde un nombre fige.
  fs <- regmatches(mod, gregexpr("DT::formatSignif\\([^;]*?\\)\\n", mod, perl = TRUE))[[1]]
  expect_gt(length(gregexpr("chiffres()", mod, fixed = TRUE)[[1]]), 5L)
  expect_false(grepl("DT::formatSignif(c(\"Chi2\", \"p\"), 4)", mod, fixed = TRUE))
  # Le reglage est declare dans l'interface et lu par un reactif borne.
  expect_true(grepl('ns("chiffres")', mod, fixed = TRUE))
  expect_true(grepl("min(max(v, 1L), 15L)", mod, fixed = TRUE))
})

test_that("le chemin de convergence de WIN DL est reproductible, et couteux", {
  ref <- .hstat_dl50_essai_ref()
  # LE DEFAUT RESTE LE CHEMIN RAPIDE. `c` part de la mortalite du temoin --
  # zero ici -- l'etape [E] rend des poids nuls, `c` ne bouge plus, et
  # l'ajustement converge en trois boucles.
  r <- hstat_dl50_ajuste(ref, "em")
  expect_equal(r$chemin, "rapide")
  expect_lt(r$iterations, 10L)

  # LE CHEMIN DE WIN DL part d'une mortalite naturelle NON NULLE : `c` explore,
  # l'EM rampe vers la borne, et le compte passe de 3 a plusieurs dizaines --
  # l'ordre de grandeur des 75 qu'annonce le logiciel.
  w <- hstat_dl50_ajuste(ref, "em", chemin = "windl", itmax = 500)
  expect_equal(w$chemin, "windl")
  expect_gt(w$iterations, 40L)
  expect_true(w$converge)

  # IL RAPPROCHE DES CHIFFRES PUBLIES. `a` sort a 2,19760 comme le logiciel
  # l'imprime, la ou le chemin rapide donne 2,19759.
  expect_equal(round(w$a, 5), 2.19760)
  expect_equal(round(r$a, 5), 2.19759)
  expect_equal(round(w$b, 5), 0.97319)

  # ET IL S'ARRETE AVANT LE MAXIMUM. C'est le fait qui interdit d'en faire le
  # defaut : la vraisemblance du chemin rapide est PLUS HAUTE. WIN DL imprime
  # -105,63592, plus bas encore que les deux.
  expect_gt(r$ll0, w$ll0)
  expect_lt(abs(r$ll0 - (-105.63582)), 1e-4)

  # LES DEUX RESTENT DANS L'ENVELOPPE DE CONFORMITE : l'ecart porte sur le
  # sixieme chiffre, pas sur un resultat.
  expect_equal(w$a, r$a, tolerance = 1e-5)
  expect_equal(w$b, r$b, tolerance = 1e-5)
  d1 <- hstat_dl50_doses_letales(r, 50)$Dose[1]
  d2 <- hstat_dl50_doses_letales(w, 50)$Dose[1]
  expect_equal(d1, d2, tolerance = 1e-4)

  # LE COUT EST REEL, et c'est pourquoi le chemin est propose et non impose :
  # sur un essai a reponse plate, cent iterations n'y suffisent plus, la ou le
  # chemin rapide aboutit en trois.
  plat <- hstat_dl50_essai(c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1), rep(200, 7),
                           c(30, 30, 30, 31, 33, 42, 57), 25, 0)
  expect_true(hstat_dl50_ajuste(plat, "em")$converge)
  expect_false(hstat_dl50_ajuste(plat, "em", chemin = "windl", itmax = 100)$converge)
})

test_that("les quatre tests de comparaison sont ceux du manuel de WIN DL", {
  # Le manuel enumere QUATRE tests de rapport de vraisemblance -- ajustement
  # lineaire, identite des droites, mortalite naturelle, parallelisme -- selon
  # l'un de TROIS scenarios de definition de la mortalite naturelle :
  # heterogene (propre a chaque essai, estimee), homogene (commune, estimee),
  # ou fixee a zero.
  #
  # Aucun fichier de sortie livre avec le logiciel n'exerce ces tests : ils ne
  # sont donc PAS confrontes chiffre a chiffre, a la difference de
  # l'ajustement. Ce test epingle au moins leur STRUCTURE, pour qu'un
  # renommage ou une disparition se voie.
  expect_equal(unname(HSTAT_DL50_SCENARIOS), c("heterogene", "homogene", "nulle"))

  # LES DOUZE COUPLES DE MODELES SONT CEUX DU MANUEL, qui donne H0 et H1 de
  # chaque test sous chaque scenario. On les reconstruit ici a la main, a
  # partir de l'ajusteur general, et on exige que le Chi-2 rendu par le module
  # soit EXACTEMENT 2 (ll(H1) - ll(H0)) sur ces couples-la.
  #
  # C'est la seule confrontation possible sur ces tests : aucun fichier de
  # sortie livre avec le logiciel ne les exerce. Elle porte donc sur la
  # STRUCTURE -- quels modeles sont opposes -- et non sur des chiffres publies.
  # Une inversion de couple y serait invisible autrement : le test rendrait un
  # Chi-2 parfaitement plausible, et faux.
  ez1 <- hstat_dl50_essai(c(0.05, 0.1, 0.25, 0.5, 1, 2), rep(120, 6),
                          c(18, 33, 58, 80, 101, 113), 120, 5)
  ez2 <- hstat_dl50_essai(c(0.05, 0.1, 0.25, 0.5, 1, 2), rep(120, 6),
                          c(9, 19, 38, 60, 85, 105), 120, 8)
  jeu <- list(A = ez1, B = ez2)
  for (sc in c("heterogene", "homogene")) {
    cm <- if (identical(sc, "heterogene")) "libre" else "commun"
    base  <- .hstat_dl50_fit_multi(jeu, FALSE, FALSE, cm)   # a_i, b_i
    ident <- .hstat_dl50_fit_multi(jeu, TRUE,  TRUE,  cm)   # a,  b
    paral <- .hstat_dl50_fit_multi(jeu, FALSE, TRUE,  cm)   # a_i, b
    libre <- .hstat_dl50_fit_multi(jeu, FALSE, FALSE, "libre")
    commun <- .hstat_dl50_fit_multi(jeu, FALSE, FALSE, "commun")
    sat <- .hstat_dl50_ll_sature(jeu)
    r <- hstat_dl50_comparaison(jeu, scenario = sc)
    attendu <- c(2 * (sat$ll - base$ll),      # T1 : ajustement contre sature
                 2 * (base$ll - ident$ll),    # T2 : a et b communs
                 2 * (libre$ll - commun$ll),  # T3 : c communs contre c libres
                 2 * (base$ll - paral$ll))    # T4 : b commun
    expect_equal(r$Chi2, pmax(0, attendu), tolerance = 1e-4, info = sc)
  }
  # Sous le scenario a mortalite nulle, le TROISIEME test change d'hypothese
  # nulle : le manuel oppose « c = 0 » a « c_i libres », la ou les deux autres
  # scenarios opposent « c commun » a « c_i libres ». Prendre le modele de base
  # dans les trois cas comparerait un modele a lui-meme sous l'hetero.
  jz <- list(A = hstat_dl50_essai(c(0.05, 0.1, 0.25, 0.5, 1, 2), rep(120, 6),
                                  c(15, 30, 55, 78, 100, 113), 120, 0),
             B = hstat_dl50_essai(c(0.05, 0.1, 0.25, 0.5, 1, 2), rep(120, 6),
                                  c(6, 16, 35, 58, 84, 105), 120, 0))
  rz <- hstat_dl50_comparaison(jz, scenario = "nulle")
  nul <- .hstat_dl50_fit_multi(jz, FALSE, FALSE, "nul")
  lib <- .hstat_dl50_fit_multi(jz, FALSE, FALSE, "libre")
  expect_equal(rz$Chi2[3], max(0, 2 * (lib$ll - nul$ll)), tolerance = 1e-4)

  # LA LIMITE DE CENT DOSES VAUT AUSSI POUR LE TOTAL -- le manuel l'ecrit a
  # part de la limite par essai. Deux essais de soixante doses passaient un a
  # un et depassaient ensemble, sans un mot.
  gros <- function(k) {
    d <- seq(0.01, 10, length.out = k)
    hstat_dl50_essai(d, rep(200, k), round(200 * stats::pnorm(0.5 + 1.2 * log10(d))),
                     200, 0)
  }
  r60 <- hstat_dl50_comparaison(list(A = gros(60), B = gros(60)), scenario = "nulle")
  expect_equal(nrow(r60), 0L)
  expect_true(grepl("120", attr(r60, "message"), fixed = TRUE))
  expect_true(grepl("100", attr(r60, "message"), fixed = TRUE))
  # Et le total juste en dessous passe.
  expect_gt(nrow(hstat_dl50_comparaison(list(A = gros(40), B = gros(40)),
                                        scenario = "nulle")), 0L)
})

test_that("le Chi-2 d'ajustement est la DEVIANCE, celle qu'imprime WIN DL", {
  # Le choix se tranche sur le seul fichier de sortie du logiciel : il imprime
  # « Chi2 calcule : 0.672 ». La deviance vaut 0,67217 et s'arrondit a 0,672 ;
  # le Chi-2 de PEARSON vaut 0,66768 et s'arrondirait a 0,668. Le .PRN decide.
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  P <- f$table$Mortalite_attendue; O <- f$table$Morts; N <- f$table$Effectif
  pearson <- sum((O - N * P)^2 / (N * P * (1 - P)))
  expect_equal(round(f$chi2, 3), 0.672)
  expect_false(identical(round(pearson, 3), 0.672))
  expect_equal(round(pearson, 3), 0.668)

  # LES SIX ESSAIS LIVRES, EUX, STOCKENT UN PEARSON -- et c'est la meme
  # explication que pour leurs bornes sans Fieller : ces valeurs viennent du
  # moteur MS-DOS. La difference creve les yeux sur l'essai dont une dose tue
  # TOUT : la deviance y vaut 1,579 et Pearson 1,212, pour 1,20967 stocke.
  # Aligner HStat sur ces fichiers-la le desalignerait du logiciel Windows,
  # qui est celui auquel on se compare.
  en <- hstat_dl50_ajuste(hstat_dl50_essai(c(10, 4, 2, 1, 0.5, 0.25), rep(30, 6),
                                           c(30, 28, 22, 15, 10, 4), 25, 0), "abbott")
  Pe <- en$table$Mortalite_attendue; Oe <- en$table$Morts; Ne <- en$table$Effectif
  pearson_en <- sum((Oe - Ne * Pe)^2 / (Ne * Pe * (1 - Pe)))
  expect_equal(pearson_en, 1.20967, tolerance = 5e-3)
  expect_gt(en$chi2 / pearson_en, 1.2)

  # DEUX JEUX, parce que le scenario a mortalite nulle REFUSE des essais dont
  # le temoin compte des morts -- et c'est juste : poser c = 0 devant un temoin
  # qui en montre serait une contradiction de saisie, pas une hypothese.
  e1 <- hstat_dl50_essai(c(0.05, 0.1, 0.25, 0.5, 1, 2), rep(120, 6),
                         c(18, 33, 58, 80, 101, 113), 120, 3)
  e2 <- hstat_dl50_essai(c(0.05, 0.1, 0.25, 0.5, 1, 2), rep(120, 6),
                         c(9, 19, 38, 60, 85, 105), 120, 4)
  z1 <- hstat_dl50_essai(c(0.05, 0.1, 0.25, 0.5, 1, 2), rep(120, 6),
                         c(15, 30, 55, 78, 100, 113), 120, 0)
  z2 <- hstat_dl50_essai(c(0.05, 0.1, 0.25, 0.5, 1, 2), rep(120, 6),
                         c(6, 16, 35, 58, 84, 105), 120, 0)

  # Le refus est explicite et il porte un motif -- pas un tableau vide.
  ref <- hstat_dl50_comparaison(list(A = e1, B = e2), scenario = "nulle")
  expect_equal(nrow(ref), 0L)
  expect_true(nzchar(attr(ref, "message") %||% ""))

  for (sc in unname(HSTAT_DL50_SCENARIOS)) {
    jeu <- if (identical(sc, "nulle")) list(A = z1, B = z2) else list(A = e1, B = e2)
    r <- hstat_dl50_comparaison(jeu, scenario = sc)
    expect_equal(nrow(r), 4L, info = sc)
    expect_equal(attr(r, "scenario"), sc)
    expect_true(any(grepl("[Aa]justement", r$Hypothese)), info = sc)
    expect_true(any(grepl("Identit", r$Hypothese)), info = sc)
    expect_true(any(grepl("[Pp]arall", r$Hypothese)), info = sc)
    # Le troisieme porte sur la mortalite naturelle, et son libelle SUIT le
    # scenario : sous « nulle » on teste que c vaut zero, sinon qu'elles sont
    # egales. Un libelle fige annoncerait le mauvais test.
    expect_true(any(grepl(if (identical(sc, "nulle")) "[Nn]ullit" else "galit",
                          r$Hypothese)), info = sc)
    # AUCUN TEST SANS DEGRE DE LIBERTE : c'est le defaut qui avait ete corrige
    # sur le troisieme, ou comparer le modele de base a lui-meme donnait ddl 0.
    expect_true(all(is.na(r$DDL) | r$DDL > 0), info = sc)
  }
})

test_that("Fieller est exact, et cede a la delta-methode quand il n'est plus borne", {
  # FIELLER par definition : l'ensemble des m tels que
  #   (y - a - b m)^2 <= t^2 (Vaa + m^2 Vbb + 2 m Vab)
  # soit les racines d'un polynome du second degre. Une forme approchee du
  # terme sous la racine rendait des bornes qui n'encadraient meme pas
  # l'estimation -- c'est le premier symptome, et le seul si l'on ne regarde
  # pas le tableau.
  racines <- function(a, b, V, t, p) {
    # MEME inverse normale des deux cotes : le module emploie celle de WIN DL
    # (Hastings), et une racine calculee avec `qnorm` exact comparerait deux
    # polynomes differents.
    y <- .hstat_dl50_qnorm(p)
    A <- b^2 - t^2 * V[2, 2]
    B <- -2 * (b * (y - a) + t^2 * V[1, 2])
    C <- (y - a)^2 - t^2 * V[1, 1]
    d <- B^2 - 4 * A * C
    if (A <= 0 || d < 0) return(c(NA_real_, NA_real_))
    sort(c((-B - sqrt(d)) / (2 * A), (-B + sqrt(d)) / (2 * A)))
  }
  # Essai bien determine : g < 1, Fieller s'applique.
  e <- hstat_dl50_essai(c(0.25, 0.5, 1, 2, 4, 10), rep(300, 6),
                        c(40, 100, 150, 220, 280, 299), 300, 0)
  f <- hstat_dl50_ajuste(e, "em")
  dl <- hstat_dl50_doses_letales(f)
  expect_lt(attr(dl, "g"), 1)
  expect_true(all(dl$Intervalle == "Fieller"))
  for (i in seq_len(nrow(dl))) {
    r <- racines(f$a, f$b, f$Vh, f$t, dl$Seuil[i] / 100)
    expect_equal(log10(dl$Limite_inf[i]), r[1], tolerance = 1e-8)
    expect_equal(log10(dl$Limite_sup[i]), r[2], tolerance = 1e-8)
  }
  # Et, dans tous les cas, les bornes ENCADRENT l'estimation.
  expect_true(all(dl$Limite_inf < dl$Dose & dl$Dose < dl$Limite_sup))

  # Essai de reference : g = 1.44, l'ensemble n'est plus borne. WIN DL y rend
  # des bornes symetriques en log-dose, celles de la delta-methode.
  f2 <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  dl2 <- hstat_dl50_doses_letales(f2)
  expect_gt(attr(dl2, "g"), 1)
  expect_true(all(dl2$Intervalle == "delta"))
  expect_equal(log10(dl2$Limite_sup) - dl2$Log_dose,
               dl2$Log_dose - log10(dl2$Limite_inf), tolerance = 1e-8)
  expect_true(all(dl2$Limite_inf < dl2$Dose & dl2$Dose < dl2$Limite_sup))
})

test_that("les trois methodes d'estimation de la mortalite naturelle se distinguent", {
  # Temoin a 4 morts sur 90 : Abbott fixe c a 4/90, EM l'estime sur tout
  # l'essai, la troisieme la force a zero.
  e <- hstat_dl50_essai(c(0.00848, 0.0163, 0.03423, 0.05053, 0.08476),
                        rep(90, 5), c(30, 43, 62, 75, 82), 90, 4)
  em <- hstat_dl50_ajuste(e, "em")
  ab <- hstat_dl50_ajuste(e, "abbott")
  nu <- hstat_dl50_ajuste(e, "nulle")
  expect_true(all(vapply(list(em, ab, nu), function(f) isTRUE(f$ok), logical(1))))
  expect_equal(ab$c, 4 / 90)
  expect_equal(nu$c, 0)
  expect_gt(em$c, 0)
  expect_false(em$heterogene)

  # FORCER c A ZERO DEVANT UN TEMOIN QUI COMPTE DES MORTS EST UNE
  # CONTRADICTION DE SAISIE, ET ELLE SE DIT PAR SON NOM.
  #
  # Ce test affirmait auparavant que l'ajustement « s'effondre » --
  # `nu$heterogene` vrai, facteur superieur a 1. C'etait un ARTEFACT : le
  # modele affirme p = 0 la ou le temoin montre des deces, la vraisemblance
  # vaut moins l'infini, et `hstat_dl50_logvrais()` la bornait a 1e-12. Le
  # Chi-2 ressortait fini mais entierement determine par cette borne (119,8 a
  # 1e-10 ; 258,0 a 1e-20). Il pilotait le facteur d'heterogeneite, donc la
  # largeur de tous les intervalles.
  #
  # Le temoin ne fait plus partie de la vraisemblance quand c est DECLAREE :
  # l'ajustement se juge sur la serie de doses, comme le degre de liberte
  # (doses - 2) et la matrice d'information le font deja. Ici cette serie
  # s'ajuste bien, et c'est la verite -- ses mortalites vont de 33 % a 91 %,
  # une droite les traverse sans peine avec ou sans correction.
  expect_true(nu$temoin_contredit)
  expect_false(isTRUE(em$temoin_contredit))
  expect_false(isTRUE(ab$temoin_contredit))
  al <- hstat_dl50_verdict(nu, 50)$alertes
  expect_true(any(grepl("déclarée nulle", al)))

  # ET LE CHI-2 NE DEPEND PLUS D'UNE CONSTANTE D'IMPLEMENTATION. C'est la
  # verification qui compte : il ne porte plus que sur les doses, dont aucune
  # probabilite ajustee n'approche la borne.
  expect_equal(nu$chi2, 2 * (hstat_dl50_logvrais(e$doses$x, e$doses$n,
                                                 e$doses$x / e$doses$n) -
                             hstat_dl50_logvrais(e$doses$x, e$doses$n,
                                                 nu$table$Mortalite_attendue)),
               tolerance = 1e-10)

  # Le test Abbott/EM oppose c libre a c fixee au temoin : 1 degre de liberte.
  t <- hstat_dl50_test_abbott_em(e)
  expect_true(t$ok)
  expect_equal(t$ddl, 1L)
  expect_equal(t$c_abbott, 4 / 90)
  expect_true(nzchar(t$conseil))

  # LES DEUX VRAISEMBLANCES PORTENT SUR LES MEMES DONNEES, temoin compris.
  # C'est lui qui separe les deux modeles -- il dit ou est la mortalite
  # naturelle. Prendre `ll0`, qui ne le contient plus quand c est declaree,
  # chargeait la difference du terme du temoin : elle ressortait negative,
  # `max(0, .)` la ramenait a zero, et le verdict devenait « les deux
  # concordent » QUELLES QUE SOIENT LES DONNEES.
  llc <- function(f) {
    dd <- f$essai$doses
    hstat_dl50_logvrais(dd$x, dd$n, f$table$Mortalite_attendue) +
      hstat_dl50_logvrais(f$essai$x0, f$essai$n0, f$c)
  }
  expect_equal(t$chi2, 2 * (llc(em) - llc(ab)), tolerance = 1e-8)
  # Abbott est EM contraint a c = x0/n0 : la difference est positive par
  # construction, EM maximisant exactement cet objectif. Un test qui buterait
  # sur le plancher a zero serait un test qui ne teste plus rien.
  expect_gt(t$chi2, 0)

  # Et il DISTINGUE : sur un essai ou le temoin contredit les doses, la
  # statistique doit decoller. Temoin a 30 %, doses tres mortelles des la plus
  # faible : EM ne peut pas placer c la ou Abbott l'impose.
  e2 <- hstat_dl50_essai(c(0.01, 0.02, 0.05, 0.1, 0.2), rep(200, 5),
                         c(20, 44, 96, 150, 186), 200, 60)
  t2 <- hstat_dl50_test_abbott_em(e2)
  expect_true(t2$ok)
  expect_gt(t2$chi2, t$chi2)
  expect_lt(t2$p, 0.05)
})

test_that("moins de trois doses exploitables : le calcul est refuse, en le disant", {
  # WIN DL refuse de continuer sous trois doses donnant une mortalite corrigee
  # strictement comprise entre 0 et 100 %. Rendre une droite sur deux points
  # utiles serait pire que refuser.
  e <- hstat_dl50_essai(c(0.1, 1, 10, 100), rep(20, 4), c(0, 0, 20, 20), 20, 0)
  f <- hstat_dl50_ajuste(e, "em")
  expect_false(isTRUE(f$ok))
  expect_true(grepl("Ajoutez", f$message, fixed = TRUE))

  # Et les saisies impossibles sont nommees, pas avalees.
  for (cas in list(
      hstat_dl50_essai(c(0, 1, 2), rep(20, 3), c(1, 5, 9)),
      hstat_dl50_essai(c(1, 2, 3), rep(20, 3), c(1, 25, 9)),
      hstat_dl50_essai(c(1, 2, 3), c(20, 0, 20), c(1, 5, 9)))) {
    r <- hstat_dl50_ajuste(cas, "em")
    expect_false(isTRUE(r$ok))
    expect_true(grepl("verifiez|vérifiez|renseignez|Renseignez|temoin|témoin",
                      r$message))
  }
})

test_that("les doses identiques sont regroupees et triees", {
  # Deux lignes a la meme dose sont deux repetitions du meme point : les
  # sommer est la seule lecture qui garde juste le nombre d'individus testes.
  e <- hstat_dl50_essai(c(10, 1, 10, 0.1), c(20, 20, 30, 20), c(15, 2, 24, 1))
  expect_equal(e$doses$dose, c(0.1, 1, 10))
  expect_equal(e$doses$n, c(20, 20, 50))
  expect_equal(e$doses$x, c(1, 2, 39))
})

test_that("le calcul inverse rend des doses croissantes avec la mortalite", {
  # `doses_letales()` TRIE ses lignes par seuil decroissant. Y recoller la
  # mortalite demandee dans l'ordre de saisie decalait toutes les colonnes :
  # on lisait la dose de la DL25 sur la ligne de la DL95.
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  d <- hstat_dl50_dose_pour(f, c(95, 25, 50))
  expect_equal(d$Mortalite_demandee, c(25, 50, 95))
  expect_true(all(diff(d$Dose) > 0))
  # La DL50 rendue ici est la meme que celle du tableau des doses letales.
  expect_equal(d$Dose[d$Mortalite_demandee == 50],
               hstat_dl50_doses_letales(f)$Dose[2])

  # Une mortalite sous la mortalite naturelle n'a pas de dose : on rend NULL
  # plutot qu'un nombre qui aurait l'air d'une reponse.
  e <- hstat_dl50_essai(c(0.00848, 0.0163, 0.03423, 0.05053, 0.08476),
                        rep(90, 5), c(30, 43, 62, 75, 82), 90, 20)
  f2 <- hstat_dl50_ajuste(e, "abbott")
  expect_null(hstat_dl50_dose_pour(f2, 5))

  # La mortalite attendue reste dans [0 ; 1], bornes comprises : l'intervalle
  # est construit sur le probit puis transporte, jamais sur la proportion.
  m <- hstat_dl50_mortalite(f, c(1e-6, 0.005, 1e6))
  expect_true(all(m$Limite_inf >= 0 & m$Limite_sup <= 1))
  expect_true(all(m$Limite_inf <= m$Mortalite & m$Mortalite <= m$Limite_sup))
})

test_that("la comparaison d'essais ne compare jamais un modele a lui-meme", {
  # Sous le scenario heterogene, prendre le modele de base comme hypothese
  # nulle du 3e test revenait a le comparer a lui-meme : zero degre de
  # liberte, et un test qui ne teste rien.
  a <- .hstat_dl50_essai_ref()
  b <- hstat_dl50_essai(c(0.25, 0.5, 1, 2, 4, 10), rep(30, 6),
                        c(4, 10, 15, 22, 28, 30), 25, 0, titre = "endosulfan")
  for (sc in c("heterogene", "homogene", "nulle")) {
    r <- hstat_dl50_comparaison(list(a, b), sc)
    expect_equal(nrow(r), 4L)
    expect_true(all(r$DDL > 0), info = sc)
    expect_true(all(is.finite(r$Chi2)), info = sc)
    expect_true(all(nzchar(r$Conclusion)))
    expect_true(nzchar(attr(r, "avertissement")))
  }
  # Deux produits differents : les droites different, et le test le dit.
  r <- hstat_dl50_comparaison(list(a, b), "heterogene")
  expect_lt(r$p[r$Hypothese == tr("Identité des droites (a et b identiques)")], 0.05)

  # Le meme essai deux fois : rien ne peut differer.
  r2 <- hstat_dl50_comparaison(list(a, a), "heterogene")
  expect_gt(r2$p[2], 0.99)
  expect_gt(r2$p[4], 0.99)

  expect_equal(nrow(hstat_dl50_comparaison(list(a), "heterogene")), 0L)
})

test_that("le scenario a mortalite nulle est refuse si un temoin compte des morts", {
  # Fixer c a zero devant un temoin qui compte des morts ferait porter cette
  # mortalite par la pente. Le manuel l'ecrit ; on le refuse en le disant.
  a <- .hstat_dl50_essai_ref()
  b <- hstat_dl50_essai(c(0.25, 0.5, 1, 2, 4, 10), rep(30, 6),
                        c(4, 10, 15, 22, 28, 30), 25, 3)
  r <- hstat_dl50_comparaison(list(a, b), "nulle")
  expect_equal(nrow(r), 0L)
  expect_true(grepl("mortalité naturelle", attr(r, "message")))
  # Les deux autres scenarios, eux, restent disponibles.
  expect_equal(nrow(hstat_dl50_comparaison(list(a, b), "heterogene")), 4L)
})

test_that("la fusion exige des champs identiques et un test d'identite non significatif", {
  ch <- list(espece = "C. leucotreta", stade = "Adulte", duree = "48 h",
             temperature = "25", matiere1 = "Cyfluthrine", matiere2 = "",
             ratio = "", methode = "Application topique", unite = "ug/insecte")
  a <- hstat_dl50_essai(c(0.00063, 0.00125, 0.0025, 0.005, 0.01, 0.02),
                        rep(25, 6), c(5, 7, 9, 11, 14, 18), 25, 0,
                        titre = "rep 1", champs = ch)
  b <- hstat_dl50_essai(c(0.00063, 0.00125, 0.0025, 0.005, 0.01, 0.02),
                        rep(25, 6), c(4, 8, 10, 12, 13, 17), 25, 0,
                        titre = "rep 2", champs = ch)
  r <- hstat_dl50_fusion(list(a, b))
  expect_true(isTRUE(r$ok))
  # Les effectifs s'additionnent dose par dose.
  expect_equal(r$essai$doses$n, rep(50, 6))
  expect_equal(r$essai$doses$x, c(9, 15, 19, 23, 27, 35))
  expect_equal(r$essai$n0, 50)

  # Un champ qui differe suffit a bloquer : la fusion assemblerait deux
  # experimentations differentes.
  ch2 <- ch; ch2$matiere1 <- "Endosulfan"
  b2 <- hstat_dl50_essai(b$doses$dose, b$doses$n, b$doses$x, 25, 0, champs = ch2)
  r2 <- hstat_dl50_fusion(list(a, b2))
  expect_false(isTRUE(r2$ok))
  expect_true(grepl("Matière active", r2$message, fixed = TRUE))

  # Des essais qui different vraiment : le test d'identite bloque la fusion.
  b3 <- hstat_dl50_essai(c(0.25, 0.5, 1, 2, 4, 10), rep(30, 6),
                         c(4, 10, 15, 22, 28, 30), 25, 0, champs = ch)
  r3 <- hstat_dl50_fusion(list(a, b3))
  expect_false(isTRUE(r3$ok))
  expect_true(grepl("masquerait", r3$message, fixed = TRUE))
})

test_that("un fichier WIN DL se lit, s'ecrit et se relit a l'identique", {
  # Les separateurs de la premiere ligne sont les octets 0x00 a 0x09 :
  # `readLines()` s'arrete sur le premier zero et perdrait l'en-tete entier.
  # Le fichier se lit donc en OCTETS, decoupage en lignes compris.
  ch <- list(date = "1/2/95", auteur = "Jean-Michel Vassal", duree = "48 h",
             temperature = "25", espece = "Cryptophlebia leucotreta",
             stade = "Adulte", matiere1 = "Cyfluthrine", matiere2 = "",
             ratio = "", methode = "Application topique",
             unite = "µg / insecte")
  e <- hstat_dl50_essai(c(0.00063, 0.00125, 0.0025, 0.005, 0.01, 0.02, 0.03),
                        rep(25, 7), c(5, 7, 9, 11, 14, 18, 20), 25, 0,
                        titre = "C. leucotreta reference", champs = ch)
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  hstat_dl50_ecrire_windl(e, tmp)
  # Le fichier porte bien des octets de controle : c'est ce qui le rend
  # illisible par `readLines()`, et lisible par WIN DL.
  oct <- readBin(tmp, "raw", n = file.size(tmp))
  expect_true(any(oct == as.raw(0L)))
  expect_true(any(oct == as.raw(219L)))          # 0xDB, marqueur de fin

  r <- hstat_dl50_lire_windl(tmp)
  expect_true(isTRUE(r$ok))
  expect_equal(r$essai$doses$dose, e$doses$dose, tolerance = 1e-6)
  expect_equal(r$essai$doses$n, e$doses$n)
  expect_equal(r$essai$doses$x, e$doses$x)
  expect_equal(r$essai$n0, 25)
  expect_equal(r$essai$x0, 0)
  expect_equal(r$essai$titre, "C. leucotreta reference")
  for (nm in c("date", "auteur", "espece", "matiere1", "methode"))
    expect_equal(r$essai$champs[[nm]], ch[[nm]], info = nm)
  # Le micro passe l'aller-retour par le CP437.
  expect_true(grepl("g / insecte", r$essai$champs$unite, fixed = TRUE))

  # Et l'ajustement du fichier relu est celui de l'essai d'origine.
  expect_equal(hstat_dl50_ajuste(r$essai, "em")$a,
               hstat_dl50_ajuste(e, "em")$a, tolerance = 1e-8)

  # Un fichier qui n'en est pas un se refuse, sans lever.
  vide <- tempfile(fileext = ".txt")
  writeLines(c("n'importe quoi", "deux lignes"), vide)
  on.exit(unlink(vide), add = TRUE)
  expect_false(isTRUE(hstat_dl50_lire_windl(vide)$ok))
  expect_false(isTRUE(hstat_dl50_lire_windl(tempfile())$ok))
})

test_that("la dose zero devient le temoin, elle n'est pas ecartee", {
  # Son logarithme n'existe pas : elle ne peut pas entrer dans la regression.
  # L'ecarter en silence perdrait la mortalite naturelle de l'essai.
  df <- data.frame(
    essai = rep(c("A", "B"), each = 4),
    dose = c(0, 1, 2, 4, 0, 1, 2, 4),
    n = rep(20, 8), morts = c(2, 5, 9, 15, 0, 4, 8, 16))
  r <- hstat_dl50_depuis_donnees(df, "dose", "n", "morts", "essai")
  expect_true(isTRUE(r$ok))
  expect_equal(length(r$essais), 2L)
  expect_equal(r$essais[["A"]]$n0, 20)
  expect_equal(r$essais[["A"]]$x0, 2)
  expect_equal(r$essais[["B"]]$x0, 0)
  expect_equal(nrow(r$essais[["A"]]$doses), 3L)

  # Sans colonne de regroupement, un seul essai.
  r2 <- hstat_dl50_depuis_donnees(df[df$essai == "A", ], "dose", "n", "morts")
  expect_equal(length(r2$essais), 1L)

  # Une colonne absente est nommee, pas devinee.
  r3 <- hstat_dl50_depuis_donnees(df, "dose", "absente", "morts")
  expect_false(isTRUE(r3$ok))
  expect_true(grepl("absente", r3$message, fixed = TRUE))
})

test_that("le rapport .PRN porte les nombres et les libelles attendus", {
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  p <- hstat_dl50_prn(f, "CL94AC1.TXT")
  txt <- paste(p, collapse = "\n")
  expect_true(grepl("CL94AC1.TXT", txt, fixed = TRUE))
  expect_true(grepl("2.19759", txt, fixed = TRUE) ||
              grepl("2.19760", txt, fixed = TRUE))
  expect_true(grepl("0.97319", txt, fixed = TRUE))
  expect_true(grepl("-105.635", txt, fixed = TRUE))
  # Une ligne par dose, plus les trois doses letales.
  expect_equal(sum(grepl("^DL ", p)), 3L)
  expect_true(grepl("Chi-2", txt, fixed = TRUE))
  # Un ajustement en echec ne produit pas un rapport a moitie ecrit.
  expect_equal(length(hstat_dl50_prn(list(ok = FALSE))), 0L)
})

test_that("le facteur d'heterogeneite s'applique quand l'ajustement est rejete", {
  # Un Chi-2 significatif signale une dispersion que le modele binomial ne
  # contient pas. Les variances sont multipliees par Chi2/ddl et le quantile
  # devient celui de STUDENT : ne pas le faire publierait des intervalles trop
  # etroits precisement quand le modele est douteux.
  e <- hstat_dl50_essai(c(0.00848, 0.0163, 0.03423, 0.05053, 0.08476),
                        c(90, 10000, 90, 90, 90), c(1, 1, 62, 75, 89), 100, 0)
  f <- hstat_dl50_ajuste(e, "em")
  expect_true(isTRUE(f$ok))
  expect_true(f$heterogene)
  expect_gt(f$facteur, 1)
  expect_equal(f$Vh, f$V * f$facteur)
  expect_equal(f$t, stats::qt(0.975, f$ddl))
  # Sans heterogeneite, le quantile est celui de la loi normale.
  f2 <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  expect_equal(f2$facteur, 1)
  expect_equal(f2$t, stats::qnorm(0.975))
  expect_equal(f2$Vh, f2$V)
})

test_that("une liste deroulante refuse les doublons, casse comprise", {
  # « Si l'element existe deja dans la liste, il ne sera pas ajoute afin
  # d'eviter les doublons. » La comparaison ignore la casse et les espaces de
  # bord : « Cyfluthrine » et « cyfluthrine  » sont le meme produit, et les
  # garder tous deux rendrait la selection inefficace -- ce que la liste
  # existe precisement pour eviter.
  l <- character(0)
  l <- hstat_dl50_liste_ajouter(l, "Cyfluthrine")
  l <- hstat_dl50_liste_ajouter(l, "cyfluthrine ")
  l <- hstat_dl50_liste_ajouter(l, "  CYFLUTHRINE")
  expect_equal(l, "Cyfluthrine")
  l <- hstat_dl50_liste_ajouter(l, c("Endosulfan", "", "  ", "Deltaméthrine"))
  expect_equal(l, sort(c("Cyfluthrine", "Endosulfan", "Deltaméthrine")))
  expect_equal(hstat_dl50_liste_retirer(l, "ENDOSULFAN"),
               sort(c("Cyfluthrine", "Deltaméthrine")))
  # Retirer ce qui n'y est pas ne retire rien.
  expect_equal(hstat_dl50_liste_retirer(l, "Zzz"), l)
})

test_that("un fichier de liste se relit sans son marqueur de fin", {
  # Le marqueur de fin est l'OCTET 0xDB. Le manuel l'ecrit « Û » -- c'est son
  # rendu en CP1252 ; en CP437, celui du fichier, c'est « █ ». Le retirer par
  # son apparence textuelle ne marche donc pas : la liste relue portait un
  # element de plus, un carre plein, invisible a la lecture du code.
  f <- tempfile(fileext = ".TXT")
  on.exit(unlink(f), add = TRUE)
  v <- c("Cryptophlebia leucotreta", "Helicoverpa armigera", "Aphis gossypii")
  hstat_dl50_liste_ecrire(v, f)
  oct <- readBin(f, "raw", n = file.size(f))
  expect_true(any(oct == as.raw(219L)))
  relu <- hstat_dl50_liste_lire(f)
  expect_equal(relu, v)
  expect_false(any(grepl("█", relu)))

  # Une liste vide s'ecrit et se relit vide, sans lever.
  g <- tempfile(fileext = ".TXT")
  on.exit(unlink(g), add = TRUE)
  hstat_dl50_liste_ecrire(character(0), g)
  expect_equal(hstat_dl50_liste_lire(g), character(0))
  expect_equal(hstat_dl50_liste_lire(tempfile()), character(0))
})

test_that("regenerer les listes reprend le vocabulaire des essais", {
  ch <- function(esp, ma) list(auteur = "Vassal", espece = esp, stade = "Adulte",
                               duree = "48 h", temperature = "25", matiere1 = ma,
                               matiere2 = "", ratio = "",
                               methode = "Application topique", unite = "µg/insecte")
  es <- list(
    A = hstat_dl50_essai(c(1, 2, 4), rep(20, 3), c(3, 9, 16), 20, 0,
                         champs = ch("C. leucotreta", "Cyfluthrine")),
    B = hstat_dl50_essai(c(1, 2, 4), rep(20, 3), c(2, 8, 15), 20, 0,
                         champs = ch("H. armigera", "Endosulfan")))
  L <- hstat_dl50_regenerer(hstat_dl50_listes_vides(), es)
  expect_equal(L$espece, sort(c("C. leucotreta", "H. armigera")))
  expect_equal(L$matiere, sort(c("Cyfluthrine", "Endosulfan")))
  expect_equal(L$auteur, "Vassal")
  expect_equal(L$unite, "µg/insecte")
  # Idempotent : regenerer deux fois ne duplique rien.
  expect_equal(hstat_dl50_regenerer(L, es), L)
  # Sans essai, les listes ne changent pas.
  expect_equal(hstat_dl50_regenerer(L, list()), L)
})

test_that("la selection multi-criteres filtre sans jamais tout ecarter a vide", {
  # Un critere VIDE ne filtre pas. C'est la difference entre « je ne demande
  # rien sur l'espece » et « je demande une espece qui n'existe pas » : traiter
  # le premier comme le second ne rendrait jamais aucun essai, et l'on
  # conclurait que le fonds est vide.
  ch <- function(esp, ma, meth, tp, ma2 = "", rat = "")
    list(auteur = "Vassal", espece = esp, stade = "Adulte", duree = "48 h",
         temperature = tp, matiere1 = ma, matiere2 = ma2, ratio = rat,
         methode = meth, unite = "ug/insecte")
  mk <- function(...) hstat_dl50_essai(c(1, 2, 4), rep(20, 3), c(3, 9, 16), 20, 0,
                                       champs = ch(...))
  es <- list(
    A = mk("C. leucotreta", "Cyfluthrine", "Application topique", "25"),
    B = mk("C. leucotreta", "Endosulfan",  "Application topique", "30"),
    C = mk("H. armigera",   "Cyfluthrine", "Ingestion",           "25"),
    D = mk("C. leucotreta", "Cyfluthrine", "Application topique", "27",
           ma2 = "Profénofos", rat = "1:2"))

  expect_equal(hstat_dl50_selection(es, list()), c("A", "B", "C", "D"))
  expect_equal(hstat_dl50_selection(es, list(espece = "")), c("A", "B", "C", "D"))
  expect_equal(hstat_dl50_selection(es, list(espece = "C. leucotreta")), c("A", "B", "D"))
  # La casse ne separe pas deux essais du meme produit.
  expect_equal(hstat_dl50_selection(es, list(matiere1 = "cyfluthrine")), c("A", "C", "D"))
  # Plusieurs valeurs pour un critere : l'une OU l'autre.
  expect_equal(hstat_dl50_selection(es, list(methode = c("Ingestion", "Application topique"))),
               c("A", "B", "C", "D"))
  # Les criteres se cumulent : l'un ET l'autre.
  expect_equal(hstat_dl50_selection(es, list(espece = "C. leucotreta",
                                             matiere1 = "Cyfluthrine")), c("A", "D"))
  # Une espece absente rend zero essai -- et c'est bien ce qu'on a demande.
  expect_equal(hstat_dl50_selection(es, list(espece = "Zzz")), character(0))

  # Temperature : une valeur -> egalite, deux -> intervalle. Les bornes sont
  # remises dans l'ordre plutot que de rendre zero essai sur une inversion de
  # saisie, qui n'apprendrait rien a personne.
  expect_equal(hstat_dl50_selection(es, list(temperature = 25)), c("A", "C"))
  expect_equal(hstat_dl50_selection(es, list(temperature = c(25, 27))), c("A", "C", "D"))
  expect_equal(hstat_dl50_selection(es, list(temperature = c(27, 25))), c("A", "C", "D"))

  # La seconde matiere active et le ratio ne servent QUE si l'on trie sur les
  # deux : sans la case cochee, un essai a une seule matiere active serait
  # ecarte par un critere qui ne le concerne pas.
  expect_equal(hstat_dl50_selection(es, list(matiere1 = "Cyfluthrine",
                                             matiere2 = "Profénofos")),
               c("A", "C", "D"))
  expect_equal(hstat_dl50_selection(es, list(matiere1 = "Cyfluthrine",
                                             matiere2 = "Profénofos",
                                             avec_ma2 = TRUE)), "D")
  expect_equal(hstat_dl50_selection(es, list(avec_ma2 = TRUE, ratio = "1:2")), "D")

  expect_equal(hstat_dl50_selection(list(), list(espece = "A")), character(0))
})

test_that("une pente negative est refusee, en nommant la cause probable", {
  # Sans ce refus, deux colonnes inversees produisent un rapport COMPLET --
  # equation, intervalles, graphique -- ou la DL10 vaut mille fois la DL90.
  # Rien a l'ecran ne le signale : c'est le resultat faux le plus facile a
  # publier de bonne foi.
  inv <- hstat_dl50_essai(c(0.1, 0.5, 1, 5, 10), rep(40, 5), c(36, 30, 20, 10, 3), 40, 0)
  f <- hstat_dl50_ajuste(inv, "em")
  expect_false(isTRUE(f$ok))
  expect_true(grepl("décroît", f$message, fixed = TRUE))
  # Le message NOMME la cause probable : c'est ce qui le rend actionnable.
  expect_true(grepl("inversées", f$message, fixed = TRUE))
  expect_true(grepl("effectif testé", f$message, fixed = TRUE))
  # Et il ne rend rien d'exploitable par la suite.
  expect_null(hstat_dl50_doses_letales(f))

  # LE REFUS PORTE SUR LA PENTE AJUSTEE, PAS SUR LA VALEUR DE DEPART.
  #
  # Il portait sur les deux. La valeur de depart vient d'une regression NON
  # PONDEREE sur les seules doses de mortalite intermediaire : sur un essai
  # bruite elle sort negative alors que l'ajustement rend une pente franchement
  # positive. Mesure sur quatre mille essais tires au sort, 23 etaient refuses
  # a tort -- avec un message qui accusait l'utilisateur d'avoir inverse ses
  # colonnes.
  #
  # Cet essai-la EST le cas : mortalites 0, 5, 7, 5 sur dix individus, donc
  # 0 %, 50 %, 70 %, 50 % -- bruitee, mais croissante. Depart -0,356, ajuste
  # +2,49.
  bruite <- hstat_dl50_essai(c(2.105, 3.363, 4.332, 7.586), rep(10, 4), c(0, 5, 7, 5), 20, 0)
  z <- log10(bruite$doses$dose)
  ini <- .hstat_dl50_init(z, bruite$doses$n, bruite$doses$x, 0)
  expect_lt(ini$b, 0)                       # le depart est bien negatif
  fb <- hstat_dl50_ajuste(bruite, "nulle")
  expect_true(isTRUE(fb$ok))                # et l'essai est accepte
  expect_gt(fb$b, 0)

  # LES DEUX GARDES SE TESTENT SEPAREMENT. Tant qu'elles etaient deux, retirer
  # l'une laissait l'autre attraper le cas d'essai : aucune assertion ne les
  # distinguait, et le refus a tort a vecu la. Ces deux essais-ci les separent
  # -- l'un doit passer, l'autre non, et un seul controle subsiste.
  expect_false(isTRUE(hstat_dl50_ajuste(inv, "nulle")$ok))
  expect_true(isTRUE(hstat_dl50_ajuste(bruite, "em")$ok))
})

test_that("une dose letale hors de l'etendue testee est marquee", {
  # Sur l'essai de reference lui-meme, DEUX des trois doses letales publiees
  # tombent hors de l'etendue reellement testee : la DL90 vaut pres de quatre
  # fois la dose la plus forte appliquee. Rien ne les distinguait de la DL50,
  # seule interpolee.
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  d <- hstat_dl50_doses_letales(f)
  etendue <- range(f$essai$doses$dose)
  expect_equal(attr(d, "etendue"), etendue)
  attendu <- ifelse(d$Dose < etendue[1] | d$Dose > etendue[2],
                    tr("extrapolée"), tr("interpolée"))
  expect_equal(d$Position, attendu)
  expect_equal(d$Position[d$Seuil == 50], tr("interpolée"))
  expect_equal(sum(d$Position == tr("extrapolée")), 2L)
  # L'avertissement NOMME les seuils concernes, chacun avec son « DL » : la
  # liste brute « DL90, 10 » se lisait comme une dose de 10.
  av <- attr(d, "avertissement")
  expect_true(nzchar(av))
  expect_true(grepl("DL90", av, fixed = TRUE))
  expect_true(grepl("DL10", av, fixed = TRUE))
  expect_false(grepl("  ", av, fixed = TRUE))

  # Un essai dont les trois seuils tombent dans l'etendue ne declenche rien :
  # un avertissement permanent finit par ne plus etre lu.
  large <- hstat_dl50_essai(c(0.05, 0.25, 0.5, 1, 2, 4, 20), rep(200, 7),
                            c(4, 40, 80, 100, 130, 170, 198), 200, 0)
  dl <- hstat_dl50_doses_letales(hstat_dl50_ajuste(large, "em"))
  expect_true(all(dl$Position == tr("interpolée")))
  expect_null(attr(dl, "avertissement"))

  # La mortalite a une dose donnee porte la meme colonne.
  m <- hstat_dl50_mortalite(f, c(1e-9, 0.005, 1e9))
  expect_equal(m$Position, c(tr("extrapolée"), tr("interpolée"), tr("extrapolée")))
})

test_that("un Chi-2 sans degre de liberte residuel ne se presente pas comme un bon ajustement", {
  # Avec autant de doses que de parametres estimes, le modele passe exactement
  # par les points : le Chi-2 vaut zero PAR CONSTRUCTION. Le plancher
  # `max(1L, ...)`, qui existe pour eviter une division par zero, le
  # transformait en « p = 1,0000 -- ajustement probit legitime ». Un verdict
  # rassurant sur un test qui n'a pas eu lieu est pire qu'un silence.
  m <- hstat_dl50_essai(c(1, 2, 4), rep(20, 3), c(4, 10, 16), 20, 0)
  em <- hstat_dl50_ajuste(m, "em")          # 3 parametres, 3 doses
  expect_true(isTRUE(em$ok))
  expect_false(em$informatif)
  expect_equal(em$npar, 3L)
  expect_equal(em$chi2, 0)
  expect_true(grepl("non testable", hstat_dl50_verdict(em)$ajustement, fixed = TRUE))
  # Le facteur d'heterogeneite ne s'applique pas sur un test qui n'a pas eu lieu.
  expect_equal(em$facteur, 1)
  expect_false(em$heterogene)

  # La meme saisie avec deux parametres laisse un degre de liberte : le test
  # redevient informatif.
  ab <- hstat_dl50_ajuste(m, "abbott")
  expect_equal(ab$npar, 2L)
  expect_true(ab$informatif)

  # L'essai de reference, lui, n'est pas concerne.
  f <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  expect_true(f$informatif)
  expect_true(grepl("légitime", hstat_dl50_verdict(f)$ajustement, fixed = TRUE))
})

test_that("un modele contraint ne peut pas depasser le modele libre", {
  # L'invariant d'emboitement. S'il est viole, c'est l'optimisation qui a
  # echoue -- et le `max(0, ...)` transformerait cet echec en Chi-2 nul, donc
  # en « non significatif » : la conclusion inverse de la verite.
  m1 <- list(ok = TRUE, ll = -100, npar = 6)
  m0 <- list(ok = TRUE, ll = -98,  npar = 4)      # contraint MEILLEUR : impossible
  r <- .hstat_dl50_ligne_test("test", m0, m1, 2, 0.05, "oui", "non")
  expect_true(is.na(r$Chi2))
  expect_true(grepl("pas effectué", r$Conclusion, fixed = TRUE))
  # Le cas normal passe.
  r2 <- .hstat_dl50_ligne_test("test", list(ok = TRUE, ll = -104, npar = 4), m1,
                               2, 0.05, "oui", "non")
  expect_equal(r2$Chi2, 8)
  # Et une estimation en echec ne rend pas un test rassurant.
  r3 <- .hstat_dl50_ligne_test("test", list(ok = FALSE, ll = NA_real_), m1,
                               2, 0.05, "oui", "non")
  expect_true(is.na(r3$Chi2))
})

test_that("une mortalite temoin elevee est signalee, sans bloquer le calcul", {
  # Au-dela d'environ 20 %, la correction d'Abbott devient peu fiable et
  # l'usage veut qu'on refasse l'essai (Finney). C'est un defaut de conduite
  # d'essai, pas de saisie : le dire suffit.
  haut <- hstat_dl50_essai(c(0.00848, 0.0163, 0.03423, 0.05053, 0.08476),
                           rep(90, 5), c(30, 43, 62, 75, 82), 90, 25)
  f <- hstat_dl50_ajuste(haut, "em")
  expect_true(isTRUE(f$ok))                 # le calcul continue
  expect_true(f$temoin_eleve)
  expect_gt(f$c_temoin, HSTAT_DL50_TEMOIN_MAX)
  al <- hstat_dl50_verdict(f)$alertes
  expect_true(any(grepl("témoin élevée", al, fixed = TRUE)))

  bas <- hstat_dl50_essai(haut$doses$dose, haut$doses$n, haut$doses$x, 90, 4)
  expect_false(hstat_dl50_ajuste(bas, "em")$temoin_eleve)
})

test_that("le verdict porte la dose letale, son unite et son intervalle", {
  # Le bloc de resume annoncait l'equation, la mortalite naturelle et le test
  # d'ajustement -- mais PAS la dose letale, et jamais son unite. On ouvre un
  # module de DL50 pour lire une DL50.
  e <- .hstat_dl50_essai_ref()
  e$champs <- list(unite = "µg / insecte")
  f <- hstat_dl50_ajuste(e, "em")
  v <- hstat_dl50_verdict(f)
  expect_equal(v$seuil, 50)
  expect_equal(v$unite, "µg / insecte")
  expect_true(grepl("DL50 = ", v$texte, fixed = TRUE))
  expect_true(grepl("µg / insecte", v$texte, fixed = TRUE))
  expect_true(grepl("[", v$texte, fixed = TRUE))
  expect_equal(hstat_dl50_libelle_dose(f), "Dose (µg / insecte)")

  # Sans unite renseignee, le libelle reste nu -- pas de parenthese vide.
  f2 <- hstat_dl50_ajuste(.hstat_dl50_essai_ref(), "em")
  expect_equal(hstat_dl50_unite(f2), "")
  expect_equal(hstat_dl50_libelle_dose(f2), tr("Dose"))
  expect_false(grepl("()", hstat_dl50_verdict(f2)$texte, fixed = TRUE))

  # Des seuils qui ne comprennent pas 50 : on repond sur le premier demande,
  # pas sur un seuil que personne n'a demande.
  v3 <- hstat_dl50_verdict(f, seuils = c(20, 80))
  expect_true(v3$seuil %in% c(20, 80))
  expect_true(grepl(sprintf("DL%g = ", v3$seuil), v3$texte, fixed = TRUE))
})

test_that("coller trois colonnes d'un tableur : la virgule a un seul role", {
  # Un tableur francais copie « 0,00063 » avec des TABULATIONS : la virgule y
  # est une decimale. Un CSV anglais copie « 0.00063,25,5 » : elle y est un
  # separateur. On ne peut pas lui donner les deux roles a la fois.
  fr <- hstat_dl50_coller("Dose\tEffectif\tMorts\n0,00063\t25\t5\n0,00125\t25\t7\n0,0025\t25\t9")
  expect_true(fr$ok)
  expect_equal(fr$lignes, 3L)
  expect_equal(fr$decimale, ",")
  expect_equal(fr$table$Dose, c(0.00063, 0.00125, 0.0025))
  expect_equal(fr$table$Morts, c(5, 7, 9))

  en <- hstat_dl50_coller("0.00063,25,5\n0.00125,25,7\n0.0025,25,9")
  expect_true(en$ok)
  expect_equal(en$decimale, ".")
  expect_equal(en$table$Dose, c(0.00063, 0.00125, 0.0025))

  pv <- hstat_dl50_coller("0,00063;25;5\n0,00125;25;7\n0,0025;25;9")
  expect_equal(pv$table$Dose, c(0.00063, 0.00125, 0.0025))
  expect_equal(pv$decimale, ",")

  esp <- hstat_dl50_coller("0.00063 25 5\n0.00125 25 7\n0.0025 25 9")
  expect_equal(esp$table$Effectif, rep(25, 3))

  # L'en-tete se reconnait a ce qu'il ne porte AUCUN nombre.
  expect_equal(nrow(hstat_dl50_coller("dose;n;morts\n1;20;5\n2;20;9")$table), 2L)
  expect_equal(nrow(hstat_dl50_coller("1;20;5\n2;20;9")$table), 2L)

  # Ce qui ne se lit pas se refuse, en disant quoi corriger.
  for (mauvais in list("", "   ", "dose\tn\tmorts",
                       "0.001\t25\n0.002\t25\t7", "0.001\t25\t5\n0.002\tabc\t7")) {
    r <- hstat_dl50_coller(mauvais)
    expect_false(isTRUE(r$ok))
    expect_true(nzchar(r$message))
    expect_true(grepl("[Cc]opiez|[Ii]l faut|[Vv]érifiez|manque", r$message))
  }

  # Le collage alimente reellement un essai analysable.
  cl <- hstat_dl50_coller("0,00063\t25\t5\n0,00125\t25\t7\n0,0025\t25\t9\n0,005\t25\t11\n0,01\t25\t14")
  es <- hstat_dl50_essai(cl$table$Dose, cl$table$Effectif, cl$table$Morts, 25, 0)
  expect_true(isTRUE(hstat_dl50_ajuste(es, "em")$ok))
})

test_that("le rapport de puissance retrouve un ratio de resistance connu", {
  # C'est le chiffre que publie la surveillance des resistances : combien de
  # fois faut-il plus de produit pour tuer la souche etudiee. On le verifie sur
  # deux droites PARALLELES par construction, dont le rapport vrai vaut 5.
  set.seed(7)
  simule <- function(dl50, b, doses, n = 400) {
    a <- -b * log10(dl50)
    stats::rbinom(length(doses), n, stats::pnorm(a + b * log10(doses)))
  }
  d <- c(0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10)
  sens <- hstat_dl50_essai(d, rep(400, 8), simule(0.5, 2, d), 400, 0, titre = "sensible")
  resi <- hstat_dl50_essai(d, rep(400, 8), simule(2.5, 2, d), 400, 0, titre = "résistante")

  r <- hstat_dl50_puissance(list(sensible = sens, resistante = resi),
                            reference = 1, scenario = "nulle")
  expect_equal(nrow(r), 2L)
  expect_true(r$Reference[1])
  expect_equal(r$Rapport[1], 1)
  # Le rapport vrai vaut 5, et son intervalle doit le contenir.
  expect_equal(r$Rapport[2], 5, tolerance = 0.15)
  expect_lt(r$Limite_inf[2], 5)
  expect_gt(r$Limite_sup[2], 5)
  expect_equal(r$Intervalle[2], "Fieller")
  # La pente commune retrouve la pente simulee. Et elle sort NUE : le vecteur de
  # parametres nomme ses elements, et ce nom se propagerait au rapport et a ses
  # bornes, ou il ne designerait plus rien.
  expect_null(names(attr(r, "pente_commune")))
  expect_equal(attr(r, "pente_commune"), 2, tolerance = 0.1)
  # Et le rapport reste coherent avec les DL50 ajustees essai par essai.
  expect_equal(r$Rapport[2], r$DL50[2] / r$DL50[1], tolerance = 0.05)

  # Le parallelisme est teste, et il passe : aucun avertissement.
  pa <- attr(r, "parallelisme")
  expect_equal(nrow(pa), 1L)
  expect_gt(pa$p, 0.05)
  expect_null(attr(r, "avertissement"))

  # INVERSER LA REFERENCE INVERSE LE RAPPORT. C'est la verification qui attrape
  # un signe pris a l'envers dans (a_ref - a_i) / b -- une erreur qui rendrait
  # un ratio de resistance parfaitement plausible, et faux.
  r2 <- hstat_dl50_puissance(list(sensible = sens, resistante = resi),
                             reference = 2, scenario = "nulle")
  expect_equal(r2$Rapport[1], 1 / r$Rapport[2], tolerance = 0.02)
  expect_true(r2$Reference[2])
})

test_that("un rapport de puissance sans parallelisme est annonce comme tel", {
  # Le rapport n'existe qu'a pente commune. Si les droites ne sont pas
  # paralleles, il change avec le niveau de mortalite : il vaut 3 a la DL50 et
  # 12 a la DL90, et publier « R = 3 » revient a choisir un chiffre parmi
  # d'autres sans le dire.
  set.seed(11)
  simule <- function(dl50, b, doses, n = 400) {
    a <- -b * log10(dl50)
    stats::rbinom(length(doses), n, stats::pnorm(a + b * log10(doses)))
  }
  d <- c(0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10)
  s1 <- hstat_dl50_essai(d, rep(400, 8), simule(0.5, 2, d), 400, 0, titre = "a")
  s2 <- hstat_dl50_essai(d, rep(400, 8), simule(2.5, 4.5, d), 400, 0, titre = "b")
  r <- hstat_dl50_puissance(list(a = s1, b = s2), 1, "nulle")
  expect_equal(nrow(r), 2L)
  expect_lt(attr(r, "parallelisme")$p, 0.05)
  av <- attr(r, "avertissement")
  expect_true(!is.null(av) && nzchar(av))
  expect_true(grepl("pas parallèles", av, fixed = TRUE))
  expect_true(grepl("seuil par seuil", av, fixed = TRUE))
})

test_that("le rapport de puissance refuse ce qu'il ne peut pas calculer", {
  a <- .hstat_dl50_essai_ref()
  expect_equal(nrow(hstat_dl50_puissance(list(a))), 0L)
  expect_true(grepl("au moins deux essais",
                    attr(hstat_dl50_puissance(list(a)), "message"), fixed = TRUE))
  # Une saisie invalide est nommee, pas contournee.
  mauvais <- hstat_dl50_essai(c(1, 2, 3), rep(20, 3), c(1, 25, 9))
  expect_equal(nrow(hstat_dl50_puissance(list(a, mauvais))), 0L)
  # Le scenario a mortalite nulle reste refuse devant un temoin qui compte des
  # morts, comme pour la comparaison.
  t <- hstat_dl50_essai(a$doses$dose, a$doses$n, a$doses$x, 25, 3)
  r <- hstat_dl50_puissance(list(a, t), 1, "nulle")
  expect_equal(nrow(r), 0L)
  expect_true(grepl("mortalité naturelle", attr(r, "message")))
  # UNE REFERENCE HORS BORNES EST REFUSEE. Ce test exigeait l'inverse -- qu'elle
  # retombe sur le premier essai -- et c'etait un mauvais choix de ma part :
  # demander le rapport par rapport a l'essai 99 sur un jeu qui en compte deux
  # rendait le tableau du premier, sa colonne « Reference » cochee sur lui. Un
  # resultat plausible, et pas celui qu'on avait demande ; c'est le defaut que
  # ce depot traque partout ailleurs.
  r2 <- hstat_dl50_puissance(list(a = a, b = t), reference = 99, "heterogene")
  expect_equal(nrow(r2), 0L)
  expect_true(grepl("hors bornes", attr(r2, "message"), fixed = TRUE))
})

test_that("le module DL50 est branche et depose son contexte", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  ux  <- paste(readLines(file.path(root, "inst", "app", "UX.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  srv <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl('tabName = "dl50"', ux, fixed = TRUE))
  expect_true(grepl('mod_dl50_ui("dl50")', ux, fixed = TRUE))
  expect_true(grepl('mod_dl50_server("dl50", values)', srv, fixed = TRUE))

  mod <- paste(readLines(file.path(root, "R", "mod_dl50.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # La table de saisie passe par un proxy : la relire dans le rendu la
  # reconstruirait a chaque cellule modifiee, detruisant sous le curseur celle
  # que l'on est en train d'editer.
  expect_true(grepl("DT::dataTableProxy(\"saisie\")", mod, fixed = TRUE))
  expect_true(grepl("DT::replaceData(proxy_saisie", mod, fixed = TRUE))
  # Le corps du rendu lit la table SOUS `isolate` -- le nom du reactif a
  # change quand la saisie en pourcentage est arrivee (`saisie_affichee()`),
  # et epingler la chaine exacte faisait echouer le test sur un renommage
  # alors que la propriete gardee, elle, tenait toujours.
  # `(?s)` : sans le mode « point-mange-la-ligne », le motif ne franchit pas le
  # premier retour a la ligne et ne ramene RIEN. Un bloc vide fait passer les
  # verifications suivantes sans rien lire -- le test qui ne balaie rien.
  bloc <- regmatches(mod, regexpr(
    "(?s)output\\$saisie <- DT::renderDT\\(\\{.*?\\n    \\}\\)", mod, perl = TRUE))
  expect_equal(length(bloc), 1L)
  expect_gt(nchar(bloc), 200)
  expect_true(grepl("shiny::isolate(", bloc, fixed = TRUE))
  # ET LA TABLE N'EST LUE QUE LA. On retire du bloc les appels a `isolate()`,
  # puis on exige qu'il ne reste plus aucune lecture du reactif de saisie : une
  # lecture nue rendrait le rendu dependant de chaque cellule modifiee, et
  # ramenerait exactement le defaut que le proxy existe pour eviter.
  reste <- gsub("shiny::isolate\\([^)]*\\)", "", bloc)
  expect_false(grepl("saisie_affichee()", reste, fixed = TRUE))
  expect_false(grepl("saisie()", reste, fixed = TRUE))
  # Le choix d'unite, lui, DOIT y etre lu nu : l'en-tete de colonne le suit, et
  # la table doit donc etre reconstruite quand il change.
  expect_true(grepl("input$saisieUnite", reste, fixed = TRUE))
})

test_that("le module de doses est branche et depose son contexte", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  ux  <- paste(readLines(file.path(root, "inst", "app", "UX.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  srv <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Un module non appele est un fichier mort : l'onglet existe, il reste vide.
  expect_true(grepl('tabName = "dosage"', ux, fixed = TRUE))
  expect_true(grepl('mod_dosage_ui("dosage")', ux, fixed = TRUE))
  expect_true(grepl('mod_dosage_server("dosage", values)', srv, fixed = TRUE))

  mod <- paste(readLines(file.path(root, "R", "mod_dosage.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
})


# =============================================================================
#  LE BANC DE MUTATION SE GARDE LUI-MEME
# -----------------------------------------------------------------------------
#  `tools/mutation.R` est l'outil qui mesure si les assertions gardent quelque
#  chose. Quand il se trompe, il se trompe TOUJOURS DANS LE SENS RASSURANT : un
#  test non joue ne peut pas echouer, la mutation passe, et le rapport annonce
#  un trou dans les assertions la ou il n'y a qu'un trou dans le banc. Trois
#  fausses pistes ont ete suivies avant que la cause soit vue.
# =============================================================================

test_that("le banc de mutation ne se laisse pas ecraser par le code qu'il joue", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  banc <- file.path(root, "tools", "mutation.R")
  skip_if_not(file.exists(banc))
  rscript <- file.path(R.home("bin"), "Rscript")
  skip_if_not(file.exists(rscript))

  # Une suite MINIATURE, pas la vraie : on mesure le banc, pas le depot.
  # Le premier test ecrit une variable nommee `motif` -- ce que fait le
  # balayage des p-values. Le banc rangeait SON motif sous ce nom dans
  # `globalenv()`, et `test_that()` evalue le corps du test dans le cadre de
  # son appelant : le filtre se retrouvait a comparer les descriptions
  # suivantes a une expression reguliere de code R, qui ne colle a aucune.
  # TOUS LES TESTS D'APRES ETAIENT SAUTES EN SILENCE.
  d <- file.path(tempdir(), paste0("hstat_banc_", as.integer(runif(1, 1e6, 1e7))))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  faux <- file.path(d, "test-faux.R")
  writeLines(c(
    'test_that("premier test, qui ecrit une variable nommee motif", {',
    '  motif <- "if\\\\s*\\\\([^)]*\\\\$p\\\\.value"',
    '  expect_true(nzchar(motif))',
    '})',
    'test_that("second test, qui doit encore etre joue", {',
    '  expect_true(TRUE)',
    '})'), faux)

  sortie <- suppressWarnings(system2(rscript, c(shQuote(banc),
                                                shQuote("premier test|second test"),
                                                shQuote(faux)),
                                     stdout = TRUE, stderr = TRUE))
  txt <- paste(sortie, collapse = "\n")
  # LES DEUX tests doivent avoir ete retenus ET joues.
  expect_true(grepl("TESTS: 2 / 2", txt, fixed = TRUE), info = txt)
  expect_true(grepl("ASSERTIONS: 2", txt, fixed = TRUE), info = txt)

  # ET un filtre qui ne retient rien est une ERREUR, pas un « 0 echec ».
  # Sans ce refus, une faute de frappe dans le motif se lit exactement comme
  # une mutation non detectee.
  st <- suppressWarnings(system2(rscript, c(shQuote(banc),
                                            shQuote("aucun test ne porte ce nom"),
                                            shQuote(faux)),
                                 stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("ne retient aucun", st)), info = paste(st, collapse = "\n"))
})

test_that("le collage de la récolte lit trois colonnes, dont une de texte", {
  # L'EN-TETE SE RECONNAIT SUR LES COLONNES QUI DOIVENT ETRE NUMERIQUES.
  # Chercher « aucun nombre sur la ligne » -- la regle du collage des DL50, ou
  # les trois colonnes sont numeriques -- classerait ici toute premiere ligne
  # comme un en-tete, puisque la modalite n'est jamais un nombre.
  r <- hstat_rdt_coller("Traitement\tMasse\tSurface\nT0\t12,5\t0,25\nT1\t18,3\t0,25")
  expect_true(r$ok)
  expect_equal(nrow(r$table), 2L)
  expect_equal(r$table$Modalite, c("T0", "T1"))
  expect_equal(r$table$Masse, c(12.5, 18.3))

  expect_true(r$entete)
  # Et une modalite qui COMMENCE par un chiffre -- « 2SP », « 2PV », des noms
  # de traitement courants -- ne fait pas davantage un en-tete.
  r2 <- hstat_rdt_coller("2SP;12,5;0,25\n2PV;18,3;0,25")
  expect_true(r2$ok)
  expect_equal(nrow(r2$table), 2L)
  expect_equal(r2$table$Modalite, c("2SP", "2PV"))
  expect_false(r2$entete)

  # L'EN-TETE SE JUGE SUR LES COLONNES QUI DOIVENT ETRE NUMERIQUES. Sa premiere
  # cellule, elle, peut parfaitement etre un nombre -- une campagne, un numero
  # d'essai, un code de parcelle. « Aucun nombre sur la ligne entiere » ferait
  # alors partir le titre des colonnes dans les donnees, et le refus parlerait
  # d'une masse illisible sans jamais dire qu'il s'agit de l'en-tete.
  r5 <- hstat_rdt_coller("2024;Masse;Surface\nT0;12,5;0,25\nT1;18,3;0,25")
  expect_true(r5$ok)
  expect_true(r5$entete)
  expect_equal(nrow(r5$table), 2L)
  expect_equal(r5$table$Modalite, c("T0", "T1"))

  # ET LA LIGNE SAUTEE SE DIT. La reconnaissance est une heuristique : une
  # observation dont la masse serait illisible lui ressemble, et la taire
  # ferait disparaitre une ligne sans cause visible.
  expect_true(grepl("r$entete", paste(readLines(
    .hstat_module_path("mod_yield.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"), fixed = TRUE))

  # LA VIRGULE EST UNE DECIMALE DES QUE LA LIGNE PORTE UN VRAI SEPARATEUR.
  # « T0;12,5;0,25 » sort d'un tableur francais : y voir un separateur ferait
  # cinq champs de trois, donc un refus incomprehensible.
  expect_equal(r2$decimale, ",")
  expect_equal(r2$table$Surface, c(0.25, 0.25))
  # Sans tabulation ni point-virgule, elle redevient le separateur.
  r3 <- hstat_rdt_coller("T0,12.5,0.25\nT1,18.3,0.25")
  expect_true(r3$ok)
  expect_equal(r3$decimale, ".")
  expect_equal(r3$table$Masse, c(12.5, 18.3))

  # La quatrieme colonne est la repetition ; absente partout, elle ne fait pas
  # une colonne vide -- qui laisserait croire a une information manquante.
  r4 <- hstat_rdt_coller("T0;12,5;0,25;B1\nT0;13,1;0,25;B2")
  expect_equal(r4$table$Repetition, c("B1", "B2"))
  expect_true(all(is.na(r$table$Repetition)))

  # LES REFUS NOMMENT LA LIGNE FAUTIVE. « Collage invalide » obligerait a
  # relire trente lignes pour trouver la seule qui pose probleme.
  expect_false(hstat_rdt_coller("")$ok)
  expect_false(hstat_rdt_coller("   ")$ok)
  expect_match(hstat_rdt_coller("T0;12,5\nT1;18,3;0,25")$message, "1")
  expect_match(hstat_rdt_coller("T0;abc;0,25")$message, "1")
  expect_match(hstat_rdt_coller(";12,5;0,25")$message, "1")
  # Un collage qui n'est QUE l'en-tete se dit, plutot que de rendre zero ligne
  # en silence.
  expect_match(hstat_rdt_coller("Traitement\tMasse\tSurface")$message,
               "en-tête")
})

test_that("la grille de saisie ne retient que ce qui est exploitable", {
  v <- hstat_rdt_saisie_vide(4L)
  expect_equal(nrow(v), 4L)
  expect_equal(names(v), c("Modalite", "Masse", "Surface", "Repetition"))
  # UNE GRILLE VIDE N'EST PAS UNE ERREUR : elle attend qu'on la remplisse.
  # Ecarter les lignes vides ici, une fois, evite que chaque lecteur refasse --
  # ou oublie -- le tri, et que le calcul compte une modalite « NA » parmi les
  # traitements.
  expect_equal(nrow(hstat_rdt_saisie_propre(v)), 0L)
  # Rien du tout n'est pas davantage une erreur : l'onglet s'ouvre avant que la
  # grille existe.
  expect_equal(nrow(hstat_rdt_saisie_propre(NULL)), 0L)
  expect_equal(nrow(hstat_rdt_saisie_propre(hstat_rdt_saisie_vide(0L))), 0L)

  v$Modalite[c(1, 3)] <- c("T0", "T1")
  v$Masse[c(1, 3)] <- c(10, 20)
  v$Surface[c(1, 3)] <- c(1, 1)
  p <- hstat_rdt_saisie_propre(v)
  expect_equal(nrow(p), 2L)
  expect_equal(p$Modalite, c("T0", "T1"))
  # Une repetition vide partout perd sa colonne, comme dans le tableau des
  # resultats : une colonne vide fait croire a une information absente.
  expect_false("Repetition" %in% names(p))
  v$Repetition[1] <- "B1"
  expect_true("Repetition" %in% names(hstat_rdt_saisie_propre(v)))

  # Une ligne A MOITIE remplie n'est pas exploitable : la surface manquante
  # rendrait un rendement infini, pas une valeur.
  v2 <- hstat_rdt_saisie_vide(2L)
  v2$Modalite <- c("T0", "T1"); v2$Masse <- c(10, 20); v2$Surface <- c(1, NA)
  expect_equal(nrow(hstat_rdt_saisie_propre(v2)), 1L)
  # Une modalite faite d'espaces n'en est pas une.
  v2$Surface <- c(1, 1); v2$Modalite <- c("T0", "   ")
  expect_equal(nrow(hstat_rdt_saisie_propre(v2)), 1L)

  # Et le calcul traverse une grille vide sans lever : l'onglet s'ouvre avant
  # qu'une seule ligne soit tapee.
  vide <- hstat_rdt_saisie_propre(hstat_rdt_saisie_vide(3L))
  expect_equal(nrow(hstat_rdt_complet(vide, "Modalite", "Masse", "Surface", "")), 0L)
})

test_that("la saisie manuelle du rendement suit l'idiome de DL50/CL50", {
  chemin <- .hstat_module_path("mod_yield.R")
  skip_if_not(file.exists(chemin))
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # LE RENDU ISOLE LA TABLE. Relire `saisie()` dans `renderDT` la reconstruit a
  # chaque cellule modifiee : la cellule en cours d'edition est detruite sous
  # le curseur, et la saisie devient inutilisable.
  expect_true(grepl("DT::datatable(shiny::isolate(saisie())", txt, fixed = TRUE))
  expect_true(grepl("DT::dataTableProxy(\"yieldSaisie\")", txt, fixed = TRUE))
  expect_true(grepl("DT::replaceData(proxy_saisie", txt, fixed = TRUE))
  expect_true(grepl("input$yieldSaisie_cell_edit", txt, fixed = TRUE))

  # LA MODALITE EST DU TEXTE. Passer toutes les colonnes au meme convertisseur
  # numerique ferait disparaitre le nom du traitement des qu'il n'est pas un
  # nombre -- c'est-a-dire toujours.
  expect_true(grepl('names(d)[j] %in% c("Masse", "Surface")', txt, fixed = TRUE))

  # LA SOURCE EST UN CHOIX, ET C'EST `donnees()` QUI LE LIT. Basculer tout seul
  # sur la saisie des qu'un fichier manque ferait changer les chiffres sans que
  # personne ait rien demande -- et poser le choix ailleurs que dans le reactif
  # qui sert les donnees le laisserait sans effet.
  corps <- sub("^.*donnees <- shiny::reactive\\(\\{", "", txt)
  # On s'arrete a la fermeture du reactif : le bloc suivant lit lui aussi
  # `input$yieldSource`, et l'englober ferait passer le test alors que
  # `donnees()` ne regarderait plus la source du tout.
  corps <- sub("\n    \\}\\).*$", "", corps)
  expect_match(corps, 'input$yieldSource', fixed = TRUE)
  expect_match(corps, 'saisie_utile()', fixed = TRUE)
  expect_true(grepl("hstat_rdt_saisie_propre(saisie())", txt, fixed = TRUE))
  # Le fichier chargé reste la source par defaut.
  expect_true(grepl('selected = "fichier"', txt, fixed = TRUE))
})

test_that("la palette par défaut de ggplot2 colore chaque modalité", {
  # Elle ne doit JAMAIS rejoindre la liste des qualitatives : un test verifie
  # que chaque entree de celle-ci existe chez RColorBrewer, et un nom inconnu
  # fait tomber tout le graphique de qui l'aurait choisi.
  expect_false(unname(HSTAT_PALETTE_GG) %in% unname(HSTAT_PALETTES_QUALI))
  expect_false(unname(HSTAT_PALETTE_GG) %in% unname(HSTAT_PALETTES_DEGRADE))

  # ELLE VIENT EN TETE DU CATALOGUE. C'est la seule qui ne plafonne pas : quand
  # on ignore combien il y aura de modalites, c'est le seul defaut sur.
  ch <- hstat_palettes_choix()
  expect_identical(unname(unlist(ch))[1], unname(HSTAT_PALETTE_GG))
  expect_true(all(unlist(HSTAT_PALETTES_DEGRADE) %in% unlist(ch)))
  expect_false(any(unlist(HSTAT_PALETTES_DEGRADE) %in%
                     unlist(hstat_palettes_choix(degrades = FALSE))))

  skip_if_not_installed("ggplot2")
  d <- data.frame(m = factor(sprintf("T%02d", 1:15)), v = 1:15)
  teintes <- function(pal, ...) {
    p <- ggplot2::ggplot(d, ggplot2::aes(m, v, fill = m)) + ggplot2::geom_col()
    for (sc in hstat_scales_palette(pal, ...)) p <- p + sc
    suppressWarnings(ggplot2::ggplot_build(p)$data[[1]]$fill)
  }
  # CE QU'ELLE APPORTE, MESURE : quinze modalites, quinze teintes. Les
  # qualitatives de Brewer PLAFONNENT (Set2 a 8) et rendent les series
  # surnumeraires en `NA`, donc en gris -- un essai a quinze traitements n'a
  # rien d'exotique en agronomie.
  gg <- teintes(unname(HSTAT_PALETTE_GG))
  expect_equal(length(unique(gg)), 15L)
  expect_false(anyNA(gg))

  # ELLE SE POSE, ELLE NE S'OMET PAS. Ne rien rendre donnerait les MEMES
  # couleurs -- l'echelle par defaut de ggplot est deja `hue` -- si bien que le
  # defaut serait invisible sur la seule teinte. Ce qui se perdrait, c'est tout
  # ce que l'appelant passe a l'echelle : le module des seuils y fait voyager
  # le titre de sa legende, qui disparaitrait sans un mot.
  expect_false(is.null(hstat_scales_palette(unname(HSTAT_PALETTE_GG))))
  expect_equal(length(hstat_scales_palette(unname(HSTAT_PALETTE_GG))), 2L)
  lg <- ggplot2::ggplot(d, ggplot2::aes(m, v, fill = m)) + ggplot2::geom_col()
  for (sc in hstat_scales_palette(unname(HSTAT_PALETTE_GG), colour = FALSE,
                                  name = "Traitements")) lg <- lg + sc
  expect_equal(ggplot2::ggplot_build(lg)$plot$scales$scales[[1]]$name,
               "Traitements")
  set2 <- teintes("Set2")
  expect_equal(length(unique(set2[!is.na(set2)])), 8L)
  expect_gt(sum(is.na(set2)), 0L)

  # UN NOM INCONNU NE POSE RIEN. Lui appliquer `scale_*_brewer()` ferait
  # avertir ggplot a chaque trace et rendrait un graphique gris ; ne rien poser
  # laisse l'echelle par defaut, qui est deja celle de ggplot2.
  expect_null(hstat_scales_palette("PaletteQuiNExistePas"))
  expect_null(hstat_scales_palette(""))
  expect_null(hstat_scales_palette(NULL))
  expect_equal(length(unique(teintes("PaletteQuiNExistePas"))), 15L)

  # Le sens de l'echelle se choisit, et le libelle de legende voyage jusqu'a
  # elle : c'est ce qui permet a l'aide commune de servir un module qui renomme
  # sa legende plutot que d'en faire un cas particulier.
  expect_equal(length(hstat_scales_palette("Set2")), 2L)
  expect_equal(length(hstat_scales_palette("Set2", colour = FALSE)), 1L)
  expect_s3_class(hstat_scales_palette("Set2", colour = FALSE,
                                       name = "Traitements")[[1]], "Scale")
})

test_that("la palette ne se pose qu'à un endroit", {
  # SIX MODULES PORTAIENT CHACUN LEUR LISTE ET LEUR `switch`, et ils avaient
  # diverge : le post-hoc offrait la palette de ggplot2 sous le libelle
  # « Défaut (gris) » -- qui est faux, elle rend quinze teintes distinctes --
  # et les seuils d'efficacite ne l'offraient pas du tout.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  for (m in c("mod_tests.R", "mod_threshold.R", "mod_yield.R", "mod_dl50.R")) {
    chemin <- .hstat_module_path(m)
    skip_if_not(file.exists(chemin))
    txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_true(grepl("hstat_palettes_choix(", txt, fixed = TRUE), info = m)
    expect_true(grepl("hstat_scales_palette(", txt, fixed = TRUE), info = m)
    # Plus aucun `scale_*_brewer()` pose a la main : c'est la recopie qui a
    # fait diverger les six listes.
    expect_false(grepl("scale_fill_brewer(palette = ", txt, fixed = TRUE), info = m)
    expect_false(grepl("scale_colour_brewer(palette = ", txt, fixed = TRUE), info = m)
    expect_false(grepl("scale_color_brewer(palette = ", txt, fixed = TRUE), info = m)
    # Et le libelle faux ne revient pas.
    expect_false(grepl("Défaut (gris)", txt, fixed = TRUE), info = m)
  }
})

test_that("un nom de couleur seul n'entre pas au dictionnaire", {
  # UN NOM DE COULEUR COURT EST UNE VALEUR DE DONNEES PLAUSIBLE. « Bleus »,
  # « Verts », « Mauve » peuvent etre les modalites d'une colonne du fichier de
  # l'utilisateur : entres seuls au dictionnaire, ils seraient traduits jusque
  # DANS ses donnees. Le libelle porte donc ce qu'il est -- un degrade.
  for (lib in names(HSTAT_PALETTES_DEGRADE))
    expect_false(lib %in% c("Bleus", "Verts", "Rouges", "Violets", "Mauve",
                            "Bleu-vert", "Spectral"),
                 info = lib)
  expect_true(all(grepl("égradé|Spectral \\(|Rouge-jaune",
                        names(HSTAT_PALETTES_DEGRADE))))

  # Et chaque libelle offert a l'ecran est traduit : le defaut corrige etait
  # qu'ils restaient tous en francais dans la version anglaise.
  # Et TOUS les libelles de palette sont traduits -- qualitatives comprises.
  # Le defaut corrige etait qu'ils restaient en francais dans la version
  # anglaise : « Set1 - vive », « Dark2 - soutenue »… au milieu d'une interface
  # anglaise, alors meme que la liste est declaree une seule fois.
  dico <- hstat_i18n_dict("en")
  skip_if(!length(dico))
  for (lib in c(names(HSTAT_PALETTES_QUALI), names(HSTAT_PALETTES_DEGRADE),
                names(HSTAT_PALETTE_GG)))
    expect_true(lib %in% names(dico), info = lib)
})

test_that("le refus de la variable de répétition passe par un gabarit", {
  # `sprintf(paste0(...))` compose une phrase qui n'existe nulle part comme
  # chaine entiere : ni le dictionnaire du navigateur ni `tr()` ne peuvent
  # l'atteindre, et elle ressort en francais au milieu d'une interface
  # anglaise. Seul `trf()` traduit le GABARIT avant d'appliquer `sprintf`.
  d <- data.frame(Trait = c("T0", "T1"), Y = c(10, 5), stringsAsFactors = FALSE)
  r <- hstat_efficacite(d, "Trait", "Y", "T0", var_repetition = "Bloc")
  msg <- attr(r, "message")
  expect_true(nzchar(msg %||% ""))
  expect_match(msg, "Bloc")
  # La phrase entiere est au dictionnaire sous sa forme de gabarit.
  expect_true(grepl("%s", tr(
    "La variable de répétition « %s » est introuvable dans les données : choisissez-en une autre.",
    lang = "en"), fixed = TRUE))
  expect_false(identical(
    tr("La variable de répétition « %s » est introuvable dans les données : choisissez-en une autre.",
       lang = "en"),
    "La variable de répétition « %s » est introuvable dans les données : choisissez-en une autre."))

  # Et le module ne compose plus la phrase a la main.
  chemin <- .hstat_module_path("utils.R")
  skip_if_not(file.exists(chemin))
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("sprintf(paste0(\"La variable de r", txt, fixed = TRUE))
})

test_that("« Gain de rendement » precede les seuils d'efficacite", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  chemin <- file.path(root, "inst", "app", "UX.R")
  skip_if_not(file.exists(chemin))
  txt <- readLines(chemin, warn = FALSE, encoding = "UTF-8")
  pos <- function(tab) grep(sprintf('tabName = "%s"', tab), txt)[1]
  # LE MENU EST RANGE PAR NATURE D'ANALYSE, PLUS PAR CHRONOLOGIE D'ESSAI.
  # Cette assertion exigeait `design < yield` -- le gain de rendement venant
  # APRES le plan d'experience, dans l'ordre ou l'on travaille. Le rangement
  # demande est autre : rendement, seuils, diversite, epidemiologie et DL50
  # sont des analyses INFERENTIELLES et rejoignent la section 3, tandis que la
  # section 5 garde ce qui se calcule AVANT l'essai (plan, doses). Les deux
  # lectures sont legitimes ; c'est un choix d'organisation, pas une regle que
  # les chiffres imposent, et il a ete tranche.
  #
  # Ce qui reste vrai et se garde : le gain de rendement precede les seuils
  # d'efficacite -- on calcule le rendement avant de le comparer a un seuil.
  expect_true(is.finite(pos("design")))
  expect_true(is.finite(pos("yield")))
  expect_true(is.finite(pos("threshold")))
  expect_lt(pos("yield"), pos("threshold"))
})


test_that("une agrégation numérique ne porte jamais sur une variable de texte", {
  # ONZE AVERTISSEMENTS POUR UNE COURBE VIDE. Les agregations *automatiques* de
  # l'onglet Visualisation appelaient `mean()` sans regarder le type : sur une
  # date de traitement notee « T1+13 », R rend NA en avertissant, une fois par
  # groupe, et le graphique sort vide sans un mot a l'ecran.
  d <- data.frame(a = 1:3, b = c("T1+13", "T2", "T3"),
                  l = c(TRUE, FALSE, TRUE), stringsAsFactors = FALSE)
  d$dt <- as.Date("2024-01-01") + 0:2
  expect_true(hstat_y_agregeable(d, "a"))
  expect_true(hstat_y_agregeable(d, "l"))
  expect_true(hstat_y_agregeable(d, "dt"))
  expect_false(hstat_y_agregeable(d, "b"))
  # Les cas degeneres refusent, ils ne levent pas : la fonction alimente un
  # reactif de graphique.
  expect_false(hstat_y_agregeable(d, "absente"))
  expect_false(hstat_y_agregeable(NULL, "a"))
  expect_false(hstat_y_agregeable(d, NULL))
  expect_false(hstat_y_agregeable(d, ""))

  chemin <- .hstat_module_path("mod_viz.R")
  skip_if_not(file.exists(chemin))
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Le garde-fou est pose la ou l'agregation se declenche TOUTE SEULE.
  motif <- "has_duplicates && !isTRUE(input$useAggregation) && agregeable"
  expect_equal(length(gregexpr(motif, txt, fixed = TRUE)[[1]]), 2L)
  # Les parts d'un camembert se pesent par une colonne numerique, ou se
  # comptent : `sum()` sur du texte fait tomber tout le graphique.
  expect_false(grepl("sum(.data[[y_var]], na.rm = TRUE)", txt, fixed = TRUE))
  # ET LE REDESSIN EN BOUCLE NE REVIENT PAS : un `observeEvent` qui se
  # reprogramme relance l'observateur vingt fois par seconde, sans fin.
  #
  # LE BALAYAGE RETIRE LES COMMENTAIRES PAR L'ANALYSEUR DE R, jamais par une
  # heuristique : le commentaire qui documente la correction contient le motif,
  # et le test se signalait lui-meme. Un faux positif permanent finit toujours
  # par faire desactiver le test.
  pd <- utils::getParseData(parse(chemin, keep.source = TRUE))
  code <- paste(pd$text[pd$token != "COMMENT"], collapse = " ")
  expect_false(grepl("invalidateLater", code, fixed = TRUE))
})

test_that("renommer deux modalités pareil ne les fond pas en une seule", {
  n <- c("T0", "T1", "T2")
  expect_equal(unname(hstat_etiquettes_x(n)), n)
  expect_equal(names(hstat_etiquettes_x(n)), n)
  # Une etiquette s'applique ; vide ou absente, elle ne fait rien -- on
  # n'invente jamais un nom.
  expect_equal(unname(hstat_etiquettes_x(n, c(T1 = "Témoin traité"))),
               c("T0", "Témoin traité", "T2"))
  expect_equal(unname(hstat_etiquettes_x(n, c(T1 = "", T2 = NA))), n)
  expect_equal(unname(hstat_etiquettes_x(n, c(absente = "Z"))), n)
  # LA COLLISION EST LE PIEGE. Deux modalites affichees sous le meme nom se
  # lisent comme une seule, et les lettres de groupes deviennent
  # contradictoires sur ce qui parait etre une meme barre.
  r <- hstat_etiquettes_x(n, c(T1 = "X", T2 = "X"))
  # `as.character()` et non `unname()` : le second retire les NOMS, pas les
  # autres attributs, et la comparaison portait aussi sur « collisions ».
  expect_equal(as.character(r), n)
  expect_equal(attr(r, "collisions"), "X")
  # Y compris la collision avec un niveau qu'on n'a PAS renomme.
  r2 <- hstat_etiquettes_x(n, c(T1 = "T0"))
  expect_equal(as.character(r2), n)
  expect_equal(attr(r2, "collisions"), "T0")
})

test_that("un rendement déjà calculé donne les mêmes chiffres, sans le global", {
  d <- data.frame(Traitement = rep(c("T0", "T1", "T2"), each = 3),
                  Bloc = rep(1:3, 3),
                  Masse = c(10, 11, 12, 18, 19, 20, 14, 15, 16),
                  Surface = rep(0.01, 9), stringsAsFactors = FALSE)
  ref <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface",
                           non_traite = "T0", var_repetition = "Bloc")
  d$Rdt <- hstat_rendement(d$Masse, d$Surface)
  pret <- hstat_rdt_complet(d, "Traitement", NULL, NULL, non_traite = "T0",
                            var_repetition = "Bloc", var_rendement = "Rdt")
  # LE CHEMIN COURT DONNE LES MEMES NOMBRES : c'est ce qui autorise a l'offrir.
  expect_equal(pret$Rendement_moyen, ref$Rendement_moyen)
  expect_equal(pret$Rendement_somme, ref$Rendement_somme)
  expect_equal(pret$Gain_moyen, ref$Gain_moyen)
  expect_equal(pret$Ecart_type, ref$Ecart_type)
  expect_equal(pret$Repetitions, ref$Repetitions)
  # LE RENDEMENT GLOBAL EXIGE LES SURFACES : il est ABSENT, jamais remplace par
  # la moyenne -- ce serait annoncer une ponderation qui n'a pas eu lieu.
  expect_false("Rendement_global" %in% names(pret))
  expect_false("Gain_global" %in% names(pret))
  expect_true("Rendement_global" %in% names(ref))

  # L'unite est DECLAREE, et elle titre l'axe.
  u <- hstat_rdt_complet(d, "Traitement", NULL, NULL, non_traite = "T0",
                         sortie_masse = "tonne (1000 kg)",
                         sortie_surface = "hectare (ha)", var_rendement = "Rdt")
  expect_equal(attr(u, "unite"), "t/ha")

  # Un refus se dit, il ne rend pas un tableau vide muet.
  v <- hstat_rdt_table_prete(d, "Traitement", "")
  expect_equal(NROW(v), 0L)
  expect_true(grepl("rendement", attr(v, "message")))
  d$Texte <- letters[1:9]
  v2 <- hstat_rdt_table_prete(d, "Traitement", "Texte")
  expect_equal(NROW(v2), 0L)
  expect_true(grepl("num\u00e9rique", attr(v2, "message")))

  # UN GAIN NE SE CONVERTIT PAS : c'est un pourcentage.
  pc <- hstat_rdt_complet(d, "Traitement", NULL, NULL, non_traite = "T0",
                          var_rendement = "Rdt",
                          conv_masse = "tonne (1000 kg)",
                          conv_surface = "hectare (ha)")
  expect_true("Rendement_moyen_conv" %in% names(pc))
  expect_equal(pc$Rendement_moyen_conv, pc$Rendement_moyen / 1000)
  expect_length(grep("^Gain_.*_conv$", names(pc)), 0)
})

test_that("une boîte de résultats ne s'affiche pas avant d'avoir un résultat", {
  chemin <- .hstat_module_path("mod_tests.R")
  skip_if_not(file.exists(chemin))
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # UNE BOITE VIDE OCCUPE LA PLACE DES REGLAGES. Les six boites concernees ne
  # se construisent que sur demande.
  for (cond in c("output.hasTestResults", "output.hasLMPostHoc",
                 "output.hasMultivariatePosthoc", "output.hasRMPostHoc",
                 "input.showTransformTools"))
    expect_true(grepl(sprintf('condition = "%s"', cond), txt, fixed = TRUE),
                info = cond)
  # Le drapeau existe cote serveur, et il est calcule meme cache -- sinon la
  # boite ne reapparaitrait jamais.
  expect_true(grepl("output$hasTestResults <- shiny::reactive(", txt, fixed = TRUE))
  expect_true(grepl('outputOptions(output, "hasTestResults", suspendWhenHidden = FALSE)',
                    txt, fixed = TRUE))
  # LES PANNEAUX « AUCUN RESULTAT » SONT DEVENUS INATTEIGNABLES : les garder
  # serait du code mort dans une branche qui ne s'evalue jamais.
  for (cond in c("!output.hasLMPostHoc", "!output.hasMultivariatePosthoc",
                 "!output.hasRMPostHoc"))
    expect_false(grepl(sprintf('condition = "%s"', cond), txt, fixed = TRUE),
                 info = cond)
  # Le second bouton de diagnostic vivait dans la boite d'attente retiree : il
  # ne pouvait que refuser, il ne revient pas.
  expect_false(grepl("runManovaDiagnostic2", txt, fixed = TRUE))
})

test_that("les boutons de l'éditeur d'ordre post-hoc sont namespacés", {
  chemin <- .hstat_module_path("mod_tests.R")
  skip_if_not(file.exists(chemin))
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # CREES HORS DU NAMESPACE DU MODULE, les fleches haut/bas n'etaient jamais
  # vues par `input$moveUp_1` : l'ordre des categories ne bougeait pas.
  for (s in c("moveUp_", "moveDown_")) {
    expect_true(grepl(sprintf('ns(paste0("%s", i))', s), txt, fixed = TRUE), info = s)
    expect_false(grepl(sprintf('actionButton(paste0("%s", i)', s), txt, fixed = TRUE),
                 info = s)
  }
  # Meme regle pour les elements glissables de la visualisation.
  cv <- .hstat_module_path("mod_viz.R")
  skip_if_not(file.exists(cv))
  tv <- paste(readLines(cv, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl('id = ns(paste0("xorder_", i))', tv, fixed = TRUE))
})


test_that("tout identifiant de widget construit dans un module passe par ns()", {
  # LE DEFAUT LE PLUS SILENCIEUX DU DEPOT, ET LE TROISIEME DE SA FAMILLE.
  # Un widget cree HORS du namespace de son module n'existe pour personne :
  # `input[["xLevel_Alpha"]]` lit « visualization-xLevel_Alpha », le widget
  # porte « xLevel_Alpha ». Rien ne leve, rien ne s'affiche de travers -- le
  # reglage ne fait simplement RIEN.
  #
  # Mesure avant correction : le renommage des etiquettes de l'axe X et celui
  # des etiquettes de legende etaient morts dans le navigateur, et avec eux le
  # prefixe, le suffixe, la casse et la remise a zero, qui lisent les memes
  # champs. Trois champs poses, zero prefixe, zero notification au clic.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  widgets <- c("actionButton", "actionLink", "selectInput", "numericInput",
               "textInput", "checkboxInput", "radioButtons", "sliderInput",
               "selectizeInput", "textAreaInput", "fileInput", "downloadButton",
               "dateInput", "checkboxGroupInput", "colourInput", "pickerInput",
               "plotOutput", "uiOutput", "verbatimTextOutput", "tableOutput",
               "DTOutput", "dataTableOutput", "plotlyOutput")
  fautifs <- character(0)
  for (f in list.files(file.path(root, "R"), "^mod_.*\\.R$", full.names = TRUE)) {
    d <- utils::getParseData(parse(f, keep.source = TRUE))

    # UN MODULE PEUT SE DONNER SON PROPRE RACCOURCI. `mod_ai.R` definit
    # `id <- function(s) ns(...)` : ces noms-la namespacent aussi, et les
    # compter pour fautifs ferait echouer le test sur du code sain.
    aides <- unique(d$text[d$token == "SYMBOL" &
                           d$id %in% d$id[d$token == "SYMBOL"]])
    src <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    aides <- unique(c("ns", regmatches(src, gregexpr(
      "[A-Za-z_.][A-Za-z0-9_.]*(?=\\s*<-\\s*function\\([^)]*\\)\\s*ns\\()",
      src, perl = TRUE))[[1]]))

    w <- d[d$token == "SYMBOL_FUNCTION_CALL" & d$text %in% widgets, ]
    for (i in seq_len(nrow(w))) {
      appel <- d$parent[match(d$parent[match(w$id[i], d$id)], d$id)]
      args  <- d[d$parent == appel & d$token == "expr", ]
      if (nrow(args) < 2) next
      a1 <- args$id[2]
      # tous les symboles d'appel du premier argument, a n'importe quelle
      # profondeur : `ns(paste0("x_", i))` comme `paste0(ns("x"), i)`.
      desc <- a1; k <- 0
      repeat {
        nouv <- d$id[d$parent %in% desc]
        if (!length(setdiff(nouv, desc)) || k > 12) break
        desc <- unique(c(desc, nouv)); k <- k + 1
      }
      appels <- d$text[d$id %in% desc & d$token == "SYMBOL_FUNCTION_CALL"]
      if (!any(appels %in% aides))
        fautifs <- c(fautifs, sprintf("%s:%d %s", basename(f), w$line1[i], w$text[i]))
    }
  }
  expect_equal(fautifs, character(0))
})

test_that("un bouton déclaré est un bouton branché", {
  # « Actualiser le graphique », pleine largeur et en vert, n'etait relie a
  # AUCUN observateur : le geste le plus visible de l'onglet ne faisait rien.
  # Deux autres etaient dans le meme cas -- « Aperçu » du theme et
  # « Afficher quand même » au-dela de cinquante niveaux, qui promet une issue
  # que le clic ne donnait pas.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  orphelins <- character(0)
  for (f in list.files(file.path(root, "R"), "^mod_.*\\.R$", full.names = TRUE)) {
    src <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    d <- utils::getParseData(parse(f, keep.source = TRUE))
    b <- d[d$token == "SYMBOL_FUNCTION_CALL" & d$text %in% c("actionButton", "actionLink"), ]
    for (i in seq_len(nrow(b))) {
      appel <- d$parent[match(d$parent[match(b$id[i], d$id)], d$id)]
      args  <- d[d$parent == appel & d$token == "expr", ]
      if (nrow(args) < 2) next
      s <- d[d$parent %in% d$id[d$parent == args$id[2]] & d$token == "STR_CONST", ]
      if (!nrow(s)) next
      id <- gsub('"', "", s$text[1])
      if (!nzchar(id)) next
      if (!grepl(sprintf("input\\$%s\\b|input\\[\\[\"%s\"\\]\\]", id, id), src))
        orphelins <- c(orphelins, sprintf("%s:%d %s", basename(f), b$line1[i], id))
    }
  }
  expect_equal(orphelins, character(0))
})


test_that("une variable réponse en échec n'emporte pas les autres", {
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("DT")
  suppressMessages(hstat_installer_replis_ui())
  # UN `tryCatch` QUI COUVRE LA BOUCLE EMPORTE TOUTE LA SORTIE -- la regle est
  # ecrite dans ce dépôt, elle n'etait pas appliquee a la regression, au GLM ni
  # au modele mixte : le gestionnaire enveloppait les N variables reponses.
  d <- data.frame(Bonne = c(10, 11, 12, 18, 19, 20, 14, 15, 16),
                  Vide  = rep(NA_real_, 9),
                  X     = 1:9)
  for (bouton in c("testLM", "testGLM")) {
    vals <- shiny::reactiveValues(filteredData = d)
    shiny::testServer(mod_tests_server, args = list(values = vals), {
      session$setInputs(responseVar = c("Bonne", "Vide"), factorVar = "X")
      session$setInputs(!!bouton := 1)
      r <- values$testResultsDF
      expect_false(is.null(r), info = bouton)
      # La bonne variable sort ses resultats...
      expect_true("Bonne" %in% r$Variable, info = bouton)
      expect_true(any(is.finite(r$Statistique[r$Variable == "Bonne"])), info = bouton)
      # ...et celle qui echoue devient une LIGNE NOMMEE, jamais un vide.
      expect_true("Vide" %in% r$Variable, info = bouton)
      expect_true(all(nzchar(r$Interpretation[r$Variable == "Vide"])), info = bouton)
    })
  }
})

test_that("un prédicteur constant donne un verdict, pas une chute", {
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("DT")
  suppressMessages(hstat_installer_replis_ui())
  # LE CAS DEGENERE LE PLUS ORDINAIRE : une colonne constante. R ne garde alors
  # que l'ordonnee a l'origine -- `summary()$fstatistic` vaut NULL, et
  # `round(NULL[1], 4)` leve « non-numeric argument to mathematical function ».
  # Pire, `2:nrow(coef_table)` rend `c(2, 1)` a une seule ligne : l'indexation
  # sort du tableau. Les deux tombaient dans le meme `tryCatch` global.
  d <- data.frame(Y = c(10, 11, 12, 18, 19, 20, 14, 15, 16), Cst = rep(7, 9))
  vals <- shiny::reactiveValues(filteredData = d)
  shiny::testServer(mod_tests_server, args = list(values = vals), {
    session$setInputs(responseVar = "Y", factorVar = "Cst")
    session$setInputs(testLM = 1)
    r <- values$testResultsDF
    expect_false(is.null(r))
    expect_equal(nrow(r), 1L)
    # Le verdict NOMME la cause et dit quoi faire -- un NA nu ne se lit pas.
    expect_true(grepl("constant|colin", r$Interpretation[1]))
    expect_true(is.na(r$Statistique[1]))
  })

  # Et la sequence inversee ne revient pas : `seq_len(n)[-1]`, jamais `2:n`.
  #
  # LE BALAYAGE COLLE LES JETONS SANS ESPACE. `getParseData()` rend
  # « 2 », « : », « nrow », « ( »... : les joindre par un espace donne
  # « 2 : nrow ( coef_table ) », qu'aucun motif ecrit en R ne rencontre. La
  # premiere version de cette assertion ne pouvait donc PAS echouer -- une
  # mutation l'a montre, pas la relecture.
  chemin <- .hstat_module_path("mod_tests.R")
  skip_if_not(file.exists(chemin))
  pd <- utils::getParseData(parse(chemin, keep.source = TRUE))
  code <- paste(pd$text[pd$token != "COMMENT" & nzchar(pd$text)], collapse = "")
  expect_false(grepl("2:nrow(coef_table)", code, fixed = TRUE))
  expect_true(grepl("seq_len(nrow(coef_table))[-1]", code, fixed = TRUE))
})


test_that("un nom de traitement avec tiret ne fait pas fusionner les lettres CLD", {
  skip_if_not_installed("multcompView")
  # LE DEFAUT LE PLUS COUTEUX TROUVE JUSQU'ICI, parce qu'il est MUET et qu'il
  # sort un chiffre publiable. `TukeyHSD` nomme ses lignes « B-A » ; le code
  # decoupait sur le tiret. Avec « T-1 » / « T-2 », `strsplit("T-2-T-1", "-")`
  # rend quatre morceaux, la paire etait ecartee, et sa p-value restait a la
  # valeur d'initialisation -- 1, c'est-a-dire « pas de difference ».
  #
  # Mesure : trois groupes a 10, 20 et 30, Tukey a p = 4e-14 sur chaque paire.
  # Lettres obtenues « a | a | a ».
  set.seed(1)
  jeu <- function(niv) data.frame(
    y = c(stats::rnorm(8, 10), stats::rnorm(8, 20), stats::rnorm(8, 30)),
    g = rep(niv, each = 8), stringsAsFactors = FALSE)

  for (niv in list(c("A", "B", "C"), c("T-1", "T-2", "T-3"),
                   c("Rdt-2023", "Rdt-2024", "Rdt-2025"))) {
    r <- build_letters_per_variable(jeu(niv), "y", "g", parametric = TRUE)
    expect_false(is.null(r), info = paste(niv, collapse = ","))
    # TROIS groupes nettement separes : TROIS lettres distinctes.
    expect_equal(length(unique(r$Groupes)), 3L, info = paste(niv, collapse = ","))
  }

  # ET L'INVERSE, sinon l'assertion passerait sur un code qui rendrait
  # toujours des lettres differentes : deux groupes identiques la PARTAGENT.
  set.seed(2)
  meme <- data.frame(y = c(stats::rnorm(10, 10), stats::rnorm(10, 10),
                           stats::rnorm(10, 40)),
                     g = rep(c("T-1", "T-2", "T-3"), each = 10),
                     stringsAsFactors = FALSE)
  r2 <- build_letters_per_variable(meme, "y", "g", parametric = TRUE)
  expect_equal(r2$Groupes[r2$Niveau == "T-1"], r2$Groupes[r2$Niveau == "T-2"])
  expect_false(identical(r2$Groupes[r2$Niveau == "T-1"],
                         r2$Groupes[r2$Niveau == "T-3"]))
})

test_that("une étiquette de comparaison se rapproche des niveaux, elle ne se découpe pas", {
  n <- c("A", "B", "C")
  expect_equal(hstat_paire_niveaux("B-A", n), c("B", "A"))
  # `FSA::dunnTest` ecrit « A - B », avec des espaces.
  expect_equal(hstat_paire_niveaux("A - B", n), c("A", "B"))
  # Le tiret DANS le nom : c'est tout le sujet.
  h <- c("T-1", "T-2", "T-3")
  expect_equal(hstat_paire_niveaux("T-2-T-1", h), c("T-2", "T-1"))
  expect_equal(hstat_paire_niveaux("T-3 - T-1", h), c("T-3", "T-1"))
  # Une etiquette qui ne parle pas de ces niveaux ne rend rien -- on ne devine
  # pas, et le NULL est ce que l'appelant compte pour le dire.
  expect_null(hstat_paire_niveaux("X-Y", n))
  expect_null(hstat_paire_niveaux("A", n))
  expect_null(hstat_paire_niveaux("", n))
  # UNE SEULE COUPE VALIDE SUFFIT, meme quand un niveau porte un tiret :
  # avec « A », « B » et « A-B », l'etiquette « A-B-A » ne se coupe qu'en
  # (« A-B », « A ») -- (« A », « B-A ») echoue, « B-A » n'etant pas un niveau.
  expect_equal(hstat_paire_niveaux("A-B-A", c("A", "B", "A-B")), c("A-B", "A"))
  # AMBIGUITE REELLE : il faut que les DEUX coupes tombent sur des niveaux
  # connus. Avec « A-B » ET « B-A » au catalogue, « A-B-A » se lit des deux
  # façons -- aucune methode ne peut trancher, et deviner serait pire que
  # s'abstenir.
  expect_null(hstat_paire_niveaux("A-B-A", c("A", "B", "A-B", "B-A")))

  # La matrice : ce qui n'a pas pu etre rapproche est NOMME.
  m <- hstat_pmat_comparaisons(c("B-A", "C-A", "Z-Q"), c(0.01, 0.4, 0.02), n)
  expect_equal(m["B", "A"], 0.01)
  expect_equal(m["A", "B"], 0.01)
  expect_equal(m["C", "A"], 0.4)
  expect_equal(diag(m), stats::setNames(rep(1, 3), n))
  expect_equal(attr(m, "non_resolues"), "Z-Q")
  # Une paire jamais renseignee garde 1, donc « pas de difference » : c'est
  # justement pourquoi il faut la nommer.
  expect_equal(unname(m["B", "C"]), 1)
})


test_that("le coefficient de variation ne rend ni l'infini ni un signe négatif", {
  # DEUX DIVISIONS PAR ZERO, ET UN SIGNE QUI NE VEUT RIEN DIRE.
  # `sd / mean` rendait `Inf` sur une moyenne nulle -- une valeur d'apparence
  # normale dans une cellule de tableau -- et un CV NEGATIF sur des donnees de
  # moyenne negative. Un coefficient de variation negatif n'existe pas : c'est
  # une dispersion RELATIVE, prise en valeur absolue par convention.
  expect_equal(calc_cv(c(10, 12, 14, 16)), stats::sd(c(10, 12, 14, 16)) / 13 * 100)
  # Moyenne exactement nulle : indefini, donc NA -- jamais `Inf`.
  expect_true(is.na(calc_cv(c(-2, -1, 1, 2))))
  # Moyenne negative : meme dispersion relative que son oppose, et POSITIVE.
  expect_equal(calc_cv(c(-10, -12, -14, -16)), calc_cv(c(10, 12, 14, 16)))
  expect_gt(calc_cv(c(-10, -12, -14, -16)), 0)
  # Rien de calculable : NA, pas une chute.
  expect_true(is.na(calc_cv(c(NA_real_, NA_real_))))
  expect_true(is.na(calc_cv(numeric(0))))
  expect_true(is.na(calc_cv(5)))          # un seul point : sd vaut NA

  # UNE STATISTIQUE N'A QU'UNE DEFINITION. `mod_tests.R` en portait une copie
  # qui divergeait -- et qui levait « missing value where TRUE/FALSE needed »
  # sur une colonne vide, `sd(NA, na.rm = TRUE) == 0` valant NA.
  chemin <- .hstat_module_path("mod_tests.R")
  skip_if_not(file.exists(chemin))
  pd <- utils::getParseData(parse(chemin, keep.source = TRUE))
  code <- paste(pd$text[pd$token != "COMMENT" & nzchar(pd$text)], collapse = "")
  expect_false(grepl("calc_cv<-function", code, fixed = TRUE))
})

test_that("une matrice de corrélation refuse au lieu de lever", {
  # `sapply()` sur un tableau SANS COLONNE rend `list()`, et `df[, list()]`
  # leve « invalid subscript type 'list' ». Le cas s'atteint des que le
  # filtrage a tout retire -- et `safe_cor` alimente une sortie Shiny, ou une
  # erreur fait tomber le panneau entier.
  expect_null(safe_cor(data.frame()))
  expect_null(safe_cor(NULL))
  expect_null(safe_cor(data.frame(a = 1:5)))                    # une seule colonne
  expect_null(safe_cor(data.frame(a = letters[1:5], b = letters[1:5])))  # rien de numerique
  expect_null(safe_cor(data.frame(a = rep(1, 5), b = rep(2, 5))))        # variance nulle
  m <- safe_cor(data.frame(a = 1:5, b = c(2, 4, 6, 8, 10)))
  expect_equal(unname(m["a", "b"]), 1)
})


test_that("la corrélation cophénétique ne dépend pas de l'échelle des mesures", {
  # C'EST SA PROPRIETE ESSENTIELLE, et elle porte le chiffre de qualite d'une
  # CAH. `dist` change avec l'unite, `cor` non : mesurer en microgrammes ou en
  # tonnes doit rendre exactement le meme indice. Un jour ou l'on normaliserait
  # les coordonnees quelque part, seul ce test le verrait.
  set.seed(7)
  co <- rbind(matrix(stats::rnorm(40, 0), 20, 2), matrix(stats::rnorm(40, 8), 20, 2))
  a <- hstat_cophenetic_corr(co, stats::hclust(stats::dist(co), method = "ward.D2"))
  b2 <- hstat_cophenetic_corr(co * 1000,
          stats::hclust(stats::dist(co * 1000), method = "ward.D2"))
  expect_equal(a, b2)
  # Deux nuages nettement separes : l'arbre represente bien les distances.
  expect_gt(a, 0.9)

  # UN ARBRE QUI NE CORRESPOND PAS AUX COORDONNEES est recalcule, jamais
  # confronte tel quel : `cor(dist, cophenetic)` sur deux tailles differentes
  # rendrait un nombre sans rapport, ou leverait.
  petit <- stats::hclust(stats::dist(co[1:10, ]), method = "ward.D2")
  r <- hstat_cophenetic_corr(co, petit)
  expect_true(is.finite(r))
  expect_equal(r, a, tolerance = 1e-6)
})

test_that("la MANOVA refuse en nommant ce qui manque", {
  # ONZE SORTIES ANTICIPEES protegent l'appel a `manova()`, et aucune n'etait
  # testee. Toutes doivent NOMMER la cause : « Test impossible » se lit, un
  # `ok = FALSE` nu ne se lit pas.
  set.seed(11)
  d <- data.frame(y1 = stats::rnorm(30), y2 = stats::rnorm(30),
                  g = rep(c("A", "B", "C"), 10), cst = 1,
                  stringsAsFactors = FALSE)
  expect_true(check_manova_data(d, c("y1", "y2"), "g")$ok)

  refus <- function(r, motif) {
    expect_false(r$ok)
    expect_true(grepl(motif, r$message), info = r$message)
  }
  refus(check_manova_data(d, "y1", "g"), "2 variables")
  refus(check_manova_data(d, c("y1", "y2"), character(0)), "facteur")
  # La variable fautive est NOMMEE, pas seulement comptee.
  refus(check_manova_data(d, c("y1", "cst"), "g"), "cst")
  d2 <- d; d2$g <- "A"
  refus(check_manova_data(d2, c("y1", "y2"), "g"), "'g'")
  # n < p + 3 : le nombre reel est dit.
  refus(check_manova_data(d[1:4, ], c("y1", "y2"), "g"), "n=4")

  # Le tableau nettoye revient a l'appelant, facteurs deja convertis.
  ok <- check_manova_data(d, c("y1", "y2"), "g")
  expect_true(is.factor(ok$df_clean$g))
  expect_equal(ok$n, 30L)
  expect_equal(ok$p, 2L)
})


test_that("une graine posée dans une boucle doit varier avec la boucle", {
  # UN BOOTSTRAP DONT LES REPLICATS SONT IDENTIQUES NE MESURE PLUS RIEN.
  # `hstat_set_seed(input$globalSeed)` vivait DANS la boucle de stabilite du
  # clustering : les trente sous-echantillons etaient trente copies du meme
  # tirage. Mesure : un seul tirage distinct sur cinq. L'ecart-type des indices
  # de Rand valait donc 0 -- « parfaitement reproductible » -- et le verdict
  # affiche ne decrivait qu'un echantillon.
  #
  # La graine reste legitime DANS une boucle quand elle depend de l'indice :
  # c'est ainsi que `mod_design.R` rend une randomisation par bloc
  # reproductible. La regle est donc « elle varie », pas « elle est absente ».
  #
  # C'EST LA BOUCLE LA PLUS INTERIEURE QUI COMPTE. Une premiere version
  # s'arretait a la premiere boucle englobante trouvee -- l'exterieure -- et
  # signalait `mod_design.R`, dont la graine depend bien de l'indice INTERNE.
  # Une graine qui varie avec la boucle du dessus laisse malgre tout les
  # replicats de la boucle du dessous identiques.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    d <- utils::getParseData(parse(f, keep.source = TRUE))
    l <- readLines(f, warn = FALSE, encoding = "UTF-8")
    graines <- d[d$token == "SYMBOL_FUNCTION_CALL" &
                 d$text %in% c("set.seed", "hstat_set_seed"), ]
    boucles <- d[d$token %in% c("FOR", "WHILE"), ]
    for (i in seq_len(nrow(graines))) {
      # l'appel entier, pas la seule ligne du symbole
      ap <- d[d$id == d$parent[match(d$parent[match(graines$id[i], d$id)], d$id)], ]
      if (!nrow(ap)) next
      appel <- paste(l[ap$line1:ap$line2], collapse = " ")
      englobantes <- integer(0)
      for (j in seq_len(nrow(boucles))) {
        r <- d[d$id == d$parent[match(boucles$id[j], d$id)], ]
        if (nrow(r) && graines$line1[i] > r$line1 && graines$line1[i] <= r$line2)
          englobantes <- c(englobantes, r$line1)
      }
      if (!length(englobantes)) next
      interieure <- max(englobantes)          # la plus proche du `set.seed`
      vb <- sub("^.*for\\s*\\(\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s+in.*$", "\\1",
                l[interieure])
      if (identical(vb, l[interieure])) next  # `while` : pas de variable
      if (!grepl(sprintf("[^A-Za-z0-9_.]%s[^A-Za-z0-9_.]", vb), appel))
        fautifs <- c(fautifs, sprintf("%s:%d", basename(f), graines$line1[i]))
    }
  }
  expect_equal(unique(fautifs), character(0))
})


test_that("une PERMANOVA par paires refuse un groupe d'une seule observation", {
  skip_if_not_installed("vegan")
  set.seed(21)
  n <- 12
  Y <- rbind(matrix(stats::rnorm(2 * n, 0), n, 2),
             matrix(stats::rnorm(2 * n, 0), n, 2),
             matrix(stats::rnorm(2 * n, 6), n, 2))
  g <- rep(c("T-1", "T-2", "T-3"), each = n)

  r <- pairwise_permanova(Y, g, permutations = 199)
  expect_equal(nrow(r), 3L)
  # LES NIVEAUX REVIENNENT EN COLONNES, pas concatenes dans une etiquette :
  # c'est la forme qui rend le tiret inoffensif.
  expect_setequal(paste(r$Niveau1, r$Niveau2), c("T-1 T-2", "T-1 T-3", "T-2 T-3"))
  # Deux nuages confondus, un nuage a part : une paire non significative,
  # deux significatives. Verifier le VERDICT, pas seulement la presence.
  expect_equal(sum(r$Significatif == "Oui"), 2L)
  expect_equal(r$Significatif[r$Niveau1 == "T-1" & r$Niveau2 == "T-2"], "Non")
  # Bonferroni sur trois paires.
  expect_equal(r$p_adj, pmin(r$p_value * 3, 1))

  # UN GROUPE D'UNE SEULE OBSERVATION N'A PAS DE DISPERSION INTERNE. La garde
  # ne portait que sur l'effectif TOTAL : une paire 12 contre 1 la franchissait
  # et rendait un p parfaitement plausible -- mesure : p = 0,08, « Non
  # significatif ». La non-significativite y est un artefact d'effectif.
  idx <- c(1:12, 13, 25:36)
  r2 <- pairwise_permanova(Y[idx, ], g[idx], permutations = 99)
  petites <- r2$n1 < 2 | r2$n2 < 2
  expect_true(any(petites))
  expect_true(all(is.na(r2$p_value[petites])))
  expect_true(all(r2$Significatif[petites] == "NA"))
  # ...et la paire complete, elle, reste testee.
  expect_false(any(is.na(r2$p_value[!petites])))
})

test_that("les lettres d'interaction survivent aux tirets dans les deux facteurs", {
  skip_if_not_installed("multcompView")
  # Les cellules d'interaction sont composees (« A-1 . B-1 ») : l'etiquette de
  # Tukey y porte DEUX separateurs possibles. Rapprocher des niveaux connus
  # tranche, decouper ne le pouvait pas.
  set.seed(5)
  d <- expand.grid(A = c("A-1", "A-2"), B = c("B-1", "B-2"), r = 1:6)
  d$y <- stats::rnorm(nrow(d), 10) +
         ifelse(d$A == "A-2", 8, 0) + ifelse(d$B == "B-2", 5, 0)
  r <- build_letters_interaction(d, "y", c("A", "B"), parametric = TRUE)
  expect_false(is.null(r))
  expect_equal(nrow(r), 4L)
  # Quatre cellules nettement separees -> quatre lettres distinctes.
  expect_equal(length(unique(r$Groupes)), 4L)
})

test_that("le niveau de confiance et le sens du test atteignent cor.test", {
  # LE DEPOT A DEJA ETE MORDU PAR CETTE FAMILLE : le niveau annonce differait
  # du niveau calcule, et la phrase d'interpretation ecrivait 95 % sous des
  # bornes a 99 %. Ici les deux reglages sont des ARGUMENTS -- ils doivent
  # arriver jusqu'a `cor.test`, et rien ne le verifiait.
  set.seed(9)
  dd <- data.frame(a = 1:20, b = (1:20) * 2 + stats::rnorm(20, 0, 3))
  c95 <- hstat_correlation_tests(dd, c("a", "b"), conf.level = 0.95)
  c99 <- hstat_correlation_tests(dd, c("a", "b"), conf.level = 0.99)
  ref99 <- stats::cor.test(dd$a, dd$b, conf.level = 0.99)$conf.int
  # Le tableau est arrondi a l'affichage : on compare a cette precision-la.
  expect_equal(c99$IC_bas, ref99[1], tolerance = 1e-4)
  expect_equal(c99$IC_haut, ref99[2], tolerance = 1e-4)
  # ET L'INTERVALLE A 99 % EST PLUS LARGE : sans quoi l'assertion passerait
  # sur une fonction qui ignorerait le niveau et rendrait toujours 95 %.
  expect_lt(c99$IC_bas, c95$IC_bas)
  expect_gt(c99$IC_haut, c95$IC_haut)

  # Le sens du test aussi : unilateral = moitie du bilateral quand l'effet va
  # dans le sens demande.
  #
  # LA DONNEE D'ESSAI DOIT RENDRE LA DIFFERENCE MESURABLE. Sur la correlation
  # quasi parfaite ci-dessus, p vaut ~1e-12 : sa moitie en differe de 7e-13,
  # soit MOINS que toute tolerance raisonnable -- l'assertion passait alors
  # meme en ignorant `alternative`. Une correlation faible donne p ~ 0,3, et
  # la moitie s'en distingue.
  set.seed(31)
  faible <- data.frame(a = stats::rnorm(20), b = stats::rnorm(20))
  bi  <- hstat_correlation_tests(faible, c("a", "b"))
  uni <- hstat_correlation_tests(faible, c("a", "b"),
                                 alternative = if (bi$Coefficient > 0) "greater" else "less")
  expect_gt(bi$p_value, 0.05)          # p bien loin de zero, donc mesurable
  expect_equal(uni$p_value, bi$p_value / 2, tolerance = 1e-3)
})


test_that("un niveau écarté des effets simples est nommé, pas escamoté", {
  skip_if_not_installed("vegan")
  set.seed(17)
  d <- expand.grid(A = c("A-1", "A-2"), B = c("B-1", "B-2"), r = 1:8)
  d$y1 <- stats::rnorm(nrow(d), 10) + ifelse(d$A == "A-2" & d$B == "B-2", 9, 0)
  d$y2 <- stats::rnorm(nrow(d), 5)  + ifelse(d$A == "A-2" & d$B == "B-2", 7, 0)

  # Interaction vraie : l'effet de B n'existe que dans A-2.
  r <- manova_simple_effects(d, c("y1", "y2"), fixed = "A", tested = "B")
  expect_equal(nrow(r), 2L)
  expect_equal(r$Significatif, c("Non", "Oui"))
  expect_null(attr(r, "niveaux_ecartes"))

  # UN NIVEAU NON CALCULABLE DISPARAISSAIT SANS UN MOT. Le motif n'etait
  # attache que si TOUS echouaient ; quand un seul tombait, l'utilisateur
  # lisait des effets simples pour un niveau sur deux en croyant les avoir
  # tous. Et la correction de Bonferroni porte sur les tests RESTANTS : un
  # niveau escamote rend les p survivantes MOINS corrigees, donc plus
  # facilement significatives.
  d2 <- d; d2$y1[d2$A == "A-1"] <- 3; d2$y2[d2$A == "A-1"] <- 3
  r2 <- manova_simple_effects(d2, c("y1", "y2"), fixed = "A", tested = "B")
  expect_equal(nrow(r2), 1L)
  expect_equal(attr(r2, "niveaux_ecartes"), "A-1")
  expect_true(grepl("A-1", attr(r2, "message"), fixed = TRUE))
  expect_true(grepl("Bonferroni", attr(r2, "message"), fixed = TRUE))
  # La preuve du cout : la meme p-value est MOINS corrigee sans le niveau.
  expect_lt(r2$p_adj[1], r$p_adj[r$Niveau_fixe == "A = A-2"])

  # Meme regle pour la version non parametrique, qui ne collectait meme pas
  # les niveaux ecartes.
  d3 <- d
  d3$B[d3$A == "A-1"] <- "B-1"           # un seul niveau teste dans A-1
  r3 <- permanova_simple_effects(d3, c("y1", "y2"), fixed = "A", tested = "B",
                                 permutations = 99)
  expect_equal(attr(r3, "niveaux_ecartes"), "A-1")
  expect_true(grepl("A-1", attr(r3, "message"), fixed = TRUE))

  # ET LE MESSAGE EST AFFICHE : une alerte que personne ne lit ne vaut rien.
  chemin <- .hstat_module_path("mod_tests.R")
  skip_if_not(file.exists(chemin))
  pd <- utils::getParseData(parse(chemin, keep.source = TRUE))
  code <- paste(pd$text[pd$token != "COMMENT" & nzchar(pd$text)], collapse = "")
  expect_true(grepl('attr(res,"message")', code, fixed = TRUE))
  expect_true(grepl("showNotification(msg_ecartes", code, fixed = TRUE))
})


test_that("sans différence globale, les lettres ne classent pas les modalités", {
  # SIGNALE A L'USAGE : l'ANOVA rend p > 0,05 -- « aucune différence » -- et le
  # post-hoc classait pourtant les modalités en a, ab, b, c, bc... Les deux
  # affirmations se contredisaient dans le même rapport.
  #
  # Mesure sur 300 jeux SANS effet réel dont l'ANOVA est non significative :
  # les comparaisons NON AJUSTEES (le LSD de Fisher) séparent quand même dans
  # 30 % des cas, Tukey dans 0 %. LSD et Duncan sont protégés par construction ;
  # les autres contrôlent le risque eux-mêmes, et leur désaccord est légitime.
  set.seed(5)
  d <- data.frame(Traitement = rep(paste0("T", 1:6), each = 5),
                  Rendement = stats::rnorm(30, 50, 10), stringsAsFactors = FALSE)
  o <- hstat_omnibus(d, "Rendement", "Traitement", parametric = TRUE)
  expect_equal(o$test, "ANOVA")
  expect_gt(o$p, 0.05)                       # non significatif, par construction
  # Le chiffre est celui de R, pas une approximation maison.
  expect_equal(o$p, summary(stats::aov(Rendement ~ factor(Traitement), d))[[1]][["Pr(>F)"]][1])

  k <- hstat_omnibus(d, "Rendement", "Traitement", parametric = FALSE)
  expect_equal(k$test, "Kruskal-Wallis")
  expect_equal(k$p, stats::kruskal.test(Rendement ~ factor(Traitement), d)$p.value)

  # Les cas dégénérés refusent en NOMMANT la cause, ils ne lèvent pas.
  vide <- hstat_omnibus(data.frame(y = 1:5, g = "A"), "y", "g")
  expect_true(is.na(vide$p))
  expect_true(grepl("deux groupes", vide$message))
  expect_true(is.na(hstat_omnibus(d, "absente", "Traitement")$p))
  expect_true(is.na(hstat_omnibus(data.frame(), "y", "g")$p))

  # LA PROTECTION : sans différence globale, une seule lettre pour tous.
  l <- c(A = "a", B = "b", C = "c")
  ns <- hstat_cld_proteger(l, 0.30, test = "ANOVA")
  expect_equal(as.character(ns), rep("a", 3))
  expect_true(attr(ns, "protege"))
  expect_true(grepl("0.3", attr(ns, "message"), fixed = TRUE))
  # ...et avec différence globale, les lettres passent intactes.
  s <- hstat_cld_proteger(l, 0.01, test = "ANOVA")
  expect_equal(as.character(s), c("a", "b", "c"))
  expect_false(attr(s, "protege"))
  # `NA` N'EST PAS « NON SIGNIFICATIF ». Un test global incalculable ne prouve
  # rien : on laisse les lettres et on le dit.
  na <- hstat_cld_proteger(l, NA_real_, test = "ANOVA")
  expect_equal(as.character(na), c("a", "b", "c"))
  expect_false(attr(na, "protege"))
  expect_true(grepl("non calculable", attr(na, "message")))
})

test_that("le post-hoc Bonferroni produit des lettres au lieu de lever", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("multcompView")
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("DT")
  suppressMessages(hstat_installer_replis_ui())
  # IL N'A JAMAIS FONCTIONNE. `summary(pairs(emm))$p.value` est un VECTEUR --
  # une p-value par contraste --, pas une matrice. `as.matrix()` en faisait un
  # tableau n x 1 sans noms : la garde `is.null(dim(pmat))` ne se declenchait
  # jamais, `diag(pmat) <- 1` ecrasait la premiere case, et `multcompLetters`
  # levait « Names required for pmat ». Reproduit au navigateur.
  set.seed(5)
  d <- data.frame(Traitement = rep(paste0("T", 1:6), each = 5),
                  Rendement = stats::rnorm(30, 50, 10), stringsAsFactors = FALSE)

  lettres <- function(protege) {
    v <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d)
    out <- NULL
    shiny::testServer(mod_tests_server, args = list(values = v), {
      vider <- function() try(session$flushReact(), silent = TRUE)
      session$setInputs(multiResponse = "Rendement", multiFactor = "Traitement",
                        testType = "param", multiTest = "bonferroni",
                        multiParamAdjust = "none", multiProtege = protege,
                        posthocInteraction = FALSE, multiRoundResults = TRUE,
                        multiDecimals = 2); vider()
      session$setInputs(runMultiple = 1); vider()
      out <<- values$multiResultsMain
    })
    out
  }

  libre <- lettres(FALSE)
  expect_false(is.null(libre))               # avant : NULL, l'analyse levait
  expect_equal(nrow(libre), 6L)
  # Non protégé, les comparaisons non ajustées séparent : c'est le défaut signalé.
  expect_gt(length(unique(libre$groups)), 1L)
  # LE TEST GLOBAL VOYAGE AVEC LES LETTRES : le lire dans un autre onglet est
  # ce qui rendait la contradiction invisible.
  expect_equal(libre$Test_global[1], "ANOVA")
  expect_gt(libre$p_global[1], 0.05)

  protege <- lettres(TRUE)
  expect_false(is.null(protege))
  expect_equal(length(unique(protege$groups)), 1L)
})

test_that("Games-Howell est jugé sur Welch, pas sur l'ANOVA classique", {
  # UN POST-HOC SE JUGE SUR SON PROPRE TEST GLOBAL.
  #
  # Games-Howell ne suppose PAS les variances égales : sa référence est
  # l'ANOVA de Welch. Les confondre n'est pas un détail théorique — sur ce jeu
  # (cinq groupes à sigma = 1, un à sigma = 20), l'ANOVA classique rend
  # p = 0,2452 et Welch p = 0,0001. Protéger les lettres de Games-Howell avec
  # l'ANOVA classique les aplatirait toutes sur « a » et EFFACERAIT un
  # résultat réel — l'inverse exact de ce que la protection existe pour faire.
  jeu <- function() {
    set.seed(8)
    ecarts <- c(1, 1, 1, 1, 1, 20)
    y <- unlist(lapply(seq_len(6), function(i)
      stats::rnorm(5, 50 + c(0, 1, 2, 3, 4, 25)[i], ecarts[i])))
    data.frame(Traitement = rep(paste0("T", seq_len(6)), each = 5),
               Rendement = y, stringsAsFactors = FALSE)
  }
  d <- jeu()

  # LA REGLE EST DECLAREE UNE FOIS, les deux sites de post-hoc la lisent.
  expect_equal(hstat_omnibus_variances("games"), "inegales")
  expect_equal(hstat_omnibus_variances("tukey"), "egales")
  expect_equal(hstat_omnibus_variances("dunn"), "egales")
  expect_equal(hstat_omnibus_variances(NA_character_), "egales")
  expect_equal(hstat_omnibus_variances(NULL), "egales")
  expect_equal(hstat_omnibus_variances(character(0)), "egales")

  classique <- hstat_omnibus(d, "Rendement", "Traitement", parametric = TRUE)
  welch <- hstat_omnibus(d, "Rendement", "Traitement", parametric = TRUE,
                         variances = "inegales")
  expect_equal(classique$test, "ANOVA")
  expect_equal(welch$test, "ANOVA de Welch")
  # Le chiffre est celui de R, pas une approximation maison.
  expect_equal(welch$p,
               stats::oneway.test(Rendement ~ factor(Traitement), d,
                                  var.equal = FALSE)$p.value)
  # LES DEUX REFERENCES DOIVENT ETRE DISCERNABLES SUR CE JEU, sinon
  # l'assertion suivante ne prouverait rien : elle passerait avec ou sans le
  # correctif. C'est la leçon des métriques de modélisation.
  expect_gt(classique$p, 0.05)
  expect_lt(welch$p, 0.05)

  # LE VERDICT SUIT LA REFERENCE. Avec Welch significatif, la protection ne
  # doit PAS s'appliquer ; avec l'ANOVA classique, elle s'appliquerait.
  l <- c(T1 = "a", T2 = "ab", T3 = "bc", T4 = "cd", T5 = "d", T6 = "abcd")
  garde <- hstat_cld_proteger(l, welch$p, test = welch$test)
  expect_false(isTRUE(attr(garde, "protege")))
  expect_equal(as.character(garde), as.character(l))
  efface <- hstat_cld_proteger(l, classique$p, test = classique$test)
  expect_true(attr(efface, "protege"))       # le défaut, si la référence est fausse
})

test_that("les lettres agricolae et Games-Howell suivent la protection dans le module", {
  skip_if_not_installed("agricolae")
  skip_if_not_installed("PMCMRplus")
  skip_if_not_installed("multcompView")
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("DT")
  suppressMessages(hstat_installer_replis_ui())
  # LE CHEMIN REEL, pas seulement les fonctions du socle. Quatre méthodes
  # agricolae sur sept séparent sous une ANOVA non significative — LSD,
  # Duncan, REGW et Waller-Duncan — et c'est ce que l'utilisateur a signalé.
  # Mesuré avec agricolae 1.3-7.
  lettres <- function(d, methode, protege) {
    v <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d)
    out <- NULL
    shiny::testServer(mod_tests_server, args = list(values = v), {
      vider <- function() try(session$flushReact(), silent = TRUE)
      session$setInputs(multiResponse = "Rendement", multiFactor = "Traitement",
                        testType = "param", multiTest = methode,
                        multiParamAdjust = "none", multiProtege = protege,
                        posthocInteraction = FALSE, multiRoundResults = TRUE,
                        multiDecimals = 2); vider()
      session$setInputs(runMultiple = 1); vider()
      out <<- values$multiResultsMain
    })
    out
  }

  set.seed(5)
  homo <- data.frame(Traitement = rep(paste0("T", seq_len(6)), each = 5),
                     Rendement = stats::rnorm(30, 50, 10),
                     stringsAsFactors = FALSE)
  # ANOVA non significative : REGW sépare quand même, la protection l'aplatit.
  libre <- lettres(homo, "regw", FALSE)
  expect_false(is.null(libre))
  expect_gt(length(unique(libre$groups)), 1L)
  expect_gt(libre$p_global[1], 0.05)
  garde <- lettres(homo, "regw", TRUE)
  expect_equal(length(unique(garde$groups)), 1L)

  # GAMES-HOWELL SUR VARIANCES INEGALES : Welch est significatif, la
  # protection ne doit RIEN aplatir. Avec l'ANOVA classique en référence,
  # `garde_gh` n'aurait qu'une seule lettre — le résultat effacé.
  set.seed(8)
  ecarts <- c(1, 1, 1, 1, 1, 20)
  y <- unlist(lapply(seq_len(6), function(i)
    stats::rnorm(5, 50 + c(0, 1, 2, 3, 4, 25)[i], ecarts[i])))
  hetero <- data.frame(Traitement = rep(paste0("T", seq_len(6)), each = 5),
                       Rendement = y, stringsAsFactors = FALSE)
  garde_gh <- lettres(hetero, "games", TRUE)
  expect_false(is.null(garde_gh))
  expect_gt(length(unique(garde_gh$groups)), 1L)
  expect_equal(garde_gh$Test_global[1], "ANOVA de Welch")
  expect_lt(garde_gh$p_global[1], 0.05)
  # ...et l'ANOVA classique, elle, ne l'est pas : les deux se distinguent bien
  # sur ce jeu, sans quoi l'assertion ci-dessus passerait sans le correctif.
  expect_gt(summary(stats::aov(Rendement ~ factor(Traitement),
                               hetero))[[1]][["Pr(>F)"]][1], 0.05)
})

test_that("les effets simples sont protégés par LEUR propre test global", {
  skip_if_not_installed("agricolae")
  skip_if_not_installed("multcompView")
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("DT")
  suppressMessages(hstat_installer_replis_ui())
  # `perform_simple_effect_posthoc()` construisait ses lettres sans regarder
  # aucun test global : la protection posée sur les comparaisons multiples ne
  # l'atteignait pas — autre fonction, autre chemin.
  #
  # Le test global est ici celui de l'EFFET SIMPLE, calculé sur le
  # sous-tableau : c'est ce sous-groupe-là qui est comparé, pas le tableau
  # entier. Jeu construit pour que les deux diffèrent — interaction
  # p = 1,4e-06 (elle déclenche la décomposition), et dans F2 = « A »
  # l'ANOVA de F1 rend p = 0,2297 alors que le LSD y sépare en « a ab ab b ».
  jeu <- function() {
    set.seed(83)
    dA <- data.frame(F1 = rep(paste0("T", seq_len(4)), each = 4), F2 = "A",
                     y = stats::rnorm(16, 50, 6), stringsAsFactors = FALSE)
    dB <- data.frame(F1 = rep(paste0("T", seq_len(4)), each = 4), F2 = "B",
                     y = stats::rnorm(16, rep(c(30, 50, 70, 90), each = 4), 6),
                     stringsAsFactors = FALSE)
    rbind(dA, dB)
  }
  d <- jeu()
  sous <- d[d$F2 == "A", ]
  # LES DEUX REFERENCES SONT DISCERNABLES SUR CE JEU, sinon l'assertion
  # finale passerait avec ou sans le correctif.
  expect_lt(summary(stats::aov(y ~ factor(F1) * factor(F2), d))[[1]][["Pr(>F)"]][3], 0.05)
  expect_gt(summary(stats::aov(y ~ factor(F1), sous))[[1]][["Pr(>F)"]][1], 0.05)

  # ET C'EST CE TEST QUI A TROUVE POURQUOI L'ONGLET RESTAIT VIDE.
  # `summary(aov)` REMPLIT SES NOMS DE LIGNE D'ESPACES — « F1:F2      » —, si
  # bien que `"F1:F2" %in% rownames(...)` est TOUJOURS faux. La p-value
  # d'interaction restait `NA`, et la décomposition bidirectionnelle ne s'est
  # jamais déclenchée, même à p = 1e-06. Aucune erreur, aucun message : juste
  # un onglet « Effets simples » perpétuellement vide.
  ar <- summary(stats::aov(y ~ F1 * F2,
                           transform(d, F1 = factor(F1), F2 = factor(F2))))[[1]]
  expect_false("F1:F2" %in% rownames(ar))            # le piège
  expect_true("F1:F2" %in% trimws(rownames(ar)))     # le remède

  effets <- function(protege) {
    v <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d)
    out <- NULL
    shiny::testServer(mod_tests_server, args = list(values = v), {
      vider <- function() try(session$flushReact(), silent = TRUE)
      session$setInputs(multiResponse = "y", multiFactor = c("F1", "F2"),
                        testType = "param", multiTest = "lsd",
                        multiParamAdjust = "none", multiProtege = protege,
                        posthocInteraction = TRUE, multiRoundResults = TRUE,
                        multiDecimals = 2); vider()
      session$setInputs(runMultiple = 1); vider()
      out <<- values$multiResultsMain
    })
    if (is.null(out)) return(NULL)
    out[!is.na(out$Type) & out$Type == "simple_effect" &
          grepl("F2=A", out$Facteur, fixed = TRUE), ]
  }

  libre <- effets(FALSE)
  expect_false(is.null(libre))
  expect_gt(nrow(libre), 0L)
  expect_gt(length(unique(libre$groups)), 1L)   # le défaut : il sépare

  garde <- effets(TRUE)
  expect_gt(nrow(garde), 0L)
  expect_equal(length(unique(garde$groups)), 1L)
})

test_that("les deux tests de Welch existent, et ils ne sont pas Student", {
  # WELCH NE SUPPOSE PAS LES VARIANCES EGALES. Il n'existe QUE deux tests
  # parametriques qui portent son nom -- le t a deux echantillons et l'ANOVA a
  # un facteur -- et la liste est close : sans second groupe il n'y a rien a
  # mettre en commun, donc pas de « t de Welch a un echantillon ».
  expect_setequal(HSTAT_TESTS_WELCH, c("t_welch", "anova_welch"))

  # LE JEU D'ESSAI DOIT RENDRE LES DEUX FORMULES DISCERNABLES : sur des
  # variances egales, Welch et Student donnent presque le meme p et
  # l'assertion se viderait de son sens. On prend donc des variances tres
  # inegales, ET on verifie qu'elles se distinguent.
  set.seed(11)
  d <- data.frame(
    g = rep(c("A", "B"), each = 12),
    y = c(stats::rnorm(12, 10, 1), stats::rnorm(12, 12, 8)),
    stringsAsFactors = FALSE)

  w <- hstat_t_deux(d, "y", "g", var_equal = FALSE)
  st <- hstat_t_deux(d, "y", "g", var_equal = TRUE)
  expect_equal(w$test, "Test t de Welch")
  expect_equal(st$test, "Test t de Student")
  # Les chiffres sont ceux de R, pas une approximation maison.
  expect_equal(w$p,  stats::t.test(y ~ g, d, var.equal = FALSE)$p.value)
  expect_equal(st$p, stats::t.test(y ~ g, d, var.equal = TRUE)$p.value)
  expect_false(isTRUE(all.equal(w$p, st$p)))

  # LE DDL DE WELCH EST FRACTIONNAIRE, c'est la signature de la methode ;
  # celui de Student vaut n - 2, un entier. L'arrondir ferait passer l'un
  # pour l'autre.
  expect_false(isTRUE(all.equal(w$ddl, round(w$ddl))))
  expect_equal(st$ddl, 22)
  expect_equal(hstat_ddl_fmt(st$ddl), "22")           # 22, pas 22.00
  expect_true(grepl("[.]", hstat_ddl_fmt(w$ddl)))     # la fraction survit

  # ET LA FRACTION DOIT SE VOIR, pas seulement le point décimal.
  # L'ANOVA de Welch rend ici ddl2 = 11,00449737 : arrondi à deux décimales il
  # s'écrit « 11.00 » et se lit comme le 11 entier de Fisher — or c'est
  # précisément ce qui sépare les deux tests. Vérifier la présence d'un point
  # ne suffit donc pas : « 11.00 » en porte un. C'est la valeur qu'on contrôle.
  expect_equal(hstat_ddl_fmt(11.00449737), "11.004")
  expect_false(identical(hstat_ddl_fmt(11.00449737), "11.00"))
  expect_equal(hstat_ddl_fmt(11.72), "11.72")         # deux décimales suffisent
  expect_equal(hstat_ddl_fmt(5), "5")
  expect_true(is.na(hstat_ddl_fmt(NA_real_)))

  # L'ANOVA DE WELCH est la reference des methodes a variances inegales.
  set.seed(8)
  ecarts <- c(1, 1, 1, 1, 1, 20)
  y <- unlist(lapply(seq_len(6), function(i)
    stats::rnorm(5, 50 + c(0, 1, 2, 3, 4, 25)[i], ecarts[i])))
  h <- data.frame(Traitement = rep(paste0("T", seq_len(6)), each = 5),
                  Rendement = y, stringsAsFactors = FALSE)
  aw <- hstat_anova_welch(h, "Rendement", "Traitement")
  expect_equal(aw$test, "ANOVA de Welch")
  expect_equal(aw$p, stats::oneway.test(Rendement ~ factor(Traitement), h,
                                        var.equal = FALSE)$p.value)
  # ...et c'est bien celle que la protection post-hoc lit pour Games-Howell.
  expect_equal(aw$p, hstat_omnibus(h, "Rendement", "Traitement",
                                   parametric = TRUE, variances = "inegales")$p)
  expect_false(isTRUE(all.equal(aw$ddl2, round(aw$ddl2))))

  # LES CAS DEGENERES REFUSENT EN NOMMANT LA CAUSE, ils ne lèvent pas.
  trois <- data.frame(g = rep(c("A", "B", "C"), each = 4),
                      y = stats::rnorm(12), stringsAsFactors = FALSE)
  r3 <- hstat_t_deux(trois, "y", "g")
  expect_true(is.na(r3$p))
  expect_true(grepl("exactement 2|ANOVA", r3$message))
  # UN GROUPE A UNE SEULE OBSERVATION N'A PAS DE VARIANCE -- or c'est
  # justement la variance PAR GROUPE que Welch estime separement.
  seul <- data.frame(g = c("A", "A", "A", "B"), y = c(1, 2, 3, 4),
                     stringsAsFactors = FALSE)
  r1 <- hstat_t_deux(seul, "y", "g")
  expect_true(is.na(r1$p))
  expect_true(grepl("deux observations", r1$message))
  expect_true(grepl("B", r1$message))              # le groupe fautif est nommé
  expect_true(is.na(hstat_anova_welch(seul, "y", "g")$p))
  expect_true(is.na(hstat_t_deux(data.frame(), "y", "g")$p))
})

test_that("le module offre les deux t et l'ANOVA de Welch, et oriente le post-hoc", {
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("DT")
  skip_if_not_installed("car")
  suppressMessages(hstat_installer_replis_ui())
  # LE DEFAUT CORRIGE : `stats::t.test()` est Welch PAR DEFAUT, et le bouton
  # s'appelait « Test t de Student ». L'application executait donc Welch sous
  # le nom de Student, et Student n'etait offert nulle part.
  set.seed(11)
  d <- data.frame(
    g = rep(c("A", "B"), each = 12),
    y = c(stats::rnorm(12, 10, 1), stats::rnorm(12, 12, 8)),
    stringsAsFactors = FALSE)

  lance <- function(bouton) {
    v <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d)
    out <- NULL
    shiny::testServer(mod_tests_server, args = list(values = v), {
      vider <- function() try(session$flushReact(), silent = TRUE)
      session$setInputs(responseVar = "y", factorVar = "g", interaction = FALSE)
      vider()
      session$setInputs(!!bouton := 1); vider()
      out <<- list(res = values$testResultsDF,
                   var = values$currentTestVariances)
    })
    out
  }

  w  <- lance("testT")
  st <- lance("testTStudent")
  expect_equal(w$res$Test[1],  "Test t de Welch")
  expect_equal(st$res$Test[1], "Test t de Student")
  # LES DEUX NE RENDENT PAS LE MEME CHIFFRE sur variances inégales — sans quoi
  # l'ajout serait cosmétique.
  expect_false(isTRUE(all.equal(w$res$p_value[1], st$res$p_value[1])))
  expect_equal(w$res$p_value[1],  stats::t.test(y ~ g, d, var.equal = FALSE)$p.value)
  expect_equal(st$res$p_value[1], stats::t.test(y ~ g, d, var.equal = TRUE)$p.value)
  # L'HYPOTHESE DE VARIANCE VOYAGE AVEC LE RESULTAT : c'est elle qui oriente
  # le post-hoc vers Games-Howell.
  expect_equal(w$var,  "inegales")
  expect_equal(st$var, "egales")
  # Le ddl fractionnaire de Welch survit à l'affichage.
  expect_true(grepl("[.]", as.character(w$res$ddl[1])))
  expect_false(grepl("[.]", as.character(st$res$ddl[1])))

  # ANOVA DE WELCH sur plus de deux groupes.
  set.seed(8)
  ecarts <- c(1, 1, 1, 1, 1, 20)
  yy <- unlist(lapply(seq_len(6), function(i)
    stats::rnorm(5, 50 + c(0, 1, 2, 3, 4, 25)[i], ecarts[i])))
  h <- data.frame(Traitement = rep(paste0("T", seq_len(6)), each = 5),
                  Rendement = yy, stringsAsFactors = FALSE)
  v <- shiny::reactiveValues(data = h, cleanData = h, filteredData = h)
  out <- NULL
  shiny::testServer(mod_tests_server, args = list(values = v), {
    vider <- function() try(session$flushReact(), silent = TRUE)
    session$setInputs(responseVar = "Rendement", factorVar = "Traitement",
                      interaction = FALSE); vider()
    session$setInputs(testANOVAWelch = 1); vider()
    out <<- list(res = values$testResultsDF, var = values$currentTestVariances)
  })
  expect_equal(out$res$Test[1], "ANOVA de Welch")
  expect_equal(out$res$p_value[1],
               stats::oneway.test(Rendement ~ factor(Traitement), h,
                                  var.equal = FALSE)$p.value)
  expect_equal(out$var, "inegales")
  # L'ANOVA DE FISHER DECLARE LA SIENNE, sinon le post-hoc lirait celle du
  # test precedent -- et sur ce jeu les deux ne concluent pas pareil.
  v2 <- shiny::reactiveValues(data = h, cleanData = h, filteredData = h)
  vf <- NULL
  shiny::testServer(mod_tests_server, args = list(values = v2), {
    vider <- function() try(session$flushReact(), silent = TRUE)
    session$setInputs(responseVar = "Rendement", factorVar = "Traitement",
                      interaction = FALSE); vider()
    session$setInputs(testANOVA = 1); vider()
    vf <<- values$currentTestVariances
  })
  expect_equal(vf, "egales")
})

test_that("un fichier de deux colonnes suffit au rendement global, moyen et cumulé", {
  # LE CAS SIGNALE : un fichier ne portant que le facteur et le rendement, cinq
  # modalités répétées dix fois. Ni masse, ni surface. L'utilisateur veut le
  # rendement global, le rendement moyen et le gain de chaque modalité.
  set.seed(1)
  d <- data.frame(
    Facteur = rep(paste0("T", seq_len(5)), each = 10),
    Rendement = round(stats::rnorm(50, rep(c(3, 3.6, 4.1, 3.9, 4.4), each = 10), .35), 2),
    stringsAsFactors = FALSE)

  r <- hstat_rdt_complet(d, "Facteur", NULL, NULL, non_traite = "T1",
                         var_rendement = "Rendement", rdt_surfaces_egales = TRUE)
  expect_equal(nrow(r), 5L)
  expect_true(all(c("Rendement_global", "Rendement_moyen", "Rendement_somme",
                    "Gain_global", "Gain_moyen", "Gain_somme") %in% names(r)))
  expect_true(all(r$N == 10L))
  # A SURFACES EGALES, LE GLOBAL VAUT EXACTEMENT LA MOYENNE. Ce n'est pas une
  # approximation : masse totale / surface totale, avec des surfaces
  # identiques, EST la moyenne des rendements. On l'écrit parce que c'est la
  # question posée, et le message dit l'égalité et sa condition.
  expect_equal(r$Rendement_global, r$Rendement_moyen)
  expect_equal(r$Gain_global, r$Gain_moyen)
  expect_match(attr(r, "message"), "[Ss]urfaces déclarées égales")
  expect_equal(attr(r, "global"), "surfaces_egales")
  # Le témoin vaut zéro de gain par définition, les autres sont calculés.
  expect_equal(r$Gain_moyen[r$Modalite == "T1"], 0)
  expect_true(all(is.finite(r$Gain_moyen)))

  # AVEC DES SURFACES, LE GLOBAL EST PONDERE -- et il DIFFERE de la moyenne,
  # sans quoi l'assertion précédente passerait aussi bien sur un code qui
  # ignorerait la pondération.
  p <- data.frame(Facteur = c("A", "A", "B", "B"), Rdt = c(10, 20, 10, 20),
                  S = c(1, 9, 5, 5), stringsAsFactors = FALSE)
  w <- hstat_rdt_complet(p, "Facteur", NULL, NULL, non_traite = "A",
                         var_rendement = "Rdt", rdt_var_surface = "S")
  expect_equal(w$Rendement_global[w$Modalite == "A"], (10 * 1 + 20 * 9) / 10)
  expect_equal(w$Rendement_global[w$Modalite == "B"], 15)
  expect_equal(w$Rendement_moyen, c(15, 15))
  expect_false(isTRUE(all.equal(w$Rendement_global, w$Rendement_moyen)))
  expect_equal(attr(w, "global"), "pondere")

  # SANS SURFACE NI DECLARATION, ON NE DEVINE PAS. Poser la moyenne sous une
  # étiquette qui promet une pondération par la surface serait exactement le
  # défaut que ce module existe pour éviter.
  q <- hstat_rdt_complet(p, "Facteur", NULL, NULL, non_traite = "A",
                         var_rendement = "Rdt")
  expect_false("Rendement_global" %in% names(q))
  expect_false("Gain_global" %in% names(q))
  expect_equal(nrow(q), 2L)                  # le tableau reste entier
  expect_true(all(is.finite(q$Rendement_moyen)))
  expect_equal(attr(q, "global"), "aucun")

  # Une colonne de surface introuvable ou inexploitable REFUSE en nommant la
  # cause, elle ne rend pas un tableau silencieusement faux.
  ko <- hstat_rdt_complet(p, "Facteur", NULL, NULL, non_traite = "A",
                          var_rendement = "Rdt", rdt_var_surface = "absente")
  expect_equal(nrow(ko), 0L)
  expect_match(attr(ko, "message"), "introuvable")
  p0 <- p; p0$S <- 0
  z <- hstat_rdt_complet(p0, "Facteur", NULL, NULL, non_traite = "A",
                         var_rendement = "Rdt", rdt_var_surface = "S")
  expect_equal(nrow(z), 0L)
  expect_match(attr(z, "message"), "strictement positive")
})

test_that("une demi-matrice PMCMRplus porte quand même toutes les modalités", {
  skip_if_not_installed("PMCMRplus")
  skip_if_not_installed("multcompView")
  # LE DEFAUT : `gamesHowellTest`, `kwAllPairsDunnTest`, `kwAllPairsConoverTest`
  # et `kwAllPairsNemenyiTest` rendent une matrice (k-1) x (k-1) dont les LIGNES
  # sont les niveaux 2..k et les COLONNES 1..(k-1). Le PREMIER niveau n'y figure
  # jamais en ligne.
  #
  # L'idiome employe -- `pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]` -- travaille
  # sur cette forme sans la completer : les dimensions concordent, rien ne leve,
  # et `multcompLetters` ne voit que k-1 niveaux. La premiere modalite ressortait
  # SANS LETTRE (`NA` apres la fusion), et les lettres des autres etaient fausses.
  set.seed(8)
  ecarts <- c(1, 1, 1, 1, 1, 20)
  y <- unlist(lapply(seq_len(6), function(i)
    stats::rnorm(5, 50 + c(0, 1, 2, 3, 4, 25)[i], ecarts[i])))
  h <- data.frame(Traitement = factor(rep(paste0("T", seq_len(6)), each = 5)),
                  Rendement = y)
  niv <- levels(h$Traitement)
  brut <- suppressWarnings(
    PMCMRplus::gamesHowellTest(Rendement ~ Traitement, data = h))$p.value

  # LE PIEGE, epingle : la forme rendue n'est pas carree sur les niveaux.
  expect_equal(dim(brut), c(5L, 5L))
  expect_false(niv[1] %in% rownames(brut))
  expect_false(niv[length(niv)] %in% colnames(brut))

  # LE REMEDE : une matrice pleine sur les niveaux CONNUS.
  m <- hstat_pmat_demi(brut, niv)
  expect_equal(dim(m), c(6L, 6L))
  expect_equal(rownames(m), niv)
  expect_equal(m, t(m))                      # symétrique
  expect_true(all(diag(m) == 1))
  # Les p-values rendues par le paquet y sont, aux deux positions.
  expect_equal(m["T2", "T1"], brut["T2", "T1"])
  expect_equal(m["T1", "T2"], brut["T2", "T1"])

  lettres <- multcompView::multcompLetters(m, threshold = 0.05)$Letters[niv]
  expect_false(anyNA(lettres))
  expect_equal(length(lettres), 6L)
  expect_true(nzchar(lettres[["T1"]]))       # la modalité qui manquait

  # UNE PAIRE ABSENTE GARDE 1 -- « pas de différence » --, le repli prudent :
  # il regroupe au lieu de séparer à tort.
  vide <- hstat_pmat_demi(NULL, niv)
  expect_equal(dim(vide), c(6L, 6L))
  expect_true(all(vide == 1))
  # Un niveau que le facteur ne connaît pas est NOMMÉ, pas absorbé en silence.
  etranger <- brut
  rownames(etranger)[1] <- "ZZZ"
  e2 <- hstat_pmat_demi(etranger, niv)
  expect_true(length(attr(e2, "non_resolues")) > 0)
  expect_true(any(grepl("ZZZ", attr(e2, "non_resolues"))))
})

# -----------------------------------------------------------------------------
# UNE SELECTION SURVIT AU JEU DE DONNEES QU'ELLE DESIGNE
# -----------------------------------------------------------------------------
test_that("une colonne disparue est reconnue comme telle, pas devinee", {
  d <- data.frame(a = 1:3, b = 4:6)
  expect_equal(hstat_cols_absentes(d, "ch_Hel"), "ch_Hel")
  expect_equal(hstat_cols_absentes(d, c("a", "b")), character(0))
  expect_equal(hstat_cols_absentes(d, c("a", "z", "z")), "z")  # dedoublonne
  # Ce qui n'est pas un nom n'est pas une colonne manquante : un selecteur vide
  # rend "" ou NA, et les compter ferait refuser un etat parfaitement normal.
  expect_equal(hstat_cols_absentes(d, c("", NA)), character(0))
  expect_equal(hstat_cols_absentes(NULL, "a"), character(0))
  expect_equal(hstat_cols_absentes(d, NULL), character(0))
  # Le pendant positif, celui qui se lit dans un `req()`.
  expect_true(hstat_cols_pretes(d, c("a", "b")))
  expect_false(hstat_cols_pretes(d, c("a", "ch_Hel")))
  expect_true(hstat_cols_pretes(d, NULL))
})

test_that("le graphique descriptif attend l'echo plutot que de tomber", {
  # Le defaut constate a l'ecran : apres un changement de fichier, le
  # navigateur renvoie encore l'ANCIENNE colonne, et `.data[[input$...]]`
  # levait « Column `ch_Hel` not found in `.data` » -- un message qui accuse
  # les donnees pour un simple aller-retour en cours.
  src <- readLines(.hstat_module_path("mod_descriptive.R"), warn = FALSE)
  src <- paste(src, collapse = "\n")
  i_def <- regexpr("generate_desc_plot <- function", src, fixed = TRUE)
  expect_true(i_def > 0)
  i_hist <- regexpr("geom_histogram", src, fixed = TRUE)
  expect_true(i_hist > i_def)
  corps <- substr(src, i_def, i_hist)
  expect_true(grepl("hstat_cols_pretes", corps, fixed = TRUE))
  # LE FACTEUR DE GROUPEMENT COMPTE AUSSI : il nomme lui aussi une colonne du
  # fichier, et « Aucun » n'en est pas une.
  expect_true(grepl("descPlotFactor", corps, fixed = TRUE))
})

# -----------------------------------------------------------------------------
# LE RANG, PAS LE DETERMINANT -- et ce qui est redondant se nomme
# -----------------------------------------------------------------------------
test_that("hstat_colineaires nomme les colonnes surnumeraires", {
  set.seed(11)
  d <- data.frame(a = stats::rnorm(40), b = stats::rnorm(40))
  d$c <- d$a + d$b                      # exactement deduite des deux autres
  r <- hstat_colineaires(d)
  expect_equal(r$rang, 2L)
  expect_equal(r$p, 3L)
  expect_equal(r$redondantes, "c")
  # Un tableau de plein rang ne fait ecarter personne.
  plein <- hstat_colineaires(d[, c("a", "b")])
  expect_equal(plein$rang, 2L)
  expect_equal(plein$redondantes, character(0))
  # Une colonne CONSTANTE n'est pas « redondante », elle est vide de variation.
  # `scale()` y rendrait des NaN et le rang deviendrait NA : elle est ecartee
  # avant, et nommee comme les autres.
  dk <- d[, c("a", "b")]; dk$k <- 5
  expect_true("k" %in% hstat_colineaires(dk)$redondantes)
  expect_equal(hstat_colineaires(dk)$rang, 2L)
  expect_equal(hstat_colineaires(NULL)$redondantes, character(0))
})

test_that("le rang se mesure sur des colonnes CENTREES, comme cor() le fait", {
  # LE CAS QUI DECIDE, et il est ordinaire : une colonne qui est la somme de
  # deux autres PLUS UNE CONSTANTE -- un total rebase, un indice ramene a 100.
  # Elle n'est pas une combinaison lineaire exacte des deux autres tant qu'on
  # ne centre pas ; la matrice de CORRELATIONS, elle, est centree par
  # construction et devient singuliere. Un rang mesure sans centrage la
  # declarerait pleine, laisserait passer les trois colonnes, et `psych::fa()`
  # retomberait sur sa pseudo-inverse en imprimant sa pile d'erreurs.
  set.seed(5)
  a <- stats::rnorm(40); b <- stats::rnorm(40)
  d <- data.frame(a = a, b = b, c = a + b + 100)
  expect_equal(qr(as.matrix(d))$rank, 3L)          # sans centrage : plein
  expect_true(inherits(try(solve(stats::cor(d)), silent = TRUE), "try-error"))
  r <- hstat_colineaires(d)
  expect_equal(r$rang, 2L)                          # avec centrage : deficient
  expect_equal(r$redondantes, "c")
})

test_that("le rang est invariant d'echelle, le determinant ne l'est pas", {
  # C'est la lecon deja tiree deux fois dans ce depot (box_m_test, puis
  # detect_multivariate_outliers) : cinq variables mesurees en microgrammes
  # font tomber le determinant de la covariance a ~1e-60 alors que le rang
  # reste PLEIN. Tout seuil pose sur le determinant crierait a la singularite
  # sur des donnees parfaitement inversibles.
  set.seed(23)
  d <- as.data.frame(matrix(stats::rnorm(200), 40, 5))
  micro <- d * 1e-6
  expect_lt(det(stats::cov(micro)), 1e-40)          # le determinant s'effondre
  expect_equal(hstat_colineaires(micro)$rang, 5L)   # le rang, lui, ne bouge pas
  expect_equal(hstat_colineaires(micro)$redondantes, character(0))
  # Et l'assertion mord dans l'autre sens : une VRAIE colinearite est vue aux
  # deux echelles. Sans cette moitie, un code qui aurait simplement retire le
  # garde-fou passerait le test.
  d$V6 <- d$V1 * 3
  expect_equal(hstat_colineaires(d)$rang, 5L)
  expect_equal(hstat_colineaires(d * 1e-6)$rang, 5L)
  expect_true(length(hstat_colineaires(d)$redondantes) == 1L)
})

test_that("l'AFE retire les colonnes colineaires en les nommant", {
  # LES COMMENTAIRES SONT RETIRES PAR L'ANALYSEUR DE R. Ecrit sur le texte
  # brut, ce test se signalait LUI-MEME : le commentaire qui documente la
  # correction cite `psych::fa()`, et l'ordre relevé devenait celui du
  # commentaire, pas celui du code. C'est le piège deja documente ailleurs
  # dans ce depot, et il s'est represente ici.
  f <- file.path(.hstat_repo_root(), "inst", "app", "app_server.R")
  code <- paste(.hstat_code_lignes(f), collapse = "\n")
  i <- regexpr("observeEvent(input$mv_efa_run", code, fixed = TRUE)
  expect_true(i > 0)
  j_fa <- regexpr("psych::fa(", substr(code, i, nchar(code)), fixed = TRUE)
  j_co <- regexpr("hstat_colineaires", substr(code, i, nchar(code)), fixed = TRUE)
  # `psych::fa()` ne s'arrete PAS sur une matrice singuliere : il imprime son
  # « Error in solve.default(r) », bascule sur une pseudo-inverse et rend quand
  # meme un tableau. On tranche AVANT lui, et sur le rang.
  expect_true(j_co > 0)
  expect_true(j_fa > 0)
  expect_lt(j_co, j_fa)
})

# -----------------------------------------------------------------------------
# UN POINT QUI DISPARAIT SE NOMME
# -----------------------------------------------------------------------------
test_that("hstat_coord_incompletes nomme les coordonnees non finies", {
  m <- matrix(c(1, 2, NaN, 4, 5, NA, 7, 8), 4, 2,
              dimnames = list(c("a", "b", "c", "d"), NULL))
  expect_equal(hstat_coord_incompletes(m), c("b", "c"))
  # L'axe demande change la reponse : `c` n'est fautive que sur le premier.
  expect_equal(hstat_coord_incompletes(m, axes = 1L), "c")
  expect_equal(hstat_coord_incompletes(m, axes = 2L), "b")
  # UN SEUL AXE : FactoMineR rend alors un VECTEUR NU, et `m[, 1:2]` echouerait
  # sur « incorrect number of dimensions ». Le passage par hstat_coord_mat()
  # est ce qui l'evite -- meme regle que partout ailleurs dans le depot.
  expect_equal(hstat_coord_incompletes(c(a = 1, b = NaN, c = 3)), "b")
  # Un axe hors du domaine ne fait rien ecarter, il n'existe simplement pas.
  expect_equal(hstat_coord_incompletes(m, axes = c(7L, 9L)), character(0))
  expect_equal(hstat_coord_incompletes(NULL), character(0))
  # Sans noms de lignes, on rend le rang : « la 2e » vaut mieux que rien.
  sans <- m; rownames(sans) <- NULL
  expect_equal(hstat_coord_incompletes(sans), c("2", "3"))
})

# -----------------------------------------------------------------------------
# RENDEMENT : LA BARRE D'ERREUR NE PORTE QUE SA MOITIE HAUTE
# -----------------------------------------------------------------------------
test_that("les barres d'erreur du rendement ne descendent jamais sous la valeur", {
  skip_if_not_installed("ggplot2")
  h <- data.frame(Modalite = rep(c("T0", "T1", "T2"), each = 4),
                  Masse    = c(10, 11, 12, 9, 14, 15, 16, 13, 20, 21, 19, 22),
                  Surface  = rep(1, 12), Bloc = rep(1:4, 3),
                  stringsAsFactors = FALSE)
  v <- shiny::reactiveValues(data = h, cleanData = h, filteredData = h)
  p <- NULL; r <- NULL
  shiny::testServer(mod_yield_server, args = list(values = v), {
    vider <- function() try(session$flushReact(), silent = TRUE)
    session$setInputs(yieldSource = "fichier", yieldModalite = "Modalite",
                      yieldMasse = "Masse", yieldSurface = "Surface",
                      yieldRepetition = "Bloc", yieldTemoin = "T0"); vider()
    session$setInputs(yieldMesure = "Rendement_moyen", yieldErreurs = TRUE,
                      yieldErreurType = "se"); vider()
    p <<- graphique(); r <<- resultat()
  })
  expect_s3_class(p, "ggplot")
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  # LA DEMI-BARRE SE MONTE EN DEUX COUCHES : une hampe, et une coiffe d'etendue
  # nulle. Un `geom_errorbar` unique allant de la valeur au sommet poserait
  # AUSSI une coiffe au pied -- invisible sur une barre, elle barrerait le
  # point d'un nuage ou d'une sucette.
  expect_true("GeomLinerange" %in% geoms)
  expect_true("GeomErrorbar" %in% geoms)

  ordre  <- order(as.character(r$Modalite))
  val    <- r$Rendement_moyen[ordre]
  haut   <- val + r$Erreur_type[ordre]

  hampe <- b$data[[which(geoms == "GeomLinerange")[1]]]
  hampe <- hampe[order(hampe$x), ]
  expect_equal(hampe$ymin, val, tolerance = 1e-8)
  expect_equal(hampe$ymax, haut, tolerance = 1e-8)

  coiffe <- b$data[[which(geoms == "GeomErrorbar")[1]]]
  coiffe <- coiffe[order(coiffe$x), ]
  expect_equal(coiffe$ymin, coiffe$ymax, tolerance = 1e-8)
  expect_equal(coiffe$ymax, haut, tolerance = 1e-8)

  # L'ASSERTION QUI MORD : aucune couche d'erreur ne descend sous la valeur.
  # Ecrite sur les seules bornes hautes, elle passerait encore sur la barre
  # symetrique d'avant -- c'est celle-ci qui la refuse.
  for (i in which(geoms %in% c("GeomLinerange", "GeomErrorbar"))) {
    d <- b$data[[i]]; d <- d[order(d$x), ]
    expect_true(all(d$ymin >= val - 1e-8))
  }
  # La demi-barre depasse VRAIMENT la valeur, sans quoi l'assertion ci-dessus
  # serait satisfaite par une erreur-type nulle -- donc par rien du tout.
  expect_true(all(haut > val))

  # Decochee, la barre d'erreur ne pose aucune des deux couches.
  v2 <- shiny::reactiveValues(data = h, cleanData = h, filteredData = h)
  p2 <- NULL
  shiny::testServer(mod_yield_server, args = list(values = v2), {
    vider <- function() try(session$flushReact(), silent = TRUE)
    session$setInputs(yieldSource = "fichier", yieldModalite = "Modalite",
                      yieldMasse = "Masse", yieldSurface = "Surface",
                      yieldRepetition = "Bloc", yieldTemoin = "T0"); vider()
    session$setInputs(yieldMesure = "Rendement_moyen", yieldErreurs = FALSE); vider()
    p2 <<- graphique()
  })
  g2 <- vapply(p2$layers, function(l) class(l$geom)[1], character(1))
  expect_false(any(c("GeomLinerange", "GeomErrorbar") %in% g2))
})

# -----------------------------------------------------------------------------
# LA PAGE RENDUE : TITRE, ICONE, ET AUCUN CHAMP QUE LE NAVIGATEUR REFUSE
# -----------------------------------------------------------------------------
# Ces trois-la ne se voient qu'en CONSTRUISANT l'interface. Chercher l'appel
# dans le source dirait « fait » d'une declaration placee hors du chemin de
# rendu -- la lecon deja tiree pour l'adoption du kit d'options.
#
# Et il faut lire le FRAGMENT D'EN-TETE autant que le corps : `renderTags()`
# rend les deux separement, et `<title>` comme `<link rel="icon">` vivent dans
# le premier. Mesurer le seul corps concluait « aucun titre » sur une page qui
# en porte un.
.hstat_ui_rendue <- function() {
  root <- .hstat_repo_root()
  if (is.na(root)) return(NULL)
  e <- new.env(parent = globalenv())
  ok <- tryCatch({
    suppressMessages(suppressWarnings({
      old <- setwd(file.path(root, "inst", "app")); on.exit(setwd(old), add = TRUE)
      socle <- file.path(root, "R")
      for (f in c(file.path(socle, "utils.R"),
                  list.files(socle, pattern = "^mod_.*[.]R$", full.names = TRUE)))
        try(sys.source(f, e), silent = TRUE)
      hstat_installer_replis_ui(e)
      try(sys.source("UX.R", e), silent = TRUE)
    }))
    exists("ui", envir = e)
  }, error = function(err) FALSE)
  if (!isTRUE(ok)) return(NULL)
  rt <- htmltools::renderTags(get("ui", e))
  paste(paste(as.character(rt$head), collapse = "\n"),
        paste(as.character(rt$html), collapse = "\n"), sep = "\n")
}

test_that("l'onglet du navigateur porte un nom, pas le balisage du bandeau", {
  html <- .hstat_ui_rendue()
  skip_if(is.null(html), "interface non constructible dans cet environnement")
  # SANS `title =`, shinydashboard reprend le titre de l'EN-TETE -- ici une
  # grappe de balises -- et le serialise dans <title>. Mesure dans la page
  # rendue avant correction : 270 caracteres de balisage brut dans l'onglet,
  # dans le signet et dans l'historique.
  titre <- regmatches(html, regexpr("<title>[^<]*</title>", html))
  expect_length(titre, 1L)
  expect_equal(titre, "<title>HStat</title>")
  expect_false(grepl("<title>[^<]*&lt;", html))   # aucune balise echappee dedans
})

test_that("l'icone d'onglet est declaree, et elle porte l'estampille de version", {
  html <- .hstat_ui_rendue()
  skip_if(is.null(html), "interface non constructible dans cet environnement")
  # Sans `rel = "icon"`, le navigateur demande `/favicon.ico` de lui-meme et
  # l'application n'en sert aucun : CHAQUE visite inscrivait un 404 dans la
  # console, ou il masque les vraies erreurs.
  expect_true(grepl('rel="icon"', html, fixed = TRUE))
  href <- regmatches(html, regexpr("hstat-favicon[^\"]*", html))
  expect_length(href, 1L)
  # L'estampille est la regle du depot : un fichier statique servi sous un nom
  # inchange reste en cache, et l'utilisateur garde l'ancienne icone.
  expect_true(grepl("[?]v=", href))
  root <- .hstat_repo_root()
  expect_true(file.exists(file.path(root, "inst", "app", "www", "hstat-favicon.svg")))
})

test_that("aucun champ numerique ne rend value=\"NA\"", {
  html <- .hstat_ui_rendue()
  skip_if(is.null(html), "interface non constructible dans cet environnement")
  # `numericInput(value = NA)` ecrit `value="NA"` dans le HTML. Le navigateur
  # REFUSE la valeur -- le champ s'affiche vide, ce qui est bien l'intention
  # (« vide = automatique ») -- mais il l'ecrit dans la console :
  #
  #   The specified value "NA" cannot be parsed, or is out of range.
  #
  # Vingt-trois fois par page. `value = NULL` omet l'attribut et rend
  # exactement le meme champ vide, sans un mot. Meme famille que le polyfill
  # de plotly et la dependance `strftime` : un avertissement permanent en
  # console masque les vrais.
  expect_equal(lengths(regmatches(html, gregexpr('value="NA"', html, fixed = TRUE))), 0L)
  # Et le balayage des sources, pour que la forme ne revienne pas : plus aucun
  # `numericInput(..., value = NA)` dans le depot.
  restants <- character(0)
  for (f in .hstat_sources_app()) {
    pd <- tryCatch(utils::getParseData(parse(f, keep.source = TRUE)),
                   error = function(e) NULL)
    if (is.null(pd)) next
    ap <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "numericInput", ]
    for (i in seq_len(nrow(ap))) {
      z <- pd[pd$line1 >= ap$line1[i] & pd$line1 <= ap$line1[i] + 3L, ]
      z <- z[order(z$line1, z$col1), ]
      k <- which(z$token == "SYMBOL_SUB" & z$text == "value")
      for (kk in k) {
        s <- z[seq_len(nrow(z)) > kk, ]
        s <- s[s$token %in% c("EQ_SUB", "NUM_CONST", "SYMBOL", "NULL_CONST"), ]
        if (nrow(s) >= 2 && s$token[1] == "EQ_SUB" && s$text[2] == "NA")
          restants <- c(restants, paste0(basename(f), ":", z$line1[kk]))
      }
    }
  }
  expect_equal(restants, character(0))
})

# -----------------------------------------------------------------------------
# `&` EST VECTORISE : SES DEUX COTES SONT EVALUES
# -----------------------------------------------------------------------------
test_that("aucun `&` ne fait calculer un cote vectorise qu'un test de type annule", {
  # Le defaut trouve dans `hstat_vars_zero()`, et il coutait cher :
  #
  #   is.character(plat) & !nzchar(plat)
  #
  # Le cote gauche est un FALSE SCALAIRE sur une colonne numerique -- le cote
  # droit ne peut donc rien changer au resultat. Mais `&` n'est pas `&&` : il
  # evalue les deux, et `nzchar()` sur un vecteur numerique le convertit
  # d'abord ENTIEREMENT en chaines. Mesure sur 100 000 lignes : 0,69 s par
  # colonne, contre 0,02 s pour le `is.na()` voisin de la meme ligne. Ce
  # diagnostic tourne a chaque chargement de fichier -- 1,38 s sont devenues
  # 0,20 s, et le resultat n'a pas bouge d'une ligne.
  #
  # Le balayage porte sur le motif, pas sur le site : c'est ce qui l'empeche de
  # revenir ailleurs. Il n'y en avait qu'un dans tout le depot.
  scalaires <- c("is.character", "is.numeric", "is.factor", "is.logical",
                 "is.data.frame", "is.null", "inherits", "is.matrix",
                 "is.list", "is.function", "is.environment")
  vectorises <- c("nzchar", "grepl", "trimws", "tolower", "toupper", "nchar",
                  "as.character", "as.numeric", "substr")
  trouves <- character(0)
  visite <- function(e, f) {
    if (!is.call(e)) return(invisible(NULL))
    op <- as.character(e[[1]])[1]
    if (op %in% c("&", "|") && length(e) == 3L) {
      cote <- function(x) if (is.call(x)) as.character(x[[1]])[1] else ""
      # `!nzchar(x)` : on regarde sous la negation, sinon le motif se cache.
      sous <- function(x) {
        if (is.call(x) && identical(as.character(x[[1]])[1], "!") && length(x) == 2L) x[[2]] else x
      }
      a <- cote(sous(e[[2]])); b <- cote(sous(e[[3]]))
      if ((a %in% scalaires && b %in% vectorises) ||
          (b %in% scalaires && a %in% vectorises))
        trouves <<- c(trouves, sprintf("%s : %s", basename(f),
                                       paste(deparse(e), collapse = " ")))
    }
    # UN ARGUMENT VIDE (`d[i, ]`) rend le symbole manquant : il n'est pas NULL,
    # et le forcer leve « argument "el" is missing ». Une premiere version
    # enveloppait la descente dans un `try()` -- elle sautait alors des
    # sous-arbres EN SILENCE, et le balayage annoncait « aucun motif » sans les
    # avoir lus. Le vide se teste, il ne se rattrape pas.
    # UN ARGUMENT VIDE (`d[i, ]`) est le symbole manquant : il n'est pas NULL,
    # et toute lecture par `e[[i]]` leve « argument is missing ». Une premiere
    # version enveloppait la descente dans un `try()` -- elle sautait alors des
    # sous-arbres EN SILENCE, et le balayage annoncait « aucun motif » sans les
    # avoir lus. `as.list()` rend le vide comme une VALEUR qu'on peut tester.
    enfants <- as.list(e)
    for (k in seq_along(enfants)) {
      # Le vide se teste PAR INDEX. Le lier a une variable de boucle ne suffit
      # pas : `for (el in ...)` rend `el` manquant, et la moindre lecture leve.
      if (identical(enfants[[k]], quote(expr = )) || is.null(enfants[[k]])) next
      visite(enfants[[k]], f)
    }
    invisible(NULL)
  }
  for (f in .hstat_sources_app()) {
    ex <- tryCatch(parse(f), error = function(e) NULL)
    if (is.null(ex)) next
    for (i in seq_along(ex)) visite(ex[[i]], f)
  }
  expect_equal(unique(trouves), character(0))
})

test_that("hstat_vars_zero garde ses trois formes de colonne vide", {
  # La correction ci-dessus ne devait rien changer au comportement : les trois
  # formes sous lesquelles une colonne vide arrive -- typee LOGIQUE par un
  # lecteur de CSV, numerique tout-NA, ou remplie de chaines vides (Excel,
  # exports SPSS) -- restent nommees a part, et la colonne de zeros reste
  # detectee.
  z <- data.frame(a = c(0, 0, 0), b = c(1, 2, 3), vide_na = NA_real_,
                  vide_txt = c("", "  ", ""), vide_log = c(NA, NA, NA),
                  stringsAsFactors = FALSE)
  r <- hstat_vars_zero(z)
  expect_equal(r$Variable, "a")
  expect_setequal(attr(r, "vides"), c("vide_na", "vide_txt", "vide_log"))
})

# -----------------------------------------------------------------------------
# UN NOM DE TELECHARGEMENT PART DANS UN EN-TETE, PAS SUR UN DISQUE
# -----------------------------------------------------------------------------
test_that("hstat_nom_fichier retire ce qui casse un en-tete HTTP", {
  # `downloadHandler(filename = ...)` ne cree aucun fichier : la chaine devient
  # la valeur `filename=` de `Content-Disposition`. Un retour a la ligne y COUPE
  # l'en-tete, un guillemet en ferme la valeur, un separateur de chemin est
  # suivi par certains clients.
  expect_equal(hstat_nom_fichier("auteur"), "auteur")
  expect_false(grepl("[[:cntrl:]]", hstat_nom_fichier("a\nb\tc")))
  expect_false(grepl('"', hstat_nom_fichier('a"b')))
  expect_false(grepl("[/\\\\]", hstat_nom_fichier("../../etc/passwd")))
  expect_false(grepl("[/\\\\:]", hstat_nom_fichier("C:\\Windows\\notepad")))
  expect_false(grepl("[.][.]", hstat_nom_fichier("../../etc/passwd")))
  # Ce qui ne laisse rien retombe sur le defaut, jamais sur une chaine vide :
  # un `filename=""` fait enregistrer le fichier sous un nom invente par le
  # navigateur, ce qui est pire que le defaut annonce.
  expect_equal(hstat_nom_fichier("", "liste"), "liste")
  expect_equal(hstat_nom_fichier("..", "liste"), "liste")
  expect_equal(hstat_nom_fichier(NA, "liste"), "liste")
  expect_lte(nchar(hstat_nom_fichier(strrep("a", 500))), 80L)
})

test_that("l'export de liste DL50 assainit le nom qu'il recoit", {
  # LA VALEUR D'UN `selectInput` N'EST PAS UNE GARANTIE : elle arrive du
  # navigateur, et un client peut envoyer tout autre chose sur le websocket.
  # Une liste de choix contraint l'interface, pas le protocole.
  code <- paste(.hstat_code_lignes(.hstat_module_path("mod_dl50.R")), collapse = "\n")
  i <- regexpr('output$listeDl <- shiny::downloadHandler', code, fixed = TRUE)
  expect_true(i > 0)
  bloc <- substr(code, i, i + 400L)
  expect_true(grepl("hstat_nom_fichier", bloc, fixed = TRUE))
})

# -----------------------------------------------------------------------------
# CE QUI SORT DE LA MACHINE SE DIT, ET AU MOMENT DU CHOIX
# -----------------------------------------------------------------------------
test_that("les moteurs en reseau annoncent ce qu'ils transmettent", {
  ns <- shiny::NS("ai")
  # Le moteur « auto » n'a rien a annoncer : il ne sort pas de R.
  auto <- as.character(hstat_ai_reglages_ui(ns, "auto"))
  expect_false(any(grepl("transmis", auto)))
  expect_true(any(grepl("sans r", auto)))       # « sans réseau »
  # TOUS LES AUTRES le disent, celui qui pointe sur un serveur LOCAL compris :
  # le champ d'adresse est librement modifiable, et une note qui ne parlerait
  # que du tiers mentirait des que l'adresse change.
  for (moteur in setdiff(names(HSTAT_AI_FOURNISSEURS), "auto")) {
    h <- paste(as.character(hstat_ai_reglages_ui(ns, moteur)), collapse = " ")
    expect_true(grepl("transmis", h),
                info = paste("moteur sans annonce de transmission :", moteur))
    # Ce qui NE part pas est dit aussi : sans cela, l'utilisateur suppose le pire
    # et se prive d'une fonctionnalite qui ne lit pas son fichier.
    expect_true(grepl("jamais envoy", h), info = moteur)
  }
})

# -----------------------------------------------------------------------------
# LE CACHE EST DANS LE PAQUET, LES SESSIONS SONT DANS LE PROCESSUS
# -----------------------------------------------------------------------------
test_that("deux sessions ne partagent pas leurs agregations", {
  # `.hstat_cache` vit dans l'espace de noms : il est PARTAGE par toutes les
  # sessions servies par le meme processus R, ce qui est le fonctionnement
  # ordinaire de Shiny Server, Posit Connect et shinyapps.io.
  #
  # La vue DuckDB porte un nom FIXE (« hstat_source »), et la cle se composait
  # de ce nom, des colonnes et des statistiques demandees. Deux collegues qui
  # analysent le meme genre de fichier d'essai -- donc les memes noms de
  # colonnes -- produisaient la MEME cle. Mesure avant correction : la seconde
  # session demandait une moyenne de 7 et recevait 42, celle de la premiere.
  # Pas une erreur, pas un vide : le chiffre d'un AUTRE, sous le bon libelle.
  faux <- function(tok) structure(list(token = tok, userData = new.env()),
                                  class = "ShinySession")
  avec <- function(tok, expr) shiny::withReactiveDomain(faux(tok), expr)
  args <- list("desc_global", "hstat_source", c("Rendement", "Azote"), c("mean", "sd"))

  kA <- avec("sessA", do.call(hstat_cache_key, args))
  kB <- avec("sessB", do.call(hstat_cache_key, args))
  expect_false(identical(kA, kB))
  # Le prefixe est bien l'identifiant, pas un sel quelconque : c'est lui qui
  # permet le vidage cible ci-dessous.
  expect_true(startsWith(kA, "sessA::"))

  avec("sessA", hstat_cache_clear()); avec("sessB", hstat_cache_clear())
  vA <- avec("sessA", hstat_cache_get(kA, function() data.frame(mean = 42)))
  vB <- avec("sessB", hstat_cache_get(kB, function() data.frame(mean = 7)))
  expect_equal(vA$mean, 42)
  expect_equal(vB$mean, 7)          # et surtout PAS 42

  # Le cache sert encore : A relit sa valeur sans recalculer.
  expect_equal(avec("sessA", hstat_cache_get(kA, function() data.frame(mean = -1)))$mean, 42)
  # Et le vidage de B ne touche pas a celui de A. Vider tout ne FAUSSERAIT
  # rien, mais ferait recalculer les agregations des autres sessions a chaque
  # fois qu'un utilisateur charge un fichier.
  avec("sessB", hstat_cache_clear())
  expect_equal(avec("sessA", hstat_cache_get(kA, function() data.frame(mean = -1)))$mean, 42)
  expect_equal(avec("sessB", hstat_cache_get(kB, function() data.frame(mean = 99)))$mean, 99)
  avec("sessA", hstat_cache_clear()); avec("sessB", hstat_cache_clear())
})

# -- CE QU'UNE SESSION A MIS EN CACHE PART AVEC ELLE --------------------------
# `hstat_cache_clear()` n'etait appele qu'au CHARGEMENT d'un fichier, et
# `onSessionEnded` ne fermait que la connexion DuckDB. Les agregations d'une
# session terminee restaient donc dans l'espace de noms du paquet, que le
# processus garde : mesure sur quarante sessions portant chacune 5 Mo,
# +367 Mo retenus, aucune ligne liberee.
#
# LE JETON EST L'ENJEU, et c'est ce qui rend l'argument `id` necessaire :
# `onSessionEnded` s'execute HORS du domaine reactif, ou `.hstat_session_id()`
# retombe sur « hors-session ». Un `hstat_cache_clear()` nu n'y retirerait
# rien -- et le test doit distinguer les deux, sinon il passerait sur un code
# qui ne vide toujours pas.
test_that("le cache d'une session est vide a sa fermeture", {
  faux <- function(tok) structure(list(token = tok, userData = new.env()),
                                  class = "ShinySession")
  avec <- function(tok, expr) shiny::withReactiveDomain(faux(tok), expr)
  restes <- function(tok) {
    cles <- ls(.hstat_cache, all.names = TRUE)
    length(cles[startsWith(cles, paste0(tok, "::"))])
  }

  kA <- avec("finA", hstat_cache_key("t", "x"))
  kB <- avec("finB", hstat_cache_key("t", "x"))
  avec("finA", hstat_cache_get(kA, function() 1))
  avec("finB", hstat_cache_get(kB, function() 2))
  expect_equal(restes("finA"), 1L)

  # 1. LE CHEMIN REEL : le jeton est capte a la construction et passe au
  #    rappel, qui s'execute SANS domaine reactif.
  hstat_cache_clear("finA")
  expect_equal(restes("finA"), 0L)

  # 2. ET LA PURGE RESTE CIBLEE : la session voisine garde la sienne. Vider
  #    tout ne fausserait rien, mais ferait recalculer les agregations des
  #    autres sessions du meme processus.
  expect_equal(restes("finB"), 1L)

  # 3. L'ASSERTION QUI DISTINGUE LES DEUX CODES : hors domaine reactif, un
  #    appel NU ne retire rien -- c'est exactement ce que faisait
  #    `onSessionEnded` avant, et c'est pourquoi l'argument existe.
  hstat_cache_clear()
  expect_equal(restes("finB"), 1L)
  hstat_cache_clear("finB")
  expect_equal(restes("finB"), 0L)

  # 4. Sans argument et DANS une session, le comportement d'avant est intact.
  avec("finB", hstat_cache_get(kB, function() 3))
  avec("finB", hstat_cache_clear())
  expect_equal(restes("finB"), 0L)
})

# Et le rappel de fermeture appelle bien la purge : une aide corrigee que
# personne n'appelle ne vide rien. On lit le corps de `onSessionEnded` dans
# `app_server.R` -- le seul endroit ou la session se termine.
test_that("onSessionEnded purge le cache, avec le jeton capte au-dehors", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  f <- file.path(root, "inst", "app", "app_server.R")
  skip_if_not(file.exists(f))
  src <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Le jeton est relevé HORS du rappel...
  expect_true(grepl("session$token", src, fixed = TRUE))
  # ...et la purge le reçoit : `hstat_cache_clear()` nu n'aurait rien retiré.
  expect_true(grepl("hstat_cache_clear(.hstat_jeton_session)", src, fixed = TRUE))
})

test_that("hors de Shiny la cle reste stable, donc testable", {
  # Le jeton de repli est une CONSTANTE, pas un tirage : une cle qui changerait
  # a chaque appel rendrait le cache inutile en ligne de commande et dans les
  # tests -- le defaut inverse de celui qu'on vient de corriger.
  k1 <- hstat_cache_key("a", "b")
  k2 <- hstat_cache_key("a", "b")
  expect_identical(k1, k2)
  expect_true(startsWith(k1, "hors-session::"))
  hstat_cache_clear()
  expect_equal(hstat_cache_get(k1, function() 1), 1)
  expect_equal(hstat_cache_get(k1, function() 2), 1)   # bien servi par le cache
  hstat_cache_clear()
  expect_equal(hstat_cache_get(k1, function() 2), 2)   # et bien vide
})

# -----------------------------------------------------------------------------
# LE REPLI D'UN PAQUET ABSENT NE PEUT PAS ETRE CE PAQUET
# -----------------------------------------------------------------------------
test_that("hstat_axe_titre survit a l'absence de ggtext", {
  skip_if_not_installed("ggplot2")
  # La garde disait « si ggtext manque, se replier » -- et se repliait sur
  # `ggtext::element_markdown()`, c'est-a-dire sur le paquet qu'elle venait de
  # constater manquant. Sans ggtext, TOUT titre d'axe levait « there is no
  # package called 'ggtext' », et avec lui le graphique entier, dans SEPT
  # modules. Le paquet est en Suggests a bon droit (l'interface se construit
  # sans lui) : la regle du depot s'applique donc en entier -- aucune fonction
  # essentielle ne doit dependre d'un paquet optionnel.
  #
  # On MESURE le repli plutot que de le lire : `requireNamespace` est masquee
  # le temps de l'appel, ce qui reproduit exactement une machine sans ggtext.
  # ON N'ECRIT PAS DANS L'ENVIRONNEMENT DE LA FONCTION.
  #
  # Depuis les sources, `environment(hstat_axe_titre)` est un environnement de
  # test, donc inscriptible -- et cette version-ci passait. Sous `R CMD check`
  # le paquet est INSTALLE : cet environnement est alors l'espace de noms, il
  # est VERROUILLE, et `assign()` leve « cannot add bindings to a locked
  # environment ». Le test etait donc vert partout sauf la ou il comptait le
  # plus, et rien ne le disait -- 189 des 190 echecs du check etaient des
  # artefacts de locale, celui-ci etait le seul reel.
  #
  # Le remede est l'idiome deja employe cinquante lignes plus bas par
  # `poser()` : on ne touche a rien, on fabrique une COPIE de la fonction dont
  # le parent porte le masque, et l'on evalue l'appel dans un masque de
  # donnees ou le nom designe cette copie.
  sans_ggtext <- function(expr) {
    vrai <- base::requireNamespace
    faux <- function(package, ...) if (identical(package, "ggtext")) FALSE
                                   else vrai(package, ...)
    f  <- hstat_axe_titre
    e2 <- new.env(parent = environment(f))
    assign("requireNamespace", faux, envir = e2)
    environment(f) <- e2
    eval(expr, list(hstat_axe_titre = f), enclos = parent.frame())
  }
  e <- sans_ggtext(quote(hstat_axe_titre(size = 17, face = "bold",
                                         align = "1", colour = "#123456")))
  # LA CLASSE SE COMPARE EXACTEMENT, PAS PAR HERITAGE. `element_markdown`
  # HERITE de `element_text` : `expect_s3_class(e, "element_text")` etait donc
  # satisfait par l'objet meme qu'il devait refuser, et la mutation qui
  # remettait le repli fautif passait sans un mot. Attrape par mutation, pas
  # par relecture -- meme famille que « precision et rappel coincident sur une
  # matrice equilibree ».
  expect_identical(class(e), class(ggplot2::element_text()))
  expect_false(inherits(e, "element_markdown"))
  expect_false(inherits(e, "element_textbox"))
  # LE REPLI GARDE LES REGLAGES. Rendre un `element_blank()` -- ou un element
  # nu -- ferait disparaitre le style que l'utilisateur vient de choisir : le
  # defaut serait alors silencieux, donc pire que l'erreur qu'on corrige.
  expect_equal(e$size, 17)
  expect_equal(e$face, "bold")
  expect_equal(e$hjust, 1)
  expect_equal(e$colour, "#123456")
  # Les deux axes restent servis : la marge n'est pas du meme cote.
  for (cas in list(quote(hstat_axe_titre(axe = "y")),
                   quote(hstat_axe_titre(retour = FALSE)),
                   quote(hstat_axe_titre(retour = TRUE)))) {
    ec <- sans_ggtext(cas)
    expect_identical(class(ec), class(ggplot2::element_text()))
  }
  # Avec ggtext, les DEUX branches d'origine sont intactes -- sans quoi le
  # correctif aurait supprime la fonctionnalite au lieu de la proteger.
  skip_if_not_installed("ggtext")
  expect_s3_class(hstat_axe_titre(retour = TRUE), "element_textbox")
  expect_s3_class(hstat_axe_titre(retour = FALSE), "element_markdown")
})

# -----------------------------------------------------------------------------
# L'AIGUILLAGE DE ggtext : LE REPLI REPREND L'ORDRE, IL NE DELEGUE PAS A `...`
# -----------------------------------------------------------------------------
test_that("element_markdown est toujours defini, avec ou sans ggtext", {
  skip_if_not_installed("ggplot2")
  # Deux environnements : l'un ou ggtext existe, l'autre ou il est declare
  # absent. `hstat_installer_replis_ui()` lit `requireNamespace` depuis SON
  # environnement d'execution -- on le masque donc la, ce qui reproduit
  # exactement une machine sans ggtext.
  poser <- function(ggtext_present) {
    env <- new.env(parent = globalenv())
    f <- hstat_installer_replis_ui
    e2 <- new.env(parent = environment(f))
    if (!ggtext_present) {
      vrai <- base::requireNamespace
      assign("requireNamespace", function(package, ...)
        if (identical(package, "ggtext")) FALSE else vrai(package, ...), envir = e2)
    }
    environment(f) <- e2
    suppressMessages(f(env))
    get("element_markdown", envir = env)
  }

  # SANS ggtext : on rend un vrai element_text, pas une erreur, pas un blanc.
  em <- poser(FALSE)
  el <- em(size = 17, face = "bold", colour = "#123456", hjust = 1)
  expect_identical(class(el), class(ggplot2::element_text()))
  expect_equal(el$size, 17); expect_equal(el$face, "bold")
  expect_equal(el$colour, "#123456"); expect_equal(el$hjust, 1)

  # LE PIEGE QUE LE REPLI EXISTE POUR EVITER. Les deux signatures divergent des
  # la troisieme position :
  #   element_markdown(family, face, size,   colour, ...)
  #   element_text    (family, face, colour, size,   ...)
  # Un repli ecrit `function(...) element_text(...)` echangerait donc taille et
  # couleur sur tout appel POSITIONNEL -- sans lever, sans avertir.
  pos <- em(NULL, "bold", 13, "#ff0000")
  expect_equal(pos$size, 13)            # et non "#ff0000"
  expect_equal(pos$colour, "#ff0000")   # et non 13

  # `color` a l'americaine reste accepte : neuf appels du depot l'ecrivent ainsi.
  expect_equal(em(color = "#00ff00")$colour, "#00ff00")
  # `halign` de gridtext n'existe pas chez element_text : le laisser tomber en
  # silence perdrait un reglage. On le rapproche de `hjust`.
  expect_equal(em(halign = 0.5)$hjust, 0.5)
  # Un argument que `element_text` ignore ne doit pas faire LEVER le repli :
  # c'est tout le graphique qui tomberait pour une bordure decorative.
  expect_silent(em(size = 10, fill = "white", box.colour = "grey", r = 3))

  # AVEC ggtext : c'est bien la vraie fonction, le rendu markdown est preserve.
  skip_if_not_installed("ggtext")
  expect_s3_class(poser(TRUE)(size = 12), "element_markdown")
})

test_that("les modules appellent element_markdown sans prefixe", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # Le balayage general (« un nom optionnel passe par son aiguillage ») couvre
  # deja la forme `ggtext::element_markdown(`. Celui-ci verifie l'autre moitie :
  # que les appels EXISTENT toujours -- une reecriture qui les aurait supprimes
  # au lieu de les deprefixer passerait le premier sans un mot.
  n <- 0L
  for (f in .hstat_sources_app()) {
    if (identical(basename(f), "utils.R")) next
    l <- .hstat_code_lignes(f)
    n <- n + sum(grepl("(^|[^:[:alnum:]._])element_markdown\\(", l))
  }
  expect_gte(n, 23L)
})

# =============================================================================
#  LE CADRE CARRE DES ANALYSES MULTIVARIEES
# =============================================================================

test_that("hstat_carre_ui pose height:auto -- la moitie qui evite le debordement", {
  h <- as.character(hstat_carre_ui("truc"))
  # La LARGEUR est plafonnee a 7 pouces.
  expect_match(h, "max-width:840px", fixed = TRUE)
  # `height:auto` sur le plotOutput. C'est la moitie mesuree au navigateur :
  # sans elle le conteneur garde les 400 px du defaut de Shiny pendant que
  # l'image en fait 840, et tout ce qui suit se dessine PAR-DESSUS sa moitie
  # basse. Rien ne le signale.
  expect_match(h, "height\\s*:\\s*auto")
  # Et surtout : AUCUNE hauteur en pixels. C'est precisement la forme qui
  # produisait le debordement.
  expect_false(grepl("height\\s*:\\s*[0-9]+px", h))
  expect_match(h, "truc", fixed = TRUE)
})

test_that("hstat_carre_hauteur suit la largeur reelle et retombe sur le cote nominal", {
  faux <- function(v) list(clientData = stats::setNames(list(v), "output_p_width"))

  # Le cas normal : la hauteur EST la largeur accordee -- le cadre est carre a
  # toute taille, et vaut 7 x 7 pouces des que la place existe.
  expect_identical(hstat_carre_hauteur(faux(818), "p")(), 818)
  expect_identical(hstat_carre_hauteur(faux(380), "p")(), 380)
  expect_identical(hstat_carre_hauteur(faux(HSTAT_CARRE_PX), "p")(), HSTAT_CARRE_PX)

  # Les trois etats ou le navigateur n'a rien a dire : pas encore repondu,
  # boite repliee (0), valeur inexploitable. On retombe sur le cote nominal --
  # demander un peripherique de hauteur nulle ferait lever R.
  for (v in list(NULL, 0, -5, NA_real_, Inf, "840"))
    expect_identical(hstat_carre_hauteur(faux(v), "p")(), HSTAT_CARRE_PX)

  # L'identifiant est fige a la CONSTRUCTION, et la BOUCLE `for` est ce qui le
  # verifie. Sans `force(id)`, l'argument reste une promesse : elle n'est
  # evaluee qu'au premier appel de la fermeture, c'est-a-dire APRES la fin de
  # la boucle, quand la variable partagee porte sa DERNIERE valeur. Les
  # fermetures liraient alors toutes le meme identifiant, et chaque graphique
  # suivrait la largeur d'un autre.
  #
  # Une premiere version de cette assertion passait par `lapply()`, et ne
  # pouvait donc rien attraper : `lapply` cree une liaison FRAICHE a chaque
  # appel, si bien que la promesse resout juste meme sans `force()`. Mesure --
  # boucle `for` : 222 222 sans la garde, 111 222 avec ; `lapply` : 111 222
  # dans les deux cas. Une assertion qui ne distingue pas les deux codes ne
  # garde rien.
  sess <- list(clientData = list(output_a_width = 111, output_b_width = 222))
  fs <- list()
  for (k in c("a", "b")) fs[[k]] <- hstat_carre_hauteur(sess, k)
  expect_identical(vapply(fs, function(f) f(), numeric(1)),
                   c(a = 111, b = 222))
})

test_that("840 px a HSTAT_CARRE_RES font exactement les 7 pouces de ggplot2", {
  # Le peripherique que R ouvre par defaut -- celui pour lequel les valeurs par
  # defaut de ggplot2 sont reglees -- est CARRE de sept pouces de cote.
  expect_identical(HSTAT_CARRE_PX / HSTAT_CARRE_RES, 7)
})

test_that("aucun graphique multivarie ne declare de hauteur en pixels", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  ux <- .hstat_code_lignes(file.path(root, "inst", "app", "UX.R"))

  # C'est LE balayage qui compte : un graphique multivarie ajoute demain avec
  # `plotOutput(..., height = "520px")` reprendrait le cadre ecrase qu'on vient
  # de retirer, et rien d'autre ne le dirait. L'interface ne connait plus
  # qu'une seule facon de poser un de ces graphiques.
  restes <- grep("shiny::plotOutput\\(", ux, value = TRUE)
  expect_identical(restes, character(0))

  # Les dix graphiques passent tous par l'aide commune.
  expect_gte(sum(grepl("hstat_carre_ui\\(", ux)), 10L)
})

test_that("chaque rendu multivarie calcule sa hauteur au lieu de la subir", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  f <- file.path(root, "inst", "app", "app_server.R")
  l <- .hstat_code_lignes(f)

  ids <- c("pcaPlot", "pcaScreePlot", "pcaParallelPlot", "pcaCTRPlot",
           "hcpcDendPlot", "hcpcClusterPlot", "hcpcHeightsPlot",
           "afdIndPlot", "afdVarPlot")
  for (id in ids) {
    att <- sprintf("height = hstat_carre_hauteur(session, \"%s\")", id)
    expect_true(any(grepl(att, l, fixed = TRUE)),
                info = paste(id, ": le rendu ne calcule pas sa hauteur"))
  }
  # Le rendu generique : un seul appel, identifiant construit, pour les
  # quatorze analyses du catalogue.
  expect_true(any(grepl(
    'height = hstat_carre_hauteur(session, paste0("mv_", key, "_plot"))',
    l, fixed = TRUE)))

  # Un `renderPlot` multivarie sans hauteur calculee retomberait sur la
  # hauteur ANNONCEE par le navigateur -- c'est-a-dire, avec `height:auto`,
  # une hauteur qui depend de l'image precedente. Le cadre cesserait d'etre
  # carre sans que rien ne leve.
  expect_false(any(grepl("}, res = 120)", l, fixed = TRUE)))
})


# ============================================================================
#  AUDIT DE SECURITE : LE BALISAGE ET LE COUT
# ============================================================================

# -- 1. Un nom de colonne n'entre jamais tel quel dans du balisage ------------
# La regle est deja ecrite pour deux notifications de `mod_tests.R` et pour les
# memos de l'atelier de codage. Un troisieme site l'avait manquee :
# l'interpretation des correlations composait « <b>%s - %s</b> » avec
# `Variable_X` et `Variable_Y`, qui SONT les noms des colonnes du fichier. Une
# colonne « Rdt <2023> » y voyait « <2023> » lu comme une balise et disparaitre
# -- on annoncait a l'utilisateur un nom qui n'etait pas le sien.
#
# Le balayage passe par l'ANALYSEUR, pas par la ligne : un appel a `HTML()`
# s'etend sur plusieurs lignes, et c'est justement le cas de celui-ci.
test_that("aucun HTML() ne porte une donnee du fichier sans echappement", {
  fichiers <- .hstat_sources_app()
  skip_if(!length(fichiers), "sources indisponibles")

  ECHAPPEURS <- c("hstat_html_escape", ".hstat_code_esc",
                  "htmlEscape", "htmltools::htmlEscape")
  # Symboles qui trahissent une valeur venue du fichier de l'utilisateur.
  DATA <- c("names", "colnames", "rownames", "levels", "unique", "input",
            "values", "Variable", "Variable_X", "Variable_Y", "Modalite",
            "Niveau", "Cible", "label", "nom")

  # Trois sites verifies un par un et NOMMES plutot que devines -- la regle du
  # depot sur les balayages : celui qui crie au loup finit desactive.
  #   mod_coding   : `Cible` vaut `names(HSTAT_MEMO_CIBLES)`, une constante du
  #                  paquet, jamais une saisie ;
  #   app_server   : les deux sites n'interpolent que des nombres mis en forme
  #                  (`format(..., big.mark)`, `hstat_format_size`).
  EXCEPTIONS <- c("mod_coding.R:Cible", "app_server.R:values")

  symboles <- function(e) {
    if (is.name(e)) return(as.character(e))
    if (is.call(e) || is.pairlist(e) || is.expression(e))
      return(unlist(lapply(as.list(e), symboles)))
    character(0)
  }
  fautifs <- character(0)
  for (f in fichiers) {
    p <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(p)) next
    visite <- function(e) {
      if (!is.call(e)) return(invisible())
      fn <- paste(deparse(e[[1]]), collapse = "")
      if (grepl("(^|::)HTML$", fn) && length(e) > 1) {
        s <- symboles(e)
        if (!any(s %in% ECHAPPEURS)) {
          d <- intersect(s, DATA)
          # `paste0("f.R", ":", character(0))` rend "f.R:" -- une chaine de
          # LONGUEUR 1, non un vecteur vide. Indexer `d` (vide) par ce TRUE
          # rendait NA, et le balayage signalait TOUS les fichiers. C'est la
          # meme famille que « les operateurs vectoriels recyclent,
          # l'indexation non » : on borne avant de composer la cle.
          if (length(d)) d <- d[!paste0(basename(f), ":", d) %in% EXCEPTIONS]
          if (length(d)) fautifs <<- c(fautifs, paste0(basename(f), " : ",
                                                       paste(d, collapse = ", ")))
        }
      }
      for (i in seq_along(e)[-1]) {
        a <- tryCatch(e[[i]], error = function(x) NULL)
        if (!missing(a) && !is.null(a) && !identical(a, quote(expr = ))) visite(a)
      }
    }
    for (i in seq_along(p)) visite(p[[i]])
  }
  expect_identical(unique(fautifs), character(0))
})

# -- 2. La liste blanche borne ce qu'on APPELLE, pas ce que cela COUTE --------
# `hstat_safe_eval` refuse bien `system()` -- 99 tentatives d'evasion essayees,
# 99 refusees. Mais `paste0` est legitime, et imbrique il DOUBLE la chaine a
# chaque niveau : mesure sans borne, 2,6 Mo de formule collee allouaient 2,5 Go
# en 98 secondes. Shiny sert toutes les sessions depuis un seul processus R :
# c'est l'application entiere qui se fige, puis qui se fait tuer.
test_that("une formule hors de prix est refusee, une vraie formule passe", {
  df <- data.frame(x = rep("ABCDEFGHIJ", 200), y = 1:200,
                   stringsAsFactors = FALSE)

  # La borne porte sur la LONGUEUR, parce que le texte double en meme temps que
  # le resultat : c'est ce qui la rend suffisante.
  gonfle <- function(n) {
    f <- "x"; for (i in seq_len(n)) f <- paste0("paste0(", f, ",", f, ")"); f
  }
  trop <- gonfle(14L)                       # ~82 000 caracteres
  expect_gt(nchar(trop), HSTAT_FORMULA_MAX_CHARS)
  expect_error(hstat_safe_eval(trop, df), "trop longue")

  # Et le refus est IMMEDIAT : sans lui, cette formule-la allouait des centaines
  # de Mo avant de rendre la main. Une borne qui n'agirait qu'apres coup ne
  # servirait a rien.
  t0 <- Sys.time()
  try(hstat_safe_eval(gonfle(18L), df), silent = TRUE)
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)

  # Le pire cas SOUS la borne reste modeste -- mesure : 4,9 Mo en 0,14 s.
  sous <- gonfle(9L)
  expect_lte(nchar(sous), HSTAT_FORMULA_MAX_CHARS)
  expect_type(hstat_safe_eval(sous, df), "character")

  # Et rien de reel n'est gene : une moyenne de ligne sur deux cents colonnes
  # aux noms de quarante caracteres fait 8 415 caracteres, mesures.
  d2 <- as.data.frame(matrix(rnorm(200 * 4), 4, 200))
  names(d2) <- paste0("Variable_mesuree_au_champ_campagne_", sprintf("%05d", 1:200))
  f <- paste0("rowMeans(cbind(", paste(names(d2), collapse = ", "), "))")
  expect_gt(nchar(f), 8000)
  expect_length(hstat_safe_eval(f, d2), 4L)

  for (b in c("x", "y + 1", "log(y)", "paste0(x, '_', y)"))
    expect_silent(hstat_safe_eval(b, df))

  # La borne se regle, et une valeur aberrante retombe sur le defaut plutot que
  # de laisser passer n'importe quoi.
  expect_true(is.finite(HSTAT_FORMULA_MAX_CHARS) && HSTAT_FORMULA_MAX_CHARS >= 100)
})

# -- 3. Le bac a sable tient toujours : les portes interdites restent fermees --
test_that("aucune fonction d'ordre superieur ni de resolution de nom n'entre", {
  INTERDITES <- c("get", "get0", "mget", "match.fun", "eval", "evalq", "quote",
                  "bquote", "substitute", "do.call", "Recall", "sapply",
                  "lapply", "vapply", "mapply", "Map", "Reduce", "Filter",
                  "rapply", "apply", "outer", "Vectorize", "Negate",
                  "environment", "globalenv", "emptyenv", "baseenv", "as.environment",
                  "parent.frame", "sys.function", "sys.call", "match.call",
                  "assign", "local", "with", "within", "attach", "source",
                  "library", "require", "loadNamespace", "getExportedValue",
                  "getFromNamespace", "system", "system2", "shell", "file",
                  "readLines", "readRDS", "save", "load", "unlink",
                  "$", "[", "[[", "@", "::", ":::", "<-", "=", "~", "function")
  expect_identical(intersect(HSTAT_FORMULA_FUNS, INTERDITES), character(0))

  df <- data.frame(x = 1:3)
  for (mechant in c("system('id')", "get('system')('id')", "environment()",
                    "do.call('system','id')", "(function() 1)()",
                    "base::system('id')", "x[1]", "quote(system('id'))"))
    expect_error(hstat_safe_eval(mechant, df))
})


# ============================================================================
#  LE DICTIONNAIRE PART EN FICHIER, PAS DANS LA PAGE
# ============================================================================

# Ecrit en clair dans l'en-tete, il pesait 476 Ko sur une page de 2 143 -- soit
# 160 Ko des 332 Ko qui transitent apres gzip, LA MOITIE du transfert -- et il
# repartait a chaque ouverture, y compris chez un francophone qui ne s'en sert
# jamais. Servi en ressource statique, il est revalide : mesure sur un banc
# httpuv, une requete conditionnelle recoit 304 et zero octet de corps.
test_that("le dictionnaire est servi en ressource estampillee, et le repli tient", {
  skip_if_not_installed("shiny")

  src <- hstat_i18n_asset("en")
  skip_if(is.na(src), "ecriture de la ressource impossible ici")

  # L'estampille vient de hstat_asset() : un dictionnaire corrige arrive avec la
  # montee de version, et pas avant. Sans elle, un navigateur garderait
  # l'ancienne traduction sans qu'aucun message ne le lui dise.
  expect_true(grepl("?v=", src, fixed = TRUE))
  expect_true(grepl(hstat_version(), src, fixed = TRUE))
  expect_true(grepl("^hstat-dict/", src))
  expect_true("hstat-dict" %in% names(shiny::resourcePaths()))

  # Le fichier servi porte EXACTEMENT la charge utile. Les deux chemins lisent
  # hstat_i18n_payload() : ils ne peuvent pas diverger.
  f <- file.path(tempdir(), "hstat-dict", "hstat-i18n-en.js")
  expect_true(file.exists(f))
  expect_identical(readBin(f, "raw", file.size(f)),
                   charToRaw(hstat_i18n_payload("en")))

  # Idempotent : l'appeler deux fois ne redeclare rien et rend la meme adresse.
  expect_identical(src, hstat_i18n_asset("en"))

  # LA BALISE PORTE L'ADRESSE, PAS LE DICTIONNAIRE. Elle ne declare plus un
  # `<script src>` -- qui partait a chaque premiere visite -- mais l'adresse
  # que `hstat-i18n.js` ira chercher au premier passage en anglais.
  #
  # L'invariant testé n'est pas la FORME de la balise, c'est que la charge
  # utile n'est plus dans la page : une assertion sur `<script src=` figeait
  # l'ancienne conception et a interdit le correctif jusqu'ici.
  tg <- as.character(hstat_i18n_script("en"))
  expect_true(grepl("HSTAT_I18N_SRC", tg, fixed = TRUE))
  expect_true(grepl(src, tg, fixed = TRUE))
  # Le dictionnaire pese des dizaines de milliers de caracteres ; la balise
  # n'en fait que quelques dizaines.
  expect_lt(nchar(tg), 400L)
  expect_gt(nchar(hstat_i18n_payload("en")), 10000L)
})

test_that("la charge utile est du pur ASCII, et elle se relit", {
  p <- hstat_i18n_payload("en")

  # PUR ASCII, et ce n'est pas un detail de confort. Un <script src> classique
  # herite du jeu de caracteres du DOCUMENT : un dictionnaire dont les accents
  # ne tiendraient que par cet heritage se corromprait au premier navigateur ou
  # au premier mandataire qui en decide autrement. Echapper coute +1,7 % apres
  # gzip, mesure, et supprime la question.
  expect_false(any(utf8ToInt(p) > 127L))

  # Et il reste du JSON valide : les cles accentuees se relisent a l'identique.
  skip_if_not_installed("jsonlite")
  d <- jsonlite::fromJSON(sub(";$", "", sub("^window\\.HSTAT_I18N = ", "", p)))
  ref <- hstat_i18n_dict("en")
  ref <- ref[names(ref) != unname(ref)]
  ref <- ref[!grepl(HSTAT_I18N_MARQUEUR, names(ref))]
  expect_equal(length(d), length(ref))
  acc <- names(ref)[grepl("[^ -~]", names(ref))]
  expect_gt(length(acc), 100L)
  for (k in utils::head(acc, 20L)) expect_identical(d[[k]], unname(ref[[k]]))
})

test_that("les paires de substitution sont produites au-dela du plan de base", {
  # `\uXXXX` ne couvre que le plan multilingue de base. Un emoji glisse dans une
  # traduction -- ils existent dans les libelles d'interface modernes -- sortirait
  # en caractere de remplacement sans la paire de substitution.
  expect_identical(.hstat_i18n_ascii("été"), "e\\u0301t\\u00e9")
  expect_identical(.hstat_i18n_ascii("abc"), "abc")
  expect_identical(.hstat_i18n_ascii(""), "")
  # U+1F4CA (graphique) = D83D DCCA
  expect_identical(.hstat_i18n_ascii("\U0001F4CA"), "\\ud83d\\udcca")
  expect_identical(.hstat_i18n_ascii("a\U0001F4CAbé"), "a\\ud83d\\udccab\\u00e9")
})


# ============================================================================
#  LES DATES NE DEPENDENT PLUS DE LA LOCALE DU SYSTEME
# ============================================================================

# Signale a l'ecran : axe regle sur « JJ Mois », donnees d'aout 2026, et
# l'etiquette sortait « 04 August » dans une interface entierement francaise.
# `%B` et `%b` lisent LC_TIME ; l'application ne pose que LC_CTYPE au demarrage,
# et c'est deliberement peu -- poser LC_TIME en francais donnerait des mois
# FRANCAIS a un utilisateur qui a choisi l'anglais. Une application bilingue ne
# peut pas faire dependre sa langue d'un reglage du systeme d'exploitation.
test_that("le nom du mois suit la langue de la session, jamais LC_TIME", {
  d <- as.Date("2026-08-04")

  # Le defaut d'origine, reproduit : il faut que la locale du banc soit
  # DISCERNABLE du francais, sinon l'assertion passerait avec ou sans
  # correctif -- la lecon du « precision et rappel coincident sur une matrice
  # equilibree ».
  skip_if(grepl("^fr", Sys.getlocale("LC_TIME"), ignore.case = TRUE),
          "locale deja francaise : le defaut ne serait pas discernable")
  expect_false(identical(format(d, "%d %B"), "04 août"))

  expect_identical(hstat_date_fmt(d, "%d %B", "fr"), "04 août")
  expect_identical(hstat_date_fmt(d, "%d %B", "en"), "04 August")
  expect_identical(hstat_date_fmt(d, "%d %B %Y", "fr"), "04 août 2026")
  expect_identical(hstat_date_fmt(d, "%B %Y", "fr"), "août 2026")
  expect_identical(hstat_date_fmt(d, "%d-%b-%Y", "fr"), "04-août-2026")
  expect_identical(hstat_date_fmt(d, "%d-%b-%Y", "en"), "04-Aug-2026")

  # Le jour de la semaine est indexe par %u (1 = lundi). Indexe par %w
  # (0 = dimanche) il decalerait TOUS les noms d'un rang -- une faute qui rend
  # un nom de jour parfaitement plausible, et faux.
  expect_identical(hstat_date_fmt(d, "%A", "fr"), "mardi")
  expect_identical(hstat_date_fmt(d, "%A", "en"), "Tuesday")
  expect_identical(hstat_date_fmt(as.Date("2026-08-09"), "%A", "fr"), "dimanche")

  # Vectorise, et un NA reste un NA plutot que de devenir une date plausible.
  v <- as.Date(c("2026-08-04", "2026-11-17", NA))
  expect_identical(hstat_date_fmt(v, "%d %B", "fr"),
                   c("04 août", "17 novembre", NA))

  # Un format sans nom de mois ne passe par aucune table : les codes chiffres
  # ne dependent d'aucune locale, il n'y a rien a corriger.
  expect_identical(hstat_date_fmt(d, "%d/%m/%Y", "fr"), "04/08/2026")
  expect_identical(hstat_date_fmt(d, "%Y-%m-%d", "en"), "2026-08-04")

  # `%%` est un pour-cent LITTERAL : un gsub("%B", ...) nu abimerait « %%B ».
  expect_identical(hstat_date_fmt(d, "%d %%B", "fr"), "04 %B")
})

test_that("la lecture reconnait les mois des DEUX langues", {
  skip_if(grepl("^fr", Sys.getlocale("LC_TIME"), ignore.case = TRUE),
          "locale deja francaise : le defaut ne serait pas discernable")

  # Le defaut symetrique, reproduit : sous une locale anglaise, une date
  # francaise est ILLISIBLE. Le fichier de l'utilisateur devenait donc
  # inexploitable selon le systeme qui fait tourner l'application.
  expect_true(is.na(as.Date("25-mars-2024", format = "%d-%b-%Y")))

  ref <- as.Date("2024-03-25")
  for (k in c("25-mars-2024", "25-Mars-2024", "25-Mar-2024", "25-MARS-2024"))
    expect_identical(hstat_date_parse(k, "%d-%b-%Y"), ref)
  expect_identical(hstat_date_parse("25 mars 2024", "%d %B %Y"), ref)
  expect_identical(hstat_date_parse("25 March 2024", "%d %B %Y"), ref)
  expect_identical(hstat_date_parse("4 août 2026", "%d %B %Y"),
                   as.Date("2026-08-04"))

  # Du PLUS LONG au plus court : « juillet » avant « juil. », sans quoi le
  # prefixe mordrait d'abord et laisserait « let » derriere lui.
  expect_identical(hstat_date_parse("25-juillet-2024", "%d-%B-%Y"),
                   as.Date("2024-07-25"))
  expect_identical(hstat_date_parse("25-juil.-2024", "%d-%b-%Y"),
                   as.Date("2024-07-25"))

  # Un format chiffre ne passe par aucune table, et une Date deja convertie
  # ressort telle quelle.
  expect_identical(hstat_date_parse("25/03/2024", "%d/%m/%Y"), ref)
  expect_identical(hstat_date_parse(ref, "%d-%b-%Y"), ref)
  expect_true(is.na(hstat_date_parse("pas une date", "%d-%b-%Y")))
})

test_that("les tables de noms sont completes et l'echelle de l'axe les emploie", {
  for (l in c("fr", "en")) {
    expect_length(HSTAT_MOIS[[l]], 12L)
    expect_length(HSTAT_MOIS_ABR[[l]], 12L)
    expect_length(HSTAT_JOURS[[l]], 7L)
    expect_length(HSTAT_JOURS_ABR[[l]], 7L)
    expect_true(all(nzchar(HSTAT_MOIS[[l]])))
    expect_false(any(duplicated(HSTAT_MOIS[[l]])))
    # Aucun nom ne porte de « % » : c'est ce qui permet de l'injecter dans le
    # format sans qu'il puisse etre relu comme un code.
    expect_false(any(grepl("%", c(HSTAT_MOIS[[l]], HSTAT_MOIS_ABR[[l]],
                                  HSTAT_JOURS[[l]], HSTAT_JOURS_ABR[[l]]),
                           fixed = TRUE)))
  }
  # « mars », « mai » et « juin » ne s'abregent pas : ecrire « mars. » serait
  # une faute, pas une abreviation.
  expect_identical(HSTAT_MOIS_ABR[["fr"]][c(3L, 5L, 6L)], c("mars", "mai", "juin"))

  # Et c'est bien l'echelle de l'axe qui les emploie : le correctif serait sans
  # effet s'il restait dans le socle sans que le graphique l'appelle.
  u <- paste(.hstat_code_lignes(file.path(.hstat_repo_root(), "R", "utils.R")),
             collapse = "\n")
  cur <- sub(".*viz_get_x_scale <- function", "", u)
  cur <- substr(cur, 1, 2500)
  expect_true(grepl("hstat_date_fmt(", cur, fixed = TRUE))
  expect_false(grepl("format(as.Date(v), disp_fmt)", cur, fixed = TRUE))
})


# ============================================================================
#  DIVERSITE ECOLOGIQUE
# ============================================================================

# Le jeu d'essai des tests de diversite : cinq relevés dont les profils sont
# DELIBEREMENT contrastés — un peuplement dominé (R3), un parfaitement régulier
# (R4), un intermédiaire. Sur des relevés qui se ressemblent, la moitié des
# assertions passeraient en ignorant complètement l'indice mesuré.
.hstat_div_jeu <- function() {
  m <- matrix(c(
    50, 20, 10,  5,  3,  2,  1,  1,
    40, 30, 15,  8,  4,  2,  1,  0,
    90,  5,  2,  1,  1,  1,  0,  0,
    12, 12, 12, 12, 12, 12, 12, 12,
    30, 25, 20, 15, 10,  0,  0,  0), nrow = 5, byrow = TRUE)
  rownames(m) <- paste0("R", 1:5); colnames(m) <- paste0("sp", 1:8)
  m
}

test_that("les indices alpha reproduisent vegan au chiffre près", {
  skip_if_not_installed("vegan")
  m <- .hstat_div_jeu()
  # LE SOCLE N'APPELLE PAS VEGAN : il calcule tout en R de base, pour que les
  # indices ne dépendent d'aucun paquet optionnel. Ce test est donc une
  # CONFRONTATION, pas une vérification de plomberie — il attraperait une
  # formule fautive que rien d'autre ne verrait.
  for (i in seq_len(nrow(m))) {
    a <- hstat_div_indices(m[i, ], "e")
    expect_equal(a$Shannon_H, unname(vegan::diversity(m[i, ], "shannon")),
                 tolerance = 1e-10, info = rownames(m)[i])
    expect_equal(a$Simpson_1_D, unname(vegan::diversity(m[i, ], "simpson")),
                 tolerance = 1e-10, info = rownames(m)[i])
    expect_equal(a$Simpson_inverse, unname(vegan::diversity(m[i, ], "invsimpson")),
                 tolerance = 1e-10, info = rownames(m)[i])
    expect_equal(a$Fisher_alpha, unname(vegan::fisher.alpha(m[i, ])),
                 tolerance = 1e-4, info = rownames(m)[i])
  }
  # Et le jeu d'essai DISCERNE les relevés : sans cela, une formule qui rendrait
  # la même valeur partout passerait toutes les assertions ci-dessus.
  h <- vapply(seq_len(nrow(m)), function(i) hstat_div_indices(m[i, ], "2")$Shannon_H, 0)
  expect_gt(diff(range(h)), 1)
})

test_that("les estimateurs de richesse reproduisent vegan", {
  skip_if_not_installed("vegan")
  m <- .hstat_div_jeu()
  for (i in seq_len(nrow(m))) {
    r <- hstat_div_richesse(m[i, ])
    v <- vegan::estimateR(m[i, ])
    expect_equal(r$Sobs, unname(v["S.obs"]), info = rownames(m)[i])
    # `S.chao1` de vegan EST la forme corrigée du biais : c'est elle qu'on
    # affiche en premier, parce qu'elle reste définie quand F2 = 0 — le cas
    # fréquent d'un petit inventaire, où la forme de 1984 divise par zéro.
    expect_equal(r$Chao1_corrige, unname(v["S.chao1"]), tolerance = 1e-8,
                 info = rownames(m)[i])
    expect_equal(r$ACE, unname(v["S.ACE"]), tolerance = 1e-6, info = rownames(m)[i])
    # `as.numeric` des deux cotes, et pas `unname` : `vegan::rarefy` accroche
    # la taille du sous-echantillon en ATTRIBUT (`Subsample`), que `unname` ne
    # retire pas -- l'assertion echouait sur un attribut alors que les valeurs
    # coincidaient au dixieme de milliardieme. C'est bien la VALEUR qu'on
    # confronte.
    expect_equal(as.numeric(hstat_div_rarefaction(m[i, ], 20)),
                 as.numeric(vegan::rarefy(m[i, ], 20)), tolerance = 1e-8,
                 info = rownames(m)[i])
  }
  # F2 = 0 : la forme de 1984 n'est pas définie, la corrigée l'est toujours.
  x <- c(10, 5, 3, 1, 1, 1)
  expect_true(is.na(hstat_div_richesse(x)$Chao1))
  expect_true(is.finite(hstat_div_richesse(x)$Chao1_corrige))
  # La borne basse de l'intervalle ne descend JAMAIS sous ce qu'on a vu : un
  # intervalle symétrique le fait dès que la variance est grande, et « moins
  # d'espèces que celles comptées » ne veut rien dire.
  r <- hstat_div_richesse(.hstat_div_jeu()[1, ])
  expect_gte(r$Chao1_IC_inf, r$Sobs)
  expect_gte(r$Chao1_IC_sup, r$Chao1_corrige)
})

test_that("les coefficients bêta reproduisent vegan", {
  skip_if_not_installed("vegan")
  m <- .hstat_div_jeu()
  cas <- list(
    list("jaccard",  vegan::vegdist(m, "jaccard", binary = TRUE)),
    list("sorensen", vegan::vegdist(m, "bray",    binary = TRUE)),
    list("bray",     vegan::vegdist(m, "bray")),
    list("horn",     vegan::vegdist(m, "horn")),
    list("morisita", vegan::vegdist(m, "morisita")))
  for (k in cas) {
    nous <- hstat_div_beta(m, k[[1]], "dissimilarite")
    expect_equal(max(abs(nous - as.matrix(k[[2]]))), 0, tolerance = 1e-10,
                 info = k[[1]])
  }
  # SIMILARITE ET DISSIMILARITE SONT COMPLEMENTAIRES, et les confondre inverse
  # la conclusion : 0,80 se lit « très semblables » d'un côté et « très
  # différents » de l'autre.
  s <- hstat_div_beta(m, "jaccard", "similarite")
  d <- hstat_div_beta(m, "jaccard", "dissimilarite")
  expect_equal(max(abs(s + d - 1)), 0, tolerance = 1e-12)
  expect_error(hstat_div_beta(m[1, , drop = FALSE]), "deux")
})

test_that("la partition de Baselga est exacte, pas approchée", {
  m <- .hstat_div_jeu()
  b <- hstat_div_baselga(m)
  # Les deux identités sont la RAISON D'ETRE de la partition : si elles ne
  # tiennent pas, les deux composantes ne se somment pas à la dissimilarité et
  # l'interprétation « remplacement contre emboîtement » ne veut plus rien dire.
  expect_equal(max(abs(b$Sorensen_total - b$Simpson_turnover - b$Sorensen_emboitement)),
               0, tolerance = 1e-12)
  expect_equal(max(abs(b$Jaccard_total - b$Jaccard_turnover - b$Jaccard_emboitement)),
               0, tolerance = 1e-12)
  # Deux relevés emboîtés (l'un sous-ensemble strict de l'autre) : TOUT est
  # emboîtement, rien n'est remplacement. C'est le cas qui distingue les deux
  # composantes — sur un jeu quelconque elles se mélangent et l'assertion ne
  # discernerait rien.
  e <- rbind(a = c(1, 1, 1, 1), b = c(1, 1, 0, 0))
  be <- hstat_div_baselga(e)
  expect_equal(be$Simpson_turnover, 0)
  expect_gt(be$Sorensen_emboitement, 0)
  # Et deux relevés de même richesse sans aucune espèce commune : tout est
  # remplacement, rien n'est emboîtement.
  r <- rbind(a = c(1, 1, 0, 0), b = c(0, 0, 1, 1))
  br <- hstat_div_baselga(r)
  expect_equal(br$Simpson_turnover, 1)
  expect_equal(br$Sorensen_emboitement, 0)
})

test_that("Hill, Rényi et Tsallis retombent sur les indices connus", {
  x <- .hstat_div_jeu()[1, ]
  a <- hstat_div_indices(x, "e")
  # Hill (1973) montre que richesse, Shannon et Simpson sont le MEME nombre à
  # trois valeurs de q : c'est cela qui les rend comparables entre eux.
  expect_equal(unname(hstat_div_hill(x, 0)), a$Richesse_S)
  expect_equal(unname(hstat_div_hill(x, 1)), exp(a$Shannon_H), tolerance = 1e-10)
  expect_equal(unname(hstat_div_hill(x, 2)), a$Simpson_inverse, tolerance = 1e-10)
  expect_equal(unname(hstat_div_hill(x, Inf)), a$Berger_Parker_inverse, tolerance = 1e-10)
  expect_equal(unname(hstat_div_renyi(x, 1)), a$Shannon_H, tolerance = 1e-10)
  expect_equal(unname(hstat_div_renyi(x, 0)), log(a$Richesse_S), tolerance = 1e-10)
  expect_equal(unname(hstat_div_tsallis(x, 2)), a$Simpson_1_D, tolerance = 1e-10)
  expect_equal(unname(hstat_div_tsallis(x, 0)), a$Richesse_S - 1, tolerance = 1e-10)
  # Hill décroît avec q par construction : q élevé favorise les dominantes.
  h <- hstat_div_hill(x, c(0, 1, 2, Inf))
  expect_true(all(diff(h) <= 1e-9))
})

test_that("la base du logarithme fait partie du seuil, et le verdict convertit", {
  # LE PIEGE, ET IL EST SILENCIEUX. `vegan::diversity()` calcule en logarithme
  # NATUREL ; la grille de Frontier est en BITS. Appliquer l'une à l'autre ne
  # lève rien et ne laisse aucun vide : cela rend un verdict plausible et faux.
  m <- .hstat_div_jeu()
  x <- m[4, ]                                   # relevé parfaitement régulier
  hb <- hstat_div_indices(x, "2")$Shannon_H     # 3 bits
  hn <- hstat_div_indices(x, "e")$Shannon_H     # 2,079 nats
  expect_equal(hb, 3, tolerance = 1e-10)
  expect_equal(hstat_div_convertir(hb, "2", "e"), hn, tolerance = 1e-12)

  # Le verdict rend la MEME classe quelle que soit la base de calcul.
  expect_identical(hstat_div_verdict("Shannon_H", hb, "2")$libelle,
                   hstat_div_verdict("Shannon_H", hn, "e")$libelle)

  # ET LE JEU D'ESSAI DISCERNE LE DEFAUT : lue sans conversion, la grille en
  # bits classerait ce relevé une classe plus bas. Sur une autre valeur les
  # deux lectures coïncideraient, et l'assertion passerait avec ou sans le
  # correctif — c'est la leçon de « précision et rappel coïncident sur une
  # matrice équilibrée ».
  g <- HSTAT_DIV_SEUILS$Shannon_H
  sans_conversion <- g$libelles[sum(hn >= g$bornes) + 1L]
  expect_false(identical(sans_conversion,
                         hstat_div_verdict("Shannon_H", hn, "e")$libelle))
  expect_identical(hstat_div_verdict("Shannon_H", hb, "2")$libelle, "Élevée")
  expect_identical(sans_conversion, "Moyenne")
})

test_that("chaque grille de seuils est complète et porte son auteur", {
  for (k in names(HSTAT_DIV_SEUILS)) {
    g <- HSTAT_DIV_SEUILS[[k]]
    n <- length(g$bornes) + 1L
    expect_length(g$etats, n)
    expect_length(g$libelles, n)
    expect_length(g$interpretations, n)
    expect_true(all(g$etats %in% c("ok", "warn", "err")), info = k)
    expect_true(!is.unsorted(g$bornes), info = k)
    # UN SEUIL SANS SON AUTEUR N'EST PAS INTERPRETABLE par le lecteur du
    # rapport : les quatre voyagent ensemble.
    expect_true(nzchar(g$indice) && nzchar(g$auteur) && nzchar(g$reference), info = k)
    expect_gt(nchar(g$reference), 60L)
    # ET L'ORIGINE EST DECLAREE. Un seuil présenté comme publié alors qu'il
    # relève de l'usage est exactement le genre d'affirmation qu'un rapport
    # recopie sans la vérifier.
    expect_true(g$origine %in% c("primaire", "usage"), info = k)
    expect_true(all(nzchar(g$interpretations)), info = k)
    # La base est déclarée, ou explicitement sans objet.
    expect_true(is.na(g$base) || g$base %in% HSTAT_DIV_BASES, info = k)
  }
  # Les deux origines existent réellement dans le catalogue : si tout était
  # « usage », la colonne ne distinguerait rien.
  org <- vapply(HSTAT_DIV_SEUILS, function(g) g$origine, "")
  expect_true(all(c("primaire", "usage") %in% org))
  # Magurran est le cadrage publié ; Frontier la convention d'usage.
  expect_identical(HSTAT_DIV_SEUILS$Shannon_H_nats$origine, "primaire")
  expect_identical(HSTAT_DIV_SEUILS$Shannon_H$origine, "usage")
})

test_that("aucun indice ne branche sur une valeur non calculable", {
  # UN PEUPLEMENT MONOSPECIFIQUE N'A PAS D'EQUITABILITE : H' = 0 et Hmax = 0,
  # donc J = 0/0. `NaN` traverserait toutes les sorties et ferait lever la
  # moindre condition posée dessus ; NA dit ce qui est le cas.
  a <- hstat_div_indices(c(10, 0, 0), "2")
  expect_equal(a$Shannon_H, 0)
  expect_true(is.na(a$Pielou_J))
  expect_false(is.nan(a$Pielou_J %||% NA_real_))
  for (k in c("Heip_E", "Camargo_E", "Smith_Wilson_Evar", "Simpson_E"))
    expect_true(is.na(a[[k]]), info = k)

  # LES ESTIMATEURS COMPTENT DES INDIVIDUS. Sur des recouvrements ou des
  # biomasses, « singleton » n'a aucun sens : on rend NA plutôt qu'un Chao1
  # calculé sur des décimales arrondies, qui serait plausible et faux.
  d <- hstat_div_richesse(c(2.5, 1.3, 0.7))
  expect_equal(d$Sobs, 3)
  for (k in c("Chao1", "Chao1_corrige", "ACE", "Jackknife1", "Bootstrap",
              "Couverture_Good"))
    expect_true(is.na(d[[k]]), info = k)
  expect_true(is.na(hstat_div_rarefaction(c(2.5, 1.3), 2)))

  # Et le verdict ne lève pas : il rend le quatrième état.
  v <- hstat_div_verdict("Pielou_J", NA_real_)
  expect_identical(v$etat, "indeterminable")
  expect_true(nzchar(v$auteur))
  expect_true(nzchar(v$grille))
  expect_identical(hstat_div_verdict("indice inconnu", 1)$etat, "indeterminable")
})

test_that("la matrice relevés × espèces se construit des deux formes", {
  long <- data.frame(
    site = c("A", "A", "B", "B", "B"),
    esp  = c("sp1", "sp2", "sp1", "sp2", "sp3"),
    n    = c(10, 5, 7, 0, 3), stringsAsFactors = FALSE)
  m <- hstat_div_matrice(long, "long", "site", "esp", "n")
  expect_equal(dim(m), c(2L, 3L))
  expect_equal(unname(m["A", "sp1"]), 10)
  expect_equal(unname(m["B", "sp3"]), 3)

  # SANS COLONNE D'EFFECTIF, CHAQUE LIGNE EST UN INDIVIDU — et c'est une
  # hypothèse, donc elle se dit. Un fichier portant une colonne d'effectif non
  # sélectionnée serait sinon compté à raison d'une observation par ligne.
  m2 <- hstat_div_matrice(long, "long", "site", "esp", NULL)
  expect_equal(unname(m2["A", "sp1"]), 1)
  expect_true(grepl("individu", attr(m2, "message")))

  # Forme large, et deux lignes de même relevé AGREGEES : deux lignes homonymes
  # compteraient sinon pour deux sites dans toute la diversité bêta.
  large <- data.frame(site = c("A", "A", "B"),
                      sp1 = c(1, 2, 5), sp2 = c(0, 3, 1),
                      stringsAsFactors = FALSE)
  ml <- hstat_div_matrice(large, "large", var_site = "site",
                          var_especes = c("sp1", "sp2"))
  expect_equal(dim(ml), c(2L, 2L))
  expect_equal(unname(ml["A", "sp1"]), 3)
  expect_true(grepl("addition", attr(ml, "message")))

  # Un effectif négatif est écarté ET compté : le taire ferait disparaître une
  # ligne sans cause visible.
  neg <- data.frame(site = "A", esp = c("sp1", "sp2"), n = c(5, -3),
                    stringsAsFactors = FALSE)
  mn <- hstat_div_matrice(neg, "long", "site", "esp", "n")
  expect_equal(ncol(mn), 1L)
  expect_true(grepl("négatif", attr(mn, "message")))

  expect_error(hstat_div_matrice(data.frame(), "long"), "agr")
  expect_error(hstat_div_matrice(long, "long", "site", NULL, "n"), "espèces")
})

test_that("les abondances relatives somment à 100 et l'ordre suit l'abondance", {
  m <- .hstat_div_jeu()
  a <- hstat_div_abondance(m)
  expect_equal(sum(a$Abondance_relative_pct), 100, tolerance = 1e-9)
  expect_equal(sum(a$Abondance), sum(m))
  expect_false(is.unsorted(rev(a$Abondance)))
  expect_equal(a$Rang[1], 1)
  # L'occurrence est un COMPTAGE DE RELEVES, pas une somme d'effectifs : les
  # deux répondent à deux questions, et les confondre ferait passer une espèce
  # rare mais omniprésente pour une espèce abondante.
  expect_true(all(a$Occurrences <= nrow(m)))
  expect_equal(a$Occurrences[a$Espece == "sp1"], 5L)

  # Par relevé, chaque ligne somme à 100.
  s <- hstat_div_abondance_site(m)
  expect_equal(unname(rowSums(s[, -1, drop = FALSE])), rep(100, nrow(m)),
               tolerance = 1e-9)
})

test_that("le tableau d'interprétation porte grille, auteur, origine et référence", {
  m <- .hstat_div_jeu()
  ind <- cbind(hstat_div_indices(colSums(m), "2"),
               hstat_div_richesse(colSums(m))[, c("Couverture_Good", "Completude")])
  d <- hstat_div_interpreter(ind, "2")
  expect_gt(nrow(d), 5L)
  for (k in c("Indice", "Valeur", "Classe", "Interpretation", "Grille",
              "Auteur", "Origine", "Reference"))
    expect_true(k %in% names(d), info = k)
  expect_true(all(nzchar(d$Auteur)))
  expect_true(all(nzchar(d$Reference)))
  expect_true(all(d$Origine %in% c("Seuils publiés par l'auteur", "Convention d'usage")))
  # La clé « Shannon_H_nats » lit la MEME colonne que « Shannon_H » : ce sont
  # deux lectures d'une seule valeur, pas deux valeurs.
  sh <- d[grepl("^Indice de Shannon", d$Indice), ]
  expect_equal(nrow(sh), 2L)
  expect_equal(hstat_div_convertir(sh$Valeur[1], "2", "e"), sh$Valeur[2],
               tolerance = 1e-9)
})

test_that("le module de diversité respecte les conventions du dépôt", {
  chemin <- .hstat_module_path("mod_diversity.R")
  skip_if(is.na(chemin) || !file.exists(chemin))
  l <- .hstat_code_lignes(chemin)
  src <- paste(l, collapse = "\n")
  # LE MODULE NE CALCULE RIEN : une statistique posée dans un `observeEvent`
  # n'est pas testable, et c'est la raison d'être de la règle.
  expect_false(grepl("function\\s*\\(x[^)]*\\)\\s*\\{[^}]*sum\\(p \\* log", src))
  # Il passe par le socle.
  for (f in c("hstat_div_matrice", "hstat_div_indices", "hstat_div_richesse",
              "hstat_div_beta", "hstat_div_interpreter", "hstat_div_abondance"))
    expect_true(grepl(f, src, fixed = TRUE), info = f)
  # Il dépose son contexte, sans quoi il manquerait à l'onglet
  # d'interprétation, au journal de reproductibilité et au rapport.
  # Il prend les kits partagés plutôt que de recopier une douzième fois.
  for (f in c("hstat_export_plot_ui", "hstat_export_plot_handler",
              "hstat_export_tables_handlers", "hstat_plot_extras_ui",
              "hstat_plot_extras_lire", "hstat_plot_extras_theme",
              "hstat_palettes_choix", "hstat_scales_palette", "hstat_axe_titre_ui"))
    expect_true(grepl(f, src, fixed = TRUE), info = f)
  # LA BOITE N'EXISTE QUE QUAND ELLE PORTE QUELQUE CHOSE, et le drapeau ne se
  # suspend pas — sans quoi elle ne réapparaîtrait jamais.
  expect_true(grepl('condition = "output.hasDiv"', src, fixed = TRUE))
  expect_true(grepl('outputOptions(output, "hasDiv", suspendWhenHidden = FALSE)',
                    src, fixed = TRUE))
  # Et il est branché dans l'interface comme dans le serveur.
  root <- .hstat_repo_root()
  ux <- paste(readLines(file.path(root, "inst", "app", "UX.R"), warn = FALSE,
                        encoding = "UTF-8"), collapse = "\n")
  sv <- paste(readLines(file.path(root, "inst", "app", "app_server.R"), warn = FALSE,
                        encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl('mod_diversity_ui("diversity")', ux, fixed = TRUE))
  expect_true(grepl('tabName = "diversity"', ux, fixed = TRUE))
  # L'appel porte desormais un TROISIEME argument : la graine de l'en-tete.
  # Le module lisait `values$globalSeed`, un champ que rien n'ecrit, et restait
  # donc fige sur 123. L'assertion suit la correction plutot que d'epingler
  # l'ancienne forme.
  expect_true(grepl('mod_diversity_server("diversity", values,', sv, fixed = TRUE))
})


test_that("les huit figures du module de diversité se tracent réellement", {
  # LE DEFAUT QUE CE TEST GARDE, mesure au navigateur : sept figures sur huit
  # s'affichaient, et la huitieme -- la carte de chaleur de dissimilarite --
  # rendait « Continuous value supplied to a discrete scale » a la place de
  # l'image. La palette choisie par l'utilisateur etait posee sur TOUTES les
  # figures ; sur une echelle de remplissage CONTINUE, `scale_fill_brewer`
  # remplace le degrade par une echelle discrete et ggplot refuse de tracer.
  #
  # Aucun test n'exercait le reactif du graphique : le catalogue etait verifie
  # (huit entrees), le constructeur ne l'etait pas. Ce test CONSTRUIT les huit
  # figures et exige que chacune passe `ggplot_build` -- c'est la seule etape
  # ou l'incompatibilite des echelles se manifeste, une figure mal composee se
  # laissant assembler sans un mot.
  skip_if_not_installed("ggplot2")
  root <- .hstat_repo_root()
  skip_if(is.na(root))

  # Un jeu en forme longue, la forme d'une fiche de terrain.
  d <- data.frame(
    Parcelle = rep(c("P1", "P2", "P3"), each = 5),
    Espece   = rep(paste0("sp", 1:5), times = 3),
    Abondance = c(40, 20, 10, 5, 1,
                  12, 12, 11, 10, 9,
                  60,  3,  2, 1, 0),
    stringsAsFactors = FALSE)
  vals <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d)

  shiny::testServer(mod_diversity_server, args = list(values = vals), {
    session$setInputs(divFormat = "long", divSite = "Parcelle",
                      divEspece = "Espece", divAbondance = "Abondance",
                      divBase = "2")
    session$setInputs(divCalculer = 1)
    expect_false(is.null(rv$mat))
    expect_equal(nrow(rv$mat), 3L)

    # La palette est celle qui declenchait le defaut : une palette Brewer, donc
    # une echelle DISCRETE. Avec la palette par defaut de ggplot2 (`hue`) le
    # defaut se manifeste de la meme facon, mais choisir Brewer rend le cas
    # explicite -- c'est celui qu'un utilisateur selectionne pour ses figures.
    session$setInputs(divPalette = "Set2", divBetaMethode = "jaccard",
                      divBetaSortie = "similarite", divIndiceTrace = "Shannon_H",
                      divRangLog = TRUE, divAbondTop = 20, divAccumPerm = 20)

    for (f in unname(HSTAT_DIV_GRAPHIQUES)) {
      session$setInputs(divGraphique = f)
      p <- graphique()
      expect_s3_class(p, "ggplot")
      # `ggplot_build` est l'etape qui resout les echelles : c'est LA seule qui
      # voit un degrade remplace par une palette qualitative.
      expect_error(ggplot2::ggplot_build(p), NA, info = f)
    }
  })
})

test_that("une figure a echelle continue ne recoit pas de palette qualitative", {
  # Le pendant de l'assertion precedente, et il tient a un detail : sans la
  # liste `HSTAT_DIV_GRAPHIQUES_CONTINUS`, la seule figure concernee serait
  # reconnue par son nom ecrit en dur dans le reactif -- une neuvieme figure a
  # degrade ajoutee demain retrouverait le defaut sans que rien ne le dise.
  expect_true(exists("HSTAT_DIV_GRAPHIQUES_CONTINUS"))
  # Ce qu'elle nomme existe bien au catalogue : une entree obsolete n'exclurait
  # plus rien, et c'est exactement le genre de derive muette que ce depot
  # traque -- la liste protegerait une figure qui n'existe plus.
  expect_true(all(HSTAT_DIV_GRAPHIQUES_CONTINUS %in% unname(HSTAT_DIV_GRAPHIQUES)))
  # Et elle ne les nomme pas TOUTES : sinon la palette de l'utilisateur ne
  # serait jamais posee sur aucune figure, et le reglage n'agirait plus.
  expect_lt(length(HSTAT_DIV_GRAPHIQUES_CONTINUS), length(HSTAT_DIV_GRAPHIQUES))
})

test_that("un comptage s'affiche sans decimales", {
  # LE DEFAUT QUE CE TEST GARDE, mesure a l'ecran : le tableau de richesse
  # annoncait « 392.000 individus » et « 12.000 especes » a cote de « 9.500 ».
  # `DT::formatRound` etait applique a TOUTES les colonnes numeriques du module,
  # donc aussi aux comptages. On doute d'un entier affiche comme s'il avait
  # trois decimales -- le defaut deja corrige sur le tableau de la DL50
  # (« 5.00000 degres de liberte »).
  #
  # LEÇON DE METHODE, prise par mutation. La premiere version de cette
  # assertion verifiait que `hstat_div_richesse()` rend bien des comptages
  # ENTIERS -- ce qui est vrai, et n'a rien a voir : le defaut etait dans
  # l'AFFICHAGE. Reposer `formatRound` sur toutes les colonnes ne la faisait
  # donc pas echouer. La regle a ete sortie du `moduleServer` (ou rien n'est
  # testable) vers le socle, et c'est elle qu'on verifie maintenant.
  d <- data.frame(
    Releve     = c("A", "B"),                 # texte : jamais arrondi
    Individus  = c(118, 26),                  # comptage
    Sobs       = c(9L, 7L),                   # comptage, deja entier de type
    Chao1      = c(9.5, 7.5),                 # estimation : fractionnaire
    Vide       = c(NA_real_, NA_real_),       # rien d'observe
    Trouee     = c(12, NA_real_),             # un NA au milieu d'entiers
    stringsAsFactors = FALSE)
  e <- hstat_cols_entieres(d)
  # Le jeu croise les deux traitements DANS LE MEME tableau : sans une colonne
  # fractionnaire a cote, une fonction qui supprimerait toutes les decimales
  # passerait l'assertion ; sans une colonne entiere, l'inverse.
  expect_false(e[["Releve"]])
  expect_true(e[["Individus"]])
  expect_true(e[["Sobs"]])
  expect_false(e[["Chao1"]])
  # Une colonne entierement manquante n'est pas « entiere » : il n'y a rien a
  # arrondir, et la dire entiere ferait afficher « NA » sous un format d'entier
  # sans que ce soit un choix.
  expect_false(e[["Vide"]])
  # Un NA au milieu d'entiers n'empeche pas la colonne d'en etre une.
  expect_true(e[["Trouee"]])

  # ET C'EST L'EFFET QUI SE VERIFIE, jamais le texte du module. Une premiere
  # version comptait les appels a `DT::formatRound` dans le source -- et ce
  # compte etait satisfait par un appel SANS RAPPORT pose ailleurs dans le meme
  # fichier : la mutation passait. Une assertion qui ne distingue pas les deux
  # codes ne garde rien. On lit donc les decimales que DT pose REELLEMENT sur
  # chaque colonne, dans la definition qu'il engendre.
  skip_if_not_installed("DT")
  dt <- hstat_dt_arrondi(DT::datatable(d, rownames = FALSE), d, digits = 3)
  dec <- stats::setNames(rep(NA_integer_, ncol(d)), names(d))
  for (cd in dt$x$options$columnDefs) {
    if (is.null(cd$render)) next
    m <- regmatches(cd$render, regexpr("formatRound\\(data, [0-9]+", cd$render))
    if (!length(m)) next
    n <- as.integer(sub(".*, ", "", m))
    for (t in unlist(cd$targets)) dec[[as.integer(t) + 1L]] <- n
  }
  expect_equal(unname(dec[["Individus"]]), 0L)
  expect_equal(unname(dec[["Sobs"]]),      0L)
  expect_equal(unname(dec[["Trouee"]]),    0L)
  # L'estimation, elle, garde les decimales demandees : c'est ce qui rend
  # l'assertion discernante -- une fonction qui arrondirait tout a zero, ou qui
  # ne distinguerait rien, en echouerait.
  expect_equal(unname(dec[["Chao1"]]),     3L)
  # Et une colonne de texte n'est pas arrondie du tout.
  expect_true(is.na(dec[["Releve"]]))
})


test_that("le format de date choisi agit sur TOUS les types de graphique", {
  # LE DEFAUT QUE CE TEST GARDE, signale a l'ecran avec sa capture : le reglage
  # « Format d'affichage sur l'axe » etait regle sur « MM-JJ » et l'axe d'un
  # nuage de points sortait « Aug / Sep / Oct » -- les graduations automatiques
  # de ggplot, et en anglais par-dessus le marche.
  #
  # La cause n'etait PAS dans l'echelle : `viz_get_x_scale()` rendait deja les
  # bonnes etiquettes, et un test isole l'aurait montre. Elle etait dans sa
  # POSE : chaque constructeur devait l'ajouter lui-meme, et TROIS sur quatorze
  # le faisaient. Les onze autres n'affichaient rien du reglage -- ni le
  # format, ni le renommage des etiquettes, ni l'ordre personnalise, qui
  # voyagent tous par cette meme fonction.
  #
  # C'est pourquoi ce test porte sur les TYPES, un par un, et pas sur la
  # fonction : c'est la seule forme qui distingue « l'echelle sait formater »
  # de « le graphique la porte ».
  skip_if_not_installed("ggplot2")
  set.seed(1)
  d <- data.frame(
    Semaine = rep(as.Date("2026-08-04") + 7 * (0:3), each = 3),
    Valeur  = stats::rpois(12, 3),
    Site    = rep(c("A", "B", "C"), 4),
    stringsAsFactors = FALSE)
  vals <- shiny::reactiveValues(
    data = d, cleanData = d, filteredData = d,
    storedLevelLabels = list(), customXOrder = NULL)

  # Les types A AXE X CARTESIEN. Camembert, anneau et treemap en sont absents
  # a dessein : ils n'ont pas d'axe des abscisses.
  types <- c("scatter", "line", "bar", "box", "violin", "area",
             "histogram", "density", "heatmap")
  attendu <- c("08-04", "08-11", "08-18", "08-25")

  shiny::testServer(mod_viz_server, args = list(values = vals), {
    for (tt in types) {
      session$setInputs(vizXVar = "Semaine", vizYVar = "Valeur",
                        xVarType = "date", vizType = tt,
                        xDateDisplayFormat = "%m-%d", xDateFormat = "%Y-%m-%d")
      p <- createPlot()
      expect_s3_class(p, "ggplot")
      # `ggplot_build` est l'etape qui resout l'echelle : c'est la seule ou
      # l'etiquette reellement affichee existe. Lire `p$scales` dirait qu'une
      # echelle est presente sans dire ce qu'elle rend.
      lab <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
      expect_setequal(lab, attendu)
    }
  })
})

test_that("une modalite qui ressemble a une date n'est jamais reecrite", {
  # Le pendant du test precedent, et c'est lui qui borne le correctif. La
  # branche discrete de l'echelle reformate les niveaux qui SONT des dates --
  # les barres et l'histogramme passent X en facteur, leurs niveaux sont donc
  # des dates ecrites. Elle ne doit toucher a rien d'autre : reecrire une
  # modalite de l'utilisateur serait le pire defaut possible pour un outil de
  # statistique, et il serait muet.
  skip_if_not_installed("ggplot2")
  etiquettes <- function(x, fmt = "%m-%d", label_map = NULL) {
    sc <- viz_get_x_scale(x, disp_fmt = fmt, label_map = label_map)
    df <- data.frame(x = x, y = seq_along(x))
    p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() + sc
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  }

  # Trois familles de cas limites, et c'est la troisieme qui compte vraiment.
  #
  # `as.Date(v, "%Y-%m-%d")` est TOLERANTE d'une facon qui surprend : mesuree,
  # elle accepte « 2026-08-04 bloc A » et rend le 4 aout, en jetant le suffixe
  # SANS UN MOT. Un critere qui se contenterait de « as.Date ne rend pas NA »
  # reecrirait donc cette modalite en « 08-04 », et « bloc A » disparaitrait de
  # l'axe -- une donnee de l'utilisateur alteree, le pire defaut possible pour
  # un outil de statistique, et parfaitement muet.
  #
  # D'ou le controle de FORME en plus du controle de valeur. La premiere
  # version de ce test ne portait que sur « T1 » et « 2024-13-45 », que
  # `as.Date` refuse d'elle-meme : retirer le controle de forme ne la faisait
  # pas echouer. Une assertion qui ne distingue pas les deux codes ne garde
  # rien -- c'est la lecon deja prise trois fois dans ce depot.
  brut <- c(
    "T1", "T2", "2024", "Rdt-2023",   # rien d'une date : `as.Date` refuse
    "2024-13-45",                      # la forme d'une date, mois 13 : refusee
    "2026-08-04 bloc A",               # ACCEPTEE par `as.Date`, et pourtant
    "2026-08-04X",                     # une modalite : seule la forme les sauve
    "2024-1-5")                        # date sans zeros : hors ISO strict
  expect_setequal(etiquettes(factor(brut)), brut)

  # Une vraie date, elle, suit le format demande.
  expect_setequal(etiquettes(factor(c("2026-08-04", "2026-08-11"))),
                  c("08-04", "08-11"))

  # Le renommage explicite PRIME sur le format : l'utilisateur qui a nomme son
  # niveau « Semaine 1 » ne veut pas le voir redevenir une date.
  expect_setequal(
    etiquettes(factor(c("2026-08-04", "2026-08-11")),
               label_map = list("2026-08-04" = "Semaine 1",
                                "2026-08-11" = "Semaine 2")),
    c("Semaine 1", "Semaine 2"))

  # Et le nom du mois suit la langue de la session, pas LC_TIME -- dans la
  # branche discrete comme dans la branche date.
  expect_setequal(etiquettes(as.Date(c("2026-08-04", "2026-09-01")), "%d %B"),
                  c("04 ao\u00fbt", "01 septembre"))
  expect_setequal(etiquettes(factor(c("2026-08-04", "2026-09-01")), "%d %B"),
                  c("04 ao\u00fbt", "01 septembre"))
})

# -----------------------------------------------------------------------------
# RENDEMENT : LA LIGNE DU ZERO, ET LES MODALITES QU'ON NE VEUT PAS VOIR
# -----------------------------------------------------------------------------
test_that("la ligne du zero est UN reglage a trois etats", {
  skip_if_not_installed("ggplot2")
  # Deux cases a cocher decriraient le MEME trait par deux commandes qui
  # peuvent se contredire : c'est la seconde, invisible, qui finirait par
  # mentir. Le choix est donc unique.
  expect_setequal(unname(HSTAT_AXE_ZERO), c("aucune", "reference", "axe"))

  aucune <- hstat_axe_zero("aucune")
  expect_length(aucune$couches, 0L)
  expect_true(identical(aucune$expand, ggplot2::waiver()))

  # « reference » garde EXACTEMENT le comportement d'origine : un repere
  # pointille, et rien d'autre -- surtout pas l'effacement du trait d'axe.
  ref <- hstat_axe_zero("reference")
  expect_length(ref$couches, 1L)
  expect_true(inherits(ref$couches[[1]], "Layer"))
  expect_true(identical(ref$expand, ggplot2::waiver()))
  expect_length(Filter(function(x) inherits(x, "theme"), ref$couches), 0L)

  # Un mode inconnu retombe sur le repere, jamais sur l'effacement : un nom de
  # travers ne doit pas faire disparaitre le trait d'axe sans un mot.
  expect_equal(hstat_axe_zero("zzz")$mode, "reference")
  expect_equal(hstat_axe_zero(NULL)$mode, "reference")

  ax <- hstat_axe_zero("axe", negatifs = FALSE, couleur = "#123456", epaisseur = 2)
  th <- Filter(function(x) inherits(x, "theme"), ax$couches)
  expect_length(th, 1L)
  # L'ANCIEN TRAIT S'EFFACE, sinon l'axe existe en DEUX exemplaires -- un au bas
  # du panneau, un a zero -- et c'est l'image que l'utilisateur vient corriger.
  expect_true(inherits(th[[1]]$axis.line.x, "element_blank"))
  expect_true(inherits(th[[1]]$axis.line.x.bottom, "element_blank"))
  # MAIS L'AXE Y N'EST PAS TOUCHE : il porte l'echelle, y compris la part
  # negative. Les deux axes se rejoignent alors a l'origine.
  expect_null(th[[1]]$axis.line.y)
  # La couleur et l'epaisseur sont celles du trait d'axe : un noir arbitraire
  # en ferait un repere de plus a cote d'un cadre d'une autre couleur.
  hl <- Filter(function(x) inherits(x, "Layer"), ax$couches)
  expect_equal(hl[[1]]$aes_params$colour, "#123456")
  expect_equal(hl[[1]]$aes_params$linewidth, 2)
  # Une epaisseur aberrante ne fait pas tomber le graphique.
  expect_equal(Filter(function(x) inherits(x, "Layer"),
                      hstat_axe_zero("axe", epaisseur = NA)$couches)[[1]]$aes_params$linewidth, 1)
})

test_that("pose sur le zero, le trait d'axe rejoint vraiment les graduations", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(M = factor(c("A", "B", "C")), v = c(10, 20, 30))
  extras <- hstat_plot_extras_lire(
    list(pfxAxisLine = TRUE, pfxAxisLineCouleur = "#000000",
         pfxAxisLineEpaisseur = 1), "pfx")
  base <- ggplot2::ggplot(d, ggplot2::aes(x = M, y = v)) + ggplot2::geom_col() +
    hstat_plot_extras_theme(extras)
  bas <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range[1]

  # LE DEFAUT MESURE, celui de la capture d'ecran : le trait d'axe est au BAS
  # du panneau, et le bas du panneau n'est pas zero -- ggplot detend l'echelle
  # de 5 % de chaque cote. Zero flotte donc au-dessus du trait.
  expect_true(inherits(base$theme$axis.line.x, "element_line"))
  expect_lt(bas(base), 0)

  z <- hstat_axe_zero("axe", negatifs = FALSE, extras$axe_col, extras$axe_ep)
  pose <- base + z$couches + ggplot2::scale_y_continuous(expand = z$expand)
  # L'ancien trait a disparu, celui de l'axe Y est intact.
  expect_true(inherits(pose$theme$axis.line.x, "element_blank"))
  expect_true(inherits(pose$theme$axis.line.y, "element_line"))
  # ET LE BAS DU PANNEAU EST EXACTEMENT ZERO. Sans cette moitie-la du
  # correctif, le trait serait pose a zero pendant que les graduations
  # resteraient dessinees au bord du panneau, 1,5 unite plus bas : on aurait
  # deplace le defaut au lieu de le corriger. C'est l'assertion qui distingue
  # les deux codes -- la couche seule ne la satisfait pas.
  expect_equal(bas(pose), 0)

  # DES QU'UNE VALEUR EST NEGATIVE, l'expansion se garde : le trait est de
  # toute facon a l'interieur du cadre, et une barre collee au bord se lit mal.
  zn <- hstat_axe_zero("axe", negatifs = TRUE)
  expect_true(identical(zn$expand, ggplot2::waiver()))
  dn <- data.frame(M = factor(c("A", "B", "C")), v = c(-10, 20, 30))
  pn <- ggplot2::ggplot(dn, ggplot2::aes(x = M, y = v)) + ggplot2::geom_col() +
    zn$couches
  expect_lt(bas(pn), -10)

  # Et le trait reste DANS le cadre meme si la serie n'atteint jamais zero :
  # sans `expand_limits`, le reglage rendrait un axe invisible.
  dl <- data.frame(M = factor(c("A", "B")), v = c(40, 50))
  pl <- ggplot2::ggplot(dl, ggplot2::aes(x = M, y = v)) +
    ggplot2::geom_point() + hstat_axe_zero("axe")$couches
  expect_lte(bas(pl), 0)
})

test_that("masquer une modalite la retire de la figure sans toucher aux chiffres", {
  skip_if_not_installed("ggplot2")
  h <- data.frame(Modalite = rep(c("T0", "T1", "T2"), each = 3),
                  Masse    = c(10, 11, 12, 20, 21, 22, 30, 31, 32),
                  Surface  = rep(1, 9), Bloc = rep(1:3, 3),
                  stringsAsFactors = FALSE)
  lance <- function(masque) {
    v <- shiny::reactiveValues(data = h, cleanData = h, filteredData = h)
    out <- list()
    shiny::testServer(mod_yield_server, args = list(values = v), {
      vider <- function() try(session$flushReact(), silent = TRUE)
      session$setInputs(yieldSource = "fichier", yieldModalite = "Modalite",
                        yieldMasse = "Masse", yieldSurface = "Surface",
                        yieldRepetition = "Bloc", yieldTemoin = "T0"); vider()
      session$setInputs(yieldMesure = "Rendement_moyen", yieldErreurs = FALSE,
                        yieldZeroMode = "reference", yieldMasquer = masque); vider()
      out <<- list(p = graphique(), r = resultat(), note = output$yieldPlotNote)
    })
    out
  }
  niveaux <- function(p) {
    b <- suppressWarnings(ggplot2::ggplot_build(p))
    as.character(b$layout$panel_params[[1]]$x$get_labels())
  }

  tout <- lance(character(0))
  expect_setequal(niveaux(tout$p), c("T0", "T1", "T2"))

  part <- lance("T1")
  # La modalite masquee quitte l'axe -- et elle n'y laisse pas sa place vide,
  # ce qu'un facteur pose avant le retrait aurait fait.
  expect_setequal(niveaux(part$p), c("T0", "T2"))

  # MASQUER N'EST PAS FILTRER, et c'est tout le reglage : le tableau et les
  # moyennes portent toujours les trois modalites. Un filtre, lui,
  # recalculerait -- et c'est l'assertion qui separe les deux comportements.
  expect_setequal(as.character(part$r$Modalite), c("T0", "T1", "T2"))
  expect_equal(part$r$Rendement_moyen, tout$r$Rendement_moyen, tolerance = 1e-10)

  # UNE BARRE ABSENTE SE NOMME. Une figure a laquelle il manque une modalite,
  # sans rien qui le dise, se lit comme un essai qui n'en comptait que deux.
  # `as.character()` d'une liste de balises rend UN element par noeud : l'aplatir
  # est la difference entre une assertion et un vecteur de trois verdicts dont
  # `expect_true` ne sait que faire.
  txt <- function(x) paste(as.character(x), collapse = "")
  expect_true(grepl("T1", txt(part$note), fixed = TRUE))
  expect_true(grepl("masquer ne change aucun chiffre", txt(part$note), fixed = TRUE))
  expect_false(grepl("T1", txt(tout$note), fixed = TRUE))

  # Tout masquer ne fait pas tomber le module : il n'y a simplement plus de
  # figure, et le motif affiche dira le masquage plutot que les colonnes.
  expect_null(lance(c("T0", "T1", "T2"))$p)
})

# -----------------------------------------------------------------------------
# DATES : FRANCAISES, ANGLAISES ET ISO, QUELLE QUE SOIT LA LANGUE DE L'APPLICATION
# -----------------------------------------------------------------------------
test_that("toutes les ecritures de date entrent, quel que soit le format declare", {
  # LE DEFAUT MESURE, celui de la capture d'ecran : le format source se
  # DECLARAIT, et il devait tomber juste. Un fichier ISO lu en « MM/JJ/AAAA »
  # rend NA sur toutes les lignes -- cadre vide, sans un mot.
  iso <- c("2026-10-20", "2026-10-27", "2026-11-03")
  expect_true(all(is.na(suppressWarnings(hstat_date_parse(iso, "%m/%d/%Y")))))

  attendu <- as.Date(c("2026-10-20", "2026-10-27", "2026-11-03"))
  jeux <- list(
    iso     = iso,
    fr_slash = c("20/10/2026", "27/10/2026", "03/11/2026"),
    us_slash = c("10/20/2026", "10/27/2026", "11/03/2026"),
    fr_point = c("20.10.2026", "27.10.2026", "03.11.2026"),
    fr_mois  = c("20 octobre 2026", "27 octobre 2026", "3 novembre 2026"),
    en_mois  = c("October 20, 2026", "October 27, 2026", "November 3, 2026"))
  # Le format declare est FAUX pour tous sauf un : c'est le balayage qui doit
  # rattraper, et il le fait sans que la langue de la session intervienne.
  for (lg in c("fr", "en")) {
    shiny::withReactiveDomain(list(userData = list(langue = lg)), {
      for (nm in names(jeux)) {
        r <- hstat_date_auto(jeux[[nm]], "%m/%d/%Y")
        expect_equal(r$dates, attendu, info = paste(lg, nm))
      }
    })
  }

  # Les mois abreges des DEUX langues passent par le meme format.
  expect_equal(hstat_date_auto(c("25-mars-2024", "1-avril-2024"))$dates,
               as.Date(c("2024-03-25", "2024-04-01")))
  expect_equal(hstat_date_auto(c("25-Mar-2024", "1-Apr-2024"))$dates,
               as.Date(c("2024-03-25", "2024-04-01")))

  # Une colonne deja typee Date traverse sans etre retouchee.
  d <- as.Date(c("2026-10-20", "2026-10-27"))
  expect_equal(hstat_date_auto(d, "%m/%d/%Y")$dates, d)

  # Ce qui n'est pas une date n'en devient pas une : le refus est franc.
  r <- hstat_date_auto(c("T1", "T2", "bloc A"))
  expect_true(all(is.na(r$dates)))
  expect_equal(r$n_ok, 0L)
})

test_that("l'aller-retour barre la date plausible et fausse", {
  # LE DEFAUT LE PLUS COUTEUX DES TROIS, parce qu'il ne laisse aucun vide :
  # `%Y` de R accepte DEUX chiffres, si bien que « 20/10/2026 » lu en
  # « %Y/%m/%d » rend l'an 20. Le graphique se trace, l'axe couvre deux mille
  # ans, et rien ne le signale.
  faux <- suppressWarnings(hstat_date_parse("20/10/2026", "%Y/%m/%d"))
  # Deux mille ans d'ecart, et aucune erreur levee.
  expect_equal(as.integer(format(faux, "%Y")), 20L)

  # C'est le controle d'aller-retour, et lui seul, qui l'ecarte : la date
  # relue s'ecrirait « 0020/10/20 », qui ne ressemble pas a l'original.
  expect_false(.hstat_date_coherent("20/10/2026", faux, "%Y/%m/%d"))
  expect_true(.hstat_date_coherent("20/10/2026", as.Date("2026-10-20"), "%d/%m/%Y"))
  # Et le balayage retient donc la bonne lecture.
  expect_equal(hstat_date_auto("20/10/2026", "%Y/%m/%d")$dates, as.Date("2026-10-20"))

  # L'ALLER-RETOUR TOLERE LE ZERO DE TETE, sans quoi « 3 novembre 2026 »
  # serait refuse par sa propre ecriture -- `format()` la rend « 03 novembre ».
  expect_true(.hstat_date_coherent("3 novembre 2026", as.Date("2026-11-03"), "%d %B %Y"))
  # Et il vaut dans LES DEUX LANGUES : un mois anglais se relit même en
  # session francaise.
  shiny::withReactiveDomain(list(userData = list(langue = "fr")), {
    expect_true(.hstat_date_coherent("March 25, 2024", as.Date("2024-03-25"), "%B %d, %Y"))
  })
})

test_that("une date ambigue se dit, elle ne se tranche pas en silence", {
  # « 01/02/2026 » est le 1er fevrier OU le 2 janvier : les deux lectures
  # relisent parfaitement, et aucune n'est plus juste que l'autre.
  a <- hstat_date_auto(c("01/02/2026", "03/04/2026"))
  expect_gt(length(a$ambigu), 1L)
  expect_true(all(c("%d/%m/%Y", "%m/%d/%Y") %in% a$ambigu))

  # UN CHOIX EXPLICITE N'EST JAMAIS ECRASE : c'est precisement sur ce jeu-la
  # que l'utilisateur est le seul a savoir, et `auto` doit rester FALSE.
  us <- hstat_date_auto(c("01/02/2026", "03/04/2026"), "%m/%d/%Y")
  fr <- hstat_date_auto(c("01/02/2026", "03/04/2026"), "%d/%m/%Y")
  expect_equal(us$dates, as.Date(c("2026-01-02", "2026-03-04")))
  expect_equal(fr$dates, as.Date(c("2026-02-01", "2026-04-03")))
  expect_false(us$auto); expect_false(fr$auto)

  # ET L'ASSERTION QUI MORD : un jour superieur a 12 tranche de lui-meme, donc
  # l'alerte ne se declenche pas partout. Sans ce cas, une fonction qui
  # crierait a l'ambiguite sur TOUTE date numerique passerait le test ci-dessus.
  net <- hstat_date_auto(c("20/10/2026", "27/10/2026"))
  expect_length(net$ambigu, 0L)
  expect_equal(net$dates, as.Date(c("2026-10-20", "2026-10-27")))
})

test_that("le selecteur de format source derive du catalogue", {
  chemin <- .hstat_module_path("mod_viz.R")
  skip_if_not(file.exists(chemin))
  txt <- paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # La liste n'est pas recopiee : six entrees y figuraient contre onze
  # reconnues, et « 20 octobre 2026 » n'etait offert nulle part.
  expect_true(grepl("HSTAT_DATE_FORMATS_SRC", txt, fixed = TRUE))
  expect_false(grepl('"AAAA-MM-JJ (ISO 8601)" = "%Y-%m-%d"', txt, fixed = TRUE))
  expect_gte(length(HSTAT_DATE_FORMATS_SRC), 11L)
  # Chaque entree offerte doit etre une entree que le lecteur sait lire.
  for (f in unname(HSTAT_DATE_FORMATS_SRC)) {
    d <- hstat_date_auto(hstat_date_fmt(as.Date("2024-03-25"), f), f)
    expect_equal(d$dates, as.Date("2024-03-25"), info = f)
  }
  # Et chaque libelle offert a l'ecran est traduit.
  dico <- hstat_i18n_dict("en")
  skip_if(!length(dico))
  for (lib in c(names(HSTAT_DATE_FORMATS_SRC), "Automatique (détecter)"))
    expect_true(lib %in% names(dico), info = lib)
})

test_that("le graphique se trace meme quand le format source declare est faux", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("DT")
  suppressMessages(hstat_installer_replis_ui())
  # LE TEST PORTE SUR LE MODULE, PAS SUR LA FONCTION. Un test appelant
  # `hstat_date_auto()` seul serait reste vert pendant que le graphique sortait
  # vide -- il aurait verifie que le LECTEUR sait lire, jamais que le module
  # l'emploie. C'est la lecon deja apprise sur l'echelle de l'axe X.
  d <- data.frame(Semaine = c("2026-10-20", "2026-10-27", "2026-11-03", "2026-11-10"),
                  Rdt = c(12, 15, 11, 18), stringsAsFactors = FALSE)
  trace <- function(fmt) {
    v <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d)
    out <- list()
    shiny::testServer(mod_viz_server, args = list(values = v), {
      vider <- function() try(session$flushReact(), silent = TRUE)
      session$setInputs(vizXVar = "Semaine", xVarType = "date", vizYVar = "Rdt",
                        vizType = "line", xDateFormat = fmt); vider()
      out <<- list(pd = plotData(), p = createPlot())
    })
    out
  }
  pts <- function(o) {
    b <- suppressWarnings(ggplot2::ggplot_build(o$p))
    sum(!is.na(b$data[[1]]$x))
  }
  # Mesure d'avant correctif, sur ces memes entrees : 0 date lue sur 4 et 0
  # point trace des que le format declare ne tombait pas juste.
  for (fmt in c("%m/%d/%Y", HSTAT_DATE_AUTO, "%Y-%m-%d")) {
    o <- trace(fmt)
    expect_s3_class(o$pd$Semaine, "Date")
    expect_equal(sum(!is.na(o$pd$Semaine)), 4L, info = fmt)
    expect_equal(pts(o), 4L, info = fmt)
  }
  # Et les ecritures francaise et anglaise passent par le meme chemin.
  for (col in list(c("20/10/2026", "27/10/2026", "03/11/2026", "10/11/2026"),
                   c("20 octobre 2026", "27 octobre 2026", "3 novembre 2026",
                     "10 novembre 2026"))) {
    d$Semaine <- col
    o <- trace(HSTAT_DATE_AUTO)
    expect_equal(sum(!is.na(o$pd$Semaine)), 4L, info = col[1])
  }
})

# ---------------------------------------------------------------------------
#  PERTES DE RECOLTE
# ---------------------------------------------------------------------------

# Le jeu d'essai rend les formules DISCERNABLES, et c'est la condition du test.
# Avec PP = 2000, PV = 1600 et NT = 1000, chacune des quatre lignes publiees
# tombe sur une valeur differente (50 / 37,5 / 20 / 80) : une inversion de
# numerateur ou un rapport pris a l'envers se voit. Sur des rendements egaux,
# toutes les mutations passeraient.
.rdt_essai <- function() list(r = c(2000, 1600, 1000), m = c("PP", "PV", "NT"))
.rdt_v <- function(x, mod, m) as.numeric(x)[match(mod, m)]

test_that("les quatre indicateurs de perte tombent sur les formules publiees", {
  e <- .rdt_essai()
  pr_pp <- hstat_rdt_perte(e$r, e$m, "PP")
  pr_pv <- hstat_rdt_perte(e$r, e$m, "PV")
  rp_pp <- hstat_rdt_relatif(e$r, e$m, "PP")

  # PR(PP) = (PP - NT) / PP x 100
  expect_equal(.rdt_v(pr_pp, "NT", e$m), 50)
  # PR(PV) = (PV - NT) / PV x 100
  expect_equal(.rdt_v(pr_pv, "NT", e$m), 37.5)
  # PR(PV/PP) = (PP - PV) / PP x 100
  expect_equal(.rdt_v(pr_pp, "PV", e$m), 20)
  # RP(PV/PP) = PV / PP x 100
  expect_equal(.rdt_v(rp_pp, "PV", e$m), 80)

  # Les quatre sont distinctes : sans cela l'assertion passerait sur une
  # fonction qui confondrait deux des formules.
  expect_equal(length(unique(c(50, 37.5, 20, 80))), 4L)
})

test_that("la perte et le rendement relatif somment a 100, chacun par sa formule", {
  e <- .rdt_essai()
  p <- as.numeric(hstat_rdt_perte(e$r, e$m, "PP"))
  q <- as.numeric(hstat_rdt_relatif(e$r, e$m, "PP"))
  expect_equal(p + q, rep(100, 3))
  # L'INVARIANT NE DOIT PAS ETRE VRAI PAR CONSTRUCTION. Si le relatif etait
  # deduit par `100 - perte`, la somme vaudrait 100 quelle que soit la formule
  # de la perte, et ce test ne garderait plus rien. On exige donc que les deux
  # soient reellement differents l'un de l'autre.
  expect_false(isTRUE(all.equal(p, q)))
})

test_that("la reference vaut 0 % de perte et 100 % de rendement relatif", {
  e <- .rdt_essai()
  expect_equal(.rdt_v(hstat_rdt_perte(e$r, e$m, "PP"),   "PP", e$m), 0)
  expect_equal(.rdt_v(hstat_rdt_relatif(e$r, e$m, "PP"), "PP", e$m), 100)
})

test_that("une perte negative est un resultat, elle n'est pas bornee a zero", {
  # PV fait MIEUX que PP : la perte est negative, et c'est ce qu'il faut voir.
  p <- hstat_rdt_perte(c(1000, 1400), c("PP", "PV"), "PP")
  expect_equal(as.numeric(p)[2], -40)
  expect_lt(as.numeric(p)[2], 0)
})

test_that("une reference de rendement nul rend NA, et le dit", {
  p <- hstat_rdt_perte(c(0, 5), c("A", "B"), "A")
  # LES VALEURS SONT VERIFIEES, PAS SEULEMENT LE MESSAGE. Un `Inf` affiche a
  # cote de son alerte est la forme la plus couteuse : le tableau parait sain
  # parce que le motif est la. C'est la lecon du temoin nul des efficacites.
  expect_true(all(is.na(as.numeric(p))))
  expect_false(any(is.infinite(as.numeric(p))))
  expect_match(attr(p, "message"), "nul")
  expect_match(attr(p, "message"), "division par z")
})

test_that("une reference non choisie ou absente ne rend aucun chiffre", {
  e <- .rdt_essai()
  for (ref in list(NULL, "", "ZZ")) {
    p <- hstat_rdt_perte(e$r, e$m, ref)
    expect_true(all(is.na(as.numeric(p))))
    expect_true(nzchar(attr(p, "message")))
  }
})

test_that("le tableau des pertes porte une reference absente plutot que de la taire", {
  res <- data.frame(Modalite = c("PP", "NT"), Rendement_moyen = c(2000, 1000),
                    stringsAsFactors = FALSE)
  t2 <- hstat_rdt_pertes_table(res, c("PP", "Inconnue"))
  expect_equal(attr(t2, "references_absentes"), "Inconnue")
  expect_equal(attr(t2, "references_perte"), "PP")
  expect_true("Perte_moyen_vs_PP" %in% names(t2))
  expect_false(any(grepl("Inconnue", names(t2))))
})

test_that("deux references de meme abrege ne s'ecrasent pas", {
  # « T 1 » et « T-1 » donnent tous deux « T_1 » : sans distinction, la seconde
  # colonne ECRASERAIT la premiere et l'essai perdrait une reference sans un mot.
  res <- data.frame(Modalite = c("T 1", "T-1", "NT"),
                    Rendement_moyen = c(2000, 1600, 1000), stringsAsFactors = FALSE)
  t2 <- hstat_rdt_pertes_table(res, c("T 1", "T-1"))
  cols <- grep("^Perte_moyen_vs_", names(t2), value = TRUE)
  expect_equal(length(cols), 2L)
  expect_equal(length(unique(cols)), 2L)
  # Et chaque colonne porte bien SA reference : la premiere met « T 1 » a zero,
  # la seconde « T-1 ».
  z <- vapply(cols, function(k) as.character(t2$Modalite[which(t2[[k]] == 0)])[1],
              character(1))
  expect_setequal(unname(z), c("T 1", "T-1"))
})

test_that("un pourcentage ne se convertit jamais, perte et relatif comprises", {
  d <- data.frame(Traitement = rep(c("PP", "NT"), each = 2),
                  Masse = c(2100, 1900, 1010, 990), Surface = 1,
                  stringsAsFactors = FALSE)
  r <- hstat_rdt_complet(d, "Traitement", "Masse", "Surface", non_traite = "NT",
                         conv_masse = "tonne (1000 kg)", conv_surface = "hectare (ha)",
                         references = "PP")
  expect_true(any(grepl("^Perte_", names(r))))
  expect_true(any(grepl("_conv$", names(r))))   # la conversion a bien eu lieu
  # UN GAIN, UNE PERTE ET UN RENDEMENT RELATIF SONT SANS DIMENSION : les
  # convertir serait une faute de categorie -- le meme rapport rendrait des
  # pourcentages multiplies par mille.
  expect_length(grep("^Gain_.*_conv$", names(r)), 0)
  expect_length(grep("^Perte_.*_conv$", names(r)), 0)
  expect_length(grep("^Relatif_.*_conv$", names(r)), 0)
  expect_setequal(HSTAT_RDT_PREFIXES_PCT, c("Gain_", "Perte_", "Relatif_"))
})

test_that("le module de rendement offre les pertes ET les trace", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("DT")
  # LE TEST PORTE SUR LE MODULE, PAS SUR LA FONCTION. Un test sur
  # `hstat_rdt_perte()` seule resterait vert pendant que le selecteur n'offre
  # rien et que la figure sort vide : il verifierait que le calcul sait
  # calculer, jamais que le module l'emploie.
  d <- data.frame(
    Traitement = rep(c("PP", "PV", "NT"), each = 3),
    Masse      = c(2100, 1950, 1950, 1650, 1580, 1570, 1010, 990, 1000),
    Surface    = 1, stringsAsFactors = FALSE)
  vals <- shiny::reactiveValues(filteredData = d)

  shiny::testServer(mod_yield_server, args = list(values = vals), {
    session$setInputs(yieldSource = "fichier", yieldModalite = "Traitement",
                      yieldMasse = "Masse", yieldSurface = "Surface",
                      yieldTemoin = "NT", yieldRefPerte = c("PP", "PV"),
                      yieldRound = FALSE)
    r <- resultat()
    g <- function(mod, col) r[[col]][match(mod, r$Modalite)]
    expect_equal(g("NT", "Perte_moyen_vs_PP"), 50)
    expect_equal(g("NT", "Perte_moyen_vs_PV"), 37.5)
    expect_equal(g("PV", "Perte_moyen_vs_PP"), 20)
    expect_equal(g("PV", "Relatif_moyen_vs_PP"), 80)

    # Le selecteur du graphique les offre, avec leur libelle.
    md <- mesures_dispo()
    expect_true("Perte_moyen_vs_PP" %in% md)
    expect_true("Relatif_moyen_vs_PP" %in% md)
    expect_match(names(md)[match("Perte_moyen_vs_PP", md)], "PP", fixed = TRUE)

    # Et la figure se construit REELLEMENT sur une mesure de perte.
    session$setInputs(yieldMesure = "Perte_moyen_vs_PP")
    p <- graphique()
    expect_false(is.null(p))
    b <- ggplot2::ggplot_build(p)
    expect_equal(nrow(b$data[[1]]), 3L)
    expect_match(p$labels$y, "Perte")

    # UN RENDEMENT RELATIF SE LIT PAR RAPPORT A 100 : l'etendue doit
    # l'atteindre, sinon la base de comparaison sort du champ.
    session$setInputs(yieldMesure = "Relatif_moyen_vs_PP")
    p2 <- graphique()
    expect_match(p2$labels$y, "relatif")
    yr <- ggplot2::ggplot_build(p2)$layout$panel_params[[1]]$y.range
    expect_lte(yr[1], 100)
    expect_gte(yr[2], 100)
  })
})

# ---------------------------------------------------------------------------
#  DIVERSITE : FICHIER DE COMPTAGES (identifiants multiples, especes x stades)
# ---------------------------------------------------------------------------

# Reproduction de la forme reellement rencontree : trois colonnes d'identite
# (traitement, periode, semaine) et une colonne par couple espece x stade.
.div_comptages <- function() {
  data.frame(
    Traitement = rep(c("Produit A", "Témoin"), each = 6),
    Periode    = rep(c("T-1", "T+7", "T+13", "T2-1", "T2+7", "T2+13"), 2),
    Semaine    = rep(c("2026-08-04", "2026-08-11", "2026-08-18",
                       "2026-08-25", "2026-09-01", "2026-09-08"), 2),
    ch_Cocc          = c(0, 1, 0, 0, 0, 0,  2, 1, 3, 0, 1, 2),
    ch_Syrphe        = c(0, 0, 0, 0, 0, 0,  1, 0, 2, 1, 0, 1),
    ad_Cocc          = c(2, 2, 1, 0, 0, 0,  3, 4, 2, 1, 2, 3),
    ad_Chrysope      = c(0, 0, 0, 0, 0, 0,  1, 1, 0, 2, 1, 0),
    ad_Syrphe        = c(0, 0, 0, 0, 1, 1,  2, 1, 1, 0, 2, 1),
    ad_Fourmi_Noire  = c(0, 0, 0, 0, 0, 0,  1, 0, 1, 1, 0, 2),
    ind_Fourmi_Rouge = c(0, 0, 0, 0, 0, 0,  0, 2, 1, 0, 1, 0),
    ind_Mante        = c(0, 0, 0, 0, 0, 0,  1, 0, 0, 1, 0, 1),
    stringsAsFactors = FALSE)
}
.div_esp <- function() setdiff(names(.div_comptages()),
                               c("Traitement", "Periode", "Semaine"))

test_that("un stade n'est reconnu que si l'espece apparait sous deux prefixes", {
  st <- hstat_div_stades(.div_esp())
  # Cocc (ch + ad) et Syrphe (ch + ad) : deux stades chacune.
  expect_setequal(st$multi, c("Cocc", "Syrphe"))
  expect_equal(st$n_multi, 2L)
  # `ad_Fourmi_Noire` coupe au PREMIER souligne : l'espece est « Fourmi_Noire ».
  expect_equal(st$stade[match("ad_Fourmi_Noire", st$colonne)], "ad")
  # UN PREFIXE ISOLE NE RENOMME RIEN. `ad_Chrysope` n'apparait qu'a un stade :
  # le renommer en « Chrysope » inventerait une lecture que le fichier ne porte
  # pas -- et `Bloc_1` seul deviendrait l'espece « 1 ».
  expect_equal(st$espece[match("ad_Chrysope", st$colonne)], "ad_Chrysope")
  expect_equal(st$espece[match("ind_Mante", st$colonne)], "ind_Mante")
  # Les deux stades de Cocc portent bien le MEME nom une fois regroupes.
  expect_equal(st$espece[match(c("ch_Cocc", "ad_Cocc"), st$colonne)],
               c("Cocc", "Cocc"))
  # Une colonne sans souligne reste elle-meme.
  expect_equal(hstat_div_stades("Mante")$espece, "Mante")
  expect_equal(hstat_div_stades("Mante")$n_multi, 0L)
})

test_that("l'identite du releve se compose de plusieurs colonnes", {
  d <- .div_comptages(); esp <- .div_esp()
  un    <- hstat_div_matrice(d, "large", var_site = "Traitement", var_especes = esp)
  trois <- hstat_div_matrice(d, "large",
                             var_site = c("Traitement", "Periode", "Semaine"),
                             var_especes = esp)
  # LES DEUX CODES DOIVENT ETRE DISCERNABLES : une fonction qui ignorerait les
  # colonnes surnumeraires rendrait 2 dans les deux cas, et l'assertion
  # passerait avec ou sans le correctif.
  expect_equal(nrow(un), 2L)
  expect_gt(nrow(trois), nrow(un))
  expect_match(rownames(trois)[1], " | ", fixed = TRUE)
  # Chaque croisement non vide fait un releve, et un seul.
  expect_equal(anyDuplicated(rownames(trois)), 0L)
})

test_that("regrouper les stades change la richesse ET les indices", {
  d <- .div_comptages(); esp <- .div_esp()
  sans <- hstat_div_matrice(d, "large", var_site = "Traitement",
                            var_especes = esp, grouper_stades = FALSE)
  avec <- hstat_div_matrice(d, "large", var_site = "Traitement",
                            var_especes = esp, grouper_stades = TRUE)
  expect_equal(ncol(sans), 8L)   # huit colonnes
  expect_equal(ncol(avec), 6L)   # six especes

  # LE COUT NE PORTE PAS QUE SUR LA RICHESSE. Shannon, Simpson et tous les
  # verdicts se calculent sur les memes colonnes : compter deux stades pour
  # deux especes gonfle l'indice lui-meme, sans que rien ne leve.
  h_sans <- hstat_div_indices(sans["Témoin", ])$Shannon_H
  h_avec <- hstat_div_indices(avec["Témoin", ])$Shannon_H
  expect_gt(h_sans, h_avec)
  expect_gt((h_sans - h_avec) / h_avec, 0.1)

  # LES EFFECTIFS SONT ADDITIONNES, pas remplaces : le total du releve ne
  # change pas -- regrouper deux stades ne fait disparaitre aucun individu.
  expect_equal(sum(sans), sum(avec))
  expect_equal(unname(rowSums(sans)), unname(rowSums(avec)))
  expect_equal(unname(avec["Témoin", "Cocc"]),
               unname(sans["Témoin", "ch_Cocc"] + sans["Témoin", "ad_Cocc"]))
})

test_that("des stades non regroupes sont nommes plutot que tus", {
  d <- .div_comptages(); esp <- .div_esp()
  sans <- hstat_div_matrice(d, "large", var_site = "Traitement",
                            var_especes = esp, grouper_stades = FALSE)
  # UNE RICHESSE GONFLEE SANS UN MOT est le defaut qu'on corrige : le cas est
  # nomme meme -- surtout -- quand on ne regroupe pas.
  msg <- attr(sans, "message")
  expect_true(!is.null(msg) && nzchar(msg))
  expect_match(msg, "Cocc")
  expect_match(msg, "Syrphe")
  expect_match(msg, "richesse")

  avec <- hstat_div_matrice(d, "large", var_site = "Traitement",
                            var_especes = esp, grouper_stades = TRUE)
  expect_match(attr(avec, "message"), "regroup", ignore.case = TRUE)

  # Un fichier sans stade ne doit produire AUCUN de ces deux messages : un
  # balayage qui crie au loup finit desactive.
  d2 <- data.frame(Site = c("A", "B"), Mante = c(1, 2), Chrysope = c(3, 4),
                   stringsAsFactors = FALSE)
  m2 <- hstat_div_matrice(d2, "large", var_site = "Site",
                          var_especes = c("Mante", "Chrysope"))
  expect_false(grepl("stade", attr(m2, "message") %||% "", ignore.case = TRUE))
})

test_that("le module de diversite lit un fichier de comptages tel quel", {
  skip_if_not_installed("ggplot2")
  # LE TEST PORTE SUR LE MODULE. Un test sur `hstat_div_matrice()` seule
  # resterait vert pendant que l'interface ne passe qu'une colonne de releve et
  # ignore le regroupement : il verifierait que le socle sait lire, jamais que
  # le module l'emploie.
  d <- .div_comptages(); esp <- .div_esp()
  vals <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d)

  shiny::testServer(mod_diversity_server, args = list(values = vals), {
    session$setInputs(divFormat = "large",
                      divSite = c("Traitement", "Periode", "Semaine"),
                      divEspeces = esp, divStades = TRUE, divBase = "2")
    session$setInputs(divCalculer = 1)
    expect_false(is.null(rv$mat))
    expect_equal(ncol(rv$mat), 6L)          # les stades ont ete regroupes
    expect_gt(nrow(rv$mat), 2L)             # la cle porte bien les trois colonnes
    expect_match(rownames(rv$mat)[1], " | ", fixed = TRUE)

    # Sans regroupement, le module rend huit especes et le dit.
    session$setInputs(divStades = FALSE)
    session$setInputs(divCalculer = 2)
    expect_equal(ncol(rv$mat), 8L)
  })
})

test_that("les gabarits de libelle de perte sont au dictionnaire", {
  # LE BALAYAGE DE COUVERTURE NE PEUT PAS LES VOIR. Il ne releve que les
  # chaines LITTERALES passees a `tr()`/`trf()` ; celles-ci sont declarees dans
  # une constante et arrivent a `trf()` par une variable. Sans ce test, une
  # entree oubliee laisserait le libelle en francais dans une interface
  # anglaise, sans que rien ne le signale -- exactement le cas de
  # `HSTAT_ERR_FR`, assemble a l'execution lui aussi.
  dic <- hstat_i18n_load()
  gab <- unlist(lapply(HSTAT_RDT_MESURES_PERTE, function(x) x[c("perte", "relatif")]))
  expect_length(gab, 6L)
  expect_equal(setdiff(unname(gab), dic$fr), character(0))

  # Et la traduction garde les MEMES marqueurs : `sprintf` leverait « too few
  # arguments » sur un `%s` perdu, et ferait tomber toute la sortie pour une
  # simple erreur de dictionnaire.
  marq <- function(x) sort(unlist(regmatches(x, gregexpr(HSTAT_I18N_MARQUEUR, x))))
  for (g in unname(gab)) {
    en <- dic$en[match(g, dic$fr)]
    expect_equal(marq(g), marq(en), info = g)
  }
})

# ---------------------------------------------------------------------------
#  PERTES : DEUX FACONS DE TENIR COMPTE DES REPETITIONS
# ---------------------------------------------------------------------------

# LES RENDEMENTS VARIENT FORTEMENT D'UN BLOC A L'AUTRE, et c'est la condition du
# test : sur des blocs identiques, la moyenne des pertes et la perte des
# moyennes COINCIDENT, et toute mutation passerait.
.rdt_blocs <- function() data.frame(
  Traitement = rep(c("PP", "NT"), each = 2),
  Bloc       = c("B1", "B2", "B1", "B2"),
  Masse      = c(2000, 1000, 1000, 800),
  Surface    = 1, stringsAsFactors = FALSE)

test_that("la moyenne des pertes n'est pas la perte des moyennes", {
  d <- .rdt_blocs()
  arg <- list(d, "Traitement", "Masse", "Surface", non_traite = "NT",
              var_repetition = "Bloc", references = "PP")
  cum <- do.call(hstat_rdt_complet, c(arg, list(mode_perte = "cumul")))
  rep <- do.call(hstat_rdt_complet, c(arg, list(mode_perte = "par_repetition")))
  g <- function(t, col) t[[col]][match("NT", t$Modalite)]

  # (1500 - 900) / 1500 = 40 %
  expect_equal(g(cum, "Perte_moyen_vs_PP"), 40)
  # (50 + 20) / 2 = 35 %
  expect_equal(g(rep, "Perte_rep_vs_PP"), 35)
  # LES DEUX DOIVENT ETRE DISCERNABLES : sans cette assertion, un mode qui
  # retomberait sur l'autre passerait inapercu.
  expect_false(isTRUE(all.equal(g(cum, "Perte_moyen_vs_PP"),
                                g(rep, "Perte_rep_vs_PP"))))

  # La dispersion n'existe que dans le mode par repetition : c'est tout son
  # interet -- une valeur unique par modalite n'en a pas.
  expect_equal(g(rep, "Perte_rep_n_vs_PP"), 2)
  expect_equal(g(rep, "Perte_rep_ET_vs_PP"), stats::sd(c(50, 20)))
  expect_equal(g(rep, "Perte_rep_ES_vs_PP"), stats::sd(c(50, 20)) / sqrt(2))
  expect_false("Perte_rep_vs_PP" %in% names(cum))
  expect_false("Perte_moyen_vs_PP" %in% names(rep))
})

test_that("la perte par repetition se mesure contre la reference du meme bloc", {
  d <- .rdt_blocs()
  det <- attr(hstat_rdt_table(d, "Traitement", "Masse", "Surface",
                              var_repetition = "Bloc"), "detail")
  rt <- hstat_rdt_pertes_rep(det, "PP")
  expect_equal(NROW(rt), 4L)
  # LE DENOMINATEUR EST CELUI DU BLOC, pas la moyenne generale : 2000 en B1,
  # 1000 en B2. Une fonction qui prendrait la moyenne (1500) rendrait 33,3 % et
  # 46,7 % au lieu de 50 % et 20 %.
  expect_equal(rt$Rendement_reference[rt$Repetition == "B1"], c(2000, 2000))
  expect_equal(rt$Rendement_reference[rt$Repetition == "B2"], c(1000, 1000))
  expect_equal(rt$Perte[rt$Modalite == "NT" & rt$Repetition == "B1"], 50)
  expect_equal(rt$Perte[rt$Modalite == "NT" & rt$Repetition == "B2"], 20)
  # La reference vaut 0 % de perte et 100 % de relatif DANS SON PROPRE BLOC.
  expect_equal(rt$Perte[rt$Modalite == "PP"], c(0, 0))
  expect_equal(rt$Relatif[rt$Modalite == "PP"], c(100, 100))
  # Les deux lectures somment toujours a 100.
  expect_equal(rt$Perte + rt$Relatif, rep(100, 4))
})

test_that("un bloc sans la reference est ecarte ET nomme", {
  d <- rbind(.rdt_blocs(),
             data.frame(Traitement = "NT", Bloc = "B3", Masse = 900, Surface = 1))
  det <- attr(hstat_rdt_table(d, "Traitement", "Masse", "Surface",
                              var_repetition = "Bloc"), "detail")
  rt <- hstat_rdt_pertes_rep(det, "PP")
  # UN BLOC SANS REFERENCE EST UN DEFAUT DE PLAN, PAS DE MESURE. Le retirer en
  # silence ferait porter la moyenne sur moins de blocs que l'essai n'en compte.
  expect_setequal(unique(rt$Repetition), c("B1", "B2"))
  expect_equal(attr(rt, "repetitions_ecartees"), "B3")
  expect_match(attr(rt, "message"), "B3")
  expect_match(attr(rt, "message"), "sans la r")
})

test_that("sans variable de repetition, le mode refuse plutot que de mentir", {
  d <- .rdt_blocs()
  det <- attr(hstat_rdt_table(d, "Traitement", "Masse", "Surface"), "detail")
  rt <- hstat_rdt_pertes_rep(det, "PP")
  # SANS REPETITION DECLAREE, toutes les lignes tombent dans le meme groupe et
  # le calcul rendrait exactement le mode « en commun » SOUS UN AUTRE NOM.
  expect_equal(NROW(rt), 0L)
  expect_match(attr(rt, "message"), "variable de r")
})

test_that("un ecart-type sur une seule repetition vaut NA, jamais zero", {
  rt <- data.frame(Modalite = c("NT", "PP"), Repetition = "B1",
                   Perte = c(50, 0), Relatif = c(50, 100),
                   stringsAsFactors = FALSE)
  som <- hstat_rdt_pertes_rep_resume(rt)
  # Zero se lirait « aucune variabilite », ce qui est un resultat. NA dit qu'il
  # n'y a rien a mesurer -- meme regle que la silhouette d'un groupe unique.
  expect_true(all(is.na(som$ET)))
  expect_true(all(is.na(som$ES)))
  expect_equal(som$n, c(1L, 1L))
})

test_that("le module de rendement transmet le mode de prise en compte", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("DT")
  # LE TEST PORTE SUR LE MODULE. Un test sur `hstat_rdt_pertes_rep()` seule
  # resterait vert pendant que l'interface reste bloquee sur « en commun ».
  d <- .rdt_blocs()
  vals <- shiny::reactiveValues(filteredData = d)
  shiny::testServer(mod_yield_server, args = list(values = vals), {
    session$setInputs(yieldSource = "fichier", yieldModalite = "Traitement",
                      yieldMasse = "Masse", yieldSurface = "Surface",
                      yieldRepetition = "Bloc", yieldTemoin = "NT",
                      yieldRefPerte = "PP", yieldRound = FALSE,
                      yieldModePerte = "cumul")
    r1 <- resultat()
    expect_equal(r1[["Perte_moyen_vs_PP"]][match("NT", r1$Modalite)], 40)
    expect_null(attr(r1, "pertes_repetitions"))

    session$setInputs(yieldModePerte = "par_repetition")
    r2 <- resultat()
    expect_equal(r2[["Perte_rep_vs_PP"]][match("NT", r2$Modalite)], 35)
    expect_equal(attr(r2, "mode_perte"), "par_repetition")

    # Le detail existe et se reanalyse : c'est ce que le mode promet.
    det <- pertes_detail()
    expect_equal(NROW(det), 4L)
    expect_setequal(unique(det$Repetition), c("B1", "B2"))

    # Et la mesure est offerte au graphique, avec son libelle.
    md <- mesures_dispo()
    expect_true("Perte_rep_vs_PP" %in% md)
    expect_match(names(md)[match("Perte_rep_vs_PP", md)], "répétition")
  })
})

test_that("les gabarits du resume par repetition sont au dictionnaire", {
  # Meme raison que pour les mesures agregees : declares dans une constante,
  # ils echappent au balayage des chaines litterales de `tr()`/`trf()`.
  dic <- hstat_i18n_load()
  gab <- vapply(HSTAT_RDT_PERTE_REP, function(x) unname(x[["lib"]]), character(1))
  expect_length(gab, 5L)
  expect_equal(setdiff(unname(gab), dic$fr), character(0))
  marq <- function(x) sort(unlist(regmatches(x, gregexpr(HSTAT_I18N_MARQUEUR, x))))
  for (g in unname(gab)) {
    en <- dic$en[match(g, dic$fr)]
    expect_equal(marq(g), marq(en), info = g)
  }
})

# ---------------------------------------------------------------------------
#  AUDIT : PERFORMANCE ET CORRECTION
# ---------------------------------------------------------------------------

test_that("`1:n` ne revient pas la ou n peut valoir zero", {
  root <- .hstat_repo_root(); skip_if(is.na(root))
  # `1:0` REND c(1, 0) : la boucle tourne une fois sur un indice qui n'existe
  # pas, et `cols = 1:ncol(x)` sur un tableau vide passe une colonne 0 a
  # openxlsx. C'est la meme inversion que `2:n`, deja documentee ici.
  # `seq_len(n)` rend le vide quand il n'y a rien a parcourir.
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    l <- .hstat_code_lignes(f)
    i <- grep("\\b1:(ncol|nrow|length|NROW|NCOL)\\(", l)
    if (length(i))
      fautifs <- c(fautifs, sprintf("%s:%d", basename(f), i))
  }
  expect_equal(fautifs, character(0))
})

test_that("le dictionnaire anglais ne part pas avec la page", {
  root <- .hstat_repo_root(); skip_if(is.na(root))
  # LE FRANCAIS EST LE DEFAUT : il ne doit rien payer pour une traduction
  # qu'il n'utilisera pas. Mesure au navigateur avant correction : 166 Ko
  # transferes (515 Ko decodes) a CHAQUE premiere visite.
  s <- as.character(hstat_i18n_script("en"))
  if (!is.na(hstat_i18n_asset("en"))) {
    # La balise ne porte que l'ADRESSE, jamais le dictionnaire lui-meme.
    expect_match(s, "HSTAT_I18N_SRC", fixed = TRUE)
    expect_false(grepl("HSTAT_I18N *=[^_]", s))
    expect_lt(nchar(s), 400L)
  } else {
    # Repli assume : le fichier n'a pas pu etre ecrit, on incorpore.
    expect_match(s, "HSTAT_I18N", fixed = TRUE)
  }
})

test_that("le traducteur va chercher le dictionnaire au premier passage en anglais", {
  root <- .hstat_repo_root(); skip_if(is.na(root))
  node <- unname(Sys.which("node")); skip_if(!nzchar(node), "node absent")
  js <- file.path(root, "inst", "app", "www", "hstat-i18n.js")
  skip_if(!file.exists(js))

  # BANC SANS DICTIONNAIRE DANS LA PAGE : seulement son adresse. L'injection
  # de script est simulee, et elle pose le dictionnaire comme le ferait le
  # vrai fichier. C'est le COMPORTEMENT qui est verifie, pas la forme du
  # code : un test textuel passerait encore si le chargement redevenait
  # immediat sous une autre ecriture.
  banc <- tempfile(fileext = ".js")
  writeLines(r"---(
var fs = require("fs"), vm = require("vm");
var SRC = process.argv[2], MODE = process.argv[3] || "en";
function El(t){ return { nodeType:1, tagName:t, childNodes:[], attributes:{},
  classList:{ contains:function(){return false;} },
  getAttribute:function(k){ return this.attributes[k]===undefined?null:this.attributes[k]; },
  setAttribute:function(k,v){ this.attributes[k]=v; },
  hasAttribute:function(k){ return this.attributes[k]!==undefined; },
  appendChild:function(n){ this.childNodes.push(n); return n; },
  get innerHTML(){ return this.childNodes.map(function(c){return c.nodeValue||"";}).join(""); },
  set innerHTML(v){}, querySelectorAll:function(){ return []; }, closest:function(){ return null; } }; }
function Txt(v){ return { nodeType:3, nodeValue:v, parentNode:null }; }
function add(p,c){ p.childNodes.push(c); c.parentNode=p; return c; }
function tous(r,acc){ acc=acc||[]; (r.childNodes||[]).forEach(function(c){ acc.push(c); tous(c,acc); }); return acc; }
var html = El("HTML"), body = add(html, El("BODY"));
var sp = add(body, El("SPAN")); var t = add(sp, Txt("Chargement"));
var injectes = [];
var ctx = {
  console: console, setTimeout:function(){return 0;}, clearTimeout:function(){},
  localStorage:{ getItem:function(){return null;}, setItem:function(){} },
  NodeFilter:{ SHOW_TEXT:4 },
  MutationObserver: function(){ this.observe=function(){}; },
  document: {
    readyState:"complete", body:body, documentElement:html,
    head: { appendChild: function (s) {
      injectes.push(s.src);
      ctx.window.HSTAT_I18N = { "Chargement": "Loading" };
      if (s.onload) s.onload();
      return s; } },
    createElement: function (tag) { return { tag: tag, src:null, onload:null, onerror:null }; },
    getElementById:function(){ return null; }, addEventListener:function(){},
    querySelectorAll:function(){ return []; },
    createTreeWalker:function(r){ var l=tous(r).filter(function(n){return n.nodeType===3;}),i=-1;
      return { nextNode:function(){ return ++i<l.length?l[i]:null; } }; }
  }
};
ctx.window = ctx;
ctx.window.HSTAT_I18N_SRC = "hstat-dict/hstat-i18n-en.js?v=test";
ctx.Shiny = { addCustomMessageHandler:function(){}, setInputValue:function(){} };
vm.createContext(ctx);
vm.runInContext(fs.readFileSync(SRC,"utf8"), ctx);
var out = { au_demarrage: injectes.length };
if (MODE === "fr") {
  ctx.window.hstatSetLangue("fr");
  out.apres_fr = injectes.length; out.texte = t.nodeValue;
} else {
  ctx.window.hstatSetLangue("en");
  out.texte = t.nodeValue; out.apres_en = injectes.length;
  ctx.window.hstatSetLangue("fr"); ctx.window.hstatSetLangue("en");
  out.apres_aller_retour = injectes.length;
}
process.stdout.write(JSON.stringify(out));
)---", banc, useBytes = TRUE)

  lancer <- function(mode) {
    s <- suppressWarnings(system2(node, c(shQuote(banc), shQuote(js), mode),
                                  stdout = TRUE, stderr = TRUE))
    if (!length(s)) return(NULL)
    tryCatch(jsonlite::fromJSON(paste(s, collapse = "")), error = function(e) NULL)
  }
  en <- lancer("en"); skip_if(is.null(en), "le banc n'a rien produit")
  fr <- lancer("fr"); skip_if(is.null(fr), "le banc n'a rien produit")

  # 1. AU DEMARRAGE, RIEN N'EST TELECHARGE. C'est tout l'objet du correctif.
  expect_equal(en$au_demarrage, 0L)
  # 2. Rester en francais ne telecharge toujours rien.
  expect_equal(fr$apres_fr, 0L)
  expect_equal(fr$texte, "Chargement")
  # 3. Le passage a l'anglais va chercher le dictionnaire, ET TRADUIT.
  #    Sans la seconde assertion, un chargement qui n'aboutirait pas
  #    passerait pour un succes.
  expect_equal(en$apres_en, 1L)
  expect_equal(en$texte, "Loading")
  # 4. IL N'EST CHARGE QU'UNE FOIS : un aller-retour n'en redemande pas un.
  expect_equal(en$apres_aller_retour, 1L)
})

# ---------------------------------------------------------------------------
#  EPIDEMIOLOGIE
# ---------------------------------------------------------------------------

test_that("l'intervalle d'un comptage est exact, et l'approximation normale sort du domaine", {
  ic <- hstat_epi_ic_poisson(3)
  ref <- stats::poisson.test(3)$conf.int
  expect_equal(ic$bas, as.numeric(ref[1]), tolerance = 1e-6)
  expect_equal(ic$haut, as.numeric(ref[2]), tolerance = 1e-6)
  # ET C'EST CE QUI JUSTIFIE L'EXACT : sur 3 evenements, l'approximation
  # normale rend une borne basse NEGATIVE -- un nombre de deces negatif n'est
  # pas un intervalle. Sans cette assertion, un code qui aurait garde le normal
  # passerait le premier controle a la tolerance pres sur de grands comptages.
  expect_lt(3 - 1.96 * sqrt(3), 0)
  expect_gt(ic$bas, 0)
  # Zero evenement : la borne basse vaut zero, jamais NA.
  expect_equal(hstat_epi_ic_poisson(0)$bas, 0)
  expect_gt(hstat_epi_ic_poisson(0)$haut, 0)
})

test_that("la variance robuste vaut celle du paquet de reference", {
  skip_if_not_installed("sandwich")
  set.seed(11); n <- 300
  x <- stats::rnorm(n); y <- stats::rpois(n, exp(0.3 + 0.5 * x))
  m <- stats::glm(y ~ x, family = stats::poisson())
  V <- hstat_epi_vcov_robuste(m)
  expect_false(is.null(V))
  expect_equal(unname(V), unname(sandwich::sandwich(m)), tolerance = 1e-4)
  # ELLE DOIT DIFFERER DE LA VARIANCE MODELE, sinon l'assertion passerait aussi
  # sur une fonction qui rendrait simplement `vcov(fit)`.
  expect_gt(max(abs(V - stats::vcov(m))), 1e-8)
})

test_that("un retard se lit par sa valeur, jamais par son rang de colonne", {
  skip_if_not_installed("dlnm")
  set.seed(1); n <- 240
  x <- 25 + 6 * sin(2 * pi * seq_len(n) / 12) + stats::rnorm(n)
  y <- stats::rpois(n, exp(1 + 0.03 * x))
  cb <- dlnm::crossbasis(x, lag = 5,
    argvar = list(fun = "ns", knots = stats::quantile(x, c(.25, .5, .75))),
    arglag = list(fun = "ns", knots = dlnm::logknots(5, nk = 2)))
  m <- stats::glm(y ~ cb + splines::ns(seq_len(n), df = 3), family = stats::poisson())
  p <- dlnm::crosspred(cb, m, at = seq(min(x), max(x), length.out = 100),
                       cen = stats::median(x), cumul = TRUE, bylag = 0.5)
  g <- .hstat_epi_lag_grille(p)
  # AU PAS DE 0,5 LA COLONNE `lg + 1` N'EST PAS LE RETARD `lg` : la colonne 6
  # porte le retard 2,5, et un tableau qui l'etiquette « Lag 5 » est complet,
  # plausible et faux. Mesure : 1,10 affiche pour un vrai retard 5 de 0,91 --
  # un effet protecteur publie comme delectere.
  expect_equal(g[6], 2.5)
  expect_equal(.hstat_epi_col_lag(p, 5), 11L)
  expect_false(identical(.hstat_epi_col_lag(p, 5), 6L))
  # Et un retard absent de la grille est rendu ABSENT, pas rapproche : sinon on
  # lirait un chiffre pour un retard que le modele n'a pas evalue.
  expect_true(is.na(.hstat_epi_col_lag(p, 2.25)))
})

test_that("le DLNM retrouve un effet simule et nomme ce qu'il ecarte", {
  skip_if_not_installed("dlnm")
  set.seed(42); n <- 240
  d <- data.frame(date = seq(as.Date("2005-01-01"), by = "month", length.out = n))
  d$tmax <- 30 + 4 * sin(2 * pi * seq_len(n) / 12) + stats::rnorm(n, 0, 1.2)
  d$nais <- stats::rpois(n, 400)
  d$prema <- stats::rpois(n, exp(log(d$nais) - 3.2 + 0.05 * (d$tmax - 30)))
  r <- hstat_epi_dlnm(d, "prema", "tmax", var_offset = "nais", var_temps = "date",
                      lag_max = 5)
  expect_true(r$ok)
  lg <- hstat_epi_dlnm_lags(r, prob = 0.90)
  # L'EFFET SIMULE EST CONTEMPORAIN : il doit ressortir au retard 0 et y etre
  # significatif. Verifier seulement que le tableau a six lignes ne distinguerait
  # pas un modele juste d'un modele qui rendrait 1 partout.
  expect_equal(lg$Retard, 0:5)
  expect_gt(lg$RR[lg$Retard == 0], 1.1)
  expect_gt(lg$IC_bas[lg$Retard == 0], 1)
  # UN DENOMINATEUR NUL EST ECARTE ET COMPTE, jamais remplace par 1 : `log(N+1)`
  # affirmerait qu'un mois sans naissance portait un denominateur de 1, et le
  # taux de ce mois serait entierement fabrique par la constante.
  d2 <- d; d2$nais[1:3] <- 0
  r2 <- hstat_epi_dlnm(d2, "prema", "tmax", var_offset = "nais", var_temps = "date",
                       lag_max = 5)
  expect_true(r2$ok)
  expect_match(r2$message, "3 observation")
  expect_equal(r2$n_utilisees, r$n_utilisees - 3L)
})

test_that("le test global porte sur UNE surface, pas sur toutes", {
  skip_if_not_installed("dlnm")
  set.seed(3); n <- 300; s <- 2 * pi * seq_len(n) / 12
  d <- data.frame(date = seq(as.Date("2000-01-01"), by = "month", length.out = n))
  d$tmax <- 31 + 3.5 * sin(s) + stats::rnorm(n, 0, 1)
  d$hum <- 70 - 12 * sin(s) + stats::rnorm(n, 0, 4)
  d$nais <- stats::rpois(n, 420)
  d$prema <- stats::rpois(n, exp(log(d$nais) - 3.3 + 0.06 * (d$tmax - 31)))
  r <- hstat_epi_dlnm(d, "prema", "tmax", vars_ajust_cb = "hum",
                      var_offset = "nais", var_temps = "date", lag_max = 4)
  expect_true(r$ok)
  wt <- hstat_epi_dlnm_wald(r, "tmax")
  wh <- hstat_epi_dlnm_wald(r, "hum")
  # UN BALAYAGE EN `^cb` PRENDRAIT LES DEUX SURFACES : les deux lignes du
  # tableau porteraient alors le MEME khi-deux sous deux noms differents.
  # L'assertion exige qu'ils soient DISCERNABLES -- sans elle, une fonction qui
  # testerait tout ensemble passerait.
  expect_false(isTRUE(all.equal(wt$chi2, wh$chi2)))
  expect_equal(wt$ddl, wh$ddl)
  # Seule la temperature agissait dans la simulation.
  expect_lt(wt$p, 0.05)
  expect_gt(wh$p, 0.05)
})

test_that("deux expositions correlees sont nommees, et l'ajustement change le verdict", {
  skip_if_not_installed("dlnm")
  set.seed(3); n <- 300; s <- 2 * pi * seq_len(n) / 12
  d <- data.frame(date = seq(as.Date("2000-01-01"), by = "month", length.out = n))
  d$tmax <- 31 + 3.5 * sin(s) + stats::rnorm(n, 0, 1)
  d$hum <- 70 - 12 * sin(s) + stats::rnorm(n, 0, 4)
  d$nais <- stats::rpois(n, 420)
  d$prema <- stats::rpois(n, exp(log(d$nais) - 3.3 + 0.06 * (d$tmax - 31)))
  co <- hstat_epi_collinearite(d, c("tmax", "hum"))
  expect_lt(co$Correlation[1], -0.7)
  expect_match(attr(co, "message"), "corr")
  m <- hstat_epi_dlnm_multi(d, "prema", c("tmax", "hum"), mutuel = TRUE,
                            var_offset = "nais", var_temps = "date", lag_max = 4)
  expect_true(m$ok)
  expect_equal(sort(names(m$resultats)), c("hum", "tmax"))
  expect_equal(nrow(m$comparaison), 2L)
  # LE TABLEAU DE COMPARAISON DOIT DISTINGUER LES EXPOSITIONS : trois lignes
  # identiques seraient la signature du test global mal cible.
  expect_false(isTRUE(all.equal(m$comparaison$Test_global_p[1],
                                m$comparaison$Test_global_p[2])))
})

test_that("l'odds ratio s'ecarte du risque relatif sur une issue frequente", {
  set.seed(7); n <- 2000
  d <- data.frame(expo = stats::rbinom(n, 1, 0.4))
  d$issue <- stats::rbinom(n, 1, stats::plogis(-0.4 + 0.9 * d$expo))
  r <- hstat_epi_risque(d, "issue", "expo")
  expect_true(r$ok)
  expect_gt(r$prevalence, 0.10)
  # L'OR SURESTIME LE RR DES QUE L'ISSUE EST FREQUENTE, et c'est tout l'objet
  # de la fonction. Mesure sur ce jeu : OR 2,36 contre RR 1,53, soit 54 % de
  # surestimation. Une assertion qui ne verifierait que la presence des deux
  # colonnes passerait sur un code qui rendrait l'OR deux fois.
  expect_gt(r$or$OR[1], r$rr$RR[1] * 1.2)
  expect_gt(r$ecart$Surestimation_pct[1], 20)
  expect_match(r$message, "fr.quente")
  # SUR UNE ISSUE RARE, les deux se rejoignent -- et le message change.
  set.seed(8)
  d2 <- data.frame(expo = stats::rbinom(n, 1, 0.4))
  d2$issue <- stats::rbinom(n, 1, stats::plogis(-4.5 + 0.9 * d2$expo))
  r2 <- hstat_epi_risque(d2, "issue", "expo")
  skip_if(!isTRUE(r2$ok))
  expect_lt(abs(r2$or$OR[1] - r2$rr$RR[1]) / r2$rr$RR[1], 0.15)
  expect_match(r2$message, "rare")
})

test_that("une issue binaire se declare : la deviner inverserait l'effet", {
  d <- data.frame(etat = rep(c("Malade", "Sain"), each = 5),
                  expo = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 1))
  # Sans declaration, la colonne n'est pas 0/1 : on REFUSE plutot que de
  # prendre la premiere modalite alphabetique -- qui donnerait « Malade » ici
  # et « Negatif » sur une colonne « Negatif/Positif », soit le meme code
  # rendant l'effet tantot a l'endroit tantot a l'envers.
  b0 <- hstat_epi_binaire(d$etat)
  expect_false(b0$ok)
  b1 <- hstat_epi_binaire(d$etat, "Malade")
  expect_true(b1$ok)
  b2 <- hstat_epi_binaire(d$etat, "Sain")
  expect_true(b2$ok)
  expect_equal(b1$y, 1L - b2$y)
  # Une modalite absente est refusee, pas silencieusement ignoree.
  expect_false(hstat_epi_binaire(d$etat, "Inexistant")$ok)
})

test_that("l'aire sous la courbe ROC vaut la statistique de Mann-Whitney", {
  set.seed(5)
  sc <- c(stats::rnorm(120, 1), stats::rnorm(180, 0))
  et <- c(rep(1L, 120), rep(0L, 180))
  ro <- hstat_epi_roc(sc, et)
  u <- stats::wilcox.test(sc[et == 1], sc[et == 0])$statistic / (120 * 180)
  # DEUX CHEMINS, UN SEUL NOMBRE : l'integration trapezoidale de la courbe et
  # le rang doivent coincider a la precision machine. C'est ce qui permet de
  # verifier l'implementation sans se fier a un paquet.
  expect_equal(ro$auc, ro$auc_rang, tolerance = 1e-12)
  expect_equal(ro$auc, as.numeric(u), tolerance = 1e-12)
  expect_gt(ro$auc, 0.7)
  # Un etat constant n'a pas de courbe : NULL, jamais une aire de 0,5 inventee.
  expect_null(hstat_epi_roc(sc, rep(1L, length(sc))))
})

test_that("la valeur predictive suit la prevalence, pas le tableau", {
  d <- data.frame(ref = c(rep(1L, 100), rep(0L, 100)),
                  tst = c(rep(1L, 99), 0L, rep(0L, 99), 1L))
  a <- hstat_epi_diagnostic(d, "tst", "ref")
  b <- hstat_epi_diagnostic(d, "tst", "ref", prevalence = 0.001)
  expect_true(a$ok && b$ok)
  # Se et Sp sont des proprietes DU TEST : elles ne bougent pas.
  expect_equal(a$table$Valeur[1], b$table$Valeur[1])
  expect_equal(a$table$Valeur[2], b$table$Valeur[2])
  # LA VPP EST UNE PROPRIETE DU TEST DANS UNE POPULATION : a 0,1 % de
  # prevalence, un test a 99/99 rend 9 % de VPP -- neuf « positifs » sur dix
  # sont sains. Lire la VPP d'un plan cas-temoins comme si elle valait en
  # population est l'erreur la plus couteuse du depistage.
  expect_gt(a$table$Valeur[3], 0.95)
  expect_lt(b$table$Valeur[3], 0.15)
  expect_match(b$message, "pr.valence")
})

test_that("Cox rend le meme rapport que le paquet, et l'hypothese PH est testee", {
  skip_if_not_installed("survival")
  set.seed(11); n <- 300
  g <- factor(sample(c("A", "B"), n, TRUE))
  tp <- stats::rexp(n, rate = ifelse(g == "B", 0.09, 0.03))
  cen <- stats::runif(n, 0, 40)
  d <- data.frame(duree = pmin(tp, cen), evt = as.integer(tp <= cen), grp = g)
  r <- hstat_epi_survie(d, "duree", "evt", "grp")
  expect_true(r$ok)
  ref <- exp(stats::coef(survival::coxph(
    survival::Surv(d$duree, d$evt) ~ d$grp)))
  expect_equal(r$cox$HR[1], round(unname(ref), 4), tolerance = 1e-4)
  expect_lt(r$logrank$p, 0.001)
  # L'HYPOTHESE PH EST TESTEE ET RENDUE : la taire laisserait publier un HR
  # unique qui, si elle tombe, ne decrit aucun instant de l'etude.
  expect_true(!is.null(r$ph))
  expect_true("GLOBAL" %in% r$ph$Terme)
  # Tout censure : aucune survie estimable, et on le dit.
  d0 <- d; d0$evt <- 0L
  expect_false(hstat_epi_survie(d0, "duree", "evt", "grp")$ok)
})

test_that("le cas-croise exige un pas journalier et retrouve l'effet simule", {
  set.seed(13); N <- 730
  dt <- seq(as.Date("2020-01-01"), by = "day", length.out = N)
  x <- 25 + 8 * sin(2 * pi * seq_len(N) / 365) + stats::rnorm(N, 0, 2)
  d <- data.frame(date = dt, cas = stats::rpois(N, exp(1.5 + 0.04 * (x - 25))),
                  tmax = x)
  r <- hstat_epi_cas_croise(d, "date", "cas", "tmax")
  expect_true(r$ok)
  expect_gt(r$strates, 100L)
  # L'intervalle doit CONTENIR la verite simulee : verifier seulement que le
  # RR est superieur a 1 passerait sur un modele qui surestimerait du double.
  expect_lt(r$coefs$IC_bas[1], exp(0.04))
  expect_gt(r$coefs$IC_haut[1], exp(0.04))
  # SUR DU MENSUEL LA STRATE NE CONTIENT QU'UNE LIGNE : il n'y a plus de
  # jour-temoin, et le modele ne peut rien estimer. On refuse en le disant.
  dm <- data.frame(date = seq(as.Date("2020-01-01"), by = "month", length.out = 60),
                   cas = stats::rpois(60, 10), tmax = stats::rnorm(60, 25))
  rm <- hstat_epi_cas_croise(dm, "date", "cas", "tmax")
  expect_false(rm$ok)
  expect_match(rm$message, "journali")
})

test_that("le SMR vaut le rapport observe/attendu et son intervalle est exact", {
  d <- data.frame(
    zone = rep(c("Nord", "Sud"), each = 4),
    age = rep(c("0-19", "20-39", "40-59", "60+"), 2),
    pop = c(5000, 6000, 4000, 2000, 3000, 3000, 5000, 6000),
    dec = c(2, 5, 20, 60, 1, 3, 26, 190),
    txref = rep(c(0.0004, 0.0008, 0.005, 0.030), 2))
  r <- hstat_epi_smr(d, "dec", "pop", "txref", "age", "zone")
  expect_true(r$ok)
  o <- sum(d$dec[1:4]); e <- sum(d$pop[1:4] * d$txref[1:4])
  expect_equal(r$smr$SMR[r$smr$Groupe == "Nord"], round(o / e, 4), tolerance = 1e-6)
  # L'INTERVALLE VIENT DU COMPTAGE, donc il est exact : l'approximation normale
  # donnerait une borne basse differente, et sur de petits comptages negative.
  ic <- hstat_epi_ic_poisson(o)
  expect_equal(r$smr$IC_bas[r$smr$Groupe == "Nord"], round(ic$bas / e, 4),
               tolerance = 1e-6)
  # SANS TAUX DE REFERENCE on ne les invente pas : on les derive du fichier, et
  # on DIT que le SMR moyen vaut alors 1 par construction -- sans quoi on
  # croirait a une comparaison nationale.
  r2 <- hstat_epi_smr(d, "dec", "pop", NULL, "age", "zone")
  expect_true(r2$ok)
  expect_match(r2$message, "interne")
})

test_that("les mesures d'impact sont exactes, et le signe change le libelle", {
  r <- hstat_epi_impact(40, 60, 20, 80)
  expect_true(r$ok)
  v <- stats::setNames(r$table$Valeur, r$table$.cle)
  expect_equal(r$table$Valeur[r$table$Mesure == tr("Risque relatif (RR)")], 2)
  expect_equal(r$table$Valeur[r$table$Mesure == tr("Risque attribuable (RA)")], 0.2)
  expect_equal(r$table$Valeur[r$table$Mesure ==
    tr("Fraction attribuable chez les exposés (FAE)")], 0.5)
  expect_equal(r$table$Valeur[r$table$Mesure ==
    tr("Fraction attribuable en population (FAP)")], 1 / 3, tolerance = 1e-3)
  # UNE CASE NULLE EST CORRIGEE ET ON LE DIT : l'appliquer en silence rendrait
  # des intervalles qui ne sont plus ceux des donnees brutes.
  r0 <- hstat_epi_impact(0, 60, 20, 80)
  expect_true(r0$corrige)
  expect_match(r0$message, "Haldane")
  # UN RISQUE ATTRIBUABLE NEGATIF EST UN RESULTAT : l'exposition protege, et le
  # libelle bascule de « a exposer » a « a traiter ». Le borner a zero
  # masquerait precisement ce qu'il faut voir.
  rn <- hstat_epi_impact(20, 80, 40, 60)
  expect_lt(rn$table$Valeur[rn$table$Mesure == tr("Risque attribuable (RA)")], 0)
  expect_true(tr("Nombre de sujets à traiter (NST)") %in% rn$table$Mesure)
  expect_false(tr("Nombre de sujets à exposer pour un cas (NSE)") %in% rn$table$Mesure)
})

test_that("chaque analyse d'epidemiologie a son catalogue et ses figures", {
  # LE CATALOGUE EST DECLARE UNE FOIS : le selecteur, l'aide et le module en
  # derivent. Une analyse sans figure rendrait un onglet Graphique vide.
  expect_setequal(names(HSTAT_EPI_FIGURES), names(HSTAT_EPI_ANALYSES))
  for (k in names(HSTAT_EPI_ANALYSES)) {
    expect_equal(length(HSTAT_EPI_ANALYSES[[k]]), 4L, info = k)
    expect_true(all(nzchar(HSTAT_EPI_ANALYSES[[k]])), info = k)
    expect_gt(length(HSTAT_EPI_FIGURES[[k]]), 0L)
  }
  # Les figures tracees en graphiques de base sont DECLAREES : c'est ce qui
  # permet de retirer le panneau de mise en forme ggplot, qu'elles ignorent.
  expect_true(all(HSTAT_EPI_FIGURES_BASE %in% unlist(HSTAT_EPI_FIGURES)))
})

test_that("les six figures du DLNM se construisent reellement", {
  skip_if_not_installed("dlnm")
  set.seed(21); n <- 240
  d <- data.frame(date = seq(as.Date("2005-01-01"), by = "month", length.out = n))
  d$tmax <- 30 + 4 * sin(2 * pi * seq_len(n) / 12) + stats::rnorm(n, 0, 1.2)
  d$nais <- stats::rpois(n, 400)
  d$prema <- stats::rpois(n, exp(log(d$nais) - 3.2 + 0.05 * (d$tmax - 30)))
  r <- hstat_epi_dlnm(d, "prema", "tmax", var_offset = "nais", var_temps = "date",
                      lag_max = 5)
  skip_if(!isTRUE(r$ok))
  # COMPTER LES ENTREES D'UN CATALOGUE NE DIT RIEN DE CE QU'ELLES PRODUISENT :
  # c'est `ggplot_build` qui revele une echelle incompatible, et l'ecriture du
  # fichier qui revele un tracage de base qui leve.
  for (f in names(HSTAT_EPI_FIGURES$dlnm)) {
    p <- hstat_epi_figure("dlnm", HSTAT_EPI_FIGURES$dlnm[[f]], r,
                          o = list(theme = "minimal", police = 11))
    expect_false(is.null(p), info = f)
    if (is.function(p)) {
      tf <- tempfile(fileext = ".png")
      ok <- hstat_ecrire_image(tf, p, "png", 8, 6, 96, secours = FALSE)
      expect_true(ok && file.exists(tf) && file.size(tf) > 1000, info = f)
      unlink(tf)
    } else {
      expect_silent(invisible(ggplot2::ggplot_build(p)))
    }
  }
})

test_that("le module d'epidemiologie lance une analyse et rend ses tableaux", {
  skip_if_not_installed("dlnm")
  skip_if_not_installed("shiny")
  set.seed(31); n <- 200
  d <- data.frame(date = seq(as.Date("2008-01-01"), by = "month", length.out = n))
  d$tmax <- 29 + 3 * sin(2 * pi * seq_len(n) / 12) + stats::rnorm(n, 0, 1)
  d$nais <- stats::rpois(n, 350)
  d$prema <- stats::rpois(n, exp(log(d$nais) - 3.1 + 0.05 * (d$tmax - 29)))
  vals <- shiny::reactiveValues(data = d, cleanData = NULL, filteredData = NULL,
                                resetSignal = 0L)
  # LE TEST PORTE SUR LE MODULE, PAS SUR LA FONCTION : un test qui appellerait
  # `hstat_epi_dlnm()` resterait vert pendant que le module ne l'emploie pas.
  shiny::testServer(mod_epidemio_server, args = list(values = vals), {
    session$setInputs(epiAnalyse = "dlnm", epiY = "prema", epiExpo = "tmax",
                      epiOffset = "nais", epiTemps = "date", epiMutuel = TRUE,
                      epiLag = 4, epiNkLag = 2, epiFamille = "auto",
                      epiPeriode = 12, epiHarmo = 2, epiTendance = 3,
                      epiConf = 0.95, epiAjust = character(0),
                      epiLancer = 1)
    expect_true(!is.null(rv$res))
    expect_equal(rv$analyse, "dlnm")
    tb <- tables()
    expect_true(is.list(tb) && length(tb) >= 2L)
    expect_true("Comparaison" %in% names(tb))
    # LES TROIS EMPLACEMENTS FIGES ONT DISPARU avec les reactifs `t1`/`t2`/`t3` :
    # l'ecran porte desormais TOUS les tableaux, par des sorties construites.
    # On verifie donc ce que l'utilisateur voit -- le conteneur les emet, et
    # chacun rend ses lignes -- plutot qu'un reactif qui n'existe plus.
    expect_true(NROW(tb[[1]]) > 0)
    expect_false(is.null(output$epiTablesUI))
    for (k in seq_len(min(length(tb), 3L)))
      expect_match(output$epiTablesUI$html, paste0("epiTable", k), fixed = TRUE)
    expect_true(NROW(output$epiTable1) > 0 || nzchar(output$epiTable1 %||% ""))
    # LE SELECTEUR DE FIGURE SE CONSTRUIT AVANT QU'AUCUNE FIGURE SOIT CHOISIE.
    # `input$epiFigure` y vaut NULL, et `NULL %in% choix` rend `logical(0)` :
    # `if()` leve « argument is of length zero », l'erreur tombe dans le
    # `renderUI`, et LE SELECTEUR N'EXISTE JAMAIS -- donc aucune figure ne se
    # trace sur une analyse pourtant calculee.
    #
    # Le defaut a ete trouve AU NAVIGATEUR, pas ici : le test posait
    # `epiFigure` avant de lire `figure()`, et n'exercait donc jamais le
    # `renderUI`. On le lit desormais AVANT tout choix.
    expect_false(is.null(output$epiFigureUI))
    expect_match(output$epiFigureUI$html, "epiFigure", fixed = TRUE)
    session$setInputs(epiFigure = "cumul")
    expect_false(is.null(figure()))
  })
})

test_that("une colonne de date en caracteres se lit comme une colonne typee", {
  skip_if_not_installed("dlnm")
  set.seed(5); n <- 240
  d <- data.frame(Date = seq(as.Date("2005-01-01"), by = "month", length.out = n))
  d$tmax <- 30 + 4 * sin(2 * pi * seq_len(n) / 12) + stats::rnorm(n, 0, 1.2)
  d$nais <- stats::rpois(n, 400)
  d$prema <- stats::rpois(n, exp(log(d$nais) - 3.2 + 0.05 * (d$tmax - 30)))

  # LE CAS ORDINAIRE EST LA COLONNE DE CARACTERES : c'est ce que rend un CSV lu
  # sans typage. `hstat_date_parse()` nu y levait « l'argument "fmt" est
  # manquant », et le `tryCatch` de `hstat_epi_dlnm_multi()` le rendait sous
  # « L'analyse a échoué » -- un motif qui n'apprend rien.
  #
  # LE TEST PORTE SUR LA COLONNE EN CARACTERES, et c'est tout son objet : une
  # colonne deja typee `Date` prend l'autre branche et passe avec ou sans le
  # correctif. C'est exactement pourquoi le test du module et le parcours au
  # navigateur l'avaient tous deux manque -- aucun des deux n'exercait le chemin
  # qui casse.
  d2 <- d; d2$Date <- as.character(d2$Date)
  r2 <- hstat_epi_dlnm(d2, "prema", "tmax", var_offset = "nais",
                       var_temps = "Date", lag_max = 5)
  expect_true(r2$ok)

  # ET LA LECTURE DOIT ETRE LA MEME, pas seulement « ne pas echouer » : un
  # lecteur qui rendrait des dates de travers passerait la premiere assertion.
  r1 <- hstat_epi_dlnm(d, "prema", "tmax", var_offset = "nais",
                       var_temps = "Date", lag_max = 5)
  expect_true(r1$ok)
  expect_equal(r2$aic, r1$aic, tolerance = 1e-8)
  expect_equal(r2$n_utilisees, r1$n_utilisees)
  # Le format retenu est NOMME : une lecture automatique qui ne se dit pas
  # laisse l'utilisateur sans moyen de vérifier qu'elle a pris la bonne.
  expect_match(r2$message, "%Y-%m-%d", fixed = TRUE)

  # Le chemin reel du module passe par `_multi`, dont le `tryCatch` masquait la
  # cause : on verifie qu'il aboutit, pas seulement la fonction sous-jacente.
  m <- hstat_epi_dlnm_multi(d2, "prema", "tmax", var_offset = "nais",
                            var_temps = "Date", lag_max = 5)
  expect_true(m$ok)
  expect_equal(names(m$resultats), "tmax")

  # Le cas-croise lisait la date par le meme appel fautif.
  N <- 400
  dc <- data.frame(date = as.character(seq(as.Date("2020-01-01"), by = "day",
                                           length.out = N)),
                   cas = stats::rpois(N, 12), tmax = stats::rnorm(N, 25, 3))
  expect_true(hstat_epi_cas_croise(dc, "date", "cas", "tmax")$ok)
})

# ---------------------------------------------------------------------------
#  UNE DATE SE COMPOSE PARFOIS DE DEUX COLONNES
# ---------------------------------------------------------------------------
test_that("le mois se lit en toutes lettres, abrege ou en numero", {
  expect_equal(hstat_epi_mois_num(c("février", "Fevr.", "March", " mars ")),
               c(2L, 2L, 3L, 3L))
  expect_equal(hstat_epi_mois_num(c(7, "07", "12")), c(7L, 7L, 12L))
  # LA CORRESPONDANCE EST EXACTE, JAMAIS PAR PREFIXE. Un libelle tronque --
  # « avri », « fevrie » -- doit etre NOMME, pas devine : un mois faux est
  # parfaitement plausible en tableau et decale toute la serie d'un rang.
  #
  # L'assertion porte sur un prefixe NON AMBIGU, et c'est ce qui la rend utile :
  # « ju » designe six entrees, si bien qu'un code a prefixe rendrait `NA` lui
  # aussi -- une assertion qui ne distingue pas les deux codes ne garde rien.
  # Mesure : « avri » vaut NA en correspondance exacte et 4 en correspondance
  # par prefixe ; « ju » vaut NA des deux cotes.
  expect_true(all(is.na(hstat_epi_mois_num(c("avri", "fevrie", "ju")))))
  expect_true(all(is.na(hstat_epi_mois_num(c("0", "13", "", NA, "bloc A")))))
})

test_that("la composition mois + annee nomme ce qu'elle ecarte", {
  r <- hstat_epi_date_mois_annee(c("janvier", "Feb", "13e mois", "mars"),
                                 c(2005, 2005, 2005, 5))
  expect_equal(as.character(r$dates[1:2]), c("2005-01-01", "2005-02-01"))
  # Le jour est une HYPOTHESE, donc elle se dit.
  expect_match(r$message, "premier du mois")
  # Un mois inconnu est NOMME, pas retire en silence.
  expect_equal(r$ecartes, "13e mois")
  expect_match(r$message, "13e mois", fixed = TRUE)
  # UNE ANNEE A DEUX CHIFFRES EST AMBIGUE : « 05 » vaut l'an 5 ou 2005 selon
  # la convention, deux series distantes de deux millenaires.
  expect_true(is.na(r$dates[4]))
  expect_match(r$message, "deux chiffres")
})

test_that("le DLNM se calcule depuis un couple mois + annee", {
  skip_if_not_installed("dlnm")
  set.seed(1)
  n <- 96
  d <- data.frame(Date = seq(as.Date("2005-01-01"), by = "month", length.out = n))
  d$Annees <- as.integer(format(d$Date, "%Y"))
  d$Mois <- HSTAT_MOIS[["fr"]][as.integer(format(d$Date, "%m"))]
  d$tmax <- 30 + 4 * sin(2 * pi * seq_len(n) / 12) + rnorm(n)
  d$nais <- rpois(n, 400)
  d$prema <- rpois(n, 20 + 0.4 * (d$tmax - 30))

  a <- hstat_epi_dlnm(d, "prema", "tmax", var_offset = "nais",
                      var_temps = "Date", lag_max = 5)
  b <- hstat_epi_dlnm(d, "prema", "tmax", var_offset = "nais",
                      var_mois = "Mois", var_annee = "Annees", lag_max = 5)
  expect_true(a$ok); expect_true(b$ok)
  # LE FICHIER PORTE LA MEME INFORMATION EN DEUX MORCEAUX : le resultat doit
  # etre le MEME, pas seulement « calculable ».
  expect_equal(b$aic, a$aic, tolerance = 1e-8)
  expect_equal(b$n_utilisees, a$n_utilisees)
  expect_match(b$message, "premier du mois")

  # Un seul des deux champs ne compose rien, et on le dit.
  s <- hstat_epi_temps(d, NULL, "Mois", NULL)
  expect_null(s$dates)
  expect_match(s$message, "ensemble")

  # Le passage par `...` de la version multi-expositions.
  m <- hstat_epi_dlnm_multi(d, "prema", "tmax", var_offset = "nais",
                            var_mois = "Mois", var_annee = "Annees",
                            lag_max = 5)
  expect_true(m$ok)
})

# ---------------------------------------------------------------------------
#  UNE DATE COMPLETE SE DECOMPOSE
# ---------------------------------------------------------------------------
test_that("l'extraction ajoute des colonnes, elle ne remplace rien", {
  d <- data.frame(Date = c("2026-01-05", "2026-08-04", "2026-12-31"),
                  y = 1:3, stringsAsFactors = FALSE)
  r <- hstat_epi_date_parts(d, "Date", c("annee", "mois", "jour", "sem"))
  expect_equal(r$ajoutees,
               c("Date_annee", "Date_mois", "Date_jour", "Date_jour_sem"))
  # LA COLONNE D'ORIGINE EST CONSERVEE, et les autres aussi : une extraction
  # qui remplacerait ferait perdre la date au fichier qui la portait.
  expect_true(all(c("Date", "y") %in% names(r$data)))
  expect_identical(r$data$Date, d$Date)
  expect_identical(r$data$y, d$y)
  expect_equal(r$data$Date_annee, c(2026L, 2026L, 2026L))
  expect_equal(r$data$Date_jour, c(5L, 4L, 31L))
  expect_match(r$message, "conservée")
})

test_that("le mois sort en facteur non ordonne, pas en nombre", {
  # Le DIMANCHE est indispensable : `%u` (1 = lundi) et `%w` (0 = dimanche) ne
  # different QUE sur lui -- lundi, mardi et jeudi valent 1, 2 et 4 des deux
  # cotes. Sans lui, un code indexant par `%w` passerait l'assertion.
  d <- data.frame(Date = as.Date(c("2026-01-05", "2026-08-04", "2026-12-31",
                                   "2026-01-04")))
  r <- hstat_epi_date_parts(d, "Date", c("mois", "sem"))
  m <- r$data$Date_mois
  expect_s3_class(m, "factor")
  expect_equal(levels(m), sprintf("%02d", 1:12))
  # UN MOIS EN NOMBRE ENTRERAIT LINEAIREMENT dans le modele -- « décembre =
  # 12 x janvier », plausible en tableau et faux ; un facteur ORDONNE y
  # poserait des contrastes polynomiaux (.L, .Q), le meme defaut sous un autre
  # nom. L'assertion porte donc sur la matrice du modele, seul endroit ou les
  # trois codes se distinguent.
  expect_false(is.ordered(m))
  mm <- stats::model.matrix(~ Date_mois, data = r$data)
  expect_true(any(grepl("Date_mois08$", colnames(mm))))
  expect_false(any(grepl("[.][LQC]$", colnames(mm))))

  # 5 janvier 2026 = lundi, 4 aout = mardi, 31 decembre = jeudi, 4 janvier =
  # dimanche.
  s <- r$data$Date_jour_sem
  expect_s3_class(s, "factor")
  expect_equal(levels(s)[1], "lundi")
  expect_equal(levels(s)[7], "dimanche")
  expect_equal(as.character(s), c("lundi", "mardi", "jeudi", "dimanche"))
})

test_that("l'extraction nomme ce qu'elle ne peut pas lire et ce qu'elle renomme", {
  d <- data.frame(Date = c("2026-01-05", "pas une date", "2026-12-31"),
                  Date_mois = c("a", "b", "c"), stringsAsFactors = FALSE)
  r <- hstat_epi_date_parts(d, "Date", c("annee", "mois"))
  # Une ligne illisible ne disparait pas en silence.
  expect_match(r$message, "1 ligne")
  expect_true(is.na(r$data$Date_annee[2]))
  # Une homonyme n'est pas ECRASEE : le fichier perdrait une colonne sans un mot.
  expect_true("Date_mois.1" %in% r$ajoutees)
  expect_identical(r$data$Date_mois, d$Date_mois)
  expect_match(r$message, "existe déjà")

  # Trois refus, chacun avec son motif.
  expect_length(hstat_epi_date_parts(d, "", "annee")$ajoutees, 0L)
  expect_length(hstat_epi_date_parts(d, "Date", character(0))$ajoutees, 0L)
  r0 <- hstat_epi_date_parts(data.frame(x = c("a", "b")), "x", "annee")
  expect_length(r0$ajoutees, 0L)
  expect_match(r0$message, "date")
})

test_that("le module depose les colonnes extraites dans le jeu de travail", {
  d <- data.frame(Date = c("2026-01-05", "2026-08-04", "2026-12-31"),
                  y = 1:3, stringsAsFactors = FALSE)
  vals <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d,
                                resetSignal = 0)
  shiny::testServer(mod_epidemio_server, args = list(values = vals), {
    session$setInputs(epiAnalyse = "dlnm", epiTemps = "Date",
                      epiParties = c("annee", "mois"), epiExtraire = 1)
    # LE TEST PORTE SUR LE MODULE, pas sur la fonction : un test qui n'appelle
    # que `hstat_epi_date_parts()` resterait vert pendant que le bouton ne
    # publie rien, et les colonnes seraient inatteignables partout ailleurs.
    expect_true(all(c("Date_annee", "Date_mois") %in% names(values$filteredData)))
    expect_true(all(c("Date_annee", "Date_mois") %in% names(values$data)))
    expect_true(all(c("Date_annee", "Date_mois") %in% names(values$cleanData)))
    expect_equal(values$filteredData$Date_annee, c(2026L, 2026L, 2026L))
  })
})

# ---------------------------------------------------------------------------
#  UN INTERVALLE ENORME EST UN BUDGET DE PARAMETRES
# ---------------------------------------------------------------------------
.hstat_epi_jeu <- function(n = 118) {
  set.seed(7)
  d <- data.frame(Date = seq(as.Date("2015-01-01"), by = "month", length.out = n))
  d$Tmax <- round(30 + 4 * sin(2 * pi * seq_len(n) / 12) + rnorm(n), 2)
  d$HR   <- round(70 - 0.8 * (d$Tmax - 30) + rnorm(n, 0, 5), 1)
  d$nais <- rpois(n, 40); d$prema <- rpois(n, 4); d$fpn <- rpois(n, 6)
  d
}

test_that("le budget de parametres nomme chaque terme et son cout", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r <- hstat_epi_dlnm_multi(d, "prema", c("Tmax", "HR"), var_offset = "nais",
                            var_temps = "Date", lag_max = 5)
  b <- hstat_epi_dlnm_budget(r$resultats[[1]])
  expect_true(is.data.frame(b))
  # LA SOMME DES TERMES EST LE NOMBRE DE PARAMETRES : un budget dont les lignes
  # ne somment pas au total decrirait un autre modele.
  expect_equal(sum(b$Parametres), attr(b, "n_par"))
  expect_equal(attr(b, "n_par"), length(stats::coef(r$resultats[[1]]$model)))
  # Les DEUX surfaces sont nommees -- l'ajustement mutuel en pose une par
  # exposition, et c'est la moitie du budget.
  expect_equal(sum(grepl("Tmax|HR", b$Terme)), 2L)
  expect_equal(attr(b, "ratio"), attr(b, "n_obs") / attr(b, "n_par"))
  expect_equal(attr(b, "verdict"), "insuffisant")
  # Le message CHIFFRE les leviers : un conseil qui ne chiffre pas se lit
  # comme une generalite.
  expect_match(attr(b, "message"), "INSUFFISANT")
  expect_match(attr(b, "message"), "nœud")
  expect_match(attr(b, "message"), "spline")
})

test_that("retirer des noeuds retrecit reellement l'intervalle", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  mes <- function(code) {
    r <- hstat_epi_dlnm(d, "prema", "Tmax", var_offset = "nais", var_temps = "Date",
                        lag_max = 5, nk_lag = 2,
                        pct_noeuds = hstat_epi_noeuds_probs(code))
    tb <- hstat_epi_dlnm_rr(r, 0.90)
    c(par = length(stats::coef(r$model)), largeur = tb$IC_haut[1] / tb$IC_bas[1])
  }
  k3 <- mes("k3"); li <- mes("lin")
  # C'EST LA MESURE QUI REPOND, pas une intuition : le retard etait DEJA lisse
  # par une spline, et c'est la souplesse de l'EXPOSITION qui pese.
  expect_lt(li[["par"]], k3[["par"]])
  expect_lt(li[["largeur"]], k3[["largeur"]])
  # Un code inconnu retombe sur le defaut, JAMAIS sur le lineaire : une faute
  # de frappe ne doit pas changer la forme du modele en silence.
  expect_equal(hstat_epi_noeuds_probs("n'importe quoi"),
               HSTAT_EPI_NOEUDS_EXPO$k3$probs)
  expect_length(hstat_epi_noeuds_probs("lin"), 0L)
  # La liste de choix DERIVE du catalogue : elle ne peut pas en diverger.
  expect_setequal(unname(hstat_epi_noeuds_choix()), names(HSTAT_EPI_NOEUDS_EXPO))
})

# ---------------------------------------------------------------------------
#  PLUSIEURS EXPOSITIONS, PLUSIEURS ISSUES
# ---------------------------------------------------------------------------
test_that("les expositions se representent ensemble, jamais sur un axe commun en unites", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r <- hstat_epi_dlnm_multi(d, "prema", c("Tmax", "HR"), var_offset = "nais",
                            var_temps = "Date", lag_max = 5)
  for (f in HSTAT_EPI_MULTI_FIG) for (m in c("facettes", "percentiles")) {
    p <- hstat_epi_figure_multi(f, r$resultats, mode = m)
    expect_s3_class(p, "ggplot")
    # `ggplot_build` EST LA SEULE ETAPE ou une figure mal composee se signale :
    # elle s'assemble sans un mot.
    expect_silent(b <- ggplot2::ggplot_build(p))
  }
  # DEUX UNITES NE PARTAGENT PAS UN AXE. En facettes, chaque exposition a son
  # panneau ; en superposition, l'axe est le PERCENTILE, donc sans dimension.
  bf <- ggplot2::ggplot_build(hstat_epi_figure_multi("cumul", r$resultats, "facettes"))
  bp <- ggplot2::ggplot_build(hstat_epi_figure_multi("cumul", r$resultats, "percentiles"))
  expect_equal(length(unique(bf$data[[1]]$PANEL)), 2L)
  expect_equal(length(unique(bp$data[[1]]$PANEL)), 1L)
  expect_true(all(bp$data[[1]]$x >= 0 & bp$data[[1]]$x <= 100))
  # Une CARTE ne se superpose pas : deux rasters l'un sur l'autre ne laissent
  # voir que le dernier. Le mode demande est donc ignore, et en facettes.
  for (f in HSTAT_EPI_MULTI_FACETTES) {
    bc <- ggplot2::ggplot_build(hstat_epi_figure_multi(f, r$resultats, "percentiles"))
    expect_equal(length(unique(bc$data[[1]]$PANEL)), 2L, info = f)
  }
  # LES DEUX LISTES DISENT L'INVERSE, ET ELLES N'EN FAISAIENT QU'UNE. Les
  # coupes SUPERPOSENT les expositions -- leurs facettes portent le RETARD --
  # et c'est legitime parce que leur axe est deja le percentile. Rangees avec
  # la carte, elles faisaient annoncer a l'ecran qu'elles ne se superposaient
  # pas, sous une figure ou les courbes se superposent. Les deux listes sont
  # donc DISJOINTES, et c'est l'assertion qui separe les deux comportements :
  # une carte a deux panneaux pour deux expositions, les coupes en ont six
  # pour six retards, toutes expositions confondues.
  expect_equal(intersect(HSTAT_EPI_MULTI_FACETTES, HSTAT_EPI_MULTI_PCT), character(0))
  # CE QUE LA LISTE GOUVERNE VRAIMENT : le selecteur de disposition ne s'offre
  # que sur les figures qui savent porter les deux modes. En proposer un aux
  # deux autres donnerait un reglage que l'image ignore -- et c'est la
  # condition du `conditionalPanel` du module, ecrite ici sous sa forme R.
  expect_setequal(setdiff(HSTAT_EPI_MULTI_FIG,
                          c(HSTAT_EPI_MULTI_FACETTES, HSTAT_EPI_MULTI_PCT)),
                  c("cumul", "retard"))
  # LA COUCHE SE CHOISIT PAR SA GEOMETRIE, JAMAIS PAR SON RANG. `data[[1]]`
  # est ici le `geom_hline` de reference : il n'a pas de colonne `x`, si bien
  # que `all(x >= 0)` y vaut TRUE SUR LE VIDE -- une assertion qui passe sans
  # rien mesurer, et qui se trompe dans le sens rassurant. C'est le meme piege
  # que la mesure du cadre carre, et il s'est represente ici.
  cl <- function(b) b$data[[which(vapply(b$plot$layers,
    function(l) inherits(l$geom, "GeomLine"), logical(1)))[1]]]
  bk <- cl(ggplot2::ggplot_build(hstat_epi_figure_multi("coupes", r$resultats, "facettes")))
  expect_gt(length(unique(bk$PANEL)), 2L)
  # Le MODE demande est sans effet sur elles : l'axe reste le percentile.
  bk2 <- cl(ggplot2::ggplot_build(hstat_epi_figure_multi("coupes", r$resultats, "percentiles")))
  expect_equal(bk$x, bk2$x)
  expect_gt(length(bk$x), 0L)
  expect_true(all(bk$x >= 0 & bk$x <= 100))
  # Et chaque panneau porte bien LES DEUX expositions, pas une seule : c'est
  # exactement ce que la carte, elle, ne peut pas faire.
  expect_equal(length(unique(bk$colour)), 2L)
  expect_equal(length(unique(bk$colour[bk$PANEL == bk$PANEL[1]])), 2L)

  # Une seule exposition, ou une figure qui n'en porte qu'une : `NULL`, et
  # l'appelant retombe sur l'exposition courante en le disant.
  expect_null(hstat_epi_figure_multi("cumul", r$resultats[1]))
  expect_null(hstat_epi_figure_multi("surface3d", r$resultats))
})

test_that("plusieurs issues se lancent en une fois, et la cle reste plate", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r1 <- hstat_epi_dlnm_multi(d, "prema", c("Tmax", "HR"), var_offset = "nais",
                             var_temps = "Date", lag_max = 5)
  r2 <- hstat_epi_dlnm_multi(d, c("prema", "fpn"), c("Tmax", "HR"),
                             var_offset = "nais", var_temps = "Date", lag_max = 5)
  # UNE SEULE ISSUE NE CHANGE RIEN : la cle reste le nom de l'exposition, donc
  # tout ce qui lit `resultats` continue de fonctionner. C'est ce qui rend
  # l'extension sure, et c'est l'assertion qui le garde.
  expect_equal(names(r1$resultats), c("Tmax", "HR"))
  expect_false("Issue" %in% names(r1$comparaison))
  expect_length(r2$resultats, 4L)
  expect_true(all(grepl("prema|fpn", names(r2$resultats))))
  expect_setequal(r2$comparaison$Issue, c("prema", "fpn"))
  # L'AJUSTEMENT MUTUEL RESTE DANS UNE ISSUE : on ajuste une exposition sur les
  # autres EXPOSITIONS, jamais sur une autre issue -- ce serait expliquer une
  # variable a expliquer par une autre.
  expect_equal(r2$resultats[[1]]$vars_ajust_cb, "HR")
  expect_equal(r2$resultats[[1]]$var_y, "prema")
  expect_equal(r2$resultats[[3]]$var_y, "fpn")
  expect_match(hstat_epi_dlnm_multi(d, character(0), "Tmax")$message, "issue")
})

# ---------------------------------------------------------------------------
#  L'ETIQUETTE DE LA LIGNE DE REFERENCE
# ---------------------------------------------------------------------------
test_that("la ligne de repere porte son etiquette, et seulement si on la demande", {
  cou <- function(l) vapply(l, function(x) class(x$geom)[1], character(1))
  # Sans texte : le trait seul.
  expect_equal(cou(hstat_ligne_repere(3, "v")), "GeomVline")
  expect_equal(cou(hstat_ligne_repere(3, "h", etiquette = "")), "GeomHline")
  # Avec texte : le trait ET l'etiquette.
  for (pos in c("haut", "milieu", "bas")) {
    l <- hstat_ligne_repere(3, "v", etiquette = "Réf.", position = pos,
                            etendue = c(0, 10))
    expect_equal(cou(l), c("GeomVline", "GeomText"))
  }
  # « MILIEU » SANS ETENDUE NE PEUT PAS ETRE PLACE : on retombe sur le haut
  # plutot que de poser l'etiquette hors du cadre, ou elle disparaitrait sans
  # un mot. L'ordonnee le prouve -- finie avec l'etendue, infinie sans.
  av <- hstat_ligne_repere(3, "v", etiquette = "R", position = "milieu",
                           etendue = c(0, 10))[[2]]
  sa <- hstat_ligne_repere(3, "v", etiquette = "R", position = "milieu")[[2]]
  expect_true(is.finite(av$data$y))
  expect_false(is.finite(sa$data$y))
  # Une valeur non finie ne pose rien du tout.
  expect_length(hstat_ligne_repere(NA_real_, "v", etiquette = "R"), 0L)
  # Le COTE change le calage le long du trait, pas sa position.
  d <- hstat_ligne_repere(3, "v", etiquette = "R", cote = "droite")[[2]]
  g <- hstat_ligne_repere(3, "v", etiquette = "R", cote = "gauche")[[2]]
  expect_false(identical(d$aes_params$vjust, g$aes_params$vjust))
})

# ---------------------------------------------------------------------------
#  PLUSIEURS DENOMINATEURS, ET LA MESURE D'AMBIANCE RAMENEE A L'INDIVIDU
# ---------------------------------------------------------------------------
test_that("plusieurs denominateurs se multiplient, et une ligne incomplete est ecartee", {
  n <- matrix(c(40, 50, 60, 2, 3, 4), ncol = 2)
  u <- .hstat_epi_offset(n[, 1, drop = FALSE])
  d <- .hstat_epi_offset(n)
  # LA SOMME DES LOGARITHMES EST LE LOGARITHME DU PRODUIT : une population ET
  # une duree donnent des personnes-mois. L'assertion porte sur la VALEUR, pas
  # sur la presence de la colonne -- un offset qui ignorerait le second
  # denominateur rendrait un decompte parfaitement plausible, et faux.
  expect_equal(d$log_off, log(n[, 1] * n[, 2]))
  expect_false(isTRUE(all.equal(d$log_off, u$log_off)))
  # UN SEUL DENOMINATEUR NON POSITIF SUFFIT A ECARTER LA LIGNE : un mois sans
  # naissance vivante n'informe aucun taux, quel que soit le second.
  z <- .hstat_epi_offset(matrix(c(40, 0, 60, 2, 3, 4), ncol = 2))
  expect_equal(z$ecartes, 1L)
  expect_equal(z$garde, c(TRUE, FALSE, TRUE))
  expect_true(is.na(z$log_off[2]))
  expect_equal(.hstat_epi_offset(NULL)$ecartes, 0L)
})

test_that("la mesure d'ambiance se ramene a l'individu sans changer le RR", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  # LE FACTEUR NUL OU NEGATIF EST REFUSE ET NOMME : nul, il ecraserait la serie
  # sur une constante ; negatif, il INVERSERAIT le sens de l'effet -- une
  # protection publiee comme un exces de risque.
  r0 <- hstat_epi_proxy(d$Tmax, a = 0, nom = "Tmax")
  expect_false(r0$applique)
  expect_identical(r0$x, d$Tmax)
  expect_match(r0$message, "Tmax")
  expect_false(hstat_epi_proxy(d$Tmax, a = -1)$applique)
  # a = 1, b = 0 n'est pas une transformation : rien n'est annonce.
  expect_null(hstat_epi_proxy(d$Tmax)$message)
  p <- hstat_epi_proxy(d$Tmax, a = 0.7, b = 2, nom = "Tmax")
  expect_true(p$applique)
  expect_equal(p$x, 0.7 * d$Tmax + 2)
  # LE BALAYAGE DE COUVERTURE NE RELEVE QUE LES CHAINES LITTERALES passees a
  # `tr()` : un message assemble dans une CONSTANTE lui est invisible. Meme cas
  # que `HSTAT_ERR_FR`, et meme remede -- une assertion dediee, sans quoi la
  # phrase resterait en francais au milieu d'une interface anglaise.
  expect_true(HSTAT_EPI_PROXY_MSG %in% hstat_i18n_load()$fr)
  expect_match(p$message, "a = 0.7")
  # L'AXE, LA REFERENCE ET LES VALEURS CHANGENT ; LE RR A UN PERCENTILE DONNE,
  # NON. La transformation est affine croissante, donc monotone : le 90e
  # percentile de l'ambiance EST le 90e percentile du personnel. C'est
  # exactement ce qui autorise a l'offrir -- et c'est l'assertion qui separe
  # une vraie transformation d'un changement d'unite qui deplacerait l'effet.
  ta <- hstat_epi_dlnm(d, "prema", "Tmax", var_offset = "nais",
                       var_temps = "Date", lag_max = 5)
  tp <- hstat_epi_dlnm(d, "prema", "Tmax", var_offset = "nais",
                       var_temps = "Date", lag_max = 5, proxy_a = 0.7, proxy_b = 2)
  expect_true(ta$ok && tp$ok)
  expect_equal(tp$proxy$a, 0.7)
  expect_true(tp$proxy$applique)
  expect_false(ta$proxy$applique)
  expect_equal(tp$reference, 0.7 * ta$reference + 2, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(tp$reference, ta$reference)))
  ra <- hstat_epi_dlnm_rr(ta, c(0.10, 0.90))
  rp <- hstat_epi_dlnm_rr(tp, c(0.10, 0.90))
  expect_equal(rp$RR, ra$RR, tolerance = 1e-6)
  expect_equal(rp$IC_bas, ra$IC_bas, tolerance = 1e-6)
  expect_false(isTRUE(all.equal(rp$Exposition, ra$Exposition)))
})

# ===========================================================================
#  EPIDEMIOLOGIE : TEST FONCTIONNEL
# ===========================================================================
#  Les sept defauts signales a l'ecran, chacun garde par l'assertion qui
#  distingue le code corrige du code d'avant -- jamais par la seule presence
#  d'un reglage.

test_that("le bouton d'export du graphique a un producteur", {
  root <- .hstat_repo_root(); skip_if(is.na(root))
  src <- paste(.hstat_code_lignes(.hstat_module_path("mod_epidemio.R")), collapse = "\n")
  # LE DEFAUT : `hstat_export_plot_handler()` REND un `downloadHandler`.
  # Appele nu, il le construisait puis le jetait -- le bouton `epiPDl` restait
  # affiche et ne telechargeait rien. Rien ne leve, et le test qui devait le
  # garder comptait l'APPEL au lieu de l'AFFECTATION.
  expect_match(src, "output\\$epiPDl *<- *hstat_export_plot_handler")
  # L'appel nu ne doit pas revenir : c'est la forme exacte du defaut.
  expect_false(grepl("(?<![-] )\\n *hstat_export_plot_handler\\(input", src, perl = TRUE))
})

test_that("tous les tableaux calcules sont atteignables a l'ecran", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r <- hstat_epi_dlnm_multi(d, c("prema", "fpn"), c("Tmax", "HR"),
                            var_offset = "nais", var_temps = "Date", lag_max = 5)
  # Deux issues x deux expositions : comparaison + budget + 2 tableaux par
  # couple + collinearite = onze. Trois emplacements figes en montraient trois.
  expect_equal(length(r$resultats), 4L)
  ui <- mod_epidemio_ui("epidemio")
  h <- as.character(ui)
  # L'ECRAN DOIT POUVOIR EN PORTER PLUS DE TROIS. On mesure le conteneur
  # dynamique, pas trois identifiants figes : c'est lui qui les emet tous.
  expect_true(grepl('epidemio-epiTablesUI', h, fixed = TRUE))
  expect_false(grepl('epidemio-epiTable3"', h, fixed = TRUE))
  src <- paste(.hstat_code_lignes(.hstat_module_path("mod_epidemio.R")), collapse = "\n")
  # Le plafond existe ET il est annonce : au-dela, les tableaux restent dans
  # l'export, et le taire ferait croire qu'ils n'existent pas.
  expect_match(src, "epi_max_tables *<- *[0-9]+L")
  expect_match(src, "partent dans l'export")
})

test_that("la loi se choisit sur l'AIC puis sur la surdispersion", {
  # LA REGLE VIT HORS DU BLOC D'AJUSTEMENT, sans quoi la branche
  # quasi-Poisson n'est atteignable qu'en faisant echouer `glm.nb`.
  expect_equal(hstat_epi_famille_auto(620, 600, 3.0)$famille, "nb")
  expect_equal(hstat_epi_famille_auto(600, 620, 0.9)$famille, "poisson")
  # LE CAS QUE L'ANCIEN CODE MANQUAIT : Poisson gagne a l'AIC mais les residus
  # sont surdisperses. Il se contentait alors de CONSEILLER quasi-Poisson, et
  # publiait des intervalles trop etroits -- des effets « significatifs » qui
  # ne le sont pas, le resultat faux et plausible que ce depot traque.
  expect_equal(hstat_epi_famille_auto(600, 620, 3.0)$famille, "quasipoisson")
  expect_equal(hstat_epi_famille_auto(600, 620, 3.0)$motif, "dispersion")
  # UN AIC ABSENT N'EST PAS UN AIC PERDANT : sans cette garde, la binomiale
  # negative gagnerait par sa seule absence.
  expect_equal(hstat_epi_famille_auto(600, NA, 0.9)$famille, "poisson")
  expect_equal(hstat_epi_famille_auto(600, NA, 3.0, nb_dispo = FALSE)$famille, "quasipoisson")
  # LE SEUIL SE TESTE DES DEUX COTES DE SA FRONTIERE : un palier deplace
  # laisse un comportement parfaitement lisible, et faux.
  expect_equal(hstat_epi_famille_auto(600, 620, HSTAT_EPI_DISP_SEUIL - 0.01)$famille, "poisson")
  expect_equal(hstat_epi_famille_auto(600, 620, HSTAT_EPI_DISP_SEUIL + 0.01)$famille, "quasipoisson")
})

test_that("le repere de reference atteint toutes les figures a axe d'exposition", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r <- hstat_epi_dlnm_multi(d, "prema", c("Tmax", "HR"), var_offset = "nais",
                            var_temps = "Date", lag_max = 5)
  txt <- function(p) sum(vapply(p$layers,
    function(l) inherits(l$geom, "GeomText"), logical(1)))
  on  <- list(repere = list(montrer = TRUE, texte = "Réf.", position = "haut",
                            cote = "droite", taille = 3.5, style = "plain"))
  off <- list(repere = list(montrer = FALSE))
  # LES SIX REGLAGES N'ETAIENT LUS QUE PAR « effet cumule ». Sur la carte et
  # les coupes, l'utilisateur les deplaçait sans que rien ne bouge --
  # « declare, lu, mais jamais utilise ».
  for (f in c("cumul", "carte", "coupes")) {
    expect_equal(txt(hstat_epi_figure("dlnm", f, r$resultats[[1]], on)), 1L, info = f)
    expect_equal(txt(hstat_epi_figure("dlnm", f, r$resultats[[1]], off)), 0L, info = f)
  }
  # L'AXE DES RETARDS NE PORTE PAS L'EXPOSITION : une reference d'exposition
  # n'est pas un nombre de mois, et l'y poser serait un repere qui ment.
  expect_equal(txt(hstat_epi_figure("dlnm", "retard", r$resultats[[1]], on)), 0L)
  # SUR UN AXE EN PERCENTILES, la reference se pose a SON percentile, jamais a
  # sa valeur : 29,7 pose sur un axe de 0 a 100 tomberait au trentieme centile
  # par coincidence d'echelle -- faux, et parfaitement plausible.
  m <- hstat_epi_figure_multi("cumul", r$resultats, "percentiles", o = on)
  expect_equal(txt(m), 1L)
  pos <- m$layers[[which(vapply(m$layers,
    function(l) inherits(l$geom, "GeomVline"), logical(1)))[1]]]$data$xintercept
  expect_true(pos >= 0 && pos <= 100)
  expect_equal(pos, .hstat_epi_pct(r$resultats[[1]]$reference, r$resultats[[1]]$expo))
  expect_false(isTRUE(all.equal(pos, r$resultats[[1]]$reference)))
})

test_that("l'echelle des facettes est un reglage, et les quatre agissent", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r <- hstat_epi_dlnm_multi(d, "prema", c("Tmax", "HR"), var_offset = "nais",
                            var_temps = "Date", lag_max = 5)
  # `free_x` etait ecrit en dur : deux panneaux dont les axes Y coincident --
  # le seul cadre ou l'amplitude des RR se compare -- etaient inatteignables.
  att <- list(free_x = c(TRUE, FALSE), fixed = c(FALSE, FALSE),
              free_y = c(FALSE, TRUE), free   = c(TRUE, TRUE))
  for (sc in names(att)) {
    b <- ggplot2::ggplot_build(hstat_epi_figure_multi(
      "carte", r$resultats, "facettes", o = list(facettes = sc)))
    expect_equal(unname(c(b$layout$facet_params$free$x,
                          b$layout$facet_params$free$y)), att[[sc]], info = sc)
  }
  # Un nom inconnu retombe sur le defaut, jamais sur une valeur que
  # `facet_wrap` refuserait : elle leve, et l'erreur emporte la figure.
  expect_equal(hstat_epi_facet_scales("zzz"), "free_x")
  expect_equal(hstat_epi_facet_scales(NULL), "free_x")
  expect_silent(ggplot2::ggplot_build(hstat_epi_figure_multi(
    "carte", r$resultats, "facettes", o = list(facettes = "zzz"))))
})

test_that("la carte change de couleurs, jamais de nature", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r <- hstat_epi_dlnm(d, "prema", "Tmax", var_temps = "Date", lag_max = 5)
  fill_de <- function(p) {
    i <- which(vapply(p$scales$scales, function(x) "fill" %in% x$aesthetics, logical(1)))[1]
    p$scales$scales[[i]]
  }
  vus <- character(0)
  for (pal in names(HSTAT_EPI_CARTE_PALETTES)) {
    p <- hstat_epi_figure("dlnm", "carte", r, list(carte_palette = pal))
    sc <- fill_de(p)
    # CE QUI NE SE NEGOCIE PAS : l'echelle reste DIVERGENTE ET CENTREE SUR 1.
    # Le RR est un rapport ; une palette sequentielle ferait passer « aucun
    # effet » pour une couleur quelconque au milieu du degrade.
    expect_equal(sc$rescaler(1, from = c(1, 1)), 0.5, info = pal)
    vus <- c(vus, HSTAT_EPI_CARTE_PALETTES[[pal]]$low)
  }
  # LES PALETTES SONT REELLEMENT DIFFERENTES : sans cette assertion, six
  # entrees rendant la meme couleur passeraient le test precedent.
  expect_equal(length(unique(vus)), length(HSTAT_EPI_CARTE_PALETTES))
  expect_gt(length(HSTAT_EPI_CARTE_PALETTES), 2L)
  # Un nom inconnu rend le thermique, jamais `NULL` : une echelle absente
  # laisserait ggplot poser la sienne, qui est SEQUENTIELLE.
  expect_equal(hstat_epi_carte_palette("zzz")$low,
               HSTAT_EPI_CARTE_PALETTES$thermique$low)
  expect_equal(length(hstat_epi_carte_choix()), length(HSTAT_EPI_CARTE_PALETTES))
})

test_that("les reglages de figure voyagent par une seule liste vers les deux constructeurs", {
  root <- .hstat_repo_root(); skip_if(is.na(root))
  src <- paste(.hstat_code_lignes(.hstat_module_path("mod_epidemio.R")), collapse = "\n")
  # LE DEFAUT SIGNALE : « les operations de modification doivent s'appliquer a
  # tous ». Les options n'atteignaient que la figure simple ; la figure multi
  # etait appelee SANS elles, donc six reglages sur six y etaient morts.
  expect_match(src, "opts_fig *<- *shiny::reactive")
  expect_match(src, "hstat_epi_figure_multi\\([^)]*o = o", perl = TRUE)
  # Les trois familles sont dans la meme liste : un module qui en lirait une
  # seule laisserait les autres sans effet.
  for (k in c("repere", "facettes", "carte_palette"))
    expect_match(src, paste0(k, " *="), info = k)
  expect_true(all(grepl("epidemio-", c(
    paste0("epidemio-", c("epiFacettes", "epiCartePal"))), fixed = TRUE)))
  # Les deux nouveaux widgets traversent `ns()`, sinon ils n'existent pour
  # personne -- le defaut le plus silencieux du depot.
  h <- as.character(mod_epidemio_ui("epidemio"))
  for (id in c("epiFacettes", "epiCartePal"))
    expect_true(grepl(paste0('epidemio-', id), h, fixed = TRUE), info = id)
})

# ===========================================================================
#  EPIDEMIOLOGIE : TEST DE PERFORMANCE
# ===========================================================================
#  Ce qu'on mesure n'est PAS une duree absolue -- elle depend de la machine, et
#  une assertion sur des secondes ressemble a une regle tout en ne gardant
#  qu'un etat du materiel. On mesure des invariants de COUT :
#
#    * le nombre d'observateurs que le module enregistre est BORNE ;
#    * le cout croit lineairement avec le nombre de surfaces, jamais au carre ;
#    * une figure ne refait pas l'ajustement.

test_that("le nombre de sorties enregistrees est borne, quel que soit le plan", {
  root <- .hstat_repo_root(); skip_if(is.na(root))
  src <- paste(.hstat_code_lignes(.hstat_module_path("mod_epidemio.R")), collapse = "\n")
  # SHINY GARDE UN OBSERVATEUR PAR SORTIE ENREGISTREE. Les identifiants des
  # tableaux sont CONSTRUITS : sans plafond, un plan a dix issues et six
  # expositions en poserait cent vingt-deux, et la page cesserait de repondre.
  m <- regmatches(src, regexpr("epi_max_tables *<- *([0-9]+)L", src))
  expect_length(m, 1L)
  plafond <- as.integer(sub(".*<- *([0-9]+)L", "\\1", m))
  expect_gt(plafond, 10L)
  expect_lt(plafond, 200L)
  # LES RENDUS SE POSENT UNE SEULE FOIS, hors de tout observateur : les
  # reenregistrer a chaque calcul empilerait un observateur par passage, et la
  # session ralentirait a l'usage sans qu'aucune erreur ne le dise.
  bloc <- regmatches(src, regexpr(
    "for \\(k in seq_len\\(epi_max_tables\\)\\) local\\(\\{", src))
  expect_length(bloc, 1L)
  expect_false(grepl("observe(Event)?\\([^)]*\\{[^}]*seq_len\\(epi_max_tables\\)",
                     src, perl = TRUE))
})

test_that("le cout du multi-expositions croit lineairement, pas au carre", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  # ON COMPTE LES AJUSTEMENTS, ON NE CHRONOMETRE PAS.
  #
  # La premiere version de ce test comparait deux durees. Mesure : deux
  # surfaces en 2,04 s et QUATRE en 0,32 s -- le premier appel paie le
  # chargement de `dlnm` et la compilation. Le rapport sortait a 0,16, si bien
  # qu'une implementation QUADRATIQUE l'aurait passe aussi. Une assertion qui
  # ne distingue pas les deux codes ne garde rien, et celle-la se trompait
  # dans le sens rassurant -- la troisieme fois dans ce depot.
  #
  # Le nombre d'ajustements, lui, est exact, deterministe, et independant de
  # la machine : c'est LA grandeur que « lineaire, pas quadratique » designe.
  compte <- new.env(); compte$n <- 0L
  suppressMessages(trace(hstat_epi_dlnm, tracer = function() compte$n <- compte$n + 1L,
                         print = FALSE, where = environment(hstat_epi_dlnm_multi)))
  on.exit(suppressMessages(untrace(hstat_epi_dlnm,
                                   where = environment(hstat_epi_dlnm_multi))), add = TRUE)

  compte$n <- 0L
  r2 <- hstat_epi_dlnm_multi(d, "prema", c("Tmax", "HR"),
                             var_temps = "Date", lag_max = 5)
  n2 <- compte$n
  compte$n <- 0L
  r4 <- hstat_epi_dlnm_multi(d, c("prema", "fpn"), c("Tmax", "HR"),
                             var_temps = "Date", lag_max = 5)
  n4 <- compte$n

  expect_equal(length(r2$resultats), 2L)
  expect_equal(length(r4$resultats), 4L)
  # UN AJUSTEMENT PAR COUPLE issue x exposition, exactement. Une boucle qui
  # ajusterait par PAIRE d'expositions en ferait quatre puis seize.
  expect_equal(n2, 2L)
  expect_equal(n4, 4L)
  expect_equal(n4 / n2, 2)
})

test_that("changer un reglage de figure ne refait aucun ajustement", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r <- hstat_epi_dlnm(d, "prema", "Tmax", var_temps = "Date", lag_max = 5)
  # MEME LEÇON QUE CI-DESSUS : la premiere version comparait le temps de cinq
  # figures a celui d'un ajustement. Mesure : 0,767 s contre 0,020 s, soit un
  # rapport de 38 -- et l'assertion ne l'a pas signale parce qu'elle se
  # SAUTAIT sur un ajustement trop rapide. Un test saute ressemble a un test
  # qui passe. Le rapport de 38 ne dit d'ailleurs rien d'un reajustement :
  # `ggplot_build` sur une carte de plusieurs milliers de cases est
  # simplement cher, et c'est normal.
  #
  # L'invariant reel est un COMPTE, pas une duree : construire une figure ne
  # doit declencher AUCUN ajustement, quel que soit le reglage change.
  compte <- new.env(); compte$n <- 0L
  for (f in c("glm", "glm.fit")) {
    suppressMessages(trace(f, tracer = function() compte$n <- compte$n + 1L,
                           print = FALSE, where = asNamespace("stats")))
  }
  on.exit({
    for (f in c("glm", "glm.fit"))
      suppressMessages(untrace(f, where = asNamespace("stats")))
  }, add = TRUE)

  compte$n <- 0L
  for (pal in names(HSTAT_EPI_CARTE_PALETTES))
    ggplot2::ggplot_build(hstat_epi_figure("dlnm", "carte", r,
                                           list(carte_palette = pal)))
  for (sc in HSTAT_EPI_FACET_SCALES)
    ggplot2::ggplot_build(hstat_epi_figure("dlnm", "coupes", r,
                                           list(facettes = sc)))
  # SIX PALETTES ET QUATRE ECHELLES, et pas un seul ajustement.
  expect_equal(compte$n, 0L)

  # Et la preuve structurelle, qui tient meme si le compte venait a mentir :
  # le corps du constructeur n'appelle aucune fonction d'ajustement.
  src <- .hstat_code_lignes(file.path(.hstat_repo_root(), "R", "utils.R"))
  i <- grep("^hstat_epi_figure <- function", src)
  j <- grep("^hstat_epi_figure_multi <- function", src)
  expect_length(i, 1L); expect_length(j, 1L)
  corps <- paste(src[i:(j - 1L)], collapse = "\n")
  for (f in c("hstat_epi_dlnm\\(", "stats::glm\\(", "MASS::glm.nb\\(",
              "dlnm::crossbasis\\(", "dlnm::crosspred\\("))
    expect_false(grepl(f, corps), info = f)
})

# ===========================================================================
#  EPIDEMIOLOGIE : PERCENTILES MARQUES ET FENETRES DE RETARD
# ===========================================================================

test_that("les percentiles se materialisent sur la courbe, et la liste se choisit", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r <- hstat_epi_dlnm(d, "prema", "Tmax", var_temps = "Date", lag_max = 5)
  pts <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))
    if (!length(i)) 0L else nrow(p$layers[[i[1]]]$data)
  }
  labs <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
    unlist(lapply(i, function(k) p$layers[[k]]$data$lab))
  }
  off <- list(repere = list(montrer = FALSE))
  p4 <- hstat_epi_figure("dlnm", "cumul", r, c(off, list(percentiles = c(10, 25, 75, 90))))
  expect_equal(pts(p4), 4L)
  # L'ETIQUETTE PORTE LE PERCENTILE *ET* SON RR : c'est le chiffre du tableau,
  # pose la ou il se lit. Un point sans sa valeur n'apprend rien.
  expect_true(all(grepl("^P(10|25|75|90)\nRR=", labs(p4))))
  # LA LISTE EST UN REGLAGE : un essai de canicule regarde P95, un essai de
  # froid P5. Ecrite en dur, elle donnerait quatre points que personne n'a
  # demandes -- et aucun de ceux qu'on cherche.
  expect_equal(pts(hstat_epi_figure("dlnm", "cumul", r,
                                    c(off, list(percentiles = c(5, 95))))), 2L)
  expect_equal(pts(hstat_epi_figure("dlnm", "cumul", r,
                                    c(off, list(percentiles = numeric(0))))), 0L)
  # LE POINT SE POSE SUR LA COURBE, jamais a cote : le RR marque est LU sur la
  # grille de prediction, il n'est pas recalcule. Sans cela le point
  # flotterait a cote de la ligne qu'il designe.
  p90 <- hstat_epi_figure("dlnm", "cumul", r, c(off, list(percentiles = 90)))
  i <- which(vapply(p90$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]
  expect_equal(round(p90$layers[[i]]$data$y, 4), hstat_epi_dlnm_rr(r, 0.90)$RR)
  # Un percentile hors de ]0 ; 100[ n'existe pas : `quantile` y leve, et
  # l'erreur emporterait la figure entiere.
  expect_equal(hstat_epi_pct_valides(c(-1, 0, 10, 100, 150, 90)), c(10, 90))
  expect_equal(hstat_epi_pct_valides(c("x", NA)), numeric(0))
})

test_that("une fenetre de retard porte la covariance de ses retards", {
  skip_if_not_installed("dlnm")
  d <- .hstat_epi_jeu()
  r <- hstat_epi_dlnm(d, "prema", "Tmax", var_temps = "Date", lag_max = 5)

  # LA RECONSTRUCTION EST EXACTE, et c'est ce qui autorise a s'en servir sur un
  # sous-intervalle ou `crosspred` ne rend rien : la fenetre ENTIERE doit
  # reproduire l'effet cumule de `crosspred`, estimation ET bornes.
  tout <- hstat_epi_dlnm_fenetres(r, list(list(nom = NA, de = 0L, a = r$lag_max)),
                                  prob = 0.90)
  cum <- hstat_epi_dlnm_rr(r, 0.90)
  expect_equal(tout$RR, cum$RR)
  expect_equal(tout$IC_bas, cum$IC_bas)
  expect_equal(tout$IC_haut, cum$IC_haut)

  # LE PIEGE QUE CETTE FONCTION EXISTE POUR EVITER. Les retards d'un meme
  # modele sont CORRELES : les effets (en log) s'ajoutent bien, mais les
  # intervalles NON. Diviser deux cumuls, ou additionner deux largeurs,
  # rendrait un intervalle faux et parfaitement plausible.
  deux <- hstat_epi_dlnm_fenetres(r, list(list(nom = NA, de = 0L, a = 1L),
                                          list(nom = NA, de = 2L, a = r$lag_max)),
                                  prob = 0.90)
  expect_equal(nrow(deux), 2L)
  expect_equal(prod(deux$RR), cum$RR, tolerance = 1e-3)
  larg <- function(x) x$IC_haut - x$IC_bas
  # LE SENS DE L'ECART N'EST PAS UN INVARIANT, et ma premiere assertion le
  # supposait : elle exigeait que les largeurs de fenetres SOMMENT a plus que
  # celle du cumul. Mesure sur deux jeux, les deux sens se rencontrent -- le
  # signe depend de celui des covariances. Ce qui est vrai, et c'est tout ce
  # qu'il faut, c'est que les largeurs ne s'AJOUTENT PAS.
  expect_false(isTRUE(all.equal(sum(larg(deux)), larg(cum), tolerance = 0.05)))

  # Le tableau dit a quelle exposition il se lit : un RR de fenetre sans son
  # percentile ne se recopie pas dans un rapport.
  expect_equal(attr(tout, "percentile"), 0.90)
  expect_true(is.finite(attr(tout, "exposition")))
  expect_setequal(names(deux), c("Fenetre", "Lag_de", "Lag_a", "RR",
                                 "IC_bas", "IC_haut", "Verdict"))
})

test_that("les fenetres se saisissent librement, et ce qui ne se lit pas est nomme", {
  f <- hstat_epi_fenetres_parse("0-1 court terme\n2-4 différé\n5-8 moyen terme", lag_max = 12)
  expect_length(f, 3L)
  # LE NOM PORTE LE SENS, et c'est lui qui part dans le tableau publie :
  # « Lag 0-1 » seul n'apprend rien a qui lit un rapport.
  expect_equal(hstat_epi_fenetre_libelle(f[[1]]), "Lag 0-1 (court terme)")
  expect_equal(hstat_epi_fenetre_libelle(f[[3]]), "Lag 5-8 (moyen terme)")
  # Un rang seul est une fenetre d'un retard, et son libelle le dit.
  expect_equal(hstat_epi_fenetre_libelle(hstat_epi_fenetres_parse("3")[[1]]), "Lag 3")
  # LES BORNES SE REMETTENT DANS L'ORDRE : « 4-2 » est une inversion de saisie.
  # Rendre une fenetre vide amputerait le tableau d'une ligne sans un mot.
  inv <- hstat_epi_fenetres_parse("4-2", lag_max = 12)
  expect_equal(c(inv[[1]]$de, inv[[1]]$a), c(2L, 4L))
  # CE QUI NE SE LIT PAS EST NOMME, jamais ecarte en silence.
  bad <- hstat_epi_fenetres_parse("abc; 0-1; 99-100", lag_max = 5)
  expect_length(bad, 1L)
  expect_setequal(attr(bad, "rejets"), c("abc", "99-100"))
  # Une fenetre qui deborde est RAMENEE au decalage du modele, pas jetee.
  expect_equal(hstat_epi_fenetres_parse("0-99", lag_max = 5)[[1]]$a, 5L)
  expect_length(hstat_epi_fenetres_parse(""), 0L)
  # Les quatre fenetres usuelles sont declarees une fois, avec leurs noms.
  expect_gte(length(HSTAT_EPI_FENETRES), 4L)
  expect_true(all(vapply(HSTAT_EPI_FENETRES,
                         function(x) nzchar(x$nom) && x$a >= x$de, logical(1))))
})

# ===========================================================================
#  EPIDEMIOLOGIE : TEST DE CYBERSECURITE
# ===========================================================================
#  Trouve en ATTAQUANT le module, pas en le relisant.

test_that("une reference hors de l'etendue ne rend ni 0 ni l'infini", {
  skip_if_not_installed("dlnm")
  set.seed(7); n <- 60
  d <- data.frame(Date = seq(as.Date("2015-01-01"), by = "month", length.out = n),
                  x = round(20 + rnorm(n), 2), y = rpois(n, 4))
  bornes <- range(d$x)
  # LE DEFAUT, MESURE : la spline n'est definie que sur l'etendue observee ;
  # au-dela elle extrapole sans borne. Sur une exposition allant de 18,4 a
  # 22,7, une reference a 1e12 rendait un RR de 0 et une reference a -1e12 un
  # RR d'INFINI -- un tableau complet, publiable, et faux, que rien ne
  # signalait. C'est la forme la plus couteuse.
  for (v in c(1e12, -1e12)) {
    r <- hstat_epi_dlnm(d, "y", "x", var_temps = "Date", lag_max = 3, reference = v)
    expect_true(isTRUE(r$ok))
    expect_gte(r$reference, bornes[1])
    expect_lte(r$reference, bornes[2])
    tb <- hstat_epi_dlnm_rr(r, c(0.10, 0.90))
    expect_true(all(is.finite(tb$RR)), info = format(v))
    expect_true(all(tb$RR > 0), info = format(v))
    # LE REPORT EST ANNONCE : sans un mot, l'utilisateur lit des RR rapportes
    # a une reference qui n'est pas celle qu'il a demandee.
    expect_match(r$message, "hors de l'étendue")
  }
  # Une reference legitime n'est pas deplacee, et rien n'est annonce a tort.
  ok <- hstat_epi_dlnm(d, "y", "x", var_temps = "Date", lag_max = 3,
                       reference = stats::median(d$x))
  expect_equal(ok$reference, stats::median(d$x))
  expect_false(grepl("hors de l'étendue", ok$message %||% ""))
})

test_that("un decalage aberrant est refuse ou nomme, jamais reinterprete en silence", {
  skip_if_not_installed("dlnm")
  set.seed(7); n <- 60
  d <- data.frame(Date = seq(as.Date("2015-01-01"), by = "month", length.out = n),
                  x = round(20 + rnorm(n), 2), y = rpois(n, 4))
  # UN DECALAGE NEGATIF ETAIT RAMENE A ZERO EN SILENCE : le modele cessait
  # alors d'etre un modele A RETARDS DISTRIBUES -- une seule colonne de
  # surface au lieu de six -- et rien ne le disait.
  neg <- hstat_epi_dlnm(d, "y", "x", var_temps = "Date", lag_max = -5)
  expect_false(isTRUE(neg$ok))
  expect_match(neg$message, "négatif")
  # Un decalage illisible retombe sur le defaut, mais le repli est NOMME.
  na <- hstat_epi_dlnm(d, "y", "x", var_temps = "Date", lag_max = NA)
  expect_true(isTRUE(na$ok))
  expect_match(na$message, "illisible")
})

test_that("le module resiste a des valeurs hostiles sur tous ses reglages", {
  hostile <- list("zzz", "'; DROP TABLE --", "<img src=x onerror=alert(1)>",
                  NA, NULL, c("a", "b"), 42, Inf, -1)
  # AUCUN REGLAGE D'APPARENCE NE DOIT LEVER : une erreur dans un constructeur
  # de figure emporte la sortie entiere, donc tout l'onglet.
  for (v in hostile) {
    expect_silent(pal <- hstat_epi_carte_palette(v))
    expect_true(nzchar(pal$low))
    expect_true(hstat_epi_facet_scales(v) %in% HSTAT_EPI_FACET_SCALES)
    expect_silent(hstat_epi_pct_valides(v))
  }
  # LE TEXTE DU REPERE RESTE UNE DONNEE DE COUCHE, jamais du balisage : ggplot
  # dessine du texte, il n'interprete pas de HTML. C'est ce qui rend
  # l'injection inoffensive ici -- et l'echappement, lui, garde l'affichage.
  ch <- "<img src=x onerror=alert(1)>"
  l <- .hstat_epi_repere_couche(
    list(repere = list(montrer = TRUE, texte = ch, position = "haut",
                       cote = "droite", taille = 3.5, style = "plain")),
    20, etendue = c(0, 2))
  expect_equal(vapply(l, function(z) class(z$geom)[1], character(1)),
               c("GeomVline", "GeomText"))
  expect_false(grepl("<", hstat_html_escape(ch), fixed = TRUE))
  # UN NOM DE FEUILLE VIENT DU FICHIER : les caracteres qu'Excel refuse
  # feraient tomber l'export ENTIER pour un seul nom de colonne.
  for (v in c(ch, "a[b]c:d*e?f/g\\h", strrep("x", 60))) {
    s <- hstat_feuille_nom(paste0("Fenetres_", v))
    expect_lte(nchar(s), 31L)
    for (bad in c("[", "]", ":", "*", "?", "/", "\\"))
      expect_false(grepl(bad, s, fixed = TRUE), info = paste(v, bad))
  }
  # LES TAILLES ABERRANTES NE FONT PAS TOMBER LA COUCHE.
  for (v in list(NA, Inf, -1, 1e9, "x", NULL))
    expect_silent(.hstat_epi_repere_couche(
      list(repere = list(montrer = TRUE, texte = "R", taille = v,
                         position = "haut", cote = "droite", style = "plain")),
      20, etendue = c(0, 2)))
})

test_that("un nom de colonne hostile ne quitte jamais son role de donnee", {
  skip_if_not_installed("dlnm")
  ch <- "<img src=x onerror=alert(1)>"
  set.seed(7); n <- 60
  d <- data.frame(Date = seq(as.Date("2015-01-01"), by = "month", length.out = n),
                  y = rpois(n, 4))
  d[[ch]] <- round(20 + rnorm(n), 2)
  r <- hstat_epi_dlnm(d, "y", ch, var_temps = "Date", lag_max = 3)
  expect_true(isTRUE(r$ok))
  # LA SURFACE ENTRE SOUS UN NOM STABLE, jamais sous celui de la colonne :
  # `as.formula` refuse accents, espaces et parentheses, et les contourner par
  # des accents graves casse l'appariement des coefficients.
  expect_match(r$cb_noms[[1]], "^cb[0-9]+$")
  expect_false(grepl("img|onerror", paste(r$cb_noms, collapse = " ")))
  expect_false(any(grepl("onerror", names(stats::coef(r$model)))))
  # Le nom traverse les tableaux INTACT -- on n'altere pas la donnee --
  # et c'est l'echappement, au rendu, qui empeche le balisage.
  cp <- hstat_epi_dlnm_comparaison(list(r))
  expect_equal(as.character(cp$Exposition[1]), ch)
  expect_false(grepl("<img", hstat_html_escape(ch), fixed = TRUE))
})

test_that("le kit de mise en forme AGIT sur le theme construit, il ne fait pas que s'appeler", {
  skip_if_not_installed("ggplot2")
  # LE DEFAUT ETAIT UNE VALEUR JETEE. `hstat_apply_plot_opts()` ecrivait
  # `g + hstat_plot_extras_theme(...)` sans affecter : la ligne suivante
  # repartait du `g` d'avant, et LES NEUF REGLAGES DU KIT n'avaient aucun effet
  # sur les quatre modules servis (ML, DL, series temporelles, epidemiologie).
  # L'utilisateur cochait « Tracer les axes X et Y » et l'image ne bougeait pas.
  #
  # Le test qui gardait le kit cherchait l'APPEL dans le source du module : il
  # etait satisfait par un appel dont la valeur se perd. On mesure donc l'EFFET
  # sur le theme construit -- verifie comme echouant sur la version d'avant
  # correction (axis.line.x `element_blank`, marge haute 6,5 pt pour 40 pt
  # demandes, cles a 1,2 ligne pour 2,5 demandees).
  pfx <- "epiG"
  inp <- list(epiGBase = 13, epiGTheme = "minimal",
              epiGAxisLine = TRUE, epiGAxisLineCouleur = "#FF0000",
              epiGAxisLineEpaisseur = 2.5, epiGLegendeCles = 2.5,
              epiGMargeHaut = 40, epiGMargeBas = 41,
              epiGMargeGauche = 42, epiGMargeDroite = 43)
  g <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  h <- hstat_apply_plot_opts(g, inp, pfx)

  ax <- ggplot2::calc_element("axis.line.x", h$theme)
  expect_true(inherits(ax, "element_line"))
  expect_equal(ax$colour, "#FF0000")
  expect_equal(as.numeric(ax$linewidth), 2.5)
  ay <- ggplot2::calc_element("axis.line.y", h$theme)
  expect_true(inherits(ay, "element_line"))

  # LES MARGES SONT L'ASSERTION QUI DISCRIMINE LE MIEUX : les quatre valeurs
  # sont DIFFERENTES entre elles, si bien qu'un kit pose de travers (ordre
  # haut/droite/bas/gauche echange) ne passerait pas non plus.
  m <- as.numeric(ggplot2::calc_element("plot.margin", h$theme))
  expect_equal(m, c(40, 43, 41, 42))

  kh <- ggplot2::calc_element("legend.key.height", h$theme)
  expect_equal(as.numeric(kh), 2.5)

  # ET DECOCHE, LE REGLAGE NE POSE RIEN : un `element_blank()` en repli
  # effacerait les axes d'un theme qui les trace de lui-meme (« classique »).
  inp$epiGAxisLine <- FALSE
  h0 <- hstat_apply_plot_opts(g, inp, pfx)
  expect_false(inherits(h0$theme$axis.line, "element_line"))
})

test_that("l'epaisseur de la ligne de reference est un reglage, et « nommer » ne commande que le nom", {
  skip_if_not_installed("dlnm")
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")
  set.seed(52); n <- 160
  d <- data.frame(date = seq(as.Date("2010-01-01"), by = "month", length.out = n))
  d$tmax  <- 28 + 3 * sin(2 * pi * seq_len(n) / 12) + stats::rnorm(n, 0, 1)
  d$nais  <- stats::rpois(n, 300)
  d$prema <- stats::rpois(n, exp(log(d$nais) - 3.1 + 0.05 * (d$tmax - 28)))
  vals <- shiny::reactiveValues(data = d, cleanData = NULL, filteredData = NULL,
                                resetSignal = 0L)
  # LE TEST PORTE SUR LE MODULE : un test appelant `.hstat_epi_repere_couche()`
  # serait reste vert pendant que `repere()` ne lit pas l'epaisseur.
  shiny::testServer(mod_epidemio_server, args = list(values = vals), {
    session$setInputs(epiAnalyse = "dlnm", epiY = "prema", epiExpo = "tmax",
                      epiOffset = "nais", epiTemps = "date", epiMutuel = TRUE,
                      epiLag = 4, epiNkLag = 2, epiFamille = "auto",
                      epiPeriode = 12, epiHarmo = 2, epiTendance = 3,
                      epiConf = 0.95, epiAjust = character(0), epiLancer = 1)
    session$setInputs(epiFigure = "cumul", epiRefLab = TRUE, epiRefEp = 2.4)
    p <- figure()
    expect_true(inherits(p, "ggplot"))
    vl <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
    expect_equal(length(vl), 1L)
    expect_equal(as.numeric(vl[[1]]$aes_params$linewidth), 2.4)

    # L'EPAISSEUR SUIT LE REGLAGE, elle n'est pas une constante : on la change
    # et on la relit. Sans cette seconde lecture, une valeur codee en dur a 2,4
    # passerait aussi.
    session$setInputs(epiRefEp = 0.8)
    vl2 <- Filter(function(l) inherits(l$geom, "GeomVline"), figure()$layers)
    expect_equal(as.numeric(vl2[[1]]$aes_params$linewidth), 0.8)

    # « NOMMER LA LIGNE » N'EFFACE PLUS LE TRAIT. Decochee, la case retirait la
    # couche entiere -- donc la reference a laquelle tous les RR se rapportent,
    # pour un libelle qui ne parle que du nom.
    nt <- function(p) length(Filter(function(l) inherits(l$geom, "GeomText"), p$layers))
    avant <- nt(figure())
    session$setInputs(epiRefLab = FALSE)
    p3 <- figure()
    vl3 <- Filter(function(l) inherits(l$geom, "GeomVline"), p3$layers)
    expect_equal(length(vl3), 1L)
    expect_equal(nt(p3), avant - 1L)
  })
})

test_that("les fenetres de retard sont une option, et decochees elles ne posent aucun tableau", {
  skip_if_not_installed("dlnm")
  skip_if_not_installed("shiny")
  set.seed(53); n <- 160
  d <- data.frame(date = seq(as.Date("2010-01-01"), by = "month", length.out = n))
  d$tmax  <- 28 + 3 * sin(2 * pi * seq_len(n) / 12) + stats::rnorm(n, 0, 1)
  d$nais  <- stats::rpois(n, 300)
  d$prema <- stats::rpois(n, exp(log(d$nais) - 3.1 + 0.05 * (d$tmax - 28)))
  vals <- shiny::reactiveValues(data = d, cleanData = NULL, filteredData = NULL,
                                resetSignal = 0L)
  shiny::testServer(mod_epidemio_server, args = list(values = vals), {
    session$setInputs(epiAnalyse = "dlnm", epiY = "prema", epiExpo = "tmax",
                      epiOffset = "nais", epiTemps = "date", epiMutuel = TRUE,
                      epiLag = 4, epiNkLag = 2, epiFamille = "auto",
                      epiPeriode = 12, epiHarmo = 2, epiTendance = 3,
                      epiConf = 0.95, epiAjust = character(0), epiLancer = 1)
    fen <- function() sum(grepl("^Fenetres", names(tables())))

    session$setInputs(epiFenOn = FALSE)
    expect_equal(length(fenetres()), 0L)
    expect_equal(fen(), 0L)

    # DECOCHEE, ON NE RETOMBE PAS SUR LE CATALOGUE PAR DEFAUT : cela poserait
    # les tableaux que l'utilisateur vient de retirer, et le reglage serait un
    # reglage que l'application ignore.
    session$setInputs(epiFenOn = TRUE,
                      epiFenetres = "0-1 court terme\n2-4 différé",
                      epiFenPct = 90)
    expect_equal(length(fenetres()), 2L)
    expect_equal(fen(), 1L)
  })
})

test_that("les cinq modules d'application demandes vivent sous « 3. Relations & inférence »", {
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("htmltools")
  root <- .hstat_repo_root()
  app <- file.path(root, "inst", "app")
  e <- new.env(parent = globalenv())
  ok <- tryCatch({
    suppressMessages(suppressWarnings({
      old <- setwd(app); on.exit(setwd(old), add = TRUE)
      socle <- file.path(root, "R")
      for (f in c(file.path(socle, "utils.R"),
                  list.files(socle, pattern = "^mod_.*[.]R$", full.names = TRUE)))
        try(sys.source(f, e), silent = TRUE)
      hstat_installer_replis_ui(e)
      try(sys.source("UX.R", e), silent = TRUE)
    }))
    exists("ui", envir = e)
  }, error = function(err) FALSE)
  skip_if_not(isTRUE(ok), "interface non constructible dans cet environnement")

  # LA MESURE PORTE SUR LE MENU RENDU, pas sur l'ordre des lignes du fichier :
  # un item place dans un autre conteneur y sortirait au meme rang de source et
  # a une tout autre place a l'ecran.
  html <- paste(as.character(htmltools::renderTags(get("ui", e))$html), collapse = "\n")
  pos <- function(motif) {
    p <- regexpr(motif, html, fixed = TRUE)
    expect_true(p > 0, label = motif)
    as.integer(p)
  }
  h3 <- pos("3. Relations &amp; inf")
  h4 <- pos("4. Mod")
  expect_true(h3 < h4)
  for (tab in c("yield", "threshold", "diversity", "epidemio", "dl50")) {
    p <- pos(paste0("#shiny-tab-", tab))
    expect_true(p > h3 && p < h4, label = tab)
  }
  # ET LA SECTION 5 GARDE LES SIENS : sans cette moitie, une interface qui
  # aurait tout empile sous la section 3 passerait aussi.
  h5 <- pos("5. Planification")
  for (tab in c("design", "dosage")) {
    p <- pos(paste0("#shiny-tab-", tab))
    expect_true(p > h5, label = tab)
  }
})

test_that("les familles ajoutees au kit agissent, et le pas refuse un axe discret", {
  skip_if_not_installed("ggplot2")
  o <- list(familles = c("styles", "angles", "legende", "bornes", "pas"),
            st_titre = "italic", st_axes = "bold", st_grad = "bold.italic",
            angle_x = 45, angle_y = 30,
            leg_titre = "Groupe", leg_titre_taille = 21, leg_taille = 19,
            bornes = c(xmin = 2, xmax = 4, ymin = NA_real_, ymax = NA_real_),
            pas = c(x = NA_real_, y = 2))
  th <- hstat_plot_extras_theme(o)
  expect_equal(ggplot2::calc_element("plot.title", th)$face, "italic")
  expect_equal(ggplot2::calc_element("axis.title", th)$face, "bold")
  expect_equal(ggplot2::calc_element("axis.text", th)$face, "bold.italic")
  expect_equal(ggplot2::calc_element("axis.text.x", th)$angle, 45)
  expect_equal(ggplot2::calc_element("axis.text.y", th)$angle, 30)
  # L'ANGLE COMMANDE LE CALAGE : une etiquette penchee finit SOUS sa
  # graduation. Sans cette assertion, un `hjust` fige a 0,5 passerait.
  expect_equal(ggplot2::calc_element("axis.text.x", th)$hjust, 1)
  expect_equal(ggplot2::calc_element("legend.title", th)$size, 21)
  expect_equal(ggplot2::calc_element("legend.text", th)$size, 19)

  # `titre = FALSE` NE TOUCHE PAS `plot.title`, et ce n'est pas un confort :
  # ggplot REFUSE de fusionner un `element_text()` sur un `element_markdown()`
  # (« Only elements of the same class can be merged »), si bien qu'un module
  # qui pose son titre en markdown verrait la figure entiere lever -- et
  # seulement la ou ggtext est installe.
  th0 <- hstat_plot_extras_theme(o, titre = FALSE)
  expect_null(th0$plot.title)
  expect_equal(ggplot2::calc_element("axis.title", th0)$face, "bold")

  d <- data.frame(g = c("A", "B", "C"), v = c(1, 5, 9), stringsAsFactors = FALSE)
  cont <- ggplot2::ggplot(d, ggplot2::aes(.data[["v"]], .data[["v"]])) +
    ggplot2::geom_point()
  disc <- ggplot2::ggplot(d, ggplot2::aes(.data[["g"]], .data[["v"]])) +
    ggplot2::geom_col()

  brk <- function(p, ax) {
    b <- ggplot2::ggplot_build(hstat_plot_extras_scales(p, ax))
    gr <- ggplot2::ggplotGrob(hstat_plot_extras_scales(p, ax))
    i <- grep("^axis-l", gr$layout$name)
    tx <- character(0)
    rec <- function(x) {
      if (!is.null(x$label)) tx <<- c(tx, as.character(x$label))
      if (!is.null(x$children)) for (k in x$children) rec(k)
      if (!is.null(x$grobs)) for (k in x$grobs) rec(k)
    }
    for (k in i) rec(gr$grobs[[k]])
    tx
  }
  # LE MAPPAGE S'EVALUE : `.data[["v"]]` est l'idiome dominant du depot, et une
  # lecture par `all.vars()` y rendait « .data » -- le pas ne s'appliquait
  # alors JAMAIS sur le cas general, sans que rien ne le dise.
  expect_equal(brk(cont, o), c("2", "4", "6", "8"))
  # Le meme reglage sur un axe DISCRET ne pose rien : « une graduation sur
  # deux » n'a pas de sens sur des noms de traitement, et poser une echelle
  # continue par-dessus ferait lever ggplot -- donc tomber l'onglet.
  ox <- o; ox$pas <- c(x = 2, y = NA_real_)
  expect_s3_class(ggplot2::ggplot_build(hstat_plot_extras_scales(disc, ox)),
                  "ggplot_built")

  # Les bornes recadrent sans rien retirer : la coord se pose, le nombre
  # d'observations tracees ne bouge pas.
  b <- ggplot2::ggplot_build(hstat_plot_extras_scales(cont, o))
  expect_equal(NROW(b$data[[1]]), 3L)
  expect_equal(b$layout$coord$limits$x, c(2, 4))
  # Et rien ne se pose quand aucune borne n'est saisie.
  o0 <- o; o0$bornes <- c(xmin = NA_real_, xmax = NA_real_,
                          ymin = NA_real_, ymax = NA_real_)
  expect_null(hstat_plot_extras_scales(cont, o0)$coordinates$limits$x)
})

test_that("le module de diversite porte tout le vocabulaire de mise en forme", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")
  d <- data.frame(
    Parcelle  = rep(c("P1", "P2", "P3"), each = 5),
    Espece    = rep(paste0("sp", 1:5), times = 3),
    Abondance = c(40, 20, 10, 5, 1, 12, 12, 11, 10, 9, 60, 3, 2, 1, 0),
    stringsAsFactors = FALSE)
  vals <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d)
  # LE TEST PORTE SUR LE MODULE : un test appelant `hstat_plot_extras_theme()`
  # serait reste vert pendant que le module ne lui passe pas ses familles.
  shiny::testServer(mod_diversity_server, args = list(values = vals), {
    session$setInputs(divFormat = "long", divSite = "Parcelle",
                      divEspece = "Espece", divAbondance = "Abondance",
                      divBase = "2", divCalculer = 1)
    session$setInputs(divGraphique = "indices", divIndiceTrace = "Shannon_H")
    ce <- function(p, w) ggplot2::calc_element(w, p$theme)
    av <- graphique()
    expect_s3_class(av, "ggplot")

    session$setInputs(divXStTitre = "italic", divXStAxes = "bold.italic",
                      divXStGrad = "italic", divXAngleX = 45, divXAngleY = 30,
                      divXLegendeTaille = 19, divXLegendeTitreTaille = 21,
                      divXYmin = 0, divXYmax = 4, divXPasY = 2)
    ap <- graphique()
    expect_equal(ce(ap, "plot.title")$face, "italic")
    # ON MESURE L'ELEMENT DESSINE, JAMAIS SON PARENT. `axis.title` est le
    # parent ; ce que la figure trace est `axis.title.x`, un `element_textbox`
    # qui fixe SA PROPRE face -- donc n'herite rien. Mesurer le parent laissait
    # passer un reglage declare, lu, applique au theme, et sans le moindre
    # effet : 20 814 octets de PNG avant et apres, mesure au navigateur.
    expect_equal(ce(ap, "axis.title.x")$face, "bold.italic")
    expect_equal(ce(ap, "axis.title.y")$face, "bold.italic")
    expect_equal(ce(ap, "axis.text.x")$face, "italic")
    expect_equal(ce(ap, "axis.text.x")$angle, 45)
    expect_equal(ce(ap, "axis.text.y")$angle, 30)
    expect_equal(ce(ap, "legend.text")$size, 19)
    expect_equal(ce(ap, "legend.title")$size, 21)
    # ET LES VALEURS D'AVANT DIFFERENT : sans cette moitie, des reglages codes
    # en dur aux memes valeurs passeraient aussi.
    expect_false(identical(ce(av, "axis.text.x")$angle, 45))
    expect_false(identical(ce(av, "legend.text")$size, 19))

    b <- ggplot2::ggplot_build(ap)
    expect_equal(b$layout$coord$limits$y, c(0, 4))
    gr <- ggplot2::ggplotGrob(ap)
    i <- grep("^axis-l", gr$layout$name)
    tx <- character(0)
    rec <- function(x) {
      if (!is.null(x$label)) tx <<- c(tx, as.character(x$label))
      if (!is.null(x$children)) for (k in x$children) rec(k)
      if (!is.null(x$grobs)) for (k in x$grobs) rec(k)
    }
    for (k in i) rec(gr$grobs[[k]])
    expect_equal(tx, c("0", "2", "4"))

    # LE TITRE RESTE UN `element_markdown` : le kit lui fournit la VALEUR du
    # style, il ne lui substitue pas un `element_text` -- qui ferait lever la
    # fusion, et tomber l'onglet, la ou ggtext est installe.
    expect_s3_class(ggplot2::ggplot_build(ap), "ggplot_built")
  })
})

test_that("le module DL50 prend du kit les deux familles qui lui manquaient", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")
  d <- data.frame(Dose = c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5),
                  N = rep(25, 7), Morts = c(0, 2, 6, 12, 18, 22, 24))
  vals <- shiny::reactiveValues(data = d, cleanData = NULL, filteredData = NULL,
                                resetSignal = 0L)
  shiny::testServer(mod_dl50_server, args = list(values = vals), {
    session$setInputs(source = "fichier", colDose = "Dose", colN = "N",
                      colMorts = "Morts", colEssai = "", importerDonnees = 1,
                      methode = "em", alpha = 0.05, gTous = TRUE,
                      gType = "probit")
    ce <- function(p, w) ggplot2::calc_element(w, p$theme)
    av <- graphe()
    expect_s3_class(av, "ggplot")
    session$setInputs(gAngleX = 45, gAngleY = 30)
    ap <- graphe()
    expect_equal(ce(ap, "axis.text.x")$angle, 45)
    expect_equal(ce(ap, "axis.text.y")$angle, 30)
    expect_false(identical(ce(av, "axis.text.x")$angle, 45))
    # LA LECTURE PORTE LES MEMES FAMILLES QUE LA DECLARATION : declarer un
    # widget que la lecture ignore, c'est le reglage qu'on deplace sans que
    # l'image bouge.
    expect_true(all(c("angles", "pas") %in% graphe_opt()$extras$familles))
  })

  # LE PAS SE MESURE SUR LES ETIQUETTES REELLEMENT DESSINEES, et l'echelle du
  # module garde son etiquetage EN DOSES : c'est ce qui prouve qu'on a modifie
  # l'echelle posee au lieu d'en ajouter une seconde, qui aurait remplace la
  # premiere en avertissant et rendu des log10 nus.
  e <- hstat_dl50_essai(c(0.00063, 0.00125, 0.0025, 0.005, 0.01, 0.02, 0.03),
                        rep(25, 7), c(5, 7, 9, 11, 14, 18, 20),
                        temoin_n = 25, temoin_morts = 0)
  f <- hstat_dl50_ajuste(e, "em")
  etiq <- function(q, cote) {
    g <- ggplot2::ggplotGrob(q)
    i <- grep(paste0("^axis-", cote), g$layout$name)
    tx <- character(0)
    rec <- function(x) {
      if (!is.null(x$label)) tx <<- c(tx, as.character(x$label))
      if (!is.null(x$children)) for (k in x$children) rec(k)
      if (!is.null(x$grobs)) for (k in x$grobs) rec(k)
    }
    for (k in i) rec(g$grobs[[k]])
    tx
  }
  sans <- hstat_dl50_graphique(list(f))
  avec <- hstat_dl50_graphique(list(f), list(extras = list(
    familles = "pas", pas = c(x = 1, y = NA_real_))))
  expect_true(all(grepl("^0[.]", etiq(avec, "b"))))
  expect_false(identical(etiq(sans, "b"), etiq(avec, "b")))
  avecy <- hstat_dl50_graphique(list(f), list(extras = list(
    familles = "pas", pas = c(x = NA_real_, y = 1))))
  expect_equal(etiq(avecy, "l"), c("-3", "-2", "-1", "0", "1", "2", "3"))
})

test_that("diversite et DL50 declarent tout le vocabulaire, sans un seul doublon", {
  skip_if_not_installed("shiny")
  ids <- function(ui) {
    h <- paste(as.character(htmltools::renderTags(ui)$html), collapse = "\n")
    x <- gsub('^ id="|"$', "", regmatches(h, gregexpr(' id="[^"]+"', h))[[1]])
    sub("^[^-]*-", "", x[!grepl("-label$", x)])
  }
  # LES FAMILLES SE PRENNENT UNE PAR UNE. Poser le kit entier sur un module qui
  # porte deja une partie du vocabulaire declarerait DEUX reglages pour un meme
  # trait, et c'est le second -- invisible -- qui finirait par mentir.
  for (m in list(list(f = mod_diversity_ui, id = "diversity", pfx = "divX",
                      fam = names(HSTAT_PLOT_EXTRAS_PAR_FAMILLE)),
                 list(f = mod_dl50_ui, id = "dl50", pfx = "g",
                      fam = c("axe", "cles", "marges", "angles", "pas")))) {
    v <- ids(m$f(m$id))
    expect_equal(v[duplicated(v)], character(0), label = m$id)
    for (fam in m$fam)
      for (suf in HSTAT_PLOT_EXTRAS_PAR_FAMILLE[[fam]])
        expect_true(paste0(m$pfx, suf) %in% v,
                    label = paste(m$id, fam, suf))
  }
})

# =============================================================================
#  ACP : LA CIBLE DU DEGRADE SUR UN BIPLOT, ET LES VARIABLES PEU INFORMATIVES
# =============================================================================

test_that("la cible du degrade se declare une fois, et un nom inconnu ne la deplace pas", {
  expect_equal(hstat_pca_cible_couleur("contrib"), "ind")
  expect_equal(hstat_pca_cible_couleur("cos2"),    "var")
  expect_equal(hstat_pca_cible_couleur("sat"),     "var")

  # Un identifiant de travers ne doit pas faire glisser la couleur d'une
  # famille a l'autre sans un mot : on retombe sur le comportement d'origine.
  expect_equal(hstat_pca_cible_couleur("n_existe_pas"), "ind")
  expect_equal(hstat_pca_cible_couleur(""),   "ind")
  expect_equal(hstat_pca_cible_couleur(NA),   "ind")

  # La table est la source unique : les trois metriques de l'interface y sont.
  expect_setequal(names(HSTAT_PCA_COLOR_CIBLE), c("contrib", "cos2", "sat"))
  expect_true(all(HSTAT_PCA_COLOR_CIBLE %in% c("ind", "var")))
})

test_that("la qualite d'une variable est la somme de ses cos2 sur les axes demandes", {
  skip_if_not_installed("FactoMineR")
  set.seed(11)
  n <- 60
  d <- data.frame(a = stats::rnorm(n), c = stats::rnorm(n),
                  dd = stats::rnorm(n), e = stats::rnorm(n))
  d$b <- d$a + stats::rnorm(n, 0, 0.1)
  res <- FactoMineR::PCA(d, graph = FALSE, ncp = 5)

  q12 <- hstat_pca_qualite_var(res, c(1, 2))
  expect_equal(nrow(q12), 5L)
  expect_setequal(q12$Variable, names(d))
  expect_equal(attr(q12, "axes"), c(1L, 2L))

  # La somme des cos2 sur TOUTES les composantes vaut 1 : c'est ce qui donne
  # son sens a la grandeur -- « la part de la variable que ces axes captent ».
  q5 <- hstat_pca_qualite_var(res, 1:5)
  expect_equal(unname(q5$Qualite), rep(1, 5), tolerance = 1e-8)

  # Trie du moins informatif au plus informatif.
  expect_false(is.unsorted(q12$Qualite, na.rm = TRUE))
  expect_equal(q12$Qualite_pct, round(100 * q12$Qualite, 1))

  # LE SOUS-ESPACE FAIT PARTIE DU CHIFFRE, et cela se mesure : le classement
  # change avec les axes. Sans cette assertion, une fonction qui ignorerait
  # completement `axes` passerait aussi -- et l'on retirerait une variable
  # informative parce qu'elle est faible sur un plan qu'on ne garde pas.
  q13 <- hstat_pca_qualite_var(res, c(1, 3))
  expect_false(identical(q12$Variable, q13$Variable))
  expect_equal(attr(q13, "axes"), c(1L, 3L))

  # Un axe hors du domaine est ignore, jamais pris pour un autre.
  expect_equal(attr(hstat_pca_qualite_var(res, c(1, 99)), "axes"), 1L)
})

test_that("une qualite incalculable reste NA, jamais zero, et passe en tete", {
  m <- matrix(c(0.90, 0.05,
                NaN,  NaN,
                0.20, 0.05),
              nrow = 3, byrow = TRUE,
              dimnames = list(c("bonne", "constante", "faible"),
                              c("Dim.1", "Dim.2")))
  q <- hstat_pca_qualite_var(list(var = list(cos2 = m, contrib = m)), c(1, 2))

  # `rowSums(na.rm = TRUE)` aurait rendu 0 -- c'est-a-dire EXACTEMENT le
  # chiffre d'une variable parfaitement orthogonale au plan. Deux situations
  # differentes sous un meme nombre : la variable constante n'a pas de cos2,
  # elle n'en a pas un nul.
  expect_true(is.na(q$Qualite[q$Variable == "constante"]))
  expect_false(isTRUE(all.equal(0, q$Qualite[q$Variable == "constante"])))

  # Elle passe en tete : c'est la variable dont on est le plus sur qu'elle
  # n'apprend rien.
  expect_equal(q$Variable[1], "constante")
  expect_equal(q$Qualite[q$Variable == "bonne"],  0.95)
  expect_equal(q$Qualite[q$Variable == "faible"], 0.25)

  # Un resultat a UN SEUL axe rend ses cos2 en VECTEUR NU : `m[, ax]` echouerait
  # sur « incorrect number of dimensions » sans `hstat_coord_mat()`.
  v <- stats::setNames(c(0.8, 0.1), c("x", "y"))
  qv <- hstat_pca_qualite_var(list(var = list(cos2 = v, contrib = v)), c(1, 2))
  expect_equal(qv$Variable, c("y", "x"))
  expect_equal(attr(qv, "axes"), 1L)

  # Aucune variable : un tableau vide, jamais une erreur -- le panneau alimente
  # une sortie Shiny, ou une erreur ferait tomber tout le bloc.
  vide <- hstat_pca_qualite_var(list(var = list(cos2 = NULL)))
  expect_equal(nrow(vide), 0L)
  expect_equal(hstat_pca_var_faibles(vide), character(0))
})

test_that("le seuil des variables faibles est strict, et retient les incalculables", {
  q <- data.frame(Variable = c("cst", "bas", "pile", "haut"),
                  Qualite  = c(NA, 0.10, 0.30, 0.90),
                  stringsAsFactors = FALSE)

  # Comparaison STRICTE : « pile au seuil » n'est pas sous le seuil.
  expect_equal(hstat_pca_var_faibles(q, 0.30), c("cst", "bas"))
  expect_equal(hstat_pca_var_faibles(q, 0.31), c("cst", "bas", "pile"))
  expect_equal(hstat_pca_var_faibles(q, 0),    "cst")

  # Une saisie videe en cours de frappe retombe sur le defaut plutot que de
  # faire tomber le panneau.
  expect_equal(hstat_pca_var_faibles(q, NA),    c("cst", "bas"))
  expect_equal(hstat_pca_var_faibles(q, "abc"), c("cst", "bas"))
})

test_that("le biplot de l'ACP route bien le degrade, pas seulement l'aide qui le calcule", {
  # LE TEST PORTE SUR LE MODULE, PAS SUR LA FONCTION. Un test qui n'appellerait
  # que `hstat_pca_cible_couleur()` resterait vert pendant que le biplot
  # continue de colorer les individus quoi qu'on choisisse : il verifierait que
  # la regle SAIT repondre, jamais que le graphique la LIT.
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")
  f <- file.path(root, "inst", "app", "app_server.R")
  skip_if_not(file.exists(f), "app_server.R absent (paquet installe)")

  ex <- parse(f, keep.source = FALSE)

  # Tous les appels a `fviz_pca_biplot`, a n'importe quelle profondeur.
  appels <- list()
  recolter <- function(e) {
    if (is.call(e)) {
      nm <- deparse(e[[1]])
      if (grepl("fviz_pca_biplot$", nm)) appels[[length(appels) + 1L]] <<- e
      for (i in seq_along(e)) if (!is.null(e[[i]])) recolter(e[[i]])
    } else if (is.pairlist(e) || is.list(e)) {
      for (i in seq_along(e)) if (!is.null(e[[i]])) recolter(e[[i]])
    }
  }
  for (i in seq_along(ex)) recolter(ex[[i]])
  expect_gte(length(appels), 2L)   # le trace, et la variante a ellipses

  # La cible est lue, et elle vient de l'aide partagee -- pas d'un `switch`
  # recopie dans chaque appel, qui finirait par diverger.
  src <- paste(deparse(ex), collapse = "\n")
  expect_true(grepl("hstat_pca_cible_couleur", src, fixed = TRUE))

  # Chaque appel conditionne `col.var` : un `col.var = "black"` en dur y
  # ignorerait la metrique. C'est l'assertion que la mutation fait echouer.
  for (k in seq_along(appels)) {
    cv <- appels[[k]][["col.var"]]
    expect_false(is.null(cv), label = paste("col.var absent, appel", k))
    expect_true(is.call(cv) && identical(as.character(cv[[1]]), "if"),
                label = paste("col.var non conditionnel, appel", k))
    expect_true(grepl("var_coloree", paste(deparse(cv), collapse = " "), fixed = TRUE),
                label = paste("col.var ne lit pas la cible, appel", k))
  }
})

test_that("le panneau des variables peu informatives est declare, lu et branche", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")
  ux <- file.path(root, "inst", "app", "UX.R")
  sv <- file.path(root, "inst", "app", "app_server.R")
  skip_if_not(file.exists(ux) && file.exists(sv), "sources absentes (paquet installe)")

  lux <- paste(.hstat_code_lignes(ux), collapse = "\n")
  lsv <- paste(.hstat_code_lignes(sv), collapse = "\n")

  # DECLARE : la sortie existe dans l'interface, sinon le serveur calcule pour
  # une place qui n'est nulle part.
  expect_true(grepl('uiOutput("pcaLowInfoPanel")', lux, fixed = TRUE))
  expect_true(grepl('output$pcaLowInfoPanel', lsv, fixed = TRUE))
  expect_true(grepl('output$pcaLowInfoList',  lsv, fixed = TRUE))

  # LU : chaque bouton declare est guette par un observateur -- un bouton
  # inerte se lit comme un reglage qui attend un clic.
  for (b in c("pcaLowInfoPreselect", "pcaLowInfoRemove")) {
    expect_true(grepl(paste0('actionButton("', b, '"'), lsv, fixed = TRUE), label = b)
    expect_true(grepl(paste0("input$", b), lsv, fixed = TRUE), label = b)
  }

  # EMPLOYE : le retrait ecrit reellement dans la selection de variables de
  # l'ACP. Sans cette ligne, on cocherait sans effet.
  expect_true(grepl('updatePickerInput(session, "pcaVars"', lsv, fixed = TRUE))

  # OPTIONNEL, ET FERME PAR DEFAUT. L'outil n'est pas un resultat de l'ACP :
  # une boite qui s'ouvrirait d'elle-meme pousserait hors de l'ecran les
  # reglages qu'on venait regler. La case commande l'AFFICHAGE *et* le CALCUL
  # -- les deux rendus sortent avant d'appeler `pcaQualiteVar()`, sans quoi la
  # qualite serait calculee pour une boite que personne n'a demandee.
  expect_true(grepl('checkboxInput("pcaShowLowInfo"', lux, fixed = TRUE))
  # LE DEFAUT SE LIT DANS L'ARBRE, PAS DANS LA LIGNE. Ma premiere assertion
  # cherchait l'appel ecrit sur une seule ligne : elle echouait sur un appel
  # correct simplement replie sur deux, c'est-a-dire qu'elle gardait une mise
  # en page et non un comportement.
  defaut <- local({
    ex <- parse(ux, keep.source = FALSE); trouve <- NULL
    visiter <- function(e) {
      if (is.call(e)) {
        if (grepl("checkboxInput$", deparse(e[[1]])) && length(e) >= 2 &&
            identical(e[[2]], "pcaShowLowInfo")) trouve <<- e
        for (i in seq_along(e)) if (!is.null(e[[i]])) visiter(e[[i]])
      }
    }
    for (i in seq_along(ex)) visiter(ex[[i]])
    trouve
  })
  expect_false(is.null(defaut))
  expect_identical(defaut[[4]], FALSE)   # ferme par defaut
  for (sortie in c("pcaLowInfoPanel", "pcaLowInfoList")) {
    i <- regexpr(paste0("output\\$", sortie, " <- shiny::renderUI"), lsv)
    corps <- substr(lsv, i, i + 400L)
    expect_true(grepl("if (!isTRUE(input$pcaShowLowInfo)) return(NULL)", corps, fixed = TRUE),
                label = sortie)
  }

  # LE COMMUTATEUR NE SE RECONSTRUIT PAS SOUS LE DOIGT : le cadre qui porte le
  # bouton radio ne depend pas de sa propre valeur (elle y est isolee), c'est
  # la LISTE qui en depend.
  i_cadre <- regexpr("output\\$pcaLowInfoPanel", lsv)
  i_liste <- regexpr("output\\$pcaLowInfoList",  lsv)
  cadre <- substr(lsv, i_cadre, i_liste - 1L)
  expect_true(grepl("isolate(input$pcaLowInfoBase)", cadre, fixed = TRUE))
  expect_false(grepl("pcaQualiteVar()", cadre, fixed = TRUE))
})

test_that("le noircissement des etiquettes ne vise que la famille qui porte le degrade", {
  skip_if_not_installed("ggplot2")
  # LES CALQUES DE GGPLOT2 SONT DES ENVIRONNEMENTS : la fonction modifie en
  # place. Rejouer deux appels sur le MEME graphique mesurerait le second etat
  # du premier et non deux codes -- c'est ce qui m'a fait conclure, a tort, que
  # le ciblage ne marchait pas. On reconstruit donc a chaque fois.
  neuf <- function() {
    ind <- data.frame(x = seq_len(20), y = seq_len(20), l = letters[1:20])
    va  <- data.frame(x = seq_len(4),  y = seq_len(4),  l = LETTERS[1:4])
    ggplot2::ggplot() +
      ggplot2::geom_text(data = ind, ggplot2::aes(x, y, label = l, colour = x)) +
      ggplot2::geom_text(data = va,  ggplot2::aes(x, y, label = l, colour = x))
  }
  teintes <- function(p) unname(vapply(p$layers,
    function(L) L$aes_params$colour %||% NA_character_, character(1)))

  expect_true(all(is.na(teintes(neuf()))))

  # Sans cible : tous les calques -- le cercle des correlations, ou il n'y a
  # qu'une famille de texte.
  expect_true(all(teintes(hstat_noircir_etiquettes(neuf())) == "#1a1a1a"))

  # Avec cible : SEULE la famille visee. L'assertion porte sur les deux
  # calques, pas seulement sur celui qu'on veut noircir -- sans le second
  # `expect_true`, une fonction qui noircirait tout passerait aussi.
  t4 <- teintes(hstat_noircir_etiquettes(neuf(), n_cible = 4, n_autre = 20))
  expect_true(is.na(t4[1]))            # les 20 individus restent en retrait
  expect_equal(t4[2], "#1a1a1a")       # les 4 variables sont noircies

  # Et la regle n'est pas « le plus petit calque » : on vise ce qu'on nomme.
  t20 <- teintes(hstat_noircir_etiquettes(neuf(), n_cible = 20, n_autre = 4))
  expect_equal(t20[1], "#1a1a1a")
  expect_true(is.na(t20[2]))

  # Comptes ambigus (autant de variables que d'individus) : on ne peut pas
  # distinguer les calques, on retombe sur « tous » plutot que de laisser un
  # nom illisible.
  expect_true(all(teintes(hstat_noircir_etiquettes(neuf(), n_cible = 4, n_autre = 4)) == "#1a1a1a"))

  # Le mapping de couleur est neutralise sur la cible, sinon le degrade
  # ecraserait la teinte posee au trace.
  q <- hstat_noircir_etiquettes(neuf(), n_cible = 4, n_autre = 20)
  expect_null(q$layers[[2]]$mapping$colour)
  expect_false(is.null(q$layers[[1]]$mapping$colour))

  # Une seule definition dans le depot : une copie locale finirait par diverger.
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")
  srcs <- .hstat_sources_app()
  skip_if(!length(srcs), "sources absentes (paquet installe)")
  defs <- sum(vapply(srcs, function(f)
    sum(grepl("mv_darken_text_labels", .hstat_code_lignes(f), fixed = TRUE)), integer(1)))
  expect_equal(defs, 0L)
})

# =============================================================================
#  LE FICHIER DES METRIQUES PORTE SES DETAILS TECHNIQUES
# =============================================================================

test_that("les details techniques se replient en une table, et une entree vide disparait", {
  x <- hstat_details_techniques(
    "Modele"      = "Foret aleatoire",
    "Predicteurs" = c("a", "b", "c"),
    "n"           = 42L,
    "Absent"      = NULL,
    "Vide"        = character(0),
    "Manquant"    = NA,
    version = FALSE, date = FALSE)

  # Noms ASCII : la table voisine du meme classeur porte `Metrique` /
  # `Interpretation`, et un nom accente fait avertir `data.frame()` hors UTF-8.
  expect_equal(names(x), c("Detail", "Valeur"))
  expect_true(all(vapply(x, is.character, logical(1))))

  # UNE ENTREE VIDE DISPARAIT, elle ne s'ecrit pas « NA » : un hyperparametre
  # absent parce que le modele n'en a pas n'est pas une valeur manquante.
  expect_equal(x$Detail, c("Modele", "Predicteurs", "n"))
  expect_false(any(c("Absent", "Vide", "Manquant") %in% x$Detail))

  # UN VECTEUR SE REPLIE EN UNE CELLULE : la liste des predicteurs est une
  # information, la tronquer en perdrait une partie.
  expect_equal(x$Valeur[x$Detail == "Predicteurs"], "a, b, c")
  expect_equal(x$Valeur[x$Detail == "n"], "42")

  # Un argument non nomme est ignore plutot que de produire une ligne sans
  # intitule.
  expect_equal(nrow(hstat_details_techniques("sans nom", "A" = 1,
                                             version = FALSE, date = FALSE)), 1L)

  # Rien a dire : une table vide, jamais une erreur -- elle alimente un
  # telechargement, ou une erreur rend une page HTML nommee `.xlsx`.
  vide <- hstat_details_techniques(version = FALSE, date = FALSE)
  expect_equal(nrow(vide), 0L)
  expect_equal(names(vide), c("Detail", "Valeur"))

  # La version vient de `hstat_version()`, jamais d'un numero recopie : un
  # repli code en dur ne se met pas a jour et finit par mentir.
  v <- hstat_details_techniques("A" = 1, date = FALSE)
  expect_true("Version HStat" %in% v$Detail)
  expect_equal(v$Valeur[v$Detail == "Version HStat"], as.character(hstat_version()))
  expect_true("Date d'export" %in% hstat_details_techniques("A" = 1)$Detail)
})

test_that("l'export des metriques ecrit reellement la feuille des details", {
  skip_if_not_installed("openxlsx")
  # LE TEST PORTE SUR LE FICHIER, PAS SUR L'APPEL. Compter un `details_fun =`
  # dans le source dirait « branche » d'un export dont le classeur ne porte
  # qu'une feuille : c'est la lecon deja prise sur le bouton d'export de
  # l'epidemiologie, ou le motif comptait l'appel au lieu de l'affectation.
  out <- new.env()
  mets <- data.frame(Metrique = c("RMSE", "R2"), Valeur = c(1.5, 0.8),
                     stringsAsFactors = FALSE)
  det  <- function() hstat_details_techniques("Modele" = "RF", "n" = 10L,
                                              version = FALSE, date = FALSE)
  hstat_export_table_handlers(out, "tst", function() mets, "les_metriques",
                              details_fun = det)

  # `downloadHandler` range son `content` DEUX environnements plus bas : la
  # fonction rendue enveloppe un `renderFunc`, et c'est chez lui que vivent
  # `content` et `filename`. Le chercher au premier niveau rend `NULL`, et
  # l'appel echoue sur « could not find function » -- mesure prise, pas devinee.
  contenu <- function(h) environment(environment(h)$renderFunc)$content

  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f), add = TRUE)
  invisible(contenu(out[["tstXlsx"]])(f))
  expect_true(file.exists(f) && file.size(f) > 0)

  feuilles <- openxlsx::getSheetNames(f)
  expect_true("les_metriques" %in% feuilles)
  expect_true("details_techniques" %in% feuilles)

  lu <- openxlsx::read.xlsx(f, sheet = "details_techniques")
  expect_equal(names(lu), c("Detail", "Valeur"))
  expect_true("Modele" %in% lu$Detail)
  expect_equal(lu$Valeur[lu$Detail == "Modele"], "RF")

  # SANS `details_fun`, RIEN NE CHANGE : un export qui n'en veut pas garde son
  # unique feuille. Sans cette moitie, une fonction qui ajouterait toujours la
  # feuille passerait aussi.
  hstat_export_table_handlers(out, "seul", function() mets, "les_metriques")
  g <- tempfile(fileext = ".xlsx"); on.exit(unlink(g), add = TRUE)
  invisible(contenu(out[["seulXlsx"]])(g))
  expect_equal(openxlsx::getSheetNames(g), "les_metriques")

  # UN DETAIL QUI ECHOUE N'EMPORTE PAS L'EXPORT : le tableau demande reste la
  # promesse principale, et un `content =` qui leve rend une page d'erreur HTML
  # que le navigateur enregistre sous le nom `.xlsx`.
  hstat_export_table_handlers(out, "casse", function() mets, "les_metriques",
                              details_fun = function() stop("boum"))
  h <- tempfile(fileext = ".xlsx"); on.exit(unlink(h), add = TRUE)
  invisible(contenu(out[["casseXlsx"]])(h))
  expect_true(file.exists(h) && file.size(h) > 0)
  expect_equal(openxlsx::getSheetNames(h), "les_metriques")

  # CSV : deux tableaux font une archive, et c'est le comportement existant de
  # l'ecrivain commun -- pas une exception introduite ici.
  z <- tempfile(fileext = ".zip"); on.exit(unlink(z), add = TRUE)
  invisible(contenu(out[["tstCsv"]])(z))
  expect_setequal(utils::unzip(z, list = TRUE)$Name,
                  c("les_metriques.csv", "details_techniques.csv"))
})

test_that("les quatre exports de metriques portent leurs details techniques", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")

  attendus <- list(
    c(f = "R/mod_ml.R",          prefixe = "mlMet"),
    c(f = "R/mod_dl.R",          prefixe = "dlMet"),
    c(f = "R/mod_dl.R",          prefixe = "lstmMet"),
    c(f = "R/mod_timeseries.R",  prefixe = "tsMet"))

  for (a in attendus) {
    chemin <- file.path(root, a[["f"]])
    skip_if_not(file.exists(chemin), paste(a[["f"]], "absent"))
    ex <- parse(chemin, keep.source = FALSE)
    trouve <- NULL
    visiter <- function(e) {
      if (is.call(e)) {
        if (grepl("hstat_export_table_handlers$", deparse(e[[1]])) &&
            length(e) >= 3 && identical(e[[3]], a[["prefixe"]])) trouve <<- e
        for (i in seq_along(e)) if (!is.null(e[[i]])) visiter(e[[i]])
      }
    }
    for (i in seq_along(ex)) visiter(ex[[i]])
    expect_false(is.null(trouve), label = a[["prefixe"]])
    expect_false(is.null(trouve[["details_fun"]]),
                 label = paste(a[["prefixe"]], ": details_fun absent"))
    expect_true(grepl("hstat_details_techniques",
                      paste(deparse(trouve[["details_fun"]]), collapse = " "),
                      fixed = TRUE),
                label = paste(a[["prefixe"]], ": details montes a la main"))
  }
})

test_that("la graine du LSTM ecrite dans les details est celle qui est posee", {
  # `input$lstmSeed` N'EXISTE PAS : aucune interface ne le declare. Le repli de
  # `hstat_finite()` aurait ecrit « 123 » sous le nom d'un reglage que
  # l'utilisateur ne peut pas toucher -- un chiffre juste pour une mauvaise
  # raison. La feuille de details ecrit donc la constante REELLEMENT posee.
  expect_identical(HSTAT_LSTM_SEED, 123L)

  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")
  f <- file.path(root, "R", "mod_dl.R")
  skip_if_not(file.exists(f), "mod_dl.R absent")
  l <- paste(.hstat_code_lignes(f), collapse = "\n")

  # La constante est posee ET ecrite : deux 123 recopies finiraient par
  # diverger, et c'est le fichier exporte qui mentirait sur le tirage employe.
  expect_true(grepl("torch_manual_seed(HSTAT_LSTM_SEED)", l, fixed = TRUE))
  expect_true(grepl("HSTAT_LSTM_SEED)", l, fixed = TRUE))
  expect_false(grepl("input$lstmSeed", l, fixed = TRUE))
  # Et `lstmSeed` reste bien absent de l'interface : si on l'y ajoute un jour,
  # ce test rappelle qu'il faut aussi brancher la graine sur lui.
  expect_false(grepl('ns("lstmSeed")', l, fixed = TRUE))
})

# =============================================================================
#  CONFIGURATION : CE QUI EST INSTALLE DOIT SERVIR, ET CE QUI EST APPELE DOIT
#  ETRE VERIFIABLE
# =============================================================================

# Tous les paquets appeles par `pkg::` ou `pkg:::` dans le code du depot,
# releves par l'ANALYSEUR : un nom cite dans un commentaire ou une chaine
# (« essayez remotes::install_github », « identique a DescTools::CramerV »)
# n'est PAS un appel. Un balayage textuel en signalait quatre a tort.
.hstat_pkgs_appeles <- function() {
  out <- character(0)
  for (f in .hstat_sources_app()) for (e in parse(f)) {
    rec <- function(x) {
      if (!is.call(x)) return(invisible())
      if (is.name(x[[1]]) && as.character(x[[1]]) %in% c("::", ":::") &&
          length(x) == 3L && is.name(x[[2]]))
        out <<- c(out, as.character(x[[2]]))
      l <- as.list(x)
      for (i in seq_along(l)) if (!identical(l[[i]], quote(expr = ))) rec(l[[i]])
    }
    rec(e)
  }
  sort(unique(out))
}

.hstat_required_pkgs <- function() {
  root <- .hstat_repo_root()
  if (is.na(root)) return(character(0))
  f <- file.path(root, "R", "utils.R")
  if (!file.exists(f)) return(character(0))
  src <- paste(readLines(f, warn = FALSE), collapse = "\n")
  m <- regmatches(src, regexpr(
    "required_packages\\s*<-\\s*c\\((?:[^()]|\\([^()]*\\))*\\)", src))
  if (!length(m)) return(character(0))
  eval(parse(text = sub("required_packages\\s*<-\\s*", "", m)))
}

test_that("aucun paquet n'est installe au demarrage sans etre employe", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")
  req <- .hstat_required_pkgs()
  skip_if(!length(req), "required_packages introuvable (paquet installe)")

  # `required_packages` n'est pas une liste de dependances : `install_and_load()`
  # les installe ET LES ATTACHE au demarrage. Un paquet qui n'y sert a rien
  # coute donc deux fois -- l'attente du premier lancement, et un risque de
  # MASQUAGE que ce depot documente comme fatal (`mclust::em` masque
  # `shiny::em` et toute l'interface cesse de se construire).
  #
  # Mesure du jour ou ce test a ete ecrit : DIX-HUIT des 86 paquets de la liste
  # n'etaient appeles nulle part -- bslib, digest, forcats, GGally, ggdendro,
  # knitr, nortest, performance, plotrix, purrr, qqplotr, questionr, report,
  # reshape2, see, stringr, DescTools, epitools. Aucune exception n'a ete
  # necessaire : tous les autres sont bien appeles par `pkg::`, y compris les
  # aiguillages (`plotly`, `shinyWidgets`, `colourpicker`...), dont la
  # DEFINITION porte l'appel qualifie meme si les appelants ne le portent pas.
  inutiles <- setdiff(req, .hstat_pkgs_appeles())
  expect_equal(inutiles, character(0),
               info = paste0("installes et attaches pour rien : ",
                             paste(inutiles, collapse = ", ")))
})

test_that("tout paquet appele est declare, et tout Suggests sert a quelque chose", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")
  desc <- file.path(root, "DESCRIPTION")
  skip_if_not(file.exists(desc), "DESCRIPTION absent")
  d <- read.dcf(desc)
  champ <- function(n) if (n %in% colnames(d))
    sub("\\s*\\(.*", "", trimws(strsplit(d[1, n], ",")[[1]])) else character(0)
  declares <- c(champ("Imports"), champ("Suggests"), champ("Depends"))
  appeles  <- .hstat_pkgs_appeles()

  # Base et recommandes sont livres avec R : les exiger dans DESCRIPTION serait
  # faux, et les compter pour non declares gonflerait le decompte.
  livres <- rownames(utils::installed.packages(priority = c("base", "recommended")))

  # [1] Un paquet appele mais declare nulle part n'est installe par personne :
  #     `install.packages("HStat")` reussit, et l'analyse tombe a l'usage.
  expect_equal(setdiff(appeles, c(declares, livres)), character(0))

  # [2] Un Suggests que rien n'appelle est du poids mort. `testthat` est la
  #     seule exception legitime : la suite l'emploie par `library()`, jamais
  #     par `testthat::`.
  mort <- setdiff(declares, c(appeles, "testthat"))
  expect_equal(mort, character(0),
               info = paste0("declares et jamais appeles : ",
                             paste(mort, collapse = ", ")))
})

test_that("la CI installe ce qu'il faut pour que les appels soient verifiables", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")
  wf <- file.path(root, ".github", "workflows", "tests.yml")
  skip_if_not(file.exists(wf), "workflow absent")

  # LE TEST QUI VERIFIE LES APPELS `pkg::objet` SAUTE LES PAQUETS ABSENTS.
  # C'est juste -- sinon il echouerait sur l'environnement et non sur le code --
  # mais cela rend son silence ambigu : un paquet que personne n'installe voit
  # TOUS ses appels passer sans controle. C'est ainsi que `PMCMRplus::dunnTest`,
  # `factoextra::fviz_mfa_biplot` et cinq familles fantomes de glmmTMB ont
  # survecu dans ce depot.
  #
  # Ce test rend le trou VISIBLE plutot que de le combler entierement : deux
  # paquets restent hors CI par decision de cout (torch telecharge libtorch,
  # prophet une chaine Stan), et ils sont NOMMES ici. Un troisieme qui
  # apparaitrait devra etre installe, ou justifie au meme endroit.
  hors_ci_assume <- c("torch", "prophet")

  y <- paste(readLines(wf, warn = FALSE), collapse = "\n")
  ci <- unique(gsub('"', "", regmatches(y, gregexpr('"[A-Za-z0-9._]+"', y))[[1]]))
  livres <- rownames(utils::installed.packages(priority = c("base", "recommended")))

  # [1] La DECLARATION : tout paquet appele figure dans la liste du workflow,
  #     hors les deux exclusions assumees.
  expect_setequal(setdiff(.hstat_pkgs_appeles(), c(ci, livres)), hors_ci_assume)

  # [2] LA REALITE, et c'est une assertion differente. La premiere version de
  #     ce test s'arretait a [1] -- elle lisait la LISTE, pas l'ensemble
  #     reellement chargeable, et elle est passee au vert pendant que la CI
  #     ecrivait « Paquets facultatifs absents : kknn, heplots ».
  #
  #     Les deux s'installaient bien, mais ne se CHARGEAIENT pas, faute d'une
  #     bibliotheque SYSTEME : `heplots` tire `rgl` (OpenGL, libGLU) et `kknn`
  #     tire `igraph` (GLPK). Un paquet qui ne se charge pas laisse tous ses
  #     appels `pkg::` sans controle -- le trou que ce lot pretendait fermer,
  #     rouvert par une dependance qu'aucune liste de paquets R ne mentionne.
  #
  #     ABSENT ET CASSE NE SE MESURENT PAS PAREIL, et la premiere version de
  #     cette assertion les confondait. Elle ne regardait que
  #     `requireNamespace`, qui rend FALSE dans les DEUX cas, et se gardait
  #     par `Sys.getenv("CI")` -- vrai dans TOUS les jobs. Le job `R CMD check`
  #     n'installe que les `Imports` (c'est la raison d'etre de
  #     `_R_CHECK_FORCE_SUGGESTS_=false`) : elle y a donc declare « installes
  #     mais non chargeables » quarante-huit paquets simplement ABSENTS, en
  #     nommant une cause -- la bibliotheque systeme -- qui n'etait pas la
  #     leur. Elle echouait sur l'environnement au lieu du code, la faute meme
  #     que le commentaire ci-dessus dit eviter.
  #
  #     `find.package()` separe les deux, et c'est mesure plutot que suppose :
  #     sur un repertoire portant un DESCRIPTION mais aucun espace de noms
  #     chargeable -- la forme exacte qu'ont `kknn` et `heplots` sans libGLU --
  #     il rend un chemin la ou un paquet absent n'en rend aucun.
  #
  #     Le garde-fou `CI` disparait avec la confusion qui le rendait
  #     necessaire : un paquet absent est desormais ecarte PAR CONSTRUCTION,
  #     partout. Ce qui reste exige est la seule chose qui ne soit jamais
  #     normale -- un paquet POSE sur le disque qui ne se charge pas, parce
  #     qu'il laisse tous ses appels `pkg::` sans controle en ayant l'air
  #     installe.
  # La propriete dont depend tout ce qui suit, EPINGLEE : `requireNamespace`
  # rend FALSE dans les deux cas, `find.package` les separe. On fabrique la
  # condition « pose mais non chargeable » au lieu de la supposer -- un
  # repertoire portant un DESCRIPTION et aucun espace de noms. Sans cette
  # assertion, un retour au seul `requireNamespace` repasserait au vert.
  lib_essai <- file.path(tempdir(), "hstat_lib_essai")
  dir.create(file.path(lib_essai, "hstatcasse"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Package: hstatcasse", "Version: 1.0", "License: GPL-2"),
             file.path(lib_essai, "hstatcasse", "DESCRIPTION"))
  anciens_chemins <- .libPaths()
  on.exit(.libPaths(anciens_chemins), add = TRUE)
  .libPaths(c(lib_essai, anciens_chemins))
  expect_length(find.package("hstatcasse", quiet = TRUE), 1L)
  expect_false(requireNamespace("hstatcasse", quietly = TRUE))
  expect_length(find.package("hstatabsent", quiet = TRUE), 0L)
  .libPaths(anciens_chemins)

  attendus <- setdiff(.hstat_pkgs_appeles(), c(livres, hors_ci_assume))
  poses <- attendus[vapply(attendus,
                           function(p) length(find.package(p, quiet = TRUE)) > 0L,
                           logical(1))]
  illisibles <- poses[!vapply(poses, requireNamespace, logical(1), quietly = TRUE)]
  expect_equal(illisibles, character(0),
               info = paste0("poses sur le disque mais non chargeables ",
                             "(bibliotheque systeme ?) : ",
                             paste(illisibles, collapse = ", ")))
})

test_that("aucun predicat de colonnes ne passe par sapply, qui rend une liste sur un tableau vide", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")

  # MESURE, pas supposition. `sapply(df, is.numeric)` rend :
  #   * un `logical` des que `df` porte au moins une colonne ;
  #   * une LISTE VIDE quand il n'en porte aucune -- `sapply` ne peut alors
  #     rien simplifier.
  # Indexer avec cette liste leve « invalid subscript type 'list' », et le cas
  # s'atteint des que le filtrage a tout retire. Ces predicats alimentent des
  # sorties Shiny : une erreur y fait tomber le PANNEAU ENTIER, pas la ligne.
  vide <- data.frame(a = 1:3)[, 0, drop = FALSE]
  expect_true(is.list(sapply(vide, is.numeric)))
  expect_error(names(vide)[sapply(vide, is.numeric)], "invalid subscript")
  # `vapply` impose le type de retour : il rend `logical(0)` et traverse.
  expect_identical(vapply(vide, is.numeric, logical(1)),
                   stats::setNames(logical(0), character(0)))
  expect_equal(ncol(vide[, vapply(vide, is.numeric, logical(1)), drop = FALSE]), 0L)

  # Le correctif existait depuis `safe_cor`, mais sur UN SEUL site : quarante-
  # huit autres portaient encore la forme fragile. Ce balayage empeche son
  # retour. Les predicats ANONYMES restent hors de portee : leur valeur de
  # retour n'est pas garantie scalaire, et `vapply` y changerait le
  # comportement au lieu de le preserver.
  motif <- "sapply\\([^,]+,\\s*(is\\.numeric|is\\.factor|is\\.character|is\\.logical|is_categorical)\\s*\\)"
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    l <- .hstat_code_lignes(f)            # commentaires retires par l'analyseur
    k <- grep(motif, l)
    if (length(k)) fautifs <- c(fautifs, sprintf("%s:%d", basename(f), k))
  }
  expect_equal(fautifs, character(0),
               info = paste0("sapply() a predicat simple, a passer en vapply(..., logical(1)) : ",
                             paste(fautifs, collapse = ", ")))
})


# -- Parcourt l'arbre syntaxique et rend TOUS les appels ----------------------
# L'argument vide (`x[, 1]`) est un symbole de nom vide : le tenir dans une
# variable leve « argument is missing », on le teste donc par son nom.
.hstat_appels_de <- function(f) {
  ex <- tryCatch(parse(f), error = function(e) NULL)
  if (is.null(ex)) return(list())
  out <- list()
  marche <- function(e) {
    if (is.symbol(e) && !nzchar(as.character(e))) return(invisible(NULL))
    if (is.call(e)) {
      out[[length(out) + 1L]] <<- e
      for (i in seq_along(e)) marche(e[[i]])
    } else if (is.pairlist(e) || is.expression(e) || is.list(e)) {
      for (i in seq_along(e)) marche(e[[i]])
    }
    invisible(NULL)
  }
  for (e in ex) marche(e)
  out
}

test_that("aucun motif ne met \\s dans une classe entre crochets", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")

  # MESURE, pas supposition. Dans une classe entre crochets, le moteur par
  # defaut de R (TRE) prend le contre-oblique pour LUI-MEME : `[;\s]` est la
  # classe { ; , \ , s }. Le motif echoue donc DANS LES DEUX SENS --
  # il decoupe sur la lettre « s », et il ne decoupe pas sur l'espace.
  expect_equal(strsplit("le, la, les, sur", "[,;\\s]+")[[1]],
               c("le", " la", " le", " ", "ur"))
  # La forme POSIX, elle, fait ce qu'elle annonce.
  expect_equal(strsplit("le, la, les, sur", "[,;[:space:]]+")[[1]],
               c("le", "la", "les", "sur"))
  # Les deux resultats doivent differer, sans quoi l'assertion ne garderait rien.
  expect_false(identical(strsplit("le, la, les, sur", "[,;\\s]+")[[1]],
                         strsplit("le, la, les, sur", "[,;[:space:]]+")[[1]]))

  # Le depot employait les deux formes : trois sites saisis par l'utilisateur
  # (bornes de classes, proportions attendues du chi2, mots vides) portaient la
  # fautive pendant que la DL50 et les series temporelles portaient la juste.
  regex_fn <- c("strsplit", "grepl", "grep", "sub", "gsub", "regmatches",
                "gregexpr", "regexpr", "regexec")
  fautifs <- character(0)
  for (f in .hstat_sources_app()) for (e in .hstat_appels_de(f)) {
    nm <- paste(deparse(e[[1]]), collapse = "")
    if (!nm %in% regex_fn) next
    a <- as.list(e)[-1]
    # `pattern` est le 1er argument de grepl/sub/gsub, le 2e de strsplit.
    cands <- a[vapply(a, function(z) is.character(z) && length(z) == 1L, logical(1))]
    for (m in cands)
      if (grepl("\\[[^]]*\\\\s[^]]*\\]", m))
        fautifs <- c(fautifs, paste0(basename(f), " : ", m))
  }
  expect_equal(fautifs, character(0),
               info = paste0("\\s dans une classe entre crochets (employer [[:space:]]) : ",
                             paste(fautifs, collapse = ", ")))
})

test_that("trf() ne meurt jamais sur un argument fautif", {
  # `sprintf("%d", 7.5)` LEVE. Le repli d'origine rejouait le MEME sprintf sur
  # le gabarit francais, avec les MEMES arguments : il rattrapait une
  # traduction fautive, jamais un argument fautif, et l'erreur emportait toute
  # la sortie appelante -- un tableau, une figure, un panneau entier.
  expect_error(sprintf("%d obs", 7.5), "invalid format")

  expect_equal(trf("total = %d obs", 7L), "total = 7 obs")
  expect_equal(trf("total = %d obs", 7),  "total = 7 obs")   # double entier : inchange
  # La valeur a virgule se LIT au lieu de tout faire tomber.
  expect_equal(trf("total = %d obs", 7.5), "total = 7.5 obs")
  # Le pour-cent litteral et les largeurs survivent au repli.
  expect_equal(trf("100 %% de %d", 2.5), "100 % de 2.5")
  expect_equal(trf("%.2f et %d", 1.234, 9.9), "1.23 et 9.9")
  # Une faute de NOMBRE d'arguments ne se repare pas : le gabarit brut vaut
  # mieux qu'une sortie morte.
  expect_equal(trf("%d et %d", 5L), "%d et %d")
})

test_that("hstat_div_whittaker ne branche pas sur une statistique non calculable", {
  # `mean(rowSums(m > 0))` vaut NaN des que la matrice n'a aucune ligne, et
  # `if (NaN > 0)` leve « missing value where TRUE/FALSE needed ». La troisieme
  # branche de la fonction portait deja `isTRUE()` ; ses deux voisines l'avaient
  # manque -- garder une garde et oublier sa jumelle.
  expect_true(is.nan(mean(rowSums(matrix(numeric(0), 0, 0) > 0))))
  # Il AVERTIT (moyenne d'un vide) ; ce qu'on exige est qu'il ne LEVE pas.
  r <- suppressWarnings(expect_no_error(hstat_div_whittaker(matrix(numeric(0), 0, 0))))
  expect_s3_class(r, "data.frame")
  expect_true(all(is.na(r$Beta_Whittaker)))

  # Le cas normal ne bouge pas : gamma = 3, alpha = 2, donc 1,5 et 0,5.
  n <- hstat_div_whittaker(matrix(c(3, 1, 0, 0, 2, 4), nrow = 2, byrow = TRUE))
  expect_equal(n$Beta_multiplicatif[1], 1.5)
  expect_equal(n$Beta_Whittaker[1], 0.5)
})

test_that("aucun champ de session n'est ecrit sans etre jamais relu", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")

  # Dix champs etaient dans ce cas : quatre `chiSq*`, deux `manova*SummaryRows`
  # -- dont le calcul ENTIER, deux `lapply` de treize lignes, ne servait plus
  # qu'a les remplir --, `scheirerResults`, `anovaModel`, `postHocSyncTrigger`
  # et `sourceKind`. Le risque n'est pas le poids : c'est de corriger la copie
  # morte en croyant corriger l'analyse.
  #
  # Le critere compte les ACCES : `values$X <- v` fait un acces (le membre
  # gauche) et une ecriture. Un champ dont les acces n'excedent pas les
  # ecritures n'est donc relu nulle part.
  ecrits <- character(0); acces <- character(0)
  for (f in .hstat_sources_app()) for (e in .hstat_appels_de(f)) {
    nm <- paste(deparse(e[[1]]), collapse = "")
    if (nm %in% c("<-", "=", "<<-") && length(e) >= 2L) {
      g <- e[[2]]
      if (is.call(g) && identical(paste(deparse(g[[1]]), collapse = ""), "$") &&
          identical(paste(deparse(g[[2]]), collapse = ""), "values"))
        ecrits <- c(ecrits, paste(deparse(g[[3]]), collapse = ""))
    }
    if (identical(nm, "$") && length(e) >= 3L &&
        identical(paste(deparse(e[[2]]), collapse = ""), "values"))
      acces <- c(acces, paste(deparse(e[[3]]), collapse = ""))
  }
  te <- table(ecrits); ta <- table(acces)
  morts <- character(0)
  for (k in names(te)) {
    na <- if (k %in% names(ta)) ta[[k]] else 0L
    if (na <= te[[k]]) morts <- c(morts, k)
  }
  expect_equal(sort(morts), character(0),
               info = paste0("champs ecrits et jamais relus : ",
                             paste(sort(morts), collapse = ", ")))
})

test_that("la graine globale atteint le module de diversite", {
  root <- .hstat_repo_root()
  skip_if(is.na(root), "depot introuvable")

  # La graine est un widget de l'EN-TETE (`numericInput("globalSeed", ...)`),
  # donc `input$globalSeed` cote app_server -- onze sites l'emploient ainsi.
  # Un module ne peut pas le lire : son `input` est namespace. `mod_diversity`
  # lisait donc `values$globalSeed`, un champ que RIEN n'ecrit :
  # `hstat_finite(NULL, 123)` rendait 123 quoi que l'utilisateur choisisse, et
  # la courbe d'accumulation restait figee.
  #
  # Le piege est que le resultat AVAIT L'AIR reproductible -- rien ne signalait
  # que le reglage etait inerte. C'est le mode de defaillance le plus couteux
  # de ce depot : un chiffre faux et plausible.
  src <- paste(readLines(file.path(root, "R", "mod_diversity.R"), warn = FALSE),
               collapse = "\n")
  expect_false(grepl("values$globalSeed", src, fixed = TRUE))
  expect_true(grepl("graine_globale()", src, fixed = TRUE))

  app <- paste(readLines(file.path(root, "inst", "app", "app_server.R"),
                         warn = FALSE), collapse = "\n")
  expect_true(grepl('mod_diversity_server("diversity", values,', app, fixed = TRUE))
  expect_true(grepl("shiny::reactive(input$globalSeed)", app, fixed = TRUE))

  # Et la graine est PORTEUSE : deux graines donnent deux courbes, la meme
  # graine rend la meme. Sans cette paire, l'assertion passerait aussi sur une
  # fonction qui ignorerait completement la graine.
  set.seed(99)
  m <- matrix(stats::rpois(120, 1.2), nrow = 20)
  a  <- hstat_div_accumulation(m, permutations = 50, graine = 1)
  b  <- hstat_div_accumulation(m, permutations = 50, graine = 2)
  a2 <- hstat_div_accumulation(m, permutations = 50, graine = 1)
  expect_equal(a, a2)
  expect_false(isTRUE(all.equal(a, b)))
})

test_that("l'analyse textuelle survit a un vocabulaire vide", {
  # Le seuil de rarete ne retient que les termes vus au moins deux fois : une
  # liste de mots vides un peu fournie suffit donc a VIDER le vocabulaire.
  # `sapply` rend alors une LISTE de vecteurs de longueur nulle -- donc sans
  # `dim` -- et `matrix(., nrow = 0)` la refuse. Tout l'onglet textuel tombait,
  # y compris les frequences deja calculees plus haut, qui sont justes.
  #
  # Le defaut etait latent tant que les mots vides arrivaient mutiles par
  # `[,;\s]+` : ils ne retiraient rien. Corriger le motif l'a rendu ATTEIGNABLE.
  vide <- sapply(list(c("a"), c("b")), function(w) as.integer(character(0) %in% w))
  expect_null(dim(vide))
  expect_error(matrix(vide, nrow = 0L), "data is too long")

  txt <- c("prix eleves sur stock", "stock sans surprise et prix bas",
           "le stock des prix reste stable", "prix du stock souci constant")
  r <- hstat_q_text_analysis(txt, "t", min_char = 2, top_n = 10,
                             extra_stopwords = c("prix", "stock", "souci"))
  expect_true(is.list(r))
  expect_true(length(r$tables) > 0L)

  # Et les mots vides AGISSENT : ils quittent la table des frequences. Sans
  # cette assertion, une fonction qui les ignorerait passerait aussi.
  mots <- r$tables[[1]][[1]]
  expect_false(any(c("prix", "stock") %in% mots))
  sans <- hstat_q_text_analysis(txt, "t", min_char = 2, top_n = 10)
  expect_true("prix" %in% sans$tables[[1]][[1]])
})
