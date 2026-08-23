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
# avant, quatre modules etaient charges par leur nom (mod_ai, mod_report,
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

test_that("la normalite est evaluee DANS chaque groupe, pas sur le melange", {
  set.seed(11)
  # Deux groupes parfaitement normaux mais bien separes : leur melange est
  # bimodal et Shapiro le rejette (p ~ 1e-6) alors que chaque groupe le passe.
  # Tester le melange conduirait a deconseiller l'ANOVA quand elle convient.
  d <- data.frame(y = c(stats::rnorm(30, 0, 1), stats::rnorm(30, 8, 1)),
                  g = rep(c("A", "B"), each = 30), stringsAsFactors = FALSE)
  expect_lt(stats::shapiro.test(d$y)$p.value, 0.001)   # le melange echoue

  p <- hstat_data_profile(d, "y", "g")
  expect_equal(p$variables$y$normale$portee, "par groupe")
  expect_true(p$variables$y$normale$ok)
  expect_equal(nrow(p$variables$y$normale$detail), 2L)
  expect_true(all(p$variables$y$normale$detail$Normale))

  r <- hstat_reco_analyses(p)
  expect_true(grepl("t de Student|Welch", r$Analyse[r$Pertinence == "Recommandée"][1]))

  # Sans facteur, la normalite est bien evaluee globalement
  p0 <- hstat_data_profile(d, "y")
  expect_equal(p0$variables$y$normale$portee, "globale")
  expect_false(isTRUE(p0$variables$y$normale$ok))
})

test_that("le profil identifie correctement le type des variables", {
  d <- data.frame(
    quanti = stats::rnorm(50),
    ordinale = sample(1:5, 50, TRUE),
    binaire = sample(c(0, 1), 50, TRUE),
    categ = sample(c("a", "b", "c", "d"), 50, TRUE),
    stringsAsFactors = FALSE)
  p <- hstat_data_profile(d, names(d))
  expect_equal(p$variables$quanti$type, "quantitative")
  expect_equal(p$variables$ordinale$type, "ordinale")   # entier a peu de niveaux
  expect_equal(p$variables$binaire$type, "binaire")
  expect_equal(p$variables$categ$type, "categorielle")
  expect_equal(p$n, 50L)
  expect_equal(p$n_quanti, 1L)
  expect_null(hstat_data_profile(NULL))
  expect_null(hstat_data_profile(d, "colonne_absente"))
})

test_that("le recommandateur suit les regles statistiques classiques", {
  set.seed(4)
  reco1 <- function(p) hstat_reco_analyses(p)$Analyse[
    hstat_reco_analyses(p)$Pertinence == "Recommandée"][1]

  # Deux groupes, normalite intra-groupe, variances homogenes -> t de Student
  d <- data.frame(y = c(stats::rnorm(40, 0, 1), stats::rnorm(40, 1, 1)),
                  g = rep(c("A", "B"), each = 40), stringsAsFactors = FALSE)
  expect_match(reco1(hstat_data_profile(d, "y", "g")), "Student")

  # Trois groupes non normaux -> Kruskal-Wallis
  d3 <- data.frame(y = stats::rexp(90, 0.2),
                   g = rep(c("A", "B", "C"), each = 30), stringsAsFactors = FALSE)
  expect_equal(reco1(hstat_data_profile(d3, "y", "g")), "Kruskal-Wallis")
  # ... et le post-hoc doit etre propose a la suite
  r3 <- hstat_reco_analyses(hstat_data_profile(d3, "y", "g"))
  expect_true("Comparaisons post-hoc" %in% r3$Analyse)

  # Un groupe sous 5 observations : pas de test parametrique en tete
  dp <- data.frame(y = stats::rnorm(20, 5, 1),
                   g = rep(c("A", "B", "C", "D"), c(3, 3, 3, 11)),
                   stringsAsFactors = FALSE)
  rp <- hstat_reco_analyses(hstat_data_profile(dp, "y", "g"))
  expect_equal(rp$Analyse[rp$Pertinence == "Recommandée"][1], "Kruskal-Wallis")
  expect_true("Test exact / permutation" %in% rp$Analyse)

  # Deux qualitatives -> chi-deux, et la taille d'effet a enchainer
  dq <- data.frame(a = sample(c("x", "y"), 120, TRUE),
                   b = sample(c("u", "v", "w"), 120, TRUE), stringsAsFactors = FALSE)
  rq <- hstat_reco_analyses(hstat_data_profile(dq, c("a", "b"), "a"))
  expect_match(rq$Analyse[rq$Pertinence == "Recommandée"][1], "chi-deux")
  expect_true(any(grepl("Cramer", rq$Analyse)))

  # Mesures appariees -> version appariee du test
  da <- data.frame(y = stats::rnorm(60), g = rep(c("avant", "apres"), each = 30),
                   stringsAsFactors = FALSE)
  expect_match(reco1(hstat_data_profile(da, "y", "g", paired = TRUE)), "apparie")

  expect_null(hstat_reco_analyses(NULL))
})

test_that("le verdict signale un ecart sans jamais desavouer l'utilisateur", {
  set.seed(5)
  d <- data.frame(y = stats::rexp(90, 0.2), g = rep(c("A", "B", "C"), each = 30),
                  stringsAsFactors = FALSE)
  r <- hstat_reco_analyses(hstat_data_profile(d, "y", "g"))

  v_ok <- hstat_reco_verdict(r, "Kruskal-Wallis sur 3 groupes")
  expect_true(v_ok$coherent)

  v_no <- hstat_reco_verdict(r, "ANOVA à un facteur")
  expect_false(v_no$coherent)
  expect_true(grepl("Kruskal-Wallis", v_no$message, fixed = TRUE))
  # Le ton compte autant que le fond : on informe, on ne condamne pas.
  expect_true(grepl("ne disqualifie pas", v_no$message, fixed = TRUE))
  expect_true(grepl("À vous de trancher", v_no$message, fixed = TRUE))
  expect_false(grepl("erreur|faux|incorrect", tolower(v_no$message)))

  expect_null(hstat_reco_verdict(NULL, "x"))
  expect_null(hstat_reco_verdict(r, ""))
})

test_that("hstat_ai_capture depose un contexte exploitable", {
  values <- new.env()
  df <- data.frame(Test = "Test t", Variable = "score", p_value = 0.012,
                   stringsAsFactors = FALSE)
  ctx <- hstat_ai_capture(values, "Tests statistiques", "Test t de Student",
                          tables = list("Resultats" = df, "Vide" = NULL),
                          meta = list(variables = "score", groupe = "sexe"))
  expect_equal(ctx$title, "Test t de Student")
  # Les tableaux vides ne sont pas conserves : ils n'apporteraient rien a l'invite
  expect_equal(names(ctx$tables), "Resultats")
  expect_equal(ctx$meta$variables, "score")
  expect_identical(values$aiContext, ctx)

  txt <- hstat_ai_context_text(ctx)
  expect_true(grepl("Test t de Student", txt, fixed = TRUE))
  expect_true(grepl("score", txt, fixed = TRUE))
  expect_true(grepl("0.012", txt, fixed = TRUE))
  expect_equal(hstat_ai_context_text(NULL), "")
})

test_that("la lecture automatique relit les p-values sans modele ni reseau", {
  set.seed(6)
  d <- data.frame(y = c(stats::rnorm(30), stats::rnorm(30, 2)),
                  g = rep(c("A", "B"), each = 30), stringsAsFactors = FALSE)
  p <- hstat_data_profile(d, "y", "g")
  r <- hstat_reco_analyses(p)
  ctx <- list(title = "Test t de Student", module = "Tests",
              tables = list("Resultats" = data.frame(
                Test = c("Groupe A vs B", "Groupe A vs C"),
                p_value = c(0.0031, 0.42), stringsAsFactors = FALSE)),
              text = NULL, meta = list(), time = Sys.time())

  out <- hstat_ai_interpret_offline(ctx, p, r, hstat_reco_verdict(r, ctx$title))
  expect_true(grepl("Test t de Student", out, fixed = TRUE))
  # Les chiffres sont LUS, pas generes : chaque p-value doit s'y retrouver
  expect_true(grepl("0.0031", out, fixed = TRUE))
  expect_true(grepl("significatif", out, fixed = TRUE))
  expect_true(grepl("non significatif", out, fixed = TRUE))
  expect_true(grepl("Analyses appelées par vos données", out, fixed = TRUE))
  # Le rappel de responsabilite ne doit jamais disparaitre
  expect_true(grepl("vous appartient", out, fixed = TRUE))

  # Un seuil different change le verdict, pas les chiffres
  out01 <- hstat_ai_interpret_offline(ctx, p, r, NULL, alpha = 0.001)
  expect_true(grepl("0.0031", out01, fixed = TRUE))
  expect_false(grepl("-> significatif", out01, fixed = TRUE))

  expect_null(hstat_ai_interpret_offline(NULL))
})

test_that("l'invite d'interpretation interdit d'inventer et de decider", {
  ctx <- list(title = "ANOVA", module = "Tests",
              tables = list("R" = data.frame(p_value = 0.02)),
              text = NULL, meta = list(), time = Sys.time())
  pr <- hstat_ai_interpret_prompt(ctx, NULL, NULL, NULL, "scientifique")
  expect_true(grepl("N'invente aucun chiffre", pr, fixed = TRUE))
  expect_true(grepl("Ne recalcule rien", pr, fixed = TRUE))
  expect_true(grepl("appartient à l'utilisateur", pr, fixed = TRUE))
  expect_true(grepl("## Analyse recommandée pour la suite", pr, fixed = TRUE))
  # Les trois niveaux de redaction produisent bien des consignes differentes
  n <- vapply(c("scientifique", "vulgarise", "detaille"),
              function(k) hstat_ai_interpret_prompt(ctx, niveau = k), character(1))
  expect_equal(length(unique(n)), 3L)
})

test_that("le markdown du modele est converti sans pouvoir injecter de HTML", {
  h <- .hstat_md_to_html("## Titre\n\nUn **gras** et un *italique*.\n\n- point A\n- point B")
  expect_true(grepl("<h4", h, fixed = TRUE))
  expect_true(grepl("<strong>gras</strong>", h, fixed = TRUE))
  expect_true(grepl("<em>italique</em>", h, fixed = TRUE))
  expect_equal(lengths(regmatches(h, gregexpr("<li>", h, fixed = TRUE))), 2L)

  # Une reponse de modele reste du contenu non fiable : jamais injectee telle quelle
  inj <- .hstat_md_to_html("Voici <script>alert(1)</script> et <img onerror=x>")
  expect_false(grepl("<script>", inj, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", inj, fixed = TRUE))
  expect_equal(.hstat_md_to_html(""), "")
  expect_equal(.hstat_md_to_html(NULL), "")
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
  expect_true(is.function(hstat_ai_capture))
  expect_true(is.function(hstat_reco_analyses))
})

test_that("une analyse descriptive n'est pas jugee comme un test", {
  set.seed(9)
  d <- data.frame(y = stats::rnorm(90), g = rep(c("A", "B", "C"), each = 30),
                  stringsAsFactors = FALSE)
  r <- hstat_reco_analyses(hstat_data_profile(d, "y", "g"))

  # Sans module, la comparaison au catalogue s'applique
  v0 <- hstat_reco_verdict(r, "Statistiques descriptives")
  expect_false(v0$coherent)

  # Avec le module, on propose la suite au lieu de juger : une moyenne n'est
  # pas un mauvais test, c'est une etape anterieure.
  v <- hstat_reco_verdict(r, "Statistiques descriptives", "Analyses descriptives")
  expect_true(v$coherent)
  expect_true(v$exploratoire)
  expect_true(grepl("étape préliminaire", v$message, fixed = TRUE))
  expect_true(grepl("pas un test", v$message, fixed = TRUE))
  expect_true(grepl("À vous de décider", v$message, fixed = TRUE))

  # Un module de tests reste evalue normalement
  vt <- hstat_reco_verdict(r, "ANOVA à un facteur", "Tests statistiques")
  expect_false(vt$exploratoire)
})

test_that("hstat_ai_as_table accepte tout ce que renvoient les modules", {
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  expect_identical(hstat_ai_as_table(df), df)

  # Liste de valeurs nommees (calcul de puissance, metriques de modele...)
  tb <- hstat_ai_as_table(list(n = 64L, puissance = 0.8012345, test = "t apparie"))
  expect_equal(names(tb), c("Grandeur", "Valeur"))
  expect_equal(tb$Grandeur, c("n", "puissance", "test"))
  expect_equal(tb$Valeur[2], "0.80123")          # arrondi lisible
  # Les elements non scalaires sont ecartes plutot que de casser la conversion
  expect_equal(nrow(hstat_ai_as_table(list(n = 10, courbe = 1:100))), 1L)

  expect_equal(nrow(hstat_ai_as_table(matrix(1:4, 2))), 2L)
  expect_equal(nrow(hstat_ai_as_table(c(alpha = 0.05, beta = 0.2))), 2L)
  expect_null(hstat_ai_as_table(NULL))
  expect_null(hstat_ai_as_table(list()))
})

test_that("le bandeau de guidage ne revient nulle part", {
  # Le bandeau greffe sur les onglets et sa notification repetaient a chaque
  # resultat une recommandation que l'onglet dedie porte deja. Ils ont ete
  # retires ; ce test barre leur reintroduction, y compris par un identifiant
  # `aihint_*` reste dans une interface.
  root <- .hstat_repo_root()
  src  <- .hstat_sources_app()
  txt  <- unlist(lapply(src, readLines, warn = FALSE))
  code <- txt[!grepl("^\\s*#", txt)]           # les commentaires en parlent encore
  for (motif in c("aihint_", "hstat_ai_hint_slot", "hstat_ai_with_hint",
                  "hstat_ai_hint_ui", "hstat_ai_hint_text", "HSTAT_AI_HINT_IDS"))
    expect_false(any(grepl(motif, code, fixed = TRUE)), label = motif)

  # Le registre de capture, lui, reste : c'est lui qui alimente l'onglet
  # d'interpretation, le journal de reproductibilite et le rapport.
  expect_true(any(grepl("hstat_ai_capture", code, fixed = TRUE)))
  expect_true(is.function(hstat_ai_capture))
})

test_that("toutes les familles d'analyse deposent un contexte", {
  src <- unlist(lapply(.hstat_sources_app(), readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]
  pose <- unique(unlist(regmatches(
    src, gregexpr('hstat_ai_capture\\(values, "[^"]+"', src))))
  pose <- gsub('.*"([^"]+)"$', "\\1", pose)
  attendu <- c("Tests statistiques", "Comparaisons multiples", "Analyses multivariées",
               "Analyses descriptives", "Machine Learning", "Analyses qualitatives",
               "Séries temporelles", "Corrélations", "Deep Learning", "Plan & Puissance")
  for (m in attendu)
    expect_true(m %in% pose, info = paste("aucune capture pour :", m))
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

test_that("aucune capture n'est branchee sur un champ que personne n'ecrit", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  # CE QUE LE TEST CI-DESSUS NE VOIT PAS. Il cherche l'APPEL dans le source, et
  # le declarait donc couvert ; mais l'observateur des comparaisons post-hoc
  # guettait `values$multiResults`, un champ qui n'existe que dans la liste
  # initiale -- personne ne l'ecrit. Il ne s'est jamais declenche, et cette
  # famille manquait a l'onglet d'interpretation, au journal de
  # reproductibilite et au rapport, sans que rien ne le signale.
  #
  # On verifie donc le DECLENCHEUR, pas la presence de l'appel.
  fichiers <- .hstat_sources_app()
  lignes <- lapply(fichiers, .hstat_code_lignes)
  tout <- unlist(lignes)

  ecrits <- unique(c(
    gsub(".*values\\$([A-Za-z0-9_.]+)\\s*<<?-.*", "\\1",
         grep("values\\$[A-Za-z0-9_.]+\\s*<<?-", tout, value = TRUE)),
    gsub('.*values\\[\\["([A-Za-z0-9_.]+)"\\]\\].*', "\\1",
         grep('values\\[\\["[A-Za-z0-9_.]+"\\]\\]\\s*<<?-', tout, value = TRUE))))

  fautifs <- character(0)
  for (k in seq_along(fichiers)) {
    l <- lignes[[k]]
    deb <- grep("observeEvent\\(\\s*values\\$[A-Za-z0-9_.]+", l)
    for (i in deb) {
      champ <- sub(".*observeEvent\\(\\s*values\\$([A-Za-z0-9_.]+).*", "\\1", l[i])
      # Le corps de l'observateur : jusqu'a la prochaine declaration de meme
      # niveau. Une fenetre de 40 lignes suffit et evite d'analyser le fichier.
      corps <- paste(l[i:min(length(l), i + 40L)], collapse = " ")
      if (grepl("hstat_ai_capture", corps) && !(champ %in% ecrits))
        fautifs <- c(fautifs, sprintf("%s:%d values$%s", basename(fichiers[k]), i, champ))
    }
  }
  expect_equal(fautifs, character(0))
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

test_that("hstat_data_quality repere les problemes courants", {
  set.seed(21)
  n <- 60
  d <- data.frame(
    constante   = rep(7, n),
    presque_na  = c(rnorm(3), rep(NA, n - 3)),
    normale     = rnorm(n),
    nombres_txt = as.character(round(rnorm(n), 3)),
    ecrasante   = c(rep("oui", n - 1), "non"),
    identifiant = paste0("ID", seq_len(n)),
    stringsAsFactors = FALSE)
  dq <- hstat_data_quality(d)

  expect_true(is.data.frame(dq))
  expect_setequal(names(dq), c("Variable", "Constat", "Gravite", "Suggestion"))
  expect_true(all(dq$Gravite %in% HSTAT_QUALITE_GRAVITES))
  # Chaque constat doit porter une suggestion : un diagnostic sans issue ne sert a rien
  expect_true(all(nzchar(dq$Suggestion)))

  c_var <- function(v) dq$Constat[dq$Variable == v]
  expect_true(any(grepl("une seule valeur", c_var("constante"))))
  expect_true(any(grepl("manquantes", c_var("presque_na"))))
  expect_true(any(grepl("nombres stockes comme du texte", c_var("nombres_txt"))))
  expect_true(any(grepl("couvre", c_var("ecrasante"))))
  expect_true(any(grepl("valeurs distinctes", c_var("identifiant"))))
  # La variable saine n'apparait pas
  expect_length(c_var("normale"), 0L)

  # Les constats les plus graves passent en tete
  rang <- match(dq$Gravite, HSTAT_QUALITE_GRAVITES)
  expect_false(is.unsorted(rang))
})

test_that("hstat_data_quality detecte redondance, doublons et effectif insuffisant", {
  set.seed(22)
  x <- rnorm(50)
  d <- data.frame(a = x, b = x * 2 + 1e-9, c = rnorm(50))  # a et b colineaires
  d <- rbind(d, d[1:3, ])                                   # 3 doublons
  dq <- hstat_data_quality(d)
  expect_true(any(grepl("corrélation", dq$Constat)))
  expect_true(any(grepl("identique", dq$Constat)))

  # Peu d'observations pour beaucoup de variables
  petit <- as.data.frame(matrix(rnorm(10 * 8), nrow = 10))
  dq2 <- hstat_data_quality(petit)
  expect_true(any(grepl("observations pour", dq2$Constat)))
  expect_true(any(grepl("effectif total", dq2$Constat)))
})

test_that("un jeu de donnees sain ne genere aucune fausse alerte", {
  set.seed(23)
  d <- data.frame(
    score = rnorm(200, 10, 2),
    age = round(runif(200, 18, 75)),
    groupe = rep(c("A", "B", "C", "D"), each = 50),
    stringsAsFactors = FALSE)
  dq <- hstat_data_quality(d)
  expect_equal(nrow(dq), 1L)
  expect_true(grepl("aucun problème", dq$Constat[1]))
  expect_true(grepl("Aucun problème", hstat_data_quality_resume(dq)))

  expect_null(hstat_data_quality(NULL))
  expect_null(hstat_data_quality(data.frame()))
  expect_null(hstat_data_quality_resume(NULL))
})

test_that("le resume compte correctement les gravites", {
  dq <- rbind(
    .hstat_q_row("a", "x", "bloquant", "s"),
    .hstat_q_row("b", "y", "important", "s"),
    .hstat_q_row("c", "z", "important", "s"),
    .hstat_q_row("d", "w", "à surveiller", "s"))
  r <- hstat_data_quality_resume(dq)
  expect_true(grepl("4 constat", r, fixed = TRUE))
  expect_true(grepl("1 bloquant", r, fixed = TRUE))
  expect_true(grepl("2 important", r, fixed = TRUE))
  expect_true(grepl("1 à surveiller", r, fixed = TRUE))
})

test_that("TOUS les modules d'analyse deposent un contexte pour l'IA", {
  src <- unlist(lapply(.hstat_sources_app(), readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]
  pose <- gsub('.*"([^"]+)"$', "\\1", unique(unlist(regmatches(
    src, gregexpr('hstat_ai_capture\\(values, "[^"]+"', src)))))

  # La liste complete : aucun module ne doit rester muet.
  attendu <- c("Exploration", "Nettoyage", "Filtrage", "Analyses descriptives",
               "Visualisation", "Corrélations", "Tests statistiques",
               "Comparaisons multiples", "Analyses multivariées",
               "Analyses qualitatives", "Séries temporelles", "Machine Learning",
               "Deep Learning", "Plan & Puissance", "Seuils d'efficacité")
  for (m in attendu)
    expect_true(m %in% pose, info = paste("aucune capture pour le module :", m))
  expect_gte(length(pose), length(attendu))
})

# =============================================================================
#  JOURNAL DE REPRODUCTIBILITE
# =============================================================================

test_that("hstat_rlog_code produit le code R attendu par famille d'analyse", {
  cas <- function(module, titre, vars = NULL, grp = NULL)
    hstat_rlog_code(list(module = module, title = titre,
                         meta = list(variables = vars, groupe = grp)))

  expect_match(paste(cas("Exploration", "Structure"), collapse = " "), "str\\(donnees\\)")

  # Le test choisi doit suivre le TITRE, pas le module
  expect_match(cas("Tests statistiques", "ANOVA", "score", "groupe")[1],
               "aov\\(score ~ groupe", perl = TRUE)
  expect_match(cas("Tests statistiques", "Kruskal-Wallis", "score", "groupe"),
               "kruskal.test\\(score ~ groupe", perl = TRUE)
  expect_match(cas("Tests statistiques", "Test t de Student", "score", "groupe"),
               "t.test\\(score ~ groupe", perl = TRUE)
  expect_match(cas("Tests statistiques", "Normalité (données brutes)", "score"),
               "shapiro.test\\(donnees\\$score\\)", perl = TRUE)
  expect_match(cas("Tests statistiques", "Régression linéaire", "y", "x")[1],
               "lm\\(y ~ x", perl = TRUE)

  expect_match(cas("Corrélations", "Tests de correlation", c("a", "b"))[2],
               "cor.test\\(donnees\\$a, donnees\\$b\\)", perl = TRUE)
  expect_match(cas("Analyses multivariées", "Analyse en Composantes Principales (ACP)",
                   c("a", "b"))[1], "FactoMineR::PCA", fixed = TRUE)
  expect_match(cas("Analyses multivariées", "Classification k-means", c("a", "b")),
               "stats::kmeans", fixed = TRUE)
  expect_match(cas("Analyses qualitatives", "Tableau croise", c("sexe", "avis")),
               "chisq.test\\(table\\(", perl = TRUE)

  # Honnetete : quand le code exact n'est pas reconstituable, on ne devine pas
  expect_null(cas("Machine Learning", "Comparaison de modeles", "score"))
  expect_null(cas("Deep Learning", "Reseau de neurones", "score"))
  expect_null(cas("Nettoyage", "Etat des donnees", "score"))
  # ... ni quand les variables necessaires manquent
  expect_null(cas("Tests statistiques", "ANOVA"))
  expect_null(hstat_rlog_code(NULL))
})

test_that("les noms de variables non syntaxiques sont proteges", {
  code <- hstat_rlog_code(list(module = "Tests statistiques", title = "ANOVA",
    meta = list(variables = "ma variable", groupe = "groupe 2")))
  expect_match(code[1], "`ma variable` ~ `groupe 2`", fixed = TRUE)
  # Un nom deja syntaxique n'est pas alourdi
  expect_equal(.hstat_rlog_nom(c("score", "ma var", "x.1", "2eme")),
               c("score", "`ma var`", "x.1", "`2eme`"))
})

test_that("le script de session est du R valide et executable", {
  h <- list(
    list(module = "Exploration", title = "Structure",
         meta = list(variables = c("score", "groupe")), time = Sys.time()),
    list(module = "Tests statistiques", title = "ANOVA",
         meta = list(variables = "score", groupe = "groupe"), time = Sys.time()),
    list(module = "Corrélations", title = "Corrélations",
         meta = list(variables = c("score", "age")), time = Sys.time()),
    list(module = "Machine Learning", title = "Comparaison",
         meta = list(variables = "score"), time = Sys.time()))
  sc <- hstat_rlog_script(h, source = "essai.csv", version = "9.9.9")

  # 1. Le script doit s'analyser : un journal qui ne parse pas ne sert a rien
  f <- tempfile(fileext = ".R"); on.exit(unlink(f), add = TRUE)
  writeLines(sc, f)
  expect_silent(parse(f))

  # 2. Il doit s'executer sur de vraies donnees
  set.seed(2)
  donnees <- data.frame(score = stats::rnorm(60), age = stats::runif(60, 18, 70),
                        groupe = rep(c("A", "B", "C"), each = 20),
                        stringsAsFactors = FALSE)
  lignes <- strsplit(sc, "\n")[[1]]
  lignes <- lignes[!grepl("^donnees <- read.csv", lignes)]   # pas de fichier reel
  env <- new.env(); assign("donnees", donnees, envir = env)
  expect_error(
    utils::capture.output(eval(parse(text = paste(lignes, collapse = "\n")), envir = env)),
    NA)

  # 3. Le contenu attendu y est
  expect_true(grepl("Journal de session HStat 9.9.9", sc, fixed = TRUE))
  expect_true(grepl("essai.csv", sc, fixed = TRUE))
  expect_true(grepl("aov(score ~ groupe", sc, fixed = TRUE))
  expect_true(grepl("NON RECONSTITUE", sc, fixed = TRUE))   # l'etape ML signalee
  # L'ordre chronologique est respecte
  expect_lt(regexpr("1. Exploration", sc, fixed = TRUE),
            regexpr("2. Tests statistiques", sc, fixed = TRUE))

  # Session vide : un script utilisable quand meme
  vide <- hstat_rlog_script(NULL)
  expect_true(grepl("Aucune analyse enregistrée", vide, fixed = TRUE))
  expect_silent(parse(text = vide))
})

test_that("le nom de l'objet de donnees est configurable", {
  h <- list(list(module = "Exploration", title = "S",
                 meta = list(variables = "x"), time = Sys.time()))
  sc <- hstat_rlog_script(h, donnees = "mes_donnees")
  expect_true(grepl("mes_donnees <- read.csv", sc, fixed = TRUE))
  expect_true(grepl("str(mes_donnees)", sc, fixed = TRUE))
  expect_false(grepl("str(donnees)", sc, fixed = TRUE))
})

test_that("l'historique s'accumule sans doublon immediat", {
  values <- new.env()
  m <- list(variables = "score", groupe = "groupe")
  hstat_ai_capture(values, "Tests statistiques", "ANOVA", meta = m)
  hstat_ai_capture(values, "Tests statistiques", "ANOVA", meta = m)   # repetition
  expect_length(values$aiHistory, 1L)

  hstat_ai_capture(values, "Tests statistiques", "Kruskal-Wallis", meta = m)
  expect_length(values$aiHistory, 2L)
  # Revenir a une analyse deja faite la reinscrit : c'est bien un journal
  hstat_ai_capture(values, "Tests statistiques", "ANOVA", meta = m)
  expect_length(values$aiHistory, 3L)
  expect_equal(vapply(values$aiHistory, function(c0) c0$title, character(1)),
               c("ANOVA", "Kruskal-Wallis", "ANOVA"))
  # Le dernier contexte reste accessible pour l'interpretation
  expect_equal(values$aiContext$title, "ANOVA")
  # L'historique conserve desormais les tableaux : le rapport les reprend.
  # Sans tableau depose, l'entree en porte une liste vide, pas NULL.
  expect_length(values$aiHistory[[1]]$tables, 0L)
})

test_that("l'historique s'allege au-dela des analyses recentes", {
  values <- new.env()
  tb <- list(Resultat = data.frame(x = 1:3))
  for (i in seq_len(HSTAT_HIST_DETAIL + 3L))
    hstat_ai_capture(values, "Tests statistiques", paste("Analyse", i),
                     tables = tb, meta = list(variables = "x"),
                     plot = function() NULL)
  h <- values$aiHistory
  expect_length(h, HSTAT_HIST_DETAIL + 3L)
  # Les plus anciennes perdent tableaux et figures, pas leur identite : le
  # journal de reproductibilite continue de les citer.
  expect_null(h[[1]]$tables)
  expect_null(h[[1]]$plot)
  expect_equal(h[[1]]$title, "Analyse 1")
  # Les recentes gardent tout
  expect_length(h[[length(h)]]$tables, 1L)
  expect_true(is.function(h[[length(h)]]$plot))
})


# ===========================================================================
# RAPPORT AUTOMATIQUE (mod_report.R)
# ===========================================================================

.hstat_test_hist <- function() list(
  list(module = "Tests statistiques", title = "Test t de Student",
       time = Sys.time(), meta = list(variables = "poids", groupe = "sexe"),
       tables = list(Resultat = data.frame(Variable = "poids", t = 2.31,
                                           p = 0.0231)),
       plot = NULL),
  list(module = "Visualisation", title = "Graphique : poids x sexe",
       time = Sys.time(), meta = list(variables = c("poids", "sexe")),
       tables = list(), plot = NULL))

test_that("le markdown du rapport respecte les sections demandees", {
  h <- .hstat_test_hist()
  md <- hstat_report_markdown(h, titre = "Mon rapport", auteur = "A. B.",
                              contexte = "essai clinique",
                              donnees_resume = data.frame(Variable = "poids",
                                                          Type = "numerique"),
                              qualite = data.frame(Variable = "poids",
                                                   Constat = "3 % manquants"),
                              interpretation = "La difference est nette.",
                              reco = data.frame(Analyse = "Test t"),
                              script = "t.test(poids ~ sexe, data = donnees)",
                              version = "9.9.9")
  expect_true(grepl("# Mon rapport", md, fixed = TRUE))
  expect_true(grepl("A. B.", md, fixed = TRUE))
  expect_true(grepl("HStat 9.9.9", md, fixed = TRUE))
  expect_true(grepl("essai clinique", md, fixed = TRUE))
  for (titre in c("## Données analysées", "## Diagnostic de qualité",
                  "## Analyses menées", "## Interprétation",
                  "## Analyses appelées", "## Annexe"))
    expect_true(grepl(titre, md, fixed = TRUE), info = titre)
  # Le contenu des analyses y est, pas seulement leur titre
  expect_true(grepl("Test t de Student", md, fixed = TRUE))
  expect_true(grepl("0.0231", md, fixed = TRUE))

  # Une section non demandee ne doit pas apparaitre. Le piege : passer les
  # LIBELLES du vecteur au lieu de ses valeurs vide le rapport en silence.
  md2 <- hstat_report_markdown(h, sections = c("analyses"),
                               qualite = data.frame(V = 1),
                               script = "x <- 1")
  expect_true(grepl("## Analyses menées", md2, fixed = TRUE))
  expect_false(grepl("## Diagnostic", md2, fixed = TRUE))
  expect_false(grepl("## Annexe", md2, fixed = TRUE))
})

test_that("les tableaux markdown sont bien formes et bornes", {
  md <- .hstat_rep_tableau_md(data.frame(a = 1:3, b = c("x", "y", "z")))
  lignes <- strsplit(md, "\n")[[1]]
  expect_true(grepl("^\\| a \\| b \\|$", lignes[1]))
  expect_true(grepl("^\\|", lignes[2]) && grepl("---", lignes[2]))
  expect_length(lignes, 5L)
  # Au-dela du plafond, le tableau est tronque ET le dit
  gros <- .hstat_rep_tableau_md(data.frame(a = 1:100), max_lignes = 10L)
  expect_true(grepl("100 lignes au total", gros, fixed = TRUE))
  # Une barre verticale dans une cellule ne casse pas la colonne
  echap <- .hstat_rep_tableau_md(data.frame(a = "gauche|droite"))
  expect_true(grepl("gauche\\|droite", echap, fixed = TRUE))
  expect_equal(.hstat_rep_tableau_md(data.frame()), "*(tableau vide)*")
})

test_that("le convertisseur du rapport rend tableaux, code et titres", {
  md <- paste("# Titre", "", "| a | b |", "| --- | --- |", "| 1 | 2 |", "",
              "- point", "", "```r", "x <- 1 < 2", "```", "",
              "Texte **gras**.", sep = "\n")
  html <- .hstat_rep_md_to_html(md)
  expect_true(grepl("<h1>Titre</h1>", html, fixed = TRUE))
  expect_true(grepl("<table>", html, fixed = TRUE))
  expect_true(grepl("<th>a</th>", html, fixed = TRUE))
  expect_true(grepl("<td>1</td>", html, fixed = TRUE))
  expect_true(grepl("<li>point</li>", html, fixed = TRUE))
  expect_true(grepl("<strong>gras</strong>", html, fixed = TRUE))
  # Un tableau ne doit JAMAIS ressortir en barres verticales dans un paragraphe
  expect_false(grepl("<p>|", html, fixed = TRUE))
  # Le code de l'annexe est recopie tel quel, et echappe
  expect_true(grepl("<pre><code>", html, fixed = TRUE))
  expect_true(grepl("x &lt;- 1 &lt; 2", html, fixed = TRUE))
})

test_that("le convertisseur echappe le HTML injecte", {
  html <- .hstat_rep_md_to_html("<script>alert(1)</script>")
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", html, fixed = TRUE))
})

test_that("le resume du jeu de donnees decrit chaque variable", {
  d <- data.frame(poids = c(60, 70, NA), sexe = factor(c("F", "H", "F")),
                  stringsAsFactors = FALSE)
  r <- hstat_report_resume_donnees(d)
  expect_equal(nrow(r), 2L)
  expect_equal(r$Type, c("numérique", "catégorielle"))
  expect_equal(r$`Renseignées`, c(2L, 3L))
  expect_equal(r$Manquantes, c(1L, 0L))
  expect_true(grepl("60", r[["Modalités / étendue"]][1]))
  expect_true(grepl("2 modalité", r[["Modalités / étendue"]][2]))
  expect_null(hstat_report_resume_donnees(NULL))
})

test_that("une figure indessinable disparait sans faire tomber le rapport", {
  skip_if_not_installed("ggplot2")
  d <- file.path(tempdir(), paste0("hstat_test_fig_", as.integer(runif(1, 1e6, 1e7))))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  h <- list(
    list(module = "Visualisation", title = "Nuage", time = Sys.time(),
         meta = list(), tables = list(),
         plot = function() ggplot2::ggplot(data.frame(x = 1:5, y = 1:5),
                                           ggplot2::aes(x, y)) + ggplot2::geom_point()),
    list(module = "Visualisation", title = "Cassee", time = Sys.time(),
         meta = list(), tables = list(),
         plot = function() stop("variable supprimee entre-temps")),
    list(module = "Tests statistiques", title = "Sans figure",
         time = Sys.time(), meta = list(), tables = list(), plot = NULL))
  figs <- hstat_report_figures(h, dossier = d)
  expect_equal(nrow(figs), 1L)
  expect_true(grepl("Nuage", figs$titre[1], fixed = TRUE))
  expect_true(file.exists(figs$fichier[1]) && file.size(figs$fichier[1]) > 0)
  # Aucune figure du tout : un data.frame vide, pas une erreur
  expect_equal(nrow(hstat_report_figures(list(), dossier = d)), 0L)
  expect_equal(nrow(hstat_report_figures(h[3], dossier = d)), 0L)
})

test_that("le HTML incorpore ses figures et reste un fichier unique", {
  skip_if_not_installed("base64enc")
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  grDevices::png(png, width = 240, height = 240); plot(1); grDevices::dev.off()

  md <- hstat_report_markdown(list(), sections = "figures",
                              figures = data.frame(titre = "Ma figure",
                                                   fichier = png,
                                                   stringsAsFactors = FALSE))
  expect_true(grepl("## Figures", md, fixed = TRUE))
  expect_true(grepl(sprintf("![Ma figure](%s)", png), md, fixed = TRUE))

  html <- .hstat_rep_images_html(.hstat_rep_md_to_html(md))
  expect_true(grepl("<img src=\"data:image/png;base64,", html, fixed = TRUE))
  # Le chemin du fichier ne doit plus apparaitre : un rapport envoye par
  # courriel ne peut pas aller relire /tmp.
  expect_false(grepl(png, html, fixed = TRUE))

  # Figure absente : le rapport le dit au lieu d'afficher une image cassee
  manquante <- .hstat_rep_images_html(
    .hstat_rep_md_to_html("![X](/introuvable/fig.png)"))
  expect_true(grepl("figure indisponible", manquante, fixed = TRUE))
})

test_that("le rendu HTML aboutit toujours, et le repli se dit", {
  md <- hstat_report_markdown(.hstat_test_hist(), titre = "T")
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)
  r <- hstat_report_render(md, f, "html", "T")
  expect_true(r$ok)
  expect_equal(r$format, "html")
  expect_equal(r$message, "")
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_true(grepl("<!DOCTYPE html>", html, fixed = TRUE))
  expect_true(grepl("<table>", html, fixed = TRUE))

  # Word demande sur une machine sans pandoc : repli HTML, et on le DIT.
  r2 <- hstat_report_render(md, f, "docx", "T",
                            dispo = c(html = TRUE, docx = FALSE, pdf = FALSE))
  expect_true(r2$ok)
  expect_equal(r2$format, "html")
  expect_true(grepl("indisponible", r2$message, fixed = TRUE))
  expect_true(grepl("DOCX", r2$message, fixed = TRUE))

  r3 <- hstat_report_render(md, f, "pdf", "T",
                            dispo = c(html = TRUE, docx = TRUE, pdf = FALSE))
  expect_equal(r3$format, "html")
  expect_true(grepl("PDF", r3$message, fixed = TRUE))
})

test_that("le message de disponibilite oriente vers une solution", {
  expect_null(hstat_report_message_dispo(c(html = TRUE, docx = TRUE, pdf = TRUE)))
  m <- hstat_report_message_dispo(c(html = TRUE, docx = FALSE, pdf = FALSE))
  expect_true(grepl("pandoc", m, fixed = TRUE))
  expect_true(grepl("tinytex", m, fixed = TRUE))
  expect_true(grepl("HTML", m, fixed = TRUE))
  # Le HTML est toujours annonce comme disponible
  d <- hstat_report_formats_dispo()
  expect_true(d[["html"]])
  expect_named(d, c("html", "docx", "pdf"))
})

test_that("le rendu Word passe par pandoc quand il est la", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(isTRUE(tryCatch(rmarkdown::pandoc_available(),
                              error = function(e) FALSE)),
              "pandoc indisponible")
  f <- tempfile(fileext = ".docx")
  on.exit(unlink(f), add = TRUE)
  r <- hstat_report_render(hstat_report_markdown(.hstat_test_hist(), titre = "T"),
                           f, "docx", "T")
  expect_true(r$ok)
  expect_equal(r$format, "docx")
  expect_true(file.size(f) > 1000)
  # Un .docx est une archive zip contenant word/document.xml
  expect_true(any(grepl("word/document.xml", utils::unzip(f, list = TRUE)$Name,
                        fixed = TRUE)))
})


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

test_that("le plancher de resolution est bien de 1000 dpi", {
  expect_gte(HSTAT_REPORT_DPI_MIN, 1000L)
  # Toutes les resolutions proposees respectent le plancher
  expect_true(all(as.numeric(HSTAT_REPORT_DPI) >= HSTAT_REPORT_DPI_MIN))
  expect_equal(unname(HSTAT_REPORT_DPI[1]), "1000")
})

test_that("une figure de rapport sort a 1000 dpi par defaut", {
  skip_if_not_installed("ggplot2")
  d <- file.path(tempdir(), paste0("hstat_dpi_", as.integer(runif(1, 1e6, 1e7))))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- hstat_report_figures(.hstat_hist_figure(), dossier = d)
  expect_equal(nrow(f), 1L)
  px <- .hstat_png_dims(f$fichier[1])
  # 9 x 5,5 pouces a 1000 dpi. On verifie les PIXELS produits, pas l'argument :
  # un ggsave qui ignorerait le dpi passerait autrement inapercu.
  expect_equal(unname(px[["largeur"]]), 9000)
  expect_equal(unname(px[["hauteur"]]), 5500)
})

test_that("une resolution inferieure au plancher est remontee, pas obeie", {
  skip_if_not_installed("ggplot2")
  d <- file.path(tempdir(), paste0("hstat_dpi_", as.integer(runif(1, 1e6, 1e7))))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  for (bas in list(72, 150, 300, NA, "", NULL)) {
    f <- hstat_report_figures(.hstat_hist_figure(), dossier = d, dpi = bas)
    px <- .hstat_png_dims(f$fichier[1])
    expect_equal(unname(px[["largeur"]]), 9000,
                 info = paste("dpi demande :", paste(bas, collapse = "")))
  }
})

test_that("une resolution superieure au plancher est respectee", {
  skip_if_not_installed("ggplot2")
  d <- file.path(tempdir(), paste0("hstat_dpi_", as.integer(runif(1, 1e6, 1e7))))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- hstat_report_figures(.hstat_hist_figure(), dossier = d, dpi = 1200)
  px <- .hstat_png_dims(f$fichier[1])
  expect_equal(unname(px[["largeur"]]), 10800)
  # La chaine de caracteres du selecteur doit marcher comme le nombre
  f2 <- hstat_report_figures(.hstat_hist_figure(), dossier = d, dpi = "1200")
  expect_equal(unname(.hstat_png_dims(f2$fichier[1])[["largeur"]]), 10800)
})

test_that("seul l'apercu a l'ecran echappe au plancher", {
  skip_if_not_installed("ggplot2")
  d <- file.path(tempdir(), paste0("hstat_dpi_", as.integer(runif(1, 1e6, 1e7))))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- hstat_report_figures(.hstat_hist_figure(), dossier = d, apercu = TRUE)
  px <- .hstat_png_dims(f$fichier[1])
  expect_equal(unname(px[["largeur"]]), 1350)   # 9 pouces a 150 dpi
  # L'apercu ignore meme une resolution elevee explicitement demandee : il sert
  # a verifier la mise en page, pas a etre imprime.
  f2 <- hstat_report_figures(.hstat_hist_figure(), dossier = d,
                             apercu = TRUE, dpi = 2400)
  expect_equal(unname(.hstat_png_dims(f2$fichier[1])[["largeur"]]), 1350)
})

test_that("la progression est rapportee figure par figure", {
  skip_if_not_installed("ggplot2")
  d <- file.path(tempdir(), paste0("hstat_dpi_", as.integer(runif(1, 1e6, 1e7))))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  h <- rep(.hstat_hist_figure(), 3)
  vus <- list()
  hstat_report_figures(h, dossier = d, apercu = TRUE,
                       progres = function(i, n, titre) vus[[length(vus) + 1L]] <<- c(i, n))
  expect_length(vus, 3L)
  expect_equal(vapply(vus, function(x) x[1], numeric(1)), c(1, 2, 3))
  expect_true(all(vapply(vus, function(x) x[2], numeric(1)) == 3))

  # Un rappel qui echoue ne doit pas emporter le rapport : la progression est
  # un confort, les figures sont le livrable.
  expect_silent(f <- hstat_report_figures(h, dossier = d, apercu = TRUE,
                                          progres = function(i, n, t) stop("boum")))
  expect_equal(nrow(f), 3L)
})

test_that("l'interface propose de choisir la resolution", {
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- paste(readLines(.hstat_module_path("mod_ai.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("rep_dpi", src, fixed = TRUE))
  expect_true(grepl("HSTAT_REPORT_DPI", src, fixed = TRUE))
  # L'apercu passe par apercu = TRUE, le telechargement non : sans cela on
  # incorporerait 9000 px en base64 dans un onglet a chaque clic.
  expect_true(grepl("rep_figures(apercu = TRUE)", src, fixed = TRUE))
})


# ===========================================================================
# AUDIT : DONNEES QUI CASSENT LA MISE EN FORME
# ---------------------------------------------------------------------------
# Cinq defauts trouves en soumettant les fonctions pures a des donnees
# degenerees. Aucun ne levait d'erreur visible : ils produisaient un document
# faux, ou un script que R refusait d'executer. Ce sont les pires.
# ===========================================================================

test_that("une barre verticale dans un NOM de colonne ne casse pas le tableau", {
  d <- data.frame(a = 1:2, b = 3:4)
  names(d) <- c("Rendement|t/ha", "normal")
  md <- .hstat_rep_tableau_md(d)
  lignes <- strsplit(md, "\n")[[1]]
  # On compte les barres SEPARATRICES, donc non echappees : « \\| » appartient
  # a une valeur et ne delimite aucune colonne.
  sep <- function(s) lengths(gregexpr("(?<!\\\\)\\|", s, perl = TRUE))
  # L'en-tete et le separateur doivent annoncer le MEME nombre de colonnes ;
  # sinon le tableau ne se rend plus du tout.
  expect_equal(sep(lignes[1]), sep(lignes[2]))
  expect_equal(sep(lignes[1]), sep(lignes[3]))
  # La barre du nom est echappee, donc toujours lisible
  expect_true(grepl("Rendement\\|t/ha", md, fixed = TRUE))

  # Et le rendu HTML ne doit pas inventer une colonne : le convertisseur doit
  # honorer l'echappement au lieu de couper sur toutes les barres.
  html <- .hstat_rep_md_to_html(md)
  expect_equal(lengths(gregexpr("<th>", html)), 2L)
  expect_true(grepl("<th>Rendement|t/ha</th>", html, fixed = TRUE))
  expect_equal(lengths(gregexpr("<td>", html)), 4L)   # 2 lignes x 2 colonnes
})

test_that("un retour a la ligne dans une cellule ne scinde pas la ligne", {
  # Cas reel : les reponses libres du module qualitatif en contiennent.
  d <- data.frame(reponse = "trop cher\net livre en retard", n = 1L,
                  stringsAsFactors = FALSE)
  md <- .hstat_rep_tableau_md(d)
  lignes <- strsplit(md, "\n")[[1]]
  expect_length(lignes, 3L)              # en-tete, separateur, UNE ligne
  expect_true(grepl("trop cher et livre en retard", md, fixed = TRUE))
  # Meme protection sur un nom de colonne
  d2 <- data.frame(x = 1); names(d2) <- "titre\nsur deux lignes"
  expect_length(strsplit(.hstat_rep_tableau_md(d2), "\n")[[1]], 3L)
})

test_that("un crochet dans un titre de figure n'empeche pas son incorporation", {
  skip_if_not_installed("base64enc")
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  grDevices::png(png, width = 240, height = 240); plot(1); grDevices::dev.off()

  for (titre in c("Visualisation — Graphique : x]y",
                  "Analyse [ACP] des donnees",
                  "Titre\nsur deux lignes")) {
    md <- hstat_report_markdown(list(), sections = "figures",
            figures = data.frame(titre = titre, fichier = png,
                                 stringsAsFactors = FALSE))
    html <- .hstat_rep_images_html(.hstat_rep_md_to_html(md))
    expect_true(grepl("<img src=\"data:image/png;base64,", html, fixed = TRUE),
                info = titre)
    # Le markdown brut ne doit jamais ressortir dans le document
    expect_false(grepl("![", html, fixed = TRUE), info = titre)
  }
})

test_that("le script du journal reste executable quel que soit le nom de colonne", {
  # Le journal promet un script qui s'analyse et s'execute. Un accent grave
  # dans un nom fermait la citation trop tot : R refusait le script entier.
  noms <- c("a`b", "a\\b", "a\"b", "a; rm -rf /", "Rendement (t/ha)",
            "variable é", "2021", "")
  for (v in noms) {
    h <- list(list(module = "Tests statistiques", title = "ANOVA",
                   meta = list(variables = v, groupe = "g"), time = Sys.time()))
    sc <- hstat_rlog_script(h)
    expect_silent(parse(text = sc))
  }
  # La citation resiste, et le nom reste lisible
  expect_equal(.hstat_rlog_nom("a`b"), "`a\\`b`")
  expect_equal(.hstat_rlog_nom("simple"), "simple")
  # Vectorise : un appel porte souvent plusieurs variables
  expect_equal(.hstat_rlog_nom(c("x", "a b")), c("x", "`a b`"))
})

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

test_that("une variable sans aucune valeur n'a pas de type", {
  # Le piege : unique(na.omit(x)) est vide, donc de longueur 0 <= 2, et la
  # variable etait typee « binaire ».
  expect_equal(.hstat_reco_type(rep(NA, 30)), "indeterminable")
  expect_equal(.hstat_reco_type(rep(NA_character_, 30)), "indeterminable")
  expect_equal(.hstat_reco_type(rep(NA_real_, 30)), "indeterminable")
  expect_equal(.hstat_reco_type(logical(0)), "indeterminable")
  expect_equal(.hstat_reco_type(NULL), "indeterminable")
  # Le typage normal est intact
  expect_equal(.hstat_reco_type(rnorm(50)), "quantitative")
  expect_equal(.hstat_reco_type(c(0, 1, 1, 0)), "binaire")
  expect_equal(.hstat_reco_type(factor(c("a", "b", "c"))), "categorielle")
  expect_equal(.hstat_reco_type(c(TRUE, FALSE, NA)), "binaire")
})

test_that("aucune analyse n'est recommandee sur une variable vide", {
  d <- data.frame(vide = rep(NA, 40), g = rep(c("a", "b"), 20))
  reco <- hstat_reco_analyses(hstat_data_profile(d, "vide", "g"))
  expect_true(NROW(reco) >= 1)
  # Plus aucun test statistique propose
  expect_false(any(grepl("chi-deux|Chi-deux|Student|Mann-Whitney|ANOVA",
                         reco$Analyse)))
  # A la place, un constat bloquant qui nomme la variable et dit quoi faire
  expect_true(any(reco$Pertinence == "Bloquant"))
  bloc <- reco[reco$Pertinence == "Bloquant", ][1, ]
  expect_true(grepl("vide", bloc$Analyse, fixed = TRUE) ||
              grepl("Aucune analyse", bloc$Analyse, fixed = TRUE))
  expect_true(grepl("« vide »", bloc$Pourquoi, fixed = TRUE))
  expect_true(grepl("Nettoyage", bloc[["Si non remplies"]], fixed = TRUE))

  # Une variable vide parmi d'autres n'empeche pas de conseiller sur le reste
  d2 <- data.frame(vide = rep(NA, 40), x = rnorm(40), g = rep(c("a", "b"), 20))
  r2 <- hstat_reco_analyses(hstat_data_profile(d2, c("vide", "x"), "g"))
  expect_true(any(r2$Pertinence == "Bloquant"))
  expect_true(any(grepl("Student|Mann-Whitney|Welch", r2$Analyse)))
})

test_that("le profil compte les variables par type sans compter les vides", {
  d <- data.frame(vide = rep(NA, 20), x = rnorm(20), g = rep(c("a", "b"), 10))
  p <- hstat_data_profile(d, c("vide", "x"), "g")
  expect_equal(p$variables$vide$type, "indeterminable")
  expect_equal(p$variables$vide$n, 0L)
  expect_equal(p$n_quanti, 1L)     # seule `x` compte
  expect_equal(p$n_quali, 0L)      # `vide` n'est plus prise pour une binaire
  # Une variable vide n'a pas de test de normalite
  expect_null(p$variables$vide$normale)
})

test_that("le resume du rapport ne montre aucun nom de classe R en anglais", {
  d <- data.frame(vide = rep(NA, 5), x = 1:5, g = c("a","b","a","b","a"),
                  d = as.Date("2026-01-01") + 0:4,
                  b = c(TRUE, FALSE, TRUE, FALSE, TRUE),
                  stringsAsFactors = FALSE)
  r <- hstat_report_resume_donnees(d)
  expect_false(any(r$Type %in% c("logical", "integer", "character", "factor",
                                 "numeric", "Date")))
  expect_equal(r$Type[r$Variable == "vide"], "vide (aucune valeur)")
  expect_equal(r$Type[r$Variable == "b"], "binaire (vrai / faux)")
  expect_equal(r$Type[r$Variable == "d"], "date")
  expect_equal(r$Type[r$Variable == "x"], "numérique")
})


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
# reels manquaient (dont le workflow de CI, mod_report.R et hstat-session.js)
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
  # Le dictionnaire est INCORPORE : le bilingue doit fonctionner hors ligne.
  expect_true(grepl("window.HSTAT_I18N", ux, fixed = TRUE))
  expect_true(grepl("hstat_i18n_json", ux, fixed = TRUE))
  expect_true(grepl("hstatLangEn", ux, fixed = TRUE))
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
  "Prix": "$& remplace", "Total": "Total (translated)"
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
  cellule_longue: tLong.nodeValue, dollar: tDollar.nodeValue
};
ctx.window.hstatSetLangue("fr");
apres.retour_menu = tMenu.nodeValue;
apres.retour_cellule = tTd.nodeValue;
apres.retour_dollar = tDollar.nodeValue;
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
  # 9. Le reste de l'interface continue de se traduire normalement.
  expect_equal(avec$menu, "Loading")
  expect_equal(avec$cellule, "Oui")
  expect_equal(avec$dollar, "$& remplace")
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

test_that("le journal de reproductibilite n'est jamais traduit", {
  # `hstat_rlog_*` construit du CODE R. Traduire ses gabarits produirait un
  # script que R refuserait d'analyser, alors que le journal a precisement
  # pour promesse d'etre executable.
  root <- .hstat_repo_root()
  skip_if(is.na(root))
  src <- readLines(.hstat_module_path("mod_ai.R"), warn = FALSE,
                   encoding = "UTF-8")
  dans <- FALSE
  fautifs <- character(0)
  for (i in seq_along(src)) {
    if (grepl("^\\.?hstat_rlog_[a-z_]* <- function", src[i])) dans <- TRUE
    else if (dans && identical(src[i], "}")) dans <- FALSE
    if (dans && grepl("\\btrf\\(", src[i]))
      fautifs <- c(fautifs, paste0("mod_ai.R:", i))
  }
  expect_equal(fautifs, character(0),
               info = paste("trf() dans le journal R :", paste(fautifs, collapse = ", ")))
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
  for (nm in c("data", "cleanData", "filteredData", "aiContext", "aiHistory",
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
  # La reinitialisation reparcourt la meme liste
  expect_true(grepl("init <- hstat_valeurs_initiales()", src, fixed = TRUE))
  expect_true(grepl("for (nm in names(init)) values[[nm]] <- init[[nm]]",
                    src, fixed = TRUE))
  # Et vide en plus ce qu'un module aurait cree en cours de session
  expect_true(grepl("setdiff(names(shiny::reactiveValuesToList(values)), names(init))",
                    src, fixed = TRUE))
  # L'ancienne enumeration a la main a bien disparu
  expect_false(grepl("values$chiSqPGlobal     <- NULL", src, fixed = TRUE))
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
    paste0(ext('hstat_export_plot_handler\\(input, "([A-Za-z0-9_.]+)"'), "Dl"),
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
  aiguillages <- c("withSpinner", "plotlyOutput", "ggplotly", "layout", "config",
                   "renderPlotly", "colourInput", "pickerInput",
                   "radioGroupButtons", "updatePickerInput", "rank_list", "%>%")
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
  optionnels <- c(shinycssloaders = "withSpinner",
                  colourpicker    = "colourInput",
                  shinyWidgets    = "pickerInput",
                  shinyWidgets    = "radioGroupButtons",
                  shinyWidgets    = "updatePickerInput",
                  sortable        = "rank_list",
                  plotly          = "plotlyOutput",
                  plotly          = "renderPlotly",
                  plotly          = "ggplotly")
  fautifs <- character(0)
  for (f in .hstat_sources_app()) {
    l <- .hstat_code_lignes(f)
    for (i in seq_along(optionnels)) {
      motif <- paste0(names(optionnels)[i], "::", optionnels[[i]], "(")
      j <- grep(motif, l, fixed = TRUE)
      if (length(j))
        fautifs <- c(fautifs, sprintf("%s:%d %s", basename(f), j, motif))
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
  v <- shiny::reactiveValues(data = d, cleanData = d, filteredData = d,
                             aiHistory = list())

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
  for (n in c("chiSqGlobalResult", "chiSqPostHocTable", "chiSqPlotMultiple",
              "chiSqVarCatSelect", "downloadChiSqPHPlot"))
    expect_false(grepl(paste0("output$", n), src, fixed = TRUE), label = n)
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
  connus <- c("Observé", "Prédit", "Modalité", "Résidu", "Thème", "Fréquence",
              "Méthode", "Interprétation", "Métrique", "Unité")
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
                      grad_x_angle = 45, titre = "T", sous_titre = "S")))
    expect_s3_class(hstat_dl50_graphique(list(f), o), "ggplot")
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

test_that("le journal reconstitue le bioessai, et seulement quand c'est fidele", {
  ref <- .hstat_dl50_essai_ref()
  ctx <- function(f) list(module = "DL50 / CL50", title = "probit",
    meta = list(a = f$a, b = f$b, c = f$c, methode = f$methode,
                doses = f$essai$doses$dose, effectifs = f$essai$doses$n,
                morts = f$essai$doses$x,
                temoin_n = f$essai$n0, temoin_x = f$essai$x0))

  # LE SEUL CAS FIDELE : a mortalite naturelle nulle, le modele de Finney EST
  # un GLM binomial a lien probit. Le script est ecrit, et il doit RENDRE LES
  # MEMES CHIFFRES -- c'est la seule verification qui vaille, un script qui
  # s'execute sans rendre le bon resultat serait pire que pas de script.
  f0 <- hstat_dl50_ajuste(ref, "nulle")
  code <- hstat_rlog_code(ctx(f0))
  expect_true(length(code) > 0)
  env <- new.env()
  expect_silent(eval(parse(text = paste(code, collapse = "\n")), envir = env))
  m <- get("dl50_modele", envir = env)
  expect_equal(unname(stats::coef(m)), unname(c(f0$a, f0$b)), tolerance = 1e-5)
  # LE SCRIPT PORTE LA MEME INVERSE NORMALE QUE L'APPLICATION. `qnorm()` exact
  # rendrait des doses letales differentes des le quatrieme chiffre : le script
  # cesserait de refaire ce que HStat a calcule, ce qui est toute sa raison
  # d'etre. C'est pourquoi il emet la fonction de Hastings.
  ab <- stats::coef(m)
  qh <- get("dl50_qnorm", envir = env)
  dl <- hstat_dl50_doses_letales(f0, c(10, 50, 90))
  expect_equal(10^((qh(c(0.1, 0.5, 0.9)) - ab[1]) / ab[2]), rev(dl$Dose),
               tolerance = 1e-5, ignore_attr = TRUE)
  expect_equal(qh(0.9), .hstat_dl50_qnorm(0.9), tolerance = 1e-12)

  # LES DEUX AUTRES NE SONT PAS RECONSTITUABLES, et on l'ecrit plutot que
  # d'ecrire un glm() plausible : `glm()` ne sait pas ajuster c dans
  # p = c + (1 - c).F(a + b.log d). Un script qui differerait en silence de ce
  # que l'application a calcule serait pire que pas de script.
  for (meth in c("em", "abbott")) {
    cd <- hstat_rlog_code(ctx(hstat_dl50_ajuste(ref, meth)))
    expect_true(any(grepl("NON RECONSTITU", cd)))
    # Aucune ligne EXECUTABLE : le commentaire, lui, nomme `glm()` -- il
    # explique justement pourquoi on ne l'ecrit pas. Chercher la chaine sans
    # ecarter les commentaires ferait echouer le test sur son propre texte.
    expect_equal(cd[!grepl("^\\s*#", cd)], character(0))
    # Les parametres obtenus figurent quand meme : sans eux le commentaire ne
    # dit rien d'utile.
    expect_true(any(grepl("a = 2.19", cd, fixed = TRUE)))
  }

  # Sans les doses, rien : le journal ne devine pas.
  c2 <- ctx(f0); c2$meta$doses <- NULL
  expect_null(hstat_rlog_code(c2))

  # LES NOMBRES SONT ECRITS EN CODE R, PAS EN AFFICHAGE. Un separateur decimal
  # francais rendrait le script inanalysable, et c'est exactement ce que
  # `format()` produirait sous une locale francaise.
  expect_equal(.hstat_rlog_num(0.00063), "0.00063")
  expect_equal(.hstat_rlog_vec_num(c(1, 2.5)), "c(1, 2.5)")
  expect_equal(.hstat_rlog_num(NA), "NA")
  expect_false(any(grepl(",", strsplit(.hstat_rlog_num(1234.5), "")[[1]], fixed = TRUE)))

  # Et le script COMPLET de session s'analyse.
  expect_silent(parse(text = hstat_rlog_script(list(ctx(f0)))))
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
  expect_true(grepl('hstat_ai_capture(values, "DL50 / CL50"', mod, fixed = TRUE))
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
  expect_true(grepl('hstat_ai_capture(values, "Doses & dilutions"', mod, fixed = TRUE))
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
