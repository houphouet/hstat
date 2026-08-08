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

# -- Charger uniquement les fonctions utilitaires (sans demarrer l'app) -------
# On source Utils.R dans un environnement isole. install_and_load() peut
# tenter de charger des packages ; on neutralise cet effet pour les tests.
local({
  # Resolution robuste du chemin de Utils.R : que les tests soient lances depuis
  # la racine de l'application (sys.source("Utils.R")) ou depuis tests/
  # (testthat::test_dir, qui se place dans le dossier du fichier de test).
  # Candidats couvrant toutes les facons de lancer les tests : depuis le
  # dossier de l'app, depuis tests/, depuis testthat/ (test_dir), ou avec le
  # package installe (R CMD check).
  candidates <- c(
    "Utils.R", file.path("..", "Utils.R"),
    file.path("..", "..", "inst", "app", "Utils.R"),   # depuis tests/testthat/
    file.path("..", "inst", "app", "Utils.R"),          # depuis tests/
    file.path("inst", "app", "Utils.R"),                # depuis la racine du package
    tryCatch(system.file("app", "Utils.R", package = "HStat"), error = function(e) "")
  )
  utils_path <- candidates[file.exists(candidates)][1]
  if (is.na(utils_path))
    stop("Impossible de localiser Utils.R depuis ", getwd())
  # Empeche install_and_load de bloquer si un package manque dans l'env de test
  e <- new.env()
  assign("install_and_load", function(...) invisible(NULL), envir = e)
  suppressWarnings(suppressMessages(
    sys.source(utils_path, envir = e, keep.source = FALSE)))
  # Les fonctions de calcul qualitatives (hstat_q_*) vivent dans
  # mod_qualitative.R, au meme endroit que Utils.R : le charger aussi, sinon
  # tous les tests qualitatifs echouent avec "could not find function".
  qual_path <- file.path(dirname(utils_path), "mod_qualitative.R")
  if (file.exists(qual_path))
    suppressWarnings(suppressMessages(
      sys.source(qual_path, envir = e, keep.source = FALSE)))
  # Le moteur d'inference et l'aide a la decision (hstat_ai_*, hstat_reco_*,
  # hstat_data_profile) vivent dans mod_ai.R : partages par tous les modules.
  ai_path <- file.path(dirname(utils_path), "mod_ai.R")
  if (file.exists(ai_path))
    suppressWarnings(suppressMessages(
      sys.source(ai_path, envir = e, keep.source = FALSE)))
  # Le generateur de rapport (hstat_report_*) vit dans mod_report.R et
  # s'appuie sur les helpers de mod_ai.R : le charger apres lui.
  rep_path <- file.path(dirname(utils_path), "mod_report.R")
  if (file.exists(rep_path))
    suppressWarnings(suppressMessages(
      sys.source(rep_path, envir = e, keep.source = FALSE)))
  # Idem pour l'atelier de codage qualitatif (hstat_code_*, hstat_seg_*),
  # qui vit dans mod_coding.R.
  cod_path <- file.path(dirname(utils_path), "mod_coding.R")
  if (file.exists(cod_path))
    suppressWarnings(suppressMessages(
      sys.source(cod_path, envir = e, keep.source = FALSE)))
  # Exporter TOUTES les fonctions (y compris cachees, ex. .hstat_sql_stat_exprs)
  for (nm in ls(e, all.names = TRUE))
    assign(nm, get(nm, envir = e), envir = globalenv())
})

# -- Racine du depot (pour les tests portant sur app.R et R/) ----------------
# Renvoie NA quand les tests tournent depuis un paquet installe, ou app.R et le
# dossier R/ n'existent plus : les tests concernes s'y skippent d'eux-memes.
.hstat_repo_root <- function() {
  cands <- c(".", "..", file.path("..", ".."), file.path("..", "..", ".."))
  hit <- cands[file.exists(file.path(cands, "DESCRIPTION")) &
               dir.exists(file.path(cands, "inst", "app"))]
  if (length(hit)) normalizePath(hit[1]) else NA_character_
}

# -- Charger le module d'analyses qualitatives (fonctions de calcul) ---------
local({
  q_path <- "mod_qualitative.R"
  if (!file.exists(q_path)) q_path <- file.path("..", "mod_qualitative.R")
  if (file.exists(q_path)) {
    eq <- new.env()
    suppressWarnings(suppressMessages(sys.source(q_path, envir = eq, keep.source = FALSE)))
    for (nm in ls(eq, all.names = TRUE))
      assign(nm, get(nm, envir = eq), envir = globalenv())
  }
})


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

test_that("hstat_model_interpretation produit un texte substantiel", {
  m <- hstat_metrics_reg(1:50, (1:50) + rnorm(50, 0, 2))
  txt <- hstat_model_interpretation("regression", m, "test", 100, 50)
  expect_true(is.character(txt) && nchar(txt) > 80)
  expect_match(txt, "generalisation")
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

test_that("la taille des labels est bornee a 12-24 pt et convertie pour ggplot2", {
  expect_equal(HSTAT_LBL_PT_MIN, 12)
  expect_equal(HSTAT_LBL_PT_MAX, 24)
  # Bornage des saisies hors domaine, absentes ou invalides.
  expect_equal(hstat_lbl_pt(3), 12)
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
  expect_true(grepl("sans reseau", st$message, fixed = TRUE))
})

test_that("le moteur par defaut est local, jamais l'API payante", {
  # Le premier choix propose a l'utilisateur et le defaut de hstat_ai_call()
  # doivent rester le moteur local : gratuit et hors ligne.
  expect_equal(unname(HSTAT_AI_ENGINES[1]), "local")
  expect_equal(formals(hstat_ai_call)$engine, "local")
  expect_equal(formals(hstat_ai_status)$engine, "local")
  expect_true("claude" %in% HSTAT_AI_ENGINES)
})

test_that("API Claude : degradation propre en l'absence de cle", {
  old <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA)
  Sys.unsetenv("ANTHROPIC_API_KEY")
  on.exit(if (!is.na(old)) Sys.setenv(ANTHROPIC_API_KEY = old), add = TRUE)

  expect_equal(hstat_ai_key(NULL), "")
  expect_equal(hstat_ai_key("  sk-test  "), "sk-test")
  expect_false(hstat_ai_available(NULL))

  st <- hstat_ai_status("claude")
  expect_false(st$ok)
  expect_true(grepl("cle d'API", st$message, fixed = TRUE))
  # Le message doit orienter vers les moteurs gratuits
  expect_true(grepl("gratuits et locaux", st$message, fixed = TRUE))

  # Aucun appel reseau ne doit partir sans cle
  r <- hstat_ai_call("bonjour", engine = "claude", api_key = NULL)
  expect_false(r$ok)
  expect_true(nzchar(r$error))

  Sys.setenv(ANTHROPIC_API_KEY = "sk-depuis-l-environnement")
  expect_equal(hstat_ai_key(NULL), "sk-depuis-l-environnement")
  Sys.unsetenv("ANTHROPIC_API_KEY")
})

test_that("adresses par defaut des serveurs d'inference locaux", {
  # 127.0.0.1 et non localhost : on veut que ce soit visiblement la machine
  # de l'utilisateur, et rien d'autre.
  expect_true(grepl("^http://127\\.0\\.0\\.1:11434$", hstat_ai_url("ollama")))
  expect_true(grepl("^http://127\\.0\\.0\\.1:8080$", hstat_ai_url("openai")))
  expect_equal(hstat_ai_url("ollama", "http://192.168.1.5:11434/"),
               "http://192.168.1.5:11434")
  expect_equal(hstat_ai_url("ollama", "   "), "http://127.0.0.1:11434")
})

test_that("serveur local injoignable : message actionnable, jamais d'erreur", {
  skip_if_not(requireNamespace("httr", quietly = TRUE))
  # Port volontairement ferme
  st <- hstat_ai_status("local", "ollama", "http://127.0.0.1:9")
  expect_false(st$ok)
  expect_true(grepl("Ollama", st$message, fixed = TRUE))
  expect_equal(hstat_ai_ollama_models("http://127.0.0.1:9", timeout = 2), character(0))

  r <- hstat_ai_call("bonjour", engine = "local", url = "http://127.0.0.1:9",
                     model = "inexistant", timeout = 3)
  expect_false(r$ok)
  expect_true(nzchar(r$error))
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

test_that("mod_coding.R est source par HStat.R avant mod_qualitative.R", {
  root <- .hstat_repo_root()
  h <- readLines(file.path(root, "inst", "app", "HStat.R"), warn = FALSE)
  i_cod <- grep('source\\("mod_coding\\.R"', h)
  i_qua <- grep('source\\("mod_qualitative\\.R"', h)
  expect_length(i_cod, 1L)
  expect_length(i_qua, 1L)
  # mod_qualitative_ui() appelle mod_coding_ui() : l'ordre doit tenir
  expect_true(i_cod < i_qua)
  expect_true(file.exists(file.path(root, "inst", "app", "mod_coding.R")))
})

test_that("le corps des requetes locales a la forme attendue par les serveurs", {
  # Ces champs sont le contrat avec Ollama et avec les serveurs compatibles
  # OpenAI : un renommage silencieux casserait l'assistant sans erreur visible.
  b <- .hstat_ai_body_ollama("code ce corpus", system = "tu reponds en JSON",
                             model = "qwen2.5", json = TRUE)
  expect_equal(b$model, "qwen2.5")
  expect_false(b$stream)                    # sinon la reponse arrive par morceaux
  expect_equal(b$format, "json")
  expect_equal(b$options$temperature, 0.2)  # thematisation stable, pas creative
  expect_equal(vapply(b$messages, function(m) m$role, character(1)),
               c("system", "user"))
  expect_equal(b$messages[[2]]$content, "code ce corpus")

  # Sans consigne systeme, un seul message
  b0 <- .hstat_ai_body_ollama("x", system = NULL, model = "m")
  expect_equal(vapply(b0$messages, function(m) m$role, character(1)), "user")
  expect_null(.hstat_ai_body_ollama("x", model = "m", json = FALSE)$format)

  o <- .hstat_ai_body_openai("code ce corpus", system = "sys", model = "local-model",
                             max_tokens = 2048L, json = TRUE)
  expect_equal(o$model, "local-model")
  expect_false(o$stream)
  expect_equal(o$max_tokens, 2048L)
  expect_equal(o$response_format$type, "json_object")
  # Le rejeu apres un refus du serveur passe par la meme fonction sans le champ
  expect_null(.hstat_ai_body_openai("x", model = "m", json = FALSE)$response_format)

  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  # `messages` doit rester un TABLEAU JSON, meme avec un seul message
  j <- jsonlite::toJSON(.hstat_ai_body_ollama("x", model = "m"), auto_unbox = TRUE)
  expect_true(grepl('"messages":[{', j, fixed = TRUE))
  expect_true(grepl('"stream":false', j, fixed = TRUE))
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
  expect_true(grepl("t de Student|Welch", r$Analyse[r$Pertinence == "Recommandee"][1]))

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
    hstat_reco_analyses(p)$Pertinence == "Recommandee"][1]

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
  expect_equal(rp$Analyse[rp$Pertinence == "Recommandee"][1], "Kruskal-Wallis")
  expect_true("Test exact / permutation" %in% rp$Analyse)

  # Deux qualitatives -> chi-deux, et la taille d'effet a enchainer
  dq <- data.frame(a = sample(c("x", "y"), 120, TRUE),
                   b = sample(c("u", "v", "w"), 120, TRUE), stringsAsFactors = FALSE)
  rq <- hstat_reco_analyses(hstat_data_profile(dq, c("a", "b"), "a"))
  expect_match(rq$Analyse[rq$Pertinence == "Recommandee"][1], "chi-deux")
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

  v_no <- hstat_reco_verdict(r, "ANOVA a un facteur")
  expect_false(v_no$coherent)
  expect_true(grepl("Kruskal-Wallis", v_no$message, fixed = TRUE))
  # Le ton compte autant que le fond : on informe, on ne condamne pas.
  expect_true(grepl("ne disqualifie pas", v_no$message, fixed = TRUE))
  expect_true(grepl("A vous de trancher", v_no$message, fixed = TRUE))
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
  expect_true(grepl("Analyses appelees par vos donnees", out, fixed = TRUE))
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
  expect_true(grepl("appartient a l'utilisateur", pr, fixed = TRUE))
  expect_true(grepl("## Analyse recommandee pour la suite", pr, fixed = TRUE))
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

test_that("mod_ai.R est source avant les modules qui s'en servent", {
  root <- .hstat_repo_root()
  h <- readLines(file.path(root, "inst", "app", "HStat.R"), warn = FALSE)
  i_ai <- grep('source\\("mod_ai\\.R"', h)
  expect_length(i_ai, 1L)
  # Le moteur est partage : il doit preceder tous les modules d'analyse
  for (m in c("mod_coding.R", "mod_qualitative.R", "mod_ml.R", "mod_timeseries.R")) {
    j <- grep(sprintf('source\\("%s"', gsub("\\.", "\\\\.", m)), h)
    expect_length(j, 1L)
    expect_true(i_ai < j, info = paste("mod_ai.R doit preceder", m))
  }
  # L'onglet est declare dans l'interface et branche cote serveur
  ux <- readLines(file.path(root, "inst", "app", "UX.R"), warn = FALSE)
  expect_true(any(grepl('mod_ai_ui\\("aidecision"\\)', ux)))
  expect_true(any(grepl('tabName = "aidecision"', ux)))
  srv <- readLines(file.path(root, "inst", "app", "app_server.R"), warn = FALSE)
  expect_true(any(grepl('mod_ai_server\\("aidecision", values\\)', srv)))
  expect_true(any(grepl("aiContext", srv)))
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
  expect_true(grepl("etape preliminaire", v$message, fixed = TRUE))
  expect_true(grepl("pas un test", v$message, fixed = TRUE))
  expect_true(grepl("A vous de decider", v$message, fixed = TRUE))

  # Un module de tests reste evalue normalement
  vt <- hstat_reco_verdict(r, "ANOVA a un facteur", "Tests statistiques")
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

test_that("le guidage de fin d'analyse annonce la recommandation", {
  set.seed(12)
  d <- data.frame(y = c(stats::rnorm(40), stats::rnorm(40, 2)),
                  g = rep(c("A", "B"), each = 40), stringsAsFactors = FALSE)
  r <- hstat_reco_analyses(hstat_data_profile(d, "y", "g"))
  ctx <- list(title = "Statistiques descriptives", module = "Analyses descriptives",
              tables = list(), text = NULL, meta = list(), time = Sys.time())

  msg <- hstat_ai_hint_text(ctx, r)
  expect_true(grepl("Statistiques descriptives", msg, fixed = TRUE))
  expect_true(grepl("Student|Welch", msg))
  # Sans recommandation calculable, pas de message plutot qu'un message creux
  expect_null(hstat_ai_hint_text(ctx, NULL))
  expect_null(hstat_ai_hint_text(NULL, r))

  ui <- hstat_ai_hint_ui(ctx, r)
  h <- paste(as.character(ui), collapse = "")
  expect_true(grepl("Aide a la decision", h, fixed = TRUE))
  expect_true(grepl("shiny-tab-aidecision", h, fixed = TRUE))   # le lien bascule d'onglet
  # Le rappel de responsabilite accompagne chaque recommandation
  expect_true(grepl("l'assistance eclaire, elle ne decide pas", h, fixed = TRUE))
  expect_null(hstat_ai_hint_ui(NULL, r))
})

test_that("hstat_ai_with_hint greffe l'emplacement sans casser l'onglet", {
  tab <- shinydashboard::tabItem(tabName = "essai", shiny::h3("contenu"))
  n0 <- length(tab$children)
  out <- hstat_ai_with_hint(tab, "aihint_tests")
  expect_equal(length(out$children), n0 + 1L)
  h <- paste(as.character(out), collapse = "")
  expect_true(grepl("contenu", h, fixed = TRUE))       # le contenu d'origine survit
  expect_true(grepl("aihint_tests", h, fixed = TRUE))
  expect_true(grepl("shiny-tab-essai", h, fixed = TRUE))

  # Un identifiant inconnu doit echouer bruyamment : un bandeau muet passerait
  # inapercu jusqu'a ce qu'un utilisateur le signale.
  expect_error(hstat_ai_hint_slot("aihint_inexistant"))
  # Une entree qui n'est pas un tabItem est renvoyee telle quelle
  expect_identical(hstat_ai_with_hint("pas un tag", "aihint_tests"), "pas un tag")
})

test_that("chaque onglet d'analyse porte un emplacement de guidage", {
  root <- .hstat_repo_root()
  ux <- readLines(file.path(root, "inst", "app", "UX.R"), warn = FALSE)
  used <- unique(unlist(regmatches(ux, gregexpr("aihint_[a-z]+", ux))))
  # Declares et poses doivent coincider exactement : un identifiant declare mais
  # jamais pose ne s'afficherait nulle part, l'inverse planterait au demarrage.
  expect_setequal(used, HSTAT_AI_HINT_IDS)
  expect_equal(length(HSTAT_AI_HINT_IDS), 12L)

  # Tous les onglets d'analyse du menu doivent etre couverts
  # Suffixes attendus : ils suivent le nom du MODULE (mod_viz -> viz), pas
  # toujours celui de l'onglet (tabName = "visualization").
  onglets <- c("descriptive", "viz", "correlation", "tests", "multiple",
               "multivariate", "qualitative", "timeseries", "ml", "dl",
               "design", "threshold")
  expect_setequal(sub("^aihint_", "", HSTAT_AI_HINT_IDS), onglets)

  # ... et le serveur doit rendre chacun d'eux
  srv <- readLines(file.path(root, "inst", "app", "app_server.R"), warn = FALSE)
  expect_true(any(grepl("HSTAT_AI_HINT_IDS", srv, fixed = TRUE)))
})

test_that("toutes les familles d'analyse deposent un contexte", {
  root <- file.path(.hstat_repo_root(), "inst", "app")
  src <- unlist(lapply(list.files(root, pattern = "\\.R$", full.names = TRUE),
                       readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]
  pose <- unique(unlist(regmatches(
    src, gregexpr('hstat_ai_capture\\(values, "[^"]+"', src))))
  pose <- gsub('.*"([^"]+)"$', "\\1", pose)
  attendu <- c("Tests statistiques", "Comparaisons multiples", "Analyses multivariees",
               "Analyses descriptives", "Machine Learning", "Analyses qualitatives",
               "Series temporelles", "Correlations", "Deep Learning", "Plan & Puissance")
  for (m in attendu)
    expect_true(m %in% pose, info = paste("aucune capture pour :", m))
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
  root <- file.path(.hstat_repo_root(), "inst", "app")
  motif <- "if\\s*\\([^)]*\\$p\\.value\\s*[<>]"
  trouve <- character(0)
  for (f in list.files(root, pattern = "\\.R$", full.names = TRUE)) {
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
  root <- file.path(.hstat_repo_root(), "inst", "app")
  fautes <- character(0)
  for (f in list.files(root, pattern = "\\.R$", full.names = TRUE)) {
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
  expect_true(any(grepl("correlation", dq$Constat)))
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
  expect_true(grepl("aucun probleme", dq$Constat[1]))
  expect_true(grepl("Aucun probleme", hstat_data_quality_resume(dq)))

  expect_null(hstat_data_quality(NULL))
  expect_null(hstat_data_quality(data.frame()))
  expect_null(hstat_data_quality_resume(NULL))
})

test_that("le resume compte correctement les gravites", {
  dq <- rbind(
    .hstat_q_row("a", "x", "bloquant", "s"),
    .hstat_q_row("b", "y", "important", "s"),
    .hstat_q_row("c", "z", "important", "s"),
    .hstat_q_row("d", "w", "a surveiller", "s"))
  r <- hstat_data_quality_resume(dq)
  expect_true(grepl("4 constat", r, fixed = TRUE))
  expect_true(grepl("1 bloquant", r, fixed = TRUE))
  expect_true(grepl("2 important", r, fixed = TRUE))
  expect_true(grepl("1 a surveiller", r, fixed = TRUE))
})

test_that("TOUS les modules d'analyse deposent un contexte pour l'IA", {
  root <- file.path(.hstat_repo_root(), "inst", "app")
  src <- unlist(lapply(list.files(root, pattern = "\\.R$", full.names = TRUE),
                       readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]
  pose <- gsub('.*"([^"]+)"$', "\\1", unique(unlist(regmatches(
    src, gregexpr('hstat_ai_capture\\(values, "[^"]+"', src)))))

  # La liste complete : aucun module ne doit rester muet.
  attendu <- c("Exploration", "Nettoyage", "Filtrage", "Analyses descriptives",
               "Visualisation", "Correlations", "Tests statistiques",
               "Comparaisons multiples", "Analyses multivariees",
               "Analyses qualitatives", "Series temporelles", "Machine Learning",
               "Deep Learning", "Plan & Puissance", "Seuils d'efficacite")
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

  expect_match(cas("Correlations", "Tests de correlation", c("a", "b"))[2],
               "cor.test\\(donnees\\$a, donnees\\$b\\)", perl = TRUE)
  expect_match(cas("Analyses multivariees", "Analyse en Composantes Principales (ACP)",
                   c("a", "b"))[1], "FactoMineR::PCA", fixed = TRUE)
  expect_match(cas("Analyses multivariees", "Classification k-means", c("a", "b")),
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
    list(module = "Correlations", title = "Correlations",
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
  expect_true(grepl("Aucune analyse enregistree", vide, fixed = TRUE))
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
  for (titre in c("## Donnees analysees", "## Diagnostic de qualite",
                  "## Analyses menees", "## Interpretation",
                  "## Analyses appelees", "## Annexe"))
    expect_true(grepl(titre, md, fixed = TRUE), info = titre)
  # Le contenu des analyses y est, pas seulement leur titre
  expect_true(grepl("Test t de Student", md, fixed = TRUE))
  expect_true(grepl("0.0231", md, fixed = TRUE))

  # Une section non demandee ne doit pas apparaitre. Le piege : passer les
  # LIBELLES du vecteur au lieu de ses valeurs vide le rapport en silence.
  md2 <- hstat_report_markdown(h, sections = c("analyses"),
                               qualite = data.frame(V = 1),
                               script = "x <- 1")
  expect_true(grepl("## Analyses menees", md2, fixed = TRUE))
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
  expect_equal(r$Type, c("numerique", "categorielle"))
  expect_equal(r$Renseignees, c(2L, 3L))
  expect_equal(r$Manquantes, c(1L, 0L))
  expect_true(grepl("60", r[["Modalites / etendue"]][1]))
  expect_true(grepl("2 modalite", r[["Modalites / etendue"]][2]))
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
    list("missing value where TRUE/FALSE needed",      "degenerees"),
    list("0 (non-NA) cases",                           "Aucune observation"),
    list("NA/NaN/Inf in foreign function call (arg 1)", "manquantes ou infinies"),
    list("undefined columns selected",                 "absente du jeu de donnees"),
    list("there is no package called 'poLCA'",         "pas installe"),
    list("contrasts can be applied only to factors with 2 or more levels",
                                                       "une seule modalite"),
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
  gestes <- paste("Choisissez|Verifiez|Retirez|Convertissez|Installez|Traitez",
                  "|Simplifiez|Croisez|Augmentez|Agrandissez|Reduisez|Utilisez",
                  "|reselectionnez|Signalez|Installez", sep = "")
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
  root <- file.path(.hstat_repo_root(), "inst", "app")
  skip_if(is.na(.hstat_repo_root()))
  fautes <- character(0)
  for (f in list.files(root, pattern = "\\.R$", full.names = TRUE)) {
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
  src <- paste(readLines(file.path(root, "inst", "app", "mod_ai.R"),
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
  expect_equal(r$Type[r$Variable == "x"], "numerique")
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
  src <- paste(readLines(file.path(root, "inst", "app", "Utils.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Habiller chaque appel ne marcherait pas : leurs corps comportent des
  # `return()` qui sauteraient le nettoyage. C'est `exprToFunction` qui rend
  # l'interception correcte.
  expect_true(grepl("renderPlotly <- function", src, fixed = TRUE))
  expect_true(grepl("exprToFunction", src, fixed = TRUE))
  expect_true(grepl("hstat_plotly_clean(fn())", src, fixed = TRUE))
})
