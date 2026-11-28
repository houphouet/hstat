# MODULE : SELECTION GENETIQUE DES VARIETES ET DES RACES
# Composantes de variance et heritabilites, aptitudes a la combinaison (AGC,
# ASC, ratio de Baker) d'un diallele ou d'un plan lignees x testeurs, interaction genotype x environnement (AMMI, ASV, YSI, WAAS,
# GGE biplot, stabilite), selection multicaractere (MGIDI, MTSI, GYT) et
# covariables environnementales. Les calculs vivent ici, au premier niveau,
# pour etre testables ; le serveur ne fait que choisir et afficher.

# Le sens de l'amelioration d'un caractere : un rendement s'augmente, une
# duree de cycle ou des degats de ravageurs se diminuent.
HSTAT_GEN_DIRECTIONS <- c("À augmenter" = "augmenter", "À diminuer" = "diminuer")

# Un nom de colonne qui designe d'ordinaire un caractere a diminuer. La
# proposition se corrige a l'ecran : elle ne se devine pas, elle se propose.
HSTAT_GEN_MOTIF_DIMINUER <- paste0(
  "jour|days|dap|floraison|flower|maturit|cycle|precoc|dégât|degat|damage|",
  "attaque|incidence|sévérité|severite|severity|infest|ravageur|pest|maladie|",
  "disease|verse|lodging|pourri|aflatox|taux_de_perte|perte")

# Les plafonds : au-dela, les tableaux ne se lisent plus et le processus
# partage travaille pour rien.
HSTAT_GEN_GENOTYPES_MAX <- 500L
HSTAT_GEN_CARACTERES_MAX <- 30L
HSTAT_GEN_PARENTS_MAX <- 40L

# Les grilles de lecture, avec leurs auteurs.
HSTAT_GEN_GRILLES <- list(
  H2  = list(bornes = c(0.3, 0.6), auteur = "Robinson et al. (1949) ; Johnson et al. (1955)"),
  GAM = list(bornes = c(10, 20),   auteur = "Johnson et al. (1955)"),
  CV  = list(bornes = c(10, 20),   auteur = "Sivasubramanian & Madhavamenon (1973)"),
  PRECISION = list(bornes = c(0.5, 0.7, 0.9), auteur = "Resende & Duarte (2007)"))

.hstat_gen_libelles_h2 <- function()
  c(tr("Faible (H² < 0,3)"), tr("Moyenne (H² de 0,3 à 0,6)"), tr("Élevée (H² > 0,6)"))

# L'intensite de selection standardisee k pour une proportion selectionnee p.
hstat_gen_intensite <- function(p) {
  p <- suppressWarnings(as.numeric(p)[1])
  if (!is.finite(p) || p <= 0 || p >= 1) return(NA_real_)
  stats::dnorm(stats::qnorm(1 - p)) / p
}

# Le sens propose pour chaque caractere, d'apres son nom.
hstat_gen_direction_proposee <- function(noms) {
  n <- hstat_sans_accents(tolower(as.character(noms)))
  ifelse(grepl(hstat_sans_accents(HSTAT_GEN_MOTIF_DIMINUER), n), "diminuer", "augmenter")
}

.hstat_gen_dir <- function(directions, traits) {
  d <- stats::setNames(rep("augmenter", length(traits)), traits)
  if (length(directions)) {
    k <- intersect(names(directions), traits)
    d[k] <- ifelse(directions[k] == "diminuer", "diminuer", "augmenter")
  }
  d
}

# Le meilleur d'un vecteur selon le sens du caractere.
.hstat_gen_signe <- function(direction) if (identical(direction, "diminuer")) -1 else 1

# Le sens d'un effet : un test incalculable ne vaut pas « non significatif ».
.hstat_gen_sens <- function(p, effet, alpha)
  ifelse(!is.finite(p), tr("Non calculable"),
         ifelse(p >= alpha, tr("Non significatif"),
                ifelse(effet > 0, tr("Favorable"), tr("Défavorable"))))

.hstat_gen_classe <- function(x, bornes, libelles) {
  vapply(x, function(v) {
    if (!is.finite(v)) return(NA_character_)
    libelles[findInterval(v, bornes) + 1L]
  }, character(1))
}

# PREPARATION DES DONNEES
# Un environnement est le croisement d'une localite et d'une annee : deux
# annees dans un meme lieu sont deux environnements.
hstat_gen_preparer <- function(df, var_gen, traits, var_loc = NULL,
                               var_annee = NULL, var_rep = NULL,
                               var_groupe = NULL, groupe = NULL,
                               var_type = NULL, var_p1 = NULL, var_p2 = NULL,
                               covariables = NULL) {
  refus <- function(m) list(ok = FALSE, motif = m)
  if (is.null(df) || !is.data.frame(df) || !nrow(df))
    return(refus(tr("Aucune donnée : chargez d'abord un fichier.")))
  vide <- function(x) is.null(x) || !length(x) || all(!nzchar(x))
  if (vide(var_gen) || !var_gen %in% names(df))
    return(refus(tr("Choisissez la colonne des génotypes (variétés, lignées, hybrides ou races).")))
  traits <- intersect(as.character(traits), names(df))
  traits <- setdiff(traits, c(var_gen, var_loc, var_annee, var_rep, var_groupe,
                              var_type, var_p1, var_p2))
  if (!length(traits))
    return(refus(tr("Choisissez au moins un caractère mesuré.")))
  notes <- character(0)
  if (length(traits) > HSTAT_GEN_CARACTERES_MAX) {
    notes <- c(notes, trf("%d caractères demandés : seuls les %d premiers sont analysés.",
                          length(traits), HSTAT_GEN_CARACTERES_MAX))
    traits <- traits[seq_len(HSTAT_GEN_CARACTERES_MAX)]
  }
  if (!vide(var_groupe) && var_groupe %in% names(df) && !vide(groupe)) {
    df <- df[as.character(df[[var_groupe]]) %in% as.character(groupe), , drop = FALSE]
    if (!nrow(df)) return(refus(tr("Aucune ligne pour l'espèce ou la race choisie.")))
  }
  g <- trimws(as.character(df[[var_gen]]))
  ok <- !is.na(g) & nzchar(g)
  if (!all(ok)) notes <- c(notes, trf("%d ligne(s) sans génotype écartée(s).", sum(!ok)))
  df <- df[ok, , drop = FALSE]; g <- g[ok]
  col <- function(v) if (!vide(v) && v %in% names(df)) trimws(as.character(df[[v]])) else NULL
  loc <- col(var_loc); an <- col(var_annee)
  env <- if (!is.null(loc) && !is.null(an)) paste(loc, an, sep = " - ")
         else if (!is.null(loc)) loc else if (!is.null(an)) an else rep("E1", length(g))
  env[is.na(env) | !nzchar(env)] <- "E?"
  rep_ <- col(var_rep)
  d <- data.frame(G = g, E = env, stringsAsFactors = FALSE)
  d$R <- if (is.null(rep_)) NA_character_ else rep_
  d$Type <- col(var_type) %||% NA_character_
  d$P1 <- col(var_p1) %||% NA_character_
  d$P2 <- col(var_p2) %||% NA_character_
  illis <- character(0)
  noms <- stats::setNames(traits, traits)
  for (t in traits) {
    v <- hstat_as_numeric_fr(df[[t]])
    if (is.null(v)) { illis <- c(illis, t); next }
    nom <- if (t %in% names(d) || startsWith(t, ".cov_"))
      make.unique(c(names(d), paste0(t, " (caractère)")), sep = " ")[ncol(d) + 1L] else t
    noms[[t]] <- nom
    d[[nom]] <- v
  }
  if (length(illis))
    notes <- c(notes, trf("Caractère(s) non numérique(s) écarté(s) : %s.",
                          paste(illis, collapse = ", ")))
  noms <- noms[setdiff(traits, illis)]
  renommes <- noms[names(noms) != noms]
  if (length(renommes))
    notes <- c(notes, trf("Caractère(s) renommé(s), le nom étant réservé à une colonne interne : %s.",
                          paste(sprintf("%s → %s", names(renommes), renommes), collapse = ", ")))
  traits <- unname(noms)
  if (!length(traits)) return(refus(tr("Aucun caractère numérique lisible.")))
  covs <- intersect(as.character(covariables), names(df))
  covs <- setdiff(covs, c(var_gen, var_loc, var_annee, var_rep, names(noms)))
  for (k in covs) {
    v <- df[[k]]
    num <- if (is.numeric(v)) v else hstat_as_numeric_fr(v)
    d[[paste0(".cov_", k)]] <- if (!is.null(num)) num else trimws(as.character(v))
  }
  d$G <- factor(d$G, levels = unique(d$G))
  d$E <- factor(d$E, levels = unique(d$E))
  if (nlevels(d$G) > HSTAT_GEN_GENOTYPES_MAX)
    return(refus(trf("%d génotypes distincts : au-delà de %d, la colonne choisie n'est sans doute pas celle des génotypes.",
                     nlevels(d$G), HSTAT_GEN_GENOTYPES_MAX)))
  if (nlevels(d$G) < 2) return(refus(tr("Il faut au moins deux génotypes à comparer.")))
  list(ok = TRUE, data = d, traits = traits, covariables = covs,
       n_gen = nlevels(d$G), n_env = nlevels(d$E),
       a_rep = !all(is.na(d$R)), notes = notes, noms = noms)
}

# ANALYSE DE VARIANCE ET COMPOSANTES DE VARIANCE
# Modele aleatoire pour les genotypes : c'est ce qui donne un sens a
# l'heritabilite. Les environnements, comme les repetitions, sont des
# facteurs ; un seul environnement donne le modele d'un essai en blocs.
hstat_gen_anova <- function(d, trait, intensite = 0.05) {
  y <- d[[trait]]
  ok <- is.finite(y)
  dd <- droplevels(d[ok, , drop = FALSE]); dd$y <- y[ok]
  g <- nlevels(dd$G); e <- nlevels(dd$E)
  if (g < 2) return(list(ok = FALSE, motif = tr("Moins de deux génotypes mesurés.")))
  a_rep <- !all(is.na(dd$R))
  dd$R <- factor(ifelse(is.na(dd$R), "1", dd$R))
  cellules <- table(dd$G, dd$E)
  r <- { v <- cellules[cellules > 0]; length(v) / sum(1 / v) }
  plusieurs <- e >= 2
  formule <- if (plusieurs && r > 1) y ~ E + E:R + G + G:E
             else if (plusieurs) y ~ E + G
             else if (a_rep && nlevels(dd$R) > 1) y ~ R + G
             else y ~ G
  fit <- tryCatch(stats::aov(formule, data = dd), error = function(err) NULL)
  if (is.null(fit)) return(list(ok = FALSE, motif = tr("Analyse de variance impossible sur ce caractère.")))
  tab <- summary(fit)[[1]]
  # L'ordre des facteurs d'une interaction suit la formule (« E:G ») : on le
  # ramene a une ecriture unique avant de lire la table.
  noms <- vapply(strsplit(trimws(rownames(tab)), ":", fixed = TRUE), function(v)
    paste(sort(v, method = "radix", decreasing = TRUE), collapse = ":"), "")
  noms[noms == "R:E"] <- "E:R"
  ms <- function(x) { i <- match(x, noms); if (is.na(i)) NA_real_ else tab[["Mean Sq"]][i] }
  dfs <- function(x) { i <- match(x, noms); if (is.na(i)) NA_real_ else tab[["Df"]][i] }
  mse <- ms("Residuals"); mg <- ms("G"); mge <- ms("G:E")
  notes <- character(0)
  s2e <- mse; s2ge <- NA_real_
  if (plusieurs && r > 1) {
    s2ge <- (mge - mse) / r
    s2g  <- (mg - mge) / (r * e)
    den_mean <- mge / (r * e); ms_den <- mge; df_den <- dfs("G:E")
    s2p_moy <- max(s2g, 0) + max(s2ge, 0) / e + s2e / (r * e)
  } else if (plusieurs) {
    # Une observation par cellule : l'interaction se confond avec l'erreur.
    s2g <- (mg - mse) / e
    den_mean <- mse / e; ms_den <- mse; df_den <- dfs("Residuals")
    s2p_moy <- max(s2g, 0) + s2e / e
    notes <- c(notes, tr("Une seule observation par génotype et par environnement : l'interaction G × E se confond avec l'erreur."))
  } else if (r > 1) {
    s2g <- (mg - mse) / r
    den_mean <- mse / r; ms_den <- mse; df_den <- dfs("Residuals")
    s2p_moy <- max(s2g, 0) + s2e / r
  } else {
    return(list(ok = FALSE, motif = tr("Sans répétition ni environnement, la variance génétique n'est pas estimable.")))
  }
  s2g_brut <- s2g
  s2p_brut <- s2p_moy - max(s2g, 0) + s2g
  if (is.finite(s2g) && s2g < 0) {
    notes <- c(notes, tr("Variance génétique estimée négative, ramenée à zéro : les génotypes ne se distinguent pas de l'erreur."))
    s2g <- 0
  }
  if (is.finite(s2ge) && s2ge < 0) s2ge <- 0
  mu <- mean(dd$y)
  H2 <- if (s2p_moy > 0) max(s2g, 0) / s2p_moy else NA_real_
  s2p_parcelle <- max(s2g, 0) + (if (is.finite(s2ge)) s2ge else 0) + s2e
  h2_parcelle <- if (s2p_parcelle > 0) max(s2g, 0) / s2p_parcelle else NA_real_
  Fg <- mg / ms_den
  precision <- if (is.finite(Fg) && Fg > 1) sqrt(1 - 1 / Fg) else 0
  k <- hstat_gen_intensite(intensite)
  GA <- k * H2 * sqrt(s2p_moy)
  pct <- function(x) if (is.finite(mu) && mu != 0) 100 * x / abs(mu) else NA_real_
  composantes <- data.frame(
    Caractere = trait, Moyenne = mu, Genotypes = g, Environnements = e,
    Repetitions = round(r, 2),
    Var_genetique = s2g, Var_interaction_GE = s2ge, Var_erreur = s2e,
    Var_phenotypique = s2p_moy,
    H2_moyenne = H2, H2_parcelle = h2_parcelle,
    CVg_pct = pct(sqrt(max(s2g, 0))), CVp_pct = pct(sqrt(s2p_moy)),
    CVe_pct = pct(sqrt(s2e)),
    CVg_CVe = if (s2e > 0) sqrt(max(s2g, 0)) / sqrt(s2e) else NA_real_,
    Precision_selective = precision,
    Gain_genetique = GA, Gain_genetique_pct = pct(GA),
    F_genotypes = Fg,
    p_genotypes = stats::pf(Fg, dfs("G"), df_den, lower.tail = FALSE),
    stringsAsFactors = FALSE, check.names = FALSE)
  anova_tab <- data.frame(
    Source = .hstat_gen_source(noms), ddl = tab[["Df"]], SC = tab[["Sum Sq"]],
    CM = tab[["Mean Sq"]], F = tab[["F value"]], p = tab[["Pr(>F)"]],
    stringsAsFactors = FALSE, check.names = FALSE)
  # En combine, le test des genotypes se fait contre l'interaction : c'est le
  # denominateur d'un modele ou les environnements sont aleatoires.
  if (plusieurs && r > 1) {
    i <- match("G", noms)
    anova_tab$F[i] <- Fg; anova_tab$p[i] <- composantes$p_genotypes
    anova_tab$Source[i] <- tr("Génotypes (testés contre G × E)")
  }
  moy <- tapply(dd$y, dd$G, mean)
  valeurs <- data.frame(Genotype = names(moy), Moyenne = as.numeric(moy),
                        Valeur_genetique = mu + (if (is.finite(H2)) H2 else 0) *
                          (as.numeric(moy) - mu),
                        stringsAsFactors = FALSE)
  list(ok = TRUE, anova = anova_tab, composantes = composantes,
       valeurs = valeurs, s2g_brut = s2g_brut, s2p_brut = s2p_brut, mse = mse, df_erreur = dfs("Residuals"),
       ms_den = ms_den, df_den = df_den, n_par_moyenne = den_mean,
       r = r, e = e, notes = notes)
}

.hstat_gen_source <- function(noms) {
  lib <- c("E" = tr("Environnements"), "E:R" = tr("Répétitions dans environnements"),
           "R" = tr("Répétitions (blocs)"), "G" = tr("Génotypes"),
           "G:E" = tr("Interaction G × E"), "Residuals" = tr("Erreur"))
  out <- unname(lib[noms]); out[is.na(out)] <- noms[is.na(out)]; out
}

# Lecture des composantes, pour la synthese et les tableaux.
hstat_gen_lecture_composantes <- function(cp) {
  if (is.null(cp) || !nrow(cp)) return(cp)
  cp$Classe_H2 <- .hstat_gen_classe(cp$H2_moyenne, HSTAT_GEN_GRILLES$H2$bornes, .hstat_gen_libelles_h2())
  cp$Classe_gain <- .hstat_gen_classe(cp$Gain_genetique_pct, HSTAT_GEN_GRILLES$GAM$bornes,
                                      c(tr("Faible (gain < 10 %)"), tr("Moyen (gain de 10 à 20 %)"), tr("Élevé (gain > 20 %)")))
  cp$Classe_precision <- .hstat_gen_classe(cp$Precision_selective,
    HSTAT_GEN_GRILLES$PRECISION$bornes,
    c(tr("Faible (précision < 0,5)"), tr("Modérée (précision de 0,5 à 0,7)"),
      tr("Élevée (précision de 0,7 à 0,9)"), tr("Très élevée (précision ≥ 0,9)")))
  cp
}

# L'heritabilite au sens strict demande la variance additive, que seul le
# diallele estime : elle s'ajoute au caractere qui en a un, NA ailleurs.
hstat_gen_composantes_h2 <- function(cp, griffing = NULL) {
  if (is.null(cp) || !nrow(cp)) return(cp)
  h <- vapply(as.character(cp$Caractere), function(t) {
    g <- if (is.list(griffing)) griffing[[t]]
    if (isTRUE(g$ok) && is.finite(g$h2_strict %||% NA_real_)) g$h2_strict else NA_real_
  }, numeric(1))
  i <- match("H2_parcelle", names(cp))
  cp <- cbind(cp[seq_len(i)], h2_strict = unname(h), cp[-seq_len(i)])
  cp$Classe_h2 <- vapply(cp$h2_strict, .hstat_gen_classe_metrique, "",
                         m = HSTAT_GEN_METRIQUES$h2_strict)
  cp
}

# Tableau genotypes x environnements des moyennes d'un caractere.
hstat_gen_table_ge <- function(d, trait) {
  y <- d[[trait]]
  ok <- is.finite(y)
  m <- tapply(y[ok], list(d$G[ok], d$E[ok]), mean)
  m <- m[rowSums(is.finite(m)) > 0, colSums(is.finite(m)) > 0, drop = FALSE]
  m
}

# Moyennes des genotypes pour tous les caracteres.
hstat_gen_moyennes <- function(d, traits) {
  g <- levels(droplevels(d$G))
  out <- data.frame(Genotype = g, stringsAsFactors = FALSE)
  for (t in traits) {
    y <- d[[t]]; ok <- is.finite(y)
    m <- tapply(y[ok], d$G[ok], mean)
    out[[t]] <- as.numeric(m[g])
  }
  out
}

# CORRELATIONS GENOTYPIQUES ET PHENOTYPIQUES
# La covariance genetique se tire des variances : cov(x, y) = (var(x + y) -
# var(x) - var(y)) / 2, chacune estimee par le meme modele.
hstat_gen_correlations <- function(d, traits) {
  k <- length(traits)
  if (k < 2) return(NULL)
  vg <- function(dat, nom) {
    a <- hstat_gen_anova(dat, nom)
    if (!isTRUE(a$ok)) return(c(g = NA_real_, p = NA_real_))
    c(g = a$s2g_brut, p = a$s2p_brut)
  }
  v <- lapply(traits, function(t) vg(d, t)); names(v) <- traits
  rg <- rp <- matrix(NA_real_, k, k, dimnames = list(traits, traits))
  diag(rg) <- diag(rp) <- 1
  for (i in seq_len(k - 1)) for (j in (i + 1):k) {
    a <- traits[i]; b <- traits[j]
    dd <- d; dd$.somme <- d[[a]] + d[[b]]
    s <- vg(dd, ".somme")
    cg <- (s["g"] - v[[a]]["g"] - v[[b]]["g"]) / 2
    cp <- (s["p"] - v[[a]]["p"] - v[[b]]["p"]) / 2
    den_g <- sqrt(v[[a]]["g"] * v[[b]]["g"]); den_p <- sqrt(v[[a]]["p"] * v[[b]]["p"])
    rg[i, j] <- rg[j, i] <- if (isTRUE(den_g > 0)) cg / den_g else NA_real_
    rp[i, j] <- rp[j, i] <- if (isTRUE(den_p > 0)) cp / den_p else NA_real_
  }
  list(genotypique = rg, phenotypique = rp)
}

# Les coefficients de piste : effets directs des caracteres sur le caractere
# principal, a partir des correlations entre moyennes des genotypes.
hstat_gen_pistes <- function(moyennes, cible, explicatives) {
  explicatives <- setdiff(intersect(explicatives, names(moyennes)), cible)
  if (length(explicatives) < 1 || !cible %in% names(moyennes))
    return(list(ok = FALSE, motif = tr("Il faut un caractère principal et au moins un autre caractère.")))
  m <- moyennes[, c(cible, explicatives), drop = FALSE]
  m <- m[stats::complete.cases(m), , drop = FALSE]
  if (nrow(m) <= length(explicatives) + 1)
    return(list(ok = FALSE, motif = tr("Trop peu de génotypes pour l'analyse des pistes : il en faut plus que de caractères explicatifs.")))
  R <- suppressWarnings(stats::cor(m))
  if (!all(is.finite(R)))
    return(list(ok = FALSE, motif = tr("Un caractère est constant d'un génotype à l'autre : ses corrélations ne sont pas calculables, l'analyse des pistes est impossible.")))
  Rxx <- R[explicatives, explicatives, drop = FALSE]
  rxy <- R[explicatives, cible]
  P <- tryCatch(solve(Rxx, rxy), error = function(e) NULL)
  if (is.null(P) || kappa(Rxx) > 1e8)
    return(list(ok = FALSE, motif = tr("Caractères explicatifs colinéaires : les effets directs ne sont pas identifiables. Retirez un caractère redondant.")))
  indirect <- Rxx * matrix(P, length(P), length(P), byrow = TRUE)
  diag(indirect) <- NA
  tab <- data.frame(Caractere = explicatives, Correlation = as.numeric(rxy),
                    Effet_direct = as.numeric(P),
                    Effets_indirects = rowSums(indirect, na.rm = TRUE),
                    stringsAsFactors = FALSE, check.names = FALSE)
  res2 <- 1 - sum(P * rxy)
  list(ok = TRUE, table = tab, indirects = indirect,
       residuel = if (isTRUE(res2 >= 0)) sqrt(res2) else NA_real_, cible = cible)
}

# DIALLELE DE GRIFFING : APTITUDES GENERALE (AGC) ET SPECIFIQUE (ASC)
# Les effets s'estiment par moindres carres sur les moyennes des cellules,
# y_ij = mu + g_i + g_j + s_ij, avec la somme des g nulle. Sur un diallele
# complet, ce sont exactement les estimateurs de Griffing (1956), methodes 2
# (parents et F1) et 4 (F1 seuls) ; les reciproques, quand il y en a, sont
# testees puis moyennees. Le meme calcul tient sur un diallele incomplet.
HSTAT_GEN_METHODES_GRIFFING <- c(
  "1" = "Méthode 1 : parents, F1 et réciproques",
  "2" = "Méthode 2 : parents et F1",
  "3" = "Méthode 3 : F1 et réciproques",
  "4" = "Méthode 4 : F1 seuls",
  "LT" = "Plan lignées × testeurs (Kempthorne 1957)")

# Les cellules du diallele : une moyenne par couple de parents.
.hstat_gen_cellules <- function(d, trait) {
  p1 <- trimws(as.character(d$P1)); p2 <- trimws(as.character(d$P2))
  g <- as.character(d$G)
  # Un parent peut figurer sous son seul nom, sans colonnes de parents.
  vide1 <- is.na(p1) | !nzchar(p1); vide2 <- is.na(p2) | !nzchar(p2)
  parents_nommes <- unique(c(p1[!vide1], p2[!vide2]))
  seul <- vide1 & vide2 & g %in% parents_nommes
  p1[seul] <- g[seul]; p2[seul] <- g[seul]
  # Un seul parent nomme : c'est ce parent si la ligne le porte, sinon la
  # ligne est un croisement incomplet, ecarte et nomme.
  un1 <- !vide1 & vide2 & g == p1; p2[un1] <- p1[un1]
  un2 <- vide1 & !vide2 & g == p2; p1[un2] <- p2[un2]
  mesure <- is.finite(d[[trait]])
  incomplet <- xor(vide1, vide2) & !(un1 | un2) & mesure
  ok <- !is.na(p1) & nzchar(p1) & !is.na(p2) & nzchar(p2) & mesure
  res <- data.frame(P1 = p1[ok], P2 = p2[ok], E = d$E[ok], R = d$R[ok], y = d[[trait]][ok],
                    stringsAsFactors = FALSE)
  attr(res, "incomplets") <- unique(g[incomplet])
  res
}

hstat_gen_griffing <- function(d, trait, F_consanguinite = 1, direction = "augmenter",
                               temoin = NULL, alpha = 0.05) {
  refus <- function(m) list(ok = FALSE, motif = m)
  if (all(is.na(d$P1)) && all(is.na(d$P2)))
    return(refus(tr("Indiquez les colonnes des deux parents pour analyser un diallèle.")))
  cel <- .hstat_gen_cellules(d, trait)
  if (!nrow(cel)) return(refus(tr("Aucun croisement mesuré pour ce caractère.")))
  parents <- sort(unique(c(cel$P1, cel$P2)))
  p <- length(parents)
  if (p < 3) return(refus(tr("Un diallèle demande au moins trois parents.")))
  if (p > HSTAT_GEN_PARENTS_MAX)
    return(refus(trf("%d parents : au-delà de %d, la colonne choisie n'est sans doute pas celle des parents.",
                     p, HSTAT_GEN_PARENTS_MAX)))
  notes <- character(0)
  inc <- attr(cel, "incomplets")
  if (length(inc))
    notes <- c(notes, trf("%d génotype(s) écarté(s) du diallèle, un seul de leurs parents étant renseigné : %s.",
                          length(inc), hstat_lignes_compactes(inc, max = 10L)))
  Fc <- suppressWarnings(as.numeric(F_consanguinite)[1])
  if (!is.finite(Fc) || Fc < 0 || Fc > 1) Fc <- 1

  # L'erreur : celle des moyennes des entrees, par le modele d'ensemble.
  de <- data.frame(G = factor(paste(cel$P1, cel$P2, sep = " × ")), E = cel$E,
                   R = cel$R, stringsAsFactors = FALSE)
  de$E <- factor(de$E); de$y <- cel$y
  names(de)[names(de) == "y"] <- trait
  er <- hstat_gen_anova(de, trait)
  s2e_moy <- if (isTRUE(er$ok)) er$n_par_moyenne else NA_real_
  df_err <- if (isTRUE(er$ok)) er$df_den else NA_real_
  if (!is.finite(s2e_moy))
    notes <- c(notes, tr("Sans répétition, l'erreur n'est pas estimable : les effets sont donnés sans test."))

  # Moyennes ordonnees, puis reciproques.
  mo <- stats::aggregate(y ~ P1 + P2, data = cel, FUN = mean)
  cle <- ifelse(mo$P1 <= mo$P2, paste(mo$P1, mo$P2, sep = "\r"), paste(mo$P2, mo$P1, sep = "\r"))
  rec <- NULL
  if (anyDuplicated(cle)) {
    dup <- cle[duplicated(cle)]
    r_ij <- vapply(dup, function(k) {
      m <- mo[cle == k, , drop = FALSE]
      m <- m[order(m$P1 != pmin(m$P1, m$P2)), ]
      (m$y[1] - m$y[2]) / 2
    }, numeric(1))
    ss_rec <- sum(2 * r_ij^2)
    df_rec <- length(r_ij)
    ms_rec <- ss_rec / df_rec
    f_rec <- if (is.finite(s2e_moy) && s2e_moy > 0) ms_rec / s2e_moy else NA_real_
    rec <- data.frame(
      Croisement = vapply(strsplit(dup, "\r"), paste, "", collapse = " × "),
      Effet_reciproque = r_ij, stringsAsFactors = FALSE)
    rec_ligne <- data.frame(Source = tr("Effets réciproques"), ddl = df_rec, SC = ss_rec,
                            CM = ms_rec, F = f_rec,
                            p = if (is.finite(f_rec)) stats::pf(f_rec, df_rec, df_err, lower.tail = FALSE) else NA_real_,
                            stringsAsFactors = FALSE)
  }
  sym <- stats::aggregate(mo$y, by = list(cle = cle), FUN = mean)
  pp <- strsplit(sym$cle, "\r")
  a <- vapply(pp, `[`, "", 1); b <- vapply(pp, `[`, "", 2)
  y <- sym$x
  n <- length(y)
  diag_present <- any(a == b)
  methode <- if (diag_present) (if (is.null(rec)) "2" else "1") else (if (is.null(rec)) "4" else "3")
  Z <- matrix(0, n, p, dimnames = list(NULL, parents))
  for (i in seq_len(n)) {
    Z[i, a[i]] <- Z[i, a[i]] + 1
    Z[i, b[i]] <- Z[i, b[i]] + 1
  }
  Zc <- Z[, seq_len(p - 1), drop = FALSE] - Z[, p]
  X <- cbind(1, Zc)
  if (qr(X)$rank < ncol(X))
    return(refus(tr("Les croisements ne relient pas assez les parents pour séparer leurs aptitudes.")))
  fit <- stats::lm.fit(X, y)
  beta <- fit$coefficients
  ajuste <- as.numeric(X %*% beta)
  sca <- y - ajuste
  ss_g <- sum((ajuste - mean(y))^2); df_g <- p - 1
  ss_s <- sum(sca^2); df_s <- n - p
  if (df_s <= 0) return(refus(tr("Trop peu de croisements : l'aptitude spécifique n'a aucun degré de liberté.")))
  ms_g <- ss_g / df_g; ms_s <- ss_s / df_s
  XtXi <- solve(crossprod(X))
  C <- rbind(cbind(0, diag(p - 1)), c(0, rep(-1, p - 1)))
  gca <- as.numeric(C %*% beta)
  H <- X %*% XtXi %*% t(X)
  test <- function(est, var) {
    se <- if (is.finite(s2e_moy)) sqrt(pmax(var, 0) * s2e_moy) else rep(NA_real_, length(est))
    t <- est / se
    list(se = se, t = t, p = if (is.finite(df_err)) 2 * stats::pt(-abs(t), df_err) else NA_real_)
  }
  tg <- test(gca, diag(C %*% XtXi %*% t(C)))
  ts <- test(sca, 1 - diag(H))
  # Le coefficient de la variance d'AGC dans l'esperance de son carre moyen :
  # (p + 2) en methode 2, (p - 2) en methode 4, calcule ici sur le plan reel.
  A_g <- H - matrix(1 / n, n, n)
  k_g <- sum(diag(A_g %*% Z %*% t(Z))) / df_g
  s2gca <- (ms_g - ms_s) / k_g
  s2sca <- ms_s - (if (is.finite(s2e_moy)) s2e_moy else 0)
  if (s2gca < 0) { notes <- c(notes, tr("Variance d'AGC estimée négative, ramenée à zéro.")); s2gca <- 0 }
  if (s2sca < 0) { notes <- c(notes, tr("Variance d'ASC estimée négative, ramenée à zéro.")); s2sca <- 0 }
  s2e_parcelle <- if (isTRUE(er$ok)) er$mse else NA_real_
  Fv <- function(ms) if (is.finite(s2e_moy) && s2e_moy > 0) ms / s2e_moy else NA_real_
  pv <- function(f, df1) if (is.finite(f) && is.finite(df_err)) stats::pf(f, df1, df_err, lower.tail = FALSE) else NA_real_
  anova_tab <- data.frame(
    Source = c(tr("Aptitude générale à la combinaison (AGC)"),
               tr("Aptitude spécifique à la combinaison (ASC)")),
    ddl = c(df_g, df_s), SC = c(ss_g, ss_s), CM = c(ms_g, ms_s),
    F = c(Fv(ms_g), Fv(ms_s)), p = c(pv(Fv(ms_g), df_g), pv(Fv(ms_s), df_s)),
    stringsAsFactors = FALSE)
  if (!is.null(rec)) anova_tab <- rbind(anova_tab, rec_ligne)
  anova_tab <- rbind(anova_tab, data.frame(
    Source = tr("Erreur (moyennes)"), ddl = df_err, SC = NA_real_, CM = s2e_moy,
    F = NA_real_, p = NA_real_, stringsAsFactors = FALSE))
  sgn <- .hstat_gen_signe(direction)
  agc <- data.frame(Parent = parents, AGC = gca, Erreur_type = tg$se, t = tg$t, p = tg$p,
                    stringsAsFactors = FALSE)
  alpha <- suppressWarnings(as.numeric(alpha)[1])
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05
  agc$Sens <- .hstat_gen_sens(agc$p, sgn * agc$AGC, alpha)
  agc$Rang <- rank(-sgn * agc$AGC, ties.method = "min")
  agc <- agc[order(agc$Rang), , drop = FALSE]

  moy_par <- stats::setNames(y[a == b], a[a == b])
  croix <- a != b
  g_of <- stats::setNames(gca, parents)
  tc <- data.frame(Croisement = paste(a, b, sep = " × "), Parent_1 = a, Parent_2 = b,
                   Moyenne = y, ASC = sca, Erreur_type = ts$se, t = ts$t, p = ts$p,
                   AGC_parent_1 = unname(g_of[a]), AGC_parent_2 = unname(g_of[b]),
                   stringsAsFactors = FALSE)[croix, , drop = FALSE]
  fin <- .hstat_gen_croisements_finir(tc, moy_par, d, trait, temoin, sgn, alpha)
  notes <- c(notes, fin$notes)
  var_tab <- .hstat_gen_variances_ac(s2gca, s2sca, Fc, s2e_parcelle)
  list(ok = TRUE, methode = methode, parents = parents, p = p, anova = anova_tab,
       agc = agc, croisements = fin$table, reciproques = rec, variances = var_tab,
       baker = attr(var_tab, "baker"), h2_strict = attr(var_tab, "h2_strict"),
       h2_large = attr(var_tab, "h2_large"),
       consanguinite = Fc, notes = notes,
       action = .hstat_gen_action_genique(attr(var_tab, "baker")))
}

# Ce qui suit les effets, commun au diallele et aux lignees x testeurs : la
# somme des AGC, l'heterosis quand les parents sont mesures, le profil des
# parents et le sens de chaque ASC.
.hstat_gen_croisements_finir <- function(tc, moy_par, d, trait, temoin, sgn, alpha) {
  notes <- character(0)
  temoin_moy <- NA_real_
  if (length(temoin) && nzchar(temoin[1])) {
    v <- d[[trait]][as.character(d$G) == temoin[1]]
    if (any(is.finite(v))) temoin_moy <- mean(v, na.rm = TRUE)
    else notes <- c(notes, trf("Le témoin « %s » n'est pas mesuré : pas d'hétérosis standard.", temoin[1]))
  }
  tc$Somme_AGC <- tc$AGC_parent_1 + tc$AGC_parent_2
  if (length(moy_par)) {
    m1 <- unname(moy_par[tc$Parent_1]); m2 <- unname(moy_par[tc$Parent_2])
    mp <- (m1 + m2) / 2
    bp <- if (sgn > 0) pmax(m1, m2) else pmin(m1, m2)
    tc$Moyenne_parents <- mp
    tc$Heterosis_moyenne_parents_pct <- ifelse(is.finite(mp) & mp != 0, 100 * (tc$Moyenne - mp) / abs(mp), NA_real_)
    tc$Heterosis_meilleur_parent_pct <- ifelse(is.finite(bp) & bp != 0, 100 * (tc$Moyenne - bp) / abs(bp), NA_real_)
  }
  if (is.finite(temoin_moy) && temoin_moy != 0)
    tc$Heterosis_standard_pct <- 100 * (tc$Moyenne - temoin_moy) / abs(temoin_moy)
  # Le profil s'ecrit en phrase entiere : « faible » seul n'entre pas au
  # dictionnaire.
  profils <- c(HH = tr("AGC élevée × AGC élevée"), HB = tr("AGC élevée × AGC faible"),
               BH = tr("AGC faible × AGC élevée"), BB = tr("AGC faible × AGC faible"))
  niveau <- function(x) ifelse(sgn * x > 0, "H", "B")
  tc$Profil_AGC <- unname(profils[paste0(niveau(tc$AGC_parent_1), niveau(tc$AGC_parent_2))])
  tc$Sens_ASC <- .hstat_gen_sens(tc$p, sgn * tc$ASC, alpha)
  tc <- tc[order(-sgn * tc$ASC), , drop = FALSE]
  rownames(tc) <- NULL
  list(table = tc, notes = notes)
}

# Les composantes genetiques tirees des variances d'AGC et d'ASC :
# s2A = 4 s2agc / (1 + F), s2D = 4 s2asc / (1 + F)^2.
.hstat_gen_variances_ac <- function(s2gca, s2sca, Fc, s2e_parcelle) {
  s2A <- 4 * s2gca / (1 + Fc)
  s2D <- 4 * s2sca / (1 + Fc)^2
  tot <- s2A + s2D + (if (is.finite(s2e_parcelle)) s2e_parcelle else 0)
  baker <- if ((2 * s2gca + s2sca) > 0) 2 * s2gca / (2 * s2gca + s2sca) else NA_real_
  h2_strict <- if (tot > 0) s2A / tot else NA_real_
  h2_large <- if (tot > 0) (s2A + s2D) / tot else NA_real_
  var_tab <- data.frame(
    Parametre = c(tr("Variance d'AGC (σ²agc)"), tr("Variance d'ASC (σ²asc)"),
                  tr("Variance additive (σ²A)"), tr("Variance de dominance (σ²D)"),
                  tr("Variance d'erreur (σ²e, parcelle)"),
                  tr("Ratio de Baker 2σ²agc / (2σ²agc + σ²asc)"),
                  tr("Rapport σ²agc / σ²asc"),
                  tr("Héritabilité au sens strict (h²)"),
                  tr("Héritabilité au sens large (H²)"),
                  tr("Degré moyen de dominance √(2σ²D / σ²A)")),
    Valeur = c(s2gca, s2sca, s2A, s2D, s2e_parcelle, baker,
               if (s2sca > 0) s2gca / s2sca else NA_real_,
               h2_strict, h2_large,
               if (s2A > 0) sqrt(2 * s2D / s2A) else NA_real_),
    stringsAsFactors = FALSE)
  structure(var_tab, baker = baker, h2_strict = h2_strict, h2_large = h2_large)
}

# PLAN LIGNEES x TESTEURS (Kempthorne 1957)
# Chaque lignee (femelle, Parent 1) est croisee avec chaque testeur (male,
# Parent 2), et les deux groupes sont distincts. Sur les moyennes des
# croisements : g_i = y_i. - y.., g_j = y.j - y.., s_ij = y_ij - y_i. - y.j + y..
# Les erreurs-types sont celles de Singh & Chaudhary (1985).
HSTAT_GEN_PLANS_CROISEMENT <- c(
  "Automatique (selon les croisements)" = "auto",
  "Diallèle (Griffing)" = "diallele",
  "Lignées × testeurs (Kempthorne)" = "ligne_testeur")

# Le plan se lit sur les croisements : des Parents 1 et des Parents 2
# disjoints, au moins deux de chaque, c'est un plan lignees x testeurs ; un
# diallele, meme demi, fait figurer un meme parent des deux cotes.
hstat_gen_plan_croisement <- function(d, trait) {
  cel <- .hstat_gen_cellules(d, trait)
  cr <- cel[cel$P1 != cel$P2, , drop = FALSE]
  l <- unique(cr$P1); t <- unique(cr$P2)
  if (length(l) >= 2 && length(t) >= 2 && !length(intersect(l, t))) "ligne_testeur" else "diallele"
}

hstat_gen_ligne_testeur <- function(d, trait, F_consanguinite = 1, direction = "augmenter",
                                    temoin = NULL, alpha = 0.05) {
  refus <- function(m) list(ok = FALSE, motif = m)
  if (all(is.na(d$P1)) && all(is.na(d$P2)))
    return(refus(tr("Indiquez la colonne des lignées (Parent 1) et celle des testeurs (Parent 2).")))
  cel <- .hstat_gen_cellules(d, trait)
  cr <- cel[cel$P1 != cel$P2, , drop = FALSE]
  if (!nrow(cr)) return(refus(tr("Aucun croisement mesuré pour ce caractère.")))
  lignees <- sort(unique(cr$P1)); testeurs <- sort(unique(cr$P2))
  communs <- intersect(lignees, testeurs)
  if (length(communs))
    return(refus(trf("« %s » est à la fois lignée et testeur : c'est un diallèle, pas un plan lignées × testeurs.",
                     communs[1])))
  nl <- length(lignees); nt <- length(testeurs)
  if (nl < 2 || nt < 2)
    return(refus(tr("Un plan lignées × testeurs demande au moins deux lignées et deux testeurs.")))
  if (nl + nt > HSTAT_GEN_PARENTS_MAX)
    return(refus(trf("%d parents : au-delà de %d, la colonne choisie n'est sans doute pas celle des parents.",
                     nl + nt, HSTAT_GEN_PARENTS_MAX)))
  notes <- character(0)
  inc <- attr(cel, "incomplets")
  if (length(inc))
    notes <- c(notes, trf("%d génotype(s) écarté(s) du plan, un seul de leurs parents étant renseigné : %s.",
                          length(inc), hstat_lignes_compactes(inc, max = 10L)))
  M <- tapply(cr$y, list(factor(cr$P1, lignees), factor(cr$P2, testeurs)), mean)
  if (anyNA(M)) {
    k <- which(is.na(M), arr.ind = TRUE)[1, ]
    return(refus(trf("Le croisement « %s × %s » n'est pas mesuré : le plan lignées × testeurs doit être complet.",
                     lignees[k[1]], testeurs[k[2]])))
  }
  Fc <- suppressWarnings(as.numeric(F_consanguinite)[1])
  if (!is.finite(Fc) || Fc < 0 || Fc > 1) Fc <- 1

  # L'erreur : celle d'une moyenne de croisement, par le modele d'ensemble.
  de <- data.frame(G = factor(paste(cr$P1, cr$P2, sep = " × ")), E = factor(cr$E),
                   R = cr$R, stringsAsFactors = FALSE)
  de[[trait]] <- cr$y
  er <- hstat_gen_anova(de, trait)
  s2e_moy <- if (isTRUE(er$ok)) er$n_par_moyenne else NA_real_
  df_err <- if (isTRUE(er$ok)) er$df_den else NA_real_
  if (!is.finite(s2e_moy))
    notes <- c(notes, tr("Sans répétition, l'erreur n'est pas estimable : les effets sont donnés sans test."))

  mu <- mean(M)
  gl <- rowMeans(M) - mu; gt <- colMeans(M) - mu
  S <- M - outer(rowMeans(M), colMeans(M), "+") + mu
  ss_l <- nt * sum(gl^2); ss_t <- nl * sum(gt^2); ss_s <- sum(S^2)
  df_l <- nl - 1; df_t <- nt - 1; df_s <- df_l * df_t
  ms_l <- ss_l / df_l; ms_t <- ss_t / df_t; ms_s <- ss_s / df_s
  Fv <- function(ms) if (is.finite(s2e_moy) && s2e_moy > 0) ms / s2e_moy else NA_real_
  pv <- function(f, df1) if (is.finite(f) && is.finite(df_err)) stats::pf(f, df1, df_err, lower.tail = FALSE) else NA_real_
  ss_c <- ss_l + ss_t + ss_s
  part <- function(x) if (ss_c > 0) 100 * x / ss_c else NA_real_
  anova_tab <- data.frame(
    Source = c(tr("Lignées (AGC des lignées)"), tr("Testeurs (AGC des testeurs)"),
               tr("Lignées × testeurs (ASC)"), tr("Erreur (moyennes)")),
    ddl = c(df_l, df_t, df_s, df_err), SC = c(ss_l, ss_t, ss_s, NA_real_),
    CM = c(ms_l, ms_t, ms_s, s2e_moy),
    F = c(Fv(ms_l), Fv(ms_t), Fv(ms_s), NA_real_),
    p = c(pv(Fv(ms_l), df_l), pv(Fv(ms_t), df_t), pv(Fv(ms_s), df_s), NA_real_),
    Contribution_pct = c(part(ss_l), part(ss_t), part(ss_s), NA_real_),
    stringsAsFactors = FALSE)

  test <- function(est, var) {
    se <- if (is.finite(s2e_moy)) rep(sqrt(var * s2e_moy), length(est)) else rep(NA_real_, length(est))
    t <- est / se
    list(se = se, t = t, p = if (is.finite(df_err)) 2 * stats::pt(-abs(t), df_err) else rep(NA_real_, length(est)))
  }
  tl <- test(gl, 1 / nt); tt <- test(gt, 1 / nl)
  sgn <- .hstat_gen_signe(direction)
  alpha <- suppressWarnings(as.numeric(alpha)[1])
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05
  agc <- data.frame(Parent = c(lignees, testeurs),
                    Role = c(rep(tr("Lignée (Parent 1)"), nl), rep(tr("Testeur (Parent 2)"), nt)),
                    AGC = c(unname(gl), unname(gt)), Erreur_type = c(tl$se, tt$se),
                    t = c(tl$t, tt$t), p = c(tl$p, tt$p), stringsAsFactors = FALSE)
  agc$Sens <- .hstat_gen_sens(agc$p, sgn * agc$AGC, alpha)
  agc$Rang <- stats::ave(-sgn * agc$AGC, agc$Role, FUN = function(v) rank(v, ties.method = "min"))
  agc <- agc[order(agc$Role != tr("Lignée (Parent 1)"), agc$Rang), , drop = FALSE]
  rownames(agc) <- NULL

  a <- rep(lignees, times = nt); b <- rep(testeurs, each = nl)
  ts <- test(as.vector(S), 1)
  tc <- data.frame(Croisement = paste(a, b, sep = " × "), Parent_1 = a, Parent_2 = b,
                   Moyenne = as.vector(M), ASC = as.vector(S), Erreur_type = ts$se,
                   t = ts$t, p = ts$p, AGC_parent_1 = unname(gl[a]), AGC_parent_2 = unname(gt[b]),
                   stringsAsFactors = FALSE)
  par <- cel[cel$P1 == cel$P2, , drop = FALSE]
  moy_par <- if (nrow(par)) tapply(par$y, par$P1, mean) else numeric(0)
  fin <- .hstat_gen_croisements_finir(tc, moy_par, d, trait, temoin, sgn, alpha)
  notes <- c(notes, fin$notes)

  # Composantes, sur la base d'une moyenne de croisement (Kempthorne 1957).
  s2gca_l <- (ms_l - ms_s) / nt
  s2gca_t <- (ms_t - ms_s) / nl
  s2gca <- (((nl - 1) * ms_l + (nt - 1) * ms_t) / (nl + nt - 2) - ms_s) / (2 * nl * nt - nl - nt)
  s2sca <- ms_s - (if (is.finite(s2e_moy)) s2e_moy else 0)
  if (s2gca < 0) { notes <- c(notes, tr("Variance d'AGC estimée négative, ramenée à zéro.")); s2gca <- 0 }
  if (s2sca < 0) { notes <- c(notes, tr("Variance d'ASC estimée négative, ramenée à zéro.")); s2sca <- 0 }
  vb <- .hstat_gen_variances_ac(s2gca, s2sca, Fc, if (isTRUE(er$ok)) er$mse else NA_real_)
  bk <- attr(vb, "baker"); h2s <- attr(vb, "h2_strict"); h2l <- attr(vb, "h2_large")
  var_tab <- rbind(data.frame(
    Parametre = c(tr("Variance d'AGC des lignées"), tr("Variance d'AGC des testeurs")),
    Valeur = c(max(s2gca_l, 0), max(s2gca_t, 0)), stringsAsFactors = FALSE), vb)
  list(ok = TRUE, methode = "LT", plan = "ligne_testeur", parents = c(lignees, testeurs),
       lignees = lignees, testeurs = testeurs, p = nl + nt, anova = anova_tab,
       agc = agc, croisements = fin$table, reciproques = NULL, variances = var_tab,
       baker = bk, h2_strict = h2s, h2_large = h2l, consanguinite = Fc, notes = notes,
       action = .hstat_gen_action_genique(bk))
}

# L'analyse des aptitudes a la combinaison, selon le plan demande ou lu sur
# les croisements.
hstat_gen_aptitudes <- function(d, trait, plan = "auto", ...) {
  if (!isTRUE(plan %in% HSTAT_GEN_PLANS_CROISEMENT) || plan == "auto")
    plan <- hstat_gen_plan_croisement(d, trait)
  if (plan == "ligne_testeur") hstat_gen_ligne_testeur(d, trait, ...)
  else { g <- hstat_gen_griffing(d, trait, ...); if (isTRUE(g$ok)) g$plan <- "diallele"; g }
}

# La lecture du ratio de Baker : proche de 1, l'action est additive et la
# selection precoce efficace ; sous 0,5, elle est non additive et l'heterosis
# s'exploite par l'hybridation.
.hstat_gen_action_genique <- function(baker) {
  if (!is.finite(baker)) return(tr("Non calculable : les variances d'AGC et d'ASC sont nulles."))
  .hstat_gen_sens_metrique(baker, HSTAT_GEN_METRIQUES$baker)
}

# CLASSEMENT MULTICRITERE DES CROISEMENTS
# Chaque critere est centre-reduit puis oriente dans le sens de
# l'amelioration ; l'indice est la moyenne ponderee sur les caracteres.
hstat_gen_classement_croisements <- function(liste, directions = NULL, poids = NULL) {
  liste <- Filter(function(r) isTRUE(r$ok), liste)
  if (!length(liste)) return(NULL)
  traits <- names(liste)
  dirs <- .hstat_gen_dir(directions, traits)
  w <- stats::setNames(rep(1, length(traits)), traits)
  if (length(poids)) { k <- intersect(names(poids), traits); w[k] <- pmax(0, as.numeric(poids[k])) }
  if (sum(w) <= 0) w[] <- 1
  z <- function(x) { s <- stats::sd(x, na.rm = TRUE); if (!is.finite(s) || s == 0) x * 0 else (x - mean(x, na.rm = TRUE)) / s }
  crx <- unique(unlist(lapply(liste, function(r) r$croisements$Croisement)))
  out <- data.frame(Croisement = crx, stringsAsFactors = FALSE)
  score <- matrix(NA_real_, length(crx), length(traits), dimnames = list(crx, traits))
  favo <- integer(length(crx))
  for (t in traits) {
    tc <- liste[[t]]$croisements
    s <- .hstat_gen_signe(dirs[[t]])
    crit <- cbind(z(s * tc$ASC), z(s * tc$Moyenne), z(s * tc$Somme_AGC),
                  if (!is.null(tc$Heterosis_moyenne_parents_pct)) z(s * tc$Heterosis_moyenne_parents_pct))
    sc <- rowMeans(crit, na.rm = TRUE)
    score[tc$Croisement, t] <- sc
    out[[paste0("ASC_", t)]] <- tc$ASC[match(crx, tc$Croisement)]
    favo <- favo + as.integer(crx %in% tc$Croisement[tc$Sens_ASC == tr("Favorable")])
  }
  idx <- apply(score, 1, function(v) { ok <- is.finite(v); if (!any(ok)) NA_real_ else sum(v[ok] * w[ok]) / sum(w[ok]) })
  out$Indice_multicritere <- as.numeric(idx)
  out$ASC_favorables <- favo
  out$Rang <- rank(-out$Indice_multicritere, ties.method = "min", na.last = "keep")
  out <- out[order(out$Rang), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# INTERACTION GENOTYPE x ENVIRONNEMENT : AMMI
# Les effets principaux s'estiment par l'analyse de variance, l'interaction
# se decompose par valeurs singulieres (Gauch 1988). Les degres de liberte
# des axes sont ceux de Gollob (1968) : g + e - 1 - 2k.
hstat_gen_ammi <- function(d, trait, direction = "augmenter", alpha = 0.05, an = NULL) {
  refus <- function(m) list(ok = FALSE, motif = m)
  Y <- hstat_gen_table_ge(d, trait)
  g <- nrow(Y); e <- ncol(Y)
  if (e < 3) return(refus(tr("AMMI et GGE demandent au moins trois environnements.")))
  if (g < 3) return(refus(tr("AMMI et GGE demandent au moins trois génotypes.")))
  manq <- which(!is.finite(Y), arr.ind = TRUE)
  if (nrow(manq))
    return(refus(trf("Tableau génotypes × environnements incomplet (%d case(s) vide(s), par exemple %s en %s) : AMMI et GGE exigent chaque génotype dans chaque environnement.",
                     nrow(manq), rownames(Y)[manq[1, 1]], colnames(Y)[manq[1, 2]])))
  if (is.null(an)) an <- hstat_gen_anova(d, trait)
  r <- if (isTRUE(an$ok)) an$r else 1
  mu <- mean(Y)
  # Un caractere constant n'a ni effet ni interaction : les parts d'axes
  # vaudraient 0/0 et l'ASV, le WAAS et le MTSI en sortiraient non numeriques.
  if (sum((Y - mu)^2) <= 1e-12 * max(1, sum(Y^2)))
    return(refus(tr("Le caractère est constant : AMMI et GGE n'ont rien à décomposer.")))
  GE <- sweep(sweep(Y, 1, rowMeans(Y)), 2, colMeans(Y)) + mu
  sv <- svd(GE)
  kmax <- min(g - 1, e - 1)
  lam <- sv$d[seq_len(kmax)]
  ss <- r * lam^2
  dfk <- g + e - 1 - 2 * seq_len(kmax)
  ss_ge <- r * sum(GE^2)
  if (ss_ge <= 1e-12 * r * sum((Y - mu)^2))
    return(refus(tr("Aucune interaction génotype × environnement : les génotypes gardent le même écart partout, l'AMMI n'a rien à décomposer.")))
  a_err <- isTRUE(an$ok) && r > 1 && is.finite(an$mse)
  ms_err <- if (a_err) an$mse else NA_real_
  df_err <- if (a_err) an$df_erreur else NA_real_
  ok_ax <- dfk > 0
  axes <- data.frame(
    Source = paste0("IPCA", seq_len(kmax)), ddl = dfk, SC = ss,
    CM = ifelse(ok_ax, ss / pmax(dfk, 1), NA_real_),
    Pct_GE = 100 * ss / ss_ge, Pct_cumule = cumsum(100 * ss / ss_ge),
    stringsAsFactors = FALSE)
  axes$F <- if (a_err) axes$CM / ms_err else NA_real_
  axes$p <- if (a_err) stats::pf(axes$F, axes$ddl, df_err, lower.tail = FALSE) else NA_real_
  axes <- axes[ok_ax, , drop = FALSE]
  if (!nrow(axes)) return(refus(tr("Le tableau est trop petit pour décomposer l'interaction.")))
  sig <- which(is.finite(axes$p) & axes$p < alpha)
  n_sig <- if (length(sig)) max(sig) else 1L
  sq <- sqrt(lam)
  sg <- sv$u[, seq_len(nrow(axes)), drop = FALSE] %*% diag(sq[seq_len(nrow(axes))], nrow(axes))
  se <- sv$v[, seq_len(nrow(axes)), drop = FALSE] %*% diag(sq[seq_len(nrow(axes))], nrow(axes))
  colnames(sg) <- colnames(se) <- axes$Source
  rownames(sg) <- rownames(Y); rownames(se) <- colnames(Y)
  # L'ASV de Purchase (2000) pondere le premier axe par sa part de l'interaction.
  asv <- if (ncol(sg) >= 2) sqrt((ss[1] / ss[2] * sg[, 1])^2 + sg[, 2]^2) else abs(sg[, 1])
  # Le WAAS d'Olivoto (2019) : moyenne des scores absolus, ponderee par la part
  # expliquee, sur les axes significatifs.
  w <- axes$Pct_GE[seq_len(n_sig)]
  waas <- as.numeric(abs(sg[, seq_len(n_sig), drop = FALSE]) %*% w / sum(w))
  sgn <- .hstat_gen_signe(direction)
  moy <- rowMeans(Y)
  rang_moy <- rank(-sgn * moy, ties.method = "min")
  rang_asv <- rank(asv, ties.method = "min")
  stab <- data.frame(Genotype = rownames(Y), Moyenne = moy, Rang_moyenne = rang_moy,
                     ASV = asv, Rang_ASV = rang_asv, YSI = rang_moy + rang_asv,
                     WAAS = waas, Rang_WAAS = rank(waas, ties.method = "min"),
                     stringsAsFactors = FALSE)
  stab$Rang_YSI <- rank(stab$YSI, ties.method = "min")
  stab <- stab[order(stab$YSI), , drop = FALSE]
  rownames(stab) <- NULL
  anova_tab <- if (isTRUE(an$ok)) an$anova else NULL
  if (!is.null(anova_tab)) {
    i <- which(anova_tab$Source == tr("Interaction G × E"))
    base <- anova_tab[setdiff(seq_len(nrow(anova_tab)), which(anova_tab$Source == tr("Erreur"))), , drop = FALSE]
    ipca <- data.frame(Source = paste0("   ", axes$Source), ddl = axes$ddl, SC = axes$SC,
                       CM = axes$CM, F = axes$F, p = axes$p, stringsAsFactors = FALSE)
    ss_res <- ss_ge - sum(axes$SC[seq_len(n_sig)])
    df_res <- sum(axes$ddl) - sum(axes$ddl[seq_len(n_sig)])
    reste <- if (df_res > 0) data.frame(Source = tr("   Résidu de l'interaction"), ddl = df_res,
                                         SC = ss_res, CM = ss_res / df_res,
                                         F = if (a_err) (ss_res / df_res) / ms_err else NA_real_,
                                         p = if (a_err) stats::pf((ss_res / df_res) / ms_err, df_res, df_err, lower.tail = FALSE) else NA_real_,
                                         stringsAsFactors = FALSE)
    # Sans repetition, la ligne « Erreur » est l'interaction elle-meme, deja
    # decomposee par les axes : la garder la compterait deux fois.
    err <- if (a_err) anova_tab[anova_tab$Source == tr("Erreur"), , drop = FALSE]
    anova_tab <- rbind(base, ipca[seq_len(n_sig), , drop = FALSE], reste, err)
  }
  list(ok = TRUE, Y = Y, axes = axes, n_sig = n_sig, axes_testes = a_err,
       n_axes_significatifs = length(sig), scores_g = sg, scores_e = se,
       moy_g = moy, moy_e = colMeans(Y), anova = anova_tab, stabilite = stab,
       ss_ge = ss_ge, r = r)
}

# GGE BIPLOT (Yan et al. 2000)
# Le tableau est centre par environnement : il garde l'effet genotype et
# l'interaction, qui sont ce que la selection exploite.
HSTAT_GEN_GGE_SVP <- c("Symétrique" = "symetrique",
                       "Centré génotypes (comparer les génotypes)" = "genotype",
                       "Centré environnements (comparer les environnements)" = "environnement")

hstat_gen_gge <- function(Y, direction = "augmenter", echelle = FALSE,
                          svp = "symetrique") {
  refus <- function(m) list(ok = FALSE, motif = m)
  if (is.null(Y) || nrow(Y) < 3 || ncol(Y) < 3)
    return(refus(tr("AMMI et GGE demandent au moins trois génotypes et trois environnements.")))
  X <- sweep(Y, 2, colMeans(Y))
  if (!all(is.finite(X)) || sum(X^2) <= 1e-12 * max(1, sum(Y^2)))
    return(refus(tr("Les génotypes ne diffèrent dans aucun environnement : le GGE biplot n'a rien à décomposer.")))
  if (isTRUE(echelle)) {
    s <- apply(Y, 2, stats::sd)
    s[!is.finite(s) | s == 0] <- 1
    X <- sweep(X, 2, s, "/")
  }
  sv <- svd(X)
  f <- switch(svp, genotype = 1, environnement = 0, 0.5)
  k <- min(2L, length(sv$d))
  if (k < 2) return(refus(tr("Le tableau n'a pas deux axes à représenter.")))
  G <- sv$u[, 1:2] %*% diag(sv$d[1:2]^f)
  E <- sv$v[, 1:2] %*% diag(sv$d[1:2]^(1 - f))
  # Les orientations de la decomposition sont arbitraires : on les fixe pour
  # que le premier axe porte la performance moyenne dans le sens positif.
  sgn <- .hstat_gen_signe(direction)
  if (stats::cor(G[, 1], rowMeans(Y)) * sgn < 0) { G[, 1] <- -G[, 1]; E[, 1] <- -E[, 1] }
  dimnames(G) <- list(rownames(Y), c("PC1", "PC2"))
  dimnames(E) <- list(colnames(Y), c("PC1", "PC2"))
  pct <- 100 * sv$d^2 / sum(sv$d^2)
  # Qui gagne ou : dans chaque environnement, le genotype de plus grand produit
  # scalaire (le plus petit pour un caractere a diminuer).
  prod <- G %*% t(E)
  gagnant <- apply(prod, 2, function(v) rownames(G)[if (sgn > 0) which.max(v) else which.min(v)])
  sommets <- rownames(G)[grDevices::chull(G[, 1], G[, 2])]
  mega <- split(names(gagnant), gagnant)
  # Moyenne et stabilite : axe de l'environnement moyen (AEC).
  aec <- colMeans(E); naec <- sqrt(sum(aec^2))
  u <- if (naec > 0) aec / naec else c(1, 0)
  proj <- as.numeric(G %*% u)
  dist <- abs(G[, 1] * u[2] - G[, 2] * u[1])
  ideal_pt <- max(sgn * proj) * u * sgn
  dist_ideal <- sqrt(rowSums(sweep(G, 2, ideal_pt)^2))
  gen <- data.frame(Genotype = rownames(G), PC1 = G[, 1], PC2 = G[, 2],
                    Moyenne = rowMeans(Y), Projection_AEC = proj,
                    Instabilite = dist, Distance_ideotype = dist_ideal,
                    Rang = rank(dist_ideal, ties.method = "min"),
                    Sommet_polygone = rownames(G) %in% sommets,
                    stringsAsFactors = FALSE)
  gen <- gen[order(gen$Rang), , drop = FALSE]; rownames(gen) <- NULL
  longueur <- sqrt(rowSums(E^2))
  cosang <- if (naec > 0) as.numeric(E %*% u) / pmax(longueur, 1e-12) else NA_real_
  env <- data.frame(Environnement = rownames(E), PC1 = E[, 1], PC2 = E[, 2],
                    Moyenne = colMeans(Y), Pouvoir_discriminant = longueur,
                    Representativite = cosang,
                    Gagnant = unname(gagnant[rownames(E)]),
                    Score_ideal = longueur * cosang,
                    stringsAsFactors = FALSE)
  env <- env[order(-env$Score_ideal), , drop = FALSE]; rownames(env) <- NULL
  meg <- data.frame(Mega_environnement = seq_along(mega), Genotype_gagnant = names(mega),
                    Environnements = vapply(mega, paste, "", collapse = ", "),
                    stringsAsFactors = FALSE)
  list(ok = TRUE, G = G, E = E, pct = pct[1:2], genotypes = gen, environnements = env,
       mega = meg, sommets = sommets, aec = u, svp = svp, echelle = isTRUE(echelle))
}

# STABILITE : Finlay & Wilkinson (1963), Eberhart & Russell (1966), Wricke
# (1962), Shukla (1972), Lin & Binns (1988).
hstat_gen_stabilite <- function(Y, mse = NA_real_, r = 1, direction = "augmenter") {
  if (is.null(Y) || ncol(Y) < 3 || any(!is.finite(Y))) return(NULL)
  g <- nrow(Y); e <- ncol(Y)
  idx <- colMeans(Y) - mean(Y)
  sxx <- sum(idx^2)
  if (sxx <= 0) return(NULL)
  sgn <- .hstat_gen_signe(direction)
  GE <- sweep(sweep(Y, 1, rowMeans(Y)), 2, colMeans(Y)) + mean(Y)
  W <- rowSums(GE^2)
  sh <- (g / ((g - 2) * (e - 1))) * W - sum(W) / ((g - 1) * (g - 2) * (e - 1))
  best <- apply(Y, 2, function(v) if (sgn > 0) max(v) else min(v))
  pi_lb <- rowSums(sweep(Y, 2, best)^2) / (2 * e)
  res <- lapply(seq_len(g), function(i) {
    y <- Y[i, ]
    b <- sum((y - mean(y)) * idx) / sxx
    dev <- y - mean(y) - b * idx
    s2d <- sum(dev^2) / (e - 2) - (if (is.finite(mse)) mse / r else 0)
    se_b <- sqrt(sum(dev^2) / (e - 2) / sxx)
    tb <- (b - 1) / se_b
    r2 <- if (stats::var(y) > 0) stats::cor(y, idx)^2 else NA_real_
    data.frame(b = b, Erreur_type_b = se_b,
               p_b_egal_1 = 2 * stats::pt(-abs(tb), e - 2),
               S2d = s2d, R2 = r2, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, res)
  out <- data.frame(Genotype = rownames(Y), Moyenne = rowMeans(Y), res,
                    Ecovalence_Wricke = W, Variance_Shukla = sh,
                    Superiorite_Lin_Binns = pi_lb,
                    CV_pct = 100 * apply(Y, 1, stats::sd) / abs(rowMeans(Y)),
                    stringsAsFactors = FALSE)
  out$Adaptation <- ifelse(out$p_b_egal_1 < 0.05 & out$b > 1,
                           tr("Adaptée aux environnements favorables (b > 1)"),
                    ifelse(out$p_b_egal_1 < 0.05 & out$b < 1,
                           tr("Adaptée aux environnements défavorables (b < 1)"),
                           tr("Adaptation générale (b ≈ 1)")))
  out$Rang_Lin_Binns <- rank(out$Superiorite_Lin_Binns, ties.method = "min")
  out <- out[order(out$Rang_Lin_Binns), , drop = FALSE]
  rownames(out) <- NULL
  list(table = out, indice = data.frame(Environnement = names(idx), Indice = idx,
                                        stringsAsFactors = FALSE))
}

# SELECTION MULTICARACTERE : L'INDICE DE DISTANCE A L'IDEOTYPE
# Le coeur commun du MGIDI (Olivoto & Nardino 2021) et du MTSI (Olivoto et
# al. 2019) : une analyse factorielle des caracteres remis a l'echelle 0-100
# (100 = le meilleur), puis la distance de chaque genotype a l'ideotype, qui
# vaut 100 partout.
.hstat_gen_reechelle <- function(x, direction = "augmenter") {
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || r[2] == r[1]) return(rep(NA_real_, length(x)))
  if (identical(direction, "diminuer")) 100 * (r[2] - x) / (r[2] - r[1])
  else 100 * (x - r[1]) / (r[2] - r[1])
}

.hstat_gen_ideotype <- function(R, si = 15) {
  refus <- function(m) list(ok = FALSE, motif = m)
  complet <- stats::complete.cases(R)
  incomplets <- rownames(R)[!complet]
  R <- R[complet, , drop = FALSE]
  if (ncol(R) < 2) return(refus(tr("L'indice multicaractère demande au moins deux caractères variables.")))
  if (nrow(R) <= ncol(R))
    return(refus(tr("Il faut plus de génotypes que de caractères pour l'analyse factorielle.")))
  cr <- stats::cor(R)
  if (qr(cr)$rank < ncol(cr))
    return(refus(tr("Des caractères sont parfaitement corrélés : retirez les doublons avant le calcul.")))
  ev <- eigen(cr, symmetric = TRUE)
  nf <- max(1L, sum(ev$values >= 1))
  A <- ev$vectors[, seq_len(nf), drop = FALSE] %*% diag(sqrt(ev$values[seq_len(nf)]), nf)
  if (nf >= 2) A <- unclass(stats::varimax(A, normalize = TRUE)$loadings)
  dimnames(A) <- list(colnames(R), paste0("FA", seq_len(nf)))
  centre <- colMeans(R); ech <- apply(R, 2, stats::sd)
  Z <- scale(R, centre, ech)
  B <- solve(cr, A)
  Fsc <- Z %*% B
  ideo <- ((rep(100, ncol(R)) - centre) / ech) %*% B
  D <- sweep(Fsc, 2, as.numeric(ideo))
  ind <- sqrt(rowSums(D^2))
  contrib <- D^2 / pmax(rowSums(D^2), 1e-12)
  colnames(contrib) <- colnames(A)
  facteur <- colnames(A)[apply(abs(A), 1, which.max)]
  n_sel <- max(1L, round(nrow(R) * si / 100))
  ordre <- order(ind)
  sel <- rownames(R)[ordre[seq_len(n_sel)]]
  charges <- data.frame(Caractere = colnames(R), A, Communalite = rowSums(A^2),
                        Unicite = 1 - rowSums(A^2), Facteur = facteur,
                        stringsAsFactors = FALSE, check.names = FALSE)
  valp <- data.frame(Facteur = paste0("PC", seq_along(ev$values)), Valeur_propre = ev$values,
                     Pct_variance = 100 * ev$values / sum(ev$values),
                     Pct_cumule = cumsum(100 * ev$values / sum(ev$values)),
                     stringsAsFactors = FALSE)
  list(ok = TRUE, genotypes = rownames(R), incomplets = incomplets,
       indice = ind, rang = rank(ind, ties.method = "min"), selection = sel,
       n_sel = n_sel, contributions = contrib, charges = charges, valeurs_propres = valp,
       scores = Fsc, ideotype = as.numeric(ideo))
}

# Les differentiels et gains de selection, caractere par caractere.
.hstat_gen_gains <- function(M, selection, directions, h2 = NULL) {
  do.call(rbind, lapply(colnames(M), function(t) {
    xo <- mean(M[, t], na.rm = TRUE); xs <- mean(M[selection, t], na.rm = TRUE)
    h <- if (!is.null(h2) && t %in% names(h2)) h2[[t]] else NA_real_
    sd <- xs - xo
    data.frame(Caractere = t, Sens = directions[[t]], Moyenne_population = xo,
               Moyenne_selection = xs, Differentiel = sd,
               Differentiel_pct = if (xo != 0) 100 * sd / abs(xo) else NA_real_,
               H2 = h, Gain = sd * h,
               Gain_pct = if (xo != 0) 100 * sd * h / abs(xo) else NA_real_,
               Gain_desire = ifelse(.hstat_gen_signe(directions[[t]]) * sd > 0,
                                    tr("Oui"), tr("Non")),
               stringsAsFactors = FALSE, check.names = FALSE)
  }))
}

hstat_gen_mgidi <- function(moyennes, traits, directions = NULL, si = 15, h2 = NULL) {
  traits <- intersect(traits, names(moyennes))
  dirs <- .hstat_gen_dir(directions, traits)
  M <- as.matrix(moyennes[, traits, drop = FALSE]); rownames(M) <- moyennes$Genotype
  R <- vapply(traits, function(t) .hstat_gen_reechelle(M[, t], dirs[[t]]), numeric(nrow(M)))
  R <- matrix(R, nrow(M), dimnames = list(rownames(M), traits))
  constants <- traits[colSums(is.finite(R)) == 0]
  R <- R[, setdiff(traits, constants), drop = FALSE]
  res <- .hstat_gen_ideotype(R, si)
  if (!isTRUE(res$ok)) return(res)
  gk <- res$genotypes
  tab <- data.frame(Genotype = gk, MGIDI = res$indice, Rang = res$rang,
                    Selectionne = gk %in% res$selection,
                    res$contributions * 100, stringsAsFactors = FALSE,
                    check.names = FALSE)
  names(tab)[5:ncol(tab)] <- paste0("Contribution_", colnames(res$contributions), "_pct")
  tab <- tab[order(tab$Rang), , drop = FALSE]; rownames(tab) <- NULL
  res$table <- tab
  res$gains <- .hstat_gen_gains(M[gk, colnames(R), drop = FALSE],
                                res$selection, dirs, h2)
  res$ecartes <- constants
  res$forces <- .hstat_gen_forces(res)
  res
}

# Forces et faiblesses : un facteur qui pese peu dans la distance a l'ideotype
# est une force du genotype, un facteur qui pese beaucoup est une faiblesse.
.hstat_gen_forces <- function(res) {
  ct <- res$contributions
  # Avec un seul facteur, force et faiblesse seraient le meme facteur.
  if (ncol(ct) < 2) return(NULL)
  fac <- res$charges
  lib <- vapply(colnames(ct), function(f) paste(fac$Caractere[fac$Facteur == f], collapse = ", "), "")
  data.frame(Genotype = rownames(ct),
             Force = paste0(colnames(ct)[apply(ct, 1, which.min)], " (",
                            lib[apply(ct, 1, which.min)], ")"),
             Faiblesse = paste0(colnames(ct)[apply(ct, 1, which.max)], " (",
                                lib[apply(ct, 1, which.max)], ")"),
             stringsAsFactors = FALSE)[order(res$indice), , drop = FALSE]
}

# Le MTSI : le meme indice, calcule sur le WAASY de chaque caractere, qui
# combine la performance et la stabilite (poids de la performance en %).
hstat_gen_mtsi <- function(ammi_liste, directions = NULL, poids_performance = 50, si = 15) {
  ammi_liste <- Filter(function(a) isTRUE(a$ok), ammi_liste)
  if (length(ammi_liste) < 2)
    return(list(ok = FALSE, motif = tr("Le MTSI demande au moins deux caractères analysés par AMMI (trois environnements ou plus, tableau complet).")))
  traits <- names(ammi_liste)
  dirs <- .hstat_gen_dir(directions, traits)
  wy <- max(0, min(100, suppressWarnings(as.numeric(poids_performance)[1]) %||% 50))
  if (!is.finite(wy)) wy <- 50
  gens <- Reduce(intersect, lapply(ammi_liste, function(a) rownames(a$Y)))
  if (length(gens) < 3)
    return(list(ok = FALSE, motif = tr("Trop peu de génotypes communs à tous les caractères.")))
  W <- vapply(traits, function(t) {
    st <- ammi_liste[[t]]$stabilite
    st <- st[match(gens, st$Genotype), ]
    ry <- .hstat_gen_reechelle(st$Moyenne, dirs[[t]])
    rw <- .hstat_gen_reechelle(st$WAAS, "diminuer")
    ry[!is.finite(ry)] <- 50; rw[!is.finite(rw)] <- 50
    (ry * wy + rw * (100 - wy)) / 100
  }, numeric(length(gens)))
  W <- matrix(W, length(gens), dimnames = list(gens, traits))
  W <- W[, colSums(!is.finite(W)) == 0, drop = FALSE]
  if (ncol(W) < 2)
    return(list(ok = FALSE, motif = tr("Le MTSI demande au moins deux caractères analysés par AMMI (trois environnements ou plus, tableau complet).")))
  traits <- colnames(W)
  res <- .hstat_gen_ideotype(W, si)
  if (!isTRUE(res$ok)) return(res)
  tab <- data.frame(Genotype = gens, MTSI = res$indice, Rang = res$rang,
                    Selectionne = gens %in% res$selection, W,
                    stringsAsFactors = FALSE, check.names = FALSE)
  names(tab)[-(1:4)] <- paste0("WAASY_", traits)
  tab <- tab[order(tab$Rang), , drop = FALSE]; rownames(tab) <- NULL
  res$table <- tab
  res$poids <- wy
  res$forces <- .hstat_gen_forces(res)
  res
}

# GYT BIPLOT (Yan & Fregeau-Reid 2018) : genotype par caractere principal *
# caractere, le principal chez les auteurs, tout caractere mesure ici. Un
# caractere a diminuer entre en principal / caractere, ce qui demande des
# valeurs strictement positives.
hstat_gen_gyt <- function(moyennes, principal, traits, directions = NULL) {
  refus <- function(m) list(ok = FALSE, motif = m)
  if (is.null(principal) || !principal %in% names(moyennes))
    return(refus(tr("Choisissez le caractère principal pour le GYT biplot.")))
  autres <- setdiff(intersect(traits, names(moyennes)), principal)
  if (!length(autres)) return(refus(tr("Le GYT biplot demande au moins un caractère en plus du caractère principal.")))
  dirs <- .hstat_gen_dir(directions, c(principal, autres))
  y <- moyennes[[principal]]
  cols <- list(); ecartes <- character(0)
  for (t in autres) {
    x <- moyennes[[t]]
    if (dirs[[t]] == "diminuer") {
      if (any(!is.finite(x) | x <= 0)) { ecartes <- c(ecartes, t); next }
      cols[[paste0(principal, "/", t)]] <- y / x
    } else cols[[paste0(principal, "*", t)]] <- y * x
  }
  if (!length(cols)) return(refus(tr("Aucune combinaison calculable : un caractère à diminuer doit être strictement positif.")))
  M <- do.call(cbind, cols); rownames(M) <- moyennes$Genotype
  M <- M[stats::complete.cases(M), , drop = FALSE]
  if (identical(dirs[[principal]], "diminuer")) M <- -M
  s <- apply(M, 2, stats::sd); s[!is.finite(s) | s == 0] <- 1
  Zs <- scale(M, center = TRUE, scale = s)
  idx <- rowMeans(Zs)
  tab <- data.frame(Genotype = rownames(M), Indice_superiorite = idx,
                    Rang = rank(-idx, ties.method = "min"), Zs,
                    stringsAsFactors = FALSE, check.names = FALSE)
  tab <- tab[order(tab$Rang), , drop = FALSE]; rownames(tab) <- NULL
  out <- list(ok = TRUE, table = tab, Z = Zs, ecartes = ecartes)
  if (ncol(Zs) >= 2 && nrow(Zs) >= 3) {
    sv <- svd(Zs)
    out$G <- sv$u[, 1:2] %*% diag(sqrt(sv$d[1:2]))
    out$C <- sv$v[, 1:2] %*% diag(sqrt(sv$d[1:2]))
    if (stats::cor(out$G[, 1], idx) < 0) { out$G[, 1] <- -out$G[, 1]; out$C[, 1] <- -out$C[, 1] }
    dimnames(out$G) <- list(rownames(Zs), c("PC1", "PC2"))
    dimnames(out$C) <- list(colnames(Zs), c("PC1", "PC2"))
    out$pct <- (100 * sv$d^2 / sum(sv$d^2))[1:2]
  }
  out
}

# COVARIABLES ENVIRONNEMENTALES
# Une covariable constante dans chaque environnement (pluviometrie, sol, annee
# climatique) se juge sur les environnements : la tester sur les parcelles
# compterait plusieurs fois le meme environnement, et ses p-values seraient
# fausses. Une covariable qui varie d'une parcelle a l'autre (pression des
# ravageurs, pratique culturale) se juge sur les parcelles, a genotype et
# environnement fixes.
.hstat_gen_par_env <- function(z, E) {
  ok <- !is.na(z)
  if (!any(ok)) return(FALSE)
  all(tapply(as.character(z[ok]), E[ok], function(v) length(unique(v)) == 1L), na.rm = TRUE)
}

hstat_gen_covariables <- function(d, trait, covariables, direction = "augmenter",
                                  alpha = 0.05) {
  covariables <- covariables[paste0(".cov_", covariables) %in% names(d)]
  if (!length(covariables)) return(NULL)
  sgn <- .hstat_gen_signe(direction)
  y <- d[[trait]]
  Y <- hstat_gen_table_ge(d, trait)
  idx_env <- colMeans(Y, na.rm = TRUE)
  lignes <- list()
  for (k in covariables) {
    z <- d[[paste0(".cov_", k)]]
    num <- is.numeric(z)
    env_niveau <- .hstat_gen_par_env(z, d$E) && nlevels(droplevels(d$E)) >= 3
    # Un ecart entre modalites n'a pas de signe : la meilleure est nommee.
    ligne <- function(niveau, beta, unite, p, n, detail = "", facteur = FALSE) {
      eff <- .hstat_gen_sens(p, if (facteur) 1 else sgn * beta, alpha)
      data.frame(Variable = k, Niveau_analyse = niveau, Effet_standardise = beta,
                 Effet_par_unite = unite, p = p, n = n, Effet = eff, Detail = detail,
                 stringsAsFactors = FALSE, check.names = FALSE)
    }
    facteur <- function(yv, zv, p, niveau) {
      m <- tapply(yv, zv, mean)
      meilleur <- names(m)[if (sgn > 0) which.max(m) else which.min(m)]
      s <- stats::sd(yv)
      ligne(niveau, if (isTRUE(s > 0)) (max(m) - min(m)) / s else NA_real_, NA_real_, p,
            length(yv), trf("Meilleure modalité : %s", meilleur), facteur = TRUE)
    }
    if (num && env_niveau) {
      ze <- tapply(z, d$E, function(v) v[!is.na(v)][1])[names(idx_env)]
      ok <- is.finite(ze) & is.finite(idx_env)
      if (sum(ok) < 3 || stats::sd(ze[ok]) == 0) next
      ct <- stats::cor.test(ze[ok], idx_env[ok])
      fit <- stats::lm(idx_env[ok] ~ ze[ok])
      lignes[[k]] <- ligne(tr("Environnements"), unname(ct$estimate),
                           unname(stats::coef(fit)[2]), ct$p.value, sum(ok),
                           trf("R² = %s sur la moyenne des environnements", format(round(ct$estimate^2, 3))))
    } else if (num) {
      dd <- data.frame(y = y, G = d$G, E = d$E, z = z)
      dd <- dd[is.finite(dd$y) & is.finite(dd$z), , drop = FALSE]
      if (nrow(dd) < 5 || stats::sd(dd$z) == 0) next
      sy <- stats::sd(dd$y); sz <- stats::sd(dd$z)
      f <- if (nlevels(droplevels(dd$E)) > 1) y ~ G + E + z else y ~ G + z
      fit <- tryCatch(stats::lm(f, data = droplevels(dd)), error = function(e) NULL)
      if (is.null(fit) || !"z" %in% names(stats::coef(fit)) || !is.finite(stats::coef(fit)[["z"]])) next
      cs <- summary(fit)$coefficients
      b <- cs["z", "Estimate"]
      lignes[[k]] <- ligne(tr("Parcelles (à génotype et environnement fixés)"),
                           b * sz / sy, b, cs["z", "Pr(>|t|)"], nrow(dd))
    } else if (env_niveau) {
      # Constante par environnement : chaque environnement compte une fois.
      ze <- tapply(as.character(z), d$E, function(v) v[!is.na(v)][1])[names(idx_env)]
      ok <- !is.na(ze) & is.finite(idx_env)
      ze <- factor(ze[ok]); ie <- idx_env[ok]
      if (nlevels(ze) < 2 || nlevels(ze) > 20) next
      p <- if (length(ie) > nlevels(ze))
        tryCatch(stats::anova(stats::lm(ie ~ ze))[1, "Pr(>F)"], error = function(e) NA_real_)
        else NA_real_
      lignes[[k]] <- facteur(ie, ze, p, tr("Environnements"))
    } else {
      dd <- data.frame(y = y, G = d$G, E = d$E, z = factor(z))
      dd <- droplevels(dd[is.finite(dd$y) & !is.na(dd$z), , drop = FALSE])
      if (nlevels(dd$z) < 2 || nlevels(dd$z) > 20) next
      f <- if (nlevels(dd$E) > 1) y ~ G + E + z else y ~ G + z
      fit <- tryCatch(stats::lm(f, data = dd), error = function(e) NULL)
      if (is.null(fit)) next
      av <- stats::anova(fit)
      p <- if ("z" %in% rownames(av)) av["z", "Pr(>F)"] else NA_real_
      lignes[[k]] <- facteur(dd$y, dd$z, p, tr("Modalités (à génotype fixé)"))
    }
  }
  if (!length(lignes)) return(NULL)
  out <- do.call(rbind, lignes); rownames(out) <- NULL
  out <- out[order(-abs(out$Effet_standardise)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# REGRESSION FACTORIELLE (Denis 1988) : l'interaction G x E expliquee par une
# covariable. Chaque genotype recoit une sensibilite, pente de son
# interaction sur la covariable centree.
hstat_gen_regression_factorielle <- function(d, trait, covariables, Y = NULL,
                                             mse = NA_real_, r = 1) {
  if (is.null(Y)) Y <- hstat_gen_table_ge(d, trait)
  if (is.null(Y) || ncol(Y) < 3 || any(!is.finite(Y))) return(NULL)
  GE <- sweep(sweep(Y, 1, rowMeans(Y)), 2, colMeans(Y)) + mean(Y)
  ss_ge <- r * sum(GE^2)
  if (!is.finite(ss_ge) || ss_ge <= 1e-12 * r * max(1, sum(Y^2))) return(NULL)
  g <- nrow(Y); e <- ncol(Y)
  tests <- list(); sens <- data.frame(Genotype = rownames(Y), stringsAsFactors = FALSE)
  for (k in covariables) {
    z <- d[[paste0(".cov_", k)]]
    if (!is.numeric(z) || !.hstat_gen_par_env(z, d$E)) next
    ze <- tapply(z, d$E, function(v) v[!is.na(v)][1])[colnames(Y)]
    if (any(!is.finite(ze)) || stats::sd(ze) == 0) next
    zc <- ze - mean(ze)
    pente <- as.numeric(GE %*% zc) / sum(zc^2)
    ss <- r * sum(pente^2) * sum(zc^2)
    df1 <- g - 1; df2 <- (g - 1) * (e - 1) - df1
    if (df2 <= 0) next
    ms_dev <- (ss_ge - ss) / df2
    Fv <- (ss / df1) / ms_dev
    tests[[k]] <- data.frame(Covariable = k, SC = ss, Pct_GE = 100 * ss / ss_ge,
                             ddl = df1, F = Fv,
                             p = stats::pf(Fv, df1, df2, lower.tail = FALSE),
                             stringsAsFactors = FALSE)
    sens[[k]] <- pente
  }
  if (!length(tests)) return(NULL)
  out <- do.call(rbind, tests); rownames(out) <- NULL
  list(tests = out[order(-out$Pct_GE), , drop = FALSE], sensibilites = sens)
}

# Les types de materiel (variete, lignee, hybride, race) compares sur leurs
# moyennes de genotypes.
hstat_gen_types <- function(d, traits) {
  if (all(is.na(d$Type))) return(NULL)
  tg <- tapply(as.character(d$Type), d$G, function(v) v[!is.na(v)][1])
  tg <- tg[!is.na(tg)]
  if (length(unique(tg)) < 2) return(NULL)
  m <- hstat_gen_moyennes(d, traits)
  m$Type <- unname(tg[m$Genotype])
  m <- m[!is.na(m$Type), , drop = FALSE]
  do.call(rbind, lapply(traits, function(t) {
    v <- m[[t]]; ok <- is.finite(v)
    if (sum(ok) < 3) return(NULL)
    agg <- tapply(v[ok], m$Type[ok], mean); n <- tapply(v[ok], m$Type[ok], length)
    p <- tryCatch(stats::anova(stats::lm(v[ok] ~ m$Type[ok]))[1, "Pr(>F)"], error = function(e) NA_real_)
    data.frame(Caractere = t, Type = names(agg), Genotypes = as.integer(n),
               Moyenne = as.numeric(agg), p_difference = p, stringsAsFactors = FALSE)
  }))
}

# Sous ce seuil, un effet direct est negligeable et le sens se lit sur la correlation.
HSTAT_GEN_PISTE_DIRECT_NEGLIGEABLE <- 0.1
HSTAT_GEN_PISTE_CORRELATION_FORTE <- 0.3

# Le sens d'un caractere associe : l'effet direct, sauf s'il est negligeable ou de
# signe contraire a une correlation forte (colinearite), ou c'est la correlation.
.hstat_gen_piste_sens <- function(d, r) {
  r_forte <- is.finite(r) && abs(r) >= HSTAT_GEN_PISTE_CORRELATION_FORTE
  faible <- !is.finite(d) || abs(d) < HSTAT_GEN_PISTE_DIRECT_NEGLIGEABLE
  contraire <- !faible && r_forte && sign(d) != sign(r)
  if (faible && !r_forte) return(NULL)
  base <- if (faible) "indirect" else if (contraire) "contraire" else "direct"
  list(base = base, valeur = if (base == "direct") d else r)
}

# SYNTHESE : ce qui fait monter le caractere recherche, et ce qui le deprecie.
hstat_gen_synthese_facteurs <- function(trait, direction = "augmenter",
                                        covariables = NULL, pistes = NULL,
                                        correlations = NULL, griffing = NULL,
                                        alpha = 0.05) {
  sgn <- .hstat_gen_signe(direction)
  out <- list()
  aj <- function(cat, el, eff, intens, mesure, preuve, lecture, rang = 1L)
    out[[length(out) + 1L]] <<- data.frame(Categorie = cat, Element = el, Effet = eff,
                                           Intensite = intens, Intensite_mesure = mesure,
                                           Preuve = preuve, Lecture = lecture,
                                           .rang = rang, stringsAsFactors = FALSE)
  if (!is.null(covariables) && nrow(covariables)) {
    for (i in seq_len(nrow(covariables))) {
      cv <- covariables[i, ]
      if (!cv$Effet %in% c(tr("Favorable"), tr("Défavorable"))) next
      lec <- if (cv$Effet == tr("Favorable"))
        trf("Une hausse de « %s » améliore « %s ».", cv$Variable, trait)
      else trf("Une hausse de « %s » déprécie « %s ».", cv$Variable, trait)
      if (nzchar(cv$Detail) && grepl(tr("Meilleure modalité"), cv$Detail, fixed = TRUE)) lec <- cv$Detail
      aj(tr("Variable environnementale ou pratique"), cv$Variable, cv$Effet,
         cv$Effet_standardise, tr("Effet standardisé de la covariable"),
         paste("p", hstat_p_egal(cv$p, alpha)), lec)
    }
  }
  if (!is.null(pistes) && isTRUE(pistes$ok)) {
    tb <- pistes$table
    m <- if (!is.null(correlations)) correlations$genotypique
    rg <- if (is.null(m)) rep(NA_real_, nrow(tb))
          else m[cbind(match(tb$Caractere, rownames(m)), match(trait, colnames(m)))]
    for (i in seq_len(nrow(tb))) {
      d <- tb$Effet_direct[i]; r <- tb$Correlation[i]; ca <- tb$Caractere[i]
      s <- .hstat_gen_piste_sens(d, r)
      if (is.null(s)) next
      fav <- sgn * s$valeur > 0
      eff <- if (fav) tr("Favorable") else tr("Défavorable")
      lec <- switch(s$base,
        direct = if (fav) trf("Effet direct : une hausse de « %s » améliore « %s ».", ca, trait)
                 else trf("Effet direct : une hausse de « %s » déprécie « %s ».", ca, trait),
        indirect = if (fav) trf("Effet surtout indirect : une hausse de « %s » accompagne une amélioration de « %s ».", ca, trait)
                   else trf("Effet surtout indirect : une hausse de « %s » accompagne une dépréciation de « %s ».", ca, trait),
        contraire = if (fav) trf("Effet direct de sens contraire à la corrélation (colinéarité) : une hausse de « %s » accompagne une amélioration de « %s ».", ca, trait)
                    else trf("Effet direct de sens contraire à la corrélation (colinéarité) : une hausse de « %s » accompagne une dépréciation de « %s ».", ca, trait))
      mes <- if (s$base == "direct") tr("Effet direct (analyse des pistes)")
             else tr("Corrélation avec le caractère principal")
      aj(tr("Caractère associé"), ca, eff, s$valeur, mes,
         trf("effet direct %s ; r = %s ; rg = %s", format(round(d, 3)),
             format(round(r, 3)), format(round(rg[i], 3))), lec,
         rang = if (s$base == "direct") 1L else 2L)
    }
  }
  if (!is.null(griffing) && isTRUE(griffing$ok)) {
    a <- griffing$agc
    for (i in seq_len(nrow(a))) {
      if (!is.finite(a$p[i]) || a$p[i] >= alpha) next
      fav <- sgn * a$AGC[i] > 0
      aj(tr("Parent (aptitude générale)"), a$Parent[i],
         if (fav) tr("Favorable") else tr("Défavorable"), a$AGC[i],
         tr("Aptitude générale à la combinaison (AGC)"),
         paste("p", hstat_p_egal(a$p[i], alpha)),
         if (fav) trf("« %s » transmet à sa descendance une valeur élevée du caractère.", a$Parent[i])
         else trf("« %s » transmet à sa descendance une valeur dépréciée du caractère.", a$Parent[i]))
    }
  }
  if (!length(out)) return(NULL)
  res <- do.call(rbind, out)
  # Une correlation ne se range pas contre un effet direct : elle vient apres.
  res <- res[order(res$Effet != tr("Favorable"), res$.rang, -abs(res$Intensite)), , drop = FALSE]
  res$.rang <- NULL
  rownames(res) <- NULL
  res
}

# Le genotype recommande par plusieurs methodes a la fois : un consensus vaut
# mieux qu'un classement isole.
hstat_gen_consensus <- function(listes) {
  listes <- Filter(function(v) length(v) > 0, listes)
  if (!length(listes)) return(NULL)
  tous <- unique(unlist(listes))
  n <- vapply(tous, function(g) sum(vapply(listes, function(v) g %in% v, logical(1))), integer(1))
  meth <- vapply(tous, function(g) paste(names(listes)[vapply(listes, function(v) g %in% v, logical(1))],
                                         collapse = ", "), "")
  out <- data.frame(Genotype = tous, Methodes = as.integer(n), Sur = length(listes),
                    Detail = meth, stringsAsFactors = FALSE)
  out <- out[order(-out$Methodes, out$Genotype), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# INTERFACE
HSTAT_GEN_GRAPHIQUES <- c(
  "AMMI1 : moyenne et premier axe d'interaction" = "ammi1",
  "AMMI2 : deux premiers axes d'interaction"     = "ammi2",
  "GGE biplot : qui gagne où (polygone)"         = "gge_polygone",
  "GGE biplot : moyenne et stabilité"            = "gge_stabilite",
  "GGE biplot : discrimination et représentativité des environnements" = "gge_env",
  "GGE biplot : vue d'ensemble"                  = "gge_base",
  "Moyennes des génotypes par environnement"     = "moyennes_env",
  "Régression de Finlay-Wilkinson (stabilité)"   = "finlay",
  "Héritabilité des caractères"                  = "heritabilite",
  "Diallèle : effets d'AGC des parents"          = "agc",
  "Diallèle : carte des ASC des croisements"     = "asc",
  "MGIDI : indice multicaractère des génotypes"  = "mgidi",
  "MGIDI : forces et faiblesses"                 = "mgidi_forces",
  "MTSI : indice de performance et de stabilité" = "mtsi",
  "GYT biplot (caractère principal × autres caractères)"           = "gyt",
  "Effets des variables environnementales"       = "covariables",
  "Sensibilité des génotypes aux covariables"    = "sensibilite",
  "Corrélations génotypiques"                    = "correlations",
  "Coefficients de piste (effets directs)"       = "pistes")

# La palette colore les groupes (types, environnements, genotypes) et les
# roles sans sens propre (genotypes, environnements, selection). Favorable,
# defavorable et non significatif portent un sens : ils gardent leurs couleurs.
HSTAT_GEN_PALETTE_ROLES <- c("genotype", "environnement", "selection")
hstat_gen_palettes_choix <- function()
  c(list("Couleurs du module" = c("Couleurs du module (défaut)" = "module")), hstat_palettes_choix())

# Les couleurs des roles : celle choisie par l'utilisateur l'emporte ; laissee
# a son defaut, elle suit la palette.
hstat_gen_couleurs_roles <- function(saisies, palette = "module") {
  C <- HSTAT_GEN_COULEURS
  for (k in names(C)) {
    v <- as.character(saisies[[k]] %||% NA_character_)[1]
    if (!is.na(v) && grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", v)) C[[k]] <- v
  }
  pal <- as.character(palette %||% "module")[1]
  if (is.na(pal) || identical(pal, "module")) return(C)
  cols <- tryCatch(hstat_palette_couleurs(pal, length(HSTAT_GEN_PALETTE_ROLES)), error = function(e) NULL)
  if (length(cols) < length(HSTAT_GEN_PALETTE_ROLES)) return(C)
  for (i in seq_along(HSTAT_GEN_PALETTE_ROLES)) {
    k <- HSTAT_GEN_PALETTE_ROLES[i]
    if (identical(toupper(C[[k]]), toupper(HSTAT_GEN_COULEURS[[k]]))) C[[k]] <- substr(cols[i], 1, 7)
  }
  C
}

# Les groupes d'une figure (couleur ou remplissage discret qu'elle ne fixe pas
# elle-meme) suivent la palette. Une echelle que la figure pose deja (code
# favorable / defavorable, degrade continu) n'est pas touchee.
hstat_gen_appliquer_palette <- function(p, palette) {
  pal <- as.character(palette %||% "module")[1]
  if (is.na(pal) || identical(pal, "module") || !inherits(p, "ggplot")) return(p)
  libres <- c("colour", "fill")[!vapply(c("colour", "fill"), function(a) p$scales$has_scale(a), logical(1))]
  if (!length(libres)) return(p)
  ech <- hstat_scales_palette(pal, fill = "fill" %in% libres, colour = "colour" %in% libres)
  if (is.null(ech)) return(p)
  q <- p + ech
  # Une esthetique continue refuse une echelle discrete : la figure reste alors telle quelle.
  ok <- tryCatch({ suppressMessages(ggplot2::ggplot_build(q)); TRUE }, error = function(e) FALSE)
  if (ok) q else p
}

# Les heritabilites que la figure peut porter.
HSTAT_GEN_HERITABILITES <- c("Les deux" = "deux", "Au sens large (H²)" = "large",
                             "Au sens strict (h²)" = "strict")

HSTAT_GEN_COULEURS <- c(genotype = "#1f6fb2", environnement = "#b03a2e",
                        favorable = "#1b7a43", defavorable = "#b03a2e",
                        neutre = "#8a94a3", selection = "#b4500b")

# Le rendu des figures : « moderne » pose un habillage de publication entre le
# theme choisi et les reglages de l'utilisateur, « classique » le theme seul.
HSTAT_GEN_STYLES <- c("Moderne (publication)" = "moderne", "Classique (thème seul)" = "classique")
.hstat_gen_habillage <- function() {
  th <- hstat_theme_moderne() + ggplot2::theme(
    plot.title = ggplot2::element_text(colour = "#1f2630", margin = ggplot2::margin(b = 4)),
    plot.subtitle = ggplot2::element_text(colour = "#5f6b7a", margin = ggplot2::margin(b = 10)),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis.text = ggplot2::element_text(colour = "#3d4652"),
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(face = "bold"),
    strip.background = ggplot2::element_rect(fill = "#eef1f5", colour = NA),
    strip.text = ggplot2::element_text(face = "bold"))
  th
}

# Les cases d'une carte, et leur texte qui passe au blanc sur une case foncee.
.hstat_gen_tuiles <- function()
  list(ggplot2::geom_tile(colour = "white", linewidth = 0.8),
       ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#1f2630"), guide = "none"),
       ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                      panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank()))

# Un sous-titre long revient a la ligne au lieu de sortir du cadre.
.hstat_gen_coupe <- function(x, largeur = 95) paste(strwrap(x, largeur), collapse = "\n")

.hstat_gen_propose <- function(cols, motif) {
  m <- cols[grepl(motif, hstat_sans_accents(tolower(cols)))]
  if (length(m)) m[1] else ""
}

mod_genetique_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    .hstat_scope_banner(exact = FALSE),
    shiny::div(class = "hstat-gen-nav",
      shiny::tabsetPanel(id = ns("genNav"), type = "pills", selected = "synthese",
        shiny::tabPanel(shiny::tagList(shiny::icon("flag-checkered"), " Synthèse et recommandations"), value = "synthese"),
        shiny::tabPanel(shiny::tagList(shiny::icon("percent"), " Variance et héritabilité"), value = "variance"),
        shiny::tabPanel(shiny::tagList(shiny::icon("layer-group"), " Comparaisons des génotypes"), value = "comparaisons"),
        shiny::tabPanel(shiny::tagList(shiny::icon("sitemap"), " Aptitudes à la combinaison (AGC, ASC)"), value = "griffing"),
        shiny::tabPanel(shiny::tagList(shiny::icon("chart-line"), " AMMI, ASV et YSI"), value = "ammi"),
        shiny::tabPanel(shiny::tagList(shiny::icon("draw-polygon"), " GGE biplot"), value = "gge"),
        shiny::tabPanel(shiny::tagList(shiny::icon("scale-balanced"), " Stabilité"), value = "stabilite"),
        shiny::tabPanel(shiny::tagList(shiny::icon("ranking-star"), " Sélection multicaractère"), value = "multicaractere"),
        shiny::tabPanel(shiny::tagList(shiny::icon("cloud-sun-rain"), " Environnement × génotype"), value = "environnement"),
        shiny::tabPanel(shiny::tagList(shiny::icon("diagram-project"), " Corrélations et pistes"), value = "correlations"),
        shiny::tabPanel(shiny::tagList(shiny::icon("chart-area"), " Graphique"), value = "graphique"))),
    shiny::fluidRow(
      shiny::column(4,
        shinydashboard::box(
          title = shiny::tagList(shiny::icon("dna"), " Données et dispositif"),
          status = "success", solidHeader = TRUE, width = 12,
          shiny::uiOutput(ns("genColsUI")),
          .hstat_opt_carte("Sens d'amélioration des caractères", "arrows-up-down", "orange",
            shiny::selectizeInput(ns("genMinimiser"), "Caractères à diminuer",
              choices = NULL, multiple = TRUE),
            shiny::tags$small(style = "color:#5f6b7a;",
              "Un rendement, une teneur ou une résistance s'augmentent ; une durée de cycle, des dégâts de ravageurs ou une incidence de maladie se diminuent. La proposition vient du nom de la colonne : vérifiez-la."),
            shiny::selectInput(ns("genPrincipal"), "Caractère principal de la sélection",
              choices = NULL),
            shiny::tags$small(style = "color:#5f6b7a;",
              "Le caractère que la sélection cherche d'abord à améliorer : rendement, précocité, résistance, qualité… N'importe quel caractère mesuré peut l'être. Les autres entrent dans la sélection multicaractère, les corrélations et les pistes.")),
          .hstat_opt_carte("Paramètres de sélection", "sliders", "violet",
            shiny::numericInput(ns("genSI"), "Intensité de sélection (% des génotypes retenus)",
              value = 15, min = 5, max = 50, step = 1),
            shiny::selectInput(ns("genAlpha"), "Seuil de signification",
              choices = c("5 %" = "0.05", "1 %" = "0.01", "10 %" = "0.1"), selected = "0.05"),
            shiny::selectInput(ns("genPlanCroisement"), "Plan de croisement (AGC, ASC)",
              choices = HSTAT_GEN_PLANS_CROISEMENT, selected = "auto"),
            shiny::numericInput(ns("genF"), "Consanguinité des parents (F, croisements)",
              value = 1, min = 0, max = 1, step = 0.25),
            shiny::sliderInput(ns("genPoidsY"), "Poids de la performance dans le MTSI (%)",
              min = 0, max = 100, value = 50, step = 5, ticks = FALSE),
            shiny::selectInput(ns("genSvp"), "Partition des valeurs singulières (GGE)",
              choices = HSTAT_GEN_GGE_SVP, selected = "symetrique"),
            shiny::selectInput(ns("genComparaison"), "Comparaison des moyennes de génotypes",
                               choices = HSTAT_GEN_COMPARAISONS, selected = "tukey"),
            shiny::checkboxInput(ns("genEchelle"),
              "Réduire chaque environnement par son écart-type (GGE)", value = FALSE)),
          shiny::actionButton(ns("genCalculer"), "Lancer l'analyse génétique",
            icon = shiny::icon("play"), class = "btn-success btn-block btn-lg"),
          shiny::uiOutput(ns("genMessage")))),

      shiny::column(8,
        shiny::conditionalPanel(
          ns = ns, condition = "output.hasGen == false",
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("circle-info"), " Ce que fait cet onglet"),
            status = "info", solidHeader = TRUE, width = 12,
            shiny::tags$p("Choisissez la colonne des génotypes (variétés, lignées, hybrides ou races), le dispositif (localités, années, répétitions), les caractères mesurés et, si vous en avez, les colonnes des parents et les variables environnementales. Une seule analyse produit :"),
            shiny::tags$ul(
              shiny::tags$li("les composantes de variance, l'héritabilité, la précision sélective et le gain génétique attendu de chaque caractère ;"),
              shiny::tags$li("les aptitudes générale (AGC) et spécifique (ASC) à la combinaison, d'un diallèle (Griffing) ou d'un plan lignées × testeurs (Kempthorne) : ratio de Baker, hétérosis et classement multicritère des croisements ;"),
              shiny::tags$li("l'interaction génotype × environnement : AMMI, ASV, YSI, WAAS, GGE biplot et indices de stabilité ;"),
              shiny::tags$li("la sélection multicaractère : MGIDI, MTSI et GYT biplot ;"),
              shiny::tags$li("l'effet des variables environnementales et des pratiques, et la sensibilité de chaque génotype à ces variables ;"),
              shiny::tags$li("une synthèse : génotypes recommandés, facteurs qui améliorent le caractère recherché et facteurs qui le déprécient.")))),

        shiny::conditionalPanel(
          ns = ns, condition = "output.hasGen",
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("dna"), " Résultats de la sélection"),
            status = "success", solidHeader = TRUE, width = 12,
            shiny::selectInput(ns("genTraitAffiche"), "Caractère affiché", choices = NULL,
                               width = "100%"),
            shiny::checkboxInput(ns("genArrondir"), "Décimales des tableaux", value = FALSE),
            shiny::conditionalPanel(
              ns = ns, condition = "input.genArrondir == true",
              shiny::numericInput(ns("genDecimales"), "Nombre de décimales :",
                value = HSTAT_GEN_DECIMALES_DEFAUT, min = 0, max = HSTAT_GEN_DECIMALES_MAX,
                step = 1, width = "220px")),
            shiny::helpText(style = "font-size: 11px; color: #5f6b7a;",
              "Si décoché, les tableaux affichent les valeurs sans arrondi."),
            shiny::tabsetPanel(id = ns("genOnglets"), type = "hidden",
              shiny::tabPanelBody("synthese",
                shiny::br(), shiny::uiOutput(ns("genSyntheseUI")),
                DT::DTOutput(ns("genConsensusTable")), shiny::tags$hr(),
                DT::DTOutput(ns("genFacteursTable"))),
              shiny::tabPanelBody("variance",
                shiny::br(), DT::DTOutput(ns("genComposantesTable")), shiny::tags$hr(),
                shiny::uiOutput(ns("genAnovaNote")), DT::DTOutput(ns("genAnovaTable")),
                shiny::tags$hr(), DT::DTOutput(ns("genValeursTable")),
                shiny::tags$hr(), DT::DTOutput(ns("genTypesTable")),
                shiny::tags$hr(),
                shiny::h4(shiny::icon("book-open"), " Lecture des métriques : seuils et signification génétique"),
                DT::DTOutput(ns("genInterpTable")),
                shiny::tags$details(
                  shiny::tags$summary(shiny::icon("table"), " Grille complète des seuils d'interprétation"),
                  DT::DTOutput(ns("genGrilleTable")))),
              shiny::tabPanelBody("comparaisons",
                shiny::br(), shiny::uiOutput(ns("genCompNote")),
                DT::DTOutput(ns("genCompTable")), shiny::tags$hr(),
                shiny::h4(shiny::icon("table-list"), " Synthèse : tous les caractères"),
                shiny::fluidRow(
                  shiny::column(5, shiny::radioButtons(ns("genSynthDisp"), "Dispersion",
                    choices = c("Écart-type" = "ecart_type", "Erreur-type" = "erreur_type"),
                    inline = TRUE)),
                  shiny::column(7, shiny::radioButtons(ns("genSynthForme"), "Présentation",
                    choices = c("Tableau" = "tableau", "Graphique (cases colorées par groupe)" = "graphique"),
                    inline = TRUE))),
                shiny::conditionalPanel(ns = ns, condition = "input.genSynthForme != 'graphique'",
                  shiny::div(class = "hstat-table-scroll", shiny::uiOutput(ns("genSynthHtml")))),
                shiny::conditionalPanel(ns = ns, condition = "input.genSynthForme == 'graphique'",
                  shiny::plotOutput(ns("genSynthPlot"), height = "auto"),
                  hstat_export_plot_ui(ns, "genSP", width = 10, height = 7, theme = FALSE)),
                shiny::helpText("Chaque case : moyenne ± dispersion et groupe ; deux génotypes qui partagent une lettre ne diffèrent pas au seuil choisi. Le meilleur génotype, selon le sens d'amélioration du caractère, porte la lettre « a ». La case prend la couleur de son groupe. Sous chaque caractère : coefficient de variation, degrés de liberté, F du test des génotypes, effectif N et probabilité P.")),
              shiny::tabPanelBody("griffing",
                shiny::br(), shiny::uiOutput(ns("genGriffingNote")),
                DT::DTOutput(ns("genGriffAnova")), shiny::tags$hr(),
                DT::DTOutput(ns("genGriffVar")), shiny::tags$hr(),
                DT::DTOutput(ns("genGriffAgc")), shiny::tags$hr(),
                DT::DTOutput(ns("genGriffAsc")), shiny::tags$hr(),
                DT::DTOutput(ns("genGriffRec")), shiny::tags$hr(),
                DT::DTOutput(ns("genClassementTable"))),
              shiny::tabPanelBody("ammi",
                shiny::br(), shiny::uiOutput(ns("genAmmiNote")),
                DT::DTOutput(ns("genAmmiAnova")), shiny::tags$hr(),
                DT::DTOutput(ns("genAmmiStab"))),
              shiny::tabPanelBody("gge",
                shiny::br(), shiny::uiOutput(ns("genGgeNote")),
                DT::DTOutput(ns("genGgeMega")), shiny::tags$hr(),
                DT::DTOutput(ns("genGgeGen")), shiny::tags$hr(),
                DT::DTOutput(ns("genGgeEnv"))),
              shiny::tabPanelBody("stabilite",
                shiny::br(), DT::DTOutput(ns("genStabTable"))),
              shiny::tabPanelBody("multicaractere",
                shiny::br(), shiny::uiOutput(ns("genMgidiNote")),
                DT::DTOutput(ns("genMgidiTable")), shiny::tags$hr(),
                DT::DTOutput(ns("genGainsTable")), shiny::tags$hr(),
                DT::DTOutput(ns("genChargesTable")), shiny::tags$hr(),
                DT::DTOutput(ns("genForcesTable")), shiny::tags$hr(),
                DT::DTOutput(ns("genMtsiTable")), shiny::tags$hr(),
                DT::DTOutput(ns("genGytTable"))),
              shiny::tabPanelBody("environnement",
                shiny::br(), shiny::uiOutput(ns("genCovNote")),
                DT::DTOutput(ns("genCovTable")), shiny::tags$hr(),
                DT::DTOutput(ns("genFrTable")), shiny::tags$hr(),
                DT::DTOutput(ns("genSensTable"))),
              shiny::tabPanelBody("correlations",
                shiny::br(), DT::DTOutput(ns("genCorGTable")), shiny::tags$hr(),
                DT::DTOutput(ns("genCorPTable")), shiny::tags$hr(),
                shiny::uiOutput(ns("genPistesNote")), DT::DTOutput(ns("genPistesTable"))),
              shiny::tabPanelBody("graphique",
                shiny::br(),
                shiny::selectInput(ns("genGraphique"), "Graphique affiché",
                                   choices = HSTAT_GEN_GRAPHIQUES, selected = "ammi1",
                                   width = "100%"),
                shiny::conditionalPanel(ns = ns, condition = "input.genGraphique == 'heritabilite'",
                  shiny::radioButtons(ns("genHeritAffiche"), "Héritabilité affichée",
                                      choices = HSTAT_GEN_HERITABILITES, selected = "deux", inline = TRUE)),
                withSpinner(shiny::plotOutput(ns("genPlot"), height = "620px"),
                            color = "#1b7a43")))),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("download"), " Téléchargements"),
            status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
            shiny::h5(shiny::icon("table"), " Tableaux"),
            shiny::downloadButton(ns("genTXlsx"), "Excel (toutes les feuilles)",
                                  class = "btn-success"),
            shiny::downloadButton(ns("genTCsv"), "CSV", class = "btn-info"),
            shiny::tags$hr(),
            shiny::h5(shiny::icon("image"), " Graphique"),
            hstat_export_plot_ui(ns, "genP", width = 9, height = 7)),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("paint-roller"), " Options du graphique"),
            status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
            collapsed = TRUE,
            shiny::fluidRow(
              shiny::column(4,
                .hstat_opt_carte("Titres et libellés", "heading", "bleu",
                  shiny::textInput(ns("genTitre"), "Titre", value = ""),
                  shiny::textInput(ns("genSousTitre"), "Sous-titre", value = ""),
                  shiny::textInput(ns("genLabX"), "Titre de l'axe X", value = ""),
                  shiny::textInput(ns("genLabY"), "Titre de l'axe Y", value = "")),
                .hstat_opt_carte("Couleurs des éléments", "fill-drip", "orange",
                  shiny::fluidRow(
                    shiny::column(6, colourInput(ns("genCoulGeno"), "Génotypes", value = HSTAT_GEN_COULEURS[["genotype"]])),
                    shiny::column(6, colourInput(ns("genCoulEnv"), "Environnements", value = HSTAT_GEN_COULEURS[["environnement"]]))),
                  shiny::fluidRow(
                    shiny::column(6, colourInput(ns("genCoulFav"), "Effet favorable", value = HSTAT_GEN_COULEURS[["favorable"]])),
                    shiny::column(6, colourInput(ns("genCoulDef"), "Effet défavorable", value = HSTAT_GEN_COULEURS[["defavorable"]]))),
                  shiny::fluidRow(
                    shiny::column(6, colourInput(ns("genCoulSel"), "Sélection et repères", value = HSTAT_GEN_COULEURS[["selection"]])),
                    shiny::column(6, colourInput(ns("genCoulNeutre"), "Non significatif", value = HSTAT_GEN_COULEURS[["neutre"]]))))),
              shiny::column(4,
                .hstat_opt_carte("Tailles", "text-height", "magenta",
                  shiny::sliderInput(ns("genTitreSize"), "Taille du titre",
                    min = 8, max = 30, value = 15, step = 1, ticks = FALSE),
                  shiny::sliderInput(ns("genEtiqSize"), "Taille des étiquettes des points",
                    min = 1, max = 8, value = 3.2, step = 0.2, ticks = FALSE),
                  shiny::sliderInput(ns("genPointSize"), "Taille des points",
                    min = 0.5, max = 8, value = 2.5, step = 0.5, ticks = FALSE),
                  shiny::sliderInput(ns("genSousTitreSize"), "Taille du sous-titre",
                    min = 6, max = 24, value = 11, step = 1, ticks = FALSE),
                  shiny::sliderInput(ns("genAxeTitreSize"), "Taille des titres d'axes",
                    min = 6, max = 24, value = 11, step = 1, ticks = FALSE),
                  shiny::sliderInput(ns("genGradSize"), "Taille des graduations",
                    min = 5, max = 20, value = 9, step = 1, ticks = FALSE),
                  shiny::sliderInput(ns("genTraitSize"), "Épaisseur des traits",
                    min = 0.1, max = 3, value = 0.5, step = 0.1, ticks = FALSE),
                  shiny::sliderInput(ns("genContour"), "Épaisseur du contour des barres",
                    min = 0, max = 3, value = 0.3, step = 0.1, ticks = FALSE))),
              shiny::column(4,
                .hstat_opt_carte("Couleurs", "palette", "sarcelle",
                  shiny::selectInput(ns("genStyle"), "Rendu des figures", choices = HSTAT_GEN_STYLES),
                  shiny::selectInput(ns("genPalette"), "Palette", choices = hstat_gen_palettes_choix(),
                    selected = "module"),
                  shiny::tags$small(style = "color:#5f6b7a;",
                    "La palette colore les génotypes, les environnements, la sélection et les groupes. Les effets favorables, défavorables et non significatifs gardent leurs couleurs propres, réglables ci-dessous."),
                  shiny::checkboxInput(ns("genEtiquettes"), "Nommer les points", value = TRUE),
                  shiny::checkboxInput(ns("genEllipses"), "Entourer chaque type de matériel", value = TRUE),
                  shiny::selectInput(ns("genLegendePos"), "Position de la légende",
                    choices = c("Droite" = "right", "Bas" = "bottom", "Haut" = "top",
                                "Gauche" = "left", "Aucune" = "none"),
                    selected = "right"),
                  shiny::selectInput(ns("genTitrePos"), "Position du titre",
                    choices = c("Centré" = "0.5", "À gauche" = "0", "À droite" = "1"),
                    selected = "0.5"),
                  shiny::sliderInput(ns("genOpacite"), "Opacité des points et des barres",
                    min = 0.1, max = 1, value = 1, step = 0.05, ticks = FALSE),
                  shiny::checkboxInput(ns("genGrilleMaj"), "Masquer la grille principale", value = FALSE),
                  shiny::checkboxInput(ns("genGrilleMin"), "Masquer la grille secondaire", value = FALSE)))),
            shiny::fluidRow(
              shiny::column(6,
                .hstat_opt_carte("Valeurs portées sur le graphique", "hashtag", "violet",
                  shiny::checkboxInput(ns("genValeurs"), "Écrire les valeurs sur les barres", value = TRUE),
                  shiny::numericInput(ns("genValeursDec"), "Décimales", value = 2, min = 0, max = 6, step = 1),
                  shiny::checkboxInput(ns("genEtiqCouleur"), "Étiquettes de la couleur de leur point", value = TRUE)))),
            hstat_plot_extras_ui(ns, "genX", familles = HSTAT_PLOT_EXTRAS_FAMILLES)))))
  )
}

# COMPARAISONS DES GENOTYPES
# Les moyennes de genotypes se comparent avec le carre moyen qui a servi a
# tester l'effet genotype (G x E dans l'analyse combinee) : c'est la variance
# d'une moyenne de genotype que porte `n_par_moyenne`.
HSTAT_GEN_COMPARAISONS <- c("Tukey (HSD)" = "tukey",
                            "Bonferroni" = "bonferroni",
                            "LSD de Fisher (non ajusté)" = "lsd")
HSTAT_GEN_LETTRES_MAX <- 150L

hstat_gen_comparaisons <- function(d, trait, an = NULL, direction = "augmenter",
                                   methode = "tukey", alpha = 0.05) {
  refus <- function(m) list(ok = FALSE, motif = m)
  if (is.null(an)) an <- hstat_gen_anova(d, trait)
  if (!isTRUE(an$ok)) return(refus(an$motif %||% tr("Analyse de variance impossible sur ce caractère.")))
  if (!methode %in% HSTAT_GEN_COMPARAISONS) methode <- "tukey"
  v <- an$n_par_moyenne; dfe <- an$df_den
  if (!isTRUE(is.finite(v) && v > 0 && is.finite(dfe) && dfe > 0))
    return(refus(tr("Sans erreur estimable, les génotypes ne se comparent pas deux à deux.")))
  y <- d[[trait]]; ok <- is.finite(y)
  g <- as.character(d$G[ok]); y <- y[ok]
  moy <- tapply(y, g, mean); et <- tapply(y, g, stats::sd); n <- tapply(y, g, length)
  k <- length(moy)
  if (k < 2) return(refus(tr("Il faut au moins deux génotypes à comparer.")))
  if (k > HSTAT_GEN_LETTRES_MAX)
    return(refus(trf("%d génotypes : au-delà de %d, les lettres de groupes ne se lisent plus. Filtrez les génotypes.",
                     k, HSTAT_GEN_LETTRES_MAX)))
  sgn <- .hstat_gen_signe(direction)
  ord <- names(moy)[order(-sgn * moy)]
  dif <- outer(moy[ord], moy[ord], "-")
  pm <- switch(methode,
    tukey = stats::ptukey(abs(dif) / sqrt(v), nmeans = k, df = dfe, lower.tail = FALSE),
    { pl <- 2 * stats::pt(-abs(dif) / sqrt(2 * v), dfe)
      if (methode == "bonferroni") pmin(1, pl * k * (k - 1) / 2) else pl })
  pm <- matrix(pm, k, k, dimnames = list(ord, ord)); diag(pm) <- 1
  lettres <- tryCatch(multcompView::multcompLetters(pm, threshold = alpha)$Letters[ord],
                      error = function(e) stats::setNames(rep(NA_character_, k), ord))
  crit <- switch(methode,
    tukey = stats::qtukey(1 - alpha, k, dfe) * sqrt(v),
    bonferroni = stats::qt(1 - alpha / (k * (k - 1)), dfe) * sqrt(2 * v),
    stats::qt(1 - alpha / 2, dfe) * sqrt(2 * v))
  tab <- data.frame(Genotype = ord, Moyenne = as.numeric(moy[ord]),
                    Ecart_type = as.numeric(et[ord]), N = as.integer(n[ord]),
                    Erreur_type_moyenne = sqrt(v), groups = unname(lettres),
                    Rang = seq_len(k), stringsAsFactors = FALSE)
  # Le format des resultats post-hoc : la synthese de l'application le lit tel quel.
  ddl <- an$anova$ddl[an$anova$Source == tr("Génotypes")][1]
  if (!is.finite(ddl)) ddl <- an$anova$ddl[grepl(tr("Génotypes"), an$anova$Source, fixed = TRUE)][1]
  bloc <- data.frame(hstat_modalite = ord, Moyenne = tab$Moyenne, Ecart_type = tab$Ecart_type,
                     Erreur_type = ifelse(tab$N > 1, tab$Ecart_type / sqrt(tab$N), NA_real_),
                     N = tab$N, groups = tab$groups, Variable = trait, Facteur = tr("Génotype"),
                     Type = "main", Symbole = "F", p_global = an$composantes$p_genotypes,
                     Statistique_globale = an$composantes$F_genotypes,
                     ddl_global = paste0(format(ddl), ", ", format(dfe)),
                     N_test = sum(tab$N), stringsAsFactors = FALSE)
  list(ok = TRUE, table = tab, bloc = bloc, methode = methode, critique = crit, pmat = pm,
       n_groupes = length(unique(unlist(strsplit(stats::na.omit(tab$groups), ""))))) 
}

# SEUILS D'INTERPRETATION ET SIGNIFICATION GENETIQUE
# Une ligne par metrique : ses bornes, la lecture de chaque classe et ce qu'elle
# dit au selectionneur, avec la reference qui fonde les bornes.
HSTAT_GEN_METRIQUES <- list(
  H2_moyenne = list(
    nom = "Héritabilité au sens large H² (base moyenne)", bornes = c(0.3, 0.6),
    classes = c("Faible (< 0,3)", "Moyenne (0,3 à 0,6)", "Élevée (> 0,6)"),
    sens = c("Le phénotype reflète surtout l'environnement : sélectionner sur la valeur observée est peu efficace ; augmenter les répétitions et les environnements, ou sélectionner sur caractères corrélés.",
             "Part génétique et part environnementale comparables : la sélection est possible mais demande des essais répétés.",
             "Le phénotype reflète bien le génotype : la sélection directe sur la valeur observée est efficace dès les premiers cycles."),
    ref = "Robinson et al. (1949) ; Johnson et al. (1955)"),
  h2_strict = list(
    nom = "Héritabilité au sens strict h² (σ²A / σ²P)", bornes = c(0.3, 0.6),
    classes = c("Faible (h² < 0,3)", "Moyenne (h² de 0,3 à 0,6)", "Élevée (h² > 0,6)"),
    sens = c("Peu de variance additive : les descendants ressemblent peu à leurs parents ; la sélection massale progresse lentement, l'hybridation ou la sélection sur descendance s'impose.",
             "Variance additive notable : la sélection sur descendance (familles, sélection récurrente) est efficace.",
             "Variance additive prépondérante : la valeur des parents se transmet, la sélection massale ou généalogique précoce est efficace."),
    ref = "Robinson et al. (1949) ; Falconer & Mackay (1996)"),
  CVg_pct = list(
    nom = "Coefficient de variation génotypique CVg (%)", bornes = c(10, 20),
    classes = c("Faible (< 10 %)", "Moyen (10 à 20 %)", "Élevé (> 20 %)"),
    sens = c("Peu de variabilité génétique disponible : élargir la base génétique (croisements, introductions) avant de sélectionner.",
             "Variabilité génétique modérée : un progrès est possible avec une intensité de sélection soutenue.",
             "Large variabilité génétique : forte marge de progrès par sélection."),
    ref = "Sivasubramanian & Madhavamenon (1973)"),
  CVe_pct = list(
    nom = "Coefficient de variation résiduel CVe (%)", bornes = c(10, 20),
    classes = c("Faible (< 10 %) : essai précis", "Moyen (10 à 20 %)", "Élevé (> 20 %) : essai peu précis"),
    sens = c("Bonne maîtrise de l'essai : les différences entre génotypes sont fiables.",
             "Précision acceptable pour la plupart des caractères agronomiques.",
             "Erreur expérimentale forte : vérifier l'hétérogénéité du terrain, augmenter les répétitions."),
    ref = "Pimentel-Gomes (1985)"),
  CVg_CVe = list(
    nom = "Rapport CVg / CVe", bornes = c(1),
    classes = c("< 1 : conditions peu favorables à la sélection", "≥ 1 : conditions favorables à la sélection"),
    sens = c("La variation génétique est noyée dans l'erreur : la sélection risque de retenir des génotypes favorisés par le hasard.",
             "La variation génétique dépasse l'erreur : situation très favorable à la sélection."),
    ref = "Vencovsky & Barriga (1992)"),
  Precision_selective = list(
    nom = "Précision sélective (√(1 − 1/F))", bornes = c(0.5, 0.7, 0.9),
    classes = c("Faible (< 0,5)", "Modérée (0,5 à 0,7)", "Élevée (0,7 à 0,9)", "Très élevée (≥ 0,9)"),
    sens = c("Classement des génotypes peu fiable : ne pas sélectionner sur cet essai seul.",
             "Classement indicatif : confirmer dans d'autres essais.",
             "Classement fiable : la sélection peut s'appuyer sur cet essai.",
             "Classement très fiable : les génotypes retenus sont réellement les meilleurs."),
    ref = "Resende & Duarte (2007)"),
  Gain_genetique_pct = list(
    nom = "Gain génétique attendu (% de la moyenne)", bornes = c(10, 20),
    classes = c("Faible (< 10 %)", "Moyen (10 à 20 %)", "Élevé (> 20 %)"),
    sens = c("Progrès faible par cycle : l'action non additive ou l'environnement dominent.",
             "Progrès modéré : sélection utile sur plusieurs cycles.",
             "Progrès important dès le premier cycle : action génique additive probable, sélection simple efficace."),
    ref = "Johnson et al. (1955)"),
  baker = list(
    nom = "Ratio de Baker 2σ²agc / (2σ²agc + σ²asc)", bornes = c(0.4, 0.7),
    classes = c("< 0,4 : non additif", "0,4 à 0,7 : additif et non additif", "> 0,7 : additif"),
    sens = c("Dominance et épistasie dominent : exploiter l'hétérosis par l'hybridation ; la sélection généalogique progresse peu.",
             "Les deux actions comptent : sélection récurrente réciproque, ou sélection tardive après fixation.",
             "Action additive prépondérante : la valeur des parents prédit celle des descendants ; la sélection précoce est efficace."),
    ref = "Baker (1978)"),
  degre_dominance = list(
    nom = "Degré moyen de dominance √(2σ²D / σ²A)", bornes = c(0.001, 1),
    classes = c("0 : pas de dominance", "0 à 1 : dominance partielle", "> 1 : superdominance"),
    sens = c("Effets purement additifs.",
             "Dominance partielle : l'hétérozygote est entre ses parents, plus près du meilleur.",
             "Superdominance : l'hybride dépasse le meilleur parent ; l'hétérosis est exploitable."),
    ref = "Comstock & Robinson (1952)"),
  b = list(
    nom = "Coefficient de régression b (Eberhart & Russell)", bornes = c(0.8, 1.2),
    classes = c("b < 1 : adaptation aux milieux défavorables", "b ≈ 1 : adaptation générale", "b > 1 : adaptation aux milieux favorables"),
    sens = c("Le génotype réagit peu à l'amélioration du milieu : sûr en conditions difficiles, mais il valorise mal les bons milieux.",
             "Le génotype suit la moyenne des milieux : adapté à une large zone.",
             "Le génotype valorise les bons milieux mais chute dans les mauvais : à réserver aux conditions intensives."),
    ref = "Finlay & Wilkinson (1963) ; Eberhart & Russell (1966)"),
  S2d = list(
    nom = "Variance des écarts à la régression S²d", bornes = c(0.000001),
    classes = c("≈ 0 : comportement prévisible", "> 0 : comportement imprévisible"),
    sens = c("La réponse du génotype aux milieux est prévisible (stabilité au sens d'Eberhart & Russell).",
             "La réponse s'écarte de la droite : le génotype est instable, son rang varie d'un milieu à l'autre."),
    ref = "Eberhart & Russell (1966)"))

.hstat_gen_classe_metrique <- function(v, m) {
  if (!is.finite(v)) return(NA_character_)
  tr(m$classes[findInterval(v, m$bornes) + 1L])
}
.hstat_gen_sens_metrique <- function(v, m) {
  if (!is.finite(v)) return(NA_character_)
  tr(m$sens[findInterval(v, m$bornes) + 1L])
}

# La grille de lecture elle-meme : une ligne par classe.
hstat_gen_grille_metriques <- function() {
  do.call(rbind, lapply(names(HSTAT_GEN_METRIQUES), function(k) {
    m <- HSTAT_GEN_METRIQUES[[k]]
    data.frame(Metrique = tr(m$nom), Classe = vapply(m$classes, tr, ""),
               Signification_genetique = vapply(m$sens, tr, ""), Reference = m$ref,
               stringsAsFactors = FALSE, row.names = NULL)
  }))
}

# La lecture des metriques d'un caractere : valeur, classe, signification.
hstat_gen_interpretation <- function(composantes, griffing = NULL) {
  out <- list()
  aj <- function(car, cle, v) {
    m <- HSTAT_GEN_METRIQUES[[cle]]
    out[[length(out) + 1L]] <<- data.frame(
      Caractere = car, Metrique = tr(m$nom), Valeur = as.numeric(v),
      Classe = .hstat_gen_classe_metrique(v, m),
      Signification_genetique = .hstat_gen_sens_metrique(v, m),
      Reference = m$ref, stringsAsFactors = FALSE)
  }
  if (!is.null(composantes) && nrow(composantes))
    for (i in seq_len(nrow(composantes))) {
      cp <- composantes[i, ]
      for (cle in c("H2_moyenne", "CVg_pct", "CVe_pct", "CVg_CVe",
                    "Precision_selective", "Gain_genetique_pct"))
        if (cle %in% names(cp)) aj(cp$Caractere, cle, cp[[cle]])
    }
  if (is.list(griffing))
    for (t in names(griffing)) {
      g <- griffing[[t]]
      if (!isTRUE(g$ok)) next
      aj(t, "h2_strict", g$h2_strict)
      aj(t, "baker", g$baker)
      vv <- g$variances
      dd <- vv$Valeur[grepl("√", vv$Parametre, fixed = TRUE)][1]
      if (length(dd) && is.finite(dd)) aj(t, "degre_dominance", dd)
    }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# La stabilite de chaque genotype, lue sur b et S2d.
hstat_gen_lecture_stabilite <- function(st) {
  if (is.null(st) || !nrow(st)) return(st)
  st$Lecture_b <- vapply(st$b, .hstat_gen_classe_metrique, "", m = HSTAT_GEN_METRIQUES$b)
  st$Lecture_S2d <- vapply(st$S2d, .hstat_gen_classe_metrique, "", m = HSTAT_GEN_METRIQUES$S2d)
  st
}

# FIGURES
# Une figure indisponible rend NULL avec son motif en attribut : le graphique
# l'affiche au lieu d'un cadre vide.
.hstat_gen_pas_dispo <- function(motif) structure(list(), class = "hstat_gen_absent", motif = motif)

.hstat_gen_etiq <- function(p, df, x, y, lab, opt, couleur = NULL, ...) {
  if (!isTRUE(opt$etiq)) return(p)
  m <- ggplot2::aes(x = .data[[x]], y = .data[[y]], label = .data[[lab]])
  args <- list(mapping = m, data = df, size = opt$taille, show.legend = FALSE, ...)
  if (!is.null(couleur) && !isFALSE(opt$etiq_coul)) args$colour <- couleur
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    args$max.overlaps <- 30; args$min.segment.length <- 0.2
    p + do.call(ggrepel::geom_text_repel, args)
  } else {
    args$vjust <- -0.8
    p + do.call(ggplot2::geom_text, args)
  }
}

# Genotypes (ronds) et environnements (triangles), cercles de blanc, et leurs
# noms. Avec un type de materiel, chaque type a sa couleur et son enveloppe
# convexe : une ellipse de dispersion sur trois ou quatre genotypes couvrirait
# tout le plan et ecraserait les points.
.hstat_gen_points_ge <- function(p, gd, ed, opt) {
  C <- opt$couleurs %||% HSTAT_GEN_COULEURS
  al <- opt$alpha %||% 1
  grp <- opt$groupes
  if (!is.null(grp)) gd$Groupe <- unname(grp[as.character(gd$nom)])
  avec_grp <- !is.null(grp) && length(unique(stats::na.omit(gd$Groupe))) >= 2
  if (avec_grp) {
    gd$Groupe[is.na(gd$Groupe)] <- tr("Type non renseigné")
    if (!isFALSE(opt$ellipses)) {
      el <- .hstat_gen_enveloppes(gd$x, gd$y, gd$Groupe)
      if (!is.null(el))
        p <- p + ggplot2::geom_polygon(data = el, ggplot2::aes(x, y, group = groupe, fill = groupe,
                                                               colour = groupe),
                                       alpha = 0.12, linewidth = 0.45, show.legend = FALSE)
    }
    p <- p + ggplot2::geom_point(data = gd, ggplot2::aes(x, y, fill = Groupe), colour = "white",
                                 shape = 21, stroke = 0.6, size = opt$point * 1.3, alpha = al) +
      ggplot2::labs(fill = tr("Type de matériel")) +
      ggplot2::guides(colour = "none",
                      fill = ggplot2::guide_legend(override.aes = list(size = opt$point * 1.6)))
  } else {
    p <- p + ggplot2::geom_point(data = gd, ggplot2::aes(x, y), fill = C[["genotype"]], colour = "white",
                                 shape = 21, stroke = 0.6, size = opt$point * 1.2, alpha = al)
  }
  p <- p + ggplot2::geom_point(data = ed, ggplot2::aes(x, y), fill = C[["environnement"]], colour = "white",
                               shape = 24, stroke = 0.6, size = opt$point * 1.1, alpha = al)
  p <- .hstat_gen_etiq(p, gd, "x", "y", "nom", opt, if (avec_grp) "#33414e" else C[["genotype"]])
  .hstat_gen_etiq(p, ed, "x", "y", "nom", opt, C[["environnement"]], fontface = "bold")
}

# L'enveloppe convexe de chaque groupe d'au moins trois points non alignes.
.hstat_gen_enveloppes <- function(x, y, g) {
  morceaux <- lapply(unique(g), function(lv) {
    i <- which(g == lv & is.finite(x) & is.finite(y))
    if (length(i) < 3L) return(NULL)
    h <- i[grDevices::chull(x[i], y[i])]
    if (length(h) < 3L) return(NULL)
    data.frame(x = x[h], y = y[h], groupe = lv, stringsAsFactors = FALSE)
  })
  morceaux <- Filter(Negate(is.null), morceaux)
  if (length(morceaux)) do.call(rbind, morceaux)
}

# Les axes d'un plan factoriel, en tirets comme ceux de l'analyse multivariee.
.hstat_gen_axes0 <- function(x0 = 0) list(
  ggplot2::geom_hline(yintercept = 0, colour = "#3a3f47", linetype = 2, linewidth = 0.4),
  ggplot2::geom_vline(xintercept = x0, colour = "#3a3f47", linetype = 2, linewidth = 0.4))

.hstat_gen_biplot <- function(G, E, pct, opt, titre, nomx = "PC1", nomy = "PC2",
                              fleches = TRUE) {
  C <- opt$couleurs %||% HSTAT_GEN_COULEURS
  lw <- opt$trait %||% 0.5
  gd <- data.frame(x = G[, 1], y = G[, 2], nom = rownames(G), stringsAsFactors = FALSE)
  ed <- data.frame(x = E[, 1], y = E[, 2], nom = rownames(E), stringsAsFactors = FALSE)
  p <- ggplot2::ggplot() + .hstat_gen_axes0()
  if (fleches)
    p <- p + ggplot2::geom_segment(data = ed, ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
      colour = C[["environnement"]], linewidth = lw * 0.8,
      arrow = grid::arrow(length = grid::unit(0.18, "cm")))
  .hstat_gen_points_ge(p, gd, ed, opt) + ggplot2::labs(title = titre,
                    x = sprintf("%s (%.1f %%)", nomx, pct[1]),
                    y = sprintf("%s (%.1f %%)", nomy, pct[2]))
}

hstat_gen_figure <- function(quoi, res, trait, opt = list(etiq = TRUE, taille = 3.2, point = 2.5)) {
  C <- opt$couleurs %||% HSTAT_GEN_COULEURS
  al <- opt$alpha %||% 1; lw <- opt$trait %||% 0.5
  # Le contour des barres : 0 l'efface.
  ct <- opt$contour %||% 0.3
  ct_col <- if (ct > 0) "black" else NA
  ct_lw <- max(ct, 0.01)
  dec <- opt$dec %||% 2
  val_txt <- function(v) formatC(v, format = "f", digits = dec)
  absent <- function(m) .hstat_gen_pas_dispo(m %||% tr("Cette figure n'est pas disponible avec les données et les colonnes choisies."))
  x <- res$par_trait[[trait]]
  am <- x$ammi; gg <- x$gge
  d0 <- res$prep$data
  if (is.null(opt$groupes) && is.data.frame(d0) && "Type" %in% names(d0) && any(!is.na(d0$Type))) {
    tg <- tapply(as.character(d0$Type), as.character(d0$G),
                 function(v) { v <- v[!is.na(v)]; if (length(v)) v[1] else NA_character_ })
    opt$groupes <- stats::setNames(as.character(tg), names(tg))
  }
  fav <- C[["favorable"]]; def <- C[["defavorable"]]
  sgn <- .hstat_gen_signe(res$dirs[[trait]])
  p <- switch(quoi,
    ammi1 = {
      if (!isTRUE(am$ok)) return(absent(am$motif))
      gd <- data.frame(x = am$moy_g, y = am$scores_g[, 1], nom = rownames(am$Y))
      ed <- data.frame(x = am$moy_e, y = am$scores_e[, 1], nom = colnames(am$Y))
      q <- ggplot2::ggplot() + .hstat_gen_axes0(mean(am$Y))
      .hstat_gen_points_ge(q, gd, ed, opt) + ggplot2::labs(title = trf("AMMI1 : « %s »", trait), x = trf("Moyenne de « %s »", trait),
                        y = sprintf("IPCA1 (%.1f %%)", am$axes$Pct_GE[1]))
    },
    ammi2 = {
      if (!isTRUE(am$ok)) return(absent(am$motif))
      if (ncol(am$scores_g) < 2) return(absent(tr("L'interaction n'a qu'un axe : AMMI2 demande deux axes.")))
      .hstat_gen_biplot(am$scores_g, am$scores_e, am$axes$Pct_GE[1:2], opt,
                        trf("AMMI2 : « %s »", trait), "IPCA1", "IPCA2")
    },
    gge_polygone = , gge_stabilite = , gge_env = , gge_base = {
      if (!isTRUE(gg$ok)) return(absent(gg$motif))
      q <- .hstat_gen_biplot(gg$G, gg$E, gg$pct, opt, trf("GGE biplot : « %s »", trait),
                             fleches = quoi %in% c("gge_base", "gge_env"))
      if (quoi == "gge_polygone") {
        h <- gg$G[gg$sommets, , drop = FALSE]
        hd <- data.frame(x = h[, 1], y = h[, 2])
        q <- q + ggplot2::geom_polygon(data = hd, ggplot2::aes(x, y), fill = NA,
                                       colour = C[["selection"]], linewidth = lw * 1.2)
        # Les rayons perpendiculaires aux cotes du polygone.
        n <- nrow(h); ray <- list()
        lim <- max(abs(c(gg$G, gg$E))) * 1.15
        for (i in seq_len(n)) {
          a <- h[i, ]; b <- h[if (i == n) 1 else i + 1, ]
          dvec <- b - a; nn <- c(-dvec[2], dvec[1])
          if (sum(nn^2) == 0) next
          if (sum(nn * ((a + b) / 2)) < 0) nn <- -nn
          nn <- nn / sqrt(sum(nn^2)) * lim
          ray[[length(ray) + 1]] <- data.frame(xend = nn[1], yend = nn[2])
        }
        if (length(ray))
          q <- q + ggplot2::geom_segment(data = do.call(rbind, ray),
                     ggplot2::aes(x = 0, y = 0, xend = xend, yend = yend),
                     colour = C[["selection"]], linetype = 2, linewidth = lw * 0.8)
        q <- q + ggplot2::labs(subtitle = tr("Qui gagne où : le génotype au sommet de chaque secteur gagne dans ses environnements."))
      } else if (quoi == "gge_stabilite") {
        u <- gg$aec; L <- max(abs(gg$G)) * 1.2
        q <- q + ggplot2::annotate("segment", x = -L * u[1], y = -L * u[2], xend = L * u[1], yend = L * u[2],
                                   colour = C[["selection"]], linewidth = lw * 1.2,
                                   arrow = grid::arrow(length = grid::unit(0.25, "cm")))
        pr <- as.numeric(gg$G %*% u)
        pied <- data.frame(x = gg$G[, 1], y = gg$G[, 2], xend = pr * u[1], yend = pr * u[2])
        q <- q + ggplot2::geom_segment(data = pied, ggplot2::aes(x, y, xend = xend, yend = yend),
                                       colour = "#8a94a3", linetype = 3, linewidth = 0.3) +
          ggplot2::labs(subtitle = tr("Axe de l'environnement moyen : la projection donne la moyenne, la distance à l'axe l'instabilité."))
      } else if (quoi == "gge_env") {
        u <- gg$aec; L <- max(sqrt(rowSums(gg$E^2))) * 1.1
        q <- q + ggplot2::annotate("segment", x = 0, y = 0, xend = L * u[1], yend = L * u[2],
                                   colour = C[["selection"]], linewidth = lw * 1.4,
                                   arrow = grid::arrow(length = grid::unit(0.25, "cm"))) +
          ggplot2::labs(subtitle = tr("Une flèche longue discrimine, un angle faible avec l'axe moyen représente."))
      }
      q + ggplot2::coord_equal()
    },
    moyennes_env = {
      Y <- hstat_gen_table_ge(res$prep$data, trait)
      if (is.null(Y) || ncol(Y) < 2) return(absent(tr("Cette figure demande au moins deux environnements.")))
      dd <- data.frame(Genotype = rep(rownames(Y), ncol(Y)), Environnement = rep(colnames(Y), each = nrow(Y)),
                       Valeur = as.numeric(Y))
      dd$Environnement <- factor(dd$Environnement, levels = names(sort(colMeans(Y, na.rm = TRUE))))
      ggplot2::ggplot(dd, ggplot2::aes(Environnement, Valeur, colour = Genotype, group = Genotype)) +
        ggplot2::geom_line(linewidth = lw, na.rm = TRUE) + ggplot2::geom_point(size = opt$point, alpha = al, na.rm = TRUE) +
        ggplot2::labs(title = trf("« %s » par environnement", trait), y = trait,
                      x = tr("Environnements (rangés par moyenne croissante)"), colour = tr("Génotype")) +
        (if (sum(nchar(levels(dd$Environnement))) > 40)
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)))
    },
    finlay = {
      s <- x$stab
      if (is.null(s)) return(absent(tr("La stabilité demande au moins trois environnements et un tableau complet.")))
      Y <- am$Y %||% hstat_gen_table_ge(res$prep$data, trait)
      idx <- s$indice$Indice; names(idx) <- s$indice$Environnement
      dd <- data.frame(Genotype = rep(rownames(Y), ncol(Y)), Indice = rep(idx[colnames(Y)], each = nrow(Y)),
                       Valeur = as.numeric(Y))
      ggplot2::ggplot(dd, ggplot2::aes(Indice, Valeur, colour = Genotype)) +
        ggplot2::geom_point(size = opt$point * 0.8, alpha = al) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = lw * 1.2) +
        ggplot2::labs(title = trf("Régression de Finlay-Wilkinson : « %s »", trait),
                      x = tr("Indice environnemental (moyenne de l'environnement − moyenne générale)"),
                      y = trait, colour = tr("Génotype"))
    },
    heritabilite = {
      cp <- res$composantes
      if (is.null(cp) || !nrow(cp)) return(absent(tr("Aucune héritabilité calculable.")))
      quoi_h <- opt$heritabilite %||% "deux"
      if (!quoi_h %in% HSTAT_GEN_HERITABILITES) quoi_h <- "deux"
      a_h2 <- "h2_strict" %in% names(cp) && any(is.finite(cp$h2_strict))
      manque_h2 <- tr("L'héritabilité au sens strict demande un plan de croisement (diallèle ou lignées × testeurs) : indiquez les colonnes Parent 1 et Parent 2.")
      if (quoi_h == "strict" && !a_h2) return(absent(manque_h2))
      lv <- cp$Caractere[order(cp$H2_moyenne)]
      bornes <- ggplot2::geom_hline(yintercept = HSTAT_GEN_GRILLES$H2$bornes, linetype = 2, colour = "#5f6b7a")
      echelle_y <- ggplot2::scale_y_continuous(limits = c(0, 1.1), expand = c(0, 0))
      # Une seule heritabilite : ses barres prennent la couleur de leur classe.
      une <- function(v, classes, libelles, titre, axe, sous_titre = NULL) {
        dd <- data.frame(Caractere = factor(cp$Caractere, levels = lv), Valeur = v, Classe = classes)
        dd <- dd[is.finite(dd$Valeur), , drop = FALSE]
        ggplot2::ggplot(dd, ggplot2::aes(Caractere, Valeur, fill = Classe)) +
          ggplot2::geom_col(width = 0.7, alpha = al, colour = ct_col, linewidth = ct_lw) +
          bornes +
          (if (!isFALSE(opt$valeurs)) ggplot2::geom_text(ggplot2::aes(label = val_txt(Valeur)), hjust = -0.15, size = opt$taille)) +
          ggplot2::scale_fill_manual(values = stats::setNames(c(def, "#c98a12", fav), libelles),
                                     breaks = libelles, drop = TRUE) +
          ggplot2::coord_flip() + echelle_y +
          ggplot2::labs(title = titre, subtitle = sous_titre, x = NULL, y = axe, fill = tr("Classe"))
      }
      if (quoi_h == "large" || (quoi_h == "deux" && !a_h2)) {
        une(cp$H2_moyenne, cp$Classe_H2, .hstat_gen_libelles_h2(),
            tr("Héritabilité au sens large (base moyenne)"), "H²",
            if (!a_h2) manque_h2)
      } else if (quoi_h == "strict") {
        une(cp$h2_strict, cp$Classe_h2, vapply(HSTAT_GEN_METRIQUES$h2_strict$classes, tr, ""),
            tr("Héritabilité au sens strict (croisements)"), "h²",
            tr("h² = σ²A / σ²P : la part additive, celle qui se transmet des parents aux descendants."))
      } else {
        # Les deux heritabilites cote a cote : la part additive (h²) dans la
        # part genetique totale (H²).
        n_l <- tr("H² au sens large (base moyenne)"); n_s <- tr("h² au sens strict (croisements)")
        dd <- rbind(data.frame(Caractere = cp$Caractere, Mesure = n_l, Valeur = cp$H2_moyenne),
                    data.frame(Caractere = cp$Caractere, Mesure = n_s, Valeur = cp$h2_strict))
        dd <- dd[is.finite(dd$Valeur), , drop = FALSE]
        dd$Caractere <- factor(dd$Caractere, levels = lv)
        # Retourne par coord_flip : H² au-dessus de h² dans chaque paire.
        dd$Mesure <- factor(dd$Mesure, levels = c(n_s, n_l))
        pos <- ggplot2::position_dodge(width = 0.8)
        ggplot2::ggplot(dd, ggplot2::aes(Caractere, Valeur, fill = Mesure)) +
          ggplot2::geom_col(position = pos, width = 0.75, alpha = al, colour = ct_col, linewidth = ct_lw) +
          bornes +
          (if (!isFALSE(opt$valeurs)) ggplot2::geom_text(ggplot2::aes(label = val_txt(Valeur)), position = pos,
                                                         hjust = -0.15, size = opt$taille)) +
          ggplot2::scale_fill_manual(values = stats::setNames(c(C[["genotype"]], C[["selection"]]), c(n_l, n_s)),
                                     breaks = c(n_l, n_s)) +
          ggplot2::coord_flip() + echelle_y +
          ggplot2::labs(title = tr("Héritabilité au sens large et au sens strict"),
                        subtitle = tr("h² = σ²A / σ²P : la part additive, celle qui se transmet des parents aux descendants."),
                        x = NULL, y = "H², h²", fill = NULL)
      }
    },
    agc = {
      g <- x$griffing
      if (!isTRUE(g$ok)) return(absent(g$motif %||% tr("Indiquez les colonnes Parent 1 et Parent 2 pour estimer les aptitudes à la combinaison : diallèle (parents et F1, avec ou sans réciproques) ou plan lignées × testeurs.")))
      a <- g$agc; a$Bon <- sgn * a$AGC > 0
      a$Parent <- factor(a$Parent, levels = a$Parent[order(sgn * a$AGC)])
      # Lignees et testeurs ne se comparent pas : chacun son panneau.
      roles <- if (!is.null(a$Role))
        ggplot2::facet_grid(Role ~ ., scales = "free_y", space = "free_y")
      ggplot2::ggplot(a, ggplot2::aes(Parent, AGC, fill = Bon)) + roles +
        ggplot2::geom_col(width = 0.7, alpha = al, colour = ct_col, linewidth = ct_lw) +
        # La moitie exterieure de la barre d'erreur : celle qui s'eloigne de zero.
        ggplot2::geom_errorbar(ggplot2::aes(ymin = AGC, ymax = AGC + sign(AGC) * Erreur_type),
                               width = 0.25, linewidth = 0.4) +
        ggplot2::geom_hline(yintercept = 0) +
        (if (!isFALSE(opt$valeurs)) ggplot2::geom_text(ggplot2::aes(label = val_txt(AGC),
           y = AGC + sign(AGC) * Erreur_type, hjust = ifelse(AGC >= 0, -0.2, 1.2)), size = opt$taille)) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
        ggplot2::scale_fill_manual(values = c(`TRUE` = fav, `FALSE` = def),
                                   labels = c(`TRUE` = tr("Favorable"), `FALSE` = tr("Défavorable"))) +
        ggplot2::coord_flip() +
        ggplot2::labs(title = trf("Aptitude générale à la combinaison : « %s »", trait),
                      x = NULL, y = tr("Effet d'AGC (± erreur-type)"), fill = NULL)
    },
    asc = {
      g <- x$griffing
      if (!isTRUE(g$ok)) return(absent(g$motif %||% tr("Indiquez les colonnes Parent 1 et Parent 2 pour estimer les aptitudes à la combinaison : diallèle (parents et F1, avec ou sans réciproques) ou plan lignées × testeurs.")))
      cr <- g$croisements
      lt <- identical(g$plan, "ligne_testeur")
      dd <- if (lt) data.frame(P1 = cr$Parent_1, P2 = cr$Parent_2, ASC = cr$ASC)
            else rbind(data.frame(P1 = cr$Parent_1, P2 = cr$Parent_2, ASC = cr$ASC),
                       data.frame(P1 = cr$Parent_2, P2 = cr$Parent_1, ASC = cr$ASC))
      dd <- dd[dd$P1 != dd$P2, , drop = FALSE]
      dd$P1 <- factor(dd$P1, levels = if (lt) g$lignees else g$parents)
      dd$P2 <- factor(dd$P2, levels = rev(if (lt) g$testeurs else g$parents))
      lim <- max(abs(dd$ASC), na.rm = TRUE)
      ggplot2::ggplot(dd, ggplot2::aes(P1, P2, fill = sgn * ASC)) +
        .hstat_gen_tuiles() +
        ggplot2::geom_text(ggplot2::aes(label = if (!isFALSE(opt$valeurs)) val_txt(ASC) else "",
                                        colour = abs(ASC) > 0.6 * lim), size = opt$taille, show.legend = FALSE) +

        ggplot2::coord_equal() +
        ggplot2::scale_fill_gradient2(low = def, mid = "#f7f7f7", high = fav, limits = c(-lim, lim),
                                      name = tr("ASC favorable")) +
        ggplot2::labs(title = trf("Aptitude spécifique des croisements : « %s »", trait),
                      x = if (lt) tr("Lignée (Parent 1)") else tr("Parent"),
                      y = if (lt) tr("Testeur (Parent 2)") else tr("Parent"))
    },
    mgidi = , mtsi = {
      m <- if (quoi == "mgidi") res$mgidi else res$mtsi
      if (!isTRUE(m$ok)) return(absent(m$motif))
      t <- m$table; col <- if (quoi == "mgidi") "MGIDI" else "MTSI"
      t$Genotype <- factor(t$Genotype, levels = rev(t$Genotype))
      seuil <- sort(t[[col]])[m$n_sel]
      ggplot2::ggplot(t, ggplot2::aes(.data[[col]], Genotype, colour = Selectionne)) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data[[col]], yend = Genotype), linewidth = lw) +
        ggplot2::geom_point(size = opt$point * 1.6, alpha = al) +
        ggplot2::geom_point(size = opt$point * 0.6, colour = "white") +
        ggplot2::geom_vline(xintercept = seuil, linetype = 2, colour = C[["selection"]]) +
        ggplot2::scale_colour_manual(values = c(`TRUE` = C[["selection"]], `FALSE` = C[["neutre"]]),
                                     labels = c(`TRUE` = tr("Sélectionné"), `FALSE` = tr("Non sélectionné"))) +
        ggplot2::labs(title = if (quoi == "mgidi") tr("MGIDI : distance à l'idéotype (plus petit = meilleur)")
                              else tr("MTSI : performance et stabilité (plus petit = meilleur)"),
                      x = col, y = NULL, colour = NULL)
    },
    mgidi_forces = {
      m <- res$mgidi
      if (!isTRUE(m$ok)) return(absent(m$motif))
      ct <- m$contributions
      if (ncol(ct) < 2) return(absent(tr("Un seul facteur : les forces et faiblesses ne se distinguent pas.")))
      gens <- m$table$Genotype
      dd <- data.frame(Genotype = rep(rownames(ct), ncol(ct)), Facteur = rep(colnames(ct), each = nrow(ct)),
                       Part = 100 * as.numeric(ct))
      dd$Genotype <- factor(dd$Genotype, levels = rev(gens))
      ggplot2::ggplot(dd, ggplot2::aes(Part, Genotype, fill = Facteur)) +
        ggplot2::geom_col(width = 0.75, alpha = al, colour = ct_col, linewidth = ct_lw) +
        ggplot2::labs(title = tr("Forces et faiblesses : part de chaque facteur dans la distance à l'idéotype"),
                      subtitle = tr("Une petite part est une force, une grande part une faiblesse."),
                      x = tr("Contribution (%)"), y = NULL, fill = tr("Facteur"))
    },
    gyt = {
      m <- res$gyt
      if (!isTRUE(m$ok)) return(absent(m$motif))
      if (is.null(m$G)) return(absent(tr("Le GYT biplot demande au moins deux combinaisons caractère principal × caractère.")))
      .hstat_gen_biplot(m$G, m$C, m$pct, opt, trf("GYT biplot : caractère principal « %s » × autres caractères", res$principal)) +
        ggplot2::coord_equal()
    },
    covariables = {
      cv <- x$cov
      if (is.null(cv) || !nrow(cv)) return(absent(tr("Aucune covariable exploitable pour ce caractère.")))
      cv$Variable <- factor(cv$Variable, levels = cv$Variable[order(sgn * cv$Effet_standardise)])
      ggplot2::ggplot(cv, ggplot2::aes(Effet_standardise, Variable, fill = Effet)) +
        ggplot2::geom_col(width = 0.7, alpha = al, colour = ct_col, linewidth = ct_lw) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::geom_text(ggplot2::aes(label = paste("p", vapply(p, hstat_p_egal, character(1), seuil = res$alpha %||% 0.05)),
                                        hjust = ifelse(Effet_standardise >= 0, -0.1, 1.1)), size = opt$taille * 0.9) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.25)) +
        ggplot2::scale_fill_manual(values = stats::setNames(c(fav, def, C[["neutre"]], C[["neutre"]]),
                                   c(tr("Favorable"), tr("Défavorable"), tr("Non significatif"), tr("Non calculable"))),
                                   na.value = C[["neutre"]], drop = TRUE) +
        ggplot2::labs(title = trf("Ce qui améliore ou déprécie « %s »", trait),
                      x = tr("Effet standardisé (écarts-types du caractère par écart-type de la variable)"),
                      y = NULL, fill = NULL)
    },
    sensibilite = {
      fr <- x$fr
      if (is.null(fr)) return(absent(tr("La régression factorielle demande au moins trois environnements, un tableau complet et une covariable constante par environnement.")))
      sv <- fr$sensibilites
      ks <- setdiff(names(sv), "Genotype")
      dd <- data.frame(Genotype = rep(sv$Genotype, length(ks)), Covariable = rep(ks, each = nrow(sv)),
                       Pente = unlist(sv[ks], use.names = FALSE))
      # Chaque covariable a son unite : la couleur se lit relativement a la
      # plus forte pente de sa colonne, le chiffre reste la pente.
      dd$Relatif <- stats::ave(dd$Pente, dd$Covariable, FUN = function(v) {
        m <- max(abs(v), na.rm = TRUE); if (is.finite(m) && m > 0) v / m else v * 0 })
      ggplot2::ggplot(dd, ggplot2::aes(Covariable, Genotype, fill = sgn * Relatif)) +
        .hstat_gen_tuiles() +
        ggplot2::geom_text(ggplot2::aes(label = signif(Pente, 2),
                                        colour = abs(Relatif) > 0.6), size = opt$taille * 0.9,
                           show.legend = FALSE) +

        ggplot2::scale_fill_gradient2(low = def, mid = "#f7f7f7", high = fav, limits = c(-1, 1),
                                      name = tr("Réponse favorable (relative)")) +
        ggplot2::labs(title = trf("Sensibilité des génotypes aux covariables : « %s »", trait),
                      x = NULL, y = NULL)
    },
    correlations = {
      cr <- res$correlations
      if (is.null(cr)) return(absent(tr("Les corrélations demandent au moins deux caractères.")))
      m <- cr$genotypique
      dd <- data.frame(A = rep(rownames(m), ncol(m)), B = rep(colnames(m), each = nrow(m)), r = as.numeric(m))
      # Le triangle inferieur suffit : la matrice est symetrique et la
      # diagonale vaut 1. Une estimation au-dela de 1 garde son chiffre.
      dd <- dd[match(dd$A, rownames(m)) > match(dd$B, colnames(m)), , drop = FALSE]
      dd$A <- factor(dd$A, levels = rownames(m)); dd$B <- factor(dd$B, levels = rev(colnames(m)))
      ggplot2::ggplot(dd, ggplot2::aes(A, B, fill = r)) +
        .hstat_gen_tuiles() +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(is.finite(r) & !isFALSE(opt$valeurs), val_txt(r), ""),
                                        colour = is.finite(r) & abs(r) > 0.6),
                           size = opt$taille, show.legend = FALSE) +

        ggplot2::scale_fill_gradient2(low = "#b03a2e", mid = "#f7f7f7", high = "#1f6fb2",
                                      limits = c(-1, 1), oob = scales::squish,
                                      na.value = "#e5e7eb", name = "rg") +
        ggplot2::coord_equal() +
        ggplot2::labs(title = tr("Corrélations génotypiques"), x = NULL, y = NULL) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    },
    pistes = {
      pi <- res$pistes
      if (!isTRUE(pi$ok)) return(absent(pi$motif))
      tb <- pi$table
      dd <- rbind(data.frame(Caractere = tb$Caractere, Type = tr("Effet direct"), Valeur = tb$Effet_direct),
                  data.frame(Caractere = tb$Caractere, Type = tr("Corrélation totale"), Valeur = tb$Correlation))
      dd$Caractere <- factor(dd$Caractere, levels = tb$Caractere[order(tb$Effet_direct)])
      ggplot2::ggplot(dd, ggplot2::aes(Valeur, Caractere, fill = Type)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7, alpha = al,
                          colour = ct_col, linewidth = ct_lw) +
        ggplot2::geom_vline(xintercept = 0) +
        (if (!isFALSE(opt$valeurs)) ggplot2::geom_text(ggplot2::aes(label = val_txt(Valeur),
           hjust = ifelse(Valeur >= 0, -0.15, 1.15)), position = ggplot2::position_dodge(width = 0.75),
           size = opt$taille * 0.9)) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
        ggplot2::scale_fill_manual(values = c(C[["genotype"]], C[["selection"]])) +
        ggplot2::labs(title = trf("Coefficients de piste sur « %s »", pi$cible),
                      subtitle = trf("Effet résiduel : %s", format(round(pi$residuel, 3))),
                      x = NULL, y = NULL, fill = NULL)
    },
    absent(NULL))
  if (inherits(p, "ggplot") && is.character(p$labels$subtitle))
    p$labels$subtitle <- .hstat_gen_coupe(p$labels$subtitle)
  p
}

# Le nombre de decimales des tableaux, un seul reglage pour tout le module.
HSTAT_GEN_DECIMALES_DEFAUT <- 3L
HSTAT_GEN_DECIMALES_MAX <- 8L
# Decoche : NA, les tableaux portent les valeurs sans arrondi.
hstat_gen_decimales <- function(arrondir, x) {
  if (!isTRUE(arrondir)) return(NA_integer_)
  hstat_borne_client(x, HSTAT_GEN_DECIMALES_DEFAUT, 0, HSTAT_GEN_DECIMALES_MAX)
}

# Un tableau affiche : les p-values en clair, le reste arrondi sur demande, les comptages
# sans decimales.
.hstat_gen_dt <- function(df, titre, digits = NA_integer_) {
  if (is.null(df) || !nrow(df)) return(NULL)
  aff <- df
  for (k in names(aff)[grepl("^p$|^p_|_p$", names(aff))])
    if (is.numeric(aff[[k]])) aff[[k]] <- hstat_p_txt(aff[[k]])
  d <- DT::datatable(aff, rownames = FALSE, filter = "top", extensions = "Buttons",
    options = list(pageLength = 12, scrollX = TRUE, dom = "Bfrtip",
                   buttons = .hstat_dt_buttons(titre)),
    caption = htmltools::tags$caption(style = "caption-side: top; font-weight: 600;", titre))
  if (is.na(digits)) d else hstat_dt_arrondi(d, aff, digits)
}

# Une matrice de correlations en tableau.
.hstat_gen_mat_df <- function(m, nom = "Caractere") {
  if (is.null(m)) return(NULL)
  out <- data.frame(x = rownames(m), m, check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[1] <- nom
  rownames(out) <- NULL
  out
}

# Les tableaux par caractere, empiles avec une colonne « Caractere ».
.hstat_gen_empile <- function(par_trait, f) {
  l <- lapply(names(par_trait), function(t) {
    x <- tryCatch(f(par_trait[[t]]), error = function(e) NULL)
    if (is.null(x) || !is.data.frame(x) || !nrow(x)) return(NULL)
    cbind(data.frame(Caractere = t, stringsAsFactors = FALSE), x)
  })
  l <- Filter(Negate(is.null), l)
  if (!length(l)) return(NULL)
  noms <- unique(unlist(lapply(l, names)))
  l <- lapply(l, function(x) { for (k in setdiff(noms, names(x))) x[[k]] <- NA; x[, noms] })
  out <- do.call(rbind, l); rownames(out) <- NULL
  out
}

HSTAT_GEN_CORREL_MAX <- 15L

mod_genetique_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    # Tous les tableaux du module lisent le meme reglage de decimales.
    gen_dt <- function(df, titre) .hstat_gen_dt(df, titre, hstat_gen_decimales(input$genArrondir, input$genDecimales))
    ns <- session$ns
    rv <- shiny::reactiveValues(res = NULL, message = NULL)

    donnees <- shiny::reactive({
      values$filteredData %||% values$cleanData %||% values$data
    })

    shiny::observeEvent(values$resetSignal, {
      rv$res <- NULL; rv$message <- NULL
    }, ignoreInit = TRUE)

    # La barre des analyses, au-dessus des deux colonnes, choisit le panneau
    # affiche dans les resultats.
    shiny::observeEvent(input$genNav, {
      shiny::updateTabsetPanel(session, "genOnglets", selected = input$genNav)
    })

    # SELECTEURS : une colonne par role, proposee d'apres son nom.
    output$genColsUI <- shiny::renderUI({
      df <- donnees()
      if (is.null(df) || !ncol(df))
        return(shiny::tags$em(style = "color:#5f6b7a;",
          "Chargez d'abord un jeu de données dans l'onglet Chargement."))
      cols <- names(df)
      aucun <- c("(aucune)" = "")
      pg <- .hstat_gen_propose(cols, "variet|genotyp|lignee|ligne|hybrid|race|cultivar|accession|entree|entry|clone")
      pl <- .hstat_gen_propose(cols, "^loc|localit|site|lieu|station|village|zone")
      pa <- .hstat_gen_propose(cols, "annee|year|campagne|saison")
      pr <- .hstat_gen_propose(cols, "^rep|repet|bloc|block")
      p1 <- .hstat_gen_propose(cols, "parent.?1|^p1$|mere|femelle|female")
      p2 <- .hstat_gen_propose(cols, "parent.?2|^p2$|pere|male$|^male")
      pt <- .hstat_gen_propose(cols, "^type|materiel|statut")
      motif_cov <- "pluvio|precip|rain|temp|humid|^hr|sol|argile|sable|limon|^ph$|_ph|azote|pression|irrig|fertil|labour|semis|densite"
      utilises <- c(pg, pl, pa, pr, p1, p2, pt)
      num <- cols[vapply(df, function(v) is.numeric(v) || !is.null(hstat_as_numeric_fr(v)), logical(1))]
      covp <- setdiff(cols[grepl(motif_cov, hstat_sans_accents(tolower(cols)))], utilises)
      trp <- setdiff(num, c(utilises, covp))
      shiny::tagList(
        .hstat_opt_carte("Matériel génétique", "seedling", "vert",
          shiny::selectInput(ns("genGen"), "Génotypes (variétés, lignées, hybrides ou races)",
            choices = cols, selected = if (nzchar(pg)) pg else cols[1]),
          shiny::selectInput(ns("genType"), "Type de matériel (facultatif)",
            choices = c(aucun, cols), selected = pt),
          shiny::selectInput(ns("genGroupe"), "Espèce ou race (facultatif)",
            choices = c(aucun, cols), selected = ""),
          shiny::uiOutput(ns("genGroupeUI")),
          shiny::fluidRow(
            shiny::column(6, shiny::selectInput(ns("genP1"), "Parent 1 (femelle, lignée)",
              choices = c(aucun, cols), selected = p1)),
            shiny::column(6, shiny::selectInput(ns("genP2"), "Parent 2 (mâle, testeur)",
              choices = c(aucun, cols), selected = p2))),
          shiny::uiOutput(ns("genTemoinUI"))),
        .hstat_opt_carte("Dispositif et environnements", "map-location-dot", "bleu",
          shiny::selectInput(ns("genLoc"), "Localité (facultatif)", choices = c(aucun, cols), selected = pl),
          shiny::selectInput(ns("genAnnee"), "Année ou campagne (facultatif)", choices = c(aucun, cols), selected = pa),
          shiny::selectInput(ns("genRep"), "Répétition ou bloc (facultatif)", choices = c(aucun, cols), selected = pr),
          shiny::tags$small(style = "color:#5f6b7a;",
            "Un environnement est le croisement d'une localité et d'une année. AMMI, GGE et la stabilité demandent au moins trois environnements.")),
        .hstat_opt_carte("Caractères mesurés", "ruler", "ocre",
          shiny::selectizeInput(ns("genTraits"),
            "Caractères (rendement, précocité, résistance, qualité, morphologie…)",
            choices = num, selected = utils::head(trp, 8), multiple = TRUE)),
        .hstat_opt_carte("Variables environnementales et pratiques", "cloud-sun-rain", "sarcelle",
          shiny::selectizeInput(ns("genCov"),
            "Covariables (pluviométrie, température, humidité, sol, ravageurs, pratiques)",
            choices = setdiff(cols, utilises), selected = covp, multiple = TRUE),
          shiny::tags$small(style = "color:#5f6b7a;",
            "Une covariable constante dans chaque environnement est jugée sur les environnements ; une covariable qui varie d'une parcelle à l'autre est jugée sur les parcelles, à génotype et environnement fixés.")))
    })

    output$genGroupeUI <- shiny::renderUI({
      df <- donnees(); v <- input$genGroupe
      if (is.null(df) || is.null(v) || !nzchar(v) || !v %in% names(df)) return(NULL)
      niv <- utils::head(sort(unique(stats::na.omit(as.character(df[[v]])))), 200)
      shiny::selectInput(ns("genGroupeNiveau"), "Espèce ou race analysée", choices = niv)
    })

    output$genTemoinUI <- shiny::renderUI({
      df <- donnees(); v <- input$genGen
      if (is.null(df) || is.null(v) || !v %in% names(df)) return(NULL)
      niv <- utils::head(sort(unique(stats::na.omit(as.character(df[[v]])))), HSTAT_GEN_GENOTYPES_MAX)
      shiny::selectInput(ns("genTemoin"), "Variété témoin (hétérosis standard, facultatif)",
                         choices = c("(aucune)" = "", niv))
    })

    shiny::observeEvent(input$genTraits, {
      t <- as.character(input$genTraits %||% character(0))
      prop <- t[hstat_gen_direction_proposee(t) == "diminuer"]
      shiny::updateSelectizeInput(session, "genMinimiser", choices = t,
        selected = hstat_garder_choix(input$genMinimiser, t, prop))
      pp <- .hstat_gen_propose(t, "rend|yield|product|poids|kg|t_ha|graine")
      shiny::updateSelectInput(session, "genPrincipal", choices = t,
        selected = hstat_garder_choix(input$genPrincipal, t, if (nzchar(pp)) pp else t[1]))
    }, ignoreNULL = FALSE)

    # CALCUL
    shiny::observeEvent(input$genCalculer, {
      df <- donnees()
      if (is.null(df)) {
        shiny::showNotification(tr("Chargez d'abord un jeu de données dans l'onglet Chargement."),
                                type = "error", duration = 5)
        return()
      }
      val <- function(x) { v <- as.character(x %||% ""); v <- v[nzchar(v)]; if (length(v)) v[1] else NULL }
      pr <- hstat_gen_preparer(df, val(input$genGen), input$genTraits,
        var_loc = val(input$genLoc), var_annee = val(input$genAnnee),
        var_rep = val(input$genRep), var_groupe = val(input$genGroupe),
        groupe = val(input$genGroupeNiveau), var_type = val(input$genType),
        var_p1 = val(input$genP1), var_p2 = val(input$genP2),
        covariables = as.character(input$genCov %||% character(0)))
      if (!isTRUE(pr$ok)) {
        rv$res <- NULL
        shiny::showNotification(pr$motif, type = "error", duration = 8)
        return()
      }
      alpha <- suppressWarnings(as.numeric(input$genAlpha %||% "0.05"))
      if (!isTRUE(alpha %in% c(0.01, 0.05, 0.1))) alpha <- 0.05
      si  <- hstat_borne_client(input$genSI, 15, 5, 50)
      Fc  <- hstat_borne_client(input$genF, 1, 0, 1)
      wy  <- hstat_borne_client(input$genPoidsY, 50, 0, 100)
      svp <- if (isTRUE(input$genSvp %in% HSTAT_GEN_GGE_SVP)) input$genSvp else "symetrique"
      meth_cmp <- if (isTRUE(input$genComparaison %in% HSTAT_GEN_COMPARAISONS)) input$genComparaison else "tukey"
      plan <- if (isTRUE(input$genPlanCroisement %in% HSTAT_GEN_PLANS_CROISEMENT)) input$genPlanCroisement else "auto"
      traits <- pr$traits
      vers <- function(x) unname(pr$noms[intersect(as.character(x), names(pr$noms))])
      dirs <- stats::setNames(ifelse(traits %in% vers(input$genMinimiser), "diminuer", "augmenter"), traits)
      principal <- vers(input$genPrincipal)
      principal <- if (length(principal)) principal[1] else traits[1]
      temoin <- val(input$genTemoin)
      d <- pr$data
      a_diallele <- any(!is.na(d$P1)) || any(!is.na(d$P2))
      par_trait <- list()
      echec <- tryCatch({ shiny::withProgress(message = tr("Analyse génétique en cours..."), value = 0, {
        # Une analyse qui leve devient un refus nomme pour ce caractere seul.
        sur <- function(expr, defaut = NULL) tryCatch(expr, error = function(e)
          if (is.null(defaut)) NULL else list(ok = FALSE, motif = hstat_err_fr(e)))
        echoue <- list(ok = FALSE)
        for (t in traits) {
          an <- sur(hstat_gen_anova(d, t, si / 100), echoue)
          am <- sur(hstat_gen_ammi(d, t, dirs[[t]], alpha, an), echoue)
          Y <- if (isTRUE(am$ok)) am$Y else {
            y <- hstat_gen_table_ge(d, t)
            if (ncol(y) >= 3 && all(is.finite(y))) y else NULL
          }
          mse <- if (isTRUE(an$ok)) an$mse else NA_real_
          r <- if (isTRUE(an$ok)) an$r else 1
          par_trait[[t]] <- list(
            anova = an, ammi = am,
            comp = sur(hstat_gen_comparaisons(d, t, an, dirs[[t]], meth_cmp, alpha), echoue),
            gge = if (isTRUE(am$ok)) sur(hstat_gen_gge(am$Y, dirs[[t]], isTRUE(input$genEchelle), svp), echoue)
                  else list(ok = FALSE, motif = am$motif),
            stab = if (!is.null(Y)) sur({
              st <- hstat_gen_stabilite(Y, mse, r, dirs[[t]])
              if (!is.null(st)) st$table <- hstat_gen_lecture_stabilite(st$table)
              st
            }),
            griffing = if (a_diallele) sur(hstat_gen_aptitudes(d, t, plan, Fc, dirs[[t]], temoin, alpha), echoue),
            cov = sur(hstat_gen_covariables(d, t, pr$covariables, dirs[[t]], alpha)),
            fr = sur(hstat_gen_regression_factorielle(d, t, pr$covariables, Y = Y, mse = mse, r = r)))
          shiny::incProgress(0.8 / length(traits))
        }
        compo <- do.call(rbind, lapply(par_trait, function(x)
          if (isTRUE(x$anova$ok)) x$anova$composantes))
        compo <- hstat_gen_composantes_h2(hstat_gen_lecture_composantes(compo),
                                          lapply(par_trait, `[[`, "griffing"))
        h2 <- if (!is.null(compo)) stats::setNames(compo$H2_moyenne, compo$Caractere)
        moy <- hstat_gen_moyennes(d, traits)
        mgidi <- if (length(traits) >= 2) hstat_gen_mgidi(moy, traits, dirs, si, h2)
                 else list(ok = FALSE, motif = tr("Le MGIDI demande au moins deux caractères."))
        mtsi <- hstat_gen_mtsi(lapply(par_trait, `[[`, "ammi"), dirs, wy, si)
        gyt <- hstat_gen_gyt(moy, principal, traits, dirs)
        classement <- if (a_diallele)
          hstat_gen_classement_croisements(lapply(par_trait, `[[`, "griffing"), dirs)
        tr_cor <- utils::head(traits, HSTAT_GEN_CORREL_MAX)
        corr <- if (length(tr_cor) >= 2) hstat_gen_correlations(d, tr_cor)
        pistes <- hstat_gen_pistes(moy, principal, setdiff(traits, principal))
        pp <- par_trait[[principal]]
        facteurs <- hstat_gen_synthese_facteurs(principal, dirs[[principal]], pp$cov, pistes,
                                                corr, pp$griffing, alpha)
        n_sel <- max(1L, round(pr$n_gen * si / 100))
        tete <- function(df, col, croissant = TRUE) {
          if (is.null(df) || !nrow(df) || !col %in% names(df)) return(character(0))
          o <- order(if (croissant) df[[col]] else -df[[col]])
          utils::head(as.character(df$Genotype[o]), n_sel)
        }
        sg <- .hstat_gen_signe(dirs[[principal]])
        val_pr <- if (isTRUE(pp$anova$ok)) pp$anova$valeurs
        if (!is.null(val_pr)) val_pr$Score <- sg * val_pr$Valeur_genetique
        consensus <- hstat_gen_consensus(stats::setNames(list(
          tete(val_pr, "Score", FALSE),
          if (isTRUE(mgidi$ok)) mgidi$selection else character(0),
          if (isTRUE(mtsi$ok)) mtsi$selection else character(0),
          if (isTRUE(pp$ammi$ok)) tete(pp$ammi$stabilite, "YSI") else character(0),
          if (isTRUE(pp$gge$ok)) tete(pp$gge$genotypes, "Rang") else character(0),
          if (isTRUE(gyt$ok)) tete(gyt$table, "Rang") else character(0),
          if (!is.null(pp$stab)) tete(pp$stab$table, "Rang_Lin_Binns") else character(0)),
          c(tr("Valeur génétique"), "MGIDI", "MTSI", "YSI", tr("GGE (génotype idéal)"),
            "GYT", "Lin & Binns")))
        shiny::incProgress(0.2)
      }); NULL }, error = function(e) e)
      if (!is.null(echec)) {
        rv$res <- NULL
        shiny::showNotification(trf("L'analyse génétique a échoué : %s", hstat_err_fr(echec)),
                                type = "error", duration = 10)
        return()
      }
      rv$res <- list(prep = pr, traits = traits, dirs = dirs, principal = principal,
                     par_trait = par_trait, composantes = compo, moyennes = moy,
                     mgidi = mgidi, mtsi = mtsi, gyt = gyt, classement = classement,
                     correlations = corr, pistes = pistes, facteurs = facteurs,
                     consensus = consensus, types = hstat_gen_types(d, traits),
                     interpretation = hstat_gen_interpretation(compo, lapply(par_trait, `[[`, "griffing")),
                     comp_blocs = .hstat_synth_empiler(lapply(par_trait, function(x)
                       if (isTRUE(x$comp$ok)) x$comp$bloc)),
                     methode_cmp = meth_cmp,
                     si = si, alpha = alpha, n_sel = n_sel, a_diallele = a_diallele,
                     tronque_cor = length(traits) > HSTAT_GEN_CORREL_MAX)
      rv$message <- pr$notes
      shiny::updateSelectInput(session, "genTraitAffiche", choices = traits, selected = principal)
      shiny::showNotification(
        trf("Analyse terminée : %d génotype(s), %d environnement(s), %d caractère(s).",
            pr$n_gen, pr$n_env, length(traits)),
        type = "message", duration = 6)
    })

    output$hasGen <- shiny::reactive(!is.null(rv$res))
    shiny::outputOptions(output, "hasGen", suspendWhenHidden = FALSE)

    output$genMessage <- shiny::renderUI({
      m <- rv$message
      if (!length(m)) return(NULL)
      shiny::div(class = "callout callout-warning", style = "margin-top:10px;padding:8px 12px;",
                 lapply(m, function(x) shiny::div(shiny::icon("triangle-exclamation"), " ", x)))
    })

    trait_aff <- shiny::reactive({
      shiny::req(rv$res)
      t <- input$genTraitAffiche
      if (isTRUE(t %in% rv$res$traits)) t else rv$res$principal
    })
    pt <- shiny::reactive(rv$res$par_trait[[trait_aff()]])

    note <- function(x, classe = "hstat-interpretation") {
      x <- x[nzchar(x)]
      if (!length(x)) return(NULL)
      shiny::div(class = classe, lapply(x, function(v) shiny::div(v)))
    }
    refus <- function(obj) if (!is.null(obj) && !isTRUE(obj$ok) && !is.null(obj$motif))
      shiny::div(class = "callout callout-warning", style = "padding:8px 12px;",
                 shiny::icon("circle-info"), " ", obj$motif)

    # SYNTHESE
    output$genSyntheseUI <- shiny::renderUI({
      shiny::req(rv$res)
      r <- rv$res; p <- r$principal; pp <- r$par_trait[[p]]
      li <- list()
      sens <- names(HSTAT_GEN_DIRECTIONS)[match(r$dirs[[p]], HSTAT_GEN_DIRECTIONS)]
      li[[length(li) + 1]] <- trf("Caractère principal : « %s » (%s).", p, tolower(tr(sens)))
      cp <- r$composantes
      if (!is.null(cp) && p %in% cp$Caractere) {
        k <- cp[cp$Caractere == p, ]
        li[[length(li) + 1]] <- trf("Héritabilité H² = %s (%s), précision sélective %s (%s), gain génétique attendu %s %% de la moyenne.",
          format(round(k$H2_moyenne, 3)), k$Classe_H2, format(round(k$Precision_selective, 3)),
          k$Classe_precision, format(round(k$Gain_genetique_pct, 1)))
        if (isTRUE(is.finite(k$h2_strict)))
          li[[length(li) + 1]] <- trf("Héritabilité au sens strict h² = %s (%s) : la part additive de la variance génétique.",
            format(round(k$h2_strict, 3)), k$Classe_h2)
      }
      if (isTRUE(pp$ammi$ok)) {
        s <- pp$ammi$stabilite
        li[[length(li) + 1]] <- trf("Performance et stabilité pour « %s » (YSI) : %s.", p,
          paste(utils::head(s$Genotype, r$n_sel), collapse = ", "))
        li[[length(li) + 1]] <- trf("L'interaction G × E est portée à %s %% par les deux premiers axes AMMI.",
          format(round(sum(utils::head(pp$ammi$axes$Pct_GE, 2)), 1)))
      }
      if (isTRUE(pp$gge$ok)) {
        mg <- pp$gge$mega
        li[[length(li) + 1]] <- trf("GGE : %d méga-environnement(s) ; gagnants : %s.",
          nrow(mg), paste(mg$Genotype_gagnant, collapse = ", "))
      }
      if (isTRUE(r$mgidi$ok))
        li[[length(li) + 1]] <- trf("MGIDI (tous caractères) : %s.", paste(r$mgidi$selection, collapse = ", "))
      if (isTRUE(r$mtsi$ok))
        li[[length(li) + 1]] <- trf("MTSI (performance et stabilité) : %s.", paste(r$mtsi$selection, collapse = ", "))
      if (isTRUE(pp$griffing$ok)) {
        g <- pp$griffing
        li[[length(li) + 1]] <- trf("Aptitudes à la combinaison (%s) : ratio de Baker %s. %s",
          tr(HSTAT_GEN_METHODES_GRIFFING[[g$methode]]), format(round(g$baker, 3)), g$action)
        bons <- g$agc$Parent[g$agc$Sens == tr("Favorable")]
        if (length(bons)) li[[length(li) + 1]] <- trf("Meilleurs parents (AGC favorable et significative) : %s.", paste(bons, collapse = ", "))
        if (!is.null(r$classement))
          li[[length(li) + 1]] <- trf("Meilleurs croisements (classement multicritère) : %s.",
            paste(utils::head(r$classement$Croisement, 3), collapse = ", "))
      }
      f <- r$facteurs
      if (!is.null(f)) {
        fav <- unique(f$Element[f$Effet == tr("Favorable")])
        dep <- unique(f$Element[f$Effet == tr("Défavorable")])
        if (length(fav)) li[[length(li) + 1]] <- trf("Facteurs qui améliorent « %s » : %s.", p, paste(fav, collapse = ", "))
        if (length(dep)) li[[length(li) + 1]] <- trf("Facteurs qui déprécient « %s » : %s.", p, paste(dep, collapse = ", "))
      }
      if (!is.null(pp$fr))
        li[[length(li) + 1]] <- trf("La covariable « %s » explique %s %% de l'interaction G × E.",
          pp$fr$tests$Covariable[1], format(round(pp$fr$tests$Pct_GE[1], 1)))
      cs <- r$consensus
      reco <- if (!is.null(cs)) cs$Genotype[cs$Methodes >= max(2, ceiling(cs$Sur[1] / 2))] else character(0)
      shiny::tagList(
        shiny::div(class = "hstat-interpretation",
          shiny::tags$h4(shiny::icon("award"), " ", tr("Génotypes recommandés")),
          shiny::tags$p(shiny::tags$b(if (length(reco)) paste(reco, collapse = ", ")
                                      else tr("Aucun génotype n'est retenu par la moitié des méthodes : comparez les classements ci-dessous."))),
          shiny::tags$ul(lapply(li, shiny::tags$li))),
        shiny::tags$small(style = "color:#5f6b7a;",
          "Un génotype est recommandé quand il figure parmi les meilleurs selon au moins la moitié des méthodes de classement."))
    })

    output$genConsensusTable <- DT::renderDT({
      shiny::req(rv$res$consensus)
      gen_dt(rv$res$consensus, tr("Génotypes retenus par chaque méthode de classement"))
    })
    output$genFacteursTable <- DT::renderDT({
      shiny::req(rv$res)
      shiny::validate(shiny::need(!is.null(rv$res$facteurs),
        tr("Aucun facteur significatif : ajoutez des covariables, des caractères associés ou des parents.")))
      gen_dt(rv$res$facteurs, trf("Facteurs qui améliorent ou déprécient « %s »", rv$res$principal))
    })

    # VARIANCE ET HERITABILITE
    output$genComposantesTable <- DT::renderDT({
      shiny::req(rv$res$composantes)
      gen_dt(rv$res$composantes, tr("Composantes de variance, héritabilités et paramètres de sélection"))
    })
    output$genAnovaNote <- shiny::renderUI({
      x <- pt(); if (is.null(x)) return(NULL)
      if (!isTRUE(x$anova$ok)) return(refus(x$anova))
      note(x$anova$notes)
    })
    output$genAnovaTable <- DT::renderDT({
      x <- pt(); shiny::req(isTRUE(x$anova$ok))
      gen_dt(x$anova$anova, trf("Analyse de variance de « %s »", trait_aff()))
    })
    output$genValeursTable <- DT::renderDT({
      x <- pt(); shiny::req(isTRUE(x$anova$ok))
      v <- x$anova$valeurs
      v$Rang <- rank(-.hstat_gen_signe(rv$res$dirs[[trait_aff()]]) * v$Valeur_genetique, ties.method = "min")
      gen_dt(v[order(v$Rang), ], trf("Moyennes et valeurs génétiques prédites de « %s »", trait_aff()))
    })
    output$genTypesTable <- DT::renderDT({
      shiny::req(rv$res$types)
      gen_dt(rv$res$types, tr("Comparaison des types de matériel"))
    })

    # LECTURE DES METRIQUES
    output$genInterpTable <- DT::renderDT({
      shiny::req(rv$res$interpretation)
      it <- rv$res$interpretation
      gen_dt(it, tr("Valeur, classe et signification génétique de chaque métrique"))
    })
    output$genGrilleTable <- DT::renderDT(
      gen_dt(hstat_gen_grille_metriques(), tr("Seuils d'interprétation des métriques génétiques")))

    # COMPARAISONS ET SYNTHESE COLOREE
    synth_tabs <- shiny::reactive({
      shiny::req(rv$res)
      b <- rv$res$comp_blocs
      shiny::validate(shiny::need(!is.null(b) && nrow(b),
        tr("Aucune comparaison de génotypes calculable : il faut une erreur estimable (répétitions ou plusieurs environnements).")))
      hstat_posthoc_synthese(b, if (identical(input$genSynthDisp, "erreur_type")) "erreur_type" else "ecart_type",
                             hstat_gen_decimales(input$genArrondir, input$genDecimales))
    })
    synth_couleurs <- shiny::reactive({
      b <- rv$res$comp_blocs
      if (is.null(b)) NULL else hstat_groupes_couleurs(b$groups)
    })
    output$genCompNote <- shiny::renderUI({
      x <- pt(); if (is.null(x)) return(NULL)
      cm <- x$comp
      if (!isTRUE(cm$ok)) return(refus(cm))
      note(trf("%s au seuil de %s : %d groupe(s) ; différence minimale significative %s. Deux génotypes qui partagent une lettre ne diffèrent pas.",
               names(HSTAT_GEN_COMPARAISONS)[HSTAT_GEN_COMPARAISONS == cm$methode],
               format(rv$res$alpha), cm$n_groupes, format(signif(cm$critique, 4))))
    })
    output$genCompTable <- DT::renderDT({
      x <- pt(); shiny::req(isTRUE(x$comp$ok))
      tb <- x$comp$table
      coul <- synth_couleurs()
      d <- gen_dt(tb, trf("Comparaison des moyennes de génotypes : « %s »", trait_aff()))
      if (length(coul))
        d <- DT::formatStyle(d, "groups", target = "row",
          backgroundColor = DT::styleEqual(names(coul), unname(coul)))
      DT::formatStyle(d, "groups", fontWeight = "bold")
    })
    output$genSynthHtml <- shiny::renderUI(hstat_synthese_html(synth_tabs(), synth_couleurs()))
    synth_fig <- shiny::reactive({
      tabs <- synth_tabs()
      hstat_synthese_figure(tabs, synth_couleurs(),
        titre = if (identical(input$genSynthDisp, "erreur_type"))
          tr("Moyenne ± erreur-type et groupes") else tr("Moyenne ± écart-type et groupes"))
    })
    output$genSynthPlot <- shiny::renderPlot(synth_fig(), height = function() {
      p <- tryCatch(synth_fig(), error = function(e) NULL)
      n <- if (is.null(p)) 8 else attr(p, "hstat_lignes") %||% 8
      max(300, min(4000, 60 + 28 * n))
    })
    output$genSPDl <- hstat_export_plot_handler(input, "genSP", function() synth_fig(),
                                                fname = "synthese_genotypes")

    # GRIFFING
    output$genGriffingNote <- shiny::renderUI({
      shiny::req(rv$res)
      if (!rv$res$a_diallele)
        return(note(tr("Indiquez les colonnes Parent 1 et Parent 2 pour estimer les aptitudes à la combinaison : diallèle (parents et F1, avec ou sans réciproques) ou plan lignées × testeurs.")))
      g <- pt()$griffing
      if (!isTRUE(g$ok)) return(refus(g))
      tete <- if (identical(g$plan, "ligne_testeur"))
        trf("%s : %d lignées, %d testeurs.", tr(HSTAT_GEN_METHODES_GRIFFING[[g$methode]]),
            length(g$lignees), length(g$testeurs))
      else trf("Diallèle de Griffing, %s, %d parents.", tr(HSTAT_GEN_METHODES_GRIFFING[[g$methode]]), g$p)
      note(c(tete, g$action, g$notes))
    })
    griff <- shiny::reactive({ g <- pt()$griffing; shiny::req(isTRUE(g$ok)); g })
    output$genGriffAnova <- DT::renderDT(gen_dt(griff()$anova,
      if (identical(griff()$plan, "ligne_testeur")) trf("Analyse de variance lignées × testeurs : « %s »", trait_aff())
      else trf("Analyse de variance du diallèle : « %s »", trait_aff())))
    output$genGriffVar <- DT::renderDT(gen_dt(griff()$variances, tr("Composantes génétiques, ratio de Baker et héritabilités")))
    output$genGriffAgc <- DT::renderDT(gen_dt(griff()$agc, tr("Effets d'aptitude générale à la combinaison (AGC) des parents")))
    output$genGriffAsc <- DT::renderDT(gen_dt(griff()$croisements, tr("Effets d'aptitude spécifique (ASC) et hétérosis des croisements")))
    output$genGriffRec <- DT::renderDT({
      shiny::req(griff()$reciproques)
      gen_dt(griff()$reciproques, tr("Effets réciproques (maternels)"))
    })
    output$genClassementTable <- DT::renderDT({
      shiny::req(rv$res$classement)
      gen_dt(rv$res$classement, tr("Classement multicritère des croisements (tous caractères)"))
    })

    # AMMI, GGE, STABILITE
    output$genAmmiNote <- shiny::renderUI({
      a <- pt()$ammi
      if (!isTRUE(a$ok)) return(refus(a))
      tete <- if (!isTRUE(a$axes_testes))
        tr("Sans répétition, les axes d'interaction ne sont pas testés : le premier axe est retenu.")
      else if (!a$n_axes_significatifs)
        tr("Aucun axe d'interaction significatif : le premier axe est retenu pour la stabilité.")
      else trf("%d axe(s) d'interaction significatif(s).", a$n_axes_significatifs)
      note(paste(tete, tr("ASV : valeur de stabilité AMMI (Purchase 2000), plus petite = plus stable ; YSI : rang de la moyenne du caractère + rang de l'ASV ; WAAS : moyenne pondérée des scores absolus (Olivoto 2019).")))
    })
    ammi <- shiny::reactive({ a <- pt()$ammi; shiny::req(isTRUE(a$ok)); a })
    output$genAmmiAnova <- DT::renderDT(gen_dt(ammi()$anova, trf("Analyse AMMI de « %s »", trait_aff())))
    output$genAmmiStab <- DT::renderDT(gen_dt(ammi()$stabilite, tr("Stabilité AMMI : ASV, YSI et WAAS")))
    output$genGgeNote <- shiny::renderUI({
      g <- pt()$gge
      if (!isTRUE(g$ok)) return(refus(g))
      note(trf("PC1 = %s %%, PC2 = %s %% de la variation G + G × E. Le génotype idéal est le plus proche du point idéal sur l'axe de l'environnement moyen.",
               format(round(g$pct[1], 1)), format(round(g$pct[2], 1))))
    })
    gge <- shiny::reactive({ g <- pt()$gge; shiny::req(isTRUE(g$ok)); g })
    output$genGgeMega <- DT::renderDT(gen_dt(gge()$mega, tr("Méga-environnements et génotypes gagnants")))
    output$genGgeGen <- DT::renderDT(gen_dt(gge()$genotypes, tr("Génotypes : moyenne, stabilité et distance au génotype idéal")))
    output$genGgeEnv <- DT::renderDT(gen_dt(gge()$environnements, tr("Environnements : pouvoir discriminant et représentativité")))
    output$genStabTable <- DT::renderDT({
      s <- pt()$stab
      shiny::validate(shiny::need(!is.null(s), tr("La stabilité demande au moins trois environnements et un tableau complet.")))
      gen_dt(s$table, tr("Indices de stabilité : Eberhart-Russell, Wricke, Shukla, Lin-Binns"))
    })

    # MULTICARACTERE
    output$genMgidiNote <- shiny::renderUI({
      shiny::req(rv$res)
      inc <- rv$res$mgidi$incomplets
      shiny::tagList(refus(rv$res$mgidi), refus(rv$res$mtsi), refus(rv$res$gyt),
        if (length(inc)) note(trf("%d génotype(s) écarté(s) du MGIDI, un caractère n'y étant pas mesuré : %s.",
                                  length(inc), hstat_lignes_compactes(inc, max = 10L))),
        note(trf("Intensité de sélection : %s %% (%d génotype(s) retenu(s)). Un indice plus petit est plus proche de l'idéotype.",
                 format(rv$res$si), rv$res$n_sel)))
    })
    output$genMgidiTable <- DT::renderDT({ m <- rv$res$mgidi; shiny::req(isTRUE(m$ok))
      gen_dt(m$table, tr("MGIDI : indice de distance génotype-idéotype multicaractère")) })
    output$genGainsTable <- DT::renderDT({ m <- rv$res$mgidi; shiny::req(isTRUE(m$ok))
      gen_dt(m$gains, tr("Différentiels et gains de sélection attendus (MGIDI)")) })
    output$genChargesTable <- DT::renderDT({ m <- rv$res$mgidi; shiny::req(isTRUE(m$ok))
      gen_dt(m$charges, tr("Analyse factorielle : charges, communalités et facteurs")) })
    output$genForcesTable <- DT::renderDT({ m <- rv$res$mgidi; shiny::req(isTRUE(m$ok), !is.null(m$forces))
      gen_dt(m$forces, tr("Forces et faiblesses des génotypes (MGIDI)")) })
    output$genMtsiTable <- DT::renderDT({ m <- rv$res$mtsi; shiny::req(isTRUE(m$ok))
      gen_dt(m$table, tr("MTSI : indice de stabilité multicaractère (WAASY)")) })
    output$genGytTable <- DT::renderDT({ m <- rv$res$gyt; shiny::req(isTRUE(m$ok))
      gen_dt(m$table, tr("GYT : indice de supériorité caractère principal × autres caractères")) })

    # ENVIRONNEMENT x GENOTYPE
    output$genCovNote <- shiny::renderUI({
      shiny::req(rv$res)
      if (!length(rv$res$prep$covariables))
        return(note(tr("Ajoutez des covariables (pluviométrie, température, humidité, sol, pression des ravageurs, pratiques) pour savoir ce qui améliore ou déprécie le caractère.")))
      if (is.null(pt()$cov)) return(note(tr("Aucune covariable exploitable pour ce caractère.")))
      NULL
    })
    output$genCovTable <- DT::renderDT({ shiny::req(pt()$cov)
      gen_dt(pt()$cov, trf("Effet des variables environnementales sur « %s »", trait_aff())) })
    output$genFrTable <- DT::renderDT({ shiny::req(pt()$fr)
      gen_dt(pt()$fr$tests, tr("Régression factorielle : part de l'interaction G × E expliquée par chaque covariable")) })
    output$genSensTable <- DT::renderDT({ shiny::req(pt()$fr)
      gen_dt(pt()$fr$sensibilites, tr("Sensibilité de chaque génotype aux covariables (pente de son interaction)")) })

    # CORRELATIONS ET PISTES
    output$genCorGTable <- DT::renderDT({ shiny::req(rv$res$correlations)
      gen_dt(.hstat_gen_mat_df(rv$res$correlations$genotypique), tr("Corrélations génotypiques")) })
    output$genCorPTable <- DT::renderDT({ shiny::req(rv$res$correlations)
      gen_dt(.hstat_gen_mat_df(rv$res$correlations$phenotypique), tr("Corrélations phénotypiques")) })
    output$genPistesNote <- shiny::renderUI({
      shiny::req(rv$res)
      p <- rv$res$pistes
      if (!isTRUE(p$ok)) return(refus(p))
      note(c(trf("Coefficients de piste sur « %s » ; effet résiduel %s.", p$cible, format(round(p$residuel, 3))),
             if (rv$res$tronque_cor) trf("Corrélations limitées aux %d premiers caractères.", HSTAT_GEN_CORREL_MAX)))
    })
    output$genPistesTable <- DT::renderDT({ p <- rv$res$pistes; shiny::req(isTRUE(p$ok))
      gen_dt(p$table, tr("Effets directs et indirects sur le caractère principal")) })

    # GRAPHIQUE
    graphique <- shiny::reactive({
      shiny::req(rv$res)
      quoi <- input$genGraphique %||% "ammi1"
      x <- pt(); tr_ <- trait_aff()
      palette <- input$genPalette %||% "module"
      opt <- list(etiq = isTRUE(input$genEtiquettes), ellipses = !isFALSE(input$genEllipses),
                  heritabilite = input$genHeritAffiche %||% "deux",
                  taille = hstat_borne_client(input$genEtiqSize, 3.2, 1, 8),
                  point = hstat_borne_client(input$genPointSize, 2.5, 0.5, 8),
                  alpha = hstat_borne_client(input$genOpacite, 1, 0.1, 1),
                  trait = hstat_borne_client(input$genTraitSize, 0.5, 0.1, 3),
                  valeurs = !isFALSE(input$genValeurs),
                  dec = hstat_borne_client(input$genValeursDec, 2, 0, 6),
                  etiq_coul = !isFALSE(input$genEtiqCouleur),
                  contour = hstat_borne_client(input$genContour, 0.3, 0, 3),
                  couleurs = hstat_gen_couleurs_roles(list(
                    genotype = input$genCoulGeno, environnement = input$genCoulEnv,
                    favorable = input$genCoulFav, defavorable = input$genCoulDef,
                    neutre = input$genCoulNeutre, selection = input$genCoulSel), palette))
      p <- hstat_gen_figure(quoi, rv$res, tr_, opt)
      shiny::validate(shiny::need(!inherits(p, "hstat_gen_absent"), attr(p, "motif")))
      extras <- hstat_plot_extras_lire(input, "genX", familles = HSTAT_PLOT_EXTRAS_FAMILLES)
      titre <- input$genTitre %||% ""; st <- input$genSousTitre %||% ""
      lx <- input$genLabX %||% ""; ly <- input$genLabY %||% ""
      if (nzchar(titre)) p <- p + ggplot2::ggtitle(titre)
      if (nzchar(st)) p <- p + ggplot2::labs(subtitle = st)
      if (nzchar(lx)) p <- p + ggplot2::labs(x = lx)
      if (nzchar(ly)) p <- p + ggplot2::labs(y = ly)
      p <- hstat_gen_appliquer_palette(p, palette)
      propre <- p$theme
      titre_pos <- suppressWarnings(as.numeric(input$genTitrePos %||% "0.5")[1])
      if (!isTRUE(titre_pos %in% c(0, 0.5, 1))) titre_pos <- 0.5
      p <- p +
        hstat_export_theme(input, "genP", base_size = extras$police %||% HSTAT_GG_BASE_SIZE) +
        (if (!identical(input$genStyle, "classique")) .hstat_gen_habillage()) +
        # Le theme propre a la figure (cases sans grille, graduations inclinees)
        # survit au theme complet pose ci-dessus.
        (if (length(propre)) do.call(ggplot2::theme, propre)) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = hstat_borne_client(input$genTitreSize, 15, 8, 30),
                                             hjust = titre_pos,
                                             face = extras$st_titre %||% "bold"),
          plot.subtitle = ggplot2::element_text(size = hstat_borne_client(input$genSousTitreSize, 11, 6, 24),
                                                hjust = titre_pos),
          axis.title = ggplot2::element_text(size = hstat_borne_client(input$genAxeTitreSize, 11, 6, 24),
                                             face = extras$st_axes %||% "plain"),
          axis.text = ggplot2::element_text(size = hstat_borne_client(input$genGradSize, 9, 5, 20),
                                            face = extras$st_grad %||% "plain"),
          legend.position = if (isTRUE(input$genLegendePos %in% c("right", "bottom", "top", "left", "none"))) input$genLegendePos else "right") +
        hstat_plot_extras_theme(extras)
      # Un angle laisse a 0 ne remplace pas l'inclinaison que la figure pose
      # elle-meme : ses libelles se chevaucheraient.
      if (!is.null(propre$axis.text.x) && isTRUE(extras$angle_x %in% 0)) {
        e <- propre$axis.text.x
        # Le kit centre verticalement une etiquette droite : penchee, elle doit
        # finir sous sa graduation, sinon elle mord sur le titre de l'axe.
        if (isTRUE(e$angle > 0)) e$vjust <- 1
        p <- p + ggplot2::theme(axis.text.x = e)
      }
      # Masquer une grille retire ce que le theme pose ; ne rien cocher le laisse.
      if (isTRUE(input$genGrilleMaj)) p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
      if (isTRUE(input$genGrilleMin)) p <- p + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      hstat_plot_extras_scales(p, extras)
    })
    output$genPlot <- shiny::renderPlot(graphique())

    # EXPORTS
    tables_export <- function() {
      r <- rv$res
      if (is.null(r)) return(list())
      pt_ <- r$par_trait
      ok <- function(o) if (isTRUE(o$ok)) o
      l <- list(
        Synthese_facteurs = r$facteurs,
        Recommandation = r$consensus,
        Composantes_variance = r$composantes,
        ANOVA = .hstat_gen_empile(pt_, function(x) ok(x$anova)$anova),
        Valeurs_genetiques = .hstat_gen_empile(pt_, function(x) ok(x$anova)$valeurs),
        Moyennes = r$moyennes,
        Types_materiel = r$types,
        Lecture_metriques = r$interpretation,
        Grille_seuils = hstat_gen_grille_metriques(),
        Comparaisons_genotypes = .hstat_gen_empile(pt_, function(x) ok(x$comp)$table),
        AGC_ASC_ANOVA = .hstat_gen_empile(pt_, function(x) ok(x$griffing)$anova),
        AGC_ASC_variances = .hstat_gen_empile(pt_, function(x) ok(x$griffing)$variances),
        AGC = .hstat_gen_empile(pt_, function(x) ok(x$griffing)$agc),
        ASC_heterosis = .hstat_gen_empile(pt_, function(x) ok(x$griffing)$croisements),
        Classement_croisements = r$classement,
        AMMI_ANOVA = .hstat_gen_empile(pt_, function(x) ok(x$ammi)$anova),
        ASV_YSI_WAAS = .hstat_gen_empile(pt_, function(x) ok(x$ammi)$stabilite),
        GGE_genotypes = .hstat_gen_empile(pt_, function(x) ok(x$gge)$genotypes),
        GGE_environnements = .hstat_gen_empile(pt_, function(x) ok(x$gge)$environnements),
        GGE_mega_env = .hstat_gen_empile(pt_, function(x) ok(x$gge)$mega),
        Stabilite = .hstat_gen_empile(pt_, function(x) x$stab$table),
        MGIDI = ok(r$mgidi)$table, Gains_selection = ok(r$mgidi)$gains,
        Analyse_factorielle = ok(r$mgidi)$charges, Forces_faiblesses = ok(r$mgidi)$forces,
        MTSI = ok(r$mtsi)$table, GYT = ok(r$gyt)$table,
        Covariables = .hstat_gen_empile(pt_, function(x) x$cov),
        Regression_factorielle = .hstat_gen_empile(pt_, function(x) x$fr$tests),
        Sensibilites = .hstat_gen_empile(pt_, function(x) x$fr$sensibilites),
        Correlations_genotypiques = .hstat_gen_mat_df(r$correlations$genotypique),
        Correlations_phenotypiques = .hstat_gen_mat_df(r$correlations$phenotypique),
        Pistes = ok(r$pistes)$table)
      if (!is.null(r$comp_blocs) && nrow(r$comp_blocs))
        l <- c(l, hstat_posthoc_synthese_feuilles(r$comp_blocs,
                                                  hstat_gen_decimales(input$genArrondir, input$genDecimales)))
      hstat_tables_non_vides(l)
    }
    hstat_export_tables_handlers(output, "genT", tables_export,
                                 fname = "selection_genetique", libelle = "sélection génétique")
    output$genPDl <- hstat_export_plot_handler(input, "genP", function() graphique(),
                                               fname = "selection_genetique")
  })
}
