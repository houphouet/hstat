#  Module Shiny : Dispositifs experimentaux & Puissance statistique (type G*Power)


.hstat_rename_treat_cols <- function(book, factors) {
  for (fn in names(factors)) {
    levs <- as.character(factors[[fn]])
    for (cn in names(book)) {
      if (cn %in% names(factors)) next
      vals <- as.character(unique(book[[cn]]))
      if (length(vals) > 0 && setequal(vals, levs)) {
        names(book)[names(book) == cn] <- fn
        break
      }
    }
  }
  book
}

hstat_power_families <- function() {
  list(
    "t" = c("Corrélation : modèle point biserial"                       = "cor_pb",
            "Regression bivariee : une pente"                           = "reg_slope",
            "Moyennes : deux moyennes dependantes (paires appariées)"   = "t_paired",
            "Moyennes : deux moyennes independantes (deux groupes)"     = "t_two",
            "Moyennes : difference par rapport a une constante (1 ech.)" = "t_one",
            "Moyennes : Wilcoxon signed-rank (paires)"                  = "wilcox_paired",
            "Moyennes : Wilcoxon signed-rank (un échantillon)"          = "wilcox_one",
            "Moyennes : Wilcoxon-Mann-Whitney (deux groupes)"           = "mwu",
            "Test t generique"                                          = "t_generic"),
    "F" = c("ANOVA : effets fixes, omnibus, un facteur"                 = "anova_oneway",
            "ANOVA : effets fixes, effets principaux et interactions"   = "anova_factorial",
            "ANOVA : mesures repetees, entre facteurs"                  = "rm_between",
            "ANOVA : mesures repetees, intra facteurs"                  = "rm_within",
            "ANOVA : mesures repetees, interaction intra-entre"         = "rm_interaction",
            "ANCOVA : effets fixes, effets principaux et interactions"  = "ancova",
            "MANOVA : effets globaux"                                   = "manova_global",
            "Regression multiple : increment de R2 (modèle fixe)"       = "reg_r2inc",
            "Regression multiple : R2 écart a zero (modèle fixe)"       = "reg_r2dev",
            "Test F generique"                                          = "f_generic"),
    "chisq" = c("Ajustement / tables de contingence (GoF)"             = "gof",
                "Test chi-deux generique"                              = "chisq_generic"),
    "z" = c("Corrélation : modèle normal bivarie"                      = "cor_biv",
            "Corrélation : modèle tetrachorique"                       = "cor_tetra",
            "Corrélations : deux r independants (Pearson)"             = "cor_2indep",
            "Corrélations : deux r dependants (Pearson)"               = "cor_2dep",
            "Regression logistique (prédicteur continu)"               = "logistic",
            "Regression de Poisson (prédicteur continu)"               = "poisson",
            "Proportions : deux groupes independants"                  = "prop2",
            "Proportions : difference vs constante (un échantillon)"   = "prop1",
            "Proportions : McNemar (groupes dependants)"               = "mcnemar",
            "Proportions : test du signe"                              = "sign")
  )
}

# Moteur F type G*Power : lambda = f^2 * N ; df2 = N - groups - covars.
# Cette convention (et non celle de pwr.f2.test) reproduit ANOVA/ANCOVA/MANOVA.
.hstat_gpower_F <- function(f = NULL, alpha, power = NULL, n_total = NULL,
                            df1, groups, covars = 0, solve_effect = FALSE) {
  if (solve_effect) {
    # Sensibilite : resoudre f pour atteindre 'power' a N fixe (n_total)
    N <- n_total; df2 <- N - groups - covars
    if (df2 <= 0) return(list(err = "ddl denominateur <= 0 : augmentez la taille."))
    fcrit <- stats::qf(1 - alpha, df1, df2)
    pw_for_f <- function(ff) 1 - stats::pf(fcrit, df1, df2, ncp = ff^2 * N)
    fsol <- tryCatch(stats::uniroot(function(ff) pw_for_f(ff) - power, c(1e-4, 10))$root,
                     error = function(e) NA)
    if (is.na(fsol)) return(list(err = "Effet detectable non resolu."))
    list(N = N, df1 = df1, df2 = df2, lambda = fsol^2 * N, crit = fcrit,
         power = power, f = fsol)
  } else if (!is.null(n_total)) {
    N <- n_total; df2 <- N - groups - covars
    if (df2 <= 0) return(list(err = "ddl denominateur <= 0 : augmentez la taille."))
    lambda <- f^2 * N
    list(N = N, df1 = df1, df2 = df2, lambda = lambda, f = f,
         crit = stats::qf(1 - alpha, df1, df2),
         power = 1 - stats::pf(stats::qf(1 - alpha, df1, df2), df1, df2, ncp = lambda))
  } else {
    for (N in (df1 + groups + covars + 1):200000) {
      df2 <- N - groups - covars; if (df2 <= 0) next
      lambda <- f^2 * N
      pw <- 1 - stats::pf(stats::qf(1 - alpha, df1, df2), df1, df2, ncp = lambda)
      if (pw >= power)
        return(list(N = N, df1 = df1, df2 = df2, lambda = lambda, f = f,
                    crit = stats::qf(1 - alpha, df1, df2), power = pw))
    }
    list(err = "Taille introuvable dans la plage exploree.")
  }
}


hstat_gpower <- function(test, analysis, effect = NULL, effect2 = NULL,
                         alpha = 0.05, power = NULL, n = NULL, n2 = NULL,
                         k = NULL, u = NULL, df1 = NULL, groups = NULL,
                         covars = 0, alt = "two.sided",
                         p0 = NULL, base_rate = NULL, mean_exposure = NULL,
                         R2_other = 0, predictor = "continuous", px = NULL) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  if (!requireNamespace("pwr", quietly = TRUE))
    return(list(err = "Le package 'pwr' est requis pour l'analyse de puissance."))

  want_n <- (analysis == "apriori")
  want_power <- (analysis == "posthoc")
  want_eff <- (analysis == "sensitivity")
  na <- if (want_n) NULL else n
  pa <- if (want_power) NULL else power
  ea <- if (want_eff) NULL else effect
  tcrit_lab <- function(df) stats::qt(1 - alpha / ifelse(alt == "two.sided", 2, 1), df)
  zcrit <- stats::qnorm(1 - alpha / ifelse(alt == "two.sided", 2, 1))

  tryCatch({
    # ---- Famille des moyennes (t) ----
    if (test %in% c("t_two", "mwu", "t_generic")) {
      r <- pwr::pwr.t.test(n = na, d = ea, sig.level = alpha, power = pa,
                           type = "two.sample", alternative = alt)
      nn <- ceiling(r$n); df <- 2 * nn - 2
      wil <- (test == "mwu"); nper <- if (wil) ceiling(nn / 0.864) else nn
      list(effect = r$d, nper = nper, ntot = 2 * nper, lambda = r$d * sqrt(nn / 2),
           crit = tcrit_lab(df), crit_lab = "t critique",
           df_lab = sprintf("ddl = %d", df), power = r$power,
           note = if (wil) "Mann-Whitney : n parametrique ajuste par l'efficacite relative asymptotique (~0.864)." else NULL)
    } else if (test %in% c("t_paired", "t_one", "wilcox_paired", "wilcox_one")) {
      typ <- if (grepl("paired", test)) "paired" else "one.sample"
      r <- pwr::pwr.t.test(n = na, d = ea, sig.level = alpha, power = pa,
                           type = typ, alternative = alt)
      nn <- ceiling(r$n); df <- nn - 1; wil <- grepl("wilcox", test)
      nper <- if (wil) ceiling(nn / 0.864) else nn
      list(effect = r$d, nper = nper, ntot = nper, lambda = r$d * sqrt(nn),
           crit = tcrit_lab(df), crit_lab = "t critique",
           df_lab = sprintf("ddl = %d", df), power = r$power,
           note = if (wil) "Wilcoxon : n parametrique ajuste par l'efficacite relative asymptotique (~0.864)." else NULL)
    } else if (test %in% c("cor_pb", "cor_biv", "cor_tetra", "reg_slope")) {
      r <- pwr::pwr.r.test(n = na, r = ea, sig.level = alpha, power = pa, alternative = alt)
      nn <- ceiling(r$n)
      list(effect = r$r, nper = NA, ntot = nn, lambda = NA, crit = zcrit,
           crit_lab = "z critique", df_lab = sprintf("ddl = %d", nn - 2),
           power = r$power,
           note = if (test == "cor_tetra") "Tetrachorique approxime par le modèle normal bivarie." else NULL)
    } else if (test %in% c("cor_2indep", "cor_2dep")) {
      dep <- (test == "cor_2dep")
      r1 <- effect %||% 0.3; r2 <- effect2 %||% 0
      res <- hstat_power_two_r(r1, r2, analysis = analysis, power = pa,
                               n = na, alpha = alpha, alt = alt, dependent = dep)
      list(effect = abs(atanh(r1) - atanh(r2)), nper = res$nper, ntot = res$ntot,
           lambda = NA, crit = res$crit, crit_lab = "z critique",
           df_lab = sprintf("r1 = %.2f ; r2 = %.2f%s", r1, r2,
                            if (dep) " (échantillon dependant)" else ""),
           power = res$power,
           note = "Comparaison de deux coefficients via la transformation z de Fisher.")
    } else if (test == "logistic") {
      res <- hstat_power_logistic(analysis = analysis, p0 = p0 %||% 0.2,
               OR = effect %||% 1.5, alpha = alpha, power = pa, n = na,
               alt = alt, R2_other = R2_other %||% 0,
               predictor = predictor %||% "continuous", px = px %||% 0.5)
      if (!is.null(res$err)) stop(res$err)
      list(effect = res$effect, nper = NA, ntot = res$n, lambda = NA,
           crit = res$crit, crit_lab = "z critique", df_lab = res$extra,
           power = res$power,
           note = trf("Regression logistique (Hsieh et al. 1998) : prédicteur %s ; OR par %s.",
                          if ((predictor %||% "continuous") == "binary") "binaire (expose/non-expose)" else "continu standardise",
                          if ((predictor %||% "continuous") == "binary") "exposition" else "écart-type"))
    } else if (test == "poisson") {
      res <- hstat_power_poisson(analysis = analysis, base_rate = base_rate %||% 1,
               RR = effect %||% 1.3, alpha = alpha, power = pa, n = na, alt = alt,
               mean_exposure = mean_exposure %||% 1, R2_other = R2_other %||% 0)
      if (!is.null(res$err)) stop(res$err)
      list(effect = res$effect, nper = NA, ntot = res$n, lambda = NA,
           crit = res$crit, crit_lab = "z critique", df_lab = res$extra,
           power = res$power,
           note = "Regression de Poisson (Signorini 1991) : prédicteur continu standardise ; RR par écart-type.")
    # ---- Famille F (ANOVA / ANCOVA / MANOVA / mesures repetees / regression) ----
    } else if (test == "anova_oneway") {
      g <- groups %||% k %||% 2
      r <- pwr::pwr.anova.test(k = g, n = na, f = ea, sig.level = alpha, power = pa)
      nper <- ceiling(r$n); N <- g * nper; d1 <- g - 1; d2 <- N - g
      list(effect = r$f, nper = nper, ntot = N, lambda = r$f^2 * N,
           crit = stats::qf(1 - alpha, d1, d2), crit_lab = "F critique",
           df_lab = sprintf("ddl num = %d ; ddl den = %d", d1, d2), power = r$power)
    } else if (test %in% c("anova_factorial", "rm_between", "rm_within",
                           "rm_interaction", "ancova", "manova_global",
                           "reg_r2inc", "reg_r2dev", "f_generic")) {
      g  <- groups %||% k %||% 2
      d1 <- df1 %||% (g - 1)
      cov <- covars %||% 0
      gp <- .hstat_gpower_F(f = ea %||% effect, alpha = alpha, power = pa,
                            n_total = if (want_n) NULL else n, df1 = d1,
                            groups = g, covars = cov, solve_effect = want_eff)
      if (!is.null(gp$err)) stop(gp$err)
      list(effect = if (want_eff) gp$f else (ea %||% effect),
           nper = NA, ntot = gp$N, lambda = gp$lambda,
           crit = gp$crit, crit_lab = "F critique",
           df_lab = sprintf("ddl num = %d ; ddl den = %d", gp$df1, gp$df2),
           power = gp$power)
    # ---- Famille chi-deux ----
    } else if (test %in% c("gof", "chisq_generic")) {
      ddl <- k %||% df1 %||% 1
      r <- pwr::pwr.chisq.test(w = ea, N = na, df = ddl, sig.level = alpha, power = pa)
      N <- ceiling(r$N)
      list(effect = r$w, nper = NA, ntot = N, lambda = r$w^2 * N,
           crit = stats::qchisq(1 - alpha, ddl), crit_lab = "Chi2 critique",
           df_lab = sprintf("ddl = %d", ddl), power = r$power)
    # ---- Famille z / proportions ----
    } else if (test %in% c("prop2", "mcnemar")) {
      r <- pwr::pwr.2p.test(h = ea, n = na, sig.level = alpha, power = pa, alternative = alt)
      nn <- ceiling(r$n)
      list(effect = r$h, nper = nn, ntot = 2 * nn, lambda = NA, crit = zcrit,
           crit_lab = "z critique", df_lab = "\u2014", power = r$power,
           note = if (test == "mcnemar") "McNemar approxime par le test de deux proportions." else NULL)
    } else if (test %in% c("prop1", "sign")) {
      r <- pwr::pwr.p.test(h = ea, n = na, sig.level = alpha, power = pa, alternative = alt)
      nn <- ceiling(r$n)
      list(effect = r$h, nper = nn, ntot = nn, lambda = NA, crit = zcrit,
           crit_lab = "z critique", df_lab = "\u2014", power = r$power,
           note = if (test == "sign") "Test du signe approxime par une proportion (p = 0.5)." else NULL)
    } else stop("Test inconnu : ", test)
  }, error = function(e) list(err = conditionMessage(e)))
}

hstat_effect_conventions <- function() {
  data.frame(
    Test = c("t (d)", "t (d)", "t (d)", "ANOVA (f)", "ANOVA (f)", "ANOVA (f)",
             "f2 (factoriel/reg)", "f2 (factoriel/reg)", "f2 (factoriel/reg)",
             "Corrélation (r)", "Corrélation (r)", "Corrélation (r)",
             "Chi-deux (w)", "Chi-deux (w)", "Chi-deux (w)",
             "Proportions (h)", "Proportions (h)", "Proportions (h)"),
    Taille = rep(c("Petit", "Moyen", "Grand"), 6),
    Valeur = c(0.20, 0.50, 0.80, 0.10, 0.25, 0.40, 0.02, 0.15, 0.35,
               0.10, 0.30, 0.50, 0.10, 0.30, 0.50, 0.20, 0.50, 0.80),
    stringsAsFactors = FALSE)
}

# Taille d'échantillon pour ENQUETE DE TERRAIN (sondage) :
# estimation d'une proportion ou d'une moyenne, avec correction population finie,
# effet de plan (grappes), taux de non-reponse, et repartition par strate.
hstat_survey_size <- function(objective = "proportion", conf_level = 0.95,
                              margin = 0.05, p = 0.5, sd = NULL,
                              population = Inf, design_effect = 1,
                              response_rate = 1, n_strata = 1) {
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  n0 <- if (objective == "proportion") {
    (z^2 * p * (1 - p)) / (margin^2)
  } else {
    if (is.null(sd) || is.na(sd) || sd <= 0)
      return(list(err = "Un écart-type estimé (> 0) est requis pour une moyenne."))
    (z^2 * sd^2) / (margin^2)
  }
  n_fpc  <- if (is.finite(population) && population > 0)
    n0 / (1 + (n0 - 1) / population) else n0
  n_deff <- n_fpc * design_effect
  n_fin  <- n_deff / max(response_rate, 1e-6)
  list(z = z, n0 = ceiling(n0), n_fpc = ceiling(n_fpc),
       n_deff = ceiling(n_deff), n_final = ceiling(n_fin),
       per_stratum = if (n_strata > 1) ceiling(ceiling(n_fin) / n_strata) else NA,
       objective = objective)
}

# Marge d'erreur atteinte pour un n donne (analyse inverse)
hstat_survey_margin <- function(n, conf_level = 0.95, p = 0.5,
                                population = Inf, objective = "proportion",
                                sd = NULL) {
  z   <- stats::qnorm(1 - (1 - conf_level) / 2)
  fpc <- if (is.finite(population) && population > n)
    sqrt((population - n) / (population - 1)) else 1
  if (objective == "proportion") z * sqrt(p * (1 - p) / n) * fpc
  else z * (sd / sqrt(n)) * fpc
}

# Puissance / n pour comparer DEUX coefficients de correlation (z de Fisher)
hstat_power_two_r <- function(r1, r2, analysis = "apriori", power = NULL,
                              n = NULL, alpha = 0.05, alt = "two.sided",
                              dependent = FALSE) {
  q  <- abs(atanh(r1) - atanh(r2))
  zc <- stats::qnorm(1 - alpha / ifelse(alt == "two.sided", 2, 1))
  fac <- if (dependent) 1 else 2   # dependant : meme échantillon
  if (analysis == "apriori") {
    zb <- stats::qnorm(power)
    nn <- ((zc + zb) / q)^2 * fac + 3
    list(ntot = ceiling(nn), nper = if (dependent) ceiling(nn) else ceiling(nn),
         crit = zc, power = power)
  } else {
    pw <- stats::pnorm(q * sqrt((n - 3) / fac) - zc)
    list(ntot = if (dependent) n else 2 * n, nper = n, crit = zc, power = pw)
  }
}

# Puissance pour la REGRESSION LOGISTIQUE (predicteur continu N(0,1)).
# Methode de Hsieh, Bloch & Larsen (1998). OR = odds-ratio par ecart-type ;
# p0 = probabilite de l'evenement a la moyenne du predicteur ;
# R2_other = part de variance du predicteur expliquee par les autres covariables.
hstat_power_logistic <- function(analysis = "apriori", p0 = 0.2, OR = 1.5,
                                 alpha = 0.05, power = NULL, n = NULL,
                                 alt = "two.sided", R2_other = 0,
                                 predictor = "continuous", px = 0.5) {
  b <- log(OR); za <- stats::qnorm(1 - alpha / ifelse(alt == "two.sided", 2, 1))

  if (predictor == "binary") {
    # Hsieh, Bloch & Larsen (1998), eq.(2) : predicteur binaire (expose/non-expose)
    if (px <= 0 || px >= 1) return(list(err = "La proportion exposee doit être dans ]0,1[."))
    p1 <- p0 * OR / (1 - p0 + p0 * OR)         # prob de l'evenement chez les exposes
    if (abs(p1 - p0) < 1e-9) return(list(err = "OR doit differer de 1."))
    pbar <- (1 - px) * p0 + px * p1
    n_from_power <- function(pw) {
      zb <- stats::qnorm(pw)
      num <- (za * sqrt(pbar * (1 - pbar) / px) +
              zb * sqrt(p0 * (1 - p0) + p1 * (1 - p1) * (1 - px) / px))^2
      den <- (p1 - p0)^2 * (1 - px)
      (num / den) / (1 - R2_other)
    }
    if (analysis == "apriori") {
      list(n = ceiling(n_from_power(power)), effect = OR, power = power, crit = za,
           extra = sprintf("OR = %.3f ; p0 = %.3f ; p1 = %.3f ; expose = %.0f%%",
                           OR, p0, p1, 100 * px))
    } else if (analysis == "posthoc") {
      # Deux fonctions objectif dans deux branches EXCLUSIVES portaient le meme
      # nom `f`, avec des arguments differents. Elles ne coexistent jamais, mais
      # le lecteur -- et l'analyseur de `R CMD check` -- ne peuvent le savoir
      # qu'en suivant les branches. Les nommer par ce qu'elles resolvent coute
      # deux mots et supprime la question.
      f_puissance <- function(pw) n_from_power(pw) - n
      pw <- tryCatch(stats::uniroot(f_puissance, c(0.0001, 0.9999))$root,
                     error = function(e) NA)
      list(n = n, effect = OR, power = pw, crit = za,
           extra = sprintf("OR = %.3f ; p0 = %.3f ; p1 = %.3f ; expose = %.0f%%",
                           OR, p0, p1, 100 * px))
    } else {
      # sensibilite : resoudre OR (donc p1) pour la puissance cible au n fixe
      f_or <- function(ORx) {
        p1x <- p0 * ORx / (1 - p0 + p0 * ORx)
        pbarx <- (1 - px) * p0 + px * p1x
        zb <- stats::qnorm(power)
        num <- (za * sqrt(pbarx * (1 - pbarx) / px) +
                zb * sqrt(p0 * (1 - p0) + p1x * (1 - p1x) * (1 - px) / px))^2
        den <- (p1x - p0)^2 * (1 - px)
        (num / den) / (1 - R2_other) - n
      }
      ORs <- tryCatch(stats::uniroot(f_or, c(1.001, 50))$root, error = function(e) NA)
      list(n = n, effect = ORs, power = power, crit = za,
           extra = if (is.na(ORs)) "OR non resolu"
                   else sprintf("OR detectable = %.3f ; p0 = %.3f ; expose = %.0f%%",
                                ORs, p0, 100 * px))
    }
  } else {
    # Predicteur continu N(0,1) : Hsieh (1998), forme standardisee
    base_var <- p0 * (1 - p0) * b^2
    if (base_var <= 0) return(list(err = "OR doit differer de 1 et p0 dans ]0,1[."))
    if (analysis == "apriori") {
      zb <- stats::qnorm(power)
      nn <- ((za + zb)^2 / base_var) / (1 - R2_other)
      list(n = ceiling(nn), effect = OR, power = power, crit = za,
           extra = sprintf("OR = %.3f ; p0 = %.3f ; beta1 = %.4f", OR, p0, b))
    } else if (analysis == "posthoc") {
      zb <- sqrt(n * (1 - R2_other) * base_var) - za
      list(n = n, effect = OR, power = stats::pnorm(zb), crit = za,
           extra = sprintf("OR = %.3f ; p0 = %.3f", OR, p0))
    } else {
      zb <- stats::qnorm(power)
      b2 <- (za + zb)^2 / (n * (1 - R2_other) * p0 * (1 - p0))
      OReff <- exp(sqrt(b2))
      list(n = n, effect = OReff, power = power, crit = za,
           extra = sprintf("OR detectable = %.3f ; p0 = %.3f", OReff, p0))
    }
  }
}

# Puissance pour la REGRESSION DE POISSON (predicteur continu N(0,1)).
# Methode de Signorini (1991). RR = ratio de taux par ecart-type ;
# base_rate = taux moyen (exp(beta0)) ; mean_exposure = exposition/duree moyenne.
hstat_power_poisson <- function(analysis = "apriori", base_rate = 1, RR = 1.3,
                               alpha = 0.05, power = NULL, n = NULL,
                               alt = "two.sided", mean_exposure = 1, R2_other = 0) {
  b <- log(RR); za <- stats::qnorm(1 - alpha / ifelse(alt == "two.sided", 2, 1))
  V1 <- exp(b^2); denom <- base_rate * mean_exposure * b^2
  if (denom <= 0) return(list(err = "RR doit differer de 1 et le taux de base > 0."))
  if (analysis == "apriori") {
    zb <- stats::qnorm(power)
    nn <- ((za + zb * sqrt(V1))^2 / denom) / (1 - R2_other)
    list(n = ceiling(nn), effect = RR, power = power, crit = za,
         extra = sprintf("RR = %.3f ; taux de base = %.3f ; beta1 = %.4f", RR, base_rate, b))
  } else if (analysis == "posthoc") {
    zb <- (sqrt(n * (1 - R2_other) * denom) - za) / sqrt(V1)
    list(n = n, effect = RR, power = stats::pnorm(zb), crit = za,
         extra = sprintf("RR = %.3f ; taux de base = %.3f", RR, base_rate))
  } else {
    zb <- stats::qnorm(power)
    f <- function(bb) {
      V <- exp(bb^2)
      (za + zb * sqrt(V))^2 / (base_rate * mean_exposure * bb^2 * (1 - R2_other)) - n
    }
    bsol <- tryCatch(stats::uniroot(f, c(0.01, 2))$root, error = function(e) NA)
    list(n = n, effect = if (is.na(bsol)) NA else exp(bsol), power = power, crit = za,
         extra = if (is.na(bsol)) "RR non resolu" else sprintf("RR detectable = %.3f", exp(bsol)))
  }
}

# Cree un petit panneau-fleche (gradient) a juxtaposer a un graphique facette via
# patchwork, pour rendre le gradient bien visible meme avec des facettes.
.gradient_arrow_panel <- function(direction, label = "Gradient", size = 3.5,
                                  orientation = c("vertical", "horizontal")) {
  orientation <- match.arg(orientation)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  col <- if (orientation == "vertical") "#c0392b" else "#2980b9"
  if (orientation == "vertical") {
    y0 <- if (direction == "vertical_up") 0 else 1
    y1 <- if (direction == "vertical_up") 1 else 0
    ggplot2::ggplot() +
      ggplot2::annotate("segment", x = 0.5, xend = 0.5, y = y0, yend = y1,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
        color = col, linewidth = 1.3) +
      ggplot2::annotate("text", x = 0.18, y = 0.5, label = label, angle = 90,
        color = col, size = size, fontface = "bold") +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) + ggplot2::theme_void()
  } else {
    x0 <- if (direction == "horizontal_left") 1 else 0
    x1 <- if (direction == "horizontal_left") 0 else 1
    ggplot2::ggplot() +
      ggplot2::annotate("segment", x = x0, xend = x1, y = 0.5, yend = 0.5,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
        color = col, linewidth = 1.3) +
      ggplot2::annotate("text", x = 0.5, y = 0.15, label = label,
        color = col, size = size, fontface = "bold") +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) + ggplot2::theme_void()
  }
}

# Tri "naturel" (A1, A2, ..., A9, A10, A11) plutot que lexicographique
# (A1, A10, A11, A2...). Gere les etiquettes melant lettres et nombres.
.natural_sort <- function(x) {
  x <- as.character(x)
  # cle de tri : remplacer chaque bloc de chiffres par une version a largeur fixe
  key <- vapply(x, function(s) {
    gsub("([0-9]+)", "", s) -> ignore
    # extraire les nombres et les zero-padder
    nums <- regmatches(s, gregexpr("[0-9]+", s))[[1]]
    out <- s
    for (n in nums) out <- sub(n, sprintf("%020d", as.numeric(n)), out, fixed = TRUE)
    out
  }, character(1))
  x[order(key)]
}

# =============================================================================
#  DISPOSITIFS DE MALHERBOLOGIE
# -----------------------------------------------------------------------------
#  Ce ne sont PAS de nouveaux plans : ce sont des structures de traitements
#  pretes a l'emploi, posees sur les plans qui existent deja (factoriel, blocs
#  de Fisher). La randomisation, la carte, l'export et les conseils d'analyse
#  restent ceux du moteur commun -- ecrire un second moteur pour la
#  malherbologie ferait diverger les deux a la premiere correction.
#
#  Ce que la specialite apporte, en revanche, c'est le CONTENU : les modalites
#  usuelles, le modele a ajuster, et le piege propre a chaque dispositif. Un
#  essai dose-reponse sans dose nulle ni dose saturante n'estime pas ED90, il
#  l'extrapole ; une serie additive sans temoin sans adventice n'exprime aucune
#  perte. C'est cela qu'un catalogue doit dire.
# =============================================================================
hstat_malherbo_catalog <- function() {
  list(
    mh_densite_croisee = list(
      label = "Malherbologie : series de densites croisees (culture x adventice)",
      base = "factorial", r = 4,
      facteurs = list(
        "Densite_culture"   = c("C1 50%", "C2 100%", "C3 150%"),
        "Densite_adventice" = c("A0 0/m2", "A1 4/m2", "A2 8/m2", "A3 16/m2")),
      but = paste("Faire varier SIMULTANEMENT les deux densites pour estimer",
                  "leurs effets propres et surtout leur interaction."),
      mesures = paste("Culture : peuplement, hauteur, biomasse, composantes du",
                      "rendement, rendement. Adventice : densite, recouvrement,",
                      "hauteur, biomasse seche. A 15, 30, 45 et 60 JAS, puis recolte."),
      modele = "y ~ Bloc + C + A + C:A",
      analyse = paste("ANOVA factorielle. L'INTERACTION se lit avant les effets",
                      "principaux : si elle est significative, l'effet d'une densite",
                      "depend de l'autre, et les moyennes marginales n'ont plus de",
                      "sens -- passer aux effets simples."),
      piege = paste("Les deux especes changent de densite en meme temps : la",
                    "densite TOTALE varie aussi. Un effet attribue a la competition",
                    "peut n'etre qu'un effet de peuplement ; c'est le prix de ce",
                    "dispositif, et la raison d'etre de la serie additive."),
      couleur = "#8e44ad"),

    mh_serie_additive = list(
      label = "Malherbologie : serie additive (adventice croissante)",
      base = "fisher", r = 4,
      facteurs = list(
        "Densite_adventice" = c("A0 0/m2", "A1 2/m2", "A2 4/m2", "A3 8/m2",
                                "A4 16/m2", "A5 32/m2")),
      but = paste("Culture a densite CONSTANTE, adventice a densite croissante :",
                  "quantifier la perte de rendement due a l'enherbement."),
      mesures = paste("Culture : hauteur, biomasse, capsules, rendement.",
                      "Adventice : densite, recouvrement, hauteur, biomasse seche.",
                      "A 15, 30, 45 et 60 JAS, puis recolte."),
      modele = "y ~ Bloc + A  ;  perte (%) ajustee par y = i*x / (1 + i*x/a)",
      analyse = paste("ANOVA en blocs, puis ajustement d'une hyperbole de",
                      "competition (Cousens) : i donne la perte par plante a",
                      "faible densite, a la perte maximale asymptotique."),
      piege = paste("Le temoin SANS adventice (A0) n'est pas une modalite comme",
                    "les autres : c'est la reference a laquelle toutes les pertes",
                    "sont rapportees. Sans lui, aucune perte n'est calculable."),
      couleur = "#16a085"),

    mh_desherbage = list(
      label = "Malherbologie : methodes integrees de desherbage",
      base = "fisher", r = 4,
      facteurs = list(
        "Strategie" = c("T0 temoin non desherbe", "T1 manuel 15+30+45 JAS",
                        "T2 sarclage mecanique 15+30 JAS", "T3 herbicide prelevee J0",
                        "T4 herbicide postlevee 20 JAS", "T5 paillage vegetal",
                        "T6 herbicide 0.5R + sarclage 30 JAS",
                        "T7 paillage + herbicide 0.5R + manuel 30 JAS")),
      but = paste("Comparer des strategies qui combinent plusieurs leviers",
                  "(preventif, cultural, manuel, mecanique, chimique)."),
      mesures = paste("Efficacite sur les adventices (%), densite, recouvrement,",
                      "biomasse ; phytotoxicite ; peuplement, hauteur, rendement.",
                      "Et le volet economique : temps de travail, cout, nombre de",
                      "passages, dose d'herbicide."),
      modele = "y ~ Bloc + Strategie",
      analyse = paste("ANOVA en blocs puis comparaison de moyennes. Le classement",
                      "final est MULTICRITERE : une strategie tres efficace mais",
                      "couteuse en travail peut etre inapplicable."),
      piege = paste("Les strategies sont des COMBINAISONS, pas les modalites d'un",
                    "facteur : ce plan ne separe pas la part de chaque levier. Pour",
                    "cela il faut un factoriel croisant les leviers."),
      couleur = "#2980b9"),

    mh_dose_reponse = list(
      label = "Malherbologie : essai dose-reponse (ED50 / ED90)",
      base = "fisher", r = 4,
      facteurs = list(
        "Dose" = c("D0 0xR", "D1 0.25xR", "D2 0.5xR", "D3 1xR", "D4 2xR", "D5 4xR")),
      but = "Estimer la dose efficace : ED50, ED90, et la dose reduite acceptable.",
      mesures = paste("Efficacite visuelle (%), densite et biomasse des adventices,",
                      "phytotoxicite, rendement. A 7, 14, 21 et 28 JAT, puis recolte."),
      modele = "Ajustement log-logistique a 4 parametres (drc::drm, LL.4)",
      analyse = paste("La courbe dose-reponse est ajustee sur l'echelle LOG de la",
                      "dose ; ED50 et ED90 en sont deduits avec leur intervalle de",
                      "confiance. L'ANOVA sur les doses ne remplace pas cet",
                      "ajustement : elle compare des doses, elle n'en estime aucune."),
      piege = paste("Il faut encadrer la reponse aux DEUX bouts : une dose nulle et",
                    "une dose qui sature. Sans elles, ED90 n'est pas estime mais",
                    "extrapole -- avec un intervalle que le modele ne dit pas."),
      couleur = "#c0392b"),

    mh_efficacite = list(
      label = "Malherbologie : efficacite et selectivite d'un herbicide",
      base = "fisher", r = 4,
      facteurs = list(
        "Traitement" = c("T0 temoin enherbe", "T1 temoin propre",
                         "T2 reference 1xR", "T3 teste 0.5xR", "T4 teste 1xR",
                         "T5 teste 2xR")),
      but = paste("Mesurer d'un meme dispositif le controle des adventices ET la",
                  "tolerance de la culture."),
      mesures = paste("Efficacite : recouvrement, densite, controle visuel,",
                      "biomasse seche, a 7, 14, 21 et 28 JAT. Selectivite :",
                      "phytotoxicite, vigueur, peuplement, hauteur, rendement."),
      modele = "y ~ Bloc + Traitement",
      analyse = paste("ANOVA en blocs et comparaison aux deux temoins. L'efficacite",
                      "se rapporte au temoin ENHERBE ; la selectivite et le potentiel",
                      "de rendement, au temoin PROPRE."),
      piege = paste("Les deux temoins sont indispensables et ne se remplacent pas.",
                    "Sans temoin enherbe, aucune efficacite ne se calcule ; sans",
                    "temoin propre, une baisse de rendement ne se distingue pas de",
                    "la concurrence residuelle des adventices."),
      couleur = "#27ae60"),

    mh_periode_critique = list(
      label = "Malherbologie : duree de competition (periode critique)",
      base = "fisher", r = 4,
      facteurs = list(
        "Duree" = c("ET enherbe permanent", "E15 enherbe jusqu'a 15 JAS",
                    "E30 enherbe jusqu'a 30 JAS", "E45 enherbe jusqu'a 45 JAS",
                    "E60 enherbe jusqu'a 60 JAS", "P15 propre jusqu'a 15 JAS",
                    "P30 propre jusqu'a 30 JAS", "P45 propre jusqu'a 45 JAS",
                    "P60 propre jusqu'a 60 JAS", "PT propre permanent")),
      but = paste("Deux series complementaires. E : enherbement initial croissant,",
                  "puis desherbage definitif -- jusqu'a quand peut-on attendre.",
                  "P : maintien propre initial croissant, puis re-enherbement --",
                  "a partir de quand peut-on arreter. Leur croisement donne la",
                  "PERIODE CRITIQUE de desherbage."),
      mesures = paste("Adventices : densite, recouvrement, biomasse.",
                      "Culture : hauteur, biomasse, capsules, rendement.",
                      "A 15, 30, 45 et 60 JAS, puis recolte. Perte de rendement",
                      "PR (%) = 100 x (Y_propre - Y_traitement) / Y_propre."),
      modele = "y ~ Bloc + Duree ; puis 2 courbes ajustees (logistiques) E et P",
      analyse = paste("ANOVA en blocs, puis ajustement d'une courbe par serie. La",
                      "periode critique se lit a l'intersection des deux courbes",
                      "avec le seuil de perte acceptable -- souvent 5 %."),
      piege = paste("Les DEUX temoins permanents sont indispensables : le propre",
                    "(PT) donne le denominateur Y_propre de toute perte, l'enherbe",
                    "(ET) borne la perte maximale. Et le seuil de perte acceptable",
                    "est une decision economique, pas un resultat statistique : il",
                    "se declare avant de lire les courbes."),
      couleur = "#d35400"),

    mh_date_frequence = list(
      label = "Malherbologie : date et frequence de desherbage",
      base = "fisher", r = 4,
      facteurs = list(
        "Calendrier" = c("T0 aucun desherbage", "TP propre tout le cycle",
                         "F1D15 1 fois a 15 JAS", "F1D30 1 fois a 30 JAS",
                         "F1D45 1 fois a 45 JAS", "F2D15 2 fois des 15 JAS",
                         "F2D30 2 fois des 30 JAS", "F2D45 2 fois des 45 JAS",
                         "F3D15 3 fois des 15 JAS", "F3D30 3 fois des 30 JAS")),
      but = paste("Trouver la date de premiere intervention et la frequence",
                  "MINIMALE qui preservent le rendement."),
      mesures = paste("Adventices : densite, recouvrement, biomasse.",
                      "Culture : hauteur, vigueur, capsules, rendement.",
                      "Et le cout du calendrier : temps de travail (h/ha) et",
                      "depense (FCFA/ha) -- c'est ce qu'on cherche a reduire."),
      modele = "y ~ Bloc + Calendrier",
      analyse = paste("ANOVA en blocs et comparaison de moyennes. Le calendrier",
                      "retenu est le moins couteux dont le rendement ne differe",
                      "pas du temoin propre."),
      piege = paste("Ce n'est PAS un factoriel date x frequence : les combinaisons",
                    "tardives et frequentes sortiraient du cycle. Date et frequence",
                    "sont donc confondues -- on compare des calendriers entiers, pas",
                    "les effets propres de l'une et de l'autre."),
      couleur = "#7f8c8d"),

    mh_paires = list(
      label = "Malherbologie : parcelles appariees (traite / temoin)",
      base = "paired", r = 4,
      facteurs = list("Traitement" = c("T0 temoin non traite", "T1 herbicide")),
      but = paste("Comparer un traitement a son temoin dans des paires locales",
                  "HOMOGENES, quand le terrain est trop heterogene pour des blocs",
                  "complets."),
      mesures = paste("Adventices : densite, recouvrement, biomasse, controle",
                      "visuel. Culture : phytotoxicite, peuplement, rendement.",
                      "Les quadrats se correspondent d'une parcelle a l'autre."),
      modele = "D = Y(T1) - Y(T0) par paire ; test sur les differences",
      analyse = paste("Analyse INTRAPAIRE : test t apparie, ou Wilcoxon signe si",
                      "la normalite des differences n'est pas tenable. Comparer les",
                      "deux groupes sans tenir compte de l'appariement gaspille",
                      "toute la precision gagnee."),
      piege = paste("L'appariement doit reposer sur une homogeneite REELLE (meme",
                    "position sur le gradient, meme sol) : apparier au hasard",
                    "n'apporte rien. Et une paire dont une parcelle est perdue est",
                    "perdue ENTIERE -- la difference n'existe plus."),
      couleur = "#2c3e50"),

    mh_bandes_traitees = list(
      label = "Malherbologie : bandes traitees (pulverisateur)",
      base = "fisher", r = 4,
      facteurs = list(
        "Traitement" = c("T0 temoin non traite", "T1 herbicide de reference",
                         "T2 teste dose 1", "T3 teste dose 2", "T4 teste dose 3")),
      but = paste("Appliquer les traitements en BANDES longues, quand le materiel",
                  "de pulverisation ne peut pas travailler sur de petites parcelles."),
      mesures = paste("Efficacite : densite, recouvrement, biomasse, controle",
                      "visuel. Culture : phytotoxicite, peuplement, rendement."),
      modele = "y ~ Bloc + Traitement",
      analyse = "ANOVA en blocs ; la BANDE est l'unite experimentale.",
      piege = paste("Les quadrats d'une meme bande ne sont pas des repetitions :",
                    "les prendre pour telles multiplie artificiellement les degres",
                    "de liberte et rend significatif ce qui ne l'est pas. Ils se",
                    "moyennent par bande. Prevoir en outre des zones tampon entre",
                    "bandes : la derive de pulverisation traite le voisin."),
      couleur = "#9b59b6"),

    mh_bandes_croisees = list(
      label = "Malherbologie : bandes croisees (strip-plot)",
      base = "strip", r = 4,
      facteurs = list(
        "Facteur_A_bandes_verticales" = c("A1", "A2", "A3"),
        "Facteur_B_bandes_horizontales" = c("B1", "B2", "B3", "B4")),
      but = paste("Croiser deux facteurs qui exigent chacun des bandes -- un",
                  "travail du sol et une pulverisation, par exemple. Les parcelles",
                  "elementaires naissent a l'intersection."),
      mesures = paste("Adventices et culture, aux dates habituelles ; l'unite de",
                      "mesure est l'intersection A x B."),
      modele = "y ~ A * B + Error(Bloc/A) + Error(Bloc/B)",
      analyse = paste("Trois termes d'erreur : bandes A, bandes B, et",
                      "l'intersection. L'INTERACTION y est estimee plus",
                      "precisement que les deux effets principaux -- l'inverse du",
                      "split-plot, et c'est ce qui justifie ce plan."),
      piege = paste("Les deux facteurs sont randomises INDEPENDAMMENT dans chaque",
                    "repetition ; les effets principaux y sont mesures avec peu de",
                    "precision. Choisir ce plan pour comparer des modalites de A",
                    "entre elles serait un contresens."),
      couleur = "#16a085"),

    mh_serie_substitutive = list(
      label = "Malherbologie : serie substitutive (remplacement)",
      base = "fisher", r = 4,
      facteurs = list(
        "Proportion" = c("S0 100% coton + 0% adventice", "S1 75% coton + 25% adventice",
                         "S2 50% coton + 50% adventice", "S3 25% coton + 75% adventice",
                         "S4 0% coton + 100% adventice")),
      but = paste("Densite TOTALE constante, proportions complementaires : chaque",
                  "plant d'une espece remplace un plant de l'autre. On compare les",
                  "deux especes a armes egales, sans changer le peuplement."),
      mesures = paste("Par ESPECE : biomasse seche, hauteur, surface foliaire.",
                      "Culture : capsules, rendement. Adventice : biomasse et",
                      "production de graines. Les peuplements purs fournissent les",
                      "references Yc,pur et Ya,pur."),
      modele = paste("y ~ Bloc + Proportion ; puis RYc = Yc,melange / Yc,pur,",
                     "RYa = Ya,melange / Ya,pur, RYT = RYc + RYa"),
      analyse = paste("ANOVA en blocs, puis rendements relatifs. RYT = 1 : les deux",
                      "especes se disputent les MEMES ressources. RYT > 1 : leurs",
                      "niches different et le melange produit plus que la somme",
                      "attendue. RYT < 1 : antagonisme mutuel."),
      piege = paste("Les deux peuplements PURS (S0 et S4) ne sont pas des modalites",
                    "comme les autres : ce sont les denominateurs de RYc et de RYa.",
                    "Sans eux, aucun rendement relatif ne se calcule. Et le",
                    "resultat ne vaut QUE pour la densite totale choisie -- une",
                    "serie substitutive ne dit rien de l'effet de la densite",
                    "elle-meme, c'est la limite qui justifie la serie additive."),
      couleur = "#5b2c6f")
  )
}

# Plan de base sur lequel repose un dispositif de malherbologie ; le type
# lui-meme pour tous les autres.
hstat_design_base <- function(type) {
  mh <- hstat_malherbo_catalog()[[type %||% ""]]
  if (is.null(mh)) type else mh$base
}
hstat_design_catalog <- function() {
  c("Completement randomise (DCR/CRD)" = "crd",
    "Bloc de Fisher (RCBD)" = "fisher",
    "Couple apparie (BCR)" = "paired",
    "Carre latin (LSD)" = "lsd",
    "Alpha Lattice (alpha-design)" = "alpha",
    "Factoriel complet" = "factorial",
    "Split-plot (parcelles divisees)" = "split",
    "Split-split-plot (3 facteurs)" = "splitsplit",
    "Criss-Cross / Strip-plot (bandes)" = "strip",
    stats::setNames(names(hstat_malherbo_catalog()),
                    vapply(hstat_malherbo_catalog(), function(x) x$label, character(1))))
}

hstat_agri_design <- function(type, factors, r = 3, seed = 123, k = NULL,
                              base_design = "rcbd", k_mode = "exact") {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  # Un dispositif de malherbologie n'est pas un plan de plus : c'est une
  # structure de traitements posee sur un plan existant. On le ramene donc
  # ici, une seule fois, et tout le reste du moteur l'ignore.
  type <- hstat_design_base(type)
  fnames <- names(factors)
  # NB : le check de disponibilite d'agricolae est place APRES le bloc alpha, car le
  # generateur alpha-lattice generalise n'utilise que du R de base (aucune dependance
  # a agricolae). Il doit donc rester disponible meme si agricolae n'est pas installe.
  # Pre-validation des contraintes specifiques (messages clairs au lieu d'erreurs
  # cryptiques d'agricolae).
  # Generateur de plan alpha-lattice GENERALISE (blocs incomplets resolvables) :
  # a chaque replique, les traitements sont randomises puis decoupes en blocs de
  # taille k (le dernier bloc peut etre plus petit). Fonctionne pour TOUT nombre de
  # traitements et TOUTE taille de bloc k >= 2. Sert aussi de filet de securite
  # quand agricolae::design.alpha echoue (contraintes internes trop strictes).
  .build_generalized_alpha <- function(trts, kk2, rr, seed) {
    nt <- length(trts)
    if (is.null(kk2) || is.na(kk2) || kk2 < 2 || kk2 >= nt) kk2 <- max(2, floor(sqrt(nt)))
    kk2 <- as.integer(kk2)
    set.seed(seed)
    rows <- list(); idx <- 1L; plot_id <- 1L
    for (rep_i in seq_len(rr)) {
      # `sample(x, n)` PIOCHE DANS 1:x QUAND x EST UN SEUL NOMBRE : un essai a un
      # seul traitement code numeriquement (« 1 », « 10 ») verrait apparaitre un
      # traitement qui n'existe pas. On tire donc sur les INDICES.
      ord <- trts[sample.int(nt)]                    # randomisation dans la replique
      nblocks <- ceiling(nt / kk2)
      blk_vec <- rep(seq_len(nblocks), each = kk2)[seq_len(nt)]
      for (j in seq_len(nt)) {
        rows[[idx]] <- data.frame(
          plots = plot_id,
          replication = rep_i,
          block = (rep_i - 1L) * nblocks + blk_vec[j],
          block_in_rep = blk_vec[j],
          Traitement = ord[j],
          stringsAsFactors = FALSE)
        idx <- idx + 1L; plot_id <- plot_id + 1L
      }
    }
    book_ib <- do.call(rbind, rows)
    names(book_ib)[names(book_ib) == "Traitement"] <- fnames[1]
    book_ib$Traitement <- as.character(book_ib[[fnames[1]]])
    attr(book_ib, "design") <- "alpha"
    attr(book_ib, "used_k") <- kk2
    reste <- nt %% kk2
    attr(book_ib, "alpha_generalized") <- trf(
      "Alpha Lattice généralisé : %d traitements répartis en blocs incomplets de taille %d (%s). Plan résolvable valable pour tout nombre de traitements.",
      nt, kk2, if (reste == 0) tr("blocs égaux")
               else trf("dernier bloc de %d", reste))
    book_ib
  }

  if (type == "alpha") {
    nt <- length(factors[[1]])
    kk <- k %||% 3
    rr <- r %||% 3
    if (rr < 2)
      stop("Alpha Lattice : au moins 2 répétitions sont requises.")
    kk <- suppressWarnings(as.integer(kk))
    if (is.na(kk) || kk < 2) kk <- max(2L, as.integer(floor(sqrt(nt))))
    # Valeurs de k "parfaites" pour agricolae : k divise nt, k < nt, et s = nt/k >= k.
    valid_k <- Filter(function(d) d >= 2 && d < nt && nt %% d == 0 && (nt / d) >= d, 2:(nt-1))

    # MODE "exact" : on respecte STRICTEMENT le k saisi par l'utilisateur, via le
    # generateur generalise (blocs de taille k, dernier bloc plus petit si nt %% k != 0).
    # Fonctionne pour TOUT k >= 2 et tout nombre de traitements. C'est le mode par defaut.
    if (identical(k_mode, "exact")) {
      return(.build_generalized_alpha(factors[[1]], kk, rr, seed))
    }

    # MODE "auto" : on optimise vers un k parfait proche (plan equilibre) quand il
    # en existe un, sinon on retombe sur le generateur generalise.
    if (length(valid_k) > 0 && !(kk %in% valid_k)) {
      kk <- valid_k[which.min(abs(valid_k - kk))]
      k <- kk
    }
    # Tenter agricolae UNIQUEMENT si un k parfait existe ET qu'agricolae est installe.
    # En cas d'echec (agricolae peut lever "subscript out of bounds" selon r et le
    # nombre de blocs/replique) OU si agricolae est absent, on bascule sur le
    # generateur generalise. Ainsi le dispositif alpha-lattice reussit dans TOUS les
    # contextes, sans jamais afficher d'erreur, meme sans agricolae installe.
    if (length(valid_k) > 0 && requireNamespace("agricolae", quietly = TRUE)) {
      res <- tryCatch(
        suppressWarnings(suppressMessages(
          agricolae::design.alpha(trt = factors[[1]], k = kk, r = rr, seed = seed))),
        error = function(e) NULL)
      if (!is.null(res) && !is.function(res) && !is.null(res$book) && is.data.frame(res$book)) {
        return(res$book)
      }
      # agricolae a echoue -> filet de securite generalise avec le k demande.
      return(.build_generalized_alpha(factors[[1]], kk, rr, seed))
    }
    # Pas de k parfait, OU agricolae absent -> generateur generalise (R de base).
    return(.build_generalized_alpha(factors[[1]], kk, rr, seed))
  }

  # SPLIT-SPLIT-PLOT (3 facteurs) : randomisation hierarchique a 3 niveaux, sans
  # dependance a agricolae (agricolae ne fournit pas design.split a 3 facteurs).
  #  - Facteur 1 (parcelle principale)     : randomise sur les grandes parcelles
  #    de chaque bloc/replique.
  #  - Facteur 2 (sous-parcelle)           : randomise DANS chaque parcelle principale.
  #  - Facteur 3 (sous-sous-parcelle)      : randomise DANS chaque sous-parcelle.
  # Structure resolvable, valable pour tout nombre de modalites par facteur.
  if (type == "splitsplit") {
    if (length(factors) < 3)
      stop("Le split-split-plot necessite 3 facteurs.")
    f1 <- factors[[1]]; f2 <- factors[[2]]; f3 <- factors[[3]]
    n1 <- length(f1); n2 <- length(f2); n3 <- length(f3)
    rr <- r %||% 3
    set.seed(seed)
    rows <- list(); idx <- 1L; plot_id <- 1L
    for (rep_i in seq_len(rr)) {
      main_order <- f1[sample.int(n1)]                 # ordre des parcelles principales
      for (mp in seq_len(n1)) {
        sub_order <- f2[sample.int(n2)]                # sous-parcelles randomisees
        for (sp in seq_len(n2)) {
          subsub_order <- f3[sample.int(n3)]           # sous-sous-parcelles randomisees
          for (ssp in seq_len(n3)) {
            rows[[idx]] <- data.frame(
              plots = plot_id,
              block = rep_i,
              mainplot = (rep_i - 1L) * n1 + mp,
              subplot  = sp,
              subsubplot = ssp,
              F1 = main_order[mp],
              F2 = sub_order[sp],
              F3 = subsub_order[ssp],
              stringsAsFactors = FALSE)
            idx <- idx + 1L; plot_id <- plot_id + 1L
          }
        }
      }
    }
    book <- do.call(rbind, rows)
    # Renommer F1/F2/F3 avec les noms de facteurs fournis par l'utilisateur.
    names(book)[names(book) == "F1"] <- fnames[1]
    names(book)[names(book) == "F2"] <- fnames[2]
    names(book)[names(book) == "F3"] <- fnames[3]
    book$Traitement <- apply(book[, fnames[1:3], drop = FALSE], 1,
                             function(z) paste(z, collapse = " | "))
    attr(book, "design") <- "splitsplit"
    return(book)
  }

  # Les autres dispositifs (crd, fisher, lsd, factoriel, bib...) requierent agricolae.
  if (!requireNamespace("agricolae", quietly = TRUE))
    stop("Le package 'agricolae' est requis pour générer ce type de plan. Installez-le avec install.packages('agricolae').")
  # Generateur robuste : agricolae renvoie parfois une closure (pas de generateur
  # disponible pour la combinaison) au lieu d'un design -> message clair.
  .safe_book <- function(res, label) {
    if (is.null(res) || is.function(res) || is.null(res$book) || !is.data.frame(res$book))
      stop(trf("%s : aucune configuration valide pour cette combinaison (nombre de traitements, taille de bloc k, répétitions). Essayez d'autres valeurs.", label))
    res$book
  }
  book <- switch(type,
    "crd" = agricolae::design.crd(trt = factors[[1]], r = r, seed = seed)$book,
    "fisher" = agricolae::design.rcbd(trt = factors[[1]], r = r, seed = seed)$book,
    "paired" = agricolae::design.rcbd(trt = factors[[1]], r = r, seed = seed)$book,
    "lsd" = agricolae::design.lsd(trt = factors[[1]], seed = seed)$book,
    "alpha" = .build_generalized_alpha(factors[[1]], k %||% 3, r %||% 3, seed),  # filet (normalement atteint plus haut via return())
    "factorial" = {
      d <- agricolae::design.ab(trt = lengths(factors), r = r, design = base_design, seed = seed)$book
      labs <- LETTERS[seq_along(fnames)]
      # Construire d'abord les colonnes nommees dans un data.frame separe
      mapped <- lapply(seq_along(fnames), function(i) {
        factors[[i]][as.integer(as.character(d[[labs[i]]]))]
      })
      names(mapped) <- fnames
      # Retirer toutes les colonnes-labels d'origine, puis ajouter les colonnes mappees
      d <- d[, setdiff(names(d), labs), drop = FALSE]
      for (nm in names(mapped)) d[[nm]] <- mapped[[nm]]
      d
    },
    "split" = agricolae::design.split(trt1 = factors[[1]], trt2 = factors[[2]], r = r, design = base_design, seed = seed)$book,
    "strip" = agricolae::design.strip(trt1 = factors[[1]], trt2 = factors[[2]], r = r, seed = seed)$book,
    stop("Type de plan inconnu : ", type))
  book <- .hstat_rename_treat_cols(book, factors)
  present <- intersect(fnames, names(book))
  if (length(present) >= 2)
    book$Traitement <- apply(book[, present, drop = FALSE], 1, function(z) paste(z, collapse = " | "))
  else if (length(present) == 1)
    book$Traitement <- as.character(book[[present]])
  attr(book, "design") <- type
  book
}

hstat_sample_allocation <- function(factors, r) {
  treat <- expand.grid(factors, stringsAsFactors = FALSE)
  n_treat <- nrow(treat)
  data.frame(
    Indicateur = c("Nombre de traitements (combinaisons)", "Nombre de répétitions / blocs",
                   "Unités par répétition", "Taille totale de l'échantillon (N)"),
    Valeur = c(n_treat, r, n_treat, n_treat * r), stringsAsFactors = FALSE)
}

# Calcule le ddl du numerateur et le nombre de cellules pour un plan factoriel,
# a partir du vecteur des modalites par facteur et de l'effet cible.
#   levels_vec : nb de modalites de chaque facteur, ex c(3, 2, 4)
#   target : "main1".."main4" (effet principal du facteur i),
#            "inter2"/"inter3"/"inter4" (interaction des i premiers facteurs),
#            "interAll" (interaction de tous les facteurs)
hstat_factorial_df <- function(levels_vec, target = "main1") {
  levels_vec <- as.integer(levels_vec)
  levels_vec <- levels_vec[!is.na(levels_vec) & levels_vec >= 2]
  if (length(levels_vec) == 0) return(list(df1 = 1, cells = 2))
  cells <- prod(levels_vec)
  df1 <- if (grepl("^main", target)) {
    i <- as.integer(sub("main", "", target))
    if (is.na(i) || i > length(levels_vec)) i <- 1
    levels_vec[i] - 1
  } else if (target == "interAll") {
    prod(levels_vec - 1)
  } else if (grepl("^inter", target)) {
    k <- as.integer(sub("inter", "", target))
    if (is.na(k) || k > length(levels_vec)) k <- length(levels_vec)
    prod(levels_vec[seq_len(k)] - 1)
  } else levels_vec[1] - 1
  list(df1 = as.integer(max(1, df1)), cells = as.integer(cells))
}

# Repartit le nombre d'unités requis par groupe (nper, issu du calcul de puissance)
# sur le plan : choisit le nombre de répétitions (R) et d'échantillons par parcelle
# élémentaire (m) tels que R * m >= nper.
#  - mode "blocks"       : R = nper, m = 1 (chaque répétition = un bloc/parcelle).
#  - mode "subsampling"  : equilibre R et m (utile quand nper est grand).
hstat_repartition <- function(nper, n_treatments, mode = "blocks",
                              max_blocks = NULL) {
  nper <- max(1, ceiling(nper))
  if (mode == "blocks" && (is.null(max_blocks) || nper <= max_blocks)) {
    R <- nper; m <- 1
  } else if (mode == "blocks") {
    # nper depasse le nb de blocs souhaite -> completer par sous-échantillonnage
    R <- max_blocks; m <- ceiling(nper / R)
  } else {
    m <- max(1, floor(sqrt(nper))); R <- ceiling(nper / m)
  }
  list(`répétitions` = R, samples_per_plot = m,
       per_treatment = R * m, total = R * m * n_treatments,
       requested = nper)
}

# Place les unités d'un field book sur une grille (x, y) selon le dispositif.
# - CRD / factoriel : aucune structure spatiale imposee -> randomisation spatiale
#   qui MINIMISE le nombre de voisins (haut/bas/gauche/droite) de meme traitement,
#   pour eviter les bandes monochromes trompeuses.
# - RCBD / alpha / lattice / bib / split / strip : la structure (bloc) est
#   respectee ; l'ordre interne (deja randomise par agricolae) est conserve.
# - LSD : lignes x colonnes du carre latin.
hstat_place_design <- function(book, type, fill_col, seed = 123, tries = 200) {
  b <- book
  # Re-randomisation independante de l'ordre des traitements DANS chaque bloc.
  # Certains plans agricolae (ex. design.ab sous certaines graines) produisent un
  # ordre quasi identique d'un bloc a l'autre ; on garantit ici une vraie
  # randomisation intra-bloc, propre a chaque dispositif.
  .reshuffle_within <- function(df, group_col, seed, fill = fill_col, tries_cap = 250) {
    if (!group_col %in% names(df)) return(df)
    grp <- df[[group_col]]
    ug <- unique(grp)
    out_idx <- integer(0)
    prev_treats <- NULL   # vecteur des traitements (par position) du bloc precedent
    prev2_treats <- NULL  # avant-dernier bloc (pour eviter les colonnes identiques sur 2 blocs)
    for (gi in seq_along(ug)) {
      rows <- which(grp == ug[gi])
      if (length(rows) <= 1) { out_idx <- c(out_idx, rows); prev2_treats <- prev_treats; prev_treats <- as.character(df[[fill]][rows]); next }
      base_seed <- as.integer((seed %% 100000L) * 131L + gi * 1299709L) %% .Machine$integer.max
      best_perm <- NULL; best_score <- Inf
      # On essaie plusieurs permutations et on garde celle qui :
      #  (a) minimise les voisins identiques DANS le bloc (positions adjacentes)
      #  (b) minimise les traitements a la MEME position que le bloc precedent
      #  (c) minimise aussi les collisions avec l'avant-dernier bloc (colonne sur 3 blocs)
      for (tt in seq_len(tries_cap)) {
        set.seed(base_seed + tt * 7919L)
        perm <- rows[sample.int(length(rows))]
        tr <- as.character(df[[fill]][perm])
        same_adj <- if (length(tr) > 1) sum(tr[-1] == tr[-length(tr)]) else 0L
        col_clash <- if (!is.null(prev_treats)) {
          m <- min(length(tr), length(prev_treats))
          sum(tr[seq_len(m)] == prev_treats[seq_len(m)])
        } else 0L
        col_clash2 <- if (!is.null(prev2_treats)) {
          m <- min(length(tr), length(prev2_treats))
          sum(tr[seq_len(m)] == prev2_treats[seq_len(m)])
        } else 0L
        # collisions diagonales avec le bloc precedent (position i vs i-1 et i+1)
        diag_clash <- if (!is.null(prev_treats) && length(tr) > 1) {
          m <- min(length(tr), length(prev_treats))
          d1 <- sum(tr[2:m] == prev_treats[1:(m-1)])
          d2 <- sum(tr[1:(m-1)] == prev_treats[2:m])
          d1 + d2
        } else 0L
        score <- same_adj * 100L + col_clash * 10L + col_clash2 * 3L + diag_clash
        if (is.na(score)) score <- .Machine$integer.max
        if (is.null(best_perm) || score < best_score) { best_score <- score; best_perm <- perm }
        if (!is.na(best_score) && best_score == 0) break
      }
      out_idx <- c(out_idx, best_perm)
      prev2_treats <- prev_treats
      prev_treats <- as.character(df[[fill]][best_perm])
    }
    df[out_idx, , drop = FALSE]
  }
  # Re-randomisation intra-bloc pour TOUS les dispositifs structures en blocs.
  grp_col <- intersect(c("block", "replication"), names(b))[1]
  if (!is.na(grp_col) && type %in% c("fisher", "paired", "factorial", "alpha", "split", "strip")) {
    b <- .reshuffle_within(b, grp_col, seed)
  }
  has_block <- "block" %in% names(b)
  has_rep   <- "replication" %in% names(b)
  if (type == "crd") {
    # Completement randomise : chaque REPETITION occupe une rangee complete
    # contenant tous les traitements (1 colonne par modalite), ordre randomise.
    # Le milieu est suppose homogene (aucun gradient).
    vals <- as.character(b[[fill_col]])
    n  <- length(vals)
    n_trt <- length(unique(vals))
    # Reconstituer les répétitions : chaque traitement apparait r fois ; on attribue
    # un numéro de répétition par occurrence de chaque traitement.
    occ <- stats::ave(seq_len(n), vals, FUN = seq_along)  # 1..r pour chaque trt
    nrep <- max(occ)
    set.seed(seed + 1)
    # Pour chaque répétition, randomiser l'ordre des traitements presents
    b$.rep <- occ
    ord_idx <- integer(0)
    for (rr in seq_len(nrep)) {
      rows <- which(occ == rr)
      set.seed(as.integer((seed %% 100000L) * 131L + rr * 1299709L) %% .Machine$integer.max)
      ord_idx <- c(ord_idx, rows[sample.int(length(rows))])
    }
    b <- b[ord_idx, , drop = FALSE]
    b$.y <- b$.rep
    b$.x <- stats::ave(seq_len(nrow(b)), b$.y, FUN = seq_along)
    attr(b, "xlab") <- "Parcelle (1 colonne par modalité, ordre randomise)"
    attr(b, "ylab") <- "Répétition"
    attr(b, "gradient") <- "none"
  } else if (type == "fisher" && has_block) {
    # Bloc de Fisher (RCBD classique, image 1) : chaque bloc est une RANGEE
    # complete contenant tous les traitements randomises ; blocs empiles
    # verticalement, gradient perpendiculaire aux blocs (vertical).
    blk <- as.integer(as.factor(b$block))
    b$.y <- blk
    b$.x <- stats::ave(seq_len(nrow(b)), blk, FUN = seq_along)
    attr(b, "xlab") <- "Parcelle (traitement randomise dans le bloc)"
    attr(b, "ylab") <- "Bloc"
    attr(b, "gradient") <- "vertical"
  } else if (type == "paired" && has_block) {
    # Couple apparie (BCR) : representation DIAGONALE en escalier -- chaque bloc
    # (paire) est decale d'un cran par rapport au precedent.
    blk <- as.integer(as.factor(b$block))
    pos_in_blk <- stats::ave(seq_len(nrow(b)), blk, FUN = seq_along)
    b$.y <- blk
    b$.x <- pos_in_blk + (blk - 1)
    attr(b, "xlab") <- "Position (decalage diagonal par bloc)"
    attr(b, "ylab") <- "Bloc"
    attr(b, "gradient") <- "diagonal"
  } else if (type == "lsd" && all(c("row", "col") %in% names(b))) {
    # Carre latin : 2 gradients orthogonaux (ligne ET colonne)
    b$.y <- as.integer(as.factor(b$row)); b$.x <- as.integer(as.factor(b$col))
    attr(b, "xlab") <- "Colonne (gradient 2)"; attr(b, "ylab") <- "Ligne (gradient 1)"
    attr(b, "gradient") <- "both"
  } else if (type == "factorial") {
    # Factoriel : organise EN BLOCS (chaque bloc = une replication contient toutes
    # les combinaisons), blocs empiles en rangees -> les blocs sont visibles.
    blkcol <- if (has_block) "block" else if (has_rep) "replication" else NULL
    if (!is.null(blkcol)) {
      blk <- as.integer(as.factor(b[[blkcol]]))
      b$.y <- blk
      b$.x <- stats::ave(seq_len(nrow(b)), blk, FUN = seq_along)
      attr(b, "xlab") <- "Combinaison dans le bloc"; attr(b, "ylab") <- "Bloc"
      attr(b, "gradient") <- "vertical"
    } else {
      n <- nrow(b); nc <- ceiling(sqrt(n))
      b$.x <- ((seq_len(n)-1) %% nc) + 1; b$.y <- ((seq_len(n)-1) %/% nc) + 1
      attr(b, "xlab") <- "Colonne"; attr(b, "ylab") <- "Rangee"
      attr(b, "gradient") <- "none"
    }
  } else if (type == "strip" && has_block) {
    fcols <- setdiff(names(b), c("plots","block","row","col","splots",".x",".y",
                                 "Traitement","n_échantillon", fill_col))
    f1 <- fcols[1]; f2 <- if (length(fcols) >= 2) fcols[2] else fcols[1]
    n_f1 <- length(unique(b[[f1]]))
    b$.x <- as.integer(as.factor(b[[f1]])) + (as.integer(as.factor(b$block)) - 1) * (n_f1 + 1)
    b$.y <- as.integer(as.factor(b[[f2]]))
    attr(b, "xlab") <- paste0("Bandes ", f1, " (perpendiculaires, par bloc)")
    attr(b, "ylab") <- paste0("Bandes ", f2, " (perpendiculaires)")
    attr(b, "gradient") <- "both"
  } else if (type == "split" && has_block) {
    fcols <- setdiff(names(b), c("plots","block","row","col","splots",".x",".y",
                                 "Traitement","n_échantillon", fill_col))
    fmain <- fcols[1]; fsub <- if (length(fcols) >= 2) fcols[2] else fcols[1]
    b$.y <- as.integer(as.factor(b$block))
    b$.x <- stats::ave(seq_len(nrow(b)), b$.y, FUN = seq_along)
    attr(b, "xlab") <- paste0("Grandes parcelles (", fmain, ") -> sous-parcelles (", fsub, ")")
    attr(b, "ylab") <- "Bloc"
    attr(b, "gradient") <- "vertical"
  } else if (type == "alpha") {
    # Alpha lattice (image 4) : blocs incomplets resolvables, organises par
    # replication. Chaque replication = un panneau ; a l'intérieur, les blocs
    # incomplets sont empiles. On dispose les blocs incomplets en rangees, avec
    # decalage horizontal par replication pour distinguer les repliques.
    repcol <- if ("replication" %in% names(b)) "replication" else NULL
    blkcol <- if ("block" %in% names(b)) "block" else NULL
    if (!is.null(repcol) && !is.null(blkcol)) {
      repi <- as.integer(as.factor(b[[repcol]]))
      blki <- as.integer(as.factor(b[[blkcol]]))
      # taille du bloc incomplet
      kk <- max(stats::ave(seq_len(nrow(b)), interaction(repi, blki, drop = TRUE), FUN = length))
      b$.y <- blki
      pos_in_blk <- stats::ave(seq_len(nrow(b)), interaction(repi, blki, drop = TRUE), FUN = seq_along)
      n_rep <- max(repi)
      b$.x <- pos_in_blk + (repi - 1) * (kk + 1)   # decalage horizontal par replique
      attr(b, "xlab") <- "Blocs incomplets (par réplique, decales horizontalement)"
      attr(b, "ylab") <- "Bloc incomplet"
      attr(b, "gradient") <- "vertical"
      attr(b, "alpha_rep") <- repi
      attr(b, "alpha_k") <- kk
    } else {
      n <- nrow(b); nc <- ceiling(sqrt(n))
      b$.x <- ((seq_len(n)-1) %% nc) + 1; b$.y <- ((seq_len(n)-1) %/% nc) + 1
      attr(b, "gradient") <- "vertical"
    }
  } else {
    blk <- intersect(c("block", "replication"), names(b))[1]
    if (is.na(blk) || is.null(blk)) {
      n <- nrow(b); nc <- ceiling(sqrt(n))
      b$.x <- ((seq_len(n) - 1) %% nc) + 1; b$.y <- ((seq_len(n) - 1) %/% nc) + 1
      attr(b, "xlab") <- "Colonne"; attr(b, "ylab") <- "Rangee"
    } else {
      b$.y <- as.integer(as.factor(b[[blk]]))
      b$.x <- stats::ave(seq_len(nrow(b)), b$.y, FUN = seq_along)
      attr(b, "xlab") <- "Position dans le bloc"; attr(b, "ylab") <- "Bloc"
    }
  }
  b
}

hstat_design_analysis <- function(type, n_factors) {
  mh <- hstat_malherbo_catalog()[[type %||% ""]]
  if (!is.null(mh))
    return(list(nom = mh$label, modele = mh$modele,
                analyse = paste(mh$analyse, "--", mh$piege)))
  switch(type,
    "crd" = list(nom = "Completement randomise (DCR/CRD)",
      modele = if (n_factors >= 2) "y ~ A * B (factoriel)" else "y ~ Traitement",
      analyse = "ANOVA a effets fixes ; post-hoc Tukey (agricolae::HSD.test). Vérifier normalité (Shapiro) et homogénéité (Levene/Bartlett). Non parametrique : Kruskal-Wallis."),
    "fisher" = list(nom = "Bloc de Fisher (RCBD)",
      modele = "y ~ Traitement + block",
      analyse = "ANOVA avec le bloc en effet (blocs complets randomises). Post-hoc Tukey/LSD. Non parametrique : Friedman."),
    "paired" = list(nom = "Couple apparie (BCR)",
      modele = "y ~ Traitement + block",
      analyse = "Comparaison appariée par bloc. Pour 2 traitements : test t apparie (ou Wilcoxon signe). Plus de 2 : ANOVA en blocs / Friedman."),
    "lsd" = list(nom = "Carre latin (LSD)",
      modele = "y ~ Traitement + row + col",
      analyse = "ANOVA controlant deux sources de variation (ligne et colonne)."),
    "alpha" = list(nom = "Alpha Lattice",
      modele = "y ~ Traitement + replication + block:replication",
      analyse = "Modèle mixte (bloc incomplet aleatoire). Recuperation inter-bloc ; plus efficace que RCBD pour beaucoup de traitements."),
    "factorial" = list(nom = "Factoriel complet",
      modele = "y ~ A * B * ... (effets principaux + interactions)",
      analyse = "ANOVA factorielle ; analyser les interactions avant les effets principaux ; effets simples si interaction significative."),
    "split" = list(nom = "Split-plot",
      modele = "y ~ A * B + Error(block/A)",
      analyse = "Modèle mixte : facteur principal sur l'erreur parcelle principale ; facteur secondaire et interaction sur l'erreur sous-parcelle (lmer/afex)."),
    "splitsplit" = list(nom = "Split-split-plot (3 facteurs)",
      modele = "y ~ A * B * C + Error(block/A/B)",
      analyse = "Modèle mixte à 3 niveaux d'erreur : facteur A (parcelle principale), facteur B (sous-parcelle), facteur C (sous-sous-parcelle). Trois termes d'erreur emboîtés (block/A, block/A/B, résiduelle). Analyser via lmer/afex avec la structure hiérarchique."),
    "strip" = list(nom = "Criss-Cross / Strip-plot",
      modele = "y ~ A * B + Error(block/A) + Error(block/B)",
      analyse = "Bandes croisées : trois termes d'erreur (bandes A, bandes B, interaction)."),
    list(nom = type, modele = "y ~ .", analyse = "ANOVA."))
}

# Construit un vecteur de noms de blocs personnalises (prefixe + numéro, ou noms
# fournis par l'utilisateur).
.block_labeller <- function(block_levels, prefix = "Bloc", custom = "", start = 1) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  bl <- as.character(block_levels)
  ubl <- unique(bl)
  custom <- trimws(custom %||% "")
  start <- suppressWarnings(as.integer(start)); if (is.na(start)) start <- 1
  names_vec <- if (nzchar(custom)) {
    parts <- trimws(strsplit(custom, ",")[[1]])
    if (length(parts) >= length(ubl)) parts[seq_along(ubl)]
    else c(parts, paste(prefix, seq(start + length(parts), length.out = length(ubl) - length(parts))))
  } else {
    paste(prefix, seq(start, length.out = length(ubl)))
  }
  stats::setNames(names_vec, ubl)
}

# Rendu FACETTE des dispositifs en blocs simples (fisher, factorial) : un panneau
# par bloc, le nom du bloc ecrit sur la bande de facette.
.build_block_faceted <- function(b, fill_scale, ttl, show_text = TRUE, label_mode = "both", cell_sep = 1,
                                  nplot = 1, font_label = 2.8, font_axis = 12, theme_gg = NULL,
                                  legend_order = "alpha", legend_pos = "right",
                                  legend_title = "", grad = "vertical_down",
                                  grad_label = "Gradient", grad_size = 3.5,
                                  xlab = "Parcelle", bold = FALSE,
                                  block_stack = "vertical", treat_vertical = FALSE) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (!".blocklab" %in% names(b)) return(NULL)
  lab_of <- function(v) switch(label_mode,
    "both" = if (nplot > 1) sprintf("%s\n(n=%d)", v, nplot) else as.character(v),
    "treat" = as.character(v), "n" = sprintf("n=%d", nplot), "none" = "")
  lv <- unique(as.character(b$.fill))
  lv <- switch(legend_order, "alpha" = .natural_sort(lv), "rev" = rev(.natural_sort(lv)), "appear" = lv, .natural_sort(lv))
  b$.fill <- factor(b$.fill, levels = lv)
  b$.lab <- vapply(as.character(b$.fill), lab_of, character(1))
  lt <- if (nzchar(legend_title)) legend_title else "Traitement"
  b$.blocklab <- factor(b$.blocklab, levels = unique(b$.blocklab[order(b$.blockord)]))

  # Deux axes INDEPENDANTS :
  #  - block_stack : disposition des blocs (pilotee par le gradient)
  #       "vertical"   -> blocs empiles de haut en bas : facet_grid(.blocklab ~ .)
  #       "horizontal" -> blocs cote a cote            : facet_grid(. ~ .blocklab)
  #  - treat_vertical : orientation des parcelles DANS chaque bloc
  #       FALSE -> parcelles en rangee horizontale (aes(factor(.x), 1))
  #       TRUE  -> parcelles empilees en colonne     (aes(1, factor(.x)))
  blocks_side_by_side <- (block_stack == "horizontal")
  # cols/rows = vars(.blocklab) : equivaut a ". ~ .blocklab" / ".blocklab ~ ."
  # sans utiliser la formule a point, qui provoque "objet '.' introuvable" selon
  # la version de ggplot2 quand elle est evaluee hors environnement global.
  facet_layer <- if (blocks_side_by_side)
                   ggplot2::facet_grid(cols = ggplot2::vars(.blocklab))
                 else
                   ggplot2::facet_grid(rows = ggplot2::vars(.blocklab), switch = "y")

  if (treat_vertical) {
    # parcelles empilees verticalement dans le bloc.
    # Largeur de case = pleine largeur du panneau (expand x = 0) pour que la
    # parcelle ait exactement la meme longueur que le bandeau du bloc.
    g <- ggplot2::ggplot(b, ggplot2::aes(1, factor(.x), fill = .fill)) +
      ggplot2::geom_tile(color = "white", linewidth = cell_sep, width = 1) +
      { if (show_text) ggplot2::geom_text(ggplot2::aes(label = .lab), size = font_label, color = "black", fontface = if (bold) "bold" else "plain") } +
      facet_layer + fill_scale +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_discrete(limits = rev, expand = ggplot2::expansion(mult = 0.01)) +
      ggplot2::labs(title = ttl, x = NULL, y = xlab, fill = lt) +
      (theme_gg %||% ggplot2::theme_minimal(base_size = font_axis)) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
  } else {
    # parcelles en rangee horizontale dans le bloc.
    # Hauteur de case = pleine hauteur du panneau (expand y = 0) pour que la
    # parcelle ait exactement la meme hauteur que le bandeau du bloc.
    g <- ggplot2::ggplot(b, ggplot2::aes(factor(.x), 1, fill = .fill)) +
      ggplot2::geom_tile(color = "white", linewidth = cell_sep, height = 1) +
      { if (show_text) ggplot2::geom_text(ggplot2::aes(label = .lab), size = font_label, color = "black", fontface = if (bold) "bold" else "plain") } +
      facet_layer + fill_scale +
      ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = 0.01)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::labs(title = ttl, x = xlab, y = NULL, fill = lt) +
      (theme_gg %||% ggplot2::theme_minimal(base_size = font_axis)) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }
  # Habillage commun des facettes (bandeaux de blocs)
  g <- g + ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold", size = font_axis),
    strip.background = ggplot2::element_rect(fill = "#ecf0f1", color = NA),
    panel.spacing = ggplot2::unit(0.5, "lines"),
    legend.position = legend_pos)

  # Gradient bien visible via un panneau-fleche juxtapose (patchwork)
  has_pw <- requireNamespace("patchwork", quietly = TRUE)
  if (grad %in% c("vertical_down", "vertical_up") && has_pw) {
    ap <- .gradient_arrow_panel(grad, grad_label, grad_size, "vertical")
    g <- patchwork::wrap_plots(ap, g, widths = c(0.06, 1))
  } else if (grad %in% c("horizontal_right", "horizontal_left") && has_pw) {
    ap <- .gradient_arrow_panel(grad, grad_label, grad_size, "horizontal")
    g <- patchwork::wrap_plots(g, ap, heights = c(1, 0.08), ncol = 1)
  } else if (grad %in% c("vertical_down", "vertical_up", "horizontal_right", "horizontal_left")) {
    # Repli sans patchwork : on indique le sens du gradient via un sous-titre
    # flheche, pour que l'information reste visible meme sans le package.
    arrow_txt <- switch(grad,
      "vertical_down"   = paste0("\u2193 ", grad_label, " (haut \u2192 bas)"),
      "vertical_up"     = paste0("\u2191 ", grad_label, " (bas \u2192 haut)"),
      "horizontal_right"= paste0(grad_label, " \u2192 (gauche \u2192 droite)"),
      "horizontal_left" = paste0("\u2190 ", grad_label, " (droite \u2192 gauche)"))
    g <- g + ggplot2::labs(caption = arrow_txt) +
         ggplot2::theme(plot.caption = ggplot2::element_text(
           hjust = 0.5, size = grad_size * 3, face = "bold", color = "#2980b9"))
  }
  g
}

# Construit les representations FACETTEES (un panneau par bloc/replique) pour les
# dispositifs split-plot, strip-plot et alpha-lattice, conformes aux conventions
# agronomiques de reference.
.build_faceted_design <- function(b, type, fill_col, fill_scale, ttl, cell_sep = 1,
                                  blk_line = 2, blk_color = "black",
                                  show_text = TRUE, label_mode = "both", nplot = 1,
                                  font_label = 2.8, font_axis = 12, theme_gg = NULL,
                                  grad = "vertical_down", grad_label = "Gradient",
                                  grad_size = 3.5, legend_order = "alpha",
                                  legend_pos = "right", legend_title = "",
                                  blk_prefix = "Bloc", blk_custom = "",
                                  grad2 = "auto", show_f2_legend = TRUE, bold = FALSE,
                                  treat_orient = "auto", blocks_axis = "vertical",
                                  grad_sens = "direct",
                                  sub_line = 0.8, sub_color = NULL,
                                  band_fill = "#d6eaf8",
                                  leg_txt_size = 11, leg_ttl_size = 12,
                                  leg_txt_bold = FALSE, leg_ttl_bold = FALSE) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  # Ordre de la legende
  reorder_fill <- function(vec) {
    lv <- unique(as.character(vec))
    lv <- switch(legend_order, "alpha" = .natural_sort(lv), "rev" = rev(.natural_sort(lv)),
                 "appear" = lv, .natural_sort(lv))
    factor(as.character(vec), levels = lv)
  }
  # Étiquettes de blocs / repliques personnalisees
  blk_facet_labeller <- function(values) {
    m <- .block_labeller(values, blk_prefix, blk_custom)
    m[as.character(values)]
  }
  lt <- if (nzchar(legend_title)) legend_title else NULL
  lab_of <- function(v) switch(label_mode,
    "both" = if (nplot > 1) sprintf("%s\n(n=%d)", v, nplot) else as.character(v),
    "treat" = as.character(v), "n" = sprintf("n=%d", nplot), "none" = "")

  if (type == "split") {
    # Image 3 : un panneau par bloc (rangees). Dans chaque bloc, le facteur
    # PRINCIPAL (Facteur1) occupe de grandes parcelles cote a cote ; le facteur
    # SECONDAIRE (Facteur2) subdivise chaque grande parcelle en colonnes.
    f1 <- "Facteur1"; f2 <- "Facteur2"
    if (!all(c(f1, f2, "block") %in% names(b))) return(NULL)
    nsub <- length(unique(b[[f2]]))
    main_levels <- unique(b[[f1]])
    # Reordonner : pour chaque bloc, regrouper les sous-parcelles SOUS leur grande
    # parcelle (meme niveau du Facteur1 contigu) ; l'ordre des grandes parcelles et
    # des sous-parcelles est randomise mais reste GROUPE.
    ord <- integer(0)
    ub <- unique(b$block)
    for (bi in seq_along(ub)) {
      bl <- ub[bi]
      rows_bl <- which(b$block == bl)
      set.seed(123 + bi * 17)
      main_order <- sample(unique(as.character(b[[f1]][rows_bl])))
      for (ml in main_order) {
        rr <- rows_bl[as.character(b[[f1]][rows_bl]) == ml]
        ord <- c(ord, rr[order(as.character(b[[f2]][rr]))])
      }
    }
    b <- b[ord, , drop = FALSE]
    b$.x <- stats::ave(seq_len(nrow(b)), b$block, FUN = function(ix) seq_along(ix))
    b$.fill <- reorder_fill(b[[f1]])    # couleur = facteur principal (meme couleur par grande parcelle)
    b$.lab <- vapply(as.character(b[[f2]]), lab_of, character(1))
    main_breaks <- seq(nsub, by = nsub, length.out = length(main_levels) - 1) + 0.5
    g <- ggplot2::ggplot(b, ggplot2::aes(factor(.x), 1, fill = .fill)) +
      ggplot2::geom_tile(color = "white", linewidth = cell_sep, height = 1) +
      { if (show_text) ggplot2::geom_text(ggplot2::aes(label = .lab), size = font_label, color = "black", fontface = if (bold) "bold" else "plain") } +
      ggplot2::geom_vline(xintercept = main_breaks, color = blk_color, linewidth = blk_line) +
      { if (blocks_axis == "horizontal")
          # cols = vars(block) equivaut a ". ~ block" mais evite l'erreur
          # "objet '.' introuvable" qui survient selon la version de ggplot2 quand
          # la formule est construite/evaluee hors de l'environnement global.
          ggplot2::facet_grid(cols = ggplot2::vars(block), labeller = ggplot2::labeller(block = blk_facet_labeller))
        else
          ggplot2::facet_grid(rows = ggplot2::vars(block), switch = "y", labeller = ggplot2::labeller(block = blk_facet_labeller)) } +
      fill_scale +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::labs(title = ttl,
                    subtitle = "Grandes parcelles (Facteur 1, couleur) divisées en sous-parcelles (Facteur 2, étiquettes)",
                    x = "Grandes parcelles (Facteur 1) → sous-parcelles (Facteur 2)",
                    y = "Bloc", fill = lt %||% "Facteur principal (F1)",
                    caption = paste0("Sous-parcelles (Facteur 2) : ",
                                     paste(sort(unique(as.character(b[[f2]]))), collapse = ", "),
                                     "  |  les lignes verticales épaisses séparent les grandes parcelles")) +
      (theme_gg %||% ggplot2::theme_minimal(base_size = font_axis)) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = font_axis - 2, color = "#5d6d7e"),
                     plot.caption = ggplot2::element_text(size = font_axis - 3, color = "#7f8c8d", hjust = 0),
                     axis.title = ggplot2::element_text(face = "bold", color = "#34495e"),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(face = "bold", color = "#2c3e50"),
                     strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold"),
                     strip.background = ggplot2::element_rect(fill = "#eaf2f8", color = "#d4e6f1"),
                     panel.spacing = ggplot2::unit(0.5, "lines"),
                     legend.position = legend_pos)
  } else if (type == "splitsplit") {
    # Split-split-plot : un panneau par bloc. Dans chaque bloc, le Facteur 1
    # (grandes parcelles) occupe de larges zones cote a cote ; a l'intérieur, le
    # Facteur 2 (sous-parcelles) subdivise en colonnes ; le Facteur 3
    # (sous-sous-parcelles) subdivise encore, montre en etiquette de cellule.
    # S'appuie sur les colonnes mainplot/subplot/subsubplot du generateur, donc
    # independant des noms de facteurs choisis par l'utilisateur.
    need_cols <- c("block", "mainplot", "subplot", "subsubplot")
    if (!all(need_cols %in% names(b))) return(NULL)
    # Noms reels des 3 facteurs (colonnes ajoutees par le generateur, hors colonnes techniques)
    tech <- c("plots", "block", "mainplot", "subplot", "subsubplot", "Traitement",
              ".x", ".y", ".fill", ".lab", ".blocklab", ".blockord")
    fnames3 <- setdiff(names(b), tech)
    f1n <- fnames3[1]; f2n <- fnames3[2]; f3n <- fnames3[3]
    # Position en x = ordre (mainplot, subplot, subsubplot) DANS le bloc.
    ord <- with(b, order(block, mainplot, subplot, subsubplot))
    b <- b[ord, , drop = FALSE]
    b$.x <- stats::ave(seq_len(nrow(b)), b$block, FUN = function(ix) seq_along(ix))
    # Couleur = Facteur 1 (grande parcelle). Etiquette de cellule = Facteur 3
    # UNIQUEMENT (le Facteur 2 est identifie par son bandeau au-dessus des
    # sous-parcelles et par les traits pointilles).
    b$.fill <- reorder_fill(b[[f1n]])
    f2_txt <- as.character(b[[f2n]]); f3_txt <- as.character(b[[f3n]])
    b$.lab <- switch(label_mode,
      "none" = "",
      "n"    = vapply(f3_txt, function(v) sprintf("n=%d", nplot), character(1)),
      # both / treat -> modalite du Facteur 3 seule
      f3_txt)
    # Traits de separation : epais entre grandes parcelles (F1), fins entre sous-parcelles (F2).
    nssp <- length(unique(b$subsubplot))                 # cellules par sous-parcelle
    nsub <- length(unique(b$subplot))                    # sous-parcelles par grande parcelle
    cells_per_main <- nsub * nssp
    n_main <- length(unique(b$mainplot[b$block == b$block[1]]))
    main_breaks <- seq(cells_per_main, by = cells_per_main, length.out = max(0, n_main - 1)) + 0.5
    sub_breaks  <- setdiff(seq(nssp, by = nssp, length.out = max(0, n_main * nsub - 1)) + 0.5, main_breaks)
    # Bandeau du Facteur 2 : fine bande bleue ACCOLEE au sommet des cellules de
    # chaque sous-parcelle, portant le niveau du F2. Hauteur volontairement
    # reduite (band_h) pour une presentation compacte, sans espace mort.
    band_df <- do.call(rbind, lapply(split(b, list(b$block, b$mainplot, b$subplot), drop = TRUE),
      function(d) data.frame(block = d$block[1],
                             .xc = mean(d$.x),
                             .x0 = min(d$.x) - 0.5,
                             .x1 = max(d$.x) + 0.5,
                             lab2 = as.character(d[[f2n]][1]),
                             stringsAsFactors = FALSE)))
    band_h <- 0.18   # hauteur du bandeau F2 (en unites de cellule, cellule = 1)
    # -- Personnalisation utilisateur : traits pointilles + couleur du bandeau --
    # Validation defensive : toute valeur invalide retombe sur un defaut sur.
    sub_lw <- suppressWarnings(as.numeric(sub_line %||% 0.8))
    if (!is.finite(sub_lw) || sub_lw < 0) sub_lw <- 0.8
    .valid_col <- function(x, fallback) {
      if (is.null(x) || !nzchar(as.character(x)[1])) return(fallback)
      ok <- !inherits(tryCatch(grDevices::col2rgb(x), error = function(e) e), "error")
      if (ok) as.character(x)[1] else fallback
    }
    sub_col <- .valid_col(sub_color, blk_color)
    bnd_fill <- .valid_col(band_fill, "#d6eaf8")
    # Couleur du texte du bandeau : contraste automatique (blanc sur fond sombre).
    brgb <- grDevices::col2rgb(bnd_fill)
    bnd_txt <- if ((0.299 * brgb[1] + 0.587 * brgb[2] + 0.114 * brgb[3]) / 255 < 0.55)
      "white" else "#1a5276"
    n_per_block <- max(b$.x)
    band_breaks <- sort(unique(c(sub_breaks, main_breaks)))
    # ---- Separations GEOMETRIQUES proportionnelles (correctif parcelles invisibles) --
    # Les anciens traits blancs avaient une epaisseur ABSOLUE (mm) : avec beaucoup
    # de parcelles, chaque trait devenait plus large que les cellules et les
    # recouvrait entierement. Les separations sont desormais des retraits
    # geometriques exprimes en FRACTION de la largeur d'une cellule : le rendu
    # est identique quel que soit le nombre de parcelles par bloc.
    gap  <- max(0, min(0.35, (suppressWarnings(as.numeric(cell_sep)) %||% 1) * 0.05))
    if (!is.finite(gap)) gap <- 0.05
    vgap <- min(0.05, gap)                     # jeu vertical bandeau / cellules
    # Cellules : rects retrecis aux frontieres INTERNES uniquement, bords du bloc flush.
    b$.xmin <- b$.x - 0.5 + ifelse(b$.x == 1, 0, gap / 2)
    b$.xmax <- b$.x + 0.5 - ifelse(b$.x == n_per_block, 0, gap / 2)
    # Bandeau : segments retrecis aux frontieres internes, bords du bloc flush.
    band_df$.x0g <- band_df$.x0 + ifelse(band_df$.x0 <= 0.5, 0, gap / 2)
    band_df$.x1g <- band_df$.x1 - ifelse(band_df$.x1 >= n_per_block + 0.5, 0, gap / 2)
    band_y0 <- 1.5 + vgap
    # Etiquettes d'axe : eclaircies quand les parcelles sont nombreuses.
    ax_breaks <- if (n_per_block > 24) unique(c(1, seq(5, n_per_block, by = 5)))
                 else seq_len(n_per_block)
    # Filets blancs FINS a epaisseur fixe (independante du curseur de separation) :
    # quand les parcelles sont nombreuses, les retraits geometriques deviennent
    # inferieurs au pixel ; ces filets garantissent une separation nette et
    # reguliere sans jamais recouvrir les cellules (contrairement a l'ancien
    # linewidth = cell_sep qui rendait les parcelles invisibles).
    hair <- 0.3
    cell_breaks <- if (n_per_block > 1) seq(1.5, n_per_block - 0.5, by = 1) else numeric(0)
    # Etiquettes de cellules : rotation verticale automatique au-dela de 24
    # parcelles par bloc (cellules etroites -> le texte horizontal se superpose).
    lab_angle <- if (n_per_block > 24) 90 else 0
    lab_size  <- if (n_per_block > 24) font_label * 0.85 else font_label
    g <- ggplot2::ggplot(b) +
      ggplot2::geom_rect(ggplot2::aes(xmin = .xmin, xmax = .xmax,
                                      ymin = 0.5, ymax = 1.5, fill = .fill),
                         color = NA) +
      { if (length(cell_breaks))
          ggplot2::geom_segment(data = data.frame(.xb = cell_breaks),
            ggplot2::aes(x = .xb, xend = .xb, y = 0.5, yend = 1.5),
            inherit.aes = FALSE, color = "white", linewidth = hair) } +
      { if (show_text) ggplot2::geom_text(ggplot2::aes(x = .x, y = 1, label = .lab), size = lab_size, angle = lab_angle, color = "black", fontface = if (bold) "bold" else "plain", lineheight = 0.85) } +
      # Bandeau du Facteur 2 : bande fine au-dessus des cellules
      # (couleur personnalisable, texte a contraste automatique).
      ggplot2::geom_rect(data = band_df,
                         ggplot2::aes(xmin = .x0g, xmax = .x1g,
                                      ymin = band_y0, ymax = band_y0 + band_h),
                         inherit.aes = FALSE, fill = bnd_fill, color = NA) +
      { if (length(band_breaks))
          ggplot2::geom_segment(data = data.frame(.xb = band_breaks),
            ggplot2::aes(x = .xb, xend = .xb, y = band_y0, yend = band_y0 + band_h),
            inherit.aes = FALSE, color = "white", linewidth = hair) } +
      ggplot2::geom_text(data = band_df,
                         ggplot2::aes(x = .xc, y = band_y0 + band_h / 2, label = lab2),
                         inherit.aes = FALSE, size = font_label * 1.0,
                         fontface = "bold", color = bnd_txt) +
      { if (length(sub_breaks) && sub_lw > 0)
          ggplot2::geom_segment(data = data.frame(.xb = sub_breaks),
            ggplot2::aes(x = .xb, xend = .xb, y = 0.5, yend = 1.5),
            inherit.aes = FALSE, color = sub_col, linewidth = sub_lw,
            linetype = "22", lineend = "butt") } +
      { if (length(main_breaks)) ggplot2::geom_vline(xintercept = main_breaks, color = blk_color, linewidth = blk_line) } +
      { if (blocks_axis == "horizontal")
          ggplot2::facet_grid(cols = ggplot2::vars(block), labeller = ggplot2::labeller(block = blk_facet_labeller))
        else
          ggplot2::facet_grid(rows = ggplot2::vars(block), switch = "y", labeller = ggplot2::labeller(block = blk_facet_labeller)) } +
      fill_scale +
      ggplot2::scale_x_continuous(breaks = ax_breaks, labels = ax_breaks,
                                  expand = c(0, 0),
                                  limits = c(0.5, n_per_block + 0.5)) +
      ggplot2::scale_y_continuous(expand = c(0, 0),
                                  limits = c(0.5, band_y0 + band_h + 0.01)) +
      ggplot2::labs(title = ttl,
                    subtitle = trf("Grandes parcelles = %s (couleur) ; sous-parcelles = %s (bandeau) ; sous-sous-parcelles = %s (étiquette de cellule)", f1n, f2n, f3n),
                    x = NULL,
                    y = "Bloc", fill = lt %||% f1n) +
      (theme_gg %||% ggplot2::theme_minimal(base_size = font_axis)) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = font_axis - 2, color = "#5d6d7e"),
                     plot.caption = ggplot2::element_text(size = font_axis - 3, color = "#7f8c8d", hjust = 0),
                     axis.title = ggplot2::element_text(face = "bold", color = "#34495e"),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(face = "bold", color = "#2c3e50",
                                                        margin = ggplot2::margin(2.5, 2.5, 2.5, 2.5)),
                     strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold",
                                                               margin = ggplot2::margin(2.5, 4, 2.5, 4)),
                     strip.background = ggplot2::element_rect(fill = "#eaf2f8", color = "#d4e6f1"),
                     panel.spacing = ggplot2::unit(0.5, "lines"),
                     legend.position = legend_pos)
  } else if (type == "strip") {
    # Image 4 : un panneau par bloc. Facteur1 en BANDES verticales (colonnes),
    # Facteur2 en BANDES horizontales (rangees). Les combinaisons sont aux
    # intersections. Fill = Facteur1 (ou traitement).
    f1 <- "Facteur1"; f2 <- "Facteur2"
    if (!all(c(f1, f2, "block") %in% names(b))) return(NULL)
    b$.col <- as.integer(as.factor(b[[f1]]))
    b$.row <- as.integer(as.factor(b[[f2]]))
    b$.fill <- reorder_fill(b[[fill_col]])
    b$.lab <- vapply(as.character(b[[fill_col]]), lab_of, character(1))
    g <- ggplot2::ggplot(b, ggplot2::aes(factor(.col), factor(.row), fill = .fill)) +
      ggplot2::geom_tile(color = "white", linewidth = cell_sep) +
      { if (show_text) ggplot2::geom_text(ggplot2::aes(label = .lab), size = font_label, color = "black", fontface = if (bold) "bold" else "plain") } +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      { if (blocks_axis == "horizontal")
          ggplot2::facet_wrap(~ block, ncol = 1, labeller = ggplot2::labeller(block = blk_facet_labeller))
        else
          ggplot2::facet_wrap(~ block, nrow = 1, labeller = ggplot2::labeller(block = blk_facet_labeller)) } +
      fill_scale +
      ggplot2::labs(title = ttl,
                    subtitle = "Bandes du Facteur 1 (colonnes) croisées avec les bandes du Facteur 2 (rangées)",
                    x = "Bandes du Facteur 1", y = "Bandes du Facteur 2",
                    fill = lt %||% "Traitement") +
      (theme_gg %||% ggplot2::theme_minimal(base_size = font_axis)) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = font_axis - 2, color = "#5d6d7e"),
                     axis.title = ggplot2::element_text(face = "bold", color = "#34495e"),
                     strip.text = ggplot2::element_text(face = "bold", color = "#2c3e50"),
                     strip.background = ggplot2::element_rect(fill = "#eaf2f8", color = "#d4e6f1"),
                     panel.spacing = ggplot2::unit(0.9, "lines"),
                     legend.position = legend_pos)
  } else if (type == "alpha") {
    # Image 5 cible : repliques ALIGNEES sur la meme ligne (cote a cote), separees
    # par un espace blanc epais. Chaque replique = panneau ; blocs incomplets en
    # rangees a l'intérieur.
    if (!all(c("replication", "block") %in% names(b))) return(NULL)
    b$.row <- stats::ave(seq_len(nrow(b)), interaction(b$replication, b$block, drop = TRUE),
                  FUN = function(ix) 1L) # placeholder
    # position : a l'intérieur de chaque (replique, bloc), les unités en colonnes
    b <- b[order(b$replication, b$block), , drop = FALSE]
    b$.x <- stats::ave(seq_len(nrow(b)), interaction(b$replication, b$block, drop = TRUE),
                FUN = function(ix) seq_along(ix))
    # .y = rang du bloc DANS sa replique (remis a 1 pour chaque replique -> alignement)
    b$.y <- as.integer(stats::ave(as.integer(as.factor(b$block)), b$replication,
                FUN = function(z) as.integer(as.factor(z))))
    b$.fill <- reorder_fill(b[[fill_col]])
    b$.lab <- vapply(as.character(b[[fill_col]]), lab_of, character(1))
    g <- ggplot2::ggplot(b, ggplot2::aes(factor(.x), factor(.y), fill = .fill)) +
      ggplot2::geom_tile(color = "white", linewidth = cell_sep) +
      { if (show_text) ggplot2::geom_text(ggplot2::aes(label = .lab), size = font_label, color = "black", fontface = if (bold) "bold" else "plain") } +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_discrete(limits = rev, expand = c(0, 0)) +
      { if (blocks_axis == "horizontal")
          ggplot2::facet_wrap(~ replication, ncol = 1,
                          labeller = ggplot2::labeller(replication = function(x) { m <- .block_labeller(x, if (nzchar(blk_custom)) blk_prefix else "Replique", blk_custom); m[as.character(x)] }))
        else
          ggplot2::facet_wrap(~ replication, nrow = 1,
                          labeller = ggplot2::labeller(replication = function(x) { m <- .block_labeller(x, if (nzchar(blk_custom)) blk_prefix else "Replique", blk_custom); m[as.character(x)] })) } +
      fill_scale +
      ggplot2::labs(title = ttl, x = "Blocs incomplets (colonnes)", y = "Bloc incomplet", fill = lt %||% "Traitement") +
      (theme_gg %||% ggplot2::theme_minimal(base_size = font_axis)) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(face = "bold", size = font_axis),
                     strip.background = ggplot2::element_rect(fill = "#ecf0f1", color = NA),
                     panel.spacing = ggplot2::unit(2.2, "lines"),  # espace blanc epais entre repliques
                     legend.position = legend_pos)
  } else return(NULL)

  # Orientation des traitements dans le bloc : si "vertical" demande pour le strip,
  # on inverse les axes (bandes).
  if (treat_orient == "vertical" && type == "strip") {
    g <- g + ggplot2::coord_flip()
  }
  # Note : la disposition des panneaux (repliques/blocs empiles vs cote a cote)
  # est desormais geree directement par l'orientation des facettes ci-dessus
  # (ncol = 1 vs nrow = 1, ou block ~ . vs . ~ block), de sorte que le bandeau
  # gris suit l'orientation des parcelles sans recourir a coord_flip.

  # ---- Gradient(s) d'heterogeneite ----
  # REGLE : le gradient PRINCIPAL (1) est TOUJOURS perpendiculaire a l'empilement
  # des blocs incomplets ; le 2e gradient reste ORTHOGONAL au 1er.
  #  - blocs empiles verticalement -> G1 vertical, G2 horizontal ;
  #  - blocs cote a cote (horizontal) -> G1 horizontal, G2 vertical.
  sens_inverse <- identical(grad_sens, "inverse")
  if (blocks_axis == "vertical") {
    main_dir <- if (sens_inverse) "vertical_up" else "vertical_down"
    main_orient <- "vertical"
    second_dir <- "horizontal_right"; second_orient <- "horizontal"
  } else {
    main_dir <- if (sens_inverse) "horizontal_left" else "horizontal_right"
    main_orient <- "horizontal"
    second_dir <- "vertical_down"; second_orient <- "vertical"
  }
  # 2e gradient : respecte un choix explicite de l'utilisateur s'il est orthogonal,
  # sinon prend la direction orthogonale par defaut.
  if (!identical(grad2, "auto") && !identical(grad2, "none")) {
    g2_is_vertical <- grad2 %in% c("vertical_down", "vertical_up")
    if ((second_orient == "vertical") == g2_is_vertical) second_dir <- grad2
  }
  if (identical(grad2, "none")) second_orient <- "none"

  # NOMBRE DE GRADIENTS selon le dispositif :
  #  - alpha-lattice : UN SEUL gradient (perpendiculaire aux parcelles).
  #  - strip-plot    : UN SEUL gradient (perpendiculaire aux parcelles) -- les deux
  #    facteurs sont en bandes croisees, mais l'heterogeneite du terrain suit une
  #    seule direction, perpendiculaire aux parcelles.
  #  - split-plot    : UN SEUL gradient (entre blocs).
  # => Aucun de ces plans n'affiche de 2e gradient.
  has_two_grad <- FALSE

  use_pw <- requireNamespace("patchwork", quietly = TRUE)
  if (use_pw && grad != "none") {
    # Gradient unique, perpendiculaire aux blocs / a l'orientation des parcelles
    glab1 <- grad_label
    if (main_orient == "vertical") {
      ap <- .gradient_arrow_panel(main_dir, glab1, grad_size, "vertical")
      g <- patchwork::wrap_plots(ap, g, widths = c(0.06, 1))
    } else {
      ap <- .gradient_arrow_panel(main_dir, glab1, grad_size, "horizontal")
      g <- patchwork::wrap_plots(g, ap, heights = c(1, 0.08), ncol = 1)
    }
  } else if (!use_pw && grad != "none") {
    # Repli sans patchwork : sens du gradient indique en sous-titre flheche.
    arrow_txt <- switch(main_dir,
      "vertical_down"    = paste0("\u2193 ", grad_label, " (haut \u2192 bas)"),
      "vertical_up"      = paste0("\u2191 ", grad_label, " (bas \u2192 haut)"),
      "horizontal_right" = paste0(grad_label, " \u2192 (gauche \u2192 droite)"),
      "horizontal_left"  = paste0("\u2190 ", grad_label, " (droite \u2192 gauche)"),
      paste0(grad_label))
    g <- g + ggplot2::labs(caption = arrow_txt) +
         ggplot2::theme(plot.caption = ggplot2::element_text(
           hjust = 0.5, size = grad_size * 3, face = "bold", color = "#2980b9"))
  }

  # Legende SEPAREE pour le Facteur 2 (split, strip) via sous-graphique combine.
  # Legendes des facteurs SECONDAIRES (F2, F3, ...) empilees VERTICALEMENT sous la
  # legende principale (F1 = couleur). Chaque facteur a son propre bloc titre + niveaux.
  # Concerne les dispositifs a plusieurs facteurs : split (F2), strip (F2),
  # split-split (F2 et F3).
  sec_factors <- character(0)
  if (type %in% c("split", "strip") && "Facteur2" %in% names(b)) {
    sec_factors <- "Facteur2"
  } else if (type == "splitsplit") {
    tech <- c("plots", "block", "mainplot", "subplot", "subsubplot", "Traitement",
              ".x", ".y", ".fill", ".lab", ".blocklab", ".blockord")
    ff <- setdiff(names(b), tech)
    if (length(ff) >= 3) sec_factors <- ff[2:3]   # F2 et F3 (F1 est deja la couleur)
  }
  if (show_f2_legend && length(sec_factors) > 0 && use_pw &&
      !identical(legend_pos, "none")) {
    # Construire une PILE de legendes : F1 (couleur), puis F2, puis F3...
    # TOUS les reglages de la section "Legende" (ordre, tailles, gras, position)
    # s'appliquent a TOUS les facteurs de cette pile.
    leg_txt_mm <- max(1, suppressWarnings(as.numeric(leg_txt_size)) %||% 11) / 2.845276
    if (!is.finite(leg_txt_mm)) leg_txt_mm <- 11 / 2.845276
    leg_ttl_pt <- suppressWarnings(as.numeric(leg_ttl_size)); if (!is.finite(leg_ttl_pt)) leg_ttl_pt <- 12
    txt_face <- if (isTRUE(leg_txt_bold)) "bold" else "plain"
    ttl_face <- if (isTRUE(leg_ttl_bold)) "bold" else "plain"
    # Ordre des modalites : la regle choisie (alphabetique / inverse / apparition)
    # est la meme pour le F1 et pour chaque facteur secondaire.
    .order_levels <- function(vals) {
      lv <- unique(as.character(vals))
      switch(legend_order,
             "alpha" = .natural_sort(lv), "rev" = rev(.natural_sort(lv)),
             "appear" = lv, .natural_sort(lv))
    }
    fill_levels <- levels(b$.fill)
    if (is.null(fill_levels)) fill_levels <- .order_levels(b$.fill)
    # Recuperer les couleurs effectives du facteur principal depuis fill_scale.
    pal <- tryCatch({
      sc <- fill_scale
      if (!is.null(sc$palette)) sc$palette(length(fill_levels)) else NULL
    }, error = function(e) NULL)
    if (is.null(pal) || length(pal) < length(fill_levels))
      pal <- grDevices::hcl.colors(length(fill_levels), "Dynamic")
    # Attribution des couleurs PAR NOM de modalite (et non par position) : avec un
    # ordre de legende inverse ou d'apparition, chaque swatch garde la couleur de
    # sa modalite dans le graphe.
    col_map <- if (!is.null(names(pal)) && all(fill_levels %in% names(pal))) {
      pal[fill_levels]
    } else {
      base_levels <- levels(b$.fill)
      if (is.null(base_levels)) base_levels <- fill_levels
      stats::setNames(pal[seq_along(base_levels)], base_levels)[fill_levels]
    }
    f1_name <- if (nzchar(legend_title)) legend_title else {
      if (type == "splitsplit") setdiff(names(b), c("plots","block","mainplot","subplot","subsubplot","Traitement",".x",".y",".fill",".lab",".blocklab",".blockord"))[1]
      else "Facteur 1"
    }
    # Panneau F1 : cases colorees + niveaux.
    f1df <- data.frame(.y = factor(fill_levels, levels = rev(fill_levels)),
                       .x = 1, lab = fill_levels, stringsAsFactors = FALSE)
    f1_panel <- ggplot2::ggplot(f1df, ggplot2::aes(.x, .y, fill = lab)) +
      ggplot2::geom_tile(color = "white", width = 0.6) +
      ggplot2::geom_text(ggplot2::aes(label = lab), size = leg_txt_mm, fontface = txt_face) +
      ggplot2::scale_fill_manual(values = col_map, guide = "none") +
      ggplot2::labs(title = f1_name) +
      ggplot2::theme_void(base_size = font_axis) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = leg_ttl_pt, face = ttl_face, hjust = 0.5,
                                                        margin = ggplot2::margin(b = 3)),
                     plot.margin = ggplot2::margin(2, 4, 6, 4))
    # Panneaux des facteurs secondaires : modalites en TEXTE SEUL, avec les memes
    # reglages de legende que le F1. Couleur des modalites : le F2 reprend la
    # couleur du bandeau (assombrie si trop claire pour rester lisible sur fond
    # blanc), les facteurs suivants (F3...) sont en gris.
    .readable_col <- function(col) {
      rgb <- tryCatch(grDevices::col2rgb(col)[, 1] / 255,
                      error = function(e) c(0.2, 0.2, 0.2))
      lum <- 0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]
      if (lum > 0.72) grDevices::rgb(rgb[1] * 0.5, rgb[2] * 0.5, rgb[3] * 0.5) else col
    }
    f2_col <- if (exists("bnd_fill", inherits = FALSE)) .readable_col(bnd_fill)
              else "#1a5276"
    sec_cols <- c(f2_col, rep("#7f8c8d", max(0, length(sec_factors) - 1)))
    make_factor_panel <- function(fac_name, lev_col = "black") {
      levs <- .order_levels(b[[fac_name]])
      ldf <- data.frame(.y = factor(levs, levels = rev(levs)), .x = 1,
                        lab = levs, stringsAsFactors = FALSE)
      ggplot2::ggplot(ldf, ggplot2::aes(.x, .y)) +
        ggplot2::geom_text(ggplot2::aes(label = lab), size = leg_txt_mm,
                           fontface = txt_face, color = lev_col) +
        ggplot2::labs(title = fac_name) +
        ggplot2::theme_void(base_size = font_axis) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = leg_ttl_pt, face = ttl_face, hjust = 0.5,
                                                          margin = ggplot2::margin(b = 3)),
                       plot.margin = ggplot2::margin(2, 4, 6, 4))
    }
    sec_panels <- Map(make_factor_panel, sec_factors, sec_cols[seq_along(sec_factors)])
    sec_panels <- unname(sec_panels)
    all_panels <- c(list(f1_panel), sec_panels)
    n_levs <- c(length(fill_levels),
                vapply(sec_factors,
                       function(f) length(unique(as.character(b[[f]]))), numeric(1)))
    # Retirer la legende native (couleur) de g puisqu'elle est recreee dans la pile.
    g <- g & ggplot2::theme(legend.position = "none")
    if (legend_pos %in% c("bottom", "top")) {
      # Position bas / haut : panneaux cote a cote sur une rangee.
      leg_row <- patchwork::wrap_plots(
        c(all_panels, list(patchwork::plot_spacer())), nrow = 1,
        widths = c(rep(1, length(all_panels)), 0.5))
      g <- if (identical(legend_pos, "top"))
        patchwork::wrap_plots(leg_row, g, ncol = 1, heights = c(0.22, 1))
      else
        patchwork::wrap_plots(g, leg_row, ncol = 1, heights = c(1, 0.22))
    } else {
      # Position droite / gauche : pile verticale COMPACTE et ALIGNEE EN HAUT
      # (hauteur de chaque panneau proportionnelle a son nombre de niveaux).
      hts <- n_levs + 1.2                      # + place du titre
      spacer_h <- max(1, sum(hts) * 0.6)       # pousse la pile vers le haut
      stacked <- patchwork::wrap_plots(
        c(all_panels, list(patchwork::plot_spacer())),
        ncol = 1, heights = c(hts, spacer_h))
      # Colonne de legende etroite : le graphique occupe toute la largeur disponible.
      g <- if (identical(legend_pos, "left"))
        patchwork::wrap_plots(stacked, g, widths = c(0.10, 1))
      else
        patchwork::wrap_plots(g, stacked, widths = c(1, 0.10))
    }
  }
  return(g)
}

mod_design_ui <- function(id) {
  ns <- shiny::NS(id)
  # Petit sous-titre de categorie pour structurer visuellement le panneau
  # "Graphique du dispositif" en sections thematiques.
  .sect <- function(icon_name, label) {
    shiny::tags$div(style = "margin:14px 0 6px;padding-bottom:3px;border-bottom:2px solid #d6dbdf;",
      shiny::tags$span(style = "font-weight:700;color:#2c3e50;font-size:14px;",
                shiny::icon(icon_name), " ", label))
  }
  banner <- if (exists(".hstat_scope_banner")) .hstat_scope_banner(exact = FALSE) else NULL
  shinydashboard::tabItem(tabName = "design",
    banner,
    shiny::fluidRow(
      shinydashboard::box(width = 12, status = "primary", solidHeader = FALSE, background = "navy",
          shiny::h3(shiny::icon("flask"), " Dispositifs experimentaux & Puissance statistique",
             style = "margin:0;color:white;"))
    ),
    shiny::tabsetPanel(id = ns("designTabs"),
      shiny::tabPanel(shiny::tagList(shiny::icon("bolt"), " Puissance statistique"), value = "power", shiny::br(),
        shiny::fluidRow(
          shinydashboard::box(title = shiny::tagList(shiny::icon("sliders"), " Paramètres d'entree"),
              status = "primary", width = 5, solidHeader = TRUE,
              shiny::selectInput(ns("powFamily"), "Famille de test",
                choices = c("Tests t" = "t", "Tests F" = "F",
                            "Tests chi-deux" = "chisq", "Tests z (proportions)" = "z")),
              shiny::uiOutput(ns("powTestSelect")),
              shiny::selectInput(ns("powAnalysis"), "Type d'analyse de puissance",
                choices = c("A priori : calculer la taille d'échantillon (n) requise" = "apriori",
                            "Post hoc : taille connue -> calculer la puissance (1-\u03b2)" = "posthoc",
                            "Sensibilite : taille connue -> calculer l'effet detectable" = "sensitivity")),
              shiny::div(style = "font-size:12px;color:#7f8c8d;margin-top:-8px;margin-bottom:8px;",
                  shiny::icon("info-circle"),
                  " Si vous connaissez déjà votre taille d'échantillon et vos modalités, ",
                  "choisissez Post hoc (pour la puissance) ou Sensibilite (pour l'effet detectable)."),
              shiny::hr(),
              shiny::uiOutput(ns("powTailUI")),
              shiny::numericInput(ns("powEffect"), "Taille d'effet", value = 0.25, min = 0.0001, step = 0.05),
              shiny::uiOutput(ns("powEffectHint")),
              shiny::conditionalPanel("input.powTest == 'cor_2indep' || input.powTest == 'cor_2dep'",
                ns = ns,
                shiny::numericInput(ns("powEffect2"), "Second coefficient r (r2)",
                             value = 0, min = -0.999, max = 0.999, step = 0.05)),
              shiny::conditionalPanel("input.powTest == 'logistic'", ns = ns,
                shiny::selectInput(ns("powPredictor"), "Type de prédicteur",
                            choices = c("Continu (OR par écart-type)" = "continuous",
                                        "Binaire (expose / non-expose)" = "binary")),
                shiny::numericInput(ns("powP0"), "Probabilité de l'evenement chez les non-exposes / a la moyenne (p0)",
                             value = 0.2, min = 0.01, max = 0.99, step = 0.05),
                shiny::conditionalPanel("input.powPredictor == 'binary'", ns = ns,
                  shiny::sliderInput(ns("powPx"), "Proportion d'exposes",
                              min = 0.05, max = 0.95, value = 0.5, step = 0.05)),
                shiny::numericInput(ns("powR2other"), "R2 du prédicteur explique par les autres covariables",
                             value = 0, min = 0, max = 0.95, step = 0.05)),
              shiny::conditionalPanel("input.powTest == 'poisson'", ns = ns,
                shiny::numericInput(ns("powBaseRate"), "Taux de base moyen (exp(beta0))",
                             value = 1, min = 0.01, step = 0.1),
                shiny::numericInput(ns("powExposure"), "Exposition / duree moyenne",
                             value = 1, min = 0.01, step = 0.5),
                shiny::numericInput(ns("powR2otherP"), "R2 explique par les autres covariables",
                             value = 0, min = 0, max = 0.95, step = 0.05)),
              shiny::numericInput(ns("powAlpha"), "\u03b1 (err prob)", value = 0.05, min = 0.0001, max = 0.5, step = 0.01),
              shiny::numericInput(ns("powPower"), "Puissance (1-\u03b2 err prob)", value = 0.95, min = 0.5, max = 0.999, step = 0.01),
              shiny::uiOutput(ns("powExtraUI")),
              shiny::actionButton(ns("powCalc"), "Calculer", class = "btn-success", icon = shiny::icon("calculator"))
          ),
          shinydashboard::box(title = shiny::tagList(shiny::icon("chart-area"), " Paramètres de sortie"),
              status = "success", width = 7, solidHeader = TRUE,
              DT::DTOutput(ns("powOutputTable")), shiny::br(),
              shiny::uiOutput(ns("powVerdict")), shiny::hr(),
              shiny::h4(shiny::icon("project-diagram"), " Répartition automatique de l'échantillon"),
              shiny::uiOutput(ns("powAllocNote")), DT::DTOutput(ns("powAllocTable")),
              shiny::br(),
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("powRepartMode"), "Mode de répartition vers le plan",
                  choices = c("Répétitions (1 échantillon/parcelle)" = "blocks",
                              "Sous-échantillonnage (equilibre R x m)" = "subsampling"))),
                shiny::column(6, shiny::div(style = "margin-top:25px;",
                  shiny::actionButton(ns("powToDesign"),
                    shiny::tagList(shiny::icon("arrow-right"), " Utiliser cette taille dans le plan"),
                    class = "btn-primary")))
              ),
              shiny::uiOutput(ns("powRepartPreview"))
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(title = shiny::tagList(shiny::icon("chart-line"), " Courbe de puissance"),
              status = "info", width = 8, solidHeader = TRUE,
              shiny::plotOutput(ns("powCurve"), height = "340px")),
          shinydashboard::box(title = shiny::tagList(shiny::icon("info-circle"), " Conventions (Cohen)"),
              status = "info", width = 4, solidHeader = TRUE,
              DT::DTOutput(ns("powConventions")))
        )
      ),
      shiny::tabPanel(shiny::tagList(shiny::icon("th"), " Plan expérimental"), value = "plan", shiny::br(),
        shiny::fluidRow(
          shinydashboard::box(title = shiny::tagList(shiny::icon("cog"), " Configuration"),
              status = "primary", width = 4, solidHeader = TRUE,
              shiny::selectInput(ns("dsgType"), "Type de plan", choices = hstat_design_catalog()),
              shiny::uiOutput(ns("dsgHint")),
              shiny::div(class = "callout callout-info", style = "padding:8px 10px;font-size:12px;",
                shiny::icon("link"),
                " Vous avez defini des facteurs dans l'onglet Puissance statistique ? ",
                shiny::actionLink(ns("dsgImportPower"),
                  shiny::tagList(shiny::icon("download"), " Recuperer le nombre de facteurs et de modalités")),
                "."),
              shiny::numericInput(ns("dsgNFactors"), "Nombre de facteurs (factoriel)", value = 2, min = 1, max = 5, step = 1),
              shiny::uiOutput(ns("dsgFactorInputs")),
              shiny::conditionalPanel("input.dsgType!='lsd'", ns = ns,
                shiny::numericInput(ns("dsgRep"), "Répétitions / blocs", value = 3, min = 1, step = 1)),
              shiny::conditionalPanel("input.dsgType=='alpha'", ns = ns,
                shiny::numericInput(ns("dsgK"), "Taille du bloc incomplet (k)", value = 3, min = 2, step = 1),
                shiny::radioButtons(ns("dsgKMode"),
                  shiny::tagList(shiny::icon("sliders-h"), " Gestion de la taille de bloc k"),
                  choices = c(
                    "Respecter exactement le k saisi (dernier bloc plus petit si besoin)" = "exact",
                    "Ajuster automatiquement vers un k parfait (plan équilibré)" = "auto"),
                  selected = "exact")),
              shiny::conditionalPanel("input.dsgType=='factorial' || input.dsgType=='split'", ns = ns,
                shiny::selectInput(ns("dsgBase"), "Plan de base",
                            choices = c("Blocs randomises (RCBD)" = "rcbd",
                                        "Completement randomise (CRD)" = "crd",
                                        "Carre latin (LSD)" = "lsd"))),
              shiny::numericInput(ns("dsgN"), "Échantillons par parcelle élémentaire (n)",
                           value = 1, min = 1, step = 1),
              shiny::numericInput(ns("dsgSeed"), "Graine (reproductibilité)", value = 123, min = 1, step = 1),

              # --- Contraintes de terrain (optionnelles) -------------------------
              shiny::div(style = "border:2px solid #d35400; border-radius:8px; padding:10px 12px; margin:10px 0; background:#fef5ec;",
                shiny::tags$label(style = "color:#a04000; font-weight:700;",
                           shiny::icon("triangle-exclamation"), " Contraintes de terrain (optionnel)"),
                shiny::checkboxInput(ns("dsgObstacle"), "Matérialiser une zone à éviter (obstacle)", value = FALSE),
                shiny::conditionalPanel("input.dsgObstacle == true", ns = ns,
                  shiny::textInput(ns("dsgObstacleLabel"), "Nom de la zone", value = "Zone à éviter",
                            placeholder = "ex : arbre, mare, talus"),
                  shiny::fluidRow(
                    shiny::column(6, shiny::selectInput(ns("dsgObstacleSide"), "Position",
                      choices = c("Bord gauche" = "left", "Bord droit" = "right",
                                  "Bord haut" = "top", "Bord bas" = "bottom"),
                      selected = "left")),
                    shiny::column(6, shiny::sliderInput(ns("dsgObstacleSpan"), "Étendue (parcelles)",
                      min = 1, max = 10, value = 1, step = 1))),
                  shiny::selectInput(ns("dsgObstacleColor"), "Couleur",
                    choices = c("Rouge hachuré" = "#e74c3c", "Gris" = "#7f8c8d",
                                "Brun" = "#8d6e63", "Noir" = "#2c3e50"),
                    selected = "#e74c3c")),
                shiny::checkboxInput(ns("dsgConstraintFactor"), "Ajouter un facteur de blocage supplémentaire", value = FALSE),
                shiny::conditionalPanel("input.dsgConstraintFactor == true", ns = ns,
                  shiny::textInput(ns("dsgConstraintName"), "Nom du facteur", value = "Bande",
                            placeholder = "ex : irrigation, exposition"),
                  shiny::sliderInput(ns("dsgConstraintBands"), "Nombre de bandes",
                    min = 2, max = 8, value = 2, step = 1),
                  shiny::radioButtons(ns("dsgConstraintDir"), "Orientation des bandes",
                    choices = c("Verticales (colonnes)" = "vertical",
                                "Horizontales (rangées)" = "horizontal"),
                    selected = "vertical", inline = TRUE))
              ),

              shiny::actionButton(ns("dsgGenerate"), "Générer le plan", class = "btn-success",
                           icon = shiny::icon("dice"),
                           style = "width:100%; font-weight:bold; background:#16a085; border-color:#138d75; color:#fff;")
          ),
          shinydashboard::box(title = shiny::tagList(shiny::icon("clipboard-list"), " Analyse recommandee & field book"),
              status = "info", width = 8, solidHeader = TRUE,
              shiny::uiOutput(ns("dsgAnalysisInfo")), shiny::hr(),
              DT::DTOutput(ns("dsgTable")), shiny::br(),
              shiny::h4(shiny::icon("list-ol"), " Taille d'échantillon par modalité / combinaison"),
              DT::DTOutput(ns("dsgTreatSummary")), shiny::br(),
              shiny::downloadButton(ns("dsgDownload"), "Télécharger (CSV)", class = "btn-success btn-sm")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(title = shiny::tagList(shiny::icon("map"), " Graphique du dispositif (randomisation)"),
              status = "success", width = 12, solidHeader = TRUE,

              # ===================== TITRES & AXES =====================
              .sect("heading", "Titres & axes"),
              shiny::fluidRow(
                shiny::column(4, shiny::textInput(ns("dsgPlotTitle"), "Titre du graphique", value = "")),
                shiny::column(4, shiny::textInput(ns("dsgXlab"), "Titre axe X", value = "")),
                shiny::column(4, shiny::textInput(ns("dsgYlab"), "Titre axe Y", value = ""))
              ),
              shiny::fluidRow(
                shiny::column(6, shiny::sliderInput(ns("dsgFontAxis"), "Police des axes / du titre",
                                      min = 8, max = 24, value = 12, step = 1))
              ),

              # ================ ÉTIQUETTES DES CELLULES ================
              .sect("font", "Étiquettes des cellules (parcelles)"),
              shiny::fluidRow(
                shiny::column(4, shiny::selectInput(ns("dsgLabelMode"), "Contenu de l'étiquette",
                  choices = c("Traitement + n" = "both", "Traitement seul" = "treat",
                              "n seul" = "n", "Aucune" = "none"))),
                shiny::column(4, shiny::sliderInput(ns("dsgFontLabel"), "Taille police des étiquettes",
                                      min = 2, max = 8, value = 2.8, step = 0.2)),
                shiny::column(4, shiny::div(style = "margin-top:25px;",
                  shiny::checkboxInput(ns("dsgShowText"), "Afficher le texte", value = TRUE)))
              ),
              shiny::fluidRow(
                shiny::column(6, shiny::div(style = "margin-top:5px;",
                  shiny::checkboxInput(ns("dsgBoldLabels"),
                    shiny::tagList(shiny::icon("bold"), " Mettre toutes les étiquettes en gras"),
                    value = FALSE)))
              ),

              # ===================== COULEURS =========================
              .sect("palette", "Couleurs"),
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("dsgPalette"), "Palette de couleurs",
                  choices = c("Vif (défaut)" = "default", "Set2 (doux)" = "Set2",
                              "Dark2" = "Dark2", "Paired" = "Paired",
                              "Spectral" = "Spectral", "Niveaux de gris" = "grey",
                              "Viridis" = "viridis")))
              ),

              # ===================== LÉGENDE ==========================
              .sect("tags", "Légende"),
              shiny::fluidRow(
                shiny::column(4, shiny::selectInput(ns("dsgLegendOrder"), shiny::tagList(shiny::icon("sort"), " Ordre de la légende"),
                  choices = c("Alphabétique / croissant" = "alpha",
                              "Inverse / décroissant" = "rev",
                              "Ordre d'apparition" = "appear"),
                  selected = "alpha")),
                shiny::column(4, shiny::selectInput(ns("dsgLegendPos"), "Position de la légende",
                  choices = c("Droite" = "right", "Bas" = "bottom",
                              "Gauche" = "left", "Haut" = "top", "Aucune" = "none"),
                  selected = "right")),
                shiny::column(4, shiny::textInput(ns("dsgLegendTitle"), "Titre de la légende", value = ""))
              ),
              shiny::fluidRow(
                shiny::column(6, shiny::sliderInput(ns("dsgLegendTextSize"),
                  shiny::tagList(shiny::icon("text-height"), " Taille police des modalités"),
                  min = 6, max = 30, value = 11, step = 1)),
                shiny::column(6, shiny::sliderInput(ns("dsgLegendTitleSize"),
                  shiny::tagList(shiny::icon("heading"), " Taille police du titre"),
                  min = 6, max = 30, value = 12, step = 1))
              ),
              shiny::fluidRow(
                shiny::column(6, shiny::div(style = "margin-top:5px;",
                  shiny::checkboxInput(ns("dsgLegendTextBold"),
                    shiny::tagList(shiny::icon("bold"), " Modalités en gras"),
                    value = FALSE))),
                shiny::column(6, shiny::div(style = "margin-top:5px;",
                  shiny::checkboxInput(ns("dsgLegendTitleBold"),
                    shiny::tagList(shiny::icon("bold"), " Titre en gras"),
                    value = FALSE)))
              ),
              shiny::conditionalPanel("input.dsgType=='split' || input.dsgType=='strip'", ns = ns,
                shiny::fluidRow(shiny::column(12, shiny::checkboxInput(ns("dsgShowF2Legend"),
                  shiny::tagList(shiny::icon("tags"), " Afficher une légende séparée pour le Facteur 2"),
                  value = TRUE)))),

              # ============ GRADIENT D'HÉTÉROGÉNÉITÉ ==================
              .sect("arrows-alt", "Gradient d'hétérogénéité"),
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("dsgGradient"),
                            shiny::tagList(shiny::icon("arrows-alt"), " Sens du gradient"),
                            choices = c("Automatique (perpendiculaire aux blocs)" = "auto",
                                        "Sens direct (haut->bas / gauche->droite)" = "vertical_down",
                                        "Sens inverse (bas->haut / droite->gauche)" = "vertical_up",
                                        "Aucun" = "none"),
                            selected = "auto")),
                shiny::column(6, shiny::conditionalPanel("input.dsgType=='lsd'", ns = ns,
                  shiny::selectInput(ns("dsgGradient2"),
                            shiny::tagList(shiny::icon("arrows-alt-h"), " Orientation du 2e gradient"),
                            choices = c("Automatique" = "auto",
                                        "Horizontal : gauche -> droite" = "horizontal_right",
                                        "Horizontal : droite -> gauche" = "horizontal_left",
                                        "Vertical : haut -> bas" = "vertical_down",
                                        "Vertical : bas -> haut" = "vertical_up",
                                        "Aucun" = "none"),
                            selected = "auto")))
              ),
              shiny::conditionalPanel("input.dsgGradient != 'none'", ns = ns,
                shiny::fluidRow(
                  shiny::column(7, shiny::textInput(ns("dsgGradLabel"), "Texte du gradient", value = "Gradient")),
                  shiny::column(5, shiny::sliderInput(ns("dsgGradSize"), "Taille du texte",
                                        min = 2, max = 8, value = 3.5, step = 0.5)))),

              # ============ BLOCS & SÉPARATEURS =======================
              .sect("border-all", "Blocs & séparateurs"),
              shiny::fluidRow(
                shiny::column(4, shiny::sliderInput(ns("dsgCellSep"),
                  "Largeur de séparation des parcelles",
                  min = 0, max = 6, value = 1, step = 0.25)),
                shiny::column(4, shiny::sliderInput(ns("dsgBlockLine"),
                  "Épaisseur des traits de séparation des blocs",
                  min = 0, max = 6, value = 2, step = 0.5)),
                shiny::column(4, shiny::selectInput(ns("dsgBlockColor"), "Couleur des séparateurs de blocs",
                  choices = c("Noir" = "black", "Gris foncé" = "grey20",
                              "Rouge" = "#c0392b", "Bleu foncé" = "#1a2980")))
              ),
              shiny::fluidRow(
                shiny::column(6, shiny::textInput(ns("dsgBlockPrefix"), "Préfixe des noms de bloc",
                                    value = "Bloc")),
                shiny::column(6, shiny::textInput(ns("dsgBlockNames"),
                  "Noms personnalisés des blocs / répliques (séparés par virgule)", value = "",
                  placeholder = "ex: Nord, Centre, Sud"))
              ),
              # -- Sous-parcelles (split / split-split) : traits pointillés & bandeau --
              shiny::fluidRow(
                shiny::column(4, shiny::sliderInput(ns("dsgSubLine"),
                  "Épaisseur des traits pointillés (sous-parcelles)",
                  min = 0, max = 3, value = 0.8, step = 0.1)),
                shiny::column(4, colourInput(ns("dsgSubColor"),
                  "Couleur des traits pointillés", value = "#000000")),
                shiny::column(4, colourInput(ns("dsgBandColor"),
                  "Couleur du bandeau (Facteur 2)", value = "#D6EAF8"))
              ),
              shiny::fluidRow(
                shiny::column(4, shiny::textInput(ns("dsgBlockStart"),
                  "Numérotation des blocs : numéro de départ", value = "1"))
              ),

              # ============ RANDOMISATION =============================
              .sect("random", "Randomisation"),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("dsgRandTries"), "Itérations de randomisation",
                                       value = 200, min = 1, max = 2000, step = 50))
              ),
              # ============ APERÇU DU DISPOSITIF ======================
              .sect("map", "Aperçu du dispositif"),
              shiny::plotOutput(ns("dsgPlot"), height = "540px"),
              shiny::br(),
              # ---- Réglages rapides de disposition (juste sous la figure) ----
              shiny::fluidRow(
                shiny::column(4, shiny::selectInput(ns("dsgBlockOrient"),
                  shiny::tagList(shiny::icon("arrows-alt"), " Sens des blocs"),
                  choices = c("Haut -> bas" = "top_down",
                              "Bas -> haut" = "bottom_up",
                              "Gauche -> droite" = "left_right",
                              "Droite -> gauche" = "right_left"),
                  selected = "top_down")),
                shiny::column(4,
                  shiny::conditionalPanel("input.dsgType != 'paired'", ns = ns,
                    shiny::selectInput(ns("dsgTreatOrient"),
                      shiny::tagList(shiny::icon("grip-horizontal"), " Orientation des traitements dans le bloc"),
                      choices = c("Automatique (selon le gradient)" = "auto",
                                  "Horizontale (blocs empilés, parcelles en rangées)" = "horizontal",
                                  "Verticale (blocs côte à côte, parcelles en colonnes)" = "vertical"),
                      selected = "auto")),
                  shiny::conditionalPanel("input.dsgType == 'paired'", ns = ns,
                    shiny::selectInput(ns("dsgPairedLayout"),
                      shiny::tagList(shiny::icon("grip-lines"), " Disposition des couples"),
                      choices = c("Diagonale (escalier)" = "diagonal",
                                  "Horizontale (couples en facettes)" = "horizontal",
                                  "Verticale (couples en facettes)" = "vertical"),
                      selected = "diagonal"))),
                shiny::column(4, shiny::div(style = "margin-top:25px;",
                  shiny::checkboxInput(ns("dsgFacetBlocks"),
                    shiny::tagList(shiny::icon("th"), " Afficher chaque bloc en facette (avec son nom)"),
                    value = TRUE)))
              ),
              # ============ EXPORT DE L'IMAGE =========================
              .sect("download", "Export de l'image"),
              hstat_export_plot_ui(ns, "dsgPl", width = 11, height = 7),
              footer = shiny::div(style = "font-size:12px;color:#7f8c8d;", shiny::icon("info-circle"),
                " Chaque cellule = une unité expérimentale ; couleur = traitement randomise. ",
                "Pour un CRD/factoriel, le placement minimise les voisins identiques."))
        )
      ),

      # ================= ONGLET ENQUETE DE TERRAIN =================
      shiny::tabPanel(shiny::tagList(shiny::icon("clipboard-check"), " Enquête de terrain"), value = "survey", shiny::br(),
        shiny::fluidRow(
          shinydashboard::box(title = shiny::tagList(shiny::icon("sliders"), " Paramètres de l'enquête"),
              status = "primary", width = 5, solidHeader = TRUE,
              shiny::selectInput(ns("svObjective"), "Objectif d'estimation",
                choices = c("Une proportion (ex. taux d'adoption)" = "proportion",
                            "Une moyenne (variable quantitative)"  = "moyenne")),
              shiny::sliderInput(ns("svConf"), "Niveau de confiance",
                          min = 0.80, max = 0.99, value = 0.95, step = 0.01),
              shiny::conditionalPanel("input.svObjective == 'proportion'", ns = ns,
                shiny::numericInput(ns("svMargin"), "Marge d'erreur (en proportion, ex. 0.05 = 5%)",
                             value = 0.05, min = 0.005, max = 0.5, step = 0.005),
                shiny::sliderInput(ns("svP"), "Proportion attendue p (0.5 = cas le plus prudent)",
                            min = 0.05, max = 0.95, value = 0.5, step = 0.05)),
              shiny::conditionalPanel("input.svObjective == 'moyenne'", ns = ns,
                shiny::numericInput(ns("svMarginM"), "Marge d'erreur absolue (mêmes unités que la variable)",
                             value = 2, min = 0.01, step = 0.5),
                shiny::numericInput(ns("svSd"), "Écart-type estimé (pilote / litterature)",
                             value = 15, min = 0.01, step = 1)),
              shiny::numericInput(ns("svPop"), "Taille de la population (0 ou vide = infinie)",
                           value = 0, min = 0, step = 100),
              shiny::numericInput(ns("svDeff"), "Effet de plan (design effect, 1 = sondage aleatoire simple ; 1.5-2 = grappes)",
                           value = 1, min = 1, step = 0.1),
              shiny::sliderInput(ns("svResp"), "Taux de réponse anticipé",
                          min = 0.3, max = 1, value = 0.8, step = 0.05),
              shiny::numericInput(ns("svStrata"), "Nombre de strates (1 = pas de stratification)",
                           value = 1, min = 1, step = 1),
              shiny::actionButton(ns("svCalc"), "Calculer la taille", class = "btn-success",
                           icon = shiny::icon("calculator"))
          ),
          shinydashboard::box(title = shiny::tagList(shiny::icon("users"), " Taille d'échantillon requise"),
              status = "success", width = 7, solidHeader = TRUE,
              shiny::uiOutput(ns("svResult")),
              shiny::hr(),
              DT::DTOutput(ns("svTable")),
              shiny::br(),
              shiny::div(class = "callout callout-info", style = "font-size:12px;",
                shiny::icon("info-circle"),
                " La taille finale tient compte, dans l'ordre : taille de base, ",
                "correction pour population finie, effet de plan, puis ajustement ",
                "pour la non-réponse.")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(title = shiny::tagList(shiny::icon("chart-line"), " Marge d'erreur selon la taille d'échantillon"),
              status = "info", width = 12, solidHeader = TRUE,
              shiny::plotOutput(ns("svCurve"), height = "320px"))
        )
      )
    )
  )
}

mod_design_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

    # Dispositif en couples : "Disposition des couples" pilote directement
    # l'orientation des membres du couple, donc "Orientation des traitements dans
    # le bloc" n'a aucun effet pour CE dispositif -> le controle est MASQUE
    # (conditionalPanel dans l'UI, sous la figure) tant que le dispositif en
    # couples est selectionne, et reapparait pour tous les autres dispositifs.
    # On remet aussi sa valeur sur "Automatique" pour qu'aucun reglage residuel
    # n'influence le rendu du dispositif en couples.
    shiny::observe({
      is_paired <- identical(input$dsgType, "paired")
      if (is_paired && !identical(input$dsgTreatOrient %||% "auto", "auto"))
        shiny::updateSelectInput(session, "dsgTreatOrient", selected = "auto")
    })

    # Reinitialisation globale : ramene les controles du module "Plan & Puissance"
    # (plan experimental + calcul de puissance) a leurs valeurs par defaut.
    shiny::observeEvent(values$resetSignal, {
      if ((values$resetSignal %||% 0) == 0) return()
      # -- Plan experimental --
      shiny::updateSelectInput(session, "dsgTreatOrient", selected = "auto")
      shiny::updateSelectInput(session, "dsgPairedLayout", selected = "diagonal")
      shiny::updateNumericInput(session, "dsgBlockLine", value = 2)
      shiny::updateTextInput(session, "dsgBlockStart", value = "1")
      shiny::updateTextInput(session, "dsgBlockPrefix", value = "Bloc")
      shiny::updateTextInput(session, "dsgBlockNames", value = "")
      shiny::updateCheckboxInput(session, "dsgBoldLabels", value = FALSE)
      shiny::updateCheckboxInput(session, "dsgLegendTextBold", value = FALSE)
      shiny::updateCheckboxInput(session, "dsgLegendTitleBold", value = FALSE)
      # -- Calcul de puissance --
      shiny::updateNumericInput(session, "powAlpha", value = 0.05)
      shiny::updateNumericInput(session, "powPower", value = 0.95)
      shiny::updateNumericInput(session, "powEffect", value = 0.25)
      shiny::updateNumericInput(session, "powGroups", value = 5)
      shiny::updateNumericInput(session, "powK", value = 1)
    }, ignoreInit = TRUE)

    output$powTestSelect <- shiny::renderUI({
      fam <- input$powFamily %||% "t"
      shiny::selectInput(ns("powTest"), "Test statistique", choices = hstat_power_families()[[fam]])
    })

    output$powTailUI <- shiny::renderUI({
      test <- input$powTest %||% "t_two"
      one_or_two <- c("t_two", "t_paired", "t_one", "mwu", "wilcox_paired",
                      "wilcox_one", "t_generic", "cor_pb", "cor_biv", "cor_tetra",
                      "cor_2indep", "cor_2dep", "logistic", "poisson", "reg_slope",
                      "prop2", "prop1", "mcnemar", "sign")
      if (test %in% one_or_two)
        shiny::selectInput(ns("powAlt"), "Queue(s)",
                    choices = c("Bilaterale" = "two.sided", "Unilaterale" = "greater"))
    })

    # ddl numerateur et nombre de cellules calcules a partir des modalites saisies
    pow_factorial <- shiny::reactive({
      nf <- input$powNFactors %||% 2
      levs <- vapply(seq_len(nf), function(i) {
        v <- input[[paste0("powLev", i)]]
        if (is.null(v) || is.na(v)) NA_integer_ else as.integer(v)
      }, integer(1))
      levs <- levs[!is.na(levs)]
      if (length(levs) == 0) levs <- c(2, 3)[seq_len(max(1, nf))]
      hstat_factorial_df(levs, input$powEffectTarget %||% "main1")
    })

    output$powDfComputed <- shiny::renderUI({
      fd <- pow_factorial()
      shiny::div(class = "callout", style = "border-left:4px solid #27ae60;padding:8px 12px;background:#eafaf1;",
          shiny::icon("calculator"),
          shiny::HTML(trf(" <b>ddl du numerateur calculé = %d</b> &nbsp;|&nbsp; Nombre de cellules (groupes) = %d",
                       fd$df1, fd$cells)))
    })

    output$powExtraUI <- shiny::renderUI({
      test <- input$powTest %||% "t_two"; analysis <- input$powAnalysis %||% "apriori"
      F_factorial <- c("anova_factorial", "rm_between", "rm_within", "rm_interaction",
                       "ancova", "manova_global", "reg_r2inc", "reg_r2dev", "f_generic")
      els <- list()
      if (test == "anova_oneway")
        els <- c(els, list(shiny::numericInput(ns("powGroups"), "Nombre de groupes (k)", value = 5, min = 2, step = 1)))
      if (test %in% F_factorial) {
        nf <- input$powNFactors %||% 2
        els <- c(els, list(
          shiny::div(class = "callout callout-info", style = "padding:8px 10px;font-size:12px;",
            shiny::icon("lightbulb"),
            shiny::HTML(" <b>Aide ddl numerateur</b> : effet principal d'un facteur a <i>k</i> ",
                 "modalités &rarr; ddl = <i>k</i>&minus;1 ; interaction A&times;B &rarr; ",
                 "ddl = (a&minus;1)(b&minus;1) ; interaction A&times;B&times;C &rarr; ",
                 "ddl = (a&minus;1)(b&minus;1)(c&minus;1). Nombre de cellules = produit des modalités. ",
                 "Renseignez les modalités ci-dessous : le ddl est calculé automatiquement.")),
          shiny::numericInput(ns("powNFactors"), "Nombre de facteurs", value = nf,
                       min = 1, max = 4, step = 1)))
        for (i in seq_len(nf))
          els <- c(els, list(shiny::numericInput(ns(paste0("powLev", i)),
            trf("Nombre de modalités du facteur %s", LETTERS[i]),
            value = if (i == 1) 2 else 3, min = 2, step = 1)))
        eff_choices <- c(stats::setNames(paste0("main", seq_len(nf)),
                                  sprintf("Effet principal du facteur %s", LETTERS[seq_len(nf)])))
        if (nf >= 2)
          eff_choices <- c(eff_choices,
            stats::setNames("inter2", "Interaction A x B"),
            if (nf >= 3) stats::setNames("inter3", "Interaction A x B x C"),
            if (nf >= 4) stats::setNames("inter4", "Interaction A x B x C x D"),
            stats::setNames("interAll", "Interaction de tous les facteurs"))
        # Conserver le choix courant si toujours valide (evite la remise a main1)
        cur_target <- input$powEffectTarget
        sel_target <- if (!is.null(cur_target) && cur_target %in% eff_choices) cur_target else eff_choices[[1]]
        els <- c(els, list(
          shiny::selectInput(ns("powEffectTarget"), "Effet cible (pour le ddl)",
                      choices = eff_choices, selected = sel_target),
          shiny::uiOutput(ns("powDfComputed")),
          shiny::numericInput(ns("powCovars"), "Nombre de covariables (0 si ANOVA)",
                       value = 0, min = 0, step = 1)))
      }
      if (test %in% c("gof", "chisq_generic"))
        els <- c(els, list(shiny::numericInput(ns("powK"), "Degrés de liberté (df)", value = 1, min = 1, step = 1)))
      if (analysis != "apriori") {
        per_group <- test %in% c("anova_oneway", "t_two", "t_paired", "t_one",
                                 "mwu", "wilcox_paired", "wilcox_one", "t_generic",
                                 "prop2", "prop1", "mcnemar", "sign")
        is_F_total <- test %in% F_factorial
        lbl <- if (is_F_total) "Taille totale (N)" else if (per_group) "n par groupe" else "Taille (n / N total)"
        els <- c(els, list(shiny::numericInput(ns("powN"), lbl, value = if (is_F_total) 60 else 30, min = 2, step = 1)))
      }
      do.call(shiny::tagList, els)
    })

    output$powEffectHint <- shiny::renderUI({
      test <- input$powTest %||% "t_two"
      lab <- if (test %in% c("t_two", "t_paired", "t_one", "mwu", "wilcox_paired", "wilcox_one", "t_generic")) "d de Cohen"
             else if (test %in% c("cor_pb", "cor_biv", "cor_tetra", "reg_slope")) "coefficient r"
             else if (test %in% c("cor_2indep", "cor_2dep")) "premier coefficient r (r1) ; renseignez r2 ci-dessous"
             else if (test == "logistic") "odds-ratio (OR) par écart-type du prédicteur"
             else if (test == "poisson") "ratio de taux (RR) par écart-type du prédicteur"
             else if (test == "anova_oneway") "f de Cohen"
             else if (test %in% c("anova_factorial", "rm_between", "rm_within", "rm_interaction",
                                  "ancova", "manova_global", "reg_r2inc", "reg_r2dev", "f_generic")) "f de Cohen (lambda = f\u00b2 \u00d7 N)"
             else if (test %in% c("gof", "chisq_generic")) "w de Cohen"
             else if (test %in% c("prop2", "prop1", "mcnemar", "sign")) "h de Cohen"
             else ""
      shiny::div(style = "font-size:12px;color:#7f8c8d;margin-top:-8px;margin-bottom:8px;",
          "Mesure : ", shiny::tags$b(lab))
    })

    output$powConventions <- DT::renderDT({
      DT::datatable(hstat_effect_conventions(), rownames = FALSE, options = list(pageLength = 18, dom = "t"))
    })

    # Depot du calcul de puissance pour l'aide a la decision.
    shiny::observeEvent(pow_res(), {
      r <- tryCatch(pow_res(), error = function(e) NULL)
      tb <- hstat_ai_as_table(if (is.list(r) && !is.null(r$table)) r$table else r)
      if (is.null(tb) || !NROW(tb)) return()
      hstat_ai_capture(values, "Plan & Puissance",
        sprintf("Analyse de puissance (%s)", input$powTest %||% "t_two"),
        tables = list("Resultat du calcul" = tb),
        meta = list(`type d'analyse` = input$powAnalysis,
                    `taille d'effet` = input$powEffect,
                    alpha = input$powAlpha, puissance = input$powPower))
    }, ignoreInit = TRUE)

    pow_res <- shiny::eventReactive(input$powCalc, {
      test <- input$powTest %||% "t_two"
      F_factorial <- c("anova_factorial", "rm_between", "rm_within", "rm_interaction",
                       "ancova", "manova_global", "reg_r2inc", "reg_r2dev", "f_generic")
      # Pour les tests F factoriels, df1 et groups sont calcules a partir des modalites
      df1_use <- if (test %in% F_factorial) pow_factorial()$df1 else input$powDf1
      groups_use <- if (test %in% F_factorial) pow_factorial()$cells else input$powGroups
      hstat_gpower(test = test, analysis = input$powAnalysis %||% "apriori",
        effect = input$powEffect, effect2 = input$powEffect2,
        alpha = input$powAlpha %||% 0.05, power = input$powPower %||% 0.95,
        n = input$powN, k = input$powK, df1 = df1_use, groups = groups_use,
        covars = input$powCovars %||% 0, alt = input$powAlt %||% "two.sided",
        p0 = input$powP0, base_rate = input$powBaseRate,
        mean_exposure = input$powExposure,
        predictor = input$powPredictor %||% "continuous", px = input$powPx,
        R2_other = if ((input$powTest %||% "") == "poisson") input$powR2otherP %||% 0
                   else input$powR2other %||% 0)
    })

    output$powOutputTable <- DT::renderDT({
      r <- pow_res(); shiny::validate(shiny::need(is.null(r$err), r$err %||% ""))
      fmt <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) "\u2014" else format(round(x, 4), nsmall = 0)
      tab <- data.frame(
        Parametre = c("Non-centralite \u03bb / \u03b4", r$crit_lab, "Degrés de liberté",
                      "Taille d'effet", "Taille totale (N)", "Puissance reelle (1-\u03b2)"),
        Valeur = c(fmt(r$lambda), fmt(r$crit), r$df_lab, fmt(r$effect), as.character(r$ntot), fmt(r$power)),
        stringsAsFactors = FALSE)
      DT::datatable(tab, rownames = FALSE, options = list(dom = "t", ordering = FALSE),
        caption = htmltools::tags$caption(style = "caption-side:top;font-weight:600;",
          "Paramètres de sortie (style G*Power)"))
    })

    output$powVerdict <- shiny::renderUI({
      r <- pow_res(); shiny::req(is.null(r$err)); pw <- r$power
      if (is.null(pw) || is.na(pw)) return(NULL)
      col <- if (pw >= 0.80) "#27ae60" else if (pw >= 0.5) "#f39c12" else "#c0392b"
      msg <- if (pw >= 0.80) "Puissance adequate (\u2265 0.80)." else if (pw >= 0.5) "Puissance modérée." else "Puissance insuffisante."
      shiny::div(class = "callout", style = sprintf("border-left:4px solid %s;padding:8px 12px;", col),
          shiny::icon("info-circle"), sprintf(" %s  N total = %s.", msg, r$ntot))
    })

    output$powAllocNote <- shiny::renderUI({
      r <- pow_res()
      shiny::tagList(
        shiny::div(style = "font-size:12px;color:#7f8c8d;",
            "Répartition de la taille totale par groupe/traitement issue du calcul de puissance."),
        if (!is.null(r) && is.null(r$err) && !is.null(r$note))
          shiny::div(class = "callout callout-warning", style = "padding:6px 10px;font-size:12px;margin-top:6px;",
              shiny::icon("exclamation-triangle"), " ", r$note)
      )
    })
    output$powAllocTable <- DT::renderDT({
      r <- pow_res(); shiny::req(is.null(r$err)); nper <- r$nper
      tab <- if (!is.null(nper) && !is.na(nper)) {
        ng <- input$powGroups %||% (if ((input$powTest %||% "") == "anova_oneway") 2 else 2)
        data.frame(Indicateur = c("Unités par groupe/traitement", "Nombre de groupes/cellules", "Taille totale (N)"),
                   Valeur = c(nper, ng, r$ntot), stringsAsFactors = FALSE)
      } else data.frame(Indicateur = "Taille totale (N)", Valeur = r$ntot, stringsAsFactors = FALSE)
      DT::datatable(tab, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
    })

    # Nombre de traitements estime pour la repartition (selon le test)
    pow_n_treatments <- shiny::reactive({
      test <- input$powTest %||% "t_two"
      if (test == "anova_oneway") input$powGroups %||% 2
      else if (test %in% c("anova_factorial", "rm_between", "rm_within",
                           "rm_interaction", "ancova", "manova_global"))
        input$powGroups %||% 2
      else 2
    })

    pow_repartition <- shiny::reactive({
      r <- pow_res(); shiny::req(is.null(r$err))
      nper <- r$nper
      if (is.null(nper) || is.na(nper)) {
        # tests sans n/groupe (correlation, regression...) : repartir la taille totale
        nper <- r$ntot
      }
      hstat_repartition(nper, pow_n_treatments(),
                        mode = input$powRepartMode %||% "blocks")
    })

    output$powRepartPreview <- shiny::renderUI({
      rp <- tryCatch(pow_repartition(), error = function(e) NULL)
      shiny::req(!is.null(rp))
      shiny::div(class = "callout callout-info", style = "margin-top:10px;",
          shiny::icon("info-circle"),
          trf(" Répartition proposée : %d répétition(s)/bloc(s) x %d échantillon(s) par parcelle = %d unités par traitement (total %d). Cliquez sur le bouton pour l'appliquer au plan.",
                  rp$`répétitions`, rp$samples_per_plot, rp$per_treatment, rp$total))
    })

    shiny::observeEvent(input$powToDesign, {
      rp <- tryCatch(pow_repartition(), error = function(e) NULL)
      shiny::req(!is.null(rp))
      shiny::updateNumericInput(session, "dsgRep", value = rp$`répétitions`)
      shiny::updateNumericInput(session, "dsgN", value = rp$samples_per_plot)
      shiny::updateTabsetPanel(session, "designTabs", selected = "plan")
      shiny::showNotification(
        trf("Taille transférée : %d répétitions x %d échantillon(s)/parcelle. Vérifiez l'onglet Plan expérimental.",
                rp$`répétitions`, rp$samples_per_plot),
        type = "message", duration = 6)
    })

    output$powCurve <- shiny::renderPlot({
      r <- pow_res(); shiny::req(is.null(r$err))
      test <- input$powTest %||% "t_two"; alpha <- input$powAlpha %||% 0.05
      eff <- r$effect %||% input$powEffect; alt <- input$powAlt %||% "two.sided"
      k <- input$powK; df1 <- input$powDf1; groups <- input$powGroups; covars <- input$powCovars %||% 0
      F_total <- test %in% c("anova_factorial", "rm_between", "rm_within", "rm_interaction",
                             "ancova", "manova_global", "reg_r2inc", "reg_r2dev", "f_generic")
      # Bornes assainies : %||% ne protege que contre NULL, or un champ vide
      # renvoie NA et un calcul de puissance peut renvoyer n = Inf (effet ~0) ;
      # seq() exige des bornes finies.
      base <- if (F_total) hstat_finite(r$ntot, 60) else hstat_finite(r$nper, 60)
      from <- if (F_total) hstat_finite(df1, 2) + hstat_finite(groups, 2) +
        hstat_finite(covars, 0) + 2 else 3
      to   <- min(max(120, ceiling(base * 2)), 100000)
      if (!is.finite(from) || !is.finite(to) || to <= from) { from <- 3; to <- 120 }
      # Pas adaptatif : au plus ~600 evaluations de puissance, sinon la courbe
      # gele l'interface quand le n requis est tres grand.
      ns_seq <- seq(from, to, by = max(1, ceiling((to - from) / 600)))
      pw <- vapply(ns_seq, function(nn) {
        res <- suppressWarnings(tryCatch(hstat_gpower(test, "posthoc", effect = eff, alpha = alpha,
          n = nn, k = k, df1 = df1, groups = groups, covars = covars, alt = alt)$power,
          error = function(e) NA))
        if (is.null(res)) NA_real_ else res
      }, numeric(1))
      d <- data.frame(n = ns_seq, power = pw); d <- d[!is.na(d$power), ]
      shiny::validate(shiny::need(nrow(d) > 0, "Courbe indisponible pour ce test."))
      xlab <- if (F_total) "Taille totale (N)" else "Taille par groupe (n)"
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        plot(d$n, d$power, type = "l", ylim = c(0, 1), xlab = xlab, ylab = "Puissance")
        graphics::abline(h = 0.8, lty = 2); return(invisible())
      }
      ggplot2::ggplot(d, ggplot2::aes(n, power)) +
        ggplot2::geom_line(color = "#2980b9", linewidth = 1) +
        ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed", color = "#c0392b") +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::labs(x = "Taille par groupe (n)", y = "Puissance (1-\u03b2)", title = "Courbe de puissance") +
        ggplot2::theme_minimal(base_size = 13)
    })

    # Le nombre de repetitions usuel du dispositif est propose des qu'on le
    # choisit ; il reste modifiable, c'est une suggestion et non une contrainte.
    shiny::observeEvent(input$dsgType, {
      mh <- hstat_malherbo_catalog()[[input$dsgType %||% ""]]
      if (!is.null(mh)) shiny::updateNumericInput(session, "dsgRep", value = mh$r)
    }, ignoreInit = TRUE)

    output$dsgHint <- shiny::renderUI({
      t <- input$dsgType %||% "crd"
      mh <- hstat_malherbo_catalog()[[t]]
      if (!is.null(mh)) {
        nb <- prod(vapply(mh$facteurs, length, integer(1)))
        return(shiny::div(style = sprintf(paste0("border-left:4px solid %s;background:#faf7fd;",
                                          "border-radius:0 6px 6px 0;padding:10px 14px;margin:8px 0;"),
                                   mh$couleur),
          shiny::tags$b(style = sprintf("color:%s;", mh$couleur), shiny::icon("seedling"), " ", mh$label),
          shiny::tags$p(style = "margin:6px 0 0 0;font-size:13px;", shiny::tags$b("But : "), mh$but),
          shiny::tags$p(style = "margin:4px 0 0 0;font-size:13px;", shiny::tags$b("Plan : "),
                 trf("%d traitements x %d repetitions = %d parcelles, sur un plan %s.",
                     nb, mh$r, nb * mh$r,
                     names(which(hstat_design_catalog() == mh$base))[1] %||% mh$base)),
          shiny::tags$p(style = "margin:4px 0 0 0;font-size:13px;", shiny::tags$b("A mesurer : "), mh$mesures),
          shiny::tags$p(style = "margin:4px 0 0 0;font-size:13px;", shiny::tags$b("Modele : "),
                 shiny::tags$code(mh$modele)),
          shiny::div(style = "margin-top:8px;padding:8px 10px;background:#fff4e5;border-left:3px solid #e67e22;border-radius:0 4px 4px 0;",
              shiny::tags$b(style = "color:#a04000;", shiny::icon("triangle-exclamation"), " Le piege : "),
              shiny::tags$span(style = "font-size:13px;", mh$piege))))
      }
      # Description structurelle precise de chaque dispositif (facteurs, gradients
      # d'heterogeneite, blocs, contraintes) selon les conventions agronomiques.
      info <- switch(t,
        "crd" = list(
          facteurs = "1 facteur etudie",
          gradient = "0 gradient d'hétérogénéité (milieu suppose homogene)",
          structure = "Sans bloc, avec répétitions. Les traitements sont affectés totalement au hasard aux parcelles.",
          couleur = "#27ae60"),
        "fisher" = list(
          facteurs = "1 facteur etudie",
          gradient = "1 gradient d'hétérogénéité",
          structure = "Bloc de Fisher (RCBD) : chaque bloc (= 1 répétition) est une rangee complete contenant tous les traitements une fois, randomises a l'intérieur. Les blocs sont perpendiculaires au gradient. Nombre de blocs = nombre de répétitions.",
          couleur = "#2980b9"),
        "paired" = list(
          facteurs = "1 facteur etudie (souvent 2 traitements)",
          gradient = "1 gradient d'hétérogénéité",
          structure = "Couple apparie (BCR) : chaque bloc contient une paire (ou un petit groupe) de traitements compares sur des unités très semblables. Représentation en escalier diagonal suivant le gradient.",
          couleur = "#16a085"),
        "lsd" = list(
          facteurs = "1 facteur etudie",
          gradient = "2 gradients d'hétérogénéité (ligne et colonne)",
          structure = "Carre latin : chaque traitement apparait exactement une fois par ligne ET une fois par colonne. Contrôle deux sources de variation orthogonales.",
          couleur = "#8e44ad"),
        "alpha" = list(
          facteurs = "1 facteur etudie",
          gradient = "1 gradient d'hétérogénéité",
          structure = "Blocs incomplets resolvables (alpha-design) : permet de tester un grand nombre de traitements sans les placer tous dans chaque bloc. 1 contrainte expérimentale (taille de bloc < nombre de traitements).",
          couleur = "#2980b9"),
        "factorial" = list(
          facteurs = "Au moins 2 facteurs etudies (croisés)",
          gradient = "1 gradient d'hétérogénéité",
          structure = "Toutes les combinaisons des niveaux des facteurs sont testees, organisees en blocs perpendiculaires au gradient.",
          couleur = "#e67e22"),
        "split" = list(
          facteurs = "2 facteurs etudies",
          gradient = "1 gradient d'hétérogénéité, 1 contrainte expérimentale",
          structure = "Parcelles divisees : le facteur principal est applique sur de GRANDES parcelles, le facteur secondaire sur des SOUS-parcelles a l'intérieur. La contrainte porte sur l'application du facteur principal.",
          couleur = "#e67e22"),
        "splitsplit" = list(
          facteurs = "3 facteurs etudies (parcelle principale, sous-parcelle, sous-sous-parcelle)",
          gradient = "1 gradient d'hétérogénéité, 2 contraintes expérimentales emboîtées",
          structure = "Split-split-plot : le facteur A (parcelle principale) est applique sur de GRANDES parcelles ; le facteur B (sous-parcelle) est randomise a l'intérieur de chaque grande parcelle ; le facteur C (sous-sous-parcelle) est randomise a l'intérieur de chaque sous-parcelle. Trois niveaux d'erreur emboîtés.",
          couleur = "#e67e22"),
        "strip" = list(
          facteurs = "2 facteurs etudies",
          gradient = "1 gradient d'hétérogénéité, contraintes expérimentales",
          structure = "Criss-Cross (bandes) : les deux facteurs sont appliques en BANDES perpendiculaires. Les combinaisons de traitements apparaissent aux INTERSECTIONS des bandes.",
          couleur = "#e67e22"),
        list(facteurs = "1 facteur", gradient = "-", structure = "-", couleur = "#3c8dbc"))
      shiny::div(style = sprintf("border-left:4px solid %s;background:#f8f9fa;padding:10px 12px;margin:6px 0;font-size:12px;", info$couleur),
        shiny::div(style = "font-weight:700;color:#2c3e50;margin-bottom:4px;",
            shiny::icon("seedling"), " Structure du dispositif"),
        shiny::tags$ul(style = "margin:0;padding-left:18px;",
          shiny::tags$li(shiny::tags$b("Facteurs : "), info$facteurs),
          shiny::tags$li(shiny::tags$b("Heterogeneite : "), info$gradient),
          shiny::tags$li(info$structure)))
    })

    # Recupere le nombre de facteurs et de modalites definis dans l'onglet Puissance
    # statistique et pre-remplit le plan factoriel (rend le module interactif).
    shiny::observeEvent(input$dsgImportPower, {
      nf <- input$powNFactors %||% 2
      levs <- vapply(seq_len(nf), function(i) {
        v <- input[[paste0("powLev", i)]]
        if (is.null(v) || is.na(v)) NA_integer_ else as.integer(v)
      }, integer(1))
      levs <- levs[!is.na(levs)]
      if (length(levs) == 0) {
        shiny::showNotification("Aucun facteur defini dans l'onglet Puissance statistique.",
                         type = "warning"); return()
      }
      # Bascule sur un plan factoriel si plusieurs facteurs
      if (length(levs) >= 2) shiny::updateSelectInput(session, "dsgType", selected = "factorial")
      shiny::updateNumericInput(session, "dsgNFactors", value = length(levs))
      # Pre-remplir noms + modalites generees (F1_M1, F1_M2, ...)
      for (i in seq_along(levs)) {
        shiny::updateTextInput(session, paste0("dsgFName", i), value = LETTERS[i])
        mods <- paste0(LETTERS[i], seq_len(levs[i]))
        shiny::updateTextInput(session, paste0("dsgFLevels", i),
                        value = paste(mods, collapse = ", "))
      }
      shiny::showNotification(
        trf("Importe depuis Puissance : %d facteur(s) (%s modalités).",
                length(levs), paste(levs, collapse = " x ")),
        type = "message", duration = 5)
    })

    output$dsgFactorInputs <- shiny::renderUI({
      t <- input$dsgType %||% "crd"
      mh <- hstat_malherbo_catalog()[[t]]
      nf <- if (!is.null(mh)) length(mh$facteurs)
            else if (t == "splitsplit") 3 else if (t %in% c("split", "strip")) 2 else if (t == "factorial") max(2, input$dsgNFactors %||% 2) else 1
      lapply(seq_len(nf), function(i) {
        default_letter <- LETTERS[i]              # A, B, C...
        default_levels <- if (i == 1) "A0, A1, A2" else paste0(default_letter, 0:1, collapse = ", ")
        # Dispositif de malherbologie : les modalites usuelles sont proposees
        # d'emblee. Elles restent EDITABLES -- un essai se cale sur ses propres
        # doses et ses propres densites, le catalogue ne fait que dispenser de
        # les retaper.
        if (!is.null(mh)) {
          nom_i <- names(mh$facteurs)[i]
          return(shiny::div(style = "border-left:3px solid #8e44ad;padding-left:8px;margin-bottom:8px;",
            shiny::textInput(ns(paste0("dsgFName", i)), paste0("Nom du facteur ", i), value = nom_i),
            shiny::textInput(ns(paste0("dsgFLevels", i)),
                      paste0("Modalites de ", nom_i, " (separees par virgule)"),
                      value = paste(mh$facteurs[[i]], collapse = ", "))))
        }
        shiny::div(style = "border-left:3px solid #3c8dbc;padding-left:8px;margin-bottom:8px;",
          shiny::textInput(ns(paste0("dsgFName", i)), paste0("Nom du facteur ", i), value = paste0("Facteur", i)),
          # Generateur automatique de modalites : lettre + chiffre de debut + chiffre de fin.
          # Ex : lettre = A, début = 0, fin = 2  ->  A0, A1, A2 (rempli automatiquement ci-dessous).
          shiny::fluidRow(
            shiny::column(4, shiny::textInput(ns(paste0("dsgFLetter", i)),
                                shiny::tagList(shiny::icon("font"), " Lettre"), value = default_letter)),
            shiny::column(4, shiny::numericInput(ns(paste0("dsgFStart", i)),
                                   shiny::tagList(shiny::icon("play"), " Début"), value = 0, min = 0, step = 1)),
            shiny::column(4, shiny::numericInput(ns(paste0("dsgFEnd", i)),
                                   shiny::tagList(shiny::icon("stop"), " Fin"), value = if (i == 1) 2 else 1, min = 0, step = 1))
          ),
          shiny::textInput(ns(paste0("dsgFLevels", i)),
                    paste0("Modalités du facteur ", i, " (séparées par virgule)"),
                    value = default_levels))
      })
    })

    # Generation automatique de la suite de modalites (lettre + debut..fin) qui
    # remplit le champ "separees par virgule". L'utilisateur peut ensuite editer
    # ce champ manuellement s'il le souhaite. Le debut peut valoir 0 (ex : A0, A1, A2).
    shiny::observe({
      t <- input$dsgType %||% "crd"
      # Un dispositif de malherbologie porte ses propres modalites (doses,
      # densites, strategies) : le generateur automatique les ecraserait par
      # « A0, A1, A2 » des le premier passage.
      if (!is.null(hstat_malherbo_catalog()[[t]])) return()
      nf <- if (t == "splitsplit") 3 else if (t %in% c("split", "strip")) 2 else if (t == "factorial") max(2, input$dsgNFactors %||% 2) else 1
      for (i in seq_len(nf)) {
        raw_letter <- input[[paste0("dsgFLetter", i)]]
        raw_start  <- input[[paste0("dsgFStart", i)]]
        raw_end    <- input[[paste0("dsgFEnd", i)]]
        # Les controles sont crees dynamiquement (renderUI) : au premier passage
        # de cet observateur ils peuvent encore etre NULL / integer(0). On saute
        # alors ce facteur pour eviter un if() sur une valeur de longueur 0.
        if (is.null(raw_start) || is.null(raw_end) ||
            length(raw_start) == 0 || length(raw_end) == 0) next
        letter <- toupper(trimws(raw_letter %||% ""))
        start  <- suppressWarnings(as.integer(raw_start))
        end    <- suppressWarnings(as.integer(raw_end))
        if (length(start) == 0 || length(end) == 0 || is.na(start) || is.na(end)) next
        # Bornes coherentes : debut >= 0 et fin >= debut.
        start <- max(0L, start)
        if (end < start) end <- start
        seq_vals <- seq.int(start, end)
        mods <- if (nzchar(letter)) paste0(letter, seq_vals) else as.character(seq_vals)
        shiny::updateTextInput(session, paste0("dsgFLevels", i),
                        value = paste(mods, collapse = ", "))
      }
    })

    get_factors <- shiny::reactive({
      t <- input$dsgType %||% "crd"
      mh <- hstat_malherbo_catalog()[[t]]
      nf <- if (!is.null(mh)) length(mh$facteurs)
            else if (t == "splitsplit") 3 else if (t %in% c("split", "strip")) 2 else if (t == "factorial") max(2, input$dsgNFactors %||% 2) else 1
      fl <- list()
      for (i in seq_len(nf)) {
        nm <- input[[paste0("dsgFName", i)]] %||% paste0("Facteur", i)
        lvl <- trimws(strsplit(input[[paste0("dsgFLevels", i)]] %||% "", ",")[[1]])
        lvl <- lvl[nzchar(lvl)]
        if (length(lvl) >= 1) fl[[nm]] <- lvl
      }
      fl
    })

    design_book <- shiny::eventReactive(input$dsgGenerate, {
      fl <- get_factors(); t <- input$dsgType %||% "crd"
      shiny::validate(shiny::need(length(fl) >= 1, "Definissez au moins un facteur avec ses modalités."))
      if (t %in% c("split", "strip")) shiny::validate(shiny::need(length(fl) >= 2, "Ce plan nécessite 2 facteurs."))
      if (t == "splitsplit") shiny::validate(shiny::need(length(fl) >= 3, "Le split-split-plot nécessite 3 facteurs (parcelle principale, sous-parcelle, sous-sous-parcelle)."))
      kk <- input$dsgK
      k_mode <- input$dsgKMode %||% "exact"
      # Alpha Lattice : le dispositif reussit desormais dans TOUS les contextes.
      # - Mode "exact" : la taille de bloc k saisie est respectee telle quelle (le
      #   dernier bloc peut etre plus petit si le nombre de traitements n'est pas
      #   divisible par k).
      # - Mode "auto" : si un k "parfait" existe (k divise nt, s = nt/k >= k), on
      #   ajuste vers le plus proche pour obtenir un plan equilibre ; sinon plan
      #   generalise. Aucune erreur bloquante n'est possible dans les deux modes.
      if (t == "alpha") {
        nt <- length(fl[[1]])
        valid_k <- Filter(function(d) d >= 2 && d < nt && nt %% d == 0 && (nt / d) >= d, 2:(nt-1))
        if (k_mode == "exact") {
          reste <- if (!is.null(kk) && kk >= 2) nt %% kk else 0
          if (reste != 0)
            shiny::showNotification(trf(
              "Alpha Lattice (k exact) : %d traitements en blocs de %d -> le dernier bloc de chaque réplique contient %d traitement(s).",
              nt, kk, reste), type = "message", duration = 7)
        } else {
          # mode auto
          if (length(valid_k) == 0) {
            shiny::showNotification(trf(
              "Alpha Lattice : aucun k parfait pour %d traitements -> plan en blocs incomplets généralisé.", nt),
              type = "message", duration = 7)
          } else if (is.null(kk) || !(kk %in% valid_k)) {
            new_k <- valid_k[which.min(abs(valid_k - (kk %||% valid_k[1])))]
            shiny::showNotification(trf("Alpha Lattice : k=%s ajusté automatiquement à k=%d pour %d traitements (valeurs conseillées : %s).",
                                     as.character(kk %||% "?"), new_k, nt, paste(valid_k, collapse=", ")),
                             type = "warning", duration = 7)
            kk <- new_k
          }
        }
      }
      b <- tryCatch(hstat_agri_design(type = t, factors = fl, r = input$dsgRep %||% 3,
          seed = input$dsgSeed %||% 123, k = kk, base_design = input$dsgBase %||% "rcbd",
          k_mode = k_mode),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, hstat_err_fr(e)))
        })
      # Message si alpha-lattice generalise (blocs incomplets inegaux)
      if (!is.null(attr(b, "alpha_generalized")))
        shiny::showNotification(attr(b, "alpha_generalized"), type = "message", duration = 8)
      b$n_echantillon <- input$dsgN %||% 1
      attr(b, "used_k") <- kk
      b
    })

    output$dsgAnalysisInfo <- shiny::renderUI({
      fl <- get_factors(); info <- hstat_design_analysis(input$dsgType %||% "crd", length(fl))
      alloc <- hstat_sample_allocation(fl, input$dsgRep %||% 3)
      nplot <- input$dsgN %||% 1
      shiny::div(shiny::h4(info$nom, style = "color:#2c3e50;margin-top:0;"),
        shiny::tags$p(shiny::tags$b("Modèle suggere : "), shiny::tags$code(info$modele)),
        shiny::div(class = "callout callout-info", shiny::icon("lightbulb"), " ", info$analyse),
        shiny::tags$p(shiny::tags$b("Répartition : "),
               trf("%d traitement(s) x %d répétition(s) x %d échantillon(s)/parcelle = %d unités d'observation.",
                       alloc$Valeur[1], alloc$Valeur[2], nplot, alloc$Valeur[4] * nplot)))
    })

    output$dsgTable <- DT::renderDT({
      b <- design_book()
      DT::datatable(b, rownames = FALSE, filter = "top", extensions = "Buttons",
        options = list(pageLength = 12, scrollX = TRUE, dom = "Bfrtip", buttons = .hstat_dt_buttons("plan_expérience")),
        caption = htmltools::tags$caption(style = "caption-side:top;font-weight:600;",
          trf("Field book : %d unités expérimentales", nrow(b))))
    })

    output$dsgTreatSummary <- DT::renderDT({
      b <- design_book(); nplot <- input$dsgN %||% 1
      tcol <- if ("Traitement" %in% names(b)) "Traitement" else names(get_factors())[1]
      tab <- as.data.frame(table(b[[tcol]]), stringsAsFactors = FALSE)
      names(tab) <- c("Modalité / combinaison", "Parcelles (répétitions)")
      tab$`Échantillons par parcelle` <- nplot
      tab$`Taille totale (n)` <- tab$`Parcelles (répétitions)` * nplot
      DT::datatable(tab, rownames = FALSE,
        options = list(pageLength = 12, dom = "tp"),
        caption = htmltools::tags$caption(style = "caption-side:top;font-weight:600;",
          trf("Taille par modalité (total = %d observations)", sum(tab$`Taille totale (n)`))))
    })

    output$dsgDownload <- shiny::downloadHandler(
      filename = function() paste0("plan_", input$dsgType, "_", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(design_book(), file, row.names = FALSE, fileEncoding = "UTF-8"))

    # Construction du graphique du dispositif (partagee affichage + export PNG)
    # Ajoute les contraintes de terrain (obstacle + bandes de blocage) a un
    # graphique de dispositif, qu'il soit facette ou non. Utilise des coordonnees
    # -Inf/Inf pour couvrir tout le panneau (ou chaque facette).
    .add_terrain_constraints <- function(g) {
      if (is.null(g)) return(g)
      # Bandes (facteur de blocage supplementaire)
      if (isTRUE(input$dsgConstraintFactor)) {
        nb   <- max(2, as.integer(input$dsgConstraintBands %||% 2))
        cdir <- input$dsgConstraintDir %||% "vertical"
        cname <- input$dsgConstraintName %||% "Bande"
        band_cols <- grDevices::adjustcolor(
          grDevices::rainbow(nb, s = 0.35, v = 1), alpha.f = 0.16)
        # On recupere l'etendue numerique du panneau a partir du build ggplot.
        rng <- tryCatch({
          bld <- ggplot2::ggplot_build(g)
          list(x = bld$layout$panel_params[[1]]$x.range,
               y = bld$layout$panel_params[[1]]$y.range)
        }, error = function(e) NULL)
        if (!is.null(rng) && all(is.finite(c(rng$x, rng$y)))) {
          if (identical(cdir, "vertical")) {
            edges <- seq(rng$x[1], rng$x[2], length.out = nb + 1)
            for (i in seq_len(nb)) {
              g <- g + ggplot2::annotate("rect", xmin = edges[i], xmax = edges[i + 1],
                         ymin = -Inf, ymax = Inf, fill = band_cols[i], color = NA)
            }
            if (nb > 1)
              g <- g + ggplot2::geom_vline(xintercept = edges[2:nb], color = "#34495e",
                         linewidth = 0.6, linetype = "dashed")
            lab_df <- data.frame(
              .lx = (edges[-length(edges)] + edges[-1]) / 2,
              .ly = rng$y[2],
              .ltxt = paste0(cname, " ", seq_len(nb)),
              stringsAsFactors = FALSE)
            g <- g + ggplot2::geom_text(data = lab_df,
                       ggplot2::aes(x = .lx, y = .ly, label = .ltxt),
                       inherit.aes = FALSE, color = "#34495e", vjust = -0.3,
                       size = (input$dsgGradSize %||% 3.5) * 0.75, fontface = "bold")
          } else {
            edges <- seq(rng$y[1], rng$y[2], length.out = nb + 1)
            for (i in seq_len(nb)) {
              g <- g + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                         ymin = edges[i], ymax = edges[i + 1], fill = band_cols[i], color = NA)
            }
            if (nb > 1)
              g <- g + ggplot2::geom_hline(yintercept = edges[2:nb], color = "#34495e",
                         linewidth = 0.6, linetype = "dashed")
            lab_df <- data.frame(
              .lx = rng$x[2],
              .ly = (edges[-length(edges)] + edges[-1]) / 2,
              .ltxt = paste0(cname, " ", seq_len(nb)),
              stringsAsFactors = FALSE)
            g <- g + ggplot2::geom_text(data = lab_df,
                       ggplot2::aes(x = .lx, y = .ly, label = .ltxt),
                       inherit.aes = FALSE, angle = -90, color = "#34495e", hjust = 1.05,
                       size = (input$dsgGradSize %||% 3.5) * 0.75, fontface = "bold")
          }
        }
      }
      # Obstacle (zone a eviter) : bande sur un bord, couvrant tout le panneau.
      if (isTRUE(input$dsgObstacle)) {
        side  <- input$dsgObstacleSide %||% "left"
        ocol  <- input$dsgObstacleColor %||% "#e74c3c"
        olab  <- input$dsgObstacleLabel %||% "Zone à éviter"
        rng <- tryCatch({
          bld <- ggplot2::ggplot_build(g)
          list(x = bld$layout$panel_params[[1]]$x.range,
               y = bld$layout$panel_params[[1]]$y.range)
        }, error = function(e) NULL)
        if (!is.null(rng) && all(is.finite(c(rng$x, rng$y)))) {
          span_frac <- 0.12 * max(1, as.integer(input$dsgObstacleSpan %||% 1))
          xw <- diff(rng$x); yw <- diff(rng$y)
          rect <- switch(side,
            "left"   = list(xmin = rng$x[1], xmax = rng$x[1] + span_frac * xw,
                            ymin = -Inf, ymax = Inf,
                            lx = rng$x[1] + span_frac * xw / 2, ly = mean(rng$y), ang = 90),
            "right"  = list(xmin = rng$x[2] - span_frac * xw, xmax = rng$x[2],
                            ymin = -Inf, ymax = Inf,
                            lx = rng$x[2] - span_frac * xw / 2, ly = mean(rng$y), ang = 90),
            "top"    = list(xmin = -Inf, xmax = Inf,
                            ymin = rng$y[2] - span_frac * yw, ymax = rng$y[2],
                            lx = mean(rng$x), ly = rng$y[2] - span_frac * yw / 2, ang = 0),
            "bottom" = list(xmin = -Inf, xmax = Inf,
                            ymin = rng$y[1], ymax = rng$y[1] + span_frac * yw,
                            lx = mean(rng$x), ly = rng$y[1] + span_frac * yw / 2, ang = 0))
          g <- g +
            ggplot2::annotate("rect", xmin = rect$xmin, xmax = rect$xmax,
                              ymin = rect$ymin, ymax = rect$ymax,
                              fill = ocol, alpha = 0.30, color = ocol, linewidth = 1.1) +
            ggplot2::annotate("label", x = rect$lx, y = rect$ly, label = olab,
                              angle = rect$ang, color = "white", fill = ocol,
                              fontface = "bold", size = (input$dsgGradSize %||% 3.5) * 0.9,
                              label.size = 0)
        }
      }
      if (isTRUE(input$dsgObstacle) || isTRUE(input$dsgConstraintFactor))
        g <- g + ggplot2::coord_cartesian(clip = "off")
      g
    }

    build_design_plot <- shiny::reactive({
      b <- design_book(); t <- attr(b, "design") %||% input$dsgType
      if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
      # Applique les tailles de police de la legende choisies par l'utilisateur
      # (modalites via legend.text, titre via legend.title). Utilise sur les deux
      # chemins de rendu (facette et non facette) pour un comportement homogene.
      .apply_legend_fonts <- function(g) {
        if (is.null(g) || !inherits(g, "ggplot")) return(g)
        # Les compositions patchwork (pile de legendes multi-facteurs) gerent
        # deja les polices de legende en interne : ne pas modifier leur dernier
        # panneau par accident.
        if (inherits(g, "patchwork")) return(g)
        txt_sz  <- suppressWarnings(as.numeric(input$dsgLegendTextSize  %||% 11))
        ttl_sz  <- suppressWarnings(as.numeric(input$dsgLegendTitleSize %||% 12))
        if (is.na(txt_sz)) txt_sz <- 11
        if (is.na(ttl_sz)) ttl_sz <- 12
        txt_face <- if (isTRUE(input$dsgLegendTextBold))  "bold" else "plain"
        ttl_face <- if (isTRUE(input$dsgLegendTitleBold)) "bold" else "plain"
        g + ggplot2::theme(
          legend.text  = ggplot2::element_text(size = txt_sz, face = txt_face),
          legend.title = ggplot2::element_text(size = ttl_sz, face = ttl_face))
      }
      fill_col <- if ("Traitement" %in% names(b)) "Traitement" else names(get_factors())[1]
      nplot <- input$dsgN %||% 1

      # Placement spatial selon le dispositif (CRD : minimise voisins identiques)
      b <- hstat_place_design(b, t, fill_col, seed = input$dsgSeed %||% 123,
                              tries = input$dsgRandTries %||% 200)
      b$.fill <- as.character(b[[fill_col]])
      mode <- input$dsgLabelMode %||% "both"
      b$.lab <- switch(mode,
        "both" = if (nplot > 1) sprintf("%s\n(n=%d)", b$.fill, nplot) else b$.fill,
        "treat" = b$.fill,
        "n"    = sprintf("n=%d", nplot),
        "none" = "")
      xlab <- if (nzchar(input$dsgXlab %||% "")) input$dsgXlab else attr(b, "xlab") %||% "Colonne"
      ylab <- if (nzchar(input$dsgYlab %||% "")) input$dsgYlab else attr(b, "ylab") %||% "Rangee"
      ttl  <- if (nzchar(input$dsgPlotTitle %||% "")) input$dsgPlotTitle
              else sprintf("Plan %s - randomisation (graine %s) - n=%d/parcelle",
                           toupper(t), input$dsgSeed %||% 123, nplot)

      nlev <- length(unique(b$.fill))
      pal  <- input$dsgPalette %||% "default"
      fill_scale <- if (pal == "default") {
        ggplot2::scale_fill_hue()
      } else if (pal == "grey") {
        ggplot2::scale_fill_grey(start = 0.3, end = 0.9)
      } else if (pal == "viridis") {
        ggplot2::scale_fill_viridis_d()
      } else {
        maxc <- switch(pal, "Set2" = 8, "Dark2" = 8, "Paired" = 12, "Spectral" = 11, 8)
        cols <- grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(min(maxc, max(3, nlev)), pal))(nlev)
        ggplot2::scale_fill_manual(values = cols)
      }

      # ---- Rendu FACETTE pour split-plot, strip-plot et alpha (un panneau par
      # bloc/replique), conforme aux representations agronomiques de reference ----
      if (t %in% c("split", "strip", "alpha", "splitsplit")) {
        # Orientation de l'empilement des blocs incomplets (meme logique que RCBD) :
        #  - "Orientation des traitements" = VERTICALE  -> blocs cote a cote (horizontal)
        #  - "Orientation des traitements" = HORIZONTALE -> blocs empiles (vertical)
        #  - "Automatique" -> deduit du "Sens des blocs".
        to_f <- input$dsgTreatOrient %||% "auto"
        orient_f <- input$dsgBlockOrient %||% "top_down"
        blocks_axis_f <- if (to_f == "vertical") "horizontal"
                         else if (to_f == "horizontal") "vertical"
                         else if (orient_f %in% c("left_right", "right_left")) "horizontal"
                         else "vertical"
        ug_f <- input$dsgGradient %||% "auto"
        grad_sens_f <- if (ug_f %in% c("vertical_up", "horizontal_left")) "inverse" else "direct"
        gf <- .build_faceted_design(b, t, fill_col, fill_scale, ttl,
                                    cell_sep = input$dsgCellSep %||% 1,
                                    blk_line = input$dsgBlockLine %||% 2,
                                    blk_color = input$dsgBlockColor %||% "black",
                                    show_text = isTRUE(input$dsgShowText),
                                    label_mode = mode, nplot = nplot,
                                    font_label = input$dsgFontLabel %||% 2.8,
                                    font_axis = input$dsgFontAxis %||% 12,
                                    theme_gg = hstat_export_theme(
                                      input, "dsgPl", base_size = input$dsgFontAxis %||% 12),
                                    grad = { if (ug_f == "none") "none" else "on" },
                                    grad_label = input$dsgGradLabel %||% "Gradient",
                                    grad_size = input$dsgGradSize %||% 3.5,
                                    legend_order = input$dsgLegendOrder %||% "alpha",
                                    legend_pos = input$dsgLegendPos %||% "right",
                                    legend_title = input$dsgLegendTitle %||% "",
                                    blk_prefix = input$dsgBlockPrefix %||% "Bloc",
                                    blk_custom = input$dsgBlockNames %||% "",
                                    grad2 = input$dsgGradient2 %||% "auto",
                                    show_f2_legend = isTRUE(input$dsgShowF2Legend),
                                    bold = isTRUE(input$dsgBoldLabels),
                                    treat_orient = input$dsgTreatOrient %||% "auto",
                                    blocks_axis = blocks_axis_f,
                                    grad_sens = grad_sens_f,
                                    sub_line = input$dsgSubLine %||% 0.8,
                                    sub_color = input$dsgSubColor %||% "black",
                                    band_fill = input$dsgBandColor %||% "#d6eaf8",
                                    leg_txt_size = input$dsgLegendTextSize %||% 11,
                                    leg_ttl_size = input$dsgLegendTitleSize %||% 12,
                                    leg_txt_bold = isTRUE(input$dsgLegendTextBold),
                                    leg_ttl_bold = isTRUE(input$dsgLegendTitleBold))
        if (!is.null(gf)) return(.apply_legend_fonts(.add_terrain_constraints(gf)))
      }

      # Ordre / position / titre de la legende (choisis par l'utilisateur)
      leg_order <- input$dsgLegendOrder %||% "alpha"
      lv <- unique(as.character(b$.fill))
      lv <- switch(leg_order,
        "alpha" = .natural_sort(lv),
        "rev" = rev(.natural_sort(lv)),
        "appear" = lv, .natural_sort(lv))
      b$.fill <- factor(b$.fill, levels = lv)
      leg_pos <- input$dsgLegendPos %||% "right"
      leg_title <- if (nzchar(input$dsgLegendTitle %||% "")) input$dsgLegendTitle else "Traitement"

      # Affichage en FACETTES (un panneau par bloc, nom du bloc sur la bande) pour
      # les dispositifs en blocs/rangees, si l'utilisateur l'a active.
      paired_facet <- (t == "paired" && (input$dsgPairedLayout %||% "diagonal") != "diagonal")
      if (isTRUE(input$dsgFacetBlocks) && (t %in% c("fisher", "factorial") || paired_facet) &&
          "block" %in% names(b)) {
        # Pour le couple apparie en facettes : les 2 membres du couple sont places
        # via .x = rang du membre (1 ou 2). C'est ensuite 'treat_vertical' (calcule
        # plus bas selon "Disposition des couples") qui decide si .x est rendu en
        # colonnes (cote a cote) ou en rangees (empile).
        if (paired_facet) {
          b$.x <- stats::ave(seq_len(nrow(b)), as.integer(as.factor(b$block)), FUN = seq_along)
        }
        labs_map <- .block_labeller(b$block, input$dsgBlockPrefix %||% "Bloc",
                                    input$dsgBlockNames %||% "", input$dsgBlockStart %||% "1")
        b$.blocklab <- labs_map[as.character(b$block)]
        # --- Deux reglages INDEPENDANTS ---
        # 1) Le GRADIENT pilote la disposition des blocs (empiles vs cote a cote)
        #    et leur sens de numerotation.
        # 2) L'ORIENTATION DES TRAITEMENTS pilote, separement, le sens des
        #    parcelles a l'intérieur de chaque bloc (rangee vs colonne).
        # --- Reglages de disposition ---
        ug <- input$dsgGradient %||% "auto"      # ne fixe plus que le SENS
        # Sens generique : "vertical_up" (= inverse) sinon direct.
        sens_inverse <- ug %in% c("vertical_up", "horizontal_left")

        orient <- input$dsgBlockOrient %||% "top_down"
        to <- input$dsgTreatOrient %||% "auto"
        paired_dir <- input$dsgPairedLayout %||% "diagonal"

        # (1) Disposition des blocs :
        #  - "Orientation des traitements" = VERTICALE  -> blocs cote a cote.
        #  - "Orientation des traitements" = HORIZONTALE -> blocs empiles.
        #  - "Automatique" -> deduit du "Sens des blocs" (gauche/droite = cote a
        #    cote ; haut/bas = empiles).
        if (paired_facet) {
          # Couple apparie : "Disposition des couples" pilote directement
          # l'orientation des 2 membres dans la facette.
          #  - vertical   -> membres empiles (2 rangees)  -> treat_vertical = TRUE
          #  - horizontal -> membres cote a cote (2 col.) -> treat_vertical = FALSE
          treat_vertical <- identical(paired_dir, "vertical")
          # Les facettes (blocs/couples) sont alignees horizontalement par defaut.
          block_stack <- "horizontal"
        } else if (to == "vertical") {
          block_stack   <- "horizontal"
          treat_vertical <- TRUE
        } else if (to == "horizontal") {
          block_stack   <- "vertical"
          treat_vertical <- FALSE
        } else {
          if (orient %in% c("left_right", "right_left")) {
            block_stack <- "horizontal"
          } else {
            block_stack <- "vertical"
          }
          treat_vertical <- (block_stack == "horizontal")
        }

        # (2) Le GRADIENT est TOUJOURS perpendiculaire aux blocs :
        #  - blocs cote a cote (block_stack horizontal) -> gradient HORIZONTAL ;
        #  - blocs empiles      (block_stack vertical)   -> gradient VERTICAL.
        # Le menu ne fixe que le sens (direct / inverse) le long de cet axe impose.
        if (ug == "none") {
          grad_v <- "none"
        } else if (block_stack == "horizontal") {
          grad_v <- if (sens_inverse) "horizontal_left" else "horizontal_right"
        } else {
          grad_v <- if (sens_inverse) "vertical_up" else "vertical_down"
        }

        # Sens de numerotation / d'empilement des blocs : suit le sens du gradient.
        bord <- as.integer(as.factor(b$block))
        reverse_order <- if (grad_v %in% c("vertical_up", "horizontal_left")) TRUE
                         else if (grad_v %in% c("vertical_down", "horizontal_right")) FALSE
                         else orient %in% c("bottom_up", "right_left")
        if (isTRUE(reverse_order)) bord <- max(bord) - bord + 1L
        b$.blockord <- bord

        gbf <- .build_block_faceted(b, fill_scale, ttl,
                  cell_sep = input$dsgCellSep %||% 1,
                  show_text = isTRUE(input$dsgShowText), label_mode = mode, nplot = nplot,
                  font_label = input$dsgFontLabel %||% 2.8, font_axis = input$dsgFontAxis %||% 12,
                  theme_gg = hstat_export_theme(
                    input, "dsgPl", base_size = input$dsgFontAxis %||% 12),
                  legend_order = leg_order, legend_pos = leg_pos,
                  legend_title = input$dsgLegendTitle %||% "",
                  grad = grad_v, grad_label = input$dsgGradLabel %||% "Gradient",
                  grad_size = input$dsgGradSize %||% 3.5,
                  xlab = if (paired_facet) "Couple" else "Parcelle (traitement randomise dans le bloc)",
                  bold = isTRUE(input$dsgBoldLabels),
                  block_stack = block_stack, treat_vertical = treat_vertical)
        if (!is.null(gbf)) return(.apply_legend_fonts(.add_terrain_constraints(gbf)))
      }

      # Orientation des traitements dans le plan principal (non facette) : si
      # l'utilisateur force "verticale", on transpose lignes <-> colonnes.
      to_main <- input$dsgTreatOrient %||% "auto"
      if (to_main %in% c("horizontal", "vertical")) {
        cur_horizontal <- length(unique(b$.x)) >= length(unique(b$.y))
        want_horizontal <- (to_main == "horizontal")
        if (cur_horizontal != want_horizontal) {
          tmp <- b$.x; b$.x <- b$.y; b$.y <- tmp
          tmpl <- xlab; xlab <- ylab; ylab <- tmpl
        }
      }

      g <- ggplot2::ggplot(b, ggplot2::aes(factor(.x), factor(.y), fill = .fill)) +
        ggplot2::geom_tile(color = "white", linewidth = (input$dsgCellSep %||% 1)) +
        ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = 0.02)) +
        ggplot2::scale_y_discrete(limits = rev, expand = ggplot2::expansion(mult = 0.02)) +
        fill_scale +
        ggplot2::labs(x = xlab, y = ylab, fill = leg_title, title = ttl) +
        ggplot2::theme_minimal(base_size = input$dsgFontAxis %||% 12) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = leg_pos)

      # Separateurs de blocs (dispositifs structures en blocs / lignes).
      # RCBD est en diagonale -> pas de lignes horizontales. Factoriel par blocs -> oui.
      # Separateurs de blocs (dispositifs en blocs/rangees). Paired = diagonal -> non.
      block_designs <- c("fisher", "alpha", "split", "strip", "factorial")
      blk_line <- input$dsgBlockLine %||% 2
      if (t %in% block_designs && blk_line > 0) {
        nblocks <- length(unique(b$.y))
        if (nblocks > 1) {
          # frontieres entre blocs adjacents (espace discret : k + 0.5).
          # On borne le trait a l'etendue reelle des parcelles (0.5 -> max(.x)+0.5)
          # au lieu d'un geom_hline pleine largeur qui deborderait a gauche/droite.
          x_min <- 0.5
          x_max <- max(b$.x) + 0.5
          g <- g + ggplot2::geom_segment(
            data = data.frame(yy = seq_len(nblocks - 1) + 0.5),
            ggplot2::aes(x = x_min, xend = x_max, y = yy, yend = yy),
            inherit.aes = FALSE,
            color = input$dsgBlockColor %||% "black",
            linewidth = blk_line)
        }
      }
      if (isTRUE(input$dsgShowText) && mode != "none")
        g <- g + ggplot2::geom_text(ggplot2::aes(label = .lab),
                                    size = input$dsgFontLabel %||% 2.8,
                                    color = "black", lineheight = 0.85,
                                    fontface = if (isTRUE(input$dsgBoldLabels)) "bold" else "plain")

      # Fleche d'orientation du gradient d'heterogeneite.
      # REGLE : le gradient est TOUJOURS perpendiculaire aux blocs.
      # Dans ce rendu non facette, les blocs sont empiles le long de l'axe Y
      # (separateurs horizontaux) -> l'axe du gradient est donc VERTICAL ; si la
      # structure ne comporte pas de blocs en rangees mais en colonnes, on bascule
      # en HORIZONTAL. Le menu "Orientation du gradient" ne fixe plus que le sens.
      grad_struct <- attr(b, "gradient") %||% "none"
      user_grad <- input$dsgGradient %||% "auto"
      nx <- length(unique(b$.x)); ny <- length(unique(b$.y))
      glab <- input$dsgGradLabel %||% "Gradient"
      gsz  <- input$dsgGradSize %||% 3.5
      gcol_v <- "#c0392b"; gcol_h <- "#2980b9"

      # Axe des blocs dans ce plan : par defaut empiles sur Y (rangees de blocs).
      # On considere "blocs en colonnes" si la structure est explicitement reperee
      # comme telle apres transposition (plus de colonnes que de rangees ET
      # dispositif en blocs).
      block_designs_perp <- c("fisher", "alpha", "split", "strip", "factorial")
      blocks_on_y <- TRUE
      if (t %in% block_designs_perp) {
        # Si l'utilisateur a force des traitements "horizontaux" alors que les
        # blocs seraient en colonnes, l'axe d'empilement peut devenir X.
        blocks_on_y <- ny >= nx
      }
      # Gradient perpendiculaire : blocs sur Y -> gradient vertical ;
      #                            blocs sur X -> gradient horizontal.
      grad_axis_perp <- if (blocks_on_y) "vertical" else "horizontal"

      if (user_grad == "none" || identical(grad_struct, "none")) {
        grad <- "none"
      } else if (grad_struct %in% c("both", "diagonal") && user_grad == "auto") {
        # Plans a 2 gradients (carre latin) ou diagonale assumee : on conserve.
        grad <- grad_struct
      } else {
        # Sens generique : "vertical_up" (= inverse) sinon direct.
        sens_inverse <- user_grad %in% c("vertical_up", "horizontal_left")
        if (grad_axis_perp == "vertical") {
          grad <- if (sens_inverse) "vertical_up" else "vertical_down"
        } else {
          grad <- if (sens_inverse) "horizontal_left" else "horizontal_right"
        }
      }

      draw_varrow <- function(g, down = TRUE) {
        # axe vertical place juste a gauche des cases ; down = haut->bas
        y0 <- ny + 0.5; y1 <- 0.5
        if (!down) { tmp <- y0; y0 <- y1; y1 <- tmp }
        g + ggplot2::annotate("segment", x = 0.25, xend = 0.25, y = y0, yend = y1,
              arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), ends = "last", type = "closed"),
              color = gcol_v, linewidth = 1.1) +
          ggplot2::annotate("text", x = 0.08, y = (ny + 1) / 2,
              label = glab, angle = 90, color = gcol_v, size = gsz, fontface = "bold")
      }
      draw_harrow <- function(g, right = TRUE) {
        # axe horizontal place en bas ; right = gauche->droite
        x0 <- 0.5; x1 <- nx + 0.5
        if (!right) { tmp <- x0; x0 <- x1; x1 <- tmp }
        g + ggplot2::annotate("segment", x = x0, xend = x1, y = -0.35, yend = -0.35,
              arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), ends = "last", type = "closed"),
              color = gcol_h, linewidth = 1.1) +
          ggplot2::annotate("text", x = (nx + 1) / 2, y = -0.78,
              label = glab, color = gcol_h, size = gsz, fontface = "bold")
      }
      if (grad == "vertical_down") g <- draw_varrow(g, down = TRUE)
      else if (grad == "vertical_up") g <- draw_varrow(g, down = FALSE)
      else if (grad == "horizontal_right") g <- draw_harrow(g, right = TRUE)
      else if (grad == "horizontal_left") g <- draw_harrow(g, right = FALSE)
      else if (grad == "diagonal") {
        g <- g + ggplot2::annotate("segment", x = 0.3, xend = nx + 0.7, y = ny + 0.7, yend = 0.3,
              arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), ends = "last", type = "closed"),
              color = gcol_v, linewidth = 1.1) +
          ggplot2::annotate("text", x = (nx + 1) / 2 + 0.7, y = (ny + 1) / 2 + 0.7,
              label = glab, color = gcol_v, size = gsz, fontface = "bold", angle = -35)
      } else if (grad == "both") {
        g <- draw_varrow(g, down = TRUE); g <- draw_harrow(g, right = TRUE)
      }
      if (grad != "none") {
        g <- g + ggplot2::coord_cartesian(clip = "off") +
             ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 26, 10))
      }

      # Contraintes de terrain (obstacle + bandes) via l'aide commune.
      g <- .add_terrain_constraints(g)

      # Tailles de police de la legende (modalites + titre), reglables par l'utilisateur.
      g <- .apply_legend_fonts(g)

      g
    })

    output$dsgPlot <- shiny::renderPlot({
      g <- build_design_plot()
      if (is.null(g)) { graphics::plot.new(); graphics::text(.5, .5, "ggplot2 requis"); return(invisible()) }
      g
    })

    # L'export passe par le kit partage. Le plan avait sa taille FIGEE a
    # 11 x 7 pouces : un dispositif a vingt blocs y ecrasait ses etiquettes,
    # sans que rien ne permette de l'agrandir.
    output$dsgPlDl <- hstat_export_plot_handler(input, "dsgPl",
                        function() build_design_plot(), "dispositif")

    # ================= ENQUETE DE TERRAIN =================
    sv_res <- shiny::eventReactive(input$svCalc, {
      pop <- input$svPop %||% 0
      hstat_survey_size(
        objective = input$svObjective %||% "proportion",
        conf_level = input$svConf %||% 0.95,
        margin = if ((input$svObjective %||% "proportion") == "proportion")
                   input$svMargin %||% 0.05 else input$svMarginM %||% 2,
        p = input$svP %||% 0.5, sd = input$svSd,
        population = if (is.null(pop) || pop <= 0) Inf else pop,
        design_effect = input$svDeff %||% 1,
        response_rate = input$svResp %||% 1,
        n_strata = input$svStrata %||% 1)
    })

    output$svResult <- shiny::renderUI({
      r <- sv_res()
      shiny::validate(shiny::need(is.null(r$err), r$err %||% ""))
      shiny::div(
        shiny::div(style = "font-size:42px;font-weight:700;color:#27ae60;", r$n_final),
        shiny::div(style = "font-size:14px;color:#7f8c8d;",
            "personnes a enqueter (taille finale, non-réponse incluse)"),
        if (!is.na(r$per_stratum))
          shiny::div(class = "callout callout-info", style = "margin-top:10px;",
              shiny::icon("layer-group"),
              sprintf(" Soit environ %d par strate (%d strates).",
                      r$per_stratum, input$svStrata %||% 1))
      )
    })

    output$svTable <- DT::renderDT({
      r <- sv_res(); shiny::req(is.null(r$err))
      tab <- data.frame(
        Etape = c("1. Taille de base (population infinie)",
                  "2. Apres correction population finie",
                  "3. Apres effet de plan (grappes)",
                  "4. Taille finale (ajustee pour non-réponse)"),
        Taille = c(r$n0, r$n_fpc, r$n_deff, r$n_final),
        stringsAsFactors = FALSE)
      DT::datatable(tab, rownames = FALSE, options = list(dom = "t", ordering = FALSE),
        caption = htmltools::tags$caption(style = "caption-side:top;font-weight:600;",
          "Detail du calcul"))
    })

    output$svCurve <- shiny::renderPlot({
      r <- sv_res(); shiny::req(is.null(r$err))
      obj <- input$svObjective %||% "proportion"
      pop <- input$svPop %||% 0; pop <- if (is.null(pop) || pop <= 0) Inf else pop
      ns_seq <- seq(20, max(1500, r$n_final * 1.5), by = 5)
      marg <- vapply(ns_seq, function(nn) hstat_survey_margin(
        nn, input$svConf %||% 0.95, input$svP %||% 0.5, pop, obj, input$svSd),
        numeric(1))
      d <- data.frame(n = ns_seq, marge = marg)
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        plot(d$n, d$marge, type = "l", xlab = "n", ylab = "Marge"); return(invisible())
      }
      target <- if (obj == "proportion") input$svMargin %||% 0.05 else input$svMarginM %||% 2
      ggplot2::ggplot(d, ggplot2::aes(n, marge)) +
        ggplot2::geom_line(color = "#2980b9", linewidth = 1) +
        ggplot2::geom_hline(yintercept = target, linetype = "dashed", color = "#c0392b") +
        ggplot2::geom_vline(xintercept = r$n_final, linetype = "dotted", color = "#27ae60") +
        ggplot2::labs(x = "Taille d'échantillon (n)",
                      y = if (obj == "proportion") "Marge d'erreur" else "Marge d'erreur (unités)",
                      title = "Marge d'erreur en fonction de la taille") +
        ggplot2::theme_minimal(base_size = 13)
    })
  })
}
