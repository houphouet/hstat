# =============================================================================
#  DL50 / CL50 -- ANALYSE PROBIT DOSE-MORTALITE  --  mod_dl50.R
# -----------------------------------------------------------------------------
#  Ce module refait, dans HStat, ce que faisait WIN DL (CIRAD -- Giner & Joly
#  1993 pour la version DOS, MABIS et le Programme Coton pour la version
#  Windows) : ajuster une droite de HENRY entre le probit de la mortalite et le
#  logarithme decimal de la dose, en tenant compte de la mortalite naturelle,
#  puis en deduire les doses letales remarquables et leurs intervalles.
#
#  LE MODELE, ET RIEN D'AUTRE (FINNEY, 1971) :
#
#      P'(d) = c + (1 - c) . F(a + b . log10(d))
#
#  ou F est la fonction de repartition de la loi normale centree reduite, c la
#  mortalite naturelle, a le terme constant et b la pente. Il en decoule
#  immediatement DL50 = 10^(-a/b), et plus generalement
#  DLp = 10^((F^-1(p) - a) / b).
#
#  DEUX ESTIMATIONS, ET ELLES NE REPONDENT PAS A LA MEME QUESTION :
#
#   - ABBOTT (1925) estime c sur le SEUL lot temoin, corrige les mortalites
#     observees, puis ajuste a et b. C'est la methode ancienne : robuste, mais
#     elle traite c comme connue, donc les intervalles sont trop etroits.
#   - EM (DEMPSTER 1977 ; HASSELBLAD 1980) estime c par maximum de
#     vraisemblance sur TOUTES les observations, temoin compris. c devient un
#     parametre a part entiere : les intervalles sur a, b et les doses letales
#     tiennent compte de l'incertitude sur la mortalite naturelle, et le cas
#     ou le temoin meurt davantage que les plus faibles doses cesse d'etre
#     inexploitable.
#
#  EM est le defaut, comme dans WIN DL. Le repli sur ABBOTT se justifie quand
#  le test de divergence entre les deux estimations est significatif ET que la
#  mortalite du temoin est certaine -- c'est le seul cas ou la robustesse
#  d'ABBOTT vaut son biais.
#
#  CONFORMITE NUMERIQUE. Le noyau a ete valide contre un fichier de resultats
#  produit par WIN DL lui-meme (CL94AC1.PRN : 7 doses, 25 insectes par dose,
#  temoin 25/0). Concordance a 5 ou 6 chiffres significatifs sur a, b, c, les
#  deux log-vraisemblances, le Chi-2, les six termes de la matrice de
#  variances, et les DL10/50/90 avec leurs bornes. Le test de non-regression
#  porte ces valeurs.
#
#  Deux conventions du logiciel, qui ne se devinent pas, sont reprises telles
#  quelles et documentees a leur place : la log-vraisemblance SANS les
#  coefficients binomiaux, et la matrice d'information de FISHER assemblee sur
#  les DOSES SEULES.
# =============================================================================

HSTAT_DL50_ITMAX      <- 500L    # iterations EM
HSTAT_DL50_ITMAX_NR   <- 50L     # iterations Newton-Raphson (WIN DL : 50)
HSTAT_DL50_TOL        <- 1e-5    # ecart absolu sur la log-vraisemblance
HSTAT_DL50_DOSES_MAX  <- 100L    # limite de WIN DL
HSTAT_DL50_ESSAIS_MAX <- 6L      # limite de WIN DL
HSTAT_DL50_MIN_UTILES <- 3L      # doses a mortalite corrigee strictement entre 0 et 1

HSTAT_DL50_METHODES <- c(
  "Maximum de vraisemblance sur tout l'essai (EM)"     = "em",
  "Mortalité naturelle du seul témoin (Abbott)"        = "abbott",
  "Mortalité naturelle fixée à zéro"                   = "nulle")

HSTAT_DL50_SCENARIOS <- c(
  "Mortalité naturelle propre à chaque essai (hétérogène, estimée)"   = "heterogene",
  "Mortalité naturelle commune à tous les essais (homogène, estimée)" = "homogene",
  "Mortalité naturelle fixée à 0 % pour tous les essais"              = "nulle")

# Les trois doses letales que WIN DL affiche et trace. L'utilisateur peut en
# demander d'autres, mais celles-ci sont toujours calculees : ce sont elles que
# les rapports de bioessai portent.
HSTAT_DL50_SEUILS <- c(10, 50, 90)

# Les champs d'un essai qui doivent etre STRICTEMENT identiques pour qu'une
# fusion ait un sens : fusionner deux especes ou deux durees d'exposition
# fabriquerait un essai qui n'a jamais eu lieu.
HSTAT_DL50_CHAMPS_FUSION <- c("espece", "stade", "duree", "temperature",
                              "matiere1", "matiere2", "ratio", "methode", "unite")

HSTAT_DL50_CHAMPS <- c(
  date = "Date de l'essai", auteur = "Auteur", espece = "Espèce",
  stade = "Stade", duree = "Durée", temperature = "Température (°C)",
  matiere1 = "Matière active n°1", matiere2 = "Matière active n°2",
  ratio = "Ratio", methode = "Méthode de traitement", unite = "Unité de dose")

# ---------------------------------------------------------------------------
#  Log-vraisemblance : la convention de WIN DL
# ---------------------------------------------------------------------------
#  WIN DL rend -105.63592 la ou R rend -12.55876 sur le meme ajustement.
#  L'ecart est exactement sum(log(choose(n, x))) : le logiciel omet les
#  coefficients binomiaux. Ils ne dependent pas des parametres, donc tout
#  RAPPORT de vraisemblance est identique -- mais la valeur affichee, elle,
#  ne l'est pas. On garde la convention du logiciel pour que les nombres
#  imprimes coincident avec ceux des rapports deja publies.
hstat_dl50_logvrais <- function(x, n, p) {
  if (!length(x)) return(0)
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  sum(x * log(p) + (n - x) * log(1 - p))
}

# Probit centre. WIN DL ecrit les probits centres (F^-1(p)) dans ses fichiers
# recents et les probits decales de 5 dans ceux de la version DOS ; les deux se
# lisent, c'est le meme nombre a 5 pres.
hstat_dl50_probit <- function(p) stats::qnorm(pmin(pmax(p, 1e-12), 1 - 1e-12))

# ---------------------------------------------------------------------------
#  Valeurs initiales : la regression NON PONDEREE du probit corrige
# ---------------------------------------------------------------------------
#  Le refus est ici, et il est volontaire : moins de trois doses donnant une
#  mortalite corrigee strictement comprise entre 0 et 1, et la droite n'est
#  plus determinee. WIN DL refuse de continuer ; nous aussi, en le disant.
.hstat_dl50_init <- function(z, n, x, cc) {
  p <- (x / n - cc) / (1 - cc)
  util <- is.finite(p) & p > 0 & p < 1
  if (sum(util) < HSTAT_DL50_MIN_UTILES) return(NULL)
  y <- hstat_dl50_probit(p[util])
  zz <- z[util]
  if (stats::sd(zz) < 1e-12) return(NULL)
  b <- stats::cov(zz, y) / stats::var(zz)
  if (!is.finite(b) || b == 0) return(NULL)
  list(a = mean(y) - b * mean(zz), b = b, utiles = sum(util))
}

# ---------------------------------------------------------------------------
#  Newton-Raphson avec scoring de FISHER (IRLS), mortalite naturelle FIXEE
# ---------------------------------------------------------------------------
#  C'est l'algorithme de DAVIES repris par LOTODE puis GINER & JOLY, dont la
#  reponderation iterative est celle de GREEN (1984). Les poids sont ceux de
#  FINNEY :
#
#              Z^2                                    P - P1
#      W = -----------------      et      Y2 = Y1 + ------------
#          Q . (P1 + c/(1-c))                            Z
#
#  ou Z est la densite normale au probit attendu Y1, P1 la mortalite attendue,
#  Q = 1 - P1 et P la mortalite corrigee d'ABBOTT.
.hstat_dl50_nr <- function(z, n, x, cc, a0, b0,
                           itmax = HSTAT_DL50_ITMAX_NR, tol = HSTAT_DL50_TOL) {
  a <- a0; b <- b0; ll_prec <- NA_real_; it <- 0L; conv <- FALSE
  for (i in seq_len(itmax)) {
    it <- i
    eta <- a + b * z
    P1 <- stats::pnorm(eta); Z <- stats::dnorm(eta); Q <- 1 - P1
    ll <- hstat_dl50_logvrais(x, n, cc + (1 - cc) * P1)
    if (is.finite(ll_prec) && abs(ll - ll_prec) < tol) { conv <- TRUE; break }
    ll_prec <- ll
    W  <- Z^2 / (Q * (P1 + cc / (1 - cc)))
    WN <- W * n
    P  <- (x / n - cc) / (1 - cc)
    Y2 <- eta + (P - P1) / Z
    ok <- is.finite(WN) & WN > 0 & is.finite(Y2)
    if (sum(ok) < 2L) break
    sw <- sum(WN[ok]); sx <- sum(WN[ok] * z[ok]); sy <- sum(WN[ok] * Y2[ok])
    sxx <- sum(WN[ok] * z[ok]^2); sxy <- sum(WN[ok] * z[ok] * Y2[ok])
    den <- sw * sxx - sx^2
    if (!is.finite(den) || abs(den) < 1e-300) break
    b <- (sw * sxy - sx * sy) / den
    a <- (sy - b * sx) / sw
    if (!is.finite(a) || !is.finite(b)) break
  }
  list(a = a, b = b, iterations = it, converge = conv)
}

# ---------------------------------------------------------------------------
#  EM : la mortalite naturelle devient un parametre
# ---------------------------------------------------------------------------
#  [E] Pour un insecte mort a la dose d, la probabilite qu'il soit mort
#      NATURELLEMENT vaut w = c / (c + (1-c) P). Dans le temoin, w = 1.
#  [M] c est la somme des morts naturels estimes rapportee au total teste ;
#      on retranche ces morts naturels des effectifs, puis on relance
#      Newton-Raphson avec c = 0 sur les effectifs corriges.
#
#  L'interet sur ABBOTT n'est pas theorique : le temoin seul fonde l'estimation
#  de c sur un unique lot, alors que l'information sur la mortalite naturelle
#  est presente dans TOUTES les doses faibles.
.hstat_dl50_em <- function(z, n, x, n0, x0, a0, b0,
                           itmax = HSTAT_DL50_ITMAX, tol = HSTAT_DL50_TOL) {
  a <- a0; b <- b0
  cc <- if (n0 > 0) x0 / n0 else 0
  ll_prec <- NA_real_; it <- 0L; conv <- FALSE
  for (i in seq_len(itmax)) {
    it <- i
    eta <- a + b * z; P1 <- stats::pnorm(eta)
    Pobs <- cc + (1 - cc) * P1
    ll <- hstat_dl50_logvrais(x, n, Pobs) +
          if (n0 > 0) hstat_dl50_logvrais(x0, n0, cc) else 0
    if (is.finite(ll_prec) && abs(ll - ll_prec) < tol) { conv <- TRUE; break }
    ll_prec <- ll
    w <- if (cc <= 0) rep(0, length(z)) else cc / pmax(Pobs, 1e-12)
    m <- w * x
    cc <- (sum(m) + x0) / (sum(n) + n0)
    cc <- min(max(cc, 0), 0.999)
    f <- .hstat_dl50_nr(z, n - m, x - m, 0, a, b, itmax = HSTAT_DL50_ITMAX_NR,
                        tol = tol / 100)
    if (!is.finite(f$a) || !is.finite(f$b)) break
    a <- f$a; b <- f$b
  }
  list(a = a, b = b, c = cc, iterations = it, converge = conv)
}

# ---------------------------------------------------------------------------
#  Matrice d'information de FISHER, TROIS parametres
# ---------------------------------------------------------------------------
#  ASSEMBLEE SUR LES DOSES SEULES, et c'est la seconde convention de WIN DL
#  qu'il fallait retrouver. Le lot temoin apporte pourtant de l'information sur
#  c -- mais sa contribution vaut n0 / (c(1-c)), donc INFINIE des que c vaut 0,
#  ce qui rendrait un ecart-type nul sur la mortalite naturelle. WIN DL affiche
#  ET(c) = 0.4387 sur un essai ou c = 0 exactement : le temoin n'y est pas.
#
#  Reprendre ce choix est ce qui fait coincider les six termes de variance avec
#  ceux du logiciel. C'est aussi le choix prudent : l'incertitude sur c y est
#  plus grande, jamais plus petite.
.hstat_dl50_fisher <- function(z, n, a, b, cc) {
  eta <- a + b * z
  P1 <- stats::pnorm(eta); ph <- stats::dnorm(eta); Q1 <- 1 - P1
  Pobs <- cc + (1 - cc) * P1; Qobs <- 1 - Pobs
  w <- n / pmax(Pobs * Qobs, 1e-300)
  da <- (1 - cc) * ph; db <- da * z; dc <- Q1
  I <- matrix(0, 3, 3, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  I[1, 1] <- sum(w * da * da); I[1, 2] <- sum(w * da * db); I[1, 3] <- sum(w * da * dc)
  I[2, 2] <- sum(w * db * db); I[2, 3] <- sum(w * db * dc); I[3, 3] <- sum(w * dc * dc)
  I[2, 1] <- I[1, 2]; I[3, 1] <- I[1, 3]; I[3, 2] <- I[2, 3]
  I
}

# ---------------------------------------------------------------------------
#  Un essai : la structure d'entree
# ---------------------------------------------------------------------------
hstat_dl50_essai <- function(dose, effectif, morts, temoin_n = 0, temoin_morts = 0,
                             titre = "", champs = list()) {
  d <- data.frame(dose = suppressWarnings(as.numeric(dose)),
                  n = suppressWarnings(as.numeric(effectif)),
                  x = suppressWarnings(as.numeric(morts)),
                  stringsAsFactors = FALSE)
  d <- d[stats::complete.cases(d), , drop = FALSE]
  # WIN DL trie les doses et REGROUPE les doses identiques : deux lignes a la
  # meme dose sont deux repetitions du meme point, les sommer est la seule
  # lecture qui garde juste le nombre d'insectes testes.
  if (nrow(d)) {
    ag <- stats::aggregate(cbind(n, x) ~ dose, data = d, FUN = sum)
    d <- ag[order(ag$dose), , drop = FALSE]
    rownames(d) <- NULL
  }
  n0 <- suppressWarnings(as.numeric(temoin_n)[1])
  x0 <- suppressWarnings(as.numeric(temoin_morts)[1])
  structure(list(
    doses  = d,
    n0     = if (is.finite(n0)) max(0, n0) else 0,
    x0     = if (is.finite(x0)) max(0, x0) else 0,
    titre  = as.character(titre)[1],
    champs = champs), class = "hstat_dl50_essai")
}

.hstat_dl50_valide <- function(essai) {
  d <- essai$doses
  if (!nrow(d))
    return(tr("Aucune dose saisie : il faut au moins trois doses donnant des mortalités intermédiaires."))
  if (nrow(d) > HSTAT_DL50_DOSES_MAX)
    return(trf("Trop de doses (%d) : la limite est de %d, comme dans WIN DL.",
               nrow(d), HSTAT_DL50_DOSES_MAX))
  if (any(!is.finite(d$dose) | d$dose <= 0))
    return(tr("Dose nulle ou négative : l'analyse passe par le logarithme de la dose, le témoin se saisit à part."))
  if (any(!is.finite(d$n) | d$n <= 0))
    return(tr("Effectif testé nul ou absent : renseignez le nombre d'individus exposés à chaque dose."))
  if (any(d$x < 0 | d$x > d$n))
    return(tr("Nombre de morts supérieur à l'effectif testé, ou négatif : vérifiez la saisie."))
  if (essai$x0 > essai$n0)
    return(tr("Le témoin compte plus de morts que d'individus testés : vérifiez la saisie."))
  NULL
}

# ---------------------------------------------------------------------------
#  L'AJUSTEMENT
# ---------------------------------------------------------------------------
hstat_dl50_ajuste <- function(essai, methode = c("em", "abbott", "nulle"),
                              alpha = 0.05) {
  methode <- match.arg(methode)
  echec <- function(motif)
    structure(list(ok = FALSE, message = motif, methode = methode),
              class = "hstat_dl50_fit")
  if (!inherits(essai, "hstat_dl50_essai")) return(echec(tr("Essai non conforme.")))
  msg <- .hstat_dl50_valide(essai)
  if (!is.null(msg)) return(echec(msg))

  d <- essai$doses
  z <- log10(d$dose); n <- d$n; x <- d$x
  n0 <- essai$n0; x0 <- essai$x0
  c_temoin <- if (n0 > 0) x0 / n0 else 0
  c_depart <- if (identical(methode, "nulle")) 0 else c_temoin
  if (c_depart >= 1)
    return(echec(tr("Le témoin est mort en totalité : la mortalité naturelle vaut 100 %, aucune dose ne peut être évaluée.")))

  ini <- .hstat_dl50_init(z, n, x, c_depart)
  if (is.null(ini))
    return(echec(trf("Moins de %d doses donnent une mortalité corrigée strictement comprise entre 0 %% et 100 %% : la droite de régression n'est pas déterminée. Ajoutez des doses intermédiaires.",
                     HSTAT_DL50_MIN_UTILES)))

  fit <- if (identical(methode, "em")) {
    .hstat_dl50_em(z, n, x, n0, x0, ini$a, ini$b)
  } else {
    f <- .hstat_dl50_nr(z, n, x, c_depart, ini$a, ini$b)
    c(f, list(c = c_depart))
  }
  if (!is.finite(fit$a) || !is.finite(fit$b))
    return(echec(tr("L'estimation n'a pas convergé : vérifiez que la mortalité croît avec la dose.")))

  a <- fit$a; b <- fit$b; cc <- fit$c
  eta <- a + b * z
  Pobs <- cc + (1 - cc) * stats::pnorm(eta)

  ll0 <- hstat_dl50_logvrais(x, n, Pobs) +
         if (n0 > 0) hstat_dl50_logvrais(x0, n0, cc) else 0
  ll1 <- hstat_dl50_logvrais(x, n, x / n) +
         if (n0 > 0) hstat_dl50_logvrais(x0, n0, x0 / n0) else 0
  chi2 <- max(0, 2 * (ll1 - ll0))
  # Le degre de liberte est celui de WIN DL : nombre de doses - 2. Le manuel
  # l'ecrit, le fichier de reference le confirme (7 doses, ddl = 5) -- y
  # compris quand c est estimee, ou la theorie voudrait retirer un parametre
  # de plus. On garde la convention du logiciel : c'est elle qui fixe le seuil
  # a partir duquel le facteur d'heterogeneite s'applique.
  ddl <- max(1L, nrow(d) - 2L)
  p_chi2 <- stats::pchisq(chi2, ddl, lower.tail = FALSE)

  V <- tryCatch(solve(.hstat_dl50_fisher(z, n, a, b, cc)), error = function(e) NULL)
  if (is.null(V) || any(!is.finite(V)))
    return(echec(tr("Matrice d'information singulière : les variances des paramètres ne peuvent pas être calculées. Ajoutez des doses ou augmentez les effectifs.")))

  # Heterogeneite : un ajustement rejete a 5 % signale une dispersion que le
  # modele binomial ne contient pas. Les variances sont alors multipliees par
  # Chi2/ddl et le quantile devient celui de STUDENT. Ne pas le faire
  # publierait des intervalles trop etroits precisement quand le modele est
  # douteux.
  hetero <- isTRUE(p_chi2 < alpha)
  facteur <- if (hetero) chi2 / ddl else 1
  Vh <- V * facteur
  tq <- if (hetero) stats::qt(1 - alpha / 2, ddl) else stats::qnorm(1 - alpha / 2)

  p_cor <- (x / n - cc) / (1 - cc)
  table <- data.frame(
    N = seq_len(nrow(d)),
    Dose = d$dose, Effectif = n, Morts = x,
    Log_dose = z,
    Mortalite_observee = x / n,
    Mortalite_corrigee = p_cor,
    Probit_corrige = hstat_dl50_probit(p_cor),
    Mortalite_attendue = Pobs,
    Probit_attendu = eta,
    stringsAsFactors = FALSE)

  structure(list(
    ok = TRUE, methode = methode, essai = essai,
    a = a, b = b, c = cc, c_temoin = c_temoin,
    iterations = fit$iterations, converge = isTRUE(fit$converge),
    ll0 = ll0, ll1 = ll1, chi2 = chi2, ddl = ddl, p_chi2 = p_chi2,
    heterogene = hetero, facteur = facteur, t = tq, alpha = alpha,
    V = V, Vh = Vh, table = table,
    n_doses = nrow(d), n_zero = sum(p_cor <= 0), n_cent = sum(p_cor >= 1)),
    class = "hstat_dl50_fit")
}

# ---------------------------------------------------------------------------
#  Doses letales : FIELLER quand il s'applique, delta-methode sinon
# ---------------------------------------------------------------------------
#  Le theoreme de FIELLER donne l'intervalle exact d'un rapport de deux
#  estimateurs normaux -- ici (F^-1(p) - a) / b. Il ne s'applique que si la
#  pente est significativement non nulle, condition qui s'ecrit
#
#      g = t^2 . var(b) / b^2  <  1
#
#  Au-dela, l'ensemble des valeurs compatibles n'est plus un intervalle borne.
#  C'est le cas de l'essai de reference (g = 1.44), et WIN DL y rend bien des
#  bornes SYMETRIQUES en log-dose, celles de la delta-methode (MORGAN, 1992).
#  Le basculement n'est donc pas un detail d'implementation : il est visible
#  sur les nombres publies.
hstat_dl50_doses_letales <- function(fit, seuils = HSTAT_DL50_SEUILS) {
  if (!isTRUE(fit$ok)) return(NULL)
  V <- fit$Vh; a <- fit$a; b <- fit$b; tq <- fit$t
  vaa <- V[1, 1]; vbb <- V[2, 2]; vab <- V[1, 2]
  g <- tq^2 * vbb / b^2
  out <- lapply(seuils, function(s) {
    y <- stats::qnorm(s / 100)
    m <- (y - a) / b
    g1 <- -1 / b; g2 <- -(y - a) / b^2
    v <- g1^2 * vaa + g2^2 * vbb + 2 * g1 * g2 * vab
    se <- sqrt(max(v, 0))
    # FIELLER, ecrit avec le numerateur N = y - a et le denominateur D = b :
    #   cov(N, D) = -V_ab,  var(N) = V_aa,  var(D) = V_bb
    #   centre = (m + g.V_ab/V_bb) / (1 - g)
    #   demi   = t / (|b|(1-g)) . sqrt( b^2.var(m) - g.(V_aa - V_ab^2/V_bb) )
    # La verification qui compte : quand g tend vers 0, la demi-largeur doit
    # tendre vers t.ET(m), celle de la delta-methode. Une forme approchee qui
    # ne le fait pas rend des bornes qui n'encadrent meme pas l'estimation --
    # c'est le premier symptome, et le seul si l'on ne regarde pas.
    dis <- if (vbb > 0) b^2 * v - g * (vaa - vab^2 / vbb) else NA_real_
    if (is.finite(g) && g < 1 && is.finite(dis) && dis >= 0) {
      centre <- (m + g * vab / vbb) / (1 - g)
      demi <- (tq / (abs(b) * (1 - g))) * sqrt(dis)
      lo <- centre - demi; hi <- centre + demi; meth <- "Fieller"
    } else {
      lo <- m - tq * se; hi <- m + tq * se; meth <- "delta"
    }
    data.frame(Seuil = s, Log_dose = m, Ecart_type = se, Dose = 10^m,
               Limite_inf = 10^lo, Limite_sup = 10^hi,
               Intervalle = meth, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, out)
  res <- res[order(-res$Seuil), , drop = FALSE]
  rownames(res) <- NULL
  attr(res, "g") <- g
  res
}

# Mortalite attendue a une dose donnee, avec son intervalle.
#
# L'intervalle est construit SUR LE PROBIT puis transporte par F : c'est une
# transformation monotone, donc les bornes restent dans [0 ; 1]. Le construire
# directement sur la proportion produirait des bornes hors de [0 ; 1] des que
# la mortalite approche ses extremes -- la ou l'on veut justement lire.
hstat_dl50_mortalite <- function(fit, dose) {
  if (!isTRUE(fit$ok)) return(NULL)
  d <- suppressWarnings(as.numeric(dose))
  d <- d[is.finite(d) & d > 0]
  if (!length(d)) return(NULL)
  V <- fit$Vh; tq <- fit$t; cc <- fit$c
  z <- log10(d)
  eta <- fit$a + fit$b * z
  se <- sqrt(pmax(V[1, 1] + z^2 * V[2, 2] + 2 * z * V[1, 2], 0))
  vers_p <- function(e) cc + (1 - cc) * stats::pnorm(e)
  data.frame(Dose = d, Log_dose = z, Probit_attendu = eta, Ecart_type = se,
             Mortalite = vers_p(eta),
             Limite_inf = vers_p(eta - tq * se),
             Limite_sup = vers_p(eta + tq * se),
             stringsAsFactors = FALSE)
}

# Dose correspondant a une mortalite donnee. La mortalite demandee est celle
# OBSERVEE (temoin compris) : on la ramene d'abord par ABBOTT, sans quoi
# demander 5 % de mortalite sur un essai a 10 % de mortalite naturelle
# renverrait une dose alors qu'aucune ne convient.
hstat_dl50_dose_pour <- function(fit, mortalite) {
  if (!isTRUE(fit$ok)) return(NULL)
  m <- suppressWarnings(as.numeric(mortalite))
  m <- m[is.finite(m)]
  if (!length(m)) return(NULL)
  p <- (m / 100 - fit$c) / (1 - fit$c)
  ok <- p > 0 & p < 1
  if (!any(ok)) return(NULL)
  m <- m[ok]; p <- p[ok]
  # UNE LIGNE A LA FOIS, et surtout pas un appel groupe : `doses_letales()`
  # TRIE ses lignes par seuil decroissant. Y recoller ensuite la mortalite
  # demandee dans l'ordre de saisie decalait toutes les colonnes -- on lisait
  # la dose de la DL25 sur la ligne de la DL95, sans que rien ne le signale.
  res <- do.call(rbind, lapply(seq_along(p), function(i) {
    r <- hstat_dl50_doses_letales(fit, seuils = p[i] * 100)
    r$Mortalite_demandee <- m[i]
    r
  }))
  res <- res[order(res$Mortalite_demandee), , drop = FALSE]
  rownames(res) <- NULL
  res
}

# ---------------------------------------------------------------------------
#  ABBOTT ou EM ? Le test que WIN DL pose sur sa seconde page
# ---------------------------------------------------------------------------
#  Les deux estimations de la mortalite naturelle divergent quand le modele
#  probit s'ajuste mal, ou quand le temoin est trop petit pour porter seul
#  l'estimation. Le test est un rapport de vraisemblance a 1 degre de liberte
#  entre le modele ou c est libre (EM) et celui ou c est FIXEE a la valeur du
#  temoin (ABBOTT).
hstat_dl50_test_abbott_em <- function(essai) {
  em <- hstat_dl50_ajuste(essai, "em")
  ab <- hstat_dl50_ajuste(essai, "abbott")
  if (!isTRUE(em$ok) || !isTRUE(ab$ok))
    return(list(ok = FALSE, message = if (!isTRUE(em$ok)) em$message else ab$message))
  chi2 <- max(0, 2 * (em$ll0 - ab$ll0))
  p <- stats::pchisq(chi2, 1, lower.tail = FALSE)
  v <- hstat_p_verdict(p)
  list(ok = TRUE, c_em = em$c, c_abbott = ab$c, chi2 = chi2, ddl = 1L, p = p,
       verdict = v,
       conseil = switch(v,
         significatif = tr("Les deux estimations de la mortalité naturelle divergent : si la mortalité du témoin est certaine, préférez Abbott pour sa robustesse ; sinon, c'est le modèle probit qui s'ajuste mal aux données."),
         `non significatif` = tr("Les deux estimations de la mortalité naturelle concordent : l'estimation par EM est préférable, elle tient compte de l'incertitude sur la mortalité naturelle."),
         tr("Test non calculable : la statistique est indéterminée.")))
}

# ---------------------------------------------------------------------------
#  COMPARAISON D'ESSAIS : quatre tests, trois scenarios
# ---------------------------------------------------------------------------
#  Un ajusteur general, parametre par ce qui est COMMUN entre essais. Tous les
#  modeles dont les quatre tests ont besoin en decoulent -- ecrire un ajusteur
#  par hypothese ferait huit fois le meme code, et huit occasions de diverger.
#
#  `c_mode` : "libre" (une mortalite naturelle par essai), "commun" (une seule,
#  estimee), "nul" (fixee a zero).
.hstat_dl50_fit_multi <- function(essais, a_commun = FALSE, b_commun = FALSE,
                                  c_mode = c("libre", "commun", "nul")) {
  c_mode <- match.arg(c_mode)
  k <- length(essais)
  z <- lapply(essais, function(e) log10(e$doses$dose))
  n <- lapply(essais, function(e) e$doses$n)
  x <- lapply(essais, function(e) e$doses$x)
  n0 <- vapply(essais, function(e) e$n0, numeric(1))
  x0 <- vapply(essais, function(e) e$x0, numeric(1))

  # Depart : l'ajustement de chaque essai pris isolement. Une valeur initiale
  # arbitraire ferait echouer l'optimisation sur les essais a faible pente.
  dep <- lapply(essais, function(e) {
    f <- hstat_dl50_ajuste(e, if (identical(c_mode, "nul")) "nulle" else "em")
    if (isTRUE(f$ok)) c(f$a, f$b, f$c) else c(0, 1, 0)
  })
  a0 <- vapply(dep, `[`, numeric(1), 1)
  b0 <- vapply(dep, `[`, numeric(1), 2)
  c0 <- vapply(dep, `[`, numeric(1), 3)

  na <- if (a_commun) 1L else k
  nb <- if (b_commun) 1L else k
  nc <- switch(c_mode, libre = k, commun = 1L, nul = 0L)
  par0 <- c(if (a_commun) mean(a0) else a0,
            if (b_commun) mean(b0) else b0,
            if (nc == k) c0 else if (nc == 1L) mean(c0) else numeric(0))

  decoupe <- function(par) {
    av <- par[seq_len(na)]
    bv <- par[na + seq_len(nb)]
    cv <- if (nc > 0) par[na + nb + seq_len(nc)] else 0
    list(a = if (a_commun) rep(av, k) else av,
         b = if (b_commun) rep(bv, k) else bv,
         c = if (nc == 1L) rep(cv, k) else if (nc == 0L) rep(0, k) else cv)
  }
  nll <- function(par) {
    if (any(!is.finite(par))) return(1e100)
    p <- decoupe(par)
    s <- 0
    for (i in seq_len(k)) {
      cc <- min(max(p$c[i], 0), 0.999)
      P <- cc + (1 - cc) * stats::pnorm(p$a[i] + p$b[i] * z[[i]])
      s <- s + hstat_dl50_logvrais(x[[i]], n[[i]], P)
      if (n0[i] > 0) s <- s + hstat_dl50_logvrais(x0[i], n0[i], cc)
    }
    if (!is.finite(s)) 1e100 else -s
  }
  lo <- c(rep(-Inf, na + nb), rep(0, nc))
  hi <- c(rep(Inf, na + nb), rep(0.999, nc))
  op <- tryCatch(stats::optim(par0, nll, method = "L-BFGS-B", lower = lo,
                              upper = hi, control = list(maxit = 1000, factr = 1e5)),
                 error = function(e) NULL)
  if (is.null(op) || !is.finite(op$value) || op$value >= 1e99)
    return(list(ok = FALSE, ll = NA_real_, npar = na + nb + nc))
  list(ok = TRUE, ll = -op$value, npar = na + nb + nc, par = decoupe(op$par))
}

.hstat_dl50_ll_sature <- function(essais) {
  s <- 0; g <- 0L
  for (e in essais) {
    s <- s + hstat_dl50_logvrais(e$doses$x, e$doses$n, e$doses$x / e$doses$n)
    g <- g + nrow(e$doses)
    if (e$n0 > 0) {
      s <- s + hstat_dl50_logvrais(e$x0, e$n0, e$x0 / e$n0)
      g <- g + 1L
    }
  }
  list(ll = s, groupes = g)
}

.hstat_dl50_ligne_test <- function(libelle, m0, m1, ddl, alpha = 0.05,
                                   concl_oui, concl_non) {
  if (!isTRUE(m0$ok) || !isTRUE(m1$ok) || ddl <= 0)
    return(data.frame(Hypothese = libelle, Chi2 = NA_real_, DDL = as.integer(ddl),
                      p = NA_real_,
                      Conclusion = tr("Estimation impossible ou hypothèses confondues : le test n'est pas effectué."),
                      stringsAsFactors = FALSE))
  chi2 <- max(0, 2 * (m1$ll - m0$ll))
  p <- stats::pchisq(chi2, ddl, lower.tail = FALSE)
  data.frame(Hypothese = libelle, Chi2 = chi2, DDL = as.integer(ddl), p = p,
             Conclusion = switch(hstat_p_verdict(p, alpha),
               significatif = concl_oui,
               `non significatif` = concl_non,
               tr("Test non calculable : la statistique est indéterminée.")),
             stringsAsFactors = FALSE)
}

hstat_dl50_comparaison <- function(essais,
                                   scenario = c("heterogene", "homogene", "nulle"),
                                   alpha = 0.05) {
  scenario <- match.arg(scenario)
  vide <- function(motif) {
    r <- data.frame()
    attr(r, "message") <- motif
    r
  }
  if (length(essais) < 2L)
    return(vide(tr("La comparaison demande au moins deux essais.")))
  if (length(essais) > HSTAT_DL50_ESSAIS_MAX)
    return(vide(trf("Comparaison limitée à %d essais, comme dans WIN DL.",
                    HSTAT_DL50_ESSAIS_MAX)))
  for (e in essais) {
    m <- .hstat_dl50_valide(e)
    if (!is.null(m)) return(vide(m))
  }
  # Le scenario a mortalite nulle n'a de sens que si AUCUN essai n'a observe de
  # mortalite dans son temoin : le manuel l'ecrit, et fixer c a zero devant un
  # temoin qui compte des morts ferait porter cette mortalite par la pente.
  if (identical(scenario, "nulle") &&
      any(vapply(essais, function(e) e$n0 > 0 && e$x0 > 0, logical(1))))
    return(vide(tr("Au moins un essai présente une mortalité naturelle observée : le scénario à mortalité nulle n'est pas applicable.")))

  sat <- .hstat_dl50_ll_sature(essais)
  cm <- switch(scenario, heterogene = "libre", homogene = "commun", nulle = "nul")

  base  <- .hstat_dl50_fit_multi(essais, FALSE, FALSE, cm)
  ident <- .hstat_dl50_fit_multi(essais, TRUE,  TRUE,  cm)
  paral <- .hstat_dl50_fit_multi(essais, FALSE, TRUE,  cm)

  # Le 3e test oppose TOUJOURS une mortalite naturelle contrainte a une
  # mortalite libre par essai. Sous le scenario heterogene, prendre le modele
  # de base comme hypothese nulle reviendrait a comparer un modele a lui-meme :
  # zero degre de liberte, et un test qui ne teste rien.
  c_libre <- .hstat_dl50_fit_multi(essais, FALSE, FALSE, "libre")
  t3_0 <- if (identical(scenario, "nulle"))
    .hstat_dl50_fit_multi(essais, FALSE, FALSE, "nul")
  else .hstat_dl50_fit_multi(essais, FALSE, FALSE, "commun")

  lib3 <- if (identical(scenario, "nulle"))
    tr("Nullité de la mortalité naturelle (c = 0)")
  else tr("Égalité des mortalités naturelles (c identiques)")
  concl3_oui <- if (identical(scenario, "nulle"))
    tr("Test significatif : la mortalité naturelle n'est pas nulle.")
  else tr("Test significatif : les mortalités naturelles diffèrent entre essais.")
  concl3_non <- if (identical(scenario, "nulle"))
    tr("Test non significatif : la mortalité naturelle peut être considérée comme nulle.")
  else tr("Test non significatif : les mortalités naturelles peuvent être considérées comme identiques.")

  res <- rbind(
    .hstat_dl50_ligne_test(
      tr("Ajustement des modèles aux données"), base,
      list(ok = TRUE, ll = sat$ll, npar = sat$groupes),
      sat$groupes - base$npar, alpha,
      tr("Test significatif : le modèle probit s'ajuste mal aux données."),
      tr("Test non significatif : l'ajustement probit est légitime.")),
    .hstat_dl50_ligne_test(
      tr("Identité des droites (a et b identiques)"), ident, base,
      base$npar - ident$npar, alpha,
      tr("Test significatif : les droites de régression diffèrent entre essais."),
      tr("Test non significatif : les droites peuvent être considérées comme identiques.")),
    .hstat_dl50_ligne_test(lib3, t3_0, c_libre,
      c_libre$npar - t3_0$npar, alpha, concl3_oui, concl3_non),
    .hstat_dl50_ligne_test(
      tr("Parallélisme des droites (b identiques)"), paral, base,
      base$npar - paral$npar, alpha,
      tr("Test significatif : les droites ne sont pas parallèles."),
      tr("Test non significatif : les droites peuvent être considérées comme parallèles.")))
  rownames(res) <- NULL
  attr(res, "scenario") <- scenario
  attr(res, "essais") <- length(essais)
  attr(res, "avertissement") <- tr("Les quatre tests sont indépendants les uns des autres : deux tests non significatifs pris séparément ne permettent pas de conclure sur leur conjonction. Vérifiez que le scénario retenu ne crée pas de conflit entre les conclusions.")
  res
}

# ---------------------------------------------------------------------------
#  FUSION : regrouper les repetitions d'une meme experimentation
# ---------------------------------------------------------------------------
#  Deux garde-fous, tous deux du logiciel d'origine :
#  1. les champs qui DEFINISSENT l'experience doivent etre identiques -- sinon
#     la fusion fabrique un essai qui n'a jamais eu lieu ;
#  2. un test d'identite des modeles significatif a 5 % BLOQUE la fusion :
#     empiler des repetitions qui ne se ressemblent pas noierait leur
#     difference au lieu de la montrer.
hstat_dl50_fusion <- function(essais, alpha = 0.05) {
  vide <- function(motif) list(ok = FALSE, message = motif)
  if (length(essais) < 2L) return(vide(tr("La fusion demande au moins deux essais.")))
  ch <- lapply(essais, function(e) e$champs)
  for (nm in HSTAT_DL50_CHAMPS_FUSION) {
    v <- vapply(ch, function(c1) trimws(as.character(c1[[nm]] %||% "")), character(1))
    if (length(unique(v)) > 1L)
      return(vide(trf("Le champ « %s » diffère entre les essais (%s) : la fusion assemblerait des expérimentations différentes.",
                      HSTAT_DL50_CHAMPS[[nm]] %||% nm,
                      paste(unique(v), collapse = " / "))))
  }
  d <- do.call(rbind, lapply(essais, function(e) e$doses))
  ag <- stats::aggregate(cbind(n, x) ~ dose, data = d, FUN = sum)
  if (nrow(ag) > HSTAT_DL50_DOSES_MAX)
    return(vide(trf("La fusion produirait %d doses : la limite est de %d.",
                    nrow(ag), HSTAT_DL50_DOSES_MAX)))

  m1 <- .hstat_dl50_fit_multi(essais, FALSE, FALSE, "libre")
  m0 <- .hstat_dl50_fit_multi(essais, TRUE, TRUE, "commun")
  fixe_c <- FALSE
  if (!isTRUE(m1$ok) || !isTRUE(m0$ok)) {
    # Repli du logiciel : en cas d'echec d'estimation ET de mortalite naturelle
    # globale nulle, le test est refait avec c fixee a zero.
    if (all(vapply(essais, function(e) e$x0 == 0, logical(1)))) {
      m1 <- .hstat_dl50_fit_multi(essais, FALSE, FALSE, "nul")
      m0 <- .hstat_dl50_fit_multi(essais, TRUE, TRUE, "nul")
      fixe_c <- TRUE
    }
  }
  if (!isTRUE(m1$ok) || !isTRUE(m0$ok))
    return(vide(tr("L'estimation a échoué sous l'une des deux hypothèses : la fusion est bloquée.")))
  chi2 <- max(0, 2 * (m1$ll - m0$ll))
  ddl <- max(1L, m1$npar - m0$npar)
  p <- stats::pchisq(chi2, ddl, lower.tail = FALSE)
  if (identical(hstat_p_verdict(p, alpha), "significatif"))
    return(list(ok = FALSE, chi2 = chi2, ddl = ddl, p = p, c_fixe = fixe_c,
                message = trf("Test d'identité des essais significatif (Chi-2 = %.3f, ddl = %d, p = %.4f) : les essais diffèrent, les fusionner masquerait cette différence.",
                              chi2, ddl, p)))
  ess <- hstat_dl50_essai(
    ag$dose, ag$n, ag$x,
    temoin_n = sum(vapply(essais, function(e) e$n0, numeric(1))),
    temoin_morts = sum(vapply(essais, function(e) e$x0, numeric(1))),
    titre = trf("Fusion de %d essais", length(essais)),
    champs = essais[[1]]$champs)
  list(ok = TRUE, essai = ess, chi2 = chi2, ddl = ddl, p = p, c_fixe = fixe_c,
       message = trf("Test d'identité non significatif (Chi-2 = %.3f, ddl = %d, p = %.4f) : la fusion est légitime.",
                     chi2, ddl, p))
}

# =============================================================================
#  LECTURE ET ECRITURE DES FICHIERS
# =============================================================================
#  Trois portes d'entree, parce que les donnees arrivent de trois endroits :
#   - un fichier natif de WIN DL (.TXT), encode en CP437, separateurs de champ
#     invisibles (octets 0x00 a 0x09) ;
#   - le jeu de donnees deja charge dans HStat -- donc CSV, Excel, SPSS, SAS,
#     Stata, texte delimite, base SQL : tout ce que l'onglet Chargement sait
#     lire, sans qu'une ligne de plus soit necessaire ici ;
#   - la saisie directe au clavier.
# =============================================================================

.hstat_dl50_decode <- function(brut) {
  # Les fichiers de WIN DL viennent du DOS : CP437. Un fichier retape depuis
  # Windows sera en CP1252, un fichier moderne en UTF-8. On essaie dans cet
  # ordre et on garde le premier qui ne produit pas de caractere invalide.
  for (enc in c("CP437", "CP1252", "UTF-8")) {
    s <- tryCatch(iconv(brut, from = enc, to = "UTF-8"), error = function(e) NA)
    if (!anyNA(s)) return(s)
  }
  iconv(brut, from = "CP437", to = "UTF-8", sub = "?")
}

#  LE FICHIER SE LIT EN OCTETS, PAS EN LIGNES DE TEXTE. Les separateurs de la
#  premiere ligne sont les octets 0x00 a 0x09 : `readLines()` s'arrete sur le
#  premier zero (« nul character not allowed ») et perdrait l'en-tete entier.
#  Le decoupage en lignes se fait donc AUSSI sur les octets -- `rawToChar()`
#  sur l'ensemble rendrait une chaine invalide dans la locale courante, les
#  accents du CP437 n'etant pas de l'UTF-8, et `strsplit()` refuserait.
hstat_dl50_lire_windl <- function(chemin) {
  echec <- function(motif) list(ok = FALSE, message = motif)
  taille <- tryCatch(file.size(chemin), error = function(e) NA_real_)
  if (!isTRUE(is.finite(taille)) || taille <= 0)
    return(echec(tr("Fichier vide ou illisible.")))
  oct <- tryCatch(readBin(chemin, "raw", n = taille), error = function(e) NULL)
  if (is.null(oct) || !length(oct)) return(echec(tr("Fichier vide ou illisible.")))
  garde_lf <- oct == as.raw(10L)
  oct[oct <= as.raw(9L)] <- as.raw(31L)          # 0x00 à 0x09 -> séparateur
  oct[oct == as.raw(13L)] <- as.raw(32L)         # CR -> espace
  oct[garde_lf] <- as.raw(10L)
  fins <- which(oct == as.raw(10L))
  deb <- c(1L, fins + 1L); fin <- c(fins - 1L, length(oct))
  brut <- vapply(seq_along(deb), function(i) {
    if (fin[i] < deb[i] || deb[i] > length(oct)) "" else rawToChar(oct[deb[i]:fin[i]])
  }, character(1))
  L <- .hstat_dl50_decode(brut)
  L <- sub("Û\\s*$", "", L)                      # marqueur de fin de fichier
  L <- trimws(L, which = "right")
  if (length(L) < 6L)
    return(echec(tr("Fichier trop court pour être un essai WIN DL : il faut l'en-tête, le titre, l'unité, le témoin et au moins une dose.")))

  # Ligne 1 : dix champs separes par les octets de controle ramenes ci-dessus a
  # 0x1F. Le manuel les figure par « _ » : on accepte les deux.
  ch <- strsplit(L[1], "[\037_]")[[1]]
  if (length(ch) && !nzchar(ch[1])) ch <- ch[-1]
  ch <- c(ch, rep("", max(0, 10 - length(ch))))
  champs <- list(date = ch[1], auteur = ch[2], duree = ch[3], temperature = ch[4],
                 espece = ch[5], stade = ch[6], matiere1 = ch[7], matiere2 = ch[8],
                 ratio = ch[9], methode = ch[10])

  titre <- trimws(L[2])
  l3 <- trimws(L[3])
  k <- suppressWarnings(as.integer(sub("^\\s*([0-9]+).*$", "\\1", l3)))
  champs$unite <- trimws(sub("^\\s*[0-9]+\\s*", "", l3))
  n0 <- suppressWarnings(as.numeric(trimws(L[4])))
  x0 <- suppressWarnings(as.numeric(trimws(L[5])))
  if (!isTRUE(is.finite(k)) || k < 1L)
    return(echec(tr("Nombre de doses illisible en troisième ligne du fichier.")))
  if (length(L) < 5L + k)
    return(echec(trf("Le fichier annonce %d doses mais n'en contient que %d.",
                     k, max(0L, length(L) - 5L))))

  mat <- lapply(L[5 + seq_len(k)], function(li) {
    v <- suppressWarnings(as.numeric(strsplit(trimws(li), "[[:space:]]+")[[1]]))
    v[is.finite(v)]
  })
  if (any(vapply(mat, length, integer(1)) < 3L))
    return(echec(tr("Ligne de dose incomplète : il faut au moins la dose, l'effectif testé et le nombre de morts.")))
  ess <- hstat_dl50_essai(
    dose = vapply(mat, `[`, numeric(1), 1),
    effectif = vapply(mat, `[`, numeric(1), 2),
    morts = vapply(mat, `[`, numeric(1), 3),
    temoin_n = if (isTRUE(is.finite(n0))) n0 else 0,
    temoin_morts = if (isTRUE(is.finite(x0))) x0 else 0,
    titre = titre, champs = champs)
  msg <- .hstat_dl50_valide(ess)
  if (!is.null(msg)) return(echec(msg))
  list(ok = TRUE, essai = ess)
}

# Ecriture au format natif : un fichier ecrit ici se relit dans WIN DL.
# L'en-tete se monte EN OCTETS, ses separateurs allant de 0x00 a 0x09 -- et le
# zero ne peut pas exister dans une chaine de R. C'est la contrainte symetrique
# de celle de la lecture.
hstat_dl50_ecrire_windl <- function(essai, chemin) {
  ch <- essai$champs
  g <- function(nm) as.character(ch[[nm]] %||% "")
  d <- essai$doses
  suite <- c(
    essai$titre,
    sprintf("%3d %s", nrow(d), g("unite")),
    sprintf("%5d", round(essai$n0)), sprintf("%5d", round(essai$x0)),
    sprintf("%15.5f %14d %14d", d$dose, round(d$n), round(d$x)))
  ordre <- c("date", "auteur", "duree", "temperature", "espece", "stade",
             "matiere1", "matiere2", "ratio", "methode")
  entete <- raw(0)
  for (i in seq_along(ordre))
    entete <- c(entete, as.raw(i - 1L),
                charToRaw(iconv(g(ordre[i]), "UTF-8", "CP437", sub = "?")))
  con <- file(chemin, open = "wb")
  on.exit(close(con))
  writeBin(c(entete, charToRaw("\r\n")), con)
  writeBin(charToRaw(paste0(
    paste(iconv(suite, "UTF-8", "CP437", sub = "?"), collapse = "\r\n"),
    "\r\n\333\r\n")), con)          # 0xDB : marqueur de fin de WIN DL
  invisible(chemin)
}

# ---------------------------------------------------------------------------
#  Le fichier de resultats .PRN, ligne pour ligne
# ---------------------------------------------------------------------------
#  Meme disposition que WIN DL : c'est ce fichier que les rapports de bioessai
#  recopient, et en changer obligerait a refaire les gabarits de rapport.
hstat_dl50_prn <- function(fit, nom_fichier = "") {
  if (!isTRUE(fit$ok)) return(character(0))
  e <- fit$essai; ch <- e$champs
  g <- function(nm) as.character(ch[[nm]] %||% "")
  dl <- hstat_dl50_doses_letales(fit)
  nb <- function(v, d = 5) formatC(v, format = "f", digits = d)
  sc <- function(v) formatC(v, format = "e", digits = 5)
  tb <- fit$table
  c(
    trf("Nom du fichier : %s", nom_fichier),
    trf("Titre de l'essai : %s", e$titre),
    trf("Date de l'essai : %s", g("date")),
    trf("Auteur : %s", g("auteur")),
    trf("Espèce : %s", g("espece")),
    trf("Stade : %s", g("stade")),
    trf("Durée : %s", g("duree")),
    trf("Température : %s", g("temperature")),
    trf("Matières actives n°1 : %s", g("matiere1")),
    trf("                 n°2 : %s", g("matiere2")),
    trf("Ratio : %s", g("ratio")),
    trf("Méthode de traitement : %s", g("methode")),
    trf("%d doses exprimées en : %s", fit$n_doses, g("unite")),
    trf("Nombre d'individus testés dans l'échantillon témoin : %s", format(e$n0)),
    trf("Nombre d'individus morts dans l'échantillon témoin : %s", format(e$x0)),
    trf("Pourcentage de mortalité naturelle dans l'échantillon témoin : %.2f %%",
        100 * fit$c_temoin),
    trf("Réponses donnant 0 %% de mortalité : %d ; 100 %% de mortalité : %d (après correction par Abbott)",
        fit$n_zero, fit$n_cent),
    trf("Méthode d'estimation : %s",
        names(HSTAT_DL50_METHODES)[match(fit$methode, HSTAT_DL50_METHODES)]),
    tr("Résultats de la régression :"),
    if (fit$converge)
      trf("  ** Convergence atteinte après %d itérations (écart absolu sur log-vraisemblance < 1e-5)",
          fit$iterations)
    else trf("  ** Convergence NON atteinte après %d itérations", fit$iterations),
    tr("Paramètres statistiques de la régression :"),
    paste(c(tr("Pente (b)"), tr("Ordonnée à l'origine (a)"), tr("Mort. naturelle (c)"),
            tr("Log-vrais.(H0)"), tr("Log-vrais.(H1)")), collapse = "\t"),
    paste(c(nb(fit$b), nb(fit$a), nb(fit$c), nb(fit$ll0), nb(fit$ll1)), collapse = "\t"),
    tr("Écarts-types (ET) et covariances des paramètres :"),
    paste(c("ET(a)", "ET(b)", "ET(c)", "Vab", "Vac", "Vbc"), collapse = "\t"),
    paste(sc(c(sqrt(fit$Vh[1, 1]), sqrt(fit$Vh[2, 2]), sqrt(fit$Vh[3, 3]),
               fit$Vh[1, 2], fit$Vh[1, 3], fit$Vh[2, 3])), collapse = "\t"),
    tr("Équation de la droite de régression : Mortalité (en probit) = a + b . log(dose)"),
    trf("    Y = %s + (%s * X)", nb(fit$a), nb(fit$b)),
    trf("Pourcentage estimé de mortalité naturelle dans l'essai : %.2f %%", 100 * fit$c),
    tr("Test d'ajustement du modèle aux données (test du Chi-2) :"),
    trf("* Chi-2 calculé : %.3f  * Degrés de liberté : %d  * Probabilité de dépassement : %.4f",
        fit$chi2, fit$ddl, fit$p_chi2),
    if (fit$heterogene)
      trf("** Test du Chi-2 significatif à 5 %% : facteur d'hétérogénéité %.3f appliqué aux variances, quantile de Student à %d degrés de liberté.",
          fit$facteur, fit$ddl)
    else tr("** Test du Chi-2 non significatif à 5 % : bon ajustement du modèle."),
    trf("Doses létales et intervalles à %d %% :", round(100 * (1 - fit$alpha))),
    paste(c("", tr("log.Dose"), tr("Écart-type"), tr("Dose"), tr("Limite inf."),
            tr("Limite sup."), tr("Intervalle")), collapse = "\t"),
    vapply(seq_len(nrow(dl)), function(i) paste(c(
      trf("DL %g", dl$Seuil[i]), sc(dl$Log_dose[i]), sc(dl$Ecart_type[i]),
      sc(dl$Dose[i]), sc(dl$Limite_inf[i]), sc(dl$Limite_sup[i]),
      dl$Intervalle[i]), collapse = "\t"), character(1)),
    paste(c("N", tr("Dose"), tr("Pop.test"), tr("Mort."), tr("log.dose"),
            tr("Mort.corrigée"), tr("Probit corr."), tr("Mort.attendue"),
            tr("Probit att.")), collapse = "\t"),
    vapply(seq_len(nrow(tb)), function(i) paste(c(
      format(tb$N[i]), formatC(tb$Dose[i], format = "g", digits = 5),
      format(tb$Effectif[i]), format(tb$Morts[i]),
      nb(tb$Log_dose[i], 4), nb(tb$Mortalite_corrigee[i], 4),
      nb(tb$Probit_corrige[i], 4), nb(tb$Mortalite_attendue[i], 4),
      nb(tb$Probit_attendu[i], 4)), collapse = "\t"), character(1)))
}

# ---------------------------------------------------------------------------
#  Depuis un tableau de donnees deja charge dans HStat
# ---------------------------------------------------------------------------
#  Le temoin peut se declarer de deux facons, et les deux existent sur le
#  terrain : une ligne portant la dose zero, ou des effectifs saisis a part.
#  La dose zero ne peut pas entrer dans la regression (son logarithme n'existe
#  pas), elle est donc EXTRAITE et devient le temoin -- plutot qu'ecartee en
#  silence, ce qui perdrait la mortalite naturelle de l'essai.
hstat_dl50_depuis_donnees <- function(df, col_dose, col_effectif, col_morts,
                                      col_essai = NULL, titre = "", champs = list()) {
  echec <- function(motif) list(ok = FALSE, message = motif)
  if (!is.data.frame(df) || !nrow(df)) return(echec(tr("Aucune donnée chargée.")))
  groupe <- if (!is.null(col_essai) && nzchar(col_essai)) col_essai else NULL
  manque <- setdiff(c(col_dose, col_effectif, col_morts, groupe), names(df))
  if (length(manque))
    return(echec(trf("Colonne(s) absente(s) du tableau : %s.",
                     paste(manque, collapse = ", "))))
  d <- data.frame(
    dose = suppressWarnings(as.numeric(df[[col_dose]])),
    n    = suppressWarnings(as.numeric(df[[col_effectif]])),
    x    = suppressWarnings(as.numeric(df[[col_morts]])),
    stringsAsFactors = FALSE)
  d$essai <- if (!is.null(groupe)) as.character(df[[groupe]]) else rep(titre, nrow(d))
  d <- d[stats::complete.cases(d[c("dose", "n", "x")]), , drop = FALSE]
  if (!nrow(d))
    return(echec(tr("Aucune ligne exploitable : les colonnes choisies ne contiennent pas de nombres.")))

  essais <- lapply(split(d, d$essai), function(s) {
    tem <- s[s$dose <= 0, , drop = FALSE]
    dos <- s[s$dose > 0, , drop = FALSE]
    hstat_dl50_essai(dos$dose, dos$n, dos$x,
                     temoin_n = sum(tem$n), temoin_morts = sum(tem$x),
                     titre = s$essai[1], champs = champs)
  })
  if (length(essais) > HSTAT_DL50_ESSAIS_MAX)
    return(echec(trf("%d essais dans la colonne de regroupement : la limite est de %d.",
                     length(essais), HSTAT_DL50_ESSAIS_MAX)))
  list(ok = TRUE, essais = essais)
}

# ---------------------------------------------------------------------------
#  Le graphique : log-dose en abscisse, probit ET pourcentage en ordonnee
# ---------------------------------------------------------------------------
#  Quatre elements, exactement ceux de WIN DL : les points de l'essai, la
#  courbe qui les relie, la droite de regression, et la bande d'intervalle.
#  Chacun se montre ou se cache -- superposes sur six essais, ils deviennent
#  illisibles, et c'est le graphique qui sert a decider.
#
#  L'axe des ordonnees porte DEUX graduations : le probit, qui est l'echelle du
#  modele, et le pourcentage, qui est celle de la question posee. Une seule des
#  deux obligerait a convertir de tete.
hstat_dl50_graphique <- function(fits, points = TRUE, courbe = FALSE,
                                 droite = TRUE, bande = TRUE, reperes = TRUE,
                                 titre = "", xlab = NULL, ylab = NULL,
                                 theme = NULL, palette = "Set1") {
  fits <- Filter(function(f) isTRUE(f$ok), fits)
  if (!length(fits)) return(NULL)
  nom <- vapply(fits, function(f) {
    t <- f$essai$titre
    if (length(t) && nzchar(t)) t else tr("Essai")
  }, character(1))
  nom <- make.unique(nom, sep = " ")

  pts <- do.call(rbind, lapply(seq_along(fits), function(i) {
    t <- fits[[i]]$table
    data.frame(Essai = nom[i], x = t$Log_dose, y = t$Probit_corrige,
               stringsAsFactors = FALSE)
  }))
  pts <- pts[is.finite(pts$y), , drop = FALSE]

  rg <- range(unlist(lapply(fits, function(f) f$table$Log_dose)), finite = TRUE)
  if (!all(is.finite(rg))) return(NULL)
  if (diff(rg) <= 0) rg <- rg + c(-1, 1)
  rg <- rg + c(-1, 1) * diff(rg) * 0.08
  grille <- seq(rg[1], rg[2], length.out = 200)

  lig <- do.call(rbind, lapply(seq_along(fits), function(i) {
    f <- fits[[i]]
    eta <- f$a + f$b * grille
    se <- sqrt(pmax(f$Vh[1, 1] + grille^2 * f$Vh[2, 2] + 2 * grille * f$Vh[1, 2], 0))
    data.frame(Essai = nom[i], x = grille, y = eta,
               lo = eta - f$t * se, hi = eta + f$t * se, stringsAsFactors = FALSE)
  }))
  # La bande d'intervalle n'a de sens que dans la fenetre lisible : sur deux
  # decades, elle atteint des probits de +/- 20 et ecrase tout le reste.
  ylim <- range(c(pts$y, stats::qnorm(c(0.001, 0.999))), finite = TRUE)

  pc <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)
  brk <- stats::qnorm(pc / 100)
  p <- ggplot2::ggplot()
  if (isTRUE(bande))
    p <- p + ggplot2::geom_ribbon(
      data = lig, ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
                               fill = .data$Essai), alpha = 0.12, colour = NA)
  if (isTRUE(droite))
    p <- p + ggplot2::geom_line(
      data = lig, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$Essai),
      linewidth = 0.9)
  if (isTRUE(courbe) && nrow(pts))
    p <- p + ggplot2::geom_line(
      data = pts, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$Essai),
      linetype = "dashed", linewidth = 0.6)
  if (isTRUE(points) && nrow(pts))
    p <- p + ggplot2::geom_point(
      data = pts, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$Essai),
      size = 2.4)
  if (isTRUE(reperes))
    p <- p + ggplot2::geom_hline(yintercept = stats::qnorm(HSTAT_DL50_SEUILS / 100),
                                 linetype = "dotted", colour = "grey45")
  p <- p +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::scale_x_continuous(
      name = xlab %||% tr("Dose (échelle logarithmique)"),
      labels = function(v) formatC(10^v, format = "g", digits = 3)) +
    ggplot2::scale_y_continuous(
      name = ylab %||% tr("Mortalité (probit)"),
      sec.axis = ggplot2::sec_axis(~ ., breaks = brk, labels = paste0(pc, " %"),
                                   name = tr("Mortalité (%)"))) +
    ggplot2::labs(title = titre, colour = tr("Essai"), fill = tr("Essai")) +
    (theme %||% viz_get_theme("minimal"))
  if (length(fits) > 1L)
    p <- p + ggplot2::scale_colour_brewer(palette = palette) +
             ggplot2::scale_fill_brewer(palette = palette)
  p
}
