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

# LE PLAFOND D'ITERATIONS EST UN REGLAGE, PAS UNE CONSTANTE.
#
# Il valait 500 sans que personne puisse le voir ni le changer. Cent suffit
# tres largement -- l'essai de reference converge en trois boucles -- et un
# plafond qu'on peut lever est ce qu'il faut le jour ou un essai difficile
# n'aboutit pas : mieux vaut lever le plafond en connaissance de cause que
# lire « convergence NON atteinte » sans pouvoir agir.
#
# Le plafond de NEWTON-RAPHSON EMBOITE dans l'EM reste a 50, la valeur du
# manuel (« avec 50 iterations maximum ») : c'est une convention du logiciel,
# pas un reglage de confort. Quand Newton-Raphson est l'ajustement lui-meme
# -- Abbott, mortalite nulle -- c'est le plafond choisi par l'utilisateur qui
# s'applique, puisque c'est lui la boucle qu'il regarde converger.
# LE CHEMIN DE CONVERGENCE DE L'EM EST UN CHOIX, ET IL A ETE MESURE.
#
# HStat part de la mortalite du temoin. Quand celle-ci vaut ZERO -- le cas de
# l'essai de reference -- l'etape [E] rend des poids nuls et `c` ne bouge plus :
# l'ajustement converge en TROIS boucles, sur la vraisemblance la plus haute.
#
# WIN DL, lui, en annonce 75. La cause est identifiee : son `c` de depart n'est
# pas nul, l'EM rampe alors vers la borne, et il s'arrete AVANT le maximum --
# sa log-vraisemblance imprimee (-105,63592) est plus basse que celle que HStat
# atteint (-105,63582).
#
# Reproduire ce chemin RAPPROCHE de WIN DL : sur les 26 grandeurs publiees, il
# gagne sur 18 et divise l'ecart median par 3,2 (1,3e-5 -> 4,1e-6) ; `a` sort a
# 2,19760 comme le logiciel l'imprime, au lieu de 2,19759.
#
# Mais il COUTE : sur un essai a reponse plate, il ne converge plus en cent
# iterations la ou le depart nul aboutit en trois. C'est pourquoi il est
# propose, et non impose -- le defaut reste le chemin rapide, qui atteint un
# optimum meilleur sur tous les essais essayes.
HSTAT_DL50_CONVERGENCE <- c(
  "Rapide — la vraisemblance la plus haute (défaut)" = "rapide",
  "Chemin de WIN DL — comparaison chiffre pour chiffre" = "windl")

# Chiffres significatifs A L'AFFICHAGE. Les exports gardent la precision
# complete : arrondir une donnee exportee la ferait diverger du calcul.
HSTAT_DL50_CHIFFRES   <- 5L

HSTAT_DL50_ITMAX      <- 100L    # plafond par defaut, modifiable a l'ecran
HSTAT_DL50_ITMAX_MAX  <- 5000L   # garde-fou : au-dela, c'est le modele qui cloche
HSTAT_DL50_ITMAX_NR   <- 50L     # Newton-Raphson emboite (WIN DL : 50)
HSTAT_DL50_TOL        <- 1e-5    # ecart absolu sur la log-vraisemblance
HSTAT_DL50_DOSES_MAX  <- 100L    # limite de WIN DL
HSTAT_DL50_ESSAIS_MAX <- 6L      # limite de WIN DL
HSTAT_DL50_MIN_UTILES <- 3L      # doses a mortalite corrigee strictement entre 0 et 1

# Au-dela, la correction d'ABBOTT devient peu fiable et l'usage veut qu'on
# refasse l'essai (FINNEY, 1971). Ce n'est pas une erreur de saisie : c'est un
# defaut de conduite d'essai, et le dire suffit -- le calcul continue.
HSTAT_DL50_TEMOIN_MAX <- 0.20

# WIN DL n'en propose que DEUX pour un essai isole -- Newton-Raphson avec
# Abbott, et EM. La troisieme est un ajout de HStat : elle donne l'ajustement
# probit nu, celui que `glm(binomial(probit))` produit. Utile, mais sans
# vis-a-vis dans le logiciel ; le libelle le dit, pour qu'on ne cherche pas un
# desaccord la ou il n'y a rien a comparer. (Cote WIN DL, la mortalite fixee a
# zero n'existe que comme scenario de COMPARAISON entre essais.)
HSTAT_DL50_METHODES <- c(
  "Maximum de vraisemblance sur tout l'essai (EM)"     = "em",
  "Mortalité naturelle du seul témoin (Abbott)"        = "abbott",
  "Mortalité naturelle fixée à zéro (hors WIN DL)"     = "nulle")

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
# WIN DL N'INVERSE PAS LA NORMALE EXACTEMENT, ET CELA SE VOIT SUR LES DOSES
# LETALES. Le manuel le dit -- « algorithme d'approximation polynomiale de la
# distribution normale inverse de HASTINGS » -- et les chiffres publies le
# confirment. C'est la formule 26.2.23 d'Abramowitz & Stegun, dont l'erreur
# absolue est bornee par 4,5e-4 :
#
#   F^-1(p) ~ t - (c0 + c1 t + c2 t^2) / (1 + d1 t + d2 t^2 + d3 t^3),
#   t = sqrt(ln(1/q^2)),  q = min(p, 1-p),  signe selon le cote.
#
# Sur l'essai de reference, la DL90 en log-dose vaut :
#
#   | normale exacte | Hastings    | WIN DL imprime |
#   |----------------|-------------|----------------|
#   | -0,9412818     | -0,9410997  | -9,41099e-01   |
#
# L'ecart passe de 1,8e-4 a 7e-7. C'est ce qui obligeait la suite de tests a
# tolerer 1 % sur des bornes que le logiciel imprime a six chiffres.
#
# LE PRIX EST NUL, ET IL A ETE MESURE : l'erreur de l'approximation vaut
# 1,8e-4 en probit, soit 0,04 % sur la dose -- quatre ordres de grandeur sous
# l'intervalle de confiance de la DL50 elle-meme, qui couvre un facteur 20 sur
# l'essai de reference. On ne perd donc aucune precision utile, et on gagne la
# comparabilite chiffre pour chiffre, qui est la promesse du module.
#
# La reciproque n'est PAS reprise : l'approximation de Hastings pour F elle-meme
# a une erreur de 7,5e-8, invisible a la precision d'impression, et l'employer
# degraderait la vraisemblance sans rien rapprocher. La frontiere est mesuree,
# pas choisie.
.hstat_dl50_qnorm <- function(p) {
  p <- as.numeric(p)
  q <- pmin(pmax(ifelse(p > 0.5, 1 - p, p), 1e-300), 0.5)
  t <- sqrt(log(1 / q^2))
  v <- t - (2.515517 + 0.802853 * t + 0.010328 * t^2) /
           (1 + 1.432788 * t + 0.189269 * t^2 + 0.001308 * t^3)
  ifelse(p > 0.5, v, -v)
}

# Le probit de la mortalite corrigee, avec L'AFFECTATION DES VALEURS EXTREMES
# du manuel : « 6 pour 100 % de mortalite et -6 pour 0 % ». Ce sont des valeurs
# CONVENTIONNELLES, pas des mesures -- le probit de 0 et celui de 1 n'existent
# pas. La borne a 1e-12 employee jusqu'ici rendait +/- 7,0345, un nombre qui ne
# dependait que de l'epsilon choisi.
HSTAT_DL50_PROBIT_EXTREME <- 6

hstat_dl50_probit <- function(p) {
  p <- as.numeric(p)
  ifelse(!is.finite(p), NA_real_,
    ifelse(p <= 0, -HSTAT_DL50_PROBIT_EXTREME,
      ifelse(p >= 1, HSTAT_DL50_PROBIT_EXTREME, .hstat_dl50_qnorm(p))))
}

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
                           itmax = HSTAT_DL50_ITMAX, tol = HSTAT_DL50_TOL,
                           chemin = "rapide") {
  a <- a0; b <- b0
  # LE DEPART DECIDE DU CHEMIN. A `c = 0`, l'etape [E] rend des poids nuls et
  # `c` ne bouge plus : trois boucles, la vraisemblance la plus haute. Le
  # depart LISSE -- la regle de Laplace sur le temoin -- laisse `c` explorer,
  # l'EM rampe vers la borne et suit le chemin de WIN DL.
  cc <- if (identical(chemin, "windl")) {
    if (n0 > 0) (x0 + 0.5) / (n0 + 1) else 0.5 / (sum(n) + 1)
  } else if (n0 > 0) x0 / n0 else 0
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

# LA MATRICE A INVERSER EST CELLE DES PARAMETRES REELLEMENT ESTIMES.
#
# C'est la faute la plus couteuse que ce module ait portee, et elle etait
# entierement silencieuse : sous « Abbott » et « mortalite nulle », `c` est
# DECLAREE -- lue sur le temoin, ou posee a zero -- et pourtant la matrice a
# trois lignes etait inversee comme si elle avait ete estimee. Inverser une
# matrice 3x3 pour n'en lire que le bloc (a, b) fait payer une incertitude sur
# `c` que l'hypothese exclut : les variances ressortent bien plus grandes.
#
# Mesure sur l'essai de reference, `c` fixee a zero :
#
#   |            | var(a) | var(b) | ET(DL50) |
#   |------------|--------|--------|----------|
#   | 3x3 (faux) | 0,5893 | 0,3554 |   0,6716 |
#   | 2x2 (juste)| 0,1841 | 0,0329 |   0,1032 |
#
# soit une erreur-type de la DL50 **6,5 fois trop grande**, donc des
# intervalles absurdement larges -- et un `g = t².var(b)/b²` gonfle au point de
# franchir 1, ce qui faisait abandonner Fieller pour la delta-methode sans
# raison.
#
# Le bloc 2x2 est verifie contre `glm(binomial(probit))` quand `c` vaut zero :
# c'est alors exactement le meme modele, et les deux coincident a 1e-7.
#
# L'incoherence etait d'ailleurs INTERNE : `npar` vaut deja 2 pour ces deux
# methodes -- c'est lui qui decide si le Chi-2 a un degre de liberte
# residuel. Le meme ajustement comptait donc deux parametres pour le test
# d'ajustement et trois pour les variances.
#
# L'essai de reference de WIN DL est ajuste par EM, ou `c` EST estimee : ce
# cas-la garde ses trois lignes et ne bouge pas d'un chiffre.
.hstat_dl50_vcov <- function(z, n, a, b, cc, npar) {
  I <- .hstat_dl50_fisher(z, n, a, b, cc)
  if (npar >= 3L) return(solve(I))
  V <- matrix(NA_real_, 3, 3, dimnames = dimnames(I))
  V[1:2, 1:2] <- solve(I[1:2, 1:2])
  # `c` n'etant pas estimee, son ecart-type et ses covariances N'EXISTENT PAS.
  # Zero dirait « connue exactement », ce qui n'est pas la question posee ;
  # `NA` dit « sans objet ici », et c'est deja l'idiome du tableau de
  # parametres pour les lignes qui ne portent pas d'erreur-type.
  V
}

# ---------------------------------------------------------------------------
#  SAISIE EN POURCENTAGE : l'arrondi est le sujet, pas la conversion
# ---------------------------------------------------------------------------
#  Beaucoup d'operateurs notent « 40 % » plutot que « 12 sur 30 ». La
#  conversion est triviale ; ce qui ne l'est pas, c'est que le modele binomial
#  a besoin d'un ENTIER. Trois pieges, tous silencieux :
#
#  1. L'ARRONDI CHANGE LE POURCENTAGE, et il faut le dire. 40 % de 7 individus
#     font 2,8 : on enregistre 3, soit 42,86 %. Rendre la valeur sans le
#     signaler laisse croire que l'essai porte le chiffre saisi.
#  2. DEUX POURCENTAGES DIFFERENTS DONNENT LE MEME EFFECTIF. Sur n = 7, 40 % et
#     43 % rendent tous deux 3 morts. Une saisie plus fine que l'essai ne
#     l'autorise n'ajoute pas d'information, elle en promet une qui n'existe
#     pas.
#  3. `round()` ARRONDIT AU PAIR EN R : `round(2.5)` vaut 2, pas 3. « 50 % de
#     5 individus » rendrait donc 2, ce que personne n'attend. On arrondit au
#     plus proche, moities vers le haut -- la convention de l'operateur.
#
#  Un pourcentage sans effectif ne donne rien : il reste `NA`, et le compte des
#  lignes concernees est rendu a part.
hstat_dl50_pct_vers_morts <- function(effectif, pct) {
  p <- suppressWarnings(as.numeric(pct))
  # `n` EST RECYCLE EXPLICITEMENT. Les operateurs vectoriels le recyclent tout
  # seuls, mais l'INDEXATION non : `n[ok]` sur un effectif de longueur 1 et un
  # masque de longueur 4 rend `20 NA NA NA`, et trois lignes sur quatre
  # ressortaient vides. Le defaut ne se voit que lorsqu'un seul effectif sert a
  # plusieurs doses -- c'est-a-dire dans le cas le plus courant.
  n <- rep_len(suppressWarnings(as.numeric(effectif)), length(p))
  ok <- is.finite(n) & n > 0 & is.finite(p)
  x <- rep(NA_real_, length(p))
  brut <- n * p / 100
  x[ok] <- floor(brut[ok] + 0.5)
  x[ok] <- pmin(pmax(x[ok], 0), n[ok])
  ecart <- rep(NA_real_, length(p))
  ecart[ok] <- 100 * x[ok] / n[ok] - p[ok]
  structure(x, ecart = ecart,
            arrondies = which(ok & abs(ecart) > 1e-9),
            sans_effectif = which(!ok & is.finite(p)))
}

# Le chemin inverse, pour l'affichage. Il ne perd rien : c'est l'effectif qui
# fait foi, le pourcentage n'en est qu'une lecture.
hstat_dl50_morts_vers_pct <- function(effectif, morts) {
  n <- suppressWarnings(as.numeric(effectif))
  x <- suppressWarnings(as.numeric(morts))
  ifelse(is.finite(n) & n > 0 & is.finite(x), 100 * x / n, NA_real_)
}

HSTAT_DL50_UNITES_SAISIE <- c(
  "Morts (effectif)"  = "morts",
  "Mortalité (%)"     = "pct")

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

# Le message de la pente negative, ecrit UNE fois : il est rendu depuis deux
# endroits, et deux formulations du meme refus finiraient par diverger.
.hstat_dl50_msg_pente <- function()
  tr("La mortalité décroît quand la dose augmente : la droite de Henry n'a pas de sens ici. Vérifiez que les colonnes « effectif testé » et « morts » ne sont pas inversées, et que les doses correspondent bien aux mortalités.")

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
                              alpha = 0.05, itmax = HSTAT_DL50_ITMAX,
                              chemin = c("rapide", "windl")) {
  methode <- match.arg(methode)
  chemin <- match.arg(chemin)
  # Le plafond vient de l'interface : on le borne ici plutot que de faire
  # confiance a un champ numerique, ou l'on peut taper zero ou du texte.
  itmax <- suppressWarnings(as.integer(itmax)[1])
  if (!length(itmax) || is.na(itmax) || itmax < 1L) itmax <- HSTAT_DL50_ITMAX
  itmax <- min(itmax, HSTAT_DL50_ITMAX_MAX)
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

  # UNE PENTE NEGATIVE N'EST PAS UN AJUSTEMENT, C'EST UNE SAISIE A L'ENVERS.
  # Sans ce refus, deux colonnes inversees produisaient un rapport COMPLET --
  # equation, intervalles de confiance, graphique -- ou la DL10 valait mille
  # fois la DL90. Rien a l'ecran ne le signalait : c'est le resultat faux le
  # plus facile a publier de bonne foi.
  #
  # LE REFUS PORTE SUR LA PENTE AJUSTEE, PAS SUR LA VALEUR DE DEPART.
  #
  # Il portait sur les deux, « la premiere pour eviter d'iterer pour rien ».
  # Mais la valeur de depart vient d'une regression NON PONDEREE sur les seules
  # doses de mortalite intermediaire : sur un essai bruite elle sort negative
  # alors que l'ajustement, lui, rend une pente franchement positive. Mesure
  # sur quatre mille essais tires au sort : 23 etaient refuses a tort, et l'un
  # d'eux passait de -0,36 au depart a +2,49 ajuste -- avec un message qui
  # accusait l'utilisateur d'avoir inverse ses colonnes.
  #
  # Iterer « pour rien » coute cinquante iterations d'une regression ponderee
  # sur quelques doses : rien du tout. Un refus a tort, lui, coute un essai.
  # Le defaut a ete trouve par MUTATION : desactiver ce controle ne faisait
  # echouer aucune assertion, ce qui a mene a regarder ce qu'il gardait.

  fit <- if (identical(methode, "em")) {
    .hstat_dl50_em(z, n, x, n0, x0, ini$a, ini$b, itmax = itmax, chemin = chemin)
  } else {
    # Ici Newton-Raphson EST l'ajustement : c'est la boucle que l'utilisateur
    # regarde converger, donc c'est son plafond qui s'applique.
    f <- .hstat_dl50_nr(z, n, x, c_depart, ini$a, ini$b, itmax = itmax)
    c(f, list(c = c_depart))
  }
  if (!is.finite(fit$a) || !is.finite(fit$b))
    return(echec(tr("L'estimation n'a pas convergé : vérifiez que la mortalité croît avec la dose.")))
  if (fit$b <= 0) return(echec(.hstat_dl50_msg_pente()))

  a <- fit$a; b <- fit$b; cc <- fit$c
  eta <- a + b * z
  Pobs <- cc + (1 - cc) * stats::pnorm(eta)

  # LE TEMOIN INFORME `c` ; IL N'EST PAS UNE DOSE.
  #
  # Quand `c` est ESTIMEE (EM), le temoin entre dans la vraisemblance : c'est
  # lui qui la tire, et c'est ce chemin-la que WIN DL reproduit au chiffre
  # pres. Quand `c` est DECLAREE -- Abbott, ou mortalite nulle -- il n'informe
  # plus rien : le modele est fixe avant qu'on le regarde, et l'ajustement se
  # juge sur la serie de doses seule. C'est deja la convention du degre de
  # liberte (nombre de doses moins deux) et celle de la matrice d'information
  # (les doses seules) ; l'y ranger rend les trois coherentes.
  #
  # Ce n'est pas une question de doctrine. Sous « mortalite nulle » avec un
  # temoin qui compte des morts, le modele affirme p = 0 la ou l'on a observe
  # des deces : la vraisemblance vaut moins l'infini. `hstat_dl50_logvrais()`
  # borne la probabilite a 1e-12 pour ne pas rendre l'infini, et le Chi-2
  # ressortait donc FINI -- mais entierement determine par cette borne :
  #
  #   epsilon | 1e-10 | 1e-12 | 1e-15 | 1e-20
  #   Chi-2   | 119,8 | 147,4 | 188,9 | 258,0
  #
  # Un nombre qui change avec une constante d'implementation n'est pas une
  # statistique de test. Il pilotait pourtant le facteur d'heterogeneite, donc
  # la largeur de TOUS les intervalles publies.
  #
  # L'essai de reference de WIN DL n'est pas touche : son temoin est 25/0 et sa
  # mortalite naturelle vaut zero, le terme valait deja zero des deux cotes.
  temoin_lu <- identical(methode, "em") && n0 > 0
  ll0 <- hstat_dl50_logvrais(x, n, Pobs) +
         if (temoin_lu) hstat_dl50_logvrais(x0, n0, cc) else 0
  ll1 <- hstat_dl50_logvrais(x, n, x / n) +
         if (temoin_lu) hstat_dl50_logvrais(x0, n0, x0 / n0) else 0
  chi2 <- max(0, 2 * (ll1 - ll0))
  # Le degre de liberte est celui de WIN DL : nombre de doses - 2. Le manuel
  # l'ecrit, le fichier de reference le confirme (7 doses, ddl = 5) -- y
  # compris quand c est estimee, ou la theorie voudrait retirer un parametre
  # de plus. On garde la convention du logiciel : c'est elle qui fixe le seuil
  # a partir duquel le facteur d'heterogeneite s'applique.
  ddl <- max(1L, nrow(d) - 2L)
  p_chi2 <- stats::pchisq(chi2, ddl, lower.tail = FALSE)

  # MAIS UN TEST SANS DEGRE DE LIBERTE RESIDUEL NE TESTE RIEN. Avec autant de
  # doses que de parametres estimes, le modele passe exactement par les points :
  # le Chi-2 vaut zero par construction, et le plancher `max(1L, ...)` -- qui
  # existe pour eviter une division par zero -- le transformait en « p = 1,0000,
  # ajustement probit legitime ». Un verdict rassurant sur un test qui n'a pas
  # eu lieu est pire qu'un silence.
  npar <- if (identical(methode, "em")) 3L else 2L
  informatif <- nrow(d) > npar

  V <- tryCatch(.hstat_dl50_vcov(z, n, a, b, cc, npar), error = function(e) NULL)
  # Le controle porte sur le BLOC ESTIME seulement : quand `c` est declaree,
  # sa ligne vaut NA par construction, et exiger la finitude partout ferait
  # refuser un ajustement parfaitement calculable.
  if (is.null(V) || any(!is.finite(V[1:2, 1:2])))
    return(echec(tr("Matrice d'information singulière : les variances des paramètres ne peuvent pas être calculées. Ajoutez des doses ou augmentez les effectifs.")))

  # Heterogeneite : un ajustement rejete a 5 % signale une dispersion que le
  # modele binomial ne contient pas. Les variances sont alors multipliees par
  # Chi2/ddl et le quantile devient celui de STUDENT. Ne pas le faire
  # publierait des intervalles trop etroits precisement quand le modele est
  # douteux.
  hetero <- isTRUE(informatif && p_chi2 < alpha)
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
    itmax = itmax, chemin = chemin,
    ll0 = ll0, ll1 = ll1, chi2 = chi2, ddl = ddl, p_chi2 = p_chi2,
    heterogene = hetero, facteur = facteur, t = tq, alpha = alpha,
    informatif = informatif, npar = npar,
    temoin_eleve = isTRUE(c_temoin > HSTAT_DL50_TEMOIN_MAX),
    # Declarer une mortalite naturelle nulle alors que le temoin compte des
    # morts est une CONTRADICTION DE SAISIE, pas un resultat. Le calcul se
    # poursuit -- c'est un choix legitime quand on tient la perte du temoin
    # pour accidentelle -- mais il ne doit pas passer inapercu.
    temoin_contredit = isTRUE(identical(methode, "nulle") && x0 > 0),
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
#  DEUX DISPERSIONS, ET ELLES NE DISENT PAS LA MEME CHOSE
#  -------------------------------------------------------
#  Les confondre est l'erreur classique du bioessai, et elle change la
#  conclusion :
#
#   - l'ERREUR-TYPE mesure la PRECISION DE L'ESTIMATION. Elle diminue quand on
#     teste plus d'individus : c'est elle qui fonde les intervalles de
#     confiance, et c'est elle que WIN DL imprime sous le nom « Ecart-type ».
#   - l'ECART-TYPE mesure la DISPERSION DES SENSIBILITES dans la population.
#     Dans le modele probit, les log-tolerances suivent une loi normale de
#     moyenne -a/b et d'ecart-type 1/b (FINNEY, 1971) : il vaut donc 1/b, il
#     est le meme pour toutes les doses letales, et il ne diminue PAS quand on
#     teste plus d'individus. Une population heterogene garde un grand
#     ecart-type meme mesuree parfaitement.
#
#  Verification qui les separe : 10^(log DL50 +/- 1/b) rend exactement la DL84
#  et la DL16 -- l'ecart-type est une propriete de la courbe, pas de l'essai.
# L'unite de dose saisie dans la fiche. Elle etait DEMANDEE puis jamais
# reaffichee : on lisait « DL50 = 0,00552 » sans savoir si c'etait des
# microgrammes par insecte ou des grammes par litre. Une dose sans unite n'est
# pas un resultat.
hstat_dl50_unite <- function(fit_ou_essai) {
  e <- if (inherits(fit_ou_essai, "hstat_dl50_fit")) fit_ou_essai$essai else fit_ou_essai
  u <- trimws(as.character(e$champs$unite %||% ""))
  if (length(u) && !is.na(u) && nzchar(u)) u else ""
}

# « Dose » ou « Dose (µg/insecte) » : le libelle porte l'unite des qu'elle est
# connue, et reste nu sinon.
hstat_dl50_libelle_dose <- function(fit, base = tr("Dose")) {
  u <- hstat_dl50_unite(fit)
  if (nzchar(u)) trf("%s (%s)", base, u) else base
}

hstat_dl50_doses_letales <- function(fit, seuils = HSTAT_DL50_SEUILS) {
  if (!isTRUE(fit$ok)) return(NULL)
  # UN SEUIL SE FILTRE ICI, PAS CHEZ L'APPELANT.
  #
  # Une dose letale a 0 % ou a 100 % n'existe pas : l'inverse normale y vaut
  # l'infini, et la fonction rendait une ligne de `NaN` -- un tableau de
  # resultats qui affiche NaN sans rien dire. Au-dela de 100 % ou en dessous de
  # zero, c'est une faute de frappe, pas une demande.
  #
  # Et une liste VIDE levait « invalid argument to unary operator » : le champ
  # des seuils qu'on efface pour le retaper faisait tomber tout le tableau.
  # L'interface filtrait bien avant d'appeler, mais elle n'est pas le seul
  # appelant -- l'onglet dose/mortalite, le rapport et les tests passent aussi
  # par ici.
  s <- suppressWarnings(as.numeric(seuils))
  garde <- is.finite(s) & s > 0 & s < 100
  ecartes <- s[!garde]
  s <- s[garde]
  if (!length(s)) {
    vide <- data.frame(
      Seuil = numeric(0), Log_dose = numeric(0), Dose = numeric(0),
      Erreur_type = numeric(0), Ecart_type = numeric(0),
      DL_erreur_type = character(0), DL_ecart_type = character(0),
      Limite_inf = numeric(0), Limite_sup = numeric(0),
      Intervalle = character(0), Position = character(0),
      stringsAsFactors = FALSE)
    attr(vide, "message") <- tr("Aucun seuil exploitable : une dose létale se demande strictement entre 0 % et 100 % de mortalité.")
    return(vide)
  }
  seuils <- s
  # L'ETENDUE REELLEMENT TESTEE. La droite de Henry se prolonge a l'infini, la
  # population testee non : une DL90 quatre fois au-dessus de la plus forte
  # dose appliquee repose entierement sur l'hypothese de linearite du probit,
  # la ou plus aucune observation ne vient la contraindre. Sur l'essai de
  # reference, DEUX des trois doses letales publiees sont dans ce cas -- et
  # rien ne les distinguait de la DL50, seule interpolee.
  etendue <- range(fit$essai$doses$dose, finite = TRUE)
  V <- fit$Vh; a <- fit$a; b <- fit$b; tq <- fit$t
  vaa <- V[1, 1]; vbb <- V[2, 2]; vab <- V[1, 2]
  g <- tq^2 * vbb / b^2
  sigma <- if (is.finite(b) && b != 0) 1 / abs(b) else NA_real_
  bornes <- function(m, d) {
    if (!is.finite(d)) return(NA_character_)
    sprintf("%s – %s", formatC(10^(m - d), format = "g", digits = 4),
            formatC(10^(m + d), format = "g", digits = 4))
  }
  out <- lapply(seuils, function(s) {
    # Meme inverse normale que WIN DL : c'est ICI qu'elle se voit.
    y <- .hstat_dl50_qnorm(s / 100)
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
    data.frame(Seuil = s, Log_dose = m, Dose = 10^m,
               Erreur_type = se, Ecart_type = sigma,
               DL_erreur_type = bornes(m, se),
               DL_ecart_type = bornes(m, sigma),
               Limite_inf = 10^lo, Limite_sup = 10^hi,
               Intervalle = meth,
               Position = if (!is.finite(10^m)) NA_character_
                          else if (10^m < etendue[1] || 10^m > etendue[2])
                            tr("extrapolée") else tr("interpolée"),
               stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, out)
  res <- res[order(-res$Seuil), , drop = FALSE]
  rownames(res) <- NULL
  if (length(ecartes))
    attr(res, "ecartes") <- trf("Seuil(s) écarté(s) : %s. Une dose létale se demande strictement entre 0 %% et 100 %% de mortalité.",
                                paste(trimws(formatC(ecartes, format = "g", digits = 4)),
                                      collapse = ", "))
  attr(res, "g") <- g
  attr(res, "etendue") <- etendue
  hors <- res$Seuil[!is.na(res$Position) & res$Position == tr("extrapolée")]
  if (length(hors))
    # `formatC` aligne sur une largeur commune et laisse des espaces de tete :
    # « 0.00063 à  0.03 » se lit comme une coquille. Et chaque seuil porte son
    # propre « DL » -- « DL90, 10 » se lit comme une dose de 10.
    attr(res, "avertissement") <- trf(
      "Dose(s) létale(s) hors de l'étendue testée (%s à %s) : %s. Elles reposent sur le prolongement de la droite, là où aucune observation ne vient plus la contraindre.",
      trimws(formatC(etendue[1], format = "g", digits = 4)),
      trimws(formatC(etendue[2], format = "g", digits = 4)),
      paste0("DL", hors, collapse = ", "))
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
  etendue <- range(fit$essai$doses$dose, finite = TRUE)
  data.frame(Dose = d, Log_dose = z, Probit_attendu = eta, Erreur_type = se,
             Mortalite = vers_p(eta),
             Limite_inf = vers_p(eta - tq * se),
             Limite_sup = vers_p(eta + tq * se),
             Position = ifelse(d < etendue[1] | d > etendue[2],
                               tr("extrapolée"), tr("interpolée")),
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
#  Le verdict : la phrase que l'on vient chercher
# ---------------------------------------------------------------------------
#  Le bloc de resume annoncait l'equation, la mortalite naturelle, le test
#  d'ajustement et le nombre d'iterations -- mais PAS la dose letale. Le chiffre
#  pour lequel le module existe se trouvait plus bas, dans un tableau de dix
#  colonnes, sans son unite. On ouvre un module de DL50 pour lire une DL50.
#
#  Le seuil retenu est la DL50 quand elle est demandee, sinon la premiere des
#  doses letales calculees : demander « DL20, DL80 » et se voir repondre sur un
#  seuil qu'on n'a pas demande serait pire que ne rien dire.
hstat_dl50_verdict <- function(fit, seuils = HSTAT_DL50_SEUILS) {
  if (!isTRUE(fit$ok)) return(NULL)
  dl <- hstat_dl50_doses_letales(fit, seuils)
  if (is.null(dl) || !nrow(dl)) return(NULL)
  i <- which(dl$Seuil == 50)
  if (!length(i)) i <- 1L else i <- i[1]
  u <- hstat_dl50_unite(fit)
  nb <- function(v) trimws(formatC(v, format = "g", digits = 4))
  list(
    seuil = dl$Seuil[i],
    libelle = trf("DL%g", dl$Seuil[i]),
    dose = dl$Dose[i], unite = u,
    limite_inf = dl$Limite_inf[i], limite_sup = dl$Limite_sup[i],
    intervalle = dl$Intervalle[i], position = dl$Position[i],
    extrapolee = identical(dl$Position[i], tr("extrapolée")),
    texte = trf("DL%g = %s%s   [%s ; %s]", dl$Seuil[i], nb(dl$Dose[i]),
                if (nzchar(u)) paste0(" ", u) else "",
                nb(dl$Limite_inf[i]), nb(dl$Limite_sup[i])),
    ajustement = if (!isTRUE(fit$informatif))
        tr("Ajustement non testable : autant de doses que de paramètres estimés, il ne reste aucun degré de liberté résiduel.")
      else switch(hstat_p_verdict(fit$p_chi2),
        significatif = trf("Ajustement rejeté à 5 %% (Chi-2 = %.3f, ddl = %d, p = %.4f) : facteur d'hétérogénéité %.3f appliqué aux variances.",
                           fit$chi2, fit$ddl, fit$p_chi2, fit$facteur),
        `non significatif` = trf("Ajustement probit légitime (Chi-2 = %.3f, ddl = %d, p = %.4f).",
                                 fit$chi2, fit$ddl, fit$p_chi2),
        tr("Test d'ajustement non calculable.")),
    alertes = c(
      if (isTRUE(fit$temoin_eleve))
        trf("Mortalité du témoin élevée (%.1f %%) : au-delà de %.0f %%, la correction d'Abbott devient peu fiable et l'usage veut qu'on refasse l'essai.",
            100 * fit$c_temoin, 100 * HSTAT_DL50_TEMOIN_MAX),
      if (isTRUE(fit$temoin_contredit))
        trf("Mortalité naturelle déclarée nulle alors que le témoin en compte %.1f %% : les doses sont analysées sans correction, et l'ajustement est jugé sur elles seules. Choisissez Abbott ou EM pour tenir compte du témoin.",
            100 * fit$c_temoin),
      if (identical(dl$Position[i], tr("extrapolée")))
        trf("Cette dose est hors de l'étendue testée (%s à %s) : elle repose sur le prolongement de la droite.",
            nb(attr(dl, "etendue")[1]), nb(attr(dl, "etendue")[2])),
      attr(dl, "avertissement")))
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
  # DEUX VRAISEMBLANCES NE SE SOUSTRAIENT QUE SI ELLES PORTENT SUR LES MEMES
  # DONNEES. `ll0` ne convient pas ici : depuis que le temoin est retire de la
  # vraisemblance quand c est DECLAREE, celui d'EM le contient et celui
  # d'Abbott non. La difference chargeait alors le terme du temoin, ressortait
  # negative, et `max(0, .)` la ramenait a zero -- p = 1, « les deux
  # estimations concordent », QUELLES QUE SOIENT LES DONNEES. Le verdict
  # devenait constant sans que rien ne le signale.
  #
  # Or le temoin est precisement la donnee qui separe les deux modeles : c'est
  # lui qui dit ou est la mortalite naturelle. Il entre donc des deux cotes.
  # Abbott est alors EM contraint a c = x0/n0, le rapport de vraisemblance est
  # bien emboite, et la difference est positive par construction -- EM
  # maximise exactement cet objectif-la.
  ll_complet <- function(f) {
    dd <- f$essai$doses
    hstat_dl50_logvrais(dd$x, dd$n, f$table$Mortalite_attendue) +
      if (f$essai$n0 > 0) hstat_dl50_logvrais(f$essai$x0, f$essai$n0, f$c) else 0
  }
  chi2 <- max(0, 2 * (ll_complet(em) - ll_complet(ab)))
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
  # Le HESSIEN sert au rapport de puissance : c'est lui qui donne les variances
  # et les covariances de (a_i, b) sous le modele a pente commune, dont
  # l'intervalle de FIELLER du rapport a besoin. Le demander ici coute une
  # evaluation de plus et evite de reecrire l'information de Fisher pour le cas
  # multi-essais.
  op <- tryCatch(stats::optim(par0, nll, method = "L-BFGS-B", lower = lo,
                              upper = hi, hessian = TRUE,
                              control = list(maxit = 1000, factr = 1e5)),
                 error = function(e) NULL)
  # LE CODE DE CONVERGENCE SE LIT. Une optimisation qui n'aboutit pas rendait
  # malgre tout `ok = TRUE` : si le modele contraint ressortait alors au-dessus
  # du modele libre -- impossible pour des modeles emboites -- le Chi-2 negatif
  # etait ecrase a zero et le test concluait « non significatif ». L'echec se
  # presentait comme une absence de difference, ce qui est exactement la
  # conclusion inverse.
  if (is.null(op) || !is.finite(op$value) || op$value >= 1e99 ||
      !identical(as.integer(op$convergence), 0L))
    return(list(ok = FALSE, ll = NA_real_, npar = na + nb + nc))
  # `nll` est l'OPPOSE de la log-vraisemblance : son hessien est donc
  # directement la matrice d'information observee, et son inverse la matrice
  # de variances. Une inversion qui echoue ne rend pas un objet a moitie
  # valide -- elle rend NULL, et l'appelant s'en garde.
  V <- tryCatch(solve(op$hessian), error = function(e) NULL)
  if (!is.null(V) && any(!is.finite(V))) V <- NULL
  list(ok = TRUE, ll = -op$value, npar = na + nb + nc, par = decoupe(op$par),
       V = V, na = na, nb = nb, nc = nc, k = k,
       a_commun = a_commun, b_commun = b_commun)
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
  # L'INVARIANT D'EMBOITEMENT : le modele contraint ne peut pas mieux ajuster
  # que le modele libre. S'il le fait, c'est l'optimisation qui a echoue, et le
  # `max(0, ...)` d'en dessous transformerait cet echec en « non significatif ».
  # On refuse le test plutot que de le rendre rassurant.
  emboite <- isTRUE(m0$ok) && isTRUE(m1$ok) &&
             is.finite(m0$ll) && is.finite(m1$ll) &&
             m0$ll <= m1$ll + 1e-6
  if (!isTRUE(m0$ok) || !isTRUE(m1$ok) || ddl <= 0 || !emboite)
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
  # LA LIMITE DE CENT DOSES VAUT AUSSI POUR LE TOTAL. Le manuel l'ecrit a part
  # de la limite par essai : « dans le module de comparaison, le regroupement
  # de doses ne peut pas exceder 100 au total ». Six essais de quatre-vingts
  # doses passaient donc un a un et depassaient ensemble sans que rien ne le
  # dise -- la comparaison partait, et elle n'aurait eu aucun equivalent dans
  # le logiciel.
  total <- sum(vapply(essais, function(e) nrow(e$doses), integer(1)))
  if (total > HSTAT_DL50_DOSES_MAX)
    return(vide(trf("Les %d essais totalisent %d doses : la limite du module de comparaison est de %d, comme dans WIN DL.",
                    length(essais), total, HSTAT_DL50_DOSES_MAX)))
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
#  RAPPORT DE PUISSANCE -- le ratio de resistance
# ---------------------------------------------------------------------------
#  C'est le chiffre que publie la surveillance des resistances : combien de
#  fois faut-il plus de produit pour tuer la souche etudiee que la souche de
#  reference. Toutes les pieces etaient la ; il manquait de les assembler.
#
#      R = DL50(essai) / DL50(reference) = 10^((a_ref - a_essai) / b)
#
#  IL N'EXISTE QU'A PENTE COMMUNE, et c'est tout le sujet. Si les droites ne
#  sont pas paralleles, le rapport change avec le niveau de mortalite : il vaut
#  3 a la DL50 et 12 a la DL90, et publier « R = 3 » revient alors a choisir un
#  chiffre parmi d'autres sans le dire. Le test de parallelisme est donc
#  calcule AVANT et rendu AVEC -- pas en option, pas plus bas dans la page.
#
#  L'intervalle passe par FIELLER, comme celui des doses letales, parce que
#  c'est encore un rapport de deux estimateurs normaux : le numerateur
#  a_ref - a_essai, le denominateur b, tous deux tires du MEME ajustement a
#  pente commune -- d'ou la covariance, qu'un calcul essai par essai ignorerait.
hstat_dl50_puissance <- function(essais, reference = 1,
                                 scenario = c("heterogene", "homogene", "nulle"),
                                 alpha = 0.05) {
  scenario <- match.arg(scenario)
  vide <- function(motif) {
    r <- data.frame()
    attr(r, "message") <- motif
    r
  }
  if (length(essais) < 2L)
    return(vide(tr("Le rapport de puissance demande au moins deux essais : c'est un rapport.")))
  if (length(essais) > HSTAT_DL50_ESSAIS_MAX)
    return(vide(trf("Comparaison limitée à %d essais, comme dans WIN DL.",
                    HSTAT_DL50_ESSAIS_MAX)))
  for (e in essais) {
    m <- .hstat_dl50_valide(e)
    if (!is.null(m)) return(vide(m))
  }
  k <- length(essais)
  # UNE REFERENCE HORS BORNES SE REFUSE, ELLE NE SE REMPLACE PAS. Elle
  # retombait sur le premier essai : demander le rapport par rapport a l'essai
  # 9 sur un jeu qui en compte deux rendait le tableau du premier, avec sa
  # colonne « Reference » cochee sur lui -- un resultat plausible, et pas celui
  # qu'on avait demande. C'est le defaut le plus difficile a voir, parce que
  # rien n'est vide et rien ne leve.
  ref <- suppressWarnings(as.integer(reference)[1])
  if (!isTRUE(is.finite(ref)) || ref < 1L || ref > k)
    return(vide(trf("Essai de référence hors bornes (%s) : il y en a %d.",
                    paste(as.character(reference)[1]), k)))
  cm <- switch(scenario, heterogene = "libre", homogene = "commun", nulle = "nul")
  if (identical(scenario, "nulle") &&
      any(vapply(essais, function(e) e$n0 > 0 && e$x0 > 0, logical(1))))
    return(vide(tr("Au moins un essai présente une mortalité naturelle observée : le scénario à mortalité nulle n'est pas applicable.")))

  # Le modele a PENTE COMMUNE : c'est lui qui definit le rapport.
  m_par <- .hstat_dl50_fit_multi(essais, FALSE, TRUE, cm)
  # Et le modele a pentes libres, pour tester le parallelisme.
  m_lib <- .hstat_dl50_fit_multi(essais, FALSE, FALSE, cm)
  if (!isTRUE(m_par$ok))
    return(vide(tr("L'ajustement à pente commune a échoué : le rapport de puissance n'est pas calculable.")))
  if (is.null(m_par$V))
    return(vide(tr("Matrice de variances non inversible sous le modèle à pente commune : le rapport se calcule, pas son intervalle. Augmentez les effectifs ou le nombre de doses.")))

  para <- .hstat_dl50_ligne_test(
    tr("Parallélisme des droites (b identiques)"), m_par, m_lib,
    m_lib$npar - m_par$npar, alpha,
    tr("Test significatif : les droites ne sont pas parallèles."),
    tr("Test non significatif : les droites peuvent être considérées comme parallèles."))

  # `unname()` : ordonnees et pente sortent du vecteur de parametres, ou elles
  # portent un nom. Ce nom se propage silencieusement au rapport, a ses bornes
  # et a l'attribut `pente_commune`, ou il ne veut plus rien dire -- et il suffit
  # a faire echouer toute comparaison qui compare aussi les noms.
  a <- unname(m_par$par$a)
  b <- unname(m_par$par$b[1])
  V <- m_par$V
  # Ordre des parametres dans `optim` : les a_i (k valeurs), puis b, puis les c.
  ib <- k + 1L
  tq <- stats::qnorm(1 - alpha / 2)
  nom <- names(essais) %||% vapply(essais, function(e)
    if (nzchar(e$titre)) e$titre else tr("Essai"), character(1))
  nom <- make.unique(nom, sep = " ")

  lig <- lapply(seq_len(k), function(i) {
    if (i == ref)
      return(data.frame(Essai = nom[i], Reference = TRUE,
                        Rapport = 1, Limite_inf = NA_real_, Limite_sup = NA_real_,
                        Intervalle = NA_character_, stringsAsFactors = FALSE))
    # log10(R) = (a_ref - a_i) / b
    N <- a[ref] - a[i]
    vN <- V[ref, ref] + V[i, i] - 2 * V[ref, i]
    vD <- V[ib, ib]
    cND <- V[ref, ib] - V[i, ib]
    m <- N / b
    v <- (vN + m^2 * vD - 2 * m * cND) / b^2
    se <- sqrt(max(v, 0))
    g <- tq^2 * vD / b^2
    dis <- b^2 * v - g * (vN - cND^2 / vD)
    if (is.finite(g) && g < 1 && is.finite(dis) && dis >= 0) {
      centre <- (m - g * cND / vD) / (1 - g)
      demi <- (tq / (abs(b) * (1 - g))) * sqrt(dis)
      lo <- centre - demi; hi <- centre + demi; meth <- "Fieller"
    } else {
      lo <- m - tq * se; hi <- m + tq * se; meth <- "delta"
    }
    data.frame(Essai = nom[i], Reference = FALSE,
               Rapport = 10^m, Limite_inf = 10^lo, Limite_sup = 10^hi,
               Intervalle = meth, stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, lig)

  # La DL50 propre a chaque essai, pour situer le rapport. Elle vient de
  # l'ajustement INDIVIDUEL : le rapport, lui, vient du modele a pente commune.
  # Les deux ne coincident que si les droites sont effectivement paralleles --
  # c'est encore une facon de voir ce que le test dit.
  res$DL50 <- vapply(essais, function(e) {
    f <- hstat_dl50_ajuste(e, if (identical(cm, "nul")) "nulle" else "em")
    if (!isTRUE(f$ok)) return(NA_real_)
    d <- hstat_dl50_doses_letales(f, 50)
    if (is.null(d) || !nrow(d)) NA_real_ else d$Dose[1]
  }, numeric(1))
  res <- res[, c("Essai", "Reference", "DL50", "Rapport", "Limite_inf",
                 "Limite_sup", "Intervalle")]
  rownames(res) <- NULL

  attr(res, "reference") <- nom[ref]
  attr(res, "parallelisme") <- para
  attr(res, "scenario") <- scenario
  attr(res, "pente_commune") <- b
  v_para <- if (is.na(para$p)) "indeterminable" else hstat_p_verdict(para$p, alpha)
  attr(res, "avertissement") <- switch(v_para,
    significatif = tr("Les droites ne sont pas parallèles : le rapport de puissance change avec le niveau de mortalité, et le chiffre ci-dessous ne vaut qu'à la DL50. Comparez les doses létales seuil par seuil plutôt que par un rapport unique."),
    `non significatif` = NULL,
    tr("Le test de parallélisme n'est pas calculable : le rapport ci-dessous suppose des droites parallèles sans que rien ne le confirme."))
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

#  UN FICHIER DE WIN DL SE LIT EN OCTETS, PAS EN LIGNES DE TEXTE.
#
#  Trois raisons, et chacune casse une lecture naive :
#
#  1. les separateurs de champ de la premiere ligne sont les octets 0x00 a
#     0x09 -- `readLines()` s'arrete sur le premier zero (« nul character not
#     allowed ») et perdrait l'en-tete entier ;
#  2. `rawToChar()` sur l'ensemble rendrait une chaine invalide dans la locale
#     courante (les accents du CP437 ne sont pas de l'UTF-8) et `strsplit()`
#     refuserait de travailler dessus : le decoupage en lignes se fait donc
#     aussi sur les octets ;
#  3. le marqueur de fin de fichier est l'OCTET 0xDB. Le manuel l'ecrit « Û »
#     -- c'est son rendu en CP1252 ; en CP437, celui du fichier, c'est « █ ».
#     Le retirer par son apparence textuelle ne marche donc pas, et la liste
#     relue portait un element de plus, invisible a la lecture du code. On
#     tronque l'octet.
.hstat_dl50_lignes <- function(chemin) {
  taille <- tryCatch(file.size(chemin), error = function(e) NA_real_)
  if (!isTRUE(is.finite(taille)) || taille <= 0) return(character(0))
  oct <- tryCatch(readBin(chemin, "raw", n = taille), error = function(e) NULL)
  if (is.null(oct) || !length(oct)) return(character(0))
  fin_fichier <- which(oct == as.raw(219L))
  if (length(fin_fichier)) {
    if (fin_fichier[1] == 1L) return(character(0))
    oct <- oct[seq_len(fin_fichier[1] - 1L)]
  }
  garde_lf <- oct == as.raw(10L)
  oct[oct <= as.raw(9L)] <- as.raw(31L)          # 0x00 à 0x09 -> séparateur
  oct[oct == as.raw(13L)] <- as.raw(32L)         # CR -> espace
  oct[garde_lf] <- as.raw(10L)
  fins <- which(oct == as.raw(10L))
  deb <- c(1L, fins + 1L); fin <- c(fins - 1L, length(oct))
  brut <- vapply(seq_along(deb), function(i) {
    if (fin[i] < deb[i] || deb[i] > length(oct)) "" else rawToChar(oct[deb[i]:fin[i]])
  }, character(1))
  trimws(.hstat_dl50_decode(brut), which = "right")
}

hstat_dl50_lire_windl <- function(chemin) {
  echec <- function(motif) list(ok = FALSE, message = motif)
  L <- .hstat_dl50_lignes(chemin)
  if (!length(L)) return(echec(tr("Fichier vide ou illisible.")))
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
  writeBin(c(charToRaw(paste0(
    paste(iconv(suite, "UTF-8", "CP437", sub = "?"), collapse = "\r\n"), "\r\n")),
    as.raw(219L), charToRaw("\r\n")), con)   # 0xDB : marqueur de fin de WIN DL
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
  # Une valeur sans objet s'ecrit « - », pas « NA » : sous Abbott et sous
  # mortalite nulle, `c` est declaree et n'a donc ni ecart-type ni covariance.
  sc <- function(v) ifelse(is.finite(v), formatC(v, format = "e", digits = 5), "-")
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
      trf("  ** Convergence atteinte après %d itérations de HStat (écart absolu sur log-vraisemblance < 1e-5 ; le compte dépend de l'algorithme et ne se compare pas à celui de WIN DL)",
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
    paste(c("", tr("log.Dose"), tr("Dose"), tr("Erreur-type"), tr("Écart-type"),
            tr("DL ± erreur-type"), tr("DL ± écart-type"), tr("Limite inf."),
            tr("Limite sup."), tr("Intervalle")), collapse = "\t"),
    vapply(seq_len(nrow(dl)), function(i) paste(c(
      trf("DL %g", dl$Seuil[i]), sc(dl$Log_dose[i]), sc(dl$Dose[i]),
      sc(dl$Erreur_type[i]), sc(dl$Ecart_type[i]),
      dl$DL_erreur_type[i], dl$DL_ecart_type[i],
      sc(dl$Limite_inf[i]), sc(dl$Limite_sup[i]),
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
#  Coller trois colonnes venues d'un tableur
# ---------------------------------------------------------------------------
#  Les comptages existent deja dans un classeur : les retaper cellule par
#  cellule dans une table qui ne dit meme pas qu'elle est modifiable est la
#  friction la plus quotidienne du module.
#
#  LE PIEGE EST LA VIRGULE, et il est le meme que dans le module de nettoyage.
#  Un tableur francais copie « 0,00063 » avec des TABULATIONS entre colonnes :
#  la virgule y est une decimale. Un CSV anglais copie « 0.00063,25,5 » : la
#  virgule y est un separateur. On ne peut pas lui donner les deux roles, alors
#  on regarde d'abord s'il existe un separateur NON ambigu -- tabulation ou
#  point-virgule. S'il y en a un, la virgule est une decimale. Sinon seulement,
#  elle separe.
#
#  Une ligne d'en-tete est reconnue a ce qu'elle ne contient aucun nombre : la
#  jeter en silence vaut mieux que de rendre une premiere dose absurde, et la
#  garder obligerait a demander « votre collage a-t-il un en-tete ? » pour un
#  fait que le texte porte deja.
hstat_dl50_coller <- function(txt) {
  echec <- function(motif) list(ok = FALSE, message = motif)
  if (is.null(txt) || !length(txt) || !nzchar(trimws(paste(txt, collapse = "")))) 
    return(echec(tr("Rien à coller : copiez trois colonnes (dose, effectif testé, morts) depuis votre tableur.")))
  lignes <- unlist(strsplit(paste(txt, collapse = "\n"), "[\r\n]+"))
  lignes <- trimws(lignes)
  lignes <- lignes[nzchar(lignes)]
  if (!length(lignes)) return(echec(tr("Rien à coller : copiez trois colonnes (dose, effectif testé, morts) depuis votre tableur.")))

  franc <- any(grepl("[\t;]", lignes))
  motif <- if (franc) "[\t;]+" else "[\t;,[:space:]]+"
  decoupe <- function(li) {
    v <- trimws(strsplit(li, motif)[[1]])
    v <- v[nzchar(v)]
    if (franc) v <- gsub(",", ".", v, fixed = TRUE)
    suppressWarnings(as.numeric(v))
  }
  # L'en-tete ne porte aucun nombre : c'est a cela qu'on le reconnait.
  if (length(lignes) && all(!is.finite(decoupe(lignes[1])))) lignes <- lignes[-1]
  if (!length(lignes))
    return(echec(tr("Le collage ne contient que des en-têtes : il manque les lignes de données.")))

  mat <- lapply(lignes, decoupe)
  larg <- vapply(mat, length, integer(1))
  if (any(larg < 3L))
    return(echec(trf("Ligne(s) à moins de trois colonnes : %s. Il faut la dose, l'effectif testé et le nombre de morts.",
                     paste(which(larg < 3L), collapse = ", "))))
  d <- data.frame(
    Dose     = vapply(mat, `[`, numeric(1), 1),
    Effectif = vapply(mat, `[`, numeric(1), 2),
    Morts    = vapply(mat, `[`, numeric(1), 3),
    stringsAsFactors = FALSE)
  if (any(!is.finite(as.matrix(d))))
    return(echec(tr("Valeur illisible dans le collage : vérifiez qu'il n'y a ni texte ni cellule vide dans les trois premières colonnes.")))
  list(ok = TRUE, table = d, lignes = nrow(d),
       decimale = if (franc) "," else ".")
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

# =============================================================================
#  LISTES DEROULANTES ET SELECTION MULTI-CRITERES
# =============================================================================
#  Deux pieces de WIN DL qui n'ont l'air de rien et qui se tiennent l'une
#  l'autre : les listes existent POUR que la selection fonctionne.
#
#  Le manuel le dit sans detour : « il est conseille pour les ajouts dans les
#  listes de ne pas utiliser des orthographes differentes pour decrire un meme
#  element car la selection de fichiers serait inefficace ». Deux essais notes
#  « Cyfluthrine » et « cyfluthrine » ne se retrouvent jamais ensemble ; le
#  vocabulaire controle est ce qui l'evite.
# =============================================================================

HSTAT_DL50_LISTES <- c(
  auteur   = "Auteur", espece = "Espèce", stade = "Stade", duree = "Durée",
  matiere  = "Matière active", methode = "Méthode de traitement",
  unite    = "Unité de dose")

# Les champs de la fiche alimentes par une liste. La date, la temperature et
# le ratio restent libres : ce sont des valeurs, pas un vocabulaire.
HSTAT_DL50_CHAMPS_LISTE <- c(auteur = "auteur", espece = "espece",
                             stade = "stade", duree = "duree",
                             matiere1 = "matiere", matiere2 = "matiere",
                             methode = "methode", unite = "unite")

# Lecture d'un fichier de liste (AUTEUR.TXT, ESPECE.TXT, MATACT.TXT...).
# Meme encodage et meme marqueur de fin que les fichiers d'essai. Le manuel
# avertit de ne pas les editer a la main pour cette raison precise : sans le
# marqueur, WIN DL ne sait plus ou la liste s'arrete.
hstat_dl50_liste_lire <- function(chemin) {
  v <- trimws(.hstat_dl50_lignes(chemin))
  unique(v[nzchar(v)])
}

hstat_dl50_liste_ecrire <- function(valeurs, chemin) {
  v <- iconv(as.character(valeurs), "UTF-8", "CP437", sub = "?")
  con <- file(chemin, open = "wb")
  on.exit(close(con))
  if (length(v))
    writeBin(charToRaw(paste0(paste(v, collapse = "\r\n"), "\r\n")), con)
  writeBin(c(as.raw(219L), charToRaw("\r\n")), con)   # 0xDB : fin de liste
  invisible(chemin)
}

# « Si l'element existe deja dans la liste, il ne sera pas ajoute afin d'eviter
# les doublons. » La comparaison ignore la casse et les espaces de bord :
# « Cyfluthrine » et « cyfluthrine  » sont le meme produit, et les garder tous
# deux rendrait la selection inefficace -- exactement ce que la liste evite.
hstat_dl50_liste_ajouter <- function(liste, valeur) {
  v <- trimws(as.character(valeur))
  v <- v[nzchar(v)]
  if (!length(v)) return(liste)
  for (x in v)
    if (!(tolower(x) %in% tolower(liste))) liste <- c(liste, x)
  sort(liste)
}

hstat_dl50_liste_retirer <- function(liste, valeur) {
  v <- tolower(trimws(as.character(valeur)))
  liste[!(tolower(liste) %in% v)]
}

# « Regenerer » : le programme parcourt les essais disponibles et ajoute les
# elements nouveaux. C'est ce qui permet de reprendre un fonds d'essais deja
# constitue sans ressaisir son vocabulaire.
hstat_dl50_regenerer <- function(listes, essais) {
  if (!length(essais)) return(listes)
  for (nm in names(HSTAT_DL50_LISTES)) {
    champs <- names(HSTAT_DL50_CHAMPS_LISTE)[HSTAT_DL50_CHAMPS_LISTE == nm]
    vals <- unlist(lapply(essais, function(e)
      vapply(champs, function(cn) trimws(as.character(e$champs[[cn]] %||% "")),
             character(1))), use.names = FALSE)
    listes[[nm]] <- hstat_dl50_liste_ajouter(listes[[nm]] %||% character(0), vals)
  }
  listes
}

hstat_dl50_listes_vides <- function()
  stats::setNames(rep(list(character(0)), length(HSTAT_DL50_LISTES)),
                  names(HSTAT_DL50_LISTES))

# ---------------------------------------------------------------------------
#  Selection multi-criteres
# ---------------------------------------------------------------------------
#  Un critere VIDE ne filtre pas. C'est la difference entre « je ne demande
#  rien sur l'espece » et « je demande une espece qui n'existe pas » : traiter
#  le premier comme le second ne rendrait jamais aucun essai, et l'utilisateur
#  conclurait que son fonds est vide.
#
#  La temperature suit le manuel : une valeur -> egalite, deux valeurs ->
#  intervalle. Les bornes sont remises dans l'ordre plutot que de rendre zero
#  essai sur une inversion de saisie, qui n'apprend rien a personne.
hstat_dl50_selection <- function(essais, criteres = list()) {
  if (!length(essais)) return(character(0))
  nom <- names(essais) %||% as.character(seq_along(essais))
  garde <- rep(TRUE, length(essais))

  txt <- function(e, champ) trimws(as.character(e$champs[[champ]] %||% ""))
  for (champ in c("auteur", "espece", "stade", "duree", "methode", "unite")) {
    ref <- trimws(as.character(criteres[[champ]] %||% character(0)))
    ref <- ref[nzchar(ref)]
    if (!length(ref)) next
    garde <- garde & vapply(essais, function(e)
      tolower(txt(e, champ)) %in% tolower(ref), logical(1))
  }

  ma1 <- trimws(as.character(criteres$matiere1 %||% character(0)))
  ma1 <- ma1[nzchar(ma1)]
  if (length(ma1))
    garde <- garde & vapply(essais, function(e)
      tolower(txt(e, "matiere1")) %in% tolower(ma1), logical(1))

  # La seconde matiere active et le ratio ne servent QUE si l'on trie sur les
  # deux : c'est la case « (MA1) ou (MA1 et MA2) » du logiciel. Sans elle, un
  # essai a une seule matiere active serait ecarte par un critere qui ne le
  # concerne pas.
  if (isTRUE(criteres$avec_ma2)) {
    ma2 <- trimws(as.character(criteres$matiere2 %||% character(0)))
    ma2 <- ma2[nzchar(ma2)]
    if (length(ma2))
      garde <- garde & vapply(essais, function(e)
        tolower(txt(e, "matiere2")) %in% tolower(ma2), logical(1))
    rat <- trimws(as.character(criteres$ratio %||% ""))[1]
    if (length(rat) && !is.na(rat) && nzchar(rat))
      garde <- garde & vapply(essais, function(e)
        identical(txt(e, "ratio"), rat), logical(1))
  }

  tp <- suppressWarnings(as.numeric(criteres$temperature %||% numeric(0)))
  tp <- tp[is.finite(tp)]
  if (length(tp)) {
    val <- vapply(essais, function(e)
      suppressWarnings(as.numeric(gsub(",", ".", txt(e, "temperature"),
                                       fixed = TRUE))), numeric(1))
    garde <- garde & if (length(tp) == 1L) {
      !is.na(val) & val == tp
    } else {
      b <- sort(tp[1:2])
      !is.na(val) & val >= b[1] & val <= b[2]
    }
  }
  nom[garde]
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
#
#  TOUS LES REGLAGES PASSENT PAR `opt`, une liste nommee. Un argument par
#  reglage ferait une signature de quarante lignes ou personne ne verrait
#  qu'il en manque un ; une liste se complete, se transmet et se teste d'un
#  bloc. `.hstat_dl50_opt()` donne la valeur par defaut de chaque cle : un
#  appel qui n'en passe aucune rend le meme graphique qu'avant.
# ---------------------------------------------------------------------------

HSTAT_DL50_FORMES <- c("Cercle plein" = "16", "Carré plein" = "15",
                       "Triangle plein" = "17", "Losange plein" = "18",
                       "Cercle vide" = "1", "Carré vide" = "0",
                       "Triangle vide" = "2", "Croix" = "4")

HSTAT_DL50_TRAITS <- c("Continu" = "solid", "Tirets" = "dashed",
                       "Pointillé" = "dotted", "Tiret-point" = "dotdash",
                       "Tirets longs" = "longdash", "Deux tirets" = "twodash")

HSTAT_DL50_LEGENDE <- c("À droite" = "right", "À gauche" = "left",
                        "En haut" = "top", "En bas" = "bottom",
                        "Aucune" = "none")

HSTAT_DL50_POS <- c("À gauche" = "0", "Au centre" = "0.5", "À droite" = "1")

# Deux lectures du meme ajustement, et ce ne sont pas deux habillages : la
# droite de Henry montre le MODELE (le probit est lineaire en log-dose, c'est
# ce qui se verifie a l'oeil), la courbe dose-reponse montre la REPONSE telle
# qu'on l'a mesuree -- une sigmoide bornee par la mortalite naturelle en bas et
# par 100 % en haut. C'est celle-la qu'un rapport d'essai publie, parce qu'elle
# se lit sans savoir ce qu'est un probit.
HSTAT_DL50_GRAPHES <- c(
  "Droite de Henry (probit)"    = "probit",
  "Courbe dose-réponse (%)"     = "reponse")

HSTAT_DL50_OPT_DEFAUT <- list(
  type = "probit",
  points = TRUE, courbe = FALSE, droite = TRUE, bande = TRUE, reperes = TRUE,
  titre = "", sous_titre = "", xlab = "", ylab = "", ylab2 = "",
  titre_taille = 15, titre_style = "bold", titre_pos = 0.5,
  sous_titre_taille = 12, sous_titre_style = "italic", sous_titre_pos = 0.5,
  axe_titre_taille = 12, axe_titre_style = "plain",
  grad_x_taille = 10, grad_x_style = "plain", grad_x_angle = 0,
  grad_y_taille = 10, grad_y_style = "plain",
  legende_pos = "right", legende_taille = 10, legende_titre_taille = 11,
  legende_titre = "",
  point_taille = 2.4, point_forme = "16", point_opacite = 1,
  droite_epaisseur = 0.9, droite_type = "solid",
  courbe_epaisseur = 0.6, courbe_type = "dashed",
  bande_opacite = 0.12,
  repere_couleur = "#6b7280", repere_type = "dotted", repere_epaisseur = 0.5,
  repere_etiquette = TRUE,
  theme = "minimal", base_size = 12, palette = "Set1",
  couleur = "#2e86c1", grille = TRUE, axe2 = TRUE,
  x_min = NA_real_, x_max = NA_real_, y_min = NA_real_, y_max = NA_real_)

.hstat_dl50_opt <- function(opt = list()) {
  o <- HSTAT_DL50_OPT_DEFAUT
  for (nm in intersect(names(opt), names(o)))
    if (!is.null(opt[[nm]])) o[[nm]] <- opt[[nm]]
  o
}

.hstat_dl50_txt <- function(taille, style, angle = 0, hjust = NULL, couleur = NULL) {
  st <- as.character(style %||% "plain")
  ggplot2::element_text(
    size = max(4, suppressWarnings(as.numeric(taille)[1]) %||% 10),
    face = if (st %in% c("plain", "bold", "italic", "bold.italic")) st else "plain",
    angle = suppressWarnings(as.numeric(angle)[1]) %||% 0,
    hjust = hjust, colour = couleur)
}

hstat_dl50_graphique <- function(fits, opt = list()) {
  o <- .hstat_dl50_opt(opt)
  fits <- Filter(function(f) isTRUE(f$ok), fits)
  if (!length(fits)) return(NULL)
  nom <- vapply(fits, function(f) {
    t <- f$essai$titre
    if (length(t) && nzchar(t)) t else tr("Essai")
  }, character(1))
  nom <- make.unique(nom, sep = " ")
  reponse <- identical(as.character(o$type %||% "probit")[1], "reponse")

  # LE POINT TRACE N'EST PAS LE MEME D'UN GRAPHIQUE A L'AUTRE, et les confondre
  # revient a superposer deux quantites differentes. La droite de Henry porte
  # le probit de la mortalite CORRIGEE : c'est elle que la droite modelise. La
  # courbe dose-reponse porte la mortalite OBSERVEE, parce que la courbe
  # ajustee inclut deja la mortalite naturelle -- y poser les points corriges
  # les descendrait tous de la valeur de c, et l'ecart passerait pour un defaut
  # d'ajustement.
  #
  # UNE MORTALITE CORRIGEE DE 0 % OU DE 100 % N'A PAS DE PROBIT, et c'est la
  # raison la plus concrete d'avoir les deux graphiques. `hstat_dl50_probit()`
  # ramene la proportion dans [1e-12 ; 1 - 1e-12] pour ne pas rendre l'infini :
  # le point ressort alors a +/- 7,03, une valeur qui ne mesure rien -- elle
  # depend de l'epsilon choisi. Et comme l'etendue de l'axe se calcule sur les
  # points, DEUX artefacts suffisent a etirer l'axe de -7,7 a +7,7 : sur un
  # essai a six doses dont une a 0 % et une a 100 %, les quatre points reels
  # occupaient 17 % de la hauteur, la droite et sa bande ecrasees au milieu.
  #
  # Ces points sont donc ecartes de la droite de Henry -- et NOMMES, parce
  # qu'un point qui disparait sans un mot est le defaut que ce depot traque
  # ailleurs. Sur la courbe dose-reponse ils se tracent tous : 0 % et 100 %
  # sont des observations comme les autres, et ce sont justement les doses qui
  # bornent l'essai.
  ecartes <- character(0)
  pts <- do.call(rbind, lapply(seq_along(fits), function(i) {
    t <- fits[[i]]$table
    if (reponse)
      return(data.frame(Essai = nom[i], x = t$Log_dose,
                        y = 100 * t$Mortalite_observee, stringsAsFactors = FALSE))
    ok <- is.finite(t$Mortalite_corrigee) &
          t$Mortalite_corrigee > 0 & t$Mortalite_corrigee < 1
    if (any(!ok))
      ecartes <<- c(ecartes, trf("%s (doses %s)", nom[i], paste(
        trimws(formatC(t$Dose[!ok], format = "g", digits = 4)), collapse = ", ")))
    data.frame(Essai = nom[i], x = t$Log_dose[ok], y = t$Probit_corrige[ok],
               stringsAsFactors = FALSE)
  }))
  pts <- pts[is.finite(pts$y), , drop = FALSE]

  rg <- range(unlist(lapply(fits, function(f) f$table$Log_dose)), finite = TRUE)
  if (!all(is.finite(rg))) return(NULL)
  if (diff(rg) <= 0) rg <- rg + c(-1, 1)
  rg <- rg + c(-1, 1) * diff(rg) * 0.08
  # Les limites de l'axe des doses se saisissent EN DOSES, pas en logarithmes :
  # personne ne raisonne en log10 devant un plan d'essai.
  if (is.finite(o$x_min) && o$x_min > 0) rg[1] <- log10(o$x_min)
  if (is.finite(o$x_max) && o$x_max > 0) rg[2] <- log10(o$x_max)
  if (diff(rg) <= 0) return(NULL)
  grille_x <- seq(rg[1], rg[2], length.out = 200)

  lig <- do.call(rbind, lapply(seq_along(fits), function(i) {
    f <- fits[[i]]
    eta <- f$a + f$b * grille_x
    se <- sqrt(pmax(f$Vh[1, 1] + grille_x^2 * f$Vh[2, 2] +
                      2 * grille_x * f$Vh[1, 2], 0))
    # L'INTERVALLE SE CONSTRUIT SUR LE PROBIT, PUIS SE TRANSPORTE. F est
    # monotone : les bornes restent donc dans [0 ; 100] et l'asymetrie de la
    # sigmoide est respectee. Les construire directement sur le pourcentage
    # les ferait sortir du cadre des que la mortalite approche ses extremes --
    # c'est-a-dire la ou l'on veut justement lire. Meme regle que pour
    # `hstat_dl50_mortalite()`.
    vers <- if (reponse)
      function(e) 100 * (f$c + (1 - f$c) * stats::pnorm(e)) else identity
    data.frame(Essai = nom[i], x = grille_x, y = vers(eta),
               lo = vers(eta - f$t * se), hi = vers(eta + f$t * se),
               stringsAsFactors = FALSE)
  }))

  # La bande d'intervalle n'a de sens que dans la fenetre lisible : sur deux
  # decades, elle atteint des probits de +/- 20 et ecrase tout le reste.
  #
  # Les deux champs de bornes se saisissent EN POURCENTAGE dans les deux cas :
  # c'est ce que l'utilisateur lit sur son essai. Seule la conversion change --
  # la courbe dose-reponse les prend telles quelles, la droite de Henry les
  # passe par qnorm. Zero et cent n'ont pas de probit fini, d'ou les bornes
  # strictes du second cas ; sur la courbe, ce sont au contraire les valeurs
  # naturelles.
  borne <- function(v, defaut) {
    if (!is.finite(v)) return(defaut)
    if (reponse) if (v >= 0 && v <= 100) v else defaut
    else if (v > 0 && v < 100) stats::qnorm(v / 100) else defaut
  }
  ylim <- if (reponse) c(0, 100)
          else range(c(pts$y, stats::qnorm(c(0.001, 0.999))), finite = TRUE)
  ylim <- c(borne(o$y_min, ylim[1]), borne(o$y_max, ylim[2]))
  if (diff(ylim) <= 0) ylim <- range(pts$y, finite = TRUE)

  pc <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)
  multiple <- length(fits) > 1L
  aes_col <- if (multiple) ggplot2::aes(colour = .data$Essai) else NULL

  p <- ggplot2::ggplot()
  if (isTRUE(o$bande)) {
    p <- p + if (multiple)
      ggplot2::geom_ribbon(data = lig,
        ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi,
                     fill = .data$Essai), alpha = o$bande_opacite, colour = NA)
    else ggplot2::geom_ribbon(data = lig,
        ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi),
        alpha = o$bande_opacite, fill = o$couleur, colour = NA)
  }
  if (isTRUE(o$droite)) {
    p <- p + if (multiple)
      ggplot2::geom_line(data = lig,
        ggplot2::aes(x = .data$x, y = .data$y, colour = .data$Essai),
        linewidth = o$droite_epaisseur, linetype = o$droite_type)
    else ggplot2::geom_line(data = lig, ggplot2::aes(x = .data$x, y = .data$y),
        linewidth = o$droite_epaisseur, linetype = o$droite_type,
        colour = o$couleur)
  }
  if (isTRUE(o$courbe) && nrow(pts)) {
    p <- p + if (multiple)
      ggplot2::geom_line(data = pts,
        ggplot2::aes(x = .data$x, y = .data$y, colour = .data$Essai),
        linetype = o$courbe_type, linewidth = o$courbe_epaisseur)
    else ggplot2::geom_line(data = pts, ggplot2::aes(x = .data$x, y = .data$y),
        linetype = o$courbe_type, linewidth = o$courbe_epaisseur,
        colour = o$couleur)
  }
  if (isTRUE(o$points) && nrow(pts)) {
    forme <- suppressWarnings(as.integer(o$point_forme))
    if (!isTRUE(is.finite(forme))) forme <- 16L
    p <- p + if (multiple)
      ggplot2::geom_point(data = pts,
        ggplot2::aes(x = .data$x, y = .data$y, colour = .data$Essai),
        size = o$point_taille, shape = forme, alpha = o$point_opacite)
    else ggplot2::geom_point(data = pts, ggplot2::aes(x = .data$x, y = .data$y),
        size = o$point_taille, shape = forme, alpha = o$point_opacite,
        colour = o$couleur)
  }
  if (isTRUE(o$reperes)) {
    # LA DL50 N'EST PAS A 50 % DE MORTALITE OBSERVEE. Elle est definie sur la
    # mortalite CORRIGEE : la dose a laquelle le produit tue la moitie des
    # individus que le temoin aurait laisses vivants. Sur une courbe de
    # mortalites observees, le repere passe donc a c + (1 - c).s, pas a s.
    #
    # Avec un temoin nul les deux coincident -- ce qui rend l'erreur invisible
    # precisement sur les essais les plus propres, et visible seulement sur
    # ceux ou elle change la lecture.
    cc <- vapply(fits, function(f) f$c, numeric(1))
    yr <- if (reponse) unique(round(100 * (cc[1] + (1 - cc[1]) *
                                             HSTAT_DL50_SEUILS / 100), 10))
          else .hstat_dl50_qnorm(HSTAT_DL50_SEUILS / 100)
    # Plusieurs essais de mortalites naturelles differentes n'ont pas un repere
    # commun : un seul trait en tiendrait lieu et serait faux pour tous sauf un.
    # On s'abstient plutot que d'en tracer un au hasard.
    commun <- !reponse || length(unique(round(cc, 10))) == 1L
    if (commun) {
      p <- p + ggplot2::geom_hline(yintercept = yr, linetype = o$repere_type,
                                   colour = o$repere_couleur,
                                   linewidth = o$repere_epaisseur)
      # L'etiquette est ce qui rend le repere lisible : trois traits
      # horizontaux sans nom obligent a compter les graduations pour savoir
      # lequel est la DL50.
      if (isTRUE(o$repere_etiquette))
        p <- p + ggplot2::annotate(
          "text", x = rg[1], y = yr, label = paste0("DL", HSTAT_DL50_SEUILS),
          hjust = -0.1, vjust = -0.4, size = max(2, o$grad_y_taille / 3),
          colour = o$repere_couleur)
    }
    # La courbe ne part pas de zero quand le temoin meurt : elle part de c.
    # Le dire evite de lire un defaut d'ajustement la ou il y a une mortalite
    # naturelle -- et c est la seule quantite du modele qui se voie a l'oeil.
    if (reponse && commun && cc[1] > 0.005) {
      p <- p + ggplot2::geom_hline(yintercept = 100 * cc[1],
                                   linetype = o$repere_type,
                                   colour = o$repere_couleur,
                                   linewidth = o$repere_epaisseur)
      if (isTRUE(o$repere_etiquette))
        p <- p + ggplot2::annotate(
          "text", x = rg[2], y = 100 * cc[1], label = tr("mortalité naturelle"),
          hjust = 1.05, vjust = -0.4, size = max(2, o$grad_y_taille / 3),
          colour = o$repere_couleur)
    }
  }

  # Sur la courbe dose-reponse, le pourcentage EST l'axe principal : un second
  # axe en probit y placerait l'infini a 0 % et a 100 %, c'est-a-dire aux deux
  # graduations que cette courbe existe pour montrer. Le reglage est masque
  # cote interface plutot qu'ignore en silence.
  axe_y <- if (reponse)
    ggplot2::scale_y_continuous(
      name = if (nzchar(o$ylab)) o$ylab else tr("Mortalité observée (%)"),
      labels = function(v) paste0(formatC(v, format = "g", digits = 4), " %"))
  else ggplot2::scale_y_continuous(
    name = if (nzchar(o$ylab)) o$ylab else tr("Mortalité (probit)"),
    sec.axis = if (isTRUE(o$axe2))
      ggplot2::sec_axis(~ ., breaks = stats::qnorm(pc / 100),
                        labels = paste0(pc, " %"),
                        name = if (nzchar(o$ylab2)) o$ylab2 else tr("Mortalité (%)"))
    else ggplot2::waiver())

  p <- p +
    ggplot2::coord_cartesian(xlim = rg, ylim = ylim) +
    ggplot2::scale_x_continuous(
      name = if (nzchar(o$xlab)) o$xlab else {
        # L'unite vient de l'essai, pas d'un reglage : la retaper dans le titre
        # d'axe alors qu'elle figure deja dans la fiche serait une deuxieme
        # saisie du meme fait, donc une occasion de divergence.
        u <- hstat_dl50_unite(fits[[1]])
        if (nzchar(u)) trf("Dose en %s (échelle logarithmique)", u)
        else tr("Dose (échelle logarithmique)")
      },
      labels = function(v) formatC(10^v, format = "g", digits = 3)) +
    axe_y +
    ggplot2::labs(title = if (nzchar(o$titre)) o$titre else NULL,
                  subtitle = if (nzchar(o$sous_titre)) o$sous_titre else NULL,
                  colour = if (nzchar(o$legende_titre)) o$legende_titre else tr("Essai"),
                  fill = if (nzchar(o$legende_titre)) o$legende_titre else tr("Essai")) +
    (viz_get_theme(o$theme, base_size = o$base_size)) +
    ggplot2::theme(
      plot.title = .hstat_dl50_txt(o$titre_taille, o$titre_style,
                                   hjust = o$titre_pos),
      plot.subtitle = .hstat_dl50_txt(o$sous_titre_taille, o$sous_titre_style,
                                      hjust = o$sous_titre_pos),
      axis.title = .hstat_dl50_txt(o$axe_titre_taille, o$axe_titre_style),
      axis.text.x = .hstat_dl50_txt(o$grad_x_taille, o$grad_x_style,
                                    angle = o$grad_x_angle,
                                    hjust = if (o$grad_x_angle != 0) 1 else NULL),
      axis.text.y = .hstat_dl50_txt(o$grad_y_taille, o$grad_y_style),
      legend.position = o$legende_pos,
      legend.text = .hstat_dl50_txt(o$legende_taille, "plain"),
      legend.title = .hstat_dl50_txt(o$legende_titre_taille, "bold"),
      panel.grid = if (isTRUE(o$grille)) ggplot2::element_line()
                   else ggplot2::element_blank())
  if (multiple) {
    # LA PALETTE PAR DEFAUT DE ggplot2 N'EST PAS UN NOM RColorBrewer : elle se
    # pose par `scale_*_hue()`. La passer a `scale_*_brewer()` la ferait
    # retomber sur Set1 sans que rien ne le dise.
    #
    # C'est aussi la seule qui ne PLAFONNE pas : les qualitatives de Brewer
    # s'arretent a 8, 9 ou 12 couleurs, et au-dela ggplot avertit puis rend les
    # essais surnumeraires en gris. Sur un graphique nomme « plusieurs essais »,
    # c'est le cas qu'on rencontre pour de vrai.
    if (identical(as.character(o$palette), unname(HSTAT_PALETTE_GG))) {
      p <- p + ggplot2::scale_colour_hue() + ggplot2::scale_fill_hue()
    } else {
      # Une palette inconnue de RColorBrewer fait AVERTIR ggplot a chaque trace
      # et rend un graphique gris. L'interface n'offre que des noms valides,
      # mais la fonction est publique : on retombe sur le defaut plutot que de
      # laisser un avertissement s'accumuler dans la console d'un serveur
      # partage.
      pal <- if (o$palette %in% c(HSTAT_PALETTES_QUALI, HSTAT_PALETTES_DEGRADE))
        o$palette else HSTAT_DL50_OPT_DEFAUT$palette
      p <- p + ggplot2::scale_colour_brewer(palette = pal) +
               ggplot2::scale_fill_brewer(palette = pal)
    }
  }
  # Pose EN DERNIER : un `p + couche` reconstruit l'objet et emporterait
  # l'attribut avec lui.
  if (length(ecartes)) attr(p, "ecartes") <- ecartes
  p
}

# =============================================================================
#  INTERFACE
# =============================================================================
#  Cinq onglets, qui reprennent les fenetres de WIN DL : la fiche d'essai, les
#  deux pages de resultats reunies (elles se lisent ensemble), le graphique, la
#  comparaison d'essais, et l'outil dose <-> mortalite.

#  Un champ adosse a une liste se saisit au CHOIX ou au clavier : `create =
#  TRUE` accepte une valeur nouvelle sans passer par l'editeur, ce qui evite le
#  detour du logiciel d'origine (« il faut cliquer sur le bouton comportant le
#  dessin d'un losange... pour mettre a jour leur contenu »). La valeur tapee
#  rejoint la liste au moment de l'enregistrement.
.hstat_dl50_champ_ui <- function(ns, nm, largeur = 6) {
  shiny::column(largeur,
    if (nm %in% names(HSTAT_DL50_CHAMPS_LISTE))
      shiny::selectizeInput(ns(paste0("ch_", nm)), HSTAT_DL50_CHAMPS[[nm]],
                            choices = NULL, selected = NULL, multiple = FALSE,
                            options = list(create = TRUE, persist = FALSE,
                                           placeholder = ""))
    else shiny::textInput(ns(paste0("ch_", nm)), HSTAT_DL50_CHAMPS[[nm]]))
}

mod_dl50_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::div(class = "callout callout-info", style = "margin-bottom:14px;",
        shiny::icon("skull-crossbones"), shiny::strong(" DL50 / CL50. "),
        "Régression probit dose-mortalité (droite de Henry), doses létales et",
        " intervalles, d'après le modèle de Finney tel que le logiciel WIN DL",
        " du CIRAD l'applique. La mortalité naturelle est estimée par maximum",
        " de vraisemblance sur l'ensemble de l'essai, ou par la formule",
        " d'Abbott sur le seul témoin.")),

    shiny::tabsetPanel(
      id = ns("dl50Tabs"),

      # ------------------------------------------------------------------
      shiny::tabPanel(
        shiny::tagList(shiny::icon("clipboard-list"), " L'essai"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("file-import"), " D'où viennent les données"),
            status = "primary", width = 4, solidHeader = TRUE,
            shiny::radioButtons(ns("source"), "Source des données",
              choiceNames = list(
                shiny::HTML("<b>Saisie directe</b> <small style='color:#7f8c8d;'>(doses, effectifs, morts)</small>"),
                shiny::HTML("<b>Jeu de données chargé</b> <small style='color:#7f8c8d;'>(CSV, Excel, SPSS…)</small>"),
                shiny::HTML("<b>Fichier WIN DL</b> <small style='color:#7f8c8d;'>(.TXT natif)</small>")),
              choiceValues = list("manual", "dataset", "windl"), selected = "manual"),

            # CE QU'IL FAUT POUR CALCULER, dit avant de saisir plutot qu'en
            # refus apres coup. Le seuil des trois doses utiles est la regle de
            # WIN DL, et c'est le refus que l'on rencontre le plus souvent sans
            # comprendre pourquoi.
            shiny::div(class = "callout callout-success",
                       style = "margin:10px 0;padding:8px 12px;font-size:0.92em;",
              shiny::strong("Le minimum pour calculer :"),
              shiny::tags$ul(style = "margin:6px 0 0 0;padding-left:18px;",
                shiny::tags$li("les doses, l'effectif testé et les morts de chaque dose ;"),
                shiny::tags$li("l'effectif et les morts du lot témoin (0 et 0 s'il n'y en a pas) ;"),
                shiny::tags$li(shiny::HTML("<b>trois doses au moins</b> dont la mortalité corrigée ne soit ni 0 % ni 100 % — en deçà, la droite n'est pas déterminée et le calcul est refusé, comme dans WIN DL."))),
              shiny::HTML("La <i>fiche de l'essai</i> ci-contre est facultative : elle identifie l'essai, elle ne change aucun calcul.")),

            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'dataset'", ns("source")),
              shiny::helpText("Les colonnes viennent du jeu de travail : tout ce que",
                              " l'onglet Chargement sait lire est donc utilisable ici."),
              shiny::selectInput(ns("colDose"), "Colonne des doses", choices = NULL),
              shiny::selectInput(ns("colN"), "Colonne des effectifs testés", choices = NULL),
              shiny::selectInput(ns("colMorts"), "Colonne des morts", choices = NULL),
              shiny::selectInput(ns("colEssai"), "Colonne de regroupement (facultatif)",
                                 choices = NULL),
              shiny::helpText("Une ligne à la dose 0 est lue comme le témoin :",
                              " son logarithme n'existe pas, elle ne peut pas entrer",
                              " dans la régression."),
              shiny::actionButton(ns("importerDonnees"),
                shiny::tagList(shiny::icon("table"), " Charger depuis le jeu de données"),
                class = "btn-primary btn-block")),

            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'windl'", ns("source")),
              shiny::fileInput(ns("fichierWindl"), "Fichier d'essai WIN DL",
                               multiple = TRUE, accept = c(".txt", ".TXT", ".dat")),
              shiny::helpText("Les fichiers de la version DOS comme de la version",
                              " Windows se lisent : encodage CP437, séparateurs de",
                              " champ invisibles, marqueur de fin de fichier."))),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("vial"), " Fiche de l'essai (facultative)"),
            # REPLIEE PAR DEFAUT. Vingt champs d'identification s'ouvraient
            # au-dessus du tableau de doses, alors qu'aucun ne change un
            # calcul -- ils poussaient hors de l'ecran la seule chose qu'il
            # faut vraiment saisir. Qui en a besoin ouvre la boite ; qui veut
            # trois doses et un temoin ne la voit plus.
            status = "info", width = 8, solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            shiny::textInput(ns("titre"), "Titre de l'essai",
                             placeholder = "ex : C. leucotreta référence cyperméthrine"),
            shiny::fluidRow(
              .hstat_dl50_champ_ui(ns, "date", 4),
              .hstat_dl50_champ_ui(ns, "auteur", 4),
              .hstat_dl50_champ_ui(ns, "espece", 4)),
            shiny::fluidRow(
              .hstat_dl50_champ_ui(ns, "stade", 4),
              .hstat_dl50_champ_ui(ns, "duree", 4),
              .hstat_dl50_champ_ui(ns, "temperature", 4)),
            shiny::fluidRow(
              .hstat_dl50_champ_ui(ns, "matiere1", 4),
              .hstat_dl50_champ_ui(ns, "matiere2", 4),
              .hstat_dl50_champ_ui(ns, "ratio", 4)),
            shiny::fluidRow(
              .hstat_dl50_champ_ui(ns, "methode", 6),
              .hstat_dl50_champ_ui(ns, "unite", 6)),
            shiny::helpText("Ces champs ne changent aucun calcul : ils identifient",
                            " l'essai, et ce sont eux que la fusion compare avant",
                            " d'assembler deux répétitions."))),

        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("table-list"), " Doses et mortalités"),
            status = "primary", width = 8, solidHeader = TRUE,
            shiny::fluidRow(
              shiny::column(4, shiny::numericInput(ns("temoinN"),
                "Témoin : individus testés", value = 0, min = 0, step = 1)),
              shiny::column(4, shiny::numericInput(ns("temoinX"),
                "Témoin : individus morts", value = 0, min = 0, step = 1)),
              shiny::column(4, shiny::div(style = "margin-top:26px;",
                shiny::actionButton(ns("ligne"),
                  shiny::tagList(shiny::icon("plus"), " Ligne"), class = "btn-sm"),
                shiny::actionButton(ns("vider"),
                  shiny::tagList(shiny::icon("eraser"), " Vider"), class = "btn-sm")))),
            # LA TROISIEME COLONNE SE SAISIT DANS L'UNE OU L'AUTRE UNITE.
            # Beaucoup d'operateurs notent « 40 % » plutot que « 12 sur 30 » ;
            # c'est l'effectif qui est conserve, le pourcentage n'en est qu'une
            # lecture -- le modele binomial a besoin d'un entier.
            shiny::radioButtons(ns("saisieUnite"), "Troisième colonne",
              choices = HSTAT_DL50_UNITES_SAISIE, selected = "morts",
              inline = TRUE),
            shiny::helpText("Double-cliquez une cellule pour la modifier."),
            DT::DTOutput(ns("saisie")),
            shiny::uiOutput(ns("noteArrondi")),
            shiny::br(),
            shiny::actionButton(ns("enregistrer"),
              shiny::tagList(shiny::icon("floppy-disk"), " Enregistrer cet essai"),
              class = "btn-primary"),
            shiny::helpText("Les doses identiques sont regroupées et les doses triées,",
                            " comme dans WIN DL : deux lignes à la même dose sont deux",
                            " répétitions du même point."),
            shiny::hr(),
            .hstat_opt_section("Coller depuis un tableur", "clipboard", "#16a085", "#e8f6f3",
              shiny::helpText("Copiez trois colonnes — dose, effectif testé, morts —",
                              " et collez-les ici. La ligne d'en-tête est reconnue et",
                              " ignorée ; la virgule décimale d'un tableur français",
                              " aussi."),
              shiny::textAreaInput(ns("collage"), NULL, rows = 5,
                placeholder = "0,00063\t25\t5\n0,00125\t25\t7\n0,0025\t25\t9",
                width = "100%"),
              shiny::actionButton(ns("collerGo"),
                shiny::tagList(shiny::icon("paste"), " Remplir le tableau"),
                class = "btn-primary"),
              shiny::actionButton(ns("collerAjout"),
                shiny::tagList(shiny::icon("plus"), " Ajouter à la suite"),
                class = "btn-sm"))),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("layer-group"), " Essais en mémoire"),
            status = "success", width = 4, solidHeader = TRUE,
            shiny::uiOutput(ns("listeEssais")),
            shiny::hr(),
            shiny::selectInput(ns("essaiActif"), "Essai analysé", choices = NULL),
            shiny::actionButton(ns("retirer"),
              shiny::tagList(shiny::icon("trash"), " Retirer cet essai"),
              class = "btn-sm btn-danger"),
            # LES DEUX LIMITES SONT CELLES DE WIN DL, ET ELLES SE DISENT.
            # Elles etaient appliquees en silence : on les rencontrait sous
            # forme de refus, sans savoir d'ou elles venaient ni si elles
            # tenaient a HStat.
            shiny::helpText(
              trf(paste("Deux limites héritées de WIN DL, reprises pour rester",
                        "comparable avec lui : %d essais en mémoire au plus —",
                        "c'est ce que sa fenêtre de comparaison accepte — et %d",
                        "doses par essai. Aucune ne tient à HStat ; elles ne",
                        "gênent pas un bioessai courant, qui compte cinq à huit",
                        "doses."),
                  HSTAT_DL50_ESSAIS_MAX, HSTAT_DL50_DOSES_MAX))))),

      # ------------------------------------------------------------------
      shiny::tabPanel(
        shiny::tagList(shiny::icon("square-root-variable"), " Résultats"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("sliders"), " Estimation"),
            status = "primary", width = 4, solidHeader = TRUE,
            shiny::radioButtons(ns("methode"), "Mortalité naturelle",
                                choices = HSTAT_DL50_METHODES, selected = "em"),
            shiny::numericInput(ns("alpha"), "Risque α (intervalles)",
                                value = 0.05, min = 0.001, max = 0.2, step = 0.01),
            shiny::radioButtons(ns("chemin"), "Chemin de convergence",
                                choices = HSTAT_DL50_CONVERGENCE, selected = "rapide"),
            shiny::helpText("Le chemin de WIN DL part d'une mortalité naturelle",
                            " non nulle et rampe vers l'optimum : il rapproche des",
                            " chiffres publiés par le logiciel (écart médian divisé",
                            " par 3), mais s'arrête avant le maximum de",
                            " vraisemblance et demande beaucoup plus d'itérations —",
                            " sur un essai à réponse plate, cent n'y suffisent pas."),
            shiny::numericInput(ns("chiffres"), "Chiffres significatifs affichés",
                                value = HSTAT_DL50_CHIFFRES, min = 1, max = 15, step = 1),
            shiny::helpText("Ne change que l'affichage : les calculs, les exports",
                            " Excel et CSV gardent toute leur précision."),
            shiny::numericInput(ns("itmax"), "Itérations maximum",
                                value = HSTAT_DL50_ITMAX, min = 1,
                                max = HSTAT_DL50_ITMAX_MAX, step = 10),
            shiny::helpText("Le calcul s'arrête dès que la log-vraisemblance ne",
                            " bouge plus de 1e-5 : cent itérations suffisent très",
                            " largement, l'essai de référence en demande trois.",
                            " Relevez ce plafond seulement si un essai difficile",
                            " annonce « convergence non atteinte » — et méfiez-vous",
                            " alors du résultat plutôt que du plafond."),
            shiny::actionButton(ns("testAbbott"),
              shiny::tagList(shiny::icon("scale-balanced"), " Abbott ou EM ?"),
              class = "btn-default btn-block"),
            shiny::uiOutput(ns("resTestAbbott"))),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("chart-line"), " Paramètres de la régression"),
            status = "success", width = 8, solidHeader = TRUE,
            shiny::uiOutput(ns("messageFit")),
            shiny::uiOutput(ns("verdict")),
            shiny::uiOutput(ns("resume")),
            DT::DTOutput(ns("parametres")))),

        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("crosshairs"), " Doses létales"),
            status = "warning", width = 12, solidHeader = TRUE,
            shiny::textInput(ns("seuils"), "Seuils de mortalité (%)",
                             value = "10, 50, 90"),
            shiny::helpText("Séparés par des virgules ou des points-virgules.",
                            " DL10, DL50 et DL90 sont celles que les rapports de",
                            " bioessai portent."),
            shiny::uiOutput(ns("noteSeuils")),
            DT::DTOutput(ns("dosesLetales")))),

        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("table"), " Détail par dose"),
            status = "info", width = 12, solidHeader = TRUE,
            DT::DTOutput(ns("detail")),
            shiny::br(),
            shiny::downloadButton(ns("dlCsv"), " Télécharger (CSV)", class = "btn-sm"),
            shiny::downloadButton(ns("dlXlsx"), " Télécharger (Excel)", class = "btn-sm"),
            shiny::downloadButton(ns("dlPrn"), " Rapport .PRN (format WIN DL)",
                                  class = "btn-sm"),
            shiny::downloadButton(ns("dlTxt"), " Essai .TXT (format WIN DL)",
                                  class = "btn-sm")))),

      # ------------------------------------------------------------------
      shiny::tabPanel(
        shiny::tagList(shiny::icon("chart-simple"), " Graphique"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("sliders"), " Options du graphique"),
            status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,

            .hstat_opt_section("Type de graphique", "chart-line", "#d35400", "#fdf0e6",
              shiny::radioButtons(ns("gType"), NULL, choices = HSTAT_DL50_GRAPHES,
                                  selected = "probit"),
              shiny::helpText("La droite de Henry montre le modèle : le probit de la",
                              " mortalité corrigée est linéaire en log-dose, et cela se",
                              " vérifie à l'œil. La courbe dose-réponse montre la",
                              " réponse mesurée — une sigmoïde qui part de la mortalité",
                              " naturelle et monte vers 100 %. Elle se lit sans savoir",
                              " ce qu'est un probit, et elle porte les doses à 0 % et à",
                              " 100 %, que la droite ne peut pas tracer.")),

            .hstat_opt_section("Éléments tracés", "eye", "#2e86c1", "#eaf3fb",
              shiny::checkboxInput(ns("gPoints"), "Points de l'essai (PE)", TRUE),
              shiny::checkboxInput(ns("gCourbe"), "Courbe de l'essai (CE)", FALSE),
              shiny::checkboxInput(ns("gDroite"), "Modèle ajusté (DR)", TRUE),
              shiny::checkboxInput(ns("gBande"), "Intervalles (IF ou IC)", TRUE),
              shiny::checkboxInput(ns("gReperes"), "Repères DL10 / DL50 / DL90", TRUE),
              shiny::checkboxInput(ns("gTous"), "Tous les essais en mémoire", FALSE)),

            .hstat_opt_section("Titres", "heading", "#8e44ad", "#f4ecfa",
              shiny::textInput(ns("gTitre"), "Titre du graphique"),
              shiny::fluidRow(
                shiny::column(4, shiny::numericInput(ns("gTitreTaille"),
                  "Taille du titre", value = 15, min = 6, max = 40, step = 1)),
                shiny::column(4, shiny::selectInput(ns("gTitreStyle"),
                  "Style du titre", choices = HSTAT_FONT_STYLES, selected = "bold")),
                shiny::column(4, shiny::selectInput(ns("gTitrePos"),
                  "Position du titre", choices = HSTAT_DL50_POS, selected = "0.5"))),
              shiny::textInput(ns("gSousTitre"), "Sous-titre"),
              shiny::fluidRow(
                shiny::column(4, shiny::numericInput(ns("gSousTitreTaille"),
                  "Taille du sous-titre", value = 12, min = 6, max = 40, step = 1)),
                shiny::column(4, shiny::selectInput(ns("gSousTitreStyle"),
                  "Style du sous-titre", choices = HSTAT_FONT_STYLES,
                  selected = "italic")),
                shiny::column(4, shiny::selectInput(ns("gSousTitrePos"),
                  "Position du sous-titre", choices = HSTAT_DL50_POS,
                  selected = "0.5")))),

            .hstat_opt_section("Axes", "ruler-combined", "#16a085", "#e8f6f3",
              shiny::textInput(ns("gXlab"), "Titre de l'axe des doses"),
              shiny::textInput(ns("gYlab"), "Titre de l'axe des mortalités"),
              # Le second axe n'existe que sur la droite de Henry : sur la
              # courbe dose-reponse, le pourcentage est deja l'axe principal et
              # son pendant en probit placerait l'infini a 0 % et a 100 %. Un
              # reglage que l'image ignore vaut mieux masque que declare.
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'probit'", ns("gType")),
                shiny::textInput(ns("gYlab2"), "Titre de l'axe des pourcentages"),
                shiny::checkboxInput(ns("gAxe2"), "Second axe en pourcentage", TRUE)),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("gAxeTitreTaille"),
                  "Taille des titres d'axe", value = 12, min = 6, max = 30, step = 1)),
                shiny::column(6, shiny::selectInput(ns("gAxeTitreStyle"),
                  "Style des titres d'axe", choices = HSTAT_FONT_STYLES))),
              shiny::fluidRow(
                shiny::column(4, shiny::numericInput(ns("gGradXTaille"),
                  "Taille des graduations X", value = 10, min = 4, max = 30, step = 1)),
                shiny::column(4, shiny::selectInput(ns("gGradXStyle"),
                  "Style des graduations X", choices = HSTAT_FONT_STYLES)),
                shiny::column(4, shiny::numericInput(ns("gGradXAngle"),
                  "Angle des graduations X", value = 0, min = -90, max = 90, step = 15))),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("gGradYTaille"),
                  "Taille des graduations Y", value = 10, min = 4, max = 30, step = 1)),
                shiny::column(6, shiny::selectInput(ns("gGradYStyle"),
                  "Style des graduations Y", choices = HSTAT_FONT_STYLES))),
              shiny::helpText("Les limites de l'axe des doses se saisissent en doses,",
                              " pas en logarithmes. Laissez vide pour l'étendue",
                              " automatique."),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("gXmin"),
                  "Dose minimale", value = NA, min = 0)),
                shiny::column(6, shiny::numericInput(ns("gXmax"),
                  "Dose maximale", value = NA, min = 0))),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("gYmin"),
                  "Mortalité minimale (%)", value = NA, min = 0, max = 100)),
                shiny::column(6, shiny::numericInput(ns("gYmax"),
                  "Mortalité maximale (%)", value = NA, min = 0, max = 100)))),

            .hstat_opt_section("Points et traits", "bezier-curve", "#e67e22", "#fdf1e6",
              shiny::fluidRow(
                shiny::column(4, shiny::numericInput(ns("gPointTaille"),
                  "Taille des points", value = 2.4, min = 0.5, max = 12, step = 0.2)),
                shiny::column(4, shiny::selectInput(ns("gPointForme"),
                  "Forme des points", choices = HSTAT_DL50_FORMES, selected = "16")),
                shiny::column(4, shiny::numericInput(ns("gPointOpacite"),
                  "Opacité des points", value = 1, min = 0.1, max = 1, step = 0.05))),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("gDroiteEp"),
                  "Épaisseur de la droite", value = 0.9, min = 0.1, max = 5, step = 0.1)),
                shiny::column(6, shiny::selectInput(ns("gDroiteType"),
                  "Trait de la droite", choices = HSTAT_DL50_TRAITS,
                  selected = "solid"))),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("gCourbeEp"),
                  "Épaisseur de la courbe", value = 0.6, min = 0.1, max = 5, step = 0.1)),
                shiny::column(6, shiny::selectInput(ns("gCourbeType"),
                  "Trait de la courbe", choices = HSTAT_DL50_TRAITS,
                  selected = "dashed"))),
              shiny::numericInput(ns("gBandeOpacite"), "Opacité des intervalles",
                                  value = 0.12, min = 0.02, max = 1, step = 0.02)),

            .hstat_opt_section("Repères DL", "location-crosshairs", "#c0392b", "#fbeceb",
              shiny::checkboxInput(ns("gRepereEtiq"), "Étiqueter les repères", TRUE),
              shiny::fluidRow(
                shiny::column(4, colourInput(ns("gRepereCouleur"),
                  "Couleur des repères", value = "#6b7280")),
                shiny::column(4, shiny::selectInput(ns("gRepereType"),
                  "Trait des repères", choices = HSTAT_DL50_TRAITS,
                  selected = "dotted")),
                shiny::column(4, shiny::numericInput(ns("gRepereEp"),
                  "Épaisseur des repères", value = 0.5, min = 0.1, max = 3,
                  step = 0.1)))),

            .hstat_opt_section("Couleurs et légende", "palette", "#2c3e50", "#eceff1",
              shiny::selectInput(ns("gTheme"), "Thème", choices = HSTAT_THEMES_GG,
                                 selected = "minimal"),
              shiny::numericInput(ns("gBaseSize"), "Taille de police de base",
                                  value = 12, min = 6, max = 30, step = 1),
              shiny::checkboxInput(ns("gGrille"), "Afficher la grille", TRUE),
              shiny::selectInput(ns("gPalette"), "Palette (plusieurs essais)",
                                 choices = c(HSTAT_PALETTE_GG,
                                             HSTAT_PALETTES_QUALI),
                                 selected = "Set1"),
              colourInput(ns("gCouleur"), "Couleur (un seul essai)",
                                        value = "#2e86c1"),
              shiny::textInput(ns("gLegendeTitre"), "Titre de la légende"),
              shiny::fluidRow(
                shiny::column(4, shiny::selectInput(ns("gLegendePos"),
                  "Position de la légende", choices = HSTAT_DL50_LEGENDE,
                  selected = "right")),
                shiny::column(4, shiny::numericInput(ns("gLegendeTaille"),
                  "Taille de la légende", value = 10, min = 4, max = 30, step = 1)),
                shiny::column(4, shiny::numericInput(ns("gLegendeTitreTaille"),
                  "Taille du titre de légende", value = 11, min = 4, max = 30,
                  step = 1))))),

          shinydashboard::box(
            # Le titre suit le type choisi : une boite intitulee « Droite de
            # Henry » au-dessus d'une sigmoide annonce le mauvais graphique.
            title = shiny::tagList(shiny::icon("chart-simple"), " ",
                                   shiny::textOutput(ns("gNom"), inline = TRUE)),
            status = "success", width = 8, solidHeader = TRUE,
            shiny::plotOutput(ns("graphe"), height = "600px"),
            shiny::uiOutput(ns("gNote")),
            shiny::hr(),
            .hstat_opt_section("Export de l'image", "download", "#27ae60", "#e9f7ef",
              shiny::fluidRow(
                shiny::column(3, hstat_format_input(ns("gFmt"), "Format")),
                shiny::column(3, shiny::numericInput(ns("gLargeur"),
                  "Largeur (pixels)", value = 1200, min = 200, step = 100)),
                shiny::column(3, shiny::numericInput(ns("gHauteur"),
                  "Hauteur (pixels)", value = 800, min = 200, step = 100)),
                shiny::column(3, hstat_dpi_input(ns("gDpi"), "Résolution (DPI)"))),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'jpeg'", ns("gFmt")),
                shiny::sliderInput(ns("gQualite"), "Qualité JPEG",
                                   min = 50, max = 100, value = 95, step = 5)),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'tiff'", ns("gFmt")),
                shiny::selectInput(ns("gCompression"), "Compression TIFF",
                                   choices = HSTAT_TIFF_COMPRESSION, selected = "lzw")),
              shiny::verbatimTextOutput(ns("gTaille")),
              shiny::downloadButton(ns("gDl"), " Télécharger le graphique",
                                    class = "btn-success"))))),

      shiny::tabPanel(
        shiny::tagList(shiny::icon("code-compare"), " Comparaison d'essais"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("sliders"), " Scénario sur la mortalité naturelle"),
            status = "primary", width = 4, solidHeader = TRUE,
            shiny::radioButtons(ns("scenario"), "Mortalité naturelle",
                                choices = HSTAT_DL50_SCENARIOS, selected = "heterogene"),
            shiny::actionButton(ns("comparer"),
              shiny::tagList(shiny::icon("play"), " Comparer les essais"),
              class = "btn-primary btn-block"),
            shiny::hr(),
            shiny::actionButton(ns("fusionner"),
              shiny::tagList(shiny::icon("object-group"), " Fusionner les essais"),
              class = "btn-default btn-block"),
            shiny::helpText("La fusion regroupe les répétitions d'une même",
                            " expérimentation. Elle exige des champs identiques et",
                            " un test d'identité non significatif.")),
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("vials"), " Tests du rapport de vraisemblance"),
            status = "success", width = 8, solidHeader = TRUE,
            shiny::uiOutput(ns("messageComp")),
            DT::DTOutput(ns("tableComp")),
            shiny::br(),
            shiny::downloadButton(ns("compCsv"), " Télécharger (CSV)", class = "btn-sm"),
            shiny::downloadButton(ns("compXlsx"), " Télécharger (Excel)", class = "btn-sm"))),

        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("scale-unbalanced"),
                                   " Rapport de puissance (ratio de résistance)"),
            status = "warning", width = 12, solidHeader = TRUE,
            shiny::div(class = "callout callout-warning", style = "padding:8px 12px;",
              shiny::icon("lightbulb"),
              shiny::strong(" Combien de fois plus de produit ? "),
              "Le rapport des DL50 dit combien de fois il en faut davantage pour",
              " tuer l'essai comparé que l'essai de référence. Il n'existe qu'à",
              " pente commune : si les droites ne sont pas parallèles, il change",
              " avec le niveau de mortalité, et le test de parallélisme le dit."),
            shiny::fluidRow(
              shiny::column(5, shiny::selectInput(ns("puiRef"), "Essai de référence",
                                                  choices = NULL)),
              shiny::column(4, shiny::div(style = "margin-top:26px;",
                shiny::actionButton(ns("puiGo"),
                  shiny::tagList(shiny::icon("divide"), " Calculer le rapport"),
                  class = "btn-primary")))),
            shiny::uiOutput(ns("messagePui")),
            DT::DTOutput(ns("tablePui")),
            shiny::br(),
            shiny::downloadButton(ns("puiCsv"), " Télécharger (CSV)", class = "btn-sm"),
            shiny::downloadButton(ns("puiXlsx"), " Télécharger (Excel)", class = "btn-sm")))),

      # ------------------------------------------------------------------
      shiny::tabPanel(
        shiny::tagList(shiny::icon("list-check"), " Vocabulaire & sélection"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("rectangle-list"), " Listes déroulantes"),
            status = "primary", width = 5, solidHeader = TRUE,
            shiny::div(class = "callout callout-info", style = "padding:8px 12px;",
              shiny::icon("lightbulb"),
              " Les listes existent pour que la sélection fonctionne : deux",
              " essais notés « Cyfluthrine » et « cyfluthrine » ne se",
              " retrouveraient jamais ensemble."),
            shiny::selectInput(ns("listeNom"), "Liste à modifier",
                               choices = HSTAT_DL50_LISTES),
            shiny::fluidRow(
              shiny::column(8, shiny::textInput(ns("listeValeur"), "Valeur à ajouter")),
              shiny::column(4, shiny::div(style = "margin-top:26px;",
                shiny::actionButton(ns("listeAjouter"),
                  shiny::tagList(shiny::icon("plus"), " Ajouter"),
                  class = "btn-primary btn-block")))),
            shiny::selectInput(ns("listeSupprimer"), "Valeur à supprimer",
                               choices = NULL),
            shiny::actionButton(ns("listeRetirer"),
              shiny::tagList(shiny::icon("trash"), " Supprimer"), class = "btn-sm"),
            shiny::actionButton(ns("listeRegenerer"),
              shiny::tagList(shiny::icon("rotate"), " Régénérer depuis les essais"),
              class = "btn-sm"),
            shiny::hr(),
            shiny::fileInput(ns("listeFichier"), "Importer une liste (.TXT)",
                             accept = c(".txt", ".TXT")),
            shiny::downloadButton(ns("listeDl"), " Exporter la liste (.TXT)",
                                  class = "btn-sm"),
            shiny::helpText("Format de WIN DL : une valeur par ligne, marqueur de",
                            " fin de fichier compris.")),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("filter"), " Sélection multi-critères"),
            status = "info", width = 7, solidHeader = TRUE,
            shiny::helpText("Un critère laissé vide ne filtre pas. La sélection",
                            " restreint les essais analysés, comparés et fusionnés."),
            shiny::fluidRow(
              shiny::column(6, shiny::selectizeInput(ns("selAuteur"), "Auteur",
                choices = NULL, multiple = TRUE)),
              shiny::column(6, shiny::selectizeInput(ns("selEspece"), "Espèce",
                choices = NULL, multiple = TRUE))),
            shiny::fluidRow(
              shiny::column(6, shiny::selectizeInput(ns("selStade"), "Stade",
                choices = NULL, multiple = TRUE)),
              shiny::column(6, shiny::selectizeInput(ns("selDuree"), "Durée",
                choices = NULL, multiple = TRUE))),
            shiny::fluidRow(
              shiny::column(6, shiny::selectizeInput(ns("selMethode"),
                "Méthode de traitement", choices = NULL, multiple = TRUE)),
              shiny::column(6, shiny::selectizeInput(ns("selUnite"), "Unité de dose",
                choices = NULL, multiple = TRUE))),
            shiny::fluidRow(
              shiny::column(6, shiny::selectizeInput(ns("selMa1"), "Matière active n°1",
                choices = NULL, multiple = TRUE)),
              shiny::column(6, shiny::div(style = "margin-top:26px;",
                shiny::checkboxInput(ns("selAvecMa2"),
                  "Trier aussi sur la matière active n°2 et le ratio", FALSE)))),
            shiny::conditionalPanel(
              condition = sprintf("input['%s']", ns("selAvecMa2")),
              shiny::fluidRow(
                shiny::column(6, shiny::selectizeInput(ns("selMa2"),
                  "Matière active n°2", choices = NULL, multiple = TRUE)),
                shiny::column(6, shiny::textInput(ns("selRatio"), "Ratio",
                                                  placeholder = "ex : 1:2")))),
            shiny::fluidRow(
              shiny::column(4, shiny::numericInput(ns("selTempMin"),
                "Température minimale", value = NA, step = 1)),
              shiny::column(4, shiny::numericInput(ns("selTempMax"),
                "Température maximale", value = NA, step = 1)),
              shiny::column(4, shiny::div(style = "margin-top:26px;",
                shiny::actionButton(ns("selValider"),
                  shiny::tagList(shiny::icon("filter"), " Sélectionner"),
                  class = "btn-primary"),
                shiny::actionButton(ns("selAnnuler"),
                  shiny::tagList(shiny::icon("xmark"), " Annuler"),
                  class = "btn-sm")))),
            shiny::helpText("Une seule température : égalité. Deux : intervalle."),
            shiny::hr(),
            shiny::uiOutput(ns("selResultat")),
            DT::DTOutput(ns("selTable"))))),

      shiny::tabPanel(
        shiny::tagList(shiny::icon("calculator"), " Dose ⇄ mortalité"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("arrow-right"), " Une dose, quelle mortalité ?"),
            status = "primary", width = 6, solidHeader = TRUE,
            shiny::textInput(ns("dosesCalc"), "Doses à évaluer", value = "0.01, 0.05"),
            shiny::helpText("Intervalle de prédiction, construit sur le probit puis",
                            " ramené en pourcentage : les bornes restent donc entre",
                            " 0 et 100 %."),
            DT::DTOutput(ns("tableMort")),
            shiny::br(),
            shiny::downloadButton(ns("mortCsv"), " Télécharger (CSV)", class = "btn-sm"),
            shiny::downloadButton(ns("mortXlsx"), " Télécharger (Excel)", class = "btn-sm")),
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("arrow-left"), " Une mortalité, quelle dose ?"),
            status = "info", width = 6, solidHeader = TRUE,
            shiny::textInput(ns("mortsCalc"), "Mortalités visées (%)", value = "25, 50, 95"),
            shiny::helpText("Mortalité observée, témoin compris : elle est ramenée",
                            " par Abbott avant l'inversion de la droite."),
            DT::DTOutput(ns("tableDose")),
            shiny::br(),
            shiny::downloadButton(ns("doseCsv"), " Télécharger (CSV)", class = "btn-sm"),
            shiny::downloadButton(ns("doseXlsx"), " Télécharger (Excel)", class = "btn-sm"))))
    ))
}

# =============================================================================
#  SERVEUR
# =============================================================================
mod_dl50_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    bandeau <- function(motif, type = "warning") {
      if (is.null(motif) || !length(motif) || !nzchar(motif)) return(NULL)
      shiny::div(class = paste0("callout callout-", type),
                 style = "padding:10px 14px;",
                 shiny::icon(if (identical(type, "warning"))
                   "triangle-exclamation" else "circle-info"), " ", motif)
    }

    # Une liste de nombres saisie au clavier. La virgule y est un SEPARATEUR --
    # a la difference de la saisie d'une valeur unique, ou elle serait la
    # decimale. On l'accepte donc comme separateur seulement quand elle
    # separe deux nombres, ce que le point-virgule et l'espace font sans
    # ambiguite.
    .nombres <- function(txt) {
      if (is.null(txt) || !nzchar(txt)) return(numeric(0))
      v <- suppressWarnings(as.numeric(strsplit(txt, "[;,[:space:]]+")[[1]]))
      v[is.finite(v)]
    }

    # -- Le magasin d'essais ------------------------------------------------
    essais <- shiny::reactiveVal(list())
    saisie <- shiny::reactiveVal(
      data.frame(Dose = rep(NA_real_, 5), Effectif = rep(NA_real_, 5),
                 Morts = rep(NA_real_, 5), stringsAsFactors = FALSE))

    champs_saisis <- function() {
      st <- stats::setNames(lapply(names(HSTAT_DL50_CHAMPS), function(nm)
        trimws(input[[paste0("ch_", nm)]] %||% "")), names(HSTAT_DL50_CHAMPS))
      st
    }

    # L'EFFECTIF EST CE QUI EST CONSERVE ; le pourcentage n'en est qu'une
    # lecture. Le modele binomial a besoin d'un entier, et garder le
    # pourcentage comme source obligerait a le reconvertir a chaque calcul --
    # donc a arrondir plusieurs fois, ce qui ne rend pas toujours le meme
    # nombre.
    # Chiffres significatifs A L'AFFICHAGE : borne ici, une fois, plutot que
    # dans chacune des six tables. Un champ numerique accepte le vide et le
    # zero, et `formatSignif(0)` leve.
    chiffres <- shiny::reactive({
      v <- suppressWarnings(as.integer(.hstat_num1(input$chiffres, HSTAT_DL50_CHIFFRES)))
      if (!length(v) || is.na(v)) v <- HSTAT_DL50_CHIFFRES
      min(max(v, 1L), 15L)
    })

    en_pct <- function() identical(input$saisieUnite %||% "morts", "pct")
    saisie_affichee <- shiny::reactive({
      d <- saisie()
      if (en_pct())
        d$Morts <- round(hstat_dl50_morts_vers_pct(d$Effectif, d$Morts), 4)
      d
    })

    # LA TABLE N'EST RECONSTRUITE QUE SUR CHANGEMENT D'UNITE, et mise a jour
    # ensuite par son proxy. Relire `saisie()` dans le rendu la reconstruirait
    # a CHAQUE cellule modifiee : la cellule en cours d'edition est alors
    # detruite sous le curseur, et deux saisies rapprochees se perdent l'une
    # l'autre. C'est l'idiome prevu par DT, et le seul qui rende la saisie
    # utilisable. L'en-tete, lui, doit suivre l'unite -- d'ou la dependance
    # explicite a `input$saisieUnite`, la seule du bloc.
    output$saisie <- DT::renderDT({
      pct <- identical(input$saisieUnite %||% "morts", "pct")
      DT::datatable(shiny::isolate(saisie_affichee()), rownames = FALSE,
                    editable = list(target = "cell"), selection = "none",
                    colnames = c(tr("Dose"), tr("Effectif testé"),
                                 if (pct) tr("Mortalité (%)") else tr("Morts")),
                    options = list(dom = "t", pageLength = 100, ordering = FALSE))
    })
    proxy_saisie <- DT::dataTableProxy("saisie")
    # LA TABLE EST RAFRAICHIE PAR SON PROXY, SANS EXCEPTION -- et c'est bien.
    #
    # J'ai cru pendant deux passages qu'une cellule fraichement modifiee
    # s'affichait VIDE, et j'ai construit un contournement pour l'eviter :
    # sauter la mise a jour du proxy quand le changement naissait dans la
    # table. Le defaut n'existe pas.
    #
    # `innerText` d'une cellule en cours d'edition rend la chaine vide, parce
    # que DT y a place son editeur `<input type="number">` et que le texte d'un
    # champ de saisie n'est pas du texte de noeud. La mesure etait fausse, pas
    # l'affichage : des que le focus quitte la cellule, elle montre la valeur.
    #
    # Le contournement, lui, apportait une vraie regression : en pourcentage,
    # la cellule aurait garde le chiffre TAPE (« 50 ») alors que l'arrondi
    # range 4 morts sur 7, soit 57,14 % -- l'ecran aurait cesse de dire la
    # verite pour eviter un defaut inexistant. Ne pas le reintroduire.
    shiny::observeEvent(saisie_affichee(), {
      DT::replaceData(proxy_saisie, saisie_affichee(), resetPaging = FALSE,
                      rownames = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$saisie_cell_edit, {
      info <- input$saisie_cell_edit
      d <- saisie()
      j <- info$col + 1L
      if (j < 1L || j > ncol(d)) return()
      v <- suppressWarnings(as.numeric(gsub(",", ".", info$value, fixed = TRUE)))
      # La colonne des morts saisie en POURCENTAGE est convertie tout de suite :
      # ce qui est range est toujours un effectif.
      d[info$row, j] <- if (j == 3L && en_pct())
        as.numeric(hstat_dl50_pct_vers_morts(d$Effectif[info$row], v)) else v
      saisie(d)
    })

    # L'ARRONDI SE DIT. « 40 % » de 7 individus font 2,8 : on enregistre 3,
    # soit 42,86 %. Rendre la valeur sans le signaler laisserait croire que
    # l'essai porte le chiffre saisi -- et deux pourcentages differents
    # donnent souvent le meme effectif, une precision que l'essai n'autorise
    # pas.
    #
    # Meme artefact de repeinture que la cellule blanche ci-dessus, et meme
    # cause : apres une edition VENUE DE LA TABLE, la note reste sur l'etat
    # precedent jusqu'au redessin suivant. Verifie a la sonde -- le serveur la
    # recalcule bien, c'est l'affichage qui traine. La valeur rangee, elle, est
    # juste : la bascule d'unite la montre aussitot. Coller, ajouter une ligne
    # ou basculer l'unite remet tout d'aplomb.
    output$noteArrondi <- shiny::renderUI({
      if (!en_pct()) return(NULL)
      d <- saisie()
      ok <- is.finite(d$Effectif) & d$Effectif > 0 & is.finite(d$Morts)
      if (!any(ok)) return(NULL)
      reel <- 100 * d$Morts[ok] / d$Effectif[ok]
      # LA NOTE PASSE EN AVERTISSEMENT des que le pourcentage reellement
      # atteignable n'est pas un nombre rond -- 4 morts sur 7 font 57,142857 %,
      # que personne n'a tape. Quand il tombe juste (12 sur 30 = 40 %), la
      # saisie a ete respectee a l'unite pres et une alerte serait du bruit.
      cls <- if (any(abs(reel - round(reel)) > 1e-9)) "callout-warning" else "callout-info"
      shiny::div(class = paste("callout", cls),
                 style = "margin-top:8px;padding:8px 12px;font-size:0.92em;",
        shiny::icon("circle-info"), " ",
        trf("Les pourcentages sont enregistrés en effectifs : %s. C'est l'effectif qui fait foi — un pourcentage plus fin que %d individus ne l'autorisent n'ajoute rien.",
            paste(sprintf("%s %% → %d/%d",
                          trimws(formatC(reel, format = "g", digits = 4)),
                          as.integer(d$Morts[ok]), as.integer(d$Effectif[ok])),
                  collapse = " ; "),
            as.integer(max(d$Effectif[ok]))))
    })

    shiny::observeEvent(input$ligne, {
      d <- rbind(saisie(), data.frame(Dose = NA_real_, Effectif = NA_real_,
                                      Morts = NA_real_))
      rownames(d) <- NULL
      saisie(d)
    })
    .coller <- function(ajouter) {
      r <- hstat_dl50_coller(input$collage)
      if (!isTRUE(r$ok)) {
        shiny::showNotification(r$message, type = "warning", duration = 10)
        return()
      }
      # LE COLLAGE SUIT L'UNITE CHOISIE. Coller une colonne de pourcentages
      # pendant que le tableau est en « morts » enregistrerait « 40 » morts sur
      # 30 individus -- refuse plus loin, mais pour une raison qui n'aurait
      # aucun rapport avec la cause.
      if (en_pct())
        r$table$Morts <- as.numeric(
          hstat_dl50_pct_vers_morts(r$table$Effectif, r$table$Morts))
      d <- if (isTRUE(ajouter)) {
        cur <- saisie()
        # Les lignes entierement vides du tableau de depart ne sont pas des
        # donnees : les garder devant le collage ferait un tableau a trous.
        cur <- cur[rowSums(!is.na(cur)) > 0, , drop = FALSE]
        rbind(cur, r$table)
      } else r$table
      rownames(d) <- NULL
      saisie(d)
      shiny::updateTextAreaInput(session, "collage", value = "")
      shiny::showNotification(
        trf("%d ligne(s) collée(s) ; la virgule y a été lue comme %s.",
            r$lignes, if (identical(r$decimale, ","))
              tr("une décimale") else tr("un séparateur")),
        type = "message", duration = 7)
    }
    shiny::observeEvent(input$collerGo, .coller(FALSE))
    shiny::observeEvent(input$collerAjout, .coller(TRUE))

    shiny::observeEvent(input$vider, saisie(
      data.frame(Dose = rep(NA_real_, 5), Effectif = rep(NA_real_, 5),
                 Morts = rep(NA_real_, 5), stringsAsFactors = FALSE)))

    # -- Les colonnes du jeu de travail ------------------------------------
    donnees <- shiny::reactive({
      values$filteredData %||% values$cleanData %||% values$data
    })
    shiny::observe({
      d <- donnees()
      shiny::req(is.data.frame(d))
      num <- names(d)[vapply(d, is.numeric, logical(1))]
      for (id2 in c("colDose", "colN", "colMorts"))
        shiny::updateSelectInput(session, id2, choices = num,
                                 selected = shiny::isolate(input[[id2]]))
      shiny::updateSelectInput(session, "colEssai",
        choices = c(stats::setNames("", tr("(aucune)")), names(d)),
        selected = shiny::isolate(input$colEssai))
    })

    .ajouter <- function(nouveaux) {
      cur <- essais()
      libre <- HSTAT_DL50_ESSAIS_MAX - length(cur)
      if (libre <= 0) {
        shiny::showNotification(
          trf("Six essais sont déjà en mémoire : retirez-en un avant d'en ajouter (limite de WIN DL, %d).",
              HSTAT_DL50_ESSAIS_MAX), type = "warning", duration = 8)
        return(invisible(FALSE))
      }
      if (length(nouveaux) > libre) {
        shiny::showNotification(
          trf("%d essais retenus sur %d : la mémoire n'accepte que %d essais au total.",
              libre, length(nouveaux), HSTAT_DL50_ESSAIS_MAX),
          type = "warning", duration = 8)
        nouveaux <- nouveaux[seq_len(libre)]
      }
      nom <- vapply(nouveaux, function(e)
        if (nzchar(e$titre)) e$titre else tr("Essai"), character(1))
      names(nouveaux) <- make.unique(c(names(cur), nom), sep = " ")[
        length(cur) + seq_along(nouveaux)]
      essais(c(cur, nouveaux))
      invisible(TRUE)
    }

    shiny::observeEvent(input$enregistrer, {
      d <- saisie()
      e <- hstat_dl50_essai(d$Dose, d$Effectif, d$Morts,
                            temoin_n = .hstat_num1(input$temoinN, 0),
                            temoin_morts = .hstat_num1(input$temoinX, 0),
                            titre = trimws(input$titre %||% ""),
                            champs = champs_saisis())
      msg <- .hstat_dl50_valide(e)
      if (!is.null(msg)) {
        shiny::showNotification(msg, type = "warning", duration = 10)
        return()
      }
      # Ce qui a ete tape dans la fiche rejoint le vocabulaire : sans cela, la
      # valeur nouvelle serait absente de la liste au prochain essai, et deux
      # orthographes finiraient par cohabiter.
      listes(hstat_dl50_regenerer(listes(), list(e)))
      if (isTRUE(.ajouter(list(e))))
        shiny::showNotification(
          trf("Essai enregistré : %d dose(s), témoin %g/%g.",
              nrow(e$doses), e$x0, e$n0), type = "message", duration = 5)
    })

    shiny::observeEvent(input$importerDonnees, {
      r <- hstat_dl50_depuis_donnees(
        donnees(), input$colDose, input$colN, input$colMorts,
        col_essai = input$colEssai,
        titre = trimws(input$titre %||% "") , champs = champs_saisis())
      if (!isTRUE(r$ok)) {
        shiny::showNotification(r$message, type = "warning", duration = 10)
        return()
      }
      bons <- Filter(function(e) is.null(.hstat_dl50_valide(e)), r$essais)
      ecartes <- length(r$essais) - length(bons)
      if (!length(bons)) {
        shiny::showNotification(
          .hstat_dl50_valide(r$essais[[1]]) %||%
            tr("Aucun essai exploitable dans les colonnes choisies."),
          type = "warning", duration = 10)
        return()
      }
      if (isTRUE(.ajouter(bons)))
        shiny::showNotification(
          trf("%d essai(s) importé(s) du jeu de données%s.", length(bons),
              if (ecartes > 0) trf(" ; %d écarté(s), faute de doses exploitables", ecartes)
              else ""),
          type = "message", duration = 8)
    })

    shiny::observeEvent(input$fichierWindl, {
      f <- input$fichierWindl
      shiny::req(f)
      lus <- list(); refus <- character(0)
      for (i in seq_len(nrow(f))) {
        r <- hstat_dl50_lire_windl(f$datapath[i])
        # Le nom du fichier est celui de l'utilisateur, pas le chemin temporaire
        # de Shiny : afficher `0a1b2c.txt` ne lui dirait rien.
        if (isTRUE(r$ok)) {
          e <- r$essai
          if (!nzchar(e$titre)) e$titre <- f$name[i]
          lus[[length(lus) + 1L]] <- e
        } else refus <- c(refus, trf("%s : %s", f$name[i], r$message))
      }
      if (length(lus)) .ajouter(lus)
      if (length(refus))
        shiny::showNotification(paste(refus, collapse = " — "),
                                type = "warning", duration = 12)
      if (length(lus))
        shiny::showNotification(trf("%d fichier(s) WIN DL lu(s).", length(lus)),
                                type = "message", duration = 6)
    })

    shiny::observeEvent(input$retirer, {
      cur <- essais(); nm <- input$essaiActif
      if (!length(cur) || is.null(nm) || !(nm %in% names(cur))) return()
      essais(cur[setdiff(names(cur), nm)])
    })

    shiny::observe({
      nm <- names(essais_retenus())
      shiny::updateSelectInput(session, "essaiActif", choices = nm,
        selected = if (length(nm)) {
          a <- shiny::isolate(input$essaiActif)
          if (!is.null(a) && a %in% nm) a else nm[1]
        } else NULL)
    })

    output$listeEssais <- shiny::renderUI({
      cur <- essais()
      if (!length(cur))
        return(shiny::helpText(tr("Aucun essai en mémoire. Saisissez des doses, importez le jeu de données ou ouvrez un fichier WIN DL.")))
      shiny::tags$ul(style = "padding-left:18px;margin-bottom:0;",
        lapply(seq_along(cur), function(i) {
          e <- cur[[i]]
          shiny::tags$li(shiny::strong(names(cur)[i]), " — ",
                         trf("%d doses, témoin %g/%g", nrow(e$doses), e$x0, e$n0))
        }))
    })

    # -- Listes deroulantes -------------------------------------------------
    listes <- shiny::reactiveVal(hstat_dl50_listes_vides())

    # Les champs de la fiche puisent dans les listes, sans perdre ce qui y est
    # deja tape : `create = TRUE` accepte une valeur nouvelle, et la relire
    # avant de reconstruire les choix evite de l'effacer sous le curseur.
    shiny::observe({
      L <- listes()
      for (nm in names(HSTAT_DL50_CHAMPS_LISTE)) {
        id <- paste0("ch_", nm)
        cur <- shiny::isolate(input[[id]]) %||% ""
        ch <- unique(c(L[[HSTAT_DL50_CHAMPS_LISTE[[nm]]]], cur))
        ch <- ch[nzchar(ch)]
        shiny::updateSelectizeInput(session, id, choices = ch, selected = cur,
                                    server = FALSE)
      }
    })

    shiny::observe({
      L <- listes()
      shiny::updateSelectInput(session, "listeSupprimer",
        choices = L[[input$listeNom %||% "auteur"]] %||% character(0))
    })

    shiny::observeEvent(input$listeAjouter, {
      nm <- input$listeNom %||% "auteur"
      v <- trimws(input$listeValeur %||% "")
      if (!nzchar(v)) return()
      L <- listes()
      avant <- length(L[[nm]])
      L[[nm]] <- hstat_dl50_liste_ajouter(L[[nm]], v)
      listes(L)
      shiny::updateTextInput(session, "listeValeur", value = "")
      # Le doublon n'est pas une erreur, mais le taire ferait croire a un ajout.
      shiny::showNotification(
        if (length(L[[nm]]) > avant) trf("« %s » ajouté à la liste.", v)
        else trf("« %s » figure déjà dans la liste : rien n'a été ajouté.", v),
        type = "message", duration = 5)
    })

    shiny::observeEvent(input$listeRetirer, {
      nm <- input$listeNom %||% "auteur"
      v <- input$listeSupprimer
      if (is.null(v) || !nzchar(v)) return()
      L <- listes()
      L[[nm]] <- hstat_dl50_liste_retirer(L[[nm]], v)
      listes(L)
    })

    shiny::observeEvent(input$listeRegenerer, {
      cur <- essais()
      if (!length(cur)) {
        shiny::showNotification(
          tr("Aucun essai en mémoire : il n'y a rien d'où régénérer les listes."),
          type = "warning", duration = 6)
        return()
      }
      L <- hstat_dl50_regenerer(listes(), cur)
      listes(L)
      shiny::showNotification(
        trf("Listes régénérées depuis %d essai(s). Vérifiez leur contenu : deux orthographes d'un même terme y figureraient toutes les deux.",
            length(cur)), type = "message", duration = 10)
    })

    shiny::observeEvent(input$listeFichier, {
      f <- input$listeFichier
      shiny::req(f)
      v <- hstat_dl50_liste_lire(f$datapath[1])
      if (!length(v)) {
        shiny::showNotification(tr("Fichier de liste vide ou illisible."),
                                type = "warning", duration = 8)
        return()
      }
      nm <- input$listeNom %||% "auteur"
      L <- listes()
      L[[nm]] <- hstat_dl50_liste_ajouter(L[[nm]], v)
      listes(L)
      shiny::showNotification(trf("%d valeur(s) lue(s) dans le fichier.", length(v)),
                              type = "message", duration = 6)
    })

    output$listeDl <- shiny::downloadHandler(
      filename = function() paste0(toupper(input$listeNom %||% "liste"), ".TXT"),
      content = function(file)
        hstat_dl50_liste_ecrire(listes()[[input$listeNom %||% "auteur"]] %||%
                                  character(0), file))

    # -- Selection multi-criteres -------------------------------------------
    # NULL = aucune selection active, ce qui n'est PAS la meme chose qu'une
    # selection vide : la premiere laisse tous les essais, la seconde n'en
    # laisse aucun, et l'ecran doit distinguer les deux.
    selection <- shiny::reactiveVal(NULL)

    # Les criteres proposes sont ceux qui EXISTENT dans les essais charges,
    # pas le vocabulaire entier : offrir une espece absente du fonds ne peut
    # que rendre zero essai.
    shiny::observe({
      cur <- essais()
      val <- function(champ) sort(unique(Filter(nzchar, vapply(cur, function(e)
        trimws(as.character(e$champs[[champ]] %||% "")), character(1)))))
      for (p in list(c("selAuteur", "auteur"), c("selEspece", "espece"),
                     c("selStade", "stade"), c("selDuree", "duree"),
                     c("selMethode", "methode"), c("selUnite", "unite"),
                     c("selMa1", "matiere1"), c("selMa2", "matiere2")))
        shiny::updateSelectizeInput(session, p[1], choices = val(p[2]),
          selected = shiny::isolate(input[[p[1]]]), server = FALSE)
    })

    shiny::observeEvent(input$selValider, {
      cur <- essais()
      if (!length(cur)) return()
      tp <- c(.hstat_num1(input$selTempMin, NA_real_),
              .hstat_num1(input$selTempMax, NA_real_))
      tp <- tp[is.finite(tp)]
      selection(hstat_dl50_selection(cur, list(
        auteur = input$selAuteur, espece = input$selEspece,
        stade = input$selStade, duree = input$selDuree,
        methode = input$selMethode, unite = input$selUnite,
        matiere1 = input$selMa1, matiere2 = input$selMa2,
        ratio = input$selRatio, avec_ma2 = isTRUE(input$selAvecMa2),
        temperature = tp)))
    })
    shiny::observeEvent(input$selAnnuler, selection(NULL))

    # La selection s'applique aux essais RETENUS : c'est elle qui decide de ce
    # qui est analyse, compare et fusionne.
    essais_retenus <- shiny::reactive({
      cur <- essais(); s <- selection()
      if (is.null(s)) return(cur)
      cur[intersect(names(cur), s)]
    })

    output$selResultat <- shiny::renderUI({
      s <- selection()
      if (is.null(s))
        return(bandeau(trf("Aucune sélection active : les %d essai(s) en mémoire sont analysés.",
                           length(essais())), "info"))
      if (!length(s))
        return(bandeau(tr("Aucun essai ne correspond aux critères : la sélection est vide. Élargissez les critères ou annulez-la.")))
      bandeau(trf("%d essai(s) retenu(s) sur %d.", length(s), length(essais())), "info")
    })

    output$selTable <- DT::renderDT({
      cur <- essais_retenus()
      shiny::validate(shiny::need(length(cur) > 0,
        tr("Aucun essai retenu.")))
      d <- do.call(rbind, lapply(seq_along(cur), function(i) {
        e <- cur[[i]]
        g <- function(nm) trimws(as.character(e$champs[[nm]] %||% ""))
        data.frame(Essai = names(cur)[i], Espece = g("espece"),
                   Matiere = g("matiere1"), Methode = g("methode"),
                   Temperature = g("temperature"), Doses = nrow(e$doses),
                   stringsAsFactors = FALSE)
      }))
      DT::datatable(d, rownames = FALSE,
        colnames = c(tr("Essai"), tr("Espèce"), tr("Matière active"),
                     tr("Méthode de traitement"), tr("Température (°C)"),
                     tr("Doses")),
        options = list(dom = "t", pageLength = 10, ordering = FALSE,
                       scrollX = TRUE))
    })

    # -- L'ajustement -------------------------------------------------------
    essai_actif <- shiny::reactive({
      cur <- essais_retenus(); nm <- input$essaiActif
      if (!length(cur)) return(NULL)
      if (is.null(nm) || !(nm %in% names(cur))) return(cur[[1]])
      cur[[nm]]
    })

    fit <- shiny::reactive({
      e <- essai_actif()
      if (is.null(e)) return(NULL)
      al <- .hstat_num1(input$alpha, 0.05)
      al <- min(max(al, 0.001), 0.2)
      hstat_dl50_ajuste(e, input$methode %||% "em", alpha = al,
                        itmax = .hstat_num1(input$itmax, HSTAT_DL50_ITMAX),
                        chemin = input$chemin %||% "rapide")
    })

    output$messageFit <- shiny::renderUI({
      f <- fit()
      if (is.null(f))
        return(bandeau(tr("Aucun essai en mémoire : commencez par l'onglet « L'essai »."), "info"))
      if (!isTRUE(f$ok)) return(bandeau(f$message))
      if (!isTRUE(f$converge))
        return(bandeau(trf("L'estimation n'a pas convergé en %d itérations : les résultats ci-dessous sont ceux de la dernière itération.",
                           f$iterations)))
      NULL
    })

    # LE CHIFFRE QU'ON VIENT CHERCHER, EN PREMIER ET EN GRAND. Il vivait au bas
    # d'un tableau de dix colonnes, sans son unite : on ouvre un module de DL50
    # pour lire une DL50.
    output$verdict <- shiny::renderUI({
      f <- fit()
      if (is.null(f) || !isTRUE(f$ok)) return(NULL)
      v <- hstat_dl50_verdict(f, seuils_demandes())
      if (is.null(v)) return(NULL)
      shiny::div(
        style = paste0("background:", if (length(v$alertes)) "#fdf6e3" else "#eaf6ec",
                       ";border-left:5px solid ",
                       if (length(v$alertes)) "#b8860b" else "#2e7d43",
                       ";border-radius:6px;padding:14px 18px;margin-bottom:16px;"),
        shiny::div(style = paste0("font-family:'IBM Plex Mono',Consolas,monospace;",
                                  "font-size:19px;font-weight:600;color:#1b4332;",
                                  "letter-spacing:-.01em;"),
                   v$texte),
        shiny::div(style = "margin-top:6px;font-size:13.5px;color:#4b5563;",
                   v$ajustement),
        if (length(v$alertes))
          shiny::div(style = "margin-top:8px;font-size:13px;color:#8a6d1f;",
                     lapply(v$alertes, function(a)
                       shiny::div(shiny::icon("triangle-exclamation"), " ", a))))
    })

    output$resume <- shiny::renderUI({
      f <- fit()
      if (is.null(f) || !isTRUE(f$ok)) return(NULL)
      v <- hstat_p_verdict(f$p_chi2)
      shiny::tagList(
        shiny::p(shiny::strong(tr("Équation :")), " ",
                 sprintf("Y = %.5f + %.5f × log10(dose)", f$a, f$b)),
        shiny::p(shiny::strong(tr("Mortalité naturelle estimée :")), " ",
                 sprintf("%.2f %%", 100 * f$c),
                 sprintf(" (témoin : %.2f %%)", 100 * f$c_temoin)),
        shiny::p(shiny::strong(tr("Test d'ajustement :")), " ",
                 sprintf("Chi-2 = %.3f, ddl = %d, p = %.4f", f$chi2, f$ddl, f$p_chi2),
                 " — ", switch(v,
                   significatif = trf("ajustement rejeté à 5 %% : facteur d'hétérogénéité %.3f appliqué aux variances, quantile de Student retenu.",
                                      f$facteur),
                   `non significatif` = tr("ajustement probit légitime."),
                   tr("test non calculable."))),
        shiny::p(shiny::strong(tr("Convergence :")), " ",
                 trf("%d itérations", f$iterations)))
    })

    # TOUS les parametres statistiques dans UN tableau : un chiffre lu dans un
    # paragraphe ne se recopie pas dans un rapport, et ne s'exporte pas. Le
    # resume en prose reste au-dessus pour la lecture, le tableau porte les
    # valeurs -- et c'est lui qui part en CSV et en Excel.
    parametres <- shiny::reactive({
      f <- fit()
      if (is.null(f) || !isTRUE(f$ok)) return(NULL)
      sigma <- if (is.finite(f$b) && f$b != 0) 1 / abs(f$b) else NA_real_
      lig <- function(p, v, e = NA_real_, u = "") {
        data.frame(Parametre = p, Valeur = v, Erreur_type = e, Unite = u,
                   stringsAsFactors = FALSE)
      }
      do.call(rbind, list(
        lig(tr("Terme constant (a)"), f$a, sqrt(f$Vh[1, 1]), tr("probit")),
        lig(tr("Pente (b)"), f$b, sqrt(f$Vh[2, 2]), tr("probit / log10(dose)")),
        lig(tr("Mortalité naturelle (c)"), f$c, sqrt(f$Vh[3, 3]), tr("proportion")),
        lig(tr("Mortalité naturelle du témoin"), f$c_temoin, NA_real_, tr("proportion")),
        lig(tr("Écart-type des tolérances (1/b)"), sigma, NA_real_, tr("log10(dose)")),
        lig(tr("Covariance a-b (Vab)"), f$Vh[1, 2]),
        lig(tr("Covariance a-c (Vac)"), f$Vh[1, 3]),
        lig(tr("Covariance b-c (Vbc)"), f$Vh[2, 3]),
        lig(tr("Log-vraisemblance du modèle (H0)"), f$ll0),
        lig(tr("Log-vraisemblance saturée (H1)"), f$ll1),
        lig(tr("Chi-2 d'ajustement"), f$chi2),
        lig(tr("Degrés de liberté"), f$ddl),
        lig(tr("Probabilité de dépassement du Chi-2"), f$p_chi2),
        lig(tr("Facteur d'hétérogénéité"), f$facteur),
        lig(tr("Quantile employé pour les intervalles"), f$t),
        lig(tr("Risque α"), f$alpha),
        lig(tr("Nombre de doses"), f$n_doses),
        lig(tr("Doses à 0 % après correction"), f$n_zero),
        lig(tr("Doses à 100 % après correction"), f$n_cent),
        # LE COMPTE D'ITERATIONS N'EST PAS COMPARABLE A CELUI DE WIN DL, et
        # le libelle le dit plutot que de laisser croire a un desaccord. Les
        # deux algorithmes atteignent le meme optimum -- a, b et c coincident
        # a six chiffres -- mais pas au meme rythme : sur l'essai de reference,
        # HStat converge en 3 boucles EM (6 iterations de Newton-Raphson
        # cumulees) la ou le logiciel en annonce 75.
        #
        # La cause est mesuree : la mortalite naturelle y vaut exactement zero,
        # et HStat part de la valeur du temoin -- ici 0/25, donc zero. L'etape
        # [E] rend alors des poids nuls et c ne bouge plus. WIN DL, lui, rampe
        # vers cette borne. Reproduire son compte demanderait de ralentir
        # deliberement l'algorithme pour une valeur d'affichage.
        lig(tr("Itérations (boucles EM de HStat)"), f$iterations),
        lig(tr("Plafond d'itérations"), f$itmax)))
    })

    output$parametres <- DT::renderDT({
      d <- parametres()
      shiny::validate(shiny::need(!is.null(d), tr("Aucun résultat à afficher.")))
      # Le tableau melange des grandeurs (2,19759), des probabilites (0,05) et
      # des ENTIERS (5 degres de liberte). Un format a decimales fixes ecrivait
      # « 5.00000 » degres de liberte -- on doute d'un chiffre affiche comme
      # s'il avait cinq decimales. Le tableau EXPORTE reste numerique ; seule
      # sa mise en forme change ici.
      aff <- d
      for (k in c("Valeur", "Erreur_type"))
        aff[[k]] <- ifelse(is.finite(d[[k]]),
                           trimws(formatC(signif(d[[k]], chiffres()), format = "g",
                                          digits = chiffres())), "")
      DT::datatable(aff, rownames = FALSE,
        colnames = c(tr("Paramètre"), tr("Valeur"), tr("Erreur-type"), tr("Unité")),
        options = list(dom = "tp", pageLength = 20, ordering = FALSE,
                       scrollX = TRUE,
                       columnDefs = list(list(className = "dt-right",
                                              targets = c(1, 2)))))
    })

    # LES SEUILS PARTENT TELS QUE SAISIS. Les filtrer ici privait
    # `hstat_dl50_doses_letales()` de la possibilite de NOMMER ce qu'elle
    # ecarte : taper « 0, 50, 120 » rendait une seule ligne, sans un mot sur
    # les deux autres. Le champ VIDE, lui, reste un cas normal -- on retombe
    # sur les trois seuils d'usage plutot que d'annoncer un refus a quelqu'un
    # qui n'a rien demande.
    seuils_demandes <- shiny::reactive({
      v <- .nombres(input$seuils)
      if (!length(v)) HSTAT_DL50_SEUILS else v
    })

    doses_letales <- shiny::reactive({
      f <- fit()
      if (is.null(f) || !isTRUE(f$ok)) return(NULL)
      hstat_dl50_doses_letales(f, seuils_demandes())
    })

    # Un seuil ecarte, ou une liste vide, se dit : le tableau se contenterait
    # de rendre moins de lignes qu'on en a demande.
    output$noteSeuils <- shiny::renderUI({
      d <- doses_letales()
      msg <- c(attr(d, "message"), attr(d, "ecartes"))
      msg <- msg[!is.na(msg) & nzchar(msg)]
      if (!length(msg)) return(NULL)
      shiny::div(class = "callout callout-warning",
                 style = "margin-bottom:8px;padding:8px 12px;font-size:0.92em;",
        shiny::icon("triangle-exclamation"), " ", paste(msg, collapse = " "))
    })

    output$dosesLetales <- DT::renderDT({
      d <- doses_letales()
      shiny::validate(shiny::need(!is.null(d), tr("Aucun résultat à afficher.")))
      DT::datatable(d, rownames = FALSE,
        colnames = c(tr("Seuil (%)"), tr("log10(dose)"), hstat_dl50_libelle_dose(fit()),
                     tr("Erreur-type"), tr("Écart-type"),
                     tr("DL ± erreur-type"), tr("DL ± écart-type"),
                     tr("Limite inférieure"), tr("Limite supérieure"),
                     tr("Type d'intervalle"), tr("Position")),
        options = list(dom = "t", pageLength = 20, ordering = FALSE,
                       scrollX = TRUE)) |>
        DT::formatSignif(c("Log_dose", "Dose", "Erreur_type", "Ecart_type",
                           "Limite_inf", "Limite_sup"), chiffres())
    })

    output$detail <- DT::renderDT({
      f <- fit()
      shiny::validate(shiny::need(!is.null(f) && isTRUE(f$ok),
                                  tr("Aucun résultat à afficher.")))
      DT::datatable(f$table, rownames = FALSE,
        colnames = c("N", hstat_dl50_libelle_dose(f), tr("Effectif testé"), tr("Morts"),
                     tr("log10(dose)"), tr("Mortalité observée"),
                     tr("Mortalité corrigée"), tr("Probit corrigé"),
                     tr("Mortalité attendue"), tr("Probit attendu")),
        options = list(dom = "tp", pageLength = 25, ordering = FALSE,
                       scrollX = TRUE)) |>
        DT::formatSignif(c("Dose", "Log_dose", "Mortalite_observee",
                           "Mortalite_corrigee", "Probit_corrige",
                           "Mortalite_attendue", "Probit_attendu"), chiffres())
    })

    tables_export <- function() {
      f <- fit()
      if (is.null(f) || !isTRUE(f$ok)) return(NULL)
      list("Parametres" = parametres(),
           "Doses_letales" = doses_letales(),
           "Detail_par_dose" = f$table)
    }
    output$dlCsv  <- hstat_csv_handler(tables_export, "dl50")
    output$dlXlsx <- hstat_classeur_handler(tables_export, "dl50")

    output$dlPrn <- shiny::downloadHandler(
      filename = function() paste0("dl50_", Sys.Date(), ".prn"),
      content = function(file) {
        f <- fit()
        # Un `stop()` ici ne laisserait AUCUN fichier, et le navigateur
        # enregistrerait la page d'erreur de Shiny sous le nom demande : on
        # ecrit le motif dans le fichier plutot que de faire croire a un
        # rapport.
        lignes <- if (is.null(f) || !isTRUE(f$ok))
          tr("Aucun résultat : l'ajustement n'a pas abouti.")
        else hstat_dl50_prn(f, nom_fichier = input$essaiActif %||% "")
        writeLines(lignes, file, useBytes = TRUE)
      })

    output$dlTxt <- shiny::downloadHandler(
      filename = function() paste0("essai_", Sys.Date(), ".txt"),
      content = function(file) {
        e <- essai_actif()
        if (is.null(e)) {
          writeLines(tr("Aucun essai en mémoire."), file)
          return(invisible(NULL))
        }
        hstat_dl50_ecrire_windl(e, file)
      })

    # -- Abbott ou EM ? -----------------------------------------------------
    test_ae <- shiny::eventReactive(input$testAbbott, {
      e <- essai_actif()
      if (is.null(e)) return(list(ok = FALSE, message = tr("Aucun essai en mémoire.")))
      hstat_dl50_test_abbott_em(e)
    })

    output$resTestAbbott <- shiny::renderUI({
      r <- test_ae()
      if (!isTRUE(r$ok)) return(bandeau(r$message))
      shiny::div(style = "margin-top:10px;",
        shiny::p(sprintf("c (EM) = %.4f — c (Abbott) = %.4f", r$c_em, r$c_abbott)),
        shiny::p(sprintf("Chi-2 = %.4f, ddl = %d, p = %.4f", r$chi2, r$ddl, r$p)),
        bandeau(r$conseil, if (identical(r$verdict, "significatif"))
          "warning" else "info"))
    })

    # -- Graphique ----------------------------------------------------------
    fits_graphe <- shiny::reactive({
      cur <- essais_retenus()
      if (!length(cur)) return(list())
      al <- min(max(.hstat_num1(input$alpha, 0.05), 0.001), 0.2)
      m <- input$methode %||% "em"
      choisis <- if (isTRUE(input$gTous)) cur else {
        e <- essai_actif()
        if (is.null(e)) list() else list(e)
      }
      lapply(choisis, function(e) hstat_dl50_ajuste(
        e, m, alpha = al, itmax = .hstat_num1(input$itmax, HSTAT_DL50_ITMAX),
        chemin = input$chemin %||% "rapide"))
    })

    # TOUS les reglages sont lus ICI, dans le reactif qui construit l'image :
    # un reglage que le reactif n'observe pas se change sans que rien ne bouge
    # a l'ecran, et l'utilisateur conclut que le bouton ne marche pas.
    graphe_opt <- shiny::reactive({
      nb <- function(id, defaut) .hstat_num1(input[[id]], defaut)
      list(
        type = input$gType %||% "probit",
        points = isTRUE(input$gPoints), courbe = isTRUE(input$gCourbe),
        droite = isTRUE(input$gDroite), bande = isTRUE(input$gBande),
        reperes = isTRUE(input$gReperes),
        titre = input$gTitre %||% "", sous_titre = input$gSousTitre %||% "",
        xlab = input$gXlab %||% "", ylab = input$gYlab %||% "",
        ylab2 = input$gYlab2 %||% "",
        titre_taille = nb("gTitreTaille", 15),
        titre_style = input$gTitreStyle %||% "bold",
        titre_pos = nb("gTitrePos", 0.5),
        sous_titre_taille = nb("gSousTitreTaille", 12),
        sous_titre_style = input$gSousTitreStyle %||% "italic",
        sous_titre_pos = nb("gSousTitrePos", 0.5),
        axe_titre_taille = nb("gAxeTitreTaille", 12),
        axe_titre_style = input$gAxeTitreStyle %||% "plain",
        grad_x_taille = nb("gGradXTaille", 10),
        grad_x_style = input$gGradXStyle %||% "plain",
        grad_x_angle = nb("gGradXAngle", 0),
        grad_y_taille = nb("gGradYTaille", 10),
        grad_y_style = input$gGradYStyle %||% "plain",
        legende_pos = input$gLegendePos %||% "right",
        legende_taille = nb("gLegendeTaille", 10),
        legende_titre_taille = nb("gLegendeTitreTaille", 11),
        legende_titre = input$gLegendeTitre %||% "",
        point_taille = nb("gPointTaille", 2.4),
        point_forme = input$gPointForme %||% "16",
        point_opacite = nb("gPointOpacite", 1),
        droite_epaisseur = nb("gDroiteEp", 0.9),
        droite_type = input$gDroiteType %||% "solid",
        courbe_epaisseur = nb("gCourbeEp", 0.6),
        courbe_type = input$gCourbeType %||% "dashed",
        bande_opacite = nb("gBandeOpacite", 0.12),
        repere_couleur = input$gRepereCouleur %||% "#6b7280",
        repere_type = input$gRepereType %||% "dotted",
        repere_epaisseur = nb("gRepereEp", 0.5),
        repere_etiquette = isTRUE(input$gRepereEtiq),
        theme = input$gTheme %||% "minimal",
        base_size = nb("gBaseSize", 12),
        palette = input$gPalette %||% "Set1",
        couleur = input$gCouleur %||% "#2e86c1",
        grille = isTRUE(input$gGrille), axe2 = isTRUE(input$gAxe2),
        x_min = nb("gXmin", NA_real_), x_max = nb("gXmax", NA_real_),
        y_min = nb("gYmin", NA_real_), y_max = nb("gYmax", NA_real_))
    })

    graphe <- shiny::reactive({
      fs <- fits_graphe()
      if (!length(fs)) return(NULL)
      hstat_dl50_graphique(fs, graphe_opt())
    })

    # Le nom de la boite suit le type choisi. `HSTAT_DL50_GRAPHES` en est la
    # seule source : recopier « Droite de Henry » ici ferait deux libelles a
    # tenir d'accord, et c'est toujours le second qui se perime.
    # Un point ecarte se nomme. Sans cela l'utilisateur compte six doses dans
    # son tableau et quatre sur son graphique, sans savoir lesquelles manquent
    # ni pourquoi -- et la reponse (« 0 % et 100 % n'ont pas de probit ») est
    # precisement ce qui doit l'amener a la courbe dose-reponse.
    output$gNote <- shiny::renderUI({
      g <- graphe()
      ec <- if (is.null(g)) NULL else attr(g, "ecartes")
      if (!length(ec)) return(NULL)
      shiny::div(class = "callout callout-warning",
                 style = "margin-top:10px;padding:8px 12px;",
        shiny::icon("circle-info"), " ",
        trf("Mortalité corrigée nulle ou totale : ces doses n'ont pas de probit et ne figurent pas sur la droite de Henry — %s. La courbe dose-réponse les porte, elle.",
            paste(ec, collapse = " ; ")))
    })

    output$gNom <- shiny::renderText({
      t <- input$gType %||% "probit"
      i <- match(t, HSTAT_DL50_GRAPHES)
      tr(if (is.na(i)) names(HSTAT_DL50_GRAPHES)[1] else names(HSTAT_DL50_GRAPHES)[i])
    })

    output$graphe <- shiny::renderPlot({
      p <- graphe()
      shiny::validate(shiny::need(!is.null(p),
        tr("Aucun essai ajustable : vérifiez les doses et les effectifs.")))
      p
    })

    # LES PIXELS SAISIS SONT UNE MISE EN PAGE, PAS LA SORTIE. Ils sont lus a la
    # resolution de l'ecran (96 ppp) ; le DPI multiplie ensuite la finesse. Le
    # decompte est affiche : ne montrer que la mise en page laisserait croire
    # qu'un DPI plus eleve ne change rien au fichier.
    dims_export <- shiny::reactive(
      hstat_export_dims(input$gLargeur, input$gHauteur, input$gDpi))

    output$gTaille <- shiny::renderText({
      d <- dims_export()
      fmt <- hstat_img_fmt(input$gFmt %||% "png")
      if (fmt %in% c("svg", "pdf", "eps"))
        return(trf("Mise en page %s × %s pouces. Format vectoriel : résolution illimitée, le DPI ne s'applique pas.",
                   round(d$width_in, 2), round(d$height_in, 2)))
      px <- d$width_out * d$height_out
      mo <- if (identical(fmt, "jpeg")) px * 0.3 / 1048576 else px * 3 / 1048576
      paste0(trf("Mise en page %s × %s pouces → fichier de %s × %s pixels à %s DPI",
                 round(d$width_in, 2), round(d$height_in, 2),
                 d$width_out, d$height_out, d$dpi),
             " | ", trf("Taille estimée : %s Mo", round(mo, 2)))
    })

    output$gDl <- shiny::downloadHandler(
      filename = function()
        paste0("dl50_henry_", Sys.Date(), ".", hstat_img_fmt(input$gFmt %||% "png")),
      content = function(file) {
        p <- graphe()
        d <- dims_export()
        if (!is.null(d$note))
          shiny::showNotification(d$note, type = "warning", duration = 8)
        fmt <- hstat_img_fmt(input$gFmt %||% "png")
        # Un `stop()` ne laisserait AUCUN fichier : Shiny renverrait sa page
        # d'erreur, que le navigateur enregistrerait sous le nom demande. On
        # croit tenir une image, on ouvre du HTML.
        ok <- !is.null(p) && hstat_ecrire_image(
          file, p, fmt, d$width_in, d$height_in, d$dpi,
          qualite = .hstat_num1(input$gQualite, 95),
          compression = input$gCompression %||% "lzw")
        if (isTRUE(ok))
          shiny::showNotification(
            trf("Graphique exporté : %s, %s × %s pixels à %s DPI.",
                toupper(fmt), d$width_out, d$height_out, d$dpi),
            type = "message", duration = 6)
        else {
          writeLines(tr("Export impossible : aucun essai ajustable, ou dimensions trop grandes. Réduisez la taille ou le DPI, ou choisissez un format vectoriel (SVG, PDF)."),
                     file)
          shiny::showNotification(
            tr("Export impossible : aucun essai ajustable, ou dimensions trop grandes. Réduisez la taille ou le DPI, ou choisissez un format vectoriel (SVG, PDF)."),
            type = "error", duration = 10)
        }
      })

    # -- Comparaison et fusion ---------------------------------------------
    comparaison <- shiny::eventReactive(input$comparer, {
      cur <- essais_retenus()
      hstat_dl50_comparaison(unname(cur), input$scenario %||% "heterogene",
                             alpha = min(max(.hstat_num1(input$alpha, 0.05), 0.001), 0.2))
    })

    output$messageComp <- shiny::renderUI({
      r <- comparaison()
      shiny::tagList(
        bandeau(attr(r, "message")),
        if (nrow(r)) bandeau(attr(r, "avertissement"), "info"))
    })

    output$tableComp <- DT::renderDT({
      r <- comparaison()
      shiny::validate(shiny::need(nrow(r) > 0,
        tr("Chargez au moins deux essais, puis lancez la comparaison.")))
      DT::datatable(r, rownames = FALSE,
        colnames = c(tr("Hypothèse testée"), tr("Chi-2"), tr("ddl"), tr("p"),
                     tr("Conclusion")),
        options = list(dom = "t", pageLength = 10, ordering = FALSE,
                       scrollX = TRUE,
                       columnDefs = list(list(width = "38%", targets = 4)))) |>
        DT::formatSignif(c("Chi2", "p"), chiffres())
    })

    comp_tables <- function() {
      r <- comparaison()
      if (!nrow(r)) return(NULL)
      list("Comparaison" = as.data.frame(r))
    }
    output$compCsv  <- hstat_csv_handler(comp_tables, "dl50_comparaison")
    output$compXlsx <- hstat_classeur_handler(comp_tables, "dl50_comparaison")

    # -- Rapport de puissance ------------------------------------------------
    shiny::observe({
      nm <- names(essais_retenus())
      shiny::updateSelectInput(session, "puiRef", choices = nm,
        selected = if (length(nm)) {
          a <- shiny::isolate(input$puiRef)
          if (!is.null(a) && a %in% nm) a else nm[1]
        } else NULL)
    })

    puissance <- shiny::eventReactive(input$puiGo, {
      cur <- essais_retenus()
      i <- match(input$puiRef %||% "", names(cur))
      hstat_dl50_puissance(unname(cur), reference = if (is.na(i)) 1L else i,
                           scenario = input$scenario %||% "heterogene",
                           alpha = min(max(.hstat_num1(input$alpha, 0.05), 0.001), 0.2))
    })

    output$messagePui <- shiny::renderUI({
      r <- puissance()
      if (!nrow(r)) return(bandeau(attr(r, "message")))
      pa <- attr(r, "parallelisme")
      shiny::tagList(
        bandeau(attr(r, "avertissement")),
        shiny::div(style = "margin:8px 0;font-size:14px;color:#4b5563;",
          shiny::strong(trf("Référence : %s.", attr(r, "reference"))), " ",
          trf("Pente commune : %.4f.", attr(r, "pente_commune")), " ",
          if (!is.na(pa$Chi2))
            trf("Parallélisme : Chi-2 = %.3f, ddl = %d, p = %.4f — %s",
                pa$Chi2, pa$DDL, pa$p, pa$Conclusion)
          else pa$Conclusion))
    })

    output$tablePui <- DT::renderDT({
      r <- puissance()
      shiny::validate(shiny::need(nrow(r) > 0,
        tr("Chargez au moins deux essais, choisissez la référence, puis lancez le calcul.")))
      DT::datatable(as.data.frame(r), rownames = FALSE,
        colnames = c(tr("Essai"), tr("Référence"), tr("DL50"), tr("Rapport"),
                     tr("Limite inférieure"), tr("Limite supérieure"),
                     tr("Type d'intervalle")),
        options = list(dom = "t", pageLength = 10, ordering = FALSE,
                       scrollX = TRUE)) |>
        DT::formatSignif(c("DL50", "Rapport", "Limite_inf", "Limite_sup"), chiffres())
    })

    pui_tables <- function() {
      r <- puissance()
      if (!nrow(r)) return(NULL)
      list("Rapport_de_puissance" = as.data.frame(r),
           "Parallelisme" = as.data.frame(attr(r, "parallelisme")))
    }
    output$puiCsv  <- hstat_csv_handler(pui_tables, "dl50_puissance")
    output$puiXlsx <- hstat_classeur_handler(pui_tables, "dl50_puissance")

    shiny::observeEvent(input$fusionner, {
      cur <- essais_retenus()
      if (length(cur) < 2L) {
        shiny::showNotification(tr("La fusion demande au moins deux essais."),
                                type = "warning", duration = 6)
        return()
      }
      r <- hstat_dl50_fusion(unname(cur),
                             alpha = min(max(.hstat_num1(input$alpha, 0.05), 0.001), 0.2))
      if (!isTRUE(r$ok)) {
        shiny::showNotification(r$message, type = "warning", duration = 12)
        return()
      }
      # La fusion REMPLACE les essais qu'elle assemble : les garder a cote
      # ferait comparer un essai a ses propres composantes.
      essais(stats::setNames(list(r$essai), r$essai$titre))
      shiny::showNotification(r$message, type = "message", duration = 12)
    })

    # -- Dose <-> mortalite -------------------------------------------------
    table_mort <- shiny::reactive({
      f <- fit()
      if (is.null(f) || !isTRUE(f$ok)) return(NULL)
      hstat_dl50_mortalite(f, .nombres(input$dosesCalc))
    })
    output$tableMort <- DT::renderDT({
      d <- table_mort()
      shiny::validate(shiny::need(!is.null(d), tr("Saisissez au moins une dose strictement positive.")))
      DT::datatable(d, rownames = FALSE,
        colnames = c(hstat_dl50_libelle_dose(fit()), tr("log10(dose)"),
                     tr("Probit attendu"), tr("Erreur-type"), tr("Mortalité"),
                     tr("Limite inférieure"), tr("Limite supérieure"),
                     tr("Position")),
        options = list(dom = "t", pageLength = 20, ordering = FALSE,
                       scrollX = TRUE)) |>
        DT::formatSignif(c("Dose", "Log_dose", "Probit_attendu", "Erreur_type",
                           "Mortalite", "Limite_inf", "Limite_sup"), chiffres())
    })

    table_dose <- shiny::reactive({
      f <- fit()
      if (is.null(f) || !isTRUE(f$ok)) return(NULL)
      d <- hstat_dl50_dose_pour(f, .nombres(input$mortsCalc))
      if (is.null(d)) return(NULL)
      d[c("Mortalite_demandee", "Log_dose", "Dose", "Erreur_type", "Ecart_type",
          "DL_erreur_type", "DL_ecart_type", "Limite_inf", "Limite_sup",
          "Intervalle", "Position")]
    })
    output$tableDose <- DT::renderDT({
      d <- table_dose()
      shiny::validate(shiny::need(!is.null(d),
        tr("Aucune mortalité exploitable : elle doit dépasser la mortalité naturelle et rester sous 100 %.")))
      DT::datatable(d, rownames = FALSE,
        colnames = c(tr("Mortalité visée (%)"), tr("log10(dose)"),
                     hstat_dl50_libelle_dose(fit()), tr("Erreur-type"),
                     tr("Écart-type"), tr("DL ± erreur-type"),
                     tr("DL ± écart-type"), tr("Limite inférieure"),
                     tr("Limite supérieure"), tr("Type d'intervalle"),
                     tr("Position")),
        options = list(dom = "t", pageLength = 20, ordering = FALSE,
                       scrollX = TRUE)) |>
        DT::formatSignif(c("Log_dose", "Dose", "Erreur_type", "Ecart_type",
                           "Limite_inf", "Limite_sup"), chiffres())
    })

    # Chaque tableau de resultats s'exporte : un chiffre qu'on ne peut pas
    # sortir de l'ecran ne sert qu'a l'ecran.
    output$mortCsv  <- hstat_csv_handler(function() {
      d <- table_mort(); if (is.null(d)) NULL else list("Mortalite_par_dose" = d)
    }, "dl50_mortalite")
    output$mortXlsx <- hstat_classeur_handler(function() {
      d <- table_mort(); if (is.null(d)) NULL else list("Mortalite_par_dose" = d)
    }, "dl50_mortalite")
    output$doseCsv  <- hstat_csv_handler(function() {
      d <- table_dose(); if (is.null(d)) NULL else list("Dose_par_mortalite" = d)
    }, "dl50_dose")
    output$doseXlsx <- hstat_classeur_handler(function() {
      d <- table_dose(); if (is.null(d)) NULL else list("Dose_par_mortalite" = d)
    }, "dl50_dose")

    # -- L'assistance observe, elle n'instrumente pas -----------------------
    shiny::observeEvent(fit(), {
      f <- fit()
      if (is.null(f) || !isTRUE(f$ok)) return()
      dl <- doses_letales()
      hstat_ai_capture(values, "DL50 / CL50",
        trf("Régression probit%s", {
          t <- f$essai$titre
          if (length(t) && nzchar(t)) paste0(" -- ", t) else ""
        }),
        tables = list("Doses_letales" = dl, "Detail_par_dose" = f$table),
        plot = function() shiny::isolate(graphe()),
        # Les doses voyagent avec la capture : c'est la seule analyse dont les
        # donnees ne viennent pas du fichier de travail, et sans elles le
        # journal de reproductibilite n'a rien a reconstituer.
        meta = list(a = f$a, b = f$b, c = f$c, methode = f$methode,
                    chi2 = f$chi2, ddl = f$ddl, p = f$p_chi2,
                    doses = f$essai$doses$dose, effectifs = f$essai$doses$n,
                    morts = f$essai$doses$x,
                    temoin_n = f$essai$n0, temoin_x = f$essai$x0,
                    unite = hstat_dl50_unite(f)))
    }, ignoreInit = TRUE)
  })
}
