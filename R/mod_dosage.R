# =============================================================================
#  DOSES ET DILUTIONS  --  mod_dosage.R
# -----------------------------------------------------------------------------
#  Trois calculs que tout essai phytosanitaire refait a la main, sur un coin de
#  table, a chaque campagne :
#
#   1. la DOSE de produit commercial a apporter, connaissant le grammage de
#      matiere active vise a l'hectare ;
#   2. le GRAMMAGE reellement apporte, connaissant la dose homologuee -- et ce
#      que cela donne dans la cuve, puisque c'est la que se fait l'erreur ;
#   3. les solutions FILLES obtenues d'une solution mere par un coefficient de
#      dilution, avec le volume a prelever et l'eau a ajouter.
#
#  Les trois reposent sur deux egalites, et sur rien d'autre :
#
#      grammage = dose x concentration            (conservation de la matiere)
#      C_mere x V_preleve = C_fille x V_final     (conservation du solute)
#
#  D'ou une regle de conduite pour tout ce fichier : CHAQUE resultat porte la
#  formule qui l'a produit. Un chiffre de dose qu'on ne peut pas refaire a la
#  main ne sera pas applique au champ -- il sera recalcule, et c'est le
#  recalcul qui fera foi.
# =============================================================================

# --- Unites -----------------------------------------------------------------
# Facteur vers l'unite de reference : le LITRE pour un produit liquide, le
# KILOGRAMME pour un produit solide. Les deux familles partagent la meme
# arithmetique, seul le libelle change -- 400 g/L et 400 g/kg se manipulent
# de la meme facon.
HSTAT_DOSE_UNITES <- c("mL/ha" = 0.001, "L/ha" = 1, "g/ha" = 0.001, "kg/ha" = 1)

# Concentration de matiere active, ramenee a des grammes par litre (ou par
# kilogramme). Le pour-cent est entendu en masse/volume : 1 % = 1 g pour
# 100 mL = 10 g/L, la convention des etiquettes phytosanitaires.
HSTAT_CONC_UNITES <- c("g/L" = 1, "g/kg" = 1, "mg/L" = 0.001, "mg/kg" = 0.001,
                       "%" = 10, "ppm" = 0.001)

# Le MICROLITRE manquait, et c'est l'unite de la paillasse : une gamme au
# 1/10 depuis 100 mL descend a 0,01 mL des le quatrieme etage -- un nombre que
# personne ne lit, alors que 10 uL se pipette. Le facteur est le meme partout :
# une valeur en litres.
HSTAT_VOL_UNITES <- c("µL" = 1e-6, "mL" = 0.001, "L" = 1)

# Les formules, ecrites une fois. Elles sont AFFICHEES a l'utilisateur et
# EXPORTEES avec les resultats : c'est le meme texte, il n'a donc pas deux
# versions qui pourraient diverger.
HSTAT_DOSE_FORMULES <- c(
  grammage   = "Grammage (g m.a./ha) = Dose (L ou kg/ha) × Concentration (g/L ou g/kg)",
  dose       = "Dose (L ou kg/ha) = Grammage (g m.a./ha) ÷ Concentration (g/L ou g/kg)",
  conc_bouil = "Concentration de la bouillie (g m.a./L) = Grammage (g m.a./ha) ÷ Volume de bouillie (L/ha)",
  dose_bouil = "Dose dans la bouillie (mL ou g de produit/L) = Dose (mL ou g/ha) ÷ Volume de bouillie (L/ha)",
  total_prod = "Quantité totale de produit = Dose (par ha) × Superficie (ha)",
  total_ma   = "Quantité totale de matière active = Grammage (g/ha) × Superficie (ha)",
  total_eau  = "Volume d'eau total = Volume de bouillie (L/ha) × Superficie (ha)",
  par_cuve   = "Produit par cuve = Dose (par ha) × (Volume de cuve (L) ÷ Volume de bouillie (L/ha))",
  nb_cuves   = "Nombre de cuves = Volume d'eau total (L) ÷ Volume de cuve (L)",
  surf_cuve  = "Surface par cuve (ha) = Volume de cuve (L) ÷ Volume de bouillie (L/ha)",
  conservation = "Conservation de la matière : Vi × Ci = Vf × Cf  (Vi prélevé dans la mère, Ci = concentration mère)",
  c_fille    = "Concentration de la fille n (Cf) = Concentration de la fille n−1 ÷ Coefficient de dilution",
  c_fille_n  = "soit Concentration de la fille n = Concentration mère ÷ Coefficient^n",
  v_preleve  = "Volume à prélever dans la mère (Vi) = Vf × Cf ÷ Concentration mère = Volume final ÷ Coefficient^n",
  v_eau      = "Volume d'eau à ajouter = Volume final (Vf) − Volume prélevé (Vi)",
  v_restant  = "Volume restant de la fille n (en cascade) = Volume final − Volume prélevé pour la fille n+1",
  v_mere     = "Volume de solution mère requis = somme des volumes prélevés sur la mère",
  c_totale   = "Concentration totale de la fille = somme des concentrations filles de chaque matière active")

# ---------------------------------------------------------------------------
#  Conversions : une seule porte d'entree
# ---------------------------------------------------------------------------
# Une unite inconnue ne doit JAMAIS valoir 1 par defaut : un facteur muet
# rendrait un resultat mille fois trop grand sans le moindre signe. On rend
# NA, et l'appelant le nomme.
.hstat_dose_facteur <- function(unite, table) {
  u <- as.character(unite)[1]
  if (is.na(u) || !nzchar(u) || !(u %in% names(table))) return(NA_real_)
  unname(table[[u]])
}

hstat_dose_vers_ref <- function(valeur, unite)
  suppressWarnings(as.numeric(valeur)[1]) * .hstat_dose_facteur(unite, HSTAT_DOSE_UNITES)

hstat_conc_vers_ref <- function(valeur, unite)
  suppressWarnings(as.numeric(valeur)[1]) * .hstat_dose_facteur(unite, HSTAT_CONC_UNITES)

hstat_vol_vers_ref <- function(valeur, unite)
  suppressWarnings(as.numeric(valeur)[1]) * .hstat_dose_facteur(unite, HSTAT_VOL_UNITES)

# Rendre une valeur de reference (L ou kg) dans l'unite demandee.
hstat_dose_depuis_ref <- function(valeur, unite) {
  f <- .hstat_dose_facteur(unite, HSTAT_DOSE_UNITES)
  if (!isTRUE(is.finite(f)) || f == 0) return(NA_real_)
  valeur / f
}

# Nombre lisible : `dec` decimales au plus, sans zeros inutiles. `format()`
# rendrait « 1.5e+05 » sur une grande surface, illisible sur une etiquette.
#
# DEUX DEFAUTS CORRIGES ICI, tous deux silencieux.
#
# 1. LE RETRAIT DES ZEROS MANGEAIT LES ENTIERS. `sub("\\.?0+$", "")` ne demande
#    pas de point : a zero decimale, « 1 000 » ressortait « 1 ». Le motif exige
#    donc desormais un point, en deux passes -- les zeros APRES un chiffre
#    significatif, puis une partie decimale entierement nulle.
#
# 2. UNE VALEUR NON NULLE QUI S'ARRONDIT A ZERO S'AFFICHAIT « 0 ». Sur une
#    gamme de dilution c'est le cas NORMAL, pas le cas rare : au 1/10 depuis
#    100 g/L, la cinquieme fille vaut 1e-3 et la septieme 1e-5. A quatre
#    decimales, l'ecran annoncait une concentration NULLE pour une solution
#    qui en a une. On bascule ces valeurs en notation scientifique : c'est
#    l'ecriture des chimistes, et elle dit la verite.
hstat_fmt_nb <- function(x, dec = 2) {
  v <- suppressWarnings(as.numeric(x))
  dec <- max(0L, as.integer(dec))
  out <- ifelse(is.finite(v),
                formatC(round(v, dec), format = "f", digits = dec, big.mark = " "),
                NA_character_)
  # Les zeros de fin ne se retirent QU'APRES un point decimal.
  out <- sub("(\\.[0-9]*[1-9])0+$", "\\1", out)
  out <- sub("\\.0+$", "", out)
  petit <- is.finite(v) & v != 0 & round(v, dec) == 0
  out[petit] <- formatC(v[petit], format = "e", digits = 2)
  out[is.na(out) | !nzchar(out)] <- NA_character_
  out
}

# ---------------------------------------------------------------------------
#  1 et 2. Dose <-> grammage
# ---------------------------------------------------------------------------
#  `sens` dit ce que l'utilisateur CONNAIT :
#    "dose"     -- il connait la dose homologuee, il cherche le grammage ;
#    "grammage" -- il connait le grammage vise, il cherche la dose.
#
#  Le reste (bouillie, cuve, superficie) se deduit de l'un comme de l'autre :
#  ce sont les memes multiplications, elles n'ont pas a etre ecrites deux fois.
# ---------------------------------------------------------------------------
hstat_dose_bilan <- function(sens = c("dose", "grammage"),
                             valeur, unite_valeur,
                             concentration, unite_concentration,
                             volume_bouillie = NA_real_,
                             superficie = 1,
                             volume_cuve = NA_real_,
                             produit = "", matiere = "") {
  sens <- match.arg(sens)
  vide <- function(motif) {
    r <- data.frame(Grandeur = character(0), Valeur = numeric(0),
                    Formule = character(0), stringsAsFactors = FALSE)
    attr(r, "message") <- motif
    r
  }

  conc <- hstat_conc_vers_ref(concentration, unite_concentration)
  if (!isTRUE(is.finite(conc)) || conc <= 0)
    return(vide(tr(paste0("Concentration en matière active absente ou nulle : renseignez-la ",
                          "(elle figure sur l'étiquette du produit) et vérifiez l'unité choisie."))))

  val <- suppressWarnings(as.numeric(valeur)[1])
  if (!isTRUE(is.finite(val)) || val <= 0)
    return(vide(tr("Valeur de départ absente ou nulle : saisissez une dose ou un grammage strictement positif.")))

  if (sens == "dose") {
    dose_ref <- hstat_dose_vers_ref(val, unite_valeur)
    if (!isTRUE(is.finite(dose_ref)))
      return(vide(tr("Unité de dose inconnue : choisissez mL/ha, L/ha, g/ha ou kg/ha.")))
    grammage <- dose_ref * conc
    u_dose   <- as.character(unite_valeur)[1]
    dose_aff <- val
  } else {
    grammage <- val                      # deja en g de matiere active par ha
    dose_ref <- grammage / conc
    # On rend la dose dans une unite lisible : le litre au-dela de 1, le
    # millilitre en dessous. Une dose de 0,0004 L/ha ne se lit pas.
    u_dose   <- if (dose_ref >= 1) "L/ha" else "mL/ha"
    dose_aff <- hstat_dose_depuis_ref(dose_ref, u_dose)
  }

  surf <- suppressWarnings(as.numeric(superficie)[1])
  if (!isTRUE(is.finite(surf)) || surf <= 0) surf <- 1
  vb   <- suppressWarnings(as.numeric(volume_bouillie)[1])
  cuve <- suppressWarnings(as.numeric(volume_cuve)[1])
  a_vb   <- isTRUE(is.finite(vb)) && vb > 0
  a_cuve <- a_vb && isTRUE(is.finite(cuve)) && cuve > 0

  liq   <- !(u_dose %in% c("g/ha", "kg/ha"))
  u_pet <- if (liq) "mL/ha" else "g/ha"          # petite unite du produit
  u_tot <- if (liq) "L" else "kg"

  # Bouillie : la ou se joue l'erreur de terrain. Sans volume declare, ces
  # lignes n'ont pas de valeur -- elles restent NA plutot que de sortir un
  # zero qu'on prendrait pour une mesure.
  conc_bouillie <- if (a_vb) grammage / vb else NA_real_
  dose_bouillie <- if (a_vb) hstat_dose_depuis_ref(dose_ref / vb, u_pet) else NA_real_
  eau_totale    <- if (a_vb) vb * surf else NA_real_
  par_cuve      <- if (a_cuve) hstat_dose_depuis_ref(dose_ref * (cuve / vb), u_pet) else NA_real_
  nb_cuves      <- if (a_cuve) eau_totale / cuve else NA_real_
  surf_cuve     <- if (a_cuve) cuve / vb else NA_real_

  s_aff <- hstat_fmt_nb(surf)
  res <- data.frame(
    Grandeur = c(
      trf("Dose de produit commercial (%s)", u_dose),
      "Grammage de matière active (g m.a./ha)",
      "Concentration de la bouillie (g m.a./L)",
      trf("Dose dans la bouillie (%s de produit/L)", if (liq) "mL" else "g"),
      trf("Quantité totale de produit pour %s ha (%s)", s_aff, u_tot),
      trf("Quantité totale de matière active pour %s ha (g)", s_aff),
      trf("Volume d'eau total pour %s ha (L)", s_aff),
      trf("Produit par cuve (%s)", if (liq) "mL" else "g"),
      "Nombre de cuves",
      "Surface traitée par cuve (ha)"),
    Valeur = c(dose_aff, grammage, conc_bouillie, dose_bouillie,
               dose_ref * surf, grammage * surf, eau_totale,
               par_cuve, nb_cuves, surf_cuve),
    Formule = c(
      if (sens == "dose") tr("(valeur saisie)") else HSTAT_DOSE_FORMULES[["dose"]],
      if (sens == "grammage") tr("(valeur saisie)") else HSTAT_DOSE_FORMULES[["grammage"]],
      HSTAT_DOSE_FORMULES[["conc_bouil"]],
      HSTAT_DOSE_FORMULES[["dose_bouil"]],
      HSTAT_DOSE_FORMULES[["total_prod"]],
      HSTAT_DOSE_FORMULES[["total_ma"]],
      HSTAT_DOSE_FORMULES[["total_eau"]],
      HSTAT_DOSE_FORMULES[["par_cuve"]],
      HSTAT_DOSE_FORMULES[["nb_cuves"]],
      HSTAT_DOSE_FORMULES[["surf_cuve"]]),
    stringsAsFactors = FALSE)

  attr(res, "produit")    <- as.character(produit)[1]
  attr(res, "matiere")    <- as.character(matiere)[1]
  attr(res, "grammage")   <- grammage
  attr(res, "dose_ref")   <- dose_ref
  attr(res, "unite")      <- u_dose
  attr(res, "superficie") <- surf
  res
}

# ---------------------------------------------------------------------------
#  3. Dilutions : solutions filles a partir d'une solution mere
# ---------------------------------------------------------------------------
#  L'unite de saisie est le couple (produit, matiere active) : un produit a
#  plusieurs matieres actives occupe plusieurs lignes. C'est ce qui permet de
#  detailler la concentration fille de CHAQUE matiere active tout en donnant
#  la concentration totale de la solution -- la question posee.
#
#  Le coefficient de dilution, le volume final et le NOMBRE DE FILLES
#  appartiennent au PRODUIT, pas a la matiere active : deux lignes du meme
#  produit qui les contrediraient decriraient deux preparations differentes.
#  On le refuse en nommant le produit, plutot que de retenir l'une des deux
#  valeurs au hasard.
#
#  UNE GAMME, ET LE PRELEVEMENT SE FAIT TOUJOURS DANS LA MERE :
#
#    C_n = C_(n-1) / k = C_mere / k^n        (chaque fille depuis la precedente)
#    V_preleve_n x C_mere = V_final x C_n    (conservation de la matiere)
#      d'ou  V_preleve_n = V_final / k^n
#
#  Le volume final est celui que l'utilisateur declare : c'est le volume de
#  CHAQUE fille, et le volume a prelever s'en deduit. Rien n'est preleve dans
#  une fille -- on revient a la mere a chaque etage, ce qui evite qu'une
#  erreur de pipetage se propage a toute la gamme.
#
#  Le piege de la methode est a l'autre bout : le volume a prelever est divise
#  par k a chaque etage et devient vite impipetable (100 mL au 1/10 donnent
#  0,001 mL au cinquieme etage). Le module le SIGNALE au lieu de rendre un
#  nombre que personne ne peut mesurer.
# ---------------------------------------------------------------------------
HSTAT_DILUTION_COLS <- c("Produit", "Matiere_active", "Concentration_mere",
                         "Unite", "Coefficient", "Volume_final", "Unite_volume")

# Le nombre de filles est FACULTATIF : absent, il vaut 1, ce qui redonne
# exactement le comportement d'avant la gamme. Un appel existant continue
# donc de rendre ce qu'il rendait.
HSTAT_DILUTION_NB_MAX <- 50L

# En dessous, le prelevement ne se mesure plus a la pipette. Ce n'est pas une
# erreur de calcul : c'est un plan de gamme irrealisable, et il vaut mieux le
# dire avant la paillasse qu'apres.
HSTAT_DILUTION_MIN_ML <- 0.01

# Au-dela, un nombre a virgule flottante double ne porte plus d'information :
# afficher une seizieme decimale, ce serait inventer de la precision. La borne
# est donc une VERITE de la representation, pas un choix d'interface.
HSTAT_DEC_MAX <- 15L

hstat_dilution_table_vide <- function(n = 3) {
  data.frame(
    Produit              = rep("", n),
    Matiere_active       = rep("", n),
    Concentration_mere   = rep(NA_real_, n),
    Concentration_depart = rep(NA_real_, n),
    Unite                = rep("g/L", n),
    Coefficient          = rep(NA_real_, n),
    Nb_filles            = rep(1, n),
    Volume_final         = rep(NA_real_, n),
    Unite_volume         = rep("L", n),
    stringsAsFactors     = FALSE)
}

hstat_dilution_calcul <- function(df) {
  vide <- function(motif) {
    r <- data.frame()
    attr(r, "message") <- motif
    r
  }
  if (!is.data.frame(df) || !nrow(df))
    return(vide(tr("Aucun produit saisi : ajoutez au moins une ligne (produit, matière active, concentration mère).")))
  manque <- setdiff(HSTAT_DILUTION_COLS, names(df))
  if (length(manque))
    return(vide(trf("Colonne(s) absente(s) du tableau : %s.", paste(manque, collapse = ", "))))

  d <- df[HSTAT_DILUTION_COLS]
  d$Produit        <- trimws(as.character(d$Produit))
  d$Matiere_active <- trimws(as.character(d$Matiere_active))
  d$Unite          <- trimws(as.character(d$Unite))
  d$Unite_volume   <- trimws(as.character(d$Unite_volume))
  for (k in c("Concentration_mere", "Coefficient", "Volume_final"))
    d[[k]] <- suppressWarnings(as.numeric(d[[k]]))
  # Facultatif : absent, il vaut 1 -- le comportement d'avant la gamme.
  d$Nb_filles <- if ("Nb_filles" %in% names(df))
    suppressWarnings(as.numeric(df$Nb_filles)) else rep(1, nrow(d))
  d$Nb_filles[!is.finite(d$Nb_filles)] <- 1

  # LA CONCENTRATION DE DEPART EST FACULTATIVE, et c'est tout son interet : une
  # gamme ne commence pas toujours a C_mere/k. Absente, le premier etage vaut
  # le coefficient courant et le calcul est EXACTEMENT celui d'avant -- un
  # tableau existant continue donc de rendre ce qu'il rendait.
  d$Concentration_depart <- if ("Concentration_depart" %in% names(df))
    suppressWarnings(as.numeric(df$Concentration_depart)) else rep(NA_real_, nrow(d))

  # Une ligne entierement vide est un reste de saisie, pas une erreur : on la
  # retire en silence. Une ligne A MOITIE remplie, elle, se signale.
  garde <- nzchar(d$Produit) | nzchar(d$Matiere_active) | is.finite(d$Concentration_mere)
  d <- d[garde, , drop = FALSE]
  if (!nrow(d))
    return(vide(tr("Aucun produit saisi : ajoutez au moins une ligne (produit, matière active, concentration mère).")))

  sans_nom <- !nzchar(d$Produit)
  if (any(sans_nom))
    return(vide(trf("%d ligne(s) sans nom de produit : nommez-les, ou videz-les entièrement.",
                    sum(sans_nom))))

  mauvaise_c <- !is.finite(d$Concentration_mere) | d$Concentration_mere <= 0
  if (any(mauvaise_c))
    return(vide(trf("Concentration mère absente ou nulle pour : %s. Renseignez-la pour chaque matière active.",
                    paste(unique(d$Produit[mauvaise_c]), collapse = ", "))))

  # Un coefficient inferieur a 1 CONCENTRE la solution au lieu de la diluer.
  # C'est presque toujours une inversion de saisie (0,1 pour 10) : on le dit
  # au lieu de rendre un volume a prelever superieur au volume final.
  mauvais_k <- !is.finite(d$Coefficient) | d$Coefficient < 1
  if (any(mauvais_k))
    return(vide(trf("Coefficient de dilution absent ou inférieur à 1 pour : %s. Un coefficient de 10 signifie une dilution au 1/10 ; une valeur inférieure à 1 concentrerait la solution.",
                    paste(unique(d$Produit[mauvais_k]), collapse = ", "))))

  mauvais_v <- !is.finite(d$Volume_final) | d$Volume_final <= 0
  if (any(mauvais_v))
    return(vide(trf("Volume final absent ou nul pour : %s. C'est le volume de solution fille à préparer.",
                    paste(unique(d$Produit[mauvais_v]), collapse = ", "))))

  mauvais_n <- d$Nb_filles < 1 | d$Nb_filles != round(d$Nb_filles) |
               d$Nb_filles > HSTAT_DILUTION_NB_MAX
  if (any(mauvais_n))
    return(vide(trf("Nombre de solutions filles invalide pour : %s. Attendu un entier compris entre 1 et %d.",
                    paste(unique(d$Produit[mauvais_n]), collapse = ", "),
                    HSTAT_DILUTION_NB_MAX)))
  d$Nb_filles <- as.integer(d$Nb_filles)

  # Coefficient, volume final et nombre de filles appartiennent au produit :
  # deux valeurs differentes sous le meme nom decrivent deux preparations.
  incoherent <- character(0)
  for (p in unique(d$Produit)) {
    s <- d[d$Produit == p, , drop = FALSE]
    if (length(unique(s$Coefficient)) > 1 || length(unique(s$Volume_final)) > 1 ||
        length(unique(s$Unite_volume)) > 1 || length(unique(s$Nb_filles)) > 1)
      incoherent <- c(incoherent, p)
  }
  if (length(incoherent))
    return(vide(trf("Coefficient de dilution, nombre de filles ou volume final contradictoire pour : %s. Ces valeurs décrivent la préparation : elles sont les mêmes pour toutes les matières actives d'un produit.",
                    paste(incoherent, collapse = ", "))))

  fac <- vapply(d$Unite, function(u) .hstat_dose_facteur(u, HSTAT_CONC_UNITES),
                numeric(1), USE.NAMES = FALSE)
  if (any(!is.finite(fac)))
    return(vide(trf("Unité de concentration inconnue : %s. Choisissez parmi %s.",
                    paste(unique(d$Unite[!is.finite(fac)]), collapse = ", "),
                    paste(names(HSTAT_CONC_UNITES), collapse = ", "))))

  fac_v <- vapply(d$Unite_volume, function(u) .hstat_dose_facteur(u, HSTAT_VOL_UNITES),
                  numeric(1), USE.NAMES = FALSE)
  if (any(!is.finite(fac_v)))
    return(vide(trf("Unité de volume inconnue : %s. Choisissez parmi %s.",
                    paste(unique(d$Unite_volume[!is.finite(fac_v)]), collapse = ", "),
                    paste(names(HSTAT_VOL_UNITES), collapse = ", "))))

  d$.fac   <- fac
  d$.fac_v <- fac_v
  d <- d[order(d$Produit, d$Matiere_active), , drop = FALSE]

  # -- Le PREMIER etage, quand l'utilisateur le fixe -------------------------
  #  Donner la concentration de la premiere fille revient a fixer le
  #  coefficient du PREMIER etage : k1 = C_mere / C_depart. Les etages suivants
  #  gardent le coefficient courant.
  #
  #  UNE DILUTION EST UN GESTE UNIQUE SUR LA SOLUTION MERE : elle divise TOUTES
  #  les matieres actives par le meme nombre. Une concentration de depart est
  #  donc saisie par matiere active -- c'est une concentration de celle-ci --
  #  mais le k1 qu'elle implique appartient au PRODUIT. Deux matieres actives
  #  qui en impliqueraient deux differents decriraient deux gestes differents
  #  sur le meme flacon : on le refuse en nommant le produit.
  #
  #  La tolerance est RELATIVE et lache (0,1 %) : les concentrations sont
  #  saisies a la main et souvent arrondies (33,3 pour 5 et 3,33 pour 0,5
  #  donnent 10,009 et 10). Une vraie erreur de saisie, elle, se compte en
  #  dizaines de pour-cent.
  k1_par_produit <- function(sp) {
    dep <- sp$Concentration_depart
    ok  <- is.finite(dep) & is.finite(sp$Concentration_mere) & sp$Concentration_mere > 0
    if (!any(ok)) return(list(k1 = sp$Coefficient[1], motif = NULL))
    if (any(ok & dep <= 0))
      return(list(k1 = NA_real_, motif = "nulle"))
    r <- sp$Concentration_mere[ok] / dep[ok]
    if (any(r < 1)) return(list(k1 = NA_real_, motif = "plus_concentree"))
    if (max(r) - min(r) > 1e-3 * min(r))
      return(list(k1 = NA_real_, motif = "contradictoire"))
    list(k1 = mean(r), motif = NULL)
  }
  # Les trois refus sont ecrits comme des appels LITTERAUX a `trf()` : le
  # balayage qui verifie que chaque cle passee a `tr()`/`trf()` figure au
  # dictionnaire lit l'arbre syntaxique, et une chaine rangee dans une variable
  # lui echapperait -- le message resterait en francais dans une interface
  # anglaise, sans que rien ne le signale.
  refus <- function(motif, p) switch(motif,
    nulle = trf("Concentration de départ nulle ou négative pour : %s. Indiquez la concentration de la première fille, ou laissez la case vide pour partir de la mère divisée par le coefficient.", p),
    plus_concentree = trf("Concentration de départ supérieure à la concentration mère pour : %s. Une dilution ne concentre pas : choisissez une valeur inférieure ou égale à la mère.", p),
    contradictoire = trf("Concentrations de départ contradictoires pour : %s. Une dilution divise toutes les matières actives d'un flacon par le même nombre : les concentrations de départ doivent être dans le même rapport que les concentrations mères.", p))

  # -- La gamme : une fille par rang, pour chaque matiere active ------------
  #  C_n = C_1 / k^(n-1) = C_mere / (k1 x k^(n-1)), et le prelevement se fait
  #  TOUJOURS dans la mere : V_preleve_n x C_mere = V_final x C_n, d'ou
  #  V_preleve_n = V_final x C_n / C_mere. Sans concentration de depart,
  #  k1 = k et l'on retrouve exactement C_mere / k^n et V_final / k^n.
  morceaux <- list()
  emploie_depart <- FALSE
  for (p in unique(d$Produit)) {
    sp <- d[d$Produit == p, , drop = FALSE]
    k  <- sp$Coefficient[1]
    n  <- sp$Nb_filles[1]
    vf <- sp$Volume_final[1]

    dep <- k1_par_produit(sp)
    if (!is.null(dep$motif)) return(vide(refus(dep$motif, p)))
    k1 <- dep$k1
    if (any(is.finite(sp$Concentration_depart))) emploie_depart <- TRUE

    for (i in seq_len(n)) {
      q <- sp
      q$Rang <- i
      # Le rang 1 vient de la mere ; au-dela, de la fille precedente.
      q$Concentration_precedente <- if (i == 1L) q$Concentration_mere
                                    else q$Concentration_mere / (k1 * k^(i - 2L))
      q$Concentration_fille      <- q$Concentration_mere / (k1 * k^(i - 1L))
      q$Volume_a_prelever        <- vf / (k1 * k^(i - 1L))
      q$Volume_eau_a_ajouter     <- vf - q$Volume_a_prelever
      morceaux[[length(morceaux) + 1L]] <- q
    }
  }
  d <- do.call(rbind, morceaux)

  # La concentration ramenee a g/L rend les matieres actives ADDITIONNABLES :
  # sommer des pour-cent et des mg/L donnerait un total qui ne veut rien dire.
  cle <- paste(d$Produit, d$Rang, sep = "\u001f")
  totaux <- tapply(d$Concentration_fille * d$.fac, cle, sum, na.rm = TRUE)
  d$Concentration_totale_g_L <- unname(totaux[cle])

  # Ce que la MERE doit fournir : la somme de tous les prelevements, puisqu'ils
  # viennent tous d'elle. « Ai-je assez de solution mere ? » est la premiere
  # question qu'on se pose devant une gamme, et la seule qu'on ne puisse pas
  # rattraper une fois la paillasse installee.
  v_mere <- tapply(ifelse(!duplicated(cle), d$Volume_a_prelever, 0),
                   d$Produit, sum, na.rm = TRUE)

  d <- d[order(d$Produit, d$Rang, d$Matiere_active), , drop = FALSE]
  # Le volume a prelever, l'eau a ajouter et la concentration totale
  # appartiennent au couple (PRODUIT, RANG) : les repeter sur chaque matiere
  # active ferait croire qu'il faut prelever autant de fois. Ils ne figurent
  # donc que sur la premiere ligne -- apres le tri, sans quoi ce ne serait
  # pas la bonne.
  cle <- paste(d$Produit, d$Rang, sep = "\u001f")
  suivante <- duplicated(cle)
  for (k2 in c("Volume_a_prelever", "Volume_eau_a_ajouter", "Concentration_totale_g_L"))
    d[[k2]][suivante] <- NA_real_
  # Le besoin en solution mere est propre au PRODUIT : une seule ligne.
  d$Volume_mere_requis <- NA_real_
  d$Volume_mere_requis[!duplicated(d$Produit)] <-
    unname(v_mere[unique(d$Produit)])

  # La colonne de depart DISPARAIT quand personne ne s'en sert : une colonne
  # vide fait croire a une information absente. Meme regle que « Groupe » et
  # « Variable » dans le calcul d'efficacite.
  cols <- c("Produit", "Matiere_active", "Concentration_mere",
            if (emploie_depart) "Concentration_depart", "Unite",
            "Coefficient", "Rang", "Concentration_precedente",
            "Concentration_fille", "Volume_final", "Unite_volume",
            "Volume_a_prelever", "Volume_eau_a_ajouter",
            "Concentration_totale_g_L", "Volume_mere_requis")
  res <- d[, cols, drop = FALSE]
  rownames(res) <- NULL
  attr(res, "formules") <- unname(HSTAT_DOSE_FORMULES[
    c("conservation", "c_fille", "c_fille_n", "v_preleve", "v_eau",
      "v_mere", "c_totale")])

  # Un prelevement impipetable n'est pas un chiffre, c'est un plan a refaire :
  # on nomme les produits concernes et on dit quoi changer.
  ml <- d$Volume_a_prelever * d$.fac_v * 1000
  minuscule <- unique(d$Produit[is.finite(ml) & ml < HSTAT_DILUTION_MIN_ML])
  if (length(minuscule))
    attr(res, "avertissement") <- trf(
      "Volume à prélever inférieur à %s mL pour : %s. Un tel prélèvement ne se mesure pas à la pipette : augmentez le volume final, réduisez le coefficient de dilution ou préparez la gamme en plusieurs étapes.",
      hstat_fmt_nb(HSTAT_DILUTION_MIN_ML, 3), paste(minuscule, collapse = ", "))
  res
}

# Les noms de colonne sont des IDENTIFIANTS -- le code y accede par
# `res$Volume_a_prelever`, et un nom accentue casserait cet acces sur une
# machine hors UTF-8. Ils sont donc relabellises a l'AFFICHAGE, ici et dans
# l'export : les deux, sinon le fichier telecharge garde l'orthographe que
# l'ecran vient de corriger.
HSTAT_DILUTION_LIBELLES <- c(
  Produit                   = "Produit",
  Matiere_active            = "Matière active",
  Concentration_mere        = "Concentration mère",
  Concentration_depart      = "Concentration de la 1re fille (départ)",
  Unite                     = "Unité",
  Coefficient               = "Coefficient de dilution",
  Nb_filles                 = "Nombre de filles",
  Rang                      = "Rang de la fille",
  Concentration_precedente  = "Concentration précédente",
  Concentration_fille       = "Concentration fille",
  Volume_final              = "Volume final (Vf)",
  Unite_volume              = "Unité de volume",
  Volume_a_prelever         = "Volume à prélever dans la mère (Vi)",
  Volume_eau_a_ajouter      = "Eau à ajouter (Vf − Vi)",
  Concentration_totale_g_L  = "Concentration totale de la fille (g/L)",
  Volume_mere_requis        = "Solution mère nécessaire (total)",
  # Les colonnes de conversion. Elles ne remplacent pas leurs originales, elles
  # les accompagnent : le libellé doit donc dire « converti » sans ambiguïté,
  # sans quoi deux colonnes de volume côte à côte se confondraient.
  Unite_volume_conv         = "Unité convertie",
  Volume_final_conv         = "Volume final converti",
  Volume_a_prelever_conv    = "Volume à prélever converti",
  Volume_eau_a_ajouter_conv = "Eau à ajouter convertie",
  Volume_mere_requis_conv   = "Solution mère nécessaire convertie")

# Volumes ramenes a UNE unite commune.
#
# Chaque produit garde SON unite de saisie, ce qui est juste a la saisie et
# illisible au resultat : deux produits declares l'un en litres et l'autre en
# millilitres donnent une colonne « volume à prélever » ou 0,1 et 100 designent
# la meme chose. La conversion rend la colonne comparable -- et c'est elle qui
# donne son interet au microlitre, l'unite dans laquelle une gamme profonde
# cesse d'etre une suite de zeros.
#
# `unite` vaut NULL ou "" pour ne rien convertir : c'est une OPTION, le defaut
# reste l'unite de saisie.
HSTAT_DILUTION_VOL_COLS <- c("Volume_final", "Volume_a_prelever",
                             "Volume_eau_a_ajouter", "Volume_mere_requis")

hstat_dilution_convertir <- function(d, unite = NULL) {
  if (!is.data.frame(d) || !nrow(d)) return(d)
  if (is.null(unite) || !nzchar(unite) || !"Unite_volume" %in% names(d)) return(d)
  vers <- .hstat_dose_facteur(unite, HSTAT_VOL_UNITES)
  # Une unite inconnue ne doit pas rendre des volumes faux : on ne convertit
  # pas, plutot que de diviser par NA et de vider la colonne en silence.
  if (!isTRUE(is.finite(vers)) || vers == 0) return(d)
  depuis <- vapply(as.character(d$Unite_volume),
                   function(u) .hstat_dose_facteur(u, HSTAT_VOL_UNITES),
                   numeric(1), USE.NAMES = FALSE)
  ok <- is.finite(depuis) & depuis > 0
  if (!any(ok)) return(d)

  # LA CONVERSION AJOUTE, ELLE NE REMPLACE PAS. Une premiere version ecrasait
  # la valeur saisie : l'utilisateur qui avait tape « 100 mL » ne le retrouvait
  # plus nulle part, et ne pouvait plus verifier ce qu'il avait entre. Or c'est
  # la premiere chose qu'on relit devant une paillasse -- la valeur convertie
  # sert au geste, la valeur saisie sert au controle.
  #
  # Chaque colonne convertie est posee JUSTE APRES son originale : l'oeil
  # apparie les deux sans avoir a traverser le tableau.
  for (k in intersect(HSTAT_DILUTION_VOL_COLS, names(d))) {
    v <- rep(NA_real_, nrow(d))
    v[ok] <- d[[k]][ok] * depuis[ok] / vers
    d <- .hstat_col_apres(d, k, paste0(k, "_conv"), v)
  }
  # L'unite convertie est une colonne A PART : `Unite_volume` continue de dire
  # dans quoi l'utilisateur a saisi. Deux colonnes, deux questions.
  u <- rep(NA_character_, nrow(d))
  u[ok] <- unite
  .hstat_col_apres(d, "Unite_volume", "Unite_volume_conv", u)
}

# Insere une colonne juste apres une autre, en conservant l'ordre du reste.
.hstat_col_apres <- function(d, apres, nom, valeurs) {
  d[[nom]] <- valeurs
  nm <- setdiff(names(d), nom)
  i  <- match(apres, nm)
  if (is.na(i)) return(d)
  d[, append(nm, nom, after = i), drop = FALSE]
}

hstat_dilution_affichage <- function(d) {
  if (!is.data.frame(d) || !nrow(d)) return(d)
  nm  <- names(d)
  vus <- nm %in% names(HSTAT_DILUTION_LIBELLES)
  nm[vus] <- unname(HSTAT_DILUTION_LIBELLES[nm[vus]])
  names(d) <- nm
  d
}

# =============================================================================
#  INTERFACE
# =============================================================================
#  Deux onglets, parce que ce sont deux questions distinctes : ce qu'on apporte
#  a l'hectare, et ce qu'on prepare au laboratoire. Les melanger sur un meme
#  ecran obligerait a lire des champs qui ne servent pas au calcul en cours.

.hstat_dose_bloc_formule <- function(txt) {
  shiny::div(
    style = paste0("background:#eef7ff;border-left:4px solid #2e86c1;border-radius:6px;",
                   "padding:10px 14px;margin:12px 0;font-family:'IBM Plex Mono',",
                   "Consolas,monospace;font-size:12.5px;color:#1b4f72;"),
    lapply(txt, function(f) shiny::div(style = "margin:3px 0;", f)))
}

mod_dosage_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::div(class = "callout callout-info", style = "margin-bottom:14px;",
        shiny::icon("flask-vial"), shiny::strong(" Doses et dilutions. "),
        "Chaque résultat porte la formule qui l'a produit : un chiffre de dose",
        " qu'on ne peut pas refaire à la main sera de toute façon recalculé.")),

    shiny::tabsetPanel(
      id = ns("dosageTabs"),

      # ------------------------------------------------------------------
      shiny::tabPanel(
        shiny::tagList(shiny::icon("seedling"), " Dose et grammage à l'hectare"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("sliders"), " Le produit"),
            status = "primary", width = 4, solidHeader = TRUE,
            shiny::textInput(ns("doseProduit"), "Nom commercial du produit",
                             placeholder = "ex : Cypercal 50 EC"),
            shiny::textInput(ns("doseMatiere"), "Matière active",
                             placeholder = "ex : Cyperméthrine"),
            shiny::fluidRow(
              shiny::column(7, shiny::numericInput(ns("doseConc"),
                "Concentration en matière active", value = 400, min = 0, step = 1)),
              shiny::column(5, shiny::selectInput(ns("doseConcUnite"), "Unité",
                choices = names(HSTAT_CONC_UNITES), selected = "g/L"))),

            shiny::hr(),
            shiny::radioButtons(ns("doseSens"), "Ce que vous connaissez",
              choiceNames = list(
                shiny::HTML("<b>La dose homologuée</b> <small style='color:#7f8c8d;'>(je cherche le grammage)</small>"),
                shiny::HTML("<b>Le grammage visé</b> <small style='color:#7f8c8d;'>(je cherche la dose)</small>")),
              choiceValues = list("dose", "grammage"), selected = "dose"),

            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'dose'", ns("doseSens")),
              shiny::fluidRow(
                shiny::column(7, shiny::numericInput(ns("doseValeur"),
                  "Dose homologuée", value = 250, min = 0, step = 1)),
                shiny::column(5, shiny::selectInput(ns("doseUnite"), "Unité",
                  choices = names(HSTAT_DOSE_UNITES), selected = "mL/ha")))),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'grammage'", ns("doseSens")),
              shiny::numericInput(ns("doseGrammage"),
                "Grammage visé (g de matière active/ha)", value = 100, min = 0, step = 1))),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("droplet"), " Le chantier"),
            status = "info", width = 4, solidHeader = TRUE,
            shiny::numericInput(ns("doseBouillie"),
              "Volume de bouillie (L/ha)", value = 60, min = 0, step = 5),
            shiny::helpText("L'eau apportée à l'hectare. Elle ne change pas le grammage,",
                            " seulement la concentration de ce qu'il y a dans la cuve."),
            shiny::numericInput(ns("doseSurface"),
              "Superficie du champ (ha)", value = 1, min = 0, step = 0.1),
            shiny::numericInput(ns("doseCuve"),
              "Volume de la cuve ou du pulvérisateur (L)", value = 15, min = 0, step = 1),
            shiny::helpText("Facultatif. Renseigné, il donne la quantité de produit",
                            " par cuve et le nombre de cuves à préparer.")),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("square-root-variable"), " Formules employées"),
            status = "warning", width = 4, solidHeader = TRUE, collapsible = TRUE,
            .hstat_dose_bloc_formule(unname(HSTAT_DOSE_FORMULES[
              c("grammage", "dose", "conc_bouil", "dose_bouil",
                "total_prod", "total_ma", "total_eau", "par_cuve", "nb_cuves")])))),

        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("table"), " Résultat"),
            status = "success", width = 12, solidHeader = TRUE,
            shiny::uiOutput(ns("doseMessage")),
            DT::DTOutput(ns("doseTable")),
            shiny::br(),
            shiny::downloadButton(ns("doseDlCsv"), " Télécharger (CSV)",
                                  class = "btn-sm"),
            shiny::downloadButton(ns("doseDlXlsx"), " Télécharger (Excel)",
                                  class = "btn-sm")))),

      # ------------------------------------------------------------------
      shiny::tabPanel(
        shiny::tagList(shiny::icon("vials"), " Solutions filles (dilutions)"),
        shiny::div(style = "padding-top:14px;"),

        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("bottle-droplet"), " La solution mère"),
            status = "primary", width = 5, solidHeader = TRUE,
            shiny::textInput(ns("dilProduit"), "Nom commercial du produit",
                             placeholder = "ex : Lambdacal P 212 EC"),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("dilNbMa"),
                "Nombre de matières actives", value = 1, min = 1, max = 10, step = 1)),
              shiny::column(6, shiny::selectInput(ns("dilUnite"),
                "Unité de concentration", choices = names(HSTAT_CONC_UNITES),
                selected = "g/L"))),
            shiny::helpText("Une concentration mère par matière active : c'est ce qui",
                            " permet de détailler chaque fille et d'en donner la",
                            " concentration totale."),
            shiny::uiOutput(ns("dilMaUi")),
            shiny::helpText(shiny::strong("Concentration de départ : facultative."),
                            " Laissée vide, la première fille vaut la mère divisée par",
                            " le coefficient. Renseignée, la gamme commence à cette",
                            " valeur, puis se divise par le coefficient à chaque étage."),

            shiny::hr(),
            shiny::div(class = "callout callout-info", style = "padding:8px 12px;",
              shiny::icon("arrow-down"), shiny::strong(" Reprendre le calcul de dose. "),
              "L'onglet précédent a déjà déterminé le grammage et la concentration",
              " de la bouillie : inutile de les ressaisir."),
            shiny::fluidRow(
              shiny::column(7, shiny::selectInput(ns("dilSource"), "Valeur à reprendre",
                choices = c("Concentration du produit commercial" = "conc_produit",
                            "Concentration de la bouillie (g m.a./L)" = "conc_bouillie",
                            "Grammage à l'hectare (g m.a./ha)" = "grammage"),
                selected = "conc_bouillie")),
              shiny::column(5, shiny::div(style = "margin-top:26px;",
                shiny::actionButton(ns("dilImport"),
                  shiny::tagList(shiny::icon("file-import"), " Reprendre"),
                  class = "btn-default btn-block"))))),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("layer-group"), " La gamme à préparer"),
            status = "info", width = 3, solidHeader = TRUE,
            shiny::numericInput(ns("dilCoef"), "Coefficient de dilution",
                                value = 10, min = 1, step = 1),
            shiny::helpText("10 signifie une dilution au 1/10 : chaque fille est dix",
                            " fois moins concentrée que la précédente."),
            shiny::numericInput(ns("dilNb"), "Nombre de solutions filles",
                                value = 3, min = 1, max = HSTAT_DILUTION_NB_MAX, step = 1),
            shiny::fluidRow(
              shiny::column(7, shiny::numericInput(ns("dilVf"),
                "Volume final de chaque fille (Vf)", value = 100, min = 0, step = 10)),
              shiny::column(5, shiny::selectInput(ns("dilVfUnite"), "Unité",
                choices = names(HSTAT_VOL_UNITES), selected = "mL"))),
            shiny::hr(),
            # Deux reglages d'AFFICHAGE : ils ne changent aucun calcul, ils
            # changent ce qu'on peut en lire.
            shiny::selectInput(ns("dilConvVol"), "Convertir les volumes en",
              choices = c("(unité de saisie)" = "", names(HSTAT_VOL_UNITES)),
              selected = ""),
            shiny::helpText("Les résultats portent alors les ", shiny::strong("deux"),
                            " valeurs : celle que vous avez saisie et sa conversion,",
                            " chacune dans sa colonne et avec son unité."),
            shiny::numericInput(ns("dilDecimales"), "Chiffres après la virgule",
              value = 4, min = 0, max = HSTAT_DEC_MAX, step = 1),
            shiny::helpText("Une gamme profonde descend vite : au 1/10 depuis 100 g/L,",
                            " la 5e fille vaut 0,001. Au-delà de 15 décimales un nombre",
                            " à virgule flottante ne porte plus d'information ;",
                            " une valeur trop petite pour l'affichage choisi passe",
                            " automatiquement en notation scientifique."),
            shiny::hr(),
            shiny::actionButton(ns("dilAjouter"),
              shiny::tagList(shiny::icon("plus"), " Ajouter ce produit"),
              class = "btn-primary btn-block"),
            shiny::helpText("Ajoutez autant de produits que nécessaire : chacun garde",
                            " son propre coefficient et son propre nombre de filles.")),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("square-root-variable"), " Formules employées"),
            status = "warning", width = 4, solidHeader = TRUE, collapsible = TRUE,
            .hstat_dose_bloc_formule(unname(HSTAT_DOSE_FORMULES[
              c("conservation", "c_fille", "c_fille_n", "v_preleve", "v_eau",
                "v_mere", "c_totale")])),
            shiny::helpText("Le prélèvement se fait à chaque fois dans la solution",
                            " mère : une erreur de pipetage ne se propage donc pas",
                            " d'une fille à la suivante."),
            shiny::helpText("Le volume à prélever, l'eau à ajouter et la concentration",
                            " totale appartiennent au couple produit × fille : ils ne",
                            " figurent que sur sa première ligne."))),

        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("table-list"), " Produits saisis"),
            status = "primary", width = 12, solidHeader = TRUE,
            shiny::div(class = "callout callout-warning", style = "padding:8px 12px;",
              shiny::icon("lightbulb"),
              shiny::strong(" Une ligne par matière active. "),
              "Le tableau est modifiable case par case : un double-clic corrige",
              " une valeur sans tout ressaisir. Le coefficient, le nombre de filles",
              " et le volume final sont ceux du produit — ils sont donc identiques",
              " sur toutes ses lignes."),
            DT::DTOutput(ns("dilSaisie")),
            shiny::br(),
            shiny::actionButton(ns("dilCalculer"),
              shiny::tagList(shiny::icon("calculator"), " Calculer les solutions filles"),
              class = "btn-primary"),
            shiny::actionButton(ns("dilLigne"),
              shiny::tagList(shiny::icon("plus"), " Ligne vide"), class = "btn-sm"),
            shiny::actionButton(ns("dilVider"),
              shiny::tagList(shiny::icon("eraser"), " Vider le tableau"),
              class = "btn-sm"))),

        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("flask"), " Solutions filles"),
            status = "success", width = 12, solidHeader = TRUE,
            shiny::uiOutput(ns("dilMessage")),
            DT::DTOutput(ns("dilTable")),
            shiny::br(),
            shiny::downloadButton(ns("dilDlCsv"), " Télécharger (CSV)",
                                  class = "btn-sm"),
            shiny::downloadButton(ns("dilDlXlsx"), " Télécharger (Excel)",
                                  class = "btn-sm"))))
    ))
}

# =============================================================================
#  SERVEUR
# =============================================================================
mod_dosage_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Message d'echec, jamais un tableau vide sans explication -----------
    bandeau <- function(motif) {
      if (is.null(motif) || !nzchar(motif)) return(NULL)
      shiny::div(class = "callout callout-warning", style = "padding:10px 14px;",
                 shiny::icon("triangle-exclamation"), " ", motif)
    }

    # ---------------------------------------------------------------- dose
    dose_res <- shiny::reactive({
      sens <- input$doseSens %||% "dose"
      hstat_dose_bilan(
        sens            = sens,
        valeur          = if (identical(sens, "dose")) input$doseValeur else input$doseGrammage,
        unite_valeur    = if (identical(sens, "dose")) (input$doseUnite %||% "mL/ha") else "g/ha",
        concentration   = input$doseConc,
        unite_concentration = input$doseConcUnite %||% "g/L",
        volume_bouillie = input$doseBouillie,
        superficie      = input$doseSurface,
        volume_cuve     = input$doseCuve,
        produit         = input$doseProduit %||% "",
        matiere         = input$doseMatiere %||% "")
    })

    output$doseMessage <- shiny::renderUI(bandeau(attr(dose_res(), "message")))

    # Le tableau affiche porte les valeurs MISES EN FORME : une dose de
    # 4.166666666667 mL/L ne se verse pas. Le fichier telecharge garde la
    # meme mise en forme -- deux precisions differentes a l'ecran et dans le
    # fichier feraient douter du calcul.
    dose_affiche <- shiny::reactive({
      d <- dose_res()
      if (!nrow(d)) return(NULL)
      data.frame(Grandeur = d$Grandeur,
                 Valeur   = hstat_fmt_nb(d$Valeur, 3),
                 Formule  = d$Formule, stringsAsFactors = FALSE)
    })

    output$doseTable <- DT::renderDT({
      d <- dose_affiche()
      shiny::validate(shiny::need(!is.null(d), tr("Renseignez le produit et la dose.")))
      DT::datatable(d, rownames = FALSE,
                    colnames = c(tr("Grandeur"), tr("Valeur"), tr("Formule")),
                    options = list(dom = "t", pageLength = 20, ordering = FALSE,
                                   columnDefs = list(list(width = "46%", targets = 2))))
    })

    hstat_export_table_handlers(output, "doseDl", function() dose_affiche(),
                                "doses_a_l_hectare")

    # L'assistance observe les resultats la ou ils sont deposes ; le module
    # ne l'appelle pas, il se contente de remplir l'emplacement commun.
    shiny::observeEvent(dose_res(), {
      d <- dose_res()
      if (!nrow(d)) return()
      p <- trimws(input$doseProduit %||% "")
      hstat_ai_capture(values, "Doses & dilutions",
        trf("Dose et grammage%s", if (nzchar(p)) paste0(" -- ", p) else ""),
        tables = list("Doses" = dose_affiche()),
        meta = list(superficie = input$doseSurface,
                    bouillie   = input$doseBouillie,
                    matiere    = input$doseMatiere))
    }, ignoreInit = TRUE)

    # ------------------------------------------------------------ dilution
    # Le tableau part VIDE : les produits y sont deposes par le formulaire.
    # Une ligne d'exemple prete a etre calculee ferait sortir un resultat que
    # personne n'a demande.
    saisie <- shiny::reactiveVal(hstat_dilution_table_vide(0))

    # -- Une paire de champs par matiere active ----------------------------
    # Les valeurs deja saisies sont relues avant de reconstruire les champs :
    # sans cela, passer de 2 a 3 matieres actives effacerait les deux
    # premieres, ce qui est exactement le moment ou on ne veut pas ressaisir.
    output$dilMaUi <- shiny::renderUI({
      n <- max(1L, min(10L, as.integer(.hstat_num1(input$dilNbMa, 1))))
      lapply(seq_len(n), function(i) {
        nom <- shiny::isolate(input[[paste0("dilMaNom", i)]]) %||% ""
        cnc <- shiny::isolate(input[[paste0("dilMaConc", i)]])
        dep <- shiny::isolate(input[[paste0("dilMaDep", i)]])
        shiny::fluidRow(
          shiny::column(5, shiny::textInput(ns(paste0("dilMaNom", i)),
            if (i == 1L) "Matière active" else NULL, value = nom,
            placeholder = if (i == 1L) "ex : Lambda-cyhalothrine" else NULL)),
          shiny::column(4, shiny::numericInput(ns(paste0("dilMaConc", i)),
            if (i == 1L) "Concentration mère" else NULL,
            value = if (is.null(cnc)) NA_real_ else cnc, min = 0, step = 1)),
          # Facultatif : vide, la gamme part de la mère divisée par le
          # coefficient -- exactement ce qu'elle faisait avant ce champ.
          shiny::column(3, shiny::numericInput(ns(paste0("dilMaDep", i)),
            if (i == 1L) "1re fille (option.)" else NULL,
            value = if (is.null(dep)) NA_real_ else dep, min = 0, step = 1)))
      })
    })

    # -- Reprise du calcul de dose ----------------------------------------
    # L'utilisateur a deja saisi son produit dans l'onglet precedent : lui
    # faire retaper le nom, la matiere active et la concentration serait le
    # meilleur moyen d'introduire un ecart entre les deux onglets.
    shiny::observeEvent(input$dilImport, {
      d <- dose_res()
      if (!nrow(d)) {
        shiny::showNotification(
          tr("Le calcul de dose n'a rien produit : complétez l'onglet « Dose et grammage à l'hectare » avant de reprendre ses valeurs."),
          type = "warning", duration = 8)
        return()
      }
      src <- input$dilSource %||% "conc_bouillie"
      vb  <- .hstat_num1(input$doseBouillie, NA_real_)
      val <- switch(src,
        conc_produit  = .hstat_num1(input$doseConc, NA_real_),
        grammage      = attr(d, "grammage"),
        conc_bouillie = if (isTRUE(is.finite(vb)) && vb > 0)
                          attr(d, "grammage") / vb else NA_real_)
      unite <- if (identical(src, "conc_produit")) (input$doseConcUnite %||% "g/L") else "g/L"
      if (!isTRUE(is.finite(val))) {
        shiny::showNotification(
          tr("Valeur non calculable : renseignez le volume de bouillie dans l'onglet « Dose et grammage à l'hectare »."),
          type = "warning", duration = 8)
        return()
      }
      shiny::updateTextInput(session, "dilProduit", value = attr(d, "produit"))
      shiny::updateSelectInput(session, "dilUnite", selected = unite)
      shiny::updateNumericInput(session, "dilNbMa", value = 1)
      shiny::updateTextInput(session, "dilMaNom1", value = attr(d, "matiere"))
      shiny::updateNumericInput(session, "dilMaConc1", value = round(val, 6))
      # Le grammage est une quantite A L'HECTARE, pas une concentration : le
      # reprendre tel quel comme concentration mere est un choix de
      # l'utilisateur, il doit savoir ce qu'il vient de faire.
      shiny::showNotification(
        if (identical(src, "grammage"))
          trf("Grammage repris comme concentration mère : %s. Vérifiez l'unité — un grammage s'exprime par hectare, une concentration par litre.",
              hstat_fmt_nb(val, 4))
        else trf("Valeur reprise : %s %s.", hstat_fmt_nb(val, 4), unite),
        type = "message", duration = 8)
    })

    # -- Ajout du produit au tableau ---------------------------------------
    shiny::observeEvent(input$dilAjouter, {
      nom <- trimws(input$dilProduit %||% "")
      if (!nzchar(nom)) {
        shiny::showNotification(
          tr("Nommez le produit avant de l'ajouter : c'est le nom qui regroupe ses matières actives."),
          type = "warning", duration = 6)
        return()
      }
      n <- max(1L, min(10L, as.integer(.hstat_num1(input$dilNbMa, 1))))
      mas <- vapply(seq_len(n), function(i)
        trimws(input[[paste0("dilMaNom", i)]] %||% ""), character(1))
      cnc <- vapply(seq_len(n), function(i)
        .hstat_num1(input[[paste0("dilMaConc", i)]], NA_real_), numeric(1))
      # Une matiere active sans nom se nomme d'elle-meme : « Matière active 2 »
      # vaut mieux qu'une cellule vide qui ferait echouer le calcul.
      vides <- !nzchar(mas)
      mas[vides] <- trf("Matière active %d", seq_len(n))[vides]
      if (all(!is.finite(cnc))) {
        shiny::showNotification(
          tr("Renseignez la concentration mère d'au moins une matière active."),
          type = "warning", duration = 6)
        return()
      }
      dep <- vapply(seq_len(n), function(i)
        .hstat_num1(input[[paste0("dilMaDep", i)]], NA_real_), numeric(1))
      ajout <- data.frame(
        Produit              = rep(nom, n),
        Matiere_active       = mas,
        Concentration_mere   = cnc,
        Concentration_depart = dep,
        Unite                = rep(input$dilUnite %||% "g/L", n),
        Coefficient          = rep(.hstat_num1(input$dilCoef, NA_real_), n),
        Nb_filles            = rep(.hstat_num1(input$dilNb, 1), n),
        Volume_final         = rep(.hstat_num1(input$dilVf, NA_real_), n),
        Unite_volume         = rep(input$dilVfUnite %||% "mL", n),
        stringsAsFactors     = FALSE)
      d <- saisie()
      # Ajouter deux fois le meme produit decrirait deux preparations sous un
      # seul nom : on remplace ses lignes plutot que de les empiler.
      d <- d[trimws(as.character(d$Produit)) != nom, , drop = FALSE]
      d <- rbind(d, ajout)
      rownames(d) <- NULL
      saisie(d)
      shiny::showNotification(
        trf("%s ajouté : %d matière(s) active(s).", nom, n),
        type = "message", duration = 5)
    })

    shiny::observeEvent(input$dilLigne, {
      d <- rbind(saisie(), hstat_dilution_table_vide(1))
      rownames(d) <- NULL
      saisie(d)
    })

    shiny::observeEvent(input$dilVider, saisie(hstat_dilution_table_vide(0)))

    output$dilSaisie <- DT::renderDT({
      DT::datatable(hstat_dilution_affichage(saisie()), rownames = FALSE,
                    editable = list(target = "cell"), selection = "none",
                    options = list(dom = "t", pageLength = 100, ordering = FALSE,
                                   scrollX = TRUE))
    })

    shiny::observeEvent(input$dilSaisie_cell_edit, {
      info <- input$dilSaisie_cell_edit
      d <- saisie()
      j <- info$col + 1L                      # `rownames = FALSE` decale d'un
      if (j < 1L || j > ncol(d)) return()
      v <- info$value
      d[info$row, j] <- if (is.numeric(d[[j]]))
        suppressWarnings(as.numeric(gsub(",", ".", v, fixed = TRUE))) else as.character(v)
      saisie(d)
    })

    dilution <- shiny::eventReactive(input$dilCalculer, hstat_dilution_calcul(saisie()))

    # Deux bandeaux distincts : l'echec (rien n'est calcule) et l'avertissement
    # (le calcul tient, mais le plan de gamme ne se realise pas a la paillasse).
    output$dilMessage <- shiny::renderUI({
      shiny::tagList(bandeau(attr(dilution(), "message")),
                     bandeau(attr(dilution(), "avertissement")))
    })

    dil_affiche <- shiny::reactive({
      d <- dilution()
      if (!is.data.frame(d) || !nrow(d)) return(NULL)
      # La conversion vient AVANT la mise en forme : convertir des chaines
      # deja arrondies perdrait les décimales que l'utilisateur vient de
      # demander, et le nombre affiché ne serait plus celui du calcul.
      d <- hstat_dilution_convertir(d, input$dilConvVol %||% "")
      dec <- max(0L, min(HSTAT_DEC_MAX,
                         as.integer(.hstat_num1(input$dilDecimales, 4))))
      num <- vapply(d, is.numeric, logical(1))
      d[num] <- lapply(d[num], function(x) hstat_fmt_nb(x, dec))
      hstat_dilution_affichage(d)
    })

    output$dilTable <- DT::renderDT({
      d <- dil_affiche()
      shiny::validate(shiny::need(!is.null(d),
        tr("Ajoutez un produit, puis cliquez sur « Calculer les solutions filles ».")))
      DT::datatable(d, rownames = FALSE,
                    options = list(dom = "t", pageLength = 100, ordering = FALSE,
                                   scrollX = TRUE))
    })

    # Les formules partent AVEC les resultats : un fichier de dilutions sans
    # ses formules oblige a rouvrir l'application pour savoir ce qu'il dit.
    dil_tables <- function() {
      d <- dil_affiche()
      if (is.null(d)) return(NULL)
      list("Solutions_filles" = d,
           "Formules" = data.frame(Formule = attr(dilution(), "formules"),
                                   stringsAsFactors = FALSE))
    }
    output$dilDlCsv  <- hstat_csv_handler(dil_tables, "solutions_filles")
    output$dilDlXlsx <- hstat_classeur_handler(dil_tables, "solutions_filles")

    shiny::observeEvent(dilution(), {
      d <- dilution()
      if (!is.data.frame(d) || !nrow(d)) return()
      hstat_ai_capture(values, "Doses & dilutions",
        trf("Solutions filles (%d produit(s))", length(unique(d$Produit))),
        tables = list("Solutions_filles" = dil_affiche()),
        meta = list(produits = unique(d$Produit)))
    }, ignoreInit = TRUE)
  })
}
