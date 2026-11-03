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
  conservation = "Conservation de la matière : Vi × Ci = Vf × Cf (Vi prélevé dans la mère, Ci = concentration mère)",
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
    # L'ETAT DU PRODUIT SE LIT SUR L'UNITE DE SA CONCENTRATION : en g/kg ou
    # mg/kg, il est solide et sa dose se donne en masse. L'ecrire en mL/ha
    # rendait une dose de granules a mesurer a l'eprouvette, et le meme
    # produit sortait en kg/ha dans le sens « dose ».
    solide   <- as.character(unite_concentration)[1] %in% c("g/kg", "mg/kg")
    u_dose   <- if (solide) { if (dose_ref >= 1) "kg/ha" else "g/ha" }
                else if (dose_ref >= 1) "L/ha" else "mL/ha"
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

# `.hstat_col_apres()` a rejoint le socle (`Utils.R`) : le module de rendement
# l'emploie aussi, et deux copies d'une aide de cinq lignes finissent par
# diverger sur le cas limite -- ici, la colonne « apres » introuvable.

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

# =============================================================================
#  SOLUTION MERE : LES TROIS TYPES DE TEST DES PROTOCOLES DU PROGRAMME COTON
# =============================================================================
#  Le type de test decide de la GRANDEUR publiee et de la formule de la mere :
#
#  | Test                          | Grandeur      | Unite du niveau | Mere          |
#  |-------------------------------|---------------|-----------------|---------------|
#  | application topique           | dose (DL)     | µg/g            | Q en µg       |
#  | trempage de disques (IRAC 035)| concentration | mg/L            | m en mg       |
#  | tubes impregnes (vial test)   | concentration | µg/tube         | Q en mg       |
#
#  Les formules sont celles des protocoles, reprises a l'identique et ECRITES
#  UNE FOIS : l'ecran les affiche a cote de chaque resultat, comme pour les
#  doses et les dilutions de ce module. Un chiffre de pesee qu'on ne peut pas refaire a
#  la main ne sera pas pese, il sera recalcule.
#
#  Le calcul vit ici, hors du serveur, pour etre teste : une pesee fausse ne
#  leve rien, elle rend une serie entiere decalee d'un facteur constant.
HSTAT_MERE_TESTS <- list(
  topique = list(
    libelle   = "Application topique (dose par masse corporelle) — DL",
    grandeur  = "dose", unite = "µg/g", methode = "Application topique",
    protocole = "Test d'application topique — chenilles carpophages, matière active de grade technique",
    solvant   = "Acétone", repetitions = 4L, individus = 10L, niveaux = 8L,
    mere_premier = FALSE, seuil_rejet = 10, lectures = "24 h, 48 h et 72 h"),
  disque = list(
    libelle   = "Trempage de disques foliaires, IRAC 035 (concentration) — CL",
    grandeur  = "concentration", unite = "mg/L",
    methode   = "Trempage de disques foliaires (IRAC 035)",
    protocole = "Protocole IRAC 035, CNRA Bouaké — jassides (nymphes N3 et adultes)",
    solvant   = "Selon la formulation", repetitions = 4L, individus = 10L,
    niveaux = 6L, mere_premier = TRUE, seuil_rejet = 10,
    lectures  = "24 h, 48 h, 72 h, 96 h et 120 h"),
  tube = list(
    libelle   = "Tubes imprégnés, vial test au champ (quantité par tube) — CL",
    grandeur  = "concentration", unite = "µg/tube",
    methode   = "Tubes imprégnés (vial test)",
    protocole = "Surveillance de la résistance au champ de H. armigera aux pyréthrinoïdes",
    solvant   = "Acétone", repetitions = 1L, individus = 30L,
    niveaux = 2L, mere_premier = FALSE, seuil_rejet = 10,
    lectures  = "24 h (et si possible 48 h)"))

# Les choix du selecteur, derives du catalogue : une liste recopiee finirait
# par diverger.
HSTAT_MERE_TESTS_CHOIX <- stats::setNames(
  names(HSTAT_MERE_TESTS), vapply(HSTAT_MERE_TESTS, `[[`, "", "libelle"))

# Formulations : ce que porte l'etiquette decide de la conversion.
HSTAT_MERE_PRODUITS <- c(
  "Matière active technique (pureté en %)"              = "technique",
  "Formulation liquide en g/L de matière active"         = "liquide_gl",
  "Formulation solide en % massique (m/m)"               = "solide_pct",
  "Formulation liquide en % massique (m/m) + densité"    = "liquide_pct")

HSTAT_MERE_FORMULES <- c(
  topique     = "Q (µg) = D (µg/g) × x (g) × V (µL) × 100 ÷ [a (µL) × P (%)]",
  topique_c   = "Concentration de la mère (µg m.a./µL) = D (µg/g) × x (g) ÷ a (µL)",
  technique   = "m (mg) = C (mg/mL) × V (mL) ÷ (P / 100)",
  controle    = "C réelle (mg/mL) = m (mg) × (P / 100) ÷ V final (mL)",
  liquide_gl  = "Volume de formulation (mL) = C (mg/mL) × V (mL) ÷ Teneur (g/L), car 1 g/L = 1 mg/mL",
  solide_pct  = "m formulation (mg) = m matière active cible (mg) ÷ (P / 100)",
  liquide_pct = "Volume de formulation (mL) = m formulation (mg) ÷ [densité (g/mL) × 1000]",
  tube        = "Q (mg) = C (mg) × 100 ÷ P (%) ; concentration de la mère (mg/mL) = C (mg) ÷ V (mL)",
  tube_q      = "Quantité par tube (µg) = C mère (mg/mL) × Vi (mL) ÷ V fiole (mL) × V tube (mL) × 1000",
  tube_vi     = "Vi (mL) = Quantité par tube (µg) × V fiole (mL) ÷ [C mère (mg/mL) × V tube (mL) × 1000]",
  dilution    = "Ci × Vi = Cf × Vf, d'où Vi = Cf × Vf ÷ Ci",
  serie       = "Cf (rang i) = C mère ÷ k^(i − 1)",
  serie_filles = "Cf (rang i) = C mère ÷ k^i",
  abbott      = "Mortalité corrigée (%) = 100 × (mortalité traitement − mortalité témoin) ÷ (100 − mortalité témoin)")

# Un nombre strictement positif et fini, ou NA. Une pesee ne se fait pas sur
# une valeur nulle, negative ou manquante.
.hstat_mere_pos <- function(x) {
  v <- suppressWarnings(as.numeric(x)[1])
  if (length(v) == 1L && is.finite(v) && v > 0) v else NA_real_
}

# La purete est un pourcentage de ]0 ; 100] : au-dela, c'est une faute de
# frappe, et la masse pesee serait plus PETITE que la masse active visee.
.hstat_mere_purete <- function(P) {
  v <- .hstat_mere_pos(P)
  if (is.finite(v) && v <= 100) v else NA_real_
}

.hstat_mere_refus <- function(motif) list(ok = FALSE, motif = motif)

# -- Application topique --------------------------------------------------------
# D : dose cible (µg/g) ; x : masse moyenne de l'insecte (g, pesee de 10
# chenilles) ; V : volume final de la mere (µL) ; a : volume applique par
# insecte (µL) ; P : purete (%).
hstat_mere_topique <- function(D, x, V, a = 1, P) {
  D <- .hstat_mere_pos(D); x <- .hstat_mere_pos(x)
  V <- .hstat_mere_pos(V); a <- .hstat_mere_pos(a)
  P <- .hstat_mere_purete(P)
  if (anyNA(c(D, x, V, a)))
    return(.hstat_mere_refus(tr("La dose, la masse de l'insecte, le volume de la mère et le volume appliqué doivent être des nombres strictement positifs.")))
  if (is.na(P))
    return(.hstat_mere_refus(tr("La pureté doit être comprise entre 0 (exclu) et 100 %.")))
  Q <- D * x * V * 100 / (a * P)
  list(ok = TRUE, Q_ug = Q, Q_mg = Q / 1000,
       c_mere = D * x / a,            # µg de matiere active par µL
       dose_individu = D * x,         # µg deposes sur chaque insecte
       niveau_haut = D)
}

# -- Concentration (trempage de disques, IRAC 035) ------------------------------
# C : concentration visee de la mere (mg/mL) ; V : volume final (mL).
# `P` est la purete (technique) ou la teneur massique (formulations en % m/m) ;
# `teneur` la teneur en g/L d'une formulation liquide ; `densite` en g/mL.
hstat_mere_concentration <- function(C, V, produit = "technique", P = NA,
                                          teneur = NA, densite = NA) {
  produit <- if (length(produit) && produit[1] %in% HSTAT_MERE_PRODUITS)
    produit[1] else "technique"
  C <- .hstat_mere_pos(C); V <- .hstat_mere_pos(V)
  if (anyNA(c(C, V)))
    return(.hstat_mere_refus(tr("La concentration visée et le volume final doivent être des nombres strictement positifs.")))
  m_ma <- C * V                       # mg de matiere active pure
  res <- list(ok = TRUE, produit = produit, m_ma = m_ma, c_mere = C,
              niveau_haut = C * 1000) # mg/L
  if (identical(produit, "liquide_gl")) {
    t <- .hstat_mere_pos(teneur)
    if (is.na(t))
      return(.hstat_mere_refus(tr("La teneur de la formulation (g/L) doit être un nombre strictement positif.")))
    res$v_formulation <- m_ma / t     # mL, car 1 g/L = 1 mg/mL
    return(res)
  }
  P <- .hstat_mere_purete(P)
  if (is.na(P))
    return(.hstat_mere_refus(tr("La pureté doit être comprise entre 0 (exclu) et 100 %.")))
  res$m_pese <- m_ma / (P / 100)      # mg de produit a peser
  res$c_reelle <- res$m_pese * (P / 100) / V
  if (identical(produit, "liquide_pct")) {
    d <- .hstat_mere_pos(densite)
    if (is.na(d))
      return(.hstat_mere_refus(tr("Une formulation liquide en % massique demande sa densité (g/mL) pour passer de la masse au volume.")))
    res$v_formulation <- res$m_pese / (d * 1000)
  }
  res
}

# -- Tubes impregnes (vial test) ------------------------------------------------
# C : masse de matiere active pure dans la mere (mg) ; V : volume d'acetone
# (mL) ; P : purete (%). Exemple du protocole : 12 mg, 90 % -> 13,3333 mg dans
# 10 mL, soit 1,2 mg/mL.
hstat_mere_tube <- function(C, V, P) {
  C <- .hstat_mere_pos(C); V <- .hstat_mere_pos(V)
  P <- .hstat_mere_purete(P)
  if (anyNA(c(C, V)))
    return(.hstat_mere_refus(tr("La quantité de matière active et le volume d'acétone doivent être des nombres strictement positifs.")))
  if (is.na(P))
    return(.hstat_mere_refus(tr("La pureté doit être comprise entre 0 (exclu) et 100 %.")))
  list(ok = TRUE, Q_mg = C * 100 / P, c_mere = C / V)
}

# Quantite deposee dans un tube, et son inverse : le volume a prelever dans
# la mere pour la fiole. 1 mL de mere a 1,2 mg/mL dans 100 mL, 0,5 mL par
# tube -> 6 µg ; 5 mL -> 30 µg (tubes rouges et noirs du protocole).
hstat_mere_tube_quantite <- function(c_mere, v_preleve, v_fiole, v_tube)
  c_mere * v_preleve / v_fiole * v_tube * 1000

hstat_mere_tube_preleve <- function(q, c_mere, v_fiole, v_tube) {
  q <- suppressWarnings(as.numeric(q))
  ok <- is.finite(q) & q > 0
  c_mere <- .hstat_mere_pos(c_mere); v_fiole <- .hstat_mere_pos(v_fiole)
  v_tube <- .hstat_mere_pos(v_tube)
  if (!any(ok) || anyNA(c(c_mere, v_fiole, v_tube)))
    return(data.frame(Quantite = numeric(0), Volume_mere = numeric(0),
                      Solvant = numeric(0), Possible = logical(0)))
  q <- q[ok]
  vi <- q * v_fiole / (c_mere * v_tube * 1000)
  # Un prelevement plus grand que la fiole n'est pas une dilution : la mere
  # est trop faible pour cette quantite, et on le dit au lieu d'un solvant
  # negatif.
  data.frame(Quantite = q, Volume_mere = vi, Solvant = v_fiole - vi,
             Possible = vi <= v_fiole, stringsAsFactors = FALSE)
}

# -- La serie des solutions filles ----------------------------------------------
# Rang i : Cf = C mere / k^(i-1) quand la mere est le premier niveau teste
# (formule du protocole IRAC 035) ; Cf = C mere / k^i quand les filles seules
# sont testees (« huit doses de solutions filles », application topique).
# `niveau_haut` porte la grandeur publiee (µg/g, mg/L) : elle suit le meme
# facteur que la concentration, une dilution ne change que le rapport.
# Vi se lit TOUJOURS sur la mere -- la regle de la gamme de dilutions
# de ce module.
hstat_mere_serie <- function(c_mere, niveau_haut, k, n, v_final,
                             mere_premier = TRUE, v_min = NA) {
  c_mere <- .hstat_mere_pos(c_mere); niveau_haut <- .hstat_mere_pos(niveau_haut)
  k <- suppressWarnings(as.numeric(k)[1]); v_final <- .hstat_mere_pos(v_final)
  n <- suppressWarnings(as.integer(n)[1])
  vide <- data.frame(Rang = integer(0), Facteur = numeric(0), Niveau = numeric(0),
                     Concentration = numeric(0), Volume_mere = numeric(0),
                     Solvant = numeric(0), Pipetable = logical(0))
  if (anyNA(c(c_mere, niveau_haut, v_final)) || !is.finite(k) || k <= 1 ||
      is.na(n) || n < 1L)
    return(structure(vide, motif = tr("La série demande une concentration mère, un volume final positifs, un coefficient de dilution supérieur à 1 et au moins un niveau.")))
  n <- min(n, 20L)
  i <- seq_len(n)
  expo <- if (isTRUE(mere_premier)) i - 1L else i
  f <- k^expo
  vi <- v_final / f
  vm <- suppressWarnings(as.numeric(v_min)[1])
  d <- data.frame(Rang = i, Facteur = f, Niveau = niveau_haut / f,
                  Concentration = c_mere / f, Volume_mere = vi,
                  Solvant = v_final - vi,
                  Pipetable = if (is.finite(vm) && vm > 0) vi >= vm else TRUE)
  # Un prelevement sous la plage de la micropipette est imprecis : le
  # protocole IRAC 035 demande alors une dilution intermediaire validee.
  if (!all(d$Pipetable))
    attr(d, "avertissement") <- trf("%d niveau(x) demande(nt) un prélèvement inférieur à %s : préparez une dilution intermédiaire validée plutôt qu'un prélèvement imprécis.",
                                    sum(!d$Pipetable), format(vm))
  d
}


mod_dosage_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::div(class = "callout callout-info", style = "margin-bottom:14px;",
        shiny::icon("flask-vial"), shiny::strong(" Doses et dilutions. "),
        # Une seule chaine, sinon le bandeau reste en francais : voir mod_dl50.R.
        paste("Chaque résultat porte la formule qui l'a produit : un chiffre de",
              "dose qu'on ne peut pas refaire à la main sera de toute façon",
              "recalculé."))),

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
                                  class = "btn-sm")))),

      # ------------------------------------------------------------------
      # LA SOLUTION MERE SE PREPARE AVANT L'ESSAI, et le type de test decide
      # de sa formule comme de la grandeur publiee (DL ou CL). Les formules
      # sont celles des protocoles, affichees a cote de chaque chiffre.
      shiny::tabPanel(
        shiny::tagList(shiny::icon("flask"), " Solution mère"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("vial"), " Type de test"),
            status = "primary", width = 4, solidHeader = TRUE,
            shiny::selectInput(ns("mereTest"), "Type de test",
                               choices = HSTAT_MERE_TESTS_CHOIX, selected = "topique"),
            shiny::uiOutput(ns("mereTestFiche"))),
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("scale-balanced"), " Calcul de la solution mère"),
            status = "info", width = 8, solidHeader = TRUE,
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'topique'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(4, shiny::numericInput(ns("mereTopD"), "Dose cible D (µg/g)", value = 1, min = 0)),
                shiny::column(4, shiny::numericInput(ns("mereTopX"), "Masse moyenne de l'insecte x (g)", value = 0.05, min = 0)),
                shiny::column(4, shiny::numericInput(ns("mereTopP"), "Pureté P (%)", value = 95, min = 0, max = 100))),
              shiny::fluidRow(
                shiny::column(4, shiny::numericInput(ns("mereTopV"), "Volume final de la mère V (mL)", value = 50, min = 0)),
                shiny::column(4, shiny::numericInput(ns("mereTopA"), "Volume appliqué par insecte a (µL)", value = 1, min = 0))),
              shiny::helpText("La masse moyenne x s'obtient en pesant 10 chenilles. Le volume V est saisi en mL et converti en µL dans la formule.")),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'disque'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("mereProduit"), "Produit utilisé",
                                                    choices = HSTAT_MERE_PRODUITS, selected = "technique")),
                shiny::column(3, shiny::numericInput(ns("mereDisC"), "Concentration de la mère C (mg/mL)", value = 1, min = 0)),
                shiny::column(3, shiny::numericInput(ns("mereDisV"), "Volume final V (mL)", value = 50, min = 0))),
              shiny::fluidRow(
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] != 'liquide_gl'", ns("mereProduit")),
                  shiny::column(4, shiny::numericInput(ns("mereDisP"), "Pureté ou teneur P (%)", value = 95, min = 0, max = 100))),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'liquide_gl'", ns("mereProduit")),
                  shiny::column(4, shiny::numericInput(ns("mereDisTeneur"), "Teneur de la formulation (g/L)", value = 100, min = 0))),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'liquide_pct'", ns("mereProduit")),
                  shiny::column(4, shiny::numericInput(ns("mereDisDensite"), "Densité de la formulation (g/mL)", value = 1, min = 0)))),
              shiny::helpText("Ne pas assimiler un % m/m à des g/L sans la densité. Le solvant est ajouté jusqu'au volume final, sans confondre volume de solvant ajouté et volume final.")),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'tube'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(4, shiny::numericInput(ns("mereTubC"), "Matière active pure dans la mère C (mg)", value = 12, min = 0)),
                shiny::column(4, shiny::numericInput(ns("mereTubV"), "Volume d'acétone V (mL)", value = 10, min = 0)),
                shiny::column(4, shiny::numericInput(ns("mereTubP"), "Pureté P (%)", value = 90, min = 0, max = 100)))),
            shiny::uiOutput(ns("mereResultat")))),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("layer-group"), " Solutions filles"),
            status = "success", width = 12, solidHeader = TRUE,
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] != 'tube'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(3, shiny::numericInput(ns("mereK"), "Coefficient de dilution k", value = 2, min = 1)),
                shiny::column(3, shiny::numericInput(ns("mereN"), "Nombre de niveaux", value = 8, min = 1, max = 20)),
                shiny::column(3, shiny::numericInput(ns("mereVf"), "Volume final de chaque fille (mL)", value = 10, min = 0)),
                shiny::column(3, shiny::numericInput(ns("mereVmin"), "Plus petit volume pipetable (mL)", value = 0.01, min = 0))),
              shiny::checkboxInput(ns("mereMerePremier"), "La solution mère est le premier niveau testé (rang 1)", value = FALSE)),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'tube'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(4, shiny::textInput(ns("mereTubQ"), "Quantités par tube (µg), séparées par « ; »", value = "6 ; 30")),
                shiny::column(4, shiny::numericInput(ns("mereTubFiole"), "Volume de la fiole (mL)", value = 100, min = 0)),
                shiny::column(4, shiny::numericInput(ns("mereTubTube"), "Volume déposé par tube (mL)", value = 0.5, min = 0)))),
            shiny::uiOutput(ns("mereSerieNote")),
            DT::DTOutput(ns("mereSerie")))))
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
      if (nrow(d) + nrow(ajout) > HSTAT_SAISIE_MAX) {
        shiny::showNotification(
          trf("Le tableau de saisie compte déjà %d lignes, le maximum.", HSTAT_SAISIE_MAX),
          type = "warning", duration = 6)
        return()
      }
      d <- rbind(d, ajout)
      rownames(d) <- NULL
      saisie(d)
      shiny::showNotification(
        trf("%s ajouté : %d matière(s) active(s).", nom, n),
        type = "message", duration = 5)
    })

    shiny::observeEvent(input$dilLigne, {
      if (nrow(saisie()) >= HSTAT_SAISIE_MAX) {
        shiny::showNotification(
          trf("Le tableau de saisie compte déjà %d lignes, le maximum.", HSTAT_SAISIE_MAX),
          type = "warning", duration = 6)
        return()
      }
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
      # Le message vient du navigateur : une seule cellule EXISTANTE, ou rien.
      # `row = -1` ecrasait sinon toutes les lignes sauf la premiere, et
      # `row = 2e6` faisait grossir le tableau d'autant (hstat_indice_client).
      i <- hstat_indice_client(info$row, nrow(d))
      j <- hstat_indice_client(info$col + 1L, ncol(d))   # `rownames = FALSE` decale d'un
      if (is.na(i) || is.na(j) || length(info$value) != 1L) return()
      v <- info$value
      d[i, j] <- if (is.numeric(d[[j]]))
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
    }, ignoreInit = TRUE)

    # -- La solution mere ------------------------------------------------------
    # Le type de test choisi propose ses valeurs du protocole (nombre de
    # niveaux, mere comprise ou non) ; elles restent modifiables. Un nom
    # inconnu retombe sur l'application topique plutot que de lever.
    mere_test <- shiny::reactive({
      t <- input$mereTest %||% "topique"
      if (length(t) && t[1] %in% names(HSTAT_MERE_TESTS)) t[1] else "topique"
    })
    shiny::observeEvent(mere_test(), {
      tt <- HSTAT_MERE_TESTS[[mere_test()]]
      shiny::updateNumericInput(session, "mereN", value = tt$niveaux)
      shiny::updateCheckboxInput(session, "mereMerePremier", value = isTRUE(tt$mere_premier))
    }, ignoreInit = TRUE)

    output$mereTestFiche <- shiny::renderUI({
      tt <- HSTAT_MERE_TESTS[[mere_test()]]
      nom <- if (identical(tt$grandeur, "dose")) tr("Nom publié : DL10, DL50, DL90 (dose).")
             else tr("Nom publié : CL10, CL50, CL90 (concentration).")
      shiny::div(style = "font-size:0.92em;",
        shiny::p(shiny::strong(tr(tt$protocole))),
        shiny::tags$ul(
          shiny::tags$li(nom),
          shiny::tags$li(trf("Unité des niveaux : %s", tt$unite)),
          shiny::tags$li(trf("Solvant : %s", tr(tt$solvant))),
          shiny::tags$li(trf("%d répétition(s) de %d individus par niveau", tt$repetitions, tt$individus)),
          shiny::tags$li(trf("Lectures : %s", tr(tt$lectures))),
          shiny::tags$li(trf("Essai invalide si la mortalité du témoin dépasse %s %%", format(tt$seuil_rejet)))))
    })

    mere_calc <- shiny::reactive({
      switch(mere_test(),
        topique = hstat_mere_topique(
          D = input$mereTopD, x = input$mereTopX,
          V = .hstat_num1(input$mereTopV, NA) * 1000,   # mL -> µL
          a = input$mereTopA, P = input$mereTopP),
        disque = hstat_mere_concentration(
          C = input$mereDisC, V = input$mereDisV,
          produit = input$mereProduit %||% "technique",
          P = input$mereDisP, teneur = input$mereDisTeneur,
          densite = input$mereDisDensite),
        tube = hstat_mere_tube(C = input$mereTubC, V = input$mereTubV,
                                    P = input$mereTubP))
    })

    .mere_ligne <- function(libelle, valeur, formule) shiny::tags$tr(
      shiny::tags$td(libelle), shiny::tags$td(shiny::strong(valeur)),
      shiny::tags$td(shiny::tags$code(tr(formule))))

    output$mereResultat <- shiny::renderUI({
      r <- mere_calc()
      if (!isTRUE(r$ok))
        return(shiny::div(class = "alert alert-warning", r$motif))
      FM <- HSTAT_MERE_FORMULES
      lignes <- switch(mere_test(),
        topique = list(
          .mere_ligne(tr("Matière active technique à peser"),
                      trf("%s µg (%s mg)", hstat_fmt_nb(r$Q_ug, 2), hstat_fmt_nb(r$Q_mg, 4)), FM[["topique"]]),
          .mere_ligne(tr("Concentration de la mère"),
                      trf("%s µg m.a./µL", hstat_fmt_nb(r$c_mere, 6)), FM[["topique_c"]]),
          .mere_ligne(tr("Quantité déposée sur chaque insecte"),
                      paste(hstat_fmt_nb(r$dose_individu, 6), "µg"), "D × x")),
        disque = c(
          list(.mere_ligne(tr("Matière active pure dans la mère"),
                           paste(hstat_fmt_nb(r$m_ma, 4), "mg"), "C × V")),
          if (!is.null(r$m_pese)) list(
            .mere_ligne(tr("Produit à peser"), paste(hstat_fmt_nb(r$m_pese, 4), "mg"),
                        if (identical(r$produit, "technique")) FM[["technique"]] else FM[["solide_pct"]]),
            .mere_ligne(tr("Concentration réelle (contrôle)"),
                        paste(hstat_fmt_nb(r$c_reelle, 6), "mg/mL"), FM[["controle"]])),
          if (!is.null(r$v_formulation)) list(
            .mere_ligne(tr("Volume de formulation à prélever"),
                        paste(hstat_fmt_nb(r$v_formulation, 4), "mL"),
                        if (identical(r$produit, "liquide_gl")) FM[["liquide_gl"]] else FM[["liquide_pct"]])),
          list(.mere_ligne(tr("Concentration de la mère"),
                           trf("%s mg/mL, soit %s mg/L", hstat_fmt_nb(r$c_mere, 6),
                               hstat_fmt_nb(r$niveau_haut, 4)), "1 mg/mL = 1 000 mg/L"))),
        tube = list(
          .mere_ligne(tr("Matière active technique à peser"),
                      paste(hstat_fmt_nb(r$Q_mg, 4), "mg"), FM[["tube"]]),
          .mere_ligne(tr("Concentration de la mère"),
                      paste(hstat_fmt_nb(r$c_mere, 6), "mg/mL"), "C ÷ V")))
      shiny::tags$table(class = "table table-condensed",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th(tr("Grandeur")),
          shiny::tags$th(tr("Valeur")), shiny::tags$th(tr("Formule")))),
        do.call(shiny::tags$tbody, lignes))
    })

    mere_serie <- shiny::reactive({
      r <- mere_calc()
      if (!isTRUE(r$ok)) return(NULL)
      if (identical(mere_test(), "tube"))
        return(hstat_mere_tube_preleve(
          hstat_as_numeric_fr(strsplit(input$mereTubQ %||% "", "[;\n]+")[[1]]),
          r$c_mere, input$mereTubFiole, input$mereTubTube))
      hstat_mere_serie(r$c_mere, r$niveau_haut, input$mereK,
                       hstat_borne_client(input$mereN, 8, 1, 20),
                       input$mereVf, isTRUE(input$mereMerePremier), input$mereVmin)
    })

    output$mereSerieNote <- shiny::renderUI({
      d <- mere_serie()
      if (is.null(d)) return(NULL)
      txt <- if (identical(mere_test(), "tube")) {
        c(HSTAT_MERE_FORMULES[["tube_vi"]],
          if (nrow(d) && !all(d$Possible))
            tr("Certaines quantités demandent un prélèvement plus grand que la fiole : la mère est trop faible pour elles."),
          if (!nrow(d)) tr("Saisissez au moins une quantité par tube positive."))
      } else c(HSTAT_MERE_FORMULES[[if (isTRUE(input$mereMerePremier)) "serie" else "serie_filles"]],
               HSTAT_MERE_FORMULES[["dilution"]],
               attr(d, "motif"), attr(d, "avertissement"))
      shiny::div(style = "font-size:0.92em;margin-bottom:6px;",
                 lapply(txt, function(x) shiny::div(shiny::tags$code(tr(x)))))
    })

    output$mereSerie <- DT::renderDT({
      d <- mere_serie()
      shiny::req(!is.null(d), nrow(d) > 0)
      u <- HSTAT_MERE_TESTS[[mere_test()]]$unite
      if (identical(mere_test(), "tube")) {
        aff <- data.frame(d$Quantite, round(d$Volume_mere, 4), round(d$Solvant, 4),
                          ifelse(d$Possible, "\u2713", "\u2717"))
        names(aff) <- c(trf("Quantité par tube (%s)", u), tr("Mère à prélever (mL)"),
                        tr("Acétone pour compléter la fiole (mL)"), tr("Réalisable"))
      } else {
        aff <- data.frame(d$Rang, signif(d$Facteur, 6), signif(d$Niveau, 6),
                          signif(d$Concentration, 6), round(d$Volume_mere, 4),
                          round(d$Solvant, 4),
                          ifelse(d$Pipetable, "\u2713", "\u2717"))
        names(aff) <- c(tr("Rang"), tr("Facteur k^j"), trf("Niveau (%s)", u),
                        if (identical(mere_test(), "topique")) tr("Concentration (µg/µL)")
                        else tr("Concentration (mg/mL)"),
                        tr("Mère à prélever (mL)"), tr("Solvant à ajouter (mL)"),
                        tr("Pipetable"))
      }
      DT::datatable(aff, rownames = FALSE, extensions = "Buttons",
                    options = list(dom = "Bt", pageLength = 50, ordering = FALSE,
                                   buttons = .hstat_dt_buttons("solution_mere")))
    })

  })
}
