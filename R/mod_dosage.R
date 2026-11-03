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
#  UN SEUL CATALOGUE, D'OU DERIVENT TOUTES LES LISTES DU MODULE.
#
#  Chaque unite porte sa CATEGORIE (ce qu'elle mesure), sa BASE (l'unite de
#  reference de sa famille) et son FACTEUR vers cette base. Une conversion
#  entre deux unites de meme base est une multiplication et une division ; un
#  tableau deux a deux aurait des centaines de cases, et une faute de frappe
#  y fausserait un chemin sans toucher les autres -- la regle deja posee pour
#  les unites de rendement.
#
#  Les bases, et ce qui fait passer de l'une a l'autre :
#
#  | Categorie     | Bases           | Pont                                   |
#  |---------------|-----------------|----------------------------------------|
#  | masse, volume | g, L            | masse volumique : g = L x rho x 1000   |
#  | dose          | kg/ha, L/ha     | masse volumique : kg/ha = L/ha x rho   |
#  | concentration | g/L, g/kg, mol/L| rho : g/L = g/kg x rho ; M : g/L = mol/L x M |
#  | surface       | ha              | --                                     |
#
#  UN PONT NE SE DEVINE PAS. Une masse et un volume ne se convertissent qu'avec
#  la masse volumique du produit ; une concentration molaire qu'avec la masse
#  molaire de la matiere active. Sans elle, la conversion rend NA ET LE DIT --
#  poser une masse volumique de 1 par defaut rendrait 400 g/kg egal a 400 g/L,
#  faux de 20 % sur une formulation de densite 1,2, et sans le moindre signe.
#
#  Le pour-cent nu reste celui des etiquettes phytosanitaires : masse/volume,
#  1 % = 1 g pour 100 mL = 10 g/L. Le pour-cent massique est une autre unite,
#  et il s'ecrit « % m/m ». De meme « ppm » est la convention des solutions
#  aqueuses (1 ppm = 1 mg/L) ; sa version massique s'ecrit « ppm m/m ».
.hstat_unites_bloc <- function(categorie, base, groupe, facteurs, libelles = NULL) {
  u <- names(facteurs)
  lib <- u
  if (!is.null(libelles)) lib[match(names(libelles), u)] <- unname(libelles)
  data.frame(unite = u, categorie = categorie, base = base, groupe = groupe,
             facteur = unname(as.numeric(facteurs)), libelle = lib,
             stringsAsFactors = FALSE)
}

# Les composantes, pour batir les unites composees (doses) sans recopier un
# seul facteur : le litre du gallon et le kilogramme de la livre sont ceux
# des tables de masse et de volume, ecrits une fois.
.HSTAT_U_MASSE <- c("ng" = 1e-9, "µg" = 1e-6, "mg" = 1e-3, "cg" = 1e-2, "g" = 1,
                    "kg" = 1e3, "q" = 1e5, "t" = 1e6,
                    "oz" = 28.349523125, "lb" = 453.59237)
.HSTAT_U_VOLUME <- c("nL" = 1e-9, "µL" = 1e-6, "mL" = 1e-3, "cL" = 1e-2, "dL" = 0.1,
                     "L" = 1, "hL" = 100, "m³" = 1000,
                     "fl oz (US)" = 0.0295735295625, "gal (US)" = 3.785411784,
                     "gal (UK)" = 4.54609)
.HSTAT_U_SURFACE <- c("m²" = 1e-4, "100 m²" = 1e-2, "a" = 1e-2, "ha" = 1,
                      "km²" = 100, "acre" = 0.40468564224)

# Une dose est une quantite de produit par surface. On ne croise pas toutes les
# quantites avec toutes les surfaces -- « t/m² » n'existe sur aucune etiquette --
# mais celles qu'on trouve reellement sur un emballage, une notice ou un
# protocole. Les unites anglo-saxonnes (fl oz, gal, oz, lb) vont a l'acre.
.hstat_unites_doses <- function(paires, base_num, table_num) {
  num <- vapply(strsplit(paires, "/", fixed = TRUE), `[`, "", 1)
  srf <- vapply(strsplit(paires, "/", fixed = TRUE), `[`, "", 2)
  alias <- c("fl oz" = "fl oz (US)", "gal" = "gal (US)")
  cle <- ifelse(num %in% names(alias), unname(alias[num]), num)
  stats::setNames(as.numeric(table_num[cle]) / base_num /
                    as.numeric(.HSTAT_U_SURFACE[srf]), paires)
}

HSTAT_UNITES <- rbind(
  .hstat_unites_bloc("masse", "g", "Masses", .HSTAT_U_MASSE,
                     c("q" = "q (quintal)", "t" = "t (tonne)")),
  .hstat_unites_bloc("volume", "L", "Volumes", .HSTAT_U_VOLUME),
  .hstat_unites_bloc("surface", "ha", "Surfaces", .HSTAT_U_SURFACE,
                     c("a" = "a (are)")),
  .hstat_unites_bloc("dose", "L/ha", "Produit liquide (volume par surface)",
    .hstat_unites_doses(c("µL/m²", "mL/m²", "mL/100 m²", "L/100 m²", "mL/a", "L/a",
                          "mL/ha", "cL/ha", "L/ha", "hL/ha", "fl oz/acre", "gal/acre"),
                        1, .HSTAT_U_VOLUME)),
  .hstat_unites_bloc("dose", "kg/ha", "Produit solide (masse par surface)",
    .hstat_unites_doses(c("mg/m²", "g/m²", "g/100 m²", "kg/100 m²", "g/a", "kg/a",
                          "mg/ha", "g/ha", "kg/ha", "q/ha", "t/ha", "oz/acre", "lb/acre"),
                        1000, .HSTAT_U_MASSE)),
  .hstat_unites_bloc("concentration", "g/L", "Masse par volume (m/v)",
    c("ng/L" = 1e-9, "µg/L" = 1e-6, "mg/L" = 1e-3, "g/L" = 1, "g/hL" = 1e-2,
      "kg/L" = 1e3, "g/m³" = 1e-3, "kg/m³" = 1, "ng/mL" = 1e-6, "µg/mL" = 1e-3,
      "mg/mL" = 1, "g/mL" = 1e3, "µg/µL" = 1, "mg/dL" = 1e-2, "%" = 10,
      "ppm" = 1e-3, "ppb" = 1e-6),
    c("%" = "% (m/v, g/100 mL)", "ppm" = "ppm (mg/L)", "ppb" = "ppb (µg/L)")),
  .hstat_unites_bloc("concentration", "g/kg", "Masse par masse (m/m)",
    c("ng/g" = 1e-6, "µg/kg" = 1e-6, "mg/kg" = 1e-3, "g/kg" = 1, "µg/g" = 1e-3,
      "mg/g" = 1, "% m/m" = 10, "ppm m/m" = 1e-3, "ppb m/m" = 1e-6)),
  .hstat_unites_bloc("concentration", "mol/L", "Molaire (quantité de matière par volume)",
    c("mol/L" = 1, "mmol/L" = 1e-3, "µmol/L" = 1e-6, "nmol/L" = 1e-9,
      "M" = 1, "mM" = 1e-3, "µM" = 1e-6, "nM" = 1e-9)))
rownames(HSTAT_UNITES) <- NULL

# Les categories que le convertisseur sait relier entre elles : une masse
# devient un volume (et l'inverse) par la masse volumique.
HSTAT_UNITES_CATEGORIES <- c(
  "Masse ou volume (quantité de produit)" = "quantite",
  "Surface"                               = "surface",
  "Dose à la surface (produit ou matière active)" = "dose",
  "Concentration"                         = "concentration")

.hstat_unites_de <- function(categorie)
  HSTAT_UNITES[HSTAT_UNITES$categorie %in% categorie, , drop = FALSE]
.hstat_unites_vecteur <- function(categorie, base = NULL) {
  u <- .hstat_unites_de(categorie)
  if (!is.null(base)) u <- u[u$base %in% base, , drop = FALSE]
  stats::setNames(u$facteur, u$unite)
}

# Les tables nommees d'avant, DERIVEES du catalogue : elles restent la forme
# que lisent les appels existants, et ne peuvent plus diverger de lui.
#  - HSTAT_DOSE_UNITES : facteur vers L/ha (liquide) ou kg/ha (solide) ;
#  - HSTAT_CONC_UNITES : facteur vers g/L, g/kg ou mol/L selon la famille ;
#  - HSTAT_VOL_UNITES  : facteur vers le litre.
HSTAT_DOSE_UNITES <- .hstat_unites_vecteur("dose")
HSTAT_CONC_UNITES <- .hstat_unites_vecteur("concentration")
HSTAT_VOL_UNITES  <- .hstat_unites_vecteur("volume")
HSTAT_MASSE_UNITES   <- .hstat_unites_vecteur("masse")
HSTAT_SURFACE_UNITES <- .hstat_unites_vecteur("surface")

# Choix d'un selecteur, groupes par famille : une liste de quarante unites a
# plat ne se parcourt pas, et c'est le groupe qui dit ce que l'unite mesure.
# `trf()`/`tr()` ne sont PAS appeles ici : le libelle d'un groupe est traduit
# par le dictionnaire du navigateur, comme toute option.
hstat_unites_choix <- function(categorie, base = NULL) {
  u <- .hstat_unites_de(categorie)
  if (!is.null(base)) u <- u[u$base %in% base, , drop = FALSE]
  gr <- unique(u$groupe)
  stats::setNames(lapply(gr, function(g) {
    s <- u[u$groupe == g, , drop = FALSE]
    stats::setNames(s$unite, s$libelle)
  }), gr)
}

# Un nombre strictement positif et fini, ou NA.
.hstat_unite_pos <- function(x) {
  v <- suppressWarnings(as.numeric(x)[1])
  if (length(v) == 1L && is.finite(v) && v > 0) v else NA_real_
}

hstat_unite_info <- function(u) {
  u <- as.character(u)[1]
  if (is.na(u) || !nzchar(u)) return(NULL)
  i <- match(u, HSTAT_UNITES$unite)
  if (is.na(i)) NULL else HSTAT_UNITES[i, , drop = FALSE]
}

# -----------------------------------------------------------------------------
#  hstat_unite_convertir() -- la seule porte de conversion du module
# -----------------------------------------------------------------------------
#  `densite` est la MASSE VOLUMIQUE du produit en kg/L (soit g/mL), `masse_molaire`
#  celle de la matiere active en g/mol. L'une et l'autre ne servent qu'a passer
#  d'une base a l'autre ; sans elles le pont est refuse, et le motif nomme la
#  grandeur qui manque (attribut « motif »).
hstat_unite_convertir <- function(valeur, de, vers, densite = NA, masse_molaire = NA) {
  v <- suppressWarnings(as.numeric(valeur))
  rate <- function(motif) structure(rep(NA_real_, length(v)), motif = motif)
  a <- hstat_unite_info(de); b <- hstat_unite_info(vers)
  if (is.null(a) || is.null(b))
    return(rate(trf("Unité inconnue : %s. Choisissez une unité de la liste.",
                    paste(c(if (is.null(a)) as.character(de)[1],
                            if (is.null(b)) as.character(vers)[1]), collapse = ", "))))
  x <- v * a$facteur
  if (identical(a$base, b$base)) return(x / b$facteur)

  rho <- .hstat_unite_pos(densite)
  M   <- .hstat_unite_pos(masse_molaire)
  manque_rho <- function()
    rate(trf("Passer de %s à %s demande la masse volumique du produit (kg/L, soit g/mL) : renseignez-la.",
             a$unite, b$unite))
  manque_M <- function()
    rate(trf("Passer de %s à %s demande la masse molaire de la matière active (g/mol) : renseignez-la.",
             a$unite, b$unite))
  pont <- paste(a$base, b$base, sep = ">")
  y <- switch(pont,
    "g>L"         = if (is.na(rho)) return(manque_rho()) else x / (rho * 1000),
    "L>g"         = if (is.na(rho)) return(manque_rho()) else x * rho * 1000,
    "L/ha>kg/ha"  = if (is.na(rho)) return(manque_rho()) else x * rho,
    "kg/ha>L/ha"  = if (is.na(rho)) return(manque_rho()) else x / rho,
    "g/kg>g/L"    = if (is.na(rho)) return(manque_rho()) else x * rho,
    "g/L>g/kg"    = if (is.na(rho)) return(manque_rho()) else x / rho,
    "mol/L>g/L"   = if (is.na(M)) return(manque_M()) else x * M,
    "g/L>mol/L"   = if (is.na(M)) return(manque_M()) else x / M,
    "mol/L>g/kg"  = if (is.na(M)) return(manque_M()) else if (is.na(rho)) return(manque_rho()) else x * M / rho,
    "g/kg>mol/L"  = if (is.na(M)) return(manque_M()) else if (is.na(rho)) return(manque_rho()) else x * rho / M,
    NULL)
  if (is.null(y))
    return(rate(trf("Conversion impossible : %s et %s ne mesurent pas la même grandeur.",
                    a$unite, b$unite)))
  y / b$facteur
}

# Toutes les conversions d'une valeur, pour le convertisseur : une ligne par
# unite de la meme categorie (masses ET volumes pour une quantite de produit),
# avec le motif quand le pont manque plutot qu'une ligne qui disparait.
hstat_unite_table <- function(valeur, de, densite = NA, masse_molaire = NA) {
  a <- hstat_unite_info(de)
  v <- .hstat_unite_pos(valeur)
  vide <- data.frame(Groupe = character(0), Unite = character(0),
                     Valeur = numeric(0), Conversion = character(0),
                     stringsAsFactors = FALSE)
  if (is.null(a)) return(structure(vide, message = tr("Unité de départ inconnue : choisissez-la dans la liste.")))
  if (is.na(v)) return(structure(vide, message = tr("Saisissez une valeur strictement positive à convertir.")))
  cats <- if (a$categorie %in% c("masse", "volume")) c("masse", "volume") else a$categorie
  u <- .hstat_unites_de(cats)
  val <- numeric(nrow(u)); note <- character(nrow(u))
  for (i in seq_len(nrow(u))) {
    r <- hstat_unite_convertir(v, a$unite, u$unite[i], densite, masse_molaire)
    val[i] <- r[1]
    f <- hstat_unite_convertir(1, a$unite, u$unite[i], densite, masse_molaire)[1]
    note[i] <- if (is.finite(r[1])) trf("1 %s = %s %s", a$unite, hstat_fmt_nb(f, 6), u$unite[i])
               else attr(r, "motif") %||% ""
  }
  data.frame(Groupe = u$groupe, Unite = u$libelle, Valeur = val, Conversion = note,
             stringsAsFactors = FALSE)
}

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

# La concentration ramenee a sa base (g/L, g/kg ou mol/L). Avec la masse
# molaire, une concentration molaire est rendue en g/L : sans elle, NA --
# des moles ne s'additionnent pas a des grammes.
hstat_conc_vers_ref <- function(valeur, unite, masse_molaire = NA) {
  v <- suppressWarnings(as.numeric(valeur)[1]) * .hstat_dose_facteur(unite, HSTAT_CONC_UNITES)
  b <- hstat_unite_info(unite)
  if (!is.null(b) && identical(b$base, "mol/L")) v <- v * .hstat_unite_pos(masse_molaire)
  v
}

hstat_vol_vers_ref <- function(valeur, unite)
  suppressWarnings(as.numeric(valeur)[1]) * .hstat_dose_facteur(unite, HSTAT_VOL_UNITES)

# Rendre une valeur de reference (L/ha ou kg/ha) dans l'unite demandee.
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
                             produit = "", matiere = "",
                             densite = NA_real_, masse_molaire = NA_real_,
                             unite_bouillie = "L/ha", unite_superficie = "ha",
                             unite_cuve = "L",
                             sortie_dose = NULL, sortie_grammage = NULL) {
  sens <- match.arg(sens)
  vide <- function(motif) {
    r <- data.frame(Grandeur = character(0), Valeur = numeric(0),
                    Formule = character(0), stringsAsFactors = FALSE)
    attr(r, "message") <- motif
    r
  }

  ci <- hstat_unite_info(unite_concentration)
  if (is.null(ci) || !identical(ci$categorie, "concentration"))
    return(vide(tr("Unité de concentration inconnue : choisissez-la dans la liste.")))
  c_val <- .hstat_unite_pos(concentration)
  if (is.na(c_val))
    return(vide(tr(paste0("Concentration en matière active absente ou nulle : renseignez-la ",
                          "(elle figure sur l'étiquette du produit) et vérifiez l'unité choisie."))))

  val <- suppressWarnings(as.numeric(valeur)[1])
  if (!isTRUE(is.finite(val)) || val <= 0)
    return(vide(tr("Valeur de départ absente ou nulle : saisissez une dose ou un grammage strictement positif.")))

  # L'ETAT DU PRODUIT -- liquide (dose en volume) ou solide (dose en masse) --
  # se lit sur l'unite de DOSE quand on la donne, sur l'unite de CONCENTRATION
  # sinon : en g/kg ou % m/m le produit est solide, sa dose se donne en masse.
  # L'ecrire en mL/ha rendait une dose de granules a mesurer a l'eprouvette.
  if (sens == "dose") {
    di <- hstat_unite_info(unite_valeur)
    if (is.null(di) || !identical(di$categorie, "dose"))
      return(vide(tr("Unité de dose inconnue : choisissez une quantité de produit par surface (mL/ha, L/ha, g/ha, kg/ha…).")))
    base_dose <- di$base
  } else {
    base_dose <- if (identical(ci$base, "g/kg")) "kg/ha" else "L/ha"
  }
  # La concentration se ramene a des grammes PAR UNITE DE PRODUIT DOSE : par
  # litre pour une dose en volume, par kilogramme pour une dose en masse. Le
  # passage de l'un a l'autre demande la masse volumique, une concentration
  # molaire la masse molaire -- et leur absence se dit, elle ne vaut pas 1.
  conc <- hstat_unite_convertir(c_val, ci$unite,
                                if (identical(base_dose, "L/ha")) "g/L" else "g/kg",
                                densite, masse_molaire)
  if (!isTRUE(is.finite(conc)))
    return(vide(attr(conc, "motif") %||%
                  tr("Concentration non convertible : vérifiez l'unité choisie.")))

  if (sens == "dose") {
    dose_ref <- val * di$facteur        # L/ha ou kg/ha
    grammage <- dose_ref * conc
    u_dose   <- di$unite
    dose_aff <- val
  } else {
    gi <- hstat_unite_info(unite_valeur)
    if (is.null(gi) || !identical(gi$categorie, "dose") || !identical(gi$base, "kg/ha"))
      return(vide(tr("Unité de grammage inconnue : choisissez une masse de matière active par surface (g/ha, kg/ha, g/m²…).")))
    grammage <- hstat_unite_convertir(val, gi$unite, "g/ha")[1]   # g m.a./ha
    dose_ref <- grammage / conc
    # On rend la dose dans une unite lisible : le litre au-dela de 1, le
    # millilitre en dessous. Une dose de 0,0004 L/ha ne se lit pas.
    u_dose <- if (identical(base_dose, "kg/ha")) { if (dose_ref >= 1) "kg/ha" else "g/ha" }
              else if (dose_ref >= 1) "L/ha" else "mL/ha"
    dose_aff <- hstat_dose_depuis_ref(dose_ref, u_dose)
  }

  # Les grandeurs du chantier se saisissent dans l'unite de chacun et se
  # ramenent ici au litre, a l'hectare et au litre par hectare.
  conv_pos <- function(x, de, vers) {
    v <- .hstat_unite_pos(x)
    if (is.na(v)) return(NA_real_)
    hstat_unite_convertir(v, de, vers)[1]
  }
  surf_saisie <- .hstat_unite_pos(superficie)
  u_surf <- if (is.null(hstat_unite_info(unite_superficie))) "ha" else as.character(unite_superficie)[1]
  surf <- conv_pos(superficie, u_surf, "ha")
  if (!isTRUE(is.finite(surf))) { surf <- 1; surf_saisie <- 1; u_surf <- "ha" }
  vb   <- conv_pos(volume_bouillie, unite_bouillie, "L/ha")
  cuve <- conv_pos(volume_cuve, unite_cuve, "L")
  a_vb   <- isTRUE(is.finite(vb)) && vb > 0
  a_cuve <- a_vb && isTRUE(is.finite(cuve)) && cuve > 0

  liq   <- identical(base_dose, "L/ha")
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

  # La superficie est ecrite dans l'unite saisie : « pour 500 m² » se relit,
  # « pour 0.05 ha » oblige a refaire la conversion de tete.
  s_aff <- paste(hstat_fmt_nb(surf_saisie), u_surf)
  res <- data.frame(
    Grandeur = c(
      trf("Dose de produit commercial (%s)", u_dose),
      "Grammage de matière active (g m.a./ha)",
      "Concentration de la bouillie (g m.a./L)",
      trf("Dose dans la bouillie (%s de produit/L)", if (liq) "mL" else "g"),
      trf("Quantité totale de produit pour %s (%s)", s_aff, u_tot),
      trf("Quantité totale de matière active pour %s (g)", s_aff),
      trf("Volume d'eau total pour %s (L)", s_aff),
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

  # LA CONVERSION AJOUTE, ELLE NE REMPLACE PAS -- la regle des dilutions et des
  # rendements. La ligne convertie se pose JUSTE APRES son originale, avec le
  # facteur employe en guise de formule ; une conversion impossible (masse
  # volumique absente) garde sa ligne, motif a l'appui, plutot que de
  # disparaitre.
  ajoute_conv <- function(res, apres, valeur, de, vers, gabarit) {
    if (is.null(vers) || !nzchar(vers[1]) || identical(vers[1], de)) return(res)
    if (is.null(hstat_unite_info(vers[1]))) return(res)
    r <- hstat_unite_convertir(valeur, de, vers[1], densite, masse_molaire)
    f <- hstat_unite_convertir(1, de, vers[1], densite, masse_molaire)
    ligne <- data.frame(
      Grandeur = trf(gabarit, vers[1]), Valeur = r[1],
      Formule = if (is.finite(r[1])) trf("Conversion : 1 %s = %s %s", de,
                                         hstat_fmt_nb(f[1], 6), vers[1])
                else attr(r, "motif") %||% "",
      stringsAsFactors = FALSE)
    i <- match(apres, res$Grandeur)
    rbind(res[seq_len(i), , drop = FALSE], ligne,
          res[setdiff(seq_len(nrow(res)), seq_len(i)), , drop = FALSE])
  }
  res <- ajoute_conv(res, res$Grandeur[2], grammage, "g/ha", sortie_grammage,
                     "Grammage converti (%s)")
  res <- ajoute_conv(res, res$Grandeur[1], dose_aff, u_dose, sortie_dose,
                     "Dose de produit commercial convertie (%s)")
  rownames(res) <- NULL

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
    Densite              = rep(NA_real_, n),
    Masse_molaire        = rep(NA_real_, n),
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

  # Masse volumique de la mere (kg/L) et masse molaire de la matiere active
  # (g/mol) : facultatives, elles ne servent qu'a ramener une concentration
  # m/m ou molaire en g/L -- pour la sommer aux autres, ou la convertir.
  for (k in c("Densite", "Masse_molaire"))
    d[[k]] <- if (k %in% names(df)) suppressWarnings(as.numeric(df[[k]]))
              else rep(NA_real_, nrow(d))

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
    return(vide(trf("Unité de concentration inconnue : %s. Choisissez une unité de la liste (g/L, mg/L, %%, ppm, mol/L…).",
                    paste(unique(d$Unite[!is.finite(fac)]), collapse = ", "))))

  fac_v <- vapply(d$Unite_volume, function(u) .hstat_dose_facteur(u, HSTAT_VOL_UNITES),
                  numeric(1), USE.NAMES = FALSE)
  if (any(!is.finite(fac_v)))
    return(vide(trf("Unité de volume inconnue : %s. Choisissez une unité de la liste (µL, mL, L…).",
                    paste(unique(d$Unite_volume[!is.finite(fac_v)]), collapse = ", "))))

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
  # Une unite m/m (g/kg, % m/m) ne devient des g/L qu'avec la masse volumique
  # de la mere, une unite molaire qu'avec la masse molaire : sans elles, le
  # total de CE produit n'est pas calcule, et on le dit. Les traiter comme des
  # g/L -- ce que faisait la version d'avant pour g/kg -- rendait un total
  # faux de l'ecart de densite, sans le moindre signe.
  g_l <- vapply(seq_len(nrow(d)), function(i)
    hstat_unite_convertir(d$Concentration_fille[i], d$Unite[i], "g/L",
                          d$Densite[i], d$Masse_molaire[i])[1], numeric(1))
  cle <- paste(d$Produit, d$Rang, sep = "\u001f")
  totaux <- tapply(g_l, cle, sum)                 # un NA garde le total a NA
  d$Concentration_totale_g_L <- unname(totaux[cle])
  sans_total <- unique(d$Produit[!is.finite(g_l)])

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
            if (any(is.finite(d$Densite))) "Densite",
            if (any(is.finite(d$Masse_molaire))) "Masse_molaire",
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
  avert <- c(
    if (length(minuscule)) trf(
      "Volume à prélever inférieur à %s mL pour : %s. Un tel prélèvement ne se mesure pas à la pipette : augmentez le volume final, réduisez le coefficient de dilution ou préparez la gamme en plusieurs étapes.",
      hstat_fmt_nb(HSTAT_DILUTION_MIN_ML, 3), paste(minuscule, collapse = ", ")),
    if (length(sans_total)) trf(
      "Concentration totale non calculée pour : %s. Additionner des matières actives demande de les ramener en g/L : renseignez la masse volumique de la mère (unités m/m) ou la masse molaire (unités molaires).",
      paste(sans_total, collapse = ", ")))
  if (length(avert)) attr(res, "avertissement") <- paste(avert, collapse = " ")
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
  Densite                   = "Masse volumique de la mère (kg/L)",
  Masse_molaire             = "Masse molaire (g/mol)",
  Unite_conc_conv           = "Unité de concentration convertie",
  Concentration_mere_conv   = "Concentration mère convertie",
  Concentration_precedente_conv = "Concentration précédente convertie",
  Concentration_fille_conv  = "Concentration fille convertie",
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

hstat_dilution_convertir <- function(d, unite = NULL, unite_conc = NULL) {
  if (!is.data.frame(d) || !nrow(d)) return(d)
  d <- .hstat_dilution_convertir_conc(d, unite_conc)
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

# Les concentrations converties, sur le meme principe que les volumes : chaque
# colonne convertie suit son originale, et l'unite convertie est une colonne a
# part. Le pont entre familles (m/v, m/m, molaire) passe par la masse volumique
# et la masse molaire de la ligne : une ligne qui ne les porte pas garde une
# case vide plutot qu'un chiffre faux.
HSTAT_DILUTION_CONC_COLS <- c("Concentration_mere", "Concentration_precedente",
                              "Concentration_fille")

.hstat_dilution_convertir_conc <- function(d, unite_conc = NULL) {
  if (is.null(unite_conc) || !nzchar(unite_conc[1]) || !"Unite" %in% names(d)) return(d)
  b <- hstat_unite_info(unite_conc)
  if (is.null(b) || !identical(b$categorie, "concentration")) return(d)
  rho <- if ("Densite" %in% names(d)) d$Densite else rep(NA_real_, nrow(d))
  M   <- if ("Masse_molaire" %in% names(d)) d$Masse_molaire else rep(NA_real_, nrow(d))
  for (k in intersect(HSTAT_DILUTION_CONC_COLS, names(d))) {
    v <- vapply(seq_len(nrow(d)), function(i)
      hstat_unite_convertir(d[[k]][i], d$Unite[i], b$unite, rho[i], M[i])[1],
      numeric(1))
    d <- .hstat_col_apres(d, k, paste0(k, "_conv"), v)
  }
  .hstat_col_apres(d, "Unite", "Unite_conc_conv", rep(b$unite, nrow(d)))
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


# Un champ du protocole et son unite. La valeur se saisit dans l'unite choisie,
# et le serveur la ramene a celle de la formule (`.mere_val()`) : la formule
# affichee reste celle du protocole, au chiffre pres.
.hstat_mere_champ <- function(ns, id, libelle, valeur, categorie, base, unite) {
  shiny::fluidRow(
    shiny::column(7, shiny::numericInput(ns(id), libelle, value = valeur, min = 0)),
    shiny::column(5, shiny::selectInput(ns(paste0(id, "Unite")), "Unité",
                                        choices = hstat_unites_choix(categorie, base),
                                        selected = unite)))
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
                choices = hstat_unites_choix("concentration"), selected = "g/L"))),
            # Les deux ponts entre familles d'unites. Facultatifs : ils ne
            # servent que si la dose et la concentration ne parlent pas de la
            # meme chose (mL/ha et g/kg), ou si la concentration est molaire.
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("doseDensite"),
                "Masse volumique du produit (kg/L)", value = NULL, min = 0, step = 0.01)),
              shiny::column(6, shiny::numericInput(ns("doseMasseMol"),
                "Masse molaire de la m.a. (g/mol)", value = NULL, min = 0, step = 0.1))),
            shiny::helpText("Facultatives. La masse volumique (= densité, en g/mL) relie une",
                            " dose en volume à une concentration en masse par masse",
                            " (g/kg, % m/m) ; la masse molaire relie une concentration",
                            " molaire (mol/L, mM) à des grammes."),

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
                  choices = hstat_unites_choix("dose"), selected = "mL/ha")))),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'grammage'", ns("doseSens")),
              shiny::fluidRow(
                shiny::column(7, shiny::numericInput(ns("doseGrammage"),
                  "Grammage visé (matière active)", value = 100, min = 0, step = 1)),
                shiny::column(5, shiny::selectInput(ns("doseGrammageUnite"), "Unité",
                  choices = hstat_unites_choix("dose", "kg/ha"), selected = "g/ha")))),
            shiny::hr(),
            # LA CONVERSION AJOUTE UNE LIGNE, ELLE NE REMPLACE PAS : la valeur
            # dans l'unite de saisie reste au tableau, pour le controle.
            shiny::selectInput(ns("doseSortieDose"), "Exprimer aussi la dose en",
              choices = c(list("(aucune conversion)" = ""), hstat_unites_choix("dose")),
              selected = ""),
            shiny::selectInput(ns("doseSortieGrammage"), "Exprimer aussi le grammage en",
              choices = c(list("(aucune conversion)" = ""), hstat_unites_choix("dose", "kg/ha")),
              selected = "")),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("droplet"), " Le chantier"),
            status = "info", width = 4, solidHeader = TRUE,
            shiny::fluidRow(
              shiny::column(7, shiny::numericInput(ns("doseBouillie"),
                "Volume de bouillie", value = 60, min = 0, step = 5)),
              shiny::column(5, shiny::selectInput(ns("doseBouillieUnite"), "Unité",
                choices = hstat_unites_choix("dose", "L/ha"), selected = "L/ha"))),
            shiny::helpText("L'eau apportée à la surface. Elle ne change pas le grammage,",
                            " seulement la concentration de ce qu'il y a dans la cuve."),
            shiny::fluidRow(
              shiny::column(7, shiny::numericInput(ns("doseSurface"),
                "Superficie du champ", value = 1, min = 0, step = 0.1)),
              shiny::column(5, shiny::selectInput(ns("doseSurfaceUnite"), "Unité",
                choices = hstat_unites_choix("surface"), selected = "ha"))),
            shiny::fluidRow(
              shiny::column(7, shiny::numericInput(ns("doseCuve"),
                "Volume de la cuve ou du pulvérisateur", value = 15, min = 0, step = 1)),
              shiny::column(5, shiny::selectInput(ns("doseCuveUnite"), "Unité",
                choices = hstat_unites_choix("volume"), selected = "L"))),
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
                "Unité de concentration", choices = hstat_unites_choix("concentration"),
                selected = "g/L"))),
            shiny::numericInput(ns("dilDensite"),
              "Masse volumique de la solution mère (kg/L, facultative)",
              value = NULL, min = 0, step = 0.01),
            shiny::helpText("Une concentration mère par matière active : c'est ce qui",
                            " permet de détailler chaque fille et d'en donner la",
                            " concentration totale."),
            shiny::uiOutput(ns("dilMaUi")),
            shiny::helpText(shiny::strong("Masse volumique et masse molaire : facultatives."),
                            " Elles ramènent en g/L une concentration en masse par masse",
                            " (g/kg, % m/m) ou molaire (mol/L, mM) : c'est ce qui permet de",
                            " sommer les matières actives et de convertir les concentrations."),
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
                choices = hstat_unites_choix("volume"), selected = "mL"))),
            shiny::hr(),
            # Deux reglages d'AFFICHAGE : ils ne changent aucun calcul, ils
            # changent ce qu'on peut en lire.
            shiny::selectInput(ns("dilConvVol"), "Convertir les volumes en",
              choices = c(list("(unité de saisie)" = ""), hstat_unites_choix("volume")),
              selected = ""),
            shiny::selectInput(ns("dilConvConc"), "Convertir les concentrations en",
              choices = c(list("(unité de saisie)" = ""), hstat_unites_choix("concentration")),
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
                shiny::column(4, .hstat_mere_champ(ns, "mereTopD", "Dose cible D", 1, "concentration", "g/kg", "µg/g")),
                shiny::column(4, .hstat_mere_champ(ns, "mereTopX", "Masse moyenne de l'insecte x", 0.05, "masse", NULL, "g")),
                shiny::column(4, shiny::numericInput(ns("mereTopP"), "Pureté P (%)", value = 95, min = 0, max = 100))),
              shiny::fluidRow(
                shiny::column(4, .hstat_mere_champ(ns, "mereTopV", "Volume final de la mère V", 50, "volume", NULL, "mL")),
                shiny::column(4, .hstat_mere_champ(ns, "mereTopA", "Volume appliqué par insecte a", 1, "volume", NULL, "µL"))),
              shiny::helpText("La masse moyenne x s'obtient en pesant 10 chenilles. Chaque valeur se saisit dans l'unité choisie à côté d'elle ; les formules et les résultats restent dans les unités du protocole.")),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'disque'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("mereProduit"), "Produit utilisé",
                                                    choices = HSTAT_MERE_PRODUITS, selected = "technique")),
                shiny::column(3, .hstat_mere_champ(ns, "mereDisC", "Concentration de la mère C", 1, "concentration", "g/L", "mg/mL")),
                shiny::column(3, .hstat_mere_champ(ns, "mereDisV", "Volume final V", 50, "volume", NULL, "mL"))),
              shiny::fluidRow(
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] != 'liquide_gl'", ns("mereProduit")),
                  shiny::column(4, shiny::numericInput(ns("mereDisP"), "Pureté ou teneur P (%)", value = 95, min = 0, max = 100))),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'liquide_gl'", ns("mereProduit")),
                  shiny::column(4, .hstat_mere_champ(ns, "mereDisTeneur", "Teneur de la formulation", 100, "concentration", "g/L", "g/L"))),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'liquide_pct'", ns("mereProduit")),
                  shiny::column(4, shiny::numericInput(ns("mereDisDensite"), "Densité de la formulation (g/mL)", value = 1, min = 0)))),
              shiny::helpText("Ne pas assimiler un % m/m à des g/L sans la densité. Le solvant est ajouté jusqu'au volume final, sans confondre volume de solvant ajouté et volume final.")),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'tube'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(4, .hstat_mere_champ(ns, "mereTubC", "Matière active pure dans la mère C", 12, "masse", NULL, "mg")),
                shiny::column(4, .hstat_mere_champ(ns, "mereTubV", "Volume d'acétone V", 10, "volume", NULL, "mL")),
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
                shiny::column(3, .hstat_mere_champ(ns, "mereVf", "Volume final de chaque fille", 10, "volume", NULL, "mL")),
                shiny::column(3, .hstat_mere_champ(ns, "mereVmin", "Plus petit volume pipetable", 0.01, "volume", NULL, "mL"))),
              shiny::checkboxInput(ns("mereMerePremier"), "La solution mère est le premier niveau testé (rang 1)", value = FALSE)),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'tube'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(4, shiny::fluidRow(
                  shiny::column(7, shiny::textInput(ns("mereTubQ"), "Quantités par tube, séparées par « ; »", value = "6 ; 30")),
                  shiny::column(5, shiny::selectInput(ns("mereTubQUnite"), "Unité",
                    choices = hstat_unites_choix("masse"), selected = "µg")))),
                shiny::column(4, .hstat_mere_champ(ns, "mereTubFiole", "Volume de la fiole", 100, "volume", NULL, "mL")),
                shiny::column(4, .hstat_mere_champ(ns, "mereTubTube", "Volume déposé par tube", 0.5, "volume", NULL, "mL")))),
            shiny::uiOutput(ns("mereSerieNote")),
            DT::DTOutput(ns("mereSerie"))))),

      # ------------------------------------------------------------------
      # LE CONVERTISSEUR : une valeur, toutes ses ecritures. Il lit le meme
      # catalogue que les trois autres onglets -- une table de conversion
      # recopiee ici finirait par contredire celle des calculs.
      shiny::tabPanel(
        shiny::tagList(shiny::icon("right-left"), " Convertisseur d'unités"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("pen-to-square"), " Valeur à convertir"),
            status = "primary", width = 4, solidHeader = TRUE,
            shiny::selectInput(ns("convCategorie"), "Grandeur",
                               choices = HSTAT_UNITES_CATEGORIES, selected = "quantite"),
            shiny::fluidRow(
              shiny::column(7, shiny::numericInput(ns("convValeur"), "Valeur",
                                                   value = 1, min = 0)),
              shiny::column(5, shiny::selectInput(ns("convUnite"), "Unité",
                choices = hstat_unites_choix(c("masse", "volume")), selected = "L"))),
            shiny::numericInput(ns("convDensite"),
              "Masse volumique du produit (kg/L, facultative)", value = NULL, min = 0, step = 0.01),
            shiny::numericInput(ns("convMasseMol"),
              "Masse molaire de la matière active (g/mol, facultative)", value = NULL,
              min = 0, step = 0.1),
            shiny::helpText("Une masse ne devient un volume — une dose en kg/ha une dose en",
                            " L/ha, une concentration m/m une concentration m/v — qu'avec la",
                            " masse volumique du produit. Une concentration molaire ne devient",
                            " des grammes qu'avec la masse molaire. Sans elles, la conversion",
                            " est refusée et dit ce qui manque : une valeur de 1 posée par",
                            " défaut rendrait un chiffre faux sans le moindre signe.")),
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("table"), " Toutes les conversions"),
            status = "success", width = 8, solidHeader = TRUE,
            shiny::uiOutput(ns("convMessage")),
            DT::DTOutput(ns("convTable")),
            shiny::br(),
            shiny::downloadButton(ns("convDlCsv"), " Télécharger (CSV)", class = "btn-sm"),
            shiny::downloadButton(ns("convDlXlsx"), " Télécharger (Excel)", class = "btn-sm"))))
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
        unite_valeur    = if (identical(sens, "dose")) (input$doseUnite %||% "mL/ha")
                          else (input$doseGrammageUnite %||% "g/ha"),
        concentration   = input$doseConc,
        unite_concentration = input$doseConcUnite %||% "g/L",
        volume_bouillie = input$doseBouillie,
        superficie      = input$doseSurface,
        volume_cuve     = input$doseCuve,
        produit         = input$doseProduit %||% "",
        matiere         = input$doseMatiere %||% "",
        densite         = input$doseDensite,
        masse_molaire   = input$doseMasseMol,
        unite_bouillie  = input$doseBouillieUnite %||% "L/ha",
        unite_superficie = input$doseSurfaceUnite %||% "ha",
        unite_cuve      = input$doseCuveUnite %||% "L",
        sortie_dose     = input$doseSortieDose %||% "",
        sortie_grammage = input$doseSortieGrammage %||% "")
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

    # ------------------------------------------------------- convertisseur
    # Les categories du selecteur, ramenees a celles du catalogue : une
    # quantite de produit couvre les masses ET les volumes.
    .conv_cats <- function(c) switch(c %||% "quantite",
      quantite = c("masse", "volume"), surface = "surface", dose = "dose",
      concentration = "concentration", c("masse", "volume"))
    shiny::observeEvent(input$convCategorie, {
      cats <- .conv_cats(input$convCategorie)
      dispo <- .hstat_unites_de(cats)$unite
      defaut <- switch(input$convCategorie %||% "quantite", quantite = "L",
                       surface = "ha", dose = "L/ha", concentration = "g/L", dispo[1])
      shiny::updateSelectInput(session, "convUnite", choices = hstat_unites_choix(cats),
        selected = hstat_garder_choix(shiny::isolate(input$convUnite), dispo, defaut))
    }, ignoreInit = TRUE)

    conv_res <- shiny::reactive({
      hstat_unite_table(input$convValeur, input$convUnite %||% "L",
                        input$convDensite, input$convMasseMol)
    })
    output$convMessage <- shiny::renderUI(bandeau(attr(conv_res(), "message")))
    conv_affiche <- shiny::reactive({
      d <- conv_res()
      if (!nrow(d)) return(NULL)
      data.frame(Groupe = d$Groupe, Unite = d$Unite,
                 Valeur = hstat_fmt_nb(d$Valeur, 6), Conversion = d$Conversion,
                 stringsAsFactors = FALSE)
    })
    output$convTable <- DT::renderDT({
      d <- conv_affiche()
      shiny::validate(shiny::need(!is.null(d), tr("Saisissez une valeur et son unité.")))
      DT::datatable(d, rownames = FALSE,
                    colnames = c(tr("Famille"), tr("Unité"), tr("Valeur"), tr("Conversion")),
                    options = list(dom = "t", pageLength = 100, ordering = FALSE))
    })
    hstat_export_table_handlers(output, "convDl", function() conv_affiche(),
                                "conversions_d_unites")

    # ------------------------------------------------------------ dilution
    # Le tableau part VIDE : les produits y sont deposes par le formulaire.
    # Une ligne d'exemple prete a etre calculee ferait sortir un resultat que
    # personne n'a demande.
    saisie <- shiny::reactiveVal(hstat_dilution_table_vide(0))

    # -- Une paire de champs par matiere active ----------------------------
    # Les valeurs deja saisies sont relues avant de reconstruire les champs :
    # sans cela, passer de 2 a 3 matieres actives effacerait les deux
    # premieres, ce qui est exactement le moment ou on ne veut pas ressaisir.
    # Une unite MOLAIRE ajoute une colonne : la masse molaire, propre a chaque
    # matiere active. Offerte pour une unite en g/L, elle serait un reglage
    # que le calcul ignore.
    dil_molaire <- shiny::reactive({
      b <- hstat_unite_info(input$dilUnite %||% "g/L")
      !is.null(b) && identical(b$base, "mol/L")
    })
    output$dilMaUi <- shiny::renderUI({
      n <- max(1L, min(10L, as.integer(.hstat_num1(input$dilNbMa, 1))))
      mol <- dil_molaire()
      lapply(seq_len(n), function(i) {
        nom <- shiny::isolate(input[[paste0("dilMaNom", i)]]) %||% ""
        cnc <- shiny::isolate(input[[paste0("dilMaConc", i)]])
        dep <- shiny::isolate(input[[paste0("dilMaDep", i)]])
        mm  <- shiny::isolate(input[[paste0("dilMaMol", i)]])
        shiny::fluidRow(
          shiny::column(if (mol) 4 else 5, shiny::textInput(ns(paste0("dilMaNom", i)),
            if (i == 1L) "Matière active" else NULL, value = nom,
            placeholder = if (i == 1L) "ex : Lambda-cyhalothrine" else NULL)),
          shiny::column(if (mol) 3 else 4, shiny::numericInput(ns(paste0("dilMaConc", i)),
            if (i == 1L) "Concentration mère" else NULL,
            value = if (is.null(cnc)) NA_real_ else cnc, min = 0, step = 1)),
          # Facultatif : vide, la gamme part de la mère divisée par le
          # coefficient -- exactement ce qu'elle faisait avant ce champ.
          shiny::column(if (mol) 2 else 3, shiny::numericInput(ns(paste0("dilMaDep", i)),
            if (i == 1L) "1re fille (option.)" else NULL,
            value = if (is.null(dep)) NA_real_ else dep, min = 0, step = 1)),
          if (mol) shiny::column(3, shiny::numericInput(ns(paste0("dilMaMol", i)),
            if (i == 1L) "Masse molaire (g/mol)" else NULL,
            value = if (is.null(mm)) NA_real_ else mm, min = 0, step = 0.1)))
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
      # Le volume de bouillie se saisit dans l'unite choisie : il est ramene
      # au litre par hectare avant de diviser le grammage.
      vb  <- hstat_unite_convertir(.hstat_num1(input$doseBouillie, NA_real_),
                                   input$doseBouillieUnite %||% "L/ha", "L/ha")[1]
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
        Densite              = rep(.hstat_num1(input$dilDensite, NA_real_), n),
        Masse_molaire        = if (dil_molaire()) vapply(seq_len(n), function(i)
                                 .hstat_num1(input[[paste0("dilMaMol", i)]], NA_real_),
                                 numeric(1)) else rep(NA_real_, n),
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
      d <- hstat_dilution_convertir(d, input$dilConvVol %||% "",
                                    input$dilConvConc %||% "")
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

    # Une valeur du protocole, ramenee de l'unite choisie a celle de la
    # formule. Une unite absente (premier rendu) vaut celle de la formule.
    .mere_val <- function(id, vers) {
      v <- .hstat_num1(input[[id]], NA_real_)
      u <- input[[paste0(id, "Unite")]] %||% vers
      hstat_unite_convertir(v, u, vers)[1]
    }

    mere_calc <- shiny::reactive({
      switch(mere_test(),
        topique = hstat_mere_topique(
          D = .mere_val("mereTopD", "µg/g"), x = .mere_val("mereTopX", "g"),
          V = .mere_val("mereTopV", "µL"),
          a = .mere_val("mereTopA", "µL"), P = input$mereTopP),
        disque = hstat_mere_concentration(
          C = .mere_val("mereDisC", "mg/mL"), V = .mere_val("mereDisV", "mL"),
          produit = input$mereProduit %||% "technique",
          P = input$mereDisP, teneur = .mere_val("mereDisTeneur", "g/L"),
          densite = input$mereDisDensite),
        tube = hstat_mere_tube(C = .mere_val("mereTubC", "mg"),
                               V = .mere_val("mereTubV", "mL"),
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
          hstat_unite_convertir(
            hstat_as_numeric_fr(strsplit(input$mereTubQ %||% "", "[;\n]+")[[1]]),
            input$mereTubQUnite %||% "µg", "µg"),
          r$c_mere, .mere_val("mereTubFiole", "mL"), .mere_val("mereTubTube", "mL")))
      hstat_mere_serie(r$c_mere, r$niveau_haut, input$mereK,
                       hstat_borne_client(input$mereN, 8, 1, 20),
                       .mere_val("mereVf", "mL"), isTRUE(input$mereMerePremier),
                       .mere_val("mereVmin", "mL"))
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
