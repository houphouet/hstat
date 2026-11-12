# DOSES ET DILUTIONS -- mod_dosage.R
# Trois calculs que tout essai phytosanitaire refait a la main, sur un coin de
# table, a chaque campagne :
# 1. la DOSE de produit commercial a apporter, connaissant le grammage de
#    matiere active vise a l'hectare ;
# 2. le GRAMMAGE reellement apporte, connaissant la dose homologuee.
# 3. les solutions FILLES obtenues d'une solution mere par un coefficient de
#    dilution, avec le volume a prelever et l'eau a ajouter.

# Unites
#  UN SEUL CATALOGUE, D'OU DERIVENT TOUTES LES LISTES DU MODULE.
.hstat_unites_bloc <- function(categorie, base, groupe, facteurs, libelles = NULL) {
  u <- names(facteurs)
  lib <- u
  if (!is.null(libelles)) lib[match(names(libelles), u)] <- unname(libelles)
  data.frame(unite = u, categorie = categorie, base = base, groupe = groupe,
             facteur = unname(as.numeric(facteurs)), libelle = lib,
             stringsAsFactors = FALSE)
}

# Les composantes, pour batir les unites composees (doses) sans recopier un
# seul facteur.
.HSTAT_U_MASSE <- c("ng" = 1e-9, "µg" = 1e-6, "mg" = 1e-3, "cg" = 1e-2, "g" = 1,
                    "kg" = 1e3, "q" = 1e5, "t" = 1e6,
                    "oz" = 28.349523125, "lb" = 453.59237)
.HSTAT_U_VOLUME <- c("nl" = 1e-9, "µl" = 1e-6, "ml" = 1e-3, "cl" = 1e-2, "dl" = 0.1,
                     "l" = 1, "hl" = 100, "m³" = 1000,
                     "fl oz (US)" = 0.0295735295625, "gal (US)" = 3.785411784,
                     "gal (UK)" = 4.54609)
.HSTAT_U_SURFACE <- c("m²" = 1e-4, "100 m²" = 1e-2, "a" = 1e-2, "ha" = 1,
                      "km²" = 100, "acre" = 0.40468564224)

# Une dose est une quantite de produit par surface.
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
  .hstat_unites_bloc("volume", "l", "Volumes", .HSTAT_U_VOLUME),
  .hstat_unites_bloc("surface", "ha", "Surfaces", .HSTAT_U_SURFACE,
                     c("a" = "a (are)")),
  .hstat_unites_bloc("dose", "l/ha", "Produit liquide (volume par surface)",
    .hstat_unites_doses(c("µl/m²", "ml/m²", "ml/100 m²", "l/100 m²", "ml/a", "l/a",
                          "ml/ha", "cl/ha", "l/ha", "hl/ha", "fl oz/acre", "gal/acre"),
                        1, .HSTAT_U_VOLUME)),
  .hstat_unites_bloc("dose", "kg/ha", "Produit solide (masse par surface)",
    .hstat_unites_doses(c("mg/m²", "g/m²", "g/100 m²", "kg/100 m²", "g/a", "kg/a",
                          "mg/ha", "g/ha", "kg/ha", "q/ha", "t/ha", "oz/acre", "lb/acre"),
                        1000, .HSTAT_U_MASSE)),
  .hstat_unites_bloc("concentration", "g/l", "Masse par volume (m/v)",
    c("ng/l" = 1e-9, "µg/l" = 1e-6, "mg/l" = 1e-3, "g/l" = 1, "g/hl" = 1e-2,
      "kg/l" = 1e3, "g/m³" = 1e-3, "kg/m³" = 1, "ng/ml" = 1e-6, "µg/ml" = 1e-3,
      "mg/ml" = 1, "g/ml" = 1e3, "µg/µl" = 1, "mg/dl" = 1e-2, "%" = 10,
      "ppm" = 1e-3, "ppb" = 1e-6),
    c("%" = "% (m/v, g/100 ml)", "ppm" = "ppm (mg/l)", "ppb" = "ppb (µg/l)")),
  .hstat_unites_bloc("concentration", "g/kg", "Masse par masse (m/m)",
    c("ng/g" = 1e-6, "µg/kg" = 1e-6, "mg/kg" = 1e-3, "g/kg" = 1, "µg/g" = 1e-3,
      "mg/g" = 1, "% m/m" = 10, "ppm m/m" = 1e-3, "ppb m/m" = 1e-6)),
  .hstat_unites_bloc("concentration", "mol/l", "Molaire (quantité de matière par volume)",
    c("mol/l" = 1, "mmol/l" = 1e-3, "µmol/l" = 1e-6, "nmol/l" = 1e-9,
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

HSTAT_DOSE_UNITES <- .hstat_unites_vecteur("dose")
HSTAT_CONC_UNITES <- .hstat_unites_vecteur("concentration")
HSTAT_VOL_UNITES  <- .hstat_unites_vecteur("volume")

# Choix d'un selecteur, groupes par famille.
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

# LE LITRE S'ECRIT « l », A LA DEMANDE : ml, µl, g/l, l/ha, mol/l.
.HSTAT_UNITE_L <- "(?<![A-Za-z\u00C0-\u00FF_'])([n\u00B5mcdhk]?)L(?![A-Za-z\u00C0-\u00FF_'])"
.hstat_unite_cle <- function(u) {
  u <- as.character(u)
  if (!length(u)) return(u)
  hors <- !is.na(u) & !(u %in% HSTAT_UNITES$unite)
  u[hors] <- gsub(.HSTAT_UNITE_L, "\\1l", u[hors], perl = TRUE)
  u
}

hstat_unite_info <- function(u) {
  u <- .hstat_unite_cle(as.character(u)[1])[1]
  if (is.na(u) || !nzchar(u)) return(NULL)
  i <- match(u, HSTAT_UNITES$unite)
  if (is.na(i)) NULL else HSTAT_UNITES[i, , drop = FALSE]
}

# hstat_unite_convertir() -- la seule porte de conversion du module
# `densite` est la MASSE VOLUMIQUE du produit en kg/L (soit g/mL),
# `masse_molaire` celle de la matiere active en g/mol.
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
    rate(trf("Passer de %s à %s demande la masse volumique du produit (kg/l, soit g/ml) : renseignez-la.",
             a$unite, b$unite))
  manque_M <- function()
    rate(trf("Passer de %s à %s demande la masse molaire de la matière active (g/mol) : renseignez-la.",
             a$unite, b$unite))
  pont <- paste(a$base, b$base, sep = ">")
  y <- switch(pont,
    "g>l"         = if (is.na(rho)) return(manque_rho()) else x / (rho * 1000),
    "l>g"         = if (is.na(rho)) return(manque_rho()) else x * rho * 1000,
    "l/ha>kg/ha"  = if (is.na(rho)) return(manque_rho()) else x * rho,
    "kg/ha>l/ha"  = if (is.na(rho)) return(manque_rho()) else x / rho,
    "g/kg>g/l"    = if (is.na(rho)) return(manque_rho()) else x * rho,
    "g/l>g/kg"    = if (is.na(rho)) return(manque_rho()) else x / rho,
    "mol/l>g/l"   = if (is.na(M)) return(manque_M()) else x * M,
    "g/l>mol/l"   = if (is.na(M)) return(manque_M()) else x / M,
    "mol/l>g/kg"  = if (is.na(M)) return(manque_M()) else if (is.na(rho)) return(manque_rho()) else x * M / rho,
    "g/kg>mol/l"  = if (is.na(M)) return(manque_M()) else if (is.na(rho)) return(manque_rho()) else x * rho / M,
    NULL)
  if (is.null(y))
    return(rate(trf("Conversion impossible : %s et %s ne mesurent pas la même grandeur.",
                    a$unite, b$unite)))
  y / b$facteur
}

# Toutes les conversions d'une valeur, pour le convertisseur.
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
    note[i] <- if (is.finite(r[1])) trf("1 %s = %s %s", a$unite, hstat_fmt_complet(f), u$unite[i])
               else attr(r, "motif") %||% ""
  }
  data.frame(Groupe = u$groupe, Unite = u$libelle, Valeur = val, Conversion = note,
             stringsAsFactors = FALSE)
}

# Les formules, ecrites une fois.
HSTAT_DOSE_FORMULES <- c(
  grammage   = "Grammage (g m.a./ha) = Dose (l ou kg/ha) × Concentration (g/l ou g/kg)",
  dose       = "Dose (l ou kg/ha) = Grammage (g m.a./ha) ÷ Concentration (g/l ou g/kg)",
  conc_bouil = "Concentration de la bouillie (g m.a./l) = Grammage (g m.a./ha) ÷ Volume de bouillie (l/ha)",
  dose_bouil = "Dose dans la bouillie (ml ou g de produit/l) = Dose (ml ou g/ha) ÷ Volume de bouillie (l/ha)",
  total_prod = "Quantité totale de produit = Dose (par ha) × Superficie (ha)",
  total_ma   = "Quantité totale de matière active = Grammage (g/ha) × Superficie (ha)",
  total_eau  = "Volume d'eau total = Volume de bouillie (l/ha) × Superficie (ha)",
  par_cuve   = "Produit par cuve = Dose (par ha) × (Volume de cuve (l) ÷ Volume de bouillie (l/ha))",
  nb_cuves   = "Nombre de cuves = Volume d'eau total (l) ÷ Volume de cuve (l)",
  surf_cuve  = "Surface par cuve (ha) = Volume de cuve (l) ÷ Volume de bouillie (l/ha)",
  conservation = "Conservation de la matière : Vi × Ci = Vf × Cf (Vi prélevé dans la mère, Ci = concentration mère)",
  c_fille    = "Concentration de la fille n (Cf) = Concentration de la fille n−1 ÷ Coefficient de dilution",
  c_fille_n  = "soit Concentration de la fille n = Concentration mère ÷ Coefficient^n",
  v_preleve  = "Volume à prélever dans la mère (Vi) = Vf × Cf ÷ Concentration mère = Volume final ÷ Coefficient^n",
  v_eau      = "Volume d'eau à ajouter = Volume final (Vf) − Volume prélevé (Vi)",
  v_restant  = "Volume restant de la fille n (en cascade) = Volume final − Volume prélevé pour la fille n+1",
  v_mere     = "Volume de solution mère requis = somme des volumes prélevés sur la mère",
  c_totale   = "Concentration totale de la fille = somme des concentrations filles de chaque matière active")

# Conversions : une seule porte d'entree
# Une unite inconnue ne doit JAMAIS valoir 1 par defaut : un facteur muet
# rendrait un resultat mille fois trop grand sans le moindre signe.
.hstat_dose_facteur <- function(unite, table) {
  u <- .hstat_unite_cle(as.character(unite)[1])[1]
  if (is.na(u) || !nzchar(u) || !(u %in% names(table))) return(NA_real_)
  unname(table[[u]])
}

hstat_dose_vers_ref <- function(valeur, unite)
  suppressWarnings(as.numeric(valeur)[1]) * .hstat_dose_facteur(unite, HSTAT_DOSE_UNITES)

# La concentration ramenee a sa base (g/L, g/kg ou mol/L).
hstat_conc_vers_ref <- function(valeur, unite, masse_molaire = NA) {
  v <- suppressWarnings(as.numeric(valeur)[1]) * .hstat_dose_facteur(unite, HSTAT_CONC_UNITES)
  b <- hstat_unite_info(unite)
  if (!is.null(b) && identical(b$base, "mol/l")) v <- v * .hstat_unite_pos(masse_molaire)
  v
}

# Rendre une valeur de reference (L/ha ou kg/ha) dans l'unite demandee.
hstat_dose_depuis_ref <- function(valeur, unite) {
  f <- .hstat_dose_facteur(unite, HSTAT_DOSE_UNITES)
  if (!isTRUE(is.finite(f)) || f == 0) return(NA_real_)
  valeur / f
}

# Nombre lisible : `dec` decimales au plus, sans zeros inutiles.
# 1. LE RETRAIT DES ZEROS MANGEAIT LES ENTIERS.
# 2. UNE VALEUR NON NULLE QUI S'ARRONDIT A ZERO S'AFFICHAIT « 0 ».
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

# LES CHIFFRES COMPLETS : la masse a peser et la concentration de la mere ne
# s'arrondissent pas.
hstat_fmt_complet <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  out <- trimws(formatC(v, digits = 15, format = "fg", big.mark = " "))
  out[!is.finite(v)] <- NA_character_
  out
}

# 1 et 2. Dose <-> grammage
# `sens` dit ce que l'utilisateur CONNAIT : "dose" -- il connait la dose
# homologuee, il cherche le grammage ; "grammage".
hstat_dose_bilan <- function(sens = c("dose", "grammage"),
                             valeur, unite_valeur,
                             concentration, unite_concentration,
                             volume_bouillie = NA_real_,
                             superficie = 1,
                             volume_cuve = NA_real_,
                             produit = "", matiere = "",
                             densite = NA_real_, masse_molaire = NA_real_,
                             unite_bouillie = "l/ha", unite_superficie = "ha",
                             unite_cuve = "l",
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

  # L'ETAT DU PRODUIT -- liquide (dose en volume) ou solide (dose en masse).
  if (sens == "dose") {
    di <- hstat_unite_info(unite_valeur)
    if (is.null(di) || !identical(di$categorie, "dose"))
      return(vide(tr("Unité de dose inconnue : choisissez une quantité de produit par surface (ml/ha, l/ha, g/ha, kg/ha…).")))
    base_dose <- di$base
  } else {
    base_dose <- if (identical(ci$base, "g/kg")) "kg/ha" else "l/ha"
  }
  # La concentration se ramene a des grammes PAR UNITE DE PRODUIT DOSE.
  conc <- hstat_unite_convertir(c_val, ci$unite,
                                if (identical(base_dose, "l/ha")) "g/l" else "g/kg",
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
    # millilitre en dessous.
    u_dose <- if (identical(base_dose, "kg/ha")) { if (dose_ref >= 1) "kg/ha" else "g/ha" }
              else if (dose_ref >= 1) "l/ha" else "ml/ha"
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
  vb   <- conv_pos(volume_bouillie, unite_bouillie, "l/ha")
  cuve <- conv_pos(volume_cuve, unite_cuve, "l")
  a_vb   <- isTRUE(is.finite(vb)) && vb > 0
  a_cuve <- a_vb && isTRUE(is.finite(cuve)) && cuve > 0

  liq   <- identical(base_dose, "l/ha")
  u_pet <- if (liq) "ml/ha" else "g/ha"          # petite unite du produit
  u_tot <- if (liq) "l" else "kg"

  # Bouillie : la ou se joue l'erreur de terrain.
  conc_bouillie <- if (a_vb) grammage / vb else NA_real_
  dose_bouillie <- if (a_vb) hstat_dose_depuis_ref(dose_ref / vb, u_pet) else NA_real_
  eau_totale    <- if (a_vb) vb * surf else NA_real_
  par_cuve      <- if (a_cuve) hstat_dose_depuis_ref(dose_ref * (cuve / vb), u_pet) else NA_real_
  nb_cuves      <- if (a_cuve) eau_totale / cuve else NA_real_
  surf_cuve     <- if (a_cuve) cuve / vb else NA_real_

  # La superficie est ecrite dans l'unite saisie : « pour 500 m² » se relit,
  # « pour 0.05 ha » oblige a refaire la conversion de tete.
  s_aff <- paste(hstat_fmt_complet(surf_saisie), u_surf)
  res <- data.frame(
    Grandeur = c(
      trf("Dose de produit commercial (%s)", u_dose),
      "Grammage de matière active (g m.a./ha)",
      "Concentration de la bouillie (g m.a./l)",
      trf("Dose dans la bouillie (%s de produit/l)", if (liq) "ml" else "g"),
      trf("Quantité totale de produit pour %s (%s)", s_aff, u_tot),
      trf("Quantité totale de matière active pour %s (g)", s_aff),
      trf("Volume d'eau total pour %s (l)", s_aff),
      trf("Produit par cuve (%s)", if (liq) "ml" else "g"),
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
  # rendements.
  ajoute_conv <- function(res, apres, valeur, de, vers, gabarit) {
    if (is.null(vers) || !nzchar(vers[1])) return(res)
    vi <- hstat_unite_info(vers[1])
    if (is.null(vi) || identical(vi$unite, de)) return(res)
    vers <- vi$unite                          # la cle, ecrite en « l »
    r <- hstat_unite_convertir(valeur, de, vers[1], densite, masse_molaire)
    f <- hstat_unite_convertir(1, de, vers[1], densite, masse_molaire)
    ligne <- data.frame(
      Grandeur = trf(gabarit, vers[1]), Valeur = r[1],
      Formule = if (is.finite(r[1])) trf("Conversion : 1 %s = %s %s", de,
                                         hstat_fmt_complet(f[1]), vers[1])
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

# 3. Dilutions : solutions filles a partir d'une solution mere
# L'unite de saisie est le couple (produit, matiere active) : un produit a
# plusieurs matieres actives occupe plusieurs lignes.
HSTAT_DILUTION_COLS <- c("Produit", "Matiere_active", "Concentration_mere",
                         "Unite", "Coefficient", "Volume_final", "Unite_volume")

HSTAT_DILUTION_NB_MAX <- 50L

# En dessous, le prelevement ne se mesure plus a la pipette.
HSTAT_DILUTION_MIN_ML <- 0.01

# Au-dela, un nombre a virgule flottante double ne porte plus d'information.
HSTAT_DEC_MAX <- 15L

hstat_dilution_table_vide <- function(n = 3) {
  data.frame(
    Produit              = rep("", n),
    Matiere_active       = rep("", n),
    Concentration_mere   = rep(NA_real_, n),
    Concentration_depart = rep(NA_real_, n),
    Unite                = rep("g/l", n),
    Coefficient          = rep(NA_real_, n),
    Nb_filles            = rep(1, n),
    Volume_final         = rep(NA_real_, n),
    Unite_volume         = rep("l", n),
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
  d$Nb_filles <- if ("Nb_filles" %in% names(df))
    suppressWarnings(as.numeric(df$Nb_filles)) else rep(1, nrow(d))
  d$Nb_filles[!is.finite(d$Nb_filles)] <- 1

  # LA CONCENTRATION DE DEPART EST FACULTATIVE, et c'est tout son interet : une
  # gamme ne commence pas toujours a C_mere/k.
  d$Concentration_depart <- if ("Concentration_depart" %in% names(df))
    suppressWarnings(as.numeric(df$Concentration_depart)) else rep(NA_real_, nrow(d))

  # Masse volumique de la mere (kg/L) et masse molaire de la matiere active
  # (g/mol).
  for (k in c("Densite", "Masse_molaire"))
    d[[k]] <- if (k %in% names(df)) suppressWarnings(as.numeric(df[[k]]))
              else rep(NA_real_, nrow(d))

  # Une ligne entierement vide est un reste de saisie, pas une erreur : on la
  # retire en silence.
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

  # Coefficient, volume final et nombre de filles appartiennent au produit.
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
    return(vide(trf("Unité de concentration inconnue : %s. Choisissez une unité de la liste (g/l, mg/l, %%, ppm, mol/l…).",
                    paste(unique(d$Unite[!is.finite(fac)]), collapse = ", "))))

  fac_v <- vapply(d$Unite_volume, function(u) .hstat_dose_facteur(u, HSTAT_VOL_UNITES),
                  numeric(1), USE.NAMES = FALSE)
  if (any(!is.finite(fac_v)))
    return(vide(trf("Unité de volume inconnue : %s. Choisissez une unité de la liste (µl, ml, l…).",
                    paste(unique(d$Unite_volume[!is.finite(fac_v)]), collapse = ", "))))

  d$.fac   <- fac
  d$.fac_v <- fac_v
  d <- d[order(d$Produit, d$Matiere_active), , drop = FALSE]

  # -- Le PREMIER etage, quand l'utilisateur le fixe.
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
  # Les trois refus sont ecrits comme des appels LITTERAUX a `trf()`.
  refus <- function(motif, p) switch(motif,
    nulle = trf("Concentration de départ nulle ou négative pour : %s. Indiquez la concentration de la première fille, ou laissez la case vide pour partir de la mère divisée par le coefficient.", p),
    plus_concentree = trf("Concentration de départ supérieure à la concentration mère pour : %s. Une dilution ne concentre pas : choisissez une valeur inférieure ou égale à la mère.", p),
    contradictoire = trf("Concentrations de départ contradictoires pour : %s. Une dilution divise toutes les matières actives d'un flacon par le même nombre : les concentrations de départ doivent être dans le même rapport que les concentrations mères.", p))

  # -- La gamme : une fille par rang, pour chaque matiere active.
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

  # La concentration ramenee a g/L rend les matieres actives ADDITIONNABLES.
  g_l <- vapply(seq_len(nrow(d)), function(i)
    hstat_unite_convertir(d$Concentration_fille[i], d$Unite[i], "g/l",
                          d$Densite[i], d$Masse_molaire[i])[1], numeric(1))
  cle <- paste(d$Produit, d$Rang, sep = "\u001f")
  totaux <- tapply(g_l, cle, sum)                 # un NA garde le total a NA
  d$Concentration_totale_g_L <- unname(totaux[cle])
  sans_total <- unique(d$Produit[!is.finite(g_l)])

  # Ce que la MERE doit fournir : la somme de tous les prelevements, puisqu'ils
  # viennent tous d'elle.
  v_mere <- tapply(ifelse(!duplicated(cle), d$Volume_a_prelever, 0),
                   d$Produit, sum, na.rm = TRUE)

  d <- d[order(d$Produit, d$Rang, d$Matiere_active), , drop = FALSE]
  # Le volume a prelever, l'eau a ajouter et la concentration totale
  # appartiennent au couple (PRODUIT, RANG).
  cle <- paste(d$Produit, d$Rang, sep = "\u001f")
  suivante <- duplicated(cle)
  for (k2 in c("Volume_a_prelever", "Volume_eau_a_ajouter", "Concentration_totale_g_L"))
    d[[k2]][suivante] <- NA_real_
  # Le besoin en solution mere est propre au PRODUIT : une seule ligne.
  d$Volume_mere_requis <- NA_real_
  d$Volume_mere_requis[!duplicated(d$Produit)] <-
    unname(v_mere[unique(d$Produit)])

  # La colonne de depart DISPARAIT quand personne ne s'en sert : une colonne
  # vide fait croire a une information absente.
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
      "Volume à prélever inférieur à %s ml pour : %s. Un tel prélèvement ne se mesure pas à la pipette : augmentez le volume final, réduisez le coefficient de dilution ou préparez la gamme en plusieurs étapes.",
      hstat_fmt_nb(HSTAT_DILUTION_MIN_ML, 3), paste(minuscule, collapse = ", ")),
    if (length(sans_total)) trf(
      "Concentration totale non calculée pour : %s. Additionner des matières actives demande de les ramener en g/l : renseignez la masse volumique de la mère (unités m/m) ou la masse molaire (unités molaires).",
      paste(sans_total, collapse = ", ")))
  if (length(avert)) attr(res, "avertissement") <- paste(avert, collapse = " ")
  res
}

# Les noms de colonne sont des IDENTIFIANTS.
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
  Concentration_totale_g_L  = "Concentration totale de la fille (g/l)",
  Volume_mere_requis        = "Solution mère nécessaire (total)",
  Densite                   = "Masse volumique de la mère (kg/l)",
  Masse_molaire             = "Masse molaire (g/mol)",
  Unite_conc_conv           = "Unité de concentration convertie",
  Concentration_mere_conv   = "Concentration mère convertie",
  Concentration_precedente_conv = "Concentration précédente convertie",
  Concentration_fille_conv  = "Concentration fille convertie",
  # Les colonnes de conversion.
  Unite_volume_conv         = "Unité convertie",
  Volume_final_conv         = "Volume final converti",
  Volume_a_prelever_conv    = "Volume à prélever converti",
  Volume_eau_a_ajouter_conv = "Eau à ajouter convertie",
  Volume_mere_requis_conv   = "Solution mère nécessaire convertie")

# Volumes ramenes a UNE unite commune.
HSTAT_DILUTION_VOL_COLS <- c("Volume_final", "Volume_a_prelever",
                             "Volume_eau_a_ajouter", "Volume_mere_requis")

hstat_dilution_convertir <- function(d, unite = NULL, unite_conc = NULL) {
  if (!is.data.frame(d) || !nrow(d)) return(d)
  d <- .hstat_dilution_convertir_conc(d, unite_conc)
  if (is.null(unite) || !nzchar(unite) || !"Unite_volume" %in% names(d)) return(d)
  vers <- .hstat_dose_facteur(unite, HSTAT_VOL_UNITES)
  # Une unite inconnue ne doit pas rendre des volumes faux.
  if (!isTRUE(is.finite(vers)) || vers == 0) return(d)
  depuis <- vapply(as.character(d$Unite_volume),
                   function(u) .hstat_dose_facteur(u, HSTAT_VOL_UNITES),
                   numeric(1), USE.NAMES = FALSE)
  ok <- is.finite(depuis) & depuis > 0
  if (!any(ok)) return(d)

  # LA CONVERSION AJOUTE, ELLE NE REMPLACE PAS.
  for (k in intersect(HSTAT_DILUTION_VOL_COLS, names(d))) {
    v <- rep(NA_real_, nrow(d))
    v[ok] <- d[[k]][ok] * depuis[ok] / vers
    d <- .hstat_col_apres(d, k, paste0(k, "_conv"), v)
  }
  # L'unite convertie est une colonne A PART : `Unite_volume` continue de dire
  # dans quoi l'utilisateur a saisi.
  u <- rep(NA_character_, nrow(d))
  u[ok] <- unite
  .hstat_col_apres(d, "Unite_volume", "Unite_volume_conv", u)
}

# Les concentrations converties, sur le meme principe que les volumes.
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

# `.hstat_col_apres()` a rejoint le socle (`Utils.R`).

# Le tableau des solutions filles decoupe par produit, une entree par produit
# dans l'ordre du tableau, chacune relabellisee comme a l'ecran.
hstat_dilution_feuilles <- function(d) {
  if (!is.data.frame(d) || !nrow(d) || !"Produit" %in% names(d)) return(list())
  p <- as.character(d$Produit)
  p[is.na(p) | !nzchar(trimws(p))] <- tr("Produit sans nom")
  niv <- unique(p)
  stats::setNames(lapply(niv, function(x) {
    s <- d[p == x, , drop = FALSE]
    rownames(s) <- NULL
    hstat_dilution_affichage(s)
  }), niv)
}

hstat_dilution_affichage <- function(d) {
  if (!is.data.frame(d) || !nrow(d)) return(d)
  nm  <- names(d)
  vus <- nm %in% names(HSTAT_DILUTION_LIBELLES)
  nm[vus] <- unname(HSTAT_DILUTION_LIBELLES[nm[vus]])
  names(d) <- nm
  d
}

# INTERFACE
# Deux onglets, parce que ce sont deux questions distinctes : ce qu'on apporte
# a l'hectare, et ce qu'on prepare au laboratoire.

.hstat_dose_bloc_formule <- function(txt) {
  shiny::div(
    style = paste0("background:#eef7ff;border-left:4px solid #2e86c1;border-radius:6px;",
                   "padding:10px 14px;margin:12px 0;font-family:'IBM Plex Mono',",
                   "Consolas,monospace;font-size:12.5px;color:#1b4f72;"),
    lapply(txt, function(f) shiny::div(style = "margin:3px 0;", f)))
}

# SOLUTION MERE : LES TROIS TYPES DE TEST DES PROTOCOLES DU PROGRAMME COTON
# Le type de test decide de la GRANDEUR publiee et de la formule de la mere :
HSTAT_MERE_TESTS <- list(
  topique = list(
    libelle   = "Application topique (dose par masse corporelle) — DL",
    grandeur  = "dose", unite = "µg/g", methode = "Application topique",
    protocole = "Test d'application topique — chenilles carpophages, matière active de grade technique",
    solvant   = "Acétone", repetitions = 4L, individus = 10L, seuil_rejet = 10,
    lectures = "24 h, 48 h et 72 h"),
  disque = list(
    libelle   = "Trempage de disques foliaires, IRAC 035 (concentration) — CL",
    grandeur  = "concentration", unite = "mg/l",
    methode   = "Trempage de disques foliaires (IRAC 035)",
    protocole = "Protocole IRAC 015, 016 et 035",
    solvant   = "Selon la formulation", repetitions = 4L, individus = 10L,
    seuil_rejet = 10,
    lectures  = "24 h, 48 h, 72 h, 96 h et 120 h"),
  tube = list(
    libelle   = "Tubes imprégnés, vial test au champ (quantité par tube) — CL",
    grandeur  = "concentration", unite = "µg/tube",
    methode   = "Tubes imprégnés (vial test)",
    protocole = "Surveillance de la résistance au champ de H. armigera aux pyréthrinoïdes",
    solvant   = "Acétone", repetitions = 1L, individus = 30L,
    seuil_rejet = 10,
    lectures  = "24 h (et si possible 48 h)"))

# Les choix du selecteur, derives du catalogue : une liste recopiee finirait
# par diverger.
HSTAT_MERE_TESTS_CHOIX <- stats::setNames(
  names(HSTAT_MERE_TESTS), vapply(HSTAT_MERE_TESTS, `[[`, "", "libelle"))

# Formulations : ce que porte l'etiquette decide de la conversion.
HSTAT_MERE_PRODUITS <- c(
  "Matière active technique (pureté en %)"              = "technique",
  "Formulation liquide en g/l de matière active"         = "liquide_gl",
  "Formulation solide en % massique (m/m)"               = "solide_pct",
  "Formulation liquide en % massique (m/m) + densité"    = "liquide_pct")

HSTAT_MERE_FORMULES <- c(
  topique     = "Q (µg) = D (µg/g) × x (g) × V (µl) × 100 ÷ [a (µl) × P (%)]",
  topique_c   = "Concentration de la solution mère (µg m.a./µl) = D (µg/g) × x (g) ÷ a (µl)",
  technique   = "m (mg) = C (mg/ml) × V (ml) ÷ (P / 100)",
  controle    = "C réelle (mg/ml) = m (mg) × (P / 100) ÷ V final (ml)",
  liquide_gl  = "Volume de formulation (ml) = C (mg/ml) × V (ml) ÷ Teneur (g/l), car 1 g/l = 1 mg/ml",
  solide_pct  = "m formulation (mg) = m matière active cible (mg) ÷ (P / 100)",
  liquide_pct = "Volume de formulation (ml) = m formulation (mg) ÷ [densité (g/ml) × 1000]",
  tube        = "Q (mg) = C (mg/l) × V (l) × 100 ÷ P (%)",
  tube_c      = "Concentration de la solution mère = C",
  tube_flacon = "Quantité par flacon (µg) = C (mg/l) × v (ml)",
  tube_prep   = "Q (mg) = C (mg/l) × volume (l) × 100 ÷ P (%)",
  abbott      = "Mortalité corrigée (%) = 100 × (mortalité traitement − mortalité témoin) ÷ (100 − mortalité témoin)")

# Un nombre strictement positif et fini, ou NA.
.hstat_mere_pos <- function(x) {
  v <- suppressWarnings(as.numeric(x)[1])
  if (length(v) == 1L && is.finite(v) && v > 0) v else NA_real_
}

# La purete est un pourcentage de ]0 ; 100].
.hstat_mere_purete <- function(P) {
  v <- .hstat_mere_pos(P)
  if (is.finite(v) && v <= 100) v else NA_real_
}

.hstat_mere_refus <- function(motif) list(ok = FALSE, motif = motif)

# -- Application topique
# D : dose cible (µg/g) ; x : masse moyenne de l'insecte (g) ; V : volume
# final de la mere (µL) ; a : volume applique par insecte (µL) ; P : purete (%).
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

# -- Concentration (trempage de disques, IRAC 035)
# C : concentration visee de la mere (mg/mL) ; V : volume final (mL).
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
      return(.hstat_mere_refus(tr("La teneur de la formulation (g/l) doit être un nombre strictement positif.")))
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
      return(.hstat_mere_refus(tr("Une formulation liquide en % massique demande sa densité (g/ml) pour passer de la masse au volume.")))
    res$v_formulation <- res$m_pese / (d * 1000)
  }
  res
}

# -- Tubes impregnes (vial test)
# C : concentration de matiere active (mg/l) ; V : volume a preparer (ml) ;
# P : purete (%) ; v : volume depose dans chaque flacon (ml).
hstat_mere_tube <- function(C, v, V, P, Vprep = NA) {
  C <- .hstat_mere_pos(C); v <- .hstat_mere_pos(v); V <- .hstat_mere_pos(V)
  P <- .hstat_mere_purete(P)
  if (anyNA(c(C, v, V)))
    return(.hstat_mere_refus(tr("La concentration de matière active, le volume de solution par flacon et le volume total doivent être des nombres strictement positifs.")))
  if (is.na(P))
    return(.hstat_mere_refus(tr("La pureté doit être comprise entre 0 (exclu) et 100 %.")))
  pesee <- function(vol_ml) C * vol_ml / 1000 * 100 / P
  Vprep <- .hstat_mere_pos(Vprep)
  list(ok = TRUE, Q_mg = pesee(V), c_mgl = C, c_mere = C / 1000,
       flacon_ug = C * v, flacons = V / v,
       Q_prep = if (is.na(Vprep)) NA_real_ else pesee(Vprep))
}

# Un champ du protocole et son unite.
HSTAT_MATIERES_ACTIVES <- c(
  "2,4-D (amine salt)", "Abamectin", "Acetamiprid", "Alpha-cypermethrin",
  "Ametryn", "Azadirachtin", "Azoxystrobin", "Bacillus thuringiensis",
  "Bacillus thuringiensis subsp. kurstaki",
  "Bacillus thuringiensis subsp. kurstaki strain EG2348",
  "Bacillus thuringiensis subsp. kurstaki strain PB54", "Beauveria bassiana",
  "Beta-cyfluthrin", "Beta-cypermethrin", "Bifenazate", "Bifenthrin",
  "Buprofezin", "Butachlor", "Cadusafos", "Capsicum oleoresin", "Cartap",
  "Chlorantraniliprole", "Chlorimuron-ethyl", "Chlorpyrifos", "Clethodim",
  "Cyantraniliprole", "Cyfluthrin", "Cyhalodiamide", "Cypermethrin",
  "Deltamethrin", "Diafenthiuron", "Diamine", "Difenoconazole",
  "Diflubenzuron", "Diflufenican", "Dimethoate", "Dinotefuran", "Diuron",
  "Emamectin", "Emamectin benzoate", "Etoxazole", "Flonicamid",
  "Fluazifop-P-butyl", "Flubendiamide", "Fludioxonil", "Flufenacet",
  "Fluometuron", "Flupyradifurone", "Fluxametamide", "Garlic oil",
  "Glyphosate", "Glyphosate-isopropylamine", "Haloxyfop-P-ethyl",
  "Haloxyfop-P-methyl", "Helicoverpa armigera nucleopolyhedrovirus",
  "Helicoverpa armigera single nucleopolyhedrovirus (HearSPNV)",
  "Imidacloprid", "Indoxacarb", "Isaria fumosorosea strain FE9901",
  "Isocycloseram", "Karanja oil", "Karanjin", "Lambda-cyhalothrin",
  "Lufenuron", "Malathion", "Mepiquat chloride", "Mesotrione", "Metalaxyl",
  "Metalaxyl-M", "Metamifop", "Metolachlor", "Nicosulfuron", "Nitenpyram",
  "Novaluron", "Orange oil", "Oxymatrine", "Pencycuron", "Pendimethalin",
  "Penoxsulam", "Permethrin", "Profenofos", "Prometryn", "Propaquizafop",
  "Propiconazole", "Pyridines", "Pyriproxyfen", "Pyrithiobac-sodium",
  "Pyroxasulfuron", "Quinalphos", "Quizalofop-P-ethyl", "Quizalofop-P-tefuryl",
  "Rimsulfuron", "S-metolachlor", "Spinetoram", "Spinosad", "Spiromesifen",
  "Spirotetramat", "Sulfoxaflor", "Tebuconazole", "Teflubenzuron",
  "Terbuthylazine", "Terbutryn", "Thiacloprid", "Thiamethoxam", "Thidiazuron",
  "Thiram", "Tolfenpyrad", "Trifloxysulfuron", "Trifloxysulfuron-sodium")

# Les ajouts de l'utilisateur, gardes d'une session a l'autre.
hstat_ma_fichier <- function() {
  f <- getOption("hstat.ma.fichier")
  if (!is.null(f)) return(if (is.na(f[1])) NA_character_ else as.character(f[1]))
  # Sous testthat, jamais le dossier reel de l'utilisateur : une suite qui
  # tape un nom hors liste l'y ecrirait a chaque passe.
  if (identical(Sys.getenv("TESTTHAT"), "true") || !interactive()) return(NA_character_)
  file.path(tools::R_user_dir("HStat", "data"), "matieres_actives_ajoutees.txt")
}

# Plafond des ajouts : au-dela, rien ne s'ecrit plus.
HSTAT_MA_AJOUTS_MAX <- 1000L

hstat_ma_ajoutees <- function(fichier = hstat_ma_fichier()) {
  if (is.na(fichier) || !file.exists(fichier)) return(character(0))
  # `n` : on ne lit pas plus que ce qu'on gardera.
  v <- tryCatch(readLines(fichier, n = HSTAT_MA_AJOUTS_MAX, warn = FALSE,
                          encoding = "UTF-8"),
                error = function(e) character(0))
  v <- trimws(enc2utf8(v))
  unique(v[nzchar(v)])
}

# Les choix proposes : la liste fournie, puis les ajouts.
hstat_ma_choix <- function(ajouts = character(0)) {
  ch <- unique(c(HSTAT_MATIERES_ACTIVES, as.character(ajouts)))
  ch[!is.na(ch) & nzchar(ch)]
}

# La liste complete proposee partout ou l'on saisit une matiere active : celle
# fournie, puis les ajouts enregistres.
hstat_ma_catalogue <- function(fichier = hstat_ma_fichier())
  hstat_ma_choix(hstat_ma_ajoutees(fichier))

# Un nom venu du navigateur.
.hstat_ma_nettoyer <- function(noms) {
  noms <- gsub("[[:cntrl:]]+", " ", as.character(noms))
  noms <- substr(trimws(noms), 1L, 120L)
  noms[!is.na(noms) & nzchar(noms)]
}

# Les ajouts de la SESSION, partages par les deux modules qui saisissent une
# matiere active.
hstat_ma_session <- function(session) {
  ud <- session$userData
  if (is.null(ud$hstat_ma)) ud$hstat_ma <- shiny::reactiveVal(hstat_ma_ajoutees())
  ud$hstat_ma
}

# Retient les noms tapes hors liste : rend ceux qui sont reellement nouveaux,
# enregistres ou non, et l'annonce.
HSTAT_MA_PAR_APPEL <- 20L
hstat_ma_retenir <- function(noms, connus = character(0)) {
  noms <- utils::head(.hstat_ma_nettoyer(noms), HSTAT_MA_PAR_APPEL)
  neuf <- unique(noms[!tolower(noms) %in% tolower(c(HSTAT_MATIERES_ACTIVES, connus))])
  if (!length(neuf)) return(character(0))
  ajout <- hstat_ma_ajouter(neuf)
  if (length(ajout) && !is.null(shiny::getDefaultReactiveDomain()))
    shiny::showNotification(trf("Ajouté à la liste des matières actives : %s",
                                paste(ajout, collapse = " ; ")),
                            type = "message", duration = 5)
  neuf
}

# Ajoute ce qui est NOUVEAU, une ligne par nom.
hstat_ma_ajouter <- function(noms, fichier = hstat_ma_fichier()) {
  noms <- .hstat_ma_nettoyer(noms)
  deja <- hstat_ma_ajoutees(fichier)
  connus <- tolower(c(HSTAT_MATIERES_ACTIVES, deja))
  neufs <- unique(noms[!tolower(noms) %in% connus])
  neufs <- neufs[!duplicated(tolower(neufs))]
  if (!length(neufs) || is.na(fichier)) return(neufs)
  # Au plafond, le fichier ne grossit plus : les ajouts restent ceux de la
  # session.
  neufs <- utils::head(neufs, max(0L, HSTAT_MA_AJOUTS_MAX - length(deja)))
  if (!length(neufs)) return(character(0))
  ok <- tryCatch({
    dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
    con <- file(fichier, open = "a", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(neufs, con)
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(ok)) neufs else character(0)
}

.hstat_mere_champ <- function(ns, id, libelle, valeur, categorie, base, unite) {
  shiny::fluidRow(
    shiny::column(7, shiny::numericInput(ns(id), libelle, value = valeur, min = 0)),
    shiny::column(5, shiny::selectInput(ns(paste0(id, "Unite")), "Unité",
                                        choices = hstat_unites_choix(categorie, base),
                                        selected = unite)))
}

# La concentration de la mere, dans l'unite ou elle a ete obtenue : la saisie
# de l'utilisateur pour un trempage ou un vial test, le µg/µl de l'application
# topique. Une unite que la liste des dilutions ignore retombe sur le mg/ml.
hstat_mere_conc_unite <- function(test, r, saisie = NULL) {
  repli <- list(valeur = r$c_mere, unite = "mg/ml")
  if (identical(test, "topique")) return(list(valeur = r$c_mere, unite = "µg/µl"))
  if (is.null(saisie) || !isTRUE(is.finite(saisie$v)) || saisie$v <= 0) return(repli)
  u <- .hstat_unite_cle(as.character(saisie$u %||% "")[1])
  b <- hstat_unite_info(u)
  if (is.null(b) || !identical(b$categorie, "concentration") ||
      !u %in% unlist(hstat_unites_choix("concentration"))) return(repli)
  list(valeur = saisie$v, unite = u)
}

# Le nom d'une solution saisie sans nom commercial : ses matieres actives, ou
# « Solution mère » ; un nom deja pris recoit un numero.
hstat_dilution_nom_defaut <- function(matieres, pris = character(0)) {
  m <- trimws(as.character(matieres))
  m <- m[nzchar(m)]
  base <- if (length(m)) paste(m, collapse = " + ") else tr("Solution mère")
  nom <- base; k <- 2L
  while (nom %in% pris) { nom <- paste0(base, " (", k, ")"); k <- k + 1L }
  nom
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

    # L'ARRONDI EST UN CHOIX DE L'UTILISATEUR, jamais un defaut.
    shiny::div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 12px;",
      shiny::fluidRow(
        shiny::column(4, shiny::checkboxInput(ns("dosArrondir"), "Arrondir les résultats", value = FALSE)),
        shiny::column(4, shiny::conditionalPanel(
          ns = ns, condition = "input.dosArrondir == true",
          shiny::numericInput(ns("dosDecimales"), "Décimales :", value = 2,
                              min = 0, max = HSTAT_DEC_MAX, step = 1))))),

    shiny::tabsetPanel(
      id = ns("dosageTabs"),

      shiny::tabPanel(
        shiny::tagList(shiny::icon("seedling"), " Dose et grammage à l'hectare"),
        shiny::div(style = "padding-top:14px;"),
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("sliders"), " Le produit"),
            status = "primary", width = 4, solidHeader = TRUE,
            shiny::textInput(ns("doseProduit"), "Nom commercial du produit",
                             placeholder = "ex : Cypercal 50 EC"),
            hstat_ma_input(ns("doseMatiere"), "Matière active",
                           placeholder = "ex : Cypermethrin"),
            shiny::fluidRow(
              shiny::column(7, shiny::numericInput(ns("doseConc"),
                "Concentration en matière active", value = 400, min = 0, step = 1)),
              shiny::column(5, shiny::selectInput(ns("doseConcUnite"), "Unité",
                choices = hstat_unites_choix("concentration"), selected = "g/l"))),
            # Les deux ponts entre familles d'unites.
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("doseDensite"),
                "Masse volumique du produit (kg/l)", value = NULL, min = 0, step = 0.01)),
              shiny::column(6, shiny::numericInput(ns("doseMasseMol"),
                "Masse molaire de la m.a. (g/mol)", value = NULL, min = 0, step = 0.1))),
            shiny::helpText("Facultatives. La masse volumique (= densité, en g/ml) relie une",
                            " dose en volume à une concentration en masse par masse",
                            " (g/kg, % m/m) ; la masse molaire relie une concentration",
                            " molaire (mol/l, mM) à des grammes."),

            shiny::hr(),
            shiny::radioButtons(ns("doseSens"), "Ce que vous connaissez",
              choiceNames = list(
                shiny::HTML("<b>La dose homologuée</b> <small style='color:#5f6b7a;'>(je cherche le grammage)</small>"),
                shiny::HTML("<b>Le grammage visé</b> <small style='color:#5f6b7a;'>(je cherche la dose)</small>")),
              choiceValues = list("dose", "grammage"), selected = "dose"),

            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'dose'", ns("doseSens")),
              shiny::fluidRow(
                shiny::column(7, shiny::numericInput(ns("doseValeur"),
                  "Dose homologuée", value = 250, min = 0, step = 1)),
                shiny::column(5, shiny::selectInput(ns("doseUnite"), "Unité",
                  choices = hstat_unites_choix("dose"), selected = "ml/ha")))),
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
                choices = hstat_unites_choix("dose", "l/ha"), selected = "l/ha"))),
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
                choices = hstat_unites_choix("volume"), selected = "l"))),
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

      # LA SOLUTION MERE SE PREPARE AVANT L'ESSAI, et le type de test decide de
      # sa formule comme de la grandeur publiee (DL ou CL).
      shiny::tabPanel(
        shiny::tagList(shiny::icon("flask"), " Solution mère"), value = "mere",
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
                shiny::column(4, .hstat_mere_champ(ns, "mereTopV", "Volume final de la mère V", 50, "volume", NULL, "ml")),
                shiny::column(4, .hstat_mere_champ(ns, "mereTopA", "Volume appliqué par insecte a", 1, "volume", NULL, "µl"))),
              shiny::helpText("La masse moyenne x s'obtient en pesant 10 chenilles. Chaque valeur se saisit dans l'unité choisie à côté d'elle ; les formules et les résultats restent dans les unités du protocole.")),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'disque'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("mereProduit"), "Produit utilisé",
                                                    choices = HSTAT_MERE_PRODUITS, selected = "technique")),
                shiny::column(3, .hstat_mere_champ(ns, "mereDisC", "Concentration de la solution mère C", 1, "concentration", "g/l", "mg/ml")),
                shiny::column(3, .hstat_mere_champ(ns, "mereDisV", "Volume final V", 50, "volume", NULL, "ml"))),
              shiny::fluidRow(
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] != 'liquide_gl'", ns("mereProduit")),
                  shiny::column(4, shiny::numericInput(ns("mereDisP"), "Pureté ou teneur P (%)", value = 95, min = 0, max = 100))),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'liquide_gl'", ns("mereProduit")),
                  shiny::column(4, .hstat_mere_champ(ns, "mereDisTeneur", "Teneur de la formulation", 100, "concentration", "g/l", "g/l"))),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] == 'liquide_pct'", ns("mereProduit")),
                  shiny::column(4, shiny::numericInput(ns("mereDisDensite"), "Densité de la formulation (g/ml)", value = 1, min = 0)))),
              shiny::helpText("Ne pas assimiler un % m/m à des g/l sans la densité. Le solvant est ajouté jusqu'au volume final, sans confondre volume de solvant ajouté et volume final.")),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'tube'", ns("mereTest")),
              shiny::fluidRow(
                shiny::column(6, .hstat_mere_champ(ns, "mereTubC", "Concentration de matière active C", 40, "concentration", "g/l", "mg/l")),
                shiny::column(6, .hstat_mere_champ(ns, "mereTubVflacon", "Volume de solution par flacon v", 1, "volume", NULL, "ml"))),
              shiny::fluidRow(
                shiny::column(6, .hstat_mere_champ(ns, "mereTubV", "Volume total de solution à préparer V", 1, "volume", NULL, "l")),
                shiny::column(6, shiny::numericInput(ns("mereTubP"), "Pureté P (%)", value = 97.5, min = 0, max = 100))),
              # La pesee pour un autre volume que V, a la meme concentration.
              shiny::fluidRow(
                shiny::column(6, .hstat_mere_champ(ns, "mereTubVprep", "Convertir la masse à peser pour un volume de", 100, "volume", NULL, "ml")))),
            shiny::uiOutput(ns("mereResultat")),
            # La gamme se prepare dans l'onglet des dilutions, qui sait deja
            # tout faire.
            shiny::actionButton(ns("mereVersDilution"),
              shiny::tagList(shiny::icon("share"),
                             " Utiliser cette solution mère dans « Solutions filles (dilutions) »"),
              class = "btn-primary"),
            shiny::helpText("La concentration calculée est reprise comme concentration",
                            " mère d'une gamme de dilutions, dans l'unité où elle a été obtenue.")))),

      shiny::tabPanel(
        shiny::tagList(shiny::icon("vials"), " Solutions filles (dilutions)"), value = "dilution",
        shiny::div(style = "padding-top:14px;"),

        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("bottle-droplet"), " La solution mère"),
            status = "primary", width = 5, solidHeader = TRUE,
            shiny::textInput(ns("dilProduit"), "Nom commercial du produit (facultatif)",
                             placeholder = "ex : Lambdacal P 212 EC"),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput(ns("dilNbMa"),
                "Nombre de matières actives", value = 1, min = 1, max = 10, step = 1)),
              shiny::column(6, shiny::selectInput(ns("dilUnite"),
                "Unité de concentration", choices = hstat_unites_choix("concentration"),
                selected = "g/l"))),
            shiny::numericInput(ns("dilDensite"),
              "Masse volumique de la solution mère (kg/l, facultative)",
              value = NULL, min = 0, step = 0.01),
            shiny::helpText("Une concentration mère par matière active : c'est ce qui",
                            " permet de détailler chaque fille et d'en donner la",
                            " concentration totale."),
            shiny::uiOutput(ns("dilMaUi")),
            shiny::helpText(shiny::strong("Masse volumique et masse molaire : facultatives."),
                            " Elles ramènent en g/l une concentration en masse par masse",
                            " (g/kg, % m/m) ou molaire (mol/l, mM) : c'est ce qui permet de",
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
                            "Concentration de la bouillie (g m.a./l)" = "conc_bouillie",
                            "Grammage à l'hectare (g m.a./ha)" = "grammage"),
                selected = "conc_bouillie")),
              shiny::column(5, shiny::div(style = "margin-top:26px;",
                shiny::actionButton(ns("dilImport"),
                  shiny::tagList(shiny::icon("file-import"), " Reprendre"),
                  class = "btn-default btn-block"))))),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("layer-group"), " La gamme à préparer"),
            status = "info", width = 4, solidHeader = TRUE,
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
                choices = hstat_unites_choix("volume"), selected = "ml"))),
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
            shiny::helpText("Une gamme profonde descend vite : au 1/10 depuis 100 g/l,",
                            " la 5e fille vaut 0,001. Les valeurs sont données en",
                            " entier, sauf si vous cochez « Arrondir les résultats »",
                            " en haut de la page."),
            shiny::hr(),
            shiny::actionButton(ns("dilAjouter"),
              shiny::tagList(shiny::icon("plus"), " Ajouter ce produit"),
              class = "btn-primary btn-block"),
            shiny::helpText("Ajoutez autant de produits que nécessaire : chacun garde",
                            " son propre coefficient et son propre nombre de filles.")),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("square-root-variable"), " Formules employées"),
            status = "warning", width = 3, solidHeader = TRUE, collapsible = TRUE,
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
            # Le champ d'edition de la colonne « Matiere active » recoit la
            # liste des matieres actives en suggestions (`list`), a l'instant
            # ou DT le cree.
            shiny::uiOutput(ns("maListeUi")),
            shiny::tags$script(shiny::HTML(sprintf(
              "$(document).on('focusin', '#%s td input[type=text]', function() {
                 var td = $(this).closest('td')[0];
                 if (td && td.cellIndex === %d) this.setAttribute('list', '%s');
               });", ns("dilSaisie"), match("Matiere_active", HSTAT_DILUTION_COLS) - 1L,
              ns("maListe")))),
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

      # LE CONVERTISSEUR : une valeur, toutes ses ecritures.
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
                choices = hstat_unites_choix(c("masse", "volume")), selected = "l"))),
            shiny::numericInput(ns("convDensite"),
              "Masse volumique du produit (kg/l, facultative)", value = NULL, min = 0, step = 0.01),
            shiny::numericInput(ns("convMasseMol"),
              "Masse molaire de la matière active (g/mol, facultative)", value = NULL,
              min = 0, step = 0.1),
            shiny::helpText("Une masse ne devient un volume — une dose en kg/ha une dose en",
                            " l/ha, une concentration m/m une concentration m/v — qu'avec la",
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

# SERVEUR
mod_dosage_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {

    # La mise en forme d'un resultat suit le reglage « Arrondir les resultats » :
    # decoche (le defaut), la valeur est complete.
    .dec <- function() {
      if (!isTRUE(input$dosArrondir)) return(NULL)
      max(0L, min(HSTAT_DEC_MAX, as.integer(.hstat_num1(input$dosDecimales, 2))))
    }
    .fmt <- function(x) {
      dec <- .dec()
      if (is.null(dec)) hstat_fmt_complet(x) else hstat_fmt_nb(x, dec)
    }
    # LE FICHIER PORTE DES NOMBRES, PAS LEUR MISE EN FORME.
    .num_export <- function(d) {
      if (is.null(d)) return(NULL)
      dec <- .dec()
      if (!is.null(dec)) {
        num <- vapply(d, is.numeric, logical(1))
        d[num] <- lapply(d[num], round, dec)
      }
      d
    }
    ns <- session$ns

    # -- Message d'echec, jamais un tableau vide sans explication -----------
    bandeau <- function(motif) {
      if (is.null(motif) || !nzchar(motif)) return(NULL)
      shiny::div(class = "callout callout-warning", style = "padding:10px 14px;",
                 shiny::icon("triangle-exclamation"), " ", motif)
    }

    # -- Matieres actives saisies hors catalogue --------------------------- Un
    # nom tape dans un champ est propose dans tous les autres.
    ma_saisies <- hstat_ma_session(session)
    .ma_ids <- function() c("doseMatiere", paste0("dilMaNom", seq_len(10L)))
    shiny::observe({
      v <- unlist(lapply(.ma_ids(), function(id) input[[id]]), use.names = FALSE)
      .ma_retenir(v)
    })
    .ma_retenir <- function(v) {
      neuf <- hstat_ma_retenir(v, shiny::isolate(ma_saisies()))
      if (length(neuf)) ma_saisies(c(shiny::isolate(ma_saisies()), neuf))
    }
    # La liste de suggestions de la colonne « Matiere active » du tableau des
    # produits.
    output$maListeUi <- shiny::renderUI(
      shiny::tags$datalist(id = ns("maListe"),
        lapply(hstat_ma_choix(ma_saisies()),
               function(x) shiny::tags$option(value = x))))
    shiny::outputOptions(output, "maListeUi", suspendWhenHidden = FALSE)
    # Les choix des champs deja affiches suivent ; chacun garde sa valeur.
    shiny::observeEvent(list(ma_saisies(), is.null(input$doseMatiere)), {
      choix <- hstat_ma_choix(ma_saisies())
      for (id in .ma_ids()) {
        cur <- shiny::isolate(input[[id]])
        if (is.null(cur)) next
        shiny::updateSelectizeInput(session, id, choices = c("", choix),
                                    selected = cur)
      }
    })
    # Poser une valeur (reprise depuis un autre onglet) : elle entre d'abord
    # dans les choix, sans quoi selectize refuserait une valeur inconnue.
    .ma_poser <- function(id, valeur) {
      valeur <- trimws(as.character(valeur %||% ""))[1]
      if (is.na(valeur)) valeur <- ""
      choix <- hstat_ma_choix(c(ma_saisies(), valeur))
      shiny::updateSelectizeInput(session, id, choices = c("", choix), selected = valeur)
    }

    # dose
    dose_res <- shiny::reactive({
      sens <- input$doseSens %||% "dose"
      hstat_dose_bilan(
        sens            = sens,
        valeur          = if (identical(sens, "dose")) input$doseValeur else input$doseGrammage,
        unite_valeur    = if (identical(sens, "dose")) (input$doseUnite %||% "ml/ha")
                          else (input$doseGrammageUnite %||% "g/ha"),
        concentration   = input$doseConc,
        unite_concentration = input$doseConcUnite %||% "g/l",
        volume_bouillie = input$doseBouillie,
        superficie      = input$doseSurface,
        volume_cuve     = input$doseCuve,
        produit         = input$doseProduit %||% "",
        matiere         = input$doseMatiere %||% "",
        densite         = input$doseDensite,
        masse_molaire   = input$doseMasseMol,
        unite_bouillie  = input$doseBouillieUnite %||% "l/ha",
        unite_superficie = input$doseSurfaceUnite %||% "ha",
        unite_cuve      = input$doseCuveUnite %||% "l",
        sortie_dose     = input$doseSortieDose %||% "",
        sortie_grammage = input$doseSortieGrammage %||% "")
    })

    output$doseMessage <- shiny::renderUI(bandeau(attr(dose_res(), "message")))

    # Le tableau affiche porte les valeurs MISES EN FORME : une dose de
    # 4.166666666667 mL/L ne se verse pas.
    dose_affiche <- shiny::reactive({
      d <- dose_res()
      if (!nrow(d)) return(NULL)
      data.frame(Grandeur = d$Grandeur,
                 Valeur   = .fmt(d$Valeur),
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

    hstat_export_table_handlers(output, "doseDl", function() {
                                  d <- dose_res()
                                  if (!nrow(d)) return(NULL)
                                  .num_export(data.frame(Grandeur = d$Grandeur, Valeur = d$Valeur, Formule = d$Formule,
                                                         stringsAsFactors = FALSE))
                                },
                                "doses_a_l_hectare")

    # convertisseur Les
    # categories du selecteur, ramenees a celles du catalogue.
    .conv_cats <- function(c) switch(c %||% "quantite",
      quantite = c("masse", "volume"), surface = "surface", dose = "dose",
      concentration = "concentration", c("masse", "volume"))
    shiny::observeEvent(input$convCategorie, {
      cats <- .conv_cats(input$convCategorie)
      dispo <- .hstat_unites_de(cats)$unite
      defaut <- switch(input$convCategorie %||% "quantite", quantite = "l",
                       surface = "ha", dose = "l/ha", concentration = "g/l", dispo[1])
      shiny::updateSelectInput(session, "convUnite", choices = hstat_unites_choix(cats),
        selected = hstat_garder_choix(shiny::isolate(input$convUnite), dispo, defaut))
    }, ignoreInit = TRUE)

    conv_res <- shiny::reactive({
      hstat_unite_table(input$convValeur, input$convUnite %||% "l",
                        input$convDensite, input$convMasseMol)
    })
    output$convMessage <- shiny::renderUI(bandeau(attr(conv_res(), "message")))
    conv_affiche <- shiny::reactive({
      d <- conv_res()
      if (!nrow(d)) return(NULL)
      data.frame(Groupe = d$Groupe, Unite = d$Unite,
                 Valeur = .fmt(d$Valeur), Conversion = d$Conversion,
                 stringsAsFactors = FALSE)
    })
    output$convTable <- DT::renderDT({
      d <- conv_affiche()
      shiny::validate(shiny::need(!is.null(d), tr("Saisissez une valeur et son unité.")))
      DT::datatable(d, rownames = FALSE,
                    colnames = c(tr("Famille"), tr("Unité"), tr("Valeur"), tr("Conversion")),
                    options = list(dom = "t", pageLength = 100, ordering = FALSE))
    })
    hstat_export_table_handlers(output, "convDl", function() {
                                  d <- conv_res()
                                  if (!nrow(d)) return(NULL)
                                  .num_export(data.frame(Groupe = d$Groupe, Unite = d$Unite, Valeur = d$Valeur, Conversion = d$Conversion,
                                                         stringsAsFactors = FALSE))
                                },
                                "conversions_d_unites")

    # dilution
    # Le tableau part VIDE : les produits y sont deposes par le formulaire.
    saisie <- shiny::reactiveVal(hstat_dilution_table_vide(0))

    # -- Une paire de champs par matiere active.
    dil_molaire <- shiny::reactiveVal(FALSE)
    shiny::observe({
      b <- hstat_unite_info(input$dilUnite %||% "g/l")
      v <- !is.null(b) && identical(b$base, "mol/l")
      if (!identical(v, shiny::isolate(dil_molaire()))) dil_molaire(v)
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
          shiny::column(if (mol) 4 else 5, hstat_ma_input(ns(paste0("dilMaNom", i)),
            if (i == 1L) "Matière active" else NULL, selected = nom,
            saisies = shiny::isolate(ma_saisies()),
            placeholder = if (i == 1L) "ex : Lambda-cyhalothrin" else NULL)),
          shiny::column(if (mol) 3 else 4, shiny::numericInput(ns(paste0("dilMaConc", i)),
            if (i == 1L) "Concentration mère" else NULL,
            value = if (is.null(cnc)) NA_real_ else cnc, min = 0, step = 1)),
          shiny::column(if (mol) 2 else 3, shiny::numericInput(ns(paste0("dilMaDep", i)),
            if (i == 1L) "1re fille (option.)" else NULL,
            value = if (is.null(dep)) NA_real_ else dep, min = 0, step = 1)),
          if (mol) shiny::column(3, shiny::numericInput(ns(paste0("dilMaMol", i)),
            if (i == 1L) "Masse molaire (g/mol)" else NULL,
            value = if (is.null(mm)) NA_real_ else mm, min = 0, step = 0.1)))
      })
    })
    # Rendu meme quand l'onglet est cache.
    shiny::outputOptions(output, "dilMaUi", suspendWhenHidden = FALSE)

    # -- Reprise du calcul de dose ----------------------------------------
    # L'utilisateur a deja saisi son produit dans l'onglet precedent.
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
                                   input$doseBouillieUnite %||% "l/ha", "l/ha")[1]
      val <- switch(src,
        conc_produit  = .hstat_num1(input$doseConc, NA_real_),
        grammage      = attr(d, "grammage"),
        conc_bouillie = if (isTRUE(is.finite(vb)) && vb > 0)
                          attr(d, "grammage") / vb else NA_real_)
      unite <- if (identical(src, "conc_produit")) (input$doseConcUnite %||% "g/l") else "g/l"
      if (!isTRUE(is.finite(val))) {
        shiny::showNotification(
          tr("Valeur non calculable : renseignez le volume de bouillie dans l'onglet « Dose et grammage à l'hectare »."),
          type = "warning", duration = 8)
        return()
      }
      shiny::updateTextInput(session, "dilProduit", value = attr(d, "produit"))
      shiny::updateSelectInput(session, "dilUnite", selected = unite)
      shiny::updateNumericInput(session, "dilNbMa", value = 1)
      .ma_poser("dilMaNom1", attr(d, "matiere"))
      shiny::updateNumericInput(session, "dilMaConc1", value = val)
      # Le grammage est une quantite A L'HECTARE, pas une concentration.
      shiny::showNotification(
        if (identical(src, "grammage"))
          trf("Grammage repris comme concentration mère : %s. Vérifiez l'unité — un grammage s'exprime par hectare, une concentration par litre.",
              .fmt(val))
        else trf("Valeur reprise : %s %s.", .fmt(val), unite),
        type = "message", duration = 8)
    })

    # -- Ajout du produit au tableau ---------------------------------------
    shiny::observeEvent(input$dilAjouter, {
      nom <- hstat_texte_client(input$dilProduit)
      n <- max(1L, min(10L, as.integer(.hstat_num1(input$dilNbMa, 1))))
      mas <- vapply(seq_len(n), function(i)
        trimws(input[[paste0("dilMaNom", i)]] %||% ""), character(1))
      # Le nom est facultatif : sans lui, la solution prend celui de ses
      # matieres actives, et un nom neuf ne remplace pas un produit deja saisi.
      if (!nzchar(nom)) {
        noms_pris <- trimws(as.character(saisie()$Produit))
        nom <- hstat_dilution_nom_defaut(mas, noms_pris)
      }
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
        Unite                = rep(input$dilUnite %||% "g/l", n),
        Coefficient          = rep(.hstat_num1(input$dilCoef, NA_real_), n),
        Nb_filles            = rep(.hstat_num1(input$dilNb, 1), n),
        Volume_final         = rep(.hstat_num1(input$dilVf, NA_real_), n),
        Unite_volume         = rep(input$dilVfUnite %||% "ml", n),
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
      i <- hstat_indice_client(info$row, nrow(d))
      j <- hstat_indice_client(info$col + 1L, ncol(d))   # `rownames = FALSE` decale d'un
      if (is.na(i) || is.na(j) || length(info$value) != 1L) return()
      v <- info$value
      if (identical(names(d)[j], "Matiere_active")) .ma_retenir(v)
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

    # La conversion vient AVANT la mise en forme.
    dil_brut <- shiny::reactive({
      d <- dilution()
      if (!is.data.frame(d) || !nrow(d)) return(NULL)
      hstat_dilution_convertir(d, input$dilConvVol %||% "",
                               input$dilConvConc %||% "")
    })
    dil_forme <- shiny::reactive({
      d <- dil_brut()
      if (is.null(d)) return(NULL)
      num <- vapply(d, is.numeric, logical(1))
      d[num] <- lapply(d[num], .fmt)
      d
    })
    dil_affiche <- shiny::reactive({
      d <- dil_forme()
      if (is.null(d)) NULL else hstat_dilution_affichage(d)
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
      d <- .num_export(dil_brut())
      if (is.null(d)) return(NULL)
      c(hstat_dilution_feuilles(d),
        list("Formules" = data.frame(Formule = attr(dilution(), "formules"),
                                     stringsAsFactors = FALSE)))
    }
    output$dilDlCsv  <- hstat_csv_handler(dil_tables, "solutions_filles")
    output$dilDlXlsx <- hstat_classeur_handler(dil_tables, "solutions_filles")

    # -- La solution mere ------------------------------------------------------
    # Un nom de test inconnu retombe sur l'application topique plutot que de
    # lever.
    mere_test <- shiny::reactive({
      t <- input$mereTest %||% "topique"
      if (length(t) && t[1] %in% names(HSTAT_MERE_TESTS)) t[1] else "topique"
    })

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
    # formule.
    .mere_val <- function(id, vers) {
      v <- .hstat_num1(input[[id]], NA_real_)
      u <- input[[paste0(id, "Unite")]] %||% vers
      hstat_unite_convertir(v, u, vers)[1]
    }

    mere_calc <- shiny::reactive({
      switch(mere_test(),
        topique = hstat_mere_topique(
          D = .mere_val("mereTopD", "µg/g"), x = .mere_val("mereTopX", "g"),
          V = .mere_val("mereTopV", "µl"),
          a = .mere_val("mereTopA", "µl"), P = input$mereTopP),
        disque = hstat_mere_concentration(
          C = .mere_val("mereDisC", "mg/ml"), V = .mere_val("mereDisV", "ml"),
          produit = input$mereProduit %||% "technique",
          P = input$mereDisP, teneur = .mere_val("mereDisTeneur", "g/l"),
          densite = input$mereDisDensite),
        tube = hstat_mere_tube(C = .mere_val("mereTubC", "mg/l"),
                               v = .mere_val("mereTubVflacon", "ml"),
                               V = .mere_val("mereTubV", "ml"),
                               P = input$mereTubP,
                               Vprep = .mere_val("mereTubVprep", "ml")))
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
                      trf("%s µg (%s mg)", .fmt(r$Q_ug), .fmt(r$Q_mg)), FM[["topique"]]),
          .mere_ligne(tr("Concentration de la solution mère"),
                      trf("%s µg m.a./µl", .fmt(r$c_mere)), FM[["topique_c"]]),
          .mere_ligne(tr("Quantité déposée sur chaque insecte"),
                      paste(.fmt(r$dose_individu), "µg"), "D × x")),
        disque = c(
          # La matiere active pure (C × V) n'est pas une ligne : on ne la pese
          # pas, et elle se lisait comme la quantite a peser.
          if (!is.null(r$m_pese)) list(
            .mere_ligne(if (identical(r$produit, "technique")) tr("Grade technique")
                        else tr("Produit à peser"), paste(.fmt(r$m_pese), "mg"),
                        if (identical(r$produit, "technique")) FM[["technique"]] else FM[["solide_pct"]]),
            .mere_ligne(tr("Concentration réelle (contrôle)"),
                        paste(.fmt(r$c_reelle), "mg/ml"), FM[["controle"]])),
          if (!is.null(r$v_formulation)) list(
            .mere_ligne(tr("Volume de formulation à prélever"),
                        paste(.fmt(r$v_formulation), "ml"),
                        if (identical(r$produit, "liquide_gl")) FM[["liquide_gl"]] else FM[["liquide_pct"]])),
          list(.mere_ligne(tr("Concentration de la solution mère"),
                           trf("%s mg/ml, soit %s mg/l", .fmt(r$c_mere),
                               .fmt(r$niveau_haut)), "1 mg/ml = 1 000 mg/l"))),
        tube = list(
          .mere_ligne(tr("Matière active technique à peser"),
                      trf("%s mg pour %s %s", .fmt(r$Q_mg),
                          .fmt(.hstat_num1(input$mereTubV, NA_real_)),
                          input$mereTubVUnite %||% "l"), FM[["tube"]]),
          if (is.finite(r$Q_prep)) .mere_ligne(
            tr("Matière active technique à peser (volume converti)"),
            trf("%s mg pour %s %s", .fmt(r$Q_prep),
                .fmt(.hstat_num1(input$mereTubVprep, NA_real_)),
                input$mereTubVprepUnite %||% "ml"), FM[["tube_prep"]]),
          .mere_ligne(tr("Quantité de matière active par flacon"),
                      paste(.fmt(r$flacon_ug), "µg"), FM[["tube_flacon"]]),
          .mere_ligne(tr("Nombre de flacons à traiter"),
                      .fmt(r$flacons), "V ÷ v"),
          .mere_ligne(tr("Concentration de la solution mère"),
                      trf("%s mg/l, soit %s mg/ml", .fmt(r$c_mgl), .fmt(r$c_mere)),
                      FM[["tube_c"]])))
      shiny::tags$table(class = "table table-condensed",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th(tr("Grandeur")),
          shiny::tags$th(tr("Valeur")), shiny::tags$th(tr("Formule")))),
        do.call(shiny::tags$tbody, lignes))
    })

    # LA CONCENTRATION DE LA MERE PART VERS LES DILUTIONS.
    shiny::observeEvent(input$mereVersDilution, {
      r <- mere_calc()
      if (!isTRUE(r$ok) || !isTRUE(is.finite(r$c_mere)) || r$c_mere <= 0) {
        shiny::showNotification(
          tr("La solution mère n'est pas calculée : complétez ses valeurs avant de la reprendre."),
          type = "warning", duration = 8)
        return()
      }
      # La concentration part dans l'unite ou elle a ete obtenue.
      cu <- hstat_mere_conc_unite(mere_test(), r,
        saisie = switch(mere_test(),
          disque = list(v = .hstat_num1(input$mereDisC, NA_real_), u = input$mereDisCUnite),
          tube   = list(v = .hstat_num1(input$mereTubC, NA_real_), u = input$mereTubCUnite),
          NULL))
      shiny::updateSelectInput(session, "dilUnite", selected = cu$unite)
      shiny::updateNumericInput(session, "dilNbMa", value = 1)
      shiny::updateNumericInput(session, "dilMaConc1", value = cu$valeur)
      shiny::updateTabsetPanel(session, "dosageTabs", selected = "dilution")
      shiny::showNotification(
        trf("Solution mère reprise dans « Solutions filles (dilutions) » : %s %s.",
            .fmt(cu$valeur), cu$unite),
        type = "message", duration = 8)
    })

  })
}