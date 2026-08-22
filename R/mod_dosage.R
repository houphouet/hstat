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

HSTAT_VOL_UNITES <- c("mL" = 0.001, "L" = 1)

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
  c_fille    = "Concentration fille = Concentration mère ÷ Coefficient de dilution",
  v_preleve  = "Volume à prélever = Volume final ÷ Coefficient de dilution",
  v_eau      = "Volume d'eau à ajouter = Volume final − Volume à prélever",
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

# Nombre lisible : deux decimales au plus, sans zeros inutiles. `format()`
# rendrait « 1.5e+05 » sur une grande surface, illisible sur une etiquette.
hstat_fmt_nb <- function(x, dec = 2) {
  v <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.finite(v),
                formatC(round(v, dec), format = "f", digits = dec, big.mark = " "),
                NA_character_)
  out <- sub("\\.?0+$", "", out)
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
#  Le coefficient de dilution et le volume final appartiennent au PRODUIT, pas
#  a la matiere active : deux lignes du meme produit qui les contrediraient
#  decriraient deux preparations differentes. On le refuse en nommant le
#  produit, plutot que de retenir l'une des deux valeurs au hasard.
# ---------------------------------------------------------------------------
HSTAT_DILUTION_COLS <- c("Produit", "Matiere_active", "Concentration_mere",
                         "Unite", "Coefficient", "Volume_final", "Unite_volume")

hstat_dilution_table_vide <- function(n = 3) {
  data.frame(
    Produit            = rep("", n),
    Matiere_active     = rep("", n),
    Concentration_mere = rep(NA_real_, n),
    Unite              = rep("g/L", n),
    Coefficient        = rep(NA_real_, n),
    Volume_final       = rep(NA_real_, n),
    Unite_volume       = rep("L", n),
    stringsAsFactors   = FALSE)
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

  # Coefficient et volume final appartiennent au produit : deux valeurs
  # differentes sous le meme nom decrivent deux preparations.
  incoherent <- character(0)
  for (p in unique(d$Produit)) {
    s <- d[d$Produit == p, , drop = FALSE]
    if (length(unique(s$Coefficient)) > 1 || length(unique(s$Volume_final)) > 1 ||
        length(unique(s$Unite_volume)) > 1)
      incoherent <- c(incoherent, p)
  }
  if (length(incoherent))
    return(vide(trf("Coefficient de dilution ou volume final contradictoire pour : %s. Ces deux valeurs décrivent la préparation : elles sont les mêmes pour toutes les matières actives d'un produit.",
                    paste(incoherent, collapse = ", "))))

  fac <- vapply(d$Unite, function(u) .hstat_dose_facteur(u, HSTAT_CONC_UNITES),
                numeric(1), USE.NAMES = FALSE)
  if (any(!is.finite(fac)))
    return(vide(trf("Unité de concentration inconnue : %s. Choisissez parmi %s.",
                    paste(unique(d$Unite[!is.finite(fac)]), collapse = ", "),
                    paste(names(HSTAT_CONC_UNITES), collapse = ", "))))

  d$Concentration_fille <- d$Concentration_mere / d$Coefficient
  # La concentration ramenee a g/L rend les matieres actives ADDITIONNABLES :
  # sommer des pour-cent et des mg/L donnerait un total qui ne veut rien dire.
  c_ref <- d$Concentration_fille * fac
  totaux <- tapply(c_ref, d$Produit, sum, na.rm = TRUE)

  d$Volume_a_prelever    <- d$Volume_final / d$Coefficient
  d$Volume_eau_a_ajouter <- d$Volume_final - d$Volume_a_prelever
  d$Concentration_totale_g_L <- unname(totaux[d$Produit])

  d <- d[order(d$Produit, d$Matiere_active), , drop = FALSE]
  # Le volume a prelever, l'eau a ajouter et la concentration totale
  # appartiennent au PRODUIT : les repeter sur chaque matiere active ferait
  # croire qu'il faut prelever autant de fois. Ils ne figurent donc que sur
  # la premiere ligne -- apres le tri, sans quoi ce ne serait pas la bonne.
  suivante <- duplicated(d$Produit)
  d$Volume_a_prelever[suivante]        <- NA_real_
  d$Volume_eau_a_ajouter[suivante]     <- NA_real_
  d$Concentration_totale_g_L[suivante] <- NA_real_

  res <- d[, c("Produit", "Matiere_active", "Concentration_mere", "Unite",
               "Coefficient", "Concentration_fille", "Volume_final",
               "Unite_volume", "Volume_a_prelever", "Volume_eau_a_ajouter",
               "Concentration_totale_g_L"), drop = FALSE]
  rownames(res) <- NULL
  attr(res, "formules") <- unname(
    HSTAT_DOSE_FORMULES[c("c_fille", "v_preleve", "v_eau", "c_totale")])
  res
}

# Les noms de colonne sont des IDENTIFIANTS -- le code y accede par
# `res$Volume_a_prelever`, et un nom accentue casserait cet acces sur une
# machine hors UTF-8. Ils sont donc relabellises a l'AFFICHAGE, ici et dans
# l'export : les deux, sinon le fichier telecharge garde l'orthographe que
# l'ecran vient de corriger.
HSTAT_DILUTION_LIBELLES <- c(
  Produit                  = "Produit",
  Matiere_active           = "Matière active",
  Concentration_mere       = "Concentration mère",
  Unite                    = "Unité",
  Coefficient              = "Coefficient de dilution",
  Concentration_fille      = "Concentration fille",
  Volume_final             = "Volume final",
  Unite_volume             = "Unité de volume",
  Volume_a_prelever        = "Volume à prélever",
  Volume_eau_a_ajouter     = "Eau à ajouter",
  Concentration_totale_g_L = "Concentration totale de la fille (g/L)")

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
            title = shiny::tagList(shiny::icon("table-list"), " Produits et solutions mères"),
            status = "primary", width = 8, solidHeader = TRUE,
            shiny::div(class = "callout callout-warning", style = "padding:8px 12px;",
              shiny::icon("lightbulb"),
              shiny::strong(" Une ligne par matière active. "),
              "Un produit qui en compte plusieurs occupe donc plusieurs lignes,",
              " sous le même nom commercial : le coefficient de dilution et le",
              " volume final y sont identiques, puisqu'ils décrivent la même",
              " préparation."),
            shiny::fluidRow(
              shiny::column(4, shiny::numericInput(ns("dilNlignes"),
                "Nombre de lignes", value = 3, min = 1, max = 60, step = 1)),
              shiny::column(8, shiny::div(style = "margin-top:26px;",
                shiny::actionButton(ns("dilCalculer"),
                  shiny::tagList(shiny::icon("calculator"), " Calculer les solutions filles"),
                  class = "btn-primary")))),
            shiny::br(),
            DT::DTOutput(ns("dilSaisie"))),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("square-root-variable"), " Formules employées"),
            status = "warning", width = 4, solidHeader = TRUE, collapsible = TRUE,
            .hstat_dose_bloc_formule(unname(HSTAT_DOSE_FORMULES[
              c("c_fille", "v_preleve", "v_eau", "c_totale")])),
            shiny::helpText("Le volume à prélever, l'eau à ajouter et la concentration",
                            " totale appartiennent au produit : ils ne figurent que sur",
                            " sa première ligne."))),

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
    saisie <- shiny::reactiveVal(hstat_dilution_table_vide(3))

    shiny::observeEvent(input$dilNlignes, {
      n <- max(1L, min(60L, as.integer(.hstat_num1(input$dilNlignes, 3))))
      d <- saisie()
      if (nrow(d) == n) return()
      d <- if (nrow(d) > n) d[seq_len(n), , drop = FALSE]
           else rbind(d, hstat_dilution_table_vide(n - nrow(d)))
      rownames(d) <- NULL
      saisie(d)
    }, ignoreInit = TRUE)

    output$dilSaisie <- DT::renderDT({
      DT::datatable(hstat_dilution_affichage(saisie()), rownames = FALSE,
                    editable = list(target = "cell"), selection = "none",
                    options = list(dom = "t", pageLength = 60, ordering = FALSE,
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

    output$dilMessage <- shiny::renderUI(bandeau(attr(dilution(), "message")))

    dil_affiche <- shiny::reactive({
      d <- dilution()
      if (!is.data.frame(d) || !nrow(d)) return(NULL)
      num <- vapply(d, is.numeric, logical(1))
      d[num] <- lapply(d[num], function(x) hstat_fmt_nb(x, 4))
      hstat_dilution_affichage(d)
    })

    output$dilTable <- DT::renderDT({
      d <- dil_affiche()
      shiny::validate(shiny::need(!is.null(d),
        tr("Remplissez le tableau, puis cliquez sur « Calculer les solutions filles ».")))
      DT::datatable(d, rownames = FALSE,
                    options = list(dom = "t", pageLength = 60, ordering = FALSE,
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
