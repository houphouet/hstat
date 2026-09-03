# =============================================================================
#  Module Shiny : GAIN DE RENDEMENT
# -----------------------------------------------------------------------------
#  Rendement (kg/ha) = masse recoltee (kg) / surface (ha)
#  Gain (%)          = (rendement du traitement - rendement du non traite)
#                      / rendement du non traite x 100
#
#  Le calcul lui-meme vit dans `Utils.R` (`hstat_rendement()`,
#  `hstat_rdt_table()`, `hstat_rdt_gain()`, `hstat_rdt_complet()`) : c'est la
#  seule facon de le tester sans demarrer Shiny, et c'est la regle du depot.
#  Ce fichier ne fait que choisir, afficher et mettre en forme.
#
#  DEUX RENDEMENTS, DEUX GAINS, ET CE N'EST PAS UNE REDONDANCE. Le rendement
#  GLOBAL rapporte la masse totale a la surface totale : il pondere chaque
#  repetition par sa surface. Le rendement MOYEN traite chaque repetition a
#  egalite. Ils coincident a surfaces egales et divergent sinon ; le module
#  affiche les deux plutot que d'en choisir un a la place de l'utilisateur, et
#  signale la divergence quand elle apparait.
# =============================================================================

# Mesures representables, et ce qu'elles portent en ordonnee.
HSTAT_RDT_MESURES <- c(
  "Rendement moyen par modalité" = "Rendement_moyen",
  "Rendement global par modalité" = "Rendement_global",
  "Rendement cumulé des répétitions" = "Rendement_somme",
  "Gain de rendement global (%)" = "Gain_global",
  "Gain de rendement moyen (%)" = "Gain_moyen",
  "Gain de rendement cumulé (%)" = "Gain_somme")

# Une mesure de GAIN se lit par rapport a zero : la ligne de reference n'est
# pas une decoration, c'est l'axe de lecture.
.hstat_rdt_est_gain <- function(mesure) grepl("^Gain_", mesure %||% "")

mod_yield_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    .hstat_scope_banner(),

    shiny::fluidRow(
      # ---------------------------------------------------------------- config
      shinydashboard::box(
        title = shiny::tagList(shiny::icon("wheat-awn"), " Configuration de l'analyse"),
        status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,

        # LA RECOLTE SE PESE AVANT D'ETRE MISE EN FICHIER. Un essai de dix
        # traitements sur trois blocs tient en trente lignes : exiger un CSV
        # pour trente nombres oblige a ouvrir un tableur, a le nommer, a le
        # relire -- pour un calcul de trois minutes. La saisie directe est
        # celle de l'onglet DL50/CL50, et pour la meme raison.
        .hstat_opt_section(
          "Source des données", "database", "#2c3e50", "#eceff1",
          shiny::radioButtons(ns("yieldSource"), NULL,
                       choices = c("Jeu de données chargé" = "fichier",
                                   "Saisie manuelle de la récolte" = "saisie"),
                       selected = "fichier"),
          shiny::conditionalPanel(
            ns = ns, condition = "input.yieldSource == 'saisie'",
            shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                       "Le tableau se remplit dans l'onglet « Saisie ». Le fichier chargé, lui, reste intact.")),
          shiny::uiOutput(ns("yieldSourceNote"))
        ),

        .hstat_opt_section(
          "Colonnes de la récolte", "table-columns", "#2980b9", "#eaf3fa",
          shiny::uiOutput(ns("yieldModaliteUI")),
          shiny::uiOutput(ns("yieldMasseUI")),
          shiny::uiOutput(ns("yieldSurfaceUI")),
          shiny::uiOutput(ns("yieldRepetitionUI")),
          shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                     "La répétition est facultative : elle ne change aucun chiffre, elle documente le plan.")
        ),

        .hstat_opt_section(
          "Unités saisies", "ruler", "#8e44ad", "#f7f0fb",
          shiny::selectInput(ns("yieldUniteMasse"), "Unité de la masse récoltée",
                      choices = names(HSTAT_RDT_MASSE), selected = "kilogramme (kg)"),
          shiny::selectInput(ns("yieldUniteSurface"), "Unité de la surface",
                      choices = names(HSTAT_RDT_SURFACE), selected = "hectare (ha)"),
          shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                     "Le calcul part de n'importe quelle unité : elle est ramenée au kilogramme et au mètre carré avant d'être exprimée dans l'unité voulue.")
        ),

        .hstat_opt_section(
          "Unité du rendement", "arrow-right-arrow-left", "#16a085", "#e8f8f4",
          shiny::selectInput(ns("yieldSortieMasse"), "Exprimer la masse en",
                      choices = names(HSTAT_RDT_MASSE), selected = "kilogramme (kg)"),
          shiny::selectInput(ns("yieldSortieSurface"), "Par unité de surface",
                      choices = names(HSTAT_RDT_SURFACE), selected = "hectare (ha)"),
          shiny::uiOutput(ns("yieldUniteApercu")),
          # LA CONVERSION AJOUTE, ELLE NE REMPLACE PAS. C'est une OPTION : le
          # tableau garde l'unite demandee ci-dessus, et gagne une colonne par
          # mesure convertie. L'une sert a comparer avec un barème ou une
          # publication, l'autre au contrôle -- et l'une sans l'autre oblige à
          # refaire le calcul de tête.
          shiny::checkboxInput(ns("yieldConvertir"),
                        "Convertir aussi le rendement dans une autre unité",
                        value = FALSE),
          shiny::conditionalPanel(
            ns = ns, condition = "input.yieldConvertir == true",
            shiny::selectInput(ns("yieldConvMasse"), "Convertir la masse en",
                        choices = names(HSTAT_RDT_MASSE), selected = "tonne (1000 kg)"),
            shiny::selectInput(ns("yieldConvSurface"), "Par unité de surface",
                        choices = names(HSTAT_RDT_SURFACE), selected = "hectare (ha)"),
            shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                       "Les deux valeurs figurent dans les résultats : la colonne convertie se pose juste après son originale."))
        ),

        .hstat_opt_section(
          "Programme non traité", "seedling", "#c0392b", "#fdeeec",
          shiny::uiOutput(ns("yieldTemoinUI")),
          shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                     "C'est la référence du gain : toutes les autres modalités lui sont comparées.")
        ),

        .hstat_opt_section(
          "Affichage des résultats", "hashtag", "#d35400", "#fdf2e9",
          shiny::checkboxInput(ns("yieldRound"), "Arrondir les résultats numériques", value = TRUE),
          shiny::conditionalPanel(
            ns = ns, condition = "input.yieldRound == true",
            shiny::numericInput(ns("yieldDecimals"), "Nombre de décimales :",
                         value = 2, min = 0, max = 8, step = 1)),
          shiny::helpText(style = "font-size:11px;color:#7f8c8d;",
                   "Si décoché, les valeurs s'affichent sans arrondi.")
        )
      ),

      # -------------------------------------------------------------- resultats
      shinydashboard::box(
        title = shiny::tagList(shiny::icon("chart-column"), " Rendements, gains et graphiques"),
        status = "primary", width = 8, solidHeader = TRUE,

        shiny::uiOutput(ns("yieldMessage")),

        shiny::tabsetPanel(
          id = ns("yieldTabs"), type = "tabs",

          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("table"), " Rendements"),
            value = "rendements",
            shiny::br(),
            shiny::uiOutput(ns("yieldResume")),
            DT::DTOutput(ns("yieldTableRdt")),
            shiny::br(),
            shiny::uiOutput(ns("yieldExportTable"))
          ),

          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("percent"), " Gains"),
            value = "gains",
            shiny::br(),
            shiny::uiOutput(ns("yieldGainNote")),
            DT::DTOutput(ns("yieldTableGain"))
          ),

          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("chart-simple"), " Graphique"),
            value = "graphique",
            shiny::br(),
            shiny::selectInput(ns("yieldMesure"), "Mesure représentée",
                        choices = HSTAT_RDT_MESURES, selected = "Rendement_moyen",
                        width = "100%"),
            shiny::div(style = "background:#fff;padding:16px;border-radius:8px;box-shadow:0 2px 8px rgba(0,0,0,.08);",
                shiny::plotOutput(ns("yieldPlot"), height = "620px")),
            shiny::br(),
            shiny::uiOutput(ns("yieldPlotNote")),
            shiny::uiOutput(ns("yieldExportPlot"))
          ),

          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("keyboard"), " Saisie"),
            value = "saisie",
            shiny::br(),
            # UN SEUL ARGUMENT, PAS TROIS. Des enfants texte adjacents ne font
            # qu'un seul noeud dans le DOM : le traducteur du navigateur, qui ne
            # remplace que des correspondances entieres, ne verrait aucun des
            # trois morceaux.
            shiny::helpText("Double-cliquez une cellule pour la modifier. La modalité est du texte, la masse et la surface des nombres ; la répétition est facultative."),
            DT::DTOutput(ns("yieldSaisie")),
            shiny::br(),
            shiny::actionButton(ns("yieldLigne"),
              shiny::tagList(shiny::icon("plus"), " Ligne"), class = "btn-sm"),
            shiny::actionButton(ns("yieldVider"),
              shiny::tagList(shiny::icon("eraser"), " Vider"), class = "btn-sm"),
            shiny::actionButton(ns("yieldSaisieGo"),
              shiny::tagList(shiny::icon("play"), " Analyser cette saisie"),
              class = "btn-primary btn-sm"),
            shiny::hr(),
            .hstat_opt_section("Coller depuis un tableur", "clipboard", "#16a085", "#e8f6f3",
              shiny::helpText("Copiez trois colonnes — modalité, masse récoltée, surface — et collez-les ici ; une quatrième colonne est lue comme la répétition. La ligne d'en-tête est reconnue et ignorée, et la virgule décimale d'un tableur français aussi."),
              shiny::textAreaInput(ns("yieldCollage"), NULL, rows = 5,
                placeholder = "T0\t12,5\t0,25\nT1\t18,3\t0,25\nT2\t17,1\t0,25",
                width = "100%"),
              shiny::actionButton(ns("yieldCollerGo"),
                shiny::tagList(shiny::icon("paste"), " Remplir le tableau"),
                class = "btn-primary"),
              shiny::actionButton(ns("yieldCollerAjout"),
                shiny::tagList(shiny::icon("plus"), " Ajouter à la suite"),
                class = "btn-sm"))
          ),

          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("circle-question"), " Méthode"),
            value = "aide",
            shiny::br(),
            shiny::htmlOutput(ns("yieldAide"))
          )
        )
      )
    ),

    # ------------------------------------------------------- options graphiques
    shiny::fluidRow(
      # Les options occupent une boite entiere sous la configuration, comme
      # celles du post-hoc : quarante reglages dans une colonne etroite ne se
      # parcourent pas, et le graphique doit rester visible pendant qu'on regle.
      shinydashboard::box(
        title = shiny::tagList(shiny::icon("sliders-h"), " Options du graphique"),
        status = "primary", width = 12, solidHeader = TRUE,
        collapsible = TRUE, collapsed = FALSE,
        shiny::div(style = "font-size:12px;color:#7f8c8d;margin-bottom:12px;",
            shiny::icon("info-circle"),
            " Toute la mise en forme du graphique des rendements, de la palette à l'export."),

        shiny::fluidRow(
          # ---- COL 1 : type, couleurs, geometrie ----
          shiny::column(3,
            .hstat_opt_section(
              "Type et couleurs", "palette", "#8e44ad", "#f7f0fb",
              shiny::radioButtons(ns("yieldPlotType"), "Type de graphique",
                           choices = c("Barres" = "bar", "Barres horizontales" = "barh",
                                       "Points + barres d'erreur" = "point",
                                       "Sucettes (lollipop)" = "lollipop"),
                           selected = "bar"),
              # LA PALETTE PAR DEFAUT DE ggplot2 EST OFFERTE, ET EN PREMIER.
              # C'est la seule qui ne PLAFONNE pas : les qualitatives de Brewer
              # s'arretent a 8, 9 ou 12 couleurs, et au-dela ggplot avertit puis
              # rend les modalites surnumeraires en GRIS. Un essai a quinze
              # traitements n'a rien d'exotique en agronomie.
              shiny::selectInput(ns("yieldPalette"), "Palette de couleurs",
                          choices = c(list("Sans palette" = c("Couleur unique" = "unique")),
                                      hstat_palettes_choix()),
                          selected = unname(HSTAT_PALETTE_GG)),
              shiny::conditionalPanel(
                ns = ns, condition = "input.yieldPalette == 'unique'",
                colourInput(ns("yieldCouleurUnique"), "Couleur", value = "#2E86C1")),
              shiny::selectInput(ns("yieldTheme"), "Thème du graphique",
                          choices = HSTAT_THEMES_GG, selected = "minimal")
            ),
            .hstat_opt_section(
              "Géométrie", "shapes", "#e67e22", "#fdf2e6",
              shiny::sliderInput(ns("yieldAlpha"), "Opacité",
                          min = 0.1, max = 1, value = 0.85, step = 0.05, ticks = FALSE),
              shiny::sliderInput(ns("yieldLargeur"), "Largeur des barres",
                          min = 0.1, max = 1, value = 0.7, step = 0.05, ticks = FALSE),
              shiny::sliderInput(ns("yieldPointSize"), "Taille des points",
                          min = 1, max = 12, value = 4, step = 0.5, ticks = FALSE),
              # `colour = NA` n'est PAS l'absence d'argument : il EFFACE le
              # contour. Il se demande donc explicitement.
              shiny::checkboxInput(ns("yieldContour"), "Contour des barres", value = TRUE),
              shiny::conditionalPanel(
                ns = ns, condition = "input.yieldContour == true",
                shiny::fluidRow(
                  shiny::column(6, colourInput(ns("yieldContourCouleur"), "Couleur", value = "#000000")),
                  shiny::column(6, shiny::sliderInput(ns("yieldContourEpaisseur"), "Épaisseur",
                                              min = 0.1, max = 3, value = 0.4, step = 0.1, ticks = FALSE))))
            )
          ),

          # ---- COL 2 : textes ----
          shiny::column(3,
            .hstat_opt_section(
              "Titres et libellés", "heading", "#2980b9", "#eaf3fa",
              shiny::textInput(ns("yieldTitre"), "Titre", placeholder = "Auto"),
              shiny::textInput(ns("yieldSousTitre"), "Sous-titre", placeholder = "Optionnel"),
              shiny::fluidRow(
                shiny::column(6, shiny::textInput(ns("yieldLabelX"), "Libellé X", placeholder = "Auto")),
                shiny::column(6, shiny::textInput(ns("yieldLabelY"), "Libellé Y", placeholder = "Auto"))),
              shiny::textInput(ns("yieldLegendeTitre"), "Titre de la légende", placeholder = "Auto"),
              shiny::selectInput(ns("yieldTitrePos"), "Position du titre",
                          choices = HSTAT_ALIGNEMENTS, selected = "0.5"),
              shiny::selectInput(ns("yieldSousTitrePos"), "Position du sous-titre",
                          choices = HSTAT_ALIGNEMENTS, selected = "0.5")
            ),
            .hstat_opt_section(
              "Styles d'écriture", "font", "#c0392b", "#fdeeec",
              shiny::selectInput(ns("yieldTitreStyle"), "Titre",
                          choices = HSTAT_FONT_STYLES, selected = "bold"),
              shiny::selectInput(ns("yieldSousTitreStyle"), "Sous-titre",
                          choices = HSTAT_FONT_STYLES, selected = "italic"),
              shiny::selectInput(ns("yieldAxisTitleStyle"), "Titres des axes",
                          choices = HSTAT_FONT_STYLES, selected = "plain"),
              shiny::selectInput(ns("yieldAxisTextXStyle"), "Graduations X",
                          choices = HSTAT_FONT_STYLES, selected = "plain"),
              shiny::selectInput(ns("yieldAxisTextYStyle"), "Graduations Y",
                          choices = HSTAT_FONT_STYLES, selected = "plain")
            )
          ),

          # ---- COL 3 : tailles et valeurs portees ----
          shiny::column(3,
            .hstat_opt_section(
              "Tailles", "text-height", "#d35400", "#fdf2e9",
              shiny::fluidRow(
                shiny::column(6, shiny::sliderInput(ns("yieldTitreSize"), "Titre",
                                            min = 8, max = 32, value = 16, step = 1, ticks = FALSE)),
                shiny::column(6, shiny::sliderInput(ns("yieldSousTitreSize"), "Sous-titre",
                                            min = 6, max = 28, value = 12, step = 1, ticks = FALSE))),
              shiny::fluidRow(
                shiny::column(6, shiny::sliderInput(ns("yieldAxisTitleSize"), "Titres des axes",
                                            min = 8, max = 28, value = 14, step = 1, ticks = FALSE)),
                shiny::column(6, shiny::sliderInput(ns("yieldAxisTextSize"), "Graduations",
                                            min = 6, max = 24, value = 12, step = 1, ticks = FALSE))),
              hstat_axe_titre_ui(ns, "yield")
            ),
            .hstat_opt_section(
              "Valeurs portées sur le graphique", "tag", "#27ae60", "#eafaf1",
              shiny::checkboxInput(ns("yieldValeurs"), "Afficher les valeurs", value = TRUE),
              shiny::conditionalPanel(
                ns = ns, condition = "input.yieldValeurs == true",
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput(ns("yieldValeursDec"), "Décimales",
                                               value = 1, min = 0, max = 6, step = 1)),
                  shiny::column(6, shiny::sliderInput(ns("yieldValeursSize"), "Taille",
                                              min = 2, max = 12, value = 4, step = 0.5, ticks = FALSE))),
                shiny::fluidRow(
                  shiny::column(6, colourInput(ns("yieldValeursCouleur"), "Couleur", value = "#2C3E50")),
                  shiny::column(6, shiny::selectInput(ns("yieldValeursStyle"), "Style",
                                              choices = HSTAT_FONT_STYLES, selected = "bold"))),
                # L'ETIQUETTE D'UNE VALEUR DEPEND DU SIGNE : un gain negatif
                # descend sous l'axe, et un `vjust` fige ecrirait son etiquette
                # du mauvais cote de la barre. `hstat_valeur_pos()` rend
                # l'ordonnee ET le calage ensemble.
                shiny::selectInput(ns("yieldValeursPos"), "Position",
                            choices = c("Au bout, à l'extérieur" = "dessus",
                                        "Au bout, à l'intérieur" = "dedans",
                                        "Au pied de la barre" = "pied"),
                            selected = "dessus"),
                shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                           "Un gain négatif descend sous l'axe : la position suit le signe.")
              )
            )
          ),

          # ---- COL 4 : axes, legende, export ----
          shiny::column(3,
            .hstat_opt_section(
              "Axes et ordre", "ruler-combined", "#16a085", "#e8f8f4",
              shiny::sliderInput(ns("yieldAngleX"), "Inclinaison des libellés X (°)",
                          min = 0, max = 90, value = 45, step = 5, ticks = FALSE),
              shiny::sliderInput(ns("yieldAngleY"), "Inclinaison des libellés Y (°)",
                          min = 0, max = 90, value = 0, step = 5, ticks = FALSE),
              shiny::selectInput(ns("yieldOrdre"), "Ordre des modalités",
                          choices = c("Alphabétique" = "alpha",
                                      "Valeur croissante" = "croissant",
                                      "Valeur décroissante" = "decroissant",
                                      "Non traité en premier" = "temoin"),
                          selected = "alpha"),
              shiny::checkboxInput(ns("yieldLimites"), "Personnaliser les limites Y", value = FALSE),
              shiny::conditionalPanel(
                ns = ns, condition = "input.yieldLimites == true",
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput(ns("yieldYMin"), "Y min", value = NA, step = 1)),
                  shiny::column(6, shiny::numericInput(ns("yieldYMax"), "Y max", value = NA, step = 1)))),
              shiny::numericInput(ns("yieldPasY"), "Pas des graduations Y (vide = auto)",
                           value = NA, min = 0, step = 1),
              shiny::fluidRow(
                shiny::column(6, shiny::checkboxInput(ns("yieldGrilleMaj"), "Grille principale", value = TRUE)),
                shiny::column(6, shiny::checkboxInput(ns("yieldGrilleMin"), "Grille secondaire", value = FALSE))),
              # UNE VALEUR NEGATIVE SE LIT PAR RAPPORT A ZERO. Sans le trait,
              # un gain negatif se confond avec un petit gain positif, et un
              # rendement negatif -- une correction, un solde -- ne se
              # distingue plus d'un rendement faible. Le trait n'est donc pas
              # reserve aux gains.
              shiny::checkboxInput(ns("yieldLigneZero"), "Ligne de référence à 0", value = TRUE),
              shiny::checkboxInput(ns("yieldErreurs"), "Barres d'erreur (rendement moyen)", value = TRUE),
              shiny::conditionalPanel(
                ns = ns, condition = "input.yieldErreurs == true",
                shiny::radioButtons(ns("yieldErreurType"), NULL,
                             choices = c("Erreur-type" = "se", "Écart-type" = "sd",
                                         "IC 95 %" = "ci"),
                             selected = "se", inline = TRUE))
            ),
            .hstat_opt_section(
              "Légende", "list", "#7f8c8d", "#f4f6f7",
              shiny::selectInput(ns("yieldLegendePos"), "Position",
                          choices = list("Droite" = "right", "Gauche" = "left",
                                         "En haut" = "top", "En bas" = "bottom",
                                         "Masquée" = "none"),
                          selected = "none"),
              shiny::fluidRow(
                shiny::column(6, shiny::sliderInput(ns("yieldLegendeTitreSize"), "Titre",
                                            min = 6, max = 24, value = 12, step = 1, ticks = FALSE)),
                shiny::column(6, shiny::sliderInput(ns("yieldLegendeTexteSize"), "Texte",
                                            min = 6, max = 20, value = 10, step = 1, ticks = FALSE)))
            ),
            .hstat_opt_section(
              "Fichier exporté", "download", "#2c3e50", "#eef1f4",
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("yieldExportW"), "Largeur (px)",
                                             value = 1600, min = 200, step = 50)),
                shiny::column(6, shiny::numericInput(ns("yieldExportH"), "Hauteur (px)",
                                             value = 1000, min = 200, step = 50))),
              hstat_format_input(ns("yieldExportFormat"), "Format", "png"),
              # Plancher a 300 DPI : en dessous, une figure est nette a l'ecran
              # et floue sur papier, et le defaut ne se voit qu'une fois le
              # document remis.
              hstat_dpi_input(ns("yieldExportDPI"), "Résolution (DPI)",
                              valeur = 300, min = 300, step = 100),
              shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                         "Les pixels fixent la mise en page (lue à 96 ppp) ; le DPI multiplie la finesse.")
            )
          )
        )
      )
    )
  )
}


mod_yield_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------ saisie
    saisie <- shiny::reactiveVal(hstat_rdt_saisie_vide())

    # LA TABLE N'EST CONSTRUITE QU'UNE FOIS, et mise a jour ensuite par son
    # proxy. Relire `saisie()` dans le rendu la reconstruirait a CHAQUE cellule
    # modifiee : la cellule en cours d'edition est alors detruite sous le
    # curseur. C'est l'idiome de l'onglet DL50/CL50, et le seul qui rende la
    # saisie utilisable -- d'ou le `isolate()`.
    output$yieldSaisie <- DT::renderDT({
      DT::datatable(shiny::isolate(saisie()), rownames = FALSE,
                    editable = list(target = "cell"), selection = "none",
                    colnames = c(tr("Modalité"), tr("Masse récoltée"),
                                 tr("Surface récoltée"), tr("Répétition")),
                    options = list(dom = "t", pageLength = 100, ordering = FALSE))
    })
    proxy_saisie <- DT::dataTableProxy("yieldSaisie")
    shiny::observeEvent(saisie(), {
      DT::replaceData(proxy_saisie, saisie(), resetPaging = FALSE, rownames = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$yieldSaisie_cell_edit, {
      info <- input$yieldSaisie_cell_edit
      d <- saisie()
      j <- info$col + 1L
      if (j < 1L || j > ncol(d)) return()
      # LA MODALITE ET LA REPETITION SONT DU TEXTE, la masse et la surface des
      # nombres. Passer tout au meme convertisseur ferait disparaitre le nom du
      # traitement des qu'il ne serait pas un nombre -- c'est-a-dire toujours.
      d[info$row, j] <- if (names(d)[j] %in% c("Masse", "Surface"))
        suppressWarnings(as.numeric(gsub(",", ".", info$value, fixed = TRUE)))
      else trimws(as.character(info$value))
      saisie(d)
    })

    shiny::observeEvent(input$yieldLigne, {
      d <- rbind(saisie(), hstat_rdt_saisie_vide(1L))
      rownames(d) <- NULL
      saisie(d)
    })
    shiny::observeEvent(input$yieldVider, saisie(hstat_rdt_saisie_vide()))

    # Le bouton ne calcule rien : il BASCULE LA SOURCE. Le calcul suit la
    # source, et l'utilisateur qui vient de remplir le tableau ne doit pas
    # avoir a remonter jusqu'au bouton radio pour voir son resultat.
    shiny::observeEvent(input$yieldSaisieGo, {
      shiny::updateRadioButtons(session, "yieldSource", selected = "saisie")
      shiny::updateTabsetPanel(session, "yieldTabs", selected = "rendements")
    })

    .yield_coller <- function(ajouter) {
      r <- hstat_rdt_coller(input$yieldCollage)
      if (!isTRUE(r$ok)) {
        shiny::showNotification(r$message, type = "warning", duration = 10)
        return()
      }
      tab <- r$table
      if (!"Repetition" %in% names(tab)) tab$Repetition <- NA_character_
      d <- if (isTRUE(ajouter)) {
        cur <- saisie()
        # Les lignes entierement vides du tableau de depart ne sont pas des
        # donnees : les garder devant le collage ferait un tableau a trous.
        cur <- cur[rowSums(!is.na(cur) & cur != "") > 0, , drop = FALSE]
        rbind(cur, tab[names(cur)])
      } else tab
      rownames(d) <- NULL
      saisie(d)
      shiny::updateTextAreaInput(session, "yieldCollage", value = "")
      shiny::updateRadioButtons(session, "yieldSource", selected = "saisie")
      shiny::showNotification(
        paste0(
          trf("%d ligne(s) collée(s) ; la virgule y a été lue comme %s.",
              r$lignes, if (identical(r$decimale, ","))
                tr("une décimale") else tr("un séparateur")),
          # L'EN-TETE ECARTE SE DIT. Sa reconnaissance est une heuristique :
          # taire la ligne sautee ferait disparaitre une observation en
          # silence si elle n'en etait pas un.
          if (isTRUE(r$entete))
            paste0(" ", tr("La première ligne a été lue comme un en-tête et écartée."))
          else ""),
        type = "message", duration = 7)
    }
    shiny::observeEvent(input$yieldCollerGo, .yield_coller(FALSE))
    shiny::observeEvent(input$yieldCollerAjout, .yield_coller(TRUE))

    saisie_utile <- shiny::reactive(hstat_rdt_saisie_propre(saisie()))

    # LA SOURCE EST UN CHOIX, PAS UNE DEVINETTE. Basculer tout seul sur la
    # saisie des qu'un fichier manque -- ou l'inverse -- ferait changer les
    # chiffres sans que personne ait rien demande.
    donnees <- shiny::reactive({
      if (identical(input$yieldSource %||% "fichier", "saisie"))
        return(saisie_utile())
      values$filteredData
    })

    output$yieldSourceNote <- shiny::renderUI({
      if (!identical(input$yieldSource %||% "fichier", "saisie")) return(NULL)
      n <- NROW(saisie_utile())
      shiny::div(class = if (n) "callout callout-info" else "callout callout-warning",
          style = "padding:8px 12px;margin:6px 0 0 0;font-size:13px;",
          shiny::icon("keyboard"), " ",
          if (n) trf("%d ligne(s) exploitable(s) dans le tableau de saisie.", n)
          else tr("Le tableau de saisie est vide : renseignez au moins la modalité, la masse et la surface."))
    })

    cols_num <- shiny::reactive({
      d <- donnees(); if (is.null(d)) return(character(0))
      names(d)[vapply(d, is.numeric, logical(1))]
    })
    cols_toutes <- shiny::reactive({
      d <- donnees(); if (is.null(d)) return(character(0))
      names(d)
    })

    # ---------------------------------------------------------------- selecteurs
    # LE TABLEAU DE SAISIE PORTE DES NOMS CONNUS : les proposer est gratuit, et
    # offrir la premiere colonne venue obligerait a re-choisir trois fois ce que
    # le module vient lui-meme de nommer.
    .prefere <- function(nom, dispo, repli) {
      if (nom %in% dispo) nom else repli
    }
    output$yieldModaliteUI <- shiny::renderUI({
      cn <- cols_toutes()
      shiny::selectInput(ns("yieldModalite"), "Variable des traitements (modalités)",
                  choices = cn, selected = .prefere("Modalite", cn, cn[1]))
    })
    output$yieldMasseUI <- shiny::renderUI({
      n <- cols_num()
      shiny::selectInput(ns("yieldMasse"), "Masse récoltée", choices = n,
                  selected = if (length(n)) .prefere("Masse", n, n[1]) else NULL)
    })
    output$yieldSurfaceUI <- shiny::renderUI({
      n <- cols_num()
      shiny::selectInput(ns("yieldSurface"), "Surface récoltée", choices = n,
                  selected = if (length(n) > 1) .prefere("Surface", n, n[2])
                             else if (length(n)) .prefere("Surface", n, n[1]) else NULL)
    })
    output$yieldRepetitionUI <- shiny::renderUI({
      cn <- cols_toutes()
      shiny::selectInput(ns("yieldRepetition"), "Répétition (facultatif)",
                  choices = c("(aucune)" = "", cn),
                  selected = .prefere("Repetition", cn, ""))
    })

    modalites <- shiny::reactive({
      d <- donnees(); v <- input$yieldModalite
      if (is.null(d) || is.null(v) || !nzchar(v) || !(v %in% names(d)))
        return(character(0))
      m <- trimws(as.character(d[[v]]))
      sort(unique(m[!is.na(m) & nzchar(m)]))
    })

    output$yieldTemoinUI <- shiny::renderUI({
      m <- modalites()
      # LE TEMOIN NE SE DEVINE PAS. Prendre la premiere modalite par ordre
      # alphabetique donne un gain a toutes les autres sans que personne ait
      # designe la reference : les chiffres sont alors faux ET vraisemblables.
      # L'utilisateur choisit, et le calcul refuse tant qu'il n'a pas choisi.
      shiny::selectInput(ns("yieldTemoin"), "Modalité non traitée (référence)",
                  choices = c("(à choisir)" = "", m), selected = "")
    })

    unite_rdt <- shiny::reactive({
      hstat_rdt_unite_libelle(input$yieldSortieMasse %||% "kilogramme (kg)",
                              input$yieldSortieSurface %||% "hectare (ha)")
    })

    unite_conv <- shiny::reactive({
      if (!isTRUE(input$yieldConvertir)) return(NULL)
      u <- hstat_rdt_unite_libelle(input$yieldConvMasse %||% "tonne (1000 kg)",
                                   input$yieldConvSurface %||% "hectare (ha)")
      if (identical(u, unite_rdt())) NULL else u
    })

    output$yieldUniteApercu <- shiny::renderUI({
      shiny::div(class = "callout callout-info",
          style = "padding:8px 12px;margin:6px 0 0 0;font-size:13px;",
          shiny::icon("equals"), " ",
          shiny::strong(trf("Rendement exprimé en %s", unite_rdt())),
          if (!is.null(unite_conv()))
            shiny::div(style = "margin-top:4px;",
                shiny::icon("arrow-right-arrow-left"), " ",
                trf("Converti aussi en %s", unite_conv())))
    })

    # ------------------------------------------------------------------ calcul
    resultat <- shiny::reactive({
      d <- donnees()
      shiny::req(d, input$yieldModalite, input$yieldMasse, input$yieldSurface)
      hstat_rdt_complet(
        d, input$yieldModalite, input$yieldMasse, input$yieldSurface,
        non_traite = input$yieldTemoin,
        unite_masse = input$yieldUniteMasse %||% "kilogramme (kg)",
        unite_surface = input$yieldUniteSurface %||% "hectare (ha)",
        sortie_masse = input$yieldSortieMasse %||% "kilogramme (kg)",
        sortie_surface = input$yieldSortieSurface %||% "hectare (ha)",
        var_repetition = if (nzchar(input$yieldRepetition %||% "")) input$yieldRepetition else NULL,
        conv_masse = if (isTRUE(input$yieldConvertir)) input$yieldConvMasse else NULL,
        conv_surface = if (isTRUE(input$yieldConvertir)) input$yieldConvSurface else NULL)
    })

    output$yieldMessage <- shiny::renderUI({
      r <- resultat()
      m <- attr(r, "message")
      if (is.null(m) || !nzchar(m)) return(NULL)
      alerte <- grepl("^Attention", m) || !NROW(r)
      shiny::div(class = if (alerte) "callout callout-warning" else "callout callout-success",
          style = "padding:10px 14px;font-size:13px;margin-bottom:12px;",
          shiny::icon(if (alerte) "triangle-exclamation" else "circle-check"), " ", m)
    })

    output$yieldGainNote <- shiny::renderUI({
      r <- resultat(); shiny::req(NROW(r))
      m <- attr(r, "message_gain")
      if (is.null(m) || !nzchar(m)) return(NULL)
      shiny::div(class = "callout callout-info",
          style = "padding:10px 14px;font-size:13px;margin-bottom:12px;",
          shiny::icon("circle-info"), " ", m)
    })

    output$yieldResume <- shiny::renderUI({
      r <- resultat(); shiny::req(NROW(r))
      carte <- function(titre, valeur, couleur) {
        shiny::column(4, shiny::div(
          style = sprintf("background:%s22;border-left:4px solid %s;border-radius:6px;padding:10px 14px;margin-bottom:12px;", couleur, couleur),
          shiny::div(style = "font-size:11px;text-transform:uppercase;letter-spacing:.4px;color:#7f8c8d;", titre),
          shiny::div(style = sprintf("font-size:20px;font-weight:700;color:%s;", couleur), valeur)))
      }
      dec <- hstat_dec_affichage(input$yieldRound, input$yieldDecimals)
      arr <- function(x) if (is.na(dec)) format(x, digits = 7) else format(round(x, dec), nsmall = min(dec, 6))
      # UN ZERO N'EST PAS UNE ABSENCE. `which.max` sur des gains tous nuls rend
      # bien une modalite ; c'est `all(is.na(...))` qui doit decider, jamais
      # une comparaison a zero.
      meilleur <- if (!any(is.finite(r$Gain_moyen))) NA else r$Modalite[which.max(r$Gain_moyen)]
      shiny::fluidRow(
        carte("Modalités comparées", nrow(r), "#2980b9"),
        carte(trf("Rendement global le plus élevé (%s)", unite_rdt()),
              if (all(is.na(r$Rendement_global))) "—" else arr(max(r$Rendement_global, na.rm = TRUE)),
              "#16a085"),
        carte("Meilleur gain moyen", if (is.na(meilleur)) "—" else meilleur, "#27ae60"),
        carte("Programme non traité",
              if (nzchar(input$yieldTemoin %||% "")) input$yieldTemoin else "(à choisir)",
              "#c0392b"))
    })

    # Tableau mis en forme : arrondi commande par l'utilisateur.
    table_affiche <- function(cols) {
      r <- resultat()
      if (!NROW(r)) return(NULL)
      cols <- intersect(cols, names(r))
      d <- r[, cols, drop = FALSE]
      round_numeric_df(d, input$yieldRound, input$yieldDecimals)
    }

    # Les colonnes converties suivent leurs originales : `intersect()` sur les
    # noms REELS du tableau garde cet ordre, la enumerer a la main le perdrait.
    avec_conv <- function(cols) {
      r <- resultat()
      if (!NROW(r)) return(cols)
      voulues <- unlist(lapply(cols, function(k) c(k, paste0(k, "_conv"))))
      c(intersect(names(r), voulues), intersect(names(r), c("Unite", "Unite_conv")))
    }

    output$yieldTableRdt <- DT::renderDT({
      d <- table_affiche(avec_conv(c("Modalite", "N", "Repetitions", "Masse_totale",
                           "Surface_totale", "Rendement_global", "Rendement_moyen",
                           "Rendement_somme", "Ecart_type", "Erreur_type")))
      shiny::req(d)
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })

    output$yieldTableGain <- DT::renderDT({
      d <- table_affiche(avec_conv(c("Modalite", "Rendement_global", "Gain_global",
                           "Rendement_moyen", "Gain_moyen",
                           "Rendement_somme", "Gain_somme")))
      shiny::req(d)
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })

    # ------------------------------------------------------------- graphique
    # L'ordre des modalites est un REGLAGE : trier par valeur fait ressortir le
    # classement, l'ordre alphabetique permet de retrouver une modalite, et
    # « non traite en premier » met la reference a gauche, la ou on la lit.
    # Les reglages sont LUS PAR LE REACTIF et passes ici : lus dans l'aide, la
    # dependance existe mais ne se voit pas a la lecture du reactif -- et c'est
    # exactement ce qu'un balayage ne peut pas verifier.
    ordonner <- function(d, mesure, ordre, nt) {
      niveaux <- switch(ordre,
        croissant   = d$Modalite[order(d[[mesure]], na.last = TRUE)],
        decroissant = d$Modalite[order(-d[[mesure]], na.last = TRUE)],
        temoin      = c(intersect(nt, d$Modalite), setdiff(sort(d$Modalite), nt)),
        sort(d$Modalite))
      d$Modalite <- factor(d$Modalite, levels = unique(niveaux))
      d
    }

    graphique <- shiny::reactive({
      r <- resultat()
      shiny::req(NROW(r))
      mesure <- input$yieldMesure %||% "Rendement_moyen"
      if (!(mesure %in% names(r))) return(NULL)
      d <- r[is.finite(r[[mesure]]), , drop = FALSE]
      if (!NROW(d)) return(NULL)
      d <- ordonner(d, mesure, input$yieldOrdre %||% "alpha", input$yieldTemoin %||% "")
      d$.val <- d[[mesure]]

      gain <- .hstat_rdt_est_gain(mesure)
      # La mesure tracee peut etre une colonne CONVERTIE : l'ordonnee doit
      # alors porter l'unite de conversion, sinon le graphique annonce des
      # kg/ha en affichant des t/ha.
      u_axe <- if (grepl("_conv$", mesure)) attr(r, "unite_conv") %||% unite_rdt()
               else unite_rdt()
      titre_auto <- names(HSTAT_RDT_MESURES)[match(mesure, HSTAT_RDT_MESURES)]
      if (is.na(titre_auto)) titre_auto <- mesure
      y_auto <- if (gain) "Gain (%)" else trf("Rendement (%s)", u_axe)

      alpha <- input$yieldAlpha %||% 0.85
      sty <- hstat_barre_style(alpha, isTRUE(input$yieldContour),
                              input$yieldContourCouleur %||% "#000000",
                              input$yieldContourEpaisseur %||% 0.4)
      larg <- input$yieldLargeur %||% 0.7
      pal  <- input$yieldPalette %||% "Set2"
      type <- input$yieldPlotType %||% "bar"

      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Modalite, y = .data$.val,
                                           fill = .data$Modalite,
                                           colour = .data$Modalite))
      if (type %in% c("bar", "barh")) {
        p <- p + do.call(ggplot2::geom_col, c(list(width = larg), sty))
      } else if (type == "lollipop") {
        p <- p + ggplot2::geom_segment(
              ggplot2::aes(xend = .data$Modalite, y = 0, yend = .data$.val),
              linewidth = max(0.2, larg), alpha = alpha) +
          ggplot2::geom_point(size = input$yieldPointSize %||% 4, alpha = alpha)
      } else {
        p <- p + ggplot2::geom_point(size = input$yieldPointSize %||% 4, alpha = alpha)
      }

      # BARRES D'ERREUR : elles n'ont de sens que sur le rendement MOYEN. Les
      # poser sur un rendement global ou un gain afficherait la dispersion
      # d'une autre grandeur que celle qui est tracee.
      if (isTRUE(input$yieldErreurs) && identical(mesure, "Rendement_moyen") &&
          "Erreur_type" %in% names(d)) {
        e <- switch(input$yieldErreurType %||% "se",
                    sd = d$Ecart_type, ci = 1.96 * d$Erreur_type, d$Erreur_type)
        if (any(is.finite(e))) {
          d$.lo <- d$.val - e; d$.hi <- d$.val + e
          p <- p + ggplot2::geom_errorbar(
            data = d, ggplot2::aes(ymin = .data$.lo, ymax = .data$.hi),
            width = 0.2, colour = "black", inherit.aes = TRUE)
        }
      }

      # LES VALEURS NEGATIVES SE REPRESENTENT COMME LES AUTRES, et il faut
      # deux choses pour qu'elles se LISENT : le trait de zero, et un axe qui
      # atteint zero. Sur un nuage de points dont toutes les valeurs sont
      # negatives, ggplot cadre sur les donnees : la base de comparaison sort
      # du champ et l'ampleur du recul devient invisible.
      # UNE VALEUR EXACTEMENT NULLE NE DESSINE RIEN. Une barre de hauteur zero
      # est invisible : le temoin, dont le gain vaut 0 PAR DEFINITION,
      # disparaissait du graphique de gain alors qu'il en est la reference.
      # La categorie restait sur l'axe, mais il n'y avait rien a voir -- ce
      # qui se lit « pas de resultat » et non « egal a la reference ».
      zeros <- d[is.finite(d$.val) & d$.val == 0, , drop = FALSE]
      if (NROW(zeros) && type %in% c("bar", "barh"))
        p <- p + ggplot2::geom_point(data = zeros, ggplot2::aes(y = 0),
                                     size = max(2, (input$yieldPointSize %||% 4) * 0.6),
                                     shape = 18, show.legend = FALSE)
      negatifs <- any(is.finite(d$.val) & d$.val < 0)
      if ((gain || negatifs) && isTRUE(input$yieldLigneZero))
        p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                     colour = "#7f8c8d", linewidth = 0.5)
      if (negatifs) p <- p + ggplot2::expand_limits(y = 0)

      if (isTRUE(input$yieldValeurs)) {
        dec <- max(0L, as.integer(input$yieldValeursDec %||% 1))
        pos <- hstat_valeur_pos(d$.val, input$yieldValeursPos %||% "dessus")
        d$.ylab <- pos$y; d$.vjust <- pos$vjust
        p <- p + ggplot2::geom_text(
          data = d,
          ggplot2::aes(y = .data$.ylab, label = format(round(.data$.val, dec), nsmall = dec)),
          vjust = d$.vjust, size = input$yieldValeursSize %||% 4,
          fontface = input$yieldValeursStyle %||% "bold",
          colour = input$yieldValeursCouleur %||% "#2C3E50",
          show.legend = FALSE)
      }

      if (identical(pal, "unique")) {
        u <- input$yieldCouleurUnique %||% "#2E86C1"
        p <- p + ggplot2::scale_fill_manual(values = rep(u, nrow(d))) +
          ggplot2::scale_colour_manual(values = rep(u, nrow(d)))
      } else {
        # Le choix des echelles vit dans `hstat_scales_palette()` : « ggplot2 »
        # se pose par `scale_*_hue()`, un nom de Brewer par `scale_*_brewer()`,
        # et un nom inconnu ne pose rien plutot que de faire avertir ggplot a
        # chaque trace.
        for (sc in hstat_scales_palette(pal)) p <- p + sc
      }

      lim <- if (isTRUE(input$yieldLimites))
        c(if (is.finite(input$yieldYMin %||% NA)) input$yieldYMin else NA,
          if (is.finite(input$yieldYMax %||% NA)) input$yieldYMax else NA) else c(NA, NA)
      pas <- input$yieldPasY
      if (isTRUE(is.finite(pas)) && pas > 0) {
        bornes <- hstat_etendue_axe(c(d$.val, lim))
        p <- p + ggplot2::scale_y_continuous(
          breaks = seq(hstat_pas_debut(bornes[1], pas), bornes[2] + pas, by = pas))
      }
      if (any(is.finite(lim))) p <- p + ggplot2::coord_cartesian(ylim = lim)
      if (identical(type, "barh")) p <- p + ggplot2::coord_flip()

      angle_x <- input$yieldAngleX %||% 45
      angle_y <- input$yieldAngleY %||% 0
      nt <- input$yieldTemoin %||% ""
      sous_auto <- if (gain && nzchar(nt))
        trf("Référence : %s (gain nul par définition)", nt) else NULL
      p + ggplot2::labs(
            title = if (nzchar(input$yieldTitre %||% "")) input$yieldTitre else titre_auto,
            subtitle = if (nzchar(input$yieldSousTitre %||% "")) input$yieldSousTitre else sous_auto,
            x = if (nzchar(input$yieldLabelX %||% "")) input$yieldLabelX else input$yieldModalite,
            y = if (nzchar(input$yieldLabelY %||% "")) input$yieldLabelY else y_auto,
            fill = if (nzchar(input$yieldLegendeTitre %||% "")) input$yieldLegendeTitre else input$yieldModalite,
            colour = if (nzchar(input$yieldLegendeTitre %||% "")) input$yieldLegendeTitre else input$yieldModalite) +
        viz_get_theme(input$yieldTheme %||% "minimal", base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = input$yieldTitreSize %||% 16,
            face = input$yieldTitreStyle %||% "bold",
            hjust = as.numeric(input$yieldTitrePos %||% "0.5")),
          # Le sous-titre s'affiche aussi quand il est AUTOMATIQUE (la
          # reference du gain) : le conditionner au seul champ de saisie
          # masquait la mention du temoin.
          plot.subtitle = ggplot2::element_text(
            size = input$yieldSousTitreSize %||% 12,
            face = input$yieldSousTitreStyle %||% "italic",
            hjust = as.numeric(input$yieldSousTitrePos %||% "0.5"),
            colour = "gray30"),
          axis.title.x = hstat_axe_titre_lire(input, "yield",
                                              input$yieldAxisTitleSize %||% 14,
                                              input$yieldAxisTitleStyle %||% "plain", "x"),
          axis.title.y = hstat_axe_titre_lire(input, "yield",
                                              input$yieldAxisTitleSize %||% 14,
                                              input$yieldAxisTitleStyle %||% "plain", "y"),
          # L'INCLINAISON COMMANDE AUSSI L'ALIGNEMENT : une etiquette penchee
          # doit finir SOUS sa graduation, une etiquette droite se centre.
          axis.text.x = ggplot2::element_text(
            angle = angle_x, hjust = if (angle_x > 0) 1 else 0.5,
            vjust = if (angle_x >= 90) 0.5 else 1,
            size = input$yieldAxisTextSize %||% 12,
            face = input$yieldAxisTextXStyle %||% "plain"),
          axis.text.y = ggplot2::element_text(
            angle = angle_y, hjust = if (angle_y >= 90) 0.5 else 1,
            size = input$yieldAxisTextSize %||% 12,
            face = input$yieldAxisTextYStyle %||% "plain"),
          legend.position = input$yieldLegendePos %||% "none",
          legend.title = ggplot2::element_text(size = input$yieldLegendeTitreSize %||% 12),
          legend.text = ggplot2::element_text(size = input$yieldLegendeTexteSize %||% 10),
          panel.grid.major = if (isTRUE(input$yieldGrilleMaj))
            ggplot2::element_line(colour = "gray90") else ggplot2::element_blank(),
          panel.grid.minor = if (isTRUE(input$yieldGrilleMin))
            ggplot2::element_line(colour = "gray95") else ggplot2::element_blank())
    })

    output$yieldPlot <- shiny::renderPlot({
      p <- graphique()
      shiny::validate(shiny::need(!is.null(p),
        "Aucune valeur à représenter pour cette mesure : vérifiez les colonnes choisies et le programme non traité."))
      p
    })

    output$yieldPlotNote <- shiny::renderUI({
      shiny::req(input$yieldMesure)
      r <- resultat()
      m <- input$yieldMesure
      neg <- NROW(r) && m %in% names(r) && any(is.finite(r[[m]]) & r[[m]] < 0)
      if (!.hstat_rdt_est_gain(m) && !neg) return(NULL)
      shiny::div(style = "font-size:12px;color:#7f8c8d;margin-bottom:10px;",
          shiny::icon("circle-info"), " ",
          if (.hstat_rdt_est_gain(m))
            "Un gain négatif signifie que la modalité fait moins bien que le programme non traité : c'est un résultat, pas une erreur."
          else
            "Les valeurs négatives sont représentées telles quelles, sous la ligne de zéro : les masquer ou les ramener à zéro cacherait précisément ce qu'il faut voir.")
    })

    # -------------------------------------------------------------- exports
    output$yieldExportTable <- shiny::renderUI({
      shiny::req(NROW(resultat()))
      shiny::fluidRow(
        shiny::column(6, shiny::downloadButton(ns("yieldDlCsv"),
          shiny::tagList(shiny::icon("file-csv"), " Télécharger (CSV)"),
          class = "btn-success btn-block")),
        shiny::column(6, shiny::downloadButton(ns("yieldDlXlsx"),
          shiny::tagList(shiny::icon("file-excel"), " Télécharger (Excel)"),
          class = "btn-success btn-block")))
    })

    output$yieldExportPlot <- shiny::renderUI({
      shiny::req(NROW(resultat()))
      shiny::div(style = "max-width:420px;margin:0 auto;",
        shiny::downloadButton(ns("yieldDlPlot"),
          shiny::tagList(shiny::icon("download"), " Télécharger le graphique"),
          class = "btn-success",
          style = "width:100%;height:48px;font-weight:bold;"))
    })

    output$yieldDlCsv <- shiny::downloadHandler(
      filename = function() paste0("rendements_", Sys.Date(), ".csv"),
      content = function(file) {
        r <- resultat(); shiny::req(NROW(r))
        utils::write.csv(as.data.frame(r), file, row.names = FALSE,
                         fileEncoding = "UTF-8")
      })

    output$yieldDlXlsx <- shiny::downloadHandler(
      filename = function() paste0("rendements_", Sys.Date(), ".xlsx"),
      content = function(file) {
        r <- resultat(); shiny::req(NROW(r))
        if (requireNamespace("openxlsx", quietly = TRUE))
          openxlsx::write.xlsx(list(Rendements = as.data.frame(r)), file)
        else
          utils::write.csv(as.data.frame(r), file, row.names = FALSE,
                           fileEncoding = "UTF-8")
      })

    output$yieldDlPlot <- shiny::downloadHandler(
      filename = function()
        paste0("rendement_", Sys.Date(), ".",
               hstat_img_fmt(input$yieldExportFormat)),
      content = function(file) {
        # Les pixels saisis fixent la MISE EN PAGE (lue a 96 ppp), le DPI la
        # finesse du rendu. Voir hstat_export_dims().
        dims <- hstat_export_dims(input$yieldExportW, input$yieldExportH,
                                  input$yieldExportDPI)
        if (!is.null(dims$note))
          shiny::showNotification(dims$note, type = "warning", duration = 8)
        fmt <- hstat_img_fmt(input$yieldExportFormat)
        ok <- hstat_ecrire_image(file, graphique(), fmt,
                                 dims$width_in, dims$height_in, dims$dpi)
        if (ok)
          shiny::showNotification(
            trf("Graphique exporté : %s, %s × %s pouces, %s DPI.",
                toupper(fmt), round(dims$width_in, 2), round(dims$height_in, 2),
                dims$dpi),
            type = "message", duration = 5)
        else
          shiny::showNotification(
            "Export impossible : le fichier téléchargé porte le motif. Réduisez les dimensions ou le DPI, ou choisissez un format vectoriel (SVG, PDF).",
            type = "error", duration = 10)
      })

    # ----------------------------------------------------------------- aide
    output$yieldAide <- shiny::renderUI({
      shiny::HTML(tr(paste0(
        "<h4>Les deux formules</h4>",
        "<p><b>Rendement</b> = masse récoltée / surface. La masse et la surface ",
        "se saisissent dans n'importe quelle unité : elles sont ramenées au ",
        "kilogramme et au mètre carré avant d'être exprimées dans l'unité que ",
        "vous choisissez.</p>",
        "<p><b>Gain (%)</b> = (rendement du traitement − rendement du programme ",
        "non traité) / rendement du programme non traité × 100.</p>",
        "<h4>Global ou moyen ?</h4>",
        "<p>Le rendement <b>global</b> rapporte la masse totale à la surface ",
        "totale : il pondère chaque répétition par sa surface. Le rendement ",
        "<b>moyen</b> traite chaque répétition à égalité. Les deux coïncident ",
        "quand les surfaces sont égales et divergent sinon — aucun n'est faux, ",
        "ils répondent à deux questions.</p>",
        "<h4>Ce que l'application refuse de calculer</h4>",
        "<ul>",
        "<li>Une surface nulle, négative ou manquante : le rendement serait ",
        "infini. La ligne est écartée et comptée.</li>",
        "<li>Un programme non traité de rendement nul : le gain serait une ",
        "division par zéro. Aucun gain n'est alors calculé.</li>",
        "</ul>")))
    })

    # ---------------------------------------------------- capture pour l'IA
    # Le registre est observe, pas instrumente : le module depose son resultat,
    # l'onglet d'interpretation et le rapport le reprennent.
    shiny::observeEvent(resultat(), {
      r <- resultat()
      if (!NROW(r)) return()
      hstat_ai_capture(
        values, "Rendement/Gain de rendement", "Rendements et gains par modalité",
        tables = list("Rendements et gains" = utils::head(as.data.frame(r), 200)),
        meta = list(`programme non traité` = input$yieldTemoin %||% "",
                    unité = unite_rdt(),
                    modalités = NROW(r)),
        plot = function() shiny::isolate(graphique()))
    }, ignoreInit = TRUE)
  })
}
