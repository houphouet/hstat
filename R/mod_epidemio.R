# ===========================================================================
#  MODULE EPIDEMIOLOGIE
# ===========================================================================
#  Le module ne CALCULE rien : tout vit dans `R/utils.R` (`hstat_epi_*`), ou
#  c'est testable. Ici on choisit, on affiche et on met en forme -- la regle du
#  depot, et elle a une raison propre a cette discipline : un rapport de risque
#  n'est jamais faux bruyamment.

# Le module prend le kit COMPLET (`hstat_plot_opts_ui`), celui que portent deja
# l'apprentissage, l'apprentissage profond et les series temporelles : titres,
# titres d'axes, theme, taille du texte, position de la legende, couleur,
# epaisseur, rotation des graduations, styles, bornes d'axes -- plus le trait
# des axes, la taille des cles et les quatre marges qu'il tire lui-meme du kit
# d'extras. C'est le vocabulaire du module Visualisation, et le prendre entier
# evite d'en recopier une douzieme version.
#
# Corollaire : le bloc d'export ne declare PLUS son propre selecteur de theme
# (`theme = FALSE`). Deux selecteurs pour un meme reglage, c'est l'utilisateur
# qui en change un pendant que la figure lit l'autre.
HSTAT_EPI_PREFIXE_MEF <- "epiG"

.hstat_epi_aide <- function(cle) {
  a <- HSTAT_EPI_ANALYSES[[cle]]
  if (is.null(a)) return(NULL)
  shiny::div(class = "hstat-interpretation",
    shiny::tags$b(tr(a[1])), shiny::tags$br(),
    shiny::tags$span(tr(a[2])), shiny::tags$br(),
    shiny::tags$em(paste0(tr("Quand l'employer :"), " "), tr(a[3])), shiny::tags$br(),
    shiny::tags$small(style = "color:#6b7280;",
      paste0(tr("Ce qu'il faut :"), " "), tr(a[4])))
}

mod_epidemio_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    .hstat_scope_banner(exact = FALSE),

    shiny::fluidRow(
      shiny::column(4,
        shinydashboard::box(
          title = shiny::tagList(shiny::icon("virus-covid"), " Analyse épidémiologique"),
          status = "danger", solidHeader = TRUE, width = 12,

          .hstat_opt_section(
            "Modèle", "stethoscope", "#c0392b", "#fdecea",
            shiny::selectInput(ns("epiAnalyse"), NULL,
              choices = stats::setNames(names(HSTAT_EPI_ANALYSES),
                                        vapply(HSTAT_EPI_ANALYSES, `[`, character(1), 1)),
              selected = "dlnm"),
            shiny::uiOutput(ns("epiAide"))),

          shiny::uiOutput(ns("epiConfigUI")),

          .hstat_opt_section(
            "Niveau de confiance", "percent", "#8e44ad", "#f4ecf7",
            shiny::sliderInput(ns("epiConf"), NULL, min = 0.80, max = 0.99,
                               value = 0.95, step = 0.01)),

          shiny::actionButton(ns("epiLancer"), "Lancer l'analyse",
            icon = shiny::icon("play"), class = "btn-danger btn-block btn-lg"),
          shiny::uiOutput(ns("epiMessage"))
        ),

        # LA BOITE DE FIGURE N'EXISTE QUE QUAND IL Y A UNE FIGURE A REGLER :
        # construite d'emblee, elle pousse hors de l'ecran ce qu'on vient regler.
        shiny::conditionalPanel(
          ns = ns, condition = "output.hasEpi",
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("sliders"), " Choix de la figure"),
            status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
            shiny::uiOutput(ns("epiFigureUI")))),

        # ELLE SE RETIRE POUR LES FIGURES TRACEES EN GRAPHIQUES DE BASE : la
        # surface 3D et les diagnostics n'obeissent pas au theme ggplot, et
        # offrir un reglage que l'image ignore est le defaut que ce depot
        # traque partout ailleurs.
        # LA CONDITION SE LIT SUR `input`, PAS SUR UN DRAPEAU SERVEUR. Les deux
        # marchent -- verifie au navigateur : `display:none` sur la surface 3D
        # et les diagnostics, `block` sur les quatre figures ggplot. Celle-ci
        # evite simplement un aller-retour serveur : `input.epiFigure` est deja
        # dans le navigateur, la bascule est donc immediate, et il y a une
        # sortie reactive de moins a tenir.
        #
        # La liste vient de `HSTAT_EPI_FIGURES_BASE` : la recopier en JavaScript
        # la ferait diverger au premier ajout, et c'est la copie oubliee qui
        # ment.
      ),

      shiny::column(8,
        shinydashboard::box(
          title = shiny::tagList(shiny::icon("table-list"), " Résultats"),
          status = "danger", solidHeader = TRUE, width = 12,
          shiny::tabsetPanel(
            shiny::tabPanel(
              shiny::tagList(shiny::icon("list-check"), " Synthèse"),
              shiny::br(),
              shiny::uiOutput(ns("epiSynthese")),
              DT::DTOutput(ns("epiTable1"))),
            shiny::tabPanel(
              shiny::tagList(shiny::icon("clock-rotate-left"), " Détail"),
              shiny::br(),
              shiny::uiOutput(ns("epiTitre2")),
              DT::DTOutput(ns("epiTable2"))),
            shiny::tabPanel(
              shiny::tagList(shiny::icon("chart-line"), " Graphique"),
              shiny::br(),
              shiny::plotOutput(ns("epiPlot"), height = "560px"),
              shiny::uiOutput(ns("epiPlotNote"))),
            shiny::tabPanel(
              shiny::tagList(shiny::icon("triangle-exclamation"), " Diagnostic"),
              shiny::br(),
              shiny::uiOutput(ns("epiDiagTitre")),
              DT::DTOutput(ns("epiTable3"))))),

        shiny::conditionalPanel(
          ns = ns, condition = "output.hasEpi",
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("download"), " Exporter"),
            status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE,
            shiny::h5(tr("Graphique")),
            hstat_export_plot_ui(ns, "epiP", width = 10, height = 6, theme = FALSE),
            shiny::hr(),
            shiny::h5(tr("Tableaux")),
            shiny::fluidRow(
              shiny::column(6, shiny::downloadButton(ns("epiTXlsx"), "Excel (.xlsx)",
                class = "btn-success btn-block")),
              shiny::column(6, shiny::downloadButton(ns("epiTCsv"), "CSV (.zip)",
                class = "btn-info btn-block"))))),

        # LA MISE EN FORME VIT SOUS L'EXPORT, ET DU MEME COTE QUE LA FIGURE :
        # demande a l'ecran. Elle etait dans la colonne des reglages d'analyse,
        # a gauche -- donc loin de l'image qu'elle habille, et il fallait
        # traverser l'ecran des yeux entre chaque essai.
        #
        # ELLE SE RETIRE POUR LES FIGURES TRACEES EN GRAPHIQUES DE BASE : la
        # surface 3D et les diagnostics n'obeissent pas au theme ggplot, et
        # offrir un reglage que l'image ignore est le defaut que ce depot
        # traque partout ailleurs. La liste vient de `HSTAT_EPI_FIGURES_BASE` :
        # la recopier en JavaScript la ferait diverger au premier ajout.
        shiny::conditionalPanel(
          ns = ns,
          condition = sprintf("output.hasEpi && ['%s'].indexOf(input.epiFigure) < 0",
                              paste(HSTAT_EPI_FIGURES_BASE, collapse = "','")),
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("paint-roller"), " Mise en forme du graphique"),
            status = "primary", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = TRUE,
            hstat_plot_opts_ui(ns, HSTAT_EPI_PREFIXE_MEF),
            shiny::hr(),
            .hstat_opt_section(
              "Étiquette de la ligne de référence", "tag", "#2980b9", "#eaf4fb",
              shiny::checkboxInput(ns("epiRefLab"),
                "Nommer la ligne de référence sur la figure", value = TRUE),
              shiny::textInput(ns("epiRefTxt"),
                "Texte (vide = la valeur de référence)", value = ""),
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("epiRefPos"), "Position",
                  choices = HSTAT_EPI_REPERE_POS, selected = "haut")),
                shiny::column(6, shiny::selectInput(ns("epiRefCote"), "Côté",
                  choices = HSTAT_EPI_REPERE_COTE, selected = "droite"))),
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput(ns("epiRefTaille"), "Taille",
                  value = 3.5, min = 1.5, max = 12, step = 0.5)),
                shiny::column(6, shiny::selectInput(ns("epiRefStyle"), "Style",
                  choices = HSTAT_FONT_STYLES, selected = "plain"))))))
      )
    )
  )
}

mod_epidemio_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # L'etat du module lui appartient : `values` porte l'etat de SESSION, et y
    # ranger un modele le ferait survivre a un changement de fichier -- le
    # defaut deja corrige sur les resultats multivaries.
    rv <- shiny::reactiveValues(res = NULL, analyse = NULL, message = NULL)

    donnees <- shiny::reactive({
      values$filteredData %||% values$cleanData %||% values$data
    })

    shiny::observeEvent(values$resetSignal, {
      rv$res <- NULL; rv$analyse <- NULL; rv$message <- NULL
    }, ignoreInit = TRUE)

    cols_num <- shiny::reactive({
      df <- donnees(); if (is.null(df)) return(character(0))
      names(df)[vapply(df, is.numeric, logical(1))]
    })
    cols_tous <- shiny::reactive({
      df <- donnees(); if (is.null(df)) return(character(0)); names(df)
    })

    output$epiAide <- shiny::renderUI(.hstat_epi_aide(input$epiAnalyse %||% "dlnm"))

    # ------------------------------------------------------------- CONFIG
    output$epiConfigUI <- shiny::renderUI({
      df <- donnees()
      if (is.null(df) || !ncol(df))
        return(shiny::tags$em(style = "color:#7f8c8d;",
          tr("Chargez d'abord un jeu de données dans l'onglet Chargement.")))
      n <- cols_num(); a <- cols_tous()
      switch(input$epiAnalyse %||% "dlnm",

        dlnm = shiny::tagList(
          .hstat_opt_section("Issue et expositions", "temperature-three-quarters",
            "#c0392b", "#fdecea",
            # PLUSIEURS ISSUES EN UN SEUL LANCEMENT : un registre en porte
            # souvent trois ou quatre, et les relancer une par une refait a la
            # main la boucle que le module sait faire -- en re-saisissant dix
            # reglages a chaque tour.
            shiny::selectInput(ns("epiY"), "Issues (décomptes d'événements)",
                               choices = n, multiple = TRUE),
            shiny::tags$small(style = "color:#6b7280;",
              tr("Chaque issue est analysée avec chaque variable d'influence ; les résultats sont listés par couple.")),
            # PLUSIEURS EXPOSITIONS, PARCE QU'ELLES VARIENT ENSEMBLE. La
            # temperature n'agit pas seule : pluviometrie, humidite relative et
            # vent covarient, et estimer l'une sans les autres lui attribue ce
            # qui revient aux autres.
            shiny::selectInput(ns("epiExpo"),
              "Variable d'influence (°C, mm, %, etc)",
              choices = n, multiple = TRUE),
            shiny::tags$small(style = "color:#6b7280;",
              tr("Chaque exposition retenue est analysée à son tour ; les autres servent alors d'ajustement.")),
            shiny::checkboxInput(ns("epiMutuel"),
              "Ajuster chaque exposition sur les autres", value = TRUE),
            # PLUSIEURS DENOMINATEURS S'AJOUTENT SUR L'ECHELLE LOG : une
            # population ET une durée d'observation donnent des personnes-mois,
            # donc la somme des logarithmes. Les déclarer séparément évite de
            # fabriquer la colonne produit dans un tableur.
            shiny::selectInput(ns("epiOffset"),
              "Dénominateurs (population, durée… — facultatif)",
              choices = n, multiple = TRUE),
            shiny::tags$small(style = "color:#6b7280;",
              tr("Plusieurs dénominateurs se multiplient (personnes × mois). Une ligne dont un seul manque est écartée.")),
            # UNE MESURE D'AMBIANCE N'EST PAS L'EXPOSITION D'UNE PERSONNE.
            shiny::checkboxInput(ns("epiProxy"),
              "Ramener les mesures d'ambiance à l'exposition individuelle",
              value = FALSE),
            shiny::conditionalPanel(ns = ns, condition = "input.epiProxy",
              shiny::uiOutput(ns("epiProxyUI")),
              shiny::tags$small(style = "color:#6b7280;",
                tr("Personnelle = a × ambiance + b. Cela change l'axe, la référence et les valeurs des tableaux ; le RR lu à un percentile donné, lui, ne change pas."))),
            shiny::selectInput(ns("epiTemps"),
              "Colonne de date complète (jour, mois et année — facultatif)",
              choices = c("(ordre des lignes)" = "", a)),
            # UNE DATE SE COMPOSE PARFOIS DE DEUX COLONNES. Un registre mensuel
            # saisi a la main porte « Mois » en toutes lettres et « Annee » a
            # cote, et aucune colonne de date : exiger l'ISO obligerait a
            # rouvrir le fichier pour fabriquer ce qu'il contient deja.
            shiny::fluidRow(
              shiny::column(6, shiny::selectInput(ns("epiMois"),
                "Mois (si aucune colonne de date)",
                choices = c("(aucun)" = "", a))),
              shiny::column(6, shiny::selectInput(ns("epiAnnee"),
                "Année (si aucune colonne de date)",
                choices = c("(aucune)" = "", a)))),
            shiny::tags$small(style = "color:#6b7280;",
              tr("Le mois et l'année se déclarent ensemble ; le jour est alors fixé au premier du mois.")),
            # LE CHEMIN INVERSE : une date complete se DECOMPOSE. C'est une
            # action, pas une case -- les colonnes doivent exister pour etre
            # choisies en covariable, ici comme dans les autres onglets, et un
            # drapeau lu au calcul donnerait des colonnes inatteignables.
            shiny::checkboxInput(ns("epiDecomposer"),
              "J'ai une date complète : en extraire l'année, le mois et le jour",
              value = FALSE),
            shiny::conditionalPanel(
              ns = ns, condition = "input.epiDecomposer",
              shiny::checkboxGroupInput(ns("epiParties"), "Éléments à extraire",
                choices = HSTAT_EPI_PARTIES,
                selected = c("annee", "mois", "jour")),
              shiny::actionButton(ns("epiExtraire"),
                shiny::tagList(shiny::icon("scissors"), " Extraire les colonnes"),
                class = "btn-primary btn-sm"),
              shiny::tags$small(style = "color:#6b7280; display:block; margin-top:6px;",
                tr("Les colonnes sont ajoutées au jeu de données ; la colonne de date d'origine est conservée et reste utilisable ici."))),
            shiny::selectInput(ns("epiAjust"), "Covariables simples (facultatif)",
              choices = a, multiple = TRUE)),
          .hstat_opt_section("Structure du retard", "hourglass-half", "#2980b9", "#eaf4fb",
            shiny::numericInput(ns("epiLag"), "Décalage maximal", value = 5,
                                min = 0, max = 60, step = 1),
            shiny::numericInput(ns("epiNkLag"), "Nœuds sur l'axe des retards",
                                value = 2, min = 1, max = 5, step = 1),
            # LA SOUPLESSE DE L'EXPOSITION EST LE LEVIER QUI PESE LE PLUS sur le
            # budget de parametres, et elle n'etait pas atteignable : elle
            # valait trois noeuds en dur. Mesure sur 118 mois et deux
            # expositions, l'intervalle du RR au 90e percentile passe d'un
            # rapport de 24 (trois noeuds) a 5 (lineaire).
            shiny::selectInput(ns("epiNoeuds"), "Souplesse de la réponse",
                               choices = hstat_epi_noeuds_choix(), selected = "k3"),
            shiny::tags$small(style = "color:#6b7280;",
              tr("Moins de nœuds = moins de paramètres = intervalles plus étroits. Le budget est chiffré dans l'onglet Diagnostic.")),
            shiny::numericInput(ns("epiRef"),
              "Référence de l'exposition (vide = médiane)", value = NULL)),
          .hstat_opt_section("Saison, tendance et loi", "calendar-days", "#16a085", "#e8f8f4",
            shiny::selectInput(ns("epiFamille"), "Loi du modèle",
              choices = HSTAT_EPI_DLNM_FAMILLES, selected = "auto"),
            shiny::numericInput(ns("epiPeriode"),
              "Période saisonnière (12 = mensuel, 365 = journalier)",
              value = 12, min = 2, max = 400, step = 1),
            shiny::numericInput(ns("epiHarmo"), "Harmoniques de Fourier",
                                value = 2, min = 0, max = 6, step = 1),
            shiny::numericInput(ns("epiTendance"), "Souplesse de la tendance longue",
                                value = 3, min = 0, max = 20, step = 1))),

        taux = .hstat_opt_section("Colonnes", "list-ul", "#c0392b", "#fdecea",
          shiny::selectInput(ns("epiY"), "Nombre d'événements", choices = n),
          shiny::selectInput(ns("epiX"), "Facteurs à comparer", choices = a, multiple = TRUE),
          shiny::selectInput(ns("epiTp"), "Temps-personne (offset)",
            choices = c("(aucun)" = "", n)),
          shiny::selectInput(ns("epiFamille"), "Loi du modèle",
            choices = HSTAT_EPI_DLNM_FAMILLES, selected = "auto")),

        risque = .hstat_opt_section("Colonnes", "list-ul", "#c0392b", "#fdecea",
          shiny::selectInput(ns("epiY"), "Issue (0/1 ou modalité déclarée)", choices = a),
          shiny::uiOutput(ns("epiCasUI")),
          shiny::selectInput(ns("epiX"), "Facteurs d'exposition",
                             choices = a, multiple = TRUE)),

        survie = .hstat_opt_section("Colonnes", "list-ul", "#c0392b", "#fdecea",
          shiny::selectInput(ns("epiDuree"), "Durée de suivi", choices = n),
          shiny::selectInput(ns("epiEvent"), "Événement (1 = survenu)", choices = a),
          shiny::uiOutput(ns("epiCasUI")),
          shiny::selectInput(ns("epiX"), "Facteurs (le premier sert au log-rank)",
                             choices = a, multiple = TRUE)),

        cascroise = .hstat_opt_section("Colonnes", "list-ul", "#c0392b", "#fdecea",
          shiny::selectInput(ns("epiTemps"), "Date (pas journalier)", choices = a),
          shiny::selectInput(ns("epiY"), "Nombre de cas", choices = n),
          shiny::selectInput(ns("epiExpoU"), "Exposition", choices = n),
          shiny::selectInput(ns("epiAjust"), "Covariables (facultatif)",
                             choices = a, multiple = TRUE)),

        smr = .hstat_opt_section("Colonnes", "list-ul", "#c0392b", "#fdecea",
          shiny::selectInput(ns("epiY"), "Événements observés", choices = n),
          shiny::selectInput(ns("epiPop"), "Population (ou temps-personne)", choices = n),
          shiny::selectInput(ns("epiTxRef"), "Taux de référence par strate (facultatif)",
            choices = c("(taux internes)" = "", n)),
          shiny::selectInput(ns("epiStrate"), "Strate (âge, sexe…)",
            choices = c("(aucune)" = "", a)),
          shiny::selectInput(ns("epiGroupe"), "Groupe à comparer",
            choices = c("(ensemble)" = "", a))),

        diagnostic = .hstat_opt_section("Colonnes", "list-ul", "#c0392b", "#fdecea",
          shiny::selectInput(ns("epiTest"), "Résultat du test", choices = a),
          shiny::selectInput(ns("epiY"), "État de référence", choices = a),
          shiny::uiOutput(ns("epiCasUI")),
          shiny::numericInput(ns("epiSeuil"),
            "Seuil (test continu ; vide = médiane)", value = NULL),
          shiny::numericInput(ns("epiPrev"),
            "Prévalence en population (vide = celle du fichier)",
            value = NULL, min = 0, max = 1, step = 0.001)),

        impact = .hstat_opt_section("Tableau 2 × 2", "table-cells", "#c0392b", "#fdecea",
          shiny::tags$small(style = "color:#6b7280;",
            tr("Effectifs du croisement exposition × issue.")),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("epiA"), "Exposés, malades",
                                                 value = 40, min = 0, step = 1)),
            shiny::column(6, shiny::numericInput(ns("epiB"), "Exposés, sains",
                                                 value = 60, min = 0, step = 1))),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("epiC"), "Non exposés, malades",
                                                 value = 20, min = 0, step = 1)),
            shiny::column(6, shiny::numericInput(ns("epiD"), "Non exposés, sains",
                                                 value = 80, min = 0, step = 1))),
          shiny::numericInput(ns("epiPrevExpo"),
            "Prévalence de l'exposition en population (vide = celle du fichier)",
            value = NULL, min = 0, max = 1, step = 0.001)),
        NULL)
    })

    # LA MODALITE « CAS » SE DECLARE, ELLE NE SE DEVINE PAS. La prendre par ordre
    # alphabetique rendrait l'effet a l'endroit sur « Malade/Sain » et a
    # l'ENVERS sur « Negatif/Positif » -- le meme code, deux conclusions
    # opposees selon l'orthographe du fichier.
    output$epiCasUI <- shiny::renderUI({
      df <- donnees()
      v <- if (identical(input$epiAnalyse, "survie")) input$epiEvent else input$epiY
      if (is.null(df) || !isTRUE(nzchar(v %||% "")) || !v %in% names(df)) return(NULL)
      x <- df[[v]]
      if (is.logical(x)) return(NULL)
      nx <- suppressWarnings(as.numeric(x))
      u <- sort(unique(nx[is.finite(nx)]))
      if (length(u) == 2L && all(u %in% c(0, 1))) return(NULL)
      m <- sort(unique(as.character(stats::na.omit(x))))
      if (length(m) > 20L) return(shiny::tags$small(style = "color:#c0392b;",
        tr("Cette colonne porte plus de 20 modalités : ce n'est pas une issue binaire.")))
      shiny::selectInput(ns("epiCas"), tr("Modalité représentant le cas"),
                         choices = c("(à choisir)" = "", m))
    })

    # ------------------------------------------------------------- LANCEMENT
    # ------------------------------------------------ DECOMPOSITION D'UNE DATE
    #  Les colonnes rejoignent le JEU DE TRAVAIL, comme le fait le typage de
    #  l'onglet Nettoyage : elles derivent du fichier courant, elles ne le
    #  remplacent pas. Pas de remise a zero de session, donc -- purger ici
    #  effacerait l'analyse qui vient de les demander.
    shiny::observeEvent(input$epiExtraire, {
      df <- donnees()
      if (is.null(df) || !NROW(df)) {
        shiny::showNotification(
          shiny::tagList(shiny::icon("triangle-exclamation"),
                         tr(" Chargez d'abord un jeu de données.")),
          type = "warning")
        return()
      }
      r <- hstat_epi_date_parts(df, input$epiTemps, input$epiParties)
      if (!length(r$ajoutees)) {
        shiny::showNotification(
          shiny::tagList(shiny::icon("triangle-exclamation"),
                         " ", r$message %||% tr("Aucune colonne n'a pu être extraite.")),
          type = "warning", duration = 10)
        return()
      }
      values$data         <- r$data
      values$cleanData    <- r$data
      values$filteredData <- r$data
      shiny::showNotification(
        shiny::tagList(shiny::icon("check"), " ", r$message),
        type = "message", duration = 10)
    })

    shiny::observeEvent(input$epiLancer, {
      df <- donnees()
      an <- input$epiAnalyse %||% "dlnm"
      if (is.null(df) && !identical(an, "impact")) {
        shiny::showNotification(
          tr("Chargez d'abord un jeu de données dans l'onglet Chargement."),
          type = "error", duration = 5)
        return()
      }
      conf <- hstat_finite(input$epiConf, 0.95)
      vide <- function(x) { v <- as.character(x %||% character(0)); v[nzchar(v)] }
      # Les facteurs de transfert sont relus PAR EXPOSITION, et seulement si la
      # case est cochee : un champ laisse a 0,8 puis decoche ne doit pas
      # continuer d'agir en silence.
      pr <- list(a = NULL, b = NULL)
      if (isTRUE(input$epiProxy)) for (v in vide(input$epiExpo)) {
        pr$a[[v]] <- hstat_finite(input[[paste0("epiPxA_", v)]], 1)
        pr$b[[v]] <- hstat_finite(input[[paste0("epiPxB_", v)]], 0)
      }

      res <- tryCatch(switch(an,
        dlnm = hstat_epi_dlnm_multi(df, vide(input$epiY), vide(input$epiExpo),
          mutuel = isTRUE(input$epiMutuel), var_offset = input$epiOffset,
          proxy_a = pr$a, proxy_b = pr$b,
          var_temps = input$epiTemps, var_mois = input$epiMois,
          var_annee = input$epiAnnee, vars_ajust = vide(input$epiAjust),
          lag_max = hstat_finite(input$epiLag, 5),
          nk_lag = hstat_finite(input$epiNkLag, 2),
          pct_noeuds = hstat_epi_noeuds_probs(input$epiNoeuds),
          reference = input$epiRef,
          famille = input$epiFamille %||% "auto",
          periode = hstat_finite(input$epiPeriode, 12),
          harmoniques = hstat_finite(input$epiHarmo, 2),
          df_tendance = hstat_finite(input$epiTendance, 3), conf = conf),
        taux = hstat_epi_taux(df, input$epiY, vide(input$epiX), input$epiTp,
          famille = input$epiFamille %||% "auto", conf = conf),
        risque = hstat_epi_risque(df, input$epiY, vide(input$epiX),
          cas = input$epiCas, conf = conf),
        survie = hstat_epi_survie(df, input$epiDuree, input$epiEvent,
          vide(input$epiX), event_cas = input$epiCas, conf = conf),
        cascroise = hstat_epi_cas_croise(df, input$epiTemps, input$epiY,
          input$epiExpoU, vide(input$epiAjust), conf = conf),
        smr = hstat_epi_smr(df, input$epiY, input$epiPop, input$epiTxRef,
          input$epiStrate, input$epiGroupe, conf = conf),
        diagnostic = hstat_epi_diagnostic(df, input$epiTest, input$epiY,
          ref_positif = input$epiCas, seuil = input$epiSeuil,
          conf = conf, prevalence = input$epiPrev),
        impact = hstat_epi_impact(input$epiA, input$epiB, input$epiC, input$epiD,
          conf = conf, prevalence_expo = input$epiPrevExpo),
        list(ok = FALSE, message = tr("Analyse inconnue."))),
        error = function(e) list(ok = FALSE, message = hstat_err_fr(e)))

      if (!isTRUE(res$ok)) {
        rv$res <- NULL; rv$analyse <- NULL
        rv$message <- res$message %||% tr("L'analyse a échoué.")
        shiny::showNotification(rv$message, type = "error", duration = 12)
        return()
      }
      rv$res <- res; rv$analyse <- an; rv$message <- res$message
      shiny::showNotification(tr("Analyse terminée."), type = "message", duration = 4)
    })

    output$epiMessage <- shiny::renderUI({
      if (!isTRUE(nzchar(rv$message %||% ""))) return(NULL)
      shiny::div(class = "hstat-interpretation",
        shiny::icon("circle-info"), " ",
        # LE MESSAGE PORTE DES NOMS DE COLONNES VENUS DU FICHIER : un intitule
        # « Rendement <2023> » y verrait ses chevrons lus comme une balise et
        # DISPARAITRAIT. On echappe.
        shiny::HTML(hstat_html_escape(rv$message)))
    })

    # Le drapeau double d'un `outputOptions(suspendWhenHidden = FALSE)` : sans
    # lui, une sortie suspendue ne se recalcule pas et la boite ne reapparait
    # jamais.
    output$hasEpi <- shiny::reactive(!is.null(rv$res))
    shiny::outputOptions(output, "hasEpi", suspendWhenHidden = FALSE)

    # UN CHAMP PAR EXPOSITION, ET CHACUN TRAVERSE `ns()` : un widget cree sans
    # lui porte « epiPxA_Tmax » quand le serveur lit « epidemio-epiPxA_Tmax ».
    # Les deux ne se rencontrent jamais, rien ne leve, et le reglage ne fait
    # simplement rien -- le defaut le plus silencieux du depot.
    output$epiProxyUI <- shiny::renderUI({
      vs <- as.character(input$epiExpo %||% character(0))
      vs <- vs[nzchar(vs)]
      if (!length(vs)) return(shiny::tags$em(style = "color:#7f8c8d;",
        tr("Choisissez d'abord une variable d'influence.")))
      do.call(shiny::tagList, lapply(vs, function(v) shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns(paste0("epiPxA_", v)),
          paste0(v, " — a"), value = 1, min = 0.01, step = 0.05)),
        shiny::column(6, shiny::numericInput(ns(paste0("epiPxB_", v)),
          paste0(v, " — b"), value = 0, step = 0.5)))))
    })

    output$epiFigureUI <- shiny::renderUI({
      an <- rv$analyse
      if (is.null(an)) return(NULL)
      fg <- HSTAT_EPI_FIGURES[[an]]
      if (is.null(fg)) return(NULL)
      shiny::tagList(
        # `input$epiFigure` VAUT NULL AU PREMIER RENDU, et `NULL %in% fg` rend
        # `logical(0)` : `if()` y leve « argument is of length zero », l'erreur
        # tombe dans le `renderUI`, et LE SELECTEUR N'EXISTE JAMAIS -- donc
        # aucune figure ne se trace, sur une analyse pourtant calculee. Meme
        # famille que le piege deja documente sur `input$divSite`. C'est la
        # longueur qui decide, jamais l'appartenance seule.
        shiny::selectInput(ns("epiFigure"), tr("Figure"), choices = fg,
                           selected = shiny::isolate(
                             if (isTRUE((input$epiFigure %||% "") %in% fg))
                               input$epiFigure else fg[[1]])),
        # PLUSIEURS EXPOSITIONS SE REPRESENTENT ENSEMBLE, et l'entree n'apparait
        # qu'a partir de deux -- proposer « toutes » sur une seule serait un
        # choix qui ne change rien.
        if (identical(an, "dlnm") && length(rv$res$resultats) > 1L)
          shiny::selectInput(ns("epiFigExpo"), tr("Variable représentée"),
                             choices = c(stats::setNames(
                               HSTAT_EPI_TOUTES,
                               tr("Toutes ensemble")), names(rv$res$resultats))),
        # Le MODE ne s'offre que pour les figures qui savent en porter
        # plusieurs, et seulement quand « toutes » est choisi.
        if (identical(an, "dlnm") && length(rv$res$resultats) > 1L)
          shiny::conditionalPanel(
            ns = ns,
            condition = sprintf("input.epiFigExpo == '%s' && ['%s'].indexOf(input.epiFigure) >= 0",
                                HSTAT_EPI_TOUTES,
                                paste(setdiff(HSTAT_EPI_MULTI_FIG,
                                              c(HSTAT_EPI_MULTI_FACETTES,
                                                HSTAT_EPI_MULTI_PCT)), collapse = "','")),
            shiny::radioButtons(ns("epiFigMode"), tr("Disposition"),
                                choices = HSTAT_EPI_MULTI_MODES,
                                selected = "facettes")))
    })

    # ------------------------------------------------------------- TABLEAUX
    # `courant()` rend le resultat A TRACER : pour le DLNM multi-expositions,
    # c'est celui de l'exposition choisie. Le detour par un reactif evite que
    # chaque sortie refasse le meme aiguillage -- trois copies finiraient par
    # diverger, et c'est la copie oubliee qui ment.
    courant <- shiny::reactive({
      r <- rv$res; if (is.null(r)) return(NULL)
      if (!identical(rv$analyse, "dlnm")) return(r)
      v <- input$epiFigExpo %||% names(r$resultats)[1]
      r$resultats[[if (v %in% names(r$resultats)) v else names(r$resultats)[1]]]

    })

    tables <- shiny::reactive({
      r <- rv$res; an <- rv$analyse
      if (is.null(r) || is.null(an)) return(NULL)
      switch(an,
        dlnm = {
          # LE BUDGET DE PARAMETRES EST UN TABLEAU DE RESULTAT, pas une note :
          # c'est lui qui explique un intervalle de [1,1 ; 352], et un chiffre
          # qu'on ne peut pas exporter ne part pas dans le rapport.
          bg <- tryCatch(hstat_epi_dlnm_budget(courant()), error = function(e) NULL)
          out <- list(Comparaison = r$comparaison)
          if (!is.null(bg)) out[["Budget_parametres"]] <- as.data.frame(bg)
          for (v in names(r$resultats)) {
            # GARDE PAR EXPOSITION : une surface dont le tableau echoue ne doit
            # pas emporter celui des autres.
            tb <- tryCatch(hstat_epi_dlnm_rr(r$resultats[[v]]), error = function(e) NULL)
            lg <- tryCatch(hstat_epi_dlnm_lags(r$resultats[[v]]), error = function(e) NULL)
            if (!is.null(tb)) out[[hstat_feuille_nom(paste0("Cumul_", v))]] <- tb
            if (!is.null(lg)) out[[hstat_feuille_nom(paste0("Retards_", v))]] <- as.data.frame(lg)
          }
          if (!is.null(r$resultats[[1]]$collinearite))
            out[["Collinearite"]] <- as.data.frame(r$resultats[[1]]$collinearite)
          out
        },
        taux   = list(IRR = r$coefs),
        risque = c(list(OR = r$or),
                   if (!is.null(r$rr)) list(RR = r$rr),
                   if (!is.null(r$ecart)) list(Ecart_OR_RR = r$ecart)),
        survie = c(if (!is.null(r$cox)) list(Cox_HR = r$cox),
                   if (!is.null(r$ph)) list(Hypothese_PH = r$ph),
                   if (!is.null(r$mediane)) list(Resume = r$mediane)),
        cascroise  = list(RR = r$coefs),
        smr        = c(list(SMR = r$smr),
                       if (!is.null(r$directe)) list(Standardisation_directe = r$directe)),
        diagnostic = c(list(Mesures = r$table),
                       if (!is.null(r$roc)) list(ROC = r$roc$courbe)),
        impact     = list(Impact = r$table),
        NULL)
    })

    t1 <- shiny::reactive({ tb <- tables(); if (is.null(tb) || !length(tb)) NULL else tb[[1]] })
    t2 <- shiny::reactive({ tb <- tables(); if (is.null(tb) || length(tb) < 2L) NULL else tb[[2]] })
    t3 <- shiny::reactive({ tb <- tables(); if (is.null(tb) || length(tb) < 3L) NULL else tb[[3]] })

    rendre <- function(fun) DT::renderDT({
      d <- fun()
      shiny::validate(shiny::need(!is.null(d) && NROW(d),
        tr("Lancez une analyse pour voir les résultats.")))
      d <- as.data.frame(d)
      hstat_dt_arrondi(DT::datatable(d, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)), d)
    })
    output$epiTable1 <- rendre(t1)
    output$epiTable2 <- rendre(t2)
    output$epiTable3 <- rendre(t3)

    titre_de <- function(k) shiny::renderUI({
      tb <- tables()
      if (is.null(tb) || length(tb) < k) return(NULL)
      shiny::h5(hstat_html_escape(names(tb)[k]))
    })
    output$epiTitre2 <- titre_de(2L)
    output$epiDiagTitre <- titre_de(3L)

    output$epiSynthese <- shiny::renderUI({
      r <- rv$res; an <- rv$analyse
      if (is.null(r)) return(NULL)
      l <- switch(an,
        dlnm = {
          c0 <- courant()
          trf("Loi retenue : %s | AIC : %.1f | déviance expliquée : %.1f %% | %d observations utilisées | référence : %.4g",
              c0$famille, c0$aic, c0$dev_expl, c0$n_utilisees, c0$reference)
        },
        taux = trf("Loi retenue : %s | AIC : %.1f | %d observations", r$famille, r$aic, r$n),
        risque = trf("%d observations | issue présente chez %.1f %% | risque relatif issu de : %s",
                     r$n, 100 * r$prevalence, r$rr_source),
        survie = trf("%d sujets | %d événements observés", r$n, r$evenements),
        cascroise = trf("%d journées | %d strates", r$n, r$strates),
        smr = trf("%d groupe(s) comparé(s)", nrow(r$smr)),
        diagnostic = trf("Prévalence retenue : %.3g %% (observée : %.3g %%)",
                         100 * r$prevalence, 100 * r$prevalence_observee),
        impact = trf("Prévalence de l'exposition : %.1f %%", 100 * r$prevalence_expo),
        NULL)
      if (identical(an, "dlnm")) {
        # UN INTERVALLE ENORME SE LIT DANS LE BUDGET, PAS DANS LA FIGURE. Le
        # verdict est affiche a cote de l'ajustement plutot que range dans un
        # onglet : c'est la premiere question qu'on se pose devant un RR de 20.
        bg <- tryCatch(hstat_epi_dlnm_budget(courant()), error = function(e) NULL)
        if (!is.null(bg) && !identical(attr(bg, "verdict"), "confortable"))
          l <- paste(l, "|", attr(bg, "message"))
        w <- tryCatch(hstat_epi_dlnm_wald(courant()), error = function(e) NULL)
        if (!is.null(w))
          l <- paste(l, trf("| test global de la surface : χ² = %.2f (%d ddl), p = %s — %s",
                            w$chi2, w$ddl, format.pval(w$p, digits = 3), w$verdict))
      }
      if (is.null(l)) return(NULL)
      shiny::div(class = "hstat-interpretation", shiny::HTML(hstat_html_escape(l)))
    })

    # ------------------------------------------------------------- GRAPHIQUE
    # L'ETIQUETTE DE LA LIGNE DE REFERENCE voyage AVEC la figure : elle est
    # tracee en coordonnees de donnees sur l'axe du trait, donc elle ne peut
    # pas etre posee apres coup par le kit de mise en forme, qui ne connait ni
    # la valeur de reference ni l'etendue de l'autre axe.
    repere <- shiny::reactive(list(
      montrer = isTRUE(input$epiRefLab %||% TRUE),
      texte   = input$epiRefTxt %||% "",
      position = input$epiRefPos %||% "haut",
      cote     = input$epiRefCote %||% "droite",
      taille   = hstat_finite(input$epiRefTaille, 3.5),
      style    = input$epiRefStyle %||% "plain"))

    figure <- shiny::reactive({
      r <- rv$res; an <- rv$analyse
      if (is.null(r) || is.null(an)) return(NULL)
      f <- input$epiFigure %||% names(HSTAT_EPI_FIGURES[[an]])[1]
      o <- list(repere = repere())

      # « TOUTES ENSEMBLE » N'EST PAS UN REPLI : la figure multi rend `NULL`
      # quand elle ne s'y prete pas (surface 3D, diagnostics), et on retombe
      # alors sur l'exposition courante -- ce que la note sous la figure dit.
      p <- NULL
      if (identical(an, "dlnm") &&
          identical(input$epiFigExpo %||% "", HSTAT_EPI_TOUTES))
        p <- hstat_epi_figure_multi(f, r$resultats,
                                    mode = input$epiFigMode %||% "facettes")
      if (is.null(p)) {
        cr <- courant(); if (is.null(cr)) return(NULL)
        p <- hstat_epi_figure(an, f, cr, o)
      }
      # LE KIT SE POSE EN DERNIER, ET SEULEMENT SUR UN GGPLOT : les figures de
      # base sont des FONCTIONS de trace, et `g + theme()` y leverait.
      if (inherits(p, "ggplot")) p <- hstat_apply_plot_opts(p, input, HSTAT_EPI_PREFIXE_MEF)
      p
    })

    output$epiPlot <- shiny::renderPlot({
      p <- figure()
      shiny::validate(shiny::need(!is.null(p),
        tr("Lancez une analyse, puis choisissez une figure.")))
      if (is.function(p)) p() else print(p)
    })

    output$epiPlotNote <- shiny::renderUI({
      f <- input$epiFigure %||% ""
      n <- list()
      if (f %in% HSTAT_EPI_FIGURES_BASE)
        n <- c(n, list(tr("Cette figure est tracée en graphiques de base : le panneau « Mise en forme du graphique » ne s'y applique pas, il est donc retiré.")))
      # UNE FIGURE QUI N'A PAS PU MONTRER TOUTES LES EXPOSITIONS LE DIT. Sans
      # ce mot, on lit une seule variable en croyant les voir toutes -- et
      # c'est cette figure-la qui part au rapport.
      if (identical(input$epiFigExpo %||% "", HSTAT_EPI_TOUTES) &&
          !f %in% HSTAT_EPI_MULTI_FIG)
        n <- c(n, list(trf("Cette figure ne porte qu'une variable à la fois : elle montre « %s ».",
                           names(rv$res$resultats)[1] %||% "")))
      # DEUX FIGURES, DEUX PHRASES, PARCE QUE LEURS COMPORTEMENTS SONT OPPOSES.
      # Une seule liste les portait toutes les deux, et la phrase disait « une
      # carte » sur les coupes -- ou les courbes se superposent bel et bien.
      # Un message qui annonce le contraire de ce que la figure montre est le
      # defaut que ce depot traque partout ailleurs.
      tt <- identical(input$epiFigExpo %||% "", HSTAT_EPI_TOUTES)
      if (tt && f %in% HSTAT_EPI_MULTI_FACETTES)
        n <- c(n, list(tr("Cette figure ne se superpose pas : les variables sont mises en facettes, chacune avec sa propre échelle.")))
      if (tt && f %in% HSTAT_EPI_MULTI_PCT)
        n <- c(n, list(tr("Les variables sont superposées sur un axe en percentiles — sans dimension, donc comparable — et les panneaux portent le retard.")))
      if (!length(n)) return(NULL)
      shiny::tags$small(style = "color:#6b7280;",
        do.call(shiny::tagList, lapply(n, function(x) shiny::div(x))))
    })

    hstat_export_plot_handler(input, "epiP", figure, fname = "epidemiologie")
    hstat_export_tables_handlers(output, "epiT", tables, "epidemiologie",
                                 tr("épidémiologie"))
  })
}
