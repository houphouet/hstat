# ===========================================================================
#  MODULE EPIDEMIOLOGIE
# ===========================================================================
#  Le module ne CALCULE rien : tout vit dans `R/utils.R` (`hstat_epi_*`), ou
#  c'est testable. Ici on choisit, on affiche et on met en forme -- la regle du
#  depot, et elle a une raison propre a cette discipline : un rapport de risque
#  n'est jamais faux bruyamment.

# Les familles du kit de mise en forme que ce module prend. Il ne porte AUCUN
# reglage de police en propre : le kit les fournit toutes les quatre.
HSTAT_EPI_EXTRAS <- c("police", "axe", "cles", "marges")

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
        shiny::conditionalPanel(
          ns = ns,
          condition = sprintf("output.hasEpi && ['%s'].indexOf(input.epiFigure) < 0",
                              paste(HSTAT_EPI_FIGURES_BASE, collapse = "','")),
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("paint-roller"), " Mise en forme générale"),
            status = "primary", solidHeader = TRUE, width = 12,
            collapsible = TRUE, collapsed = TRUE,
            hstat_plot_extras_ui(ns, "epiX", familles = HSTAT_EPI_EXTRAS)))
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
            hstat_export_plot_ui(ns, "epiP", width = 10, height = 6),
            shiny::hr(),
            shiny::h5(tr("Tableaux")),
            shiny::fluidRow(
              shiny::column(6, shiny::downloadButton(ns("epiTXlsx"), "Excel (.xlsx)",
                class = "btn-success btn-block")),
              shiny::column(6, shiny::downloadButton(ns("epiTCsv"), "CSV (.zip)",
                class = "btn-info btn-block")))))
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
            shiny::selectInput(ns("epiY"), "Issue (décompte d'événements)", choices = n),
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
            shiny::selectInput(ns("epiOffset"),
              "Dénominateur (population exposée, facultatif)",
              choices = c("(aucun)" = "", n)),
            shiny::selectInput(ns("epiTemps"), "Colonne de date (facultatif)",
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
            shiny::selectInput(ns("epiAjust"), "Covariables simples (facultatif)",
              choices = a, multiple = TRUE)),
          .hstat_opt_section("Structure du retard", "hourglass-half", "#2980b9", "#eaf4fb",
            shiny::numericInput(ns("epiLag"), "Décalage maximal", value = 5,
                                min = 0, max = 60, step = 1),
            shiny::numericInput(ns("epiNkLag"), "Nœuds sur l'axe des retards",
                                value = 2, min = 1, max = 5, step = 1),
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

      res <- tryCatch(switch(an,
        dlnm = hstat_epi_dlnm_multi(df, input$epiY, vide(input$epiExpo),
          mutuel = isTRUE(input$epiMutuel), var_offset = input$epiOffset,
          var_temps = input$epiTemps, var_mois = input$epiMois,
          var_annee = input$epiAnnee, vars_ajust = vide(input$epiAjust),
          lag_max = hstat_finite(input$epiLag, 5),
          nk_lag = hstat_finite(input$epiNkLag, 2),
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
        if (identical(an, "dlnm") && length(rv$res$resultats) > 1L)
          shiny::selectInput(ns("epiFigExpo"), tr("Exposition représentée"),
                             choices = names(rv$res$resultats)))
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
          out <- list(Comparaison = r$comparaison)
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
        w <- tryCatch(hstat_epi_dlnm_wald(courant()), error = function(e) NULL)
        if (!is.null(w))
          l <- paste(l, trf("| test global de la surface : χ² = %.2f (%d ddl), p = %s — %s",
                            w$chi2, w$ddl, format.pval(w$p, digits = 3), w$verdict))
      }
      if (is.null(l)) return(NULL)
      shiny::div(class = "hstat-interpretation", shiny::HTML(hstat_html_escape(l)))
    })

    # ------------------------------------------------------------- GRAPHIQUE
    opts <- shiny::reactive(
      hstat_plot_extras_lire(input, "epiX", familles = HSTAT_EPI_EXTRAS))

    figure <- shiny::reactive({
      r <- courant(); an <- rv$analyse
      if (is.null(r) || is.null(an)) return(NULL)
      f <- input$epiFigure %||% names(HSTAT_EPI_FIGURES[[an]])[1]
      o <- opts(); o$theme <- input$epiPTheme %||% "minimal"
      hstat_epi_figure(an, f, r, o)
    })

    output$epiPlot <- shiny::renderPlot({
      p <- figure()
      shiny::validate(shiny::need(!is.null(p),
        tr("Lancez une analyse, puis choisissez une figure.")))
      if (is.function(p)) p() else print(p)
    })

    output$epiPlotNote <- shiny::renderUI({
      if ((input$epiFigure %||% "") %in% HSTAT_EPI_FIGURES_BASE)
        shiny::tags$small(style = "color:#6b7280;",
          tr("Cette figure est tracée en graphiques de base : le panneau « Mise en forme générale » ne s'y applique pas, il est donc retiré."))
    })

    hstat_export_plot_handler(input, "epiP", figure, fname = "epidemiologie")
    hstat_export_tables_handlers(output, "epiT", tables, "epidemiologie",
                                 tr("épidémiologie"))
  })
}
