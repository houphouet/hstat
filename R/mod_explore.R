#  Module Shiny : Exploration des donnees


mod_explore_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
              .hstat_scope_banner(exact = FALSE),
              shiny::fluidRow(
                shinydashboard::box(
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  background = "light-blue",
                  shiny::h3(shiny::icon("chart-line"), "Exploration des Données", style = "margin: 0; color: white;"),
                  shiny::p("Analysez la structure, les corrélations et les distributions de vos données", 
                    style = "margin: 5px 0 0 0; color: white; opacity: 0.9;")
                )
              ),
              
              shiny::fluidRow(
                shinydashboard::box(
                  title = shiny::tagList(shiny::icon("database"), "Structure des données"), 
                  status = "info", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DT::DTOutput(ns("dataStructure")),
                  footer = shiny::div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    shiny::icon("info-circle"), 
                    " Visualisez les types de variables et leur structure"
                  )
                ),
                shinydashboard::box(
                  title = shiny::tagList(shiny::icon("calculator"), "Résumé statistique"), 
                  status = "info", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  shiny::div(style = "overflow-x:auto;",
                    shiny::verbatimTextOutput(ns("dataSummary"))),
                  footer = shiny::div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    shiny::icon("info-circle"), 
                    " Statistiques descriptives pour chaque variable"
                  )
                )
              ),
              
              
              shiny::fluidRow(
                shinydashboard::box(
                  title = shiny::tagList(shiny::icon("chart-area"), "Distribution des Variables"), 
                  status = "primary", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  shiny::uiOutput(ns("distVarSelect")),
                  
                  shiny::tags$div(
                    class = "panel-group",
                    id = "distOptionsAccordion",
                    style = "margin-top: 15px;",
                    
                    shiny::tags$div(
                      class = "panel panel-default",
                      shiny::tags$div(
                        class = "panel-heading",
                        shiny::tags$h4(
                          class = "panel-title",
                          shiny::tags$a(
                            `data-toggle` = "collapse",
                            `data-parent` = "#distOptionsAccordion",
                            href = "#distOptionsCollapse",
                            style = "text-decoration: none;",
                            shiny::icon("cog"), " Options graphiques",
                            shiny::tags$span(class = "pull-right", shiny::icon("chevron-down"))
                          )
                        )
                      ),
                      shiny::tags$div(
                        id = "distOptionsCollapse",
                        class = "panel-collapse collapse",
                        shiny::tags$div(
                          class = "panel-body",
                          style = "background-color: #f8f9fa;",
                          
                          shiny::fluidRow(
                            shiny::column(6,
                                   shiny::h5(shiny::icon("text-height"), "Tailles", style = "color: #3498db; font-weight: bold;"),
                                   shiny::sliderInput(ns("distTitleSize"), "Taille titre :", 
                                               min = 8, max = 24, value = 14, ticks = FALSE),
                                   shiny::sliderInput(ns("distAxisTitleSize"), "Taille titres axes :", 
                                               min = 8, max = 20, value = 12, ticks = FALSE),
                                   shiny::sliderInput(ns("distAxisTextSize"), "Taille texte axes :", 
                                               min = 6, max = 16, value = 10, ticks = FALSE),
                                   hstat_axe_titre_ui(ns, "dist"),
                                   shiny::sliderInput(ns("distLegendTextSize"), "Taille texte légende :",
                                               min = 6, max = 16, value = 10, ticks = FALSE)
                            ),
                            shiny::column(6,
                                   shiny::h5(shiny::icon("heading"), "Personnalisation", style = "color: #3498db; font-weight: bold;"),
                                   shiny::textInput(ns("distTitle"), "Titre personnalisé :", 
                                             placeholder = "Laisser vide pour titre auto"),
                                   shiny::checkboxInput(ns("distCenterTitle"), 
                                                 shiny::tagList(shiny::icon("align-center"), " Centrer le titre"), 
                                                 value = TRUE),
                                   shiny::checkboxInput(ns("distShowDensity"), 
                                                 shiny::tagList(shiny::icon("wave-square"), " Afficher courbe densité"), 
                                                 value = TRUE),
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  shiny::hr(),
                  
                  hstat_plot_extras_ui(ns, "distPl"),

                  hstat_export_plot_ui(ns, "distPl", width = 10, height = 8),
                  
                  shiny::div(
                    style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    withSpinner(
                      shiny::plotOutput(ns("distPlot"), height = "500px"),
                      type = 6,
                      color = "#3498db"
                    )
                  ),
                  
                  footer = shiny::div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    shiny::icon("info-circle"), 
                    " Analysez la normalité et la dispersion de vos variables"
                  )
                ),
                
                shinydashboard::box(
                  title = shiny::tagList(shiny::icon("exclamation-triangle"), "Analyse des Valeurs Manquantes"), 
                  status = "warning", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  shiny::tags$div(
                    class = "panel-group",
                    id = "missingOptionsAccordion",
                    
                    shiny::tags$div(
                      class = "panel panel-default",
                      shiny::tags$div(
                        class = "panel-heading",
                        shiny::tags$h4(
                          class = "panel-title",
                          shiny::tags$a(
                            `data-toggle` = "collapse",
                            `data-parent` = "#missingOptionsAccordion",
                            href = "#missingOptionsCollapse",
                            style = "text-decoration: none;",
                            shiny::icon("cog"), " Options graphiques",
                            shiny::tags$span(class = "pull-right", shiny::icon("chevron-down"))
                          )
                        )
                      ),
                      shiny::tags$div(
                        id = "missingOptionsCollapse",
                        class = "panel-collapse collapse",
                        shiny::tags$div(
                          class = "panel-body",
                          style = "background-color: #fff8e1;",
                          
                          shiny::fluidRow(
                            shiny::column(4,
                                   shiny::h5(shiny::icon("text-height"), "Tailles", style = "color: #f39c12; font-weight: bold;"),
                                   shiny::sliderInput(ns("missingTitleSize"), "Taille titre :", 
                                               min = 8, max = 24, value = 14, ticks = FALSE),
                                   shiny::sliderInput(ns("missingAxisTitleSize"), "Taille titres axes :", 
                                               min = 8, max = 20, value = 12, ticks = FALSE)
                            ),
                            shiny::column(4,
                                   shiny::h5(shiny::icon("palette"), "Affichage", style = "color: #f39c12; font-weight: bold;"),
                                   shiny::sliderInput(ns("missingAxisTextSize"), "Taille texte axes :", 
                                               min = 6, max = 16, value = 10, ticks = FALSE),
                                   hstat_axe_titre_ui(ns, "miss"),
                                   shiny::checkboxInput(ns("missingRotateLabels"), 
                                                 shiny::tagList(shiny::icon("sync-alt"), " Incliner labels X"), 
                                                 value = TRUE)
                            ),
                            shiny::column(4,
                                   shiny::h5(shiny::icon("heading"), "Personnalisation", style = "color: #f39c12; font-weight: bold;"),
                                   shiny::textInput(ns("missingTitle"), "Titre personnalisé :", 
                                             placeholder = "Laisser vide pour titre auto"),
                                   shiny::checkboxInput(ns("missingCenterTitle"), 
                                                 shiny::tagList(shiny::icon("align-center"), " Centrer le titre"), 
                                                 value = TRUE),
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  shiny::hr(),
                  
                  hstat_plot_extras_ui(ns, "missPl"),

                  hstat_export_plot_ui(ns, "missPl", width = 12, height = 8),
                  
                  shiny::div(
                    style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    withSpinner(
                      shiny::plotOutput(ns("missingPlot"), height = "400px"),
                      type = 6,
                      color = "#f39c12"
                    )
                  ),
                  
                  footer = shiny::div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    shiny::icon("info-circle"), 
                    " Identifiez les variables nécessitant un traitement"
                  )
                )
              )
  )
}

mod_explore_server <- function(id, values) {
  # Depot du diagnostic de qualite pour l'aide a la decision. L'exploration ne
  # produit pas de test : ce qu'elle a d'exploitable, ce sont les constats de
  # qualite et les suggestions concretes qui en decoulent.
  shiny::observeEvent(values$data, {
    d <- values$data
    if (is.null(d) || !NROW(d)) return()
    dq <- tryCatch(hstat_data_quality(d), error = function(e) NULL)
    hstat_ai_capture(values, "Exploration",
      "Structure et qualité du jeu de données",
      tables = list("Diagnostic de qualité" = dq),
      meta = list(variables = names(d),
                  observations = NROW(d), colonnes = NCOL(d)))
  }, ignoreInit = FALSE)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  # ---- Exploration ----
  
  output$dataStructure <- DT::renderDT({
    shiny::req(values$data)
    df <- values$data
    n  <- nrow(df)
    structure_df <- do.call(rbind, lapply(names(df), function(nm) {
      x <- df[[nm]]
      cls <- class(x)[1]
      type_fr <- if (is.numeric(x)) "Numérique"
                 else if (is.factor(x)) "Facteur"
                 else if (is.logical(x)) "Logique"
                 else if (inherits(x, c("Date", "POSIXt"))) "Date/Heure"
                 else "Texte"
      n_na      <- sum(is.na(x))
      n_distinct <- length(unique(x[!is.na(x)]))
      ex_vals <- utils::head(x[!is.na(x)], 3)
      ex_str  <- if (length(ex_vals) == 0) "—"
                 else paste(format(ex_vals, trim = TRUE, digits = 4), collapse = ", ")
      data.frame(
        Variable    = nm,
        Type        = type_fr,
        Classe_R    = cls,
        Valeurs_distinctes = n_distinct,
        Manquants   = n_na,
        `Manquants_%` = if (n > 0) round(100 * n_na / n, 1) else 0,
        Exemples    = ex_str,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
    DT::datatable(
      structure_df, rownames = FALSE, filter = "top",
      extensions = "Buttons",
      options = list(pageLength = 10, scrollX = TRUE, dom = "Bfrtip",
                     buttons = .hstat_dt_buttons("structure_données")),
      caption = htmltools::tags$caption(
        style = "caption-side: top; font-weight: 600;",
        trf("Structure : %d variables, %d observations", ncol(df), n))
    )
  })

  output$dataSummary <- shiny::renderPrint({
    shiny::req(values$data)
    df <- as.data.frame(values$data)
    # Conversion texte/labelled/logique -> facteur pour obtenir le decompte des
    # modalites dans summary() (et non Length/Class/Mode).
    df[] <- lapply(df, function(x) {
      if (is.character(x) || inherits(x, "haven_labelled") ||
          inherits(x, "labelled") || is.logical(x)) factor(x) else x
    })
    # Affichage NATIF de summary() en grille multi-colonnes (plusieurs variables
    # cote a cote, occupant la largeur), par paquets de quelques variables, avec
    # une ligne vide entre les paquets -- exactement comme R l'affiche en console.
    per_block <- 6L
    ncols <- ncol(df)
    idx <- seq_len(ncols)
    blocks <- split(idx, ceiling(idx / per_block))
    for (b in blocks) {
      print(summary(df[, b, drop = FALSE]))
      cat("\n\n")
    }
  })
  
  output$distVarSelect <- shiny::renderUI({
    shiny::req(values$data)
    num_cols <- unique(names(values$data)[sapply(values$data, is.numeric)])

    if (length(num_cols) == 0) {
      return(shiny::div(class = "alert alert-warning", 
                 shiny::icon("exclamation-triangle"), 
                 " Aucune variable numérique disponible"))
    }
    
    shiny::selectInput(ns("distVar"), "Sélectionnez une variable :", 
                choices = num_cols, selected = num_cols[1])
  })
  
  generate_dist_plot <- function(data, var, show_density = TRUE, title = NULL, 
                                 center_title = TRUE, title_size = 14, 
                                 axis_title_size = 12, axis_text_size = 10,
                                 legend_text_size = 10,
                                 titre_retour = TRUE, titre_align = "0.5") {
    
    plot_title <- if (!is.null(title) && title != "") {
      title
    } else {
      trf("Distribution de %s", var)
    }

    # On extrait UNIQUEMENT la variable a tracer dans un data.frame propre. Cela
    # evite l'erreur "data must be uniquely named but has duplicate columns" quand
    # le jeu de donnees contient des colonnes en double (p. ex. apres une fusion ou
    # un CSV aux en-tetes dupliques), et garantit un nom de colonne unique.
    if (!var %in% names(data))
      stop(trf("La variable « %s » est introuvable.", var))
    col_idx <- which(names(data) == var)[1]      # 1re colonne portant ce nom
    xv <- data[[col_idx]]
    if (!is.numeric(xv))
      stop(trf("La variable « %s » n'est pas numérique : l'histogramme requiert une variable numérique.", var))
    pdata <- data.frame(.x = xv)
    pdata <- pdata[!is.na(pdata$.x), , drop = FALSE]
    if (nrow(pdata) == 0)
      stop(trf("La variable « %s » ne contient aucune valeur numérique valide.", var))

    p <- ggplot2::ggplot(pdata, ggplot2::aes(x = .x)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), fill = "lightblue",
                     color = "black", alpha = 0.7, bins = 30)

    if (show_density) {
      p <- p + ggplot2::geom_density(color = "red", linewidth = 1.2)
    }
    
    extras <- hstat_plot_extras_lire(input, "distPl")
    p <- p + (hstat_export_theme(input, "distPl", base_size = extras$police) %||%
                ggplot2::theme_minimal(base_size = extras$police)) +
      ggplot2::labs(title = plot_title, x = var, y = "Densité") +
      ggplot2::theme(
        plot.title = ggtext::element_markdown(size = title_size, hjust = if (center_title) 0.5 else 0),
        axis.title = hstat_axe_titre(axis_title_size, "plain", titre_align,
                                     "x", retour = titre_retour),
        axis.title.y = hstat_axe_titre(axis_title_size, "plain", titre_align,
                                       "y", retour = titre_retour),
        axis.text = ggplot2::element_text(size = axis_text_size),
        legend.text = ggplot2::element_text(size = legend_text_size),
        legend.title = ggtext::element_markdown(size = legend_text_size)
      )

    # LE KIT SE POSE EN DERNIER : un theme complet remplace tout ce qui
    # precede, si bien que pose avant celui du module il serait efface.
    return(p + hstat_plot_extras_theme(extras))
  }
  
  distParams <- shiny::reactive({
    list(
      show_density = if (is.null(input$distShowDensity)) TRUE else input$distShowDensity,
      title = input$distTitle,
      center_title = if (is.null(input$distCenterTitle)) TRUE else input$distCenterTitle,
      title_size = input$distTitleSize %||% 14,
      axis_title_size = input$distAxisTitleSize %||% 12,
      axis_text_size = input$distAxisTextSize %||% 10,
      legend_text_size = input$distLegendTextSize %||% 10,
      titre_retour = isTRUE(input$distTitreRetour %||% TRUE),
      titre_align = input$distTitreAlign %||% "0.5"
    )
  }) %>% shiny::debounce(500)
  
  # LE GRAPHIQUE N'EST CONSTRUIT QU'UNE FOIS. L'apercu et le telechargement en
  # montaient chacun leur exemplaire, avec les memes huit arguments : deux
  # copies a tenir d'accord, et rien pour signaler qu'elles avaient diverge.
  dist_gg <- shiny::reactive({
    shiny::req(values$data, input$distVar)
    params <- distParams()
    tryCatch({
      generate_dist_plot(
        data = values$data,
        var = input$distVar,
        show_density = params$show_density,
        title = params$title,
        center_title = params$center_title,
        title_size = params$title_size,
        axis_title_size = params$axis_title_size,
        axis_text_size = params$axis_text_size,
        legend_text_size = params$legend_text_size,
        titre_retour = params$titre_retour,
        titre_align = params$titre_align
      )
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur lors de la création du graphique"),
                       type = "error", duration = 5)
      NULL
    })
  })

  output$distPlot <- shiny::renderPlot({ dist_gg() })

  # L'export passe par le kit partage : format, dimensions, DPI et bouton sont
  # ceux de toute l'application, et le module n'ecrit plus une ligne de fichier.
  output$distPlDl <- hstat_export_plot_handler(input, "distPl",
                       function() dist_gg(), "distribution")

  generate_missing_data <- function(data) {
    missing_counts <- sapply(data, function(x) sum(is.na(x)))
    
    missing_data <- data.frame(
      Variable = names(missing_counts),
      Missing = as.numeric(missing_counts),
      stringsAsFactors = FALSE
    )
    
    missing_data$PctMissing <- (missing_data$Missing / nrow(data)) * 100
    
    return(missing_data)
  }
  
  # Fonction réutilisable pour générer le plot des valeurs manquantes
  generate_missing_plot <- function(data, title = NULL, center_title = TRUE,
                                    title_size = 14, axis_title_size = 12,
                                    titre_retour = TRUE, titre_align = "0.5",
                                    axis_text_size = 10, rotate_labels = TRUE) {
    
    missing_data <- generate_missing_data(data)
    
    plot_title <- if (!is.null(title) && title != "") {
      title
    } else {
      "Analyse des valeurs manquantes"
    }
    
    extras <- hstat_plot_extras_lire(input, "missPl")
    p <- ggplot2::ggplot(missing_data, ggplot2::aes(x = stats::reorder(Variable, -Missing), y = Missing)) +
      ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(round(PctMissing, 1), "%")), 
                vjust = -0.5, size = 3.5) +
      (hstat_export_theme(input, "missPl", base_size = extras$police) %||%
         ggplot2::theme_minimal(base_size = extras$police)) +
      ggplot2::labs(title = plot_title, x = "Variable", y = "Nombre de valeurs manquantes") +
      ggplot2::theme(
        plot.title = ggtext::element_markdown(size = title_size, hjust = if (center_title) 0.5 else 0),
        axis.title = hstat_axe_titre(axis_title_size, "plain", titre_align,
                                     "x", retour = titre_retour),
        axis.title.y = hstat_axe_titre(axis_title_size, "plain", titre_align,
                                       "y", retour = titre_retour),
        axis.text = ggplot2::element_text(size = axis_text_size),
        axis.text.x = ggplot2::element_text(
          angle = if (rotate_labels) 45 else 0, 
          hjust = if (rotate_labels) 1 else 0.5
        )
      )

    # LE KIT SE POSE EN DERNIER : un theme complet remplace tout ce qui
    # precede, si bien que pose avant celui du module il serait efface.
    return(p + hstat_plot_extras_theme(extras))
  }
  
  missingParams <- shiny::reactive({
    list(
      title = input$missingTitle,
      center_title = if (is.null(input$missingCenterTitle)) TRUE else input$missingCenterTitle,
      title_size = input$missingTitleSize %||% 14,
      axis_title_size = input$missingAxisTitleSize %||% 12,
      axis_text_size = input$missingAxisTextSize %||% 10,
      rotate_labels = if (is.null(input$missingRotateLabels)) TRUE else input$missingRotateLabels,
      titre_retour = isTRUE(input$missTitreRetour %||% TRUE),
      titre_align = input$missTitreAlign %||% "0.5"
    )
  }) %>% shiny::debounce(500)
  
  missing_gg <- shiny::reactive({
    shiny::req(values$data)
    params <- missingParams()
    tryCatch({
      generate_missing_plot(
        data = values$data,
        title = params$title,
        center_title = params$center_title,
        title_size = params$title_size,
        axis_title_size = params$axis_title_size,
        axis_text_size = params$axis_text_size,
        rotate_labels = params$rotate_labels,
        titre_retour = params$titre_retour,
        titre_align = params$titre_align
      )
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur lors de la création du graphique"),
                       type = "error", duration = 5)
      NULL
    })
  })

  output$missingPlot <- shiny::renderPlot({ missing_gg() })

  output$missPlDl <- hstat_export_plot_handler(input, "missPl",
                       function() missing_gg(), "valeurs_manquantes")
  })
}
