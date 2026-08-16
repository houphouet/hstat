#  Module Shiny : Exploration des donnees


mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
              .hstat_scope_banner(exact = FALSE),
              fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  background = "light-blue",
                  h3(icon("chart-line"), "Exploration des Données", style = "margin: 0; color: white;"),
                  p("Analysez la structure, les corrélations et les distributions de vos données", 
                    style = "margin: 5px 0 0 0; color: white; opacity: 0.9;")
                )
              ),
              
              fluidRow(
                box(
                  title = tagList(icon("database"), "Structure des données"), 
                  status = "info", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DT::DTOutput(ns("dataStructure")),
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("info-circle"), 
                    " Visualisez les types de variables et leur structure"
                  )
                ),
                box(
                  title = tagList(icon("calculator"), "Résumé statistique"), 
                  status = "info", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  div(style = "overflow-x:auto;",
                    verbatimTextOutput(ns("dataSummary"))),
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("info-circle"), 
                    " Statistiques descriptives pour chaque variable"
                  )
                )
              ),
              
              
              fluidRow(
                box(
                  title = tagList(icon("chart-area"), "Distribution des Variables"), 
                  status = "primary", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  uiOutput(ns("distVarSelect")),
                  
                  tags$div(
                    class = "panel-group",
                    id = "distOptionsAccordion",
                    style = "margin-top: 15px;",
                    
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h4(
                          class = "panel-title",
                          tags$a(
                            `data-toggle` = "collapse",
                            `data-parent` = "#distOptionsAccordion",
                            href = "#distOptionsCollapse",
                            style = "text-decoration: none;",
                            icon("cog"), " Options graphiques",
                            tags$span(class = "pull-right", icon("chevron-down"))
                          )
                        )
                      ),
                      tags$div(
                        id = "distOptionsCollapse",
                        class = "panel-collapse collapse",
                        tags$div(
                          class = "panel-body",
                          style = "background-color: #f8f9fa;",
                          
                          fluidRow(
                            column(6,
                                   h5(icon("text-height"), "Tailles", style = "color: #3498db; font-weight: bold;"),
                                   sliderInput(ns("distTitleSize"), "Taille titre:", 
                                               min = 8, max = 24, value = 14, ticks = FALSE),
                                   sliderInput(ns("distAxisTitleSize"), "Taille titres axes:", 
                                               min = 8, max = 20, value = 12, ticks = FALSE),
                                   sliderInput(ns("distAxisTextSize"), "Taille texte axes:", 
                                               min = 6, max = 16, value = 10, ticks = FALSE),
                                   sliderInput(ns("distLegendTextSize"), "Taille texte légende:",
                                               min = 6, max = 16, value = 10, ticks = FALSE)
                            ),
                            column(6,
                                   h5(icon("heading"), "Personnalisation", style = "color: #3498db; font-weight: bold;"),
                                   textInput(ns("distTitle"), "Titre personnalisé:", 
                                             placeholder = "Laisser vide pour titre auto"),
                                   checkboxInput(ns("distCenterTitle"), 
                                                 tagList(icon("align-center"), " Centrer le titre"), 
                                                 value = TRUE),
                                   checkboxInput(ns("distShowDensity"), 
                                                 tagList(icon("wave-square"), " Afficher courbe densité"), 
                                                 value = TRUE),
                                   numericInput(ns("distDPI"), tagList(icon("image"), " DPI export:"), 
                                                value = 300, min = 72, max = HSTAT_DPI_MAX, step = 50)
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  hr(),
                  
                  div(
                    style = "text-align: center; margin: 15px 0;",
                    downloadButton(ns("downloadDistPlot"), 
                                   tagList(icon("download"), " Télécharger PNG"), 
                                   class = "btn-info")
                  ),
                  
                  div(
                    style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    withSpinner(
                      plotOutput(ns("distPlot"), height = "500px"),
                      type = 6,
                      color = "#3498db"
                    )
                  ),
                  
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("info-circle"), 
                    " Analysez la normalité et la dispersion de vos variables"
                  )
                ),
                
                box(
                  title = tagList(icon("exclamation-triangle"), "Analyse des Valeurs Manquantes"), 
                  status = "warning", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  tags$div(
                    class = "panel-group",
                    id = "missingOptionsAccordion",
                    
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h4(
                          class = "panel-title",
                          tags$a(
                            `data-toggle` = "collapse",
                            `data-parent` = "#missingOptionsAccordion",
                            href = "#missingOptionsCollapse",
                            style = "text-decoration: none;",
                            icon("cog"), " Options graphiques",
                            tags$span(class = "pull-right", icon("chevron-down"))
                          )
                        )
                      ),
                      tags$div(
                        id = "missingOptionsCollapse",
                        class = "panel-collapse collapse",
                        tags$div(
                          class = "panel-body",
                          style = "background-color: #fff8e1;",
                          
                          fluidRow(
                            column(4,
                                   h5(icon("text-height"), "Tailles", style = "color: #f39c12; font-weight: bold;"),
                                   sliderInput(ns("missingTitleSize"), "Taille titre:", 
                                               min = 8, max = 24, value = 14, ticks = FALSE),
                                   sliderInput(ns("missingAxisTitleSize"), "Taille titres axes:", 
                                               min = 8, max = 20, value = 12, ticks = FALSE)
                            ),
                            column(4,
                                   h5(icon("palette"), "Affichage", style = "color: #f39c12; font-weight: bold;"),
                                   sliderInput(ns("missingAxisTextSize"), "Taille texte axes:", 
                                               min = 6, max = 16, value = 10, ticks = FALSE),
                                   checkboxInput(ns("missingRotateLabels"), 
                                                 tagList(icon("sync-alt"), " Incliner labels X"), 
                                                 value = TRUE)
                            ),
                            column(4,
                                   h5(icon("heading"), "Personnalisation", style = "color: #f39c12; font-weight: bold;"),
                                   textInput(ns("missingTitle"), "Titre personnalisé:", 
                                             placeholder = "Laisser vide pour titre auto"),
                                   checkboxInput(ns("missingCenterTitle"), 
                                                 tagList(icon("align-center"), " Centrer le titre"), 
                                                 value = TRUE),
                                   numericInput(ns("missingDPI"), tagList(icon("image"), " DPI export:"), 
                                                value = 300, min = 72, max = HSTAT_DPI_MAX, step = 50)
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  hr(),
                  
                  div(
                    style = "text-align: center; margin: 15px 0;",
                    downloadButton(ns("downloadMissingPlot"), 
                                   tagList(icon("download"), " Télécharger PNG"), 
                                   class = "btn-info")
                  ),
                  
                  div(
                    style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    withSpinner(
                      plotOutput(ns("missingPlot"), height = "400px"),
                      type = 6,
                      color = "#f39c12"
                    )
                  ),
                  
                  footer = div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    icon("info-circle"), 
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
      "Structure et qualite du jeu de donnees",
      tables = list("Diagnostic de qualite" = dq),
      meta = list(variables = names(d),
                  observations = NROW(d), colonnes = NCOL(d)))
  }, ignoreInit = FALSE)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  # ---- Exploration ----
  
  output$dataStructure <- DT::renderDT({
    req(values$data)
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

  output$dataSummary <- renderPrint({
    req(values$data)
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
  
  output$distVarSelect <- renderUI({
    req(values$data)
    num_cols <- unique(names(values$data)[sapply(values$data, is.numeric)])

    if (length(num_cols) == 0) {
      return(div(class = "alert alert-warning", 
                 icon("exclamation-triangle"), 
                 " Aucune variable numérique disponible"))
    }
    
    selectInput(ns("distVar"), "Sélectionnez une variable:", 
                choices = num_cols, selected = num_cols[1])
  })
  
  generate_dist_plot <- function(data, var, show_density = TRUE, title = NULL, 
                                 center_title = TRUE, title_size = 14, 
                                 axis_title_size = 12, axis_text_size = 10,
                                 legend_text_size = 10) {
    
    plot_title <- if (!is.null(title) && title != "") {
      title
    } else {
      paste("Distribution de", var)
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

    p <- ggplot(pdata, aes(x = .x)) +
      geom_histogram(aes(y = after_stat(density)), fill = "lightblue",
                     color = "black", alpha = 0.7, bins = 30)

    if (show_density) {
      p <- p + geom_density(color = "red", linewidth = 1.2)
    }
    
    p <- p + theme_minimal() +
      labs(title = plot_title, x = var, y = "Densité") +
      theme(
        plot.title = element_markdown(size = title_size, hjust = if (center_title) 0.5 else 0),
        axis.title = element_markdown(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        legend.text = element_text(size = legend_text_size),
        legend.title = element_markdown(size = legend_text_size)
      )
    
    return(p)
  }
  
  distParams <- reactive({
    list(
      show_density = if (is.null(input$distShowDensity)) TRUE else input$distShowDensity,
      title = input$distTitle,
      center_title = if (is.null(input$distCenterTitle)) TRUE else input$distCenterTitle,
      title_size = input$distTitleSize %||% 14,
      axis_title_size = input$distAxisTitleSize %||% 12,
      axis_text_size = input$distAxisTextSize %||% 10,
      legend_text_size = input$distLegendTextSize %||% 10
    )
  }) %>% debounce(500)
  
  output$distPlot <- renderPlot({
    req(values$data, input$distVar)
    
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
        legend_text_size = params$legend_text_size
      )
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur lors de la création du graphique"), 
                       type = "error", duration = 5)
      return(NULL)
    })
  })
  
  output$downloadDistPlot <- downloadHandler(
    filename = function() {
      paste0("distribution_", input$distVar, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$data, input$distVar)
      
      params <- distParams()
      dpi <- input$distDPI %||% 300
      
      p <- tryCatch({
        generate_dist_plot(
          data = values$data,
          var = input$distVar,
          show_density = params$show_density,
          title = params$title,
          center_title = params$center_title,
          title_size = params$title_size,
          axis_title_size = params$axis_title_size,
          axis_text_size = params$axis_text_size,
          legend_text_size = params$legend_text_size
        )
      }, error = function(e) {
        showNotification(hstat_err_fr(e, "Erreur téléchargement"), type = "error")
        return(NULL)
      })
      
      # Un fichier toujours valide, meme quand le graphique a echoue : sans
      # cela Shiny renvoie sa page d'erreur HTML sous le nom « .png ».
      if (hstat_ecrire_image(file, p, "png", 10, 8, dpi))
        showNotification("Graphique téléchargé avec succès!", type = "message", duration = 3)
      else
        showNotification("Graphique indisponible : le fichier téléchargé porte le motif.",
                         type = "error", duration = 6)
    }
  )
  
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
                                    axis_text_size = 10, rotate_labels = TRUE) {
    
    missing_data <- generate_missing_data(data)
    
    plot_title <- if (!is.null(title) && title != "") {
      title
    } else {
      "Analyse des valeurs manquantes"
    }
    
    p <- ggplot(missing_data, aes(x = reorder(Variable, -Missing), y = Missing)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = paste0(round(PctMissing, 1), "%")), 
                vjust = -0.5, size = 3.5) +
      theme_minimal() +
      labs(title = plot_title, x = "Variable", y = "Nombre de valeurs manquantes") +
      theme(
        plot.title = element_markdown(size = title_size, hjust = if (center_title) 0.5 else 0),
        axis.title = element_markdown(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        axis.text.x = element_text(
          angle = if (rotate_labels) 45 else 0, 
          hjust = if (rotate_labels) 1 else 0.5
        )
      )
    
    return(p)
  }
  
  missingParams <- reactive({
    list(
      title = input$missingTitle,
      center_title = if (is.null(input$missingCenterTitle)) TRUE else input$missingCenterTitle,
      title_size = input$missingTitleSize %||% 14,
      axis_title_size = input$missingAxisTitleSize %||% 12,
      axis_text_size = input$missingAxisTextSize %||% 10,
      rotate_labels = if (is.null(input$missingRotateLabels)) TRUE else input$missingRotateLabels
    )
  }) %>% debounce(500)
  
  output$missingPlot <- renderPlot({
    req(values$data)
    
    params <- missingParams()
    
    tryCatch({
      generate_missing_plot(
        data = values$data,
        title = params$title,
        center_title = params$center_title,
        title_size = params$title_size,
        axis_title_size = params$axis_title_size,
        axis_text_size = params$axis_text_size,
        rotate_labels = params$rotate_labels
      )
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur lors de la création du graphique"), 
                       type = "error", duration = 5)
      return(NULL)
    })
  })
  
  output$downloadMissingPlot <- downloadHandler(
    filename = function() {
      paste0("valeurs_manquantes_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$data)
      
      params <- missingParams()
      dpi <- input$missingDPI %||% 300
      
      p <- tryCatch({
        generate_missing_plot(
          data = values$data,
          title = params$title,
          center_title = params$center_title,
          title_size = params$title_size,
          axis_title_size = params$axis_title_size,
          axis_text_size = params$axis_text_size,
          rotate_labels = params$rotate_labels
        )
      }, error = function(e) {
        showNotification(hstat_err_fr(e, "Erreur téléchargement"), type = "error")
        return(NULL)
      })
      
      if (hstat_ecrire_image(file, p, "png", 12, 8, dpi))
        showNotification("Graphique téléchargé avec succès!", type = "message", duration = 3)
      else
        showNotification("Graphique indisponible : le fichier téléchargé porte le motif.",
                         type = "error", duration = 6)
    }
  )
  })
}
