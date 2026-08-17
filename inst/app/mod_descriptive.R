#  Module Shiny : Analyses descriptives


mod_descriptive_ui <- function(id) {
  ns <- NS(id)
  tagList(
              fluidRow(
                box(title = tags$span(icon("chart-bar"), " Sélection des Variables"), 
                    status = "success", width = 4, solidHeader = TRUE,
                    uiOutput(ns("numVarSelect")),
                    checkboxInput(ns("selectAllNum"), 
                                  HTML("<span style='font-weight: 600;'><i class='fa fa-check-square'></i> Sélectionner toutes les variables numériques</span>"), 
                                  FALSE),
                    hr(style = "border-color: #ddd; margin: 20px 0;"),
                    tags$div(
                      style = "background-color: #f8f9fa; padding: 12px; border-radius: 6px; border-left: 4px solid #28a745;",
                      tags$h6(icon("calculator"), " Statistiques à calculer", 
                              style = "font-weight: bold; color: #155724; margin-bottom: 12px;"),
                      checkboxGroupInput(ns("descStats"), NULL,
                                         choices = list(
                                           "Moyenne" = "mean", 
                                           "Médiane" = "median", 
                                           "Écart-type" = "sd", 
                                           "Variance" = "var", 
                                           "CV (%)" = "cv", 
                                           "Minimum" = "min", 
                                           "Maximum" = "max", 
                                           "1er Quartile" = "q1", 
                                           "3ème Quartile" = "q3"
                                         ),
                                         selected = c("mean", "median", "sd", "cv")
                      )
                    ),
                    hr(style = "border-color: #ddd; margin: 20px 0;"),
                    uiOutput(ns("descFactorUI")),
                    hr(style = "border-color: #ddd; margin: 15px 0;"),
                    div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; border-radius: 4px;",
                        fluidRow(
                          column(6,
                                 checkboxInput(ns("descRoundResults"), "Arrondir les résultats", value = FALSE)
                          ),
                          column(6,
                                 conditionalPanel(
              ns = ns,
                                   condition = "input.descRoundResults == true",
                                   numericInput(ns("descDecimals"), "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                 )
                          )
                        )
                    ),
                    br(),
                    actionButton(ns("calcDesc"), 
                                 HTML("<i class='fa fa-calculator'></i> Calculer les statistiques"), 
                                 class = "btn-success btn-block btn-lg", 
                                 style = "font-weight: bold; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: all 0.3s;"),
                    # Bouton de calcul sur le jeu COMPLET -- visible uniquement
                    # quand l'application est en mode hors-memoire (DuckDB)
                    conditionalPanel(
              ns = ns,
                      condition = "output.hstatBigData == true",
                      div(style = "margin-top:10px; padding:10px; background:#fff4e5; border:1px solid #ed6c02; border-radius:8px;",
                        tags$p(style = "margin:0 0 8px 0; font-size:12px; color:#7a4a1a;",
                          icon("database"),
                          HTML(" Les statistiques ci-dessus portent sur l'<b>échantillon</b>. Le bouton ci-dessous calculé les valeurs <b>exactes sur le jeu complet</b> (via DuckDB).")),
                        actionButton(ns("calcDescFull"),
                          HTML("<i class='fa fa-server'></i> Calculer sur le jeu complet"),
                          class = "btn-warning btn-block",
                          style = "font-weight:bold;"))
                    )
                ),
                box(title = tags$span(icon("table"), " Résultats des Analyses Descriptives"), 
                    status = "success", width = 8, solidHeader = TRUE,
                    tags$div(
                      style = "background-color: #e8f5e9; padding: 12px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #4caf50;",
                      tags$p(tagList(icon("info-circle"), " Les résultats s'afficheront ici après le calcul. Vous pouvez trier, filtrer et rechercher dans le tableau."),
                             style = "margin: 0; color: #2e7d32; font-size: 14px;")
                    ),
                    DTOutput(ns("descResults")),
                    br(),
                    fluidRow(
                      column(6,
                             downloadButton(ns("downloadDesc"), 
                                            HTML("<i class='fa fa-file-csv'></i> Télécharger CSV"), 
                                            class = "btn-info btn-block", 
                                            style = "font-weight: 600; padding: 10px;")
                      ),
                      column(6,
                             downloadButton(ns("downloadDescExcel"), 
                                            HTML("<i class='fa fa-file-excel'></i> Télécharger Excel"), 
                                            class = "btn-success btn-block", 
                                            style = "font-weight: 600; padding: 10px;")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = tags$span(icon("chart-line"), " Visualisation des Données"), 
                    status = "info", width = 12, solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(
                      column(4,
                             tags$div(
                               style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); padding: 15px; border-radius: 8px; margin-bottom: 15px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                               tags$h5(icon("chart-pie"), " Configuration du graphique", 
                                       style = "margin: 0; font-weight: bold; text-align: center;")
                             ),
                             uiOutput(ns("descPlotVarSelect")),
                             uiOutput(ns("descPlotFactorSelect")),
                             hr(style = "border-color: #ddd;"),
                             
                             conditionalPanel(
              ns = ns,
                               condition = "input.descPlotFactor != 'Aucun'",
                               tags$div(
                                 style = "background-color: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                                 h6(icon("sliders-h"), " Options du Boxplot", 
                                    style = "color: #856404; font-weight: bold; margin-bottom: 12px;"),
                                 checkboxInput(ns("descPlotShowValues"), 
                                               HTML("<span style='font-size: 14px;'><i class='fa fa-list-ol'></i> Afficher les valeurs extrêmes (min/max)</span>"), 
                                               value = FALSE)
                               ),
                               hr(style = "border-color: #ddd;")
                             ),
                             
                             tags$div(
                               style = "background-color: #f3e5f5; padding: 15px; border-radius: 8px; border-left: 4px solid #9c27b0; margin-bottom: 15px;",
                               h6(icon("palette"), " Palette de couleurs", 
                                  style = "color: #6a1b9a; font-weight: bold; margin-bottom: 12px;"),
                               selectInput(ns("descPlotColorPalette"), NULL,
                                           choices = list(
                                             "Par défaut (ggplot2)" = "ggplot2",
                                             "--- Palettes RColorBrewer ---" = "",
                                             "Set1 (Multicolore vif)" = "Set1",
                                             "Set2 (Multicolore doux)" = "Set2",
                                             "Pastel1 (Pastel)" = "Pastel1",
                                             "Dark2 (Foncé)" = "Dark2",
                                             "--- Palette Viridis ---" = "",
                                             "Viridis (Accessible)" = "viridis",
                                             "--- Monochromatique ---" = "",
                                             "Bleu" = "mono_blue",
                                             "Vert" = "mono_green",
                                             "Rouge" = "mono_red",
                                             "Violet" = "mono_purple",
                                             "Orange" = "mono_orange",
                                             "Noir" = "mono_black"
                                           ),
                                           selected = "ggplot2",
                                           width = "100%")
                             ),
                             
                             tags$div(
                               style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #6c757d; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                               h6(icon("expand-arrows-alt"), " Dimensions du Graphique", 
                                  style = "color: #343a40; font-weight: bold; margin-bottom: 12px;"),
                               sliderInput(ns("descPlotWidth"), "Largeur (pixels):", 
                                           min = 400, max = 2000, value = 900, step = 50, width = "100%"),
                               sliderInput(ns("descPlotHeight"), "Hauteur (pixels):", 
                                           min = 400, max = 2000, value = 600, step = 50, width = "100%")
                             ),
                             
                             hstat_export_plot_ui(ns, "descPl", width = 9.8, height = 7.1)
                      ),
                      column(8,
                             wellPanel(
                               style = "background: linear-gradient(to right, #f8f9fa 0%, #e9ecef 100%); border-left: 5px solid #3498db; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                               h5(icon("paint-brush"), " Personnalisation du Graphique", 
                                  style = "font-weight: bold; color: #2c3e50; margin-bottom: 20px; text-align: center; font-size: 18px;"),
                               
                               tags$div(
                                 style = "background-color: white; padding: 20px; border-radius: 10px; margin-bottom: 15px; box-shadow: 0 3px 6px rgba(0,0,0,0.08); border-top: 3px solid #3498db;",
                                 h6(icon("heading"), " Titre du graphique", 
                                    style = "font-weight: bold; color: #495057; margin-bottom: 15px; font-size: 15px;"),
                                 textInput(ns("descPlotTitle"), NULL, 
                                           placeholder = "Ex: Distribution des rendements par traitement...",
                                           width = "100%"),
                                 fluidRow(
                                   column(4,
                                          checkboxInput(ns("descPlotCenterTitle"), 
                                                        HTML("<span style='font-size: 14px;'><i class='fa fa-align-center'></i> Centrer</span>"), 
                                                        value = TRUE)
                                   ),
                                   column(4,
                                          checkboxInput(ns("descPlotTitleBold"), 
                                                        HTML("<span style='font-size: 14px;'><strong>Gras</strong></span>"), 
                                                        value = TRUE)
                                   ),
                                   column(4,
                                          checkboxInput(ns("descPlotTitleItalic"), 
                                                        HTML("<span style='font-size: 14px;'><em>Italique</em></span>"), 
                                                        value = FALSE)
                                   )
                                 )
                               ),
                               
                               tags$div(
                                 style = "background-color: white; padding: 20px; border-radius: 10px; margin-bottom: 15px; box-shadow: 0 3px 6px rgba(0,0,0,0.08); border-top: 3px solid #27ae60;",
                                 h6(icon("arrows-alt-h"), " Labels des axes", 
                                    style = "font-weight: bold; color: #27ae60; margin-bottom: 15px; font-size: 15px;"),
                                 fluidRow(
                                   column(6,
                                          tags$div(
                                            style = "background: linear-gradient(to bottom, #ebf8ff 0%, #bee3f8 100%); padding: 15px; border-radius: 8px; border-left: 4px solid #3498db;",
                                            h6(icon("arrows-alt-h"), " Axe X", 
                                               style = "color: #2c5aa0; font-weight: bold; margin-bottom: 12px;"),
                                            textInput(ns("descPlotXLabel"), NULL, 
                                                      placeholder = "Label horizontal...",
                                                      width = "100%"),
                                            fluidRow(
                                              column(6,
                                                     checkboxInput(ns("descPlotXBold"), 
                                                                   HTML("<small><strong>Gras</strong></small>"), 
                                                                   value = FALSE)
                                              ),
                                              column(6,
                                                     checkboxInput(ns("descPlotXItalic"), 
                                                                   HTML("<small><em>Italique</em></small>"), 
                                                                   value = FALSE)
                                              )
                                            )
                                          )
                                   ),
                                   column(6,
                                          tags$div(
                                            style = "background: linear-gradient(to bottom, #f0fff4 0%, #c6f6d5 100%); padding: 15px; border-radius: 8px; border-left: 4px solid #27ae60;",
                                            h6(icon("arrows-alt-v"), " Axe Y", 
                                               style = "color: #22863a; font-weight: bold; margin-bottom: 12px;"),
                                            textInput(ns("descPlotYLabel"), NULL, 
                                                      placeholder = "Label vertical...",
                                                      width = "100%"),
                                            fluidRow(
                                              column(6,
                                                     checkboxInput(ns("descPlotYBold"), 
                                                                   HTML("<small><strong>Gras</strong></small>"), 
                                                                   value = FALSE)
                                              ),
                                              column(6,
                                                     checkboxInput(ns("descPlotYItalic"), 
                                                                   HTML("<small><em>Italique</em></small>"), 
                                                                   value = FALSE)
                                              )
                                            )
                                          )
                                   )
                                 )
                               ),
                               
                               tags$div(
                                 style = "background-color: white; padding: 20px; border-radius: 10px; box-shadow: 0 3px 6px rgba(0,0,0,0.08); border-top: 3px solid #e74c3c;",
                                 h6(icon("ruler-horizontal"), " Graduations des axes", 
                                    style = "font-weight: bold; color: #e74c3c; margin-bottom: 15px; font-size: 15px;"),
                                 fluidRow(
                                   column(6,
                                          tags$div(
                                            style = "background-color: #f0f8ff; padding: 15px; border-radius: 8px; border: 2px solid #b3d9ff;",
                                            h6(icon("long-arrow-alt-right"), " Axe X", 
                                               style = "color: #3498db; font-weight: bold; margin-bottom: 12px; text-align: center;"),
                                            sliderInput(ns("descPlotXAngle"), "Angle d'inclinaison:", 
                                                        min = 0, max = 90, value = 0, step = 15,
                                                        post = "°", width = "100%"),
                                            tags$hr(style = "margin: 15px 0; border-color: #b3d9ff;"),
                                            h6("Style du texte:", style = "font-size: 13px; color: #555; margin-bottom: 10px; font-weight: bold;"),
                                            fluidRow(
                                              column(6,
                                                     checkboxInput(ns("descPlotXTickBold"), 
                                                                   HTML("<small><strong>Gras</strong></small>"), 
                                                                   value = FALSE)
                                              ),
                                              column(6,
                                                     checkboxInput(ns("descPlotXTickItalic"), 
                                                                   HTML("<small><em>Italique</em></small>"), 
                                                                   value = FALSE)
                                              )
                                            )
                                          )
                                   ),
                                   column(6,
                                          tags$div(
                                            style = "background-color: #f0fff4; padding: 15px; border-radius: 8px; border: 2px solid #b3e6cc;",
                                            h6(icon("long-arrow-alt-up"), " Axe Y", 
                                               style = "color: #27ae60; font-weight: bold; margin-bottom: 12px; text-align: center;"),
                                            tags$div(
                                              style = "height: 78px; display: flex; align-items: center; justify-content: center; background-color: rgba(39,174,96,0.1); border-radius: 6px; color: #27ae60; font-size: 13px; font-style: italic; padding: 10px; text-align: center;", 
                                              "Les graduations Y restent toujours horizontales"
                                            ),
                                            tags$hr(style = "margin: 15px 0; border-color: #b3e6cc;"),
                                            h6("Style du texte:", style = "font-size: 13px; color: #555; margin-bottom: 10px; font-weight: bold;"),
                                            fluidRow(
                                              column(6,
                                                     checkboxInput(ns("descPlotYTickBold"), 
                                                                   HTML("<small><strong>Gras</strong></small>"), 
                                                                   value = FALSE)
                                              ),
                                              column(6,
                                                     checkboxInput(ns("descPlotYTickItalic"), 
                                                                   HTML("<small><em>Italique</em></small>"), 
                                                                   value = FALSE)
                                              )
                                            )
                                          )
                                   )
                                 )
                               )
                             )
                      )
                    ),
                    hr(style = "border-color: #ccc; margin: 30px 0;"),
                    tags$div(
                      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 15px; border-radius: 10px; margin-bottom: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.15);",
                      h4(icon("chart-area"), " Aperçu du graphique", 
                         style = "color: white; margin: 0; text-align: center; font-weight: bold;")
                    ),
                    uiOutput(ns("descPlotOutput"))
                )
              )
  )
}

mod_descriptive_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Depot des statistiques descriptives pour l'aide a la decision. Ici, et non
    # dans app_server.R : les selecteurs sont namespaces, `input$numVars`
    # n'existe qu'a l'interieur de ce module.
    observeEvent(values$descStats, {
      df <- values$descStats
      if (is.null(df) || !NROW(df)) return()
      hstat_ai_capture(values, "Analyses descriptives",
        "Statistiques descriptives",
        tables = list("Statistiques descriptives" = df),
        meta = list(variables = input$numVars, groupe = input$descFactors),
        plot = function() shiny::isolate(generate_desc_plot()))
    }, ignoreInit = TRUE)
  # ---- Analyse descriptives ----
  
  output$numVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    tagList(
      pickerInput(
        inputId = ns("numVars"),
        label = "Sélectionnez les variables numériques:", 
        choices = num_cols,
        multiple = TRUE,
        selected = num_cols[1:min(5, length(num_cols))],
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      div(style = "margin-top: 10px;",
          actionButton(ns("selectAllNumVars"), "Tout sélectionner", 
                       class = "btn-success btn-sm", 
                       icon = icon("check-double")),
          actionButton(ns("deselectAllNumVars"), "Tout désélectionner", 
                       class = "btn-warning btn-sm", 
                       icon = icon("times"),
                       style = "margin-left: 5px;")
      )
    )
  })
  
  observeEvent(input$selectAllNumVars, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "numVars", selected = num_cols)
    showNotification("Toutes les variables sélectionnées", type = "message", duration = 2)
  })
  
  observeEvent(input$deselectAllNumVars, {
    updatePickerInput(session, "numVars", selected = character(0))
    showNotification("Toutes les variables désélectionnées", type = "message", duration = 2)
  })
  
  output$descFactorUI <- renderUI({
    req(values$filteredData)
    fac_cols <- get_all_factor_candidates(values$filteredData)
    tagList(
      pickerInput(ns("descFactors"), "Calcul par facteurs (optionnel)",
                  choices  = fac_cols,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE, `live-search` = TRUE)),
      helpText(icon("info-circle"),
               " Tous types acceptés (facteur, texte, date, numérique <=30 niveaux).",
               " Laissez vide pour des descriptives globales.")
    )
  })
  
  observeEvent(input$selectAllNum, {
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (input$selectAllNum) {
      updatePickerInput(session, "numVars", selected = num_cols)
    } else {
      updatePickerInput(session, "numVars", selected = character(0))
    }
  })
  
  observeEvent(input$calcDesc, {
    req(input$numVars)
    
    showNotification("Calcul en cours...", type = "message", duration = NULL, id = "calcProgress")
    
    tryCatch({
      stats_sel <- input$descStats
      
      make_summ_global <- function(df_in, num_vars, stats_sel) {
        summ_list <- lapply(num_vars, function(var) {
          x <- df_in[[var]]
          stats <- list()
          if ("mean" %in% stats_sel) stats$mean <- mean(x, na.rm = TRUE)
          if ("median" %in% stats_sel) stats$median <- median(x, na.rm = TRUE)
          if ("sd" %in% stats_sel) stats$sd <- sd(x, na.rm = TRUE)
          if ("var" %in% stats_sel) stats$var <- var(x, na.rm = TRUE)
          if ("cv" %in% stats_sel) stats$cv <- calc_cv(x)
          if ("min" %in% stats_sel) stats$min <- min(x, na.rm = TRUE)
          if ("max" %in% stats_sel) stats$max <- max(x, na.rm = TRUE)
          if ("q1" %in% stats_sel) stats$q1 <- quantile(x, 0.25, na.rm = TRUE)
          if ("q3" %in% stats_sel) stats$q3 <- quantile(x, 0.75, na.rm = TRUE)
          
          data.frame(Facteurs = "Global", Variable = var, as.data.frame(stats), check.names = FALSE)
        })
        do.call(rbind, summ_list)
      }
      
      make_summ_grouped <- function(df_in, group_vars, num_vars, stats_sel) {
        results_list <- lapply(num_vars, function(var_name) {
          var_results <- df_in %>%
            group_by(!!!syms(group_vars)) %>%
            summarise(
              mean = if("mean" %in% stats_sel) mean(.data[[var_name]], na.rm = TRUE) else NA_real_,
              median = if("median" %in% stats_sel) median(.data[[var_name]], na.rm = TRUE) else NA_real_,
              sd = if("sd" %in% stats_sel) sd(.data[[var_name]], na.rm = TRUE) else NA_real_,
              var = if("var" %in% stats_sel) var(.data[[var_name]], na.rm = TRUE) else NA_real_,
              cv = if("cv" %in% stats_sel) calc_cv(.data[[var_name]]) else NA_real_,
              min = if("min" %in% stats_sel) min(.data[[var_name]], na.rm = TRUE) else NA_real_,
              max = if("max" %in% stats_sel) max(.data[[var_name]], na.rm = TRUE) else NA_real_,
              q1 = if("q1" %in% stats_sel) quantile(.data[[var_name]], 0.25, na.rm = TRUE) else NA_real_,
              q3 = if("q3" %in% stats_sel) quantile(.data[[var_name]], 0.75, na.rm = TRUE) else NA_real_,
              .groups = "drop"
            ) %>%
            mutate(Variable = var_name)
          
          return(var_results)
        })
        
        df_combined <- bind_rows(results_list)
        
        selected_cols <- c(group_vars, "Variable", stats_sel)
        final_cols <- selected_cols[selected_cols %in% names(df_combined)]
        df_combined <- df_combined[, final_cols, drop = FALSE]
        
        return(df_combined)
      }
      
      if (!is.null(input$descFactors) && length(input$descFactors) > 0) {
        values$descStats <- make_summ_grouped(values$filteredData, input$descFactors, input$numVars, stats_sel)
      } else {
        values$descStats <- make_summ_global(values$filteredData, input$numVars, stats_sel)
      }
      
      removeNotification("calcProgress")
      showNotification("Statistiques calculées avec succès!", type = "message", duration = 3)
      
    }, error = function(e) {
      removeNotification("calcProgress")
      showNotification(hstat_err_fr(e, "Erreur"), type = "error", duration = 5)
    })
  })

  # NB : l'indicateur output$hstatBigData (mode hors-memoire) est defini une
  # seule fois, dans app_server.R -- le redefinir ici ecraserait silencieusement
  # la premiere definition (les output$ Shiny doivent etre uniques).

  # Calcul des statistiques descriptives EXACTES sur le jeu complet (DuckDB)
  observeEvent(input$calcDescFull, {
    if (!identical(values$dataMode, "duckdb") || is.null(values$dbCon)) {
      showNotification("Le calcul sur jeu complet n'est disponible qu'en mode hors-mémoire.",
                       type = "warning")
      return(invisible(NULL))
    }
    if (is.null(input$numVars) || length(input$numVars) == 0) {
      showNotification("Sélectionnez au moins une variable numérique.", type = "warning")
      return(invisible(NULL))
    }
    stats_sel <- input$descStats
    if (is.null(stats_sel) || length(stats_sel) == 0) {
      showNotification("Sélectionnez au moins une statistique.", type = "warning")
      return(invisible(NULL))
    }
    tryCatch({
      withProgress(message = "Calcul exact sur le jeu complet (DuckDB)", value = 0.4, {
        if (!is.null(input$descFactors) && length(input$descFactors) > 0) {
          ck <- hstat_cache_key("desc_grouped", values$dbTable,
                                input$descFactors, input$numVars, stats_sel)
          values$descStats <- hstat_cache_get(ck, function()
            hstat_duckdb_describe_grouped(
              values$dbCon, values$dbTable, input$descFactors, input$numVars, stats_sel))
        } else {
          ck <- hstat_cache_key("desc_global", values$dbTable,
                                input$numVars, stats_sel)
          values$descStats <- hstat_cache_get(ck, function()
            hstat_duckdb_describe_global(
              values$dbCon, values$dbTable, input$numVars, stats_sel))
        }
        incProgress(1)
      })
      showNotification(
        trf("Statistiques exactes calculées sur le jeu complet (%s lignes).",
                format(values$fullNrow %||% 0, big.mark = " ")),
        type = "message", duration = 6)
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Calcul sur le jeu complet"),
                       type = "error", duration = 6)
    })
  })
  
  output$descResults <- renderDT({
    req(values$descStats)
    
    use_round <- !is.null(input$descRoundResults) && input$descRoundResults
    dec <- if (use_round && !is.null(input$descDecimals)) input$descDecimals else 4
    
    datatable(
      values$descStats, 
      options = list(
        scrollX = TRUE, 
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        language = list(
          search = "Rechercher:",
          lengthMenu = "Afficher _MENU_ lignes",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ lignes",
          paginate = list(previous = "Précédent", `next` = "Suivant")
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover',
      filter = 'top'
    ) %>%
      formatRound(columns = which(sapply(values$descStats, is.numeric)), digits = dec)
  })
  
  output$downloadDesc <- downloadHandler(
    filename = function() {
      paste0("descriptives_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$descStats, file, row.names = FALSE)
      showNotification("Fichier CSV téléchargé!", type = "message", duration = 3)
    }
  )
  
  output$downloadDescExcel <- downloadHandler(
    filename = function() {
      paste0("descriptives_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # openxlsx fait partie des packages requis ; repli propre si indisponible.
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        openxlsx::write.xlsx(values$descStats, file)
        showNotification("Fichier Excel téléchargé!", type = "message", duration = 3)
      } else if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(values$descStats, file)
        showNotification("Fichier Excel téléchargé!", type = "message", duration = 3)
      } else {
        utils::write.csv(values$descStats, file, row.names = FALSE)
        showNotification("Package Excel absent : export CSV fourni à la place.",
                         type = "warning", duration = 5)
      }
    }
  )
  
  output$descPlotVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    selectInput(ns("descPlotVar"), "Variable à visualiser:", choices = num_cols, width = "100%")
  })
  
  output$descPlotFactorSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- names(values$filteredData)[sapply(values$filteredData, is.factor)]
    selectInput(ns("descPlotFactor"), "Grouper par:", choices = c("Aucun", fac_cols), width = "100%")
  })
  
  observe({
    req(input$descPlotVar)
    
    if (is.null(input$descPlotTitle) || input$descPlotTitle == "") {
      updateTextInput(session, "descPlotTitle", value = paste("Distribution de", input$descPlotVar))
    }
    
    if (is.null(input$descPlotXLabel) || input$descPlotXLabel == "") {
      updateTextInput(session, "descPlotXLabel", value = input$descPlotVar)
    }
    
    if (is.null(input$descPlotYLabel) || input$descPlotYLabel == "") {
      if (input$descPlotFactor == "Aucun") {
        updateTextInput(session, "descPlotYLabel", value = "Densité")
      } else {
        updateTextInput(session, "descPlotYLabel", value = input$descPlotVar)
      }
    }
  })
  
  generate_desc_plot <- function() {
    req(values$filteredData, input$descPlotVar)
    
    plot_title <- if(!is.null(input$descPlotTitle) && input$descPlotTitle != "") {
      input$descPlotTitle
    } else {
      paste("Distribution de", input$descPlotVar)
    }
    
    x_label <- if(!is.null(input$descPlotXLabel) && input$descPlotXLabel != "") {
      input$descPlotXLabel
    } else {
      input$descPlotVar
    }
    
    y_label <- if(!is.null(input$descPlotYLabel) && input$descPlotYLabel != "") {
      input$descPlotYLabel
    } else {
      "Valeur"
    }
    
    title_font_face <- "plain"
    if (isTRUE(input$descPlotTitleBold) && isTRUE(input$descPlotTitleItalic)) {
      title_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotTitleBold)) {
      title_font_face <- "bold"
    } else if (isTRUE(input$descPlotTitleItalic)) {
      title_font_face <- "italic"
    }
    
    x_label_font_face <- "plain"
    if (isTRUE(input$descPlotXBold) && isTRUE(input$descPlotXItalic)) {
      x_label_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotXBold)) {
      x_label_font_face <- "bold"
    } else if (isTRUE(input$descPlotXItalic)) {
      x_label_font_face <- "italic"
    }
    
    y_label_font_face <- "plain"
    if (isTRUE(input$descPlotYBold) && isTRUE(input$descPlotYItalic)) {
      y_label_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotYBold)) {
      y_label_font_face <- "bold"
    } else if (isTRUE(input$descPlotYItalic)) {
      y_label_font_face <- "italic"
    }
    
    x_tick_font_face <- "plain"
    if (isTRUE(input$descPlotXTickBold) && isTRUE(input$descPlotXTickItalic)) {
      x_tick_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotXTickBold)) {
      x_tick_font_face <- "bold"
    } else if (isTRUE(input$descPlotXTickItalic)) {
      x_tick_font_face <- "italic"
    }
    
    y_tick_font_face <- "plain"
    if (isTRUE(input$descPlotYTickBold) && isTRUE(input$descPlotYTickItalic)) {
      y_tick_font_face <- "bold.italic"
    } else if (isTRUE(input$descPlotYTickBold)) {
      y_tick_font_face <- "bold"
    } else if (isTRUE(input$descPlotYTickItalic)) {
      y_tick_font_face <- "italic"
    }
    
    x_angle <- if(!is.null(input$descPlotXAngle)) input$descPlotXAngle else 0
    x_hjust <- if(x_angle > 0) 1 else 0.5
    x_vjust <- if(x_angle > 0) 1 else 0.5
    
    color_palette <- input$descPlotColorPalette
    if (is.null(color_palette)) color_palette <- "ggplot2"
    
    p <- if (input$descPlotFactor == "Aucun") {
      hist_color <- switch(color_palette,
                           "ggplot2" = "#3498db",
                           "viridis" = "#440154",
                           "Set1" = "#E41A1C",
                           "Set2" = "#66C2A5",
                           "Pastel1" = "#FBB4AE",
                           "Dark2" = "#1B9E77",
                           "mono_blue" = "#2E86AB",
                           "mono_green" = "#06A77D",
                           "mono_red" = "#D62828",
                           "mono_purple" = "#6A4C93",
                           "mono_orange" = "#F77F00",
                           "mono_black" = "#2C3E50",
                           "#3498db")
      
      density_color <- switch(color_palette,
                              "ggplot2" = "#e74c3c",
                              "viridis" = "#FDE724",
                              "mono_blue" = "#1A5490",
                              "mono_green" = "#048060",
                              "mono_red" = "#9B1B1B",
                              "mono_purple" = "#4A2C6A",
                              "mono_orange" = "#C66300",
                              "mono_black" = "#000000",
                              "#e74c3c")
      
      ggplot(values$filteredData, aes(x = .data[[input$descPlotVar]])) +
        geom_histogram(aes(y = after_stat(density)), alpha = 0.7, fill = hist_color, bins = 30) +
        geom_density(linewidth = 1.2, color = density_color) +
        theme_minimal() +
        labs(title = plot_title, x = x_label, y = y_label) +
        theme(
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5),
          axis.title.x = element_markdown(face = x_label_font_face, size = 13, margin = margin(t = 10)),
          axis.title.y = element_markdown(face = y_label_font_face, size = 13, margin = margin(r = 10)),
          axis.text.x = element_text(
            face = x_tick_font_face, 
            angle = x_angle, 
            hjust = x_hjust,
            vjust = x_vjust,
            size = 11
          ),
          axis.text.y = element_text(face = y_tick_font_face, size = 11),
          plot.title = element_markdown(
            hjust = if(isTRUE(input$descPlotCenterTitle)) 0.5 else 0, 
            face = title_font_face, 
            size = 16,
            margin = margin(b = 15)
          ),
          panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
          panel.grid.minor = element_line(color = "grey95", linewidth = 0.2)
        )
    } else {
      
      
      base_plot <- ggplot(values$filteredData, aes(
        x = .data[[input$descPlotFactor]], 
        y = .data[[input$descPlotVar]], 
        fill = .data[[input$descPlotFactor]]
      )) +
        stat_boxplot(geom = "errorbar", width = 0.3) +
        geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2)
      
      if (isTRUE(input$descPlotShowValues)) {
        extreme_data <- values$filteredData %>%
          group_by(.data[[input$descPlotFactor]]) %>%
          summarise(
            min_val = min(.data[[input$descPlotVar]], na.rm = TRUE),
            max_val = max(.data[[input$descPlotVar]], na.rm = TRUE),
            .groups = "drop"
          )
        
        base_plot <- base_plot +
          geom_text(
            data = extreme_data,
            aes(x = .data[[input$descPlotFactor]], y = min_val, label = round(min_val, 2)),
            vjust = 1.5,
            size = 3.5,
            fontface = "bold",
            color = "#2c3e50",
            inherit.aes = FALSE
          ) +
          geom_text(
            data = extreme_data,
            aes(x = .data[[input$descPlotFactor]], y = max_val, label = round(max_val, 2)),
            vjust = -0.5,
            size = 3.5,
            fontface = "bold",
            color = "#2c3e50",
            inherit.aes = FALSE
          )
      }
      
      base_plot <- base_plot +
        theme_minimal() +
        theme(
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5),
          axis.title.x = element_markdown(face = x_label_font_face, size = 13, margin = margin(t = 10)),
          axis.title.y = element_markdown(face = y_label_font_face, size = 13, margin = margin(r = 10)),
          axis.text.x = element_text(
            face = x_tick_font_face, 
            angle = x_angle, 
            hjust = x_hjust,
            vjust = x_vjust,
            size = 11
          ),
          axis.text.y = element_text(face = y_tick_font_face, size = 11),
          plot.title = element_markdown(
            hjust = if(isTRUE(input$descPlotCenterTitle)) 0.5 else 0,
            face = title_font_face, 
            size = 16,
            margin = margin(b = 15)
          ),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
          panel.grid.minor = element_blank()
        ) +
        labs(title = plot_title, x = x_label, y = y_label)
      
      if (color_palette == "ggplot2") {
        base_plot
      } else if (startsWith(color_palette, "mono_")) {
        mono_color <- switch(color_palette,
                             "mono_blue" = "#2E86AB",
                             "mono_green" = "#06A77D",
                             "mono_red" = "#D62828",
                             "mono_purple" = "#6A4C93",
                             "mono_orange" = "#F77F00",
                             "mono_black" = "#2C3E50",
                             "#2E86AB")
        n_groups <- length(unique(values$filteredData[[input$descPlotFactor]]))
        base_plot + scale_fill_manual(
          values = colorRampPalette(c("#FFFFFF", mono_color))(n_groups + 2)[2:(n_groups + 1)]
        )
      } else if (color_palette == "viridis") {
        base_plot + scale_fill_viridis_d(option = "D")
      } else {
        base_plot + scale_fill_brewer(palette = color_palette)
      }
    }
    
    return(p)
  }
  
  output$descPlotOutput <- renderUI({
    req(input$descPlotWidth, input$descPlotHeight)
    plotOutput(ns("descPlot"), width = paste0(input$descPlotWidth, "px"), height = paste0(input$descPlotHeight, "px"))
  })
  
  output$descPlot <- renderPlot({
    req(values$filteredData)
    p <- generate_desc_plot()
    print(p)
  }, res = 96)
  
  # L'export passe par le kit partage. La taille etait FIGEE a 9,8 x 7,1
  # pouces : les curseurs voisins ne reglent que l'apercu a l'ecran, et rien
  # n'agissait sur le fichier produit.
  output$descPlDl <- hstat_export_plot_handler(input, "descPl",
                       function() generate_desc_plot(), "graphique_descriptif")
  })
}
