#  Module Shiny : Filtrage des donnees


mod_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
              fluidRow(
                box(title = "Actions globales", status = "warning", width = 12, solidHeader = TRUE,
                    icon = icon("cog"),
                    fluidRow(
                      column(6,
                             actionButton(ns("resetFilter"), "Réinitialiser tous les filtres", 
                                          class = "btn-warning btn-lg btn-block", 
                                          icon = icon("redo"))
                      ),
                      column(6,
                             downloadButton(ns("downloadFilteredData"), "Télécharger les données filtrées", 
                                            class = "btn-success btn-lg btn-block")
                      )
                    )
                )
              ),
              
              fluidRow(
                valueBoxOutput(ns("originalRows"), width = 3),
                valueBoxOutput(ns("filteredRows"), width = 3),
                valueBoxOutput(ns("removedRows"), width = 3),
                valueBoxOutput(ns("columnsCount"), width = 3)
              ),
              
              #  Section 1: Filtres basiques (lignes et valeurs) 
              fluidRow(
                box(title = "Filtre par sélection de lignes", status = "primary", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE,
                    icon = icon("list-ol"),
                    uiOutput(ns("rowRangeUI")),
                    hr(),
                    actionButton(ns("applyRowRange"), "Appliquer la sélection de lignes", 
                                 class = "btn-primary btn-block", icon = icon("filter"))
                ),
                
                box(title = "Filtre par valeur(s)", status = "primary", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE,
                    icon = icon("search"),
                    uiOutput(ns("valueFilterUI")),
                    hr(),
                    actionButton(ns("applyValueFilter"), "Appliquer le filtre par valeur", 
                                 class = "btn-primary btn-block", icon = icon("filter"))
                )
              ),
              
              #  Section 2: Filtre par colonnes 
              fluidRow(
                box(title = "Sélection des colonnes", status = "info", width = 12, 
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    icon = icon("columns"),
                    fluidRow(
                      column(12,
                             uiOutput(ns("columnSelectUI"))
                      )
                    ),
                    hr(),
                    actionButton(ns("applyColumnFilter"), "Appliquer la sélection de colonnes", 
                                 class = "btn-info btn-block", icon = icon("check"))
                )
              ),
              
              #  Section 3: Filtres avancés  
              fluidRow(
                box(title = "Filtre croisement complet (2 facteurs)", status = "success", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    icon = icon("th"),
                    uiOutput(ns("filterFactorA")),
                    uiOutput(ns("filterFactorB")),
                    checkboxInput(ns("requireA"), "Garder niveaux de A présents pour tous les niveaux de B", TRUE),
                    checkboxInput(ns("requireB"), "Garder niveaux de B présents pour tous les niveaux de A", FALSE),
                    hr(),
                    actionButton(ns("applyCrossFilter"), "Appliquer (2 facteurs)", 
                                 class = "btn-success btn-block", icon = icon("filter")),
                    helpText("Filtre les données pour ne garder que les combinaisons complètes entre deux facteurs.")
                ),
                
                box(title = "Filtre croisement complet (N facteurs)", status = "success", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    icon = icon("project-diagram"),
                    uiOutput(ns("filterFactorsN")),
                    helpText("Garde uniquement les niveaux qui forment un croisement complet entre tous les facteurs sélectionnés."),
                    hr(),
                    actionButton(ns("applyCrossFilterN"), "Appliquer (N facteurs)", 
                                 class = "btn-success btn-block", icon = icon("filter"))
                )
              ),
              
              fluidRow(
                box(title = "Aperçu des données filtrées", status = "info", width = 12, 
                    solidHeader = TRUE, collapsible = TRUE,
                    icon = icon("table"),
                    DTOutput(ns("filteredData")),
                    br(),
                    helpText("Ce tableau affiche les données après application des filtres.")
                )
              )
  )
}

mod_filter_server <- function(id, values) {
  # Depot de l'effet du filtrage. Le point qui compte : combien d'observations
  # restent, et donc quelle puissance statistique subsiste.
  shiny::observeEvent(values$filteredData, {
    d <- values$filteredData
    if (is.null(d) || !NROW(d)) return()
    n0 <- NROW(values$cleanData %||% values$data)
    # Au chargement, `filteredData` vaut le jeu complet : ce n'est pas un
    # filtrage. Ne revendiquer le contexte que si un filtre a reellement
    # retire des observations, sinon ce module ecraserait celui qui a
    # veritablement quelque chose a dire.
    if (NROW(d) >= n0) return()
    dq <- tryCatch(hstat_data_quality(d), error = function(e) NULL)
    hstat_ai_capture(values, "Filtrage",
      sprintf("Sous-echantillon filtre (%d observations sur %d)", NROW(d), n0),
      tables = list("Diagnostic de qualite" = dq),
      meta = list(variables = names(d), `observations retenues` = NROW(d),
                  `observations initiales` = n0,
                  `part conservee` = sprintf("%.1f %%", 100 * NROW(d) / max(1, n0))))
  }, ignoreInit = TRUE)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  # ---- Filtrage ----
  
  
  output$rowRangeUI <- renderUI({
    req(values$cleanData)
    max_rows <- nrow(values$cleanData)
    tagList(
      textAreaInput(ns("rowSelection"), "Sélection de lignes :",
                    placeholder = "Exemples :\n1 à 10\n1,3,4,5,10,15,20\n1,3,4,5,10,15,20 à 30",
                    rows = 3),
      helpText(HTML(paste0(
        "<b>Formats acceptés :</b><br>",
        "- Plage : <code>1 à 10</code> ou <code>5-15</code><br>",
        "- Liste : <code>1,3,4,5,10,15,20</code><br>",
        "- Combinaison : <code>1,3,5,10 à 15,20,25 à 30</code><br>",
        "Total de lignes disponibles : ", max_rows
      )))
    )
  })
  
  parseRowSelection <- function(selection_text, max_rows) {
    if (is.null(selection_text) || nchar(trimws(selection_text)) == 0) {
      stop("Veuillez entrer une sélection de lignes valide.")
    }
    
    text <- trimws(selection_text)
    text <- gsub("\\s+", " ", text)  
    
    # Remplacer différents formats de plage par un format uniforme
    text <- gsub("à", "-", text, ignore.case = TRUE)
    text <- gsub("a", "-", text, ignore.case = TRUE)
    
    parts <- strsplit(text, ",")[[1]]
    parts <- trimws(parts)
    
    all_rows <- c()
    
    for (part in parts) {
      if (grepl("-", part)) {
        # C'est une plage (ex: "1-10" ou "20-30")
        range_parts <- strsplit(part, "-")[[1]]
        range_parts <- trimws(range_parts)
        
        if (length(range_parts) != 2) {
          stop(paste("Format de plage invalide :", part))
        }
        
        start <- as.numeric(range_parts[1])
        end <- as.numeric(range_parts[2])
        
        if (is.na(start) || is.na(end)) {
          stop(paste("Plage invalide :", part))
        }
        
        if (start > end) {
          stop(paste("La ligne de début doit être <= ligne de fin dans :", part))
        }
        
        if (start < 1 || end > max_rows) {
          stop(paste("Plage hors limites :", part, "(min: 1, max:", max_rows, ")"))
        }
        
        all_rows <- c(all_rows, start:end)
        
      } else {
        row_num <- as.numeric(part)
        
        if (is.na(row_num)) {
          stop(paste("Numéro de ligne invalide :", part))
        }
        
        if (row_num < 1 || row_num > max_rows) {
          stop(paste("Ligne hors limites :", row_num, "(min: 1, max:", max_rows, ")"))
        }
        
        all_rows <- c(all_rows, row_num)
      }
    }
    
    all_rows <- unique(sort(all_rows))
    
    return(all_rows)
  }
  
  observeEvent(input$applyRowRange, {
    req(values$cleanData, input$rowSelection)
    
    tryCatch({
      max_rows <- nrow(values$cleanData)
      selected_rows <- parseRowSelection(input$rowSelection, max_rows)
      
      filtered <- values$cleanData[selected_rows, ]
      values$filteredData <- filtered
      
      showNotification(
        paste("Filtre par plage appliqué.", length(selected_rows), "lignes sélectionnées"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Erreur filtre par plage:", e$message), type = "error", duration = 10)
    })
  })
  
  
  # Permet de rechercher des lignes contenant une ou plusieurs valeurs spécifiques
  output$valueFilterUI <- renderUI({
    req(values$cleanData)
    col_names <- names(values$cleanData)
    tagList(
      selectInput(ns("valueFilterCol"), "Colonne à filtrer :",
                  choices = col_names, selected = col_names[1]),
      textAreaInput(ns("valueFilterText"), "Valeur(s) à rechercher (une par ligne) :",
                    placeholder = "Aout 04-10\nSeptembre 01-07\nJuillet 28-03",
                    rows = 4),
      checkboxInput(ns("valueFilterExact"), "Correspondance exacte", FALSE),
      checkboxInput(ns("valueFilterCaseSensitive"), "Sensible à la casse", FALSE),
      helpText("Entrez une ou plusieurs valeurs, une par ligne. Les lignes contenant au moins une de ces valeurs seront conservées.")
    )
  })
  
  observeEvent(input$applyValueFilter, {
    req(values$cleanData, input$valueFilterCol, input$valueFilterText)
    
    validate(need(nchar(trimws(input$valueFilterText)) > 0, "Veuillez entrer au moins une valeur à rechercher."))
    
    tryCatch({
      df <- values$cleanData
      col_name <- input$valueFilterCol
      
      search_values <- strsplit(input$valueFilterText, "\n")[[1]]
      search_values <- trimws(search_values)
      search_values <- search_values[nchar(search_values) > 0]  
      
      validate(need(length(search_values) > 0, "Veuillez entrer au moins une valeur valide."))
      
      col_data <- as.character(df[[col_name]])
      
      mask <- rep(FALSE, length(col_data))
      
      for (search_value in search_values) {
        if (input$valueFilterExact) {
          if (input$valueFilterCaseSensitive) {
            mask <- mask | (col_data == search_value)
          } else {
            mask <- mask | (tolower(col_data) == tolower(search_value))
          }
        } else {
          if (input$valueFilterCaseSensitive) {
            mask <- mask | grepl(search_value, col_data, fixed = TRUE)
          } else {
            mask <- mask | grepl(search_value, col_data, ignore.case = TRUE)
          }
        }
      }
      
      mask[is.na(mask)] <- FALSE
      
      filtered <- df[mask, ]
      values$filteredData <- filtered
      
      showNotification(
        paste("Filtre par valeur appliqué:", nrow(filtered), "lignes trouvées avec", length(search_values), "valeur(s)"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Erreur filtre par valeur:", e$message), type = "error", duration = 10)
    })
  })
  
  
  output$columnSelectUI <- renderUI({
    req(values$cleanData)
    col_names <- names(values$cleanData)
    
    tagList(
      checkboxGroupInput(ns("selectedColumns"), "Sélectionner les colonnes à conserver :",
                         choices = col_names,
                         selected = col_names,
                         inline = FALSE),
      fluidRow(
        column(6,
               actionButton(ns("selectAllCols"), "Tout sélectionner", 
                            class = "btn-sm btn-default btn-block", icon = icon("check-square"))
        ),
        column(6,
               actionButton(ns("deselectAllCols"), "Tout désélectionner", 
                            class = "btn-sm btn-default btn-block", icon = icon("square"))
        )
      )
    )
  })
  
  observeEvent(input$selectAllCols, {
    req(values$cleanData)
    col_names <- names(values$cleanData)
    updateCheckboxGroupInput(session, "selectedColumns", selected = col_names)
  })
  
  observeEvent(input$deselectAllCols, {
    updateCheckboxGroupInput(session, "selectedColumns", selected = character(0))
  })
  
  observeEvent(input$applyColumnFilter, {
    req(values$cleanData, input$selectedColumns)
    
    validate(need(length(input$selectedColumns) > 0, "Veuillez sélectionner au moins une colonne."))
    
    tryCatch({
      filtered <- values$cleanData[, input$selectedColumns, drop = FALSE]
      values$filteredData <- filtered
      
      showNotification(
        paste("Filtre par colonnes appliqué:", length(input$selectedColumns), "colonnes sélectionnées"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("Erreur filtre par colonnes:", e$message), type = "error", duration = 10)
    })
  })
  
  
  output$filterFactorA <- renderUI({
    req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    if (length(fac_cols) == 0) {
      helpText("Aucun facteur. Convertissez d'abord des variables en facteurs dans l'onglet Nettoyage.")
    } else {
      selectInput(ns("factorA"), "Facteur A :", choices = fac_cols)
    }
  })
  
  output$filterFactorB <- renderUI({
    req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    if (length(fac_cols) == 0) {
      NULL
    } else {
      selectInput(ns("factorB"), "Facteur B :", choices = fac_cols, selected = fac_cols[min(2, length(fac_cols))])
    }
  })
  
  observeEvent(input$applyCrossFilter, {
    req(values$cleanData, input$factorA, input$factorB)
    validate(need(input$factorA != input$factorB, "Choisissez deux facteurs distincts."))
    tryCatch({
      df <- values$cleanData
      filtered <- filter_complete_cross(df, input$factorA, input$factorB,
                                        reqA = isTRUE(input$requireA),
                                        reqB = isTRUE(input$requireB))
      values$filteredData <- filtered
      showNotification(paste("Filtrage (2 facteurs) appliqué. Lignes:", nrow(filtered)), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Erreur filtrage:", e$message), type = "error", duration = 10)
    })
  })
  
  output$filterFactorsN <- renderUI({
    req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    selectInput(ns("factorsN"), "Facteurs (>=2) :", choices = fac_cols, multiple = TRUE)
  })
  
  observeEvent(input$applyCrossFilterN, {
    req(values$cleanData, input$factorsN)
    validate(need(length(input$factorsN) >= 2, "Sélectionnez au moins deux facteurs."))
    tryCatch({
      filtered <- filter_complete_cross_n(values$cleanData, input$factorsN)
      values$filteredData <- filtered
      showNotification(paste("Filtrage (N facteurs) appliqué. Lignes:", nrow(filtered)), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Erreur filtrage N facteurs:", e$message), type = "error", duration = 10)
    })
  })
  
  observeEvent(input$resetFilter, {
    values$filteredData <- values$cleanData
    showNotification("Tous les filtres réinitialisés", type = "message", duration = 5)
  })
  
  output$originalRows <- renderValueBox({
    req(values$cleanData)
    valueBox(
      nrow(values$cleanData), "Lignes originales", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$filteredRows <- renderValueBox({
    req(values$filteredData)
    valueBox(
      nrow(values$filteredData), "Lignes filtrées", icon = icon("filter"),
      color = "green"
    )
  })
  
  output$removedRows <- renderValueBox({
    req(values$cleanData, values$filteredData)
    removed <- nrow(values$cleanData) - nrow(values$filteredData)
    valueBox(
      removed, "Lignes supprimées", icon = icon("trash"),
      color = ifelse(removed > 0, "red", "green")
    )
  })
  
  output$columnsCount <- renderValueBox({
    req(values$filteredData)
    valueBox(
      ncol(values$filteredData), "Colonnes actives", icon = icon("columns"),
      color = "purple"
    )
  })
  
  output$filteredData <- renderDT({
    req(values$filteredData)
    datatable(values$filteredData, 
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100),
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              filter = "top",
              rownames = TRUE,
              class = 'cell-border stripe')
  })
  
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      paste("données_filtrees_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$filteredData)
      write.csv(values$filteredData, file, row.names = FALSE)
    }
  )
  })
}
