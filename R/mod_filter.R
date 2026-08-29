#  Module Shiny : Filtrage des donnees


mod_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
              shiny::fluidRow(
                shinydashboard::box(title = "Actions globales", status = "warning", width = 12, solidHeader = TRUE,
                    icon = shiny::icon("cog"),
                    shiny::fluidRow(
                      shiny::column(6,
                             shiny::actionButton(ns("resetFilter"), "Réinitialiser tous les filtres", 
                                          class = "btn-warning btn-lg btn-block", 
                                          icon = shiny::icon("redo"))
                      ),
                      shiny::column(6,
                             shiny::downloadButton(ns("downloadFilteredData"), "Télécharger les données filtrées", 
                                            class = "btn-success btn-lg btn-block")
                      )
                    )
                )
              ),
              
              shiny::fluidRow(
                shinydashboard::valueBoxOutput(ns("originalRows"), width = 3),
                shinydashboard::valueBoxOutput(ns("filteredRows"), width = 3),
                shinydashboard::valueBoxOutput(ns("removedRows"), width = 3),
                shinydashboard::valueBoxOutput(ns("columnsCount"), width = 3)
              ),
              
              #  Section 1: Filtres basiques (lignes et valeurs) 
              shiny::fluidRow(
                shinydashboard::box(title = "Filtre par sélection de lignes", status = "primary", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE,
                    icon = shiny::icon("list-ol"),
                    shiny::uiOutput(ns("rowRangeUI")),
                    shiny::hr(),
                    shiny::actionButton(ns("applyRowRange"), "Appliquer la sélection de lignes", 
                                 class = "btn-primary btn-block", icon = shiny::icon("filter"))
                ),
                
                shinydashboard::box(title = "Filtre par valeur(s)", status = "primary", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE,
                    icon = shiny::icon("search"),
                    shiny::uiOutput(ns("valueFilterUI")),
                    shiny::hr(),
                    shiny::actionButton(ns("applyValueFilter"), "Appliquer le filtre par valeur", 
                                 class = "btn-primary btn-block", icon = shiny::icon("filter"))
                )
              ),
              
              #  Section 2: Filtre par colonnes 
              shiny::fluidRow(
                shinydashboard::box(title = "Sélection des colonnes", status = "info", width = 12, 
                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    icon = shiny::icon("columns"),
                    shiny::fluidRow(
                      shiny::column(12,
                             shiny::uiOutput(ns("columnSelectUI"))
                      )
                    ),
                    shiny::hr(),
                    shiny::actionButton(ns("applyColumnFilter"), "Appliquer la sélection de colonnes", 
                                 class = "btn-info btn-block", icon = shiny::icon("check"))
                )
              ),
              
              #  Section 3: Filtres avancés  
              shiny::fluidRow(
                shinydashboard::box(title = "Filtre croisement complet (2 facteurs)", status = "success", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    icon = shiny::icon("th"),
                    shiny::uiOutput(ns("filterFactorA")),
                    shiny::uiOutput(ns("filterFactorB")),
                    shiny::checkboxInput(ns("requireA"), "Garder niveaux de A présents pour tous les niveaux de B", TRUE),
                    shiny::checkboxInput(ns("requireB"), "Garder niveaux de B présents pour tous les niveaux de A", FALSE),
                    shiny::hr(),
                    shiny::actionButton(ns("applyCrossFilter"), "Appliquer (2 facteurs)", 
                                 class = "btn-success btn-block", icon = shiny::icon("filter")),
                    shiny::helpText("Filtre les données pour ne garder que les combinaisons complètes entre deux facteurs.")
                ),
                
                shinydashboard::box(title = "Filtre croisement complet (N facteurs)", status = "success", width = 6, 
                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    icon = shiny::icon("project-diagram"),
                    shiny::uiOutput(ns("filterFactorsN")),
                    shiny::helpText("Garde uniquement les niveaux qui forment un croisement complet entre tous les facteurs sélectionnés."),
                    shiny::hr(),
                    shiny::actionButton(ns("applyCrossFilterN"), "Appliquer (N facteurs)", 
                                 class = "btn-success btn-block", icon = shiny::icon("filter"))
                )
              ),
              
              shiny::fluidRow(
                shinydashboard::box(title = "Aperçu des données filtrées", status = "info", width = 12, 
                    solidHeader = TRUE, collapsible = TRUE,
                    icon = shiny::icon("table"),
                    DT::DTOutput(ns("filteredData")),
                    shiny::br(),
                    shiny::helpText("Ce tableau affiche les données après application des filtres.")
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
      trf("Sous-échantillon filtre (%d observations sur %d)", NROW(d), n0),
      tables = list("Diagnostic de qualité" = dq),
      meta = list(variables = names(d), `observations retenues` = NROW(d),
                  `observations initiales` = n0,
                  `part conservee` = sprintf("%.1f %%", 100 * NROW(d) / max(1, n0))))
  }, ignoreInit = TRUE)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  # ---- Filtrage ----
  
  
  output$rowRangeUI <- shiny::renderUI({
    shiny::req(values$cleanData)
    max_rows <- nrow(values$cleanData)
    shiny::tagList(
      shiny::textAreaInput(ns("rowSelection"), "Sélection de lignes :",
                    placeholder = "Exemples :\n1 à 10\n1,3,4,5,10,15,20\n1,3,4,5,10,15,20 à 30",
                    rows = 3),
      # Le contenu d'un <code> n'est JAMAIS traduit par le navigateur (la balise
      # est dans sa liste d'exclusion, et c'est voulu : on n'altere pas un
      # extrait de code). Les exemples de syntaxe resteraient donc en francais.
      # Le bloc passe par un gabarit traduit cote serveur, ou ils suivent la
      # langue -- « 1 to 10 » en anglais, que l'analyseur accepte desormais.
      shiny::helpText(shiny::HTML(trf(paste0(
        "<b>Formats acceptés :</b><br>",
        "- Plage : <code>1 à 10</code> ou <code>5-15</code><br>",
        "- Liste : <code>1,3,4,5,10,15,20</code><br>",
        "- Combinaison : <code>1,3,5,10 à 15,20,25 à 30</code><br>",
        "Total de lignes disponibles : %s"), max_rows)))
    )
  })
  
  parseRowSelection <- function(selection_text, max_rows) {
    if (is.null(selection_text) || nchar(trimws(selection_text)) == 0) {
      stop("Veuillez entrer une sélection de lignes valide.")
    }
    
    text <- trimws(selection_text)
    text <- gsub("\\s+", " ", text)  
    
    # Remplacer différents formats de plage par un format uniforme.
    # « to » est accepté au même titre que « à » : les exemples affichés sous
    # le champ sont traduits dans la version anglaise, et un utilisateur qui
    # recopie « 1 to 10 » doit obtenir la plage qu'on lui a montrée -- sinon la
    # traduction lui enseigne une syntaxe que l'application refuse.
    text <- gsub("\\bto\\b", "-", text, ignore.case = TRUE)
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
          stop(trf("Format de plage invalide : %s", part))
        }
        
        start <- as.numeric(range_parts[1])
        end <- as.numeric(range_parts[2])
        
        if (is.na(start) || is.na(end)) {
          stop(paste("Plage invalide :", part))
        }
        
        if (start > end) {
          stop(trf("La ligne de début doit être <= ligne de fin dans : %s", part))
        }
        
        if (start < 1 || end > max_rows) {
          stop(paste("Plage hors limites :", part, "(min: 1, max:", max_rows, ")"))
        }
        
        all_rows <- c(all_rows, start:end)
        
      } else {
        row_num <- as.numeric(part)
        
        if (is.na(row_num)) {
          stop(trf("Numéro de ligne invalide : %s", part))
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
  
  shiny::observeEvent(input$applyRowRange, {
    shiny::req(values$cleanData, input$rowSelection)
    
    tryCatch({
      max_rows <- nrow(values$cleanData)
      selected_rows <- parseRowSelection(input$rowSelection, max_rows)
      
      filtered <- values$cleanData[selected_rows, ]
      values$filteredData <- filtered
      
      shiny::showNotification(
        trf("Filtre par plage appliqué. %s lignes sélectionnées", length(selected_rows)),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur filtre par plage"), type = "error", duration = 10)
    })
  })
  
  
  # Permet de rechercher des lignes contenant une ou plusieurs valeurs spécifiques
  output$valueFilterUI <- shiny::renderUI({
    shiny::req(values$cleanData)
    col_names <- names(values$cleanData)
    shiny::tagList(
      shiny::selectInput(ns("valueFilterCol"), "Colonne à filtrer :",
                  choices = col_names, selected = col_names[1]),
      shiny::textAreaInput(ns("valueFilterText"), "Valeur(s) à rechercher (une par ligne) :",
                    placeholder = "Août 04-10\nSeptembre 01-07\nJuillet 28-03",
                    rows = 4),
      shiny::checkboxInput(ns("valueFilterExact"), "Correspondance exacte", FALSE),
      shiny::checkboxInput(ns("valueFilterCaseSensitive"), "Sensible à la casse", FALSE),
      shiny::helpText("Entrez une ou plusieurs valeurs, une par ligne. Les lignes contenant au moins une de ces valeurs seront conservées.")
    )
  })
  
  shiny::observeEvent(input$applyValueFilter, {
    shiny::req(values$cleanData, input$valueFilterCol, input$valueFilterText)
    
    shiny::validate(shiny::need(nchar(trimws(input$valueFilterText)) > 0, "Veuillez entrer au moins une valeur à rechercher."))
    
    tryCatch({
      df <- values$cleanData
      col_name <- input$valueFilterCol
      
      search_values <- strsplit(input$valueFilterText, "\n")[[1]]
      search_values <- trimws(search_values)
      search_values <- search_values[nchar(search_values) > 0]  
      
      shiny::validate(shiny::need(length(search_values) > 0, "Veuillez entrer au moins une valeur valide."))
      
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
      
      shiny::showNotification(
        trf("Filtre par valeur appliqué : %s lignes trouvées avec %s valeur(s)", nrow(filtered), length(search_values)),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur filtre par valeur"), type = "error", duration = 10)
    })
  })
  
  
  output$columnSelectUI <- shiny::renderUI({
    shiny::req(values$cleanData)
    col_names <- names(values$cleanData)
    
    shiny::tagList(
      shiny::checkboxGroupInput(ns("selectedColumns"), "Sélectionner les colonnes à conserver :",
                         choices = col_names,
                         selected = col_names,
                         inline = FALSE),
      shiny::fluidRow(
        shiny::column(6,
               shiny::actionButton(ns("selectAllCols"), "Tout sélectionner", 
                            class = "btn-sm btn-default btn-block", icon = shiny::icon("check-square"))
        ),
        shiny::column(6,
               shiny::actionButton(ns("deselectAllCols"), "Tout désélectionner", 
                            class = "btn-sm btn-default btn-block", icon = shiny::icon("square"))
        )
      )
    )
  })
  
  shiny::observeEvent(input$selectAllCols, {
    shiny::req(values$cleanData)
    col_names <- names(values$cleanData)
    shiny::updateCheckboxGroupInput(session, "selectedColumns", selected = col_names)
  })
  
  shiny::observeEvent(input$deselectAllCols, {
    shiny::updateCheckboxGroupInput(session, "selectedColumns", selected = character(0))
  })
  
  shiny::observeEvent(input$applyColumnFilter, {
    shiny::req(values$cleanData, input$selectedColumns)
    
    shiny::validate(shiny::need(length(input$selectedColumns) > 0, "Veuillez sélectionner au moins une colonne."))
    
    tryCatch({
      filtered <- values$cleanData[, input$selectedColumns, drop = FALSE]
      values$filteredData <- filtered
      
      shiny::showNotification(
        trf("Filtre par colonnes appliqué : %s colonnes sélectionnées", length(input$selectedColumns)),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur filtre par colonnes"), type = "error", duration = 10)
    })
  })
  
  
  output$filterFactorA <- shiny::renderUI({
    shiny::req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    if (length(fac_cols) == 0) {
      shiny::helpText("Aucun facteur. Convertissez d'abord des variables en facteurs dans l'onglet Nettoyage.")
    } else {
      shiny::selectInput(ns("factorA"), "Facteur A :", choices = fac_cols)
    }
  })
  
  output$filterFactorB <- shiny::renderUI({
    shiny::req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    if (length(fac_cols) == 0) {
      NULL
    } else {
      shiny::selectInput(ns("factorB"), "Facteur B :", choices = fac_cols, selected = fac_cols[min(2, length(fac_cols))])
    }
  })
  
  shiny::observeEvent(input$applyCrossFilter, {
    shiny::req(values$cleanData, input$factorA, input$factorB)
    shiny::validate(shiny::need(input$factorA != input$factorB, "Choisissez deux facteurs distincts."))
    tryCatch({
      df <- values$cleanData
      filtered <- filter_complete_cross(df, input$factorA, input$factorB,
                                        reqA = isTRUE(input$requireA),
                                        reqB = isTRUE(input$requireB))
      values$filteredData <- filtered
      shiny::showNotification(trf("Filtrage (2 facteurs) appliqué. Lignes : %s", nrow(filtered)), type = "message", duration = 5)
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur filtrage"), type = "error", duration = 10)
    })
  })
  
  output$filterFactorsN <- shiny::renderUI({
    shiny::req(values$cleanData)
    fac_cols <- names(values$cleanData)[sapply(values$cleanData, is.factor)]
    shiny::selectInput(ns("factorsN"), "Facteurs (>=2) :", choices = fac_cols, multiple = TRUE)
  })
  
  shiny::observeEvent(input$applyCrossFilterN, {
    shiny::req(values$cleanData, input$factorsN)
    shiny::validate(shiny::need(length(input$factorsN) >= 2, "Sélectionnez au moins deux facteurs."))
    tryCatch({
      filtered <- filter_complete_cross_n(values$cleanData, input$factorsN)
      values$filteredData <- filtered
      shiny::showNotification(trf("Filtrage (N facteurs) appliqué. Lignes : %s", nrow(filtered)), type = "message", duration = 5)
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur filtrage N facteurs"), type = "error", duration = 10)
    })
  })
  
  shiny::observeEvent(input$resetFilter, {
    values$filteredData <- values$cleanData
    shiny::showNotification("Tous les filtres réinitialisés", type = "message", duration = 5)
  })
  
  output$originalRows <- shinydashboard::renderValueBox({
    shiny::req(values$cleanData)
    shinydashboard::valueBox(
      nrow(values$cleanData), "Lignes originales", icon = shiny::icon("list"),
      color = "blue"
    )
  })
  
  output$filteredRows <- shinydashboard::renderValueBox({
    shiny::req(values$filteredData)
    shinydashboard::valueBox(
      nrow(values$filteredData), "Lignes filtrées", icon = shiny::icon("filter"),
      color = "green"
    )
  })
  
  output$removedRows <- shinydashboard::renderValueBox({
    shiny::req(values$cleanData, values$filteredData)
    removed <- nrow(values$cleanData) - nrow(values$filteredData)
    shinydashboard::valueBox(
      removed, "Lignes supprimées", icon = shiny::icon("trash"),
      color = ifelse(removed > 0, "red", "green")
    )
  })
  
  output$columnsCount <- shinydashboard::renderValueBox({
    shiny::req(values$filteredData)
    shinydashboard::valueBox(
      ncol(values$filteredData), "Colonnes actives", icon = shiny::icon("columns"),
      color = "purple"
    )
  })
  
  output$filteredData <- DT::renderDT({
    shiny::req(values$filteredData)
    DT::datatable(values$filteredData, 
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
  
  output$downloadFilteredData <- shiny::downloadHandler(
    filename = function() {
      paste("données_filtrees_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      shiny::req(values$filteredData)
      utils::write.csv(values$filteredData, file, row.names = FALSE)
    }
  )
  })
}
