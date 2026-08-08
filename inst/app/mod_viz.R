#  Module Shiny : Visualisation des donnees


mod_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(
              .hstat_scope_banner(exact = FALSE),
              fluidRow(
                box(
                  title = tagList(icon("chart-line"), " Visualisation Interactive des Données"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  p("Cette interface se met à jour automatiquement à chaque modification. ", 
                    "Sélectionnez vos variables et ajustez les paramètres pour voir les changements en temps réel.",
                    style = "font-size: 14px;"),
                  tags$ul(
                    tags$li(icon("check-circle", style = "color: #28a745;"), " Sélection automatique des variables"),
                    tags$li(icon("check-circle", style = "color: #28a745;"), " Mise à jour instantanée du graphique"),
                    tags$li(icon("check-circle", style = "color: #28a745;"), " Détection automatique des types de données"),
                    tags$li(icon("check-circle", style = "color: #28a745;"), " Personnalisation en temps réel")
                  )
                )
              ),
              
              fluidRow(
                
                box(
                  title = tagList(icon("sliders-h"), " Paramètres de Visualisation"),
                  status = "primary",
                  width = 4,
                  solidHeader = TRUE,
                  
                  div(
                    class = "well",
                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(icon("database"), " Sélection des Variables", 
                       style = "color: #007bff; margin-top: 0; font-size: 16px;"),
                    
                    uiOutput(ns("vizXVarSelect")),
                    
                    # Iddentiquer le type de variable X avec détection automatique
                    selectInput(ns("xVarType"),
                      tagList(icon("magic"), " Type de la variable X:"),
                      choices = c(
                        "Auto (détection automatique)" = "auto",
                        "Date/Temporelle" = "date",
                        "Catégorielle" = "categorical",
                        "Texte libre" = "text",
                        "Facteur" = "factor",
                        "Numérique continue" = "numeric"
                      ),
                      selected = "auto"
                    ),
                    helpText(
                      icon("info-circle", style = "color: #17a2b8;"),
                      "Le mode 'Auto' détecte automatiquement le type optimal pour votre variable."
                    )
                  ),
                  
                  conditionalPanel(
              ns = ns,
                    condition = "true",
                    div(
                      class = "well",
                      style = "background-color: #fff3e0; border-left: 4px solid #ff9800; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      h4(
                        icon("edit"), " Personnalisation des Étiquettes",
                        style = "color: #ff9800; font-weight: bold; margin-top: 0; font-size: 15px;"
                      ),
                      p("Modifiez les noms des catégories pour améliorer la lisibilité de votre graphique.",
                        style = "font-size: 13px; color: #666; margin-bottom: 10px;"),
                      uiOutput(ns("xLevelsEditor")),
                      helpText(
                        icon("lightbulb", style = "color: #ffc107;"),
                        "Les modifications sont conservées jusqu'à la fermeture de l'application."
                      )
                    )
                  ),
                  
                  conditionalPanel(
              ns = ns,
                    condition = "input.vizType != 'histogram' && input.vizType != 'density' && input.vizType != 'pie' && input.vizType != 'donut' && input.vizType != 'treemap'",
                    div(
                      class = "well",
                      style = "background-color: #e8f5e9; border-left: 4px solid #28a745; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      h4(
                        icon("sort"), " Ordre des Catégories (Axe X)",
                        style = "color: #28a745; font-weight: bold; margin-top: 0; font-size: 15px;"
                      ),
                      p("Glissez-déposez pour réorganiser l'ordre d'affichage sur l'axe X.",
                        style = "font-size: 13px; color: #666; margin-bottom: 10px;"),
                      uiOutput(ns("xOrderEditor")),
                      
                    )
                  ),
                  
                  conditionalPanel(
              ns = ns,
                    condition = "input.xVarType == 'date'",
                    div(
                      class = "well",
                      style = "background-color: #e3f2fd; border-left: 4px solid #2196F3; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      h4(
                        icon("calendar-alt"), " Paramètres de Date",
                        style = "color: #2196F3; font-weight: bold; margin-top: 0; font-size: 15px;"
                      ),
                      selectInput(ns("xDateFormat"),
                        tagList(icon("file-import"), " Format des données source :"),
                        choices = c(
                          "AAAA-MM-JJ (ISO 8601)" = "%Y-%m-%d",
                          "JJ/MM/AAAA (France)"   = "%d/%m/%Y",
                          "MM/JJ/AAAA (US)"       = "%m/%d/%Y",
                          "AAAA/MM/JJ"            = "%Y/%m/%d",
                          "JJ-Mois-AAAA"          = "%d-%b-%Y",
                          "Mois JJ, AAAA"         = "%B %d, %Y"
                        ),
                        selected = "%Y-%m-%d"
                      ),
                      helpText(
                        icon("exclamation-triangle", style = "color: #ffc107;"),
                        "Format des dates dans vos données brutes (pour la conversion)."
                      ),
                      
                      hr(style = "margin: 10px 0;"),
                      
                      selectInput(ns("xDateDisplayFormat"),
                        tagList(icon("eye"), " Format d'affichage sur l'axe :"),
                        choices = c(
                          "- Formats chiffres -" = "",
                          "JJ-MM-AAAA (ex: 25-03-2024)"  = "%d-%m-%Y",
                          "MM-JJ-AAAA (ex: 03-25-2024)"  = "%m-%d-%Y",
                          "AAAA-MM-JJ (ex: 2024-03-25)"  = "%Y-%m-%d",
                          "AAAA-JJ-MM (ex: 2024-25-03)"  = "%Y-%d-%m",
                          "JJ/MM/AAAA (ex: 25/03/2024)"  = "%d/%m/%Y",
                          "MM/JJ/AAAA (ex: 03/25/2024)"  = "%m/%d/%Y",
                          "JJ-MM (ex: 25-03)"            = "%d-%m",
                          "MM-JJ (ex: 03-25)"            = "%m-%d",
                          "MM-AAAA (ex: 03-2024)"        = "%m-%Y",
                          "AAAA-MM (ex: 2024-03)"        = "%Y-%m",
                          "- Mois abrégé -" = "",
                          "JJ-Mois-AAAA (ex: 25-Mar-2024)"        = "%d-%b-%Y",
                          "Mois-AAAA (ex: Mar-2024)"              = "%b-%Y",
                          "JJ-Mois (ex: 25-Mar)"                  = "%d-%b",
                          "Mois-JJ (ex: Mar-25)"                  = "%b-%d",
                          "AAAA-Mois-JJ (ex: 2024-Mar-25)"        = "%Y-%b-%d",
                          "- Mois entier -" = "",
                          "JJ Mois AAAA (ex: 25 Mars 2024)"       = "%d %B %Y",
                          "Mois AAAA (ex: Mars 2024)"             = "%B %Y",
                          "JJ Mois (ex: 25 Mars)"                 = "%d %B",
                          "Mois JJ (ex: Mars 25)"                 = "%B %d",
                          "AAAA Mois (ex: 2024 Mars)"             = "%Y %B"
                        ),
                        selected = "%d-%m-%Y"
                      ),
                      helpText(
                        icon("info-circle", style = "color: #2196F3;"),
                        "Format d'affichage des dates sur l'axe X du graphique.",
                        tags$br(),
                        tags$small(
                          style = "color: #888;",
                          "Mois abrégé = Jan, Fév, Mar... | Mois entier = Janvier, Février..."
                        )
                      )
                    )
                  ),
                  
                  div(
                    class = "well",
                    style = "background-color: #f1f8e9; border-left: 4px solid #8bc34a; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(
                      icon("chart-bar"), " Variable(s) à Visualiser",
                      style = "color: #689f38; font-weight: bold; margin-top: 0; font-size: 15px;"
                    ),
                    uiOutput(ns("vizYVarSelect")),
                    conditionalPanel(
              ns = ns,
                      condition = "output.multiYIndicator === true",
                      div(
                        style = "margin-top: 10px; padding: 8px; background-color: #c8e6c9; border-radius: 4px; border: 1px solid #81c784;",
                        icon("layer-group", style = "color: #388e3c;"),
                        span(
                          id = "multiYBadge",
                          style = "font-weight: bold; color: #1b5e20; margin-left: 5px;",
                          "Mode multi-variables activé"
                        )
                      )
                    ),
                  ),
                  
                  div(
                    class = "well",
                    style = "background-color: #fce4ec; border-left: 4px solid #e91e63; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(
                      icon("palette"), " Variables Supplémentaires",
                      style = "color: #c2185b; font-weight: bold; margin-top: 0; font-size: 15px;"
                    ),
                    
                    uiOutput(ns("vizColorVarSelect")),
                    
                    # Couleur fixe de la courbe (quand pas de variable couleur)
                    conditionalPanel(
              ns = ns,
                      condition = "input.vizColorVar == 'Aucun' || input.vizColorVar == null",
                      div(
                        style = "margin-top:8px;padding:10px;background:#f3e5f5;border-radius:6px;border-left:3px solid #9c27b0;",
                        h6(tagList(icon("paint-brush")," Couleur de la courbe / points"),
                           style="color:#6a1b9a;font-weight:bold;margin-top:0;margin-bottom:6px;font-size:12px;"),
                        colourInput(ns("lineFixedColor"), NULL, value="#2196F3", showColour="background"),
                        helpText(style="font-size:11px;color:#666;margin-top:4px;",
                                 icon("info-circle"),
                                 " Couleur appliquée à la courbe, aux points et au lissage.")
                      )
                    ),
                    
                    uiOutput(ns("vizFacetVarSelect")),
                    
                    conditionalPanel(
              ns = ns,
                      condition = "input.vizFacetVar != 'Aucun' && input.vizFacetVar != null",
                      div(
                        style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 4px;",
                        icon("exclamation-triangle", style = "color: #ff9800;"),
                        span(
                          style = "font-size: 12px; color: #856404; margin-left: 5px;",
                          "Utilisez une variable avec 2-10 catégories maximum et sans valeurs manquantes."
                        )
                      )
                    ),
                    
                    conditionalPanel(
              ns = ns,
                      condition = "input.vizFacetVar != 'Aucun' && input.vizFacetVar != null",
                      div(
                        style = "margin-top: 10px;",
                        checkboxInput(ns("facetScalesFree"),
                          tagList(icon("expand"), " Échelles libres pour chaque facette"),
                          value = FALSE
                        ),
                        helpText("Permet à chaque sous-graphique d'avoir ses propres limites d'axes.")
                      )
                    )
                  ),
                  
                  div(
                    class = "well",
                    style = "background-color: #e1f5fe; border-left: 4px solid #03a9f4; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(
                      icon("chart-pie"), " Type de Graphique",
                      style = "color: #0288d1; font-weight: bold; margin-top: 0; font-size: 15px;"
                    ),
                    selectInput(ns("vizType"),
                      "Sélectionnez le type:",
                      choices = c(
                        "Nuage de points (Scatter)" = "scatter",
                        "Courbe avec lissage (Seasonal Smooth)" = "seasonal_smooth",
                        "Courbe d'évolution (Seasonal Evolution)" = "seasonal_evolution",
                        "Boîte à moustaches (Boxplot)" = "box",
                        "Graphique en violon (Violin)" = "violin",
                        "Diagramme en barres (Bar)" = "bar",
                        "Graphique en lignes (Line)" = "line",
                        "Densité de distribution (Density)" = "density",
                        "Histogramme" = "histogram",
                        "Carte de chaleur (Heatmap)" = "heatmap",
                        "Aires empilées (Area)" = "area",
                        "Diagramme circulaire (Pie)" = "pie",
                        "Graphique en anneau (Donut)" = "donut",
                        "Carte proportionnelle (Treemap)" = "treemap"
                      ),
                      selected = "scatter"
                    ),
                    
                    conditionalPanel(
              ns = ns,
                      condition = "input.vizType == 'scatter' || input.vizType == 'line' || input.vizType == 'area' || input.vizType == 'bar' || input.vizType == 'seasonal_smooth' || input.vizType == 'seasonal_evolution'",
                      div(
                        style = "margin-top: 8px; padding: 6px 10px; background-color: #d4edda; border-radius: 4px; font-size: 12px; display: inline-block; border: 1px solid #c3e6cb;",
                        icon("layer-group", style = "color: #28a745;"),
                        span(
                          style = "color: #155724; margin-left: 5px; font-weight: 600;",
                          "Compatible multi-variables Y"
                        )
                      )
                    )
                  ),
                  
                  div(
                    class = "well",
                    style = "background-color: #fff8e1; border-left: 4px solid #ffc107; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    h4(
                      icon("calculator"), " Agrégation des Données",
                      style = "color: #f57c00; font-weight: bold; margin-top: 0; font-size: 15px;"
                    ),
                    checkboxInput(ns("useAggregation"),
                      tagList(icon("check-square"), " Activer l'agrégation"),
                      value = FALSE
                    ),
                    
                    conditionalPanel(
              ns = ns,
                      condition = "input.useAggregation == true",
                      selectInput(ns("aggFunction"),
                        "Fonction d'agrégation:",
                        choices = c(
                          "Moyenne" = "mean",
                          "Médiane" = "median",
                          "Somme" = "sum",
                          "Comptage" = "count",
                          "Minimum" = "min",
                          "Maximum" = "max",
                          "Écart-type" = "sd"
                        ),
                        selected = "mean"
                      ),
                      uiOutput(ns("groupVarsSelect")),
                      div(
                        style = "margin-top: 10px; padding: 8px; background-color: #d1ecf1; border-radius: 4px;",
                        icon("info-circle", style = "color: #0c5460;"),
                        span(
                          style = "font-size: 12px; color: #0c5460; margin-left: 5px;",
                          "L'agrégation résume vos données selon la fonction choisie."
                        )
                      ),
                      verbatimTextOutput(ns("aggregationInfo"))
                    )
                  ),
                  
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    actionButton(ns("refreshPlot"),
                      tagList(icon("sync-alt"), " Actualiser le Graphique"),
                      class = "btn-success btn-lg",
                      style = "width: 100%; font-weight: bold;"
                    )
                  )
                ),
                
                box(
                  title = tagList(icon("chart-area"), " Graphique "),
                  status = "success",
                  width = 8,
                  solidHeader = TRUE,
                  
                  div(
                    style = "position: relative; min-height: 500px;",
                    
                    div(
                      id = "plotLoader",
                      style = "display: none; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); text-align: center; z-index: 1000;",
                      div(
                        style = "font-size: 48px; color: #007bff;",
                        icon("spinner", class = "fa-spin")
                      ),
                      p("Chargement du graphique...", 
                        style = "margin-top: 10px; font-size: 16px; color: #666;")
                    ),
                    
                    plotlyOutput(ns("interactivePlot"), height = "600px")
                  ),
                  
                  div(
                    style = "margin-top: 15px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
                    div(
                      style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
                      div(
                        style = "display: flex; gap: 15px; align-items: center;",
                        div(
                          style = "padding: 5px 10px; background-color: #d4edda; border-radius: 4px; font-size: 12px; border: 1px solid #c3e6cb;",
                          icon("check-circle", style = "color: #28a745;"),
                          span(
                            style = "margin-left: 5px; color: #155724; font-weight: 600;",
                            "Mise à jour auto"
                          )
                        ),
                        div(
                          id = "lastUpdateTime",
                          style = "font-size: 12px; color: #666;",
                          paste("Dernière mise à jour:", format(Sys.time(), "%H:%M:%S"))
                        )
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = tagList(icon("download"), " Exporter le graphique"),
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  
                  # Ligne 1: Format et DPI
                  fluidRow(
                    column(4,
                           selectInput(ns("exportFormat"), 
                                       label = tagList(icon("file-image"), " Format:"),
                                       choices = c(
                                         "PNG" = "png",
                                         "JPEG" = "jpeg",
                                         "TIFF" = "tiff",
                                         "BMP" = "bmp",
                                         "PDF" = "pdf",
                                         "SVG" = "svg",
                                         "EPS" = "eps"
                                       ),
                                       selected = "png")
                    ),
                    column(4,
                           numericInput(ns("exportDPI"), 
                                        label = tagList(icon("sliders-h"), " DPI:"),
                                        value = 300, min = 300, max = 20000, step = 50)
                    ),
                    column(4,
                           tags$label(tagList(icon("ruler-combined"), " Dimensions:")),
                           uiOutput(ns("calculatedDimensions"))
                    )
                  ),
                  
                  # Ligne 2: Options JPEG/TIFF
                  conditionalPanel(
              ns = ns,
                    condition = "input.exportFormat == 'jpeg'",
                    fluidRow(
                      column(6,
                             sliderInput(ns("jpegQuality"), "Qualité JPEG:", 
                                         min = 50, max = 100, value = 95, step = 5)
                      )
                    )
                  ),
                  conditionalPanel(
              ns = ns,
                    condition = "input.exportFormat == 'tiff'",
                    fluidRow(
                      column(6,
                             selectInput(ns("tiffCompression"), "Compression TIFF:",
                                         choices = c("Aucune" = "none", "LZW" = "lzw", "ZIP" = "zip"),
                                         selected = "lzw")
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(12,
                           div(style = "margin-top: 15px; text-align: center;",
                               actionButton(ns("downloadPlotBtn"), 
                                            label = tagList(icon("download"), " Télécharger le graphique"),
                                            class = "btn-success btn-lg",
                                            style = "padding: 15px 50px; font-size: 16px;")
                           )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = tagList(icon("cog"), " Options Avancées de Personnalisation"),
                  status = "warning",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  fluidRow(
                    
                    # COLONNE 1 : TEXTES, LABELS ET LEGENDE 
                    column(
                      width = 4,
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px;",
                        
                        h4(icon("font"), " Textes, Labels et Légende",
                           style = "color: #9c27b0; font-weight: bold; border-bottom: 3px solid #9c27b0; padding-bottom: 10px; margin-top: 0;"),
                        
                        div(
                          style = "margin-bottom: 18px; padding: 12px; background-color: #f3e5f5; border-radius: 6px;",
                          h5(icon("heading"), " Titre",
                             style = "color: #6a1b9a; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          textInput(ns("plotTitle"),
                            "Titre principal:",
                            value = "",
                            placeholder = "Titre du graphique..."
                          )
                        ),
                        
                        div(
                          style = "margin-bottom: 18px; padding: 12px; background-color: #ede7f6; border-radius: 6px;",
                          h5(icon("text-width"), " Tailles de police",
                             style = "color: #4a148c; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          sliderInput(ns("baseFontSize"),
                            "Police de base:",
                            min = 8, max = 20, value = 12, step = 1
                          ),
                          sliderInput(ns("titleSize"),
                            "Titre du graphique:",
                            min = 10, max = 24, value = 14, step = 1
                          ),
                          sliderInput(ns("axisLabelSize"),
                            "Labels des axes:",
                            min = 8, max = 18, value = 11, step = 1
                          )
                        ),
                        
                        div(
                          style = "margin-bottom: 18px; padding: 12px; background-color: #e8eaf6; border-radius: 6px;",
                          h5(icon("arrows-alt-h"), " Labels des axes",
                             style = "color: #283593; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          textInput(ns("xAxisLabel"),
                            "Label axe X:",
                            value = "",
                            placeholder = "Auto"
                          ),
                          textInput(ns("yAxisLabel"),
                            "Label axe Y:",
                            value = "",
                            placeholder = "Auto"
                          )
                        ),
                        
                        div(
                          style = "padding: 12px; background-color: #fce4ec; border-radius: 6px;",
                          h5(icon("list"), " Légende",
                             style = "color: #880e4f; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 10px;"),
                          textInput(ns("legendTitle"),
                            "Titre de la légende:",
                            value = "",
                            placeholder = "Auto"
                          ),
                          sliderInput(ns("legendTitleSize"),
                            "Taille du titre:",
                            min = 8, max = 22, value = 12, step = 1
                          ),
                          sliderInput(ns("legendTextSize"),
                            "Taille des entrées:",
                            min = 6, max = 20, value = 10, step = 1
                          ),
                          sliderInput(ns("legendKeySize"),
                            "Taille des symboles:",
                            min = 0.3, max = 3, value = 1, step = 0.1
                          ),
                          selectInput(ns("legendPosition"),
                            "Position:",
                            choices = c(
                              "Droite" = "right",
                              "Gauche" = "left",
                              "Haut" = "top",
                              "Bas" = "bottom",
                              "Aucune" = "none"
                            ),
                            selected = "right"
                          ),
                          div(
                            style = "margin-top: 8px;",
                            # Bouton toujours visible (fonctionne pour color var ET multi-Y)
                            actionButton(ns("customizeLegendLabels"),
                              tagList(icon("tags"), " Modifier les labels de légende"),
                              class = "btn-sm btn-warning btn-block",
                              style = "font-weight:600; letter-spacing:0.3px;",
                              title = "Ouvre un éditeur pour renommer chaque niveau de la légende"
                            ),
                            div(
                              style = "margin-top:5px; padding:5px 8px; background:#fff8e1; border-radius:4px; font-size:11px; color:#7f6000; border-left:3px solid #ff9800;",
                              icon("lightbulb", style="color:#ff9800;"),
                              " Cliquez pour renommer A->Groupe 1, Ctrl->Traitement...",
                              br(),
                              tags$small(style="color:#aaa;", "Fonctionne pour variable couleur et Y multiples.")
                            ),
                            uiOutput(ns("legendLabelsStatus"))
                          )
                        )
                      )
                    ),
                    
                    #  COLONNE 2 : AXES - FORMATAGE ET ECHELLE 
                    column(
                      width = 4,
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px;",
                        
                        h4(icon("ruler-combined"), " Axes : Formatage et Échelle",
                           style = "color: #ff5722; font-weight: bold; border-bottom: 3px solid #ff5722; padding-bottom: 10px; margin-top: 0;"),
                        
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #e3f2fd; border-radius: 6px;",
                          h5(icon("long-arrow-alt-right"), " Style label axe X",
                             style = "color: #495057; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput(ns("xAxisBold"),   tagList(icon("bold"),   " Gras"),    value = FALSE),
                            checkboxInput(ns("xAxisItalic"), tagList(icon("italic"), " Italique"), value = FALSE)
                          )
                        ),
                        
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #f3e5f5; border-radius: 6px;",
                          h5(icon("long-arrow-alt-up"), " Style label axe Y",
                             style = "color: #7b1fa2; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput(ns("yAxisBold"),   tagList(icon("bold"),   " Gras"),    value = FALSE),
                            checkboxInput(ns("yAxisItalic"), tagList(icon("italic"), " Italique"), value = FALSE)
                          )
                        ),
                        
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #fff3e0; border-radius: 6px;",
                          h5(icon("tag"), " Niveaux axe X",
                             style = "color: #f57c00; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput(ns("xTickBold"),   tagList(icon("bold"),   " Gras"),    value = FALSE),
                            checkboxInput(ns("xTickItalic"), tagList(icon("italic"), " Italique"), value = FALSE)
                          ),
                          sliderInput(ns("xTickSize"),  "Taille:",           min = 6, max = 20, value = 10, step = 1),
                          sliderInput(ns("xAxisAngle"), "Angle (°):", min = 0, max = 90, value = 0, step = 15)
                        ),
                        
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #ede7f6; border-radius: 6px;",
                          h5(icon("tag"), " Niveaux axe Y",
                             style = "color: #6a1b9a; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 10px;",
                            checkboxInput(ns("yTickBold"),   tagList(icon("bold"),   " Gras"),    value = FALSE),
                            checkboxInput(ns("yTickItalic"), tagList(icon("italic"), " Italique"), value = FALSE)
                          ),
                          sliderInput(ns("yTickSize"), "Taille:", min = 6, max = 20, value = 10, step = 1)
                        ),
                        
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #e8f5e9; border-radius: 6px;",
                          h5(icon("ruler"), " Graduations des axes",
                             style = "color: #2e7d32; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          checkboxInput(ns("customAxisBreaks"),
                            tagList(icon("sliders-h"), " Personnaliser les graduations"),
                            value = FALSE
                          ),
                          conditionalPanel(
              ns = ns,
                            condition = "input.customAxisBreaks == true",
                            div(
                              style = "margin-top: 8px;",
                              numericInput(ns("yAxisBreakStep"),
                                tagList(icon("long-arrow-alt-up"), " Pas axe Y:"),
                                value = NA, min = 0.001, step = 1
                              ),
                              helpText(icon("info-circle"), "Ex: 10 -> graduations 0, 10, 20...",
                                       style = "font-size: 11px; color: #555;"),
                              numericInput(ns("xAxisBreakStep"),
                                tagList(icon("long-arrow-alt-right"), " Pas axe X (numérique):"),
                                value = NA, min = 0.001, step = 1
                              ),
                              helpText(icon("info-circle"), "Uniquement si l'axe X est numérique.",
                                       style = "font-size: 11px; color: #555;")
                            )
                          )
                        ),
                        
                        div(
                          style = "margin-bottom: 15px; padding: 12px; background-color: #fce4ec; border-radius: 6px;",
                          h5(icon("minus"), " Traits des axes",
                             style = "color: #c62828; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          sliderInput(ns("axisLineSize"),
                            tagList(icon("ruler-horizontal"), " Epaisseur des axes:"),
                            min = 0, max = 3, value = 0.8, step = 0.1
                          ),
                          helpText(icon("info-circle"), "0 = axes invisibles. Les traits sont toujours noirs.",
                                   style = "font-size: 11px; color: #555;")
                        ),
                        
                        div(
                          style = "padding: 12px; background-color: #e0f2f1; border-radius: 6px;",
                          h5(icon("compress-arrows-alt"), " Limites des axes (min / max)",
                             style = "color: #00695c; font-size: 13px; font-weight: bold; margin-top: 0; margin-bottom: 8px;"),
                          helpText(icon("info-circle"), "Laissez vide pour les limites automatiques.",
                                   style = "font-size: 11px; color: #555; margin-bottom: 8px;"),
                          div(
                            style = "display: flex; gap: 8px;",
                            numericInput(ns("yAxisMin"), tagList(icon("long-arrow-alt-up"),    " Y min:"), value = NA),
                            numericInput(ns("yAxisMax"), tagList(icon("long-arrow-alt-up"),    " Y max:"), value = NA)
                          ),
                          div(
                            style = "display: flex; gap: 8px; margin-top: 8px;",
                            numericInput(ns("xAxisMin"), tagList(icon("long-arrow-alt-right"), " X min:"), value = NA),
                            numericInput(ns("xAxisMax"), tagList(icon("long-arrow-alt-right"), " X max:"), value = NA)
                          ),
                          helpText(icon("info-circle"), "Axe X uniquement pour variables numériques.",
                                   style = "font-size: 11px; color: #555; margin-top: 6px;")
                        ),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "output.multiYIndicator == true",
                          div(
                            style = "margin-top:15px; padding:15px; background:linear-gradient(135deg,#fff8e1,#fffde7); border:2px solid #ff9800; border-radius:8px;",
                            h5(
                              icon("chart-line", style="color:#e65100;"),
                              tags$b(style="color:#e65100; margin-left:4px;", " Axe Y Secondaire (droite)"),
                              style = "margin:0 0 12px 0; font-size:13px; font-weight:bold;"
                            ),
                            
                            uiOutput(ns("vizY2VarSelect")),
                            
                            hr(style="border-color:#ffe0b2; margin:10px 0;"),
                            
                            # Type de graphique Y2 -- choisi independamment de Y1
                            div(
                              style = "margin-bottom:12px; padding:10px; background:#fff3e0; border-radius:6px; border-left:3px solid #ff9800;",
                              h6(icon("chart-pie"), " Type de graphique Y2",
                                 style = "color:#e65100; font-size:12px; font-weight:bold; margin:0 0 8px 0;"),
                              selectInput(ns("vizY2Type"), NULL,
                                choices = c(
                                  "Courbe (Line)"                            = "line",
                                  "Points (Scatter)"                         = "scatter",
                                  "Points + Courbe"                          = "points_line",
                                  "Courbe d'évolution (Seasonal Evolution)"  = "seasonal_evolution",
                                  "Courbe lissee (Smooth)"                   = "smooth",
                                  "Aires (Area)"                             = "area",
                                  "Barres (Bar)"                             = "bar",
                                  "Boxplot"                                  = "box",
                                  "Violon (Violin)"                          = "violin",
                                  "Barres d'erreur (Mean +/- SD)"            = "errorbar"
                                ),
                                selected = "line"
                              ),
                              helpText(icon("info-circle"),
                                       " Y2 utilise la même agregation que Y1.",
                                       style = "font-size:11px; color:#888;")
                            ),
                            
                            # - Note miroir Y1 -
                            div(
                              style = "margin-bottom:12px; padding:10px; background:#fff8e1; border-radius:6px; border-left:3px solid #ff9800;",
                              tags$p(
                                icon("sync-alt", style="color:#e65100;"),
                                tags$b(style="color:#e65100;", " Mise en forme automatique"),
                                style = "font-size:12px; margin:0 0 4px 0;"
                              ),
                              tags$p(
                                "Taille, police (gras/italique) et épaisseur de l'axe Y2 suivent automatiquement les paramètres de l'axe Y principal.",
                                style = "font-size:11px; color:#555; margin:0;"
                              )
                            ),
                            
                            div(
                              style = "margin-bottom:12px; padding:10px; background:#fff3e0; border-radius:6px;",
                              h6(icon("font"), " Nom du label axe Y2",
                                 style = "color:#e65100; font-size:12px; font-weight:bold; margin:0 0 8px 0;"),
                              textInput(ns("y2AxisLabel"), "Nom de l'axe Y2:", placeholder = "ex: Température (°C)")
                            ),
                            
                            div(
                              style = "margin-bottom:12px; padding:10px; background:#fff3e0; border-radius:6px;",
                              h6(icon("chart-line"), " Épaisseur courbe Y2",
                                 style = "color:#e65100; font-size:12px; font-weight:bold; margin:0 0 8px 0;"),
                              sliderInput(ns("y2CurveWidth"),
                                tagList(icon("minus"), " Épaisseur courbe :"),
                                min = 0.5, max = 5, value = 1.2, step = 0.5
                              ),
                              helpText(icon("info-circle"), "Indépendant de 'Épaisseur des lignes' Y1.",
                                       style = "font-size:11px; color:#888;")
                            ),
                            
                            div(
                              style = "margin-bottom:12px; padding:10px; background:#fff3e0; border-radius:6px;",
                              h6(icon("ruler"), " Graduations axe Y2",
                                 style = "color:#e65100; font-size:12px; font-weight:bold; margin:0 0 8px 0;"),
                              numericInput(ns("y2AxisBreakStep"),
                                tagList(icon("long-arrow-alt-up"), " Pas (intervalle):"),
                                value = NA, min = 0.001, step = 1
                              ),
                              helpText(icon("info-circle"), "Ex: 5 -> graduations 0, 5, 10...",
                                       style = "font-size:11px; color:#888;")
                            ),
                            
                            div(
                              style = "padding:10px; background:#fff3e0; border-radius:6px;",
                              h6(icon("compress-arrows-alt"), " Limites axe Y2 (min / max)",
                                 style = "color:#e65100; font-size:12px; font-weight:bold; margin:0 0 8px 0;"),
                              helpText(icon("info-circle"), "Vide = automatique.",
                                       style = "font-size:11px; color:#888; margin-bottom:6px;"),
                              div(
                                style = "display:flex; gap:8px;",
                                numericInput(ns("y2AxisMin"), tagList(icon("long-arrow-alt-down"), " Y2 min:"), value = NA),
                                numericInput(ns("y2AxisMax"), tagList(icon("long-arrow-alt-up"),   " Y2 max:"), value = NA)
                              )
                            )
                          )
                        )
                      )
                    ),
                    
                    # COLONNE 3 : APPARENCE ET ELEMENTS DU GRAPHIQUE 
                    column(
                      width = 4,
                      
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px; margin-bottom: 15px;",
                        
                        h4(icon("palette"), " Apparence Visuelle",
                           style = "color: #ff9800; font-weight: bold; border-bottom: 3px solid #ff9800; padding-bottom: 10px; margin-top: 0;"),
                        
                        sliderInput(ns("pointSize"),
                          tagList(icon("circle"), " Taille des points:"),
                          min = 1, max = 10, value = 3, step = 0.5
                        ),
                        sliderInput(ns("pointAlpha"),
                          tagList(icon("adjust"), " Transparence des points:"),
                          min = 0, max = 1, value = 0.7, step = 0.1
                        ),
                        sliderInput(ns("lineWidth"),
                          tagList(icon("minus"), " Épaisseur des lignes:"),
                          min = 0.5, max = 5, value = 1, step = 0.5
                        ),
                        
                        div(
                          style = "margin-bottom:15px; padding:12px; background-color:#e8f5e9; border-radius:6px;",
                          h5(icon("expand-arrows-alt"), " Marges du graphique",
                             style = "color:#2e7d32; font-size:13px; font-weight:bold; margin-top:0; margin-bottom:10px;"),
                          helpText(icon("info-circle"), "Espace entre le graphique et ses bordures (en px).",
                                   style = "font-size:11px; color:#555; margin-bottom:8px;"),
                          div(
                            style = "display:grid; grid-template-columns:1fr 1fr; gap:6px;",
                            numericInput(ns("plotMarginTop"),    tagList(icon("arrow-up"),    " Haut:"),   value = 10, min = 0, max = 120, step = 5),
                            numericInput(ns("plotMarginRight"),  tagList(icon("arrow-right"), " Droite:"), value = 30, min = 0, max = 120, step = 5),
                            numericInput(ns("plotMarginBottom"), tagList(icon("arrow-down"),  " Bas:"),    value = 10, min = 0, max = 120, step = 5),
                            numericInput(ns("plotMarginLeft"),   tagList(icon("arrow-left"),  " Gauche:"), value = 10, min = 0, max = 120, step = 5)
                          )
                        ),
                        
                        div(
                          style = "margin-top: 15px; padding: 10px; background-color: #e8eaf6; border-radius: 6px; border-left: 3px solid #3f51b5;",
                          h6(
                            tagList(icon("fill-drip"), " Arrière-plan du graphique"),
                            style = "color: #283593; font-weight: bold; margin-top: 0; margin-bottom: 8px; font-size: 12px;"
                          ),
                          selectInput(ns("plotTheme"),
                            NULL,
                            choices = c(
                              "Minimal (blanc, grille grise)"   = "minimal",
                              "Classique (blanc, sans grille)"  = "classic",
                              "Blanc & grille complète"         = "bw",
                              "Fond blanc lumineux"             = "light",
                              "Fond gris doux"                  = "gray",
                              "Fond sombre (dark)"              = "dark",
                              "Fond noir total"                 = "void",
                              "Ligne de base seulement"         = "linedraw"
                            ),
                            selected = "minimal"
                          ),
                          div(
                            style = "margin-top: 6px; display: flex; gap: 6px; flex-wrap: wrap;",
                            actionButton(ns("previewThemeBtn"), tagList(icon("eye"), " Aperçu"),
                                         class = "btn-xs btn-info")
                          )
                        ),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.vizType == 'bar'",
                          div(
                            style = "margin-top: 15px; padding: 10px; background-color: #e8f5e9; border-radius: 6px;",
                            h5(icon("chart-bar"), " Options Barres",
                               style = "color: #388e3c; font-size: 13px; font-weight: bold; margin-top: 0;"),
                            sliderInput(ns("barWidth"),  "Largeur:", min = 0.3, max = 1, value = 0.8, step = 0.1),
                            selectInput(ns("barPosition"), "Position:",
                              choices = c("Côte à côte" = "dodge", "Empilées" = "stack", "Remplissage" = "fill"),
                              selected = "dodge"
                            )
                          )
                        ),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.vizType == 'area'",
                          div(
                            style = "margin-top: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 6px;",
                            h5(icon("chart-area"), " Options Aires",
                               style = "color: #495057; font-size: 13px; font-weight: bold; margin-top: 0;"),
                            selectInput(ns("areaPosition"), "Position des aires :",
                              choices = c(
                                "Empilées" = "stack",
                                "Côte à côte" = "dodge",
                                "Remplissage (100%)" = "fill",
                                "Identité (superposées)" = "identity"
                              ),
                              selected = "stack"
                            )
                          )
                        ),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.vizType == 'histogram'",
                          div(
                            style = "margin-top: 15px; padding: 10px; background-color: #e1f5fe; border-radius: 6px;",
                            h5(icon("chart-area"), " Options Histogramme",
                               style = "color: #0277bd; font-size: 13px; font-weight: bold; margin-top: 0;"),
                            sliderInput(ns("histBins"), "Nombre de bins:", min = 10, max = 100, value = 30, step = 5),
                            colourInput(ns("histColor"), "Couleur:", value = "steelblue")
                          )
                        )
                      ),
                      
                      div(
                        class = "well",
                        style = "background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%); border: 2px solid #e0e0e0; border-radius: 8px; padding: 20px;",
                        
                        h4(icon("layer-group"), " Éléments du Graphique",
                           style = "color: #2196F3; font-weight: bold; border-bottom: 3px solid #2196F3; padding-bottom: 10px; margin-top: 0;"),
                        
                        checkboxInput(ns("showPoints"), tagList(icon("circle"),  " Afficher les points"), value = TRUE),
                        
                        checkboxInput(ns("showValues"), tagList(icon("hashtag"), " Afficher les valeurs"), value = FALSE),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.showValues == true",
                          div(
                            style = "margin-top: 8px; padding: 12px; background-color: #e8f4fd; border-radius: 6px; border-left: 3px solid #3498db;",
                            h6(icon("sliders-h"), " Options des valeurs",
                               style = "color: #343a40; font-weight: bold; margin-top: 0;"),
                            sliderInput(ns("valueLabelSize"), "Taille:", min = 2, max = 10, value = 3, step = 0.5),
                            selectInput(ns("valueLabelPosition"), "Position:",
                              choices = c("Au-dessus" = "above", "En-dessous" = "below", "Au centre" = "center",
                                          "A droite" = "right", "A gauche" = "left"),
                              selected = "above"
                            ),
                            colourInput(ns("valueLabelColor"), "Couleur:", value = "#333333"),
                            div(
                              style = "display: flex; gap: 10px;",
                              checkboxInput(ns("valueLabelBold"),   tagList(icon("bold"),   " Gras"),    value = FALSE),
                              checkboxInput(ns("valueLabelItalic"), tagList(icon("italic"), " Italique"), value = FALSE)
                            ),
                            numericInput(ns("valueLabelDigits"), "Décimales:", value = 2, min = 0, max = 6, step = 1)
                          )
                        ),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.vizType == 'line' || input.vizType == 'area'",
                          div(
                            style = "margin-top: 10px; padding: 12px; background-color: #fff3e0; border-radius: 6px; border-left: 3px solid #ff9800;",
                            h6(tagList(icon("exclamation-triangle"), " Gestion des valeurs manquantes (NA)"),
                               style = "color: #e65100; font-weight: bold; margin-top: 0; font-size: 12px;"),
                            checkboxInput(ns("lineConnectNA"),
                                          tagList(icon("link"), " Connecter par-dessus les NA"),
                                          value = FALSE),
                            helpText(style = "font-size: 11px; color: #666;",
                                     icon("info-circle"),
                                     " Coché : la courbe relie les points valides en ignorant les NA.",
                                     " Décoché : la courbe est interrompue aux NA (trou visible)."),
                            checkboxInput(ns("lineShowNAMarker"),
                                          tagList(icon("times"), " Marquer les positions NA (x)"),
                                          value = FALSE),
                            helpText(style = "font-size: 11px; color: #666;",
                                     "Affiche une croix grise à chaque position NA en bas du graphique.")
                          )
                        ),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.vizType == 'scatter' || input.vizType == 'line'",
                          checkboxInput(ns("showTrendLine"), tagList(icon("chart-line"), " Ligne de tendance"), value = FALSE),
                          conditionalPanel(
              ns = ns,
                            condition = "input.showTrendLine == true",
                            selectInput(ns("trendMethod"), "Méthode:",
                              choices = c("Linéaire" = "lm", "LOESS" = "loess", "GAM" = "gam"),
                              selected = "lm"
                            )
                          )
                        ),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.vizType == 'seasonal_smooth'",
                          checkboxInput(ns("showSmoothLine"), tagList(icon("bezier-curve"), " Ligne de lissage"), value = TRUE),
                          conditionalPanel(
              ns = ns,
                            condition = "input.showSmoothLine == true",
                            selectInput(ns("smoothMethod"), "Méthode:",
                              choices = c("LOESS" = "loess", "Linéaire" = "lm", "GAM" = "gam"),
                              selected = "loess"
                            ),
                            sliderInput(ns("smoothSpan"), "Degré de lissage:", min = 0.1, max = 2, value = 0.75, step = 0.05)
                          )
                        ),
                        
                        checkboxInput(ns("showConfidenceInterval"), tagList(icon("area-chart"), " Intervalle de confiance"), value = TRUE),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.vizType == 'box'",
                          checkboxInput(ns("showOutliers"), tagList(icon("circle-notch"), " Afficher les outliers"), value = TRUE)
                        ),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.vizType == 'violin'",
                          checkboxInput(ns("showBoxInsideViolin"), tagList(icon("box"), " Boîte à l'intérieur"), value = FALSE)
                        )
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                conditionalPanel(
              ns = ns,
                  condition = "input.vizType == 'seasonal_smooth' || input.vizType == 'seasonal_evolution'",
                  box(
                    title = tagList(icon("calendar-alt"), " Analyse Saisonnière"),
                    status = "success",
                    width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    div(
                      style = "padding: 15px;",
                      uiOutput(ns("seasonalDuplicateWarning")),
                      verbatimTextOutput(ns("seasonalAnalysisSummary"))
                    )
                  )
                )
              ),
              
              tags$head(
                # Script SortableJS pour le drag-and-drop (servi en local depuis www/)
                tags$script(src = "Sortable.min.js"),
                
                tags$script(HTML("
    $(document).ready(function() {
      // Fonction pour initialiser Sortable (recrée toujours une instance fraiche)
      function initSortable() {
        var el = document.getElementById('xOrderSortable');
        if (!el) return;
        // Détruire l'ancienne instance si elle existe
        if (el.sortableInstance) {
          el.sortableInstance.destroy();
          el.sortableInstance = null;
        }
        el.sortableInstance = Sortable.create(el, {
          animation: 150,
          ghostClass: 'sortable-ghost',
          handle: '.sortable-item',
          onEnd: function(evt) {
            var items = el.querySelectorAll('.sortable-item');
            var order = [];
            items.forEach(function(item) {
              order.push(item.getAttribute('data-value'));
            });
            Shiny.setInputValue('xLevelOrder', order, {priority: 'event'});
          }
        });
      }
      
      // Initialiser au chargement
      setTimeout(initSortable, 500);
      
      // Réinitialiser chaque fois que xOrderEditor ou xLevelsEditor est mis à jour
      $(document).on('shiny:value', function(event) {
        if (event.target.id === 'xOrderEditor' || event.target.id === 'xLevelsEditor') {
          setTimeout(initSortable, 150);
        }
      });
      
      // Réinitialiser aussi après shiny:recalculated (au cas où le DOM est reconstruit)
      $(document).on('shiny:recalculated', function() {
        if (document.getElementById('xOrderSortable')) {
          setTimeout(initSortable, 200);
        }
      });
    });
  ")),
                
                tags$style(HTML("
    /* Styles pour le drag-and-drop */
    .sortable-ghost {
      opacity: 0.4;
      background-color: #e3f2fd;
    }
    
    /* BOUTON TÉLÉCHARGEMENT */
      pointer-events: auto !important;
      cursor: pointer !important;
      background-color: #28a745 !important;
      border-color: #28a745 !important;
      color: white !important;
    }
    
      background-color: #218838 !important;
      border-color: #1e7e34 !important;
      transform: scale(1.02);
    }
    
      transform: scale(0.98);
    }
    
      background-color: #6c757d !important;
      border-color: #6c757d !important;
      cursor: wait !important;
    }
    
    .sortable-item:hover {
      box-shadow: 0 4px 8px rgba(0,0,0,0.2) !important;
      border-color: #28a745 !important;
    }
  "))
              )
  )
}

mod_viz_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Depot des variables portees au graphique. Ici, et non dans le corps du
    # module : les selecteurs sont namespaces, `input$vizXVar` n'existe qu'a
    # l'interieur du moduleServer. Un graphique suggere une relation ;
    # l'aide a la decision dit quel test la formaliserait.
    observeEvent(list(input$vizXVar, input$vizYVar, input$vizType), {
      d <- values$filteredData %||% values$data
      if (is.null(d) || !NROW(d)) return()
      vars <- intersect(unique(c(input$vizXVar, input$vizYVar)), names(d))
      if (!length(vars)) return()
      grp <- intersect(input$vizColorVar %||% character(0), names(d))
      hstat_ai_capture(values, "Visualisation",
        sprintf("Graphique : %s", paste(vars, collapse = " x ")),
        meta = list(variables = vars, groupe = grp,
                    `type de graphique` = input$vizType))
    }, ignoreInit = TRUE)

  get_date_display_fmt <- function() {
    fmt <- input$xDateDisplayFormat %||% "%d-%m-%Y"
    if (viz_valid_date_fmt(fmt)) fmt else "%d-%m-%Y"
  }
  
  get_plot_theme <- function(base_size = 12) {
    viz_get_theme(input$plotTheme %||% "minimal", base_size = base_size)
  }
  
  get_x_scale <- function(data, x_var) {
    viz_get_x_scale(
      x_col      = data[[x_var]],
      disp_fmt   = get_date_display_fmt(),                       # format d'affichage
      label_map  = values$storedLevelLabels[[x_var]],            # renommage étiquettes
      custom_ord = values$customXOrder                           # ordre personnalisé
    )
  }

  
  
  output$vizXVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    all_cols <- iconv(all_cols, to = "UTF-8", sub = "")
    selectInput(ns("vizXVar"), "Variable X:", 
                choices = all_cols,
                selected = if(length(all_cols) > 0) all_cols[1] else NULL)
  })
  
  output$vizYVarSelect <- renderUI({
    req(values$filteredData)
    all_cols <- names(values$filteredData)
    all_cols <- iconv(all_cols, to = "UTF-8", sub = "")
    
    div(
      selectizeInput(ns("vizYVar"), "Variable(s) Y:", 
                     choices = all_cols,
                     selected = if(length(all_cols) > 1) all_cols[2] else NULL,
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Sélectionnez une ou plusieurs variables...',
                       maxItems = 50,
                       plugins = list('remove_button')
                     )),
      helpText(icon("info-circle"), "Sélectionnez une ou plusieurs variables Y (jusqu'à 50) pour les superposer sur le même graphique.")
    )
  })
  
  output$vizColorVarSelect <- renderUI({
    req(values$filteredData)
    
    all_cols <- names(values$filteredData)
    if(!is.null(values$multipleY) && values$multipleY) {
      all_cols <- setdiff(all_cols, input$vizYVar)
    }
    
    selectInput(ns("vizColorVar"), "Variable couleur:",
                choices = c("Aucun" = "Aucun", all_cols),
                selected = "Aucun")
  })
  
  output$vizY2VarSelect <- renderUI({
    req(values$filteredData)
    
    # Seulement les variables numériques parmi celles sélectionnées en Y1
    num_y_vars <- input$vizYVar[sapply(input$vizYVar %||% character(0), function(v) {
      v %in% names(values$filteredData) && is.numeric(values$filteredData[[v]])
    })]
    
    # Si moins de 2 variables Y numériques, afficher un message d'aide
    if (length(num_y_vars) < 2) {
      return(div(
        style = "margin-top:8px; padding:8px 10px; background:#fff3e0; border-left:3px solid #ff9800; border-radius:4px;",
        tags$small(
          style = "color:#e65100; font-size:11px;",
          icon("info-circle"),
          " Sélectionnez ", tags$b(">= 2 variables Y numériques"), " pour activer l'axe secondaire."
        )
      ))
    }
    
    default_colors <- c("#2196F3","#4CAF50","#9C27B0","#FF5722","#00BCD4",
                        "#FFC107","#E91E63","#607D8B","#795548","#009688")
    
    color_pickers <- lapply(seq_along(num_y_vars), function(i) {
      v      <- num_y_vars[i]
      col_id <- paste0("curveColor_", make.names(v))
      col_def <- default_colors[((i - 1L) %% length(default_colors)) + 1L]
      div(
        style = "display:flex; align-items:center; gap:8px; margin-bottom:5px;",
        div(
          style = "flex:1; font-size:12px; color:#333; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
          tags$b(v)
        ),
        colourInput(col_id, label = NULL, value = col_def,
                    showColour = "background", palette = "square")
      )
    })
    
    tagList(
      div(
        style = "margin-top:10px; padding:12px; background:#fff8e1; border-left:4px solid #ff9800; border-radius:6px;",
        h5(
          icon("arrows-alt-v"), " Axe Y secondaire (droite)",
          style = "color:#e65100; font-size:13px; font-weight:bold; margin:0 0 8px 0;"
        ),
        checkboxInput(ns("enableDualAxis"),
          tagList(icon("toggle-on"), " Activer l'axe Y secondaire"),
          value = FALSE
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.enableDualAxis == true",
          div(
            style = "margin-top:8px;",
            pickerInput(ns("vizY2Vars"),
              label = div(
                style = "font-size:12px; color:#555; margin-bottom:4px;",
                icon("chart-line"), " Variables sur l'axe Y2 (droite) :"
              ),
              choices  = num_y_vars,
              selected = num_y_vars[length(num_y_vars)],
              multiple = TRUE,
              options  = list(
                `actions-box`           = TRUE,
                `selected-text-format`  = "count > 2",
                `none-selected-text`    = "Sélectionnez des variables..."
              )
            ),
            tags$small(
              style = "color:#888; font-size:11px;",
              icon("info-circle"), " Label, limites et graduations Y2 disponibles dans 'Options Avancées de Personnalisation'."
            )
          )
        ),
        # Sélecteurs de couleurs -- toujours visibles (Y1 et Y2)
        hr(style = "margin:10px 0; border-color:#ffe0b2;"),
        tags$small(
          style = "color:#e65100; font-weight:600; display:block; margin-bottom:6px; font-size:11px;",
          icon("palette"), " Couleur de chaque courbe :"
        ),
        color_pickers
      )
    )
  })
  
  observeEvent(input$enableDualAxis, {
    if (isTRUE(input$enableDualAxis)) {
      values$dualAxisActive <- TRUE
    } else {
      values$dualAxisActive <- FALSE
      values$y2Vars <- NULL
    }
  })
  
  observe({
    req(isTRUE(input$enableDualAxis))
    values$y2Vars <- input$vizY2Vars
  })
  
  output$vizFacetVarSelect <- renderUI({
    req(values$filteredData)
    
    all_cols <- names(values$filteredData)
    cat_cols <- names(values$filteredData)[sapply(values$filteredData, function(x) {
      is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 20)
    })]
    
    selectInput(ns("vizFacetVar"), "Variable facetting:",
                choices = c("Aucun" = "Aucun", cat_cols),
                selected = "Aucun")
  })
  
  output$groupVarsSelect <- renderUI({
    req(values$filteredData)
    req(isTRUE(input$useAggregation))
    
    all_cols <- names(values$filteredData)
    
    excluded_vars <- unique(as.character(unlist(c(input$vizXVar, input$vizYVar))))
    excluded_vars <- excluded_vars[!is.na(excluded_vars) & nchar(excluded_vars) > 0]
    available_cols <- setdiff(all_cols, excluded_vars)
    
    if (length(available_cols) == 0) {
      return(helpText(icon("info-circle"), "Aucune variable disponible pour le regroupement."))
    }
    
    # Priorité aux variables catégorielles mais inclure aussi les numériques à faible cardinalité
    cat_cols <- available_cols[sapply(seq_along(available_cols), function(i) {
      x <- values$filteredData[[ available_cols[i] ]]
      is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 20)
    })]
    
    # Si pas de variables catégorielles, utiliser toutes les variables disponibles
    if (length(cat_cols) == 0) cat_cols <- available_cols
    
    x_default <- if (!is.null(input$vizXVar) && nchar(input$vizXVar) > 0) input$vizXVar else cat_cols[1]
    
    div(
      selectizeInput(ns("groupVars"),
        "Variables de regroupement:",
        choices = c(setNames(x_default, paste0("Variable X (", x_default, ")")), cat_cols),
        selected = x_default,
        multiple = TRUE,
        options = list(
          placeholder = "Sélectionnez les variables...",
          maxItems = 5,
          plugins = list("remove_button")
        )
      ),
      helpText(
        icon("info-circle", style = "color: #17a2b8;"),
        "Sélectionnez les variables qui définissent les groupes pour l'agrégation. Par défaut, X est utilisée."
      )
    )
  })
  
  
  observe({
    req(values$filteredData, input$vizXVar)
    
    if(input$xVarType == "auto") {
      data <- values$filteredData
      x_var_data <- data[[input$vizXVar]]
      
      detected_type <- viz_detect_x_type(x_var_data)
      
      values$detectedXType <- detected_type
    }
  })
  
  observe({
    req(input$vizYVar)
    
    if(length(input$vizYVar) > 1) {
      values$multipleY <- TRUE
      values$yVarNames <- input$vizYVar
    } else {
      values$multipleY <- FALSE
      values$yVarNames <- NULL
      values$y2VarsActive <- NULL
      values$dualAxisActive <- FALSE
    }
  })
  
  
  observe({
    if(is.null(values$storedLevelLabels)) {
      values$storedLevelLabels <- list()
    }
    if(is.null(values$legendLabels)) {
      values$legendLabels <- list()
    }
  }, priority = 1000)
  
  observe({
    req(values$filteredData, input$vizXVar)
    
    data    <- values$filteredData
    x_var   <- input$vizXVar
    x_type  <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    if (is.null(x_type) || x_type == "auto") x_type <- "text"
    
    unique_vals <- if (x_type == "date" ||
                       inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))) {
      # Dates : représenter sous forme de chaîne pour l'édition des labels
      as.character(sort(unique(data[[x_var]])))
    } else if (x_type == "numeric" || is.numeric(data[[x_var]])) {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.factor(data[[x_var]])) {
      levels(data[[x_var]])
    } else {
      sort(unique(as.character(data[[x_var]])))
    }
    
    values$currentXLevels <- unique_vals
    
    if (!is.null(input$vizXVar) && is.null(values$storedLevelLabels[[input$vizXVar]])) {
      values$storedLevelLabels[[input$vizXVar]] <- setNames(unique_vals, unique_vals)
    }
  })
  
  output$xLevelsEditor <- renderUI({
    req(values$filteredData, input$vizXVar)
    req(values$filteredData, input$vizXVar)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    
    if (is.null(x_type)) return(NULL)
    
    unique_vals <- if (inherits(data[[x_var]], c("Date","POSIXct","POSIXlt")) ||
                       x_type == "date") {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.numeric(data[[x_var]]) || x_type == "numeric") {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.factor(data[[x_var]])) {
      levels(droplevels(data[[x_var]]))
    } else {
      sort(unique(as.character(data[[x_var]])))
    }
    
    if(length(unique_vals) == 0) {
      return(div(p("Aucune valeur trouvée pour la variable sélectionnée.", 
                   style = "color: #999;")))
    }
    
    if(length(unique_vals) > 50) {
      return(div(
        p(paste("Trop de valeurs uniques (", length(unique_vals), "). ", 
                "Considérez l'agrégation ou le regroupement.", sep = ""), 
          style = "color: #ff9800; font-weight: bold;"),
        actionButton(ns("showAllLevels"), "Afficher quand même", 
                     class = "btn-warning btn-sm", icon = icon("eye"))
      ))
    }
    
    div(
      div(style = "margin-bottom: 10px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              span(paste(length(unique_vals), "niveaux/valeurs"), 
                   style = "color: #666; font-size: 12px;"),
              div(style = "display: flex; gap: 5px;",
                  actionButton(ns("applyLabels"), "Appliquer", 
                               class = "btn-success btn-xs", icon = icon("check")),
                  actionButton(ns("resetLevels"), "Réinitialiser", 
                               class = "btn-default btn-xs", icon = icon("undo"))
              )
          )
      ),
      
      div(style = if(length(unique_vals) > 10) "max-height: 400px; overflow-y: auto; padding-right: 10px;" else "",
          lapply(seq_along(unique_vals), function(i) {
            lvl <- unique_vals[i]
            stored_label <- values$storedLevelLabels[[x_var]]
            default_val <- if(!is.null(stored_label) && !is.null(names(stored_label)) && lvl %in% names(stored_label)) {
              stored_label[[lvl]]
            } else {
              lvl
            }
            
            div(style = "margin-bottom: 8px; padding: 8px; background-color: #fafafa; border-radius: 4px; border: 1px solid #e0e0e0;",
                div(style = "display: flex; align-items: center; gap: 10px;",
                    span(paste0(i, "."), style = "color: #999; font-weight: bold; min-width: 25px;"),
                    div(style = "flex: 1;",
                        div(style = "font-size: 11px; color: #666; margin-bottom: 2px;",
                            paste("Original:", lvl)),
                        textInput(
                          inputId = paste0("xLevel_", make.names(lvl)),
                          label = NULL,
                          value = default_val,
                          placeholder = "Nouvelle étiquette...",
                          width = "100%"
                        )
                    )
                )
            )
          })
      ),
      
      if(length(unique_vals) > 3) {
        div(style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #e0e0e0;",
            h6("Actions rapides", style = "color: #666; font-weight: bold;"),
            div(style = "display: flex; gap: 5px; flex-wrap: wrap;",
                actionButton(ns("addPrefixBtn"), "Ajouter préfixe", 
                             class = "btn-info btn-xs", icon = icon("plus-circle")),
                actionButton(ns("addSuffixBtn"), "Ajouter suffixe", 
                             class = "btn-info btn-xs", icon = icon("plus-circle")),
                actionButton(ns("numberLevelsBtn"), "Numéroter", 
                             class = "btn-info btn-xs", icon = icon("sort-numeric-up")),
                actionButton(ns("cleanSpacesBtn"), "Nettoyer espaces", 
                             class = "btn-info btn-xs", icon = icon("broom"))
            )
        )
      }
    )
  })
  
  observeEvent(input$applyLabels, {
    req(values$currentXLevels, input$vizXVar)
    
    level_mapping <- vapply(values$currentXLevels, function(lvl) {
      new_label <- input[[paste0("xLevel_", make.names(lvl))]]
      if(is.null(new_label) || trimws(new_label) == "") as.character(lvl) else trimws(new_label)
    }, character(1))
    if (length(level_mapping) == length(values$currentXLevels))
      names(level_mapping) <- values$currentXLevels
    
    values$storedLevelLabels[[input$vizXVar]] <- level_mapping
    
    if(!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
      inv_map <- setNames(names(level_mapping), as.character(level_mapping))
      values$customXOrder <- sapply(values$customXOrder, function(v) {
        new_v <- as.character(level_mapping[v])
        if(!is.na(new_v)) new_v else v
      })
    } else {
      # Pas d'ordre existant : définir l'ordre par les nouveaux labels
      values$customXOrder <- as.character(level_mapping[values$currentXLevels])
    }
    
    values$plotUpdateTrigger <- runif(1)
    invalidateLater(80)
    
    showNotification(
      paste0("Labels appliqués : ", length(level_mapping), " niveaux mis à jour."),
      type = "message", duration = 2
    )
  })
  
  observeEvent(input$resetLevels, {
    req(values$currentXLevels, input$vizXVar)
    
    lapply(values$currentXLevels, function(lvl) {
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), value = lvl)
    })
    
    values$storedLevelLabels[[input$vizXVar]] <- NULL
    
    values$customXOrder <- NULL
    
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Niveaux et ordre réinitialisés", type = "message", duration = 2)
  })
  
  observeEvent(input$addPrefixBtn, {
    req(values$currentXLevels)
    
    showModal(modalDialog(
      title = "Ajouter un préfixe",
      textInput(ns("prefixText"), "Préfixe à ajouter:", value = ""),
      footer = tagList(
        actionButton(ns("applyPrefix"), "Appliquer", class = "btn-primary"),
        modalButton("Annuler")
      )
    ))
  })
  
  observeEvent(input$applyPrefix, {
    req(input$prefixText, values$currentXLevels)
    
    lapply(values$currentXLevels, function(lvl) {
      current_val <- input[[paste0("xLevel_", make.names(lvl))]]
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), 
                      value = paste0(input$prefixText, current_val))
    })
    
    removeModal()
    showNotification("Préfixe ajouté à tous les niveaux", type = "message", duration = 2)
  })
  
  observeEvent(input$addSuffixBtn, {
    req(values$currentXLevels)
    
    showModal(modalDialog(
      title = "Ajouter un suffixe",
      textInput(ns("suffixText"), "Suffixe à ajouter:", value = ""),
      footer = tagList(
        actionButton(ns("applySuffix"), "Appliquer", class = "btn-primary"),
        modalButton("Annuler")
      )
    ))
  })
  
  observeEvent(input$applySuffix, {
    req(input$suffixText, values$currentXLevels)
    
    lapply(values$currentXLevels, function(lvl) {
      current_val <- input[[paste0("xLevel_", make.names(lvl))]]
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), 
                      value = paste0(current_val, input$suffixText))
    })
    
    removeModal()
    showNotification("Suffixe ajouté à tous les niveaux", type = "message", duration = 2)
  })
  
  observeEvent(input$numberLevelsBtn, {
    req(values$currentXLevels)
    
    lapply(seq_along(values$currentXLevels), function(i) {
      lvl <- values$currentXLevels[i]
      current_val <- input[[paste0("xLevel_", make.names(lvl))]]
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), 
                      value = paste0(i, ". ", current_val))
    })
    
    showNotification("Niveaux numérotés", type = "message", duration = 2)
  })
  
  observeEvent(input$cleanSpacesBtn, {
    req(values$currentXLevels)
    
    lapply(values$currentXLevels, function(lvl) {
      current_val <- input[[paste0("xLevel_", make.names(lvl))]]
      cleaned_val <- trimws(gsub("\\s+", " ", current_val))
      updateTextInput(session, paste0("xLevel_", make.names(lvl)), 
                      value = cleaned_val)
    })
    
    showNotification("Espaces nettoyés", type = "message", duration = 2)
  })
  
  
  output$xOrderEditor <- renderUI({
    req(values$filteredData, input$vizXVar)
    req(values$filteredData, input$vizXVar)
    excluded_types <- c("histogram", "density", "pie", "donut", "treemap")
    if(isTRUE(input$vizType %in% excluded_types)) return(NULL)
    
    x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
    if (is.null(x_type)) x_type <- "text"
    
    data  <- values$filteredData
    x_var <- input$vizXVar
    
    unique_vals <- if (inherits(data[[x_var]], c("Date","POSIXct","POSIXlt")) ||
                       x_type == "date") {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.numeric(data[[x_var]]) || x_type == "numeric") {
      as.character(sort(unique(data[[x_var]])))
    } else if (is.factor(data[[x_var]])) {
      levels(data[[x_var]])
    } else {
      sort(unique(as.character(data[[x_var]])))
    }
    
    if(length(unique_vals) == 0) return(NULL)
    if(length(unique_vals) > 100) {
      return(div(
        p(paste("Trop de catégories (", length(unique_vals), "). Réduisez vos données."), 
          style = "color: #ff9800; font-weight: bold;")
      ))
    }
    
    display_vals <- unique_vals
    if(!is.null(values$storedLevelLabels[[x_var]])) {
      level_mapping <- values$storedLevelLabels[[x_var]]
      display_vals <- sapply(unique_vals, function(val) {
        if(val %in% names(level_mapping)) {
          as.character(level_mapping[[val]])
        } else {
          val
        }
      })
    }
    
    if(!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
      # Filtrer pour ne garder que les valeurs qui existent
      ordered_vals <- values$customXOrder[values$customXOrder %in% display_vals]
      if(length(ordered_vals) > 0) {
        display_vals <- ordered_vals
      }
    }
    
    div(
      div(style = "margin-bottom: 10px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              span(paste(length(display_vals), "catégories détectées"), 
                   style = "color: #666; font-size: 12px;"),
              div(style = "display: flex; gap: 5px;",
                  actionButton(ns("autoSortX"), "Tri auto", 
                               class = "btn-default btn-xs", icon = icon("sort-alpha-down")),
                  actionButton(ns("reverseOrderX"), "Inverser", 
                               class = "btn-default btn-xs", icon = icon("exchange-alt")),
                  actionButton(ns("resetOrderX"), "Réinitialiser", 
                               class = "btn-default btn-xs", icon = icon("undo"))
              )
          )
      ),
      
      div(id = "xOrderSortable",
          style = if(length(display_vals) > 15) "max-height: 500px; overflow-y: auto; padding: 10px; background-color: #f9f9f9; border-radius: 5px;" else "padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          lapply(seq_along(display_vals), function(i) {
            val <- display_vals[i]
            div(
              id = paste0("xorder_", i),
              `data-value` = val,
              class = "sortable-item",
              style = "cursor: move; padding: 12px; margin-bottom: 8px; background-color: white; border: 2px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); transition: all 0.2s;",
              div(style = "display: flex; align-items: center; gap: 10px;",
                  icon("grip-vertical", style = "color: #999;"),
                  span(style = "font-weight: bold; color: #28a745; min-width: 30px;", paste0(i, ".")),
                  span(style = "flex: 1; font-size: 14px;", val)
              )
            )
          })
      ),
      
      helpText(icon("hand-point-up"), 
               "Glissez-déposez les éléments pour définir l'ordre d'apparition sur le graphique.",
               style = "margin-top: 10px; color: #666;")
    )
  })
  
  observeEvent(input$autoSortX, {
    req(values$currentXLevels, input$vizXVar)
    
    if(!is.null(values$storedLevelLabels[[input$vizXVar]])) {
      level_mapping <- values$storedLevelLabels[[input$vizXVar]]
      sorted_labels <- sort(as.character(level_mapping[values$currentXLevels]))
      values$customXOrder <- sorted_labels
    } else {
      sorted_levels <- sort(values$currentXLevels)
      values$customXOrder <- sorted_levels
    }
    
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Ordre trié alphabétiquement", type = "message", duration = 2)
  })
  
  observeEvent(input$reverseOrderX, {
    req(values$currentXLevels, input$vizXVar)
    
    if(!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
      values$customXOrder <- rev(values$customXOrder)
    } else if(!is.null(values$storedLevelLabels[[input$vizXVar]])) {
      level_mapping <- values$storedLevelLabels[[input$vizXVar]]
      values$customXOrder <- rev(as.character(level_mapping[values$currentXLevels]))
    } else {
      values$customXOrder <- rev(values$currentXLevels)
    }
    
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Ordre inversé", type = "message", duration = 2)
  })
  
  observeEvent(input$resetOrderX, {
    values$customXOrder <- NULL
    
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Ordre réinitialisé", type = "message", duration = 2)
  })
  
  observeEvent(input$xLevelOrder, {
    if(!is.null(input$xLevelOrder) && length(input$xLevelOrder) > 0) {
      values$customXOrder <- input$xLevelOrder
      values$plotUpdateTrigger <- runif(1)
      invalidateLater(50)
    }
  })
  
  observe({
    req(values$customXOrder)
    isolate({
      values$plotUpdateTrigger <- runif(1)
    })
  })
  
  
  aggregatedData <- reactive({
    req(values$filteredData, input$vizXVar, input$vizYVar)
    
    data <- values$filteredData
    x_var <- input$vizXVar
    y_vars <- input$vizYVar
    
    if(isTRUE(input$useAggregation) && !is.null(input$aggFunction)) {
      
      group_vars <- if(!is.null(input$groupVars) && length(input$groupVars) > 0) {
        input$groupVars
      } else {
        x_var
      }
      group_vars <- intersect(group_vars, names(data))
      if (length(group_vars) == 0) group_vars <- x_var
      
      # Ajouter la variable de couleur au groupement si présente et pas en mode multi-Y
      if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun" && 
         (is.null(values$multipleY) || !values$multipleY) &&
         input$vizColorVar %in% names(data)) {
        group_vars <- unique(c(group_vars, input$vizColorVar))
      }
      
      # Vérifier que les variables Y existent dans les données
      y_vars <- intersect(y_vars, names(data))
      if (length(y_vars) == 0) return(data)
      
      y_numeric <- y_vars[sapply(y_vars, function(v) is.numeric(data[[v]]))]
      y_non_num <- setdiff(y_vars, y_numeric)
      
      # Si aucune Y n'est numérique et la fonction n'est pas "count",
      # basculer automatiquement sur "count"
      effective_func <- input$aggFunction
      if (length(y_numeric) == 0 && effective_func != "count") {
        effective_func <- "count"
        showNotification(
          "Variable Y non numérique : agrégation forcée en Comptage.",
          type = "warning", duration = 4
        )
      }
      
      agg_func <- switch(effective_func,
                         "mean"   = function(x) mean(as.numeric(x), na.rm = TRUE),
                         "median" = function(x) median(as.numeric(x), na.rm = TRUE),
                         "sum"    = function(x) sum(as.numeric(x), na.rm = TRUE),
                         "count"  = function(x) sum(!is.na(x)),
                         "min"    = function(x) min(as.numeric(x), na.rm = TRUE),
                         "max"    = function(x) max(as.numeric(x), na.rm = TRUE),
                         "sd"     = function(x) sd(as.numeric(x), na.rm = TRUE),
                         function(x) mean(as.numeric(x), na.rm = TRUE))
      
      # Variables à agréger : Y numériques, ou toutes les Y si on fait du count
      vars_to_agg <- if (effective_func == "count") y_vars else y_numeric
      
      if(length(vars_to_agg) == 1) {
        # Mode Y simple : ne sélectionner que les colonnes utiles avant group_by
        cols_needed <- unique(c(group_vars, vars_to_agg[1]))
        cols_needed <- intersect(cols_needed, names(data))
        data <- data %>%
          dplyr::select(dplyr::all_of(cols_needed)) %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(!!vars_to_agg[1] := agg_func(.data[[vars_to_agg[1]]]),
                    .groups = "drop")
      } else if (length(vars_to_agg) > 1) {
        agg_list <- list()
        for(y_var in vars_to_agg) {
          cols_needed <- unique(c(group_vars, y_var))
          cols_needed <- intersect(cols_needed, names(data))
          temp_data <- data %>%
            dplyr::select(dplyr::all_of(cols_needed)) %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(value = agg_func(.data[[y_var]]), .groups = "drop") %>%
            mutate(Variable = y_var)
          agg_list[[y_var]] <- temp_data
        }
        data <- bind_rows(agg_list)
      } else {
        # Aucune variable Y agrégeable : retourner les données originales
        showNotification(
          "Aucune variable Y compatible avec la fonction d'agrégation choisie.",
          type = "warning", duration = 4
        )
        return(data)
      }
      
      showNotification(
        paste("Données agrégées:", nrow(data), "observations"),
        type = "message",
        duration = 2
      )
    }
    
    return(data)
  })
  
  
  plotData <- reactive({
    req(values$filteredData, input$vizXVar, input$vizYVar)
    
    # Forcer la réactivité à customXOrder, storedLevelLabels, format date et thème
    values$customXOrder
    values$storedLevelLabels
    values$plotUpdateTrigger
    input$xDateDisplayFormat
    input$plotTheme
    input$enableDualAxis
    input$vizY2Vars
    input$y2AxisLabel
    values$y2VarsActive
    values$dualAxisActive
    
    data <- if(isTRUE(input$useAggregation)) {
      aggregatedData()
    } else {
      values$filteredData
    }
    
    x_var <- input$vizXVar
    y_vars <- input$vizYVar
    
    x_type <- if(input$xVarType == "auto") {
      if (!is.null(values$detectedXType)) values$detectedXType else {
        viz_detect_x_type(data[[x_var]])
      }
    } else input$xVarType
    
    x_is_date_type    <- x_type == "date" || inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))
    x_is_numeric_type <- x_type == "numeric" || (is.numeric(data[[x_var]]) && !x_is_date_type)
    x_is_cat_type     <- x_type %in% c("factor","categorical","text") || is.factor(data[[x_var]])
    
    level_mapping <- values$storedLevelLabels[[x_var]]
    has_custom_labels <- !is.null(level_mapping) &&
      any(as.character(level_mapping) != names(level_mapping))
    
    if (x_is_cat_type || x_is_date_type || x_is_numeric_type) {
      # Représenter X comme chaîne de caractères pour le mapping
      orig_vals <- as.character(data[[x_var]])
      
      if (!is.null(level_mapping) && length(level_mapping) > 0) {
        valid_keys <- names(level_mapping)[names(level_mapping) %in% unique(orig_vals)]
        if (length(valid_keys) > 0) {
          mapped_vals <- ifelse(orig_vals %in% names(level_mapping),
                                as.character(level_mapping[orig_vals]),
                                orig_vals)
          if (x_is_cat_type) {
            # Facteur : conserver l'ordre du mapping
            new_lvls <- as.character(level_mapping[valid_keys])
            if (!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
              ord <- values$customXOrder[values$customXOrder %in% new_lvls]
              if (length(ord) > 0) new_lvls <- ord
            }
            data[[x_var]] <- factor(mapped_vals, levels = new_lvls)
          } else {
            # Date/numérique : le mapping est lu directement via values$storedLevelLabels
            
          }
        }
      } else if (x_is_cat_type) {
        # Pas de mapping : juste facteur avec ordre personnalisé
        current_levels <- sort(unique(orig_vals))
        if (!is.null(values$customXOrder) && length(values$customXOrder) > 0) {
          ord <- values$customXOrder[values$customXOrder %in% current_levels]
          if (length(ord) > 0) current_levels <- ord
        }
        data[[x_var]] <- factor(data[[x_var]], levels = current_levels)
      }
      
    }
    
    # Nuage de points / X numerique : convertir reellement la colonne quand le
    # type retenu est "numeric" mais que la colonne est stockee en texte
    # (decimales a virgule, espaces). Sans cela, ggplot traite X comme discret
    # et le nuage de points quantitatif est impossible.
    if (x_type == "numeric" && !is.null(x_var) && x_var %in% names(data) &&
        !is.numeric(data[[x_var]]) &&
        !inherits(data[[x_var]], c("Date", "POSIXct", "POSIXlt"))) {
      x_num <- hstat_as_numeric_fr(data[[x_var]])
      if (!is.null(x_num)) {
        data[[x_var]] <- x_num
      } else {
        showNotification(
          "Variable X non convertible en numérique : valeurs non numériques présentes.",
          type = "warning", duration = 4)
      }
    }
    
    # Idem pour la variable Y d'un nuage de points : Y doit etre quantitative.
    if (identical(input$vizType %||% "scatter", "scatter")) {
      yv1 <- y_vars[1]
      if (!is.null(yv1) && yv1 %in% names(data) && !is.numeric(data[[yv1]])) {
        y_num <- hstat_as_numeric_fr(data[[yv1]])
        if (!is.null(y_num)) {
          data[[yv1]] <- y_num
        } else {
          showNotification(
            "Nuage de points : la variable Y doit être quantitative (conversion impossible).",
            type = "warning", duration = 4)
        }
      }
    }
    
    if(x_type == "date" && !is.null(x_var) && x_var %in% names(data) &&
       !inherits(data[[x_var]], "Date")) {
      date_format <- input$xDateFormat %||% "%Y-%m-%d"
      converted <- tryCatch({
        result <- as.Date(data[[x_var]], format = date_format)
        result
      }, error = function(e) {
        showNotification("Erreur de conversion de date. Vérifiez le format.", type = "error")
        NULL
      })
      # Appliquer uniquement si le résultat a la même longueur que les données
      if (!is.null(converted) && length(converted) == nrow(data)) {
        data[[x_var]] <- converted
      }
    }
    
    if(!is.null(values$multipleY) && values$multipleY) {
      if(isTRUE(input$useAggregation) && "Variable" %in% names(data)) {
        data_long <- data %>%
          rename(Value = value)
      } else {
        # Transformer en format long pour multi-Y
        # Séparer Y1 (axe gauche) et Y2 (axe droit si dual axis actif)
        y2_vars   <- if (isTRUE(values$dualAxisActive) && !is.null(values$y2Vars))
          intersect(values$y2Vars, y_vars) else character(0)
        y1_vars   <- setdiff(y_vars, y2_vars)
        valid_y1  <- y1_vars[y1_vars %in% names(data)]
        
        if (length(valid_y1) == 0) {
          showNotification("Erreur: Aucune variable Y1 disponible dans les données", type = "error")
          return(data)
        }
        if (!x_var %in% names(data)) {
          showNotification("Erreur: Variable X non disponible", type = "error")
          return(data)
        }
        
        cols_to_keep <- c(x_var, valid_y1)
        # Garder aussi les vars Y2 en wide pour le dual axis
        valid_y2 <- y2_vars[y2_vars %in% names(data)]
        cols_wide <- unique(c(cols_to_keep, valid_y2))
        
        data_wide <- data %>% dplyr::select(dplyr::any_of(cols_wide))
        
        data_long <- data_wide %>%
          tidyr::pivot_longer(
            cols    = dplyr::any_of(valid_y1),
            names_to  = "Variable",
            values_to = "Value"
          )
        
        values$y2VarsActive <- valid_y2
      }
      return(data_long)
    }
    
    cols_needed <- unique(c(x_var, y_vars[1], input$vizColorVar, input$vizFacetVar))
    cols_needed <- cols_needed[cols_needed != "Aucun"]
    cols_needed <- cols_needed[cols_needed %in% names(data)]
    
    return(data[, cols_needed, drop = FALSE])
  })
  
  # Stocker les données préparées dans values pour accès global
  observe({
    values$plotData <- plotData()
  })
  
  
  # Créer une interface pour personnaliser les labels de légende
  observeEvent(input$customizeLegendLabels, {
    if (is.null(values$plotData)) {
      showNotification(
        tagList(icon("chart-bar"), " Générez d'abord un graphique avant de modifier la légende."),
        type = "warning", duration = 4
      )
      return()
    }
    
    legend_levels <- NULL
    legend_var_name <- NULL
    
    if(!is.null(values$multipleY) && values$multipleY) {
      legend_levels  <- values$yVarNames
      legend_var_name <- "Variables Y"
    } else if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      color_data <- values$plotData[[input$vizColorVar]]
      if (is.factor(color_data)) {
        legend_levels <- levels(color_data)
      } else if (is.numeric(color_data) || is.integer(color_data)) {
        legend_levels <- as.character(sort(unique(color_data)))
        if (length(legend_levels) > 20) {
          showNotification(
            "Variable continue avec trop de valeurs distinctes (> 20). Utilisez une variable catégorielle pour personnaliser les labels.",
            type = "warning", duration = 5)
          return()
        }
      } else if (inherits(color_data, c("Date","POSIXct","POSIXlt"))) {
        legend_levels <- as.character(sort(unique(color_data)))
      } else {
        legend_levels <- sort(unique(as.character(color_data)))
      }
      legend_var_name <- input$vizColorVar
    }
    
    if(is.null(legend_levels) || length(legend_levels) == 0) {
      showNotification(
        tagList(
          icon("exclamation-circle"), " Aucune légende à personnaliser.",
          tags$br(),
          tags$small(
            "Sélectionnez une ", tags$b("variable couleur"), " ou activez le ",
            tags$b("mode multi-Y"), " (>=2 variables Y) pour utiliser cet éditeur."
          )
        ),
        type = "warning", duration = 5
      )
      return()
    }
    
    # Créer l'interface modale pour éditer les labels de légende
    showModal(modalDialog(
      title = tagList(icon("tags"), " Personnaliser les labels de la légende"),
      size = "m",
      
      div(
        div(
          style = "background:#f0f4ff; border-left:4px solid #3498db; padding:10px 14px; border-radius:4px; margin-bottom:14px;",
          tags$b(style="color:#2980b9;", icon("info-circle"), " Variable : "),
          tags$span(style="color:#34495e; font-size:13px;", legend_var_name),
          tags$br(),
          tags$small(style="color:#7f8c8d;",
                     "Saisissez les nouveaux noms à afficher dans la légende. ",
                     "Laissez vide pour conserver l'original."
          )
        ),
        
        div(style = if(length(legend_levels) > 8) "max-height: 420px; overflow-y: auto;" else "",
            lapply(seq_along(legend_levels), function(i) {
              lvl <- legend_levels[i]
              storage_key <- if(legend_var_name == "Variables Y") "multiY_legend" else legend_var_name
              
              current_label <- if(!is.null(values$legendLabels[[storage_key]]) && 
                                  !is.null(values$legendLabels[[storage_key]][[lvl]])) {
                values$legendLabels[[storage_key]][[lvl]]
              } else { lvl }
              
              n_total <- length(legend_levels)
              lvl_color <- tryCatch(scales::hue_pal()(n_total)[i], error=function(e) "#cccccc")
              
              div(
                style = "display:flex; align-items:center; gap:10px; margin-bottom:8px; padding:8px 10px; background:#fafafa; border-radius:6px; border:1px solid #e8e8e8;",
                div(style = paste0("width:18px; height:18px; border-radius:50%; background:", lvl_color,
                                   "; flex-shrink:0; border:2px solid #fff; box-shadow:0 0 0 1px #ccc;")),
                div(style = "min-width:100px; max-width:120px; font-size:11px; color:#666; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
                    tags$span(title=lvl, lvl),
                    tags$br(),
                    tags$span(style="color:#bbb; font-size:10px;", paste0("Niveau ", i))),
                div(style="color:#bbb; font-size:14px;", "->"),
                div(style="flex:1;",
                    textInput(
                      inputId = paste0("legendLevel_", make.names(lvl)),
                      label   = NULL,
                      value   = current_label,
                      placeholder = paste0("ex: Groupe ", i),
                      width   = "100%"
                    )
                )
              )
            })
        )
      ),
      
      footer = tagList(
        actionButton(ns("applyLegendLabels"),  tagList(icon("check"), " Appliquer"),       class = "btn-primary"),
        actionButton(ns("previewLegendLabels"),tagList(icon("eye"),   " Prévisualiser"),   class = "btn-info"),
        actionButton(ns("resetLegendLabels"),  tagList(icon("undo"),  " Réinitialiser"),   class = "btn-default"),
        modalButton(tagList(icon("times"), " Fermer"))
      )
    ))
  })
  
  output$legendLabelsStatus <- renderUI({
    if (is.null(values$legendLabels) || length(values$legendLabels) == 0) return(NULL)
    n_custom <- sum(sapply(values$legendLabels, function(m) {
      if (is.null(m)) return(0L)
      sum(names(m) != as.character(m))
    }))
    if (n_custom == 0) return(NULL)
    tags$small(
      style = "color: #e67e22; font-size: 11px; display: block; margin-top: 3px;",
      icon("check-circle"), " ", n_custom, " label(s) personnalisé(s) actif(s)"
    )
  })
  
  observeEvent(input$applyLegendLabels, {
    storage_key <- if(!is.null(values$multipleY) && values$multipleY) {
      "multiY_legend"
    } else if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      input$vizColorVar
    } else {
      return()
    }
    
    legend_levels <- if(storage_key == "multiY_legend") {
      values$yVarNames
    } else {
      color_data <- values$plotData[[input$vizColorVar]]
      if (is.factor(color_data)) levels(color_data)
      else if (is.numeric(color_data) || is.integer(color_data)) as.character(sort(unique(color_data)))
      else if (inherits(color_data, c("Date","POSIXct","POSIXlt"))) as.character(sort(unique(color_data)))
      else sort(unique(as.character(color_data)))
    }
    
    legend_mapping <- vapply(legend_levels, function(lvl) {
      new_label <- input[[paste0("legendLevel_", make.names(lvl))]]
      if(is.null(new_label) || new_label == "") as.character(lvl) else as.character(new_label)
    }, character(1))
    if (length(legend_mapping) == length(legend_levels)) names(legend_mapping) <- legend_levels
    
    values$legendLabels[[storage_key]] <- legend_mapping
    
    values$plotUpdateTrigger <- runif(1)
    
    removeModal()
    showNotification("Labels de légende appliqués", type = "message", duration = 2)
  })
  
  observeEvent(input$previewLegendLabels, {
    storage_key <- if (!is.null(values$multipleY) && values$multipleY) {
      "multiY_legend"
    } else if (!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      input$vizColorVar
    } else { return() }
    
    legend_levels <- if (storage_key == "multiY_legend") {
      values$yVarNames
    } else {
      color_data <- values$plotData[[input$vizColorVar]]
      if (is.factor(color_data)) levels(color_data)
      else if (is.numeric(color_data)||is.integer(color_data)) as.character(sort(unique(color_data)))
      else if (inherits(color_data, c("Date","POSIXct","POSIXlt"))) as.character(sort(unique(color_data)))
      else sort(unique(as.character(color_data)))
    }
    
    legend_mapping <- vapply(legend_levels, function(lvl) {
      new_label <- input[[paste0("legendLevel_", make.names(lvl))]]
      if (is.null(new_label) || new_label == "") as.character(lvl) else as.character(new_label)
    }, character(1))
    if (length(legend_mapping) == length(legend_levels)) names(legend_mapping) <- legend_levels
    values$legendLabels[[storage_key]] <- legend_mapping
    values$plotUpdateTrigger <- runif(1)
    showNotification("Prévisualisation appliquée au graphique", type = "message", duration = 2)
  })
  
  observeEvent(input$resetLegendLabels, {
    storage_key <- if(!is.null(values$multipleY) && values$multipleY) {
      "multiY_legend"
    } else if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
      input$vizColorVar
    } else {
      return()
    }
    
    legend_levels <- if(storage_key == "multiY_legend") {
      values$yVarNames
    } else {
      color_data <- values$plotData[[input$vizColorVar]]
      if (is.factor(color_data)) levels(color_data)
      else if (is.numeric(color_data)||is.integer(color_data)) as.character(sort(unique(color_data)))
      else if (inherits(color_data, c("Date","POSIXct","POSIXlt"))) as.character(sort(unique(color_data)))
      else sort(unique(as.character(color_data)))
    }
    
    lapply(legend_levels, function(lvl) {
      updateTextInput(session, paste0("legendLevel_", make.names(lvl)), value = lvl)
    })
    
    values$legendLabels[[storage_key]] <- NULL
    
    values$plotUpdateTrigger <- runif(1)
    
    showNotification("Labels de légende réinitialisés", type = "message", duration = 2)
  })
  
  
  # Expression réactive pour créer le graphique avec mise à jour automatique
  
  # Paramètres pour les étiquettes de valeurs 
  # Wrapper réactif -> viz_label_params() de Utils.R
  get_label_params <- function() {
    viz_label_params(
      size     = input$valueLabelSize     %||% 3,
      color    = input$valueLabelColor    %||% "#333333",
      bold     = isTRUE(input$valueLabelBold),
      italic   = isTRUE(input$valueLabelItalic),
      digits   = input$valueLabelDigits   %||% 2,
      position = input$valueLabelPosition %||% "above"
    )
  }
  
  create_scatter_plot <- function(data, x_var, y_var, color_var = NULL) {
    # Gros volumes : on limite le nombre de points AFFICHES (echantillon
    # reproductible) pour garder le navigateur reactif avec 1M+ lignes.
    data <- hstat_sample_rows(data)
    data <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]), ]
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    if(!is.null(color_var)) {
      p <- p + geom_point(aes(color = .data[[color_var]]),
                          size = input$pointSize %||% 3,
                          alpha = input$pointAlpha %||% 0.7, na.rm=TRUE)
    } else {
      p <- p + geom_point(size = input$pointSize %||% 3,
                          alpha = input$pointAlpha %||% 0.7, na.rm=TRUE)
    }
    
    if(isTRUE(input$showTrendLine)) {
      p <- p + geom_smooth(method = input$trendMethod %||% "lm", 
                           formula = y ~ x,
                           se = isTRUE(input$showConfidenceInterval))
    }
    
    if(isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                         vjust = lp$vjust, hjust = lp$hjust,
                         size = lp$size, color = lp$color,
                         fontface = lp$fontface, check_overlap = TRUE)
    }
    return(p)
  }
  
  create_line_plot <- function(data, x_var, y_var, color_var = NULL) {
    connect_na  <- isTRUE(input$lineConnectNA)
    show_na_mk  <- isTRUE(input$lineShowNAMarker)
    lw          <- input$lineWidth  %||% 1
    ps          <- input$pointSize  %||% 2
    pa          <- input$pointAlpha %||% 0.8
    show_pts    <- isTRUE(input$showPoints)
    fixed_color <- input$lineFixedColor %||% "#2196F3"
    x_is_date    <- inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))
    x_is_numeric <- is.numeric(data[[x_var]])
    data <- tryCatch({
      if (x_is_date || x_is_numeric) data[order(data[[x_var]], na.last=TRUE), ]
      else data
    }, error=function(e) data)
    data_line <- if (connect_na) data[!is.na(data[[y_var]]), ] else data
    data_pts  <- data[!is.na(data[[y_var]]), ]
    p <- ggplot(data_line, aes(x=.data[[x_var]], y=.data[[y_var]]))
    if (!is.null(color_var)) {
      p <- p + geom_line(aes(color=.data[[color_var]], group=.data[[color_var]]),
                         linewidth=lw, na.rm=TRUE)
      if (show_pts)
        p <- p + geom_point(data=data_pts,
                            aes(x=.data[[x_var]], y=.data[[y_var]], color=.data[[color_var]]),
                            size=ps, alpha=pa, na.rm=TRUE)
    } else {
      p <- p + geom_line(color=fixed_color, linewidth=lw, na.rm=TRUE)
      if (show_pts)
        p <- p + geom_point(data=data_pts,
                            aes(x=.data[[x_var]], y=.data[[y_var]]),
                            color=fixed_color, size=ps, alpha=pa, na.rm=TRUE)
    }
    if (show_na_mk && !connect_na) {
      data_na <- data[is.na(data[[y_var]]), ]
      if (nrow(data_na) > 0) {
        y_ref <- if (nrow(data_pts)>0) min(data_pts[[y_var]],na.rm=TRUE) else 0
        data_na[[y_var]] <- y_ref
        if (!is.null(color_var) && color_var %in% names(data_na)) {
          p <- p + geom_point(data=data_na, aes(x=.data[[x_var]], y=.data[[y_var]],
                                                color=.data[[color_var]]),
                              shape=4, size=ps+1.5, alpha=0.5, na.rm=TRUE)
        } else {
          p <- p + geom_point(data=data_na, aes(x=.data[[x_var]], y=.data[[y_var]]),
                              shape=4, size=ps+1.5, color="grey55", alpha=0.5, na.rm=TRUE)
        }
      }
    }
    # Axe X : tous les types + labels + ordre 
    p <- p + get_x_scale(data, x_var)
    if (isTRUE(input$showTrendLine)) {
      p <- p + geom_smooth(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]]),
                           method=input$trendMethod %||% "lm", formula=y~x,
                           se=isTRUE(input$showConfidenceInterval), na.rm=TRUE)
    }
    if (isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]],
                                            label=round(.data[[y_var]], lp$digits)),
                         vjust=lp$vjust, hjust=lp$hjust, size=lp$size, color=lp$color,
                         fontface=lp$fontface, check_overlap=TRUE, na.rm=TRUE)
    }
    return(p)
  }
  
  create_bar_plot <- function(data, x_var, y_var, color_var = NULL) {
    if (nrow(data) == 0 || !x_var %in% names(data) || !y_var %in% names(data)) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Données insuffisantes") +
               theme_void())
    }
    
    if (!is.factor(data[[x_var]])) {
      data[[x_var]] <- factor(data[[x_var]], levels = unique(as.character(data[[x_var]])))
    }
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_col(aes(fill = .data[[color_var]]), 
                        position = input$barPosition %||% "dodge",
                        width = input$barWidth %||% 0.8)
    } else {
      p <- p + geom_col(width = input$barWidth %||% 0.8)
    }
    
    if(isTRUE(input$showValues)) {
      lp <- get_label_params()
      bar_pos <- input$barPosition %||% "dodge"
      bar_width <- input$barWidth %||% 0.8
      
      if(bar_pos == "dodge" && !is.null(color_var)) {
        p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                           vjust = lp$vjust, hjust = lp$hjust,
                           size = lp$size, color = lp$color, fontface = lp$fontface,
                           position = position_dodge(width = bar_width))
      } else if(bar_pos == "stack") {
        p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                           size = lp$size, color = lp$color, fontface = lp$fontface,
                           position = position_stack(vjust = if(lp$vjust == 0.5) 0.5 else 0.9))
      } else if(bar_pos == "fill") {
        p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                           size = lp$size, color = lp$color, fontface = lp$fontface,
                           position = position_fill(vjust = 0.5))
      } else {
        p <- p + geom_text(aes(label = round(.data[[y_var]], lp$digits)),
                           vjust = lp$vjust, hjust = lp$hjust,
                           size = lp$size, color = lp$color, fontface = lp$fontface)
      }
    }
    return(p)
  }
  
  create_box_plot <- function(data, x_var, y_var, color_var = NULL) {
    if (nrow(data) == 0 || !x_var %in% names(data) || !y_var %in% names(data)) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Données insuffisantes") +
               theme_void())
    }
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    # NB capuchons de moustache : ils sont ajoutes UNIQUEMENT a l'export
    # (hstat_add_whisker_caps dans le telechargement). A l'ecran, plotly
    # dessine ses propres capuchons ; une couche errorbar ici deviendrait des
    # traces desalignees apres conversion ggplotly.
    # Points aberrants : si le jitter est affiche, on masque ceux du boxplot
    # pour eviter leur affichage en double (points noirs + points gris).
    out_shape <- if (isTRUE(input$showOutliers)) NA else 16
    if(!is.null(color_var)) {
      p <- p + geom_boxplot(aes(fill = .data[[color_var]]), alpha = 0.7,
                            outlier.shape = out_shape)
    } else {
      p <- p + geom_boxplot(alpha = 0.7, outlier.shape = out_shape)
    }
    
    if(isTRUE(input$showOutliers))
      p <- p + geom_jitter(data = function(d) hstat_sample_rows(d, notify = FALSE),
                           width = 0.2, alpha = 0.3, size = 1)
    
    return(p)
  }
  
  create_violin_plot <- function(data, x_var, y_var, color_var = NULL) {
    
    # Filtrer les groupes ayant moins de 2 observations (évite le crash stat_ydensity)
    group_var <- if(!is.null(color_var)) color_var else x_var
    
    if(group_var %in% names(data)) {
      group_counts <- table(data[[group_var]])
      valid_groups <- names(group_counts[group_counts >= 2])
      
      if(length(valid_groups) == 0) {
        showNotification(
          "Violin: aucun groupe ne contient au moins 2 observations. Passez en Boxplot.",
          type = "warning", duration = 4
        )
        return(create_box_plot(data, x_var, y_var, color_var))
      }
      
      removed <- setdiff(names(group_counts), valid_groups)
      if(length(removed) > 0) {
        showNotification(
          paste("Violin: groupes ignorés (< 2 obs.):", paste(removed, collapse = ", ")),
          type = "warning", duration = 4
        )
      }
      
      data <- data[data[[group_var]] %in% valid_groups, ]
      if(is.factor(data[[group_var]])) {
        data[[group_var]] <- droplevels(data[[group_var]])
      }
    }
    
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    if(!is.null(color_var)) {
      p <- p + geom_violin(aes(fill = .data[[color_var]]), alpha = 0.7, drop = FALSE)
    } else {
      p <- p + geom_violin(alpha = 0.7, drop = FALSE)
    }
    
    if(isTRUE(input$showBoxInsideViolin)) {
      p <- p + geom_boxplot(width = 0.1)
    }
    
    return(p)
  }
  
  create_seasonal_smooth_plot <- function(data, x_var, y_var, color_var = NULL) {
    x_is_date    <- inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))
    x_is_numeric <- is.numeric(data[[x_var]]) && !x_is_date
    x_is_factor  <- is.factor(data[[x_var]]) || is.character(data[[x_var]])
    all_x_orig <- data[[x_var]]
    
    co_ord <- values$customXOrder
    # Pour les X catégoriels : convertir en facteur ordonné
    if (x_is_factor) {
      lvls_orig <- if (is.factor(data[[x_var]])) levels(data[[x_var]])
      else sort(unique(as.character(data[[x_var]])))
      if (!is.null(co_ord) && length(co_ord) > 0) {
        ord <- co_ord[co_ord %in% lvls_orig]
        if (length(ord) > 0) lvls_orig <- ord
      }
      data[[x_var]] <- factor(as.character(data[[x_var]]), levels = lvls_orig)
      all_x_orig <- data[[x_var]]
    }
    
    group_cols <- if (!is.null(color_var) && color_var %in% names(data)) c(x_var, color_var) else x_var
    x_counts <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(n=dplyr::n(), .groups="drop")
    has_duplicates <- any(x_counts$n > 1)
    if (has_duplicates && !isTRUE(input$useAggregation)) {
      data <- data %>%
        dplyr::filter(!is.na(.data[[y_var]])) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::summarise(!!y_var := mean(.data[[y_var]], na.rm=TRUE), .groups="drop")
      showNotification(paste0("Seasonal Smooth : agrégation auto par moyenne (",nrow(data)," pts)."),
                       type="message", duration=4)
    } else {
      data <- data[!is.na(data[[y_var]]), ]
    }
    if (x_is_date || x_is_numeric) data <- data[order(data[[x_var]]), ]
    n_pts <- if (!is.null(color_var) && color_var %in% names(data)) min(table(data[[color_var]])) else nrow(data)
    smooth_method <- input$smoothMethod %||% "loess"
    if (n_pts < 4 && smooth_method=="loess") {
      showNotification("Trop peu de points pour LOESS. Méthode lm utilisée.", type="warning", duration=4)
      smooth_method <- "lm"
    }
    lw <- input$lineWidth %||% 1
    ps <- input$pointSize %||% 2
    fixed_color <- input$lineFixedColor %||% "#2196F3"

    # Pour un X categoriel, le lissage exige une position NUMERIQUE : on mappe le
    # facteur sur 1..k pour le calcul, et on replace les etiquettes sur l'axe.
    x_for_plot <- x_var
    if (x_is_factor) {
      data[[".x_num"]] <- as.integer(data[[x_var]])
      x_for_plot <- ".x_num"
    }
    # all_x_orig doit refleter les donnees finales (post-agregation)
    all_x_orig <- data[[x_var]]

    p <- ggplot(data, aes(x=.data[[x_for_plot]], y=.data[[y_var]]))
    if (!is.null(color_var) && color_var %in% names(data)) {
      p <- p +
        geom_line(aes(color=.data[[color_var]], group=.data[[color_var]]),
                  linewidth=lw, alpha=0.5, na.rm=TRUE) +
        geom_smooth(aes(color=.data[[color_var]], group=.data[[color_var]]),
                    method=smooth_method, formula=y~x, span=input$smoothSpan %||% 0.75,
                    se=isTRUE(input$showConfidenceInterval), na.rm=TRUE)
      if (isTRUE(input$showPoints))
        p <- p + geom_point(aes(color=.data[[color_var]]), size=ps, alpha=0.7, na.rm=TRUE)
    } else {
      p <- p +
        geom_line(color=fixed_color, linewidth=lw, alpha=0.5, na.rm=TRUE, group=1) +
        geom_smooth(color=fixed_color, fill=fixed_color,
                    method=smooth_method, formula=y~x, span=input$smoothSpan %||% 0.75,
                    se=isTRUE(input$showConfidenceInterval), na.rm=TRUE)
      if (isTRUE(input$showPoints))
        p <- p + geom_point(color=fixed_color, size=ps, alpha=0.7, na.rm=TRUE)
    }
    # Axe X : pour un facteur, replacer les etiquettes d'origine sur 1..k
    if (x_is_factor) {
      lvls <- levels(data[[x_var]])
      p <- p + scale_x_continuous(breaks = seq_along(lvls), labels = lvls)
    } else {
      tmp_df_scale <- data.frame(all_x_orig)
      names(tmp_df_scale) <- x_var
      p <- p + get_x_scale(tmp_df_scale, x_var)
    }
    if (isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(aes(label=round(.data[[y_var]], lp$digits)),
                         vjust=lp$vjust, hjust=lp$hjust, size=lp$size, color=lp$color,
                         fontface=lp$fontface, check_overlap=TRUE, na.rm=TRUE)
    }
    return(p)
  }
  
  create_seasonal_evolution_plot <- function(data, x_var, y_var, color_var = NULL) {
    x_is_date    <- inherits(data[[x_var]], c("Date","POSIXct","POSIXlt"))
    x_is_numeric <- is.numeric(data[[x_var]]) && !x_is_date
    x_is_factor  <- is.factor(data[[x_var]]) || is.character(data[[x_var]])
    all_x_orig <- data[[x_var]]
    co_ord <- values$customXOrder
    
    group_cols <- if (!is.null(color_var) && color_var %in% names(data)) c(x_var, color_var) else x_var
    
    # Pour les X catégoriels/texte : convertir en facteur ordonné d'abord
    if (x_is_factor) {
      lvls_orig <- if (is.factor(data[[x_var]])) levels(data[[x_var]])
      else sort(unique(as.character(data[[x_var]])))
      if (!is.null(co_ord) && length(co_ord) > 0) {
        ord <- co_ord[co_ord %in% lvls_orig]
        if (length(ord) > 0) lvls_orig <- ord
      }
      data[[x_var]] <- factor(as.character(data[[x_var]]), levels = lvls_orig)
      all_x_orig <- data[[x_var]]
    }
    
    x_counts <- data %>%
      dplyr::filter(!is.na(.data[[y_var]])) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(n=dplyr::n(), .groups="drop")
    has_duplicates <- nrow(x_counts)>0 && any(x_counts$n>1)
    if (has_duplicates && !isTRUE(input$useAggregation)) {
      data_valid <- data %>%
        dplyr::filter(!is.na(.data[[y_var]])) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::summarise(!!y_var := mean(.data[[y_var]],na.rm=TRUE), .groups="drop")
      showNotification(paste0("Évolution : agrégation auto par moyenne. ",
                              nrow(data_valid)," pts uniques."), type="message", duration=5)
      data_plot <- data_valid; data_pts <- data_valid
    } else {
      data_plot <- data[!is.na(data[[y_var]]), ]
      data_pts  <- data_plot
    }
    # Trier : dates et numériques par valeur, facteurs par ordre des niveaux
    # Guard : données vides après filtrage/agrégation
    if (nrow(data_plot) == 0) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Aucune donnée valide",
                                 color="#e74c3c", size=5) + theme_void())
    }
    if (x_is_date || x_is_numeric) {
      data_plot <- data_plot[order(data_plot[[x_var]]), ]
      data_pts  <- data_pts[order(data_pts[[x_var]]),  ]
    } else if (x_is_factor) {
      if (is.factor(data_plot[[x_var]])) {
        data_plot <- data_plot[order(as.integer(data_plot[[x_var]])), ]
        data_pts  <- data_pts[order(as.integer(data_pts[[x_var]])),  ]
      }
    }
    # Pour le geom_line sur X catégoriel : nécessite group aesthetic
    needs_group_aes <- x_is_factor && is.null(color_var)
    lw          <- input$lineWidth %||% 1.2
    ps          <- input$pointSize %||% 3
    fixed_color <- input$lineFixedColor %||% "#2196F3"
    p <- ggplot(data_plot, aes(x=.data[[x_var]], y=.data[[y_var]]))
    if (!is.null(color_var) && color_var %in% names(data_plot)) {
      p <- p +
        geom_line(aes(color=.data[[color_var]], group=.data[[color_var]]),
                  linewidth=lw, na.rm=TRUE) +
        geom_point(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]],
                                      color=.data[[color_var]]), size=ps, na.rm=TRUE)
    } else {
      # Pour X catégoriel sans color_var : group=1 requis par geom_line
      if (isTRUE(needs_group_aes)) {
        p <- p +
          geom_line(aes(group=1), color=fixed_color, linewidth=lw, na.rm=TRUE) +
          geom_point(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]]),
                     color=fixed_color, size=ps, na.rm=TRUE)
      } else {
        p <- p +
          geom_line(color=fixed_color, linewidth=lw, na.rm=TRUE) +
          geom_point(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]]),
                     color=fixed_color, size=ps, na.rm=TRUE)
      }
    }
    if (isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(data=data_pts, aes(x=.data[[x_var]], y=.data[[y_var]],
                                            label=round(.data[[y_var]], lp$digits)),
                         vjust=lp$vjust, hjust=lp$hjust, size=lp$size, color=lp$color,
                         fontface=lp$fontface, check_overlap=TRUE, na.rm=TRUE)
    }
    # Axe X : tous les types + labels + ordre 
    tmp_df_scale <- data.frame(I(all_x_orig))
    names(tmp_df_scale) <- x_var
    p <- p + get_x_scale(tmp_df_scale, x_var)
    # NOTE : thème, scale_y et limites sont appliqués par createPlot après cet appel.
    # On ajoute uniquement la grille spécifique aux courbes d'évolution.
    p <- p + theme(
      panel.grid.major.y = element_line(color = "#e0e0e0", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.margin      = margin(t = 15)
    )
    if (!is.null(color_var) && color_var %in% names(data_plot))
      p <- p + guides(color = guide_legend(override.aes = list(size = 2), ncol = 2))
    return(p)
  }
  
  create_histogram_plot <- function(data, x_var) {
    xv <- data[[x_var]]
    if (!is.numeric(xv)) {
      xnum <- suppressWarnings(as.numeric(as.character(xv)))
      if (sum(!is.na(xnum)) >= 2) {
        data[[x_var]] <- xnum
      } else {
        df <- as.data.frame(table(xv, dnn = x_var), stringsAsFactors = FALSE)
        names(df)[2] <- "Freq"
        return(ggplot(df, aes(x = .data[[x_var]], y = .data$Freq)) +
                 geom_col(fill = input$histColor %||% "steelblue", alpha = 0.7, color = "white") +
                 labs(y = "Fréquence") +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1)))
      }
    }
    ggplot(data, aes(x = .data[[x_var]])) +
      geom_histogram(bins = input$histBins %||% 30,
                     fill  = input$histColor %||% "steelblue",
                     alpha = 0.7, color = "white", na.rm = TRUE)
  }
  
  create_density_plot <- function(data, x_var, color_var = NULL) {
    xv <- data[[x_var]]
    if (!is.numeric(xv)) {
      xnum <- suppressWarnings(as.numeric(as.character(xv)))
      if (sum(!is.na(xnum)) >= 2) {
        data[[x_var]] <- xnum
      } else {
        showNotification("La densité nécessite une variable X numérique.",
                         type = "warning", duration = 4)
        return(create_bar_plot(data, x_var, x_var, color_var))
      }
    }
    p <- ggplot(data, aes(x = .data[[x_var]]))
    if (!is.null(color_var)) {
      p + geom_density(aes(fill = .data[[color_var]], color = .data[[color_var]]),
                       alpha = 0.5, na.rm = TRUE)
    } else {
      p + geom_density(fill = "steelblue", alpha = 0.5, na.rm = TRUE)
    }
  }
  
  create_heatmap_plot <- function(data, x_var, y_var) {
    agg_data <- data %>%
      group_by(across(all_of(c(x_var, y_var)))) %>%
      summarise(count = n(), .groups = "drop")
    
    p <- ggplot(agg_data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if(isTRUE(input$showValues)) {
      p <- p + geom_text(aes(label = count), color = "black", size = 3)
    }
    
    return(p)
  }
  
  create_area_plot <- function(data, x_var, y_var, color_var = NULL) {
    data <- tryCatch({
      if (inherits(data[[x_var]], c("Date","POSIXct","POSIXlt")) ||
          is.numeric(data[[x_var]])) data[order(data[[x_var]], na.last=TRUE), ]
      else data
    }, error=function(e) data)
    data <- data[!is.na(data[[y_var]]), ]
    lw <- input$lineWidth %||% 1
    p <- ggplot(data, aes(x=.data[[x_var]], y=.data[[y_var]]))
    if (!is.null(color_var)) {
      p <- p + geom_area(aes(fill=.data[[color_var]], group=.data[[color_var]]),
                         position=input$areaPosition %||% "stack", alpha=0.7, na.rm=TRUE) +
        geom_line(aes(color=.data[[color_var]], group=.data[[color_var]]),
                  linewidth=lw, na.rm=TRUE)
    } else {
      p <- p + geom_area(fill="steelblue", alpha=0.7, na.rm=TRUE) +
        geom_line(linewidth=lw, color="steelblue4", na.rm=TRUE)
    }
    if (isTRUE(input$showPoints)) {
      if (!is.null(color_var))
        p <- p + geom_point(aes(color=.data[[color_var]]), size=input$pointSize %||% 2, na.rm=TRUE)
      else
        p <- p + geom_point(size=input$pointSize %||% 2, na.rm=TRUE)
    }
    if (isTRUE(input$showValues)) {
      lp <- get_label_params()
      p <- p + geom_text(aes(label=round(.data[[y_var]], lp$digits)),
                         vjust=lp$vjust, hjust=lp$hjust, size=lp$size, color=lp$color,
                         fontface=lp$fontface, check_overlap=TRUE, na.rm=TRUE)
    }
    return(p)
  }
  
  create_pie_plot <- function(data, x_var, y_var) {
    pie_data <- data %>%
      group_by(across(all_of(x_var))) %>%
      summarise(total = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop") %>%
      mutate(percentage = total / sum(total) * 100)
    
    p <- ggplot(pie_data, aes(x = "", y = total, fill = .data[[x_var]])) +
      geom_col() +
      coord_polar(theta = "y") +
      theme_void()
    
    p <- p + geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                       position = position_stack(vjust = 0.5))
    
    return(p)
  }
  
  create_donut_plot <- function(data, x_var, y_var) {
    # Similaire au pie mais avec un trou au centre
    donut_data <- data %>%
      group_by(across(all_of(x_var))) %>%
      summarise(total = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop") %>%
      mutate(percentage = total / sum(total) * 100)
    
    p <- ggplot(donut_data, aes(x = 2, y = total, fill = .data[[x_var]])) +
      geom_col() +
      coord_polar(theta = "y") +
      xlim(c(0.5, 2.5)) +
      theme_void()
    
    p <- p + geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                       position = position_stack(vjust = 0.5))
    
    return(p)
  }
  
  create_treemap_plot <- function(data, x_var, y_var) {
    
    treemap_data <- data %>%
      group_by(across(all_of(x_var))) %>%
      summarise(total = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(treemap_data, aes(area = total, fill = .data[[x_var]], label = .data[[x_var]])) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(colour = "white", place = "centre")
    
    return(p)
  }
  
  createPlot <- reactive({
    req(values$plotData, input$vizXVar, input$vizYVar, input$vizType)
    
    # Réactivité aux options de personnalisation ET à l'ordre personnalisé
    input$plotTitle
    input$xAxisLabel
    input$yAxisLabel
    input$legendTitle
    input$legendTitleSize
    input$legendTextSize
    input$legendKeySize
    input$pointSize
    input$lineWidth
    input$pointAlpha
    input$barWidth
    input$barPosition
    input$titleSize
    input$axisLabelSize
    input$baseFontSize
    input$xAxisBold
    input$xAxisItalic
    input$yAxisBold
    input$yAxisItalic
    input$xTickBold
    input$xTickItalic
    input$xAxisAngle
    input$legendPosition
    input$showValues
    input$valueLabelSize
    input$valueLabelPosition
    input$valueLabelColor
    input$valueLabelBold
    input$valueLabelItalic
    input$valueLabelDigits
    input$showTrendLine
    input$showPoints
    input$customAxisBreaks
    input$yAxisBreakStep
    input$xAxisBreakStep
    input$xAxisMin
    input$xAxisMax
    input$yAxisMin
    input$yAxisMax
    input$yTickBold
    input$yTickItalic
    input$xTickSize
    input$yTickSize
    values$customXOrder
    values$storedLevelLabels
    values$plotUpdateTrigger
    input$xDateDisplayFormat
    input$plotTheme
    input$y2AxisLabel
    input$enableDualAxis
    input$y2AxisMin
    input$y2AxisMax
    input$y2AxisStep
    input$y2AxisBreakStep
    input$y2TickBold
    input$y2TickItalic
    input$y2TickSize
    input$y2AxisBold
    input$y2AxisItalic
    input$y2CurveWidth
    input$vizY2Type
    input$plotMarginTop
    input$plotMarginRight
    input$plotMarginBottom
    input$plotMarginLeft
    
    if (!is.null(values$yVarNames)) {
      lapply(values$yVarNames, function(v) input[[paste0("curveColor_", make.names(v))]])
    }
    
    data <- values$plotData
    x_var <- input$vizXVar
    viz_type <- input$vizType
    
    if(!is.null(values$multipleY) && values$multipleY &&
       all(c("Value", "Variable") %in% names(data))) {
      y_var <- "Value"
      color_var <- "Variable"
    } else {
      y_var <- input$vizYVar[1]
      color_var <- if(!is.null(input$vizColorVar) && input$vizColorVar != "Aucun" &&
                       input$vizColorVar %in% names(data)) {
        input$vizColorVar
      } else {
        NULL
      }
    }
    
    p <- tryCatch({
      switch(viz_type,
             "scatter" = create_scatter_plot(data, x_var, y_var, color_var),
             "line" = create_line_plot(data, x_var, y_var, color_var),
             "bar" = create_bar_plot(data, x_var, y_var, color_var),
             "box" = create_box_plot(data, x_var, y_var, color_var),
             "violin" = create_violin_plot(data, x_var, y_var, color_var),
             "seasonal_smooth" = create_seasonal_smooth_plot(data, x_var, y_var, color_var),
             "seasonal_evolution" = create_seasonal_evolution_plot(data, x_var, y_var, color_var),
             "histogram" = create_histogram_plot(data, x_var),
             "density" = create_density_plot(data, x_var, color_var),
             "heatmap" = create_heatmap_plot(data, x_var, y_var),
             "area" = create_area_plot(data, x_var, y_var, color_var),
             "pie" = create_pie_plot(data, x_var, y_var),
             "donut" = create_donut_plot(data, x_var, y_var),
             "treemap" = create_treemap_plot(data, x_var, y_var),
             {
               showNotification("Type de visualisation non reconnu", type = "error")
               return(NULL)
             }
      )
    }, error = function(e) {
      showNotification(paste("Erreur lors de la création du graphique:", e$message), 
                       type = "error", duration = 5)
      return(NULL)
    })
    
    if(is.null(p)) return(NULL)
    
    # Couleurs Y1 : differees jusqu'au scale_color_manual unifie Y1+Y2
    
    y2_active_vars <- if (isTRUE(values$dualAxisActive) && !is.null(values$y2VarsActive))
      values$y2VarsActive else character(0)
    
    if (length(y2_active_vars) > 0 &&
        !viz_type %in% c("pie","donut","treemap","heatmap","histogram","density")) {
      
      tryCatch({
        raw_full <- values$filteredData
        
        agg_func_y2 <- function(x) mean(as.numeric(x), na.rm = TRUE)
        if (isTRUE(input$useAggregation) && !is.null(input$aggFunction)) {
          agg_func_y2 <- switch(input$aggFunction,
                                "mean"   = function(x) mean(as.numeric(x),   na.rm = TRUE),
                                "median" = function(x) median(as.numeric(x), na.rm = TRUE),
                                "sum"    = function(x) sum(as.numeric(x),    na.rm = TRUE),
                                "count"  = function(x) sum(!is.na(x)),
                                "min"    = function(x) min(as.numeric(x),    na.rm = TRUE),
                                "max"    = function(x) max(as.numeric(x),    na.rm = TRUE),
                                "sd"     = function(x) sd(as.numeric(x),     na.rm = TRUE),
                                function(x) mean(as.numeric(x), na.rm = TRUE))
        }
        grp_vars_y2 <- if (!is.null(input$groupVars) && length(input$groupVars) > 0)
          intersect(input$groupVars, names(raw_full)) else x_var
        if (length(grp_vars_y2) == 0) grp_vars_y2 <- x_var
        
        y1_vals  <- if ("Value" %in% names(data)) data[["Value"]] else data[[y_var]]
        y1_min   <- min(y1_vals, na.rm = TRUE)
        y1_max   <- max(y1_vals, na.rm = TRUE)
        y1_range <- y1_max - y1_min
        if (!is.finite(y1_range) || y1_range == 0) y1_range <- 1
        
        default_colors <- c("#2196F3","#4CAF50","#9C27B0","#FF5722","#00BCD4",
                            "#FFC107","#E91E63","#607D8B","#795548","#009688")
        
        all_y_vars <- if (!is.null(values$yVarNames)) values$yVarNames else y_var
        y1_vars    <- setdiff(all_y_vars, y2_active_vars)
        
        get_curve_color <- function(var_name, idx) {
          col_id   <- paste0("curveColor_", make.names(var_name))
          user_col <- input[[col_id]]
          if (!is.null(user_col) && nchar(trimws(user_col)) > 0) return(user_col)
          default_colors[((idx - 1L) %% length(default_colors)) + 1L]
        }
        
        y1_color_map <- setNames(
          vapply(seq_along(y1_vars), function(i) get_curve_color(y1_vars[i], i), character(1)),
          y1_vars
        )
        
        x_is_date <- inherits(raw_full[[x_var]], c("Date","POSIXct","POSIXlt"))
        
        y2_idx_start <- length(y1_vars) + 1L
        y2_color_map <- list()
        y2_type <- input$vizY2Type %||% "line"
        
        # Pour boxplot/violin : on conserve les donnees brutes (par groupe X)
        # Pour les autres : on agrege en une valeur par groupe X
        types_distrib <- c("box", "violin")
        is_distrib_type <- y2_type %in% types_distrib
        
        y2_global_min <-  Inf
        y2_global_max <- -Inf
        y2_traces <- list()
        
        for (i in seq_along(y2_active_vars)) {
          yv <- y2_active_vars[i]
          if (!yv %in% names(raw_full)) next
          
          d2 <- NULL
          if (is_distrib_type) {
            # box/violin : donnees brutes par groupe X (pour montrer la distribution)
            d2 <- raw_full[!is.na(raw_full[[x_var]]) & !is.na(raw_full[[yv]]), , drop = FALSE]
            if (nrow(d2) > 0) d2$.y2_raw <- as.numeric(d2[[yv]])
          } else if (isTRUE(input$useAggregation)) {
            d2 <- tryCatch(
              raw_full %>%
                dplyr::filter(!is.na(.data[[x_var]]), !is.na(.data[[yv]])) %>%
                dplyr::group_by(dplyr::across(dplyr::all_of(grp_vars_y2))) %>%
                dplyr::summarise(.y2_raw = agg_func_y2(.data[[yv]]), .groups = "drop"),
              error = function(e) NULL)
          } else {
            # Pas d'agregation -> agreger quand meme en moyenne pour eviter le zigzag
            d2 <- tryCatch(
              raw_full %>%
                dplyr::filter(!is.na(.data[[x_var]]), !is.na(.data[[yv]])) %>%
                dplyr::group_by(dplyr::across(dplyr::all_of(x_var))) %>%
                dplyr::summarise(.y2_raw = mean(as.numeric(.data[[yv]]), na.rm = TRUE), .groups = "drop"),
              error = function(e) NULL)
          }
          if (is.null(d2) || nrow(d2) == 0) next
          
          if (x_is_date && !inherits(d2[[x_var]], c("Date","POSIXct","POSIXlt"))) {
            d2[[x_var]] <- as.Date(d2[[x_var]], origin = "1970-01-01")
          }
          d2 <- d2[order(d2[[x_var]]), , drop = FALSE]
          d2$.curve_label <- yv
          y2_traces[[yv]] <- d2
          
          mn <- suppressWarnings(min(d2$.y2_raw, na.rm = TRUE))
          mx <- suppressWarnings(max(d2$.y2_raw, na.rm = TRUE))
          if (is.finite(mn)) y2_global_min <- min(y2_global_min, mn)
          if (is.finite(mx)) y2_global_max <- max(y2_global_max, mx)
        }
        
        if (length(y2_traces) == 0) {
          values$y2RangeForAxis <- NULL
        } else {
          if (!is.finite(y2_global_min)) y2_global_min <- 0
          if (!is.finite(y2_global_max)) y2_global_max <- 1
          if (y2_global_max == y2_global_min) y2_global_max <- y2_global_min + 1
          
          user_y2_min <- if (!is.null(input$y2AxisMin) && !is.na(input$y2AxisMin)) input$y2AxisMin else y2_global_min
          user_y2_max <- if (!is.null(input$y2AxisMax) && !is.na(input$y2AxisMax)) input$y2AxisMax else y2_global_max
          eff_y2_rng  <- user_y2_max - user_y2_min
          if (!is.finite(eff_y2_rng) || eff_y2_rng == 0) eff_y2_rng <- 1
          
          sf  <- y1_range / eff_y2_rng
          off <- y1_min - user_y2_min * sf
          values$y2RangeForAxis <- list(min = user_y2_min, max = user_y2_max,
                                        sf = sf, off = off)
          
          y2_lw <- input$y2CurveWidth %||% 1.2
          
          for (i in seq_along(names(y2_traces))) {
            yv <- names(y2_traces)[i]
            d2 <- y2_traces[[yv]]
            d2$.y2_scaled <- d2$.y2_raw * sf + off
            col_y2 <- get_curve_color(yv, y2_idx_start + i - 1L)
            y2_color_map[[yv]] <- col_y2
            
            # CRITIQUE : inherit.aes = FALSE sur TOUS les geoms Y2
            # evite que ggplotly cherche les aes du plot principal (ex: Value)
            if (y2_type == "line") {
              p <- p + geom_line(
                data = d2, inherit.aes = FALSE,
                aes(x = .data[[x_var]], y = .data$.y2_scaled,
                    color = .data$.curve_label, group = .data$.curve_label),
                linewidth = y2_lw, na.rm = TRUE, show.legend = TRUE)
            } else if (y2_type == "scatter") {
              p <- p + geom_point(
                data = d2, inherit.aes = FALSE,
                aes(x = .data[[x_var]], y = .data$.y2_scaled,
                    color = .data$.curve_label, group = .data$.curve_label),
                size = 3, shape = 17, alpha = 0.9, na.rm = TRUE, show.legend = TRUE)
            } else if (y2_type == "points_line") {
              p <- p +
                geom_line(
                  data = d2, inherit.aes = FALSE,
                  aes(x = .data[[x_var]], y = .data$.y2_scaled,
                      color = .data$.curve_label, group = .data$.curve_label),
                  linewidth = y2_lw, na.rm = TRUE, show.legend = TRUE) +
                geom_point(
                  data = d2, inherit.aes = FALSE,
                  aes(x = .data[[x_var]], y = .data$.y2_scaled,
                      color = .data$.curve_label, group = .data$.curve_label),
                  size = 2.5, na.rm = TRUE, show.legend = FALSE)
            } else if (y2_type == "seasonal_evolution") {
              p <- p +
                geom_line(
                  data = d2, inherit.aes = FALSE,
                  aes(x = .data[[x_var]], y = .data$.y2_scaled,
                      color = .data$.curve_label, group = .data$.curve_label),
                  linewidth = y2_lw, na.rm = TRUE, show.legend = TRUE) +
                geom_point(
                  data = d2, inherit.aes = FALSE,
                  aes(x = .data[[x_var]], y = .data$.y2_scaled,
                      color = .data$.curve_label, group = .data$.curve_label),
                  size = 2, alpha = 0.85, na.rm = TRUE, show.legend = FALSE)
            } else if (y2_type == "smooth") {
              p <- p + geom_smooth(
                data = d2, inherit.aes = FALSE,
                aes(x = .data[[x_var]], y = .data$.y2_scaled,
                    color = .data$.curve_label, group = .data$.curve_label),
                method = "loess", se = FALSE,
                linewidth = y2_lw, na.rm = TRUE, show.legend = TRUE)
            } else if (y2_type == "area") {
              p <- p +
                geom_area(
                  data = d2, inherit.aes = FALSE,
                  aes(x = .data[[x_var]], y = .data$.y2_scaled,
                      fill = .data$.curve_label, group = .data$.curve_label),
                  alpha = 0.35, na.rm = TRUE, show.legend = TRUE) +
                geom_line(
                  data = d2, inherit.aes = FALSE,
                  aes(x = .data[[x_var]], y = .data$.y2_scaled,
                      color = .data$.curve_label, group = .data$.curve_label),
                  linewidth = y2_lw * 0.8, na.rm = TRUE, show.legend = FALSE)
            } else if (y2_type == "bar") {
              p <- p + geom_col(
                data = d2, inherit.aes = FALSE,
                aes(x = .data[[x_var]], y = .data$.y2_scaled,
                    fill = .data$.curve_label, group = .data$.curve_label),
                alpha = 0.7, width = 0.7, position = "dodge",
                na.rm = TRUE, show.legend = TRUE)
            } else if (y2_type == "box") {
              p <- p + geom_boxplot(
                data = d2, inherit.aes = FALSE,
                aes(x = .data[[x_var]], y = .data$.y2_scaled,
                    color = .data$.curve_label, fill = .data$.curve_label,
                    group = interaction(.data[[x_var]], .data$.curve_label)),
                alpha = 0.3, outlier.size = 1.5, na.rm = TRUE, show.legend = TRUE)
            } else if (y2_type == "violin") {
              p <- p + geom_violin(
                data = d2, inherit.aes = FALSE,
                aes(x = .data[[x_var]], y = .data$.y2_scaled,
                    color = .data$.curve_label, fill = .data$.curve_label,
                    group = interaction(.data[[x_var]], .data$.curve_label)),
                alpha = 0.3, trim = FALSE, na.rm = TRUE, show.legend = TRUE)
            } else if (y2_type == "errorbar") {
              # Stats : moyenne et ecart-type des valeurs deja scalees, par groupe X
              d2_stat <- d2 %>%
                dplyr::group_by(.data[[x_var]], .data$.curve_label) %>%
                dplyr::summarise(
                  .y2_mean = mean(.data$.y2_scaled, na.rm = TRUE),
                  .y2_sd   = stats::sd(.data$.y2_scaled, na.rm = TRUE),
                  .groups  = "drop") %>%
                dplyr::mutate(.y2_sd = ifelse(is.na(.y2_sd), 0, .y2_sd))
              p <- p +
                geom_point(
                  data = d2_stat, inherit.aes = FALSE,
                  aes(x = .data[[x_var]], y = .data$.y2_mean,
                      color = .data$.curve_label, group = .data$.curve_label),
                  size = 3, na.rm = TRUE, show.legend = TRUE) +
                geom_errorbar(
                  data = d2_stat, inherit.aes = FALSE,
                  aes(x    = .data[[x_var]],
                      ymin = .data$.y2_mean - .data$.y2_sd,
                      ymax = .data$.y2_mean + .data$.y2_sd,
                      color = .data$.curve_label, group = .data$.curve_label),
                  width = 0.25, na.rm = TRUE, show.legend = FALSE)
            } else {
              p <- p + geom_line(
                data = d2, inherit.aes = FALSE,
                aes(x = .data[[x_var]], y = .data$.y2_scaled,
                    color = .data$.curve_label, group = .data$.curve_label),
                linewidth = y2_lw, na.rm = TRUE, show.legend = TRUE)
            }
          }
        }
        
        all_color_map <- c(y1_color_map, unlist(y2_color_map))
        if (length(all_color_map) > 0) {
          values$y2UnifiedColorMap <- all_color_map
          tryCatch(
            suppressWarnings(
              p <- p + scale_color_manual(
                values = all_color_map,
                breaks = names(all_color_map),
                labels = names(all_color_map),
                name   = NULL, drop = FALSE
              ) + scale_fill_manual(
                values = all_color_map,
                breaks = names(all_color_map),
                labels = names(all_color_map),
                name   = NULL, drop = FALSE
              )
            ),
            error = function(e) invisible(NULL)
          )
        }
        
        if (!is.null(values$y2RangeForAxis)) {
          rng <- values$y2RangeForAxis
          sf2  <- rng$sf
          off2 <- rng$off
          eff_y2_mn <- rng$min
          eff_y2_mx <- rng$max
          
          y2_label <- if (!is.null(input$y2AxisLabel) && nchar(trimws(input$y2AxisLabel)) > 0)
            input$y2AxisLabel else paste(y2_active_vars, collapse = " / ")
          
          user_y2_step <- {
            s <- input$y2AxisBreakStep %||% input$y2AxisStep
            if (!is.null(s) && !is.na(s) && is.numeric(s) && s > 0) s else NULL
          }
          sec_breaks <- if (!is.null(user_y2_step))
            seq(eff_y2_mn, eff_y2_mx, by = user_y2_step) else waiver()
          
          tryCatch(
            suppressMessages(suppressWarnings(
              p <- p + scale_y_continuous(
                sec.axis = sec_axis(
                  ~ (. - off2) / sf2,
                  name   = y2_label,
                  breaks = sec_breaks
                )
              )
            )),
            error = function(e) {
              showNotification(paste("Axe Y2 :", conditionMessage(e)),
                               type = "warning", duration = 3)
            }
          )
        }
        
      }, error = function(e) {
        showNotification(paste("Axe Y2 :", e$message), type = "warning", duration = 4)
      })
    }
    
    if(!is.null(input$vizFacetVar) && input$vizFacetVar != "Aucun") {
      facet_var_safe <- if (grepl("[/+*^()%$@!? -]|^[0-9]", input$vizFacetVar, perl = TRUE)) {
        paste0("`", input$vizFacetVar, "`")
      } else { input$vizFacetVar }
      p <- p + facet_wrap(as.formula(paste("~", facet_var_safe)),
                          scales = if(isTRUE(input$facetScalesFree)) "free" else "fixed")
    }
    
    # Labels personnalises de legende -- inclus Y1 + Y2
    if (!is.null(color_var)) {
      storage_key <- if (color_var == "Variable") "multiY_legend" else color_var
      legend_map  <- values$legendLabels[[storage_key]]
      unified_map <- if (!is.null(values$y2UnifiedColorMap)) values$y2UnifiedColorMap else NULL
      
      if (!is.null(legend_map) && length(legend_map) > 0) {
        if (color_var %in% names(data)) {
          current_levels <- unique(as.character(data[[color_var]]))
        } else {
          current_levels <- names(legend_map)
        }
        
        all_keys <- if (!is.null(unified_map)) unique(c(names(unified_map), current_levels))
        else current_levels
        lm_full  <- setNames(all_keys, all_keys)
        for (k in names(legend_map)) {
          if (k %in% all_keys) lm_full[[k]] <- as.character(legend_map[[k]])
        }
        
        default_pal <- c("#2196F3","#4CAF50","#9C27B0","#FF5722","#00BCD4",
                         "#FFC107","#E91E63","#607D8B","#795548","#009688")
        color_vals  <- setNames(
          vapply(seq_along(all_keys), function(i) {
            k <- all_keys[i]
            if (!is.null(unified_map) && k %in% names(unified_map))
              return(unified_map[[k]])
            col_id  <- paste0("curveColor_", make.names(k))
            usr_col <- input[[col_id]]
            if (!is.null(usr_col) && nchar(trimws(usr_col)) > 0) return(usr_col)
            default_pal[((i-1L) %% length(default_pal)) + 1L]
          }, character(1)),
          all_keys
        )
        
        tryCatch(
          suppressWarnings(
            p <- p +
              scale_color_manual(
                values = color_vals,
                breaks = all_keys,
                labels = as.character(lm_full[all_keys]),
                name   = NULL, drop = FALSE
              ) +
              scale_fill_manual(
                values = color_vals,
                breaks = all_keys,
                labels = as.character(lm_full[all_keys]),
                name   = NULL, drop = FALSE
              )
          ),
          error = function(e) invisible(NULL)
        )
      }
    }
    
    # Personnalisation du thème pour tous les types de graphiques
    # Créer les paramètres de formatage des axes
    x_axis_face <- if(isTRUE(input$xAxisBold) && isTRUE(input$xAxisItalic)) {
      "bold.italic"
    } else if(isTRUE(input$xAxisBold)) {
      "bold"
    } else if(isTRUE(input$xAxisItalic)) {
      "italic"
    } else {
      "plain"
    }
    
    y_axis_face <- if(isTRUE(input$yAxisBold) && isTRUE(input$yAxisItalic)) {
      "bold.italic"
    } else if(isTRUE(input$yAxisBold)) {
      "bold"
    } else if(isTRUE(input$yAxisItalic)) {
      "italic"
    } else {
      "plain"
    }
    
    x_tick_face <- if(isTRUE(input$xTickBold) && isTRUE(input$xTickItalic)) {
      "bold.italic"
    } else if(isTRUE(input$xTickBold)) {
      "bold"
    } else if(isTRUE(input$xTickItalic)) {
      "italic"
    } else {
      "plain"
    }
    
    y_tick_face <- if(isTRUE(input$yTickBold) && isTRUE(input$yTickItalic)) {
      "bold.italic"
    } else if(isTRUE(input$yTickBold)) {
      "bold"
    } else if(isTRUE(input$yTickItalic)) {
      "italic"
    } else {
      "plain"
    }
    
    x_tick_size <- input$xTickSize %||% 12
    y_tick_size <- input$yTickSize %||% 12
    
    x_angle <- input$xAxisAngle %||% 0
    x_hjust <- if(x_angle > 0) 1 else 0.5
    x_vjust <- if(x_angle > 0) 1 else 0.5
    
    # Appliquer le thème pour tous les graphiques (arrière-plan choisi)
    axis_lw <- input$axisLineSize %||% 0.8
    
    # Marges du graphique (en points -- 1pt ~ 1px écran)
    pm_top    <- input$plotMarginTop    %||% 10
    pm_right  <- input$plotMarginRight  %||% 30
    pm_bottom <- input$plotMarginBottom %||% 10
    pm_left   <- input$plotMarginLeft   %||% 10
    
    p <- p +
      get_plot_theme(base_size = input$baseFontSize %||% 12) +
      theme(
        plot.title    = element_markdown(hjust = 0.5, face = "bold", size = input$titleSize %||% 14),
        axis.title.x  = element_markdown(face = x_axis_face, size = input$axisLabelSize %||% 12),
        axis.title.y  = element_markdown(face = y_axis_face, size = input$axisLabelSize %||% 12),
        axis.text.x   = element_text(face = x_tick_face, size = x_tick_size, angle = x_angle, hjust = x_hjust, vjust = x_vjust),
        axis.text.y   = element_text(face = y_tick_face, size = y_tick_size),
        legend.position    = input$legendPosition %||% "right",
        legend.title       = element_markdown(size = input$legendTitleSize %||% 12, face = "bold"),
        legend.text        = element_text(size = input$legendTextSize  %||% 12),
        legend.key.size    = unit(input$legendKeySize %||% 1, "lines"),
        legend.margin      = margin(t = 6, r = 6, b = 6, l = 6),
        legend.box.margin  = margin(t = 10, r = 0, b = 0, l = 0),
        axis.line   = element_line(color = "black", linewidth = axis_lw),
        axis.ticks  = element_line(color = "black", linewidth = axis_lw * 0.75),
        plot.margin = margin(t = pm_top, r = pm_right, b = pm_bottom, l = pm_left, unit = "pt")
      )
    
    if(!is.null(input$plotTitle) && input$plotTitle != "") {
      p <- p + ggtitle(input$plotTitle)
    }
    
    if(!is.null(input$xAxisLabel) && input$xAxisLabel != "") {
      p <- p + xlab(input$xAxisLabel)
    } else {
      p <- p + xlab(x_var)
    }
    
    if(!is.null(input$yAxisLabel) && input$yAxisLabel != "") {
      p <- p + ylab(input$yAxisLabel)
    } else {
      p <- p + ylab(y_var)
    }
    
    if(!is.null(input$legendTitle) && input$legendTitle != "" && !is.null(color_var)) {
      p <- p + labs(color = input$legendTitle, fill = input$legendTitle)
    }
    
    # ---- Graduations et limites des axes ----
    # Calculer les breaks Y si demandé
    y_breaks_custom <- NULL
    x_breaks_custom <- NULL
    
    if(isTRUE(input$customAxisBreaks)) {
      y_step <- input$yAxisBreakStep
      if(!is.null(y_step) && !is.na(y_step) && is.numeric(y_step) && y_step > 0) {
        tryCatch({
          y_data <- data[[y_var]]
          y_data <- y_data[is.finite(y_data)]
          if(length(y_data) > 0) {
            y_b_min <- floor(min(y_data, na.rm = TRUE) / y_step) * y_step
            y_b_max <- ceiling(max(y_data, na.rm = TRUE) / y_step) * y_step
            y_breaks_custom <- seq(y_b_min, y_b_max, by = y_step)
          }
        }, error = function(e) NULL)
      }
      
      x_step <- input$xAxisBreakStep
      if(!is.null(x_step) && !is.na(x_step) && is.numeric(x_step) && x_step > 0) {
        tryCatch({
          x_data <- data[[x_var]]
          if(is.numeric(x_data)) {
            x_data <- x_data[is.finite(x_data)]
            if(length(x_data) > 0) {
              x_b_min <- floor(min(x_data, na.rm = TRUE) / x_step) * x_step
              x_b_max <- ceiling(max(x_data, na.rm = TRUE) / x_step) * x_step
              x_breaks_custom <- seq(x_b_min, x_b_max, by = x_step)
            }
          }
        }, error = function(e) NULL)
      }
    }
    
    if(!is.null(y_breaks_custom) && !viz_type %in% c("pie", "donut", "treemap")) {
      tryCatch({ p <- p + scale_y_continuous(breaks = y_breaks_custom) }, error = function(e) NULL)
    }
    
    if(!is.null(x_breaks_custom) && !viz_type %in% c(
      "pie", "donut", "treemap", "bar", "box", "violin",
      "histogram", "density", "line", "scatter", "area",
      "seasonal_smooth", "seasonal_evolution")) {
      tryCatch({ p <- p + scale_x_continuous(breaks = x_breaks_custom) }, error = function(e) NULL)
    }
    
    # ---- Limites des axes via coord_cartesian uniquement 
    y_min_val <- input$yAxisMin
    y_max_val <- input$yAxisMax
    x_min_val <- input$xAxisMin
    x_max_val <- input$xAxisMax
    
    has_y_limits <- (!is.null(y_min_val) && !is.na(y_min_val)) || (!is.null(y_max_val) && !is.na(y_max_val))
    has_x_limits <- (!is.null(x_min_val) && !is.na(x_min_val)) || (!is.null(x_max_val) && !is.na(x_max_val))
    
    y_lim <- NULL
    x_lim <- NULL
    
    if(has_y_limits && !viz_type %in% c("pie", "donut", "treemap")) {
      y_lim <- c(
        if(!is.null(y_min_val) && !is.na(y_min_val)) as.numeric(y_min_val) else NA_real_,
        if(!is.null(y_max_val) && !is.na(y_max_val)) as.numeric(y_max_val) else NA_real_
      )
    }
    
    if(has_x_limits && !viz_type %in% c("pie", "donut", "treemap", "bar", "box", "violin", "histogram", "density")) {
      x_data_check <- data[[x_var]]
      if(is.numeric(x_data_check)) {
        x_lim <- c(
          if(!is.null(x_min_val) && !is.na(x_min_val)) as.numeric(x_min_val) else NA_real_,
          if(!is.null(x_max_val) && !is.na(x_max_val)) as.numeric(x_max_val) else NA_real_
        )
      }
    }
    
    if(!is.null(y_lim) || !is.null(x_lim)) {
      tryCatch({
        p <- p + coord_cartesian(
          ylim = y_lim,
          xlim = x_lim,
          expand = TRUE
        )
      }, error = function(e) NULL)
    }
    
    # - Appliquer les contrôles ggplot thème pour l'axe Y secondaire -
    
    # Tous les paramètres visuels MIROIR de l'axe Y principal.
    if (isTRUE(values$dualAxisActive) && length(values$y2VarsActive %||% character(0)) > 0) {
      # Miroir exact de Y1 : mêmes inputs
      y2_lw_gg    <- axis_lw                 
      y2_tick_sz  <- y_tick_size             
      y2_tick_fce <- y_tick_face             
      y2_lbl_sz   <- input$axisLabelSize %||% 12  
      y2_lbl_fce  <- y_axis_face             
      tryCatch({
        p <- p + theme(
          axis.text.y.right  = element_text(
            size  = y2_tick_sz,
            face  = y2_tick_fce,
            color = "black"
          ),
          axis.title.y.right = element_text(
            size   = y2_lbl_sz,
            face   = y2_lbl_fce,
            color  = "black",
            margin = margin(l = 8)
          ),
          axis.line.y.right  = element_line(
            color     = "black",
            linewidth = y2_lw_gg
          ),
          axis.ticks.y.right = element_line(
            color     = "black",
            linewidth = y2_lw_gg * 0.75
          ),
          axis.ticks.length.y.right = unit(4, "pt")
        )
      }, error = function(e) invisible(NULL))
    }
    
    return(p)
  })
  
  
  output$aggregationInfo <- renderText({
    req(input$useAggregation, input$aggFunction)
    
    if(!isTRUE(input$useAggregation)) return("")
    
    tryCatch({
      agg_names <- c(
        "mean" = "Moyenne", "median" = "Médiane", "sum" = "Somme",
        "count" = "Comptage", "min" = "Minimum", "max" = "Maximum", "sd" = "Écart-type"
      )
      
      info_text <- paste0("Fonction: ", agg_names[[input$aggFunction]] %||% "Inconnue", "\n")
      
      if(!is.null(input$groupVars) && length(input$groupVars) > 0) {
        info_text <- paste0(info_text, "Groupement par: ", 
                            paste(input$groupVars, collapse = ", "), "\n")
      }
      
      if(values$multipleY) {
        info_text <- paste0(info_text, "\n\nMode multi-Y: Agrégation par variable")
      }
      
      return(info_text)
    }, error = function(e) {
      "Erreur dans le calcul"
    })
  })
  
  output$seasonalInfo <- renderText({
    req(input$vizType %in% c("seasonal_smooth", "seasonal_evolution"))
    
    tryCatch({
      x_type <- if(input$xVarType == "auto") values$detectedXType else input$xVarType
      
      info_text <- if(input$vizType == "seasonal_evolution") {
        "Type: Courbe d'évolution temporelle\n"
      } else {
        "Type: Courbe avec lissage\n"
      }
      
      info_text <- paste0(info_text, "Variable X: ", x_type, "\n")
      
      if(x_type %in% c("factor", "categorical", "text")) {
        info_text <- paste0(info_text, 
                            "Mode catégoriel activé\n",
                            "Ordre personnalisé: ",
                            if(!is.null(input$xLevelOrder)) "Oui" else "Par défaut")
      }
      
      if(values$multipleY) {
        info_text <- paste0(info_text, "\n\nComparaison de ", 
                            length(values$yVarNames), " séries:\n",
                            paste(values$yVarNames, collapse = "\n"))
      }
      
      if(isTRUE(input$useAggregation)) {
        info_text <- paste0(info_text, "\n\nAgrégation: Activée")
      }
      
      return(info_text)
    }, error = function(e) {
      "Erreur"
    })
  })
  
  output$dataStatsSummary <- renderText({
    req(values$plotData)
    tryCatch({
      data <- values$plotData
      num_vars <- sum(sapply(data, is.numeric))
      cat_vars <- sum(sapply(data, function(x) is.factor(x) || is.character(x)))
      date_vars <- sum(sapply(data, function(x) inherits(x, "Date") || inherits(x, "POSIXt")))
      logical_vars <- sum(sapply(data, is.logical))
      missing_values <- sum(is.na(data))
      complete_rows <- sum(complete.cases(data))
      
      base_stats <- paste0("Nombre d'observations: ", nrow(data), "\n",
                           "Variables totales: ", ncol(data), "\n",
                           "Variables numériques: ", num_vars, "\n",
                           "Variables catégorielles: ", cat_vars, "\n",
                           "Variables temporelles: ", date_vars, "\n",
                           "Variables logiques: ", logical_vars, "\n",
                           "Valeurs manquantes: ", missing_values, "\n",
                           "Lignes complètes: ", complete_rows, " (", round(complete_rows/nrow(data)*100, 1), "%)")
      
      if(!is.null(values$multipleY) && values$multipleY) {
        base_stats <- paste0(base_stats, "\n\n",
                             "Mode: Variables Y multiples\n",
                             "Variables Y actives: ", length(values$yVarNames), "\n",
                             "Variables: ", paste(values$yVarNames, collapse = ", "))
      }
      
      return(base_stats)
    }, error = function(e) {
      showNotification("Erreur dans le calcul des statistiques", type = "error", duration = 5)
      "Erreur dans le calcul des statistiques"
    })
  })
  
  output$plotStatsSummary <- renderText({
    req(values$currentInteractivePlot, input$vizXVar, input$vizYVar)
    tryCatch({
      viz_type_names <- c(
        "scatter" = "Nuage de points",
        "seasonal_smooth" = "Courbe saisonnière avec lissage",
        "seasonal_evolution" = "Courbe évolution saison",
        "box" = "Boxplot",
        "violin" = "Violon",
        "bar" = "Barres",
        "line" = "Lignes",
        "density" = "Densité",
        "histogram" = "Histogramme",
        "heatmap" = "Heatmap",
        "area" = "Aires empilées",
        "pie" = "Camembert",
        "donut" = "Donut",
        "treemap" = "Treemap"
      )
      
      viz_type_name <- viz_type_names[[input$vizType]] %||% "Type inconnu"
      
      y_display <- if(!is.null(values$multipleY) && values$multipleY) {
        paste0("Variables Y (", length(values$yVarNames), "): ", 
               paste(values$yVarNames, collapse = ", "))
      } else {
        paste0("Variable Y: ", input$vizYVar[1])
      }
      
      plot_info <- paste0("Type: ", viz_type_name, "\n",
                          "Variable X: ", input$vizXVar, "\n",
                          y_display)
      
      if(is.null(values$multipleY) || !values$multipleY) {
        if (!is.null(input$vizColorVar) && input$vizColorVar != "Aucun") {
          plot_info <- paste0(plot_info, "\nVariable couleur: ", input$vizColorVar)
        }
      } else {
        plot_info <- paste0(plot_info, "\nCouleurs: Distinguent les variables Y")
      }
      
      if (!is.null(input$vizFacetVar) && input$vizFacetVar != "Aucun") {
        plot_info <- paste0(plot_info, "\nFacetting: ", input$vizFacetVar)
      }
      
      if (isTRUE(input$useAggregation) && !is.null(input$aggFunction)) {
        agg_names <- c(
          "mean" = "Moyenne", "median" = "Médiane", "sum" = "Somme",
          "count" = "Comptage", "min" = "Minimum", "max" = "Maximum", "sd" = "Écart-type"
        )
        agg_name <- agg_names[[input$aggFunction]] %||% "Inconnue"
        plot_info <- paste0(plot_info, "\nAgrégation: ", agg_name)
        if (!is.null(input$groupVars) && length(input$groupVars) > 0) {
          plot_info <- paste0(plot_info, " par ", paste(input$groupVars, collapse = ", "))
        }
      }
      
      plot_info <- paste0(plot_info, "\nObservations utilisées: ", nrow(values$plotData))
      return(plot_info)
    }, error = function(e) {
      showNotification("Erreur dans le calcul des statistiques du graphique", type = "error", duration = 5)
      "Erreur dans le calcul des statistiques du graphique"
    })
  })
  
  output$seasonalDuplicateWarning <- renderUI({
    req(input$vizType %in% c("seasonal_smooth","seasonal_evolution"))
    req(values$filteredData, input$vizXVar, input$vizYVar)
    tryCatch({
      d  <- if (isTRUE(input$useAggregation)) aggregatedData() else values$filteredData
      xv <- input$vizXVar; yv <- input$vizYVar[1]
      if (!xv %in% names(d) || !yv %in% names(d)) return(NULL)
      d_valid <- d[!is.na(d[[yv]]), ]
      x_total <- nrow(d_valid); x_uniq <- length(unique(d_valid[[xv]]))
      if (x_total==0) return(div(class="alert alert-danger",
                                 style="padding:10px;margin-bottom:10px;font-size:13px;",
                                 icon("times-circle"), strong(" Aucune donnée valide -- toutes les Y sont NA.")))
      if (x_total>x_uniq && !isTRUE(input$useAggregation))
        return(div(class="alert alert-warning",
                   style="padding:10px;margin-bottom:10px;font-size:13px;",
                   icon("exclamation-triangle"), strong(" Valeurs X répétées"),
                   sprintf(" (%d obs. -> %d valeurs uniques).", x_total, x_uniq), tags$br(),
                   "Agrégation automatique par ", strong("moyenne"), " appliquée.",tags$br(),
                   "Pour une autre fonction, activez ", strong("l'agrégation manuelle"), "."))
      return(div(class="alert alert-success",
                 style="padding:8px 12px;margin-bottom:10px;font-size:12px;",
                 icon("check-circle"), sprintf(" %d valeurs X uniques -- courbe directe.", x_uniq)))
    }, error=function(e) NULL)
  })
  
  output$seasonalAnalysisSummary <- renderText({
    req(input$vizType %in% c("seasonal_smooth", "seasonal_evolution"))
    tryCatch({
      summary_text <- "Analyse saisonnière activée\n"
      
      if (input$vizType == "seasonal_evolution") {
        summary_text <- paste0(summary_text, "Évolution temporelle directe des observations\n")
      } else {
        summary_text <- paste0(summary_text, "Analyse avec lissage des tendances\n")
      }
      
      if (isTRUE(input$showSmoothLine) && !is.null(input$smoothMethod)) {
        smooth_info <- switch(input$smoothMethod,
                              "loess" = paste0("Lissage LOESS (span: ", input$smoothSpan %||% 0.75, ")"),
                              "lm" = "Régression linéaire",
                              "gam" = "Modèle additif généralisé",
                              "Lissage activé")
        summary_text <- paste0(summary_text, smooth_info, "\n")
      }
      
      if (isTRUE(input$showConfidenceInterval) && isTRUE(input$showSmoothLine)) {
        summary_text <- paste0(summary_text, "Intervalle de confiance affiché\n")
      }
      
      if(!is.null(values$multipleY) && values$multipleY) {
        summary_text <- paste0(summary_text, "\nMode Y multiple: Comparaison de ", 
                               length(values$yVarNames), " séries temporelles")
      }
      
      return(summary_text)
    }, error = function(e) {
      showNotification("Erreur dans l'analyse saisonnière", type = "error", duration = 5)
      "Erreur dans l'analyse saisonnière"
    })
  })
  
  
  output$multiYIndicator <- reactive({
    !is.null(values$multipleY) && values$multipleY
  })
  outputOptions(output, "multiYIndicator", suspendWhenHidden = FALSE)
  
  observe({
    if(!is.null(values$multipleY) && values$multipleY) {
      runjs(paste0("
      $('#multiYBadge').text('", length(values$yVarNames), " variables');
    "))
    }
  })
  
  observeEvent(input$customizePlot, {
    showModal(modalDialog(
      title = tagList(icon("paint-brush"), " Personnalisation Rapide"),
      size = "l",
      
      fluidRow(
        column(6,
               h5("Titres", style = "color: #007bff; font-weight: bold;"),
               textInput(ns("quickPlotTitle"), "Titre:", value = input$plotTitle %||% "", placeholder = "Titre du graphique"),
               textInput(ns("quickXLabel"), "Label X:", value = input$xAxisLabel %||% "", placeholder = "Auto"),
               textInput(ns("quickYLabel"), "Label Y:", value = input$yAxisLabel %||% "", placeholder = "Auto")
        ),
        column(6,
               h5("Apparence", style = "color: #007bff; font-weight: bold;"),
               sliderInput(ns("quickPointSize"), "Taille des points:", min = 1, max = 10, value = input$pointSize %||% 3, step = 0.5),
               sliderInput(ns("quickLineWidth"), "Épaisseur des lignes:", min = 0.5, max = 5, value = input$lineWidth %||% 1, step = 0.5),
               selectInput(ns("quickLegendPos"), "Position légende:", 
                           choices = c("Droite" = "right", "Gauche" = "left", "Haut" = "top", "Bas" = "bottom", "Aucune" = "none"),
                           selected = input$legendPosition %||% "right")
        )
      ),
      
      footer = tagList(
        actionButton(ns("applyQuickCustom"), "Appliquer", class = "btn-primary"),
        modalButton("Fermer")
      )
    ))
  })
  
  observeEvent(input$applyQuickCustom, {
    updateTextInput(session, "plotTitle", value = input$quickPlotTitle)
    updateTextInput(session, "xAxisLabel", value = input$quickXLabel)
    updateTextInput(session, "yAxisLabel", value = input$quickYLabel)
    updateSliderInput(session, "pointSize", value = input$quickPointSize)
    updateSliderInput(session, "lineWidth", value = input$quickLineWidth)
    updateSelectInput(session, "legendPosition", selected = input$quickLegendPos)
    
    values$plotUpdateTrigger <- runif(1)
    
    removeModal()
    showNotification("Personnalisation appliquée", type = "message", duration = 2)
  })
  
  
  output$calculatedDimensions <- renderUI({
    req(input$exportDPI)
    dpi <- input$exportDPI
    if (is.null(dpi) || is.na(dpi)) dpi <- 300
    dpi <- max(300, min(20000, dpi))
    
    if (dpi <= 600) { w <- 12; h <- 8 }
    else if (dpi <= 1200) { w <- 10; h <- 6.67 }
    else if (dpi <= 2400) { w <- 8; h <- 5.33 }
    else if (dpi <= 5000) { w <- 7; h <- 4.67 }
    else { w <- 6; h <- 4 }
    
    px_w <- round(w * dpi)
    px_h <- round(h * dpi)
    
    div(
      style = "padding: 8px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
             border-radius: 5px; color: white; font-size: 13px;",
      div(style = "font-weight: bold;", sprintf("%.1f x %.1f pouces", w, h)),
      div(style = "font-size: 11px; opacity: 0.9;",
          sprintf("%s x %s px", format(px_w, big.mark = " "), format(px_h, big.mark = " ")))
    )
  })
  
  getPlotForDownload <- function() {
    if (is.null(values$plotData) || 
        is.null(input$vizXVar) || 
        is.null(input$vizYVar) || 
        is.null(input$vizType)) {
      return(NULL)
    }
    
    tryCatch({
      return(createPlot())
    }, error = function(e) {
      return(NULL)
    })
  }
  
  observeEvent(input$downloadPlotBtn, {
    
    shinyjs::disable("downloadPlotBtn")
    
    tryCatch({
      showNotification("Génération du graphique en cours...", type = "message", duration = 2, id = "download_notif")
      
      p <- getPlotForDownload()
      # Capuchons de moustache sur les boxplots du fichier exporte (l'apercu
      # plotly les dessine deja ; le ggplot brut non). Idempotent.
      if (inherits(p, "ggplot")) p <- hstat_add_whisker_caps(p)
      
      if (is.null(p)) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Veuillez d'abord charger des données\net créer un graphique", 
                   size = 5) + 
          theme_void() +
          theme(plot.background = element_rect(fill = "white", color = NA))
      }
      
      fmt <- input$exportFormat
      if (is.null(fmt)) fmt <- "png"
      
      dpi <- input$exportDPI
      if (is.null(dpi) || is.na(dpi)) dpi <- 300
      dpi <- as.integer(max(300, min(20000, dpi)))
      
      if (dpi <= 600) { w <- 12; h <- 8 }
      else if (dpi <= 1200) { w <- 10; h <- 6.67 }
      else if (dpi <= 2400) { w <- 8; h <- 5.33 }
      else if (dpi <= 5000) { w <- 7; h <- 4.67 }
      else { w <- 6; h <- 4 }
      
      ext <- switch(fmt,
                    "jpeg" = "jpg",
                    "tiff" = "tif",
                    fmt)
      
      temp_file <- tempfile(fileext = paste0(".", ext))
      
      while(length(dev.list()) > 0) try(dev.off(), silent = TRUE)
      
      ggplot2::ggsave(
        filename = temp_file,
        plot = p,
        device = fmt,
        width = w,
        height = h,
        units = "in",
        dpi = dpi,
        bg = "white"
      )
      
      if (!file.exists(temp_file)) {
        showNotification("Erreur: Le fichier n'a pas pu être créé", type = "error", duration = 5)
        shinyjs::enable("downloadPlotBtn")
        return()
      }
      
      # Lire le fichier en binaire et encoder en base64
      file_content <- readBin(temp_file, "raw", file.info(temp_file)$size)
      base64_content <- base64enc::base64encode(file_content)
      
      mime_type <- switch(fmt,
                          "png" = "image/png",
                          "jpeg" = "image/jpeg",
                          "tiff" = "image/tiff",
                          "bmp" = "image/bmp",
                          "pdf" = "application/pdf",
                          "svg" = "image/svg+xml",
                          "eps" = "application/postscript",
                          "application/octet-stream")
      
      filename <- paste0("graphique_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      
      js_code <- sprintf(
        "
      (function() {
        var link = document.createElement('a');
        link.href = 'data:%s;base64,%s';
        link.download = '%s';
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      })();
      ",
        mime_type,
        base64_content,
        filename
      )
      
      shinyjs::runjs(js_code)
      
      unlink(temp_file)
      
      removeNotification(id = "download_notif")
      showNotification(
        paste("Téléchargement réussi:", filename), 
        type = "message", 
        duration = 4
      )
      
    }, error = function(e) {
      showNotification(
        paste("Erreur lors du téléchargement:", e$message), 
        type = "error", 
        duration = 8
      )
    })
    
    shinyjs::enable("downloadPlotBtn")
    
  })
  
  output$interactivePlot <- renderPlotly({
    viz_type <- input$vizType %||% "scatter"
    
    if (viz_type %in% c("pie", "donut")) {
      req(values$filteredData, input$vizXVar, input$vizYVar)
      raw <- values$filteredData
      xv  <- input$vizXVar
      yv  <- input$vizYVar[1]
      req(xv %in% names(raw), yv %in% names(raw))
      df <- raw %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(xv))) %>%
        dplyr::summarise(total = sum(as.numeric(.data[[yv]]), na.rm = TRUE),
                         .groups = "drop") %>%
        dplyr::filter(total > 0)
      title_txt <- if (!is.null(input$plotTitle) && nchar(trimws(input$plotTitle)) > 0)
        input$plotTitle else paste(yv, "par", xv)
      hole_val  <- if (viz_type == "donut") 0.45 else 0
      p_ply <- plot_ly(df,
                       labels = ~.data[[xv]], values = ~total, type = "pie", hole = hole_val,
                       textinfo = "label+percent",
                       hovertemplate = "<b>%{label}</b><br>Valeur: %{value}<br>Part: %{percent}<extra></extra>") %>%
        layout(title = list(text = title_txt), showlegend = TRUE,
                       margin = list(t = 60, b = 40)) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE)
      values$currentInteractivePlot <- p_ply
      return(p_ply)
    }
    
    if (viz_type == "treemap") {
      req(values$filteredData, input$vizXVar, input$vizYVar)
      raw <- values$filteredData
      xv  <- input$vizXVar
      yv  <- input$vizYVar[1]
      req(xv %in% names(raw), yv %in% names(raw))
      df <- raw %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(xv))) %>%
        dplyr::summarise(total = sum(as.numeric(.data[[yv]]), na.rm = TRUE),
                         .groups = "drop") %>%
        dplyr::filter(total > 0)
      title_txt <- if (!is.null(input$plotTitle) && nchar(trimws(input$plotTitle)) > 0)
        input$plotTitle else paste(yv, "par", xv)
      p_ply <- plot_ly(type = "treemap",
                       labels = df[[xv]], values = df$total,
                       parents = rep("", nrow(df)),
                       texttemplate  = "<b>%{label}</b><br>%{value}",
                       hovertemplate = "<b>%{label}</b><br>Valeur: %{value}<extra></extra>") %>%
        layout(title = list(text = title_txt),
                       margin = list(t = 60, b = 40)) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE)
      values$currentInteractivePlot <- p_ply
      return(p_ply)
    }
    
    req(createPlot())
    p <- createPlot()
    
    make_font_family <- function(bold, italic) {
      if (isTRUE(bold) && isTRUE(italic)) "Arial Bold Italic, Helvetica Bold Italic, sans-serif"
      else if (isTRUE(bold))   "Arial Bold, Helvetica Bold, sans-serif"
      else if (isTRUE(italic)) "Arial Italic, Helvetica Oblique, sans-serif"
      else                     "Arial, Helvetica, sans-serif"
    }
    
    x_tick_size <- input$xTickSize %||% 10
    y_tick_size <- input$yTickSize %||% 10
    x_tick_bold <- isTRUE(input$xTickBold);  x_tick_italic <- isTRUE(input$xTickItalic)
    y_tick_bold <- isTRUE(input$yTickBold);  y_tick_italic <- isTRUE(input$yTickItalic)
    x_axis_bold <- isTRUE(input$xAxisBold);  x_axis_italic <- isTRUE(input$xAxisItalic)
    y_axis_bold <- isTRUE(input$yAxisBold);  y_axis_italic <- isTRUE(input$yAxisItalic)
    
    show_values <- isTRUE(input$showValues)
    val_pos_key <- input$valueLabelPosition %||% "above"
    val_bold    <- isTRUE(input$valueLabelBold)
    val_italic  <- isTRUE(input$valueLabelItalic)
    val_color   <- input$valueLabelColor %||% "#333333"
    val_size_px <- round((input$valueLabelSize %||% 3) * 4)
    val_pos <- switch(val_pos_key,
                      "above"  = "top center", "below"  = "bottom center",
                      "center" = "middle center", "right"  = "middle right",
                      "left"   = "middle left", "top center")
    val_family <- make_font_family(val_bold, val_italic)
    
    plotly_obj <- tryCatch(
      suppressWarnings(suppressMessages(ggplotly(p, tooltip = "all"))),
      error = function(e) {
        showNotification(paste("Conversion interactive:", conditionMessage(e)),
                         type = "warning", duration = 4)
        NULL
      }
    )
    if (is.null(plotly_obj)) return(NULL)
    
    # ggplotly convertit geom_boxplot en traces plotly natives SANS reporter le
    # "dodge" de ggplot : boites superposees quand il y a PLUSIEURS boites par
    # categorie x. boxmode = "group" corrige ce cas, mais NE DOIT PAS etre
    # applique quand chaque trace n'a qu'une categorie (fill = X ou couleur =
    # X) : plotly reserverait alors un "slot" par trace et decalerait les
    # boites hors de l'axe (points de jitter desaxes). On ne l'applique donc
    # que si la variable de couleur est distincte de X (ou multi-Y en long).
    grouped_box <- identical(viz_type, "box") && (
      isTRUE(values$multipleY) ||
      (!is.null(input$vizColorVar) && input$vizColorVar != "Aucun" &&
         !identical(input$vizColorVar, input$vizXVar))
    )
    if (grouped_box) {
      plotly_obj <- suppressWarnings(plotly::layout(plotly_obj, boxmode = "group"))
    }
    
    if (show_values) {
      for (i in seq_along(plotly_obj$x$data)) {
        tr <- plotly_obj$x$data[[i]]
        if (!is.null(tr$mode) && grepl("text", tr$mode, fixed = TRUE)) {
          plotly_obj$x$data[[i]]$textposition <- val_pos
          plotly_obj$x$data[[i]]$textfont <- list(
            family = val_family, size = val_size_px, color = val_color)
        }
      }
    }
    
    y2_active <- if (isTRUE(values$dualAxisActive) &&
                     !is.null(values$y2VarsActive) &&
                     length(values$y2VarsActive) > 0)
      values$y2VarsActive else character(0)
    
    if (length(y2_active) > 0) {
      x_var_ply <- input$vizXVar
      y2_label  <- if (!is.null(input$y2AxisLabel) && nchar(trimws(input$y2AxisLabel)) > 0)
        input$y2AxisLabel else paste(y2_active, collapse = " / ")
      
      # Plage Y2 : preferer values$y2RangeForAxis (deja calcule dans createPlot)
      rng_axis <- values$y2RangeForAxis
      user_y2_min <- if (!is.null(input$y2AxisMin) && !is.na(input$y2AxisMin)) input$y2AxisMin else NULL
      user_y2_max <- if (!is.null(input$y2AxisMax) && !is.na(input$y2AxisMax)) input$y2AxisMax else NULL
      
      if (!is.null(rng_axis)) {
        real_mn <- if (!is.null(user_y2_min)) user_y2_min else rng_axis$min
        real_mx <- if (!is.null(user_y2_max)) user_y2_max else rng_axis$max
      } else {
        real_mn <- if (!is.null(user_y2_min)) user_y2_min else 0
        real_mx <- if (!is.null(user_y2_max)) user_y2_max else 1
      }
      y2_margin <- (real_mx - real_mn) * 0.05
      if (!is.finite(y2_margin) || y2_margin == 0) y2_margin <- 1
      y2_rng_cfg <- list(real_mn - y2_margin, real_mx + y2_margin)
      
      legend_labels_map <- values$legendLabels[["multiY_legend"]]
      get_legend_label <- function(yv) {
        if (!is.null(legend_labels_map) && yv %in% names(legend_labels_map))
          as.character(legend_labels_map[[yv]]) else yv
      }
      
      # Map plotly traces vers axe y2 quand leur nom correspond a une var Y2
      strip_pattern <- "<[^>]*>.*|[[:space:]]*\\(.*$"
      for (i in seq_along(plotly_obj$x$data)) {
        tr_raw   <- plotly_obj$x$data[[i]]$name %||% ""
        tr_clean <- trimws(gsub(strip_pattern, "", tr_raw))
        is_y2_trace <- FALSE
        for (yv in y2_active) {
          lbl <- get_legend_label(yv)
          if (tr_clean == yv || tr_raw == yv ||
              tr_clean == lbl || tr_raw == lbl ||
              grepl(yv, tr_raw, fixed = TRUE) ||
              grepl(lbl, tr_raw, fixed = TRUE)) {
            is_y2_trace <- TRUE; break
          }
        }
        if (is_y2_trace) {
          # Inverser le scaling sf/off pour retrouver les valeurs originales Y2
          if (!is.null(rng_axis)) {
            sf2 <- rng_axis$sf; off2 <- rng_axis$off
            ys <- plotly_obj$x$data[[i]]$y
            if (!is.null(ys)) {
              orig_y <- (as.numeric(ys) - off2) / sf2
              plotly_obj$x$data[[i]]$y <- orig_y
            }
            if (!is.null(plotly_obj$x$data[[i]]$error_y) &&
                !is.null(plotly_obj$x$data[[i]]$error_y$array)) {
              plotly_obj$x$data[[i]]$error_y$array <-
                as.numeric(plotly_obj$x$data[[i]]$error_y$array) / sf2
            }
          }
          plotly_obj$x$data[[i]]$yaxis <- "y2"
        }
      }
      
      y2_cfg <- list(
        title = list(
          text = y2_label,
          font = list(
            family = make_font_family(isTRUE(input$yAxisBold), isTRUE(input$yAxisItalic)),
            size   = input$axisLabelSize %||% 12,
            color  = "black")),
        linewidth      = max(1, (input$axisLineSize %||% 0.8) * 2),
        overlaying     = "y", side = "right",
        showgrid       = FALSE, zeroline = FALSE,
        range          = y2_rng_cfg,
        tickfont       = list(
          size = input$yTickSize %||% 12,
          family = make_font_family(isTRUE(input$yTickBold), isTRUE(input$yTickItalic)),
          color  = "black"),
        showline = TRUE, linecolor = "black", ticks = "outside",
        ticklen = 5, tickwidth = 1, tickcolor = "black",
        showticklabels = TRUE, mirror = FALSE, layer = "above traces"
      )
      step_val <- input$y2AxisBreakStep %||% input$y2AxisStep
      if (!is.null(step_val) && !is.na(step_val) && is.numeric(step_val) && step_val > 0) {
        ticks <- seq(real_mn, real_mx, by = step_val)
        y2_cfg$tickvals <- ticks
        y2_cfg$ticktext <- as.character(round(ticks, 8))
      }
      
      pm_r <- max(60, (input$plotMarginRight %||% 30) + 40)
      pm_l <- max(20,  input$plotMarginLeft   %||% 10)
      pm_t <- max(20,  input$plotMarginTop    %||% 10)
      pm_b <- max(40,  input$plotMarginBottom %||% 10)
      layout_args <- list(
        hovermode = "closest", dragmode = "zoom",
        margin = list(r = pm_r, l = pm_l, t = pm_t, b = pm_b, pad = 4),
        xaxis  = list(tickfont  = list(size = x_tick_size, family = make_font_family(x_tick_bold, x_tick_italic)),
                      titlefont = list(family = make_font_family(x_axis_bold, x_axis_italic))),
        yaxis  = list(tickfont  = list(size = y_tick_size, family = make_font_family(y_tick_bold, y_tick_italic)),
                      titlefont = list(family = make_font_family(y_axis_bold, y_axis_italic))),
        yaxis2 = y2_cfg
      )
    } else {
      pm_r <- max(20, input$plotMarginRight  %||% 30)
      pm_l <- max(20, input$plotMarginLeft   %||% 10)
      pm_t <- max(20, input$plotMarginTop    %||% 10)
      pm_b <- max(40, input$plotMarginBottom %||% 10)
      layout_args <- list(
        hovermode = "closest", dragmode = "zoom",
        margin = list(r = pm_r, l = pm_l, t = pm_t, b = pm_b, pad = 4),
        xaxis = list(tickfont  = list(size = x_tick_size, family = make_font_family(x_tick_bold, x_tick_italic)),
                     titlefont = list(family = make_font_family(x_axis_bold, x_axis_italic))),
        yaxis = list(tickfont  = list(size = y_tick_size, family = make_font_family(y_tick_bold, y_tick_italic)),
                     titlefont = list(family = make_font_family(y_axis_bold, y_axis_italic)))
      )
    }
    
    plotly_obj <- do.call(layout, c(list(plotly_obj), layout_args)) %>%
      config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = c("lasso2d", "select2d"),
                     displaylogo = FALSE)
    values$currentInteractivePlot <- plotly_obj
    return(plotly_obj)
  })
  })
}
