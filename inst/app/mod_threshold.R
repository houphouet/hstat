#  Module Shiny : Seuils d'efficacite


mod_threshold_ui <- function(id) {
  ns <- NS(id)
  tagList(
              fluidRow(
                div(class = "callout callout-info", style = "margin-bottom:14px;",
                    icon("info-circle"), 
                    strong(" Mode mise à jour automatique : "),
                    "les modifications sont appliquées instantanément au graphique.")
              ),
              
              fluidRow(
                box(title = tagList(icon("sliders"), " Configuration de l'analyse"), 
                    status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,
                    tabsetPanel(
                      id = ns("thresholdConfigTabs"),
                      tabPanel(tagList(icon("database"), " Données & seuil"),
                        div(style = "padding-top:14px;",
                    
                    div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        h5(icon("database"), " Sélection des variables", 
                           style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                    ),
                    
                    # La source du graphique : le fichier charge, ou le tableau
                    # d'efficacites calcule dans l'onglet « Calcul depuis un temoin ».
                    # Sans ce choix, tracer les efficacites obligeait a REMPLACER le
                    # jeu de travail puis a re-selectionner X et Y -- un detour qui
                    # fait perdre le fichier d'origine pour un simple graphique.
                    radioButtons(ns("thresholdSource"),
                      tagList(icon("database"), " Source des données"),
                      choiceNames = list(
                        HTML("<b>Jeu de données chargé</b>"),
                        HTML("<b>Efficacités calculées</b> <small style='color:#7f8c8d;'>(onglet « Calcul depuis un témoin »)</small>")),
                      choiceValues = list("donnees", "calcul"),
                      selected = "donnees"),
                    uiOutput(ns("thresholdSourceNote")),

                    uiOutput(ns("thresholdXVarSelect")),
                    
                    h6(icon("chart-line"), " Variables Y (Efficacité)", 
                       style = "font-weight: bold; color: #3c8dbc; margin-top: 15px;"),
                    checkboxInput(ns("thresholdMultipleY"), 
                                  tagList(icon("layer-group"), " Activer la sélection multiple de Y"), 
                                  value = FALSE),
                    uiOutput(ns("thresholdYVarSelect")),
                    
                    conditionalPanel(
              ns = ns,
                      condition = "input.thresholdMultipleY && input.thresholdYVar && input.thresholdYVar.length > 1",
                      div(style = "background-color: #e3f2fd; padding: 12px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #2196F3;",
                          icon("palette", style = "color: #2196F3;"),
                          strong(" Info : "), 
                          "Les couleurs des variables Y multiples utilisent automatiquement la palette ggplot2 par défaut pour une meilleure distinction visuelle."
                      )
                    ),
                    
                    hr(style = "border-top: 2px solid #3c8dbc; margin: 20px 0;"),
                    
                    div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        h5(icon("bullseye"), " Paramètres du seuil", 
                           style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                    ),
                    
                    numericInput(ns("thresholdValue"), 
                                 tagList(icon("percent"), " Valeur du seuil (%)"), 
                                 value = 80, min = 0, max = 100, step = 1),
                    
                    fluidRow(
                      column(6,
                             colourInput(ns("thresholdColor"), "Couleur de la ligne:", 
                                         value = "#e74c3c", showColour = "background")
                      ),
                      column(6,
                             numericInput(ns("thresholdLineWidth"), "Épaisseur:", 
                                          value = 1.5, min = 0.5, max = 5, step = 0.5)
                      )
                    ),
                    
                    selectInput(ns("thresholdLineType"), "Type de ligne:",
                                choices = c("Solide" = "solid",
                                            "Pointillé" = "dotted",
                                            "Tirets" = "dashed",
                                            "Tirets-points" = "dotdash",
                                            "Tirets longs" = "longdash",
                                            "Deux tirets" = "twodash"),
                                selected = "solid"),
                    
                    hr(style = "border-top: 2px solid #f39c12; margin: 20px 0;"),
                    
                    div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        h5(icon("filter"), " Filtrage des données", 
                           style = "color: #d35400; font-weight: bold; margin: 0;")
                    ),
                    
                    uiOutput(ns("thresholdFilterSelect")),
                    
                    hr(style = "border-top: 2px solid #27ae60; margin: 20px 0;"),
                    
                    div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        h5(icon("tag"), " Personnalisation des labels X", 
                           style = "color: #16a085; font-weight: bold; margin: 0;")
                    ),
                    
                    div(style = "background-color: #fff9e6; padding: 10px; border-radius: 6px; margin-bottom: 10px; border-left: 4px solid #f39c12;",
                        icon("lightbulb", style = "color: #f39c12;"),
                        em(" Astuce : Modifiez les étiquettes des traitements et appliquez des styles (gras/italique) pour une meilleure présentation.")
                    ),
                    
                    uiOutput(ns("thresholdLevelsEditor")),
                    
                    conditionalPanel(
              ns = ns,
                      condition = "input.thresholdMultipleY && input.thresholdYVar && input.thresholdYVar.length > 1",
                      hr(style = "border-top: 2px solid #9b59b6; margin: 20px 0;"),
                      
                      div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                          h5(icon("list-ul"), " Personnalisation des labels de légende", 
                             style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                      ),
                      
                      div(style = "background-color: #f3e5f5; padding: 10px; border-radius: 6px; margin-bottom: 10px; border-left: 4px solid #9b59b6;",
                          icon("info-circle", style = "color: #9b59b6;"),
                          em(" Info : Personnalisez les étiquettes affichées dans la légende pour les variables Y sélectionnées.")
                      ),
                      
                      uiOutput(ns("thresholdLegendEditor"))
                    ),
                        )
                      ),
                      tabPanel(tagList(icon("palette"), " Apparence & options"),
                        div(style = "padding-top:14px;",
                    div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        h5(icon("palette"), " Options graphiques avancées", 
                           style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                    ),
                    
                    div(style = "background-color: #f9f9f9; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #e0e0e0;",
                        h6(icon("heading"), " Titres et étiquettes", 
                           style = "font-weight: bold; color: #34495e; margin-bottom: 10px;"),
                        textInput(ns("thresholdPlotTitle"), "Titre du graphique:", 
                                  value = "Analyse des seuils d'efficacité"),
                        textInput(ns("thresholdXLabel"), "Label axe X:", 
                                  value = "", placeholder = "Par défaut: Traitements"),
                        textInput(ns("thresholdYLabel"), "Label axe Y:", 
                                  value = "", placeholder = "Par défaut: Seuil d'efficacité (%)")
                    ),
                    
                    div(style = "background-color: #fff8e1; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #ffd54f;",
                        h6(icon("font"), " Style des labels d'axes", 
                           style = "font-weight: bold; color: #f57f17; margin-bottom: 10px;"),
                        
                        div(style = "margin-bottom: 10px;",
                            strong("Label axe X:"),
                            div(style = "margin-left: 15px; margin-top: 5px; display: flex; gap: 15px;",
                                checkboxInput(ns("thresholdXLabelBold"), "Gras", value = FALSE),
                                checkboxInput(ns("thresholdXLabelItalic"), "Italique", value = FALSE)
                            )
                        ),
                        
                        div(
                          strong("Label axe Y:"),
                          div(style = "margin-left: 15px; margin-top: 5px; display: flex; gap: 15px;",
                              checkboxInput(ns("thresholdYLabelBold"), "Gras", value = FALSE),
                              checkboxInput(ns("thresholdYLabelItalic"), "Italique", value = FALSE)
                          )
                        )
                    ),
                    
                    conditionalPanel(
              ns = ns,
                      condition = "!input.thresholdMultipleY || (input.thresholdYVar && input.thresholdYVar.length == 1)",
                      div(style = "background-color: #e3f2fd; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #90caf9;",
                          h6(icon("paint-brush"), " Couleurs des barres", 
                             style = "font-weight: bold; color: #495057; margin-bottom: 10px;"),
                          
                          checkboxInput(ns("thresholdUseColor"), 
                                        tagList(icon("palette"), " Personnaliser les couleurs"), 
                                        value = TRUE),
                          
                          conditionalPanel(
              ns = ns,
                            condition = "input.thresholdUseColor",
                            radioButtons(ns("thresholdBarColor"), "Type de coloration:",
                                         choices = c("ggplot2 (défaut)" = "ggplot",
                                                     "Palette prédéfinie" = "palette",
                                                     "Personnalisé par traitement" = "custom",
                                                     "Couleur unique" = "single",
                                                     "Noir (monochrome)" = "black"),
                                         selected = "ggplot"),
                            
                            conditionalPanel(
              ns = ns,
                              condition = "input.thresholdBarColor == 'palette'",
                              selectInput(ns("thresholdPalette"), "Choisir une palette:",
                                          choices = list(
                                            "Palettes qualitatives" = c("Set1" = "Set1", "Set2" = "Set2", "Set3" = "Set3",
                                                                        "Pastel1" = "Pastel1", "Pastel2" = "Pastel2",
                                                                        "Paired" = "Paired", "Dark2" = "Dark2", "Accent" = "Accent"),
                                            "Palettes divergentes" = c("Spectral" = "Spectral", "RdYlBu" = "RdYlBu", "RdBu" = "RdBu"),
                                            "Palettes séquentielles" = c("Blues" = "Blues", "Greens" = "Greens", 
                                                                         "Oranges" = "Oranges", "Purples" = "Purples")
                                          ),
                                          selected = "Set1")
                            ),
                            
                            conditionalPanel(
              ns = ns,
                              condition = "input.thresholdBarColor == 'custom'",
                              div(style = "max-height: 300px; overflow-y: auto; padding: 5px;",
                                  uiOutput(ns("thresholdColorPickers"))
                              )
                            ),
                            
                            conditionalPanel(
              ns = ns,
                              condition = "input.thresholdBarColor == 'single'",
                              colourInput(ns("thresholdSingleBarColor"), "Couleur des barres:", 
                                          value = "#3498db", showColour = "background")
                            )
                          )
                      )
                    ),
                    
                    div(style = "background-color: #f0f8ff; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #b3d9ff;",
                        h6(icon("arrows-alt-h"), " Dimensions et espacement des barres", 
                           style = "font-weight: bold; color: #1e3a8a; margin-bottom: 10px;"),
                        
                        sliderInput(ns("thresholdBarWidth"), "Largeur des barres:", 
                                    min = 0.1, max = 1, value = 0.8, step = 0.05),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.thresholdMultipleY && input.thresholdYVar && input.thresholdYVar.length > 1",
                          
                          sliderInput(ns("thresholdBarSpacing"), 
                                      tagList(icon("arrows-alt-h"), " Espacement entre barres:"), 
                                      min = 0, max = 0.5, value = 0.1, step = 0.05),
                          
                          div(style = "background-color: #e8f5e9; padding: 8px; border-radius: 4px; margin-top: 10px; border-left: 3px solid #4caf50;",
                              icon("info-circle", style = "color: #388e3c;"),
                              tags$small(" Plus l'espacement est élevé, plus les groupes de barres sont espacés.")
                          ),
                          
                          radioButtons(ns("thresholdBarPosition"), "Position des barres:",
                                       choices = c("Côte à côte" = "dodge",
                                                   "Empilées" = "stack"),
                                       selected = "dodge", inline = TRUE)
                        )
                    ),
                    
                    div(style = "background-color: #fff3e0; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #ffb74d;",
                        h6(icon("list"), " Configuration de la légende", 
                           style = "font-weight: bold; color: #e65100; margin-bottom: 10px;"),
                        
                        checkboxInput(ns("thresholdShowLegend"), 
                                      tagList(icon("eye"), " Afficher la légende"), 
                                      value = TRUE),
                        
                        conditionalPanel(
              ns = ns,
                          condition = "input.thresholdShowLegend",
                          textInput(ns("thresholdLegendTitle"), "Titre de la légende:", 
                                    value = "", placeholder = "Laisser vide pour défaut"),
                          
                          selectInput(ns("thresholdLegendPosition"), "Position:",
                                      choices = c("En bas" = "bottom",
                                                  "En haut" = "top",
                                                  "À gauche" = "left",
                                                  "À droite" = "right",
                                                  "Coin supérieur droit" = "top_right",
                                                  "Coin supérieur gauche" = "top_left",
                                                  "Coin inférieur droit" = "bottom_right",
                                                  "Coin inférieur gauche" = "bottom_left"),
                                      selected = "right"),
                          
                          div(style = "display: flex; gap: 15px; margin-top: 5px;",
                              checkboxInput(ns("thresholdLegendBold"), "Titre en gras", value = TRUE),
                              checkboxInput(ns("thresholdLegendItalic"), "Titre en italique", value = FALSE)
                          )
                        )
                    ),
                    
                    div(style = "background-color: #f5f5f5; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #cccccc;",
                        h6(icon("ruler"), " Apparence des axes", 
                           style = "font-weight: bold; color: #424242; margin-bottom: 10px;"),
                        
                        checkboxInput(ns("thresholdBlackAxes"), 
                                      tagList(icon("paint-roller"), " Axes en noir (sinon gris)"), 
                                      value = TRUE),
                        checkboxInput(ns("thresholdShowAxisLines"), 
                                      tagList(icon("minus"), " Afficher les lignes d'axes"), 
                                      value = TRUE),
                        checkboxInput(ns("thresholdShowTicks"), 
                                      tagList(icon("grip-lines"), " Afficher les graduations"), 
                                      value = TRUE),
                        checkboxInput(ns("thresholdShowGrid"), 
                                      tagList(icon("th"), " Afficher la grille"), 
                                      value = TRUE),
                        sliderInput(ns("thresholdLabelAngle"),
                                    tagList(icon("undo"), " Inclinaison des labels X (°)"),
                                    min = 0, max = 90, value = 45, step = 5)
                    ),
                    
                    div(style = "background-color: #fce4ec; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #f48fb1;",
                        h6(icon("text-height"), " Tailles de texte", 
                           style = "font-weight: bold; color: #c2185b; margin-bottom: 10px;"),
                        
                        sliderInput(ns("thresholdTitleSize"), "Titre:", 
                                    min = 8, max = 28, value = 16, step = 1),
                        sliderInput(ns("thresholdAxisTitleSize"), "Titres des axes:", 
                                    min = 8, max = 24, value = 14, step = 1),
                        sliderInput(ns("thresholdAxisTextSize"), "Texte des axes:", 
                                    min = 6, max = 20, value = 12, step = 1),
                        sliderInput(ns("thresholdLegendSize"), "Légende:", 
                                    min = 6, max = 20, value = 10, step = 1)
                    ),
                    
                    div(style = "background-color: #e8f5e9; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #81c784;",
                        h6(icon("arrows-alt-v"), " Limites de l'axe Y", 
                           style = "font-weight: bold; color: #2e7d32; margin-bottom: 10px;"),
                        
                        fluidRow(
                          column(6,
                                 numericInput(ns("thresholdYMin"), "Minimum:", 
                                              value = 0, min = 0, max = 100)
                          ),
                          column(6,
                                 numericInput(ns("thresholdYMax"), "Maximum:", 
                                              value = 100, min = 0, max = 200)
                          )
                        )
                    )
                        )
                      ),

                      # ---- Calcul de l'efficacite depuis un temoin ----
                      tabPanel(tagList(icon("calculator"), " Calcul depuis un témoin"),
                        div(style = "padding-top:14px;",

                          div(style = "background-color:#e8f5e9;padding:12px 14px;border-radius:6px;border-left:4px solid #27ae60;font-size:13px;",
                              icon("flask", style = "color:#27ae60;"),
                              strong(" Formule d'Abbott. "),
                              "L'efficacité de chaque modalité est calculée par rapport au témoin :",
                              tags$div(style = "text-align:center;margin:8px 0;font-family:monospace;font-size:14px;",
                                       "efficacité (%) = (témoin − traitement) × 100 / témoin"),
                              "Le témoin ne se compare pas à lui-même : son efficacité vaut ",
                              tags$b("0"), " par définition."),
                          br(),

                          uiOutput(ns("effFactorSelect")),
                          uiOutput(ns("effControlSelect")),
                          uiOutput(ns("effOtherLevels")),
                          uiOutput(ns("effResponseSelect")),

                          fluidRow(
                            column(6,
                              selectInput(ns("effAgg"), "Valeur résumée par modalité",
                                          choices = HSTAT_EFF_AGG, selected = "moyenne")),
                            column(6, uiOutput(ns("effGroupSelect")))),

                          radioButtons(ns("effMode"),
                            tagList(icon("layer-group"), " Traitement des répétitions"),
                            choiceNames = list(
                              HTML("<b>Mettre les répétitions en commun</b> <small style='color:#7f8c8d;'>(une efficacité par modalité — le chiffre que l'on publie)</small>"),
                              HTML("<b>Une efficacité par répétition</b> <small style='color:#7f8c8d;'>(autant de valeurs que de répétitions — analysable par ANOVA)</small>")),
                            choiceValues = list("cumul", "par_repetition"),
                            selected = "cumul"),

                          div(style = "background-color:#fff9e6;padding:10px 12px;border-radius:6px;border-left:4px solid #f39c12;font-size:12px;",
                              icon("lightbulb", style = "color:#f39c12;"),
                              em(" Les deux modes ne répondent pas à la même question. ",
                                 "En commun, la moyenne (ou la somme) de la modalité porte sur ",
                                 "toutes ses répétitions : c'est le chiffre du rapport. ",
                                 "Par répétition, on obtient une variable analysable ensuite par ",
                                 "ANOVA ou comparaisons multiples.")),

                          div(style = "background-color:#fdedec;padding:10px 12px;border-radius:6px;border-left:4px solid #c0392b;font-size:12px;",
                              icon("triangle-exclamation", style = "color:#c0392b;"),
                              em(" La ", tags$b("somme"), " n'est comparable que si les répétitions ",
                                 "sont en nombre égal : la modalité la plus répétée accumule ",
                                 "mécaniquement davantage et ressort artificiellement moins efficace. ",
                                 "La moyenne n'en souffre pas. L'application le signale si le cas se ",
                                 "présente.")),
                          br(),

                          actionButton(ns("effCompute"),
                                       tagList(icon("calculator"), " Calculer les efficacités"),
                                       class = "btn-success btn-block"),
                          br(),
                          uiOutput(ns("effMessage"))
                        )
                      )
                    )
                ),
                
                box(title = tagList(icon("chart-bar"), " Graphique des seuils d'efficacité"), 
                    status = "primary", width = 8, solidHeader = TRUE, collapsible = TRUE,
                    
                    plotlyOutput(ns("thresholdPlot"), height = "600px"),
                    
                    br(),
                    hr(style = "border-top: 2px solid #3c8dbc;"),
                    
                    div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        h4(icon("download"), " Options d'exportation haute qualité", 
                           style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                    ),
                    
                    div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
                        h6(icon("cogs"), " Paramètres personnalisés", 
                           style = "font-weight: bold; color: #424242; margin-bottom: 10px;"),
                        
                        fluidRow(
                          column(4,
                                 numericInput(ns("thresholdExportWidth"), 
                                              tagList(icon("arrows-alt-h"), " Largeur (pixels)"), 
                                              value = 1200, min = 400, max = 20000, step = 100)
                          ),
                          column(4,
                                 numericInput(ns("thresholdExportHeight"), 
                                              tagList(icon("arrows-alt-v"), " Hauteur (pixels)"), 
                                              value = 800, min = 400, max = 20000, step = 100)
                          ),
                          column(4,
                                 numericInput(ns("thresholdExportDPI"), 
                                              tagList(icon("crosshairs"), " Résolution (DPI)"), 
                                              value = 300, min = 72, max = 20000, step = 50)
                          )
                        ),
                        
                        div(style = "background-color: #e1f5fe; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #0288d1;",
                            icon("info-circle", style = "color: #01579b;"),
                            strong(" Aperçu : "),
                            textOutput(ns("exportSizeEstimate"), inline = TRUE)
                        )
                    ),
                    
                    fluidRow(
                      column(12,
                             selectInput(ns("thresholdExportFormat"), 
                                         tagList(icon("file-image"), " Format d'export"),
                                         choices = list(
                                           "Formats raster (pixels)" = c("PNG (recommandé)" = "png",
                                                                         "JPEG (compressé)" = "jpeg",
                                                                         "TIFF (haute qualité)" = "tiff",
                                                                         "BMP (non compressé)" = "bmp"),
                                           "Formats vectoriels (résolution infinie)" = c("SVG (web, idéal)" = "svg",
                                                                                         "PDF (publication)" = "pdf",
                                                                                         "EPS (impression pro)" = "eps")
                                         ),
                                         selected = "png")
                      )
                    ),
                    
                    downloadButton(ns("downloadThresholdPlot"), 
                                   tagList(icon("download"), " Télécharger le graphique"), 
                                   class = "btn-success btn-lg btn-block", 
                                   style = "font-weight:600;")
                )
              ),
              
              fluidRow(
                box(title = tagList(icon("calculator"), " Efficacités calculées depuis le témoin"),
                    status = "success", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,

                    uiOutput(ns("effResume")),
                    # Deux lectures du meme calcul. « Une colonne par variable »
                    # est le defaut : c'est elle qui alimente le selecteur
                    # « Variable Y » et qui se reinjecte dans l'application.
                    radioButtons(ns("effPresentation"),
                                 tagList(icon("table-columns"), " Présentation du tableau"),
                                 choiceNames = list(
                                   HTML("<b>Une colonne par variable mesurée</b> <small style='color:#7f8c8d;'>(une ligne par modalité)</small>"),
                                   HTML("<b>Détail</b> <small style='color:#7f8c8d;'>(une ligne par variable : effectif, témoin, valeur résumée)</small>")),
                                 choiceValues = list("large", "long"),
                                 selected = "large", inline = TRUE),
                    DTOutput(ns("effTable")),
                    br(),
                    fluidRow(
                      column(4, downloadButton(ns("effDownloadCsv"),
                                               tagList(icon("file-csv"), " Télécharger (CSV)"),
                                               class = "btn-info btn-block")),
                      column(4, downloadButton(ns("effDownloadXlsx"),
                                               tagList(icon("file-excel"), " Télécharger (Excel)"),
                                               class = "btn-success btn-block")),
                      column(4, actionButton(ns("effUseAsData"),
                                             tagList(icon("right-left"), " Utiliser comme jeu de données"),
                                             class = "btn-warning btn-block"))),
                    br(),
                    div(style = "background-color:#fdedec;padding:10px 12px;border-radius:6px;border-left:4px solid #c0392b;font-size:12px;",
                        icon("triangle-exclamation", style = "color:#c0392b;"),
                        em(" « Utiliser comme jeu de données » REMPLACE le jeu de travail par ce ",
                           "tableau, pour l'analyser dans les autres onglets. Le fichier d'origine ",
                           "n'est pas modifié : rechargez-le pour revenir en arrière."))
                )
              ),

              fluidRow(
                box(title = tagList(icon("table"), " Tableau des données utilisées"), 
                    status = "info", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    
                    div(style = "background-color: #fff9e6; padding: 12px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #ffa726;",
                        icon("info-circle", style = "color: #f57c00;"),
                        strong(" Information : "),
                        "Ce tableau affiche les données filtrées et transformées utilisées pour générer le graphique. ",
                        "Vous pouvez copier, exporter en CSV ou Excel directement depuis le tableau."
                    ),
                    
                    DTOutput(ns("thresholdDataTable")),
                    
                    br(),
                    
                    downloadButton(ns("downloadThresholdData"), 
                                   tagList(icon("file-excel"), " Télécharger données complètes (Excel)"), 
                                   class = "btn-info btn-lg",
                                   style = "font-size: 16px; font-weight: bold; padding: 12px 24px;")
                )
              )
  )
}

mod_threshold_server <- function(id, values) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  # ---- Seuils d'efficacité ----
  
  threshold_values <- reactiveValues(
    plot_data = NULL,
    current_plot = NULL,
    label_mapping = NULL,
    label_styles = NULL,
    data_prepared = FALSE,
    selected_y_vars = NULL,
    y_colors = NULL,
    auto_update = TRUE,
    legend_label_mapping = NULL,
    legend_label_styles = NULL
  )

  # Depot des donnees de seuils pour l'aide a la decision. Pose ICI, apres la
  # creation de `threshold_values` : place plus haut, l'objet n'existe pas
  # encore au moment ou l'observateur est enregistre.
  observeEvent(threshold_values$plot_data, {
    pd <- threshold_values$plot_data
    if (is.null(pd) || !NROW(pd)) return()
    hstat_ai_capture(values, "Seuils d'efficacite",
      "Courbes de seuils d'efficacite",
      tables = list("Points des courbes" = utils::head(as.data.frame(pd), 200)),
      meta = list(variables = threshold_values$selected_y_vars,
                  `points traces` = NROW(pd)),
      plot = function() shiny::isolate(threshold_values$current_plot))
  }, ignoreInit = TRUE)

  # Reinitialisation globale : quand l'utilisateur clique sur "Réinitialiser" dans
  # l'en-tete de l'application, on remet ce module (Seuils d'efficacite) a zero :
  # etat interne efface et principaux controles visuels ramenes a leurs defauts.
  observeEvent(values$resetSignal, {
    if ((values$resetSignal %||% 0) == 0) return()
    threshold_values$plot_data <- NULL
    threshold_values$current_plot <- NULL
    threshold_values$label_mapping <- NULL
    threshold_values$label_styles <- NULL
    threshold_values$data_prepared <- FALSE
    threshold_values$selected_y_vars <- NULL
    threshold_values$y_colors <- NULL
    threshold_values$legend_label_mapping <- NULL
    threshold_values$legend_label_styles <- NULL
    updateNumericInput(session, "thresholdValue", value = 80)
    updateTextInput(session, "thresholdPlotTitle", value = "")
    updateTextInput(session, "thresholdXLabel", value = "")
    updateTextInput(session, "thresholdYLabel", value = "")
    updateTextInput(session, "thresholdLegendTitle", value = "")
    updateCheckboxInput(session, "thresholdMultipleY", value = FALSE)
    tryCatch(shinyjs::reset("thresholdValue"), error = function(e) NULL)
  }, ignoreInit = TRUE)

  # =========================================================================
  # CALCUL DE L'EFFICACITE DEPUIS UN TEMOIN (formule d'Abbott)
  # -------------------------------------------------------------------------
  # Le module tracait des courbes a partir d'une variable d'efficacite DEJA
  # calculee ailleurs. Ici, elle est calculee dans l'application : on choisit
  # la modalite temoin, et toutes les autres lui sont comparees en boucle.
  # =========================================================================
  eff_res <- reactiveVal(NULL)

  output$effFactorSelect <- renderUI({
    d <- values$filteredData
    validate(need(!is.null(d) && NROW(d), "Chargez d'abord un jeu de données."))
    cand <- names(d)[vapply(d, function(x)
      is.character(x) || is.factor(x) || length(unique(x[!is.na(x)])) <= 30,
      logical(1))]
    if (!length(cand)) cand <- names(d)
    selectInput(ns("effFactor"), tagList(icon("layer-group"), " Variable des traitements"),
                choices = cand, selected = isolate(input$effFactor) %||% cand[1])
  })

  output$effControlSelect <- renderUI({
    d <- values$filteredData
    req(d, input$effFactor)
    m <- hstat_eff_modalites(d, input$effFactor)
    validate(need(length(m) >= 2,
      "Cette variable ne porte pas au moins deux modalités : le témoin ne pourrait être comparé à rien."))
    selectInput(ns("effControl"), tagList(icon("vial"), " Modalité témoin"),
                choices = m, selected = isolate(input$effControl) %||% m[1])
  })

  # « Une fois le temoin choisi, les autres modalites passent dans une
  # variable » : on la MONTRE, pour que l'utilisateur verifie d'un coup d'oeil
  # sur quoi la boucle va porter.
  output$effOtherLevels <- renderUI({
    d <- values$filteredData
    req(d, input$effFactor, input$effControl)
    autres <- hstat_eff_modalites(d, input$effFactor, input$effControl)
    if (!length(autres))
      return(div(class = "alert alert-warning", style = "padding:8px;",
                 icon("triangle-exclamation"),
                 " Aucune autre modalité à comparer au témoin."))
    div(style = "background-color:#eef7fb;padding:9px 12px;border-radius:6px;border-left:4px solid #3c8dbc;font-size:12px;margin-bottom:12px;",
        icon("arrow-right-arrow-left", style = "color:#3c8dbc;"),
        trf(" %d modalité(s) seront comparées au témoin : ", length(autres)),
        tags$b(paste(autres, collapse = ", ")))
  })

  output$effResponseSelect <- renderUI({
    d <- values$filteredData
    req(d)
    num <- names(d)[vapply(d, is.numeric, logical(1))]
    validate(need(length(num) > 0, "Aucune variable numérique à mesurer."))
    selectInput(ns("effResponse"), tagList(icon("ruler"), " Variable(s) mesurée(s)"),
                choices = num, selected = isolate(input$effResponse) %||% num[1],
                multiple = TRUE)
  })

  output$effGroupSelect <- renderUI({
    d <- values$filteredData
    req(d, input$effFactor)
    cand <- setdiff(names(d), input$effFactor)
    selectInput(ns("effGroup"), tagList(icon("object-group"), " Variable de répétition"),
                choices = c("(aucune répétition déclarée)" = "", cand),
                selected = isolate(input$effGroup) %||% "")
  })

  observeEvent(input$effCompute, {
    d <- values$filteredData
    if (is.null(d) || !NROW(d)) {
      showNotification("Chargez d'abord un jeu de données.", type = "warning")
      return()
    }
    r <- tryCatch(
      hstat_efficacite(d, input$effFactor, input$effResponse, input$effControl,
                       agg = input$effAgg %||% "moyenne",
                       var_repetition = input$effGroup,
                       mode = input$effMode %||% "cumul"),
      error = function(e) {
        showNotification(hstat_err_fr(e), type = "error", duration = 8)
        NULL
      })
    if (is.null(r)) return()
    eff_res(r)
    if (!NROW(r)) {
      showNotification(attr(r, "message") %||% "Calcul impossible.",
                       type = "warning", duration = 8)
      return()
    }
    showNotification(tagList(icon("check"), " ", attr(r, "message")),
                     type = "message", duration = 5)
    hstat_ai_capture(values, "Seuils d'efficacite",
      "Efficacites calculees depuis le temoin (formule d'Abbott)",
      tables = list("Efficacites" = utils::head(as.data.frame(r), 200)),
      text = attr(r, "message"),
      meta = list(temoin = attr(r, "temoin"), resume = attr(r, "agg"),
                  variables = paste(input$effResponse, collapse = ", ")))
  })

  output$effMessage <- renderUI({
    r <- eff_res()
    if (is.null(r))
      return(div(class = "callout callout-info", style = "padding:8px 12px;",
                 icon("circle-info"),
                 " Choisissez le témoin puis cliquez sur « Calculer ». Le tableau",
                 " apparaît en bas de page."))
    m <- attr(r, "message") %||% ""
    alerte <- grepl("^Attention", m)
    div(class = if (alerte) "callout callout-warning" else "callout callout-success",
        style = "padding:8px 12px;",
        icon(if (alerte) "triangle-exclamation" else "circle-check"), " ", m)
  })

  output$effResume <- renderUI({
    r <- eff_res()
    if (is.null(r) || !NROW(r))
      return(p(style = "color:#999;font-style:italic;",
               "Aucun calcul pour l'instant : rendez-vous dans « Calcul depuis un témoin »."))
    ind <- sum(!is.finite(r$Efficacite))
    tagList(
      p(tags$b(trf("%d ligne(s)", NROW(r))),
        trf(" — témoin « %s », valeur résumée : %s.",
            attr(r, "temoin") %||% "", attr(r, "agg") %||% "")),
      if (ind > 0)
        div(class = "alert alert-warning", style = "padding:8px;",
            icon("triangle-exclamation"),
            trf(" %d efficacité(s) non calculable(s) : le témoin y vaut zéro ou n'a aucune valeur mesurable.", ind))
      else NULL)
  })

  # Une colonne d'efficacite par variable mesuree. Sans cela, quinze variables
  # sur onze modalites faisaient 165 lignes portant toutes la meme colonne
  # « Efficacite » : le selecteur Y n'avait qu'un choix, et le graphique
  # superposait quinze series sur onze positions.
  eff_large <- reactive({
    r <- eff_res()
    if (is.null(r) || !NROW(r)) return(NULL)
    hstat_eff_large(r)
  })

  # Tableau effectivement montre et telecharge : celui que l'utilisateur a sous
  # les yeux. Deux boutons qui n'exportent pas ce qui est affiche seraient un
  # piege.
  eff_affiche <- reactive({
    if (identical(input$effPresentation %||% "large", "long")) eff_res()
    else eff_large()
  })

  output$effTable <- renderDT({
    r <- eff_affiche()
    req(r, NROW(r) > 0)
    x <- as.data.frame(r)
    num <- names(x)[vapply(x, is.numeric, logical(1))]
    for (k in num) x[[k]] <- round(x[[k]], 2)
    datatable(x, rownames = FALSE, extensions = "Buttons",
              options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel")),
              class = "cell-border stripe hover")
  })

  output$effDownloadCsv <- downloadHandler(
    filename = function() paste0("efficacites_", Sys.Date(), ".csv"),
    content = function(file) {
      r <- eff_affiche(); req(r)
      utils::write.csv(as.data.frame(r), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    })

  output$effDownloadXlsx <- downloadHandler(
    filename = function() paste0("efficacites_", Sys.Date(), ".xlsx"),
    content = function(file) {
      r <- eff_affiche(); req(r)
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        # Les deux lectures dans le meme classeur : le tableau large pour
        # travailler, le detail pour verifier d'ou vient chaque pourcentage.
        feuilles <- list(Efficacites = as.data.frame(r))
        det <- eff_res()
        if (!is.null(det) && !identical(NCOL(det), NCOL(r)))
          feuilles[["Detail"]] <- as.data.frame(det)
        openxlsx::write.xlsx(feuilles, file)
      } else
        utils::write.csv(as.data.frame(r), file, row.names = FALSE,
                         fileEncoding = "UTF-8")
    })

  # « L'utilisateur doit pouvoir selectionner ce dataframe pour les autres
  # operations » : le tableau devient le jeu de travail. On le dit clairement
  # -- remplacer les donnees sans prevenir serait le pire des services.
  observeEvent(input$effUseAsData, {
    # Le tableau LARGE : c'est celui qui porte une variable par colonne, donc
    # le seul directement analysable par les autres onglets.
    r <- eff_large()
    if (is.null(r) || !NROW(r)) {
      showNotification("Calculez d'abord les efficacités.", type = "warning")
      return()
    }
    x <- as.data.frame(r)
    attributes(x) <- attributes(x)[c("names", "class", "row.names")]
    values$data <- x
    values$cleanData <- x
    values$filteredData <- x
    showNotification(
      tagList(icon("check"),
              trf(" Jeu de données remplacé par le tableau des efficacités (%d lignes, %d colonnes). Les autres onglets travaillent maintenant dessus.",
                  NROW(x), NCOL(x))),
      type = "message", duration = 8)
  })

  # Source du graphique. Le tableau d'efficacites est utilisable SANS remplacer
  # le jeu de travail : le fichier d'origine reste disponible pour le reste de
  # l'application.
  source_data <- reactive({
    if (identical(input$thresholdSource %||% "donnees", "calcul")) {
      # Large : le selecteur « Variable Y » doit lister les variables mesurees,
      # pas une unique colonne « Efficacite » ou quinze series se superposent.
      r <- eff_large()
      validate(need(!is.null(r) && NROW(r) > 0,
        "Aucune efficacité calculée pour l'instant : passez par l'onglet « Calcul depuis un témoin »."))
      x <- as.data.frame(r)
      attributes(x) <- attributes(x)[c("names", "class", "row.names")]
      x
    } else {
      # `values$filteredData`, PAS `source_data()` : la substitution mecanique
      # avait touche le corps du reactif lui-meme, qui s'appelait donc
      # recursivement. R s'arretait sur « C stack usage is too close to the
      # limit » et l'application ne demarrait plus.
      values$filteredData
    }
  })

  output$thresholdSourceNote <- renderUI({
    if (!identical(input$thresholdSource %||% "donnees", "calcul")) return(NULL)
    r <- eff_large()
    if (is.null(r) || !NROW(r))
      return(div(class = "alert alert-warning", style = "padding:8px;",
                 icon("triangle-exclamation"),
                 " Aucune efficacité calculée : rendez-vous dans l'onglet",
                 " « Calcul depuis un témoin »."))
    vars <- attr(r, "variables")
    div(class = "alert alert-success", style = "padding:8px;",
        icon("circle-check"),
        trf(" Le graphique trace les efficacités calculées (%d modalité(s)). Le jeu de données chargé n'est pas modifié.",
            NROW(r)),
        if (length(vars) > 1)
          tagList(tags$br(),
                  trf("Une colonne par variable mesurée (%d) : choisissez celle à représenter dans « Variable Y ».",
                      length(vars)))
        else NULL)
  })

  # Quand la source est le tableau d'efficacites, ses colonnes portent des noms
  # connus : on preselectionne le couple qui a du sens (Modalite / Efficacite)
  # plutot que la premiere colonne venue.
  .eff_defaut <- function(cols, souhait) {
    if (identical(input$thresholdSource %||% "donnees", "calcul")) {
      if (souhait %in% cols) return(souhait)
      # Plusieurs variables mesurees : les colonnes s'appellent
      # « Efficacite_<variable> ». On propose la premiere plutot que la
      # premiere colonne venue, qui serait le nombre de repetitions.
      eff <- cols[startsWith(cols, HSTAT_EFF_PREFIXE)]
      if (length(eff)) return(eff[1])
    }
    if (length(cols)) cols[1] else NULL
  }

  output$thresholdXVarSelect <- renderUI({
    req(source_data())
    all_cols <- names(source_data())
    selectInput(ns("thresholdXVar"), "Variable X (Traitements):", 
                choices = all_cols,
                selected = .eff_defaut(all_cols, "Modalite"))
  })
  
  output$thresholdYVarSelect <- renderUI({
    req(source_data())
    num_cols <- names(source_data())[sapply(source_data(), is.numeric)]
    
    if(input$thresholdMultipleY) {
      pickerInput(ns("thresholdYVar"), "Variables Y (Efficacité) - Sélection multiple:", 
                  choices = num_cols,
                  selected = .eff_defaut(num_cols, "Efficacite"),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `selected-text-format` = "count > 2",
                                 `count-selected-text` = "{0} variables sélectionnées"))
    } else {
      selectInput(ns("thresholdYVar"), "Variable Y (Efficacité):", 
                  choices = num_cols,
                  selected = .eff_defaut(num_cols, "Efficacite"))
    }
  })
  
  output$thresholdFilterSelect <- renderUI({
    req(source_data(), input$thresholdXVar)
    
    if(is.null(input$thresholdXVar)) return(NULL)
    
    x_data <- source_data()[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(x_data)
    } else {
      unique(as.character(x_data))
    }
    
    pickerInput(ns("thresholdFilter"), 
                "Exclure des traitements (optionnel):",
                choices = unique_vals,
                multiple = TRUE,
                options = list(`actions-box` = TRUE))
  })
  
  # Éditeur de labels pour la variable X avec options de style
  output$thresholdLevelsEditor <- renderUI({
    req(source_data(), input$thresholdXVar)
    
    x_data <- source_data()[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(droplevels(x_data))
    } else {
      sort(unique(as.character(x_data)))
    }
    
    if(length(unique_vals) == 0) {
      return(p("Aucune valeur trouvée", style = "color: #999;"))
    }
    
    div(
      actionButton(ns("resetThresholdLabels"), "Réinitialiser", 
                   class = "btn-default btn-sm", icon = icon("undo"),
                   style = "margin-bottom: 10px;"),
      
      div(style = if(length(unique_vals) > 10) "max-height: 400px; overflow-y: auto;" else "",
          lapply(seq_along(unique_vals), function(i) {
            lvl <- unique_vals[i]
            input_id <- paste0("thresholdLevel_", make.names(lvl))
            bold_id <- paste0("thresholdLevelBold_", make.names(lvl))
            italic_id <- paste0("thresholdLevelItalic_", make.names(lvl))
            
            div(style = "margin-bottom: 10px; padding: 10px; background-color: #f5f5f5; border-radius: 4px; border-left: 4px solid #3498db;",
                div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
                    span(paste0(i, "."), style = "color: #3498db; font-weight: bold; min-width: 25px; font-size: 14px;"),
                    div(style = "flex: 1;",
                        div(style = "font-size: 11px; color: #666; margin-bottom: 3px; font-style: italic;",
                            paste("Original:", lvl)),
                        textInput(
                          inputId = ns(input_id),
                          label = NULL,
                          value = lvl,
                          placeholder = "Nouvelle étiquette...",
                          width = "100%"
                        )
                    )
                ),
                div(style = "display: flex; gap: 15px; padding-left: 35px; align-items: center;",
                    div(style = "display: flex; align-items: center; gap: 5px;",
                        checkboxInput(ns(bold_id), NULL, value = FALSE, width = "20px"),
                        tags$label(`for` = ns(bold_id), style = "margin: 0; font-weight: bold; cursor: pointer;", "Gras")
                    ),
                    div(style = "display: flex; align-items: center; gap: 5px;",
                        checkboxInput(ns(italic_id), NULL, value = FALSE, width = "20px"),
                        tags$label(`for` = ns(italic_id), style = "margin: 0; font-style: italic; cursor: pointer;", "Italique")
                    )
                )
            )
          })
      )
    )
  })
  
  # Éditeur de labels pour la légende (Variables Y multiples)
  output$thresholdLegendEditor <- renderUI({
    req(source_data(), input$thresholdXVar, input$thresholdYVar)

    multiple_y <- isTRUE(input$thresholdMultipleY) && length(input$thresholdYVar) > 1

    if (multiple_y) {
      # Mode Y multiples : chaque variable Y est une entree de legende
      legend_items <- input$thresholdYVar
      id_prefix    <- "thresholdLegendLevel_"
      bold_prefix  <- "thresholdLegendLevelBold_"
      ital_prefix  <- "thresholdLegendLevelItalic_"
      note <- "Renommez les variables affichées dans la légende."
    } else {
      # Mode Y simple : la legende = les niveaux de X (traitements colores).
      # On ne l'affiche que si les barres sont effectivement colorees.
      colored <- isTRUE(input$thresholdUseColor) &&
                 (input$thresholdBarColor %||% "") %in% c("ggplot", "palette", "custom")
      if (!colored)
        return(div(style = "font-size:12px; color:#888; font-style:italic; padding:6px;",
                   icon("info-circle"),
                   " La légende n'apparaît que lorsque les barres sont colorées par traitement (voir « Couleurs des barres »)."))
      x_data <- source_data()[[input$thresholdXVar]]
      legend_items <- if (is.factor(x_data)) levels(droplevels(x_data))
                      else sort(unique(as.character(x_data)))
      if (!is.null(input$thresholdFilter) && length(input$thresholdFilter) > 0)
        legend_items <- legend_items[!legend_items %in% input$thresholdFilter]
      id_prefix    <- "thresholdLegendItem_"
      bold_prefix  <- "thresholdLegendItemBold_"
      ital_prefix  <- "thresholdLegendItemItalic_"
      note <- "Renommez les traitements affichés dans la légende (indépendamment de l'axe X)."
    }

    div(
      div(style = "font-size:12px; color:#666; font-style:italic; margin-bottom:8px;",
          icon("info-circle"), " ", note),
      actionButton(ns("resetThresholdLegendLabels"), "Réinitialiser", 
                   class = "btn-default btn-sm", icon = icon("undo"),
                   style = "margin-bottom: 10px;"),
      
      div(style = if(length(legend_items) > 10) "max-height: 400px; overflow-y: auto;" else "",
          lapply(seq_along(legend_items), function(i) {
            var_name <- legend_items[i]
            input_id <- paste0(id_prefix, make.names(var_name))
            bold_id <- paste0(bold_prefix, make.names(var_name))
            italic_id <- paste0(ital_prefix, make.names(var_name))
            
            div(style = "margin-bottom: 10px; padding: 10px; background-color: #f5f5f5; border-radius: 4px; border-left: 4px solid #9b59b6;",
                div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
                    span(paste0(i, "."), style = "color: #9b59b6; font-weight: bold; min-width: 25px; font-size: 14px;"),
                    div(style = "flex: 1;",
                        div(style = "font-size: 11px; color: #666; margin-bottom: 3px; font-style: italic;",
                            paste("Original:", var_name)),
                        textInput(
                          inputId = ns(input_id),
                          label = NULL,
                          value = var_name,
                          placeholder = "Nouvelle étiquette...",
                          width = "100%"
                        )
                    )
                ),
                div(style = "display: flex; gap: 15px; padding-left: 35px; align-items: center;",
                    div(style = "display: flex; align-items: center; gap: 5px;",
                        checkboxInput(ns(bold_id), NULL, value = FALSE, width = "20px"),
                        tags$label(`for` = ns(bold_id), style = "margin: 0; font-weight: bold; cursor: pointer;", "Gras")
                    ),
                    div(style = "display: flex; align-items: center; gap: 5px;",
                        checkboxInput(ns(italic_id), NULL, value = FALSE, width = "20px"),
                        tags$label(`for` = ns(italic_id), style = "margin: 0; font-style: italic; cursor: pointer;", "Italique")
                    )
                )
            )
          })
      )
    )
  })
  
  observeEvent(input$resetThresholdLabels, {
    req(source_data(), input$thresholdXVar)
    
    x_data <- source_data()[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(droplevels(x_data))
    } else {
      sort(unique(as.character(x_data)))
    }
    
    for(lvl in unique_vals) {
      updateTextInput(session, paste0("thresholdLevel_", make.names(lvl)), value = lvl)
      updateCheckboxInput(session, paste0("thresholdLevelBold_", make.names(lvl)), value = FALSE)
      updateCheckboxInput(session, paste0("thresholdLevelItalic_", make.names(lvl)), value = FALSE)
    }
    
    showNotification("Étiquettes X réinitialisées", type = "message", duration = 2)
  })
  
  observeEvent(input$resetThresholdLegendLabels, {
    req(source_data(), input$thresholdXVar)
    multiple_y <- isTRUE(input$thresholdMultipleY) && length(input$thresholdYVar) > 1

    if (multiple_y) {
      for(var_name in input$thresholdYVar) {
        updateTextInput(session, paste0("thresholdLegendLevel_", make.names(var_name)), value = var_name)
        updateCheckboxInput(session, paste0("thresholdLegendLevelBold_", make.names(var_name)), value = FALSE)
        updateCheckboxInput(session, paste0("thresholdLegendLevelItalic_", make.names(var_name)), value = FALSE)
      }
    } else {
      x_data <- source_data()[[input$thresholdXVar]]
      items <- if (is.factor(x_data)) levels(droplevels(x_data))
               else sort(unique(as.character(x_data)))
      for(it in items) {
        updateTextInput(session, paste0("thresholdLegendItem_", make.names(it)), value = it)
        updateCheckboxInput(session, paste0("thresholdLegendItemBold_", make.names(it)), value = FALSE)
        updateCheckboxInput(session, paste0("thresholdLegendItemItalic_", make.names(it)), value = FALSE)
      }
    }
    
    showNotification("Étiquettes de légende réinitialisées", type = "message", duration = 2)
  })
  
  # Color pickers personnalisés pour les traitements (une seule variable Y)
  output$thresholdColorPickers <- renderUI({
    req(source_data(), input$thresholdXVar)
    req(!input$thresholdMultipleY || length(input$thresholdYVar) == 1)
    
    x_data <- source_data()[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(droplevels(x_data))
    } else {
      sort(unique(as.character(x_data)))
    }
    
    if(!is.null(input$thresholdFilter) && length(input$thresholdFilter) > 0) {
      unique_vals <- unique_vals[!unique_vals %in% input$thresholdFilter]
    }
    
    default_colors <- scales::hue_pal()(length(unique_vals))
    
    div(
      lapply(seq_along(unique_vals), function(i) {
        colourInput(ns(paste0("thresholdCustomColor_", i)), 
                    paste("Couleur", unique_vals[i], ":"),
                    value = default_colors[i],
                    showColour = "background")
      })
    )
  })
  
  observe({
    req(source_data(), input$thresholdXVar, input$thresholdYVar)
    
    tryCatch({
      if(length(input$thresholdYVar) == 1) {
        plot_data <- source_data()[, c(input$thresholdXVar, input$thresholdYVar)]
        colnames(plot_data) <- c("Treatment", "Efficacy")
        plot_data <- na.omit(plot_data)
        
        if(!is.null(input$thresholdFilter) && length(input$thresholdFilter) > 0) {
          plot_data <- plot_data[!plot_data$Treatment %in% input$thresholdFilter, ]
        }
        
      } else {
        plot_data <- source_data()[, c(input$thresholdXVar, input$thresholdYVar)]
        colnames(plot_data)[1] <- "Treatment"
        
        plot_data <- tidyr::pivot_longer(plot_data, 
                                         cols = -Treatment,
                                         names_to = "Variable",
                                         values_to = "Efficacy")
        plot_data <- na.omit(plot_data)
        
        if(!is.null(input$thresholdFilter) && length(input$thresholdFilter) > 0) {
          plot_data <- plot_data[!plot_data$Treatment %in% input$thresholdFilter, ]
        }
        
        threshold_values$y_colors <- NULL
      }
      
      if(nrow(plot_data) == 0) {
        threshold_values$data_prepared <- FALSE
        return()
      }
      
      plot_data$Treatment <- as.character(plot_data$Treatment)
      
      unique_treatments <- sort(unique(plot_data$Treatment))
      label_mapping <- sapply(unique_treatments, function(lvl) {
        new_label <- input[[paste0("thresholdLevel_", make.names(lvl))]]
        if(is.null(new_label) || new_label == "") lvl else new_label
      })
      
      label_styles <- sapply(unique_treatments, function(lvl) {
        is_bold <- input[[paste0("thresholdLevelBold_", make.names(lvl))]]
        is_italic <- input[[paste0("thresholdLevelItalic_", make.names(lvl))]]
        
        if(is.null(is_bold)) is_bold <- FALSE
        if(is.null(is_italic)) is_italic <- FALSE
        
        if(is_bold && is_italic) "bolditalic"
        else if(is_bold) "bold"
        else if(is_italic) "italic"
        else "plain"
      })
      
      if(any(duplicated(label_mapping))) {
        threshold_values$data_prepared <- FALSE
        return()
      }
      
      plot_data$Treatment <- factor(plot_data$Treatment, 
                                    levels = unique_treatments,
                                    labels = label_mapping)
      
      # Appliquer les labels de légende personnalisés pour Y multiples
      if(length(input$thresholdYVar) > 1) {
        unique_vars <- unique(plot_data$Variable)
        
        legend_label_mapping <- sapply(unique_vars, function(var_name) {
          new_label <- input[[paste0("thresholdLegendLevel_", make.names(var_name))]]
          if(is.null(new_label) || new_label == "") var_name else new_label
        })
        
        legend_label_styles <- sapply(unique_vars, function(var_name) {
          is_bold <- input[[paste0("thresholdLegendLevelBold_", make.names(var_name))]]
          is_italic <- input[[paste0("thresholdLegendLevelItalic_", make.names(var_name))]]
          
          if(is.null(is_bold)) is_bold <- FALSE
          if(is.null(is_italic)) is_italic <- FALSE
          
          if(is_bold && is_italic) "bolditalic"
          else if(is_bold) "bold"
          else if(is_italic) "italic"
          else "plain"
        })
        
        plot_data$Variable <- factor(plot_data$Variable,
                                     levels = unique_vars,
                                     labels = legend_label_mapping)
        
        threshold_values$legend_label_mapping <- legend_label_mapping
        threshold_values$legend_label_styles <- legend_label_styles
      }
      
      threshold_values$plot_data <- plot_data
      threshold_values$label_mapping <- label_mapping
      threshold_values$label_styles <- label_styles
      threshold_values$selected_y_vars <- input$thresholdYVar
      threshold_values$data_prepared <- TRUE
      
    }, error = function(e) {
      threshold_values$data_prepared <- FALSE
    })
  })
  
  threshold_plot_reactive <- reactive({
    req(threshold_values$data_prepared)
    req(threshold_values$plot_data)
    
    input$thresholdValue
    input$thresholdColor
    input$thresholdLineWidth
    input$thresholdLineType
    input$thresholdPlotTitle
    input$thresholdXLabel
    input$thresholdYLabel
    input$thresholdUseColor
    input$thresholdBarColor
    input$thresholdPalette
    input$thresholdSingleBarColor
    input$thresholdXLabelBold
    input$thresholdXLabelItalic
    input$thresholdYLabelBold
    input$thresholdYLabelItalic
    input$thresholdBlackAxes
    input$thresholdShowAxisLines
    input$thresholdShowTicks
    input$thresholdShowGrid
    input$thresholdLabelAngle
    input$thresholdTitleSize
    input$thresholdAxisTitleSize
    input$thresholdAxisTextSize
    input$thresholdLegendSize
    input$thresholdYMin
    input$thresholdYMax
    input$thresholdShowLegend
    input$thresholdLegendPosition
    input$thresholdLegendTitle
    input$thresholdLegendBold
    input$thresholdLegendItalic
    input$thresholdBarWidth
    input$thresholdBarSpacing
    input$thresholdBarPosition
    
    lapply(names(threshold_values$label_mapping), function(lvl) {
      input[[paste0("thresholdLevel_", make.names(lvl))]]
      input[[paste0("thresholdLevelBold_", make.names(lvl))]]
      input[[paste0("thresholdLevelItalic_", make.names(lvl))]]
    })
    
    if(length(threshold_values$selected_y_vars) > 1) {
      lapply(threshold_values$selected_y_vars, function(var_name) {
        input[[paste0("thresholdLegendLevel_", make.names(var_name))]]
        input[[paste0("thresholdLegendLevelBold_", make.names(var_name))]]
        input[[paste0("thresholdLegendLevelItalic_", make.names(var_name))]]
      })
    }
    
    if(!is.null(input$thresholdBarColor) && input$thresholdBarColor == "custom") {
      lapply(seq_along(levels(threshold_values$plot_data$Treatment)), function(i) {
        input[[paste0("thresholdCustomColor_", i)]]
      })
    }
    
    plot_data <- threshold_values$plot_data
    label_styles <- threshold_values$label_styles
    is_multiple_y <- length(threshold_values$selected_y_vars) > 1
    
    tryCatch({
      if(is_multiple_y) {
        p <- ggplot(plot_data, aes(x = Treatment, y = Efficacy, fill = Variable))
        
        bar_width <- (input$thresholdBarWidth %||% 0.8)
        dodge_width <- bar_width + (input$thresholdBarSpacing %||% 0.1)
        
        position <- if(!is.null(input$thresholdBarPosition) && input$thresholdBarPosition == "stack") {
          "stack"
        } else {
          position_dodge(width = dodge_width)
        }
        
        p <- p + geom_col(position = position, 
                          width = bar_width,
                          alpha = 0.8)
        
        p <- p + labs(fill = input$thresholdLegendTitle %||% "Variables")
        
      } else {
        p <- ggplot(plot_data, aes(x = Treatment, y = Efficacy))
        
        bar_width <- input$thresholdBarWidth %||% 0.8

        # Libelles de legende personnalises (mode Y simple) : la legende montre
        # les niveaux (traitements) ; on lit l'editeur thresholdLegendItem_* pour
        # renommer ces entrees independamment de l'axe X. 'levels(Treatment)' est
        # deja relabelise par l'editeur d'axe X, donc on mappe a partir de ces
        legend_levels <- levels(plot_data$Treatment)
        legend_labels <- sapply(legend_levels, function(lbl) {
          custom <- input[[paste0("thresholdLegendItem_", make.names(lbl))]]
          if (is.null(custom) || custom == "") lbl else custom
        })
        names(legend_labels) <- legend_levels
        
        if(input$thresholdUseColor) {
          if(input$thresholdBarColor == "ggplot") {
            p <- p + geom_col(aes(fill = Treatment), width = bar_width, alpha = 0.8) +
              scale_fill_discrete(name = input$thresholdLegendTitle %||% "Traitements",
                                  labels = legend_labels)
          } else if(input$thresholdBarColor == "palette") {
            p <- p + geom_col(aes(fill = Treatment), width = bar_width, alpha = 0.8) +
              scale_fill_brewer(palette = input$thresholdPalette %||% "Set1",
                                name = input$thresholdLegendTitle %||% "Traitements",
                                labels = legend_labels)
          } else if(input$thresholdBarColor == "custom") {
            custom_colors <- sapply(seq_along(levels(plot_data$Treatment)), function(i) {
              color_input <- input[[paste0("thresholdCustomColor_", i)]]
              if(is.null(color_input)) scales::hue_pal()(length(levels(plot_data$Treatment)))[i] else color_input
            })
            p <- p + geom_col(aes(fill = Treatment), width = bar_width, alpha = 0.8) +
              scale_fill_manual(values = custom_colors,
                                name = input$thresholdLegendTitle %||% "Traitements",
                                labels = legend_labels)
          } else if(input$thresholdBarColor == "black") {
            p <- p + geom_col(fill = "#000000", width = bar_width, alpha = 0.8)
          } else if(input$thresholdBarColor == "single") {
            p <- p + geom_col(fill = input$thresholdSingleBarColor %||% "#3498db", 
                              width = bar_width, alpha = 0.8)
          }
        } else {
          p <- p + geom_col(fill = "#3498db", width = bar_width, alpha = 0.8)
        }
      }
      
      p <- p + geom_hline(yintercept = input$thresholdValue %||% 80, 
                          color = input$thresholdColor %||% "#e74c3c",
                          linewidth = input$thresholdLineWidth %||% 1.5,
                          linetype = input$thresholdLineType %||% "solid")
      
      p <- p + annotate("text", 
                        x = length(levels(plot_data$Treatment)) * 0.9,
                        y = (input$thresholdValue %||% 80) + 5,
                        label = paste("Seuil:", input$thresholdValue %||% 80, "%"),
                        color = input$thresholdColor %||% "#e74c3c",
                        fontface = "bold",
                        size = 4)
      
      plot_title <- if(!is.null(input$thresholdPlotTitle) && input$thresholdPlotTitle != "") {
        input$thresholdPlotTitle
      } else {
        "Analyse des seuils d'efficacité"
      }
      
      x_label <- if(!is.null(input$thresholdXLabel) && input$thresholdXLabel != "") {
        input$thresholdXLabel
      } else {
        "Traitements"
      }
      
      y_label <- if(!is.null(input$thresholdYLabel) && input$thresholdYLabel != "") {
        input$thresholdYLabel
      } else {
        "Seuil d'efficacité (%)"
      }
      
      x_label_face <- if(input$thresholdXLabelBold && input$thresholdXLabelItalic) {
        "bold.italic"
      } else if(input$thresholdXLabelBold) {
        "bold"
      } else if(input$thresholdXLabelItalic) {
        "italic"
      } else {
        "plain"
      }
      
      y_label_face <- if(input$thresholdYLabelBold && input$thresholdYLabelItalic) {
        "bold.italic"
      } else if(input$thresholdYLabelBold) {
        "bold"
      } else if(input$thresholdYLabelItalic) {
        "italic"
      } else {
        "plain"
      }
      
      legend_title_face <- if(input$thresholdLegendBold && input$thresholdLegendItalic) {
        "bold.italic"
      } else if(input$thresholdLegendBold) {
        "bold"
      } else if(input$thresholdLegendItalic) {
        "italic"
      } else {
        "plain"
      }
      
      axis_color <- if(!is.null(input$thresholdBlackAxes) && input$thresholdBlackAxes) {
        "black"
      } else {
        "grey50"
      }
      
      legend_position <- if(!is.null(input$thresholdShowLegend) && input$thresholdShowLegend) {
        pos <- input$thresholdLegendPosition %||% "right"
        if(pos == "top_right") c(0.95, 0.95)
        else if(pos == "top_left") c(0.05, 0.95)
        else if(pos == "bottom_right") c(0.95, 0.05)
        else if(pos == "bottom_left") c(0.05, 0.05)
        else pos
      } else {
        "none"
      }
      
      legend_justification <- if(!is.null(input$thresholdLegendPosition)) {
        pos <- input$thresholdLegendPosition
        if(pos == "top_right") c(1, 1)
        else if(pos == "top_left") c(0, 1)
        else if(pos == "bottom_right") c(1, 0)
        else if(pos == "bottom_left") c(0, 0)
        else "center"
      } else {
        "center"
      }
      
      show_legend <- (is_multiple_y || (input$thresholdUseColor && 
                                          input$thresholdBarColor %in% c("ggplot", "custom", "palette"))) &&
        (!is.character(legend_position) || legend_position != "none")
      
      p <- p + labs(title = plot_title, x = x_label, y = y_label) +
        scale_y_continuous(limits = c(input$thresholdYMin %||% 0, input$thresholdYMax %||% 100)) +
        theme_minimal() +
        theme(
          plot.title = element_markdown(size = input$thresholdTitleSize %||% 16, 
                                        hjust = 0.5, face = "bold"),
          axis.title.x = element_markdown(size = input$thresholdAxisTitleSize %||% 14, 
                                          face = x_label_face,
                                          color = axis_color),
          axis.title.y = element_markdown(size = input$thresholdAxisTitleSize %||% 14, 
                                          face = y_label_face,
                                          color = axis_color),
          axis.text.y = element_text(size = input$thresholdAxisTextSize %||% 12,
                                     color = axis_color),
          axis.text.x = {
            ang <- input$thresholdLabelAngle %||% 45
            element_text(angle = ang,
                         hjust = if (ang == 0) 0.5 else 1,
                         vjust = if (ang == 0) 1 else 1,
                         color = axis_color,
                         size = input$thresholdAxisTextSize %||% 12)
          },
          axis.line = if(input$thresholdShowAxisLines) {
            element_line(color = axis_color, linewidth = 0.5)
          } else {
            element_blank()
          },
          axis.ticks = if(input$thresholdShowTicks) {
            element_line(color = axis_color, linewidth = 0.5)
          } else {
            element_blank()
          },
          legend.position = if(show_legend) legend_position else "none",
          legend.justification = if(show_legend && is.numeric(legend_position)) legend_justification else NULL,
          legend.background = if(show_legend && is.numeric(legend_position)) {
            element_rect(fill = "white", color = "grey80", linewidth = 0.5)
          } else {
            element_blank()
          },
          legend.title = element_markdown(size = input$thresholdLegendSize %||% 10, face = legend_title_face),
          legend.text = element_text(size = input$thresholdLegendSize %||% 10),
          panel.grid.major = if(input$thresholdShowGrid) {
            element_line(color = "grey90")
          } else {
            element_blank()
          },
          panel.grid.minor = element_blank(),
          panel.border = if(input$thresholdShowAxisLines) {
            element_rect(color = axis_color, fill = NA, linewidth = 0.5)
          } else {
            element_blank()
          }
        )

      # Legende : on l'organise en une seule colonne pour que les longues
      # etiquettes (noms de traitements complets) restent lisibles, qu'elle soit
      # a droite/gauche (colonne verticale) ou en bas/haut (empilement vertical
      # qui evite de recouvrir les labels X inclines).
      if (show_legend) {
        p <- p + guides(fill = guide_legend(ncol = 1, byrow = TRUE),
                        color = guide_legend(ncol = 1, byrow = TRUE))
      }
      
      # Mise en forme des etiquettes de l'axe X. Le style est retenu A PART
      # (`x_label_styles`) : le graphique porte du plotmath, que ggsave rend
      # correctement, mais que ggplotly DEPARSE -- l'axe affichait
      # `bold("2SP(0,5)&2PV")` en toutes lettres a l'ecran. Le rendu
      # interactif reprend donc les memes styles en HTML, que plotly comprend.
      threshold_values$x_label_levels <- NULL
      threshold_values$x_label_styles <- NULL
      if(!is.null(label_styles) && length(label_styles) > 0) {
        treatment_levels <- levels(plot_data$Treatment)
        
        styles_par_niveau <- vapply(seq_along(treatment_levels), function(i) {
          current_label <- as.character(treatment_levels[i])
          idx <- which(threshold_values$label_mapping == current_label)
          if(length(idx) > 0) as.character(label_styles[idx[1]]) else "plain"
        }, character(1))
        styles_par_niveau[is.na(styles_par_niveau)] <- "plain"
        
        styled_labels <- lapply(seq_along(treatment_levels), function(i) {
          current_label <- as.character(treatment_levels[i])
          switch(styles_par_niveau[i],
                 "bold"       = bquote(bold(.(current_label))),
                 "italic"     = bquote(italic(.(current_label))),
                 "bolditalic" = bquote(bolditalic(.(current_label))),
                 current_label)
        })
        
        threshold_values$x_label_levels <- treatment_levels
        threshold_values$x_label_styles <- styles_par_niveau
        p <- p + scale_x_discrete(labels = styled_labels)
      }
      
      threshold_values$current_plot <- p
      
      return(p)
      
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur lors de la mise à jour"), type = "error", duration = 5)
      return(NULL)
    })
  })
  
  output$thresholdPlot <- renderPlotly({
    p <- threshold_plot_reactive()
    req(p)
    
    is_multiple_y <- length(threshold_values$selected_y_vars) > 1
    show_legend <- if(!is.null(input$thresholdShowLegend) && input$thresholdShowLegend) {
      is_multiple_y || (input$thresholdUseColor && input$thresholdBarColor %in% c("ggplot", "custom", "palette"))
    } else {
      FALSE
    }

    # Position de la legende choisie par l'utilisateur, traduite pour plotly.
    pos <- input$thresholdLegendPosition %||% "right"
    # plotly gere la legende independamment du graphique : on la place SOUS ou
    # A COTE de la zone de trace (jamais par-dessus), ce que ggplotly ne fait
    # pas correctement a partir du theme ggplot seul.
    leg <- switch(pos,
      "bottom"       = list(orientation = "h", x = 0.5, xanchor = "center",
                            y = -0.35, yanchor = "top"),
      "top"          = list(orientation = "h", x = 0.5, xanchor = "center",
                            y = 1.12,  yanchor = "bottom"),
      "left"         = list(orientation = "v", x = -0.25, xanchor = "right",
                            y = 0.5,   yanchor = "middle"),
      "right"        = list(orientation = "v", x = 1.02,  xanchor = "left",
                            y = 0.5,   yanchor = "middle"),
      "top_right"    = list(orientation = "v", x = 0.99,  xanchor = "right",
                            y = 0.99,  yanchor = "top",
                            bgcolor = "rgba(255,255,255,0.7)"),
      "top_left"     = list(orientation = "v", x = 0.01,  xanchor = "left",
                            y = 0.99,  yanchor = "top",
                            bgcolor = "rgba(255,255,255,0.7)"),
      "bottom_right" = list(orientation = "v", x = 0.99,  xanchor = "right",
                            y = 0.01,  yanchor = "bottom",
                            bgcolor = "rgba(255,255,255,0.7)"),
      "bottom_left"  = list(orientation = "v", x = 0.01,  xanchor = "left",
                            y = 0.01,  yanchor = "bottom",
                            bgcolor = "rgba(255,255,255,0.7)"),
      list(orientation = "v", x = 1.02, xanchor = "left", y = 0.5, yanchor = "middle"))

    # Marge basse genereuse pour laisser respirer les labels X inclines, et
    # marge droite suffisante quand la legende est a droite.
    mar <- list(l = 70, r = if (pos == "right") 60 else 30,
                b = 140, t = 60, pad = 4)

    gp <- ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = show_legend,
             legend = leg,
             margin = mar,
             autosize = TRUE) %>%
      config(responsive = TRUE)

    # Gras et italique de l'axe X, en HTML : plotmath ne survit pas a la
    # conversion. Les positions d'un axe discret valent 1..n.
    lv <- threshold_values$x_label_levels
    st <- threshold_values$x_label_styles
    if (!is.null(lv) && length(lv) && length(st) == length(lv) &&
        any(st != "plain")) {
      gp <- gp %>% layout(xaxis = list(
        tickmode = "array",
        tickvals = seq_along(lv),
        ticktext = hstat_html_style_label(lv, st)))
    }
    gp
  })
  
  output$thresholdDataTable <- renderDT({
    req(threshold_values$plot_data)
    
    datatable(threshold_values$plot_data,
              options = list(
                pageLength = 10, 
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              class = 'cell-border stripe hover',
              caption = tags$caption(
                style = 'caption-side: top; text-align: center; color: #3c8dbc; font-size: 16px; font-weight: bold;',
                'Données utilisées pour le graphique'
              ))
  })
  
  output$downloadThresholdPlot <- downloadHandler(
    filename = function() {
      format <- input$thresholdExportFormat %||% "png"
      paste0("seuils_efficacite_", Sys.Date(), ".", format)
    },
    content = function(file) {
      req(threshold_values$current_plot)
      
      # Les pixels saisis fixent la MISE EN PAGE (lue a 96 ppp, la reference de
      # l'ecran), le DPI la finesse du rendu. Diviser les pixels par le DPI,
      # comme ici auparavant, donnait une toile de quatre pouces ou les onze
      # etiquettes de traitement s'ecrasaient -- et monter le DPI la
      # retrecissait encore. Voir hstat_export_dims() dans Utils.R.
      dims <- hstat_export_dims(input$thresholdExportWidth,
                                input$thresholdExportHeight,
                                input$thresholdExportDPI)
      width_in  <- dims$width_in
      height_in <- dims$height_in
      dpi       <- dims$dpi
      if (!is.null(dims$note))
        showNotification(dims$note, type = "warning", duration = 8)
      
      format <- input$thresholdExportFormat %||% "png"
      
      tryCatch({
        if(format == "svg") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 device = "svg")
        } else if(format == "pdf") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 device = "pdf")
        } else if(format == "eps") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 device = "eps")
        } else if(format == "tiff") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 dpi = dpi,
                 device = "tiff",
                 compression = "lzw")
        } else if(format == "bmp") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 dpi = dpi,
                 device = "bmp")
        } else if(format == "jpeg") {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 dpi = dpi,
                 device = "jpeg",
                 quality = 95)
        } else {
          ggsave(file, 
                 plot = threshold_values$current_plot,
                 width = width_in,
                 height = height_in,
                 dpi = dpi,
                 device = "png",
                 type = "cairo")
        }
        
        showNotification(
          paste0("Graphique exporté avec succès\n",
                 "Format: ", toupper(format), "\n",
                 "Dimensions: ", round(width_in, 2), "x", round(height_in, 2), " pouces\n",
                 "Résolution: ", dpi, " DPI"), 
          type = "message", 
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste0(hstat_err_fr(e, "Export"), " ",
                 "\n\nConseils:",
                 "\n- Réduisez les dimensions ou le DPI",
                 "\n- Utilisez un format vectoriel (SVG, PDF) pour haute résolution",
                 "\n- Maximum recommandé: 5000x5000 px à 600 DPI"), 
          type = "error", 
          duration = 10
        )
      })
    }
  )
  
  # L'apercu annonce les DEUX tailles : la mise en page demandee et les pixels
  # reellement produits. Ne montrer que la premiere laissait croire qu'un DPI
  # plus eleve ne changeait rien au fichier.
  output$exportSizeEstimate <- renderText({
    format <- input$thresholdExportFormat %||% "png"
    d <- hstat_export_dims(input$thresholdExportWidth,
                           input$thresholdExportHeight,
                           input$thresholdExportDPI)
    
    if (format %in% c("svg", "pdf", "eps"))
      return(trf("Mise en page %s × %s pouces. Format vectoriel : résolution illimitée, le DPI ne s'applique pas.",
                 round(d$width_in, 2), round(d$height_in, 2)))
    
    pixels <- d$width_out * d$height_out
    size_mb <- if(format == "jpeg") (pixels * 0.3) / (1024 * 1024)
               else (pixels * 3) / (1024 * 1024)
    
    paste0(trf("Mise en page %s × %s pouces → fichier de %s × %s pixels à %s DPI",
               round(d$width_in, 2), round(d$height_in, 2),
               d$width_out, d$height_out, d$dpi),
           " | ", trf("Taille estimée : %s Mo", round(size_mb, 2)))
  })
  
  output$downloadThresholdData <- downloadHandler(
    filename = function() {
      paste0("données_seuils_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(threshold_values$plot_data)
      
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "Données")
      openxlsx::writeData(wb, "Données", threshold_values$plot_data)
      
      headerStyle <- openxlsx::createStyle(
        fontSize = 12,
        fontColour = "#FFFFFF",
        halign = "center",
        fgFill = "#3c8dbc",
        border = "TopBottomLeftRight",
        borderColour = "#000000",
        textDecoration = "bold"
      )
      
      openxlsx::addStyle(wb, "Données", headerStyle, rows = 1, cols = 1:ncol(threshold_values$plot_data), gridExpand = TRUE)
      
      y_vars_text <- if(length(threshold_values$selected_y_vars) > 1) {
        paste(threshold_values$selected_y_vars, collapse = ", ")
      } else {
        threshold_values$selected_y_vars
      }
      
      params <- data.frame(
        `Paramètre` = c("Seuil d'efficacité (%)", 
                      "Variable X", 
                      "Variable(s) Y",
                      "Date d'export",
                      "Nombre de traitements",
                      "Limites Y",
                      "Format d'export",
                      "Dimensions (pixels)",
                      "Résolution (DPI)",
                      "Largeur barres",
                      "Espacement barres"),
        Valeur = c(input$thresholdValue %||% 80, 
                   input$thresholdXVar, 
                   y_vars_text,
                   as.character(Sys.Date()),
                   length(unique(threshold_values$plot_data$Treatment)),
                   paste(input$thresholdYMin %||% 0, "-", input$thresholdYMax %||% 100),
                   input$thresholdExportFormat %||% "png",
                   paste(input$thresholdExportWidth %||% 1200, "x", input$thresholdExportHeight %||% 800),
                   input$thresholdExportDPI %||% 300,
                   input$thresholdBarWidth %||% 0.8,
                   input$thresholdBarSpacing %||% 0.1)
      )
      
      openxlsx::addWorksheet(wb, "Paramètres")
      openxlsx::writeData(wb, "Paramètres", params)
      openxlsx::addStyle(wb, "Paramètres", headerStyle, rows = 1, cols = 1:2, gridExpand = TRUE)
      
      if(!is.null(threshold_values$label_mapping)) {
        label_info <- data.frame(
          Label_Original = names(threshold_values$label_mapping),
          `Label_Personnalisé` = as.character(threshold_values$label_mapping),
          Style = if(!is.null(threshold_values$label_styles)) {
            threshold_values$label_styles
          } else {
            rep("plain", length(threshold_values$label_mapping))
          }
        )
        
        openxlsx::addWorksheet(wb, "Labels X")
        openxlsx::writeData(wb, "Labels X", label_info)
        openxlsx::addStyle(wb, "Labels X", headerStyle, rows = 1, cols = 1:3, gridExpand = TRUE)
      }
      
      # Ajouter le mapping des labels de légende (Y multiples)
      if(!is.null(threshold_values$legend_label_mapping)) {
        legend_info <- data.frame(
          Variable_Originale = names(threshold_values$legend_label_mapping),
          `Label_Légende` = as.character(threshold_values$legend_label_mapping),
          Style = if(!is.null(threshold_values$legend_label_styles)) {
            threshold_values$legend_label_styles
          } else {
            rep("plain", length(threshold_values$legend_label_mapping))
          }
        )
        
        openxlsx::addWorksheet(wb, "Labels Légende")
        openxlsx::writeData(wb, "Labels Légende", legend_info)
        openxlsx::addStyle(wb, "Labels Légende", headerStyle, rows = 1, cols = 1:3, gridExpand = TRUE)
      }
      
      openxlsx::setColWidths(wb, "Données", cols = 1:ncol(threshold_values$plot_data), widths = "auto")
      openxlsx::setColWidths(wb, "Paramètres", cols = 1:2, widths = c(25, 30))
      if(!is.null(threshold_values$label_mapping)) {
        openxlsx::setColWidths(wb, "Labels X", cols = 1:3, widths = c(20, 25, 15))
      }
      if(!is.null(threshold_values$legend_label_mapping)) {
        openxlsx::setColWidths(wb, "Labels Légende", cols = 1:3, widths = c(20, 25, 15))
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      
      showNotification("Données exportées avec succès!", type = "message", duration = 3)
    }
  )
  })
}
