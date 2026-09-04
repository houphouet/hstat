#  Module Shiny : Seuils d'efficacite


mod_threshold_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
              shiny::fluidRow(
                shiny::div(class = "callout callout-info", style = "margin-bottom:14px;",
                    shiny::icon("info-circle"), 
                    shiny::strong(" Mode mise à jour automatique : "),
                    "les modifications sont appliquées instantanément au graphique.")
              ),
              
              shiny::fluidRow(
                shinydashboard::box(title = shiny::tagList(shiny::icon("sliders"), " Configuration de l'analyse"), 
                    status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,
                    shiny::tabsetPanel(
                      id = ns("thresholdConfigTabs"),
                      shiny::tabPanel(shiny::tagList(shiny::icon("database"), " Données & seuil"),
                        shiny::div(style = "padding-top:14px;",
                    
                    shiny::div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        shiny::h5(shiny::icon("database"), " Sélection des variables", 
                           style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                    ),
                    
                    # La source du graphique : le fichier charge, ou le tableau
                    # d'efficacites calcule dans l'onglet « Calcul depuis un temoin ».
                    # Sans ce choix, tracer les efficacites obligeait a REMPLACER le
                    # jeu de travail puis a re-selectionner X et Y -- un detour qui
                    # fait perdre le fichier d'origine pour un simple graphique.
                    shiny::radioButtons(ns("thresholdSource"),
                      shiny::tagList(shiny::icon("database"), " Source des données"),
                      choiceNames = list(
                        shiny::HTML("<b>Jeu de données chargé</b>"),
                        shiny::HTML("<b>Efficacités calculées</b> <small style='color:#7f8c8d;'>(onglet « Calcul depuis un témoin »)</small>")),
                      choiceValues = list("donnees", "calcul"),
                      selected = "donnees"),
                    shiny::uiOutput(ns("thresholdSourceNote")),

                    shiny::uiOutput(ns("thresholdXVarSelect")),
                    
                    shiny::h6(shiny::icon("chart-line"), " Variables Y (Efficacité)", 
                       style = "font-weight: bold; color: #3c8dbc; margin-top: 15px;"),
                    shiny::checkboxInput(ns("thresholdMultipleY"), 
                                  shiny::tagList(shiny::icon("layer-group"), " Activer la sélection multiple de Y"), 
                                  value = FALSE),
                    shiny::uiOutput(ns("thresholdYVarSelect")),
                    
                    shiny::conditionalPanel(
              ns = ns,
                      condition = "input.thresholdMultipleY && input.thresholdYVar && input.thresholdYVar.length > 1",
                      shiny::div(style = "background-color: #e3f2fd; padding: 12px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #2196F3;",
                          shiny::icon("palette", style = "color: #2196F3;"),
                          shiny::strong(" Info : "), 
                          "Les couleurs des variables Y multiples utilisent automatiquement la palette ggplot2 par défaut pour une meilleure distinction visuelle."
                      )
                    ),
                    
                    shiny::hr(style = "border-top: 2px solid #3c8dbc; margin: 20px 0;"),
                    
                    shiny::div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        shiny::h5(shiny::icon("bullseye"), " Paramètres du seuil", 
                           style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                    ),
                    
                    # AUCUNE BORNE. Le seuil etait bride a 0-100 : on ne pouvait
                    # donc pas poser de repere sur une efficacite NEGATIVE (la
                    # modalite fait moins bien que le temoin), ni au-dela de 100
                    # sur un rapport qui le permet. Un repere qu'on ne peut pas
                    # saisir est un repere qui n'existe pas.
                    shiny::numericInput(ns("thresholdValue"), 
                                 shiny::tagList(shiny::icon("percent"), " Valeur du seuil (%)"), 
                                 value = 80, step = 1),
                    
                    shiny::fluidRow(
                      shiny::column(6,
                             colourInput(ns("thresholdColor"), "Couleur de la ligne :", 
                                         value = "#e74c3c", showColour = "background")
                      ),
                      shiny::column(6,
                             shiny::numericInput(ns("thresholdLineWidth"), "Épaisseur :", 
                                          value = 1.5, min = 0.5, max = 5, step = 0.5)
                      )
                    ),
                    
                    shiny::selectInput(ns("thresholdLineType"), "Type de ligne :",
                                choices = c("Solide" = "solid",
                                            "Pointillé" = "dotted",
                                            "Tirets" = "dashed",
                                            "Tirets-points" = "dotdash",
                                            "Tirets longs" = "longdash",
                                            "Deux tirets" = "twodash"),
                                selected = "solid"),

                    # L'etiquette qui porte la valeur du seuil se regle LA OU
                    # la valeur se saisit. Sa taille vivait dans « Apparence &
                    # options », deux onglets plus loin : on la cherchait ici.
                    shiny::div(style = "background:#fdeeec; border-left:4px solid #c0392b; border-radius:6px; padding:12px 14px; margin-bottom:14px;",
                        shiny::h6(shiny::icon("tag"), " Étiquette de la valeur sur le graphique",
                           style = "font-weight:700; color:#c0392b; margin:0 0 10px 0; text-transform:uppercase; letter-spacing:.4px; font-size:12px;"),
                        shiny::checkboxInput(ns("thresholdShowLabel"),
                                      shiny::tagList(shiny::icon("eye"), " Afficher « Seuil : x % » sur le graphique"),
                                      value = TRUE),
                        shiny::conditionalPanel(
                          ns = ns,
                          condition = "input.thresholdShowLabel",
                          shiny::sliderInput(ns("thresholdValueLabelSize"),
                                      shiny::tagList(shiny::icon("text-height"), " Taille de l'étiquette"),
                                      min = 2, max = 16, value = 4, step = 0.5),
                          shiny::fluidRow(
                            shiny::column(6, shiny::selectInput(ns("thresholdLabelPos"), "Position",
                                                  choices = c("À droite" = "droite",
                                                              "Au centre" = "centre",
                                                              "À gauche" = "gauche"),
                                                  selected = "droite")),
                            shiny::column(6, shiny::selectInput(ns("thresholdLabelStyle"), "Style",
                                                  choices = HSTAT_FONT_STYLES,
                                                  selected = "bold"))),
                          shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                                     "L'étiquette reprend la couleur de la ligne de seuil."))),
                    
                    shiny::hr(style = "border-top: 2px solid #f39c12; margin: 20px 0;"),
                    
                    shiny::div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        shiny::h5(shiny::icon("filter"), " Filtrage des données", 
                           style = "color: #d35400; font-weight: bold; margin: 0;")
                    ),
                    
                    shiny::uiOutput(ns("thresholdFilterSelect")),
                    
                    shiny::hr(style = "border-top: 2px solid #27ae60; margin: 20px 0;"),
                    
                    shiny::div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        shiny::h5(shiny::icon("tag"), " Personnalisation des labels X", 
                           style = "color: #16a085; font-weight: bold; margin: 0;")
                    ),
                    
                    shiny::div(style = "background-color: #fff9e6; padding: 10px; border-radius: 6px; margin-bottom: 10px; border-left: 4px solid #f39c12;",
                        shiny::icon("lightbulb", style = "color: #f39c12;"),
                        shiny::em(" Astuce : Modifiez les étiquettes des traitements et appliquez des styles (gras/italique) pour une meilleure présentation.")
                    ),
                    
                    shiny::uiOutput(ns("thresholdLevelsEditor")),
                    
                    shiny::conditionalPanel(
              ns = ns,
                      condition = "input.thresholdMultipleY && input.thresholdYVar && input.thresholdYVar.length > 1",
                      shiny::hr(style = "border-top: 2px solid #9b59b6; margin: 20px 0;"),
                      
                      shiny::div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                          shiny::h5(shiny::icon("list-ul"), " Personnalisation des labels de légende", 
                             style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                      ),
                      
                      shiny::div(style = "background-color: #f3e5f5; padding: 10px; border-radius: 6px; margin-bottom: 10px; border-left: 4px solid #9b59b6;",
                          shiny::icon("info-circle", style = "color: #9b59b6;"),
                          shiny::em(" Info : Personnalisez les étiquettes affichées dans la légende pour les variables Y sélectionnées.")
                      ),
                      
                      shiny::uiOutput(ns("thresholdLegendEditor"))
                    ),
                        )
                      ),
                      shiny::tabPanel(shiny::tagList(shiny::icon("palette"), " Apparence & options"),
                        shiny::div(style = "padding-top:14px;",
                    shiny::div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        shiny::h5(shiny::icon("palette"), " Options graphiques avancées", 
                           style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                    ),
                    
                    shiny::div(style = "background-color: #f9f9f9; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #e0e0e0;",
                        shiny::h6(shiny::icon("heading"), " Titres et étiquettes", 
                           style = "font-weight: bold; color: #34495e; margin-bottom: 10px;"),
                        shiny::textInput(ns("thresholdPlotTitle"), "Titre du graphique :", 
                                  value = "Analyse des seuils d'efficacité"),
                        shiny::textInput(ns("thresholdXLabel"), "Label axe X :", 
                                  value = "", placeholder = "Par défaut: Traitements"),
                        shiny::textInput(ns("thresholdYLabel"), "Label axe Y :", 
                                  value = "", placeholder = "Par défaut: Seuil d'efficacité (%)"),
                        shiny::textInput(ns("thresholdSubtitle"), "Sous-titre :",
                                  value = "", placeholder = "Optionnel"),
                        shiny::fluidRow(
                          shiny::column(6, shiny::selectInput(ns("thresholdTitleStyle"), "Style du titre",
                                                choices = HSTAT_FONT_STYLES, selected = "bold")),
                          shiny::column(6, shiny::selectInput(ns("thresholdTitlePosition"), "Position du titre",
                                                choices = HSTAT_ALIGNEMENTS, selected = "0.5"))
                        ),
                        shiny::fluidRow(
                          shiny::column(6, shiny::selectInput(ns("thresholdSubtitleStyle"), "Style du sous-titre",
                                                choices = HSTAT_FONT_STYLES, selected = "italic")),
                          shiny::column(6, shiny::selectInput(ns("thresholdSubtitlePosition"), "Position du sous-titre",
                                                choices = HSTAT_ALIGNEMENTS, selected = "0.5"))
                        ),
                        shiny::selectInput(ns("thresholdTheme"), "Thème du graphique",
                                    choices = HSTAT_THEMES_GG, selected = "minimal")
                    ),
                    
                    shiny::div(style = "background-color: #fff8e1; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #ffd54f;",
                        shiny::h6(shiny::icon("font"), " Style des labels d'axes", 
                           style = "font-weight: bold; color: #f57f17; margin-bottom: 10px;"),
                        
                        shiny::div(style = "margin-bottom: 10px;",
                            shiny::strong("Label axe X :"),
                            shiny::div(style = "margin-left: 15px; margin-top: 5px; display: flex; gap: 15px;",
                                shiny::checkboxInput(ns("thresholdXLabelBold"), "Gras", value = FALSE),
                                shiny::checkboxInput(ns("thresholdXLabelItalic"), "Italique", value = FALSE)
                            )
                        ),
                        
                        shiny::div(
                          shiny::strong("Label axe Y :"),
                          shiny::div(style = "margin-left: 15px; margin-top: 5px; display: flex; gap: 15px;",
                              shiny::checkboxInput(ns("thresholdYLabelBold"), "Gras", value = FALSE),
                              shiny::checkboxInput(ns("thresholdYLabelItalic"), "Italique", value = FALSE)
                          )
                        ),
                        # Les GRADUATIONS sont une famille a part : les titres
                        # d'axes avaient leur style, les valeurs portees sur les
                        # axes n'en avaient aucun.
                        shiny::fluidRow(
                          shiny::column(6, shiny::selectInput(ns("thresholdAxisTextXStyle"), "Style graduations X",
                                                choices = HSTAT_FONT_STYLES, selected = "plain")),
                          shiny::column(6, shiny::selectInput(ns("thresholdAxisTextYStyle"), "Style graduations Y",
                                                choices = HSTAT_FONT_STYLES, selected = "plain"))
                        )
                    ),

                    # ---- Valeurs portees sur les barres ----
                    .hstat_opt_section(
                      "Valeurs sur les barres", "hashtag", "#8e44ad", "#f7f0fb",
                      shiny::checkboxInput(ns("thresholdShowValues"),
                                    shiny::tagList(shiny::icon("eye"), " Afficher la valeur de chaque barre"),
                                    value = FALSE),
                      shiny::conditionalPanel(
                        ns = ns,
                        condition = "input.thresholdShowValues",
                        shiny::fluidRow(
                          shiny::column(6, shiny::numericInput(ns("thresholdValueDigits"), "Décimales",
                                                 value = 1, min = 0, max = 4, step = 1)),
                          shiny::column(6, shiny::sliderInput(ns("thresholdValueSize"), "Taille",
                                                min = 2, max = 12, value = 4, step = 0.5))
                        ),
                        shiny::fluidRow(
                          shiny::column(6, shiny::selectInput(ns("thresholdValueStyle"), "Style",
                                                choices = HSTAT_FONT_STYLES, selected = "plain")),
                          shiny::column(6, shiny::selectInput(ns("thresholdValuePosition"), "Position",
                                                choices = c("Au-dessus de la barre" = "dessus",
                                                            "Dans la barre, en haut" = "dedans",
                                                            "Au pied de la barre" = "pied"),
                                                selected = "dessus"))
                        ),
                        colourInput(ns("thresholdValueColor"), "Couleur", value = "#2c3e50"),
                        shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                                   "Une efficacité négative se lit sous l'axe : la position « au pied » la garde visible.")
                      )
                    ),
                    
                    shiny::conditionalPanel(
              ns = ns,
                      condition = "!input.thresholdMultipleY || (input.thresholdYVar && input.thresholdYVar.length == 1)",
                      shiny::div(style = "background-color: #e3f2fd; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #90caf9;",
                          shiny::h6(shiny::icon("paint-brush"), " Couleurs des barres", 
                             style = "font-weight: bold; color: #495057; margin-bottom: 10px;"),
                          
                          shiny::checkboxInput(ns("thresholdUseColor"), 
                                        shiny::tagList(shiny::icon("palette"), " Personnaliser les couleurs"), 
                                        value = TRUE),
                          
                          shiny::conditionalPanel(
              ns = ns,
                            condition = "input.thresholdUseColor",
                            shiny::radioButtons(ns("thresholdBarColor"), "Type de coloration :",
                                         choices = c("ggplot2 (défaut)" = "ggplot",
                                                     "Palette prédéfinie" = "palette",
                                                     "Personnalisé par traitement" = "custom",
                                                     "Couleur unique" = "single",
                                                     "Noir (monochrome)" = "black"),
                                         selected = "ggplot"),
                            
                            shiny::conditionalPanel(
              ns = ns,
                              condition = "input.thresholdBarColor == 'palette'",
                              # La liste vient du catalogue commun : celle qui
                              # vivait ici n'offrait AUCUNE palette non
                              # plafonnee, et un essai a plus de huit
                              # modalites y perdait ses dernieres barres en
                              # gris.
                              shiny::selectInput(ns("thresholdPalette"), "Choisir une palette :",
                                          choices = hstat_palettes_choix(),
                                          selected = unname(HSTAT_PALETTE_GG))
                            ),
                            
                            shiny::conditionalPanel(
              ns = ns,
                              condition = "input.thresholdBarColor == 'custom'",
                              shiny::div(style = "max-height: 300px; overflow-y: auto; padding: 5px;",
                                  shiny::uiOutput(ns("thresholdColorPickers"))
                              )
                            ),
                            
                            shiny::conditionalPanel(
              ns = ns,
                              condition = "input.thresholdBarColor == 'single'",
                              colourInput(ns("thresholdSingleBarColor"), "Couleur des barres :", 
                                          value = "#3498db", showColour = "background")
                            )
                          )
                      )
                    ),
                    
                    shiny::div(style = "background-color: #f0f8ff; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #b3d9ff;",
                        shiny::h6(shiny::icon("arrows-alt-h"), " Dimensions et espacement des barres", 
                           style = "font-weight: bold; color: #1e3a8a; margin-bottom: 10px;"),
                        
                        shiny::sliderInput(ns("thresholdBarWidth"), "Largeur des barres :", 
                                    min = 0.1, max = 1, value = 0.8, step = 0.05),
                        # La transparence etait figee a 0,8 aux six endroits ou
                        # les barres sont tracees.
                        shiny::sliderInput(ns("thresholdBarAlpha"), "Opacité des barres :",
                                    min = 0.2, max = 1, value = 0.8, step = 0.05),
                        shiny::checkboxInput(ns("thresholdBarBorder"),
                                      shiny::tagList(shiny::icon("border-style"), " Contour des barres"),
                                      value = FALSE),
                        shiny::conditionalPanel(
                          ns = ns,
                          condition = "input.thresholdBarBorder",
                          shiny::fluidRow(
                            shiny::column(6, colourInput(ns("thresholdBarBorderColor"), "Couleur du contour",
                                                  value = "#2c3e50")),
                            shiny::column(6, shiny::sliderInput(ns("thresholdBarBorderWidth"), "Épaisseur",
                                                  min = 0.1, max = 3, value = 0.5, step = 0.1))
                          )
                        ),
                        
                        shiny::conditionalPanel(
              ns = ns,
                          condition = "input.thresholdMultipleY && input.thresholdYVar && input.thresholdYVar.length > 1",
                          
                          shiny::sliderInput(ns("thresholdBarSpacing"), 
                                      shiny::tagList(shiny::icon("arrows-alt-h"), " Espacement entre barres :"), 
                                      min = 0, max = 0.5, value = 0.1, step = 0.05),
                          
                          shiny::div(style = "background-color: #e8f5e9; padding: 8px; border-radius: 4px; margin-top: 10px; border-left: 3px solid #4caf50;",
                              shiny::icon("info-circle", style = "color: #388e3c;"),
                              shiny::tags$small(" Plus l'espacement est élevé, plus les groupes de barres sont espacés.")
                          ),
                          
                          shiny::radioButtons(ns("thresholdBarPosition"), "Position des barres :",
                                       choices = c("Côte à côte" = "dodge",
                                                   "Empilées" = "stack"),
                                       selected = "dodge", inline = TRUE)
                        )
                    ),
                    
                    shiny::div(style = "background-color: #fff3e0; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #ffb74d;",
                        shiny::h6(shiny::icon("list"), " Configuration de la légende", 
                           style = "font-weight: bold; color: #e65100; margin-bottom: 10px;"),
                        
                        shiny::checkboxInput(ns("thresholdShowLegend"), 
                                      shiny::tagList(shiny::icon("eye"), " Afficher la légende"), 
                                      value = TRUE),
                        
                        shiny::conditionalPanel(
              ns = ns,
                          condition = "input.thresholdShowLegend",
                          shiny::textInput(ns("thresholdLegendTitle"), "Titre de la légende :", 
                                    value = "", placeholder = "Laisser vide pour défaut"),
                          
                          shiny::selectInput(ns("thresholdLegendPosition"), "Position :",
                                      choices = c("En bas" = "bottom",
                                                  "En haut" = "top",
                                                  "À gauche" = "left",
                                                  "À droite" = "right",
                                                  "Coin supérieur droit" = "top_right",
                                                  "Coin supérieur gauche" = "top_left",
                                                  "Coin inférieur droit" = "bottom_right",
                                                  "Coin inférieur gauche" = "bottom_left"),
                                      selected = "right"),
                          
                          shiny::div(style = "display: flex; gap: 15px; margin-top: 5px;",
                              shiny::checkboxInput(ns("thresholdLegendBold"), "Titre en gras", value = TRUE),
                              shiny::checkboxInput(ns("thresholdLegendItalic"), "Titre en italique", value = FALSE)
                          )
                        )
                    ),
                    
                    shiny::div(style = "background-color: #f5f5f5; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #cccccc;",
                        shiny::h6(shiny::icon("ruler"), " Apparence des axes", 
                           style = "font-weight: bold; color: #424242; margin-bottom: 10px;"),
                        
                        shiny::checkboxInput(ns("thresholdBlackAxes"), 
                                      shiny::tagList(shiny::icon("paint-roller"), " Axes en noir (sinon gris)"), 
                                      value = TRUE),
                        shiny::checkboxInput(ns("thresholdShowAxisLines"), 
                                      shiny::tagList(shiny::icon("minus"), " Afficher les lignes d'axes"), 
                                      value = TRUE),
                        shiny::checkboxInput(ns("thresholdShowTicks"), 
                                      shiny::tagList(shiny::icon("grip-lines"), " Afficher les graduations"), 
                                      value = TRUE),
                        shiny::checkboxInput(ns("thresholdShowGrid"), 
                                      shiny::tagList(shiny::icon("th"), " Afficher la grille"), 
                                      value = TRUE),
                        shiny::sliderInput(ns("thresholdLabelAngle"),
                                    shiny::tagList(shiny::icon("undo"), " Inclinaison des labels X (°)"),
                                    min = 0, max = 90, value = 45, step = 5)
                    ),

                    # Le module porte deja « Afficher les lignes d'axes » : il
                    # ne prend du kit que les trois familles qui lui manquaient.
                    hstat_plot_extras_ui(ns, "threshold",
                                         familles = c("police", "cles", "marges")),
                    
                    shiny::div(style = "background-color: #fce4ec; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #f48fb1;",
                        shiny::h6(shiny::icon("text-height"), " Tailles de texte", 
                           style = "font-weight: bold; color: #c2185b; margin-bottom: 10px;"),
                        
                        shiny::sliderInput(ns("thresholdTitleSize"), "Titre :", 
                                    min = 8, max = 28, value = 16, step = 1),
                        shiny::sliderInput(ns("thresholdAxisTitleSize"), "Titres des axes :", 
                                    min = 8, max = 24, value = 14, step = 1),
                        # Un titre plus long que son axe deborde et se fait
                        # rogner a l'export : il revient donc a la ligne, a la
                        # largeur REELLE de l'axe.
                        hstat_axe_titre_ui(ns, "threshold"),
                        shiny::sliderInput(ns("thresholdAxisTextSize"), "Texte des axes :", 
                                    min = 6, max = 20, value = 12, step = 1),
                        # Le titre de la legende et son texte partageaient un
                        # seul reglage : on ne pouvait pas grossir le titre sans
                        # grossir toutes les entrees.
                        shiny::sliderInput(ns("thresholdLegendSize"), "Titre de la légende :", 
                                    min = 6, max = 20, value = 10, step = 1),
                        shiny::sliderInput(ns("thresholdLegendTextSize"), "Texte de la légende :",
                                    min = 6, max = 20, value = 10, step = 1)
                    ),
                    
                    shiny::div(style = "background-color: #e8f5e9; padding: 12px; border-radius: 6px; margin-bottom: 12px; border: 1px solid #81c784;",
                        shiny::h6(shiny::icon("arrows-alt-v"), " Limites de l'axe Y", 
                           style = "font-weight: bold; color: #2e7d32; margin-bottom: 10px;"),
                        
                        # UNE EFFICACITE NEGATIVE EST UN RESULTAT -- la modalite fait
                        # moins bien que le temoin. Le minimum valait 0 ET portait
                        # `min = 0` : non seulement la barre negative sortait du
                        # cadre, mais l'utilisateur ne POUVAIT PAS saisir la valeur
                        # qui l'aurait ramenee. Le champ accepte donc le negatif,
                        # et vide il laisse l'axe suivre les donnees.
                        shiny::fluidRow(
                          shiny::column(6,
                                 shiny::numericInput(ns("thresholdYMin"), "Minimum (vide = auto) :",
                                              value = NA)
                          ),
                          shiny::column(6,
                                 shiny::numericInput(ns("thresholdYMax"), "Maximum (vide = auto) :",
                                              value = NA)
                          )
                        ),
                        shiny::checkboxInput(ns("thresholdZeroLine"),
                          "Ligne de référence à zéro (sépare gains et pertes)", TRUE),
                        # Sans pas de graduation, l'axe ne portait que les
                        # reperes choisis par ggplot -- rarement ceux qu'on veut
                        # sur un pourcentage (0, 10, 20...).
                        shiny::numericInput(ns("thresholdYBreakStep"),
                                     shiny::tagList(shiny::icon("ruler-vertical"), " Pas des graduations Y"),
                                     value = NA, min = 0.01, step = 5),
                        shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                                   "Laisser vide pour laisser ggplot choisir.")
                    ),

                        )
                      ),

                      # ---- Calcul de l'efficacite depuis un temoin ----
                      shiny::tabPanel(shiny::tagList(shiny::icon("calculator"), " Calcul depuis un témoin"),
                        shiny::div(style = "padding-top:14px;",

                          shiny::div(style = "background-color:#e8f5e9;padding:12px 14px;border-radius:6px;border-left:4px solid #27ae60;font-size:13px;",
                              shiny::icon("flask", style = "color:#27ae60;"),
                              shiny::strong(" Formule d'Abbott. "),
                              "L'efficacité de chaque modalité est calculée par rapport au témoin :",
                              shiny::tags$div(style = "text-align:center;margin:8px 0;font-family:monospace;font-size:14px;",
                                       "efficacité (%) = (témoin − traitement) × 100 / témoin"),
                              "Le témoin ne se compare pas à lui-même : son efficacité vaut ",
                              shiny::tags$b("0"), " par définition."),
                          shiny::br(),

                          shiny::uiOutput(ns("effFactorSelect")),
                          shiny::uiOutput(ns("effControlSelect")),
                          shiny::uiOutput(ns("effOtherLevels")),
                          shiny::uiOutput(ns("effResponseSelect")),

                          shiny::fluidRow(
                            shiny::column(6,
                              shiny::selectInput(ns("effAgg"), "Valeur résumée par modalité",
                                          choices = HSTAT_EFF_AGG, selected = "moyenne")),
                            shiny::column(6, shiny::uiOutput(ns("effGroupSelect")))),

                          shiny::radioButtons(ns("effMode"),
                            shiny::tagList(shiny::icon("layer-group"), " Traitement des répétitions"),
                            choiceNames = list(
                              shiny::HTML("<b>Mettre les répétitions en commun</b> <small style='color:#7f8c8d;'>(une efficacité par modalité — le chiffre que l'on publie)</small>"),
                              shiny::HTML("<b>Une efficacité par répétition</b> <small style='color:#7f8c8d;'>(autant de valeurs que de répétitions — analysable par ANOVA)</small>")),
                            choiceValues = list("cumul", "par_repetition"),
                            selected = "cumul"),

                          shiny::div(style = "background-color:#fff9e6;padding:10px 12px;border-radius:6px;border-left:4px solid #f39c12;font-size:12px;",
                              shiny::icon("lightbulb", style = "color:#f39c12;"),
                              shiny::em(" Les deux modes ne répondent pas à la même question. ",
                                 "En commun, la moyenne (ou la somme) de la modalité porte sur ",
                                 "toutes ses répétitions : c'est le chiffre du rapport. ",
                                 "Par répétition, on obtient une variable analysable ensuite par ",
                                 "ANOVA ou comparaisons multiples.")),

                          shiny::div(style = "background-color:#fdedec;padding:10px 12px;border-radius:6px;border-left:4px solid #c0392b;font-size:12px;",
                              shiny::icon("triangle-exclamation", style = "color:#c0392b;"),
                              shiny::em(" La ", shiny::tags$b("somme"), " n'est comparable que si les répétitions ",
                                 "sont en nombre égal : la modalité la plus répétée accumule ",
                                 "mécaniquement davantage et ressort artificiellement moins efficace. ",
                                 "La moyenne n'en souffre pas. L'application le signale si le cas se ",
                                 "présente.")),
                          shiny::br(),

                          shiny::actionButton(ns("effCompute"),
                                       shiny::tagList(shiny::icon("calculator"), " Calculer les efficacités"),
                                       class = "btn-success btn-block"),
                          shiny::br(),
                          shiny::uiOutput(ns("effMessage"))
                        )
                      )
                    )
                ),
                
                shinydashboard::box(title = shiny::tagList(shiny::icon("chart-bar"), " Graphique des seuils d'efficacité"), 
                    status = "primary", width = 8, solidHeader = TRUE, collapsible = TRUE,
                    
                    plotlyOutput(ns("thresholdPlot"), height = "600px"),
                    
                    shiny::br(),
                    shiny::hr(style = "border-top: 2px solid #3c8dbc;"),
                    
                    shiny::div(style = "background:#f4f4f4; border-left:3px solid #3c8dbc; padding:9px 12px; border-radius:0; margin-bottom:14px;",
                        shiny::h4(shiny::icon("download"), " Options d'exportation haute qualité", 
                           style = "color:#2b2b2b; font-weight:600; margin:0; font-size:13px;")
                    ),
                    
                    shiny::div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
                        shiny::h6(shiny::icon("cogs"), " Paramètres personnalisés", 
                           style = "font-weight: bold; color: #424242; margin-bottom: 10px;"),
                        
                        shiny::fluidRow(
                          shiny::column(4,
                                 shiny::numericInput(ns("thresholdExportWidth"), 
                                              shiny::tagList(shiny::icon("arrows-alt-h"), " Largeur (pixels)"), 
                                              value = 1200, min = 400, max = 20000, step = 100)
                          ),
                          shiny::column(4,
                                 shiny::numericInput(ns("thresholdExportHeight"), 
                                              shiny::tagList(shiny::icon("arrows-alt-v"), " Hauteur (pixels)"), 
                                              value = 800, min = 400, max = 20000, step = 100)
                          ),
                          shiny::column(4,
                                 hstat_dpi_input(ns("thresholdExportDPI"),
                                               shiny::tagList(shiny::icon("crosshairs"), " Résolution (DPI)"))
                          )
                        ),
                        
                        shiny::div(style = "background-color: #e1f5fe; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #0288d1;",
                            shiny::icon("info-circle", style = "color: #01579b;"),
                            shiny::strong(" Aperçu : "),
                            shiny::textOutput(ns("exportSizeEstimate"), inline = TRUE)
                        )
                    ),
                    
                    shiny::fluidRow(
                      shiny::column(12,
                             shiny::selectInput(ns("thresholdExportFormat"), 
                                         shiny::tagList(shiny::icon("file-image"), " Format d'export"),
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
                    
                    shiny::downloadButton(ns("downloadThresholdPlot"), 
                                   shiny::tagList(shiny::icon("download"), " Télécharger le graphique"), 
                                   class = "btn-success btn-lg btn-block", 
                                   style = "font-weight:600;")
                )
              ),
              
              shiny::fluidRow(
                shinydashboard::box(title = shiny::tagList(shiny::icon("calculator"), " Efficacités calculées depuis le témoin"),
                    status = "success", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,

                    shiny::uiOutput(ns("effResume")),
                    # Deux lectures du meme calcul. « Une colonne par variable »
                    # est le defaut : c'est elle qui alimente le selecteur
                    # « Variable Y » et qui se reinjecte dans l'application.
                    shiny::radioButtons(ns("effPresentation"),
                                 shiny::tagList(shiny::icon("table-columns"), " Présentation du tableau"),
                                 choiceNames = list(
                                   shiny::HTML("<b>Une colonne par variable mesurée</b> <small style='color:#7f8c8d;'>(une ligne par modalité)</small>"),
                                   shiny::HTML("<b>Détail</b> <small style='color:#7f8c8d;'>(une ligne par variable : effectif, témoin, valeur résumée)</small>")),
                                 choiceValues = list("large", "long"),
                                 selected = "large", inline = TRUE),
                    DT::DTOutput(ns("effTable")),
                    shiny::br(),
                    shiny::fluidRow(
                      shiny::column(4, shiny::downloadButton(ns("effDownloadCsv"),
                                               shiny::tagList(shiny::icon("file-csv"), " Télécharger (CSV)"),
                                               class = "btn-info btn-block")),
                      shiny::column(4, shiny::downloadButton(ns("effDownloadXlsx"),
                                               shiny::tagList(shiny::icon("file-excel"), " Télécharger (Excel)"),
                                               class = "btn-success btn-block")),
                      shiny::column(4, shiny::actionButton(ns("effUseAsData"),
                                             shiny::tagList(shiny::icon("right-left"), " Utiliser comme jeu de données"),
                                             class = "btn-warning btn-block"))),
                    shiny::br(),
                    shiny::div(style = "background-color:#fdedec;padding:10px 12px;border-radius:6px;border-left:4px solid #c0392b;font-size:12px;",
                        shiny::icon("triangle-exclamation", style = "color:#c0392b;"),
                        shiny::em(" « Utiliser comme jeu de données » REMPLACE le jeu de travail par ce ",
                           "tableau, pour l'analyser dans les autres onglets. Le fichier d'origine ",
                           "n'est pas modifié : rechargez-le pour revenir en arrière."))
                )
              ),

              shiny::fluidRow(
                shinydashboard::box(title = shiny::tagList(shiny::icon("table"), " Tableau des données utilisées"), 
                    status = "info", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    
                    shiny::div(style = "background-color: #fff9e6; padding: 12px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #ffa726;",
                        shiny::icon("info-circle", style = "color: #f57c00;"),
                        shiny::strong(" Information : "),
                        "Ce tableau affiche les données filtrées et transformées utilisées pour générer le graphique. ",
                        "Vous pouvez copier, exporter en CSV ou Excel directement depuis le tableau."
                    ),
                    
                    DT::DTOutput(ns("thresholdDataTable")),
                    
                    shiny::br(),
                    
                    shiny::downloadButton(ns("downloadThresholdData"), 
                                   shiny::tagList(shiny::icon("file-excel"), " Télécharger données complètes (Excel)"), 
                                   class = "btn-info btn-lg",
                                   style = "font-size: 16px; font-weight: bold; padding: 12px 24px;")
                )
              )
  )
}

mod_threshold_server <- function(id, values) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  # ---- Seuils d'efficacité ----
  
  threshold_values <- shiny::reactiveValues(
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
  shiny::observeEvent(threshold_values$plot_data, {
    pd <- threshold_values$plot_data
    if (is.null(pd) || !NROW(pd)) return()
    hstat_ai_capture(values, "Seuils d'efficacité",
      "Courbes de seuils d'efficacité",
      tables = list("Points des courbes" = utils::head(as.data.frame(pd), 200)),
      meta = list(variables = threshold_values$selected_y_vars,
                  `points traces` = NROW(pd)),
      plot = function() shiny::isolate(threshold_values$current_plot))
  }, ignoreInit = TRUE)

  # Reinitialisation globale : quand l'utilisateur clique sur "Réinitialiser" dans
  # l'en-tete de l'application, on remet ce module (Seuils d'efficacite) a zero :
  # etat interne efface et principaux controles visuels ramenes a leurs defauts.
  shiny::observeEvent(values$resetSignal, {
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
    # Le tableau d'efficacites calcule sur le fichier PRECEDENT : il alimente
    # `source_data()`, donc le graphique et les selecteurs. Sans cette ligne,
    # les modalites de l'ancien essai restaient proposees apres un nouveau
    # chargement.
    eff_res(NULL)
    shiny::updateNumericInput(session, "thresholdValue", value = 80)
    shiny::updateTextInput(session, "thresholdPlotTitle", value = "")
    shiny::updateTextInput(session, "thresholdXLabel", value = "")
    shiny::updateTextInput(session, "thresholdYLabel", value = "")
    shiny::updateTextInput(session, "thresholdLegendTitle", value = "")
    shiny::updateCheckboxInput(session, "thresholdMultipleY", value = FALSE)
    tryCatch(shinyjs::reset("thresholdValue"), error = function(e) NULL)
  }, ignoreInit = TRUE)

  # =========================================================================
  # CALCUL DE L'EFFICACITE DEPUIS UN TEMOIN (formule d'Abbott)
  # -------------------------------------------------------------------------
  # Le module tracait des courbes a partir d'une variable d'efficacite DEJA
  # calculee ailleurs. Ici, elle est calculee dans l'application : on choisit
  # la modalite temoin, et toutes les autres lui sont comparees en boucle.
  # =========================================================================
  eff_res <- shiny::reactiveVal(NULL)

  output$effFactorSelect <- shiny::renderUI({
    d <- values$filteredData
    shiny::validate(shiny::need(!is.null(d) && NROW(d), "Chargez d'abord un jeu de données."))
    cand <- names(d)[vapply(d, function(x)
      is.character(x) || is.factor(x) || length(unique(x[!is.na(x)])) <= 30,
      logical(1))]
    if (!length(cand)) cand <- names(d)
    shiny::selectInput(ns("effFactor"), shiny::tagList(shiny::icon("layer-group"), " Variable des traitements"),
                choices = cand, selected = shiny::isolate(input$effFactor) %||% cand[1])
  })

  output$effControlSelect <- shiny::renderUI({
    d <- values$filteredData
    shiny::req(d, input$effFactor)
    m <- hstat_eff_modalites(d, input$effFactor)
    shiny::validate(shiny::need(length(m) >= 2,
      "Cette variable ne porte pas au moins deux modalités : le témoin ne pourrait être comparé à rien."))
    shiny::selectInput(ns("effControl"), shiny::tagList(shiny::icon("vial"), " Modalité témoin"),
                choices = m, selected = shiny::isolate(input$effControl) %||% m[1])
  })

  # « Une fois le temoin choisi, les autres modalites passent dans une
  # variable » : on la MONTRE, pour que l'utilisateur verifie d'un coup d'oeil
  # sur quoi la boucle va porter.
  output$effOtherLevels <- shiny::renderUI({
    d <- values$filteredData
    shiny::req(d, input$effFactor, input$effControl)
    autres <- hstat_eff_modalites(d, input$effFactor, input$effControl)
    if (!length(autres))
      return(shiny::div(class = "alert alert-warning", style = "padding:8px;",
                 shiny::icon("triangle-exclamation"),
                 " Aucune autre modalité à comparer au témoin."))
    shiny::div(style = "background-color:#eef7fb;padding:9px 12px;border-radius:6px;border-left:4px solid #3c8dbc;font-size:12px;margin-bottom:12px;",
        shiny::icon("arrow-right-arrow-left", style = "color:#3c8dbc;"),
        trf(" %d modalité(s) seront comparées au témoin : ", length(autres)),
        shiny::tags$b(paste(autres, collapse = ", ")))
  })

  output$effResponseSelect <- shiny::renderUI({
    d <- values$filteredData
    shiny::req(d)
    num <- names(d)[vapply(d, is.numeric, logical(1))]
    shiny::validate(shiny::need(length(num) > 0, "Aucune variable numérique à mesurer."))
    shiny::selectInput(ns("effResponse"), shiny::tagList(shiny::icon("ruler"), " Variable(s) mesurée(s)"),
                choices = num, selected = shiny::isolate(input$effResponse) %||% num[1],
                multiple = TRUE)
  })

  output$effGroupSelect <- shiny::renderUI({
    d <- values$filteredData
    shiny::req(d, input$effFactor)
    cand <- setdiff(names(d), input$effFactor)
    shiny::selectInput(ns("effGroup"), shiny::tagList(shiny::icon("object-group"), " Variable de répétition"),
                choices = c("(aucune répétition déclarée)" = "", cand),
                selected = shiny::isolate(input$effGroup) %||% "")
  })

  shiny::observeEvent(input$effCompute, {
    d <- values$filteredData
    if (is.null(d) || !NROW(d)) {
      shiny::showNotification("Chargez d'abord un jeu de données.", type = "warning")
      return()
    }
    r <- tryCatch(
      hstat_efficacite(d, input$effFactor, input$effResponse, input$effControl,
                       agg = input$effAgg %||% "moyenne",
                       var_repetition = input$effGroup,
                       mode = input$effMode %||% "cumul"),
      error = function(e) {
        shiny::showNotification(hstat_err_fr(e), type = "error", duration = 8)
        NULL
      })
    if (is.null(r)) return()
    eff_res(r)
    if (!NROW(r)) {
      shiny::showNotification(attr(r, "message") %||% "Calcul impossible.",
                       type = "warning", duration = 8)
      return()
    }
    shiny::showNotification(shiny::tagList(shiny::icon("check"), " ", attr(r, "message")),
                     type = "message", duration = 5)
    hstat_ai_capture(values, "Seuils d'efficacité",
      "Efficacités calculées depuis le témoin (formule d'Abbott)",
      tables = list("Efficacités" = utils::head(as.data.frame(r), 200)),
      text = attr(r, "message"),
      meta = list(temoin = attr(r, "temoin"), resume = attr(r, "agg"),
                  variables = paste(input$effResponse, collapse = ", ")))
  })

  output$effMessage <- shiny::renderUI({
    r <- eff_res()
    if (is.null(r))
      return(shiny::div(class = "callout callout-info", style = "padding:8px 12px;",
                 shiny::icon("circle-info"),
                 " Choisissez le témoin puis cliquez sur « Calculer ». Le tableau",
                 " apparaît en bas de page."))
    m <- attr(r, "message") %||% ""
    alerte <- grepl("^Attention", m)
    shiny::div(class = if (alerte) "callout callout-warning" else "callout callout-success",
        style = "padding:8px 12px;",
        shiny::icon(if (alerte) "triangle-exclamation" else "circle-check"), " ", m)
  })

  output$effResume <- shiny::renderUI({
    r <- eff_res()
    if (is.null(r) || !NROW(r))
      return(shiny::p(style = "color:#999;font-style:italic;",
               "Aucun calcul pour l'instant : rendez-vous dans « Calcul depuis un témoin »."))
    ind <- sum(!is.finite(r$Efficacite))
    shiny::tagList(
      shiny::p(shiny::tags$b(trf("%d ligne(s)", NROW(r))),
        trf(" — témoin « %s », valeur résumée : %s.",
            attr(r, "temoin") %||% "", attr(r, "agg") %||% "")),
      if (ind > 0)
        shiny::div(class = "alert alert-warning", style = "padding:8px;",
            shiny::icon("triangle-exclamation"),
            trf(" %d efficacité(s) non calculable(s) : le témoin y vaut zéro ou n'a aucune valeur mesurable.", ind))
      else NULL)
  })

  # Une colonne d'efficacite par variable mesuree. Sans cela, quinze variables
  # sur onze modalites faisaient 165 lignes portant toutes la meme colonne
  # « Efficacite » : le selecteur Y n'avait qu'un choix, et le graphique
  # superposait quinze series sur onze positions.
  eff_large <- shiny::reactive({
    r <- eff_res()
    if (is.null(r) || !NROW(r)) return(NULL)
    hstat_eff_large(r)
  })

  # Tableau effectivement montre et telecharge : celui que l'utilisateur a sous
  # les yeux. Deux boutons qui n'exportent pas ce qui est affiche seraient un
  # piege.
  eff_affiche <- shiny::reactive({
    if (identical(input$effPresentation %||% "large", "long")) eff_res()
    else eff_large()
  })

  output$effTable <- DT::renderDT({
    r <- eff_affiche()
    shiny::req(r, NROW(r) > 0)
    x <- as.data.frame(r)
    num <- names(x)[vapply(x, is.numeric, logical(1))]
    for (k in num) x[[k]] <- round(x[[k]], 2)
    DT::datatable(x, rownames = FALSE, extensions = "Buttons",
              options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel")),
              class = "cell-border stripe hover")
  })

  output$effDownloadCsv <- shiny::downloadHandler(
    filename = function() paste0("efficacites_", Sys.Date(), ".csv"),
    content = function(file) {
      r <- eff_affiche(); shiny::req(r)
      utils::write.csv(as.data.frame(r), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    })

  output$effDownloadXlsx <- shiny::downloadHandler(
    filename = function() paste0("efficacites_", Sys.Date(), ".xlsx"),
    content = function(file) {
      r <- eff_affiche(); shiny::req(r)
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        # Les deux lectures dans le meme classeur : le tableau large pour
        # travailler, le detail pour verifier d'ou vient chaque pourcentage.
        feuilles <- list(Efficacites = as.data.frame(r))
        det <- eff_res()
        if (!is.null(det) && !identical(NCOL(det), NCOL(r)))
          feuilles[["Détail"]] <- as.data.frame(det)
        openxlsx::write.xlsx(feuilles, file)
      } else
        utils::write.csv(as.data.frame(r), file, row.names = FALSE,
                         fileEncoding = "UTF-8")
    })

  # « L'utilisateur doit pouvoir selectionner ce dataframe pour les autres
  # operations » : le tableau devient le jeu de travail. On le dit clairement
  # -- remplacer les donnees sans prevenir serait le pire des services.
  shiny::observeEvent(input$effUseAsData, {
    # Le tableau LARGE : c'est celui qui porte une variable par colonne, donc
    # le seul directement analysable par les autres onglets.
    r <- eff_large()
    if (is.null(r) || !NROW(r)) {
      shiny::showNotification("Calculez d'abord les efficacités.", type = "warning")
      return()
    }
    x <- as.data.frame(r)
    attributes(x) <- attributes(x)[c("names", "class", "row.names")]
    values$data <- x
    values$cleanData <- x
    values$filteredData <- x
    shiny::showNotification(
      shiny::tagList(shiny::icon("check"),
              trf(" Jeu de données remplacé par le tableau des efficacités (%d lignes, %d colonnes). Les autres onglets travaillent maintenant dessus.",
                  NROW(x), NCOL(x))),
      type = "message", duration = 8)
  })

  # Source du graphique. Le tableau d'efficacites est utilisable SANS remplacer
  # le jeu de travail : le fichier d'origine reste disponible pour le reste de
  # l'application.
  source_data <- shiny::reactive({
    if (identical(input$thresholdSource %||% "donnees", "calcul")) {
      # Large : le selecteur « Variable Y » doit lister les variables mesurees,
      # pas une unique colonne « Efficacite » ou quinze series se superposent.
      r <- eff_large()
      shiny::validate(shiny::need(!is.null(r) && NROW(r) > 0,
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

  output$thresholdSourceNote <- shiny::renderUI({
    if (!identical(input$thresholdSource %||% "donnees", "calcul")) return(NULL)
    r <- eff_large()
    if (is.null(r) || !NROW(r))
      return(shiny::div(class = "alert alert-warning", style = "padding:8px;",
                 shiny::icon("triangle-exclamation"),
                 " Aucune efficacité calculée : rendez-vous dans l'onglet",
                 " « Calcul depuis un témoin »."))
    vars <- attr(r, "variables")
    shiny::div(class = "alert alert-success", style = "padding:8px;",
        shiny::icon("circle-check"),
        trf(" Le graphique trace les efficacités calculées (%d modalité(s)). Le jeu de données chargé n'est pas modifié.",
            NROW(r)),
        if (length(vars) > 1)
          shiny::tagList(shiny::tags$br(),
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

  output$thresholdXVarSelect <- shiny::renderUI({
    shiny::req(source_data())
    all_cols <- names(source_data())
    shiny::selectInput(ns("thresholdXVar"), "Variable X (Traitements) :", 
                choices = all_cols,
                selected = .eff_defaut(all_cols, "Modalite"))
  })
  
  output$thresholdYVarSelect <- shiny::renderUI({
    shiny::req(source_data())
    num_cols <- names(source_data())[sapply(source_data(), is.numeric)]
    
    if(input$thresholdMultipleY) {
      pickerInput(ns("thresholdYVar"), "Variables Y (efficacité) — sélection multiple :", 
                  choices = num_cols,
                  selected = .eff_defaut(num_cols, "Efficacite"),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `selected-text-format` = "count > 2",
                                 `count-selected-text` = "{0} variables sélectionnées"))
    } else {
      shiny::selectInput(ns("thresholdYVar"), "Variable Y (Efficacité) :", 
                  choices = num_cols,
                  selected = .eff_defaut(num_cols, "Efficacite"))
    }
  })
  
  output$thresholdFilterSelect <- shiny::renderUI({
    shiny::req(source_data(), input$thresholdXVar)
    
    if(is.null(input$thresholdXVar)) return(NULL)
    
    x_data <- source_data()[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(x_data)
    } else {
      unique(as.character(x_data))
    }
    
    pickerInput(ns("thresholdFilter"), 
                "Exclure des traitements (optionnel) :",
                choices = unique_vals,
                multiple = TRUE,
                options = list(`actions-box` = TRUE))
  })
  
  # Éditeur de labels pour la variable X avec options de style
  output$thresholdLevelsEditor <- shiny::renderUI({
    shiny::req(source_data(), input$thresholdXVar)
    
    x_data <- source_data()[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(droplevels(x_data))
    } else {
      sort(unique(as.character(x_data)))
    }
    
    if(length(unique_vals) == 0) {
      return(shiny::p("Aucune valeur trouvée", style = "color: #999;"))
    }
    
    shiny::div(
      shiny::actionButton(ns("resetThresholdLabels"), "Réinitialiser", 
                   class = "btn-default btn-sm", icon = shiny::icon("undo"),
                   style = "margin-bottom: 10px;"),
      
      shiny::div(style = if(length(unique_vals) > 10) "max-height: 400px; overflow-y: auto;" else "",
          lapply(seq_along(unique_vals), function(i) {
            lvl <- unique_vals[i]
            input_id <- paste0("thresholdLevel_", make.names(lvl))
            bold_id <- paste0("thresholdLevelBold_", make.names(lvl))
            italic_id <- paste0("thresholdLevelItalic_", make.names(lvl))
            
            shiny::div(style = "margin-bottom: 10px; padding: 10px; background-color: #f5f5f5; border-radius: 4px; border-left: 4px solid #3498db;",
                shiny::div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
                    shiny::span(paste0(i, "."), style = "color: #3498db; font-weight: bold; min-width: 25px; font-size: 14px;"),
                    shiny::div(style = "flex: 1;",
                        shiny::div(style = "font-size: 11px; color: #666; margin-bottom: 3px; font-style: italic;",
                            paste("Original :", lvl)),
                        shiny::textInput(
                          inputId = ns(input_id),
                          label = NULL,
                          value = lvl,
                          placeholder = "Nouvelle étiquette...",
                          width = "100%"
                        )
                    )
                ),
                shiny::div(style = "display: flex; gap: 15px; padding-left: 35px; align-items: center;",
                    shiny::div(style = "display: flex; align-items: center; gap: 5px;",
                        shiny::checkboxInput(ns(bold_id), NULL, value = FALSE, width = "20px"),
                        shiny::tags$label(`for` = ns(bold_id), style = "margin: 0; font-weight: bold; cursor: pointer;", "Gras")
                    ),
                    shiny::div(style = "display: flex; align-items: center; gap: 5px;",
                        shiny::checkboxInput(ns(italic_id), NULL, value = FALSE, width = "20px"),
                        shiny::tags$label(`for` = ns(italic_id), style = "margin: 0; font-style: italic; cursor: pointer;", "Italique")
                    )
                )
            )
          })
      )
    )
  })
  
  # Éditeur de labels pour la légende (Variables Y multiples)
  output$thresholdLegendEditor <- shiny::renderUI({
    shiny::req(source_data(), input$thresholdXVar, input$thresholdYVar)

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
        return(shiny::div(style = "font-size:12px; color:#888; font-style:italic; padding:6px;",
                   shiny::icon("info-circle"),
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

    shiny::div(
      shiny::div(style = "font-size:12px; color:#666; font-style:italic; margin-bottom:8px;",
          shiny::icon("info-circle"), " ", note),
      shiny::actionButton(ns("resetThresholdLegendLabels"), "Réinitialiser", 
                   class = "btn-default btn-sm", icon = shiny::icon("undo"),
                   style = "margin-bottom: 10px;"),
      
      shiny::div(style = if(length(legend_items) > 10) "max-height: 400px; overflow-y: auto;" else "",
          lapply(seq_along(legend_items), function(i) {
            var_name <- legend_items[i]
            input_id <- paste0(id_prefix, make.names(var_name))
            bold_id <- paste0(bold_prefix, make.names(var_name))
            italic_id <- paste0(ital_prefix, make.names(var_name))
            
            shiny::div(style = "margin-bottom: 10px; padding: 10px; background-color: #f5f5f5; border-radius: 4px; border-left: 4px solid #9b59b6;",
                shiny::div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
                    shiny::span(paste0(i, "."), style = "color: #9b59b6; font-weight: bold; min-width: 25px; font-size: 14px;"),
                    shiny::div(style = "flex: 1;",
                        shiny::div(style = "font-size: 11px; color: #666; margin-bottom: 3px; font-style: italic;",
                            paste("Original :", var_name)),
                        shiny::textInput(
                          inputId = ns(input_id),
                          label = NULL,
                          value = var_name,
                          placeholder = "Nouvelle étiquette...",
                          width = "100%"
                        )
                    )
                ),
                shiny::div(style = "display: flex; gap: 15px; padding-left: 35px; align-items: center;",
                    shiny::div(style = "display: flex; align-items: center; gap: 5px;",
                        shiny::checkboxInput(ns(bold_id), NULL, value = FALSE, width = "20px"),
                        shiny::tags$label(`for` = ns(bold_id), style = "margin: 0; font-weight: bold; cursor: pointer;", "Gras")
                    ),
                    shiny::div(style = "display: flex; align-items: center; gap: 5px;",
                        shiny::checkboxInput(ns(italic_id), NULL, value = FALSE, width = "20px"),
                        shiny::tags$label(`for` = ns(italic_id), style = "margin: 0; font-style: italic; cursor: pointer;", "Italique")
                    )
                )
            )
          })
      )
    )
  })
  
  shiny::observeEvent(input$resetThresholdLabels, {
    shiny::req(source_data(), input$thresholdXVar)
    
    x_data <- source_data()[[input$thresholdXVar]]
    unique_vals <- if(is.factor(x_data)) {
      levels(droplevels(x_data))
    } else {
      sort(unique(as.character(x_data)))
    }
    
    for(lvl in unique_vals) {
      shiny::updateTextInput(session, paste0("thresholdLevel_", make.names(lvl)), value = lvl)
      shiny::updateCheckboxInput(session, paste0("thresholdLevelBold_", make.names(lvl)), value = FALSE)
      shiny::updateCheckboxInput(session, paste0("thresholdLevelItalic_", make.names(lvl)), value = FALSE)
    }
    
    shiny::showNotification("Étiquettes X réinitialisées", type = "message", duration = 2)
  })
  
  shiny::observeEvent(input$resetThresholdLegendLabels, {
    shiny::req(source_data(), input$thresholdXVar)
    multiple_y <- isTRUE(input$thresholdMultipleY) && length(input$thresholdYVar) > 1

    if (multiple_y) {
      for(var_name in input$thresholdYVar) {
        shiny::updateTextInput(session, paste0("thresholdLegendLevel_", make.names(var_name)), value = var_name)
        shiny::updateCheckboxInput(session, paste0("thresholdLegendLevelBold_", make.names(var_name)), value = FALSE)
        shiny::updateCheckboxInput(session, paste0("thresholdLegendLevelItalic_", make.names(var_name)), value = FALSE)
      }
    } else {
      x_data <- source_data()[[input$thresholdXVar]]
      items <- if (is.factor(x_data)) levels(droplevels(x_data))
               else sort(unique(as.character(x_data)))
      for(it in items) {
        shiny::updateTextInput(session, paste0("thresholdLegendItem_", make.names(it)), value = it)
        shiny::updateCheckboxInput(session, paste0("thresholdLegendItemBold_", make.names(it)), value = FALSE)
        shiny::updateCheckboxInput(session, paste0("thresholdLegendItemItalic_", make.names(it)), value = FALSE)
      }
    }
    
    shiny::showNotification("Étiquettes de légende réinitialisées", type = "message", duration = 2)
  })
  
  # Color pickers personnalisés pour les traitements (une seule variable Y)
  output$thresholdColorPickers <- shiny::renderUI({
    shiny::req(source_data(), input$thresholdXVar)
    shiny::req(!input$thresholdMultipleY || length(input$thresholdYVar) == 1)
    
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
    
    shiny::div(
      lapply(seq_along(unique_vals), function(i) {
        colourInput(ns(paste0("thresholdCustomColor_", i)), 
                    paste("Couleur", unique_vals[i], ":"),
                    value = default_colors[i],
                    showColour = "background")
      })
    )
  })
  
  shiny::observe({
    shiny::req(source_data(), input$thresholdXVar, input$thresholdYVar)
    
    tryCatch({
      if(length(input$thresholdYVar) == 1) {
        plot_data <- source_data()[, c(input$thresholdXVar, input$thresholdYVar)]
        colnames(plot_data) <- c("Treatment", "Efficacy")
        plot_data <- stats::na.omit(plot_data)
        
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
        plot_data <- stats::na.omit(plot_data)
        
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
  
  threshold_plot_reactive <- shiny::reactive({
    shiny::req(threshold_values$data_prepared)
    shiny::req(threshold_values$plot_data)
    
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
    input$thresholdTitreRetour
    input$thresholdTitreAlign
    input$thresholdAxisTextSize
    input$thresholdLegendSize
    input$thresholdYMin
    input$thresholdYMax
    input$thresholdZeroLine
    input$thresholdShowLegend
    input$thresholdLegendPosition
    input$thresholdLegendTitle
    input$thresholdLegendBold
    input$thresholdLegendItalic
    input$thresholdBarWidth
    input$thresholdBarSpacing
    input$thresholdBarPosition
    input$thresholdTheme
    input$thresholdSubtitle
    input$thresholdTitleStyle
    input$thresholdTitlePosition
    input$thresholdSubtitleStyle
    input$thresholdSubtitlePosition
    input$thresholdAxisTextXStyle
    input$thresholdAxisTextYStyle
    input$thresholdShowValues
    input$thresholdValueDigits
    input$thresholdValueSize
    input$thresholdValueStyle
    input$thresholdValuePosition
    input$thresholdValueColor
    input$thresholdBarAlpha
    input$thresholdBarBorder
    input$thresholdBarBorderColor
    input$thresholdBarBorderWidth
    input$thresholdLegendTextSize
    input$thresholdValueLabelSize
    input$thresholdYBreakStep
    input$thresholdShowLabel
    input$thresholdLabelPos
    input$thresholdLabelStyle
    
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
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Treatment, y = Efficacy, fill = Variable))
        
        bar_width <- (input$thresholdBarWidth %||% 0.8)
        dodge_width <- bar_width + (input$thresholdBarSpacing %||% 0.1)
        
        position <- if(!is.null(input$thresholdBarPosition) && input$thresholdBarPosition == "stack") {
          "stack"
        } else {
          ggplot2::position_dodge(width = dodge_width)
        }
        
        p <- p + do.call(ggplot2::geom_col, c(list(position = position, width = bar_width, na.rm = TRUE),
                                     hstat_barre_style(input$thresholdBarAlpha,
                                                       input$thresholdBarBorder,
                                                       input$thresholdBarBorderColor,
                                                       input$thresholdBarBorderWidth)))
        
        p <- p + ggplot2::labs(fill = input$thresholdLegendTitle %||% "Variables")
        
      } else {
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Treatment, y = Efficacy))
        
        bar_width <- input$thresholdBarWidth %||% 0.8
        sty_barre <- hstat_barre_style(input$thresholdBarAlpha,
                                       input$thresholdBarBorder,
                                       input$thresholdBarBorderColor,
                                       input$thresholdBarBorderWidth)

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
            p <- p + do.call(ggplot2::geom_col, c(list(mapping = ggplot2::aes(fill = Treatment), width = bar_width, na.rm = TRUE), sty_barre)) +
              ggplot2::scale_fill_discrete(name = input$thresholdLegendTitle %||% "Traitements",
                                  labels = legend_labels)
          } else if(input$thresholdBarColor == "palette") {
            p <- p + do.call(ggplot2::geom_col, c(list(mapping = ggplot2::aes(fill = Treatment), width = bar_width, na.rm = TRUE), sty_barre))
            # `name` et `labels` voyagent jusqu'a l'echelle : c'est ce qui
            # permet a l'aide commune de servir un site qui renomme sa legende.
            for (sc in hstat_scales_palette(input$thresholdPalette %||% unname(HSTAT_PALETTE_GG),
                                            colour = FALSE,
                                            name = input$thresholdLegendTitle %||% "Traitements",
                                            labels = legend_labels))
              p <- p + sc
          } else if(input$thresholdBarColor == "custom") {
            custom_colors <- sapply(seq_along(levels(plot_data$Treatment)), function(i) {
              color_input <- input[[paste0("thresholdCustomColor_", i)]]
              if(is.null(color_input)) scales::hue_pal()(length(levels(plot_data$Treatment)))[i] else color_input
            })
            p <- p + do.call(ggplot2::geom_col, c(list(mapping = ggplot2::aes(fill = Treatment), width = bar_width, na.rm = TRUE), sty_barre)) +
              ggplot2::scale_fill_manual(values = custom_colors,
                                name = input$thresholdLegendTitle %||% "Traitements",
                                labels = legend_labels)
          } else if(input$thresholdBarColor == "black") {
            p <- p + do.call(ggplot2::geom_col, c(list(fill = "#000000", width = bar_width, na.rm = TRUE), sty_barre))
          } else if(input$thresholdBarColor == "single") {
            p <- p + do.call(ggplot2::geom_col, c(list(fill = input$thresholdSingleBarColor %||% "#3498db", na.rm = TRUE,
                                              width = bar_width), sty_barre))
          }
        } else {
          p <- p + do.call(ggplot2::geom_col, c(list(fill = "#3498db", width = bar_width, na.rm = TRUE), sty_barre))
        }
      }
      
      p <- p + ggplot2::geom_hline(yintercept = input$thresholdValue %||% 80, 
                          color = input$thresholdColor %||% "#e74c3c",
                          linewidth = input$thresholdLineWidth %||% 1.5,
                          linetype = input$thresholdLineType %||% "solid")
      
      if (isTRUE(input$thresholdShowLabel %||% TRUE)) {
        n_niv <- length(levels(plot_data$Treatment))
        x_lab <- switch(input$thresholdLabelPos %||% "droite",
                        "gauche" = max(1, n_niv * 0.15),
                        "centre" = (n_niv + 1) / 2,
                        n_niv * 0.9)
        p <- p + ggplot2::annotate("text",
                          x = x_lab,
                          y = (input$thresholdValue %||% 80) + 5,
                          label = paste("Seuil :", input$thresholdValue %||% 80, "%"),
                          color = input$thresholdColor %||% "#e74c3c",
                          fontface = input$thresholdLabelStyle %||% "bold",
                          size = input$thresholdValueLabelSize %||% 4)
      }

      # ---- Valeur portee sur chaque barre ----
      if (isTRUE(input$thresholdShowValues)) {
        dec     <- max(0, .hstat_num1(input$thresholdValueDigits, 1))
        empile  <- is_multiple_y &&
                   identical(input$thresholdBarPosition %||% "dodge", "stack")
        # Barres empilees : une etiquette posee a la valeur de la serie
        # mentirait, puisque les segments s'additionnent. On la place AU MILIEU
        # de son segment, seul endroit ou elle designe ce qu'elle annonce.
        pos <- if (empile) list(y = plot_data$Efficacy, vjust = rep(0.5, NROW(plot_data)))
               else hstat_valeur_pos(plot_data$Efficacy,
                                     input$thresholdValuePosition %||% "dessus")
        etiq <- plot_data
        etiq$.y_lab <- pos$y
        etiq$.vj    <- pos$vjust
        etiq$.txt   <- ifelse(is.finite(plot_data$Efficacy),
                              formatC(plot_data$Efficacy, format = "f", digits = dec), "")
        map_lab <- if (is_multiple_y)
          ggplot2::aes(x = Treatment, y = .data$.y_lab, label = .data$.txt,
              vjust = .data$.vj, group = Variable)
        else
          ggplot2::aes(x = Treatment, y = .data$.y_lab, label = .data$.txt, vjust = .data$.vj)
        pos_lab <- if (empile) ggplot2::position_stack(vjust = 0.5)
                   else if (is_multiple_y)
                     ggplot2::position_dodge(width = (input$thresholdBarWidth %||% 0.8) +
                                            (input$thresholdBarSpacing %||% 0.1))
                   else "identity"
        p <- p + ggplot2::geom_text(data = etiq, mapping = map_lab, inherit.aes = FALSE,
                           size = .hstat_num1(input$thresholdValueSize, 4),
                           colour = input$thresholdValueColor %||% "#2c3e50",
                           fontface = input$thresholdValueStyle %||% "plain",
                           position = pos_lab, na.rm = TRUE)
      }
      
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
      
      extras <- hstat_plot_extras_lire(input, "threshold",
                                       familles = c("police", "cles", "marges"))
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
      
      # Graduations de l'axe Y : un pas demande, sinon le choix de ggplot.
      pas_y <- suppressWarnings(as.numeric(input$thresholdYBreakStep %||% NA)[1])
      # `NA` sur une borne = « automatique » pour ggplot : ce cote suit alors les
      # donnees. C'est le defaut, et c'est ce qui garantit qu'une efficacite
      # NEGATIVE -- la modalite fait moins bien que le temoin, c'est un resultat
      # -- reste visible. L'ancien defaut 0-100 la faisait sortir du cadre.
      num1 <- function(x) {
        v <- suppressWarnings(as.numeric(x %||% NA)[1])
        if (isTRUE(is.finite(v))) v else NA_real_
      }
      y_min <- num1(input$thresholdYMin)
      y_max <- num1(input$thresholdYMax)
      # Etendue REELLE de l'axe, bornes automatiques resolues : elle sert aux
      # graduations. Zero y est toujours inclus -- c'est la reference de la
      # formule d'Abbott, un cadre qui l'exclurait serait illisible.
      obs <- plot_data$Efficacy[is.finite(plot_data$Efficacy)]
      # Le SEUIL entre dans l'etendue : c'est la ligne que ce module existe pour
      # montrer. Sans lui, un seuil a 80 sur des efficacites allant de -60 a 45
      # etait trace hors des graduations -- la ligne apparaissait, mais aucune
      # graduation ne disait a quelle hauteur elle passait.
      etendue <- hstat_etendue_axe(obs, c(0, suppressWarnings(
        as.numeric(input$thresholdValue %||% NA)[1])))
      b_min <- if (is.na(y_min)) etendue[1] else y_min
      b_max <- if (is.na(y_max)) etendue[2] else y_max
      ech_y <- if (isTRUE(is.finite(pas_y)) && pas_y > 0 && is.finite(b_max - b_min))
        ggplot2::scale_y_continuous(limits = c(y_min, y_max),
                           breaks = seq(hstat_pas_debut(b_min, pas_y), b_max, by = pas_y))
      else ggplot2::scale_y_continuous(limits = c(y_min, y_max))

      # UNE EFFICACITE INDETERMINABLE NE TRACE AUCUNE BARRE, et rien ne le
      # disait : l'axe gardait la place de la modalite, vide, et le seul
      # signal partait dans la console de R (« Removed 1 row containing
      # missing values »). L'utilisateur voyait un trou. La formule d'Abbott
      # rend NA des que le temoin vaut zero -- c'est un cas normal, il se
      # nomme.
      absentes <- !is.finite(plot_data$Efficacy)
      if (any(absentes)) {
        qui <- unique(as.character(plot_data$Treatment[absentes]))
        shiny::showNotification(
          trf("%d modalité(s) sans efficacité calculable : aucune barre n'est tracée pour %s. Vérifiez le témoin (une valeur nulle rend la formule indéfinie) et les valeurs manquantes.",
              length(qui), paste(utils::head(qui, 5), collapse = ", ")),
          type = "warning", duration = 10, id = session$ns("seuilSansValeur"))
      }

      # Une barre hors des limites de l'axe DISPARAIT, avec son etiquette. Le
      # decompte ne porte que sur les bornes REELLEMENT fixees : une borne
      # automatique ne peut, par construction, rien exclure.
      hors <- sum(is.finite(plot_data$Efficacy) &
                  ((!is.na(y_min) & plot_data$Efficacy < y_min) |
                   (!is.na(y_max) & plot_data$Efficacy > y_max)))
      if (hors > 0)
        shiny::showNotification(
          trf("%d valeur(s) hors des limites de l'axe Y (%s à %s) : elles n'apparaissent pas. Élargissez les limites, ou videz les champs pour laisser l'axe suivre les données.",
              hors, if (is.na(y_min)) tr("auto") else y_min,
              if (is.na(y_max)) tr("auto") else y_max),
          type = "warning", duration = 8, id = session$ns("seuilHorsAxe"))

      # LA LIGNE DE SEUIL AUSSI PEUT SORTIR DU CADRE, et son absence est plus
      # trompeuse encore qu'une barre manquante : on croit lire un graphique
      # sans seuil alors qu'on en a demande un.
      seuil_v <- suppressWarnings(as.numeric(input$thresholdValue %||% NA)[1])
      if (isTRUE(is.finite(seuil_v)) &&
          ((!is.na(y_min) && seuil_v < y_min) || (!is.na(y_max) && seuil_v > y_max)))
        shiny::showNotification(
          trf("La ligne de seuil (%s) est hors des limites de l'axe Y : elle n'apparaît pas. Élargissez les limites, ou videz les champs.",
              seuil_v),
          type = "warning", duration = 8, id = session$ns("seuilLigneHorsAxe"))

      # Ligne de reference a zero : sans elle, une barre negative se lit comme
      # une barre courte vers le bas sans qu'on voie ou passe la frontiere.
      if (isTRUE(input$thresholdZeroLine %||% TRUE) && any(obs < 0))
        p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.4,
                                     colour = "#374151")

      sous_titre <- input$thresholdSubtitle %||% ""
      hj <- function(x, defaut = 0.5) {
        v <- suppressWarnings(as.numeric(x %||% defaut)[1])
        if (isTRUE(is.finite(v))) v else defaut
      }

      p <- p + ggplot2::labs(title = plot_title, x = x_label, y = y_label,
                    subtitle = if (nzchar(sous_titre)) sous_titre else NULL) +
        ech_y +
        viz_get_theme(input$thresholdTheme %||% "minimal",
                      base_size = extras$police) +
        ggplot2::theme(
          plot.title = ggtext::element_markdown(size = input$thresholdTitleSize %||% 16, 
                                        hjust = hj(input$thresholdTitlePosition),
                                        face = input$thresholdTitleStyle %||% "bold"),
          plot.subtitle = if (nzchar(sous_titre))
            ggtext::element_markdown(size = max(6, (input$thresholdTitleSize %||% 16) - 4),
                             hjust = hj(input$thresholdSubtitlePosition),
                             face = input$thresholdSubtitleStyle %||% "italic",
                             colour = "gray30")
          else ggplot2::element_blank(),
          axis.title.x = hstat_axe_titre_lire(input, "threshold",
                                              input$thresholdAxisTitleSize %||% 14,
                                              x_label_face, "x", colour = axis_color),
          axis.title.y = hstat_axe_titre_lire(input, "threshold",
                                              input$thresholdAxisTitleSize %||% 14,
                                              y_label_face, "y", colour = axis_color),
          axis.text.y = ggplot2::element_text(size = input$thresholdAxisTextSize %||% 12,
                                     color = axis_color,
                                     face = input$thresholdAxisTextYStyle %||% "plain"),
          axis.text.x = {
            ang <- input$thresholdLabelAngle %||% 45
            ggplot2::element_text(angle = ang,
                         hjust = if (ang == 0) 0.5 else 1,
                         vjust = if (ang == 0) 1 else 1,
                         color = axis_color,
                         face = input$thresholdAxisTextXStyle %||% "plain",
                         size = input$thresholdAxisTextSize %||% 12)
          },
          axis.line = if(input$thresholdShowAxisLines) {
            ggplot2::element_line(color = axis_color, linewidth = 0.5)
          } else {
            ggplot2::element_blank()
          },
          axis.ticks = if(input$thresholdShowTicks) {
            ggplot2::element_line(color = axis_color, linewidth = 0.5)
          } else {
            ggplot2::element_blank()
          },
          legend.position = if(show_legend) legend_position else "none",
          legend.justification = if(show_legend && is.numeric(legend_position)) legend_justification else NULL,
          legend.background = if(show_legend && is.numeric(legend_position)) {
            ggplot2::element_rect(fill = "white", color = "grey80", linewidth = 0.5)
          } else {
            ggplot2::element_blank()
          },
          legend.title = ggtext::element_markdown(size = input$thresholdLegendSize %||% 10, face = legend_title_face),
          legend.text = ggplot2::element_text(size = input$thresholdLegendTextSize %||%
                                            input$thresholdLegendSize %||% 10),
          panel.grid.major = if(input$thresholdShowGrid) {
            ggplot2::element_line(color = "grey90")
          } else {
            ggplot2::element_blank()
          },
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = if(input$thresholdShowAxisLines) {
            ggplot2::element_rect(color = axis_color, fill = NA, linewidth = 0.5)
          } else {
            ggplot2::element_blank()
          }
        )

      # Legende : on l'organise en une seule colonne pour que les longues
      # etiquettes (noms de traitements complets) restent lisibles, qu'elle soit
      # a droite/gauche (colonne verticale) ou en bas/haut (empilement vertical
      # qui evite de recouvrir les labels X inclines).
      if (show_legend) {
        p <- p + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, byrow = TRUE),
                        color = ggplot2::guide_legend(ncol = 1, byrow = TRUE))
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
        p <- p + ggplot2::scale_x_discrete(labels = styled_labels)
      }
      
      # LE KIT SE POSE EN DERNIER : un theme complet remplace tout ce qui
      # precede, si bien que pose avant celui du module il serait efface.
      p <- p + hstat_plot_extras_theme(extras)

      threshold_values$current_plot <- p
      
      return(p)
      
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur lors de la mise à jour"), type = "error", duration = 5)
      return(NULL)
    })
  })
  
  output$thresholdPlot <- renderPlotly({
    p <- threshold_plot_reactive()
    shiny::req(p)
    
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

    # LE SOUS-TITRE NE SURVIT PAS A ggplotly : la conversion le laisse
    # simplement tomber. L'utilisateur en saisissait un, ne voyait rien a
    # l'ecran, et le retrouvait dans le fichier telecharge -- soit le pire des
    # deux mondes. On le remet donc en seconde ligne du titre plotly. Le texte
    # vient de l'utilisateur : il est echappe avant d'entrer dans la balise.
    sous <- trimws(input$thresholdSubtitle %||% "")
    if (nzchar(sous)) {
      titre <- p$labels$title %||% ""
      hj <- suppressWarnings(as.numeric(input$thresholdTitlePosition %||% 0.5)[1])
      if (!isTRUE(is.finite(hj))) hj <- 0.5
      gp <- gp %>% layout(title = list(
        text = paste0("<b>", hstat_html_escape(titre), "</b><br><sup>",
                      hstat_html_escape(sous), "</sup>"),
        x = hj, xanchor = if (hj < 0.25) "left" else if (hj > 0.75) "right" else "center"))
    }

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
  
  output$thresholdDataTable <- DT::renderDT({
    shiny::req(threshold_values$plot_data)
    
    DT::datatable(threshold_values$plot_data,
              options = list(
                pageLength = 10, 
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              class = 'cell-border stripe hover',
              caption = shiny::tags$caption(
                style = 'caption-side: top; text-align: center; color: #3c8dbc; font-size: 16px; font-weight: bold;',
                'Données utilisées pour le graphique'
              ))
  })
  
  output$downloadThresholdPlot <- shiny::downloadHandler(
    filename = function() {
      paste0("seuils_efficacite_", Sys.Date(), ".",
             hstat_img_fmt(input$thresholdExportFormat))
    },
    content = function(file) {
      # Les pixels saisis fixent la MISE EN PAGE (lue a 96 ppp, la reference de
      # l'ecran), le DPI la finesse du rendu. Voir hstat_export_dims().
      dims <- hstat_export_dims(input$thresholdExportWidth,
                                input$thresholdExportHeight,
                                input$thresholdExportDPI)
      if (!is.null(dims$note))
        shiny::showNotification(dims$note, type = "warning", duration = 8)

      # Un seul chemin d'ecriture, et il garantit un fichier VALIDE du format
      # demande. Les sept branches de ggsave qui vivaient ici pouvaient lever
      # sans rien ecrire : Shiny renvoyait alors sa page d'erreur HTML, que le
      # navigateur enregistrait sous le nom demande -- on croyait tenir un PNG,
      # on ouvrait du HTML.
      fmt <- hstat_img_fmt(input$thresholdExportFormat)
      ok  <- hstat_ecrire_image(file, threshold_values$current_plot, fmt,
                                dims$width_in, dims$height_in, dims$dpi)
      if (ok)
        shiny::showNotification(
          trf("Graphique exporté : %s, %s × %s pouces, %s DPI.",
              toupper(fmt), round(dims$width_in, 2), round(dims$height_in, 2),
              dims$dpi),
          type = "message", duration = 5)
      else
        shiny::showNotification(
          paste0("Export impossible : le fichier téléchargé porte le motif. ",
                 "Réduisez les dimensions ou le DPI, ou choisissez un format ",
                 "vectoriel (SVG, PDF)."),
          type = "error", duration = 10)
    }
  )

  # L'apercu annonce les DEUX tailles : la mise en page demandee et les pixels
  # reellement produits. Ne montrer que la premiere laissait croire qu'un DPI
  # plus eleve ne changeait rien au fichier.
  output$exportSizeEstimate <- shiny::renderText({
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
  
  output$downloadThresholdData <- shiny::downloadHandler(
    filename = function() {
      paste0("données_seuils_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      shiny::req(threshold_values$plot_data)
      
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
      
      shiny::showNotification("Données exportées avec succès !", type = "message", duration = 3)
    }
  )
  })
}
