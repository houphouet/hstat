#  Module Shiny : Nettoyage des donnees


mod_clean_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
              shiny::fluidRow(
                shinydashboard::box(
                  width = 12,
                  status = "warning",
                  solidHeader = FALSE,
                  background = "yellow",
                  shiny::h3(shiny::icon("broom"), "Nettoyage et Préparation des Données", style = "margin: 0;"),
                  shiny::p("Transformez, nettoyez et préparez vos données pour l'analyse", 
                    style = "margin: 5px 0 0 0; opacity: 0.9;")
                )
              ),
              
              # Étape 1: Types des variables
              shiny::fluidRow(
                shinydashboard::box(
                  title = shiny::tagList(
                    shiny::tags$span(
                      class = "badge bg-yellow",
                      style = "font-size: 14px; margin-right: 10px;",
                      "1"
                    ),
                    shiny::icon("cogs"), 
                    "Définition des Types de Variables"
                  ), 
                  status = "warning", 
                  width = 12, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  
                  shiny::fluidRow(
                    shiny::column(9,
                           shiny::div(
                             style = "max-height: 400px; overflow-y: auto; background-color: #fafafa; padding: 15px; border-radius: 5px;",
                             shiny::uiOutput(ns("varTypeUI"))
                           )
                    ),
                    shiny::column(3,
                           shiny::div(
                             style = "text-align: center; padding: 20px;",
                             shiny::actionButton(ns("applyTypes"),
                               shiny::tagList(shiny::icon("check-circle"), " Appliquer les Types"),
                               class = "btn-warning btn-lg btn-block",
                               style = "font-size: 16px; padding: 12px 20px;"
                             )
                           )
                    )
                  )
                )
              ),
              
              # Étape 2: Gestion des variables
              shiny::fluidRow(
                shinydashboard::box(
                  title = shiny::tagList(
                    shiny::tags$span(
                      class = "badge bg-red",
                      style = "font-size: 14px; margin-right: 10px;",
                      "3"
                    ),
                    shiny::icon("edit"),
                    "Gestion des Variables et Lignes"
                  ),
                  status = "danger",
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  shiny::tabsetPanel(
                    type = "tabs",
                    
                    # Onglet 1 : Supprimer variable
                    shiny::tabPanel(
                      title = shiny::tagList(shiny::icon("columns"), " Supprimer Variable"),
                      shiny::br(),
                      shiny::div(
                        style = "background-color: #ffebee; padding: 15px; border-radius: 5px;",
                        shiny::h5(shiny::icon("trash-alt"), "Supprimer une Variable", style = "color: #e74c3c; margin-top: 0;"),
                        shiny::uiOutput(ns("removeVarUI")),
                        shiny::actionButton(ns("removeVar"),
                          shiny::tagList(shiny::icon("trash"), " Supprimer"),
                          class = "btn-danger btn-block",
                          style = "margin-top: 10px;"
                        )
                      )
                    ),
                    
                    # Onglet 1b : Renommer une variable
                    shiny::tabPanel(
                      title = shiny::tagList(shiny::icon("i-cursor"), " Renommer Variable"),
                      shiny::br(),
                      shiny::div(
                        style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px;",
                        shiny::h5(shiny::icon("i-cursor"), "Renommer une Variable", style = "color: #1565c0; margin-top: 0;"),
                        shiny::tags$p(style = "font-size: 12px; color: #7f8c8d;",
                               "Modifiez le nom d'une colonne. Le nouveau nom doit être unique et non vide."),
                        shiny::uiOutput(ns("renameVarUI")),
                        shiny::textInput(ns("renameVarNew"), "Nouveau nom :", placeholder = "ex : Age_années"),
                        shiny::actionButton(ns("applyRenameVar"),
                          shiny::tagList(shiny::icon("check"), " Renommer"),
                          class = "btn-primary btn-block", style = "margin-top: 10px;"),
                        shiny::uiOutput(ns("renameVarStatus"))
                      )
                    ),

                    # Onglet 1c : Recoder classes (catégorielles / ordinales)
                    shiny::tabPanel(
                      title = shiny::tagList(shiny::icon("wand-magic-sparkles"), " Recoder Classes"),
                      shiny::br(),
                      shiny::div(
                        style = "background-color: #fff3e0; padding: 15px; border-radius: 5px;",
                        shiny::h5(shiny::icon("wand-magic-sparkles"), "Recoder une variable catégorielle ou ordinale",
                           style = "color: #e65100; margin-top: 0;"),
                        shiny::uiOutput(ns("recodeVarSelect")),
                        shiny::radioButtons(ns("recodeType"),
                          shiny::tagList(shiny::icon("tags"), " Nature de la variable"),
                          choices = c("Catégorielle (nominale)" = "nominal",
                                      "Ordinale (ordre des modalités)" = "ordinal"),
                          selected = "nominal"),
                        shiny::helpText(shiny::icon("info-circle"),
                          " Plus de 12 modalités : tableau éditable (ancienne = nouvelle). Sinon : un champ par modalité. En ordinal, l'ordre des lignes/champs définit l'ordre du facteur."),
                        shiny::uiOutput(ns("recodeInterface")),
                        shiny::div(style = "text-align:center; margin-top:10px;",
                          shiny::actionButton(ns("applyRecode"),
                            shiny::tagList(shiny::icon("check"), " Appliquer le recodage"),
                            class = "btn-warning btn-block")),
                        shiny::uiOutput(ns("recodeStatus"))
                      )
                    ),

                    # Onglet 2 : Supprimer lignes
                    shiny::tabPanel(
                      title = shiny::tagList(shiny::icon("trash-alt"), " Supprimer Lignes"),
                      shiny::br(),
                      shiny::tabsetPanel(
                        type = "pills",
                        
                        shiny::tabPanel(
                          title = shiny::tagList(shiny::icon("keyboard"), " Saisie"),
                          shiny::br(),
                          shiny::div(
                            style = "background-color: #ffebee; padding: 12px; border-radius: 5px;",
                            # Le contenu d'un <code> n'est jamais traduit par le
                            # navigateur : les exemples de syntaxe resteraient en
                            # francais. Le bloc entier passe donc par tr().
                            shiny::tags$p(style = "font-size: 11px; color: #7f8c8d; margin-bottom: 8px;",
                                   shiny::HTML(tr(paste0("Formats : <code>1,3,5</code> -- ",
                                                         "<code>10 à 20</code> -- ",
                                                         "<code>1,3,10 à 15</code>")))),
                            shiny::textAreaInput(ns("deleteRowsInput"), NULL,
                                          placeholder = "1,3,5
10 à 20
1,3,5,10 à 15",
                                          rows = 3, width = "100%"),
                            shiny::uiOutput(ns("deleteRowsPreview")),
                            shiny::actionButton(ns("applyDeleteRows"),
                                         shiny::tagList(shiny::icon("trash"), " Supprimer"),
                                         class = "btn-danger btn-block", style = "margin-top: 8px; font-weight: bold;")
                          )
                        ),
                        
                        shiny::tabPanel(
                          title = shiny::tagList(shiny::icon("mouse-pointer"), " Interactif"),
                          shiny::br(),
                          shiny::tags$p(style = "font-size: 11px; color: #555; margin-bottom: 8px;",
                                 shiny::icon("hand-pointer"), " Cliquez sur les lignes (Ctrl = multiple)"),
                          shiny::div(
                            style = "border: 1px solid #dee2e6; border-radius: 5px; overflow: hidden;",
                            DT::dataTableOutput(ns("deleteRowsTable"), height = "260px")
                          ),
                          shiny::br(),
                          shiny::uiOutput(ns("deleteRowsInteractivePreview")),
                          shiny::br(),
                          shiny::actionButton(ns("applyDeleteRowsInteractive"),
                                       shiny::tagList(shiny::icon("trash"), " Supprimer la sélection"),
                                       class = "btn-danger btn-block", style = "font-weight: bold;")
                        )
                      )
                    ),
                    
                    # Onglet 3 : Ajouter variable constante
                    shiny::tabPanel(
                      title = shiny::tagList(shiny::icon("plus-circle"), " Ajouter Variable"),
                      shiny::br(),
                      shiny::div(
                        style = "background-color: #e8f5e9; padding: 15px; border-radius: 5px;",
                        shiny::h5(shiny::icon("plus-circle"), "Ajouter une Variable Constante", style = "color: #27ae60; margin-top: 0;"),
                        shiny::tags$p(style = "font-size: 12px; color: #7f8c8d;",
                               "Crée une nouvelle colonne avec une valeur identique pour toutes les lignes"),
                        shiny::textInput(ns("newVarName"), "Nom :", placeholder = "ex: Catégorie"),
                        shiny::numericInput(ns("newVarValue"), "Valeur par défaut :", 0),
                        shiny::actionButton(ns("addVar"),
                                     shiny::tagList(shiny::icon("plus"), " Ajouter"),
                                     class = "btn-success btn-block")
                      )
                    )
                  )
                ),
                
                shinydashboard::box(
                  title = shiny::tagList(
                    shiny::tags$span(
                      class = "badge bg-blue",
                      style = "font-size: 14px; margin-right: 10px;",
                      "4"
                    ),
                    shiny::icon("calculator"), 
                    "Créer une Variable Calculée"
                  ), 
                  status = "primary", 
                  width = 6, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  shiny::div(
                    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                    
                    shiny::h5(shiny::icon("info-circle"), "Assistant de Formule", style = "color: #3498db; margin-top: 0;"),
                    shiny::p(style = "font-size: 12px; color: #7f8c8d;", 
                      "Créez des variables basées sur des calculs. Exemple : (Var1 + Var2) / 2"),
                    
                    shiny::textInput(ns("calcVarName"), 
                      "Nom de la variable calculée :",
                      placeholder = "ex: Moyenne_Score"
                    ),
                    
                    shiny::fluidRow(
                      shiny::column(6,
                             shiny::div(
                               style = "background-color: #eef7ff; padding: 10px; border-radius: 5px;",
                               shiny::h6(shiny::icon("columns"), " Colonnes", style = "margin-bottom: 6px; color: #2c5aa0;"),
                               shiny::uiOutput(ns("colPicker")),
                               shiny::tags$small(style = "color: #6c757d;",
                                          shiny::icon("info-circle"), " Cliquez pour insérer dans la formule")
                             )
                      ),
                      shiny::column(6,
                             shiny::div(
                               style = "background-color: #f0fff4; padding: 10px; border-radius: 5px;",
                               shiny::h6(shiny::icon("filter"), " Filtrer sur lignes (optionnel)", style = "margin-bottom: 6px; color: #1a6e2e;"),
                               shiny::uiOutput(ns("rowCondPicker")),
                               shiny::tags$small(style = "color: #6c757d;",
                                          shiny::icon("info-circle"), " Génère ifelse() dans la formule")
                             )
                      )
                    ),
                    
                    shiny::br(),
                    
                    shiny::fluidRow(
                      shiny::column(12,
                             shiny::div(
                               style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                               shiny::fluidRow(
                                 shiny::column(6,
                                        shiny::h6("Opérateurs :", style = "margin-bottom: 5px; font-size: 11px; color: #555;"),
                                        shiny::div(style = "display: flex; flex-wrap: wrap; gap: 4px;",
                                            shiny::actionButton(ns("insertPlus"),  "+",  class = "btn-outline-secondary btn-sm"),
                                            shiny::actionButton(ns("insertMoins"), "-",  class = "btn-outline-secondary btn-sm"),
                                            shiny::actionButton(ns("insertMult"),  "x",  class = "btn-outline-secondary btn-sm"),
                                            shiny::actionButton(ns("insertDiv"),   "÷",  class = "btn-outline-secondary btn-sm"),
                                            shiny::actionButton(ns("insertPow"),   "^",  class = "btn-outline-secondary btn-sm"),
                                            shiny::actionButton(ns("insertParen"), "()", class = "btn-outline-secondary btn-sm")
                                        )
                                 ),
                                 shiny::column(6,
                                        shiny::h6("Fonctions :", style = "margin-bottom: 5px; font-size: 11px; color: #555;"),
                                        shiny::div(style = "display: flex; flex-wrap: wrap; gap: 4px;",
                                            shiny::actionButton(ns("insertLog"),   "log()",    class = "btn-outline-info btn-sm"),
                                            shiny::actionButton(ns("insertLog10"), "log10()",  class = "btn-outline-info btn-sm"),
                                            shiny::actionButton(ns("insertSqrt"),  "sqrt()",   class = "btn-outline-info btn-sm"),
                                            shiny::actionButton(ns("insertAbs"),   "abs()",    class = "btn-outline-info btn-sm"),
                                            shiny::actionButton(ns("insertRound"), "round()",  class = "btn-outline-info btn-sm"),
                                            shiny::actionButton(ns("insertExp"),   "exp()",    class = "btn-outline-info btn-sm"),
                                            shiny::actionButton(ns("insertMean"),  "mean()",   class = "btn-outline-info btn-sm"),
                                            shiny::actionButton(ns("insertSum"),   "sum()",    class = "btn-outline-info btn-sm"),
                                            shiny::actionButton(ns("insertIfelse"),"ifelse()", class = "btn-outline-warning btn-sm"),
                                            shiny::actionButton(ns("insertIsNA"),  "is.na()",  class = "btn-outline-warning btn-sm")
                                        )
                                 )
                               )
                             )
                      )
                    ),
                    
                    shiny::textInput(ns("calcFormula"), 
                      "Formule de calcul :",
                      placeholder = "ex: (Rendement + Biomasse) / 2   |   sum(Poids, Hauteur)   |   sqrt(Var1 * Var2)"
                    ),
                    
                    shiny::tags$div(
                      style = "margin-top:4px; margin-bottom:12px; padding:10px 12px; background:#f0f7ff; border:1px solid #bee3f8; border-radius:6px; font-size:11.5px;",
                      shiny::tags$b(style="color:#1a56db; font-size:12px;", shiny::icon("calculator"), " Exemples d'utilisation :"),
                      shiny::tags$table(
                        style = "width:100%; margin-top:6px; border-collapse:collapse;",
                        shiny::tags$tr(style="background:#dbeafe;",
                                shiny::tags$td(colspan="2",style="padding:4px 8px;color:#1e40af;font-size:11px;font-weight:bold;",
                                        shiny::icon("layer-group")," Opérations sur plusieurs variables")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;color:#555;width:52%;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "(Rendement + Biomasse) / 2")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Moyenne de 2 variables")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "mean(c(Rendement, Biomasse, Poids))")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Moyenne (n variables)")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "Rendement + Biomasse + Poids")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Somme de variables")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "Rendement / (Biomasse + Poids)")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Ratio entre variables")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "sqrt(Rendement * Biomasse)")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Moyenne géométrique")
                        ),
                        shiny::tags$tr(style="background:#dcfce7;",
                                shiny::tags$td(colspan="2",style="padding:4px 8px;color:#166534;font-size:11px;font-weight:bold;",
                                        shiny::icon("calculator")," Transformations classiques")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "log(Rendement)")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Logarithme naturel")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "log10(Rendement + 1)")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Log10 (si zéros présents)")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "sqrt(Rendement)")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Racine carrée")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "Rendement^2")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Mise au carré")
                        ),
                        shiny::tags$tr(style="border-bottom:1px solid #d0e8ff;",
                                shiny::tags$td(style="padding:3px 6px;",
                                        shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                                  "round(Rendement / Biomasse, 3)")),
                                shiny::tags$td(style="padding:3px 6px;color:#666;","-> Ratio arrondi à 3 déc.")
                        ),
                        shiny::tags$tr(
                          shiny::tags$td(style="padding:3px 6px;",
                                  shiny::tags$code(style="background:#e8f0fe;padding:1px 4px;border-radius:3px;",
                                            "(Rendement - mean(Rendement)) / sd(Rendement)")),
                          shiny::tags$td(style="padding:3px 6px;color:#666;","-> Z-score")
                        )
                      )
                    ),
                    
                    shiny::actionButton(ns("addCalcVar"), 
                      shiny::tagList(shiny::icon("calculator"), " Créer Variable Calculée"), 
                      class = "btn-primary btn-block btn-lg"
                    )
                  ),
                  
                  footer = shiny::div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    shiny::icon("lightbulb"), 
                    " Astuce : Cliquez sur une colonne pour l'insérer dans la formule"
                  )
                )
              ),
              
              # Étape 4: Valeurs manquantes
              shiny::fluidRow(
                shinydashboard::box(
                  title = shiny::tagList(
                    shiny::tags$span(
                      class = "badge bg-aqua",
                      style = "font-size: 14px; margin-right: 10px;",
                      "2"
                    ),
                    shiny::icon("band-aid"), 
                    "Traitement des Valeurs Manquantes"
                  ), 
                  status = "info", 
                  width = 12, 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  shiny::fluidRow(
                    shiny::column(8,
                           shiny::div(
                             style = "background-color: #e1f5fe; padding: 15px; border-radius: 5px;",

                             shiny::h5(shiny::icon("percent"), "Pourcentage de valeurs manquantes", style = "color: #3498db; margin-top: 0;"),
                             DT::DTOutput(ns("naSummaryTable")),
                             shiny::div(style = "margin: 10px 0; padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
                                 shiny::uiOutput(ns("naRecommendation"))),

                             shiny::uiOutput(ns("naVarSelect")),

                             shiny::h5(shiny::icon("tools"), "Méthode de Traitement", style = "color: #3498db; margin-top: 20px;"),

                             shiny::radioButtons(ns("naMethod"), 
                               NULL,
                               choices = c(
                                 "Supprimer les lignes contenant des NA" = "remove", 
                                 "Remplacer par la moyenne (numériques)" = "mean",
                                 "Remplacer par la médiane (numériques)" = "median",
                                 "Remplacer par le mode (catégorielles)" = "mode",
                                 "Imputation par k plus proches voisins (KNN)" = "knn",
                                 "Imputation multiple par équations chaînées (MICE/PMM)" = "mice",
                                 "Imputation par forets aléatoires (missForest)" = "rf",
                                 "Remplacer par une valeur spécifique" = "value"
                               ),
                               selected = "remove"
                             ),

                             shiny::conditionalPanel(
              ns = ns,
                               condition = "input.naMethod == 'knn'",
                               shiny::numericInput(ns("naKnnK"), "Nombre de voisins (k) :", value = 5, min = 1, max = 30)),
                             shiny::conditionalPanel(
              ns = ns,
                               condition = "input.naMethod == 'mice'",
                               shiny::numericInput(ns("naMiceM"), "Nombre d'imputations (m) :", value = 5, min = 1, max = 20),
                               shiny::p(style="font-size:11px;color:#666;font-style:italic;",
                                 "PMM (prédictive mean matching) : robuste, conserve la distribution. Les m jeux sont agrégés par la moyenne.")),

                             shiny::conditionalPanel(
              ns = ns,
                               condition = "input.naMethod == 'value'",
                               shiny::div(
                                 style = "margin-top: 15px; padding: 10px; background-color: white; border-radius: 5px;",
                                 shiny::numericInput(ns("naValue"), 
                                   "Valeur de remplacement :", 
                                   0
                                 )
                               )
                             )
                           )
                    ),
                    shiny::column(4,
                           shiny::div(
                             style = "background-color: #fff; padding: 20px; border-radius: 5px; border: 2px solid #3498db;",
                             shiny::h5(shiny::icon("exclamation-triangle"), "Guide des méthodes", style = "color: #e74c3c; margin-top: 0;"),
                             shiny::tags$ul(
                               style = "font-size: 12px; color: #7f8c8d;",
                               shiny::tags$li(shiny::tags$b("Suppression"), " : si < 5% de NA et perte de lignes acceptable."),
                               shiny::tags$li(shiny::tags$b("Moyenne/médiane"), " : rapide, mais sous-estimé la variance."),
                               shiny::tags$li(shiny::tags$b("KNN"), " : exploite la similarité entre observations."),
                               shiny::tags$li(shiny::tags$b("MICE/PMM"), " : imputation multiple, recommandée pour NA non négligeables (MAR)."),
                               shiny::tags$li(shiny::tags$b("missForest"), " : non paramétrique, gère mixte numérique/catégoriel.")
                             ),
                             shiny::hr(),
                             shiny::actionButton(ns("applyNA"), 
                               shiny::tagList(shiny::icon("magic"), " Appliquer le Traitement"), 
                               class = "btn-info btn-block btn-lg",
                               style = "margin-top: 10px;"
                             )
                           )
                    )
                  )
                )
              ),

              # Étape 5: Valeurs aberrantes (outliers) et winsorisation
              shiny::fluidRow(
                shinydashboard::box(
                  title = shiny::tagList(
                    shiny::tags$span(class = "badge bg-yellow",
                              style = "font-size: 14px; margin-right: 10px;", "5"),
                    shiny::icon("crosshairs"),
                    "Valeurs Aberrantes et Winsorisation"
                  ),
                  status = "primary", width = 12, solidHeader = TRUE,
                  collapsible = TRUE, collapsed = TRUE,
                  shiny::fluidRow(
                    shiny::column(6,
                      shiny::div(style = "background-color:#e8f6f3;padding:15px;border-radius:5px;",
                        shiny::uiOutput(ns("outlierVarSelect")),
                        shiny::h5(shiny::icon("search"), " Méthode de détection", style = "color:#16a085;margin-top:15px;"),
                        shiny::radioButtons(ns("outlierMethod"), NULL,
                          choices = c(
                            "Écart interquartile (IQR, k x 1,5)" = "iqr",
                            "Score Z (|z| > seuil)" = "zscore",
                            "Score Z robuste (MAD)" = "mad"),
                          selected = "iqr"),
                        shiny::conditionalPanel(ns = ns,
                          condition = "input.outlierMethod == 'iqr'",
                          shiny::numericInput(ns("outlierIqrK"), "Coefficient IQR (k) :", value = 1.5, min = 0.5, max = 5, step = 0.1)),
                        shiny::conditionalPanel(ns = ns,
                          condition = "input.outlierMethod == 'zscore' || input.outlierMethod == 'mad'",
                          shiny::numericInput(ns("outlierZThresh"), "Seuil (|z|) :", value = 3, min = 1, max = 6, step = 0.5)),
                        shiny::hr(),
                        shiny::h5(shiny::icon("compress-arrows-alt"), " Traitement", style = "color:#16a085;"),
                        shiny::radioButtons(ns("outlierAction"), NULL,
                          choices = c(
                            "Seulement détecter (aucune modification)" = "detect",
                            "Winsoriser (ramener aux quartiles Q1/Q3)" = "winsor",
                            "Remplacer par NA" = "tona",
                            "Supprimer les lignes aberrantes" = "remove"),
                          selected = "detect"),
                        shiny::conditionalPanel(ns = ns,
                          condition = "input.outlierAction == 'winsor'",
                          shiny::p(style="font-size:11px;color:#666;font-style:italic;",
                            "Winsorisation aux quartiles : les valeurs sous le 25e percentile (Q1) sont ramenées à Q1, celles au-dessus du 75e percentile (Q3) à Q3.")),
                        shiny::div(style = "margin-top:12px;",
                          shiny::actionButton(ns("detectOutliers"), shiny::tagList(shiny::icon("search"), " Détecter"),
                                       class = "btn-default"),
                          shiny::actionButton(ns("applyOutliers"), shiny::tagList(shiny::icon("magic"), " Appliquer le traitement"),
                                       class = "btn-primary"))
                      )
                    ),
                    shiny::column(6,
                      shiny::div(style = "background-color:#fff;padding:15px;border-radius:5px;border:2px solid #16a085;",
                        shiny::h5(shiny::icon("info-circle"), " Résumé de détection", style="color:#16a085;margin-top:0;"),
                        shiny::uiOutput(ns("outlierSummary")),
                        # Sortie DEDIEE : `renderTable()` appele depuis un
                        # renderUI ne produit qu'un conteneur vide, jamais
                        # alimente. Le tableau des bornes ne s'affichait donc
                        # jamais -- et la note en dessous expliquait des
                        # « Bornes basse/haute » absentes de l'ecran.
                        shiny::div(style = "overflow-x:auto; max-width:100%;",
                            shiny::tableOutput(ns("outlierTable"))),
                        shiny::uiOutput(ns("outlierNote")),
                        shiny::hr(),
                        shiny::tags$ul(style="font-size:12px;color:#7f8c8d;",
                          shiny::tags$li(shiny::tags$b("IQR"), " : robuste, standard pour distributions asymétriques."),
                          shiny::tags$li(shiny::tags$b("Z"), " : suppose une distribution ~normale."),
                          shiny::tags$li(shiny::tags$b("MAD"), " : version robuste du Z, résiste aux extrêmes."),
                          shiny::tags$li(shiny::tags$b("Winsoriser"), " : conserve les lignes en limitant l'influence des extrêmes."))
                      )
                    )
                  )
                )
              ),

              # Étape 6: Classes d'intervalles (discrétisation)
              shiny::fluidRow(
                shinydashboard::box(
                  title = shiny::tagList(
                    shiny::tags$span(class = "badge bg-purple",
                              style = "font-size: 14px; margin-right: 10px;", "6"),
                    shiny::icon("layer-group"),
                    "Classes d'intervalles (discrétisation)"
                  ),
                  status = "primary", width = 12, solidHeader = TRUE,
                  collapsible = TRUE, collapsed = TRUE,
                  shiny::fluidRow(
                    shiny::column(6,
                      shiny::div(style = "background-color:#f4ecf7;padding:15px;border-radius:5px;",
                        shiny::uiOutput(ns("cutVarSelect")),
                        shiny::radioButtons(ns("cutMethod"), shiny::tagList(shiny::icon("ruler"), " Méthode de découpage"),
                          choiceNames = list(
                            shiny::HTML("<b>Largeur égale</b> <small style='color:#7f8c8d;'>(intervalles de même amplitude)</small>"),
                            shiny::HTML("<b>Effectifs égaux</b> <small style='color:#7f8c8d;'>(quantiles)</small>"),
                            shiny::HTML("<b>Bornes personnalisées</b> <small style='color:#7f8c8d;'>(ex. classes d'âge)</small>")),
                          choiceValues = list("width", "quantile", "manual"),
                          selected = "manual"),
                        shiny::conditionalPanel(
                          condition = sprintf("input['%s'] != 'manual'", ns("cutMethod")),
                          shiny::sliderInput(ns("cutNClasses"), "Nombre de classes", min = 2, max = 12, value = 4, step = 1)),
                        shiny::conditionalPanel(
                          condition = sprintf("input['%s'] == 'manual'", ns("cutMethod")),
                          shiny::textInput(ns("cutBreaks"), "Bornes (séparées par virgules)",
                                    value = "0, 3, 15, 100",
                                    placeholder = "ex. 0, 3, 15, 100"),
                          shiny::tags$small(style = "color:#7f8c8d;", shiny::icon("info-circle"),
                            " n bornes = n-1 classes. Les valeurs hors bornes deviendront NA.")),
                        shiny::radioButtons(ns("cutStyle"), shiny::tagList(shiny::icon("grip-lines-vertical"), " Convention des bornes (étiquettes automatiques)"),
                          choiceNames = list(
                            shiny::HTML("<b>Standard</b> — <code>[a ; b[</code>, <code>[b ; c[</code>, …, <code>[y ; z]</code> <small style='color:#7f8c8d;'>(dernière fermée des deux côtés)</small>"),
                            shiny::HTML("<b>Toutes ouvertes à droite</b> — <code>[a ; b[</code>, <code>[b ; c[</code>, …, <code>[y ; z[</code>"),
                            shiny::HTML("<b>Milieu ouvert</b> — <code>[a ; b[</code>, <code>]b ; c[</code>, …, <code>]y ; z]</code>"),
                            shiny::HTML("<b>Fermées à droite</b> — <code>[a ; b]</code>, <code>]b ; c]</code>, …, <code>]y ; z]</code> <small style='color:#7f8c8d;'>(la borne haute inclut la valeur)</small>"),
                            shiny::HTML("<b>Toutes fermées</b> — <code>[a ; b]</code>, <code>[b ; c]</code>, …, <code>[y ; z]</code> <small style='color:#7f8c8d;'>(fermées des deux côtés)</small>")),
                          choiceValues = list("std_last_closed", "all_left_closed", "mixed_open",
                                              "all_right_closed", "all_closed"),
                          selected = "std_last_closed"),
                        shiny::radioButtons(ns("cutLabels"), "Étiquettes des classes",
                          choices = c("Automatiques (selon la convention)" = "auto",
                                      "Personnalisées" = "custom"),
                          selected = "auto", inline = TRUE),
                        shiny::conditionalPanel(
                          condition = sprintf("input['%s'] == 'custom'", ns("cutLabels")),
                          shiny::textInput(ns("cutLabelsTxt"), "Étiquettes (une par classe, séparées par virgules)",
                                    value = "0-3 ans, 4-15 ans, +15 ans",
                                    placeholder = "ex. 0-3 ans, 4-15 ans, +15 ans")),
                        shiny::uiOutput(ns("cutNewNameUI")),
                        shiny::actionButton(ns("applyCut"), shiny::tagList(shiny::icon("layer-group"), " Créer la variable de classes"),
                                     class = "btn-primary")
                      )
                    ),
                    shiny::column(6,
                      shiny::div(style = "background-color:#fff;padding:15px;border-radius:5px;border:2px solid #8e44ad;",
                        shiny::h5(shiny::icon("eye"), " Aperçu en direct", style = "color:#8e44ad;margin-top:0;"),
                        shiny::uiOutput(ns("cutPreviewMsg")),
                        shiny::tableOutput(ns("cutPreviewTable")),
                        shiny::plotOutput(ns("cutPreviewPlot"), height = "230px"),
                        shiny::p(style = "font-size:11px;color:#7f8c8d;font-style:italic;margin-top:6px;",
                          shiny::icon("info-circle"),
                          " La variable créée est un facteur ORDONNÉ : les classes sont directement utilisables dans les analyses ordinales, tris et comparaisons.")
                      )
                    )
                  )
                )
              ),

              # Étape 7: Variables entièrement nulles
              shiny::fluidRow(
                shinydashboard::box(
                  title = shiny::tagList(
                    shiny::tags$span(class = "badge bg-red",
                              style = "font-size: 14px; margin-right: 10px;", "7"),
                    shiny::icon("circle-notch"),
                    "Variables à valeurs nulles"
                  ),
                  status = "primary", width = 12, solidHeader = TRUE,
                  collapsible = TRUE, collapsed = TRUE,
                  shiny::fluidRow(
                    shiny::column(7,
                      shiny::div(style = "background-color:#fdedec;padding:15px;border-radius:5px;",
                        shiny::radioButtons(ns("zeroSeuil"), shiny::tagList(shiny::icon("filter"), " Variables à lister"),
                          choiceNames = list(
                            shiny::HTML("<b>Entièrement nulles</b> <small style='color:#7f8c8d;'>(toutes les valeurs observées valent 0)</small>"),
                            shiny::HTML("<b>Quasi nulles</b> <small style='color:#7f8c8d;'>(au moins 90 % de zéros)</small>")),
                          choiceValues = list("1", "0.9"), selected = "1"),
                        shiny::uiOutput(ns("zeroVarsResume")),
                        shiny::div(style = "overflow-x:auto;max-width:100%;",
                            shiny::tableOutput(ns("zeroVarsTable"))),
                        shiny::uiOutput(ns("zeroVarsVides")),
                        shiny::p(style = "font-size:11px;color:#7f8c8d;font-style:italic;margin-top:6px;",
                          shiny::icon("info-circle"),
                          " Une variable dont toutes les valeurs valent 0 à une variance nulle : ",
                          "aucune corrélation ni test statistique n'est calculable sur elle. ",
                          "Les colonnes logiques (vrai/faux) ne sont pas listées : un « non » est une réponse, pas une mesure à zéro.")
                      )
                    ),
                    shiny::column(5,
                      shiny::div(style = "background-color:#fff;padding:15px;border-radius:5px;border:2px solid #c0392b;",
                        shiny::h5(shiny::icon("wrench"), " Corriger ou retirer", style = "color:#c0392b;margin-top:0;"),
                        shiny::uiOutput(ns("zeroVarsSelect")),
                        shiny::radioButtons(ns("zeroAction"), shiny::tagList(shiny::icon("sliders-h"), " Que faire ?"),
                          choiceNames = list(
                            shiny::HTML("<b>Déclarer les 0 manquants</b> <small style='color:#7f8c8d;'>(remplacer par NA)</small>"),
                            shiny::HTML("<b>Remplacer les 0 par une valeur</b>"),
                            shiny::HTML("<b>Saisir les valeurs une à une</b> <small style='color:#7f8c8d;'>(une seule variable)</small>"),
                            shiny::HTML("<b>Supprimer les variables</b> <small style='color:#7f8c8d;'>(définitif sur les données de travail)</small>")),
                          choiceValues = list("na", "valeur", "saisie", "supprimer"),
                          selected = "na"),
                        shiny::conditionalPanel(
                          condition = sprintf("input['%s'] == 'valeur'", ns("zeroAction")),
                          shiny::numericInput(ns("zeroRemplacement"), "Valeur de remplacement :", value = NA)),
                        shiny::conditionalPanel(
                          condition = sprintf("input['%s'] == 'saisie'", ns("zeroAction")),
                          shiny::uiOutput(ns("zeroSaisieUI"))),
                        shiny::div(style = "margin-top:12px;",
                          shiny::actionButton(ns("applyZero"), shiny::tagList(shiny::icon("check"), " Appliquer"),
                                       class = "btn-primary")),
                        shiny::uiOutput(ns("zeroMessage"))
                      )
                    )
                  )
                )
              ),

              shiny::fluidRow(
                shinydashboard::box(
                  title = shiny::tagList(
                    shiny::icon("table"),
                    "Aperçu des Données Nettoyées"
                  ),
                  status = "success",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  withSpinner(
                    DT::DTOutput(ns("cleanedData")),
                    type = 6,
                    color = "#27ae60"
                  ),
                  
                  footer = shiny::div(
                    style = "font-size: 12px; color: #7f8c8d;",
                    shiny::icon("info-circle"),
                    " Vérifiez vos données avant de passer à l'analyse ou à la modélisation"
                  )
                )
              )
  )
}

mod_clean_server <- function(id, values) {
  # Depot de l'etat des donnees apres nettoyage : c'est le nouveau diagnostic
  # de qualite qui dit si le nettoyage a atteint son but.
  shiny::observeEvent(values$cleanData, {
    d <- values$cleanData
    if (is.null(d) || !NROW(d)) return()
    # Au chargement, `cleanData` est une copie du jeu brut : rien n'a ete
    # nettoye. On n'annonce un nettoyage que s'il a vraiment eu lieu.
    if (!length(values$transformationLog) &&
        identical(dim(d), dim(values$data))) return()
    dq <- tryCatch(hstat_data_quality(d), error = function(e) NULL)
    hstat_ai_capture(values, "Nettoyage",
      "État des données après nettoyage",
      tables = list("Diagnostic de qualité" = dq),
      meta = list(variables = names(d), observations = NROW(d),
                  `transformations appliquees` =
                    if (length(values$transformationLog))
                      length(values$transformationLog) else 0L))
  }, ignoreInit = TRUE)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # -- Helper : parser une selection de lignes (ex. "1-5, 8, 10-12") --
  parseRowSelection <- function(selection_text, max_rows) {
    if (is.null(selection_text) || nchar(trimws(selection_text)) == 0) {
      stop("Veuillez entrer une sélection de lignes valide.")
    }
    
    text <- trimws(selection_text)
    text <- gsub("\\s+", " ", text)  
    
    # Remplacer différents formats de plage par un format uniforme.
    # Correction : ne remplacer « a »/« à » que s'ils sont ENTRE deux chiffres
    # (ex. « 1 a 5 »), sinon toute lettre a du texte etait transformee en tiret.
    # « to » est accepte au meme titre : les exemples affiches sous le champ
    # sont traduits, et une syntaxe montree doit etre une syntaxe acceptee.
    text <- gsub("(?i)(?<=[0-9])\\s*(?:to|[a\u00e0])\\s*(?=[0-9])", "-", text, perl = TRUE)
    
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

  
  output$deleteRowsPreview <- shiny::renderUI({
    shiny::req(values$cleanData)
    txt <- input$deleteRowsInput %||% ""
    if (nchar(trimws(txt)) == 0) return(NULL)
    
    max_rows <- nrow(values$cleanData)
    rows <- tryCatch(
      parseRowSelection(txt, max_rows),
      error = function(e) NULL
    )
    if (is.null(rows)) {
      return(shiny::div(
        style = "margin-top: 8px; padding: 8px; background-color: #ffcdd2; border-radius: 4px;",
        shiny::icon("times-circle", style = "color: #c62828;"),
        shiny::tags$span(style = "color: #c62828; font-size: 12px; margin-left: 5px;",
                  "Format invalide -- vérifiez votre saisie.")
      ))
    }
    n <- length(rows)
    preview_ids <- utils::head(rows, 8)
    preview_str <- paste(preview_ids, collapse = ", ")
    if (n > 8) preview_str <- trf("%s ... (%s de plus)", preview_str, n - 8)
    
    shiny::div(
      style = "margin-top: 8px; padding: 10px; background-color: #fff3e0; border-radius: 4px; border-left: 3px solid #f57c00;",
      shiny::icon("info-circle", style = "color: #f57c00;"),
      shiny::tags$span(style = "color: #e65100; font-size: 12px; font-weight: bold; margin-left: 5px;",
                trf("%s ligne(s) à supprimer : ", n)),
      shiny::tags$code(style = "font-size: 11px;", preview_str)
    )
  })
  
  shiny::observeEvent(input$applyDeleteRows, {
    shiny::req(values$cleanData, input$deleteRowsInput)
    tryCatch({
      max_rows <- nrow(values$cleanData)
      rows_to_delete <- parseRowSelection(input$deleteRowsInput, max_rows)
      
      if (length(rows_to_delete) == 0) {
        shiny::showNotification("Aucune ligne sélectionnée.", type = "warning", duration = 4)
        return()
      }
      if (length(rows_to_delete) >= max_rows) {
        shiny::showNotification("Impossible de supprimer toutes les lignes.", type = "error", duration = 5)
        return()
      }
      
      values$cleanData    <- values$cleanData[-rows_to_delete, ]
      values$filteredData <- values$cleanData
      
      shiny::showNotification(
        trf("%s ligne(s) supprimée(s). %s lignes restantes.", length(rows_to_delete), nrow(values$cleanData)),
        type = "message", duration = 5
      )
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur suppression"), type = "error", duration = 8)
    })
  })
  
  # - Tableau interactif pour sélection des lignes à supprimer
  output$deleteRowsTable <- DT::renderDataTable({
    shiny::req(values$cleanData)
    DT::datatable(
      utils::head(values$cleanData, 500),   
      selection  = "multiple",
      extensions = "Scroller",
      options    = list(
        scrollY    = "280px",
        scroller   = TRUE,
        pageLength = 25,
        dom        = "fti",
        language   = list(
          search      = "Rechercher :",
          info        = "Lignes _START_ à _END_ sur _TOTAL_",
          infoEmpty   = "Aucune ligne",
          emptyTable  = "Aucune donnée disponible"
        )
      ),
      rownames = TRUE,
      class    = "cell-border stripe hover compact"
    )
  })
  
  output$deleteRowsInteractivePreview <- shiny::renderUI({
    sel <- input$deleteRowsTable_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(shiny::div(style = "padding: 8px; color: #6c757d; font-size: 12px;",
                 shiny::icon("info-circle"), " Aucune ligne sélectionnée -- cliquez sur des lignes dans le tableau."))
    }
    shiny::div(style = "padding: 10px; background-color: #fff3e0; border-radius: 4px; border-left: 3px solid #f57c00;",
        shiny::icon("info-circle", style = "color: #f57c00;"),
        shiny::tags$span(style = "color: #e65100; font-size: 12px; font-weight: bold; margin-left: 5px;",
                  trf("%s ligne(s) sélectionnée(s) : ", length(sel))),
        shiny::tags$code(style = "font-size: 11px;", paste(utils::head(sel, 10), collapse = ", "),
                  if (length(sel) > 10) paste0(" ... +", length(sel)-10, " autres") else "")
    )
  })
  
  shiny::observeEvent(input$applyDeleteRowsInteractive, {
    sel <- input$deleteRowsTable_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      shiny::showNotification("Aucune ligne sélectionnée dans le tableau.", type = "warning", duration = 4)
      return()
    }
    shiny::req(values$cleanData)
    if (length(sel) >= nrow(values$cleanData)) {
      shiny::showNotification("Impossible de supprimer toutes les lignes.", type = "error", duration = 5)
      return()
    }
    values$cleanData    <- values$cleanData[-sel, ]
    values$filteredData <- values$cleanData
    shiny::showNotification(
      trf("%s ligne(s) supprimée(s). %s lignes restantes.", length(sel), nrow(values$cleanData)),
      type = "message", duration = 5)
  })
  

  # ---- Nettoyage ----
  
  output$varTypeUI <- shiny::renderUI({
    shiny::req(values$data)
    cols <- names(values$data)
    # IDs surs : indexes et namespaces (ns). Les noms de colonnes contenant des
    # espaces ou des accents (ex. "Date de semis", "Société") produisaient sinon
    # des identifiants HTML invalides qui empechaient l'affichage du module.
    shiny::tagList(
      shiny::div(class = "alert alert-info", 
          shiny::icon("info-circle"), 
          " Sélectionnez le type souhaité pour chaque variable"),
      lapply(seq_along(cols), function(i) {
        col <- cols[i]
        x <- values$data[[col]]
        current_type <- if (is.numeric(x)) "numeric"
          else if (is.ordered(x)) "ordered"
          else if (is.factor(x)) "factor"
          else if (inherits(x, "Date")) "date"
          else "character"
        # Niveaux proposes pour l'ordre d'un facteur ordinal : niveaux actuels
        # si deja facteur (ordre conserve), sinon valeurs uniques triees.
        lvl_choices <- if (is.factor(x)) levels(x) else {
          v <- unique(as.character(x)); v <- v[!is.na(v) & nzchar(trimws(v))]
          sort(v)
        }
        too_many_lvls <- length(lvl_choices) > 100
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6, shiny::strong(col)),
            shiny::column(6, 
                   shiny::selectInput(
                     ns(paste0("type_", i)), 
                     NULL,
                     choices = c("Numérique" = "numeric", 
                                 "Facteur nominal (non ordonné)" = "factor", 
                                 "Facteur ordinal (ordonné)" = "ordered", 
                                 "Texte" = "character", 
                                 "Date" = "date"),
                     selected = current_type,
                     width = "100%"
                   )
            )
          ),
          # Ordre des modalites, visible uniquement pour le type ordinal
          shiny::conditionalPanel(
            condition = sprintf("input.type_%d == 'ordered'", i),
            ns = ns,
            shiny::div(
              style = "background-color:#fff8e1;border-left:3px solid #ffb300;padding:8px 12px;margin:0 0 10px 0;border-radius:4px;",
              if (too_many_lvls) {
                shiny::tags$small(style = "color:#e65100;",
                  shiny::icon("exclamation-triangle"),
                  trf(" %d modalités distinctes : trop pour définir un ordre manuel. La variable sera ordonnée selon l'ordre alphabétique.", length(lvl_choices)))
              } else {
                shiny::tagList(
                  shiny::tags$small(style = "color:#8d6e63;",
                    shiny::icon("sort-amount-up"),
                    " Ordre des modalités, du plus petit au plus grand (glissez-déposez pour réordonner) :"),
                  shiny::selectizeInput(
                    ns(paste0("order_", i)), NULL,
                    choices = lvl_choices,
                    selected = if (is.ordered(x)) levels(x) else lvl_choices,
                    multiple = TRUE, width = "100%",
                    options = list(plugins = list("drag_drop", "remove_button"),
                                   placeholder = "Sélectionnez les modalités dans l'ordre croissant..."))
                )
              }
            )
          )
        )
      })
    )
  })
  
  shiny::observeEvent(input$applyTypes, {
    shiny::req(values$cleanData)
    
    shiny::withProgress(message = 'Application des types...', value = 0, {
      data_temp <- values$cleanData
      cols <- names(data_temp)
      n <- length(cols)
      
      for (i in seq_along(cols)) {
        col <- cols[i]
        type_input <- input[[paste0("type_", i)]]   # lu par index (ns auto)
        
        if (!is.null(type_input)) {
          tryCatch({
            if (type_input == "numeric") {
              data_temp[[col]] <- suppressWarnings(as.numeric(as.character(data_temp[[col]])))
            } else if (type_input == "factor") {
              # Nominal EXPLICITE : as.factor() conserverait la classe
              # "ordered" d'un facteur deja ordinal.
              data_temp[[col]] <- factor(as.character(data_temp[[col]]), ordered = FALSE)
            } else if (type_input == "ordered") {
              vals_chr <- as.character(data_temp[[col]])
              uniq <- unique(vals_chr[!is.na(vals_chr)])
              lv <- input[[paste0("order_", i)]]
              lv <- lv[lv %in% uniq]
              if (length(lv) == 0) lv <- sort(uniq)
              # Modalites non listees par l'utilisateur : ajoutees a la fin,
              # dans l'ordre alphabetique, pour ne perdre aucune donnee.
              lv <- c(lv, sort(setdiff(uniq, lv)))
              data_temp[[col]] <- factor(vals_chr, levels = lv, ordered = TRUE)
            } else if (type_input == "character") {
              data_temp[[col]] <- as.character(data_temp[[col]])
            } else if (type_input == "date") {
              data_temp[[col]] <- as.Date(data_temp[[col]])
            }
          }, error = function(e) {
            shiny::showNotification(hstat_err_fr(e, trf("Variable `%s`", col)), 
                             type = "warning", duration = 5)
          })
        }
        
        shiny::incProgress(1/n, detail = paste("Variable", i, "sur", n))
      }
      
      values$cleanData <- data_temp
      values$filteredData <- values$cleanData
      values$data <- data_temp
    })
    
    shiny::showNotification(
      ui = shiny::tagList(shiny::icon("check"), " Types de variables appliqués avec succès !"),
      type = "message", 
      duration = 3
    )
  })
  
  output$removeVarUI <- shiny::renderUI({
    shiny::req(values$cleanData)
    shiny::selectInput(ns("removeVarName"), "Supprimer variable :", 
                choices = names(values$cleanData))
  })
  
  shiny::observeEvent(input$removeVar, {
    shiny::req(input$removeVarName)
    
    var_name <- input$removeVarName
    values$cleanData <- values$cleanData[, !(names(values$cleanData) %in% var_name), drop = FALSE]
    values$filteredData <- values$cleanData
    
    shiny::showNotification(
      ui = shiny::tagList(shiny::icon("trash"), trf(" Variable ` %s ` supprimée avec succès", var_name)),
      type = "message", 
      duration = 3
    )
  })

  # ---- Renommer une variable (colonne) ----
  output$renameVarUI <- shiny::renderUI({
    shiny::req(values$cleanData)
    shiny::selectInput(ns("renameVarOld"), "Variable à renommer :",
                choices = names(values$cleanData))
  })

  rename_msg <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$applyRenameVar, {
    old <- input$renameVarOld
    new <- trimws(input$renameVarNew %||% "")
    if (is.null(old) || !nzchar(old) || !(old %in% names(values$cleanData))) {
      rename_msg(list(ok = FALSE, msg = "Choisissez une variable à renommer.")); return()
    }
    if (!nzchar(new)) {
      rename_msg(list(ok = FALSE, msg = "Le nouveau nom ne peut pas être vide.")); return()
    }
    if (identical(new, old)) {
      rename_msg(list(ok = FALSE, msg = "Le nouveau nom est identique à l'ancien.")); return()
    }
    if (new %in% setdiff(names(values$cleanData), old)) {
      rename_msg(list(ok = FALSE, msg = trf("Le nom « %s » existe déjà. Choisissez un nom unique.", new))); return()
    }
    # Renommage répercuté dans tous les jeux de données pour rester cohérent.
    for (slot in c("data", "cleanData", "filteredData")) {
      dd <- values[[slot]]
      if (!is.null(dd) && old %in% names(dd)) {
        names(dd)[names(dd) == old] <- new
        values[[slot]] <- dd
      }
    }
    rename_msg(list(ok = TRUE, msg = trf("Variable « %s » renommée en « %s ».", old, new)))
    shiny::updateTextInput(session, "renameVarNew", value = "")
    shiny::showNotification(
      ui = shiny::tagList(shiny::icon("check"), trf(" « %s » renommée en « %s »", old, new)),
      type = "message", duration = 3)
  })

  output$renameVarStatus <- shiny::renderUI({
    m <- rename_msg(); if (is.null(m)) return(NULL)
    col <- if (isTRUE(m$ok)) "#27ae60" else "#c0392b"
    ic <- if (isTRUE(m$ok)) "check-circle" else "exclamation-triangle"
    shiny::div(style = sprintf("margin-top:10px;padding:8px;border-radius:4px;background:%s22;color:%s;font-size:12px;", col, col),
        shiny::icon(ic), " ", m$msg)
  })

  # ---- Recodage des variables catégorielles et ordinales ----
  RECODE_THRESHOLD <- 12L
  recode_candidates <- shiny::reactive({
    df <- values$cleanData; shiny::req(df)
    nm <- names(df)
    keep <- vapply(nm, function(v) {
      x <- df[[v]]
      if (is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
      if (is.numeric(x)) return(length(unique(stats::na.omit(x))) <= 20)
      FALSE
    }, logical(1))
    nm[keep]
  })
  output$recodeVarSelect <- shiny::renderUI({
    vars <- recode_candidates()
    if (length(vars) == 0)
      return(shiny::helpText("Aucune variable catégorielle ou ordinale détectée."))
    shiny::selectInput(ns("recodeVar"), shiny::tagList(shiny::icon("list"), " Variable à recoder"), choices = vars)
  })
  recode_levels <- shiny::reactive({
    df <- values$cleanData; v <- input$recodeVar
    shiny::req(df, v %in% names(df))
    sort(unique(as.character(stats::na.omit(df[[v]]))))
  })
  output$recodeInterface <- shiny::renderUI({
    lv <- recode_levels()
    if (length(lv) == 0) return(shiny::helpText("Aucune modalité à recoder."))
    is_ordinal <- (input$recodeType %||% "nominal") == "ordinal"
    if (length(lv) > RECODE_THRESHOLD) {
      default_txt <- paste(sprintf("%s = %s", lv, lv), collapse = "\n")
      shiny::tagList(
        shiny::tags$small(trf("%d modalités (tableau éditable).%s", length(lv),
          if (is_ordinal) tr(" L'ordre des lignes = ordre du facteur ordinal.") else "")),
        shiny::textAreaInput(ns("recodeTable"), NULL, value = default_txt,
                      rows = min(20, length(lv) + 1), width = "100%"))
    } else {
      shiny::tagList(
        shiny::tags$small(trf("%d modalités.%s", length(lv),
          if (is_ordinal) tr(" L'ordre ci-dessous = ordre du facteur ordinal.") else "")),
        lapply(seq_along(lv), function(i)
          shiny::textInput(ns(paste0("recodeLvl_", i)),
                    label = sprintf("%s« %s » devient :",
                                    if (is_ordinal) sprintf("(%d) ", i) else "", lv[i]),
                    value = lv[i])))
    }
  })
  recode_msg <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$applyRecode, {
    df <- values$cleanData; v <- input$recodeVar
    if (is.null(v) || !(v %in% names(df))) {
      recode_msg(list(ok = FALSE, msg = "Choisissez une variable à recoder.")); return()
    }
    lv <- recode_levels()
    is_ordinal <- (input$recodeType %||% "nominal") == "ordinal"
    mapping <- stats::setNames(lv, lv); new_order <- lv
    if (length(lv) > RECODE_THRESHOLD) {
      raw <- input$recodeTable %||% ""
      lines <- strsplit(raw, "\n")[[1]]; ord <- character(0)
      for (ln in lines) {
        if (!grepl("=", ln)) next
        parts <- strsplit(ln, "=", fixed = TRUE)[[1]]
        old <- trimws(parts[1]); new <- trimws(paste(parts[-1], collapse = "="))
        if (nzchar(old) && old %in% lv) { mapping[[old]] <- new; ord <- c(ord, new) }
      }
      if (length(ord)) new_order <- unique(ord)
    } else {
      ord <- character(0)
      for (i in seq_along(lv)) {
        val <- input[[paste0("recodeLvl_", i)]]
        nv <- if (!is.null(val) && nzchar(trimws(val))) trimws(val) else lv[i]
        mapping[[lv[i]]] <- nv; ord <- c(ord, nv)
      }
      new_order <- unique(ord)
    }
    apply_map <- function(col) {
      cc <- as.character(col); idx <- match(cc, names(mapping))
      out <- ifelse(is.na(idx), cc, mapping[idx])
      if (is_ordinal) factor(out, levels = unique(new_order), ordered = TRUE)
      else factor(out, levels = unique(new_order))
    }
    n_changed <- 0L
    for (slot in c("data", "cleanData", "filteredData")) {
      dd <- values[[slot]]
      if (!is.null(dd) && v %in% names(dd)) {
        before <- as.character(dd[[v]])
        dd[[v]] <- apply_map(dd[[v]])
        if (identical(slot, "cleanData")) n_changed <- sum(before != as.character(dd[[v]]), na.rm = TRUE)
        values[[slot]] <- dd
      }
    }
    recode_msg(list(ok = TRUE,
      msg = trf("Recodage %s appliqué à « %s » : %d valeur(s) modifiée(s), %d modalité(s)%s.",
                    if (is_ordinal) "ordinal" else "nominal", v, n_changed,
                    length(unique(new_order)), if (is_ordinal) tr(" (ordre défini)") else "")))
  })
  output$recodeStatus <- shiny::renderUI({
    m <- recode_msg(); if (is.null(m)) return(NULL)
    cls <- if (isTRUE(m$ok)) "#27ae60" else "#c0392b"
    ic <- if (isTRUE(m$ok)) "check-circle" else "exclamation-triangle"
    shiny::div(style = sprintf("margin-top:10px; padding:8px; border-radius:4px; background:%s22; color:%s; font-size:12px;", cls, cls),
        shiny::icon(ic), " ", m$msg)
  })

  shiny::observeEvent(input$addVar, {
    shiny::req(input$newVarName)
    
    if (input$newVarName %in% names(values$cleanData)) {
      shiny::showNotification(
        ui = shiny::tagList(shiny::icon("exclamation-triangle"), " Cette variable existe déjà !"),
        type = "warning", 
        duration = 3
      )
    } else if (input$newVarName == "") {
      shiny::showNotification("Le nom de la variable ne peut pas être vide", 
                       type = "error", duration = 3)
    } else {
      values$cleanData[[input$newVarName]] <- rep(input$newVarValue, nrow(values$cleanData))
      values$filteredData <- values$cleanData
      
      shiny::showNotification(
        ui = shiny::tagList(shiny::icon("plus"), trf(" Variable ` %s ` ajoutée avec succès", input$newVarName)),
        type = "message", 
        duration = 3
      )
    }
  })
  
  output$colPicker <- shiny::renderUI({
    shiny::req(values$cleanData)
    shiny::selectInput(ns("colInsert"), "Insérer colonne :", 
                choices = c("", names(values$cleanData)))
  })
  
  shiny::observeEvent(input$colInsert, {
    shiny::req(input$colInsert != "")
    current_formula <- input$calcFormula %||% ""
    # Backtick-quoting automatique si le nom contient des espaces ou caractères spéciaux
    col_safe <- if (grepl("[/+*^()%$@!? -]", input$colInsert, perl = TRUE) || grepl("^[0-9]", input$colInsert)) {
      paste0("`", input$colInsert, "`")
    } else {
      input$colInsert
    }
    new_formula <- paste0(current_formula,
                          ifelse(nchar(current_formula) > 0, " ", ""),
                          col_safe)
    shiny::updateTextInput(session, "calcFormula", value = new_formula)
  })
  
  shiny::observeEvent(input$insertPlus, { 
    shiny::updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " + ")) 
  })
  
  shiny::observeEvent(input$insertMoins, { 
    shiny::updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " - ")) 
  })
  
  shiny::observeEvent(input$insertMult, { 
    shiny::updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " * ")) 
  })
  
  shiny::observeEvent(input$insertDiv, { 
    shiny::updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " / ")) 
  })
  
  shiny::observeEvent(input$insertLog, { 
    shiny::updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " log()")) 
  })
  
  shiny::observeEvent(input$insertSqrt, { 
    shiny::updateTextInput(session, "calcFormula", 
                    value = paste0(input$calcFormula %||% "", " sqrt()")) 
  })
  shiny::observeEvent(input$insertLog10, { shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " log10()")) })
  shiny::observeEvent(input$insertAbs,   { shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " abs()")) })
  shiny::observeEvent(input$insertRound, { shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " round(,2)")) })
  shiny::observeEvent(input$insertExp,   { shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " exp()")) })
  shiny::observeEvent(input$insertMean,  { shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " mean()")) })
  shiny::observeEvent(input$insertSum,   { shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " sum()")) })
  shiny::observeEvent(input$insertPow,   { shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", "^")) })
  shiny::observeEvent(input$insertParen, { shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", "()")) })
  shiny::observeEvent(input$insertIfelse,{ shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " ifelse(, , )")) })
  shiny::observeEvent(input$insertIsNA,  { shiny::updateTextInput(session, "calcFormula", value = paste0(input$calcFormula %||% "", " is.na()")) })
  
  # Insérer une condition sur lignes dans la formule
  output$rowCondPicker <- shiny::renderUI({
    shiny::req(values$cleanData)
    shiny::tagList(
      shiny::selectInput(ns("rowCondCol"), "Colonne :", choices = c("", names(values$cleanData))),
      shiny::conditionalPanel(
        ns = ns,
        condition = "input.rowCondCol != ''",
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("rowCondOp"), "Opérateur :",
                                choices = c("==" = "==", "!=" = "!=", ">" = ">", ">=" = ">=", "<" = "<", "<=" = "<=", "is.na" = "is.na"),
                                selected = "==")),
          shiny::column(6, shiny::textInput(ns("rowCondVal"), "Valeur :", placeholder = "ex: 'A' ou 10"))
        ),
        shiny::actionButton(ns("insertRowCond"), shiny::tagList(shiny::icon("filter"), " Insérer condition"),
                     class = "btn-success btn-sm btn-block")
      )
    )
  })
  
  shiny::observeEvent(input$insertRowCond, {
    shiny::req(input$rowCondCol, input$rowCondCol != "")
    col_safe_cond <- if (grepl("[-/ +*^()%$@!?]|^[0-9]", input$rowCondCol, perl = TRUE)) {
      paste0("`", input$rowCondCol, "`")
    } else { input$rowCondCol }
    cond <- if (input$rowCondOp == "is.na") {
      paste0("is.na(", col_safe_cond, ")")
    } else {
      paste0(col_safe_cond, " ", input$rowCondOp, " ", input$rowCondVal)
    }
    cur <- input$calcFormula %||% ""
    shiny::updateTextInput(session, "calcFormula",
                    value = paste0("ifelse(", cond, ", ", cur, ", NA)"))
  })
  
  shiny::observeEvent(input$addCalcVar, {
    shiny::req(input$calcVarName, input$calcFormula)
    
    if (input$calcVarName == "") {
      shiny::showNotification("Le nom de la variable ne peut pas être vide", 
                       type = "error", duration = 3)
      return()
    }
    
    if (input$calcFormula == "") {
      shiny::showNotification("La formule ne peut pas être vide", 
                       type = "error", duration = 3)
      return()
    }
    
    tryCatch({
      formula_safe <- auto_quote_colnames(input$calcFormula, names(values$cleanData))
      # Securite : evaluation en environnement clos avec liste blanche de
      # fonctions (empeche system(), file.remove(), source(), etc.).
      new_col <- hstat_safe_eval(formula_safe, values$cleanData)
      
      if (length(new_col) == 1) {
        
        new_col <- rep(new_col, nrow(values$cleanData))
      }
      
      if (length(new_col) != nrow(values$cleanData)) {
        shiny::showNotification(
          shiny::tagList(
            shiny::icon("exclamation-triangle"),
            # LES ENFANTS TEXTE ADJACENTS NE FONT QU'UN SEUL NOEUD. Les
            # morceaux et les valeurs se retrouvent fondus dans la meme chaine
            # (« Formule incorrecte : le résultat à 3 valeur(s) au lieu de 10. »)
            # qu'aucune cle ne peut couvrir : seul un gabarit le peut.
            trf(" Formule incorrecte : le résultat à %s valeur(s) au lieu de %s. ",
                length(new_col), nrow(values$cleanData)),
            shiny::tags$br(),
            shiny::tags$small("Astuce : utilisez rowMeans(cbind(Var1, Var2)) pour la moyenne ligne par ligne.")
          ),
          type = "error", duration = 8)
        return()
      }
      
      values$cleanData[[input$calcVarName]] <- new_col
      values$filteredData <- values$cleanData
      
      shiny::showNotification(
        ui = shiny::tagList(shiny::icon("calculator"), trf(" Variable ` %s ` créée avec succès !", input$calcVarName)),
        type = "message", 
        duration = 3
      )
      
      shiny::updateTextInput(session, "calcVarName", value = "")
      shiny::updateTextInput(session, "calcFormula", value = "")
      
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur dans la formule"), 
                       type = "error", duration = 5)
    })
  })
  
  # Tableau du pourcentage de valeurs manquantes par variable
  na_summary_df <- shiny::reactive({
    shiny::req(values$cleanData)
    d <- values$cleanData
    n <- nrow(d)
    df <- data.frame(
      Variable = names(d),
      `Type` = vapply(d, function(x) if (is.numeric(x)) "numérique" else "catégorielle", character(1)),
      `NA (n)` = vapply(d, function(x) sum(is.na(x)), integer(1)),
      `NA (%)` = round(vapply(d, function(x) mean(is.na(x)) * 100, numeric(1)), 2),
      check.names = FALSE, row.names = NULL, stringsAsFactors = FALSE)
    df[order(-df$`NA (%)`), , drop = FALSE]
  })

  output$naSummaryTable <- DT::renderDT({
    df <- na_summary_df()
    DT::datatable(df, rownames = FALSE,
                  options = list(pageLength = 5, dom = "tp", scrollX = TRUE),
                  class = "compact stripe hover") |>
      DT::formatStyle("NA (%)",
        background = DT::styleColorBar(c(0, 100), "#ffcdd2"),
        backgroundSize = "98% 88%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center")
  })

  # Recommandation automatique de la methode d'imputation
  output$naRecommendation <- shiny::renderUI({
    df <- na_summary_df()
    max_pct <- max(df$`NA (%)`, na.rm = TRUE)
    has_cat <- any(df$Type == "catégorielle" & df$`NA (%)` > 0)
    n_obs <- nrow(values$cleanData)
    rec <- if (max_pct == 0) {
      list(txt = "Aucune valeur manquante : aucun traitement nécessaire.", m = NULL)
    } else if (max_pct < 5) {
      list(txt = "Moins de 5% de NA : la suppression des lignes est acceptable, ou une imputation simple (médiane).", m = "median")
    } else if (max_pct <= 20 && has_cat) {
      list(txt = "NA modérés avec variables catégorielles : missForest (gère le mixte) ou MICE/PMM sont recommandés.", m = "rf")
    } else if (max_pct <= 30) {
      list(txt = "NA non négligeables (mécanisme MAR plausible) : l'imputation multiple MICE/PMM est recommandée pour préserver la variance.", m = "mice")
    } else {
      list(txt = "Plus de 30% de NA sur au moins une variable : envisagez de retirer la variable ; sinon MICE/PMM avec prudence.", m = "mice")
    }
    shiny::tagList(
      shiny::tags$b(shiny::icon("lightbulb"), " Recommandation : "),
      shiny::span(rec$txt),
      if (!is.null(rec$m)) shiny::div(style = "margin-top:6px;",
        shiny::actionButton(ns("naApplyRecommend"),
          shiny::tagList(shiny::icon("check"), " Adopter la méthode recommandée"),
          class = "btn-warning btn-sm")))
  })

  shiny::observeEvent(input$naApplyRecommend, {
    df <- na_summary_df()
    max_pct <- max(df$`NA (%)`, na.rm = TRUE)
    has_cat <- any(df$Type == "catégorielle" & df$`NA (%)` > 0)
    m <- if (max_pct < 5) "median" else if (max_pct <= 20 && has_cat) "rf" else "mice"
    shiny::updateRadioButtons(session, "naMethod", selected = m)
    shiny::showNotification(trf("Méthode recommandée sélectionnée : %s", m), type = "message", duration = 3)
  })

  output$naVarSelect <- shiny::renderUI({
    shiny::req(values$cleanData)
    
    vars_with_na <- names(values$cleanData)[sapply(values$cleanData, function(x) any(is.na(x)))]
    
    if (length(vars_with_na) == 0) {
      return(shiny::div(class = "alert alert-success", 
                 shiny::icon("check-circle"), 
                 " Aucune valeur manquante détectée dans les données"))
    }
    
    shiny::selectInput(
      inputId = ns("naVars"),
      label = trf("Sélectionnez les variables à traiter (%s variables avec NA) :", length(vars_with_na)), 
      choices = names(values$cleanData),
      selected = vars_with_na,
      multiple = TRUE,
      selectize = TRUE
    )
  })
  
  shiny::observeEvent(input$applyNA, {
    shiny::req(values$cleanData, input$naVars)
    
    if (length(input$naVars) == 0) {
      shiny::showNotification("Veuillez sélectionner au moins une variable", 
                       type = "warning", duration = 3)
      return()
    }
    
    shiny::withProgress(message = 'Traitement des valeurs manquantes...', value = 0, {
      data_temp <- values$cleanData
      n <- length(input$naVars)

      # Methodes multivariees (operent sur l'ensemble des colonnes selectionnees)
      if (input$naMethod %in% c("knn", "mice", "rf")) {
        sel <- input$naVars
        ok <- TRUE
        if (input$naMethod == "knn") {
          if (!requireNamespace("VIM", quietly = TRUE)) {
            ok <- FALSE
            shiny::showNotification("Package 'VIM' indisponible. Installez-le pour l'imputation KNN.",
                             type = "error", duration = 6)
          } else if (nrow(data_temp) > HSTAT_IMPUTE_MAX_N) {
            # kNN est en O(n^2) : au-dela du seuil il gelerait l'application.
            ok <- FALSE
            shiny::showNotification(sprintf(paste0(
              "Imputation KNN impossible sur %s lignes (limite : %s, algorithme ",
              "en O(n^2)). Utilisez 'mice' ou la médiane/le mode, adaptes aux ",
              "grands jeux de données."),
              format(nrow(data_temp), big.mark = " "),
              format(HSTAT_IMPUTE_MAX_N, big.mark = " ")),
              type = "warning", duration = 12)
          } else {
            imp <- tryCatch(VIM::kNN(data_temp, variable = sel,
                                     k = input$naKnnK %||% 5, imp_var = FALSE),
                            error = function(e) { ok <<- FALSE; NULL })
            if (ok && !is.null(imp)) data_temp <- imp
            if (!ok) shiny::showNotification("Échec de l'imputation KNN.",
                                      type = "error", duration = 6)
          }
        } else if (input$naMethod == "mice") {
          if (requireNamespace("mice", quietly = TRUE)) {
            if (nrow(data_temp) > 200000L)
              shiny::showNotification(trf("Imputation multiple (mice) sur %s lignes : le calcul peut prendre plusieurs minutes.", format(nrow(data_temp), big.mark = " ")),
                type = "message", duration = 10)
            sub <- data_temp[, sel, drop = FALSE]
            mids <- tryCatch(mice::mice(sub, m = input$naMiceM %||% 5,
                                        method = "pmm", printFlag = FALSE),
                             error = function(e) { ok <<- FALSE; NULL })
            if (ok && !is.null(mids)) {
              comp_list <- lapply(seq_len(mids$m), function(i) mice::complete(mids, i))
              for (cn in sel) {
                if (is.numeric(data_temp[[cn]])) {
                  mat <- sapply(comp_list, function(cc) cc[[cn]])
                  data_temp[[cn]] <- rowMeans(mat, na.rm = TRUE)
                } else {
                  data_temp[[cn]] <- comp_list[[1]][[cn]]  # 1re imputation pour les facteurs
                }
              }
            }
          } else ok <- FALSE
          if (!ok) shiny::showNotification("Package 'mice' indisponible. Installez-le pour l'imputation multiple.",
                                    type = "error", duration = 6)
        } else if (input$naMethod == "rf") {
          if (!requireNamespace("missForest", quietly = TRUE)) {
            ok <- FALSE
            shiny::showNotification("Package 'missForest' indisponible. Repli : médiane/mode.",
                             type = "warning", duration = 6)
          } else if (nrow(data_temp) > HSTAT_IMPUTE_MAX_N) {
            ok <- FALSE
            shiny::showNotification(sprintf(paste0(
              "missForest est trop lent au-delà de %s lignes (%s ici). ",
              "Repli automatique : médiane/mode."),
              format(HSTAT_IMPUTE_MAX_N, big.mark = " "),
              format(nrow(data_temp), big.mark = " ")),
              type = "warning", duration = 12)
          } else {
            sub <- data_temp[, sel, drop = FALSE]
            sub[] <- lapply(sub, function(x) if (is.character(x)) as.factor(x) else x)
            imp <- tryCatch(missForest::missForest(sub)$ximp,
                            error = function(e) { ok <<- FALSE; NULL })
            if (ok && !is.null(imp)) data_temp[, sel] <- imp
            if (!ok) shiny::showNotification("Échec de missForest. Repli : médiane/mode.",
                                      type = "warning", duration = 6)
          }
          if (!ok) {
            for (col in sel) {
              x <- data_temp[[col]]
              if (is.numeric(x)) data_temp[[col]][is.na(x)] <- stats::median(x, na.rm = TRUE)
              else { mv <- names(sort(table(x), decreasing = TRUE))[1]; data_temp[[col]][is.na(x)] <- mv }
            }
          }
        }
        shiny::incProgress(1, detail = "Imputation multivariée")
        values$cleanData <- data_temp
        values$filteredData <- values$cleanData
        shiny::showNotification(shiny::tagList(shiny::icon("check"), " Imputation appliquée."),
                         type = "message", duration = 3)
        return()
      }

      for (i in seq_along(input$naVars)) {
        col <- input$naVars[i]
        
        tryCatch({
          if (input$naMethod == "remove") {
            data_temp <- data_temp[!is.na(data_temp[[col]]), ]
          } else if (input$naMethod == "mean") {
            if (is.numeric(data_temp[[col]])) {
              mean_val <- mean(data_temp[[col]], na.rm = TRUE)
              data_temp[[col]][is.na(data_temp[[col]])] <- mean_val
            } else {
              shiny::showNotification(trf("La variable ` %s ` n'est pas numérique. Moyenne impossible.", col), 
                               type = "warning", duration = 3)
            }
          } else if (input$naMethod == "median") {
            if (is.numeric(data_temp[[col]])) {
              median_val <- stats::median(data_temp[[col]], na.rm = TRUE)
              data_temp[[col]][is.na(data_temp[[col]])] <- median_val
            } else {
              shiny::showNotification(trf("La variable ` %s ` n'est pas numérique. Médiane impossible.", col), 
                               type = "warning", duration = 3)
            }
          } else if (input$naMethod == "mode") {
            x <- data_temp[[col]]
            mv <- names(sort(table(x), decreasing = TRUE))[1]
            if (is.factor(x)) data_temp[[col]][is.na(x)] <- mv
            else if (is.numeric(x)) data_temp[[col]][is.na(x)] <- as.numeric(mv)
            else data_temp[[col]][is.na(x)] <- mv
          } else if (input$naMethod == "value") {
            data_temp[[col]][is.na(data_temp[[col]])] <- input$naValue
          }
        }, error = function(e) {
          shiny::showNotification(hstat_err_fr(e, trf("Variable `%s`", col)), 
                           type = "error", duration = 5)
        })
        
        shiny::incProgress(1/n, detail = paste("Variable", i, "sur", n))
      }
      
      values$cleanData <- data_temp
      values$filteredData <- values$cleanData
    })
    
    shiny::showNotification(
      ui = shiny::tagList(shiny::icon("check"), " Traitement des valeurs manquantes terminé avec succès !"),
      type = "message", 
      duration = 3
    )
  })
  
  # ---- Valeurs aberrantes et winsorisation ----
  output$outlierVarSelect <- shiny::renderUI({
    shiny::req(values$cleanData)
    num_cols <- names(values$cleanData)[sapply(values$cleanData, is.numeric)]
    if (length(num_cols) == 0)
      return(shiny::div(class = "alert alert-warning", shiny::icon("exclamation-triangle"),
                 " Aucune variable numérique disponible."))
    shiny::selectInput(ns("outlierVars"), "Variables numériques à analyser :",
                choices = num_cols, selected = num_cols, multiple = TRUE, selectize = TRUE)
  })

  # Renvoie les bornes (basse, haute) selon la methode choisie
  .outlier_bounds <- function(x, method, iqr_k, z_thr) {
    x <- x[is.finite(x)]
    if (length(x) < 2) return(c(NA_real_, NA_real_))
    if (method == "iqr") {
      q <- stats::quantile(x, c(.25, .75), names = FALSE)
      iqrv <- q[2] - q[1]
      c(q[1] - iqr_k * iqrv, q[2] + iqr_k * iqrv)
    } else if (method == "zscore") {
      m <- mean(x); s <- stats::sd(x)
      c(m - z_thr * s, m + z_thr * s)
    } else {  # mad
      med <- stats::median(x); madv <- stats::mad(x)
      if (madv == 0) madv <- 1e-9
      c(med - z_thr * madv, med + z_thr * madv)
    }
  }

  outlier_report <- shiny::reactiveVal(NULL)

  compute_outliers <- function() {
    shiny::req(values$cleanData, input$outlierVars)
    d <- values$cleanData
    method <- input$outlierMethod %||% "iqr"
    iqr_k <- input$outlierIqrK %||% 1.5
    z_thr <- input$outlierZThresh %||% 3
    rows <- lapply(input$outlierVars, function(cn) {
      x <- d[[cn]]
      b <- .outlier_bounds(x, method, iqr_k, z_thr)
      xs <- x[is.finite(x)]
      n_valid <- length(xs)
      if (n_valid == 0 || anyNA(b)) {
        return(data.frame(Variable = cn,
                          `Borne basse` = NA_real_,
                          `Borne haute` = NA_real_,
                          `Aberrants (n)` = 0L,
                          `Aberrants (%)` = NA_real_,
                          check.names = FALSE, stringsAsFactors = FALSE))
      }
      n_out <- sum(xs < b[1] | xs > b[2])
      # Bornes affichees = valeurs REELLES observees dans les donnees :
      # plus petite et plus grande valeur non aberrante (convention des
      # moustaches du boxplot), et non les bornes theoriques (Q1 - k*IQR...)
      # qui peuvent etre negatives ou hors de la plage des donnees.
      inb <- xs[xs >= b[1] & xs <= b[2]]
      low_real  <- if (length(inb) > 0) min(inb) else min(xs)
      high_real <- if (length(inb) > 0) max(inb) else max(xs)
      data.frame(Variable = cn,
                 `Borne basse` = round(low_real, 4),
                 `Borne haute` = round(high_real, 4),
                 `Aberrants (n)` = n_out,
                 `Aberrants (%)` = round(n_out / n_valid * 100, 2),
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  }

  shiny::observeEvent(input$detectOutliers, {
    rep <- tryCatch(compute_outliers(), error = function(e) NULL)
    outlier_report(rep)
    if (is.null(rep)) shiny::showNotification("Détection impossible.", type = "error", duration = 4)
    else shiny::showNotification(shiny::tagList(shiny::icon("check"), " Détection terminée."), type = "message", duration = 3)
  })

  output$outlierSummary <- shiny::renderUI({
    rep <- outlier_report()
    if (is.null(rep)) return(shiny::p(style="color:#999;font-style:italic;", "Cliquez sur Détecter pour analyser."))
    total <- sum(rep$`Aberrants (n)`)
    shiny::p(shiny::tags$b(total), " valeur(s) aberrante(s) détectée(s) au total.")
  })

  output$outlierTable <- shiny::renderTable({
    rep <- outlier_report()
    shiny::req(rep)
    rep
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "auto",
     align = "lrrrr")

  # La note n'a de sens qu'avec le tableau : l'afficher seule expliquerait des
  # colonnes que l'utilisateur ne voit pas.
  output$outlierNote <- shiny::renderUI({
    if (is.null(outlier_report())) return(NULL)
    shiny::p(style = "font-size:11px;color:#7f8c8d;font-style:italic;margin-top:6px;",
      shiny::icon("info-circle"),
      " Bornes basse/haute = valeurs réelles extrêmes non aberrantes observées dans les données (convention des moustaches du boxplot).")
  })

  shiny::observeEvent(input$applyOutliers, {
    shiny::req(values$cleanData, input$outlierVars)
    action <- input$outlierAction %||% "detect"
    if (action == "detect") {
      rep <- tryCatch(compute_outliers(), error = function(e) NULL)
      outlier_report(rep)
      shiny::showNotification("Mode détection : aucune donnée modifiée.", type = "message", duration = 3)
      return()
    }
    method <- input$outlierMethod %||% "iqr"
    iqr_k <- input$outlierIqrK %||% 1.5
    z_thr <- input$outlierZThresh %||% 3
    d <- values$cleanData
    n_changed <- 0
    shiny::withProgress(message = "Traitement des valeurs aberrantes...", value = 0, {
      if (action == "winsor") {
        # Winsorisation aux quartiles : les valeurs sous le 25e percentile (Q1)
        # sont ramenées à Q1, celles au-dessus du 75e percentile (Q3) à Q3.
        for (cn in input$outlierVars) {
          x <- d[[cn]]
          if (all(is.na(x))) next
          q <- stats::quantile(x, c(.25, .75), na.rm = TRUE, names = FALSE)
          ql <- q[1]; qh <- q[2]
          before <- sum(x < ql | x > qh, na.rm = TRUE)
          x[!is.na(x) & x < ql] <- ql
          x[!is.na(x) & x > qh] <- qh
          d[[cn]] <- x; n_changed <- n_changed + before
        }
      } else {
        for (cn in input$outlierVars) {
          x <- d[[cn]]
          b <- .outlier_bounds(x, method, iqr_k, z_thr)
          if (anyNA(b)) next
          idx <- which(x < b[1] | x > b[2])
          if (action == "tona") { d[[cn]][idx] <- NA; n_changed <- n_changed + length(idx) }
        }
        if (action == "remove") {
          keep <- rep(TRUE, nrow(d))
          for (cn in input$outlierVars) {
            x <- d[[cn]]; b <- .outlier_bounds(x, method, iqr_k, z_thr)
            if (anyNA(b)) next
            # Correction : l'ancienne expression `keep & !out | is.na(x)`
            # re-gardait une ligne deja marquee aberrante des qu'une autre
            # colonne etait NA (priorite de & sur |). Une valeur NA n'est
            # simplement pas consideree comme aberrante pour cette colonne.
            out <- !is.na(x) & (x < b[1] | x > b[2])
            keep <- keep & !out
          }
          n_changed <- sum(!keep)
          d <- d[keep, , drop = FALSE]
        }
      }
      shiny::incProgress(1)
    })
    values$cleanData <- d
    values$filteredData <- d
    outlier_report(tryCatch(compute_outliers(), error = function(e) NULL))
    shiny::showNotification(shiny::tagList(shiny::icon("check"),
      trf(" Traitement appliqué (%d valeur(s)/ligne(s) affectée(s)).", n_changed)),
      type = "message", duration = 4)
  })

  # =========================================================================
  # VARIABLES A VALEURS NULLES
  # -------------------------------------------------------------------------
  # La liste se recalcule sur `values$cleanData` : elle dit l'etat REEL des
  # donnees de travail apres chaque geste, sans bouton « rafraichir » qui
  # pourrait afficher un diagnostic perime.
  # =========================================================================
  zero_message <- shiny::reactiveVal(NULL)

  # Les quatre messages de ce module decrivent un geste pose sur le fichier
  # PRECEDENT : renommage, recodage, valeurs extremes, variables nulles. Ils
  # restaient affiches sous un panneau qui parle desormais d'autres colonnes.
  # `resetSignal` est incremente par les quatre gestes qui remettent la session
  # a zero, chargement d'un nouveau fichier compris.
  shiny::observeEvent(values$resetSignal, {
    if ((values$resetSignal %||% 0) == 0) return()
    rename_msg(NULL); recode_msg(NULL); outlier_report(NULL); zero_message(NULL)
  }, ignoreInit = TRUE)

  zero_table <- shiny::reactive({
    d <- values$cleanData
    if (is.null(d) || !NCOL(d)) return(NULL)
    s <- suppressWarnings(as.numeric(input$zeroSeuil %||% "1"))
    if (!is.finite(s)) s <- 1
    tryCatch(hstat_vars_zero(d, seuil = s), error = function(e) NULL)
  })

  output$zeroVarsResume <- shiny::renderUI({
    z <- zero_table()
    if (is.null(z)) return(shiny::p(style = "color:#999;font-style:italic;",
                             "Chargez des données pour lancer le diagnostic."))
    if (!nrow(z))
      return(shiny::div(class = "alert alert-success", style = "padding:8px;",
                 shiny::icon("check-circle"),
                 " Aucune variable concernée : toutes les variables numériques portent au moins une valeur non nulle."))
    shiny::p(shiny::tags$b(nrow(z)), trf(" variable(s) sur %s concernée(s).", NCOL(values$cleanData)))
  })

  output$zeroVarsTable <- shiny::renderTable({
    z <- zero_table()
    if (is.null(z) || !nrow(z)) return(NULL)
    z
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "auto",
     align = "llrrrrl", digits = 2)

  # Une colonne entierement vide n'est pas une colonne de zeros : la taire
  # laisserait croire qu'elle va bien.
  output$zeroVarsVides <- shiny::renderUI({
    z <- zero_table()
    v <- if (is.null(z)) character(0) else attr(z, "vides")
    if (!length(v)) return(NULL)
    shiny::div(class = "alert alert-warning", style = "padding:8px;margin-top:8px;",
        shiny::icon("exclamation-triangle"),
        trf(" %s variable(s) entièrement vide(s), sans aucune valeur observée : ", length(v)),
        shiny::tags$b(paste(v, collapse = ", ")),
        ". Ce ne sont pas des zéros : il n'y a rien à comparer à zéro.")
  })

  output$zeroVarsSelect <- shiny::renderUI({
    z <- zero_table()
    if (is.null(z) || !nrow(z))
      return(shiny::p(style = "color:#999;font-style:italic;", "Rien à corriger."))
    shiny::tagList(
      shiny::selectInput(ns("zeroVars"), "Variables à traiter :", choices = z$Variable,
                  selected = NULL, multiple = TRUE, selectize = TRUE),
      shiny::div(style = "margin-top:-8px;margin-bottom:12px;",
          shiny::actionButton(ns("zeroSelectAll"), "Tout sélectionner",
                       icon = shiny::icon("check-double"), class = "btn-xs btn-default"),
          shiny::actionButton(ns("zeroSelectNone"), "Tout désélectionner",
                       icon = shiny::icon("eraser"), class = "btn-xs btn-default",
                       style = "margin-left:6px;"),
          shiny::tags$small(style = "color:#7f8c8d;display:block;margin-top:4px;",
                     trf("%s variable(s) listée(s).", nrow(z)))))
  })

  # Un fichier d'enquete peut porter vingt colonnes entierement nulles ; les
  # cocher une a une est precisement le geste qu'on veut eviter.
  shiny::observeEvent(input$zeroSelectAll, {
    z <- zero_table()
    if (is.null(z) || !nrow(z)) return()
    shiny::updateSelectInput(session, "zeroVars", choices = z$Variable,
                      selected = z$Variable)
  })

  # Les choix sont renvoyes avec la deselection : `selected = character(0)`
  # seul est lu par Shiny comme « ne rien changer », et la liste resterait
  # cochee -- l'utilisateur croirait le bouton sans effet.
  shiny::observeEvent(input$zeroSelectNone, {
    z <- zero_table()
    if (is.null(z) || !nrow(z)) return()
    shiny::updateSelectInput(session, "zeroVars", choices = z$Variable,
                      selected = character(0))
  })

  # Saisie directe : la zone est PRE-REMPLIE avec les valeurs actuelles, pour
  # que l'utilisateur corrige au lieu de tout retaper -- et pour que le nombre
  # de lignes attendu soit visible d'emblee.
  output$zeroSaisieUI <- shiny::renderUI({
    v <- input$zeroVars
    d <- values$cleanData
    if (is.null(d) || length(v) != 1)
      return(shiny::div(class = "alert alert-info", style = "padding:8px;",
                 shiny::icon("info-circle"),
                 " Sélectionnez exactement une variable pour saisir ses valeurs."))
    if (NROW(d) > 500)
      return(shiny::div(class = "alert alert-warning", style = "padding:8px;",
                 shiny::icon("exclamation-triangle"),
                 trf(" %s observations : la saisie une à une n'est pas praticable. ", NROW(d)),
                 "Utilisez le remplacement par une valeur, ou corrigez le fichier source."))
    x <- d[[v]]
    txt <- paste(ifelse(is.na(x), "NA", as.character(x)), collapse = "\n")
    shiny::tagList(
      shiny::textAreaInput(ns("zeroSaisie"),
                    trf("Valeurs (%s lignes attendues, une par ligne) :", NROW(d)),
                    value = txt, rows = 10, width = "100%"),
      shiny::tags$small(style = "color:#7f8c8d;",
                 "Une valeur par ligne (ou séparées par des points-virgules). Écrivez NA pour une valeur manquante ; la virgule décimale est acceptée."))
  })

  output$zeroMessage <- shiny::renderUI({
    m <- zero_message()
    if (is.null(m)) return(NULL)
    shiny::div(class = paste("alert", if (isTRUE(m$ok)) "alert-success" else "alert-danger"),
        style = "padding:8px;margin-top:10px;",
        shiny::icon(if (isTRUE(m$ok)) "check" else "exclamation-triangle"), " ", m$texte)
  })

  shiny::observeEvent(input$applyZero, {
    d <- values$cleanData
    if (is.null(d)) {
      zero_message(list(ok = FALSE, texte = "Aucune donnée chargée.")); return()
    }
    vars <- intersect(input$zeroVars %||% character(0), names(d))
    if (!length(vars)) {
      zero_message(list(ok = FALSE,
        texte = "Aucune variable sélectionnée : choisissez-en au moins une dans la liste."))
      return()
    }
    action <- input$zeroAction %||% "na"
    # Le tableau est preleve AVANT le geste : c'est le diagnostic qui l'a
    # motive. Apres coup, les variables corrigees en ont disparu.
    zt <- zero_table()

    if (action == "saisie" && length(vars) != 1) {
      zero_message(list(ok = FALSE,
        texte = "La saisie une à une porte sur une seule variable à la fois."))
      return()
    }
    # Les refus de saisie sont deja des phrases francaises actionnables : les
    # faire passer par hstat_err_fr() les ferait annoncer « message renvoye par
    # R (non traduit) », ce qui est faux et deroutant. hstat_err_fr reste pour
    # les erreurs qui viennent REELLEMENT de R, plus bas.
    val <- NA_real_; saisie <- NULL
    if (action == "valeur") {
      val <- suppressWarnings(as.numeric(input$zeroRemplacement)[1])
      if (!is.finite(val)) {
        zero_message(list(ok = FALSE,
          texte = "Entrez une valeur de remplacement numérique."))
        return()
      }
    } else if (action == "saisie") {
      saisie <- hstat_zero_valeurs_parse(input$zeroSaisie, NROW(d))
      if (!isTRUE(saisie$ok)) {
        zero_message(list(ok = FALSE, texte = saisie$message)); return()
      }
    }

    res <- tryCatch({
      if (action == "supprimer") {
        d <- d[, setdiff(names(d), vars), drop = FALSE]
        list(ok = TRUE, texte = trf("%s variable(s) supprimée(s) : %s.",
                                        length(vars), paste(vars, collapse = ", ")))
      } else if (action == "na") {
        n <- 0L
        for (cn in vars) {
          x <- hstat_as_numeric_fr(d[[cn]])
          if (is.null(x)) next
          idx <- which(!is.na(x) & x == 0)
          n <- n + length(idx)
          x[idx] <- NA_real_
          d[[cn]] <- x
        }
        list(ok = TRUE, texte = trf("%s zéro(s) déclaré(s) manquant(s) sur %s variable(s).",
                                        n, length(vars)))
      } else if (action == "valeur") {
        n <- 0L
        for (cn in vars) {
          x <- hstat_as_numeric_fr(d[[cn]])
          if (is.null(x)) next
          idx <- which(!is.na(x) & x == 0)
          n <- n + length(idx)
          x[idx] <- val
          d[[cn]] <- x
        }
        list(ok = TRUE, texte = trf("%s zéro(s) remplacé(s) par %s sur %s variable(s).",
                                        n, val, length(vars)))
      } else {
        d[[vars[1]]] <- saisie$valeurs
        list(ok = TRUE, texte = trf("« %s » : %s valeur(s) enregistrée(s).",
                                        vars[1], NROW(d)))
      }
    }, error = function(e) list(ok = FALSE, texte = hstat_err_fr(e)))

    if (!isTRUE(res$ok)) { zero_message(res); return() }

    values$cleanData <- d
    values$filteredData <- d
    # `transformationLog` est un registre TYPE : ses entrees sont relues champ
    # par champ (methode, lambda) pour inverser les transformations. Y deposer
    # une phrase casserait son affichage. Le geste est donc capture ici, la ou
    # il a reellement eu lieu.
    hstat_ai_capture(values, "Nettoyage", "Variables à valeurs nulles",
      tables = list("Variables concernées" = zt),
      text = res$texte,
      meta = list(action = action, variables = paste(vars, collapse = ", "),
                  observations = NROW(d)))
    zero_message(res)
    shiny::showNotification(shiny::tagList(shiny::icon("check"), " ", res$texte),
                     type = "message", duration = 4)
  })

  # =========================================================================
  # CLASSES D'INTERVALLES (discrétisation) -- ex. classes d'âge
  # =========================================================================
  output$cutVarSelect <- shiny::renderUI({
    shiny::req(values$cleanData)
    d <- values$cleanData
    # candidates : numeriques + colonnes texte convertibles (format FR)
    is_cand <- vapply(d, function(col)
      is.numeric(col) || !is.null(hstat_as_numeric_fr(col)), logical(1))
    ch <- names(d)[is_cand]
    if (length(ch) == 0)
      return(shiny::div(class = "alert alert-warning",
                 shiny::icon("exclamation-triangle"), " Aucune variable numérique disponible."))
    shiny::selectInput(ns("cutVar"), shiny::tagList(shiny::icon("hashtag"), " Variable numérique à découper"),
                choices = ch, width = "100%")
  })

  output$cutNewNameUI <- shiny::renderUI({
    shiny::req(input$cutVar)
    shiny::textInput(ns("cutNewName"), "Nom de la nouvelle variable",
              value = paste0(input$cutVar, "_classes"), width = "100%")
  })

  # Calcul (partage par l'apercu et l'application)
  cut_result <- shiny::reactive({
    shiny::req(values$cleanData, input$cutVar, input$cutVar %in% names(values$cleanData))
    brks <- NULL
    if (identical(input$cutMethod, "manual")) {
      toks <- strsplit(trimws(input$cutBreaks %||% ""), "[;\\s]+")[[1]]
      if (length(toks) == 1) toks <- strsplit(toks, ",")[[1]]
      toks <- gsub("[,;]+$", "", toks)
      toks <- gsub(",", ".", toks, fixed = TRUE)
      brks <- suppressWarnings(as.numeric(toks[nzchar(toks)]))
    }
    labs <- NULL
    if (identical(input$cutLabels, "custom") && nzchar(trimws(input$cutLabelsTxt %||% ""))) {
      labs <- trimws(strsplit(input$cutLabelsTxt, ",")[[1]])
    }
    labs_use <- if (identical(input$cutLabels, "custom")) labs else NULL
    hstat_cut_intervals(values$cleanData[[input$cutVar]],
                        method = input$cutMethod %||% "manual",
                        n_classes = input$cutNClasses %||% 4,
                        breaks_manual = brks,
                        labels_custom = labs_use,
                        interval_style = input$cutStyle %||% "std_last_closed")
  })

  output$cutPreviewMsg <- shiny::renderUI({
    r <- tryCatch(cut_result(), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    if (!isTRUE(r$ok))
      return(shiny::div(class = "alert alert-danger", style = "padding:8px;",
                 shiny::icon("times-circle"), " ", r$msg))
    shiny::tagList(
      shiny::div(class = "alert alert-success", style = "padding:8px;",
          shiny::icon("check-circle"),
          sprintf(" %d classes -- bornes : %s", nlevels(r$factor),
                  paste(formatC(signif(r$breaks, 4), format = "g"), collapse = " | "))),
      if (!is.null(r$msg))
        shiny::div(class = "alert alert-warning", style = "padding:8px;",
            shiny::icon("exclamation-triangle"), " ", r$msg))
  })

  output$cutPreviewTable <- shiny::renderTable({
    r <- tryCatch(cut_result(), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$ok)) return(NULL)
    r$counts
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")

  output$cutPreviewPlot <- shiny::renderPlot({
    r <- tryCatch(cut_result(), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$ok)) return(NULL)
    d <- r$counts
    ggplot2::ggplot(d, ggplot2::aes(Classe, Effectif, fill = Classe)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d (%.0f%%)", Effectif, Pourcentage)),
                         vjust = -0.3, size = 3.4) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
      ggplot2::labs(x = NULL, y = "Effectif") +
      ggplot2::theme_minimal(base_size = 12)
  })

  shiny::observeEvent(input$applyCut, {
    r <- tryCatch(cut_result(), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$ok)) {
      shiny::showNotification(shiny::tagList(shiny::icon("times"), " ",
        if (!is.null(r)) r$msg else "Paramètres incomplets."), type = "error", duration = 5)
      return()
    }
    new_name <- trimws(input$cutNewName %||% "")
    if (!nzchar(new_name)) new_name <- paste0(input$cutVar, "_classes")
    d <- values$cleanData
    overwrite <- new_name %in% names(d)
    d[[new_name]] <- r$factor
    values$cleanData <- d
    values$filteredData <- d
    values$data <- d
    shiny::showNotification(shiny::tagList(shiny::icon("check"),
      trf(" Variable « %s » créée (%d classes, facteur ordonné)%s.",
              new_name, nlevels(r$factor),
              if (overwrite) tr(" -- ancienne colonne remplacée") else "")),
      type = "message", duration = 5)
  })

  output$cleanedData <- DT::renderDT({
    shiny::req(values$cleanData)
    nsId <- session$ns("")
    # Rappel attache APRES initialisation (initComplete) et protege par try, pour
    # ne jamais interrompre le rendu du tableau ni le reste du module.
    cb <- DT::JS(
      "function(settings, json) {",
      "  try {",
      "    var api = this.api();",
      paste0("    var nsId = '", nsId, "';"),
      "    $(api.table().header()).find('th').css('cursor','pointer')",
      "      .off('dblclick.rn').on('dblclick.rn', function() {",
      "        var current = $(this).text();",
      "        if (!current) { return; }",
      "        var nv = window.prompt('Nouveau nom pour la colonne : ' + current, current);",
      "        if (nv !== null && nv.trim() !== '' && nv.trim() !== current) {",
      "          Shiny.setInputValue(nsId + 'renameColDirect',",
      "            {old: current, nw: nv.trim(), nonce: Math.random()}, {priority: 'event'});",
      "        }",
      "    });",
      "  } catch(e) { console.log('rename header init skipped', e); }",
      "}")
    DT::datatable(
      values$cleanData, 
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        dom = 'Bfrtip',
        buttons = .hstat_dt_buttons("données_nettoyees"),
        initComplete = cb
      ),
      rownames = TRUE,
      class = 'cell-border stripe'
    )
  })

  # Renommage en place declenche par le double-clic sur l'en-tete.
  shiny::observeEvent(input$renameColDirect, {
    info <- input$renameColDirect
    if (is.null(info)) return()
    old <- info$old; new <- trimws(info$nw %||% "")
    if (is.null(old) || !nzchar(new)) return()
    if (!(old %in% names(values$cleanData))) {
      shiny::showNotification(trf("Colonne « %s » introuvable.", old), type = "warning"); return()
    }
    if (new %in% setdiff(names(values$cleanData), old)) {
      shiny::showNotification(trf("Le nom « %s » existe déjà.", new), type = "error"); return()
    }
    for (slot in c("data", "cleanData", "filteredData")) {
      dd <- values[[slot]]
      if (!is.null(dd) && old %in% names(dd)) {
        names(dd)[names(dd) == old] <- new
        values[[slot]] <- dd
      }
    }
    shiny::showNotification(shiny::tagList(shiny::icon("check"), trf(" « %s » renommée en « %s »", old, new)),
                     type = "message", duration = 3)
  })
  })
}
