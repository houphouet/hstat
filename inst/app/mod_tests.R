#  Module Shiny : Tests statistiques + Comparaisons post-hoc (combine)


mod_tests_ui <- function(id) {
  ns <- NS(id)
      tabItem(tabName = "tests",
                .hstat_scope_banner(exact = FALSE),
                fluidRow(
                  box(title = "Paramètres des tests", status = "danger", width = 12, solidHeader = TRUE,
                      fluidRow(
                        column(4,
                               uiOutput(ns("responseVarSelect")),
                               uiOutput(ns("factorVarSelect")),
                               checkboxInput(ns("interaction"), "Inclure les interactions (ANOVA/Scheirer-Ray-Hare)", FALSE),
                               hr(),
                               div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px;",
                                   fluidRow(
                                     column(6,
                                            checkboxInput(ns("testsRoundResults"), "Arrondir les résultats", value = FALSE)
                                     ),
                                     column(6,
                                            conditionalPanel(
                                              ns = ns,
                                              condition = "input.testsRoundResults == true",
                                              numericInput(ns("testsDecimals"), "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                            )
                                     )
                                   )
                               ),
                               # --- Configuration du modele (generalise) mixte ---
                               div(style = "background-color:#e8f6f3; border-left:4px solid #16a085; padding:10px; margin-top:12px;",
                                   h6(tagList(icon("sitemap"), " Modèle mixte (GLMM)"),
                                      style = "color:#6c3483; margin-top:0; font-weight:bold;"),
                                   div(style = "font-size:11px; color:#6c3483; margin-bottom:8px;",
                                       "Paramètres utilisés par le bouton « Modèle (généralisé) mixte »."),
                                   fluidRow(
                                     column(6,
                                            selectInput(ns("glmmEngine"), "Moteur :",
                                                        choices = c("lme4 (g/lmer)" = "lme4",
                                                                    "glmmTMB"       = "glmmTMB"),
                                                        selected = "lme4")
                                     ),
                                     column(6,
                                            selectInput(ns("glmmFamily"), "Famille :",
                                                        choices = c("Gaussienne"          = "gaussian",
                                                                    "Binomiale"           = "binomial",
                                                                    "Poisson"             = "poisson",
                                                                    "Gamma"               = "Gamma",
                                                                    "Binomiale négative"  = "nbinom",
                                                                    "Inverse gaussienne"  = "inverse.gaussian",
                                                                    "Beta (glmmTMB)"      = "beta_family",
                                                                    "Tweedie (glmmTMB)"   = "tweedie"),
                                                        selected = "gaussian")
                                     )
                                   ),
                                   selectInput(ns("glmmLink"), "Fonction de lien :",
                                               choices = c("Automatique (lien canonique)" = "auto",
                                                           "identity" = "identity", "log" = "log",
                                                           "logit"    = "logit",    "probit" = "probit",
                                                           "cloglog"  = "cloglog",  "inverse" = "inverse",
                                                           "sqrt"     = "sqrt"),
                                               selected = "auto"),
                                   uiOutput(ns("glmmFamilyHelp")),
                                   uiOutput(ns("glmmLinkHelp")),
                                   uiOutput(ns("glmmRandomSelect")),
                                   div(style = "font-size:10px; color:#7f8c8d; margin-top:4px;",
                                       icon("circle-info"),
                                       " Effet aléatoire d'ordonnée à l'origine : ", tags$code("(1 | groupe)"), ".")
                               ),
                               # --- Configuration ANOVA a mesures repetees ---
                               div(style = "background-color:#e0f2f1; border-left:4px solid #00897b; padding:10px; margin-top:12px;",
                                   h6(tagList(icon("repeat"), " Mesures répétées (rmANOVA & non param.)"),
                                      style = "color:#00695c; margin-top:0; font-weight:bold;"),
                                   div(style = "font-size:11px; color:#00695c; margin-bottom:8px;",
                                       "Paramètres des boutons « ANOVA à mesures répétées » et « Non paramétrique répété »."),
                                   selectInput(ns("rmSubject"),
                                               tagList(icon("user"), " Sujet / identifiant :"),
                                               choices = NULL),
                                   selectizeInput(ns("rmWithin"),
                                                  tagList(icon("clock"), HTML(" <b>Période</b> (facteur intra-sujet, répété) :")),
                                                  choices = NULL, multiple = TRUE,
                                                  options = list(plugins = list("remove_button"),
                                                                 placeholder = "Ex. : temps, date, stade...")),
                                   selectizeInput(ns("rmBetween"),
                                                  tagList(icon("flask"), HTML(" <b>Traitement</b> (facteur inter-sujet, optionnel) :")),
                                                  choices = NULL, multiple = TRUE,
                                                  options = list(plugins = list("remove_button"),
                                                                 placeholder = "Ex. : traitement, groupe...")),
                                   div(style = "font-size:10px; color:#7f8c8d; margin:-2px 0 6px 0;",
                                       icon("circle-info"),
                                       HTML(" <b>Sujet</b> = unité mesurée plusieurs fois (ex. plante, parcelle). <b>Période</b> = facteur de temps répété sur le même sujet (intra-sujet). <b>Traitement</b> = facteur appliqué (souvent inter-sujet ; mettez-le en intra s'il varie au sein d'un même sujet).")),
                                   selectInput(ns("rmEngine"), "Moteur (paramétrique) :",
                                               choices = c("Modèle mixte (lmer)" = "mixed",
                                                           "afex (aov classique)" = "afex"),
                                               selected = "mixed"),
                                   selectInput(ns("rmNonParam"), "Test non paramétrique :",
                                               choices = c("Friedman (1 facteur intra)"      = "friedman",
                                                           "Durbin (plans incomplets)"       = "durbin",
                                                           "ART (rangs alignés, factoriel)"  = "art"),
                                               selected = "friedman"),
                                   selectInput(ns("rmPostHocAdjust"), "Ajustement post-hoc :",
                                               choices = c("Holm" = "holm", "Bonferroni" = "bonferroni",
                                                           "BH (FDR)" = "BH", "Tukey" = "tukey", "Aucun" = "none"),
                                               selected = "holm")
                               )
                        ),
                        column(4,
                               h4("Tests sur données brutes", style = "color: #3c8dbc;"),
                               div(style="display:flex; flex-direction:column; gap:8px; margin-bottom:12px;",
                                   actionButton(ns("testNormalityRaw"),   "Test de normalité",     class = "btn-warning btn-block", icon = icon("chart-line")),
                                   actionButton(ns("testHomogeneityRaw"), "Test d'homogénéité",    class = "btn-warning btn-block", icon = icon("balance-scale"))
                               ),
                               h4("Tests paramétriques", style = "color: #00a65a;"),
                               div(style="display:flex; flex-direction:column; gap:8px;",
                                   actionButton(ns("testT"),    "Test t de Student",           class = "btn-success btn-block", icon = icon("check")),
                                   actionButton(ns("testANOVA"),"ANOVA",                        class = "btn-success btn-block", icon = icon("check")),
                                   actionButton(ns("testMANOVA"),"MANOVA (>= 2 réponses)",     class = "btn-success btn-block", icon = icon("layer-group")),
                                   actionButton(ns("testLM"),   "Régression linéaire",          class = "btn-success btn-block", icon = icon("check")),
                                   actionButton(ns("testGLM"),  "Modèle linéaire généralisé",   class = "btn-success btn-block", icon = icon("check")),
                                   actionButton(ns("testGLMM"), "Modèle (généralisé) mixte",    class = "btn-success btn-block", icon = icon("sitemap")),
                                   actionButton(ns("testRMAnova"), "ANOVA à mesures répétées",   class = "btn-success btn-block", icon = icon("repeat"))
                               )
                        ),
                        column(4,
                               h4("Tests non-paramétriques", style = "color: #f39c12;"),
                               div(style="display:flex; flex-direction:column; gap:8px;",
                                   actionButton(ns("testWilcox"),          "Test de Wilcoxon",          class = "btn-warning btn-block", icon = icon("check")),
                                   actionButton(ns("testKruskal"),         "Test de Kruskal-Wallis",     class = "btn-warning btn-block", icon = icon("check")),
                                   actionButton(ns("testScheirerRayHare"), "Test de Scheirer-Ray-Hare",  class = "btn-warning btn-block", icon = icon("check")),
                                   actionButton(ns("testRMNonParam"), "Non paramétrique répété",  class = "btn-warning btn-block", icon = icon("repeat")),
                                   actionButton(ns("testPERMANOVA"),       "PERMANOVA (>= 2 réponses)",  class = "btn-warning btn-block", icon = icon("layer-group"))
                               ),
                               # Le Test Chi² / Multinomial a été déplacé dans
                               # « Analyses qualitatives » (famille Nominale) où
                               # il vit désormais avec les tableaux croisés.

                               # --- Comparaison a une valeur de reference (norme) -------------
                               # Tests a UN echantillon : les donnees sont confrontees a une
                               # valeur cible connue au lieu d'etre comparees entre groupes.
                               # La section entiere occupe le bas de cette colonne, dans son
                               # ordre de lecture : d'abord les tests offerts, puis les
                               # reglages qui les pilotent, puis le volet proportions.
                               div(style = "border-left:4px solid #6a1b9a; padding-left:10px; margin-top:16px;",
                                   h4(tagList(icon("bullseye"), " Comparaison à une norme"),
                                      style = "color:#6a1b9a;"),
                                   div(style = "font-size:11px; color:#7f8c8d; margin:-4px 0 8px 0;",
                                       "Confronte les variables réponse à une valeur de référence (réglages ci-dessous)."),
                                   div(style = "display:flex; flex-direction:column; gap:8px;",
                                       actionButton(ns("testRefT"), "Test t (1 échantillon)",
                                                    class = "btn-block", icon = icon("bullseye"),
                                                    style = "background:#6a1b9a; color:#fff; border-color:#59167f;"),
                                       actionButton(ns("testRefZ"), "Test z (écart-type connu)",
                                                    class = "btn-block", icon = icon("bullseye"),
                                                    style = "background:#6a1b9a; color:#fff; border-color:#59167f;"),
                                       actionButton(ns("testRefTOST"), "Équivalence à la norme (TOST)",
                                                    class = "btn-block", icon = icon("arrows-left-right"),
                                                    style = "background:#6a1b9a; color:#fff; border-color:#59167f;"),
                                       actionButton(ns("testRefVar"), "Chi² de conformité (variance)",
                                                    class = "btn-block", icon = icon("wave-square"),
                                                    style = "background:#6a1b9a; color:#fff; border-color:#59167f;"),
                                       actionButton(ns("testRefWilcox"), "Wilcoxon signé (médiane)",
                                                    class = "btn-block", icon = icon("bullseye"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;"),
                                       actionButton(ns("testRefSign"), "Test du signe (médiane)",
                                                    class = "btn-block", icon = icon("bullseye"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;"))),

                               # --- Reglages de la comparaison a une norme --------------------
                               # Places SOUS les boutons ci-dessus : meme accent violet, meme
                               # colonne, l'ensemble se lit d'un seul tenant.
                               div(style = "border-left:4px solid #6a1b9a; padding-left:10px; margin-top:12px;",
                                   h4(tagList(icon("sliders-h"), " Réglages de la norme"),
                                      style = "color:#6a1b9a;"),
                                   numericInput(ns("refValue"),
                                                tagList(icon("bullseye"), " Valeur de référence :"),
                                                value = 0),
                                   fluidRow(
                                     column(6, numericInput(ns("refSigma"),
                                                tagList(icon("wave-square"), " Écart-type (test z / variance)"),
                                                value = NULL, min = 0)),
                                     column(6, numericInput(ns("refMargin"),
                                                tagList(icon("arrows-left-right"), " Marge (TOST)"),
                                                value = NULL, min = 0))),
                                   selectInput(ns("refAlt"), "Hypothèse alternative :",
                                               choices = c("Bilatérale (différente de la norme)" = "two.sided",
                                                           "Unilatérale : supérieure à la norme" = "greater",
                                                           "Unilatérale : inférieure à la norme" = "less"),
                                               selected = "two.sided"),
                                   sliderInput(ns("refConf"), "Niveau de confiance :",
                                               min = 0.80, max = 0.99, value = 0.95, step = 0.01),
                                   div(style = "font-size:11px; color:#7f8c8d; margin-top:-4px;",
                                       icon("circle-info"),
                                       paste(" L'écart-type ne sert qu'au test z et au Chi² de variance ;",
                                             "la marge, qu'au test d'équivalence.")),
                                   tags$hr(style = "margin:10px 0;"),
                                   h5(tagList(icon("percent"), " Proportion / taux vs norme"),
                                      style = "color:#6a1b9a; font-weight:bold;"),
                                   uiOutput(ns("refPropVarSelect")),
                                   uiOutput(ns("refPropLevelSelect")),
                                   numericInput(ns("refPropP0"),
                                                "Proportion (ou taux) de référence :",
                                                value = 0.5, min = 0, step = 0.01),
                                   div(style = "display:flex; flex-direction:column; gap:8px;",
                                       actionButton(ns("testRefBinom"), "Test binomial exact",
                                                    class = "btn-block", icon = icon("percent"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;"),
                                       actionButton(ns("testRefProp"), "Chi² de conformité (proportion)",
                                                    class = "btn-block", icon = icon("percent"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;"),
                                       actionButton(ns("testRefPoisson"), "Test de Poisson (taux)",
                                                    class = "btn-block", icon = icon("percent"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;")),
                                   div(style = "font-size:11px; color:#7f8c8d; margin-top:6px;",
                                       icon("circle-info"),
                                       paste(" Pour Poisson, la référence est un taux d'événements",
                                             "par observation, non une proportion bornée à 1.")))
                        )
                      )
                  )
                ),

                # RESULTAT DETAILLE DE LA COMPARAISON A UNE NORME
                # Les reglages et les boutons vivent desormais dans la boite
                # « Parametres des tests » ; seul le detail du resultat
                # (estimation, intervalle de confiance, taille d'effet) apparait
                # ici, et uniquement apres execution d'un test de conformite.
                conditionalPanel(
                  ns = ns,
                  condition = "output.hasRefTest",
                  fluidRow(
                    box(title = tagList(icon("bullseye"),
                                        " Détail de la comparaison à la référence"),
                        status = "primary", width = 12, solidHeader = TRUE,
                        collapsible = TRUE,
                        DTOutput(ns("refTestDetails")),
                        uiOutput(ns("refTestNote")))
                  )
                ),

                # BOX MANOVA / PERMANOVA -- Assistant guide visible apres execution

                conditionalPanel(
                  ns = ns,
                  condition = "output.showManovaWorkflow",
                  fluidRow(
                    div(id = "boxWrap_manovaAssist",
                        box(
                          title = tagList(icon("layer-group"),
                                          " Analyse multivariee assistee (MANOVA / PERMANOVA)"),
                          status = "success", width = 12, solidHeader = TRUE,
                          collapsible = TRUE, collapsed = TRUE,
                        
                          tabsetPanel(id = "manovaAssistantTabs", type = "tabs",
                                    
                                      tabPanel(
                                        title = tagList(icon("magic"), " 1. Diagnostic & recommandation"),
                                        value = "manova_recommendation", br(),
                                      
                                        conditionalPanel(
                                          ns = ns,
                                          condition = "output.hasManovaRecommendation",
                                          uiOutput(ns("manovaRecommendationCard")),
                                          uiOutput(ns("manovaOutliersCard")),
                                          br()
                                        ),
                                        conditionalPanel(
                                          ns = ns,
                                          condition = "!output.hasManovaRecommendation",
                                          div(style = "padding:30px; text-align:center; color:#888;",
                                              icon("magic", style = "font-size:48px; opacity:0.3;"),
                                              h4("Aucune recommandation calculée"),
                                              p("Cliquez sur ", strong("'Diagnostiquer mes données'"),
                                                " ci-dessous pour obtenir une recommandation automatique."))
                                        )
                                      ),
                                    
                                      tabPanel(
                                        title = tagList(icon("clipboard-check"), " 2. Details techniques"),
                                        value = "manova_prereq", br(),
                                      
                                        div(style = "background:#fff8e1; border-left:4px solid #fb8c00; padding:10px 14px; border-radius:6px; margin-bottom:12px; font-size:12px;",
                                            icon("info-circle", style = "color:#e65100;"),
                                            strong(" Pour les utilisateurs avances : "),
                                            "consultez les valeurs brutes des tests de prerequis. ",
                                            "L'assistant a déjà synthétisé ces résultats dans l'onglet 'Diagnostic & recommandation'."
                                        ),
                                      
                                        conditionalPanel(
                                          ns = ns,
                                          condition = "output.hasManovaParam",
                                          h5(icon("table"), " 4 statistiques MANOVA",
                                             style = "color:#00a65a; margin-top:0;"),
                                          withSpinner(DTOutput(ns("manovaParamTable")), color = "#00a65a"),
                                          br()
                                        ),
                                      
                                        conditionalPanel(
                                          ns = ns,
                                          condition = "output.hasManovaPermanova",
                                          h5(icon("random"), " Résultats PERMANOVA (par permutations)",
                                             style = "color:#f39c12; margin-top:0;"),
                                          div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                                              icon("info-circle"),
                                              " pseudo-F, R² (part de variance expliquée), p-value par permutations. Interactions incluses si l'option est cochée."),
                                          withSpinner(DTOutput(ns("manovaPermanovaTable")), color = "#f39c12"),
                                          br()
                                        ),
                                      
                                        h5(icon("chart-area"), " Normalite multivariee (Mardia)",
                                           style = "color:#1565C0; margin-top:0;"),
                                        withSpinner(DTOutput(ns("manovaMardiaTable")), color = "#1565C0"),
                                        uiOutput(ns("manovaMardiaInterpretation")),
                                        br(),
                                      
                                        h5(icon("balance-scale"), " Homogeneite des covariances (Box\'s M)",
                                           style = "color:#1565C0;"),
                                        withSpinner(DTOutput(ns("manovaBoxMTable")), color = "#1565C0"),
                                        uiOutput(ns("manovaBoxMInterpretation")),
                                        br(),
                                      
                                        h5(icon("project-diagram"), " Homogeneite des dispersions (PERMDISP)",
                                           style = "color:#f39c12;"),
                                        div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                                            icon("info-circle"), " Equivalent multivarie non parametrique du test de Levene."),
                                        withSpinner(DTOutput(ns("manovaPermDispTable")), color = "#f39c12"),
                                        uiOutput(ns("manovaPermDispInterpretation"))
                                      ),
                                    
                                      tabPanel(
                                        title = tagList(icon("brain"), " 3. Décomposition des effets"),
                                        value = "manova_interprétation", br(),
                                      
                                        uiOutput(ns("manovaInterpretationGuidance")),
                                      
                                        conditionalPanel(
                                          ns = ns,
                                          condition = "output.hasManovaInteraction",
                                          br(),
                                          div(style = "background:#fff3e0; border:2px solid #fb8c00; border-radius:8px; padding:14px 18px; margin-top:14px;",
                                              h4(icon("project-diagram"),
                                                 " Decomposition de l\'interaction (effets simples)",
                                                 style = "color:#e65100; margin-top:0;"),
                                              p(style = "color:#555; font-size:13px;",
                                                "Une interaction est significative : l\'effet d\'un facteur depend du niveau de l\'autre. ",
                                                "Choisissez un facteur a ", em("fixer"), " et un facteur a ", em("tester"),
                                                ", puis cliquez ", strong("Calculer"), "."),
                                              uiOutput(ns("manovaSimpleEffectsSelectors")),
                                              br(),
                                              conditionalPanel(
                                                ns = ns,
                                                condition = "output.hasManovaSimpleEffects",
                                                withSpinner(DTOutput(ns("manovaSimpleEffectsTable")), color = "#fb8c00"),
                                                div(style = "font-size:11px; color:#888; margin-top:8px;",
                                                    icon("info-circle"),
                                                    " Les p-valeurs sont ajustees par Bonferroni sur l\'ensemble des niveaux fixes.")
                                              )
                                          )
                                        )
                                      )
                          ),
                        
                          br(),
                        
                          fluidRow(
                            column(4,
                                   div(style = "background:#e3f2fd; padding:12px 14px; border-radius:8px;",
                                       h6(icon("magic"), " Diagnostic automatique",
                                          style = "margin-top:0; color:#1565C0; font-weight:bold;"),
                                       p(style = "font-size:11px; color:#555; margin-bottom:8px;",
                                         "Verifie les prerequis et recommande le test optimal."),
                                       actionButton(ns("runManovaDiagnostic"),
                                                    tagList(icon("magic"), " Diagnostiquer mes données"),
                                                    class = "btn-primary btn-block",
                                                    style = "font-weight:bold;")
                                   )
                            ),
                            column(4,
                                   conditionalPanel(
                                     ns = ns,
                                     condition = "output.hasManovaParam",
                                     downloadButton(ns("downloadManovaParam"),
                                                    tagList(icon("file-excel"), " Télécharger MANOVA (.xlsx)"),
                                                    class = "btn-success btn-block",
                                                    style = "margin-top:42px;")
                                   )
                            ),
                            column(4,
                                   conditionalPanel(
                                     ns = ns,
                                     condition = "output.hasManovaPermanova",
                                     downloadButton(ns("downloadManovaPermanova"),
                                                    tagList(icon("file-excel"), " Télécharger PERMANOVA (.xlsx)"),
                                                    class = "btn-success btn-block",
                                                    style = "margin-top:42px;")
                                   )
                            )
                          )
                        )
                    )
                  )
                ),
              
                conditionalPanel(
                  ns = ns,
                  condition = "!output.showManovaWorkflow",
                  fluidRow(
                    div(id = "boxWrap_manovaPlaceholder",
                        box(
                          title = tagList(icon("magic"), " Analyse multivariee assistee"),
                          status = "info", width = 12, solidHeader = TRUE,
                          collapsible = TRUE, collapsed = TRUE,
                          div(style = "padding:20px; text-align:center;",
                              icon("magic", style = "font-size:48px; color:#1565C0; opacity:0.6;"),
                              h4("Workflow pour debutants et experts",
                                 style = "color:#1565C0;"),
                              p(style = "font-size:13px; color:#555; max-width:600px; margin:8px auto;",
                                "Sélectionnez au moins ", strong("2 variables réponses numériques"),
                                " et ", strong("1 facteur"), " dans \'Paramètres des tests\'. ",
                                "Puis cliquez sur le bouton ci-dessous pour un diagnostic complet et une recommandation automatique."),
                              br(),
                              actionButton(ns("runManovaDiagnostic"),
                                           tagList(icon("magic"), " Diagnostiquer mes données"),
                                           class = "btn-primary btn-lg",
                                           style = "padding:10px 30px; font-weight:bold;")
                          )
                        )
                    )
                  )
                ),
              
              
                fluidRow(
                  box(
                    title = div(
                      icon("magic", style = "color:#f57c00; margin-right:6px;"),
                      tags$span("Transformation des variables",
                                style = "font-size:14px; font-weight:bold;"),
                      tags$span(
                        style = paste0("font-size:10px; font-weight:normal; color:#fff;",
                                       "background:#ef6c00; padding:2px 7px;",
                                       "border-radius:10px; margin-left:8px;"),
                        "Tests paramétriques uniquement"
                      )
                    ),
                    status = "warning", width = 12,
                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  
                    div(
                      style = paste0("padding:10px 14px;background:#fff3e0;",
                                     "border-left:4px solid #fb8c00;border-radius:4px;",
                                     "margin-bottom:14px;font-size:12px;"),
                      icon("lightbulb", style = "color:#e65100;"),
                      tags$b(style = "color:#bf360c;", " Quand utiliser ?"),
                      tags$br(),
                      tags$span(style = "color:#6d4c41;",
                                "Après avoir testé la normalité/homogénéité -- si les conditions ne sont ",
                                tags$b("pas"), " remplies, appliquez une transformation.",
                                " La variable transformée apparaît dans les sélecteurs.",
                                " Retestez ensuite les conditions sur la variable transformée."
                      )
                    ),
                  
                    fluidRow(
                    
                      # Col 1 : Sélection + méthode + bouton
                      column(4,
                             h5(icon("sliders-h"), " Variable & méthode",
                                style = "color:#e65100;margin-top:0;border-bottom:2px solid #ffcc80;padding-bottom:6px;"),
                             uiOutput(ns("transformVarSelect")),
                             br(),
                             selectInput(ns("transformMethod"),
                               tags$span(icon("flask"), " Transformation :"),
                               choices = list(
                                 "─── Asymétrie positive (rendements, concentrations) ───" = list(
                                   "Logarithme naturel  log(x)  [x > 0]"          = "log",
                                   "log(x+1) — tolère les zéros  [x ≥ 0]"        = "log1p",
                                   "Log base 10  log10(x)  [x > 0]"              = "log10"
                                 ),
                                 "─── Comptage / Asymétrie modérée ───" = list(
                                   "Racine carrée  sqrt(x)  [x ≥ 0]"            = "sqrt",
                                   "Racine cubique  x^(1/3)  [toutes valeurs]"   = "cuberoot"
                                 ),
                                 "─── Transformations optimales (automatiques) ───" = list(
                                   "Box-Cox  (λ optimal MV)  [x > 0]"             = "boxcox",
                                   "Yeo-Johnson  (bestNormalize)  [toutes valeurs]" = "yeojohnson"
                                 ),
                                 "─── Proportions & taux ───" = list(
                                   "Arcsinus  asin(sqrt(x))  [0 ≤ x ≤ 1]"       = "arcsin",
                                   "Logit  log(p/(1-p))  [0 < x < 1]"           = "logit"
                                 )
                               ),
                               selected = "log"
                             ),
                             uiOutput(ns("transformFeasibilityCheck")),
                             br(),
                             actionButton(ns("applyTransformation"),
                               HTML("<i class='fa fa-magic'></i>&nbsp;<b>Appliquer la transformation</b>"),
                               class = "btn-warning btn-lg btn-block",
                               style = "height:50px;box-shadow:0 3px 5px rgba(0,0,0,0.2);"
                             )
                      ),
                    
                      # Col 2 : Journal des transformations actives
                      column(4,
                             h5(icon("history"), " Transformations actives",
                                style = "color:#e65100;margin-top:0;border-bottom:2px solid #ffcc80;padding-bottom:6px;"),
                             div(style = "min-height:120px;", uiOutput(ns("transformationLogDisplay"))),
                             uiOutput(ns("removeTransformSelect"))
                      ),
                    
                      # Col 3 : Guide de sélection
                      column(4,
                             h5(icon("book-open"), " Guide de sélection",
                                style = "color:#e65100;margin-top:0;border-bottom:2px solid #ffcc80;padding-bottom:6px;"),
                             tags$table(
                               style = "width:100%;border-collapse:collapse;font-size:11px;",
                               tags$thead(
                                 tags$tr(
                                   style = "background:#ef6c00;color:white;",
                                   tags$th(style = "padding:4px 6px;", "Méthode"),
                                   tags$th(style = "padding:4px 6px;", "Données"),
                                   tags$th(style = "padding:4px 6px;text-align:center;", "Négatifs ?")
                                 )
                               ),
                               tags$tbody(
                                 tags$tr(style="background:#fff8e1;",
                                         tags$td(style="padding:3px 6px;font-family:monospace;","log(x)"),
                                         tags$td(style="padding:3px 6px;","Très asym., rendements"),
                                         tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", icon("times"))),
                                 tags$tr(style="background:#fffff0;",
                                         tags$td(style="padding:3px 6px;font-family:monospace;","log(x+1)"),
                                         tags$td(style="padding:3px 6px;","Idem + zéros"),
                                         tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", icon("times"))),
                                 tags$tr(style="background:#fff8e1;",
                                         tags$td(style="padding:3px 6px;font-family:monospace;","sqrt(x)"),
                                         tags$td(style="padding:3px 6px;","Comptage, Poisson"),
                                         tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", icon("times"))),
                                 tags$tr(style="background:#fffff0;",
                                         tags$td(style="padding:3px 6px;font-family:monospace;","x^(1/3)"),
                                         tags$td(style="padding:3px 6px;","Toutes valeurs"),
                                         tags$td(style="padding:3px 6px;text-align:center;color:#43a047;", icon("check"))),
                                 tags$tr(style="background:#fff8e1;",
                                         tags$td(style="padding:3px 6px;font-family:monospace;","Box-Cox"),
                                         tags$td(style="padding:3px 6px;","λ optimal (MV)"),
                                         tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", icon("times"))),
                                 tags$tr(style="background:#fffff0;",
                                         tags$td(style="padding:3px 6px;font-family:monospace;","Yeo-Johnson"),
                                         tags$td(style="padding:3px 6px;","Optimale généralisée"),
                                         tags$td(style="padding:3px 6px;text-align:center;color:#43a047;", icon("check"))),
                                 tags$tr(style="background:#fff8e1;",
                                         tags$td(style="padding:3px 6px;font-family:monospace;","asin(√x)"),
                                         tags$td(style="padding:3px 6px;","Proportions [0,1]"),
                                         tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", icon("times"))),
                                 tags$tr(style="background:#fffff0;",
                                         tags$td(style="padding:3px 6px;font-family:monospace;","logit"),
                                         tags$td(style="padding:3px 6px;","Taux ]0,1["),
                                         tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", icon("times")))
                               )
                             ),
                             br(),
                             div(
                               style = paste0("padding:8px 10px;background:#e8f5e9;",
                                              "border-left:3px solid #43a047;border-radius:4px;font-size:11px;"),
                               icon("route", style = "color:#2e7d32;"),
                               tags$b(style = "color:#1b5e20;", " Workflow :"),
                               tags$ol(
                                 style = "margin:4px 0 0 0;padding-left:16px;color:#33691e;line-height:1.6;",
                                 tags$li("Tester normalité (données brutes)"),
                                 tags$li("Si p < 0.05 → transformer"),
                                 tags$li("Retester sur var. transformée"),
                                 tags$li("Lancer le test paramétrique"),
                                 tags$li("PostHoc sur var. transformée")
                               )
                             )
                      )
                    )  # fin fluidRow interne
                  )  # fin box transformation
                ),
              
                fluidRow(
                  box(title = "Résultats des tests", status = "danger", width = 12, solidHeader = TRUE,
                      DTOutput(ns("testResultsDF")),
                      br(),
                      downloadButton(ns("downloadTestsExcel"), "Télécharger les résultats (Excel)", class = "btn-info"))
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "output.showParametricDiagnostics",
                  fluidRow(
                    box(title = "Diagnostics des modèles", status = "info", width = 6, solidHeader = TRUE,
                        conditionalPanel(
                          ns = ns,
                          condition = "output.showModelNavigation",
                          wellPanel(
                            h6("Navigation des modèles", style = "margin-top: 0; margin-bottom: 10px;"),
                            div(style = "text-align: center;",
                                uiOutput(ns("modelDiagNavigation"))
                            )
                          )
                        ),
                        plotOutput(ns("modelDiagnostics"), height = "500px"),
                        br(),
                        downloadButton(ns("downloadModelDiagnostics"), "Télécharger (PNG)", class = "btn-success"),
                        htmlOutput("modelDiagnosticsInterpretation")
                    ),
                    box(title = "Résidus et validation", status = "info", width = 6, solidHeader = TRUE,
                        conditionalPanel(
                          ns = ns,
                          condition = "output.showResidNavigation",
                          wellPanel(
                            h6("Navigation des variables", style = "margin-top: 0; margin-bottom: 10px;"),
                            div(style = "text-align: center;",
                                uiOutput(ns("residNavigation"))
                            )
                          )
                        ),
                        tabBox(
                          title = "Analyses des résidus",
                          id = "residualTabs", width = 12,
                          tabPanel("QQ-plot", 
                                   plotOutput(ns("qqPlotResiduals"), height = "320px"),
                                   br(),
                                   downloadButton(ns("downloadQQPlot"), "Télécharger (PNG)", class = "btn-success"),
                                   htmlOutput("qqPlotInterpretation")),
                          tabPanel("Normalité", 
                                   verbatimTextOutput(ns("normalityResult")),
                                   htmlOutput("normalityResidInterpretation")),
                          tabPanel("Homogénéité", 
                                   verbatimTextOutput(ns("leveneResidResult")),
                                   htmlOutput("homogeneityResidInterpretation")),
                          tabPanel("Autocorrélation", 
                                   verbatimTextOutput(ns("autocorrResult")),
                                   htmlOutput("autocorrInterpretation")),
                          tabPanel("Summary", verbatimTextOutput(ns("modelSummary")))
                        )
                    )
                  )
                )
      )
}

mod_posthoc_ui <- function(id) {
  ns <- NS(id)
      tabItem(tabName = "multiple",
                .hstat_scope_banner(exact = FALSE),
                fluidRow(
                
                
                  box(title = div(icon("cog"), " Configuration de l'analyse"), 
                      status = "primary", width = 4, solidHeader = TRUE,
                    
                    
                      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                          h4(style = "color: #2c3e50; margin-top: 0;", icon("chart-line"), " Sélection des variables"),
                          uiOutput(ns("multiResponseSelect")),
                          uiOutput(ns("multiFactorSelect")),
                          # Bandeau info transformations actives (affiché si variables transformées sélectionnées)
                          uiOutput(ns("postHocTransformInfo"))
                      ),
                    
                    
                      div(style = "background-color: #fef9e7; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                          h4(style = "color: #2c3e50; margin-top: 0;", icon("hashtag"), " Affichage des résultats"),
                          checkboxInput(ns("multiRoundResults"), "Arrondir les résultats numériques", value = FALSE),
                          conditionalPanel(
                            ns = ns,
                            condition = "input.multiRoundResults == true",
                            numericInput(ns("multiDecimals"), "Nombre de décimales :",
                                         value = 2, min = 0, max = 8, step = 1)
                          ),
                          helpText(style = "font-size: 11px; color: #7f8c8d;",
                                   "Si décoché, les valeurs s'affichent sans arrondi.")
                      ),
                    
                    
                      div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                          h4(style = "color: #2c3e50; margin-top: 0;", icon("vial"), " Tests statistiques"),
                          radioButtons(ns("testType"), "Type de comparaisons",
                                       choiceNames = list(
                                         HTML("<b>Paramétrique</b> <small style='color:#7f8c8d;'>- Données normales</small>"), 
                                         HTML("<b>Non paramétrique</b> <small style='color:#7f8c8d;'>- Sans normalité</small>")
                                       ),
                                       choiceValues = list("param", "nonparam"),
                                       selected = "param"
                          ),
                          conditionalPanel(
                            ns = ns,
                            condition = "input.testType == 'param'",
                            selectInput(ns("multiTest"), "Méthode post-hoc paramétrique",
                                        choices = list(
                                          "Tukey HSD (recommandé)" = "tukey", 
                                          "LSD (Fisher)" = "lsd", 
                                          "Duncan" = "duncan", 
                                          "SNK (Student-Newman-Keuls)" = "snk",
                                          "Scheffe (conservateur)" = "scheffe",
                                          "REGW" = "regw",
                                          "Waller-Duncan" = "waller",
                                          "Bonferroni" = "bonferroni",
                                          "Dunnett" = "dunnett", 
                                          "Games-Howell (variances inégales)" = "games",
                                          "MANOVA paramétrique (multivarié, >= 2 réponses)" = "manova",
                                          "LM / GLM (emmeans + lettres CLD)" = "lm_emmeans"
                                        ),
                                        selected = "tukey"
                            ),
                            selectInput(ns("multiParamAdjust"),
                                        tagList(icon("sliders-h"), " Ajustement des p-values (homogénéisation des groupes)"),
                                        choices = c("Holm" = "holm", "Bonferroni" = "bonferroni",
                                                    "BH (FDR)" = "BH", "BY" = "BY",
                                                    "Hochberg" = "hochberg", "Hommel" = "hommel",
                                                    "Aucun" = "none"),
                                        selected = "holm"),
                            div(style = "font-size:11px;color:#7f8c8d;margin-top:-6px;margin-bottom:8px;",
                                icon("info-circle"),
                                HTML(" S'applique aux méthodes par comparaisons de paires (LSD, Bonferroni, LM/GLM). Un ajustement plus strict (Bonferroni, Holm) rend les groupes plus homogènes ; les méthodes à contrôle intégré (Tukey, Duncan, SNK, Scheffé, Games-Howell) conservent le leur."))
                          ),
                          conditionalPanel(
                            ns = ns,
                            condition = "input.testType == 'nonparam'",
                            selectInput(ns("multiTestNonParam"), "Méthode post-hoc non paramétrique",
                                        choices = list(
                                          "Kruskal-Wallis (base)" = "kruskal",
                                          "Dunn" = "dunn",
                                          "Conover" = "conover",
                                          "Nemenyi" = "nemenyi",
                                          "PERMANOVA pairwise (multivarié, >= 2 réponses)" = "permanova"
                                        ),
                                        selected = "dunn"
                            ),
                            selectInput(ns("multiNonParamAdjust"),
                                        tagList(icon("sliders-h"), " Ajustement des p-values (homogénéisation des groupes)"),
                                        choices = c("Holm" = "holm", "Bonferroni" = "bonferroni",
                                                    "BH (FDR)" = "BH", "BY" = "BY",
                                                    "Hochberg" = "hochberg", "Hommel" = "hommel",
                                                    "Aucun" = "none"),
                                        selected = "holm")
                          ),
                          # Option retro-transformation -- visible uniquement si des variables
                          # transformées sont sélectionnées dans multiResponse
                          conditionalPanel(
                            ns = ns,
                            condition = "output.hasTransformedVarsSelected",
                            div(
                              style = paste0("margin-top:10px;padding:10px 12px;",
                                             "background:#fff8e1;border:1px solid #ffb300;",
                                             "border-radius:6px;"),
                              checkboxInput(ns("showBackTransformed"),
                                HTML(paste0(
                                  "<b style='color:#e65100;'>",
                                  "<i class='fa fa-exchange-alt'></i>&nbsp;",
                                  "Retro-transformer les moyennes</b><br>",
                                  "<small style='color:#6d4c41;font-weight:normal;'>",
                                  "Affiche les moyennes sur l'échelle originale (interprétation).",
                                  "<br>Les lettres de comparaison restent sur l'échelle transformée.",
                                  "</small>"
                                )),
                                value = FALSE
                              )
                            )
                          )
                      ),
                    
                    
                      div(style = "border: 3px solid #e74c3c; border-radius: 8px; padding: 15px; margin-bottom: 15px; background: linear-gradient(135deg, #fff5f5 0%, #ffe8e8 100%);",
                          h4(style = "color: #c0392b; margin-top: 0;", 
                             icon("project-diagram"), " Analyse des interactions"),
                          checkboxInput(ns("posthocInteraction"), 
                                        HTML("<strong style='color: #c0392b;'>Activer l'analyse des interactions</strong>"), 
                                        value = FALSE),
                          conditionalPanel(
                            ns = ns,
                            condition = "input.posthocInteraction == true",
                            div(style = "margin-top: 8px; padding: 8px 10px; background:#fff3e0; border-left:3px solid #ff9800; border-radius:4px;",
                                tags$small(style="color:#e65100;",
                                           icon("info-circle"), " Sélectionnez >= 2 facteurs. Les effets simples s'affichent dans l'onglet 'Effets simples'."
                                )
                            )
                          )
                      ),

                      div(style = "border: 2px solid #16a085; border-radius: 8px; padding: 12px 15px; margin-bottom: 15px; background:#eafaf3;",
                          checkboxInput(ns("posthocMultivariate"),
                                        HTML("<strong style='color:#0e6655;'>Calculer aussi le post-hoc multivarié (MANOVA / PERMANOVA)</strong>"),
                                        value = FALSE),
                          tags$small(style = "color:#0e6655;",
                                     icon("info-circle"),
                                     " Décoché par défaut. Cette analyse (permutations) ne se lance PAS automatiquement avec le post-hoc ANOVA ; cochez-la seulement si vous voulez les comparaisons multivariées (nécessite >= 2 variables réponses).")
                      ),
                    
                      hr(),
                    
                    
                      actionButton(ns("runMultiple"), 
                                   HTML("<h5 style='margin: 5px 0;'><i class='fa fa-play'></i> LANCER L'ANALYSE</h5>"), 
                                   class = "btn-success btn-lg", 
                                   style = "width: 100%; height: 70px; font-weight: bold; box-shadow: 0 4px 6px rgba(0,0,0,0.2);"),
                    
                      br(), br(),
                    
                  ),
                
                
                  box(title = div(icon("table"), " Résultats et visualisations"), 
                      status = "primary", width = 8, solidHeader = TRUE,
                    
                      tabsetPanel(id = "resultsTabs", type = "tabs",
                                
                                  # ONGLET 1 : Effets principaux 
                                
                                  tabPanel(
                                    title = div(icon("layer-group"), " Effets principaux"),
                                    value = "mainEffects",
                                    br(),
                                    conditionalPanel(
                                      ns = ns,
                                      condition = "output.showPosthocResults",
                                      div(style = "margin-bottom: 15px;",
                                          uiOutput(ns("analysisSummaryMain"))
                                      ),
                                      div(class = "hstat-table-scroll",
                                          DTOutput(ns("mainEffectsTable"))),
                                      br(),
                                      downloadButton(ns("downloadMainEffects"), 
                                                     "Télécharger effets principaux (.xlsx)", 
                                                     class = "btn-success", 
                                                     style = "width: 100%; height: 50px; font-weight: bold;",
                                                     icon = icon("download"))
                                    )
                                  ),
                                
                                  # ONGLET 2 : Effets simples 
                                
                                  tabPanel(
                                    title = div(icon("project-diagram"), " Effets simples"),
                                    value = "simpleEffects",
                                    br(),
                                    conditionalPanel(
                                      ns = ns,
                                      condition = "output.showSimpleEffects",
                                      div(style = "background: linear-gradient(135deg, #fff5f5 0%, #ffe8e8 100%); padding: 15px; border-radius: 8px; border-left: 5px solid #e74c3c; margin-bottom: 15px;",
                                          h4(style = "color: #c0392b; margin-top: 0;", 
                                             icon("info-circle"), " Interprétation des effets simples"),
                                          HTML("<div style='color: #34495e;'>
                                         <p><b>Objectif :</b> Décomposer les interactions significatives en comparaisons plus simples.</p>
                                       
                                         <p><b>Lecture du format :</b><br/>
                                         <code style='background:#fff;padding:2px 6px;border-radius:3px;'>Facteur testé | Facteur fixé = niveau</code></p>
                                       
                                         <p><b>Exemples concrets :</b></p>
                                         <ul style='margin-left: 20px;'>
                                           <li><code style='background:#fff;padding:2px 6px;'>Traitement | Temps=T0</code><br/>
                                               -> Compare les traitements <u>au temps T0 uniquement</u></li>
                                           <li><code style='background:#fff;padding:2px 6px;'>Temps | Traitement=Ctrl</code><br/>
                                               -> Compare les temps <u>pour le contrôle uniquement</u></li>
                                         </ul>
                                       
                                         <p><b>Utilité :</b> Identifier <i>où précisément</i> les facteurs diffèrent lorsqu'ils interagissent.</p>
                                         </div>")
                                      ),
                                    
                                      fluidRow(
                                        column(6,
                                               div(style = "background:#f8f9fa; padding:10px; border-radius:5px;",
                                                   selectInput(ns("filterSimpleEffectVar"), 
                                                               HTML("<b>Filtrer par variable</b>"),
                                                               choices = NULL,
                                                               width = "100%")
                                               )
                                        ),
                                        column(6,
                                               div(style = "background:#f8f9fa; padding:10px; border-radius:5px;",
                                                   selectInput(ns("filterSimpleEffectInteraction"), 
                                                               HTML("<b>Filtrer par interaction</b>"),
                                                               choices = NULL,
                                                               width = "100%")
                                               )
                                        )
                                      ),
                                    
                                      br(),
                                      uiOutput(ns("simpleEffectsSummary")),
                                      div(class = "hstat-table-scroll",
                                          DTOutput(ns("simpleEffectsTable"))),
                                      br(),
                                    
                                      downloadButton(ns("downloadSimpleEffects"), 
                                                     "Télécharger effets simples (.xlsx)", 
                                                     class = "btn-success",
                                                     style = "width: 100%; height: 50px; font-weight: bold;",
                                                     icon = icon("download"))
                                    ),
                                    conditionalPanel(
                                      ns = ns,
                                      condition = "!output.showSimpleEffects",
                                      div(style = "text-align: center; padding: 50px; color: #95a5a6;",
                                          icon("project-diagram", style = "font-size: 4em; opacity: 0.3;"),
                                          h4("Aucun effet simple détecté"),
                                          p("Les effets simples apparaissent uniquement quand :"),
                                          tags$ul(style = "text-align: left; display: inline-block;",
                                                  tags$li("L'option 'Analyse des interactions' est activée"),
                                                  tags$li("Au moins 2 facteurs sont sélectionnés"),
                                                  tags$li("Une interaction est significative (p < 0.05)")
                                          )
                                      )
                                    )
                                  ),
                                
                                  # ONGLET 3 : Visualisations 
                                
                                  tabPanel(
                                    title = div(icon("chart-bar"), " Graphiques"),
                                    value = "plots",
                                    br(),
                                  
                                    conditionalPanel(
                                      ns = ns,
                                      condition = "output.showVariableNavigation",
                                      wellPanel(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; color: white;",
                                                div(style = "display: flex; align-items: center; justify-content: center;",
                                                    uiOutput(ns("variableNavigation"))
                                                )
                                      )
                                    ),
                                  
                                    fluidRow(
                                      column(6,
                                             div(style = "background:#e8f4fd; padding:15px; border-radius:8px;",
                                                 h5(icon("layer-group"), " Type d'effet"),
                                                 selectInput(ns("plotDisplayType"), 
                                                             NULL,
                                                             choices = list(
                                                               "Effets principaux" = "main",
                                                               "Effets simples (interactions)" = "simple"
                                                             ),
                                                             selected = "main")
                                             )
                                      ),
                                      column(6,
                                             conditionalPanel(
                                               ns = ns,
                                               condition = "input.plotDisplayType == 'simple'",
                                               div(style = "background:#fff5f5; padding:15px; border-radius:8px;",
                                                   h5(icon("filter"), " Sélection effet simple"),
                                                   uiOutput(ns("selectSimpleEffectPlot"))
                                               )
                                             )
                                      )
                                    ),
                                  
                                    hr(),
                                  
                                    div(style = "background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                        h4(uiOutput(ns("plotTitle")), style = "text-align: center; color: #2c3e50;"),
                                        plotlyOutput(ns("multiPlot"), height = "600px")
                                    ),
                                  
                                    br(),
                                  
                                    div(style = "border: 1px solid #dee2e6; border-radius: 8px; overflow: hidden;",
                                        div(
                                          class = "panel-heading",
                                          style = "background-color: #343a40; color: white; padding: 12px 18px; cursor: pointer; display: flex; align-items: center;",
                                          `data-toggle` = "collapse",
                                          `data-target` = "#graphOptionsPanel",
                                          icon("sliders-h", style = "margin-right: 8px;"),
                                          tags$strong("Options du graphique"),
                                          tags$span(style = "margin-left: auto; font-size: 12px; opacity: 0.75;",
                                                    icon("chevron-down"), " Développer / Réduire")
                                        ),
                                        div(id = "graphOptionsPanel", class = "collapse",
                                            div(style = "padding: 20px; background-color: #fdfdfd;",
                                              
                                                fluidRow(
                                                  # COL 1 : Type + Couleurs
                                                  column(4,
                                                         div(style = "padding-right: 15px; border-right: 1px solid #e9ecef;",
                                                             h6(icon("palette"), " Type et couleurs",
                                                                style = "font-weight: bold; color: #343a40; border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 12px;"),
                                                             selectInput(ns("boxColor"), "Palette",
                                                                         choices = c("Défaut" = "default", "Bleu" = "Blues",
                                                                                     "Vert" = "Greens", "Rouge" = "Reds",
                                                                                     "Set1" = "Set1", "Pastel" = "Pastel1",
                                                                                     "Paired" = "Paired"),
                                                                         selected = "Set1"),
                                                             radioButtons(ns("plotType"), "Type de graphique",
                                                                          choices = c("Boxplot" = "box", "Violon" = "violin",
                                                                                      "Points + barres" = "point", "Barres" = "hist"),
                                                                          selected = "box", inline = TRUE),
                                                             radioButtons(ns("errorType"), "Barres d'erreur",
                                                                          choices = c("SE" = "se", "SD" = "sd",
                                                                                      "IC 95%" = "ci", "Aucune" = "none"),
                                                                          selected = "se", inline = TRUE),
                                                             checkboxInput(ns("colorByGroups"),
                                                                           HTML("Colorer par groupes statistiques <small style='color:#6c757d;'>(a, b, c...)</small>"),
                                                                           value = FALSE)
                                                         )
                                                  ),
                                                
                                                  # COL 2 : Titres + Tailles
                                                  column(4,
                                                         div(style = "padding-left: 15px; padding-right: 15px; border-right: 1px solid #e9ecef;",
                                                             h6(icon("heading"), " Titres et tailles",
                                                                style = "font-weight: bold; color: #343a40; border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 12px;"),
                                                             textInput(ns("customTitle"), "Titre", placeholder = "Auto"),
                                                             textInput(ns("customSubtitle"), "Sous-titre", placeholder = "Optionnel"),
                                                             fluidRow(
                                                               column(6, textInput(ns("customXLabel"), "Label X", placeholder = "Auto")),
                                                               column(6, textInput(ns("customYLabel"), "Label Y", placeholder = "Auto"))
                                                             ),
                                                             textInput(ns("customLegendTitle"), "Titre légende", placeholder = "Auto"),
                                                             fluidRow(
                                                               column(6, sliderInput(ns("titleSize"), "Titre", min = 8, max = 32, value = 16, step = 1, ticks = FALSE)),
                                                               column(6, sliderInput(ns("axisTitleSize"), "Axes titres", min = 8, max = 28, value = 14, step = 1, ticks = FALSE))
                                                             ),
                                                             fluidRow(
                                                               column(6, sliderInput(ns("axisTextSize"), "Texte axes", min = 6, max = 24, value = 12, step = 1, ticks = FALSE)),
                                                               column(6, sliderInput(ns("graphValueSize"), "Lettres (a,b,c)", min = 2, max = 20, value = 5, step = 0.5, ticks = FALSE))
                                                             ),
                                                             sliderInput(ns("meanValueSize"), "Taille moyennes dans barres",
                                                                         min = 2, max = 12, value = 4, step = 0.5, ticks = FALSE),
                                                             fluidRow(
                                                               column(6,
                                                                      selectInput(ns("titleFontStyle"), "Style titre",
                                                                                  choices = c("Normal" = "plain", "Gras" = "bold",
                                                                                              "Italique" = "italic", "Gras+Italique" = "bold.italic"),
                                                                                  selected = "bold")
                                                               ),
                                                               column(6,
                                                                      selectInput(ns("axisTitleFontStyle"), "Style titres axes",
                                                                                  choices = c("Normal" = "plain", "Gras" = "bold",
                                                                                              "Italique" = "italic", "Gras+Italique" = "bold.italic"),
                                                                                  selected = "plain")
                                                               )
                                                             ),
                                                             fluidRow(
                                                               column(6,
                                                                      selectInput(ns("graphValueFontStyle"), "Style lettres (a,b,c)",
                                                                                  choices = c("Normal" = "plain", "Gras" = "bold",
                                                                                              "Italique" = "italic", "Gras+Italique" = "bold.italic"),
                                                                                  selected = "bold")
                                                               ),
                                                               column(6,
                                                                      checkboxInput(ns("rotateXLabels"), "Labels X à 45°", value = TRUE)
                                                               )
                                                             )
                                                         )
                                                  ),
                                                
                                                  # COL 3 : Axes + Ordre
                                                  column(4,
                                                         div(style = "padding-left: 15px;",
                                                             h6(icon("ruler-combined"), " Axes et ordre",
                                                                style = "font-weight: bold; color: #343a40; border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 12px;"),
                                                             checkboxInput(ns("customAxisLimits"), "Personnaliser les limites des axes", value = FALSE),
                                                             conditionalPanel(
                                                               ns = ns,
                                                               condition = "input.customAxisLimits == true",
                                                               fluidRow(
                                                                 column(6, numericInput(ns("yAxisMin"), "Y min", value = NULL, step = 0.1)),
                                                                 column(6, numericInput(ns("yAxisMax"), "Y max", value = NULL, step = 0.1))
                                                               )
                                                             ),
                                                             checkboxInput(ns("customAxisBreaks"), "Personnaliser les graduations", value = FALSE),
                                                             conditionalPanel(
                                                               ns = ns,
                                                               condition = "input.customAxisBreaks == true",
                                                               fluidRow(
                                                                 column(6, numericInput(ns("yAxisBreakStep"), "Pas Y", value = NULL, step = 0.1, min = 0.01)),
                                                                 column(6, numericInput(ns("xAxisBreakStep"), "Pas X", value = NULL, step = 0.1, min = 0.01))
                                                               )
                                                             ),
                                                             hr(style = "margin: 10px 0;"),
                                                             checkboxInput(ns("customXOrder"), "Personnaliser l'ordre axe X", value = FALSE),
                                                             conditionalPanel(
                                                               ns = ns,
                                                               condition = "input.customXOrder == true",
                                                               uiOutput(ns("xAxisOrderUI"))
                                                             ),
                                                             hr(style = "margin: 10px 0;"),
                                                             fluidRow(
                                                               column(6, sliderInput(ns("legendTitleSize"), "Titre légende", min = 6, max = 24, value = 12, step = 1, ticks = FALSE)),
                                                               column(6, sliderInput(ns("legendTextSize"), "Texte légende", min = 6, max = 20, value = 10, step = 1, ticks = FALSE))
                                                             ),
                                                             sliderInput(ns("legendSpacing"), "Espacement légende",
                                                                         min = 0, max = 6, value = 0, step = 0.1, ticks = FALSE),
                                                             tags$div(style = "display:none;",
                                                                      numericInput(ns("plotWidth"),  "Largeur", value = 8,   min = 3, max = 20),
                                                                      numericInput(ns("plotHeight"), "Hauteur", value = 6,   min = 3, max = 20),
                                                                      numericInput(ns("plotDPI"),    "DPI",     value = 300, min = 72, max = 600),
                                                                      numericInput(ns("xAxisMin"), "X min", value = NULL, step = 0.1),
                                                                      numericInput(ns("xAxisMax"), "X max", value = NULL, step = 0.1),
                                                                      sliderInput(ns("subtitleSize"), "Sous-titre", min = 6, max = 28, value = 12, step = 1),
                                                                      selectInput(ns("subtitleFontStyle"), "Style sous-titre",
                                                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                                                  selected = "italic"),
                                                                      selectInput(ns("axisTextXFontStyle"), "Style axe X",
                                                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                                                  selected = "plain"),
                                                                      selectInput(ns("axisTextYFontStyle"), "Style axe Y",
                                                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                                                  selected = "plain"),
                                                                      selectInput(ns("legendTitleFontStyle"), "Style titre légende",
                                                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                                                  selected = "bold"),
                                                                      selectInput(ns("legendTextFontStyle"), "Style texte légende",
                                                                                  choices = c("Normal"="plain","Gras"="bold","Italique"="italic","Gras+Italique"="bold.italic"),
                                                                                  selected = "plain"),
                                                                      selectInput(ns("subtitlePosition"), "Position sous-titre",
                                                                                  choices = list("Centré"="0.5","Gauche"="0","Droite"="1"), selected="0.5"),
                                                                      numericInput(ns("legendKeySize"), "Icône légende", value=0.5, min=0.1, max=3, step=0.1)
                                                             )
                                                         )
                                                  )
                                                )
                                            )
                                        )
                                    ),
                                  
                                    br(),
                                  
                                    div(style = "max-width: 400px; margin: 0 auto;",
                                        fluidRow(
                                          column(7,
                                            selectInput(ns("multiPlotFormat"),
                                              tagList(icon("file-image"), " Format d'export"),
                                              choices = c("PNG" = "png", "PDF" = "pdf",
                                                          "JPEG" = "jpeg", "TIFF" = "tiff", "SVG" = "svg"),
                                              selected = "png", width = "100%")),
                                          column(5,
                                            numericInput(ns("plotDPIVisible"), tagList(icon("image"), " DPI"),
                                                         value = 300, min = 72, max = 600, step = 50, width = "100%"))
                                        ),
                                        downloadButton(ns("downloadMultiPlot"),
                                                       tagList(icon("download"), " Télécharger le graphique"),
                                                       class = "btn-success",
                                                       style = "width: 100%; height: 50px; font-weight: bold;")
                                    )
                                  ),
                                
                                  # ONGLET 4 : Rapport complet 
                                
                                  tabPanel(
                                    title = div(icon("file-alt"), " Rapport"),
                                    value = "report",
                                    br(),
                                  
                                    div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                                        h3(style = "color: #2c3e50;", icon("clipboard-check"), " Résumé de l'analyse"),
                                        hr(),
                                        uiOutput(ns("fullAnalysisReport"))
                                    ),
                                  
                                    div(style = "background: linear-gradient(135deg, #27ae60 0%, #229954 100%); padding: 20px; border-radius: 8px;",
                                        h4(style = "color: white; margin-top: 0;", 
                                           icon("download"), " Téléchargements"),
                                        fluidRow(
                                          column(4,
                                                 downloadButton(ns("downloadAllResults"), 
                                                                div(icon("file-excel", style = "font-size: 2em; display: block; margin-bottom: 10px;"), 
                                                                    "Toutes les données"),
                                                                class = "btn-light btn-lg",
                                                                style = "width: 100%; height: 120px; font-weight: bold;")
                                          ),
                                          column(4,
                                                 downloadButton(ns("downloadSummaryStats"), 
                                                                div(icon("chart-pie", style = "font-size: 2em; display: block; margin-bottom: 10px;"), 
                                                                    "Statistiques résumées"),
                                                                class = "btn-light btn-lg",
                                                                style = "width: 100%; height: 120px; font-weight: bold;")
                                          ),
                                          column(4,
                                                 downloadButton(ns("downloadFullReport"), 
                                                                div(icon("file-pdf", style = "font-size: 2em; display: block; margin-bottom: 10px;"), 
                                                                    "Rapport PDF"),
                                                                class = "btn-light btn-lg",
                                                                style = "width: 100%; height: 120px; font-weight: bold;")
                                          )
                                        )
                                    )
                                  )
                      )
                  )
                ),
              
                fluidRow(
                  box(
                    title = tagList(icon("calculator"),
                                    " PostHoc Régression linéaire / GLM -- Comparaisons et lettres CLD"),
                    status = "primary", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                  
                    conditionalPanel(
                      ns = ns,
                      condition = "output.hasLMPostHoc",
                      fluidRow(
                        column(4,
                               div(style = "background:#f8f9fa; padding:14px; border-radius:8px;",
                                   h5(tagList(icon("filter"), " Sélection"),
                                      style = "font-weight:bold; margin-top:0; color:#1565C0;"),
                                   uiOutput(ns("lmPostHocSelector")),
                                   br(),
                                   uiOutput(ns("lmPostHocInfo")),
                                   br(),
                                   downloadButton(ns("downloadLMPostHoc"),
                                                  tagList(icon("file-excel"), " Télécharger Excel"),
                                                  class = "btn-success btn-block",
                                                  style = "height:50px; font-weight:bold;")
                               )
                        ),
                        column(8,
                               tabsetPanel(type = "tabs",
                                           tabPanel(
                                             title = tagList(icon("layer-group"), " Lettres CLD"),
                                             br(),
                                             div(style = "background:#e3f2fd; border-left:3px solid #1565C0; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:11px;",
                                                 icon("info-circle"),
                                                 " Niveaux partageant une même lettre = pas de différence significative (alpha = 0.05)."
                                             ),
                                             withSpinner(DTOutput(ns("lmPostHocLettersTable")), color = "#1565C0")
                                           ),
                                           tabPanel(
                                             title = tagList(icon("code-branch"), " Paires (emmeans)"),
                                             br(),
                                             div(style = "background:#fff3e0; border-left:3px solid #fb8c00; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:11px;",
                                                 icon("info-circle"),
                                                 " Comparaisons par paires sur les moyennes marginales estimées (emmeans). ",
                                                 "Pour les GLM non-gaussiens, les valeurs sont sur l'échelle du lien."
                                             ),
                                             withSpinner(DTOutput(ns("lmPostHocPairsTable")), color = "#fb8c00")
                                           )
                               )
                        )
                      )
                    ),
                    conditionalPanel(
                      ns = ns,
                      condition = "!output.hasLMPostHoc",
                      div(style = "text-align:center; padding:40px; color:#95a5a6;",
                          icon("calculator", style = "font-size:4em; opacity:0.3;"),
                          h4("Aucun PostHoc LM/GLM calculé"),
                          p("Pour activer cette section :"),
                          tags$ul(style = "text-align:left; display:inline-block; color:#555;",
                                  tags$li("Lancez d'abord une ", strong("Régression linéaire"),
                                          " ou un ", strong("GLM"),
                                          " dans l'onglet 'Tests statistiques'"),
                                  tags$li("Le modèle doit contenir au moins ", strong("un prédicteur catégoriel (factor)")),
                                  tags$li("Revenez ici, sélectionnez ", strong("'LM / GLM (emmeans + lettres CLD)'"),
                                          " dans la liste 'Méthode post-hoc paramétrique'"),
                                  tags$li("Cliquez sur ", strong("LANCER L'ANALYSE"))
                          )
                      )
                    )
                  )
                ),
              
                # MANOVA / PERMANOVA POSTHOC -- Comparaisons multivariees par paires + lettres
              
                fluidRow(
                  div(id = "boxWrap_manovaPosthoc",
                      box(
                        title = tagList(icon("layer-group"),
                                        " PostHoc MANOVA/PERMANOVA -- Comparaisons multivariées par paires et lettres de groupes"),
                        status = "success", width = 12, solidHeader = TRUE,
                        collapsible = TRUE, collapsed = TRUE,
                      
                        conditionalPanel(
                          ns = ns,
                          condition = "output.hasMultivariatePosthoc",
                        
                          fluidRow(
                            column(4,
                                   div(style = "background:#f8f9fa; padding:14px; border-radius:8px;",
                                       h5(tagList(icon("filter"), " Sélection du facteur"),
                                          style = "font-weight:bold; margin-top:0; color:#2e7d32;"),
                                       uiOutput(ns("multivariatePosthocFactorSelect")),
                                       br(),
                                       uiOutput(ns("multivariatePosthocInfo")),
                                       br(),
                                       downloadButton(ns("downloadMultivariatePosthoc"),
                                                      tagList(icon("file-excel"), " Télécharger Excel (lettres + paires)"),
                                                      class = "btn-success btn-block",
                                                      style = "height: 50px; font-weight: bold;")
                                   )
                            ),
                            column(8,
                                   tabsetPanel(type = "tabs",
                                               tabPanel(
                                                 title = tagList(icon("layer-group"), " Groupes distincts (lettres)"),
                                                 br(),
                                                 div(style = "background:#e3f2fd; border-left:3px solid #1565C0; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:11px;",
                                                     icon("info-circle"),
                                                     " Les niveaux partageant une même lettre ne diffèrent pas significativement sur l'ensemble des réponses (test multivarié, alpha = 0.05)."
                                                 ),
                                                 withSpinner(DTOutput(ns("multivariatePosthocLettersTable")), color = "#2e7d32")
                                               ),
                                               tabPanel(
                                                 title = tagList(icon("code-branch"), " Paires (PERMANOVA Bonferroni)"),
                                                 br(),
                                                 div(style = "background:#fff3e0; border-left:3px solid #fb8c00; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:11px;",
                                                     icon("info-circle"),
                                                     " Comparaisons par paires sur l'ensemble des variables réponses. ",
                                                     "Pour chaque paire : F (pseudo), R², p-value brute, p-value ajustée (Bonferroni)."
                                                 ),
                                                 withSpinner(DTOutput(ns("multivariatePosthocPairsTable")), color = "#f39c12")
                                               ),
                                               tabPanel(
                                                 title = tagList(icon("project-diagram"), " Interaction (cellules croisées)"),
                                                 br(),
                                                 conditionalPanel(
                                                   ns = ns,
                                                   condition = "output.hasManovaInteractionPostHoc",
                                                   uiOutput(ns("manovaInteractionPostHocInfo")),
                                                   h5(icon("layer-group"), " Lettres par cellule d'interaction",
                                                      style = "color:#2e7d32; margin-top:0;"),
                                                   withSpinner(DTOutput(ns("manovaInteractionLettersTable")), color = "#2e7d32"),
                                                   br(),
                                                   h5(icon("code-branch"), " Comparaisons par paires des cellules",
                                                      style = "color:#f39c12;"),
                                                   withSpinner(DTOutput(ns("manovaInteractionPairsTable")), color = "#f39c12")
                                                 ),
                                                 conditionalPanel(
                                                   ns = ns,
                                                   condition = "!output.hasManovaInteractionPostHoc",
                                                   div(style = "text-align:center; padding:30px; color:#95a5a6;",
                                                       icon("project-diagram", style = "font-size:3em; opacity:0.3;"),
                                                       h5("Aucun PostHoc d'interaction"),
                                                       p(style = "font-size:12px;",
                                                         "Cochez ", strong("'Activer l'analyse des interactions'"),
                                                         " et sélectionnez au moins 2 facteurs avant de lancer l'analyse."))
                                                 )
                                               )
                                   )
                            )
                          )
                        ),
                      
                        conditionalPanel(
                          ns = ns,
                          condition = "!output.hasMultivariatePosthoc",
                          div(style = "text-align: center; padding: 40px; color: #95a5a6;",
                              icon("layer-group", style = "font-size: 4em; opacity: 0.3;"),
                              h4("Aucun PostHoc multivarié calculé"),
                              p("Pour activer cette section :"),
                              tags$ul(style = "text-align: left; display: inline-block; color: #555;",
                                      tags$li("Sélectionnez ", strong(">= 2 variables réponses"),
                                              " dans le panneau de configuration"),
                                      tags$li("Sélectionnez au moins ", strong("1 facteur")),
                                      tags$li("Cliquez sur ", strong("LANCER L'ANALYSE")),
                                      tags$li("Les comparaisons multivariées par paires et les lettres de groupes s'afficheront ici")
                              )
                          )
                        )
                      )
                  )
                ),
              
              
                fluidRow(
                  box(
                    title = tagList(icon("repeat"), " PostHoc Mesures répétées -- Comparaisons par paires (Période / Traitement)"),
                    status = "success", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    conditionalPanel(
                      ns = ns,
                      condition = "output.hasRMPostHoc",
                      div(style = "background:#e0f2f1; border-left:4px solid #00897b; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:12px;",
                          icon("circle-info"),
                          HTML(" Comparaisons par paires issues de la dernière <b>ANOVA à mesures répétées</b> ou du dernier <b>test non paramétrique répété</b>. Une ligne par paire de niveaux, avec p-value ajustée.")),
                      uiOutput(ns("rmPostHocInfo")),
                      withSpinner(DTOutput(ns("rmPostHocTable")), color = "#00897b"),
                      br(),
                      downloadButton(ns("downloadRMPostHoc"), "Télécharger (Excel)", class = "btn-success")
                    ),
                    conditionalPanel(
                      ns = ns,
                      condition = "!output.hasRMPostHoc",
                      div(style = "text-align:center; padding:40px; color:#95a5a6;",
                          icon("repeat", style = "font-size:4em; opacity:0.3;"),
                          h4("Aucun post-hoc de mesures répétées calculé"),
                          p("Pour activer cette section :"),
                          tags$ul(style = "text-align:left; display:inline-block; color:#555;",
                                  tags$li("Renseignez Sujet, Période (et Traitement) dans le panneau « Mesures répétées »"),
                                  tags$li(HTML("Lancez <b>ANOVA à mesures répétées</b> ou <b>Non paramétrique répété</b> dans l'onglet « Tests statistiques »")),
                                  tags$li("Les comparaisons par paires s'afficheront ici")))
                    )
                  )
                )
              
              
      )
}


# ============================ TESTS DE CORRÉLATION ============================
# Page dédiée : corrélations de Pearson, Kendall et Spearman sur toutes les
# paires de variables numériques, avec correction pour comparaisons multiples.
# Tous les résultats sont présentés dans un data.frame (DT).
mod_correlation_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "corrélation",
    .hstat_scope_banner(exact = FALSE),
    fluidRow(
      box(title = tagList(icon("cog"), " Configuration"),
          status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,
          uiOutput(ns("corTestVarSelect")),
          checkboxInput(ns("corTestTargetMode"),
            tagList(icon("crosshairs"), " Corréler une seule variable avec les autres"),
            value = FALSE),
          conditionalPanel(sprintf("input['%s'] == true", ns("corTestTargetMode")),
            uiOutput(ns("corTestTargetSelect"))),
          selectInput(ns("corTestMethod"), "Méthode de corrélation",
            choices = c("Pearson (linéaire)"            = "pearson",
                        "Spearman (rang, monotone)"     = "spearman",
                        "Kendall (rang, concordance)"   = "kendall"),
            selected = "pearson"),
          selectInput(ns("corTestAlt"), "Hypothèse alternative",
            choices = c("Bilatérale (\u2260 0)"  = "two.sided",
                        "Unilatérale (> 0)"      = "greater",
                        "Unilatérale (< 0)"      = "less"),
            selected = "two.sided"),
          sliderInput(ns("corTestConf"), "Niveau de confiance",
            min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          selectInput(ns("corTestAdjust"), "Correction (corrélations multiples)",
            choices = c("Holm"                 = "holm",
                        "Bonferroni"           = "bonferroni",
                        "Benjamini-Hochberg (FDR)" = "BH",
                        "Benjamini-Yekutieli"  = "BY",
                        "Hochberg"             = "hochberg",
                        "Aucune"               = "none"),
            selected = "holm"),
          checkboxInput(ns("corTestAllMethods"),
            tagList(icon("layer-group"), " Comparer les 3 méthodes"), value = FALSE),
          div(class = "callout callout-info", style = "margin-top:10px;",
              icon("info-circle"),
              " Toutes les métriques (coefficient, statistique, ddl, p brute et ",
              "ajustée, IC, R², force, sens) figurent dans le tableau de résultats.")
      ),
      box(title = tagList(icon("table"), " Résultats des tests de corrélation"),
          status = "success", width = 8, solidHeader = TRUE,
          DT::DTOutput(ns("corTestTable")),
          br(),
          downloadButton(ns("corTestDownload"), "Télécharger (CSV)",
                         class = "btn-success btn-sm"),
          br(), br(),
          uiOutput(ns("corTestInterpretation"))
      )
    ),
    fluidRow(
      box(title = tagList(icon("project-diagram"), " Matrice de Corrélation"),
          status = "success", width = 12, solidHeader = TRUE, collapsible = TRUE,
          fluidRow(
            column(12, div(style = "background:#f8f9fa;padding:15px;border-radius:5px;margin-bottom:15px;",
                           uiOutput(ns("corrVarSelect")),
                           checkboxInput(ns("corrFocusMode"),
                             tagList(icon("crosshairs"), " Corréler une seule variable avec les autres"),
                             value = FALSE),
                           conditionalPanel(sprintf("input['%s'] == true", ns("corrFocusMode")),
                             uiOutput(ns("corrFocusSelect")))))
          ),
          fluidRow(
            column(3,
              h5(icon("sliders-h"), " Méthode", style = "color:#27ae60;font-weight:bold;"),
              selectInput(ns("corrMethod"), "Méthode de corrélation",
                choices = c("Pearson (linéaire)" = "pearson",
                            "Spearman (monotone)" = "spearman",
                            "Kendall (robuste)" = "kendall"),
                selected = "pearson")),
            column(3,
              h5(icon("palette"), " Affichage", style = "color:#27ae60;font-weight:bold;"),
              selectInput(ns("corrDisplay"), "Mode d'affichage",
                choices = c("Nombres" = "number", "Cercles" = "circle",
                            "Carrés" = "square", "Ellipses" = "ellipse",
                            "Couleurs" = "color", "Secteurs" = "pie"),
                selected = "circle"),
              selectInput(ns("corrType"), "Type d'affichage",
                choices = c("Complet" = "full", "Triangulaire supérieur" = "upper",
                            "Triangulaire inférieur" = "lower"),
                selected = "upper")),
            column(3,
              h5(icon("text-height"), " Tailles", style = "color:#27ae60;font-weight:bold;"),
              sliderInput(ns("corrTextSize"), "Taille des valeurs",
                          min = 0.3, max = 2, value = 0.8, step = 0.1, ticks = FALSE),
              sliderInput(ns("corrLabelSize"), "Taille des labels",
                          min = 0.3, max = 2, value = 0.9, step = 0.1, ticks = FALSE)),
            column(3,
              h5(icon("heading"), " Titre & probabilités", style = "color:#27ae60;font-weight:bold;"),
              textInput(ns("corrTitle"), "Titre personnalisé",
                        placeholder = "Vide = titre auto"),
              numericInput(ns("corrDPI"), tagList(icon("image"), " DPI export"),
                           value = 300, min = 72, max = 1200, step = 50),
              selectInput(ns("corrFormat"), tagList(icon("file-image"), " Format d'export"),
                          choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff",
                                      "BMP" = "bmp", "PDF" = "pdf", "SVG" = "svg"),
                          selected = "png"),
              numericInput(ns("corrSizeIn"), tagList(icon("ruler-combined"), " Taille (pouces, carré)"),
                           value = 8, min = 3, max = 30, step = 1))
          ),
          fluidRow(
            column(4,
              selectInput(ns("corrPval"), "Probabilités (p-values) sur le graphique",
                choices = c("Coefficient + p-value (même cellule)" = "both",
                            "Afficher les p-values" = "show",
                            "Marquer le non-significatif d'une croix" = "mark",
                            "Masquer le non-significatif" = "blank",
                            "Ne rien afficher" = "none"),
                selected = "both")),
            column(3,
              sliderInput(ns("corrSigLevel"), "Seuil de significativité (α)",
                          min = 0.01, max = 0.10, value = 0.05, step = 0.01)),
            column(2,
              sliderInput(ns("corrPvalSize"), "Taille des p-values",
                          min = 0.4, max = 2, value = 0.8, step = 0.1, ticks = FALSE)),
            column(3,
              div(style = "margin-top:25px;",
                checkboxInput(ns("corrReorder"),
                  tagList(icon("sort"), " Réordonner (regroupement hiérarchique)"),
                  value = TRUE)))
          ),
          fluidRow(
            column(3,
              selectInput(ns("corrPalette"), "Palette de couleurs",
                choices = c("Par défaut (corrplot)" = "default",
                            "Rouge-Bleu (RdBu)" = "RdBu",
                            "Rouge-Jaune-Bleu (RdYlBu)" = "RdYlBu",
                            "Violet-Orange (PuOr)" = "PuOr",
                            "Spectral" = "spectral",
                            "Viridis" = "viridis"),
                selected = "default")),
            column(3,
              div(style = "margin-top:25px;",
                checkboxInput(ns("corrWhiteOnDark"),
                  tagList(icon("adjust"), " Texte blanc sur cellules sombres"),
                  value = TRUE))),
            column(2,
              div(style = "margin-top:8px;",
                colourInput(ns("corrCoefColor"), "Couleur coefficients",
                                          value = "#000000"))),
            column(2,
              div(style = "margin-top:8px;",
                colourInput(ns("corrPvalColorSig"), "Couleur p-value (signif.)",
                                          value = "#1a7a1a"))),
            column(2,
              div(style = "margin-top:8px;",
                colourInput(ns("corrPvalColorNs"), "Couleur p-value (non signif.)",
                                          value = "#999999")))
          ),
          hr(),
          div(style = "text-align:center;margin:15px 0;",
            downloadButton(ns("downloadCorrPlot"),
              tagList(icon("download"), " Télécharger l'image"), class = "btn-info")),
          div(style = "background:white;padding:20px;border-radius:5px;box-shadow:0 2px 4px rgba(0,0,0,0.1);",
            plotOutput(ns("corrPlot"), height = "620px"))
      )
    )
  )
}

# Serveur associé (appelé depuis le serveur principal sur le même id "corrélation")
mod_correlation_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$corTestVarSelect <- renderUI({
      req(values$data)
      num_cols <- names(values$data)[vapply(values$data, is.numeric, logical(1))]
      if (length(num_cols) < 2)
        return(div(class = "alert alert-warning", icon("exclamation-triangle"),
                   " Au moins deux variables numériques sont nécessaires."))
      tagList(
        if (requireNamespace("shinyWidgets", quietly = TRUE))
          shinyWidgets::pickerInput(ns("corTestVars"),
            "Variables numériques (\u2265 2)", choices = num_cols,
            selected = num_cols[seq_len(min(5, length(num_cols)))],
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE))
        else
          selectInput(ns("corTestVars"), "Variables numériques (\u2265 2)",
            choices = num_cols,
            selected = num_cols[seq_len(min(5, length(num_cols)))],
            multiple = TRUE)
      )
    })

    output$corTestTargetSelect <- renderUI({
      req(values$data, input$corTestVars)
      sel <- input$corTestVars
      if (length(sel) < 2) return(NULL)
      selectInput(ns("corTestTarget"),
        tagList(icon("bullseye"), " Variable cible (corrélée à toutes les autres)"),
        choices = sel, selected = sel[1])
    })

    cor_results <- reactive({
      req(values$data, input$corTestVars)
      validate(need(length(input$corTestVars) >= 2,
                    "Sélectionnez au moins deux variables."))
      tgt <- if (isTRUE(input$corTestTargetMode)) input$corTestTarget else NULL
      methods <- if (isTRUE(input$corTestAllMethods))
        c("pearson", "spearman", "kendall") else input$corTestMethod
      out <- do.call(rbind, lapply(methods, function(m)
        hstat_correlation_tests(
          values$data, input$corTestVars, method = m,
          alternative = input$corTestAlt %||% "two.sided",
          conf.level = input$corTestConf %||% 0.95,
          p.adjust.method = input$corTestAdjust %||% "holm",
          target = tgt)))
      out
    })

    # Depot des tests de correlation pour l'aide a la decision.
    observeEvent(cor_results(), {
      res <- tryCatch(cor_results(), error = function(e) NULL)
      if (is.null(res) || !NROW(res)) return()
      hstat_ai_capture(values, "Correlations",
        sprintf("Tests de correlation (%s)",
                paste(unique(if ("Methode" %in% names(res)) res$Methode
                             else input$corTestMethod %||% "pearson"), collapse = ", ")),
        tables = list("Correlations par paire" = res),
        meta = list(variables = input$corTestVars,
                    `correction des p` = input$corTestAdjust,
                    `niveau de confiance` = input$corTestConf))
    }, ignoreInit = TRUE)

    output$corTestTable <- DT::renderDT({
      res <- cor_results()
      req(!is.null(res))
      fname <- paste0("corrélations_", Sys.Date())
      DT::datatable(res, rownames = FALSE, filter = "top",
        extensions = "Buttons",
        options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
                       buttons = .hstat_dt_buttons(fname)),
        caption = htmltools::tags$caption(
          style = "caption-side: top; font-weight: 600;",
          "Tests de corrélation par paire — toutes les métriques")) |>
        DT::formatStyle("Significatif",
          backgroundColor = DT::styleEqual(
            c("*** (p<.001)", "** (p<.01)", "* (p<.05)", "ns"),
            c("#c8e6c9", "#dcedc8", "#f0f4c3", "#ffffff")))
    })

    output$corTestDownload <- downloadHandler(
      filename = function() paste0("corrélations_", Sys.Date(), ".csv"),
      content  = function(file) {
        res <- cor_results()
        utils::write.csv(res, file, row.names = FALSE, fileEncoding = "UTF-8")
      })

    output$corTestInterpretation <- renderUI({
      res <- cor_results()
      req(!is.null(res))
      sig <- res[!is.na(res$p_ajuste) & res$p_ajuste < 0.05, , drop = FALSE]
      if (nrow(sig) == 0)
        return(div(class = "alert alert-info", icon("info-circle"),
                   " Aucune corrélation significative après correction."))
      items <- lapply(seq_len(nrow(sig)), function(i) {
        r <- sig[i, ]
        tags$li(HTML(sprintf(
          "<b>%s \u2013 %s</b> (%s) : r = %.3f, %s, %s (p<sub>ajust</sub> = %s)",
          r$Variable_X, r$Variable_Y, r$Methode, r$Coefficient,
          tolower(r$Force), tolower(r$Sens), format(r$p_ajuste))))
      })
      div(class = "alert alert-success",
          strong(icon("check-circle"), " Corrélations significatives :"),
          tags$ul(items))
    })

    # ---------- Matrice de Corrélation (deplacee depuis Exploration) ----------
    output$corrVarSelect <- renderUI({
      req(values$data)
      num_cols <- names(values$data)[vapply(values$data, is.numeric, logical(1))]
      if (length(num_cols) < 2)
        return(div(class = "alert alert-warning", icon("exclamation-triangle"),
                   " Au moins deux variables numériques sont nécessaires."))
      tagList(
        if (requireNamespace("shinyWidgets", quietly = TRUE))
          shinyWidgets::pickerInput(ns("corrVars"), "Variables numériques",
            choices = num_cols, multiple = TRUE,
            selected = num_cols[seq_len(min(6, length(num_cols)))],
            options = list(`actions-box` = TRUE, `live-search` = TRUE))
        else
          selectInput(ns("corrVars"), "Variables numériques", choices = num_cols,
            multiple = TRUE, selected = num_cols[seq_len(min(6, length(num_cols)))])
      )
    })

    output$corrFocusSelect <- renderUI({
      req(values$data, input$corrVars)
      sel <- input$corrVars
      if (length(sel) < 2) return(NULL)
      selectInput(ns("corrFocusVar"),
        tagList(icon("bullseye"), " Variable cible (corrélée à toutes les autres)"),
        choices = sel, selected = sel[1])
    })

    # Genere la matrice de correlation avec p-values sur le graphique
    generate_corr_matrix_plot <- function(data, vars, method, display, type,
                                           label_size, text_size, title,
                                           pval_mode, sig_level, pval_size, reorder,
                                           palette = "default", coef_color = "#000000",
                                           pval_color_sig = "#1a7a1a", pval_color_ns = "#999999",
                                           white_on_dark = TRUE, focus_var = NULL) {
      if (is.null(vars) || length(vars) < 2) return(invisible())
      cor_data <- data[, vars, drop = FALSE]
      # Securite : ne conserver que les colonnes NUMERIQUES (cor() echoue sinon avec
      # "argument non numérique pour un operateur binaire").
      num_ok <- vapply(cor_data, is.numeric, logical(1))
      if (!all(num_ok)) {
        dropped <- names(cor_data)[!num_ok]
        cor_data <- cor_data[, num_ok, drop = FALSE]
        showNotification(trf("Corrélation : variable(s) non numérique(s) ignorée(s) : %s.",
                                 paste(dropped, collapse = ", ")), type = "message", duration = 5)
      }
      cor_data <- remove_zero_var_cols(cor_data)
      if (ncol(cor_data) < 2) {
        showNotification("Moins de 2 variables numériques à variance non nulle.", type = "warning")
        return(invisible())
      }
      # Retire les colonnes parfaitement colineaires (|r| ~ 1) qui rendent la matrice
      # singuliere et font echouer le reordonnancement hierarchique (solve()).
      if (identical(method, "kendall"))
        cor_data <- hstat_cap_df_rows(cor_data, HSTAT_KENDALL_MAX_N, "Correlation de Kendall")
      cm0 <- suppressWarnings(stats::cor(cor_data, use = "pairwise.complete.obs", method = method))
      if (!is.null(cm0)) {
        cm0[is.na(cm0)] <- 0
        drop_idx <- integer(0)
        for (i in 2:ncol(cm0)) {
          if (any(abs(cm0[i, seq_len(i - 1)]) > 0.9999)) drop_idx <- c(drop_idx, i)
        }
        if (length(drop_idx) > 0) {
          showNotification(trf("Corrélation : %d variable(s) parfaitement colinéaire(s) retirée(s).",
                                   length(drop_idx)), type = "message", duration = 5)
          cor_data <- cor_data[, -drop_idx, drop = FALSE]
        }
      }
      if (ncol(cor_data) < 2) {
        showNotification("Moins de 2 variables non colinéaires.", type = "warning")
        return(invisible())
      }
      if (identical(method, "kendall"))
        cor_data <- hstat_cap_df_rows(cor_data, HSTAT_KENDALL_MAX_N, "Correlation de Kendall")
      cor_matrix <- suppressWarnings(stats::cor(cor_data, use = "complete.obs", method = method))
      p_matrix <- tryCatch(
        corrplot::cor.mtest(cor_data, conf.level = 1 - sig_level, method = method)$p,
        error = function(e) NULL)

      # Mode "une variable contre les autres" : on reduit la matrice a la ligne de
      # la variable cible (cible x toutes les autres). corrplot affiche alors une
      # bande 1 x (p-1). Le reordonnancement hierarchique et le triangulaire n'ont
      # plus de sens dans ce mode : on force order = original et type = full.
      focus_mode <- !is.null(focus_var) && nzchar(focus_var) && focus_var %in% colnames(cor_matrix)
      if (focus_mode) {
        others <- setdiff(colnames(cor_matrix), focus_var)
        cor_matrix <- cor_matrix[focus_var, others, drop = FALSE]
        if (!is.null(p_matrix)) p_matrix <- p_matrix[focus_var, others, drop = FALSE]
        reorder <- FALSE; type <- "full"
      }
      method_label <- switch(method, pearson = "Pearson", spearman = "Spearman", kendall = "Kendall")
      plot_title <- if (!is.null(title) && nzchar(title)) title
                    else paste("Matrice de corrélation -", method_label)
      ord <- if (isTRUE(reorder)) "hclust" else "original"

      # Palette de couleurs de la matrice (corrplot col)
      col_pal <- switch(palette,
        "default"  = NULL,
        "RdBu"     = rev(grDevices::colorRampPalette(c("#67001F","#B2182B","#D6604D","#F4A582","#FDDBC7","#FFFFFF","#D1E5F0","#92C5DE","#4393C3","#2166AC","#053061"))(200)),
        "RdYlBu"   = rev(grDevices::colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(200)),
        "PuOr"     = grDevices::colorRampPalette(c("#2D004B","#542788","#8073AC","#B2ABD2","#D8DAEB","#F7F7F7","#FEE0B6","#FDB863","#E08214","#B35806","#7F3B08"))(200),
        "viridis"  = grDevices::hcl.colors(200, "Viridis"),
        "spectral" = grDevices::colorRampPalette(c("#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B","#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2"))(200),
        NULL)

      # ---- Rendu dédié au mode "une variable contre les autres" (matrice 1 x N) ----
      # La matrice n'est pas carrée : on utilise is.corr = FALSE (mais l'échelle reste
      # [-1, 1] via col.lim) et on superpose coefficient + p-value proprement.
      if (focus_mode) {
        cp_args <- list(corr = cor_matrix, method = if (display == "number") "color" else display,
                        is.corr = FALSE, col.lim = c(-1, 1),
                        tl.cex = label_size, tl.col = "black", tl.srt = 45,
                        number.cex = text_size, cl.pos = "r",
                        title = paste0(plot_title, " — ", focus_var, " vs autres"),
                        mar = c(1, 0, 2, 0))
        if (!is.null(col_pal)) cp_args$col <- col_pal
        do.call(corrplot::corrplot, cp_args)
        nc <- ncol(cor_matrix)
        filled <- (if (display == "number") "color" else display) %in% c("color","circle","square","ellipse","pie")
        ramp <- if (!is.null(col_pal)) grDevices::colorRamp(col_pal) else
          grDevices::colorRamp(c("#B2182B", "#FFFFFF", "#2166AC"))
        lum <- function(r) { rgbv <- ramp(max(0, min(1, (r + 1) / 2))) / 255
                             0.299 * rgbv[1] + 0.587 * rgbv[2] + 0.114 * rgbv[3] }
        for (j in seq_len(nc)) {
          r_val <- cor_matrix[1, j]
          coef_col <- coef_color
          if (isTRUE(white_on_dark) && filled && !is.na(r_val) && lum(r_val) < 0.55) coef_col <- "#FFFFFF"
          if (pval_mode %in% c("both", "show", "mark", "blank")) {
            graphics::text(j, 1.16, formatC(r_val, format = "f", digits = 2),
                           cex = text_size * 0.85, font = 2, col = coef_col, xpd = NA)
            if (!is.null(p_matrix)) {
              p_val <- p_matrix[1, j]
              p_txt <- if (is.na(p_val)) "" else if (p_val < 0.001) "p<.001" else paste0("p=", formatC(p_val, format = "f", digits = 3))
              p_col <- if (!is.na(p_val) && p_val < sig_level) pval_color_sig else pval_color_ns
              if (isTRUE(white_on_dark) && filled && !is.na(r_val) && lum(r_val) < 0.5) p_col <- "#FFFFFF"
              graphics::text(j, 0.78, p_txt, cex = pval_size * 0.85, font = 3, col = p_col, xpd = NA)
            }
          }
        }
        return(invisible())
      }

      args <- list(corr = cor_matrix, method = display, type = type, order = ord,
                   tl.cex = label_size, tl.col = "black", number.cex = text_size,
                   title = plot_title, mar = c(0, 0, 2, 0), tl.srt = 45)
      if (!is.null(col_pal)) args$col <- col_pal
      coef_overlay <- if (display %in% c("circle","square","ellipse","color","pie")) "black" else NULL
      # Mode "both" : coefficient ET p-value dans la MEME cellule.
      if (!is.null(p_matrix) && pval_mode == "both") {
        disp <- if (display == "number") "color" else display
        # Les cellules sont "remplies" (donc fond colore -> texte blanc possible) pour
        # tous ces modes, y compris "number" converti en "color".
        filled <- disp %in% c("color","circle","square","ellipse","pie")
        cp_args <- list(cor_matrix, method = disp, type = type,
                        order = "original", tl.cex = label_size, tl.col = "black",
                        tl.srt = 45, title = plot_title, mar = c(1, 0, 2, 0),
                        cl.pos = "r", diag = TRUE)
        if (!is.null(col_pal)) cp_args$col <- col_pal
        do.call(corrplot::corrplot, cp_args)
        n <- ncol(cor_matrix)
        ramp <- if (!is.null(col_pal)) grDevices::colorRamp(col_pal) else
          grDevices::colorRamp(c("#B2182B", "#FFFFFF", "#2166AC"))
        lum <- function(r) {
          rgbv <- ramp(max(0, min(1, (r + 1) / 2))) / 255
          0.299 * rgbv[1] + 0.587 * rgbv[2] + 0.114 * rgbv[3]
        }
        for (i in seq_len(n)) for (j in seq_len(n)) {
          show_cell <- switch(type, "upper" = j >= i, "lower" = j <= i, TRUE)
          if (!show_cell) next
          xj <- j; yi <- n - i + 1
          r_val <- cor_matrix[i, j]
          this_coef_col <- coef_color
          if (isTRUE(white_on_dark) && filled && !is.na(r_val) && lum(r_val) < 0.55)
            this_coef_col <- "#FFFFFF"
          graphics::text(xj, yi + 0.16, formatC(r_val, format = "f", digits = 2),
                         cex = text_size * 0.85, font = 2, col = this_coef_col, xpd = NA)
          if (i != j) {
            p_val <- p_matrix[i, j]
            p_txt <- if (is.na(p_val)) "" else
              if (p_val < 0.001) "p<.001" else paste0("p=", formatC(p_val, format = "f", digits = 3))
            base_p_col <- if (!is.na(p_val) && p_val < sig_level) pval_color_sig else pval_color_ns
            if (isTRUE(white_on_dark) && filled && !is.na(r_val) && lum(r_val) < 0.5)
              base_p_col <- "#FFFFFF"
            graphics::text(xj, yi - 0.22, p_txt,
                           cex = pval_size * 0.85, font = 3, col = base_p_col, xpd = NA)
          }
        }
        return(invisible())
      }
      if (!is.null(p_matrix) && pval_mode != "none") {
        args$p.mat <- p_matrix
        args$sig.level <- sig_level
        if (pval_mode == "show") {
          args$insig <- "p-value"; args$pch.cex <- pval_size
          coef_overlay <- NULL
        } else if (pval_mode == "mark") {
          args$insig <- "pch"; args$pch <- 4; args$pch.cex <- pval_size * 2; args$pch.col <- "grey40"
        } else if (pval_mode == "blank") {
          args$insig <- "blank"
        }
      }
      args$addCoef.col <- coef_overlay
      do.call(corrplot::corrplot, args)
    }

    corr_params <- reactive({
      list(method = input$corrMethod %||% "pearson",
           display = input$corrDisplay %||% "circle",
           type = input$corrType %||% "upper",
           label_size = input$corrLabelSize %||% 0.9,
           text_size = input$corrTextSize %||% 0.8,
           title = input$corrTitle,
           pval_mode = input$corrPval %||% "both",
           sig_level = input$corrSigLevel %||% 0.05,
           pval_size = input$corrPvalSize %||% 0.8,
           reorder = if (is.null(input$corrReorder)) TRUE else input$corrReorder,
           palette = input$corrPalette %||% "default",
           coef_color = input$corrCoefColor %||% "#000000",
           pval_color_sig = input$corrPvalColorSig %||% "#1a7a1a",
           pval_color_ns = input$corrPvalColorNs %||% "#999999",
           white_on_dark = if (is.null(input$corrWhiteOnDark)) TRUE else input$corrWhiteOnDark)
    })

    output$corrPlot <- renderPlot({
      req(values$data, input$corrVars)
      p <- corr_params()
      fv <- if (isTRUE(input$corrFocusMode)) input$corrFocusVar else NULL
      tryCatch(
        generate_corr_matrix_plot(values$data, input$corrVars, p$method, p$display,
          p$type, p$label_size, p$text_size, p$title, p$pval_mode, p$sig_level,
          p$pval_size, p$reorder, p$palette, p$coef_color, p$pval_color_sig,
          p$pval_color_ns, p$white_on_dark, focus_var = fv),
        error = function(e) showNotification(hstat_err_fr(e, "Erreur matrice"),
                                             type = "error", duration = 5))
    },
    # Apercu a l'ecran : on rend a HAUTE RESOLUTION (res = 192 = 2x densite) pour un
    # affichage net sur ecrans standards ET Retina/HiDPI. La dimension d'affichage
    # reste pilotee par le conteneur (height = 620px cote UI), mais le bitmap sous-jacent
    # est genere a 1440x1440 px, ce qui supprime le flou observe au scaling navigateur.
    # L'export, lui, applique toujours le DPI et la taille choisis par l'utilisateur.
    res = 192,
    width = 1440,
    height = 1440)

    output$downloadCorrPlot <- downloadHandler(
      filename = function() {
        fmt <- input$corrFormat %||% "png"
        paste0("matrice_corrélation_", Sys.Date(), ".", fmt)
      },
      content = function(file) {
        req(values$data, input$corrVars)
        p <- corr_params()
        fv <- if (isTRUE(input$corrFocusMode)) input$corrFocusVar else NULL
        fmt <- input$corrFormat %||% "png"
        dpi <- .hstat_num1(input$corrDPI, 300)
        size_in <- .hstat_num1(input$corrSizeIn, 8)
        # La taille en pixels s'adapte au DPI choisi : pixels = pouces x DPI.
        px <- round(size_in * dpi)
        draw <- function() generate_corr_matrix_plot(values$data, input$corrVars, p$method, p$display,
            p$type, p$label_size, p$text_size, p$title, p$pval_mode, p$sig_level,
            p$pval_size, p$reorder, p$palette, p$coef_color, p$pval_color_sig,
            p$pval_color_ns, p$white_on_dark, focus_var = fv)
        if (fmt == "png") {
          grDevices::png(file, width = px, height = px, res = dpi, type = "cairo")
        } else if (fmt == "jpeg") {
          grDevices::jpeg(file, width = px, height = px, res = dpi, quality = 95, type = "cairo")
        } else if (fmt == "tiff") {
          grDevices::tiff(file, width = px, height = px, res = dpi, type = "cairo", compression = "lzw")
        } else if (fmt == "bmp") {
          grDevices::bmp(file, width = px, height = px, res = dpi, type = "cairo")
        } else if (fmt == "pdf") {
          grDevices::pdf(file, width = size_in, height = size_in)
        } else if (fmt == "svg") {
          if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite(file, width = size_in, height = size_in)
          else grDevices::svg(file, width = size_in, height = size_in)
        } else {
          grDevices::png(file, width = px, height = px, res = dpi, type = "cairo")
        }
        tryCatch(draw(), finally = grDevices::dev.off())
        showNotification(trf("Graphique téléchargé (%s, %d DPI).", toupper(fmt), dpi),
                         type = "message", duration = 3)
      })
  })
}


mod_tests_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # Theme ggplot2 (utilise par les graphiques du Chi2). viz_get_theme est global.
  get_plot_theme <- function(base_size = 12) {
    viz_get_theme(input$plotTheme %||% "minimal", base_size = base_size)
  }

  output$responseVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    tagList(
      pickerInput(ns("responseVar"), "Variable(s) réponse:", 
                  choices = num_cols, 
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      actionButton(ns("selectAllResponse"), "Tout sélectionner", class = "btn-success btn-sm"),
      actionButton(ns("deselectAllResponse"), "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllResponse, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "responseVar", selected = num_cols)
  })
  
  observeEvent(input$deselectAllResponse, {
    updatePickerInput(session, "responseVar", selected = character(0))
  })
  
  
  # Bloc 1 : Sélecteur de variables à transformer
  output$transformVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    transform_suffixes <- c("_log$","_log1p$","_log10$","_sqrt$",
                            "_cuberoot$","_boxcox$","_yeojohnson$","_arcsin$","_logit$")
    pattern   <- paste(transform_suffixes, collapse = "|")
    orig_cols <- num_cols[!grepl(pattern, num_cols)]
    pickerInput(ns("transformVar"), "Variable(s) à transformer :",
      choices  = orig_cols, multiple = TRUE,
      options  = list(`actions-box` = TRUE, `live-search` = TRUE,
                      `selected-text-format` = "count > 2",
                      `none-selected-text` = "Sélectionner...")
    )
  })
  
  # Bloc 2 : Vérification de faisabilité avant application
  output$transformFeasibilityCheck <- renderUI({
    req(input$transformVar, input$transformMethod, values$filteredData)
    checks <- lapply(input$transformVar, function(var) {
      x     <- values$filteredData[[var]]
      check <- check_transformation_feasibility(x, input$transformMethod)
      icon_el <- if (check$ok) icon("check-circle", style = "color:#4caf50;")
      else           icon("times-circle", style = "color:#e53935;")
      bg_col  <- if (check$ok) "#e8f5e9" else "#ffebee"
      txt_col <- if (check$ok) "#1b5e20" else "#b71c1c"
      div(
        style = paste0("display:flex;align-items:center;padding:4px 8px;",
                       "background:", bg_col, ";border-radius:4px;margin-bottom:3px;"),
        icon_el,
        tags$span(style = paste0("font-size:12px;margin-left:6px;color:", txt_col, ";"),
                  tags$b(var), " — ", check$message)
      )
    })
    tagList(
      div(style = "font-size:11px;font-weight:bold;color:#555;margin-bottom:4px;",
          icon("clipboard-check"), " Vérification de faisabilité :"),
      tagList(checks)
    )
  })
  
  # Bloc 3 : Application de la transformation
  observeEvent(input$applyTransformation, {
    req(input$transformVar, input$transformMethod, values$filteredData)
    if (length(input$transformVar) == 0) {
      showNotification("Sélectionnez au moins une variable à transformer.", type = "warning")
      return()
    }
    df          <- values$filteredData
    log_entries <- values$transformationLog
    errors  <- c(); added <- c(); skipped <- c()
    
    for (var in input$transformVar) {
      new_var_name <- paste0(var, "_", input$transformMethod)
      if (new_var_name %in% names(df)) { skipped <- c(skipped, new_var_name); next }
      tryCatch({
        x           <- df[[var]]
        transformed <- apply_variable_transformation(x, input$transformMethod)
        df[[new_var_name]] <- as.numeric(transformed)
        log_entry <- list(
          original = var, method = input$transformMethod,
          label    = get_transformation_label(input$transformMethod),
          formula  = get_transformation_formula(input$transformMethod),
          applied_at = format(Sys.time(), "%H:%M:%S")
        )
        if (!is.null(attr(transformed, "lambda")))    log_entry$lambda    <- attr(transformed, "lambda")
        if (!is.null(attr(transformed, "yj_object"))) log_entry$yj_object <- attr(transformed, "yj_object")
        log_entries[[new_var_name]] <- log_entry
        added <- c(added, new_var_name)
      }, error = function(e) {
        errors <<- c(errors, hstat_err_fr(e, sprintf("[%s]", var)))
      })
    }
    if (length(errors) > 0)
      showNotification(HTML(paste0("<b>Erreur(s):</b><br>", paste(errors, collapse = "<br>"))),
                       type = "error", duration = 12)
    if (length(skipped) > 0)
      showNotification(paste0("Déjà existante(s): ", paste(skipped, collapse = ", ")),
                       type = "warning", duration = 5)
    if (length(added) > 0) {
      values$filteredData      <- df
      values$transformationLog <- log_entries
      showNotification(
        HTML(paste0("<b>Transformation appliquée</b><br>",
                    length(added), " variable(s) créée(s): ",
                    paste0("<b>", added, "</b>", collapse = ", "))),
        type = "message", duration = 5)
    }
  })
  
  # Bloc 4 : Suppression d'une transformation
  observeEvent(input$removeTransformation, {
    req(input$removeTransformVar, values$filteredData)
    df          <- values$filteredData
    log_entries <- values$transformationLog
    removed <- c()
    for (vname in input$removeTransformVar) {
      if (vname %in% names(df)) { df[[vname]] <- NULL; removed <- c(removed, vname) }
      log_entries[[vname]] <- NULL
    }
    if (length(removed) > 0) {
      values$filteredData      <- df
      values$transformationLog <- log_entries
      showNotification(paste0("Supprimée(s): ", paste(removed, collapse = ", ")),
                       type = "warning", duration = 3)
    }
  })
  
  # Bloc 5 : Journal des transformations actives
  output$transformationLogDisplay <- renderUI({
    log <- values$transformationLog
    if (is.null(log) || length(log) == 0) {
      return(div(
        style = paste0("padding:10px;background:#f5f5f5;border-radius:4px;",
                       "font-size:12px;color:#888;text-align:center;border:1px dashed #ccc;"),
        icon("info-circle"), " Aucune transformation active"))
    }
    entries <- lapply(names(log), function(vname) {
      entry <- log[[vname]]
      lambda_tag <- if (!is.null(entry$lambda))
        tags$span(style = "font-size:10px;color:#757575;margin-left:4px;",
                  paste0("λ = ", entry$lambda)) else NULL
      div(
        style = paste0("display:flex;flex-wrap:wrap;align-items:center;",
                       "padding:5px 8px;background:#e8f5e9;",
                       "border-left:3px solid #4caf50;border-radius:3px;margin-bottom:4px;"),
        icon("check-circle", style = "color:#4caf50;flex-shrink:0;"),
        div(style = "margin-left:6px;flex:1;",
            tags$span(style = "font-size:12px;font-weight:bold;color:#2e7d32;",
                      entry$original, " → ", vname), tags$br(),
            tags$span(style = "font-size:11px;color:#555;font-family:monospace;",
                      entry$formula),
            lambda_tag,
            tags$span(style = "font-size:10px;color:#9e9e9e;margin-left:6px;",
                      paste0("@ ", entry$applied_at))
        )
      )
    })
    tagList(
      div(style = "margin-bottom:6px;",
          tags$b(style = "font-size:12px;color:#1b5e20;",
                 icon("history"), " ", length(log), " transformation(s) active(s)")),
      tagList(entries)
    )
  })
  
  # Bloc 6 : Sélecteur de suppression
  output$removeTransformSelect <- renderUI({
    log <- values$transformationLog
    if (is.null(log) || length(log) == 0) return(NULL)
    tagList(
      hr(style = "margin:8px 0;"),
      pickerInput(ns("removeTransformVar"), "Supprimer des transformations :",
                  choices = names(log), multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      actionButton(ns("removeTransformation"), "Supprimer la sélection",
                   class = "btn-danger btn-sm btn-block", icon = icon("trash"))
    )
  })
  
  output$factorVarSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- get_all_factor_candidates(values$filteredData)
    tagList(
      pickerInput(ns("factorVar"), "Facteur(s):",
                  choices  = fac_cols,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE)),
      tags$small(style = "color:#6c757d; font-size:11px;",
                 icon("info-circle"), " Facteur, texte, date et numérique (<= 30 niveaux) acceptés"),
      actionButton(ns("selectAllFactors"),   "Tout sélectionner",   class = "btn-success btn-sm"),
      actionButton(ns("deselectAllFactors"), "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  observeEvent(input$selectAllFactors, {
    updatePickerInput(session, "factorVar", selected = get_all_factor_candidates(values$filteredData))
  })
  
  observeEvent(input$deselectAllFactors, {
    updatePickerInput(session, "factorVar", selected = character(0))
  })
  
  observeEvent(input$testNormalityRaw, {
    req(input$responseVar)
    
    results_list <- list()
    
    df_norm <- values$filteredData
    if (length(input$factorVar) > 0) {
      for (f in input$factorVar) {
        if (!is.null(df_norm[[f]]) && !is.factor(df_norm[[f]])) {
          df_norm[[f]] <- factor(
            if (inherits(df_norm[[f]], c("Date","POSIXct","POSIXlt")))
              format(df_norm[[f]], "%Y-%m-%d")
            else as.character(df_norm[[f]])
          )
          df_norm[[f]] <- droplevels(df_norm[[f]])
        }
      }
    }
    
    for (var in input$responseVar) {
      tryCatch({
        data_values <- df_norm[[var]]
        data_values <- data_values[!is.na(data_values)]
        
        if (length(data_values) >= 3 && length(data_values) <= 5000) {
          norm_test <- hstat_shapiro(data_values)
          results_list[[var]] <- data.frame(
            Test = "Normalité (données brutes)",
            Variable = var,
            Facteur = "Global",
            Statistique = round(norm_test$statistic, 4),
            ddl = NA,
            p_value = norm_test$p.value,
            Interpretation = interpret_test_results("shapiro", norm_test$p.value),
            stringsAsFactors = FALSE
          )
        } else {
          results_list[[var]] <- data.frame(
            Test = "Normalité (données brutes)",
            Variable = var,
            Facteur = "Global",
            Statistique = NA,
            ddl = NA,
            p_value = NA,
            Interpretation = "Échantillon trop petit/grand pour Shapiro-Wilk",
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        results_list[[var]] <<- data.frame(
          Test = "Normalité (données brutes)",
          Variable = var,
          Facteur = "Global",
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = hstat_err_fr(e),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
    } else {
      showNotification("Aucun résultat de normalité généré", type = "warning")
    }
  })
  
  observeEvent(input$testHomogeneityRaw, {
    req(input$responseVar, input$factorVar)
    
    if (length(input$factorVar) != 1) {
      showNotification("Le test d'homogénéité nécessite exactement un facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        data_hom <- values$filteredData
        if (!is.factor(data_hom[[fvar]])) {
          if (inherits(data_hom[[fvar]], c("Date","POSIXct","POSIXlt"))) {
            data_hom[[fvar]] <- factor(format(data_hom[[fvar]], "%Y-%m-%d"))
          } else {
            data_hom[[fvar]] <- factor(as.character(data_hom[[fvar]]))
          }
          data_hom[[fvar]] <- droplevels(data_hom[[fvar]])
        }
        formula_str <- as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        levene_test <- car::leveneTest(formula_str, data = data_hom)
        
        results_list[[var]] <- data.frame(
          Test = "Homogénéité (données brutes)",
          Variable = var,
          Facteur = fvar,
          Statistique = round(levene_test$`F value`[1], 4),
          ddl = paste(levene_test$Df[1], ",", levene_test$Df[2]),
          p_value = levene_test$`Pr(>F)`[1],
          Interpretation = interpret_test_results("levene", levene_test$`Pr(>F)`[1]),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[var]] <<- data.frame(
          Test = "Homogénéité (données brutes)",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = hstat_err_fr(e),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
    } else {
      showNotification("Aucun résultat d'homogénéité généré", type = "warning")
    }
  })
  
  observeEvent(input$testT, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Le test t nécessite un seul facteur", type = "warning")
      return()
    }
    
    # Motifs de refus, collectes pendant la boucle. « Aucun résultat généré »
    # laisse l'utilisateur deviner ce qui cloche ; on lui dit ce qui bloque ET
    # quelle analyse convient a son cas.
    motifs_refus <- character(0)
    results_list <- list()
    normality_results <- list()
    homogeneity_results <- list()
    model_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        factor_levels <- levels(factor(as.character(values$filteredData[[fvar]])))
        
        if (length(factor_levels) != 2) {
          # `<-` et non `<<-` : l'expression d'un tryCatch est evaluee dans le
          # cadre de son appelant, donc ici meme. `<<-` sauterait ce cadre et
          # creerait une variable globale, laissant `motifs_refus` vide.
          motifs_refus <- c(motifs_refus, trf(
            "« %s » compte %d modalité(s) (%s). Le test t en compare exactement deux. %s",
            fvar, length(factor_levels),
            paste(utils::head(factor_levels, 5), collapse = ", "),
            if (length(factor_levels) > 2)
              "Pour plus de deux groupes, utilisez l'ANOVA — ou Kruskal-Wallis si la normalité n'est pas acquise."
            else
              "Vérifiez le facteur choisi : il ne distingue qu'un seul groupe."))
          next
        }
        
        group1_data <- values$filteredData[values$filteredData[[fvar]] == factor_levels[1], var]
        group2_data <- values$filteredData[values$filteredData[[fvar]] == factor_levels[2], var]
        
        group1_data <- group1_data[!is.na(group1_data)]
        group2_data <- group2_data[!is.na(group2_data)]
        
        normality_group1 <- if(length(group1_data) >= 3 && length(group1_data) <= 5000) {
          hstat_shapiro(group1_data)
        } else {
          list(p.value = NA)
        }
        
        normality_group2 <- if(length(group2_data) >= 3 && length(group2_data) <= 5000) {
          hstat_shapiro(group2_data)
        } else {
          list(p.value = NA)
        }
        
        test_data <- data.frame(
          values = c(group1_data, group2_data),
          group = factor(c(rep(factor_levels[1], length(group1_data)), 
                           rep(factor_levels[2], length(group2_data))))
        )
        
        homogeneity_test <- car::leveneTest(values ~ group, data = test_data)
        
        normality_results[[var]] <- list(
          group1 = normality_group1,
          group2 = normality_group2,
          group1_name = factor_levels[1],
          group2_name = factor_levels[2]
        )
        
        homogeneity_results[[var]] <- homogeneity_test
        
        # Exécuter le t-test et le modèle pour les diagnostics
        formula_str <- as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        test_result <- t.test(formula_str, data = values$filteredData)
        
        lm_model <- lm(formula_str, data = values$filteredData)
        model_list[[var]] <- lm_model
        
        results_list[[var]] <- data.frame(
          Test = "t-test",
          Variable = var,
          Facteur = fvar,
          Statistique = round(test_result$statistic, 4),
          ddl = round(test_result$parameter, 2),
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("t.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
        
      }, error = function(e) {
        results_list[[var]] <<- data.frame(
          Test = "t-test",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = hstat_err_fr(e),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- normality_results
      values$homogeneityResults <- homogeneity_results
      values$currentValidationVar <- 1
      values$modelList <- model_list
      values$currentModelVar <- 1
      values$currentTestType <- "parametric"
    } else {
      showNotification(
        if (length(motifs_refus))
          paste("Test t impossible :", paste(unique(motifs_refus), collapse = " "))
        else
          paste("Aucun résultat t-test généré. Vérifiez que les variables réponse",
                "sont numériques et comportent assez de données non manquantes."),
        type = "warning", duration = 12)
    }
  })
  
  observeEvent(input$testWilcox, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Le test de Wilcoxon nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        formula_str <- as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        test_result <- wilcox.test(formula_str, data = values$filteredData, exact = FALSE)
        
        results_list[[var]] <- data.frame(
          Test = "Wilcoxon",
          Variable = var,
          Facteur = fvar,
          Statistique = round(test_result$statistic, 4),
          ddl = NA,
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("wilcox.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[var]] <<- data.frame(
          Test = "Wilcoxon",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = hstat_err_fr(e),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
    } else {
      showNotification("Aucun résultat Wilcoxon généré", type = "warning")
    }
  })

  # ===========================================================================
  #  COMPARAISON A UNE VALEUR DE REFERENCE (NORME) -- tests a un echantillon
  # ---------------------------------------------------------------------------
  #  Les moteurs de calcul vivent dans Utils.R (hstat_ref_test /
  #  hstat_ref_prop_test). Ici on collecte les reglages de l'interface, on boucle
  #  sur les variables reponse selectionnees, et on alimente le tableau de
  #  resultats commun ainsi qu'un tableau de detail (estimation, intervalle de
  #  confiance, taille d'effet).
  # ===========================================================================

  # Niveau de confiance borne au domaine valide.
  .ref_conf <- function() {
    v <- suppressWarnings(as.numeric(input$refConf))[1]
    if (is.na(v) || v <= 0 || v >= 1) 0.95 else v
  }
  .ref_alt <- function() input$refAlt %||% "two.sided"
  # Libellé lisible d'une méthode, pour les lignes en erreur (les lignes
  # calculées portent déjà le nom complet renvoyé par le moteur).
  .ref_method_label <- function(m) switch(m,
    ttest = "Test t (1 échantillon)", ztest = "Test z (1 échantillon)",
    wilcoxon = "Wilcoxon signé (1 échantillon)", sign = "Test du signe (1 échantillon)",
    variance = "Chi² de conformité (variance)", tost = "TOST (équivalence à la norme)",
    m)

  # Lance une methode quantitative sur toutes les variables reponse choisies.
  .run_ref_quanti <- function(method, need_sigma = FALSE, need_margin = FALSE) {
    if (is.null(input$responseVar) || length(input$responseVar) == 0) {
      showNotification("Sélectionnez au moins une variable réponse.", type = "warning")
      return()
    }
    mu <- suppressWarnings(as.numeric(input$refValue))[1]
    if (is.na(mu)) {
      showNotification("Renseignez une valeur de référence numérique.", type = "error")
      return()
    }
    sigma <- suppressWarnings(as.numeric(input$refSigma))[1]
    if (need_sigma && (is.na(sigma) || sigma <= 0)) {
      showNotification(paste("Ce test exige un écart-type de référence",
                             "strictement positif."), type = "error")
      return()
    }
    margin <- suppressWarnings(as.numeric(input$refMargin))[1]
    if (need_margin && (is.na(margin) || margin <= 0)) {
      showNotification(paste("Le test d'équivalence exige une marge",
                             "strictement positive."), type = "error")
      return()
    }
    rows <- list(); details <- list(); notes <- character(0)
    for (var in input$responseVar) {
      res <- tryCatch(
        hstat_ref_test(values$filteredData[[var]], mu = mu, method = method,
                       alternative = .ref_alt(), conf.level = .ref_conf(),
                       sigma = if (is.na(sigma)) NULL else sigma,
                       margin = if (is.na(margin)) NULL else margin),
        error = function(e) hstat_err_fr(e))
      if (is.character(res)) {
        rows[[var]] <- data.frame(
          Test = .ref_method_label(method), Variable = var,
          Facteur = paste("Référence =", format(mu)),
          Statistique = NA_real_, ddl = NA_real_, p_value = NA_real_,
          Interpretation = res, stringsAsFactors = FALSE)
        next
      }
      rows[[var]] <- hstat_ref_result_row(res, var)
      details[[var]] <- .ref_detail_row(res, var)
      if (!is.na(res$note)) notes <- c(notes, res$note)
    }
    .push_ref_results(rows, details, notes)
  }

  # Ligne du tableau de detail (une par variable testee).
  .ref_detail_row <- function(res, var) {
    ic <- if (is.na(res$conf.low) && is.na(res$conf.high)) "-"
          else sprintf("[%s ; %s]",
                       if (is.finite(res$conf.low)) signif(res$conf.low, 5) else "-Inf",
                       if (is.finite(res$conf.high)) signif(res$conf.high, 5) else "+Inf")
    data.frame(
      Variable     = var,
      Test         = res$test,
      n            = res$n,
      Estimation   = signif(res$estimate, 6),
      Reference    = signif(res$reference, 6),
      Ecart        = signif(res$estimate - res$reference, 6),
      IC           = ic,
      Taille_effet = if (is.na(res$effect)) NA_real_ else signif(res$effect, 4),
      Mesure_effet = res$effect_label,
      Hypothese    = .hstat_ref_alt_label(res$alternative),
      p_value      = res$p.value,
      stringsAsFactors = FALSE)
  }

  # Publie les résultats dans le tableau commun + le tableau de détail.
  .push_ref_results <- function(rows, details, notes) {
    if (length(rows) == 0) {
      showNotification("Aucun résultat généré.", type = "warning")
      return()
    }
    values$testResultsDF     <- do.call(rbind, rows)
    values$refTestDetails    <- if (length(details)) do.call(rbind, details) else NULL
    values$refTestNotes      <- unique(notes)
    values$normalityResults  <- NULL
    values$homogeneityResults <- NULL
    values$currentTestType   <- "reference"
    ps <- vapply(rows, function(r) r$p_value[1], numeric(1))
    n_sig <- sum(!is.na(ps) & ps < 0.05)
    # Le TOST inverse l'hypothèse nulle : un p petit y conclut à l'ÉQUIVALENCE
    # et non à un écart. Le message doit suivre.
    is_tost <- any(grepl("TOST", vapply(rows, function(r) r$Test[1], character(1)),
                         fixed = TRUE))
    showNotification(
      if (is_tost)
        trf("%d test(s) d'équivalence : équivalence démontrée pour %d variable(s).",
                length(rows), n_sig)
      else
        trf("%d test(s) de conformité : %d écart(s) significatif(s) à la norme.",
                length(rows), n_sig),
      type = "message", duration = 5)
  }

  observeEvent(input$testRefT,      .run_ref_quanti("ttest"))
  observeEvent(input$testRefZ,      .run_ref_quanti("ztest", need_sigma = TRUE))
  observeEvent(input$testRefWilcox, .run_ref_quanti("wilcoxon"))
  observeEvent(input$testRefSign,   .run_ref_quanti("sign"))
  observeEvent(input$testRefVar,    .run_ref_quanti("variance", need_sigma = TRUE))
  observeEvent(input$testRefTOST,   .run_ref_quanti("tost", need_margin = TRUE))

  # --- Proportion / taux vs référence ---------------------------------------
  output$refPropVarSelect <- renderUI({
    req(values$filteredData)
    df <- values$filteredData
    cat_cols <- names(df)[sapply(df, function(x)
      is.factor(x) || is.character(x) || is.logical(x) ||
      (is.numeric(x) && length(unique(stats::na.omit(x))) <= 20))]
    if (length(cat_cols) == 0)
      return(div(style = "color:#c0392b; font-size:12px;",
                 "Aucune variable catégorielle disponible."))
    pickerInput(ns("refPropVar"),
                tagList(icon("tag"), " Variable catégorielle :"),
                choices = cat_cols, multiple = FALSE,
                options = list(`live-search` = TRUE))
  })

  output$refPropLevelSelect <- renderUI({
    req(values$filteredData, input$refPropVar)
    lv <- unique(stats::na.omit(as.character(values$filteredData[[input$refPropVar]])))
    if (length(lv) == 0) return(NULL)
    selectInput(ns("refPropLevel"),
                tagList(icon("check"), " Modalité comptée (« succès ») :"),
                choices = sort(lv))
  })

  .run_ref_prop <- function(method) {
    if (is.null(input$refPropVar) || is.null(input$refPropLevel)) {
      showNotification(paste("Choisissez une variable catégorielle et la",
                             "modalité à compter."), type = "warning")
      return()
    }
    p0 <- suppressWarnings(as.numeric(input$refPropP0))[1]
    if (is.na(p0) || p0 <= 0) {
      showNotification("Renseignez une proportion (ou un taux) de référence positive.",
                       type = "error")
      return()
    }
    v <- as.character(values$filteredData[[input$refPropVar]])
    v <- v[!is.na(v)]
    n <- length(v); k <- sum(v == input$refPropLevel)
    if (n == 0) {
      showNotification("Aucune observation valide pour cette variable.", type = "error")
      return()
    }
    res <- tryCatch(
      hstat_ref_prop_test(k, n, p0 = p0, method = method,
                          alternative = .ref_alt(), conf.level = .ref_conf()),
      error = function(e) hstat_err_fr(e))
    lbl <- paste0(input$refPropVar, " = ", input$refPropLevel)
    if (is.character(res)) {
      showNotification(res, type = "error", duration = 12)
      return()
    }
    row <- hstat_ref_result_row(res, lbl,
             reference_label = paste("Référence =", format(p0)))
    det <- .ref_detail_row(res, lbl)
    det$n <- n
    .push_ref_results(stats::setNames(list(row), lbl),
                      stats::setNames(list(det), lbl),
                      if (is.na(res$note)) character(0) else res$note)
  }

  observeEvent(input$testRefBinom,   .run_ref_prop("binom"))
  observeEvent(input$testRefProp,    .run_ref_prop("prop"))
  observeEvent(input$testRefPoisson, .run_ref_prop("poisson"))

  # Le détail ne s'affiche que tant que le dernier test lancé est bien un test
  # de conformité : sinon il resterait à l'écran après un autre test.
  output$hasRefTest <- reactive(
    !is.null(values$refTestDetails) &&
    identical(values$currentTestType, "reference"))
  outputOptions(output, "hasRefTest", suspendWhenHidden = FALSE)

  output$refTestDetails <- renderDT({
    req(values$refTestDetails, identical(values$currentTestType, "reference"))
    d <- values$refTestDetails
    d$p_value <- sapply(d$p_value, function(p) if (is.na(p)) NA_character_ else fmt_p(p))
    datatable(d, rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE, pageLength = 25))
  })

  output$refTestNote <- renderUI({
    n <- values$refTestNotes
    if (!identical(values$currentTestType, "reference")) return(NULL)
    if (is.null(n) || length(n) == 0) return(NULL)
    div(class = "callout callout-info", style = "margin-top:8px;",
        icon("circle-info"), strong(" À propos de ce test : "),
        tags$ul(lapply(n, tags$li)))
  })

  observeEvent(input$testKruskal, {
    req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      showNotification("Kruskal-Wallis nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    df_kw <- values$filteredData
    fvar_kw <- input$factorVar[1]
    if (!is.factor(df_kw[[fvar_kw]])) {
      df_kw[[fvar_kw]] <- factor(
        if (inherits(df_kw[[fvar_kw]], c("Date","POSIXct","POSIXlt")))
          format(df_kw[[fvar_kw]], "%Y-%m-%d")
        else as.character(df_kw[[fvar_kw]])
      )
      df_kw[[fvar_kw]] <- droplevels(df_kw[[fvar_kw]])
    }
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- fvar_kw
        formula_str <- as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        test_result <- kruskal.test(formula_str, data = df_kw)
        
        results_list[[var]] <- data.frame(
          Test = "Kruskal-Wallis",
          Variable = var,
          Facteur = fvar,
          Statistique = round(test_result$statistic, 4),
          ddl = test_result$parameter,
          p_value = test_result$p.value,
          Interpretation = interpret_test_results("kruskal.test", test_result$p.value),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        results_list[[var]] <<- data.frame(
          Test = "Kruskal-Wallis",
          Variable = var,
          Facteur = fvar,
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = hstat_err_fr(e),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
    } else {
      showNotification("Aucun résultat Kruskal-Wallis généré", type = "warning")
    }
  })
  
  observeEvent(input$testScheirerRayHare, {
    req(input$responseVar, input$factorVar)
    
    if (length(input$factorVar) < 2) {
      showNotification("Scheirer-Ray-Hare nécessite au moins 2 facteurs", type = "warning")
      return()
    }
    
    results_list <- list()
    error_messages <- c()
    
    for (var in input$responseVar) {
      tryCatch({
        # Vérifier que les données sont valides + convertir les facteurs 
        test_data <- values$filteredData[, c(var, input$factorVar), drop = FALSE]
        for (f in input$factorVar) {
          if (!is.factor(test_data[[f]])) {
            test_data[[f]] <- factor(
              if (inherits(test_data[[f]], c("Date","POSIXct","POSIXlt")))
                format(test_data[[f]], "%Y-%m-%d")
              else as.character(test_data[[f]])
            )
            test_data[[f]] <- droplevels(test_data[[f]])
          }
        }
        test_data <- na.omit(test_data)
        
        if (nrow(test_data) < 3) {
          error_messages <- c(error_messages, paste(var, ": Pas assez de données après suppression des NA"))
          next
        }
        
        safe_resp    <- "resp_var_srh"
        safe_factors <- paste0("factor_srh_", seq_along(input$factorVar))
        # Table de correspondance pour restaurer les noms dans les résultats
        factor_label_map <- setNames(input$factorVar, safe_factors)
        if (length(input$factorVar) == 2) {
          factor_label_map[paste0(safe_factors[1], ":", safe_factors[2])] <-
            paste0(input$factorVar[1], ":", input$factorVar[2])
        }
        
        safe_data <- test_data
        names(safe_data)[names(safe_data) == var] <- safe_resp
        for (fi in seq_along(input$factorVar)) {
          names(safe_data)[names(safe_data) == input$factorVar[fi]] <- safe_factors[fi]
        }
        
        # Préparer la formule avec noms sûrs
        if (isTRUE(input$interaction) && length(input$factorVar) == 2) {
          formula_str <- as.formula(paste0(safe_resp, " ~ ", paste(safe_factors, collapse = "*")))
        } else {
          formula_str <- as.formula(paste0(safe_resp, " ~ ", paste(safe_factors, collapse = "+")))
        }
        
        test_result <- rcompanion::scheirerRayHare(formula_str, data = safe_data)
        
        # Restaurer les noms originaux dans les rownames du résultat
        orig_rnames <- rownames(test_result)
        for (sf in names(factor_label_map)) {
          orig_rnames <- gsub(sf, factor_label_map[sf], orig_rnames, fixed = TRUE)
        }
        rownames(test_result) <- orig_rnames
        
        if (is.null(test_result) || nrow(test_result) == 0) {
          error_messages <- c(error_messages, paste(var, ": Test n'a produit aucun résultat"))
          next
        }
        
        effects_found <- 0
        for (i in seq_len(nrow(test_result))) {
          effect_name <- rownames(test_result)[i]
          if (!is.null(effect_name) && effect_name != "Residuals" && !is.na(effect_name)) {
            results_list[[paste(var, effect_name, sep = "_")]] <- data.frame(
              Test = "Scheirer-Ray-Hare",
              Variable = var,
              Facteur = effect_name,
              Statistique = round(test_result$H[i], 4),
              ddl = test_result$Df[i],
              p_value = test_result$`p.value`[i],
              Interpretation = interpret_test_results("scheirerRayHare", test_result$`p.value`[i]),
              stringsAsFactors = FALSE
            )
            effects_found <- effects_found + 1
          }
        }
        
        if (effects_found == 0) {
          error_messages <- c(error_messages, paste(var, ": Aucun effet trouvé (uniquement des résidus)"))
        }
        
        values$scheirerResults <- test_result
        
      }, error = function(e) {
        error_msg <- hstat_err_fr(e, var)
        error_messages <<- c(error_messages, error_msg)
        
        results_list[[var]] <<- data.frame(
          Test = "Scheirer-Ray-Hare",
          Variable = var,
          Facteur = paste(input$factorVar, collapse = " + "),
          Statistique = NA,
          ddl = NA,
          p_value = NA,
          Interpretation = hstat_err_fr(e),
          stringsAsFactors = FALSE
        )
      })
    }
    
    if (length(error_messages) > 0) {
      showNotification(
        paste("Problèmes détectés:\n", paste(error_messages, collapse = "\n")),
        type = "warning",
        duration = 10
      )
    }
    
    if (length(results_list) > 0) {
      values$testResultsDF <- do.call(rbind, results_list)
      values$normalityResults <- NULL
      values$homogeneityResults <- NULL
      values$currentTestType <- "non-parametric"
      
      showNotification(
        paste("Test Scheirer-Ray-Hare terminé:", length(results_list), "résultat(s) généré(s)"),
        type = "message",
        duration = 3
      )
    } else {
      showNotification("Aucun résultat Scheirer-Ray-Hare généré. Vérifiez vos données et facteurs.", type = "error", duration = 10)
    }
  })
  
  observeEvent(input$testANOVA, {
    req(input$responseVar, input$factorVar)
    
    results_list <- list()
    normality_results <- list()
    homogeneity_results <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (f in input$factorVar) {
        if (!is.factor(df[[f]])) df[[f]] <- factor(df[[f]])
      }
      
      for (var in input$responseVar) {
        if (!is.numeric(df[[var]])) df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
        if (all(is.na(df[[var]]))) {
          showNotification(paste0("ANOVA : '", var, "' non numérique -- ignorée."), type = "warning", duration = 5)
          next
        }
        df_clean <- df[, c(var, input$factorVar), drop = FALSE]
        df_clean <- df_clean[complete.cases(df_clean), ]
        if (nrow(df_clean) < 4) { showNotification(paste0("ANOVA : trop peu d'obs pour '", var, "'."), type = "warning", duration = 4); next }
        for (f in input$factorVar) {
          # Conversion universelle: tous les types vers facteur
          if (!is.factor(df_clean[[f]])) {
            df_clean[[f]] <- tryCatch(
              factor(as.character(df_clean[[f]])),
              error = function(e) factor(df_clean[[f]])
            )
          }
          df_clean[[f]] <- droplevels(df_clean[[f]])
        }
        formula_str <- paste0("`", var, "` ~ ", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = ifelse(input$interaction, "*", "+")))
        model <- aov(as.formula(formula_str), data = df_clean)
        anova_table <- summary(model)[[1]]
        
        model_list[[var]] <- model
        
        for (i in 1:(nrow(anova_table) - 1)) {
          effect_name <- rownames(anova_table)[i]
          results_list[[paste(var, effect_name, sep = "_")]] <- data.frame(
            Test = "ANOVA",
            Variable = var,
            Facteur = effect_name,
            Statistique = round(anova_table$`F value`[i], 4),
            ddl = paste(anova_table$Df[i], ",", anova_table$Df[nrow(anova_table)]),
            p_value = anova_table$`Pr(>F)`[i],
            Interpretation = interpret_test_results("anova", anova_table$`Pr(>F)`[i]),
            stringsAsFactors = FALSE
          )
        }
        
        residuals_data <- residuals(model)
        if (length(residuals_data) > 3) {
          normality_results[[var]] <- hstat_shapiro(residuals_data)
        }
        
        fitted_data <- fitted(model)
        fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
        test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
        homogeneity_results[[var]] <- car::leveneTest(residuals ~ fitted_group, data = test_data)
      }
      
      if (length(results_list) > 0) {
        values$testResultsDF <- do.call(rbind, results_list)
        values$anovaModel <- model
        values$currentModel <- model
        values$modelList <- model_list
        values$currentModelVar <- 1
        values$normalityResults <- normality_results
        values$homogeneityResults <- homogeneity_results
        values$currentValidationVar <- 1
        values$currentTestType <- "parametric"
      } else {
        showNotification("Aucun résultat ANOVA généré", type = "warning")
      }
      
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur ANOVA"), type = "error")
    })
  })
  
  observeEvent(input$testLM, {
    req(input$responseVar, input$factorVar)
    
    results_list <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (var in input$responseVar) {
        formula_str <- paste0("`", var, "` ~ ", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+"))
        model <- lm(as.formula(formula_str), data = df)
        summary_model <- summary(model)
        
        model_list[[var]] <- model
        
        results_list[[paste(var, "global", sep = "_")]] <- data.frame(
          Test = "Régression linéaire",
          Variable = var,
          Facteur = "Modèle global",
          Statistique = round(summary_model$fstatistic[1], 4),
          ddl = paste(summary_model$fstatistic[2], ",", summary_model$fstatistic[3]),
          p_value = pf(summary_model$fstatistic[1], summary_model$fstatistic[2], 
                       summary_model$fstatistic[3], lower.tail = FALSE),
          Interpretation = paste("R² =", round(summary_model$r.squared, 4)),
          stringsAsFactors = FALSE
        )
        
        coef_table <- summary_model$coefficients
        for (i in 2:nrow(coef_table)) {
          results_list[[paste(var, rownames(coef_table)[i], sep = "_")]] <- data.frame(
            Test = "Régression linéaire",
            Variable = var,
            Facteur = rownames(coef_table)[i],
            Statistique = round(coef_table[i, "t value"], 4),
            ddl = summary_model$df[2],
            p_value = coef_table[i, "Pr(>|t|)"],
            Interpretation = interpret_test_results("lm", coef_table[i, "Pr(>|t|)"]),
            stringsAsFactors = FALSE
          )
        }
      }
      
      if (length(results_list) > 0) {
        values$testResultsDF <- do.call(rbind, results_list)
        values$currentModel <- model
        values$modelList <- model_list
        values$currentModelVar <- 1
        values$currentTestType <- "parametric"
      } else {
        showNotification("Aucun résultat de régression généré", type = "warning")
      }
      
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur régression"), type = "error")
    })
  })
  
  observeEvent(input$testGLM, {
    req(input$responseVar, input$factorVar)
    
    results_list <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (var in input$responseVar) {
        formula_str <- paste0("`", var, "` ~ ", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+"))
        gl <- hstat_glm_fit(as.formula(formula_str), data = df, family = gaussian())
        model <- gl$fit
        if (!is.null(gl$note))
          showNotification(paste0("GLM (", var, ") : ", gl$note),
                           type = "warning", duration = 12)
        summary_model <- summary(model)

        model_list[[var]] <- model
        
        coef_table <- summary_model$coefficients
        # Détection dynamique des colonnes (t value pour gaussian, z value pour autres familles)
        stat_col  <- if ("z value"   %in% colnames(coef_table)) "z value"   else "t value"
        pval_col  <- if ("Pr(>|z|)"  %in% colnames(coef_table)) "Pr(>|z|)"  else "Pr(>|t|)"
        df_resid   <- model$df.residual
        df_null    <- model$df.null
        n_params   <- nrow(coef_table)
        if (nrow(coef_table) < 1) {
          showNotification(
            paste0("GLM (", var, ") : le modèle est vide."),
            type = "warning"
          )
        } else {
          # Inclure l'intercept (ligne 1) ET tous les coefficients suivants
          for (i in seq_len(nrow(coef_table))) {
            label <- rownames(coef_table)[i]
            # ddl : résiduel pour l'intercept, df null - df résiduel pour les facteurs
            ddl_i <- if (i == 1) df_resid else (df_null - df_resid)
            results_list[[paste(var, label, sep = "_")]] <- data.frame(
              Test = "GLM",
              Variable = var,
              Facteur = label,
              Statistique = round(coef_table[i, stat_col], 4),
              ddl = ddl_i,
              p_value = coef_table[i, pval_col],
              Interpretation = if (i == 1) paste0("Intercept : ", round(coef_table[i, "Estimate"], 4))
              else interpret_test_results("glm", coef_table[i, pval_col]),
              stringsAsFactors = FALSE
            )
          }
        }
      }
      
      if (length(results_list) > 0) {
        values$testResultsDF <- do.call(rbind, results_list)
        values$currentModel <- model
        values$modelList <- model_list
        values$currentModelVar <- 1
        values$currentTestType <- "parametric"
      } else {
        showNotification("Aucun résultat GLM généré", type = "warning")
      }
      
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur GLM"), type = "error")
    })
  })
  
  # --- Infobulle dynamique : type de donnees correspondant a la famille choisie ---
  output$glmmFamilyHelp <- renderUI({
    fam <- input$glmmFamily %||% "gaussian"
    info <- switch(fam,
      "gaussian" = list(
        titre = "Gaussienne (normale)",
        txt   = "Réponse continue à valeurs réelles, résidus ~ symétriques (en cloche). Ex. : rendement, taille, poids, biomasse, concentration. Lien canonique : identity. Donne un LMM."),
      "binomial" = list(
        titre = "Binomiale",
        txt   = "Données binaires (0/1 : succès/échec, présence/absence) ou proportions issues d'un comptage sur un total connu (k atteints sur n). Lien : logit (ou probit, cloglog)."),
      "poisson" = list(
        titre = "Poisson",
        txt   = "Comptages d'événements sans plafond (entiers ≥ 0) : nb d'insectes, de lésions, de captures, de fruits. Variance = moyenne. Lien : log."),
      "Gamma" = list(
        titre = "Gamma",
        txt   = "Réponse continue strictement positive et asymétrique à droite : durées, temps d'attente, montants, surfaces. Variance ∝ moyenne². Lien usuel : log (ou inverse)."),
      "nbinom" = list(
        titre = "Binomiale négative (glmmTMB)",
        txt   = "Comptages surdispersés (variance > moyenne), fréquents en écologie/agronomie (agrégation, hétérogénéité). À utiliser si Poisson montre de la surdispersion. Entiers ≥ 0."),
      "inverse.gaussian" = list(
        titre = "Inverse gaussienne",
        txt   = "Données continues positives très fortement asymétriques (queue à droite plus longue que Gamma). Cas plus rare. Lien canonique : 1/µ²."),
      "beta_family" = list(
        titre = "Beta (glmmTMB)",
        txt   = "Proportions/taux continus strictement dans ]0,1[ (0 et 1 exclus), NON issus d'un comptage : taux de couverture, fraction de surface atteinte, indices bornés. Lien : logit."),
      "tweedie" = list(
        titre = "Tweedie (glmmTMB)",
        txt   = "Données continues positives avec excès de zéros exacts : biomasse souvent nulle puis continue, captures, précipitations. Interpole entre Poisson et Gamma."),
      list(titre = fam, txt = "")
    )
    div(style = "background:#e8f4fd; border-left:4px solid #1565C0; border-radius:6px; padding:8px 10px; margin:6px 0 4px 0;",
        div(style = "font-size:11px; font-weight:bold; color:#0d47a1; margin-bottom:2px;",
            icon("circle-info"), " ", info$titre),
        div(style = "font-size:11px; color:#37474f; line-height:1.4;", info$txt))
  })
  
  # --- Infobulle dynamique : quand utiliser la fonction de lien choisie ---
  output$glmmLinkHelp <- renderUI({
    lk  <- input$glmmLink %||% "auto"
    info <- switch(lk,
      "auto" = list(
        titre = "Automatique (lien canonique)",
        txt   = "À privilégier par défaut : applique le lien naturel de la famille (identity pour gaussienne, logit pour binomiale/Beta, log pour Poisson/Gamma/nbinom, 1/µ² pour inverse gaussienne). Estimation stable, interprétation cohérente."),
      "identity" = list(
        titre = "identity (µ = prédicteur)",
        txt   = "Effets ADDITIFS, coefficients directement dans l'unité de Y. Lien naturel de la gaussienne. À éviter avec familles bornées (Poisson, binomiale) : peut prédire des valeurs impossibles."),
      "log" = list(
        titre = "log",
        txt   = "Effets MULTIPLICATIFS : exp(coef) = ratio (facteur multiplicatif). Canonique pour Poisson, usuel pour Gamma et binomiale négative. Pour comptages/réponses positives ; garantit des prédictions > 0."),
      "logit" = list(
        titre = "logit (log-odds)",
        txt   = "Pour probabilités/proportions : exp(coef) = odds ratio. Choix par défaut des données binaires (présence/absence) et taux dans ]0,1[. Courbe symétrique autour de p = 0,5. Canonique : binomiale, Beta."),
      "probit" = list(
        titre = "probit",
        txt   = "Alternative au logit pour données binaires/binomiales, basée sur la loi normale. Ajustement très proche du logit. Préféré en économétrie, psychométrie, toxicologie (dose-réponse) ou si variable latente gaussienne."),
      "cloglog" = list(
        titre = "cloglog (complementary log-log)",
        txt   = "Lien ASYMÉTRIQUE pour données binaires/binomiales. À utiliser quand les proportions sont déséquilibrées (succès très rare ou très fréquent), en survie à temps discret, épidémiologie, ou processus « premier contact »."),
      "inverse" = list(
        titre = "inverse (1/µ)",
        txt   = "Lien canonique de la Gamma, mais souvent délaissé au profit de log (plus interprétable, positivité garantie). À réserver aux cas où la théorie suggère une relation en 1/µ (taux, vitesses, débits)."),
      "sqrt" = list(
        titre = "sqrt (racine carrée)",
        txt   = "Stabilisateur de variance parfois utilisé avec Poisson (rapproche la variance d'une constante). Alternative au log pour comptages quand on veut une échelle additive ou que le log donne des effets trop extrêmes."),
      list(titre = lk, txt = "")
    )
    div(style = "background:#fff8e1; border-left:4px solid #f9a825; border-radius:6px; padding:8px 10px; margin:4px 0 6px 0;",
        div(style = "font-size:11px; font-weight:bold; color:#e65100; margin-bottom:2px;",
            icon("link"), " ", info$titre),
        div(style = "font-size:11px; color:#5d4037; line-height:1.4;", info$txt),
        div(style = "font-size:10px; color:#9e9e9e; margin-top:4px; font-style:italic;",
            icon("triangle-exclamation"),
            " Tous les liens ne sont pas compatibles avec toutes les familles (R renverra une erreur sinon)."))
  })
  
  # --- Mise a jour des choix des selecteurs de mesures repetees (UI statique) ---
  observe({
    df <- values$filteredData %||% values$cleanData %||% values$data
    req(df)
    all_cols <- names(df)
    fac_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x) ||
                                   (is.numeric(x) && length(unique(x[!is.na(x)])) <= 20))]
    if (length(fac_cols) == 0) fac_cols <- all_cols
    updateSelectInput(session, "rmSubject", choices = all_cols,
                      selected = isolate(input$rmSubject) %||% all_cols[1])
    updateSelectizeInput(session, "rmWithin", choices = fac_cols,
                         selected = isolate(input$rmWithin))
    updateSelectizeInput(session, "rmBetween", choices = fac_cols,
                         selected = isolate(input$rmBetween))
  })
  
  # --- Selecteur COMPLET de la structure d'effets aleatoires du GLMM ---
  # Interface guidee : groupes croises, emboitement, pentes aleatoires,
  # + saisie libre prioritaire. Un apercu de la formule est affiche.
  output$glmmRandomSelect <- renderUI({
    df <- values$filteredData %||% values$cleanData %||% values$data
    req(df)
    cand <- tryCatch(get_all_factor_candidates(df), error = function(e) NULL)
    if (is.null(cand) || length(cand) == 0) {
      cand <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    }
    if (length(cand) == 0) cand <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    tagList(
      selectizeInput(ns("glmmRandom"),
                     tagList(icon("layer-group"), " Variable(s) de groupement :"),
                     choices = cand, selected = cand[1], multiple = TRUE,
                     options = list(plugins = list("remove_button"),
                                    placeholder = "Un ou plusieurs groupes...")),
      # 2) Emboitement (nested) au lieu de croise
      conditionalPanel(
        ns = ns,
        condition = "input.glmmRandom && input.glmmRandom.length > 1",
        checkboxInput(ns("glmmNested"),
                      tagList(icon("diagram-next"), " Emboîter les groupes (A/B/...) au lieu de croisés"),
                      value = FALSE),
        div(style = "font-size:10px; color:#7f8c8d; margin:-4px 0 6px 0;",
            "Croisés : ", tags$code("(1|A) + (1|B)"), " — Emboîtés : ", tags$code("(1|A/B)"), ".")
      ),
      checkboxInput(ns("glmmUseSlope"),
                    tagList(icon("chart-line"), " Ajouter une pente aléatoire"),
                    value = FALSE),
      conditionalPanel(
        ns = ns,
        condition = "input.glmmUseSlope == true",
        if (length(num_cols) > 0) {
          selectInput(ns("glmmSlopeVar"), "Variable de pente (continue) :",
                      choices = num_cols, selected = num_cols[1])
        } else {
          div(class = "alert alert-warning", style = "padding:6px; font-size:11px;",
              icon("exclamation-triangle"), " Aucune variable continue pour une pente.")
        },
        checkboxInput(ns("glmmCorrSlope"),
                      "Corréler pente et ordonnée à l'origine",
                      value = TRUE),
        div(style = "font-size:10px; color:#7f8c8d; margin:-4px 0 6px 0;",
            "Corrélés : ", tags$code("(1+X|g)"), " — Indépendants : ", tags$code("(1|g) + (0+X|g)"), ".")
      ),
      hr(style = "margin:8px 0;"),
      textInput(ns("glmmRandomCustom"),
                tagList(icon("pen"), " Formule libre (avancé, prioritaire) :"),
                value = "", placeholder = "ex. (1|Bloc) + (1+Dose|Site)"),
      div(style = "font-size:10px; color:#7f8c8d; margin-top:-4px;",
          "Si renseigné, ce champ remplace l'interface guidée ci-dessus."),
      # 5) Apercu de la formule complete
      div(style = "background:#ede7f6; border:1px solid #b39ddb; border-radius:6px; padding:8px 10px; margin-top:8px;",
          div(style = "font-size:10px; color:#6c3483; font-weight:bold; margin-bottom:3px;",
              icon("eye"), " Aperçu de la formule :"),
          uiOutput(ns("glmmFormulaPreview")))
    )
  })
  
  # Construit le terme d'effets aleatoires a partir de l'interface OU du champ libre.
  # Source unique de verite : utilisee par l'apercu ET par l'observer.
  glmm_random_term <- reactive({
    custom <- input$glmmRandomCustom
    if (!is.null(custom) && nzchar(trimws(custom))) {
      return(trimws(custom))
    }
    groups <- input$glmmRandom
    if (is.null(groups) || length(groups) == 0) return(NULL)
    
    bt <- function(x) paste0("`", x, "`")
    use_slope <- isTRUE(input$glmmUseSlope) && !is.null(input$glmmSlopeVar) && nzchar(input$glmmSlopeVar %||% "")
    slope_var <- input$glmmSlopeVar
    corr      <- isTRUE(input$glmmCorrSlope)
    
    # Emboitement : un seul terme (1|A/B/C)
    if (length(groups) > 1 && isTRUE(input$glmmNested)) {
      nested <- paste(sapply(groups, bt), collapse = "/")
      if (use_slope) {
        if (corr) return(paste0("(1 + ", bt(slope_var), " | ", nested, ")"))
        return(paste0("(1 | ", nested, ") + (0 + ", bt(slope_var), " | ", nested, ")"))
      }
      return(paste0("(1 | ", nested, ")"))
    }
    
    # Croises : un terme par groupe
    terms <- vapply(groups, function(g) {
      if (use_slope) {
        if (corr) paste0("(1 + ", bt(slope_var), " | ", bt(g), ")")
        else      paste0("(1 | ", bt(g), ") + (0 + ", bt(slope_var), " | ", bt(g), ")")
      } else {
        paste0("(1 | ", bt(g), ")")
      }
    }, character(1))
    paste(terms, collapse = " + ")
  })
  
  # Apercu en direct de la formule complete (fixe + aleatoire).
  output$glmmFormulaPreview <- renderUI({
    rand <- glmm_random_term()
    resp <- if (!is.null(input$responseVar) && length(input$responseVar) > 0) input$responseVar[1] else "réponse"
    fixed <- if (!is.null(input$factorVar) && length(input$factorVar) > 0) {
      paste(sapply(input$factorVar, function(x) paste0("`", x, "`")),
            collapse = if (isTRUE(input$interaction)) " * " else " + ")
    } else "facteur"
    if (is.null(rand) || !nzchar(rand)) {
      return(div(style = "font-family:monospace; font-size:11px; color:#c0392b;",
                 icon("triangle-exclamation"), " Sélectionnez au moins une variable de groupement."))
    }
    div(style = "font-family:monospace; font-size:12px; color:#4a235a; word-break:break-all;",
        paste0("`", resp, "` ~ ", fixed, " + ", rand))
  })
  
  # --- Modele lineaire (generalise) mixte : lme4 ou glmmTMB ---
  observeEvent(input$testGLMM, {
    req(input$responseVar, input$factorVar)
    
    df <- values$filteredData %||% values$cleanData %||% values$data
    if (is.null(df)) { showNotification("Aucune donnée disponible.", type = "error"); return() }
    
    rand_term <- glmm_random_term()
    if (is.null(rand_term) || !nzchar(rand_term)) {
      showNotification("Définissez la structure d'effets aléatoires (panneau « Modèle mixte ») : au moins une variable de groupement ou une formule libre.",
                       type = "warning", duration = 6)
      return()
    }
    # Verifier que les variables de groupement existent (sauf si formule libre,
    # ou l'utilisateur est responsable de la syntaxe).
    custom_used <- !is.null(input$glmmRandomCustom) && nzchar(trimws(input$glmmRandomCustom))
    if (!custom_used) {
      missing_grp <- setdiff(input$glmmRandom, names(df))
      if (length(missing_grp) > 0) {
        showNotification(paste("Variable(s) d'effet aléatoire introuvable(s) :",
                               paste(missing_grp, collapse = ", ")), type = "error")
        return()
      }
    }
    
    engine <- input$glmmEngine %||% "lme4"
    fam    <- input$glmmFamily %||% "gaussian"
    link   <- input$glmmLink   %||% "auto"
    
    # Verifier la disponibilite du moteur demande.
    if (engine == "lme4" && !requireNamespace("lme4", quietly = TRUE)) {
      showNotification("Le package 'lme4' n'est pas installé.", type = "error"); return()
    }
    if (engine == "glmmTMB" && !requireNamespace("glmmTMB", quietly = TRUE)) {
      showNotification("Le package 'glmmTMB' n'est pas installé.", type = "error"); return()
    }
    # Certaines familles ne sont disponibles que via glmmTMB.
    if (engine == "lme4" && fam %in% c("nbinom", "beta_family", "tweedie")) {
      showNotification(paste0("La famille « ", fam, " » nécessite le moteur glmmTMB. Changez de moteur."),
                       type = "warning", duration = 7); return()
    }
    
    # Construit l'objet famille selon le moteur, la famille et le lien choisis.
    build_family <- function(engine, fam, link) {
      lk <- if (identical(link, "auto")) NULL else link
      if (engine == "glmmTMB") {
        switch(fam,
          "gaussian"         = if (is.null(lk)) glmmTMB::gaussian()         else glmmTMB::gaussian(link = lk),
          "binomial"         = if (is.null(lk)) glmmTMB::binomial()         else glmmTMB::binomial(link = lk),
          "poisson"          = if (is.null(lk)) glmmTMB::poisson()          else glmmTMB::poisson(link = lk),
          "Gamma"            = if (is.null(lk)) glmmTMB::Gamma(link = "log")else glmmTMB::Gamma(link = lk),
          "inverse.gaussian" = if (is.null(lk)) glmmTMB::inverse.gaussian() else glmmTMB::inverse.gaussian(link = lk),
          "nbinom"           = if (is.null(lk)) glmmTMB::nbinom2()          else glmmTMB::nbinom2(link = lk),
          "beta_family"      = if (is.null(lk)) glmmTMB::beta_family()      else glmmTMB::beta_family(link = lk),
          "tweedie"          = if (is.null(lk)) glmmTMB::tweedie()          else glmmTMB::tweedie(link = lk),
          glmmTMB::gaussian())
      } else {
        switch(fam,
          "gaussian"         = if (is.null(lk)) gaussian()         else gaussian(link = lk),
          "binomial"         = if (is.null(lk)) binomial()         else binomial(link = lk),
          "poisson"          = if (is.null(lk)) poisson()          else poisson(link = lk),
          "Gamma"            = if (is.null(lk)) Gamma(link = "log")else Gamma(link = lk),
          "inverse.gaussian" = if (is.null(lk)) inverse.gaussian() else inverse.gaussian(link = lk),
          gaussian())
      }
    }
    
    # --- Validation prealable : la famille est-elle compatible avec les donnees ? ---
    # Evite des messages d'erreur bruts de R (ex. "y values must be 0 <= y <= 1").
    fam_check <- function(var) {
      y <- suppressWarnings(as.numeric(df[[var]]))
      y <- y[!is.na(y)]
      if (length(y) == 0) return(paste0("« ", var, " » : aucune valeur numérique exploitable."))
      rng <- range(y)
      is_int <- all(abs(y - round(y)) < 1e-8)
      msg <- NULL
      if (fam == "binomial") {
        # Tolere le 0/1 ; ou proportions dans [0,1] ; sinon erreur.
        if (rng[1] < 0 || rng[2] > 1)
          msg <- trf("La famille « Binomiale » exige une réponse entre 0 et 1 (binaire 0/1 ou proportion), mais « %s » varie de %.3g à %.3g. Utilisez une variable 0/1 ou une proportion, ou changez de famille (Poisson/nbinom pour des comptages, Gamma/Gaussienne pour du continu).",
                         var, rng[1], rng[2])
      } else if (fam == "beta_family") {
        if (rng[1] <= 0 || rng[2] >= 1)
          msg <- trf("La famille « Beta » exige une réponse strictement comprise entre 0 et 1 (exclus), mais « %s » varie de %.3g à %.3g. Pour des proportions incluant 0 ou 1, utilisez la binomiale ; pour du continu positif, la Gamma.",
                         var, rng[1], rng[2])
      } else if (fam == "poisson") {
        if (rng[1] < 0)
          msg <- trf("La famille « Poisson » exige des comptages ≥ 0, mais « %s » contient des valeurs négatives (min = %.3g).", var, rng[1])
        else if (!is_int)
          msg <- trf("La famille « Poisson » attend des nombres entiers (comptages), mais « %s » contient des valeurs décimales. Utilisez Gamma ou Gaussienne pour du continu, ou nbinom si surdispersion.", var)
      } else if (fam == "nbinom") {
        if (rng[1] < 0)
          msg <- trf("La famille « Binomiale négative » exige des comptages ≥ 0, mais « %s » contient des valeurs négatives (min = %.3g).", var, rng[1])
        else if (!is_int)
          msg <- trf("La famille « Binomiale négative » attend des nombres entiers (comptages), mais « %s » contient des valeurs décimales.", var)
      } else if (fam %in% c("Gamma", "inverse.gaussian", "tweedie")) {
        lo <- if (fam == "tweedie") -1e-9 else 0  # Tweedie tolere les zeros
        if (rng[1] < lo || (fam != "tweedie" && rng[1] <= 0))
          msg <- trf("La famille « %s » exige une réponse strictement positive%s, mais « %s » a un minimum de %.3g. Utilisez la Gaussienne pour des valeurs réelles, ou Tweedie si beaucoup de zéros.",
                         fam, if (fam == "tweedie") " ou nulle" else "", var, rng[1])
      }
      msg
    }
    bad_msgs <- Filter(Negate(is.null), lapply(input$responseVar, fam_check))
    if (length(bad_msgs) > 0) {
      showNotification(bad_msgs[[1]], type = "error", duration = 12)
      return()
    }
    
    results_list <- list()
    model_list   <- list()
    is_gaussian  <- (fam == "gaussian")
    
    withProgress(message = "Ajustement du modèle mixte...", value = 0.2, {
      tryCatch({
        fam_obj <- build_family(engine, fam, link)
        n_resp  <- length(input$responseVar)
        
        for (var in input$responseVar) {
          fixed <- paste(sapply(input$factorVar, function(x) paste0("`", x, "`")),
                         collapse = if (isTRUE(input$interaction)) " * " else " + ")
          formula_str <- paste0("`", var, "` ~ ", fixed, " + ", rand_term)
          form <- as.formula(formula_str)
          
          # LMM gaussien -> lmer (lme4) ; sinon glmer / glmmTMB.
          model <- if (engine == "glmmTMB") {
            glmmTMB::glmmTMB(form, data = df, family = fam_obj)
          } else if (is_gaussian) {
            # lmerTest::lmer ajoute ddl + p-values (Satterthwaite) ; repli sur lme4.
            if (requireNamespace("lmerTest", quietly = TRUE)) {
              lmerTest::lmer(form, data = df)
            } else {
              lme4::lmer(form, data = df)
            }
          } else {
            lme4::glmer(form, data = df, family = fam_obj)
          }
          
          model_list[[var]] <- model
          
          # Extraction des coefficients d'effets fixes (table commune aux 2 moteurs).
          coef_table <- if (engine == "glmmTMB") {
            cc <- summary(model)$coefficients$cond
            cc
          } else {
            summary(model)$coefficients
          }
          if (is.null(coef_table) || nrow(coef_table) < 1) {
            showNotification(paste0("GLMM (", var, ") : aucun coefficient d'effet fixe."), type = "warning")
            next
          }
          
          cn <- colnames(coef_table)
          stat_col <- if ("z value" %in% cn) "z value" else if ("t value" %in% cn) "t value" else cn[3]
          pval_col <- if ("Pr(>|z|)" %in% cn) "Pr(>|z|)" else if ("Pr(>|t|)" %in% cn) "Pr(>|t|)" else NA
          # lmerTest fournit une colonne 'df' (Satterthwaite) pour les LMM gaussiens.
          df_col   <- if ("df" %in% cn) "df" else NA
          
          for (i in seq_len(nrow(coef_table))) {
            label <- rownames(coef_table)[i]
            pval  <- if (!is.na(pval_col)) coef_table[i, pval_col] else NA_real_
            ddl_i <- if (!is.na(df_col))  round(coef_table[i, df_col], 1) else NA_real_
            results_list[[paste(var, label, sep = "_")]] <- data.frame(
              Test        = if (is_gaussian) "LMM" else "GLMM",
              Variable    = var,
              Facteur     = label,
              Statistique = round(coef_table[i, stat_col], 4),
              ddl         = ddl_i,
              p_value     = pval,
              Interpretation = if (i == 1) {
                paste0("Intercept : ", round(coef_table[i, "Estimate"], 4))
              } else if (!is.na(pval)) {
                interpret_test_results("glm", pval)
              } else {
                paste0("Effet aléatoire : ", rand_term)
              },
              stringsAsFactors = FALSE
            )
          }
          incProgress(0.6 / max(n_resp, 1))
        }
        
        if (length(results_list) > 0) {
          values$testResultsDF   <- do.call(rbind, results_list)
          values$currentModel    <- model_list[[length(model_list)]]
          values$modelList       <- model_list
          values$currentModelVar <- 1
          values$currentTestType <- "parametric"
          showNotification(
            trf("Modèle mixte ajusté (%s, famille %s) sur %d variable(s).",
                    engine, fam, length(model_list)),
            type = "message", duration = 5
          )
        } else {
          showNotification("Aucun résultat GLMM généré.", type = "warning")
        }
      }, error = function(e) {
        showNotification(hstat_err_fr(e, "Erreur modèle mixte"), type = "error", duration = 8)
      })
    })
  })
  
  
  #  ANOVA A MESURES REPETEES (parametrique) -- moteurs lmer (mixte) ou afex
  #  + post-hoc (emmeans, comparaisons par paires sur le facteur intra/inter)
  observeEvent(input$testRMAnova, {
    req(input$responseVar)
    df <- values$filteredData %||% values$cleanData %||% values$data
    if (is.null(df)) { showNotification("Aucune donnée disponible.", type = "error"); return() }
    
    subj   <- input$rmSubject
    within <- input$rmWithin
    between <- input$rmBetween
    engine <- input$rmEngine %||% "mixed"
    adj    <- input$rmPostHocAdjust %||% "holm"
    
    if (is.null(subj) || !nzchar(subj) || !subj %in% names(df)) {
      showNotification("Sélectionnez une variable « Sujet / identifiant » (panneau « Mesures répétées »).",
                       type = "warning", duration = 6); return()
    }
    if (is.null(within) || length(within) == 0) {
      showNotification("Sélectionnez au moins un facteur « Période » (intra-sujet).", type = "warning", duration = 6); return()
    }
    all_fac <- c(within, between)
    if (!all(all_fac %in% names(df))) {
      showNotification("Facteur(s) « Période » / « Traitement » introuvable(s) dans les données.", type = "error"); return()
    }
    if (engine == "mixed" && !requireNamespace("lmerTest", quietly = TRUE) &&
        !requireNamespace("lme4", quietly = TRUE)) {
      showNotification("Le package 'lme4'/'lmerTest' est requis pour le moteur mixte.", type = "error"); return()
    }
    if (engine == "afex" && !requireNamespace("afex", quietly = TRUE)) {
      showNotification("Le package 'afex' n'est pas installé (moteur afex).", type = "error"); return()
    }
    has_emm <- requireNamespace("emmeans", quietly = TRUE)
    
    results_list <- list()
    model_list   <- list()
    posthoc_list <- list()
    bt <- function(x) paste0("`", x, "`")
    
    withProgress(message = "ANOVA à mesures répétées...", value = 0.2, {
      tryCatch({
        # Facteurs en facteurs ; sujet en facteur.
        df[[subj]] <- factor(df[[subj]])
        for (f in all_fac) if (!is.factor(df[[f]])) df[[f]] <- factor(df[[f]])
        
        for (var in input$responseVar) {
          if (!is.numeric(df[[var]])) df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
          dsub <- df[, c(var, subj, all_fac), drop = FALSE]
          dsub <- dsub[stats::complete.cases(dsub), ]
          if (nrow(dsub) < 3) { showNotification(paste0("rmANOVA (", var, ") : trop peu de données."), type = "warning"); next }
          
          fixed <- paste(sapply(all_fac, bt), collapse = " * ")
          
          if (engine == "afex") {
            # afex::aov_ez : ANOVA classique avec correction de sphericite (GG).
            mod <- afex::aov_ez(id = subj, dv = var, data = dsub,
                                within = within,
                                between = if (length(between) > 0) between else NULL,
                                anova_table = list(correction = "GG"))
            atab <- as.data.frame(mod$anova_table)
            for (i in seq_len(nrow(atab))) {
              eff <- rownames(atab)[i]
              pcol <- if ("Pr(>F)" %in% colnames(atab)) "Pr(>F)" else tail(colnames(atab), 1)
              fcol <- if ("F" %in% colnames(atab)) "F" else colnames(atab)[grep("^F", colnames(atab))[1]]
              pval <- atab[i, pcol]
              results_list[[paste(var, eff, sep = "_")]] <- data.frame(
                Test = "rmANOVA (afex)", Variable = var, Facteur = eff,
                Statistique = round(atab[i, fcol], 4),
                ddl = if ("num Df" %in% colnames(atab)) paste0(round(atab[i, "num Df"], 1), ", ", round(atab[i, "den Df"], 1)) else NA,
                p_value = pval,
                Interpretation = interpret_test_results("anova", pval),
                stringsAsFactors = FALSE)
            }
            model_list[[var]] <- mod$aov %||% mod
            emm_model <- mod  # afex object marche avec emmeans
          } else {
            # Moteur mixte : var ~ within*between + (1|sujet). lmerTest -> p-values.
            form <- as.formula(paste0(bt(var), " ~ ", fixed, " + (1 | ", bt(subj), ")"))
            mod <- if (requireNamespace("lmerTest", quietly = TRUE)) lmerTest::lmer(form, data = dsub)
                   else lme4::lmer(form, data = dsub)
            # Table d'ANOVA de type III (Satterthwaite) si lmerTest.
            atab <- tryCatch(as.data.frame(anova(mod)), error = function(e) NULL)
            if (!is.null(atab) && nrow(atab) > 0) {
              for (i in seq_len(nrow(atab))) {
                eff <- rownames(atab)[i]
                pcol <- if ("Pr(>F)" %in% colnames(atab)) "Pr(>F)" else NA
                fcol <- if ("F value" %in% colnames(atab)) "F value" else if ("F" %in% colnames(atab)) "F" else NA
                pval <- if (!is.na(pcol)) atab[i, pcol] else NA_real_
                results_list[[paste(var, eff, sep = "_")]] <- data.frame(
                  Test = "rmANOVA (mixte)", Variable = var, Facteur = eff,
                  Statistique = if (!is.na(fcol)) round(atab[i, fcol], 4) else NA,
                  ddl = if ("NumDF" %in% colnames(atab)) paste0(round(atab[i, "NumDF"], 1), ", ", round(atab[i, "DenDF"], 1)) else NA,
                  p_value = pval,
                  Interpretation = if (!is.na(pval)) interpret_test_results("anova", pval) else "Effet (voir modèle)",
                  stringsAsFactors = FALSE)
              }
            }
            model_list[[var]] <- mod
            emm_model <- mod
          }
          incProgress(0.4 / max(length(input$responseVar), 1))
          
          # --- Post-hoc : comparaisons par paires (emmeans) sur chaque facteur ---
          if (has_emm) {
            for (f in all_fac) {
              ph <- tryCatch({
                emm <- emmeans::emmeans(emm_model, specs = f)
                pr  <- emmeans::contrast(emm, method = "pairwise", adjust = adj)
                as.data.frame(pr)
              }, error = function(e) NULL)
              if (!is.null(ph) && nrow(ph) > 0) {
                pcol <- if ("p.value" %in% colnames(ph)) "p.value" else tail(colnames(ph), 1)
                ecol <- if ("estimate" %in% colnames(ph)) "estimate" else colnames(ph)[2]
                role <- if (f %in% within) "Période" else "Traitement"
                for (i in seq_len(nrow(ph))) {
                  posthoc_list[[paste(var, f, i, sep = "_")]] <- data.frame(
                    Variable = var, Role = role, Facteur = f,
                    Comparaison = as.character(ph[[1]][i]),
                    Estimation = round(ph[i, ecol], 4),
                    p_value = round(ph[i, pcol], 5),
                    Significatif = if (!is.na(ph[i, pcol]) && ph[i, pcol] < 0.05) "Oui" else "n.s.",
                    stringsAsFactors = FALSE)
                }
              }
            }
          }
        }
        
        if (length(results_list) > 0) {
          values$testResultsDF   <- do.call(rbind, results_list)
          if (length(model_list) > 0) {
            values$currentModel    <- model_list[[length(model_list)]]
            values$modelList       <- model_list
            values$currentModelVar <- 1
            values$currentTestType <- "parametric"
          }
          values$rmPostHocData <- if (length(posthoc_list) > 0) do.call(rbind, posthoc_list) else NULL
          values$rmPostHocMethod <- paste0("rmANOVA (", engine, "), ajustement ", adj)
          showNotification(trf("ANOVA à mesures répétées (%s) sur %d variable(s).", engine, length(model_list)),
                           type = "message", duration = 5)
        } else {
          showNotification("Aucun résultat rmANOVA généré.", type = "warning")
        }
      }, error = function(e) {
        showNotification(hstat_err_fr(e, "Erreur rmANOVA"), type = "error", duration = 9)
      })
    })
  })
  
  #  NON PARAMETRIQUE A MESURES REPETEES : Friedman / Durbin / ART (+ post-hoc)
  observeEvent(input$testRMNonParam, {
    req(input$responseVar)
    df <- values$filteredData %||% values$cleanData %||% values$data
    if (is.null(df)) { showNotification("Aucune donnée disponible.", type = "error"); return() }
    
    subj   <- input$rmSubject
    within <- input$rmWithin
    between <- input$rmBetween
    method <- input$rmNonParam %||% "friedman"
    adj    <- input$rmPostHocAdjust %||% "holm"
    if (adj == "tukey") adj <- "holm"  # Tukey non applicable ici
    
    if (is.null(subj) || !nzchar(subj) || !subj %in% names(df)) {
      showNotification("Sélectionnez une variable « Sujet / identifiant ».", type = "warning", duration = 6); return()
    }
    if (is.null(within) || length(within) == 0) {
      showNotification("Sélectionnez au moins un facteur « Période » (intra-sujet).", type = "warning", duration = 6); return()
    }
    if (method %in% c("friedman", "durbin") && !requireNamespace("PMCMRplus", quietly = TRUE)) {
      showNotification("Le package 'PMCMRplus' est requis pour les post-hoc de Friedman/Durbin.", type = "warning")
    }
    if (method == "art" && !requireNamespace("ARTool", quietly = TRUE)) {
      showNotification("Le package 'ARTool' n'est pas installé (méthode ART).", type = "error"); return()
    }
    
    results_list <- list()
    posthoc_list <- list()
    bt <- function(x) paste0("`", x, "`")
    
    withProgress(message = "Test non paramétrique répété...", value = 0.3, {
      tryCatch({
        df[[subj]] <- factor(df[[subj]])
        for (f in c(within, between)) if (!is.factor(df[[f]])) df[[f]] <- factor(df[[f]])
        
        for (var in input$responseVar) {
          if (!is.numeric(df[[var]])) df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
          
          if (method == "art") {
            # ART : ANOVA sur rangs alignes, gere les plans factoriels intra/inter.
            all_fac <- c(within, between)
            dsub <- df[, c(var, subj, all_fac), drop = FALSE]
            dsub <- dsub[stats::complete.cases(dsub), ]
            fixed <- paste(sapply(all_fac, bt), collapse = " * ")
            form  <- as.formula(paste0(bt(var), " ~ ", fixed, " + (1 | ", bt(subj), ")"))
            m <- ARTool::art(form, data = dsub)
            atab <- as.data.frame(anova(m))
            for (i in seq_len(nrow(atab))) {
              eff  <- if ("Term" %in% colnames(atab)) atab[["Term"]][i] else rownames(atab)[i]
              pcol <- if ("Pr(>F)" %in% colnames(atab)) "Pr(>F)" else tail(colnames(atab), 1)
              fcol <- if ("F" %in% colnames(atab)) "F" else colnames(atab)[grep("^F", colnames(atab))[1]]
              pval <- atab[i, pcol]
              results_list[[paste(var, eff, sep = "_")]] <- data.frame(
                Test = "ART", Variable = var, Facteur = as.character(eff),
                Statistique = round(atab[i, fcol], 4),
                ddl = if ("Df" %in% colnames(atab)) atab[i, "Df"] else NA,
                p_value = pval,
                Interpretation = interpret_test_results("anova", pval),
                stringsAsFactors = FALSE)
            }
            # Post-hoc ART : comparaisons sur le 1er facteur intra (art.con).
            ph <- tryCatch(as.data.frame(ARTool::art.con(m, within[1], adjust = adj)),
                           error = function(e) NULL)
            if (!is.null(ph) && nrow(ph) > 0) {
              pcol <- if ("p.value" %in% colnames(ph)) "p.value" else tail(colnames(ph), 1)
              for (i in seq_len(nrow(ph))) {
                posthoc_list[[paste(var, "art", i, sep = "_")]] <- data.frame(
                  Variable = var, Role = "Période", Facteur = within[1],
                  Comparaison = as.character(ph[[1]][i]),
                  Estimation = NA,
                  p_value = round(ph[i, pcol], 5),
                  Significatif = if (!is.na(ph[i, pcol]) && ph[i, pcol] < 0.05) "Oui" else "n.s.",
                  stringsAsFactors = FALSE)
              }
            }
            
          } else {
            # Friedman / Durbin : un seul facteur intra. Mise en forme wide.
            wf <- within[1]
            dsub <- df[, c(var, subj, wf), drop = FALSE]
            dsub <- dsub[stats::complete.cases(dsub), ]
            y <- dsub[[var]]; g <- dsub[[wf]]; b <- dsub[[subj]]
            
            if (method == "durbin") {
              dt_res <- tryCatch(PMCMRplus::durbinTest(y = y, groups = g, blocks = b),
                                 error = function(e) NULL)
              stat <- if (!is.null(dt_res)) unname(dt_res$statistic) else NA
              pval <- if (!is.null(dt_res)) dt_res$p.value else NA
              ttl  <- "Durbin"
              ph_fun <- function() PMCMRplus::durbinAllPairsTest(y = y, groups = g, blocks = b, p.adjust.method = adj)
            } else {
              fr <- tryCatch(stats::friedman.test(y = y, groups = g, blocks = b), error = function(e) NULL)
              stat <- if (!is.null(fr)) unname(fr$statistic) else NA
              pval <- if (!is.null(fr)) fr$p.value else NA
              ttl  <- "Friedman"
              ph_fun <- function() PMCMRplus::frdAllPairsConoverTest(y = y, groups = g, blocks = b, p.adjust.method = adj)
            }
            results_list[[paste(var, ttl, sep = "_")]] <- data.frame(
              Test = ttl, Variable = var, Facteur = wf,
              Statistique = round(stat, 4), ddl = length(unique(g)) - 1,
              p_value = pval,
              Interpretation = if (!is.na(pval)) interpret_test_results("anova", pval) else "—",
              stringsAsFactors = FALSE)
            
            # Post-hoc par paires (Conover pour Friedman ; paires de Durbin).
            if (requireNamespace("PMCMRplus", quietly = TRUE) && !is.na(pval)) {
              pmat <- tryCatch(ph_fun()$p.value, error = function(e) NULL)
              if (!is.null(pmat)) {
                rn <- rownames(pmat); cn <- colnames(pmat)
                for (i in seq_len(nrow(pmat))) for (j in seq_len(ncol(pmat))) {
                  pv <- pmat[i, j]
                  if (!is.na(pv)) {
                    posthoc_list[[paste(var, i, j, sep = "_")]] <- data.frame(
                      Variable = var, Role = "Période", Facteur = wf,
                      Comparaison = paste0(rn[i], " vs ", cn[j]),
                      Estimation = NA, p_value = round(pv, 5),
                      Significatif = if (pv < 0.05) "Oui" else "n.s.",
                      stringsAsFactors = FALSE)
                  }
                }
              }
            }
          }
          incProgress(0.6 / max(length(input$responseVar), 1))
        }
        
        if (length(results_list) > 0) {
          values$testResultsDF   <- do.call(rbind, results_list)
          values$currentTestType <- "nonparametric"
          values$rmPostHocData <- if (length(posthoc_list) > 0) do.call(rbind, posthoc_list) else NULL
          values$rmPostHocMethod <- paste0(switch(method, friedman = "Friedman/Conover",
                                                  durbin = "Durbin", art = "ART"), ", ajustement ", adj)
          showNotification(trf("Test non paramétrique répété (%s) terminé.", method),
                           type = "message", duration = 5)
        } else {
          showNotification("Aucun résultat généré.", type = "warning")
        }
      }, error = function(e) {
        showNotification(hstat_err_fr(e, "Erreur non paramétrique répété"), type = "error", duration = 9)
      })
    })
  })
  

  observeEvent(input$testMANOVA, {
    req(input$responseVar, input$factorVar)
    
    if (length(input$responseVar) < 2) {
      showNotification("MANOVA nécessite au moins 2 variables réponses numériques.",
                       type = "warning", duration = 6)
      return()
    }
    
    tryCatch({
      chk <- check_manova_data(values$filteredData, input$responseVar, input$factorVar)
      if (!isTRUE(chk$ok)) {
        showNotification(chk$message, type = "error", duration = 8)
        return()
      }
      df_clean <- chk$df_clean
      
      rhs <- paste(sapply(input$factorVar, function(x) paste0("`", x, "`")),
                   collapse = ifelse(isTRUE(input$interaction), "*", "+"))
      lhs <- paste0("cbind(", paste(sapply(input$responseVar, function(x) paste0("`", x, "`")),
                                    collapse = ", "), ")")
      fml <- stats::as.formula(paste(lhs, "~", rhs))
      
      fit <- stats::manova(fml, data = df_clean)
      
      stats_df <- manova_format_all_stats(fit)
      stats_df <- manova_effect_sizes(stats_df, p = length(input$responseVar))
      stats_df$Interpretation <- mapply(interpret_manova_effect,
                                        stats_df$p_Pillai, stats_df$eta2_partial,
                                        USE.NAMES = FALSE)
      
      Y <- as.matrix(df_clean[, input$responseVar, drop = FALSE])
      mardia <- multivariate_normality_mardia(Y)
      values$manovaMardia <- data.frame(
        Test       = "Mardia (normalité multivariée)",
        n          = mardia$n,
        p          = mardia$p,
        Skewness   = mardia$skewness,
        p_Skewness = mardia$p.skewness,
        Kurtosis   = mardia$kurtosis,
        p_Kurtosis = mardia$p.kurtosis,
        Conclusion = mardia$conclusion,
        stringsAsFactors = FALSE
      )
      values$manovaBoxM     <- boxm_per_factor(Y, df_clean, input$factorVar)
      values$manovaPermDisp <- permdisp_per_factor(Y, df_clean, input$factorVar)
      
      rows <- lapply(seq_len(nrow(stats_df)), function(i) {
        data.frame(
          Test = "MANOVA (Pillai)",
          Variable = paste(input$responseVar, collapse = " + "),
          Facteur = stats_df$Effet[i],
          Statistique = round(stats_df$F_Pillai[i], 4),
          ddl = paste0(stats_df$ddl_num[i], ", ", stats_df$ddl_den[i]),
          p_value = stats_df$p_Pillai[i],
          Interpretation = stats_df$Interpretation[i],
          stringsAsFactors = FALSE
        )
      })
      # MANOVA s'affiche dans la box "Diagnostics multivariés", pas dans testResultsDF.
      values$manovaParamResults     <- stats_df
      values$manovaParamSummaryRows <- do.call(rbind, rows)
      values$manovaPermanovaResults <- NULL
      values$currentTestType        <- "manova"
      values$normalityResults       <- NULL
      values$homogeneityResults     <- NULL
      values$modelList              <- NULL
      
      showNotification(
        paste0("MANOVA terminée : ", nrow(stats_df), " effet(s) testé(s)."),
        type = "message", duration = 4
      )
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur MANOVA"), type = "error", duration = 10)
    })
  })
  
  observeEvent(input$testPERMANOVA, {
    req(input$responseVar, input$factorVar)
    
    if (length(input$responseVar) < 2) {
      showNotification("PERMANOVA nécessite au moins 2 variables réponses numériques.",
                       type = "warning", duration = 6)
      return()
    }
    
    showNotification("PERMANOVA en cours (999 permutations)...",
                     type = "message", duration = NULL, id = "permanovaProgress")
    
    tryCatch({
      chk <- check_manova_data(values$filteredData, input$responseVar, input$factorVar)
      if (!isTRUE(chk$ok)) {
        removeNotification("permanovaProgress")
        showNotification(chk$message, type = "error", duration = 8)
        return()
      }
      df_clean <- chk$df_clean
      
      dist_method <- "euclidean"
      nperm       <- 999L
      
      df_clean <- hstat_cap_df_rows(df_clean, what = "PERMANOVA")
      Y <- as.matrix(df_clean[, input$responseVar, drop = FALSE])
      
      rhs <- paste(sapply(input$factorVar, function(x) paste0("`", x, "`")),
                   collapse = ifelse(isTRUE(input$interaction), "*", "+"))
      
      d <- vegan::vegdist(Y, method = dist_method)
      fml <- stats::as.formula(paste("d ~", rhs))
      
      ad <- vegan::adonis2(fml, data = df_clean, permutations = nperm, by = "terms")
      tab <- as.data.frame(ad); tab$Effet <- rownames(ad)
      eff_idx <- which(!tab$Effet %in% c("Residual", "Total"))
      
      out <- do.call(rbind, lapply(eff_idx, function(i) {
        data.frame(
          Effet         = tab$Effet[i],
          ddl           = tab$Df[i],
          SS            = tab$SumOfSqs[i],
          R2            = tab$R2[i],
          F_pseudo      = tab$F[i],
          p_value       = tab$`Pr(>F)`[i],
          Permutations  = nperm,
          Distance      = dist_method,
          stringsAsFactors = FALSE
        )
      }))
      out$Interpretation <- mapply(interpret_permanova_effect, out$p_value, out$R2,
                                   USE.NAMES = FALSE)
      
      values$manovaPermDisp <- permdisp_per_factor(Y, df_clean, input$factorVar,
                                                   dist_method = dist_method)
      
      rows <- lapply(seq_len(nrow(out)), function(i) {
        data.frame(
          Test = paste0("PERMANOVA (", dist_method, ")"),
          Variable = paste(input$responseVar, collapse = " + "),
          Facteur = out$Effet[i],
          Statistique = round(out$F_pseudo[i], 4),
          ddl = out$ddl[i],
          p_value = out$p_value[i],
          Interpretation = paste0("R² = ", round(out$R2[i], 3), " | ", out$Interpretation[i]),
          stringsAsFactors = FALSE
        )
      })
      values$manovaPermanovaResults     <- out
      values$manovaPermanovaSummaryRows <- do.call(rbind, rows)
      values$manovaParamResults         <- NULL
      values$manovaMardia               <- NULL
      values$manovaBoxM             <- NULL
      values$currentTestType        <- "permanova"
      values$normalityResults       <- NULL
      values$homogeneityResults     <- NULL
      values$modelList              <- NULL
      
      removeNotification("permanovaProgress")
      showNotification(
        paste0("PERMANOVA terminée : ", nrow(out), " effet(s) (", nperm,
               " permutations, distance ", dist_method, ")."),
        type = "message", duration = 4
      )
    }, error = function(e) {
      removeNotification("permanovaProgress")
      showNotification(hstat_err_fr(e, "Erreur PERMANOVA"), type = "error", duration = 10)
    })
  })
  
  observeEvent(input$runManovaDiagnostic, {
    req(input$responseVar, input$factorVar)
    if (length(input$responseVar) < 2) {
      showNotification("Le diagnostic nécessite au moins 2 variables réponses.",
                       type = "warning", duration = 6)
      return()
    }
    tryCatch({
      chk <- check_manova_data(values$filteredData, input$responseVar, input$factorVar)
      if (!isTRUE(chk$ok)) {
        showNotification(chk$message, type = "error", duration = 8)
        return()
      }
      df_clean <- chk$df_clean
      Y <- as.matrix(df_clean[, input$responseVar, drop = FALSE])
      
      mardia <- multivariate_normality_mardia(Y)
      values$manovaMardia <- data.frame(
        Test       = "Mardia (normalité multivariée)",
        n          = mardia$n, p = mardia$p,
        Skewness   = mardia$skewness, p_Skewness = mardia$p.skewness,
        Kurtosis   = mardia$kurtosis, p_Kurtosis = mardia$p.kurtosis,
        Conclusion = mardia$conclusion, stringsAsFactors = FALSE
      )
      values$manovaBoxM     <- boxm_per_factor(Y, df_clean, input$factorVar)
      values$manovaPermDisp <- permdisp_per_factor(Y, df_clean, input$factorVar)
      
      outliers <- detect_multivariate_outliers(Y, alpha = 0.001)
      values$manovaOutliers <- list(
        n_outliers = outliers$n_outliers,
        idx        = outliers$idx_outliers,
        threshold  = outliers$threshold,
        conclusion = outliers$conclusion
      )
      
      rec <- recommend_manova_test(mardia, values$manovaBoxM, values$manovaPermDisp,
                                   n = chk$n)
      values$manovaRecommendation <- rec
      
      values$currentTestType <- "manova_diagnostic"
      
      showNotification(
        paste0("Diagnostic terminé. Test recommandé : ", rec$test_recommande,
               " (confiance : ", rec$niveau_confiance, ")."),
        type = "message", duration = 6
      )
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur diagnostic"), type = "error", duration = 10)
    })
  })
  
  observeEvent(input$runManovaSimpleEffects, {
    req(input$responseVar, input$factorVar, input$manovaSimpleFixed, input$manovaSimpleTested)
    if (length(input$factorVar) < 2) {
      showNotification("Les effets simples nécessitent au moins 2 facteurs.",
                       type = "warning"); return()
    }
    if (input$manovaSimpleFixed == input$manovaSimpleTested) {
      showNotification("Le facteur fixé et le facteur testé doivent être différents.",
                       type = "warning"); return()
    }
    tryCatch({
      chk <- check_manova_data(values$filteredData, input$responseVar,
                               c(input$manovaSimpleFixed, input$manovaSimpleTested))
      if (!isTRUE(chk$ok)) {
        showNotification(chk$message, type = "error"); return()
      }
      
      want_param <- isTRUE(values$currentTestType == "manova")
      res <- NULL
      used_fallback <- FALSE
      
      if (want_param) {
        res <- manova_simple_effects(chk$df_clean, input$responseVar,
                                     input$manovaSimpleFixed, input$manovaSimpleTested)
        # Si la MANOVA conditionnelle echoue (colinearite / rang deficient),
        # on bascule sur la PERMANOVA conditionnelle, plus robuste.
        if (is.null(res)) {
          res <- permanova_simple_effects(chk$df_clean, input$responseVar,
                                          input$manovaSimpleFixed, input$manovaSimpleTested,
                                          permutations = 999)
          used_fallback <- !is.null(res)
        }
      } else {
        res <- permanova_simple_effects(chk$df_clean, input$responseVar,
                                        input$manovaSimpleFixed, input$manovaSimpleTested,
                                        permutations = 999)
      }
      
      if (is.null(res)) {
        showNotification(paste0("Effets simples non calculables : sous-groupes trop petits ",
                                "ou variables réponses problématiques."),
                         type = "warning", duration = 7); return()
      }
      
      is_param_result <- want_param && !used_fallback
      res$Type_test <- if (is_param_result) "MANOVA conditionnelle"
      else "PERMANOVA conditionnelle"
      values$manovaSimpleEffects <- res
      
      if (used_fallback) {
        showNotification(paste0("La MANOVA conditionnelle n'est pas calculable ",
                                "(variables réponses colinéaires) : bascule automatique ",
                                "sur la PERMANOVA conditionnelle. ", nrow(res),
                                " niveau(x) testé(s)."),
                         type = "warning", duration = 8)
      } else {
        showNotification(paste0("Effets simples calculés : ", nrow(res),
                                " niveau(x) testé(s)."),
                         type = "message", duration = 4)
      }
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur effets simples"), type = "error")
    })
  })
  
  
  output$showManovaDiagnostics <- reactive({
    ctt <- values$currentTestType
    isTRUE(!is.null(ctt) && ctt %in% c("manova", "permanova"))
  })
  outputOptions(output, "showManovaDiagnostics", suspendWhenHidden = FALSE)
  
  output$hasManovaParam <- reactive({
    !is.null(values$manovaParamResults) && nrow(values$manovaParamResults) > 0
  })
  outputOptions(output, "hasManovaParam", suspendWhenHidden = FALSE)
  
  output$hasManovaPermanova <- reactive({
    !is.null(values$manovaPermanovaResults) && nrow(values$manovaPermanovaResults) > 0
  })
  outputOptions(output, "hasManovaPermanova", suspendWhenHidden = FALSE)
  
  output$hasManovaDispersion <- reactive({
    !is.null(values$manovaPermDisp) && nrow(values$manovaPermDisp) > 0
  })
  outputOptions(output, "hasManovaDispersion", suspendWhenHidden = FALSE)
  
  # Détail MANOVA paramétrique : 4 statistiques
  output$manovaParamTable <- renderDT({
    req(values$manovaParamResults)
    df <- values$manovaParamResults
    for (col in c("p_Pillai", "p_Wilks", "p_Hotelling", "p_Roy")) {
      if (col %in% names(df)) df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    dt <- datatable(df, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    
    # Colore les colonnes de la statistique recommandee par l'assistant
    rec <- values$manovaRecommendation
    if (!is.null(rec)) {
      stat_lbl <- rec$statistique_recommandee
      target <- if (grepl("Wilks", stat_lbl)) "Wilks"
      else if (grepl("Pillai", stat_lbl)) "Pillai"
      else if (grepl("Hotelling", stat_lbl)) "Hotelling"
      else if (grepl("Roy", stat_lbl)) "Roy"
      else NA
      if (!is.na(target)) {
        cols_to_color <- intersect(
          c(target, paste0("F_", target), paste0("p_", target)),
          names(df)
        )
        if (length(cols_to_color) > 0)
          dt <- dt %>% formatStyle(cols_to_color,
                                   backgroundColor = "#c8e6c9",
                                   fontWeight = "bold")
      }
    }
    dt
  })
  
  output$manovaPermanovaTable <- renderDT({
    req(values$manovaPermanovaResults)
    df <- values$manovaPermanovaResults
    if ("p_value" %in% names(df))
      df$p_value <- sapply(df$p_value, function(p) if (is.na(p)) NA else fmt_p(p))
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    datatable(df, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$manovaMardiaTable <- renderDT({
    req(values$manovaMardia)
    df <- values$manovaMardia
    for (col in c("p_Skewness", "p_Kurtosis")) {
      if (col %in% names(df)) df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    datatable(df, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  
  output$manovaMardiaInterpretation <- renderUI({
    req(values$manovaMardia)
    conc <- values$manovaMardia$Conclusion[1]
    color <- if (grepl("plausible", conc)) "#2e7d32" else "#c62828"
    bg    <- if (grepl("plausible", conc)) "#e8f5e9" else "#ffebee"
    div(style = paste0("padding:8px 12px; background:", bg,
                       "; border-left:4px solid ", color,
                       "; border-radius:4px; margin-top:6px; font-size:13px;"),
        icon("info-circle", style = paste0("color:", color, ";")),
        tags$b(" Conclusion Mardia : "), conc)
  })
  
  output$manovaBoxMTable <- renderDT({
    req(values$manovaBoxM)
    df <- values$manovaBoxM
    if ("p_value" %in% names(df))
      df$p_value <- sapply(df$p_value, function(p) if (is.na(p)) NA else fmt_p(p))
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    datatable(df, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  
  output$manovaBoxMInterpretation <- renderUI({
    req(values$manovaBoxM)
    conclusions <- values$manovaBoxM$Conclusion
    any_violation <- any(grepl("Violation", conclusions))
    any_singular  <- any(grepl("singuli|non applicable|trop petit|indisponible",
                               conclusions, ignore.case = TRUE))
    n_ok <- sum(grepl("respect|OK", conclusions, ignore.case = TRUE))
    
    if (any_violation) {
      color <- "#c62828"; bg <- "#fff3e0"
      msg <- "Au moins un facteur viole l'homogénéité des matrices de covariance -- privilégier Pillai (le plus robuste) ou la PERMANOVA."
    } else if (any_singular && n_ok == 0) {
      color <- "#e65100"; bg <- "#fff8e1"
      msg <- paste0("Le test de Box's M n'a pas pu être calculé : matrice de covariance ",
                    "singulière dans au moins un groupe (variables réponses colinéaires ou ",
                    "effectifs trop faibles). Utilisez la statistique de Pillai ou la PERMANOVA, ",
                    "qui ne dépendent pas de cette hypothèse.")
    } else if (any_singular) {
      color <- "#e65100"; bg <- "#fff8e1"
      msg <- paste0("Box's M calculable pour certains facteurs seulement (matrices ",
                    "singulières ailleurs). Interprétez avec prudence ; Pillai reste le choix sûr.")
    } else {
      color <- "#2e7d32"; bg <- "#e8f5e9"
      msg <- "Homogénéité des matrices de covariance respectée pour tous les facteurs."
    }
    div(style = paste0("padding:8px 12px; background:", bg,
                       "; border-left:4px solid ", color,
                       "; border-radius:4px; margin-top:6px; font-size:13px;"),
        icon("info-circle", style = paste0("color:", color, ";")),
        tags$b(" Conclusion Box's M : "), msg)
  })
  
  output$manovaPermDispTable <- renderDT({
    req(values$manovaPermDisp)
    df <- values$manovaPermDisp
    if ("p_value" %in% names(df))
      df$p_value <- sapply(df$p_value, function(p) if (is.na(p)) NA else fmt_p(p))
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    datatable(df, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  
  output$manovaPermDispInterpretation <- renderUI({
    req(values$manovaPermDisp)
    conclusions <- values$manovaPermDisp$Conclusion
    any_violation <- any(grepl("hétérogènes", conclusions))
    color <- if (!any_violation) "#2e7d32" else "#c62828"
    bg    <- if (!any_violation) "#e8f5e9" else "#fff3e0"
    msg <- if (!any_violation)
      "Dispersions multivariées homogènes -- analyse multivariée fiable."
    else
      "Dispersions hétérogènes -- la PERMANOVA peut confondre différences de localisation et de dispersion."
    div(style = paste0("padding:8px 12px; background:", bg,
                       "; border-left:4px solid ", color,
                       "; border-radius:4px; margin-top:6px; font-size:13px;"),
        icon("info-circle", style = paste0("color:", color, ";")),
        tags$b(" Conclusion PERMDISP : "), msg)
  })
  
  output$downloadManovaParam <- downloadHandler(
    filename = function() paste0("MANOVA_parametrique_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      if (!is.null(values$manovaParamResults)) {
        openxlsx::addWorksheet(wb, "MANOVA_stats")
        openxlsx::writeData(wb, "MANOVA_stats", values$manovaParamResults)
      }
      if (!is.null(values$manovaMardia)) {
        openxlsx::addWorksheet(wb, "Mardia"); openxlsx::writeData(wb, "Mardia", values$manovaMardia)
      }
      if (!is.null(values$manovaBoxM)) {
        openxlsx::addWorksheet(wb, "BoxM"); openxlsx::writeData(wb, "BoxM", values$manovaBoxM)
      }
      if (!is.null(values$manovaPermDisp)) {
        openxlsx::addWorksheet(wb, "PERMDISP"); openxlsx::writeData(wb, "PERMDISP", values$manovaPermDisp)
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadManovaPermanova <- downloadHandler(
    filename = function() paste0("PERMANOVA_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      if (!is.null(values$manovaPermanovaResults)) {
        openxlsx::addWorksheet(wb, "PERMANOVA"); openxlsx::writeData(wb, "PERMANOVA", values$manovaPermanovaResults)
      }
      if (!is.null(values$manovaPermDisp)) {
        openxlsx::addWorksheet(wb, "PERMDISP"); openxlsx::writeData(wb, "PERMDISP", values$manovaPermDisp)
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  output$showManovaWorkflow <- reactive({
    ctt <- values$currentTestType
    test_done <- isTRUE(!is.null(ctt) && ctt %in% c("manova", "permanova", "manova_diagnostic"))
    enough_vars <- length(input$responseVar) >= 2
    test_done || enough_vars
  })
  outputOptions(output, "showManovaWorkflow", suspendWhenHidden = FALSE)
  
  observeEvent(input$responseVar, {
    if (length(input$responseVar) >= 2) {
      session$sendCustomMessage("expandBox", "boxWrap_manovaAssist")
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Repli des boxes de l'assistant multivarie au demarrage (le conditionalPanel
  # peut empecher collapsed=TRUE de s'appliquer correctement au rendu initial).
  session$onFlushed(function() {
    session$sendCustomMessage("collapseBox", "boxWrap_manovaPlaceholder")
    session$sendCustomMessage("collapseBox", "boxWrap_manovaAssist")
  }, once = TRUE)
  
  output$hasManovaRecommendation <- reactive({
    !is.null(values$manovaRecommendation)
  })
  outputOptions(output, "hasManovaRecommendation", suspendWhenHidden = FALSE)
  
  output$hasManovaInteraction <- reactive({
    param_res <- values$manovaParamResults
    perm_res  <- values$manovaPermanovaResults
    check_df <- function(df, effet_col, p_col) {
      if (is.null(df)) return(FALSE)
      inter <- grepl(":", df[[effet_col]])
      any(inter) && any(df[[p_col]][inter] < 0.05, na.rm = TRUE)
    }
    check_df(param_res, "Effet", "p_Pillai") ||
      check_df(perm_res, "Effet", "p_value")
  })
  outputOptions(output, "hasManovaInteraction", suspendWhenHidden = FALSE)
  
  output$hasManovaSimpleEffects <- reactive({
    !is.null(values$manovaSimpleEffects) && nrow(values$manovaSimpleEffects) > 0
  })
  outputOptions(output, "hasManovaSimpleEffects", suspendWhenHidden = FALSE)
  
  output$hasManovaOutliers <- reactive({
    !is.null(values$manovaOutliers)
  })
  outputOptions(output, "hasManovaOutliers", suspendWhenHidden = FALSE)
  
  # Frise visuelle des etapes du workflow
  # Carte de recommandation du test
  output$manovaRecommendationCard <- renderUI({
    req(values$manovaRecommendation)
    rec <- values$manovaRecommendation
    
    bg_color <- switch(rec$niveau_confiance,
                       "élevée" = "#e8f5e9", "modérée" = "#fff8e1", "#ffebee")
    border_color <- switch(rec$niveau_confiance,
                           "élevée" = "#43a047", "modérée" = "#fb8c00", "#e53935")
    
    is_param <- grepl("MANOVA", rec$test_recommande) && !grepl("PERM", rec$test_recommande)

    # IMPORTANT : on NE recree PAS ici de boutons "testMANOVA"/"testPERMANOVA".
    # Ces identifiants existent deja dans le bloc "Tests parametriques" /
    # "Tests non-parametriques". Dupliquer l'inputId provoquait des declenchements
    # intempestifs (analyse lancee sans clic) et un triple affichage des boutons.
    # On se contente d'orienter l'utilisateur vers le bon bouton.
    reco_name <- if (is_param) "MANOVA (paramétrique)" else "PERMANOVA"
    reco_loc  <- if (is_param) "Tests paramétriques" else "Tests non-paramétriques"
    buttons <- div(
      style = "margin-top:12px; background:#eef7fb; border-left:4px solid #1b9fd0; padding:10px 14px; border-radius:0 6px 6px 0;",
      div(style = "font-size:13px; color:#2c3e50;",
        icon("hand-point-right"),
        " Pour exécuter le test recommandé, cliquez sur le bouton ",
        tags$b(reco_name),
        " dans la colonne « ", tags$b(reco_loc), " » des ",
        tags$b("Paramètres des tests"), " (en haut).")
    )
    
    div(style = paste0("background:", bg_color, "; border-left:6px solid ", border_color,
                       "; padding:18px 22px; border-radius:8px;"),
        div(style = "display:flex; align-items:center; gap:14px; margin-bottom:10px;",
            icon("magic", style = paste0("font-size:32px; color:", border_color, ";")),
            div(
              div(style = "font-size:11px; color:#555; letter-spacing:1px;", "RECOMMANDATION AUTOMATIQUE"),
              div(style = paste0("font-size:22px; font-weight:bold; color:", border_color, ";"),
                  rec$test_recommande),
              div(style = "font-size:13px; color:#555; margin-top:2px;",
                  "Statistique : ", strong(rec$statistique_recommandee),
                  " | Confiance : ", strong(rec$niveau_confiance))
            )
        ),
        div(style = "background:white; padding:12px 16px; border-radius:6px; margin-top:10px;",
            h5(icon("clipboard-list"), " Justification", style = "margin-top:0; color:#333;"),
            tags$ul(style = "margin-bottom:0; padding-left:18px;",
                    lapply(rec$justifications, function(j) tags$li(style = "margin:4px 0;", j)))
        ),
        if (length(rec$alertes) > 0) {
          div(style = "background:#fff3e0; border-left:4px solid #ff9800; padding:10px 14px; margin-top:10px; border-radius:4px;",
              icon("exclamation-triangle", style = "color:#e65100;"),
              tags$strong(" Points de vigilance :"),
              tags$ul(style = "margin:6px 0 0 18px;",
                      lapply(rec$alertes, function(a) tags$li(style = "color:#bf360c;", a))))
        } else NULL,
        buttons
    )
  })
  
  output$manovaOutliersCard <- renderUI({
    req(values$manovaOutliers)
    out <- values$manovaOutliers
    n_out <- out$n_outliers
    has_issue <- !is.null(n_out) && !is.na(n_out) && n_out > 0
    
    color <- if (has_issue) "#fb8c00" else "#43a047"
    bg    <- if (has_issue) "#fff3e0" else "#e8f5e9"
    iconame <- if (has_issue) "exclamation-circle" else "check-circle"
    
    div(style = paste0("background:", bg, "; border-left:4px solid ", color,
                       "; padding:12px 16px; border-radius:6px; margin-top:10px;"),
        icon(iconame, style = paste0("color:", color, ";")),
        strong(" Détection d'outliers multivariés : "),
        out$conclusion,
        if (has_issue && length(out$idx) > 0 && length(out$idx) <= 10) {
          div(style = "font-size:11px; color:#666; margin-top:4px;",
              "Index des lignes : ", paste(out$idx, collapse = ", "))
        } else NULL
    )
  })
  
  output$manovaInterpretationGuidance <- renderUI({
    param_res <- values$manovaParamResults
    perm_res  <- values$manovaPermanovaResults
    
    # Cas 1 : aucun test lancé, seulement le diagnostic
    if (is.null(param_res) && is.null(perm_res)) {
      return(div(style = "background:#e3f2fd; border-left:4px solid #1565C0; padding:14px 18px; border-radius:8px;",
                 icon("info-circle", style = "color:#1565C0;"),
                 strong(" En attente d'un test multivarié. "),
                 "Lancez une MANOVA ou une PERMANOVA depuis l'onglet ",
                 strong("'1. Diagnostic & recommandation'"),
                 " pour obtenir l'interprétation guidée de vos résultats."))
    }
    
    if (!is.null(param_res)) {
      effet_col <- "Effet"
      p_col     <- "p_Pillai"
      df        <- param_res
      test_lbl  <- "MANOVA paramétrique (statistique de Pillai)"
    } else {
      effet_col <- "Effet"
      p_col     <- "p_value"
      df        <- perm_res
      test_lbl  <- "PERMANOVA (pseudo-F par permutations)"
    }
    
    effets  <- df[[effet_col]]
    pvals   <- df[[p_col]]
    sig_idx   <- which(pvals < 0.05 & !is.na(pvals))
    insig_idx <- which(pvals >= 0.05 & !is.na(pvals))
    has_interaction <- any(grepl(":", effets[sig_idx]))
    
    msg_lines <- list()
    if (length(sig_idx) > 0) {
      msg_lines <- c(msg_lines, list(
        tags$li(style = "color:#2e7d32;",
                icon("check-circle"), " ",
                strong(paste0(length(sig_idx), " effet(s) significatif(s)")),
                " détecté(s) : ", paste(effets[sig_idx], collapse = ", "))
      ))
    }
    if (length(insig_idx) > 0) {
      msg_lines <- c(msg_lines, list(
        tags$li(style = "color:#757575;",
                icon("minus-circle"), " ",
                paste0(length(insig_idx), " effet(s) non significatif(s)"),
                " : ", paste(effets[insig_idx], collapse = ", "))
      ))
    }
    
    action_box <- if (has_interaction) {
      div(style = "background:#fff3e0; border-left:4px solid #fb8c00; padding:10px 14px; margin-top:10px; border-radius:4px;",
          icon("lightbulb", style = "color:#e65100;"),
          strong(" Action recommandée : "),
          "Une interaction est significative. Calculez les ",
          strong("effets simples"),
          " ci-dessous pour comprendre quel facteur agit dans quel contexte.")
    } else if (length(sig_idx) > 0) {
      div(style = "background:#e3f2fd; border-left:4px solid #1565C0; padding:10px 14px; margin-top:10px; border-radius:4px;",
          icon("lightbulb", style = "color:#0d47a1;"),
          strong(" Action recommandée : "),
          "Allez à la section ", strong("'PostHoc MANOVA/PERMANOVA'"),
          " (onglet Comparaisons multiples) pour identifier quels niveaux diffèrent (lettres de groupes).")
    } else {
      div(style = "background:#f5f5f5; border-left:4px solid #9e9e9e; padding:10px 14px; margin-top:10px; border-radius:4px;",
          icon("info-circle"),
          strong(" Aucun effet significatif. "),
          "Vérifiez la taille d'effet, l'effectif par groupe, et la pertinence des facteurs.")
    }
    
    div(style = "background:white; border:1px solid #cfd8dc; padding:14px 18px; border-radius:8px;",
        h5(icon("brain"), " Ce que vos résultats signifient",
           style = "margin-top:0; color:#1565C0;"),
        div(style = "font-size:12px; color:#777; margin-bottom:8px;",
            "Test analysé : ", strong(test_lbl)),
        tags$ul(style = "margin-bottom:0; padding-left:18px;", msg_lines),
        action_box
    )
  })
  
  # Selecteurs pour effets simples (un facteur "fixe", un facteur "teste")
  output$manovaSimpleEffectsSelectors <- renderUI({
    req(input$factorVar)
    if (length(input$factorVar) < 2) {
      return(div(style = "color:#888; padding:10px;",
                 icon("info-circle"), " Au moins 2 facteurs requis pour les effets simples."))
    }
    fluidRow(
      column(5, selectInput(ns("manovaSimpleFixed"),
                            tagList(icon("anchor"), " Facteur à fixer :"),
                            choices = input$factorVar, selected = input$factorVar[1])),
      column(5, selectInput(ns("manovaSimpleTested"),
                            tagList(icon("crosshairs"), " Facteur à tester :"),
                            choices = input$factorVar, selected = input$factorVar[2])),
      column(2, div(style = "padding-top:25px;",
                    actionButton(ns("runManovaSimpleEffects"),
                                 tagList(icon("play"), " Calculer"),
                                 class = "btn-info btn-block")))
    )
  })
  
  output$manovaSimpleEffectsTable <- renderDT({
    req(values$manovaSimpleEffects)
    df <- values$manovaSimpleEffects
    for (col in c("p_value", "p_adj")) {
      if (col %in% names(df)) df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    dt <- datatable(df, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    if ("Significatif" %in% names(df)) {
      dt <- dt %>% formatStyle("Significatif",
                               backgroundColor = styleEqual(c("Oui", "Non"),
                                                            c("#e8f5e9", "#f5f5f5")),
                               fontWeight = "bold")
    }
    dt
  })
  
  output$testResultsDF <- renderDT({
    req(values$testResultsDF)
    
    df  <- values$testResultsDF
    log <- values$transformationLog %||% list()
    
    # Colonne "Transformation" : indique la méthode si la variable est transformée
    df$Transformation <- sapply(df$Variable, function(v) {
      if (v %in% names(log)) log[[v]]$label else NA_character_
    })
    
    use_round <- !is.null(input$testsRoundResults) && input$testsRoundResults
    if (use_round) {
      dec      <- if (!is.null(input$testsDecimals)) input$testsDecimals else 2
      num_cols <- sapply(df, is.numeric)
      num_cols_safe <- num_cols & !names(df) %in% "p_value"
      df[, num_cols_safe] <- lapply(df[, num_cols_safe, drop = FALSE], function(x) round(x, dec))
    }
    
    # Formater p_value avec notation scientifique si très petite (évite l'arrondi à 0)
    if ("p_value" %in% names(df)) {
      df$p_value <- sapply(df$p_value, function(p) {
        if (is.na(p) || !is.numeric(p)) return(p)
        fmt_p(p)
      })
    }
    
    dt <- datatable(df,
                    options  = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    
    if (any(!is.na(df$Transformation))) {
      dt <- dt %>%
        formatStyle(
          "Transformation",
          target          = "row",
          backgroundColor = styleEqual(
            levels = unique(na.omit(df$Transformation)),
            values = rep("#fff8e1", length(unique(na.omit(df$Transformation))))
          )
        )
    }
    dt
  })
  
  
  output$chiSqCatVarSelect <- renderUI({
    req(values$filteredData)
    cat_cols <- names(values$filteredData)[sapply(values$filteredData, function(x)
      is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(na.omit(x))) <= 20))]
    pickerInput(ns("chiSqCatVar"), "Variable catégorielle (groupes) :",
                choices = cat_cols, multiple = FALSE,
                options = list(`live-search` = TRUE, `none-selected-text` = "Sélectionner..."))
  })
  
  output$chiSqFreqVarSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    pickerInput(ns("chiSqFreqVar"), "Variable de valeurs (fréq. ou %) :",
                choices = num_cols, multiple = FALSE,
                options = list(`live-search` = TRUE, `none-selected-text` = "Sélectionner..."))
  })
  
  output$chiSqExpectedInput <- renderUI({
    req(input$chiSqCatVar, values$filteredData)
    if (is.null(input$chiSqUniform) || isTRUE(input$chiSqUniform)) return(NULL)
    df   <- values$filteredData
    cats <- unique(na.omit(as.character(df[[input$chiSqCatVar]])))
    tagList(
      tags$p(tags$b("Probabilités attendues :"), tags$small(style="color:#777;"," (somme = 1)")),
      lapply(seq_along(cats), function(i)
        numericInput(paste0("chiSqP_", i), label = cats[i],
                     value = round(1/length(cats), 4), min = 0, max = 1, step = 0.001))
    )
  })
  
  observeEvent(input$testChiSq, {
    req(input$chiSqCatVar, input$chiSqFreqVar, values$filteredData)
    df      <- values$filteredData
    cat_var <- input$chiSqCatVar
    frq_var <- input$chiSqFreqVar
    dtype   <- input$chiSqDataType %||% "freq"
    cats    <- as.character(df[[cat_var]])
    vals    <- suppressWarnings(as.numeric(df[[frq_var]]))
    valid   <- !is.na(cats) & !is.na(vals)
    cats    <- cats[valid]; vals <- vals[valid]
    if (length(vals) < 2) { showNotification("Pas assez de données.", type="error"); return() }
    if (dtype == "pct") { pcts <- vals; obs <- round(vals / sum(vals) * 1000) } else {
      obs <- round(vals); pcts <- obs / sum(obs) * 100 }
    if (!is.null(input$chiSqUniform) && !input$chiSqUniform) {
      p_list <- lapply(seq_along(cats), function(i) input[[paste0("chiSqP_",i)]] %||% (1/length(cats)))
      p_exp  <- unlist(p_list)
      if (abs(sum(p_exp)-1) > 0.01) p_exp <- p_exp / sum(p_exp)
    } else { p_exp <- rep(1/length(obs), length(obs)) }
    tryCatch({
      tr <- chisq.test(obs, p = p_exp, rescale.p = TRUE)
      values$chiSqFreqData <- data.frame(
        Categorie    = cats, Observes = obs, Pct_obs = round(pcts, 2),
        Attendus     = round(as.numeric(tr$expected), 2),
        Residus_std  = round(as.numeric(tr$stdres), 4),
        Type_donnees = dtype, stringsAsFactors = FALSE)
      values$testResultsDF <- data.frame(
        Test = "Chi² (adéquation)", Variable = frq_var, Facteur = cat_var,
        Statistique = round(tr$statistic, 4), ddl = tr$parameter, p_value = tr$p.value,
        Interpretation = interpret_test_results("chisq.test", tr$p.value),
        stringsAsFactors = FALSE)
      values$chiSqResults   <- tr
      values$currentTestType <- "chisq"
      showNotification(paste0("Chi²=",round(tr$statistic,3)," p=",formatC(tr$p.value,"g",digits=4)),
                       type="message", duration=4)
    }, error = function(e) showNotification(hstat_err_fr(e, "Erreur Chi²"), type="error"))
  })
  
  observeEvent(input$testMultinomial, {
    req(input$chiSqCatVar, input$chiSqFreqVar, values$filteredData)
    df      <- values$filteredData
    cat_var <- input$chiSqCatVar; frq_var <- input$chiSqFreqVar
    dtype   <- input$chiSqDataType %||% "freq"
    cats    <- as.character(df[[cat_var]])
    vals    <- suppressWarnings(as.numeric(df[[frq_var]]))
    valid   <- !is.na(cats) & !is.na(vals)
    cats    <- cats[valid]; vals <- vals[valid]
    if (length(vals) < 2) { showNotification("Pas assez de données.", type="error"); return() }
    if (dtype == "pct") { pcts <- vals; obs <- round(vals/sum(vals)*1000) } else {
      obs <- round(vals); pcts <- obs/sum(obs)*100 }
    tryCatch({
      tr <- chisq.test(obs, p = rep(1/length(obs),length(obs)), simulate.p.value = TRUE, B = 10000)
      values$chiSqFreqData <- data.frame(
        Categorie = cats, Observes = obs, Pct_obs = round(pcts,2),
        Attendus  = round(as.numeric(tr$expected),2),
        Residus_std = round(as.numeric(tr$stdres),4),
        Type_donnees = dtype, stringsAsFactors = FALSE)
      values$testResultsDF <- data.frame(
        Test = "Multinomial (Monte Carlo)", Variable = frq_var, Facteur = cat_var,
        Statistique = round(tr$statistic,4), ddl = NA, p_value = tr$p.value,
        Interpretation = interpret_test_results("chisq.test", tr$p.value),
        stringsAsFactors = FALSE)
      values$chiSqResults    <- tr
      values$currentTestType  <- "chisq"
      showNotification(paste0("Multinomial p=",formatC(tr$p.value,"g",digits=4)),
                       type="message",duration=4)
    }, error = function(e) showNotification(hstat_err_fr(e, "Erreur Multinomial"),type="error"))
  })
  
  observeEvent(input$runChiSqPostHoc, {
    req(values$chiSqFreqData)
    chi_data   <- values$chiSqFreqData
    obs        <- chi_data$Observes
    cats       <- chi_data$Categorie
    n          <- length(obs)
    adj_method <- input$chiSqPostHocAdj %||% "bonferroni"
    if (n < 2) { showNotification("Trop peu de catégories.", type="warning"); return() }
    N_total <- sum(obs); p_raw <- c(); chi2_v <- c(); comps <- c()
    for (i in 1:(n-1)) for (j in (i+1):n) {
      m <- matrix(c(obs[i], obs[j], N_total-obs[i], N_total-obs[j]), 2)
      tryCatch({ t2 <- chisq.test(m, correct=(n==2))
      p_raw <<- c(p_raw, t2$p.value); chi2_v <<- c(chi2_v, round(t2$statistic,4))
      }, error = function(e) { p_raw <<- c(p_raw,NA); chi2_v <<- c(chi2_v,NA) })
      comps <- c(comps, paste0(cats[i]," — ",cats[j]))
    }
    p_adj <- p.adjust(p_raw, method = adj_method)
    values$chiSqPostHocData <- data.frame(
      Comparaison        = comps, Chi2 = chi2_v,
      p_brut             = round(p_raw,6), p_ajuste = round(p_adj,6),
      Significatif       = ifelse(!is.na(p_adj) & p_adj < 0.05,"OUI *","non"),
      Methode_correction = adj_method, stringsAsFactors = FALSE)
    p_mat <- matrix(1, n, n, dimnames = list(cats, cats)); k <- 1
    for (i in 1:(n-1)) for (j in (i+1):n) {
      p_mat[i,j] <- p_mat[j,i] <- p_adj[k]; k <- k+1 }
    diag(p_mat) <- 1
    tryCatch({
      gl <- multcompView::multcompLetters(p_mat, threshold = 0.05)$Letters
      chi_data$Groupes <- gl[match(cats, names(gl))]
      values$chiSqFreqData <- chi_data
    }, error = function(e) NULL)
    showNotification(paste0("Post-hoc chi² terminé (",adj_method,")"),type="message",duration=3)
  })
  
  output$chiSqPostHocTable <- renderDT({
    req(values$chiSqPostHocData)
    datatable(values$chiSqPostHocData, options=list(pageLength=15,scrollX=TRUE), rownames=FALSE) %>%
      formatStyle("Significatif", color=styleEqual(c("OUI *","non"),c("#c62828","#555")),
                  fontWeight=styleEqual(c("OUI *"),"bold"))
  })
  
  output$chiSqFreqTable <- renderDT({
    req(values$chiSqFreqData)
    datatable(values$chiSqFreqData, options=list(pageLength=20,scrollX=TRUE), rownames=FALSE)
  })
  
  output$chiSqPlot <- renderPlot({
    req(values$chiSqFreqData)
    chi_data  <- values$chiSqFreqData
    plot_type <- input$chiSqPlotType %||% "bar"
    show_vals <- isTRUE(input$chiSqShowValues)
    show_grps <- isTRUE(input$chiSqShowGroups) && "Groupes" %in% names(chi_data)
    show_pval <- isTRUE(input$chiSqShowPval)   && !is.null(values$chiSqResults)
    use_pct   <- isTRUE(input$chiSqShowPct) || chi_data$Type_donnees[1] == "pct"
    y_var     <- if (use_pct) "Pct_obs" else "Observés"
    y_lab     <- if (use_pct) "Pourcentage (%)" else "Fréquence observée"
    val_str   <- if (use_pct) paste0(round(chi_data$Pct_obs,1),"%") else as.character(chi_data$Observes)
    grp_str   <- if (show_grps) paste0("(",chi_data$Groupes,")") else ""
    chi_data$vlabel <- paste0(
      if(show_vals) val_str else "",
      if(show_vals && show_grps) "\n" else "",
      if(show_grps) grp_str else "")
    cap <- if (show_pval) {
      pv <- values$chiSqResults$p.value
      paste0("Chi²=",round(values$chiSqResults$statistic,3),
             "  p=",formatC(pv,"g",digits=4),
             if(pv<0.001)" ***" else if(pv<0.01)" **" else if(pv<0.05)" *" else " ns")
    } else ""
    base_t <- get_plot_theme()
    p <- if (plot_type == "bar") {
      gg <- ggplot(chi_data, aes(x=reorder(Categorie,-!!sym(y_var)), y=!!sym(y_var), fill=Categorie)) +
        geom_col(color="white",width=0.7,alpha=0.88) + scale_fill_brewer(palette="Set2",guide="none") +
        labs(x=NULL,y=y_lab,title="Distribution des catégories",caption=cap) + base_t
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(label=vlabel),vjust=-0.4,size=3.5,fontface="bold",color="#333")
      gg
    } else if (plot_type == "pie") {
      chi_data$frac <- chi_data[[y_var]] / sum(chi_data[[y_var]])
      chi_data$ypos <- cumsum(chi_data$frac) - 0.5*chi_data$frac
      gg <- ggplot(chi_data,aes(x="",y=frac,fill=Categorie)) +
        geom_bar(stat="identity",width=1,color="white") + coord_polar("y",start=0) +
        scale_fill_brewer(palette="Set2") +
        labs(title="Distribution des catégories",fill=NULL,caption=cap) + theme_void(base_size=12) +
        theme(legend.position="right",plot.title=element_text(hjust=0.5,face="bold"),
              plot.caption=element_text(hjust=0.5,color="#555",size=10))
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(y=ypos,label=vlabel),size=3.5,color="white",fontface="bold")
      gg
    } else if (plot_type == "lollipop") {
      gg <- ggplot(chi_data,aes(x=reorder(Categorie,-!!sym(y_var)),y=!!sym(y_var))) +
        geom_segment(aes(xend=reorder(Categorie,-!!sym(y_var)),yend=0),color="#b0bec5",linewidth=1.2) +
        geom_point(aes(color=Categorie),size=7,alpha=0.9) + scale_color_brewer(palette="Set2",guide="none") +
        labs(x=NULL,y=y_lab,title="Distribution des catégories",caption=cap) + base_t
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(label=vlabel),vjust=-1.3,size=3.5,fontface="bold",color="#333")
      gg
    } else if (plot_type == "dot") {
      gg <- ggplot(chi_data,aes(x=reorder(Categorie,!!sym(y_var)),y=!!sym(y_var),color=Categorie,size=!!sym(y_var))) +
        geom_point(alpha=0.85) + scale_color_brewer(palette="Set2",guide="none") +
        scale_size_continuous(range=c(4,14),guide="none") + coord_flip() +
        labs(x=NULL,y=y_lab,title="Distribution des catégories",caption=cap) + base_t
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(label=vlabel),hjust=-0.4,size=3.5,fontface="bold",color="#333")
      gg
    } else {
      gg <- ggplot(chi_data,aes(x=Categorie,y=!!sym(y_var),fill=Categorie)) +
        geom_bar(stat="identity",color="white",width=0.85) + scale_fill_brewer(palette="Set2",guide="none") +
        labs(x=NULL,y=y_lab,title="Distribution des catégories",caption=cap) + base_t
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(label=vlabel),vjust=-0.4,size=3.5,fontface="bold",color="#333")
      gg
    }
    values$chiSqPlotObj <- p; p
  })
  
  # NB : les handlers downloadChiSqExcel / downloadChiSqCSV / downloadChiSqPlot
  # sont definis plus bas (versions completes avec mise en forme). Une premiere
  # version existait ici : les output$ Shiny etant uniques, elle etait ecrasee
  # silencieusement -- code mort supprime.

  output$showValidation <- reactive({
    !is.null(values$normalityResults) || !is.null(values$homogeneityResults)
  })
  outputOptions(output, "showValidation", suspendWhenHidden = FALSE)
  
  output$showChiSqResults <- reactive({
    !is.null(values$chiSqFreqData)
  })
  outputOptions(output, "showChiSqResults", suspendWhenHidden = FALSE)
  
  # Graphique chi2 pour l'onglet PostHoc (même logique, inputs distincts)
  output$chiSqPlotMultiple <- renderPlot({
    req(values$chiSqFreqData)
    chi_data  <- values$chiSqFreqData
    plot_type <- input$chiSqPlotTypeMultiple %||% "bar"
    show_vals <- isTRUE(input$chiSqShowValM)
    show_grps <- isTRUE(input$chiSqShowGrpM)  && "Groupes" %in% names(chi_data)
    show_pval <- isTRUE(input$chiSqShowPvalM) && !is.null(values$chiSqResults)
    use_pct   <- isTRUE(input$chiSqShowPctM)  || chi_data$Type_donnees[1] == "pct"
    y_var     <- if (use_pct) "Pct_obs" else "Observés"
    y_lab     <- if (use_pct) "Pourcentage (%)" else "Fréquence observée"
    val_str   <- if (use_pct) paste0(round(chi_data$Pct_obs,1),"%") else as.character(chi_data$Observes)
    grp_str   <- if (show_grps) paste0("(",chi_data$Groupes,")") else ""
    chi_data$vlabel <- paste0(
      if(show_vals) val_str else "",
      if(show_vals && show_grps) "\n" else "",
      if(show_grps) grp_str else "")
    cap <- if (show_pval) {
      pv <- values$chiSqResults$p.value
      paste0("Chi²=",round(values$chiSqResults$statistic,3),
             "  p=",formatC(pv,"g",digits=4),
             if(pv<0.001)" ***" else if(pv<0.01)" **" else if(pv<0.05)" *" else " ns")
    } else ""
    base_t <- get_plot_theme()
    p <- if (plot_type == "bar") {
      gg <- ggplot(chi_data, aes(x=reorder(Categorie,-!!sym(y_var)), y=!!sym(y_var), fill=Categorie)) +
        geom_col(color="white",width=0.7,alpha=0.88) + scale_fill_brewer(palette="Set2",guide="none") +
        labs(x=NULL,y=y_lab,title="Distribution des catégories",caption=cap) + base_t
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(label=vlabel),vjust=-0.4,size=3.5,fontface="bold",color="#333")
      gg
    } else if (plot_type == "pie") {
      chi_data$frac <- chi_data[[y_var]] / sum(chi_data[[y_var]])
      chi_data$ypos <- cumsum(chi_data$frac) - 0.5*chi_data$frac
      gg <- ggplot(chi_data,aes(x="",y=frac,fill=Categorie)) +
        geom_bar(stat="identity",width=1,color="white") + coord_polar("y",start=0) +
        scale_fill_brewer(palette="Set2") +
        labs(title="Distribution des catégories",fill=NULL,caption=cap) + theme_void(base_size=12) +
        theme(legend.position="right",plot.title=element_text(hjust=0.5,face="bold"),
              plot.caption=element_text(hjust=0.5,color="#555",size=10))
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(y=ypos,label=vlabel),size=3.5,color="white",fontface="bold")
      gg
    } else if (plot_type == "lollipop") {
      gg <- ggplot(chi_data,aes(x=reorder(Categorie,-!!sym(y_var)),y=!!sym(y_var))) +
        geom_segment(aes(xend=reorder(Categorie,-!!sym(y_var)),yend=0),color="#b0bec5",linewidth=1.2) +
        geom_point(aes(color=Categorie),size=7,alpha=0.9) + scale_color_brewer(palette="Set2",guide="none") +
        labs(x=NULL,y=y_lab,title="Distribution des catégories",caption=cap) + base_t
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(label=vlabel),vjust=-1.3,size=3.5,fontface="bold",color="#333")
      gg
    } else if (plot_type == "dot") {
      gg <- ggplot(chi_data,aes(x=reorder(Categorie,!!sym(y_var)),y=!!sym(y_var),color=Categorie,size=!!sym(y_var))) +
        geom_point(alpha=0.85) + scale_color_brewer(palette="Set2",guide="none") +
        scale_size_continuous(range=c(4,14),guide="none") + coord_flip() +
        labs(x=NULL,y=y_lab,title="Distribution des catégories",caption=cap) + base_t
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(label=vlabel),hjust=-0.4,size=3.5,fontface="bold",color="#333")
      gg
    } else {
      gg <- ggplot(chi_data,aes(x=Categorie,y=!!sym(y_var),fill=Categorie)) +
        geom_bar(stat="identity",color="white",width=0.85) + scale_fill_brewer(palette="Set2",guide="none") +
        labs(x=NULL,y=y_lab,title="Distribution des catégories",caption=cap) + base_t
      if(any(nzchar(chi_data$vlabel))) gg <- gg+geom_text(aes(label=vlabel),vjust=-0.4,size=3.5,fontface="bold",color="#333")
      gg
    }
    values$chiSqPlotObj <- p; p
  })
  
  # runChiSqPostHoc2 : lien depuis l'onglet PostHoc (chiSqDataType2 + chiSqPostHocAdj2)
  observeEvent(input$runChiSqPostHoc2, {
    req(input$chiSqCatVar, input$chiSqFreqVar, values$filteredData)
    df      <- values$filteredData
    cat_var <- input$chiSqCatVar; frq_var <- input$chiSqFreqVar
    dtype   <- input$chiSqDataType2 %||% input$chiSqDataType %||% "freq"
    cats    <- as.character(df[[cat_var]])
    vals    <- suppressWarnings(as.numeric(df[[frq_var]]))
    valid   <- !is.na(cats) & !is.na(vals)
    cats    <- cats[valid]; vals <- vals[valid]
    if (length(vals) < 2) { showNotification("Pas assez de données.", type="error"); return() }
    if (dtype == "pct") { pcts <- vals; obs <- round(vals/sum(vals)*1000) } else {
      obs <- round(vals); pcts <- obs/sum(obs)*100 }
    tryCatch({
      p_exp <- rep(1/length(obs), length(obs))
      tr <- chisq.test(obs, p = p_exp, rescale.p = TRUE)
      values$chiSqFreqData <- data.frame(
        Categorie = cats, Observes = obs, Pct_obs = round(pcts,2),
        Attendus  = round(as.numeric(tr$expected),2),
        Residus_std = round(as.numeric(tr$stdres),4),
        Type_donnees = dtype, stringsAsFactors = FALSE)
      values$chiSqResults    <- tr
      values$currentTestType  <- "chisq"
      # Déclencher le post-hoc avec la méthode de l'onglet PostHoc
      adj_method <- input$chiSqPostHocAdj2 %||% "holm"
      chi_data   <- values$chiSqFreqData
      n <- length(obs); N_total <- sum(obs)
      p_raw <- c(); chi2_v <- c(); comps <- c()
      for (i in 1:(n-1)) for (j in (i+1):n) {
        m <- matrix(c(obs[i],obs[j],N_total-obs[i],N_total-obs[j]),2)
        tryCatch({ t2 <- chisq.test(m, correct=(n==2))
        p_raw <<- c(p_raw,t2$p.value); chi2_v <<- c(chi2_v,round(t2$statistic,4))
        }, error=function(e){ p_raw <<- c(p_raw,NA); chi2_v <<- c(chi2_v,NA) })
        comps <- c(comps, paste0(cats[i]," — ",cats[j]))
      }
      p_adj <- p.adjust(p_raw, method=adj_method)
      values$chiSqPostHocData <- data.frame(
        Comparaison=comps, Chi2=chi2_v, p_brut=round(p_raw,6),
        p_ajuste=round(p_adj,6),
        Significatif=ifelse(!is.na(p_adj)&p_adj<0.05,"OUI *","non"),
        Methode_correction=adj_method, stringsAsFactors=FALSE)
      p_mat <- matrix(1,n,n,dimnames=list(cats,cats)); k <- 1
      for (i in 1:(n-1)) for (j in (i+1):n) {
        p_mat[i,j] <- p_mat[j,i] <- p_adj[k]; k <- k+1 }
      diag(p_mat) <- 1
      tryCatch({
        gl <- multcompView::multcompLetters(p_mat, threshold=0.05)$Letters
        chi_data$Groupes <- gl[match(cats,names(gl))]
        values$chiSqFreqData <- chi_data
      }, error=function(e) NULL)
      showNotification(
        paste0("Chi² + Post-hoc terminés (", adj_method, ")"),
        type="message", duration=4)
    }, error=function(e) showNotification(hstat_err_fr(e, "Erreur"),type="error"))
  })
  
  output$showParametricDiagnostics <- reactive({
    !is.null(values$currentTestType) && values$currentTestType == "parametric" && !is.null(values$modelList)
  })
  outputOptions(output, "showParametricDiagnostics", suspendWhenHidden = FALSE)
  
  output$showValidationNavigation <- reactive({
    length(input$responseVar) > 1 && !is.null(values$normalityResults)
  })
  outputOptions(output, "showValidationNavigation", suspendWhenHidden = FALSE)
  
  output$validationNavigation <- renderUI({
    req(input$responseVar, length(input$responseVar) > 1)
    
    current_idx <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total_vars <- length(input$responseVar)
    
    div(style = "display: inline-block;",
        actionButton(ns("prevValidationVar"), "", icon = icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        span(paste("Variable", current_idx, "sur", total_vars, ":", input$responseVar[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        actionButton(ns("nextValidationVar"), "", icon = icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  output$showModelNavigation <- reactive({
    !is.null(values$modelList) && length(values$modelList) > 1
  })
  outputOptions(output, "showModelNavigation", suspendWhenHidden = FALSE)
  
  output$modelDiagNavigation <- renderUI({
    req(values$modelList, length(values$modelList) > 1)
    
    current_idx <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total_vars <- length(values$modelList)
    var_names <- names(values$modelList)
    
    div(style = "display: inline-block; margin-bottom: 15px;",
        actionButton(ns("prevModelVar"), "", icon = icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        span(paste("Modèle", current_idx, "sur", total_vars, ":", var_names[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        actionButton(ns("nextModelVar"), "", icon = icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  output$showResidNavigation <- reactive({
    !is.null(values$modelList) && length(values$modelList) > 1
  })
  outputOptions(output, "showResidNavigation", suspendWhenHidden = FALSE)
  
  output$residNavigation <- renderUI({
    req(values$modelList, length(values$modelList) > 1)
    
    current_idx <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total_vars <- length(values$modelList)
    var_names <- names(values$modelList)
    
    div(style = "display: inline-block; margin-bottom: 15px;",
        actionButton(ns("prevResidVar"), "", icon = icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        span(paste("Variable", current_idx, "sur", total_vars, ":", var_names[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        actionButton(ns("nextResidVar"), "", icon = icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  observeEvent(input$prevValidationVar, {
    current <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total <- length(input$responseVar)
    values$currentValidationVar <- if (current > 1) current - 1 else total
  })
  
  observeEvent(input$nextValidationVar, {
    current <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total <- length(input$responseVar)
    values$currentValidationVar <- if (current < total) current + 1 else 1
  })
  
  observeEvent(input$prevModelVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current > 1) current - 1 else total
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  observeEvent(input$nextModelVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current < total) current + 1 else 1
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  observeEvent(input$prevResidVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current > 1) current - 1 else total
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  observeEvent(input$nextResidVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current < total) current + 1 else 1
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  output$normalityResults <- renderPrint({
    req(values$normalityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    norm <- values$normalityResults[[current_var]]
    
    if (is.null(norm)) {
      cat("Aucun résultat de normalité disponible pour cette variable.\n")
    } else if ("group1" %in% names(norm)) {
      cat("Groupe 1 (", norm$group1_name, "): p = ", norm$group1$p.value, "\n")
      cat("Groupe 2 (", norm$group2_name, "): p = ", norm$group2$p.value, "\n")
    } else {
      cat("Résidus : p = ", norm$p.value, "\n")
    }
  })
  
  output$normalityInterpretation <- renderUI({
    req(values$normalityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    norm <- values$normalityResults[[current_var]]
    
    if (is.null(norm)) {
      interp_text <- "Aucun résultat de normalité disponible pour cette variable."
    } else if ("group1" %in% names(norm)) {
      interp1 <- interpret_normality(norm$group1$p.value)
      interp2 <- interpret_normality(norm$group2$p.value)
      interp_text <- paste0("Groupe 1: ", interp1, "<br>Groupe 2: ", interp2)
    } else {
      interp_text <- interpret_normality(norm$p.value)
    }
    HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>"))
  })
  
  output$homogeneityResults <- renderPrint({
    req(values$homogeneityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    hom <- values$homogeneityResults[[current_var]]
    
    if (is.null(hom)) {
      cat("Aucun résultat d'homogénéité disponible pour cette variable.\n")
    } else {
      cat("p = ", hom$`Pr(>F)`[1], "\n")
    }
  })
  
  output$homogeneityInterpretation <- renderUI({
    req(values$homogeneityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    hom <- values$homogeneityResults[[current_var]]
    
    if (is.null(hom)) {
      interp_text <- "Aucun résultat d'homogénéité disponible pour cette variable."
    } else {
      interp_text <- interpret_homogeneity(hom$`Pr(>F)`[1])
    }
    HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>"))
  })
  
  output$modelDiagnostics <- renderPlot({
    req(values$currentModel)
    
    tryCatch({
      # Vérifier si le modèle a des problèmes de leverage
      model <- values$currentModel
      h <- hatvalues(model)
      
      # Si tous les leverage sont 0 ou très proche de 0
      if (all(h < 1e-10) || sum(h > 0) < 3) {
        par(mfrow = c(1, 1))
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Ajustement parfait détecté\nLes diagnostics graphiques standards ne sont pas disponibles\nVoir les tests numériques ci-dessous", 
             cex = 1.2, col = "red")
        return()
      }
      
      par(mfrow = c(2, 2))
      plot(model, which = 1:4)
      
    }, error = function(e) {
      par(mfrow = c(1, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Erreur dans les diagnostics graphiques:\n", 
                       substr(e$message, 1, 100), 
                       "\n\nVoir les tests numériques ci-dessous"), 
           cex = 1, col = "red")
    })
  })
  
  output$modelDiagnosticsInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      model <- values$currentModel
      h <- hatvalues(model)
      
      if (all(h < 1e-10) || sum(h > 0) < 3) {
        interp_text <- "<span style='color: orange;'><strong>Ajustement parfait ou quasi-parfait détecté.</strong></span><br>
      Le modèle s'ajuste parfaitement aux données (leverage = 0 pour la plupart des observations).
      Cela peut indiquer :<br>
      - Nombre d'observations = nombre de paramètres<br>
      - Données avec structure particulière<br>
      - Surparamétrage du modèle<br>
      Les diagnostics graphiques standards ne sont pas fiables dans ce cas.<br>
      <strong>Recommandation :</strong> Vérifiez les tests numériques ci-dessous et considérez simplifier le modèle."
      } else {
        interp_text <- "Vérifiez les graphiques pour les violations des hypothèses :<br>
      - <strong>Residuals vs Fitted :</strong> Les résidus doivent être répartis aléatoirement autour de 0<br>
      - <strong>Normal Q-Q :</strong> Les points doivent suivre la ligne diagonale<br>
      - <strong>Scale-Location :</strong> La ligne rouge doit être approximativement horizontale<br>
      - <strong>Residuals vs Leverage :</strong> Identifie les points influents"
      }
      
      HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>"))
      
    }, error = function(e) {
      HTML("<div class='interprétation-box'><span style='color: red;'>Erreur dans l'interprétation des diagnostics</span></div>")
    })
  })
  
  output$downloadModelDiagnostics <- downloadHandler(
    filename = function() {
      paste0("diagnostics_modèle_", Sys.Date(), ".png")
    },
    content = function(file) {
      tryCatch({
        model <- values$currentModel
        h <- hatvalues(model)
        
        png(file, width = 3200, height = 2400, res = 300, type = "cairo")
        
        if (all(h < 1e-10) || sum(h > 0) < 3) {
          par(mfrow = c(1, 1))
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Ajustement parfait détecté\nLes diagnostics graphiques ne sont pas disponibles", 
               cex = 1.5, col = "red")
        } else {
          par(mfrow = c(2, 2))
          plot(model, which = 1:4)
        }
        
        dev.off()
      }, error = function(e) {
        png(file, width = 3200, height = 2400, res = 300, type = "cairo")
        par(mfrow = c(1, 1))
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste(strwrap(hstat_err_fr(e), 55), collapse = "\n"), cex = 0.85, col = "red")
        dev.off()
      })
    }
  )
  
  output$downloadQQPlot <- downloadHandler(
    filename = function() {
      paste0("qqplot_residus_", Sys.Date(), ".png")
    },
    content = function(file) {
      tryCatch({
        req(values$currentModel)
        residuals_data <- residuals(values$currentModel)
        residuals_data <- residuals_data[!is.na(residuals_data)]
        
        if (length(residuals_data) < 3 || sd(residuals_data) < 1e-10) {
          png(file, width = 2000, height = 1600, res = 300, type = "cairo-png")
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "QQ-plot non disponible\n(résidus constants ou insuffisants)", 
               cex = 1.2, col = "orange")
          dev.off()
          return()
        }
        
        n <- length(residuals_data)
        theoretical_quantiles <- qnorm(ppoints(n))
        sample_quantiles <- sort(residuals_data)
        
        se <- (sd(residuals_data) / sqrt(n)) * sqrt(theoretical_quantiles^2 + 1)
        upper_band <- theoretical_quantiles + 1.96 * se
        lower_band <- theoretical_quantiles - 1.96 * se
        
        df <- data.frame(
          theoretical = theoretical_quantiles,
          sample = sample_quantiles,
          upper = upper_band,
          lower = lower_band
        )
        
        p <- ggplot(df, aes(x = theoretical, y = sample)) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
          geom_point(shape = 1, size = 2) +
          theme_minimal() +
          labs(title = "QQ-plot des résidus", 
               x = "Quantiles théoriques", 
               y = "Quantiles observés") +
          theme(plot.title = element_markdown(hjust = 0.5))
        
        ggsave(file, plot = p, width = 10, height = 8, dpi = 300, type = "cairo-png")
        
      }, error = function(e) {
        png(file, width = 2000, height = 1600, res = 300, type = "cairo-png")
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste(strwrap(hstat_err_fr(e), 55), collapse = "\n"), cex = 0.85, col = "red")
        dev.off()
      })
    }
  )
  
  output$qqPlotResiduals <- renderPlot({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Pas assez de résidus pour le QQ-plot (n < 3)", cex = 1.2, col = "red")
        return()
      }
      
      if (sd(residuals_data) < 1e-10) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Résidus constants (ajustement parfait)\nQQ-plot non applicable", cex = 1.2, col = "orange")
        return()
      }
      
      n <- length(residuals_data)
      theoretical_quantiles <- qnorm(ppoints(n))
      sample_quantiles <- sort(residuals_data)
      
      se <- (sd(residuals_data) / sqrt(n)) * sqrt(theoretical_quantiles^2 + 1)
      upper_band <- theoretical_quantiles + 1.96 * se
      lower_band <- theoretical_quantiles - 1.96 * se
      
      df <- data.frame(
        theoretical = theoretical_quantiles,
        sample = sample_quantiles,
        upper = upper_band,
        lower = lower_band
      )
      
      ggplot(df, aes(x = theoretical, y = sample)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        geom_point(shape = 1, size = 2) +
        theme_minimal() +
        labs(title = "QQ-plot des résidus", 
             x = "Quantiles théoriques", 
             y = "Quantiles observés") +
        theme(plot.title = element_markdown(hjust = 0.5))
      
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste(strwrap(hstat_err_fr(e, "QQ-plot"), 55), collapse = "\n"), cex = 0.85, col = "red")
    })
  })
  
  output$qqPlotInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        interp_text <- "<span style='color: red;'>Pas assez de résidus pour évaluer la normalité.</span>"
      } else if (sd(residuals_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Résidus constants (ajustement parfait). Normalité non évaluable.</span>"
      } else {
        interp_text <- "Les points devraient suivre la ligne droite pour une normalité des résidus.<br>
      <strong>Déviations acceptables :</strong> Légères aux extrémités<br>
      <strong>Problèmes :</strong> Courbure prononcée, points très éloignés de la ligne"
      }
      
      HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>"))
      
    }, error = function(e) {
      HTML("<div class='interprétation-box'><span style='color: red;'>Erreur dans l'interprétation du QQ-plot</span></div>")
    })
  })
  
  output$normalityResult <- renderPrint({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        cat("Nombre d'observations insuffisant pour le test de Shapiro-Wilk (n < 3).\n")
      } else if (length(residuals_data) > 5000) {
        cat("Trop d'observations pour le test de Shapiro-Wilk (n > 5000).\n")
        cat("Utilisez le QQ-plot ci-dessus pour évaluer visuellement la normalité.\n")
      } else if (sd(residuals_data) < 1e-10) {
        cat("Résidus constants ou quasi-constants (ajustement parfait).\n")
        cat("Le test de normalité n'est pas applicable.\n")
      } else {
        hstat_shapiro(residuals_data)
      }
    }, error = function(e) {
      cat("Erreur dans le test de normalité:", e$message, "\n")
    })
  })
  
  output$normalityResidInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        interp_text <- "Nombre d'observations insuffisant pour le test de Shapiro-Wilk."
      } else if (length(residuals_data) > 5000) {
        interp_text <- "Trop d'observations pour Shapiro-Wilk. Référez-vous au QQ-plot."
      } else if (sd(residuals_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Résidus constants (ajustement parfait). Test non applicable.</span>"
      } else {
        norm_test <- hstat_shapiro(residuals_data)
        interp_text <- interpret_normality_resid(norm_test$p.value)
      }
      
      HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>"))
    }, error = function(e) {
      HTML("<div class='interprétation-box'><span style='color: red;'>Erreur dans le test de normalité</span></div>")
    })
  })
  
  output$leveneResidResult <- renderPrint({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      fitted_data <- fitted(values$currentModel)
      
      # Vérifier s'il y a une variation dans les valeurs ajustées
      if (sd(fitted_data) < 1e-10) {
        cat("Les valeurs ajustées sont constantes (ajustement parfait).\n")
        cat("Le test d'homogénéité des résidus n'est pas applicable.\n")
        return()
      }
      
      # Vérifier qu'on a assez de valeurs uniques pour cut()
      n_unique <- length(unique(fitted_data))
      if (n_unique < 2) {
        cat("Pas assez de valeurs uniques dans les prédictions.\n")
        cat("Le test d'homogénéité n'est pas applicable.\n")
        return()
      }
      
      fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
      
      if (length(levels(fitted_factor)) < 2 || any(table(fitted_factor) < 2)) {
        cat("Impossible de créer deux groupes équilibrés.\n")
        cat("Le test d'homogénéité n'est pas applicable.\n")
        return()
      }
      
      test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
      car::leveneTest(residuals ~ fitted_group, data = test_data)
      
    }, error = function(e) {
      cat("Erreur dans le test d'homogénéité:", e$message, "\n")
    })
  })
  
  output$homogeneityResidInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      fitted_data <- fitted(values$currentModel)
      
      if (sd(fitted_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Valeurs ajustées constantes (ajustement parfait). Test non applicable.</span>"
        return(HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>")))
      }
      
      n_unique <- length(unique(fitted_data))
      if (n_unique < 2) {
        interp_text <- "<span style='color: orange;'>Pas assez de variation dans les prédictions. Test non applicable.</span>"
        return(HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>")))
      }
      
      fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
      
      if (length(levels(fitted_factor)) < 2 || any(table(fitted_factor) < 2)) {
        interp_text <- "<span style='color: orange;'>Impossible de créer deux groupes équilibrés. Test non applicable.</span>"
        return(HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>")))
      }
      
      test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
      hom_test <- car::leveneTest(residuals ~ fitted_group, data = test_data)
      interp_text <- interpret_homogeneity_resid(hom_test$`Pr(>F)`[1])
      
      HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>"))
      
    }, error = function(e) {
      HTML("<div class='interprétation-box'><span style='color: red;'>Erreur dans le test d'homogénéité</span></div>")
    })
  })
  
  output$autocorrResult <- renderPrint({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      
      if (length(residuals_data) < 3) {
        cat("Nombre d'observations insuffisant pour le test de Durbin-Watson (n < 3).\n")
        return()
      }
      
      if (sd(residuals_data) < 1e-10) {
        cat("Résidus constants (ajustement parfait).\n")
        cat("Le test d'autocorrélation n'est pas applicable.\n")
        return()
      }
      
      lmtest::dwtest(values$currentModel)
      
    }, error = function(e) {
      cat("Erreur dans le test de Durbin-Watson:", e$message, "\n")
    })
  })
  
  output$autocorrInterpretation <- renderUI({
    req(values$currentModel)
    
    tryCatch({
      residuals_data <- residuals(values$currentModel)
      
      if (length(residuals_data) < 3) {
        interp_text <- "Nombre d'observations insuffisant pour le test de Durbin-Watson."
        return(HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>")))
      }
      
      if (sd(residuals_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Résidus constants (ajustement parfait). Test non applicable.</span>"
        return(HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>")))
      }
      
      dw_test <- lmtest::dwtest(values$currentModel)
      # Un modele degenere (residus constants) rend p = NA : on le dit au lieu
      # de laisser `if (p > 0.05)` faire tomber la sortie.
      interp_text <- switch(
        hstat_p_verdict(dw_test$p.value),
        "non significatif" = "Pas d'autocorrélation significative des résidus (p > 0.05).",
        "significatif" = "Autocorrélation significative des résidus (p < 0.05). Vérifiez l'indépendance des observations.",
        "Test de Durbin-Watson non calculable sur ce modèle (résidus dégénérés).")
      
      HTML(paste0("<div class='interprétation-box'>", interp_text, "</div>"))
      
    }, error = function(e) {
      HTML("<div class='interprétation-box'><span style='color: red;'>Erreur dans le test d'autocorrélation</span></div>")
    })
  })
  
  output$modelSummary <- renderPrint({
    req(values$currentModel)
    summary(values$currentModel)
  })
  
  output$downloadTestsExcel <- downloadHandler(
    filename = function() {
      paste0("résultats_tests_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "Résultats")
      openxlsx::writeData(wb, "Résultats", values$testResultsDF)
      
      if (!is.null(values$normalityResults)) {
        validation_df <- data.frame(Variable = names(values$normalityResults))
        validation_df$Normality_p <- sapply(values$normalityResults, function(x) x$p.value %||% NA)
        validation_df$Homogeneity_p <- sapply(values$homogeneityResults, function(x) x$`Pr(>F)`[1] %||% NA)
        
        openxlsx::addWorksheet(wb, "Validation")
        openxlsx::writeData(wb, "Validation", validation_df)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Module Chi² / Multinomial -- Tests du Khi², comparaisons paires, graphiques
  
  
  chi2_palette <- function(n) {
    cols <- c("#1565C0","#2E7D32","#C62828","#6A1B9A","#E65100",
              "#00695C","#AD1457","#4E342E","#37474F","#F9A825")
    if (n <= length(cols)) return(cols[seq_len(n)])
    colorRampPalette(cols)(n)
  }
  
  # Formater une p-valeur sans l'arrondir à 0 quand elle est très faible
  fmt_p <- function(p, digits = 6) {
    if (is.na(p)) return("NA")
    if (p == 0)   return("< 2.2e-16")
    if (p < 1e-4) return(formatC(p, format = "e", digits = 3))
    formatC(p, format = "f", digits = digits)
  }
  
  chi2_interp_p <- function(p) {
    if (is.na(p))   return("NA")
    if (p < 0.001)  return("Hautement significatif (p < 0.001)")
    if (p < 0.01)   return("Très significatif (p < 0.01)")
    if (p < 0.05)   return("Significatif (p < 0.05)")
    return("Non significatif (p >= 0.05)")
  }
  
  # Attribuer lettres de groupes (Bonferroni pairwise)
  chi2_group_letters <- function(modalites, observed, nb_paires, paires) {
    n  <- length(modalites)
    mat <- matrix(1, n, n, dimnames = list(modalites, modalites))
    
    res_paires <- data.frame()
    for (k in seq_len(nb_paires)) {
      i <- paires[1, k]; j <- paires[2, k]
      oi <- observed[i]; oj <- observed[j]
      if (is.na(oi) | is.na(oj) | oi < 0 | oj < 0 | (oi + oj) == 0) next
      tt  <- tryCatch(binom.test(oi, oi + oj, p = 0.5), error = function(e) NULL)
      if (is.null(tt)) next
      p_adj <- min(tt$p.value * nb_paires, 1)
      res_paires <- rbind(res_paires, data.frame(
        Groupe1      = modalites[i], Groupe2 = modalites[j],
        p_brute      = tt$p.value,
        p_Bonferroni = p_adj,
        Decision     = ifelse(p_adj < 0.05, "Différent", "Similaire"),
        stringsAsFactors = FALSE
      ))
      if (p_adj < 0.05) { mat[i, j] <- 0; mat[j, i] <- 0 }
    }
    
    grp <- rep(NA_character_, n); grp[1] <- "a"; cpt <- 1
    for (i in 2:n) {
      ok <- FALSE
      for (j in 1:(i-1)) {
        if (mat[i, j] == 1) { grp[i] <- grp[j]; ok <- TRUE; break }
      }
      if (!ok) { cpt <- cpt + 1; grp[i] <- letters[cpt] }
    }
    list(groupes = grp, paires = res_paires)
  }
  
  
  output$chiSqVarCatSelect <- renderUI({
    df <- values$filteredData %||% values$cleanData %||% values$data
    req(df)
    cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    if (length(cats) == 0) cats <- names(df)
    selectInput(ns("chiSqVarCat"), tagList(icon("tag"), " Variable catégorielle (modalités)"),
                choices = cats, selected = cats[1])
  })
  
  output$chiSqVarNumSelect <- renderUI({
    df <- values$filteredData %||% values$cleanData %||% values$data
    req(df)
    nums <- names(df)[sapply(df, is.numeric)]
    if (length(nums) == 0) {
      return(div(class = "alert alert-warning",
                 icon("exclamation-triangle"), " Aucune variable numérique disponible."))
    }
    selectInput(ns("chiSqVarNum"), tagList(icon("hashtag"), " Variable numérique (effectifs/proportions)"),
                choices = nums, selected = nums[1])
  })
  
  
  observeEvent(input$runChiSqTest, {
    df <- values$filteredData %||% values$cleanData %||% values$data
    req(df)
    
    if (is.null(input$factorVar) || length(input$factorVar) == 0 ||
        is.null(input$responseVar) || length(input$responseVar) == 0) {
      showNotification(
        "Veuillez sélectionner une variable réponse (numérique) et un facteur (catégoriel) dans 'Paramètres des tests'.",
        type = "warning", duration = 6
      )
      return()
    }
    
    col_cat  <- input$factorVar[1]
    col_num  <- input$responseVar[1]
    type_d   <- input$chiSqDataType  # "fréquences" ou "pourcentages"
    methode  <- input$chiSqMethod    # "chisq" ou "multinomial"
    
    if (!col_cat %in% names(df) || !col_num %in% names(df)) {
      showNotification("Variables introuvables dans les données.", type = "error"); return()
    }
    
    modalites_brutes <- as.character(df[[col_cat]])
    valeurs_brutes   <- as.numeric(df[[col_num]])
    
    ok_na <- !is.na(valeurs_brutes) & !is.na(modalites_brutes)
    if (any(!ok_na)) {
      showNotification("Valeurs manquantes détectées -- ignorées.", type = "warning")
    }
    modalites_brutes <- modalites_brutes[ok_na]
    valeurs_brutes   <- valeurs_brutes[ok_na]
    
    # Agreger par modalite : une seule valeur (somme) par categorie. Indispensable
    # car le test d'ajustement compare des effectifs PAR MODALITE, pas par ligne.
    # Sans cela, des donnees brutes (plusieurs lignes par categorie) generent
    # autant de "modalités" que de lignes -> combn() explose et l'app se fige.
    agg <- tapply(valeurs_brutes, modalites_brutes, sum)
    modalites    <- names(agg)
    valeurs_orig <- as.numeric(agg)
    
    n <- length(valeurs_orig)
    if (n < 2) { showNotification("Au moins 2 modalités requises.", type = "error"); return() }
    if (n > 100) {
      showNotification(
        trf("La variable '%s' a %d modalités distinctes -- trop pour un test d'ajustement. Vérifiez que vous avez bien choisi une variable catégorielle.", col_cat, n),
        type = "error", duration = 8
      )
      return()
    }
    
    if (type_d == "fréquences") {
      observed  <- as.integer(round(valeurs_orig))
      note_type <- "Fréquences (utilisées telles quelles)"
    } else {
      observed  <- as.integer(round(valeurs_orig))
      note_type <- "Pourcentages (utilisés directement)"
    }
    
    withProgress(message = "Test chi² en cours...", value = 0.3, {
      
      if (methode == "chisq") {
        res_test <- tryCatch(
          chisq.test(observed, p = rep(1/n, n)),
          warning = function(w) {
            showNotification(hstat_err_fr(w, "Avertissement"), type = "warning", duration = 10)
            suppressWarnings(chisq.test(observed, p = rep(1/n, n)))
          },
          error = function(e) { showNotification(hstat_err_fr(e, "Erreur chi²"), type = "error"); NULL }
        )
        if (is.null(res_test)) return()
        stat_name <- "Chi2"
        stat_val  <- round(res_test$statistic, 4)
        df_test   <- res_test$parameter
        p_val     <- res_test$p.value
        attendus  <- res_test$expected
      } else {
        res_test <- tryCatch(
          EMT::multinomial.test(observed, p = rep(1/n, n)),
          error = function(e) { showNotification(hstat_err_fr(e, "Erreur multinomial"), type = "error"); NULL }
        )
        if (is.null(res_test)) return()
        stat_name <- "Multinomial"
        stat_val  <- NA
        df_test   <- n - 1
        p_val     <- res_test$p.value
        attendus  <- observed / sum(observed) * sum(observed)  # proportions égales
      }
      
      incProgress(0.3)
      
      paires    <- combn(n, 2)
      nb_paires <- ncol(paires)
      ph        <- chi2_group_letters(modalites, observed, nb_paires, paires)
      
      incProgress(0.3)
      
      resume <- data.frame(
        Modalite         = modalites,
        Valeur_originale = valeurs_orig,
        Pct              = round(observed / sum(observed) * 100, 2),
        Valeur_test      = observed,
        Valeur_attendue  = round(attendus, 2),
        Residu_std       = round((observed - attendus) / sqrt(attendus), 3),
        Groupe           = ph$groupes,
        Type_donnee      = note_type,
        stringsAsFactors = FALSE
      )
      resume$Statut <- ifelse(resume$Residu_std > 1.96, "Sur-représenté",
                              ifelse(resume$Residu_std < -1.96, "Sous-représenté", "Conforme"))
      
      global_df <- data.frame(
        Test        = ifelse(methode == "chisq", "Chi² d'ajustement (chisq.test)", "Test multinomial exact (EMT)"),
        Statistique = paste0(stat_name, " = ", ifelse(is.na(stat_val), "—", stat_val)),
        DL          = df_test,
        p_valeur    = p_val,
        Interpretation = chi2_interp_p(p_val),
        Variable_cat = col_cat,
        Variable_num = col_num,
        Type_donnee  = note_type,
        stringsAsFactors = FALSE
      )
      
      values$chiSqResults    <- global_df
      values$chiSqFreqData   <- resume
      values$chiSqPostHocData <- ph$paires
      values$chiSqRawObs     <- observed
      values$chiSqModalites  <- modalites
      values$chiSqValeursOrig <- valeurs_orig
      values$chiSqTypeDonnee  <- type_d
      values$chiSqPGlobal     <- p_val
      
      # - Ajouter dans les Résultats des tests (tableau principal) -
      chi_row <- data.frame(
        Test           = global_df$Test[1],
        Variable       = col_num,
        Facteur        = col_cat,
        Statistique    = ifelse(is.na(stat_val), NA_real_, as.numeric(stat_val)),
        ddl            = df_test,
        p_value        = p_val,
        Interpretation = chi2_interp_p(p_val),
        stringsAsFactors = FALSE
      )
      prev <- values$testResultsDF
      if (!is.null(prev) && nrow(prev) > 0) {
        # Retirer les éventuelles lignes Chi²/Multinomial précédentes pour éviter les doublons
        prev <- prev[!grepl("Chi²|Multinomial", prev$Test, ignore.case = TRUE), , drop = FALSE]
        for (col in setdiff(names(prev), names(chi_row))) chi_row[[col]] <- NA
        for (col in setdiff(names(chi_row), names(prev))) prev[[col]]    <- NA
        values$testResultsDF <- rbind(prev, chi_row)
      } else {
        values$testResultsDF <- chi_row
      }
    })
    
    showNotification(
      paste0("Test ", toupper(methode), " terminé -- p = ", fmt_p(p_val)),
      type = if (p_val < 0.05) "message" else "warning",
      duration = 5
    )
    
    updateTabsetPanel(session, "chiSqResultsTabs", selected = "chiSq_résumé")
  })
  
  
  output$chiSqGlobalResult <- renderDT({
    req(values$chiSqResults)
    df_display <- values$chiSqResults
    # Formater la p-valeur pour l'affichage (éviter l'arrondi à 0)
    df_display$p_valeur <- sapply(df_display$p_valeur, fmt_p)
    datatable(df_display,
              rownames = FALSE, options = list(dom = "t", scrollX = TRUE),
              class = "table-bordered table-striped")
  })
  
  output$chiSqResumeTable <- renderDT({
    req(values$chiSqFreqData)
    dt <- values$chiSqFreqData
    datatable(dt, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
              class = "table-bordered table-striped") |>
      formatStyle("Groupe", fontWeight = "bold", color = "#1565C0", fontSize = "14px") |>
      formatStyle("Statut",
                  backgroundColor = styleEqual(
                    c("Sur-représenté", "Sous-représenté", "Conforme"),
                    c("#C8E6C9",        "#FFCDD2",          "#FFF9C4")))
  })
  
  output$chiSqPairesTable <- renderDT({
    req(values$chiSqPostHocData)
    df_p <- values$chiSqPostHocData
    if ("p_brute"      %in% names(df_p)) df_p$p_brute      <- sapply(df_p$p_brute,      fmt_p)
    if ("p_Bonferroni" %in% names(df_p)) df_p$p_Bonferroni <- sapply(df_p$p_Bonferroni, fmt_p)
    datatable(df_p, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE, dom = "tip"),
              class = "table-bordered table-striped") |>
      formatStyle("Decision",
                  backgroundColor = styleEqual(c("Différent", "Similaire"), c("#C8E6C9", "#FFCDD2")),
                  fontWeight = "bold")
  })
  
  output$chiSqInterpretation <- renderUI({
    req(values$chiSqResults, values$chiSqFreqData)
    gdf   <- values$chiSqResults
    p_val <- gdf$p_valeur[1]
    fdf   <- values$chiSqFreqData
    n_grp <- length(unique(fdf$Groupe))
    
    sig_color <- if (p_val < 0.05) "#1b5e20" else "#b71c1c"
    sig_bg    <- if (p_val < 0.05) "#e8f5e9"  else "#ffebee"
    
    tagList(
      div(style = paste0("background:", sig_bg, "; border-left: 5px solid ", sig_color,
                         "; padding: 15px; border-radius: 6px; margin-bottom: 10px;"),
          h4(icon("microscope"), " Interprétation globale", style = paste0("color:", sig_color, ";")),
          p(strong(gdf$Interpretation[1])),
          p(paste0("p = ", fmt_p(p_val), " | Test : ", gdf$Test[1])),
          p(paste0("Variable : ", gdf$Variable_cat[1], " | Données : ", gdf$Type_donnee[1]))
      ),
      div(style = "background: #e3f2fd; border-left: 5px solid #1565C0; padding: 12px; border-radius: 6px;",
          h5(icon("layer-group"), " Groupes identifiés", style = "color: #1565C0;"),
          p(paste0(n_grp, " groupe(s) distinct(s) → ",
                   paste(sort(unique(fdf$Groupe)), collapse = ", "))),
          p("Même lettre = pas de différence significative après correction Bonferroni.")
      )
    )
  })
  
  
  creer_graphique_chi2 <- reactive({
    req(values$chiSqFreqData)
    fdf       <- values$chiSqFreqData
    type_g    <- input$chiSqGraphType    %||% "bar_v"
    palette_g <- input$chiSqPalette      %||% "default"
    p_val     <- values$chiSqPGlobal     %||% NA
    
    show_grp  <- isTRUE(input$chiSqShowGroupes)
    show_val  <- isTRUE(input$chiSqShowValeurs)
    show_pval <- isTRUE(input$chiSqShowPval)
    
    n   <- nrow(fdf)
    pal <- switch(palette_g,
                  "default" = chi2_palette(n),
                  "Set1"    = RColorBrewer::brewer.pal(max(3, n), "Set1")[seq_len(n)],
                  "Set2"    = RColorBrewer::brewer.pal(max(3, n), "Set2")[seq_len(n)],
                  "Dark2"   = RColorBrewer::brewer.pal(max(3, n), "Dark2")[seq_len(n)],
                  "Pastel1" = RColorBrewer::brewer.pal(max(3, n), "Pastel1")[seq_len(n)],
                  chi2_palette(n)
    )
    if (n > length(pal)) pal <- colorRampPalette(pal)(n)
    
    df_plot <- data.frame(
      modalite = factor(fdf$Modalite, levels = fdf$Modalite),
      valeur   = fdf$Valeur_originale,
      groupe   = fdf$Groupe,
      stringsAsFactors = FALSE
    )
    
    sous_titre <- if (show_pval && !is.na(p_val))
      paste0("p = ", fmt_p(p_val)) else NULL
    
    lbl_fn <- function(val, grp) {
      s <- round(val, 2)
      if (show_grp) s <- paste0(s, "\n(", grp, ")")
      s
    }
    
    if (type_g == "bar_v") {
      g <- ggplot(df_plot, aes(x = modalite, y = valeur, fill = modalite)) +
        geom_col(width = 0.65, color = "white", linewidth = 0.4) +
        scale_fill_manual(values = pal) +
        labs(subtitle = sous_titre, x = NULL, y = "Valeur") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
      if (show_val)
        g <- g + geom_text(aes(label = lbl_fn(valeur, groupe)), vjust = -0.4,
                           size = 4, fontface = "bold")
      
    } else if (type_g == "bar_h") {
      g <- ggplot(df_plot, aes(x = valeur, y = reorder(modalite, valeur), fill = modalite)) +
        geom_col(width = 0.65, color = "white") +
        scale_fill_manual(values = pal) +
        labs(subtitle = sous_titre, x = "Valeur", y = NULL) +
        theme_minimal(base_size = 13) + theme(legend.position = "none")
      if (show_val)
        g <- g + geom_text(aes(label = lbl_fn(valeur, groupe)), hjust = -0.1,
                           size = 4, fontface = "bold")
      
    } else if (type_g == "pie") {
      df_plot$pct <- df_plot$valeur / sum(df_plot$valeur) * 100
      lbl_pie <- paste0(df_plot$modalite,
                        if (show_val)  paste0("\n", round(df_plot$pct, 1), "%") else "",
                        if (show_grp) paste0("\n(", df_plot$groupe, ")") else "")
      g <- ggplot(df_plot, aes(x = "", y = valeur, fill = modalite)) +
        geom_col(width = 1, color = "white") + coord_polar("y") +
        geom_text(aes(label = lbl_pie), position = position_stack(vjust = 0.5),
                  size = 4, fontface = "bold") +
        scale_fill_manual(values = pal, name = NULL) +
        labs(subtitle = sous_titre) +
        theme_void(base_size = 13) + theme(plot.subtitle = element_text(hjust = 0.5))
      
    } else if (type_g == "donut") {
      df_plot$pct  <- df_plot$valeur / sum(df_plot$valeur) * 100
      df_plot$ymax <- cumsum(df_plot$pct)
      df_plot$ymin <- c(0, head(df_plot$ymax, -1))
      df_plot$mid  <- (df_plot$ymin + df_plot$ymax) / 2
      g <- ggplot(df_plot, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = modalite)) +
        geom_rect(color = "white", linewidth = 0.5) +
        coord_polar(theta = "y") + xlim(0, 4) +
        scale_fill_manual(values = pal, name = NULL) +
        labs(subtitle = sous_titre) +
        theme_void(base_size = 13) + theme(plot.subtitle = element_text(hjust = 0.5))
      if (show_val)
        g <- g + geom_text(aes(x = 3.25, y = mid,
                               label = paste0(round(pct, 1), "%",
                                              if (show_grp) paste0("\n(", groupe, ")") else "")),
                           size = 3.5, fontface = "bold")
      
    } else if (type_g == "lollipop") {
      g <- ggplot(df_plot, aes(x = reorder(modalite, -valeur), y = valeur, color = modalite)) +
        geom_segment(aes(xend = modalite, yend = 0), linewidth = 1.5) +
        geom_point(size = 7) +
        scale_color_manual(values = pal) +
        labs(subtitle = sous_titre, x = NULL, y = "Valeur") +
        theme_minimal(base_size = 13) + theme(legend.position = "none")
      if (show_val)
        g <- g + geom_text(aes(label = lbl_fn(valeur, groupe)),
                           vjust = -1.2, size = 4, fontface = "bold")
      
    } else if (type_g == "residus") {
      df_r <- data.frame(
        modalite = factor(fdf$Modalite, levels = fdf$Modalite),
        residu   = fdf$Residu_std,
        couleur  = ifelse(fdf$Residu_std > 1.96, "Sur-représenté",
                          ifelse(fdf$Residu_std < -1.96, "Sous-représenté", "Conforme"))
      )
      g <- ggplot(df_r, aes(x = modalite, y = residu, fill = couleur)) +
        geom_col(color = "white", width = 0.65) +
        geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
        scale_fill_manual(values = c("Sur-représenté" = "#1565C0",
                                     "Sous-représenté" = "#C62828",
                                     "Conforme"        = "#9E9E9E")) +
        labs(subtitle = sous_titre, x = NULL, y = "Résidu standardisé", fill = "Statut") +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
      if (show_grp)
        g <- g + geom_text(aes(label = paste0("(", fdf$Groupe, ")")),
                           vjust = ifelse(df_r$residu >= 0, -0.5, 1.2),
                           size = 4, fontface = "bold", color = "black")
      
    } else {  # histogramme classique
      g <- ggplot(df_plot, aes(x = modalite, y = valeur, fill = modalite)) +
        geom_col(width = 1, color = "white", linewidth = 0.3) +
        scale_fill_manual(values = pal) +
        labs(subtitle = sous_titre, x = NULL, y = "Valeur") +
        theme_classic(base_size = 13) + theme(legend.position = "none")
      if (show_val)
        g <- g + geom_text(aes(label = lbl_fn(valeur, groupe)), vjust = -0.3,
                           size = 4, fontface = "bold")
    }
    
    titre <- if (!is.null(input$chiSqGraphTitle) && nzchar(input$chiSqGraphTitle))
      input$chiSqGraphTitle else
        paste0("Distribution -- ", values$chiSqResults$Variable_cat[1])
    
    g <- g + labs(title = titre) +
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
            plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#555"))
    
    values$chiSqPlotObj <- g
    g
  })
  
  output$chiSqGraph <- renderPlot({
    creer_graphique_chi2()
  }, height = function() input$chiSqGraphHeight %||% 500)
  
  
  output$downloadChiSqPlot <- downloadHandler(
    filename = function() paste0("chi2_graphique_", Sys.Date(), ".png"),
    content  = function(file) {
      req(values$chiSqPlotObj)
      w <- (input$chiSqGraphWidth  %||% 800) / 96
      h <- (input$chiSqGraphHeight %||% 500) / 96
      ggsave(file, plot = creer_graphique_chi2(), width = w, height = h,
             dpi = input$chiSqGraphDPI %||% 150, bg = "white")
    }
  )
  
  output$downloadChiSqExcel <- downloadHandler(
    filename = function() paste0("chi2_résultats_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(values$chiSqResults)
      wb <- openxlsx::createWorkbook()
      
      h_style <- openxlsx::createStyle(
        fontColour = "#FFFFFF", fgFill = "#1565C0",
        halign = "CENTER", textDecoration = "Bold",
        border = "TopBottomLeftRight"
      )
      sig_s <- openxlsx::createStyle(fgFill = "#C8E6C9")
      ns_s  <- openxlsx::createStyle(fgFill = "#FFCDD2")
      
      openxlsx::addWorksheet(wb, "Résultat global")
      openxlsx::writeData(wb, "Résultat global", values$chiSqResults, headerStyle = h_style)
      openxlsx::setColWidths(wb, "Résultat global", cols = 1:ncol(values$chiSqResults), widths = "auto")
      
      openxlsx::addWorksheet(wb, "Modalités et groupes")
      openxlsx::writeData(wb, "Modalités et groupes", values$chiSqFreqData, headerStyle = h_style)
      sr <- which(values$chiSqFreqData$Statut == "Sur-représenté")  + 1
      nr <- which(values$chiSqFreqData$Statut == "Sous-représenté") + 1
      if (length(sr) > 0) openxlsx::addStyle(wb, "Modalités et groupes", sig_s, rows = sr,
                                             cols = 1:ncol(values$chiSqFreqData), gridExpand = TRUE)
      if (length(nr) > 0) openxlsx::addStyle(wb, "Modalités et groupes", ns_s, rows = nr,
                                             cols = 1:ncol(values$chiSqFreqData), gridExpand = TRUE)
      openxlsx::setColWidths(wb, "Modalités et groupes", cols = 1:ncol(values$chiSqFreqData), widths = "auto")
      
      if (!is.null(values$chiSqPostHocData) && nrow(values$chiSqPostHocData) > 0) {
        openxlsx::addWorksheet(wb, "Comparaisons paires")
        openxlsx::writeData(wb, "Comparaisons paires", values$chiSqPostHocData, headerStyle = h_style)
        dr <- which(values$chiSqPostHocData$Decision == "Différent") + 1
        mr <- which(values$chiSqPostHocData$Decision == "Similaire") + 1
        if (length(dr) > 0) openxlsx::addStyle(wb, "Comparaisons paires", sig_s, rows = dr,
                                               cols = 1:5, gridExpand = TRUE)
        if (length(mr) > 0) openxlsx::addStyle(wb, "Comparaisons paires", ns_s, rows = mr,
                                               cols = 1:5, gridExpand = TRUE)
        openxlsx::setColWidths(wb, "Comparaisons paires", cols = 1:5, widths = "auto")
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Excel exporté avec succès.", type = "message", duration = 3)
    }
  )
  
  output$downloadChiSqCSV <- downloadHandler(
    filename = function() paste0("chi2_modalités_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(values$chiSqFreqData)
      write.csv(values$chiSqFreqData, file, row.names = FALSE)
    }
  )
  
  output$downloadChiSqCSVPaires <- downloadHandler(
    filename = function() paste0("chi2_paires_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(values$chiSqPostHocData)
      write.csv(values$chiSqPostHocData, file, row.names = FALSE)
    }
  )
  
  
  output$chiSqPostHocPairesTable <- renderDT({
    req(values$chiSqPostHocData)
    df_p <- values$chiSqPostHocData
    if ("p_brute"      %in% names(df_p)) df_p$p_brute      <- sapply(df_p$p_brute,      fmt_p)
    if ("p_Bonferroni" %in% names(df_p)) df_p$p_Bonferroni <- sapply(df_p$p_Bonferroni, fmt_p)
    datatable(df_p, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE),
              class = "table-bordered table-striped") |>
      formatStyle("Decision",
                  backgroundColor = styleEqual(c("Différent", "Similaire"), c("#C8E6C9", "#FFCDD2")),
                  fontWeight = "bold")
  })
  
  output$chiSqPostHocGroupesTable <- renderDT({
    req(values$chiSqFreqData)
    fdf <- values$chiSqFreqData
    # Afficher pourcentages au lieu de Valeur_originale
    if (!"Pct" %in% names(fdf)) {
      fdf$Pct <- round(fdf$Valeur_test / sum(fdf$Valeur_test) * 100, 2)
    }
    cols_show <- intersect(c("Modalité", "Pct", "Groupe", "Statut"), names(fdf))
    df_g <- fdf[, cols_show, drop = FALSE]
    names(df_g)[names(df_g) == "Pct"] <- "Pct (%)"
    datatable(df_g, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE),
              class = "table-bordered table-striped") |>
      formatStyle("Groupe", fontWeight = "bold", color = "#1565C0", fontSize = "14px") |>
      formatStyle("Statut",
                  backgroundColor = styleEqual(
                    c("Sur-représenté", "Sous-représenté", "Conforme"),
                    c("#C8E6C9",        "#FFCDD2",          "#FFF9C4")))
  })
  
  output$chiSqPostHocInfo <- renderUI({
    req(values$chiSqResults)
    gdf <- values$chiSqResults
    div(style = "background:#e8f5e9; border-left:5px solid #2e7d32; padding:12px; border-radius:6px;",
        h5(icon("check-circle"), " Test chi² effectué", style = "color:#2e7d32; margin-top:0;"),
        p(strong("Test : "), gdf$Test[1]),
        p(strong("p-valeur : "), fmt_p(gdf$p_valeur[1])),
        p(strong("Interprétation : "), gdf$Interpretation[1]),
        p(strong("Type de données : "), gdf$Type_donnee[1])
    )
  })
  
  # UI dynamique : renommage des modalités (labels niveaux X)
  output$chiSqPHLevelLabels <- renderUI({
    req(values$chiSqFreqData)
    fdf  <- values$chiSqFreqData
    levs <- fdf$Modalite
    if (length(levs) == 0) return(NULL)
    tagList(
      tags$table(
        style = "width:100%; border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th(style = "font-size:10px; color:#888; padding:2px 4px; text-align:left;", "Original"),
          tags$th(style = "font-size:10px; color:#888; padding:2px 4px; text-align:left;", "Nouveau label")
        )),
        tags$tbody(lapply(seq_along(levs), function(i) {
          tags$tr(
            tags$td(style = "font-size:11px; padding:2px 4px; color:#444; vertical-align:middle; white-space:nowrap;",
                    levs[i]),
            tags$td(
              textInput(paste0("chiSqPHLevel_", i), label = NULL,
                        placeholder = levs[i], value = "",
                        width = "100%")
            )
          )
        }))
      )
    )
  })
  
  # UI info taille export (pixels calculés à partir de DPI x pouces)
  output$chiSqPHExportSizeInfo <- renderUI({
    w_in <- input$chiSqPHWidthIn  %||% 8
    h_in <- input$chiSqPHHeightIn %||% 6
    dpi  <- input$chiSqPHDPI      %||% 300
    w_px <- round(w_in * dpi)
    h_px <- round(h_in * dpi)
    div(style = "background:#e3f2fd; padding:6px 10px; border-radius:4px; font-size:11px; margin-top:4px; border-left:3px solid #1565C0;",
        icon("image", style = "color:#1565C0;"),
        strong(" Export : "),
        paste0(w_px, " x ", h_px, " px  @", dpi, " DPI")
    )
  })
  
  # Helper : carte de renommage des modalités (niveaux X)
  chi2_ph_level_map <- function(fdf) {
    levs <- as.character(fdf$Modalite)
    map  <- setNames(levs, levs)
    for (i in seq_along(levs)) {
      val <- input[[paste0("chiSqPHLevel_", i)]]
      if (!is.null(val) && nzchar(trimws(val))) map[levs[i]] <- trimws(val)
    }
    map
  }
  
  # Helper : construire le graphique PostHoc chi2 (réutilisé rendu + export)
  build_chi2_ph_graph <- function(fdf, opts) {
    type_g  <- opts$type_g
    show_g  <- opts$show_g
    show_v  <- opts$show_v
    titre   <- opts$titre
    sous_t  <- opts$sous_t
    x_lab   <- opts$x_lab
    y_lab   <- opts$y_lab
    leg_tit <- opts$leg_tit
    lev_map <- opts$lev_map
    p_val   <- opts$p_val
    show_p  <- opts$show_p
    
    pct_vals <- if ("Pct" %in% names(fdf)) fdf$Pct else
      round(fdf$Valeur_test / sum(fdf$Valeur_test) * 100, 2)
    
    modalites_orig  <- as.character(fdf$Modalite)
    modalites_label <- lev_map[modalites_orig]
    
    df_plot <- data.frame(
      modalite = factor(modalites_label, levels = unique(modalites_label)),
      pct      = pct_vals,
      groupe   = fdf$Groupe,
      stringsAsFactors = FALSE
    )
    
    # Sous-titre : p-value automatique si cochée et non surchargée
    if (show_p && !is.na(p_val) && (is.null(sous_t) || !nzchar(trimws(sous_t)))) {
      sous_t <- paste0("p = ", fmt_p(p_val))
    }
    if (is.null(sous_t) || !nzchar(trimws(sous_t))) sous_t <- NULL
    
    if (is.null(titre) || !nzchar(trimws(titre))) {
      titre <- if (type_g == "residus") "Résidus standardisés — PostHoc Chi²"
      else "Distribution (%) — PostHoc Chi²"
    }
    if (is.null(y_lab) || !nzchar(trimws(y_lab))) {
      y_lab <- if (type_g == "residus") "Résidu standardisé" else "Pourcentage (%)"
    }
    if (is.null(x_lab) || !nzchar(trimws(x_lab))) x_lab <- NULL
    if (is.null(leg_tit) || !nzchar(trimws(leg_tit))) leg_tit <- NULL
    
    lbl_fn <- function(pct, grp) {
      s <- paste0(round(pct, 1), "%")
      if (show_g && !is.na(grp)) s <- paste0(s, "\n(", grp, ")")
      s
    }
    
    if (type_g == "bar_v") {
      gg <- ggplot(df_plot, aes(x = modalite, y = pct, fill = modalite)) +
        geom_col(width = 0.65, color = "white") +
        labs(title = titre, subtitle = sous_t,
             x = x_lab, y = y_lab, fill = leg_tit) +
        theme_minimal(base_size = 13) +
        theme(legend.position  = if (!is.null(leg_tit)) "right" else "none",
              axis.text.x      = element_text(angle = 20, hjust = 1),
              plot.title       = element_text(face = "bold", hjust = 0.5),
              plot.subtitle    = element_text(hjust = 0.5))
      if (show_v) gg <- gg +
          geom_text(aes(label = lbl_fn(pct, groupe)), vjust = -0.4, size = 4, fontface = "bold")
      gg
      
    } else if (type_g == "pie") {
      lbl_pie <- paste0(df_plot$modalite,
                        if (show_v) paste0("\n", round(df_plot$pct, 1), "%") else "",
                        if (show_g) paste0("\n(", df_plot$groupe, ")") else "")
      ggplot(df_plot, aes(x = "", y = pct, fill = modalite)) +
        geom_col(width = 1, color = "white") + coord_polar("y") +
        geom_text(aes(label = lbl_pie),
                  position = position_stack(vjust = 0.5), size = 4, fontface = "bold") +
        labs(title = titre, subtitle = sous_t, fill = leg_tit) +
        theme_void(base_size = 13) +
        theme(plot.title    = element_text(face = "bold", hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              legend.title  = if (!is.null(leg_tit)) element_text(face = "bold") else element_blank())
      
    } else {
      df_r <- data.frame(
        modalite = factor(modalites_label, levels = unique(modalites_label)),
        residu   = fdf$Residu_std,
        couleur  = ifelse(fdf$Residu_std >  1.96, "Sur-représenté",
                          ifelse(fdf$Residu_std < -1.96, "Sous-représenté", "Conforme"))
      )
      ggplot(df_r, aes(x = modalite, y = residu, fill = couleur)) +
        geom_col(color = "white", width = 0.65) +
        geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
        scale_fill_manual(values = c("Sur-représenté" = "#1565C0",
                                     "Sous-représenté" = "#C62828",
                                     "Conforme"              = "#9E9E9E")) +
        labs(title = titre, subtitle = sous_t,
             x = x_lab, y = y_lab, fill = leg_tit %||% "Statut") +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 20, hjust = 1),
              plot.title    = element_text(face = "bold", hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
  }
  
  
  output$chiSqPostHocGraph <- renderPlot({
    req(values$chiSqFreqData)
    fdf     <- values$chiSqFreqData
    lev_map <- chi2_ph_level_map(fdf)
    build_chi2_ph_graph(fdf, list(
      type_g  = input$chiSqPHGraphType %||% "bar_v",
      show_g  = isTRUE(input$chiSqPHShowGroupes),
      show_v  = isTRUE(input$chiSqPHShowValeurs),
      show_p  = isTRUE(input$chiSqPHShowPval),
      titre   = input$chiSqPHTitle    %||% "",
      sous_t  = input$chiSqPHSubtitle %||% "",
      x_lab   = input$chiSqPHXLabel   %||% "",
      y_lab   = input$chiSqPHYLabel   %||% "",
      leg_tit = input$chiSqPHLegTitle %||% "",
      lev_map = lev_map,
      p_val   = values$chiSqPGlobal   %||% NA
    ))
  }, height = function() {
    h_in <- input$chiSqPHHeightIn %||% 6
    # Aperçu écran : limiter à 150 DPI equivalent
    max(300, min(900, round(h_in * 75)))
  })
  
  output$downloadChiSqPHPlot <- downloadHandler(
    filename = function() paste0("chi2_posthoc_", Sys.Date(), ".png"),
    content  = function(file) {
      req(values$chiSqFreqData)
      fdf     <- values$chiSqFreqData
      lev_map <- chi2_ph_level_map(fdf)
      dpi     <- max(300, min(20000, input$chiSqPHDPI     %||% 300))
      w_in    <- input$chiSqPHWidthIn  %||% 8
      h_in    <- input$chiSqPHHeightIn %||% 6
      p <- build_chi2_ph_graph(fdf, list(
        type_g  = input$chiSqPHGraphType %||% "bar_v",
        show_g  = isTRUE(input$chiSqPHShowGroupes),
        show_v  = isTRUE(input$chiSqPHShowValeurs),
        show_p  = isTRUE(input$chiSqPHShowPval),
        titre   = input$chiSqPHTitle    %||% "",
        sous_t  = input$chiSqPHSubtitle %||% "",
        x_lab   = input$chiSqPHXLabel   %||% "",
        y_lab   = input$chiSqPHYLabel   %||% "",
        leg_tit = input$chiSqPHLegTitle %||% "",
        lev_map = lev_map,
        p_val   = values$chiSqPGlobal   %||% NA
      ))
      ggsave(file, plot = p, width = w_in, height = h_in, dpi = dpi, bg = "white")
      showNotification(
        paste0("PNG exporté : ", round(w_in*dpi), "×", round(h_in*dpi),
               " px @", dpi, " DPI"),
        type = "message", duration = 5
      )
    }
  )
  
  output$downloadChiSqPHExcel <- downloadHandler(
    filename = function() paste0("chi2_posthoc_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(values$chiSqFreqData)
      wb <- openxlsx::createWorkbook()
      h_style <- openxlsx::createStyle(fontColour="#FFFFFF",fgFill="#1565C0",
                                       halign="CENTER",textDecoration="Bold")
      openxlsx::addWorksheet(wb, "Groupes")
      openxlsx::writeData(wb, "Groupes", values$chiSqFreqData, headerStyle = h_style)
      openxlsx::setColWidths(wb, "Groupes", cols=1:ncol(values$chiSqFreqData), widths="auto")
      if (!is.null(values$chiSqPostHocData) && nrow(values$chiSqPostHocData) > 0) {
        openxlsx::addWorksheet(wb, "Comparaisons paires")
        openxlsx::writeData(wb, "Comparaisons paires", values$chiSqPostHocData, headerStyle = h_style)
        openxlsx::setColWidths(wb, "Comparaisons paires", cols=1:5, widths="auto")
      }
      openxlsx::saveWorkbook(wb, file, overwrite=TRUE)
      showNotification("Excel exporté.", type="message", duration=3)
    }
  )
  
  # ---- Comparaisons multiples PostHoc  ----
  
  calc_cv <- function(x) {
    if (length(x) <= 1 || sd(x, na.rm = TRUE) == 0) return(0)
    return((sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100)
  }
  
  perform_simple_effect_posthoc <- function(df, var, factor1, factor2, level, test_type, test_method) {
    
    # Filtrer les données pour ce niveau spécifique (en excluant les NA)
    df_subset <- df[!is.na(df[[factor2]]) & as.character(df[[factor2]]) == as.character(level), ]
    
    if (is.null(df_subset) || nrow(df_subset) < 3) return(NULL)
    
    if (!is.numeric(df_subset[[var]])) {
      df_subset[[var]] <- suppressWarnings(as.numeric(df_subset[[var]]))
    }
    df_subset <- df_subset[!is.na(df_subset[[var]]), ]
    if (nrow(df_subset) < 3) return(NULL)
    
    df_subset[[factor1]] <- tryCatch(
      factor(as.character(df_subset[[factor1]])),
      error = function(e) factor(df_subset[[factor1]])
    )
    df_subset[[factor1]] <- droplevels(df_subset[[factor1]])
    
    # Guard : au moins 2 niveaux pour effectuer des comparaisons
    if (nlevels(df_subset[[factor1]]) < 2) return(NULL)
    
    tryCatch({
      groups <- NULL
      
      if (test_type == "param") {
        bt <- function(x) paste0("`", x, "`")
        model <- aov(as.formula(paste0(bt(var), " ~ ", bt(factor1))), data = df_subset)
        
        if (test_method %in% c("lsd", "tukey", "duncan", "snk", "scheffe", "regw", "waller")) {
          mc_func <- switch(test_method,
                            "lsd" = agricolae::LSD.test,
                            "tukey" = agricolae::HSD.test,
                            "duncan" = agricolae::duncan.test,
                            "snk" = agricolae::SNK.test,
                            "scheffe" = agricolae::scheffe.test,
                            "regw" = agricolae::REGW.test,
                            "waller" = agricolae::waller.test)
          mc <- mc_func(model, factor1, group = TRUE)
          groups <- mc$groups
          colnames(groups)[1:2] <- c("means", "groups")
          groups[[factor1]] <- rownames(groups)
        } else if (test_method == "bonferroni") {
          emm <- emmeans::emmeans(model, as.formula(paste0("~ `", factor1, "`")))
          mc <- pairs(emm, adjust = "bonferroni")
          pmat <- as.matrix(summary(mc)$p.value)
          if (is.null(dim(pmat))) {
            groups <- data.frame(groups = rep("a", length(unique(df_subset[[factor1]]))))
            groups[[factor1]] <- unique(df_subset[[factor1]])
          } else {
            pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
            diag(pmat) <- 1
            groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
            groups <- data.frame(groups = groups_letters)
            groups[[factor1]] <- names(groups_letters)
          }
        } else if (test_method == "dunnett") {
          emm <- emmeans::emmeans(model, as.formula(paste0("~ `", factor1, "`")))
          groups_cld <- multcomp::cld(emm, Letters = letters)
          groups <- as.data.frame(groups_cld)
          groups <- groups[, c(factor1, ".group")]
          colnames(groups) <- c(factor1, "groups")
          groups$groups <- trimws(groups$groups)
        } else if (test_method == "games") {
          bt_loc <- function(x) paste0("`", x, "`")
          mc <- PMCMRplus::gamesHowellTest(as.formula(paste0(bt_loc(var), " ~ ", bt_loc(factor1))), data = df_subset)
          pmat <- as.matrix(mc$p.value)
          pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
          diag(pmat) <- 1
          groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
          groups <- data.frame(groups = groups_letters)
          groups[[factor1]] <- names(groups_letters)
        }
      } else {  # NON-PARAMÉTRIQUE 
        if (test_method == "kruskal") {
          mc <- agricolae::kruskal(df_subset[[var]], df_subset[[factor1]], group = TRUE)
          groups <- mc$groups
          colnames(groups)[1:2] <- c("means", "groups")
          groups[[factor1]] <- rownames(groups)
        } else if (test_method == "dunn") {
          mc <- PMCMRplus::dunnTest(df_subset[[var]], df_subset[[factor1]])
          pmat <- as.matrix(mc$p.value)
          pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
          diag(pmat) <- 1
          groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
          groups <- data.frame(groups = groups_letters)
          groups[[factor1]] <- names(groups_letters)
        } else if (test_method == "conover") {
          mc <- PMCMRplus::kwAllPairsConoverTest(df_subset[[var]], df_subset[[factor1]])
          pmat <- as.matrix(mc$p.value)
          pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
          diag(pmat) <- 1
          groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
          groups <- data.frame(groups = groups_letters)
          groups[[factor1]] <- names(groups_letters)
        } else if (test_method == "nemenyi") {
          mc <- PMCMRplus::kwAllPairsNemenyiTest(df_subset[[var]], df_subset[[factor1]])
          pmat <- as.matrix(mc$p.value)
          pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
          diag(pmat) <- 1
          groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
          groups <- data.frame(groups = groups_letters)
          groups[[factor1]] <- names(groups_letters)
        }
      }
      
      if (is.null(groups)) return(NULL)
      
      desc <- df_subset %>%
        group_by(across(all_of(factor1))) %>%
        summarise(
          Moyenne = mean(.data[[var]], na.rm = TRUE),
          Ecart_type = sd(.data[[var]], na.rm = TRUE),
          N = n(),
          Erreur_type = Ecart_type / sqrt(N),
          CV = calc_cv(.data[[var]]),
          .groups = "drop"
        )
      
      res <- merge(desc, groups, by = factor1, all.x = TRUE)
      res[[factor2]] <- level
      res$groups[is.na(res$groups)] <- "a"
      
      return(res)
      
    }, error = function(e) {
      return(NULL)
    })
  }
  
  
  observeEvent(values$testResultsDF, {
    req(values$testResultsDF)
    
    if (!is.null(values$currentTestType)) {
      new_type <- if (values$currentTestType == "parametric") "param" else "nonparam"
      updateRadioButtons(session, "testType", selected = new_type)
    }
    
    values$postHocSyncTrigger <- runif(1)
    
    showNotification(
      tagList(
        icon("link"), " PostHoc mis à jour avec les résultats des tests (",
        nrow(values$testResultsDF), " résultats)"
      ),
      type = "message", duration = 3
    )
  }, ignoreInit = TRUE)
  
  # - Tableau récapitulatif des p-values pour guider le PostHoc
  output$testResultsSummaryForPostHoc <- renderUI({
    if (is.null(values$testResultsDF) || nrow(values$testResultsDF) == 0) {
      return(div(
        style = "padding:10px; background:#fff8e1; border-radius:4px; border-left:3px solid #ff9800; font-size:12px;",
        icon("exclamation-triangle", style="color:#e65100;"),
        " Aucun résultat de test disponible.",
        tags$br(),
        tags$small(style="color:#bf360c;",
                   "Allez dans 'Réalisation des tests statistiques' et lancez un test avant de faire le PostHoc.")
      ))
    }
    
    df <- values$testResultsDF
    df$p_value <- as.numeric(df$p_value)
    
    rows <- lapply(seq_len(nrow(df)), function(i) {
      row   <- df[i, ]
      p_val <- row$p_value
      sig   <- if (!is.na(p_val)) {
        if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else "ns"
      } else "?"
      
      bg_col <- if (!is.na(p_val) && p_val < 0.05) "#e8f5e9" else "#fafafa"
      p_col  <- if (!is.na(p_val) && p_val < 0.05) "#2e7d32" else "#666"
      
      tags$tr(
        style = paste0("background:", bg_col, "; border-bottom:1px solid #e0e0e0;"),
        tags$td(style = "padding:4px 8px; font-size:11.5px; font-weight:600;", row$Variable),
        tags$td(style = "padding:4px 8px; font-size:11.5px;", row$Facteur %||% "-"),
        tags$td(style = "padding:4px 8px; font-size:11.5px;", row$Test),
        tags$td(style = paste0("padding:4px 8px; font-size:12px; font-weight:bold; color:", p_col, ";"),
                if (!is.na(p_val)) formatC(p_val, format="e", digits=3) else "NA"),
        tags$td(style = paste0("padding:4px 8px; font-size:13px; font-weight:bold; color:", p_col, ";"), sig)
      )
    })
    
    n_sig <- sum(!is.na(df$p_value) & df$p_value < 0.05)
    
    tagList(
      div(
        style = "margin-bottom:8px; padding:8px 10px; background:#e3f2fd; border-left:4px solid #1976d2; border-radius:4px;",
        icon("table", style="color:#1565c0;"),
        tags$b(style="color:#0d47a1; font-size:12px;",
               paste0(" Résultats des tests -- ", n_sig, "/", nrow(df), " significatif(s)")),
        tags$br(),
        tags$small(style="color:#1976d2;",
                   "Les variables en vert (p < 0.05) sont pré-sélectionnées dans l'analyse PostHoc.")
      ),
      div(
        style = "max-height:200px; overflow-y:auto; border:1px solid #e0e0e0; border-radius:4px;",
        tags$table(
          style = "width:100%; border-collapse:collapse;",
          tags$thead(
            tags$tr(
              style = "background:#1976d2; color:white;",
              tags$th(style="padding:5px 8px; font-size:11px;", "Variable"),
              tags$th(style="padding:5px 8px; font-size:11px;", "Facteur"),
              tags$th(style="padding:5px 8px; font-size:11px;", "Test"),
              tags$th(style="padding:5px 8px; font-size:11px;", "p-value"),
              tags$th(style="padding:5px 8px; font-size:11px;", "Sig.")
            )
          ),
          tags$tbody(rows)
        )
      )
    )
  })
  
  observeEvent(input$runMultiple, {
    req(input$multiResponse, input$multiFactor)
    
    if (isTRUE(input$testType == "param" && input$multiTest == "lm_emmeans")) {
      if (is.null(values$modelList) || length(values$modelList) == 0) {
        showNotification(paste0("Aucun modèle LM/GLM disponible. Lancez d'abord ",
                                "une 'Régression linéaire' ou un 'GLM' dans l'onglet ",
                                "'Tests statistiques' avant le PostHoc."),
                         type = "warning", duration = 8)
        return()
      }
      # Ajustement choisi par l'utilisateur (défaut Tukey si "none" laissé)
      adjust  <- { a <- input$multiParamAdjust %||% "tukey"; if (identical(a, "none")) "none" else a }
      results <- list()
      for (var in names(values$modelList)) {
        model <- values$modelList[[var]]
        if (is.null(model)) next
        cat_preds <- identify_categorical_predictors(model)
        if (length(cat_preds) == 0) next
        for (pred in cat_preds) {
          pairs_df <- tryCatch(lm_pairwise_emmeans(model, pred, adjust = adjust),
                               error = function(e) NULL)
          cld_df   <- tryCatch(lm_cld_letters(model, pred, adjust = adjust),
                               error = function(e) NULL)
          if (is.null(pairs_df) && is.null(cld_df)) next
          results[[paste(var, pred, sep = "__")]] <- list(
            variable = var, predictor = pred, adjust = adjust,
            pairs = pairs_df, letters = cld_df,
            model_type = if (inherits(model, "glm")) "GLM" else "LM"
          )
        }
      }
      if (length(results) == 0) {
        showNotification("Aucun prédicteur catégoriel détecté dans le(s) modèle(s).",
                         type = "warning")
        values$lmPostHocResults <- NULL
      } else {
        values$lmPostHocResults <- results
        showNotification(paste0("PostHoc LM/GLM calculé : ", length(results),
                                " combinaison(s) Variable × Prédicteur."),
                         type = "message", duration = 4)
      }
      return()
    }
    
    updateTabsetPanel(session, "resultsTabs", selected = "mainEffects")
    
    showNotification("Analyse en cours...", type = "message", duration = NULL, id = "loading")
    
    multi_results_list <- list()
    simple_effects_list <- list()
    df <- values$filteredData
    
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Aucune donnée disponible pour l'analyse.", type="error", duration=5)
      removeNotification("loading")
      return()
    }
    for (fv in input$multiFactor) {
      if (!is.null(df[[fv]])) {
        # Conversion universelle: factor, character, date, numeric -> factor
        if (!is.factor(df[[fv]])) {
          df[[fv]] <- tryCatch(
            factor(as.character(df[[fv]])),
            error = function(e) factor(df[[fv]])
          )
        }
        df[[fv]] <- droplevels(df[[fv]])
      }
    }
    
    for (var in input$multiResponse) {
      
      for (fvar in input$multiFactor) {
        tryCatch({
          # Guard: variable réponse doit être numérique
          if (!is.numeric(df[[var]])) {
            df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
            if (all(is.na(df[[var]]))) {
              showNotification(paste0("Variable '", var, "' non convertible en numérique."), type="warning", duration=4)
              next
            }
          }
          df_var <- df[!is.na(df[[var]]), ]
          if (!is.factor(df_var[[fvar]])) df_var[[fvar]] <- factor(as.character(df_var[[fvar]]))
          df_var[[fvar]] <- droplevels(df_var[[fvar]])
          if (nlevels(df_var[[fvar]]) < 2) {
            showNotification(paste0("PostHoc '", fvar, "': moins de 2 niveaux après nettoyage."), type="warning", duration=4)
            next
          }
          if (nrow(df_var) < 4) { next }
          if (input$testType == "param") {
            bt <- function(x) paste0("`", x, "`")
            model <- aov(as.formula(paste0(bt(var), " ~ ", bt(fvar))), data = df_var)
            
            if (input$multiTest %in% c("lsd", "tukey", "duncan", "snk", "scheffe", "regw", "waller")) {
              mc_func <- switch(input$multiTest,
                                "lsd" = agricolae::LSD.test,
                                "tukey" = agricolae::HSD.test,
                                "duncan" = agricolae::duncan.test,
                                "snk" = agricolae::SNK.test,
                                "scheffe" = agricolae::scheffe.test,
                                "regw" = agricolae::REGW.test,
                                "waller" = agricolae::waller.test)
              if (length(levels(df[[fvar]])) < 2) {
                showNotification(paste0("PostHoc '", fvar, "': moins de 2 niveaux, test ignoré."), type="warning", duration=4)
                next
              }
              # Ajustement des p-values : LSD accepte p.adj ; les autres
              # (Tukey, Duncan, SNK, Scheffé, REGW, Waller) gèrent leur propre
              # contrôle de l'erreur -> l'argument est ignoré pour eux.
              pm_adj <- input$multiParamAdjust %||% "holm"
              mc <- tryCatch(
                if (input$multiTest == "lsd") {
                  padj <- if (identical(pm_adj, "none")) "none" else pm_adj
                  mc_func(model, fvar, group = TRUE, p.adj = padj)
                } else {
                  mc_func(model, fvar, group = TRUE)
                }, error = function(e) NULL)
              if (is.null(mc)) { next }
              groups <- mc$groups
              colnames(groups)[1:2] <- c("means", "groups")
              groups[[fvar]] <- rownames(groups)
            } else if (input$multiTest == "bonferroni") {
              emm <- emmeans::emmeans(model, as.formula(paste0("~ `", fvar, "`")))
              # emmeans accepte tukey/bonferroni/holm/BH/... ; "none" = brut
              pm_adj <- input$multiParamAdjust %||% "bonferroni"
              mc <- pairs(emm, adjust = if (identical(pm_adj, "none")) "none" else pm_adj)
              pmat <- as.matrix(summary(mc)$p.value)
              if (is.null(dim(pmat))) {
                groups <- data.frame(groups = rep("a", length(levels(df[[fvar]]))))
                groups[[fvar]] <- levels(df[[fvar]])
              } else {
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                groups <- data.frame(groups = groups_letters)
                groups[[fvar]] <- names(groups_letters)
              }
            } else if (input$multiTest == "dunnett") {
              emm <- emmeans::emmeans(model, as.formula(paste0("~ `", fvar, "`")))
              groups_cld <- multcomp::cld(emm, Letters = letters)
              groups <- as.data.frame(groups_cld)
              groups <- groups[, c(fvar, ".group")]
              colnames(groups) <- c(fvar, "groups")
              groups$groups <- trimws(groups$groups)
            } else if (input$multiTest == "games") {
              mc <- PMCMRplus::gamesHowellTest(as.formula(paste0("`", var, "` ~ `", fvar, "`")), data = df)
              pmat <- as.matrix(mc$p.value)
              pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
              diag(pmat) <- 1
              groups_letters <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
              groups <- data.frame(groups = groups_letters)
              groups[[fvar]] <- names(groups_letters)
            }
          } else {  # NON-PARAMÉTRIQUE
            df_var <- df[!is.na(df[[var]]), ]
            if (!is.numeric(df_var[[var]])) df_var[[var]] <- suppressWarnings(as.numeric(df_var[[var]]))
            if (!is.factor(df_var[[fvar]])) df_var[[fvar]] <- factor(as.character(df_var[[fvar]]))
            df_var[[fvar]] <- droplevels(df_var[[fvar]])
            if (nrow(df_var) < 4) { next }
            if (input$multiTestNonParam == "kruskal") {
              mc <- agricolae::kruskal(df_var[[var]], df_var[[fvar]], group = TRUE)
              groups <- mc$groups
              colnames(groups) <- c("means", "groups")
              groups[[fvar]] <- rownames(groups)
            } else if (input$multiTestNonParam == "dunn") {
              np_adj <- input$multiNonParamAdjust %||% "holm"
              groups <- tryCatch({
                mc <- PMCMRplus::dunnTest(df_var[[var]], df_var[[fvar]], p.adjust.method = np_adj)
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                gl <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                gdf <- data.frame(groups = gl); gdf[[fvar]] <- names(gl); gdf
              }, error = function(e) {
                mc <- agricolae::kruskal(df_var[[var]], df_var[[fvar]], group = TRUE)
                gdf <- mc$groups
                colnames(gdf)[1:2] <- c("means", "groups")
                gdf[[fvar]] <- rownames(gdf); gdf
              })
            } else if (input$multiTestNonParam == "conover") {
              np_adj <- input$multiNonParamAdjust %||% "holm"
              groups <- tryCatch({
                mc <- PMCMRplus::kwAllPairsConoverTest(df_var[[var]], df_var[[fvar]], p.adjust.method = np_adj)
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                gl <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                gdf <- data.frame(groups = gl); gdf[[fvar]] <- names(gl); gdf
              }, error = function(e) {
                mc <- agricolae::kruskal(df_var[[var]], df_var[[fvar]], group = TRUE)
                gdf <- mc$groups
                colnames(gdf)[1:2] <- c("means", "groups")
                gdf[[fvar]] <- rownames(gdf); gdf
              })
            } else if (input$multiTestNonParam == "nemenyi") {
              groups <- tryCatch({
                mc <- PMCMRplus::kwAllPairsNemenyiTest(df_var[[var]], df_var[[fvar]])
                pmat <- as.matrix(mc$p.value)
                pmat[is.na(pmat)] <- t(pmat)[is.na(pmat)]
                diag(pmat) <- 1
                gl <- multcompView::multcompLetters(pmat, threshold = 0.05)$Letters
                gdf <- data.frame(groups = gl); gdf[[fvar]] <- names(gl); gdf
              }, error = function(e) {
                mc <- agricolae::kruskal(df_var[[var]], df_var[[fvar]], group = TRUE)
                gdf <- mc$groups
                colnames(gdf)[1:2] <- c("means", "groups")
                gdf[[fvar]] <- rownames(gdf); gdf
              })
            }
          }
          
          # Utiliser df_var (sans NA dans var) pour les stats descriptives
          df_desc <- if (exists("df_var") && !is.null(df_var)) df_var else df
          desc <- df_desc %>%
            group_by(across(all_of(fvar))) %>%
            summarise(
              Moyenne    = mean(.data[[var]], na.rm = TRUE),
              Ecart_type = sd(.data[[var]], na.rm = TRUE),
              N          = n(),
              Erreur_type = ifelse(N > 1, Ecart_type / sqrt(N), NA_real_),
              CV         = calc_cv(.data[[var]]),
              .groups    = "drop"
            )
          
          if (fvar %in% colnames(groups) && fvar %in% colnames(desc)) {
            res <- merge(desc, groups, by = fvar, all.x = TRUE)
            res <- res %>%
              mutate(
                Moyenne = round(Moyenne, 2),
                Ecart_type = round(Ecart_type, 2),
                Erreur_type = round(Erreur_type, 2),
                CV = round(CV, 2),
                `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                Variable = var,
                Facteur = fvar,
                Type = "main"
              )
            
            multi_results_list[[paste(var, fvar, "main", sep = "_")]] <- res
          }
        }, error = function(e) {
          showNotification(hstat_err_fr(e, sprintf("Effet principal %s x %s", var, fvar)),
                           type = "error", duration = 12)
        })
      }
      
      if (input$posthocInteraction && length(input$multiFactor) > 1) {
        factor_combinations <- combn(input$multiFactor, 2, simplify = FALSE)
        
        for (fcomb in factor_combinations) {
          fvar1 <- fcomb[1]
          fvar2 <- fcomb[2]
          interaction_term <- paste(fvar1, fvar2, sep = ":")
          formula_str <- paste(var, "~", fvar1, "*", fvar2)
          
          tryCatch({
            cols_inter <- c(var, fvar1, fvar2)
            df_temp <- df[, intersect(cols_inter, names(df)), drop = FALSE]
            df_temp <- df_temp[complete.cases(df_temp), ]
            for (.f in c(fvar1, fvar2)) {
              if (!is.factor(df_temp[[.f]])) df_temp[[.f]] <- factor(df_temp[[.f]])
              df_temp[[.f]] <- droplevels(df_temp[[.f]])
            }
            if (!is.numeric(df_temp[[var]])) df_temp[[var]] <- suppressWarnings(as.numeric(df_temp[[var]]))
            if (nrow(df_temp) < 4 || all(is.na(df_temp[[var]]))) {
              showNotification(paste0("Interaction ", fvar1, ":", fvar2, " -- données insuffisantes."), type="warning", duration=4)
              return(NULL)
            }
            interaction_pvalue <- NA
            
            if (input$testType == "param") {
              formula_inter <- paste0("`", var, "` ~ `", fvar1, "` * `", fvar2, "`")
              model <- tryCatch(aov(as.formula(formula_inter), data = df_temp), error = function(e) NULL)
              if (is.null(model)) return(NULL)
              anova_res <- tryCatch(summary(model)[[1]], error = function(e) NULL)
              if (!is.null(anova_res)) {
                interaction_row <- paste(fvar1, fvar2, sep = ":")
                if (interaction_row %in% rownames(anova_res)) {
                  interaction_pvalue <- anova_res[interaction_row, "Pr(>F)"]
                }
              }
            } else { 
              
              df_temp$interaction_combined <- interaction(df_temp[[fvar1]], df_temp[[fvar2]], 
                                                          drop = TRUE, sep = ":")
              
              kw_interaction <- kruskal.test(df_temp[[var]] ~ df_temp$interaction_combined)
              interaction_pvalue <- kw_interaction$p.value
              
              showNotification(
                paste0("Test non-paramétrique (Kruskal-Wallis) pour ", fvar1, " x ", fvar2, 
                       ": p = ", round(interaction_pvalue, 4)),
                type = "message", duration = 3
              )
            }
            
            # SI INTERACTION SIGNIFICATIVE : DÉCOMPOSITION BIDIRECTIONNELLE 
            if (!is.na(interaction_pvalue) && interaction_pvalue < 0.05) {
              showNotification(
                paste0("[OK] Interaction significative détectée: ", fvar1, " x ", fvar2, 
                       " (p = ", round(interaction_pvalue, 4), ")\n",
                       "-> Décomposition bidirectionnelle en cours..."),
                type = "warning", duration = 5
              )
              
              # - Effets simples : utiliser df_temp (données nettoyées sans NA) -
              
              test_m <- ifelse(input$testType == "param", input$multiTest, input$multiTestNonParam)
              
              # Comparer fvar1 à chaque niveau de fvar2 (niveaux de df_temp, pas df brut)
              levels_fvar2 <- levels(df_temp[[fvar2]])
              if (is.null(levels_fvar2) || length(levels_fvar2) == 0)
                levels_fvar2 <- unique(as.character(df_temp[[fvar2]]))
              levels_fvar2 <- levels_fvar2[!is.na(levels_fvar2)]
              
              for (level2 in levels_fvar2) {
                res <- tryCatch(
                  perform_simple_effect_posthoc(df_temp, var, fvar1, fvar2, level2,
                                                input$testType, test_m),
                  error = function(e) NULL
                )
                if (!is.null(res) && nrow(res) > 0) {
                  res <- tryCatch(res %>%
                                    mutate(
                                      Moyenne = round(as.numeric(Moyenne), 2),
                                      Ecart_type = round(as.numeric(Ecart_type), 2),
                                      Erreur_type = round(as.numeric(Erreur_type), 2),
                                      CV = round(as.numeric(CV), 2),
                                      `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                                      `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                                      Variable = var,
                                      Facteur = paste0(fvar1, " | ", fvar2, "=", level2),
                                      Type = "simple_effect",
                                      Interaction_base = interaction_term,
                                      P_interaction = round(interaction_pvalue, 4),
                                      Direction = paste0(fvar1, " vers ", fvar2)
                                    ), error = function(e) NULL)
                  if (!is.null(res))
                    simple_effects_list[[paste(var, fvar1, fvar2, level2, sep = "_")]] <- res
                }
              }
              
              levels_fvar1 <- levels(df_temp[[fvar1]])
              if (is.null(levels_fvar1) || length(levels_fvar1) == 0)
                levels_fvar1 <- unique(as.character(df_temp[[fvar1]]))
              levels_fvar1 <- levels_fvar1[!is.na(levels_fvar1)]
              
              for (level1 in levels_fvar1) {
                res <- tryCatch(
                  perform_simple_effect_posthoc(df_temp, var, fvar2, fvar1, level1,
                                                input$testType, test_m),
                  error = function(e) NULL
                )
                if (!is.null(res) && nrow(res) > 0) {
                  res <- tryCatch(res %>%
                                    mutate(
                                      Moyenne = round(as.numeric(Moyenne), 2),
                                      Ecart_type = round(as.numeric(Ecart_type), 2),
                                      Erreur_type = round(as.numeric(Erreur_type), 2),
                                      CV = round(as.numeric(CV), 2),
                                      `Moyenne±Ecart_type` = paste0(Moyenne, "±", Ecart_type, " ", groups),
                                      `Moyenne±Erreur_type` = paste0(Moyenne, "±", Erreur_type, " ", groups),
                                      Variable = var,
                                      Facteur = paste0(fvar2, " | ", fvar1, "=", level1),
                                      Type = "simple_effect",
                                      Interaction_base = interaction_term,
                                      P_interaction = round(interaction_pvalue, 4),
                                      Direction = paste0(fvar2, " vers ", fvar1)
                                    ), error = function(e) NULL)
                  if (!is.null(res))
                    simple_effects_list[[paste(var, fvar2, fvar1, level1, sep = "_")]] <- res
                }
              }
              
              showNotification(
                paste0("[OK] Décomposition complétée pour ", fvar1, " x ", fvar2),
                type = "message", duration = 3
              )
            } else if (!is.na(interaction_pvalue)) {
              showNotification(
                paste0("[--] Interaction non significative: ", fvar1, " x ", fvar2, 
                       " (p = ", round(interaction_pvalue, 4), ")"),
                type = "default", duration = 3
              )
            }
          }, error = function(e) {
            showNotification(hstat_err_fr(e, paste("Interaction", interaction_term)),
                             type = "error", duration = 12)
          })
        }
      }
    }
    
    removeNotification("loading")
    
    all_results <- c(multi_results_list, simple_effects_list)
    
    if (length(all_results) > 0) {
      all_cols <- unique(unlist(lapply(all_results, colnames)))
      all_results <- lapply(all_results, function(df) {
        missing_cols <- setdiff(all_cols, colnames(df))
        for (col in missing_cols) {
          df[[col]] <- NA
        }
        return(df[, all_cols])
      })
      
      combined_results <- do.call(rbind, all_results)
      
      # - Bloc 9 : Retro-transformation optionnelle des moyennes PostHoc -
      # Les lettres (groupes) restent sur l'échelle transformée (rigueur stat).
      # Seules les MOYENNES affichées sont retro-transformées si option activée.
      if (!is.null(input$showBackTransformed) && isTRUE(input$showBackTransformed)) {
        log_bt <- values$transformationLog %||% list()
        if (length(log_bt) > 0 && "Variable" %in% names(combined_results)) {
          for (vname in unique(combined_results$Variable)) {
            if (vname %in% names(log_bt)) {
              entry  <- log_bt[[vname]]
              rows_v <- combined_results$Variable == vname
              for (col in c("Moyenne", "Erreur_type", "Écart_type")) {
                if (col %in% names(combined_results)) {
                  vals_orig <- as.numeric(combined_results[rows_v, col])
                  combined_results[rows_v, col] <- round(
                    back_transform_values(
                      x = vals_orig, method = entry$method,
                      lambda = entry$lambda, yj_object = entry$yj_object
                    ), 4)
                }
              }
              if (all(c("Moyenne","Écart_type","Erreur_type","groups") %in% names(combined_results))) {
                combined_results[rows_v, "Moyenne±Écart_type"]  <- paste0(
                  combined_results[rows_v,"Moyenne"], "±",
                  combined_results[rows_v,"Écart_type"], " ",
                  combined_results[rows_v,"groups"])
                combined_results[rows_v, "Moyenne±Erreur_type"] <- paste0(
                  combined_results[rows_v,"Moyenne"], "±",
                  combined_results[rows_v,"Erreur_type"], " ",
                  combined_results[rows_v,"groups"])
              }
              if ("Échelle" %in% names(combined_results) || TRUE)
                combined_results[rows_v, "Échelle"] <- paste0("originale [", entry$original, "]")
            } else {
              if ("Échelle" %in% names(combined_results))
                combined_results[combined_results$Variable == vname, "Échelle"] <- "brute"
            }
          }
        }
      }
      
      values$allPostHocResults[[length(values$allPostHocResults) + 1]] <- combined_results
      values$multiResultsMain <- combined_results
      values$currentVarIndex <- 1
      
      n_main <- sum(combined_results$Type == "main", na.rm = TRUE)
      n_simple <- sum(combined_results$Type == "simple_effect", na.rm = TRUE)
      n_interactions <- length(unique(combined_results$Interaction_base[!is.na(combined_results$Interaction_base)]))
      
      showNotification(
        HTML(paste0(
          "<b>[OK] ANALYSE TERMINÉE</b><br/>",
          "- ", n_main, " effet(s) principal(aux)<br/>",
          "- ", n_simple, " effet(s) simple(s)<br/>",
          "- ", n_interactions, " interaction(s) décomposée(s)"
        )),
        type = "message", duration = 8
      )
    } else {
      showNotification("Aucun résultat généré", type = "warning")
    }
    
    # PostHoc multivarié : pour chaque facteur, comparaisons par paires (PERMANOVA)
    # ET lettres CLD calculées séparément pour chaque variable réponse.
    # NE SE LANCE PLUS automatiquement : uniquement si l'utilisateur a coché
    # explicitement « Calculer aussi le post-hoc multivarié ».
    values$manovaMultiPostHoc <- NULL
    if (isTRUE(input$posthocMultivariate) &&
        length(input$multiResponse) >= 2 && length(input$multiFactor) >= 1) {
      mvg_test  <- if (input$testType == "param") "MANOVA" else "PERMANOVA"
      mvg_label <- paste(input$multiResponse, collapse = " + ")
      is_param  <- isTRUE(input$testType == "param")
      
      keep_rows <- stats::complete.cases(
        df[, c(input$multiResponse, input$multiFactor), drop = FALSE]
      )
      df_mvg <- df[keep_rows, , drop = FALSE]
      multi_posthoc_list <- list()
      
      for (fvar in input$multiFactor) {
        tryCatch({
          if (!is.factor(df_mvg[[fvar]])) df_mvg[[fvar]] <- factor(as.character(df_mvg[[fvar]]))
          df_mvg[[fvar]] <- droplevels(df_mvg[[fvar]])
          if (nlevels(df_mvg[[fvar]]) < 2) next
          
          Ymat <- as.matrix(df_mvg[, input$multiResponse, drop = FALSE])
          grp  <- df_mvg[[fvar]]
          
          pairs_df <- pairwise_permanova(
            Y = Ymat, group = grp,
            permutations = 999, dist_method = "euclidean",
            p_adjust = "bonferroni"
          )
          if (is.null(pairs_df) || nrow(pairs_df) == 0) next
          
          letters_per_var <- build_letters_per_variable(
            df_mvg, input$multiResponse, fvar, parametric = is_param
          )
          if (is.null(letters_per_var)) next
          
          multi_posthoc_list[[fvar]] <- list(
            pairs          = pairs_df,
            letters        = letters_per_var,
            test           = mvg_test,
            response_label = mvg_label,
            responses      = input$multiResponse,
            n_levels       = nlevels(grp)
          )
        }, error = function(e) {
          showNotification(hstat_err_fr(e, trf("Post-hoc multivarié (facteur %s)", fvar)),
                           type = "warning", duration = 5)
        })
      }
      
      if (length(multi_posthoc_list) > 0) {
        values$manovaMultiPostHoc <- multi_posthoc_list
        session$sendCustomMessage("expandBox", "boxWrap_manovaPosthoc")
      }
      
      # PostHoc d'interaction : si l'option est cochee et qu'il y a >= 2 facteurs,
      # comparer les cellules croisees (combinaisons de niveaux) pour apprecier
      # simultanement le facteur fixe et le facteur evalue.
      values$manovaInteractionPostHoc <- NULL
      if (isTRUE(input$posthocMultivariate) &&
          isTRUE(input$posthocInteraction) && length(input$multiFactor) >= 2) {
        inter_letters <- tryCatch(
          build_letters_interaction(df_mvg, input$multiResponse, input$multiFactor,
                                    parametric = is_param),
          error = function(e) NULL
        )
        if (!is.null(inter_letters)) {
          Ymat_all <- as.matrix(df_mvg[, input$multiResponse, drop = FALSE])
          cell_all <- droplevels(interaction(df_mvg[input$multiFactor], sep = " . ", drop = TRUE))
          inter_pairs <- tryCatch(
            pairwise_permanova(Ymat_all, cell_all, permutations = 999,
                               dist_method = "euclidean", p_adjust = "bonferroni"),
            error = function(e) NULL
          )
          values$manovaInteractionPostHoc <- list(
            letters   = inter_letters,
            pairs     = inter_pairs,
            factors   = input$multiFactor,
            responses = input$multiResponse,
            test      = mvg_test
          )
        }
      }
    }
  })
  
  observeEvent(input$runLMPostHoc, {
    req(values$modelList)
    if (length(values$modelList) == 0) {
      showNotification("Aucun modèle LM ou GLM à analyser.", type = "warning")
      return()
    }
    adjust  <- input$lmPostHocAdjust %||% "tukey"
    results <- list()
    for (var in names(values$modelList)) {
      model <- values$modelList[[var]]
      if (is.null(model)) next
      cat_preds <- identify_categorical_predictors(model)
      if (length(cat_preds) == 0) next
      for (pred in cat_preds) {
        pairs_df <- tryCatch(lm_pairwise_emmeans(model, pred, adjust = adjust),
                             error = function(e) NULL)
        cld_df   <- tryCatch(lm_cld_letters(model, pred, adjust = adjust),
                             error = function(e) NULL)
        if (is.null(pairs_df) && is.null(cld_df)) next
        results[[paste(var, pred, sep = "__")]] <- list(
          variable   = var,
          predictor  = pred,
          adjust     = adjust,
          pairs      = pairs_df,
          letters    = cld_df,
          model_type = if (inherits(model, "glm")) "GLM" else "LM"
        )
      }
    }
    if (length(results) == 0) {
      showNotification(paste0("Aucun prédicteur catégoriel détecté dans le(s) modèle(s) ",
                              "(les variables doivent être de type factor)."),
                       type = "warning", duration = 6)
      values$lmPostHocResults <- NULL
      return()
    }
    values$lmPostHocResults <- results
    showNotification(
      paste0("PostHoc LM/GLM calculé : ", length(results), " combinaison(s) Variable × Prédicteur."),
      type = "message", duration = 4
    )
  })
  
  output$hasLMPostHoc <- reactive({
    !is.null(values$lmPostHocResults) && length(values$lmPostHocResults) > 0
  })
  outputOptions(output, "hasLMPostHoc", suspendWhenHidden = FALSE)
  
  output$hasLMModel <- reactive({
    !is.null(values$modelList) && length(values$modelList) > 0
  })
  outputOptions(output, "hasLMModel", suspendWhenHidden = FALSE)
  
  output$lmPostHocSelector <- renderUI({
    req(values$lmPostHocResults)
    combos <- names(values$lmPostHocResults)
    labels <- vapply(values$lmPostHocResults, function(r)
      paste0(r$model_type, " : ", r$variable, " ~ ", r$predictor), character(1))
    named_choices <- setNames(combos, labels)
    selectInput(ns("lmPostHocCombo"),
                tagList(icon("filter"), " Choisir une combinaison Variable / Prédicteur :"),
                choices = named_choices, selected = combos[1], width = "100%")
  })
  
  output$lmPostHocLettersTable <- renderDT({
    req(values$lmPostHocResults, input$lmPostHocCombo)
    entry <- values$lmPostHocResults[[input$lmPostHocCombo]]
    req(entry$letters)
    df <- entry$letters
    if ("Moyenne_pm_SD" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SD"] <- "Moyenne \u00b1 Écart-type groupe"
    if ("Moyenne_pm_SE" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SE"] <- "Moyenne \u00b1 Erreur-type groupe"
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    dt <- datatable(df, options = list(scrollX = TRUE, pageLength = 15, dom = "tip"),
                    rownames = FALSE)
    if ("Groupes" %in% names(df))
      dt <- color_groups_dt(dt, df, "Groupes")
    dt
  })
  
  output$lmPostHocPairsTable <- renderDT({
    req(values$lmPostHocResults, input$lmPostHocCombo)
    entry <- values$lmPostHocResults[[input$lmPostHocCombo]]
    req(entry$pairs)
    df <- entry$pairs
    for (col in c("p_value", "p_adj")) {
      if (col %in% names(df)) df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    dt <- datatable(df, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
    if ("Significatif" %in% names(df))
      dt <- dt %>% formatStyle("Significatif",
                               backgroundColor = styleEqual(c("Oui","Non"), c("#ffebee","#f1f8e9")),
                               fontWeight = "bold")
    dt
  })
  
  output$lmPostHocInfo <- renderUI({
    req(values$lmPostHocResults, input$lmPostHocCombo)
    entry <- values$lmPostHocResults[[input$lmPostHocCombo]]
    n_pairs <- if (is.null(entry$pairs)) 0 else nrow(entry$pairs)
    n_sig   <- if (is.null(entry$pairs)) 0 else sum(entry$pairs$p_adj < 0.05, na.rm = TRUE)
    n_lev   <- if (is.null(entry$letters)) 0 else nrow(entry$letters)
    div(style = "background:#e3f2fd; border-left:4px solid #1565C0; padding:10px 14px; border-radius:6px; margin-bottom:12px; font-size:12px;",
        icon("info-circle", style = "color:#1565C0;"),
        strong(paste0(" PostHoc ", entry$model_type, " : ", entry$variable, " ~ ", entry$predictor)),
        tags$ul(style = "margin:4px 0 0 18px;",
                tags$li("Méthode : moyennes ajustées (emmeans) sur le prédicteur catégoriel"),
                tags$li(paste0("Ajustement des p-values : ", entry$adjust)),
                tags$li(paste0("Niveaux comparés : ", n_lev, " -- Paires : ", n_pairs,
                               " -- Paires significatives : ", n_sig))
        ),
        "Pour les modèles GLM non gaussiens, les comparaisons sont sur l'échelle du lien (logit, log...)."
    )
  })
  
  output$downloadLMPostHoc <- downloadHandler(
    filename = function() paste0("PostHoc_LM_GLM_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      for (key in names(values$lmPostHocResults)) {
        entry <- values$lmPostHocResults[[key]]
        sheet_l <- substr(paste0("Lettres_", entry$variable, "_", entry$predictor), 1, 31)
        sheet_p <- substr(paste0("Paires_",  entry$variable, "_", entry$predictor), 1, 31)
        if (!is.null(entry$letters)) {
          openxlsx::addWorksheet(wb, sheet_l)
          openxlsx::writeData(wb, sheet_l, entry$letters)
        }
        if (!is.null(entry$pairs)) {
          openxlsx::addWorksheet(wb, sheet_p)
          openxlsx::writeData(wb, sheet_p, entry$pairs)
        }
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  output$hasMultivariatePosthoc <- reactive({
    !is.null(values$manovaMultiPostHoc) && length(values$manovaMultiPostHoc) > 0
  })
  outputOptions(output, "hasMultivariatePosthoc", suspendWhenHidden = FALSE)
  
  # --- PostHoc Mesures repetees : flag, info, tableau, telechargement ---
  output$hasRMPostHoc <- reactive({
    !is.null(values$rmPostHocData) && nrow(values$rmPostHocData) > 0
  })
  outputOptions(output, "hasRMPostHoc", suspendWhenHidden = FALSE)
  
  output$rmPostHocInfo <- renderUI({
    req(values$rmPostHocData)
    meth <- values$rmPostHocMethod %||% "—"
    n_sig <- sum(values$rmPostHocData$Significatif == "Oui", na.rm = TRUE)
    div(style = "font-size:12px; color:#00695c; margin-bottom:8px;",
        tags$b("Méthode : "), meth, " — ",
        tags$b(n_sig), " comparaison(s) significative(s) sur ", nrow(values$rmPostHocData), ".")
  })
  
  output$rmPostHocTable <- renderDT({
    req(values$rmPostHocData)
    d <- values$rmPostHocData
    # Retirer la colonne Estimation si entierement vide (cas non parametrique).
    if (all(is.na(d$Estimation))) d$Estimation <- NULL
    datatable(d, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle("Significatif",
                  backgroundColor = styleEqual(c("Oui", "n.s."), c("#c8e6c9", "#fff")),
                  fontWeight = styleEqual("Oui", "bold"))
  })
  
  output$downloadRMPostHoc <- downloadHandler(
    filename = function() paste0("posthoc_mesures_repetees_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(values$rmPostHocData)
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        openxlsx::write.xlsx(values$rmPostHocData, file)
      } else {
        utils::write.csv(values$rmPostHocData, sub("\\.xlsx$", ".csv", file), row.names = FALSE)
      }
    }
  )
  
  # Selecteur de facteur (dynamique selon les facteurs disponibles dans manovaMultiPostHoc)
  output$multivariatePosthocFactorSelect <- renderUI({
    req(values$manovaMultiPostHoc)
    fcts <- names(values$manovaMultiPostHoc)
    if (length(fcts) == 0) return(NULL)
    selectInput(ns("multivariatePosthocFactor"),
                label = NULL,
                choices = fcts,
                selected = fcts[1],
                width = "100%")
  })
  
  output$multivariatePosthocInfo <- renderUI({
    req(values$manovaMultiPostHoc, input$multivariatePosthocFactor)
    entry <- values$manovaMultiPostHoc[[input$multivariatePosthocFactor]]
    if (is.null(entry)) return(NULL)
    
    n_sig <- sum(entry$pairs$p_adj < 0.05, na.rm = TRUE)
    n_pairs <- nrow(entry$pairs)
    
    div(style = "background:#e8f5e9; border-left:4px solid #43a047; padding:10px 14px; border-radius:6px; margin-bottom:12px; font-size:12px;",
        icon("info-circle", style = "color:#2e7d32;"),
        strong(paste0(" PostHoc multivarié -- ", entry$test, " sur ", entry$response_label, " :")),
        tags$ul(style = "margin:4px 0 0 18px;",
                tags$li("Méthode : pairwise PERMANOVA (vegan::adonis2), distance euclidienne, 999 permutations"),
                tags$li("Ajustement des p-values : Bonferroni"),
                tags$li("Lettres CLD générées par multcompView::multcompLetters sur la matrice de p-values ajustées"),
                tags$li(paste0("Niveaux comparés : ", entry$n_levels,
                               " -- Paires : ", n_pairs,
                               " -- Paires significatives : ", n_sig))
        ),
        "Interprétation : deux niveaux partageant une même lettre ne diffèrent pas significativement sur le vecteur de réponses multivariées."
    )
  })
  
  output$multivariatePosthocLettersTable <- renderDT({
    req(values$manovaMultiPostHoc, input$multivariatePosthocFactor)
    entry <- values$manovaMultiPostHoc[[input$multivariatePosthocFactor]]
    req(entry$letters)
    
    df <- entry$letters
    names(df)[names(df) == "Niveau"] <- input$multivariatePosthocFactor
    if ("Moyenne_pm_SD" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SD"] <- "Moyenne \u00b1 Écart-type groupe"
    if ("Moyenne_pm_SE" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SE"] <- "Moyenne \u00b1 Erreur-type groupe"
    
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    
    dt <- datatable(df,
                    options = list(pageLength = 25, scrollX = TRUE, dom = "tip"),
                    rownames = FALSE)
    dt <- color_groups_dt(dt, df, "Groupes")
    if ("Variable" %in% names(df))
      dt <- dt %>% formatStyle("Variable", fontWeight = "bold",
                               backgroundColor = "#e3f2fd")
    dt
  })
  
  output$multivariatePosthocPairsTable <- renderDT({
    req(values$manovaMultiPostHoc, input$multivariatePosthocFactor)
    entry <- values$manovaMultiPostHoc[[input$multivariatePosthocFactor]]
    req(entry$pairs)
    
    df <- entry$pairs
    for (col in c("p_value", "p_adj")) {
      if (col %in% names(df))
        df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    
    dt <- datatable(df,
                    options = list(pageLength = 25, scrollX = TRUE),
                    rownames = FALSE)
    
    if ("Significatif" %in% names(df)) {
      dt <- dt %>% formatStyle("Significatif",
                               backgroundColor = styleEqual(c("Oui", "Non"),
                                                            c("#ffebee", "#f1f8e9")),
                               fontWeight = "bold")
    }
    dt
  })
  
  # Telechargement Excel multi-feuilles (1 feuille de lettres + 1 feuille de paires par facteur)
  output$downloadMultivariatePosthoc <- downloadHandler(
    filename = function() paste0("PostHoc_MANOVA_multivarie_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      for (fname in names(values$manovaMultiPostHoc)) {
        entry <- values$manovaMultiPostHoc[[fname]]
        sheet_letters <- substr(paste0("Lettres_", fname), 1, 31)
        sheet_pairs   <- substr(paste0("Paires_",  fname), 1, 31)
        
        letters_export <- entry$letters
        names(letters_export)[names(letters_export) == "Niveau"] <- fname
        openxlsx::addWorksheet(wb, sheet_letters)
        openxlsx::writeData(wb, sheet_letters, letters_export)
        
        openxlsx::addWorksheet(wb, sheet_pairs)
        openxlsx::writeData(wb, sheet_pairs, entry$pairs)
      }
      if (!is.null(values$manovaInteractionPostHoc)) {
        openxlsx::addWorksheet(wb, "Interaction_lettres")
        openxlsx::writeData(wb, "Interaction_lettres", values$manovaInteractionPostHoc$letters)
        if (!is.null(values$manovaInteractionPostHoc$pairs)) {
          openxlsx::addWorksheet(wb, "Interaction_paires")
          openxlsx::writeData(wb, "Interaction_paires", values$manovaInteractionPostHoc$pairs)
        }
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$hasManovaInteractionPostHoc <- reactive({
    !is.null(values$manovaInteractionPostHoc) &&
      !is.null(values$manovaInteractionPostHoc$letters)
  })
  outputOptions(output, "hasManovaInteractionPostHoc", suspendWhenHidden = FALSE)
  
  output$manovaInteractionPostHocInfo <- renderUI({
    req(values$manovaInteractionPostHoc)
    ent <- values$manovaInteractionPostHoc
    div(style = "background:#fff3e0; border-left:4px solid #fb8c00; padding:10px 14px; border-radius:6px; margin-bottom:12px; font-size:12px;",
        icon("project-diagram", style = "color:#e65100;"),
        strong(" Comparaison des cellules d'interaction : "),
        "chaque cellule combine un niveau de ", strong(paste(ent$factors, collapse = " et de ")),
        ". Les lettres comparent simultanément l'effet du facteur fixé et du facteur évalué. ",
        "Deux cellules partageant une lettre ne diffèrent pas significativement (alpha = 0.05)."
    )
  })
  
  output$manovaInteractionLettersTable <- renderDT({
    req(values$manovaInteractionPostHoc)
    df <- values$manovaInteractionPostHoc$letters
    if ("Moyenne_pm_SD" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SD"] <- "Moyenne \u00b1 Écart-type groupe"
    if ("Moyenne_pm_SE" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SE"] <- "Moyenne \u00b1 Erreur-type groupe"
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    dt <- datatable(df, options = list(pageLength = 25, scrollX = TRUE, dom = "tip"),
                    rownames = FALSE)
    dt <- color_groups_dt(dt, df, "Groupes")
    if ("Variable" %in% names(df))
      dt <- dt %>% formatStyle("Variable", fontWeight = "bold", backgroundColor = "#e3f2fd")
    dt
  })
  
  output$manovaInteractionPairsTable <- renderDT({
    req(values$manovaInteractionPostHoc)
    df <- values$manovaInteractionPostHoc$pairs
    if (is.null(df))
      return(datatable(data.frame(Information = "Comparaisons par paires indisponibles."),
                       options = list(dom = "t"), rownames = FALSE))
    for (col in c("p_value", "p_adj")) {
      if (col %in% names(df))
        df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    dt <- datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
    if ("Significatif" %in% names(df))
      dt <- dt %>% formatStyle("Significatif",
                               backgroundColor = styleEqual(c("Oui", "Non"),
                                                            c("#ffebee", "#f1f8e9")),
                               fontWeight = "bold")
    dt
  })
  
  output$multiResponseSelect <- renderUI({
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    
    # Pré-sélection initiale : variables choisies dans "Paramètres des tests",
    # sinon repli sur les variables des résultats de tests.
    pre_selected <- if (!is.null(isolate(input$responseVar)) &&
                        length(isolate(input$responseVar)) > 0) {
      isolate(input$responseVar)
    } else if (!is.null(values$testResultsDF) && "Variable" %in% names(values$testResultsDF)) {
      sig_vars <- unique(values$testResultsDF$Variable[
        !is.na(values$testResultsDF$p_value) & values$testResultsDF$p_value < 0.05
      ])
      if (length(sig_vars) == 0) unique(values$testResultsDF$Variable) else sig_vars
    } else { character(0) }
    pre_selected <- intersect(pre_selected, num_cols)
    
    tagList(
      pickerInput(ns("multiResponse"), "Variable(s) réponse:",
                  choices  = num_cols,
                  selected = if (length(pre_selected) > 0) pre_selected else NULL,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      div(style = "display: flex; gap: 10px;",
          actionButton(ns("selectAllMultiResponse"), "Tout sélectionner",
                       class = "btn-success btn-sm", style = "flex: 1; height: 40px;"),
          actionButton(ns("deselectAllMultiResponse"), "Tout désélectionner",
                       class = "btn-danger btn-sm", style = "flex: 1; height: 40px;")
      )
    )
  })
  
  # Synchronise multiResponse avec les variables choisies dans "Paramètres des tests"
  observeEvent(input$responseVar, {
    req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    sel <- intersect(input$responseVar, num_cols)
    if (length(sel) > 0)
      updatePickerInput(session, "multiResponse", selected = sel)
  }, ignoreNULL = TRUE)
  
  observeEvent(input$selectAllMultiResponse, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "multiResponse", selected = num_cols)
  })
  
  observeEvent(input$deselectAllMultiResponse, {
    updatePickerInput(session, "multiResponse", selected = character(0))
  })
  
  output$multiFactorSelect <- renderUI({
    req(values$filteredData)
    fac_cols <- get_all_factor_candidates(values$filteredData)
    
    # Pré-sélection initiale : facteurs choisis dans "Paramètres des tests",
    # sinon repli sur les facteurs des résultats de tests.
    pre_fac <- if (!is.null(isolate(input$factorVar)) &&
                   length(isolate(input$factorVar)) > 0) {
      isolate(input$factorVar)
    } else if (!is.null(values$testResultsDF) && "Facteur" %in% names(values$testResultsDF)) {
      unique(values$testResultsDF$Facteur[!is.na(values$testResultsDF$Facteur)])
    } else { character(0) }
    pre_fac <- intersect(pre_fac, fac_cols)
    
    tagList(
      pickerInput(ns("multiFactor"), "Facteur(s):",
                  choices  = fac_cols,
                  selected = if (length(pre_fac) > 0) pre_fac else NULL,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      tags$small(style = "color:#6c757d; font-size:11px;",
                 icon("info-circle"), " Facteur, texte, date et numérique (<= 30 niveaux) acceptés"),
      div(style = "display: flex; gap: 10px;",
          actionButton(ns("selectAllMultiFactors"),   "Tout sélectionner",
                       class = "btn-success btn-sm", style = "flex: 1; height: 40px;"),
          actionButton(ns("deselectAllMultiFactors"), "Tout désélectionner",
                       class = "btn-danger btn-sm",  style = "flex: 1; height: 40px;")
      )
    )
  })
  
  # Synchronise multiFactor avec les facteurs choisis dans "Paramètres des tests"
  observeEvent(input$factorVar, {
    req(values$filteredData)
    fac_cols <- get_all_factor_candidates(values$filteredData)
    sel <- intersect(input$factorVar, fac_cols)
    if (length(sel) > 0)
      updatePickerInput(session, "multiFactor", selected = sel)
  }, ignoreNULL = TRUE)
  
  observeEvent(input$selectAllMultiFactors, {
    updatePickerInput(session, "multiFactor", selected = get_all_factor_candidates(values$filteredData))
  })
  
  observeEvent(input$deselectAllMultiFactors, {
    updatePickerInput(session, "multiFactor", selected = character(0))
  })
  
  # - Bloc 8 : Info transformations dans le panel PostHoc -
  output$postHocTransformInfo <- renderUI({
    log           <- values$transformationLog %||% list()
    selected_vars <- input$multiResponse
    if (length(log) == 0 || is.null(selected_vars) || length(selected_vars) == 0) return(NULL)
    trans_selected <- intersect(selected_vars, names(log))
    if (length(trans_selected) == 0) return(NULL)
    entries <- lapply(trans_selected, function(vname) {
      entry       <- log[[vname]]
      lambda_info <- if (!is.null(entry$lambda)) paste0(" (λ = ", entry$lambda, ")") else ""
      tags$li(
        style = "font-size:11.5px;margin-bottom:4px;line-height:1.5;",
        tags$b(style = "color:#1565c0;", vname),
        tags$span(style = "color:#555;", " ← "),
        tags$b(entry$original),
        tags$code(
          style = paste0("font-size:10.5px;background:#e3f2fd;padding:1px 4px;",
                         "border-radius:3px;color:#0d47a1;margin-left:4px;"),
          paste0(entry$formula, lambda_info)
        )
      )
    })
    div(
      style = paste0("padding:10px 12px;background:#e3f2fd;",
                     "border-left:4px solid #1976d2;border-radius:4px;margin-bottom:10px;"),
      div(style = "font-weight:bold;color:#0d47a1;font-size:12px;margin-bottom:6px;",
          icon("flask"), " Variables transformées sélectionnées"),
      tags$ul(style = "margin:0;padding-left:16px;", tagList(entries)),
      div(
        style = paste0("font-size:11px;color:#1565c0;margin-top:8px;",
                       "padding-top:6px;border-top:1px solid #90caf9;font-style:italic;"),
        icon("info-circle"),
        " Le PostHoc est réalisé sur les données transformées.",
        " Les lettres de significativité s'appliquent à l'échelle transformée.",
        " Activez 'Retro-transformation' pour afficher les moyennes sur l'échelle originale."
      )
    )
  })
  
  output$hasTransformedVarsSelected <- reactive({
    log      <- values$transformationLog %||% list()
    selected <- input$multiResponse
    if (length(log) == 0 || is.null(selected)) return(FALSE)
    any(selected %in% names(log))
  })
  outputOptions(output, "hasTransformedVarsSelected", suspendWhenHidden = FALSE) 
  output$mainEffectsTable <- renderDT({
    req(values$multiResultsMain)
    
    main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
    
    if (nrow(main_data) == 0) return(NULL)
    
    cols_to_show <- c("Variable", "Facteur", "Moyenne", "Écart_type", "Erreur_type", "CV", "groups", "N", "Moyenne±Écart_type", "Moyenne±Erreur_type")
    
    for (fvar in input$multiFactor) {
      if (fvar %in% colnames(main_data)) {
        cols_to_show <- c(cols_to_show, fvar)
      }
    }
    
    cols_to_show <- unique(cols_to_show)
    cols_to_show <- cols_to_show[cols_to_show %in% colnames(main_data)]
    
    dt <- datatable(
      main_data[, cols_to_show, drop = FALSE],
      options = list(
        # scrollX retire volontairement : avec scrollX, DataTables scinde
        # l'en-tete et le corps en DEUX tables dont les largeurs derivent ->
        # les valeurs se retrouvent sous la mauvaise colonne. En table unique,
        # en-tete et corps partagent la meme grille : alignement garanti.
        scrollX = FALSE,
        autoWidth = FALSE,
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      extensions = 'Buttons',
      class = 'cell-border stripe hstat-fixedcols'
    )
    if (isTRUE(input$multiRoundResults)) {
      dec <- if (is.null(input$multiDecimals) || is.na(input$multiDecimals)) 2
      else as.integer(input$multiDecimals)
      round_cols <- intersect(c("Moyenne", "Écart_type", "Erreur_type", "CV"),
                              cols_to_show)
      if (length(round_cols) > 0)
        dt <- dt %>% formatRound(columns = round_cols, digits = dec)
    }
    dt %>%
      formatStyle(
        'groups',
        backgroundColor = styleEqual(
          unique(main_data$groups),
          rainbow(length(unique(main_data$groups)), alpha = 0.3)
        ),
        fontWeight = 'bold'
      )
  })
  
  output$simpleEffectsTable <- renderDT({
    req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    if (!is.null(input$filterSimpleEffectVar) && input$filterSimpleEffectVar != "Toutes") {
      simple_data <- simple_data[simple_data$Variable == input$filterSimpleEffectVar, ]
    }
    
    if (!is.null(input$filterSimpleEffectInteraction) && input$filterSimpleEffectInteraction != "Toutes") {
      simple_data <- simple_data[simple_data$Interaction_base == input$filterSimpleEffectInteraction, ]
    }
    
    cols_to_show <- c("Variable", "Facteur", "Direction", "Interaction_base", "P_interaction", 
                      "Moyenne", "Écart_type", "Erreur_type", "CV", "groups", "N", "Moyenne±Écart_type", "Moyenne±Erreur_type")
    
    for (fvar in input$multiFactor) {
      if (fvar %in% colnames(simple_data)) {
        cols_to_show <- c(cols_to_show, fvar)
      }
    }
    
    cols_to_show <- unique(cols_to_show)
    cols_to_show <- cols_to_show[cols_to_show %in% colnames(simple_data)]
    
    dt <- datatable(
      simple_data[, cols_to_show, drop = FALSE],
      options = list(
        scrollX = FALSE,
        autoWidth = FALSE,
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      extensions = 'Buttons',
      class = 'cell-border stripe hstat-fixedcols'
    )
    if (isTRUE(input$multiRoundResults)) {
      dec <- if (is.null(input$multiDecimals) || is.na(input$multiDecimals)) 2
      else as.integer(input$multiDecimals)
      round_cols <- intersect(c("Moyenne", "Écart_type", "Erreur_type", "CV", "P_interaction"),
                              cols_to_show)
      if (length(round_cols) > 0)
        dt <- dt %>% formatRound(columns = round_cols, digits = dec)
    }
    dt %>%
      formatStyle(
        'groups',
        backgroundColor = styleEqual(
          unique(simple_data$groups),
          rainbow(length(unique(simple_data$groups)), alpha = 0.3)
        ),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'P_interaction',
        backgroundColor = styleInterval(c(0.01, 0.05), c('#e74c3c', '#f39c12', '#95a5a6')),
        color = 'white',
        fontWeight = 'bold'
      )
  })
  
  output$showPosthocResults <- reactive({
    !is.null(values$multiResultsMain) && nrow(values$multiResultsMain) > 0
  })
  outputOptions(output, "showPosthocResults", suspendWhenHidden = FALSE)
  
  output$showSimpleEffects <- reactive({
    !is.null(values$multiResultsMain) && 
      any(values$multiResultsMain$Type == "simple_effect", na.rm = TRUE)
  })
  outputOptions(output, "showSimpleEffects", suspendWhenHidden = FALSE)
  
  output$analysisSummaryMain <- renderUI({
    req(values$multiResultsMain)
    
    main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
    
    n_vars <- length(unique(main_data$Variable))
    n_factors <- length(unique(main_data$Facteur))
    n_comparisons <- nrow(main_data)
    
    # Méthode et ajustement appliqués (transparence)
    is_param <- identical(input$testType, "param")
    meth_lab <- if (is_param) {
      switch(input$multiTest %||% "tukey",
             tukey = "Tukey HSD", lsd = "LSD (Fisher)", duncan = "Duncan",
             snk = "SNK", scheffe = "Scheffé", regw = "REGW", waller = "Waller-Duncan",
             bonferroni = "Bonferroni", dunnett = "Dunnett", games = "Games-Howell",
             manova = "MANOVA", lm_emmeans = "LM/GLM (emmeans)", input$multiTest)
    } else {
      switch(input$multiTestNonParam %||% "dunn",
             kruskal = "Kruskal-Wallis", dunn = "Dunn", conover = "Conover",
             nemenyi = "Nemenyi", permanova = "PERMANOVA", input$multiTestNonParam)
    }
    adj_used <- if (is_param) input$multiParamAdjust %||% "holm" else input$multiNonParamAdjust %||% "holm"
    adj_lab <- switch(adj_used, holm = "Holm", bonferroni = "Bonferroni", BH = "BH (FDR)",
                      BY = "BY", hochberg = "Hochberg", hommel = "Hommel", none = "aucun", adj_used)
    # L'ajustement ne s'applique qu'aux méthodes par paires
    adj_applies <- (is_param && (input$multiTest %||% "") %in% c("lsd", "bonferroni", "lm_emmeans")) || !is_param

    div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px; border-radius: 8px;",
        h5(icon("layer-group"), " Effets principaux", style = "margin-top: 0;"),
        tags$ul(
          tags$li(strong(n_vars), " variable(s) analysée(s)"),
          tags$li(strong(n_factors), " facteur(s) testé(s)"),
          tags$li(strong(n_comparisons), " comparaison(s)"),
          tags$li("Méthode : ", strong(meth_lab)),
          tags$li("Ajustement des p-values : ",
                  strong(if (adj_applies) adj_lab else "intégré à la méthode"))
        )
    )
  })
  
  output$simpleEffectsSummary <- renderUI({
    req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    n_interactions <- length(unique(simple_data$Interaction_base))
    n_tests <- nrow(simple_data)
    n_directions <- length(unique(simple_data$Direction[!is.na(simple_data$Direction)]))
    
    div(style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
        h5(icon("project-diagram"), " Décomposition des interactions", style = "margin-top: 0;"),
        tags$ul(
          tags$li(strong(n_interactions), " interaction(s) significative(s)"),
          tags$li(strong(n_tests), " test(s) d'effets simples"),
          tags$li(strong(n_directions), " direction(s) d'analyse")
        )
    )
  })
  
  observe({
    req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) > 0) {
      vars <- c("Toutes", unique(simple_data$Variable))
      interactions <- c("Toutes", unique(simple_data$Interaction_base))
      
      updateSelectInput(session, "filterSimpleEffectVar", choices = vars, selected = "Toutes")
      updateSelectInput(session, "filterSimpleEffectInteraction", choices = interactions, selected = "Toutes")
    }
  })
  
  output$selectSimpleEffectPlot <- renderUI({
    req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
    }
    
    if (is.null(input$multiResponse) || length(input$multiResponse) == 0) {
      return(div(style = "color: #e74c3c; font-style: italic;", 
                 "Aucune variable sélectionnée"))
    }
    
    current_var_idx <- values$currentVarIndex
    max_idx <- length(input$multiResponse)
    
    if (current_var_idx < 1 || current_var_idx > max_idx) {
      current_var_idx <- 1
      values$currentVarIndex <- 1
    }
    
    resp_var <- tryCatch({
      input$multiResponse[[current_var_idx]]
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(resp_var) || is.na(resp_var) || resp_var == "") {
      return(div(style = "color: #e74c3c; font-style: italic;", 
                 "Erreur d'accès à la variable"))
    }
    
    simple_var_data <- simple_data[simple_data$Variable == resp_var, ]
    
    if (nrow(simple_var_data) == 0) {
      return(div(style = "color: #e74c3c; font-style: italic;", 
                 paste("Aucun effet simple pour", resp_var)))
    }
    
    factors <- unique(simple_var_data$Facteur)
    
    if (length(factors) == 0) {
      return(div(style = "color: #e74c3c; font-style: italic;", 
                 "Aucun facteur disponible"))
    }
    
    selectInput(ns("selectedSimpleEffect"), 
                "Sélectionner l'effet simple:",
                choices = factors,
                width = "100%")
  })
  
  output$fullAnalysisReport <- renderUI({
    req(values$multiResultsMain)
    
    main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    tagList(
      h4("Vue d'ensemble"),
      fluidRow(
        column(6,
               div(style = "background-color: white; padding: 15px; border-radius: 5px; border-left: 4px solid #667eea;",
                   h5(icon("layer-group"), " Effets principaux"),
                   p(strong(nrow(main_data)), " comparaisons"),
                   p(strong(length(unique(main_data$Variable))), " variables"),
                   p(strong(length(unique(main_data$Facteur))), " facteurs")
               )
        ),
        column(6,
               div(style = "background-color: white; padding: 15px; border-radius: 5px; border-left: 4px solid #e74c3c;",
                   h5(icon("project-diagram"), " Effets simples"),
                   p(strong(nrow(simple_data)), " tests"),
                   p(strong(length(unique(simple_data$Interaction_base))), " interactions décomposées"),
                   p(strong(length(unique(simple_data$Direction[!is.na(simple_data$Direction)]))), " directions d'analyse")
               )
        )
      ),
      br(),
      h4("Détails par variable"),
      uiOutput(ns("variableDetailedReport"))
    )
  })
  
  output$variableDetailedReport <- renderUI({
    req(values$multiResultsMain)
    
    vars <- unique(values$multiResultsMain$Variable)
    
    reports <- lapply(vars, function(v) {
      var_data <- values$multiResultsMain[values$multiResultsMain$Variable == v, ]
      main_count <- sum(var_data$Type == "main")
      simple_count <- sum(var_data$Type == "simple_effect")
      
      div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          strong(v),
          br(),
          sprintf("- %d effet(s) principal(aux)", main_count),
          br(),
          sprintf("- %d effet(s) simple(s)", simple_count)
      )
    })
    
    do.call(tagList, reports)
  })
  
  output$showVariableNavigation <- reactive({
    length(input$multiResponse) > 1
  })
  outputOptions(output, "showVariableNavigation", suspendWhenHidden = FALSE)
  
  output$variableNavigation <- renderUI({
    req(input$multiResponse)
    if (is.null(input$multiResponse) || length(input$multiResponse) <= 1) {
      return(NULL)
    }
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
    }
    
    current_idx <- values$currentVarIndex
    total_vars <- length(input$multiResponse)
    
    if (current_idx < 1 || current_idx > total_vars) {
      current_idx <- 1
      values$currentVarIndex <- 1
    }
    
    current_var_name <- tryCatch({
      var_temp <- input$multiResponse[[current_idx]]
      if (is.null(var_temp) || is.na(var_temp) || var_temp == "") {
        paste("Variable", current_idx)
      } else {
        var_temp
      }
    }, error = function(e) {
      paste("Variable", current_idx)
    })
    
    div(style = "display: flex; align-items: center; gap: 15px;",
        actionButton(ns("prevMultiVar"), "", 
                     icon = icon("chevron-left"), 
                     class = "btn-light btn-lg",
                     style = "font-size: 1.5em; padding: 10px 20px; height: 60px; width: 60px;"),
        div(style = "color: white; font-size: 1.2em; font-weight: bold; text-align: center;",
            span(style = "display: block; font-size: 0.8em; opacity: 0.8;", 
                 paste("Variable", current_idx, "/", total_vars)),
            span(current_var_name)
        ),
        actionButton(ns("nextMultiVar"), "", 
                     icon = icon("chevron-right"), 
                     class = "btn-light btn-lg",
                     style = "font-size: 1.5em; padding: 10px 20px; height: 60px; width: 60px;")
    )
  })
  
  observeEvent(input$prevMultiVar, {
    if (is.null(input$multiResponse) || length(input$multiResponse) == 0) {
      return(NULL)
    }
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
      return(NULL)
    }
    
    tryCatch({
      current <- as.integer(values$currentVarIndex)
      total <- length(input$multiResponse)
      
      new_idx <- if (current > 1) current - 1 else total
      
      if (new_idx >= 1 && new_idx <= total) {
        values$currentVarIndex <- new_idx
      }
    }, error = function(e) {
      NULL
    })
  })
  
  observeEvent(input$nextMultiVar, {
    if (is.null(input$multiResponse) || length(input$multiResponse) == 0) {
      return(NULL)
    }
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
      return(NULL)
    }
    
    tryCatch({
      current <- as.integer(values$currentVarIndex)
      total <- length(input$multiResponse)
      
      new_idx <- if (current < total) current + 1 else 1
      
      if (new_idx >= 1 && new_idx <= total) {
        values$currentVarIndex <- new_idx
      }
    }, error = function(e) {
      NULL
    })
  })
  
  
  output$xAxisOrderUI <- renderUI({
    req(input$multiResponse, input$multiFactor, values$filteredData)
    
    fvar <- NULL
    if (input$plotDisplayType == "main") {
      fvar <- tryCatch({
        input$multiFactor[[1]]
      }, error = function(e) NULL)
    } else {
      # Pour les effets simples, extraire le facteur du nom
      if (!is.null(input$selectedSimpleEffect) && input$selectedSimpleEffect != "") {
        parse_result <- tryCatch({
          parts <- strsplit(input$selectedSimpleEffect, " | ", fixed = TRUE)[[1]]
          if (length(parts) >= 1) trimws(parts[1]) else NULL
        }, error = function(e) NULL)
        fvar <- parse_result
      }
    }
    
    if (is.null(fvar) || !fvar %in% colnames(values$filteredData)) return(NULL)
    
    current_levels <- if (!is.null(values$customXLevels())) {
      values$customXLevels()
    } else if (is.factor(values$filteredData[[fvar]])) {
      levels(values$filteredData[[fvar]])
    } else {
      unique(as.character(values$filteredData[[fvar]]))
    }
    
    if (length(current_levels) == 0) return(NULL)
    
    tagList(
      h6("Ordre actuel des catégories :", style = "font-weight: bold; color: #27ae60;"),
      div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; border-radius: 5px; padding: 5px;",
          lapply(seq_along(current_levels), function(i) {
            level_name <- current_levels[i]
            div(style = "background-color: white; padding: 8px; margin-bottom: 5px; border-radius: 4px; border: 1px solid #e0e0e0;",
                fluidRow(
                  column(2, 
                         strong(paste0(i, ".")),
                         style = "text-align: center; padding-top: 5px;"
                  ),
                  column(6, 
                         span(level_name, style = "font-weight: bold; color: #2c3e50;")
                  ),
                  column(4,
                         div(style = "text-align: right;",
                             if (i > 1) {
                               actionButton(paste0("moveUp_", i), "", 
                                            icon = icon("arrow-up"),
                                            class = "btn-sm btn-primary",
                                            style = "margin-right: 5px; padding: 2px 8px;")
                             },
                             if (i < length(current_levels)) {
                               actionButton(paste0("moveDown_", i), "", 
                                            icon = icon("arrow-down"),
                                            class = "btn-sm btn-primary",
                                            style = "padding: 2px 8px;")
                             }
                         )
                  )
                )
            )
          })
      ),
      hr(),
      actionButton(ns("resetXOrder"), "Réinitialiser l'ordre", 
                   class = "btn-warning btn-sm", 
                   icon = icon("undo"),
                   style = "width: 100%;")
    )
  })
  
  
  observe({
    req(input$multiResponse, input$multiFactor, values$filteredData)
    
    fvar <- NULL
    if (!is.null(input$plotDisplayType) && input$plotDisplayType == "main") {
      fvar <- tryCatch({
        input$multiFactor[[1]]
      }, error = function(e) NULL)
    } else {
      if (!is.null(input$selectedSimpleEffect) && input$selectedSimpleEffect != "") {
        parse_result <- tryCatch({
          parts <- strsplit(input$selectedSimpleEffect, " | ", fixed = TRUE)[[1]]
          if (length(parts) >= 1) trimws(parts[1]) else NULL
        }, error = function(e) NULL)
        fvar <- parse_result
      }
    }
    
    if (is.null(fvar) || !fvar %in% colnames(values$filteredData)) return(NULL)
    
    current_levels <- if (!is.null(values$customXLevels())) {
      values$customXLevels()
    } else if (is.factor(values$filteredData[[fvar]])) {
      levels(values$filteredData[[fvar]])
    } else {
      unique(as.character(values$filteredData[[fvar]]))
    }
    
    if (length(current_levels) == 0) return(NULL)
    
    lapply(seq_along(current_levels), function(i) {
      observeEvent(input[[paste0("moveUp_", i)]], {
        if (i > 1) {
          new_order <- current_levels
          new_order[c(i-1, i)] <- new_order[c(i, i-1)]
          values$customXLevels(new_order)
        }
      }, ignoreInit = TRUE)
    })
    
    lapply(seq_along(current_levels), function(i) {
      observeEvent(input[[paste0("moveDown_", i)]], {
        if (i < length(current_levels)) {
          new_order <- current_levels
          new_order[c(i, i+1)] <- new_order[c(i+1, i)]
          values$customXLevels(new_order)
        }
      }, ignoreInit = TRUE)
    })
  })
  
  
  observeEvent(input$resetXOrder, {
    values$customXLevels(NULL)
    showNotification("Ordre des catégories réinitialisé", type = "message", duration = 2)
  })
  
  # Message expliquant pourquoi le graphique post-hoc est vide (le cas echeant).
  # NON reactif (simple environnement) pour eviter tout cycle d'invalidation entre
  # create_posthoc_plot() qui l'ecrit et multiPlot qui le lit.
  .posthoc_msg_env <- new.env(parent = emptyenv())
  .posthoc_msg_env$msg <- NULL
  posthoc_plot_msg <- function(x) {
    if (missing(x)) return(.posthoc_msg_env$msg)
    .posthoc_msg_env$msg <- x
    invisible(x)
  }

  create_posthoc_plot <- reactive({
    
    plot_type <- input$plotType
    error_type <- input$errorType
    color_by_groups <- input$colorByGroups
    box_color <- input$boxColor
    rotate_labels <- input$rotateXLabels
    
    custom_title <- input$customTitle
    custom_subtitle <- input$customSubtitle
    custom_x_label <- input$customXLabel
    custom_y_label <- input$customYLabel
    custom_legend_title <- input$customLegendTitle
    
    title_size <- input$titleSize
    subtitle_size <- input$subtitleSize
    axis_title_size <- input$axisTitleSize
    axis_text_size <- input$axisTextSize
    graph_value_size <- input$graphValueSize
    mean_value_size <- input$meanValueSize  
    legend_title_size <- input$legendTitleSize
    legend_text_size <- input$legendTextSize
    legend_spacing <- input$legendSpacing
    
    title_font_style <- input$titleFontStyle
    subtitle_font_style <- input$subtitleFontStyle
    axis_title_font_style <- input$axisTitleFontStyle
    axis_text_x_font_style <- input$axisTextXFontStyle
    axis_text_y_font_style <- input$axisTextYFontStyle
    graph_value_font_style <- input$graphValueFontStyle
    legend_title_font_style <- input$legendTitleFontStyle
    legend_text_font_style <- input$legendTextFontStyle
    
    custom_axis_limits <- input$customAxisLimits
    y_axis_min <- input$yAxisMin
    y_axis_max <- input$yAxisMax
    x_axis_min <- input$xAxisMin
    x_axis_max <- input$xAxisMax
    custom_axis_breaks <- input$customAxisBreaks
    y_axis_break_step <- input$yAxisBreakStep
    x_axis_break_step <- input$xAxisBreakStep
    
    custom_x_order <- input$customXOrder
    custom_x_levels <- values$customXLevels()
    
    subtitle_position <- input$subtitlePosition
    
    req(values$multiResultsMain, input$multiResponse, input$multiFactor, values$filteredData)
    
    if (nrow(values$multiResultsMain) == 0 || length(input$multiResponse) == 0 || length(input$multiFactor) == 0) {
      return(NULL)
    }
    
    if (is.null(values$currentVarIndex)) values$currentVarIndex <- 1
    
    max_idx <- length(input$multiResponse)
    current_var_idx <- as.integer(values$currentVarIndex)
    
    if (is.na(current_var_idx) || current_var_idx < 1 || current_var_idx > max_idx) {
      current_var_idx <- 1
      values$currentVarIndex <- 1
    }
    
    resp_var <- tryCatch({
      input$multiResponse[[current_var_idx]]
    }, error = function(e) NULL)
    
    if (is.null(resp_var) || is.na(resp_var) || resp_var == "" || !resp_var %in% colnames(values$filteredData)) {
      return(NULL)
    }
    
    fvar <- NULL
    plot_data <- NULL
    agg <- NULL
    
    if (input$plotDisplayType == "main") {
      fvar <- tryCatch({
        input$multiFactor[[1]]
      }, error = function(e) NULL)
      
      if (is.null(fvar) || is.na(fvar) || !fvar %in% colnames(values$filteredData)) {
        return(NULL)
      }
      
      plot_data <- values$filteredData
      
      agg <- values$multiResultsMain[
        !is.na(values$multiResultsMain$Variable) &
          !is.na(values$multiResultsMain$Facteur) &
          !is.na(values$multiResultsMain$Type) &
          values$multiResultsMain$Variable == resp_var & 
          values$multiResultsMain$Facteur == fvar &
          values$multiResultsMain$Type == "main", 
      ]
      
      if (nrow(agg) == 0) return(NULL)
      
    } else {
      if (is.null(input$selectedSimpleEffect) || input$selectedSimpleEffect == "") {
        return(NULL)
      }
      
      parse_result <- tryCatch({
        parts <- strsplit(input$selectedSimpleEffect, " | ", fixed = TRUE)[[1]]
        if (length(parts) != 2) return(list(success = FALSE))
        
        main_factor <- trimws(parts[1])
        condition <- trimws(parts[2])
        cond_parts <- strsplit(condition, "=", fixed = TRUE)[[1]]
        if (length(cond_parts) != 2) return(list(success = FALSE))
        
        cond_factor <- trimws(cond_parts[1])
        cond_level <- trimws(cond_parts[2])
        
        if (!cond_factor %in% colnames(values$filteredData)) return(list(success = FALSE))
        
        filtered_data <- values$filteredData[values$filteredData[[cond_factor]] == cond_level, ]
        if (nrow(filtered_data) == 0) return(list(success = FALSE))
        
        agg_data <- values$multiResultsMain[
          !is.na(values$multiResultsMain$Variable) &
            !is.na(values$multiResultsMain$Facteur) &
            !is.na(values$multiResultsMain$Type) &
            values$multiResultsMain$Variable == resp_var & 
            values$multiResultsMain$Facteur == input$selectedSimpleEffect &
            values$multiResultsMain$Type == "simple_effect", 
        ]
        
        if (nrow(agg_data) == 0) return(list(success = FALSE))
        
        list(success = TRUE, plot_data = filtered_data, agg = agg_data, fvar = main_factor)
        
      }, error = function(e) list(success = FALSE))
      
      if (!parse_result$success) return(NULL)
      
      plot_data <- parse_result$plot_data
      agg <- parse_result$agg
      fvar <- parse_result$fvar
    }
    
    if (is.null(agg) || is.null(fvar) || is.null(plot_data) || nrow(agg) == 0 || nrow(plot_data) == 0) {
      posthoc_plot_msg("Aucune donnée à tracer pour cette sélection. Vérifiez que l'analyse a bien produit des résultats pour cette variable / cet effet simple.")
      return(NULL)
    }

    required_cols <- c(fvar, "Moyenne")
    if (!all(required_cols %in% colnames(agg))) {
      missing <- setdiff(c(fvar, "Moyenne", "Écart_type", "Erreur_type", "groups"), colnames(agg))
      posthoc_plot_msg(paste0(
        "Ce graphique ne peut pas être tracé : les résultats ne contiennent pas les colonnes attendues (",
        paste(missing, collapse = ", "),
        "). Cela arrive notamment pour les analyses multivariées (MANOVA / PERMANOVA), qui n'ont pas de moyennes par groupe à représenter : consultez les onglets de résultats correspondants."))
      return(NULL)
    }
    # Colonnes secondaires optionnelles : on les cree vides si absentes pour ne pas
    # bloquer le trace d'une ANOVA valide qui n'aurait pas tout fourni.
    if (!"Écart_type" %in% colnames(agg))  agg[["Écart_type"]]  <- NA_real_
    if (!"Erreur_type" %in% colnames(agg)) agg[["Erreur_type"]] <- NA_real_
    if (!"groups" %in% colnames(agg))      agg[["groups"]]      <- ""
    if (!fvar %in% colnames(plot_data) || !resp_var %in% colnames(plot_data)) {
      posthoc_plot_msg("Les colonnes nécessaires sont introuvables dans les données filtrées.")
      return(NULL)
    }
    posthoc_plot_msg(NULL)
    
    
    base_theme <- theme_minimal() +
      theme(
        plot.title = element_markdown(
          size = title_size, 
          face = title_font_style, 
          hjust = 0.5
        ),
        
        plot.subtitle = if (!is.null(custom_subtitle) && custom_subtitle != "") {
          element_text(
            size = if (!is.null(subtitle_size)) subtitle_size else 12,
            face = if (!is.null(subtitle_font_style)) subtitle_font_style else "italic",
            hjust = as.numeric(if (!is.null(subtitle_position)) subtitle_position else 0.5),
            color = "gray30",
            margin = margin(t = 5, b = 10)
          )
        } else {
          element_blank()
        },
        
        axis.title = element_markdown(
          size = axis_title_size, 
          face = axis_title_font_style
        ),
        axis.text.x = if (rotate_labels) {
          element_text(
            angle = 45, 
            hjust = 1, 
            size = axis_text_size, 
            face = axis_text_x_font_style
          )
        } else {
          element_text(
            size = axis_text_size, 
            face = axis_text_x_font_style
          )
        },
        axis.text.y = element_text(
          size = axis_text_size, 
          face = axis_text_y_font_style
        ),
        legend.position = if (color_by_groups) "right" else "none",
        legend.title = element_markdown(
          size = legend_title_size, 
          face = legend_title_font_style
        ),
        legend.text = element_text(
          size = legend_text_size, 
          face = legend_text_font_style
        ),
        legend.key.height = unit(1.2, "lines"),  
        legend.key.width = unit(1.5, "lines"),   
        legend.spacing.y = unit(legend_spacing, "lines"),  
        legend.key.spacing.y = unit(legend_spacing, "lines"),  
        legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.box.spacing = unit(0.5, "lines"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    
    plot_title <- if (!is.null(custom_title) && custom_title != "") {
      custom_title
    } else {
      if (input$plotDisplayType == "main") {
        paste("Effet principal:", resp_var, "par", fvar)
      } else {
        paste("Effet simple:", resp_var, "-", input$selectedSimpleEffect)
      }
    }
    
    base_labels <- labs(
      title = plot_title,
      subtitle = if (!is.null(custom_subtitle) && custom_subtitle != "") {
        custom_subtitle
      } else {
        NULL
      },
      x = if (!is.null(custom_x_label) && custom_x_label != "") custom_x_label else fvar,
      y = if (!is.null(custom_y_label) && custom_y_label != "") custom_y_label else resp_var
    )
    
    legend_title <- if (is.null(custom_legend_title) || custom_legend_title == "") {
      "Groupes statistiques"
    } else {
      custom_legend_title
    }
    
    p <- NULL
    
    tryCatch({
      if (!is.factor(plot_data[[fvar]])) plot_data[[fvar]] <- as.factor(plot_data[[fvar]])
      if (!is.factor(agg[[fvar]])) agg[[fvar]] <- as.factor(agg[[fvar]])
      
      common_levels <- intersect(levels(plot_data[[fvar]]), levels(agg[[fvar]]))
      if (length(common_levels) == 0) return(NULL)
      
      plot_data[[fvar]] <- factor(plot_data[[fvar]], levels = common_levels)
      agg[[fvar]] <- factor(agg[[fvar]], levels = common_levels)
      
      
      if (!is.null(custom_x_order) && custom_x_order && !is.null(custom_x_levels)) {
        custom_order <- custom_x_levels
        
        if (all(common_levels %in% custom_order)) {
          plot_data[[fvar]] <- factor(plot_data[[fvar]], levels = custom_order)
          agg[[fvar]] <- factor(agg[[fvar]], levels = custom_order)
          common_levels <- custom_order
        }
      }
      
      # Créer des colonnes avec noms simples pour éviter les problèmes plotly
      plot_data$x_var <- plot_data[[fvar]]
      plot_data$y_var <- plot_data[[resp_var]]
      agg$x_var <- agg[[fvar]]
      
      y_max <- max(plot_data$y_var, na.rm = TRUE)
      y_min <- min(plot_data$y_var, na.rm = TRUE)
      y_range <- y_max - y_min
      
      # Valeurs par défaut sécurisées pour la taille et le style
      
      # Valeurs par défaut si les inputs sont NULL ou non définis
      safe_graph_value_size <- if (!is.null(graph_value_size) && !is.na(graph_value_size)) {
        graph_value_size
      } else {
        5  # Valeur par défaut
      }
      
      safe_graph_value_font_style <- if (!is.null(graph_value_font_style) && graph_value_font_style != "") {
        graph_value_font_style
      } else {
        "bold"  # Valeur par défaut
      }
      
      safe_mean_value_size <- if (!is.null(mean_value_size) && !is.na(mean_value_size)) {
        mean_value_size
      } else {
        4  # Valeur par défaut
      }
      
      
      if (plot_type == "box") {
        if (color_by_groups) {
          agg_subset <- agg[, c(fvar, "groups"), drop = FALSE]
          names(agg_subset)[1] <- "x_var"
          plot_data_merged <- merge(plot_data, agg_subset, by = "x_var", all.x = TRUE)
          
          p <- ggplot(plot_data_merged, aes(x = x_var, y = y_var, fill = groups)) +
            geom_boxplot(alpha = 0.7) +
            scale_fill_discrete(name = legend_title)
        } else {
          p <- ggplot(plot_data, aes(x = x_var, y = y_var, fill = x_var)) +
            geom_boxplot(alpha = 0.7) +
            annotate("text", 
                     x = seq_len(nrow(agg)),   # seq_len : 1:0 rendrait c(1, 0)
                     y = y_max + y_range * 0.05, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
      } else if (plot_type == "violin") {
        if (color_by_groups) {
          agg_subset <- agg[, c(fvar, "groups"), drop = FALSE]
          names(agg_subset)[1] <- "x_var"
          plot_data_merged <- merge(plot_data, agg_subset, by = "x_var", all.x = TRUE)
          
          p <- ggplot(plot_data_merged, aes(x = x_var, y = y_var, fill = groups)) +
            geom_violin(alpha = 0.7) +
            geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
            scale_fill_discrete(name = legend_title)
        } else {
          p <- ggplot(plot_data, aes(x = x_var, y = y_var, fill = x_var)) +
            geom_violin(alpha = 0.7) +
            geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
            annotate("text", 
                     x = seq_len(nrow(agg)),   # seq_len : 1:0 rendrait c(1, 0)
                     y = y_max + y_range * 0.05, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
      } else if (plot_type == "point") {
        error_val <- if (error_type == "se") agg$Erreur_type
        else if (error_type == "sd") agg$Ecart_type
        else if (error_type == "ci") 1.96 * agg$Erreur_type
        else 0
        
        agg$y_err_max <- agg$Moyenne + error_val
        y_text_pos <- max(agg$y_err_max, na.rm = TRUE) * 1.05
        
        if (color_by_groups) {
          p <- ggplot(agg, aes(x = x_var, y = Moyenne, fill = groups, color = groups)) +
            geom_point(size = 4, shape = 21, stroke = 2) +
            scale_fill_discrete(name = legend_title) +
            scale_color_discrete(name = legend_title)
        } else {
          p <- ggplot(agg, aes(x = x_var, y = Moyenne, fill = x_var, color = x_var)) +
            geom_point(size = 4, shape = 21, stroke = 2) +
            annotate("text", 
                     x = seq_len(nrow(agg)),   # seq_len : 1:0 rendrait c(1, 0)
                     y = y_text_pos, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
        if (error_type == "se") {
          p <- p + geom_errorbar(aes(ymin = Moyenne - Erreur_type, ymax = Moyenne + Erreur_type), 
                                 width = 0.2, color = "black")
        } else if (error_type == "sd") {
          p <- p + geom_errorbar(aes(ymin = Moyenne - Ecart_type, ymax = Moyenne + Ecart_type), 
                                 width = 0.2, color = "black")
        } else if (error_type == "ci") {
          agg$ci_margin <- 1.96 * agg$Erreur_type
          p <- p + geom_errorbar(aes(ymin = Moyenne - ci_margin, ymax = Moyenne + ci_margin), 
                                 width = 0.2, color = "black")
        }
        
      } else if (plot_type == "hist") {
        if (color_by_groups) {
          p <- ggplot(agg, aes(x = x_var, y = Moyenne, fill = groups)) +
            geom_col(alpha = 0.7, color = "black") +
            scale_fill_discrete(name = legend_title)
        } else {
          p <- ggplot(agg, aes(x = x_var, y = Moyenne, fill = x_var)) +
            geom_col(alpha = 0.7, color = "black") +
            annotate("text", 
                     x = seq_len(nrow(agg)),   # seq_len : 1:0 rendrait c(1, 0)
                     y = agg$Moyenne * 0.8, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
        p <- p + geom_text(aes(y = Moyenne/2, label = round(Moyenne, 2)),
                           size = safe_mean_value_size,  # Taille personnalisable
                           fontface = "bold", color = "white")
        
        if (error_type != "none") {
          if (error_type == "se") {
            p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + Erreur_type), 
                                   width = 0.2, color = "black")
          } else if (error_type == "sd") {
            p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + Ecart_type), 
                                   width = 0.2, color = "black")
          } else if (error_type == "ci") {
            agg$ci_margin <- 1.96 * agg$Erreur_type
            p <- p + geom_errorbar(aes(ymin = Moyenne, ymax = Moyenne + ci_margin), 
                                   width = 0.2, color = "black")
          }
        }
      }
      
      p <- p + base_theme + base_labels
      
      if (!is.null(p) && box_color != "default" && !color_by_groups) {
        p <- p + scale_fill_brewer(palette = box_color) +
          scale_color_brewer(palette = box_color)
      }
      
      
      if (!is.null(p) && !is.null(custom_axis_limits) && custom_axis_limits) {
        ylim_min <- if (!is.null(y_axis_min) && !is.na(y_axis_min)) y_axis_min else NA
        ylim_max <- if (!is.null(y_axis_max) && !is.na(y_axis_max)) y_axis_max else NA
        xlim_min <- if (!is.null(x_axis_min) && !is.na(x_axis_min)) x_axis_min else NA
        xlim_max <- if (!is.null(x_axis_max) && !is.na(x_axis_max)) x_axis_max else NA
        
        if (!is.na(ylim_min) || !is.na(ylim_max) || !is.na(xlim_min) || !is.na(xlim_max)) {
          p <- p + coord_cartesian(
            xlim = if (!is.na(xlim_min) || !is.na(xlim_max)) c(xlim_min, xlim_max) else NULL,
            ylim = if (!is.na(ylim_min) || !is.na(ylim_max)) c(ylim_min, ylim_max) else NULL,
            expand = TRUE
          )
        }
      }
      
      
      if (!is.null(p) && !is.null(custom_axis_breaks) && custom_axis_breaks) {
        
        if (!is.null(y_axis_break_step) && !is.na(y_axis_break_step) && y_axis_break_step > 0) {
          y_data_min <- min(plot_data$y_var, na.rm = TRUE)
          y_data_max <- max(plot_data$y_var, na.rm = TRUE)
          
          if (!is.null(custom_axis_limits) && custom_axis_limits) {
            if (!is.null(y_axis_min) && !is.na(y_axis_min)) y_data_min <- y_axis_min
            if (!is.null(y_axis_max) && !is.na(y_axis_max)) y_data_max <- y_axis_max
          }
          
          y_breaks <- seq(
            from = floor(y_data_min / y_axis_break_step) * y_axis_break_step,
            to = ceiling(y_data_max / y_axis_break_step) * y_axis_break_step,
            by = y_axis_break_step
          )
          
          p <- p + scale_y_continuous(breaks = y_breaks)
        }
        
        if (!is.null(x_axis_break_step) && !is.na(x_axis_break_step) && x_axis_break_step > 0) {
          if (is.numeric(plot_data$x_var)) {
            x_data_min <- min(plot_data$x_var, na.rm = TRUE)
            x_data_max <- max(plot_data$x_var, na.rm = TRUE)
            
            if (!is.null(custom_axis_limits) && custom_axis_limits) {
              if (!is.null(x_axis_min) && !is.na(x_axis_min)) x_data_min <- x_axis_min
              if (!is.null(x_axis_max) && !is.na(x_axis_max)) x_data_max <- x_axis_max
            }
            
            x_breaks <- seq(
              from = floor(x_data_min / x_axis_break_step) * x_axis_break_step,
              to = ceiling(x_data_max / x_axis_break_step) * x_axis_break_step,
              by = x_axis_break_step
            )
            
            p <- p + scale_x_continuous(breaks = x_breaks)
          }
        }
      }
      
      if (!is.null(p)) {
        values$currentPlot <- p
        return(p)
      }
      
    }, error = function(e) {
      showNotification(hstat_err_fr(e, "Erreur graphique"), type = "error", duration = 10)
      return(NULL)
    })
    
    return(NULL)
  })
  
  output$multiPlot <- renderPlotly({
    req(values$filteredData)
    p <- create_posthoc_plot()

    # Pas de graphique encore disponible : on affiche un message clair plutôt qu'une
    # zone blanche. Si la fonction a attaché une raison précise (attribut
    # posthoc_msg), on l'affiche ; sinon, message générique d'invite.
    if (is.null(p)) {
      reason <- posthoc_plot_msg()
      lbl <- if (!is.null(reason)) reason
             else "Configurez la méthode puis cliquez sur « Lancer l'analyse »\npour afficher le graphique."
      placeholder <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = lbl,
          size = 4.5, colour = "#95a5a6") +
        ggplot2::theme_void()
      return(suppressWarnings(suppressMessages(ggplotly(placeholder))) %>% layout(showlegend = FALSE))
    }

    tryCatch({
      gp <- ggplotly(p) %>%
        layout(showlegend = if (isTRUE(input$colorByGroups)) TRUE else FALSE)
      # NB : pas de boxmode = "group" ici -- le post-hoc n'a jamais plusieurs
      # boites par categorie x, et ce mode decalerait les boites de leur axe.
      gp %>% config(displaylogo = FALSE)
    }, error = function(e_plotly) {
      # Repli : si la conversion plotly echoue, on renvoie tout de meme un objet
      # plotly valide (jamais un renderPlot, qui serait invalide ici et laisserait
      # la zone vide). On convertit l'image en plotly minimal porteur d'un message.
      msg <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, size = 4.5, colour = "#7f8c8d",
          label = "Le graphique n'a pas pu être rendu en interactif.\nUtilisez le téléchargement pour l'image haute qualité.") +
        ggplot2::theme_void()
      suppressWarnings(suppressMessages(ggplotly(msg))) %>% layout(showlegend = FALSE)
    })
  })
  
  output$plotTitle <- renderUI({
    req(values$filteredData)
    if (is.null(input$multiResponse) || length(input$multiResponse) == 0) {
      return("Visualisations")
    }
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
    }
    
    current_var_idx <- values$currentVarIndex
    max_idx <- length(input$multiResponse)
    
    if (current_var_idx < 1 || current_var_idx > max_idx) {
      current_var_idx <- 1
      values$currentVarIndex <- 1
    }
    
    current_var <- tryCatch({
      input$multiResponse[[current_var_idx]]
    }, error = function(e) {
      return("Variable")
    })
    
    if (is.null(current_var) || is.na(current_var) || current_var == "") {
      current_var <- "Variable"
    }
    
    type_text <- if (!is.null(input$plotDisplayType) && input$plotDisplayType == "simple") {
      "Effets simples"
    } else {
      "Effets principaux"
    }
    
    tags$span(type_text, " - ", current_var)
  })
  
  output$downloadMainEffects <- downloadHandler(
    filename = function() { paste0("effets_principaux_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(values$multiResultsMain)
      main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Effets_principaux")
      openxlsx::writeData(wb, "Effets_principaux", main_data)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadSimpleEffects <- downloadHandler(
    filename = function() { paste0("effets_simples_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(values$multiResultsMain)
      simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Effets_simples")
      openxlsx::writeData(wb, "Effets_simples", simple_data)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadAllResults <- downloadHandler(
    filename = function() { paste0("analyse_complete_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(values$multiResultsMain)
      wb <- openxlsx::createWorkbook()
      
      main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
      simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
      
      if (nrow(main_data) > 0) {
        openxlsx::addWorksheet(wb, "Effets_principaux")
        openxlsx::writeData(wb, "Effets_principaux", main_data)
      }
      
      if (nrow(simple_data) > 0) {
        openxlsx::addWorksheet(wb, "Effets_simples")
        openxlsx::writeData(wb, "Effets_simples", simple_data)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadMultiPlot <- downloadHandler(
    filename = function() {
      fmt <- input$multiPlotFormat %||% "png"
      paste0("graphique_posthoc_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      # On s'assure d'avoir un graphique : si l'utilisateur n'a pas (re)lancé
      # l'analyse, currentPlot peut être NULL. On tente de le (re)construire à la
      # volée. Si c'est impossible, on AVERTIT et on génère malgré tout un fichier
      # valide (au bon format) contenant un message, pour ne JAMAIS renvoyer une
      # page HTML d'erreur que le navigateur enregistrerait en « .htm ».
      p <- values$currentPlot
      if (is.null(p)) p <- tryCatch(create_posthoc_plot(), error = function(e) NULL)
      # Capuchons de moustache sur le fichier exporte (l'apercu plotly les
      # dessine deja ; le ggplot brut non). Idempotent.
      if (inherits(p, "ggplot")) p <- hstat_add_whisker_caps(p)

      fmt <- input$multiPlotFormat %||% "png"
      w   <- .hstat_num1(input$plotWidth, 8)
      h   <- .hstat_num1(input$plotHeight, 6)
      dpi <- .hstat_num1(input$plotDPIVisible %||% input$plotDPI, 300)

      if (is.null(p)) {
        showNotification(
          "Aucun graphique à exporter : lancez d'abord l'analyse, puis réessayez.",
          type = "warning", duration = 6)
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0,
            label = "Aucun graphique disponible.\nLancez l'analyse puis téléchargez.",
            size = 6, colour = "#7f8c8d") +
          ggplot2::theme_void()
      }

      # Correction : avec un device PERSONNALISE, ggsave transmet width/height
      # tels quels et png()/jpeg()/tiff() les interpretent en PIXELS par defaut
      # (le fichier sortait en 8x6 px). On force units = "in" et res = dpi.
      device <- switch(fmt,
        png  = function(filename, ...) grDevices::png(filename, type = "cairo",
                                                      units = "in", res = dpi, ...),
        jpeg = function(filename, ...) grDevices::jpeg(filename, type = "cairo", quality = 95,
                                                       units = "in", res = dpi, ...),
        tiff = function(filename, ...) grDevices::tiff(filename, type = "cairo", compression = "lzw",
                                                       units = "in", res = dpi, ...),
        pdf  = grDevices::cairo_pdf,
        svg  = grDevices::svg,
        function(filename, ...) grDevices::png(filename, type = "cairo",
                                               units = "in", res = dpi, ...))

      ok <- tryCatch({
        if (fmt %in% c("pdf", "svg")) {
          ggplot2::ggsave(file, plot = p, width = w, height = h, units = "in", device = device)
        } else {
          ggplot2::ggsave(file, plot = p, width = w, height = h, units = "in",
                          dpi = dpi, device = device)
        }
        TRUE
      }, error = function(e) {
        showNotification(hstat_err_fr(e, "Échec de l'export"), type = "error", duration = 8)
        FALSE
      })

      if (ok) {
        showNotification(trf("Graphique téléchargé (%s, %d DPI).", toupper(fmt), as.integer(dpi)),
                         type = "message", duration = 3)
      }
    }
  )
  
  output$downloadSummaryStats <- downloadHandler(
    filename = function() { paste0("statistiques_resumees_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(values$multiResultsMain)
      
      summary_stats <- values$multiResultsMain %>%
        group_by(Variable, Facteur, Type) %>%
        summarise(
          Nb_groupes = n(),
          Moyenne_generale = round(mean(Moyenne, na.rm = TRUE), 2),
          CV_moyen = round(mean(CV, na.rm = TRUE), 2),
          Ecart_type_moyen = round(mean(Ecart_type, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      var_summary <- values$multiResultsMain %>%
        group_by(Variable) %>%
        summarise(
          Nb_facteurs_testes = n_distinct(Facteur),
          Nb_total_groupes = n(),
          Moyenne_min = round(min(Moyenne, na.rm = TRUE), 2),
          Moyenne_max = round(max(Moyenne, na.rm = TRUE), 2),
          CV_min = round(min(CV, na.rm = TRUE), 2),
          CV_max = round(max(CV, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Resume_par_facteur")
      openxlsx::writeData(wb, "Resume_par_facteur", summary_stats)
      openxlsx::addWorksheet(wb, "Resume_par_variable")
      openxlsx::writeData(wb, "Resume_par_variable", var_summary)
      openxlsx::addWorksheet(wb, "Données_completes")
      openxlsx::writeData(wb, "Données_completes", values$multiResultsMain)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadFullReport <- downloadHandler(
    filename = function() { paste0("rapport_complet_", Sys.Date(), ".pdf") },
    content = function(file) {
      showNotification("Génération du PDF - Fonctionnalité nécessite rmarkdown", 
                       type = "warning", duration = 5)
    }
  )
  })
}
