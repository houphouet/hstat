#  Module Shiny : Tests statistiques + Comparaisons post-hoc (combine)

mod_tests_ui <- function(id) {
  ns <- shiny::NS(id)
      shinydashboard::tabItem(tabName = "tests",
                .hstat_scope_banner(exact = FALSE),
                shiny::fluidRow(
                  shinydashboard::box(title = "Paramètres des tests", status = "danger", width = 12, solidHeader = TRUE,
                      shiny::fluidRow(
                        shiny::column(4,
                               shiny::uiOutput(ns("responseVarSelect")),
                               shiny::uiOutput(ns("factorVarSelect")),
                               shiny::checkboxInput(ns("interaction"), "Inclure les interactions (ANOVA/Scheirer-Ray-Hare)", FALSE),
                               shiny::hr(),
                               shiny::div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px;",
                                   shiny::fluidRow(
                                     shiny::column(6,
                                            shiny::checkboxInput(ns("testsRoundResults"), "Arrondir les résultats", value = FALSE)
                                     ),
                                     shiny::column(6,
                                            shiny::conditionalPanel(
                                              ns = ns,
                                              condition = "input.testsRoundResults == true",
                                              shiny::numericInput(ns("testsDecimals"), "Décimales :", value = 2, min = 0, max = 8, step = 1)
                                            )
                                     )
                                   )
                               ),
                               # --- Configuration du modele (generalise) mixte ---
                               shiny::div(style = "background-color:#e8f6f3; border-left:4px solid #16a085; padding:10px; margin-top:12px;",
                                   shiny::h6(shiny::tagList(shiny::icon("sitemap"), " Modèle mixte (GLMM)"),
                                      style = "color:#6c3483; margin-top:0; font-weight:bold;"),
                                   shiny::div(style = "font-size:11px; color:#6c3483; margin-bottom:8px;",
                                       "Paramètres utilisés par le bouton « Modèle (généralisé) mixte »."),
                                   shiny::fluidRow(
                                     shiny::column(6,
                                            shiny::selectInput(ns("glmmEngine"), "Moteur :",
                                                        choices = c("lme4 (g/lmer)" = "lme4",
                                                                    "glmmTMB"       = "glmmTMB"),
                                                        selected = "lme4")
                                     ),
                                     shiny::column(6,
                                            shiny::selectInput(ns("glmmFamily"), "Famille :",
                                                        choices = c("Gaussienne"          = "gaussian",
                                                                    "Binomiale"           = "binomial",
                                                                    "Poisson"             = "poisson",
                                                                    "Gamma"               = "Gamma",
                                                                    "Binomiale négative"  = "nbinom",
                                                                    "Inverse gaussienne"  = "inverse.gaussian",
                                                                    "Bêta (glmmTMB)"      = "beta_family",
                                                                    "Tweedie (glmmTMB)"   = "tweedie"),
                                                        selected = "gaussian")
                                     )
                                   ),
                                   shiny::selectInput(ns("glmmLink"), "Fonction de lien :",
                                               choices = c("Automatique (lien canonique)" = "auto",
                                                           "identity" = "identity", "log" = "log",
                                                           "logit"    = "logit",    "probit" = "probit",
                                                           "cloglog"  = "cloglog",  "inverse" = "inverse",
                                                           "sqrt"     = "sqrt"),
                                               selected = "auto"),
                                   shiny::uiOutput(ns("glmmFamilyHelp")),
                                   shiny::uiOutput(ns("glmmLinkHelp")),
                                   shiny::uiOutput(ns("glmmRandomSelect")),
                                   shiny::div(style = "font-size:10px; color:#7f8c8d; margin-top:4px;",
                                       shiny::icon("circle-info"),
                                       " Effet aléatoire d'ordonnée à l'origine : ", shiny::tags$code("(1 | groupe)"), ".")
                               ),
                               # --- Configuration ANOVA a mesures repetees ---
                               shiny::div(style = "background-color:#e0f2f1; border-left:4px solid #00897b; padding:10px; margin-top:12px;",
                                   shiny::h6(shiny::tagList(shiny::icon("repeat"), " Mesures répétées (rmANOVA & non param.)"),
                                      style = "color:#00695c; margin-top:0; font-weight:bold;"),
                                   shiny::div(style = "font-size:11px; color:#00695c; margin-bottom:8px;",
                                       "Paramètres des boutons « ANOVA à mesures répétées » et « Non paramétrique répété »."),
                                   shiny::selectInput(ns("rmSubject"),
                                               shiny::tagList(shiny::icon("user"), " Sujet / identifiant :"),
                                               choices = NULL),
                                   shiny::selectizeInput(ns("rmWithin"),
                                                  shiny::tagList(shiny::icon("clock"), shiny::HTML(" <b>Période</b> (facteur intra-sujet, répété) :")),
                                                  choices = NULL, multiple = TRUE,
                                                  options = list(plugins = list("remove_button"),
                                                                 placeholder = "Ex. : temps, date, stade...")),
                                   shiny::selectizeInput(ns("rmBetween"),
                                                  shiny::tagList(shiny::icon("flask"), shiny::HTML(" <b>Traitement</b> (facteur inter-sujet, optionnel) :")),
                                                  choices = NULL, multiple = TRUE,
                                                  options = list(plugins = list("remove_button"),
                                                                 placeholder = "Ex. : traitement, groupe...")),
                                   shiny::div(style = "font-size:10px; color:#7f8c8d; margin:-2px 0 6px 0;",
                                       shiny::icon("circle-info"),
                                       shiny::HTML(" <b>Sujet</b> = unité mesurée plusieurs fois (ex. plante, parcelle). <b>Période</b> = facteur de temps répété sur le même sujet (intra-sujet). <b>Traitement</b> = facteur appliqué (souvent inter-sujet ; mettez-le en intra s'il varie au sein d'un même sujet).")),
                                   shiny::selectInput(ns("rmEngine"), "Moteur (paramétrique) :",
                                               choices = c("Modèle mixte (lmer)" = "mixed",
                                                           "afex (aov classique)" = "afex"),
                                               selected = "mixed"),
                                   shiny::selectInput(ns("rmNonParam"), "Test non paramétrique :",
                                               choices = c("Friedman (1 facteur intra)"      = "friedman",
                                                           "Durbin (plans incomplets)"       = "durbin",
                                                           "ART (rangs alignés, factoriel)"  = "art"),
                                               selected = "friedman"),
                                   shiny::selectInput(ns("rmPostHocAdjust"), "Ajustement post-hoc :",
                                               choices = c("Holm" = "holm", "Bonferroni" = "bonferroni",
                                                           "BH (FDR)" = "BH", "Tukey" = "tukey", "Aucun" = "none"),
                                               selected = "holm")
                               )
                        ),
                        shiny::column(4,
                               shiny::h4("Tests sur données brutes", style = "color: #3c8dbc;"),
                               shiny::div(style="display:flex; flex-direction:column; gap:8px; margin-bottom:12px;",
                                   shiny::actionButton(ns("testNormalityRaw"),   "Test de normalité",     class = "btn-warning btn-block", icon = shiny::icon("chart-line")),
                                   shiny::actionButton(ns("testHomogeneityRaw"), "Test d'homogénéité",    class = "btn-warning btn-block", icon = shiny::icon("balance-scale"))
                               ),
                               shiny::h4("Tests paramétriques", style = "color: #00a65a;"),
                               shiny::div(style="display:flex; flex-direction:column; gap:8px;",
                                   shiny::actionButton(ns("testT"),    "Test t de Student",           class = "btn-success btn-block", icon = shiny::icon("check")),
                                   shiny::actionButton(ns("testANOVA"),"ANOVA",                        class = "btn-success btn-block", icon = shiny::icon("check")),
                                   shiny::actionButton(ns("testMANOVA"),"MANOVA (>= 2 réponses)",     class = "btn-success btn-block", icon = shiny::icon("layer-group")),
                                   shiny::actionButton(ns("testLM"),   "Régression linéaire",          class = "btn-success btn-block", icon = shiny::icon("check")),
                                   shiny::actionButton(ns("testGLM"),  "Modèle linéaire généralisé",   class = "btn-success btn-block", icon = shiny::icon("check")),
                                   shiny::actionButton(ns("testGLMM"), "Modèle (généralisé) mixte",    class = "btn-success btn-block", icon = shiny::icon("sitemap")),
                                   shiny::actionButton(ns("testRMAnova"), "ANOVA à mesures répétées",   class = "btn-success btn-block", icon = shiny::icon("repeat"))
                               )
                        ),
                        shiny::column(4,
                               shiny::h4("Tests non-paramétriques", style = "color: #f39c12;"),
                               shiny::div(style="display:flex; flex-direction:column; gap:8px;",
                                   shiny::actionButton(ns("testWilcox"),          "Test de Wilcoxon",          class = "btn-warning btn-block", icon = shiny::icon("check")),
                                   shiny::actionButton(ns("testKruskal"),         "Test de Kruskal-Wallis",     class = "btn-warning btn-block", icon = shiny::icon("check")),
                                   shiny::actionButton(ns("testScheirerRayHare"), "Test de Scheirer-Ray-Hare",  class = "btn-warning btn-block", icon = shiny::icon("check")),
                                   shiny::actionButton(ns("testRMNonParam"), "Non paramétrique répété",  class = "btn-warning btn-block", icon = shiny::icon("repeat")),
                                   shiny::actionButton(ns("testPERMANOVA"),       "PERMANOVA (>= 2 réponses)",  class = "btn-warning btn-block", icon = shiny::icon("layer-group"))
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
                               shiny::div(style = "border-left:4px solid #6a1b9a; padding-left:10px; margin-top:16px;",
                                   shiny::h4(shiny::tagList(shiny::icon("bullseye"), " Comparaison à une norme"),
                                      style = "color:#6a1b9a;"),
                                   shiny::div(style = "font-size:11px; color:#7f8c8d; margin:-4px 0 8px 0;",
                                       "Confronte les variables réponse à une valeur de référence (réglages ci-dessous)."),
                                   shiny::div(style = "display:flex; flex-direction:column; gap:8px;",
                                       shiny::actionButton(ns("testRefT"), "Test t (1 échantillon)",
                                                    class = "btn-block", icon = shiny::icon("bullseye"),
                                                    style = "background:#6a1b9a; color:#fff; border-color:#59167f;"),
                                       shiny::actionButton(ns("testRefZ"), "Test z (écart-type connu)",
                                                    class = "btn-block", icon = shiny::icon("bullseye"),
                                                    style = "background:#6a1b9a; color:#fff; border-color:#59167f;"),
                                       shiny::actionButton(ns("testRefTOST"), "Équivalence à la norme (TOST)",
                                                    class = "btn-block", icon = shiny::icon("arrows-left-right"),
                                                    style = "background:#6a1b9a; color:#fff; border-color:#59167f;"),
                                       shiny::actionButton(ns("testRefVar"), "Chi² de conformité (variance)",
                                                    class = "btn-block", icon = shiny::icon("wave-square"),
                                                    style = "background:#6a1b9a; color:#fff; border-color:#59167f;"),
                                       shiny::actionButton(ns("testRefWilcox"), "Wilcoxon signé (médiane)",
                                                    class = "btn-block", icon = shiny::icon("bullseye"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;"),
                                       shiny::actionButton(ns("testRefSign"), "Test du signe (médiane)",
                                                    class = "btn-block", icon = shiny::icon("bullseye"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;"))),

                               # --- Reglages de la comparaison a une norme --------------------
                               # Places SOUS les boutons ci-dessus : meme accent violet, meme
                               # colonne, l'ensemble se lit d'un seul tenant.
                               shiny::div(style = "border-left:4px solid #6a1b9a; padding-left:10px; margin-top:12px;",
                                   shiny::h4(shiny::tagList(shiny::icon("sliders-h"), " Réglages de la norme"),
                                      style = "color:#6a1b9a;"),
                                   shiny::numericInput(ns("refValue"),
                                                shiny::tagList(shiny::icon("bullseye"), " Valeur de référence :"),
                                                value = 0),
                                   shiny::fluidRow(
                                     shiny::column(6, shiny::numericInput(ns("refSigma"),
                                                shiny::tagList(shiny::icon("wave-square"), " Écart-type (test z / variance)"),
                                                value = NULL, min = 0)),
                                     shiny::column(6, shiny::numericInput(ns("refMargin"),
                                                shiny::tagList(shiny::icon("arrows-left-right"), " Marge (TOST)"),
                                                value = NULL, min = 0))),
                                   shiny::selectInput(ns("refAlt"), "Hypothèse alternative :",
                                               choices = c("Bilatérale (différente de la norme)" = "two.sided",
                                                           "Unilatérale : supérieure à la norme" = "greater",
                                                           "Unilatérale : inférieure à la norme" = "less"),
                                               selected = "two.sided"),
                                   shiny::sliderInput(ns("refConf"), "Niveau de confiance :",
                                               min = 0.80, max = 0.99, value = 0.95, step = 0.01),
                                   shiny::div(style = "font-size:11px; color:#7f8c8d; margin-top:-4px;",
                                       shiny::icon("circle-info"),
                                       paste(" L'écart-type ne sert qu'au test z et au Chi² de variance ;",
                                             "la marge, qu'au test d'équivalence.")),
                                   shiny::tags$hr(style = "margin:10px 0;"),
                                   shiny::h5(shiny::tagList(shiny::icon("percent"), " Proportion / taux vs norme"),
                                      style = "color:#6a1b9a; font-weight:bold;"),
                                   shiny::uiOutput(ns("refPropVarSelect")),
                                   shiny::uiOutput(ns("refPropLevelSelect")),
                                   shiny::numericInput(ns("refPropP0"),
                                                "Proportion (ou taux) de référence :",
                                                value = 0.5, min = 0, step = 0.01),
                                   shiny::div(style = "display:flex; flex-direction:column; gap:8px;",
                                       shiny::actionButton(ns("testRefBinom"), "Test binomial exact",
                                                    class = "btn-block", icon = shiny::icon("percent"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;"),
                                       shiny::actionButton(ns("testRefProp"), "Chi² de conformité (proportion)",
                                                    class = "btn-block", icon = shiny::icon("percent"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;"),
                                       shiny::actionButton(ns("testRefPoisson"), "Test de Poisson (taux)",
                                                    class = "btn-block", icon = shiny::icon("percent"),
                                                    style = "background:#8e44ad; color:#fff; border-color:#76398f;")),
                                   shiny::div(style = "font-size:11px; color:#7f8c8d; margin-top:6px;",
                                       shiny::icon("circle-info"),
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
                shiny::conditionalPanel(
                  ns = ns,
                  condition = "output.hasRefTest",
                  shiny::fluidRow(
                    shinydashboard::box(title = shiny::tagList(shiny::icon("bullseye"),
                                        " Détail de la comparaison à la référence"),
                        status = "primary", width = 12, solidHeader = TRUE,
                        collapsible = TRUE,
                        DT::DTOutput(ns("refTestDetails")),
                        shiny::uiOutput(ns("refTestNote")))
                  )
                ),

                # BOX MANOVA / PERMANOVA -- Assistant guide visible apres execution

                shiny::conditionalPanel(
                  ns = ns,
                  condition = "output.showManovaWorkflow",
                  shiny::fluidRow(
                    shiny::div(id = "boxWrap_manovaAssist",
                        shinydashboard::box(
                          title = shiny::tagList(shiny::icon("layer-group"),
                                          " Analyse multivariée assistée (MANOVA / PERMANOVA)"),
                          status = "success", width = 12, solidHeader = TRUE,
                          collapsible = TRUE, collapsed = TRUE,
                        
                          shiny::tabsetPanel(id = "manovaAssistantTabs", type = "tabs",
                                    
                                      shiny::tabPanel(
                                        title = shiny::tagList(shiny::icon("magic"), " 1. Diagnostic & recommandation"),
                                        value = "manova_recommendation", shiny::br(),
                                      
                                        shiny::conditionalPanel(
                                          ns = ns,
                                          condition = "output.hasManovaRecommendation",
                                          shiny::uiOutput(ns("manovaRecommendationCard")),
                                          shiny::uiOutput(ns("manovaOutliersCard")),
                                          shiny::br()
                                        ),
                                        shiny::conditionalPanel(
                                          ns = ns,
                                          condition = "!output.hasManovaRecommendation",
                                          shiny::div(style = "padding:30px; text-align:center; color:#888;",
                                              shiny::icon("magic", style = "font-size:48px; opacity:0.3;"),
                                              shiny::h4("Aucune recommandation calculée"),
                                              shiny::p("Cliquez sur ", shiny::strong("'Diagnostiquer mes données'"),
                                                " ci-dessous pour obtenir une recommandation automatique."))
                                        )
                                      ),
                                    
                                      shiny::tabPanel(
                                        title = shiny::tagList(shiny::icon("clipboard-check"), " 2. Détails techniques"),
                                        value = "manova_prereq", shiny::br(),
                                      
                                        shiny::div(style = "background:#fff8e1; border-left:4px solid #fb8c00; padding:10px 14px; border-radius:6px; margin-bottom:12px; font-size:12px;",
                                            shiny::icon("info-circle", style = "color:#e65100;"),
                                            shiny::strong(" Pour les utilisateurs avancés : "),
                                            "consultez les valeurs brutes des tests de prérequis. ",
                                            "L'assistant a déjà synthétisé ces résultats dans l'onglet 'Diagnostic & recommandation'."
                                        ),
                                      
                                        shiny::conditionalPanel(
                                          ns = ns,
                                          condition = "output.hasManovaParam",
                                          shiny::h5(shiny::icon("table"), " 4 statistiques MANOVA",
                                             style = "color:#00a65a; margin-top:0;"),
                                          withSpinner(DT::DTOutput(ns("manovaParamTable")), color = "#00a65a"),
                                          shiny::br()
                                        ),
                                      
                                        shiny::conditionalPanel(
                                          ns = ns,
                                          condition = "output.hasManovaPermanova",
                                          shiny::h5(shiny::icon("random"), " Résultats PERMANOVA (par permutations)",
                                             style = "color:#f39c12; margin-top:0;"),
                                          shiny::div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                                              shiny::icon("info-circle"),
                                              " pseudo-F, R² (part de variance expliquée), p-value par permutations. Interactions incluses si l'option est cochée."),
                                          withSpinner(DT::DTOutput(ns("manovaPermanovaTable")), color = "#f39c12"),
                                          shiny::br()
                                        ),
                                      
                                        shiny::h5(shiny::icon("chart-area"), " Normalité multivariée (Mardia)",
                                           style = "color:#1565C0; margin-top:0;"),
                                        withSpinner(DT::DTOutput(ns("manovaMardiaTable")), color = "#1565C0"),
                                        shiny::uiOutput(ns("manovaMardiaInterpretation")),
                                        shiny::br(),
                                      
                                        shiny::h5(shiny::icon("balance-scale"), " Homogénéité des covariances (Box\'s M)",
                                           style = "color:#1565C0;"),
                                        withSpinner(DT::DTOutput(ns("manovaBoxMTable")), color = "#1565C0"),
                                        shiny::uiOutput(ns("manovaBoxMInterpretation")),
                                        shiny::br(),
                                      
                                        shiny::h5(shiny::icon("project-diagram"), " Homogénéité des dispersions (PERMDISP)",
                                           style = "color:#f39c12;"),
                                        shiny::div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                                            shiny::icon("info-circle"), " Équivalent multivarié non paramétrique du test de Levene."),
                                        withSpinner(DT::DTOutput(ns("manovaPermDispTable")), color = "#f39c12"),
                                        shiny::uiOutput(ns("manovaPermDispInterpretation"))
                                      ),
                                    
                                      shiny::tabPanel(
                                        title = shiny::tagList(shiny::icon("brain"), " 3. Décomposition des effets"),
                                        value = "manova_interprétation", shiny::br(),
                                      
                                        shiny::uiOutput(ns("manovaInterpretationGuidance")),
                                      
                                        shiny::conditionalPanel(
                                          ns = ns,
                                          condition = "output.hasManovaInteraction",
                                          shiny::br(),
                                          shiny::div(style = "background:#fff3e0; border:2px solid #fb8c00; border-radius:8px; padding:14px 18px; margin-top:14px;",
                                              shiny::h4(shiny::icon("project-diagram"),
                                                 " Décomposition de l\'interaction (effets simples)",
                                                 style = "color:#e65100; margin-top:0;"),
                                              shiny::p(style = "color:#555; font-size:13px;",
                                                "Une interaction est significative : l\'effet d\'un facteur dépend du niveau de l\'autre. ",
                                                "Choisissez un facteur à ", shiny::em("fixer"), " et un facteur à ", shiny::em("tester"),
                                                ", puis cliquez ", shiny::strong("Calculer"), "."),
                                              shiny::uiOutput(ns("manovaSimpleEffectsSelectors")),
                                              shiny::br(),
                                              shiny::conditionalPanel(
                                                ns = ns,
                                                condition = "output.hasManovaSimpleEffects",
                                                withSpinner(DT::DTOutput(ns("manovaSimpleEffectsTable")), color = "#fb8c00"),
                                                shiny::div(style = "font-size:11px; color:#888; margin-top:8px;",
                                                    shiny::icon("info-circle"),
                                                    " Les p-valeurs sont ajustées par Bonferroni sur l\'ensemble des niveaux fixes.")
                                              )
                                          )
                                        )
                                      )
                          ),
                        
                          shiny::br(),
                        
                          shiny::fluidRow(
                            shiny::column(4,
                                   shiny::div(style = "background:#e3f2fd; padding:12px 14px; border-radius:8px;",
                                       shiny::h6(shiny::icon("magic"), " Diagnostic automatique",
                                          style = "margin-top:0; color:#1565C0; font-weight:bold;"),
                                       shiny::p(style = "font-size:11px; color:#555; margin-bottom:8px;",
                                         "Vérifie les prérequis et recommande le test optimal."),
                                       shiny::actionButton(ns("runManovaDiagnostic"),
                                                    shiny::tagList(shiny::icon("magic"), " Diagnostiquer mes données"),
                                                    class = "btn-primary btn-block",
                                                    style = "font-weight:bold;")
                                   )
                            ),
                            shiny::column(4,
                                   shiny::conditionalPanel(
                                     ns = ns,
                                     condition = "output.hasManovaParam",
                                     shiny::downloadButton(ns("downloadManovaParam"),
                                                    shiny::tagList(shiny::icon("file-excel"), " Télécharger MANOVA (.xlsx)"),
                                                    class = "btn-success btn-block",
                                                    style = "margin-top:42px;")
                                   )
                            ),
                            shiny::column(4,
                                   shiny::conditionalPanel(
                                     ns = ns,
                                     condition = "output.hasManovaPermanova",
                                     shiny::downloadButton(ns("downloadManovaPermanova"),
                                                    shiny::tagList(shiny::icon("file-excel"), " Télécharger PERMANOVA (.xlsx)"),
                                                    class = "btn-success btn-block",
                                                    style = "margin-top:42px;")
                                   )
                            )
                          )
                        )
                    )
                  )
                ),
              
                shiny::conditionalPanel(
                  ns = ns,
                  condition = "!output.showManovaWorkflow",
                  shiny::fluidRow(
                    shiny::div(id = "boxWrap_manovaPlaceholder",
                        shinydashboard::box(
                          title = shiny::tagList(shiny::icon("magic"), " Analyse multivariée assistée"),
                          status = "info", width = 12, solidHeader = TRUE,
                          collapsible = TRUE, collapsed = TRUE,
                          shiny::div(style = "padding:20px; text-align:center;",
                              shiny::icon("magic", style = "font-size:48px; color:#1565C0; opacity:0.6;"),
                              shiny::h4("Parcours pour débutants et experts",
                                 style = "color:#1565C0;"),
                              shiny::p(style = "font-size:13px; color:#555; max-width:600px; margin:8px auto;",
                                "Sélectionnez au moins ", shiny::strong("2 variables réponses numériques"),
                                " et ", shiny::strong("1 facteur"), " dans \'Paramètres des tests\'. ",
                                "Puis cliquez sur le bouton ci-dessous pour un diagnostic complet et une recommandation automatique."),
                              shiny::br(),
                              # DEUXIEME point d'entree du meme diagnostic, et non
                              # un doublon : les deux boutons portaient le meme
                              # identifiant, si bien que la page en contenait deux
                              # exemplaires. Cela marche tant qu'on se contente de
                              # cliquer -- la liaison de Shiny lit l'id de
                              # l'element -- mais `updateActionButton()` ou
                              # `shinyjs::disable()` n'en atteindraient qu'un seul,
                              # et le HTML est invalide.
                              shiny::actionButton(ns("runManovaDiagnostic2"),
                                           shiny::tagList(shiny::icon("magic"), " Diagnostiquer mes données"),
                                           class = "btn-primary btn-lg",
                                           style = "padding:10px 30px; font-weight:bold;")
                          )
                        )
                    )
                  )
                ),
              
              
                shiny::fluidRow(
                  shinydashboard::box(
                    title = shiny::div(
                      shiny::icon("magic", style = "color:#f57c00; margin-right:6px;"),
                      shiny::tags$span("Transformation des variables",
                                style = "font-size:14px; font-weight:bold;"),
                      shiny::tags$span(
                        style = paste0("font-size:10px; font-weight:normal; color:#fff;",
                                       "background:#ef6c00; padding:2px 7px;",
                                       "border-radius:10px; margin-left:8px;"),
                        "Tests paramétriques uniquement"
                      )
                    ),
                    status = "warning", width = 12,
                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  
                    shiny::div(
                      style = paste0("padding:10px 14px;background:#fff3e0;",
                                     "border-left:4px solid #fb8c00;border-radius:4px;",
                                     "margin-bottom:14px;font-size:12px;"),
                      shiny::icon("lightbulb", style = "color:#e65100;"),
                      shiny::tags$b(style = "color:#bf360c;", " Quand utiliser ?"),
                      shiny::tags$br(),
                      shiny::tags$span(style = "color:#6d4c41;",
                                "Après avoir testé la normalité/homogénéité -- si les conditions ne sont ",
                                shiny::tags$b("pas"), " remplies, appliquez une transformation.",
                                " La variable transformée apparaît dans les sélecteurs.",
                                " Retestez ensuite les conditions sur la variable transformée."
                      )
                    ),
                  
                    shiny::fluidRow(
                    
                      # Col 1 : Sélection + méthode + bouton
                      shiny::column(4,
                             shiny::h5(shiny::icon("sliders-h"), " Variable & méthode",
                                style = "color:#e65100;margin-top:0;border-bottom:2px solid #ffcc80;padding-bottom:6px;"),
                             shiny::uiOutput(ns("transformVarSelect")),
                             shiny::br(),
                             shiny::selectInput(ns("transformMethod"),
                               shiny::tags$span(shiny::icon("flask"), " Transformation :"),
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
                             shiny::uiOutput(ns("transformFeasibilityCheck")),
                             shiny::br(),
                             shiny::actionButton(ns("applyTransformation"),
                               shiny::HTML("<i class='fa fa-magic'></i>&nbsp;<b>Appliquer la transformation</b>"),
                               class = "btn-warning btn-lg btn-block",
                               style = "height:50px;box-shadow:0 3px 5px rgba(0,0,0,0.2);"
                             )
                      ),
                    
                      # Col 2 : Journal des transformations actives
                      shiny::column(4,
                             shiny::h5(shiny::icon("history"), " Transformations actives",
                                style = "color:#e65100;margin-top:0;border-bottom:2px solid #ffcc80;padding-bottom:6px;"),
                             shiny::div(style = "min-height:120px;", shiny::uiOutput(ns("transformationLogDisplay"))),
                             shiny::uiOutput(ns("removeTransformSelect"))
                      ),
                    
                      # Col 3 : Guide de sélection
                      shiny::column(4,
                             shiny::h5(shiny::icon("book-open"), " Guide de sélection",
                                style = "color:#e65100;margin-top:0;border-bottom:2px solid #ffcc80;padding-bottom:6px;"),
                             shiny::tags$table(
                               style = "width:100%;border-collapse:collapse;font-size:11px;",
                               shiny::tags$thead(
                                 shiny::tags$tr(
                                   style = "background:#ef6c00;color:white;",
                                   shiny::tags$th(style = "padding:4px 6px;", "Méthode"),
                                   shiny::tags$th(style = "padding:4px 6px;", "Données"),
                                   shiny::tags$th(style = "padding:4px 6px;text-align:center;", "Négatifs ?")
                                 )
                               ),
                               shiny::tags$tbody(
                                 shiny::tags$tr(style="background:#fff8e1;",
                                         shiny::tags$td(style="padding:3px 6px;font-family:monospace;","log(x)"),
                                         shiny::tags$td(style="padding:3px 6px;","Très asym., rendements"),
                                         shiny::tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", shiny::icon("times"))),
                                 shiny::tags$tr(style="background:#fffff0;",
                                         shiny::tags$td(style="padding:3px 6px;font-family:monospace;","log(x+1)"),
                                         shiny::tags$td(style="padding:3px 6px;","Idem + zéros"),
                                         shiny::tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", shiny::icon("times"))),
                                 shiny::tags$tr(style="background:#fff8e1;",
                                         shiny::tags$td(style="padding:3px 6px;font-family:monospace;","sqrt(x)"),
                                         shiny::tags$td(style="padding:3px 6px;","Comptage, Poisson"),
                                         shiny::tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", shiny::icon("times"))),
                                 shiny::tags$tr(style="background:#fffff0;",
                                         shiny::tags$td(style="padding:3px 6px;font-family:monospace;","x^(1/3)"),
                                         shiny::tags$td(style="padding:3px 6px;","Toutes valeurs"),
                                         shiny::tags$td(style="padding:3px 6px;text-align:center;color:#43a047;", shiny::icon("check"))),
                                 shiny::tags$tr(style="background:#fff8e1;",
                                         shiny::tags$td(style="padding:3px 6px;font-family:monospace;","Box-Cox"),
                                         shiny::tags$td(style="padding:3px 6px;","λ optimal (MV)"),
                                         shiny::tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", shiny::icon("times"))),
                                 shiny::tags$tr(style="background:#fffff0;",
                                         shiny::tags$td(style="padding:3px 6px;font-family:monospace;","Yeo-Johnson"),
                                         shiny::tags$td(style="padding:3px 6px;","Optimale généralisée"),
                                         shiny::tags$td(style="padding:3px 6px;text-align:center;color:#43a047;", shiny::icon("check"))),
                                 shiny::tags$tr(style="background:#fff8e1;",
                                         shiny::tags$td(style="padding:3px 6px;font-family:monospace;","asin(√x)"),
                                         shiny::tags$td(style="padding:3px 6px;","Proportions [0,1]"),
                                         shiny::tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", shiny::icon("times"))),
                                 shiny::tags$tr(style="background:#fffff0;",
                                         shiny::tags$td(style="padding:3px 6px;font-family:monospace;","logit"),
                                         shiny::tags$td(style="padding:3px 6px;","Taux ]0,1["),
                                         shiny::tags$td(style="padding:3px 6px;text-align:center;color:#e53935;", shiny::icon("times")))
                               )
                             ),
                             shiny::br(),
                             shiny::div(
                               style = paste0("padding:8px 10px;background:#e8f5e9;",
                                              "border-left:3px solid #43a047;border-radius:4px;font-size:11px;"),
                               shiny::icon("route", style = "color:#2e7d32;"),
                               shiny::tags$b(style = "color:#1b5e20;", " Workflow :"),
                               shiny::tags$ol(
                                 style = "margin:4px 0 0 0;padding-left:16px;color:#33691e;line-height:1.6;",
                                 shiny::tags$li("Tester normalité (données brutes)"),
                                 shiny::tags$li("Si p < 0.05 → transformer"),
                                 shiny::tags$li("Retester sur var. transformée"),
                                 shiny::tags$li("Lancer le test paramétrique"),
                                 shiny::tags$li("PostHoc sur var. transformée")
                               )
                             )
                      )
                    )  # fin fluidRow interne
                  )  # fin box transformation
                ),
              
                shiny::fluidRow(
                  shinydashboard::box(title = "Résultats des tests", status = "danger", width = 12, solidHeader = TRUE,
                      DT::DTOutput(ns("testResultsDF")),
                      shiny::br(),
                      shiny::downloadButton(ns("downloadTestsExcel"), "Télécharger les résultats (Excel)", class = "btn-info"))
                ),
                shiny::conditionalPanel(
                  ns = ns,
                  condition = "output.showParametricDiagnostics",
                  shiny::fluidRow(
                    shinydashboard::box(title = "Diagnostics des modèles", status = "info", width = 6, solidHeader = TRUE,
                        shiny::conditionalPanel(
                          ns = ns,
                          condition = "output.showModelNavigation",
                          shiny::wellPanel(
                            shiny::h6("Navigation des modèles", style = "margin-top: 0; margin-bottom: 10px;"),
                            shiny::div(style = "text-align: center;",
                                shiny::uiOutput(ns("modelDiagNavigation"))
                            )
                          )
                        ),
                        shiny::plotOutput(ns("modelDiagnostics"), height = "500px"),
                        shiny::br(),
                        shiny::downloadButton(ns("downloadModelDiagnostics"), "Télécharger (PNG)", class = "btn-success"),
                        shiny::htmlOutput("modelDiagnosticsInterpretation")
                    ),
                    shinydashboard::box(title = "Résidus et validation", status = "info", width = 6, solidHeader = TRUE,
                        shiny::conditionalPanel(
                          ns = ns,
                          condition = "output.showResidNavigation",
                          shiny::wellPanel(
                            shiny::h6("Navigation des variables", style = "margin-top: 0; margin-bottom: 10px;"),
                            shiny::div(style = "text-align: center;",
                                shiny::uiOutput(ns("residNavigation"))
                            )
                          )
                        ),
                        shinydashboard::tabBox(
                          title = "Analyses des résidus",
                          id = "residualTabs", width = 12,
                          shiny::tabPanel("QQ-plot", 
                                   shiny::plotOutput(ns("qqPlotResiduals"), height = "320px"),
                                   shiny::br(),
                                   shiny::downloadButton(ns("downloadQQPlot"), "Télécharger (PNG)", class = "btn-success"),
                                   shiny::htmlOutput("qqPlotInterpretation")),
                          shiny::tabPanel("Normalité", 
                                   shiny::verbatimTextOutput(ns("normalityResult")),
                                   shiny::htmlOutput("normalityResidInterpretation")),
                          shiny::tabPanel("Homogénéité", 
                                   shiny::verbatimTextOutput(ns("leveneResidResult")),
                                   shiny::htmlOutput("homogeneityResidInterpretation")),
                          shiny::tabPanel("Autocorrélation", 
                                   shiny::verbatimTextOutput(ns("autocorrResult")),
                                   shiny::htmlOutput("autocorrInterpretation")),
                          shiny::tabPanel("Summary", shiny::verbatimTextOutput(ns("modelSummary")))
                        )
                    )
                  )
                )
      )
}

mod_posthoc_ui <- function(id) {
  ns <- shiny::NS(id)
      shinydashboard::tabItem(tabName = "multiple",
                .hstat_scope_banner(exact = FALSE),
                shiny::fluidRow(
                
                
                  shinydashboard::box(title = shiny::div(shiny::icon("cog"), " Configuration de l'analyse"), 
                      status = "primary", width = 4, solidHeader = TRUE,
                    
                    
                      shiny::div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                          shiny::h4(style = "color: #2c3e50; margin-top: 0;", shiny::icon("chart-line"), " Sélection des variables"),
                          shiny::uiOutput(ns("multiResponseSelect")),
                          shiny::uiOutput(ns("multiFactorSelect")),
                          # Bandeau info transformations actives (affiché si variables transformées sélectionnées)
                          shiny::uiOutput(ns("postHocTransformInfo"))
                      ),
                    
                    
                      shiny::div(style = "background-color: #fef9e7; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                          shiny::h4(style = "color: #2c3e50; margin-top: 0;", shiny::icon("hashtag"), " Affichage des résultats"),
                          shiny::checkboxInput(ns("multiRoundResults"), "Arrondir les résultats numériques", value = FALSE),
                          shiny::conditionalPanel(
                            ns = ns,
                            condition = "input.multiRoundResults == true",
                            shiny::numericInput(ns("multiDecimals"), "Nombre de décimales :",
                                         value = 2, min = 0, max = 8, step = 1)
                          ),
                          shiny::helpText(style = "font-size: 11px; color: #7f8c8d;",
                                   "Si décoché, les valeurs s'affichent sans arrondi.")
                      ),
                    
                    
                      shiny::div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                          shiny::h4(style = "color: #2c3e50; margin-top: 0;", shiny::icon("vial"), " Tests statistiques"),
                          shiny::radioButtons(ns("testType"), "Type de comparaisons",
                                       choiceNames = list(
                                         shiny::HTML("<b>Paramétrique</b> <small style='color:#7f8c8d;'>- Données normales</small>"), 
                                         shiny::HTML("<b>Non paramétrique</b> <small style='color:#7f8c8d;'>- Sans normalité</small>")
                                       ),
                                       choiceValues = list("param", "nonparam"),
                                       selected = "param"
                          ),
                          shiny::conditionalPanel(
                            ns = ns,
                            condition = "input.testType == 'param'",
                            shiny::selectInput(ns("multiTest"), "Méthode post-hoc paramétrique",
                                        choices = list(
                                          "Tukey HSD (recommandé)" = "tukey", 
                                          "LSD (Fisher)" = "lsd", 
                                          "Duncan" = "duncan", 
                                          "SNK (Student-Newman-Keuls)" = "snk",
                                          "Scheffé (conservateur)" = "scheffe",
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
                            shiny::selectInput(ns("multiParamAdjust"),
                                        shiny::tagList(shiny::icon("sliders-h"), " Ajustement des p-values (homogénéisation des groupes)"),
                                        choices = c("Holm" = "holm", "Bonferroni" = "bonferroni",
                                                    "BH (FDR)" = "BH", "BY" = "BY",
                                                    "Hochberg" = "hochberg", "Hommel" = "hommel",
                                                    "Aucun" = "none"),
                                        selected = "holm"),
                            shiny::div(style = "font-size:11px;color:#7f8c8d;margin-top:-6px;margin-bottom:8px;",
                                shiny::icon("info-circle"),
                                shiny::HTML(" S'applique aux méthodes par comparaisons de paires (LSD, Bonferroni, LM/GLM). Un ajustement plus strict (Bonferroni, Holm) rend les groupes plus homogènes ; les méthodes à contrôle intégré (Tukey, Duncan, SNK, Scheffé, Games-Howell) conservent le leur."))
                          ),
                          shiny::conditionalPanel(
                            ns = ns,
                            condition = "input.testType == 'nonparam'",
                            shiny::selectInput(ns("multiTestNonParam"), "Méthode post-hoc non paramétrique",
                                        choices = list(
                                          "Kruskal-Wallis (base)" = "kruskal",
                                          "Dunn" = "dunn",
                                          "Conover" = "conover",
                                          "Nemenyi" = "nemenyi",
                                          "PERMANOVA pairwise (multivarié, >= 2 réponses)" = "permanova"
                                        ),
                                        selected = "dunn"
                            ),
                            shiny::selectInput(ns("multiNonParamAdjust"),
                                        shiny::tagList(shiny::icon("sliders-h"), " Ajustement des p-values (homogénéisation des groupes)"),
                                        choices = c("Holm" = "holm", "Bonferroni" = "bonferroni",
                                                    "BH (FDR)" = "BH", "BY" = "BY",
                                                    "Hochberg" = "hochberg", "Hommel" = "hommel",
                                                    "Aucun" = "none"),
                                        selected = "holm")
                          ),
                          # Option retro-transformation -- visible uniquement si des variables
                          # transformées sont sélectionnées dans multiResponse
                          shiny::conditionalPanel(
                            ns = ns,
                            condition = "output.hasTransformedVarsSelected",
                            shiny::div(
                              style = paste0("margin-top:10px;padding:10px 12px;",
                                             "background:#fff8e1;border:1px solid #ffb300;",
                                             "border-radius:6px;"),
                              shiny::checkboxInput(ns("showBackTransformed"),
                                shiny::HTML(paste0(
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
                    
                    
                      shiny::div(style = "border: 3px solid #e74c3c; border-radius: 8px; padding: 15px; margin-bottom: 15px; background: linear-gradient(135deg, #fff5f5 0%, #ffe8e8 100%);",
                          shiny::h4(style = "color: #c0392b; margin-top: 0;", 
                             shiny::icon("project-diagram"), " Analyse des interactions"),
                          shiny::checkboxInput(ns("posthocInteraction"), 
                                        shiny::HTML("<strong style='color: #c0392b;'>Activer l'analyse des interactions</strong>"), 
                                        value = FALSE),
                          shiny::conditionalPanel(
                            ns = ns,
                            condition = "input.posthocInteraction == true",
                            shiny::div(style = "margin-top: 8px; padding: 8px 10px; background:#fff3e0; border-left:3px solid #ff9800; border-radius:4px;",
                                shiny::tags$small(style="color:#e65100;",
                                           shiny::icon("info-circle"), " Sélectionnez >= 2 facteurs. Les effets simples s'affichent dans l'onglet 'Effets simples'."
                                )
                            )
                          )
                      ),

                      shiny::div(style = "border: 2px solid #16a085; border-radius: 8px; padding: 12px 15px; margin-bottom: 15px; background:#eafaf3;",
                          shiny::checkboxInput(ns("posthocMultivariate"),
                                        shiny::HTML("<strong style='color:#0e6655;'>Calculer aussi le post-hoc multivarié (MANOVA / PERMANOVA)</strong>"),
                                        value = FALSE),
                          shiny::tags$small(style = "color:#0e6655;",
                                     shiny::icon("info-circle"),
                                     " Décoché par défaut. Cette analyse (permutations) ne se lance PAS automatiquement avec le post-hoc ANOVA ; cochez-la seulement si vous voulez les comparaisons multivariées (nécessite >= 2 variables réponses).")
                      ),
                    
                      shiny::hr(),
                    
                    
                      shiny::actionButton(ns("runMultiple"), 
                                   shiny::HTML("<h5 style='margin: 5px 0;'><i class='fa fa-play'></i> LANCER L'ANALYSE</h5>"), 
                                   class = "btn-success btn-lg", 
                                   style = "width: 100%; height: 70px; font-weight: bold; box-shadow: 0 4px 6px rgba(0,0,0,0.2);"),
                    
                      shiny::br(), shiny::br(),
                    
                  ),
                
                
                  shinydashboard::box(title = shiny::div(shiny::icon("table"), " Résultats et visualisations"), 
                      status = "primary", width = 8, solidHeader = TRUE,
                    
                      shiny::tabsetPanel(id = "resultsTabs", type = "tabs",
                                
                                  # ONGLET 1 : Effets principaux 
                                
                                  shiny::tabPanel(
                                    title = shiny::div(shiny::icon("layer-group"), " Effets principaux"),
                                    value = "mainEffects",
                                    shiny::br(),
                                    shiny::conditionalPanel(
                                      ns = ns,
                                      condition = "output.showPosthocResults",
                                      shiny::div(style = "margin-bottom: 15px;",
                                          shiny::uiOutput(ns("analysisSummaryMain"))
                                      ),
                                      shiny::div(class = "hstat-table-scroll",
                                          DT::DTOutput(ns("mainEffectsTable"))),
                                      shiny::br(),
                                      shiny::downloadButton(ns("downloadMainEffects"), 
                                                     "Télécharger effets principaux (.xlsx)", 
                                                     class = "btn-success", 
                                                     style = "width: 100%; height: 50px; font-weight: bold;",
                                                     icon = shiny::icon("download"))
                                    )
                                  ),
                                
                                  # ONGLET 2 : Effets simples 
                                
                                  shiny::tabPanel(
                                    title = shiny::div(shiny::icon("project-diagram"), " Effets simples"),
                                    value = "simpleEffects",
                                    shiny::br(),
                                    shiny::conditionalPanel(
                                      ns = ns,
                                      condition = "output.showSimpleEffects",
                                      shiny::div(style = "background: linear-gradient(135deg, #fff5f5 0%, #ffe8e8 100%); padding: 15px; border-radius: 8px; border-left: 5px solid #e74c3c; margin-bottom: 15px;",
                                          shiny::h4(style = "color: #c0392b; margin-top: 0;", 
                                             shiny::icon("info-circle"), " Interprétation des effets simples"),
                                          shiny::HTML("<div style='color: #34495e;'>
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
                                    
                                      shiny::fluidRow(
                                        shiny::column(6,
                                               shiny::div(style = "background:#f8f9fa; padding:10px; border-radius:5px;",
                                                   shiny::selectInput(ns("filterSimpleEffectVar"), 
                                                               shiny::HTML("<b>Filtrer par variable</b>"),
                                                               choices = NULL,
                                                               width = "100%")
                                               )
                                        ),
                                        shiny::column(6,
                                               shiny::div(style = "background:#f8f9fa; padding:10px; border-radius:5px;",
                                                   shiny::selectInput(ns("filterSimpleEffectInteraction"), 
                                                               shiny::HTML("<b>Filtrer par interaction</b>"),
                                                               choices = NULL,
                                                               width = "100%")
                                               )
                                        )
                                      ),
                                    
                                      shiny::br(),
                                      shiny::uiOutput(ns("simpleEffectsSummary")),
                                      shiny::div(class = "hstat-table-scroll",
                                          DT::DTOutput(ns("simpleEffectsTable"))),
                                      shiny::br(),
                                    
                                      shiny::downloadButton(ns("downloadSimpleEffects"), 
                                                     "Télécharger effets simples (.xlsx)", 
                                                     class = "btn-success",
                                                     style = "width: 100%; height: 50px; font-weight: bold;",
                                                     icon = shiny::icon("download"))
                                    ),
                                    shiny::conditionalPanel(
                                      ns = ns,
                                      condition = "!output.showSimpleEffects",
                                      shiny::div(style = "text-align: center; padding: 50px; color: #95a5a6;",
                                          shiny::icon("project-diagram", style = "font-size: 4em; opacity: 0.3;"),
                                          shiny::h4("Aucun effet simple détecté"),
                                          shiny::p("Les effets simples apparaissent uniquement quand :"),
                                          shiny::tags$ul(style = "text-align: left; display: inline-block;",
                                                  shiny::tags$li("L'option 'Analyse des interactions' est activée"),
                                                  shiny::tags$li("Au moins 2 facteurs sont sélectionnés"),
                                                  shiny::tags$li("Une interaction est significative (p < 0.05)")
                                          )
                                      )
                                    )
                                  ),
                                
                                  # ONGLET 3 : Visualisations 
                                
                                  shiny::tabPanel(
                                    title = shiny::div(shiny::icon("chart-bar"), " Graphiques"),
                                    value = "plots",
                                    shiny::br(),
                                  
                                    shiny::conditionalPanel(
                                      ns = ns,
                                      condition = "output.showVariableNavigation",
                                      shiny::wellPanel(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; color: white;",
                                                shiny::div(style = "display: flex; align-items: center; justify-content: center;",
                                                    shiny::uiOutput(ns("variableNavigation"))
                                                )
                                      )
                                    ),
                                  
                                    shiny::fluidRow(
                                      shiny::column(6,
                                             shiny::div(style = "background:#e8f4fd; padding:15px; border-radius:8px;",
                                                 shiny::h5(shiny::icon("layer-group"), " Type d'effet"),
                                                 shiny::selectInput(ns("plotDisplayType"), 
                                                             NULL,
                                                             choices = list(
                                                               "Effets principaux" = "main",
                                                               "Effets simples (interactions)" = "simple"
                                                             ),
                                                             selected = "main")
                                             )
                                      ),
                                      shiny::column(6,
                                             shiny::conditionalPanel(
                                               ns = ns,
                                               condition = "input.plotDisplayType == 'simple'",
                                               shiny::div(style = "background:#fff5f5; padding:15px; border-radius:8px;",
                                                   shiny::h5(shiny::icon("filter"), " Sélection effet simple"),
                                                   shiny::uiOutput(ns("selectSimpleEffectPlot"))
                                               )
                                             )
                                      )
                                    ),
                                  
                                    shiny::hr(),
                                  
                                    shiny::div(style = "background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                        shiny::h4(shiny::uiOutput(ns("plotTitle")), style = "text-align: center; color: #2c3e50;"),
                                        plotlyOutput(ns("multiPlot"), height = "600px")
                                    ),
                                  
                                    shiny::br(),
                                  
                                    shiny::div(style = "border:1px solid #dee2e6; border-radius:10px; overflow:hidden; box-shadow:0 2px 10px rgba(0,0,0,.06);",
                                        shiny::div(
                                          class = "panel-heading",
                                          style = paste0("background:linear-gradient(135deg,#3c8dbc 0%,#8e44ad 100%);",
                                                         "color:white; padding:14px 18px; cursor:pointer;",
                                                         "display:flex; align-items:center;"),
                                          `data-toggle` = "collapse",
                                          `data-target` = "#graphOptionsPanel",
                                          shiny::icon("sliders-h", style = "margin-right: 10px;"),
                                          shiny::tags$strong("Options du graphique"),
                                          shiny::tags$span(style = "margin-left:12px; font-size:12px; opacity:.85;",
                                                    "toute la mise en forme, de la palette à l'export"),
                                          shiny::tags$span(style = "margin-left: auto; font-size: 12px; opacity: 0.85;",
                                                    shiny::icon("chevron-down"), " Développer / Réduire")
                                        ),
                                        shiny::div(id = "graphOptionsPanel", class = "collapse",
                                            shiny::div(style = "padding: 18px; background-color: #fbfcfd;",

                                                shiny::fluidRow(
                                                  # ---- COL 1 : type, palette, barres d'erreur ----
                                                  shiny::column(3,
                                                    .hstat_opt_section(
                                                      "Type et couleurs", "palette", "#8e44ad", "#f7f0fb",
                                                      shiny::radioButtons(ns("plotType"), "Type de graphique",
                                                                   choices = c("Boxplot" = "box", "Violon" = "violin",
                                                                               "Points + barres" = "point", "Barres" = "hist"),
                                                                   selected = "box"),
                                                      shiny::selectInput(ns("boxColor"), "Palette de couleurs",
                                                                  choices = list(
                                                                    "Sans palette" = c("Défaut (gris)" = "default"),
                                                                    "Teintes vives (groupes distincts)" = HSTAT_PALETTES_QUALI,
                                                                    "Dégradés (valeurs ordonnées)" = HSTAT_PALETTES_DEGRADE),
                                                                  selected = "Set2"),
                                                      shiny::selectInput(ns("posthocTheme"), "Thème du graphique",
                                                                  choices = HSTAT_THEMES_GG, selected = "minimal"),
                                                      shiny::radioButtons(ns("errorType"), "Barres d'erreur",
                                                                   choices = c("SE" = "se", "SD" = "sd",
                                                                               "IC 95%" = "ci", "Aucune" = "none"),
                                                                   selected = "se", inline = TRUE),
                                                      shiny::checkboxInput(ns("colorByGroups"),
                                                                    shiny::HTML("Colorer par groupes statistiques <small style='color:#6c757d;'>(a, b, c...)</small>"),
                                                                    value = FALSE),
                                                      shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                                                                 "Un dégradé sur des groupes sans ordre naturel suggère une progression qui n'existe pas.")
                                                    )
                                                  ),

                                                  # ---- COL 2 : textes ----
                                                  shiny::column(3,
                                                    .hstat_opt_section(
                                                      "Titres et libellés", "heading", "#2980b9", "#eaf3fa",
                                                      shiny::textInput(ns("customTitle"), "Titre", placeholder = "Auto"),
                                                      shiny::textInput(ns("customSubtitle"), "Sous-titre", placeholder = "Optionnel"),
                                                      shiny::fluidRow(
                                                        shiny::column(6, shiny::textInput(ns("customXLabel"), "Libellé X", placeholder = "Auto")),
                                                        shiny::column(6, shiny::textInput(ns("customYLabel"), "Libellé Y", placeholder = "Auto"))
                                                      ),
                                                      shiny::textInput(ns("customLegendTitle"), "Titre de la légende", placeholder = "Auto"),
                                                      shiny::selectInput(ns("subtitlePosition"), "Position du sous-titre",
                                                                  choices = list("Centré" = "0.5", "Gauche" = "0", "Droite" = "1"),
                                                                  selected = "0.5"),
                                                      shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                                                                 "Le gras et l'italique s'écrivent aussi dans le texte : **gras**, *italique*.")
                                                    )
                                                  ),

                                                  # ---- COL 3 : tailles et styles ----
                                                  shiny::column(3,
                                                    .hstat_opt_section(
                                                      "Tailles", "text-height", "#d35400", "#fdf2e9",
                                                      shiny::fluidRow(
                                                        shiny::column(6, shiny::sliderInput(ns("titleSize"), "Titre", min = 8, max = 32, value = 16, step = 1, ticks = FALSE)),
                                                        shiny::column(6, shiny::sliderInput(ns("subtitleSize"), "Sous-titre", min = 6, max = 28, value = 12, step = 1, ticks = FALSE))
                                                      ),
                                                      shiny::fluidRow(
                                                        shiny::column(6, shiny::sliderInput(ns("axisTitleSize"), "Titres des axes", min = 8, max = 28, value = 14, step = 1, ticks = FALSE)),
                                                        shiny::column(6, shiny::sliderInput(ns("axisTextSize"), "Graduations", min = 6, max = 24, value = 12, step = 1, ticks = FALSE))
                                                      ),
                                                      # Les noms de traitement sont longs par nature
                                                      # ici (« 2SP(0,5)&2PV »...) : le titre d'axe
                                                      # deborde des qu'on le nomme vraiment.
                                                      hstat_axe_titre_ui(ns, "posthoc"),
                                                      shiny::fluidRow(
                                                        shiny::column(6, shiny::sliderInput(ns("graphValueSize"), "Lettres (a, b, c)", min = 2, max = 20, value = 5, step = 0.5, ticks = FALSE)),
                                                        shiny::column(6, shiny::sliderInput(ns("meanValueSize"), "Moyennes", min = 2, max = 12, value = 4, step = 0.5, ticks = FALSE))
                                                      )
                                                    ),
                                                    .hstat_opt_section(
                                                      "Styles d'écriture", "font", "#c0392b", "#fdeeec",
                                                      # Un select par ligne : appairés, leurs libellés étaient
                                                      # rognés dans une colonne de panneau.
                                                      shiny::selectInput(ns("titleFontStyle"), "Titre",
                                                                  choices = HSTAT_FONT_STYLES, selected = "bold"),
                                                      shiny::selectInput(ns("subtitleFontStyle"), "Sous-titre",
                                                                  choices = HSTAT_FONT_STYLES, selected = "italic"),
                                                      shiny::selectInput(ns("axisTitleFontStyle"), "Titres des axes",
                                                                  choices = HSTAT_FONT_STYLES, selected = "plain"),
                                                      shiny::selectInput(ns("graphValueFontStyle"), "Lettres (a, b, c)",
                                                                  choices = HSTAT_FONT_STYLES, selected = "bold"),
                                                      shiny::selectInput(ns("axisTextXFontStyle"), "Graduations X",
                                                                  choices = HSTAT_FONT_STYLES, selected = "plain"),
                                                      shiny::selectInput(ns("axisTextYFontStyle"), "Graduations Y",
                                                                  choices = HSTAT_FONT_STYLES, selected = "plain"),
                                                      shiny::checkboxInput(ns("rotateXLabels"), "Libellés X inclinés à 45°", value = TRUE)
                                                    )
                                                  ),

                                                  # ---- COL 4 : axes, legende, export ----
                                                  shiny::column(3,
                                                    .hstat_opt_section(
                                                      "Axes et ordre", "ruler-combined", "#16a085", "#e8f8f4",
                                                      shiny::checkboxInput(ns("customAxisLimits"), "Personnaliser les limites", value = FALSE),
                                                      shiny::conditionalPanel(
                                                        ns = ns,
                                                        condition = "input.customAxisLimits == true",
                                                        shiny::fluidRow(
                                                          shiny::column(6, shiny::numericInput(ns("yAxisMin"), "Y min", value = NULL, step = 0.1)),
                                                          shiny::column(6, shiny::numericInput(ns("yAxisMax"), "Y max", value = NULL, step = 0.1))
                                                        ),
                                                        shiny::fluidRow(
                                                          shiny::column(6, shiny::numericInput(ns("xAxisMin"), "X min", value = NULL, step = 0.1)),
                                                          shiny::column(6, shiny::numericInput(ns("xAxisMax"), "X max", value = NULL, step = 0.1))
                                                        ),
                                                        shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                                                                   "Les limites X ne s'appliquent qu'à un axe numérique.")
                                                      ),
                                                      shiny::checkboxInput(ns("customAxisBreaks"), "Personnaliser les graduations", value = FALSE),
                                                      shiny::conditionalPanel(
                                                        ns = ns,
                                                        condition = "input.customAxisBreaks == true",
                                                        shiny::fluidRow(
                                                          shiny::column(6, shiny::numericInput(ns("yAxisBreakStep"), "Pas Y", value = NULL, step = 0.1, min = 0.01)),
                                                          shiny::column(6, shiny::numericInput(ns("xAxisBreakStep"), "Pas X", value = NULL, step = 0.1, min = 0.01))
                                                        )
                                                      ),
                                                      shiny::checkboxInput(ns("customXOrder"), "Personnaliser l'ordre de l'axe X", value = FALSE),
                                                      shiny::conditionalPanel(
                                                        ns = ns,
                                                        condition = "input.customXOrder == true",
                                                        shiny::uiOutput(ns("xAxisOrderUI"))
                                                      )
                                                    ),
                                                    .hstat_opt_section(
                                                      "Légende", "list", "#7f8c8d", "#f4f6f7",
                                                      shiny::fluidRow(
                                                        shiny::column(6, shiny::sliderInput(ns("legendTitleSize"), "Titre", min = 6, max = 24, value = 12, step = 1, ticks = FALSE)),
                                                        shiny::column(6, shiny::sliderInput(ns("legendTextSize"), "Texte", min = 6, max = 20, value = 10, step = 1, ticks = FALSE))
                                                      ),
                                                      shiny::selectInput(ns("legendTitleFontStyle"), "Style du titre",
                                                                  choices = HSTAT_FONT_STYLES, selected = "bold"),
                                                      shiny::selectInput(ns("legendTextFontStyle"), "Style du texte",
                                                                  choices = HSTAT_FONT_STYLES, selected = "plain"),
                                                      shiny::fluidRow(
                                                        shiny::column(6, shiny::sliderInput(ns("legendSpacing"), "Espacement", min = 0, max = 6, value = 0, step = 0.1, ticks = FALSE)),
                                                        shiny::column(6, shiny::sliderInput(ns("legendKeySize"), "Taille des clés", min = 0.4, max = 3, value = 1.2, step = 0.1, ticks = FALSE))
                                                      ),
                                                      shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                                                                 "La légende n'apparaît qu'avec « Colorer par groupes statistiques ».")
                                                    ),
                                                    .hstat_opt_section(
                                                      "Taille du fichier exporté", "download", "#2c3e50", "#eef1f4",
                                                      shiny::fluidRow(
                                                        shiny::column(6, shiny::numericInput(ns("plotWidth"), "Largeur (pouces)", value = 8, min = 3, max = 20, step = 0.5)),
                                                        shiny::column(6, shiny::numericInput(ns("plotHeight"), "Hauteur (pouces)", value = 6, min = 3, max = 20, step = 0.5))
                                                      ),
                                                      shiny::tags$small(style = "color:#7f8c8d;font-style:italic;",
                                                                 "Format et résolution se choisissent sous le graphique.")
                                                    )
                                                  )
                                                )
                                            )
                                        )
                                    ),
                                  
                                    shiny::br(),
                                  
                                    shiny::div(style = "max-width: 400px; margin: 0 auto;",
                                        shiny::fluidRow(
                                          shiny::column(7,
                                            hstat_format_input(ns("multiPlotFormat"),
                                              shiny::tagList(shiny::icon("file-image"), " Format d\'export"),
                                              width = "100%")),
                                          shiny::column(5,
                                            hstat_dpi_input(ns("plotDPIVisible"), shiny::tagList(shiny::icon("image"), " DPI"), width = "100%"))
                                        ),
                                        shiny::downloadButton(ns("downloadMultiPlot"),
                                                       shiny::tagList(shiny::icon("download"), " Télécharger le graphique"),
                                                       class = "btn-success",
                                                       style = "width: 100%; height: 50px; font-weight: bold;")
                                    )
                                  ),
                                
                                  # ONGLET 4 : Rapport complet 
                                
                                  shiny::tabPanel(
                                    title = shiny::div(shiny::icon("file-alt"), " Rapport"),
                                    value = "report",
                                    shiny::br(),
                                  
                                    shiny::div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                                        shiny::h3(style = "color: #2c3e50;", shiny::icon("clipboard-check"), " Résumé de l'analyse"),
                                        shiny::hr(),
                                        shiny::uiOutput(ns("fullAnalysisReport"))
                                    ),
                                  
                                    shiny::div(style = "background: linear-gradient(135deg, #27ae60 0%, #229954 100%); padding: 20px; border-radius: 8px;",
                                        shiny::h4(style = "color: white; margin-top: 0;", 
                                           shiny::icon("download"), " Téléchargements"),
                                        shiny::fluidRow(
                                          shiny::column(4,
                                                 shiny::downloadButton(ns("downloadAllResults"), 
                                                                shiny::div(shiny::icon("file-excel", style = "font-size: 2em; display: block; margin-bottom: 10px;"), 
                                                                    "Toutes les données"),
                                                                class = "btn-light btn-lg",
                                                                style = "width: 100%; height: 120px; font-weight: bold;")
                                          ),
                                          shiny::column(4,
                                                 shiny::downloadButton(ns("downloadSummaryStats"), 
                                                                shiny::div(shiny::icon("chart-pie", style = "font-size: 2em; display: block; margin-bottom: 10px;"), 
                                                                    "Statistiques résumées"),
                                                                class = "btn-light btn-lg",
                                                                style = "width: 100%; height: 120px; font-weight: bold;")
                                          ),
                                          shiny::column(4,
                                                 shiny::downloadButton(ns("downloadFullReport"), 
                                                                shiny::div(shiny::icon("file-pdf", style = "font-size: 2em; display: block; margin-bottom: 10px;"), 
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
              
                shiny::fluidRow(
                  shinydashboard::box(
                    title = shiny::tagList(shiny::icon("calculator"),
                                    " PostHoc Régression linéaire / GLM -- Comparaisons et lettres CLD"),
                    status = "primary", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                  
                    shiny::conditionalPanel(
                      ns = ns,
                      condition = "output.hasLMPostHoc",
                      shiny::fluidRow(
                        shiny::column(4,
                               shiny::div(style = "background:#f8f9fa; padding:14px; border-radius:8px;",
                                   shiny::h5(shiny::tagList(shiny::icon("filter"), " Sélection"),
                                      style = "font-weight:bold; margin-top:0; color:#1565C0;"),
                                   shiny::uiOutput(ns("lmPostHocSelector")),
                                   shiny::br(),
                                   shiny::uiOutput(ns("lmPostHocInfo")),
                                   shiny::br(),
                                   shiny::downloadButton(ns("downloadLMPostHoc"),
                                                  shiny::tagList(shiny::icon("file-excel"), " Télécharger Excel"),
                                                  class = "btn-success btn-block",
                                                  style = "height:50px; font-weight:bold;")
                               )
                        ),
                        shiny::column(8,
                               shiny::tabsetPanel(type = "tabs",
                                           shiny::tabPanel(
                                             title = shiny::tagList(shiny::icon("layer-group"), " Lettres CLD"),
                                             shiny::br(),
                                             shiny::div(style = "background:#e3f2fd; border-left:3px solid #1565C0; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:11px;",
                                                 shiny::icon("info-circle"),
                                                 " Niveaux partageant une même lettre = pas de différence significative (alpha = 0.05)."
                                             ),
                                             withSpinner(DT::DTOutput(ns("lmPostHocLettersTable")), color = "#1565C0")
                                           ),
                                           shiny::tabPanel(
                                             title = shiny::tagList(shiny::icon("code-branch"), " Paires (emmeans)"),
                                             shiny::br(),
                                             shiny::div(style = "background:#fff3e0; border-left:3px solid #fb8c00; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:11px;",
                                                 shiny::icon("info-circle"),
                                                 " Comparaisons par paires sur les moyennes marginales estimées (emmeans). ",
                                                 "Pour les GLM non-gaussiens, les valeurs sont sur l'échelle du lien."
                                             ),
                                             withSpinner(DT::DTOutput(ns("lmPostHocPairsTable")), color = "#fb8c00")
                                           )
                               )
                        )
                      )
                    ),
                    shiny::conditionalPanel(
                      ns = ns,
                      condition = "!output.hasLMPostHoc",
                      shiny::div(style = "text-align:center; padding:40px; color:#95a5a6;",
                          shiny::icon("calculator", style = "font-size:4em; opacity:0.3;"),
                          shiny::h4("Aucun PostHoc LM/GLM calculé"),
                          shiny::p("Pour activer cette section :"),
                          shiny::tags$ul(style = "text-align:left; display:inline-block; color:#555;",
                                  shiny::tags$li("Lancez d'abord une ", shiny::strong("Régression linéaire"),
                                          " ou un ", shiny::strong("GLM"),
                                          " dans l'onglet 'Tests statistiques'"),
                                  shiny::tags$li("Le modèle doit contenir au moins ", shiny::strong("un prédicteur catégoriel (factor)")),
                                  shiny::tags$li("Revenez ici, sélectionnez ", shiny::strong("'LM / GLM (emmeans + lettres CLD)'"),
                                          " dans la liste 'Méthode post-hoc paramétrique'"),
                                  shiny::tags$li("Cliquez sur ", shiny::strong("LANCER L'ANALYSE"))
                          )
                      )
                    )
                  )
                ),
              
                # MANOVA / PERMANOVA POSTHOC -- Comparaisons multivariees par paires + lettres
              
                shiny::fluidRow(
                  shiny::div(id = "boxWrap_manovaPosthoc",
                      shinydashboard::box(
                        title = shiny::tagList(shiny::icon("layer-group"),
                                        " PostHoc MANOVA/PERMANOVA -- Comparaisons multivariées par paires et lettres de groupes"),
                        status = "success", width = 12, solidHeader = TRUE,
                        collapsible = TRUE, collapsed = TRUE,
                      
                        shiny::conditionalPanel(
                          ns = ns,
                          condition = "output.hasMultivariatePosthoc",
                        
                          shiny::fluidRow(
                            shiny::column(4,
                                   shiny::div(style = "background:#f8f9fa; padding:14px; border-radius:8px;",
                                       shiny::h5(shiny::tagList(shiny::icon("filter"), " Sélection du facteur"),
                                          style = "font-weight:bold; margin-top:0; color:#2e7d32;"),
                                       shiny::uiOutput(ns("multivariatePosthocFactorSelect")),
                                       shiny::br(),
                                       shiny::uiOutput(ns("multivariatePosthocInfo")),
                                       shiny::br(),
                                       shiny::downloadButton(ns("downloadMultivariatePosthoc"),
                                                      shiny::tagList(shiny::icon("file-excel"), " Télécharger Excel (lettres + paires)"),
                                                      class = "btn-success btn-block",
                                                      style = "height: 50px; font-weight: bold;")
                                   )
                            ),
                            shiny::column(8,
                                   shiny::tabsetPanel(type = "tabs",
                                               shiny::tabPanel(
                                                 title = shiny::tagList(shiny::icon("layer-group"), " Groupes distincts (lettres)"),
                                                 shiny::br(),
                                                 shiny::div(style = "background:#e3f2fd; border-left:3px solid #1565C0; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:11px;",
                                                     shiny::icon("info-circle"),
                                                     " Les niveaux partageant une même lettre ne diffèrent pas significativement sur l'ensemble des réponses (test multivarié, alpha = 0.05)."
                                                 ),
                                                 withSpinner(DT::DTOutput(ns("multivariatePosthocLettersTable")), color = "#2e7d32")
                                               ),
                                               shiny::tabPanel(
                                                 title = shiny::tagList(shiny::icon("code-branch"), " Paires (PERMANOVA Bonferroni)"),
                                                 shiny::br(),
                                                 shiny::div(style = "background:#fff3e0; border-left:3px solid #fb8c00; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:11px;",
                                                     shiny::icon("info-circle"),
                                                     " Comparaisons par paires sur l'ensemble des variables réponses. ",
                                                     "Pour chaque paire : F (pseudo), R², p-value brute, p-value ajustée (Bonferroni)."
                                                 ),
                                                 withSpinner(DT::DTOutput(ns("multivariatePosthocPairsTable")), color = "#f39c12")
                                               ),
                                               shiny::tabPanel(
                                                 title = shiny::tagList(shiny::icon("project-diagram"), " Interaction (cellules croisées)"),
                                                 shiny::br(),
                                                 shiny::conditionalPanel(
                                                   ns = ns,
                                                   condition = "output.hasManovaInteractionPostHoc",
                                                   shiny::uiOutput(ns("manovaInteractionPostHocInfo")),
                                                   shiny::h5(shiny::icon("layer-group"), " Lettres par cellule d'interaction",
                                                      style = "color:#2e7d32; margin-top:0;"),
                                                   withSpinner(DT::DTOutput(ns("manovaInteractionLettersTable")), color = "#2e7d32"),
                                                   shiny::br(),
                                                   shiny::h5(shiny::icon("code-branch"), " Comparaisons par paires des cellules",
                                                      style = "color:#f39c12;"),
                                                   withSpinner(DT::DTOutput(ns("manovaInteractionPairsTable")), color = "#f39c12")
                                                 ),
                                                 shiny::conditionalPanel(
                                                   ns = ns,
                                                   condition = "!output.hasManovaInteractionPostHoc",
                                                   shiny::div(style = "text-align:center; padding:30px; color:#95a5a6;",
                                                       shiny::icon("project-diagram", style = "font-size:3em; opacity:0.3;"),
                                                       shiny::h5("Aucun PostHoc d'interaction"),
                                                       shiny::p(style = "font-size:12px;",
                                                         "Cochez ", shiny::strong("'Activer l'analyse des interactions'"),
                                                         " et sélectionnez au moins 2 facteurs avant de lancer l'analyse."))
                                                 )
                                               )
                                   )
                            )
                          )
                        ),
                      
                        shiny::conditionalPanel(
                          ns = ns,
                          condition = "!output.hasMultivariatePosthoc",
                          shiny::div(style = "text-align: center; padding: 40px; color: #95a5a6;",
                              shiny::icon("layer-group", style = "font-size: 4em; opacity: 0.3;"),
                              shiny::h4("Aucun PostHoc multivarié calculé"),
                              shiny::p("Pour activer cette section :"),
                              shiny::tags$ul(style = "text-align: left; display: inline-block; color: #555;",
                                      shiny::tags$li("Sélectionnez ", shiny::strong(">= 2 variables réponses"),
                                              " dans le panneau de configuration"),
                                      shiny::tags$li("Sélectionnez au moins ", shiny::strong("1 facteur")),
                                      shiny::tags$li("Cliquez sur ", shiny::strong("LANCER L'ANALYSE")),
                                      shiny::tags$li("Les comparaisons multivariées par paires et les lettres de groupes s'afficheront ici")
                              )
                          )
                        )
                      )
                  )
                ),
              
              
                shiny::fluidRow(
                  shinydashboard::box(
                    title = shiny::tagList(shiny::icon("repeat"), " PostHoc Mesures répétées -- Comparaisons par paires (Période / Traitement)"),
                    status = "success", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    shiny::conditionalPanel(
                      ns = ns,
                      condition = "output.hasRMPostHoc",
                      shiny::div(style = "background:#e0f2f1; border-left:4px solid #00897b; padding:8px 12px; border-radius:4px; margin-bottom:10px; font-size:12px;",
                          shiny::icon("circle-info"),
                          shiny::HTML(" Comparaisons par paires issues de la dernière <b>ANOVA à mesures répétées</b> ou du dernier <b>test non paramétrique répété</b>. Une ligne par paire de niveaux, avec p-value ajustée.")),
                      shiny::uiOutput(ns("rmPostHocInfo")),
                      withSpinner(DT::DTOutput(ns("rmPostHocTable")), color = "#00897b"),
                      shiny::br(),
                      shiny::downloadButton(ns("downloadRMPostHoc"), "Télécharger (Excel)", class = "btn-success")
                    ),
                    shiny::conditionalPanel(
                      ns = ns,
                      condition = "!output.hasRMPostHoc",
                      shiny::div(style = "text-align:center; padding:40px; color:#95a5a6;",
                          shiny::icon("repeat", style = "font-size:4em; opacity:0.3;"),
                          shiny::h4("Aucun post-hoc de mesures répétées calculé"),
                          shiny::p("Pour activer cette section :"),
                          shiny::tags$ul(style = "text-align:left; display:inline-block; color:#555;",
                                  shiny::tags$li("Renseignez Sujet, Période (et Traitement) dans le panneau « Mesures répétées »"),
                                  shiny::tags$li(shiny::HTML("Lancez <b>ANOVA à mesures répétées</b> ou <b>Non paramétrique répété</b> dans l'onglet « Tests statistiques »")),
                                  shiny::tags$li("Les comparaisons par paires s'afficheront ici")))
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
  ns <- shiny::NS(id)
  shinydashboard::tabItem(tabName = "corrélation",
    .hstat_scope_banner(exact = FALSE),
    shiny::fluidRow(
      shinydashboard::box(title = shiny::tagList(shiny::icon("cog"), " Configuration"),
          status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE,
          shiny::uiOutput(ns("corTestVarSelect")),
          shiny::checkboxInput(ns("corTestTargetMode"),
            shiny::tagList(shiny::icon("crosshairs"), " Corréler une seule variable avec les autres"),
            value = FALSE),
          shiny::conditionalPanel(sprintf("input['%s'] == true", ns("corTestTargetMode")),
            shiny::uiOutput(ns("corTestTargetSelect"))),
          shiny::selectInput(ns("corTestMethod"), "Méthode de corrélation",
            choices = c("Pearson (linéaire)"            = "pearson",
                        "Spearman (rang, monotone)"     = "spearman",
                        "Kendall (rang, concordance)"   = "kendall"),
            selected = "pearson"),
          shiny::selectInput(ns("corTestAlt"), "Hypothèse alternative",
            choices = c("Bilatérale (\u2260 0)"  = "two.sided",
                        "Unilatérale (> 0)"      = "greater",
                        "Unilatérale (< 0)"      = "less"),
            selected = "two.sided"),
          shiny::sliderInput(ns("corTestConf"), "Niveau de confiance",
            min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          shiny::selectInput(ns("corTestAdjust"), "Correction (corrélations multiples)",
            choices = c("Holm"                 = "holm",
                        "Bonferroni"           = "bonferroni",
                        "Benjamini-Hochberg (FDR)" = "BH",
                        "Benjamini-Yekutieli"  = "BY",
                        "Hochberg"             = "hochberg",
                        "Aucune"               = "none"),
            selected = "holm"),
          shiny::checkboxInput(ns("corTestAllMethods"),
            shiny::tagList(shiny::icon("layer-group"), " Comparer les 3 méthodes"), value = FALSE),
          shiny::div(class = "callout callout-info", style = "margin-top:10px;",
              shiny::icon("info-circle"),
              " Toutes les métriques (coefficient, statistique, ddl, p brute et ",
              "ajustée, IC, R², force, sens) figurent dans le tableau de résultats.")
      ),
      shinydashboard::box(title = shiny::tagList(shiny::icon("table"), " Résultats des tests de corrélation"),
          status = "success", width = 8, solidHeader = TRUE,
          DT::DTOutput(ns("corTestTable")),
          shiny::br(),
          shiny::downloadButton(ns("corTestDownload"), "Télécharger (CSV)",
                         class = "btn-success btn-sm"),
          shiny::br(), shiny::br(),
          shiny::uiOutput(ns("corTestInterpretation"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(title = shiny::tagList(shiny::icon("project-diagram"), " Matrice de Corrélation"),
          status = "success", width = 12, solidHeader = TRUE, collapsible = TRUE,
          shiny::fluidRow(
            shiny::column(12, shiny::div(style = "background:#f8f9fa;padding:15px;border-radius:5px;margin-bottom:15px;",
                           shiny::uiOutput(ns("corrVarSelect")),
                           shiny::checkboxInput(ns("corrFocusMode"),
                             shiny::tagList(shiny::icon("crosshairs"), " Corréler une seule variable avec les autres"),
                             value = FALSE),
                           shiny::conditionalPanel(sprintf("input['%s'] == true", ns("corrFocusMode")),
                             shiny::uiOutput(ns("corrFocusSelect")))))
          ),
          shiny::fluidRow(
            shiny::column(3,
              shiny::h5(shiny::icon("sliders-h"), " Méthode", style = "color:#27ae60;font-weight:bold;"),
              shiny::selectInput(ns("corrMethod"), "Méthode de corrélation",
                choices = c("Pearson (linéaire)" = "pearson",
                            "Spearman (monotone)" = "spearman",
                            "Kendall (robuste)" = "kendall"),
                selected = "pearson")),
            shiny::column(3,
              shiny::h5(shiny::icon("palette"), " Affichage", style = "color:#27ae60;font-weight:bold;"),
              shiny::selectInput(ns("corrDisplay"), "Mode d'affichage",
                choices = c("Nombres" = "number", "Cercles" = "circle",
                            "Carrés" = "square", "Ellipses" = "ellipse",
                            "Couleurs" = "color", "Secteurs" = "pie"),
                selected = "circle"),
              shiny::selectInput(ns("corrType"), "Type d'affichage",
                choices = c("Complet" = "full", "Triangulaire supérieur" = "upper",
                            "Triangulaire inférieur" = "lower"),
                selected = "upper")),
            shiny::column(3,
              shiny::h5(shiny::icon("text-height"), " Tailles", style = "color:#27ae60;font-weight:bold;"),
              shiny::sliderInput(ns("corrTextSize"), "Taille des valeurs",
                          min = 0.3, max = 2, value = 0.8, step = 0.1, ticks = FALSE),
              shiny::sliderInput(ns("corrLabelSize"), "Taille des labels",
                          min = 0.3, max = 2, value = 0.9, step = 0.1, ticks = FALSE)),
            shiny::column(3,
              shiny::h5(shiny::icon("heading"), " Titre & probabilités", style = "color:#27ae60;font-weight:bold;"),
              shiny::textInput(ns("corrTitle"), "Titre personnalisé",
                        placeholder = "Vide = titre auto"),
              hstat_dpi_input(ns("corrDPI"), shiny::tagList(shiny::icon("image"), " DPI export")),
              hstat_format_input(ns("corrFormat"), shiny::tagList(shiny::icon("file-image"), " Format d\'export")),
              shiny::numericInput(ns("corrSizeIn"), shiny::tagList(shiny::icon("ruler-combined"), " Taille (pouces, carré)"),
                           value = 8, min = 3, max = 30, step = 1))
          ),
          shiny::fluidRow(
            shiny::column(4,
              shiny::selectInput(ns("corrPval"), "Probabilités (p-values) sur le graphique",
                choices = c("Coefficient + p-value (même cellule)" = "both",
                            "Afficher les p-values" = "show",
                            "Marquer le non-significatif d'une croix" = "mark",
                            "Masquer le non-significatif" = "blank",
                            "Ne rien afficher" = "none"),
                selected = "both")),
            shiny::column(3,
              shiny::sliderInput(ns("corrSigLevel"), "Seuil de significativité (α)",
                          min = 0.01, max = 0.10, value = 0.05, step = 0.01)),
            shiny::column(2,
              shiny::sliderInput(ns("corrPvalSize"), "Taille des p-values",
                          min = 0.4, max = 2, value = 0.8, step = 0.1, ticks = FALSE)),
            shiny::column(3,
              shiny::div(style = "margin-top:25px;",
                shiny::checkboxInput(ns("corrReorder"),
                  shiny::tagList(shiny::icon("sort"), " Réordonner (regroupement hiérarchique)"),
                  value = TRUE)))
          ),
          shiny::fluidRow(
            shiny::column(3,
              shiny::selectInput(ns("corrPalette"), "Palette de couleurs",
                choices = c("Par défaut (corrplot)" = "default",
                            "Rouge-Bleu (RdBu)" = "RdBu",
                            "Rouge-Jaune-Bleu (RdYlBu)" = "RdYlBu",
                            "Violet-Orange (PuOr)" = "PuOr",
                            "Spectral" = "spectral",
                            "Viridis" = "viridis"),
                selected = "default")),
            shiny::column(3,
              shiny::div(style = "margin-top:25px;",
                shiny::checkboxInput(ns("corrWhiteOnDark"),
                  shiny::tagList(shiny::icon("adjust"), " Texte blanc sur cellules sombres"),
                  value = TRUE))),
            shiny::column(2,
              shiny::div(style = "margin-top:8px;",
                colourInput(ns("corrCoefColor"), "Couleur coefficients",
                                          value = "#000000"))),
            shiny::column(2,
              shiny::div(style = "margin-top:8px;",
                colourInput(ns("corrPvalColorSig"), "Couleur p-value (signif.)",
                                          value = "#1a7a1a"))),
            shiny::column(2,
              shiny::div(style = "margin-top:8px;",
                colourInput(ns("corrPvalColorNs"), "Couleur p-value (non signif.)",
                                          value = "#999999")))
          ),
          shiny::hr(),
          shiny::div(style = "text-align:center;margin:15px 0;",
            shiny::downloadButton(ns("downloadCorrPlot"),
              shiny::tagList(shiny::icon("download"), " Télécharger l'image"), class = "btn-info")),
          shiny::div(style = "background:white;padding:20px;border-radius:5px;box-shadow:0 2px 4px rgba(0,0,0,0.1);",
            shiny::plotOutput(ns("corrPlot"), height = "620px"))
      )
    )
  )
}

# Serveur associé (appelé depuis le serveur principal sur le même id "corrélation")
mod_correlation_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$corTestVarSelect <- shiny::renderUI({
      shiny::req(values$data)
      num_cols <- names(values$data)[vapply(values$data, is.numeric, logical(1))]
      if (length(num_cols) < 2)
        return(shiny::div(class = "alert alert-warning", shiny::icon("exclamation-triangle"),
                   " Au moins deux variables numériques sont nécessaires."))
      shiny::tagList(
        if (requireNamespace("shinyWidgets", quietly = TRUE))
          pickerInput(ns("corTestVars"),
            "Variables numériques (\u2265 2)", choices = num_cols,
            selected = num_cols[seq_len(min(5, length(num_cols)))],
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE))
        else
          shiny::selectInput(ns("corTestVars"), "Variables numériques (\u2265 2)",
            choices = num_cols,
            selected = num_cols[seq_len(min(5, length(num_cols)))],
            multiple = TRUE)
      )
    })

    output$corTestTargetSelect <- shiny::renderUI({
      shiny::req(values$data, input$corTestVars)
      sel <- input$corTestVars
      if (length(sel) < 2) return(NULL)
      shiny::selectInput(ns("corTestTarget"),
        shiny::tagList(shiny::icon("bullseye"), " Variable cible (corrélée à toutes les autres)"),
        choices = sel, selected = sel[1])
    })

    cor_results <- shiny::reactive({
      shiny::req(values$data, input$corTestVars)
      shiny::validate(shiny::need(length(input$corTestVars) >= 2,
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
    shiny::observeEvent(cor_results(), {
      res <- tryCatch(cor_results(), error = function(e) NULL)
      if (is.null(res) || !NROW(res)) return()
      hstat_ai_capture(values, "Corrélations",
        trf("Tests de corrélation (%s)",
                paste(unique(if ("Methode" %in% names(res)) res$Methode
                             else input$corTestMethod %||% "pearson"), collapse = ", ")),
        tables = list("Corrélations par paire" = res),
        meta = list(variables = input$corTestVars,
                    `correction des p` = input$corTestAdjust,
                    `niveau de confiance` = input$corTestConf))
    }, ignoreInit = TRUE)

    output$corTestTable <- DT::renderDT({
      res <- cor_results()
      shiny::req(!is.null(res))
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

    output$corTestDownload <- shiny::downloadHandler(
      filename = function() paste0("corrélations_", Sys.Date(), ".csv"),
      content  = function(file) {
        res <- cor_results()
        utils::write.csv(res, file, row.names = FALSE, fileEncoding = "UTF-8")
      })

    output$corTestInterpretation <- shiny::renderUI({
      res <- cor_results()
      shiny::req(!is.null(res))
      sig <- res[!is.na(res$p_ajuste) & res$p_ajuste < 0.05, , drop = FALSE]
      if (nrow(sig) == 0)
        return(shiny::div(class = "alert alert-info", shiny::icon("info-circle"),
                   " Aucune corrélation significative après correction."))
      items <- lapply(seq_len(nrow(sig)), function(i) {
        r <- sig[i, ]
        shiny::tags$li(shiny::HTML(sprintf(
          "<b>%s \u2013 %s</b> (%s) : r = %.3f, %s, %s (p<sub>ajust</sub> = %s)",
          r$Variable_X, r$Variable_Y, r$Methode, r$Coefficient,
          tolower(r$Force), tolower(r$Sens), format(r$p_ajuste))))
      })
      shiny::div(class = "alert alert-success",
          shiny::strong(shiny::icon("check-circle"), " Corrélations significatives :"),
          shiny::tags$ul(items))
    })

    # ---------- Matrice de Corrélation (deplacee depuis Exploration) ----------
    output$corrVarSelect <- shiny::renderUI({
      shiny::req(values$data)
      num_cols <- names(values$data)[vapply(values$data, is.numeric, logical(1))]
      if (length(num_cols) < 2)
        return(shiny::div(class = "alert alert-warning", shiny::icon("exclamation-triangle"),
                   " Au moins deux variables numériques sont nécessaires."))
      shiny::tagList(
        if (requireNamespace("shinyWidgets", quietly = TRUE))
          pickerInput(ns("corrVars"), "Variables numériques",
            choices = num_cols, multiple = TRUE,
            selected = num_cols[seq_len(min(6, length(num_cols)))],
            options = list(`actions-box` = TRUE, `live-search` = TRUE))
        else
          shiny::selectInput(ns("corrVars"), "Variables numériques", choices = num_cols,
            multiple = TRUE, selected = num_cols[seq_len(min(6, length(num_cols)))])
      )
    })

    output$corrFocusSelect <- shiny::renderUI({
      shiny::req(values$data, input$corrVars)
      sel <- input$corrVars
      if (length(sel) < 2) return(NULL)
      shiny::selectInput(ns("corrFocusVar"),
        shiny::tagList(shiny::icon("bullseye"), " Variable cible (corrélée à toutes les autres)"),
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
        shiny::showNotification(trf("Corrélation : variable(s) non numérique(s) ignorée(s) : %s.",
                                 paste(dropped, collapse = ", ")), type = "message", duration = 5)
      }
      cor_data <- remove_zero_var_cols(cor_data)
      if (ncol(cor_data) < 2) {
        shiny::showNotification("Moins de 2 variables numériques à variance non nulle.", type = "warning")
        return(invisible())
      }
      # Retire les colonnes parfaitement colineaires (|r| ~ 1) qui rendent la matrice
      # singuliere et font echouer le reordonnancement hierarchique (solve()).
      if (identical(method, "kendall"))
        cor_data <- hstat_cap_df_rows(cor_data, HSTAT_KENDALL_MAX_N, "Corrélation de Kendall")
      cm0 <- suppressWarnings(stats::cor(cor_data, use = "pairwise.complete.obs", method = method))
      if (!is.null(cm0)) {
        cm0[is.na(cm0)] <- 0
        drop_idx <- integer(0)
        for (i in 2:ncol(cm0)) {
          if (any(abs(cm0[i, seq_len(i - 1)]) > 0.9999)) drop_idx <- c(drop_idx, i)
        }
        if (length(drop_idx) > 0) {
          shiny::showNotification(trf("Corrélation : %d variable(s) parfaitement colinéaire(s) retirée(s).",
                                   length(drop_idx)), type = "message", duration = 5)
          cor_data <- cor_data[, -drop_idx, drop = FALSE]
        }
      }
      if (ncol(cor_data) < 2) {
        shiny::showNotification("Moins de 2 variables non colinéaires.", type = "warning")
        return(invisible())
      }
      if (identical(method, "kendall"))
        cor_data <- hstat_cap_df_rows(cor_data, HSTAT_KENDALL_MAX_N, "Corrélation de Kendall")
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
                    else trf("Matrice de corrélation - %s", method_label)
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

    corr_params <- shiny::reactive({
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

    output$corrPlot <- shiny::renderPlot({
      shiny::req(values$data, input$corrVars)
      p <- corr_params()
      fv <- if (isTRUE(input$corrFocusMode)) input$corrFocusVar else NULL
      tryCatch(
        generate_corr_matrix_plot(values$data, input$corrVars, p$method, p$display,
          p$type, p$label_size, p$text_size, p$title, p$pval_mode, p$sig_level,
          p$pval_size, p$reorder, p$palette, p$coef_color, p$pval_color_sig,
          p$pval_color_ns, p$white_on_dark, focus_var = fv),
        error = function(e) shiny::showNotification(hstat_err_fr(e, "Erreur matrice"),
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

    output$downloadCorrPlot <- shiny::downloadHandler(
      filename = function() {
        paste0("matrice_corrélation_", Sys.Date(), ".",
               hstat_img_fmt(input$corrFormat))
      },
      content = function(file) {
        shiny::req(values$data, input$corrVars)
        p <- corr_params()
        fv <- if (isTRUE(input$corrFocusMode)) input$corrFocusVar else NULL
        fmt <- hstat_img_fmt(input$corrFormat)
        dpi <- .hstat_num1(input$corrDPI, 300)
        size_in <- .hstat_num1(input$corrSizeIn, 8)
        # La taille en pixels s'adapte au DPI choisi : pixels = pouces x DPI.
        px <- round(size_in * dpi)
        draw <- function() generate_corr_matrix_plot(values$data, input$corrVars, p$method, p$display,
            p$type, p$label_size, p$text_size, p$title, p$pval_mode, p$sig_level,
            p$pval_size, p$reorder, p$palette, p$coef_color, p$pval_color_sig,
            p$pval_color_ns, p$white_on_dark, focus_var = fv)
        # Un seul chemin d'ecriture, qui garantit un fichier valide du format
        # demande : les sept branches de peripheriques qui vivaient ici
        # laissaient un fichier vide quand le trace levait, et Shiny renvoyait
        # alors sa page d'erreur HTML sous le nom demande.
        hstat_ecrire_image(file, draw, fmt, size_in, size_in, dpi)
        shiny::showNotification(trf("Graphique téléchargé (%s, %d DPI).", toupper(fmt), dpi),
                         type = "message", duration = 3)
      })
  })
}


mod_tests_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # Theme ggplot2 (utilise par les graphiques du Chi2). viz_get_theme est global.
  get_plot_theme <- function(base_size = 12) {
    viz_get_theme(input$plotTheme %||% "minimal", base_size = base_size)
  }

  output$responseVarSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    shiny::tagList(
      pickerInput(ns("responseVar"), "Variable(s) réponse :", 
                  choices = num_cols, 
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      shiny::actionButton(ns("selectAllResponse"), "Tout sélectionner", class = "btn-success btn-sm"),
      shiny::actionButton(ns("deselectAllResponse"), "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  shiny::observeEvent(input$selectAllResponse, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "responseVar", selected = num_cols)
  })
  
  shiny::observeEvent(input$deselectAllResponse, {
    updatePickerInput(session, "responseVar", selected = character(0))
  })
  
  
  # Bloc 1 : Sélecteur de variables à transformer
  output$transformVarSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
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
  output$transformFeasibilityCheck <- shiny::renderUI({
    shiny::req(input$transformVar, input$transformMethod, values$filteredData)
    checks <- lapply(input$transformVar, function(var) {
      x     <- values$filteredData[[var]]
      check <- check_transformation_feasibility(x, input$transformMethod)
      icon_el <- if (check$ok) shiny::icon("check-circle", style = "color:#4caf50;")
      else           shiny::icon("times-circle", style = "color:#e53935;")
      bg_col  <- if (check$ok) "#e8f5e9" else "#ffebee"
      txt_col <- if (check$ok) "#1b5e20" else "#b71c1c"
      shiny::div(
        style = paste0("display:flex;align-items:center;padding:4px 8px;",
                       "background:", bg_col, ";border-radius:4px;margin-bottom:3px;"),
        icon_el,
        shiny::tags$span(style = paste0("font-size:12px;margin-left:6px;color:", txt_col, ";"),
                  shiny::tags$b(var), " — ", check$message)
      )
    })
    shiny::tagList(
      shiny::div(style = "font-size:11px;font-weight:bold;color:#555;margin-bottom:4px;",
          shiny::icon("clipboard-check"), " Vérification de faisabilité :"),
      shiny::tagList(checks)
    )
  })
  
  # Bloc 3 : Application de la transformation
  shiny::observeEvent(input$applyTransformation, {
    shiny::req(input$transformVar, input$transformMethod, values$filteredData)
    if (length(input$transformVar) == 0) {
      shiny::showNotification("Sélectionnez au moins une variable à transformer.", type = "warning")
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
      shiny::showNotification(shiny::HTML(paste0("<b>Erreur(s):</b><br>", paste(errors, collapse = "<br>"))),
                       type = "error", duration = 12)
    if (length(skipped) > 0)
      shiny::showNotification(trf("Déjà existante(s) : %s", paste(skipped, collapse = ", ")),
                       type = "warning", duration = 5)
    if (length(added) > 0) {
      values$filteredData      <- df
      values$transformationLog <- log_entries
      shiny::showNotification(
        shiny::HTML(paste0("<b>Transformation appliquée</b><br>",
                    length(added), " variable(s) créée(s): ",
                    paste0("<b>", added, "</b>", collapse = ", "))),
        type = "message", duration = 5)
    }
  })
  
  # Bloc 4 : Suppression d'une transformation
  shiny::observeEvent(input$removeTransformation, {
    shiny::req(input$removeTransformVar, values$filteredData)
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
      shiny::showNotification(trf("Supprimée(s): %s", paste(removed, collapse = ", ")),
                       type = "warning", duration = 3)
    }
  })
  
  # Bloc 5 : Journal des transformations actives
  output$transformationLogDisplay <- shiny::renderUI({
    log <- values$transformationLog
    if (is.null(log) || length(log) == 0) {
      return(shiny::div(
        style = paste0("padding:10px;background:#f5f5f5;border-radius:4px;",
                       "font-size:12px;color:#888;text-align:center;border:1px dashed #ccc;"),
        shiny::icon("info-circle"), " Aucune transformation active"))
    }
    entries <- lapply(names(log), function(vname) {
      entry <- log[[vname]]
      lambda_tag <- if (!is.null(entry$lambda))
        shiny::tags$span(style = "font-size:10px;color:#757575;margin-left:4px;",
                  paste0("λ = ", entry$lambda)) else NULL
      shiny::div(
        style = paste0("display:flex;flex-wrap:wrap;align-items:center;",
                       "padding:5px 8px;background:#e8f5e9;",
                       "border-left:3px solid #4caf50;border-radius:3px;margin-bottom:4px;"),
        shiny::icon("check-circle", style = "color:#4caf50;flex-shrink:0;"),
        shiny::div(style = "margin-left:6px;flex:1;",
            shiny::tags$span(style = "font-size:12px;font-weight:bold;color:#2e7d32;",
                      entry$original, " → ", vname), shiny::tags$br(),
            shiny::tags$span(style = "font-size:11px;color:#555;font-family:monospace;",
                      entry$formula),
            lambda_tag,
            shiny::tags$span(style = "font-size:10px;color:#9e9e9e;margin-left:6px;",
                      paste0("@ ", entry$applied_at))
        )
      )
    })
    shiny::tagList(
      shiny::div(style = "margin-bottom:6px;",
          shiny::tags$b(style = "font-size:12px;color:#1b5e20;",
                 shiny::icon("history"), " ", length(log), " transformation(s) active(s)")),
      shiny::tagList(entries)
    )
  })
  
  # Bloc 6 : Sélecteur de suppression
  output$removeTransformSelect <- shiny::renderUI({
    log <- values$transformationLog
    if (is.null(log) || length(log) == 0) return(NULL)
    shiny::tagList(
      shiny::hr(style = "margin:8px 0;"),
      pickerInput(ns("removeTransformVar"), "Supprimer des transformations :",
                  choices = names(log), multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      shiny::actionButton(ns("removeTransformation"), "Supprimer la sélection",
                   class = "btn-danger btn-sm btn-block", icon = shiny::icon("trash"))
    )
  })
  
  output$factorVarSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    fac_cols <- get_all_factor_candidates(values$filteredData)
    shiny::tagList(
      pickerInput(ns("factorVar"), "Facteur(s) :",
                  choices  = fac_cols,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE)),
      shiny::tags$small(style = "color:#6c757d; font-size:11px;",
                 shiny::icon("info-circle"), " Facteur, texte, date et numérique (<= 30 niveaux) acceptés"),
      shiny::actionButton(ns("selectAllFactors"),   "Tout sélectionner",   class = "btn-success btn-sm"),
      shiny::actionButton(ns("deselectAllFactors"), "Tout désélectionner", class = "btn-danger btn-sm")
    )
  })
  
  shiny::observeEvent(input$selectAllFactors, {
    updatePickerInput(session, "factorVar", selected = get_all_factor_candidates(values$filteredData))
  })
  
  shiny::observeEvent(input$deselectAllFactors, {
    updatePickerInput(session, "factorVar", selected = character(0))
  })
  
  shiny::observeEvent(input$testNormalityRaw, {
    shiny::req(input$responseVar)
    
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
      shiny::showNotification("Aucun résultat de normalité généré", type = "warning")
    }
  })
  
  shiny::observeEvent(input$testHomogeneityRaw, {
    shiny::req(input$responseVar, input$factorVar)
    
    if (length(input$factorVar) != 1) {
      shiny::showNotification("Le test d'homogénéité nécessite exactement un facteur", type = "warning")
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
        formula_str <- stats::as.formula(paste0("`", var, "` ~ `", fvar, "`"))
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
      shiny::showNotification("Aucun résultat d'homogénéité généré", type = "warning")
    }
  })
  
  shiny::observeEvent(input$testT, {
    shiny::req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      shiny::showNotification("Le test t nécessite un seul facteur", type = "warning")
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
        formula_str <- stats::as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        test_result <- stats::t.test(formula_str, data = values$filteredData)
        
        lm_model <- stats::lm(formula_str, data = values$filteredData)
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
      shiny::showNotification(
        if (length(motifs_refus))
          trf("Test t impossible : %s", paste(unique(motifs_refus), collapse = " "))
        else
          paste("Aucun résultat t-test généré. Vérifiez que les variables réponse",
                "sont numériques et comportent assez de données non manquantes."),
        type = "warning", duration = 12)
    }
  })
  
  shiny::observeEvent(input$testWilcox, {
    shiny::req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      shiny::showNotification("Le test de Wilcoxon nécessite un seul facteur", type = "warning")
      return()
    }
    
    results_list <- list()
    
    for (var in input$responseVar) {
      tryCatch({
        fvar <- input$factorVar[1]
        formula_str <- stats::as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        test_result <- stats::wilcox.test(formula_str, data = values$filteredData, exact = FALSE)
        
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
      shiny::showNotification("Aucun résultat Wilcoxon généré", type = "warning")
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
      shiny::showNotification("Sélectionnez au moins une variable réponse.", type = "warning")
      return()
    }
    mu <- suppressWarnings(as.numeric(input$refValue))[1]
    if (is.na(mu)) {
      shiny::showNotification("Renseignez une valeur de référence numérique.", type = "error")
      return()
    }
    sigma <- suppressWarnings(as.numeric(input$refSigma))[1]
    if (need_sigma && (is.na(sigma) || sigma <= 0)) {
      shiny::showNotification(paste("Ce test exige un écart-type de référence",
                             "strictement positif."), type = "error")
      return()
    }
    margin <- suppressWarnings(as.numeric(input$refMargin))[1]
    if (need_margin && (is.na(margin) || margin <= 0)) {
      shiny::showNotification(paste("Le test d'équivalence exige une marge",
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
          Facteur = trf("Référence = %s", format(mu)),
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
      shiny::showNotification("Aucun résultat généré.", type = "warning")
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
    shiny::showNotification(
      if (is_tost)
        trf("%d test(s) d'équivalence : équivalence démontrée pour %d variable(s).",
                length(rows), n_sig)
      else
        trf("%d test(s) de conformité : %d écart(s) significatif(s) à la norme.",
                length(rows), n_sig),
      type = "message", duration = 5)
  }

  shiny::observeEvent(input$testRefT,      .run_ref_quanti("ttest"))
  shiny::observeEvent(input$testRefZ,      .run_ref_quanti("ztest", need_sigma = TRUE))
  shiny::observeEvent(input$testRefWilcox, .run_ref_quanti("wilcoxon"))
  shiny::observeEvent(input$testRefSign,   .run_ref_quanti("sign"))
  shiny::observeEvent(input$testRefVar,    .run_ref_quanti("variance", need_sigma = TRUE))
  shiny::observeEvent(input$testRefTOST,   .run_ref_quanti("tost", need_margin = TRUE))

  # --- Proportion / taux vs référence ---------------------------------------
  output$refPropVarSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    df <- values$filteredData
    cat_cols <- names(df)[sapply(df, function(x)
      is.factor(x) || is.character(x) || is.logical(x) ||
      (is.numeric(x) && length(unique(stats::na.omit(x))) <= 20))]
    if (length(cat_cols) == 0)
      return(shiny::div(style = "color:#c0392b; font-size:12px;",
                 "Aucune variable catégorielle disponible."))
    pickerInput(ns("refPropVar"),
                shiny::tagList(shiny::icon("tag"), " Variable catégorielle :"),
                choices = cat_cols, multiple = FALSE,
                options = list(`live-search` = TRUE))
  })

  output$refPropLevelSelect <- shiny::renderUI({
    shiny::req(values$filteredData, input$refPropVar)
    lv <- unique(stats::na.omit(as.character(values$filteredData[[input$refPropVar]])))
    if (length(lv) == 0) return(NULL)
    shiny::selectInput(ns("refPropLevel"),
                shiny::tagList(shiny::icon("check"), " Modalité comptée (« succès ») :"),
                choices = sort(lv))
  })

  .run_ref_prop <- function(method) {
    if (is.null(input$refPropVar) || is.null(input$refPropLevel)) {
      shiny::showNotification(paste("Choisissez une variable catégorielle et la",
                             "modalité à compter."), type = "warning")
      return()
    }
    p0 <- suppressWarnings(as.numeric(input$refPropP0))[1]
    if (is.na(p0) || p0 <= 0) {
      shiny::showNotification("Renseignez une proportion (ou un taux) de référence positive.",
                       type = "error")
      return()
    }
    v <- as.character(values$filteredData[[input$refPropVar]])
    v <- v[!is.na(v)]
    n <- length(v); k <- sum(v == input$refPropLevel)
    if (n == 0) {
      shiny::showNotification("Aucune observation valide pour cette variable.", type = "error")
      return()
    }
    res <- tryCatch(
      hstat_ref_prop_test(k, n, p0 = p0, method = method,
                          alternative = .ref_alt(), conf.level = .ref_conf()),
      error = function(e) hstat_err_fr(e))
    lbl <- paste0(input$refPropVar, " = ", input$refPropLevel)
    if (is.character(res)) {
      shiny::showNotification(res, type = "error", duration = 12)
      return()
    }
    row <- hstat_ref_result_row(res, lbl,
             reference_label = trf("Référence = %s", format(p0)))
    det <- .ref_detail_row(res, lbl)
    det$n <- n
    .push_ref_results(stats::setNames(list(row), lbl),
                      stats::setNames(list(det), lbl),
                      if (is.na(res$note)) character(0) else res$note)
  }

  shiny::observeEvent(input$testRefBinom,   .run_ref_prop("binom"))
  shiny::observeEvent(input$testRefProp,    .run_ref_prop("prop"))
  shiny::observeEvent(input$testRefPoisson, .run_ref_prop("poisson"))

  # Le détail ne s'affiche que tant que le dernier test lancé est bien un test
  # de conformité : sinon il resterait à l'écran après un autre test.
  output$hasRefTest <- shiny::reactive(
    !is.null(values$refTestDetails) &&
    identical(values$currentTestType, "reference"))
  shiny::outputOptions(output, "hasRefTest", suspendWhenHidden = FALSE)

  output$refTestDetails <- DT::renderDT({
    shiny::req(values$refTestDetails, identical(values$currentTestType, "reference"))
    d <- values$refTestDetails
    d$p_value <- sapply(d$p_value, function(p) if (is.na(p)) NA_character_ else fmt_p(p))
    DT::datatable(d, rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE, pageLength = 25))
  })

  output$refTestNote <- shiny::renderUI({
    n <- values$refTestNotes
    if (!identical(values$currentTestType, "reference")) return(NULL)
    if (is.null(n) || length(n) == 0) return(NULL)
    shiny::div(class = "callout callout-info", style = "margin-top:8px;",
        shiny::icon("circle-info"), shiny::strong(" À propos de ce test : "),
        shiny::tags$ul(lapply(n, shiny::tags$li)))
  })

  shiny::observeEvent(input$testKruskal, {
    shiny::req(input$responseVar, input$factorVar)
    if (length(input$factorVar) > 1) {
      shiny::showNotification("Kruskal-Wallis nécessite un seul facteur", type = "warning")
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
        formula_str <- stats::as.formula(paste0("`", var, "` ~ `", fvar, "`"))
        test_result <- stats::kruskal.test(formula_str, data = df_kw)
        
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
      shiny::showNotification("Aucun résultat Kruskal-Wallis généré", type = "warning")
    }
  })
  
  shiny::observeEvent(input$testScheirerRayHare, {
    shiny::req(input$responseVar, input$factorVar)
    
    if (length(input$factorVar) < 2) {
      shiny::showNotification("Scheirer-Ray-Hare nécessite au moins 2 facteurs", type = "warning")
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
        test_data <- stats::na.omit(test_data)
        
        if (nrow(test_data) < 3) {
          error_messages <- c(error_messages, trf("%s : Pas assez de données après suppression des NA", var))
          next
        }
        
        safe_resp    <- "resp_var_srh"
        safe_factors <- paste0("factor_srh_", seq_along(input$factorVar))
        # Table de correspondance pour restaurer les noms dans les résultats
        factor_label_map <- stats::setNames(input$factorVar, safe_factors)
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
          formula_str <- stats::as.formula(paste0(safe_resp, " ~ ", paste(safe_factors, collapse = "*")))
        } else {
          formula_str <- stats::as.formula(paste0(safe_resp, " ~ ", paste(safe_factors, collapse = "+")))
        }
        
        test_result <- rcompanion::scheirerRayHare(formula_str, data = safe_data)
        
        # Restaurer les noms originaux dans les rownames du résultat
        orig_rnames <- rownames(test_result)
        for (sf in names(factor_label_map)) {
          orig_rnames <- gsub(sf, factor_label_map[sf], orig_rnames, fixed = TRUE)
        }
        rownames(test_result) <- orig_rnames
        
        if (is.null(test_result) || nrow(test_result) == 0) {
          error_messages <- c(error_messages, trf("%s : Test n'a produit aucun résultat", var))
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
          error_messages <- c(error_messages, trf("%s : Aucun effet trouvé (uniquement des résidus)", var))
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
      shiny::showNotification(
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
      
      shiny::showNotification(
        trf("Test Scheirer-Ray-Hare terminé : %s résultat(s) généré(s)", length(results_list)),
        type = "message",
        duration = 3
      )
    } else {
      shiny::showNotification("Aucun résultat Scheirer-Ray-Hare généré. Vérifiez vos données et facteurs.", type = "error", duration = 10)
    }
  })
  
  shiny::observeEvent(input$testANOVA, {
    shiny::req(input$responseVar, input$factorVar)
    
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
          shiny::showNotification(trf("ANOVA : '%s' non numérique -- ignorée.", var), type = "warning", duration = 5)
          next
        }
        df_clean <- df[, c(var, input$factorVar), drop = FALSE]
        df_clean <- df_clean[stats::complete.cases(df_clean), ]
        if (nrow(df_clean) < 4) { shiny::showNotification(trf("ANOVA : trop peu d'obs pour '%s'.", var), type = "warning", duration = 4); next }
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
        model <- stats::aov(stats::as.formula(formula_str), data = df_clean)
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
        
        # ICI LE tryCatch ENVELOPPE TOUTE LA BOUCLE : une seule variable
        # degeneree emporterait l'ANOVA de TOUTES les autres. On garde donc
        # chaque diagnostic separement, comme le fait deja l'onglet des
        # residus (`sd(...) < 1e-10`), et l'ANOVA survit.
        residuals_data <- stats::residuals(model)
        if (length(residuals_data) > 3 && stats::sd(residuals_data) > 1e-10) {
          normality_results[[var]] <- tryCatch(hstat_shapiro(residuals_data),
                                               error = function(e) NULL)
        }
        
        fitted_data <- stats::fitted(model)
        # Deux ecueils : des valeurs ajustees constantes ne donnent qu'un seul
        # niveau, et leveneTest exige au moins deux groupes (« contrasts can be
        # applied only to factors with 2 or more levels »).
        fitted_factor <- tryCatch(
          cut(fitted_data, breaks = 2, labels = c("Bas", "Haut")),
          error = function(e) NULL)
        if (!is.null(fitted_factor) &&
            length(unique(stats::na.omit(fitted_factor))) >= 2) {
          test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
          homogeneity_results[[var]] <- tryCatch(
            car::leveneTest(residuals ~ fitted_group, data = test_data),
            error = function(e) NULL)
        }
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
        shiny::showNotification("Aucun résultat ANOVA généré", type = "warning")
      }
      
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur ANOVA"), type = "error")
    })
  })
  
  shiny::observeEvent(input$testLM, {
    shiny::req(input$responseVar, input$factorVar)
    
    results_list <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (var in input$responseVar) {
        formula_str <- paste0("`", var, "` ~ ", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+"))
        model <- stats::lm(stats::as.formula(formula_str), data = df)
        summary_model <- summary(model)
        
        model_list[[var]] <- model
        
        results_list[[paste(var, "global", sep = "_")]] <- data.frame(
          Test = "Régression linéaire",
          Variable = var,
          Facteur = "Modèle global",
          Statistique = round(summary_model$fstatistic[1], 4),
          ddl = paste(summary_model$fstatistic[2], ",", summary_model$fstatistic[3]),
          p_value = stats::pf(summary_model$fstatistic[1], summary_model$fstatistic[2], 
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
        shiny::showNotification("Aucun résultat de régression généré", type = "warning")
      }
      
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur régression"), type = "error")
    })
  })
  
  shiny::observeEvent(input$testGLM, {
    shiny::req(input$responseVar, input$factorVar)
    
    results_list <- list()
    model_list <- list()
    
    tryCatch({
      df <- values$filteredData
      for (var in input$responseVar) {
        formula_str <- paste0("`", var, "` ~ ", paste(sapply(input$factorVar, function(x) paste0("`", x, "`")), collapse = "+"))
        gl <- hstat_glm_fit(stats::as.formula(formula_str), data = df, family = stats::gaussian())
        model <- gl$fit
        if (!is.null(gl$note))
          shiny::showNotification(paste0("GLM (", var, ") : ", gl$note),
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
          shiny::showNotification(
            trf("GLM (%s) : le modèle est vide.", var),
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
        shiny::showNotification("Aucun résultat GLM généré", type = "warning")
      }
      
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur GLM"), type = "error")
    })
  })
  
  # --- Infobulle dynamique : type de donnees correspondant a la famille choisie ---
  output$glmmFamilyHelp <- shiny::renderUI({
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
        titre = "Bêta (glmmTMB)",
        txt   = "Proportions/taux continus strictement dans ]0,1[ (0 et 1 exclus), NON issus d'un comptage : taux de couverture, fraction de surface atteinte, indices bornés. Lien : logit."),
      "tweedie" = list(
        titre = "Tweedie (glmmTMB)",
        txt   = "Données continues positives avec excès de zéros exacts : biomasse souvent nulle puis continue, captures, précipitations. Interpole entre Poisson et Gamma."),
      list(titre = fam, txt = "")
    )
    shiny::div(style = "background:#e8f4fd; border-left:4px solid #1565C0; border-radius:6px; padding:8px 10px; margin:6px 0 4px 0;",
        shiny::div(style = "font-size:11px; font-weight:bold; color:#0d47a1; margin-bottom:2px;",
            shiny::icon("circle-info"), " ", info$titre),
        shiny::div(style = "font-size:11px; color:#37474f; line-height:1.4;", info$txt))
  })
  
  # --- Infobulle dynamique : quand utiliser la fonction de lien choisie ---
  output$glmmLinkHelp <- shiny::renderUI({
    lk  <- input$glmmLink %||% "auto"
    info <- switch(lk,
      "auto" = list(
        titre = "Automatique (lien canonique)",
        txt   = "À privilégier par défaut : applique le lien naturel de la famille (identity pour gaussienne, logit pour binomiale/Bêta, log pour Poisson/Gamma/nbinom, 1/µ² pour inverse gaussienne). Estimation stable, interprétation cohérente."),
      "identity" = list(
        titre = "identity (µ = prédicteur)",
        txt   = "Effets ADDITIFS, coefficients directement dans l'unité de Y. Lien naturel de la gaussienne. À éviter avec familles bornées (Poisson, binomiale) : peut prédire des valeurs impossibles."),
      "log" = list(
        titre = "log",
        txt   = "Effets MULTIPLICATIFS : exp(coef) = ratio (facteur multiplicatif). Canonique pour Poisson, usuel pour Gamma et binomiale négative. Pour comptages/réponses positives ; garantit des prédictions > 0."),
      "logit" = list(
        titre = "logit (log-odds)",
        txt   = "Pour probabilités/proportions : exp(coef) = odds ratio. Choix par défaut des données binaires (présence/absence) et taux dans ]0,1[. Courbe symétrique autour de p = 0,5. Canonique : binomiale, Bêta."),
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
    shiny::div(style = "background:#fff8e1; border-left:4px solid #f9a825; border-radius:6px; padding:8px 10px; margin:4px 0 6px 0;",
        shiny::div(style = "font-size:11px; font-weight:bold; color:#e65100; margin-bottom:2px;",
            shiny::icon("link"), " ", info$titre),
        shiny::div(style = "font-size:11px; color:#5d4037; line-height:1.4;", info$txt),
        shiny::div(style = "font-size:10px; color:#9e9e9e; margin-top:4px; font-style:italic;",
            shiny::icon("triangle-exclamation"),
            " Tous les liens ne sont pas compatibles avec toutes les familles (R renverra une erreur sinon)."))
  })
  
  # --- Mise a jour des choix des selecteurs de mesures repetees (UI statique) ---
  shiny::observe({
    df <- values$filteredData %||% values$cleanData %||% values$data
    shiny::req(df)
    all_cols <- names(df)
    # LES DATES SONT ELIGIBLES, et c'est le cas le plus courant : une periode
    # repetee est presque toujours une date de mesure. Le filtre ne retenait que
    # facteurs, chaines et numeriques a peu de modalites -- une colonne `Date`
    # n'est aucun des trois, et n'apparaissait donc JAMAIS dans la liste, alors
    # que l'exemple affiche sous le champ annonce « date ».
    #
    # L'ordre est celui du temps : `factor()` sur une `Date` classe ses niveaux
    # par la valeur sous-jacente, donc chronologiquement, et les etiquette au
    # format ISO. C'est ce que la suite de l'analyse attend.
    est_date <- function(x) inherits(x, c("Date", "POSIXct", "POSIXlt"))
    fac_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x) ||
                                   est_date(x) ||
                                   (is.numeric(x) && length(unique(x[!is.na(x)])) <= 20))]
    if (length(fac_cols) == 0) fac_cols <- all_cols
    shiny::updateSelectInput(session, "rmSubject", choices = all_cols,
                      selected = shiny::isolate(input$rmSubject) %||% all_cols[1])
    shiny::updateSelectizeInput(session, "rmWithin", choices = fac_cols,
                         selected = shiny::isolate(input$rmWithin))
    shiny::updateSelectizeInput(session, "rmBetween", choices = fac_cols,
                         selected = shiny::isolate(input$rmBetween))
  })
  
  # --- Selecteur COMPLET de la structure d'effets aleatoires du GLMM ---
  # Interface guidee : groupes croises, emboitement, pentes aleatoires,
  # + saisie libre prioritaire. Un apercu de la formule est affiche.
  output$glmmRandomSelect <- shiny::renderUI({
    df <- values$filteredData %||% values$cleanData %||% values$data
    shiny::req(df)
    cand <- tryCatch(get_all_factor_candidates(df), error = function(e) NULL)
    if (is.null(cand) || length(cand) == 0) {
      cand <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    }
    if (length(cand) == 0) cand <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    shiny::tagList(
      shiny::selectizeInput(ns("glmmRandom"),
                     shiny::tagList(shiny::icon("layer-group"), " Variable(s) de groupement :"),
                     choices = cand, selected = cand[1], multiple = TRUE,
                     options = list(plugins = list("remove_button"),
                                    placeholder = "Un ou plusieurs groupes...")),
      # 2) Emboitement (nested) au lieu de croise
      shiny::conditionalPanel(
        ns = ns,
        condition = "input.glmmRandom && input.glmmRandom.length > 1",
        shiny::checkboxInput(ns("glmmNested"),
                      shiny::tagList(shiny::icon("diagram-next"), " Emboîter les groupes (A/B/...) au lieu de croisés"),
                      value = FALSE),
        shiny::div(style = "font-size:10px; color:#7f8c8d; margin:-4px 0 6px 0;",
            "Croisés : ", shiny::tags$code("(1|A) + (1|B)"), " — Emboîtés : ", shiny::tags$code("(1|A/B)"), ".")
      ),
      shiny::checkboxInput(ns("glmmUseSlope"),
                    shiny::tagList(shiny::icon("chart-line"), " Ajouter une pente aléatoire"),
                    value = FALSE),
      shiny::conditionalPanel(
        ns = ns,
        condition = "input.glmmUseSlope == true",
        if (length(num_cols) > 0) {
          shiny::selectInput(ns("glmmSlopeVar"), "Variable de pente (continue) :",
                      choices = num_cols, selected = num_cols[1])
        } else {
          shiny::div(class = "alert alert-warning", style = "padding:6px; font-size:11px;",
              shiny::icon("exclamation-triangle"), " Aucune variable continue pour une pente.")
        },
        shiny::checkboxInput(ns("glmmCorrSlope"),
                      "Corréler pente et ordonnée à l'origine",
                      value = TRUE),
        shiny::div(style = "font-size:10px; color:#7f8c8d; margin:-4px 0 6px 0;",
            "Corrélés : ", shiny::tags$code("(1+X|g)"), " — Indépendants : ", shiny::tags$code("(1|g) + (0+X|g)"), ".")
      ),
      shiny::hr(style = "margin:8px 0;"),
      shiny::textInput(ns("glmmRandomCustom"),
                shiny::tagList(shiny::icon("pen"), " Formule libre (avancé, prioritaire) :"),
                value = "", placeholder = "ex. (1|Bloc) + (1+Dose|Site)"),
      shiny::div(style = "font-size:10px; color:#7f8c8d; margin-top:-4px;",
          "Si renseigné, ce champ remplace l'interface guidée ci-dessus."),
      # 5) Apercu de la formule complete
      shiny::div(style = "background:#ede7f6; border:1px solid #b39ddb; border-radius:6px; padding:8px 10px; margin-top:8px;",
          shiny::div(style = "font-size:10px; color:#6c3483; font-weight:bold; margin-bottom:3px;",
              shiny::icon("eye"), " Aperçu de la formule :"),
          shiny::uiOutput(ns("glmmFormulaPreview")))
    )
  })
  
  # Construit le terme d'effets aleatoires a partir de l'interface OU du champ libre.
  # Source unique de verite : utilisee par l'apercu ET par l'observer.
  glmm_random_term <- shiny::reactive({
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
  output$glmmFormulaPreview <- shiny::renderUI({
    rand <- glmm_random_term()
    resp <- if (!is.null(input$responseVar) && length(input$responseVar) > 0) input$responseVar[1] else "réponse"
    fixed <- if (!is.null(input$factorVar) && length(input$factorVar) > 0) {
      paste(sapply(input$factorVar, function(x) paste0("`", x, "`")),
            collapse = if (isTRUE(input$interaction)) " * " else " + ")
    } else "facteur"
    if (is.null(rand) || !nzchar(rand)) {
      return(shiny::div(style = "font-family:monospace; font-size:11px; color:#c0392b;",
                 shiny::icon("triangle-exclamation"), " Sélectionnez au moins une variable de groupement."))
    }
    shiny::div(style = "font-family:monospace; font-size:12px; color:#4a235a; word-break:break-all;",
        paste0("`", resp, "` ~ ", fixed, " + ", rand))
  })
  
  # --- Modele lineaire (generalise) mixte : lme4 ou glmmTMB ---
  shiny::observeEvent(input$testGLMM, {
    shiny::req(input$responseVar, input$factorVar)
    
    df <- values$filteredData %||% values$cleanData %||% values$data
    if (is.null(df)) { shiny::showNotification("Aucune donnée disponible.", type = "error"); return() }
    
    rand_term <- glmm_random_term()
    if (is.null(rand_term) || !nzchar(rand_term)) {
      shiny::showNotification("Définissez la structure d'effets aléatoires (panneau « Modèle mixte ») : au moins une variable de groupement ou une formule libre.",
                       type = "warning", duration = 6)
      return()
    }
    # Verifier que les variables de groupement existent (sauf si formule libre,
    # ou l'utilisateur est responsable de la syntaxe).
    custom_used <- !is.null(input$glmmRandomCustom) && nzchar(trimws(input$glmmRandomCustom))
    if (!custom_used) {
      missing_grp <- setdiff(input$glmmRandom, names(df))
      if (length(missing_grp) > 0) {
        shiny::showNotification(trf("Variable(s) d'effet aléatoire introuvable(s) : %s", paste(missing_grp, collapse = ", ")), type = "error")
        return()
      }
    }
    
    engine <- input$glmmEngine %||% "lme4"
    fam    <- input$glmmFamily %||% "gaussian"
    link   <- input$glmmLink   %||% "auto"
    
    # Verifier la disponibilite du moteur demande.
    if (engine == "lme4" && !requireNamespace("lme4", quietly = TRUE)) {
      shiny::showNotification("Le package 'lme4' n'est pas installé.", type = "error"); return()
    }
    if (engine == "glmmTMB" && !requireNamespace("glmmTMB", quietly = TRUE)) {
      shiny::showNotification("Le package 'glmmTMB' n'est pas installé.", type = "error"); return()
    }
    # Certaines familles ne sont disponibles que via glmmTMB.
    if (engine == "lme4" && fam %in% c("nbinom", "beta_family", "tweedie")) {
      shiny::showNotification(trf("La famille « %s » nécessite le moteur glmmTMB. Changez de moteur.", fam),
                       type = "warning", duration = 7); return()
    }
    
    # Construit l'objet famille selon le moteur, la famille et le lien choisis.
    build_family <- function(engine, fam, link) {
      lk <- if (identical(link, "auto")) NULL else link
      if (engine == "glmmTMB") {
        switch(fam,
          "gaussian"         = if (is.null(lk)) stats::gaussian()         else stats::gaussian(link = lk),
          "binomial"         = if (is.null(lk)) stats::binomial()         else stats::binomial(link = lk),
          "poisson"          = if (is.null(lk)) stats::poisson()          else stats::poisson(link = lk),
          "Gamma"            = if (is.null(lk)) stats::Gamma(link = "log")else stats::Gamma(link = lk),
          "inverse.gaussian" = if (is.null(lk)) stats::inverse.gaussian() else stats::inverse.gaussian(link = lk),
          "nbinom"           = if (is.null(lk)) glmmTMB::nbinom2()          else glmmTMB::nbinom2(link = lk),
          "beta_family"      = if (is.null(lk)) glmmTMB::beta_family()      else glmmTMB::beta_family(link = lk),
          "tweedie"          = if (is.null(lk)) glmmTMB::tweedie()          else glmmTMB::tweedie(link = lk),
          stats::gaussian())
      } else {
        switch(fam,
          "gaussian"         = if (is.null(lk)) stats::gaussian()         else stats::gaussian(link = lk),
          "binomial"         = if (is.null(lk)) stats::binomial()         else stats::binomial(link = lk),
          "poisson"          = if (is.null(lk)) stats::poisson()          else stats::poisson(link = lk),
          "Gamma"            = if (is.null(lk)) stats::Gamma(link = "log")else stats::Gamma(link = lk),
          "inverse.gaussian" = if (is.null(lk)) stats::inverse.gaussian() else stats::inverse.gaussian(link = lk),
          stats::gaussian())
      }
    }
    
    # --- Validation prealable : la famille est-elle compatible avec les donnees ? ---
    # Evite des messages d'erreur bruts de R (ex. "y values must be 0 <= y <= 1").
    fam_check <- function(var) {
      y <- suppressWarnings(as.numeric(df[[var]]))
      y <- y[!is.na(y)]
      if (length(y) == 0) return(trf("« %s » : aucune valeur numérique exploitable.", var))
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
          msg <- trf("La famille « Bêta » exige une réponse strictement comprise entre 0 et 1 (exclus), mais « %s » varie de %.3g à %.3g. Pour des proportions incluant 0 ou 1, utilisez la binomiale ; pour du continu positif, la Gamma.",
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
      shiny::showNotification(bad_msgs[[1]], type = "error", duration = 12)
      return()
    }
    
    results_list <- list()
    model_list   <- list()
    is_gaussian  <- (fam == "gaussian")
    
    shiny::withProgress(message = "Ajustement du modèle mixte...", value = 0.2, {
      tryCatch({
        fam_obj <- build_family(engine, fam, link)
        n_resp  <- length(input$responseVar)
        
        for (var in input$responseVar) {
          fixed <- paste(sapply(input$factorVar, function(x) paste0("`", x, "`")),
                         collapse = if (isTRUE(input$interaction)) " * " else " + ")
          formula_str <- paste0("`", var, "` ~ ", fixed, " + ", rand_term)
          form <- stats::as.formula(formula_str)
          
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
            shiny::showNotification(trf("GLMM (%s) : aucun coefficient d'effet fixe.", var), type = "warning")
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
                trf("Effet aléatoire : %s", rand_term)
              },
              stringsAsFactors = FALSE
            )
          }
          shiny::incProgress(0.6 / max(n_resp, 1))
        }
        
        if (length(results_list) > 0) {
          values$testResultsDF   <- do.call(rbind, results_list)
          values$currentModel    <- model_list[[length(model_list)]]
          values$modelList       <- model_list
          values$currentModelVar <- 1
          values$currentTestType <- "parametric"
          shiny::showNotification(
            trf("Modèle mixte ajusté (%s, famille %s) sur %d variable(s).",
                    engine, fam, length(model_list)),
            type = "message", duration = 5
          )
        } else {
          shiny::showNotification("Aucun résultat GLMM généré.", type = "warning")
        }
      }, error = function(e) {
        shiny::showNotification(hstat_err_fr(e, "Erreur modèle mixte"), type = "error", duration = 8)
      })
    })
  })
  
  
  #  ANOVA A MESURES REPETEES (parametrique) -- moteurs lmer (mixte) ou afex
  #  + post-hoc (emmeans, comparaisons par paires sur le facteur intra/inter)
  shiny::observeEvent(input$testRMAnova, {
    shiny::req(input$responseVar)
    df <- values$filteredData %||% values$cleanData %||% values$data
    if (is.null(df)) { shiny::showNotification("Aucune donnée disponible.", type = "error"); return() }
    
    subj   <- input$rmSubject
    within <- input$rmWithin
    between <- input$rmBetween
    engine <- input$rmEngine %||% "mixed"
    adj    <- input$rmPostHocAdjust %||% "holm"
    
    if (is.null(subj) || !nzchar(subj) || !subj %in% names(df)) {
      shiny::showNotification("Sélectionnez une variable « Sujet / identifiant » (panneau « Mesures répétées »).",
                       type = "warning", duration = 6); return()
    }
    if (is.null(within) || length(within) == 0) {
      shiny::showNotification("Sélectionnez au moins un facteur « Période » (intra-sujet).", type = "warning", duration = 6); return()
    }
    all_fac <- c(within, between)
    if (!all(all_fac %in% names(df))) {
      shiny::showNotification("Facteur(s) « Période » / « Traitement » introuvable(s) dans les données.", type = "error"); return()
    }
    if (engine == "mixed" && !requireNamespace("lmerTest", quietly = TRUE) &&
        !requireNamespace("lme4", quietly = TRUE)) {
      shiny::showNotification("Le package 'lme4'/'lmerTest' est requis pour le moteur mixte.", type = "error"); return()
    }
    if (engine == "afex" && !requireNamespace("afex", quietly = TRUE)) {
      shiny::showNotification("Le package 'afex' n'est pas installé (moteur afex).", type = "error"); return()
    }
    has_emm <- requireNamespace("emmeans", quietly = TRUE)
    
    results_list <- list()
    model_list   <- list()
    posthoc_list <- list()
    bt <- function(x) paste0("`", x, "`")
    
    shiny::withProgress(message = "ANOVA à mesures répétées...", value = 0.2, {
      tryCatch({
        # Facteurs en facteurs ; sujet en facteur.
        df[[subj]] <- factor(df[[subj]])
        # Une date devient un facteur ORDONNE PAR LE TEMPS : `factor()` classe
        # ses niveaux sur la valeur numerique sous-jacente. Passer par
        # `as.character()` d'abord les classerait alphabetiquement, ce qui est
        # juste en ISO et faux des qu'un fichier porte « 03/04/2026 ».
        for (f in all_fac) if (!is.factor(df[[f]])) df[[f]] <- factor(df[[f]])

        # Un facteur intra-sujet a autant de niveaux que d'observations n'est
        # pas repete : le modele ne peut pas separer le sujet de la periode. On
        # le dit, plutot que de laisser lmer echouer sur un message obscur.
        for (f in within) {
          n_niv <- nlevels(droplevels(factor(df[[f]])))
          if (n_niv < 2) {
            shiny::showNotification(trf(
              "« %s » ne compte qu'une seule modalité : ce n'est pas un facteur répété.", f),
              type = "warning", duration = 8)
          } else if (n_niv >= nrow(df)) {
            shiny::showNotification(trf(
              "« %s » compte autant de modalités que d'observations (%d) : aucune mesure n'est répétée. Regroupez les dates (par mois, par stade) avant l'analyse.",
              f, n_niv), type = "warning", duration = 10)
          }
        }
        
        for (var in input$responseVar) {
          if (!is.numeric(df[[var]])) df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
          dsub <- df[, c(var, subj, all_fac), drop = FALSE]
          dsub <- dsub[stats::complete.cases(dsub), ]
          if (nrow(dsub) < 3) { shiny::showNotification(trf("rmANOVA (%s) : trop peu de données.", var), type = "warning"); next }
          
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
              pcol <- if ("Pr(>F)" %in% colnames(atab)) "Pr(>F)" else utils::tail(colnames(atab), 1)
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
            form <- stats::as.formula(paste0(bt(var), " ~ ", fixed, " + (1 | ", bt(subj), ")"))
            mod <- if (requireNamespace("lmerTest", quietly = TRUE)) lmerTest::lmer(form, data = dsub)
                   else lme4::lmer(form, data = dsub)
            # Table d'ANOVA de type III (Satterthwaite) si lmerTest.
            atab <- tryCatch(as.data.frame(stats::anova(mod)), error = function(e) NULL)
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
          shiny::incProgress(0.4 / max(length(input$responseVar), 1))
          
          # --- Post-hoc : comparaisons par paires (emmeans) sur chaque facteur ---
          if (has_emm) {
            for (f in all_fac) {
              ph <- tryCatch({
                emm <- emmeans::emmeans(emm_model, specs = f)
                pr  <- emmeans::contrast(emm, method = "pairwise", adjust = adj)
                as.data.frame(pr)
              }, error = function(e) NULL)
              if (!is.null(ph) && nrow(ph) > 0) {
                pcol <- if ("p.value" %in% colnames(ph)) "p.value" else utils::tail(colnames(ph), 1)
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
          shiny::showNotification(trf("ANOVA à mesures répétées (%s) sur %d variable(s).", engine, length(model_list)),
                           type = "message", duration = 5)
        } else {
          shiny::showNotification("Aucun résultat rmANOVA généré.", type = "warning")
        }
      }, error = function(e) {
        shiny::showNotification(hstat_err_fr(e, "Erreur rmANOVA"), type = "error", duration = 9)
      })
    })
  })
  
  #  NON PARAMETRIQUE A MESURES REPETEES : Friedman / Durbin / ART (+ post-hoc)
  shiny::observeEvent(input$testRMNonParam, {
    shiny::req(input$responseVar)
    df <- values$filteredData %||% values$cleanData %||% values$data
    if (is.null(df)) { shiny::showNotification("Aucune donnée disponible.", type = "error"); return() }
    
    subj   <- input$rmSubject
    within <- input$rmWithin
    between <- input$rmBetween
    method <- input$rmNonParam %||% "friedman"
    adj    <- input$rmPostHocAdjust %||% "holm"
    if (adj == "tukey") adj <- "holm"  # Tukey non applicable ici
    
    if (is.null(subj) || !nzchar(subj) || !subj %in% names(df)) {
      shiny::showNotification("Sélectionnez une variable « Sujet / identifiant ».", type = "warning", duration = 6); return()
    }
    if (is.null(within) || length(within) == 0) {
      shiny::showNotification("Sélectionnez au moins un facteur « Période » (intra-sujet).", type = "warning", duration = 6); return()
    }
    if (method %in% c("friedman", "durbin") && !requireNamespace("PMCMRplus", quietly = TRUE)) {
      shiny::showNotification("Le package 'PMCMRplus' est requis pour les post-hoc de Friedman/Durbin.", type = "warning")
    }
    if (method == "art" && !requireNamespace("ARTool", quietly = TRUE)) {
      shiny::showNotification("Le package 'ARTool' n'est pas installé (méthode ART).", type = "error"); return()
    }
    
    results_list <- list()
    posthoc_list <- list()
    bt <- function(x) paste0("`", x, "`")
    
    shiny::withProgress(message = "Test non paramétrique répété...", value = 0.3, {
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
            form  <- stats::as.formula(paste0(bt(var), " ~ ", fixed, " + (1 | ", bt(subj), ")"))
            m <- ARTool::art(form, data = dsub)
            atab <- as.data.frame(stats::anova(m))
            for (i in seq_len(nrow(atab))) {
              eff  <- if ("Term" %in% colnames(atab)) atab[["Term"]][i] else rownames(atab)[i]
              pcol <- if ("Pr(>F)" %in% colnames(atab)) "Pr(>F)" else utils::tail(colnames(atab), 1)
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
              pcol <- if ("p.value" %in% colnames(ph)) "p.value" else utils::tail(colnames(ph), 1)
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
          shiny::incProgress(0.6 / max(length(input$responseVar), 1))
        }
        
        if (length(results_list) > 0) {
          values$testResultsDF   <- do.call(rbind, results_list)
          values$currentTestType <- "nonparametric"
          values$rmPostHocData <- if (length(posthoc_list) > 0) do.call(rbind, posthoc_list) else NULL
          values$rmPostHocMethod <- paste0(switch(method, friedman = "Friedman/Conover",
                                                  durbin = "Durbin", art = "ART"), ", ajustement ", adj)
          shiny::showNotification(trf("Test non paramétrique répété (%s) terminé.", method),
                           type = "message", duration = 5)
        } else {
          shiny::showNotification("Aucun résultat généré.", type = "warning")
        }
      }, error = function(e) {
        shiny::showNotification(hstat_err_fr(e, "Erreur non paramétrique répété"), type = "error", duration = 9)
      })
    })
  })
  

  shiny::observeEvent(input$testMANOVA, {
    shiny::req(input$responseVar, input$factorVar)
    
    if (length(input$responseVar) < 2) {
      shiny::showNotification("MANOVA nécessite au moins 2 variables réponses numériques.",
                       type = "warning", duration = 6)
      return()
    }
    
    tryCatch({
      chk <- check_manova_data(values$filteredData, input$responseVar, input$factorVar)
      if (!isTRUE(chk$ok)) {
        shiny::showNotification(chk$message, type = "error", duration = 8)
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
      
      shiny::showNotification(
        trf("MANOVA terminée : %s effet(s) testé(s).", nrow(stats_df)),
        type = "message", duration = 4
      )
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur MANOVA"), type = "error", duration = 10)
    })
  })
  
  shiny::observeEvent(input$testPERMANOVA, {
    shiny::req(input$responseVar, input$factorVar)
    
    if (length(input$responseVar) < 2) {
      shiny::showNotification("PERMANOVA nécessite au moins 2 variables réponses numériques.",
                       type = "warning", duration = 6)
      return()
    }
    
    shiny::showNotification("PERMANOVA en cours (999 permutations)...",
                     type = "message", duration = NULL, id = "permanovaProgress")
    
    tryCatch({
      chk <- check_manova_data(values$filteredData, input$responseVar, input$factorVar)
      if (!isTRUE(chk$ok)) {
        shiny::removeNotification("permanovaProgress")
        shiny::showNotification(chk$message, type = "error", duration = 8)
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
      
      shiny::removeNotification("permanovaProgress")
      shiny::showNotification(
        trf("PERMANOVA terminée : %s effet(s) (%s permutations, distance %s).", nrow(out), nperm, dist_method),
        type = "message", duration = 4
      )
    }, error = function(e) {
      shiny::removeNotification("permanovaProgress")
      shiny::showNotification(hstat_err_fr(e, "Erreur PERMANOVA"), type = "error", duration = 10)
    })
  })
  
  # Les DEUX boutons de diagnostic declenchent le meme travail.
  shiny::observeEvent(list(input$runManovaDiagnostic, input$runManovaDiagnostic2), {
    if (((input$runManovaDiagnostic %||% 0) +
         (input$runManovaDiagnostic2 %||% 0)) == 0) return()
    shiny::req(input$responseVar, input$factorVar)
    if (length(input$responseVar) < 2) {
      shiny::showNotification("Le diagnostic nécessite au moins 2 variables réponses.",
                       type = "warning", duration = 6)
      return()
    }
    tryCatch({
      chk <- check_manova_data(values$filteredData, input$responseVar, input$factorVar)
      if (!isTRUE(chk$ok)) {
        shiny::showNotification(chk$message, type = "error", duration = 8)
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
      
      shiny::showNotification(
        trf("Diagnostic terminé. Test recommandé : %s (confiance : %s).", rec$test_recommande, rec$niveau_confiance),
        type = "message", duration = 6
      )
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur diagnostic"), type = "error", duration = 10)
    })
  })
  
  shiny::observeEvent(input$runManovaSimpleEffects, {
    shiny::req(input$responseVar, input$factorVar, input$manovaSimpleFixed, input$manovaSimpleTested)
    if (length(input$factorVar) < 2) {
      shiny::showNotification("Les effets simples nécessitent au moins 2 facteurs.",
                       type = "warning"); return()
    }
    if (input$manovaSimpleFixed == input$manovaSimpleTested) {
      shiny::showNotification("Le facteur fixé et le facteur testé doivent être différents.",
                       type = "warning"); return()
    }
    tryCatch({
      chk <- check_manova_data(values$filteredData, input$responseVar,
                               c(input$manovaSimpleFixed, input$manovaSimpleTested))
      if (!isTRUE(chk$ok)) {
        shiny::showNotification(chk$message, type = "error"); return()
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
        shiny::showNotification(paste0("Effets simples non calculables : sous-groupes trop petits ",
                                "ou variables réponses problématiques."),
                         type = "warning", duration = 7); return()
      }
      
      is_param_result <- want_param && !used_fallback
      res$Type_test <- if (is_param_result) "MANOVA conditionnelle"
      else "PERMANOVA conditionnelle"
      values$manovaSimpleEffects <- res
      
      if (used_fallback) {
        shiny::showNotification(trf("La MANOVA conditionnelle n'est pas calculable (variables réponses colinéaires) : bascule automatique sur la PERMANOVA conditionnelle. %s niveau(x) testé(s).", nrow(res)),
                         type = "warning", duration = 8)
      } else {
        shiny::showNotification(trf("Effets simples calculés : %s niveau(x) testé(s).", nrow(res)),
                         type = "message", duration = 4)
      }
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur effets simples"), type = "error")
    })
  })
  
  
  output$showManovaDiagnostics <- shiny::reactive({
    ctt <- values$currentTestType
    isTRUE(!is.null(ctt) && ctt %in% c("manova", "permanova"))
  })
  shiny::outputOptions(output, "showManovaDiagnostics", suspendWhenHidden = FALSE)
  
  output$hasManovaParam <- shiny::reactive({
    !is.null(values$manovaParamResults) && nrow(values$manovaParamResults) > 0
  })
  shiny::outputOptions(output, "hasManovaParam", suspendWhenHidden = FALSE)
  
  output$hasManovaPermanova <- shiny::reactive({
    !is.null(values$manovaPermanovaResults) && nrow(values$manovaPermanovaResults) > 0
  })
  shiny::outputOptions(output, "hasManovaPermanova", suspendWhenHidden = FALSE)
  
  output$hasManovaDispersion <- shiny::reactive({
    !is.null(values$manovaPermDisp) && nrow(values$manovaPermDisp) > 0
  })
  shiny::outputOptions(output, "hasManovaDispersion", suspendWhenHidden = FALSE)
  
  # Détail MANOVA paramétrique : 4 statistiques
  output$manovaParamTable <- DT::renderDT({
    shiny::req(values$manovaParamResults)
    df <- values$manovaParamResults
    for (col in c("p_Pillai", "p_Wilks", "p_Hotelling", "p_Roy")) {
      if (col %in% names(df)) df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    dt <- DT::datatable(df, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    
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
          dt <- dt %>% DT::formatStyle(cols_to_color,
                                   backgroundColor = "#c8e6c9",
                                   fontWeight = "bold")
      }
    }
    dt
  })
  
  output$manovaPermanovaTable <- DT::renderDT({
    shiny::req(values$manovaPermanovaResults)
    df <- values$manovaPermanovaResults
    if ("p_value" %in% names(df))
      df$p_value <- sapply(df$p_value, function(p) if (is.na(p)) NA else fmt_p(p))
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    DT::datatable(df, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$manovaMardiaTable <- DT::renderDT({
    shiny::req(values$manovaMardia)
    df <- values$manovaMardia
    for (col in c("p_Skewness", "p_Kurtosis")) {
      if (col %in% names(df)) df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    DT::datatable(df, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  
  output$manovaMardiaInterpretation <- shiny::renderUI({
    shiny::req(values$manovaMardia)
    conc <- values$manovaMardia$Conclusion[1]
    color <- if (grepl("plausible", conc)) "#2e7d32" else "#c62828"
    bg    <- if (grepl("plausible", conc)) "#e8f5e9" else "#ffebee"
    shiny::div(style = paste0("padding:8px 12px; background:", bg,
                       "; border-left:4px solid ", color,
                       "; border-radius:4px; margin-top:6px; font-size:13px;"),
        shiny::icon("info-circle", style = paste0("color:", color, ";")),
        shiny::tags$b(" Conclusion Mardia : "), conc)
  })
  
  output$manovaBoxMTable <- DT::renderDT({
    shiny::req(values$manovaBoxM)
    df <- values$manovaBoxM
    if ("p_value" %in% names(df))
      df$p_value <- sapply(df$p_value, function(p) if (is.na(p)) NA else fmt_p(p))
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    DT::datatable(df, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  
  output$manovaBoxMInterpretation <- shiny::renderUI({
    shiny::req(values$manovaBoxM)
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
    shiny::div(style = paste0("padding:8px 12px; background:", bg,
                       "; border-left:4px solid ", color,
                       "; border-radius:4px; margin-top:6px; font-size:13px;"),
        shiny::icon("info-circle", style = paste0("color:", color, ";")),
        shiny::tags$b(" Conclusion Box's M : "), msg)
  })
  
  output$manovaPermDispTable <- DT::renderDT({
    shiny::req(values$manovaPermDisp)
    df <- values$manovaPermDisp
    if ("p_value" %in% names(df))
      df$p_value <- sapply(df$p_value, function(p) if (is.na(p)) NA else fmt_p(p))
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    DT::datatable(df, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  
  output$manovaPermDispInterpretation <- shiny::renderUI({
    shiny::req(values$manovaPermDisp)
    conclusions <- values$manovaPermDisp$Conclusion
    any_violation <- any(grepl("hétérogènes", conclusions))
    color <- if (!any_violation) "#2e7d32" else "#c62828"
    bg    <- if (!any_violation) "#e8f5e9" else "#fff3e0"
    msg <- if (!any_violation)
      "Dispersions multivariées homogènes -- analyse multivariée fiable."
    else
      "Dispersions hétérogènes -- la PERMANOVA peut confondre différences de localisation et de dispersion."
    shiny::div(style = paste0("padding:8px 12px; background:", bg,
                       "; border-left:4px solid ", color,
                       "; border-radius:4px; margin-top:6px; font-size:13px;"),
        shiny::icon("info-circle", style = paste0("color:", color, ";")),
        shiny::tags$b(" Conclusion PERMDISP : "), msg)
  })
  
  # Un classeur = une liste nommee de tableaux. La boucle sur les feuilles, le
  # nom de feuille valide et le fichier toujours ecrit viennent de l'ecrivain
  # commun ; le module ne dit plus que CE QU'IL exporte.
  output$downloadManovaParam <- hstat_classeur_handler(function() {
    hstat_tables_non_vides(list(
      "MANOVA_stats" = values$manovaParamResults,
      "Mardia"       = values$manovaMardia,
      "BoxM"         = values$manovaBoxM,
      "PERMDISP"     = values$manovaPermDisp))
  }, "MANOVA_parametrique", "MANOVA paramétrique")
  
  output$downloadManovaPermanova <- hstat_classeur_handler(function() {
    hstat_tables_non_vides(list(
      "PERMANOVA" = values$manovaPermanovaResults,
      "PERMDISP"  = values$manovaPermDisp))
  }, "PERMANOVA", "PERMANOVA")
  
  
  output$showManovaWorkflow <- shiny::reactive({
    ctt <- values$currentTestType
    test_done <- isTRUE(!is.null(ctt) && ctt %in% c("manova", "permanova", "manova_diagnostic"))
    enough_vars <- length(input$responseVar) >= 2
    test_done || enough_vars
  })
  shiny::outputOptions(output, "showManovaWorkflow", suspendWhenHidden = FALSE)
  
  shiny::observeEvent(input$responseVar, {
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
  
  output$hasManovaRecommendation <- shiny::reactive({
    !is.null(values$manovaRecommendation)
  })
  shiny::outputOptions(output, "hasManovaRecommendation", suspendWhenHidden = FALSE)
  
  output$hasManovaInteraction <- shiny::reactive({
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
  shiny::outputOptions(output, "hasManovaInteraction", suspendWhenHidden = FALSE)
  
  output$hasManovaSimpleEffects <- shiny::reactive({
    !is.null(values$manovaSimpleEffects) && nrow(values$manovaSimpleEffects) > 0
  })
  shiny::outputOptions(output, "hasManovaSimpleEffects", suspendWhenHidden = FALSE)
  
  output$hasManovaOutliers <- shiny::reactive({
    !is.null(values$manovaOutliers)
  })
  shiny::outputOptions(output, "hasManovaOutliers", suspendWhenHidden = FALSE)
  
  # Frise visuelle des etapes du workflow
  # Carte de recommandation du test
  output$manovaRecommendationCard <- shiny::renderUI({
    shiny::req(values$manovaRecommendation)
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
    buttons <- shiny::div(
      style = "margin-top:12px; background:#eef7fb; border-left:4px solid #1b9fd0; padding:10px 14px; border-radius:0 6px 6px 0;",
      shiny::div(style = "font-size:13px; color:#2c3e50;",
        shiny::icon("hand-point-right"),
        " Pour exécuter le test recommandé, cliquez sur le bouton ",
        shiny::tags$b(reco_name),
        " dans la colonne « ", shiny::tags$b(reco_loc), " » des ",
        shiny::tags$b("Paramètres des tests"), " (en haut).")
    )
    
    shiny::div(style = paste0("background:", bg_color, "; border-left:6px solid ", border_color,
                       "; padding:18px 22px; border-radius:8px;"),
        shiny::div(style = "display:flex; align-items:center; gap:14px; margin-bottom:10px;",
            shiny::icon("magic", style = paste0("font-size:32px; color:", border_color, ";")),
            shiny::div(
              shiny::div(style = "font-size:11px; color:#555; letter-spacing:1px;", "RECOMMANDATION AUTOMATIQUE"),
              shiny::div(style = paste0("font-size:22px; font-weight:bold; color:", border_color, ";"),
                  rec$test_recommande),
              shiny::div(style = "font-size:13px; color:#555; margin-top:2px;",
                  "Statistique : ", shiny::strong(rec$statistique_recommandee),
                  " | Confiance : ", shiny::strong(rec$niveau_confiance))
            )
        ),
        shiny::div(style = "background:white; padding:12px 16px; border-radius:6px; margin-top:10px;",
            shiny::h5(shiny::icon("clipboard-list"), " Justification", style = "margin-top:0; color:#333;"),
            shiny::tags$ul(style = "margin-bottom:0; padding-left:18px;",
                    lapply(rec$justifications, function(j) shiny::tags$li(style = "margin:4px 0;", j)))
        ),
        if (length(rec$alertes) > 0) {
          shiny::div(style = "background:#fff3e0; border-left:4px solid #ff9800; padding:10px 14px; margin-top:10px; border-radius:4px;",
              shiny::icon("exclamation-triangle", style = "color:#e65100;"),
              shiny::tags$strong(" Points de vigilance :"),
              shiny::tags$ul(style = "margin:6px 0 0 18px;",
                      lapply(rec$alertes, function(a) shiny::tags$li(style = "color:#bf360c;", a))))
        } else NULL,
        buttons
    )
  })
  
  output$manovaOutliersCard <- shiny::renderUI({
    shiny::req(values$manovaOutliers)
    out <- values$manovaOutliers
    n_out <- out$n_outliers
    has_issue <- !is.null(n_out) && !is.na(n_out) && n_out > 0
    
    color <- if (has_issue) "#fb8c00" else "#43a047"
    bg    <- if (has_issue) "#fff3e0" else "#e8f5e9"
    iconame <- if (has_issue) "exclamation-circle" else "check-circle"
    
    shiny::div(style = paste0("background:", bg, "; border-left:4px solid ", color,
                       "; padding:12px 16px; border-radius:6px; margin-top:10px;"),
        shiny::icon(iconame, style = paste0("color:", color, ";")),
        shiny::strong(" Détection des valeurs aberrantes multivariées : "),
        out$conclusion,
        if (has_issue && length(out$idx) > 0 && length(out$idx) <= 10) {
          shiny::div(style = "font-size:11px; color:#666; margin-top:4px;",
              "Index des lignes : ", paste(out$idx, collapse = ", "))
        } else NULL
    )
  })
  
  output$manovaInterpretationGuidance <- shiny::renderUI({
    param_res <- values$manovaParamResults
    perm_res  <- values$manovaPermanovaResults
    
    # Cas 1 : aucun test lancé, seulement le diagnostic
    if (is.null(param_res) && is.null(perm_res)) {
      return(shiny::div(style = "background:#e3f2fd; border-left:4px solid #1565C0; padding:14px 18px; border-radius:8px;",
                 shiny::icon("info-circle", style = "color:#1565C0;"),
                 shiny::strong(" En attente d'un test multivarié. "),
                 "Lancez une MANOVA ou une PERMANOVA depuis l'onglet ",
                 shiny::strong("'1. Diagnostic & recommandation'"),
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
        shiny::tags$li(style = "color:#2e7d32;",
                shiny::icon("check-circle"), " ",
                shiny::strong(paste0(length(sig_idx), " effet(s) significatif(s)")),
                " détecté(s) : ", paste(effets[sig_idx], collapse = ", "))
      ))
    }
    if (length(insig_idx) > 0) {
      msg_lines <- c(msg_lines, list(
        shiny::tags$li(style = "color:#757575;",
                shiny::icon("minus-circle"), " ",
                paste0(length(insig_idx), " effet(s) non significatif(s)"),
                " : ", paste(effets[insig_idx], collapse = ", "))
      ))
    }
    
    action_box <- if (has_interaction) {
      shiny::div(style = "background:#fff3e0; border-left:4px solid #fb8c00; padding:10px 14px; margin-top:10px; border-radius:4px;",
          shiny::icon("lightbulb", style = "color:#e65100;"),
          shiny::strong(" Action recommandée : "),
          "Une interaction est significative. Calculez les ",
          shiny::strong("effets simples"),
          " ci-dessous pour comprendre quel facteur agit dans quel contexte.")
    } else if (length(sig_idx) > 0) {
      shiny::div(style = "background:#e3f2fd; border-left:4px solid #1565C0; padding:10px 14px; margin-top:10px; border-radius:4px;",
          shiny::icon("lightbulb", style = "color:#0d47a1;"),
          shiny::strong(" Action recommandée : "),
          "Allez à la section ", shiny::strong("'PostHoc MANOVA/PERMANOVA'"),
          " (onglet Comparaisons multiples) pour identifier quels niveaux diffèrent (lettres de groupes).")
    } else {
      shiny::div(style = "background:#f5f5f5; border-left:4px solid #9e9e9e; padding:10px 14px; margin-top:10px; border-radius:4px;",
          shiny::icon("info-circle"),
          shiny::strong(" Aucun effet significatif. "),
          "Vérifiez la taille d'effet, l'effectif par groupe, et la pertinence des facteurs.")
    }
    
    shiny::div(style = "background:white; border:1px solid #cfd8dc; padding:14px 18px; border-radius:8px;",
        shiny::h5(shiny::icon("brain"), " Ce que vos résultats signifient",
           style = "margin-top:0; color:#1565C0;"),
        shiny::div(style = "font-size:12px; color:#777; margin-bottom:8px;",
            "Test analysé : ", shiny::strong(test_lbl)),
        shiny::tags$ul(style = "margin-bottom:0; padding-left:18px;", msg_lines),
        action_box
    )
  })
  
  # Selecteurs pour effets simples (un facteur "fixe", un facteur "teste")
  output$manovaSimpleEffectsSelectors <- shiny::renderUI({
    shiny::req(input$factorVar)
    if (length(input$factorVar) < 2) {
      return(shiny::div(style = "color:#888; padding:10px;",
                 shiny::icon("info-circle"), " Au moins 2 facteurs requis pour les effets simples."))
    }
    shiny::fluidRow(
      shiny::column(5, shiny::selectInput(ns("manovaSimpleFixed"),
                            shiny::tagList(shiny::icon("anchor"), " Facteur à fixer :"),
                            choices = input$factorVar, selected = input$factorVar[1])),
      shiny::column(5, shiny::selectInput(ns("manovaSimpleTested"),
                            shiny::tagList(shiny::icon("crosshairs"), " Facteur à tester :"),
                            choices = input$factorVar, selected = input$factorVar[2])),
      shiny::column(2, shiny::div(style = "padding-top:25px;",
                    shiny::actionButton(ns("runManovaSimpleEffects"),
                                 shiny::tagList(shiny::icon("play"), " Calculer"),
                                 class = "btn-info btn-block")))
    )
  })
  
  output$manovaSimpleEffectsTable <- DT::renderDT({
    shiny::req(values$manovaSimpleEffects)
    df <- values$manovaSimpleEffects
    for (col in c("p_value", "p_adj")) {
      if (col %in% names(df)) df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$testsRoundResults, input$testsDecimals)
    dt <- DT::datatable(df, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    if ("Significatif" %in% names(df)) {
      dt <- dt %>% DT::formatStyle("Significatif",
                               backgroundColor = DT::styleEqual(c("Oui", "Non"),
                                                            c("#e8f5e9", "#f5f5f5")),
                               fontWeight = "bold")
    }
    dt
  })
  
  output$testResultsDF <- DT::renderDT({
    shiny::req(values$testResultsDF)
    
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
    
    dt <- DT::datatable(df,
                    options  = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    
    if (any(!is.na(df$Transformation))) {
      dt <- dt %>%
        DT::formatStyle(
          "Transformation",
          target          = "row",
          backgroundColor = DT::styleEqual(
            levels = unique(stats::na.omit(df$Transformation)),
            values = rep("#fff8e1", length(unique(stats::na.omit(df$Transformation))))
          )
        )
    }
    dt
  })
  
  
  shiny::observeEvent(input$testChiSq, {
    shiny::req(input$chiSqCatVar, input$chiSqFreqVar, values$filteredData)
    df      <- values$filteredData
    cat_var <- input$chiSqCatVar
    frq_var <- input$chiSqFreqVar
    dtype   <- input$chiSqDataType %||% "freq"
    cats    <- as.character(df[[cat_var]])
    vals    <- suppressWarnings(as.numeric(df[[frq_var]]))
    valid   <- !is.na(cats) & !is.na(vals)
    cats    <- cats[valid]; vals <- vals[valid]
    if (length(vals) < 2) { shiny::showNotification("Pas assez de données.", type="error"); return() }
    if (dtype == "pct") { pcts <- vals; obs <- round(vals / sum(vals) * 1000) } else {
      obs <- round(vals); pcts <- obs / sum(obs) * 100 }
    if (!is.null(input$chiSqUniform) && !input$chiSqUniform) {
      p_list <- lapply(seq_along(cats), function(i) input[[paste0("chiSqP_",i)]] %||% (1/length(cats)))
      p_exp  <- unlist(p_list)
      if (abs(sum(p_exp)-1) > 0.01) p_exp <- p_exp / sum(p_exp)
    } else { p_exp <- rep(1/length(obs), length(obs)) }
    tryCatch({
      tr <- stats::chisq.test(obs, p = p_exp, rescale.p = TRUE)
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
      shiny::showNotification(paste0("Chi²=",round(tr$statistic,3)," p=",formatC(tr$p.value,"g",digits=4)),
                       type="message", duration=4)
    }, error = function(e) shiny::showNotification(hstat_err_fr(e, "Erreur Chi²"), type="error"))
  })
  
  shiny::observeEvent(input$testMultinomial, {
    shiny::req(input$chiSqCatVar, input$chiSqFreqVar, values$filteredData)
    df      <- values$filteredData
    cat_var <- input$chiSqCatVar; frq_var <- input$chiSqFreqVar
    dtype   <- input$chiSqDataType %||% "freq"
    cats    <- as.character(df[[cat_var]])
    vals    <- suppressWarnings(as.numeric(df[[frq_var]]))
    valid   <- !is.na(cats) & !is.na(vals)
    cats    <- cats[valid]; vals <- vals[valid]
    if (length(vals) < 2) { shiny::showNotification("Pas assez de données.", type="error"); return() }
    if (dtype == "pct") { pcts <- vals; obs <- round(vals/sum(vals)*1000) } else {
      obs <- round(vals); pcts <- obs/sum(obs)*100 }
    tryCatch({
      tr <- stats::chisq.test(obs, p = rep(1/length(obs),length(obs)), simulate.p.value = TRUE, B = 10000)
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
      shiny::showNotification(paste0("Multinomial p=",formatC(tr$p.value,"g",digits=4)),
                       type="message",duration=4)
    }, error = function(e) shiny::showNotification(hstat_err_fr(e, "Erreur Multinomial"),type="error"))
  })
  
  shiny::observeEvent(input$runChiSqPostHoc, {
    shiny::req(values$chiSqFreqData)
    chi_data   <- values$chiSqFreqData
    obs        <- chi_data$Observes
    cats       <- chi_data$Categorie
    n          <- length(obs)
    adj_method <- input$chiSqPostHocAdj %||% "bonferroni"
    if (n < 2) { shiny::showNotification("Trop peu de catégories.", type="warning"); return() }
    N_total <- sum(obs); p_raw <- c(); chi2_v <- c(); comps <- c()
    for (i in 1:(n-1)) for (j in (i+1):n) {
      m <- matrix(c(obs[i], obs[j], N_total-obs[i], N_total-obs[j]), 2)
      tryCatch({ t2 <- stats::chisq.test(m, correct=(n==2))
      p_raw <<- c(p_raw, t2$p.value); chi2_v <<- c(chi2_v, round(t2$statistic,4))
      }, error = function(e) { p_raw <<- c(p_raw,NA); chi2_v <<- c(chi2_v,NA) })
      comps <- c(comps, paste0(cats[i]," — ",cats[j]))
    }
    p_adj <- stats::p.adjust(p_raw, method = adj_method)
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
    shiny::showNotification(trf("Post-hoc chi² terminé (%s)", adj_method),type="message",duration=3)
  })
  
  # NB : les handlers downloadChiSqExcel / downloadChiSqCSV / downloadChiSqPlot
  # sont definis plus bas (versions completes avec mise en forme). Une premiere
  # version existait ici : les output$ Shiny etant uniques, elle etait ecrasee
  # silencieusement -- code mort supprime.

  output$showValidation <- shiny::reactive({
    !is.null(values$normalityResults) || !is.null(values$homogeneityResults)
  })
  shiny::outputOptions(output, "showValidation", suspendWhenHidden = FALSE)
  
  output$showChiSqResults <- shiny::reactive({
    !is.null(values$chiSqFreqData)
  })
  shiny::outputOptions(output, "showChiSqResults", suspendWhenHidden = FALSE)
  
  # Graphique chi2 pour l'onglet PostHoc (même logique, inputs distincts)
  # runChiSqPostHoc2 : lien depuis l'onglet PostHoc (chiSqDataType2 + chiSqPostHocAdj2)
  shiny::observeEvent(input$runChiSqPostHoc2, {
    shiny::req(input$chiSqCatVar, input$chiSqFreqVar, values$filteredData)
    df      <- values$filteredData
    cat_var <- input$chiSqCatVar; frq_var <- input$chiSqFreqVar
    dtype   <- input$chiSqDataType2 %||% input$chiSqDataType %||% "freq"
    cats    <- as.character(df[[cat_var]])
    vals    <- suppressWarnings(as.numeric(df[[frq_var]]))
    valid   <- !is.na(cats) & !is.na(vals)
    cats    <- cats[valid]; vals <- vals[valid]
    if (length(vals) < 2) { shiny::showNotification("Pas assez de données.", type="error"); return() }
    if (dtype == "pct") { pcts <- vals; obs <- round(vals/sum(vals)*1000) } else {
      obs <- round(vals); pcts <- obs/sum(obs)*100 }
    tryCatch({
      p_exp <- rep(1/length(obs), length(obs))
      tr <- stats::chisq.test(obs, p = p_exp, rescale.p = TRUE)
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
        tryCatch({ t2 <- stats::chisq.test(m, correct=(n==2))
        p_raw <<- c(p_raw,t2$p.value); chi2_v <<- c(chi2_v,round(t2$statistic,4))
        }, error=function(e){ p_raw <<- c(p_raw,NA); chi2_v <<- c(chi2_v,NA) })
        comps <- c(comps, paste0(cats[i]," — ",cats[j]))
      }
      p_adj <- stats::p.adjust(p_raw, method=adj_method)
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
      shiny::showNotification(
        trf("Chi² + Post-hoc terminés (%s)", adj_method),
        type="message", duration=4)
    }, error=function(e) shiny::showNotification(hstat_err_fr(e, "Erreur"),type="error"))
  })
  
  output$showParametricDiagnostics <- shiny::reactive({
    !is.null(values$currentTestType) && values$currentTestType == "parametric" && !is.null(values$modelList)
  })
  shiny::outputOptions(output, "showParametricDiagnostics", suspendWhenHidden = FALSE)
  
  output$showValidationNavigation <- shiny::reactive({
    length(input$responseVar) > 1 && !is.null(values$normalityResults)
  })
  shiny::outputOptions(output, "showValidationNavigation", suspendWhenHidden = FALSE)
  
  output$validationNavigation <- shiny::renderUI({
    shiny::req(input$responseVar, length(input$responseVar) > 1)
    
    current_idx <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total_vars <- length(input$responseVar)
    
    shiny::div(style = "display: inline-block;",
        shiny::actionButton(ns("prevValidationVar"), "", icon = shiny::icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        shiny::span(paste("Variable", current_idx, "sur", total_vars, ":", input$responseVar[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        shiny::actionButton(ns("nextValidationVar"), "", icon = shiny::icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  output$showModelNavigation <- shiny::reactive({
    !is.null(values$modelList) && length(values$modelList) > 1
  })
  shiny::outputOptions(output, "showModelNavigation", suspendWhenHidden = FALSE)
  
  output$modelDiagNavigation <- shiny::renderUI({
    shiny::req(values$modelList, length(values$modelList) > 1)
    
    current_idx <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total_vars <- length(values$modelList)
    var_names <- names(values$modelList)
    
    shiny::div(style = "display: inline-block; margin-bottom: 15px;",
        shiny::actionButton(ns("prevModelVar"), "", icon = shiny::icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        shiny::span(paste("Modèle", current_idx, "sur", total_vars, ":", var_names[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        shiny::actionButton(ns("nextModelVar"), "", icon = shiny::icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  output$showResidNavigation <- shiny::reactive({
    !is.null(values$modelList) && length(values$modelList) > 1
  })
  shiny::outputOptions(output, "showResidNavigation", suspendWhenHidden = FALSE)
  
  output$residNavigation <- shiny::renderUI({
    shiny::req(values$modelList, length(values$modelList) > 1)
    
    current_idx <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total_vars <- length(values$modelList)
    var_names <- names(values$modelList)
    
    shiny::div(style = "display: inline-block; margin-bottom: 15px;",
        shiny::actionButton(ns("prevResidVar"), "", icon = shiny::icon("chevron-left"), 
                     style = "margin-right: 10px;", class = "btn-sm"),
        shiny::span(paste("Variable", current_idx, "sur", total_vars, ":", var_names[current_idx]),
             style = "vertical-align: middle; margin: 0 15px; font-weight: bold;"),
        shiny::actionButton(ns("nextResidVar"), "", icon = shiny::icon("chevron-right"), 
                     style = "margin-left: 10px;", class = "btn-sm")
    )
  })
  
  shiny::observeEvent(input$prevValidationVar, {
    current <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total <- length(input$responseVar)
    values$currentValidationVar <- if (current > 1) current - 1 else total
  })
  
  shiny::observeEvent(input$nextValidationVar, {
    current <- if (is.null(values$currentValidationVar)) 1 else values$currentValidationVar
    total <- length(input$responseVar)
    values$currentValidationVar <- if (current < total) current + 1 else 1
  })
  
  shiny::observeEvent(input$prevModelVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current > 1) current - 1 else total
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  shiny::observeEvent(input$nextModelVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current < total) current + 1 else 1
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  shiny::observeEvent(input$prevResidVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current > 1) current - 1 else total
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  shiny::observeEvent(input$nextResidVar, {
    current <- if (is.null(values$currentModelVar)) 1 else values$currentModelVar
    total <- length(values$modelList)
    values$currentModelVar <- if (current < total) current + 1 else 1
    values$currentModel <- values$modelList[[values$currentModelVar]]
  })
  
  output$normalityResults <- shiny::renderPrint({
    shiny::req(values$normalityResults, input$responseVar)
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
  
  output$normalityInterpretation <- shiny::renderUI({
    shiny::req(values$normalityResults, input$responseVar)
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
    shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>"))
  })
  
  output$homogeneityResults <- shiny::renderPrint({
    shiny::req(values$homogeneityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    hom <- values$homogeneityResults[[current_var]]
    
    if (is.null(hom)) {
      cat("Aucun résultat d'homogénéité disponible pour cette variable.\n")
    } else {
      cat("p = ", hom$`Pr(>F)`[1], "\n")
    }
  })
  
  output$homogeneityInterpretation <- shiny::renderUI({
    shiny::req(values$homogeneityResults, input$responseVar)
    current_var <- input$responseVar[values$currentValidationVar]
    hom <- values$homogeneityResults[[current_var]]
    
    if (is.null(hom)) {
      interp_text <- "Aucun résultat d'homogénéité disponible pour cette variable."
    } else {
      interp_text <- interpret_homogeneity(hom$`Pr(>F)`[1])
    }
    shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>"))
  })
  
  output$modelDiagnostics <- shiny::renderPlot({
    shiny::req(values$currentModel)
    
    tryCatch({
      # Vérifier si le modèle a des problèmes de leverage
      model <- values$currentModel
      h <- stats::hatvalues(model)
      
      # Si tous les leverage sont 0 ou très proche de 0
      if (all(h < 1e-10) || sum(h > 0) < 3) {
        graphics::par(mfrow = c(1, 1))
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        graphics::text(1, 1, "Ajustement parfait détecté\nLes diagnostics graphiques standards ne sont pas disponibles\nVoir les tests numériques ci-dessous", 
             cex = 1.2, col = "red")
        return()
      }
      
      graphics::par(mfrow = c(2, 2))
      plot(model, which = 1:4)
      
    }, error = function(e) {
      graphics::par(mfrow = c(1, 1))
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      graphics::text(1, 1, paste("Erreur dans les diagnostics graphiques:\n", 
                       substr(e$message, 1, 100), 
                       "\n\nVoir les tests numériques ci-dessous"), 
           cex = 1, col = "red")
    })
  })
  
  output$modelDiagnosticsInterpretation <- shiny::renderUI({
    shiny::req(values$currentModel)
    
    tryCatch({
      model <- values$currentModel
      h <- stats::hatvalues(model)
      
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
      
      shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>"))
      
    }, error = function(e) {
      shiny::HTML("<div class='hstat-interpretation'><span style='color: red;'>Erreur dans l'interprétation des diagnostics</span></div>")
    })
  })
  
  output$downloadModelDiagnostics <- shiny::downloadHandler(
    filename = function() {
      paste0("diagnostics_modèle_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Le peripherique, le chemin d'echec et l'image portant le motif etaient
      # ecrits ici a la main. Ils vivent desormais chez `hstat_ecrire_image()` :
      # ne reste que le DESSIN, qui est le seul propre a cet export.
      model <- values$currentModel
      dessin <- function() {
        h <- stats::hatvalues(model)
        if (all(h < 1e-10) || sum(h > 0) < 3) {
          graphics::par(mfrow = c(1, 1))
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          graphics::text(1, 1, "Ajustement parfait détecté\nLes diagnostics graphiques ne sont pas disponibles",
               cex = 1.5, col = "red")
        } else {
          graphics::par(mfrow = c(2, 2))
          plot(model, which = 1:4)
        }
      }
      # 10,67 x 8 pouces a 300 DPI : exactement les 3200 x 2400 px d'avant.
      hstat_ecrire_image(file, dessin, "png", 10.67, 8, 300)
    }
  )
  
  output$downloadQQPlot <- shiny::downloadHandler(
    filename = function() {
      paste0("qqplot_residus_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Meme principe que les diagnostics : le module construit le graphique,
      # l'ecrivain commun se charge du fichier. Le motif d'indisponibilite est
      # porte par `echec`, et sort en image valide au lieu d'un peripherique
      # ouvert a la main dans chaque branche.
      motif <- NULL
      p <- tryCatch({
        shiny::req(values$currentModel)
        residuals_data <- stats::residuals(values$currentModel)
        residuals_data <- residuals_data[!is.na(residuals_data)]

        if (length(residuals_data) < 3 || stats::sd(residuals_data) < 1e-10) {
          # `<-` et non `<<-` : l'expression d'un tryCatch s'evalue dans le
          # cadre APPELANT, donc ici meme. C'est dans le GESTIONNAIRE, plus
          # bas, que `<<-` est indispensable.
          motif <- "QQ-plot non disponible : les résidus sont constants ou trop peu nombreux."
          return(NULL)
        }

        n <- length(residuals_data)
        theoretical_quantiles <- stats::qnorm(stats::ppoints(n))
        sample_quantiles <- sort(residuals_data)
        
        se <- (stats::sd(residuals_data) / sqrt(n)) * sqrt(theoretical_quantiles^2 + 1)
        upper_band <- theoretical_quantiles + 1.96 * se
        lower_band <- theoretical_quantiles - 1.96 * se
        
        df <- data.frame(
          theoretical = theoretical_quantiles,
          sample = sample_quantiles,
          upper = upper_band,
          lower = lower_band
        )
        
        p <- ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = sample)) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) +
          ggplot2::geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
          ggplot2::geom_point(shape = 1, size = 2) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = "QQ-plot des résidus", 
               x = "Quantiles théoriques", 
               y = "Quantiles observés") +
          ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0.5))

        p
      }, error = function(e) {
        motif <<- hstat_err_fr(e, "QQ-plot des résidus")
        NULL
      })
      hstat_ecrire_image(file, p, "png", 10, 8, 300, echec = motif)
    }
  )
  
  output$qqPlotResiduals <- shiny::renderPlot({
    shiny::req(values$currentModel)
    
    tryCatch({
      residuals_data <- stats::residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        graphics::text(1, 1, "Pas assez de résidus pour le QQ-plot (n < 3)", cex = 1.2, col = "red")
        return()
      }
      
      if (stats::sd(residuals_data) < 1e-10) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        graphics::text(1, 1, "Résidus constants (ajustement parfait)\nQQ-plot non applicable", cex = 1.2, col = "orange")
        return()
      }
      
      n <- length(residuals_data)
      theoretical_quantiles <- stats::qnorm(stats::ppoints(n))
      sample_quantiles <- sort(residuals_data)
      
      se <- (stats::sd(residuals_data) / sqrt(n)) * sqrt(theoretical_quantiles^2 + 1)
      upper_band <- theoretical_quantiles + 1.96 * se
      lower_band <- theoretical_quantiles - 1.96 * se
      
      df <- data.frame(
        theoretical = theoretical_quantiles,
        sample = sample_quantiles,
        upper = upper_band,
        lower = lower_band
      )
      
      ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = sample)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) +
        ggplot2::geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        ggplot2::geom_point(shape = 1, size = 2) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "QQ-plot des résidus", 
             x = "Quantiles théoriques", 
             y = "Quantiles observés") +
        ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0.5))
      
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      graphics::text(1, 1, paste(strwrap(hstat_err_fr(e, "QQ-plot"), 55), collapse = "\n"), cex = 0.85, col = "red")
    })
  })
  
  output$qqPlotInterpretation <- shiny::renderUI({
    shiny::req(values$currentModel)
    
    tryCatch({
      residuals_data <- stats::residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        interp_text <- "<span style='color: red;'>Pas assez de résidus pour évaluer la normalité.</span>"
      } else if (stats::sd(residuals_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Résidus constants (ajustement parfait). Normalité non évaluable.</span>"
      } else {
        interp_text <- "Les points devraient suivre la ligne droite pour une normalité des résidus.<br>
      <strong>Déviations acceptables :</strong> Légères aux extrémités<br>
      <strong>Problèmes :</strong> Courbure prononcée, points très éloignés de la ligne"
      }
      
      shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>"))
      
    }, error = function(e) {
      shiny::HTML("<div class='hstat-interpretation'><span style='color: red;'>Erreur dans l'interprétation du QQ-plot</span></div>")
    })
  })
  
  output$normalityResult <- shiny::renderPrint({
    shiny::req(values$currentModel)
    
    tryCatch({
      residuals_data <- stats::residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        cat("Nombre d'observations insuffisant pour le test de Shapiro-Wilk (n < 3).\n")
      } else if (length(residuals_data) > 5000) {
        cat("Trop d'observations pour le test de Shapiro-Wilk (n > 5000).\n")
        cat("Utilisez le QQ-plot ci-dessus pour évaluer visuellement la normalité.\n")
      } else if (stats::sd(residuals_data) < 1e-10) {
        cat("Résidus constants ou quasi-constants (ajustement parfait).\n")
        cat("Le test de normalité n'est pas applicable.\n")
      } else {
        hstat_shapiro(residuals_data)
      }
    }, error = function(e) {
      cat("Erreur dans le test de normalité :", e$message, "\n")
    })
  })
  
  output$normalityResidInterpretation <- shiny::renderUI({
    shiny::req(values$currentModel)
    
    tryCatch({
      residuals_data <- stats::residuals(values$currentModel)
      residuals_data <- residuals_data[!is.na(residuals_data)]
      
      if (length(residuals_data) < 3) {
        interp_text <- "Nombre d'observations insuffisant pour le test de Shapiro-Wilk."
      } else if (length(residuals_data) > 5000) {
        interp_text <- "Trop d'observations pour Shapiro-Wilk. Référez-vous au QQ-plot."
      } else if (stats::sd(residuals_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Résidus constants (ajustement parfait). Test non applicable.</span>"
      } else {
        norm_test <- hstat_shapiro(residuals_data)
        interp_text <- interpret_normality_resid(norm_test$p.value)
      }
      
      shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>"))
    }, error = function(e) {
      shiny::HTML("<div class='hstat-interpretation'><span style='color: red;'>Erreur dans le test de normalité</span></div>")
    })
  })
  
  output$leveneResidResult <- shiny::renderPrint({
    shiny::req(values$currentModel)
    
    tryCatch({
      residuals_data <- stats::residuals(values$currentModel)
      fitted_data <- stats::fitted(values$currentModel)
      
      # Vérifier s'il y a une variation dans les valeurs ajustées
      if (stats::sd(fitted_data) < 1e-10) {
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
      cat("Erreur dans le test d'homogénéité :", e$message, "\n")
    })
  })
  
  output$homogeneityResidInterpretation <- shiny::renderUI({
    shiny::req(values$currentModel)
    
    tryCatch({
      residuals_data <- stats::residuals(values$currentModel)
      fitted_data <- stats::fitted(values$currentModel)
      
      if (stats::sd(fitted_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Valeurs ajustées constantes (ajustement parfait). Test non applicable.</span>"
        return(shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>")))
      }
      
      n_unique <- length(unique(fitted_data))
      if (n_unique < 2) {
        interp_text <- "<span style='color: orange;'>Pas assez de variation dans les prédictions. Test non applicable.</span>"
        return(shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>")))
      }
      
      fitted_factor <- cut(fitted_data, breaks = 2, labels = c("Bas", "Haut"))
      
      if (length(levels(fitted_factor)) < 2 || any(table(fitted_factor) < 2)) {
        interp_text <- "<span style='color: orange;'>Impossible de créer deux groupes équilibrés. Test non applicable.</span>"
        return(shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>")))
      }
      
      test_data <- data.frame(residuals = residuals_data, fitted_group = fitted_factor)
      hom_test <- car::leveneTest(residuals ~ fitted_group, data = test_data)
      interp_text <- interpret_homogeneity_resid(hom_test$`Pr(>F)`[1])
      
      shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>"))
      
    }, error = function(e) {
      shiny::HTML("<div class='hstat-interpretation'><span style='color: red;'>Erreur dans le test d'homogénéité</span></div>")
    })
  })
  
  output$autocorrResult <- shiny::renderPrint({
    shiny::req(values$currentModel)
    
    tryCatch({
      residuals_data <- stats::residuals(values$currentModel)
      
      if (length(residuals_data) < 3) {
        cat("Nombre d'observations insuffisant pour le test de Durbin-Watson (n < 3).\n")
        return()
      }
      
      if (stats::sd(residuals_data) < 1e-10) {
        cat("Résidus constants (ajustement parfait).\n")
        cat("Le test d'autocorrélation n'est pas applicable.\n")
        return()
      }
      
      lmtest::dwtest(values$currentModel)
      
    }, error = function(e) {
      cat("Erreur dans le test de Durbin-Watson :", e$message, "\n")
    })
  })
  
  output$autocorrInterpretation <- shiny::renderUI({
    shiny::req(values$currentModel)
    
    tryCatch({
      residuals_data <- stats::residuals(values$currentModel)
      
      if (length(residuals_data) < 3) {
        interp_text <- "Nombre d'observations insuffisant pour le test de Durbin-Watson."
        return(shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>")))
      }
      
      if (stats::sd(residuals_data) < 1e-10) {
        interp_text <- "<span style='color: orange;'>Résidus constants (ajustement parfait). Test non applicable.</span>"
        return(shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>")))
      }
      
      dw_test <- lmtest::dwtest(values$currentModel)
      # Un modele degenere (residus constants) rend p = NA : on le dit au lieu
      # de laisser `if (p > 0.05)` faire tomber la sortie.
      interp_text <- switch(
        hstat_p_verdict(dw_test$p.value),
        "non significatif" = "Pas d'autocorrélation significative des résidus (p > 0.05).",
        "significatif" = "Autocorrélation significative des résidus (p < 0.05). Vérifiez l'indépendance des observations.",
        "Test de Durbin-Watson non calculable sur ce modèle (résidus dégénérés).")
      
      shiny::HTML(paste0("<div class='hstat-interpretation'>", interp_text, "</div>"))
      
    }, error = function(e) {
      shiny::HTML("<div class='hstat-interpretation'><span style='color: red;'>Erreur dans le test d'autocorrélation</span></div>")
    })
  })
  
  output$modelSummary <- shiny::renderPrint({
    shiny::req(values$currentModel)
    summary(values$currentModel)
  })
  
  output$downloadTestsExcel <- hstat_classeur_handler(function() {
    validation <- NULL
    if (!is.null(values$normalityResults)) {
      validation <- data.frame(Variable = names(values$normalityResults))
      validation$Normality_p <- sapply(values$normalityResults,
                                       function(x) x$p.value %||% NA)
      validation$Homogeneity_p <- sapply(values$homogeneityResults,
                                         function(x) x$`Pr(>F)`[1] %||% NA)
    }
    hstat_tables_non_vides(list("Résultats" = values$testResultsDF,
                                "Validation" = validation))
  }, "resultats_tests", "Tests statistiques")
  
  # Module Chi² / Multinomial -- Tests du Khi², comparaisons paires, graphiques
  
  
  chi2_palette <- function(n) {
    cols <- c("#1565C0","#2E7D32","#C62828","#6A1B9A","#E65100",
              "#00695C","#AD1457","#4E342E","#37474F","#F9A825")
    if (n <= length(cols)) return(cols[seq_len(n)])
    grDevices::colorRampPalette(cols)(n)
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
      tt  <- tryCatch(stats::binom.test(oi, oi + oj, p = 0.5), error = function(e) NULL)
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
  
  
  shiny::observeEvent(input$runChiSqTest, {
    df <- values$filteredData %||% values$cleanData %||% values$data
    shiny::req(df)
    
    if (is.null(input$factorVar) || length(input$factorVar) == 0 ||
        is.null(input$responseVar) || length(input$responseVar) == 0) {
      shiny::showNotification(
        "Veuillez sélectionner une variable réponse (numérique) et un facteur (catégoriel) dans « Paramètres des tests ».",
        type = "warning", duration = 6
      )
      return()
    }
    
    col_cat  <- input$factorVar[1]
    col_num  <- input$responseVar[1]
    type_d   <- input$chiSqDataType  # "fréquences" ou "pourcentages"
    methode  <- input$chiSqMethod    # "chisq" ou "multinomial"
    
    if (!col_cat %in% names(df) || !col_num %in% names(df)) {
      shiny::showNotification("Variables introuvables dans les données.", type = "error"); return()
    }
    
    modalites_brutes <- as.character(df[[col_cat]])
    valeurs_brutes   <- as.numeric(df[[col_num]])
    
    ok_na <- !is.na(valeurs_brutes) & !is.na(modalites_brutes)
    if (any(!ok_na)) {
      shiny::showNotification("Valeurs manquantes détectées — ignorées.", type = "warning")
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
    if (n < 2) { shiny::showNotification("Au moins 2 modalités requises.", type = "error"); return() }
    if (n > 100) {
      shiny::showNotification(
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
    
    shiny::withProgress(message = "Test chi² en cours...", value = 0.3, {
      
      if (methode == "chisq") {
        res_test <- tryCatch(
          stats::chisq.test(observed, p = rep(1/n, n)),
          warning = function(w) {
            shiny::showNotification(hstat_err_fr(w, "Avertissement"), type = "warning", duration = 10)
            suppressWarnings(stats::chisq.test(observed, p = rep(1/n, n)))
          },
          error = function(e) { shiny::showNotification(hstat_err_fr(e, "Erreur chi²"), type = "error"); NULL }
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
          error = function(e) { shiny::showNotification(hstat_err_fr(e, "Erreur multinomial"), type = "error"); NULL }
        )
        if (is.null(res_test)) return()
        stat_name <- "Multinomial"
        stat_val  <- NA
        df_test   <- n - 1
        p_val     <- res_test$p.value
        attendus  <- observed / sum(observed) * sum(observed)  # proportions égales
      }
      
      shiny::incProgress(0.3)
      
      paires    <- utils::combn(n, 2)
      nb_paires <- ncol(paires)
      ph        <- chi2_group_letters(modalites, observed, nb_paires, paires)
      
      shiny::incProgress(0.3)
      
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
    
    shiny::showNotification(
      trf("Test %s terminé -- p = %s", toupper(methode), fmt_p(p_val)),
      type = if (p_val < 0.05) "message" else "warning",
      duration = 5
    )
    
    shiny::updateTabsetPanel(session, "chiSqResultsTabs", selected = "chiSq_résumé")
  })
  
  
  creer_graphique_chi2 <- shiny::reactive({
    shiny::req(values$chiSqFreqData)
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
    if (n > length(pal)) pal <- grDevices::colorRampPalette(pal)(n)
    
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
      g <- ggplot2::ggplot(df_plot, ggplot2::aes(x = modalite, y = valeur, fill = modalite)) +
        ggplot2::geom_col(width = 0.65, color = "white", linewidth = 0.4) +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::labs(subtitle = sous_titre, x = NULL, y = "Valeur") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
      if (show_val)
        g <- g + ggplot2::geom_text(ggplot2::aes(label = lbl_fn(valeur, groupe)), vjust = -0.4,
                           size = 4, fontface = "bold")
      
    } else if (type_g == "bar_h") {
      g <- ggplot2::ggplot(df_plot, ggplot2::aes(x = valeur, y = stats::reorder(modalite, valeur), fill = modalite)) +
        ggplot2::geom_col(width = 0.65, color = "white") +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::labs(subtitle = sous_titre, x = "Valeur", y = NULL) +
        ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(legend.position = "none")
      if (show_val)
        g <- g + ggplot2::geom_text(ggplot2::aes(label = lbl_fn(valeur, groupe)), hjust = -0.1,
                           size = 4, fontface = "bold")
      
    } else if (type_g == "pie") {
      df_plot$pct <- df_plot$valeur / sum(df_plot$valeur) * 100
      lbl_pie <- paste0(df_plot$modalite,
                        if (show_val)  paste0("\n", round(df_plot$pct, 1), "%") else "",
                        if (show_grp) paste0("\n(", df_plot$groupe, ")") else "")
      g <- ggplot2::ggplot(df_plot, ggplot2::aes(x = "", y = valeur, fill = modalite)) +
        ggplot2::geom_col(width = 1, color = "white") + ggplot2::coord_polar("y") +
        ggplot2::geom_text(ggplot2::aes(label = lbl_pie), position = ggplot2::position_stack(vjust = 0.5),
                  size = 4, fontface = "bold") +
        ggplot2::scale_fill_manual(values = pal, name = NULL) +
        ggplot2::labs(subtitle = sous_titre) +
        ggplot2::theme_void(base_size = 13) + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
      
    } else if (type_g == "donut") {
      df_plot$pct  <- df_plot$valeur / sum(df_plot$valeur) * 100
      df_plot$ymax <- cumsum(df_plot$pct)
      df_plot$ymin <- c(0, utils::head(df_plot$ymax, -1))
      df_plot$mid  <- (df_plot$ymin + df_plot$ymax) / 2
      g <- ggplot2::ggplot(df_plot, ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = modalite)) +
        ggplot2::geom_rect(color = "white", linewidth = 0.5) +
        ggplot2::coord_polar(theta = "y") + ggplot2::xlim(0, 4) +
        ggplot2::scale_fill_manual(values = pal, name = NULL) +
        ggplot2::labs(subtitle = sous_titre) +
        ggplot2::theme_void(base_size = 13) + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
      if (show_val)
        g <- g + ggplot2::geom_text(ggplot2::aes(x = 3.25, y = mid,
                               label = paste0(round(pct, 1), "%",
                                              if (show_grp) paste0("\n(", groupe, ")") else "")),
                           size = 3.5, fontface = "bold")
      
    } else if (type_g == "lollipop") {
      g <- ggplot2::ggplot(df_plot, ggplot2::aes(x = stats::reorder(modalite, -valeur), y = valeur, color = modalite)) +
        ggplot2::geom_segment(ggplot2::aes(xend = modalite, yend = 0), linewidth = 1.5) +
        ggplot2::geom_point(size = 7) +
        ggplot2::scale_color_manual(values = pal) +
        ggplot2::labs(subtitle = sous_titre, x = NULL, y = "Valeur") +
        ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(legend.position = "none")
      if (show_val)
        g <- g + ggplot2::geom_text(ggplot2::aes(label = lbl_fn(valeur, groupe)),
                           vjust = -1.2, size = 4, fontface = "bold")
      
    } else if (type_g == "residus") {
      df_r <- data.frame(
        modalite = factor(fdf$Modalite, levels = fdf$Modalite),
        residu   = fdf$Residu_std,
        couleur  = ifelse(fdf$Residu_std > 1.96, "Sur-représenté",
                          ifelse(fdf$Residu_std < -1.96, "Sous-représenté", "Conforme"))
      )
      g <- ggplot2::ggplot(df_r, ggplot2::aes(x = modalite, y = residu, fill = couleur)) +
        ggplot2::geom_col(color = "white", width = 0.65) +
        ggplot2::geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
        ggplot2::scale_fill_manual(values = c("Sur-représenté" = "#1565C0",
                                     "Sous-représenté" = "#C62828",
                                     "Conforme"        = "#9E9E9E")) +
        ggplot2::labs(subtitle = sous_titre, x = NULL, y = "Résidu standardisé", fill = "Statut") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
      if (show_grp)
        g <- g + ggplot2::geom_text(ggplot2::aes(label = paste0("(", fdf$Groupe, ")")),
                           vjust = ifelse(df_r$residu >= 0, -0.5, 1.2),
                           size = 4, fontface = "bold", color = "black")
      
    } else {  # histogramme classique
      g <- ggplot2::ggplot(df_plot, ggplot2::aes(x = modalite, y = valeur, fill = modalite)) +
        ggplot2::geom_col(width = 1, color = "white", linewidth = 0.3) +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::labs(subtitle = sous_titre, x = NULL, y = "Valeur") +
        ggplot2::theme_classic(base_size = 13) + ggplot2::theme(legend.position = "none")
      if (show_val)
        g <- g + ggplot2::geom_text(ggplot2::aes(label = lbl_fn(valeur, groupe)), vjust = -0.3,
                           size = 4, fontface = "bold")
    }
    
    titre <- if (!is.null(input$chiSqGraphTitle) && nzchar(input$chiSqGraphTitle))
      input$chiSqGraphTitle else
        paste0("Distribution -- ", values$chiSqResults$Variable_cat[1])
    
    g <- g + ggplot2::labs(title = titre) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5),
            plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "#555"))
    
    values$chiSqPlotObj <- g
    g
  })
  
  output$downloadChiSqPlot <- shiny::downloadHandler(
    filename = function() paste0("chi2_graphique_", Sys.Date(), ".png"),
    content  = function(file) {
      shiny::req(values$chiSqPlotObj)
      w <- (input$chiSqGraphWidth  %||% 800) / 96
      h <- (input$chiSqGraphHeight %||% 500) / 96
      # Sans fichier ecrit, Shiny renvoie sa page d'erreur HTML sous « .png ».
      hstat_ecrire_image(file, tryCatch(creer_graphique_chi2(), error = function(e) NULL),
                         "png", w, h, .hstat_num1(input$chiSqGraphDPI, 150))
    }
  )
  
  output$downloadChiSqExcel <- shiny::downloadHandler(
    filename = function() paste0("chi2_résultats_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      shiny::req(values$chiSqResults)
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
      shiny::showNotification("Excel exporté avec succès.", type = "message", duration = 3)
    }
  )
  
  output$downloadChiSqCSV <- shiny::downloadHandler(
    filename = function() paste0("chi2_modalités_", Sys.Date(), ".csv"),
    content  = function(file) {
      shiny::req(values$chiSqFreqData)
      utils::write.csv(values$chiSqFreqData, file, row.names = FALSE)
    }
  )
  
  # UI dynamique : renommage des modalités (labels niveaux X)
  # UI info taille export (pixels calculés à partir de DPI x pouces)
  # Helper : carte de renommage des modalités (niveaux X)
  chi2_ph_level_map <- function(fdf) {
    levs <- as.character(fdf$Modalite)
    map  <- stats::setNames(levs, levs)
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
      gg <- ggplot2::ggplot(df_plot, ggplot2::aes(x = modalite, y = pct, fill = modalite)) +
        ggplot2::geom_col(width = 0.65, color = "white") +
        ggplot2::labs(title = titre, subtitle = sous_t,
             x = x_lab, y = y_lab, fill = leg_tit) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position  = if (!is.null(leg_tit)) "right" else "none",
              axis.text.x      = ggplot2::element_text(angle = 20, hjust = 1),
              plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
              plot.subtitle    = ggplot2::element_text(hjust = 0.5))
      if (show_v) gg <- gg +
          ggplot2::geom_text(ggplot2::aes(label = lbl_fn(pct, groupe)), vjust = -0.4, size = 4, fontface = "bold")
      gg
      
    } else if (type_g == "pie") {
      lbl_pie <- paste0(df_plot$modalite,
                        if (show_v) paste0("\n", round(df_plot$pct, 1), "%") else "",
                        if (show_g) paste0("\n(", df_plot$groupe, ")") else "")
      ggplot2::ggplot(df_plot, ggplot2::aes(x = "", y = pct, fill = modalite)) +
        ggplot2::geom_col(width = 1, color = "white") + ggplot2::coord_polar("y") +
        ggplot2::geom_text(ggplot2::aes(label = lbl_pie),
                  position = ggplot2::position_stack(vjust = 0.5), size = 4, fontface = "bold") +
        ggplot2::labs(title = titre, subtitle = sous_t, fill = leg_tit) +
        ggplot2::theme_void(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
              plot.subtitle = ggplot2::element_text(hjust = 0.5),
              legend.title  = if (!is.null(leg_tit)) ggplot2::element_text(face = "bold") else ggplot2::element_blank())
      
    } else {
      df_r <- data.frame(
        modalite = factor(modalites_label, levels = unique(modalites_label)),
        residu   = fdf$Residu_std,
        couleur  = ifelse(fdf$Residu_std >  1.96, "Sur-représenté",
                          ifelse(fdf$Residu_std < -1.96, "Sous-représenté", "Conforme"))
      )
      ggplot2::ggplot(df_r, ggplot2::aes(x = modalite, y = residu, fill = couleur)) +
        ggplot2::geom_col(color = "white", width = 0.65) +
        ggplot2::geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
        ggplot2::scale_fill_manual(values = c("Sur-représenté" = "#1565C0",
                                     "Sous-représenté" = "#C62828",
                                     "Conforme"              = "#9E9E9E")) +
        ggplot2::labs(title = titre, subtitle = sous_t,
             x = x_lab, y = y_lab, fill = leg_tit %||% "Statut") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
              plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
              plot.subtitle = ggplot2::element_text(hjust = 0.5))
    }
  }
  
  
  # ---- Comparaisons multiples PostHoc  ----
  
  calc_cv <- function(x) {
    if (length(x) <= 1 || stats::sd(x, na.rm = TRUE) == 0) return(0)
    return((stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100)
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
        model <- stats::aov(stats::as.formula(paste0(bt(var), " ~ ", bt(factor1))), data = df_subset)
        
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
          emm <- emmeans::emmeans(model, stats::as.formula(paste0("~ `", factor1, "`")))
          mc <- graphics::pairs(emm, adjust = "bonferroni")
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
          emm <- emmeans::emmeans(model, stats::as.formula(paste0("~ `", factor1, "`")))
          groups_cld <- multcomp::cld(emm, Letters = letters)
          groups <- as.data.frame(groups_cld)
          groups <- groups[, c(factor1, ".group")]
          colnames(groups) <- c(factor1, "groups")
          groups$groups <- trimws(groups$groups)
        } else if (test_method == "games") {
          bt_loc <- function(x) paste0("`", x, "`")
          mc <- PMCMRplus::gamesHowellTest(stats::as.formula(paste0(bt_loc(var), " ~ ", bt_loc(factor1))), data = df_subset)
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
          mc <- PMCMRplus::kwAllPairsDunnTest(df_subset[[var]], df_subset[[factor1]])
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
        dplyr::group_by(dplyr::across(dplyr::all_of(factor1))) %>%
        dplyr::summarise(
          Moyenne = mean(.data[[var]], na.rm = TRUE),
          Ecart_type = stats::sd(.data[[var]], na.rm = TRUE),
          N = dplyr::n(),
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
  
  
  shiny::observeEvent(values$testResultsDF, {
    shiny::req(values$testResultsDF)
    
    if (!is.null(values$currentTestType)) {
      new_type <- if (values$currentTestType == "parametric") "param" else "nonparam"
      shiny::updateRadioButtons(session, "testType", selected = new_type)
    }
    
    values$postHocSyncTrigger <- stats::runif(1)
    
    shiny::showNotification(
      shiny::tagList(
        shiny::icon("link"), " PostHoc mis à jour avec les résultats des tests (",
        nrow(values$testResultsDF), " résultats)"
      ),
      type = "message", duration = 3
    )
  }, ignoreInit = TRUE)
  
  # - Tableau récapitulatif des p-values pour guider le PostHoc
  output$testResultsSummaryForPostHoc <- shiny::renderUI({
    if (is.null(values$testResultsDF) || nrow(values$testResultsDF) == 0) {
      return(shiny::div(
        style = "padding:10px; background:#fff8e1; border-radius:4px; border-left:3px solid #ff9800; font-size:12px;",
        shiny::icon("exclamation-triangle", style="color:#e65100;"),
        " Aucun résultat de test disponible.",
        shiny::tags$br(),
        shiny::tags$small(style="color:#bf360c;",
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
      
      shiny::tags$tr(
        style = paste0("background:", bg_col, "; border-bottom:1px solid #e0e0e0;"),
        shiny::tags$td(style = "padding:4px 8px; font-size:11.5px; font-weight:600;", row$Variable),
        shiny::tags$td(style = "padding:4px 8px; font-size:11.5px;", row$Facteur %||% "-"),
        shiny::tags$td(style = "padding:4px 8px; font-size:11.5px;", row$Test),
        shiny::tags$td(style = paste0("padding:4px 8px; font-size:12px; font-weight:bold; color:", p_col, ";"),
                if (!is.na(p_val)) formatC(p_val, format="e", digits=3) else "NA"),
        shiny::tags$td(style = paste0("padding:4px 8px; font-size:13px; font-weight:bold; color:", p_col, ";"), sig)
      )
    })
    
    n_sig <- sum(!is.na(df$p_value) & df$p_value < 0.05)
    
    shiny::tagList(
      shiny::div(
        style = "margin-bottom:8px; padding:8px 10px; background:#e3f2fd; border-left:4px solid #1976d2; border-radius:4px;",
        shiny::icon("table", style="color:#1565c0;"),
        shiny::tags$b(style="color:#0d47a1; font-size:12px;",
               trf(" Résultats des tests -- %s/%s significatif(s)", n_sig, nrow(df))),
        shiny::tags$br(),
        shiny::tags$small(style="color:#1976d2;",
                   "Les variables en vert (p < 0.05) sont pré-sélectionnées dans l'analyse PostHoc.")
      ),
      shiny::div(
        style = "max-height:200px; overflow-y:auto; border:1px solid #e0e0e0; border-radius:4px;",
        shiny::tags$table(
          style = "width:100%; border-collapse:collapse;",
          shiny::tags$thead(
            shiny::tags$tr(
              style = "background:#1976d2; color:white;",
              shiny::tags$th(style="padding:5px 8px; font-size:11px;", "Variable"),
              shiny::tags$th(style="padding:5px 8px; font-size:11px;", "Facteur"),
              shiny::tags$th(style="padding:5px 8px; font-size:11px;", "Test"),
              shiny::tags$th(style="padding:5px 8px; font-size:11px;", "p-value"),
              shiny::tags$th(style="padding:5px 8px; font-size:11px;", "Sig.")
            )
          ),
          shiny::tags$tbody(rows)
        )
      )
    )
  })
  
  shiny::observeEvent(input$runMultiple, {
    shiny::req(input$multiResponse, input$multiFactor)
    
    if (isTRUE(input$testType == "param" && input$multiTest == "lm_emmeans")) {
      if (is.null(values$modelList) || length(values$modelList) == 0) {
        shiny::showNotification(paste0("Aucun modèle LM/GLM disponible. Lancez d'abord ",
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
        shiny::showNotification("Aucun prédicteur catégoriel détecté dans le(s) modèle(s).",
                         type = "warning")
        values$lmPostHocResults <- NULL
      } else {
        values$lmPostHocResults <- results
        shiny::showNotification(trf("PostHoc LM/GLM calculé : %s combinaison(s) Variable × Prédicteur.", length(results)),
                         type = "message", duration = 4)
      }
      return()
    }
    
    shiny::updateTabsetPanel(session, "resultsTabs", selected = "mainEffects")
    
    shiny::showNotification("Analyse en cours...", type = "message", duration = NULL, id = "loading")
    
    multi_results_list <- list()
    simple_effects_list <- list()
    # Precision des seules chaines « moyenne ± dispersion ». Les colonnes
    # numeriques, elles, restent en pleine precision : c'est `round_numeric_df`
    # qui les arrondit A L'AFFICHAGE, et seulement si l'utilisateur le demande.
    .dec_aff <- hstat_dec_affichage(input$multiRoundResults, input$multiDecimals)
    df <- values$filteredData
    
    if (is.null(df) || nrow(df) == 0) {
      shiny::showNotification("Aucune donnée disponible pour l'analyse.", type="error", duration=5)
      shiny::removeNotification("loading")
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
              shiny::showNotification(trf("Variable '%s' non convertible en numérique.", var), type="warning", duration=4)
              next
            }
          }
          df_var <- df[!is.na(df[[var]]), ]
          if (!is.factor(df_var[[fvar]])) df_var[[fvar]] <- factor(as.character(df_var[[fvar]]))
          df_var[[fvar]] <- droplevels(df_var[[fvar]])
          if (nlevels(df_var[[fvar]]) < 2) {
            shiny::showNotification(trf("PostHoc '%s' : moins de 2 niveaux après nettoyage.", fvar), type="warning", duration=4)
            next
          }
          if (nrow(df_var) < 4) { next }
          if (input$testType == "param") {
            bt <- function(x) paste0("`", x, "`")
            model <- stats::aov(stats::as.formula(paste0(bt(var), " ~ ", bt(fvar))), data = df_var)
            
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
                shiny::showNotification(trf("PostHoc '%s' : moins de 2 niveaux, test ignoré.", fvar), type="warning", duration=4)
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
              emm <- emmeans::emmeans(model, stats::as.formula(paste0("~ `", fvar, "`")))
              # emmeans accepte tukey/bonferroni/holm/BH/... ; "none" = brut
              pm_adj <- input$multiParamAdjust %||% "bonferroni"
              mc <- graphics::pairs(emm, adjust = if (identical(pm_adj, "none")) "none" else pm_adj)
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
              emm <- emmeans::emmeans(model, stats::as.formula(paste0("~ `", fvar, "`")))
              groups_cld <- multcomp::cld(emm, Letters = letters)
              groups <- as.data.frame(groups_cld)
              groups <- groups[, c(fvar, ".group")]
              colnames(groups) <- c(fvar, "groups")
              groups$groups <- trimws(groups$groups)
            } else if (input$multiTest == "games") {
              mc <- PMCMRplus::gamesHowellTest(stats::as.formula(paste0("`", var, "` ~ `", fvar, "`")), data = df)
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
                mc <- PMCMRplus::kwAllPairsDunnTest(df_var[[var]], df_var[[fvar]], p.adjust.method = np_adj)
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
            dplyr::group_by(dplyr::across(dplyr::all_of(fvar))) %>%
            dplyr::summarise(
              Moyenne    = mean(.data[[var]], na.rm = TRUE),
              Ecart_type = stats::sd(.data[[var]], na.rm = TRUE),
              N          = dplyr::n(),
              Erreur_type = ifelse(N > 1, Ecart_type / sqrt(N), NA_real_),
              CV         = calc_cv(.data[[var]]),
              .groups    = "drop"
            )
          
          if (fvar %in% colnames(groups) && fvar %in% colnames(desc)) {
            res <- merge(desc, groups, by = fvar, all.x = TRUE)
            res <- res %>%
              dplyr::mutate(
                # LES COLONNES NUMERIQUES GARDENT LEUR PRECISION. Les arrondir
                # ici DETRUISAIT la valeur : le reglage « Si decoche, les
                # valeurs s'affichent sans arrondi » ne pouvait plus rien
                # restituer, et l'export comme le rapport ne portaient plus
                # qu'une moyenne a deux decimales. L'arrondi appartient a
                # l'affichage (`round_numeric_df`), pas au calcul.
                `Moyenne±Ecart_type` = .hstat_pm(Moyenne, Ecart_type, groups, .dec_aff),
                `Moyenne±Erreur_type` = .hstat_pm(Moyenne, Erreur_type, groups, .dec_aff),
                Variable = var,
                Facteur = fvar,
                Type = "main"
              )
            
            multi_results_list[[paste(var, fvar, "main", sep = "_")]] <- res
          }
        }, error = function(e) {
          shiny::showNotification(hstat_err_fr(e, sprintf("Effet principal %s x %s", var, fvar)),
                           type = "error", duration = 12)
        })
      }
      
      if (input$posthocInteraction && length(input$multiFactor) > 1) {
        factor_combinations <- utils::combn(input$multiFactor, 2, simplify = FALSE)
        
        for (fcomb in factor_combinations) {
          fvar1 <- fcomb[1]
          fvar2 <- fcomb[2]
          interaction_term <- paste(fvar1, fvar2, sep = ":")
          formula_str <- paste(var, "~", fvar1, "*", fvar2)
          
          tryCatch({
            cols_inter <- c(var, fvar1, fvar2)
            df_temp <- df[, intersect(cols_inter, names(df)), drop = FALSE]
            df_temp <- df_temp[stats::complete.cases(df_temp), ]
            for (.f in c(fvar1, fvar2)) {
              if (!is.factor(df_temp[[.f]])) df_temp[[.f]] <- factor(df_temp[[.f]])
              df_temp[[.f]] <- droplevels(df_temp[[.f]])
            }
            if (!is.numeric(df_temp[[var]])) df_temp[[var]] <- suppressWarnings(as.numeric(df_temp[[var]]))
            if (nrow(df_temp) < 4 || all(is.na(df_temp[[var]]))) {
              shiny::showNotification(trf("Interaction %s:%s -- données insuffisantes.", fvar1, fvar2), type="warning", duration=4)
              return(NULL)
            }
            interaction_pvalue <- NA
            
            if (input$testType == "param") {
              formula_inter <- paste0("`", var, "` ~ `", fvar1, "` * `", fvar2, "`")
              model <- tryCatch(stats::aov(stats::as.formula(formula_inter), data = df_temp), error = function(e) NULL)
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
              
              kw_interaction <- stats::kruskal.test(df_temp[[var]] ~ df_temp$interaction_combined)
              interaction_pvalue <- kw_interaction$p.value
              
              shiny::showNotification(
                trf("Test non-paramétrique (Kruskal-Wallis) pour %s x %s: p = %s", fvar1, fvar2, round(interaction_pvalue, 4)),
                type = "message", duration = 3
              )
            }
            
            # SI INTERACTION SIGNIFICATIVE : DÉCOMPOSITION BIDIRECTIONNELLE 
            if (!is.na(interaction_pvalue) && interaction_pvalue < 0.05) {
              shiny::showNotification(
                paste0("[OK] Interaction significative détectée : ", fvar1, " x ", fvar2, 
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
                                    dplyr::mutate(
                                      Moyenne = as.numeric(Moyenne),
                                      Ecart_type = as.numeric(Ecart_type),
                                      Erreur_type = as.numeric(Erreur_type),
                                      CV = as.numeric(CV),
                                      `Moyenne±Ecart_type` = .hstat_pm(Moyenne, Ecart_type, groups, .dec_aff),
                                      `Moyenne±Erreur_type` = .hstat_pm(Moyenne, Erreur_type, groups, .dec_aff),
                                      Variable = var,
                                      Facteur = paste0(fvar1, " | ", fvar2, "=", level2),
                                      Type = "simple_effect",
                                      Interaction_base = interaction_term,
                                      # PAS D'ARRONDI AU CALCUL : le choix
                                      # appartient a l'utilisateur, et
                                      # l'affichage l'applique deja.
                                      P_interaction = interaction_pvalue,
                                      Direction = trf("%s vers %s", fvar1, fvar2)
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
                                    dplyr::mutate(
                                      Moyenne = as.numeric(Moyenne),
                                      Ecart_type = as.numeric(Ecart_type),
                                      Erreur_type = as.numeric(Erreur_type),
                                      CV = as.numeric(CV),
                                      `Moyenne±Ecart_type` = .hstat_pm(Moyenne, Ecart_type, groups, .dec_aff),
                                      `Moyenne±Erreur_type` = .hstat_pm(Moyenne, Erreur_type, groups, .dec_aff),
                                      Variable = var,
                                      Facteur = paste0(fvar2, " | ", fvar1, "=", level1),
                                      Type = "simple_effect",
                                      Interaction_base = interaction_term,
                                      # PAS D'ARRONDI AU CALCUL : le choix
                                      # appartient a l'utilisateur, et
                                      # l'affichage l'applique deja.
                                      P_interaction = interaction_pvalue,
                                      Direction = trf("%s vers %s", fvar2, fvar1)
                                    ), error = function(e) NULL)
                  if (!is.null(res))
                    simple_effects_list[[paste(var, fvar2, fvar1, level1, sep = "_")]] <- res
                }
              }
              
              shiny::showNotification(
                trf("[OK] Décomposition complétée pour %s x %s", fvar1, fvar2),
                type = "message", duration = 3
              )
            } else if (!is.na(interaction_pvalue)) {
              shiny::showNotification(
                paste0("[--] Interaction non significative : ", fvar1, " x ", fvar2, 
                       " (p = ", round(interaction_pvalue, 4), ")"),
                type = "default", duration = 3
              )
            }
          }, error = function(e) {
            shiny::showNotification(hstat_err_fr(e, paste("Interaction", interaction_term)),
                             type = "error", duration = 12)
          })
        }
      }
    }
    
    shiny::removeNotification("loading")
    
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
              for (col in c("Moyenne", "Erreur_type", "Ecart_type")) {
                if (col %in% names(combined_results)) {
                  vals_orig <- as.numeric(combined_results[rows_v, col])
                  combined_results[rows_v, col] <- round(
                    back_transform_values(
                      x = vals_orig, method = entry$method,
                      lambda = entry$lambda, yj_object = entry$yj_object
                    ), 4)
                }
              }
              if (all(c("Moyenne","Ecart_type","Erreur_type","groups") %in% names(combined_results))) {
                combined_results[rows_v, "Moyenne±Ecart_type"]  <- paste0(
                  combined_results[rows_v,"Moyenne"], "±",
                  combined_results[rows_v,"Ecart_type"], " ",
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
      
      shiny::showNotification(
        shiny::HTML(paste0(
          "<b>[OK] ANALYSE TERMINÉE</b><br/>",
          "- ", n_main, " effet(s) principal(aux)<br/>",
          "- ", n_simple, " effet(s) simple(s)<br/>",
          "- ", n_interactions, " interaction(s) décomposée(s)"
        )),
        type = "message", duration = 8
      )
    } else {
      shiny::showNotification("Aucun résultat généré", type = "warning")
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
          shiny::showNotification(hstat_err_fr(e, trf("Post-hoc multivarié (facteur %s)", fvar)),
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
  
  shiny::observeEvent(input$runLMPostHoc, {
    shiny::req(values$modelList)
    if (length(values$modelList) == 0) {
      shiny::showNotification("Aucun modèle LM ou GLM à analyser.", type = "warning")
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
      shiny::showNotification(paste0("Aucun prédicteur catégoriel détecté dans le(s) modèle(s) ",
                              "(les variables doivent être de type factor)."),
                       type = "warning", duration = 6)
      values$lmPostHocResults <- NULL
      return()
    }
    values$lmPostHocResults <- results
    shiny::showNotification(
      trf("PostHoc LM/GLM calculé : %s combinaison(s) Variable × Prédicteur.", length(results)),
      type = "message", duration = 4
    )
  })
  
  output$hasLMPostHoc <- shiny::reactive({
    !is.null(values$lmPostHocResults) && length(values$lmPostHocResults) > 0
  })
  shiny::outputOptions(output, "hasLMPostHoc", suspendWhenHidden = FALSE)
  
  output$hasLMModel <- shiny::reactive({
    !is.null(values$modelList) && length(values$modelList) > 0
  })
  shiny::outputOptions(output, "hasLMModel", suspendWhenHidden = FALSE)
  
  output$lmPostHocSelector <- shiny::renderUI({
    shiny::req(values$lmPostHocResults)
    combos <- names(values$lmPostHocResults)
    labels <- vapply(values$lmPostHocResults, function(r)
      paste0(r$model_type, " : ", r$variable, " ~ ", r$predictor), character(1))
    named_choices <- stats::setNames(combos, labels)
    shiny::selectInput(ns("lmPostHocCombo"),
                shiny::tagList(shiny::icon("filter"), " Choisir une combinaison Variable / Prédicteur :"),
                choices = named_choices, selected = combos[1], width = "100%")
  })
  
  output$lmPostHocLettersTable <- DT::renderDT({
    shiny::req(values$lmPostHocResults, input$lmPostHocCombo)
    entry <- values$lmPostHocResults[[input$lmPostHocCombo]]
    shiny::req(entry$letters)
    df <- entry$letters
    if ("Moyenne_pm_SD" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SD"] <- "Moyenne \u00b1 Écart-type groupe"
    if ("Moyenne_pm_SE" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SE"] <- "Moyenne \u00b1 Erreur-type groupe"
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    dt <- DT::datatable(df, options = list(scrollX = TRUE, pageLength = 15, dom = "tip"),
                    rownames = FALSE)
    if ("Groupes" %in% names(df))
      dt <- color_groups_dt(dt, df, "Groupes")
    dt
  })
  
  output$lmPostHocPairsTable <- DT::renderDT({
    shiny::req(values$lmPostHocResults, input$lmPostHocCombo)
    entry <- values$lmPostHocResults[[input$lmPostHocCombo]]
    shiny::req(entry$pairs)
    df <- entry$pairs
    for (col in c("p_value", "p_adj")) {
      if (col %in% names(df)) df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    dt <- DT::datatable(df, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
    if ("Significatif" %in% names(df))
      dt <- dt %>% DT::formatStyle("Significatif",
                               backgroundColor = DT::styleEqual(c("Oui","Non"), c("#ffebee","#f1f8e9")),
                               fontWeight = "bold")
    dt
  })
  
  output$lmPostHocInfo <- shiny::renderUI({
    shiny::req(values$lmPostHocResults, input$lmPostHocCombo)
    entry <- values$lmPostHocResults[[input$lmPostHocCombo]]
    n_pairs <- if (is.null(entry$pairs)) 0 else nrow(entry$pairs)
    n_sig   <- if (is.null(entry$pairs)) 0 else sum(entry$pairs$p_adj < 0.05, na.rm = TRUE)
    n_lev   <- if (is.null(entry$letters)) 0 else nrow(entry$letters)
    shiny::div(style = "background:#e3f2fd; border-left:4px solid #1565C0; padding:10px 14px; border-radius:6px; margin-bottom:12px; font-size:12px;",
        shiny::icon("info-circle", style = "color:#1565C0;"),
        shiny::strong(paste0(" PostHoc ", entry$model_type, " : ", entry$variable, " ~ ", entry$predictor)),
        shiny::tags$ul(style = "margin:4px 0 0 18px;",
                shiny::tags$li("Méthode : moyennes ajustées (emmeans) sur le prédicteur catégoriel"),
                shiny::tags$li(trf("Ajustement des p-values : %s", entry$adjust)),
                shiny::tags$li(trf("Niveaux comparés : %s -- Paires : %s -- Paires significatives : %s", n_lev, n_pairs, n_sig))
        ),
        "Pour les modèles GLM non gaussiens, les comparaisons sont sur l'échelle du lien (logit, log...)."
    )
  })
  
  output$downloadLMPostHoc <- hstat_classeur_handler(function() {
    # LES NOMS DE FEUILLE VIENNENT DES VARIABLES DE L'UTILISATEUR : tronques a
    # 31 caracteres mais jamais nettoyes, ils faisaient LEVER `addWorksheet()`
    # des qu'une variable portait un caractere interdit par Excel ([]:*?/\).
    # `hstat_feuille_nom()`, applique par l'ecrivain, s'en charge.
    tb <- list()
    for (key in names(values$lmPostHocResults)) {
      e <- values$lmPostHocResults[[key]]
      tb[[paste0("Lettres_", e$variable, "_", e$predictor)]] <- e$letters
      tb[[paste0("Paires_",  e$variable, "_", e$predictor)]] <- e$pairs
    }
    hstat_tables_non_vides(tb)
  }, "PostHoc_LM_GLM", "Comparaisons post-hoc")
  
  
  output$hasMultivariatePosthoc <- shiny::reactive({
    !is.null(values$manovaMultiPostHoc) && length(values$manovaMultiPostHoc) > 0
  })
  shiny::outputOptions(output, "hasMultivariatePosthoc", suspendWhenHidden = FALSE)
  
  # --- PostHoc Mesures repetees : flag, info, tableau, telechargement ---
  output$hasRMPostHoc <- shiny::reactive({
    !is.null(values$rmPostHocData) && nrow(values$rmPostHocData) > 0
  })
  shiny::outputOptions(output, "hasRMPostHoc", suspendWhenHidden = FALSE)
  
  output$rmPostHocInfo <- shiny::renderUI({
    shiny::req(values$rmPostHocData)
    meth <- values$rmPostHocMethod %||% "—"
    n_sig <- sum(values$rmPostHocData$Significatif == "Oui", na.rm = TRUE)
    shiny::div(style = "font-size:12px; color:#00695c; margin-bottom:8px;",
        shiny::tags$b("Méthode : "), meth, " — ",
        shiny::tags$b(n_sig), " comparaison(s) significative(s) sur ", nrow(values$rmPostHocData), ".")
  })
  
  output$rmPostHocTable <- DT::renderDT({
    shiny::req(values$rmPostHocData)
    d <- values$rmPostHocData
    # Retirer la colonne Estimation si entierement vide (cas non parametrique).
    if (all(is.na(d$Estimation))) d$Estimation <- NULL
    DT::datatable(d, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatStyle("Significatif",
                  backgroundColor = DT::styleEqual(c("Oui", "n.s."), c("#c8e6c9", "#fff")),
                  fontWeight = DT::styleEqual("Oui", "bold"))
  })
  
  output$downloadRMPostHoc <- shiny::downloadHandler(
    filename = function() paste0("posthoc_mesures_repetees_", Sys.Date(), ".xlsx"),
    content = function(file) {
      shiny::req(values$rmPostHocData)
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        openxlsx::write.xlsx(values$rmPostHocData, file)
      } else {
        utils::write.csv(values$rmPostHocData, sub("\\.xlsx$", ".csv", file), row.names = FALSE)
      }
    }
  )
  
  # Selecteur de facteur (dynamique selon les facteurs disponibles dans manovaMultiPostHoc)
  output$multivariatePosthocFactorSelect <- shiny::renderUI({
    shiny::req(values$manovaMultiPostHoc)
    fcts <- names(values$manovaMultiPostHoc)
    if (length(fcts) == 0) return(NULL)
    shiny::selectInput(ns("multivariatePosthocFactor"),
                label = NULL,
                choices = fcts,
                selected = fcts[1],
                width = "100%")
  })
  
  output$multivariatePosthocInfo <- shiny::renderUI({
    shiny::req(values$manovaMultiPostHoc, input$multivariatePosthocFactor)
    entry <- values$manovaMultiPostHoc[[input$multivariatePosthocFactor]]
    if (is.null(entry)) return(NULL)
    
    n_sig <- sum(entry$pairs$p_adj < 0.05, na.rm = TRUE)
    n_pairs <- nrow(entry$pairs)
    
    shiny::div(style = "background:#e8f5e9; border-left:4px solid #43a047; padding:10px 14px; border-radius:6px; margin-bottom:12px; font-size:12px;",
        shiny::icon("info-circle", style = "color:#2e7d32;"),
        shiny::strong(trf(" PostHoc multivarié -- %s sur %s :", entry$test, entry$response_label)),
        shiny::tags$ul(style = "margin:4px 0 0 18px;",
                shiny::tags$li("Méthode : pairwise PERMANOVA (vegan::adonis2), distance euclidienne, 999 permutations"),
                shiny::tags$li("Ajustement des p-values : Bonferroni"),
                shiny::tags$li("Lettres CLD générées par multcompView::multcompLetters sur la matrice de p-values ajustées"),
                shiny::tags$li(trf("Niveaux comparés : %s -- Paires : %s -- Paires significatives : %s", entry$n_levels, n_pairs, n_sig))
        ),
        "Interprétation : deux niveaux partageant une même lettre ne diffèrent pas significativement sur le vecteur de réponses multivariées."
    )
  })
  
  output$multivariatePosthocLettersTable <- DT::renderDT({
    shiny::req(values$manovaMultiPostHoc, input$multivariatePosthocFactor)
    entry <- values$manovaMultiPostHoc[[input$multivariatePosthocFactor]]
    shiny::req(entry$letters)
    
    df <- entry$letters
    names(df)[names(df) == "Niveau"] <- input$multivariatePosthocFactor
    if ("Moyenne_pm_SD" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SD"] <- "Moyenne \u00b1 Écart-type groupe"
    if ("Moyenne_pm_SE" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SE"] <- "Moyenne \u00b1 Erreur-type groupe"
    
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    
    dt <- DT::datatable(df,
                    options = list(pageLength = 25, scrollX = TRUE, dom = "tip"),
                    rownames = FALSE)
    dt <- color_groups_dt(dt, df, "Groupes")
    if ("Variable" %in% names(df))
      dt <- dt %>% DT::formatStyle("Variable", fontWeight = "bold",
                               backgroundColor = "#e3f2fd")
    dt
  })
  
  output$multivariatePosthocPairsTable <- DT::renderDT({
    shiny::req(values$manovaMultiPostHoc, input$multivariatePosthocFactor)
    entry <- values$manovaMultiPostHoc[[input$multivariatePosthocFactor]]
    shiny::req(entry$pairs)
    
    df <- entry$pairs
    for (col in c("p_value", "p_adj")) {
      if (col %in% names(df))
        df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    
    dt <- DT::datatable(df,
                    options = list(pageLength = 25, scrollX = TRUE),
                    rownames = FALSE)
    
    if ("Significatif" %in% names(df)) {
      dt <- dt %>% DT::formatStyle("Significatif",
                               backgroundColor = DT::styleEqual(c("Oui", "Non"),
                                                            c("#ffebee", "#f1f8e9")),
                               fontWeight = "bold")
    }
    dt
  })
  
  # Telechargement Excel multi-feuilles (1 feuille de lettres + 1 feuille de paires par facteur)
  output$downloadMultivariatePosthoc <- hstat_classeur_handler(function() {
    tb <- list()
    for (fn in names(values$manovaMultiPostHoc)) {
      e <- values$manovaMultiPostHoc[[fn]]
      lettres <- e$letters
      names(lettres)[names(lettres) == "Niveau"] <- fn
      tb[[paste0("Lettres_", fn)]] <- lettres
      tb[[paste0("Paires_",  fn)]] <- e$pairs
    }
    hstat_tables_non_vides(tb)
  }, "PostHoc_MANOVA_multivarie", "Post-hoc MANOVA")
  
  output$hasManovaInteractionPostHoc <- shiny::reactive({
    !is.null(values$manovaInteractionPostHoc) &&
      !is.null(values$manovaInteractionPostHoc$letters)
  })
  shiny::outputOptions(output, "hasManovaInteractionPostHoc", suspendWhenHidden = FALSE)
  
  output$manovaInteractionPostHocInfo <- shiny::renderUI({
    shiny::req(values$manovaInteractionPostHoc)
    ent <- values$manovaInteractionPostHoc
    shiny::div(style = "background:#fff3e0; border-left:4px solid #fb8c00; padding:10px 14px; border-radius:6px; margin-bottom:12px; font-size:12px;",
        shiny::icon("project-diagram", style = "color:#e65100;"),
        shiny::strong(" Comparaison des cellules d'interaction : "),
        "chaque cellule combine un niveau de ", shiny::strong(paste(ent$factors, collapse = " et de ")),
        ". Les lettres comparent simultanément l'effet du facteur fixé et du facteur évalué. ",
        "Deux cellules partageant une lettre ne diffèrent pas significativement (alpha = 0.05)."
    )
  })
  
  output$manovaInteractionLettersTable <- DT::renderDT({
    shiny::req(values$manovaInteractionPostHoc)
    df <- values$manovaInteractionPostHoc$letters
    if ("Moyenne_pm_SD" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SD"] <- "Moyenne \u00b1 Écart-type groupe"
    if ("Moyenne_pm_SE" %in% names(df))
      names(df)[names(df) == "Moyenne_pm_SE"] <- "Moyenne \u00b1 Erreur-type groupe"
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    dt <- DT::datatable(df, options = list(pageLength = 25, scrollX = TRUE, dom = "tip"),
                    rownames = FALSE)
    dt <- color_groups_dt(dt, df, "Groupes")
    if ("Variable" %in% names(df))
      dt <- dt %>% DT::formatStyle("Variable", fontWeight = "bold", backgroundColor = "#e3f2fd")
    dt
  })
  
  output$manovaInteractionPairsTable <- DT::renderDT({
    shiny::req(values$manovaInteractionPostHoc)
    df <- values$manovaInteractionPostHoc$pairs
    if (is.null(df))
      return(DT::datatable(data.frame(Information = "Comparaisons par paires indisponibles."),
                       options = list(dom = "t"), rownames = FALSE))
    for (col in c("p_value", "p_adj")) {
      if (col %in% names(df))
        df[[col]] <- sapply(df[[col]], function(p) if (is.na(p)) NA else fmt_p(p))
    }
    df <- round_numeric_df(df, input$multiRoundResults, input$multiDecimals)
    dt <- DT::datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
    if ("Significatif" %in% names(df))
      dt <- dt %>% DT::formatStyle("Significatif",
                               backgroundColor = DT::styleEqual(c("Oui", "Non"),
                                                            c("#ffebee", "#f1f8e9")),
                               fontWeight = "bold")
    dt
  })
  
  output$multiResponseSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    
    # Pré-sélection initiale : variables choisies dans "Paramètres des tests",
    # sinon repli sur les variables des résultats de tests.
    pre_selected <- if (!is.null(shiny::isolate(input$responseVar)) &&
                        length(shiny::isolate(input$responseVar)) > 0) {
      shiny::isolate(input$responseVar)
    } else if (!is.null(values$testResultsDF) && "Variable" %in% names(values$testResultsDF)) {
      sig_vars <- unique(values$testResultsDF$Variable[
        !is.na(values$testResultsDF$p_value) & values$testResultsDF$p_value < 0.05
      ])
      if (length(sig_vars) == 0) unique(values$testResultsDF$Variable) else sig_vars
    } else { character(0) }
    pre_selected <- intersect(pre_selected, num_cols)
    
    shiny::tagList(
      pickerInput(ns("multiResponse"), "Variable(s) réponse :",
                  choices  = num_cols,
                  selected = if (length(pre_selected) > 0) pre_selected else NULL,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      shiny::div(style = "display: flex; gap: 10px;",
          shiny::actionButton(ns("selectAllMultiResponse"), "Tout sélectionner",
                       class = "btn-success btn-sm", style = "flex: 1; height: 40px;"),
          shiny::actionButton(ns("deselectAllMultiResponse"), "Tout désélectionner",
                       class = "btn-danger btn-sm", style = "flex: 1; height: 40px;")
      )
    )
  })
  
  # Synchronise multiResponse avec les variables choisies dans "Paramètres des tests"
  shiny::observeEvent(input$responseVar, {
    shiny::req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    sel <- intersect(input$responseVar, num_cols)
    if (length(sel) > 0)
      updatePickerInput(session, "multiResponse", selected = sel)
  }, ignoreNULL = TRUE)
  
  shiny::observeEvent(input$selectAllMultiResponse, {
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    updatePickerInput(session, "multiResponse", selected = num_cols)
  })
  
  shiny::observeEvent(input$deselectAllMultiResponse, {
    updatePickerInput(session, "multiResponse", selected = character(0))
  })
  
  output$multiFactorSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    fac_cols <- get_all_factor_candidates(values$filteredData)
    
    # Pré-sélection initiale : facteurs choisis dans "Paramètres des tests",
    # sinon repli sur les facteurs des résultats de tests.
    pre_fac <- if (!is.null(shiny::isolate(input$factorVar)) &&
                   length(shiny::isolate(input$factorVar)) > 0) {
      shiny::isolate(input$factorVar)
    } else if (!is.null(values$testResultsDF) && "Facteur" %in% names(values$testResultsDF)) {
      unique(values$testResultsDF$Facteur[!is.na(values$testResultsDF$Facteur)])
    } else { character(0) }
    pre_fac <- intersect(pre_fac, fac_cols)
    
    shiny::tagList(
      pickerInput(ns("multiFactor"), "Facteur(s) :",
                  choices  = fac_cols,
                  selected = if (length(pre_fac) > 0) pre_fac else NULL,
                  multiple = TRUE,
                  options  = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")),
      shiny::tags$small(style = "color:#6c757d; font-size:11px;",
                 shiny::icon("info-circle"), " Facteur, texte, date et numérique (<= 30 niveaux) acceptés"),
      shiny::div(style = "display: flex; gap: 10px;",
          shiny::actionButton(ns("selectAllMultiFactors"),   "Tout sélectionner",
                       class = "btn-success btn-sm", style = "flex: 1; height: 40px;"),
          shiny::actionButton(ns("deselectAllMultiFactors"), "Tout désélectionner",
                       class = "btn-danger btn-sm",  style = "flex: 1; height: 40px;")
      )
    )
  })
  
  # Synchronise multiFactor avec les facteurs choisis dans "Paramètres des tests"
  shiny::observeEvent(input$factorVar, {
    shiny::req(values$filteredData)
    fac_cols <- get_all_factor_candidates(values$filteredData)
    sel <- intersect(input$factorVar, fac_cols)
    if (length(sel) > 0)
      updatePickerInput(session, "multiFactor", selected = sel)
  }, ignoreNULL = TRUE)
  
  shiny::observeEvent(input$selectAllMultiFactors, {
    updatePickerInput(session, "multiFactor", selected = get_all_factor_candidates(values$filteredData))
  })
  
  shiny::observeEvent(input$deselectAllMultiFactors, {
    updatePickerInput(session, "multiFactor", selected = character(0))
  })
  
  # - Bloc 8 : Info transformations dans le panel PostHoc -
  output$postHocTransformInfo <- shiny::renderUI({
    log           <- values$transformationLog %||% list()
    selected_vars <- input$multiResponse
    if (length(log) == 0 || is.null(selected_vars) || length(selected_vars) == 0) return(NULL)
    trans_selected <- intersect(selected_vars, names(log))
    if (length(trans_selected) == 0) return(NULL)
    entries <- lapply(trans_selected, function(vname) {
      entry       <- log[[vname]]
      lambda_info <- if (!is.null(entry$lambda)) paste0(" (λ = ", entry$lambda, ")") else ""
      shiny::tags$li(
        style = "font-size:11.5px;margin-bottom:4px;line-height:1.5;",
        shiny::tags$b(style = "color:#1565c0;", vname),
        shiny::tags$span(style = "color:#555;", " ← "),
        shiny::tags$b(entry$original),
        shiny::tags$code(
          style = paste0("font-size:10.5px;background:#e3f2fd;padding:1px 4px;",
                         "border-radius:3px;color:#0d47a1;margin-left:4px;"),
          paste0(entry$formula, lambda_info)
        )
      )
    })
    shiny::div(
      style = paste0("padding:10px 12px;background:#e3f2fd;",
                     "border-left:4px solid #1976d2;border-radius:4px;margin-bottom:10px;"),
      shiny::div(style = "font-weight:bold;color:#0d47a1;font-size:12px;margin-bottom:6px;",
          shiny::icon("flask"), " Variables transformées sélectionnées"),
      shiny::tags$ul(style = "margin:0;padding-left:16px;", shiny::tagList(entries)),
      shiny::div(
        style = paste0("font-size:11px;color:#1565c0;margin-top:8px;",
                       "padding-top:6px;border-top:1px solid #90caf9;font-style:italic;"),
        shiny::icon("info-circle"),
        " Le PostHoc est réalisé sur les données transformées.",
        " Les lettres de significativité s'appliquent à l'échelle transformée.",
        " Activez 'Retro-transformation' pour afficher les moyennes sur l'échelle originale."
      )
    )
  })
  
  output$hasTransformedVarsSelected <- shiny::reactive({
    log      <- values$transformationLog %||% list()
    selected <- input$multiResponse
    if (length(log) == 0 || is.null(selected)) return(FALSE)
    any(selected %in% names(log))
  })
  shiny::outputOptions(output, "hasTransformedVarsSelected", suspendWhenHidden = FALSE) 
  output$mainEffectsTable <- DT::renderDT({
    shiny::req(values$multiResultsMain)
    
    main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
    
    if (nrow(main_data) == 0) return(NULL)
    
    cols_to_show <- c("Variable", "Facteur", "Moyenne", "Ecart_type", "Erreur_type", "CV", "groups", "N", "Moyenne±Ecart_type", "Moyenne±Erreur_type")
    
    for (fvar in input$multiFactor) {
      if (fvar %in% colnames(main_data)) {
        cols_to_show <- c(cols_to_show, fvar)
      }
    }
    
    cols_to_show <- unique(cols_to_show)
    cols_to_show <- cols_to_show[cols_to_show %in% colnames(main_data)]
    
    dt <- DT::datatable(
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
      dec <- hstat_dec_affichage(TRUE, input$multiDecimals)
      round_cols <- intersect(c("Moyenne", "Ecart_type", "Erreur_type", "CV"),
                              cols_to_show)
      if (length(round_cols) > 0)
        dt <- dt %>% DT::formatRound(columns = round_cols, digits = dec)
    }
    dt %>%
      DT::formatStyle(
        'groups',
        backgroundColor = DT::styleEqual(
          unique(main_data$groups),
          grDevices::rainbow(length(unique(main_data$groups)), alpha = 0.3)
        ),
        fontWeight = 'bold'
      )
  })
  
  output$simpleEffectsTable <- DT::renderDT({
    shiny::req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    if (!is.null(input$filterSimpleEffectVar) && input$filterSimpleEffectVar != "Toutes") {
      simple_data <- simple_data[simple_data$Variable == input$filterSimpleEffectVar, ]
    }
    
    if (!is.null(input$filterSimpleEffectInteraction) && input$filterSimpleEffectInteraction != "Toutes") {
      simple_data <- simple_data[simple_data$Interaction_base == input$filterSimpleEffectInteraction, ]
    }
    
    cols_to_show <- c("Variable", "Facteur", "Direction", "Interaction_base", "P_interaction", 
                      "Moyenne", "Ecart_type", "Erreur_type", "CV", "groups", "N", "Moyenne±Ecart_type", "Moyenne±Erreur_type")
    
    for (fvar in input$multiFactor) {
      if (fvar %in% colnames(simple_data)) {
        cols_to_show <- c(cols_to_show, fvar)
      }
    }
    
    cols_to_show <- unique(cols_to_show)
    cols_to_show <- cols_to_show[cols_to_show %in% colnames(simple_data)]
    
    dt <- DT::datatable(
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
      dec <- hstat_dec_affichage(TRUE, input$multiDecimals)
      round_cols <- intersect(c("Moyenne", "Ecart_type", "Erreur_type", "CV", "P_interaction"),
                              cols_to_show)
      if (length(round_cols) > 0)
        dt <- dt %>% DT::formatRound(columns = round_cols, digits = dec)
    }
    dt %>%
      DT::formatStyle(
        'groups',
        backgroundColor = DT::styleEqual(
          unique(simple_data$groups),
          grDevices::rainbow(length(unique(simple_data$groups)), alpha = 0.3)
        ),
        fontWeight = 'bold'
      ) %>%
      DT::formatStyle(
        'P_interaction',
        backgroundColor = DT::styleInterval(c(0.01, 0.05), c('#e74c3c', '#f39c12', '#95a5a6')),
        color = 'white',
        fontWeight = 'bold'
      )
  })
  
  output$showPosthocResults <- shiny::reactive({
    !is.null(values$multiResultsMain) && nrow(values$multiResultsMain) > 0
  })
  shiny::outputOptions(output, "showPosthocResults", suspendWhenHidden = FALSE)
  
  output$showSimpleEffects <- shiny::reactive({
    !is.null(values$multiResultsMain) && 
      any(values$multiResultsMain$Type == "simple_effect", na.rm = TRUE)
  })
  shiny::outputOptions(output, "showSimpleEffects", suspendWhenHidden = FALSE)
  
  output$analysisSummaryMain <- shiny::renderUI({
    shiny::req(values$multiResultsMain)
    
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

    shiny::div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px; border-radius: 8px;",
        shiny::h5(shiny::icon("layer-group"), " Effets principaux", style = "margin-top: 0;"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong(n_vars), " variable(s) analysée(s)"),
          shiny::tags$li(shiny::strong(n_factors), " facteur(s) testé(s)"),
          shiny::tags$li(shiny::strong(n_comparisons), " comparaison(s)"),
          shiny::tags$li("Méthode : ", shiny::strong(meth_lab)),
          shiny::tags$li("Ajustement des p-values : ",
                  shiny::strong(if (adj_applies) adj_lab else "intégré à la méthode"))
        )
    )
  })
  
  output$simpleEffectsSummary <- shiny::renderUI({
    shiny::req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    n_interactions <- length(unique(simple_data$Interaction_base))
    n_tests <- nrow(simple_data)
    n_directions <- length(unique(simple_data$Direction[!is.na(simple_data$Direction)]))
    
    shiny::div(style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
        shiny::h5(shiny::icon("project-diagram"), " Décomposition des interactions", style = "margin-top: 0;"),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong(n_interactions), " interaction(s) significative(s)"),
          shiny::tags$li(shiny::strong(n_tests), " test(s) d'effets simples"),
          shiny::tags$li(shiny::strong(n_directions), " direction(s) d'analyse")
        )
    )
  })
  
  shiny::observe({
    shiny::req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) > 0) {
      vars <- c("Toutes", unique(simple_data$Variable))
      interactions <- c("Toutes", unique(simple_data$Interaction_base))
      
      shiny::updateSelectInput(session, "filterSimpleEffectVar", choices = vars, selected = "Toutes")
      shiny::updateSelectInput(session, "filterSimpleEffectInteraction", choices = interactions, selected = "Toutes")
    }
  })
  
  output$selectSimpleEffectPlot <- shiny::renderUI({
    shiny::req(values$multiResultsMain)
    
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    if (nrow(simple_data) == 0) return(NULL)
    
    if (is.null(values$currentVarIndex)) {
      values$currentVarIndex <- 1
    }
    
    if (is.null(input$multiResponse) || length(input$multiResponse) == 0) {
      return(shiny::div(style = "color: #e74c3c; font-style: italic;", 
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
      return(shiny::div(style = "color: #e74c3c; font-style: italic;", 
                 "Erreur d'accès à la variable"))
    }
    
    simple_var_data <- simple_data[simple_data$Variable == resp_var, ]
    
    if (nrow(simple_var_data) == 0) {
      return(shiny::div(style = "color: #e74c3c; font-style: italic;", 
                 trf("Aucun effet simple pour %s", resp_var)))
    }
    
    factors <- unique(simple_var_data$Facteur)
    
    if (length(factors) == 0) {
      return(shiny::div(style = "color: #e74c3c; font-style: italic;", 
                 "Aucun facteur disponible"))
    }
    
    shiny::selectInput(ns("selectedSimpleEffect"), 
                "Sélectionner l'effet simple :",
                choices = factors,
                width = "100%")
  })
  
  output$fullAnalysisReport <- shiny::renderUI({
    shiny::req(values$multiResultsMain)
    
    main_data <- values$multiResultsMain[values$multiResultsMain$Type == "main", ]
    simple_data <- values$multiResultsMain[values$multiResultsMain$Type == "simple_effect", ]
    
    shiny::tagList(
      shiny::h4("Vue d'ensemble"),
      shiny::fluidRow(
        shiny::column(6,
               shiny::div(style = "background-color: white; padding: 15px; border-radius: 5px; border-left: 4px solid #667eea;",
                   shiny::h5(shiny::icon("layer-group"), " Effets principaux"),
                   shiny::p(shiny::strong(nrow(main_data)), " comparaisons"),
                   shiny::p(shiny::strong(length(unique(main_data$Variable))), " variables"),
                   shiny::p(shiny::strong(length(unique(main_data$Facteur))), " facteurs")
               )
        ),
        shiny::column(6,
               shiny::div(style = "background-color: white; padding: 15px; border-radius: 5px; border-left: 4px solid #e74c3c;",
                   shiny::h5(shiny::icon("project-diagram"), " Effets simples"),
                   shiny::p(shiny::strong(nrow(simple_data)), " tests"),
                   shiny::p(shiny::strong(length(unique(simple_data$Interaction_base))), " interactions décomposées"),
                   shiny::p(shiny::strong(length(unique(simple_data$Direction[!is.na(simple_data$Direction)]))), " directions d'analyse")
               )
        )
      ),
      shiny::br(),
      shiny::h4("Détails par variable"),
      shiny::uiOutput(ns("variableDetailedReport"))
    )
  })
  
  output$variableDetailedReport <- shiny::renderUI({
    shiny::req(values$multiResultsMain)
    
    vars <- unique(values$multiResultsMain$Variable)
    
    reports <- lapply(vars, function(v) {
      var_data <- values$multiResultsMain[values$multiResultsMain$Variable == v, ]
      main_count <- sum(var_data$Type == "main")
      simple_count <- sum(var_data$Type == "simple_effect")
      
      shiny::div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          shiny::strong(v),
          shiny::br(),
          trf("- %d effet(s) principal(aux)", main_count),
          shiny::br(),
          sprintf("- %d effet(s) simple(s)", simple_count)
      )
    })
    
    do.call(shiny::tagList, reports)
  })
  
  output$showVariableNavigation <- shiny::reactive({
    length(input$multiResponse) > 1
  })
  shiny::outputOptions(output, "showVariableNavigation", suspendWhenHidden = FALSE)
  
  output$variableNavigation <- shiny::renderUI({
    shiny::req(input$multiResponse)
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
    
    shiny::div(style = "display: flex; align-items: center; gap: 15px;",
        shiny::actionButton(ns("prevMultiVar"), "", 
                     icon = shiny::icon("chevron-left"), 
                     class = "btn-light btn-lg",
                     style = "font-size: 1.5em; padding: 10px 20px; height: 60px; width: 60px;"),
        shiny::div(style = "color: white; font-size: 1.2em; font-weight: bold; text-align: center;",
            shiny::span(style = "display: block; font-size: 0.8em; opacity: 0.8;", 
                 paste("Variable", current_idx, "/", total_vars)),
            shiny::span(current_var_name)
        ),
        shiny::actionButton(ns("nextMultiVar"), "", 
                     icon = shiny::icon("chevron-right"), 
                     class = "btn-light btn-lg",
                     style = "font-size: 1.5em; padding: 10px 20px; height: 60px; width: 60px;")
    )
  })
  
  shiny::observeEvent(input$prevMultiVar, {
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
  
  shiny::observeEvent(input$nextMultiVar, {
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
  
  
  output$xAxisOrderUI <- shiny::renderUI({
    shiny::req(input$multiResponse, input$multiFactor, values$filteredData)
    
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
    
    shiny::tagList(
      shiny::h6("Ordre actuel des catégories :", style = "font-weight: bold; color: #27ae60;"),
      shiny::div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; border-radius: 5px; padding: 5px;",
          lapply(seq_along(current_levels), function(i) {
            level_name <- current_levels[i]
            shiny::div(style = "background-color: white; padding: 8px; margin-bottom: 5px; border-radius: 4px; border: 1px solid #e0e0e0;",
                shiny::fluidRow(
                  shiny::column(2, 
                         shiny::strong(paste0(i, ".")),
                         style = "text-align: center; padding-top: 5px;"
                  ),
                  shiny::column(6, 
                         shiny::span(level_name, style = "font-weight: bold; color: #2c3e50;")
                  ),
                  shiny::column(4,
                         shiny::div(style = "text-align: right;",
                             if (i > 1) {
                               shiny::actionButton(paste0("moveUp_", i), "", 
                                            icon = shiny::icon("arrow-up"),
                                            class = "btn-sm btn-primary",
                                            style = "margin-right: 5px; padding: 2px 8px;")
                             },
                             if (i < length(current_levels)) {
                               shiny::actionButton(paste0("moveDown_", i), "", 
                                            icon = shiny::icon("arrow-down"),
                                            class = "btn-sm btn-primary",
                                            style = "padding: 2px 8px;")
                             }
                         )
                  )
                )
            )
          })
      ),
      shiny::hr(),
      shiny::actionButton(ns("resetXOrder"), "Réinitialiser l'ordre", 
                   class = "btn-warning btn-sm", 
                   icon = shiny::icon("undo"),
                   style = "width: 100%;")
    )
  })
  
  
  shiny::observe({
    shiny::req(input$multiResponse, input$multiFactor, values$filteredData)
    
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
      shiny::observeEvent(input[[paste0("moveUp_", i)]], {
        if (i > 1) {
          new_order <- current_levels
          new_order[c(i-1, i)] <- new_order[c(i, i-1)]
          values$customXLevels(new_order)
        }
      }, ignoreInit = TRUE)
    })
    
    lapply(seq_along(current_levels), function(i) {
      shiny::observeEvent(input[[paste0("moveDown_", i)]], {
        if (i < length(current_levels)) {
          new_order <- current_levels
          new_order[c(i, i+1)] <- new_order[c(i+1, i)]
          values$customXLevels(new_order)
        }
      }, ignoreInit = TRUE)
    })
  })
  
  
  shiny::observeEvent(input$resetXOrder, {
    values$customXLevels(NULL)
    shiny::showNotification("Ordre des catégories réinitialisé", type = "message", duration = 2)
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

  create_posthoc_plot <- shiny::reactive({
    
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
    legend_key_size <- .hstat_num1(input$legendKeySize, 1.2)
    
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
    
    shiny::req(values$multiResultsMain, input$multiResponse, input$multiFactor, values$filteredData)
    
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
      missing <- setdiff(c(fvar, "Moyenne", "Ecart_type", "Erreur_type", "groups"), colnames(agg))
      posthoc_plot_msg(trf("Ce graphique ne peut pas être tracé : les résultats ne contiennent pas les colonnes attendues (%s). Cela arrive notamment pour les analyses multivariées (MANOVA / PERMANOVA), qui n'ont pas de moyennes par groupe à représenter : consultez les onglets de résultats correspondants.", paste(missing, collapse = ", ")))
      return(NULL)
    }
    # Colonnes secondaires optionnelles : on les cree vides si absentes pour ne pas
    # bloquer le trace d'une ANOVA valide qui n'aurait pas tout fourni.
    if (!"Ecart_type" %in% colnames(agg))  agg[["Ecart_type"]]  <- NA_real_
    if (!"Erreur_type" %in% colnames(agg)) agg[["Erreur_type"]] <- NA_real_
    if (!"groups" %in% colnames(agg))      agg[["groups"]]      <- ""
    if (!fvar %in% colnames(plot_data) || !resp_var %in% colnames(plot_data)) {
      posthoc_plot_msg("Les colonnes nécessaires sont introuvables dans les données filtrées.")
      return(NULL)
    }
    posthoc_plot_msg(NULL)
    
    
    # Theme de base choisi par l'utilisateur (viz_get_theme est le meme helper
    # que le module de visualisation : un theme ajoute la profite aux deux).
    base_theme <- viz_get_theme(input$posthocTheme %||% "minimal", base_size = 11) +
      ggplot2::theme(
        plot.title = ggtext::element_markdown(
          size = title_size, 
          face = title_font_style, 
          hjust = 0.5
        ),
        
        plot.subtitle = if (!is.null(custom_subtitle) && custom_subtitle != "") {
          ggplot2::element_text(
            size = if (!is.null(subtitle_size)) subtitle_size else 12,
            face = if (!is.null(subtitle_font_style)) subtitle_font_style else "italic",
            hjust = as.numeric(if (!is.null(subtitle_position)) subtitle_position else 0.5),
            color = "gray30",
            margin = ggplot2::margin(t = 5, b = 10)
          )
        } else {
          ggplot2::element_blank()
        },
        
        axis.title.x = hstat_axe_titre_lire(input, "posthoc", axis_title_size,
                                            axis_title_font_style, "x"),
        axis.title.y = hstat_axe_titre_lire(input, "posthoc", axis_title_size,
                                            axis_title_font_style, "y"),
        axis.text.x = if (rotate_labels) {
          ggplot2::element_text(
            angle = 45, 
            hjust = 1, 
            size = axis_text_size, 
            face = axis_text_x_font_style
          )
        } else {
          ggplot2::element_text(
            size = axis_text_size, 
            face = axis_text_x_font_style
          )
        },
        axis.text.y = ggplot2::element_text(
          size = axis_text_size, 
          face = axis_text_y_font_style
        ),
        legend.position = if (color_by_groups) "right" else "none",
        legend.title = ggtext::element_markdown(
          size = legend_title_size, 
          face = legend_title_font_style
        ),
        legend.text = ggplot2::element_text(
          size = legend_text_size, 
          face = legend_text_font_style
        ),
        # `legendKeySize` etait declare dans l'interface mais jamais lu : le
        # reglage existait a l'ecran et ne faisait rien.
        legend.key.height = ggplot2::unit(legend_key_size, "lines"),
        legend.key.width = ggplot2::unit(legend_key_size * 1.25, "lines"),
        legend.spacing.y = ggplot2::unit(legend_spacing, "lines"),  
        legend.key.spacing.y = ggplot2::unit(legend_spacing, "lines"),  
        legend.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
        legend.box.spacing = ggplot2::unit(0.5, "lines"),
        panel.grid.major = ggplot2::element_line(color = "gray90"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA)
      )
    
    
    plot_title <- if (!is.null(custom_title) && custom_title != "") {
      custom_title
    } else {
      if (input$plotDisplayType == "main") {
        paste("Effet principal :", resp_var, "par", fvar)
      } else {
        paste("Effet simple :", resp_var, "-", input$selectedSimpleEffect)
      }
    }
    
    base_labels <- ggplot2::labs(
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
          
          p <- ggplot2::ggplot(plot_data_merged, ggplot2::aes(x = x_var, y = y_var, fill = groups)) +
            ggplot2::geom_boxplot(alpha = 0.7) +
            ggplot2::scale_fill_discrete(name = legend_title)
        } else {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x_var, y = y_var, fill = x_var)) +
            ggplot2::geom_boxplot(alpha = 0.7) +
            ggplot2::annotate("text", 
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
          
          p <- ggplot2::ggplot(plot_data_merged, ggplot2::aes(x = x_var, y = y_var, fill = groups)) +
            ggplot2::geom_violin(alpha = 0.7) +
            ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
            ggplot2::scale_fill_discrete(name = legend_title)
        } else {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x_var, y = y_var, fill = x_var)) +
            ggplot2::geom_violin(alpha = 0.7) +
            ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, fill = "white") +
            ggplot2::annotate("text", 
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
          p <- ggplot2::ggplot(agg, ggplot2::aes(x = x_var, y = Moyenne, fill = groups, color = groups)) +
            ggplot2::geom_point(size = 4, shape = 21, stroke = 2) +
            ggplot2::scale_fill_discrete(name = legend_title) +
            ggplot2::scale_color_discrete(name = legend_title)
        } else {
          p <- ggplot2::ggplot(agg, ggplot2::aes(x = x_var, y = Moyenne, fill = x_var, color = x_var)) +
            ggplot2::geom_point(size = 4, shape = 21, stroke = 2) +
            ggplot2::annotate("text", 
                     x = seq_len(nrow(agg)),   # seq_len : 1:0 rendrait c(1, 0)
                     y = y_text_pos, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
        if (error_type == "se") {
          p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = Moyenne - Erreur_type, ymax = Moyenne + Erreur_type), 
                                 width = 0.2, color = "black")
        } else if (error_type == "sd") {
          p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = Moyenne - Ecart_type, ymax = Moyenne + Ecart_type), 
                                 width = 0.2, color = "black")
        } else if (error_type == "ci") {
          agg$ci_margin <- 1.96 * agg$Erreur_type
          p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = Moyenne - ci_margin, ymax = Moyenne + ci_margin), 
                                 width = 0.2, color = "black")
        }
        
      } else if (plot_type == "hist") {
        if (color_by_groups) {
          p <- ggplot2::ggplot(agg, ggplot2::aes(x = x_var, y = Moyenne, fill = groups)) +
            ggplot2::geom_col(alpha = 0.7, color = "black") +
            ggplot2::scale_fill_discrete(name = legend_title)
        } else {
          p <- ggplot2::ggplot(agg, ggplot2::aes(x = x_var, y = Moyenne, fill = x_var)) +
            ggplot2::geom_col(alpha = 0.7, color = "black") +
            ggplot2::annotate("text", 
                     x = seq_len(nrow(agg)),   # seq_len : 1:0 rendrait c(1, 0)
                     y = agg$Moyenne * 0.8, 
                     label = agg$groups, 
                     size = safe_graph_value_size,  
                     fontface = safe_graph_value_font_style,  
                     color = "red")
        }
        
        p <- p + ggplot2::geom_text(ggplot2::aes(y = Moyenne/2, label = round(Moyenne, 2)),
                           size = safe_mean_value_size,  # Taille personnalisable
                           fontface = "bold", color = "white")
        
        if (error_type != "none") {
          if (error_type == "se") {
            p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = Moyenne, ymax = Moyenne + Erreur_type), 
                                   width = 0.2, color = "black")
          } else if (error_type == "sd") {
            p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = Moyenne, ymax = Moyenne + Ecart_type), 
                                   width = 0.2, color = "black")
          } else if (error_type == "ci") {
            agg$ci_margin <- 1.96 * agg$Erreur_type
            p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = Moyenne, ymax = Moyenne + ci_margin), 
                                   width = 0.2, color = "black")
          }
        }
      }
      
      p <- p + base_theme + base_labels
      
      if (!is.null(p) && box_color != "default" && !color_by_groups) {
        p <- p + ggplot2::scale_fill_brewer(palette = box_color) +
          ggplot2::scale_color_brewer(palette = box_color)
      }
      
      
      if (!is.null(p) && !is.null(custom_axis_limits) && custom_axis_limits) {
        ylim_min <- if (!is.null(y_axis_min) && !is.na(y_axis_min)) y_axis_min else NA
        ylim_max <- if (!is.null(y_axis_max) && !is.na(y_axis_max)) y_axis_max else NA
        xlim_min <- if (!is.null(x_axis_min) && !is.na(x_axis_min)) x_axis_min else NA
        xlim_max <- if (!is.null(x_axis_max) && !is.na(x_axis_max)) x_axis_max else NA
        
        if (!is.na(ylim_min) || !is.na(ylim_max) || !is.na(xlim_min) || !is.na(xlim_max)) {
          p <- p + ggplot2::coord_cartesian(
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
          
          p <- p + ggplot2::scale_y_continuous(breaks = y_breaks)
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
            
            p <- p + ggplot2::scale_x_continuous(breaks = x_breaks)
          }
        }
      }
      
      if (!is.null(p)) {
        values$currentPlot <- p
        return(p)
      }
      
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur graphique"), type = "error", duration = 10)
      return(NULL)
    })
    
    return(NULL)
  })
  
  output$multiPlot <- renderPlotly({
    shiny::req(values$filteredData)
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
  
  output$plotTitle <- shiny::renderUI({
    shiny::req(values$filteredData)
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
    
    shiny::tags$span(type_text, " - ", current_var)
  })
  
  output$downloadMainEffects <- hstat_classeur_handler(function() {
    d <- values$multiResultsMain
    if (is.null(d)) return(NULL)
    list("Effets_principaux" = d[d$Type == "main", ])
  }, "effets_principaux", "Effets principaux")
  
  output$downloadSimpleEffects <- hstat_classeur_handler(function() {
    d <- values$multiResultsMain
    if (is.null(d)) return(NULL)
    list("Effets_simples" = d[d$Type == "simple_effect", ])
  }, "effets_simples", "Effets simples")
  
  output$downloadAllResults <- hstat_classeur_handler(function() {
    d <- values$multiResultsMain
    if (is.null(d)) return(NULL)
    hstat_tables_non_vides(list(
      "Effets_principaux" = d[d$Type == "main", ],
      "Effets_simples"    = d[d$Type == "simple_effect", ]))
  }, "analyse_complete", "Analyse complète")
  
  output$downloadMultiPlot <- shiny::downloadHandler(
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
        shiny::showNotification(
          "Aucun graphique à exporter : lancez d'abord l'analyse, puis réessayez.",
          type = "warning", duration = 6)
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0,
            label = "Aucun graphique disponible.\nLancez l'analyse puis téléchargez.",
            size = 6, colour = "#7f8c8d") +
          ggplot2::theme_void()
      }

      # Le commutateur de peripheriques qui vivait ici etait CALCULE PUIS JAMAIS
      # LU depuis le branchement sur l'ecrivain commun : quinze lignes qui
      # decrivaient un comportement que le code n'avait plus.

      # Le chemin d'echec ne laissait AUCUN fichier : Shiny renvoyait sa page
      # d'erreur HTML, enregistree sous le nom demande.
      ok <- hstat_ecrire_image(file, p, fmt, w, h, dpi)
      if (!ok)
        shiny::showNotification("Échec de l'export : le fichier téléchargé porte le motif.",
                         type = "error", duration = 8)

      if (ok) {
        shiny::showNotification(trf("Graphique téléchargé (%s, %d DPI).", toupper(fmt), as.integer(dpi)),
                         type = "message", duration = 3)
      }
    }
  )
  
  output$downloadSummaryStats <- shiny::downloadHandler(
    filename = function() { paste0("statistiques_resumees_", Sys.Date(), ".xlsx") },
    content = function(file) {
      shiny::req(values$multiResultsMain)
      
      summary_stats <- values$multiResultsMain %>%
        dplyr::group_by(Variable, Facteur, Type) %>%
        dplyr::summarise(
          Nb_groupes = dplyr::n(),
          Moyenne_generale = round(mean(Moyenne, na.rm = TRUE), 2),
          CV_moyen = round(mean(CV, na.rm = TRUE), 2),
          Ecart_type_moyen = round(mean(Ecart_type, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      var_summary <- values$multiResultsMain %>%
        dplyr::group_by(Variable) %>%
        dplyr::summarise(
          Nb_facteurs_testes = dplyr::n_distinct(Facteur),
          Nb_total_groupes = dplyr::n(),
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
  
  # Ce bouton n'ecrivait RIEN : Shiny renvoyait sa page d'erreur, enregistree
  # sous « rapport_complet_....pdf ». On rend desormais un PDF valide qui dit
  # ou se trouve le rapport -- un fichier qui explique vaut mieux qu'un fichier
  # qu'aucun lecteur n'ouvre.
  output$downloadFullReport <- shiny::downloadHandler(
    filename = function() { paste0("rapport_complet_", Sys.Date(), ".pdf") },
    content = function(file) {
      shiny::showNotification(
        "Le rapport complet se compose dans l'onglet « Rapport » (Word, PDF ou HTML).",
        type = "message", duration = 8)
      hstat_image_secours(file, "pdf",
        paste("Le rapport complet se compose dans l'onglet « Rapport » de",
              "l'application, qui assemble les analyses de la session en Word,",
              "PDF ou HTML."), width = 8.3, height = 11.7)
    }
  )
  })
}
