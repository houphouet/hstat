# ==============================================================================
#  MODULE : DIVERSITE ECOLOGIQUE
# ==============================================================================
# Richesse observee et estimee, indices de diversite et d'equitabilite,
# abondances, diversite beta, profils et rarefaction.
#
# LE MODULE NE CALCULE RIEN. Toutes les statistiques vivent dans `R/utils.R`
# (`hstat_div_*`) : c'est la regle du depot, et elle se paie ici en clarte --
# ce fichier choisit, affiche et met en forme, rien d'autre. Une statistique
# posee dans un `observeEvent` ne serait pas testable, et `vegan` n'est pas
# garanti present sur le poste de l'utilisateur.

# Le catalogue des figures. Une SEULE sortie graphique, un seul bloc d'export,
# un seul panneau d'options : huit `plotOutput` cote a cote auraient demande
# huit blocs d'export a tenir d'accord, et c'est la copie oubliee qui ment.
HSTAT_DIV_GRAPHIQUES <- c(
  "Rangs-abondances (diagramme de Whittaker)" = "rang",
  "Abondance relative par espèce"             = "abondance",
  "Indices par relevé"                        = "indices",
  "Matrice de dissimilarité (carte de chaleur)" = "beta",
  "Courbes de raréfaction"                    = "rarefaction",
  "Courbe d'accumulation des espèces"         = "accumulation",
  "Profil de diversité de Rényi"              = "renyi",
  "Profil des nombres de Hill"                = "hill")

# UNE ECHELLE CONTINUE NE SE REMPLACE PAS PAR UNE PALETTE QUALITATIVE.
# La palette choisie par l'utilisateur est posee sur TOUTES les figures, ce qui
# est juste tant que l'esthetique de groupe est discrete (une couleur par
# releve, une par espece). La carte de chaleur, elle, remplit ses cases par une
# VALEUR -- une dissimilarite entre 0 et 1 -- et porte donc sa propre echelle
# continue. Lui poser `scale_fill_brewer` par-dessus fait lever ggplot sur
# « Continuous value supplied to a discrete scale », et le graphique ne sort
# pas du tout : mesure a l'ecran, sept figures sur huit s'affichaient.
# La liste est declaree ici, a cote du catalogue : une neuvieme figure a
# echelle continue s'y inscrit au lieu de retrouver le defaut.
HSTAT_DIV_GRAPHIQUES_CONTINUS <- c("beta")

# Les indices proposes au graphique « Indices par releve ». Declares une fois :
# le selecteur, le trace et l'etiquette de l'axe y lisent la meme liste.
HSTAT_DIV_INDICES_TRACABLES <- c(
  "Richesse observée (Sobs)"        = "Richesse_S",
  "Indice de Shannon (H')"          = "Shannon_H",
  "Équitabilité de Pielou (J)"      = "Pielou_J",
  "Diversité de Simpson (1 − D)"    = "Simpson_1_D",
  "Simpson inverse (1/D)"           = "Simpson_inverse",
  "Dominance de Berger-Parker (d)"  = "Berger_Parker_d",
  "Richesse de Margalef (DMg)"      = "Margalef_DMg",
  "Indice de Menhinick (DMn)"       = "Menhinick_DMn",
  "Alpha de Fisher"                 = "Fisher_alpha",
  "Indice de Brillouin (HB)"        = "Brillouin_HB",
  "Équitabilité de Heip"            = "Heip_E",
  "Équitabilité de Simpson (E1/D)"  = "Simpson_E")


# LE REGLAGE DES STADES EST LE MEME DANS LES DEUX FORMES : `ch_Cocc` peut tout
# aussi bien etre une colonne (forme large) qu'une modalite de la colonne
# d'especes (forme longue). Le declarer deux fois serait la copie oubliee qui
# ment -- et c'est un reglage qui change la richesse.
.hstat_div_stades_ui <- function(ns) {
  shiny::tagList(
    shiny::checkboxInput(ns("divStades"),
      "Regrouper les stades d'une même espèce", value = FALSE),
    shiny::tags$small(style = "color:#6b7280;",
      "« ch_Cocc » et « ad_Cocc » sont la même espèce à deux stades : comptés ",
      "séparément, ils gonflent la richesse et tous les indices. Le cas est ",
      "signalé même si vous ne regroupez pas."))
}

mod_diversity_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    .hstat_scope_banner(exact = FALSE),

    shiny::fluidRow(
      # ---------------------------------------------------------------- CONFIG
      shiny::column(4,
        shinydashboard::box(
          title = shiny::tagList(shiny::icon("seedling"), " Constitution du tableau"),
          status = "success", solidHeader = TRUE, width = 12,

          .hstat_opt_section(
            "Forme du fichier", "table", "#16a085", "#e8f8f4",
            # LE FORMAT SE DECLARE, IL NE S'INFERE PAS. Une fiche large dont la
            # premiere colonne porte des noms d'especes serait lue a l'envers
            # sans un mot, et tous les indices sortiraient plausibles et faux.
            shiny::radioButtons(ns("divFormat"), NULL,
              choices = c("Long — une ligne par (relevé, espèce)" = "long",
                          "Large — une colonne par espèce"        = "large"),
              selected = "long"),
            shiny::tags$small(style = "color:#6b7280;",
              "En forme longue, une colonne d'effectif est facultative : sans elle, ",
              "chaque ligne compte pour un individu (fiche de terrain).")),

          .hstat_opt_section(
            "Colonnes", "list-ul", "#2980b9", "#eaf4fb",
            shiny::uiOutput(ns("divVarsUI"))),

          .hstat_opt_section(
            "Base du logarithme", "calculator", "#8e44ad", "#f4ecf7",
            # LA BASE FAIT PARTIE DU SEUIL, pas de l'affichage. La grille de
            # Frontier est en bits, le cadrage de Magurran en nats : la meme
            # valeur de H' y tombe dans deux classes differentes. Le verdict
            # convertit avant de comparer, et le selecteur dit ce qui est lu.
            shiny::selectInput(ns("divBase"), NULL,
              choices = HSTAT_DIV_BASES, selected = "2"),
            shiny::tags$small(style = "color:#6b7280;",
              "Shannon et Brillouin dépendent de la base ; Simpson, Pielou et ",
              "les équitabilités n'en dépendent pas. Les seuils sont convertis ",
              "automatiquement dans la base de leur auteur.")),

          shiny::actionButton(ns("divCalculer"), "Calculer les indices",
            icon = shiny::icon("play"), class = "btn-success btn-block btn-lg"),
          shiny::uiOutput(ns("divMessage"))
        ),

        # LA BOITE D'OPTIONS N'EXISTE QUE QUAND IL Y A UNE FIGURE A REGLER.
        # Construite d'emblee, elle pousse hors de l'ecran la seule chose que
        # l'on vient regler -- le defaut deja corrige sur six boites ailleurs.
        shiny::conditionalPanel(
          ns = ns, condition = "output.hasDiv",
          shinydashboard::box(
            # DEUX BOITES DU MEME NOM, C'EST UNE DE TROP. Celle-ci porte le
            # CHOIX de la figure et ses parametres de calcul (indice trace,
            # echelle, nombre d'especes, permutations) ; celle de la colonne de
            # droite porte sa MISE EN FORME (titres, tailles, theme, marges).
            # Sous un titre identique, qui cherche le theme ouvre celle-ci et ne
            # le trouve pas -- le libelle qui promet ce que la carte ne porte
            # pas, defaut que ce depot traque partout ailleurs.
            title = shiny::tagList(shiny::icon("sliders"), " Choix de la figure"),
            status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
            shiny::selectInput(ns("divGraphique"), "Graphique affiché",
              choices = HSTAT_DIV_GRAPHIQUES, selected = "rang"),
            shiny::conditionalPanel(
              ns = ns, condition = "input.divGraphique == 'indices'",
              shiny::selectInput(ns("divIndiceTrace"), "Indice représenté",
                choices = HSTAT_DIV_INDICES_TRACABLES, selected = "Shannon_H")),
            shiny::conditionalPanel(
              ns = ns, condition = "input.divGraphique == 'rang'",
              shiny::checkboxInput(ns("divRangLog"),
                "Échelle logarithmique des abondances", value = TRUE)),
            shiny::conditionalPanel(
              ns = ns, condition = "input.divGraphique == 'abondance'",
              shiny::numericInput(ns("divAbondTop"),
                "Nombre d'espèces affichées (les plus abondantes)",
                value = 20, min = 1, max = 500, step = 1)),
            shiny::conditionalPanel(
              ns = ns, condition = "input.divGraphique == 'accumulation'",
              shiny::numericInput(ns("divAccumPerm"), "Permutations",
                value = 100, min = 10, max = 1000, step = 10))
          )
        )
      ),

      # --------------------------------------------------------------- RESULTS
      shiny::column(8,
        shiny::conditionalPanel(
          ns = ns, condition = "output.hasDiv == false",
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("circle-info"), " Comment lire cet onglet"),
            status = "info", solidHeader = TRUE, width = 12,
            shiny::HTML(paste0(
              "<p>Choisissez la forme de votre fichier, les colonnes, puis lancez le calcul.</p>",
              "<ul>",
              "<li><b>Richesse</b> — ce que vous avez vu (S<sub>obs</sub>) et ce que ",
              "l'inventaire laisse estimer (Chao1, ACE, jackknife, bootstrap).</li>",
              "<li><b>Diversité</b> — Shannon, Simpson, Brillouin, Fisher, Hill, ",
              "Margalef, Menhinick, McIntosh, Berger-Parker.</li>",
              "<li><b>Équitabilité</b> — Pielou, Simpson, Heip, Sheldon, Camargo, ",
              "Smith &amp; Wilson, Brillouin, McIntosh.</li>",
              "<li><b>Diversité bêta</b> — Jaccard, Sørensen, Bray-Curtis, ",
              "Morisita-Horn, Ochiai, Kulczynski, Simpson, Whittaker, Chao, ",
              "et la partition de Baselga.</li>",
              "</ul>",
              "<p>Chaque indice interprété porte <b>sa grille, son auteur et sa ",
              "référence</b>, et la distinction entre un seuil publié et une ",
              "convention d'usage.</p>")))),

        shiny::conditionalPanel(
          ns = ns, condition = "output.hasDiv",
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("leaf"), " Résultats"),
            status = "success", solidHeader = TRUE, width = 12,
            shiny::tabsetPanel(
              shiny::tabPanel(
                shiny::tagList(shiny::icon("hashtag"), " Richesse"),
                shiny::br(),
                shiny::uiOutput(ns("divRichesseNote")),
                DT::DTOutput(ns("divRichesseTable"))),

              shiny::tabPanel(
                shiny::tagList(shiny::icon("chart-simple"), " Diversité"),
                shiny::br(),
                DT::DTOutput(ns("divIndicesTable"))),

              shiny::tabPanel(
                shiny::tagList(shiny::icon("scale-balanced"), " Équitabilité"),
                shiny::br(),
                DT::DTOutput(ns("divEquitTable"))),

              shiny::tabPanel(
                shiny::tagList(shiny::icon("lightbulb"), " Interprétation & seuils"),
                shiny::br(),
                shiny::uiOutput(ns("divSeuilsNote")),
                DT::DTOutput(ns("divSeuilsTable"))),

              shiny::tabPanel(
                shiny::tagList(shiny::icon("list-ol"), " Abondances"),
                shiny::br(),
                DT::DTOutput(ns("divAbondanceTable")),
                shiny::tags$hr(),
                shiny::h5(shiny::icon("table"), " Abondance relative par relevé (%)"),
                DT::DTOutput(ns("divAbondanceSiteTable"))),

              shiny::tabPanel(
                shiny::tagList(shiny::icon("arrows-left-right"), " Diversité bêta"),
                shiny::br(),
                shiny::fluidRow(
                  shiny::column(6, shiny::selectInput(ns("divBetaMethode"), "Coefficient",
                    choices = list("Présence / absence" = as.list(HSTAT_DIV_BETA_BINAIRES),
                                   "Abondances"         = as.list(HSTAT_DIV_BETA_ABONDANCE)),
                    selected = "jaccard")),
                  shiny::column(6, shiny::radioButtons(ns("divBetaSortie"), "Exprimée en",
                    choices = c("Similarité" = "similarite",
                                "Dissimilarité" = "dissimilarite"),
                    selected = "similarite", inline = TRUE))),
                shiny::uiOutput(ns("divBetaNote")),
                DT::DTOutput(ns("divBetaTable")),
                shiny::tags$hr(),
                shiny::h5(shiny::icon("code-branch"),
                          " Partition de Baselga : remplacement ou emboîtement ?"),
                shiny::tags$small(style = "color:#6b7280;",
                  "Une même dissimilarité recouvre deux phénomènes opposés : ",
                  "les espèces se remplacent (turnover), ou l'un des relevés est ",
                  "un sous-ensemble appauvri de l'autre (emboîtement)."),
                DT::DTOutput(ns("divBaselgaTable")),
                shiny::tags$hr(),
                shiny::h5(shiny::icon("layer-group"), " Partition α / β / γ de Whittaker"),
                DT::DTOutput(ns("divWhittakerTable"))),

              shiny::tabPanel(
                shiny::tagList(shiny::icon("chart-area"), " Graphique"),
                shiny::br(),
                withSpinner(shiny::plotOutput(ns("divPlot"), height = "520px"),
                            color = "#27ae60")),

              shiny::tabPanel(
                shiny::tagList(shiny::icon("book"), " Références des seuils"),
                shiny::br(),
                shiny::tags$small(style = "color:#6b7280;",
                  "Un seuil présenté comme publié alors qu'il relève de l'usage ",
                  "est exactement le genre d'affirmation qu'un rapport recopie ",
                  "sans la vérifier : la colonne « Origine » fait la distinction."),
                DT::DTOutput(ns("divReferencesTable"))),

              shiny::tabPanel(
                shiny::tagList(shiny::icon("border-all"), " Matrice relevés × espèces"),
                shiny::br(),
                DT::DTOutput(ns("divMatriceTable")))
            )
          ),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("download"), " Téléchargements"),
            status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
            collapsed = TRUE,
            shiny::h5(shiny::icon("table"), " Tableaux"),
            shiny::downloadButton(ns("divTXlsx"), "Excel (toutes les feuilles)",
                                  class = "btn-success"),
            shiny::downloadButton(ns("divTCsv"), "CSV", class = "btn-info"),
            shiny::tags$hr(),
            shiny::h5(shiny::icon("image"), " Graphique"),
            hstat_export_plot_ui(ns, "divP", width = 10, height = 6)
          ),

          shinydashboard::box(
            title = shiny::tagList(shiny::icon("paint-roller"), " Options du graphique"),
            status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
            collapsed = TRUE,
            shiny::fluidRow(
              shiny::column(4,
                .hstat_opt_section(
                  "Titres et libellés", "heading", "#2980b9", "#eaf4fb",
                  shiny::textInput(ns("divTitre"), "Titre", value = ""),
                  shiny::textInput(ns("divSousTitre"), "Sous-titre", value = ""),
                  shiny::textInput(ns("divLabX"), "Titre de l'axe X", value = ""),
                  shiny::textInput(ns("divLabY"), "Titre de l'axe Y", value = ""),
                  hstat_axe_titre_ui(ns, "div"))),
              shiny::column(4,
                .hstat_opt_section(
                  "Tailles et styles", "text-height", "#8e44ad", "#f4ecf7",
                  shiny::sliderInput(ns("divTitreSize"), "Taille du titre",
                    min = 8, max = 30, value = 15, step = 1, ticks = FALSE),
                  shiny::sliderInput(ns("divAxeSize"), "Taille des titres d'axe",
                    min = 6, max = 24, value = 12, step = 1, ticks = FALSE),
                  shiny::sliderInput(ns("divGradSize"), "Taille des graduations",
                    min = 5, max = 20, value = 10, step = 1, ticks = FALSE))),
              shiny::column(4,
                .hstat_opt_section(
                  "Couleurs", "palette", "#e67e22", "#fdf2e9",
                  shiny::selectInput(ns("divPalette"), "Palette",
                    choices = hstat_palettes_choix()),
                  shiny::sliderInput(ns("divOpacite"), "Opacité",
                    min = 0.1, max = 1, value = 0.85, step = 0.05, ticks = FALSE),
                  shiny::checkboxInput(ns("divGrilleMaj"), "Grille principale", TRUE),
                  shiny::checkboxInput(ns("divGrilleMin"), "Grille secondaire", FALSE),
                  shiny::selectInput(ns("divLegendePos"), "Position de la légende",
                    choices = c("Droite" = "right", "Bas" = "bottom",
                                "Haut" = "top", "Gauche" = "left", "Aucune" = "none"),
                    selected = "right")))),
            # LE MODULE PORTE SES PROPRES TAILLES (titre, titres d'axes,
            # graduations) : il ne les reprend pas du kit, qui n'en a qu'une.
            # Tout le reste du vocabulaire de « Visualisation des donnees » lui
            # arrive d'ici -- styles, inclinaisons des DEUX axes, bornes et pas
            # de graduations -- plutot que d'etre recopie une treizieme fois.
            #
            # `divAngleX` et `divTitreStyle` ont disparu : le kit les porte sous
            # `divXAngleX` et `divXStTitre`. Les garder en plus aurait donne
            # DEUX reglages pour un meme trait, dont un seul agirait.
            hstat_plot_extras_ui(ns, "divX",
                                 familles = HSTAT_PLOT_EXTRAS_FAMILLES)
          )
        )
      )
    )
  )
}


mod_diversity_server <- function(id, values,
                                 graine_globale = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # L'etat du module lui appartient : `values` porte l'etat de SESSION, et y
    # ranger une matrice de travail la ferait survivre a un changement de
    # fichier -- le defaut deja corrige sur les resultats multivaries.
    rv <- shiny::reactiveValues(mat = NULL, richesse = NULL, indices = NULL,
                                message = NULL)

    donnees <- shiny::reactive({
      values$filteredData %||% values$cleanData %||% values$data
    })

    # Vider au changement de fichier. `resetSignal` est monotone : c'est ce qui
    # permet a `observeEvent` de reagir au DEUXIEME chargement comme au premier.
    shiny::observeEvent(values$resetSignal, {
      rv$mat <- NULL; rv$richesse <- NULL; rv$indices <- NULL; rv$message <- NULL
    }, ignoreInit = TRUE)

    # ------------------------------------------------------------- SELECTEURS
    output$divVarsUI <- shiny::renderUI({
      df <- donnees()
      if (is.null(df) || !ncol(df))
        return(shiny::tags$em(style = "color:#7f8c8d;",
          "Chargez d'abord un jeu de données dans l'onglet Chargement."))
      cols <- names(df)
      num  <- cols[vapply(df, is.numeric, logical(1))]
      if (identical(input$divFormat %||% "long", "large")) {
        shiny::tagList(
          # UN RELEVE EST SOUVENT DESIGNE PAR PLUSIEURS COLONNES : c'est le
          # croisement (traitement x periode x semaine) qui fait l'unite
          # d'echantillonnage. N'en prendre qu'une agrege tout le reste sans un
          # mot, et la beta-diversite compte alors trois releves la ou l'essai
          # en porte quarante-cinq.
          shiny::selectInput(ns("divSite"), "Colonnes identifiant le relevé (facultatif)",
            choices = cols, multiple = TRUE),
          shiny::tags$small(style = "color:#6b7280;",
            "Plusieurs colonnes se combinent : traitement, période et semaine ",
            "forment alors un relevé par croisement. Sans sélection, chaque ",
            "ligne est un relevé."),
          shiny::selectInput(ns("divEspeces"), "Colonnes d'espèces",
            choices = num, multiple = TRUE,
            selected = utils::head(num, min(length(num), 50))),
          .hstat_div_stades_ui(ns))
      } else {
        shiny::tagList(
          shiny::selectInput(ns("divSite"), "Colonnes identifiant le relevé (facultatif)",
            choices = cols, multiple = TRUE),
          shiny::tags$small(style = "color:#6b7280;",
            "Plusieurs colonnes se combinent. Sans sélection, tout le fichier ",
            "forme un seul relevé."),
          shiny::selectInput(ns("divEspece"), "Colonne des espèces", choices = cols),
          shiny::selectInput(ns("divAbondance"), "Colonne des effectifs (facultatif)",
            choices = c("(une ligne = un individu)" = "", num)),
          .hstat_div_stades_ui(ns))
      }
    })

    # ---------------------------------------------------------------- CALCUL
    shiny::observeEvent(input$divCalculer, {
      df <- donnees()
      if (is.null(df)) {
        shiny::showNotification(
          tr("Chargez d'abord un jeu de données dans l'onglet Chargement."),
          type = "error", duration = 5)
        return()
      }
      res <- tryCatch(
        # `input$divSite` est maintenant un VECTEUR : `nzchar()` y rendrait un
        # vecteur de booleens, et `if()` sur plus d'une valeur leve depuis R 4.2.
        # C'est la longueur qui decide.
        hstat_div_matrice(df, format = input$divFormat %||% "long",
                          var_site = {
                            v <- as.character(input$divSite %||% character(0))
                            v <- v[nzchar(v)]
                            if (length(v)) v else NULL
                          },
                          var_espece = input$divEspece,
                          var_abondance = input$divAbondance,
                          var_especes = input$divEspeces,
                          grouper_stades = isTRUE(input$divStades)),
        error = function(e) e)
      if (inherits(res, "error")) {
        rv$mat <- NULL
        shiny::showNotification(hstat_err_fr(res, "Constitution du tableau impossible"),
                                type = "error", duration = 8)
        return()
      }
      base <- input$divBase %||% "2"
      rv$mat <- res
      rv$message <- attr(res, "message")
      rv$richesse <- cbind(
        data.frame(Releve = rownames(res), stringsAsFactors = FALSE),
        do.call(rbind, lapply(seq_len(nrow(res)), function(i) hstat_div_richesse(res[i, ]))))
      rv$indices <- cbind(
        data.frame(Releve = rownames(res), stringsAsFactors = FALSE),
        do.call(rbind, lapply(seq_len(nrow(res)), function(i) hstat_div_indices(res[i, ], base))))

      if (!is.null(rv$message))
        shiny::showNotification(shiny::tagList(shiny::icon("circle-info"), " ", rv$message),
                                type = "warning", duration = 10)
      shiny::showNotification(
        trf("Diversité calculée : %d relevé(s), %d espèce(s), %s individus.",
            nrow(res), ncol(res), format(sum(res), big.mark = " ")),
        type = "message", duration = 5)

    })

    output$hasDiv <- shiny::reactive(!is.null(rv$mat))
    # SANS CECI LA BOITE NE REAPPARAITRAIT JAMAIS : une sortie suspendue ne se
    # recalcule pas, et le drapeau resterait a sa derniere valeur connue.
    shiny::outputOptions(output, "hasDiv", suspendWhenHidden = FALSE)

    output$divMessage <- shiny::renderUI({
      if (is.null(rv$message)) return(NULL)
      shiny::div(class = "callout callout-warning", style = "margin-top:10px;padding:8px 12px;",
                 shiny::icon("triangle-exclamation"), " ", rv$message)
    })

    # ------------------------------------------------------------- AGREGATS
    # Le peuplement TOTAL, toutes stations confondues. C'est la ligne que l'on
    # interprete : les seuils de Frontier, de Pielou ou de Good portent sur un
    # peuplement, pas sur la moyenne de plusieurs.
    total_indices <- shiny::reactive({
      shiny::req(rv$mat)
      hstat_div_indices(colSums(rv$mat), input$divBase %||% "2")
    })
    total_richesse <- shiny::reactive({
      shiny::req(rv$mat)
      hstat_div_richesse(colSums(rv$mat))
    })

    seuils_table <- shiny::reactive({
      shiny::req(rv$mat)
      ind <- cbind(total_indices(), total_richesse()[, c("Couverture_Good", "Completude")])
      hstat_div_interpreter(ind, base = input$divBase %||% "2",
                            cles = c("Shannon_H", "Shannon_H_nats", "Pielou_J",
                                     "Simpson_1_D", "Simpson_E", "Berger_Parker_d",
                                     "Margalef_DMg", "Couverture_Good", "Completude"))
    })

    beta_mat <- shiny::reactive({
      shiny::req(rv$mat)
      shiny::validate(shiny::need(nrow(rv$mat) >= 2,
        "La diversité bêta compare des relevés : il en faut au moins deux."))
      hstat_div_beta(rv$mat, input$divBetaMethode %||% "jaccard",
                     input$divBetaSortie %||% "similarite")
    })

    # ---------------------------------------------------------------- TABLES
    # UN COMPTAGE N'A PAS DE DECIMALES. `formatRound` sur TOUTES les colonnes
    # numeriques ecrivait « 392.000 individus » et « 12.000 especes » : on doute
    # d'un entier affiche comme s'il avait trois decimales, et l'oeil cherche la
    # partie fractionnaire qui le justifierait. C'est le defaut deja corrige sur
    # le tableau de parametres de la DL50 (« 5.00000 degres de liberte »).
    # Une colonne dont toutes les valeurs observees sont entieres est donc
    # rendue SANS decimale ; les autres gardent la precision demandee. Le
    # tableau EXPORTE, lui, reste numerique -- la mise en forme est un fait
    # d'affichage, elle ne touche pas la donnee.
    .tab <- function(df, titre, digits = 4) {
      d <- DT::datatable(df, rownames = FALSE, filter = "top", extensions = "Buttons",
        options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
                       buttons = .hstat_dt_buttons(titre)),
        caption = htmltools::tags$caption(
          style = "caption-side: top; font-weight: 600;", titre))
      hstat_dt_arrondi(d, df, digits)
    }

    output$divRichesseTable <- DT::renderDT({
      shiny::req(rv$richesse)
      .tab(rbind(rv$richesse,
                 cbind(data.frame(Releve = "— Ensemble —", stringsAsFactors = FALSE),
                       total_richesse())),
           "Richesse observée et estimateurs non paramétriques", 3)
    })

    output$divRichesseNote <- shiny::renderUI({
      shiny::req(rv$richesse)
      r <- total_richesse()
      shiny::div(class = "hstat-interpretation",
        shiny::HTML(trf(paste0(
          "<b>%s espèces observées</b> pour %s individus. Chao1 corrigé du biais ",
          "estime <b>%s espèces</b> présentes (IC 95 %% : %s – %s), soit une ",
          "complétude de <b>%s %%</b>. Singletons : %s ; doubletons : %s."),
          .hstat_div_num(r$Sobs), format(r$Individus, big.mark = " "),
          .hstat_div_num(r$Chao1_corrige), .hstat_div_num(r$Chao1_IC_inf),
          .hstat_div_num(r$Chao1_IC_sup),
          .hstat_div_num(r$Completude * 100),
          .hstat_div_num(r$Singletons), .hstat_div_num(r$Doubletons))),
        shiny::tags$br(),
        shiny::tags$small(style = "color:#6b7280;",
          "Les singletons et les doubletons sont affichés à côté de l'estimation : ",
          "ce sont eux qui la portent, et sans eux le chiffre ne se discute pas. ",
          "Les estimateurs comptent des individus — sur des recouvrements ou des ",
          "biomasses, ils rendent « — » plutôt qu'un nombre calculé sur des décimales."))
    })

    output$divIndicesTable <- DT::renderDT({
      shiny::req(rv$indices)
      cols <- c("Releve", "Individus", "Richesse_S", "Shannon_H", "Shannon_Hmax",
                "Simpson_D", "Simpson_1_D", "Simpson_inverse", "Brillouin_HB",
                "McIntosh_U", "McIntosh_D", "Berger_Parker_d", "Berger_Parker_inverse",
                "Margalef_DMg", "Menhinick_DMn", "Fisher_alpha",
                "Hill_N0", "Hill_N1", "Hill_N2", "Hill_Ninf")
      d <- rbind(rv$indices, cbind(data.frame(Releve = "— Ensemble —",
                                              stringsAsFactors = FALSE), total_indices()))
      .tab(d[, intersect(cols, names(d)), drop = FALSE],
           trf("Indices de diversité — entropies en %s",
               names(HSTAT_DIV_BASES)[match(input$divBase %||% "2", HSTAT_DIV_BASES)]))
    })

    output$divEquitTable <- DT::renderDT({
      shiny::req(rv$indices)
      cols <- c("Releve", "Richesse_S", "Pielou_J", "Simpson_E", "Heip_E",
                "Sheldon_E", "Camargo_E", "Smith_Wilson_Evar", "Brillouin_E",
                "McIntosh_E")
      d <- rbind(rv$indices, cbind(data.frame(Releve = "— Ensemble —",
                                              stringsAsFactors = FALSE), total_indices()))
      .tab(d[, intersect(cols, names(d)), drop = FALSE],
           "Indices d'équitabilité (régularité de la répartition des individus)")
    })

    output$divSeuilsNote <- shiny::renderUI({
      shiny::req(rv$mat)
      shiny::div(class = "hstat-interpretation",
        shiny::HTML(paste0(
          "<b>Les seuils portent sur le peuplement d'ensemble</b>, toutes stations ",
          "réunies — les grilles de Frontier, de Pielou ou de Good décrivent un ",
          "peuplement, pas la moyenne de plusieurs. La base du logarithme est ",
          "<b>convertie automatiquement</b> dans celle de l'auteur avant comparaison : ",
          "une grille en bits lue sur une valeur en nats classerait « faible » un ",
          "peuplement moyen, sans rien signaler.")))
    })

    output$divSeuilsTable <- DT::renderDT({
      shiny::req(rv$mat)
      d <- seuils_table()
      DT::datatable(d, rownames = FALSE, escape = TRUE, extensions = "Buttons",
        options = list(pageLength = 12, scrollX = TRUE, dom = "Bfrtip",
                       columnDefs = list(list(width = "320px", targets = 3)),
                       buttons = .hstat_dt_buttons("interpretation_diversite")),
        caption = htmltools::tags$caption(
          style = "caption-side: top; font-weight: 600;",
          "Interprétation selon les seuils, avec leur auteur et leur origine")) |>
        DT::formatRound("Valeur", 4)
    })

    output$divAbondanceTable <- DT::renderDT({
      shiny::req(rv$mat)
      .tab(hstat_div_abondance(rv$mat),
           "Abondance absolue, abondance relative et fréquence d'occurrence", 3)
    })

    output$divAbondanceSiteTable <- DT::renderDT({
      shiny::req(rv$mat)
      .tab(hstat_div_abondance_site(rv$mat), "Abondance relative dans chaque relevé (%)", 2)
    })

    output$divBetaNote <- shiny::renderUI({
      shiny::req(rv$mat)
      m <- beta_mat()
      shiny::div(class = "hstat-interpretation",
        shiny::HTML(trf(paste0(
          "Coefficient : <b>%s</b>, exprimé en <b>%s</b>. ",
          "Confondre similarité et dissimilarité inverse la conclusion : ",
          "0,80 se lit « très semblables » dans un sens et « très différents » ",
          "dans l'autre."),
          hstat_html_escape(names(c(HSTAT_DIV_BETA_BINAIRES, HSTAT_DIV_BETA_ABONDANCE))[
            match(attr(m, "methode"), c(HSTAT_DIV_BETA_BINAIRES, HSTAT_DIV_BETA_ABONDANCE))]),
          hstat_html_escape(attr(m, "sortie")))))
    })

    output$divBetaTable <- DT::renderDT({
      shiny::req(rv$mat)
      m <- beta_mat()
      d <- data.frame(Releve = rownames(m), as.data.frame(unclass(m), check.names = FALSE),
                      check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
      .tab(d, trf("Matrice de %s entre relevés", attr(m, "sortie")))
    })

    output$divBaselgaTable <- DT::renderDT({
      shiny::req(rv$mat)
      shiny::validate(shiny::need(nrow(rv$mat) >= 2,
        "La partition de Baselga compare des relevés : il en faut au moins deux."))
      .tab(hstat_div_baselga(rv$mat),
           "Partition de Baselga (2010) : βsor = βsim (remplacement) + βsne (emboîtement)")
    })

    output$divWhittakerTable <- DT::renderDT({
      shiny::req(rv$mat)
      .tab(hstat_div_whittaker(rv$mat, input$divBase %||% "2"),
           "Partition de Whittaker (1960) : α, β et γ")
    })

    output$divReferencesTable <- DT::renderDT({
      d <- do.call(rbind, lapply(names(HSTAT_DIV_SEUILS), function(k) {
        g <- HSTAT_DIV_SEUILS[[k]]
        data.frame(
          Indice = g$indice,
          Base = if (is.na(g$base)) "sans objet"
                 else names(HSTAT_DIV_BASES)[match(g$base, HSTAT_DIV_BASES)],
          Grille = .hstat_div_grille_texte(g),
          Auteur = g$auteur,
          Origine = if (identical(g$origine, "primaire"))
            "Seuils publiés par l'auteur" else "Convention d'usage",
          Reference = g$reference, stringsAsFactors = FALSE)
      }))
      DT::datatable(d, rownames = FALSE, escape = TRUE, extensions = "Buttons",
        options = list(pageLength = 12, scrollX = TRUE, dom = "Bfrtip",
                       buttons = .hstat_dt_buttons("references_seuils_diversite")),
        caption = htmltools::tags$caption(
          style = "caption-side: top; font-weight: 600;",
          "Catalogue des seuils : bornes, auteur, origine et référence complète"))
    })

    output$divMatriceTable <- DT::renderDT({
      shiny::req(rv$mat)
      .tab(data.frame(Releve = rownames(rv$mat),
                      as.data.frame(unclass(rv$mat), check.names = FALSE),
                      check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL),
           "Matrice relevés × espèces", 2)
    })

    # -------------------------------------------------------------- GRAPHIQUE
    # UNE SEULE FONCTION POUR HUIT FIGURES. L'apercu et le telechargement la
    # lisent tous les deux : deux exemplaires du meme trace divergeraient a la
    # premiere correction, et rien ne signalerait laquelle des deux ment.
    graphique <- shiny::reactive({
      shiny::req(rv$mat)
      m <- rv$mat
      base <- input$divBase %||% "2"
      pal  <- input$divPalette %||% unname(HSTAT_PALETTE_GG)
      alpha <- hstat_finite(input$divOpacite, 0.85)
      quoi <- input$divGraphique %||% "rang"

      p <- switch(quoi,

        rang = {
          # Diagramme de Whittaker : les especes rangees de la plus abondante a
          # la plus rare. La pente dit la dominance -- une droite raide signale
          # un peuplement tenu par quelques especes.
          d <- hstat_div_abondance(m)
          d <- d[d$Abondance > 0, , drop = FALSE]
          d$Rang <- seq_len(nrow(d))
          g <- ggplot2::ggplot(d, ggplot2::aes(x = .data[["Rang"]],
                                               y = .data[["Abondance"]])) +
            ggplot2::geom_line(colour = "#7f8c8d", linewidth = HSTAT_GG_LINEWIDTH) +
            ggplot2::geom_point(size = HSTAT_GG_POINT_SIZE * 1.6, alpha = alpha,
                                colour = "#16a085") +
            ggplot2::labs(x = "Rang de l'espèce", y = "Abondance")
          if (isTRUE(input$divRangLog)) g <- g + ggplot2::scale_y_log10()
          g
        },

        abondance = {
          n <- max(1L, as.integer(hstat_finite(input$divAbondTop, 20)))
          d <- utils::head(hstat_div_abondance(m), n)
          d$Espece <- factor(d$Espece, levels = rev(d$Espece))
          ggplot2::ggplot(d, ggplot2::aes(x = .data[["Espece"]],
                                          y = .data[["Abondance_relative_pct"]],
                                          fill = .data[["Espece"]])) +
            ggplot2::geom_col(alpha = alpha, na.rm = TRUE) +
            ggplot2::coord_flip() +
            ggplot2::labs(x = "Espèce", y = "Abondance relative (%)") +
            ggplot2::guides(fill = "none")
        },

        indices = {
          col <- input$divIndiceTrace %||% "Shannon_H"
          shiny::validate(shiny::need(col %in% names(rv$indices),
            "Cet indice n'a pas été calculé sur ce jeu de données."))
          d <- rv$indices[, c("Releve", col)]
          names(d) <- c("Releve", "Valeur")
          d <- d[is.finite(d$Valeur), , drop = FALSE]
          shiny::validate(shiny::need(nrow(d) > 0,
            "Cet indice n'est calculable sur aucun relevé (une seule espèce, ou effectifs non entiers)."))
          d$Releve <- factor(d$Releve, levels = d$Releve)
          ggplot2::ggplot(d, ggplot2::aes(x = .data[["Releve"]], y = .data[["Valeur"]],
                                          fill = .data[["Releve"]])) +
            ggplot2::geom_col(alpha = alpha, na.rm = TRUE) +
            ggplot2::labs(x = "Relevé",
                          y = names(HSTAT_DIV_INDICES_TRACABLES)[
                                match(col, HSTAT_DIV_INDICES_TRACABLES)]) +
            ggplot2::guides(fill = "none")
        },

        beta = {
          bm <- beta_mat()
          d <- expand.grid(Ligne = rownames(bm), Colonne = colnames(bm),
                           stringsAsFactors = FALSE)
          d$Valeur <- as.numeric(bm)
          d$Ligne  <- factor(d$Ligne, levels = rev(rownames(bm)))
          d$Colonne <- factor(d$Colonne, levels = colnames(bm))
          ggplot2::ggplot(d, ggplot2::aes(x = .data[["Colonne"]], y = .data[["Ligne"]],
                                          fill = .data[["Valeur"]])) +
            ggplot2::geom_tile(colour = "white", alpha = alpha, na.rm = TRUE) +
            ggplot2::geom_text(ggplot2::aes(label = round(.data[["Valeur"]], 2)),
                               size = 3, na.rm = TRUE) +
            ggplot2::scale_fill_gradient(low = "#fdf2e9", high = "#16a085",
                                         na.value = "#ecf0f1") +
            ggplot2::labs(x = NULL, y = NULL,
                          fill = tools::toTitleCase(attr(bm, "sortie")))
        },

        rarefaction = {
          d <- do.call(rbind, lapply(seq_len(nrow(m)), function(i) {
            cb <- hstat_div_courbe_rarefaction(m[i, ])
            if (is.null(cb)) return(NULL)
            cb$Releve <- rownames(m)[i]; cb
          }))
          shiny::validate(shiny::need(!is.null(d) && nrow(d) > 0,
            "La raréfaction demande des effectifs entiers : elle compte des individus."))
          ggplot2::ggplot(d, ggplot2::aes(x = .data[["Individus"]], y = .data[["Especes"]],
                                          colour = .data[["Releve"]])) +
            ggplot2::geom_line(linewidth = HSTAT_GG_LINEWIDTH * 1.6, na.rm = TRUE) +
            ggplot2::labs(x = "Individus échantillonnés",
                          y = "Espèces attendues", colour = "Relevé")
        },

        accumulation = {
          perm <- max(10L, as.integer(hstat_finite(input$divAccumPerm, 100)))
          d <- hstat_div_accumulation(m, permutations = perm,
                                      graine = hstat_finite(graine_globale(), 123))
          shiny::validate(shiny::need(!is.null(d) && nrow(d) > 0,
            "La courbe d'accumulation demande au moins un relevé."))
          ggplot2::ggplot(d, ggplot2::aes(x = .data[["Releves"]], y = .data[["Especes"]])) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["IC_inf"]],
                                              ymax = .data[["IC_sup"]]),
                                 fill = "#16a085", alpha = alpha * 0.3, na.rm = TRUE) +
            ggplot2::geom_line(colour = "#16a085",
                               linewidth = HSTAT_GG_LINEWIDTH * 1.6, na.rm = TRUE) +
            ggplot2::geom_point(size = HSTAT_GG_POINT_SIZE * 1.4,
                                colour = "#16a085", na.rm = TRUE) +
            ggplot2::labs(x = "Nombre de relevés", y = "Espèces cumulées")
        },

        renyi = {
          al <- c(0, 0.25, 0.5, 1, 2, 4, 8)
          d <- do.call(rbind, lapply(seq_len(nrow(m)), function(i)
            data.frame(Alpha = al, Valeur = as.numeric(hstat_div_renyi(m[i, ], al)),
                       Releve = rownames(m)[i], stringsAsFactors = FALSE)))
          ggplot2::ggplot(d, ggplot2::aes(x = .data[["Alpha"]], y = .data[["Valeur"]],
                                          colour = .data[["Releve"]])) +
            ggplot2::geom_line(linewidth = HSTAT_GG_LINEWIDTH * 1.6, na.rm = TRUE) +
            ggplot2::geom_point(size = HSTAT_GG_POINT_SIZE * 1.4, na.rm = TRUE) +
            ggplot2::labs(x = "Ordre α", y = "Entropie de Rényi (nats)", colour = "Relevé")
        },

        hill = {
          qs <- c(0, 0.5, 1, 1.5, 2, 3, 4)
          d <- do.call(rbind, lapply(seq_len(nrow(m)), function(i)
            data.frame(q = qs, Valeur = as.numeric(hstat_div_hill(m[i, ], qs)),
                       Releve = rownames(m)[i], stringsAsFactors = FALSE)))
          ggplot2::ggplot(d, ggplot2::aes(x = .data[["q"]], y = .data[["Valeur"]],
                                          colour = .data[["Releve"]])) +
            ggplot2::geom_line(linewidth = HSTAT_GG_LINEWIDTH * 1.6, na.rm = TRUE) +
            ggplot2::geom_point(size = HSTAT_GG_POINT_SIZE * 1.4, na.rm = TRUE) +
            ggplot2::labs(x = "Ordre q", y = "Nombre d'espèces équivalentes",
                          colour = "Relevé")
        },

        NULL)

      shiny::validate(shiny::need(!is.null(p), "Graphique inconnu."))

      # LE KIT SE POSE EN DERNIER. Un theme complet remplace tout ce qui
      # precede : pose avant celui du module, il serait efface sans un mot.
      # LA LECTURE PORTE LES MEMES FAMILLES QUE LA DECLARATION : le defaut
      # du kit n'en rend que quatre, et les cinq autres seraient declarees,
      # deplacees, et sans effet.
      extras <- hstat_plot_extras_lire(input, "divX",
                                       familles = HSTAT_PLOT_EXTRAS_FAMILLES)
      titre  <- input$divTitre %||% ""
      st     <- input$divSousTitre %||% ""
      lx     <- input$divLabX %||% ""
      ly     <- input$divLabY %||% ""
      tsize  <- hstat_finite(input$divTitreSize, 15)
      asize  <- hstat_finite(input$divAxeSize, 12)
      gsize  <- hstat_finite(input$divGradSize, 10)

      if (nzchar(titre)) p <- p + ggplot2::ggtitle(titre)
      if (nzchar(st))    p <- p + ggplot2::labs(subtitle = st)
      if (nzchar(lx))    p <- p + ggplot2::labs(x = lx)
      if (nzchar(ly))    p <- p + ggplot2::labs(y = ly)

      # La palette qualitative ne se pose pas sur une figure a echelle continue
      # (cf. HSTAT_DIV_GRAPHIQUES_CONTINUS) : elle y remplacerait le degrade par
      # une echelle discrete, et ggplot refuserait de tracer.
      if (!(quoi %in% HSTAT_DIV_GRAPHIQUES_CONTINUS)) {
        ech <- hstat_scales_palette(pal)
        if (!is.null(ech)) p <- p + ech
      }

      p <- p +
        hstat_export_theme(input, "divP", base_size = extras$police %||% HSTAT_GG_BASE_SIZE) +
        ggplot2::theme(
          # LE STYLE ET L'INCLINAISON VIENNENT DU KIT, posé juste après : on ne
          # fixe ici que ce que le kit ne porte pas -- les tailles nommées.
          # Y redire une face ou un angle les figerait, et le réglage du kit
          # se changerait sans que l'image bouge.
          plot.title = element_markdown(size = tsize, hjust = 0.5,
                                        face = extras$st_titre %||% "bold"),
          # LE STYLE SE PASSE ICI, il ne s'herite pas. `axis.title.x` est un
          # `element_textbox` qui fixe SA PROPRE face : le `axis.title` du kit
          # est son PARENT, et un enfant qui declare la sienne n'herite rien.
          # Le reglage etait donc declare, lu, applique au theme -- et l'image
          # ne bougeait pas d'un octet. Mesure au navigateur : 20 814 octets
          # avant et apres le changement de style.
          axis.title.x = hstat_axe_titre_lire(input, "div", size = asize,
                                              face = extras$st_axes %||% "plain",
                                              axe = "x"),
          axis.title.y = hstat_axe_titre_lire(input, "div", size = asize,
                                              face = extras$st_axes %||% "plain",
                                              axe = "y"),
          axis.text.x = ggplot2::element_text(size = gsize),
          axis.text.y = ggplot2::element_text(size = gsize),
          legend.position = input$divLegendePos %||% "right",
          panel.grid.major = if (isTRUE(input$divGrilleMaj))
            ggplot2::element_line() else ggplot2::element_blank(),
          panel.grid.minor = if (isTRUE(input$divGrilleMin))
            ggplot2::element_line() else ggplot2::element_blank()) +
        # `titre = FALSE` : le titre est pose ici en `element_markdown()`, et
        # ggplot refuse de lui fusionner un `element_text()` -- la figure
        # entiere leverait. Le kit fournit la VALEUR du style, le module
        # l'applique sur son propre element.
        hstat_plot_extras_theme(extras, titre = FALSE)
      # LES BORNES ET LE PAS SONT DES ECHELLES, pas un theme : ils se posent
      # sur le graphique, jamais dans `theme()`.
      p <- hstat_plot_extras_scales(p, extras)
      p
    })

    output$divPlot <- shiny::renderPlot({ graphique() })

    # ---------------------------------------------------------------- EXPORTS
    tables_export <- function() {
      if (is.null(rv$mat)) return(list())
      l <- list(
        Richesse       = rv$richesse,
        Diversite      = rv$indices,
        Interpretation = seuils_table(),
        Abondances     = hstat_div_abondance(rv$mat),
        Abondance_site = hstat_div_abondance_site(rv$mat),
        Matrice        = data.frame(Releve = rownames(rv$mat),
                                    as.data.frame(unclass(rv$mat), check.names = FALSE),
                                    check.names = FALSE, stringsAsFactors = FALSE,
                                    row.names = NULL))
      if (nrow(rv$mat) >= 2) {
        bm <- beta_mat()
        l$Beta      <- data.frame(Releve = rownames(bm),
                                  as.data.frame(unclass(bm), check.names = FALSE),
                                  check.names = FALSE, stringsAsFactors = FALSE,
                                  row.names = NULL)
        l$Baselga   <- hstat_div_baselga(rv$mat)
        l$Whittaker <- hstat_div_whittaker(rv$mat, input$divBase %||% "2")
      }
      l
    }

    hstat_export_tables_handlers(output, "divT", tables_export,
                                 fname = "diversite_ecologique",
                                 libelle = "diversité écologique")
    output$divPDl <- hstat_export_plot_handler(input, "divP", function() graphique(),
                                               fname = "diversite")
  })
}
