#  Helpers UI -- Analyses multivariees etendues

# `.hstat_scope_banner()` a rejoint le socle (`R/utils.R`). Il etait defini ICI
# et appele par les UI de MODULE -- donc par du code du paquet, qui ne peut pas
# voir une definition de l'application. Cela ne cassait rien parce que `UX.R`
# est source avant que la moindre UI ne soit CONSTRUITE, mais la dependance
# allait a l'envers : le paquet ne doit rien attendre de l'application.


.mv_theme <- function(cat) {
  switch(cat,
    quanti = list(main = "#3c8dbc", bg = "#e8f2fc", border = "#b8daf0",
                  status = "primary", grad = "linear-gradient(135deg,#3c8dbc,#367fa9)"),
    quali  = list(main = "#e67e22", bg = "#fdf2e6", border = "#f5cba0",
                  status = "warning", grad = "linear-gradient(135deg,#e67e22,#d35400)"),
    mixte  = list(main = "#00a65a", bg = "#e6f5ee", border = "#aedec9",
                  status = "success", grad = "linear-gradient(135deg,#00a65a,#008d4c)"))
}

.mv_info_panel <- function(key, cat, principes, objectifs, taille, variables) {
  th <- .mv_theme(cat)
  bid <- paste0("mv-", key, "-info")
  shiny::div(style = "margin-bottom:10px;",
    shiny::div(
      style = paste0("cursor:pointer; background:", th$grad,
                     "; color:white; padding:8px 12px; border-radius:6px;",
                     " font-weight:bold; font-size:13px; user-select:none;"),
      onclick = sprintf("var c=document.getElementById('%s'); c.style.display=(c.style.display==='none'?'block':'none');", bid),
      shiny::icon("info-circle"), " Principes, objectifs & conditions ", shiny::icon("chevron-down")
    ),
    shiny::div(id = bid,
        style = paste0("display:none; background:", th$bg, "; border:1px solid ",
                       th$border, "; border-radius:0 0 6px 6px; padding:12px; font-size:12px;"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::tags$b(style = paste0("color:", th$main, ";"), shiny::icon("drafting-compass"), " Principes :"),
          shiny::tags$p(style = "margin:2px 0 6px 0; color:#333;", principes),
          shiny::tags$b(style = paste0("color:", th$main, ";"), shiny::icon("bullseye"), " Objectifs :"),
          shiny::tags$p(style = "margin:2px 0 0 0; color:#333;", objectifs)
        ),
        shiny::column(6,
          shiny::tags$b(style = paste0("color:", th$main, ";"), shiny::icon("ruler"), " Taille nécessaire :"),
          shiny::tags$ul(style = "margin:2px 0 6px 12px; padding:0; color:#333;",
                  lapply(taille, function(x) shiny::tags$li(shiny::HTML(x)))),
          shiny::tags$b(style = paste0("color:", th$main, ";"), shiny::icon("hashtag"), " Variables :"),
          shiny::tags$ul(style = "margin:2px 0 0 12px; padding:0; color:#333;",
                  lapply(variables, function(x) shiny::tags$li(shiny::HTML(x))))
        )
      ),
      shiny::uiOutput(paste0("mv_", key, "_conditions"))
    )
  )
}

# Element du catalogue de methodes (style maquette) : un titre cliquable + une
# courte description. Le clic est gere par le JS du catalogue (mvSelectMethod).
.mv_cat_item <- function(title, desc, cat = "quanti") {
  shiny::div(class = "mv-cat-item", `data-title` = title, `data-cat` = cat,
    shiny::div(class = "mv-cat-item-title", title),
    shiny::div(class = "mv-cat-item-desc", desc))
}

.mv_category_header <- function(label, ic, color) {
  # En-tetes de section masques : le catalogue (facon maquette) assure deja le
  # regroupement par categorie. On retourne un conteneur vide.
  NULL
}

.mv_analysis_box <- function(key, title, cat, principes, objectifs, taille, variables,
                             intro = NULL) {
  th <- .mv_theme(cat)
  shinydashboard::box(
    title = shiny::tagList(shiny::icon("project-diagram"), " ", title),
    status = th$status, width = 12, solidHeader = TRUE, collapsible = TRUE,
    collapsed = TRUE,

    if (!is.null(intro))
      shiny::p(style = "color:#555; font-style:italic; margin-bottom:8px;", shiny::icon("lightbulb"), " ", intro),

    .mv_info_panel(key, cat, principes, objectifs, taille, variables),

    shiny::uiOutput(paste0("mv_", key, "_controls")),

    shiny::div(style = "text-align:center; margin:12px 0;",
      shiny::actionButton(paste0("mv_", key, "_run"),
                   shiny::tagList(shiny::icon("play-circle"), " Lancer l'analyse"),
                   class = "btn-primary",
                   style = paste0("font-weight:bold; padding:8px 22px; background:",
                                  th$main, "; border-color:", th$main, ";"))
    ),

    shiny::uiOutput(paste0("mv_", key, "_status")),

    shinydashboard::tabBox(width = 12,
      shiny::tabPanel(shiny::tagList(shiny::icon("table"), " Résultats & métriques"),
        withSpinner(shiny::uiOutput(paste0("mv_", key, "_metrics")), color = th$main)
      ),
      shiny::tabPanel(shiny::tagList(shiny::icon("chart-area"), " Graphique"),
        shiny::div(style = "max-width:860px; margin:0 auto; width:100%;",
          withSpinner(shiny::plotOutput(paste0("mv_", key, "_plot"), height = "560px"), color = th$main))
      ),
      shiny::tabPanel(shiny::tagList(shiny::icon("file-alt"), " Détails techniques"),
        shiny::div(style = "max-height:520px; overflow-y:auto; font-family:'Courier New',monospace; font-size:12px; background:#fff; padding:14px; border-radius:5px;",
            shiny::verbatimTextOutput(paste0("mv_", key, "_summary")))
      )
    ),

    shiny::div(style = paste0("background:", th$grad, "; border-radius:10px; padding:14px; margin-top:8px;"),
      shiny::h4(style = "color:white; font-weight:bold; margin-top:0; text-align:center;",
         shiny::icon("file-export"), " Export des résultats"),
      shiny::fluidRow(shiny::column(12, style = "text-align:center;",
        shiny::downloadButton(paste0("mv_", key, "_dl_xlsx"),
                       shiny::HTML(paste0(as.character(shiny::icon("file-excel")), " <strong>Excel</strong>")),
                       class = "btn-success", style = "margin:4px; padding:7px 16px;"),
        shiny::downloadButton(paste0("mv_", key, "_dl_csv"),
                       shiny::HTML(paste0(as.character(shiny::icon("file-csv")), " <strong>CSV</strong>")),
                       class = "btn-warning", style = "margin:4px; padding:7px 16px;")
      ))
    )
  )
}

ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(
    title = shiny::span(
      style = "display:inline-flex; align-items:baseline; gap:9px;",
      shiny::span(shiny::icon("flask"), style = "font-size:17px;"),
      shiny::span("HStat", style = "font-family:'IBM Plex Sans',sans-serif; font-weight:600;"),
    ),
    titleWidth = 300,
    # Outils du bandeau (comme la maquette) : graine aleatoire, Aide, Reinitialiser,
    # puis la bascule de theme.
    shiny::tags$li(class = "dropdown hstat-header-tools",
      shiny::div(class = "hstat-seed",
        shiny::tags$label(`for` = "globalSeed", "Graine"),
        shiny::numericInput("globalSeed", label = NULL, value = 123, min = 1, max = 1e9, step = 1, width = "78px")),
      shiny::actionButton("helpBtn", "Aide", icon = shiny::icon("question-circle"), class = "hstat-hdr-btn"),
      shiny::actionButton("resetBtn", "Réinitialiser", icon = shiny::icon("redo"), class = "hstat-hdr-btn hstat-hdr-btn-warn")),
    # Bascule de langue. Meme forme que la bascule de theme : deux segments,
    # pas de menu deroulant — c'est un choix binaire, consulte rarement mais
    # qui doit rester visible.
    shiny::tags$li(class = "dropdown",
      shiny::div(class = "hstat-theme-toggle", `data-hstat-notranslate` = NA,
        shiny::tags$span(class = "seg active", id = "hstatLangFr",
          onclick = "window.hstatSetLangue && hstatSetLangue('fr');this.classList.add('active');document.getElementById('hstatLangEn').classList.remove('active');",
          "FR"),
        shiny::tags$span(class = "seg", id = "hstatLangEn",
          onclick = "window.hstatSetLangue && hstatSetLangue('en');this.classList.add('active');document.getElementById('hstatLangFr').classList.remove('active');",
          "EN"))),
    shiny::tags$li(class = "dropdown",
      shiny::div(class = "hstat-theme-toggle",
        shiny::tags$span(class = "seg active", id = "hstatThemeLight",
          onclick = "document.body.classList.remove('hstat-dark');document.body.classList.add('hstat-light');this.classList.add('active');document.getElementById('hstatThemeDark').classList.remove('active');",
          "Clair"),
        shiny::tags$span(class = "seg", id = "hstatThemeDark",
          onclick = "document.body.classList.remove('hstat-light');document.body.classList.add('hstat-dark');this.classList.add('active');document.getElementById('hstatThemeLight').classList.remove('active');",
          "Sombre"))),
    shinydashboard::dropdownMenu(
      type = "notifications", 
      badgeStatus = "info",
      shinydashboard::notificationItem(
        text = "L'application est prête",
        icon = shiny::icon("check-circle"),
        status = "success"
      )
    )
  ),
  shinydashboard::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shiny::tags$li(class = "header", "1. Préparation des données"),
      shinydashboard::menuItem("Chargement", tabName = "load", icon = shiny::icon("upload")),
      shinydashboard::menuItem("Exploration", tabName = "explore", icon = shiny::icon("binoculars")),
      shinydashboard::menuItem("Nettoyage", tabName = "clean", icon = shiny::icon("broom")),
      shinydashboard::menuItem("Filtrage", tabName = "filter", icon = shiny::icon("filter")),
      shiny::tags$li(class = "header", "2. Description"),
      shinydashboard::menuItem("Analyses descriptives", tabName = "descriptive", icon = shiny::icon("chart-bar")),
      shinydashboard::menuItem("Visualisation des données", tabName = "visualization", icon = shiny::icon("chart-line")),
      shiny::tags$li(class = "header", "3. Relations & inférence"),
      shinydashboard::menuItem("Corrélations", tabName = "corrélation", icon = shiny::icon("link")),
      shinydashboard::menuItem("Tests statistiques", tabName = "tests", icon = shiny::icon("calculator")),
      shinydashboard::menuItem("Comparaisons post-hoc", tabName = "multiple", icon = shiny::icon("sort-amount-down")),
      shinydashboard::menuItem("Analyses multivariées", tabName = "multivariate", icon = shiny::icon("project-diagram")),
      shinydashboard::menuItem("Analyses qualitatives", tabName = "qualitative", icon = shiny::icon("comments")),
      shinydashboard::menuItem("Interprétation & aide à la décision", tabName = "aidecision",
               icon = shiny::icon("compass-drafting")),
      shiny::tags$li(class = "header", "4. Modélisation & prédiction"),
      shinydashboard::menuItem("Séries temporelles", tabName = "timeseries", icon = shiny::icon("clock")),
      shinydashboard::menuItem("Machine Learning", tabName = "ml", icon = shiny::icon("robot")),
      shinydashboard::menuItem("Deep Learning", tabName = "dl", icon = shiny::icon("brain")),
      shiny::tags$li(class = "header", "5. Planification & outils"),
      shinydashboard::menuItem("Plan & Puissance", tabName = "design", icon = shiny::icon("flask")),
      shinydashboard::menuItem("Seuils d'efficacité", tabName = "threshold", icon = shiny::icon("gauge-high")),
      shiny::tags$li(class = "header", "6. À propos"),
      shinydashboard::menuItem("Citer HStat", tabName = "cite", icon = shiny::icon("quote-right"))
    )
  ),
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    # Ne pas bloquer le demarrage si shinyalert est absent (package optionnel)
    if (requireNamespace("shinyalert", quietly = TRUE))
      shinyalert::useShinyalert(force = TRUE),
    shiny::tags$head(
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, shrink-to-fit=no"),
      # Feuille de theme HStat (polices IBM Plex LOCALES -> fonctionne hors-ligne).
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                href = hstat_asset("hstat-theme.css")),
      # Theme clair par defaut ; la bascule ajoute/retire la classe hstat-dark.
      shiny::tags$script(shiny::HTML(
        "document.addEventListener('DOMContentLoaded',function(){if(!document.body.classList.contains('hstat-dark')){document.body.classList.add('hstat-light');}});")),
      # Accessibilite : declare la langue du document (lecteurs d'ecran, prononciation).
      shiny::tags$script(shiny::HTML(
        "document.documentElement.setAttribute('lang','fr');")),
      # Persistance de la session : le voile gris de Shiny (« Disconnected from
      # the server ») est masque au profit d'un bandeau francais qui annonce
      # l'etat et laisse reprendre la main. Un verrouillage d'ecran ne doit pas
      # fermer l'application : seul l'utilisateur la ferme.
      shiny::tags$style(shiny::HTML(
        "#shiny-disconnected-overlay{display:none !important;}")),
      shiny::tags$script(src = hstat_asset("hstat-session.js")),
      # Dictionnaire de traduction INCORPORE dans la page : aucune requete
      # reseau, le bilingue fonctionne hors ligne. La cle est la chaine
      # francaise elle-meme, donc une chaine non traduite reste en francais.
      shiny::tags$script(shiny::HTML(sprintf("window.HSTAT_I18N = %s;",
                               hstat_i18n_json("en")))),
      shiny::tags$script(src = hstat_asset("hstat-i18n.js")),
      # Copie de la citation dans le presse-papiers (API moderne + repli execCommand).
      shiny::tags$script(shiny::HTML(
        "Shiny.addCustomMessageHandler('hstat_copy_clip', function(m){",
        "  var t = m.text || '';",
        "  if (navigator.clipboard && window.isSecureContext) {",
        "    navigator.clipboard.writeText(t);",
        "  } else {",
        "    var ta = document.createElement('textarea'); ta.value = t;",
        "    ta.style.position='fixed'; ta.style.opacity='0';",
        "    document.body.appendChild(ta); ta.focus(); ta.select();",
        "    try { document.execCommand('copy'); } catch(e) {}",
        "    document.body.removeChild(ta);",
        "  }",
        "});")),
      # Realigne en-tetes et corps des DataTables (scrollX) : quand une table est
      # rendue dans un onglet/box masque, DataTables fige des largeurs d'en-tete
      # erronees -> decalage colonnes/valeurs. On rajuste a chaque affichage.
      shiny::tags$script(shiny::HTML("
        (function() {
          function hstatAdjustTables() {
            if (window.jQuery && $.fn.dataTable) {
              try {
                $($.fn.dataTable.tables({ visible: true, api: true })).columns.adjust();
              } catch (e) {}
            }
          }
          $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
            setTimeout(hstatAdjustTables, 60);
          });
          $(document).on('expanded.boxwidget shown.bs.collapse', function() {
            setTimeout(hstatAdjustTables, 60);
          });
          var hstatResizeTimer = null;
          $(window).on('resize', function() {
            clearTimeout(hstatResizeTimer);
            hstatResizeTimer = setTimeout(hstatAdjustTables, 150);
          });
          $(document).on('shiny:value', function(ev) {
            setTimeout(hstatAdjustTables, 120);
          });
        })();
      ")),
      # Les regles responsive vivent dans www/hstat-theme.css, section
      # « RESPONSIVE ». Elles etaient ici, et l'escamotage de la barre laterale
      # y etait code en dur a -230 px alors que la barre fait 300 : 70 px
      # restaient poses sur le contenu, coupe a gauche sur tous les onglets.
      # Une regle responsive dispersee en deux endroits finit par se
      # contredire ; il n'y en a plus qu'un.
      # Sur telephone, le menu est un TIROIR : il se referme des qu'on choisit
      # une entree ou qu'on touche le contenu, comme partout ailleurs. Sans
      # cela il reste ouvert PAR-DESSUS les resultats -- on ouvre le menu, on
      # choisit un onglet, et l'on ne voit plus ce qu'on etait venu lire. Il
      # faut alors retrouver le bouton, lui aussi recouvert.
      #
      # Les evenements de Shiny passent par jQuery : `$(document).on` et non
      # `document.addEventListener`, qui ne les voit jamais.
      shiny::tags$script(shiny::HTML("
        (function(){
          var TIROIR = 991;
          function fermer(){
            if (window.innerWidth <= TIROIR) {
              document.body.classList.remove('sidebar-open');
            }
          }
          $(document).on('click', '.sidebar-menu a', fermer);
          $(document).on('click touchstart', '.content-wrapper', fermer);
        })();
      ")),
      shiny::tags$script(shiny::HTML("
        Shiny.addCustomMessageHandler('expandBox', function(boxId) {
          var wrap = document.getElementById(boxId);
          if (!wrap) return;
          var box = wrap.classList.contains('box') ? wrap : wrap.querySelector('.box');
          if (box && box.classList.contains('collapsed-box')) {
            var btn = box.querySelector('[data-widget=\"collapse\"]');
            if (btn) btn.click();
          }
        });
        Shiny.addCustomMessageHandler('collapseBox', function(boxId) {
          var attempt = function(tries) {
            var wrap = document.getElementById(boxId);
            if (!wrap) { if (tries > 0) setTimeout(function(){attempt(tries-1);}, 150); return; }
            var box = wrap.classList.contains('box') ? wrap : wrap.querySelector('.box');
            if (!box) { if (tries > 0) setTimeout(function(){attempt(tries-1);}, 150); return; }
            if (!box.classList.contains('collapsed-box')) {
              var btn = box.querySelector('[data-widget=\"collapse\"]');
              if (btn) btn.click();
            }
          };
          attempt(10);
        });
      ")),
      # ---- Polices professionnelles (locales, sans dependance reseau) ----
      # Newsreader : serif editoriale a fort contraste -> titres.
      # Archivo    : grotesque neo-suisse -> corps et interface.
      shiny::tags$style(shiny::HTML("
        @font-face { font-family:'Newsreader'; font-style:normal; font-weight:400;
          font-display:swap; src:url('fonts/newsreader-latin-400-normal.woff2') format('woff2'); }
        @font-face { font-family:'Newsreader'; font-style:italic; font-weight:400;
          font-display:swap; src:url('fonts/newsreader-latin-400-italic.woff2') format('woff2'); }
        @font-face { font-family:'Newsreader'; font-style:normal; font-weight:500;
          font-display:swap; src:url('fonts/newsreader-latin-500-normal.woff2') format('woff2'); }
        @font-face { font-family:'Newsreader'; font-style:normal; font-weight:600;
          font-display:swap; src:url('fonts/newsreader-latin-600-normal.woff2') format('woff2'); }
        @font-face { font-family:'Archivo'; font-style:normal; font-weight:400;
          font-display:swap; src:url('fonts/archivo-latin-400-normal.woff2') format('woff2'); }
        @font-face { font-family:'Archivo'; font-style:normal; font-weight:500;
          font-display:swap; src:url('fonts/archivo-latin-500-normal.woff2') format('woff2'); }
        @font-face { font-family:'Archivo'; font-style:normal; font-weight:600;
          font-display:swap; src:url('fonts/archivo-latin-600-normal.woff2') format('woff2'); }
        @font-face { font-family:'Archivo'; font-style:normal; font-weight:700;
          font-display:swap; src:url('fonts/archivo-latin-700-normal.woff2') format('woff2'); }
      ")),

      # ---- Typographie HStat (sans toucher aux couleurs natives de Shiny) ----
      # On conserve le theme bleu par defaut d'AdminLTE (skin = "blue") pour une
      # experience familiere et confortable ; on applique uniquement les polices
      # professionnelles (Newsreader pour les titres, Archivo pour le corps) et
      # quelques raffinements neutres (rythme, lisibilite).
      shiny::tags$style(shiny::HTML("
        /* -- Variables typographiques (alignees sur la maquette HStat) -- */
        :root {
          --serif: 'Newsreader', Georgia, 'Times New Roman', serif;
          --sans:  'IBM Plex Sans', 'Archivo', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          --mono:  'IBM Plex Mono', ui-monospace, 'Cascadia Code', Menlo, Consolas, monospace;
        }

        /* -- Corps en grotesque Archivo -- */
        body, .content-wrapper, .wrapper, .main-sidebar, .left-side,
        .main-header .logo, .main-header .navbar, .form-control, .btn,
        label, .selectize-input, .nav-tabs > li > a, table.dataTable,
        .box-body, .small-box p, .info-box-text {
          font-family: var(--sans);
          -webkit-font-smoothing: antialiased;
          -moz-osx-font-smoothing: grayscale;
        }
        body { font-size: 14px; line-height: 1.6; letter-spacing: -0.002em; }

        /* -- Titres : grotesque IBM Plex Sans (conforme a la maquette) -- */
        h1, h2, h3, h4,
        .box-header .box-title, .modal-title {
          font-family: var(--sans);
          font-weight: 600;
          letter-spacing: -0.015em;
        }
        /* h5/h6 : etiquettes en grotesque -- */
        h5, h6 {
          font-family: var(--sans);
          font-weight: 700;
          letter-spacing: 0.04em;
        }
        /* Chiffres-cles des value boxes en serif -- */
        .small-box h3, .info-box-number { font-family: var(--serif); font-weight: 600; }

        /* Logo de l'en-tete : IBM Plex Sans (conforme a la maquette) -- */
        .main-header .logo { font-family: var(--sans); font-weight: 600; }

        /* Code / sorties console en monospace -- */
        pre, code, .shiny-text-output, samp { font-family: var(--mono); font-size: 13px; }

        /* -- Quelques raffinements neutres de lisibilite -- */
        .box-header .box-title { font-size: 15px; }
        label, .control-label { font-weight: 600; }
        .nav-tabs > li > a { font-weight: 600; }
        .btn { font-weight: 600; letter-spacing: 0.01em; }

        /* -- Accessibilite : focus clavier visible -- */
        a:focus-visible, button:focus-visible, .btn:focus-visible,
        input:focus-visible, select:focus-visible, textarea:focus-visible,
        [tabindex]:focus-visible {
          outline: 2px solid #3c8dbc !important;
          outline-offset: 2px !important;
        }
      ")),
      shiny::tags$script(shiny::HTML("
        $(document).ready(function() {

          // Raccourcis clavier : Enter dans un champ titre/axe/legende
          // declenche immediatement la mise a jour du graphique
          var titleInputIds = [
            'plotTitle', 'legendTitle', 'xAxisLabel', 'yAxisLabel',
            'quickPlotTitle', 'quickXLabel', 'quickYLabel',
            'customTitle', 'customSubtitle', 'customLegendTitle',
            'descPlotTitle',
            'distTitle', 'missingTitle',
            'pcaPlotTitle', 'hcpcClusterTitle', 'hcpcDendTitle',
            'afdIndTitle', 'afdVarTitle',
            'thresholdPlotTitle', 'thresholdLegendTitle'
          ];

          titleInputIds.forEach(function(id) {
            $(document).on('keydown', '#' + id, function(e) {
              if (e.key === 'Enter') {
                e.preventDefault();
                $(this).trigger('change');
              }
            });
          });

          // Double-clic sur un champ texte/numérique : sélectionner tout le contenu
          $(document).on('dblclick', 'input[type=text], input[type=number], textarea', function() {
            this.select();
          });

          // Ctrl+A dans un champ texte : sélectionner tout le contenu du champ
          // (evite que Ctrl+A sélectionné toute la page)
          $(document).on('keydown', 'input[type=text], textarea', function(e) {
            if ((e.ctrlKey || e.metaKey) && e.key === 'a') {
              e.stopPropagation();
              this.select();
            }
          });

          // ---- Raccourcis Gras (Ctrl+B) et Italique (Ctrl+I) ----
          // Wrap le texte sélectionné (ou le mot courant) avec <b>...</b> ou <i>...</i>
          // Compatible avec ggtext::element_markdown() pour le rendu dans les graphiques.
          function wrapSelection(input, tagOpen, tagClose) {
            var start = input.selectionStart;
            var end   = input.selectionEnd;
            var val   = input.value;
            var selected = val.substring(start, end);

            // Si rien n'est sélectionné, on tente de sélectionner le mot courant
            if (start === end) {
              var left  = start;
              var right = end;
              while (left > 0 && val.charCodeAt(left - 1) > 32) left--;
              while (right < val.length && val.charCodeAt(right) > 32) right++;
              selected = val.substring(left, right);
              start = left;
              end   = right;
            }

            // Si déjà entoure du même tag -> on retire (toggle)
            var fullTag = tagOpen + selected + tagClose;
            var before  = val.substring(0, start);
            var after   = val.substring(end);

            // Detecter si la sélection est déjà wrappee (toggle off)
            var tagLen = tagOpen.length;
            if (before.endsWith(tagOpen) && after.startsWith(tagClose)) {
              // Retirer le tag
              var newVal = before.slice(0, before.length - tagLen) + selected + after.slice(tagClose.length);
              input.value = newVal;
              input.setSelectionRange(start - tagLen, start - tagLen + selected.length);
            } else {
              // Ajouter le tag
              var newVal = before + fullTag + after;
              input.value = newVal;
              input.setSelectionRange(start + tagLen, start + tagLen + selected.length);
            }

            // Notifier Shiny du changement
            $(input).trigger('input').trigger('change');
          }

          $(document).on('keydown', 'input[type=text], textarea', function(e) {
            if ((e.ctrlKey || e.metaKey) && e.key === 'b') {
              e.preventDefault();
              wrapSelection(this, '<b>', '</b>');
            }
            if ((e.ctrlKey || e.metaKey) && e.key === 'i') {
              e.preventDefault();
              wrapSelection(this, '<i>', '</i>');
            }
          });

        });
      "))
    ),
    shinydashboard::tabItems(
      # ---- Chargement ----
      shinydashboard::tabItem(tabName = "load",
              shiny::fluidRow(
                shinydashboard::box(title = "Charger données", status = "primary", width = 12, solidHeader = TRUE,
                    shiny::fileInput("file", "Choisir un fichier",
                              accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv",
                                         ".sav", ".dta", ".rds", ".parquet", ".duckdb")),
                    shiny::tags$small(style = "color:#6b7280; display:block; margin:-8px 0 10px 0;",
                               shiny::icon("circle-info"),
                               " Formats : CSV, TXT, Excel, SPSS, Stata, RDS, Parquet, DuckDB. ",
                               "Les fichiers volumineux (CSV/Parquet) basculent automatiquement en mode hors-mémoire."),
                    shiny::uiOutput("sheetUI"),
                    shiny::radioButtons("sep", "Séparateur (CSV/TXT)",
                                 choices = c(Virgule = ",", `Point-virgule` = ";", Tab = "\t"), selected = ","),
                    shiny::checkboxInput("header", "Avec en-têtes", TRUE),
                    shiny::tags$details(
                      style = "margin:8px 0; padding:8px 12px; background:#f4f6f8; border-radius:8px;",
                      shiny::tags$summary(style = "cursor:pointer; font-weight:600; color:#4b5563; font-size:13px;",
                                   shiny::icon("sliders"), " Options avancées (gros fichiers)"),
                      shiny::div(style = "padding-top:10px;",
                        shiny::fluidRow(
                          shiny::column(6, shiny::numericInput("bigDataThreshold",
                                     "Seuil hors-mémoire (Mo)", value = 500, min = 50, max = 1000000, step = 50)),
                          shiny::column(6, shiny::numericInput("sampleSize",
                                     "Taille de l'échantillon (lignes)", value = 100000, min = 1000, max = 10000000, step = 10000))
                        ),
                        shiny::tags$small(style = "color:#6b7280;",
                          "Au-delà du seuil, le fichier n'est pas chargé en RAM : DuckDB l'interroge sur disque ",
                          "et les analyses portent sur un échantillon représentatif de la taille indiquée."))
                    ),
                    shiny::actionButton("loadData", "Charger", class = "btn-primary", icon = shiny::icon("upload")),

                    # --- Fusion de plusieurs fichiers (section integree au meme bloc) ---
                    shiny::tags$hr(style = "margin:18px 0 12px;"),
                    shiny::tags$details(
                      style = "margin:4px 0; padding:10px 14px; background:#eef7fb; border:1px solid #b6e0ef; border-radius:8px;",
                      shiny::tags$summary(style = "cursor:pointer; font-weight:700; color:#1b6f8c; font-size:14px;",
                                   shiny::icon("object-group"), " Importer et fusionner plusieurs fichiers (optionnel)"),
                      shiny::div(style = "padding-top:12px;",
                    shiny::p(style = "color:#5a6a7a; font-size:13px;",
                      "Importez deux fichiers ou plus, puis combinez-les : jointure par clé, empilement (ajout de lignes) ou juxtaposition (ajout de colonnes). ",
                      "Le résultat remplace les données de travail actuelles. ",
                      shiny::tags$b("Un classeur Excel est déplié en ses feuilles"),
                      " : un seul classeur à plusieurs feuilles suffit donc ici."),
                    shiny::fileInput("mergeFiles",
                              "Choisir plusieurs fichiers (ou un classeur Excel a plusieurs feuilles)",
                              multiple = TRUE,
                              accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv", ".sav", ".dta", ".rds")),
                    shiny::radioButtons("mergeSep", "Séparateur (CSV/TXT)",
                                 choices = c(Virgule = ",", `Point-virgule` = ";", Tab = "\t"),
                                 selected = ",", inline = TRUE),
                    shiny::selectInput("mergeType", "Type de fusion",
                      choices = list(
                        "Jointures (ajout de colonnes)" = c(
                          "Jointure interne (lignes communes)" = "inner",
                          "Jointure à gauche (toutes les lignes du 1er)" = "left",
                          "Jointure à droite (toutes les lignes du 2e)" = "right",
                          "Jointure complète (toutes les lignes)" = "full",
                          "Jointure croisée (produit cartésien, sans clé)" = "cross"),
                        "Jointures filtrantes (sans ajout de colonnes)" = c(
                          "Semi-jointure (1er AVEC correspondance)" = "semi",
                          "Anti-jointure (1er SANS correspondance)" = "anti",
                          "Anti-jointure (2e SANS correspondance)" = "anti_right"),
                        "Empilement / colonnes" = c(
                          "Empiler les lignes (UNION, NA-fill)" = "rows",
                          "Union distincte (empiler + dédupliquer)" = "union_distinct",
                          "Juxtaposer les colonnes (côte à côte)" = "cols"),
                        "Opérations ensemblistes (colonnes communes)" = c(
                          "Intersection des lignes" = "intersect",
                          "Différence : 1er sauf 2e" = "setdiff",
                          "Différence : 2e sauf 1er" = "setdiff_right"),
                        "Mise à jour des valeurs (par clé)" = c(
                          "Mettre à jour (remplacer par le 2e)" = "update",
                          "Compléter les valeurs manquantes (NA) du 1er" = "patch")),
                      selected = "inner"),
                    shiny::conditionalPanel(
                      condition = "['inner','left','right','full','semi','anti','anti_right','update','patch'].indexOf(input.mergeType) >= 0",
                      shiny::fluidRow(
                        shiny::column(6, shiny::uiOutput("mergeKeyLeftUI")),
                        shiny::column(6, shiny::uiOutput("mergeKeyRightUI"))),
                      shiny::tags$small(style = "color:#6b7280;",
                        shiny::icon("info-circle"),
                        " La clé peut comporter plusieurs colonnes (clé composite) et porter un nom différent dans chaque fichier. Pour plus de 2 fichiers, la jointure est enchaînée sur la même clé.")),
                    shiny::conditionalPanel(
                      condition = "input.mergeType == 'cross'",
                      shiny::tags$small(style = "color:#6b7280;", shiny::icon("info-circle"),
                        " La jointure croisée associe chaque ligne du 1er fichier à chaque ligne du 2e (toutes les combinaisons). Aucune clé requise.")),
                    shiny::conditionalPanel(
                      condition = "['intersect','setdiff','setdiff_right','union_distinct'].indexOf(input.mergeType) >= 0",
                      shiny::tags$small(style = "color:#6b7280;", shiny::icon("info-circle"),
                        " Opération sur les lignes entières, en utilisant les colonnes communes aux deux fichiers.")),
                    shiny::conditionalPanel(
                      condition = "input.mergeType == 'rows'",
                      shiny::checkboxInput("mergeAddSource", "Ajouter une colonne remplie avec le nom de chaque fichier", value = TRUE),
                      shiny::conditionalPanel(
                        condition = "input.mergeAddSource == true",
                        shiny::fluidRow(
                          shiny::column(6,
                            shiny::textInput("mergeSourceName", "Nom de cette colonne",
                                      value = "année", placeholder = "ex : année, source, lot")),
                          shiny::column(6,
                            shiny::selectInput("mergeSourceMode", "Valeur à inscrire",
                              choices = c("Nom du fichier (sans extension)" = "name",
                                          "Nombre extrait du nom (ex : 2021)" = "number"),
                              selected = "name"))),
                        shiny::tags$small(style = "color:#6b7280;", shiny::icon("info-circle"),
                          " Chaque ligne reçoit la valeur correspondant à son fichier d'origine. Avec « Nombre extrait », « 2021.csv » donne 2021."))),
                    shiny::actionButton("applyMerge", shiny::tagList(shiny::icon("object-group"), " Fusionner les fichiers"),
                                 class = "btn-info"),
                    shiny::uiOutput("mergeStatus")
                      )
                    )
                )
              ),
              shiny::conditionalPanel(
                condition = "output.hstatBigData == true",
                shiny::fluidRow(shiny::column(12,
                  shinydashboard::box(title = shiny::tagList(shiny::icon("vials"), " Échantillon de travail"),
                      status = "warning", width = 12, solidHeader = TRUE,
                    shiny::p(style = "color:#5a6a7a; font-size:13px;",
                      "Les analyses qui ajustent un modèle (ANOVA, régression, ACP, classifications, multivariées…) ",
                      "ne peuvent pas s'exécuter sur la totalité d'un très gros fichier. Elles travaillent sur cet échantillon. ",
                      "Vous pouvez l'agrandir autant que la mémoire de votre machine le permet, puis le retirer."),
                    shiny::fluidRow(
                      shiny::column(5, shiny::numericInput("sampleSizeLive",
                               shiny::tagList(shiny::icon("arrows-up-down"), " Taille de l'échantillon (lignes)"),
                               value = 100000, min = 1000, max = 20000000, step = 50000)),
                      shiny::column(7, shiny::div(style = "margin-top:25px;",
                        shiny::actionButton("redrawSample",
                          shiny::tagList(shiny::icon("rotate"), " Re-tirer l'échantillon"),
                          class = "btn-warning", style = "font-weight:bold;"),
                        shiny::tags$span(style = "margin-left:12px; font-size:12px; color:#6b7280;",
                          "Un nouveau tirage aléatoire remplace l'échantillon courant ; relancez ensuite vos analyses.")))
                    ),
                    shiny::uiOutput("sampleInfoLine")
                  )
                ))
              ),
              shiny::fluidRow(
                shinydashboard::valueBoxOutput("nrowBox", width = 3),
                shinydashboard::valueBoxOutput("ncolBox", width = 3),
                shinydashboard::valueBoxOutput("naBox", width = 3),
                shinydashboard::valueBoxOutput("memBox", width = 3)
              ),
              shiny::fluidRow(
                shinydashboard::box(title = "Aperçu des données", status = "info", width = 12, solidHeader = TRUE,
                    DT::DTOutput("preview"))
              )
      ),
      
      # ---- Exploration ----
      
      shinydashboard::tabItem(tabName = "explore",
              mod_explore_ui("explore")
      ),
      # ---- Nettoyage ----
      
      shinydashboard::tabItem(tabName = "clean",
              mod_clean_ui("clean")
      ),
      # ---- Filtrage ----
      
      shinydashboard::tabItem(tabName = "filter",
              mod_filter_ui("filter")
      ),
      # ---- Analyse descriptives ----
      shinydashboard::tabItem(tabName = "descriptive",
              mod_descriptive_ui("descriptive")),
      
      # ---- Visualisation des données ----
      shinydashboard::tabItem(tabName = "visualization",
              mod_viz_ui("visualization")),
      # ---- 3. Relations & inférence : Corrélations -> Tests -> Post-hoc -> Multivariées ----
      mod_correlation_ui("corrélation"),
      mod_tests_ui("tests"),
      mod_posthoc_ui("tests"),
      # ---- Analyses multivariees ----
      
      shinydashboard::tabItem(tabName = "multivariate",
              shiny::tags$style(shiny::HTML("
                #multivariate .box-body, #multivariate p, #multivariate li { font-size: 14px; }
                #multivariate .shiny-html-output { font-size: 14px; }
                #multivariate h4 { font-size: 18px; }
                #multivariate h5 { font-size: 15px; }
                #multivariate .nav-tabs > li > a { font-size: 15px; font-weight: 600; }
                #multivariate pré { font-size: 13px; }
              ")),
              .hstat_scope_banner(exact = FALSE),
              shiny::tags$div(class = "mv-layout",
                shiny::tags$div(class = "mv-catalog-col",
                    shiny::div(class = "mv-catalog",
                      shiny::div(class = "mv-catalog-head", shiny::icon("th-list"), " Catalogue de méthodes"),
                      shiny::tags$input(type = "text", id = "mvCatalogSearch",
                                 class = "form-control mv-catalog-search",
                                 placeholder = "Rechercher une méthode..."),
                      shiny::div(class = "mv-cat-group mv-grp-quanti", shiny::tags$span(class="mv-grp-dot"), "Quantitatives", shiny::tags$span(class="mv-grp-count", "9")),
                      shiny::div(class = "mv-cat-list",
                        .mv_cat_item("Analyse en Composantes Principales (ACP)", "Réduit p variables corrélées en composantes orthogonales."),
                        .mv_cat_item("Classification Hiérarchique sur Composantes Principales (HCPC)", "Dendrogramme consolidé par k-means sur composantes."),
                        .mv_cat_item("Analyse Factorielle Discriminante (AFD)", "Sépare des groupes connus par combinaisons discriminantes."),
                        .mv_cat_item("Classification k-means (partitionnement)", "Partitionne n individus en k groupes homogènes."),
                        .mv_cat_item("Analyse Factorielle Exploratoire (AFE)", "Découvre les facteurs latents sous-jacents aux variables."),
                        .mv_cat_item("Analyse Factorielle Confirmatoire (AFC-c)", "Teste un modèle de mesure facteurs ↔ items pré-spécifié."),
                        .mv_cat_item("Multi-Trait Multi-Method (MTMM)", "Valide la convergence et la discrimination traits × méthodes."),
                        .mv_cat_item("Régression PLS / PLS-DA", "Prédit en grande dimension via composantes latentes."),
                        .mv_cat_item("Régression linéaire multiple", "Explique une réponse continue par plusieurs prédicteurs.")
                      ),
                      shiny::div(class = "mv-cat-group mv-grp-quali", shiny::tags$span(class="mv-grp-dot"), "Qualitatives / catégorielles", shiny::tags$span(class="mv-grp-count", "5")),
                      shiny::div(class = "mv-cat-list",
                        .mv_cat_item("Analyse Factorielle des Correspondances (AFC)", "Associe deux variables qualitatives (table de contingence).", "quali"),
                        .mv_cat_item("Analyse des Correspondances Multiples (ACM)", "Généralise l'AFC à plusieurs variables qualitatives.", "quali"),
                        .mv_cat_item("Classification k-modes (partitionnement)", "k-means pour données qualitatives (modes, Hamming).", "quali"),
                        .mv_cat_item("Analyse en Classes Latentes (LCA)", "Identifie des sous-populations latentes (mélange EM).", "quali"),
                        .mv_cat_item("Régression logistique / multinomiale", "Prédit une réponse catégorielle, odds ratios.", "quali")
                      ),
                      shiny::div(class = "mv-cat-group mv-grp-mixte", shiny::tags$span(class="mv-grp-dot"), "Mixtes (quanti + quali)", shiny::tags$span(class="mv-grp-count", "3")),
                      shiny::div(class = "mv-cat-list",
                        .mv_cat_item("Analyse Factorielle de Données Mixtes (AFDM)", "Combine ACP et ACM pour tableaux mixtes.", "mixte"),
                        .mv_cat_item("Analyse Factorielle Multiple (AFM)", "Intègre plusieurs groupes de variables équilibrés.", "mixte"),
                        .mv_cat_item("Classification k-prototypes (partitionnement mixte)", "k-means + k-modes pour variables mixtes.", "mixte")
                      )
                    )
                ),
                shiny::tags$div(class = "mv-analyses-col",
                  shiny::div(class = "mv-config-hint mv-empty-hint",
                    shiny::icon("hand-point-left"),
                    shiny::HTML(" Choisissez une méthode dans le catalogue : sa configuration s'affiche ci-dessous. <b>Une seule méthode active à la fois.</b>")),
                  # En-tete de methode facon maquette (badge + titre + description),
                  # rempli dynamiquement au clic sur le catalogue. Masque tant qu'aucune
                  # methode n'est choisie.
                  shiny::div(id = "mvMethodHeader", class = "mv-method-header", style = "display:none;",
                    shiny::div(class = "mv-mh-top",
                      shiny::tags$span(id = "mvMhBadge", class = "mv-mh-badge", "QUANTITATIVE")),
                    shiny::tags$h3(id = "mvMhTitle", class = "mv-mh-title", ""),
                    shiny::tags$p(id = "mvMhDesc", class = "mv-mh-desc", "")),
              shiny::tags$script(shiny::HTML("
                (function(){
                  function tabRoot(){ return document.getElementById('shiny-tab-multivariate') || document.querySelector('[data-value=\"multivariate\"]'); }
                  function colRoot(){ var t=tabRoot(); return t?t.querySelector('.mv-analyses-col'):null; }
                  function norm(s){ return (s||'').replace(/\\s+/g,' ').trim().toLowerCase(); }
                  function expand(box){ if(box && box.classList.contains('collapsed-box')){ var b=box.querySelector('[data-widget=\"collapse\"]'); if(b) b.click(); } }
                  function collapse(box){ if(box && !box.classList.contains('collapsed-box')){ var b=box.querySelector('[data-widget=\"collapse\"]'); if(b) b.click(); } }
                  function isOptionsBox(b){ var h=b.querySelector('.box-title'); return !!(h && norm(h.textContent).indexOf('options d')>=0); }
                  // Enveloppe de premier niveau (la .row qui porte la box) dans la colonne d'analyses.
                  function rowWrap(b){
                    var col=colRoot();
                    var r=b.closest('.row');
                    // remonte jusqu'a la .row enfant directe de la colonne d'analyses
                    while(r && r.parentElement && r.parentElement!==col){
                      var up=r.parentElement.closest('.row');
                      if(!up || up===r) break;
                      r=up;
                    }
                    return r || b.closest('.col-sm-12') || b.parentElement || b;
                  }
                  // Toutes les box METHODES (on exclut la box d'options, traitee a part).
                  function methodBoxes(){
                    var col=colRoot(); if(!col) return [];
                    return Array.prototype.slice.call(col.querySelectorAll('.box')).filter(function(b){
                      var h=b.querySelector('.box-title'); if(!h) return false;
                      var t=norm(h.textContent);
                      if(isOptionsBox(b)) return false;
                      if(t.indexOf('résultats de')===0) return false; // box imbriquees de résultats
                      // on ne garde que les box dont l'ancetre .box le plus proche est elle-même
                      var parentBox = b.parentElement && b.parentElement.closest('.box');
                      if(parentBox) return false;
                      return true;
                    });
                  }
                  function optionsBox(){
                    var col=colRoot(); if(!col) return null;
                    var bs=Array.prototype.slice.call(col.querySelectorAll('.box'));
                    for(var i=0;i<bs.length;i++){ if(isOptionsBox(bs[i])) return bs[i]; }
                    return null;
                  }
                  function boxByTitle(t){
                    var bs=methodBoxes(), tt=norm(t);
                    for(var i=0;i<bs.length;i++){ if(norm(bs[i].querySelector('.box-title').textContent)===tt) return bs[i]; }
                    for(var j=0;j<bs.length;j++){ if(norm(bs[j].querySelector('.box-title').textContent).indexOf(tt)>=0) return bs[j]; }
                    return null;
                  }
                  // N'affiche QUE la methode choisie. Les options d'affichage sont
                  // desormais une sous-boite PROPRE a chaque methode (imbriquee), donc
                  // pas de box d'options globale a preserver.
                  function showOnly(target){
                    methodBoxes().forEach(function(b){
                      var w=rowWrap(b);
                      if(w) w.style.display = (b===target ? '' : 'none');
                    });
                  }
                  // Etat initial : aucune methode -> on masque toutes les box methodes ;
                  // seule l'invite « choisissez une methode » reste visible.
                  function hideAllMethodBoxes(){
                    methodBoxes().forEach(function(b){ var w=rowWrap(b); if(w) w.style.display='none'; });
                  }

                  function catLabel(cat){ return cat==='quali' ? 'QUALITATIVE' : (cat==='mixte' ? 'MIXTE' : 'QUANTITATIVE'); }
                  function selectMethod(title, cat){
                    if(cat && window.Shiny && Shiny.setInputValue){ Shiny.setInputValue('mv_category', cat, {priority:'event'}); }
                    document.querySelectorAll('.mv-cat-item').forEach(function(it){ it.classList.remove('active'); });
                    var col=colRoot(); if(col) col.classList.add('has-selection');
                    // En-tete de methode facon maquette
                    var act=document.querySelector('.mv-cat-item[data-title=\"'+title.replace(/\"/g,'')+'\"]');
                    var desc = act ? (act.querySelector('.mv-cat-item-desc')||{}).textContent||'' : '';
                    var hdr=document.getElementById('mvMethodHeader');
                    if(hdr){
                      hdr.style.display='';
                      var b=document.getElementById('mvMhBadge');
                      if(b){ b.textContent=catLabel(cat); b.className='mv-mh-badge mv-mh-'+(cat||'quanti'); }
                      var ti=document.getElementById('mvMhTitle'); if(ti) ti.textContent=title;
                      var de=document.getElementById('mvMhDesc'); if(de) de.textContent=desc;
                    }
                    var tries=0;
                    var iv=setInterval(function(){
                      var target=boxByTitle(title);
                      if(target){
                        showOnly(target);
                        expand(target);
                        if(act) act.classList.add('active');
                        setTimeout(function(){ try{ document.getElementById('mvMethodHeader').scrollIntoView({behavior:'smooth',block:'start'});}catch(e){} },150);
                        clearInterval(iv);
                      }
                      if(++tries>12) clearInterval(iv);
                    }, 200);
                  }
                  window.mvSelectMethod = selectMethod;

                  document.addEventListener('click', function(e){
                    var it=e.target.closest && e.target.closest('.mv-cat-item');
                    if(it){ selectMethod(it.getAttribute('data-title'), it.getAttribute('data-cat')); }
                  });
                  document.addEventListener('input', function(e){
                    if(e.target && e.target.id==='mvCatalogSearch'){
                      var q=e.target.value.trim().toLowerCase();
                      document.querySelectorAll('.mv-cat-item').forEach(function(it){
                        var hay=(it.getAttribute('data-title')+' '+it.textContent).toLowerCase();
                        it.style.display=(q===''||hay.indexOf(q)>=0)?'':'none';
                      });
                    }
                  });
                  // Masque l'ancien bandeau de catégorie + en-tetes de section (redondants avec le catalogue)
                  function hideLegacy(){
                    var t=tabRoot(); if(!t) return;
                    var r=t.querySelector('#mv_category'); if(r){ var w=r.closest('div'); if(w&&w.parentElement){ w.parentElement.style.display='none'; } }
                    Array.prototype.slice.call(t.querySelectorAll('h3')).forEach(function(h){
                      if(norm(h.textContent).indexOf('analyses multivariees')===0){ var rr=h.closest('.row'); if(rr) rr.style.display='none'; }
                    });
                  }
                  // Etat initial facon maquette : tant qu'aucune methode n'est choisie dans le
                  // catalogue, on masque TOUTES les box de methodes (et la box d'options) ; seule
                  // l'invite « choisissez une methode » reste visible. Ne s'execute qu'une fois,
                  // et seulement si l'utilisateur n'a pas déjà sélectionné une methode.
                  var mvInitDone=false;
                  function mvInitialState(){
                    if(mvInitDone) return;
                    var col=colRoot(); if(!col) return;
                    if(col.classList.contains('has-selection')){ mvInitDone=true; return; }
                    if(methodBoxes().length>0){ hideAllMethodBoxes(); mvInitDone=true; }
                  }
                  var t2=0, iv2=setInterval(function(){ if(tabRoot()){ hideLegacy(); mvInitialState(); if(++t2>6) clearInterval(iv2);} if(t2>25) clearInterval(iv2); },400);
                })();
              ")),
              shiny::fluidRow(
                shinydashboard::box(title = "Analyse en Composantes Principales (ACP)", status = "info", width = 12, solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                    
                    shiny::div(style = "margin-bottom:10px;",
                        shiny::div(
                          style = "cursor:pointer; background:linear-gradient(135deg,#1565c0,#1976d2); color:white; padding:8px 12px; border-radius:6px; font-weight:bold; font-size:13px; user-select:none;",
                          onclick = "var c=document.getElementById('pca-info-body'); c.style.display=(c.style.display==='none'?'block':'none');",
                          shiny::icon("info-circle"), " Principes, objectifs & conditions de l'ACP ",
                          shiny::icon("chevron-down")
                        ),
                        shiny::div(id = "pca-info-body", style = "display:none; background:#e8f4f8; border:1px solid #90caf9; border-radius:0 0 6px 6px; padding:12px; font-size:12px;",
                            shiny::fluidRow(
                              shiny::column(6,
                                     shiny::tags$b(style="color:#1565c0;", shiny::icon("drafting-compass"), " Principes :"),
                                     shiny::tags$p(style="margin:2px 0 6px 0; color:#333;", "Réduction dimensionnelle par rotation orthogonale maximisant la variance. Transforme p variables corrélées en composantes principales indépendantes (valeurs propres de la matrice de corrélation)."),
                                     shiny::tags$b(style="color:#1565c0;", shiny::icon("bullseye"), " Objectifs :"),
                                     shiny::tags$p(style="margin:2px 0 0 0; color:#333;", "Explorer la structure, identifier des patterns, visualiser des données multivariées, réduire la dimensionnalité avant classification.")
                              ),
                              shiny::column(6,
                                     shiny::tags$b(style="color:#1565c0;", shiny::icon("ruler"), " Taille nécessaire :"),
                                     shiny::tags$ul(style="margin:2px 0 6px 12px; padding:0; color:#333;",
                                             shiny::tags$li("Minimum absolu : n", shiny::icon("arrow-right"), "30"),
                                             shiny::tags$li("Recommandé : n", shiny::icon("arrow-right"), "max(50, 5xp)"),
                                             shiny::tags$li("Idéal : n", shiny::icon("arrow-right"), "100 pour stabilité")
                                     ),
                                     shiny::tags$b(style="color:#1565c0;", shiny::icon("hashtag"), " Variables minimales :"),
                                     shiny::tags$ul(style="margin:2px 0 0 12px; padding:0; color:#333;",
                                             shiny::tags$li("Minimum absolu : p", shiny::icon("arrow-right"), "2"),
                                             shiny::tags$li("Recommandé : p", shiny::icon("arrow-right"), "3-5"),
                                             shiny::tags$li("Toutes doivent être numériques")
                                     )
                              )
                            ),
                            shiny::uiOutput("pcaConditionsCheck")
                        )
                    ),
                    
                    shiny::uiOutput("pcaVarSelect"),
                    
                    shiny::uiOutput("pcaCollinearityPanel"),
                    
                    shiny::checkboxInput("pcaScale", "Standardiser les variables", TRUE),
                    shiny::checkboxInput("pcaUseMeans", "Utiliser les moyennes par groupe", FALSE),
                    shiny::conditionalPanel(
                      condition = "input.pcaUseMeans == true",
                      shiny::uiOutput("pcaMeansGroupSelect"),
                      shiny::actionButton("pcaRefresh", "Actualiser l'ACP", 
                                   icon = shiny::icon("sync"), 
                                   class = "btn-info btn-sm",
                                   style = "margin-bottom: 10px;")
                    ),
                    shiny::uiOutput("pcaQualiSupSelect"),
                    shiny::uiOutput("pcaQuantiSupSelect"),
                    shiny::uiOutput("pcaIndSupSelect"),
                    shiny::uiOutput("pcaLabelSourceSelect"),
                    shiny::hr(),
                    shiny::radioButtons("pcaPlotType", "Type de visualisation:",
                                 choices = c("Variables" = "var", "Individus" = "ind", "Biplot" = "biplot"),
                                 selected = "var", inline = TRUE),
                    
                    shiny::div(style = "background:#f0f7ff; border-left:3px solid #2196f3; padding:8px 12px; margin:6px 0 10px 0; border-radius:0 4px 4px 0;",
                        shiny::selectInput("pcaColorBy", 
                                    shiny::tagList(shiny::icon("palette"), " Colorer les éléments par :"),
                                    choices = c(
                                      "Contribution (% à la composante)"       = "contrib",
                                      "Cos² (qualité de représentation)"       = "cos2",
                                      "Indice de saturation (|corrélation|)"   = "sat"
                                    ),
                                    selected = "contrib"),
                        shiny::uiOutput("pcaColorByLegend")
                    ),
                    shiny::numericInput("pcaComponents", "Nombre de composantes:", value = 5, min = 2, max = 10),
                    
                    shiny::div(style = "text-align:center;margin:12px 0;",
                        shiny::actionButton("pcaRun", shiny::tagList(shiny::icon("play"), " Lancer l'ACP"),
                                     class = "btn-success btn-lg btn-block",
                                     style = "font-weight:bold;")),
                    shiny::div(class = "alert alert-info", style = "font-size:12px;",
                        shiny::icon("info-circle"),
                        " L'ACP ne se lance qu'après ce clic. Modifier les variables annule le lancement (recliquez pour relancer). ",
                        "Les options de personnalisation du graphique se trouvent sous le graphique."),
                    shiny::hr(),
                shiny::div(id = "boxWrap_pcaResults",
                shinydashboard::box(title = shiny::tagList(shiny::icon("project-diagram"), " Résultats de l'ACP"),
                    status = "info", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    shiny::tabsetPanel(
                      id = "pcaTabs", type = "tabs",
                      shiny::tabPanel(
                        shiny::tagList(shiny::icon("chart-area"), " Graphiques"),
                        shiny::br(),
                        shiny::div(style="max-width:900px;margin:0 auto;", shiny::plotOutput("pcaPlot", height = "560px")),
                        shiny::hr(),
                                            shiny::div(style = "background-color:#eef2f5;border-left:4px solid #3c8dbc;padding:10px;margin:6px 0;",
                                                shiny::h4(style = "margin-top:0;color:#2c3e50;", shiny::icon("sliders-h"),
                                                   " Paramètres de modification du graphique"),
                                                shiny::fluidRow(
                                                  shiny::column(4,
                                                    shiny::h5(style = "color:#495057;", shiny::icon("sync-alt"), " Rotation orthogonale"),
                                                    shiny::selectInput("pcaRotationMethod", "Méthode de rotation:",
                                                                choices = c("Varimax" = "varimax", "Quartimax" = "quartimax",
                                                                            "Oblimin" = "oblimin", "Aucune" = "none"),
                                                                selected = "varimax"),
                                                    shiny::numericInput("pcaRotationNFactors", "Facteurs à rotationner:",
                                                                 value = 2, min = 2, max = 10)),
                                                  shiny::column(4,
                                                    shiny::h5(style = "color:#495057;", shiny::icon("chart-line"), " Axes représentés"),
                                                    shiny::uiOutput("pcaAxisXSelect"),
                                                    shiny::uiOutput("pcaAxisYSelect")),
                                                  shiny::column(4,
                                                    shiny::h5(style = "color:#495057;", shiny::icon("font"), " Titres & libellés"),
                                                    shiny::textInput("pcaPlotTitle", "Titre du graphique:",
                                                              value = "ACP - Analyse en Composantes Principales"),
                                                    shiny::textInput("pcaXLabel", "Label axe X:", value = ""),
                                                    shiny::textInput("pcaYLabel", "Label axe Y:", value = "")),
                                                  hstat_mv_forme_ui("pcaPlot")
                                                ),
                                                shiny::fluidRow(
                                                  shiny::column(4, shiny::checkboxInput("pcaCenterAxes", "Centrer sur (0,0)", TRUE)),
                                                  shiny::column(4, shiny::checkboxInput("pcaRoundResults", "Arrondir les résultats", value = FALSE)),
                                                  shiny::column(4, shiny::conditionalPanel(
                                                    condition = "input.pcaRoundResults == true",
                                                    shiny::numericInput("pcaDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)))
                                                ),
                                                shiny::div(style = "background-color:#f4f6f8;border-left:4px solid #6c757d;padding:10px;margin:6px 0;",
                                                  shiny::h5(style="color:#495057;margin-top:0;", shiny::icon("text-height"), " Style du texte, des points et des tracés"),
                                                  shiny::fluidRow(
                                                    shiny::column(3, shiny::sliderInput("pcaAxisTextSize", "Taille texte des axes",
                                                                          min = 8, max = 24, value = HSTAT_GG_BASE_SIZE, step = 1)),
                                                    shiny::column(3, shiny::sliderInput("pcaAxisTitleSize", "Taille titres d'axes",
                                                                          min = 8, max = 26, value = HSTAT_GG_BASE_SIZE, step = 1)),
                                                    shiny::column(3, hstat_lbl_slider("pcaLabelSize", "Taille des labels des individus")),
                                                    shiny::column(3, hstat_lbl_slider("pcaVarLabelSize", "Taille des labels des variables"))
                                                  ),
                                                  shiny::fluidRow(
                                                    shiny::column(3, shiny::sliderInput("pcaPointSize", "Taille des points (individus)",
                                                                          min = 0.5, max = 8, value = HSTAT_GG_POINT_SIZE, step = 0.5))
                                                  ),
                                                  shiny::fluidRow(
                                                    shiny::column(3, shiny::sliderInput("pcaLineWidth", "Largeur des tracés (flèches/axes)",
                                                                          min = 0.3, max = 4, value = HSTAT_GG_LINEWIDTH, step = 0.1)),
                                                    shiny::column(3, shiny::div(style="margin-top:25px;",
                                                              shiny::checkboxInput("pcaBoldText", "Texte en gras", value = FALSE))),
                                                    shiny::column(3, shiny::div(style="margin-top:25px;",
                                                              shiny::checkboxInput("pcaItalicText", "Texte en italique", value = FALSE))),
                                                    shiny::column(3, shiny::div(style="margin-top:5px;",
                                                              shiny::checkboxInput("pcaShowEllipses",
                                                                shiny::tagList(shiny::icon("draw-polygon"), " Ellipses (groupes)"),
                                                                value = FALSE)))
                                                  ),
                                                  shiny::fluidRow(
                                                    shiny::column(12, shiny::conditionalPanel(
                                                      condition = "input.pcaShowEllipses == true",
                                                      shiny::uiOutput("pcaEllipseGroupSelect")))
                                                  )
                                                ),
                                                shiny::fluidRow(
                                                  shiny::column(3, hstat_format_input("pcaPlot_format", "Format:")),
                                                  shiny::column(3, hstat_dpi_input("pcaPlot_dpi", "DPI:")),
                                                  shiny::column(3, shiny::numericInput("pcaPlot_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)),
                                                  shiny::column(3, shiny::numericInput("pcaPlot_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100))
                                                ),
                                                hstat_mv_dim_note_ui("pcaPlot"),
                                                shiny::div(style = "text-align:center;margin-top:8px;",
                                                    shiny::downloadButton("downloadPcaPlot", "Télécharger graphique", class = "btn-info", style = "margin:5px;"),
                                                    shiny::downloadButton("downloadPcaDataXlsx", "Données (Excel)", class = "btn-success", style = "margin:5px;"),
                                                    shiny::downloadButton("downloadPcaDataCsv", "Données (CSV)", class = "btn-success", style = "margin:5px;"))
                                            ),
                      ),
                      shiny::tabPanel(
                        shiny::tagList(shiny::icon("clipboard-check"), " Métriques"),
                        shiny::br(),
                        shiny::div(style = "background-color: #f8f9fa; border-left: 5px solid #495057; padding: 12px; margin: 0 0 12px 0;",
                            shiny::h4(style = "color: #343a40; font-weight: bold; margin-top: 0;",
                               shiny::icon("clipboard-check"), " Métriques de validation de l'ACP"),
                            shiny::p(style = "font-size: 12px; color: #555; margin-bottom: 0;",
                              "Évaluez ces métriques avant d'interpréter le graphique.")),
                        shiny::h5(style = "color: #2c3e50; font-weight: bold;",
                           shiny::icon("check-circle"), " Adéquation des données a l'ACP"),
                        shiny::uiOutput("pcaBartlettKMO"),
                        shiny::hr(),
                                            shiny::h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;",
                                               shiny::icon("chart-line"), " Graphique des éboulis (Scree Plot)"),
                                            shiny::p(style = "font-size: 11px; color: #666; font-style: italic;",
                                              "Critère de Kaiser (valeur propre min. 1) : les composantes en vert sont retenues. Cherchez le 'coude' de la courbe."),
                                            shiny::plotOutput("pcaScreePlot", height = "320px"),
                                            hstat_mv_forme_ui("pcaScree", "Apparence du graphique des éboulis"),
                                            shiny::fluidRow(
                                              shiny::column(12, hstat_format_input("pcaScree_format", "Format:")),
                                              shiny::column(4, hstat_dpi_input("pcaScree_dpi", "DPI:")),
                                              shiny::column(4, shiny::numericInput("pcaScree_width", "Largeur (px):", value = 2500, min = 200, max = 20000, step = 50)),
                                              shiny::column(4, shiny::numericInput("pcaScree_height", "Hauteur (px):", value = 1800, min = 200, max = 20000, step = 50))
                                            ),
                                            hstat_mv_dim_note_ui("pcaScree"),
                                            shiny::div(style = "text-align: center; margin-bottom: 10px;",
                                                shiny::downloadButton("downloadPcaScreePlot", "Télécharger Scree Plot", class = "btn-info btn-sm")
                                            ),
                    
                                            shiny::h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                                               shiny::icon("random"), " Analyse parallèle de Horn"),
                                            shiny::p(style = "font-size: 11px; color: #666; font-style: italic;",
                                              "Méthode plus rigoureuse que Kaiser : retenir les composantes dont la valeur propre observée dépasse le percentile 95 des simulations aléatoires."),
                                            shiny::plotOutput("pcaParallelPlot", height = "320px"),
                                            hstat_mv_forme_ui("pcaParallel", "Apparence de l'analyse parallèle"),
                                            shiny::fluidRow(
                                              shiny::column(12, hstat_format_input("pcaParallel_format", "Format:")),
                                              shiny::column(4, hstat_dpi_input("pcaParallel_dpi", "DPI:")),
                                              shiny::column(4, shiny::numericInput("pcaParallel_width", "Largeur (px):", value = 2500, min = 200, max = 20000, step = 50)),
                                              shiny::column(4, shiny::numericInput("pcaParallel_height", "Hauteur (px):", value = 1800, min = 200, max = 20000, step = 50))
                                            ),
                                            hstat_mv_dim_note_ui("pcaParallel"),
                                            shiny::div(style = "text-align: center; margin-bottom: 10px;",
                                                shiny::downloadButton("downloadPcaParallelPlot", "Télécharger Analyse Parallèle", class = "btn-info btn-sm")
                                            ),
                    
                                            shiny::h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                                               shiny::icon("percentage"), " Contributions absolues (CTR) des variables"),
                                            shiny::p(style = "font-size: 11px; color: #666; font-style: italic;",
                                              "Seuil théorique = 100% / nb variables. Les variables au-dessus du seuil (en vert) structurent principalement l'axe."),
                                            shiny::uiOutput("pcaCTRAxisSelect"),
                                            shiny::plotOutput("pcaCTRPlot", height = "300px"),
                                            hstat_mv_forme_ui("pcaCTR", "Apparence du graphique CTR"),
                                            shiny::fluidRow(
                                              shiny::column(12, hstat_format_input("pcaCTR_format", "Format:")),
                                              shiny::column(4, hstat_dpi_input("pcaCTR_dpi", "DPI:")),
                                              shiny::column(4, shiny::numericInput("pcaCTR_width", "Largeur (px):", value = 2500, min = 200, max = 20000, step = 50)),
                                              shiny::column(4, shiny::numericInput("pcaCTR_height", "Hauteur (px):", value = 1800, min = 200, max = 20000, step = 50))
                                            ),
                                            hstat_mv_dim_note_ui("pcaCTR"),
                                            shiny::div(style = "text-align: center; margin-bottom: 10px;",
                                                shiny::downloadButton("downloadPcaCTRPlot", "Télécharger Graphique CTR", class = "btn-info btn-sm")
                                            ),
                    
                      ),
                      shiny::tabPanel(
                        shiny::tagList(shiny::icon("cogs"), " Détails techniques"),
                        shiny::br(),
                                            shiny::h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                                               shiny::icon("sync-alt"), " Résultats de la rotation orthogonale"),
                                            shiny::p(style = "font-size: 11px; color: #666; font-style: italic;",
                                              "La rotation simplifie la structure : un loading |x| min. 0,70 indique une contribution forte, |x| de 0,40 à 0,70 une contribution modérée."),
                                            shiny::div(style = "max-height: 350px; overflow-y: auto; font-size: 11px;",
                                                shiny::verbatimTextOutput("pcaRotationResult")),
                    
                                            shiny::hr(),
                                            shiny::div(style = "max-height: 300px; overflow-y: auto; font-size: 12px;",
                                                shiny::verbatimTextOutput("pcaSummary")),
                                            shiny::hr(),
                                            # ---- Export métriques ACP 
                                            shiny::div(style = "background: linear-gradient(135deg, #343a40 0%, #495057 100%); border-radius: 10px; padding: 18px; margin-top: 10px;",
                                                shiny::h4(style = "color: white; font-weight: bold; margin-top: 0; text-align: center;",
                                                   shiny::icon("file-export"), " Export des métriques ACP"),
                                                shiny::p(style = "color: #aed6f1; font-size: 12px; text-align: center; margin-bottom: 12px;",
                                                  "Valeurs propres, Bartlett/KMO, Contributions absolues (CTR), Qualité de représentation (cos²)"),
                                                shiny::fluidRow(
                                                  shiny::column(12, style = "text-align: center;",
                                                         shiny::downloadButton("downloadPcaMetricsXlsx",
                                                                        shiny::HTML(paste0(as.character(shiny::icon("file-excel")), " <strong>Métriques ACP (Excel)</strong>")),
                                                                        class = "btn-success",
                                                                        style = "margin: 4px; padding: 7px 16px;"),
                                                         shiny::downloadButton("downloadPcaMetricsCsv",
                                                                        shiny::HTML(paste0(as.character(shiny::icon("file-csv")), " <strong>Métriques ACP (CSV)</strong>")),
                                                                        class = "btn-warning",
                                                                        style = "margin: 4px; padding: 7px 16px;")
                                                  )
                                                )
                                            )
                      )
                    )
                )
                )
                )
              ),
              
              shiny::fluidRow(
                shinydashboard::box(title = "Classification Hiérarchique sur Composantes Principales (HCPC)", 
                    status = "success", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                                        shiny::p("Cette analyse combine l'ACP avec une classification hiérarchique automatique."),
                    
                                        shiny::div(style = "margin-bottom:10px;",
                                            shiny::div(
                                              style = "cursor:pointer; background:linear-gradient(135deg,#2e7d32,#388e3c); color:white; padding:8px 12px; border-radius:6px; font-weight:bold; font-size:13px; user-select:none;",
                                              onclick = "var c=document.getElementById('hcpc-info-body'); c.style.display=(c.style.display==='none'?'block':'none');",
                                              shiny::icon("info-circle"), " Principes, objectifs & conditions de la HCPC ",
                                              shiny::icon("chevron-down")
                                            ),
                                            shiny::div(id = "hcpc-info-body", style = "display:none; background:#e8f5e9; border:1px solid #a5d6a7; border-radius:0 0 6px 6px; padding:12px; font-size:12px;",
                                                shiny::fluidRow(
                                                  shiny::column(6,
                                                         shiny::tags$b(style="color:#2e7d32;", shiny::icon("drafting-compass"), " Principes :"),
                                                         shiny::tags$p(style="margin:2px 0 6px 0; color:#333;", "Classification ascendante hiérarchique (critère de Ward) appliquée aux coordonnées ACP. Minimise la variance intra-cluster à chaque fusion. La coupure optimale est déterminée par le saut maximal des hauteurs."),
                                                         shiny::tags$b(style="color:#2e7d32;", shiny::icon("bullseye"), " Objectifs :"),
                                                         shiny::tags$p(style="margin:2px 0 0 0; color:#333;", "Identifier des groupes naturels (typologies), profiler les individus similaires, réduire les données individuelles en groupes interprétables.")
                                                  ),
                                                  shiny::column(6,
                                                         shiny::tags$b(style="color:#2e7d32;", shiny::icon("ruler"), " Taille nécessaire :"),
                                                         shiny::tags$ul(style="margin:2px 0 6px 12px; padding:0; color:#333;",
                                                                 shiny::tags$li("Minimum absolu : n", shiny::icon("arrow-right"), "2xk (k=clusters)"),
                                                                 shiny::tags$li("Recommandé : n", shiny::icon("arrow-right"), "10xk pour stabilité"),
                                                                 shiny::tags$li("Hérite des conditions de l'ACP")
                                                         ),
                                                         shiny::tags$b(style="color:#2e7d32;", shiny::icon("hashtag"), " Composantes nécessaires :"),
                                                         shiny::tags$ul(style="margin:2px 0 0 12px; padding:0; color:#333;",
                                                                 shiny::tags$li("Minimum : ", shiny::icon("arrow-right"), "1 composante ACP"),
                                                                 shiny::tags$li("Recommandé : ", shiny::icon("arrow-right"), "2 composantes retenues (", shiny::tags$em("\u03bb"), " ", shiny::icon("arrow-right"), "1)"),
                                                                 shiny::tags$li("Utilise les composantes de l'ACP précédente")
                                                         )
                                                  )
                                                ),
                                                shiny::uiOutput("hcpcConditionsCheck")
                                            )
                                        ),
                    
                                        shiny::fluidRow(
                                          shiny::column(4,
                                                 shiny::numericInput("hcpcClusters", "Nombre de clusters:", value = 3, min = 2, max = 10)
                                          ),
                                          shiny::column(4,
                                                 shiny::checkboxInput("hcpcUseMeans", shiny::tagList(shiny::icon("layer-group"), " Classer les moyennes par groupe"), value = FALSE),
                                                 shiny::conditionalPanel(
                                                   condition = "input.hcpcUseMeans == true",
                                                   shiny::uiOutput("hcpcMeansGroupSelect")
                                                 )
                                          ),
                                          shiny::column(4,
                                                 shiny::uiOutput("hcpcLabelSourceSelect")
                                          )
                                        ),
                                        shiny::fluidRow(
                                          shiny::column(12,
                                                 shiny::div(style = "text-align: center; margin-top: 10px;",
                                                     shiny::actionButton("hcpcRun", shiny::tagList(shiny::icon("play"), " Lancer la classification (HCPC)"),
                                                                  class = "btn-success", style = "margin:5px;font-weight:bold;"),
                                                     shiny::downloadButton("downloadHcpcDataXlsx", "Télécharger données (Excel)", 
                                                                    class = "btn-success", style = "margin: 5px;"),
                                                     shiny::downloadButton("downloadHcpcDataCsv", "Télécharger données (CSV)", 
                                                                    class = "btn-success", style = "margin: 5px;")
                                                 )
                                          )
                                        ),
                    
                                        shiny::div(style = "background-color: #e8f5e9; border-left: 4px solid #4caf50; padding: 10px; margin: 10px 0;",
                                            shiny::h5(style = "margin-top: 0; color: #2e7d32;", shiny::icon("chart-line"), " Sélection des axes à représenter"),
                                            shiny::fluidRow(
                                              shiny::column(6,
                                                     shiny::uiOutput("hcpcAxisXSelect")
                                              ),
                                              shiny::column(6,
                                                     shiny::uiOutput("hcpcAxisYSelect")
                                              )
                                            ),
                                            shiny::fluidRow(
                                              shiny::column(6, shiny::sliderInput("hcpcPointSize", "Taille des points", min = 0.5, max = 8, value = HSTAT_GG_POINT_SIZE, step = 0.5)),
                                              shiny::column(6, shiny::sliderInput("hcpcAxisTextSize", "Taille texte des axes", min = 8, max = 24, value = HSTAT_GG_BASE_SIZE, step = 1))
                                            ),
                                            shiny::p(style = "margin: 5px 0 0 0; font-size: 11px; color: #1b5e20; font-style: italic;",
                                              shiny::icon("info-circle"), " Les polygones colores entourent chaque cluster. Activez les étiquettes uniquement pour de petits jeux de données.")
                                        ),
                    
                                        shiny::hr(),
                                        shiny::div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin: 10px 0;",
                                            shiny::fluidRow(
                                              shiny::column(6,
                                                     shiny::checkboxInput("hcpcRoundResults", "Arrondir les résultats", value = FALSE)
                                              ),
                                              shiny::column(6,
                                                     shiny::conditionalPanel(
                                                       condition = "input.hcpcRoundResults == true",
                                                       shiny::numericInput("hcpcDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                                     )
                                              )
                                            )
                                        ),
                    shiny::hr(),
                    shiny::tabsetPanel(
                      id = "hcpcTabs", type = "tabs",
                      shiny::tabPanel(
                        shiny::tagList(shiny::icon("chart-area"), " Graphiques"),
                        shiny::br(),
                                            shiny::hr(),
                                            shiny::h5("Personnalisation graphique :", style = "font-weight: bold; color: #5cb85c;"),
                                            shiny::fluidRow(
                                              shiny::column(6,
                                                     hstat_mv_forme_ui("hcpcCluster"),
                                                     hstat_mv_forme_ui("hcpcDend", "Apparence du dendrogramme"),
                                                     hstat_mv_forme_ui("hcpcHeights", "Apparence des hauteurs de fusion"),
                                                     shiny::textInput("hcpcClusterTitle", "Titre carte des clusters:", 
                                                               value = "Carte des clusters HCPC"),
                                                     shiny::textInput("hcpcClusterXLabel", "Label axe X:", value = ""),
                                                     shiny::textInput("hcpcClusterYLabel", "Label axe Y:", value = ""),
                                                     shiny::checkboxInput("hcpcCenterAxes", "Centrer sur (0,0)", TRUE),
                                                     shiny::div(style = "background:#eafaf3; border-left:3px solid #16a085; padding:8px 12px; border-radius:0 4px 4px 0; margin:6px 0;",
                                                       shiny::checkboxInput("hcpcClusterShowLabels", shiny::tagList(shiny::icon("font"), " Afficher les labels des individus sur la carte"), value = FALSE),
                                                       hstat_lbl_slider("hcpcClusterLabelSize", "Taille des labels des individus (carte)")),
                                                     shiny::hr(),
                                                     shiny::h5("Options de téléchargement — carte des clusters :"),
                                                     shiny::p(style = "font-size: 11px; color: #5cb85c; font-style: italic;",
                                                       shiny::icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                                                     shiny::fluidRow(
                                                       shiny::column(6,
                                                              hstat_format_input("hcpcCluster_format", "Format:")
                                                       ),
                                                       shiny::column(6,
                                                              hstat_dpi_input("hcpcCluster_dpi", "DPI:")
                                                       )
                                                     ),
                                                     shiny::fluidRow(
                                                       shiny::column(6,
                                                              shiny::numericInput("hcpcCluster_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                                                       ),
                                                       shiny::column(6,
                                                              shiny::numericInput("hcpcCluster_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                                                       )
                                                     ),
                                                     hstat_mv_dim_note_ui("hcpcCluster")
                                              ),
                                              shiny::column(6,
                                                     shiny::textInput("hcpcDendTitle", "Titre dendrogramme:", 
                                                               value = "Dendrogramme HCPC"),
                                                     shiny::div(style = "background:#eafaf3; border-left:3px solid #16a085; padding:8px 12px; border-radius:0 4px 4px 0; margin:6px 0;",
                                                       shiny::sliderInput("hcpcBranchWidth", shiny::tagList(shiny::icon("grip-lines"), " Largeur des branches"),
                                                                   min = 0.1, max = 4, value = 0.5, step = 0.1),
                                                       hstat_lbl_slider("hcpcLabelSize", shiny::tagList(shiny::icon("font"), " Taille des labels (individus)")),
                                                       shiny::checkboxInput("hcpcShowLabels", "Afficher les labels des individus sur les branches", value = FALSE)),
                                                     shiny::p(style = "font-style: italic; color: #666;", 
                                                       "Le dendrogramme n'est pas centré sur (0,0)"),
                                                     shiny::hr(),
                                                     shiny::h5("Options de téléchargement — dendrogramme :"),
                                                     shiny::p(style = "font-size: 11px; color: #5cb85c; font-style: italic;",
                                                       shiny::icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                                                     shiny::fluidRow(
                                                       shiny::column(6,
                                                              hstat_format_input("hcpcDend_format", "Format:")
                                                       ),
                                                       shiny::column(6,
                                                              hstat_dpi_input("hcpcDend_dpi", "DPI:")
                                                       )
                                                     ),
                                                     shiny::fluidRow(
                                                       shiny::column(6,
                                                              shiny::numericInput("hcpcDend_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                                                       ),
                                                       shiny::column(6,
                                                              shiny::numericInput("hcpcDend_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                                                       )
                                                     ),
                                                     hstat_mv_dim_note_ui("hcpcDend")
                                              )
                                            ),
                                            shiny::hr(),
                                            shiny::fluidRow(
                                              shiny::column(12,
                                                     shiny::div(class = "box box-solid box-success",
                                                         shiny::div(class = "box-header with-border",
                                                             shiny::h4(class = "box-title", "Carte des clusters")
                                                         ),
                                                         shiny::div(class = "box-body",
                                                             shiny::div(style="max-width:850px;margin:0 auto;", shiny::plotOutput("hcpcClusterPlot", height = "520px")),
                                                             shiny::downloadButton("downloadHcpcClusterPlot", "Télécharger carte")
                                                         )
                                                     )
                                              ),
                                              shiny::column(12,
                                                     shiny::div(class = "box box-solid box-success",
                                                         shiny::div(class = "box-header with-border",
                                                             shiny::h4(class = "box-title", "Dendrogramme")
                                                         ),
                                                         shiny::div(class = "box-body",
                                                             shiny::div(style="max-width:850px;margin:0 auto;", shiny::plotOutput("hcpcDendPlot", height = "520px")),
                                                             shiny::downloadButton("downloadHcpcDendPlot", "Télécharger dendrogramme")
                                                         )
                                                     )
                                              )
                                            ),
                      ),
                      shiny::tabPanel(
                        shiny::tagList(shiny::icon("microscope"), " Métriques"),
                        shiny::br(),
                                            # ---- MÉTRIQUES DE VALIDATION HCPC 
                                            shiny::div(style = "background-color: #eafaf1; border-left: 5px solid #27ae60; padding: 12px; margin: 15px 0 10px 0;",
                                                shiny::h4(style = "color: #1e8449; font-weight: bold; margin-top: 0;",
                                                   shiny::icon("microscope"), " Métriques de validation de la classification"),
                                                shiny::p(style = "font-size: 12px; color: #555; margin-bottom: 0;",
                                                  "Les métriques suivantes permettent d'évaluer la qualité et la robustesse de la partition obtenue. Elles doivent être analysées dans l'ordre présenté.")
                                            ),
                    
                                            # 1. Hauteurs de fusion
                                            shiny::div(class = "box box-solid",
                                                shiny::div(class = "box-header with-border", style = "background-color: #2980b9; color: white;",
                                                    shiny::h4(class = "box-title", style = "color: white;",
                                                       shiny::icon("chart-area"), " Graphique des hauteurs de fusion")
                                                ),
                                                shiny::div(class = "box-body",
                                                    shiny::p(style = "font-size: 12px; color: #555; font-style: italic;",
                                                      "Un saut important entre deux fusions consécutives suggère la coupure optimale du dendrogramme (règle du coude). Ce graphique complète la lecture visuelle du dendrogramme."),
                                                    shiny::plotOutput("hcpcHeightsPlot", height = "320px"),
                                                    shiny::fluidRow(
                                                      shiny::column(12, hstat_format_input("hcpcHeights_format", "Format:")),
                                                      shiny::column(4, hstat_dpi_input("hcpcHeights_dpi", "DPI:")),
                                              shiny::column(4, shiny::numericInput("hcpcHeights_width", "Largeur (px):", value = 2500, min = 200, max = 20000, step = 50)),
                                              shiny::column(4, shiny::numericInput("hcpcHeights_height", "Hauteur (px):", value = 1800, min = 200, max = 20000, step = 50))
                                                    ),
                                                    hstat_mv_dim_note_ui("hcpcHeights"),
                                                    shiny::div(style = "text-align: center; margin-top: 4px;",
                                                        shiny::downloadButton("downloadHcpcHeightsPlot",
                                                                       "Télécharger hauteurs de fusion",
                                                                       class = "btn-info btn-sm")
                                                    )
                                                )
                                            ),
                    
                                            # 2. CH, DB, Silhouette, Cophénétique
                                            shiny::div(class = "box box-solid",
                                                shiny::div(class = "box-header with-border", style = "background-color: #e67e22; color: white;",
                                                    shiny::h4(class = "box-title", style = "color: white;",
                                                       shiny::icon("ruler-combined"), " Indices de validation des clusters")
                                                ),
                                                shiny::div(class = "box-body",
                                                    shiny::p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 12px;",
                                                      "Ces quatre indices évaluent respectivement la séparation inter-classes (CH), la compacité relative (DB), la cohérence individuelle (Silhouette) et la fidélité du dendrogramme (Cophénétique)."),
                                                    shiny::uiOutput("hcpcMetricsUI")
                                                )
                                            ),
                    
                                            shiny::div(class = "box box-solid",
                                                shiny::div(class = "box-header with-border", style = "background-color: #16a085; color: white;",
                                                    shiny::h4(class = "box-title", style = "color: white;",
                                                       shiny::icon("shield-alt"), " Stabilité par sous-échantillonnage")
                                                ),
                                                shiny::div(class = "box-body",
                                                    shiny::p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 12px;",
                                                      "Évalue si la structure de clusters est reproductible sur des sous-échantillons aléatoires des données. Un indice de Rand proche de 1 confirme la robustesse de la partition."),
                                                    shiny::uiOutput("hcpcStabilityUI")
                                                )
                                            ),
                      ),
                      shiny::tabPanel(
                        shiny::tagList(shiny::icon("cogs"), " Détails techniques"),
                        shiny::br(),
                                            shiny::div(class = "box box-solid",
                                                shiny::div(class = "box-header with-border", style = "background-color: #5cb85c; color: white;",
                                                    shiny::h4(class = "box-title", "Résultats détaillés HCPC", style = "color: white; font-weight: bold;")
                                                ),
                                                shiny::div(class = "box-body", style = "background-color: #f9f9f9;",
                                                    shiny::div(style = "max-height: 500px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                                        shiny::verbatimTextOutput("hcpcSummary"))
                                                )
                                            ),
                                            # ---- Export métriques HCPC 
                                            shiny::div(style = "background: linear-gradient(135deg, #0e6655 0%, #117a65 100%); border-radius: 10px; padding: 18px; margin-top: 20px;",
                                                shiny::h4(style = "color: white; font-weight: bold; margin-top: 0; text-align: center;",
                                                   shiny::icon("file-export"), " Export des métriques HCPC / CAH"),
                                                shiny::p(style = "color: #a9dfbf; font-size: 12px; text-align: center; margin-bottom: 12px;",
                                                  "Indices CH, Davies-Bouldin, Silhouette, corrélation cophénétique, affectation des individus aux clusters"),
                                                shiny::fluidRow(
                                                  shiny::column(12, style = "text-align: center;",
                                                         shiny::downloadButton("downloadHcpcMetricsXlsx",
                                                                        shiny::HTML(paste0(as.character(shiny::icon("file-excel")), " <strong>Métriques HCPC (Excel)</strong>")),
                                                                        class = "btn-success",
                                                                        style = "margin: 4px; padding: 7px 16px;"),
                                                         shiny::downloadButton("downloadHcpcMetricsCsv",
                                                                        shiny::HTML(paste0(as.character(shiny::icon("file-csv")), " <strong>Métriques HCPC (CSV)</strong>")),
                                                                        class = "btn-warning",
                                                                        style = "margin: 4px; padding: 7px 16px;")
                                                  )
                                                )
                                            )
                      )
                    )
                )
              ),
              
              shiny::fluidRow(
                shinydashboard::box(title = "Analyse Factorielle Discriminante (AFD)", 
                    status = "primary", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    
                                        shiny::div(style = "margin-bottom:12px;",
                                            shiny::div(
                                              style = "cursor:pointer; background:linear-gradient(135deg,#1a237e,#283593); color:white; padding:8px 12px; border-radius:6px; font-weight:bold; font-size:13px; user-select:none;",
                                              onclick = "var c=document.getElementById('afd-info-body'); c.style.display=(c.style.display==='none'?'block':'none');",
                                              shiny::icon("info-circle"), " Principes, objectifs & conditions de l'AFD ",
                                              shiny::icon("chevron-down")
                                            ),
                                            shiny::div(id = "afd-info-body", style = "display:none; background:#e8eaf6; border:1px solid #9fa8da; border-radius:0 0 6px 6px; padding:12px; font-size:12px;",
                                                shiny::fluidRow(
                                                  shiny::column(6,
                                                         shiny::tags$b(style="color:#1a237e;", shiny::icon("drafting-compass"), " Principes :"),
                                                         shiny::tags$p(style="margin:2px 0 6px 0; color:#333;", "Recherche les combinaisons linéaires de variables (fonctions discriminantes) maximisant le ratio variance inter-groupes / variance intra-groupes (critère de Fisher-Rao). Généralisation multivariée de l'ANOVA."),
                                                         shiny::tags$b(style="color:#1a237e;", shiny::icon("bullseye"), " Objectifs :"),
                                                         shiny::tags$p(style="margin:2px 0 0 0; color:#333;", "Discriminer des individus dans des groupes prédéfinis, identifier les variables les plus discriminantes, prédire l'appartenance à un groupe pour de nouveaux individus.")
                                                  ),
                                                  shiny::column(6,
                                                         shiny::tags$b(style="color:#1a237e;", shiny::icon("ruler"), " Taille nécessaire :"),
                                                         shiny::tags$ul(style="margin:2px 0 6px 12px; padding:0; color:#333;",
                                                                 shiny::tags$li("Minimum absolu : n > p + g - 1"),
                                                                 shiny::tags$li("Recommandé : min. 20 obs. par groupe"),
                                                                 shiny::tags$li("Idéal : ratio n/p min. 10"),
                                                                 shiny::tags$li(shiny::tags$em("p = nb variables, g = nb groupes"))
                                                         ),
                                                         shiny::tags$b(style="color:#1a237e;", shiny::icon("hashtag"), " Variables minimales :"),
                                                         shiny::tags$ul(style="margin:2px 0 0 12px; padding:0; color:#333;",
                                                                 shiny::tags$li("Minimum absolu : p min. 1"),
                                                                 shiny::tags$li("Recommandé : p min. 2"),
                                                                 shiny::tags$li("Maximum : p < n - g"),
                                                                 shiny::tags$li("Groupes minimum : g min. 2")
                                                         )
                                                  )
                                                ),
                                                shiny::uiOutput("afdConditionsCheck")
                                            )
                                        ),
                    
                                        shiny::div(style = "background-color: #f8f9fa; border-left: 4px solid #343a40; padding: 15px; margin-bottom: 15px;",
                                            shiny::h4(style = "color: #343a40; margin-top: 0;",
                                               shiny::icon("bullseye"), " Variable à discriminer (OBLIGATOIRE)"),
                                            shiny::p(style = "margin: 5px 0; font-size: 13px; color: #555;",
                                              "Sélectionnez la variable catégorielle que vous souhaitez discriminer (prédire). Cette variable doit contenir au moins 2 groupes différents."),
                                            shiny::uiOutput("afdFactorSelect"),
                                            shiny::p(style = "margin: 5px 0 0 0; font-size: 11px; color: #e74c3c; font-weight: bold;",
                                              shiny::icon("exclamation-circle"), " Si aucune variable n'apparaît, vérifiez que vos données contiennent des variables catégorielles (facteurs).")
                                        ),
                    
                                        shiny::div(style = "background-color: #f8f9fa; border-left: 4px solid #495057; padding: 15px; margin-bottom: 15px;",
                                            shiny::h4(style = "color: #495057; margin-top: 0;",
                                               shiny::icon("chart-line"), " Variables quantitatives (OBLIGATOIRE)"),
                                            shiny::p(style = "margin: 5px 0; font-size: 13px; color: #555;",
                                              "Sélectionnez les variables numériques qui serviront à discriminer les groupes. Plus il y a de variables pertinentes, meilleure sera la discrimination."),
                                            shiny::uiOutput("afdVarSelect"),
                                            shiny::p(style = "margin: 5px 0 0 0; font-size: 11px; color: #27ae60; font-style: italic;",
                                              shiny::icon("check-circle"), " Conseil : Sélectionnez au moins 2-3 variables pour obtenir de bons résultats.")
                                        ),
                    
                                        shiny::uiOutput("afdCollinearityPanel"),
                    
                                        shiny::hr(),
                                        shiny::h4(style = "color: #6c757d; margin-top: 10px;", shiny::icon("cogs"), " Options avancées (optionnel)"),
                                        shiny::checkboxInput("afdUseMeans", "Utiliser les moyennes par groupe", FALSE),
                                        shiny::conditionalPanel(
                                          condition = "input.afdUseMeans == true",
                                          shiny::uiOutput("afdMeansGroupSelect"),
                                          shiny::p(style = "margin: 5px 0 10px 0; font-size: 11px; color: #6c757d;",
                                            shiny::icon("lightbulb"), 
                                            " Conseil: Utilisez la même variable que le facteur de discrimination pour une AFD sur moyennes de groupes."),
                                          shiny::actionButton("afdRefresh", "Actualiser l'AFD", 
                                                       icon = shiny::icon("sync"), 
                                                       class = "btn-info btn-sm",
                                                       style = "margin-bottom: 10px;")
                                        ),
                    
                                        shiny::div(style = "text-align:center;margin:12px 0;",
                                            shiny::actionButton("afdRun", shiny::tagList(shiny::icon("play"), " Lancer l'AFD"),
                                                         class = "btn-success btn-lg btn-block",
                                                         style = "font-weight:bold;")),
                                        shiny::div(class = "alert alert-info", style = "font-size:12px;",
                                            shiny::icon("info-circle"),
                                            " L'AFD ne se lance qu'après ce clic. Modifier les variables ou le facteur annule le lancement (recliquez pour relancer)."),
                    
                                        shiny::div(style = "background-color: #e3f2fd; border-left: 4px solid #2196f3; padding: 10px; margin: 10px 0;",
                                            shiny::h5(style = "margin-top: 0; color: #495057;", shiny::icon("chart-line"), " Sélection des axes à représenter"),
                                            shiny::fluidRow(
                                              shiny::column(6,
                                                     shiny::uiOutput("afdAxisXSelect")
                                              ),
                                              shiny::column(6,
                                                     shiny::uiOutput("afdAxisYSelect")
                                              )
                                            ),
                                            shiny::p(style = "margin: 5px 0 0 0; font-size: 11px; color: #495057; font-style: italic;",
                                              shiny::icon("info-circle"), " Choisissez les fonctions discriminantes à afficher")
                                        ),
                    
                                        shiny::uiOutput("afdPredictVarsSelect"),
                                        shiny::div(style = "background-color: #d1ecf1; border-left: 4px solid #17a2b8; padding: 10px; margin: 10px 0;",
                                            shiny::p(style = "margin: 0; font-size: 12px; color: #0c5460;",
                                              shiny::icon("info-circle"), 
                                              shiny::HTML(" <strong>Variables de prédiction:</strong> Sélectionnez des variables catégorielles supplémentaires pour enrichir la prédiction du modèle."))
                                        ),
                                        shiny::uiOutput("afdQualiSupSelect"),
                                        shiny::conditionalPanel(
                                          condition = "input.afdUseMeans == false || input.afdUseMeans == null",
                                          shiny::div(style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin: 15px 0;",
                                              shiny::checkboxInput("afdCrossValidation", 
                                                            shiny::HTML("<strong>Activer la validation croisée (Leave-One-Out)</strong>"), 
                                                            FALSE),
                                              shiny::p(style = "margin: 5px 0 0 25px; font-size: 12px; color: #856404;",
                                                shiny::icon("exclamation-triangle"), 
                                                " ATTENTION: La validation croisée peut être très longue sur de grands jeux de données.")
                                          )
                                        ),
                                        shiny::conditionalPanel(
                                          condition = "input.afdUseMeans == true",
                                          shiny::div(style = "background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 15px 0;",
                                              shiny::p(style = "margin: 0; font-size: 12px; color: #721c24;",
                                                shiny::icon("info-circle"), 
                                                shiny::HTML(" <strong>Note:</strong> La validation croisée Leave-One-Out n'est pas disponible avec les moyennes par groupe (nombre d'observations insuffisant)."))
                                          )
                                        ),
                                        shiny::hr(),
                                        shiny::div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin: 10px 0;",
                                            shiny::fluidRow(
                                              shiny::column(6,
                                                     shiny::checkboxInput("afdRoundResults", "Arrondir les résultats", value = FALSE)
                                              ),
                                              shiny::column(6,
                                                     shiny::conditionalPanel(
                                                       condition = "input.afdRoundResults == true",
                                                       shiny::numericInput("afdDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                                     )
                                              )
                                            )
                                        ),
                    shiny::hr(),
                    shiny::tabsetPanel(
                      id = "afdTabs", type = "tabs",
                      shiny::tabPanel(
                        shiny::tagList(shiny::icon("chart-area"), " Graphiques"),
                        shiny::br(),
                                            shiny::div(style="background-color:#f4f6f8;border-left:4px solid #3c8dbc;padding:10px;margin-bottom:10px;",
                                              shiny::h5(style="margin-top:0;color:#495057;", shiny::icon("sliders-h"), " Taille des éléments"),
                                              shiny::fluidRow(
                                                shiny::column(3, shiny::sliderInput("afdPointSize", "Taille des points", min = 0.5, max = 8, value = HSTAT_GG_POINT_SIZE, step = 0.5)),
                                                shiny::column(3, hstat_lbl_slider("afdLabelSize", "Taille des labels des individus")),
                                                shiny::column(3, hstat_lbl_slider("afdVarLabelSize", "Taille des labels des variables")),
                                                shiny::column(3, shiny::sliderInput("afdLineWidth", "Largeur des flèches", min = 0.3, max = 4, value = HSTAT_GG_LINEWIDTH, step = 0.1))
                                              ),
                                              shiny::fluidRow(
                                                shiny::column(3, shiny::sliderInput("afdAxisTextSize", "Taille texte axes", min = 8, max = 22, value = HSTAT_GG_BASE_SIZE, step = 1))
                                              )
                                            ),
                                            shiny::h5("Personnalisation graphique :", style = "font-weight: bold; color: #495057;"),
                                            shiny::fluidRow(
                                              shiny::column(6,
                                                     hstat_mv_forme_ui("afdInd", "Apparence — projection des individus"),
                                                     hstat_mv_forme_ui("afdVar", "Apparence — contribution des variables"),
                                                     shiny::textInput("afdIndTitle", "Titre projection individus:", 
                                                               value = "AFD - Projection des individus"),
                                                     shiny::textInput("afdIndXLabel", "Label axe X:", value = ""),
                                                     shiny::textInput("afdIndYLabel", "Label axe Y:", value = ""),
                                                     shiny::checkboxInput("afdIndCenterAxes", "Centrer sur (0,0)", TRUE),
                                                     shiny::hr(),
                                                     shiny::h5("Options de téléchargement — projection des individus :"),
                                                     shiny::p(style = "font-size: 11px; color: #495057; font-style: italic;",
                                                       shiny::icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                                                     shiny::fluidRow(
                                                       shiny::column(6,
                                                              hstat_format_input("afdInd_format", "Format:")
                                                       ),
                                                       shiny::column(6,
                                                              hstat_dpi_input("afdInd_dpi", "DPI:")
                                                       )
                                                     ),
                                                     shiny::fluidRow(
                                                       shiny::column(6,
                                                              shiny::numericInput("afdInd_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                                                       ),
                                                       shiny::column(6,
                                                              shiny::numericInput("afdInd_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                                                       )
                                                     ),
                                                     hstat_mv_dim_note_ui("afdInd")
                                              ),
                                              shiny::column(6,
                                                     shiny::textInput("afdVarTitle", "Titre contribution variables:", 
                                                               value = "AFD - Contribution des variables"),
                                                     shiny::textInput("afdVarXLabel", "Label axe X:", value = ""),
                                                     shiny::textInput("afdVarYLabel", "Label axe Y:", value = ""),
                                                     shiny::checkboxInput("afdVarCenterAxes", "Centrer sur (0,0)", TRUE),
                                                     shiny::hr(),
                                                     shiny::h5("Options de téléchargement — contribution des variables :"),
                                                     shiny::p(style = "font-size: 11px; color: #495057; font-style: italic;",
                                                       shiny::icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                                                     shiny::fluidRow(
                                                       shiny::column(6,
                                                              hstat_format_input("afdVar_format", "Format:")
                                                       ),
                                                       shiny::column(6,
                                                              hstat_dpi_input("afdVar_dpi", "DPI:")
                                                       )
                                                     ),
                                                     shiny::fluidRow(
                                                       shiny::column(6,
                                                              shiny::numericInput("afdVar_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                                                       ),
                                                       shiny::column(6,
                                                              shiny::numericInput("afdVar_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                                                       )
                                                     ),
                                                     hstat_mv_dim_note_ui("afdVar")
                                              )
                                            ),
                                            shiny::hr(),
                                            shiny::div(style = "text-align: center;",
                                                shiny::downloadButton("downloadAfdDataXlsx", "Télécharger données (Excel)", 
                                                               class = "btn-success", style = "margin: 5px;"),
                                                shiny::downloadButton("downloadAfdDataCsv", "Télécharger données (CSV)", 
                                                               class = "btn-success", style = "margin: 5px;")
                                            ),
                                            shiny::hr(),
                                            shiny::fluidRow(
                                              shiny::column(12,
                                                     shiny::div(class = "box box-solid box-primary",
                                                         shiny::div(class = "box-header with-border",
                                                             shiny::h4(class = "box-title", "Projection des individus", style = "color: #fff;")
                                                         ),
                                                         shiny::div(class = "box-body",
                                                             shiny::div(style="max-width:850px;margin:0 auto;", shiny::plotOutput("afdIndPlot", height = "520px")),
                                                             shiny::downloadButton("downloadAfdIndPlot", "Télécharger projection")
                                                         )
                                                     )
                                              ),
                                              shiny::column(12,
                                                     shiny::div(class = "box box-solid box-primary",
                                                         shiny::div(class = "box-header with-border",
                                                             shiny::h4(class = "box-title", "Contribution des variables", style = "color: #fff;")
                                                         ),
                                                         shiny::div(class = "box-body",
                                                             shiny::div(style="max-width:850px;margin:0 auto;", shiny::plotOutput("afdVarPlot", height = "520px")),
                                                             shiny::downloadButton("downloadAfdVarPlot", "Télécharger contribution")
                                                         )
                                                     )
                                              )
                                            ),
                      ),
                      shiny::tabPanel(
                        shiny::tagList(shiny::icon("microscope"), " Métriques"),
                        shiny::br(),
                                            shiny::div(class = "box box-solid",
                                                shiny::div(class = "box-header with-border", style = "background-color: #d9534f; color: white;",
                                                    shiny::h4(class = "box-title", "Résultats détaillés de l'AFD", style = "color: white; font-weight: bold;")
                                                ),
                                                shiny::div(class = "box-body", style = "background-color: #f9f9f9;",
                                                    shiny::div(style = "background-color: #fdf2f8; border-left: 5px solid #d9534f; padding: 12px; margin-bottom: 15px;",
                                                        shiny::h5(style = "color: #922b21; font-weight: bold; margin-top: 0;",
                                                           shiny::icon("microscope"), " Métriques de validation de la classification"),
                                                        shiny::p(style = "font-size: 12px; color: #555; margin-bottom: 0;",
                                                          "Les métriques suivantes permettent d'évaluer la qualité et la robustesse de la partition obtenue. Elles doivent être analysées dans l'ordre présenté.")
                                                    ),
                                                    shiny::div(style = "max-height: 700px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                                        shiny::uiOutput("afdSummary"))
                                                )
                                            ),
                      ),
                      shiny::tabPanel(
                        shiny::tagList(shiny::icon("cogs"), " Détails techniques"),
                        shiny::br(),
                                            # ---- Export métriques AFD 
                                            shiny::div(style = "background: linear-gradient(135deg, #641e16 0%, #922b21 100%); border-radius: 10px; padding: 18px; margin-top: 10px;",
                                                shiny::h4(style = "color: white; font-weight: bold; margin-top: 0; text-align: center;",
                                                   shiny::icon("file-export"), " Export des métriques AFD"),
                                                shiny::p(style = "color: #f1948a; font-size: 12px; text-align: center; margin-bottom: 12px;",
                                                  "Variance expliquée, corrélations canoniques, eta², accuracy, Kappa de Cohen, matrice de confusion, taux par groupe"),
                                                shiny::fluidRow(
                                                  shiny::column(12, style = "text-align: center;",
                                                         shiny::downloadButton("downloadAfdMetricsXlsx",
                                                                        shiny::HTML(paste0(as.character(shiny::icon("file-excel")), " <strong>Métriques AFD (Excel)</strong>")),
                                                                        class = "btn-success",
                                                                        style = "margin: 4px; padding: 7px 16px;"),
                                                         shiny::downloadButton("downloadAfdMetricsCsv",
                                                                        shiny::HTML(paste0(as.character(shiny::icon("file-csv")), " <strong>Métriques AFD (CSV)</strong>")),
                                                                        class = "btn-warning",
                                                                        style = "margin: 4px; padding: 7px 16px;")
                                                  )
                                                )
                                            )
                      )
                    )
                )
              ),

              # ---- Analyses multivariees etendues ----
  shiny::div(
    style = "margin-top:18px;",

    # -- Selecteur de categorie MASQUE (le catalogue pilote 'mv_category') --
    # On conserve l'input radioGroupButtons (invisible) car le catalogue JS l'utilise
    # pour basculer la categorie affichee ; le bandeau visuel est supprime (redondant
    # avec le catalogue facon maquette).
    shiny::tags$div(style = "display:none;",
      radioGroupButtons(
        inputId = "mv_category",
        label = NULL,
        choices = c(
          "Quantitatives" = "quanti",
          "Qualitatives / catégorielles" = "quali",
          "Mixtes (quanti + quali)" = "mixte"
        ),
        selected = "quanti"
      )
    ),
    # Les options d'affichage des graphiques sont desormais propres a CHAQUE
    # methode (boite "Options d'affichage des graphiques (optionnel)" rendue dans
    # les controles de chaque analyse). On ne duplique donc plus de boite globale
    # partagee ici, pour eviter le doublon.

    # =================== CATEGORIE QUANTITATIVES ===================
    shiny::conditionalPanel(
      condition = "input.mv_category == 'quanti'",
      .mv_category_header("Analyses multivariées QUANTITATIVES",
                          "ruler-combined", "#3c8dbc"),
      shiny::fluidRow(.mv_analysis_box(
        "kmeans", "Classification k-means (partitionnement)", "quanti",
        principes  = "Partitionne n individus en k groupes en minimisant itérativement l'inertie intra-classe (somme des carres aux centroides). Algorithme de Lloyd/Hartigan-Wong.",
        objectifs  = "Construire une typologie d'individus, segmenter une population, identifier des profils homogènes sur variables quantitatives.",
        taille     = c("Minimum : n &ge; 2&times;k", "Recommande : n &ge; 10&times;k",
                       "Idéal : n &ge; 30&times;k pour des centroides stables"),
        variables  = c("Minimum : p &ge; 2 variables numériques", "Recommande : p &ge; 3",
                       "Standardisation conseillée si échelles hétérogènes"),
        intro = "Partitionnement non hiérarchique : le nombre de clusters est fixe a priori."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "efa", "Analyse Factorielle Exploratoire (AFE)", "quanti",
        principes  = "Modèle a facteurs communs séparant variance commune et variance spécifique. Extraction (ML, axes principaux) puis rotation (varimax/oblimin) pour simplifier la structure.",
        objectifs  = "Découvrir les facteurs latents sous-jacents a un ensemble de variables, valider la structure d'un questionnaire, réduire la dimension.",
        taille     = c("Minimum : n &ge; 5&times;p", "Recommande : n &ge; 100",
                       "Idéal : n &ge; 200 et &ge; 10 individus / variable"),
        variables  = c("Minimum : p &ge; 3 variables numériques", "KMO &ge; 0,60 requis",
                       "Test de Bartlett significatif (p &lt; 0,05)"),
        intro = "Cherche une structure latente sans hypothèse imposée (exploratoire)."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "cfa", "Analyse Factorielle Confirmatoire (AFC-c)", "quanti",
        principes  = "Modèle d'equations structurelles : la structure facteurs <-> items est imposee a priori, puis estimée et evaluee par des indices d'ajustement.",
        objectifs  = "Tester un modèle de mesure théorique, confirmer la validité convergente et discriminante d'un instrument.",
        taille     = c("Minimum : n &ge; 100", "Recommande : n &ge; 200",
                       "Idéal : &ge; 10 individus par paramètre estimé"),
        variables  = c("Modèle spécifie en syntaxe lavaan", "&ge; 3 indicateurs par facteur conseille",
                       "Variables numériques (estimateurs robustes sinon)"),
        intro = "Confirme un modèle de mesure pré-spécifie. Renseignez la syntaxe du modèle."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "mtmm", "Multi-Trait Multi-Method (MTMM)", "quanti",
        principes  = "Matrice de corrélations organisée en blocs traits &times; méthodes (Campbell &amp; Fiske, 1959). La diagonale de validité (même trait, méthodes différentes) est confrontée aux triangles hétérotrait-monométhode et hétérotrait-hétérométhode.",
        objectifs  = "Établir la validité convergente (un même trait mesuré par plusieurs méthodes converge) et discriminante (des traits distincts restent distincts), et quantifier les effets de méthode.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Idéal : n &ge; 200 pour des corrélations stables"),
        variables  = c("&ge; 2 traits &times; &ge; 2 méthodes", "1 variable numérique par combinaison trait &times; méthode",
                       "Nommage Trait_Méthode conseillé pour l'affectation automatique"),
        intro = "Chaque variable mesure UN trait par UNE méthode. Affectez trait et méthode a chaque variable (ou utilisez le nommage Trait_Méthode)."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "pls", "Régression PLS / PLS-DA", "quanti",
        principes  = "Construit des composantes latentes maximisant la covariance entre les prédicteurs X et la réponse Y. Adaptée aux cas p &gt;&gt; n et forte multicolinéarité.",
        objectifs  = "Prédire une réponse (quantitative = PLS, catégorielle = PLS-DA) en grande dimension, identifier les variables influentes (VIP).",
        taille     = c("Fonctionne même si n &lt; p", "Recommande : n &ge; 20",
                       "Validation croisée conseillée"),
        variables  = c("1 variable réponse Y", "p &ge; 2 prédicteurs numériques X",
                       "Prédicteurs corrélés : aucun problème"),
        intro = "Régression sur composantes latentes, robuste a la colinéarité."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "regmult", "Régression linéaire multiple", "quanti",
        principes  = "Estimé par moindres carres ordinaires une réponse quantitative comme combinaison linéaire de plusieurs prédicteurs.",
        objectifs  = "Expliquer et prédire une variable continue, quantifier l'effet de chaque prédicteur, contrôler des facteurs de confusion.",
        taille     = c("Minimum : n &ge; 10&times;p", "Recommande : n &ge; 15&times;p",
                       "Idéal : n &ge; 20&times;p"),
        variables  = c("1 réponse Y numérique", "p &ge; 1 prédicteur (numérique ou facteur)",
                       "Résidus : normalité, homoscedasticite, indépendance"),
        intro = "Modèle explicatif/prédictif de référence pour une réponse continue."
      ))
    ),

    # =================== CATEGORIE QUALITATIVES ===================
    shiny::conditionalPanel(
      condition = "input.mv_category == 'quali'",
      .mv_category_header("Analyses multivariées QUALITATIVES / CATÉGORIELLES",
                          "shapes", "#e67e22"),
      shiny::fluidRow(.mv_analysis_box(
        "afc", "Analyse Factorielle des Correspondances (AFC)", "quali",
        principes  = "Décompose l'inertie du khi-deux d'une table de contingence ; compare les profils-lignes et profils-colonnes via la distance du khi-deux.",
        objectifs  = "Analyser et visualiser l'association entre DEUX variables qualitatives, repérer les modalités attractives ou répulsives.",
        taille     = c("Effectifs théoriques &ge; 5 par case conseille",
                       "Recommande : n &ge; 50", "Éviter cases vides"),
        variables  = c("Exactement 2 variables qualitatives", "Variable-ligne + variable-colonne",
                       "Modalités a effectif suffisant"),
        intro = "Association entre deux variables catégorielles (table croisée)."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "mca", "Analyse des Correspondances Multiples (ACM)", "quali",
        principes  = "Généralise l'AFC a plus de deux variables qualitatives via le tableau disjonctif complet (ou tableau de Burt).",
        objectifs  = "Explorer la structure d'associations entre plusieurs variables qualitatives, positionner individus et modalités.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Regrouper les modalités rares (&lt; 5 %)"),
        variables  = c("Minimum : p &ge; 2 variables qualitatives", "Recommande : p &ge; 3",
                       "Variables nominales"),
        intro = "Structure d'associations entre plusieurs variables catégorielles."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "kmodes", "Classification k-modes (partitionnement)", "quali",
        principes  = "Équivalent du k-means pour données qualitatives : dissimilarité d'appariement simple (Hamming), les centres sont des modes.",
        objectifs  = "Segmenter une population décrite par des variables catégorielles, construire une typologie qualitative.",
        taille     = c("Minimum : n &ge; 2&times;k", "Recommande : n &ge; 10&times;k",
                       "Idéal : n &ge; 30&times;k"),
        variables  = c("Minimum : p &ge; 2 variables qualitatives", "Recommande : p &ge; 3",
                       "Modalités a effectif suffisant"),
        intro = "Partitionnement non hiérarchique pour variables catégorielles."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "lca", "Analyse en Classes Latentes (LCA)", "quali",
        principes  = "Modèle de mélange probabiliste : sous hypothèse d'indépendance locale conditionnelle, estimé des classes latentes par maximum de vraisemblance (EM).",
        objectifs  = "Identifier des sous-populations non observées a partir de variables catégorielles, clustering base sur un modèle.",
        taille     = c("Minimum : n &ge; 100", "Recommande : n &ge; 300",
                       "Plus de classes => plus d'effectif"),
        variables  = c("Minimum : p &ge; 3 variables qualitatives", "Variables catégorielles",
                       "Indépendance locale conditionnelle"),
        intro = "Clustering probabiliste : classes latentes derrière des réponses catégorielles."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "logit", "Régression logistique / multinomiale", "quali",
        principes  = "Modèle linéaire généralise a lien logit estimé par maximum de vraisemblance ; produit des rapports de cotes (odds ratios).",
        objectifs  = "Prédire une réponse catégorielle (binaire ou multinomiale), quantifier l'effet des prédicteurs.",
        taille     = c("Règle : &ge; 10 événements par prédicteur",
                       "Recommande : n &ge; 100", "Éviter la séparation parfaite"),
        variables  = c("1 réponse Y catégorielle", "p &ge; 1 prédicteur (numérique ou facteur)",
                       "Indépendance des observations"),
        intro = "Modèle explicatif/prédictif pour une réponse catégorielle."
      ))
    ),

    # =================== CATEGORIE MIXTES ===================
    shiny::conditionalPanel(
      condition = "input.mv_category == 'mixte'",
      .mv_category_header("Analyses multivariées MIXTES (quanti + quali)",
                          "layer-group", "#00a65a"),
      shiny::fluidRow(.mv_analysis_box(
        "famd", "Analyse Factorielle de Données Mixtes (AFDM)", "mixte",
        principes  = "Combine ACP (variables quantitatives standardisées) et ACM (variables qualitatives), avec une pondération équilibrant les deux types.",
        objectifs  = "Réduire la dimension d'un tableau mêlant variables quantitatives et qualitatives, visualiser individus et modalités.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Idéal : n &ge; 5&times;p"),
        variables  = c("Au moins 1 variable quantitative", "Au moins 1 variable qualitative",
                       "p &ge; 3 au total conseille"),
        intro = "Réduction de dimension pour un tableau de variables mixtes."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "mfa", "Analyse Factorielle Multiple (AFM)", "mixte",
        principes  = "Analyse des données structurées en groupes de variables ; chaque groupe est équilibre par sa première valeur propre afin qu'aucun ne domine.",
        objectifs  = "Comparer et intégrer plusieurs groupes de variables (bloc quantitatif et bloc qualitatif), étudier leur cohérence.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Idéal : n &ge; 5&times;p"),
        variables  = c("Bloc quantitatif : &ge; 1 variable", "Bloc qualitatif : &ge; 1 variable",
                       "Définir explicitement les deux blocs"),
        intro = "Intégration de blocs de variables (bloc quanti + bloc quali)."
      )),
      shiny::fluidRow(.mv_analysis_box(
        "kproto", "Classification k-prototypes (partitionnement mixte)", "mixte",
        principes  = "Combine k-means (distance euclidienne sur le quantitatif) et k-modes (appariement sur le qualitatif), pondérés par un paramètre gamma.",
        objectifs  = "Segmenter une population décrite par des variables a la fois quantitatives et qualitatives.",
        taille     = c("Minimum : n &ge; 2&times;k", "Recommande : n &ge; 10&times;k",
                       "Idéal : n &ge; 30&times;k"),
        variables  = c("Au moins 1 variable quantitative", "Au moins 1 variable qualitative",
                       "Standardisation du quantitatif appliquée"),
        intro = "Partitionnement non hiérarchique pour données mixtes."
      ))
    )
  )
                )  # ferme mv-analyses-col
              )    # ferme mv-layout
      ),
      # ---- 4. Planification & outils : Plan & Puissance -> Seuils ----
      mod_design_ui("design"),
      # ---- Analyses qualitatives d'enquete ----
      mod_qualitative_ui("qualitative"),

      # ---- Interpretation des resultats et aide a la decision ----
      mod_ai_ui("aidecision"),
      # ---- Seuils d'efficacité ----
      shinydashboard::tabItem(tabName = "timeseries",
              mod_timeseries_ui("timeseries")
      ),
      shinydashboard::tabItem(tabName = "ml",
              mod_ml_ui("ml")
      ),
      shinydashboard::tabItem(tabName = "dl",
              mod_dl_ui("dl")
      ),
      shinydashboard::tabItem(tabName = "threshold",
              mod_threshold_ui("threshold")
      ),

      # ---- Citer HStat ----
      shinydashboard::tabItem(tabName = "cite",
        shiny::fluidRow(
          shinydashboard::box(
            title = shiny::tagList(shiny::icon("quote-right"), " Citer HStat"),
            status = "primary", width = 12, solidHeader = TRUE,
            shiny::p("Si HStat vous a été utile dans un travail de recherche, un rapport ou une publication, ",
              "merci de le citer. Choisissez le style souhaité, puis copiez la citation."),
            shiny::fluidRow(
              shiny::column(5,
                shiny::radioButtons("citeStyle", shiny::tagList(shiny::icon("list"), " Style de citation"),
                  choiceNames = list(
                    "Texte (auteur-date)", "BibTeX (LaTeX)", "RIS (EndNote, Zotero, Mendeley)",
                    "APA (7e édition)", "Vancouver", "Markdown"),
                  choiceValues = list("text", "bibtex", "ris", "apa", "vancouver", "markdown"),
                  selected = "text")),
              shiny::column(7,
                shiny::div(style = "background:#f7f9fb;border:1px solid #d9e2ec;border-radius:8px;padding:14px;",
                  shiny::tags$strong(shiny::icon("file-lines"), " Citation"),
                  shiny::tags$pre(id = "citeText", style = "white-space:pre-wrap;word-break:break-word;margin-top:8px;background:#fff;border:1px solid #e1e8ed;border-radius:6px;padding:10px;font-size:12.5px;max-height:320px;overflow:auto;",
                           shiny::verbatimTextOutput("citeOutput", placeholder = TRUE)),
                  shiny::actionButton("citeCopy", shiny::tagList(shiny::icon("copy"), " Copier dans le presse-papiers"),
                               class = "btn-success", style = "margin-top:8px;"),
                  shiny::downloadButton("citeDownload", " Télécharger", class = "btn-info", style = "margin-top:8px;"))
              )
            ),
            shiny::hr(),
            shiny::p(style = "font-size:12px;color:#7f8c8d;",
              shiny::icon("info-circle"),
              shiny::HTML(" Dans R, vous pouvez aussi exécuter <code>citation(\"HStat\")</code> pour obtenir la citation officielle du package."))
          )
        )
      )

    )
  )
)
