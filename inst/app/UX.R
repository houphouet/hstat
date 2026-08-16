#  Helpers UI -- Analyses multivariees etendues

# Bandeau "portee des données" : affiche sur chaque onglet d'analyse en mode
# hors-memoire pour rappeler que l'analyse porte sur un echantillon, avec acces
# rapide au reglage de l'echantillon. 'exact' = TRUE si l'onglet propose en plus
# un calcul exact sur le jeu complet.
.hstat_scope_banner <- function(exact = FALSE) {
  conditionalPanel(
    condition = "output.hstatBigData == true",
    div(class = "callout callout-warning", style = "margin-bottom:16px;",
      tags$p(style = "margin:0; font-size:13px;",
        icon("database"),
        if (exact)
          HTML(" <b>Mode hors-mémoire.</b> Cette analyse s'exécute sur l'échantillon de travail ; l'option <b>« calculer sur le jeu complet »</b> ci-dessous fournit un résultat exact lorsque c'est applicable.")
        else
          HTML(" <b>Mode hors-mémoire.</b> Cette analyse ajuste un modèle et s'exécute donc sur l'<b>échantillon de travail</b>. Pour gagner en fidélité, agrandissez l'échantillon dans l'onglet « Chargement » &rarr; « Échantillon de travail »."))
    )
  )
}

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
  div(style = "margin-bottom:10px;",
    div(
      style = paste0("cursor:pointer; background:", th$grad,
                     "; color:white; padding:8px 12px; border-radius:6px;",
                     " font-weight:bold; font-size:13px; user-select:none;"),
      onclick = sprintf("var c=document.getElementById('%s'); c.style.display=(c.style.display==='none'?'block':'none');", bid),
      icon("info-circle"), " Principes, objectifs & conditions ", icon("chevron-down")
    ),
    div(id = bid,
        style = paste0("display:none; background:", th$bg, "; border:1px solid ",
                       th$border, "; border-radius:0 0 6px 6px; padding:12px; font-size:12px;"),
      fluidRow(
        column(6,
          tags$b(style = paste0("color:", th$main, ";"), icon("drafting-compass"), " Principes :"),
          tags$p(style = "margin:2px 0 6px 0; color:#333;", principes),
          tags$b(style = paste0("color:", th$main, ";"), icon("bullseye"), " Objectifs :"),
          tags$p(style = "margin:2px 0 0 0; color:#333;", objectifs)
        ),
        column(6,
          tags$b(style = paste0("color:", th$main, ";"), icon("ruler"), " Taille nécessaire :"),
          tags$ul(style = "margin:2px 0 6px 12px; padding:0; color:#333;",
                  lapply(taille, function(x) tags$li(HTML(x)))),
          tags$b(style = paste0("color:", th$main, ";"), icon("hashtag"), " Variables :"),
          tags$ul(style = "margin:2px 0 0 12px; padding:0; color:#333;",
                  lapply(variables, function(x) tags$li(HTML(x))))
        )
      ),
      uiOutput(paste0("mv_", key, "_conditions"))
    )
  )
}

# Element du catalogue de methodes (style maquette) : un titre cliquable + une
# courte description. Le clic est gere par le JS du catalogue (mvSelectMethod).
.mv_cat_item <- function(title, desc, cat = "quanti") {
  div(class = "mv-cat-item", `data-title` = title, `data-cat` = cat,
    div(class = "mv-cat-item-title", title),
    div(class = "mv-cat-item-desc", desc))
}

.mv_category_header <- function(label, ic, color) {
  # En-tetes de section masques : le catalogue (facon maquette) assure deja le
  # regroupement par categorie. On retourne un conteneur vide.
  NULL
}

.mv_analysis_box <- function(key, title, cat, principes, objectifs, taille, variables,
                             intro = NULL) {
  th <- .mv_theme(cat)
  box(
    title = tagList(icon("project-diagram"), " ", title),
    status = th$status, width = 12, solidHeader = TRUE, collapsible = TRUE,
    collapsed = TRUE,

    if (!is.null(intro))
      p(style = "color:#555; font-style:italic; margin-bottom:8px;", icon("lightbulb"), " ", intro),

    .mv_info_panel(key, cat, principes, objectifs, taille, variables),

    uiOutput(paste0("mv_", key, "_controls")),

    div(style = "text-align:center; margin:12px 0;",
      actionButton(paste0("mv_", key, "_run"),
                   tagList(icon("play-circle"), " Lancer l'analyse"),
                   class = "btn-primary",
                   style = paste0("font-weight:bold; padding:8px 22px; background:",
                                  th$main, "; border-color:", th$main, ";"))
    ),

    uiOutput(paste0("mv_", key, "_status")),

    tabBox(width = 12,
      tabPanel(tagList(icon("table"), " Résultats & métriques"),
        withSpinner(uiOutput(paste0("mv_", key, "_metrics")), color = th$main)
      ),
      tabPanel(tagList(icon("chart-area"), " Graphique"),
        div(style = "max-width:860px; margin:0 auto; width:100%;",
          withSpinner(plotOutput(paste0("mv_", key, "_plot"), height = "560px"), color = th$main))
      ),
      tabPanel(tagList(icon("file-alt"), " Details techniques"),
        div(style = "max-height:520px; overflow-y:auto; font-family:'Courier New',monospace; font-size:12px; background:#fff; padding:14px; border-radius:5px;",
            verbatimTextOutput(paste0("mv_", key, "_summary")))
      )
    ),

    div(style = paste0("background:", th$grad, "; border-radius:10px; padding:14px; margin-top:8px;"),
      h4(style = "color:white; font-weight:bold; margin-top:0; text-align:center;",
         icon("file-export"), " Export des résultats"),
      fluidRow(column(12, style = "text-align:center;",
        downloadButton(paste0("mv_", key, "_dl_xlsx"),
                       HTML(paste0(as.character(icon("file-excel")), " <strong>Excel</strong>")),
                       class = "btn-success", style = "margin:4px; padding:7px 16px;"),
        downloadButton(paste0("mv_", key, "_dl_csv"),
                       HTML(paste0(as.character(icon("file-csv")), " <strong>CSV</strong>")),
                       class = "btn-warning", style = "margin:4px; padding:7px 16px;")
      ))
    )
  )
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(
      style = "display:inline-flex; align-items:baseline; gap:9px;",
      span(icon("flask"), style = "font-size:17px;"),
      span("HStat", style = "font-family:'IBM Plex Sans',sans-serif; font-weight:600;"),
    ),
    titleWidth = 300,
    # Outils du bandeau (comme la maquette) : graine aleatoire, Aide, Reinitialiser,
    # puis la bascule de theme.
    tags$li(class = "dropdown hstat-header-tools",
      div(class = "hstat-seed",
        tags$label(`for` = "globalSeed", "Graine"),
        numericInput("globalSeed", label = NULL, value = 123, min = 1, max = 1e9, step = 1, width = "78px")),
      actionButton("helpBtn", "Aide", icon = icon("question-circle"), class = "hstat-hdr-btn"),
      actionButton("resetBtn", "Réinitialiser", icon = icon("redo"), class = "hstat-hdr-btn hstat-hdr-btn-warn")),
    # Bascule de langue. Meme forme que la bascule de theme : deux segments,
    # pas de menu deroulant — c'est un choix binaire, consulte rarement mais
    # qui doit rester visible.
    tags$li(class = "dropdown",
      div(class = "hstat-theme-toggle", `data-hstat-notranslate` = NA,
        tags$span(class = "seg active", id = "hstatLangFr",
          onclick = "window.hstatSetLangue && hstatSetLangue('fr');this.classList.add('active');document.getElementById('hstatLangEn').classList.remove('active');",
          "FR"),
        tags$span(class = "seg", id = "hstatLangEn",
          onclick = "window.hstatSetLangue && hstatSetLangue('en');this.classList.add('active');document.getElementById('hstatLangFr').classList.remove('active');",
          "EN"))),
    tags$li(class = "dropdown",
      div(class = "hstat-theme-toggle",
        tags$span(class = "seg active", id = "hstatThemeLight",
          onclick = "document.body.classList.remove('hstat-dark');document.body.classList.add('hstat-light');this.classList.add('active');document.getElementById('hstatThemeDark').classList.remove('active');",
          "Clair"),
        tags$span(class = "seg", id = "hstatThemeDark",
          onclick = "document.body.classList.remove('hstat-light');document.body.classList.add('hstat-dark');this.classList.add('active');document.getElementById('hstatThemeLight').classList.remove('active');",
          "Sombre"))),
    dropdownMenu(
      type = "notifications", 
      badgeStatus = "info",
      notificationItem(
        text = "L'application est prête",
        icon = icon("check-circle"),
        status = "success"
      )
    )
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      tags$li(class = "header", "1. Préparation des données"),
      menuItem("Chargement", tabName = "load", icon = icon("upload")),
      menuItem("Exploration", tabName = "explore", icon = icon("binoculars")),
      menuItem("Nettoyage", tabName = "clean", icon = icon("broom")),
      menuItem("Filtrage", tabName = "filter", icon = icon("filter")),
      tags$li(class = "header", "2. Description"),
      menuItem("Analyses descriptives", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("chart-line")),
      tags$li(class = "header", "3. Relations & inférence"),
      menuItem("Corrélations", tabName = "corrélation", icon = icon("link")),
      menuItem("Tests statistiques", tabName = "tests", icon = icon("calculator")),
      menuItem("Comparaisons post-hoc", tabName = "multiple", icon = icon("sort-amount-down")),
      menuItem("Analyses multivariées", tabName = "multivariate", icon = icon("project-diagram")),
      menuItem("Analyses qualitatives", tabName = "qualitative", icon = icon("comments")),
      menuItem("Interprétation & aide à la décision", tabName = "aidecision",
               icon = icon("compass-drafting")),
      tags$li(class = "header", "4. Modélisation & prédiction"),
      menuItem("Séries temporelles", tabName = "timeseries", icon = icon("clock")),
      menuItem("Machine Learning", tabName = "ml", icon = icon("robot")),
      menuItem("Deep Learning", tabName = "dl", icon = icon("brain")),
      tags$li(class = "header", "5. Planification & outils"),
      menuItem("Plan & Puissance", tabName = "design", icon = icon("flask")),
      menuItem("Seuils d'efficacité", tabName = "threshold", icon = icon("gauge-high")),
      tags$li(class = "header", "6. À propos"),
      menuItem("Citer HStat", tabName = "cite", icon = icon("quote-right"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    # Ne pas bloquer le demarrage si shinyalert est absent (package optionnel)
    if (requireNamespace("shinyalert", quietly = TRUE))
      shinyalert::useShinyalert(force = TRUE),
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, shrink-to-fit=no"),
      # Feuille de theme HStat (polices IBM Plex LOCALES -> fonctionne hors-ligne).
      tags$link(rel = "stylesheet", type = "text/css",
                href = hstat_asset("hstat-theme.css")),
      # Theme clair par defaut ; la bascule ajoute/retire la classe hstat-dark.
      tags$script(HTML(
        "document.addEventListener('DOMContentLoaded',function(){if(!document.body.classList.contains('hstat-dark')){document.body.classList.add('hstat-light');}});")),
      # Accessibilite : declare la langue du document (lecteurs d'ecran, prononciation).
      tags$script(HTML(
        "document.documentElement.setAttribute('lang','fr');")),
      # Persistance de la session : le voile gris de Shiny (« Disconnected from
      # the server ») est masque au profit d'un bandeau francais qui annonce
      # l'etat et laisse reprendre la main. Un verrouillage d'ecran ne doit pas
      # fermer l'application : seul l'utilisateur la ferme.
      tags$style(HTML(
        "#shiny-disconnected-overlay{display:none !important;}")),
      tags$script(src = hstat_asset("hstat-session.js")),
      # Dictionnaire de traduction INCORPORE dans la page : aucune requete
      # reseau, le bilingue fonctionne hors ligne. La cle est la chaine
      # francaise elle-meme, donc une chaine non traduite reste en francais.
      tags$script(HTML(sprintf("window.HSTAT_I18N = %s;",
                               hstat_i18n_json("en")))),
      tags$script(src = hstat_asset("hstat-i18n.js")),
      # Copie de la citation dans le presse-papiers (API moderne + repli execCommand).
      tags$script(HTML(
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
      tags$script(HTML("
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
      tags$script(HTML("
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
      tags$script(HTML("
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
      tags$style(HTML("
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
      tags$style(HTML("
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
      tags$script(HTML("
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
    tabItems(
      # ---- Chargement ----
      tabItem(tabName = "load",
              fluidRow(
                box(title = "Charger données", status = "primary", width = 12, solidHeader = TRUE,
                    fileInput("file", "Choisir un fichier",
                              accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv",
                                         ".sav", ".dta", ".rds", ".parquet", ".duckdb")),
                    tags$small(style = "color:#6b7280; display:block; margin:-8px 0 10px 0;",
                               icon("circle-info"),
                               " Formats : CSV, TXT, Excel, SPSS, Stata, RDS, Parquet, DuckDB. ",
                               "Les fichiers volumineux (CSV/Parquet) basculent automatiquement en mode hors-mémoire."),
                    uiOutput("sheetUI"),
                    radioButtons("sep", "Séparateur (CSV/TXT)",
                                 choices = c(Virgule = ",", `Point-virgule` = ";", Tab = "\t"), selected = ","),
                    checkboxInput("header", "Avec en-têtes", TRUE),
                    tags$details(
                      style = "margin:8px 0; padding:8px 12px; background:#f4f6f8; border-radius:8px;",
                      tags$summary(style = "cursor:pointer; font-weight:600; color:#4b5563; font-size:13px;",
                                   icon("sliders"), " Options avancées (gros fichiers)"),
                      div(style = "padding-top:10px;",
                        fluidRow(
                          column(6, numericInput("bigDataThreshold",
                                     "Seuil hors-mémoire (Mo)", value = 500, min = 50, max = 1000000, step = 50)),
                          column(6, numericInput("sampleSize",
                                     "Taille de l'échantillon (lignes)", value = 100000, min = 1000, max = 10000000, step = 10000))
                        ),
                        tags$small(style = "color:#6b7280;",
                          "Au-delà du seuil, le fichier n'est pas chargé en RAM : DuckDB l'interroge sur disque ",
                          "et les analyses portent sur un échantillon représentatif de la taille indiquée."))
                    ),
                    actionButton("loadData", "Charger", class = "btn-primary", icon = icon("upload")),

                    # --- Fusion de plusieurs fichiers (section integree au meme bloc) ---
                    tags$hr(style = "margin:18px 0 12px;"),
                    tags$details(
                      style = "margin:4px 0; padding:10px 14px; background:#eef7fb; border:1px solid #b6e0ef; border-radius:8px;",
                      tags$summary(style = "cursor:pointer; font-weight:700; color:#1b6f8c; font-size:14px;",
                                   icon("object-group"), " Importer et fusionner plusieurs fichiers (optionnel)"),
                      div(style = "padding-top:12px;",
                    p(style = "color:#5a6a7a; font-size:13px;",
                      "Importez deux fichiers ou plus, puis combinez-les : jointure par clé, empilement (ajout de lignes) ou juxtaposition (ajout de colonnes). ",
                      "Le résultat remplace les données de travail actuelles. ",
                      tags$b("Un classeur Excel est déplié en ses feuilles"),
                      " : un seul classeur à plusieurs feuilles suffit donc ici."),
                    fileInput("mergeFiles",
                              "Choisir plusieurs fichiers (ou un classeur Excel a plusieurs feuilles)",
                              multiple = TRUE,
                              accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv", ".sav", ".dta", ".rds")),
                    radioButtons("mergeSep", "Séparateur (CSV/TXT)",
                                 choices = c(Virgule = ",", `Point-virgule` = ";", Tab = "\t"),
                                 selected = ",", inline = TRUE),
                    selectInput("mergeType", "Type de fusion",
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
                    conditionalPanel(
                      condition = "['inner','left','right','full','semi','anti','anti_right','update','patch'].indexOf(input.mergeType) >= 0",
                      fluidRow(
                        column(6, uiOutput("mergeKeyLeftUI")),
                        column(6, uiOutput("mergeKeyRightUI"))),
                      tags$small(style = "color:#6b7280;",
                        icon("info-circle"),
                        " La clé peut comporter plusieurs colonnes (clé composite) et porter un nom différent dans chaque fichier. Pour plus de 2 fichiers, la jointure est enchaînée sur la même clé.")),
                    conditionalPanel(
                      condition = "input.mergeType == 'cross'",
                      tags$small(style = "color:#6b7280;", icon("info-circle"),
                        " La jointure croisée associe chaque ligne du 1er fichier à chaque ligne du 2e (toutes les combinaisons). Aucune clé requise.")),
                    conditionalPanel(
                      condition = "['intersect','setdiff','setdiff_right','union_distinct'].indexOf(input.mergeType) >= 0",
                      tags$small(style = "color:#6b7280;", icon("info-circle"),
                        " Opération sur les lignes entières, en utilisant les colonnes communes aux deux fichiers.")),
                    conditionalPanel(
                      condition = "input.mergeType == 'rows'",
                      checkboxInput("mergeAddSource", "Ajouter une colonne remplie avec le nom de chaque fichier", value = TRUE),
                      conditionalPanel(
                        condition = "input.mergeAddSource == true",
                        fluidRow(
                          column(6,
                            textInput("mergeSourceName", "Nom de cette colonne",
                                      value = "année", placeholder = "ex : année, source, lot")),
                          column(6,
                            selectInput("mergeSourceMode", "Valeur à inscrire",
                              choices = c("Nom du fichier (sans extension)" = "name",
                                          "Nombre extrait du nom (ex : 2021)" = "number"),
                              selected = "name"))),
                        tags$small(style = "color:#6b7280;", icon("info-circle"),
                          " Chaque ligne reçoit la valeur correspondant à son fichier d'origine. Avec « Nombre extrait », « 2021.csv » donne 2021."))),
                    actionButton("applyMerge", tagList(icon("object-group"), " Fusionner les fichiers"),
                                 class = "btn-info"),
                    uiOutput("mergeStatus")
                      )
                    )
                )
              ),
              conditionalPanel(
                condition = "output.hstatBigData == true",
                fluidRow(column(12,
                  box(title = tagList(icon("vials"), " Échantillon de travail"),
                      status = "warning", width = 12, solidHeader = TRUE,
                    p(style = "color:#5a6a7a; font-size:13px;",
                      "Les analyses qui ajustent un modèle (ANOVA, régression, ACP, classifications, multivariées…) ",
                      "ne peuvent pas s'exécuter sur la totalité d'un très gros fichier. Elles travaillent sur cet échantillon. ",
                      "Vous pouvez l'agrandir autant que la mémoire de votre machine le permet, puis le retirer."),
                    fluidRow(
                      column(5, numericInput("sampleSizeLive",
                               tagList(icon("arrows-up-down"), " Taille de l'échantillon (lignes)"),
                               value = 100000, min = 1000, max = 20000000, step = 50000)),
                      column(7, div(style = "margin-top:25px;",
                        actionButton("redrawSample",
                          tagList(icon("rotate"), " Re-tirer l'échantillon"),
                          class = "btn-warning", style = "font-weight:bold;"),
                        tags$span(style = "margin-left:12px; font-size:12px; color:#6b7280;",
                          "Un nouveau tirage aléatoire remplace l'échantillon courant ; relancez ensuite vos analyses.")))
                    ),
                    uiOutput("sampleInfoLine")
                  )
                ))
              ),
              fluidRow(
                valueBoxOutput("nrowBox", width = 3),
                valueBoxOutput("ncolBox", width = 3),
                valueBoxOutput("naBox", width = 3),
                valueBoxOutput("memBox", width = 3)
              ),
              fluidRow(
                box(title = "Aperçu des données", status = "info", width = 12, solidHeader = TRUE,
                    DTOutput("preview"))
              )
      ),
      
      # ---- Exploration ----
      
      tabItem(tabName = "explore",
              mod_explore_ui("explore")
      ),
      # ---- Nettoyage ----
      
      tabItem(tabName = "clean",
              mod_clean_ui("clean")
      ),
      # ---- Filtrage ----
      
      tabItem(tabName = "filter",
              mod_filter_ui("filter")
      ),
      # ---- Analyse descriptives ----
      tabItem(tabName = "descriptive",
              mod_descriptive_ui("descriptive")),
      
      # ---- Visualisation des données ----
      tabItem(tabName = "visualization",
              mod_viz_ui("visualization")),
      # ---- 3. Relations & inférence : Corrélations -> Tests -> Post-hoc -> Multivariées ----
      mod_correlation_ui("corrélation"),
      mod_tests_ui("tests"),
      mod_posthoc_ui("tests"),
      # ---- Analyses multivariees ----
      
      tabItem(tabName = "multivariate",
              tags$style(HTML("
                #multivariate .box-body, #multivariate p, #multivariate li { font-size: 14px; }
                #multivariate .shiny-html-output { font-size: 14px; }
                #multivariate h4 { font-size: 18px; }
                #multivariate h5 { font-size: 15px; }
                #multivariate .nav-tabs > li > a { font-size: 15px; font-weight: 600; }
                #multivariate pre { font-size: 13px; }
              ")),
              .hstat_scope_banner(exact = FALSE),
              tags$div(class = "mv-layout",
                tags$div(class = "mv-catalog-col",
                    div(class = "mv-catalog",
                      div(class = "mv-catalog-head", icon("th-list"), " Catalogue de méthodes"),
                      tags$input(type = "text", id = "mvCatalogSearch",
                                 class = "form-control mv-catalog-search",
                                 placeholder = "Rechercher une méthode..."),
                      div(class = "mv-cat-group mv-grp-quanti", tags$span(class="mv-grp-dot"), "Quantitatives", tags$span(class="mv-grp-count", "9")),
                      div(class = "mv-cat-list",
                        .mv_cat_item("Analyse en Composantes Principales (ACP)", "Réduit p variables corrélées en composantes orthogonales."),
                        .mv_cat_item("Classification Hiérarchique sur Composantes Principales (HCPC)", "Dendrogramme consolidé par k-means sur composantes."),
                        .mv_cat_item("Analyse Factorielle Discriminante (AFD)", "Sépare des groupes connus par combinaisons discriminantes."),
                        .mv_cat_item("Classification k-means (partitionnement)", "Partitionne n individus en k groupes homogènes."),
                        .mv_cat_item("Analyse Factorielle Exploratoire (AFE)", "Découvre les facteurs latents sous-jacents aux variables."),
                        .mv_cat_item("Analyse Factorielle Confirmatoire (AFC-c)", "Teste un modèle de mesure facteurs ↔ items pré-spécifié."),
                        .mv_cat_item("Multi-Trait Multi-Method (MTMM)", "Valide la convergence et la discrimination traits × méthodes."),
                        .mv_cat_item("Regression PLS / PLS-DA", "Prédit en grande dimension via composantes latentes."),
                        .mv_cat_item("Regression linéaire multiple", "Explique une réponse continue par plusieurs prédicteurs.")
                      ),
                      div(class = "mv-cat-group mv-grp-quali", tags$span(class="mv-grp-dot"), "Qualitatives / catégorielles", tags$span(class="mv-grp-count", "5")),
                      div(class = "mv-cat-list",
                        .mv_cat_item("Analyse Factorielle des Correspondances (AFC)", "Associe deux variables qualitatives (table de contingence).", "quali"),
                        .mv_cat_item("Analyse des Correspondances Multiples (ACM)", "Généralise l'AFC à plusieurs variables qualitatives.", "quali"),
                        .mv_cat_item("Classification k-modes (partitionnement)", "k-means pour données qualitatives (modes, Hamming).", "quali"),
                        .mv_cat_item("Analyse en Classes Latentes (LCA)", "Identifie des sous-populations latentes (mélange EM).", "quali"),
                        .mv_cat_item("Regression logistique / multinomiale", "Prédit une réponse catégorielle, odds ratios.", "quali")
                      ),
                      div(class = "mv-cat-group mv-grp-mixte", tags$span(class="mv-grp-dot"), "Mixtes (quanti + quali)", tags$span(class="mv-grp-count", "3")),
                      div(class = "mv-cat-list",
                        .mv_cat_item("Analyse Factorielle de Données Mixtes (AFDM)", "Combine ACP et ACM pour tableaux mixtes.", "mixte"),
                        .mv_cat_item("Analyse Factorielle Multiple (AFM)", "Intègre plusieurs groupes de variables équilibrés.", "mixte"),
                        .mv_cat_item("Classification k-prototypes (partitionnement mixte)", "k-means + k-modes pour variables mixtes.", "mixte")
                      )
                    )
                ),
                tags$div(class = "mv-analyses-col",
                  div(class = "mv-config-hint mv-empty-hint",
                    icon("hand-point-left"),
                    HTML(" Choisissez une méthode dans le catalogue : sa configuration s'affiche ci-dessous. <b>Une seule méthode active à la fois.</b>")),
                  # En-tete de methode facon maquette (badge + titre + description),
                  # rempli dynamiquement au clic sur le catalogue. Masque tant qu'aucune
                  # methode n'est choisie.
                  div(id = "mvMethodHeader", class = "mv-method-header", style = "display:none;",
                    div(class = "mv-mh-top",
                      tags$span(id = "mvMhBadge", class = "mv-mh-badge", "QUANTITATIVE")),
                    tags$h3(id = "mvMhTitle", class = "mv-mh-title", ""),
                    tags$p(id = "mvMhDesc", class = "mv-mh-desc", "")),
              tags$script(HTML("
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
              fluidRow(
                box(title = "Analyse en Composantes Principales (ACP)", status = "info", width = 12, solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                    
                    div(style = "margin-bottom:10px;",
                        div(
                          style = "cursor:pointer; background:linear-gradient(135deg,#1565c0,#1976d2); color:white; padding:8px 12px; border-radius:6px; font-weight:bold; font-size:13px; user-select:none;",
                          onclick = "var c=document.getElementById('pca-info-body'); c.style.display=(c.style.display==='none'?'block':'none');",
                          icon("info-circle"), " Principes, objectifs & conditions de l'ACP ",
                          icon("chevron-down")
                        ),
                        div(id = "pca-info-body", style = "display:none; background:#e8f4f8; border:1px solid #90caf9; border-radius:0 0 6px 6px; padding:12px; font-size:12px;",
                            fluidRow(
                              column(6,
                                     tags$b(style="color:#1565c0;", icon("drafting-compass"), " Principes :"),
                                     tags$p(style="margin:2px 0 6px 0; color:#333;", "Réduction dimensionnelle par rotation orthogonale maximisant la variance. Transforme p variables corrélées en composantes principales indépendantes (valeurs propres de la matrice de corrélation)."),
                                     tags$b(style="color:#1565c0;", icon("bullseye"), " Objectifs :"),
                                     tags$p(style="margin:2px 0 0 0; color:#333;", "Explorer la structure, identifier des patterns, visualiser des données multivariées, réduire la dimensionnalité avant classification.")
                              ),
                              column(6,
                                     tags$b(style="color:#1565c0;", icon("ruler"), " Taille nécessaire :"),
                                     tags$ul(style="margin:2px 0 6px 12px; padding:0; color:#333;",
                                             tags$li("Minimum absolu : n", icon("arrow-right"), "30"),
                                             tags$li("Recommandé : n", icon("arrow-right"), "max(50, 5xp)"),
                                             tags$li("Idéal : n", icon("arrow-right"), "100 pour stabilité")
                                     ),
                                     tags$b(style="color:#1565c0;", icon("hashtag"), " Variables minimales :"),
                                     tags$ul(style="margin:2px 0 0 12px; padding:0; color:#333;",
                                             tags$li("Minimum absolu : p", icon("arrow-right"), "2"),
                                             tags$li("Recommandé : p", icon("arrow-right"), "3-5"),
                                             tags$li("Toutes doivent être numériques")
                                     )
                              )
                            ),
                            uiOutput("pcaConditionsCheck")
                        )
                    ),
                    
                    uiOutput("pcaVarSelect"),
                    
                    uiOutput("pcaCollinearityPanel"),
                    
                    checkboxInput("pcaScale", "Standardiser les variables", TRUE),
                    checkboxInput("pcaUseMeans", "Utiliser les moyennes par groupe", FALSE),
                    conditionalPanel(
                      condition = "input.pcaUseMeans == true",
                      uiOutput("pcaMeansGroupSelect"),
                      actionButton("pcaRefresh", "Actualiser l'ACP", 
                                   icon = icon("sync"), 
                                   class = "btn-info btn-sm",
                                   style = "margin-bottom: 10px;")
                    ),
                    uiOutput("pcaQualiSupSelect"),
                    uiOutput("pcaQuantiSupSelect"),
                    uiOutput("pcaIndSupSelect"),
                    uiOutput("pcaLabelSourceSelect"),
                    hr(),
                    radioButtons("pcaPlotType", "Type de visualisation:",
                                 choices = c("Variables" = "var", "Individus" = "ind", "Biplot" = "biplot"),
                                 selected = "var", inline = TRUE),
                    
                    div(style = "background:#f0f7ff; border-left:3px solid #2196f3; padding:8px 12px; margin:6px 0 10px 0; border-radius:0 4px 4px 0;",
                        selectInput("pcaColorBy", 
                                    tagList(icon("palette"), " Colorer les éléments par :"),
                                    choices = c(
                                      "Contribution (% à la composante)"       = "contrib",
                                      "Cos² (qualité de représentation)"       = "cos2",
                                      "Indice de saturation (|corrélation|)"   = "sat"
                                    ),
                                    selected = "contrib"),
                        uiOutput("pcaColorByLegend")
                    ),
                    numericInput("pcaComponents", "Nombre de composantes:", value = 5, min = 2, max = 10),
                    
                    div(style = "text-align:center;margin:12px 0;",
                        actionButton("pcaRun", tagList(icon("play"), " Lancer l'ACP"),
                                     class = "btn-success btn-lg btn-block",
                                     style = "font-weight:bold;")),
                    div(class = "alert alert-info", style = "font-size:12px;",
                        icon("info-circle"),
                        " L'ACP ne se lance qu'après ce clic. Modifier les variables annule le lancement (recliquez pour relancer). ",
                        "Les options de personnalisation du graphique se trouvent sous le graphique."),
                    hr(),
                div(id = "boxWrap_pcaResults",
                box(title = tagList(icon("project-diagram"), " Résultats de l'ACP"),
                    status = "info", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    tabsetPanel(
                      id = "pcaTabs", type = "tabs",
                      tabPanel(
                        tagList(icon("chart-area"), " Graphiques"),
                        br(),
                        div(style="max-width:900px;margin:0 auto;", plotOutput("pcaPlot", height = "560px")),
                        hr(),
                                            div(style = "background-color:#eef2f5;border-left:4px solid #3c8dbc;padding:10px;margin:6px 0;",
                                                h4(style = "margin-top:0;color:#2c3e50;", icon("sliders-h"),
                                                   " Paramètres de modification du graphique"),
                                                fluidRow(
                                                  column(4,
                                                    h5(style = "color:#495057;", icon("sync-alt"), " Rotation orthogonale"),
                                                    selectInput("pcaRotationMethod", "Méthode de rotation:",
                                                                choices = c("Varimax" = "varimax", "Quartimax" = "quartimax",
                                                                            "Oblimin" = "oblimin", "Aucune" = "none"),
                                                                selected = "varimax"),
                                                    numericInput("pcaRotationNFactors", "Facteurs à rotationner:",
                                                                 value = 2, min = 2, max = 10)),
                                                  column(4,
                                                    h5(style = "color:#495057;", icon("chart-line"), " Axes représentés"),
                                                    uiOutput("pcaAxisXSelect"),
                                                    uiOutput("pcaAxisYSelect")),
                                                  column(4,
                                                    h5(style = "color:#495057;", icon("font"), " Titres & libellés"),
                                                    textInput("pcaPlotTitle", "Titre du graphique:",
                                                              value = "ACP - Analyse en Composantes Principales"),
                                                    textInput("pcaXLabel", "Label axe X:", value = ""),
                                                    textInput("pcaYLabel", "Label axe Y:", value = "")),
                                                  hstat_mv_forme_ui("pcaPlot")
                                                ),
                                                fluidRow(
                                                  column(4, checkboxInput("pcaCenterAxes", "Centrer sur (0,0)", TRUE)),
                                                  column(4, checkboxInput("pcaRoundResults", "Arrondir les résultats", value = FALSE)),
                                                  column(4, conditionalPanel(
                                                    condition = "input.pcaRoundResults == true",
                                                    numericInput("pcaDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)))
                                                ),
                                                div(style = "background-color:#f4f6f8;border-left:4px solid #6c757d;padding:10px;margin:6px 0;",
                                                  h5(style="color:#495057;margin-top:0;", icon("text-height"), " Style du texte, des points et des tracés"),
                                                  fluidRow(
                                                    column(3, sliderInput("pcaAxisTextSize", "Taille texte des axes",
                                                                          min = 8, max = 24, value = HSTAT_GG_BASE_SIZE, step = 1)),
                                                    column(3, sliderInput("pcaAxisTitleSize", "Taille titres d'axes",
                                                                          min = 8, max = 26, value = HSTAT_GG_BASE_SIZE, step = 1)),
                                                    column(3, hstat_lbl_slider("pcaLabelSize", "Taille des labels des individus")),
                                                    column(3, hstat_lbl_slider("pcaVarLabelSize", "Taille des labels des variables"))
                                                  ),
                                                  fluidRow(
                                                    column(3, sliderInput("pcaPointSize", "Taille des points (individus)",
                                                                          min = 0.5, max = 8, value = HSTAT_GG_POINT_SIZE, step = 0.5))
                                                  ),
                                                  fluidRow(
                                                    column(3, sliderInput("pcaLineWidth", "Largeur des tracés (flèches/axes)",
                                                                          min = 0.3, max = 4, value = HSTAT_GG_LINEWIDTH, step = 0.1)),
                                                    column(3, div(style="margin-top:25px;",
                                                              checkboxInput("pcaBoldText", "Texte en gras", value = FALSE))),
                                                    column(3, div(style="margin-top:25px;",
                                                              checkboxInput("pcaItalicText", "Texte en italique", value = FALSE))),
                                                    column(3, div(style="margin-top:5px;",
                                                              checkboxInput("pcaShowEllipses",
                                                                tagList(icon("draw-polygon"), " Ellipses (groupes)"),
                                                                value = FALSE)))
                                                  ),
                                                  fluidRow(
                                                    column(12, conditionalPanel(
                                                      condition = "input.pcaShowEllipses == true",
                                                      uiOutput("pcaEllipseGroupSelect")))
                                                  )
                                                ),
                                                fluidRow(
                                                  column(3, selectInput("pcaPlot_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")),
                                                  column(3, numericInput("pcaPlot_dpi", "DPI:", value = 300, min = 72, max = HSTAT_DPI_MAX)),
                                                  column(3, numericInput("pcaPlot_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)),
                                                  column(3, numericInput("pcaPlot_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100))
                                                ),
                                                hstat_mv_dim_note_ui("pcaPlot"),
                                                div(style = "text-align:center;margin-top:8px;",
                                                    downloadButton("downloadPcaPlot", "Télécharger graphique", class = "btn-info", style = "margin:5px;"),
                                                    downloadButton("downloadPcaDataXlsx", "Données (Excel)", class = "btn-success", style = "margin:5px;"),
                                                    downloadButton("downloadPcaDataCsv", "Données (CSV)", class = "btn-success", style = "margin:5px;"))
                                            ),
                      ),
                      tabPanel(
                        tagList(icon("clipboard-check"), " Métriques"),
                        br(),
                        div(style = "background-color: #f8f9fa; border-left: 5px solid #495057; padding: 12px; margin: 0 0 12px 0;",
                            h4(style = "color: #343a40; font-weight: bold; margin-top: 0;",
                               icon("clipboard-check"), " Métriques de validation de l'ACP"),
                            p(style = "font-size: 12px; color: #555; margin-bottom: 0;",
                              "Evaluez ces métriques avant d'interpreter le graphique.")),
                        h5(style = "color: #2c3e50; font-weight: bold;",
                           icon("check-circle"), " Adequation des données a l'ACP"),
                        uiOutput("pcaBartlettKMO"),
                        hr(),
                                            h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;",
                                               icon("chart-line"), " Graphique des éboulis (Scree Plot)"),
                                            p(style = "font-size: 11px; color: #666; font-style: italic;",
                                              "Critère de Kaiser (valeur propre min. 1) : les composantes en vert sont retenues. Cherchez le 'coude' de la courbe."),
                                            plotOutput("pcaScreePlot", height = "320px"),
                                            hstat_mv_forme_ui("pcaScree", "Apparence du graphique des éboulis"),
                                            fluidRow(
                                              column(12, selectInput("pcaScree_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")),
                                              column(4, numericInput("pcaScree_dpi", "DPI:", value = 300, min = 72, max = HSTAT_DPI_MAX)),
                                              column(4, numericInput("pcaScree_width", "Largeur (px):", value = 2500, min = 200, max = 20000, step = 50)),
                                              column(4, numericInput("pcaScree_height", "Hauteur (px):", value = 1800, min = 200, max = 20000, step = 50))
                                            ),
                                            hstat_mv_dim_note_ui("pcaScree"),
                                            div(style = "text-align: center; margin-bottom: 10px;",
                                                downloadButton("downloadPcaScreePlot", "Télécharger Scree Plot", class = "btn-info btn-sm")
                                            ),
                    
                                            h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                                               icon("random"), " Analyse parallèle de Horn"),
                                            p(style = "font-size: 11px; color: #666; font-style: italic;",
                                              "Méthode plus rigoureuse que Kaiser : retenir les composantes dont la valeur propre observée dépasse le percentile 95 des simulations aléatoires."),
                                            plotOutput("pcaParallelPlot", height = "320px"),
                                            hstat_mv_forme_ui("pcaParallel", "Apparence de l'analyse parallèle"),
                                            fluidRow(
                                              column(12, selectInput("pcaParallel_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")),
                                              column(4, numericInput("pcaParallel_dpi", "DPI:", value = 300, min = 72, max = HSTAT_DPI_MAX)),
                                              column(4, numericInput("pcaParallel_width", "Largeur (px):", value = 2500, min = 200, max = 20000, step = 50)),
                                              column(4, numericInput("pcaParallel_height", "Hauteur (px):", value = 1800, min = 200, max = 20000, step = 50))
                                            ),
                                            hstat_mv_dim_note_ui("pcaParallel"),
                                            div(style = "text-align: center; margin-bottom: 10px;",
                                                downloadButton("downloadPcaParallelPlot", "Télécharger Analyse Parallèle", class = "btn-info btn-sm")
                                            ),
                    
                                            h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                                               icon("percentage"), " Contributions absolues (CTR) des variables"),
                                            p(style = "font-size: 11px; color: #666; font-style: italic;",
                                              "Seuil théorique = 100% / nb variables. Les variables au-dessus du seuil (en vert) structurent principalement l'axe."),
                                            uiOutput("pcaCTRAxisSelect"),
                                            plotOutput("pcaCTRPlot", height = "300px"),
                                            hstat_mv_forme_ui("pcaCTR", "Apparence du graphique CTR"),
                                            fluidRow(
                                              column(12, selectInput("pcaCTR_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")),
                                              column(4, numericInput("pcaCTR_dpi", "DPI:", value = 300, min = 72, max = HSTAT_DPI_MAX)),
                                              column(4, numericInput("pcaCTR_width", "Largeur (px):", value = 2500, min = 200, max = 20000, step = 50)),
                                              column(4, numericInput("pcaCTR_height", "Hauteur (px):", value = 1800, min = 200, max = 20000, step = 50))
                                            ),
                                            hstat_mv_dim_note_ui("pcaCTR"),
                                            div(style = "text-align: center; margin-bottom: 10px;",
                                                downloadButton("downloadPcaCTRPlot", "Télécharger Graphique CTR", class = "btn-info btn-sm")
                                            ),
                    
                      ),
                      tabPanel(
                        tagList(icon("cogs"), " Details techniques"),
                        br(),
                                            h5(style = "color: #2c3e50; font-weight: bold; margin-top: 15px;", 
                                               icon("sync-alt"), " Résultats de la rotation orthogonale"),
                                            p(style = "font-size: 11px; color: #666; font-style: italic;",
                                              "La rotation simplifie la structure : un loading |x| min. 0,70 indique une contribution forte, |x| de 0,40 à 0,70 une contribution modérée."),
                                            div(style = "max-height: 350px; overflow-y: auto; font-size: 11px;",
                                                verbatimTextOutput("pcaRotationResult")),
                    
                                            hr(),
                                            div(style = "max-height: 300px; overflow-y: auto; font-size: 12px;",
                                                verbatimTextOutput("pcaSummary")),
                                            hr(),
                                            # ---- Export métriques ACP 
                                            div(style = "background: linear-gradient(135deg, #343a40 0%, #495057 100%); border-radius: 10px; padding: 18px; margin-top: 10px;",
                                                h4(style = "color: white; font-weight: bold; margin-top: 0; text-align: center;",
                                                   icon("file-export"), " Export des métriques ACP"),
                                                p(style = "color: #aed6f1; font-size: 12px; text-align: center; margin-bottom: 12px;",
                                                  "Valeurs propres, Bartlett/KMO, Contributions absolues (CTR), Qualité de représentation (cos²)"),
                                                fluidRow(
                                                  column(12, style = "text-align: center;",
                                                         downloadButton("downloadPcaMetricsXlsx",
                                                                        HTML(paste0(as.character(icon("file-excel")), " <strong>Métriques ACP (Excel)</strong>")),
                                                                        class = "btn-success",
                                                                        style = "margin: 4px; padding: 7px 16px;"),
                                                         downloadButton("downloadPcaMetricsCsv",
                                                                        HTML(paste0(as.character(icon("file-csv")), " <strong>Métriques ACP (CSV)</strong>")),
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
              
              fluidRow(
                box(title = "Classification Hiérarchique sur Composantes Principales (HCPC)", 
                    status = "success", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                                        p("Cette analyse combine l'ACP avec une classification hiérarchique automatique."),
                    
                                        div(style = "margin-bottom:10px;",
                                            div(
                                              style = "cursor:pointer; background:linear-gradient(135deg,#2e7d32,#388e3c); color:white; padding:8px 12px; border-radius:6px; font-weight:bold; font-size:13px; user-select:none;",
                                              onclick = "var c=document.getElementById('hcpc-info-body'); c.style.display=(c.style.display==='none'?'block':'none');",
                                              icon("info-circle"), " Principes, objectifs & conditions de la HCPC ",
                                              icon("chevron-down")
                                            ),
                                            div(id = "hcpc-info-body", style = "display:none; background:#e8f5e9; border:1px solid #a5d6a7; border-radius:0 0 6px 6px; padding:12px; font-size:12px;",
                                                fluidRow(
                                                  column(6,
                                                         tags$b(style="color:#2e7d32;", icon("drafting-compass"), " Principes :"),
                                                         tags$p(style="margin:2px 0 6px 0; color:#333;", "Classification ascendante hiérarchique (critère de Ward) appliquée aux coordonnées ACP. Minimise la variance intra-cluster à chaque fusion. La coupure optimale est déterminée par le saut maximal des hauteurs."),
                                                         tags$b(style="color:#2e7d32;", icon("bullseye"), " Objectifs :"),
                                                         tags$p(style="margin:2px 0 0 0; color:#333;", "Identifier des groupes naturels (typologies), profiler les individus similaires, réduire les données individuelles en groupes interprétables.")
                                                  ),
                                                  column(6,
                                                         tags$b(style="color:#2e7d32;", icon("ruler"), " Taille nécessaire :"),
                                                         tags$ul(style="margin:2px 0 6px 12px; padding:0; color:#333;",
                                                                 tags$li("Minimum absolu : n", icon("arrow-right"), "2xk (k=clusters)"),
                                                                 tags$li("Recommandé : n", icon("arrow-right"), "10xk pour stabilité"),
                                                                 tags$li("Hérite des conditions de l'ACP")
                                                         ),
                                                         tags$b(style="color:#2e7d32;", icon("hashtag"), " Composantes nécessaires :"),
                                                         tags$ul(style="margin:2px 0 0 12px; padding:0; color:#333;",
                                                                 tags$li("Minimum : ", icon("arrow-right"), "1 composante ACP"),
                                                                 tags$li("Recommandé : ", icon("arrow-right"), "2 composantes retenues (", tags$em("\u03bb"), " ", icon("arrow-right"), "1)"),
                                                                 tags$li("Utilise les composantes de l'ACP précédente")
                                                         )
                                                  )
                                                ),
                                                uiOutput("hcpcConditionsCheck")
                                            )
                                        ),
                    
                                        fluidRow(
                                          column(4,
                                                 numericInput("hcpcClusters", "Nombre de clusters:", value = 3, min = 2, max = 10)
                                          ),
                                          column(4,
                                                 checkboxInput("hcpcUseMeans", tagList(icon("layer-group"), " Classer les moyennes par groupe"), value = FALSE),
                                                 conditionalPanel(
                                                   condition = "input.hcpcUseMeans == true",
                                                   uiOutput("hcpcMeansGroupSelect")
                                                 )
                                          ),
                                          column(4,
                                                 uiOutput("hcpcLabelSourceSelect")
                                          )
                                        ),
                                        fluidRow(
                                          column(12,
                                                 div(style = "text-align: center; margin-top: 10px;",
                                                     actionButton("hcpcRun", tagList(icon("play"), " Lancer la classification (HCPC)"),
                                                                  class = "btn-success", style = "margin:5px;font-weight:bold;"),
                                                     downloadButton("downloadHcpcDataXlsx", "Télécharger données (Excel)", 
                                                                    class = "btn-success", style = "margin: 5px;"),
                                                     downloadButton("downloadHcpcDataCsv", "Télécharger données (CSV)", 
                                                                    class = "btn-success", style = "margin: 5px;")
                                                 )
                                          )
                                        ),
                    
                                        div(style = "background-color: #e8f5e9; border-left: 4px solid #4caf50; padding: 10px; margin: 10px 0;",
                                            h5(style = "margin-top: 0; color: #2e7d32;", icon("chart-line"), " Sélection des axes à représenter"),
                                            fluidRow(
                                              column(6,
                                                     uiOutput("hcpcAxisXSelect")
                                              ),
                                              column(6,
                                                     uiOutput("hcpcAxisYSelect")
                                              )
                                            ),
                                            fluidRow(
                                              column(6, sliderInput("hcpcPointSize", "Taille des points", min = 0.5, max = 8, value = HSTAT_GG_POINT_SIZE, step = 0.5)),
                                              column(6, sliderInput("hcpcAxisTextSize", "Taille texte des axes", min = 8, max = 24, value = HSTAT_GG_BASE_SIZE, step = 1))
                                            ),
                                            p(style = "margin: 5px 0 0 0; font-size: 11px; color: #1b5e20; font-style: italic;",
                                              icon("info-circle"), " Les polygones colores entourent chaque cluster. Activez les étiquettes uniquement pour de petits jeux de données.")
                                        ),
                    
                                        hr(),
                                        div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin: 10px 0;",
                                            fluidRow(
                                              column(6,
                                                     checkboxInput("hcpcRoundResults", "Arrondir les résultats", value = FALSE)
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.hcpcRoundResults == true",
                                                       numericInput("hcpcDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                                     )
                                              )
                                            )
                                        ),
                    hr(),
                    tabsetPanel(
                      id = "hcpcTabs", type = "tabs",
                      tabPanel(
                        tagList(icon("chart-area"), " Graphiques"),
                        br(),
                                            hr(),
                                            h5("Personnalisation graphique:", style = "font-weight: bold; color: #5cb85c;"),
                                            fluidRow(
                                              column(6,
                                                     hstat_mv_forme_ui("hcpcCluster"),
                                                     hstat_mv_forme_ui("hcpcDend", "Apparence du dendrogramme"),
                                                     hstat_mv_forme_ui("hcpcHeights", "Apparence des hauteurs de fusion"),
                                                     textInput("hcpcClusterTitle", "Titre carte des clusters:", 
                                                               value = "Carte des clusters HCPC"),
                                                     textInput("hcpcClusterXLabel", "Label axe X:", value = ""),
                                                     textInput("hcpcClusterYLabel", "Label axe Y:", value = ""),
                                                     checkboxInput("hcpcCenterAxes", "Centrer sur (0,0)", TRUE),
                                                     div(style = "background:#eafaf3; border-left:3px solid #16a085; padding:8px 12px; border-radius:0 4px 4px 0; margin:6px 0;",
                                                       checkboxInput("hcpcClusterShowLabels", tagList(icon("font"), " Afficher les labels des individus sur la carte"), value = FALSE),
                                                       hstat_lbl_slider("hcpcClusterLabelSize", "Taille des labels des individus (carte)")),
                                                     hr(),
                                                     h5("Options Téléchargement carte clusters:"),
                                                     p(style = "font-size: 11px; color: #5cb85c; font-style: italic;",
                                                       icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                                                     fluidRow(
                                                       column(6,
                                                              selectInput("hcpcCluster_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")
                                                       ),
                                                       column(6,
                                                              numericInput("hcpcCluster_dpi", "DPI:", value = 300, min = 72, max = HSTAT_DPI_MAX)
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(6,
                                                              numericInput("hcpcCluster_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                                                       ),
                                                       column(6,
                                                              numericInput("hcpcCluster_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                                                       )
                                                     ),
                                                     hstat_mv_dim_note_ui("hcpcCluster")
                                              ),
                                              column(6,
                                                     textInput("hcpcDendTitle", "Titre dendrogramme:", 
                                                               value = "Dendrogramme HCPC"),
                                                     div(style = "background:#eafaf3; border-left:3px solid #16a085; padding:8px 12px; border-radius:0 4px 4px 0; margin:6px 0;",
                                                       sliderInput("hcpcBranchWidth", tagList(icon("grip-lines"), " Largeur des branches"),
                                                                   min = 0.1, max = 4, value = 0.5, step = 0.1),
                                                       hstat_lbl_slider("hcpcLabelSize", tagList(icon("font"), " Taille des labels (individus)")),
                                                       checkboxInput("hcpcShowLabels", "Afficher les labels des individus sur les branches", value = FALSE)),
                                                     p(style = "font-style: italic; color: #666;", 
                                                       "Le dendrogramme n'est pas centré sur (0,0)"),
                                                     hr(),
                                                     h5("Options Téléchargement dendrogramme:"),
                                                     p(style = "font-size: 11px; color: #5cb85c; font-style: italic;",
                                                       icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                                                     fluidRow(
                                                       column(6,
                                                              selectInput("hcpcDend_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")
                                                       ),
                                                       column(6,
                                                              numericInput("hcpcDend_dpi", "DPI:", value = 300, min = 72, max = HSTAT_DPI_MAX)
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(6,
                                                              numericInput("hcpcDend_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                                                       ),
                                                       column(6,
                                                              numericInput("hcpcDend_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                                                       )
                                                     ),
                                                     hstat_mv_dim_note_ui("hcpcDend")
                                              )
                                            ),
                                            hr(),
                                            fluidRow(
                                              column(12,
                                                     div(class = "box box-solid box-success",
                                                         div(class = "box-header with-border",
                                                             h4(class = "box-title", "Carte des clusters")
                                                         ),
                                                         div(class = "box-body",
                                                             div(style="max-width:850px;margin:0 auto;", plotOutput("hcpcClusterPlot", height = "520px")),
                                                             downloadButton("downloadHcpcClusterPlot", "Télécharger carte")
                                                         )
                                                     )
                                              ),
                                              column(12,
                                                     div(class = "box box-solid box-success",
                                                         div(class = "box-header with-border",
                                                             h4(class = "box-title", "Dendrogramme")
                                                         ),
                                                         div(class = "box-body",
                                                             div(style="max-width:850px;margin:0 auto;", plotOutput("hcpcDendPlot", height = "520px")),
                                                             downloadButton("downloadHcpcDendPlot", "Télécharger dendrogramme")
                                                         )
                                                     )
                                              )
                                            ),
                      ),
                      tabPanel(
                        tagList(icon("microscope"), " Métriques"),
                        br(),
                                            # ---- MÉTRIQUES DE VALIDATION HCPC 
                                            div(style = "background-color: #eafaf1; border-left: 5px solid #27ae60; padding: 12px; margin: 15px 0 10px 0;",
                                                h4(style = "color: #1e8449; font-weight: bold; margin-top: 0;",
                                                   icon("microscope"), " Métriques de validation de la classification"),
                                                p(style = "font-size: 12px; color: #555; margin-bottom: 0;",
                                                  "Les métriques suivantes permettent d'évaluer la qualité et la robustesse de la partition obtenue. Elles doivent être analysées dans l'ordre présenté.")
                                            ),
                    
                                            # 1. Hauteurs de fusion
                                            div(class = "box box-solid",
                                                div(class = "box-header with-border", style = "background-color: #2980b9; color: white;",
                                                    h4(class = "box-title", style = "color: white;",
                                                       icon("chart-area"), " Graphique des hauteurs de fusion")
                                                ),
                                                div(class = "box-body",
                                                    p(style = "font-size: 12px; color: #555; font-style: italic;",
                                                      "Un saut important entre deux fusions consécutives suggère la coupure optimale du dendrogramme (règle du coude). Ce graphique complète la lecture visuelle du dendrogramme."),
                                                    plotOutput("hcpcHeightsPlot", height = "320px"),
                                                    fluidRow(
                                                      column(12, selectInput("hcpcHeights_format", "Format:",
                                                                            choices = c("png", "svg", "pdf", "tiff"), selected = "png")),
                                                      column(4, numericInput("hcpcHeights_dpi", "DPI:", value = 300, min = 72, max = HSTAT_DPI_MAX)),
                                              column(4, numericInput("hcpcHeights_width", "Largeur (px):", value = 2500, min = 200, max = 20000, step = 50)),
                                              column(4, numericInput("hcpcHeights_height", "Hauteur (px):", value = 1800, min = 200, max = 20000, step = 50))
                                                    ),
                                                    hstat_mv_dim_note_ui("hcpcHeights"),
                                                    div(style = "text-align: center; margin-top: 4px;",
                                                        downloadButton("downloadHcpcHeightsPlot",
                                                                       "Télécharger hauteurs de fusion",
                                                                       class = "btn-info btn-sm")
                                                    )
                                                )
                                            ),
                    
                                            # 2. CH, DB, Silhouette, Cophénétique
                                            div(class = "box box-solid",
                                                div(class = "box-header with-border", style = "background-color: #e67e22; color: white;",
                                                    h4(class = "box-title", style = "color: white;",
                                                       icon("ruler-combined"), " Indices de validation des clusters")
                                                ),
                                                div(class = "box-body",
                                                    p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 12px;",
                                                      "Ces quatre indices évaluent respectivement la séparation inter-classes (CH), la compacité relative (DB), la cohérence individuelle (Silhouette) et la fidélité du dendrogramme (Cophénétique)."),
                                                    uiOutput("hcpcMetricsUI")
                                                )
                                            ),
                    
                                            div(class = "box box-solid",
                                                div(class = "box-header with-border", style = "background-color: #16a085; color: white;",
                                                    h4(class = "box-title", style = "color: white;",
                                                       icon("shield-alt"), " Stabilité par sous-échantillonnage")
                                                ),
                                                div(class = "box-body",
                                                    p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 12px;",
                                                      "Évalue si la structure de clusters est reproductible sur des sous-échantillons aléatoires des données. Un indice de Rand proche de 1 confirme la robustesse de la partition."),
                                                    uiOutput("hcpcStabilityUI")
                                                )
                                            ),
                      ),
                      tabPanel(
                        tagList(icon("cogs"), " Details techniques"),
                        br(),
                                            div(class = "box box-solid",
                                                div(class = "box-header with-border", style = "background-color: #5cb85c; color: white;",
                                                    h4(class = "box-title", "Résultats detailles HCPC", style = "color: white; font-weight: bold;")
                                                ),
                                                div(class = "box-body", style = "background-color: #f9f9f9;",
                                                    div(style = "max-height: 500px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                                        verbatimTextOutput("hcpcSummary"))
                                                )
                                            ),
                                            # ---- Export métriques HCPC 
                                            div(style = "background: linear-gradient(135deg, #0e6655 0%, #117a65 100%); border-radius: 10px; padding: 18px; margin-top: 20px;",
                                                h4(style = "color: white; font-weight: bold; margin-top: 0; text-align: center;",
                                                   icon("file-export"), " Export des métriques HCPC / CAH"),
                                                p(style = "color: #a9dfbf; font-size: 12px; text-align: center; margin-bottom: 12px;",
                                                  "Indices CH, Davies-Bouldin, Silhouette, corrélation cophénétique, affectation des individus aux clusters"),
                                                fluidRow(
                                                  column(12, style = "text-align: center;",
                                                         downloadButton("downloadHcpcMetricsXlsx",
                                                                        HTML(paste0(as.character(icon("file-excel")), " <strong>Métriques HCPC (Excel)</strong>")),
                                                                        class = "btn-success",
                                                                        style = "margin: 4px; padding: 7px 16px;"),
                                                         downloadButton("downloadHcpcMetricsCsv",
                                                                        HTML(paste0(as.character(icon("file-csv")), " <strong>Métriques HCPC (CSV)</strong>")),
                                                                        class = "btn-warning",
                                                                        style = "margin: 4px; padding: 7px 16px;")
                                                  )
                                                )
                                            )
                      )
                    )
                )
              ),
              
              fluidRow(
                box(title = "Analyse Factorielle Discriminante (AFD)", 
                    status = "primary", width = 12, solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    
                                        div(style = "margin-bottom:12px;",
                                            div(
                                              style = "cursor:pointer; background:linear-gradient(135deg,#1a237e,#283593); color:white; padding:8px 12px; border-radius:6px; font-weight:bold; font-size:13px; user-select:none;",
                                              onclick = "var c=document.getElementById('afd-info-body'); c.style.display=(c.style.display==='none'?'block':'none');",
                                              icon("info-circle"), " Principes, objectifs & conditions de l'AFD ",
                                              icon("chevron-down")
                                            ),
                                            div(id = "afd-info-body", style = "display:none; background:#e8eaf6; border:1px solid #9fa8da; border-radius:0 0 6px 6px; padding:12px; font-size:12px;",
                                                fluidRow(
                                                  column(6,
                                                         tags$b(style="color:#1a237e;", icon("drafting-compass"), " Principes :"),
                                                         tags$p(style="margin:2px 0 6px 0; color:#333;", "Recherche les combinaisons linéaires de variables (fonctions discriminantes) maximisant le ratio variance inter-groupes / variance intra-groupes (critère de Fisher-Rao). Généralisation multivariée de l'ANOVA."),
                                                         tags$b(style="color:#1a237e;", icon("bullseye"), " Objectifs :"),
                                                         tags$p(style="margin:2px 0 0 0; color:#333;", "Discriminer des individus dans des groupes prédéfinis, identifier les variables les plus discriminantes, prédire l'appartenance à un groupe pour de nouveaux individus.")
                                                  ),
                                                  column(6,
                                                         tags$b(style="color:#1a237e;", icon("ruler"), " Taille nécessaire :"),
                                                         tags$ul(style="margin:2px 0 6px 12px; padding:0; color:#333;",
                                                                 tags$li("Minimum absolu : n > p + g - 1"),
                                                                 tags$li("Recommandé : min. 20 obs. par groupe"),
                                                                 tags$li("Idéal : ratio n/p min. 10"),
                                                                 tags$li(tags$em("p = nb variables, g = nb groupes"))
                                                         ),
                                                         tags$b(style="color:#1a237e;", icon("hashtag"), " Variables minimales :"),
                                                         tags$ul(style="margin:2px 0 0 12px; padding:0; color:#333;",
                                                                 tags$li("Minimum absolu : p min. 1"),
                                                                 tags$li("Recommandé : p min. 2"),
                                                                 tags$li("Maximum : p < n - g"),
                                                                 tags$li("Groupes minimum : g min. 2")
                                                         )
                                                  )
                                                ),
                                                uiOutput("afdConditionsCheck")
                                            )
                                        ),
                    
                                        div(style = "background-color: #f8f9fa; border-left: 4px solid #343a40; padding: 15px; margin-bottom: 15px;",
                                            h4(style = "color: #343a40; margin-top: 0;",
                                               icon("bullseye"), " Variable à discriminer (OBLIGATOIRE)"),
                                            p(style = "margin: 5px 0; font-size: 13px; color: #555;",
                                              "Sélectionnez la variable catégorielle que vous souhaitez discriminer (prédire). Cette variable doit contenir au moins 2 groupes différents."),
                                            uiOutput("afdFactorSelect"),
                                            p(style = "margin: 5px 0 0 0; font-size: 11px; color: #e74c3c; font-weight: bold;",
                                              icon("exclamation-circle"), " Si aucune variable n'apparaît, vérifiez que vos données contiennent des variables catégorielles (facteurs).")
                                        ),
                    
                                        div(style = "background-color: #f8f9fa; border-left: 4px solid #495057; padding: 15px; margin-bottom: 15px;",
                                            h4(style = "color: #495057; margin-top: 0;",
                                               icon("chart-line"), " Variables quantitatives (OBLIGATOIRE)"),
                                            p(style = "margin: 5px 0; font-size: 13px; color: #555;",
                                              "Sélectionnez les variables numériques qui serviront à discriminer les groupes. Plus il y a de variables pertinentes, meilleure sera la discrimination."),
                                            uiOutput("afdVarSelect"),
                                            p(style = "margin: 5px 0 0 0; font-size: 11px; color: #27ae60; font-style: italic;",
                                              icon("check-circle"), " Conseil : Sélectionnez au moins 2-3 variables pour obtenir de bons résultats.")
                                        ),
                    
                                        uiOutput("afdCollinearityPanel"),
                    
                                        hr(),
                                        h4(style = "color: #6c757d; margin-top: 10px;", icon("cogs"), " Options avancées (optionnel)"),
                                        checkboxInput("afdUseMeans", "Utiliser les moyennes par groupe", FALSE),
                                        conditionalPanel(
                                          condition = "input.afdUseMeans == true",
                                          uiOutput("afdMeansGroupSelect"),
                                          p(style = "margin: 5px 0 10px 0; font-size: 11px; color: #6c757d;",
                                            icon("lightbulb"), 
                                            " Conseil: Utilisez la même variable que le facteur de discrimination pour une AFD sur moyennes de groupes."),
                                          actionButton("afdRefresh", "Actualiser l'AFD", 
                                                       icon = icon("sync"), 
                                                       class = "btn-info btn-sm",
                                                       style = "margin-bottom: 10px;")
                                        ),
                    
                                        div(style = "text-align:center;margin:12px 0;",
                                            actionButton("afdRun", tagList(icon("play"), " Lancer l'AFD"),
                                                         class = "btn-success btn-lg btn-block",
                                                         style = "font-weight:bold;")),
                                        div(class = "alert alert-info", style = "font-size:12px;",
                                            icon("info-circle"),
                                            " L'AFD ne se lance qu'après ce clic. Modifier les variables ou le facteur annule le lancement (recliquez pour relancer)."),
                    
                                        div(style = "background-color: #e3f2fd; border-left: 4px solid #2196f3; padding: 10px; margin: 10px 0;",
                                            h5(style = "margin-top: 0; color: #495057;", icon("chart-line"), " Sélection des axes à représenter"),
                                            fluidRow(
                                              column(6,
                                                     uiOutput("afdAxisXSelect")
                                              ),
                                              column(6,
                                                     uiOutput("afdAxisYSelect")
                                              )
                                            ),
                                            p(style = "margin: 5px 0 0 0; font-size: 11px; color: #495057; font-style: italic;",
                                              icon("info-circle"), " Choisissez les fonctions discriminantes à afficher")
                                        ),
                    
                                        uiOutput("afdPredictVarsSelect"),
                                        div(style = "background-color: #d1ecf1; border-left: 4px solid #17a2b8; padding: 10px; margin: 10px 0;",
                                            p(style = "margin: 0; font-size: 12px; color: #0c5460;",
                                              icon("info-circle"), 
                                              HTML(" <strong>Variables de prédiction:</strong> Sélectionnez des variables catégorielles supplémentaires pour enrichir la prédiction du modèle."))
                                        ),
                                        uiOutput("afdQualiSupSelect"),
                                        conditionalPanel(
                                          condition = "input.afdUseMeans == false || input.afdUseMeans == null",
                                          div(style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin: 15px 0;",
                                              checkboxInput("afdCrossValidation", 
                                                            HTML("<strong>Activer la validation croisée (Leave-One-Out)</strong>"), 
                                                            FALSE),
                                              p(style = "margin: 5px 0 0 25px; font-size: 12px; color: #856404;",
                                                icon("exclamation-triangle"), 
                                                " ATTENTION: La validation croisée peut être très longue sur de grands jeux de données.")
                                          )
                                        ),
                                        conditionalPanel(
                                          condition = "input.afdUseMeans == true",
                                          div(style = "background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 15px 0;",
                                              p(style = "margin: 0; font-size: 12px; color: #721c24;",
                                                icon("info-circle"), 
                                                HTML(" <strong>Note:</strong> La validation croisée Leave-One-Out n'est pas disponible avec les moyennes par groupe (nombre d'observations insuffisant)."))
                                          )
                                        ),
                                        hr(),
                                        div(style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin: 10px 0;",
                                            fluidRow(
                                              column(6,
                                                     checkboxInput("afdRoundResults", "Arrondir les résultats", value = FALSE)
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.afdRoundResults == true",
                                                       numericInput("afdDecimals", "Décimales:", value = 2, min = 0, max = 8, step = 1)
                                                     )
                                              )
                                            )
                                        ),
                    hr(),
                    tabsetPanel(
                      id = "afdTabs", type = "tabs",
                      tabPanel(
                        tagList(icon("chart-area"), " Graphiques"),
                        br(),
                                            div(style="background-color:#f4f6f8;border-left:4px solid #3c8dbc;padding:10px;margin-bottom:10px;",
                                              h5(style="margin-top:0;color:#495057;", icon("sliders-h"), " Taille des éléments"),
                                              fluidRow(
                                                column(3, sliderInput("afdPointSize", "Taille des points", min = 0.5, max = 8, value = HSTAT_GG_POINT_SIZE, step = 0.5)),
                                                column(3, hstat_lbl_slider("afdLabelSize", "Taille des labels des individus")),
                                                column(3, hstat_lbl_slider("afdVarLabelSize", "Taille des labels des variables")),
                                                column(3, sliderInput("afdLineWidth", "Largeur des flèches", min = 0.3, max = 4, value = HSTAT_GG_LINEWIDTH, step = 0.1))
                                              ),
                                              fluidRow(
                                                column(3, sliderInput("afdAxisTextSize", "Taille texte axes", min = 8, max = 22, value = HSTAT_GG_BASE_SIZE, step = 1))
                                              )
                                            ),
                                            h5("Personnalisation graphique:", style = "font-weight: bold; color: #495057;"),
                                            fluidRow(
                                              column(6,
                                                     hstat_mv_forme_ui("afdInd", "Apparence — projection des individus"),
                                                     hstat_mv_forme_ui("afdVar", "Apparence — contribution des variables"),
                                                     textInput("afdIndTitle", "Titre projection individus:", 
                                                               value = "AFD - Projection des individus"),
                                                     textInput("afdIndXLabel", "Label axe X:", value = ""),
                                                     textInput("afdIndYLabel", "Label axe Y:", value = ""),
                                                     checkboxInput("afdIndCenterAxes", "Centrer sur (0,0)", TRUE),
                                                     hr(),
                                                     h5("Options Téléchargement projection individus:"),
                                                     p(style = "font-size: 11px; color: #495057; font-style: italic;",
                                                       icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                                                     fluidRow(
                                                       column(6,
                                                              selectInput("afdInd_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")
                                                       ),
                                                       column(6,
                                                              numericInput("afdInd_dpi", "DPI:", value = 300, min = 72, max = HSTAT_DPI_MAX)
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(6,
                                                              numericInput("afdInd_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                                                       ),
                                                       column(6,
                                                              numericInput("afdInd_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                                                       )
                                                     ),
                                                     hstat_mv_dim_note_ui("afdInd")
                                              ),
                                              column(6,
                                                     textInput("afdVarTitle", "Titre contribution variables:", 
                                                               value = "AFD - Contribution des variables"),
                                                     textInput("afdVarXLabel", "Label axe X:", value = ""),
                                                     textInput("afdVarYLabel", "Label axe Y:", value = ""),
                                                     checkboxInput("afdVarCenterAxes", "Centrer sur (0,0)", TRUE),
                                                     hr(),
                                                     h5("Options Téléchargement contribution variables:"),
                                                     p(style = "font-size: 11px; color: #495057; font-style: italic;",
                                                       icon("magic"), " Dimensions calculées automatiquement selon le DPI"),
                                                     fluidRow(
                                                       column(6,
                                                              selectInput("afdVar_format", "Format:", choices = c("png","svg","pdf","tiff"), selected = "png")
                                                       ),
                                                       column(6,
                                                              numericInput("afdVar_dpi", "DPI:", value = 300, min = 72, max = HSTAT_DPI_MAX)
                                                       )
                                                     ),
                                                     fluidRow(
                                                       column(6,
                                                              numericInput("afdVar_width", "Largeur (px):", value = 1200, min = 400, max = 4000, step = 100)
                                                       ),
                                                       column(6,
                                                              numericInput("afdVar_height", "Hauteur (px):", value = 900, min = 300, max = 4000, step = 100)
                                                       )
                                                     ),
                                                     hstat_mv_dim_note_ui("afdVar")
                                              )
                                            ),
                                            hr(),
                                            div(style = "text-align: center;",
                                                downloadButton("downloadAfdDataXlsx", "Télécharger données (Excel)", 
                                                               class = "btn-success", style = "margin: 5px;"),
                                                downloadButton("downloadAfdDataCsv", "Télécharger données (CSV)", 
                                                               class = "btn-success", style = "margin: 5px;")
                                            ),
                                            hr(),
                                            fluidRow(
                                              column(12,
                                                     div(class = "box box-solid box-primary",
                                                         div(class = "box-header with-border",
                                                             h4(class = "box-title", "Projection des individus", style = "color: #fff;")
                                                         ),
                                                         div(class = "box-body",
                                                             div(style="max-width:850px;margin:0 auto;", plotOutput("afdIndPlot", height = "520px")),
                                                             downloadButton("downloadAfdIndPlot", "Télécharger projection")
                                                         )
                                                     )
                                              ),
                                              column(12,
                                                     div(class = "box box-solid box-primary",
                                                         div(class = "box-header with-border",
                                                             h4(class = "box-title", "Contribution des variables", style = "color: #fff;")
                                                         ),
                                                         div(class = "box-body",
                                                             div(style="max-width:850px;margin:0 auto;", plotOutput("afdVarPlot", height = "520px")),
                                                             downloadButton("downloadAfdVarPlot", "Télécharger contribution")
                                                         )
                                                     )
                                              )
                                            ),
                      ),
                      tabPanel(
                        tagList(icon("microscope"), " Métriques"),
                        br(),
                                            div(class = "box box-solid",
                                                div(class = "box-header with-border", style = "background-color: #d9534f; color: white;",
                                                    h4(class = "box-title", "Résultats détaillés de l'AFD", style = "color: white; font-weight: bold;")
                                                ),
                                                div(class = "box-body", style = "background-color: #f9f9f9;",
                                                    div(style = "background-color: #fdf2f8; border-left: 5px solid #d9534f; padding: 12px; margin-bottom: 15px;",
                                                        h5(style = "color: #922b21; font-weight: bold; margin-top: 0;",
                                                           icon("microscope"), " Métriques de validation de la classification"),
                                                        p(style = "font-size: 12px; color: #555; margin-bottom: 0;",
                                                          "Les métriques suivantes permettent d'évaluer la qualité et la robustesse de la partition obtenue. Elles doivent être analysées dans l'ordre présenté.")
                                                    ),
                                                    div(style = "max-height: 700px; overflow-y: auto; font-family: 'Courier New', monospace; font-size: 11px; background-color: white; padding: 15px; border-radius: 5px;",
                                                        uiOutput("afdSummary"))
                                                )
                                            ),
                      ),
                      tabPanel(
                        tagList(icon("cogs"), " Details techniques"),
                        br(),
                                            # ---- Export métriques AFD 
                                            div(style = "background: linear-gradient(135deg, #641e16 0%, #922b21 100%); border-radius: 10px; padding: 18px; margin-top: 10px;",
                                                h4(style = "color: white; font-weight: bold; margin-top: 0; text-align: center;",
                                                   icon("file-export"), " Export des métriques AFD"),
                                                p(style = "color: #f1948a; font-size: 12px; text-align: center; margin-bottom: 12px;",
                                                  "Variance expliquée, corrélations canoniques, eta², accuracy, Kappa de Cohen, matrice de confusion, taux par groupe"),
                                                fluidRow(
                                                  column(12, style = "text-align: center;",
                                                         downloadButton("downloadAfdMetricsXlsx",
                                                                        HTML(paste0(as.character(icon("file-excel")), " <strong>Métriques AFD (Excel)</strong>")),
                                                                        class = "btn-success",
                                                                        style = "margin: 4px; padding: 7px 16px;"),
                                                         downloadButton("downloadAfdMetricsCsv",
                                                                        HTML(paste0(as.character(icon("file-csv")), " <strong>Métriques AFD (CSV)</strong>")),
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
  div(
    style = "margin-top:18px;",

    # -- Selecteur de categorie MASQUE (le catalogue pilote 'mv_category') --
    # On conserve l'input radioGroupButtons (invisible) car le catalogue JS l'utilise
    # pour basculer la categorie affichee ; le bandeau visuel est supprime (redondant
    # avec le catalogue facon maquette).
    tags$div(style = "display:none;",
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
    conditionalPanel(
      condition = "input.mv_category == 'quanti'",
      .mv_category_header("Analyses multivariees QUANTITATIVES",
                          "ruler-combined", "#3c8dbc"),
      fluidRow(.mv_analysis_box(
        "kmeans", "Classification k-means (partitionnement)", "quanti",
        principes  = "Partitionne n individus en k groupes en minimisant iterativement l'inertie intra-classe (somme des carres aux centroides). Algorithme de Lloyd/Hartigan-Wong.",
        objectifs  = "Construire une typologie d'individus, segmenter une population, identifier des profils homogenes sur variables quantitatives.",
        taille     = c("Minimum : n &ge; 2&times;k", "Recommande : n &ge; 10&times;k",
                       "Ideal : n &ge; 30&times;k pour des centroides stables"),
        variables  = c("Minimum : p &ge; 2 variables numériques", "Recommande : p &ge; 3",
                       "Standardisation conseillee si échelles heterogenes"),
        intro = "Partitionnement non hiérarchique : le nombre de clusters est fixe a priori."
      )),
      fluidRow(.mv_analysis_box(
        "efa", "Analyse Factorielle Exploratoire (AFE)", "quanti",
        principes  = "Modèle a facteurs communs separant variance commune et variance spécifique. Extraction (ML, axes principaux) puis rotation (varimax/oblimin) pour simplifier la structure.",
        objectifs  = "Decouvrir les facteurs latents sous-jacents a un ensemble de variables, valider la structure d'un questionnaire, reduire la dimension.",
        taille     = c("Minimum : n &ge; 5&times;p", "Recommande : n &ge; 100",
                       "Ideal : n &ge; 200 et &ge; 10 individus / variable"),
        variables  = c("Minimum : p &ge; 3 variables numériques", "KMO &ge; 0,60 requis",
                       "Test de Bartlett significatif (p &lt; 0,05)"),
        intro = "Cherche une structure latente sans hypothese imposee (exploratoire)."
      )),
      fluidRow(.mv_analysis_box(
        "cfa", "Analyse Factorielle Confirmatoire (AFC-c)", "quanti",
        principes  = "Modèle d'equations structurelles : la structure facteurs <-> items est imposee a priori, puis estimée et evaluee par des indices d'ajustement.",
        objectifs  = "Tester un modèle de mesure théorique, confirmer la validite convergente et discriminante d'un instrument.",
        taille     = c("Minimum : n &ge; 100", "Recommande : n &ge; 200",
                       "Ideal : &ge; 10 individus par paramètre estimé"),
        variables  = c("Modèle specifie en syntaxe lavaan", "&ge; 3 indicateurs par facteur conseille",
                       "Variables numériques (estimateurs robustes sinon)"),
        intro = "Confirme un modèle de mesure pre-specifie. Renseignez la syntaxe du modèle."
      )),
      fluidRow(.mv_analysis_box(
        "mtmm", "Multi-Trait Multi-Method (MTMM)", "quanti",
        principes  = "Matrice de corrélations organisée en blocs traits &times; méthodes (Campbell &amp; Fiske, 1959). La diagonale de validité (même trait, méthodes différentes) est confrontée aux triangles hétérotrait-monométhode et hétérotrait-hétérométhode.",
        objectifs  = "Établir la validité convergente (un même trait mesuré par plusieurs méthodes converge) et discriminante (des traits distincts restent distincts), et quantifier les effets de méthode.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Idéal : n &ge; 200 pour des corrélations stables"),
        variables  = c("&ge; 2 traits &times; &ge; 2 méthodes", "1 variable numérique par combinaison trait &times; méthode",
                       "Nommage Trait_Méthode conseillé pour l'affectation automatique"),
        intro = "Chaque variable mesure UN trait par UNE méthode. Affectez trait et méthode a chaque variable (ou utilisez le nommage Trait_Méthode)."
      )),
      fluidRow(.mv_analysis_box(
        "pls", "Regression PLS / PLS-DA", "quanti",
        principes  = "Construit des composantes latentes maximisant la covariance entre les prédicteurs X et la réponse Y. Adaptée aux cas p &gt;&gt; n et forte multicolinéarité.",
        objectifs  = "Prédire une réponse (quantitative = PLS, catégorielle = PLS-DA) en grande dimension, identifier les variables influentes (VIP).",
        taille     = c("Fonctionne même si n &lt; p", "Recommande : n &ge; 20",
                       "Validation croisée conseillee"),
        variables  = c("1 variable réponse Y", "p &ge; 2 prédicteurs numériques X",
                       "Prédicteurs correles : aucun probleme"),
        intro = "Regression sur composantes latentes, robuste a la colinearite."
      )),
      fluidRow(.mv_analysis_box(
        "regmult", "Regression linéaire multiple", "quanti",
        principes  = "Estimé par moindres carres ordinaires une réponse quantitative comme combinaison linéaire de plusieurs prédicteurs.",
        objectifs  = "Expliquer et prédire une variable continue, quantifier l'effet de chaque prédicteur, controler des facteurs de confusion.",
        taille     = c("Minimum : n &ge; 10&times;p", "Recommande : n &ge; 15&times;p",
                       "Ideal : n &ge; 20&times;p"),
        variables  = c("1 réponse Y numérique", "p &ge; 1 prédicteur (numérique ou facteur)",
                       "Residus : normalité, homoscedasticite, indépendance"),
        intro = "Modèle explicatif/prédictif de référence pour une réponse continue."
      ))
    ),

    # =================== CATEGORIE QUALITATIVES ===================
    conditionalPanel(
      condition = "input.mv_category == 'quali'",
      .mv_category_header("Analyses multivariees QUALITATIVES / CATÉGORIELLES",
                          "shapes", "#e67e22"),
      fluidRow(.mv_analysis_box(
        "afc", "Analyse Factorielle des Correspondances (AFC)", "quali",
        principes  = "Decompose l'inertie du khi-deux d'une table de contingence ; compare les profils-lignes et profils-colonnes via la distance du khi-deux.",
        objectifs  = "Analyser et visualiser l'association entre DEUX variables qualitatives, reperer les modalités attractives ou repulsives.",
        taille     = c("Effectifs théoriques &ge; 5 par case conseille",
                       "Recommande : n &ge; 50", "Eviter cases vides"),
        variables  = c("Exactement 2 variables qualitatives", "Variable-ligne + variable-colonne",
                       "Modalités a effectif suffisant"),
        intro = "Association entre deux variables catégorielles (table croisée)."
      )),
      fluidRow(.mv_analysis_box(
        "mca", "Analyse des Correspondances Multiples (ACM)", "quali",
        principes  = "Generalise l'AFC a plus de deux variables qualitatives via le tableau disjonctif complet (ou tableau de Burt).",
        objectifs  = "Explorer la structure d'associations entre plusieurs variables qualitatives, positionner individus et modalités.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Regrouper les modalités rares (&lt; 5 %)"),
        variables  = c("Minimum : p &ge; 2 variables qualitatives", "Recommande : p &ge; 3",
                       "Variables nominales"),
        intro = "Structure d'associations entre plusieurs variables catégorielles."
      )),
      fluidRow(.mv_analysis_box(
        "kmodes", "Classification k-modes (partitionnement)", "quali",
        principes  = "Equivalent du k-means pour données qualitatives : dissimilarite d'appariement simple (Hamming), les centres sont des modes.",
        objectifs  = "Segmenter une population décrite par des variables catégorielles, construire une typologie qualitative.",
        taille     = c("Minimum : n &ge; 2&times;k", "Recommande : n &ge; 10&times;k",
                       "Ideal : n &ge; 30&times;k"),
        variables  = c("Minimum : p &ge; 2 variables qualitatives", "Recommande : p &ge; 3",
                       "Modalités a effectif suffisant"),
        intro = "Partitionnement non hiérarchique pour variables catégorielles."
      )),
      fluidRow(.mv_analysis_box(
        "lca", "Analyse en Classes Latentes (LCA)", "quali",
        principes  = "Modèle de melange probabiliste : sous hypothese d'indépendance locale conditionnelle, estimé des classes latentes par maximum de vraisemblance (EM).",
        objectifs  = "Identifier des sous-populations non observées a partir de variables catégorielles, clustering base sur un modèle.",
        taille     = c("Minimum : n &ge; 100", "Recommande : n &ge; 300",
                       "Plus de classes => plus d'effectif"),
        variables  = c("Minimum : p &ge; 3 variables qualitatives", "Variables catégorielles",
                       "Indépendance locale conditionnelle"),
        intro = "Clustering probabiliste : classes latentes derrière des réponses catégorielles."
      )),
      fluidRow(.mv_analysis_box(
        "logit", "Regression logistique / multinomiale", "quali",
        principes  = "Modèle linéaire generalise a lien logit estimé par maximum de vraisemblance ; produit des rapports de cotes (odds ratios).",
        objectifs  = "Prédire une réponse catégorielle (binaire ou multinomiale), quantifier l'effet des prédicteurs.",
        taille     = c("Regle : &ge; 10 evenements par prédicteur",
                       "Recommande : n &ge; 100", "Eviter la separation parfaite"),
        variables  = c("1 réponse Y catégorielle", "p &ge; 1 prédicteur (numérique ou facteur)",
                       "Indépendance des observations"),
        intro = "Modèle explicatif/prédictif pour une réponse catégorielle."
      ))
    ),

    # =================== CATEGORIE MIXTES ===================
    conditionalPanel(
      condition = "input.mv_category == 'mixte'",
      .mv_category_header("Analyses multivariees MIXTES (quanti + quali)",
                          "layer-group", "#00a65a"),
      fluidRow(.mv_analysis_box(
        "famd", "Analyse Factorielle de Données Mixtes (AFDM)", "mixte",
        principes  = "Combine ACP (variables quantitatives standardisees) et ACM (variables qualitatives), avec une ponderation equilibrant les deux types.",
        objectifs  = "Reduire la dimension d'un tableau melant variables quantitatives et qualitatives, visualiser individus et modalités.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Ideal : n &ge; 5&times;p"),
        variables  = c("Au moins 1 variable quantitative", "Au moins 1 variable qualitative",
                       "p &ge; 3 au total conseille"),
        intro = "Reduction de dimension pour un tableau de variables mixtes."
      )),
      fluidRow(.mv_analysis_box(
        "mfa", "Analyse Factorielle Multiple (AFM)", "mixte",
        principes  = "Analyse des données structurees en groupes de variables ; chaque groupe est equilibre par sa premiere valeur propre afin qu'aucun ne domine.",
        objectifs  = "Comparer et integrer plusieurs groupes de variables (bloc quantitatif et bloc qualitatif), etudier leur coherence.",
        taille     = c("Minimum : n &ge; 50", "Recommande : n &ge; 100",
                       "Ideal : n &ge; 5&times;p"),
        variables  = c("Bloc quantitatif : &ge; 1 variable", "Bloc qualitatif : &ge; 1 variable",
                       "Definir explicitement les deux blocs"),
        intro = "Integration de blocs de variables (bloc quanti + bloc quali)."
      )),
      fluidRow(.mv_analysis_box(
        "kproto", "Classification k-prototypes (partitionnement mixte)", "mixte",
        principes  = "Combine k-means (distance euclidienne sur le quantitatif) et k-modes (appariement sur le qualitatif), ponderes par un paramètre gamma.",
        objectifs  = "Segmenter une population décrite par des variables a la fois quantitatives et qualitatives.",
        taille     = c("Minimum : n &ge; 2&times;k", "Recommande : n &ge; 10&times;k",
                       "Ideal : n &ge; 30&times;k"),
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
      tabItem(tabName = "timeseries",
              mod_timeseries_ui("timeseries")
      ),
      tabItem(tabName = "ml",
              mod_ml_ui("ml")
      ),
      tabItem(tabName = "dl",
              mod_dl_ui("dl")
      ),
      tabItem(tabName = "threshold",
              mod_threshold_ui("threshold")
      ),

      # ---- Citer HStat ----
      tabItem(tabName = "cite",
        fluidRow(
          box(
            title = tagList(icon("quote-right"), " Citer HStat"),
            status = "primary", width = 12, solidHeader = TRUE,
            p("Si HStat vous a été utile dans un travail de recherche, un rapport ou une publication, ",
              "merci de le citer. Choisissez le style souhaité, puis copiez la citation."),
            fluidRow(
              column(5,
                radioButtons("citeStyle", tagList(icon("list"), " Style de citation"),
                  choiceNames = list(
                    "Texte (auteur-date)", "BibTeX (LaTeX)", "RIS (EndNote, Zotero, Mendeley)",
                    "APA (7e édition)", "Vancouver", "Markdown"),
                  choiceValues = list("text", "bibtex", "ris", "apa", "vancouver", "markdown"),
                  selected = "text")),
              column(7,
                div(style = "background:#f7f9fb;border:1px solid #d9e2ec;border-radius:8px;padding:14px;",
                  tags$strong(icon("file-lines"), " Citation"),
                  tags$pre(id = "citeText", style = "white-space:pre-wrap;word-break:break-word;margin-top:8px;background:#fff;border:1px solid #e1e8ed;border-radius:6px;padding:10px;font-size:12.5px;max-height:320px;overflow:auto;",
                           verbatimTextOutput("citeOutput", placeholder = TRUE)),
                  actionButton("citeCopy", tagList(icon("copy"), " Copier dans le presse-papiers"),
                               class = "btn-success", style = "margin-top:8px;"),
                  downloadButton("citeDownload", " Télécharger", class = "btn-info", style = "margin-top:8px;"))
              )
            ),
            hr(),
            p(style = "font-size:12px;color:#7f8c8d;",
              icon("info-circle"),
              HTML(" Dans R, vous pouvez aussi exécuter <code>citation(\"HStat\")</code> pour obtenir la citation officielle du package."))
          )
        )
      )

    )
  )
)
