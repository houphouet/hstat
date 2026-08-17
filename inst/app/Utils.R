# =============================================================================
#  HStat -- PONT vers le socle (R/utils.R)
# -----------------------------------------------------------------------------
#  Les ~5 700 lignes de definitions qui vivaient ici sont passees dans
#  `R/utils.R`, ou elles forment le code du paquet. Ce fichier garde deux roles,
#  et deux seulement :
#
#    1. AMENER LE SOCLE dans l'environnement de l'application ;
#    2. FAIRE CE QUI DOIT L'ETRE AU DEMARRAGE -- reglages de session, locale,
#       installation des paquets, alias anti-masquage, et replis des paquets
#       d'interface optionnels.
#
#  Pourquoi ce partage : dans un paquet R, le code de premier niveau est evalue
#  A L'INSTALLATION. Un repli du genre `if (!.hstat_has("plotly")) ...` y
#  figerait la decision prise sur la machine de construction, alors qu'elle
#  depend de ce qui est installe sur la machine d'execution. Ces expressions
#  restent donc du cote application, ou elles s'evaluent a chaque demarrage.
#
#  Ce fichier NE DEFINIT PLUS de fonction utilitaire -- sauf le chargeur
#  ci-dessous, qui ne peut pas venir d'ailleurs. Un test le verifie : une
#  definition qui reapparaitrait ici serait une seconde source de verite.
# =============================================================================

# --- 1. Chargement du socle ---------------------------------------------------
#  LES SOURCES PRIMENT SUR LE PAQUET INSTALLE. Sur un poste de developpement ou
#  HStat est aussi installe, charger le paquet ferait travailler l'application
#  sur une version anterieure a celle qu'on est en train d'editer -- defaut
#  particulierement penible parce qu'il ne se voit pas.
.hstat_charger_socle <- function(envir = globalenv()) {
  rel <- c(".", "..", file.path("..", ".."), file.path("..", "..", ".."))
  cands <- file.path(rel, "R", "utils.R")
  hit <- cands[file.exists(cands)]
  if (length(hit)) {
    sys.source(hit[1], envir = envir, keep.source = FALSE)
    return(invisible("sources"))
  }
  # Paquet installe : la couche R n'existe plus sous forme de fichier, on
  # recopie les objets de l'espace de noms. La recopie est necessaire parce que
  # l'application appelle aussi des aides internes (`.hstat_*`), qu'un simple
  # `library()` ne rendrait pas visibles.
  if (isTRUE(requireNamespace("HStat", quietly = TRUE))) {
    ns <- asNamespace("HStat")
    techniques <- c(".__NAMESPACE__.", ".__S3MethodsTable__.", ".packageName",
                    ".onLoad", ".onAttach", ".onUnload")
    for (nm in setdiff(ls(ns, all.names = TRUE), techniques))
      assign(nm, get(nm, envir = ns), envir = envir)
    return(invisible("paquet"))
  }
  stop("HStat : socle introuvable. Attendu R/utils.R a la racine du depot, ",
       "ou le paquet HStat installe.", call. = FALSE)
}
.hstat_charger_socle()

# --- 2. Demarrage de l'application -------------------------------------------


options(encoding = "UTF-8")

# Limite de taille des fichiers televerses (defaut Shiny = 5 Mo).
# Shiny ecrit l'upload en continu sur disque (pas en RAM) : la seule vraie
# contrainte est l'espace disque et la patience reseau. Defaut : 100 Go,
# configurable via HSTAT_MAX_UPLOAD_MB. Reduire cette valeur en cas de
# deploiement multi-utilisateurs.
.hstat_max_mb <- suppressWarnings(as.numeric(Sys.getenv("HSTAT_MAX_UPLOAD_MB", "102400")))
if (!is.finite(.hstat_max_mb) || .hstat_max_mb <= 0) .hstat_max_mb <- 102400
options(shiny.maxRequestSize = .hstat_max_mb * 1024^2)

# --- Qualite d'affichage des graphiques --------------------------------------

options(shiny.plot.res = 96)

# --- Locale UTF-8 robuste ----------------------------------------------------

local({
  candidates <- if (.Platform$OS.type == "windows") {
    c("French_France.utf8", "French_France.1252", "C.UTF-8", "en_US.UTF-8")
  } else {
    c("fr_FR.UTF-8", "fr_FR.utf8", "en_US.UTF-8", "C.UTF-8", "C.utf8")
  }
  for (loc in candidates) {
    ok <- tryCatch(
      suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "",
      error = function(e) FALSE)
    if (isTRUE(ok)) break
  }
})


install_and_load(required_packages)
hstat_load_model_packages()

# -- Blindage contre la pollution du chemin de recherche -----------------------
# L'application ne controle pas l'environnement R de l'utilisateur : un
# .Rprofile, un workspace RStudio restaure ou un library() anterieur peut
# attacher des packages dont les exports masquent des fonctions vitales.
# Cas reel observe : mclust attache dans la session -> mclust::em(modelName,
# data, parameters) masque shiny::em() et la construction de l'interface
# echoue avec "l'argument modelName est manquant". De meme,
# randomForest::margin masquerait ggplot2::margin dans tous les themes.
# Ces liaisons globales explicites sont resolues AVANT le chemin de
# recherche par tout le code de l'application (source(local = FALSE)) :
# elles rendent l'interface immunisee, quel que soit l'etat de la session.
em     <- shiny::em
margin <- ggplot2::margin

# shinycssloaders::withSpinner -> renvoie l'output tel quel si absent
if (!exists("withSpinner") && !.hstat_has("shinycssloaders")) {
  withSpinner <- function(ui_element, ...) ui_element
}

# plotly : sorties/rendu -> repli sur plotOutput/renderPlot statiques.

if (!.hstat_has("plotly")) {
  if (!exists("plotlyOutput"))
    plotlyOutput <- function(outputId, width = "100%", height = "400px", ...)
      shiny::plotOutput(outputId, width = width, height = height)
  if (!exists("ggplotly")) ggplotly <- function(p, ...) p
  # layout()/config() en no-op : renvoient l'objet graphique tel quel.

  layout <- function(p, ...) p
  config <- function(p, ...) p
  # Ne PAS redefinir %>% si un vrai pipe (magrittr via dplyr) existe deja : un repli
  # naif `rhs(lhs)` perdrait les arguments (p %>% layout(x=1) deviendrait layout(p)).
  if (!exists("%>%")) {
    if (.hstat_has("magrittr")) {
      `%>%` <- magrittr::`%>%`
    } else {
      # Repli minimal correct : insere lhs comme 1er argument de l'appel rhs.
      `%>%` <- function(lhs, rhs) {
        rc <- substitute(rhs)
        if (is.call(rc)) {
          new <- as.call(c(rc[[1]], substitute(lhs), as.list(rc)[-1]))
          eval(new, parent.frame())
        } else rhs(lhs)
      }
    }
  }
  if (!exists("renderPlotly"))
    renderPlotly <- function(expr, ...) {
      q <- substitute(expr)
      pf <- parent.frame()
      shiny::renderPlot({
        val <- eval(q, envir = pf)
        if (inherits(val, c("ggplot", "gg"))) print(val)
        else if (inherits(val, "grob")) grid::grid.draw(val)
        else val
      })
    }
}

# colourpicker::colourInput -> textInput de repli 
if (!.hstat_has("colourpicker")) {
  colourInput <- function(inputId, label, value = "#000000", ...)
    shiny::textInput(inputId, label, value = value)
}

# shinyWidgets : pickerInput / radioGroupButtons -> equivalents shiny de base
if (!.hstat_has("shinyWidgets")) {
  if (!exists("pickerInput"))
    pickerInput <- function(inputId, label = NULL, choices, selected = NULL,
                            multiple = FALSE, options = NULL, choicesOpt = NULL, ...)
      shiny::selectInput(inputId, label, choices = choices, selected = selected,
                         multiple = multiple)
  if (!exists("radioGroupButtons"))
    radioGroupButtons <- function(inputId, label = NULL, choices = NULL,
                                  selected = NULL, ...)
      shiny::radioButtons(inputId, label, choices = choices,
                          selected = selected %||% NULL, inline = TRUE)
  if (!exists("updatePickerInput"))
    updatePickerInput <- function(session, inputId, ..., choices = NULL,
                                  selected = NULL)
      shiny::updateSelectInput(session, inputId, choices = choices,
                               selected = selected)
}

# sortable::rank_list -> selecteur multiple ordonne de repli
if (!.hstat_has("sortable") && !exists("rank_list")) {
  rank_list <- function(text = NULL, labels = NULL, input_id, ...)
    shiny::selectInput(input_id, label = text, choices = labels,
                       selected = labels, multiple = TRUE)
}

# Le nettoyage est pose sur `renderPlotly` lui-meme plutot qu'a chacun des
# appels : les corps de ces sorties comportent plusieurs `return()`, et un
# habillage de l'expression serait purement et simplement saute par le premier
# d'entre eux. `exprToFunction` transforme le bloc en fonction — le `return()`
# en sort alors normalement, et la valeur passe bien par le nettoyage.
if (.hstat_has("plotly")) {
  renderPlotly <- function(expr, env = parent.frame(), quoted = FALSE) {
    fn <- shiny::exprToFunction(expr, env, quoted)
    plotly::renderPlotly(hstat_plotly_clean(fn()),
                         env = environment(), quoted = FALSE)
  }
}
