#  HStat - Application Shiny d'analyse statistique

local({
  candidates <- if (.Platform$OS.type == "windows") {
    c("French_France.utf8", "C.UTF-8", "en_US.UTF-8")
  } else {
    c("fr_FR.UTF-8", "fr_FR.utf8", "en_US.UTF-8", "C.UTF-8", "C.utf8")
  }
  for (loc in candidates) {
    ok <- tryCatch(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "",
                   error = function(e) FALSE)
    if (isTRUE(ok)) break
  }
})

source("Utils.R",  local = FALSE, encoding = "UTF-8")
source("mod_ai.R", local = FALSE, encoding = "UTF-8")
source("mod_report.R", local = FALSE, encoding = "UTF-8")
source("mod_threshold.R", local = FALSE, encoding = "UTF-8")
source("mod_explore.R", local = FALSE, encoding = "UTF-8")
source("mod_clean.R", local = FALSE, encoding = "UTF-8")
source("mod_filter.R", local = FALSE, encoding = "UTF-8")
source("mod_descriptive.R", local = FALSE, encoding = "UTF-8")
source("mod_viz.R", local = FALSE, encoding = "UTF-8")
# `mod_tests.R` a rejoint le paquet (R/mod_tests.R) : il est charge par le pont,
# avec le socle. Un module migre n'a plus de ligne ici -- et plus de place dans
# l'ordre de source(), qui etait la contrainte a lever.
source("mod_design.R", local = FALSE, encoding = "UTF-8")
source("mod_coding.R", local = FALSE, encoding = "UTF-8")
source("mod_qualitative.R", local = FALSE, encoding = "UTF-8")
source("mod_timeseries.R", local = FALSE, encoding = "UTF-8")
source("mod_ml.R", local = FALSE, encoding = "UTF-8")
source("mod_dl.R", local = FALSE, encoding = "UTF-8")
.hstat_ui_err <- NULL
tryCatch(
  source("UX.R", local = FALSE, encoding = "UTF-8"),
  error = function(e) {
    .hstat_ui_err <<- conditionMessage(e)
    message("HStat : erreur lors de la construction de l'interface : ",
            conditionMessage(e))
  })
source("app_server.R", local = FALSE, encoding = "UTF-8")

# Filet de securite : si la construction de `ui` a echoue (paquet d'interface
# manquant, etc.), on remplace par une UI de secours lisible plutot que de
# laisser Shiny afficher l'enigmatique "No UI defined".
if (!exists("ui") || is.null(ui) ||
    !(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") ||
      inherits(ui, "shiny.tag.function") || is.function(ui))) {
  ui <- shiny::fluidPage(
    shiny::tags$h2("HStat - interface indisponible"),
    shiny::tags$p("Certains paquets requis n'ont pas pu etre charges, ",
                  "l'interface n'a donc pas pu etre construite."),
    if (!is.null(.hstat_ui_err))
      shiny::tags$pre(style = "background:#fbeaea;padding:8px;border-radius:4px;",
                      paste("Detail :", .hstat_ui_err)),
    shiny::tags$p("Lancez l'application via ",
                  shiny::tags$code("HStat::run_hstat()"),
                  " : cette fonction installe automatiquement les dependances ",
                  "manquantes avant le demarrage."),
    shiny::tags$p("Vous pouvez aussi installer manuellement les paquets, ",
                  "puis relancer.")
  )
}
if (!exists("server") || !is.function(server)) {
  server <- function(input, output, session) {}
}

shinyApp(ui, server)
