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

# Les modules ont rejoint le paquet (R/mod_*.R) : le pont les charge avec le
# socle, dans un ordre qui n'a plus a etre tenu a la main. C'etait la contrainte
# a lever -- `mod_coding.R` devait etre source avant `mod_qualitative.R`, un
# test le gardait, et rien n'empechait un module ajoute plus tard de se glisser
# au mauvais endroit. Ne restent ici que les deux fichiers qui AGISSENT au
# chargement : `UX.R` construit `ui`, `app_server.R` definit `server`.
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
    shiny::tags$p("Certains paquets requis n'ont pas pu être chargés, ",
                  "l'interface n'a donc pas pu être construite."),
    if (!is.null(.hstat_ui_err))
      shiny::tags$pre(style = "background:#fbeaea;padding:8px;border-radius:4px;",
                      paste("Détail :", .hstat_ui_err)),
    shiny::tags$p("Lancez l'application via ",
                  shiny::tags$code("HStat::run_hstat()"),
                  " : cette fonction installe automatiquement les dépendances ",
                  "manquantes avant le démarrage."),
    shiny::tags$p("Vous pouvez aussi installer manuellement les paquets, ",
                  "puis relancer.")
  )
}
if (!exists("server") || !is.function(server)) {
  server <- function(input, output, session) {}
}

# Une dependance web annoncee sous un nom de fichier qui n'existe pas rend
# un 404 sur chaque page : on la repare avant de servir l'interface.
ui <- hstat_reparer_deps(ui)

shinyApp(ui, server)
