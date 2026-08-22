# Point d entree standard Shiny : runApp(dossier) detecte ce fichier et sert
# automatiquement le sous-dossier www/ (feuille de style + polices).
#
# HStat.R est l application mono-fichier : il source les modules puis se
# termine par shinyApp(ui, server). On recupere cette valeur et on la
# RETOURNE explicitement a Shiny. Si la construction a echoue (par ex. un
# paquet d interface manquant), on leve une erreur claire plutot que de
# laisser Shiny afficher le trompeur "No UI defined".
.hstat_app <- source("HStat.R", local = FALSE, encoding = "UTF-8")$value

if (!inherits(.hstat_app, "shiny.appobj")) {
  stop("HStat : l'application n'a pas pu être construite (vérifiez que tous ",
       "les paquets requis sont installés). Lancez de préférence via ",
       "HStat::run_hstat(), qui installe les dependances manquantes.",
       call. = FALSE)
}

.hstat_app
