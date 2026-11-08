# Point d entree standard Shiny : runApp(dossier) detecte ce fichier et sert
# automatiquement le sous-dossier www/ (feuille de style + polices).
.hstat_app <- source("HStat.R", local = FALSE, encoding = "UTF-8")$value

if (!inherits(.hstat_app, "shiny.appobj")) {
  stop("HStat : l'application n'a pas pu être construite (vérifiez que tous ",
       "les paquets requis sont installés). Lancez de préférence via ",
       "HStat::run_hstat(), qui installe les dependances manquantes.",
       call. = FALSE)
}

.hstat_app
