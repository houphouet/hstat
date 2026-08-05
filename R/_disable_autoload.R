# Desactive le chargement automatique du sous-dossier R/ par Shiny.
#
# Lance depuis la racine du depot, Shiny sourcait R/run_hstat.R dans
# l'environnement de l'application (verifiable : shiny::loadSupport(".")
# y injectait l'objet `run_hstat`) et avertissait :
#   "Loading R/ subdirectory for Shiny application, but this directory appears
#    to contain an R package. Sourcing files in R/ may cause unexpected
#    behavior."
# Ces fichiers appartiennent au PAQUET (lanceur run_hstat() et .onAttach) :
# l'application n'en a pas besoin, inst/app/HStat.R sourcant explicitement tout
# ce qu'il lui faut.
#
# Shiny cherche ce fichier dans le sous-dossier R/ (et non a la racine) : voir
# shiny::loadSupport(), qui teste
#   list.files(file.path(appDir, "R"), "^_disable_autoload\\.r$").
# Sa seule presence suffit, Shiny ne lit pas son contenu.
#
# A savoir : l'avertissement ci-dessus continue d'apparaitre au demarrage. Shiny
# l'emet AVANT de tester la presence de ce fichier ; on ne peut donc pas le
# faire taire depuis l'application. Ce qui compte est corrige : plus aucun
# fichier de R/ n'est source dans l'environnement de l'application.
#
# Ce fichier ne definit aucun objet : R CMD build peut l'inclure sans effet.
