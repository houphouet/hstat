# =============================================================================
# Point d'entree pour les PLATEFORMES D'HEBERGEMENT Shiny
# (shinyapps.io, Posit Connect, Shiny Server, ou `shiny::runApp()` a la racine).
#
# Ces plateformes cherchent un app.R / ui.R / www/index.html a la RACINE du
# projet deploye. La logique de l'application vit dans inst/app/ (structure de
# package R) ; ce fichier fait donc le pont vers inst/app/.
#
# Pour lancer depuis le PACKAGE installe, utilisez plutot : HStat::run_hstat()
# -----------------------------------------------------------------------------
# IMPORTANT -- pourquoi shinyAppDir() et surtout PAS setwd() + source() :
#
# Shiny sert le dossier www/ a la racine de l'URL en se basant sur le dossier
# de l'application, resolu AVANT l'evaluation de ce fichier. Un setwd() effectue
# ici arrive donc trop tard : Shiny continue de chercher www/ a la racine du
# depot, ou il n'existe pas. Resultat, une fois deploye, l'application repondait
# 404 sur hstat-theme.css, Sortable.min.js et toutes les polices -- elle
# s'affichait sans son theme et sans le glisser-deposer.
#
# shinyAppDir() declare inst/app comme LE dossier de l'application : Shiny y
# trouve app.R, y source le code et y sert www/. Aucun setwd() n'est necessaire
# (setwd() modifie de surcroit l'etat global du processus R, ce qui est a
# proscrire dans une application servie).
# =============================================================================

app_dir <- "inst/app"
if (!file.exists(file.path(app_dir, "HStat.R"))) {
  # Cas d'un package installe : inst/ est aplati, l'app est sous app/
  alt <- system.file("app", package = "HStat")
  if (nzchar(alt) && file.exists(file.path(alt, "HStat.R"))) {
    app_dir <- alt
  } else {
    stop("Impossible de localiser le code de l'application (inst/app/HStat.R).",
         call. = FALSE)
  }
}

shiny::shinyAppDir(app_dir)
