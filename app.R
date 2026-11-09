# Point d'entree pour les PLATEFORMES D'HEBERGEMENT Shiny (shinyapps.io, Posit
# Connect, Shiny Server, ou `shiny::runApp()` a la racine).
# Ces plateformes cherchent un app.R / ui.R / www/index.html a la RACINE du
# projet deploye.

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
