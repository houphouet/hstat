# Message affiche au chargement du package (library(HStat) / require(HStat)).
# Propose immediatement a l'utilisateur comment citer HStat.

.onAttach <- function(libname, pkgname) {
  vers <- hstat_version()
  cit  <- tryCatch(hstat_citation("text"), error = function(e) NA_character_)

  msg <- paste0(
    "HStat ", vers, " charge.\n",
    "Pour lancer l'application : run_hstat()\n",
    "Navigateur ou fen\u00eatre Shiny : run_hstat(ouverture = \"demander\")\n\n",
    "Si HStat vous est utile, merci de le citer :\n",
    if (!is.na(cit))
      paste0(paste(strwrap(cit, width = 78, indent = 2, exdent = 2),
                   collapse = "\n"), "\n\n") else "",
    "Citation complete et autres styles (BibTeX, RIS, APA...) : citation(\"HStat\")"
  )
  packageStartupMessage(msg)
}

# ---------------------------------------------------------------------------
# Colonnes vues par ggplot2 et dplyr, declarees pour l'analyse statique
# ---------------------------------------------------------------------------
# `aes(x = Efficacite)` designe une COLONNE, pas une variable de l'espace de
# noms.
utils::globalVariables(c(
  ".X", ".Y", ".blocklab", ".data", ".fill", ".lab", ".ltxt", ".lx", ".ly",
  ".x", ".x0g", ".x1g", ".xb", ".xc", ".xmax", ".xmin", ".y", ".y2_sd",
  "A", "B", "CV", "Classe", "Code", "Contribution", "Cooccurrence",
  "Ecart_type", "Effectif", "Efficacy", "Erreur_type", "Facteur", "Freq",
  "Frequence", "Groupe", "Importance", "Item", "Mesure", "Missing",
  "Modalite", "Mot", "Moyenne", "N", "Nb_modalites", "Nb_termes", "Niveau",
  "Observe", "Option", "PC1", "PC2", "Pct", "PctMissing", "Pct_manquant",
  "Pct_repondants", "Pourcentage", "Predit", "Residu", "Score", "Sens",
  "Theme", "Tonalite", "Total_manquants", "Treatment", "Type", "Variable",
  ":=", "acf", "block", "ci_margin", "couleur", "debut_pct", "density", "epoque",
  "est", "fin_pct", "fpr", "groupe", "hi", "hi80", "hi95", "lab", "lab2",
  "label", "lo", "lo80", "lo95", "lower", "marge", "max_val", "mid",
  "min_val", "modalite", "obs", "pct", "percentage", "perte", "power",
  "pred", "quoi", "rang", "residu", "self", "theoretical", "total", "tpr",
  "upper", "valeur", "value", "x", "x_var", "y", "y_var", "ymax", "ymin",
  "yy"
))
