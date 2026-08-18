## Message affiche au chargement du package (library(HStat) / require(HStat)).
## Propose immediatement a l'utilisateur comment citer HStat.

.onAttach <- function(libname, pkgname) {
  vers <- tryCatch(as.character(utils::packageVersion("HStat")),
                   error = function(e) "")
  year <- tryCatch(sub("-.*", "", as.character(utils::packageDate("HStat"))),
                   error = function(e) NA_character_)
  if (is.null(year) || is.na(year) || !nzchar(year))
    year <- format(Sys.Date(), "%Y")

  msg <- paste0(
    "HStat ", vers, " charge.\n",
    "Pour lancer l'application : run_hstat()\n\n",
    "Si HStat vous est utile, merci de le citer :\n",
    "  KOUADIO, Houphouet (", year, "). HStat : Application Shiny interactive ",
    "pour l'analyse statistique.\n",
    "  Version ", vers, ". https://github.com/houphouet/hstat\n\n",
    "Citation complete et autres styles (BibTeX, RIS, APA...) : citation(\"HStat\")"
  )
  packageStartupMessage(msg)
}

## ---------------------------------------------------------------------------
##  Colonnes vues par ggplot2 et dplyr, declarees pour l'analyse statique
## ---------------------------------------------------------------------------
##  `aes(x = Efficacite)` designe une COLONNE, pas une variable de l'espace de
##  noms. L'analyseur de `R CMD check` ne peut pas le savoir et rend « no
##  visible binding for global variable » -- 112 fois ici. Les declarer eteint
##  la note SANS masquer de vraie faute : une variable reellement absente
##  serait toujours signalee, du moment qu'elle ne figure pas dans cette liste.
##
##  La liste est TIREE DE LA SORTIE DU CHECK, pas ecrite a la main : une liste
##  devinee serait a la fois trop courte (note qui subsiste) et trop longue
##  (vraie faute masquee).
##
##  Ce fichier est ecarte du chargement par le pont (`a_part` dans
##  `inst/app/Utils.R`) : `utils::globalVariables()` n'a de sens qu'a la
##  construction du paquet, et le socle doit rester inerte.
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
