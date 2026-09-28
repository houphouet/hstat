## Message affiche au chargement du package (library(HStat) / require(HStat)).
## Propose immediatement a l'utilisateur comment citer HStat.
##
## La PHRASE DE CITATION N'EST PAS RECOPIEE ICI. Elle l'etait -- auteurs, titre,
## version et adresse -- et elle avait DERIVE : ce message ne nommait qu'un
## auteur sur deux quand `citation("HStat")` et le README en nommaient deux.
## Tout vient donc de `hstat_citation("text")`, qui lit DESCRIPTION ; la version
## et l'annee ne sont plus resolues une seconde fois ici non plus
## (`hstat_version()` et `hstat_pkg_year()` le font, avertissement de
## `packageDate()` neutralise compris -- il en emet un, et non une erreur,
## quand le paquet n'est pas installe).
##
## `strwrap()` plutot qu'un retour a la ligne pose a la main : la phrase
## s'allonge avec chaque auteur, et une coupe figee tomberait au mauvais
## endroit des le troisieme.

.onAttach <- function(libname, pkgname) {
  vers <- hstat_version()
  cit  <- tryCatch(hstat_citation("text"), error = function(e) NA_character_)

  msg <- paste0(
    "HStat ", vers, " charge.\n",
    "Pour lancer l'application : run_hstat()\n\n",
    "Si HStat vous est utile, merci de le citer :\n",
    if (!is.na(cit))
      paste0(paste(strwrap(cit, width = 78, indent = 2, exdent = 2),
                   collapse = "\n"), "\n\n") else "",
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
