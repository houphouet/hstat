# ===========================================================================
# HStat - mod_report.R
# Rapport automatique : analyses, figures et interpretations en un document
# ---------------------------------------------------------------------------
# Ce que produisent SPSS ou Stata en fin de session : un document redige,
# transmissible tel quel. HStat le construit a partir de ce que les modules ont
# deja depose (`values$aiHistory`), sans qu'aucun d'eux ait a le savoir.
#
# TROIS FORMATS, ET UNE REGLE DE REPLI
#   HTML  toujours disponible : le document est assemble ici, en R, sans
#         dependance externe. C'est le format de reference.
#   Word  et PDF passent par rmarkdown, donc par pandoc (et par LaTeX pour le
#         PDF). Ces outils ne sont pas garantis presents sur la machine de
#         l'utilisateur : quand ils manquent, on le DIT et on rend le HTML,
#         plutot que d'echouer avec un message technique.
#
# Le rapport ne fabrique aucun resultat : il met en forme ce qui a ete calcule.
# Les interpretations qu'il reprend sont celles produites par l'assistance, avec
# la meme reserve — elles eclairent, elles ne valident pas.
# ===========================================================================

HSTAT_REPORT_FORMATS <- c(
  "HTML (toujours disponible)"     = "html",
  "Word (.docx)"                   = "docx",
  "PDF"                            = "pdf")

HSTAT_REPORT_SECTIONS <- c(
  "Résume du jeu de données"          = "donnees",
  "Diagnostic de qualité"             = "qualite",
  "Analyses menées (tableaux)"        = "analyses",
  "Figures"                           = "figures",
  "Interprétation rédigée"            = "interpretation",
  "Analyses recommandées"             = "reco",
  "Script R de reproductibilité"      = "script")

# Disponibilite reelle de chaque format sur CETTE machine.
hstat_report_formats_dispo <- function() {
  pandoc <- isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE))
  latex  <- pandoc && (nzchar(Sys.which("pdflatex")) ||
                       isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)))
  c(html = TRUE, docx = pandoc, pdf = latex)
}

hstat_report_message_dispo <- function(dispo = hstat_report_formats_dispo()) {
  manquants <- names(dispo)[!dispo]
  if (!length(manquants)) return(NULL)
  trf("Format(s) indisponible(s) sur cette machine : %s. %s%sLe HTML, lui, est toujours produit : il s'ouvre dans un navigateur et se colle dans un traitement de texte.", paste(toupper(manquants), collapse = ", "), if ("docx" %in% manquants) tr("Word exige pandoc (fourni avec RStudio ; sinon : pandoc.org). ") else "", if ("pdf" %in% manquants) tr("Le PDF exige en plus une distribution LaTeX (install.packages(\"tinytex\"); tinytex::install_tinytex()). ") else "")
}

# Resume du jeu de donnees porte en tete du rapport. Un relecteur doit pouvoir
# repondre a « sur quoi a-t-on travaille ? » sans ouvrir le fichier de donnees.
hstat_report_resume_donnees <- function(df, max_vars = 60L) {
  if (is.null(df) || !NCOL(df)) return(NULL)
  df <- as.data.frame(df)
  vars <- utils::head(names(df), max_vars)
  type <- vapply(vars, function(v) {
    x <- df[[v]]
    # Une colonne entierement vide est lue comme `logical` par les lecteurs de
    # CSV. Rendre « logical » tel quel affichait un nom de classe R anglais au
    # milieu d'un tableau francais, et ne disait pas ce qui compte : la colonne
    # est vide. Le repli final reste traduit pour la meme raison.
    if (all(is.na(x))) "vide (aucune valeur)"
    else if (is.numeric(x)) "numérique"
    else if (is.factor(x) || is.character(x)) "catégorielle"
    else if (inherits(x, c("Date", "POSIXct"))) "date"
    else if (is.logical(x)) "binaire (vrai / faux)"
    else sprintf("autre (%s)", class(x)[1])
  }, character(1))
  data.frame(
    Variable = vars, Type = unname(type),
    `Renseignées` = vapply(vars, function(v) sum(!is.na(df[[v]])), integer(1)),
    `Manquantes`  = vapply(vars, function(v) sum(is.na(df[[v]])), integer(1)),
    `Modalités / étendue` = vapply(vars, function(v) {
      x <- df[[v]]
      if (is.numeric(x) && any(!is.na(x)))
        trf("%s à %s", format(signif(min(x, na.rm = TRUE), 4)),
                format(signif(max(x, na.rm = TRUE), 4)))
      else if (is.factor(x) || is.character(x))
        trf("%d modalité(s)", length(unique(stats::na.omit(x))))
      else ""
    }, character(1)),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
}

# --- Figures ----------------------------------------------------------------
# Les modules deposent leur graphique sous forme de FONCTION (`plot = function()
# p`) et non d'objet : le graphique n'est dessine qu'au moment ou le rapport est
# demande. Une figure devenue indessinable (donnees filtrees entre-temps,
# variable supprimee) rend alors NULL et disparait du document — elle ne fait
# pas tomber le rapport entier.
#
# RESOLUTION : 1000 dpi PAR DEFAUT, et jamais moins.
# Un rapport part chez un relecteur, un comite de lecture ou un imprimeur. Les
# revues scientifiques exigent couramment 300 a 600 dpi pour les figures ; a
# 150 dpi, une figure est nette a l'ecran et floue des qu'elle est imprimee, et
# le defaut ne se voit qu'une fois le document soumis. On tranche donc au-dessus
# de l'exigence usuelle.
#
# Le cout est mesure, pas suppose : a 9 x 5,5 pouces, un nuage de points type
# pese 0,36 Mo a 1000 dpi (contre 0,04 Mo a 150) pour 2,3 s de trace. Vingt
# figures tiennent donc dans ~7 Mo et une minute — largement soutenable pour un
# document qu'on produit une fois.
#
# `HSTAT_REPORT_DPI_MIN` est un PLANCHER, pas une valeur par defaut : un
# appelant qui demanderait moins est remonte a 1000. Seul l'apercu a l'ecran y
# echappe explicitement (voir `apercu = TRUE`) — y incorporer une image de
# 9000 px en base64 n'ajouterait rien de visible et rendrait l'onglet poussif.

HSTAT_REPORT_DPI_MIN <- 1000L

# Resolutions proposees a l'utilisateur. Toutes >= au plancher.
HSTAT_REPORT_DPI <- c(
  "1000 dpi — qualité d'impression (défaut)" = "1000",
  "1200 dpi — exigences éditoriales strictes" = "1200",
  "2400 dpi — très haute définition (fichiers lourds)" = "2400")

.hstat_rep_dessine <- function(f, fichier, largeur, hauteur, dpi) {
  obj <- tryCatch(f(), error = function(e) NULL)
  if (is.null(obj)) return(FALSE)
  # L'ECRITURE PASSE PAR L'ECRIVAIN COMMUN. Le rapport n'a pas de raison d'avoir
  # son propre peripherique : il en heritait deux defauts. D'abord le plafond de
  # pixels -- a 2400 dpi (choix propose dans l'interface) une figure de 9 pouces
  # demande 21 600 px de cote, au-dela de ce qu'un bitmap peut allouer, et
  # l'utilisateur n'obtenait rien. Ensuite la garantie de format.
  #
  # Un enregistrement de graphique de base (`recordedplot`) se rejoue ; comme
  # l'ecrivain accepte une FONCTION, ce cas passe sans second peripherique.
  dessin <- if (inherits(obj, "recordedplot"))
              function() grDevices::replayPlot(obj) else obj
  # `secours = FALSE` : ici, et ici SEULEMENT, une figure indessinable doit
  # disparaitre du document plutot que d'y laisser une image d'erreur.
  isTRUE(hstat_ecrire_image(fichier, dessin, "png", largeur, hauteur, dpi,
                            secours = FALSE))
}

# Rend en PNG les figures de l'historique. Renvoie un data.frame (titre,
# fichier) des seules figures reellement produites.
hstat_report_figures <- function(history, dossier = tempdir(),
                                 largeur = 9, hauteur = 5.5,
                                 dpi = HSTAT_REPORT_DPI_MIN,
                                 max_figures = 20L, apercu = FALSE,
                                 progres = NULL) {
  # Le plancher s'applique au document. L'apercu a l'ecran en est dispense :
  # il sert a verifier la mise en page, pas a etre imprime.
  dpi <- if (isTRUE(apercu)) 150L
         else max(suppressWarnings(as.numeric(dpi)[1]), HSTAT_REPORT_DPI_MIN,
                  na.rm = TRUE)
  vide <- data.frame(titre = character(0), fichier = character(0),
                     stringsAsFactors = FALSE)
  if (!length(history)) return(vide)
  avec <- Filter(function(h) is.function(h$plot), history)
  if (!length(avec)) return(vide)
  if (length(avec) > max_figures) avec <- utils::tail(avec, max_figures)
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  res <- vide
  for (i in seq_along(avec)) {
    h <- avec[[i]]
    # A 1000 dpi une figure demande quelques secondes : sans ce rappel,
    # l'utilisateur ferait face a une minute de silence apres avoir clique.
    if (is.function(progres))
      tryCatch(progres(i, length(avec),
                       paste(c(h$module, h$title), collapse = " — ")),
               error = function(e) NULL)
    fichier <- file.path(dossier, sprintf("figure_%02d.png", i))
    ok <- .hstat_rep_dessine(h$plot, fichier, largeur, hauteur, dpi)
    if (!ok || !file.exists(fichier) || file.size(fichier) == 0) {
      unlink(fichier)
      next
    }
    res <- rbind(res, data.frame(
      titre = paste(c(h$module, h$title), collapse = " — "),
      fichier = fichier, stringsAsFactors = FALSE))
  }
  res
}

# --- Assemblage du document ------------------------------------------------
# Le corps est ecrit en MARKDOWN : c'est le seul format que pandoc convertit
# vers Word et PDF, et que l'on sait aussi rendre en HTML sans lui.

# Texte alternatif d'une image markdown. Crochets et retours a la ligne y sont
# structurants : `![Graphique : x]y](fig.png)` se lit comme un lien ferme apres
# « x », l'image n'est plus reconnue, et `![...](...)` ressort tel quel dans le
# document. Le titre vient d'un nom d'analyse, donc de donnees utilisateur.
.hstat_rep_alt <- function(x) {
  x <- gsub("[\r\n]+", " ", as.character(x))
  x <- gsub("[][]", "", x)
  trimws(x)
}

.hstat_rep_tableau_md <- function(df, max_lignes = 40L) {
  df <- as.data.frame(df)
  if (!nrow(df) || !ncol(df)) return("*(tableau vide)*")
  # Le total est retenu AVANT la troncature : le lire apres ferait annoncer
  # « 40 lignes au total, 40 affichees », ce qui ne signale plus rien.
  n_total <- nrow(df)
  tronque <- n_total > max_lignes
  df <- utils::head(df, max_lignes)
  # Un tableau markdown vit sur UNE ligne par enregistrement, et la barre
  # verticale y est le separateur de colonnes. Deux caracteres le detruisent
  # donc, et tous deux arrivent de vraies donnees :
  #   - le retour a la ligne, omnipresent dans les reponses libres du module
  #     qualitatif : la ligne se scinde et tout le tableau part de travers ;
  #   - la barre verticale, qu'un en-tete de CSV peut porter (« Rendement|t/ha »).
  # Les valeurs etaient deja protegees, PAS les noms de colonnes : l'en-tete
  # annoncait alors une colonne de plus que le separateur, et le tableau ne se
  # rendait plus du tout.
  md_cell <- function(x) {
    x <- gsub("[\r\n]+", " ", as.character(x))
    gsub("|", "\\|", x, fixed = TRUE)
  }
  fmt <- function(x) {
    if (is.numeric(x)) ifelse(is.na(x), "", format(signif(x, 5), trim = TRUE))
    else ifelse(is.na(x), "", md_cell(x))
  }
  cellules <- lapply(df, fmt)
  lignes <- c(
    paste0("| ", paste(md_cell(names(df)), collapse = " | "), " |"),
    paste0("|", paste(rep(" --- ", ncol(df)), collapse = "|"), "|"),
    vapply(seq_len(nrow(df)), function(i)
      paste0("| ", paste(vapply(cellules, function(c) c[i], character(1)),
                         collapse = " | "), " |"), character(1)))
  if (tronque) lignes <- c(lignes, "", trf("*(%d lignes au total, %d affichées)*",
                                               n_total, max_lignes))
  paste(lignes, collapse = "\n")
}

hstat_report_markdown <- function(history, titre = "Rapport d'analyse",
                                  auteur = "", contexte = "",
                                  # Les VALEURS du vecteur ("donnees", "qualite"...),
                                  # pas ses noms, qui sont les libelles affiches.
                                  sections = unname(HSTAT_REPORT_SECTIONS),
                                  donnees_resume = NULL, qualite = NULL,
                                  interpretation = NULL, reco = NULL,
                                  script = NULL, version = NULL,
                                  figures = NULL) {
  sections <- unname(sections)
  L <- c(sprintf("# %s", titre), "")
  if (nzchar(auteur))   L <- c(L, sprintf("**Auteur** : %s  ", auteur))
  L <- c(L, sprintf("**Date** : %s  ", format(Sys.Date(), "%d/%m/%Y")))
  if (!is.null(version)) L <- c(L, trf("**Produit avec** : HStat %s  ", version))
  if (nzchar(contexte)) L <- c(L, "", trf("**Contexte de l'étude** : %s", contexte))
  L <- c(L, "", "---", "")

  if ("donnees" %in% sections && !is.null(donnees_resume)) {
    L <- c(L, "## Données analysées", "", .hstat_rep_tableau_md(donnees_resume), "")
  }

  if ("qualite" %in% sections && !is.null(qualite) && NROW(qualite)) {
    L <- c(L, "## Diagnostic de qualité des données", "",
           "Chaque constat porte sa gravité et une suggestion concrète.", "",
           .hstat_rep_tableau_md(qualite), "")
  }

  if ("analyses" %in% sections && length(history)) {
    L <- c(L, "## Analyses menées", "")
    for (i in seq_along(history)) {
      h <- history[[i]]
      L <- c(L, sprintf("### %d. %s — %s", i, h$module %||% "", h$title %||% ""), "")
      if (!is.null(h$time))
        L <- c(L, trf("*Réalisée à %s.*", format(h$time, "%H:%M:%S")), "")
      par <- unlist(lapply(names(h$meta), function(k) {
        v <- h$meta[[k]]
        if (is.null(v) || !length(v)) return(NULL)
        sprintf("- **%s** : %s", k, paste(utils::head(as.character(v), 10), collapse = ", "))
      }))
      if (length(par)) L <- c(L, par, "")
      if (length(h$tables)) {
        for (nm in names(h$tables)) {
          L <- c(L, sprintf("**%s**", nm), "", .hstat_rep_tableau_md(h$tables[[nm]]), "")
        }
      } else {
        L <- c(L, "*Détail non conservé (analyse antérieure aux %d dernières).*", "")
        L[length(L) - 1L] <- trf(
          "*Détail non conservé : seules les %d dernières analyses gardent leurs tableaux.*",
          HSTAT_HIST_DETAIL)
      }
    }
  }

  if ("figures" %in% sections && NROW(figures)) {
    L <- c(L, "## Figures", "")
    for (i in seq_len(nrow(figures))) {
      L <- c(L, sprintf("**Figure %d — %s**", i, figures$titre[i]), "",
             # Chemin de fichier : pandoc l'incorpore dans le .docx, et le rendu
             # HTML le remplace par l'image en base64 (document autonome).
             # Le texte alternatif est ASSAINI : un crochet dans un titre
             # d'analyse fermait le `![...]` trop tot, l'image n'etait plus
             # reconnue et le markdown brut ressortait dans le document.
             sprintf("![%s](%s)", .hstat_rep_alt(figures$titre[i]),
                     figures$fichier[i]), "")
    }
  }

  if ("interpretation" %in% sections && !is.null(interpretation) &&
      nzchar(interpretation)) {
    L <- c(L, "## Interprétation", "", interpretation, "")
  }

  if ("reco" %in% sections && !is.null(reco) && NROW(reco)) {
    L <- c(L, "## Analyses appelées par le profil des données", "",
           .hstat_rep_tableau_md(reco), "",
           paste("*Ces propositions découlent du type des variables, de leurs",
                 "effectifs et de tests de normalité et d'homogénéité. Le choix",
                 "de l'analyse reste celui de l'analyste.*"), "")
  }

  if ("script" %in% sections && !is.null(script) && nzchar(script)) {
    L <- c(L, "## Annexe — script R de reproductibilité", "",
           "```r", strsplit(script, "\n")[[1]], "```", "")
  }

  L <- c(L, "---", "",
         paste("*Document produit automatiquement par HStat. Les interprétations",
               "qu'il contient éclairent la lecture des résultats ; elles ne les",
               "valident pas. À relire avant diffusion.*"))
  paste(L, collapse = "\n")
}

# --- Rendu ------------------------------------------------------------------
# Conversion markdown -> HTML propre au rapport. Celle de `mod_ai.R` ne connait
# que les titres, le gras et les listes : elle suffit a une reponse de modele,
# pas a un document fait de TABLEAUX, de blocs de code et de figures — un
# tableau y ressortirait en barres verticales au milieu d'un paragraphe.
.hstat_rep_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.hstat_rep_inline <- function(x) {
  x <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", x, perl = TRUE)
  x <- gsub("(?<!\\*)\\*([^*]+?)\\*(?!\\*)", "<em>\\1</em>", x, perl = TRUE)
  gsub("`([^`]+)`", "<code>\\1</code>", x, perl = TRUE)
}

.hstat_rep_est_ligne_tab <- function(t) grepl("^\\|.*\\|$", t)
.hstat_rep_est_separateur <- function(t) grepl("^\\|[\\s:|-]+\\|$", t, perl = TRUE)

.hstat_rep_cellules <- function(t) {
  t <- sub("^\\|", "", t); t <- sub("\\|$", "", t)
  # Decoupe sur les barres NON echappees seulement. Une barre appartenant a une
  # valeur est ecrite « \| » par `.hstat_rep_tableau_md` ; la couper ici
  # ajouterait une colonne fantome et decalerait toute la ligne.
  cel <- strsplit(t, "(?<!\\\\)\\|", perl = TRUE)[[1]]
  trimws(gsub("\\|", "|", cel, fixed = TRUE))
}

.hstat_rep_md_to_html <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return("")
  lines <- strsplit(.hstat_rep_esc(txt), "\n", fixed = TRUE)[[1]]
  out <- character(0)
  i <- 1L
  n <- length(lines)
  in_ul <- FALSE
  close_ul <- function() if (in_ul) { out <<- c(out, "</ul>"); in_ul <<- FALSE }

  while (i <= n) {
    t <- trimws(lines[i])

    # Bloc de code : recopie telle quelle jusqu'a la cloture. Le script R de
    # l'annexe ne doit surtout pas passer par les regles d'inline.
    if (grepl("^```", t)) {
      close_ul()
      j <- i + 1L
      corps <- character(0)
      while (j <= n && !grepl("^```", trimws(lines[j]))) {
        corps <- c(corps, lines[j]); j <- j + 1L
      }
      out <- c(out, "<pre><code>", paste(corps, collapse = "\n"), "</code></pre>")
      i <- j + 1L
      next
    }

    # Tableau : en-tete, ligne de separation, puis corps.
    if (.hstat_rep_est_ligne_tab(t) && i + 1L <= n &&
        .hstat_rep_est_separateur(trimws(lines[i + 1L]))) {
      close_ul()
      entete <- .hstat_rep_cellules(t)
      j <- i + 2L
      corps <- character(0)
      while (j <= n && .hstat_rep_est_ligne_tab(trimws(lines[j]))) {
        cel <- .hstat_rep_cellules(trimws(lines[j]))
        length(cel) <- length(entete)
        cel[is.na(cel)] <- ""
        corps <- c(corps, paste0("<tr>", paste0("<td>",
                  .hstat_rep_inline(cel), "</td>", collapse = ""), "</tr>"))
        j <- j + 1L
      }
      out <- c(out, "<table><thead><tr>",
               paste0("<th>", .hstat_rep_inline(entete), "</th>", collapse = ""),
               "</tr></thead><tbody>", corps, "</tbody></table>")
      i <- j
      next
    }

    if (!nzchar(t)) { close_ul(); i <- i + 1L; next }

    if (grepl("^(---+|\\*\\*\\*+)$", t)) {
      close_ul(); out <- c(out, "<hr>")
    } else if (grepl("^#{1,6} ", t)) {
      close_ul()
      k <- min(6L, nchar(sub("^(#+).*$", "\\1", t)))
      out <- c(out, sprintf("<h%d>%s</h%d>", k,
                            .hstat_rep_inline(sub("^#+ +", "", t)), k))
    } else if (grepl("^[-*+] ", t)) {
      if (!in_ul) { out <- c(out, "<ul>"); in_ul <- TRUE }
      out <- c(out, sprintf("<li>%s</li>", .hstat_rep_inline(sub("^[-*+] +", "", t))))
    } else {
      close_ul()
      out <- c(out, sprintf("<p>%s</p>", .hstat_rep_inline(t)))
    }
    i <- i + 1L
  }
  close_ul()
  paste(out, collapse = "\n")
}

# Incorporation des images dans le HTML. Le document doit rester un fichier
# UNIQUE : un rapport qui pointerait vers /tmp s'afficherait sans ses figures
# des qu'on l'envoie a un relecteur, ou apres redemarrage de la machine.
.hstat_rep_images_html <- function(html) {
  motif <- "!\\[([^]]*)\\]\\(([^)]+)\\)"
  occ <- unique(unlist(regmatches(html, gregexpr(motif, html, perl = TRUE))))
  for (o in occ) {
    alt    <- sub(motif, "\\1", o, perl = TRUE)
    chemin <- sub(motif, "\\2", o, perl = TRUE)
    b64 <- if (file.exists(chemin))
      tryCatch(base64enc::base64encode(chemin), error = function(e) "") else ""
    remp <- if (nzchar(b64))
      sprintf(paste0("<img src=\"data:image/png;base64,%s\" alt=\"%s\" ",
                     "style=\"max-width:100%%;height:auto;border:1px solid #dde;",
                     "border-radius:6px;margin:.6rem 0;\">"),
              b64, hstat_html_escape(alt))
    else
      sprintf("<em>(figure indisponible : %s)</em>",
              hstat_html_escape(basename(chemin)))
    html <- gsub(o, remp, html, fixed = TRUE)
  }
  html
}

# Renvoie list(ok, format_rendu, message) : le format effectivement produit
# peut differer de celui demande si l'outil manque, et l'appelant doit pouvoir
# le dire a l'utilisateur.
hstat_report_render <- function(markdown, fichier, format = "html",
                                titre = "Rapport d'analyse",
                                dispo = hstat_report_formats_dispo()) {
  format <- match.arg(format, c("html", "docx", "pdf"))
  replis <- character(0)
  if (!isTRUE(dispo[[format]])) {
    replis <- trf("Format %s indisponible sur cette machine, repli sur HTML.",
                      toupper(format))
    format <- "html"
  }

  if (identical(format, "html")) {
    # HTML assemble ici : aucune dependance, le format de reference ne peut
    # pas echouer parce qu'un outil externe manque.
    corps <- .hstat_rep_images_html(.hstat_rep_md_to_html(markdown))
    html <- paste0(
      "<!DOCTYPE html>\n<html lang=\"fr\"><head><meta charset=\"utf-8\">\n",
      "<title>", hstat_html_escape(titre), "</title>\n<style>\n",
      "body{font-family:Georgia,'Times New Roman',serif;max-width:900px;margin:2.5rem auto;",
      "padding:0 1.5rem;line-height:1.65;color:#222;}\n",
      "h1{border-bottom:3px solid #2e86c1;padding-bottom:.4rem;}\n",
      "h2{color:#1b6f8c;margin-top:2.2rem;border-bottom:1px solid #dde;padding-bottom:.2rem;}\n",
      "h3{color:#555;margin-top:1.6rem;}\n",
      "table{border-collapse:collapse;width:100%;margin:1rem 0;font-size:.92rem;}\n",
      "th,td{border:1px solid #ccd;padding:.42rem .6rem;text-align:left;}\n",
      "th{background:#eef4fa;}\ntr:nth-child(even) td{background:#fafbfc;}\n",
      "code,pre{font-family:'IBM Plex Mono',Consolas,monospace;font-size:.86rem;}\n",
      "pre{background:#f6f8fa;border:1px solid #e1e4e8;border-radius:6px;padding:1rem;overflow-x:auto;}\n",
      "em{color:#666;}\nhr{border:none;border-top:1px solid #dde;margin:2rem 0;}\n",
      "</style></head><body>\n", corps, "\n</body></html>")
    writeLines(html, fichier, useBytes = TRUE)
    return(list(ok = TRUE, format = "html",
                message = paste(replis, collapse = " ")))
  }

  # Word / PDF : on passe par rmarkdown. Le .Rmd est ecrit dans un dossier
  # temporaire pour ne rien laisser dans le repertoire de travail.
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("---", sprintf('title: "%s"', gsub('"', "'", titre)),
               "output:", sprintf("  %s: default",
                                  if (format == "docx") "word_document" else "pdf_document"),
               "---", "", markdown), tmp, useBytes = TRUE)
  res <- tryCatch({
    rmarkdown::render(tmp, output_file = basename(fichier),
                      output_dir = dirname(fichier), quiet = TRUE)
    list(ok = TRUE, format = format, message = paste(replis, collapse = " "))
  }, error = function(e) list(ok = FALSE, format = format,
                              message = conditionMessage(e)))
  if (isTRUE(res$ok)) return(res)

  # Echec de pandoc/LaTeX : on rend le HTML plutot que de laisser
  # l'utilisateur avec un fichier vide et un message technique.
  html <- hstat_report_render(markdown, fichier, "html", titre,
                              dispo = c(html = TRUE, docx = FALSE, pdf = FALSE))
  list(ok = TRUE, format = "html",
       message = trf("La conversion en %s a échoué (%s). Le rapport est rendu en HTML.",
                         toupper(format), substr(res$message, 1, 200)))
}
