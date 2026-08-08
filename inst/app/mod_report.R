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
  "Resume du jeu de donnees"          = "donnees",
  "Diagnostic de qualite"             = "qualite",
  "Analyses menees (tableaux)"        = "analyses",
  "Figures"                           = "figures",
  "Interpretation redigee"            = "interpretation",
  "Analyses recommandees"             = "reco",
  "Script R de reproductibilite"      = "script")

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
  paste0(
    "Format(s) indisponible(s) sur cette machine : ",
    paste(toupper(manquants), collapse = ", "), ". ",
    if ("docx" %in% manquants)
      "Word exige pandoc (fourni avec RStudio ; sinon : pandoc.org). " else "",
    if ("pdf" %in% manquants)
      "Le PDF exige en plus une distribution LaTeX (install.packages(\"tinytex\"); tinytex::install_tinytex()). " else "",
    "Le HTML, lui, est toujours produit : il s'ouvre dans un navigateur et ",
    "se colle dans un traitement de texte.")
}

# Resume du jeu de donnees porte en tete du rapport. Un relecteur doit pouvoir
# repondre a « sur quoi a-t-on travaille ? » sans ouvrir le fichier de donnees.
hstat_report_resume_donnees <- function(df, max_vars = 60L) {
  if (is.null(df) || !NCOL(df)) return(NULL)
  df <- as.data.frame(df)
  vars <- utils::head(names(df), max_vars)
  type <- vapply(vars, function(v) {
    x <- df[[v]]
    if (is.numeric(x)) "numerique"
    else if (is.factor(x) || is.character(x)) "categorielle"
    else if (inherits(x, c("Date", "POSIXct"))) "date"
    else class(x)[1]
  }, character(1))
  data.frame(
    Variable = vars, Type = unname(type),
    `Renseignees` = vapply(vars, function(v) sum(!is.na(df[[v]])), integer(1)),
    `Manquantes`  = vapply(vars, function(v) sum(is.na(df[[v]])), integer(1)),
    `Modalites / etendue` = vapply(vars, function(v) {
      x <- df[[v]]
      if (is.numeric(x) && any(!is.na(x)))
        sprintf("%s a %s", format(signif(min(x, na.rm = TRUE), 4)),
                format(signif(max(x, na.rm = TRUE), 4)))
      else if (is.factor(x) || is.character(x))
        sprintf("%d modalite(s)", length(unique(stats::na.omit(x))))
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

.hstat_rep_dessine <- function(f, fichier, largeur, hauteur, dpi) {
  obj <- tryCatch(f(), error = function(e) NULL)
  if (is.null(obj)) return(FALSE)
  if (inherits(obj, "ggplot")) {
    return(isTRUE(tryCatch({
      ggplot2::ggsave(fichier, plot = obj, width = largeur, height = hauteur,
                      dpi = dpi, bg = "white")
      TRUE
    }, error = function(e) FALSE)))
  }
  # Graphique de base : soit un enregistrement (`recordedplot`), soit un objet
  # qui sait s'imprimer. Le peripherique est ferme a la sortie de la fonction,
  # donc avant que l'appelant ne verifie le fichier.
  isTRUE(tryCatch({
    grDevices::png(fichier, width = largeur * dpi, height = hauteur * dpi,
                   res = dpi, bg = "white")
    on.exit(grDevices::dev.off(), add = TRUE)
    if (inherits(obj, "recordedplot")) grDevices::replayPlot(obj) else print(obj)
    TRUE
  }, error = function(e) FALSE))
}

# Rend en PNG les figures de l'historique. Renvoie un data.frame (titre,
# fichier) des seules figures reellement produites.
hstat_report_figures <- function(history, dossier = tempdir(),
                                 largeur = 9, hauteur = 5.5, dpi = 150,
                                 max_figures = 20L) {
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

.hstat_rep_tableau_md <- function(df, max_lignes = 40L) {
  df <- as.data.frame(df)
  if (!nrow(df) || !ncol(df)) return("*(tableau vide)*")
  # Le total est retenu AVANT la troncature : le lire apres ferait annoncer
  # « 40 lignes au total, 40 affichees », ce qui ne signale plus rien.
  n_total <- nrow(df)
  tronque <- n_total > max_lignes
  df <- utils::head(df, max_lignes)
  fmt <- function(x) {
    if (is.numeric(x)) ifelse(is.na(x), "", format(signif(x, 5), trim = TRUE))
    else ifelse(is.na(x), "", gsub("|", "\\|", as.character(x), fixed = TRUE))
  }
  cellules <- lapply(df, fmt)
  lignes <- c(
    paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep(" --- ", ncol(df)), collapse = "|"), "|"),
    vapply(seq_len(nrow(df)), function(i)
      paste0("| ", paste(vapply(cellules, function(c) c[i], character(1)),
                         collapse = " | "), " |"), character(1)))
  if (tronque) lignes <- c(lignes, "", sprintf("*(%d lignes au total, %d affichees)*",
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
  if (!is.null(version)) L <- c(L, sprintf("**Produit avec** : HStat %s  ", version))
  if (nzchar(contexte)) L <- c(L, "", sprintf("**Contexte de l'etude** : %s", contexte))
  L <- c(L, "", "---", "")

  if ("donnees" %in% sections && !is.null(donnees_resume)) {
    L <- c(L, "## Donnees analysees", "", .hstat_rep_tableau_md(donnees_resume), "")
  }

  if ("qualite" %in% sections && !is.null(qualite) && NROW(qualite)) {
    L <- c(L, "## Diagnostic de qualite des donnees", "",
           "Chaque constat porte sa gravite et une suggestion concrete.", "",
           .hstat_rep_tableau_md(qualite), "")
  }

  if ("analyses" %in% sections && length(history)) {
    L <- c(L, "## Analyses menees", "")
    for (i in seq_along(history)) {
      h <- history[[i]]
      L <- c(L, sprintf("### %d. %s — %s", i, h$module %||% "", h$title %||% ""), "")
      if (!is.null(h$time))
        L <- c(L, sprintf("*Realisee a %s.*", format(h$time, "%H:%M:%S")), "")
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
        L <- c(L, "*Detail non conserve (analyse anterieure aux %d dernieres).*", "")
        L[length(L) - 1L] <- sprintf(
          "*Detail non conserve : seules les %d dernieres analyses gardent leurs tableaux.*",
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
             sprintf("![%s](%s)", figures$titre[i], figures$fichier[i]), "")
    }
  }

  if ("interpretation" %in% sections && !is.null(interpretation) &&
      nzchar(interpretation)) {
    L <- c(L, "## Interpretation", "", interpretation, "")
  }

  if ("reco" %in% sections && !is.null(reco) && NROW(reco)) {
    L <- c(L, "## Analyses appelees par le profil des donnees", "",
           .hstat_rep_tableau_md(reco), "",
           paste("*Ces propositions decoulent du type des variables, de leurs",
                 "effectifs et de tests de normalite et d'homogeneite. Le choix",
                 "de l'analyse reste celui de l'analyste.*"), "")
  }

  if ("script" %in% sections && !is.null(script) && nzchar(script)) {
    L <- c(L, "## Annexe — script R de reproductibilite", "",
           "```r", strsplit(script, "\n")[[1]], "```", "")
  }

  L <- c(L, "---", "",
         paste("*Document produit automatiquement par HStat. Les interpretations",
               "qu'il contient eclairent la lecture des resultats ; elles ne les",
               "valident pas. A relire avant diffusion.*"))
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
  trimws(strsplit(t, "|", fixed = TRUE)[[1]])
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
    replis <- sprintf("Format %s indisponible sur cette machine, repli sur HTML.",
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
       message = sprintf("La conversion en %s a echoue (%s). Le rapport est rendu en HTML.",
                         toupper(format), substr(res$message, 1, 200)))
}
