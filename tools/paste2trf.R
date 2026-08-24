# =============================================================================
#  paste0("texte ", x, " suite")  ->  trf("texte %s suite", x)
# -----------------------------------------------------------------------------
#  Une phrase assemblee par paste0 n'existe JAMAIS dans le DOM sous forme de
#  fragments : le traducteur du navigateur ne remplace que des correspondances
#  COMPLETES. Seul le gabarit peut etre traduit, et c'est le role de trf().
#
#  Le reperage passe par l'ANALYSEUR de R (getParseData), pas par une
#  expression reguliere : il faut distinguer les litteraux des expressions, ce
#  qu'un motif ne sait pas faire, et connaitre la position exacte de l'appel.
#  `srcref` ne suffit pas : il n'est porte que par les expressions de premier
#  niveau, alors que ces appels sont presque tous imbriques.
#
#  La reecriture est TEXTUELLE : reconstruire depuis l'arbre perdrait tous les
#  commentaires du fichier.
#
#  Les appels MULTILIGNES sont traites aussi : l'etendue est remplacee d'un
#  bloc et repliee sur une ligne. Les COMMENTAIRES de l'etendue sont ressortis
#  en lignes propres juste apres l'appel reecrit -- les replier avec le code
#  les ferait disparaitre, ou pire, commenterait la suite de la ligne. Un
#  commentaire est valide partout en R, y compris au milieu d'une liste
#  d'arguments, et la re-analyse du fichier reste le garde-fou : un resultat
#  qui ne s'analyse pas laisse le fichier intact.
#
#  Usage, depuis la racine du depot :
#
#      Rscript tools/paste2trf.R              # simulation : compte et explique
#      Rscript tools/paste2trf.R --appliquer  # ecrit les fichiers
#
#  Le decompte des REFUS par motif est affiche : c'est lui qui a revele que
#  l'outil sautait toute ligne accentuee (cf. le decoupage en octets ci-dessous).
# =============================================================================
args      <- commandArgs(trailingOnly = TRUE)
appliquer <- length(args) >= 1 && identical(args[1], "--appliquer")

fichiers <- c(list.files("inst/app", pattern = "[.]R$", full.names = TRUE),
              list.files("R", pattern = "[.]R$", full.names = TRUE))
fichiers <- fichiers[!grepl("run_hstat|zzz|_disable", fichiers)]

# ------------------------------------------------------------------------
#  getParseData() COMPTE EN OCTETS, substr() EN CARACTERES.
#  Le decalage vaut donc le nombre d'octets surnumeraires de la ligne -- et il
#  est nul tant qu'elle est en ASCII. La premiere version du convertisseur
#  echouait ainsi sur TOUTE ligne accentuee, c'est-a-dire sur le francais :
#  `str2lang` recevait une etendue debordant sur la virgule suivante, levait
#  « unexpected ',' », et l'appel etait saute EN SILENCE. 243 fragments de
#  phrase sont restes en francais pour cette seule raison.
# ------------------------------------------------------------------------
coupe <- function(ligne, a, b) {
  r <- charToRaw(ligne)
  if (a > length(r) || b < a) return("")
  s <- rawToChar(r[a:min(b, length(r))])
  Encoding(s) <- "UTF-8"
  s
}
n_oct <- function(ligne) length(charToRaw(ligne))

francais <- function(x) {
  grepl("[éèêëàâäîïôöùûüçÉÈÊÀÂÎÔÛÇ]", x) ||
  grepl("\\b(le|la|les|des|une|un|du|de|et|ou|pour|dans|sur|avec|sans|est|sont|pas|plus|moins|aucun|aucune|valeur|valeurs|variable|variables|colonne|lignes|analyse|erreur|attention|impossible|selon|entre|vers|minimum|groupes|modeles)\\b",
        tolower(x), perl = TRUE)
}
codeish <- function(x) {
  grepl("://|<[a-zA-Z/]|\\{|\\}|px\\b|^#[0-9a-fA-F]|input\\[|\\$\\{", x) ||
  # jQuery / JavaScript : `$('#id').text('...')`. Sans ce garde-fou, un
  # fragment de script partait au dictionnaire -- et se serait fait traduire
  # au milieu du code envoye au navigateur.
  grepl("\\$\\('|'\\);|\\.text\\(|function\\s*\\(", x) ||
  grepl("^[a-z_.]+$", x)
}

total <- 0L; par_fichier <- integer(0); gabarits <- character(0)
.refus <- new.env(); for (k in c("<2 args","arg nomme","tout litt. ou aucun","pas francais","codeish","saut de ligne","un seul jeton")) .refus[[k]] <- 0L

for (f in fichiers) {
  brut <- readBin(f, "raw", file.size(f))
  crlf <- grepl("\r\n", rawToChar(brut[seq_len(min(length(brut), 5000))]), fixed = TRUE)
  src  <- readLines(f, warn = FALSE, encoding = "UTF-8")

  ex <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) next
  pd <- utils::getParseData(ex)
  if (is.null(pd) || !nrow(pd)) next

  dans <- FALSE; interdit <- logical(length(src))
  for (i in seq_along(src)) {
    if (grepl("^\\.?hstat_rlog_[a-z_]* <- function", src[i])) dans <- TRUE
    else if (dans && identical(src[i], "}")) dans <- FALSE
    interdit[i] <- dans
  }

  # Chaque appel a paste0 : le jeton de nom, puis l'`expr` qui le contient.
  jetons <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% c("paste0", "paste"), ]
  if (!nrow(jetons)) next
  appels <- integer(0)
  for (i in seq_len(nrow(jetons))) {
    p <- pd$parent[pd$id == jetons$id[i]]          # l'expr du nom
    gp <- pd$parent[pd$id == p]                    # l'expr de l'appel complet
    if (length(gp) && gp > 0) appels <- c(appels, gp)
  }
  appels <- unique(appels)
  info <- pd[pd$id %in% appels, ]
  if (!nrow(info)) next
  # UN COMMENTAIRE DANS L'ETENDUE N'INTERDIT PLUS LE REPLIAGE : il est REMONTE.
  # La premiere version renoncait a ces appels, et c'est ce qui laissait 243
  # fragments de phrase en francais -- un cinquieme du deficit restant. Replier
  # sans precaution ferait disparaitre le commentaire, ou pire, commenterait la
  # suite de la ligne. On le ressort donc en ligne propre, juste apres l'appel
  # reecrit : un commentaire est valide partout, y compris au milieu d'une liste
  # d'arguments, et la re-analyse du fichier reste le garde-fou.
  com <- pd[pd$token == "COMMENT", ]

  # De la derniere position a la premiere : une reecriture ne doit pas decaler
  # celles qui restent a faire sur la meme ligne.
  info <- info[order(info$line1, info$col1, decreasing = TRUE), ]
  n_fic <- 0L

  for (i in seq_len(nrow(info))) {
    ligne <- info$line1[i]; fin <- info$line2[i]
    if (any(interdit[ligne:fin])) next
    texte <- if (ligne == fin) coupe(src[ligne], info$col1[i], info$col2[i])
             else paste(c(coupe(src[ligne], info$col1[i], n_oct(src[ligne])),
                          if (fin > ligne + 1) src[(ligne + 1):(fin - 1)],
                          coupe(src[fin], 1, info$col2[i])), collapse = " ")
    n <- tryCatch(str2lang(texte), error = function(e) NULL)
    if (is.null(n) || !is.call(n)) next
    nomf <- as.character(n[[1]])
    if (!nomf %in% c("paste0", "paste")) next
    # `paste` insere une ESPACE entre ses arguments : le gabarit doit la
    # porter, sinon la phrase traduite perdrait tous ses espaces.
    sep <- if (identical(nomf, "paste0")) "" else " "

    a <- as.list(n)[-1]
    if (length(a) < 2) { .refus[["<2 args"]] <- .refus[["<2 args"]] + 1L; next }
    if (!is.null(names(a)) && any(nzchar(names(a)))) { .refus[["arg nomme"]] <- .refus[["arg nomme"]] + 1L; next }

    lits <- vapply(a, function(x) is.character(x) && length(x) == 1L, logical(1))
    if (!any(lits) || all(lits)) { .refus[["tout litt. ou aucun"]] <- .refus[["tout litt. ou aucun"]] + 1L; next }
    txt_lits <- unlist(a[lits])
    if (!any(vapply(txt_lits, francais, logical(1)))) { .refus[["pas francais"]] <- .refus[["pas francais"]] + 1L; next }
    if (any(vapply(txt_lits, codeish, logical(1)))) { .refus[["codeish"]] <- .refus[["codeish"]] + 1L; next }
    if (any(grepl("\n", txt_lits, fixed = TRUE))) { .refus[["saut de ligne"]] <- .refus[["saut de ligne"]] + 1L; next }
    # UN SEUL JETON SANS ESPACE n'est pas une phrase : c'est un nom de fichier
    # ou de feuille (`paste0("correlations_", Sys.Date())`). Le traduire
    # ajouterait au dictionnaire une entree qui ne rencontrera jamais le DOM.
    if (!any(grepl("[ ]", txt_lits))) { .refus[["un seul jeton"]] <- .refus[["un seul jeton"]] + 1L; next }

    morceaux <- vapply(seq_along(a), function(k)
      if (lits[k]) gsub("%", "%%", a[[k]], fixed = TRUE) else "%s", character(1))
    gab <- paste0(morceaux, collapse = sep)
    if (!nzchar(trimws(gsub("%s", "", gab, fixed = TRUE)))) next

    exprs <- vapply(a[!lits], function(x) paste(deparse(x), collapse = " "),
                    character(1))
    n_marq <- lengths(regmatches(gab, gregexpr("%s", gab, fixed = TRUE)))
    if (n_marq != length(exprs)) next

    remplacement <- paste0("trf(", encodeString(gab, quote = "\""), ", ",
                           paste(exprs, collapse = ", "), ")")
    nouvelle <- paste0(coupe(src[ligne], 1L, info$col1[i] - 1L), remplacement,
                       coupe(src[fin], info$col2[i] + 1L, n_oct(src[fin])))
    # Les commentaires de l'etendue, remis a l'indentation de l'appel.
    dedans <- if (!nrow(com)) integer(0) else
      which(com$line1 >= ligne & com$line1 <= fin)
    retenus <- character(0)
    if (length(dedans)) {
      marge <- strrep(" ", nchar(coupe(src[ligne], 1L, info$col1[i] - 1L)))
      retenus <- paste0(marge, trimws(com$text[dedans]))
    }
    if (ligne == fin) src[ligne] <- nouvelle
    else { src[ligne] <- nouvelle; src[(ligne + 1):fin] <- NA_character_ }
    if (length(retenus)) src[ligne] <- paste(c(src[ligne], retenus), collapse = "\n")
    n_fic <- n_fic + 1L; gabarits <- c(gabarits, gab)
  }

  if (n_fic > 0L) {
    src <- src[!is.na(src)]
    # UNE LIGNE VIDE NE SURVIT PAS A strsplit : `strsplit("", "\n")` rend
    # `character(0)`, qu'`unlist` fait disparaitre. La premiere version
    # supprimait ainsi TOUTES les lignes vides des douze fichiers -- un diff de
    # plusieurs milliers de lignes pour 131 conversions reelles.
    src <- unlist(lapply(src, function(x)
      if (grepl("\n", x, fixed = TRUE)) strsplit(x, "\n", fixed = TRUE)[[1]] else x))
    tmp <- tempfile(fileext = ".R")
    con <- file(tmp, open = "wb"); writeLines(src, con, sep = "\n"); close(con)
    ok <- !inherits(tryCatch(parse(tmp), error = function(e) e), "error")
    unlink(tmp)
    if (!ok) { cat("SYNTAXE KO, fichier laisse intact :", basename(f), "\n"); next }
    par_fichier[basename(f)] <- n_fic
    total <- total + n_fic
    if (appliquer) {
      con <- file(f, open = "wb")
      writeLines(src, con, sep = if (crlf) "\r\n" else "\n")
      close(con)
    }
  }
}

cat(if (appliquer) "APPLIQUE" else "SIMULATION", "-- appels convertis :", total, "\n")
print(par_fichier)
cat("\n-- refus par motif --\n"); for (k in ls(.refus)) cat(sprintf("  %-22s %d\n", k, .refus[[k]]))
saveRDS(unique(gabarits),
        "/tmp/claude-0/-home-user-hstat/ab6dff13-3017-5659-a369-fb52c5cbe176/scratchpad/gab_paste.rds")
