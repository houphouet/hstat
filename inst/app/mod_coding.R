# ===========================================================================
# HStat - mod_coding.R
# Atelier de codage qualitatif (CAQDAS) inspire de MAXQDA
# ---------------------------------------------------------------------------
# Trois familles de fonctionnalites, dans l'ordre du flux de travail MAXQDA :
#
#   1. LE CODAGE (thematisation)
#      Lecture des reponses a l'ecran, selection d'un mot / d'une phrase / d'un
#      paragraphe a la souris, glisser-deposer de cette selection sur un code,
#      et application d'une etiquette coloree sur le segment de texte.
#
#   2. L'ANALYSE ET LE CROISEMENT
#      Clic sur un code -> tous les extraits associes ; croisement du texte
#      code avec les profils des repondants (ex. les critiques sur le prix
#      emises par les « Moins de 25 ans »).
#
#   3. LA VISUALISATION ET LES RAPPORTS
#      Nuage de mots, carte conceptuelle (equivalent MAXMaps) construite sur
#      les cooccurrences de codes, matrices de croisement exportables en Excel.
#
#   + Un assistant d'intelligence artificielle OPTIONNEL (API Claude) qui
#     propose un livre de codes a partir du corpus et pre-code les reponses.
#
# ---------------------------------------------------------------------------
# CONVENTION DES BORNES DE SEGMENT
# Les positions `start` / `end` sont exprimees en CARACTERES, avec une borne
# de depart a 0 et une borne de fin EXCLUE : c'est exactement la convention de
# `Range.toString().length` en JavaScript, ce qui evite toute conversion entre
# le navigateur (qui capture la selection) et R (qui la stocke et la restitue).
# Consequence : `substr()` cote R doit etre appele avec `start + 1`.
# ===========================================================================


# ---------------------------------------------------------------------------
# Palette du livre de codes
# Couleurs volontairement tres contrastees : une etiquette doit se distinguer
# de sa voisine meme sur un ecran de mauvaise qualite ou en impression.
# ---------------------------------------------------------------------------
HSTAT_CODE_PALETTE <- c(
  "#e74c3c", "#2980b9", "#27ae60", "#e67e22", "#8e44ad",
  "#16a085", "#c0392b", "#2c3e50", "#d4ac0d", "#7f8c8d",
  "#1abc9c", "#9b59b6", "#34495e", "#f1948a", "#5499c7"
)

# Echappement HTML du texte des repondants avant injection dans le DOM.
# L'implementation vit dans Utils.R : la meme porte sert au rapport et aux
# reponses des modeles de langue.
.hstat_code_esc <- function(x) hstat_html_escape(x)

# "#e74c3c" + alpha -> "rgba(231,76,60,0.35)". Un fond translucide laisse le
# texte lisible la ou une couleur pleine l'ecraserait.
.hstat_code_rgba <- function(hex, alpha = 0.35) {
  hex <- as.character(hex)[1]
  if (is.na(hex) || !grepl("^#?[0-9A-Fa-f]{6}$", hex)) hex <- "#cccccc"
  hex <- sub("^#", "", hex)
  v <- strtoi(substring(hex, c(1, 3, 5), c(2, 4, 6)), 16L)
  sprintf("rgba(%d,%d,%d,%s)", v[1], v[2], v[3],
          format(round(alpha, 3), nsmall = 2, trim = TRUE))
}

# Noir ou blanc selon la luminance du fond (lisibilite du texte d'une puce).
.hstat_code_ink <- function(hex) {
  hex <- as.character(hex)[1]
  if (is.na(hex) || !grepl("^#?[0-9A-Fa-f]{6}$", hex)) return("#000000")
  v <- strtoi(substring(sub("^#", "", hex), c(1, 3, 5), c(2, 4, 6)), 16L)
  lum <- (0.299 * v[1] + 0.587 * v[2] + 0.114 * v[3]) / 255
  if (lum > 0.62) "#1b2631" else "#ffffff"
}

# Identifiant technique stable derive du libelle, unicise contre l'existant.
hstat_code_slug <- function(label, existing = character(0)) {
  s <- tolower(trimws(as.character(label)[1]))
  s <- chartr("àâäéèêëîïôöùûüç",
              "aaaeeeeiioouuuc", s)
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) s <- "code"
  base <- s
  k <- 1L
  while (s %in% existing) { k <- k + 1L; s <- paste0(base, "_", k) }
  s
}


# ---------------------------------------------------------------------------
# 1. LIVRE DE CODES
# ---------------------------------------------------------------------------

# `keywords` : dictionnaire du code (mots-cles separes par des points-virgules).
# C'est ce qui permet le codage automatique hors ligne, sans modele de langue.
# `parent_id` : identifiant du code parent, "" a la racine. C'est le systeme de
# codes HIERARCHIQUE de MAXQDA. Un livre de codes plat oblige a encoder la
# hierarchie dans les libelles (« Prix - trop cher », « Prix - rapport
# qualite »), ce qui interdit toute agregation : on ne peut plus demander
# « combien de segments parlent du prix, tous sous-codes confondus ».
hstat_code_new_codebook <- function() {
  data.frame(code_id = character(0), label = character(0), color = character(0),
             memo = character(0), keywords = character(0),
             parent_id = character(0), created = character(0),
             stringsAsFactors = FALSE)
}

HSTAT_CODE_COLS <- c("code_id", "label", "color", "memo", "keywords",
                     "parent_id", "created")

# Un projet enregistre avant l'arrivee du dictionnaire n'a pas la colonne
# `keywords` ; un projet anterieur a la hierarchie n'a pas `parent_id`. On les
# recree plutot que de refuser le fichier : un utilisateur ne doit jamais
# perdre un codage parce que le format a evolue.
hstat_code_migrate_codebook <- function(codebook) {
  if (is.null(codebook)) return(hstat_code_new_codebook())
  codebook <- as.data.frame(codebook, stringsAsFactors = FALSE)
  for (col in HSTAT_CODE_COLS)
    if (!(col %in% names(codebook)))
      codebook[[col]] <- rep("", max(0L, nrow(codebook)))
  codebook <- codebook[, HSTAT_CODE_COLS, drop = FALSE]
  # Un parent efface ou introuvable rendrait le code invisible dans l'arbre :
  # on le remonte a la racine plutot que de le perdre.
  codebook$parent_id[is.na(codebook$parent_id)] <- ""
  orphelin <- nzchar(codebook$parent_id) &
              !(codebook$parent_id %in% codebook$code_id)
  codebook$parent_id[orphelin] <- ""
  # Un cycle (A enfant de B, B enfant de A) ferait tourner l'affichage de
  # l'arbre a l'infini. Il ne peut pas naitre de l'application, mais un fichier
  # edite a la main, si.
  codebook$parent_id[hstat_code_cycles(codebook)] <- ""
  codebook
}

# Identifiants dont la remontee des parents boucle ou depasse la profondeur du
# livre de codes : ce sont ceux impliques dans un cycle.
hstat_code_cycles <- function(codebook) {
  if (is.null(codebook) || !nrow(codebook)) return(logical(0))
  vapply(seq_len(nrow(codebook)), function(i) {
    vus <- codebook$code_id[i]
    p <- codebook$parent_id[i]
    while (nzchar(p)) {
      if (p %in% vus) return(TRUE)
      vus <- c(vus, p)
      j <- match(p, codebook$code_id)
      if (is.na(j)) return(FALSE)
      p <- codebook$parent_id[j]
    }
    FALSE
  }, logical(1))
}

# Ancetres d'un code, du parent immediat jusqu'a la racine.
hstat_code_ancestors <- function(codebook, code_id) {
  out <- character(0)
  if (is.null(codebook) || !nrow(codebook)) return(out)
  p <- codebook$parent_id[match(code_id[1], codebook$code_id)]
  while (length(p) && !is.na(p) && nzchar(p) && !(p %in% out)) {
    out <- c(out, p)
    p <- codebook$parent_id[match(p, codebook$code_id)]
  }
  out
}

# Descendants d'un code, a toute profondeur. `inclus = TRUE` ajoute le code
# lui-meme : c'est la forme utile pour agreger les effectifs d'une branche.
hstat_code_descendants <- function(codebook, code_id, inclus = FALSE) {
  if (is.null(codebook) || !nrow(codebook)) return(character(0))
  out <- character(0)
  file <- as.character(code_id)
  while (length(file)) {
    enfants <- codebook$code_id[codebook$parent_id %in% file]
    enfants <- setdiff(enfants, c(out, code_id))
    out <- c(out, enfants)
    file <- enfants
  }
  if (inclus) unique(c(as.character(code_id), out)) else out
}

# Arbre mis a plat, dans l'ordre d'affichage : chaque code suit son parent, et
# `profondeur` sert a l'indentation. Les fratries sont classees par libelle.
hstat_code_tree <- function(codebook) {
  vide <- data.frame(code_id = character(0), profondeur = integer(0),
                     chemin = character(0), stringsAsFactors = FALSE)
  if (is.null(codebook) || !nrow(codebook)) return(vide)
  cb <- hstat_code_migrate_codebook(codebook)
  ordre <- character(0); prof <- integer(0)
  descendre <- function(parent, niveau) {
    i <- which(cb$parent_id == parent)
    i <- i[order(tolower(cb$label[i]))]
    for (k in i) {
      ordre <<- c(ordre, cb$code_id[k]); prof <<- c(prof, niveau)
      descendre(cb$code_id[k], niveau + 1L)
    }
  }
  descendre("", 0L)
  # Filet : un code qu'aucune racine n'atteint (cas impossible apres migration)
  # est ajoute a la fin plutot que d'etre escamote.
  oublies <- setdiff(cb$code_id, ordre)
  ordre <- c(ordre, oublies); prof <- c(prof, rep(0L, length(oublies)))
  data.frame(
    code_id = ordre, profondeur = prof,
    chemin = vapply(ordre, function(id) paste(c(
      rev(hstat_code_label(cb, hstat_code_ancestors(cb, id))),
      hstat_code_label(cb, id)), collapse = " > "), character(1)),
    stringsAsFactors = FALSE, row.names = NULL)
}

# Premiere couleur de la palette non encore utilisee ; au-dela on repart au
# debut (le livre de codes n'est pas plafonne a 15 codes).
hstat_code_next_color <- function(codebook) {
  used <- if (is.null(codebook) || nrow(codebook) == 0) character(0) else codebook$color
  free <- setdiff(HSTAT_CODE_PALETTE, used)
  if (length(free) > 0) free[1] else HSTAT_CODE_PALETTE[(length(used) %% length(HSTAT_CODE_PALETTE)) + 1L]
}

hstat_code_add <- function(codebook, label, color = NULL, memo = "",
                           keywords = "", parent_id = "") {
  if (is.null(codebook)) codebook <- hstat_code_new_codebook()
  codebook <- hstat_code_migrate_codebook(codebook)
  label <- trimws(as.character(label)[1])
  if (is.na(label) || !nzchar(label)) return(codebook)
  # Un meme libelle ne peut pas exister deux fois SOUS LE MEME PARENT : le
  # codage deviendrait ambigu a la restitution. Sous deux parents differents,
  # « Prix > Qualite » et « Service > Qualite » sont en revanche legitimes, et
  # c'est meme tout l'interet de la hierarchie.
  parent_id <- as.character(parent_id %||% "")[1]
  if (is.na(parent_id)) parent_id <- ""
  if (nzchar(parent_id) && !(parent_id %in% codebook$code_id)) parent_id <- ""
  fratrie <- codebook$label[codebook$parent_id == parent_id]
  if (any(tolower(fratrie) == tolower(label))) return(codebook)
  if (is.null(color) || !nzchar(as.character(color)[1]))
    color <- hstat_code_next_color(codebook)
  rbind(codebook, data.frame(
    code_id = hstat_code_slug(label, codebook$code_id),
    label   = label,
    color   = as.character(color)[1],
    memo    = as.character(memo)[1],
    keywords = as.character(keywords)[1],
    parent_id = parent_id,
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE))
}

# `avec_descendants = TRUE` supprime la branche entiere. Par defaut les
# sous-codes sont REMONTES au parent du code supprime : supprimer « Prix » ne
# doit pas emporter en silence le codage patiemment place sous ses sous-codes.
hstat_code_remove <- function(codebook, code_id, avec_descendants = FALSE) {
  if (is.null(codebook) || nrow(codebook) == 0) return(hstat_code_new_codebook())
  codebook <- hstat_code_migrate_codebook(codebook)
  cibles <- if (isTRUE(avec_descendants))
    unique(unlist(lapply(code_id, function(id)
      hstat_code_descendants(codebook, id, inclus = TRUE))))
  else as.character(code_id)
  if (!isTRUE(avec_descendants)) {
    for (id in cibles) {
      grand_parent <- codebook$parent_id[match(id, codebook$code_id)]
      if (length(grand_parent) && !is.na(grand_parent))
        codebook$parent_id[codebook$parent_id == id] <- grand_parent
    }
  }
  hstat_code_migrate_codebook(codebook[!(codebook$code_id %in% cibles), ,
                                       drop = FALSE])
}

hstat_code_update <- function(codebook, code_id, label = NULL, color = NULL,
                              memo = NULL, keywords = NULL, parent_id = NULL) {
  if (is.null(codebook) || nrow(codebook) == 0) return(codebook)
  codebook <- hstat_code_migrate_codebook(codebook)
  i <- which(codebook$code_id == code_id[1])
  if (!length(i)) return(codebook)
  if (!is.null(label) && nzchar(trimws(label))) codebook$label[i] <- trimws(label)
  if (!is.null(color) && nzchar(color))         codebook$color[i] <- color
  if (!is.null(memo))                           codebook$memo[i]  <- memo
  if (!is.null(keywords))                       codebook$keywords[i] <- keywords
  if (!is.null(parent_id)) {
    p <- as.character(parent_id)[1]
    if (is.na(p)) p <- ""
    # Un code ne peut devenir enfant ni de lui-meme ni d'un de ses propres
    # descendants : la branche se detacherait de l'arbre et disparaitrait de
    # l'affichage. On refuse le deplacement plutot que de casser le livre.
    interdits <- hstat_code_descendants(codebook, codebook$code_id[i], inclus = TRUE)
    if (!nzchar(p) || (p %in% codebook$code_id && !(p %in% interdits)))
      codebook$parent_id[i] <- p
  }
  codebook
}

hstat_code_label <- function(codebook, code_id) {
  if (is.null(codebook) || nrow(codebook) == 0) return(as.character(code_id))
  m <- match(code_id, codebook$code_id)
  out <- codebook$label[m]
  out[is.na(out)] <- as.character(code_id)[is.na(out)]
  out
}

hstat_code_color <- function(codebook, code_id) {
  if (is.null(codebook) || nrow(codebook) == 0) return(rep("#95a5a6", length(code_id)))
  out <- codebook$color[match(code_id, codebook$code_id)]
  out[is.na(out)] <- "#95a5a6"
  out
}


# ---------------------------------------------------------------------------
# 1 bis. MEMOS
# ---------------------------------------------------------------------------
# Le memo est l'outil par lequel l'analyste garde la trace de son raisonnement :
# pourquoi ce code a ete cree, ce qui distingue deux sous-codes voisins, ce
# qu'un entretien a d'atypique, l'hypothese qui se dessine. C'est la piece qui
# transforme un codage en analyse — et c'est ce qu'un relecteur demande quand il
# veut comprendre comment on est arrive la.
#
# Quatre cibles, comme dans MAXQDA :
#   code      pourquoi ce code existe, ou passe sa frontiere
#   document  ce que cet entretien a de particulier
#   segment   ce que ce passage precis suggere
#   libre     l'hypothese en cours, qui ne se rattache encore a rien
#
# Le memo de code existait deja comme colonne du livre de codes ; il y reste
# (les projets enregistres en dependent) et `hstat_memo_sync_codes()` le reprend
# ici pour que tout se retrouve au meme endroit.

HSTAT_MEMO_CIBLES <- c("Code" = "code", "Document" = "document",
                       "Segment" = "segment", "Libre" = "libre")

hstat_memo_new <- function() {
  data.frame(memo_id = character(0), cible_type = character(0),
             cible_id = character(0), titre = character(0), texte = character(0),
             auteur = character(0), created = character(0),
             modified = character(0), stringsAsFactors = FALSE)
}

HSTAT_MEMO_COLS <- c("memo_id", "cible_type", "cible_id", "titre", "texte",
                     "auteur", "created", "modified")

hstat_memo_migrate <- function(memos) {
  if (is.null(memos)) return(hstat_memo_new())
  memos <- as.data.frame(memos, stringsAsFactors = FALSE)
  for (col in HSTAT_MEMO_COLS)
    if (!(col %in% names(memos))) memos[[col]] <- rep("", max(0L, nrow(memos)))
  memos <- memos[, HSTAT_MEMO_COLS, drop = FALSE]
  for (col in HSTAT_MEMO_COLS) {
    memos[[col]] <- as.character(memos[[col]])
    memos[[col]][is.na(memos[[col]])] <- ""
  }
  # Un type inconnu rendrait le memo introuvable dans toutes les vues : on le
  # bascule en memo libre plutot que de le laisser orphelin.
  inconnu <- !(memos$cible_type %in% HSTAT_MEMO_CIBLES)
  memos$cible_type[inconnu] <- "libre"
  memos$cible_id[memos$cible_type == "libre"] <- ""
  memos
}

hstat_memo_add <- function(memos, cible_type = "libre", cible_id = "",
                           titre = "", texte = "", auteur = "") {
  memos <- hstat_memo_migrate(memos)
  texte <- paste(as.character(texte), collapse = "\n")
  titre <- trimws(as.character(titre)[1] %||% "")
  # Un memo sans titre ni texte n'a rien a dire : on ne cree pas de coquille
  # vide qui encombrerait la liste.
  if (!nzchar(trimws(texte)) && !nzchar(titre)) return(memos)
  type <- as.character(cible_type)[1]
  if (!(type %in% HSTAT_MEMO_CIBLES)) type <- "libre"
  maintenant <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (!nzchar(titre))
    titre <- paste0(substr(gsub("[\r\n]+", " ", texte), 1, 60),
                    if (nchar(texte) > 60) "..." else "")
  rbind(memos, data.frame(
    memo_id = hstat_code_slug(paste0("memo-", type), memos$memo_id),
    cible_type = type,
    cible_id = if (identical(type, "libre")) "" else as.character(cible_id)[1],
    titre = titre, texte = texte,
    auteur = as.character(auteur)[1] %||% "",
    created = maintenant, modified = maintenant,
    stringsAsFactors = FALSE))
}

hstat_memo_update <- function(memos, memo_id, titre = NULL, texte = NULL) {
  memos <- hstat_memo_migrate(memos)
  i <- which(memos$memo_id == as.character(memo_id)[1])
  if (!length(i)) return(memos)
  if (!is.null(titre)) memos$titre[i] <- trimws(as.character(titre)[1])
  if (!is.null(texte)) memos$texte[i] <- paste(as.character(texte), collapse = "\n")
  memos$modified[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  memos
}

hstat_memo_remove <- function(memos, memo_id) {
  memos <- hstat_memo_migrate(memos)
  memos[!(memos$memo_id %in% as.character(memo_id)), , drop = FALSE]
}

# Memos attaches a une cible. Sans `cible_id`, tous ceux du type demande.
hstat_memo_for <- function(memos, cible_type = NULL, cible_id = NULL) {
  memos <- hstat_memo_migrate(memos)
  if (!is.null(cible_type))
    memos <- memos[memos$cible_type %in% cible_type, , drop = FALSE]
  if (!is.null(cible_id))
    memos <- memos[memos$cible_id %in% as.character(cible_id), , drop = FALSE]
  memos
}

# Recherche plein texte, titre et corps confondus. Insensible a la casse et aux
# accents : un memo saisi « hypothèse » se retrouve en tapant « hypothese ».
hstat_memo_search <- function(memos, motif) {
  memos <- hstat_memo_migrate(memos)
  motif <- trimws(as.character(motif)[1] %||% "")
  if (!nzchar(motif) || !nrow(memos)) return(memos)
  plat <- function(x) tolower(iconv(x, to = "ASCII//TRANSLIT") %||% tolower(x))
  aiguille <- plat(motif)
  garde <- grepl(aiguille, plat(memos$titre), fixed = TRUE) |
           grepl(aiguille, plat(memos$texte), fixed = TRUE)
  garde[is.na(garde)] <- FALSE
  memos[garde, , drop = FALSE]
}

# Reprend dans le registre les memos portes par la colonne `memo` du livre de
# codes. Sans cela, un projet ancien verrait ses memos de code disparaitre de la
# nouvelle vue alors qu'ils sont toujours la.
hstat_memo_sync_codes <- function(memos, codebook) {
  memos <- hstat_memo_migrate(memos)
  if (is.null(codebook) || !nrow(codebook)) return(memos)
  cb <- hstat_code_migrate_codebook(codebook)
  for (i in seq_len(nrow(cb))) {
    txt <- trimws(cb$memo[i])
    if (!nzchar(txt)) next
    deja <- hstat_memo_for(memos, "code", cb$code_id[i])
    if (any(trimws(deja$texte) == txt)) next
    memos <- hstat_memo_add(memos, "code", cb$code_id[i],
                            titre = cb$label[i], texte = txt)
  }
  memos
}

# Vue d'ensemble : combien de memos, et sur quoi. Un projet ou tous les memos
# portent sur des codes et aucun sur les documents signale une analyse qui n'a
# pas encore regarde ses entretiens un par un.
hstat_memo_resume <- function(memos) {
  memos <- hstat_memo_migrate(memos)
  data.frame(
    Cible = names(HSTAT_MEMO_CIBLES),
    Memos = vapply(unname(HSTAT_MEMO_CIBLES),
                   function(t) sum(memos$cible_type == t), integer(1)),
    stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# 2. SEGMENTS CODES
# ---------------------------------------------------------------------------

hstat_code_new_segments <- function() {
  data.frame(seg_id = character(0), doc_id = character(0), code_id = character(0),
             start = numeric(0), end = numeric(0), text = character(0),
             source = character(0), created = character(0),
             stringsAsFactors = FALSE)
}

# Ajout d'un segment. Renvoie le tableau inchange si la selection est vide ou
# si le MEME CODEUR a deja pose ce code sur ces bornes : un double-depot
# accidentel ne doit pas gonfler les effectifs.
#
# LE CODEUR FAIT PARTIE DE L'IDENTITE DU SEGMENT. Sans lui dans le test de
# doublon, deux codeurs qui etiquettent le meme passage a l'identique -- soit
# l'accord parfait, le cas le plus courant -- voyaient le second codage
# silencieusement ecarte, et l'accord inter-codeurs mesurait un corpus ampute.
hstat_seg_add <- function(segments, doc_id, code_id, start, end, text = "",
                          source = "manuel") {
  if (is.null(segments)) segments <- hstat_code_new_segments()
  start <- suppressWarnings(as.numeric(start)[1])
  end   <- suppressWarnings(as.numeric(end)[1])
  if (is.na(start) || is.na(end) || end <= start) return(segments)
  doc_id  <- as.character(doc_id)[1]
  code_id <- as.character(code_id)[1]
  source  <- as.character(source)[1]
  dup <- segments$doc_id == doc_id & segments$code_id == code_id &
         segments$start == start & segments$end == end &
         segments$source == source
  if (any(dup)) return(segments)
  n <- nrow(segments)
  rbind(segments, data.frame(
    seg_id  = sprintf("s%06d", n + 1L),
    doc_id  = doc_id,
    code_id = code_id,
    start   = start,
    end     = end,
    text    = as.character(text)[1],
    source  = source,
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE))
}

hstat_seg_remove <- function(segments, seg_id) {
  if (is.null(segments) || nrow(segments) == 0) return(hstat_code_new_segments())
  segments[!(segments$seg_id %in% seg_id), , drop = FALSE]
}

# Suppression d'un code : ses segments disparaissent avec lui, sinon ils
# resteraient orphelins et sans couleur.
hstat_seg_drop_code <- function(segments, code_id) {
  if (is.null(segments) || nrow(segments) == 0) return(hstat_code_new_segments())
  segments[!(segments$code_id %in% code_id), , drop = FALSE]
}

hstat_seg_for_doc <- function(segments, doc_id) {
  if (is.null(segments) || nrow(segments) == 0) return(hstat_code_new_segments())
  segments[segments$doc_id == as.character(doc_id)[1], , drop = FALSE]
}

# Effectifs par code : nombre de segments et nombre de documents distincts.
hstat_code_counts <- function(codebook, segments) {
  if (is.null(codebook) || nrow(codebook) == 0)
    return(data.frame(code_id = character(0), label = character(0),
                      color = character(0), n_seg = integer(0), n_doc = integer(0),
                      stringsAsFactors = FALSE))
  n_seg <- integer(nrow(codebook)); n_doc <- integer(nrow(codebook))
  for (i in seq_len(nrow(codebook))) {
    s <- if (is.null(segments) || nrow(segments) == 0) NULL
         else segments[segments$code_id == codebook$code_id[i], , drop = FALSE]
    n_seg[i] <- if (is.null(s)) 0L else nrow(s)
    n_doc[i] <- if (is.null(s) || nrow(s) == 0) 0L else length(unique(s$doc_id))
  }
  # Effectifs CUMULES sur la branche : « combien de segments parlent du prix,
  # tous sous-codes confondus ? » est la question que la hierarchie permet de
  # poser, et un total qui ignorerait les sous-codes afficherait zero sur un
  # code parent pourtant abondamment documente.
  cb <- hstat_code_migrate_codebook(codebook)
  cum_seg <- integer(nrow(cb)); cum_doc <- integer(nrow(cb))
  for (i in seq_len(nrow(cb))) {
    br <- hstat_code_descendants(cb, cb$code_id[i], inclus = TRUE)
    s <- if (is.null(segments) || nrow(segments) == 0) NULL
         else segments[segments$code_id %in% br, , drop = FALSE]
    cum_seg[i] <- if (is.null(s)) 0L else nrow(s)
    cum_doc[i] <- if (is.null(s) || nrow(s) == 0) 0L else length(unique(s$doc_id))
  }
  data.frame(code_id = codebook$code_id, label = codebook$label,
             color = codebook$color, n_seg = n_seg, n_doc = n_doc,
             n_seg_cumul = cum_seg, n_doc_cumul = cum_doc,
             stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# 3. RENDU DES ETIQUETTES COLOREES
# ---------------------------------------------------------------------------
# Le texte est decoupe aux bornes de TOUS les segments, puis chaque tranche est
# coloree selon les codes qui la recouvrent. Ce decoupage par balayage gere
# nativement les chevauchements : une tranche couverte par deux codes recoit un
# degrade des deux couleurs, ce qu'un simple `gsub` de balises ne saurait faire.
# ---------------------------------------------------------------------------
hstat_code_highlight_html <- function(text, segs, codebook) {
  text <- as.character(text)[1]
  if (is.na(text)) text <- ""
  n <- nchar(text)
  if (n == 0) return("<em style='color:#95a5a6;'>(reponse vide)</em>")
  if (is.null(segs) || nrow(segs) == 0) return(.hstat_code_esc(text))

  segs <- segs[!is.na(segs$start) & !is.na(segs$end), , drop = FALSE]
  segs$start <- pmax(0, pmin(n, segs$start))
  segs$end   <- pmax(0, pmin(n, segs$end))
  segs <- segs[segs$end > segs$start, , drop = FALSE]
  if (nrow(segs) == 0) return(.hstat_code_esc(text))

  cuts <- sort(unique(c(0, n, segs$start, segs$end)))
  out <- character(length(cuts) - 1L)
  for (i in seq_len(length(cuts) - 1L)) {
    a <- cuts[i]; b <- cuts[i + 1L]
    piece <- substr(text, a + 1, b)
    act <- which(segs$start <= a & segs$end >= b)
    if (!length(act)) { out[i] <- .hstat_code_esc(piece); next }
    cols <- hstat_code_color(codebook, segs$code_id[act])
    labs <- hstat_code_label(codebook, segs$code_id[act])
    bg <- if (length(cols) == 1L) {
      sprintf("background-color:%s;", .hstat_code_rgba(cols, 0.38))
    } else {
      # Chevauchement : bandes verticales, une par code.
      stops <- vapply(seq_along(cols), function(k)
        sprintf("%s %.1f%%, %s %.1f%%",
                .hstat_code_rgba(cols[k], 0.42), 100 * (k - 1) / length(cols),
                .hstat_code_rgba(cols[k], 0.42), 100 * k / length(cols)),
        character(1))
      sprintf("background-image:linear-gradient(90deg,%s);", paste(stops, collapse = ", "))
    }
    out[i] <- sprintf(
      "<mark class=\"hstat-seg\" data-seg=\"%s\" title=\"%s\" style=\"%sborder-bottom:2px solid %s;padding:1px 0;border-radius:2px;\">%s</mark>",
      .hstat_code_esc(paste(segs$seg_id[act], collapse = " ")),
      .hstat_code_esc(paste(labs, collapse = " + ")),
      bg, cols[1], .hstat_code_esc(piece))
  }
  paste(out, collapse = "")
}


# ---------------------------------------------------------------------------
# 4. CORPUS
# ---------------------------------------------------------------------------
# Une ligne du jeu de donnees = un document (une reponse a la question ouverte).
# `row` conserve le numero de ligne d'origine : c'est lui qui permet ensuite de
# rattacher un extrait au profil du repondant.
# ---------------------------------------------------------------------------
hstat_code_docs <- function(df, text_var, min_char = 1) {
  if (is.null(df) || !nrow(df) || !(text_var %in% names(df)))
    return(data.frame(doc_id = character(0), row = integer(0),
                      text = character(0), stringsAsFactors = FALSE))
  txt <- as.character(df[[text_var]])
  txt[is.na(txt)] <- ""
  keep <- nchar(trimws(txt)) >= max(1, min_char)
  data.frame(doc_id = sprintf("D%05d", which(keep)),
             row    = which(keep),
             text   = txt[keep],
             stringsAsFactors = FALSE)
}

# Profils : les colonnes du jeu de donnees rattachees a chaque document.
hstat_code_profile <- function(df, docs, vars) {
  vars <- intersect(vars, names(df))
  if (is.null(docs) || nrow(docs) == 0 || !length(vars))
    return(data.frame(doc_id = if (is.null(docs)) character(0) else docs$doc_id,
                      stringsAsFactors = FALSE))
  out <- data.frame(doc_id = docs$doc_id, stringsAsFactors = FALSE)
  for (v in vars) out[[v]] <- as.character(df[[v]])[docs$row]
  out
}


# ---------------------------------------------------------------------------
# 5. RECUPERATION (retrieval)
# ---------------------------------------------------------------------------
# Le coeur de « je clique sur un code et je vois toutes les reponses
# associees », avec le croisement profil en option :
#   codes = "prix" + filtre "Age = Moins de 25 ans"
#   -> uniquement les critiques sur le prix emises par les moins de 25 ans.
# ---------------------------------------------------------------------------
hstat_code_retrieve <- function(segments, codebook, docs, code_ids = NULL,
                                profile = NULL, filter_var = NULL,
                                filter_levels = NULL, search = NULL) {
  empty <- data.frame(Document = character(0), Code = character(0),
                      Extrait = character(0), stringsAsFactors = FALSE)
  if (is.null(segments) || nrow(segments) == 0) return(empty)

  s <- segments
  if (!is.null(code_ids) && length(code_ids))
    s <- s[s$code_id %in% code_ids, , drop = FALSE]
  if (nrow(s) == 0) return(empty)

  if (!is.null(profile) && !is.null(filter_var) && nzchar(filter_var) &&
      filter_var %in% names(profile) && !is.null(filter_levels) && length(filter_levels)) {
    ok <- profile$doc_id[profile[[filter_var]] %in% filter_levels]
    s <- s[s$doc_id %in% ok, , drop = FALSE]
  }
  if (nrow(s) == 0) return(empty)

  if (!is.null(search) && nzchar(trimws(search))) {
    hit <- grepl(trimws(search), s$text, ignore.case = TRUE, fixed = FALSE)
    hit[is.na(hit)] <- FALSE
    s <- s[hit, , drop = FALSE]
  }
  if (nrow(s) == 0) return(empty)

  s <- s[order(s$code_id, s$doc_id, s$start), , drop = FALSE]
  lab_doc <- if (!is.null(docs) && nrow(docs)) {
    r <- docs$row[match(s$doc_id, docs$doc_id)]
    ifelse(is.na(r), s$doc_id, paste0("Ligne ", r))
  } else s$doc_id

  out <- data.frame(
    Document = lab_doc,
    Code     = hstat_code_label(codebook, s$code_id),
    Extrait  = s$text,
    Position = sprintf("%d-%d", as.integer(s$start), as.integer(s$end)),
    Origine  = s$source,
    stringsAsFactors = FALSE)
  if (!is.null(profile) && ncol(profile) > 1) {
    m <- match(s$doc_id, profile$doc_id)
    for (v in setdiff(names(profile), "doc_id")) out[[v]] <- profile[[v]][m]
  }
  out$.seg_id <- s$seg_id
  out
}


# ---------------------------------------------------------------------------
# 6. MATRICE DE CROISEMENT codes x profil
# ---------------------------------------------------------------------------
# `count = "segments"` compte les etiquettes posees (un repondant qui critique
# trois fois le prix pese 3) ; `count = "documents"` compte les repondants
# distincts (il pese 1). Les deux lectures sont legitimes, MAXQDA propose les
# deux ; le choix est donc laisse a l'utilisateur.
# ---------------------------------------------------------------------------
hstat_code_matrix <- function(segments, codebook, profile = NULL, var = NULL,
                              count = c("segments", "documents")) {
  count <- match.arg(count)
  if (is.null(codebook) || nrow(codebook) == 0) return(NULL)
  if (is.null(segments)) segments <- hstat_code_new_segments()

  if (is.null(profile) || is.null(var) || !nzchar(var) || !(var %in% names(profile))) {
    # Sans variable de profil : simple colonne d'effectifs.
    cnt <- hstat_code_counts(codebook, segments)
    m <- data.frame(Code = cnt$label,
                    Total = if (count == "segments") cnt$n_seg else cnt$n_doc,
                    stringsAsFactors = FALSE, check.names = FALSE)
    return(m)
  }

  grp <- profile[[var]]
  grp[is.na(grp) | !nzchar(trimws(grp))] <- "(manquant)"
  lev <- sort(unique(grp))
  key <- stats::setNames(grp, profile$doc_id)

  m <- matrix(0L, nrow = nrow(codebook), ncol = length(lev),
              dimnames = list(codebook$label, lev))
  for (i in seq_len(nrow(codebook))) {
    s <- segments[segments$code_id == codebook$code_id[i], , drop = FALSE]
    if (!nrow(s)) next
    if (count == "documents") s <- s[!duplicated(s$doc_id), , drop = FALSE]
    g <- key[s$doc_id]
    g <- g[!is.na(g)]
    if (!length(g)) next
    tb <- table(factor(g, levels = lev))
    m[i, ] <- as.integer(tb)
  }
  out <- data.frame(Code = rownames(m), m, check.names = FALSE,
                    stringsAsFactors = FALSE)
  out$Total <- rowSums(m)
  rownames(out) <- NULL
  out
}


# ---------------------------------------------------------------------------
# 7. COOCCURRENCES DE CODES (materiau de la carte conceptuelle)
# ---------------------------------------------------------------------------
#   mode = "document" : deux codes cooccurrent s'ils apparaissent dans la meme
#                       reponse (lecture thematique large) ;
#   mode = "overlap"  : s'ils recouvrent le meme passage de texte (lecture
#                       stricte : le meme extrait porte les deux etiquettes).
# ---------------------------------------------------------------------------
hstat_code_cooccurrence <- function(segments, codebook,
                                    mode = c("document", "overlap")) {
  mode <- match.arg(mode)
  if (is.null(codebook) || nrow(codebook) < 2) return(NULL)
  ids <- codebook$code_id
  k <- length(ids)
  m <- matrix(0L, k, k, dimnames = list(codebook$label, codebook$label))
  if (is.null(segments) || nrow(segments) == 0) return(m)

  s <- segments[segments$code_id %in% ids, , drop = FALSE]
  if (!nrow(s)) return(m)

  if (mode == "document") {
    for (d in unique(s$doc_id)) {
      cs <- unique(s$code_id[s$doc_id == d])
      if (length(cs) < 2) next
      idx <- match(cs, ids)
      for (a in seq_along(idx)) for (b in seq_along(idx)) if (a != b)
        m[idx[a], idx[b]] <- m[idx[a], idx[b]] + 1L
    }
  } else {
    for (d in unique(s$doc_id)) {
      sd <- s[s$doc_id == d, , drop = FALSE]
      if (nrow(sd) < 2) next
      for (a in seq_len(nrow(sd) - 1L)) for (b in (a + 1L):nrow(sd)) {
        if (sd$code_id[a] == sd$code_id[b]) next
        if (sd$start[a] < sd$end[b] && sd$start[b] < sd$end[a]) {
          ia <- match(sd$code_id[a], ids); ib <- match(sd$code_id[b], ids)
          m[ia, ib] <- m[ia, ib] + 1L
          m[ib, ia] <- m[ib, ia] + 1L
        }
      }
    }
  }
  m
}


# ---------------------------------------------------------------------------
# 8. CARTE CONCEPTUELLE (equivalent MAXMaps)
# ---------------------------------------------------------------------------
# Positionnement par MDS classique sur la dissimilarite deduite des
# cooccurrences (indice cosinus) : deux codes souvent evoques ensemble se
# retrouvent proches. `cmdscale()` vient de {stats}, aucune dependance de
# graphe n'est requise. Repli sur un cercle quand le MDS n'est pas calculable
# (moins de 3 codes, ou aucune cooccurrence).
# ---------------------------------------------------------------------------
hstat_code_map_layout <- function(cooc, counts, seed = 42) {
  if (is.null(cooc) || nrow(cooc) < 2) return(NULL)
  k <- nrow(cooc)
  # `counts` est un vecteur nomme (effectif par LIBELLE de code), pas un
  # tableau : c'est ce que fournit setNames(cnt$n_seg, cnt$label).
  n <- if (is.null(counts)) rep(NA_real_, k) else as.numeric(counts[rownames(cooc)])
  if (all(is.na(n))) n <- rep(1, k)
  n[is.na(n) | n <= 0] <- 1

  sim <- cooc / outer(sqrt(n), sqrt(n))
  sim[!is.finite(sim)] <- 0
  diag(sim) <- 1
  d <- as.dist(1 - sim / max(1, max(sim)))

  xy <- NULL
  if (k >= 3 && sum(cooc) > 0) {
    xy <- tryCatch(stats::cmdscale(d, k = 2), error = function(e) NULL)
    if (!is.null(xy) && (ncol(xy) < 2 || any(!is.finite(xy)))) xy <- NULL
  }
  if (is.null(xy)) {
    th <- seq(0, 2 * pi, length.out = k + 1)[seq_len(k)]
    xy <- cbind(cos(th), sin(th))
  }
  data.frame(label = rownames(cooc), x = xy[, 1], y = xy[, 2],
             n = as.numeric(n), stringsAsFactors = FALSE)
}

# Au plus trois graduations entieres : les legendes de taille et d'epaisseur
# affichaient sinon une graduation par valeur entiere presente (8, 9, 10...).
# `breaks` est retenu plutot que `n.breaks`, absent de scale_linewidth_*() dans
# les versions de ggplot2 encore en circulation.
.hstat_code_breaks3 <- function(x) {
  b <- unique(round(pretty(x, n = 3)))
  b <- b[b >= min(x) & b <= max(x)]
  if (length(b) < 2) unique(round(range(x))) else b
}

hstat_code_map_plot <- function(cooc, codebook, counts_df, min_weight = 1,
                                label_size = 4, mode_label = "document") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(cooc) || nrow(cooc) < 2) return(NULL)
  nodes <- hstat_code_map_layout(cooc, stats::setNames(counts_df$n_seg, counts_df$label))
  if (is.null(nodes)) return(NULL)
  nodes$color <- hstat_code_color(codebook,
                                  codebook$code_id[match(nodes$label, codebook$label)])

  # Aretes : moitie superieure de la matrice, au-dessus du seuil.
  edges <- NULL
  idx <- which(upper.tri(cooc) & cooc >= min_weight, arr.ind = TRUE)
  if (nrow(idx) > 0) {
    edges <- data.frame(
      x    = nodes$x[idx[, 1]], y    = nodes$y[idx[, 1]],
      xend = nodes$x[idx[, 2]], yend = nodes$y[idx[, 2]],
      w    = as.numeric(cooc[idx]), stringsAsFactors = FALSE)
  }

  p <- ggplot2::ggplot()
  if (!is.null(edges) && nrow(edges) > 0) {
    p <- p + ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
                   linewidth = .data$w, alpha = .data$w),
      colour = "#7f8c8d", lineend = "round", show.legend = TRUE) +
      ggplot2::scale_linewidth_continuous(range = c(0.3, 3), name = "Cooccurrences",
                                          breaks = .hstat_code_breaks3) +
      ggplot2::scale_alpha_continuous(range = c(0.25, 0.85), guide = "none")
  }
  p <- p +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$n),
      colour = nodes$color, alpha = 0.85) +
    ggplot2::scale_size_continuous(range = c(4, 16), name = "Segments codes",
                                   breaks = .hstat_code_breaks3)

  lbl <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(
      data = nodes, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = label_size, fontface = "bold", colour = "#2c3e50",
      box.padding = 0.5, min.segment.length = 0.2, max.overlaps = 50)
  } else {
    ggplot2::geom_text(
      data = nodes, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = label_size, fontface = "bold", colour = "#2c3e50", vjust = -1.2)
  }

  sub <- if (identical(mode_label, "overlap"))
    "Lien = les deux codes etiquettent un meme passage" else
    "Lien = les deux codes apparaissent dans une meme reponse"

  # Marge autour des noeuds : sans elle, un code place en peripherie voit son
  # etiquette tronquee par le bord du graphique.
  pad_x <- max(0.15, 0.18 * diff(range(nodes$x)))
  pad_y <- max(0.15, 0.18 * diff(range(nodes$y)))

  p + lbl +
    ggplot2::expand_limits(x = range(nodes$x) + c(-1, 1) * pad_x,
                           y = range(nodes$y) + c(-1, 1) * pad_y) +
    ggplot2::labs(title = "Carte conceptuelle des codes",
                  subtitle = sub, x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, colour = "#7f8c8d"),
      plot.margin = grid::unit(c(10, 20, 10, 20), "pt"))
}


# ---------------------------------------------------------------------------
# 9. NUAGE DE MOTS
# ---------------------------------------------------------------------------
# Implementation autonome (spirale d'Archimede + test de collision par boites
# englobantes) : aucune dependance supplementaire, et le resultat est
# reproductible a graine fixee. Les paquets dedies (wordcloud, ggwordcloud) ne
# figurent pas dans les dependances de HStat et ne sont pas garantis installes.
# ---------------------------------------------------------------------------
hstat_code_cloud_layout <- function(words, freq, max_words = 90,
                                    min_size = 3, max_size = 16, seed = 42) {
  words <- as.character(words); freq <- as.numeric(freq)
  ok <- !is.na(words) & nzchar(words) & !is.na(freq) & freq > 0
  words <- words[ok]; freq <- freq[ok]
  if (!length(words)) return(NULL)
  o <- order(freq, decreasing = TRUE)
  words <- words[o]; freq <- freq[o]
  if (length(words) > max_words) {
    words <- words[seq_len(max_words)]; freq <- freq[seq_len(max_words)]
  }

  rg <- range(freq)
  size <- if (diff(rg) == 0) rep((min_size + max_size) / 2, length(freq)) else
    min_size + (max_size - min_size) * sqrt((freq - rg[1]) / diff(rg))

  # Boite englobante approximative d'un texte : 0.62 em de large par caractere.
  bw <- nchar(words) * size * 0.62
  bh <- size * 1.35

  set.seed(seed)
  px <- numeric(0); py <- numeric(0); pw <- numeric(0); ph <- numeric(0)
  x <- rep(NA_real_, length(words)); y <- rep(NA_real_, length(words))
  r_turn <- max(bh) * 0.95         # ecartement d'un tour de spirale
  for (i in seq_along(words)) {
    th <- 0; placed <- FALSE; guard <- 0L
    while (th < 2 * pi * 45 && guard < 4000L) {
      guard <- guard + 1L
      r <- r_turn * th / (2 * pi)
      cx <- r * cos(th) * 1.9      # spirale elliptique : nuage plus large que haut
      cy <- r * sin(th)
      hit <- length(px) > 0 &&
        any(abs(cx - px) < (bw[i] + pw) / 2 + 1 & abs(cy - py) < (bh[i] + ph) / 2 + 1)
      if (!hit) {
        x[i] <- cx; y[i] <- cy; placed <- TRUE
        px <- c(px, cx); py <- c(py, cy); pw <- c(pw, bw[i]); ph <- c(ph, bh[i])
        break
      }
      th <- th + max(0.08, 0.6 / max(1, r / max(bh)))
    }
    if (!placed) next
  }
  keep <- !is.na(x)
  data.frame(word = words[keep], freq = freq[keep], x = x[keep], y = y[keep],
             size = size[keep], stringsAsFactors = FALSE)
}

hstat_code_cloud_plot <- function(layout, palette = "default",
                                  low = "#aed6f1", high = "#1f618d",
                                  target_width_mm = 220) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(layout) || !nrow(layout)) return(NULL)

  # Les unites de la mise en page sont des « millimetres de texte ». On les
  # remet a l'echelle de la zone de trace pour que la taille demandee et la
  # taille rendue restent coherentes (ggplot2 exprime `size` en mm).
  span <- max(diff(range(layout$x)) + max(nchar(layout$word) * layout$size * 0.62), 1)
  f <- min(1.6, target_width_mm / span)

  p <- ggplot2::ggplot(layout, ggplot2::aes(x = .data$x, y = .data$y,
                                            label = .data$word, colour = .data$freq)) +
    ggplot2::geom_text(size = layout$size * f, fontface = "bold",
                       show.legend = TRUE) +
    ggplot2::scale_colour_gradient(low = low, high = high, name = "Frequence") +
    ggplot2::labs(title = "Nuage de mots") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 16)) +
    ggplot2::expand_limits(
      x = range(layout$x) + c(-1, 1) * 0.08 * span,
      y = range(layout$y) + c(-1, 1) * 0.12 * max(1, diff(range(layout$y))))

  if (exists("hstat_q_apply_palette") && !identical(palette, "default"))
    p <- hstat_q_apply_palette(p, palette, low = low, high = high)
  p
}


# ---------------------------------------------------------------------------
# 10. ASSISTANCE AU CODAGE
# ---------------------------------------------------------------------------
# Le moteur d'inference lui-meme (local / hors ligne / API) vit dans mod_ai.R :
# il sert a TOUTES les analyses de HStat, pas seulement au codage qualitatif.
# Ne restent ici que ce qui est propre a la thematisation : le moteur
# statistique hors ligne et les invites adressees a un modele de langue.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 10 bis. MOTEUR HORS LIGNE : THEMATISATION STATISTIQUE + DICTIONNAIRE
# ---------------------------------------------------------------------------
# Aucun modele de langue, aucun reseau, aucune installation : tout se calcule
# dans le processus R a partir du corpus lui-meme. C'est le seul moteur dont la
# disponibilite est garantie, et il sert de socle aux deux autres puisque le
# dictionnaire qu'il produit reste editable a la main.
#
# Principe : classification ascendante hierarchique des TERMES du corpus sur
# leur cooccurrence dans les reponses (distance cosinus, methode de Ward). Deux
# mots qui reviennent dans les memes reponses finissent dans le meme theme.
# ---------------------------------------------------------------------------

# Repli d'accents et passage en minuscules. Propriete essentielle : chartr() et
# tolower() operent caractere par caractere, la LONGUEUR est donc preservee.
# C'est ce qui permet de chercher un mot-cle sans accent dans un texte accentue
# tout en gardant des positions de segment valides.
.hstat_code_fold <- function(x) {
  x <- tolower(as.character(x))
  chartr("àâäéèêëîïôöùûüç",
         "aaaeeeeiioouuuc", x)
}

# Echappement pour une expression reguliere : tout ce qui n'est ni alphanumerique
# ni espace est protege. Passer par une classe POSIX evite la classe de
# caracteres invalide que produisait un echappement ecrit a la main.
.hstat_code_rx_escape <- function(x) gsub("([^[:alnum:][:space:]])", "\\\\\\1", x)

hstat_code_keywords_of <- function(codebook, code_id) {
  if (is.null(codebook) || nrow(codebook) == 0) return(character(0))
  i <- match(code_id[1], codebook$code_id)
  if (is.na(i)) return(character(0))
  k <- codebook$keywords[i]
  if (is.na(k) || !nzchar(trimws(k))) return(character(0))
  k <- trimws(strsplit(k, "[;,\n]")[[1]])
  unique(k[nzchar(k)])
}

# Construit un livre de codes a partir du corpus.
# Renvoie data.frame(label, memo, keywords) - le meme contrat que
# hstat_ai_parse_codebook(), pour que l'interface traite les deux pareillement.
hstat_code_auto_codebook <- function(texts, n_codes = 8, min_char = 4,
                                     min_docs = 2, max_terms = 60,
                                     max_df = 0.9, extra_stopwords = NULL) {
  texts <- as.character(texts)
  texts <- texts[!is.na(texts) & nzchar(trimws(texts))]
  if (length(texts) < 3) return(NULL)

  # Tokenisation SANS racinisation : on a besoin des formes de surface pour
  # constituer un dictionnaire recherchable dans le texte d'origine.
  toks <- hstat_q_tokenize(texts, min_char = min_char, stem = FALSE,
                           remove_numbers = TRUE, extra_stopwords = extra_stopwords)
  if (!length(unlist(toks))) return(NULL)

  # Regroupement des formes flechies par racine ; la forme la plus frequente
  # sert d'etiquette, toutes les formes rencontrees entrent au dictionnaire.
  all_w <- unlist(toks)
  stems <- hstat_q_stem_fr(all_w)
  surf <- split(all_w, stems)
  best <- vapply(surf, function(v) names(sort(table(v), decreasing = TRUE))[1], character(1))
  forms <- lapply(surf, function(v) sort(unique(v)))

  # Presence par racine et par document
  stem_by_doc <- lapply(toks, function(w) if (length(w)) unique(hstat_q_stem_fr(w)) else character(0))
  n_doc <- length(stem_by_doc)
  freq <- sort(table(unlist(stem_by_doc)), decreasing = TRUE)
  # Trop rare : bruit. Present dans presque toutes les reponses : ne discrimine
  # aucun theme (l'equivalent d'un mot vide propre a ce corpus).
  freq <- freq[freq >= min_docs & freq <= max_df * n_doc]
  if (length(freq) < 2) return(NULL)
  keep <- names(freq)[seq_len(min(length(freq), max_terms))]

  # Matrice termes x documents
  M <- matrix(0L, nrow = length(keep), ncol = n_doc, dimnames = list(keep, NULL))
  for (j in seq_len(n_doc)) {
    hit <- match(stem_by_doc[[j]], keep)
    hit <- hit[!is.na(hit)]
    if (length(hit)) M[hit, j] <- 1L
  }

  k <- max(2L, min(as.integer(n_codes), nrow(M) - 1L))
  norm <- sqrt(rowSums(M^2)); norm[norm == 0] <- 1
  sim <- (M %*% t(M)) / outer(norm, norm)
  sim[!is.finite(sim)] <- 0
  d <- stats::as.dist(1 - sim)
  cl <- tryCatch(stats::cutree(stats::hclust(d, method = "ward.D2"), k = k),
                 error = function(e) NULL)
  if (is.null(cl)) return(NULL)

  # Un theme par groupe, ordonne par poids decroissant dans le corpus.
  out <- lapply(sort(unique(cl)), function(g) {
    st <- names(cl)[cl == g]
    st <- st[order(freq[st], decreasing = TRUE)]
    kw <- unique(unlist(forms[st]))
    list(label = best[[st[1]]],
         n = sum(freq[st]),
         memo = sprintf("Theme construit automatiquement a partir de : %s.",
                        paste(utils::head(vapply(st, function(z) best[[z]], character(1)), 6),
                              collapse = ", ")),
         keywords = paste(kw, collapse = "; "))
  })
  out <- out[order(vapply(out, function(e) e$n, numeric(1)), decreasing = TRUE)]

  res <- data.frame(
    label    = vapply(out, function(e) e$label, character(1)),
    memo     = vapply(out, function(e) e$memo, character(1)),
    keywords = vapply(out, function(e) e$keywords, character(1)),
    stringsAsFactors = FALSE)
  # Deux groupes peuvent partager leur terme dominant : on desambigue plutot
  # que de laisser hstat_code_add() rejeter silencieusement le doublon.
  dup <- duplicated(tolower(res$label))
  res$label[dup] <- paste0(res$label[dup], " (2)")
  res
}

# Bornes des phrases d'un texte, en convention JavaScript (debut a 0, fin
# exclue), espaces de tete et de queue exclus. Sert au codage par dictionnaire :
# etiqueter le mot seul donne des extraits inexploitables a la restitution, la
# phrase qui le porte est l'unite de sens qu'un codeur humain retiendrait.
.hstat_code_sentences <- function(text) {
  n <- nchar(text)
  if (n == 0) return(NULL)
  m <- gregexpr("[.!?;\n]+", text, perl = TRUE)[[1]]
  cuts <- if (length(m) == 1L && m[1] == -1L) integer(0)
          else as.integer(m) - 1L + attr(m, "match.length")
  starts <- c(0L, cuts)
  ends   <- c(cuts, n)
  keep   <- ends > starts
  starts <- starts[keep]; ends <- ends[keep]
  if (!length(starts)) return(NULL)
  # Rognage des blancs et de la ponctuation de bordure, sans jamais sortir des
  # bornes du texte. Classes POSIX obligatoires : dans une classe de caracteres,
  # `\s` n'est pas une abreviation mais les deux caracteres « \ » et « s » —
  # la version precedente rognait donc la lettre s en fin de phrase.
  for (i in seq_along(starts)) {
    while (starts[i] < ends[i] &&
           grepl("^[[:space:]]$", substr(text, starts[i] + 1, starts[i] + 1)))
      starts[i] <- starts[i] + 1L
    while (ends[i] > starts[i] &&
           grepl("^[[:space:].!?;]$", substr(text, ends[i], ends[i])))
      ends[i] <- ends[i] - 1L
  }
  keep <- ends > starts
  if (!any(keep)) return(NULL)
  cbind(start = starts[keep], end = ends[keep])
}

# Codage automatique par dictionnaire : toute occurrence d'un mot-cle etiquette
# la phrase qui la porte (ou le mot seul si `scope = "mot"`). Instantane,
# deterministe, sans reseau — et applicable a TOUT le corpus, la ou un modele de
# langue impose d'echantillonner.
hstat_code_lexical_apply <- function(docs, codebook, whole_word = TRUE,
                                     scope = c("phrase", "mot"),
                                     max_per_doc = 20L) {
  scope <- match.arg(scope)
  out <- hstat_code_new_segments()
  if (is.null(docs) || !nrow(docs) || is.null(codebook) || !nrow(codebook)) return(out)

  pats <- lapply(seq_len(nrow(codebook)), function(i) {
    kw <- hstat_code_keywords_of(codebook, codebook$code_id[i])
    kw <- .hstat_code_fold(kw)
    kw <- kw[nzchar(kw)]
    if (!length(kw)) return(NULL)
    kw <- kw[order(nchar(kw), decreasing = TRUE)]   # le plus long d'abord
    body <- paste(.hstat_code_rx_escape(kw), collapse = "|")
    if (isTRUE(whole_word)) sprintf("\\b(?:%s)\\b", body) else sprintf("(?:%s)", body)
  })

  n_no_kw <- sum(vapply(pats, is.null, logical(1)))
  for (j in seq_len(nrow(docs))) {
    txt <- docs$text[j]
    fold <- .hstat_code_fold(txt)
    # Garde-fou : si le repli changeait la longueur, les positions calculees sur
    # `fold` ne vaudraient plus rien sur `txt`. On saute plutot que de mal coder.
    if (nchar(fold) != nchar(txt)) next
    sent <- if (identical(scope, "phrase")) .hstat_code_sentences(txt) else NULL
    for (i in seq_along(pats)) {
      if (is.null(pats[[i]])) next
      m <- gregexpr(pats[[i]], fold, perl = TRUE)[[1]]
      if (length(m) == 1L && m[1] == -1L) next
      len <- attr(m, "match.length")
      n <- min(length(m), max_per_doc)
      for (h in seq_len(n)) {
        a <- m[h] - 1L
        b <- a + len[h]
        if (!is.null(sent)) {
          k <- which(sent[, "start"] <= a & sent[, "end"] >= b)
          if (length(k)) { a <- sent[k[1], "start"]; b <- sent[k[1], "end"] }
        }
        # Plusieurs mots-cles d'un meme code dans une meme phrase donnent les
        # memes bornes : hstat_seg_add() ecarte le doublon de lui-meme.
        out <- hstat_seg_add(out, docs$doc_id[j], codebook$code_id[i], a, b,
                             substr(txt, a + 1, b), source = "auto")
      }
    }
  }
  attr(out, "codes_sans_mots_cles") <- n_no_kw
  out
}

hstat_ai_codebook_prompt <- function(texts, n_codes = 8, context = "") {
  texts <- as.character(texts)
  texts <- texts[!is.na(texts) & nzchar(trimws(texts))]
  # Corpus tronque : on borne la taille de la requete sans perdre la diversite
  # (echantillon regulier plutot que les N premieres reponses).
  if (length(texts) > 150) texts <- texts[round(seq(1, length(texts), length.out = 150))]
  corpus <- paste(sprintf("- %s", substr(texts, 1, 600)), collapse = "\n")
  paste0(
    "Tu es analyste qualitatif. Voici un corpus de reponses libres a une ",
    "question d'enquete.\n",
    if (nzchar(trimws(context))) paste0("Contexte de l'enquete : ", context, "\n") else "",
    "\nCORPUS :\n", corpus,
    "\n\nConstruis un livre de codes (grille de thematisation) de ", as.integer(n_codes),
    " codes au maximum, exhaustif et mutuellement lisible, couvrant les themes ",
    "reellement presents dans ce corpus.\n",
    "Reponds UNIQUEMENT par un objet JSON, sans texte autour, de la forme :\n",
    '{"codes":[{"label":"Prix trop eleve","memo":"Definition du code en une phrase"}]}\n',
    "Les libelles doivent etre courts (1 a 4 mots), en francais, et distincts.")
}

hstat_ai_parse_codebook <- function(parsed) {
  if (is.null(parsed)) return(NULL)
  lst <- if (!is.null(parsed$codes)) parsed$codes else parsed
  if (!is.list(lst) || !length(lst)) return(NULL)
  lab <- vapply(lst, function(e) {
    v <- if (is.list(e)) e$label else e
    if (is.null(v)) "" else as.character(v)[1]
  }, character(1))
  memo <- vapply(lst, function(e) {
    v <- if (is.list(e)) e$memo else NULL
    if (is.null(v)) "" else as.character(v)[1]
  }, character(1))
  keep <- nzchar(trimws(lab))
  if (!any(keep)) return(NULL)
  # Meme contrat que hstat_code_auto_codebook() : l'interface traite les deux
  # sources de la meme facon. Un modele ne fournit pas de dictionnaire.
  data.frame(label = trimws(lab[keep]), memo = memo[keep],
             keywords = rep("", sum(keep)), stringsAsFactors = FALSE)
}

hstat_ai_autocode_prompt <- function(docs, codebook) {
  cb <- paste(sprintf("- %s : %s", codebook$label,
                      ifelse(nzchar(codebook$memo), codebook$memo, "(pas de definition)")),
              collapse = "\n")
  corp <- paste(sprintf("[%s] %s", docs$doc_id, substr(docs$text, 1, 1500)),
                collapse = "\n")
  paste0(
    "Tu es analyste qualitatif. Applique le livre de codes ci-dessous aux ",
    "reponses fournies.\n\nLIVRE DE CODES :\n", cb,
    "\n\nREPONSES :\n", corp,
    "\n\nPour chaque reponse, identifie les passages qui relevent d'un code. ",
    "Un passage doit etre un extrait VERBATIM, copie caractere pour caractere ",
    "depuis la reponse (sans reformulation, sans guillemets ajoutes, sans ",
    "points de suspension). Une reponse peut ne relever d'aucun code.\n",
    "Reponds UNIQUEMENT par un objet JSON, sans texte autour, de la forme :\n",
    '{"codages":[{"doc":"D00001","code":"Prix trop eleve","extrait":"c est beaucoup trop cher"}]}')
}

# Localisation d'un extrait dans un document. Trois passes, de la plus stricte
# a la plus souple : le modele d'IA normalise souvent les espaces ou la casse,
# ce qui ferait echouer une recherche purement litterale. Sert aussi de filet
# de securite au codage manuel, quand les bornes renvoyees par le navigateur ne
# correspondent pas au texte attendu.
hstat_code_locate_quote <- function(text, quote) {
  text <- as.character(text)[1]; quote <- trimws(as.character(quote)[1])
  if (is.na(text) || is.na(quote) || !nzchar(quote) || !nzchar(text)) return(NULL)

  i <- regexpr(quote, text, fixed = TRUE)
  if (i > 0) return(c(start = i - 1L, end = i - 1L + nchar(quote)))

  i <- regexpr(tolower(quote), tolower(text), fixed = TRUE)
  if (i > 0) return(c(start = i - 1L, end = i - 1L + nchar(quote)))

  # Passe souple : on recherche la suite des mots de l'extrait en tolerant
  # n'importe quelle sequence d'espaces ou de ponctuation entre eux.
  w <- strsplit(gsub("[^[:alnum:][:space:]]", " ", tolower(quote)), "\\s+")[[1]]
  w <- w[nzchar(w)]
  if (length(w) < 2) return(NULL)
  # Les mots ne contiennent plus que des caracteres alphanumeriques (la
  # ponctuation vient d'etre remplacee par des espaces) : aucun echappement
  # n'est necessaire, et tenter d'en faire un produisait une classe de
  # caracteres invalide.
  pat <- paste(w, collapse = "[^[:alnum:]]+")
  m <- regexpr(pat, tolower(text), perl = TRUE)
  if (m > 0) return(c(start = m - 1L, end = m - 1L + attr(m, "match.length")))
  NULL
}

hstat_ai_parse_autocode <- function(parsed, docs, codebook) {
  if (is.null(parsed)) return(NULL)
  lst <- if (!is.null(parsed$codages)) parsed$codages else parsed
  if (!is.list(lst) || !length(lst)) return(NULL)

  out <- hstat_code_new_segments()
  miss <- 0L
  for (e in lst) {
    if (!is.list(e)) next
    d <- if (is.null(e$doc)) "" else as.character(e$doc)[1]
    l <- if (is.null(e$code)) "" else as.character(e$code)[1]
    q <- if (is.null(e$extrait)) "" else as.character(e$extrait)[1]
    if (!nzchar(d) || !nzchar(l) || !nzchar(q)) next
    ci <- match(tolower(trimws(l)), tolower(codebook$label))
    di <- match(d, docs$doc_id)
    if (is.na(ci) || is.na(di)) { miss <- miss + 1L; next }
    pos <- hstat_code_locate_quote(docs$text[di], q)
    if (is.null(pos)) { miss <- miss + 1L; next }
    out <- hstat_seg_add(out, docs$doc_id[di], codebook$code_id[ci],
                         pos[["start"]], pos[["end"]],
                         substr(docs$text[di], pos[["start"]] + 1, pos[["end"]]),
                         source = "IA")
  }
  attr(out, "non_localises") <- miss
  out
}


# ---------------------------------------------------------------------------
# 12. REQUETE DE CODAGE COMBINEE
# ---------------------------------------------------------------------------
# MAXQDA appelle cela « Complex Coding Query » : croiser deux ensembles de
# codes pour repondre a « qui dit A ET B ? », « A sans B ? », « ou A et B
# etiquettent-ils LE MEME passage ? ».
#
# LA PORTEE CHANGE LE SENS, ET DOIT DONC ETRE EXPLICITE.
#   "document"  -> A et B dans la meme reponse, meme a dix lignes d'ecart.
#                  Lecture thematique : deux themes coexistent chez la personne.
#   "overlap"   -> A et B recouvrent le MEME passage. Lecture stricte : le
#                  meme extrait porte les deux etiquettes.
#   "proximite" -> A a moins de `distance` caracteres d'un segment de B. Entre
#                  les deux : les idees se suivent sans se superposer.
# Confondre les trois donne des effectifs tres differents pour la meme
# question ; le resultat porte donc la portee employee en attribut.
#
# Le resultat est un tableau de SEGMENTS DE A (ceux qui satisfont la
# condition), reutilisable tel quel par hstat_code_retrieve().
# ---------------------------------------------------------------------------
hstat_code_query <- function(segments, codebook, docs, codes_a, codes_b = NULL,
                             operateur = c("et", "ou", "sauf"),
                             portee = c("document", "overlap", "proximite"),
                             distance = 200, profile = NULL) {
  operateur <- match.arg(operateur)
  portee    <- match.arg(portee)
  vide <- hstat_code_new_segments()
  if (is.null(segments) || !nrow(segments) || is.null(codes_a) || !length(codes_a))
    return(structure(vide, portee = portee, operateur = operateur))

  a <- segments[segments$code_id %in% codes_a, , drop = FALSE]
  b <- if (is.null(codes_b) || !length(codes_b)) vide
       else segments[segments$code_id %in% codes_b, , drop = FALSE]

  # « OU » ne croise rien : c'est la reunion des deux ensembles. La portee n'y
  # a aucun sens et serait trompeuse si on la laissait paraitre.
  if (operateur == "ou") {
    out <- rbind(a, b)
    out <- out[!duplicated(out$seg_id), , drop = FALSE]
    return(structure(out[order(out$doc_id, out$start), , drop = FALSE],
                     portee = NA_character_, operateur = operateur))
  }

  if (!nrow(a))
    return(structure(vide, portee = portee, operateur = operateur))

  # Sans second ensemble, « ET » ne peut rien confirmer et « SAUF » n'a rien a
  # retrancher : on rend A tel quel pour « sauf », et rien pour « et ».
  if (!nrow(b)) {
    out <- if (operateur == "sauf") a else vide
    return(structure(out, portee = portee, operateur = operateur))
  }

  d <- suppressWarnings(as.numeric(distance)[1])
  if (!is.finite(d) || d < 0) d <- 0

  satisfait <- vapply(seq_len(nrow(a)), function(i) {
    bb <- b[b$doc_id == a$doc_id[i], , drop = FALSE]
    if (!nrow(bb)) return(FALSE)
    if (portee == "document") return(TRUE)
    if (portee == "overlap")
      return(any(bb$start < a$end[i] & bb$end > a$start[i]))
    # proximite : bornes ecartees de `d` de part et d'autre
    any(bb$start < a$end[i] + d & bb$end > a$start[i] - d)
  }, logical(1))

  garde <- if (operateur == "et") satisfait else !satisfait
  out <- a[garde, , drop = FALSE]
  structure(out[order(out$doc_id, out$start), , drop = FALSE],
            portee = portee, operateur = operateur)
}


# ---------------------------------------------------------------------------
# 13. CONCORDANCIER (KWIC -- Key Word In Context)
# ---------------------------------------------------------------------------
# Le mot recherche entoure de son contexte gauche et droit. C'est l'outil qui
# precede le codage : on voit COMMENT un mot est employe avant de decider
# quel code lui donner.
#
# DEUX PRECAUTIONS.
#   1. Le motif de l'utilisateur est ECHAPPE par defaut (`regex = FALSE`) :
#      taper « prix (cher) » ne doit pas lever « unmatched parenthesis » ni
#      chercher un groupe de capture. Une regexp invalide en mode `regex =
#      TRUE` rend zero ligne au lieu de faire tomber le panneau.
#   2. Le contexte est coupe sur les BORNES DE MOTS quand c'est possible :
#      trancher au milieu d'un mot rend la lecture penible.
# ---------------------------------------------------------------------------
hstat_code_kwic <- function(docs, motif, fenetre = 40, regex = FALSE,
                            casse = FALSE, max_hits = 500) {
  vide <- data.frame(Document = character(0), Gauche = character(0),
                     Motif = character(0), Droite = character(0),
                     Position = integer(0), stringsAsFactors = FALSE)
  if (is.null(docs) || !NROW(docs) || is.null(motif)) return(vide)
  motif <- as.character(motif)[1]
  if (is.na(motif) || !nzchar(trimws(motif))) return(vide)
  fenetre <- suppressWarnings(as.integer(fenetre)[1])
  if (!is.finite(fenetre) || fenetre < 0) fenetre <- 40L
  pat <- if (isTRUE(regex)) motif else .hstat_code_rx_escape(motif)

  coupe_gauche <- function(s) {
    if (!nzchar(s)) return(s)
    i <- regexpr("\\s", s)
    if (i > 0 && i < nchar(s) / 2) substring(s, i + 1L) else s
  }
  coupe_droite <- function(s) {
    if (!nzchar(s)) return(s)
    i <- max(c(0L, gregexpr("\\s", s)[[1]]))
    if (i > nchar(s) / 2) substring(s, 1L, i - 1L) else s
  }

  lignes <- list()
  for (k in seq_len(NROW(docs))) {
    txt <- as.character(docs$text[k])
    if (is.na(txt) || !nzchar(txt)) next
    # Une expression reguliere invalide leve tantot une ERREUR, tantot un
    # simple AVERTISSEMENT : ne rattraper que l'erreur laissait passer
    # « unmatched parenthesis » dans la console de l'utilisateur.
    m <- tryCatch(gregexpr(pat, txt, perl = TRUE, ignore.case = !isTRUE(casse))[[1]],
                  error = function(e) -1L, warning = function(w) -1L)
    if (m[1] < 0) next
    lg <- attr(m, "match.length")
    for (j in seq_along(m)) {
      deb <- m[j]; lon <- lg[j]
      g <- substring(txt, max(1L, deb - fenetre), deb - 1L)
      dr <- substring(txt, deb + lon, min(nchar(txt), deb + lon - 1L + fenetre))
      lignes[[length(lignes) + 1L]] <- data.frame(
        Document = if (!is.null(docs$row)) paste("Ligne", docs$row[k]) else docs$doc_id[k],
        Gauche   = coupe_gauche(g),
        Motif    = substring(txt, deb, deb + lon - 1L),
        Droite   = coupe_droite(dr),
        Position = as.integer(deb),
        stringsAsFactors = FALSE)
      if (length(lignes) >= max_hits) break
    }
    if (length(lignes) >= max_hits) break
  }
  if (!length(lignes)) return(vide)
  out <- do.call(rbind, lignes)
  attr(out, "tronque") <- nrow(out) >= max_hits
  out
}


# ---------------------------------------------------------------------------
# 14. CODELINE -- portrait code d'un document
# ---------------------------------------------------------------------------
# MAXQDA montre un document comme une bande ou chaque code occupe la portion
# de texte qu'il etiquette. On voit d'un coup d'oeil l'ordre du discours :
# par quoi la personne commence, ce qui revient, ce qui ne vient qu'a la fin.
#
# La position est rendue en POURCENTAGE du document, pas en caracteres : deux
# reponses de longueurs tres differentes deviennent comparables, ce qui est
# tout l'interet de la representation.
# ---------------------------------------------------------------------------
hstat_code_codeline <- function(segments, codebook, docs, doc_id) {
  vide <- data.frame(Code = character(0), Couleur = character(0),
                     debut = numeric(0), fin = numeric(0),
                     debut_pct = numeric(0), fin_pct = numeric(0),
                     Extrait = character(0), stringsAsFactors = FALSE)
  if (is.null(segments) || !nrow(segments) || is.null(docs) || !NROW(docs))
    return(vide)
  doc_id <- as.character(doc_id)[1]
  s <- segments[segments$doc_id == doc_id, , drop = FALSE]
  if (!nrow(s)) return(vide)
  k <- match(doc_id, docs$doc_id)
  n <- if (!is.na(k)) nchar(as.character(docs$text[k])) else 0L
  # Un document vide ou introuvable ne peut pas etre mis a l'echelle : plutot
  # que de diviser par zero (et de rendre des Inf silencieux), on garde les
  # positions brutes et le pourcentage reste a zero.
  ech <- if (is.finite(n) && n > 0) 100 / n else NA_real_
  s <- s[order(s$start), , drop = FALSE]
  out <- data.frame(
    Code      = hstat_code_label(codebook, s$code_id),
    Couleur   = hstat_code_color(codebook, s$code_id),
    debut     = as.numeric(s$start),
    fin       = as.numeric(s$end),
    debut_pct = if (is.na(ech)) 0 else pmin(100, as.numeric(s$start) * ech),
    fin_pct   = if (is.na(ech)) 0 else pmin(100, as.numeric(s$end) * ech),
    Extrait   = s$text,
    stringsAsFactors = FALSE)
  attr(out, "n_char") <- n
  out
}

# Trace du codeline. Une ligne par code, un rectangle par segment.
hstat_code_codeline_plot <- function(cl, titre = "") {
  if (is.null(cl) || !nrow(cl))
    return(ggplot2::ggplot() + ggplot2::theme_void() +
           ggplot2::annotate("text", x = 0, y = 0,
                             label = "Aucun codage sur ce document."))
  cl$Code <- factor(cl$Code, levels = rev(unique(cl$Code)))
  couleurs <- stats::setNames(cl$Couleur[!duplicated(cl$Code)],
                              as.character(cl$Code[!duplicated(cl$Code)]))
  ggplot2::ggplot(cl) +
    ggplot2::geom_rect(ggplot2::aes(xmin = debut_pct, xmax = pmax(fin_pct, debut_pct + 0.6),
                                    ymin = as.numeric(Code) - 0.38,
                                    ymax = as.numeric(Code) + 0.38,
                                    fill = Code), colour = NA) +
    ggplot2::scale_fill_manual(values = couleurs, guide = "none") +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(cl$Code)),
                                labels = levels(cl$Code)) +
    ggplot2::scale_x_continuous(limits = c(0, 100),
                                labels = function(x) paste0(x, " %")) +
    ggplot2::labs(x = "Position dans le document", y = NULL, title = titre) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank())
}


# ---------------------------------------------------------------------------
# 15. ACCORD INTER-CODEURS
# ---------------------------------------------------------------------------
# Deux personnes codent le meme corpus ; on mesure leur accord. L'unite de
# comparaison est le couple DOCUMENT x CODE : le codeur a-t-il, oui ou non,
# pose ce code sur ce document ?
#
# POURQUOI PAS LE SEGMENT. Deux codeurs ne decoupent jamais aux memes bornes ;
# comparer des segments exigerait un seuil de recouvrement arbitraire qui
# ferait varier le resultat plus que le desaccord reel. Le couple
# document x code est l'unite que MAXQDA propose par defaut, pour la meme
# raison.
#
# KAPPA N'EST PAS TOUJOURS DEFINI, ET C'EST LE PIEGE. Si les deux codeurs
# posent tout partout (ou rien nulle part), l'accord attendu par hasard vaut 1,
# le denominateur 1 - pe s'annule et kappa rend NaN. Brancher dessus lèverait
# « missing value where TRUE/FALSE needed ». On rend donc un VERDICT a quatre
# etats, dont `indeterminable`, comme partout ailleurs dans l'application.
#
# Le pourcentage d'accord, lui, reste toujours calculable : c'est ce qu'on
# affiche quand kappa se derobe.
# ---------------------------------------------------------------------------
hstat_code_accord <- function(segments, codebook, codeur_a, codeur_b,
                              docs = NULL) {
  res <- list(n_unites = 0L, accord = NA_real_, kappa = NA_real_,
              verdict = "indeterminable", table = NULL,
              a = as.character(codeur_a)[1], b = as.character(codeur_b)[1],
              message = "Aucune unite comparable : les deux codeurs n'ont pas travaille sur les memes documents.")
  if (is.null(segments) || !nrow(segments) || is.null(codebook) || !nrow(codebook))
    return(res)
  ca <- as.character(codeur_a)[1]; cb <- as.character(codeur_b)[1]
  if (is.na(ca) || is.na(cb) || identical(ca, cb)) {
    res$message <- "Choisissez deux codeurs differents."
    return(res)
  }
  sa <- segments[segments$source == ca, , drop = FALSE]
  sb <- segments[segments$source == cb, , drop = FALSE]
  # Seuls les documents que LES DEUX ont vus sont comparables : compter un
  # document qu'un seul codeur a ouvert ferait passer son absence de codage
  # pour un desaccord.
  communs <- intersect(unique(sa$doc_id), unique(sb$doc_id))
  if (!length(communs)) return(res)
  codes <- codebook$code_id
  if (!length(codes)) return(res)

  grille <- expand.grid(doc_id = communs, code_id = codes,
                        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  cle <- function(s) paste(s$doc_id, s$code_id, sep = "\r")
  ka <- unique(cle(sa)); kb <- unique(cle(sb))
  k  <- paste(grille$doc_id, grille$code_id, sep = "\r")
  va <- k %in% ka
  vb <- k %in% kb

  n <- length(k)
  res$n_unites <- as.integer(n)
  res$accord <- mean(va == vb)
  tb <- table(factor(va, levels = c(FALSE, TRUE)),
              factor(vb, levels = c(FALSE, TRUE)),
              dnn = c(ca, cb))
  res$table <- tb

  po <- res$accord
  pe <- (sum(va) / n) * (sum(vb) / n) + (sum(!va) / n) * (sum(!vb) / n)
  if (is.finite(pe) && abs(1 - pe) > 1e-12) {
    res$kappa <- (po - pe) / (1 - pe)
    # Seuils de Landis & Koch, ramenes aux quatre etats de l'application.
    res$verdict <- if (!is.finite(res$kappa)) "indeterminable"
                   else if (res$kappa >= 0.8) "excellent"
                   else if (res$kappa >= 0.6) "acceptable"
                   else "faible"
    res$message <- sprintf(
      "%s unites comparees (%s document(s) x %s code(s)).",
      n, length(communs), length(codes))
  } else {
    res$message <- paste0(
      "Kappa n'est pas calculable ici : les deux codeurs ont pose (ou omis) ",
      "les memes etiquettes partout, l'accord attendu par hasard vaut deja 1. ",
      "Le pourcentage d'accord reste lisible.")
  }
  res
}


# ===========================================================================
# INTERFACE
# ===========================================================================
# `mod_coding_ui()` renvoie un bloc (et non un tabItem) : il s'insere dans
# l'onglet « Analyses qualitatives » sous la famille « Codage (MAXQDA) ».
# ===========================================================================
mod_coding_ui <- function(id) {
  ns <- shiny::NS(id)

  css <- "
  .hstat-doc {
    background:#fffdf7; border:1px solid #e5d9c3; border-radius:6px;
    padding:16px 18px; min-height:260px; max-height:420px; overflow-y:auto;
    font-size:15px; line-height:1.85; white-space:pre-wrap; word-wrap:break-word;
    cursor:text; -webkit-user-select:text; user-select:text;
  }
  .hstat-doc mark.hstat-seg { color:inherit; }
  .hstat-code-chip {
    display:block; width:100%; text-align:left; margin-bottom:6px;
    padding:8px 10px; border-radius:6px; border:2px dashed transparent;
    cursor:pointer; font-weight:bold; font-size:13px; transition:all .12s;
  }
  .hstat-code-chip:hover { filter:brightness(1.08); }
  .hstat-chip-over {
    border:2px dashed #2c3e50 !important; transform:scale(1.03);
    box-shadow:0 2px 8px rgba(0,0,0,.25);
  }
  .hstat-chip-count {
    float:right; background:rgba(255,255,255,.85); color:#2c3e50;
    border-radius:10px; padding:0 8px; font-size:11px; font-weight:bold;
  }
  .hstat-chip-del {
    float:right; margin-right:6px; cursor:pointer; opacity:.7; font-weight:bold;
  }
  .hstat-chip-del:hover { opacity:1; }
  .hstat-seg-del { color:#c0392b; cursor:pointer; font-size:11px; font-weight:bold; }
  .hstat-seg-del:hover { text-decoration:underline; }
  .hstat-sel-box {
    background:#eaf4fb; border-left:4px solid #2e86c1; padding:8px 12px;
    border-radius:4px; font-size:13px; min-height:44px;
  }
  "

  # Capture de la selection + glisser-deposer. Les gestionnaires sont poses sur
  # `document` (delegation) car les puces de codes sont re-rendues a chaque
  # modification du livre de codes : un binding direct serait perdu.
  js <- sprintf("
  (function(){
    var DOC = '%s', DROP = '%s', SEL = '%s', DEL = '%s', RMSEG = '%s';
    var pending = null;

    function container(){ return document.getElementById(DOC); }

    function offsets(){
      var c = container();
      if (!c) return null;
      var sel = window.getSelection();
      if (!sel || sel.rangeCount === 0 || sel.isCollapsed) return null;
      var range = sel.getRangeAt(0);
      if (!c.contains(range.commonAncestorContainer)) return null;
      var pre = range.cloneRange();
      pre.selectNodeContents(c);
      pre.setEnd(range.startContainer, range.startOffset);
      var start = pre.toString().length;
      var txt = range.toString();
      if (!txt.length) return null;
      return { start: start, end: start + txt.length, text: txt };
    }

    function publish(){
      var o = offsets();
      if (o) pending = o;
      Shiny.setInputValue(SEL, o, {priority:'event'});
    }

    document.addEventListener('mouseup', function(){ setTimeout(publish, 0); });
    document.addEventListener('keyup',   function(e){
      if (e.shiftKey || e.key === 'Escape') setTimeout(publish, 0);
    });

    document.addEventListener('dragstart', function(e){
      var o = offsets();
      if (o) {
        pending = o;
        try { e.dataTransfer.setData('text/plain', o.text); } catch(err){}
      }
    });

    function chipOf(e){
      var t = e.target;
      if (!t || !t.closest) t = t && t.parentElement;
      return t && t.closest ? t.closest('.hstat-code-chip') : null;
    }

    document.addEventListener('dragover', function(e){
      var chip = chipOf(e);
      if (chip) { e.preventDefault(); chip.classList.add('hstat-chip-over'); }
    });
    document.addEventListener('dragleave', function(e){
      var chip = chipOf(e);
      if (chip) chip.classList.remove('hstat-chip-over');
    });

    function apply(chip){
      if (!chip || !pending) return false;
      Shiny.setInputValue(DROP, {
        code: chip.getAttribute('data-code'),
        start: pending.start, end: pending.end, text: pending.text,
        nonce: Math.random()
      }, {priority:'event'});
      pending = null;
      var s = window.getSelection(); if (s) s.removeAllRanges();
      Shiny.setInputValue(SEL, null, {priority:'event'});
      return true;
    }

    document.addEventListener('drop', function(e){
      var chip = chipOf(e);
      if (!chip) return;
      e.preventDefault();
      chip.classList.remove('hstat-chip-over');
      apply(chip);
    });

    // Clic sur une puce : applique la derniere selection (repli clavier /
    // tablette, ou le glisser-deposer natif n'est pas disponible).
    document.addEventListener('click', function(e){
      // closest() et non classList : le clic peut atterrir sur l'icone <i>
      // interne, auquel cas la cible n'est pas l'element porteur de la classe.
      var t = e.target;
      if (t && !t.closest) t = t.parentElement;
      var del = t && t.closest ? t.closest('.hstat-chip-del') : null;
      if (del) {
        Shiny.setInputValue(DEL, {code: del.getAttribute('data-code'), nonce: Math.random()},
                            {priority:'event'});
        e.stopPropagation();
        return;
      }
      // Retrait d'une etiquette depuis le panneau de droite. Les liens sont
      // recrees a chaque rendu : un actionLink par segment laisserait autant
      // d'observateurs derriere lui, d'ou cette delegation vers un seul input.
      var rm = t && t.closest ? t.closest('.hstat-seg-del') : null;
      if (rm) {
        Shiny.setInputValue(RMSEG, {seg: rm.getAttribute('data-seg'), nonce: Math.random()},
                            {priority:'event'});
        e.stopPropagation();
        return;
      }
      apply(chipOf(e));
    });
  })();
  ", ns("docText"), ns("drop_event"), ns("sel_event"), ns("del_event"),
     ns("rmseg_event"))

  shiny::tagList(
    shiny::tags$head(shiny::tags$style(shiny::HTML(css))),

    # ---- Bandeau d'aide ----
    shiny::div(style = "background:#fdf3e3;border-left:5px solid #e67e22;padding:12px 16px;border-radius:6px;margin-bottom:12px;",
      shiny::tags$strong(shiny::icon("highlighter"), " Atelier de codage (thematisation)"),
      shiny::tags$ol(style = "margin:6px 0 0 0;padding-left:20px;font-size:13px;",
        shiny::tags$li("Choisissez la question ouverte a coder, puis lisez les reponses une a une."),
        shiny::tags$li(shiny::HTML("<b>Selectionnez</b> un mot, une phrase ou un paragraphe a la souris.")),
        shiny::tags$li(shiny::HTML("<b>Glissez-deposez</b> la selection sur un code, a gauche (ou cliquez simplement sur le code).")),
        shiny::tags$li("Le segment recoit une etiquette coloree ; cliquez sur un code pour retrouver tous ses extraits."))),

    shiny::fluidRow(
      # ------------------------------------------------ Livre de codes
      shiny::column(3,
        shinydashboard::box(width = 12, status = "warning", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("tags"), " Livre de codes"),
          shiny::fluidRow(
            shiny::column(8, shiny::textInput(ns("new_code"), NULL,
                                              placeholder = "Ex. Prix trop eleve")),
            shiny::column(4, colourpicker::colourInput(ns("new_color"), NULL,
                                                       value = HSTAT_CODE_PALETTE[1],
                                                       showColour = "background"))),
          # Code parent : c'est ce qui fait du livre de codes un ARBRE. Sans
          # lui, la hierarchie finit encodee dans les libelles (« Prix - trop
          # cher ») et aucune agregation par branche n'est plus possible.
          shiny::uiOutput(ns("new_parent_ui")),
          shiny::actionButton(ns("add_code"), "Ajouter le code",
                              icon = shiny::icon("plus"), class = "btn-warning btn-block btn-sm"),
          shiny::hr(style = "margin:10px 0;"),
          shiny::uiOutput(ns("code_chips")),
          shiny::hr(style = "margin:10px 0;"),
          shiny::uiOutput(ns("move_code_ui")),
          shiny::hr(style = "margin:10px 0;"),
          shiny::tags$small(style = "color:#7f8c8d;", shiny::icon("info-circle"),
            " Deposez une selection sur un code, ou cliquez dessus. Le ",
            shiny::tags$b("x"), " supprime le code ; ses sous-codes remontent ",
            "d'un niveau plutot que d'etre emportes avec lui."),
          shiny::br(), shiny::br(),
          shiny::actionButton(ns("clear_codes"), "Vider le livre de codes",
                              icon = shiny::icon("trash"), class = "btn-danger btn-xs btn-block"))
      ),

      # ------------------------------------------------ Document
      shiny::column(6,
        shinydashboard::box(width = 12, status = "warning", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("file-lines"), " Reponse a coder"),
          shiny::fluidRow(
            shiny::column(6, shiny::uiOutput(ns("doc_var_ui"))),
            shiny::column(6, shiny::uiOutput(ns("nav_ui")))),
          shiny::uiOutput(ns("doc_header")),
          # Le conteneur EST la sortie : son contenu textuel doit correspondre
          # caractere pour caractere au texte du document. Un uiOutput imbrique
          # dans un div ajoutait l'indentation du HTML genere (retour a la ligne
          # + espaces) au debut du conteneur, decalant d'autant les positions
          # renvoyees par la selection JavaScript.
          shiny::uiOutput(ns("docText"), class = "hstat-doc"),
          shiny::br(),
          shiny::div(class = "hstat-sel-box", shiny::uiOutput(ns("sel_info"))),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(4, shiny::actionButton(ns("prev_doc"), "Precedente",
                                                 icon = shiny::icon("chevron-left"),
                                                 class = "btn-default btn-sm btn-block")),
            shiny::column(4, shiny::actionButton(ns("next_doc"), "Suivante",
                                                 icon = shiny::icon("chevron-right"),
                                                 class = "btn-primary btn-sm btn-block")),
            shiny::column(4, shiny::actionButton(ns("next_uncoded"), "Non codee",
                                                 icon = shiny::icon("forward"),
                                                 class = "btn-default btn-sm btn-block"))),
          shiny::tags$script(shiny::HTML(js)))
      ),

      # ------------------------------------------------ Segments du document
      shiny::column(3,
        shinydashboard::box(width = 12, status = "warning", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("list-check"), " Etiquettes de cette reponse"),
          shiny::uiOutput(ns("doc_segments")),
          shiny::hr(style = "margin:10px 0;"),
          shiny::uiOutput(ns("progress_box")))
      )
    ),

    # ------------------------------------------------ Analyses
    shiny::fluidRow(
      shinydashboard::box(width = 12, status = "warning", solidHeader = TRUE,
        title = shiny::tagList(shiny::icon("magnifying-glass-chart"),
                               " Analyse, croisement et rapports"),
        shiny::tabsetPanel(id = ns("codeTabs"),

          # ---- Recuperation ----
          shiny::tabPanel(shiny::tagList(shiny::icon("quote-left"), " Recuperation"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(4, shiny::uiOutput(ns("ret_codes_ui"))),
              shiny::column(3, shiny::uiOutput(ns("ret_var_ui"))),
              shiny::column(3, shiny::uiOutput(ns("ret_lvl_ui"))),
              shiny::column(2, shiny::textInput(ns("ret_search"), "Rechercher dans les extraits",
                                                placeholder = "mot-cle"))),
            shiny::uiOutput(ns("ret_note")),
            DT::DTOutput(ns("ret_table")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, shiny::downloadButton(ns("dl_ret_csv"), "CSV",
                                                     class = "btn-info btn-sm btn-block",
                                                     icon = shiny::icon("file-csv"))),
              shiny::column(3, shiny::downloadButton(ns("dl_ret_xlsx"), "Excel",
                                                     class = "btn-success btn-sm btn-block",
                                                     icon = shiny::icon("file-excel"))))),

          # ---- Requete combinee ----
          shiny::tabPanel(shiny::tagList(shiny::icon("code-branch"), " Requete combinee"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, shiny::uiOutput(ns("qry_a_ui"))),
              shiny::column(2, shiny::radioButtons(ns("qry_op"), "Operateur",
                choices = c("A ET B" = "et", "A SAUF B" = "sauf", "A OU B" = "ou"),
                selected = "et")),
              shiny::column(3, shiny::uiOutput(ns("qry_b_ui"))),
              shiny::column(2, shiny::radioButtons(ns("qry_portee"), "Portee",
                choices = c("Meme document" = "document",
                            "Meme passage" = "overlap",
                            "A proximite" = "proximite"),
                selected = "document")),
              shiny::column(2, shiny::numericInput(ns("qry_dist"),
                "Distance (caracteres)", value = 200, min = 0, step = 50))),
            shiny::uiOutput(ns("qry_note")),
            DT::DTOutput(ns("qry_table")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, shiny::downloadButton(ns("dl_qry_csv"), "CSV",
                                                     class = "btn-info btn-sm btn-block",
                                                     icon = shiny::icon("file-csv"))))),

          # ---- Concordancier ----
          shiny::tabPanel(shiny::tagList(shiny::icon("magnifying-glass"), " Concordancier"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(4, shiny::textInput(ns("kwic_motif"), "Mot ou expression",
                                                placeholder = "ex. prix")),
              shiny::column(2, shiny::numericInput(ns("kwic_fen"), "Contexte (caracteres)",
                                                   value = 45, min = 10, max = 200, step = 5)),
              shiny::column(3, shiny::checkboxInput(ns("kwic_regex"),
                "Interpreter comme expression reguliere", value = FALSE)),
              shiny::column(3, shiny::checkboxInput(ns("kwic_casse"),
                "Respecter la casse", value = FALSE))),
            shiny::uiOutput(ns("kwic_note")),
            DT::DTOutput(ns("kwic_table")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, shiny::downloadButton(ns("dl_kwic_csv"), "CSV",
                                                     class = "btn-info btn-sm btn-block",
                                                     icon = shiny::icon("file-csv"))))),

          # ---- Portrait du document (codeline) ----
          shiny::tabPanel(shiny::tagList(shiny::icon("chart-gantt"), " Portrait du document"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(6, shiny::uiOutput(ns("cl_doc_ui")))),
            shiny::uiOutput(ns("cl_note")),
            shiny::plotOutput(ns("cl_plot"), height = "360px"),
            shiny::br(),
            DT::DTOutput(ns("cl_table"))),

          # ---- Accord inter-codeurs ----
          shiny::tabPanel(shiny::tagList(shiny::icon("user-group"), " Accord inter-codeurs"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(4, shiny::uiOutput(ns("acc_a_ui"))),
              shiny::column(4, shiny::uiOutput(ns("acc_b_ui")))),
            shiny::uiOutput(ns("acc_resume")),
            shiny::br(),
            shiny::tableOutput(ns("acc_table"))),

          # ---- Matrice de croisement ----
          shiny::tabPanel(shiny::tagList(shiny::icon("table-cells"), " Matrice de croisement"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(4, shiny::uiOutput(ns("mat_var_ui"))),
              shiny::column(4, shiny::radioButtons(ns("mat_count"), "Unite de comptage",
                choices = c("Segments codes" = "segments",
                            "Repondants distincts" = "documents"),
                selected = "segments", inline = TRUE)),
              shiny::column(4, shiny::radioButtons(ns("mat_disp"), "Affichage",
                choices = c("Effectifs" = "n", "% colonne" = "col", "% ligne" = "row"),
                selected = "n", inline = TRUE))),
            DT::DTOutput(ns("mat_table")),
            shiny::br(),
            shiny::plotOutput(ns("mat_plot"), height = "420px"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, shiny::downloadButton(ns("dl_mat_csv"), "CSV",
                                                     class = "btn-info btn-sm btn-block",
                                                     icon = shiny::icon("file-csv"))),
              shiny::column(3, shiny::downloadButton(ns("dl_mat_xlsx"), "Excel",
                                                     class = "btn-success btn-sm btn-block",
                                                     icon = shiny::icon("file-excel"))))),

          # ---- Nuage de mots ----
          shiny::tabPanel(shiny::tagList(shiny::icon("cloud"), " Nuage de mots"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, shiny::radioButtons(ns("cloud_src"), "Corpus",
                choices = c("Toutes les reponses" = "all",
                            "Segments codes seulement" = "coded"),
                selected = "all")),
              shiny::column(3, shiny::sliderInput(ns("cloud_max"), "Nombre de mots",
                                                  min = 20, max = 150, value = 80, step = 10)),
              shiny::column(3, shiny::sliderInput(ns("cloud_size"), "Taille max. du texte",
                                                  min = 8, max = 30, value = 16, step = 1)),
              shiny::column(3, shiny::sliderInput(ns("cloud_minchar"), "Longueur min. des mots",
                                                  min = 2, max = 8, value = 4, step = 1))),
            shiny::fluidRow(
              shiny::column(4, shiny::uiOutput(ns("cloud_code_ui"))),
              shiny::column(4, shiny::checkboxInput(ns("cloud_stem"),
                                                    "Racinisation (stemming)", value = TRUE)),
              shiny::column(4, shiny::textInput(ns("cloud_stop"), "Mots a exclure",
                                                placeholder = "separes par une virgule"))),
            shiny::plotOutput(ns("cloud_plot"), height = "520px"),
            shiny::br(),
            shiny::downloadButton(ns("dl_cloud"), "Telecharger le nuage (PNG)",
                                  class = "btn-success btn-sm")),

          # ---- Carte conceptuelle ----
          shiny::tabPanel(shiny::tagList(shiny::icon("diagram-project"), " Carte conceptuelle"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(4, shiny::radioButtons(ns("map_mode"), "Definition d'un lien",
                choices = c("Meme reponse" = "document",
                            "Meme passage (chevauchement)" = "overlap"),
                selected = "document")),
              shiny::column(4, shiny::sliderInput(ns("map_min"), "Cooccurrences minimales",
                                                  min = 1, max = 20, value = 1, step = 1)),
              shiny::column(4, shiny::sliderInput(ns("map_lbl"), "Taille des etiquettes (pt)",
                                                  min = 8, max = 24, value = 12, step = 1))),
            shiny::plotOutput(ns("map_plot"), height = "560px"),
            shiny::br(),
            shiny::tags$strong(shiny::icon("table"), " Matrice de cooccurrences"),
            DT::DTOutput(ns("cooc_table")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, shiny::downloadButton(ns("dl_map"), "Carte (PNG)",
                                                     class = "btn-success btn-sm btn-block")),
              shiny::column(3, shiny::downloadButton(ns("dl_cooc_xlsx"), "Matrice (Excel)",
                                                     class = "btn-success btn-sm btn-block",
                                                     icon = shiny::icon("file-excel"))))),

          # ---- Assistant de codage (trois moteurs) ----
          shiny::tabPanel(shiny::tagList(shiny::icon("wand-magic-sparkles"), " Assistant de codage"),
            shiny::br(),
            shiny::div(style = "background:#eafaf1;border-left:5px solid #27ae60;padding:12px 16px;border-radius:6px;",
              shiny::tags$strong(shiny::icon("shield-halved"),
                                 " Gratuit, local et hors ligne par defaut"),
              shiny::tags$p(style = "margin:6px 0 0 0;font-size:13px;",
                "L'assistant propose un livre de codes a partir de votre corpus, puis ",
                "pre-code les reponses. Il tourne ",
                shiny::tags$b("sur votre machine"), " : aucune donnee d'enquete ne part ",
                "sur Internet et il n'y a rien a payer. ",
                shiny::tags$b("Ses propositions restent des propositions"),
                " : elles arrivent etiquetees et vous pouvez les corriger ou les ",
                "supprimer une a une.")),
            shiny::br(),

            # ---- Choix du moteur ----
            shiny::fluidRow(
              shiny::column(5,
                shiny::radioButtons(ns("ai_engine"), "Moteur",
                  choices = HSTAT_AI_ENGINES, selected = "local"),
                shiny::tags$small(style = "color:#7f8c8d;display:block;",
                  shiny::icon("circle-info"),
                  " Le modele local est le plus performant sur le sens ; la ",
                  "thematisation automatique ne demande aucune installation et ",
                  "repond instantanement.")),

              # --- Reglages du modele local ---
              shiny::column(7,
                shiny::conditionalPanel(sprintf("input['%s'] == 'local'", ns("ai_engine")),
                  shiny::fluidRow(
                    shiny::column(5,
                      shiny::radioButtons(ns("ai_backend"), "Serveur d'inference",
                        choices = HSTAT_AI_BACKENDS, selected = "ollama")),
                    shiny::column(7,
                      shiny::textInput(ns("ai_url"), "Adresse du serveur",
                                       value = unname(HSTAT_AI_DEFAULT_URL[["ollama"]])),
                      shiny::uiOutput(ns("ai_model_ui")))),
                  shiny::actionButton(ns("ai_ping"), "Tester la connexion et lister les modeles",
                                      icon = shiny::icon("plug-circle-check"),
                                      class = "btn-default btn-sm"),
                  shiny::tags$small(style = "color:#7f8c8d;display:block;margin-top:6px;",
                    shiny::icon("download"),
                    " Une seule installation suffit : ", shiny::tags$b("ollama.com"),
                    " (gratuit), puis ", shiny::tags$code("ollama pull qwen2.5"),
                    " dans un terminal. Le modele telecharge, tout fonctionne ",
                    "sans connexion Internet.")),

                shiny::conditionalPanel(sprintf("input['%s'] == 'auto'", ns("ai_engine")),
                  shiny::div(style = "background:#f8f9fa;border:1px solid #e0e0e0;border-radius:6px;padding:10px 14px;font-size:13px;",
                    shiny::tags$b(shiny::icon("calculator"), " Comment ca marche"),
                    shiny::tags$p(style = "margin:6px 0 0 0;",
                      "Les termes du corpus sont regroupes par classification ",
                      "hierarchique sur leurs cooccurrences dans les reponses : deux ",
                      "mots qui reviennent dans les memes reponses forment un theme. ",
                      "Chaque theme arrive avec son ", shiny::tags$b("dictionnaire"),
                      " de mots-cles, editable ci-dessous, qui sert ensuite au ",
                      "pre-codage de ", shiny::tags$b("tout"), " le corpus."))),

                shiny::conditionalPanel(sprintf("input['%s'] == 'claude'", ns("ai_engine")),
                  shiny::passwordInput(ns("ai_key"), "Cle d'API Anthropic",
                                       placeholder = "sk-ant-... (ou variable ANTHROPIC_API_KEY)"),
                  shiny::tags$small(style = "color:#7f8c8d;",
                    shiny::icon("lock"), " La cle n'est utilisee que pour cette session ",
                    "et n'est ni enregistree, ni exportee avec vos resultats. ",
                    shiny::tags$b("Cette option est payante"), " et envoie vos reponses ",
                    "chez un tiers ; les deux autres moteurs ne le font pas.")))),

            shiny::hr(),
            shiny::fluidRow(
              shiny::column(3,
                shiny::sliderInput(ns("ai_ncodes"), "Nombre de codes a proposer",
                                   min = 3, max = 20, value = 8, step = 1)),
              shiny::column(3,
                shiny::sliderInput(ns("ai_minchar"), "Longueur min. des mots",
                                   min = 3, max = 8, value = 4, step = 1),
                shiny::tags$small(style = "color:#7f8c8d;",
                                  "Thematisation automatique uniquement.")),
              shiny::column(3,
                shiny::numericInput(ns("ai_maxdoc"), "Reponses envoyees au modele (max.)",
                                    value = 60, min = 5, max = 300, step = 5),
                shiny::tags$small(style = "color:#7f8c8d;",
                  "Sans effet sur la thematisation automatique et le dictionnaire, ",
                  "qui traitent tout le corpus.")),
              shiny::column(3,
                shiny::textAreaInput(ns("ai_context"), "Contexte de l'enquete (facultatif)",
                                     rows = 3,
                                     placeholder = "Ex. enquete de satisfaction clients, secteur telecom"))),

            shiny::uiOutput(ns("ai_status")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(4, shiny::actionButton(ns("ai_codebook"),
                "1. Proposer un livre de codes", icon = shiny::icon("lightbulb"),
                class = "btn-primary btn-block")),
              shiny::column(4, shiny::actionButton(ns("ai_autocode"),
                "2. Pre-coder les reponses", icon = shiny::icon("highlighter"),
                class = "btn-warning btn-block")),
              shiny::column(4, shiny::actionButton(ns("ai_drop"),
                "Supprimer les codages automatiques", icon = shiny::icon("eraser"),
                class = "btn-danger btn-block"))),
            shiny::br(),
            shiny::uiOutput(ns("ai_result")),
            DT::DTOutput(ns("ai_table")),

            # ---- Dictionnaire des codes ----
            shiny::hr(),
            shiny::div(style = "background:#fef9e7;border-left:5px solid #d4ac0d;padding:12px 16px;border-radius:6px;",
              shiny::tags$strong(shiny::icon("book"), " Dictionnaire des codes"),
              shiny::tags$p(style = "margin:6px 0 0 0;font-size:13px;",
                "Chaque code peut porter une liste de mots-cles. Le pre-codage par ",
                "dictionnaire etiquette alors, dans tout le corpus, la phrase qui ",
                "contient l'un d'eux — sans modele, sans reseau et en un instant. ",
                "Les accents et la casse sont ignores a la recherche.")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(4, shiny::uiOutput(ns("dict_code_ui"))),
              shiny::column(8, shiny::textAreaInput(ns("dict_words"),
                "Mots-cles (separes par des points-virgules)", rows = 2,
                placeholder = "prix; tarif; cher; couteux; excessif"))),
            shiny::fluidRow(
              shiny::column(4, shiny::actionButton(ns("dict_save"),
                "Enregistrer les mots-cles de ce code", icon = shiny::icon("floppy-disk"),
                class = "btn-default btn-block btn-sm")),
              shiny::column(4, shiny::radioButtons(ns("dict_scope"), NULL,
                choices = c("Etiqueter la phrase entiere" = "phrase",
                            "Etiqueter le mot seul" = "mot"),
                selected = "phrase", inline = FALSE)),
              shiny::column(4, shiny::actionButton(ns("dict_apply"),
                "Appliquer le dictionnaire a tout le corpus",
                icon = shiny::icon("wand-magic"), class = "btn-success btn-block btn-sm"))),
            shiny::br(),
            DT::DTOutput(ns("dict_table"))),

          # ---- Memos ----
          # Le memo est ce qui transforme un codage en analyse : pourquoi ce
          # code existe, ou passe sa frontiere, ce qu'un entretien a
          # d'atypique. C'est aussi ce qu'un relecteur demande pour comprendre
          # comment on est arrive la.
          shiny::tabPanel(shiny::tagList(shiny::icon("note-sticky"), " Memos"),
            shiny::br(),
            shiny::div(style = "background:#eafaf1;border-left:5px solid #27ae60;padding:12px 16px;border-radius:6px;font-size:13px;",
              shiny::tags$strong(shiny::icon("pen-to-square"), " Le carnet de bord de votre analyse"),
              shiny::tags$p(style = "margin:6px 0 0 0;",
                "Notez pourquoi un code existe, ou passe sa frontiere, ce qu'un ",
                "entretien a de particulier, l'hypothese qui se dessine. ",
                shiny::tags$b("Rien de tout cela ne se retrouve dans les tableaux"),
                " — et c'est pourtant ce qu'on vous demandera pour justifier vos ",
                "conclusions.")),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(4,
                shiny::selectInput(ns("memo_type"), "Porte sur",
                                   choices = HSTAT_MEMO_CIBLES, selected = "libre"),
                shiny::uiOutput(ns("memo_cible_ui")),
                shiny::textInput(ns("memo_titre"), "Titre (facultatif)",
                                 placeholder = "Deduit du texte s'il est vide"),
                shiny::textAreaInput(ns("memo_texte"), "Memo", rows = 6,
                                     placeholder = "Ce que vous voulez retrouver dans six mois."),
                shiny::actionButton(ns("memo_add"), "Enregistrer le memo",
                                    icon = shiny::icon("plus"),
                                    class = "btn-success btn-block"),
                shiny::br(),
                shiny::uiOutput(ns("memo_resume"))),
              shiny::column(8,
                shiny::fluidRow(
                  shiny::column(7, shiny::textInput(ns("memo_q"), NULL,
                    placeholder = "Rechercher dans les memos (accents ignores)")),
                  shiny::column(5, shiny::selectInput(ns("memo_filtre"), NULL,
                    choices = c("Tous les memos" = "", HSTAT_MEMO_CIBLES)))),
                DT::DTOutput(ns("memo_table")),
                shiny::br(),
                shiny::uiOutput(ns("memo_detail")),
                shiny::downloadButton(ns("dl_memos"), "Telecharger les memos (CSV)",
                                      class = "btn-info btn-sm")))),

          # ---- Export / sauvegarde ----
          shiny::tabPanel(shiny::tagList(shiny::icon("file-export"), " Export & sauvegarde"),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(6,
                shiny::tags$strong(shiny::icon("file-excel"), " Rapport complet"),
                shiny::tags$p(style = "font-size:13px;color:#7f8c8d;",
                  "Un classeur Excel reunissant le livre de codes, tous les segments ",
                  "codes, la matrice de croisement et la matrice de cooccurrences."),
                shiny::downloadButton(ns("dl_report"), "Telecharger le rapport (Excel)",
                                      class = "btn-success btn-block")),
              shiny::column(6,
                shiny::tags$strong(shiny::icon("floppy-disk"), " Projet de codage"),
                shiny::tags$p(style = "font-size:13px;color:#7f8c8d;",
                  "Sauvegarde du livre de codes et des segments au format RDS, pour ",
                  "reprendre le codage plus tard sur le meme jeu de donnees."),
                shiny::downloadButton(ns("dl_project"), "Enregistrer le projet (.rds)",
                                      class = "btn-info btn-block"),
                shiny::br(), shiny::br(),
                shiny::fileInput(ns("up_project"), "Recharger un projet (.rds)",
                                 accept = ".rds",
                                 buttonLabel = "Parcourir", placeholder = "Aucun fichier")))
            ))
        ))
  )
}


# ===========================================================================
# SERVEUR
# ===========================================================================
mod_coding_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      codebook = hstat_code_new_codebook(),
      segments = hstat_code_new_segments(),
      cur      = 1L,
      sel      = NULL,
      ai       = NULL,
      memos    = hstat_memo_new(),
      # Teinte proposee par defaut pour le prochain code : memorisee cote R
      # pour ne pas dependre de l'aller-retour du selecteur de couleur.
      next_color = HSTAT_CODE_PALETTE[1])

    # ---------------------------------------------------------------- corpus
    get_data <- shiny::reactive({
      d <- values$data
      shiny::validate(shiny::need(!is.null(d) && nrow(d) > 0,
        "Chargez d'abord un jeu de donnees dans l'onglet Chargement."))
      as.data.frame(d)
    })

    # Colonnes candidates : du texte, avec une vraie diversite de reponses.
    # Un facteur a 4 modalites n'est pas une question ouverte.
    text_vars <- shiny::reactive({
      d <- get_data()
      keep <- vapply(names(d), function(v) {
        x <- d[[v]]
        if (!(is.character(x) || is.factor(x))) return(FALSE)
        x <- as.character(x)
        x <- x[!is.na(x) & nzchar(trimws(x))]
        if (!length(x)) return(FALSE)
        length(unique(x)) >= 3 && stats::median(nchar(x)) >= 12
      }, logical(1))
      out <- names(d)[keep]
      if (!length(out)) out <- names(d)[vapply(d, function(x)
        is.character(x) || is.factor(x), logical(1))]
      out
    })

    output$doc_var_ui <- shiny::renderUI({
      v <- text_vars()
      shiny::validate(shiny::need(length(v) > 0,
        "Aucune colonne textuelle dans ce jeu de donnees."))
      shiny::selectInput(ns("doc_var"), "Question ouverte a coder",
                         choices = v, selected = shiny::isolate(input$doc_var) %||% v[1])
    })

    docs <- shiny::reactive({
      d <- get_data()
      v <- input$doc_var
      shiny::req(!is.null(v), v %in% names(d))
      hstat_code_docs(d, v, min_char = 1)
    })

    profile_vars <- shiny::reactive({
      d <- get_data()
      setdiff(names(d), input$doc_var)
    })

    profile <- shiny::reactive({
      hstat_code_profile(get_data(), docs(), profile_vars())
    })

    # Changer de question ouverte remet la navigation a zero ; les codages
    # restent (ils portent un doc_id lie a la ligne, pas a la colonne), mais on
    # previent l'utilisateur car ils ne correspondent plus au texte affiche.
    shiny::observeEvent(input$doc_var, {
      rv$cur <- 1L
      if (nrow(rv$segments) > 0)
        shiny::showNotification(
          "Changement de question : les etiquettes deja posees concernent la question precedente.",
          type = "warning", duration = 8)
    }, ignoreInit = TRUE)

    cur_doc <- shiny::reactive({
      dd <- docs()
      shiny::req(nrow(dd) > 0)
      i <- max(1L, min(nrow(dd), as.integer(rv$cur)))
      dd[i, , drop = FALSE]
    })

    # ---------------------------------------------------------- navigation
    output$nav_ui <- shiny::renderUI({
      dd <- docs()
      shiny::req(nrow(dd) > 0)
      shiny::sliderInput(ns("doc_idx"), sprintf("Reponse (%d au total)", nrow(dd)),
                         min = 1, max = nrow(dd), value = min(rv$cur, nrow(dd)),
                         step = 1, ticks = FALSE, width = "100%")
    })

    shiny::observeEvent(input$doc_idx, {
      if (!is.null(input$doc_idx) && input$doc_idx != rv$cur) rv$cur <- as.integer(input$doc_idx)
    })
    shiny::observeEvent(rv$cur, {
      if (!is.null(input$doc_idx) && input$doc_idx != rv$cur)
        shiny::updateSliderInput(session, "doc_idx", value = rv$cur)
    })
    shiny::observeEvent(input$prev_doc, rv$cur <- max(1L, rv$cur - 1L))
    shiny::observeEvent(input$next_doc, rv$cur <- min(nrow(docs()), rv$cur + 1L))
    shiny::observeEvent(input$next_uncoded, {
      dd <- docs()
      coded <- unique(rv$segments$doc_id)
      free <- which(!(dd$doc_id %in% coded))
      nxt <- free[free > rv$cur]
      if (length(nxt)) rv$cur <- nxt[1]
      else if (length(free)) {
        rv$cur <- free[1]
        shiny::showNotification("Retour au debut : premiere reponse non codee.",
                                type = "message", duration = 4)
      } else shiny::showNotification("Toutes les reponses portent au moins une etiquette.",
                                     type = "message", duration = 4)
    })

    output$doc_header <- shiny::renderUI({
      dd <- docs(); d <- cur_doc()
      shiny::div(style = "margin-bottom:6px;font-size:12px;color:#7f8c8d;",
        shiny::icon("hashtag"), sprintf(" Reponse %d / %d ", rv$cur, nrow(dd)),
        shiny::tags$b(sprintf("(ligne %d du jeu de donnees)", d$row)),
        sprintf(" - %d caracteres", nchar(d$text)))
    })

    # ------------------------------------------------- affichage du document
    output$docText <- shiny::renderUI({
      d <- cur_doc()
      segs <- hstat_seg_for_doc(rv$segments, d$doc_id)
      shiny::HTML(hstat_code_highlight_html(d$text, segs, rv$codebook))
    })

    # -------------------------------------------------------- selection JS
    shiny::observeEvent(input$sel_event, {
      rv$sel <- input$sel_event
    }, ignoreNULL = FALSE)

    output$sel_info <- shiny::renderUI({
      s <- rv$sel
      if (is.null(s) || is.null(s$text) || !nzchar(s$text))
        return(shiny::HTML(paste0(
          "<span style='color:#7f8c8d;'>", as.character(shiny::icon("mouse-pointer")),
          " Selectionnez un passage du texte ci-dessus, puis deposez-le (ou cliquez) sur un code.</span>")))
      shiny::HTML(sprintf(
        "%s <b>Selection</b> (%d caracteres, positions %d-%d) : <i>&laquo;&nbsp;%s&nbsp;&raquo;</i>",
        as.character(shiny::icon("i-cursor")), nchar(s$text),
        as.integer(s$start), as.integer(s$end),
        .hstat_code_esc(substr(s$text, 1, 220))))
    })

    # ------------------------------------------------------ livre de codes
    shiny::observeEvent(input$add_code, {
      lab <- trimws(input$new_code %||% "")
      if (!nzchar(lab)) {
        shiny::showNotification("Saisissez d'abord un libelle de code.",
                                type = "warning", duration = 4); return()
      }
      # La couleur n'est imposee que si l'utilisateur l'a lui-meme changee ;
      # sinon le livre de codes choisit la prochaine teinte libre. On compare a
      # la suggestion memorisee cote R plutot qu'a la valeur du widget : quand
      # deux codes sont crees a la suite, l'aller-retour du selecteur n'est pas
      # forcement revenu, et les deux codes heritaient de la meme couleur.
      chosen <- input$new_color
      col <- if (is.null(chosen) ||
                 identical(tolower(chosen), tolower(rv$next_color))) NULL else chosen
      before <- nrow(rv$codebook)
      parent <- input$new_parent %||% ""
      rv$codebook <- hstat_code_add(rv$codebook, lab, col, parent_id = parent)
      if (nrow(rv$codebook) == before) {
        shiny::showNotification(
          if (nzchar(parent))
            sprintf("Le code « %s » existe deja sous ce parent. Sous un autre parent, il serait accepte.", lab)
          else trf("Le code « %s » existe deja a la racine.", lab),
          type = "warning", duration = 6)
      } else {
        shiny::updateTextInput(session, "new_code", value = "")
        rv$next_color <- hstat_code_next_color(rv$codebook)
        # updateColourInput() vient de {colourpicker}, pas de {shiny} :
        # `shiny::updateColourInput` leve une erreur et bloque l'ajout de code.
        colourpicker::updateColourInput(session, "new_color", value = rv$next_color)
      }
    })

    shiny::observeEvent(input$clear_codes, {
      rv$codebook <- hstat_code_new_codebook()
      rv$segments <- hstat_code_new_segments()
      rv$next_color <- HSTAT_CODE_PALETTE[1]
      colourpicker::updateColourInput(session, "new_color", value = rv$next_color)
      shiny::showNotification("Livre de codes et etiquettes supprimes.",
                              type = "message", duration = 4)
    })

    shiny::observeEvent(input$del_event, {
      cid <- input$del_event$code
      shiny::req(!is.null(cid))
      lab <- hstat_code_label(rv$codebook, cid)
      n <- sum(rv$segments$code_id == cid)
      rv$segments <- hstat_seg_drop_code(rv$segments, cid)
      rv$codebook <- hstat_code_remove(rv$codebook, cid)
      shiny::showNotification(
        sprintf("Code « %s » supprime (%d etiquette(s) retiree(s)).", lab, n),
        type = "message", duration = 5)
    })

    # ------------------------------------------------------------- memos
    # La cible depend du type : un memo de code se rattache a un code, un memo
    # de document a une reponse, un memo libre a rien du tout.
    output$memo_cible_ui <- shiny::renderUI({
      type <- input$memo_type %||% "libre"
      if (identical(type, "libre")) return(NULL)
      ch <- switch(type,
        code = { cb <- rv$codebook
                 if (!nrow(cb)) character(0)
                 else { a <- hstat_code_tree(cb); stats::setNames(a$code_id, a$chemin) } },
        document = { d <- docs()
                     if (is.null(d) || !nrow(d)) character(0)
                     else stats::setNames(d$doc_id, substr(d$text, 1, 70)) },
        segment = { s <- rv$segments
                    if (is.null(s) || !nrow(s)) character(0)
                    else stats::setNames(s$seg_id, substr(s$text, 1, 70)) },
        character(0))
      if (!length(ch))
        return(shiny::tags$small(style = "color:#b9770e;",
          shiny::icon("circle-info"),
          " Rien a quoi rattacher ce memo pour l'instant."))
      shiny::selectInput(ns("memo_cible"), "Cible", choices = ch)
    })

    shiny::observeEvent(input$memo_add, {
      avant <- nrow(rv$memos)
      rv$memos <- hstat_memo_add(rv$memos, input$memo_type %||% "libre",
                                 input$memo_cible %||% "",
                                 input$memo_titre %||% "", input$memo_texte %||% "")
      if (nrow(rv$memos) == avant) {
        shiny::showNotification(
          "Un memo vide n'est pas enregistre : saisissez un titre ou un texte.",
          type = "warning", duration = 5)
      } else {
        shiny::updateTextInput(session, "memo_titre", value = "")
        shiny::updateTextAreaInput(session, "memo_texte", value = "")
        shiny::showNotification("Memo enregistre.", type = "message", duration = 3)
      }
    })

    # Les memos de code saisis dans l'ancien champ du livre de codes sont
    # repris ici : un projet ancien ne doit pas voir ses memos disparaitre.
    shiny::observeEvent(rv$codebook, {
      rv$memos <- hstat_memo_sync_codes(rv$memos, rv$codebook)
    }, ignoreInit = TRUE)

    memos_vus <- shiny::reactive({
      m <- hstat_memo_search(rv$memos, input$memo_q %||% "")
      f <- input$memo_filtre %||% ""
      if (nzchar(f)) m <- m[m$cible_type == f, , drop = FALSE]
      m
    })

    output$memo_table <- DT::renderDT({
      m <- memos_vus()
      shiny::validate(shiny::need(nrow(m) > 0,
        "Aucun memo. Le premier est souvent le plus utile : pourquoi ce code."))
      # Le libelle de la cible plutot que son identifiant : « prix-trop-eleve »
      # ne dit rien a la relecture.
      cible <- vapply(seq_len(nrow(m)), function(i) {
        switch(m$cible_type[i],
          code = hstat_code_label(rv$codebook, m$cible_id[i]),
          document = { d <- docs()
                       j <- match(m$cible_id[i], d$doc_id)
                       if (is.na(j)) m$cible_id[i] else substr(d$text[j], 1, 60) },
          segment = { s <- rv$segments
                      j <- match(m$cible_id[i], s$seg_id)
                      if (is.na(j)) m$cible_id[i] else substr(s$text[j], 1, 60) },
          "")
      }, character(1))
      DT::datatable(
        data.frame(Porte_sur = names(HSTAT_MEMO_CIBLES)[
                     match(m$cible_type, HSTAT_MEMO_CIBLES)],
                   Cible = cible, Titre = m$titre,
                   Modifie = m$modified, check.names = FALSE,
                   stringsAsFactors = FALSE),
        rownames = FALSE, selection = "single",
        options = list(pageLength = 10, scrollX = TRUE))
    })

    output$memo_detail <- shiny::renderUI({
      i <- input$memo_table_rows_selected
      m <- memos_vus()
      if (!length(i) || i > nrow(m))
        return(shiny::tags$em(style = "color:#95a5a6;",
          "Selectionnez un memo pour le lire en entier."))
      shiny::div(style = "background:#fffdf5;border:1px solid #f0e2c0;border-radius:6px;padding:14px 18px;",
        shiny::h4(style = "margin-top:0;", m$titre[i]),
        # Le texte est ECHAPPE : un memo reste du contenu saisi par
        # l'utilisateur, on ne l'injecte pas tel quel dans la page.
        shiny::HTML(gsub("\n", "<br>", hstat_html_escape(m$texte[i]))),
        shiny::hr(style = "margin:10px 0;"),
        shiny::tags$small(style = "color:#7f8c8d;",
          trf("Cree le %s, modifie le %s.", m$created[i], m$modified[i])),
        shiny::br(),
        shiny::actionButton(ns("memo_del"), "Supprimer ce memo",
                            icon = shiny::icon("trash"), class = "btn-danger btn-xs"))
    })

    shiny::observeEvent(input$memo_del, {
      i <- input$memo_table_rows_selected
      m <- memos_vus()
      shiny::req(length(i), i <= nrow(m))
      rv$memos <- hstat_memo_remove(rv$memos, m$memo_id[i])
      shiny::showNotification("Memo supprime.", type = "message", duration = 3)
    })

    output$memo_resume <- shiny::renderUI({
      r <- hstat_memo_resume(rv$memos)
      if (!sum(r$Memos))
        return(shiny::tags$small(style = "color:#7f8c8d;",
          shiny::icon("circle-info"), " Aucun memo pour l'instant."))
      shiny::div(style = "font-size:12px;color:#566573;",
        shiny::tags$b("Memos enregistres"), shiny::br(),
        shiny::HTML(paste(sprintf("%s : %d", r$Cible, r$Memos), collapse = "<br>")))
    })

    output$dl_memos <- shiny::downloadHandler(
      filename = function() sprintf("hstat_memos_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file)
        utils::write.csv(rv$memos, file, row.names = FALSE, fileEncoding = "UTF-8"))

    output$code_chips <- shiny::renderUI({
      cb <- rv$codebook
      if (nrow(cb) == 0)
        return(shiny::div(style = "color:#7f8c8d;font-size:13px;font-style:italic;",
          "Aucun code pour l'instant. Creez-en un ci-dessus, ou laissez ",
          "l'assistant IA vous en proposer."))
      cnt <- hstat_code_counts(cb, rv$segments)
      # Les codes sont affiches dans l'ORDRE DE L'ARBRE, chaque enfant sous son
      # parent et decale : c'est la seule facon de lire une hierarchie.
      arbre <- hstat_code_tree(cb)
      shiny::tagList(lapply(seq_len(nrow(arbre)), function(k) {
        i <- match(arbre$code_id[k], cb$code_id)
        j <- match(arbre$code_id[k], cnt$code_id)
        a_enfants <- length(hstat_code_descendants(cb, cb$code_id[i])) > 0
        shiny::tags$div(
          class = "hstat-code-chip", `data-code` = cb$code_id[i],
          style = sprintf("background:%s;color:%s;margin-left:%dpx;",
                          cb$color[i], .hstat_code_ink(cb$color[i]),
                          as.integer(arbre$profondeur[k]) * 14L),
          title = paste0(arbre$chemin[k],
                         if (nzchar(cb$memo[i])) paste0("\n", cb$memo[i]) else ""),
          shiny::tags$span(class = "hstat-chip-del", `data-code` = cb$code_id[i],
                           title = "Supprimer ce code", shiny::HTML("&times;")),
          # Sur un code parent, l'effectif de la BRANCHE est ce qui interesse :
          # un total qui ignorerait les sous-codes afficherait zero sur un code
          # pourtant abondamment documente.
          shiny::tags$span(class = "hstat-chip-count",
            if (a_enfants)
              sprintf("%d (%d) seg.", cnt$n_seg[j], cnt$n_seg_cumul[j])
            else sprintf("%d seg. / %d rep.", cnt$n_seg[j], cnt$n_doc[j])),
          if (arbre$profondeur[k] > 0) shiny::HTML("&#8627; "),
          cb$label[i])
      }))
    })

    # Choix du code parent a la creation. « (racine) » d'abord : la majorite
    # des codes sont crees a la racine, ce doit rester le geste par defaut.
    output$new_parent_ui <- shiny::renderUI({
      cb <- rv$codebook
      if (is.null(cb) || !nrow(cb)) return(NULL)
      arbre <- hstat_code_tree(cb)
      ch <- stats::setNames(c("", arbre$code_id), c("(racine)", arbre$chemin))
      shiny::selectInput(ns("new_parent"), NULL, choices = ch, selected = "")
    })

    # Deplacer un code existant dans l'arbre. Le refus des cycles est assure
    # par hstat_code_update() : on ne peut pas se retrouver avec une branche
    # detachee de l'arbre, donc invisible.
    output$move_code_ui <- shiny::renderUI({
      cb <- rv$codebook
      if (is.null(cb) || nrow(cb) < 2) return(NULL)
      arbre <- hstat_code_tree(cb)
      ch <- stats::setNames(arbre$code_id, arbre$chemin)
      shiny::tagList(
        shiny::tags$small(style = "color:#7f8c8d;font-weight:bold;",
                          shiny::icon("sitemap"), " Deplacer un code"),
        shiny::selectInput(ns("move_code"), NULL, choices = ch),
        shiny::selectInput(ns("move_to"), "sous",
                           choices = stats::setNames(c("", arbre$code_id),
                                                     c("(racine)", arbre$chemin))),
        shiny::actionButton(ns("do_move"), "Deplacer",
                            icon = shiny::icon("arrows-up-down-left-right"),
                            class = "btn-default btn-xs btn-block"))
    })

    shiny::observeEvent(input$do_move, {
      shiny::req(input$move_code)
      avant <- rv$codebook$parent_id[rv$codebook$code_id == input$move_code]
      rv$codebook <- hstat_code_update(rv$codebook, input$move_code,
                                       parent_id = input$move_to %||% "")
      apres <- rv$codebook$parent_id[rv$codebook$code_id == input$move_code]
      if (identical(avant, apres) && !identical(avant, input$move_to %||% ""))
        shiny::showNotification(
          paste("Deplacement refuse : un code ne peut pas devenir enfant de",
                "lui-meme ni d'un de ses propres sous-codes — sa branche se",
                "detacherait de l'arbre."),
          type = "warning", duration = 8)
    })

    # ------------------------------------------------------- depot d'un code
    shiny::observeEvent(input$drop_event, {
      ev <- input$drop_event
      shiny::req(!is.null(ev), !is.null(ev$code))
      if (!(ev$code %in% rv$codebook$code_id)) return()
      d <- cur_doc()
      st <- as.numeric(ev$start); en <- as.numeric(ev$end)
      if (is.na(st) || is.na(en) || en <= st) {
        shiny::showNotification("Selection vide : selectionnez d'abord un passage.",
                                type = "warning", duration = 4); return()
      }
      # Le texte est relu dans le document (et non repris de la selection JS) :
      # c'est lui qui fait foi pour la restitution et l'export.
      txt <- substr(d$text, st + 1, en)
      # Filet de securite : si le navigateur et R ne comptent pas les caracteres
      # de la meme facon (normalisation d'espaces, caractere hors du plan
      # multilingue de base), on recale les bornes sur le texte selectionne
      # plutot que d'etiqueter le mauvais passage.
      js_txt <- if (is.null(ev$text)) "" else as.character(ev$text)[1]
      if (nzchar(js_txt) && !identical(txt, js_txt)) {
        pos <- hstat_code_locate_quote(d$text, js_txt)
        if (!is.null(pos)) {
          st <- pos[["start"]]; en <- pos[["end"]]
          txt <- substr(d$text, st + 1, en)
        }
      }
      before <- nrow(rv$segments)
      rv$segments <- hstat_seg_add(rv$segments, d$doc_id, ev$code, st, en, txt)
      rv$sel <- NULL
      if (nrow(rv$segments) == before)
        shiny::showNotification("Ce passage porte deja ce code.",
                                type = "warning", duration = 3)
      else
        shiny::showNotification(
          sprintf("« %s » applique a : %s",
                  hstat_code_label(rv$codebook, ev$code),
                  substr(txt, 1, 60)), type = "message", duration = 3)
    })

    # ---------------------------------------------- etiquettes du document
    output$doc_segments <- shiny::renderUI({
      d <- cur_doc()
      s <- hstat_seg_for_doc(rv$segments, d$doc_id)
      if (nrow(s) == 0)
        return(shiny::div(style = "color:#7f8c8d;font-size:13px;font-style:italic;",
                          "Aucune etiquette sur cette reponse."))
      s <- s[order(s$start), , drop = FALSE]
      shiny::tagList(lapply(seq_len(nrow(s)), function(i) {
        col <- hstat_code_color(rv$codebook, s$code_id[i])
        shiny::div(style = sprintf(
            "border-left:4px solid %s;background:%s;padding:6px 8px;margin-bottom:6px;border-radius:4px;font-size:12px;",
            col, .hstat_code_rgba(col, 0.14)),
          shiny::tags$b(hstat_code_label(rv$codebook, s$code_id[i])),
          if (identical(s$source[i], "IA"))
            shiny::tags$span(style = "background:#8e44ad;color:white;border-radius:8px;padding:0 6px;margin-left:4px;font-size:10px;", "IA"),
          shiny::br(),
          shiny::tags$i(paste0("« ", substr(s$text[i], 1, 120), " »")),
          shiny::br(),
          shiny::tags$span(class = "hstat-seg-del", `data-seg` = s$seg_id[i],
                           shiny::icon("xmark"), " Retirer"))
      }))
    })

    # Un seul observateur pour tous les liens « Retirer » : le clic remonte via
    # la delegation JavaScript avec l'identifiant du segment concerne.
    shiny::observeEvent(input$rmseg_event, {
      sid <- input$rmseg_event$seg
      shiny::req(!is.null(sid))
      if (!(sid %in% rv$segments$seg_id)) return()
      lab <- hstat_code_label(rv$codebook, rv$segments$code_id[rv$segments$seg_id == sid])
      rv$segments <- hstat_seg_remove(rv$segments, sid)
      shiny::showNotification(sprintf("Etiquette « %s » retiree.", lab),
                              type = "message", duration = 3)
    })

    output$progress_box <- shiny::renderUI({
      dd <- docs()
      n_doc <- nrow(dd)
      n_coded <- length(unique(rv$segments$doc_id))
      pct <- if (n_doc > 0) round(100 * n_coded / n_doc) else 0
      shiny::div(style = "font-size:12px;",
        shiny::tags$b(shiny::icon("chart-simple"), " Avancement du codage"),
        shiny::div(style = "background:#ecf0f1;border-radius:8px;height:16px;margin:6px 0;overflow:hidden;",
          shiny::div(style = sprintf("background:#27ae60;height:100%%;width:%d%%;", pct))),
        sprintf("%d / %d reponses codees (%d%%)", n_coded, n_doc, pct),
        shiny::br(),
        sprintf("%d etiquette(s), %d code(s)", nrow(rv$segments), nrow(rv$codebook)))
    })

    # ==================================================== RECUPERATION
    # ================================================================
    # REQUETE COMBINEE, CONCORDANCIER, PORTRAIT, ACCORD INTER-CODEURS
    # ================================================================
    output$qry_a_ui <- shiny::renderUI({
      cb <- rv$codebook
      shiny::selectInput(ns("qry_a"), "Ensemble A",
        choices = stats::setNames(cb$code_id, cb$label),
        selected = cb$code_id[1], multiple = TRUE, width = "100%")
    })
    output$qry_b_ui <- shiny::renderUI({
      cb <- rv$codebook
      shiny::selectInput(ns("qry_b"), "Ensemble B",
        choices = stats::setNames(cb$code_id, cb$label),
        selected = cb$code_id[min(2L, nrow(cb))], multiple = TRUE, width = "100%")
    })

    qry_res <- shiny::reactive({
      hstat_code_query(rv$segments, rv$codebook, docs(),
                       codes_a = input$qry_a, codes_b = input$qry_b,
                       operateur = input$qry_op %||% "et",
                       portee = input$qry_portee %||% "document",
                       distance = hstat_finite(input$qry_dist, 200))
    })

    qry_df <- shiny::reactive({
      s <- qry_res()
      if (!nrow(s)) return(hstat_code_retrieve(NULL, rv$codebook, docs()))
      hstat_code_retrieve(s, rv$codebook, docs(), code_ids = unique(s$code_id))
    })

    output$qry_note <- shiny::renderUI({
      s <- qry_res()
      op <- input$qry_op %||% "et"
      # La portee change le SENS de la reponse : la taire laisserait croire
      # qu'il n'y a qu'une facon de lire « A et B ».
      quoi <- switch(attr(s, "portee") %||% "document",
        document  = "presents dans la meme reponse",
        overlap   = "etiquetant le meme passage",
        proximite = sprintf("distants de moins de %s caracteres",
                            hstat_finite(input$qry_dist, 200)),
        "")
      lib <- switch(op, et = "A ET B", sauf = "A SAUF B", ou = "A OU B")
      shiny::div(class = "callout callout-info", style = "padding:8px 12px;",
        shiny::icon("code-branch"),
        sprintf(" %s : %d extrait(s)", lib, nrow(s)),
        if (op != "ou" && nzchar(quoi)) sprintf(" %s.", quoi) else ".")
    })

    output$qry_table <- DT::renderDT({
      df <- qry_df(); df$.seg_id <- NULL
      DT::datatable(df, rownames = FALSE, filter = "top",
                    options = list(pageLength = 15, scrollX = TRUE,
                                   language = list(url = NULL)),
                    escape = TRUE)
    })

    output$dl_qry_csv <- shiny::downloadHandler(
      filename = function() sprintf("hstat_requete_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- qry_df(); df$.seg_id <- NULL
        utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      })

    # ------------------------------------------------------ concordancier
    kwic_df <- shiny::reactive({
      hstat_code_kwic(docs(), input$kwic_motif,
                      fenetre = hstat_finite(input$kwic_fen, 45),
                      regex = isTRUE(input$kwic_regex),
                      casse = isTRUE(input$kwic_casse))
    })

    output$kwic_note <- shiny::renderUI({
      m <- trimws(input$kwic_motif %||% "")
      if (!nzchar(m))
        return(shiny::div(class = "callout callout-info", style = "padding:8px 12px;",
          shiny::icon("magnifying-glass"),
          " Entrez un mot pour voir tous ses emplois dans leur contexte.",
          " C'est l'outil qui precede le codage : on voit COMMENT un mot est",
          " employe avant de decider quel code lui donner."))
      k <- kwic_df()
      if (!nrow(k))
        return(shiny::div(class = "callout callout-warning", style = "padding:8px 12px;",
          shiny::icon("circle-exclamation"),
          trf(" Aucune occurrence de « %s ».", m),
          if (isTRUE(input$kwic_regex))
            " Verifiez l'expression reguliere : une expression invalide ne rend aucune ligne." else ""))
      shiny::div(class = "callout callout-info", style = "padding:8px 12px;",
        shiny::icon("magnifying-glass"),
        sprintf(" %d occurrence(s).", nrow(k)),
        if (isTRUE(attr(k, "tronque")))
          " Affichage limite aux premieres trouvees." else "")
    })

    output$kwic_table <- DT::renderDT({
      k <- kwic_df()
      DT::datatable(k, rownames = FALSE,
                    options = list(pageLength = 20, scrollX = TRUE,
                                   language = list(url = NULL),
                                   columnDefs = list(
                                     list(className = "dt-right", targets = 1),
                                     list(className = "dt-center", targets = 2))),
                    escape = TRUE)
    })

    output$dl_kwic_csv <- shiny::downloadHandler(
      filename = function() sprintf("hstat_concordancier_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file)
        utils::write.csv(kwic_df(), file, row.names = FALSE, fileEncoding = "UTF-8"))

    # -------------------------------------------------- portrait du document
    output$cl_doc_ui <- shiny::renderUI({
      dd <- docs()
      s <- rv$segments
      # On ne propose que les documents REELLEMENT codes : offrir les autres
      # menerait a un graphique vide sans que l'utilisateur sache pourquoi.
      ids <- if (nrow(s)) intersect(dd$doc_id, unique(s$doc_id)) else character(0)
      if (!length(ids))
        return(shiny::div(class = "callout callout-warning", style = "padding:8px 12px;",
          shiny::icon("info-circle"),
          " Aucun document code pour l'instant."))
      lab <- paste("Ligne", dd$row[match(ids, dd$doc_id)])
      shiny::selectInput(ns("cl_doc"), "Document", choices = stats::setNames(ids, lab),
                         selected = shiny::isolate(input$cl_doc) %||% ids[1])
    })

    cl_df <- shiny::reactive({
      shiny::req(input$cl_doc)
      hstat_code_codeline(rv$segments, rv$codebook, docs(), input$cl_doc)
    })

    output$cl_note <- shiny::renderUI({
      cl <- tryCatch(cl_df(), error = function(e) NULL)
      if (is.null(cl) || !nrow(cl)) return(NULL)
      shiny::div(class = "callout callout-info", style = "padding:8px 12px;",
        shiny::icon("chart-gantt"),
        sprintf(" %d etiquette(s) sur %s caracteres. ", nrow(cl),
                attr(cl, "n_char") %||% 0L),
        "La position est en pourcentage du document : deux reponses de longueurs",
        " differentes restent comparables.")
    })

    output$cl_plot <- shiny::renderPlot({
      cl <- cl_df()
      dd <- docs()
      r <- dd$row[match(input$cl_doc, dd$doc_id)]
      hstat_code_codeline_plot(cl, titre = if (!is.na(r)) paste("Ligne", r) else "")
    })

    output$cl_table <- DT::renderDT({
      cl <- cl_df()
      DT::datatable(cl[, c("Code", "debut", "fin", "Extrait"), drop = FALSE],
                    rownames = FALSE, colnames = c("Code", "Debut", "Fin", "Extrait"),
                    options = list(pageLength = 10, scrollX = TRUE,
                                   language = list(url = NULL)),
                    escape = TRUE)
    })

    # ---------------------------------------------------- accord inter-codeurs
    codeurs <- shiny::reactive({
      s <- rv$segments
      if (!nrow(s)) return(character(0))
      sort(unique(s$source[!is.na(s$source) & nzchar(s$source)]))
    })

    output$acc_a_ui <- shiny::renderUI({
      cs <- codeurs()
      if (length(cs) < 2)
        return(shiny::div(class = "callout callout-warning", style = "padding:8px 12px;",
          shiny::icon("info-circle"),
          " L'accord se mesure entre DEUX codeurs. Un seul jeu de codages est",
          " present pour l'instant (origine : ",
          paste(cs, collapse = ", "), "). Importez le codage d'une autre",
          " personne, ou comparez un codage manuel a un codage automatique."))
      shiny::selectInput(ns("acc_a"), "Codeur A", choices = cs, selected = cs[1])
    })
    output$acc_b_ui <- shiny::renderUI({
      cs <- codeurs()
      if (length(cs) < 2) return(NULL)
      shiny::selectInput(ns("acc_b"), "Codeur B", choices = cs, selected = cs[2])
    })

    acc_res <- shiny::reactive({
      shiny::req(input$acc_a, input$acc_b)
      hstat_code_accord(rv$segments, rv$codebook, input$acc_a, input$acc_b)
    })

    output$acc_resume <- shiny::renderUI({
      if (length(codeurs()) < 2) return(NULL)
      a <- acc_res()
      # Kappa n'est pas toujours defini ; on traite le quatrieme etat
      # explicitement plutot que d'afficher NaN ou de brancher dessus.
      verdict <- switch(a$verdict,
        excellent  = "accord excellent",
        acceptable = "accord acceptable",
        faible     = "accord faible : relisez ensemble le livre de codes",
        "verdict indeterminable")
      classe <- switch(a$verdict, excellent = "callout callout-success",
                       acceptable = "callout callout-info",
                       faible = "callout callout-warning", "callout callout-warning")
      shiny::div(class = classe, style = "padding:10px 14px;",
        shiny::tags$b(sprintf("Accord observe : %s %%",
                              if (is.finite(a$accord)) round(a$accord * 100, 1) else "-")),
        shiny::br(),
        if (is.finite(a$kappa))
          shiny::span(sprintf("Kappa de Cohen : %.3f - %s.", a$kappa, verdict))
        else shiny::span(a$message),
        shiny::br(),
        shiny::tags$small(style = "color:#7f8c8d;",
          if (is.finite(a$kappa)) a$message else "",
          " L'unite comparee est le couple document x code : deux codeurs ne",
          " decoupent jamais aux memes bornes, comparer des segments exigerait",
          " un seuil de recouvrement arbitraire."))
    })

    output$acc_table <- shiny::renderTable({
      if (length(codeurs()) < 2) return(NULL)
      a <- acc_res()
      if (is.null(a$table)) return(NULL)
      m <- as.data.frame.matrix(a$table)
      names(m) <- paste0(a$b, " : ", c("non", "oui"))
      cbind(data.frame(` ` = paste0(a$a, " : ", c("non", "oui")),
                       check.names = FALSE), m)
    }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "auto")

    output$ret_codes_ui <- shiny::renderUI({
      cb <- rv$codebook
      shiny::selectInput(ns("ret_codes"), "Codes a afficher",
        choices = stats::setNames(cb$code_id, cb$label),
        selected = cb$code_id, multiple = TRUE, width = "100%")
    })
    output$ret_var_ui <- shiny::renderUI({
      shiny::selectInput(ns("ret_var"), "Croiser avec le profil",
        choices = c("(aucun croisement)" = "", profile_vars()), selected = "")
    })
    output$ret_lvl_ui <- shiny::renderUI({
      v <- input$ret_var
      if (is.null(v) || !nzchar(v)) return(NULL)
      p <- profile()
      lv <- sort(unique(p[[v]][!is.na(p[[v]]) & nzchar(p[[v]])]))
      shiny::selectInput(ns("ret_lvl"), sprintf("Modalites de %s", v),
                         choices = lv, selected = lv, multiple = TRUE)
    })

    ret_df <- shiny::reactive({
      hstat_code_retrieve(rv$segments, rv$codebook, docs(),
                          code_ids = input$ret_codes, profile = profile(),
                          filter_var = input$ret_var, filter_levels = input$ret_lvl,
                          search = input$ret_search)
    })

    output$ret_note <- shiny::renderUI({
      n <- nrow(ret_df())
      if (nrow(rv$segments) == 0)
        return(shiny::div(class = "callout callout-warning",
          shiny::icon("info-circle"),
          " Aucun codage pour l'instant : commencez par etiqueter des passages ci-dessus."))
      shiny::div(class = "callout callout-info", style = "padding:8px 12px;",
        shiny::icon("quote-left"),
        trf(" %d extrait(s) correspondant a la selection, sur %d etiquette(s) au total.",
                n, nrow(rv$segments)))
    })

    output$ret_table <- DT::renderDT({
      df <- ret_df()
      df$.seg_id <- NULL
      DT::datatable(df, rownames = FALSE, filter = "top",
                    options = list(pageLength = 15, scrollX = TRUE,
                                   language = list(url = NULL)),
                    escape = TRUE)
    })

    output$dl_ret_csv <- shiny::downloadHandler(
      filename = function() sprintf("hstat_extraits_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- ret_df(); df$.seg_id <- NULL
        utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      })
    output$dl_ret_xlsx <- shiny::downloadHandler(
      filename = function() sprintf("hstat_extraits_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- ret_df(); df$.seg_id <- NULL
        # .write_xlsx() (mod_qualitative.R) assainit les noms d'onglets et
        # sait se replier sur writexl ou le CSV si openxlsx manque.
        .write_xlsx(list("Extraits codes" = df), file)
      })

    # ==================================================== MATRICE
    output$mat_var_ui <- shiny::renderUI({
      shiny::selectInput(ns("mat_var"), "Variable de profil (colonnes)",
        choices = c("(effectif total)" = "", profile_vars()), selected = "")
    })

    mat_raw <- shiny::reactive({
      hstat_code_matrix(rv$segments, rv$codebook, profile(), input$mat_var,
                        count = input$mat_count %||% "segments")
    })

    # Pourcentages : le total en pied de tableau est exclu du recalcul, il est
    # recalcule a la fin pour rester coherent avec l'affichage choisi.
    mat_df <- shiny::reactive({
      m <- mat_raw()
      if (is.null(m)) return(NULL)
      disp <- input$mat_disp %||% "n"
      if (identical(disp, "n") || ncol(m) < 3) return(m)
      num <- setdiff(names(m), c("Code", "Total"))
      x <- as.matrix(m[, num, drop = FALSE])
      out <- m
      if (identical(disp, "col")) {
        cs <- colSums(x); cs[cs == 0] <- 1
        out[, num] <- round(100 * sweep(x, 2, cs, "/"), 1)
        out$Total <- round(100 * rowSums(x) / max(1, sum(x)), 1)
      } else {
        rs <- rowSums(x); rs[rs == 0] <- 1
        out[, num] <- round(100 * sweep(x, 1, rs, "/"), 1)
        out$Total <- 100
      }
      out
    })

    output$mat_table <- DT::renderDT({
      m <- mat_df()
      shiny::validate(shiny::need(!is.null(m) && nrow(m) > 0,
        "Creez des codes et etiquetez des passages pour obtenir une matrice."))
      DT::datatable(m, rownames = FALSE,
                    options = list(pageLength = 20, scrollX = TRUE, dom = "tip"))
    })

    output$mat_plot <- shiny::renderPlot({
      m <- mat_raw()
      shiny::validate(shiny::need(!is.null(m) && nrow(m) > 0 && ncol(m) >= 2,
        "Aucune donnee a representer."))
      num <- setdiff(names(m), c("Code", "Total"))
      shiny::validate(shiny::need(length(num) >= 1, "Aucune donnee a representer."))
      long <- do.call(rbind, lapply(num, function(v)
        data.frame(Code = m$Code, Modalite = v, n = m[[v]], stringsAsFactors = FALSE)))
      long$Code <- factor(long$Code, levels = rev(m$Code[order(m$Total)]))
      ggplot2::ggplot(long, ggplot2::aes(x = .data$Modalite, y = .data$Code,
                                         fill = .data$n)) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(.data$n > 0, .data$n, "")),
                           size = 3.6, colour = "#2c3e50") +
        ggplot2::scale_fill_gradient(low = "#fdf2e9", high = "#e67e22", name = "Effectif") +
        ggplot2::labs(title = "Matrice de croisement codes x profil",
                      x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 15),
          axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
          panel.grid = ggplot2::element_blank())
    })

    output$dl_mat_csv <- shiny::downloadHandler(
      filename = function() sprintf("hstat_matrice_codes_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) utils::write.csv(mat_df(), file, row.names = FALSE,
                                                fileEncoding = "UTF-8"))
    output$dl_mat_xlsx <- shiny::downloadHandler(
      filename = function() sprintf("hstat_matrice_codes_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
      content = function(file) .write_xlsx(list("Matrice de croisement" = mat_df()), file))

    # ==================================================== NUAGE DE MOTS
    output$cloud_code_ui <- shiny::renderUI({
      cb <- rv$codebook
      if (nrow(cb) == 0) return(NULL)
      shiny::selectInput(ns("cloud_codes"), "Codes retenus (corpus code)",
        choices = stats::setNames(cb$code_id, cb$label),
        selected = cb$code_id, multiple = TRUE)
    })

    cloud_texts <- shiny::reactive({
      if (identical(input$cloud_src, "coded")) {
        s <- rv$segments
        if (!is.null(input$cloud_codes) && length(input$cloud_codes))
          s <- s[s$code_id %in% input$cloud_codes, , drop = FALSE]
        s$text
      } else docs()$text
    })

    cloud_layout <- shiny::reactive({
      tx <- cloud_texts()
      shiny::validate(shiny::need(length(tx) > 0, "Aucun texte a representer."))
      extra <- input$cloud_stop
      extra <- if (is.null(extra) || !nzchar(trimws(extra))) NULL
               else trimws(strsplit(extra, "[,;[:space:]]+")[[1]])
      toks <- hstat_q_tokenize(tx, min_char = input$cloud_minchar %||% 4,
                               stem = isTRUE(input$cloud_stem),
                               remove_numbers = TRUE, extra_stopwords = extra)
      w <- unlist(toks)
      shiny::validate(shiny::need(length(w) > 0,
        "Aucun mot exploitable apres nettoyage (assouplissez la longueur minimale)."))
      tb <- sort(table(w), decreasing = TRUE)
      # La taille minimale suit la taille maximale : a 3 pt fixes, les mots les
      # plus rares devenaient des points illisibles des que l'echelle du
      # graphique se reduisait.
      mx <- input$cloud_size %||% 16
      hstat_code_cloud_layout(names(tb), as.integer(tb),
                              max_words = input$cloud_max %||% 80,
                              min_size = max(3, mx * 0.3), max_size = mx)
    })

    cloud_gg <- shiny::reactive({
      lay <- cloud_layout()
      shiny::validate(shiny::need(!is.null(lay) && nrow(lay) > 0,
        "Nuage vide : elargissez le corpus ou reduisez la taille du texte."))
      hstat_code_cloud_plot(lay)
    })

    output$cloud_plot <- shiny::renderPlot(cloud_gg())
    output$dl_cloud <- shiny::downloadHandler(
      filename = function() sprintf("hstat_nuage_mots_%s.png", format(Sys.Date(), "%Y%m%d")),
      # `validate(need(...))` dans un downloadHandler interrompt le contenu :
      # Shiny renvoie sa page d'erreur HTML, enregistree en « .png ». On ecrit
      # toujours une image valide, portant le motif s'il y en a un.
      content = function(file)
        hstat_ecrire_image(file, tryCatch(cloud_gg(), error = function(e) NULL),
                           "png", 10, 7, 300,
                           echec = "Nuage indisponible : élargissez le corpus ou réduisez la taille du texte."))

    # ==================================================== CARTE CONCEPTUELLE
    cooc <- shiny::reactive({
      hstat_code_cooccurrence(rv$segments, rv$codebook,
                              mode = input$map_mode %||% "document")
    })

    map_gg <- shiny::reactive({
      m <- cooc()
      shiny::validate(shiny::need(!is.null(m) && nrow(m) >= 2,
        "Il faut au moins deux codes pour construire une carte conceptuelle."))
      p <- hstat_code_map_plot(m, rv$codebook,
                               hstat_code_counts(rv$codebook, rv$segments),
                               min_weight = input$map_min %||% 1,
                               label_size = hstat_lbl_pt2gg(input$map_lbl, 12),
                               mode_label = input$map_mode %||% "document")
      shiny::validate(shiny::need(!is.null(p), "Carte non calculable."))
      p
    })

    output$map_plot <- shiny::renderPlot(map_gg())
    output$cooc_table <- DT::renderDT({
      m <- cooc()
      shiny::validate(shiny::need(!is.null(m) && nrow(m) >= 2,
        "Il faut au moins deux codes."))
      DT::datatable(data.frame(Code = rownames(m), m, check.names = FALSE),
                    rownames = FALSE, options = list(dom = "tip", scrollX = TRUE))
    })
    output$dl_map <- shiny::downloadHandler(
      filename = function() sprintf("hstat_carte_conceptuelle_%s.png", format(Sys.Date(), "%Y%m%d")),
      content = function(file)
        hstat_ecrire_image(file, tryCatch(map_gg(), error = function(e) NULL),
                           "png", 10, 8, 300,
                           echec = "Carte indisponible : il faut au moins deux codes co-occurrents."))
    output$dl_cooc_xlsx <- shiny::downloadHandler(
      filename = function() sprintf("hstat_cooccurrences_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        m <- cooc()
        .write_xlsx(list("Cooccurrences" =
                           data.frame(Code = rownames(m), m, check.names = FALSE)), file)
      })

    # ================================= ASSISTANT DE CODAGE (3 moteurs)
    # Les reglages du moteur, rassembles une fois pour toutes : les quatre
    # observateurs ci-dessous en dependent tous.
    ai_opts <- shiny::reactive(list(
      engine  = input$ai_engine %||% "local",
      backend = input$ai_backend %||% "ollama",
      url     = input$ai_url,
      model   = input$ai_model,
      key     = input$ai_key))

    # Changer de serveur d'inference remet l'adresse par defaut correspondante :
    # 11434 pour Ollama, 8080 pour un serveur compatible OpenAI.
    shiny::observeEvent(input$ai_backend, {
      shiny::updateTextInput(session, "ai_url",
                             value = unname(HSTAT_AI_DEFAULT_URL[[input$ai_backend]]))
    }, ignoreInit = TRUE)

    ai_models <- shiny::reactiveVal(character(0))
    shiny::observeEvent(input$ai_ping, {
      o <- ai_opts()
      st <- hstat_ai_status("local", o$backend, o$url)
      ai_models(if (is.null(st$models)) character(0) else st$models)
      shiny::showNotification(st$message,
                              type = if (isTRUE(st$ok)) "message" else "warning",
                              duration = 10)
    })

    output$ai_model_ui <- shiny::renderUI({
      m <- ai_models()
      if (!length(m))
        return(shiny::textInput(ns("ai_model"), "Modele",
                                placeholder = "ex. qwen2.5 - « Tester la connexion » liste les modeles installes"))
      shiny::selectInput(ns("ai_model"), "Modele", choices = m,
                         selected = shiny::isolate(input$ai_model) %||% m[1])
    })

    output$ai_status <- shiny::renderUI({
      o <- ai_opts()
      st <- hstat_ai_status(o$engine, o$backend, o$url, o$model, o$key)
      shiny::div(class = if (isTRUE(st$ok)) "callout callout-success" else "callout callout-warning",
        style = "padding:8px 12px;",
        shiny::icon(if (isTRUE(st$ok)) "circle-check" else "triangle-exclamation"),
        " ", st$message,
        if (!isTRUE(st$ok) && !identical(o$engine, "auto"))
          shiny::tags$small(style = "display:block;color:#7f8c8d;margin-top:4px;",
            "En attendant, le moteur « Thematisation automatique » fonctionne sans ",
            "rien installer, et tout le reste de l'atelier de codage est utilisable."))
    })

    # ---- Etape 1 : proposer un livre de codes ----
    shiny::observeEvent(input$ai_codebook, {
      o <- ai_opts()
      st <- hstat_ai_status(o$engine, o$backend, o$url, o$model, o$key)
      if (!isTRUE(st$ok)) { shiny::showNotification(st$message, type = "error", duration = 10); return() }
      tx <- docs()$text
      if (!length(tx)) { shiny::showNotification("Corpus vide.", type = "error"); return() }

      if (identical(o$engine, "auto")) {
        shiny::withProgress(message = "Thematisation du corpus...", value = 0.5, {
          cb <- hstat_code_auto_codebook(tx, n_codes = input$ai_ncodes %||% 8,
                                         min_char = input$ai_minchar %||% 4)
          if (is.null(cb)) {
            rv$ai <- list(ok = FALSE,
                          msg = paste0("Corpus trop court ou trop homogene pour en degager ",
                                       "des themes. Reduisez la longueur minimale des mots, ",
                                       "ou creez les codes a la main."))
            shiny::showNotification(rv$ai$msg, type = "warning", duration = 8); return()
          }
          .push_codebook(cb, "Thematisation automatique")
        })
        return()
      }

      n_max <- max(5L, as.integer(input$ai_maxdoc %||% 60))
      if (length(tx) > n_max) tx <- tx[round(seq(1, length(tx), length.out = n_max))]
      shiny::withProgress(message = "Le modele analyse le corpus...", value = 0.4, {
        res <- hstat_ai_call(
          hstat_ai_codebook_prompt(tx, input$ai_ncodes %||% 8, input$ai_context %||% ""),
          system = "Tu es un analyste qualitatif rigoureux. Tu reponds exclusivement en JSON valide.",
          engine = o$engine, backend = o$backend, url = o$url, model = o$model,
          api_key = o$key)
        shiny::incProgress(0.4)
        if (!isTRUE(res$ok)) {
          rv$ai <- list(ok = FALSE, msg = res$error)
          shiny::showNotification(res$error, type = "error", duration = 12); return()
        }
        cb <- hstat_ai_parse_codebook(hstat_ai_extract_json(res$text))
        if (is.null(cb)) {
          rv$ai <- list(ok = FALSE,
                        msg = paste0("Reponse du modele non exploitable. Un modele local ",
                                     "trop petit peut ne pas tenir le format JSON : ",
                                     "essayez un modele plus grand, ou le moteur ",
                                     "« Thematisation automatique »."),
                        raw = substr(res$text, 1, 1500))
          shiny::showNotification(rv$ai$msg, type = "error", duration = 10); return()
        }
        .push_codebook(cb, if (identical(o$engine, "claude")) "API Claude"
                           else sprintf("Modele local%s",
                                        if (!is.null(res$model)) paste0(" (", res$model, ")") else ""))
      })
    })

    # Ajout des codes proposes, quel que soit le moteur : meme contrat
    # data.frame(label, memo, keywords).
    .push_codebook <- function(cb, source_label) {
      added <- 0L
      for (i in seq_len(nrow(cb))) {
        before <- nrow(rv$codebook)
        rv$codebook <- hstat_code_add(rv$codebook, cb$label[i], memo = cb$memo[i],
                                      keywords = if ("keywords" %in% names(cb)) cb$keywords[i] else "")
        if (nrow(rv$codebook) > before) added <- added + 1L
      }
      rv$next_color <- hstat_code_next_color(rv$codebook)
      colourpicker::updateColourInput(session, "new_color", value = rv$next_color)
      rv$ai <- list(ok = TRUE,
                    msg = sprintf("%s : %d code(s) ajoute(s) sur %d proposition(s).",
                                  source_label, added, nrow(cb)),
                    table = cb)
      shiny::showNotification(rv$ai$msg, type = "message", duration = 7)
    }

    # ---- Etape 2 : pre-coder les reponses ----
    shiny::observeEvent(input$ai_autocode, {
      o <- ai_opts()
      st <- hstat_ai_status(o$engine, o$backend, o$url, o$model, o$key)
      if (!isTRUE(st$ok)) { shiny::showNotification(st$message, type = "error", duration = 10); return() }
      if (nrow(rv$codebook) == 0) {
        shiny::showNotification("Creez d'abord un livre de codes (etape 1, ou a la main).",
                                type = "warning", duration = 6); return()
      }
      dd <- docs()

      if (identical(o$engine, "auto")) {
        # Sans modele, le pre-codage est celui du dictionnaire : il porte sur
        # TOUT le corpus, pas sur un echantillon.
        .apply_dictionary(dd, "Thematisation automatique")
        return()
      }

      n_max <- max(5L, as.integer(input$ai_maxdoc %||% 60))
      sub <- if (nrow(dd) > n_max) dd[round(seq(1, nrow(dd), length.out = n_max)), , drop = FALSE] else dd
      shiny::withProgress(message = "Le modele pre-code les reponses...", value = 0.4, {
        res <- hstat_ai_call(
          hstat_ai_autocode_prompt(sub, rv$codebook),
          system = "Tu es un analyste qualitatif rigoureux. Tu reponds exclusivement en JSON valide et tu cites les extraits mot pour mot.",
          engine = o$engine, backend = o$backend, url = o$url, model = o$model,
          api_key = o$key)
        shiny::incProgress(0.4)
        if (!isTRUE(res$ok)) {
          rv$ai <- list(ok = FALSE, msg = res$error)
          shiny::showNotification(res$error, type = "error", duration = 12); return()
        }
        segs <- hstat_ai_parse_autocode(hstat_ai_extract_json(res$text), sub, rv$codebook)
        if (is.null(segs) || nrow(segs) == 0) {
          rv$ai <- list(ok = FALSE,
                        msg = paste0("Aucun codage exploitable n'a pu etre localise dans le ",
                                     "texte. Les extraits cites par le modele doivent etre ",
                                     "mot pour mot ; un modele local trop petit les reformule ",
                                     "souvent. Le pre-codage par dictionnaire, lui, ne peut ",
                                     "pas se tromper de passage."),
                        raw = substr(res$text, 1, 1500))
          shiny::showNotification(rv$ai$msg, type = "warning", duration = 10); return()
        }
        added <- 0L
        for (i in seq_len(nrow(segs))) {
          before <- nrow(rv$segments)
          rv$segments <- hstat_seg_add(rv$segments, segs$doc_id[i], segs$code_id[i],
                                       segs$start[i], segs$end[i], segs$text[i],
                                       source = "IA")
          if (nrow(rv$segments) > before) added <- added + 1L
        }
        miss <- attr(segs, "non_localises")
        rv$ai <- list(ok = TRUE,
                      msg = sprintf(
                        "%d etiquette(s) ajoutee(s) sur %d reponse(s) analysee(s).%s",
                        added, nrow(sub),
                        if (!is.null(miss) && miss > 0)
                          trf(" %d proposition(s) ecartee(s) : extrait introuvable dans le texte.", miss)
                        else ""),
                      table = data.frame(
                        Reponse = segs$doc_id,
                        Code = hstat_code_label(rv$codebook, segs$code_id),
                        Extrait = segs$text, stringsAsFactors = FALSE))
        shiny::showNotification(rv$ai$msg, type = "message", duration = 8)
      })
    })

    shiny::observeEvent(input$ai_drop, {
      auto <- rv$segments$source %in% c("IA", "auto")
      n <- sum(auto)
      rv$segments <- rv$segments[!auto, , drop = FALSE]
      shiny::showNotification(sprintf("%d etiquette(s) posee(s) automatiquement supprimee(s).", n),
                              type = "message", duration = 5)
    })

    output$ai_result <- shiny::renderUI({
      a <- rv$ai
      if (is.null(a)) return(NULL)
      box <- shiny::div(
        class = if (isTRUE(a$ok)) "callout callout-success" else "callout callout-danger",
        style = "padding:10px 14px;",
        shiny::icon(if (isTRUE(a$ok)) "circle-check" else "circle-xmark"), " ", a$msg,
        if (!is.null(a$raw))
          shiny::tags$pre(style = "margin-top:8px;max-height:180px;overflow:auto;font-size:11px;",
                          a$raw))
      box
    })

    # La table des propositions vit dans sa PROPRE sortie. Un DT::renderDT()
    # appele a la main depuis un renderUI echoue (« argument "name" is
    # missing ») : une fonction de rendu attend la session et le nom de sortie
    # que Shiny lui passe, elle ne s'invoque pas directement.
    output$ai_table <- DT::renderDT({
      a <- rv$ai
      shiny::validate(shiny::need(!is.null(a) && !is.null(a$table),
                                  "Aucune proposition pour l'instant."))
      tb <- a$table
      # Les colonnes techniques du contrat interne deviennent des intitules
      # lisibles a l'ecran.
      names(tb) <- vapply(names(tb), function(n) switch(
        n, label = "Code", memo = "Definition", keywords = "Mots-cles", n),
        character(1))
      DT::datatable(tb, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
    })

    # ==================================================== DICTIONNAIRE
    output$dict_code_ui <- shiny::renderUI({
      cb <- rv$codebook
      shiny::selectInput(ns("dict_code"), "Code",
        choices = if (nrow(cb)) stats::setNames(cb$code_id, cb$label) else character(0),
        selected = shiny::isolate(input$dict_code))
    })

    # Le champ suit le code selectionne, sans ecraser une saisie en cours.
    shiny::observeEvent(input$dict_code, {
      shiny::updateTextAreaInput(session, "dict_words",
        value = paste(hstat_code_keywords_of(rv$codebook, input$dict_code), collapse = "; "))
    })

    shiny::observeEvent(input$dict_save, {
      shiny::req(input$dict_code)
      kw <- trimws(input$dict_words %||% "")
      kw <- paste(unique(trimws(strsplit(kw, "[;,\n]")[[1]])), collapse = "; ")
      rv$codebook <- hstat_code_update(rv$codebook, input$dict_code, keywords = kw)
      shiny::showNotification(
        sprintf("Mots-cles enregistres pour « %s ».",
                hstat_code_label(rv$codebook, input$dict_code)),
        type = "message", duration = 4)
    })

    .apply_dictionary <- function(dd, source_label) {
      if (!any(nzchar(trimws(rv$codebook$keywords)))) {
        rv$ai <- list(ok = FALSE,
                      msg = paste0("Aucun code ne porte de mots-cles. Renseignez-en ",
                                   "ci-dessous, ou lancez d'abord la thematisation ",
                                   "automatique, qui en propose."))
        shiny::showNotification(rv$ai$msg, type = "warning", duration = 8)
        return(invisible(NULL))
      }
      shiny::withProgress(message = "Application du dictionnaire...", value = 0.5, {
        segs <- hstat_code_lexical_apply(dd, rv$codebook,
                                         scope = input$dict_scope %||% "phrase")
        added <- 0L
        for (i in seq_len(nrow(segs))) {
          before <- nrow(rv$segments)
          rv$segments <- hstat_seg_add(rv$segments, segs$doc_id[i], segs$code_id[i],
                                       segs$start[i], segs$end[i], segs$text[i],
                                       source = "auto")
          if (nrow(rv$segments) > before) added <- added + 1L
        }
        sans <- attr(segs, "codes_sans_mots_cles")
        rv$ai <- list(
          ok = TRUE,
          msg = trf("%s : %d etiquette(s) posee(s) sur %d reponse(s) analysee(s) (tout le corpus).%s",
                        source_label, added, nrow(dd),
                        if (!is.null(sans) && sans > 0)
                          sprintf(" %d code(s) sans mots-cles ont ete ignore(s).", sans) else ""),
          table = if (nrow(segs)) utils::head(data.frame(
            Reponse = segs$doc_id,
            Code = hstat_code_label(rv$codebook, segs$code_id),
            Extrait = segs$text, stringsAsFactors = FALSE), 200) else NULL)
        shiny::showNotification(rv$ai$msg, type = "message", duration = 8)
      })
    }

    shiny::observeEvent(input$dict_apply, .apply_dictionary(docs(), "Dictionnaire"))

    output$dict_table <- DT::renderDT({
      cb <- rv$codebook
      shiny::validate(shiny::need(nrow(cb) > 0,
        "Aucun code : creez-en a la main, ou faites-en proposer par l'assistant."))
      cnt <- hstat_code_counts(cb, rv$segments)
      DT::datatable(
        data.frame(Code = cb$label,
                   `Mots-cles` = ifelse(nzchar(trimws(cb$keywords)), cb$keywords, "-"),
                   Segments = cnt$n_seg,
                   Definition = ifelse(nzchar(cb$memo), cb$memo, "-"),
                   check.names = FALSE, stringsAsFactors = FALSE),
        rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
    })

    # ==================================================== EXPORT / PROJET
    output$dl_report <- shiny::downloadHandler(
      filename = function() sprintf("hstat_rapport_codage_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        sheets <- list(
          "Livre de codes" = if (nrow(rv$codebook))
            hstat_code_counts(rv$codebook, rv$segments) else
            data.frame(Note = "Livre de codes vide"),
          "Segments codes" = {
            r <- hstat_code_retrieve(rv$segments, rv$codebook, docs(),
                                     profile = profile())
            r$.seg_id <- NULL
            if (nrow(r)) r else data.frame(Note = "Aucun segment code")
          },
          "Matrice croisement" = {
            m <- hstat_code_matrix(rv$segments, rv$codebook, profile(), input$mat_var,
                                   count = input$mat_count %||% "segments")
            if (is.null(m)) data.frame(Note = "Aucun code") else m
          },
          "Cooccurrences" = {
            m <- cooc()
            if (is.null(m)) data.frame(Note = "Moins de deux codes")
            else data.frame(Code = rownames(m), m, check.names = FALSE)
          })
        .write_xlsx(sheets, file)
      })

    output$dl_project <- shiny::downloadHandler(
      filename = function() sprintf("hstat_projet_codage_%s.rds", format(Sys.Date(), "%Y%m%d")),
      content = function(file)
        saveRDS(list(hstat = "codage", version = hstat_version(),
                     doc_var = input$doc_var,
                     codebook = rv$codebook, segments = rv$segments), file))

    shiny::observeEvent(input$up_project, {
      f <- input$up_project
      shiny::req(f)
      obj <- tryCatch(readRDS(f$datapath), error = function(e) NULL)
      if (is.null(obj) || !identical(obj$hstat, "codage")) {
        shiny::showNotification("Ce fichier n'est pas un projet de codage HStat.",
                                type = "error", duration = 6); return()
      }
      # Un projet enregistre avant l'arrivee du dictionnaire n'a pas la colonne
      # `keywords` : la migration la recree plutot que de refuser le fichier.
      rv$codebook <- hstat_code_migrate_codebook(obj$codebook)
      rv$segments <- obj$segments
      rv$next_color <- hstat_code_next_color(rv$codebook)
      if (!is.null(obj$doc_var) && obj$doc_var %in% names(get_data()))
        shiny::updateSelectInput(session, "doc_var", selected = obj$doc_var)
      shiny::showNotification(
        sprintf("Projet recharge : %d code(s), %d etiquette(s).",
                nrow(rv$codebook), nrow(rv$segments)),
        type = "message", duration = 6)
    })
  })
}
