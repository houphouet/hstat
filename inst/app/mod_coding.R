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
hstat_code_new_codebook <- function() {
  data.frame(code_id = character(0), label = character(0), color = character(0),
             memo = character(0), keywords = character(0), created = character(0),
             stringsAsFactors = FALSE)
}

# Un projet enregistre avant l'arrivee du dictionnaire n'a pas la colonne
# `keywords` : on la recree plutot que de refuser le fichier.
hstat_code_migrate_codebook <- function(codebook) {
  if (is.null(codebook)) return(hstat_code_new_codebook())
  codebook <- as.data.frame(codebook, stringsAsFactors = FALSE)
  for (col in c("code_id", "label", "color", "memo", "keywords", "created"))
    if (!(col %in% names(codebook)))
      codebook[[col]] <- rep("", max(0L, nrow(codebook)))
  codebook[, c("code_id", "label", "color", "memo", "keywords", "created"),
           drop = FALSE]
}

# Premiere couleur de la palette non encore utilisee ; au-dela on repart au
# debut (le livre de codes n'est pas plafonne a 15 codes).
hstat_code_next_color <- function(codebook) {
  used <- if (is.null(codebook) || nrow(codebook) == 0) character(0) else codebook$color
  free <- setdiff(HSTAT_CODE_PALETTE, used)
  if (length(free) > 0) free[1] else HSTAT_CODE_PALETTE[(length(used) %% length(HSTAT_CODE_PALETTE)) + 1L]
}

hstat_code_add <- function(codebook, label, color = NULL, memo = "", keywords = "") {
  if (is.null(codebook)) codebook <- hstat_code_new_codebook()
  label <- trimws(as.character(label)[1])
  if (is.na(label) || !nzchar(label)) return(codebook)
  # Un meme libelle ne peut pas exister deux fois : le codage deviendrait
  # ambigu au moment de la restitution.
  if (any(tolower(codebook$label) == tolower(label))) return(codebook)
  if (is.null(color) || !nzchar(as.character(color)[1]))
    color <- hstat_code_next_color(codebook)
  rbind(codebook, data.frame(
    code_id = hstat_code_slug(label, codebook$code_id),
    label   = label,
    color   = as.character(color)[1],
    memo    = as.character(memo)[1],
    keywords = as.character(keywords)[1],
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE))
}

hstat_code_remove <- function(codebook, code_id) {
  if (is.null(codebook) || nrow(codebook) == 0) return(hstat_code_new_codebook())
  codebook[!(codebook$code_id %in% code_id), , drop = FALSE]
}

hstat_code_update <- function(codebook, code_id, label = NULL, color = NULL,
                              memo = NULL, keywords = NULL) {
  if (is.null(codebook) || nrow(codebook) == 0) return(codebook)
  i <- which(codebook$code_id == code_id[1])
  if (!length(i)) return(codebook)
  if (!is.null(label) && nzchar(trimws(label))) codebook$label[i] <- trimws(label)
  if (!is.null(color) && nzchar(color))         codebook$color[i] <- color
  if (!is.null(memo))                           codebook$memo[i]  <- memo
  if (!is.null(keywords))                       codebook$keywords[i] <- keywords
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
# 2. SEGMENTS CODES
# ---------------------------------------------------------------------------

hstat_code_new_segments <- function() {
  data.frame(seg_id = character(0), doc_id = character(0), code_id = character(0),
             start = numeric(0), end = numeric(0), text = character(0),
             source = character(0), created = character(0),
             stringsAsFactors = FALSE)
}

# Ajout d'un segment. Renvoie le tableau inchange si la selection est vide ou
# si le meme segment (meme document, meme code, memes bornes) existe deja :
# un double-depot accidentel ne doit pas gonfler les effectifs.
hstat_seg_add <- function(segments, doc_id, code_id, start, end, text = "",
                          source = "manuel") {
  if (is.null(segments)) segments <- hstat_code_new_segments()
  start <- suppressWarnings(as.numeric(start)[1])
  end   <- suppressWarnings(as.numeric(end)[1])
  if (is.na(start) || is.na(end) || end <= start) return(segments)
  doc_id  <- as.character(doc_id)[1]
  code_id <- as.character(code_id)[1]
  dup <- segments$doc_id == doc_id & segments$code_id == code_id &
         segments$start == start & segments$end == end
  if (any(dup)) return(segments)
  n <- nrow(segments)
  rbind(segments, data.frame(
    seg_id  = sprintf("s%06d", n + 1L),
    doc_id  = doc_id,
    code_id = code_id,
    start   = start,
    end     = end,
    text    = as.character(text)[1],
    source  = as.character(source)[1],
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
  data.frame(code_id = codebook$code_id, label = codebook$label,
             color = codebook$color, n_seg = n_seg, n_doc = n_doc,
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
          shiny::actionButton(ns("add_code"), "Ajouter le code",
                              icon = shiny::icon("plus"), class = "btn-warning btn-block btn-sm"),
          shiny::hr(style = "margin:10px 0;"),
          shiny::uiOutput(ns("code_chips")),
          shiny::hr(style = "margin:10px 0;"),
          shiny::tags$small(style = "color:#7f8c8d;", shiny::icon("info-circle"),
            " Deposez une selection sur un code, ou cliquez dessus. Le ",
            shiny::tags$b("x"), " supprime le code et ses etiquettes."),
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
      rv$codebook <- hstat_code_add(rv$codebook, lab, col)
      if (nrow(rv$codebook) == before) {
        shiny::showNotification(sprintf("Le code « %s » existe deja.", lab),
                                type = "warning", duration = 4)
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

    output$code_chips <- shiny::renderUI({
      cb <- rv$codebook
      if (nrow(cb) == 0)
        return(shiny::div(style = "color:#7f8c8d;font-size:13px;font-style:italic;",
          "Aucun code pour l'instant. Creez-en un ci-dessus, ou laissez ",
          "l'assistant IA vous en proposer."))
      cnt <- hstat_code_counts(cb, rv$segments)
      shiny::tagList(lapply(seq_len(nrow(cb)), function(i) {
        shiny::tags$div(
          class = "hstat-code-chip", `data-code` = cb$code_id[i],
          style = sprintf("background:%s;color:%s;", cb$color[i],
                          .hstat_code_ink(cb$color[i])),
          title = if (nzchar(cb$memo[i])) cb$memo[i] else cb$label[i],
          shiny::tags$span(class = "hstat-chip-del", `data-code` = cb$code_id[i],
                           title = "Supprimer ce code", shiny::HTML("&times;")),
          shiny::tags$span(class = "hstat-chip-count",
                           sprintf("%d seg. / %d rep.", cnt$n_seg[i], cnt$n_doc[i])),
          cb$label[i])
      }))
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
        sprintf(" %d extrait(s) correspondant a la selection, sur %d etiquette(s) au total.",
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
      content = function(file)
        ggplot2::ggsave(file, cloud_gg(), width = 10, height = 7, dpi = 300, bg = "white"))

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
        ggplot2::ggsave(file, map_gg(), width = 10, height = 8, dpi = 300, bg = "white"))
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
                          sprintf(" %d proposition(s) ecartee(s) : extrait introuvable dans le texte.", miss)
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
          msg = sprintf("%s : %d etiquette(s) posee(s) sur %d reponse(s) analysee(s) (tout le corpus).%s",
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
