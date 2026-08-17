# =============================================================================
# mod_qualitative.R  --  Analyses de donnees qualitatives d'enquete
# -----------------------------------------------------------------------------


`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Nom de fichier sûr (accents retirés, non-alphanumériques -> underscore)
.safe_name <- function(x) {
  x <- as.character(x)[1]
  x <- chartr("\u00e0\u00e2\u00e4\u00e9\u00e8\u00ea\u00eb\u00ee\u00ef\u00f4\u00f6\u00f9\u00fb\u00fc\u00e7",
              "aaaeeeeiioouuuc", tolower(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- "tableau"
  substr(x, 1, 60)
}

# Écriture d'un classeur Excel (une feuille par élément nommé de `sheets`).
# Utilise openxlsx si présent, sinon writexl ; repli CSV concaténé sinon.
.write_xlsx <- function(sheets, file) {
  # noms de feuilles uniques, <= 31 caractères, sans caractères interdits
  nm <- vapply(names(sheets), function(s) gsub("[\\\\/\\[\\]:*?]", "_", substr(s, 1, 31)),
               character(1))
  nm <- make.unique(nm, sep = "_"); names(sheets) <- substr(nm, 1, 31)
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    for (s in names(sheets)) {
      openxlsx::addWorksheet(wb, s)
      openxlsx::writeData(wb, s, sheets[[s]])
      openxlsx::setColWidths(wb, s, cols = seq_len(ncol(sheets[[s]])), widths = "auto")
    }
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  } else if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(sheets, path = file)
  } else {
    con <- file(file, open = "w", encoding = "UTF-8"); on.exit(close(con))
    for (s in names(sheets)) {
      writeLines(paste0("# ", s), con)
      utils::write.csv(sheets[[s]], con, row.names = FALSE)
      writeLines("", con)
    }
  }
}

# Applique un style utilisateur (titres, tailles de police, gras/italique,
# rotation, grille, axes/graduations en noir) à un ggplot déjà construit.
# Base ggplot2 pure (pas de dépendance ggtext). Les champs titre/axes/légende
# laissés vides conservent ceux du graphique. "opts" est une liste nommée.
hstat_q_apply_style <- function(p, opts = list()) {
  if (is.null(p) || !inherits(p, "ggplot")) return(p)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(p)
  g <- function(k, d) { v <- opts[[k]]; if (is.null(v) || (is.character(v) && !nzchar(trimws(v)))) d else v }

  # 1) Titres et étiquettes (uniquement si l'utilisateur les renseigne)
  labs_args <- list()
  if (nzchar(trimws(g("title", ""))))  labs_args$title  <- g("title", "")
  if (nzchar(trimws(g("xlab", ""))))   labs_args$x      <- g("xlab", "")
  if (nzchar(trimws(g("ylab", ""))))   labs_args$y      <- g("ylab", "")
  if (nzchar(trimws(g("legend", ""))))  { labs_args$fill <- g("legend", ""); labs_args$colour <- g("legend", "") }
  if (length(labs_args) > 0) p <- p + do.call(ggplot2::labs, labs_args)

  face <- function(bold, italic)
    if (bold && italic) "bold.italic" else if (bold) "bold" else if (italic) "italic" else "plain"
  x_rot <- g("x_rotation", 45)
  x_hjust <- if (x_rot > 0) 1 else 0.5
  black_axes <- isTRUE(g("black_axes", FALSE))
  black_ticks <- isTRUE(g("black_ticks", FALSE))
  axis_text_color <- if (black_ticks) "black" else "grey30"
  face_axtext <- face(isTRUE(g("axis_text_bold", FALSE)), isTRUE(g("axis_text_italic", FALSE)))
  face_axtitle <- face(isTRUE(g("axis_title_bold", TRUE)), isTRUE(g("axis_title_italic", FALSE)))

  p + ggplot2::theme(
    plot.title = ggplot2::element_text(size = g("title_size", 16), hjust = 0.5, face = "bold"),
    axis.line = if (black_axes) ggplot2::element_line(color = "black", linewidth = 0.75) else ggplot2::element_blank(),
    axis.ticks = if (black_ticks) ggplot2::element_line(color = "black", linewidth = 0.5) else ggplot2::element_blank(),
    axis.ticks.length = if (black_ticks) grid::unit(0.2, "cm") else grid::unit(0, "cm"),
    axis.title.x = hstat_axe_titre(g("axis_label_size", 12), face_axtitle,
                                   g("axis_title_align", "0.5"), "x",
                                   retour = isTRUE(g("axis_title_wrap", TRUE))),
    axis.title.y = hstat_axe_titre(g("axis_label_size", 12), face_axtitle,
                                   g("axis_title_align", "0.5"), "y",
                                   retour = isTRUE(g("axis_title_wrap", TRUE))),
    axis.text.x = ggplot2::element_text(angle = x_rot, hjust = x_hjust,
                                        size = g("axis_text_size", 10),
                                        face = face_axtext, color = axis_text_color),
    axis.text.y = ggplot2::element_text(size = g("axis_text_size", 10),
                                        face = face_axtext, color = axis_text_color),
    legend.text = ggplot2::element_text(size = g("legend_text_size", 10)),
    legend.title = ggplot2::element_text(size = g("legend_text_size", 10), face = "bold"),
    panel.grid.major = if (isTRUE(g("show_grid", TRUE))) ggplot2::element_line(color = "gray90", linewidth = 0.4) else ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank())
}

# Applique une palette de couleurs choisie a un ggplot, quel que soit le
# graphique (barres, camembert, nuage de mots, thèmes...). Detecte si l'echelle
# de remplissage est continue (fill numerique -> degrade) ou discrete (fill
# categoriel -> palette qualitative), et remplace aussi la couleur du texte
# (nuage de mots). "default" laisse le graphique inchange.
hstat_q_apply_palette <- function(p, palette = "default",
                                  col_low = "#aed6f1", col_high = "#1f618d") {
  if (is.null(p) || !inherits(p, "ggplot") || identical(palette, "default")) return(p)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(p)

  # Un fill/color est-il continu (numerique) dans le mapping global ?
  is_continuous_aes <- function(p, aes_name) {
    m <- p$mapping[[aes_name]]
    if (is.null(m)) {
      for (l in p$layers) if (!is.null(l$mapping[[aes_name]])) { m <- l$mapping[[aes_name]]; break }
    }
    if (is.null(m)) return(NA)
    var <- tryCatch(rlang::eval_tidy(m, p$data), error = function(e) NULL)
    if (is.null(var)) return(NA)
    is.numeric(var)
  }

  # Bornes de degrade + palette discrete (Brewer/viridis) pour chaque choix
  # Normalise un hex court (#abc -> #aabbcc) pour colorRampPalette
  norm_hex <- function(h) {
    if (is.character(h) && grepl("^#[0-9A-Fa-f]{3}$", h))
      paste0("#", paste0(rep(substring(h, 2:4, 2:4), each = 2), collapse = ""))
    else h
  }
  col_low <- norm_hex(col_low); col_high <- norm_hex(col_high)

  grad <- switch(palette,
    blues   = c("#deebf7", "#08519c"), greens  = c("#e5f5e0", "#00441b"),
    oranges = c("#fee6ce", "#7f2704"), purples = c("#efedf5", "#3f007d"),
    greys   = c("#f0f0f0", "#252525"), spectral = c("#3288bd", "#d53e4f"),
    viridis = NULL, custom = c(col_low, col_high),
    c(col_low, col_high))
  brew <- switch(palette,
    blues = "Blues", greens = "Greens", oranges = "Oranges",
    purples = "Purples", greys = "Greys", spectral = "Spectral", NULL)

  fc <- is_continuous_aes(p, "fill")
  cc <- is_continuous_aes(p, "colour")
  suppressMessages({
    if (isTRUE(fc)) {
      p <- p + if (identical(palette, "viridis"))
        ggplot2::scale_fill_viridis_c()
      else ggplot2::scale_fill_gradient(low = grad[1], high = grad[2])
    } else if (isFALSE(fc)) {
      p <- p + if (identical(palette, "viridis"))
        ggplot2::scale_fill_viridis_d()
      else if (!is.null(brew))
        ggplot2::scale_fill_brewer(palette = brew)
      else ggplot2::scale_fill_manual(
        values = grDevices::colorRampPalette(grad)(24))
    }
    if (isTRUE(cc)) {
      p <- p + if (identical(palette, "viridis"))
        ggplot2::scale_colour_viridis_c()
      else ggplot2::scale_colour_gradient(low = grad[1], high = grad[2])
    } else if (isFALSE(cc)) {
      p <- p + if (identical(palette, "viridis"))
        ggplot2::scale_colour_viridis_d()
      else if (!is.null(brew))
        ggplot2::scale_colour_brewer(palette = brew)
      else ggplot2::scale_colour_manual(
        values = grDevices::colorRampPalette(grad)(24))
    }
  })
  p
}

# ---------------------------------------------------------------------------
# Detection automatique du type d'une variable d'enquete
# ---------------------------------------------------------------------------
# Renvoie : "nominale", "ordinale", "textuelle", "numérique" ou "binaire".
hstat_q_detect_type <- function(x, ordinal_hint = NULL) {
  if (!is.null(ordinal_hint) && isTRUE(ordinal_hint)) return("ordinale")
  if (is.ordered(x)) return("ordinale")
  if (is.numeric(x)) {
    u <- length(unique(stats::na.omit(x)))
    if (u <= 2) return("binaire")
    if (u <= 10) return("ordinale")   # echelle de notation probable
    return("numérique")
  }
  xc <- as.character(x)
  xc <- xc[!is.na(xc) & nzchar(trimws(xc))]
  if (length(xc) == 0) return("nominale")
  # Texte libre : phrases longues, beaucoup de valeurs uniques, espaces
  n_words <- vapply(xc, function(s) length(strsplit(trimws(s), "\\s+")[[1]]), integer(1))
  mean_words <- mean(n_words)
  prop_unique <- length(unique(xc)) / length(xc)
  if (mean_words >= 4 && prop_unique > 0.6) return("textuelle")
  # Detection ordinale par mots-cles d'echelle
  lv <- tolower(unique(xc))
  ord_patterns <- c("jamais", "rarement", "parfois", "souvent", "toujours",
                    "pas du tout", "peu", "moyennement", "beaucoup", "enormement",
                    "très insatisfait", "insatisfait", "neutre", "satisfait", "très satisfait",
                    "pas d'accord", "d'accord", "totalement",
                    "faible", "moyen", "élevé", "fort",
                    "bas", "haut", "mauvais", "passable", "bon", "excellent")
  if (sum(vapply(ord_patterns, function(p) any(grepl(p, lv, fixed = TRUE)), logical(1))) >= 2)
    return("ordinale")
  "nominale"
}

# Ordre canonique des modalites ordinales courantes (pour ordonner les facteurs)
hstat_q_ordinal_levels <- function(x) {
  lv <- unique(as.character(stats::na.omit(x)))
  scales <- list(
    c("Jamais","Rarement","Parfois","Souvent","Toujours"),
    c("Pas du tout","Un peu","Moyennement","Beaucoup","Enormement"),
    c("Tres insatisfait","Insatisfait","Neutre","Satisfait","Tres satisfait"),
    c("Pas du tout d'accord","Plutot pas d'accord","Neutre","Plutot d'accord","Tout a fait d'accord"),
    c("Tres faible","Faible","Moyen","Élevé","Tres élevé"),
    c("Mauvais","Passable","Moyen","Bon","Excellent"))
  norm <- function(s) tolower(trimws(s))
  for (sc in scales) {
    if (all(norm(lv) %in% norm(sc))) {
      ord <- sc[norm(sc) %in% norm(lv)]
      return(ord)
    }
  }
  # sinon, tri naturel
  suppressWarnings({
    nums <- as.numeric(lv)
    if (!any(is.na(nums))) return(lv[order(nums)])
  })
  sort(lv)
}

# ===========================================================================
# ANALYSE NOMINALE -- une variable
# ===========================================================================
hstat_q_nominal_univariate <- function(x, var_name = "Variable") {
  x <- as.character(x)
  n_total <- length(x)
  x_valid <- x[!is.na(x) & nzchar(trimws(x))]
  n_valid <- length(x_valid)
  if (n_valid == 0) return(list(ok = FALSE, notes = "Aucune donnée valide."))

  tab <- sort(table(x_valid), decreasing = TRUE)
  freq_df <- data.frame(
    Modalite = names(tab),
    Effectif = as.integer(tab),
    Pourcentage = round(100 * as.integer(tab) / n_valid, 2),
    Pourcentage_cumule = round(cumsum(100 * as.integer(tab) / n_valid), 2),
    stringsAsFactors = FALSE)

  k <- length(tab)
  props <- as.integer(tab) / n_valid
  # Indices de diversite / concentration
  shannon <- -sum(props * log(props))
  shannon_max <- log(k)
  evenness <- if (shannon_max > 0) shannon / shannon_max else NA_real_
  simpson <- 1 - sum(props^2)            # probabilite que 2 tirages different
  herfindahl <- sum(props^2)             # concentration (HHI)
  max_eff <- as.integer(tab)[1]
  mode_all <- names(tab)[as.integer(tab) == max_eff]
  n_modes <- length(mode_all)
  mode_mod <- if (n_modes == 1) mode_all else paste(mode_all, collapse = ", ")
  mode_pct <- round(100 * props[1], 1)
  mode_type <- if (n_modes == 1) "Unimodale" else if (n_modes == 2) "Bimodale" else "Multimodale"

  metrics <- data.frame(
    Metrique = c("Effectif total", "Effectif valide", "Valeurs manquantes",
                 "Nombre de modalités", "Mode(s)", "Nature de la distribution",
                 "Fréquence du mode (%)",
                 "Entropie de Shannon", "Équitabilité (Pielou)",
                 "Indice de Simpson (diversité)", "Indice de Herfindahl (concentration)"),
    Valeur = c(n_total, n_valid, n_total - n_valid,
               k, mode_mod, mode_type, mode_pct,
               round(shannon, 3), round(evenness, 3),
               round(simpson, 3), round(herfindahl, 3)),
    Interpretation = c(
      "Nombre total d'observations (valides + manquantes).",
      "Observations exploitables (hors valeurs manquantes).",
      trf("%s%% des observations sont manquantes.",
              round(100 * (n_total - n_valid) / max(n_total, 1), 1)),
      if (k <= 3) "Peu de modalités : variable simple à analyser."
        else if (k <= 10) "Nombre de modalités modéré." else "Nombreuses modalités : envisager un regroupement.",
      if (n_modes == 1) trf("Modalité la plus fréquente (%.1f %% des réponses).", mode_pct)
        else trf("%d modalités à égalité au sommet (%.1f %% chacune).", n_modes, mode_pct),
      trf("Distribution %s : %d mode(s) détecté(s).", tolower(mode_type), n_modes),
      if (mode_pct > 50) "Le mode domine (> 50 %) : distribution déséquilibrée."
        else "Aucune modalité ne domine nettement.",
      trf("Diversité de l'information (max possible = %.3f). Plus c'est élevé, plus c'est diversifié.", round(shannon_max, 3)),
      if (is.na(evenness)) "Non définie." else if (evenness > 0.85) "Réponses très équilibrées entre modalités."
        else if (evenness < 0.5) "Réponses concentrées sur peu de modalités." else "Répartition modérément équilibrée.",
      "Probabilité que deux tirages au hasard diffèrent (0 = uniforme, 1 = très divers).",
      if (herfindahl > 0.5) "Forte concentration sur peu de modalités." else "Faible concentration : réponses dispersées."),
    stringsAsFactors = FALSE)

  interp <- c(
    if (n_modes == 1)
      trf("La modalité la plus fréquente (mode) est \"%s\" (%.1f %% des réponses valides).", mode_mod, mode_pct)
    else
      trf("Distribution %s : %d modalités à égalité au sommet (%s), chacune à %.1f %%.",
              tolower(mode_type), n_modes, mode_mod, mode_pct),
    trf("La variable compte %d modalités distinctes sur %d réponses valides.", k, n_valid),
    if (!is.na(evenness)) {
      if (evenness > 0.85) "Les réponses sont très réparties (forte équitabilité) : aucune modalité n'ecrase les autres."
      else if (evenness < 0.5) "Les réponses sont concentrées sur peu de modalités (faible équitabilité)."
      else "Les réponses sont modérément réparties entre les modalités."
    } else NULL,
    trf("Indice de Simpson = %.3f : probabilité que deux répondants au hasard donnent des réponses differentes.", simpson))

  # Graphiques
  plot_bar <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    d <- freq_df
    d$Modalite <- factor(d$Modalite, levels = rev(d$Modalite))
    ggplot2::ggplot(d, ggplot2::aes(x = Modalite, y = Effectif, fill = Modalite)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d (%.1f%%)", Effectif, Pourcentage)),
                         hjust = -0.1, size = 3.4) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
      ggplot2::labs(title = paste0("Distribution -- ", var_name),
                    x = NULL, y = "Effectif") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }
  plot_pie <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    d <- freq_df
    d$Modalite <- factor(d$Modalite, levels = d$Modalite)
    ggplot2::ggplot(d, ggplot2::aes(x = "", y = Pourcentage, fill = Modalite)) +
      ggplot2::geom_col(width = 1, color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f %%\n(n = %d)", Pourcentage, Effectif)),
                         position = ggplot2::position_stack(vjust = 0.5),
                         size = 3.8, fontface = "bold", color = "grey15") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::labs(title = paste0("Répartition -- ", var_name), x = NULL, y = NULL, fill = "Modalité") +
      ggplot2::theme_void(base_size = 12)
  }

  list(ok = TRUE,
       metrics = metrics,
       tables = list("Tableau de fréquences" = freq_df),
       plotfns = list("Diagramme en barres" = plot_bar, "Diagramme circulaire" = plot_pie),
       interpretation = interp,
       notes = trf("%d valeurs manquantes exclues.", n_total - n_valid))
}

# ---------------------------------------------------------------------------
# IC du V de Cramer par inversion du khi-deux non central (methode de
# Smithson, identique a DescTools::CramerV) : bornes du parametre de
# non-centralite lambda telles que P(X2 <= chi2_obs | df, lambda) = 1-a/2 et a/2,
# puis V = sqrt((lambda + df) / (n * (min(r,c) - 1))).
hstat_q_cramer_ci <- function(chi2, df, n, k_min, conf = 0.95) {
  if (any(!is.finite(c(chi2, df, n))) || k_min < 2 || n <= 0)
    return(c(NA_real_, NA_real_))
  alpha <- 1 - conf
  f <- function(ncp, target) suppressWarnings(stats::pchisq(chi2, df, ncp = ncp)) - target
  root <- function(target) {
    if (f(0, target) <= 0) return(0)
    tryCatch(stats::uniroot(f, interval = c(0, chi2 + 4 * sqrt(2 * chi2) + 100),
                            target = target, extendInt = "downX")$root,
             error = function(e) NA_real_)
  }
  lo <- root(1 - alpha / 2)
  hi <- root(alpha / 2)
  denom <- n * (k_min - 1)
  c(sqrt(max(lo + df, 0) / denom), sqrt(max(hi + df, 0) / denom))
}

# Bloc console "epitools" : $data (avec totaux), $measure (estimation + IC par
# modalite, 1re = reference), $p.value (fisher.exact, chi.square).
hstat_q_epitools_block <- function(tab2, kind = c("OR", "RR"), conf = 0.95) {
  kind <- match.arg(kind)
  z <- stats::qnorm(1 - (1 - conf) / 2)
  rn <- rownames(tab2)
  meas <- matrix(NA_real_, nrow = nrow(tab2), ncol = 3,
                 dimnames = list(rn, c("estimate", "lower", "upper")))
  meas[1, 1] <- 1
  pv <- matrix(NA_real_, nrow = nrow(tab2), ncol = 2,
               dimnames = list(rn, c("fisher.exact", "chi.square")))
  for (i in seq_len(nrow(tab2))[-1]) {
    a <- tab2[1, 1]; b <- tab2[1, 2]; cc <- tab2[i, 1]; dd <- tab2[i, 2]
    aa <- a; bb <- b; c2 <- cc; d2 <- dd
    if (any(c(aa, bb, c2, d2) == 0)) { aa <- aa + .5; bb <- bb + .5; c2 <- c2 + .5; d2 <- d2 + .5 }
    if (kind == "OR") {
      est <- (d2 / c2) / (bb / aa)
      se <- sqrt(1/aa + 1/bb + 1/c2 + 1/d2)
    } else {
      p1 <- d2 / (c2 + d2); p0 <- bb / (aa + bb)
      est <- p1 / p0
      se <- sqrt((1 - p1) / d2 + (1 - p0) / bb)
    }
    meas[i, ] <- c(est, exp(log(est) - z * se), exp(log(est) + z * se))
    sub <- rbind(tab2[1, ], tab2[i, ])
    pv[i, 1] <- tryCatch(stats::fisher.test(sub)$p.value,
                         error = function(e)
                           tryCatch(stats::fisher.test(sub, simulate.p.value = TRUE, B = 5000)$p.value,
                                    error = function(e2) NA_real_))
    pv[i, 2] <- tryCatch(suppressWarnings(stats::chisq.test(sub)$p.value),
                         error = function(e) NA_real_)
  }
  lab <- if (kind == "OR") "odds ratio" else "risk ratio"
  meth <- if (kind == "OR") "Wald (approximation normale) & IC log-normal"
          else "Wald (approximation normale) & IC log-normal"
  c("$data", utils::capture.output(print(stats::addmargins(tab2))), "",
    "$measure",
    sprintf("        %s with %.0f%% C.I.", lab, 100 * conf),
    utils::capture.output(print(round(meas, 7))), "",
    "$p.value", "        two-sided",
    utils::capture.output(print(signif(pv, 6))), "",
    sprintf('attr(,"method")'), sprintf('[1] "%s"', meth))
}

# ---------------------------------------------------------------------------
# TEST CHI-DEUX D'AJUSTEMENT / MULTINOMIAL EXACT -- une variable nominale
# Teste si la distribution observée des modalités suit des proportions
# attendues (équiprobables par défaut, ou fournies par l'utilisateur).
# method : "chisq" (khi-deux d'ajustement, Yates non applicable ici) ou
#          "multinomial" (test exact : EMT si disponible, sinon Monte-Carlo
#          par ordre de probabilité avec dmultinom, sans dépendance).
# ---------------------------------------------------------------------------
# Version STRATIFIÉE du test d'adéquation : teste la distribution de Y (variable
# analysée) séparément dans chaque modalité du facteur X, plus le test global.
hstat_q_gof_stratified <- function(y, x, yname = "Y", xname = "X",
                                   expected_props = NULL,
                                   method = "chisq", B = 10000,
                                   posthoc_adjust = "bonferroni") {
  y <- as.character(y); x <- as.character(x)
  keep <- !is.na(y) & nzchar(trimws(y)) & !is.na(x) & nzchar(trimws(x))
  y <- y[keep]; x <- x[keep]
  if (length(y) < 5) return(list(ok = FALSE, notes = "Trop peu d'observations complètes."))
  groups <- sort(unique(x))
  if (length(groups) < 2)
    return(hstat_q_gof_analysis(y, yname, expected_props, method, B, posthoc_adjust))

  # Test global (toutes modalités de Y confondues)
  glob <- hstat_q_gof_analysis(y, yname, expected_props, method, B, posthoc_adjust)

  # Un test par groupe de X
  per_group <- lapply(groups, function(g) {
    r <- hstat_q_gof_analysis(y[x == g], sprintf("%s | %s = %s", yname, xname, g),
                              expected_props, method, B, posthoc_adjust)
    list(group = g, res = r)
  })

  # Tableau de synthèse : p-value du test par groupe
  synth <- do.call(rbind, lapply(per_group, function(pg) {
    r <- pg$res
    if (!isTRUE(r$ok)) return(data.frame(
      Groupe = pg$group, n = NA, Khi2 = NA, ddl = NA, p_value = NA,
      Conclusion = r$notes %||% "non calculé", stringsAsFactors = FALSE))
    m <- r$metrics
    getv <- function(lbl) m$Valeur[m$Metrique == lbl][1]
    pval <- suppressWarnings(as.numeric(gsub("[^0-9.eE-]", "", getv("p-value (Khi-deux)"))))
    data.frame(
      Groupe = pg$group,
      n = as.integer(getv("Effectif valide (n)")),
      Khi2 = as.numeric(getv("Khi-deux")),
      ddl = as.integer(getv("Degrés de liberté")),
      p_value = round(pval, 5),
      Conclusion = if (!is.na(pval) && pval < 0.05)
        "Distribution significativement différente de H0" else "Compatible avec H0",
      stringsAsFactors = FALSE)
  }))

  # Table de contingence X x Y (utile pour lecture croisée)
  ct <- table(factor(x, levels = groups), y)
  ct_df <- as.data.frame.matrix(ct)
  ct_df <- cbind(Groupe = rownames(ct_df), ct_df); rownames(ct_df) <- NULL
  names(ct_df)[1] <- xname

  tables <- list("Synthèse par groupe (X)" = synth,
                 "Table croisée X x Y" = ct_df,
                 "Test global (toutes modalités)" = glob$tables[["Observé vs attendu"]])

  # Interprétation stratifiée
  n_sig <- sum(synth$p_value < 0.05, na.rm = TRUE)
  interp <- c(
    trf("Analyse stratifiée : distribution de « %s » testée dans chacune des %d modalités de « %s ».",
            yname, length(groups), xname),
    trf("%d groupe(s) sur %d présentent une distribution significativement différente des proportions attendues.",
            n_sig, length(groups)),
    glob$interpretation[[1]])

  # Graphique : distribution de Y par groupe de X (barres groupées, ggplot défaut)
  plot_strat <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    dd <- as.data.frame(ct); names(dd) <- c(".X", ".Y", "Effectif")
    ggplot2::ggplot(dd, ggplot2::aes(.X, Effectif, fill = .Y)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.85), width = 0.8) +
      ggplot2::labs(title = sprintf("Distribution de %s selon %s", yname, xname),
                    x = xname, y = "Effectif", fill = yname) +
      ggplot2::theme_minimal(base_size = 12)
  }

  console <- c(trf("=== Test d'adéquation stratifié : %s selon %s ===", yname, xname), "",
               "Synthèse par groupe :",
               utils::capture.output(print(synth, row.names = FALSE)), "",
               "--- Test global ---", glob$console)

  list(ok = TRUE,
       metrics = glob$metrics,   # métriques du test global (interprétées)
       tables = c(tables, glob$tables[setdiff(names(glob$tables), "Observé vs attendu")]),
       plotfns = c(list("Distribution de Y selon X" = plot_strat), glob$plotfns),
       interpretation = interp, console = console,
       notes = trf("Analyse stratifiée : %d groupes de %s.", length(groups), xname))
}

hstat_q_gof_analysis <- function(x, var_name = "Variable",
                                 expected_props = NULL,
                                 method = c("chisq", "multinomial"),
                                 B = 10000, posthoc_adjust = "bonferroni") {
  method <- match.arg(method)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  n <- length(x)
  if (n < 5) return(list(ok = FALSE, notes = "Trop peu d'observations (n < 5)."))
  tab <- table(x)
  k <- length(tab)
  if (k < 2) return(list(ok = FALSE, notes = "Une seule modalité : rien à tester."))

  # Proportions attendues : equiprobables ou fournies (recyclees/normalisees)
  if (is.null(expected_props) || length(expected_props) == 0 || anyNA(expected_props)) {
    p0 <- rep(1 / k, k)
    p0_lab <- tr("équiprobables")
  } else {
    if (length(expected_props) != k)
      return(list(ok = FALSE, notes = trf(
        "Le nombre de proportions attendues (%d) doit égaler le nombre de modalités (%d : %s).",
        length(expected_props), k, paste(names(tab), collapse = ", "))))
    if (any(expected_props <= 0))
      return(list(ok = FALSE, notes = "Les proportions attendues doivent être strictement positives."))
    p0 <- expected_props / sum(expected_props)
    p0_lab <- tr("personnalisées")
  }

  obs <- as.integer(tab)
  exp_eff <- n * p0
  chi <- suppressWarnings(stats::chisq.test(obs, p = p0))
  chi2 <- unname(chi$statistic); ddl <- unname(chi$parameter); p_chi <- chi$p.value
  resid_p <- (obs - exp_eff) / sqrt(exp_eff)
  contrib <- 100 * resid_p^2 / chi2
  expected_ok <- all(exp_eff >= 5)
  cohen_w <- sqrt(chi2 / n)
  v_gof <- sqrt(chi2 / (n * (k - 1)))

  # p Monte-Carlo du khi-deux (utile si attendus < 5)
  p_mc <- tryCatch(suppressWarnings(
    stats::chisq.test(obs, p = p0, simulate.p.value = TRUE, B = B)$p.value),
    error = function(e) NA_real_)

  # Test multinomial exact
  p_multi <- NA_real_; multi_meth <- NULL
  if (method == "multinomial") {
    if (requireNamespace("EMT", quietly = TRUE) && n <= 200 && k <= 5) {
      em <- tryCatch(EMT::multinomial.test(obs, prob = p0, useChisq = FALSE),
                     error = function(e) NULL)
      if (!is.null(em)) { p_multi <- em$p.value; multi_meth <- "EMT (énumération exacte)" }
    }
    if (is.na(p_multi)) {
      # Monte-Carlo par ordre de probabilité : P(dmultinom(X) <= dmultinom(obs))
      d_obs <- stats::dmultinom(obs, prob = p0)
      sims <- stats::rmultinom(B, size = n, prob = p0)
      d_sim <- apply(sims, 2, function(s) stats::dmultinom(s, prob = p0))
      p_multi <- (sum(d_sim <= d_obs + 1e-12) + 1) / (B + 1)
      multi_meth <- sprintf("Monte-Carlo (%d tirages)", B)
    }
  }
  p_final <- if (method == "multinomial") p_multi else p_chi

  # ---- Métriques INTERPRÉTÉES ----
  metrics <- data.frame(
    Metrique = c("Effectif valide (n)", "Nombre de modalités (k)",
                 "Proportions attendues", "Khi-deux", "Degrés de liberté",
                 "p-value (Khi-deux)", "p-value (Khi-deux Monte-Carlo)",
                 if (method == "multinomial") "p-value (Multinomial exact)" else NULL,
                 "Conditions (attendus >= 5)", "w de Cohen (taille d'effet)",
                 "V de Cramér (ajustement)"),
    Valeur = c(n, k, p0_lab, round(chi2, 3), as.integer(ddl),
               format.pval(p_chi, digits = 3),
               if (is.na(p_mc)) "non calculé" else format.pval(p_mc, digits = 3),
               if (method == "multinomial") format.pval(p_multi, digits = 3) else NULL,
               if (expected_ok) "Respectées" else "Non respectées",
               round(cohen_w, 3), round(v_gof, 3)),
    Interpretation = c(
      "Observations valides (valeurs manquantes exclues).",
      trf("Modalités comparées : %s.", paste(names(tab), collapse = ", ")),
      if (identical(p0_lab, "équiprobables"))
        trf("H0 : chaque modalité a la même probabilité (1/%d = %.1f %%).", k, 100 / k)
      else trf("H0 : les proportions valent %s.", paste(sprintf("%.1f%%", 100 * p0), collapse = ", ")),
      "Écart global entre effectifs observés et effectifs attendus sous H0.",
      sprintf("k - 1 = %d.", as.integer(ddl)),
      if (!is.na(p_chi) && p_chi < 0.05)
        "p < 0,05 : la distribution observée s'écarte significativement des proportions attendues."
      else "p >= 0,05 : pas d'écart significatif aux proportions attendues.",
      "Version par simulation, fiable même si des effectifs attendus sont < 5.",
      if (method == "multinomial")
        trf("Test exact (%s) : recommandé pour les petits effectifs.", multi_meth) else NULL,
      if (expected_ok) "Tous les effectifs attendus >= 5 : le Khi-deux asymptotique est fiable."
      else "Effectifs attendus < 5 : se fier au Monte-Carlo ou au multinomial exact.",
      # LA NUANCE EST PORTEE PAR LA PHRASE ENTIERE, pas par un mot isole.
      # « moyenne » vaut *medium* pour une taille d'effet mais *mean* en
      # statistique : une meme cle dans le dictionnaire corromprait l'autre
      # sens. Quatre phrases completes lèvent l'ambiguite.
      tr(if (cohen_w < 0.1)
           "Taille d'effet : négligeable (repères : 0,1 petite ; 0,3 moyenne ; 0,5 grande)."
         else if (cohen_w < 0.3)
           "Taille d'effet : petite (repères : 0,1 petite ; 0,3 moyenne ; 0,5 grande)."
         else if (cohen_w < 0.5)
           "Taille d'effet : moyenne (repères : 0,1 petite ; 0,3 moyenne ; 0,5 grande)."
         else
           "Taille d'effet : grande (repères : 0,1 petite ; 0,3 moyenne ; 0,5 grande)."),
      "Ampleur de l'écart, normalisée entre 0 (conformité parfaite) et 1."),
    stringsAsFactors = FALSE)

  # ---- Tables ----
  gof_df <- data.frame(
    Modalite = names(tab), Observe = obs,
    Attendu = round(exp_eff, 2), Prop_attendue_pct = round(100 * p0, 2),
    Residu_Pearson = round(resid_p, 3),
    Contribution_chi2_pct = round(contrib, 1),
    stringsAsFactors = FALSE)

  # ---- POST-HOC : comparaisons par paires de modalités + lettres de groupes ----
  # Chaque paire (i, j) est comparée par un khi-deux 2x2 (effectif i vs j contre
  # le total), les p-values sont ajustées, puis regroupées en lettres (CLD) :
  # deux modalités partageant une lettre ne diffèrent pas significativement.
  posthoc_df <- NULL; groups_letters <- NULL
  if (k >= 2) {
    N_total <- sum(obs)
    comps <- character(0); chi2_v <- numeric(0); p_raw <- numeric(0)
    for (i in 1:(k - 1)) for (j in (i + 1):k) {
      m2 <- matrix(c(obs[i], obs[j], N_total - obs[i], N_total - obs[j]), nrow = 2)
      tt <- tryCatch(suppressWarnings(stats::chisq.test(m2, correct = (k == 2))),
                     error = function(e) NULL)
      comps <- c(comps, paste0(names(tab)[i], " vs ", names(tab)[j]))
      chi2_v <- c(chi2_v, if (is.null(tt)) NA_real_ else round(unname(tt$statistic), 4))
      p_raw <- c(p_raw, if (is.null(tt)) NA_real_ else tt$p.value)
    }
    adj_m <- if (posthoc_adjust %in% c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none"))
      posthoc_adjust else "bonferroni"
    p_adj <- stats::p.adjust(p_raw, method = adj_m)
    posthoc_df <- data.frame(
      Comparaison = comps, Khi2 = chi2_v,
      p_brute = round(p_raw, 6), p_ajustee = round(p_adj, 6),
      Significatif = ifelse(!is.na(p_adj) & p_adj < 0.05, "OUI *", "non"),
      Ajustement = adj_m, stringsAsFactors = FALSE)
    # Lettres de groupes homogènes (multcompView si dispo)
    if (requireNamespace("multcompView", quietly = TRUE)) {
      pm <- matrix(1, k, k, dimnames = list(names(tab), names(tab))); z <- 1
      for (i in 1:(k - 1)) for (j in (i + 1):k) {
        pv <- p_adj[z]; if (is.na(pv)) pv <- 1
        pm[i, j] <- pm[j, i] <- pv; z <- z + 1
      }
      diag(pm) <- 1
      groups_letters <- tryCatch(
        multcompView::multcompLetters(pm, threshold = 0.05)$Letters,
        error = function(e) NULL)
    }
    if (!is.null(groups_letters)) {
      gof_df$Groupe <- groups_letters[match(names(tab), names(groups_letters))]
    }
  }

  # ---- Interprétation globale ----
  worst <- names(tab)[which.max(abs(resid_p))]
  interp <- c(
    trf("Test %s des proportions %s de \"%s\" : p = %s -- %s.",
            if (method == "multinomial") "multinomial exact" else "du Khi-deux d'ajustement",
            p0_lab, var_name, format.pval(p_final, digits = 3),
            if (!is.na(p_final) && p_final < 0.05)
              "la répartition observée diffère significativement de la répartition attendue"
            else "la répartition observée est compatible avec la répartition attendue"),
    trf("La modalité qui s'écarte le plus de l'attendu est \"%s\" (résidu = %.2f ; %.0f %% du Khi-deux).",
            worst, resid_p[which.max(abs(resid_p))], contrib[which.max(abs(resid_p))]),
    trf(if (cohen_w < 0.1) "Taille d'effet w de Cohen = %.3f : écart négligeable."
        else if (cohen_w < 0.3) "Taille d'effet w de Cohen = %.3f : écart petit."
        else if (cohen_w < 0.5) "Taille d'effet w de Cohen = %.3f : écart moyen."
        else "Taille d'effet w de Cohen = %.3f : écart grand.", cohen_w),
    if (!expected_ok)
      "Attention : des effectifs attendus sont < 5 ; privilégier la p-value Monte-Carlo ou le test multinomial exact." else NULL)

  # ---- Graphiques (échelles par défaut de ggplot) ----
  plot_obs_att <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    dd <- rbind(data.frame(Modalite = names(tab), Type = "Observé", Effectif = obs),
                data.frame(Modalite = names(tab), Type = "Attendu", Effectif = exp_eff))
    ggplot2::ggplot(dd, ggplot2::aes(Modalite, Effectif, fill = Type)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
      ggplot2::geom_text(ggplot2::aes(label = round(Effectif, 1)),
                         position = ggplot2::position_dodge(width = 0.8),
                         vjust = -0.3, size = 3.2) +
      ggplot2::labs(title = paste0("Observé vs attendu -- ", var_name),
                    x = NULL, y = "Effectif", fill = NULL) +
      ggplot2::theme_minimal(base_size = 12)
  }
  plot_contrib <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    dd <- data.frame(Modalite = names(tab), Contribution = contrib,
                     Sens = ifelse(resid_p >= 0, "Sur-représentée", "Sous-représentée"))
    ggplot2::ggplot(dd, ggplot2::aes(stats::reorder(Modalite, Contribution), Contribution, fill = Sens)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Contribution de chaque modalité au Khi-deux",
                    x = NULL, y = "Contribution (%)", fill = NULL) +
      ggplot2::theme_minimal(base_size = 12)
  }

  # ---- Sortie console R ----
  console <- c(
    utils::capture.output(print(chi)),
    "", "Effectifs attendus :",
    utils::capture.output(print(stats::setNames(round(exp_eff, 4), names(tab)))),
    "", "Résidus de Pearson :",
    utils::capture.output(print(stats::setNames(round(resid_p, 4), names(tab)))),
    if (!is.na(p_mc)) c("", sprintf("Khi-deux Monte-Carlo (B = %d) : p-value = %s", B,
                                    format.pval(p_mc, digits = 4))) else NULL,
    if (method == "multinomial") c("",
      sprintf("Test multinomial exact -- %s", multi_meth),
      sprintf("p-value = %s", format.pval(p_multi, digits = 4)),
      trf("Hypothèse nulle : proportions %s", p0_lab)) else NULL)

  gof_tables <- list("Observé vs attendu" = gof_df)
  if (!is.null(posthoc_df))
    gof_tables[["Post-hoc : comparaisons par paires"]] <- posthoc_df

  # Graphique observé/attendu enrichi des lettres de groupes homogènes
  plot_groups <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE) || is.null(groups_letters)) return(NULL)
    dd <- data.frame(Modalite = names(tab), Observe = obs,
                     Groupe = groups_letters[match(names(tab), names(groups_letters))])
    ggplot2::ggplot(dd, ggplot2::aes(stats::reorder(Modalite, -Observe), Observe, fill = Modalite)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d\n(%s)", Observe, Groupe)),
                         vjust = -0.2, size = 3.6, fontface = "bold") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
      ggplot2::labs(title = paste0("Groupes homogènes -- ", var_name),
                    subtitle = "Modalités partageant une lettre : non significativement différentes",
                    x = NULL, y = "Effectif observé") +
      ggplot2::theme_minimal(base_size = 12)
  }
  gof_plotfns <- list("Observé vs attendu" = plot_obs_att,
                      "Contributions au Khi-deux" = plot_contrib)
  if (!is.null(groups_letters)) gof_plotfns[["Groupes homogènes"]] <- plot_groups

  # Note d'interprétation sur les groupes
  if (!is.null(groups_letters)) {
    n_grp <- length(unique(groups_letters))
    interp <- c(interp,
      trf("Post-hoc (%d comparaisons par paires, ajustement %s) : %d groupe(s) homogène(s) de modalités identifié(s). Les modalités partageant une lettre ne diffèrent pas significativement.",
              nrow(posthoc_df), posthoc_df$Ajustement[1], n_grp))
  }
  if (!is.null(posthoc_df)) {
    console <- c(console, "", "Post-hoc -- comparaisons par paires (khi-deux 2x2) :",
                 utils::capture.output(print(posthoc_df, row.names = FALSE)),
                 if (!is.null(groups_letters)) c("", "Lettres de groupes homogènes :",
                   utils::capture.output(print(groups_letters))) else NULL)
  }

  list(ok = TRUE, metrics = metrics,
       tables = gof_tables,
       plotfns = gof_plotfns,
       interpretation = interp, console = console,
       notes = trf("%d observations, %d modalités, proportions %s.", n, k, p0_lab))
}

# ===========================================================================
# ANALYSE NOMINALE -- deux variables (table de contingence + association)
# ===========================================================================
hstat_q_nominal_bivariate <- function(x, y, xname = "X", yname = "Y") {
  d <- data.frame(x = as.character(x), y = as.character(y), stringsAsFactors = FALSE)
  d <- d[!is.na(d$x) & !is.na(d$y) & nzchar(trimws(d$x)) & nzchar(trimws(d$y)), ]
  if (nrow(d) < 3) return(list(ok = FALSE, notes = "Trop peu de données appariées."))
  tab <- table(d$x, d$y)
  if (nrow(tab) < 2 || ncol(tab) < 2)
    return(list(ok = FALSE, notes = "Chaque variable doit avoir au moins 2 modalités."))

  n <- sum(tab)
  # Khi-deux
  supp_warn <- FALSE
  chi <- tryCatch(suppressWarnings(stats::chisq.test(tab, correct = FALSE)),
                  error = function(e) NULL)
  expected_ok <- if (!is.null(chi)) all(chi$expected >= 5) else NA
  # Test exact de Fisher si petits effectifs et table pas trop grande
  fisher_p <- NA_real_
  if (!isTRUE(expected_ok) && nrow(tab) * ncol(tab) <= 25) {
    fisher_p <- tryCatch(stats::fisher.test(tab, simulate.p.value = TRUE, B = 2000)$p.value,
                         error = function(e) NA_real_)
  }
  # Mesures d'association
  chi2 <- if (!is.null(chi)) unname(chi$statistic) else NA_real_
  ddl <- if (!is.null(chi)) unname(chi$parameter) else NA_real_
  pval <- if (!is.null(chi)) chi$p.value else NA_real_
  phi <- if (!is.na(chi2)) sqrt(chi2 / n) else NA_real_
  cramer_v <- if (!is.na(chi2)) sqrt(chi2 / (n * (min(nrow(tab), ncol(tab)) - 1))) else NA_real_
  # Coefficient de contingence de Pearson
  contingency <- if (!is.na(chi2)) sqrt(chi2 / (chi2 + n)) else NA_real_
  # IC 95 % du V de Cramer (khi-deux non central, methode Smithson)
  v_ci <- if (!is.na(chi2)) hstat_q_cramer_ci(chi2, ddl, n, min(nrow(tab), ncol(tab)))
          else c(NA_real_, NA_real_)

  # ---- Presentation "console R" (khi-deux, attendus, residus, Fisher, V) ----
  chi_print <- tryCatch(suppressWarnings(stats::chisq.test(tab)),  # Yates auto en 2x2
                        error = function(e) NULL)
  fisher_print <- tryCatch(stats::fisher.test(tab), error = function(e)
    tryCatch(stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000),
             error = function(e2) NULL))
  console <- c(
    if (!is.null(chi_print)) utils::capture.output(print(chi_print)),
    "", "Effectifs théoriques (attendus) :",
    if (!is.null(chi_print)) utils::capture.output(print(round(chi_print$expected, 4))),
    "", "Résidus de Pearson (obs - att) / sqrt(att) :",
    if (!is.null(chi_print)) utils::capture.output(print(round(chi_print$residuals, 6))),
    "", "Résidus standardisés (ajustés) :",
    if (!is.null(chi_print)) utils::capture.output(print(round(chi_print$stdres, 4))),
    "",
    if (!is.null(fisher_print)) utils::capture.output(print(fisher_print)),
    "",
    "V de Cramér (IC 95 % par khi-deux non central) :",
    utils::capture.output(print(round(c("Cramer V" = cramer_v,
                                        "lwr.ci" = v_ci[1],
                                        "upr.ci" = v_ci[2]), 7))))

  # Tableaux
  tab_df <- as.data.frame.matrix(tab); tab_df <- cbind(Modalite = rownames(tab_df), tab_df)
  rownames(tab_df) <- NULL
  prop_row <- round(100 * prop.table(tab, 1), 1)
  prop_row_df <- as.data.frame.matrix(prop_row); prop_row_df <- cbind(Modalite = rownames(prop_row_df), prop_row_df)
  rownames(prop_row_df) <- NULL
  # Tableaux croisés complets : profils colonne et pourcentages du total
  prop_col <- round(100 * prop.table(tab, 2), 1)
  prop_col_df <- as.data.frame.matrix(prop_col); prop_col_df <- cbind(Modalite = rownames(prop_col_df), prop_col_df)
  rownames(prop_col_df) <- NULL
  prop_tot <- round(100 * prop.table(tab), 1)
  prop_tot_df <- as.data.frame.matrix(prop_tot); prop_tot_df <- cbind(Modalite = rownames(prop_tot_df), prop_tot_df)
  rownames(prop_tot_df) <- NULL
  resid_df <- NULL
  if (!is.null(chi)) {
    rs <- round(chi$stdres, 2)
    resid_df <- as.data.frame.matrix(rs); resid_df <- cbind(Modalite = rownames(resid_df), resid_df)
    rownames(resid_df) <- NULL
  }

  metrics <- data.frame(
    Metrique = c("Effectif total", "Khi-deux", "Degrés de liberté", "p-value (Khi-deux)",
                 "Conditions de Cochran (>=5)", "p-value (Fisher exact)",
                 "Coefficient phi", "V de Cramér", "IC95% du V de Cramér",
                 "Coefficient de contingence"),
    Valeur = c(n, round(chi2, 3), ddl, format.pval(pval, digits = 3),
               if (isTRUE(expected_ok)) "Respectées" else "Non respectées",
               if (is.na(fisher_p)) "non calculé" else format.pval(fisher_p, digits = 3),
               round(phi, 3), round(cramer_v, 3),
               if (anyNA(v_ci)) "non calculé" else sprintf("[%.3f ; %.3f]", v_ci[1], v_ci[2]),
               round(contingency, 3)),
    Interpretation = c(
      trf("%d observations appariées (paires complètes X, Y).", n),
      "Mesure l'écart global entre effectifs observés et attendus sous indépendance.",
      sprintf("(lignes-1) x (colonnes-1) = %d.", as.integer(ddl)),
      if (!is.na(pval) && pval < 0.05)
        "p < 0,05 : on rejette l'indépendance, les deux variables sont associées."
      else "p >= 0,05 : pas de preuve d'association au seuil de 5 %.",
      if (isTRUE(expected_ok)) "Tous les effectifs attendus >= 5 : le Khi-deux est fiable."
      else "Effectifs attendus < 5 : préférer le test exact de Fisher ci-dessous.",
      if (is.na(fisher_p)) "Non calculé (table trop grande)."
      else if (fisher_p < 0.05) "Test exact : association significative (recommandé si petits effectifs)."
      else "Test exact : pas d'association significative.",
      "Force de l'association pour un tableau 2x2 (équivaut au r de Pearson).",
      trf("Force de l'association : %s (0,1 faible ; 0,2 modérée ; 0,4 forte).",
              if (is.na(cramer_v)) "indéterminée" else if (cramer_v < 0.1) "négligeable"
              else if (cramer_v < 0.2) "faible" else if (cramer_v < 0.4) "modérée"
              else if (cramer_v < 0.6) "relativement forte" else "forte"),
      "Si l'IC exclut ~0, l'association est robuste ; s'il est large, l'estimation est incertaine.",
      "Variante normalisée entre 0 et ~0,71 (2x2) ; plus il est proche de 1, plus l'association est forte."),
    stringsAsFactors = FALSE)

  # Interpretation de la force d'association (V de Cramer)
  force <- if (is.na(cramer_v)) "indéterminée" else if (cramer_v < 0.1) "négligeable" else
    if (cramer_v < 0.2) "faible" else if (cramer_v < 0.4) "modérée" else
    if (cramer_v < 0.6) "relativement forte" else "forte"
  sig <- if (!is.na(pval) && pval < 0.05) "significative" else "non significative"
  interp <- c(
    sprintf("Association %s entre %s et %s (Khi-deux = %.2f, ddl = %d, p = %s).",
            sig, xname, yname, chi2, as.integer(ddl), format.pval(pval, digits = 3)),
    trf("Force de l'association (V de Cramér = %.3f) : %s.", cramer_v, force),
    if (!isTRUE(expected_ok))
      "Certains effectifs théoriques sont < 5 : le test exact de Fisher est plus fiable ici." else
      "Les conditions d'application du Khi-deux sont respectées.",
    "Les résidus standardisés > 2 (ou < -2) signalent les cases qui s'écartent le plus de l'indépendance.")

  plot_grouped <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    # Noms de colonnes internes fixes (.Xvar/.Yvar) : evite l'erreur
    # "duplicate columns" quand les deux variables croisées portent le même nom.
    dd <- as.data.frame(tab); names(dd) <- c(".Xvar", ".Yvar", "Effectif")
    ggplot2::ggplot(dd, ggplot2::aes(x = .data[[".Xvar"]], y = Effectif, fill = .data[[".Yvar"]])) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.85), width = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = Effectif),
                         position = ggplot2::position_dodge(width = 0.85),
                         vjust = -0.3, size = 3.2) +
      ggplot2::labs(title = paste0("Effectifs croisés : ", xname, " x ", yname),
                    x = xname, y = "Effectif", fill = yname) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }
  plot_mosaic <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    dd <- as.data.frame(tab); names(dd) <- c(".Xvar", ".Yvar", "Effectif")
    ggplot2::ggplot(dd, ggplot2::aes(x = .data[[".Xvar"]], y = Effectif, fill = .data[[".Yvar"]])) +
      ggplot2::geom_col(position = "fill") +
      ggplot2::scale_y_continuous(labels = function(z) paste0(z*100, "%")) +
      ggplot2::labs(title = paste0("Profils de ", yname, " selon ", xname),
                    x = xname, y = "Proportion", fill = yname) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }
  # Graphique des PROPORTIONS : profils ligne en barres groupées (% par modalité de X)
  plot_prop <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    pr <- prop.table(tab, 1) * 100     # profils ligne (chaque ligne = 100 %)
    dd <- as.data.frame(pr); names(dd) <- c(".Xvar", ".Yvar", "Pourcentage")
    ggplot2::ggplot(dd, ggplot2::aes(x = .data[[".Xvar"]], y = Pourcentage, fill = .data[[".Yvar"]])) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.85), width = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", Pourcentage)),
                         position = ggplot2::position_dodge(width = 0.85),
                         vjust = -0.3, size = 3) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
      ggplot2::labs(title = paste0("Proportions de ", yname, " selon ", xname),
                    subtitle = "Profils ligne : chaque modalité de X totalise 100 %",
                    x = xname, y = "Pourcentage (%)", fill = yname) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }
  plot_heat <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE) || is.null(chi)) return(NULL)
    rs <- chi$stdres
    dd <- as.data.frame(as.table(rs)); names(dd) <- c(".Xvar", ".Yvar", "Residu")
    ggplot2::ggplot(dd, ggplot2::aes(x = .data[[".Yvar"]], y = .data[[".Xvar"]], fill = Residu)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = round(Residu, 1)), size = 3.2) +
      ggplot2::scale_fill_gradient2(low = "#2980b9", mid = "white", high = "#c0392b", midpoint = 0) +
      ggplot2::labs(title = "Résidus standardisés (écart à l'indépendance)",
                    x = yname, y = xname, fill = "Résidu") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }

  # Tableau des seuils d'interprétation du V de Cramér
  seuils_cramer <- data.frame(
    Intervalle = c("V < 0,10", "0,10 <= V < 0,20", "0,20 <= V < 0,40",
                   "0,40 <= V < 0,60", "V >= 0,60"),
    Interpretation = c("Association négligeable", "Association faible",
                       "Association modérée", "Association relativement forte",
                       "Association forte"),
    stringsAsFactors = FALSE)

  expected_df <- NULL
  if (!is.null(chi)) {
    ex <- round(chi$expected, 2)
    expected_df <- as.data.frame.matrix(ex)
    expected_df <- cbind(Modalite = rownames(as.data.frame.matrix(ex)), expected_df)
    rownames(expected_df) <- NULL
  }
  tables <- list("Table de contingence" = tab_df,
                 "Profils ligne (%)" = prop_row_df,
                 "Profils colonne (%)" = prop_col_df,
                 "Pourcentages du total (%)" = prop_tot_df,
                 "Seuils du V de Cramér" = seuils_cramer)
  if (!is.null(expected_df)) tables[["Effectifs théoriques (attendus)"]] <- expected_df
  if (!is.null(resid_df)) tables[["Résidus standardisés"]] <- resid_df

  list(ok = TRUE, metrics = metrics, tables = tables,
       plotfns = list("Barres groupées (effectifs)" = plot_grouped,
                      "Proportions (% par groupe)" = plot_prop,
                      "Barres empilées (profils)" = plot_mosaic,
                      "Carte des résidus" = plot_heat),
       interpretation = interp,
       console = console,
       notes = trf("%d observations appariées analysées.", n))
}

# ===========================================================================
# CHOIX MULTIPLES -- format binaire (plusieurs colonnes 0/1) ou separe ("A;B")
# ===========================================================================
hstat_q_multiple_choice <- function(df, cols = NULL, sep_col = NULL,
                                     sep = "[;,/|]", var_name = "Choix multiples") {
  # Construit une matrice binaire reponses x options
  if (!is.null(sep_col)) {
    vals <- as.character(df[[sep_col]])
    vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
    split_list <- strsplit(vals, sep)
    split_list <- lapply(split_list, function(z) trimws(z[nzchar(trimws(z))]))
    options <- sort(unique(unlist(split_list)))
    bin <- t(vapply(split_list, function(z) as.integer(options %in% z), integer(length(options))))
    colnames(bin) <- options
    n_resp <- nrow(bin)
  } else if (!is.null(cols)) {
    sub <- df[, cols, drop = FALSE]
    bin <- as.matrix(sapply(sub, function(z) {
      if (is.numeric(z)) as.integer(z > 0)
      else as.integer(!is.na(z) & nzchar(trimws(as.character(z))) &
                        !tolower(trimws(as.character(z))) %in% c("non","no","0","false","faux"))
    }))
    colnames(bin) <- cols
    keep <- rowSums(!is.na(bin)) > 0
    bin <- bin[keep, , drop = FALSE]
    n_resp <- nrow(bin)
  } else {
    return(list(ok = FALSE, notes = "Indiquez soit des colonnes binaires, soit une colonne a valeurs séparées."))
  }
  if (n_resp == 0 || ncol(bin) == 0)
    return(list(ok = FALSE, notes = "Aucune réponse exploitable."))

  counts <- colSums(bin, na.rm = TRUE)
  ord <- order(counts, decreasing = TRUE)
  counts <- counts[ord]
  freq_df <- data.frame(
    Option = names(counts),
    Effectif = as.integer(counts),
    Pct_repondants = round(100 * as.integer(counts) / n_resp, 1),     # % de repondants ayant choisi
    Pct_reponses = round(100 * as.integer(counts) / sum(counts), 1),   # % parmi toutes les citations
    stringsAsFactors = FALSE)

  mean_choices <- round(mean(rowSums(bin, na.rm = TRUE)), 2)
  metrics <- data.frame(
    Metrique = c("Répondants", "Nombre d'options", "Citations totales",
                 "Choix moyen par répondant", "Option la plus citée"),
    Valeur = c(n_resp, ncol(bin), sum(counts), mean_choices, names(counts)[1]),
    stringsAsFactors = FALSE)

  interp <- c(
    trf("L'option la plus citée est \"%s\" (%.1f %% des répondants).", names(counts)[1], freq_df$Pct_repondants[1]),
    trf("En moyenne, chaque répondant a coché %.2f options.", mean_choices),
    "Le %% répondants peut dépasser 100 %% au total car chacun peut choisir plusieurs options.")

  plot_bar <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    d <- freq_df; d$Option <- factor(d$Option, levels = rev(d$Option))
    ggplot2::ggplot(d, ggplot2::aes(x = Option, y = Pct_repondants, fill = Option)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d (%.1f%%)", Effectif, Pct_repondants)), hjust = -0.1, size = 3.3) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
      ggplot2::labs(title = paste0("Choix multiples -- ", var_name),
                    subtitle = "% de répondants ayant choisi chaque option",
                    x = NULL, y = "% répondants") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }
  # Co-occurrences entre options
  cooc <- t(bin) %*% bin
  cooc_df <- as.data.frame.matrix(cooc); cooc_df <- cbind(Option = rownames(cooc_df), cooc_df)
  rownames(cooc_df) <- NULL
  plot_cooc <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    cc <- cooc; diag(cc) <- NA
    dd <- as.data.frame(as.table(cc)); names(dd) <- c("A", "B", "Cooccurrence")
    ggplot2::ggplot(dd, ggplot2::aes(A, B, fill = Cooccurrence)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = Cooccurrence), size = 3, color = "grey70") +
      ggplot2::labs(title = "Co-occurrences entre options", x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }

  list(ok = TRUE, metrics = metrics,
       tables = list("Fréquences des options" = freq_df, "Matrice de co-occurrence" = cooc_df),
       plotfns = list("Barres (% répondants)" = plot_bar, "Co-occurrences" = plot_cooc),
       interpretation = interp,
       notes = trf("%d répondants, format %s.", n_resp,
                       if (!is.null(sep_col)) "valeurs séparées" else "colonnes binaires"))
}

# ===========================================================================
# ANALYSE ORDINALE -- une variable (echelle de Likert / notation)
# ===========================================================================
# Convertit une variable ordinale (facteur ordonne ou texte d'echelle) en rangs
# numeriques selon l'ordre canonique detecte, puis calcule les statistiques.
hstat_q_ordinal_univariate <- function(x, var_name = "Variable", levels_order = NULL) {
  if (is.null(levels_order)) {
    if (is.ordered(x)) levels_order <- levels(x)
    else if (is.numeric(x)) levels_order <- sort(unique(stats::na.omit(x)))
    else levels_order <- hstat_q_ordinal_levels(x)
  }
  xc <- if (is.numeric(x)) as.character(x) else as.character(x)
  xf <- factor(xc, levels = as.character(levels_order), ordered = TRUE)
  xv <- xf[!is.na(xf)]
  n_valid <- length(xv)
  if (n_valid == 0) return(list(ok = FALSE, notes = "Aucune donnée ordinale valide."))
  ranks <- as.integer(xv)              # 1..k selon l'ordre
  k <- length(levels_order)

  # Statistiques de position (sur les rangs)
  med <- stats::median(ranks)
  q1 <- stats::quantile(ranks, 0.25, type = 1); q3 <- stats::quantile(ranks, 0.75, type = 1)
  mode_idx <- which.max(table(ranks)); mode_lab <- levels_order[as.integer(names(mode_idx))]
  med_lab <- levels_order[med]
  mean_score <- mean(ranks)            # score moyen (interpretable si echelle d'intervalle supposee)

  freq <- table(factor(ranks, levels = seq_len(k)))
  freq_df <- data.frame(
    Niveau = seq_len(k),
    Modalite = as.character(levels_order),
    Effectif = as.integer(freq),
    Pourcentage = round(100 * as.integer(freq) / n_valid, 2),
    Pct_cumule = round(cumsum(100 * as.integer(freq) / n_valid), 2),
    stringsAsFactors = FALSE)

  # Dispersion ordinale
  iqr <- q3 - q1
  # Coefficient de variation ordinale / consensus (mesure de Tastle-Wierman approximee
  # par 1 - dispersion normalisee)
  p <- as.integer(freq) / n_valid
  d_levels <- abs(outer(seq_len(k), seq_len(k), "-"))
  dispersion <- sum(outer(p, p) * d_levels) / (k - 1)  # 0 (consensus) a 1 (polarise)
  consensus <- 1 - dispersion

  metrics <- data.frame(
    Metrique = c("Effectif valide", "Nombre de niveaux", "Médiane", "1er quartile (Q1)",
                 "3e quartile (Q3)", "Écart interquartile", "Mode", "Score moyen (rang)",
                 "Dispersion ordinale", "Consensus"),
    Valeur = c(n_valid, k, sprintf("%s (niv. %d)", med_lab, med),
               as.integer(q1), as.integer(q3), as.integer(iqr),
               mode_lab, round(mean_score, 2),
               round(dispersion, 3), round(consensus, 3)),
    stringsAsFactors = FALSE)

  pct_top <- round(100 * sum(ranks >= ceiling((k+1)/2 + 0.5)) / n_valid, 1)
  interp <- c(
    trf("La réponse médiane est \"%s\" (niveau %d sur %d).", med_lab, med, k),
    trf("La moitie centrale des réponses se situe entre \"%s\" et \"%s\".",
            levels_order[as.integer(q1)], levels_order[as.integer(q3)]),
    if (consensus > 0.7) "Fort consensus entre les répondants (réponses peu dispersees)."
    else if (consensus < 0.45) "Réponses polarisées / dispersees : faible consensus."
    else "Consensus modéré entre les répondants.",
    trf("Score moyen = %.2f sur une échelle de 1 a %d.", mean_score, k))

  # Graphiques
  plot_bar <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    d <- freq_df; d$Modalite <- factor(d$Modalite, levels = levels_order)
    ggplot2::ggplot(d, ggplot2::aes(x = Modalite, y = Effectif, fill = Modalite)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d\n%.1f%%", Effectif, Pourcentage)), vjust = -0.2, size = 3.1) +
      ggplot2::scale_fill_brewer(palette = "RdYlGn") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
      ggplot2::labs(title = paste0("Distribution ordinale -- ", var_name), x = NULL, y = "Effectif") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
  }

  list(ok = TRUE, metrics = metrics,
       tables = list("Distribution ordinale" = freq_df),
       plotfns = list("Barres ordonnées" = plot_bar),
       interpretation = interp,
       notes = sprintf("Ordre des niveaux : %s.", paste(levels_order, collapse = " < ")),
       ranks = ranks, levels_order = levels_order)
}

# ===========================================================================
# ANALYSE ORDINALE -- echelle de Likert multi-items (questionnaire)
# ===========================================================================
# items_df : data.frame ou chaque colonne est un item de la meme echelle.
hstat_q_likert_scale <- function(items_df, levels_order = NULL, scale_name = "Échelle") {
  items_df <- as.data.frame(items_df)
  # Conversion de chaque item en rangs numeriques
  to_rank <- function(col) {
    lo <- levels_order %||% (if (is.numeric(col)) sort(unique(stats::na.omit(col))) else hstat_q_ordinal_levels(col))
    as.integer(factor(as.character(col), levels = as.character(lo), ordered = TRUE))
  }
  M <- sapply(items_df, to_rank)
  M <- as.matrix(M)
  keep <- rowSums(is.na(M)) == 0
  Mc <- M[keep, , drop = FALSE]
  n_resp <- nrow(Mc); n_items <- ncol(Mc)
  if (n_resp < 3 || n_items < 2)
    return(list(ok = FALSE, notes = "Au moins 2 items et 3 répondants complets requis."))

  # Alpha de Cronbach (psych si dispo, sinon formule base)
  alpha_val <- NA_real_
  item_stats <- NULL
  if (requireNamespace("psych", quietly = TRUE)) {
    a <- tryCatch(suppressWarnings(psych::alpha(Mc, warnings = FALSE, check.keys = FALSE)),
                  error = function(e) NULL)
    if (!is.null(a)) {
      alpha_val <- unname(a$total$raw_alpha)
      item_stats <- data.frame(
        Item = colnames(Mc),
        Moyenne = round(a$item.stats$mean, 2),
        Correlation_item_total = round(a$item.stats$r.drop, 3),
        Alpha_si_retire = round(a$alpha.drop$raw_alpha, 3),
        stringsAsFactors = FALSE)
    }
  }
  if (is.na(alpha_val)) {
    # formule de Cronbach en base R
    k <- n_items
    var_items <- apply(Mc, 2, stats::var)
    var_total <- stats::var(rowSums(Mc))
    alpha_val <- (k / (k - 1)) * (1 - sum(var_items) / var_total)
    item_stats <- data.frame(Item = colnames(Mc),
                             Moyenne = round(colMeans(Mc), 2),
                             stringsAsFactors = FALSE)
  }

  scores <- rowSums(Mc)
  metrics <- data.frame(
    Metrique = c("Répondants complets", "Nombre d'items", "Alpha de Cronbach",
                 "Score total moyen", "Écart-type du score", "Score min", "Score max"),
    Valeur = c(n_resp, n_items, round(alpha_val, 3),
               round(mean(scores), 2), round(stats::sd(scores), 2),
               min(scores), max(scores)),
    stringsAsFactors = FALSE)

  fiab <- if (is.na(alpha_val)) "indéterminée" else if (alpha_val >= 0.9) "excellente" else
    if (alpha_val >= 0.8) "bonne" else if (alpha_val >= 0.7) "acceptable" else
    if (alpha_val >= 0.6) "discutable" else "insuffisante"
  interp <- c(
    trf("Cohérence interne de l'échelle (alpha de Cronbach = %.3f) : %s.", alpha_val, fiab),
    if (!is.na(alpha_val) && alpha_val < 0.7)
      "Examinez la colonne \"Alpha si retiré\" : un item dont le retrait augmente nettement l'alpha peut être problématique." else
      "L'échelle peut être considérée comme mesurant un construit cohérent.",
    trf("Le score total moyen est de %.1f (somme de %d items).", mean(scores), n_items))

  # Graphique de Likert empile par item
  plot_likert <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    lo <- levels_order %||% seq_len(max(Mc))
    long <- do.call(rbind, lapply(seq_len(ncol(Mc)), function(j) {
      tb <- table(factor(Mc[, j], levels = seq_along(lo)))
      data.frame(Item = colnames(Mc)[j], Niveau = factor(lo, levels = lo),
                 Pct = 100 * as.integer(tb) / sum(tb), stringsAsFactors = FALSE)
    }))
    ggplot2::ggplot(long, ggplot2::aes(x = Item, y = Pct, fill = Niveau)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_brewer(palette = "RdYlGn") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = paste0("Profil de l'échelle -- ", scale_name),
                    x = NULL, y = "Pourcentage", fill = "Niveau") +
      ggplot2::theme_minimal(base_size = 12)
  }
  plot_score <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    ggplot2::ggplot(data.frame(Score = scores), ggplot2::aes(Score)) +
      ggplot2::geom_histogram(bins = min(20, length(unique(scores))), fill = "#2980b9", color = "white") +
      ggplot2::labs(title = "Distribution des scores totaux", x = "Score total", y = "Effectif") +
      ggplot2::theme_minimal(base_size = 12)
  }

  tables <- list("Statistiques par item" = item_stats)
  list(ok = TRUE, metrics = metrics, tables = tables,
       plotfns = list("Profil Likert (empilé)" = plot_likert, "Scores totaux" = plot_score),
       interpretation = interp,
       notes = trf("%d répondants complets sur %d.", n_resp, nrow(M)))
}

# ===========================================================================
# ANALYSE ORDINALE -- comparaisons de groupes & correlations
# ===========================================================================
# Compare une variable ordinale (rangs) entre groupes ou la correle a une autre.
hstat_q_ordinal_compare <- function(ordinal_x, group_or_y, levels_order = NULL,
                                    xname = "Ordinale", gname = "Groupe",
                                    second_ordinal = FALSE, levels_order_y = NULL) {
  # Conversion en rangs
  to_rank <- function(v, lo) {
    if (is.numeric(v)) return(as.numeric(v))
    if (is.null(lo)) lo <- hstat_q_ordinal_levels(v)
    as.integer(factor(as.character(v), levels = as.character(lo), ordered = TRUE))
  }
  rx <- to_rank(ordinal_x, levels_order)

  if (second_ordinal) {
    # Correlation ordinale entre deux variables
    ry <- to_rank(group_or_y, levels_order_y)
    ok_idx <- !is.na(rx) & !is.na(ry)
    rx <- rx[ok_idx]; ry <- ry[ok_idx]
    if (length(rx) < 4) return(list(ok = FALSE, notes = "Trop peu de paires."))
    sp <- suppressWarnings(stats::cor.test(rx, ry, method = "spearman"))
    kd <- suppressWarnings(hstat_kendall_test(rx, ry))
    metrics <- data.frame(
      Metrique = c("Paires valides", "Rho de Spearman", "p-value (Spearman)",
                   "Tau de Kendall", "p-value (Kendall)"),
      Valeur = c(length(rx), round(unname(sp$estimate), 3), format.pval(sp$p.value, 3),
                 round(unname(kd$estimate), 3), format.pval(kd$p.value, 3)),
      stringsAsFactors = FALSE)
    rho <- unname(sp$estimate); pval_sp <- sp$p.value; tau <- unname(kd$estimate)
    force <- if (abs(rho) < 0.2) "très faible" else if (abs(rho) < 0.4) "faible" else
      if (abs(rho) < 0.6) "modérée" else if (abs(rho) < 0.8) "forte" else "très forte"
    sens <- if (rho > 0) "positive (croissante)" else "négative (décroissante)"
    signif_sp <- !is.na(pval_sp) && pval_sp < 0.05
    # Interprétation de la p-value (seuils usuels)
    pval_interp <- if (is.na(pval_sp)) "p-value non calculable."
      else if (pval_sp < 0.001) trf("p = %s < 0,001 : association très hautement significative.", format.pval(pval_sp, 3))
      else if (pval_sp < 0.01)  sprintf("p = %s < 0,01 : association hautement significative.", format.pval(pval_sp, 3))
      else if (pval_sp < 0.05)  sprintf("p = %s < 0,05 : association significative.", format.pval(pval_sp, 3))
      else if (pval_sp < 0.10)  sprintf("p = %s : tendance non significative au seuil de 5 %% (marginale).", format.pval(pval_sp, 3))
      else sprintf("p = %s >= 0,05 : association non significative.", format.pval(pval_sp, 3))
    r2 <- round(100 * rho^2, 1)
    interp <- if (signif_sp) c(
      # --- Cas SIGNIFICATIF ---
      trf("Corrélation ordinale SIGNIFICATIVE entre « %s » et « %s » : rho de Spearman = %.3f (%s, %s).",
              xname, gname, rho, force, sens),
      pval_interp,
      trf("On rejette l'hypothèse d'indépendance : lorsque « %s » augmente, « %s » tend à %s.",
              xname, gname, if (rho > 0) "augmenter" else "diminuer"),
      trf("Le rho² suggère qu'environ %.1f %% de la variation des rangs est partagée entre les deux variables.", r2),
      trf("Le tau de Kendall (%.3f, p = %s) confirme la concordance des rangs ; il est plus robuste pour de petits échantillons.",
              tau, format.pval(kd$p.value, 3)))
    else c(
      # --- Cas NON SIGNIFICATIF ---
      trf("Corrélation ordinale NON significative entre « %s » et « %s » : rho de Spearman = %.3f (%s, %s).",
              xname, gname, rho, force, sens),
      pval_interp,
      trf("On ne peut pas conclure à une association monotone entre « %s » et « %s » : la relation observée (rho = %.3f) est compatible avec le hasard.",
              xname, gname, rho),
      "Absence de significativité ne signifie pas absence de lien : un échantillon plus grand ou une relation non monotone pourraient changer la conclusion.",
      trf("Le tau de Kendall (%.3f, p = %s) va dans le même sens.", tau, format.pval(kd$p.value, 3)))
    console <- c(utils::capture.output(print(sp)), "",
                 utils::capture.output(print(kd)))
    plot_fn <- function() {
      if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
      ggplot2::ggplot(data.frame(x = rx, y = ry), ggplot2::aes(x, y)) +
        ggplot2::geom_jitter(data = function(d) hstat_sample_rows(d, notify = FALSE),
                             width = 0.15, height = 0.15, alpha = 0.5, color = "#2980b9") +
        ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#c0392b") +
        ggplot2::labs(title = sprintf("Relation ordinale : %s vs %s", xname, gname),
                      x = xname, y = gname) +
        ggplot2::theme_minimal(base_size = 12)
    }
    return(list(ok = TRUE, metrics = metrics, tables = list(),
                plotfns = list("Nuage de points (rangs)" = plot_fn),
                interpretation = interp, console = console,
                notes = sprintf("%d paires analysées.", length(rx))))
  }

  # Comparaison entre groupes
  g <- as.factor(as.character(group_or_y))
  ok_idx <- !is.na(rx) & !is.na(g)
  rx <- rx[ok_idx]; g <- droplevels(g[ok_idx])
  ng <- nlevels(g)
  if (ng < 2) return(list(ok = FALSE, notes = "Au moins 2 groupes requis."))

  med_by <- tapply(rx, g, stats::median)
  summary_df <- data.frame(
    Groupe = levels(g),
    Effectif = as.integer(table(g)),
    Mediane_rang = round(as.numeric(med_by), 2),
    Rang_moyen = round(as.numeric(tapply(rank(rx), g, mean)), 1),
    stringsAsFactors = FALSE)

  if (ng == 2) {
    w <- suppressWarnings(stats::wilcox.test(rx ~ g))
    test_name <- "Mann-Whitney (Wilcoxon)"
    stat_val <- unname(w$statistic); pval <- w$p.value
    console <- utils::capture.output(print(w))
  } else {
    kw <- suppressWarnings(stats::kruskal.test(rx ~ g))
    test_name <- "Kruskal-Wallis"
    stat_val <- unname(kw$statistic); pval <- kw$p.value
    console <- utils::capture.output(print(kw))
  }

  # --- Test de la MÉDIANE (Mood) : les groupes ont-ils la même médiane ? ---
  med_global <- stats::median(rx)
  above <- rx > med_global
  med_tab <- table(factor(ifelse(above, "> médiane", "<= médiane"),
                          levels = c("> médiane", "<= médiane")), g)
  mood <- tryCatch(suppressWarnings(stats::chisq.test(med_tab)), error = function(e) NULL)
  mood_p <- if (is.null(mood)) NA_real_ else mood$p.value
  console <- c(console, "", "Test de la médiane (Mood) :",
               if (!is.null(mood)) utils::capture.output(print(mood)) else "non calculable")
  metrics <- data.frame(
    Metrique = c("Test utilisé (rangs)", "Statistique", "p-value",
                 "Test de la médiane (Mood)", "p-value (médiane)",
                 "Médiane globale (rang)", "Nombre de groupes", "Effectif total"),
    Valeur = c(test_name, round(stat_val, 3), format.pval(pval, 3),
               "Khi-deux d'homogénéité des médianes",
               if (is.na(mood_p)) "non calculé" else format.pval(mood_p, 3),
               round(med_global, 2), ng, length(rx)),
    Interpretation = c(
      "Compare les rangs (positions) entre groupes -- adapté aux données ordinales.",
      "Plus la statistique est grande, plus les groupes diffèrent.",
      if (!is.na(pval) && pval < 0.05)
        "p < 0,05 : au moins un groupe diffère significativement (rangs)."
      else "p >= 0,05 : pas de différence significative des rangs.",
      "Compare la proportion d'observations au-dessus de la médiane globale.",
      if (!is.na(mood_p) && mood_p < 0.05)
        "p < 0,05 : les médianes des groupes diffèrent significativement."
      else "p >= 0,05 : médianes compatibles entre groupes.",
      "Valeur de référence pour le test de la médiane.",
      sprintf("%d groupes comparés.", ng),
      sprintf("%d observations valides.", length(rx))),
    stringsAsFactors = FALSE)
  sig <- if (!is.na(pval) && pval < 0.05) "significative" else "non significative"
  pval_txt <- if (is.na(pval)) "non calculable"
    else if (pval < 0.001) trf("p = %s < 0,001 : différence très hautement significative.", format.pval(pval, 3))
    else if (pval < 0.01)  trf("p = %s < 0,01 : différence hautement significative.", format.pval(pval, 3))
    else if (pval < 0.05)  trf("p = %s < 0,05 : différence significative.", format.pval(pval, 3))
    else trf("p = %s >= 0,05 : différence non significative.", format.pval(pval, 3))
  # Groupe aux rangs les plus élevés / faibles
  hi_g <- summary_df$Groupe[which.max(summary_df$Rang_moyen)]
  lo_g <- summary_df$Groupe[which.min(summary_df$Rang_moyen)]
  interp <- c(
    trf("Différence %s de « %s » entre les %d groupes de « %s » (%s).",
            sig, xname, ng, gname, test_name),
    pval_txt,
    if (!is.na(pval) && pval < 0.05)
      trf("Le groupe « %s » présente les rangs les plus élevés, « %s » les plus faibles.", hi_g, lo_g)
    else "Les rangs médians et moyens sont proches d'un groupe à l'autre.",
    trf("Test de la médiane (Mood) : %s",
            if (is.na(mood_p)) "non calculé."
            else if (mood_p < 0.05) "les médianes diffèrent aussi significativement." else "les médianes ne diffèrent pas significativement."),
    "Analyses fondées sur les rangs et les médianes : adaptées aux données ordinales (aucune hypothèse de normalité).",
    if (ng > 2 && !is.na(pval) && pval < 0.05)
      "Un test post-hoc (Dunn) permettrait d'identifier quelles paires de groupes diffèrent." else NULL)

  plot_box <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    ggplot2::ggplot(data.frame(rang = rx, groupe = g), ggplot2::aes(groupe, rang, fill = groupe)) +
      ggplot2::stat_boxplot(geom = "errorbar", width = 0.3, show.legend = FALSE) +
      ggplot2::geom_boxplot(show.legend = FALSE, alpha = 0.7) +
      ggplot2::labs(title = sprintf("%s selon %s", xname, gname), x = gname, y = "Rang ordinal") +
      ggplot2::theme_minimal(base_size = 12)
  }

  list(ok = TRUE, metrics = metrics,
       tables = list("Médianes et rangs par groupe" = summary_df),
       plotfns = list("Boîtes a moustaches (rangs)" = plot_box),
       interpretation = interp,
       console = console,
       notes = trf("Comparaison de %d groupes (rangs et médianes).", ng))
}

# ===========================================================================
# ANALYSE TEXTUELLE -- questions ouvertes
# ===========================================================================
# Liste de mots vides francais (stopwords) integree (pas de dependance reseau).
hstat_q_stopwords_fr <- function() {
  # Liste sans accents (le tokeniseur retire les accents avant comparaison).
  unique(c(
    # articles / determinants
    "le","la","les","un","une","des","du","de","d","l","au","aux","ce","ces",
    "cet","cette","son","sa","ses","mon","ma","mes","ton","ta","tes","notre",
    "nos","votre","vos","leur","leurs","aucun","aucune","chaque","plusieurs",
    "quelque","quelques","tel","telle","tels","telles","certain","certaine",
    "certains","certaines","meme","memes","autre","autres","tout","tous","toute",
    "toutes","nul","nulle",
    # pronoms
    "je","tu","il","elle","on","nous","vous","ils","elles","me","te","se","lui",
    "moi","toi","soi","eux","y","en","ceci","cela","ca","celui","celle","ceux",
    "celles","dont","lequel","laquelle","auquel","duquel","quiconque",
    # conjonctions / liaison
    "et","ou","mais","donc","or","ni","car","que","qui","quoi","comme","si",
    "quand","lorsque","puisque","quoique","tandis","afin","ainsi","alors",
    "aussi","cependant","toutefois","neanmoins","pourtant","enfin","ensuite",
    "puis","aussitot","surtout","notamment","egalement",
    # prepositions
    "a","dans","sur","sous","avec","sans","pour","par","entre","vers","chez",
    "depuis","pendant","avant","apres","jusque","jusqu","selon","malgre","hors",
    "parmi","contre","envers","outre","via","dès","des","concernant",
    # verbes tres frequents (etre, avoir, faire, aller, pouvoir, falloir, dire)
    "est","sont","etre","ete","suis","es","sommes","etes","etait","etaient",
    "sera","seront","serai","serais","serait","soit","soient","fut","furent",
    "avoir","ai","as","a","avons","avez","ont","avais","avait","avaient","aura",
    "auront","eu","eue","aie","ait","ayant",
    "fait","faire","fais","font","faisait","faisais","ferai","ferait",
    "aller","vais","vas","va","allons","allez","vont","allait","ira","iront",
    "pouvoir","peux","peut","pouvons","pouvez","peuvent","pourra","pourrait",
    "pu","puisse","falloir","faut","fallait","faudra","faudrait","fallu",
    "dire","dis","dit","disons","dites","disent","disait","dirai",
    "vouloir","veux","veut","voulons","voulez","veulent","voulait","voudrait",
    "devoir","dois","doit","devons","devez","doivent","devait","devrait",
    "savoir","sais","sait","savons","savez","savent","savait","saurait",
    "voir","vois","voit","voyons","voyez","voient","voyait","verra","vu",
    "prendre","prend","prends","prenons","prennent","mettre","met","mets",
    # adverbes / quantifieurs vides
    "pas","ne","non","plus","moins","tres","trop","peu","bien","mal","assez",
    "beaucoup","tant","autant","aussi","encore","deja","toujours","jamais",
    "souvent","parfois","ici","la","ceans","partout","ailleurs","dehors",
    "dedans","dessus","dessous","devant","derriere","loin","pres","vite",
    "ainsi","comment","pourquoi","combien","oui","voici","voila",
    # divers mots outils courts frequents dans les enquetes
    "qu","c","n","s","j","m","t","car","etc","cad","http","https","www",
    # anglais residuel
    "the","and","of","to","in","is","it","for","an","this","that","with","on",
    "as","are","was","be","at","by","or","from"))
}

# Racinisation (stemming) francaise legere -- suffixes courants, base R, sans
# dependance. Objectif : rapprocher formes flechies d'un meme lemme (ex.
# "moustiquaire"/"moustiquaires", "dorment"/"dormir"/"dort"). Applique le
# suffixe le plus long qui laisse une racine >= 3 lettres.
hstat_q_stem_fr <- function(words) {
  # Chaque suffixe est associe a une longueur de racine MINIMALE : plus le
  # suffixe est ambigu (ex. "ant", "ent" pouvant appartenir a la racine comme
  # dans "enfant"), plus la racine restante exigee est longue. Cela evite le
  # sur-decoupage ("enfant" -> "enf").
  sufs <- list(
    c("issaient",4), c("issantes",4), c("issement",4), c("issements",4),
    c("issant",4), c("issante",4), c("issants",4), c("erions",4),
    c("eraient",4), c("assent",4), c("erent",4), c("aient",4), c("erait",4),
    c("erais",4), c("eront",4), c("ement",5), c("ements",5), c("ations",4),
    c("ation",4), c("atrice",4), c("ateur",4), c("ances",4), c("ance",4),
    c("ences",4), c("ence",4), c("ismes",4), c("isme",4), c("istes",4),
    c("iste",4), c("ables",4), c("able",4), c("ibles",4), c("ible",4),
    c("euses",4), c("euse",4), c("eurs",4), c("elles",5), c("eaux",4),
    c("aux",4), c("ales",4), c("ies",4), c("ees",4), c("ent",5), c("ont",5),
    c("ant",5), c("ait",5), c("ais",5), c("eur",4), c(" ee",4), c("ee",4),
    c("es",4), c("er",4), c("ir",4), c("s",4), c("x",4), c("e",5))
  vapply(words, function(w) {
    if (nchar(w) <= 4) return(w)
    for (sp in sufs) {
      suf <- sp[1]; min_root <- as.integer(sp[2])
      if (grepl(paste0(suf, "$"), w)) {
        root <- substr(w, 1, nchar(w) - nchar(suf))
        if (nchar(root) >= min_root) return(root)
      }
    }
    w
  }, character(1), USE.NAMES = FALSE)
}

# Nettoyage et tokenisation
hstat_q_tokenize <- function(texts, min_char = 3, stopwords = NULL,
                             lowercase = TRUE, remove_numbers = TRUE,
                             stem = FALSE, extra_stopwords = NULL) {
  if (is.null(stopwords)) stopwords <- hstat_q_stopwords_fr()
  if (!is.null(extra_stopwords)) {
    ex <- tolower(trimws(as.character(extra_stopwords)))
    ex <- chartr("\u00e0\u00e2\u00e4\u00e9\u00e8\u00ea\u00eb\u00ee\u00ef\u00f4\u00f6\u00f9\u00fb\u00fc\u00e7",
                 "aaaeeeeiioouuuc", ex)
    stopwords <- unique(c(stopwords, ex[nzchar(ex)]))
  }
  texts <- as.character(texts)
  texts <- texts[!is.na(texts) & nzchar(trimws(texts))]
  clean <- function(s) {
    if (lowercase) s <- tolower(s)
    # 1) Nettoyage : retrait des accents pour un matching robuste
    s <- chartr("\u00e0\u00e2\u00e4\u00e9\u00e8\u00ea\u00eb\u00ee\u00ef\u00f4\u00f6\u00f9\u00fb\u00fc\u00e7",
                "aaaeeeeiioouuuc", s)
    # 2) Retrait ponctuation ; chiffres retires si demande
    s <- if (remove_numbers) gsub("[^a-z ]", " ", s) else gsub("[^a-z0-9 ]", " ", s)
    s <- gsub("\\s+", " ", trimws(s))
    s
  }
  toks <- lapply(texts, function(s) {
    # 3) Tokenisation
    w <- strsplit(clean(s), " ")[[1]]
    # 4) Suppression des stopwords + mots trop courts
    w <- w[nchar(w) >= min_char & !(w %in% stopwords)]
    # 5) Stemming optionnel (regroupe les formes flechies)
    if (stem && length(w) > 0) w <- hstat_q_stem_fr(w)
    w
  })
  toks
}

hstat_q_text_analysis <- function(texts, var_name = "Texte libre", min_char = 3,
                                  top_n = 25, n_gram = 2, n_topics = 3,
                                  stem = FALSE, remove_numbers = TRUE,
                                  extra_stopwords = NULL) {
  texts <- as.character(texts)
  texts <- texts[!is.na(texts) & nzchar(trimws(texts))]
  n_doc <- length(texts)
  if (n_doc < 2) return(list(ok = FALSE, notes = "Trop peu de réponses textuelles."))

  toks <- hstat_q_tokenize(texts, min_char = min_char, stem = stem,
                           remove_numbers = remove_numbers,
                           extra_stopwords = extra_stopwords)
  all_words <- unlist(toks)
  if (length(all_words) == 0) return(list(ok = FALSE, notes = "Aucun mot exploitable après nettoyage."))

  # --- Frequences de mots ---
  wf <- sort(table(all_words), decreasing = TRUE)
  freq_df <- data.frame(Mot = names(wf), Frequence = as.integer(wf),
                        Pct = round(100 * as.integer(wf) / length(all_words), 2),
                        stringsAsFactors = FALSE)
  top_words <- head(freq_df, top_n)

  # --- Longueur des reponses ---
  lens <- vapply(toks, length, integer(1))
  char_lens <- nchar(texts)
  len_df <- data.frame(
    Metrique = c("Réponses", "Mots totaux", "Mots uniques", "Mots/réponse (moyenne)",
                 "Mots/réponse (médiane)", "Caracteres/réponse (moyenne)", "Richesse lexicale (TTR)"),
    Valeur = c(n_doc, length(all_words), length(unique(all_words)),
               round(mean(lens), 1), stats::median(lens), round(mean(char_lens), 1),
               round(length(unique(all_words)) / length(all_words), 3)),
    stringsAsFactors = FALSE)

  # --- N-grammes ---
  ngram_df <- NULL
  if (n_gram >= 2) {
    ngrams <- unlist(lapply(toks, function(w) {
      if (length(w) < n_gram) return(character(0))
      vapply(seq_len(length(w) - n_gram + 1),
             function(i) paste(w[i:(i + n_gram - 1)], collapse = " "), character(1))
    }))
    if (length(ngrams) > 0) {
      ng <- sort(table(ngrams), decreasing = TRUE)
      ngram_df <- data.frame(Expression = names(head(ng, top_n)),
                             Frequence = as.integer(head(ng, top_n)), stringsAsFactors = FALSE)
    }
  }

  # --- Matrice terme-document + TF-IDF ---
  vocab <- names(wf)[wf >= max(2, ceiling(n_doc * 0.02))]   # termes pas trop rares
  vocab <- head(vocab, 120)
  tdm <- sapply(toks, function(w) as.integer(vocab %in% w | vocab %in% unique(w)))
  if (is.null(dim(tdm))) tdm <- matrix(tdm, nrow = length(vocab))
  # comptage reel (frequence du terme dans le doc)
  tf <- sapply(toks, function(w) { tb <- table(w); as.integer(tb[vocab]) })
  tf[is.na(tf)] <- 0L
  if (is.null(dim(tf))) tf <- matrix(tf, nrow = length(vocab))
  rownames(tf) <- vocab
  df_count <- rowSums(tf > 0)
  idf <- log(n_doc / (1 + df_count))
  tfidf <- tf * idf
  tfidf_scores <- sort(rowMeans(tfidf), decreasing = TRUE)
  tfidf_df <- data.frame(Terme = names(head(tfidf_scores, top_n)),
                         Score_TFIDF = round(as.numeric(head(tfidf_scores, top_n)), 3),
                         stringsAsFactors = FALSE)

  # --- Co-occurrences (termes apparaissant ensemble) ---
  binm <- (tf > 0) * 1
  cooc <- binm %*% t(binm)
  diag(cooc) <- 0
  cooc_pairs <- which(upper.tri(cooc) & cooc > 0, arr.ind = TRUE)
  cooc_df <- NULL
  if (nrow(cooc_pairs) > 0) {
    cooc_df <- data.frame(
      Terme_A = vocab[cooc_pairs[, 1]], Terme_B = vocab[cooc_pairs[, 2]],
      Cooccurrences = cooc[cooc_pairs], stringsAsFactors = FALSE)
    cooc_df <- head(cooc_df[order(-cooc_df$Cooccurrences), ], top_n)
  }

  # --- ANALYSE THEMATIQUE (LSA + clustering, base R, sans topicmodels) ---
  themes_df <- NULL; theme_assign <- NULL
  if (length(vocab) >= n_topics * 2 && n_doc >= n_topics * 2) {
    # ponderation TF-IDF, SVD (analyse semantique latente)
    m <- tfidf
    m <- m[rowSums(m) > 0, , drop = FALSE]
    if (nrow(m) >= n_topics && ncol(m) >= n_topics) {
      sv <- tryCatch(svd(m), error = function(e) NULL)
      if (!is.null(sv)) {
        kdim <- min(n_topics + 2, length(sv$d))
        # representation des termes dans l'espace latent
        term_space <- sv$u[, seq_len(kdim), drop = FALSE] * matrix(sv$d[seq_len(kdim)],
                          nrow = nrow(sv$u), ncol = kdim, byrow = TRUE)
        set.seed(123)
        km <- tryCatch(stats::kmeans(term_space, centers = n_topics, nstart = 5),
                       error = function(e) NULL)
        if (!is.null(km)) {
          terms_m <- rownames(m)
          theme_list <- lapply(seq_len(n_topics), function(t) {
            idx <- which(km$cluster == t)
            sc <- rowMeans(m)[idx]
            top <- names(sort(sc, decreasing = TRUE))[seq_len(min(6, length(idx)))]
            paste(top, collapse = ", ")
          })
          themes_df <- data.frame(
            Theme = paste("Thème", seq_len(n_topics)),
            Mots_cles = unlist(theme_list),
            Nb_termes = as.integer(table(factor(km$cluster, levels = seq_len(n_topics)))),
            stringsAsFactors = FALSE)
          # assignation des documents au theme dominant
          doc_space <- t(m) %*% sv$u[, seq_len(kdim), drop = FALSE]
          # score de chaque doc par theme = moyenne des termes du theme presents
          doc_theme <- sapply(seq_len(n_topics), function(t) {
            terms_t <- terms_m[km$cluster == t]
            colSums(m[terms_t, , drop = FALSE])
          })
          if (is.null(dim(doc_theme))) doc_theme <- matrix(doc_theme, ncol = n_topics)
          theme_assign <- apply(doc_theme, 1, which.max)
        }
      }
    }
  }

  # --- SENTIMENT (lexique francais integre simple) ---
  pos_words <- c("bon","bien","excellent","super","genial","parfait","agreable",
                 "satisfait","content","heureux","rapide","efficace","competent",
                 "aimable","professionnel","qualité","recommande","apprecie","merci",
                 "formidable","top","ravi","plaisir","facile","clair","utile","fiable")
  neg_words <- c("mauvais","mal","nul","horrible","decevant","decu","lent","cher",
                 "problème","difficile","complique","incompetent","desagreable",
                 "insatisfait","mecontent","retard","attente","echec","panne","bug",
                 "impossible","jamais","aucun","pire","catastrophe","arnaque","honte")
  sent_scores <- vapply(toks, function(w) {
    sum(w %in% pos_words) - sum(w %in% neg_words)
  }, numeric(1))
  sentiment_label <- ifelse(sent_scores > 0, "Positif",
                     ifelse(sent_scores < 0, "Négatif", "Neutre"))
  sent_tab <- table(factor(sentiment_label, levels = c("Négatif","Neutre","Positif")))
  sentiment_df <- data.frame(
    Tonalite = names(sent_tab), Effectif = as.integer(sent_tab),
    Pourcentage = round(100 * as.integer(sent_tab) / n_doc, 1), stringsAsFactors = FALSE)

  # --- Metriques globales ---
  metrics <- len_df

  # --- Interpretation ---
  interp <- c(
    trf("Les mots les plus fréquents sont : %s.", paste(head(freq_df$Mot, 5), collapse = ", ")),
    sprintf("Richesse lexicale (TTR) = %.3f : %s.",
            length(unique(all_words)) / length(all_words),
            if (length(unique(all_words)) / length(all_words) > 0.6) "vocabulaire varie" else "vocabulaire repetitif / thèmes recurrents"),
    trf("Tonalité dominante : %s (%.0f %% positif, %.0f %% négatif).",
            names(which.max(sent_tab)),
            sentiment_df$Pourcentage[sentiment_df$Tonalite=="Positif"],
            sentiment_df$Pourcentage[sentiment_df$Tonalite=="Négatif"]),
    if (!is.null(themes_df)) c(
      trf("%d thèmes dégagés par analyse sémantique latente (SVD) puis regroupement k-means :", n_topics),
      sprintf("  - Thème %d : %s", seq_len(nrow(themes_df)), themes_df$Mots_cles))
      else "Trop peu de données pour une analyse thématique robuste (corpus insuffisant).")

  # --- Graphiques ---
  plot_freq <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    d <- head(top_words, 20); d$Mot <- factor(d$Mot, levels = rev(d$Mot))
    ggplot2::ggplot(d, ggplot2::aes(Mot, Frequence, fill = Frequence)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = paste0("Mots les plus fréquents -- ", var_name), x = NULL, y = "Fréquence") +
      ggplot2::theme_minimal(base_size = 12)
  }
  plot_wordcloud <- function() {
    # nuage de mots "maison" via ggplot (taille = frequence, positions aleatoires)
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    d <- head(freq_df, 60)
    set.seed(42)
    d$x <- stats::runif(nrow(d)); d$y <- stats::runif(nrow(d))
    ggplot2::ggplot(d, ggplot2::aes(x, y, label = Mot, size = Frequence, color = Frequence)) +
      ggplot2::geom_text(show.legend = FALSE) +
      ggplot2::scale_size_continuous(range = c(3, 12)) +
      ggplot2::labs(title = paste0("Nuage de mots -- ", var_name)) +
      ggplot2::theme_void(base_size = 12)
  }
  plot_sentiment <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    d <- sentiment_df
    ggplot2::ggplot(d, ggplot2::aes(Tonalite, Pourcentage, fill = Tonalite)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d (%.0f%%)", Effectif, Pourcentage)), vjust = -0.3, size = 3.5) +
      ggplot2::labs(title = "Analyse des sentiments", x = NULL, y = "% des réponses") +
      ggplot2::theme_minimal(base_size = 12)
  }

  # --- Recapitulatif des etapes du pipeline NLP (tracabilite methodologique) ---
  pipeline_df <- data.frame(
    Etape = c("1. Tokenisation", "2. Nettoyage",
              "3. Suppression des stopwords", "4. Racinisation (stemming)",
              "5. Vectorisation (DTM / TF-IDF)", "6. Modélisation des thèmes",
              "7. Analyse de sentiment"),
    Detail = c(
      trf("Découpage en mots : %d occurrences sur %d réponses.", length(all_words), n_doc),
      trf("Minuscules, retrait des accents, de la ponctuation%s, mots < %d lettres.",
              if (remove_numbers) " et des chiffres" else "", min_char),
      trf("%d mots vides français retirés (+ %d personnalisés).",
              length(hstat_q_stopwords_fr()),
              length(extra_stopwords %||% character(0))),
      if (stem) "Activée : formes fléchies regroupées à leur racine." else "Désactivée (mots conservés tels quels).",
      trf("Matrice document-terme : %d termes retenus ; pondération TF-IDF.", length(vocab)),
      if (!is.null(themes_df)) trf("Analyse sémantique latente (SVD) + k-means : %d thèmes.", n_topics)
        else "Non réalisée (corpus trop restreint).",
      "Lexique français intégré (mots positifs / négatifs)."),
    stringsAsFactors = FALSE)

  # --- Matrice Document-Terme (extrait : 15 premiers termes) pour consultation ---
  dtm_df <- NULL
  if (exists("tf") && !is.null(dim(tf))) {
    show_terms <- head(vocab, 15)
    dtm_show <- t(tf[show_terms, , drop = FALSE])
    dtm_df <- as.data.frame(dtm_show)
    dtm_df <- cbind(Reponse = paste0("R", seq_len(nrow(dtm_df))), dtm_df)
    rownames(dtm_df) <- NULL
    dtm_df <- head(dtm_df, 40)
  }

  # --- Table d'assignation des reponses aux thèmes ---
  theme_doc_df <- NULL
  if (!is.null(theme_assign)) {
    excerpt <- substr(texts, 1, 70)
    theme_doc_df <- data.frame(
      Reponse = paste0("R", seq_along(theme_assign)),
      Theme_dominant = paste("Thème", theme_assign),
      Extrait = ifelse(nchar(texts) > 70, paste0(excerpt, "..."), excerpt),
      stringsAsFactors = FALSE)
  }

  tables <- list("Fréquences des mots" = top_words,
                 "Statistiques textuelles" = len_df,
                 "Scores TF-IDF" = tfidf_df,
                 "Étapes du pipeline NLP" = pipeline_df,
                 "Répartition des sentiments" = sentiment_df)
  if (!is.null(ngram_df)) tables[[sprintf("Expressions (%d-grammes)", n_gram)]] <- ngram_df
  if (!is.null(cooc_df)) tables[["Co-occurrences de termes"]] <- cooc_df
  if (!is.null(dtm_df)) tables[["Matrice document-terme (extrait)"]] <- dtm_df
  if (!is.null(themes_df)) tables[["Thèmes (analyse thématique)"]] <- themes_df
  if (!is.null(theme_doc_df)) tables[["Réponses par thème"]] <- theme_doc_df

  plot_topics <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE) || is.null(themes_df)) return(NULL)
    d <- themes_df
    d$Theme <- factor(d$Theme, levels = rev(d$Theme))
    d$label <- d$Mots_cles
    ggplot2::ggplot(d, ggplot2::aes(Theme, Nb_termes, fill = Theme)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = label), hjust = 0, size = 3.3, y = 0.1) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.35))) +
      ggplot2::labs(title = paste0("Thèmes identifiés -- ", var_name),
                    subtitle = "Analyse sémantique latente (SVD) + regroupement k-means",
                    x = NULL, y = "Nombre de termes") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }

  plotfns <- list("Mots fréquents" = plot_freq, "Nuage de mots" = plot_wordcloud,
                  "Sentiments" = plot_sentiment)
  if (!is.null(themes_df)) plotfns[["Thèmes"]] <- plot_topics

  list(ok = TRUE, metrics = metrics, tables = tables, plotfns = plotfns,
       interpretation = interp,
       notes = trf("%d réponses textuelles analysées.", n_doc),
       themes = themes_df, theme_assignment = theme_assign)
}


# ===========================================================================
# OUTILS SUPPLEMENTAIRES -- recodage, modalités, valeurs manquantes, OR/RR
# (Ajouts : analyse qualitative HStat)
# ===========================================================================

# ---------------------------------------------------------------------------
# Liste des modalités de plusieurs variables (avec effectifs et pourcentages)
# ---------------------------------------------------------------------------
# Renvoie un tableau long : une ligne par couple (variable, modalité).
hstat_q_list_modalities <- function(df, vars = NULL, max_modalites = 200) {
  df <- as.data.frame(df)
  if (is.null(vars) || length(vars) == 0) vars <- names(df)
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) return(list(ok = FALSE, notes = "Aucune variable valide sélectionnée."))

  rows <- list()
  per_var <- list()
  for (v in vars) {
    x <- df[[v]]
    xc <- as.character(x)
    valides <- xc[!is.na(xc) & nzchar(trimws(xc))]
    n_valide <- length(valides)
    tab <- sort(table(valides), decreasing = TRUE)
    k <- length(tab)
    type_detec <- tryCatch(hstat_q_detect_type(x), error = function(e) "inconnu")
    per_var[[v]] <- data.frame(
      Variable = v, Type = type_detec, Nb_modalites = k,
      Effectif_valide = n_valide, NA_count = sum(is.na(x) | !nzchar(trimws(xc))),
      stringsAsFactors = FALSE)
    if (k == 0) next
    showk <- min(k, max_modalites)
    rows[[v]] <- data.frame(
      Variable = v,
      Modalite = names(tab)[seq_len(showk)],
      Effectif = as.integer(tab)[seq_len(showk)],
      Pourcentage = round(100 * as.integer(tab)[seq_len(showk)] / max(n_valide, 1), 2),
      stringsAsFactors = FALSE)
  }
  modal_df <- if (length(rows)) do.call(rbind, rows) else
    data.frame(Variable = character(), Modalite = character(),
               Effectif = integer(), Pourcentage = numeric(), stringsAsFactors = FALSE)
  rownames(modal_df) <- NULL
  resume_df <- do.call(rbind, per_var); rownames(resume_df) <- NULL

  metrics <- data.frame(
    Metrique = c("Variables analysées", "Total de modalités distinctes",
                 "Modalités par variable (moyenne)"),
    Valeur = c(length(vars), nrow(modal_df),
               round(mean(resume_df$Nb_modalites), 1)),
    stringsAsFactors = FALSE)

  interp <- c(
    trf("%d variable(s) passée(s) en revue, totalisant %d modalités distinctes.",
            length(vars), nrow(modal_df)),
    {
      vmax <- resume_df$Variable[which.max(resume_df$Nb_modalites)]
      trf("La variable la plus diversifiée est \"%s\" (%d modalités).",
              vmax, max(resume_df$Nb_modalites))
    },
    "Une variable à très nombreuses modalités est souvent du texte libre ou un identifiant : envisagez un recodage.")

  plot_fn <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    d <- resume_df[order(resume_df$Nb_modalites, decreasing = TRUE), ]
    d$Variable <- factor(d$Variable, levels = rev(d$Variable))
    ggplot2::ggplot(d, ggplot2::aes(x = Variable, y = Nb_modalites, fill = Variable)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = Nb_modalites), hjust = -0.2, size = 3.4) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
      ggplot2::labs(title = "Nombre de modalités par variable", x = NULL, y = "Modalités distinctes") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }

  list(ok = TRUE, metrics = metrics,
       tables = list("Modalités détaillées" = modal_df, "Résumé par variable" = resume_df),
       plotfns = list("Modalités par variable" = plot_fn),
       interpretation = interp,
       notes = trf("%d variable(s) analysée(s).", length(vars)))
}

# ---------------------------------------------------------------------------
# Comptage des valeurs manquantes (NA) par variable
# ---------------------------------------------------------------------------
# Compte à la fois les NA "vrais" et les chaînes vides / blancs comme manquants.
hstat_q_missing_summary <- function(df, vars = NULL, treat_blank_as_na = TRUE) {
  df <- as.data.frame(df)
  if (is.null(vars) || length(vars) == 0) vars <- names(df)
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) return(list(ok = FALSE, notes = "Aucune variable valide sélectionnée."))
  n <- nrow(df)

  rows <- lapply(vars, function(v) {
    x <- df[[v]]
    na_true <- sum(is.na(x))
    na_blank <- 0L
    if (treat_blank_as_na && !is.numeric(x)) {
      xc <- as.character(x)
      na_blank <- sum(!is.na(xc) & !nzchar(trimws(xc)))
    }
    na_tot <- na_true + na_blank
    data.frame(
      Variable = v,
      NA_reels = na_true,
      Vides_blancs = na_blank,
      Total_manquants = na_tot,
      Pct_manquant = round(100 * na_tot / max(n, 1), 2),
      Effectif_valide = n - na_tot,
      stringsAsFactors = FALSE)
  })
  miss_df <- do.call(rbind, rows); rownames(miss_df) <- NULL
  miss_df <- miss_df[order(miss_df$Total_manquants, decreasing = TRUE), ]

  tot_cells <- n * length(vars)
  tot_na <- sum(miss_df$Total_manquants)
  metrics <- data.frame(
    Metrique = c("Lignes (observations)", "Variables analysées",
                 "Cellules totales", "Cellules manquantes", "Taux global de manquants (%)",
                 "Variables sans aucun manquant", "Variable la plus incomplète"),
    Valeur = c(n, length(vars), tot_cells, tot_na,
               round(100 * tot_na / max(tot_cells, 1), 2),
               sum(miss_df$Total_manquants == 0),
               sprintf("%s (%.1f %%)", miss_df$Variable[1], miss_df$Pct_manquant[1])),
    stringsAsFactors = FALSE)

  pires <- miss_df[miss_df$Pct_manquant >= 20, ]
  interp <- c(
    trf("Le jeu de données contient %.2f %% de cellules manquantes au total.",
            100 * tot_na / max(tot_cells, 1)),
    if (nrow(pires) > 0)
      trf("%d variable(s) dépassent 20 %% de manquants : %s. Un recodage ou une imputation peut être nécessaire.",
              nrow(pires), paste(pires$Variable, collapse = ", "))
    else "Aucune variable ne dépasse 20 %% de manquants : la complétude est satisfaisante.",
    if (treat_blank_as_na)
      "Les chaînes vides et les blancs sont comptés comme manquants (colonne \"Vides_blancs\")."
    else "Seuls les NA stricts sont comptés.")

  plot_fn <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    d <- miss_df
    d$Variable <- factor(d$Variable, levels = rev(d$Variable))
    ggplot2::ggplot(d, ggplot2::aes(x = Variable, y = Pct_manquant, fill = Pct_manquant)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%d (%.1f%%)", Total_manquants, Pct_manquant)),
                         hjust = -0.1, size = 3.2) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.22))) +
      ggplot2::labs(title = "Valeurs manquantes par variable", x = NULL, y = "% manquant") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }

  list(ok = TRUE, metrics = metrics,
       tables = list("Manquants par variable" = miss_df),
       plotfns = list("Carte des manquants" = plot_fn),
       interpretation = interp,
       notes = sprintf("%d variable(s), %d ligne(s).", length(vars), n))
}

# ---------------------------------------------------------------------------
# Odds Ratio (OR) et Risque Relatif (RR) sur tableaux 2x2
# ---------------------------------------------------------------------------
# Pour une table 2x2 :
#                 Issue +      Issue -
#   Exposé +        a            b
#   Exposé -        c            d
# OR = (a*d)/(b*c) ; RR = [a/(a+b)] / [c/(c+d)]
# Correction de Haldane-Anscombe (+0.5) si une case est nulle.
hstat_q_or_rr_2x2 <- function(a, b, c, d, conf = 0.95,
                              expo_pos = "Exposé +", expo_neg = "Exposé -",
                              issue_pos = "Issue +", issue_neg = "Issue -") {
  a <- as.numeric(a); b <- as.numeric(b); c <- as.numeric(c); d <- as.numeric(d)
  zero <- any(c(a, b, c, d) == 0)
  if (zero) { a <- a + 0.5; b <- b + 0.5; c <- c + 0.5; d <- d + 0.5 }
  z <- stats::qnorm(1 - (1 - conf) / 2)

  # Odds Ratio
  or <- (a * d) / (b * c)
  se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
  or_lo <- exp(log(or) - z * se_log_or)
  or_hi <- exp(log(or) + z * se_log_or)

  # Risque Relatif
  r1 <- a / (a + b); r0 <- c / (c + d)
  rr <- r1 / r0
  se_log_rr <- sqrt((1 - r1) / (a) + (1 - r0) / (c))
  rr_lo <- exp(log(rr) - z * se_log_rr)
  rr_hi <- exp(log(rr) + z * se_log_rr)

  list(or = or, or_lo = or_lo, or_hi = or_hi,
       rr = rr, rr_lo = rr_lo, rr_hi = rr_hi,
       risk_expo = r1, risk_nonexpo = r0,
       corrected = zero, conf = conf,
       cells = c(a = a, b = b, c = c, d = d))
}

# Interprétation textuelle d'un OR ou d'un RR (sens + force + signification via IC)
hstat_q_interpret_ratio <- function(value, lo, hi, kind = c("OR", "RR")) {
  kind <- match.arg(kind)
  signif <- !(lo <= 1 && hi >= 1)          # IC excluant 1 => association significative
  sens <- if (value > 1) "augmente" else if (value < 1) "diminue" else "n'affecte pas"
  # Force (échelle indicative)
  vv <- if (value >= 1) value else 1 / value
  force <- if (vv < 1.2) "négligeable" else if (vv < 1.5) "faible" else
    if (vv < 3) "modérée" else if (vv < 5) "forte" else "très forte"
  libelle <- if (kind == "OR") "L'odds ratio" else "Le risque relatif"
  facteur <- if (kind == "OR") "la cote (odds) de l'issue" else "le risque de l'issue"
  txt <- sprintf("%s = %.2f [IC%.0f%% : %.2f - %.2f] : l'exposition %s %s d'un facteur %.2f (association %s).",
                 libelle, value, 100 * 0.95, lo, hi, sens, facteur,
                 if (value >= 1) value else 1 / value, force)
  concl <- if (signif)
    "L'intervalle de confiance n'inclut pas 1 : l'association est statistiquement significative."
  else
    "L'intervalle de confiance inclut 1 : l'association n'est pas statistiquement significative."
  list(text = txt, conclusion = concl, significatif = signif, force = force)
}

# Construit, à partir de deux variables catégorielles, l'ensemble des tableaux 2x2
# par paires de modalités (exposition vs reste OU paire de modalités) et calcule OR/RR.
# mode_expo : "binaire" (chaque modalité de X vs les autres) attendu pour généralisation.
hstat_q_or_rr_analysis <- function(x, y, xname = "X", yname = "Y",
                                   x_expose = NULL, y_issue = NULL, conf = 0.95,
                                   all_pairs = FALSE) {
  d <- data.frame(x = as.character(x), y = as.character(y), stringsAsFactors = FALSE)
  d <- d[!is.na(d$x) & !is.na(d$y) & nzchar(trimws(d$x)) & nzchar(trimws(d$y)), ]
  if (nrow(d) < 4) return(list(ok = FALSE, notes = "Trop peu de données appariées pour OR/RR."))
  lx <- sort(unique(d$x)); ly <- sort(unique(d$y))
  if (length(lx) < 2 || length(ly) < 2)
    return(list(ok = FALSE, notes = "Chaque variable doit avoir au moins 2 modalités."))

  # Cas strictement 2x2 : OR/RR direct
  est_2x2 <- (length(lx) == 2 && length(ly) == 2)

  # Détermine la modalité "exposé +" et "issue +"
  xp <- x_expose %||% lx[1]
  yp <- y_issue %||% ly[1]

  rows <- list()
  build_one <- function(expo_lab, issue_lab) {
    expo <- d$x == expo_lab
    issue <- d$y == issue_lab
    a <- sum(expo & issue); b <- sum(expo & !issue)
    cc <- sum(!expo & issue); dd <- sum(!expo & !issue)
    rr <- hstat_q_or_rr_2x2(a, b, cc, dd, conf = conf)
    # Interpretation compacte, par ligne : sens + signification (IC vs 1)
    or_sig <- !(rr$or_lo <= 1 && rr$or_hi >= 1)
    sens <- if (rr$or > 1) "facteur de risque" else if (rr$or < 1) "facteur protecteur" else "sans effet"
    interp_line <- if (or_sig)
      trf("%s significatif (OR = %.2f) : être \"%s\" %s la cote de \"%s = %s\".",
              tools::toTitleCase(sens), rr$or, expo_lab,
              if (rr$or > 1) "augmente" else "diminue", yname, issue_lab)
    else
      sprintf("Pas d'association significative (OR = %.2f, IC contient 1) entre \"%s\" et \"%s = %s\".",
              rr$or, expo_lab, yname, issue_lab)
    data.frame(
      Exposition = sprintf("%s = %s (vs reste)", xname, expo_lab),
      Issue = sprintf("%s = %s", yname, issue_lab),
      a = a, b = b, c = cc, d = dd,
      OR = round(rr$or, 3), OR_IC_bas = round(rr$or_lo, 3), OR_IC_haut = round(rr$or_hi, 3),
      RR = round(rr$rr, 3), RR_IC_bas = round(rr$rr_lo, 3), RR_IC_haut = round(rr$rr_hi, 3),
      Risque_expose = round(rr$risk_expo, 3), Risque_non_expose = round(rr$risk_nonexpo, 3),
      Correction = ifelse(rr$corrected, "Haldane (+0,5)", "-"),
      Interpretation = interp_line,
      stringsAsFactors = FALSE)
  }

  if (est_2x2 && !all_pairs) {
    rows[[1]] <- build_one(xp, yp)
    note <- "Tableau strictement 2x2."
  } else if (all_pairs) {
    # TOUTES les modalites : chaque modalite de X (vs reste) croisee avec
    # CHAQUE modalite de Y (vs reste). Couvre l'ensemble des combinaisons.
    for (xl in lx) for (yl in ly) rows[[length(rows) + 1]] <- build_one(xl, yl)
    note <- trf("Toutes les modalités : %d exposition(s) x %d issue(s) = %d tableaux 2x2.",
                    length(lx), length(ly), length(lx) * length(ly))
  } else {
    # Une issue fixee : chaque modalite de X (exposé vs reste)
    for (xl in lx) rows[[length(rows) + 1]] <- build_one(xl, yp)
    note <- trf("Généralisation par paires : %d exposition(s) vs issue \"%s\".", length(lx), yp)
  }
  res_df <- do.call(rbind, rows); rownames(res_df) <- NULL

  # Métriques + interprétation sur la (première) ligne de référence
  ref <- res_df[1, ]
  int_or <- hstat_q_interpret_ratio(ref$OR, ref$OR_IC_bas, ref$OR_IC_haut, "OR")
  int_rr <- hstat_q_interpret_ratio(ref$RR, ref$RR_IC_bas, ref$RR_IC_haut, "RR")

  metrics <- data.frame(
    Metrique = c("Exposition de référence", "Issue de référence",
                 "Odds Ratio (OR)", sprintf("IC%.0f%% de l'OR", 100 * conf),
                 "Risque Relatif (RR)", sprintf("IC%.0f%% du RR", 100 * conf),
                 "Risque chez exposés", "Risque chez non-exposés",
                 "Réduction/augmentation du risque (%)"),
    Valeur = c(ref$Exposition, ref$Issue,
               sprintf("%.3f", ref$OR), sprintf("[%.3f ; %.3f]", ref$OR_IC_bas, ref$OR_IC_haut),
               sprintf("%.3f", ref$RR), sprintf("[%.3f ; %.3f]", ref$RR_IC_bas, ref$RR_IC_haut),
               sprintf("%.1f %%", 100 * ref$Risque_expose),
               sprintf("%.1f %%", 100 * ref$Risque_non_expose),
               sprintf("%+.1f %%", 100 * (ref$RR - 1))),
    stringsAsFactors = FALSE)

  seuils_df <- data.frame(
    Indicateur = c("OR / RR = 1", "OR / RR > 1", "OR / RR < 1",
                   "Rapport 1,0-1,2", "Rapport 1,2-1,5", "Rapport 1,5-3",
                   "Rapport 3-5", "Rapport > 5", "IC incluant 1"),
    Interpretation = c("Aucune association (référence)",
                       "Facteur de risque (l'exposition augmente l'issue)",
                       "Facteur protecteur (l'exposition diminue l'issue)",
                       "Association négligeable", "Association faible",
                       "Association modérée", "Association forte", "Association très forte",
                       "Association non significative"),
    stringsAsFactors = FALSE)

  # Comptage des associations significatives (IC de l'OR excluant 1)
  sig_mask <- !(res_df$OR_IC_bas <= 1 & res_df$OR_IC_haut >= 1)
  n_sig <- sum(sig_mask)
  interp <- c(
    if (nrow(res_df) > 1)
      trf("Analyse de %d combinaison(s) de modalités : %d présente(nt) une association significative (IC de l'OR excluant 1), %d non significative(s).",
              nrow(res_df), n_sig, nrow(res_df) - n_sig)
    else NULL,
    # Une puce par combinaison (limitee a 12 pour rester lisible)
    utils::head(sprintf("%s vs %s -- %s", res_df$Exposition, res_df$Issue,
                        res_df$Interpretation), 12),
    if (nrow(res_df) > 12)
      trf("... (%d autres combinaisons dans le tableau détaillé).", nrow(res_df) - 12)
    else NULL,
    int_or$text, int_or$conclusion, int_rr$text, int_rr$conclusion,
    "Le RR s'interprète directement en termes de risque (cohortes) ; l'OR est préféré pour les études cas-témoins.",
    if (any(res_df$Correction != "-"))
      "Une correction de Haldane-Anscombe (+0,5) a été appliquée aux tableaux comportant une case nulle." else NULL)

  # ---- Presentation "console R" style epitools::oddsratio / riskratio ----
  # Table X x (non-issue, issue) : 1re modalite de X = reference.
  issues <- if (all_pairs) ly else yp
  console <- unlist(lapply(issues, function(iss) {
    y_bin <- ifelse(d$y == iss, iss, if (length(ly) == 2) setdiff(ly, iss) else "Autres")
    y_lvl <- c(setdiff(unique(y_bin), iss), iss)      # issue en 2e colonne
    tab2 <- table(factor(d$x, levels = lx), factor(y_bin, levels = y_lvl))
    c(sprintf("== ODDS RATIO -- reference : %s = %s ; issue : %s = %s ==", xname, lx[1], yname, iss),
      "", hstat_q_epitools_block(tab2, "OR", conf), "",
      sprintf("== RISQUE RELATIF -- reference : %s = %s ; issue : %s = %s ==", xname, lx[1], yname, iss),
      "", hstat_q_epitools_block(tab2, "RR", conf), "", "")
  }))

  # Forest plot OR & RR
  plot_forest <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    lab <- if (all_pairs) paste0(res_df$Exposition, "  |  ", res_df$Issue) else res_df$Exposition
    fd <- rbind(
      data.frame(Mesure = "OR", Groupe = lab, Issue = res_df$Issue,
                 est = res_df$OR, lo = res_df$OR_IC_bas, hi = res_df$OR_IC_haut),
      data.frame(Mesure = "RR", Groupe = lab, Issue = res_df$Issue,
                 est = res_df$RR, lo = res_df$RR_IC_bas, hi = res_df$RR_IC_haut))
    fd$Groupe <- factor(fd$Groupe, levels = rev(unique(lab)))
    g <- ggplot2::ggplot(fd, ggplot2::aes(x = est, y = Groupe, color = Mesure)) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5), size = 2.6) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi),
                              position = ggplot2::position_dodge(width = 0.5), height = 0.2) +
      ggplot2::scale_x_log10() +
      ggplot2::labs(title = "Forest plot -- OR et RR (échelle log)",
                    subtitle = if (all_pairs) "Toutes les combinaisons de modalités"
                               else sprintf("Issue : %s = %s", yname, yp),
                    x = "Rapport (référence = 1)", y = NULL, color = NULL) +
      ggplot2::theme_minimal(base_size = 12)
    g
  }

  list(ok = TRUE, metrics = metrics,
       tables = list("OR / RR par paire" = res_df, "Seuils d'interprétation" = seuils_df),
       plotfns = list("Forest plot (OR & RR)" = plot_forest),
       interpretation = interp,
       console = console,
       notes = note)
}

# ===========================================================================
# INTERFACE UTILISATEUR (UI)
# ===========================================================================
mod_qualitative_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = "qualitative",
    shiny::fluidRow(
      shinydashboard::box(width = 12, status = "warning", solidHeader = FALSE, background = "navy",
        shiny::h3(shiny::icon("comments"), " Analyses de données qualitatives d'enquête",
                  style = "margin:0;color:white;"),
        # Le selecteur de famille vit dans le bandeau (et non dans la boite de
        # configuration) : l'atelier de codage occupe toute la largeur, il ne
        # peut donc pas heberger ce selecteur dans une colonne de gauche.
        shiny::div(style = "max-width:560px;margin-top:12px;",
          shiny::selectInput(ns("family"), "Famille d'analyse",
            choices = c("Nominale (catégories)" = "nominal",
                        "Ordinale (échelles, Likert)" = "ordinal",
                        "Textuelle (questions ouvertes)" = "textual",
                        "Codage / thématisation (CAQDAS, façon MAXQDA)" = "coding",
                        "Outils (modalités, manquants, recodage, OR/RR)" = "tools"),
            selected = "nominal")))
    ),

    # ---- Atelier de codage qualitatif (CAQDAS) : voir mod_coding.R ----
    shiny::conditionalPanel(sprintf("input['%s'] == 'coding'", ns("family")),
      mod_coding_ui(ns("coding"))),

    shiny::conditionalPanel(sprintf("input['%s'] != 'coding'", ns("family")),
    shiny::fluidRow(
      shiny::column(4,
        shinydashboard::box(width = 12, title = shiny::tagList(shiny::icon("sliders-h"), " Configuration"),
                   status = "warning", solidHeader = TRUE,

          # ---- NOMINALE ----
          shiny::conditionalPanel(sprintf("input['%s'] == 'nominal'", ns("family")),
            shiny::radioButtons(ns("nom_mode"), "Type d'analyse",
              choices = c("Une variable" = "uni",
                          "Test Chi² / Multinomial (adéquation)" = "gof",
                          "Tableaux croisés (2 variables)" = "bi",
                          "Choix multiples" = "multi"),
              selected = "uni"),
            shiny::conditionalPanel(sprintf("input['%s'] != 'multi'", ns("nom_mode")),
              shiny::uiOutput(ns("nom_var1_ui"))),
            shiny::conditionalPanel(sprintf("input['%s'] == 'bi'", ns("nom_mode")),
              shiny::uiOutput(ns("nom_var2_ui"))),
            shiny::conditionalPanel(sprintf("input['%s'] == 'gof'", ns("nom_mode")),
              shiny::div(style = "background:#eef7ee;border-left:3px solid #27ae60;padding:6px 10px;margin-bottom:8px;font-size:12px;",
                shiny::icon("info-circle"),
                " La variable ci-dessus est la variable analysée. Vous pouvez, en option, choisir un facteur X pour tester sa distribution séparément dans chaque groupe."),
              shiny::checkboxInput(ns("gof_use_x"),
                shiny::tagList(shiny::strong("Analyser selon un facteur X (analyse stratifiée)")), value = FALSE),
              shiny::conditionalPanel(sprintf("input['%s'] == true", ns("gof_use_x")),
                shiny::uiOutput(ns("gof_x_ui"))),
              shiny::radioButtons(ns("gof_method"), "Méthode",
                choiceNames = list(
                  shiny::HTML("<b>Chi² d'ajustement</b> <small style='color:#7f8c8d;'>(chisq.test)</small>"),
                  shiny::HTML("<b>Multinomial exact</b> <small style='color:#7f8c8d;'>(petits effectifs)</small>")),
                choiceValues = list("chisq", "multinomial"),
                selected = "chisq"),
              shiny::radioButtons(ns("gof_props"), "Proportions attendues (H0)",
                choices = c("Équiprobables" = "equal",
                            "Personnalisées" = "custom"),
                selected = "equal"),
              shiny::conditionalPanel(sprintf("input['%s'] == 'custom'", ns("gof_props")),
                shiny::textInput(ns("gof_props_txt"),
                  "Proportions (une par modalité, ordre alphabétique, séparées par virgule)",
                  placeholder = "ex. 0.5, 0.3, 0.2"),
                shiny::uiOutput(ns("gof_levels_hint"))),
              shiny::selectInput(ns("gof_posthoc_adjust"),
                shiny::tagList(shiny::icon("code-branch"), " Post-hoc : ajustement des p-values (groupes homogènes)"),
                choices = c("Bonferroni" = "bonferroni", "Holm" = "holm",
                            "BH (FDR)" = "BH", "BY" = "BY", "Hochberg" = "hochberg",
                            "Hommel" = "hommel", "Aucun" = "none"),
                selected = "bonferroni"),
              shiny::tags$small(style = "color:#7f8c8d;", shiny::icon("info-circle"),
                " Compare chaque paire de modalités (khi-deux 2x2) et regroupe en lettres : les modalités partageant une lettre ne diffèrent pas significativement.")),
            shiny::conditionalPanel(sprintf("input['%s'] == 'multi'", ns("nom_mode")),
              shiny::radioButtons(ns("multi_fmt"), "Format des choix multiples",
                choices = c("Colonnes binaires (0/1)" = "binary",
                            "Une colonne (valeurs séparées)" = "sep"),
                selected = "binary"),
              shiny::conditionalPanel(sprintf("input['%s'] == 'binary'", ns("multi_fmt")),
                shiny::uiOutput(ns("multi_cols_ui"))),
              shiny::conditionalPanel(sprintf("input['%s'] == 'sep'", ns("multi_fmt")),
                shiny::uiOutput(ns("multi_sepcol_ui")),
                shiny::textInput(ns("multi_sep"), "Séparateur (regex)", value = "[;,/|]")))),

          # ---- ORDINALE ----
          shiny::conditionalPanel(sprintf("input['%s'] == 'ordinal'", ns("family")),
            shiny::radioButtons(ns("ord_mode"), "Type d'analyse",
              choices = c("Une variable" = "uni",
                          "Échelle de Likert (multi-items)" = "likert",
                          "Comparaison entre groupes" = "compare",
                          "Corrélation (2 ordinales)" = "corr"),
              selected = "uni"),
            shiny::conditionalPanel(sprintf("input['%s'] == 'uni' || input['%s'] == 'compare' || input['%s'] == 'corr'", ns("ord_mode"), ns("ord_mode"), ns("ord_mode")),
              shiny::uiOutput(ns("ord_var1_ui"))),
            shiny::conditionalPanel(sprintf("input['%s'] == 'compare'", ns("ord_mode")),
              shiny::uiOutput(ns("ord_group_ui"))),
            shiny::conditionalPanel(sprintf("input['%s'] == 'corr'", ns("ord_mode")),
              shiny::uiOutput(ns("ord_var2_ui"))),
            shiny::conditionalPanel(sprintf("input['%s'] == 'likert'", ns("ord_mode")),
              shiny::uiOutput(ns("ord_items_ui"))),
            shiny::uiOutput(ns("ord_levels_ui"))),

          # ---- TEXTUELLE ----
          shiny::conditionalPanel(sprintf("input['%s'] == 'textual'", ns("family")),
            shiny::uiOutput(ns("txt_var_ui")),
            shiny::sliderInput(ns("txt_topn"), "Nombre d'éléments (top N)", min = 5, max = 50, value = 20, step = 5),
            shiny::sliderInput(ns("txt_ngram"), "Taille des n-grammes", min = 2, max = 4, value = 2, step = 1),
            shiny::sliderInput(ns("txt_topics"), "Nombre de thèmes", min = 2, max = 8, value = 3, step = 1),
            shiny::sliderInput(ns("txt_minchar"), "Longueur min. des mots", min = 2, max = 6, value = 3, step = 1),
            shiny::checkboxInput(ns("txt_stem"),
              shiny::tagList(shiny::strong("Racinisation (stemming)"),
                shiny::br(), shiny::tags$small(style = "color:#7f8c8d;",
                  "Regroupe les formes fléchies d'un même mot (ex. moustiquaire / moustiquaires).")),
              value = TRUE),
            shiny::checkboxInput(ns("txt_nonum"), "Ignorer les chiffres", value = TRUE),
            shiny::textAreaInput(ns("txt_stopwords"),
              "Mots à exclure (personnalisés, séparés par virgule ou espace)",
              value = "", rows = 2,
              placeholder = "ex. enquete, question, reponse")),

          # ---- OUTILS (modalités, manquants, OR/RR) ----
          shiny::conditionalPanel(sprintf("input['%s'] == 'tools'", ns("family")),
            shiny::radioButtons(ns("tools_mode"), "Outil",
              choices = c("Lister les modalités" = "modal",
                          "Compter les valeurs manquantes (NA)" = "miss",
                          "Odds Ratio / Risque Relatif" = "orrr"),
              selected = "modal"),

            # --- Modalités / Manquants : sélection multi-variables ---
            shiny::conditionalPanel(
              sprintf("input['%s'] == 'modal' || input['%s'] == 'miss'", ns("tools_mode"), ns("tools_mode")),
              shiny::uiOutput(ns("tools_vars_ui")),
              shiny::conditionalPanel(sprintf("input['%s'] == 'miss'", ns("tools_mode")),
                shiny::checkboxInput(ns("miss_blank"), "Compter aussi les chaînes vides / blancs comme manquants", value = TRUE))),

            # --- OR / RR ---
            shiny::conditionalPanel(sprintf("input['%s'] == 'orrr'", ns("tools_mode")),
              shiny::uiOutput(ns("orrr_expo_ui")),
              shiny::uiOutput(ns("orrr_issue_ui")),
              shiny::uiOutput(ns("orrr_expo_lvl_ui")),
              shiny::uiOutput(ns("orrr_issue_lvl_ui")),
              shiny::checkboxInput(ns("orrr_all_pairs"),
                shiny::tagList(shiny::strong("Prendre en compte toutes les modalités"),
                  shiny::br(),
                  shiny::tags$small(style = "color:#7f8c8d;",
                    "Croise chaque modalité de X (vs reste) avec chaque modalité de Y (vs reste). Les choix « exposé + » / « issue + » ci-dessus sont alors ignorés.")),
                value = FALSE),
              shiny::sliderInput(ns("orrr_conf"), "Niveau de confiance", min = 0.80, max = 0.99, value = 0.95, step = 0.01))),

          shiny::hr(),
          shiny::actionButton(ns("run"), "Lancer l'analyse", icon = shiny::icon("play"),
                              class = "btn-warning btn-block"),
          shiny::br(),
          shiny::uiOutput(ns("type_hint"))
        )
      ),
      shiny::column(8,
        shinydashboard::box(width = 12, title = shiny::tagList(shiny::icon("chart-pie"), " Résultats"),
                   status = "warning", solidHeader = TRUE,
          shiny::uiOutput(ns("result_note")),
          shiny::tabsetPanel(id = ns("resultTabs"),
            shiny::tabPanel(shiny::tagList(shiny::icon("chart-bar"), " Graphiques"), shiny::br(),
              shiny::fluidRow(
                shiny::column(6, shiny::uiOutput(ns("graph_selector"))),
                shiny::column(6,
                  shiny::selectInput(ns("plot_palette"), "Palette de couleurs",
                    choices = c("Par défaut (du graphique)" = "default",
                                "Bleu" = "blues", "Vert" = "greens",
                                "Orange-Rouge" = "oranges", "Violet" = "purples",
                                "Viridis" = "viridis", "Spectral" = "spectral",
                                "Niveaux de gris" = "greys",
                                "Personnalisée (2 couleurs)" = "custom"),
                    selected = "default"))),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'custom'", ns("plot_palette")),
                shiny::fluidRow(
                  shiny::column(3, colourInput(ns("plot_col_low"), "Couleur basse / 1re", value = "#aed6f1")),
                  shiny::column(3, colourInput(ns("plot_col_high"), "Couleur haute / 2e", value = "#1f618d")))),
              shiny::plotOutput(ns("main_plot"), height = "440px"),
              shiny::br(),
              # ---- Mise en forme interactive du graphique (rapatriée des Tableaux croisés) ----
              shiny::tags$details(style = "background:#fbfbfd;border:1px solid #e0e0e0;border-radius:6px;padding:10px 14px;margin-bottom:10px;",
                shiny::tags$summary(style = "cursor:pointer;font-weight:bold;color:#8e44ad;",
                  shiny::icon("sliders-h"), " Personnaliser le graphique (mise à jour instantanée)"),
                shiny::br(),
                shiny::fluidRow(
                  shiny::column(3,
                    shiny::tags$strong(shiny::icon("heading"), " Titres"),
                    shiny::textInput(ns("sty_title"), "Titre", placeholder = "Automatique si vide"),
                    shiny::textInput(ns("sty_xlab"), "Axe X", placeholder = "Automatique si vide"),
                    shiny::textInput(ns("sty_ylab"), "Axe Y", placeholder = "Automatique si vide"),
                    shiny::textInput(ns("sty_legend"), "Légende", placeholder = "Automatique si vide")),
                  shiny::column(3,
                    shiny::tags$strong(shiny::icon("font"), " Tailles (pt)"),
                    shiny::sliderInput(ns("sty_title_size"), "Titre", min = 8, max = 32, value = 16, step = 1),
                    shiny::sliderInput(ns("sty_axis_label_size"), "Titres des axes", min = 6, max = 24, value = 12, step = 1),
                    hstat_axe_titre_ui(ns, "sty_axis"),
                    shiny::sliderInput(ns("sty_axis_text_size"), "Graduations", min = 5, max = 20, value = 10, step = 1),
                    shiny::sliderInput(ns("sty_legend_text_size"), "Légende", min = 5, max = 20, value = 10, step = 1)),
                  shiny::column(3,
                    shiny::tags$strong(shiny::icon("bold"), " Style du texte"),
                    shiny::tags$small(shiny::tags$b(" Titres des axes")),
                    shiny::checkboxInput(ns("sty_axis_title_bold"), "Gras", value = TRUE),
                    shiny::checkboxInput(ns("sty_axis_title_italic"), "Italique", value = FALSE),
                    shiny::tags$small(shiny::tags$b(" Graduations")),
                    shiny::checkboxInput(ns("sty_axis_text_bold"), "Gras", value = FALSE),
                    shiny::checkboxInput(ns("sty_axis_text_italic"), "Italique", value = FALSE)),
                  shiny::column(3,
                    shiny::tags$strong(shiny::icon("adjust"), " Affichage"),
                    shiny::sliderInput(ns("sty_x_rotation"), "Rotation axe X (°)", min = 0, max = 90, value = 45, step = 5),
                    shiny::checkboxInput(ns("sty_show_grid"), "Grille de fond", value = TRUE),
                    shiny::checkboxInput(ns("sty_black_axes"), "Axes en noir", value = FALSE),
                    shiny::checkboxInput(ns("sty_black_ticks"), "Graduations en noir", value = FALSE)))),
              # Paramètres d'exportation de l'image (rapatriés des Tableaux croisés)
              shiny::div(style = "background:#fff8f0;border-left:4px solid #e67e22;padding:12px;border-radius:6px;",
                shiny::tags$strong(shiny::icon("image"), " Paramètres d'exportation de l'image"),
                shiny::fluidRow(
                  shiny::column(3, hstat_format_input(ns("dl_format"), "Format")),
                  shiny::column(3, shiny::numericInput(ns("dl_width"), "Largeur (po)", value = 9, min = 3, max = 30, step = 0.5)),
                  shiny::column(3, shiny::numericInput(ns("dl_height"), "Hauteur (po)", value = 6, min = 2, max = 24, step = 0.5)),
                  shiny::column(3, hstat_dpi_input(ns("dl_dpi"), "Résolution (DPI)"))),
                shiny::tags$small(class = "text-muted", shiny::icon("info-circle"),
                  " PDF et SVG sont vectoriels (redimensionnables sans perte) ; le DPI ne s'y applique pas.")),
              shiny::br(),
              shiny::downloadButton(ns("dl_plot"), "Télécharger le graphique",
                                    class = "btn-success")),
            shiny::tabPanel(shiny::tagList(shiny::icon("list-ol"), " Métriques"), shiny::br(),
              DT::DTOutput(ns("metrics_table")),
              shiny::br(),
              shiny::div(class = "callout callout-info",
                shiny::strong(shiny::icon("lightbulb"), " Interprétation"),
                shiny::uiOutput(ns("interpretation")))),
            shiny::tabPanel(shiny::tagList(shiny::icon("table"), " Details (tableaux)"), shiny::br(),
              shiny::uiOutput(ns("tables_selector")),
              DT::DTOutput(ns("detail_table")),
              shiny::br(),
              shiny::div(style = "background:#eaf4fb;border-left:4px solid #2e86c1;padding:12px;border-radius:6px;",
                shiny::tags$strong(shiny::icon("file-export"), " Exportation des résultats"),
                shiny::br(), shiny::br(),
                shiny::fluidRow(
                  shiny::column(6, shiny::tags$small(shiny::tags$b("Tableau affiché :"))),
                  shiny::column(6, shiny::tags$small(shiny::tags$b("Tous les tableaux :")))),
                shiny::fluidRow(
                  shiny::column(3, shiny::downloadButton(ns("dl_table"), "CSV", class = "btn-info btn-sm btn-block", icon = shiny::icon("file-csv"))),
                  shiny::column(3, shiny::downloadButton(ns("dl_table_xlsx"), "Excel", class = "btn-success btn-sm btn-block", icon = shiny::icon("file-excel"))),
                  shiny::column(3, shiny::downloadButton(ns("dl_all_csv"), "CSV (ZIP)", class = "btn-info btn-sm btn-block", icon = shiny::icon("file-archive"))),
                  shiny::column(3, shiny::downloadButton(ns("dl_all_xlsx"), "Excel", class = "btn-success btn-sm btn-block", icon = shiny::icon("file-excel"))))
              )),
            shiny::tabPanel(shiny::tagList(shiny::icon("terminal"), " Tests (sortie R)"), shiny::br(),
              shiny::uiOutput(ns("console_note")),
              shiny::verbatimTextOutput(ns("console_out")),
              shiny::br(),
              shiny::downloadButton(ns("dl_console"), "Télécharger la sortie (TXT)",
                                    class = "btn-success btn-sm"))
          )
        )
      )
    ))
  )
}

# ===========================================================================
# SERVEUR
# ===========================================================================
mod_qualitative_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Atelier de codage qualitatif (CAQDAS) : module autonome, affiche quand la
    # famille « Codage / thématisation » est selectionnee.
    mod_coding_server("coding", values)

    get_data <- shiny::reactive({
      d <- values$data
      shiny::validate(shiny::need(!is.null(d) && nrow(d) > 0,
        "Chargez d'abord un jeu de données dans l'onglet Chargement."))
      as.data.frame(d)
    })
    col_names <- shiny::reactive(names(get_data()))

    # Selecteurs dynamiques de colonnes
    output$nom_var1_ui <- shiny::renderUI({
      lab <- if (identical(input$nom_mode, "gof")) "Variable analysée (Y)"
             else if (identical(input$nom_mode, "bi")) "Variable 1"
             else "Variable"
      shiny::selectInput(ns("nom_var1"), lab, choices = col_names())
    })
    output$gof_x_ui <- shiny::renderUI({
      ch <- setdiff(col_names(), input$nom_var1 %||% "")
      shiny::selectInput(ns("gof_x"), "Facteur étudié (X, groupes)", choices = ch)
    })
    output$nom_var2_ui <- shiny::renderUI(shiny::selectInput(ns("nom_var2"), "Variable 2 (croisement)", choices = col_names()))
    output$gof_levels_hint <- shiny::renderUI({
      d <- get_data(); v <- input$nom_var1
      if (is.null(v) || !(v %in% names(d))) return(NULL)
      lv <- sort(unique(as.character(stats::na.omit(d[[v]]))))
      shiny::tags$small(style = "color:#7f8c8d;",
        shiny::icon("info-circle"),
        trf(" Ordre des modalités : %s (%d proportions attendues).",
                paste(lv, collapse = ", "), length(lv)))
    })
    output$multi_cols_ui <- shiny::renderUI(shiny::selectInput(ns("multi_cols"), "Colonnes binaires des options",
                                                              choices = col_names(), multiple = TRUE))
    output$multi_sepcol_ui <- shiny::renderUI(shiny::selectInput(ns("multi_sepcol"), "Colonne a valeurs séparées", choices = col_names()))
    output$ord_var1_ui <- shiny::renderUI(shiny::selectInput(ns("ord_var1"), "Variable ordinale", choices = col_names()))
    output$ord_var2_ui <- shiny::renderUI(shiny::selectInput(ns("ord_var2"), "Variable ordinale 2", choices = col_names()))
    output$ord_group_ui <- shiny::renderUI(shiny::selectInput(ns("ord_group"), "Variable de groupe", choices = col_names()))
    output$ord_items_ui <- shiny::renderUI(shiny::selectInput(ns("ord_items"), "Items de l'échelle", choices = col_names(), multiple = TRUE))
    output$txt_var_ui <- shiny::renderUI(shiny::selectInput(ns("txt_var"), "Variable textuelle", choices = col_names()))

    # ---- Sélecteurs OUTILS ----
    output$tools_vars_ui <- shiny::renderUI(
      shiny::selectInput(ns("tools_vars"), "Variables (laisser vide = toutes)",
                         choices = col_names(), multiple = TRUE))
    output$recode_var_ui <- shiny::renderUI(
      shiny::selectInput(ns("recode_var"), "Variable à recoder", choices = col_names()))
    output$orrr_expo_ui <- shiny::renderUI(
      shiny::selectInput(ns("orrr_expo"), "Variable d'exposition (X)", choices = col_names()))
    output$orrr_issue_ui <- shiny::renderUI(
      shiny::selectInput(ns("orrr_issue"), "Variable d'issue (Y)", choices = col_names()))
    output$orrr_expo_lvl_ui <- shiny::renderUI({
      if (isTRUE(input$orrr_all_pairs)) return(NULL)
      d <- get_data(); v <- input$orrr_expo
      if (is.null(v) || !(v %in% names(d))) return(NULL)
      lv <- sort(unique(as.character(stats::na.omit(d[[v]]))))
      shiny::selectInput(ns("orrr_expo_lvl"), "Modalité « exposé + »", choices = lv)
    })
    output$orrr_issue_lvl_ui <- shiny::renderUI({
      if (isTRUE(input$orrr_all_pairs)) return(NULL)
      d <- get_data(); v <- input$orrr_issue
      if (is.null(v) || !(v %in% names(d))) return(NULL)
      lv <- sort(unique(as.character(stats::na.omit(d[[v]]))))
      shiny::selectInput(ns("orrr_issue_lvl"), "Modalité « issue + »", choices = lv)
    })

    # ---- Application du recodage (écrit dans data, cleanData, filteredData) ----
    recode_msg <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$apply_recode, {
      d <- get_data(); v <- input$recode_var
      if (is.null(v) || !(v %in% names(d))) {
        recode_msg(list(ok = FALSE, msg = "Choisissez une variable à recoder.")); return()
      }
      lv <- recode_levels()
      mapping <- stats::setNames(lv, lv)   # ancienne -> nouvelle (identité par défaut)
      if (length(lv) > HSTAT_RECODE_THRESHOLD) {
        raw <- input$recode_table %||% ""
        lines <- strsplit(raw, "\n")[[1]]
        for (ln in lines) {
          if (!grepl("=", ln)) next
          parts <- strsplit(ln, "=", fixed = TRUE)[[1]]
          old <- trimws(parts[1]); new <- trimws(paste(parts[-1], collapse = "="))
          if (nzchar(old) && old %in% lv) mapping[[old]] <- new
        }
      } else {
        for (i in seq_along(lv)) {
          val <- input[[paste0("recode_lvl_", i)]]
          if (!is.null(val) && nzchar(trimws(val))) mapping[[lv[i]]] <- trimws(val)
        }
      }
      apply_map <- function(col) {
        cc <- as.character(col)
        idx <- match(cc, names(mapping))
        out <- ifelse(is.na(idx), cc, mapping[idx])
        out
      }
      n_changed <- 0L
      for (slot in c("data", "cleanData", "filteredData")) {
        dd <- values[[slot]]
        if (!is.null(dd) && v %in% names(dd)) {
          before <- as.character(dd[[v]])
          dd[[v]] <- apply_map(dd[[v]])
          if (identical(slot, "data")) n_changed <- sum(before != as.character(dd[[v]]), na.rm = TRUE)
          values[[slot]] <- dd
        }
      }
      n_new <- length(unique(unname(mapping)))
      recode_msg(list(ok = TRUE,
        msg = trf("Recodage appliqué à « %s » : %d valeur(s) modifiée(s), %d modalité(s) après recodage.",
                      v, n_changed, n_new)))
    })
    output$recode_status <- shiny::renderUI({
      m <- recode_msg(); if (is.null(m)) return(NULL)
      cls <- if (isTRUE(m$ok)) "callout callout-success" else "callout callout-warning"
      ic <- if (isTRUE(m$ok)) "check-circle" else "exclamation-triangle"
      shiny::div(class = cls, style = "font-size:12px;", shiny::icon(ic), " ", m$msg)
    })

    HSTAT_RECODE_THRESHOLD <- 12L   # au-delà : tableau éditable ; sinon menus déroulants
    recode_levels <- shiny::reactive({
      d <- get_data(); v <- input$recode_var
      shiny::req(v %in% names(d))
      sort(unique(as.character(stats::na.omit(d[[v]]))))
    })
    output$recode_interface_ui <- shiny::renderUI({
      lv <- recode_levels()
      if (length(lv) == 0) return(shiny::helpText("Aucune modalité à recoder."))
      if (length(lv) > HSTAT_RECODE_THRESHOLD) {
        # Tableau éditable : une zone de texte "ancienne = nouvelle" par ligne
        default_txt <- paste(sprintf("%s = %s", lv, lv), collapse = "\n")
        shiny::tagList(
          shiny::tags$small(trf("%d modalités (mode tableau éditable). Format : ancienne = nouvelle, une par ligne.", length(lv))),
          shiny::textAreaInput(ns("recode_table"), NULL, value = default_txt,
                               rows = min(20, length(lv) + 1), width = "100%"))
      } else {
        # Menus déroulants : nouvelle valeur libre par modalité
        shiny::tagList(
          shiny::tags$small(trf("%d modalités (mode menus). Saisissez la nouvelle valeur de chaque modalité.", length(lv))),
          lapply(seq_along(lv), function(i) {
            shiny::textInput(ns(paste0("recode_lvl_", i)),
                             label = sprintf("« %s » devient :", lv[i]), value = lv[i])
          }))
      }
    })

    # Choix manuel de l'ordre des niveaux (ordinale)
    output$ord_levels_ui <- shiny::renderUI({
      d <- get_data()
      v <- if ((input$ord_mode %||% "uni") == "likert") input$ord_items else input$ord_var1
      if (is.null(v) || length(v) == 0) return(NULL)
      vcol <- if (length(v) > 1) d[[v[1]]] else d[[v]]
      if (is.null(vcol)) return(NULL)
      lv <- hstat_q_ordinal_levels(vcol)
      shiny::textInput(ns("ord_levels"), "Ordre des niveaux (du plus bas au plus haut, séparés par ;)",
                       value = paste(lv, collapse = " ; "))
    })

    # Indice de type detecte
    output$type_hint <- shiny::renderUI({
      d <- get_data()
      fam <- input$family %||% "nominal"
      v <- switch(fam, "nominal" = input$nom_var1, "ordinal" = input$ord_var1, "textual" = input$txt_var)
      if (is.null(v) || !(v %in% names(d))) return(NULL)
      tp <- hstat_q_detect_type(d[[v]])
      cols <- c(nominale = "#16a085", ordinale = "#8e44ad", textuelle = "#e67e22",
                numerique = "#2980b9", binaire = "#7f8c8d")
      shiny::div(style = sprintf("margin-top:8px;padding:8px 10px;border-radius:6px;background:%s;color:white;font-size:12px;",
                                 cols[tp] %||% "#7f8c8d"),
        shiny::icon("magic"), trf(" Type détecté pour \"%s\" : %s", v, toupper(tp)))
    })

    # ---- Calcul de l'analyse ----
    # Depot du resultat qualitatif pour l'aide a la decision.
    shiny::observeEvent(result(), {
      r <- tryCatch(result(), error = function(e) NULL)
      if (is.null(r) || isFALSE(r$ok)) return()
      tabs <- if (!is.null(r$tables)) r$tables else list()
      if (!is.null(r$metrics)) tabs <- c(list("Metriques" = r$metrics), tabs)
      vars <- unique(unlist(lapply(
        c("nom_var1", "nom_var2", "ord_var1", "ord_var2", "txt_var",
          "orrr_expo", "orrr_issue", "tools_vars"),
        function(id) input[[id]])))
      hstat_ai_capture(values, "Analyses qualitatives",
        r$title %||% sprintf("Analyse qualitative (%s)", input$family %||% "nominal"),
        tables = tabs,
        text = if (!is.null(r$console)) paste(r$console, collapse = "\n") else NULL,
        meta = list(variables = vars, groupe = input$ord_group %||% input$nom_var2,
                    famille = input$family))
    }, ignoreInit = TRUE)

    result <- shiny::eventReactive(input$run, {
      d <- get_data()
      fam <- input$family %||% "nominal"
      lv_order <- if (!is.null(input$ord_levels) && nzchar(input$ord_levels))
        trimws(strsplit(input$ord_levels, ";")[[1]]) else NULL

      res <- tryCatch({
        if (fam == "nominal") {
          mode <- input$nom_mode %||% "uni"
          if (mode == "gof") {
            shiny::validate(shiny::need(input$nom_var1 %in% names(d), "Choisissez une variable."))
            props <- NULL
            if (identical(input$gof_props, "custom") && nzchar(trimws(input$gof_props_txt %||% ""))) {
              # Accepte "0.5, 0.3, 0.2", "0.5 0.3 0.2" ou "0,5 ; 0,3 ; 0,2"
              toks <- strsplit(trimws(input$gof_props_txt), "[;\\s]+")[[1]]
              if (length(toks) == 1) toks <- strsplit(toks, ",")[[1]]
              toks <- gsub("[,;]+$", "", toks)          # separateurs traines
              toks <- gsub(",", ".", toks, fixed = TRUE) # decimale francaise
              props <- suppressWarnings(as.numeric(toks[nzchar(toks)]))
              shiny::validate(shiny::need(length(props) > 0 && !anyNA(props),
                "Proportions attendues illisibles : nombres séparés par des virgules ou espaces (ex. 0.5, 0.3, 0.2)."))
            }
            if (isTRUE(input$gof_use_x) && !is.null(input$gof_x) &&
                input$gof_x %in% names(d) && !identical(input$gof_x, input$nom_var1)) {
              hstat_q_gof_stratified(d[[input$nom_var1]], d[[input$gof_x]],
                                     yname = input$nom_var1, xname = input$gof_x,
                                     expected_props = props,
                                     method = input$gof_method %||% "chisq",
                                     posthoc_adjust = input$gof_posthoc_adjust %||% "bonferroni")
            } else {
              hstat_q_gof_analysis(d[[input$nom_var1]], input$nom_var1,
                                   expected_props = props,
                                   method = input$gof_method %||% "chisq",
                                   posthoc_adjust = input$gof_posthoc_adjust %||% "bonferroni")
            }
          } else if (mode == "uni") {
            shiny::validate(shiny::need(input$nom_var1 %in% names(d), "Choisissez une variable."))
            hstat_q_nominal_univariate(d[[input$nom_var1]], input$nom_var1)
          } else if (mode == "bi") {
            shiny::validate(shiny::need(input$nom_var1 %in% names(d) && input$nom_var2 %in% names(d), "Choisissez 2 variables."))
            hstat_q_nominal_bivariate(d[[input$nom_var1]], d[[input$nom_var2]], input$nom_var1, input$nom_var2)
          } else {
            if ((input$multi_fmt %||% "binary") == "binary") {
              shiny::validate(shiny::need(length(input$multi_cols) >= 2, "Choisissez au moins 2 colonnes binaires."))
              hstat_q_multiple_choice(d, cols = input$multi_cols, var_name = "Choix multiples")
            } else {
              shiny::validate(shiny::need(input$multi_sepcol %in% names(d), "Choisissez la colonne."))
              hstat_q_multiple_choice(d, sep_col = input$multi_sepcol, sep = input$multi_sep %||% "[;,/|]")
            }
          }
        } else if (fam == "ordinal") {
          mode <- input$ord_mode %||% "uni"
          if (mode == "uni") {
            shiny::validate(shiny::need(input$ord_var1 %in% names(d), "Choisissez une variable."))
            hstat_q_ordinal_univariate(d[[input$ord_var1]], input$ord_var1, levels_order = lv_order)
          } else if (mode == "likert") {
            shiny::validate(shiny::need(length(input$ord_items) >= 2, "Choisissez au moins 2 items."))
            hstat_q_likert_scale(d[, input$ord_items, drop = FALSE], levels_order = lv_order)
          } else if (mode == "compare") {
            shiny::validate(shiny::need(input$ord_var1 %in% names(d) && input$ord_group %in% names(d), "Choisissez variable et groupe."))
            hstat_q_ordinal_compare(d[[input$ord_var1]], d[[input$ord_group]], levels_order = lv_order,
                                    xname = input$ord_var1, gname = input$ord_group)
          } else {
            shiny::validate(shiny::need(input$ord_var1 %in% names(d) && input$ord_var2 %in% names(d), "Choisissez 2 variables."))
            hstat_q_ordinal_compare(d[[input$ord_var1]], d[[input$ord_var2]], levels_order = lv_order,
                                    xname = input$ord_var1, gname = input$ord_var2, second_ordinal = TRUE)
          }
        } else if (fam == "textual") {
          shiny::validate(shiny::need(input$txt_var %in% names(d), "Choisissez une variable textuelle."))
          extra_sw <- if (nzchar(trimws(input$txt_stopwords %||% "")))
            strsplit(input$txt_stopwords, "[,;\\s]+")[[1]] else NULL
          hstat_q_text_analysis(d[[input$txt_var]], input$txt_var,
                                min_char = input$txt_minchar %||% 3,
                                top_n = input$txt_topn %||% 20,
                                n_gram = input$txt_ngram %||% 2,
                                n_topics = input$txt_topics %||% 3,
                                stem = isTRUE(input$txt_stem),
                                remove_numbers = isTRUE(input$txt_nonum),
                                extra_stopwords = extra_sw)
        } else {
          tmode <- input$tools_mode %||% "modal"
          vars <- input$tools_vars
          if (is.null(vars) || length(vars) == 0) vars <- names(d)
          if (tmode == "modal") {
            hstat_q_list_modalities(d, vars = vars)
          } else if (tmode == "miss") {
            hstat_q_missing_summary(d, vars = vars,
                                    treat_blank_as_na = isTRUE(input$miss_blank))
          } else if (tmode == "orrr") {
            shiny::validate(shiny::need(input$orrr_expo %in% names(d) && input$orrr_issue %in% names(d),
                                        "Choisissez les variables d'exposition et d'issue."))
            hstat_q_or_rr_analysis(d[[input$orrr_expo]], d[[input$orrr_issue]],
                                   xname = input$orrr_expo, yname = input$orrr_issue,
                                   x_expose = input$orrr_expo_lvl, y_issue = input$orrr_issue_lvl,
                                   conf = input$orrr_conf %||% 0.95,
                                   all_pairs = isTRUE(input$orrr_all_pairs))
          } else {
            list(ok = FALSE, notes = "Le recodage s'applique avec le bouton « Appliquer le recodage ».")
          }
        }
      }, error = function(e) list(ok = FALSE, notes = hstat_err_fr(e)))
      res
    })

    output$result_note <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(NULL)
      if (!isTRUE(r$ok))
        return(shiny::div(class = "callout callout-warning", shiny::icon("exclamation-triangle"), " ", r$notes %||% "Analyse impossible."))
      shiny::div(class = "callout callout-success", style = "font-size:12px;",
                 shiny::icon("check-circle"), " ", r$notes %||% "Analyse effectuée.")
    })

    # ---- Graphiques ----
    output$graph_selector <- shiny::renderUI({
      r <- result(); if (is.null(r) || !isTRUE(r$ok) || length(r$plotfns) == 0) return(NULL)
      shiny::selectInput(ns("which_plot"), "Choisir un graphique", choices = names(r$plotfns), width = "350px")
    })
    current_plot <- shiny::reactive({
      r <- result(); shiny::req(isTRUE(r$ok), length(r$plotfns) > 0)
      wp <- input$which_plot %||% names(r$plotfns)[1]
      fn <- r$plotfns[[wp]]; if (is.null(fn)) fn <- r$plotfns[[1]]
      p <- fn()
      p <- hstat_q_apply_palette(p, input$plot_palette %||% "default",
                                 input$plot_col_low %||% "#aed6f1",
                                 input$plot_col_high %||% "#1f618d")
      # Mise en forme interactive : lit tous les curseurs sty_* -> re-rendu live
      hstat_q_apply_style(p, list(
        title = input$sty_title, xlab = input$sty_xlab, ylab = input$sty_ylab,
        legend = input$sty_legend,
        title_size = input$sty_title_size %||% 16,
        axis_label_size = input$sty_axis_label_size %||% 12,
        axis_title_wrap = isTRUE(input$sty_axisTitreRetour %||% TRUE),
        axis_title_align = input$sty_axisTitreAlign %||% "0.5",
        axis_text_size = input$sty_axis_text_size %||% 10,
        legend_text_size = input$sty_legend_text_size %||% 10,
        axis_title_bold = input$sty_axis_title_bold %||% TRUE,
        axis_title_italic = input$sty_axis_title_italic %||% FALSE,
        axis_text_bold = input$sty_axis_text_bold %||% FALSE,
        axis_text_italic = input$sty_axis_text_italic %||% FALSE,
        x_rotation = input$sty_x_rotation %||% 45,
        show_grid = input$sty_show_grid %||% TRUE,
        black_axes = input$sty_black_axes %||% FALSE,
        black_ticks = input$sty_black_ticks %||% FALSE))
    })
    output$main_plot <- shiny::renderPlot({
      p <- current_plot(); shiny::req(!is.null(p)); p
    })
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("graphique_qualitatif_", Sys.Date(),
                                   ".", input$dl_format %||% "png"),
      content = function(file) {
        # `req()` interrompait le telechargement SANS ecrire de fichier : Shiny
        # renvoyait alors sa page d'erreur HTML sous le nom `.png`. L'ecrivain
        # commun garantit un fichier valide, portant le motif le cas echeant.
        p <- tryCatch(current_plot(), error = function(e) NULL)
        hstat_ecrire_image(file, p, input$dl_format %||% "png",
                           input$dl_width %||% 9, input$dl_height %||% 6,
                           input$dl_dpi %||% 300,
                           echec = "Aucun graphique à exporter : lancez d'abord l'analyse.")
      })

    # ---- Metriques ----
    output$metrics_table <- DT::renderDT({
      r <- result(); shiny::req(isTRUE(r$ok), !is.null(r$metrics))
      DT::datatable(r$metrics, rownames = FALSE, options = list(dom = "t", pageLength = 50))
    })
    output$interpretation <- shiny::renderUI({
      r <- result(); if (is.null(r) || !isTRUE(r$ok)) return(NULL)
      shiny::tags$ul(lapply(r$interpretation, function(t) shiny::tags$li(t)))
    })

    # ---- Sorties console R (tests statistiques au format classique) ----
    output$console_note <- shiny::renderUI({
      r <- result()
      if (is.null(r) || !isTRUE(r$ok) || is.null(r$console)) {
        shiny::div(class = "callout callout-info",
          shiny::icon("info-circle"),
          " Les sorties au format console R (khi-deux avec effectifs théoriques et résidus, test exact de Fisher, V de Cramér avec IC, OR/RR, corrélations de rangs) sont disponibles pour les analyses bivariées : croisement de deux variables, OR/RR et comparaisons ordinales.")
      } else {
        shiny::div(class = "callout callout-success",
          shiny::icon("terminal"),
          " Présentation identique à la console R : directement citable dans un rapport ou vérifiable dans R.")
      }
    })
    output$console_out <- shiny::renderText({
      r <- result(); shiny::req(isTRUE(r$ok), !is.null(r$console))
      paste(r$console, collapse = "\n")
    })
    output$dl_console <- shiny::downloadHandler(
      filename = function() paste0("tests_qualitatifs_", Sys.Date(), ".txt"),
      content = function(file) {
        r <- result()
        writeLines(if (isTRUE(r$ok) && !is.null(r$console)) r$console else
                   "Aucune sortie console pour cette analyse.", file, useBytes = TRUE)
      })

    # ---- Tableaux detailles ----
    output$tables_selector <- shiny::renderUI({
      r <- result(); if (is.null(r) || !isTRUE(r$ok) || length(r$tables) == 0) return(NULL)
      shiny::selectInput(ns("which_table"), "Choisir un tableau", choices = names(r$tables), width = "350px")
    })
    current_table <- shiny::reactive({
      r <- result(); shiny::req(isTRUE(r$ok), length(r$tables) > 0)
      wt <- input$which_table %||% names(r$tables)[1]
      tb <- r$tables[[wt]]; if (is.null(tb)) tb <- r$tables[[1]]
      tb
    })
    output$detail_table <- DT::renderDT({
      tb <- current_table(); shiny::req(!is.null(tb))
      DT::datatable(tb, rownames = FALSE, filter = "top",
                    options = list(pageLength = 15, scrollX = TRUE))
    })
    output$dl_table <- shiny::downloadHandler(
      filename = function() paste0("tableau_", .safe_name(input$which_table %||% "resultat"),
                                   "_", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(current_table(), file, row.names = FALSE, fileEncoding = "UTF-8"))

    output$dl_table_xlsx <- shiny::downloadHandler(
      filename = function() paste0("tableau_", .safe_name(input$which_table %||% "resultat"),
                                   "_", Sys.Date(), ".xlsx"),
      content = function(file) {
        tb <- current_table(); shiny::req(!is.null(tb))
        .write_xlsx(stats::setNames(list(tb), substr(input$which_table %||% "Tableau", 1, 31)), file)
      })

    output$dl_all_csv <- shiny::downloadHandler(
      filename = function() paste0("tableaux_qualitatifs_", Sys.Date(), ".zip"),
      content = function(file) {
        r <- result(); shiny::req(isTRUE(r$ok), length(r$tables) > 0)
        tmp <- tempfile("qexport"); dir.create(tmp)
        paths <- character(0)
        for (nm in names(r$tables)) {
          f <- file.path(tmp, paste0(.safe_name(nm), ".csv"))
          utils::write.csv(r$tables[[nm]], f, row.names = FALSE, fileEncoding = "UTF-8")
          paths <- c(paths, f)
        }
        # métriques + interprétation jointes
        if (!is.null(r$metrics)) {
          fm <- file.path(tmp, "00_metriques_interpretees.csv")
          utils::write.csv(r$metrics, fm, row.names = FALSE, fileEncoding = "UTF-8")
          paths <- c(fm, paths)
        }
        # utils::zip exige un binaire zip externe, souvent absent sous Windows :
        # on privilegie le package 'zip' (autonome), avec repli sur utils::zip.
        if (requireNamespace("zip", quietly = TRUE)) {
          zip::zipr(file, paths)
        } else {
          utils::zip(file, paths, flags = "-j9X")
        }
      })

    output$dl_all_xlsx <- shiny::downloadHandler(
      filename = function() paste0("resultats_qualitatifs_", Sys.Date(), ".xlsx"),
      content = function(file) {
        r <- result(); shiny::req(isTRUE(r$ok))
        sheets <- list()
        if (!is.null(r$metrics)) sheets[["Métriques"]] <- r$metrics
        for (nm in names(r$tables)) sheets[[substr(nm, 1, 31)]] <- r$tables[[nm]]
        .write_xlsx(sheets, file)
      })
  })
}
