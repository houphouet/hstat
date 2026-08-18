server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # PERSISTANCE DE LA SESSION
  # Un verrouillage d'ecran, une mise en veille ou une coupure reseau passagere
  # font tomber le websocket de Shiny. Sans cette autorisation, la session est
  # DETRUITE cote serveur : l'utilisateur retrouve une page grise et a perdu ses
  # donnees, ses filtres et ses analyses. Avec elle, l'etat est conserve et le
  # navigateur peut reprendre la main (le bandeau de www/hstat-session.js s'en
  # charge). Seul l'utilisateur ferme l'application.
  #
  # « force » et non TRUE : sans lui, Shiny n'active la reprise que derriere un
  # serveur qui la gere (Connect, Shiny Server Pro). Or HStat tourne le plus
  # souvent en local, cas ou le besoin est le plus fort.
  try(session$allowReconnect("force"), silent = TRUE)

  # Langue choisie dans le bandeau, signalee par www/hstat-i18n.js. Elle est
  # rangee dans `session$userData` — donc propre a CET utilisateur — et lue par
  # `hstat_langue_session()`. Les messages composes en R (erreurs, verdicts)
  # suivent ainsi la langue sans qu'aucun de leurs ~70 points d'appel change.
  shiny::observeEvent(input$hstat_langue, {
    session$userData$langue <- if (identical(input$hstat_langue, "en")) "en" else "fr"
  }, ignoreInit = FALSE)

  # LES TERMES DU FICHIER DE L'UTILISATEUR NE SE TRADUISENT JAMAIS.
  # On les envoie au navigateur a chaque changement de jeu de donnees : noms de
  # colonnes et modalites qualitatives. Le traducteur refuse alors d'y toucher
  # ou qu'ils apparaissent -- y compris dans un tableau ajoute plus tard, qui
  # echapperait a toute annotation posee a la main.
  shiny::observe({
    d <- values$filteredData %||% values$data
    j <- tryCatch(hstat_i18n_termes_json(d), error = function(e) "[]")
    session$sendCustomMessage("hstat-termes-donnees", j)
  })

  # Signal de maintien envoye par le navigateur. Il n'alimente aucun calcul :
  # sa seule fonction est d'empecher une coupure pour inactivite. On le lit
  # explicitement pour qu'il ne soit pas pris pour une entree oubliee.
  shiny::observeEvent(input$hstat_keepalive, {
    invisible(NULL)
  }, ignoreInit = TRUE)

  # Garde-fou a la fermeture : le navigateur ne demande confirmation que si des
  # donnees sont chargees. Une confirmation qui s'affiche a chaque visite n'est
  # plus lue, et protegerait une page vide.
  shiny::observe({
    session$sendCustomMessage("hstat_travail",
                              list(actif = !is.null(values$data) && NROW(values$data) > 0))
  })

  options(shiny.error = function() {
    msg <- tryCatch(conditionMessage(sys.call()), error = function(e) "Erreur inconnue")
    shinyjs::logjs(paste("[HStat] Erreur non capturée:", msg))
  })
  
  # L'etat initial vit dans Utils.R : la creation et la reinitialisation
  # partagent la MEME liste. Un champ ajoute la-bas est cree ici et efface a la
  # reinitialisation, sans qu'on ait a y penser deux fois.
  values <- do.call(shiny::reactiveValues, hstat_valeurs_initiales())

  # Indicateurs de lancement des analyses multivariees : chaque analyse ne
  # s'execute QUE lorsque l'utilisateur clique sur son bouton dedie (evite le
  # lancement automatique simultane d'ACP/HCPC/AFD au chargement des donnees).
  mv_launch <- shiny::reactiveValues(pca = FALSE, hcpc = FALSE, afd = FALSE)
  shiny::observeEvent(input$pcaRun,  {
    mv_launch$pca  <- TRUE
    session$sendCustomMessage("expandBox", "boxWrap_pcaResults")
  })
  # HCPC s'appuie sur l'ACP : lancer HCPC declenche aussi le calcul de l'ACP
  # sous-jacente (sinon pcaResultReactive() reste vide et le HCPC n'affiche rien).
  shiny::observeEvent(input$hcpcRun, { mv_launch$pca <- TRUE; mv_launch$hcpc <- TRUE })
  shiny::observeEvent(input$afdRun,  { mv_launch$afd  <- TRUE })
  # Si les variables changent, on redemande un lancement explicite
  shiny::observeEvent(input$pcaVars,    { mv_launch$pca  <- FALSE }, ignoreInit = TRUE)
  shiny::observeEvent(input$afdVars,    { mv_launch$afd  <- FALSE }, ignoreInit = TRUE)
  shiny::observeEvent(input$afdFactor,  { mv_launch$afd  <- FALSE }, ignoreInit = TRUE)
  
  values$customXLevels <- shiny::reactiveVal(NULL)
  
  
  # ---- Aide et réinitialisation ----
  # `shinyalert` est un paquet OPTIONNEL (il figure parmi ceux dont l'absence
  # est toleree au demarrage). Sans repli, `shinyalert()` levait une erreur
  # avalee par l'observateur : le bouton « Reinitialiser » ne faisait alors
  # RIEN, en silence. Une fonction essentielle ne peut pas dependre d'un
  # paquet facultatif ; `modalDialog` fait partie de Shiny et est toujours la.
  .hstat_a_shinyalert <- isTRUE(requireNamespace("shinyalert", quietly = TRUE))

  shiny::observeEvent(input$helpBtn, {
    txt <- paste("Cette application permet d'analyser statistiquement des",
                 "données expérimentales. Naviguez à travers les onglets de",
                 "gauche pour charger, explorer, nettoyer, filtrer et analyser",
                 "vos données.")
    if (.hstat_a_shinyalert)
      shinyalert(title = "Aide", text = txt, type = "info")
    else
      shiny::showModal(shiny::modalDialog(title = "Aide", txt, easyClose = TRUE,
                            footer = shiny::modalButton("Fermer")))
  })
  
  # La remise a zero elle-meme, appelee par les deux chemins de confirmation.
  .hstat_reinitialiser <- function() {
          # La connexion hors-memoire se ferme AVANT d'effacer sa reference,
          # sinon le fichier reste verrouille et le processus DuckDB en vie.
          if (!is.null(values$dbCon)) hstat_duckdb_close(values$dbCon)

          init <- hstat_valeurs_initiales()
          for (nm in names(init)) values[[nm]] <- init[[nm]]
          # Un champ cree en cours de session par un module (il y en a) n'est
          # pas dans la liste initiale : on le vide aussi, sans quoi il
          # survivrait a la reinitialisation.
          for (nm in setdiff(names(shiny::reactiveValuesToList(values)), names(init)))
            values[[nm]] <- NULL

          # `shinyjs::reset("file")` remet le widget a blanc mais `input$file`
          # garde sa valeur : la feuille Excel choisie et le bloc de
          # combinaison de feuilles survivaient donc a la reinitialisation. On
          # note le chemin a neutraliser, et tout ce qui derive du fichier le
          # traite comme absent jusqu'a un NOUVEAU choix.
          values$fichierNeutralise <- shiny::isolate(input$file$datapath)
          tryCatch(shinyjs::reset("file"), error = function(e) NULL)

          # Messages de resultat des deux fusions : ils restaient affiches sous
          # un panneau devenu vide.
          sheet_merge_msg(NULL)
          merge_msg(NULL)

          # Signale aux modules (Plan & Puissance, Seuils d'efficacite) de
          # reinitialiser leur propre etat et leurs controles.
          values$resetSignal <- (values$resetSignal %||% 0) + 1
          shinydashboard::updateTabItems(session, "tabs", "load")

    shiny::showNotification("Application réinitialisée", type = "message")
  }

  shiny::observeEvent(input$resetBtn, {
    msg <- paste("Êtes-vous sûr de vouloir réinitialiser l'application ?",
                 "Toutes les données seront perdues.")
    if (.hstat_a_shinyalert) {
      shinyalert(title = "Réinitialiser", text = msg, type = "warning",
                 showCancelButton = TRUE, confirmButtonText = "Oui",
                 cancelButtonText = "Non",
                 callbackR = function(value) if (isTRUE(value)) .hstat_reinitialiser())
    } else {
      shiny::showModal(shiny::modalDialog(
        title = "Réinitialiser", msg, easyClose = FALSE,
        footer = shiny::tagList(shiny::modalButton("Non"),
                         shiny::actionButton("resetConfirm", "Oui", class = "btn-warning"))))
    }
  })

  shiny::observeEvent(input$resetConfirm, {
    shiny::removeModal()
    .hstat_reinitialiser()
  })
  
  # ---- Chargement ----
  # LE fichier courant, et la seule porte par laquelle on y accede.
  # `shinyjs::reset("file")` remet le widget a blanc mais `input$file` conserve
  # sa valeur : tout ce qui en derivait — la feuille Excel choisie, le bloc de
  # combinaison de feuilles, l'apercu — survivait a la reinitialisation. Le
  # fichier neutralise est donc traite comme absent jusqu'a un NOUVEAU choix.
  fichier_actif <- shiny::reactive({
    f <- input$file
    if (is.null(f)) return(NULL)
    if (!is.null(values$fichierNeutralise) &&
        identical(f$datapath, values$fichierNeutralise)) return(NULL)
    f
  })

  # Feuilles du classeur. Un classeur d'enquete porte souvent une feuille par
  # annee, par site ou par vague : ne lire que la premiere revient a jeter le
  # reste des donnees.
  excel_sheets_r <- shiny::reactive({
    f <- fichier_actif()
    if (is.null(f)) return(character(0))
    hstat_excel_sheets(f$datapath)
  })

  output$sheetUI <- shiny::renderUI({
    sheets <- excel_sheets_r()
    if (!length(sheets)) return(NULL)
    shiny::tagList(
      shiny::selectInput("sheet", "Feuille Excel :", choices = sheets, selected = sheets[1]),
      # `open = NA` : le bloc est DEPLIE d'emblee. Replie, il se resumait a une
      # ligne que l'utilisateur ne remarquait pas — la fonctionnalite existait
      # sans que personne ne la voie. Il ne s'affiche que lorsqu'il sert, c'est
      # a dire quand le classeur compte plus d'une feuille.
      if (length(sheets) > 1) shiny::tags$details(
        open = NA,
        style = "margin:4px 0 10px; padding:10px 14px; background:#eef7fb; border:1px solid #b6e0ef; border-radius:8px;",
        shiny::tags$summary(style = "cursor:pointer; font-weight:700; color:#1b6f8c; font-size:14px;",
          shiny::icon("layer-group"),
          trf(" Ce classeur contient %d feuilles — les combiner en un seul jeu de données", length(sheets))),
        shiny::div(style = "padding-top:12px;",
          p(style = "color:#5a6a7a; font-size:13px;",
            "Choisissez les feuilles a combiner. ",
            shiny::tags$b("Le resultat remplace les donnees de travail actuelles"),
            " et devient le jeu sur lequel portent toutes les analyses."),
          shiny::checkboxGroupInput("sheetPick", "Feuilles a combiner",
                             choices = sheets, selected = sheets),
          # Le conseil est calcule sur les feuilles reellement choisies : c'est
          # la structure des donnees qui dit si elles s'empilent ou se joignent,
          # pas l'utilisateur qui doit le deviner.
          shiny::uiOutput("sheetAdvice"),
          shiny::selectInput("sheetMergeType", "Comment les combiner",
            choices = list(
              "Mettre bout a bout (meme structure)" = c(
                "Empiler les lignes" = "rows",
                "Empiler et supprimer les doublons" = "union_distinct"),
              "Rapprocher par une cle (structures differentes)" = c(
                "Jointure interne (lignes presentes partout)" = "inner",
                "Jointure a gauche (garde toute la 1re feuille)" = "left",
                "Jointure complete (garde tout)" = "full")),
            selected = "rows"),
          shiny::conditionalPanel(
            condition = "['inner','left','full'].indexOf(input.sheetMergeType) >= 0",
            shiny::textInput("sheetKey", "Colonne(s) cle, separees par une virgule",
                      placeholder = "Ex. id, ou site, annee")),
          shiny::conditionalPanel(
            condition = "input.sheetMergeType == 'rows'",
            shiny::fluidRow(
              shiny::column(6, shiny::textInput("sheetSourceName", "Colonne d'origine",
                                  value = "feuille")),
              shiny::column(6, shiny::radioButtons("sheetSourceMode", "Valeur inscrite",
                       choices = c("Nom de la feuille" = "name",
                                   "Nombre extrait du nom" = "number"),
                       selected = "name"))),
            shiny::tags$small(style = "color:#6b7280;", shiny::icon("info-circle"),
              " Chaque ligne garde la trace de sa feuille d'origine. Avec ",
              shiny::tags$b("Nombre extrait"), ", une feuille nommee « 2024 » donne 2024 : ",
              "la colonne devient une vraie variable d'annee, utilisable en analyse.")),
          shiny::actionButton("applySheetMerge",
                       shiny::tagList(shiny::icon("object-group"), " Combiner ces feuilles"),
                       class = "btn-info"),
          shiny::uiOutput("sheetMergeStatus"))))
  })

  output$sheetAdvice <- shiny::renderUI({
    sel <- input$sheetPick
    if (is.null(sel) || length(sel) < 2) return(
      shiny::tags$small(style = "color:#b9770e;", shiny::icon("circle-info"),
                 " Selectionnez au moins deux feuilles."))
    r <- hstat_excel_read_sheets(fichier_actif()$datapath, sel)
    if (!length(r$frames)) return(
      shiny::div(class = "callout callout-warning", style = "padding:8px 12px;font-size:12px;", r$msg))
    a <- hstat_excel_compat(r$frames, r$names)
    shiny::div(class = if (a$identiques) "callout callout-success" else "callout callout-info",
        style = "padding:8px 12px;font-size:12px;margin-bottom:8px;",
        shiny::icon("lightbulb"), " ", a$msg,
        if (length(r$ignorees))
          shiny::tags$div(style = "margin-top:4px;", shiny::tags$b("Ecartees : "),
                   paste(r$ignorees, collapse = ", "), " (vides ou illisibles)."))
  })

  sheet_merge_msg <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$applySheetMerge, {
    tryCatch({
      sel <- input$sheetPick
      if (is.null(sel) || length(sel) < 2) {
        sheet_merge_msg(list(ok = FALSE,
          msg = "Choisissez au moins deux feuilles a combiner.")); return()
      }
      r <- hstat_excel_read_sheets(fichier_actif()$datapath, sel)
      if (length(r$frames) < 2) {
        sheet_merge_msg(list(ok = FALSE, msg = paste(
          "Au moins deux feuilles exploitables sont necessaires.", r$msg))); return()
      }
      # Meme moteur que la fusion de plusieurs fichiers : les feuilles n'ont
      # aucune raison d'avoir leur propre logique de jointure.
      res <- hstat_merge_frames(
        frames = r$frames, type = input$sheetMergeType %||% "rows",
        key_left = input$sheetKey, key_right = NULL,
        add_source = TRUE, source_names = r$names,
        source_col = input$sheetSourceName %||% "feuille",
        source_mode = input$sheetSourceMode %||% "name")
      if (!isTRUE(res$ok)) { sheet_merge_msg(list(ok = FALSE, msg = res$msg)); return() }
      d <- as.data.frame(res$data)
      values$data <- d; values$cleanData <- d; values$filteredData <- d
      values$dataMode <- "memory"
      values$sourceKind <- "xlsx"
      # Le moteur de fusion parle de « fichiers » — c'est son vocabulaire, il
      # sert d'abord a fusionner des fichiers. Ici ce sont des feuilles, et
      # laisser « 3 fichiers » sous les yeux de quelqu'un qui vient d'en
      # combiner trois d'un meme classeur serait deroutant.
      msg <- paste(gsub("fichiers", "feuilles", gsub("fichier", "feuille", res$msg)),
                   sprintf("Feuilles combinees : %s.",
                           paste(r$names, collapse = ", ")))
      sheet_merge_msg(list(ok = TRUE, msg = msg))
      shiny::showNotification(shiny::tagList(shiny::icon("check"), " ", msg), type = "message", duration = 8)
    }, error = function(e) {
      sheet_merge_msg(list(ok = FALSE, msg = hstat_err_fr(e, "Combinaison des feuilles")))
    })
  })

  output$sheetMergeStatus <- shiny::renderUI({
    m <- sheet_merge_msg()
    if (is.null(m)) return(NULL)
    shiny::div(class = if (isTRUE(m$ok)) "callout callout-success" else "callout callout-danger",
        style = "padding:8px 12px;font-size:13px;margin-top:8px;",
        shiny::icon(if (isTRUE(m$ok)) "circle-check" else "triangle-exclamation"),
        " ", m$msg)
  })

  shiny::observeEvent(input$loadData, {
    shiny::req(fichier_actif())
    kind <- hstat_file_kind(fichier_actif()$datapath)
    if (kind == "inconnu") {
      shiny::showNotification("Format de fichier non pris en charge.", type = "error")
      return(invisible(NULL))
    }
    tryCatch({
      # Fermer une eventuelle connexion DuckDB precedente
      if (!is.null(values$dbCon)) {
        hstat_duckdb_close(values$dbCon)
        values$dbCon <- NULL
      }
      res <- NULL
      shiny::withProgress(message = 'Chargement des données', value = 0, {
        shiny::incProgress(0.2, detail = "Analyse du fichier")
        hstat_cache_clear()   # vide le cache d'agregations du fichier precedent
        thr <- (input$bigDataThreshold %||% 500) * 1024^2
        smp <- as.integer(input$sampleSize %||% HSTAT_SAMPLE_SIZE)
        shiny::incProgress(0.3, detail = "Lecture")
        res <- hstat_load_data(
          path = fichier_actif()$datapath, kind = kind,
          header = input$header %||% TRUE, sep = input$sep %||% ",",
          sheet = input$sheet %||% 1,
          threshold = thr, sample_size = smp)
        shiny::incProgress(0.8, detail = "Préparation")

        values$data        <- res$data
        values$cleanData   <- res$data
        values$filteredData <- res$data
        values$dbCon       <- res$con
        values$dbTable     <- res$table
        values$dataMode    <- res$mode
        values$fullNrow    <- res$full_nrow
        values$fullNcol    <- res$full_ncol
        values$isSampled   <- res$is_sampled
        values$sourceKind  <- res$kind
        values$sourceSize  <- res$size
        # Total des NA : direct en memoire, calcule en SQL en mode DuckDB
        values$fullNA <- if (res$mode == "duckdb")
          tryCatch(hstat_duckdb_na_total(res$con, res$table),
                   error = function(e) NA_real_)
        else res$full_na
        shiny::incProgress(1)
      })
      if (isTRUE(res$is_sampled)) {
        shiny::showNotification(
          trf("Fichier volumineux (%s) : mode hors-mémoire activé. Analyse sur un échantillon de %s lignes (sur %s au total).",
                  hstat_format_size(res$size),
                  format(nrow(res$data), big.mark = " "),
                  format(res$full_nrow, big.mark = " ")),
          type = "warning", duration = 12)
      } else {
        shiny::showNotification("Données chargées avec succès.", type = "message")
      }
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Chargement des donnees"), type = "error",
                       duration = 15)
    })
  })

  # ---- Fusion de plusieurs fichiers ----
  # Pour peupler les menus de cles, on ne lit que les EN-TETES (noms de colonnes),
  # pas les donnees completes : lire 5 fichiers entiers en memoire des la selection
  # pouvait saturer la memoire et fermer l'application. La lecture complete n'a lieu
  # qu'au moment de la fusion (bouton Fusionner).
  # Les en-tetes alimentent les selecteurs de cle. Ils doivent donc suivre
  # exactement le meme depliage que `merge_frames_read()` : sinon les feuilles
  # d'un classeur sont bien fusionnees, mais aucune de leurs colonnes n'est
  # proposee comme cle — la jointure devient impossible a parametrer.
  merge_headers_read <- shiny::reactive({
    mf <- tryCatch(merge_frames_read(), error = function(e) NULL)
    if (is.null(mf)) return(NULL)
    list(cols = lapply(mf$frames, function(d) if (is.null(d)) NULL else names(d)),
         names = mf$names)
  })

  merge_frames_read <- shiny::reactive({
    shiny::req(input$mergeFiles)
    fp <- input$mergeFiles
    sep <- input$mergeSep %||% ","
    frames <- list(); noms <- character(0)
    for (i in seq_len(nrow(fp))) {
      base <- tools::file_path_sans_ext(fp$name[i])
      # Un classeur est DEPLIE en ses feuilles : deposer deux classeurs de
      # trois feuilles ici donnait deux tableaux au lieu de six, les autres
      # feuilles etant ignorees en silence. Un seul classeur multi-feuilles
      # suffit desormais a alimenter une fusion.
      feuilles <- hstat_excel_sheets(fp$datapath[i])
      if (length(feuilles) > 1) {
        r <- tryCatch(hstat_excel_read_sheets(fp$datapath[i], feuilles),
                      error = function(e) NULL)
        if (!is.null(r) && length(r$frames)) {
          frames <- c(frames, r$frames)
          # Le nom porte le fichier ET la feuille : avec plusieurs classeurs,
          # « 2024 » seul ne dirait pas de quel fichier il vient.
          noms <- c(noms, if (nrow(fp) > 1) paste(base, r$names, sep = " - ")
                          else r$names)
          next
        }
      }
      frames <- c(frames, list(tryCatch(
        hstat_read_any_mem(fp$datapath[i], sep = sep, name = fp$name[i]),
        error = function(e) NULL)))
      noms <- c(noms, base)
    }
    # Deux tableaux restent le minimum — mais ils peuvent venir d'un seul
    # classeur, ce qui n'etait pas possible auparavant.
    if (length(frames) < 2) return(NULL)
    list(frames = frames, names = noms)
  })

  output$mergeKeyLeftUI <- shiny::renderUI({
    mf <- tryCatch(merge_headers_read(), error = function(e) NULL)
    if (is.null(mf) || length(mf$cols) < 1 || is.null(mf$cols[[1]]))
      return(shiny::helpText("Importez au moins deux fichiers valides."))
    shiny::selectInput("mergeKeyLeft",
      trf("Clé(s) du 1er fichier (%s)", mf$names[1]),
      choices = mf$cols[[1]], multiple = TRUE)
  })
  output$mergeKeyRightUI <- shiny::renderUI({
    mf <- tryCatch(merge_headers_read(), error = function(e) NULL)
    if (is.null(mf) || length(mf$cols) < 2 || is.null(mf$cols[[2]]))
      return(NULL)
    shiny::selectInput("mergeKeyRight",
      trf("Clé(s) du 2e fichier (%s) — même nombre de colonnes", mf$names[2]),
      choices = mf$cols[[2]], multiple = TRUE)
  })

  merge_msg <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$applyMerge, {
    tryCatch({
      mf <- merge_frames_read()
      if (is.null(mf)) {
        merge_msg(list(ok = FALSE, msg = "Importez au moins deux fichiers.")); return()
      }
      valid <- !vapply(mf$frames, is.null, logical(1))
      if (sum(valid) < 2) {
        merge_msg(list(ok = FALSE, msg = "Au moins deux fichiers doivent être lisibles (format non reconnu ou fichier illisible ?).")); return()
      }
      res <- hstat_merge_frames(
        frames = mf$frames[valid], type = input$mergeType %||% "inner",
        key_left = input$mergeKeyLeft, key_right = input$mergeKeyRight,
        add_source = isTRUE(input$mergeAddSource), source_names = mf$names[valid],
        source_col = input$mergeSourceName %||% "source",
        source_mode = input$mergeSourceMode %||% "name")
      if (!isTRUE(res$ok)) { merge_msg(list(ok = FALSE, msg = res$msg)); return() }
      d <- as.data.frame(res$data)
      values$data <- d; values$cleanData <- d; values$filteredData <- d
      values$dataMode <- "memory"
      merge_msg(list(ok = TRUE, msg = res$msg))
      shiny::showNotification(shiny::tagList(shiny::icon("check"), " ", res$msg), type = "message", duration = 5)
    }, error = function(e) {
      merge_msg(list(ok = FALSE, msg = paste("Échec de la fusion :", conditionMessage(e))))
      shiny::showNotification(hstat_err_fr(e, "Fusion"),
                       type = "error", duration = 8)
    })
  })

  output$mergeStatus <- shiny::renderUI({
    m <- merge_msg(); if (is.null(m)) return(NULL)
    col <- if (isTRUE(m$ok)) "#27ae60" else "#c0392b"
    ic <- if (isTRUE(m$ok)) "check-circle" else "exclamation-triangle"
    shiny::div(style = sprintf("margin-top:10px;padding:8px;border-radius:4px;background:%s22;color:%s;font-size:12px;", col, col),
        shiny::icon(ic), " ", m$msg)
  })

  # Liberer la connexion DuckDB a la fermeture de la session
  session$onSessionEnded(function() {
    shiny::isolate({
      if (!is.null(values$dbCon)) hstat_duckdb_close(values$dbCon)
    })
  })

  # Banniere de mode (memoire vs hors-memoire)
  # Indicateur global : l'application est-elle en mode hors-memoire ?
  # (utilise par les conditionalPanel "calculer sur le jeu complet" au niveau
  # racine de l'UI ; chaque module definit aussi le sien pour ses propres panels)
  output$hstatBigData <- shiny::reactive({
    identical(values$dataMode, "duckdb") && !is.null(values$dbCon)
  })
  shiny::outputOptions(output, "hstatBigData", suspendWhenHidden = FALSE)

  output$dataModeBanner <- shiny::renderUI({
    shiny::req(values$data)
    if (identical(values$dataMode, "duckdb")) {
      shiny::div(class = "callout callout-warning", style = "margin-bottom:16px;",
        shiny::h4(style = "margin:0 0 5px 0; font-weight:600;",
           shiny::icon("database"), " Mode hors-mémoire (out-of-core)"),
        p(style = "margin:0; font-size:13px;",
          shiny::HTML(trf(
            "Fichier de <b>%s</b> (%s lignes). Le jeu complet reste sur disque (DuckDB) ; les analyses portent sur un <b>échantillon représentatif de %s lignes</b>. Les compteurs ci-dessous reflètent le jeu complet.",
            hstat_format_size(values$sourceSize %||% 0),
            format(values$fullNrow %||% 0, big.mark = " "),
            format(nrow(values$data), big.mark = " ")))))
    } else {
      shiny::div(class = "callout callout-success", style = "margin-bottom:16px;",
        p(style = "margin:0; font-size:13px;",
          shiny::icon("memory"), shiny::HTML(trf(
            " Mode en mémoire — jeu de données entièrement chargé (%s lignes).",
            format(values$fullNrow %||% nrow(values$data), big.mark = " ")))))
    }
  })

  output$nrowBox <- shinydashboard::renderValueBox({
    shiny::req(values$data)
    shinydashboard::valueBox(
      format(values$fullNrow %||% nrow(values$data), big.mark = " "),
      if (isTRUE(values$isSampled)) "Lignes (jeu complet)" else "Lignes",
      icon = shiny::icon("list"), color = "teal"
    )
  })

  output$ncolBox <- shinydashboard::renderValueBox({
    shiny::req(values$data)
    shinydashboard::valueBox(
      values$fullNcol %||% ncol(values$data), "Colonnes", icon = shiny::icon("columns"),
      color = "teal"
    )
  })

  output$naBox <- shinydashboard::renderValueBox({
    shiny::req(values$data)
    na_count <- values$fullNA
    if (is.null(na_count) || is.na(na_count)) na_count <- sum(is.na(values$data))
    shinydashboard::valueBox(
      format(na_count, big.mark = " "), "Valeurs manquantes", icon = shiny::icon("question"),
      color = ifelse(na_count > 0, "red", "green")
    )
  })

  output$memBox <- shinydashboard::renderValueBox({
    shiny::req(values$data)
    if (identical(values$dataMode, "duckdb")) {
      shinydashboard::valueBox(hstat_format_size(values$sourceSize %||% 0), "Taille du fichier",
               icon = shiny::icon("hard-drive"), color = "blue")
    } else {
      shinydashboard::valueBox(format(object.size(values$data), units = "auto"), "Taille mémoire",
               icon = shiny::icon("memory"), color = "blue")
    }
  })
  
  output$preview <- DT::renderDT({
    shiny::req(values$data)
    DT::datatable(utils::head(values$data, 50), options = list(scrollX = TRUE))
  })
  
  # ---- Exploration (module Shiny) ----
  mod_explore_server("explore", values)

  # ---- Nettoyage (module Shiny) ----
  mod_clean_server("clean", values)

  # ---- Filtrage (module Shiny) ----
  mod_filter_server("filter", values)

  # ---- Gestion echantillon de travail (UI dans onglet Chargement) ----
  # Ligne d'information sur l'echantillon courant
  output$sampleInfoLine <- shiny::renderUI({    shiny::req(values$data)
    if (!identical(values$dataMode, "duckdb")) return(NULL)
    full <- values$fullNrow %||% 0
    cur  <- nrow(values$data)
    pct  <- if (full > 0) round(100 * cur / full, 2) else 0
    shiny::div(style = "margin-top:8px; padding:8px 12px; background:#f4f6f8; border-radius:6px;",
      p(style = "margin:0; font-size:13px; color:#2c3e50;",
        shiny::icon("circle-info"),
        shiny::HTML(trf(" Échantillon courant : <b>%s</b> lignes sur <b>%s</b> (%s %% du jeu complet).",
                     format(cur, big.mark = " "), format(full, big.mark = " "), pct))))
  })

  shiny::observeEvent(input$redrawSample, {
    if (!identical(values$dataMode, "duckdb") || is.null(values$dbCon)) {
      shiny::showNotification("Le re-tirage n'est disponible qu'en mode hors-mémoire.",
                       type = "warning")
      return(invisible(NULL))
    }
    n <- as.integer(input$sampleSizeLive %||% HSTAT_SAMPLE_SIZE)
    if (is.na(n) || n < 1000) {
      shiny::showNotification("Taille d'échantillon invalide (minimum 1000).", type = "warning")
      return(invisible(NULL))
    }
    tryCatch({
      shiny::withProgress(message = "Tirage d'un nouvel échantillon (DuckDB)", value = 0.4, {
        hstat_set_seed(input$globalSeed)
        smp <- hstat_duckdb_sample(values$dbCon, values$dbTable, n)
        shiny::incProgress(0.8)
        values$data         <- smp
        values$cleanData    <- smp
        values$filteredData <- smp
        values$isSampled    <- (values$fullNrow %||% nrow(smp)) > nrow(smp)
        shiny::incProgress(1)
      })
      shiny::showNotification(
        trf("Nouvel échantillon de %s lignes. Relancez vos analyses pour en tenir compte.",
                format(nrow(values$data), big.mark = " ")),
        type = "message", duration = 7)
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Tirage"),
                       type = "error", duration = 6)
    })
  })

  # ---- Analyses descriptives (module Shiny) ----
  mod_descriptive_server("descriptive", values)

  # ---- Tableaux croises (module Shiny) ----

  # ---- Visualisation des donnees (module Shiny) ----
  mod_viz_server("visualization", values)

  # ---- Tests statistiques + Post-hoc (module Shiny combine) ----
  mod_tests_server("tests", values)
  mod_correlation_server("corrélation", values)
  mod_design_server("design", values)
  mod_qualitative_server("qualitative", values)
  mod_ai_server("aidecision", values)

  # ==========================================================================
  # CAPTURE AUTOMATIQUE POUR L'AIDE A LA DECISION
  # (les analyses dont les selecteurs vivent dans un module namespace deposent
  #  leur contexte depuis le module : `input$maVariable` n'a pas de sens ici)
  # --------------------------------------------------------------------------
  # Les modules deposent deja leurs resultats dans `values` : il suffit de les
  # y observer. Aucun module n'appelle l'assistance, aucune analyse n'est
  # modifiee, et une analyse ajoutee plus tard sera captee sans rien changer
  # ici du moment qu'elle alimente les memes emplacements.
  # ==========================================================================
  # Le guidage ne s'invite plus a la fin de chaque analyse. Un bandeau greffe
  # sur les douze onglets et une notification repetaient, a chaque resultat
  # depose, une recommandation que l'onglet « Interpretation & aide a la
  # decision » porte deja en entier -- au point de recouvrir les resultats que
  # l'utilisateur venait de calculer. La recommandation reste disponible,
  # elle est demandee et non subie.

  shiny::observeEvent(values$testResultsDF, {
    df <- values$testResultsDF
    if (is.null(df) || !NROW(df)) return()
    titre <- if ("Test" %in% names(df))
      paste(unique(as.character(df$Test)), collapse = " / ") else "Tests statistiques"
    vars <- if ("Variable" %in% names(df)) unique(as.character(df$Variable)) else NULL
    grp  <- if ("Facteur" %in% names(df)) {
      g <- unique(as.character(df$Facteur)); g[!is.na(g) & nzchar(g)]
    } else NULL
    tabs <- list("Resultats des tests" = df)
    if (!is.null(values$normalityResults)) tabs[["Normalite"]] <- values$normalityResults
    if (!is.null(values$homogeneityResults)) tabs[["Homogeneite des variances"]] <- values$homogeneityResults
    if (!is.null(values$chiSqFreqData)) tabs[["Effectifs observes / attendus"]] <- values$chiSqFreqData
    hstat_ai_capture(values, "Tests statistiques", titre, tables = tabs,
                     meta = list(variables = vars, groupe = grp,
                                 `type de test` = values$currentTestType))
  }, ignoreInit = TRUE)

  # LE DECLENCHEUR ETAIT UN CHAMP QUE PERSONNE N'ECRIT. `values$multiResults`
  # n'existe que dans la liste initiale : les comparaisons post-hoc deposent
  # leurs resultats dans `multiResultsMain` (mod_tests.R). L'observateur ne
  # s'est donc JAMAIS declenche, et cette famille d'analyse manquait a
  # l'onglet d'interpretation, au journal de reproductibilite et au rapport --
  # alors que le test de couverture, qui cherche l'APPEL dans le source, la
  # declarait couverte.
  shiny::observeEvent(values$multiResultsMain, {
    df <- values$multiResultsMain
    if (is.null(df) || !NROW(df)) return()
    # Les variables comparees se lisent dans le tableau lui-meme : `multiGroups`
    # n'est pas davantage ecrit, et une meta vide vaut moins que rien.
    vars <- if ("Variable" %in% names(df))
              unique(as.character(df$Variable)) else values$multiGroups
    facteurs <- if ("Facteur" %in% names(df))
                  unique(as.character(df$Facteur)) else NULL
    hstat_ai_capture(values, "Comparaisons multiples", "Comparaisons post-hoc",
      tables = list("Comparaisons" = df),
      meta = list(variables = vars, facteurs = facteurs))
  }, ignoreInit = TRUE)
  mod_timeseries_server("timeseries", values)
  mod_ml_server("ml", values)
  mod_dl_server("dl", values)

  # ---- Analyses multivariees ----
  
  options(lifecycle_verbosity = "quiet")
  suppressMessages(suppressWarnings(library(ggplot2)))
  
  # Theme des graphiques multivaries HISTORIQUES (ACP, HCPC, AFD). Ils
  # passaient tous `ggtheme = theme_minimal()` en dur : le graphique ne
  # s'affichait donc JAMAIS tel que ggplot2 le dessine, quoi qu'on choisisse.
  # Le defaut est desormais ggplot2 lui-meme.
  mv_ggtheme <- function(prefix = NULL) {
    choix <- (if (!is.null(prefix)) input[[paste0(prefix, "_theme")]] else NULL) %||%
             input$mv_theme %||% "gg"
    txt   <- .hstat_num1(input$mv_text_size, HSTAT_GG_BASE_SIZE)
    if (identical(choix, "gg")) ggplot2::theme_grey(base_size = txt)
    else if (identical(choix, "hstat")) ggplot2::theme_minimal(base_size = txt)
    else viz_get_theme(choix, base_size = txt)
  }

  # Reglages de forme des analyses HISTORIQUES, greffes sur le graphique deja
  # construit : sous-titre, position de la legende, grille. Le titre et les
  # libelles d'axes existaient deja, ces trois-la manquaient partout.
  # Un objet qui n'est pas un ggplot (dendrogramme trace en base R) ressort
  # inchange plutot que de faire tomber la sortie.
  mv_legacy <- function(p, prefix) {
    if (!inherits(p, "ggplot")) return(p)
    st <- trimws(as.character(input[[paste0(prefix, "_subtitle")]] %||% "")[1])
    if (nzchar(st)) p <- p + ggplot2::labs(subtitle = st)
    pos <- input[[paste0(prefix, "_legendpos")]] %||% "gg"
    if (!identical(pos, "gg")) p <- p + ggplot2::theme(legend.position = pos)
    gr <- input[[paste0(prefix, "_grid")]] %||% "gg"
    if (identical(gr, "sans")) p <- p + ggplot2::theme(panel.grid = ggplot2::element_blank())
    else if (identical(gr, "majeure"))
      p <- p + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    p
  }

  # LA TAILLE PHYSIQUE EST L'ETAT, LES PIXELS N'EN SONT QUE L'AFFICHAGE
  # ---------------------------------------------------------------------------
  # Un bloc d'export porte trois champs : resolution, largeur et hauteur en
  # pixels. Ce que l'utilisateur regle en realite, c'est la taille de la figure
  # sur le papier ; les pixels s'en deduisent (pixels = pouces x DPI).
  #
  # Tenir la taille physique ici, plutot que de la relire dans les champs a
  # chaque fois, a deux consequences voulues :
  #
  #   - le recalcul des champs ne depend plus de leur valeur precedente ni de
  #     l'ancienne resolution (voir mv_lier_dpi et hstat_px_pour_dpi) ;
  #   - l'export fait pouces x DPI MEME SI l'affichage n'a pas suivi. Monter la
  #     resolution monte alors la finesse dans tous les cas, ce qui est la
  #     promesse faite a l'utilisateur.
  .mv_pouces <- new.env(parent = emptyenv())   # prefixe -> c(largeur, hauteur), en pouces
  .mv_ecrit  <- new.env(parent = emptyenv())   # prefixe -> c(l, h) que NOUS venons d'ecrire

  # Taille physique retenue pour un bloc d'export. Tant qu'elle n'a pas ete
  # relevee (panneau jamais affiche), on la deduit des champs declares.
  mv_pouces_export <- function(prefix, defaut_w_in = 10, defaut_h_in = 7.5) {
    po <- .mv_pouces[[prefix]]
    if (!is.null(po) && length(po) == 2L && all(is.finite(po)) && all(po > 0)) return(po)
    dpi <- .hstat_num1(input[[paste0(prefix, "_dpi")]], 300)
    c(hstat_px_en_pouces(input[[paste0(prefix, "_width")]],  dpi, defaut_w_in),
      hstat_px_en_pouces(input[[paste0(prefix, "_height")]], dpi, defaut_h_in))
  }

  # Dimensions d'export d'un graphique multivarie, en POUCES.
  #
  # Ce qui existait ici derivait la taille du seul DPI, par paliers, et
  # IGNORAIT les champs de largeur/hauteur affiches a cote : les regler ne
  # faisait rien. Pire, le palier « > 300 DPI » REDUISAIT la figure de 10 % --
  # demander plus de finesse rendait l'image plus petite sur le papier.
  mv_dims_export <- function(prefix, defaut_w_in = 10, defaut_h_in = 7.5) {
    po <- mv_pouces_export(prefix, defaut_w_in, defaut_h_in)
    list(dpi    = .hstat_num1(input[[paste0(prefix, "_dpi")]], 300),
         width  = po[1],
         height = po[2])
  }
  
  # NOTE : `createPlotDownloadHandler()` vivait ici. Cette fabrique de
  # gestionnaires de telechargement n'etait appelee NULLE PART -- les exports
  # multivaries passent par mv_dims_export() et par le gestionnaire generique
  # de mv_register(). Un helper mort portant un calcul de dimensions DIFFERENT
  # de celui reellement employe est pire qu'absent : on le corrige en croyant
  # corriger l'export.

  
  
  output$pcaMeansGroupSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    fac_cols <- get_categorical_cols(values$filteredData)
    
    if (length(fac_cols) == 0) {
      return(shiny::div(
        style = "background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 10px 0;",
        p(style = "margin: 0; font-size: 12px; color: #721c24;",
          shiny::icon("exclamation-triangle"), 
          shiny::HTML(" <strong>Attention:</strong> Aucune variable catégorielle (facteur ou texte) disponible pour le groupement."))
      ))
    }
    
    shiny::tagList(
      shiny::selectInput("pcaMeansGroup", "Variable de groupement pour les moyennes:", 
                  choices = fac_cols,
                  selected = fac_cols[1]),
      p(style = "margin: 5px 0 10px 0; font-size: 11px; color: #6c757d;",
        shiny::icon("lightbulb"), 
        " L'ACP sera calculée sur les moyennes de chaque groupe.")
    )
  })
  
  output$pcaEllipseGroupSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    fac_cols <- get_categorical_cols(values$filteredData)
    if (length(fac_cols) == 0) {
      return(p(style = "font-size:11px;color:#721c24;",
               shiny::icon("exclamation-triangle"),
               " Aucune variable catégorielle disponible pour grouper les individus."))
    }
    shiny::selectInput("pcaEllipseGroup", "Variable de groupement (ellipses):",
                choices = fac_cols, selected = fac_cols[1])
  })

  output$pcaVarSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "pcaVars",
      label = "Sélectionnez les variables pour l'ACP :",
      choices = num_cols,
      multiple = TRUE,
      selected = num_cols,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$pcaCollinearityPanel <- shiny::renderUI({
    shiny::req(values$filteredData, input$pcaVars)
    if (length(input$pcaVars) < 2) return(NULL)
    
    pca_data <- tryCatch(
      { d <- values$filteredData[, input$pcaVars, drop = FALSE]
      d[, sapply(d, is.numeric), drop = FALSE] },
      error = function(e) NULL
    )
    if (is.null(pca_data) || ncol(pca_data) < 2) return(NULL)
    
    # Calculer la matrice de corrélation et détecter les paires colinéaires
    R_mat <- safe_cor(pca_data)
    if (is.null(R_mat) || anyNA(R_mat)) return(NULL)
    
    # Paires avec |cor| >= 0.80 (colinéarité modérée à forte)
    pairs_high <- list()
    for (i in seq_len(nrow(R_mat))) {
      for (j in seq_len(ncol(R_mat))) {
        if (i < j && abs(R_mat[i,j]) >= 0.80) {
          pairs_high[[length(pairs_high)+1]] <- list(
            v1 = rownames(R_mat)[i],
            v2 = colnames(R_mat)[j],
            cor = round(R_mat[i,j], 3)
          )
        }
      }
    }
    
    if (length(pairs_high) == 0) return(NULL)
    
    vif_vals <- tryCatch({
      if (ncol(pca_data) >= 3) {
        pca_data_nzv <- remove_zero_var_cols(pca_data)
        if (ncol(pca_data_nzv) < 2) {
          stats::setNames(rep(NA_real_, ncol(pca_data)), names(pca_data))
        } else {
          vv <- sapply(seq_len(ncol(pca_data_nzv)), function(i) {
            y <- pca_data_nzv[[i]]
            x <- remove_zero_var_cols(pca_data_nzv[, -i, drop = FALSE])
            if (ncol(x) == 0) return(Inf)
            r2 <- tryCatch(suppressWarnings(summary(stats::lm(y ~ ., data = x))$r.squared), error = function(e) NA)
            if (is.na(r2) || r2 >= 1) Inf else 1 / (1 - r2)
          })
          stats::setNames(vv, names(pca_data_nzv))
        }
      } else {
        stats::setNames(rep(NA_real_, ncol(pca_data)), names(pca_data))
      }
    }, error = function(e) stats::setNames(rep(NA_real_, ncol(pca_data)), names(pca_data)))
    # vif_vals porte deja ses noms (colonnes effectivement calculees) ; ne pas
    # reassigner names() avec un vecteur de longueur differente.
    
    high_vif <- names(vif_vals[!is.na(vif_vals) & is.finite(vif_vals) & vif_vals > 5])
    # Variables parfaitement colinéaires (|cor|>0.9999)
    perfect_collinear <- unique(unlist(lapply(pairs_high, function(p) {
      if (abs(p$cor) > 0.9999) p$v2 else NULL
    })))
    
    # Suggestions de suppression (union des variables problématiques, priorité VIF > 10)
    suggest_remove <- unique(c(
      perfect_collinear,
      names(vif_vals[!is.na(vif_vals) & is.finite(vif_vals) & vif_vals > 10])
    ))
    
    severity_color <- if (length(perfect_collinear) > 0) "#dc3545"
    else if (length(high_vif) > 0) "#fd7e14"
    else "#ffc107"
    severity_label <- if (length(perfect_collinear) > 0) "Colinéarité parfaite détectée"
    else if (length(high_vif) > 0) "Colinéarité forte (VIF > 5)"
    else "Colinéarité modérée (|r| >= 0.80)"
    
    shiny::tagList(
      shiny::div(
        style = paste0(
          "border: 2px solid ", severity_color, "; border-radius: 6px; ",
          "padding: 12px; margin: 8px 0; background-color: white;"
        ),
        shiny::div(
          style = paste0(
            "display: flex; align-items: center; gap: 8px; margin-bottom: 10px; ",
            "color: ", severity_color, ";"
          ),
          shiny::icon("exclamation-triangle"),
          shiny::tags$strong(severity_label)
        ),
        
        shiny::tags$small(
          style = "color: #495057; font-weight: bold; display: block; margin-bottom: 4px;",
          "Paires de variables corrélées :"
        ),
        shiny::tags$table(
          class = "table table-sm table-condensed",
          style = "font-size: 11px; margin-bottom: 8px;",
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th("Variable 1"), shiny::tags$th("Variable 2"),
              shiny::tags$th("r"), shiny::tags$th("Interprétation")
            )
          ),
          shiny::tags$tbody(
            lapply(pairs_high, function(p) {
              level <- if (abs(p$cor) > 0.9999) "Parfaite"
              else if (abs(p$cor) >= 0.90) "Très forte"
              else if (abs(p$cor) >= 0.80) "Forte"
              else "Modérée"
              col   <- if (abs(p$cor) > 0.9999) "#dc3545"
              else if (abs(p$cor) >= 0.90) "#fd7e14"
              else "#6c757d"
              shiny::tags$tr(
                shiny::tags$td(shiny::tags$code(p$v1)),
                shiny::tags$td(shiny::tags$code(p$v2)),
                shiny::tags$td(shiny::tags$strong(style = paste0("color:", col), p$cor)),
                shiny::tags$td(style = paste0("color:", col), level)
              )
            })
          )
        ),
        
        if (!all(is.na(vif_vals))) {
          shiny::tagList(
            shiny::tags$small(
              style = "color: #495057; font-weight: bold; display: block; margin-bottom: 4px;",
              "Facteur d'Inflation de la Variance (VIF) :"
            ),
            shiny::div(
              style = "display: flex; flex-wrap: wrap; gap: 4px; margin-bottom: 10px;",
              lapply(names(vif_vals), function(v) {
                vv <- vif_vals[v]
                col <- if (is.infinite(vv) || vv > 10) "#dc3545"
                else if (vv > 5) "#fd7e14"
                else "#28a745"
                lbl <- if (is.infinite(vv)) "Inf" else round(vv, 1)
                shiny::tags$span(
                  style = paste0(
                    "background:", col, "; color:white; border-radius:3px; ",
                    "padding:2px 6px; font-size:11px;"
                  ),
                  paste0(v, ": ", lbl)
                )
              })
            ),
            shiny::tags$small(
              style = "color: #6c757d; display: block; margin-bottom: 8px;",
              "VIF < 5 : acceptable · VIF 5-10 : élevé · VIF > 10 : très élevé (rouge)"
            )
          )
        },
        
        shiny::hr(style = "margin: 8px 0;"),
        
        shiny::tags$strong(style = "font-size: 12px; color: #495057;",
                    shiny::icon("tools"), " Corriger la multicolinéarité :"),
        shiny::div(
          style = "margin-top: 8px; display: flex; flex-direction: column; gap: 6px;",
          
          if (length(suggest_remove) > 0) {
            shiny::actionButton(
              "pcaAutoRemoveCollinear",
              shiny::tagList(shiny::icon("magic"),
                      trf(" Supprimer automatiquement les %d variable(s) suggérée(s)",
                              length(suggest_remove))),
              class = "btn-sm btn-warning btn-block",
              style = "font-size: 11px; white-space: normal; text-align: left;"
            )
          },
          
          shiny::actionButton(
            "pcaForceStandardize",
            shiny::tagList(shiny::icon("balance-scale"), " Forcer la standardisation"),
            class = "btn-sm btn-outline-secondary btn-block",
            style = "font-size: 11px; white-space: normal; text-align: left;"
          )
        ),
        
        if (length(suggest_remove) > 0) {
          shiny::div(
            style = "margin-top: 8px; padding: 8px 10px; background: #fff8e1; border-radius: 6px; font-size: 11px; color: #6c757d; word-break: break-word;",
            shiny::icon("info-circle"),
            shiny::tags$b(" Variables suggérées à retirer : "),
            shiny::tags$span(paste(suggest_remove, collapse = ", ")),
            shiny::tags$br(),
            shiny::tags$span(style = "font-style: italic;",
              "Vous pouvez aussi les désélectionner manuellement ci-dessus.")
          )
        }
      )
    )
  })
  
  # Supprimer automatiquement les variables colinéaires de la sélection ACP
  shiny::observeEvent(input$pcaAutoRemoveCollinear, {
    shiny::req(values$filteredData, input$pcaVars)
    pca_data <- values$filteredData[, input$pcaVars, drop = FALSE]
    pca_data <- pca_data[, sapply(pca_data, is.numeric), drop = FALSE]
    
    R_mat <- safe_cor(pca_data)
    if (is.null(R_mat)) return()
    
    to_remove <- c()
    for (i in seq_len(nrow(R_mat))) {
      for (j in seq_len(ncol(R_mat))) {
        if (i < j && abs(R_mat[i,j]) > 0.9999)
          to_remove <- c(to_remove, colnames(R_mat)[j])
      }
    }
    if (ncol(pca_data) >= 3) {
      pca_nzv <- remove_zero_var_cols(pca_data)
      vif_v <- sapply(seq_len(ncol(pca_nzv)), function(i) {
        y <- pca_nzv[[i]]; x <- remove_zero_var_cols(pca_nzv[,-i, drop=FALSE])
        if (ncol(x) == 0) return(Inf)
        r2 <- tryCatch(suppressWarnings(summary(stats::lm(y~., data=x))$r.squared), error=function(e) NA)
        if (is.na(r2)||r2>=1) Inf else 1/(1-r2)
      })
      names(vif_v) <- names(pca_nzv)
      to_remove <- unique(c(to_remove, names(vif_v[is.finite(vif_v) & vif_v > 10])))
    }
    
    new_vars <- setdiff(input$pcaVars, to_remove)
    if (length(new_vars) < 2) {
      shiny::showNotification(
        "Impossible de supprimer toutes ces variables (minimum 2 requis). Désélectionnez manuellement.",
        type = "warning", duration = 6)
      return()
    }
    
    updatePickerInput(session, "pcaVars", selected = new_vars)
    shiny::showNotification(
      paste0("Variables retirées : ", paste(to_remove, collapse = ", "),
             ". Relancez l'ACP."),
      type = "message", duration = 6)
  })
  
  shiny::observeEvent(input$pcaForceStandardize, {
    shiny::updateCheckboxInput(session, "pcaScale", value = TRUE)
    shiny::showNotification(
      "Standardisation activée. La standardisation réduit l'impact des différences d'échelle mais ne résout pas la colinéarité.",
      type = "message", duration = 6)
  })
  
  output$pcaQualiSupSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    fac_cols <- get_categorical_cols(values$filteredData)
    if (length(fac_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "pcaQualiSup",
      label = "Variables qualitatives supplémentaires :",
      choices = fac_cols,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  output$pcaQuantiSupSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    # On propose comme suppl. quantitatives les variables numeriques NON actives.
    avail <- setdiff(num_cols, input$pcaVars %||% character(0))
    pickerInput(
      inputId = "pcaQuantiSup",
      label = "Variables quantitatives supplémentaires (optionnel) :",
      choices = avail,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  output$pcaIndSupSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    n <- nrow(values$filteredData)
    if (is.null(n) || n == 0) return(NULL)
    # Choix = numeros de ligne, mais avec une etiquette lisible si une source de
    # labels est choisie (ex. la Zone), pour reconnaitre les individus.
    rn <- as.character(seq_len(n))
    labs <- rn
    src <- input$pcaLabelSource
    if (!is.null(src) && src != "rownames" && src %in% names(values$filteredData)) {
      lv <- as.character(values$filteredData[[src]])
      labs <- paste0(rn, " — ", lv)
    }
    choices <- stats::setNames(rn, labs)
    pickerInput(
      inputId = "pcaIndSup",
      label = "Individus supplémentaires (optionnel) — par n° de ligne :",
      choices = choices,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE,
                     `none-selected-text` = "Aucun individu supplémentaire")
    )
  })
  
  output$pcaLabelSourceSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    all_cols <- names(values$filteredData)
    shiny::selectInput("pcaLabelSource", "Source des labels pour individus (optionnel):",
                choices = c("Rownames" = "rownames", all_cols), selected = "rownames")
  })
  
  calculate_group_means <- function(data, vars, group_var) {
    if (is.null(group_var) || !group_var %in% names(data)) {
      return(data)
    }
    
    means_data <- data %>%
      dplyr::group_by(!!ggplot2::sym(group_var)) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(vars), mean, na.rm = TRUE), .groups = 'drop') %>%
      tibble::column_to_rownames(group_var)
    
    return(means_data)
  }
  
  pcaResultReactive <- shiny::reactive({
    shiny::req(values$filteredData, input$pcaVars)
    shiny::req(mv_launch$pca)
    
    input$pcaScale
    input$pcaUseMeans
    input$pcaMeansGroup
    input$pcaQualiSup
    input$pcaQuantiSup
    input$pcaIndSup
    input$pcaComponents
    input$pcaLabelSource
    input$pcaRefresh  
    
    tryCatch({
      use_means <- !is.null(input$pcaUseMeans) && input$pcaUseMeans && 
        !is.null(input$pcaMeansGroup) && input$pcaMeansGroup != ""
      
      if (use_means) {
        pca_data <- calculate_group_means(values$filteredData, input$pcaVars, input$pcaMeansGroup)
        n_groups <- nrow(pca_data)
        shiny::showNotification(
          paste0("ACP sur moyennes: ", n_groups, " groupes (", input$pcaMeansGroup, ")"), 
          type = "message", 
          duration = 3,
          id = "pca_means_notif"
        )
      } else {
        pca_data <- values$filteredData[, input$pcaVars, drop = FALSE]
      }

      if (!use_means) {
        fdata <- values$filteredData
        # Variables ACTIVES : on ne garde que les colonnes NUMERIQUES parmi pcaVars.
        # Toute variable categorielle selectionnee comme active est automatiquement
        # basculee en variable qualitative supplementaire (evite l'erreur
        # "The following variables are not quantitative").
        active_all <- input$pcaVars
        active_num <- active_all[vapply(fdata[, active_all, drop = FALSE], is.numeric, logical(1))]
        auto_quali <- setdiff(active_all, active_num)
        if (length(active_num) < 2) {
          shiny::showNotification("ACP : sélectionnez au moins 2 variables numériques actives.",
                           type = "error", duration = 6); return(NULL)
        }
        # Suppl. qualitatives (selecteur) + categorielles actives reclassees
        quali_sup_vars  <- unique(c(intersect(input$pcaQualiSup %||% character(0), names(fdata)), auto_quali))
        quali_sup_vars  <- setdiff(quali_sup_vars, active_num)
        # Suppl. quantitatives (nouveau selecteur), exclues des actives
        quanti_sup_vars <- setdiff(intersect(input$pcaQuantiSup %||% character(0), names(fdata)),
                                   c(active_num, quali_sup_vars))
        if (length(auto_quali) > 0)
          shiny::showNotification(trf("ACP : variable(s) non numérique(s) traitée(s) comme qualitative(s) supplémentaire(s) : %s.",
                                   paste(auto_quali, collapse = ", ")), type = "message", duration = 5)

        # Assemblage : actives (num) + quanti.sup (num) + quali.sup (cat)
        ordered_cols <- c(active_num, quanti_sup_vars, quali_sup_vars)
        all_data <- fdata[, ordered_cols, drop = FALSE]
        for (v in quali_sup_vars) all_data[[v]] <- factor(all_data[[v]])

        # Indices (1-based) dans all_data
        quanti_sup_indices <- if (length(quanti_sup_vars))
          match(quanti_sup_vars, names(all_data)) else NULL
        quali_sup_indices <- if (length(quali_sup_vars))
          match(quali_sup_vars, names(all_data)) else NULL

        # Individus supplementaires : accepte des numeros de ligne ou des noms.
        ind_sup_indices <- NULL
        if (!is.null(input$pcaIndSup) && length(input$pcaIndSup) > 0) {
          rn <- rownames(all_data)
          by_name <- which(rn %in% as.character(input$pcaIndSup))
          by_num  <- suppressWarnings(as.integer(input$pcaIndSup))
          by_num  <- by_num[!is.na(by_num) & by_num >= 1 & by_num <= nrow(all_data)]
          ind_sup_indices <- unique(c(by_name, by_num))
          if (length(ind_sup_indices) == 0) ind_sup_indices <- NULL
        }

        # Labels personnalises des individus
        if (!is.null(input$pcaLabelSource) && input$pcaLabelSource != "rownames" &&
            input$pcaLabelSource %in% names(fdata)) {
          rownames(all_data) <- make.unique(as.character(fdata[[input$pcaLabelSource]]))
        }
      } else {
        all_data <- pca_data
        quali_sup_indices <- NULL; quanti_sup_indices <- NULL; ind_sup_indices <- NULL
      }
      
      # - Garde contre la singularité : supprimer les colonnes numériques
      # quasi-colinéaires avant de passer à PCA() pour éviter solve.default crash
      num_cols <- sapply(all_data, is.numeric)
      if (sum(num_cols) >= 2) {
        R_mat <- safe_cor(all_data[, num_cols, drop = FALSE])
        if (!is.null(R_mat) && !anyNA(R_mat)) {
          to_drop <- c()
          for (ci in seq_len(ncol(R_mat))) {
            for (cj in seq_len(ncol(R_mat))) {
              if (ci < cj && !names(all_data[, num_cols, drop=FALSE])[ci] %in% to_drop) {
                if (abs(R_mat[ci, cj]) > 0.9999) to_drop <- c(to_drop,
                                                              names(all_data[, num_cols, drop=FALSE])[cj])
              }
            }
          }
          if (length(to_drop) > 0) {
            shiny::showNotification(
              paste0("ACP : ", paste(to_drop, collapse = ", "),
                     " est parfaitement corrélée à une autre variable et a été écartée du calcul ",
                     "(une variable redondante fausserait l'ACP). Vous pouvez la désélectionner dans la liste des variables."),
              type = "message", duration = 5)
            all_data <- all_data[, !names(all_data) %in% to_drop, drop = FALSE]
            # Recalculer TOUS les indices et la liste des actives apres suppression
            if (!use_means) {
              active_num <- setdiff(active_num, to_drop)
              quanti_sup_vars <- setdiff(quanti_sup_vars, to_drop)
              quali_sup_vars  <- setdiff(quali_sup_vars, to_drop)
              quanti_sup_indices <- if (length(quanti_sup_vars)) match(quanti_sup_vars, names(all_data)) else NULL
              quali_sup_indices  <- if (length(quali_sup_vars))  match(quali_sup_vars, names(all_data)) else NULL
            }
          }
          # Vérifier que la matrice reste inversible (det != 0)
          num_only <- all_data[, sapply(all_data, is.numeric), drop = FALSE]
          if (ncol(num_only) >= 2) {
            det_val <- tryCatch(det(suppressWarnings(safe_cor(num_only, use = "complete.obs")) %||% diag(ncol(num_only))), error = function(e) NA)
            if (!is.na(det_val) && abs(det_val) < 1e-10) {
              shiny::showNotification(
                "ACP : la matrice de corrélation est singulière même après nettoyage. Essayez de réduire le nombre de variables.",
                type = "warning", duration = 8)
            }
          }
        }
      }
      
      n_num_remaining <- if (use_means) sum(sapply(all_data, is.numeric)) else length(active_num)
      if (n_num_remaining < 2) {
        shiny::showNotification("ACP : au moins 2 variables numériques sont nécessaires.", type = "error", duration = 6)
        return(NULL)
      }
      
      res.pca <- suppressWarnings(suppressMessages(
        FactoMineR::PCA(all_data,
            scale.unit = ifelse(is.null(input$pcaScale), TRUE, input$pcaScale),
            quali.sup  = quali_sup_indices,
            quanti.sup = quanti_sup_indices,
            ind.sup    = ind_sup_indices,
            ncp        = ifelse(is.null(input$pcaComponents), 5, input$pcaComponents),
            graph      = FALSE)
      ))
      
      return(res.pca)
      
    }, error = function(e) {
      msg <- e$message
      if (grepl("singular|singulier|invertible|dgesv", msg, ignore.case = TRUE)) {
        shiny::showNotification(
          paste0("ACP : matrice singulière -- variables trop colinéaires. ",
                 "Réduisez le nombre de variables ou désactivez l'option 'Centrer/Réduire'."),
          type = "error", duration = 10)
      } else {
        shiny::showNotification(paste("Erreur ACP :", msg), type = "error")
      }
      return(NULL)
    })
  })
  
  shiny::observe({
    res <- pcaResultReactive()
    if (!is.null(res)) {
      values$pcaResult <- res
    }
  })
  
  pcaDataframes <- shiny::reactive({
    shiny::req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    
    tryCatch({
      eigenvalues_df <- as.data.frame(factoextra::get_eigenvalue(res.pca))
      eigenvalues_df <- cbind(Dimension = rownames(eigenvalues_df), eigenvalues_df)
      rownames(eigenvalues_df) <- NULL
      
      ind_coords_df <- as.data.frame(res.pca$ind$coord)
      ind_coords_df <- cbind(Individual = rownames(ind_coords_df), ind_coords_df)
      rownames(ind_coords_df) <- NULL
      
      ind_contrib_df <- as.data.frame(res.pca$ind$contrib)
      ind_contrib_df <- cbind(Individual = rownames(ind_contrib_df), ind_contrib_df)
      rownames(ind_contrib_df) <- NULL
      
      ind_cos2_df <- as.data.frame(res.pca$ind$cos2)
      ind_cos2_df <- cbind(Individual = rownames(ind_cos2_df), ind_cos2_df)
      rownames(ind_cos2_df) <- NULL
      
      var_coords_df <- as.data.frame(res.pca$var$coord)
      var_coords_df <- cbind(Variable = rownames(var_coords_df), var_coords_df)
      rownames(var_coords_df) <- NULL
      
      var_contrib_df <- as.data.frame(res.pca$var$contrib)
      var_contrib_df <- cbind(Variable = rownames(var_contrib_df), var_contrib_df)
      rownames(var_contrib_df) <- NULL
      
      var_cos2_df <- as.data.frame(res.pca$var$cos2)
      var_cos2_df <- cbind(Variable = rownames(var_cos2_df), var_cos2_df)
      rownames(var_cos2_df) <- NULL
      
      var_cor_df <- as.data.frame(res.pca$var$cor)
      var_cor_df <- cbind(Variable = rownames(var_cor_df), var_cor_df)
      rownames(var_cor_df) <- NULL
      
      return(list(
        eigenvalues = eigenvalues_df,
        ind_coords = ind_coords_df,
        ind_contrib = ind_contrib_df,
        ind_cos2 = ind_cos2_df,
        var_coords = var_coords_df,
        var_contrib = var_contrib_df,
        var_cos2 = var_cos2_df,
        var_cor = var_cor_df
      ))
      
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur dataframes ACP"), type = "error")
      return(NULL)
    })
  })
  
  # Stocker les dataframes de l'ACP dans pour un acces fiable
  shiny::observe({
    shiny::req(pcaResultReactive())
    tryCatch({
      dfs <- pcaDataframes()
      if (!is.null(dfs)) {
        values$pcaDataframes <- dfs
        shiny::showNotification("Dataframes ACP mis en cache", type = "message", duration = 2)
      }
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur stockage ACP"), type = "warning")
    })
  })
  
  output$pcaCTRAxisSelect <- shiny::renderUI({
    shiny::req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims  <- ncol(res.pca$ind$coord)
    shiny::selectInput("pcaCTRAxis", "Composante à analyser:",
                choices = stats::setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = 1)
  })
  
  output$pcaAxisXSelect <- shiny::renderUI({
    shiny::req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims <- ncol(hstat_coord_mat(res.pca$ind$coord))
    
    shiny::selectInput("pcaAxisX", "Axe X:",
                choices = stats::setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = 1)
  })
  
  output$pcaAxisYSelect <- shiny::renderUI({
    shiny::req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims <- ncol(hstat_coord_mat(res.pca$ind$coord))
    
    shiny::selectInput("pcaAxisY", "Axe Y:",
                choices = stats::setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = min(2, n_dims))
  })
  
  output$pcaColorByLegend <- shiny::renderUI({
    color_choice <- if (!is.null(input$pcaColorBy)) input$pcaColorBy else "contrib"
    desc <- switch(color_choice,
                   "contrib" = list(
                     txt  = "Contribution (%) : part de chaque élément dans la construction de la composante. Plus la contribution est élevée (rouge), plus l'élément structure l'axe.",
                     col  = "#e65100", icon = "percentage"
                   ),
                   "cos2" = list(
                     txt  = "Cos² (qualité de représentation) : indique dans quelle mesure l'élément est bien représenté sur le plan factoriel (0 = mal représenté, 1 = parfaitement représenté).",
                     col  = "#1565c0", icon = "bullseye"
                   ),
                   "sat" = list(
                     txt  = "Indice de saturation (|corrélation|) : corrélation absolue entre la variable et les axes affichés. Proche de 1 = variable fortement liée au plan, proche de 0 = variable indépendante du plan.",
                     col  = "#4a148c", icon = "link"
                   ),
                   list(txt = "", col = "#555", icon = "info-circle")
    )
    shiny::div(style = paste0("margin-top:4px; padding:6px 10px; background:white; border-radius:4px; border-left:3px solid ", desc$col, "; font-size:11px; color:#444;"),
        shiny::icon(desc$icon), " ", desc$txt)
  })
  
  output$pcaConditionsCheck <- shiny::renderUI({
    shiny::req(values$filteredData)
    
    n_obs  <- nrow(values$filteredData)
    p_vars <- if (!is.null(input$pcaVars)) length(input$pcaVars) else
      sum(sapply(values$filteredData, is.numeric))
    
    cond_n_min <- 30
    cond_n_rec <- max(50, 5 * max(p_vars, 1))
    cond_p_min <- 2
    cond_p_rec <- 3
    
    make_badge <- function(ok, warn, label) {
      if (ok)   shiny::div(style="display:inline-block;background:#27ae60;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", shiny::icon("check"), label)
      else if (warn) shiny::div(style="display:inline-block;background:#f39c12;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", shiny::icon("exclamation-triangle"), label)
      else      shiny::div(style="display:inline-block;background:#e74c3c;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", shiny::icon("times-circle"), label)
    }
    
    n_ok   <- n_obs >= cond_n_rec
    n_warn <- !n_ok && n_obs >= cond_n_min
    n_err  <- n_obs < cond_n_min
    
    p_ok   <- p_vars >= cond_p_rec
    p_warn <- !p_ok && p_vars >= cond_p_min
    p_err  <- p_vars < cond_p_min
    
    all_ok <- n_ok && p_ok
    any_err <- n_err || p_err
    
    border_col <- if (any_err) "#e74c3c" else if (!all_ok) "#f39c12" else "#27ae60"
    bg_col     <- if (any_err) "#fdf0ef" else if (!all_ok) "#fef9ec" else "#eafaf1"
    
    msgs <- list()
    if (n_err)   msgs <- c(msgs, list(shiny::tagList(shiny::icon("times-circle", style="color:#c0392b;"), paste0(" Effectif critique : n=", n_obs, " < ", cond_n_min, " (minimum absolu). L'ACP peut être instable ou non interprétable."))))
    else if (!n_ok) msgs <- c(msgs, list(shiny::tagList(shiny::icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Effectif faible : n=", n_obs, " (recommandé min. ", cond_n_rec, " = 5xp). Résultats à interpréter avec prudence."))))
    if (p_err)   msgs <- c(msgs, list(shiny::tagList(shiny::icon("times-circle", style="color:#c0392b;"), paste0(" Variables insuffisantes : p=", p_vars, " < ", cond_p_min, " minimum. Sélectionnez au moins 2 variables."))))
    else if (!p_ok) msgs <- c(msgs, list(shiny::tagList(shiny::icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Peu de variables : p=", p_vars, " (recommandé min. ", cond_p_rec, "). L'ACP sera limitée."))))
    
    shiny::tagList(
      shiny::hr(style="margin:8px 0;"),
      shiny::div(style = paste0("border:2px solid ", border_col, "; border-radius:6px; padding:8px 12px; background:", bg_col, ";"),
          shiny::div(style="margin-bottom:6px;",
              shiny::tags$b(style="font-size:12px; color:#2c3e50;", shiny::icon("clipboard-check"), " Vérification des conditions -- ACP"),
              shiny::tags$br(),
              make_badge(n_ok, n_warn, paste0("n = ", n_obs, " observations")),
              make_badge(p_ok, p_warn, paste0("p = ", p_vars, " variables"))
          ),
          if (length(msgs) > 0)
            shiny::tagList(
              lapply(msgs, function(m)
                p(style="margin:3px 0; font-size:11px; color:#555;", m)
              ),
              if (any_err)
                shiny::div(style="margin-top:6px; padding:5px 10px; background:rgba(231,76,60,0.1); border-radius:4px;",
                    p(style="margin:0; font-size:11px; color:#c0392b; font-weight:bold;",
                      shiny::icon("exclamation-triangle"),
                      " Conditions non remplies -- vous pouvez tout de même lancer l'analyse, mais les résultats seront à interpréter avec précaution.")
                )
            )
      )
    )
  })
  
  output$hcpcConditionsCheck <- shiny::renderUI({
    shiny::req(values$filteredData)
    
    n_obs <- nrow(values$filteredData)
    k     <- if (!is.null(input$hcpcClusters)) input$hcpcClusters else 3
    
    n_comp_retained <- tryCatch({
      res <- pcaResultReactive()
      if (is.null(res)) return(NA_integer_)
      sum(factoextra::get_eigenvalue(res)[, 1] >= 1)
    }, error = function(e) NA_integer_)
    
    cond_n_min <- 2 * k
    cond_n_rec <- 10 * k
    
    make_badge <- function(ok, warn, label) {
      if (ok)   shiny::div(style="display:inline-block;background:#27ae60;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", shiny::icon("check"), label)
      else if (warn) shiny::div(style="display:inline-block;background:#f39c12;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", shiny::icon("exclamation-triangle"), label)
      else      shiny::div(style="display:inline-block;background:#e74c3c;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", shiny::icon("times-circle"), label)
    }
    
    n_ok   <- n_obs >= cond_n_rec
    n_warn <- !n_ok && n_obs >= cond_n_min
    n_err  <- n_obs < cond_n_min
    
    comp_ok   <- !is.na(n_comp_retained) && n_comp_retained >= 2
    comp_warn <- !is.na(n_comp_retained) && n_comp_retained == 1
    
    msgs <- list()
    if (n_err)  msgs <- c(msgs, list(shiny::tagList(shiny::icon("times-circle", style="color:#c0392b;"), paste0(" Effectif critique : n=", n_obs, " < ", cond_n_min, " = 2xk. Classification impossible."))))
    else if (!n_ok) msgs <- c(msgs, list(shiny::tagList(shiny::icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Effectif faible : n=", n_obs, " (recommandé min. ", cond_n_rec, " = 10xk). Stabilité réduite."))))
    if (!is.na(n_comp_retained)) {
      if (comp_warn) msgs <- c(msgs, list(shiny::tagList(shiny::icon("exclamation-triangle", style="color:#b7770d;"), " Seulement 1 composante ACP retenue (valeur propre min. 1). Recommandé : min. 2 composantes pour une classification robuste.")))
      if (!comp_ok && !comp_warn) msgs <- c(msgs, list(shiny::tagList(shiny::icon("times-circle", style="color:#c0392b;"), " Aucune composante ACP disponible. Lancez d'abord l'ACP.")))
    }
    
    any_err  <- n_err
    border_col <- if (any_err) "#e74c3c" else if (!n_ok || comp_warn) "#f39c12" else "#27ae60"
    bg_col     <- if (any_err) "#fdf0ef" else if (!n_ok || comp_warn) "#fef9ec" else "#eafaf1"
    
    shiny::tagList(
      shiny::hr(style="margin:8px 0;"),
      shiny::div(style = paste0("border:2px solid ", border_col, "; border-radius:6px; padding:8px 12px; background:", bg_col, ";"),
          shiny::div(style="margin-bottom:6px;",
              shiny::tags$b(style="font-size:12px; color:#2c3e50;", shiny::icon("clipboard-check"), " Vérification des conditions -- HCPC"),
              shiny::tags$br(),
              make_badge(n_ok, n_warn, paste0("n = ", n_obs, " obs.")),
              make_badge(TRUE, FALSE, paste0("k = ", k, " clusters")),
              if (!is.na(n_comp_retained))
                make_badge(comp_ok, comp_warn, paste0(n_comp_retained, " comp. retenue(s)"))
          ),
          if (length(msgs) > 0)
            shiny::tagList(
              lapply(msgs, function(m) p(style="margin:3px 0; font-size:11px; color:#555;", m)),
              if (any_err)
                shiny::div(style="margin-top:6px; padding:5px 10px; background:rgba(231,76,60,0.1); border-radius:4px;",
                    p(style="margin:0; font-size:11px; color:#c0392b; font-weight:bold;",
                      shiny::icon("exclamation-triangle"),
                      " Conditions non remplies -- vous pouvez continuer, mais les résultats peuvent être non fiables."))
            )
      )
    )
  })
  
  output$afdConditionsCheck <- shiny::renderUI({
    shiny::req(values$filteredData)
    
    df <- values$filteredData
    n_obs  <- nrow(df)
    p_vars <- if (!is.null(input$afdVars)) length(input$afdVars) else
      sum(sapply(df, is.numeric))
    
    n_groups <- tryCatch({
      if (!is.null(input$afdFactor) && input$afdFactor %in% names(df)) {
        length(unique(stats::na.omit(df[[input$afdFactor]])))
      } else NA_integer_
    }, error = function(e) NA_integer_)
    
    g <- if (!is.na(n_groups)) n_groups else 2  # estimation par défaut
    
    cond_n_abs  <- p_vars + g     # n > p + g - 1
    cond_n_grp  <- 20 * g         # >= 20 obs par groupe
    cond_p_min  <- 1
    cond_ratio  <- 10 * p_vars    # n/p >= 10
    
    make_badge <- function(ok, warn, label) {
      if (ok)   shiny::div(style="display:inline-block;background:#27ae60;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", shiny::icon("check"), label)
      else if (warn) shiny::div(style="display:inline-block;background:#f39c12;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", shiny::icon("exclamation-triangle"), label)
      else      shiny::div(style="display:inline-block;background:#e74c3c;color:white;border-radius:4px;padding:2px 8px;font-size:11px;margin:2px;", shiny::icon("times-circle"), label)
    }
    
    n_ok_abs  <- n_obs > cond_n_abs
    n_ok_rec  <- n_obs >= cond_n_grp
    n_warn    <- n_ok_abs && !n_ok_rec
    n_err     <- !n_ok_abs
    
    g_ok      <- !is.na(n_groups) && n_groups >= 2
    p_ok      <- p_vars >= 1
    ratio_ok  <- n_obs >= cond_ratio
    ratio_warn<- !ratio_ok && n_obs >= cond_ratio / 2
    
    msgs <- list()
    if (n_err)  msgs <- c(msgs, list(shiny::tagList(shiny::icon("times-circle", style="color:#c0392b;"), paste0(" Effectif insuffisant : n=", n_obs, " inferieur ou egal a p+g=", cond_n_abs, ". L'AFD nécessite n > p + g - 1."))))
    else if (!n_ok_rec) msgs <- c(msgs, list(shiny::tagList(shiny::icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Effectif faible par groupe : n=", n_obs, " pour ", g, " groupes (recommande min. 20 obs/groupe = ", cond_n_grp, " au total)."))))
    if (!is.na(n_groups) && n_groups < 2) msgs <- c(msgs, list(shiny::tagList(shiny::icon("times-circle", style="color:#c0392b;"), " Variable discriminante : moins de 2 groupes détectés. L'AFD requiert g min. 2 groupes distincts.")))
    if (!ratio_ok && !n_err) msgs <- c(msgs, list(shiny::tagList(shiny::icon("exclamation-triangle", style="color:#b7770d;"), paste0(" Ratio n/p faible : n/p = ", round(n_obs/max(p_vars,1),1), " (recommande min. 10). Risque de sur-ajustement."))))
    
    any_err    <- n_err || (!is.na(n_groups) && n_groups < 2)
    border_col <- if (any_err) "#e74c3c" else if (!n_ok_rec || !ratio_ok) "#f39c12" else "#27ae60"
    bg_col     <- if (any_err) "#fdf0ef" else if (!n_ok_rec || !ratio_ok) "#fef9ec" else "#eafaf1"
    
    shiny::tagList(
      shiny::hr(style="margin:8px 0;"),
      shiny::div(style = paste0("border:2px solid ", border_col, "; border-radius:6px; padding:8px 12px; background:", bg_col, ";"),
          shiny::div(style="margin-bottom:6px;",
              shiny::tags$b(style="font-size:12px; color:#2c3e50;", shiny::icon("clipboard-check"), " Vérification des conditions -- AFD"),
              shiny::tags$br(),
              make_badge(n_ok_abs, n_warn, paste0("n = ", n_obs, " observations")),
              make_badge(p_ok, FALSE, paste0("p = ", p_vars, " variables")),
              if (!is.na(n_groups))
                make_badge(g_ok, FALSE, paste0("g = ", n_groups, " groupes")),
              make_badge(ratio_ok, ratio_warn, paste0("n/p = ", round(n_obs/max(p_vars,1),1)))
          ),
          if (length(msgs) > 0)
            shiny::tagList(
              lapply(msgs, function(m) p(style="margin:3px 0; font-size:11px; color:#555;", m)),
              shiny::div(
                style = paste0("margin-top:6px; padding:5px 10px; border-radius:4px; background:", if (any_err) "rgba(231,76,60,0.1);" else "rgba(243,156,18,0.1);"),
                p(style = paste0("margin:0; font-size:11px; font-weight:bold; color:", if(any_err) "#c0392b;" else "#856404;"),
                  shiny::icon("exclamation-triangle"),
                  " Conditions non remplies -- vous pouvez tout de même lancer l'AFD, mais les résultats sont a interpreter avec grande prudence.")
              )
            )
      )
    )
  })
  
  # Force les calques de texte (labels de variables/individus) en noir et gras,
  # pour qu'ils restent lisibles meme quand la couleur d'origine vient d'un degrade
  # clair (contrib/cos2). Les fleches/points gardent leur couleur.
  .mv_darken_text_labels <- function(p, colour = "#1a1a1a") {
    if (is.null(p) || is.null(p$layers)) return(p)
    for (i in seq_along(p$layers)) {
      gcl <- class(p$layers[[i]]$geom)
      if (any(grepl("Text|Label", gcl))) {
        p$layers[[i]]$aes_params$colour <- colour
        p$layers[[i]]$aes_params$fontface <- "bold"
        # neutralise un mapping de couleur eventuel sur le texte
        if (!is.null(p$layers[[i]]$mapping)) {
          p$layers[[i]]$mapping$colour <- NULL
        }
      }
    }
    p
  }

  createPcaPlot <- function(res.pca) {
    
    axis_x <- if (!is.null(input$pcaAxisX)) as.numeric(input$pcaAxisX) else 1
    axis_y <- if (!is.null(input$pcaAxisY)) as.numeric(input$pcaAxisY) else 2
    
    plot_title <- if (!is.null(input$pcaPlotTitle) && input$pcaPlotTitle != "") {
      input$pcaPlotTitle
    } else {
      "ACP - Analyse en Composantes Principales"
    }
    
    color_choice <- if (!is.null(input$pcaColorBy)) input$pcaColorBy else "contrib"
    
    # Calcul de l'indice de saturation (|corrélation| moyenne sur les 2 axes)
    compute_sat_var <- function(res, ax_x, ax_y) {
      cor_mat <- res$var$cor
      n_dim   <- ncol(cor_mat)
      ax_x_s  <- min(ax_x, n_dim)
      ax_y_s  <- min(ax_y, n_dim)
      if (ax_x_s == ax_y_s) return(abs(cor_mat[, ax_x_s]))
      rowMeans(abs(cor_mat[, c(ax_x_s, ax_y_s), drop = FALSE]))
    }
    compute_sat_ind <- function(res, ax_x, ax_y) {
      cos2_mat <- res$ind$cos2
      n_dim    <- ncol(cos2_mat)
      ax_x_s   <- min(ax_x, n_dim)
      ax_y_s   <- min(ax_y, n_dim)
      if (ax_x_s == ax_y_s) return(cos2_mat[, ax_x_s])
      rowSums(cos2_mat[, c(ax_x_s, ax_y_s), drop = FALSE])
    }
    
    col_var <- switch(color_choice,
                      "contrib" = "contrib",
                      "cos2"    = "cos2",
                      "sat"     = compute_sat_var(res.pca, axis_x, axis_y),
                      "contrib"
    )
    col_ind <- switch(color_choice,
                      "contrib" = "contrib",
                      "cos2"    = "cos2",
                      "sat"     = compute_sat_ind(res.pca, axis_x, axis_y),
                      "contrib"
    )
    
    gradient_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
    # Tailles de labels reglees en POINTS par l'utilisateur (12 a 24 pt),
    # converties vers l'unite de ggplot2 (mm) : une taille pour les individus,
    # une autre pour les variables.
    lbl_ind <- hstat_lbl_pt2gg(input$pcaLabelSize)
    lbl_var <- hstat_lbl_pt2gg(input$pcaVarLabelSize)
    n_ind_p <- nrow(res.pca$ind$coord)
    n_var_p <- nrow(res.pca$var$coord)
    pt_sz  <- if (!is.null(input$pcaPointSize)) input$pcaPointSize else 2
    ln_w   <- if (!is.null(input$pcaLineWidth)) input$pcaLineWidth else 0.8

    if (input$pcaPlotType == "var") {
      p <- factoextra::fviz_pca_var(res.pca,
                        axes = c(axis_x, axis_y),
                        col.var = col_var,
                        gradient.cols = gradient_cols,
                        repel = TRUE, max.overlaps = Inf, labelsize = lbl_var,
                        ggtheme = mv_ggtheme("pcaPlot"),
                        title = plot_title)
      # Les labels des variables heritaient du degrade de couleur (contrib/cos2),
      # rendant les noms clairs peu lisibles ("flous"). On force le TEXTE en noir
      # et en gras, tout en gardant les fleches colorees par la metrique.
      p <- .mv_darken_text_labels(p)
      p <- hstat_apply_label_sizes(p, lbl_var)
    } else if (input$pcaPlotType == "ind") {
      p <- factoextra::fviz_pca_ind(res.pca,
                        axes = c(axis_x, axis_y),
                        col.ind = col_ind,
                        gradient.cols = gradient_cols,
                        repel = TRUE, labelsize = lbl_ind, pointsize = pt_sz,
                        ggtheme = mv_ggtheme("pcaPlot"),
                        title = plot_title)
      p <- hstat_apply_label_sizes(p, lbl_ind)
    } else {
      # Biplot : les individus sont colores selon le critere choisi, mais les
      # VARIABLES (fleches + labels) sont forcees en NOIR pour rester visibles
      # (sinon elles se confondent avec le degrade des individus).
      p <- factoextra::fviz_pca_biplot(res.pca,
                           axes = c(axis_x, axis_y),
                           repel = TRUE, labelsize = lbl_ind, pointsize = pt_sz,
                           col.var = "black",
                           col.ind = col_ind,
                           gradient.cols = gradient_cols,
                           ggtheme = mv_ggtheme("pcaPlot"),
                           title = plot_title)
      # Le biplot melange les deux familles de labels : on retaille apres coup
      # les calques de texte des variables, factoextra n'exposant qu'un seul
      # argument `labelsize`.
      p <- hstat_apply_label_sizes(p, lbl_ind, lbl_var, n_var_p, n_ind_p)
    }
    # Epaissir les fleches/segments (largeur des traces)
    p <- p + ggplot2::theme(line = ggplot2::element_line(linewidth = ln_w))
    for (li in seq_along(p$layers)) {
      if (inherits(p$layers[[li]]$geom, c("GeomSegment","GeomPath"))) {
        p$layers[[li]]$aes_params$linewidth <- ln_w
      }
    }
    
    eigenvals <- factoextra::get_eigenvalue(res.pca)
    pc_x_var <- round(eigenvals[axis_x, "variance.percent"], 1)
    pc_y_var <- round(eigenvals[axis_y, "variance.percent"], 1)
    
    x_label <- if (!is.null(input$pcaXLabel) && input$pcaXLabel != "") {
      input$pcaXLabel
    } else {
      paste0("PC", axis_x, " (", pc_x_var, "%)")
    }
    
    y_label <- if (!is.null(input$pcaYLabel) && input$pcaYLabel != "") {
      input$pcaYLabel
    } else {
      paste0("PC", axis_y, " (", pc_y_var, "%)")
    }
    
    p <- p + labs(x = x_label, y = y_label)

    # Ellipses autour des groupes d'individus (si demande et variable choisie)
    if (isTRUE(input$pcaShowEllipses) && !is.null(input$pcaEllipseGroup) &&
        input$pcaPlotType %in% c("ind", "biplot")) {
      grp <- tryCatch(as.factor(values$filteredData[[input$pcaEllipseGroup]]), error = function(e) NULL)
      # Une ellipse de confiance suppose une covariance inversible DANS chaque
      # groupe. Sans ce controle, ggpubr s'arretait sur « Computation failed in
      # stat_conf_ellipse() ... valeur manquante la ou TRUE/FALSE est requis »,
      # message qui ne nomme ni le groupe ni la cause.
      ell <- if (is.null(grp)) list(ok = FALSE, motif = NULL) else
        tryCatch({
          cm <- hstat_coord_mat(res.pca$ind$coord)
          hstat_ellipse_ok(grp, cm[, min(axis_x, ncol(cm))], cm[, min(axis_y, ncol(cm))])
        }, error = function(e) list(ok = FALSE, motif = NULL))
      if (!isTRUE(ell$ok) && !is.null(ell$motif))
        shiny::showNotification(ell$motif, type = "warning", duration = 8,
                         id = "pcaEllipseImpossible")
      if (isTRUE(ell$ok) && !is.null(grp) &&
          length(grp) == nrow(res.pca$ind$coord) && nlevels(grp) >= 2) {
        if (input$pcaPlotType == "biplot") {
          p_ell <- tryCatch(
            factoextra::fviz_pca_biplot(res.pca, axes = c(axis_x, axis_y),
                            habillage = grp, addEllipses = TRUE,
                            ellipse.type = "confidence", ellipse.level = 0.95,
                            col.var = "black", repel = TRUE,
                            labelsize = lbl_ind,
                            ggtheme = mv_ggtheme("pcaPlot"), title = plot_title),
            error = function(e) NULL)
          if (!is.null(p_ell))
            p_ell <- hstat_apply_label_sizes(p_ell, lbl_ind, lbl_var,
                                             n_var_p, n_ind_p)
        } else {
          p_ell <- tryCatch(
            factoextra::fviz_pca_ind(res.pca, axes = c(axis_x, axis_y),
                         geom = "point", habillage = grp, addEllipses = TRUE,
                         ellipse.type = "confidence", ellipse.level = 0.95,
                         labelsize = lbl_ind,
                         repel = FALSE, ggtheme = mv_ggtheme("pcaPlot"), title = plot_title),
            error = function(e) NULL)
          if (!is.null(p_ell)) p_ell <- hstat_apply_label_sizes(p_ell, lbl_ind)
        }
        if (!is.null(p_ell)) p <- p_ell + labs(x = x_label, y = y_label)
      }
    }

    # Styles personnalisables : taille des axes et labels, gris, italique.
    axis_sz   <- if (!is.null(input$pcaAxisTextSize)) input$pcaAxisTextSize else 13
    title_sz  <- if (!is.null(input$pcaAxisTitleSize)) input$pcaAxisTitleSize else 14
    bold_on   <- isTRUE(input$pcaBoldText)
    italic_on <- isTRUE(input$pcaItalicText)
    txt_face  <- if (bold_on && italic_on) "bold.italic" else if (bold_on) "bold" else if (italic_on) "italic" else "plain"
    p <- p + ggplot2::theme(
      axis.title = ggplot2::element_text(size = title_sz, colour = "black", face = txt_face),
      axis.text  = ggplot2::element_text(size = axis_sz, colour = "black", face = txt_face),
      plot.title = ggplot2::element_text(size = title_sz + 2, face = "bold", colour = "black"),
      legend.title = ggplot2::element_text(size = axis_sz),
      legend.text  = ggplot2::element_text(size = axis_sz - 1)
    )

    if (!is.null(input$pcaCenterAxes) && input$pcaCenterAxes) {
      if (input$pcaPlotType == "var") {
        coords <- res.pca$var$coord[, c(axis_x, axis_y)]
      } else {
        coords <- res.pca$ind$coord[, c(axis_x, axis_y)]
      }
      max_range <- max(abs(range(coords, na.rm = TRUE)))
      p <- p + ggplot2::xlim(-max_range, max_range) + ggplot2::ylim(-max_range, max_range)
    }
    
    return(p)
  }
  
  output$pcaPlot <- shiny::renderPlot({
    shiny::req(values$pcaResult)
    p <- tryCatch(
      suppressWarnings(suppressMessages(mv_legacy(createPcaPlot(pcaResultReactive()), "pcaPlot"))),
      error = function(e) {
        shiny::showNotification(hstat_err_fr(e, "Erreur graphique ACP"), type = "error", duration = 8)
        NULL
      }
    )
    shiny::req(!is.null(p))
    p
  }, res = 120)
  
  output$pcaSummary <- shiny::renderPrint({
    shiny::req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    
    use_round <- !is.null(input$pcaRoundResults) && input$pcaRoundResults
    dec <- if (use_round && !is.null(input$pcaDecimals)) input$pcaDecimals else 4
    
    # Affiche un tableau numerique avec EXACTEMENT 'dec' decimales pour
    # chaque valeur (alignement homogene, aucune troncature).
    print_fixed <- function(m, digits) {
      m <- as.matrix(m)
      out <- format(round(m, digits), nsmall = digits, scientific = FALSE,
                    trim = FALSE)
      dimnames(out) <- dimnames(m)
      print(noquote(out))
    }
    
    cat("=== ANALYSE EN COMPOSANTES PRINCIPALES (ACP) ===\n\n")
    
    eigenvals <- factoextra::get_eigenvalue(res.pca)
    cat("Variance expliquee par les composantes principales:\n")
    print_fixed(eigenvals, dec)
    cat("\n")
    
    cat("Contribution des variables aux composantes principales:\n")
    print_fixed(res.pca$var$contrib, dec)
    cat("\n")
    
    cat("Indice de saturation (corrélation variables-axes):\n")
    cat("------------------------------------------------------------\n")
    cat("Corrélation de chaque variable avec chaque axe principal.\n")
    cat("Seuils de lecture (en valeur absolue) :\n")
    cat("  - |saturation| >= 0.70 : variable fortement liee a l'axe\n")
    cat("  - |saturation| 0.50 a 0.70 : liaison modérée\n")
    cat("  - |saturation| < 0.50 : liaison faible\n")
    cat("------------------------------------------------------------\n\n")
    saturations <- res.pca$var$coord
    print_fixed(saturations, dec)
    cat("\n")
    
    # Variable la mieux corrélée a chacun des deux premiers axes
    n_axes <- min(2, ncol(saturations))
    for (ax in seq_len(n_axes)) {
      sat_ax <- saturations[, ax]
      best   <- names(which.max(abs(sat_ax)))
      cat(trf("  Axe %d : variable la plus saturante = %s (saturation = %s)\n",
                  ax, best,
                  format(round(sat_ax[best], dec), nsmall = dec, scientific = FALSE)))
    }
    cat("\n")
    
    cat("Qualite de représentation (cos2) des variables:\n")
    print_fixed(res.pca$var$cos2, dec)
  })
  
  
  # -- 1. Bartlett + KMO (Adéquation des données à l'ACP) --
  output$pcaBartlettKMO <- shiny::renderUI({
    shiny::req(pcaResultReactive(), values$filteredData, input$pcaVars)
    tryCatch({
      pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
      pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
      pca_data_raw <- stats::na.omit(pca_data_raw)
      pca_data_raw <- remove_zero_var_cols(pca_data_raw)
      pca_data_raw <- remove_zero_var_cols(pca_data_raw)
      pca_data_raw <- remove_zero_var_cols(pca_data_raw)
      pca_data_raw <- remove_zero_var_cols(pca_data_raw)
      
      if (ncol(pca_data_raw) < 2 || nrow(pca_data_raw) < 4) {
        return(shiny::div(class = "callout callout-warning",
                   shiny::h4(shiny::icon("exclamation-triangle"), " Données insuffisantes"),
                   p("Au moins 2 variables et 4 observations sont nécessaires.")))
      }
      
      R <- safe_cor(pca_data_raw)
      if (is.null(R)) return(shiny::div(class="callout callout-warning", shiny::h4(shiny::icon("exclamation-triangle"), " Données insuffisantes"), p("Variables à variance nulle détectées -- vérifiez vos données.")))
      n <- nrow(pca_data_raw)
      p <- ncol(pca_data_raw)
      
      bartlett <- suppressWarnings(psych::cortest.bartlett(R, n = n))
      chi2_val  <- round(bartlett$chisq, 3)
      df_val    <- bartlett$df
      p_val     <- bartlett$p.value          # valeur brute, non arrondie
      # Affichage de la p-value : jamais un "0" trompeur pour les tres
      # petites valeurs ; notation decimale complete ou scientifique.
      p_display <- if (is.na(p_val)) {
        "n/d"
      } else if (p_val == 0 || p_val < 1e-15) {
        "< 1e-15"
      } else if (p_val < 1e-4) {
        format(p_val, scientific = TRUE, digits = 4)
      } else {
        format(round(p_val, 6), scientific = FALSE, nsmall = 6)
      }
      
      # KMO (suppressWarnings évite "FUN(min) Inf" quand corrélations <= 0)
      kmo_res  <- suppressWarnings(psych::KMO(R))
      kmo_val  <- round(kmo_res$MSA, 3)
      
      kmo_label <- if (kmo_val <= 0.5) {
        list(txt = "Inacceptable (< 0,5) -- L'ACP n'est pas recommandée sur ces données.", color = "#dc3545", icon = "times-circle")
      } else if (kmo_val <= 0.6) {
        list(txt = "Médiocre (0,5 - 0,6) -- L'ACP est déconseillée.", color = "#e67e22", icon = "exclamation-circle")
      } else if (kmo_val <= 0.7) {
        list(txt = "Acceptable (0,6 - 0,7) -- L'ACP est utilisable avec prudence.", color = "#f39c12", icon = "exclamation-triangle")
      } else if (kmo_val <= 0.8) {
        list(txt = "Souhaitable (0,7 - 0,8) -- L'ACP est appropriée.", color = "#3498db", icon = "check-circle")
      } else if (kmo_val <= 0.9) {
        list(txt = "Bon (0,8 - 0,9) -- L'ACP est bien adaptée.", color = "#27ae60", icon = "check-circle")
      } else {
        list(txt = "Excellent (> 0,9) -- L'ACP est parfaitement adaptée.", color = "#1a5276", icon = "star")
      }
      
      bartlett_interp <- if (p_val < 0.05) {
        list(txt = paste0("Significatif (p = ", p_display, ") -- La matrice de corrélation n'est pas une matrice identité : l'ACP est justifiée."), 
             color = "#27ae60", icon = "check-circle")
      } else {
        list(txt = paste0("Non significatif (p = ", p_display, ") -- Les variables semblent indépendantes. L'ACP n'apportera pas de structure factorielle utile."), 
             color = "#dc3545", icon = "times-circle")
      }
      
      shiny::tagList(
        shiny::div(style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 8px; padding: 18px; margin-bottom: 15px; border: 1px solid #dee2e6;",
            shiny::h5(style = "color: #2c3e50; font-weight: bold; margin-top: 0; border-bottom: 2px solid #3498db; padding-bottom: 8px;",
               shiny::icon("flask"), " Test de sphéricité de Bartlett"),
            p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 10px;",
              "Ce test vérifie si la matrice de corrélation est significativement différente d'une matrice identité. Un résultat significatif (p < 0,05) confirme que les variables sont corrélées entre elles et que l'ACP est pertinente."),
            shiny::fluidRow(
              shiny::column(4, shiny::div(style = "text-align: center; background: white; border-radius: 6px; padding: 10px; border: 1px solid #dee2e6;",
                            p(style = "margin: 0; font-size: 11px; color: #888; text-transform: uppercase;", "Chi² de Bartlett"),
                            shiny::h4(style = "margin: 4px 0; color: #2c3e50; font-weight: bold;", chi2_val))),
              shiny::column(4, shiny::div(style = "text-align: center; background: white; border-radius: 6px; padding: 10px; border: 1px solid #dee2e6;",
                            p(style = "margin: 0; font-size: 11px; color: #888; text-transform: uppercase;", "Degrés de liberté"),
                            shiny::h4(style = "margin: 4px 0; color: #2c3e50; font-weight: bold;", df_val))),
              shiny::column(4, shiny::div(style = "text-align: center; background: white; border-radius: 6px; padding: 10px; border: 1px solid #dee2e6;",
                            p(style = "margin: 0; font-size: 11px; color: #888; text-transform: uppercase;", "p-value"),
                            shiny::h4(style = paste0("margin: 4px 0; font-weight: bold; color: ", bartlett_interp$color, ";"), p_display)))
            ),
            shiny::div(style = paste0("margin-top: 10px; padding: 8px 12px; border-left: 4px solid ", bartlett_interp$color, "; background-color: white; border-radius: 0 4px 4px 0;"),
                p(style = paste0("margin: 0; font-size: 12px; color: ", bartlett_interp$color, ";"),
                  shiny::icon(bartlett_interp$icon), " ", bartlett_interp$txt))
        ),
        shiny::div(style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 8px; padding: 18px; margin-bottom: 15px; border: 1px solid #dee2e6;",
            shiny::h5(style = "color: #2c3e50; font-weight: bold; margin-top: 0; border-bottom: 2px solid #16a085; padding-bottom: 8px;",
               shiny::icon("sliders"), " Indice KMO (Kaiser-Meyer-Olkin)"),
            p(style = "font-size: 12px; color: #555; font-style: italic; margin-bottom: 10px;",
              "Le KMO mesure l'adéquation de l'échantillon. Il compare les corrélations partielles aux corrélations totales. Plus il est proche de 1, plus les données sont adaptées à une ACP."),
            shiny::div(style = "text-align: center; padding: 15px;",
                shiny::div(style = paste0("display: inline-block; background: white; border-radius: 50%; width: 100px; height: 100px; line-height: 100px; font-size: 28px; font-weight: bold; color: white; background-color: ", kmo_label$color, "; box-shadow: 0 4px 8px rgba(0,0,0,0.2);"),
                    kmo_val)
            ),
            shiny::div(style = paste0("margin-top: 10px; padding: 8px 12px; border-left: 4px solid ", kmo_label$color, "; background-color: white; border-radius: 0 4px 4px 0;"),
                p(style = paste0("margin: 0; font-size: 12px; color: ", kmo_label$color, ";"),
                  shiny::icon(kmo_label$icon), " ", kmo_label$txt))
        )
      )
    }, error = function(e) {
      shiny::div(class = "callout callout-danger",
          shiny::tags$p(shiny::tagList(shiny::icon("exclamation-triangle"), " ", hstat_err_fr(e, "Calcul Bartlett/KMO"))))
    })
  })
  
  # -- 2. Scree Plot (Graphique des éboulis) --
  # Fonction interne réutilisable pour créer le scree plot ggplot
  createScreePlot <- function(res.pca) {
    eig_mat <- res.pca$eig  # matrice FactoMineR : colonnes "eigenvalue", "percentage of variance", "cumulative percentage of variance"
    df_eig <- data.frame(
      PC            = factor(rownames(eig_mat), levels = rownames(eig_mat)),
      Valeur_propre = as.numeric(eig_mat[, 1]),
      Variance      = as.numeric(eig_mat[, 2])
    )
    kaiser_threshold <- 1
    n_kaiser <- sum(df_eig$Valeur_propre >= kaiser_threshold)
    
    ggplot2::ggplot(df_eig, ggplot2::aes(x = PC, y = Valeur_propre, group = 1)) +
      ggplot2::geom_line(color = "#2E86AB", linewidth = 1.2) +
      ggplot2::geom_point(ggplot2::aes(color = Valeur_propre >= kaiser_threshold), size = 4) +
      ggplot2::scale_color_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#dc3545"),
                         labels = c("TRUE" = "Retenue (>= 1)", "FALSE" = "Exclue (< 1)"),
                         name = "Critère de Kaiser") +
      ggplot2::geom_hline(yintercept = kaiser_threshold, linetype = "dashed", color = "#e74c3c", size = 0.8) +
      ggplot2::annotate("text", x = 1, y = kaiser_threshold + 0.05 * max(df_eig$Valeur_propre),
               label = "Seuil de Kaiser (\u03bb = 1)", hjust = 0, color = "#e74c3c", size = 3.5, fontface = "italic") +
      ggplot2::geom_text(ggplot2::aes(label = round(Valeur_propre, 2)), vjust = -0.8, size = 3.5, fontface = "bold", color = "#2c3e50") +
      labs(
        title    = "Graphique des \u00e9boulis (Scree Plot)",
        subtitle = paste0(n_kaiser, " composante(s) retenue(s) selon le crit\u00e8re de Kaiser (\u03bb \u2265 1)"),
        x        = "Composante principale",
        y        = "Valeur propre (\u03bb)",
        caption  = "Les composantes en vert ont une valeur propre \u2265 1 et sont retenues pour interpr\u00e9tation."
      ) +
      mv_ggtheme("pcaScree") +
      ggplot2::theme(
        plot.title    = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 14, color = "#2c3e50"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "#555", size = 11),
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }
  
  output$pcaScreePlot <- shiny::renderPlot({
    shiny::req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    mv_legacy(createScreePlot(res.pca), "pcaScree")
  }, res = 120)
  
  output$downloadPcaScreePlot <- shiny::downloadHandler(
    filename = function() paste0("acp_screeplot_", Sys.Date(), ".", hstat_img_fmt(input$pcaScree_format)),
    content = function(file) {
      d <- mv_dims_export("pcaScree", 9.8, 7.1); dpi <- d$dpi
      auto_dims <- d
      p <- mv_legacy(createScreePlot(pcaResultReactive()), "pcaScree")
      hstat_ecrire_image(file, p, input$pcaScree_format,
                         auto_dims$width, auto_dims$height, dpi)
    }
  )
  
  # -- 3. Analyse parallèle --
  createParallelPlot <- function(pca_data_raw, res.pca) {
    n <- nrow(pca_data_raw)
    p <- ncol(pca_data_raw)
    n_iter <- 100
    
    eig_mat       <- res.pca$eig
    eigenvals_obs <- as.numeric(eig_mat[, 1])
    
    # FactoMineR peut renvoyer moins (ncp) ou un nombre de valeurs propres
    # different de p (colonnes a variance nulle, etc.). On aligne tout sur
    # le nombre commun de composantes pour eviter tout decalage de longueur.
    n_pc <- min(length(eigenvals_obs), p)
    if (n_pc < 2)
      stop("Nombre de composantes insuffisant pour l'analyse parallèle (minimum 2).")
    eigenvals_obs <- eigenvals_obs[seq_len(n_pc)]
    
    hstat_set_seed(input$globalSeed)
    sim_eigenvals <- matrix(0, nrow = n_iter, ncol = n_pc)
    for (i in 1:n_iter) {
      random_data <- matrix(rnorm(n * p), nrow = n, ncol = p)
      random_pca  <- prcomp(random_data, scale. = TRUE)
      ev <- random_pca$sdev^2
      sim_eigenvals[i, ] <- ev[seq_len(n_pc)]
    }
    
    mean_sim   <- colMeans(sim_eigenvals)
    perc95_sim <- apply(sim_eigenvals, 2, quantile, probs = 0.95)
    n_retain   <- sum(eigenvals_obs > perc95_sim)
    
    df_plot <- data.frame(
      PC            = seq_len(n_pc),
      Observees     = eigenvals_obs,
      Aleatoire_moy = mean_sim,
      Aleatoire_p95 = perc95_sim
    )
    
    ggplot2::ggplot(df_plot, ggplot2::aes(x = PC)) +
      ggplot2::geom_line(ggplot2::aes(y = Aleatoire_p95, color = "Simulation al\u00e9atoire (p95)"), linetype = "dashed", size = 1.2) +
      ggplot2::geom_line(ggplot2::aes(y = Aleatoire_moy, color = "Simulation al\u00e9atoire (moy)"), linetype = "dotted", size = 0.9) +
      ggplot2::geom_line(ggplot2::aes(y = Observees, color = "Valeurs propres observ\u00e9es"), linewidth = 1.4) +
      ggplot2::geom_point(ggplot2::aes(y = Observees, color = "Valeurs propres observ\u00e9es"), size = 3) +
      ggplot2::geom_hline(yintercept = 1, linetype = "solid", color = "#e74c3c", size = 0.6, alpha = 0.5) +
      ggplot2::scale_color_manual(values = c(
        "Valeurs propres observ\u00e9es" = "#2E86AB",
        "Simulation al\u00e9atoire (p95)"  = "#e74c3c",
        "Simulation al\u00e9atoire (moy)"  = "#f39c12"
      ), name = NULL) +
      ggplot2::scale_x_continuous(breaks = seq_len(n_pc)) +
      labs(
        title    = "Analyse parall\u00e8le de Horn",
        subtitle = paste0(n_retain, " composante(s) \u00e0 retenir (valeurs observ\u00e9es > percentile 95 des simulations, ",
                          n_iter, " it\u00e9rations)"),
        x       = "Num\u00e9ro de la composante",
        y       = "Valeur propre",
        caption = "Les composantes dont la valeur propre observ\u00e9e d\u00e9passe la courbe rouge (p95 al\u00e9atoire) sont \u00e0 retenir."
      ) +
      mv_ggtheme("pcaParallel") +
      ggplot2::theme(
        plot.title    = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 14, color = "#2c3e50"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "#555", size = 11),
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank()
      )
  }
  
  output$pcaParallelPlot <- shiny::renderPlot({
    shiny::req(pcaResultReactive(), values$filteredData, input$pcaVars)
    tryCatch({
      pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
      pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
      pca_data_raw <- stats::na.omit(pca_data_raw)
      
      if (ncol(pca_data_raw) < 2 || nrow(pca_data_raw) < 10) {
        graphics::plot.new()
        text(0.5, 0.5, "Données insuffisantes pour l'analyse parallèle\n(minimum 10 observations requises)",
             cex = 1.2, col = "#e74c3c", adj = c(0.5, 0.5))
        return()
      }
      
      mv_legacy(createParallelPlot(pca_data_raw, pcaResultReactive()), "pcaParallel")
      
    }, error = function(e) {
      graphics::plot.new()
      text(0.5, 0.5, paste(strwrap(hstat_err_fr(e, "Analyse parallèle"), 55), collapse = "\n"),
           cex = 1, col = "#e74c3c", adj = c(0.5, 0.5))
    })
  }, res = 120)
  
  output$downloadPcaParallelPlot <- shiny::downloadHandler(
    filename = function() paste0("acp_analyse_parallele_", Sys.Date(), ".", hstat_img_fmt(input$pcaParallel_format)),
    content = function(file) {
      pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
      pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
      pca_data_raw <- stats::na.omit(pca_data_raw)
      d <- mv_dims_export("pcaParallel", 9.8, 7.1); dpi <- d$dpi
      auto <- d
      p    <- mv_legacy(createParallelPlot(pca_data_raw, pcaResultReactive()), "pcaParallel")
      hstat_ecrire_image(file, p, input$pcaParallel_format,
                         auto$width, auto$height, dpi)
    }
  )
  
  # -- 4. Rotation orthogonale --
  output$pcaRotationResult <- shiny::renderPrint({
    shiny::req(pcaResultReactive(), values$filteredData, input$pcaVars, input$pcaRotationMethod, input$pcaRotationNFactors)
    tryCatch({
      pca_data_raw <- values$filteredData[, input$pcaVars, drop = FALSE]
      pca_data_raw <- pca_data_raw[, sapply(pca_data_raw, is.numeric), drop = FALSE]
      pca_data_raw <- stats::na.omit(pca_data_raw)
      
      
      n_vars    <- ncol(pca_data_raw)
      n_obs     <- nrow(pca_data_raw)
      
      if (n_vars < 2) {
        cat("Moins de 2 variables avec variance non nulle -- rotation impossible.
")
        cat("  Vérifiez que vos variables ne sont pas constantes.
")
        return(invisible(NULL))
      }
      
      n_factors <- max(1, min(input$pcaRotationNFactors, n_vars - 1, max(1, n_obs - 1)))
      method    <- input$pcaRotationMethod
      
      R_mat <- safe_cor(pca_data_raw, use = "complete.obs")
      if (is.null(R_mat) || anyNA(R_mat)) {
        cat("Impossible de calculer la matrice de corrélation (NA persistants après na.omit).
")
        cat("  Conseil : vérifiez vos données (valeurs manquantes, variables constantes).
")
        return(invisible(NULL))
      }
      det_val <- tryCatch(det(R_mat), error = function(e) NA)
      
      is_singular <- !is.na(det_val) && abs(det_val) < 1e-8
      # Si singulière, utiliser "minres" qui est plus robuste que "pa" (pas de SMC)
      fm_method <- if (is_singular) "minres" else "pa"
      
      cat("=== ROTATION", toupper(method), "--", n_factors, "FACTEUR(S) ===

")
      cat("La rotation", method, "est une rotation orthogonale qui conserve l'indépendance des axes.
")
      cat("Elle simplifie la structure factorielle pour faciliter l'interprétation.

")
      if (is_singular) {
        cat("Note : matrice de corrélation quasi-singulière détectée.
")
        cat("  Méthode de factorisation automatiquement ajustée à 'minres' (plus robuste).
")
        cat("  Conseil : réduisez le nombre de variables ou supprimez les doublons.

")
      }
      
      # Appel fa() robuste : suppressWarnings() supprime les warnings C-level de psych
      # (FUN(min) Inf, SMC<0, cov2cor non-fini) qui échappent à withCallingHandlers
      fa_res <- tryCatch({
        suppressWarnings(
          fa(pca_data_raw,
             nfactors = n_factors,
             rotate   = method,
             fm       = fm_method,
             scores   = "regression",
             warnings = FALSE)
        )
      }, error = function(e) {
        tryCatch(
          suppressWarnings(
            fa(pca_data_raw, nfactors = n_factors, rotate = method, fm = "minres",
               scores = "regression", warnings = FALSE)
          ),
          error = function(e2) {
            cat("Échec de la rotation :", e2$message, "
")
            cat("  Conseil : réduisez le nombre de facteurs ou changez de méthode.
")
            NULL
          }
        )
      })
      
      if (is.null(fa_res)) return(invisible(NULL))
      
      if (n_factors >= n_vars) {
        cat("Avertissement : nombre de facteurs trop élevé par rapport aux variables -- cas Heywood possible.
")
        cat("  Conseil : réduisez le nombre de facteurs (essayez", max(1, n_vars - 1), "ou moins).

")
      }
      
      cat("Loadings après rotation", method, ":\n")
      cat("(Seuil de lecture : |loading| >= 0,40 indique une contribution significative)\n\n")
      print(fa_res$loadings, cutoff = 0.0, digits = 3)
      
      cat("\nVariance expliquée après rotation :\n")
      var_tab <- data.frame(
        Facteur = colnames(fa_res$loadings),
        SS_Loadings = round(fa_res$Vaccounted["SS loadings", ], 3),
        Prop_Variance = round(fa_res$Vaccounted["Proportion Var", ], 3),
        Cumul_Variance = round(fa_res$Vaccounted["Cumulative Var", ], 3)
      )
      print(var_tab, row.names = FALSE)
      
      cat("\nIndice de simplicité de la rotation (objectif : simplifier la lecture) :\n")
      cat("  - Plus les loadings sont proches de 0 ou 1 (en valeur absolue), meilleure est la structure.\n")
      cat("  - Les loadings |x| >= 0,70 : contribution forte.\n")
      cat("  - Les loadings 0,40 <= |x| < 0,70 : contribution modérée.\n")
      cat("  - Les loadings |x| < 0,40 : contribution faible (souvent ignorée).\n")
      
    }, error = function(e) {
      cat("Erreur lors de la rotation :", e$message, "\n")
      cat("Conseil : Réduisez le nombre de facteurs ou changez de méthode de rotation.\n")
    })
  })
  
  # -- 5. Contributions absolues (CTR) --
  createCTRPlot <- function(res.pca, axis_num) {
    ctr_vars <- res.pca$var$contrib[, axis_num, drop = FALSE]
    df_ctr <- data.frame(
      Variable = rownames(ctr_vars),
      CTR = ctr_vars[, 1]
    )
    df_ctr <- df_ctr[order(df_ctr$CTR, decreasing = TRUE), ]
    df_ctr$Variable <- factor(df_ctr$Variable, levels = rev(df_ctr$Variable))
    threshold <- 100 / nrow(df_ctr)
    
    ggplot2::ggplot(df_ctr, ggplot2::aes(x = Variable, y = CTR, fill = CTR >= threshold)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", color = "#e74c3c", size = 0.9) +
      ggplot2::annotate("text", x = 0.6, y = threshold + 0.2,
               label = paste0("Seuil th\u00e9orique (", round(threshold, 1), "%)"),
               hjust = 0, color = "#e74c3c", size = 3.5, fontface = "italic") +
      ggplot2::scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#bdc3c7"),
                        labels = c("TRUE" = "Contribution notable", "FALSE" = "Contribution faible"),
                        name = NULL) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        title    = paste0("Contributions absolues (CTR) \u2014 Composante PC", axis_num),
        subtitle = paste0("Seuil th\u00e9orique = 100% / ", nrow(df_ctr), " variables = ",
                          round(threshold, 1), "%. Les variables en vert contribuent au-del\u00e0 du seuil."),
        x       = NULL,
        y       = "Contribution (%)",
        caption = "Une contribution sup\u00e9rieure au seuil th\u00e9orique indique que la variable influence significativement la composante."
      ) +
      mv_ggtheme("pcaCTR") +
      ggplot2::theme(
        plot.title    = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 13, color = "#2c3e50"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "#555", size = 10),
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank()
      )
  }
  
  output$pcaCTRPlot <- shiny::renderPlot({
    shiny::req(pcaResultReactive(), input$pcaCTRAxis)
    mv_legacy(createCTRPlot(pcaResultReactive(), as.numeric(input$pcaCTRAxis)), "pcaCTR")
  }, res = 120)
  
  output$downloadPcaCTRPlot <- shiny::downloadHandler(
    filename = function() paste0("acp_CTR_PC", input$pcaCTRAxis, "_", Sys.Date(), ".", hstat_img_fmt(input$pcaCTR_format)),
    content = function(file) {
      d <- mv_dims_export("pcaCTR", 9.8, 7.1); dpi <- d$dpi
      auto <- d
      p    <- mv_legacy(createCTRPlot(pcaResultReactive(), as.numeric(input$pcaCTRAxis)), "pcaCTR")
      hstat_ecrire_image(file, p, input$pcaCTR_format,
                         auto$width, auto$height, dpi)
    }
  )
  
  
  # ---- Helpers internes 
  
  
  
  # Les deux telechargements (classeur et archive CSV) d'un meme resultat sont
  # declares d'un seul appel : ce qui appartient a l'analyse, c'est la LISTE
  # NOMMEE de tableaux, pas la boucle sur les feuilles ni l'archive.
  hstat_export_tables_handlers(output, "downloadPcaMetrics", function() {
    if (is.null(pcaResultReactive()) || is.null(values$filteredData) ||
        is.null(input$pcaVars)) return(NULL)
    d <- values$filteredData[, input$pcaVars, drop = FALSE]
    d <- stats::na.omit(d[, sapply(d, is.numeric), drop = FALSE])
    dfs <- build_pca_metrics_df(pcaResultReactive(), d)
    if (is.null(dfs)) return(NULL)
    list("Valeurs_propres" = dfs$valeurs_propres,
         "Bartlett_KMO"    = dfs$bartlett_kmo,
         "CTR_variables"   = dfs$ctr_variables,
         "Cos2_variables"  = dfs$cos2_variables)
  }, "acp_metriques", "ACP")
  
  
  hstat_export_tables_handlers(output, "downloadHcpcMetrics", function() {
    vd <- hcpcValidationData()
    if (is.null(vd)) return(NULL)
    dfs <- build_hcpc_metrics_df(vd)
    if (is.null(dfs)) return(NULL)
    list("Indices_validation"   = dfs$indices_validation,
         "Affectation_clusters" = dfs$affectation_clusters)
  }, "hcpc_metriques", "HCPC")
  
  
  hstat_export_tables_handlers(output, "downloadAfdMetrics", function() {
    dfs <- build_afd_metrics_df(afdResultReactive())
    if (is.null(dfs)) return(NULL)
    list("Variance_Eta2"          = dfs$variance_eta2,
         "Classification_globale" = dfs$classification_globale,
         "Classification_groupe"  = dfs$classification_groupe,
         "Matrice_confusion"      = dfs$matrice_confusion)
  }, "afd_metriques", "AFD")
  
  # Helper: construire le dataframe des métriques ACP
  build_pca_metrics_df <- function(res.pca, pca_data_raw) {
    tryCatch({
      eig_mat <- res.pca$eig
      df_eig  <- data.frame(
        Composante           = rownames(eig_mat),
        Valeur_propre        = round(as.numeric(eig_mat[, 1]), 4),
        Variance_pct         = round(as.numeric(eig_mat[, 2]), 4),
        Variance_cumulee_pct = round(as.numeric(eig_mat[, 3]), 4),
        Kaiser_retenue       = as.numeric(eig_mat[, 1]) >= 1
      )
      
      R <- safe_cor(pca_data_raw)
      if (is.null(R)) { return(data.frame()) }
      n <- nrow(pca_data_raw)
      bartlett  <- suppressWarnings(psych::cortest.bartlett(R, n = n))
      kmo_res   <- suppressWarnings(psych::KMO(R))
      
      df_bartlett_kmo <- data.frame(
        Test                     = c("Bartlett Chi2", "Bartlett df", "Bartlett p-value", "KMO MSA"),
        Valeur                   = c(round(bartlett$chisq, 3), bartlett$df,
                                     signif(bartlett$p.value, 6), round(kmo_res$MSA, 3)),
        Interpretation           = c(
          ifelse(bartlett$p.value < 0.05, "Significatif -- ACP justifiée", "Non significatif -- ACP non justifiée"),
          "",
          ifelse(bartlett$p.value < 0.05, "p < 0.05 : matrice != identité", "p >= 0.05 : variables indépendantes"),
          ifelse(kmo_res$MSA > 0.9, "Excellent",
                 ifelse(kmo_res$MSA > 0.8, "Bon",
                        ifelse(kmo_res$MSA > 0.7, "Souhaitable",
                               ifelse(kmo_res$MSA > 0.6, "Acceptable",
                                      ifelse(kmo_res$MSA > 0.5, "Médiocre", "Inacceptable")))))
        )
      )
      
      df_ctr_all <- as.data.frame(res.pca$var$contrib)
      df_ctr_all <- cbind(Variable = rownames(df_ctr_all), round(df_ctr_all, 4))
      rownames(df_ctr_all) <- NULL
      
      df_cos2_vars <- as.data.frame(res.pca$var$cos2)
      df_cos2_vars <- cbind(Variable = rownames(df_cos2_vars), round(df_cos2_vars, 4))
      rownames(df_cos2_vars) <- NULL
      
      list(
        valeurs_propres  = df_eig,
        bartlett_kmo     = df_bartlett_kmo,
        ctr_variables    = df_ctr_all,
        cos2_variables   = df_cos2_vars
      )
    }, error = function(e) NULL)
  }
  
  # Helper: construire le dataframe des métriques AFD
  build_afd_metrics_df <- function(afd_res) {
    tryCatch({
      afd_result  <- afd_res$model
      afd_predict <- afd_res$predictions
      afd_data    <- afd_res$data
      
      eigenvals <- afd_result$svd^2
      prop_var  <- eigenvals / sum(eigenvals) * 100
      can_cor   <- sqrt(eigenvals / (1 + eigenvals))
      eta2_vals <- eigenvals / (1 + eigenvals)
      
      df_var <- data.frame(
        Fonction             = paste0("LD", seq_along(eigenvals)),
        Valeur_propre        = round(eigenvals, 4),
        Variance_pct         = round(prop_var, 4),
        Variance_cumulee_pct = round(cumsum(prop_var), 4),
        Correlation_canonique = round(can_cor, 4),
        Eta2                 = round(eta2_vals, 4),
        Appreciation_eta2    = sapply(eta2_vals, function(e) {
          if (e >= 0.64) "Excellent" else if (e >= 0.25) "Fort" else if (e >= 0.09) "Modéré" else "Faible"
        })
      )
      
      confusion_matrix <- table(Reel   = afd_data[[input$afdFactor]],
                                Predit = afd_predict$class)
      accuracy  <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      n_total   <- sum(confusion_matrix)
      expected  <- sum(rowSums(confusion_matrix) * colSums(confusion_matrix)) / (n_total^2)
      kappa_val <- (accuracy - expected) / (1 - expected)
      
      kappa_interp <- if (kappa_val >= 0.8) "Quasi-parfait"
      else if (kappa_val >= 0.6) "Substantiel"
      else if (kappa_val >= 0.4) "Modéré"
      else if (kappa_val >= 0.2) "Passable"
      else "Faible"
      
      df_classif <- data.frame(
        Metrique       = c("Accuracy globale (%)", "Kappa de Cohen", "Appreciation Kappa"),
        Valeur         = c(round(accuracy * 100, 2), round(kappa_val, 3), kappa_interp)
      )
      
      df_group_acc <- data.frame(
        Groupe = rownames(confusion_matrix),
        Taux_classif_pct = sapply(1:nrow(confusion_matrix), function(i) {
          round(confusion_matrix[i, i] / sum(confusion_matrix[i, ]) * 100, 2)
        })
      )
      
      confusion_df <- as.data.frame.matrix(confusion_matrix)
      confusion_df <- cbind(Groupe_reel = rownames(confusion_df), confusion_df)
      rownames(confusion_df) <- NULL
      
      list(
        variance_eta2        = df_var,
        classification_globale = df_classif,
        classification_groupe  = df_group_acc,
        matrice_confusion      = confusion_df
      )
    }, error = function(e) NULL)
  }
  
  # Helper: construire le dataframe des métriques HCPC
  build_hcpc_metrics_df <- function(vd) {
    tryCatch({
      coords   <- vd$coords
      clusters <- vd$clusters
      k        <- vd$k
      n        <- nrow(coords)
      res.hcpc <- vd$hcpc
      res.pca  <- vd$pca
      
      grand_mean <- colMeans(coords)
      SS_B <- SS_W <- 0
      for (cl in unique(clusters)) {
        idx <- which(clusters == cl)
        nc  <- length(idx)
        cm  <- colMeans(coords[idx, , drop = FALSE])
        SS_B <- SS_B + nc * sum((cm - grand_mean)^2)
        for (i in idx) SS_W <- SS_W + sum((coords[i, ] - cm)^2)
      }
      CH <- if (SS_W > 0) round((SS_B / (k - 1)) / (SS_W / (n - k)), 2) else NA
      
      cluster_list <- lapply(unique(sort(clusters)), function(cl) coords[clusters == cl, , drop = FALSE])
      centroids    <- lapply(cluster_list, colMeans)
      dispersions  <- sapply(seq_along(cluster_list), function(i) {
        mean(sqrt(rowSums(sweep(cluster_list[[i]], 2, centroids[[i]])^2)))
      })
      DB_vals <- sapply(seq_along(centroids), function(i) {
        max(sapply(seq_along(centroids), function(j) {
          if (i == j) return(0)
          d_ij <- sqrt(sum((centroids[[i]] - centroids[[j]])^2))
          if (d_ij == 0) return(0)
          (dispersions[i] + dispersions[j]) / d_ij
        }))
      })
      DB <- round(mean(DB_vals), 3)
      
      # Gros volumes : silhouette vectorisee (cluster::silhouette), calculee
      # sur un echantillon au-dela de HSTAT_DIST_MAX_N (l'exact est en O(n^2)).
      sil_mean <- round(hstat_silhouette_mean(coords, clusters), 3)
      
      tree      <- res.hcpc$call$t$tree
      coph_corr <- round(hstat_cophenetic_corr(res.pca$ind$coord, tree), 3)
      
      df_indices <- data.frame(
        Indice         = c("Calinski-Harabasz (CH)", "Davies-Bouldin (DB)", "Silhouette moyenne", "Corrélation cophenétique"),
        Valeur         = c(CH, DB, sil_mean, coph_corr),
        Objectif       = c("Maximiser", "Minimiser", "Maximiser (entre -1 et 1)", "Maximiser (entre 0 et 1)"),
        Seuils         = c("Plus c'est élevé, mieux c'est",
                           "Excellent < 0,5 | Acceptable 0,5-1,0 | Faible > 1,0",
                           ">= 0,70 excellent | 0,50-0,70 raisonnable | 0,25-0,50 faible | < 0,25 pas de structure",
                           "> 0,80 très bonne | 0,75-0,80 acceptable | < 0,75 médiocre"),
        Appreciation   = c(
          paste0("CH = ", CH, " (à comparer sur différents k)"),
          if (DB < 0.5) "Excellent" else if (DB < 1.0) "Acceptable" else "Faible",
          if (sil_mean >= 0.7) "Excellent" else if (sil_mean >= 0.5) "Raisonnable" else if (sil_mean >= 0.25) "Faible" else "Pas de structure",
          if (coph_corr >= 0.80) "Très bonne représentation" else if (coph_corr >= 0.75) "Acceptable" else "Médiocre"
        )
      )
      
      df_clusters <- data.frame(
        Individual = rownames(res.hcpc$data.clust),
        Cluster    = as.character(res.hcpc$data.clust$clust)
      )
      
      list(
        indices_validation = df_indices,
        affectation_clusters = df_clusters
      )
    }, error = function(e) NULL)
  }
  
  output$downloadPcaPlot <- shiny::downloadHandler(
    filename = function() paste0("acp_", Sys.Date(), ".", hstat_img_fmt(input$pcaPlot_format)),
    content = function(file) {
      d <- mv_dims_export("pcaPlot", 9.8, 7.9); dpi <- d$dpi
      auto_dims <- d
      p <- withCallingHandlers(
        suppressMessages(mv_legacy(createPcaPlot(pcaResultReactive()), "pcaPlot")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      hstat_ecrire_image(file, p, input$pcaPlot_format,
                         auto_dims$width, auto_dims$height, dpi)
    }
  )
  
  hstat_export_tables_handlers(output, "downloadPcaData", function() {
    dfs <- values$pcaDataframes %||% pcaDataframes()
    if (is.null(dfs)) return(NULL)
    list("Valeurs_propres"         = dfs$eigenvalues,
         "Coordonnees_individus"   = dfs$ind_coords,
         "Contributions_individus" = dfs$ind_contrib,
         "Cos2_individus"          = dfs$ind_cos2,
         "Coordonnees_variables"   = dfs$var_coords,
         "Contributions_variables" = dfs$var_contrib,
         "Cos2_variables"          = dfs$var_cos2,
         "Correlations_variables"  = dfs$var_cor)
  }, "acp_resultats", "ACP")
  
  
  # Selecteurs HCPC : source des labels des individus (carte + dendrogramme) et
  # groupe pour la classification sur MOYENNES.
  output$hcpcLabelSourceSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    shiny::selectInput("hcpcLabelSource",
                shiny::tagList(shiny::icon("tag"), " Source des labels des individus (carte & dendrogramme) :"),
                choices = c("Rownames" = "rownames", names(values$filteredData)),
                selected = "rownames")
  })
  output$hcpcMeansGroupSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    cat_cols <- names(values$filteredData)[vapply(values$filteredData,
                  function(x) is.character(x) || is.factor(x), logical(1))]
    shiny::selectInput("hcpcMeansGroup", "Variable de groupement (moyennes) :",
                choices = c("", cat_cols), selected = "")
  })

  hcpcResultReactive <- shiny::reactive({
    shiny::req(mv_launch$hcpc)
    nbclust <- if (!is.null(input$hcpcClusters) && !is.na(input$hcpcClusters) &&
                   input$hcpcClusters >= 2) input$hcpcClusters else -1
    use_means <- isTRUE(input$hcpcUseMeans) &&
      !is.null(input$hcpcMeansGroup) && nzchar(input$hcpcMeansGroup)
    tryCatch({
      hstat_set_seed(input$globalSeed)
      if (use_means) {
        # Classification sur les MOYENNES par groupe : une ACP est recalculee sur
        # le tableau des moyennes (variables actives de l'ACP), puis le HCPC est
        # applique. Les "individus" deviennent les groupes -> labels = groupes.
        shiny::req(values$filteredData, input$pcaVars)
        num_vars <- input$pcaVars[vapply(values$filteredData[, input$pcaVars, drop = FALSE],
                                         is.numeric, logical(1))]
        if (length(num_vars) < 2) {
          shiny::showNotification("HCPC sur moyennes : au moins 2 variables numériques actives (ACP) sont requises.",
                           type = "error", duration = 6); return(NULL)
        }
        means_data <- calculate_group_means(values$filteredData, num_vars, input$hcpcMeansGroup)
        if (nrow(means_data) < 3) {
          shiny::showNotification("HCPC sur moyennes : au moins 3 groupes sont nécessaires pour classer.",
                           type = "error", duration = 6); return(NULL)
        }
        res.pca <- FactoMineR::PCA(means_data, graph = FALSE,
                       ncp = min(5, ncol(means_data)))
        shiny::showNotification(paste0("HCPC sur moyennes : ", nrow(means_data), " groupes (",
                                input$hcpcMeansGroup, ")."),
                         type = "message", duration = 4, id = "hcpc_means_notif")
      } else {
        res.pca <- pcaResultReactive()
        shiny::req(res.pca)
      }
      # nb.clust = -1 -> HCPC choisit automatiquement le nombre de classes
      # Gros volumes : HCPC fait une CAH (O(n^2)) sur tous les individus ;
      # au-dela du seuil, pre-partition k-means (argument kk) prevue par
      # FactoMineR pour les grands jeux de donnees.
      n_ind <- nrow(res.pca$ind$coord)
      res.hcpc <- if (n_ind > HSTAT_DIST_MAX_N) {
        hstat_bigdata_note("Classification (HCPC, pre-partition k-means)",
                           min(1000L, n_ind), n_ind)
        FactoMineR::HCPC(res.pca, nb.clust = nbclust, graph = FALSE,
             kk = min(1000L, n_ind))
      } else {
        FactoMineR::HCPC(res.pca, nb.clust = nbclust, graph = FALSE)
      }
      return(res.hcpc)
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur HCPC"), type = "error", duration = 8)
      return(NULL)
    })
  })
  
  shiny::observe({
    res <- hcpcResultReactive()
    if (!is.null(res)) {
      values$hcpcResult <- res
    }
  })
  
  hcpcDataframes <- shiny::reactive({
    shiny::req(hcpcResultReactive())
    res.hcpc <- hcpcResultReactive()
    
    tryCatch({
      # Fonction helper pour valider un element de resultat HCPC
      is_valid_hcpc_element <- function(elem) {
        if (is.null(elem)) return(FALSE)
        
        # Verifier si c'est un dataframe ou peut etre converti
        if (!is.data.frame(elem)) {
          if (is.matrix(elem)) {
            elem <- as.data.frame(elem)
          } else {
            return(FALSE)
          }
        }
        
        if (nrow(elem) == 0) return(FALSE)
        
        return(TRUE)
      }
      
      cluster_assign_df <- data.frame(
        Individual = rownames(res.hcpc$data.clust),
        Cluster = as.character(res.hcpc$data.clust$clust),
        stringsAsFactors = FALSE
      )
      
      desc_var_df <- NULL
      if (!is.null(res.hcpc$desc.var) && !is.null(res.hcpc$desc.var$quanti)) {
        desc_var_list <- list()
        for (i in seq_along(res.hcpc$desc.var$quanti)) {
          elem <- res.hcpc$desc.var$quanti[[i]]
          
          if (is_valid_hcpc_element(elem)) {
            df <- as.data.frame(elem)
            df <- cbind(Cluster = i, Variable = rownames(df), df, stringsAsFactors = FALSE)
            rownames(df) <- NULL
            desc_var_list[[length(desc_var_list) + 1]] <- df
          }
        }
        if (length(desc_var_list) > 0) {
          desc_var_df <- do.call(rbind, desc_var_list)
        }
      }
      
      desc_axes_df <- NULL
      if (!is.null(res.hcpc$desc.axes) && !is.null(res.hcpc$desc.axes$quanti)) {
        desc_axes_list <- list()
        for (i in seq_along(res.hcpc$desc.axes$quanti)) {
          elem <- res.hcpc$desc.axes$quanti[[i]]
          
          if (is_valid_hcpc_element(elem)) {
            df <- as.data.frame(elem)
            df <- cbind(Cluster = i, Axe = rownames(df), df, stringsAsFactors = FALSE)
            rownames(df) <- NULL
            desc_axes_list[[length(desc_axes_list) + 1]] <- df
          }
        }
        if (length(desc_axes_list) > 0) {
          desc_axes_df <- do.call(rbind, desc_axes_list)
        }
      }
      
      parangons_df <- NULL
      if (!is.null(res.hcpc$desc.ind) && !is.null(res.hcpc$desc.ind$para)) {
        parangons_list <- list()
        for (i in seq_along(res.hcpc$desc.ind$para)) {
          elem <- res.hcpc$desc.ind$para[[i]]
          
          if (is_valid_hcpc_element(elem)) {
            df <- as.data.frame(elem)
            df <- cbind(Cluster = i, Individual = rownames(df), df, stringsAsFactors = FALSE)
            rownames(df) <- NULL
            parangons_list[[length(parangons_list) + 1]] <- df
          }
        }
        if (length(parangons_list) > 0) {
          parangons_df <- do.call(rbind, parangons_list)
        }
      }
      
      dist_df <- NULL
      if (!is.null(res.hcpc$desc.ind) && !is.null(res.hcpc$desc.ind$dist)) {
        dist_list <- list()
        for (i in seq_along(res.hcpc$desc.ind$dist)) {
          elem <- res.hcpc$desc.ind$dist[[i]]
          
          if (is_valid_hcpc_element(elem)) {
            df <- as.data.frame(elem)
            df <- cbind(Cluster = i, Individual = rownames(df), df, stringsAsFactors = FALSE)
            rownames(df) <- NULL
            dist_list[[length(dist_list) + 1]] <- df
          }
        }
        if (length(dist_list) > 0) {
          dist_df <- do.call(rbind, dist_list)
        }
      }
      
      result <- list(
        cluster_assignment = cluster_assign_df,
        desc_variables = desc_var_df,
        desc_axes = desc_axes_df,
        parangons = parangons_df,
        distant_individuals = dist_df
      )
      
      return(result)
      
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur creation dataframes HCPC"), type = "error", duration = 10)
      return(NULL)
    })
  })
  
  # Stocker les dataframes de HCPC dans values$ pour un acces fiable
  shiny::observe({
    shiny::req(hcpcResultReactive())
    
    # Attendre un peu pour laisser le temps au reactive de s'exécuter complètement
    shiny::isolate({
      tryCatch({
        dfs <- hcpcDataframes()
        
        if (!is.null(dfs)) {
          if (!is.null(dfs$cluster_assignment) && nrow(dfs$cluster_assignment) > 0) {
            values$hcpcDataframes <- dfs
            
            n_clusters <- length(unique(dfs$cluster_assignment$Cluster))
            n_individus <- nrow(dfs$cluster_assignment)
            
            msg <- paste0("HCPC: ", n_individus, " individus classés en ", n_clusters, " clusters")
            shiny::showNotification(msg, type = "message", duration = 3)
          } else {
            shiny::showNotification("Avertissement : dataframes HCPC créés mais vides", type = "warning", duration = 5)
          }
        } else {
          shiny::showNotification("Erreur : impossible de créer les dataframes HCPC", type = "error", duration = 5)
        }
      }, error = function(e) {
        shiny::showNotification(hstat_err_fr(e, "Erreur stockage HCPC"), type = "error", duration = 5)
      })
    })
  })
  
  # Reetiquette les individus d'un objet HCPC a partir d'une colonne source
  # choisie par l'utilisateur (menu "Source des labels"). Sert a la fois au
  # dendrogramme et a la carte des clusters : les deux representations utilisent
  # ainsi la meme source de labels, choisie de facon independante de l'ACP.
  # "rownames" (defaut) conserve les identifiants d'origine.
  .hcpc_apply_label_source <- function(res.hcpc, source_col) {
    if (is.null(source_col) || identical(source_col, "rownames")) return(res.hcpc)
    fdata <- values$filteredData
    if (is.null(fdata) || !source_col %in% names(fdata)) return(res.hcpc)
    old_names <- rownames(res.hcpc$data.clust)
    if (is.null(old_names)) return(res.hcpc)
    # Correspondance par nom de ligne (robuste au reordonnancement HCPC) ; a
    # defaut, correspondance positionnelle si les tailles coincident.
    new_lab <- if (!is.null(rownames(fdata)) && all(old_names %in% rownames(fdata))) {
      as.character(fdata[old_names, source_col])
    } else if (nrow(fdata) == length(old_names)) {
      as.character(fdata[[source_col]])
    } else {
      return(res.hcpc)
    }
    new_lab <- make.unique(ifelse(is.na(new_lab) | new_lab == "", old_names, new_lab))
    map <- stats::setNames(new_lab, old_names)
    # 1) data.clust (utilise par fviz_cluster pour les etiquettes de points)
    rownames(res.hcpc$data.clust) <- new_lab
    # 2) arbre hclust (utilise par fviz_dend pour les feuilles)
    if (!is.null(res.hcpc$call$t$tree$labels)) {
      lb <- res.hcpc$call$t$tree$labels
      res.hcpc$call$t$tree$labels <- ifelse(lb %in% names(map), map[lb], lb)
    }
    # 3) coordonnees des individus (au cas ou une couche s'y refere)
    if (!is.null(res.hcpc$call$X) && !is.null(rownames(res.hcpc$call$X))) {
      rn <- rownames(res.hcpc$call$X)
      rownames(res.hcpc$call$X) <- ifelse(rn %in% names(map), map[rn], rn)
    }
    res.hcpc
  }

  createHcpcDendPlot <- function(res.hcpc) {
    res.hcpc <- .hcpc_apply_label_source(res.hcpc, input$hcpcLabelSource)
    
    dend_title <- if (!is.null(input$hcpcDendTitle) && input$hcpcDendTitle != "") {
      input$hcpcDendTitle
    } else {
      "Dendrogramme HCPC"
    }
    
    # Générer les mêmes couleurs que pour la carte des clusters
    n_clusters <- length(unique(res.hcpc$data.clust$clust))
    cluster_colors <- generate_distinct_colors(n_clusters)
    
    # Taille des labels des feuilles : reglee en POINTS par l'utilisateur
    # (12 a 24 pt). fviz_dend attend un facteur cex, ou cex = 1 correspond a la
    # police de reference de 12 pt ; on impose ensuite la taille exacte sur les
    # calques de texte du ggplot produit.
    n_individus <- nrow(res.hcpc$data.clust)
    dend_lbl_pt <- hstat_lbl_pt(input$hcpcLabelSize)
    cex_labels  <- hstat_lbl_pt2cex(input$hcpcLabelSize)
    # Largeur des branches : valeur du curseur PONDEREE par la densite du
    # dendrogramme. Avec des centaines d'individus, les branches terminales sont
    # separees de quelques pixels seulement : une largeur fixe les fait fusionner
    # en aplats (branches "trop epaisses"). Le facteur reduit automatiquement la
    # largeur quand les feuilles se densifient, tout en laissant le curseur
    # piloter l'epaisseur globale.
    dens_scale <- if (n_individus <= 100) 1
                  else if (n_individus <= 250) 0.6
                  else if (n_individus <= 500) 0.4
                  else 0.3
    branch_lwd <- (if (!is.null(input$hcpcBranchWidth)) as.numeric(input$hcpcBranchWidth) else 0.5) * dens_scale
    # Affichage des labels : le choix explicite de l'utilisateur prime ; sinon,
    # ils sont montres automatiquement jusqu'a 60 individus.
    show_labels <- if (!is.null(input$hcpcShowLabels)) isTRUE(input$hcpcShowLabels)
                   else n_individus <= 60
    # Espace reserve aux labels sous l'arbre : proportionnel a leur taille quand
    # ils sont affiches, quasi nul sinon (evite la grande zone vide sous l'arbre).
    track_h <- if (show_labels) min(1.6, 0.8 * max(1, cex_labels / 0.7)) else 0.05

    p_dend <- factoextra::fviz_dend(res.hcpc,
                        cex = cex_labels,
                        lwd = branch_lwd,
                        palette = cluster_colors,
                        rect = TRUE,
                        rect_fill = TRUE,
                        rect_border = "jco",
                        main = dend_title,
                        sub = "",
                        show_labels = show_labels,
                        labels_track_height = track_h,
                        ggtheme = mv_ggtheme("hcpcDend"))

    # Supprime la legende parasite "lwd" (factoextra mappe parfois lwd sur une
    # echelle) ainsi que tout calque de texte dont l'etiquette vaut "lwd".
    p_dend <- p_dend + ggplot2::guides(linewidth = "none", size = "none", alpha = "none")
    if (!is.null(p_dend$layers) && length(p_dend$layers)) {
      keep <- vapply(p_dend$layers, function(ly) {
        lbl <- tryCatch(as.character(ly$data$label), error = function(e) character(0))
        txt <- tryCatch({
          m <- ly$mapping
          if (!is.null(m$label)) as.character(rlang::quo_get_expr(m$label)) else character(0)
        }, error = function(e) character(0))
        !any(c(lbl, txt) %in% "lwd")
      }, logical(1))
      p_dend$layers <- p_dend$layers[keep]
    }
    # Taille exacte des etiquettes de feuilles, en points : fviz_dend ne fait
    # que moduler cex, on fixe donc la taille finale sur les calques de texte.
    if (show_labels)
      p_dend <- hstat_apply_label_sizes(p_dend, hstat_lbl_pt2gg(dend_lbl_pt))

    p_dend <- p_dend + 
      labs(caption = paste("Nombre de clusters :", n_clusters)) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),   # l'axe X (index des feuilles) n'a pas de sens
        axis.ticks.x = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(10, 10, 50, 10),  # Marges : top, right, bottom, left
        axis.title.x = ggplot2::element_blank()
      )
    
    # Note explicative UNIQUEMENT quand les labels sont reellement masques
    # (auparavant, la note s'affichait meme quand des labels minuscules etaient
    # dessines, ce qui etait contradictoire).
    if (!show_labels) {
      p_dend <- p_dend + 
        labs(caption = paste0("Nombre de clusters : ", n_clusters,
                              "  --  Labels masqués (", n_individus, " individus) : ",
                              "cochez « Afficher les labels » ou consultez les résultats détaillés."))
    }
    
    # Echantillonnage automatique des labels quand ils sont trop denses pour etre
    # lisibles : au-dela de ~150 feuilles, chaque etiquette dispose de quelques
    # pixels seulement et les textes se superposent quelle que soit leur taille.
    # On n'affiche alors qu'un label sur k (k calcule pour viser ~120 labels
    # visibles), regulierement espaces, et la note l'indique explicitement.
    if (show_labels && n_individus > 150) {
      k <- ceiling(n_individus / 120)
      for (i in seq_along(p_dend$layers)) {
        ly <- p_dend$layers[[i]]
        d <- tryCatch(ly$data, error = function(e) NULL)
        if (is.data.frame(d) && all(c("label", "x") %in% names(d)) &&
            nrow(d) >= n_individus * 0.9) {
          ord <- order(d$x)
          sel <- sort(ord[seq(1, length(ord), by = k)])
          p_dend$layers[[i]]$data <- d[sel, , drop = FALSE]
        }
      }
      p_dend <- p_dend +
        labs(caption = paste0("Nombre de clusters : ", n_clusters,
                              "  --  ", n_individus, " individus : 1 label sur ", k,
                              " affiché pour la lisibilité ",
                              "(affectation complète dans les résultats détaillés)."))
    }
    
    return(p_dend)
  }
  
  output$hcpcAxisXSelect <- shiny::renderUI({
    shiny::req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims <- ncol(hstat_coord_mat(res.pca$ind$coord))
    
    shiny::selectInput("hcpcAxisX", "Axe X:",
                choices = stats::setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = 1)
  })
  
  output$hcpcAxisYSelect <- shiny::renderUI({
    shiny::req(pcaResultReactive())
    res.pca <- pcaResultReactive()
    n_dims <- ncol(hstat_coord_mat(res.pca$ind$coord))
    
    shiny::selectInput("hcpcAxisY", "Axe Y:",
                choices = stats::setNames(1:n_dims, paste0("PC", 1:n_dims)),
                selected = min(2, n_dims))
  })
  
  createHcpcClusterPlot <- function(res.hcpc, res.pca) {

    # Garde-fou : sans ACP/HCPC valides, get_eigenvalue() leve une erreur fatale
    # ("NULL can't be handled by get_eigenvalue()"). On affiche un message clair.
    if (is.null(res.pca) || is.null(res.hcpc) ||
        is.null(tryCatch(res.pca$eig, error = function(e) NULL))) {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0,
                   label = "Lancez d'abord l'ACP puis la HCPC pour afficher la carte des clusters.",
                   size = 5, colour = "#7f8c8d") +
          ggplot2::theme_void()
      )
    }

    axis_x <- if (!is.null(input$hcpcAxisX)) as.numeric(input$hcpcAxisX) else 1
    axis_y <- if (!is.null(input$hcpcAxisY)) as.numeric(input$hcpcAxisY) else 2
    
    cluster_title <- if (!is.null(input$hcpcClusterTitle) && input$hcpcClusterTitle != "") {
      input$hcpcClusterTitle
    } else {
      "Carte des clusters HCPC"
    }
    
    eigenvals <- factoextra::get_eigenvalue(res.pca)
    pc_x_var <- round(eigenvals[axis_x, "variance.percent"], 1)
    pc_y_var <- round(eigenvals[axis_y, "variance.percent"], 1)
    
    x_label <- if (!is.null(input$hcpcClusterXLabel) && input$hcpcClusterXLabel != "") {
      input$hcpcClusterXLabel
    } else {
      paste0("PC", axis_x, " (", pc_x_var, "%)")
    }
    
    y_label <- if (!is.null(input$hcpcClusterYLabel) && input$hcpcClusterYLabel != "") {
      input$hcpcClusterYLabel
    } else {
      paste0("PC", axis_y, " (", pc_y_var, "%)")
    }
    
    n_clusters <- length(unique(res.hcpc$data.clust$clust))
    cluster_colors <- generate_distinct_colors(n_clusters)

    # Source des labels choisie par l'utilisateur (partagee avec le dendrogramme).
    res.hcpc <- .hcpc_apply_label_source(res.hcpc, input$hcpcLabelSource)

    # Afficher des polygones convexes autour de chaque cluster. Les etiquettes
    # individuelles sont masquees par defaut (illisibles au-dela de ~30 individus)
    # et activables via hcpcShowLabels.
    show_lab <- isTRUE(input$hcpcClusterShowLabels)
    hcpc_pt   <- if (!is.null(input$hcpcPointSize)) input$hcpcPointSize else HSTAT_GG_POINT_SIZE
    hcpc_txt  <- if (!is.null(input$hcpcAxisTextSize)) input$hcpcAxisTextSize else HSTAT_GG_BASE_SIZE
    p_cluster <- factoextra::fviz_cluster(res.hcpc,
                              axes = c(axis_x, axis_y),
                              geom = if (show_lab) c("point", "text") else "point",
                              repel = show_lab,
                              show.clust.cent = TRUE,
                              ellipse = TRUE,
                              ellipse.type = "convex",
                              ellipse.alpha = 0.20,
                              pointsize = hcpc_pt,
                              labelsize = hstat_lbl_pt2gg(input$hcpcClusterLabelSize),
                              palette = cluster_colors,
                              ggtheme = mv_ggtheme("hcpcCluster"),
                              main = cluster_title) +
      labs(x = x_label, y = y_label) +
      ggplot2::theme(legend.position = "right",
            legend.title = ggtext::element_markdown(size = hcpc_txt - 1, face = "bold"),
            legend.text = ggplot2::element_text(size = hcpc_txt - 2),
            axis.title = ggplot2::element_text(size = hcpc_txt),
            axis.text = ggplot2::element_text(size = hcpc_txt - 2),
            plot.title = ggplot2::element_text(size = hcpc_txt + 2, face = "bold"))

    # Taille exacte des etiquettes d'individus, en points.
    if (show_lab)
      p_cluster <- hstat_apply_label_sizes(
        p_cluster, hstat_lbl_pt2gg(input$hcpcClusterLabelSize))

    if (!is.null(input$hcpcCenterAxes) && input$hcpcCenterAxes) {
      coords <- res.pca$ind$coord[, c(axis_x, axis_y)]
      max_range <- max(abs(range(coords, na.rm = TRUE)))
      p_cluster <- p_cluster + ggplot2::xlim(-max_range, max_range) + ggplot2::ylim(-max_range, max_range)
    }
    
    return(p_cluster)
  }
  
  output$hcpcDendPlot <- shiny::renderPlot({
    shiny::req(values$pcaResult)
    suppressWarnings(suppressMessages(mv_legacy(createHcpcDendPlot(hcpcResultReactive()), "hcpcDend")))
  }, res = 120)
  
  output$hcpcClusterPlot <- shiny::renderPlot({
    shiny::req(values$pcaResult)
    suppressWarnings(suppressMessages(mv_legacy(createHcpcClusterPlot(hcpcResultReactive(), pcaResultReactive()), "hcpcCluster")))
  }, res = 120)
  
  output$hcpcSummary <- shiny::renderPrint({
    shiny::req(hcpcResultReactive())
    res.hcpc <- hcpcResultReactive()
    
    # Nombre de décimales (seulement si l'utilisateur a coché l'option)
    use_round <- !is.null(input$hcpcRoundResults) && input$hcpcRoundResults
    dec <- if (use_round && !is.null(input$hcpcDecimals)) input$hcpcDecimals else 4
    
    cat("=== CLASSIFICATION HIÉRARCHIQUE SUR COMPOSANTES PRINCIPALES (HCPC) ===\n\n")
    cat("Nombre de clusters:", length(unique(res.hcpc$data.clust$clust)), "\n\n")
    
    cluster_results <- data.frame(
      Individual = rownames(res.hcpc$data.clust),
      Cluster = res.hcpc$data.clust$clust
    )
    cat("=== AFFECTATION DES INDIVIDUS AUX CLUSTERS ===\n")
    print(cluster_results)
    cat("\n")
    
    cat("=== VARIABLES LES PLUS DISCRIMINANTES PAR CLUSTER ===\n")
    if (!is.null(res.hcpc$desc.var$quanti)) {
      for (i in seq_along(res.hcpc$desc.var$quanti)) {
        if (!is.null(res.hcpc$desc.var$quanti[[i]])) {
          cat("\n--- CLUSTER", i, "---\n")
          print(round(res.hcpc$desc.var$quanti[[i]], dec))
        }
      }
    }
    
    if (!is.null(res.hcpc$desc.axes$quanti)) {
      cat("\n=== DESCRIPTION DES AXES PAR CLUSTER ===\n")
      for (i in seq_along(res.hcpc$desc.axes$quanti)) {
        if (!is.null(res.hcpc$desc.axes$quanti[[i]])) {
          cat("\n--- CLUSTER", i, " - AXES ---\n")
          print(round(res.hcpc$desc.axes$quanti[[i]], dec))
        }
      }
    }
    
    if (!is.null(res.hcpc$desc.ind$para)) {
      cat("\n=== INDIVIDUS LES PLUS REPRESENTATIFS (PARANGONS) ===\n")
      for (i in seq_along(res.hcpc$desc.ind$para)) {
        if (!is.null(res.hcpc$desc.ind$para[[i]])) {
          cat("\n--- CLUSTER", i, " - PARANGONS ---\n")
          print(res.hcpc$desc.ind$para[[i]])
        }
      }
    }
    
    if (!is.null(res.hcpc$desc.ind$dist)) {
      cat("\n=== INDIVIDUS LES PLUS ELOIGNES DU CENTRE ===\n")
      for (i in seq_along(res.hcpc$desc.ind$dist)) {
        if (!is.null(res.hcpc$desc.ind$dist[[i]])) {
          cat("\n--- CLUSTER", i, " - INDIVIDUS ELOIGNES ---\n")
          print(res.hcpc$desc.ind$dist[[i]])
        }
      }
    }
  })
  
  
  # Helper: extraire les données numériques des clusters HCPC
  hcpcValidationData <- shiny::reactive({
    shiny::req(hcpcResultReactive(), pcaResultReactive())
    res.hcpc <- hcpcResultReactive()
    res.pca  <- pcaResultReactive()
    tryCatch({
      coords    <- res.pca$ind$coord
      clusters  <- as.integer(res.hcpc$data.clust$clust)
      k         <- length(unique(clusters))
      list(coords = coords, clusters = clusters, k = k,
           hcpc = res.hcpc, pca = res.pca)
    }, error = function(e) NULL)
  })
  
  # -- 1. Graphique des hauteurs de fusion (indices d'agrégation) --
  createHcpcHeightsPlot <- function(res.hcpc) {
    tree        <- res.hcpc$call$t$tree
    heights     <- rev(tree$height)
    n_show      <- min(length(heights), 20)
    heights_sub <- heights[1:n_show]
    
    df_h <- data.frame(
      Etape   = 1:n_show,
      Hauteur = heights_sub,
      Saut    = c(NA, diff(heights_sub))
    )
    df_h_valid  <- df_h[!is.na(df_h$Saut), ]
    best_step   <- df_h_valid$Etape[which.max(df_h_valid$Saut)]
    n_clust_opt <- best_step
    
    ggplot2::ggplot(df_h, ggplot2::aes(x = Etape, y = Hauteur)) +
      ggplot2::geom_line(color = "#2E86AB", linewidth = 1.3) +
      ggplot2::geom_point(ggplot2::aes(color = Etape == best_step), size = 4) +
      ggplot2::scale_color_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#27ae60"),
                         labels = c("TRUE" = "Coupure suggérée", "FALSE" = "Autre fusion"),
                         name = NULL) +
      ggplot2::geom_vline(xintercept = best_step, linetype = "dashed", color = "#e74c3c", size = 0.9) +
      ggplot2::annotate("text", x = best_step + 0.2, y = max(heights_sub, na.rm = TRUE) * 0.95,
               label = paste0("Coupure suggérée\n(", n_clust_opt, " clusters)"),
               hjust = 0, color = "#e74c3c", size = 3.5, fontface = "bold") +
      ggplot2::scale_x_continuous(breaks = 1:n_show) +
      labs(
        title    = "Graphique des indices d'agrégation (hauteurs de fusion)",
        subtitle = "Un saut important sur la courbe indique le nombre optimal de clusters (règle du coude)",
        x        = "Nombre de clusters (ordre de fusion, de k le plus grand au plus petit)",
        y        = "Hauteur d'agrégation",
        caption  = "La coupure optimale (point rouge) correspond au plus grand saut entre deux fusions successives."
      ) +
      mv_ggtheme("hcpcHeights") +
      ggplot2::theme(
        plot.title       = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 13, color = "#2c3e50"),
        plot.subtitle    = ggplot2::element_text(hjust = 0.5, color = "#555", size = 10),
        legend.position  = "bottom",
        panel.grid.minor = ggplot2::element_blank()
      )
  }
  
  output$hcpcHeightsPlot <- shiny::renderPlot({
    shiny::req(hcpcResultReactive())
    tryCatch(
      mv_legacy(createHcpcHeightsPlot(hcpcResultReactive()), "hcpcHeights"),
      error = function(e) {
        graphics::plot.new()
        text(0.5, 0.5, paste(strwrap(hstat_err_fr(e), 55), collapse = "\n"), cex = 0.85, col = "#e74c3c")
      }
    )
  }, res = 120)
  
  output$downloadHcpcHeightsPlot <- shiny::downloadHandler(
    filename = function() paste0("hcpc_hauteurs_fusion_", Sys.Date(), ".", hstat_img_fmt(input$hcpcHeights_format)),
    content = function(file) {
      d <- mv_dims_export("hcpcHeights", 9.8, 7.1); dpi <- d$dpi
      auto <- d
      p    <- mv_legacy(createHcpcHeightsPlot(hcpcResultReactive()), "hcpcHeights")
      hstat_ecrire_image(file, p, input$hcpcHeights_format,
                         auto$width, auto$height, dpi)
    }
  )
  
  # -- 2. Métriques de validation des clusters (CH, DB, Silhouette, Cophénétique) --
  output$hcpcMetricsUI <- shiny::renderUI({
    vd <- hcpcValidationData()
    shiny::req(vd)
    tryCatch({
      coords   <- vd$coords
      clusters <- vd$clusters
      k        <- vd$k
      res.hcpc <- vd$hcpc
      res.pca  <- vd$pca
      
      # ---- Calinski-Harabasz ----
      n <- nrow(coords)
      grand_mean <- colMeans(coords)
      
      SS_B <- 0
      SS_W <- 0
      for (cl in unique(clusters)) {
        idx <- which(clusters == cl)
        nc  <- length(idx)
        cm  <- colMeans(coords[idx, , drop = FALSE])
        SS_B <- SS_B + nc * sum((cm - grand_mean)^2)
        for (i in idx) {
          SS_W <- SS_W + sum((coords[i, ] - cm)^2)
        }
      }
      CH <- if (SS_W > 0) (SS_B / (k - 1)) / (SS_W / (n - k)) else NA
      CH <- round(CH, 2)
      
      # ---- Davies-Bouldin ----
      cluster_list <- lapply(unique(sort(clusters)), function(cl) coords[clusters == cl, , drop = FALSE])
      centroids    <- lapply(cluster_list, colMeans)
      dispersions  <- sapply(seq_along(cluster_list), function(i) {
        mean(sqrt(rowSums(sweep(cluster_list[[i]], 2, centroids[[i]])^2)))
      })
      DB_vals <- sapply(seq_along(centroids), function(i) {
        max(sapply(seq_along(centroids), function(j) {
          if (i == j) return(0)
          d_ij <- sqrt(sum((centroids[[i]] - centroids[[j]])^2))
          if (d_ij == 0) return(0)
          (dispersions[i] + dispersions[j]) / d_ij
        }))
      })
      DB <- round(mean(DB_vals), 3)
      
      # ---- Silhouette ----
      # Gros volumes : silhouette vectorisee (cluster::silhouette), calculee
      # sur un echantillon au-dela de HSTAT_DIST_MAX_N (l'exact est en O(n^2)).
      sil_mean <- round(hstat_silhouette_mean(coords, clusters), 3)
      
      # ---- Corrélation cophénétique ----
      tree        <- res.hcpc$call$t$tree
      coph_corr   <- round(hstat_cophenetic_corr(res.pca$ind$coord, tree), 3)
      
      # ---- Interprétations ----
      ch_interp <- shiny::div(
        style = "font-size: 11px; margin-top: 5px;",
        p(style = "margin: 0; color: #555; font-style: italic;",
          shiny::icon("info-circle"), " Maximiser cet indice. Plus il est élevé, meilleure est la séparation inter-classes par rapport à la compacité intra-classe.")
      )
      
      db_color <- if (DB < 0.5) "#27ae60" else if (DB < 1.0) "#f39c12" else "#dc3545"
      db_interp <- if (DB < 0.5) "Excellent (< 0,5) -- clusters bien séparés et compacts"
      else if (DB < 1.0) "Acceptable (0,5 - 1,0) -- séparation modérée"
      else "Faible (> 1,0) -- clusters qui se chevauchent"
      
      sil_color <- if (sil_mean >= 0.7) "#27ae60" else if (sil_mean >= 0.5) "#3498db" 
      else if (sil_mean >= 0.25) "#f39c12" else "#dc3545"
      sil_interp <- if (sil_mean >= 0.7) "Excellente structure (>= 0,70)"
      else if (sil_mean >= 0.5) "Structure raisonnable (0,50 - 0,70)"
      else if (sil_mean >= 0.25) "Structure faible (0,25 - 0,50)"
      else "Pas de structure substantielle (< 0,25)"
      
      coph_color <- if (coph_corr >= 0.80) "#27ae60" else if (coph_corr >= 0.75) "#3498db" else "#dc3545"
      coph_interp <- if (coph_corr >= 0.80) "Très bonne représentation (>= 0,80) -- dendrogramme fidèle"
      else if (coph_corr >= 0.75) "Représentation acceptable (0,75 - 0,80)"
      else "Représentation médiocre (< 0,75) -- le dendrogramme déforme les distances"
      
      shiny::tagList(
        shiny::div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; margin-bottom:12px; border:1px solid #dee2e6;",
            shiny::h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #3498db; padding-bottom:6px;",
               shiny::icon("chart-bar"), " Indice de Calinski-Harabasz (CH)"),
            p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
              "Ratio variance inter-classes / variance intra-classes. Un indice élevé indique une bonne séparation des groupes. À comparer sur plusieurs valeurs de k pour choisir le nombre optimal de clusters."),
            shiny::div(style = "text-align:center; padding:8px;",
                shiny::h3(style = "color:#2c3e50; margin:0; font-weight:bold;", CH)
            ),
            ch_interp
        ),
        shiny::div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; margin-bottom:12px; border:1px solid #dee2e6;",
            shiny::h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #16a085; padding-bottom:6px;",
               shiny::icon("compress"), " Indice de Davies-Bouldin (DB)"),
            p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
              "Rapport moyen entre la dispersion intra-classe et la séparation inter-classes. À minimiser : un faible DB indique des clusters compacts et bien séparés."),
            shiny::div(style = "text-align:center; padding:8px;",
                shiny::h3(style = paste0("color:", db_color, "; margin:0; font-weight:bold;"), DB)
            ),
            shiny::div(style = paste0("margin-top:8px; padding:6px 10px; border-left:4px solid ", db_color, "; background:white; border-radius:0 4px 4px 0;"),
                p(style = paste0("margin:0; font-size:12px; color:", db_color, ";"), shiny::icon("check-circle"), " ", db_interp))
        ),
        shiny::div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; margin-bottom:12px; border:1px solid #dee2e6;",
            shiny::h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #27ae60; padding-bottom:6px;",
               shiny::icon("star"), " Silhouette moyenne"),
            p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
              "Mesure pour chaque individu son appartenance à son cluster par rapport au cluster voisin. Varie de -1 à 1. Une valeur positive élevée indique que l'individu est bien classé."),
            shiny::div(style = "text-align:center; padding:8px;",
                shiny::h3(style = paste0("color:", sil_color, "; margin:0; font-weight:bold;"), sil_mean)
            ),
            shiny::div(style = paste0("margin-top:8px; padding:6px 10px; border-left:4px solid ", sil_color, "; background:white; border-radius:0 4px 4px 0;"),
                p(style = paste0("margin:0; font-size:12px; color:", sil_color, ";"), shiny::icon("check-circle"), " ", sil_interp)),
            p(style = "font-size:11px; color:#888; margin-top:6px; margin-bottom:0;",
              "Seuils : >= 0,70 excellent | 0,50-0,70 raisonnable | 0,25-0,50 faible | < 0,25 pas de structure")
        ),
        shiny::div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; border:1px solid #dee2e6;",
            shiny::h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #e67e22; padding-bottom:6px;",
               shiny::icon("project-diagram"), " Corrélation cophénétique"),
            p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
              "Corrélation entre les distances originales entre individus et les distances de fusion dans le dendrogramme. Indique si le dendrogramme représente fidèlement la structure des données."),
            shiny::div(style = "text-align:center; padding:8px;",
                shiny::h3(style = paste0("color:", coph_color, "; margin:0; font-weight:bold;"), coph_corr)
            ),
            shiny::div(style = paste0("margin-top:8px; padding:6px 10px; border-left:4px solid ", coph_color, "; background:white; border-radius:0 4px 4px 0;"),
                p(style = paste0("margin:0; font-size:12px; color:", coph_color, ";"), shiny::icon("check-circle"), " ", coph_interp)),
            p(style = "font-size:11px; color:#888; margin-top:6px; margin-bottom:0;",
              "Seuils : > 0,80 très bonne | 0,75 - 0,80 acceptable | < 0,75 médiocre")
        )
      )
    }, error = function(e) {
      shiny::div(class = "callout callout-danger",
          shiny::tags$p(shiny::tagList(shiny::icon("exclamation-triangle"), " ", hstat_err_fr(e, "Calcul des métriques HCPC"))))
    })
  })
  
  # -- 3. Stabilité par sous-échantillonnage --
  output$hcpcStabilityUI <- shiny::renderUI({
    shiny::req(hcpcValidationData())
    vd <- hcpcValidationData()
    tryCatch({
      coords   <- vd$coords
      clusters <- vd$clusters
      k        <- vd$k
      n        <- nrow(coords)
      # Gros volumes : le bootstrap refait 30 CAH completes (dist en O(n^2)) ;
      # au-dela du seuil, la stabilite est evaluee sur un echantillon.
      if (n > HSTAT_DIST_MAX_N) {
        idx_cap  <- hstat_cap_indices(n, HSTAT_DIST_MAX_N)
        hstat_bigdata_note("Stabilite du clustering (bootstrap)", length(idx_cap), n)
        coords   <- coords[idx_cap, , drop = FALSE]
        clusters <- clusters[idx_cap]
        n        <- nrow(coords)
      }
      
      n_boot  <- 30
      prop    <- 0.8
      rand_indices <- numeric(n_boot)
      
      for (b in 1:n_boot) {
        hstat_set_seed(input$globalSeed)
        idx_b   <- sample(1:n, size = floor(n * prop), replace = FALSE)
        sub_coords <- coords[idx_b, , drop = FALSE]
        d_sub   <- dist(sub_coords)
        hc_sub  <- hclust(d_sub, method = "ward.D2")
        clust_sub <- cutree(hc_sub, k = k)
        clust_ref <- clusters[idx_b]
        
        # Vectorise : combn() genere n(n-1)/2 paires, impraticable des ~10 000
        rand_indices[b] <- hstat_pair_agreement(clust_sub, clust_ref)
      }
      
      mean_rand <- round(mean(rand_indices), 3)
      sd_rand   <- round(stats::sd(rand_indices), 3)
      
      color_stab <- if (mean_rand >= 0.9) "#27ae60" else if (mean_rand >= 0.8) "#3498db" 
      else if (mean_rand >= 0.7) "#f39c12" else "#dc3545"
      stab_interp <- if (mean_rand >= 0.9) "Excellente stabilité (>= 0,90) -- la partition est très reproductible."
      else if (mean_rand >= 0.8) "Bonne stabilité (0,80 - 0,90) -- la partition est fiable."
      else if (mean_rand >= 0.7) "Stabilité modérée (0,70 - 0,80) -- à interpréter avec prudence."
      else "Faible stabilité (< 0,70) -- la partition est sensible à la composition de l'échantillon."
      
      shiny::div(style = "background: linear-gradient(135deg,#f8f9fa,#e9ecef); border-radius:8px; padding:15px; border:1px solid #dee2e6;",
          shiny::h5(style = "color:#2c3e50; font-weight:bold; margin-top:0; border-bottom:2px solid #16a085; padding-bottom:6px;",
             shiny::icon("sync"), " Stabilité par sous-échantillonnage (Rand Index)"),
          p(style = "font-size:12px; color:#555; font-style:italic; margin-bottom:8px;",
            paste0("Évaluation de la robustesse de la partition sur ", n_boot, " sous-échantillons aléatoires (",
                   round(prop * 100), "% des données). L'indice de Rand mesure la concordance entre la partition de référence et celle obtenue sur chaque sous-échantillon (0 = aucun accord, 1 = accord parfait).")),
          shiny::fluidRow(
            shiny::column(6,
                   shiny::div(style = "text-align:center; background:white; border-radius:6px; padding:10px; border:1px solid #dee2e6;",
                       p(style = "margin:0; font-size:11px; color:#888; text-transform:uppercase;", "Rand Index moyen"),
                       shiny::h3(style = paste0("margin:4px 0; font-weight:bold; color:", color_stab, ";"), mean_rand)
                   )
            ),
            shiny::column(6,
                   shiny::div(style = "text-align:center; background:white; border-radius:6px; padding:10px; border:1px solid #dee2e6;",
                       p(style = "margin:0; font-size:11px; color:#888; text-transform:uppercase;", "Écart-type"),
                       shiny::h3(style = "margin:4px 0; font-weight:bold; color:#2c3e50;", sd_rand)
                   )
            )
          ),
          shiny::div(style = paste0("margin-top:10px; padding:8px 12px; border-left:4px solid ", color_stab, "; background:white; border-radius:0 4px 4px 0;"),
              p(style = paste0("margin:0; font-size:12px; color:", color_stab, ";"),
                shiny::icon("check-circle"), " ", stab_interp))
      )
    }, error = function(e) {
      shiny::div(style = "padding:10px; background:#f8d7da; border-radius:6px;",
          p(style = "margin:0; color:#721c24; font-size:12px;",
            shiny::icon("exclamation-triangle"), " Erreur stabilité : ", e$message))
    })
  })
  
  output$downloadHcpcDendPlot <- shiny::downloadHandler(
    filename = function() paste0("hcpc_dendrogramme_", Sys.Date(), ".", hstat_img_fmt(input$hcpcDend_format)),
    content = function(file) {
      d <- mv_dims_export("hcpcDend", 11.8, 7.9); dpi <- d$dpi
      auto_dims <- d
      p <- withCallingHandlers(
        suppressMessages(mv_legacy(createHcpcDendPlot(hcpcResultReactive()), "hcpcDend")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      hstat_ecrire_image(file, p, input$hcpcDend_format,
                         auto_dims$width, auto_dims$height, dpi)
    }
  )
  
  output$downloadHcpcClusterPlot <- shiny::downloadHandler(
    filename = function() paste0("hcpc_clusters_", Sys.Date(), ".", hstat_img_fmt(input$hcpcCluster_format)),
    content = function(file) {
      d <- mv_dims_export("hcpcCluster", 9.8, 7.9); dpi <- d$dpi
      auto_dims <- d
      p <- withCallingHandlers(
        suppressMessages(mv_legacy(createHcpcClusterPlot(hcpcResultReactive(), pcaResultReactive()), "hcpcCluster")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      hstat_ecrire_image(file, p, input$hcpcCluster_format,
                         auto_dims$width, auto_dims$height, dpi)
    }
  )
  
  hstat_export_tables_handlers(output, "downloadHcpcData", function() {
    dfs <- values$hcpcDataframes %||% hcpcDataframes()
    if (is.null(dfs) || is.null(dfs$cluster_assignment) ||
        nrow(dfs$cluster_assignment) == 0) return(NULL)
    # Une feuille vide n'apporte rien : les tableaux facultatifs ne sont
    # ajoutes que s'ils portent au moins une ligne.
    hstat_tables_non_vides(list(
      "Affectation_clusters" = dfs$cluster_assignment,
      "Desc_variables"       = dfs$desc_variables,
      "Desc_axes"            = dfs$desc_axes,
      "Parangons"            = dfs$parangons,
      "Individus_eloignes"   = dfs$distant_individuals))
  }, "hcpc_resultats", "HCPC")
  
  # SECTION 3: AFD (Analyse Factorielle Discriminante) 
  
  output$afdFactorSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    df <- values$filteredData
    
    # - Regrouper les colonnes par type avec étiquettes claires 
    build_group_choices <- function(df) {
      cols_factor  <- names(df)[sapply(df, is.factor)]
      cols_char    <- names(df)[sapply(df, is.character)]
      cols_date    <- names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
      # Numériques avec <= 30 valeurs uniques -> utilisables comme groupes
      cols_num_few <- names(df)[sapply(df, function(x) {
        is.numeric(x) && length(unique(stats::na.omit(x))) <= 30
      })]
      cols_logi    <- names(df)[sapply(df, is.logical)]
      
      groups <- list()
      if (length(cols_factor)  > 0) groups[["Facteur"]]                             <- cols_factor
      if (length(cols_char)    > 0) groups[["Texte / Caractère"]]                   <- cols_char
      if (length(cols_date)    > 0) groups[["Date"]]                                <- cols_date
      if (length(cols_num_few) > 0) groups[["Numérique (<= 30 niveaux))"]]           <- cols_num_few
      if (length(cols_logi)    > 0) groups[["Logique (TRUE/FALSE)"]]                <- cols_logi
      groups
    }
    
    grouped <- build_group_choices(df)
    
    if (length(grouped) == 0) {
      return(shiny::div(
        style = "padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;",
        p(style = "margin: 0; color: #721c24;",
          shiny::icon("exclamation-triangle"),
          " Aucune colonne utilisable comme variable discriminante.")
      ))
    }
    
    # Valeur par défaut : premier facteur/char, sinon premier disponible
    all_cols <- unlist(grouped, use.names = FALSE)
    default_col <- if (!is.null(grouped[["Facteur"]]))     grouped[["Facteur"]][1]   else
      if (!is.null(grouped[["Texte / Caractère"]])) grouped[["Texte / Caractère"]][1] else
        all_cols[1]
    
    shiny::tagList(
      shiny::selectInput(
        "afdFactor",
        label = shiny::div(shiny::icon("bullseye"), " Variable à discriminer :"),
        choices  = grouped,
        selected = default_col
      ),
      shiny::uiOutput("afdFactorTypeHint")
    )
  })
  
  output$afdFactorTypeHint <- shiny::renderUI({
    shiny::req(values$filteredData, input$afdFactor)
    col <- values$filteredData[[input$afdFactor]]
    n_lvl <- length(unique(stats::na.omit(col)))
    type_str <- if (is.factor(col))      paste0("Facteur -- ", n_lvl, " niveaux")
    else if (is.character(col)) paste0("Texte -- ", n_lvl, " valeurs uniques")
    else if (inherits(col,"Date") || inherits(col,"POSIXt")) paste0("Date -- ", n_lvl, " valeurs uniques")
    else if (is.numeric(col))  paste0("Numérique -- converti en facteur (", n_lvl, " niveaux)")
    else if (is.logical(col))  "Logique -- 2 niveaux (TRUE / FALSE)"
    else paste0("Type : ", class(col)[1], " -- ", n_lvl, " niveaux")
    
    col_bg <- if (is.factor(col)) "#28a745"
    else if (is.character(col)) "#17a2b8"
    else if (is.numeric(col)) "#ffc107"
    else if (inherits(col,"Date")||inherits(col,"POSIXt")) "#6f42c1"
    else "#6c757d"
    col_tc <- if (is.numeric(col)) "#212529" else "white"
    
    shiny::div(style = "margin-top: 4px;",
        shiny::tags$span(
          style = paste0("display:inline-block; padding:3px 8px; border-radius:4px; ",
                         "background:", col_bg, "; color:", col_tc, "; font-size:11px; font-weight:500;"),
          shiny::icon("info-circle"), " ", type_str
        ))
  })
  
  output$afdVarSelect <- shiny::renderUI({
    shiny::req(values$filteredData)
    num_cols <- names(values$filteredData)[sapply(values$filteredData, is.numeric)]
    if (length(num_cols) == 0) return(NULL)
    
    pickerInput(
      inputId = "afdVars",
      label = "Variables quantitatives :",
      choices = num_cols,
      multiple = TRUE,
      selected = num_cols,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$afdQualiSupSelect <- shiny::renderUI({
    shiny::req(values$filteredData, input$afdFactor)
    if (is.null(values$filteredData) || is.null(input$afdFactor)) {
      return(NULL)
    }
    
    fac_cols <- get_categorical_cols(values$filteredData)
    
    # Exclure le facteur discriminant et le groupe de moyennes
    fac_cols <- fac_cols[fac_cols != input$afdFactor]
    if (!is.null(input$afdMeansGroup) && !is.null(input$afdUseMeans) && input$afdUseMeans) {
      fac_cols <- fac_cols[fac_cols != input$afdMeansGroup]
    }
    
    if (length(fac_cols) == 0) {
      return(p(style = "margin: 10px 0; font-size: 11px; color: #6c757d; font-style: italic;",
               shiny::icon("info-circle"), 
               " Aucune variable catégorielle supplémentaire disponible."))
    }
    
    pickerInput(
      inputId = "afdQualiSup",
      label = "Variables catégorielles supplémentaires (optionnel) :",
      choices = fac_cols,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$afdCollinearityPanel <- shiny::renderUI({
    shiny::req(values$filteredData, input$afdVars)
    if (length(input$afdVars) < 2) return(NULL)
    
    afd_data <- tryCatch(
      { d <- values$filteredData[, input$afdVars, drop = FALSE]
      d[, sapply(d, is.numeric), drop = FALSE] },
      error = function(e) NULL
    )
    if (is.null(afd_data) || ncol(afd_data) < 2) return(NULL)
    
    R_mat <- safe_cor(afd_data)
    if (is.null(R_mat) || anyNA(R_mat)) return(NULL)
    
    # Paires avec |cor| >= 0.80
    pairs_high <- list()
    for (i in seq_len(nrow(R_mat))) {
      for (j in seq_len(ncol(R_mat))) {
        if (i < j && abs(R_mat[i,j]) >= 0.80)
          pairs_high[[length(pairs_high)+1]] <- list(
            v1 = rownames(R_mat)[i], v2 = colnames(R_mat)[j],
            cor = round(R_mat[i,j], 3))
      }
    }
    if (length(pairs_high) == 0) return(NULL)
    
    vif_vals <- tryCatch({
      if (ncol(afd_data) >= 3) {
        afd_nzv <- remove_zero_var_cols(afd_data)
        v <- sapply(seq_len(ncol(afd_nzv)), function(i) {
          y <- afd_nzv[[i]]; x <- remove_zero_var_cols(afd_nzv[,-i, drop=FALSE])
          if (ncol(x) == 0) return(Inf)
          r2 <- tryCatch(suppressWarnings(summary(stats::lm(y~., data=x))$r.squared), error=function(e) NA)
          if (is.na(r2)||r2>=1) Inf else 1/(1-r2)
        })
        stats::setNames(v, names(afd_nzv))
      } else rep(NA, ncol(afd_data))
    }, error=function(e) rep(NA, ncol(afd_data)))
    
    high_vif    <- names(vif_vals[!is.na(vif_vals) & is.finite(vif_vals) & vif_vals > 5])
    perfect_col <- unique(unlist(lapply(pairs_high, function(p) if (abs(p$cor)>0.9999) p$v2 else NULL)))
    suggest_remove <- unique(c(perfect_col, names(vif_vals[!is.na(vif_vals) & is.finite(vif_vals) & vif_vals>10])))
    
    sev_col   <- if (length(perfect_col)>0) "#dc3545" else if (length(high_vif)>0) "#fd7e14" else "#ffc107"
    sev_label <- if (length(perfect_col)>0) "Colinéarité parfaite" else if (length(high_vif)>0) "Colinéarité forte (VIF > 5)" else "Colinéarité modérée (|r| >= 0.80)"
    
    shiny::tagList(shiny::div(
      style = paste0("border: 2px solid ", sev_col, "; border-radius: 6px; padding: 12px; margin: 8px 0;"),
      shiny::div(style = paste0("display:flex; align-items:center; gap:8px; margin-bottom:10px; color:", sev_col, ";"),
          shiny::icon("exclamation-triangle"), shiny::tags$strong(sev_label)),
      shiny::tags$small(style="color:#495057;font-weight:bold;display:block;margin-bottom:4px;", "Paires colinéaires :"),
      shiny::tags$table(class="table table-sm table-condensed", style="font-size:11px; margin-bottom:8px;",
                 shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Var 1"), shiny::tags$th("Var 2"), shiny::tags$th("r"), shiny::tags$th("Niveau"))),
                 shiny::tags$tbody(lapply(pairs_high, function(p) {
                   lv  <- if(abs(p$cor)>0.9999) "Parfaite" else if(abs(p$cor)>=0.90) "Très forte" else "Forte"
                   col <- if(abs(p$cor)>0.9999) "#dc3545" else if(abs(p$cor)>=0.90) "#fd7e14" else "#6c757d"
                   shiny::tags$tr(shiny::tags$td(shiny::tags$code(p$v1)), shiny::tags$td(shiny::tags$code(p$v2)),
                           shiny::tags$td(shiny::tags$strong(style=paste0("color:",col), p$cor)),
                           shiny::tags$td(style=paste0("color:",col), lv))
                 }))
      ),
      if (!all(is.na(vif_vals))) shiny::tagList(
        shiny::tags$small(style="color:#495057;font-weight:bold;display:block;margin-bottom:4px;", "VIF :"),
        shiny::div(style="display:flex;flex-wrap:wrap;gap:4px;margin-bottom:8px;",
            lapply(names(vif_vals), function(v) {
              vv <- vif_vals[v]
              col <- if(is.infinite(vv)||vv>10) "#dc3545" else if(vv>5) "#fd7e14" else "#28a745"
              lbl <- if(is.infinite(vv)) "Inf" else round(vv,1)
              shiny::tags$span(style=paste0("background:",col,";color:white;border-radius:3px;padding:2px 6px;font-size:11px;"), paste0(v,": ",lbl))
            })),
        shiny::tags$small(style="color:#6c757d;display:block;margin-bottom:8px;",
                   "VIF < 5 : acceptable · 5-10 : élevé · > 10 : très élevé")
      ),
      shiny::hr(style="margin:8px 0;"),
      shiny::tags$strong(style="font-size:12px;color:#495057;", shiny::icon("tools"), " Corriger :"),
      shiny::div(style="margin-top:8px;display:flex;flex-wrap:wrap;gap:6px;",
          if (length(suggest_remove)>0)
            shiny::actionButton("afdAutoRemoveCollinear",
                         shiny::tagList(shiny::icon("magic"), paste0(" Supprimer auto (", paste(suggest_remove, collapse=", "), ")")),
                         class="btn-sm btn-warning", style="font-size:11px;")
      ),
      if (length(suggest_remove)>0)
        shiny::div(style="margin-top:6px;font-size:11px;color:#6c757d;",
            shiny::icon("info-circle"), paste0(" Variables suggérées : ", paste(suggest_remove, collapse=", ")))
    ))
  })
  
  shiny::observeEvent(input$afdAutoRemoveCollinear, {
    shiny::req(values$filteredData, input$afdVars)
    afd_d <- values$filteredData[, input$afdVars, drop=FALSE]
    afd_d <- afd_d[, sapply(afd_d, is.numeric), drop=FALSE]
    R_mat <- safe_cor(afd_d)
    if (is.null(R_mat)) return()
    to_rem <- c()
    for (i in seq_len(nrow(R_mat))) for (j in seq_len(ncol(R_mat)))
      if (i<j && abs(R_mat[i,j])>0.9999) to_rem <- c(to_rem, colnames(R_mat)[j])
    if (ncol(afd_d)>=3) {
      afd_d_nzv <- remove_zero_var_cols(afd_d)
      vif_v <- sapply(seq_len(ncol(afd_d_nzv)), function(i) {
        y <- afd_d_nzv[[i]]; x <- remove_zero_var_cols(afd_d_nzv[,-i,drop=FALSE])
        if (ncol(x)==0) return(Inf)
        r2 <- tryCatch(suppressWarnings(summary(stats::lm(y~.,data=x))$r.squared), error=function(e) NA)
        if(is.na(r2)||r2>=1) Inf else 1/(1-r2)
      })
      names(vif_v) <- names(afd_d_nzv)
      to_rem <- unique(c(to_rem, names(vif_v[is.finite(vif_v) & vif_v>10])))
    }
    new_vars <- setdiff(input$afdVars, to_rem)
    if (length(new_vars) < 1) {
      shiny::showNotification("Impossible de supprimer toutes les variables (minimum 1 requis).", type="warning", duration=6)
      return()
    }
    updatePickerInput(session, "afdVars", selected=new_vars)
    shiny::showNotification(paste0("Variables retirées : ", paste(to_rem, collapse=", ")), type="message", duration=6)
  })
  
  output$afdMeansGroupSelect <- shiny::renderUI({
    # Vérification explicite au lieu de req()
    if (is.null(values$filteredData) || nrow(values$filteredData) == 0) {
      return(p(style = "color: #856404; font-size: 11px;", 
               shiny::icon("exclamation-triangle"), " Chargez d'abord des données."))
    }
    
    fac_cols <- get_categorical_cols(values$filteredData)
    
    if (length(fac_cols) == 0) {
      return(shiny::div(
        style = "background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 10px 0;",
        p(style = "margin: 0; font-size: 12px; color: #721c24;",
          shiny::icon("exclamation-triangle"), 
          shiny::HTML(" <strong>Attention:</strong> Aucune variable catégorielle disponible."))
      ))
    }
    
    # Par défaut, sélectionner le facteur de discrimination si disponible
    selected_val <- if (!is.null(input$afdFactor) && input$afdFactor %in% fac_cols) {
      input$afdFactor
    } else {
      fac_cols[1]
    }
    
    shiny::selectInput("afdMeansGroup", 
                "Variable de groupement pour les moyennes:", 
                choices = fac_cols,
                selected = selected_val)
  })
  
  afdResultReactive <- shiny::reactive({
    shiny::req(values$filteredData, input$afdVars, input$afdFactor)
    shiny::req(mv_launch$afd)
    
    input$afdUseMeans
    input$afdMeansGroup
    input$afdCrossValidation
    input$afdQualiSup
    input$afdRefresh  
    
    tryCatch({
      use_means <- !is.null(input$afdUseMeans) && input$afdUseMeans && 
        !is.null(input$afdMeansGroup) && input$afdMeansGroup != ""
      
      if (use_means) {
        # Vérifier que le facteur de discrimination est différent du groupe de moyennes
        # ou qu'il y a plusieurs observations par combinaison
        if (input$afdMeansGroup == input$afdFactor) {
          # Calculer les moyennes par groupe - chaque groupe aura une seule ligne
          # L'AFD n'est pas possible avec une seule observation par groupe
          shiny::showNotification(
            "Attention : avec les moyennes par groupe, chaque groupe n'a qu'une observation. L'AFD sera effectuée sur les données individuelles.",
            type = "warning",
            duration = 5
          )
          # Utiliser les données individuelles à la place
          # Conversion de la variable à discriminer en facteur (accepte tous les types)
          if (!is.null(afd_data[[input$afdFactor]])) {
            col_vals <- afd_data[[input$afdFactor]]
            if (inherits(col_vals, c("Date","POSIXct","POSIXlt"))) {
              afd_data[[input$afdFactor]] <- factor(format(col_vals, "%Y-%m-%d"))
            } else if (is.numeric(col_vals) || is.integer(col_vals)) {
              afd_data[[input$afdFactor]] <- factor(as.character(col_vals))
            } else {
              afd_data[[input$afdFactor]] <- factor(as.character(col_vals))
            }
            afd_data[[input$afdFactor]] <- droplevels(afd_data[[input$afdFactor]])
          }
          cols_to_select <- c(input$afdVars, input$afdFactor)
          afd_data <- values$filteredData[, cols_to_select, drop = FALSE]
          use_means <- FALSE
        } else {
          afd_data_numeric <- calculate_group_means(values$filteredData, 
                                                    input$afdVars, 
                                                    input$afdMeansGroup)
          afd_data <- afd_data_numeric
          afd_data[[input$afdFactor]] <- factor(rownames(afd_data_numeric))
          
          # Vérifier qu'il y a au moins 2 observations par groupe
          group_counts <- table(afd_data[[input$afdFactor]])
          if (any(group_counts < 2)) {
            shiny::showNotification(
              "Attention : certains groupes ont moins de 2 observations. L'AFD sera effectuée sur les données individuelles.",
              type = "warning",
              duration = 5
            )
            cols_to_select <- c(input$afdVars, input$afdFactor)
            afd_data <- values$filteredData[, cols_to_select, drop = FALSE]
            use_means <- FALSE
          } else {
            n_groups <- nrow(afd_data)
            shiny::showNotification(
              paste0("AFD sur moyennes: ", n_groups, " groupes (", input$afdMeansGroup, ")"), 
              type = "message", 
              duration = 3,
              id = "afd_means_notif"
            )
          }
        }
      } else {
        # Inclure les variables quali sup si pas de moyennes
        cols_to_select <- c(input$afdVars, input$afdFactor)
        if (!is.null(input$afdQualiSup) && length(input$afdQualiSup) > 0) {
          cols_to_select <- c(cols_to_select, input$afdQualiSup)
        }
        afd_data <- values$filteredData[, cols_to_select, drop = FALSE]
      }
      
      # S'assurer que la variable de groupement est un facteur 
      if (!is.factor(afd_data[[input$afdFactor]])) {
        afd_data[[input$afdFactor]] <- tryCatch(
          factor(as.character(afd_data[[input$afdFactor]])),
          error = function(e) factor(afd_data[[input$afdFactor]])
        )
      }
      # Supprimer les niveaux vides et les NA dans le facteur discriminant
      afd_data <- afd_data[!is.na(afd_data[[input$afdFactor]]), ]
      afd_data[[input$afdFactor]] <- droplevels(afd_data[[input$afdFactor]])
      
      # Vérifier qu'il y a au moins 2 observations par groupe
      group_counts <- table(afd_data[[input$afdFactor]])
      if (any(group_counts < 2)) {
        groups_with_one <- names(group_counts)[group_counts < 2]
        shiny::showNotification(
          paste0("Certains groupes n'ont qu'une observation: ", 
                 paste(groups_with_one, collapse = ", "), 
                 ". L'AFD nécessite au moins 2 observations par groupe."),
          type = "error",
          duration = 10
        )
        return(NULL)
      }
      
      # Vérifier que les variables ne sont pas constantes dans les groupes
      # Une variable est considérée constante si sa variance intra-groupe est nulle
      constant_vars <- c()
      for (var in input$afdVars) {
        var_by_group <- tapply(afd_data[[var]], afd_data[[input$afdFactor]], function(x) {
          if (length(x) < 2) return(0)
          stats::var(x, na.rm = TRUE)
        })
        # Si toutes les variances intra-groupe sont nulles ou NA, la variable est constante
        if (all(is.na(var_by_group) | var_by_group == 0)) {
          constant_vars <- c(constant_vars, var)
        }
      }
      
      if (length(constant_vars) > 0 && length(constant_vars) == length(input$afdVars)) {
        shiny::showNotification(
          paste0("Toutes les variables sont constantes à l'intérieur des groupes. ",
                 "L'AFD n'est pas possible. Vérifiez que vos données ont suffisamment de variabilité."),
          type = "error",
          duration = 10
        )
        return(NULL)
      }
      
      vars_to_use <- setdiff(input$afdVars, constant_vars)
      if (length(constant_vars) > 0) {
        shiny::showNotification(
          paste0("Variables exclues (constantes dans les groupes): ", 
                 paste(constant_vars, collapse = ", ")),
          type = "warning",
          duration = 8
        )
      }
      
      if (length(vars_to_use) < 1) {
        shiny::showNotification("Pas assez de variables non-constantes pour l'AFD.", type = "error", duration = 5)
        return(NULL)
      }
      
      # - Détecter et éliminer les variables colinéaires avant lda() 
      # lda() plante avec solve.default si la matrice intra-groupe est singulière
      if (length(vars_to_use) >= 2) {
        num_afd <- afd_data[, vars_to_use, drop = FALSE]
        num_afd <- num_afd[, sapply(num_afd, is.numeric), drop = FALSE]
        if (ncol(num_afd) >= 2) {
          R_afd <- safe_cor(num_afd)
          if (!is.null(R_afd) && !anyNA(R_afd)) {
            drop_afd <- c()
            for (ci in seq_len(ncol(R_afd))) {
              for (cj in seq_len(ncol(R_afd))) {
                if (ci < cj && colnames(R_afd)[ci] %in% vars_to_use &&
                    !colnames(R_afd)[cj] %in% drop_afd) {
                  if (abs(R_afd[ci, cj]) > 0.9999)
                    drop_afd <- c(drop_afd, colnames(R_afd)[cj])
                }
              }
            }
            if (length(drop_afd) > 0) {
              shiny::showNotification(
                paste0("AFD : variables colinéaires exclues automatiquement -- ",
                       paste(drop_afd, collapse = ", "), "."),
                type = "warning", duration = 8)
              vars_to_use <- setdiff(vars_to_use, drop_afd)
            }
          }
        }
      }
      
      if (length(vars_to_use) < 1) {
        shiny::showNotification("AFD : aucune variable non-colinéaire disponible. Vérifiez vos données.", 
                         type = "error", duration = 8)
        return(NULL)
      }
      
      factor_safe <- paste0("`", input$afdFactor, "`")
      vars_safe <- paste0("`", vars_to_use, "`", collapse = " + ")
      afd_formula <- stats::as.formula(paste(factor_safe, "~", vars_safe))
      
      afd_result <- withCallingHandlers(
        MASS::lda(afd_formula, data = afd_data),
        warning = function(w) {
          if (grepl("collinear|colinéaire", conditionMessage(w), ignore.case = TRUE)) {
            shiny::showNotification(
              paste0("AFD : variables encore partiellement colinéaires (warning LDA). ",
                     "Les résultats peuvent être instables."),
              type = "warning", duration = 6)
          }
          invokeRestart("muffleWarning")
        }
      )
      afd_predict <- tryCatch(
        stats::predict(afd_result, afd_data[, vars_to_use, drop = FALSE]),
        error = function(e) {
          shiny::showNotification(hstat_err_fr(e, "AFD predict()"), type = "error", duration = 8)
          NULL
        }
      )
      if (is.null(afd_predict)) return(NULL)
      
      cv_results <- NULL
      if (!is.null(input$afdCrossValidation) && input$afdCrossValidation && !use_means) {
        factor_levels <- levels(afd_data[[input$afdFactor]])
        
        cv_predictions <- character(nrow(afd_data))
        for (i in seq_len(nrow(afd_data))) {
          train_data <- afd_data[-i, ]
          test_data  <- afd_data[i, , drop = FALSE]
          cv_pred_class <- tryCatch({
            cv_model <- withCallingHandlers(
              MASS::lda(afd_formula, data = train_data),
              warning = function(w) invokeRestart("muffleWarning")
            )
            pred <- stats::predict(cv_model, test_data)
            as.character(pred$class)
          }, error = function(e) NA_character_)
          cv_predictions[i] <- if (is.na(cv_pred_class)) "" else cv_pred_class
        }
        cv_predictions <- factor(cv_predictions, levels = factor_levels)
        cv_results <- list(predictions = cv_predictions)
      }
      
      return(list(
        model = afd_result,
        predictions = afd_predict,
        data = afd_data,
        vars_used = vars_to_use,
        factor_name = input$afdFactor,
        cv_results = cv_results
      ))
      
    }, error = function(e) {
      err_msg <- e$message
      if (grepl("constant", err_msg, ignore.case = TRUE)) {
        shiny::showNotification(
          "Erreur AFD : certaines variables sont constantes à l'intérieur des groupes. Essayez de sélectionner d'autres variables ou vérifiez vos données.",
          type = "error",
          duration = 10
        )
      } else if (grepl("collinear", err_msg, ignore.case = TRUE)) {
        shiny::showNotification(
          "Erreur AFD : certaines variables sont colinéaires. Essayez de réduire le nombre de variables.",
          type = "error",
          duration = 10
        )
      } else {
        shiny::showNotification(paste("Erreur AFD:", err_msg), type = "error", duration = 10)
      }
      return(NULL)
    })
  })
  
  shiny::observe({
    res <- afdResultReactive()
    if (!is.null(res)) {
      values$afdResult <- res$model
    }
  })
  
  # Renommage de 'loadings' en 'coefficients' pour inclure les coefficients discriminants
  afdDataframes <- shiny::reactive({
    shiny::req(afdResultReactive())
    afd_res <- afdResultReactive()
    afd_result <- afd_res$model
    afd_predict <- afd_res$predictions
    afd_data <- afd_res$data
    
    tryCatch({
      ind_coords_df <- as.data.frame(afd_predict$x)
      ind_coords_df <- cbind(
        Individual = rownames(ind_coords_df),
        Groupe_reel = as.character(afd_data[[input$afdFactor]]),
        Groupe_predit = as.character(afd_predict$class),
        ind_coords_df,
        stringsAsFactors = FALSE
      )
      rownames(ind_coords_df) <- NULL
      
      loadings_df <- as.data.frame(afd_result$scaling)
      loadings_df <- cbind(Variable = rownames(loadings_df), loadings_df)
      rownames(loadings_df) <- NULL
      
      afd_vars_nzv2 <- intersect(input$afdVars,
                                 names(remove_zero_var_cols(afd_data[, input$afdVars, drop=FALSE])))
      if (length(afd_vars_nzv2) == 0) afd_vars_nzv2 <- input$afdVars
      X_std <- scale(afd_data[, afd_vars_nzv2, drop=FALSE])
      X_std[is.nan(X_std)] <- 0
      scaling_sub2 <- afd_result$scaling[rownames(afd_result$scaling) %in% afd_vars_nzv2, , drop=FALSE]
      scores <- tryCatch(as.matrix(X_std) %*% scaling_sub2, error=function(e) matrix(0, nrow(X_std), ncol(afd_result$scaling)))
      structure_matrix <- tryCatch(suppressWarnings(stats::cor(X_std, scores)), error=function(e) matrix(NA, ncol(X_std), ncol(scores)))
      structure_df <- as.data.frame(structure_matrix)
      structure_df <- cbind(Variable = rownames(structure_df), structure_df)
      rownames(structure_df) <- NULL
      
      # Tests F univaries avec protection des noms de variables
      f_tests_df <- data.frame(Variable = input$afdVars, stringsAsFactors = FALSE)
      for (var in input$afdVars) {
        # Utiliser backticks pour proteger les noms avec caracteres speciaux
        var_safe <- paste0("`", var, "`")
        factor_safe <- paste0("`", input$afdFactor, "`")
        formula_str <- paste(var_safe, "~", factor_safe)
        
        tryCatch({
          aov_result <- stats::aov(stats::as.formula(formula_str), data = afd_data)
          f_stat <- summary(aov_result)[[1]][1, "F value"]
          p_val <- summary(aov_result)[[1]][1, "Pr(>F)"]
          f_tests_df[f_tests_df$Variable == var, "F_statistic"] <- round(f_stat, 4)
          f_tests_df[f_tests_df$Variable == var, "p_value"] <- round(p_val, 4)
        }, error = function(e) {
          f_tests_df[f_tests_df$Variable == var, "F_statistic"] <- NA
          f_tests_df[f_tests_df$Variable == var, "p_value"] <- NA
        })
      }
      
      confusion_matrix <- table(Reel = afd_data[[input$afdFactor]], 
                                Predit = afd_predict$class)
      confusion_df <- as.data.frame.matrix(confusion_matrix)
      confusion_df <- cbind(Groupe_reel = rownames(confusion_df), confusion_df)
      rownames(confusion_df) <- NULL
      
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      group_acc_df <- data.frame(
        Groupe = rownames(confusion_matrix),
        Taux_classification = numeric(nrow(confusion_matrix)),
        stringsAsFactors = FALSE
      )
      for (i in seq_len(nrow(confusion_matrix))) {
        group_acc_df$Taux_classification[i] <- round(confusion_matrix[i,i] / sum(confusion_matrix[i,]) * 100, 2)
      }
      
      centroids_df <- as.data.frame(afd_result$means)
      centroids_df <- cbind(Groupe = rownames(centroids_df), centroids_df)
      rownames(centroids_df) <- NULL
      
      eigenvals <- afd_result$svd^2
      prop_var <- eigenvals / sum(eigenvals) * 100
      can_cor <- sqrt(eigenvals / (1 + eigenvals))
      
      variance_df <- data.frame(
        Fonction = paste0("LD", 1:length(eigenvals)),
        Valeur_propre = eigenvals,
        Variance_expliquee = round(prop_var, 2),
        Variance_cumulee = round(cumsum(prop_var), 2),
        Correlation_canonique = round(can_cor, 4),
        stringsAsFactors = FALSE
      )
      
      cv_confusion_df <- NULL
      cv_accuracy_df <- NULL
      if (!is.null(afd_res$cv_results)) {
        cv_confusion <- table(Reel = afd_data[[input$afdFactor]], 
                              Predit = afd_res$cv_results$predictions)
        cv_confusion_df <- as.data.frame.matrix(cv_confusion)
        cv_confusion_df <- cbind(Groupe_reel = rownames(cv_confusion_df), cv_confusion_df)
        rownames(cv_confusion_df) <- NULL
        
        cv_accuracy <- sum(diag(cv_confusion)) / sum(cv_confusion)
        cv_accuracy_df <- data.frame(
          Metrique = "Taux_classification_global",
          Valeur = round(cv_accuracy * 100, 2),
          stringsAsFactors = FALSE
        )
      }
      
      return(list(
        ind_coords = ind_coords_df,
        coefficients = loadings_df,
        structure_matrix = structure_df,
        f_tests = f_tests_df,
        confusion_matrix = confusion_df,
        classification_rates = group_acc_df,
        centroids = centroids_df,
        variance_explained = variance_df,
        cv_confusion = cv_confusion_df,
        cv_accuracy = cv_accuracy_df
      ))
    }, error = function(e) {
      shiny::showNotification(hstat_err_fr(e, "Erreur dataframes AFD"), type = "error")
      return(NULL)
    })
  })
  
  # Stocker les dataframes de l'AFD dans values$ pour un acces fiable
  shiny::observe({
    shiny::req(afdResultReactive())
    
    # Attendre un peu pour laisser le temps au reactive de s'exécuter complètement
    shiny::isolate({
      tryCatch({
        dfs <- afdDataframes()
        
        if (!is.null(dfs)) {
          if (!is.null(dfs$ind_coords) && nrow(dfs$ind_coords) > 0) {
            values$afdDataframes <- dfs
            
            n_individus <- nrow(dfs$ind_coords)
            n_groupes <- length(unique(dfs$ind_coords$Groupe_reel))
            
            msg <- paste0("AFD: ", n_individus, " individus, ", n_groupes, " groupes")
            shiny::showNotification(msg, type = "message", duration = 3)
          } else {
            shiny::showNotification("Avertissement : dataframes AFD créés mais vides", type = "warning", duration = 5)
          }
        } else {
          shiny::showNotification("Erreur : impossible de créer les dataframes AFD", type = "error", duration = 5)
        }
      }, error = function(e) {
        shiny::showNotification(hstat_err_fr(e, "Erreur stockage AFD"), type = "error", duration = 5)
      })
    })
  })
  
  
  generate_distinct_colors <- function(n) {
    if (n <= 0) return(character(0))
    
    if (n <= 12) {
      # Pour 12 couleurs ou moins, utiliser des palettes prédéfinies combinées
      colors <- c(
        "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
        "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB"
      )
      return(colors[1:n])
    } else if (n <= 24) {
      pal1 <- RColorBrewer::brewer.pal(min(n, 12), "Set3")
      pal2 <- RColorBrewer::brewer.pal(min(n - 12, 12), "Paired")
      return(c(pal1, pal2)[1:n])
    } else {
      # Pour plus de 24 couleurs, utiliser une génération automatique
      # avec des teintes espacées uniformément
      hcl.colors(n, palette = "Dynamic", alpha = 0.8)
    }
  }
  
  output$afdAxisXSelect <- shiny::renderUI({
    shiny::req(afdResultReactive())
    afd_res <- afdResultReactive()
    n_dims <- ncol(afd_res$predictions$x)
    
    if (n_dims == 1) {
      return(shiny::div(style = "padding: 10px; background-color: #fff3cd; border-radius: 4px;",
                 p(style = "margin: 0; color: #856404; font-size: 12px;",
                   shiny::icon("info-circle"), " Une seule fonction discriminante disponible")))
    }
    
    shiny::selectInput("afdAxisX", "Axe X:",
                choices = stats::setNames(1:n_dims, paste0("LD", 1:n_dims)),
                selected = 1)
  })
  
  output$afdAxisYSelect <- shiny::renderUI({
    shiny::req(afdResultReactive())
    afd_res <- afdResultReactive()
    n_dims <- ncol(afd_res$predictions$x)
    
    if (n_dims == 1) {
      return(shiny::div(style = "padding: 10px; background-color: #fff3cd; border-radius: 4px;",
                 p(style = "margin: 0; color: #856404; font-size: 12px;",
                   shiny::icon("info-circle"), " Une seule fonction discriminante disponible")))
    }
    
    shiny::selectInput("afdAxisY", "Axe Y:",
                choices = stats::setNames(1:n_dims, paste0("LD", 1:n_dims)),
                selected = min(2, n_dims))
  })
  
  createAfdIndPlot <- function(afd_res) {
    afd_predict <- afd_res$predictions
    afd_data    <- afd_res$data
    # Utiliser le nom du facteur stocké dans afd_res (pas input$afdFactor qui est NULL hors réactif)
    factor_name <- afd_res$factor_name %||%
      names(afd_data)[sapply(afd_data, is.factor)][1] %||%
      names(afd_data)[ncol(afd_data)]
    
    n_dims <- ncol(afd_predict$x)
    
    # Axes sélectionnés (par défaut 1 et 2, ou 1 et densité si une seule dimension)
    axis_x <- if (!is.null(input$afdAxisX)) as.numeric(input$afdAxisX) else 1
    axis_y <- if (!is.null(input$afdAxisY) && n_dims > 1) as.numeric(input$afdAxisY) else NULL
    
    ind_title <- if (!is.null(input$afdIndTitle) && input$afdIndTitle != "") {
      input$afdIndTitle
    } else {
      "AFD - Projection des individus"
    }
    
    afd_df <- as.data.frame(afd_predict$x)
    afd_df$Groupe     <- afd_data[[factor_name]]
    afd_df$Individual <- rownames(afd_data)
    
    x_label <- if (!is.null(input$afdIndXLabel) && input$afdIndXLabel != "") {
      input$afdIndXLabel
    } else {
      paste0("LD", axis_x)
    }
    
    y_label <- if (!is.null(input$afdIndYLabel) && input$afdIndYLabel != "") {
      input$afdIndYLabel
    } else {
      if (n_dims > 1 && !is.null(axis_y)) paste0("LD", axis_y) else "Densité"
    }
    
    n_groups <- length(unique(afd_df$Groupe))
    group_colors <- generate_distinct_colors(n_groups)
    names(group_colors) <- levels(factor(afd_df$Groupe))

    afd_pt   <- if (!is.null(input$afdPointSize)) input$afdPointSize else 3
    afd_lbl  <- hstat_lbl_pt2gg(input$afdLabelSize)
    afd_txt  <- if (!is.null(input$afdAxisTextSize)) input$afdAxisTextSize else 12

    if (n_dims > 1 && !is.null(axis_y)) {
      x_col <- paste0("LD", axis_x)
      y_col <- paste0("LD", axis_y)
      
      p_ind <- ggplot2::ggplot(afd_df, ggplot2::aes_string(x = x_col, y = y_col, 
                                         color = "Groupe", label = "Individual")) +
        ggplot2::geom_point(size = afd_pt, alpha = 0.7) +
        ggplot2::geom_text(vjust = -0.5, hjust = 0.5, size = afd_lbl, check_overlap = TRUE) +
        mv_ggtheme("afdInd") +
        labs(title = ind_title, x = x_label, y = y_label) +
        ggplot2::scale_color_manual(values = group_colors) +
        ggplot2::theme(legend.position = "right",
              legend.title = ggtext::element_markdown(size = afd_txt - 2, face = "bold"),
              legend.text = ggplot2::element_text(size = afd_txt - 3),
              axis.title = ggplot2::element_text(size = afd_txt),
              axis.text = ggplot2::element_text(size = afd_txt - 2))
      
      if (!is.null(input$afdIndCenterAxes) && input$afdIndCenterAxes) {
        coords <- afd_predict$x[, c(axis_x, axis_y)]
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p_ind <- p_ind + ggplot2::xlim(-max_range, max_range) + ggplot2::ylim(-max_range, max_range)
      }
    } else {
      x_col <- paste0("LD", axis_x)
      
      p_ind <- ggplot2::ggplot(afd_df, ggplot2::aes_string(x = x_col, fill = "Groupe", color = "Groupe")) +
        ggplot2::geom_density(alpha = 0.5) +
        mv_ggtheme("afdInd") +
        labs(title = ind_title, x = x_label, y = y_label) +
        ggplot2::scale_color_manual(values = group_colors) +
        ggplot2::scale_fill_manual(values = group_colors) +
        ggplot2::theme(legend.position = "right",
              legend.title = ggtext::element_markdown(size = afd_txt - 2, face = "bold"),
              legend.text = ggplot2::element_text(size = afd_txt - 3),
              axis.title = ggplot2::element_text(size = afd_txt),
              axis.text = ggplot2::element_text(size = afd_txt - 2))
    }
    
    return(p_ind)
  }
  
  createAfdVarPlot <- function(afd_res) {
    afd_result <- afd_res$model
    afd_data <- afd_res$data
    
    n_dims <- length(afd_result$svd)
    
    # Axes sélectionnés (par défaut 1 et 2, ou 1 et corrélation si une seule dimension)
    axis_x <- if (!is.null(input$afdAxisX)) as.numeric(input$afdAxisX) else 1
    axis_y <- if (!is.null(input$afdAxisY) && n_dims > 1) as.numeric(input$afdAxisY) else NULL
    
    var_title <- if (!is.null(input$afdVarTitle) && input$afdVarTitle != "") {
      input$afdVarTitle
    } else {
      "AFD - Contribution des variables"
    }
    
    # Utiliser les variables stockées dans afd_res (pas input$afdVars qui est NULL hors réactif)
    vars_used <- afd_res$vars_used
    if (is.null(vars_used) || length(vars_used) == 0) {
      vars_used <- names(afd_data)[sapply(afd_data, is.numeric)]
    }
    if (is.null(vars_used) || length(vars_used) == 0) stop("Aucune variable disponible pour le graphique AFD.")
    
    X_std <- scale(afd_data[, vars_used, drop = FALSE])
    scores <- as.matrix(X_std) %*% afd_result$scaling
    structure_matrix <- stats::cor(X_std, scores)
    
    var_df <- as.data.frame(structure_matrix)
    var_df$Variable <- rownames(structure_matrix)
    
    # - Importance discriminatoire globale -
    # Pondération par la variance expliquée de chaque fonction LD
    eigenvals  <- afd_result$svd^2
    prop_var   <- eigenvals / sum(eigenvals)   # poids de chaque LD
    n_ld_use   <- min(n_dims, ncol(structure_matrix))
    # Score d'importance = somme pondérée de |corrélation|² sur toutes les fonctions
    importance <- sapply(seq_len(nrow(structure_matrix)), function(i) {
      sqrt(sum(prop_var[seq_len(n_ld_use)] * structure_matrix[i, seq_len(n_ld_use)]^2))
    })
    var_df$Importance <- importance
    
    var_df$Niveau <- cut(
      var_df$Importance,
      breaks = c(-Inf, 0.30, 0.50, 0.70, Inf),
      labels = c("Faible (< 0.30)", "Modérée (0.30-0.50)", "Forte (0.50-0.70)", "Très forte (> 0.70)")
    )
    disc_colors <- c(
      "Faible (< 0.30)"       = "#bdc3c7",
      "Modérée (0.30-0.50)"   = "#3498db",
      "Forte (0.50-0.70)"     = "#e67e22",
      "Très forte (> 0.70)"   = "#c0392b"
    )
    
    x_label <- if (!is.null(input$afdVarXLabel) && input$afdVarXLabel != "") {
      input$afdVarXLabel
    } else {
      paste0("LD", axis_x)
    }
    
    y_label <- if (!is.null(input$afdVarYLabel) && input$afdVarYLabel != "") {
      input$afdVarYLabel
    } else {
      if (n_dims > 1 && !is.null(axis_y)) paste0("LD", axis_y) else "Corrélation"
    }
    
    if (n_dims > 1 && !is.null(axis_y)) {
      # - Graphique 2D : flèches colorées par importance -
      x_col <- paste0("LD", axis_x)
      y_col <- paste0("LD", axis_y)
      
      afd_ln  <- if (!is.null(input$afdLineWidth)) input$afdLineWidth else 1.3
      afd_vlbl <- hstat_lbl_pt2gg(input$afdVarLabelSize)
      p_var <- ggplot2::ggplot(var_df, ggplot2::aes_string(x = x_col, y = y_col,
                                         color = "Niveau", label = "Variable")) +
        ggplot2::geom_segment(ggplot2::aes_string(xend = x_col, yend = y_col, color = "Niveau"),
                     x = 0, y = 0,
                     arrow = ggplot2::arrow(length = ggplot2::unit(0.28, "cm"), type = "closed"),
                     linewidth = afd_ln) +
        ggplot2::geom_point(ggplot2::aes_string(size = "Importance"), alpha = 0.85) +
        ggplot2::geom_text(vjust = -0.6, hjust = 0.5, size = afd_vlbl, fontface = "bold",
                  color = "grey20") +
        ggplot2::scale_color_manual(values = disc_colors, name = "Importance\ndiscriminatoire",
                           drop = FALSE) +
        ggplot2::scale_size_continuous(range = c(2, 6), guide = "none") +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
        mv_ggtheme("afdVar") +
        labs(title = var_title,
             subtitle = "Couleur = importance discriminatoire globale (corrélation pondérée par la variance de chaque LD)",
             x = x_label, y = y_label) +
        ggplot2::theme(
          plot.title    = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 13),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "#555", size = 10),
          legend.position  = "right",
          legend.title     = ggplot2::element_text(size = 10, face = "bold"),
          panel.grid.minor = ggplot2::element_blank()
        )
      
      if (!is.null(input$afdVarCenterAxes) && input$afdVarCenterAxes) {
        coords    <- structure_matrix[, c(axis_x, axis_y)]
        max_range <- max(abs(range(coords, na.rm = TRUE)))
        p_var     <- p_var + ggplot2::xlim(-max_range, max_range) + ggplot2::ylim(-max_range, max_range)
      }
      
    } else {
      # - Graphique 1D : barres colorées par importance -
      x_col <- paste0("LD", axis_x)
      
      var_df_ordered <- var_df[order(var_df$Importance, decreasing = TRUE), ]
      var_df_ordered$Variable <- factor(var_df_ordered$Variable,
                                        levels = rev(var_df_ordered$Variable))
      
      p_var <- ggplot2::ggplot(var_df_ordered,
                      ggplot2::aes_string(x = "Variable", y = x_col, fill = "Niveau")) +
        ggplot2::geom_bar(stat = "identity", color = "white", linewidth = 0.4) +
        ggplot2::coord_flip() +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40",
                   linewidth = 0.7) +
        ggplot2::geom_hline(yintercept =  0.30, linetype = "dotted", color = "#3498db",
                   linewidth = 0.5, alpha = 0.7) +
        ggplot2::geom_hline(yintercept = -0.30, linetype = "dotted", color = "#3498db",
                   linewidth = 0.5, alpha = 0.7) +
        ggplot2::annotate("text", x = 0.6, y = 0.32, label = "seuil 0.30",
                 hjust = 0, color = "#3498db", size = 3, fontface = "italic") +
        ggplot2::scale_fill_manual(values = disc_colors, name = "Importance\ndiscriminatoire",
                          drop = FALSE) +
        ggplot2::scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
        labs(title = var_title,
             subtitle = "Couleur = niveau d'importance discriminatoire (|corrélation| pondérée par la variance expliquée)",
             x = NULL, y = y_label) +
        mv_ggtheme("afdVar") +
        ggplot2::theme(
          plot.title    = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 13),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "#555", size = 10),
          legend.position  = "right",
          legend.title     = ggplot2::element_text(size = 10, face = "bold"),
          panel.grid.minor = ggplot2::element_blank()
        )
    }
    
    return(p_var)
  }
  
  output$afdIndPlot <- shiny::renderPlot({
    shiny::req(values$filteredData, input$afdFactor)
    suppressWarnings(suppressMessages(mv_legacy(createAfdIndPlot(afdResultReactive()), "afdInd")))
  }, res = 120)
  
  output$afdVarPlot <- shiny::renderPlot({
    shiny::req(values$filteredData, input$afdFactor)
    suppressWarnings(suppressMessages(mv_legacy(createAfdVarPlot(afdResultReactive()), "afdVar")))
  }, res = 120)
  
  output$afdSummary <- shiny::renderUI({
    shiny::req(afdResultReactive())
    afd_res     <- afdResultReactive()
    afd_result  <- afd_res$model
    afd_predict <- afd_res$predictions
    afd_data    <- afd_res$data
    vars_used   <- if (!is.null(afd_res$vars_used)) afd_res$vars_used else input$afdVars
    use_round   <- !is.null(input$afdRoundResults) && input$afdRoundResults
    dec         <- if (use_round && !is.null(input$afdDecimals)) input$afdDecimals else 3
    
    tryCatch({
      eigenvals <- afd_result$svd^2
      prop_var  <- eigenvals / sum(eigenvals) * 100
      can_cor   <- sqrt(eigenvals / (1 + eigenvals))
      eta2_vals <- eigenvals / (1 + eigenvals)
      
      confusion_matrix     <- table(Reel = afd_data[[input$afdFactor]], Predit = afd_predict$class)
      accuracy             <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      n_total              <- sum(confusion_matrix)
      expected_agreement   <- sum(rowSums(confusion_matrix) * colSums(confusion_matrix)) / (n_total^2)
      kappa_val            <- (accuracy - expected_agreement) / (1 - expected_agreement)
      
      X_std            <- scale(afd_data[, vars_used, drop = FALSE])
      scores           <- as.matrix(X_std) %*% afd_result$scaling
      structure_matrix <- stats::cor(X_std, scores)
      
      card <- function(..., border_color = "#dee2e6", bg = "white") {
        shiny::div(style = paste0(
          "background:", bg, "; border-radius:8px; padding:16px; margin-bottom:14px;",
          "border:1px solid ", border_color, "; box-shadow: 0 1px 4px rgba(0,0,0,.06);"
        ), ...)
      }
      section_header <- function(num, label, color = "#3a6186", icon_name = "chart-bar") {
        shiny::div(style = paste0(
          "background: linear-gradient(135deg,", color, " 0%, ", color, "cc 100%);",
          "border-radius:6px; padding:11px 16px; margin-bottom:12px;"
        ),
        shiny::h5(style = "color:white; margin:0; font-weight:bold; font-size:15px;",
           shiny::icon(icon_name), paste0("  ", label))
        )
      }
      badge <- function(val, label, color) {
        shiny::div(style = "text-align:center;",
            shiny::div(style = paste0(
              "display:inline-block; background:", color, "; color:white;",
              "border-radius:8px; padding:8px 16px; font-size:26px; font-weight:bold;",
              "min-width:80px; margin-bottom:6px;"
            ), val),
            p(style = "margin:2px 0 0 0; font-size:13px; color:#444; text-transform:uppercase;", label)
        )
      }
      interp_bar <- function(text, color) {
        shiny::div(style = paste0(
          "margin-top:8px; padding:8px 14px; border-left:4px solid ", color, ";",
          "background:#f4f6f8; border-radius:0 4px 4px 0;"
        ),
        p(style = "margin:0; font-size:13px; color:#2c3e50; font-weight:500;",
          shiny::icon("info-circle"), " ", text)
        )
      }
      info_note <- function(text) {
        p(style = "font-size:13px; color:#5a6a7a; font-style:italic; margin:6px 0 0 0;", text)
      }
      kv_row <- function(label, val) {
        shiny::tags$tr(
          shiny::tags$td(style = "padding:4px 10px 4px 0; color:#555; font-size:12px; white-space:nowrap;", label),
          shiny::tags$td(style = "padding:4px 0; font-weight:bold; font-size:12px; color:#2c3e50;", val)
        )
      }
      
      acc_color <- if (accuracy >= .9) "#3a7d5c" else if (accuracy >= .8) "#4a7fa5" else if (accuracy >= .7) "#b07d2a" else "#c0392b"
      acc_interp <- if (accuracy >= .9) "Excellent -- la discrimination est quasi-parfaite."
      else if (accuracy >= .8) "Bon -- la discrimination est fiable."
      else if (accuracy >= .7) "Acceptable -- des erreurs subsistent."
      else "Faible -- le modèle discrimine mal les groupes."
      
      kappa_color <- if (kappa_val >= .8) "#3a7d5c" else if (kappa_val >= .6) "#4a7fa5" else if (kappa_val >= .4) "#b07d2a" else "#c0392b"
      kappa_interp <- if (kappa_val >= .8) "Quasi-parfait (>= 0,80) -- accord excellent au-dela du hasard."
      else if (kappa_val >= .6) "Substantiel (0,60 - 0,80) -- bon accord."
      else if (kappa_val >= .4) "Modéré (0,40 - 0,60) -- accord partiel."
      else if (kappa_val >= .2) "Passable (0,20 - 0,40) -- accord faible."
      else "Faible (< 0,20) -- accord proche du hasard."
      
      eta2_colors <- sapply(eta2_vals, function(e)
        if (e >= .64) "#3a5f7d" else if (e >= .25) "#3a7d5c" else if (e >= .09) "#b07d2a" else "#c0392b")
      eta2_interps <- sapply(eta2_vals, function(e)
        if (e >= .64) "Excellent (>= 0,64)" else if (e >= .25) "Fort (0,25 - 0,64)"
        else if (e >= .09) "Modéré (0,09 - 0,25)" else "Faible (< 0,09)")
      
      shiny::tagList(
        
        card(border_color = "#4a7fa5",
             section_header("1", "Variance expliquée & force de discrimination (eta\u00b2)", "#4a7fa5", "chart-pie"),
             info_note("Chaque fonction discriminante (LD) est évaluée par sa valeur propre, la variance qu'elle explique et son eta\u00b2."),
             shiny::tags$table(style = "width:100%; border-collapse:collapse; margin-top:10px;",
                        shiny::tags$thead(
                          shiny::tags$tr(style = "background:#4a7fa5; color:white;",
                                  shiny::tags$th(style = "padding:7px 8px; text-align:left; font-size:11px; border-radius:4px 0 0 0;", "Fonction"),
                                  shiny::tags$th(style = "padding:7px 8px; text-align:center; font-size:11px;", "Val. propre"),
                                  shiny::tags$th(style = "padding:7px 8px; text-align:center; font-size:11px;", "Variance (%)"),
                                  shiny::tags$th(style = "padding:7px 8px; text-align:center; font-size:11px;", "Cumul (%)"),
                                  shiny::tags$th(style = "padding:7px 8px; text-align:center; font-size:11px;", "Corr. canonique"),
                                  shiny::tags$th(style = "padding:7px 8px; text-align:center; font-size:11px; border-radius:0 4px 0 0;", "eta\u00b2")
                          )
                        ),
                        shiny::tags$tbody(
                          lapply(seq_along(eigenvals), function(i) {
                            bg <- if (i %% 2 == 0) "#f4f6f8" else "white"
                            shiny::tags$tr(style = paste0("background:", bg, ";"),
                                    shiny::tags$td(style = "padding:7px 8px; font-weight:bold; font-size:13px; color:#2c3e50;",
                                            paste0("LD", i)),
                                    shiny::tags$td(style = "padding:7px 8px; text-align:center; font-size:13px;",
                                            round(eigenvals[i], dec)),
                                    shiny::tags$td(style = "padding:7px 8px; text-align:center; font-size:13px;",
                                            paste0(round(prop_var[i], 1), "%")),
                                    shiny::tags$td(style = "padding:7px 8px; text-align:center; font-size:13px;",
                                            paste0(round(cumsum(prop_var)[i], 1), "%")),
                                    shiny::tags$td(style = "padding:7px 8px; text-align:center; font-size:13px;",
                                            round(can_cor[i], dec)),
                                    shiny::tags$td(style = paste0(
                                      "padding:7px 8px; text-align:center; font-size:13px;",
                                      "font-weight:bold; color:", eta2_colors[i], ";"
                                    ),
                                    paste0(round(eta2_vals[i], dec), " -- ", eta2_interps[i]))
                            )
                          })
                        )
             ),
             shiny::div(style = "margin-top:10px; padding:8px; background:#eef4f9; border-radius:6px;",
                 p(style = "margin:0; font-size:13px; color:#3a6186;",
                   shiny::icon("ruler"), "  Seuils eta\u00b2 : ",
                   shiny::tags$strong("Excellent"), " \u2265 0,64  |  ",
                   shiny::tags$strong("Fort"), " 0,25-0,64  |  ",
                   shiny::tags$strong("Modéré"), " 0,09-0,25  |  ",
                   shiny::tags$strong("Faible"), " < 0,09"
                 )
             )
        ),
        
        card(border_color = "#4a8c6f",
             section_header("2", "Qualité de classification -- Accuracy & Kappa de Cohen", "#4a8c6f", "bullseye"),
             info_note("L'accuracy mesure la proportion d'individus bien classés. Le Kappa corrige ce taux en tenant compte de l'accord dû au seul hasard."),
             shiny::fluidRow(
               shiny::column(4,
                      card(border_color = acc_color, bg = "#f8f9fa",
                           badge(paste0(round(accuracy * 100, 1), "%"), "Accuracy globale", acc_color),
                           interp_bar(acc_interp, acc_color)
                      )
               ),
               shiny::column(4,
                      card(border_color = kappa_color, bg = "#f8f9fa",
                           badge(round(kappa_val, 3), "Kappa de Cohen", kappa_color),
                           interp_bar(kappa_interp, kappa_color)
                      )
               ),
               shiny::column(4,
                      card(border_color = "#6c5b8e", bg = "#f8f9fa",
                           shiny::div(style = "text-align:center;",
                               shiny::div(style = "font-size:22px; font-weight:bold; color:#6c5b8e;",
                                   paste0(nrow(confusion_matrix), " groupes")),
                               p(style = "font-size:13px; color:#444; margin:4px 0 0 0; text-transform:uppercase;",
                                 "Groupes discriminés"),
                               p(style = "font-size:13px; color:#555; margin:8px 0 0 0;",
                                 paste0(n_total, " individus classés"))
                           ),
                           interp_bar("Seuils Kappa : \u2265 0,80 quasi-parfait | 0,60-0,80 substantiel | 0,40-0,60 modéré", "#6c5b8e")
                      )
               )
             ),
             
             shiny::h6(style = "color:#2c3e50; font-weight:bold; margin:14px 0 4px 0; font-size:14px;",
                shiny::icon("list"), "  Taux de classification par groupe"),
             shiny::div(style = "padding:6px 10px; background:#f0f4f8; border-radius:6px; margin-bottom:8px;",
                 p(style = "margin:0; font-size:12px; color:#3a6186;",
                   shiny::icon("ruler"),
                   shiny::tags$strong(" Seuils : "),
                   shiny::tags$span(style = "color:#3a7d5c;", "\u2265 90% -- Excellent"), " | ",
                   shiny::tags$span(style = "color:#b07d2a;", "70-90% -- Acceptable"), " | ",
                   shiny::tags$span(style = "color:#c0392b;", "< 70% -- Faible")
                 )
             ),
             shiny::fluidRow(
               lapply(1:nrow(confusion_matrix), function(i) {
                 g_acc <- confusion_matrix[i, i] / sum(confusion_matrix[i, ])
                 g_col <- if (g_acc >= .9) "#3a7d5c" else if (g_acc >= .7) "#b07d2a" else "#c0392b"
                 g_bg  <- if (g_acc >= .9) "#eaf5ef" else if (g_acc >= .7) "#fef9ec" else "#fdf0ef"
                 shiny::column(max(2, floor(12 / nrow(confusion_matrix))),
                        shiny::div(style = paste0(
                          "background:", g_bg, "; border:2px solid ", g_col, ";",
                          "border-radius:8px; padding:10px; text-align:center; margin-bottom:8px;"
                        ),
                        p(style = "margin:0; font-size:13px; color:#2c3e50; font-weight:bold; word-break:break-word;",
                          rownames(confusion_matrix)[i]),
                        shiny::div(style = paste0("font-size:24px; font-weight:bold; color:", g_col, "; margin-top:4px;"),
                            paste0(round(g_acc * 100, 1), "%"))
                        )
                 )
               })
             )
        ),
        
        card(border_color = "#b07840",
             section_header("3", "Matrice de confusion", "#b07840", "th"),
             info_note("Lecture : lignes = groupes réels, colonnes = groupes prédits. La diagonale représente les classifications correctes."),
             shiny::div(style = "overflow-x:auto;",
                 shiny::tags$table(style = "border-collapse:collapse; min-width:200px;",
                            shiny::tags$thead(
                              shiny::tags$tr(
                                shiny::tags$th(style = "padding:7px 10px; background:#f4f6f8; border:1px solid #dee2e6; font-size:13px;",
                                        "Réel \\ Prédit"),
                                lapply(colnames(confusion_matrix), function(cn)
                                  shiny::tags$th(style = "padding:7px 10px; background:#b07840; color:white; border:1px solid #dee2e6; font-size:13px; text-align:center;",
                                          cn)
                                )
                              )
                            ),
                            shiny::tags$tbody(
                              lapply(1:nrow(confusion_matrix), function(i) {
                                shiny::tags$tr(
                                  shiny::tags$td(style = "padding:7px 10px; background:#f9f2eb; border:1px solid #dee2e6; font-weight:bold; font-size:13px; color:#2c3e50;",
                                          rownames(confusion_matrix)[i]),
                                  lapply(1:ncol(confusion_matrix), function(j) {
                                    is_diag <- i == j
                                    bg <- if (is_diag) "#eaf5ef" else "white"
                                    fw <- if (is_diag) "bold" else "normal"
                                    col <- if (is_diag) "#3a7d5c" else "#2c3e50"
                                    shiny::tags$td(style = paste0(
                                      "padding:7px 10px; background:", bg, "; border:1px solid #dee2e6;",
                                      "text-align:center; font-weight:", fw, "; color:", col, "; font-size:13px;"
                                    ), confusion_matrix[i, j])
                                  })
                                )
                              })
                            )
                 )
             )
        ),
        
        card(border_color = "#6c5b8e",
             section_header("4", "Matrice de structure -- Corrélations variables-fonctions", "#6c5b8e", "project-diagram"),
             info_note("Les corrélations indiquent la contribution de chaque variable aux fonctions discriminantes. |r| \u2265 0,70 : forte | 0,40-0,70 : modérée | < 0,40 : faible."),
             shiny::div(style = "overflow-x:auto;",
                 shiny::tags$table(style = "border-collapse:collapse; width:100%;",
                            shiny::tags$thead(
                              shiny::tags$tr(
                                shiny::tags$th(style = "padding:7px 10px; background:#6c5b8e; color:white; border:1px solid #dee2e6; font-size:13px; text-align:left;",
                                        "Variable"),
                                lapply(colnames(structure_matrix), function(cn)
                                  shiny::tags$th(style = "padding:7px 10px; background:#6c5b8e; color:white; border:1px solid #dee2e6; font-size:13px; text-align:center;",
                                          cn)
                                )
                              )
                            ),
                            shiny::tags$tbody(
                              lapply(1:nrow(structure_matrix), function(i) {
                                bg <- if (i %% 2 == 0) "#f4f6f8" else "white"
                                shiny::tags$tr(style = paste0("background:", bg, ";"),
                                        shiny::tags$td(style = "padding:7px 10px; border:1px solid #dee2e6; font-weight:bold; font-size:13px; color:#2c3e50;",
                                                rownames(structure_matrix)[i]),
                                        lapply(1:ncol(structure_matrix), function(j) {
                                          v   <- structure_matrix[i, j]
                                          av  <- abs(v)
                                          col <- if (av >= .7) "#3a5f7d" else if (av >= .4) "#4a7fa5" else "#888"
                                          fw  <- if (av >= .4) "bold" else "normal"
                                          shiny::tags$td(style = paste0(
                                            "padding:7px 10px; border:1px solid #dee2e6;",
                                            "text-align:center; color:", col, "; font-weight:", fw, "; font-size:13px;"
                                          ), round(v, dec))
                                        })
                                )
                              })
                            )
                 )
             )
        ),
        
        card(border_color = "#3a8070",
             section_header("5", "Validation croisée Leave-One-Out (LOO)", "#3a8070", "sync-alt"),
             if (!is.null(afd_res$cv_results)) {
               cv_conf    <- table(Reel = afd_data[[input$afdFactor]], Predit = afd_res$cv_results$predictions)
               cv_acc     <- sum(diag(cv_conf)) / sum(cv_conf)
               cv_col     <- if (cv_acc >= .9) "#3a7d5c" else if (cv_acc >= .8) "#4a7fa5" else if (cv_acc >= .7) "#b07d2a" else "#c0392b"
               cv_interp  <- if (cv_acc >= .9) "Excellente capacité prédictive (>= 90%)."
               else if (cv_acc >= .8) "Bonne capacité prédictive (80-90%)."
               else if (cv_acc >= .7) "Capacité prédictive acceptable (70-80%)."
               else "Capacité prédictive faible (< 70%) -- risque de sur-ajustement."
               bias       <- accuracy - cv_acc
               bias_interp <- if (abs(bias) < 0.03) "Différence négligeable -- modèle stable."
               else if (abs(bias) < 0.08) "Légère différence -- sur-ajustement modéré."
               else "Différence importante -- attention au sur-ajustement."
               shiny::tagList(
                 info_note("La LOO exclut un individu à la fois pour tester la prédiction. Elle évalue la capacité généralisatrice du modèle."),
                 shiny::fluidRow(
                   shiny::column(4, badge(paste0(round(cv_acc * 100, 1), "%"), "Accuracy LOO", cv_col)),
                   shiny::column(4, badge(paste0(round(accuracy * 100, 1), "%"), "Accuracy entrainement", acc_color)),
                   shiny::column(4, badge(paste0(if (bias > 0) "+" else "", round(bias * 100, 1), "%"), "Biais (train - LOO)",
                                   if (abs(bias) < 0.03) "#3a7d5c" else if (abs(bias) < 0.08) "#b07d2a" else "#c0392b"))
                 ),
                 interp_bar(cv_interp, cv_col),
                 interp_bar(bias_interp, if (abs(bias) < 0.03) "#3a7d5c" else if (abs(bias) < 0.08) "#b07d2a" else "#c0392b")
               )
             } else {
               shiny::div(style = "padding:10px; background:#fff3cd; border-radius:6px;",
                   p(style = "margin:0; font-size:12px; color:#856404;",
                     shiny::icon("exclamation-triangle"),
                     if (!is.null(input$afdUseMeans) && input$afdUseMeans)
                       "  Non disponible avec les moyennes par groupe. La validation croisée LOO nécessite des observations individuelles."
                     else
                       "  Non activée. Cochez l'option 'Activer la validation croisée (LOO)' dans le panneau de gauche pour l'utiliser."
                   )
               )
             }
        ),
        
        card(border_color = "#2c3e50",
             section_header("6", "Centroides des groupes & Probabilités a priori", "#2c3e50", "map-marker-alt"),
             info_note("Les centroïdes sont les moyennes des variables par groupe dans l'espace original. Les probabilités a priori reflètent les proportions de chaque groupe."),
             shiny::fluidRow(
               shiny::column(8,
                      shiny::h6(style = "color:#2c3e50; font-weight:bold; margin-bottom:6px;", "Centroïdes des groupes"),
                      shiny::div(style = "overflow-x:auto;",
                          shiny::tags$table(style = "border-collapse:collapse; width:100%;",
                                     shiny::tags$thead(shiny::tags$tr(
                                       shiny::tags$th(style = "padding:5px 8px; background:#2c3e50; color:white; border:1px solid #dee2e6; font-size:10px;", "Groupe"),
                                       lapply(colnames(afd_result$means), function(cn)
                                         shiny::tags$th(style = "padding:5px 8px; background:#2c3e50; color:white; border:1px solid #dee2e6; font-size:10px; text-align:center;", cn)
                                       )
                                     )),
                                     shiny::tags$tbody(lapply(1:nrow(afd_result$means), function(i) {
                                       bg <- if (i %% 2 == 0) "#f8f9fa" else "white"
                                       shiny::tags$tr(style = paste0("background:", bg, ";"),
                                               shiny::tags$td(style = "padding:5px 8px; border:1px solid #dee2e6; font-weight:bold; font-size:10px;",
                                                       rownames(afd_result$means)[i]),
                                               lapply(afd_result$means[i, ], function(v)
                                                 shiny::tags$td(style = "padding:5px 8px; border:1px solid #dee2e6; text-align:center; font-size:10px;",
                                                         round(v, dec))
                                               )
                                       )
                                     }))
                          )
                      )
               ),
               shiny::column(4,
                      shiny::h6(style = "color:#2c3e50; font-weight:bold; margin-bottom:6px;", "Probabilités a priori"),
                      lapply(names(afd_result$prior), function(g) {
                        pct <- round(afd_result$prior[[g]] * 100, 1)
                        shiny::div(style = "margin-bottom:8px;",
                            shiny::div(style = "display:flex; justify-content:space-between; font-size:11px; margin-bottom:2px;",
                                shiny::tags$span(style = "font-weight:bold; color:#2c3e50;", g),
                                shiny::tags$span(style = "color:#555;", paste0(pct, "%"))
                            ),
                            shiny::div(style = "background:#dee2e6; border-radius:4px; height:8px; overflow:hidden;",
                                shiny::div(style = paste0(
                                  "background:#2c3e50; height:100%; width:", pct, "%; border-radius:4px;"
                                ))
                            )
                        )
                      })
               )
             )
        )
      )
      
    }, error = function(e) {
      shiny::div(style = "padding:12px; background:#f8d7da; border-radius:6px;",
          p(style = "margin:0; color:#721c24;",
            shiny::icon("exclamation-triangle"), " Erreur lors du rendu des métriques AFD : ", e$message))
    })
  })
  
  output$downloadAfdIndPlot <- shiny::downloadHandler(
    filename = function() paste0("afd_individus_", Sys.Date(), ".", hstat_img_fmt(input$afdInd_format)),
    content = function(file) {
      d <- mv_dims_export("afdInd", 9.8, 7.9); dpi <- d$dpi
      auto_dims <- d
      p <- withCallingHandlers(
        suppressMessages(mv_legacy(createAfdIndPlot(afdResultReactive()), "afdInd")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      hstat_ecrire_image(file, p, input$afdInd_format,
                         auto_dims$width, auto_dims$height, dpi)
    }
  )
  
  output$downloadAfdVarPlot <- shiny::downloadHandler(
    filename = function() paste0("afd_variables_", Sys.Date(), ".", hstat_img_fmt(input$afdVar_format)),
    content = function(file) {
      d <- mv_dims_export("afdVar", 9.8, 7.9); dpi <- d$dpi
      auto_dims <- d
      p <- withCallingHandlers(
        suppressMessages(mv_legacy(createAfdVarPlot(afdResultReactive()), "afdVar")),
        warning = function(w) {
          if (grepl("New names|name repair|^\\.\\.", conditionMessage(w))) invokeRestart("muffleWarning")
        }
      )
      hstat_ecrire_image(file, p, input$afdVar_format,
                         auto_dims$width, auto_dims$height, dpi)
    }
  )
  
  hstat_export_tables_handlers(output, "downloadAfdData", function() {
    dfs <- values$afdDataframes %||% afdDataframes()
    if (is.null(dfs) || is.null(dfs$ind_coords) || nrow(dfs$ind_coords) == 0)
      return(NULL)
    hstat_tables_non_vides(list(
      "Coordonnees_individus"      = dfs$ind_coords,
      "Coefficients_discriminants" = dfs$coefficients,
      "Matrice_structure"          = dfs$structure_matrix,
      "Tests_F"                    = dfs$f_tests,
      "Matrice_confusion"          = dfs$confusion_matrix,
      "Taux_classification"        = dfs$classification_rates,
      "Centroides"                 = dfs$centroids,
      "Variance_expliquee"         = dfs$variance_explained,
      "CV_confusion"               = dfs$cv_confusion,
      "CV_taux"                    = dfs$cv_accuracy))
  }, "afd_resultats", "AFD")
  # AFD - Selecteur de variables categorielles pour la prediction
  output$afdPredictVarsSelect <- shiny::renderUI({
    shiny::req(values$filteredData, input$afdFactor)
    if (is.null(values$filteredData) || is.null(input$afdFactor)) {
      return(NULL)
    }
    
    fac_cols <- get_categorical_cols(values$filteredData)
    
    fac_cols <- fac_cols[fac_cols != input$afdFactor]
    
    if (!is.null(input$afdMeansGroup) && !is.null(input$afdUseMeans) && input$afdUseMeans) {
      fac_cols <- fac_cols[fac_cols != input$afdMeansGroup]
    }
    
    if (length(fac_cols) == 0) {
      return(NULL)  # Ne pas afficher de message car le panneau d'info est déjà présent
    }
    
    pickerInput(
      inputId = "afdPredictVars",
      label = "Variables catégorielles pour la prédiction (optionnel) :",
      choices = fac_cols,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  # ---- Analyses multivariees etendues (quanti / quali / mixtes) ----

  # L'INSTALLATION NE SE FAIT PAS DANS LE SERVEUR. Le bloc qui vivait ici
  # tentait d'installer six paquets a CHAQUE session : hors ligne, chaque
  # ouverture de l'application attendait l'expiration de la requete CRAN ; sur
  # un serveur partage, la bibliotheque est le plus souvent en lecture seule, et
  # deux sessions simultanees pouvaient y ecrire ensemble. Ces paquets sont
  # desormais dans `hstat_model_packages` (Utils.R), installe une fois au
  # demarrage.
  mv_has <- function(p) isTRUE(requireNamespace(p, quietly = TRUE))

  mv_res <- shiny::reactiveValues()

  mv_data <- shiny::reactive(values$filteredData)
  mv_num_cols <- shiny::reactive({
    d <- mv_data(); if (is.null(d)) return(character(0))
    names(d)[sapply(d, is.numeric)]
  })
  mv_cat_cols <- shiny::reactive({
    d <- mv_data(); if (is.null(d)) return(character(0))
    names(d)[sapply(d, function(x) is.factor(x) || is.character(x) || is.logical(x))]
  })
  mv_subsample <- function(df, cap = 5000) {
    if (nrow(df) <= cap) return(df)
    df[sort(sample(seq_len(nrow(df)), cap)), , drop = FALSE]
  }
  # Convertit une saisie "1, 5, 12" en indices de lignes valides (dans 1..n).
  mv_parse_ind_sup <- function(txt, n) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(integer(0))
    v <- suppressWarnings(as.integer(trimws(strsplit(txt, "[,;[:space:]]+")[[1]])))
    v <- v[!is.na(v) & v >= 1 & v <= n]
    unique(v)
  }
  # Traduit le choix "Colorer par" en valeur attendue par les fonctions fviz_*.
  mv_color_value <- function(choice) {
    switch(choice %||% "contrib",
      "contrib" = "contrib", "cos2" = "cos2", "coord" = "coord", "contrib")
  }
  # Dispatcher de visualisation factorielle (Variables / Individus / Biplot) avec
  # coloration, communs a ACM / AFDM / AFM. 'kind' = "mca" | "famd" | "mfa".
  # ptsz = taille des individus, arrsz = epaisseur des fleches de variables,
  # lblsz / lblszvar = taille des labels (unite ggplot2) des INDIVIDUS et des
  # VARIABLES / modalites, show_lab = afficher les labels des individus.
  mv_factor_viz <- function(model, kind, plottype, colorby, title, subtitle,
                            ptsz = 2, arrsz = 0.6, lblsz = 4, lblszvar = NULL,
                            show_lab = FALSE,
                            axes = c(1, 2), group_values = NULL) {
    if (!requireNamespace("factoextra", quietly = TRUE)) return(NULL)
    axes <- as.integer(axes); if (length(axes) != 2 || any(is.na(axes))) axes <- c(1L, 2L)
    if (is.null(lblszvar)) lblszvar <- lblsz
    # Effectifs servant a distinguer, apres coup, les calques de texte des
    # variables de ceux des individus sur un biplot. Chaque methode range ses
    # coordonnees de variables ailleurs (modalites en ACM, quanti/quali en
    # AFDM, groupes en AFM) : on collecte tous les effectifs candidats.
    n_ind_m <- tryCatch(nrow(model$ind$coord), error = function(e) NA_integer_)
    n_var_m <- unlist(lapply(
      list(model$var$coord, model$quanti.var$coord, model$quali.var$coord,
           model$group$coord),
      function(z) tryCatch(nrow(z), error = function(e) NULL)))
    if (is.null(n_var_m)) n_var_m <- NA_integer_
    cv <- mv_color_value(colorby)
    gc <- c("#00AFBB", "#E7B800", "#FC4E07")
    th <- tryCatch(mv_gg_theme(), error = function(e) ggplot2::theme_minimal())
    ind_lab <- if (isTRUE(show_lab)) "all" else "none"
    # Coloration par groupe (variable qualitative) facon 'explor' : si un vecteur
    # de groupes valide est fourni (1 valeur par individu actif), on colore les
    # individus par ce groupe (habillage) au lieu de la metrique contrib/cos2.
    grp <- NULL
    if (!is.null(group_values)) {
      n_ind <- tryCatch(nrow(model$ind$coord), error = function(e) NA)
      gv <- as.character(group_values)
      if (!is.na(n_ind) && length(gv) == n_ind &&
          length(unique(stats::na.omit(gv))) >= 2) {
        grp <- factor(gv)
      }
    }
    # Quand on colore par groupe, on remplace la valeur de coloration des individus
    # par le facteur de groupe (factoextra colore par modalite) et on active des
    # ellipses de concentration. Les graphiques de variables gardent la metrique.
    cv_ind <- if (!is.null(grp)) grp else cv
    # Ellipses automatiques par groupe uniquement sur le trace des individus
    # (le biplot melange individus et variables : addEllipses y est moins fiable).
    # Meme controle que pour l'ACP : sans lui, un groupe de moins de trois
    # individus -- ou aux coordonnees constantes, ou parfaitement alignees --
    # faisait echouer stat_conf_ellipse() et le graphique perdait sa couche
    # d'ellipses sans que rien n'en dise la raison.
    # hstat_coord_mat : FactoMineR rend un VECTEUR nu des qu'il n'y a qu'un axe,
    # et `coord[, i]` echoue alors sur « incorrect number of dimensions ».
    ell <- if (is.null(grp)) list(ok = FALSE, motif = NULL) else
      tryCatch({
        cm <- hstat_coord_mat(model$ind$coord)
        hstat_ellipse_ok(grp, cm[, min(axes[1], ncol(cm))], cm[, min(axes[2], ncol(cm))])
      }, error = function(e) list(ok = FALSE, motif = NULL))
    if (!isTRUE(ell$ok) && !is.null(ell$motif))
      shiny::showNotification(ell$motif, type = "warning", duration = 8,
                       id = "mvEllipseImpossible")
    use_ellipse <- isTRUE(ell$ok) && identical(plottype, "ind")
    p <- tryCatch({
      if (kind == "mca") {
        switch(plottype,
          "var"    = factoextra::fviz_mca_var(model, axes = axes, col.var = cv, gradient.cols = gc,
                       repel = TRUE, max.overlaps = Inf, labelsize = lblszvar,
                       pointsize = ptsz, ggtheme = th),
          "ind"    = factoextra::fviz_mca_ind(model, axes = axes, col.ind = cv_ind, gradient.cols = gc, addEllipses = use_ellipse, ellipse.type = "confidence",
                       repel = TRUE, max.overlaps = Inf, pointsize = ptsz, labelsize = lblsz,
                       label = ind_lab, ggtheme = th),
          # Biplot : individus colores + modalites en NOIR
          factoextra::fviz_mca_biplot(model, axes = axes, col.var = "black", col.ind = cv_ind, addEllipses = use_ellipse,
                       gradient.cols = gc, repel = TRUE, max.overlaps = Inf, pointsize = ptsz,
                       labelsize = lblsz,
                       label = if (isTRUE(show_lab)) "all" else "var",
                       arrowsize = arrsz, ggtheme = th))
      } else if (kind == "famd") {
        switch(plottype,
          "var"    = factoextra::fviz_famd_var(model, axes = axes, col.var = cv, gradient.cols = gc,
                       repel = TRUE, max.overlaps = Inf, labelsize = lblszvar,
                       pointsize = ptsz, ggtheme = th),
          "ind"    = factoextra::fviz_famd_ind(model, axes = axes, col.ind = cv_ind, gradient.cols = gc, addEllipses = use_ellipse, ellipse.type = "confidence",
                       repel = TRUE, max.overlaps = Inf, pointsize = ptsz, labelsize = lblsz,
                       label = ind_lab, ggtheme = th),
          # Biplot AFDM : factoextra n'a pas de fviz_famd_biplot. On superpose les
          # individus colores et les variables quantitatives (fleches noires).
          {
            pind <- factoextra::fviz_famd_ind(model, axes = axes, col.ind = cv_ind, gradient.cols = gc, addEllipses = use_ellipse, ellipse.type = "confidence",
                       repel = TRUE, max.overlaps = Inf, pointsize = ptsz, labelsize = lblsz,
                       label = ind_lab, ggtheme = th)
            qv <- tryCatch(as.data.frame(hstat_coord_mat(model$quanti.var$coord)),
                           error = function(e) NULL)
            # Le biplot superpose deux plans : il exige DEUX axes de chaque cote.
            # Un modele reduit a une dimension n'en fournit qu'un, et
            # `coord[, 2]` echouait ; on se rabat alors sur les seuls individus.
            ind_co <- hstat_coord_mat(model$ind$coord)
            if (!is.null(qv) && ncol(qv) >= 2 && !is.null(ind_co) && ncol(ind_co) >= 2) {
              sc <- 0.9 * max(abs(range(c(ind_co[, 1], ind_co[, 2]))), na.rm = TRUE) /
                    max(abs(range(c(qv[,1], qv[,2]))), na.rm = TRUE)
              qv2 <- data.frame(x = qv[,1]*sc, y = qv[,2]*sc, lab = rownames(qv))
              pind <- pind +
                ggplot2::geom_segment(data = qv2,
                  ggplot2::aes(x = 0, y = 0, xend = x, yend = y), inherit.aes = FALSE,
                  arrow = ggplot2::arrow(length = ggplot2::unit(0.02 * (1 + arrsz), "npc")),
                  color = "black", linewidth = arrsz) +
                ggplot2::geom_text(data = qv2, ggplot2::aes(x = x, y = y, label = lab),
                  inherit.aes = FALSE, color = "black", size = lblszvar, fontface = "bold",
                  vjust = -0.4)
            }
            # Modalités des variables QUALITATIVES (points + labels) en violet,
            # pour les afficher sur le plan AFDM (demande utilisateur).
            ql <- tryCatch(as.data.frame(model$quali.var$coord), error = function(e) NULL)
            if (!is.null(ql) && ncol(ql) >= 2 && nrow(ql) > 0) {
              ql2 <- data.frame(x = ql[, axes[1]], y = ql[, axes[2]], lab = rownames(ql))
              pind <- pind +
                ggplot2::geom_point(data = ql2, ggplot2::aes(x = x, y = y),
                  inherit.aes = FALSE, color = "#7b3fa0", shape = 17,
                  size = ptsz + 0.6) +
                ggplot2::geom_text(data = ql2, ggplot2::aes(x = x, y = y, label = lab),
                  inherit.aes = FALSE, color = "#7b3fa0", size = lblszvar, fontface = "bold",
                  vjust = 1.4)
            }
            pind
          })
      } else {
        switch(plottype,
          "var"    = factoextra::fviz_mfa_var(model, axes = axes, "group", repel = TRUE, max.overlaps = Inf,
                       labelsize = lblszvar, pointsize = ptsz, ggtheme = th),
          "ind"    = factoextra::fviz_mfa_ind(model, axes = axes, col.ind = cv_ind, gradient.cols = gc, addEllipses = use_ellipse, ellipse.type = "confidence",
                       repel = TRUE, max.overlaps = Inf, pointsize = ptsz, labelsize = lblsz,
                       label = ind_lab, ggtheme = th),
          # Biplot AFM : factoextra N'A PAS de `fviz_mfa_biplot` -- pas plus
          # que de `fviz_famd_biplot`, traite juste au-dessus. L'appel etait
          # enferme dans un `tryCatch(error = NULL)` : choisir « biplot » sur
          # une AFM rendait un graphique VIDE, sans un message. Le defaut
          # n'existait qu'a l'ecran, jamais a la lecture ni a l'execution des
          # tests ; c'est `R CMD check` qui l'a nomme.
          #
          # On le compose donc comme le biplot AFDM : individus colores, puis
          # variables quantitatives en fleches noires.
          {
            pind <- factoextra::fviz_mfa_ind(model, axes = axes, col.ind = cv_ind, gradient.cols = gc,
                       addEllipses = use_ellipse, ellipse.type = "confidence",
                       repel = TRUE, max.overlaps = Inf, pointsize = ptsz, labelsize = lblsz,
                       label = ind_lab, ggtheme = th)
            qv <- tryCatch(as.data.frame(hstat_coord_mat(model$quanti.var$coord)),
                           error = function(e) NULL)
            # Le biplot superpose deux plans : il exige DEUX axes de chaque cote.
            ind_co <- hstat_coord_mat(model$ind$coord)
            if (!is.null(qv) && ncol(qv) >= 2 && !is.null(ind_co) && ncol(ind_co) >= 2) {
              sc <- 0.9 * max(abs(range(c(ind_co[, 1], ind_co[, 2]))), na.rm = TRUE) /
                    max(abs(range(c(qv[, 1], qv[, 2]))), na.rm = TRUE)
              qv2 <- data.frame(x = qv[, 1] * sc, y = qv[, 2] * sc, lab = rownames(qv))
              pind <- pind +
                ggplot2::geom_segment(data = qv2,
                  ggplot2::aes(x = 0, y = 0, xend = x, yend = y), inherit.aes = FALSE,
                  arrow = ggplot2::arrow(length = ggplot2::unit(0.02 * (1 + arrsz), "npc")),
                  color = "black", linewidth = arrsz) +
                ggplot2::geom_text(data = qv2, ggplot2::aes(x = x, y = y, label = lab),
                  inherit.aes = FALSE, color = "black", size = lblszvar, fontface = "bold",
                  vjust = -0.4)
            }
            # Modalites des variables qualitatives, quand le plan en comporte.
            ql <- tryCatch(as.data.frame(model$quali.var$coord), error = function(e) NULL)
            if (!is.null(ql) && ncol(ql) >= 2 && nrow(ql) > 0) {
              ql2 <- data.frame(x = ql[, axes[1]], y = ql[, axes[2]], lab = rownames(ql))
              pind <- pind +
                ggplot2::geom_point(data = ql2, ggplot2::aes(x = x, y = y),
                  inherit.aes = FALSE, color = "#7b3fa0", shape = 17,
                  size = ptsz + 0.6) +
                ggplot2::geom_text(data = ql2, ggplot2::aes(x = x, y = y, label = lab),
                  inherit.aes = FALSE, color = "#7b3fa0", size = lblszvar, fontface = "bold",
                  vjust = 1.4)
            }
            pind
          })
      }
    }, error = function(e) NULL)
    if (is.null(p)) return(NULL)
    p <- p + ggplot2::labs(title = title, subtitle = subtitle)
    # Labels lisibles (noir/gras) plutot que la couleur claire du degrade.
    p <- .mv_darken_text_labels(p)
    # factoextra n'expose qu'un seul `labelsize` : sur un graphique melangeant
    # individus et variables (biplot), on retaille apres coup les calques de
    # texte des variables pour honorer les deux reglages en points.
    p <- if (identical(plottype, "var"))
      hstat_apply_label_sizes(p, lblszvar)
    else
      hstat_apply_label_sizes(p, lblsz, lblszvar, n_var_m, n_ind_m)
    # Centrage : on rend les axes symetriques autour de l'origine (0,0) pour que
    # le nuage ne soit pas tasse d'un cote. On lit les bornes effectives du plot
    # (individus + fleches de variables) et on impose des limites symetriques.
    p <- tryCatch({
      bld <- ggplot2::ggplot_build(p)
      xr <- bld$layout$panel_params[[1]]$x.range
      yr <- bld$layout$panel_params[[1]]$y.range
      if (!is.null(xr) && !is.null(yr) && all(is.finite(c(xr, yr)))) {
        xm <- max(abs(xr)); ym <- max(abs(yr))
        p + ggplot2::expand_limits(x = c(-xm, xm), y = c(-ym, ym))
      } else p
    }, error = function(e) p)
    p
  }

  # Superpose des ellipses de concentration autour des individus d'une analyse
  # factorielle (ACM / AFDM). 'prefix' = prefixe des inputs (ex. "mv_mca").
  # 'group_values' = vecteur (longueur = nb individus actifs) servant a grouper
  # les ellipses ; si NULL ou "__none__", une seule ellipse englobe tout.
  mv_add_ind_ellipses <- function(p, model, prefix, group_values = NULL) {
    if (is.null(p)) return(p)
    if (!isTRUE(input[[paste0(prefix, "_ellipse")]])) return(p)
    coord <- tryCatch(as.data.frame(
                        hstat_coord_mat(model$ind$coord)[
                          , 1:min(2, ncol(hstat_coord_mat(model$ind$coord))), drop = FALSE]),
                      error = function(e) NULL)
    if (is.null(coord) || ncol(coord) < 2 || nrow(coord) < 3) return(p)
    names(coord)[1:2] <- c("Dim1", "Dim2")
    lvl <- input[[paste0(prefix, "_ellipselevel")]] %||% 0.95
    if (!is.null(group_values) && length(group_values) == nrow(coord) &&
        length(unique(stats::na.omit(group_values))) >= 2) {
      coord$.grp <- factor(group_values)
      coord <- coord[!is.na(coord$.grp), , drop = FALSE]
      # Les groupes qui ne peuvent pas porter d'ellipse sont ecartes du CALQUE
      # plutot que de faire echouer le calcul pour tous les autres.
      ell <- hstat_ellipse_ok(coord$.grp, coord$Dim1, coord$Dim2)
      if (!length(ell$groupes)) {
        if (!is.null(ell$motif))
          shiny::showNotification(ell$motif, type = "warning", duration = 8,
                           id = paste0(prefix, "_ellipse_ko"))
        return(p)
      }
      if (length(ell$faibles))
        shiny::showNotification(ell$motif, type = "warning", duration = 8,
                         id = paste0(prefix, "_ellipse_partiel"))
      coord <- coord[as.character(coord$.grp) %in% ell$groupes, , drop = FALSE]
      coord$.grp <- droplevels(coord$.grp)
      p <- p + ggplot2::stat_ellipse(data = coord,
                 ggplot2::aes(x = Dim1, y = Dim2, group = .grp, color = .grp),
                 inherit.aes = FALSE, type = "norm", level = lvl,
                 linewidth = 0.8, show.legend = FALSE)
    } else {
      p <- p + ggplot2::stat_ellipse(data = coord,
                 ggplot2::aes(x = Dim1, y = Dim2),
                 inherit.aes = FALSE, type = "norm", level = lvl,
                 linewidth = 0.8, color = "#7b3fa0")
    }
    p
  }
  # Boite "Options d'affichage des graphiques (optionnel)" COMPLETE et autonome,
  # repliable, presente dans CHAQUE methode factorielle (ACM / AFDM / AFM).
  # Chaque reglage a sa valeur par defaut alignee sur les reglages globaux, mais
  # reste pilotable localement par methode.
  mv_viz_options_ui <- function(prefix) {
    shinydashboard::box(title = shiny::tagList(shiny::icon("sliders-h"), " Options d'affichage des graphiques (optionnel)"),
        status = "primary", width = 12, solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
      shiny::selectInput(paste0(prefix, "_labelsource"),
        shiny::tagList(shiny::icon("tag"), " Source des labels pour individus (optionnel) :"),
        choices = c("Numéro de ligne" = "rownames", stats::setNames(names(mv_data()), names(mv_data())))),
      shiny::radioButtons(paste0(prefix, "_plottype"), shiny::tagList(shiny::icon("eye"), " Type de visualisation :"),
        choices = c("Variables" = "var", "Individus" = "ind", "Biplot" = "biplot"),
        selected = "biplot", inline = TRUE),
      shiny::div(style = "background:#eef7fb; border-left:3px solid #1b9fd0; padding:8px 12px; margin:6px 0; border-radius:0 4px 4px 0;",
        shiny::tags$label(style = "color:#2c3e50;font-size:13px;font-weight:700;",
                   shiny::icon("arrows-up-down-left-right"), " Axes factoriels à explorer :"),
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(paste0(prefix, "_axisx"), "Axe horizontal (Dim)",
                                choices = 1:8, selected = 1)),
          shiny::column(6, shiny::selectInput(paste0(prefix, "_axisy"), "Axe vertical (Dim)",
                                choices = 1:8, selected = 2))),
        shiny::tags$small(style = "color:#6b7280;", shiny::icon("info-circle"),
          " Changez les axes pour explorer les plans factoriels au-delà du plan 1-2 (survol interactif des points).")),
      shiny::div(style = "background:#f0f7ff; border-left:3px solid #2196f3; padding:8px 12px; margin:6px 0; border-radius:0 4px 4px 0;",
        shiny::selectInput(paste0(prefix, "_colorby"),
          shiny::tagList(shiny::icon("palette"), " Colorer les éléments par :"),
          choices = c("Contribution (% à l'axe)" = "contrib",
                      "Cos² (qualité de représentation)" = "cos2",
                      "Coordonnées" = "coord"),
          selected = "contrib")),
      shiny::div(style = "background:#eafaf1; border-left:3px solid #16a085; padding:8px 12px; margin:6px 0; border-radius:0 4px 4px 0;",
        shiny::selectInput(paste0(prefix, "_groupvar"),
          shiny::tagList(shiny::icon("users"), " Colorer les individus par groupe (variable qualitative) :"),
          choices = c("Aucun (colorer par la métrique ci-dessus)" = "__none__",
                      stats::setNames(mv_cat_cols(), mv_cat_cols())),
          selected = "__none__"),
        shiny::tags$small(style = "color:#6b7280;", shiny::icon("info-circle"),
          " Au choix d'un groupe, les individus sont colorés par modalité et entourés d'ellipses de concentration (exploration façon « explor »).")),
      shiny::div(style = "background:#f4f6f8;border:1px solid #e3e8ec;border-radius:6px;padding:10px 14px;margin:6px 0;",
        shiny::tags$label(style = "color:#2c3e50;font-size:13px;font-weight:700;",
                   shiny::icon("palette"), " Palette de couleurs :"),
        shiny::selectInput(paste0(prefix, "_palette"), label = NULL,
          choices = c("Par défaut (ggplot2)" = "default",
                      "Set1" = "Set1", "Set2" = "Set2", "Dark2" = "Dark2",
                      "Paired" = "Paired", "Viridis" = "viridis", "Plasma" = "plasma"),
          selected = "default"),
        shiny::fluidRow(
          shiny::column(6, shiny::sliderInput(paste0(prefix, "_ptsz"), "Taille des points",
                                min = 0.5, max = 8, value = HSTAT_GG_POINT_SIZE, step = 0.5)),
          shiny::column(6, shiny::sliderInput(paste0(prefix, "_arrsz"), "Épaisseur des tracés",
                                min = 0.3, max = 4, value = HSTAT_GG_LINEWIDTH, step = 0.1))),
        shiny::fluidRow(
          shiny::column(6, hstat_lbl_slider(paste0(prefix, "_lblsz"),
                                     "Taille des labels des individus")),
          shiny::column(6, hstat_lbl_slider(paste0(prefix, "_lblszvar"),
                                     "Taille des labels des variables"))),
        shiny::tags$small(style = "color:#6b7280;", shiny::icon("info-circle"),
          paste(" Tailles exprimées en points (8 à 24 pt). Le second curseur",
                "s'applique aux noms de variables, modalités ou colonnes",
                "affichés sur le graphique.")),
        shiny::checkboxInput(paste0(prefix, "_showlab"),
          shiny::tagList(shiny::icon("font"), " Afficher les labels des individus / variables"), value = FALSE)),
      shiny::div(style = "background:#f4f0ff; border-left:3px solid #7b3fa0; padding:8px 12px; margin:6px 0; border-radius:0 4px 4px 0;",
        shiny::checkboxInput(paste0(prefix, "_ellipse"),
          shiny::tagList(shiny::icon("draw-polygon"), " Entourer les individus d'ellipses de concentration"),
          value = FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s_ellipse'] == true", prefix),
          shiny::selectInput(paste0(prefix, "_ellipsevar"),
            shiny::tagList(shiny::icon("layer-group"), " Grouper les ellipses par (variable qualitative) :"),
            choices = c("Aucune (toutes les obs.)" = "__none__",
                        stats::setNames(mv_cat_cols(), mv_cat_cols()))),
          shiny::sliderInput(paste0(prefix, "_ellipselevel"), "Niveau de l'ellipse",
                      min = 0.5, max = 0.99, value = 0.95, step = 0.01))),
      mv_forme_box(prefix))
  }

  # Boite "Options d'affichage des graphiques (optionnel)" GENERIQUE pour les
  # methodes non factorielles (k-means, AFE, regressions, LCA, k-modes, etc.).
  # Contient palette + tailles + texte gras + labels + export. Les inputs sont
  # locaux a la methode (prefixe) avec repli sur les reglages globaux.
  # Bloc commun aux boites d'options : ce qui manquait dans TOUTES les analyses
  # multivariees -- titre, sous-titre, libelles d'axes, theme, position de la
  # legende, grille -- et la taille d'export, dont les champs se recalculent au
  # changement de resolution.
  mv_forme_box <- function(prefix, base_w_in = 10, base_h_in = 7.5) {
    shiny::tagList(
      .hstat_opt_section(
        "Titres et axes", "heading", "#2980b9", "#eaf3fa",
        shiny::textInput(paste0(prefix, "_title"), "Titre", placeholder = "Titre par défaut"),
        shiny::textInput(paste0(prefix, "_subtitle"), "Sous-titre", placeholder = "Optionnel"),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(paste0(prefix, "_xlab"), "Libellé X", placeholder = "Auto")),
          shiny::column(6, shiny::textInput(paste0(prefix, "_ylab"), "Libellé Y", placeholder = "Auto"))),
        shiny::tags$small(style = "color:#6b7280;",
                   "Laisser vide conserve les libellés calculés par l'analyse (axes, % de variance).")),
      .hstat_opt_section(
        "Thème, légende et grille", "brush", "#8e44ad", "#f7f0fb",
        shiny::selectInput(paste0(prefix, "_theme"), "Thème",
                    choices = c("Par défaut (ggplot2)" = "gg",
                                "HStat (titre centré, légende en bas)" = "hstat",
                                HSTAT_THEMES_GG),
                    selected = "gg"),
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(paste0(prefix, "_legendpos"), "Légende",
                                choices = c("Par défaut (ggplot2)" = "gg",
                                            "À droite" = "right", "À gauche" = "left",
                                            "En haut" = "top", "En bas" = "bottom",
                                            "Masquée" = "none"),
                                selected = "gg")),
          shiny::column(6, shiny::selectInput(paste0(prefix, "_grid"), "Grille",
                                choices = c("Par défaut (ggplot2)" = "gg",
                                            "Principale seule" = "majeure",
                                            "Aucune" = "sans"),
                                selected = "gg")))),
      .hstat_opt_section(
        "Taille du fichier exporté", "download", "#2c3e50", "#eef1f4",
        shiny::fluidRow(
          shiny::column(4, hstat_dpi_input(paste0(prefix, "_dpi"), shiny::tagList(shiny::icon("image"), " DPI"))),
          shiny::column(4, shiny::numericInput(paste0(prefix, "_width"), "Largeur (px)",
                                 value = round(base_w_in * 300), min = 200, max = 20000, step = 50)),
          shiny::column(4, shiny::numericInput(paste0(prefix, "_height"), "Hauteur (px)",
                                 value = round(base_h_in * 300), min = 200, max = 20000, step = 50))),
        hstat_format_input(paste0(prefix, "_fmt"),
                          shiny::tagList(shiny::icon("file-image"), " Format")),
        shiny::tags$small(style = "color:#6b7280;",
                   "La largeur et la hauteur se recalculent quand la résolution change : la taille physique de la figure reste la même, sa finesse augmente."),
        hstat_mv_dim_note_ui(prefix),
        shiny::div(style = "text-align:center; margin-top:8px;",
            shiny::downloadButton(paste0(prefix, "_download"),
                           shiny::tagList(shiny::icon("download"), " Télécharger le graphique"),
                           class = "btn-success"))))
  }

  mv_disp_box <- function(prefix) {
    shinydashboard::box(title = shiny::tagList(shiny::icon("sliders-h"), " Options d'affichage des graphiques (optionnel)"),
        status = "primary", width = 12, solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
      shiny::div(style = "background:#f4f6f8;border:1px solid #e3e8ec;border-radius:6px;padding:10px 14px;",
        shiny::tags$label(style = "color:#2c3e50;font-size:13px;font-weight:700;",
                   shiny::icon("palette"), " Palette de couleurs :"),
        shiny::selectInput(paste0(prefix, "_palette"), label = NULL,
          choices = c("Par défaut (ggplot2)" = "default",
                      "Set1" = "Set1", "Set2" = "Set2", "Dark2" = "Dark2",
                      "Paired" = "Paired", "Viridis" = "viridis", "Plasma" = "plasma"),
          selected = "default"),
        shiny::fluidRow(
          shiny::column(6, shiny::sliderInput(paste0(prefix, "_ptsz"), "Taille des points",
                                min = 0.5, max = 8, value = HSTAT_GG_POINT_SIZE, step = 0.5)),
          shiny::column(6, shiny::sliderInput(paste0(prefix, "_arrsz"), "Épaisseur des tracés",
                                min = 0.3, max = 4, value = HSTAT_GG_LINEWIDTH, step = 0.1))),
        shiny::fluidRow(
          shiny::column(6, hstat_lbl_slider(paste0(prefix, "_lblsz"),
                                     "Taille des labels des individus")),
          shiny::column(6, hstat_lbl_slider(paste0(prefix, "_lblszvar"),
                                     "Taille des labels des variables"))),
        shiny::tags$small(style = "color:#6b7280;", shiny::icon("info-circle"),
          paste(" Tailles exprimées en points (8 à 24 pt). Le second curseur",
                "s'applique aux noms de variables, modalités ou colonnes",
                "affichés sur le graphique.")),
        shiny::fluidRow(
          shiny::column(6, shiny::sliderInput(paste0(prefix, "_textsz"), "Taille texte axes",
                                min = 8, max = 22, value = HSTAT_GG_BASE_SIZE, step = 1)),
          shiny::column(6, shiny::div(style = "margin-top:24px;",
            shiny::checkboxInput(paste0(prefix, "_bold"), "Texte en gras", value = FALSE)))),
        shiny::checkboxInput(paste0(prefix, "_showlab"),
          shiny::tagList(shiny::icon("font"), " Afficher les labels"), value = FALSE)),
      mv_forme_box(prefix))
  }

  mv_col <- function(st) switch(st,
    ok = "#27ae60", warn = "#f39c12", err = "#e74c3c", info = "#2980b9", "#7f8c8d")
  mv_ic <- function(st) switch(st,
    ok = "check-circle", warn = "exclamation-triangle", err = "times-circle",
    info = "info-circle", "circle")

  mv_card <- function(..., border_color = "#dee2e6", bg = "white") {
    shiny::div(style = paste0("background:", bg, "; border-radius:8px; padding:16px; margin-bottom:14px;",
                       " border:1px solid ", border_color, "; box-shadow:0 1px 4px rgba(0,0,0,.06);"),
        ...)
  }
  mv_section_header <- function(label, color = "#3a6186", icon_name = "chart-bar") {
    shiny::div(style = paste0("background:linear-gradient(135deg,", color, " 0%,", color,
                       "cc 100%); border-radius:6px; padding:11px 16px; margin-bottom:12px;"),
        shiny::h5(style = "color:white; margin:0; font-weight:bold; font-size:15px;",
           shiny::icon(icon_name), paste0("  ", label)))
  }
  mv_info_note <- function(text) {
    p(style = "font-size:13px; color:#5a6a7a; font-style:italic; margin:6px 0 10px 0;", text)
  }
  mv_interp_bar <- function(text, color) {
    shiny::div(style = paste0("margin-top:8px; padding:8px 14px; border-left:4px solid ", color,
                       "; background:#f4f6f8; border-radius:0 4px 4px 0;"),
        p(style = "margin:0; font-size:13px; color:#2c3e50; font-weight:500;",
          shiny::icon("info-circle"), " ", text))
  }

  # -- Tableau de metriques structure (Metrique / Valeur / Seuil / Interpretation) --
  # df : Metrique, Valeur, Seuil, Interpretation, Statut
  mv_metrics_table <- function(df, accent = "#3a6186") {
    if (is.null(df) || nrow(df) == 0)
      return(p(style = "color:#888; font-style:italic;",
               shiny::icon("info-circle"), " Aucune métrique disponible."))
    rows <- lapply(seq_len(nrow(df)), function(i) {
      st <- df$Statut[i]; bg <- if (i %% 2 == 0) "#f4f6f8" else "white"
      shiny::tags$tr(style = paste0("background:", bg, ";"),
        shiny::tags$td(style = "padding:8px 10px; font-weight:bold; font-size:12px; color:#2c3e50; border-bottom:1px solid #ecf0f1;",
                df$Metrique[i]),
        shiny::tags$td(style = "padding:8px 10px; font-family:monospace; font-size:13px; text-align:center; border-bottom:1px solid #ecf0f1;",
                df$Valeur[i]),
        shiny::tags$td(style = "padding:8px 10px; font-size:11px; color:#666; border-bottom:1px solid #ecf0f1;",
                df$Seuil[i]),
        shiny::tags$td(style = paste0("padding:8px 10px; font-size:12px; font-weight:bold; border-bottom:1px solid #ecf0f1; color:",
                               mv_col(st), ";"),
                shiny::icon(mv_ic(st)), " ", df$Interpretation[i]))
    })
    shiny::tags$table(style = "width:100%; border-collapse:collapse; margin-top:6px;",
      shiny::tags$thead(shiny::tags$tr(style = paste0("background:", accent, "; color:white;"),
        shiny::tags$th(style = "padding:8px 10px; text-align:left; font-size:11px; border-radius:4px 0 0 0;", "Métrique"),
        shiny::tags$th(style = "padding:8px 10px; text-align:center; font-size:11px;", "Valeur"),
        shiny::tags$th(style = "padding:8px 10px; text-align:left; font-size:11px;", "Seuil de référence"),
        shiny::tags$th(style = "padding:8px 10px; text-align:left; font-size:11px; border-radius:0 4px 0 0;", "Interprétation"))),
      shiny::tags$tbody(rows))
  }

  # -- Tableau generique (data.frame -> table HTML compacte) --
  mv_data_table <- function(df, accent = "#3a6186", digits = 3) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    nums <- sapply(df, is.numeric)
    df2 <- df
    df2[nums] <- lapply(df2[nums], function(x) round(x, digits))
    rows <- lapply(seq_len(nrow(df2)), function(i) {
      bg <- if (i %% 2 == 0) "#f4f6f8" else "white"
      shiny::tags$tr(style = paste0("background:", bg, ";"),
        lapply(seq_len(ncol(df2)), function(j)
          shiny::tags$td(style = "padding:6px 10px; font-size:12px; text-align:center; border-bottom:1px solid #ecf0f1;",
                  as.character(df2[i, j]))))
    })
    shiny::tags$table(style = "width:100%; border-collapse:collapse; margin-top:6px;",
      shiny::tags$thead(shiny::tags$tr(style = paste0("background:", accent, "; color:white;"),
        lapply(names(df2), function(nm)
          shiny::tags$th(style = "padding:7px 10px; text-align:center; font-size:11px;", nm)))),
      shiny::tags$tbody(rows))
  }

  mv_status_box <- function(type, text) {
    col <- mv_col(if (type == "err") "err" else if (type == "warn") "warn" else "info")
    bg  <- if (type == "err") "#fdf0ef" else if (type == "warn") "#fef9ec" else "#eaf2f8"
    shiny::div(style = paste0("border-left:4px solid ", col, "; background:", bg,
                       "; padding:10px 14px; margin:8px 0; border-radius:0 4px 4px 0;"),
        p(style = paste0("margin:0; color:", col, "; font-size:13px; font-weight:bold;"),
          shiny::icon(mv_ic(if (type == "err") "err" else if (type == "warn") "warn" else "info")),
          " ", text))
  }

  .mv_badge <- function(st, label) {
    shiny::div(style = paste0("display:inline-block; background:", mv_col(st),
                       "; color:white; border-radius:4px; padding:2px 8px;",
                       " font-size:11px; margin:2px;"),
        shiny::icon(mv_ic(st)), " ", label)
  }
  .mv_cond_render <- function(title, badges, msgs, level) {
    border <- mv_col(level); bg <- switch(level, ok = "#eafaf1", warn = "#fef9ec", "#fdf0ef")
    shiny::tagList(
      shiny::hr(style = "margin:8px 0;"),
      shiny::div(style = paste0("border:2px solid ", border,
                         "; border-radius:6px; padding:8px 12px; background:", bg, ";"),
        shiny::div(style = "margin-bottom:6px;",
          shiny::tags$b(style = "font-size:12px; color:#2c3e50;", shiny::icon("clipboard-check"), " ", title),
          shiny::tags$br(), badges),
        if (length(msgs) > 0)
          shiny::tagList(lapply(msgs, function(m)
            p(style = "margin:3px 0; font-size:11px; color:#555;", m)),
            if (level == "err")
              shiny::div(style = "margin-top:6px; padding:5px 10px; background:rgba(231,76,60,0.1); border-radius:4px;",
                  p(style = "margin:0; font-size:11px; color:#c0392b; font-weight:bold;",
                    shiny::icon("exclamation-triangle"),
                    " Conditions non remplies -- résultats a interpreter avec prudence."))))
    )
  }

  mv_row <- function(metrique, valeur, seuil, interpretation, statut)
    data.frame(Metrique = metrique, Valeur = as.character(valeur),
               Seuil = seuil, Interpretation = interpretation,
               Statut = statut, stringsAsFactors = FALSE)

  # Prefixe de la methode dont le graphique est en cours de rendu. IMPORTANT :
  # ce N'EST PAS un reactiveVal (sinon lire+ecrire dans un renderPlot creerait une
  # boucle d'invalidation infinie -> spinner bloque). On utilise une variable
  # d'environnement simple, non reactive, posee juste avant l'appel a plotfn().
  .mv_prefix_env <- new.env(parent = emptyenv())
  .mv_prefix_env$cur <- NULL
  mv_active_prefix <- function(x) {
    if (missing(x)) return(.mv_prefix_env$cur)
    .mv_prefix_env$cur <- x
    invisible(x)
  }
  .mv_loc <- function(suffix, global) {
    pfx <- mv_active_prefix()
    if (!is.null(pfx)) {
      v <- input[[paste0(pfx, "_", suffix)]]
      if (!is.null(v)) return(v)
    }
    global
  }

  # THEME : ggplot2 D'ABORD.
  # Le module imposait son propre habillage (theme_minimal, titre centre en
  # gras colore, legende en bas, grille secondaire retiree, base 12 pt). Un
  # graphique doit d'abord s'afficher tel que ggplot2 le dessine -- c'est la
  # reference que tout le monde connait -- et l'utilisateur s'en ecarte s'il le
  # veut. L'inverse oblige a defaire avant de faire.
  #
  # "gg" (defaut) : theme_grey(11), exactement ggplot2.
  # "hstat"       : l'habillage precedent, conserve pour qui l'avait adopte.
  # les autres    : les themes de HSTAT_THEMES_GG, partages avec les autres
  #                 modules.
  mv_gg_theme <- function() {
    choix    <- .mv_loc("theme", input$mv_theme %||% "gg")
    txt_sz   <- .mv_loc("textsz", input$mv_text_size %||% HSTAT_GG_BASE_SIZE)
    face_txt <- if (isTRUE(.mv_loc("bold", input$mv_bold_text))) "bold" else "plain"
    pos_leg  <- .mv_loc("legendpos", input$mv_legend_pos %||% "gg")
    grille   <- .mv_loc("grid", input$mv_grid %||% "gg")

    base <- if (identical(choix, "hstat"))
      ggplot2::theme_minimal(base_size = txt_sz) +
        ggplot2::theme(
          plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold", size = txt_sz + 2, color = "#2c3e50"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "#555", size = txt_sz - 1),
          axis.title = ggplot2::element_text(size = txt_sz, face = face_txt),
          axis.text  = ggplot2::element_text(size = txt_sz - 1, face = face_txt),
          legend.text = ggplot2::element_text(size = txt_sz - 1),
          legend.title = ggplot2::element_text(size = txt_sz, face = "bold"),
          legend.position = "bottom",
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
        )
    else if (identical(choix, "gg")) ggplot2::theme_grey(base_size = txt_sz)
    else viz_get_theme(choix, base_size = txt_sz)

    # Reglages surajoutes SEULEMENT s'ils ont ete demandes : sans cela, le
    # theme choisi ne serait plus celui qu'on croit.
    sur <- list()
    if (isTRUE(.mv_loc("bold", input$mv_bold_text)))
      sur <- c(sur, list(ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"),
                               axis.text  = ggplot2::element_text(face = "bold"))))
    if (!identical(pos_leg, "gg"))
      sur <- c(sur, list(ggplot2::theme(legend.position = pos_leg)))
    if (identical(grille, "sans"))
      sur <- c(sur, list(ggplot2::theme(panel.grid = ggplot2::element_blank())))
    else if (identical(grille, "majeure"))
      sur <- c(sur, list(ggplot2::theme(panel.grid.minor = ggplot2::element_blank())))

    for (s in sur) base <- base + s
    base
  }
  # Titres et libelles d'axes demandes par l'utilisateur, greffes sur le
  # graphique DEJA construit. Aucune analyse n'a a les connaitre : une methode
  # ajoutee plus tard en beneficie sans une ligne de plus. Un champ vide laisse
  # le libelle calcule par l'analyse (axes, % de variance) -- l'effacer serait
  # perdre une information que l'utilisateur n'a pas demande a perdre.
  mv_habille <- function(p) {
    if (!inherits(p, "ggplot")) return(p)
    txt <- function(suffix) {
      v <- .mv_loc(suffix, NULL)
      if (is.null(v)) return(NULL)
      v <- trimws(as.character(v)[1])
      if (nzchar(v)) v else NULL
    }
    lab <- list(title = txt("title"), subtitle = txt("subtitle"),
                x = txt("xlab"), y = txt("ylab"))
    lab <- lab[!vapply(lab, is.null, logical(1))]
    if (length(lab)) p <- p + do.call(ggplot2::labs, lab)
    p
  }

  # Tailles accessibles aux plotfns (locales par methode, sinon globales)
  mv_pt_size <- function() .mv_loc("ptsz", input$mv_point_size %||% HSTAT_GG_POINT_SIZE)
  # Tailles de labels : reglees en POINTS (12 a 24 pt) par l'utilisateur, une
  # valeur pour les INDIVIDUS et une pour les VARIABLES / modalites. Les
  # accesseurs renvoient directement l'unite ggplot2 (mm).
  mv_lbl_pt_ind  <- function() hstat_lbl_pt(.mv_loc("lblsz", input$mv_label_size))
  mv_lbl_pt_var  <- function() hstat_lbl_pt(.mv_loc("lblszvar", input$mv_label_size_var))
  mv_lbl_size     <- function() hstat_lbl_pt2gg(mv_lbl_pt_ind())
  mv_lbl_size_var <- function() hstat_lbl_pt2gg(mv_lbl_pt_var())
  mv_ln_width <- function() .mv_loc("arrsz", input$mv_line_width %||% HSTAT_GG_LINEWIDTH)
  mv_show_labels <- function() isTRUE(.mv_loc("showlab", input$mv_show_labels))
  # Ajoute des etiquettes (repel) a un ggplot si l'option globale est active.
  # df doit contenir les colonnes x, y et une colonne 'label'.
  mv_add_labels <- function(p, df, xcol, ycol, labelcol = "label") {
    if (!mv_show_labels()) return(p)
    if (is.null(df[[labelcol]])) return(p)
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p + ggrepel::geom_text_repel(
        data = df,
        mapping = ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]], label = .data[[labelcol]]),
        size = mv_lbl_size(), max.overlaps = 30, show.legend = FALSE,
        inherit.aes = FALSE)
    } else {
      p + ggplot2::geom_text(
        data = df,
        mapping = ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]], label = .data[[labelcol]]),
        size = mv_lbl_size(), vjust = -0.6, check_overlap = TRUE,
        show.legend = FALSE, inherit.aes = FALSE)
    }
  }

  # Echelles de couleur choisies par l'utilisateur pour les graphiques multivaries
  # etendus. Renvoie une liste de scales (color + fill) a ajouter au ggplot.
  mv_color_scale <- function(discrete = TRUE) {
    pal <- .mv_loc("palette", input$mv_palette %||% "default")
    if (pal == "default") return(NULL)
    if (discrete) {
      switch(pal,
        "Set1"    = list(ggplot2::scale_color_brewer(palette = "Set1"), ggplot2::scale_fill_brewer(palette = "Set1")),
        "Set2"    = list(ggplot2::scale_color_brewer(palette = "Set2"), ggplot2::scale_fill_brewer(palette = "Set2")),
        "Dark2"   = list(ggplot2::scale_color_brewer(palette = "Dark2"), ggplot2::scale_fill_brewer(palette = "Dark2")),
        "Paired"  = list(ggplot2::scale_color_brewer(palette = "Paired"), ggplot2::scale_fill_brewer(palette = "Paired")),
        "viridis" = list(ggplot2::scale_color_viridis_d(), ggplot2::scale_fill_viridis_d()),
        "plasma"  = list(ggplot2::scale_color_viridis_d(option = "plasma"), ggplot2::scale_fill_viridis_d(option = "plasma")),
        NULL)
    } else {
      switch(pal,
        "viridis" = list(ggplot2::scale_color_viridis_c(), ggplot2::scale_fill_viridis_c()),
        "plasma"  = list(ggplot2::scale_color_viridis_c(option = "plasma"), ggplot2::scale_fill_viridis_c(option = "plasma")),
        NULL)
    }
  }

  mv_empty_plot <- function(msg = "Lancez l'analyse pour afficher le graphique.") {
    ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 5, color = "#888") +
      ggplot2::theme_void()
  }


  mv_register <- function(key, accent = "#3a6186") {
    output[[paste0("mv_", key, "_status")]] <- shiny::renderUI({
      r <- mv_res[[key]]
      if (is.null(r)) return(mv_status_box("info",
        "Configurez les paramètres puis cliquez sur 'Lancer l'analyse'."))
      if (isFALSE(r$ok)) return(mv_status_box("err", r$error))
      mv_status_box("info", if (!is.null(r$note)) r$note else "Analyse realisee avec succès.")
    })
    output[[paste0("mv_", key, "_metrics")]] <- shiny::renderUI({
      r <- mv_res[[key]]
      if (is.null(r))
        return(p(style = "color:#888; font-style:italic; padding:20px; text-align:center;",
                 shiny::icon("hourglass-half"), " En attente du lancement de l'analyse."))
      if (isFALSE(r$ok)) return(mv_status_box("err", r$error))
      r$render
    })
    output[[paste0("mv_", key, "_plot")]] <- shiny::renderPlot({
      r <- mv_res[[key]]
      if (is.null(r) || isFALSE(r$ok) || is.null(r$plotfn)) return(mv_empty_plot())
      mv_active_prefix(paste0("mv_", key))
      on.exit(mv_active_prefix(NULL), add = TRUE)
      suppressWarnings(suppressMessages(print(mv_habille(r$plotfn()))))
    }, res = 120)
    output[[paste0("mv_", key, "_summary")]] <- shiny::renderPrint({
      r <- mv_res[[key]]
      if (is.null(r)) { cat("En attente du lancement de l'analyse.\n"); return(invisible()) }
      if (isFALSE(r$ok)) { cat("Erreur :", r$error, "\n"); return(invisible()) }
      cat(paste(r$summary, collapse = "\n"), "\n")
    })
    output[[paste0("mv_", key, "_dl_xlsx")]] <- shiny::downloadHandler(
      filename = function() paste0("HStat_", key, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        r <- mv_res[[key]]
        wb <- openxlsx::createWorkbook()
        exps <- if (!is.null(r) && !is.null(r$exports)) r$exports else
          list(Info = data.frame(Message = "Aucun résultat disponible"))
        for (nm in names(exps)) {
          sh <- substr(gsub("[^A-Za-z0-9_]", "_", nm), 1, 31)
          openxlsx::addWorksheet(wb, sh)
          openxlsx::writeData(wb, sh, exps[[nm]])
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      })
    output[[paste0("mv_", key, "_dl_csv")]] <- shiny::downloadHandler(
      filename = function() paste0("HStat_", key, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        r <- mv_res[[key]]
        d <- if (!is.null(r) && !is.null(r$metrics)) r$metrics else
          data.frame(Message = "Aucun résultat disponible")
        utils::write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8")
      })
    # Telechargement du GRAPHIQUE (taille adaptee au DPI, multi-formats).
    local({
      kk <- key
      output[[paste0("mv_", kk, "_download")]] <- shiny::downloadHandler(
        filename = function() {
          fmt <- hstat_img_fmt(input[[paste0("mv_", kk, "_fmt")]])
          paste0("HStat_", kk, "_graphique_", Sys.Date(), ".", fmt)
        },
        content = function(file) {
          fmt <- hstat_img_fmt(input[[paste0("mv_", kk, "_fmt")]])
          dpi <- .hstat_num1(input[[paste0("mv_", kk, "_dpi")]], 300)
          r <- mv_res[[kk]]
          # Un `return()` sans avoir ecrit le fichier faisait renvoyer a Shiny
          # sa page d'erreur HTML, que le navigateur enregistrait sous le nom
          # demande : on croyait tenir un PNG, on ouvrait du HTML.
          if (is.null(r) || isFALSE(r$ok) || is.null(r$plotfn)) {
            shiny::showNotification("Lancez d'abord l'analyse avant de télécharger le graphique.",
                             type = "warning")
            hstat_image_secours(file, fmt,
              "Aucun graphique : lancez d'abord l'analyse, puis retelechargez.")
            return()
          }
          # La largeur et la hauteur demandees etaient IGNOREES : le fichier
          # sortait toujours carre, 9 pouces de cote. La taille physique retenue
          # est desormais lue -- et non recalculee depuis les champs, pour que
          # le fichier fasse pouces x DPI meme si l'affichage n'a pas suivi.
          po   <- mv_pouces_export(paste0("mv_", kk), 10, 7.5)
          w_in <- po[1]
          h_in <- po[2]
          mv_active_prefix(paste0("mv_", kk))
          on.exit(mv_active_prefix(NULL), add = TRUE)
          p <- tryCatch(mv_habille(r$plotfn()), error = function(e) NULL)
          # hstat_ecrire_image garantit un fichier VALIDE du format demande,
          # meme quand le graphique est indisponible.
          if (!hstat_ecrire_image(file, p, fmt, w_in, h_in, dpi))
            shiny::showNotification("Graphique indisponible : le fichier téléchargé porte le motif.",
                             type = "error", duration = 6)
        })
    })
  }
  # =========================================================================
  #  RESOLUTION : LA TAILLE SUIT, PARTOUT ET SANS EXCEPTION
  # -------------------------------------------------------------------------
  #  Choisir 600 DPI sans toucher aux pixels ne changeait rien a la finesse :
  #  la figure gardait la meme taille physique divisee par une resolution plus
  #  grande, donc une image plus petite sur le papier. Les champs de largeur et
  #  de hauteur se recalculent desormais a CHAQUE changement de resolution, a
  #  taille physique constante -- ils disent alors exactement ce que le fichier
  #  contiendra.
  #
  #  Le recalcul part de la TAILLE PHYSIQUE (.mv_pouces), jamais des pixels
  #  affiches et d'une resolution precedente. La chaine « px x neuf / ancien »
  #  qui vivait ici avait besoin de deux etats justes en meme temps : l'ancien
  #  DPI retenu par le serveur, et les pixels tels que le navigateur les avait
  #  deja renvoyes. Un panneau d'analyse reconstruit -- l'utilisateur revient
  #  sur la fiche, recharge un fichier -- remet les champs a leur valeur
  #  d'origine sans que l'ancien DPI change, et le maillon suivant repart d'une
  #  valeur perimee. La taille physique ne depend ni de l'un ni de l'autre.

  mv_lier_dpi <- function(prefix) {
    local({
      pfx   <- prefix
      idw   <- paste0(pfx, "_width")
      idh   <- paste0(pfx, "_height")
      iddpi <- paste0(pfx, "_dpi")

      # 1. L'utilisateur redimensionne : c'est lui qui fixe la taille physique.
      #    `ignoreInit = FALSE` la releve des le premier passage, sur les
      #    valeurs declarees dans l'interface -- rien n'est code en dur ici, et
      #    un bloc dont les champs changeraient de defaut suit tout seul.
      #
      #    Nos propres ecritures reviennent par ce meme chemin : les ignorer est
      #    indispensable. Un echo arrive en retard redefinirait sinon la taille
      #    physique en divisant d'anciens pixels par la resolution DEJA changee,
      #    et la figure retrecirait a chaque cran.
      shiny::observeEvent(list(input[[idw]], input[[idh]]), {
        w   <- .hstat_num1(input[[idw]], NA)
        h   <- .hstat_num1(input[[idh]], NA)
        dpi <- .hstat_num1(input[[iddpi]], NA)
        if (!isTRUE(is.finite(w)) || w <= 0) return()
        if (!isTRUE(is.finite(h)) || h <= 0) return()
        if (!isTRUE(is.finite(dpi)) || dpi <= 0) return()
        att <- .mv_ecrit[[pfx]]
        if (!is.null(att) && isTRUE(all(att == c(w, h)))) return()
        .mv_pouces[[pfx]] <- c(w / dpi, h / dpi)
      }, ignoreInit = FALSE)

      # 2. La resolution change : les champs disent les pixels produits.
      shiny::observeEvent(input[[iddpi]], {
        dpi <- .hstat_num1(input[[iddpi]], NA)
        # Champ vide en cours de saisie : ne rien recalculer. Retomber sur une
        # valeur par defaut redimensionnerait la figure a chaque effacement,
        # sous les doigts de l'utilisateur.
        if (!isTRUE(is.finite(dpi)) || dpi <= 0) return()
        po <- .mv_pouces[[pfx]]
        if (is.null(po)) return()
        pw <- hstat_px_pour_dpi(po[1], dpi)
        ph <- hstat_px_pour_dpi(po[2], dpi)
        if (is.null(pw) || is.null(ph)) return()
        .mv_ecrit[[pfx]] <- c(pw, ph)
        shiny::updateNumericInput(session, idw, value = pw)
        shiny::updateNumericInput(session, idh, value = ph)
      }, ignoreInit = TRUE)

      # 3. Ce que le fichier contiendra, ecrit sous les champs. Le lien entre la
      #    resolution et la taille produite ne se voyait nulle part : il ne
      #    restait qu'a le croire.
      output[[paste0(pfx, "_dimnote")]] <- shiny::renderUI({
        dpi <- .hstat_num1(input[[iddpi]], NA)
        w   <- .hstat_num1(input[[idw]], NA)
        h   <- .hstat_num1(input[[idh]], NA)
        if (!isTRUE(is.finite(dpi)) || !isTRUE(is.finite(w)) || !isTRUE(is.finite(h)))
          return(NULL)
        cm <- function(px) format(round(px / dpi * 2.54, 1), decimal.mark = ",",
                                  trim = TRUE, nsmall = 1)
        shiny::div(style = paste0("margin-top:6px;padding:6px 10px;background:#eef1f4;",
                           "border-left:3px solid #2c3e50;border-radius:4px;",
                           "font-size:12px;color:#2c3e50;"),
            shiny::icon("ruler-combined"), " ",
            trf("Fichier produit : %s × %s px, soit %s × %s cm à %s DPI.",
                round(w), round(h), cm(w), cm(h), round(dpi)))
      })
    })
  }

  # Les 14 analyses generiques (boites mv_disp_box / mv_viz_options_ui) et les
  # 9 exports historiques (ACP, HCPC, AFD), declares dans UX.R. Aucune
  # exception : c'est la demande, et c'est aussi la seule facon de ne pas
  # laisser une analyse se comporter autrement que ses voisines.
  MV_EXPORTS_DPI <- c(
    paste0("mv_", c("kmeans","efa","cfa","mtmm","pls","regmult","afc","mca",
                    "kmodes","lca","logit","famd","mfa","kproto")),
    "pcaPlot", "pcaScree", "pcaParallel", "pcaCTR",
    "hcpcCluster", "hcpcDend", "hcpcHeights", "afdInd", "afdVar")
  for (.pfx in MV_EXPORTS_DPI) mv_lier_dpi(.pfx)

  # Libelles des analyses multivariees, pour l'onglet d'aide a la decision.
  MV_LIBELLES <- c(
    kmeans = "Classification k-means", efa = "Analyse factorielle exploratoire",
    cfa = "Analyse factorielle confirmatoire", mtmm = "Matrice multitrait-multimethode",
    pls = "Regression PLS", regmult = "Regression multiple",
    afc = "Analyse factorielle des correspondances (AFC)",
    mca = "Analyse des correspondances multiples (ACM)",
    kmodes = "Classification k-modes", lca = "Analyse en classes latentes",
    logit = "Regression logistique", famd = "Analyse factorielle de donnees mixtes (AFDM)",
    mfa = "Analyse factorielle multiple (AFM)", kproto = "Classification k-prototypes")

  for (k in c("kmeans","efa","cfa","mtmm","pls","regmult","afc","mca",
              "kmodes","lca","logit","famd","mfa","kproto"))
    mv_register(k)

  # Depot du resultat multivarie courant pour l'aide a la decision. Un
  # observateur par cle plutot qu'un appel dans chacune des 14 analyses : le
  # code de l'analyse n'a rien a savoir de l'assistance.
  for (k in c("kmeans","efa","cfa","mtmm","pls","regmult","afc","mca",
              "kmodes","lca","logit","famd","mfa","kproto")) {
    local({
      key <- k
      shiny::observeEvent(mv_res[[key]], {
        r <- mv_res[[key]]
        if (is.null(r) || isFALSE(r$ok)) return()
        vars <- unlist(lapply(paste0("mv_", key, c("_vars", "_x", "_y", "_num", "_quanti")),
                              function(id) input[[id]]))
        grp <- unlist(lapply(paste0("mv_", key, c("_group", "_cat", "_quali", "_facteur")),
                             function(id) input[[id]]))
        hstat_ai_capture(values, "Analyses multivariees",
          MV_LIBELLES[[key]] %||% key,
          tables = list("Metriques" = r$metrics),
          text = if (!is.null(r$summary)) paste(r$summary, collapse = "\n") else NULL,
          meta = list(variables = unique(vars), groupe = unique(grp)))
      }, ignoreInit = TRUE)
    })
  }

  # ============================ CONTROLES =====================================
  mv_pick_num <- function(id, label, multiple = TRUE, selected = NULL) {
    nc <- mv_num_cols()
    pickerInput(id, label, choices = nc, multiple = multiple,
                selected = if (is.null(selected)) (if (multiple) nc else nc[1]) else selected,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  }
  mv_pick_cat <- function(id, label, multiple = TRUE, selected = NULL) {
    cc <- mv_cat_cols()
    pickerInput(id, label, choices = cc, multiple = multiple,
                selected = if (is.null(selected)) (if (multiple) cc else cc[1]) else selected,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  }
  mv_opt_box <- function(...) shiny::div(
    style = "background:#f8f9fa; border-left:4px solid #6c757d; padding:10px; margin:8px 0;", ...)

  output$mv_kmeans_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("chart-line"), " Variables numériques"),
        mv_pick_num("mv_kmeans_vars", "Variables à partitionner :")),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput("mv_kmeans_k", shiny::tagList(shiny::icon("object-group"), " Nombre de clusters k :"),
                               value = 3, min = 2, max = 15)),
        shiny::column(4, shiny::numericInput("mv_kmeans_nstart", shiny::tagList(shiny::icon("redo"), " Initialisations :"),
                               value = 25, min = 1, max = 100)),
        shiny::column(4, shiny::div(style = "margin-top:25px;",
          shiny::checkboxInput("mv_kmeans_scale", shiny::tagList(shiny::icon("balance-scale"), " Standardiser"), TRUE)))), mv_disp_box("mv_kmeans"))
  })

  output$mv_efa_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("chart-line"), " Variables numériques"),
        mv_pick_num("mv_efa_vars", "Variables observées :")),
      shiny::fluidRow(
        shiny::column(3, shiny::numericInput("mv_efa_nf", shiny::tagList(shiny::icon("hashtag"), " Nombre de facteurs :"),
                               value = 2, min = 1, max = 15)),
        shiny::column(3, shiny::selectInput("mv_efa_rot", shiny::tagList(shiny::icon("sync-alt"), " Rotation :"),
                              choices = c("Varimax (orthogonale)" = "varimax",
                                          "Oblimin (oblique)" = "oblimin",
                                          "Promax (oblique)" = "promax",
                                          "Aucune" = "none"), selected = "varimax")),
        shiny::column(3, shiny::selectInput("mv_efa_fm", shiny::tagList(shiny::icon("cogs"), " Extraction :"),
                              choices = c("Maximum de vraisemblance" = "ml",
                                          "Axes principaux" = "pa",
                                          "Moindres carres" = "minres"), selected = "ml")),
        shiny::column(3, shiny::selectInput("mv_efa_plot", shiny::tagList(shiny::icon("project-diagram"), " Graphique :"),
                              choices = c("Carte des saturations" = "heat",
                                          "Diagramme des variables latentes" = "path"),
                              selected = "heat"))), mv_disp_box("mv_efa"))
  })

  output$mv_cfa_controls <- shiny::renderUI({
    shiny::req(mv_data())
    nc <- mv_num_cols()
    ex <- if (length(nc) >= 4)
      paste0("F1 =~ ", paste(nc[1:2], collapse = " + "), "\n",
             "F2 =~ ", paste(nc[3:min(4,length(nc))], collapse = " + "))
    else "F1 =~ var1 + var2 + var3"
    shiny::tagList(
      mv_opt_box(
        shiny::h5(shiny::icon("project-diagram"), " Modèle de mesure (syntaxe lavaan)"),
        p(style = "font-size:11px; color:#555;",
          "Un facteur par ligne. Exemple : ", shiny::tags$code("F1 =~ x1 + x2 + x3")),
        shiny::textAreaInput("mv_cfa_model", NULL, value = ex, rows = 5, width = "100%"),
        p(style = "font-size:11px; color:#888;", shiny::icon("lightbulb"),
          " Variables numériques disponibles : ", paste(nc, collapse = ", "))),
      shiny::selectInput("mv_cfa_est", shiny::tagList(shiny::icon("cogs"), " Estimateur :"),
                  choices = c("ML (max. vraisemblance)" = "ML", "MLR (robuste)" = "MLR"),
                  selected = "MLR"), mv_disp_box("mv_cfa"))
  })

  # ---- MTMM : controles ------------------------------------------------------
  # Chaque variable selectionnee mesure UN trait par UNE methode. L'affectation
  # se fait soit automatiquement depuis le nom (format Trait<sep>Methode, coupe
  # sur le DERNIER separateur), soit manuellement via une grille de saisie.
  .mtmm_parse_name <- function(nm, sep) {
    # Coupe sur la DERNIERE occurrence LITTERALE du separateur (aucune regex :
    # robuste a tout separateur, y compris ".", "-", "|", etc.).
    pos <- gregexpr(sep, nm, fixed = TRUE)[[1]]
    if (identical(as.integer(pos[1]), -1L))
      return(c(trait = NA_character_, methode = NA_character_))
    cut <- max(pos)
    tr <- substr(nm, 1, cut - 1)
    me <- substr(nm, cut + nchar(sep), nchar(nm))
    if (!nzchar(tr) || !nzchar(me))
      return(c(trait = NA_character_, methode = NA_character_))
    c(trait = tr, methode = me)
  }
  output$mv_mtmm_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("chart-line"), " Variables trait \u00d7 m\u00e9thode"),
        mv_pick_num("mv_mtmm_vars", "Variables (1 par combinaison trait \u00d7 m\u00e9thode) :")),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput("mv_mtmm_auto",
          shiny::tagList(shiny::icon("magic"), " D\u00e9duire trait & m\u00e9thode des noms de variables"), value = TRUE)),
        shiny::column(3, shiny::conditionalPanel("input.mv_mtmm_auto == true",
          shiny::textInput("mv_mtmm_sep", "S\u00e9parateur :", value = "_")))),
      shiny::conditionalPanel("input.mv_mtmm_auto == false",
        mv_opt_box(shiny::h5(shiny::icon("tags"), " Affectation manuelle"),
          p(style = "font-size:12px;color:#666;",
            "Renseignez pour chaque variable le trait mesur\u00e9 et la m\u00e9thode employ\u00e9e."),
          shiny::uiOutput("mv_mtmm_assign"))),
      mv_disp_box("mv_mtmm"))
  })
  output$mv_mtmm_assign <- shiny::renderUI({
    vars <- input$mv_mtmm_vars
    if (is.null(vars) || !length(vars)) return(NULL)
    all_cols <- names(mv_data())
    sep <- input$mv_mtmm_sep %||% "_"
    rows <- lapply(vars, function(v) {
      idx <- match(v, all_cols)
      guess <- .mtmm_parse_name(v, sep)
      shiny::fluidRow(
        shiny::column(4, shiny::div(style = "margin-top:6px;font-weight:bold;font-size:12px;", v)),
        shiny::column(4, shiny::textInput(paste0("mv_mtmm_t_", idx), NULL,
                            value = if (!is.na(guess["trait"])) guess["trait"] else "",
                            placeholder = "Trait")),
        shiny::column(4, shiny::textInput(paste0("mv_mtmm_m_", idx), NULL,
                            value = if (!is.na(guess["methode"])) guess["methode"] else "",
                            placeholder = "M\u00e9thode")))
    })
    do.call(shiny::tagList, rows)
  })
  output$mv_mtmm_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_mtmm_vars %||% character(0))
    n_st <- if (n >= 100) "ok" else if (n >= 50) "warn" else "err"
    p_st <- if (p >= 4) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 50, recommand\u00e9 100)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif mod\u00e9r\u00e9 : n=", n, " (recommand\u00e9 n>=100)."))
    if (p_st == "err") msgs <- c(msgs, "Le MTMM requiert au moins 4 variables (2 traits \u00d7 2 m\u00e9thodes).")
    lvl <- if ("err" %in% c(n_st, p_st)) "err" else if ("warn" %in% c(n_st, p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- MTMM",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " var.")),
              .mv_badge("info", "Affectations traits/m\u00e9thodes v\u00e9rifi\u00e9es au lancement")), msgs, lvl)
  })

  output$mv_pls_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(
        shiny::h5(shiny::icon("bullseye"), " Variable réponse Y"),
        shiny::selectInput("mv_pls_y", "Réponse a prédire :", choices = names(mv_data())),
        p(style = "font-size:11px; color:#555;",
          "Y numérique => PLS | Y catégorielle => PLS-DA (détection automatique)")),
      mv_opt_box(shiny::h5(shiny::icon("chart-line"), " Prédicteurs X"),
        mv_pick_num("mv_pls_x", "Prédicteurs numériques :")),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput("mv_pls_ncomp", shiny::tagList(shiny::icon("hashtag"), " Composantes :"),
                               value = 3, min = 1, max = 15)),
        shiny::column(6, shiny::div(style = "margin-top:25px;",
          shiny::checkboxInput("mv_pls_cv", shiny::tagList(shiny::icon("redo"), " Validation croisée"), TRUE)))), mv_disp_box("mv_pls"))
  })

  output$mv_regmult_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("bullseye"), " Variable réponse Y (numérique)"),
        shiny::selectInput("mv_regmult_y", "Réponse a expliquer :", choices = mv_num_cols())),
      mv_opt_box(shiny::h5(shiny::icon("chart-line"), " Prédicteurs X"),
        shiny::selectInput("mv_regmult_x", "Prédicteurs (sélection multiple) :",
                    choices = names(mv_data()), multiple = TRUE,
                    selectize = TRUE),
        p(style = "font-size:11px;color:#6c757d;font-style:italic;margin-top:4px;",
          shiny::icon("info-circle"),
          " Cliquez pour ajouter plusieurs prédicteurs ; retirez-les avec la croix.")), mv_disp_box("mv_regmult"))
  })

  output$mv_afc_controls <- shiny::renderUI({
    shiny::req(mv_data())
    cc <- mv_cat_cols()
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("shapes"), " Deux variables qualitatives"),
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput("mv_afc_row", shiny::tagList(shiny::icon("grip-lines"), " Variable-ligne :"),
                                choices = cc)),
          shiny::column(6, shiny::selectInput("mv_afc_col", shiny::tagList(shiny::icon("grip-lines-vertical"), " Variable-colonne :"),
                                choices = cc, selected = if (length(cc) > 1) cc[2] else cc[1])))), mv_disp_box("mv_afc"))
  })

  output$mv_mca_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("shapes"), " Variables qualitatives"),
        mv_pick_cat("mv_mca_vars", "Variables à analyser :")),
      shiny::numericInput("mv_mca_ncp", shiny::tagList(shiny::icon("hashtag"), " Dimensions a retenir :"),
                   value = 5, min = 2, max = 15),
      mv_opt_box(shiny::h5(shiny::icon("plus-circle"), " Éléments supplémentaires (optionnel)"),
        pickerInput("mv_mca_quali_sup", "Variables qualitatives supplémentaires :",
                    choices = mv_cat_cols(), multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        pickerInput("mv_mca_quanti_sup", "Variables quantitatives supplémentaires :",
                    choices = mv_num_cols(), multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        shiny::textInput("mv_mca_ind_sup", "Individus supplémentaires (n° de ligne, séparés par virgule) :",
                  placeholder = "ex : 1, 5, 12")),
      mv_viz_options_ui("mv_mca"))
  })

  output$mv_kmodes_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("shapes"), " Variables qualitatives"),
        mv_pick_cat("mv_kmodes_vars", "Variables à partitionner :")),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput("mv_kmodes_k", shiny::tagList(shiny::icon("object-group"), " Nombre de clusters k :"),
                               value = 3, min = 2, max = 15)),
        shiny::column(6, shiny::numericInput("mv_kmodes_iter", shiny::tagList(shiny::icon("redo"), " Iterations max :"),
                               value = 20, min = 5, max = 100))), mv_disp_box("mv_kmodes"))
  })

  output$mv_lca_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("shapes"), " Variables qualitatives (indicateurs)"),
        mv_pick_cat("mv_lca_vars", "Indicateurs catégoriels :")),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput("mv_lca_nclass", shiny::tagList(shiny::icon("object-group"), " Nombre de classes :"),
                               value = 2, min = 2, max = 10)),
        shiny::column(6, shiny::numericInput("mv_lca_rep", shiny::tagList(shiny::icon("redo"), " Repetitions EM :"),
                               value = 5, min = 1, max = 30))), mv_disp_box("mv_lca"))
  })

  output$mv_logit_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("bullseye"), " Variable réponse Y (catégorielle)"),
        shiny::selectInput("mv_logit_y", "Réponse a prédire :", choices = mv_cat_cols()),
        p(style = "font-size:11px; color:#555;",
          "2 modalités => logistique binaire | >2 => logistique multinomiale")),
      mv_opt_box(shiny::h5(shiny::icon("chart-line"), " Prédicteurs X"),
        pickerInput("mv_logit_x", "Prédicteurs :", choices = names(mv_data()),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))), mv_disp_box("mv_logit"))
  })

  output$mv_famd_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("layer-group"), " Variables mixtes (quanti + quali)"),
        pickerInput("mv_famd_vars", "Variables à analyser :",
                    choices = names(mv_data()), multiple = TRUE,
                    selected = names(mv_data()),
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))),
      shiny::numericInput("mv_famd_ncp", shiny::tagList(shiny::icon("hashtag"), " Dimensions a retenir :"),
                   value = 5, min = 2, max = 15),
      mv_opt_box(shiny::h5(shiny::icon("plus-circle"), " Éléments supplémentaires (optionnel)"),
        pickerInput("mv_famd_quali_sup", "Variables qualitatives supplémentaires :",
                    choices = mv_cat_cols(), multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        pickerInput("mv_famd_quanti_sup", "Variables quantitatives supplémentaires :",
                    choices = mv_num_cols(), multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        shiny::textInput("mv_famd_ind_sup", "Individus supplémentaires (n° de ligne, séparés par virgule) :",
                  placeholder = "ex : 1, 5, 12")),
      mv_viz_options_ui("mv_famd"))
  })

  output$mv_mfa_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("ruler-combined"), " Bloc QUANTITATIF"),
        mv_pick_num("mv_mfa_quanti", "Variables numériques :")),
      mv_opt_box(shiny::h5(shiny::icon("shapes"), " Bloc QUALITATIF"),
        mv_pick_cat("mv_mfa_quali", "Variables qualitatives :")),
      shiny::numericInput("mv_mfa_ncp", shiny::tagList(shiny::icon("hashtag"), " Dimensions a retenir :"),
                   value = 5, min = 2, max = 15),
      mv_opt_box(shiny::h5(shiny::icon("plus-circle"), " Éléments supplémentaires (optionnel)"),
        pickerInput("mv_mfa_quali_sup", "Variables qualitatives supplémentaires :",
                    choices = mv_cat_cols(), multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        shiny::textInput("mv_mfa_ind_sup", "Individus supplémentaires (n° de ligne, séparés par virgule) :",
                  placeholder = "ex : 1, 5, 12")),
      mv_viz_options_ui("mv_mfa"))
  })

  output$mv_kproto_controls <- shiny::renderUI({
    shiny::req(mv_data())
    shiny::tagList(
      mv_opt_box(shiny::h5(shiny::icon("layer-group"), " Variables mixtes (quanti + quali)"),
        pickerInput("mv_kproto_vars", "Variables à partitionner :",
                    choices = names(mv_data()), multiple = TRUE,
                    selected = names(mv_data()),
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput("mv_kproto_k", shiny::tagList(shiny::icon("object-group"), " Nombre de clusters k :"),
                               value = 3, min = 2, max = 15)),
        shiny::column(6, shiny::numericInput("mv_kproto_iter", shiny::tagList(shiny::icon("redo"), " Iterations max :"),
                               value = 20, min = 5, max = 100))), mv_disp_box("mv_kproto"))
  })

  # ====================== VERIFICATION DES CONDITIONS =========================
  output$mv_kmeans_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); k <- input$mv_kmeans_k %||% 3
    p <- length(input$mv_kmeans_vars %||% character(0))
    n_st <- if (n >= 10*k) "ok" else if (n >= 2*k) "warn" else "err"
    p_st <- if (p >= 3) "ok" else if (p >= 2) "warn" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif critique : n=", n, " < 2k=", 2*k, "."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (recommande 10k=", 10*k, ")."))
    if (p_st == "err") msgs <- c(msgs, "Sélectionnez au moins 2 variables numériques.")
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- k-means",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " var.")),
              .mv_badge("info", paste0("k = ", k))), msgs, lvl)
  })

  # ---- AFE -----------------------------------------------------------------
  output$mv_efa_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_efa_vars %||% character(0))
    n_st <- if (n >= 200) "ok" else if (n >= 100 || n >= 5*p) "warn" else "err"
    p_st <- if (p >= 3) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 100, ideal 200)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modéré : n=", n, " (ideal n>=200, >=10 ind./var.)."))
    if (p_st == "err") msgs <- c(msgs, "L'AFE requiert au moins 3 variables observées.")
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- AFE",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " var.")),
              .mv_badge("info", "KMO & Bartlett verifies au lancement")), msgs, lvl)
  })

  # ---- AFC confirmatoire ---------------------------------------------------
  output$mv_cfa_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data())
    n_st <- if (n >= 200) "ok" else if (n >= 100) "warn" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 100, recommande 200)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modéré : n=", n, " (recommande n>=200)."))
    if (!mv_has("lavaan")) msgs <- c(msgs, "Package 'lavaan' indisponible -- installation requise.")
    lvl <- if (n_st == "err" || !mv_has("lavaan")) "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- AFC confirmatoire",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(if (mv_has("lavaan")) "ok" else "err", "lavaan")), msgs, lvl)
  })

  # ---- PLS -----------------------------------------------------------------
  output$mv_pls_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_pls_x %||% character(0))
    n_st <- if (n >= 20) "ok" else "warn"
    p_st <- if (p >= 2) "ok" else "err"
    msgs <- list()
    if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (PLS reste valide mais validation croisée conseillee)."))
    if (p_st == "err") msgs <- c(msgs, "Sélectionnez au moins 2 prédicteurs numériques.")
    if (!mv_has("pls")) msgs <- c(msgs, "Package 'pls' indisponible -- installation requise.")
    lvl <- if (p_st == "err" || !mv_has("pls")) "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- PLS / PLS-DA",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " prédicteurs")),
              .mv_badge(if (mv_has("pls")) "ok" else "err", "pls")), msgs, lvl)
  })

  # ---- Regression lineaire multiple ----------------------------------------
  output$mv_regmult_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_regmult_x %||% character(0))
    ratio <- if (p > 0) n / p else NA
    n_st <- if (!is.na(ratio) && ratio >= 15) "ok" else if (!is.na(ratio) && ratio >= 10) "warn" else "err"
    p_st <- if (p >= 1) "ok" else "err"
    msgs <- list()
    if (p_st == "err") msgs <- c(msgs, "Sélectionnez au moins 1 prédicteur.")
    else if (n_st == "err") msgs <- c(msgs, paste0("Ratio n/p faible : ", round(ratio,1), " (recommande >=10, ideal >=20)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Ratio n/p modéré : ", round(ratio,1), " (ideal >=20)."))
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- Regression linéaire multiple",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " prédicteurs")),
              .mv_badge(n_st, paste0("n/p = ", if (is.na(ratio)) "-" else round(ratio,1)))), msgs, lvl)
  })

  # ---- AFC (correspondances) ----------------------------------------------
  output$mv_afc_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data())
    diff_ok <- !is.null(input$mv_afc_row) && !is.null(input$mv_afc_col) &&
      input$mv_afc_row != input$mv_afc_col
    n_st <- if (n >= 50) "ok" else "warn"
    v_st <- if (diff_ok) "ok" else "err"
    msgs <- list()
    if (!diff_ok) msgs <- c(msgs, "Choisissez deux variables qualitatives DIFFERENTES.")
    if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (recommande >=50, effectifs théoriques >=5)."))
    lvl <- if (v_st == "err") "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- AFC",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(v_st, "2 variables distinctes")), msgs, lvl)
  })

  # ---- ACM -----------------------------------------------------------------
  output$mv_mca_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_mca_vars %||% character(0))
    n_st <- if (n >= 100) "ok" else if (n >= 50) "warn" else "err"
    p_st <- if (p >= 3) "ok" else if (p >= 2) "warn" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 50, recommande 100)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modéré : n=", n, " (recommande >=100)."))
    if (p_st == "err") msgs <- c(msgs, "L'ACM requiert au moins 2 variables qualitatives.")
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- ACM",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " var. quali"))), msgs, lvl)
  })

  # ---- k-modes -------------------------------------------------------------
  output$mv_kmodes_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); k <- input$mv_kmodes_k %||% 3
    p <- length(input$mv_kmodes_vars %||% character(0))
    n_st <- if (n >= 10*k) "ok" else if (n >= 2*k) "warn" else "err"
    p_st <- if (p >= 3) "ok" else if (p >= 2) "warn" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif critique : n=", n, " < 2k=", 2*k, "."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (recommande 10k=", 10*k, ")."))
    if (p_st == "err") msgs <- c(msgs, "Sélectionnez au moins 2 variables qualitatives.")
    if (!mv_has("klaR")) msgs <- c(msgs, "Package 'klaR' indisponible -- installation requise.")
    lvl <- if ("err" %in% c(n_st,p_st) || !mv_has("klaR")) "err"
           else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- k-modes",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " var. quali")),
              .mv_badge(if (mv_has("klaR")) "ok" else "err", "klaR")), msgs, lvl)
  })

  # ---- LCA -----------------------------------------------------------------
  output$mv_lca_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_lca_vars %||% character(0))
    n_st <- if (n >= 300) "ok" else if (n >= 100) "warn" else "err"
    p_st <- if (p >= 3) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 100, recommande 300)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modéré : n=", n, " (recommande >=300)."))
    if (p_st == "err") msgs <- c(msgs, "La LCA requiert au moins 3 indicateurs catégoriels.")
    if (!mv_has("poLCA")) msgs <- c(msgs, "Package 'poLCA' indisponible -- installation requise.")
    lvl <- if (p_st == "err" || n_st == "err" || !mv_has("poLCA")) "err"
           else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- LCA",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " indicateurs")),
              .mv_badge(if (mv_has("poLCA")) "ok" else "err", "poLCA")), msgs, lvl)
  })

  # ---- Regression logistique ----------------------------------------------
  output$mv_logit_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); p <- length(input$mv_logit_x %||% character(0))
    epp <- if (p > 0) n / (10 * p) else NA  # >=10 evenements par predicteur
    n_st <- if (!is.na(epp) && epp >= 1) "ok" else if (!is.na(epp) && epp >= 0.5) "warn" else "err"
    p_st <- if (p >= 1) "ok" else "err"
    msgs <- list()
    if (p_st == "err") msgs <- c(msgs, "Sélectionnez au moins 1 prédicteur.")
    else if (n_st == "err") msgs <- c(msgs, paste0("Effectif faible vs nb prédicteurs (regle : >=10 evenements/prédicteur)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif limite (regle des 10 evenements/prédicteur a surveiller)."))
    lvl <- if ("err" %in% c(n_st,p_st)) "err" else if ("warn" %in% c(n_st,p_st)) "warn" else "ok"
    .mv_cond_render("Conditions -- Regression logistique",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(p_st, paste0("p = ", p, " prédicteurs"))), msgs, lvl)
  })

  # ---- AFDM ----------------------------------------------------------------
  output$mv_famd_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data())
    sel <- input$mv_famd_vars %||% character(0)
    d <- mv_data()
    nq <- sum(sapply(sel, function(v) v %in% names(d) && is.numeric(d[[v]])))
    nc <- length(sel) - nq
    n_st <- if (n >= 100) "ok" else if (n >= 50) "warn" else "err"
    mix_st <- if (nq >= 1 && nc >= 1) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 50)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modéré : n=", n, " (recommande >=100)."))
    if (mix_st == "err") msgs <- c(msgs, "L'AFDM requiert au moins 1 variable quantitative ET 1 qualitative.")
    lvl <- if ("err" %in% c(n_st,mix_st)) "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- AFDM",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(mix_st, paste0(nq, " quanti / ", nc, " quali"))), msgs, lvl)
  })

  # ---- AFM -----------------------------------------------------------------
  output$mv_mfa_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data())
    nq <- length(input$mv_mfa_quanti %||% character(0))
    nc <- length(input$mv_mfa_quali %||% character(0))
    n_st <- if (n >= 100) "ok" else if (n >= 50) "warn" else "err"
    b_st <- if (nq >= 1 && nc >= 1) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif insuffisant : n=", n, " (min. 50)."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif modéré : n=", n, " (recommande >=100)."))
    if (b_st == "err") msgs <- c(msgs, "Definissez au moins 1 variable dans chaque bloc (quanti et quali).")
    lvl <- if ("err" %in% c(n_st,b_st)) "err" else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- AFM",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(b_st, paste0("bloc quanti : ", nq)),
              .mv_badge(b_st, paste0("bloc quali : ", nc))), msgs, lvl)
  })

  # ---- k-prototypes --------------------------------------------------------
  output$mv_kproto_conditions <- shiny::renderUI({
    shiny::req(mv_data())
    n <- nrow(mv_data()); k <- input$mv_kproto_k %||% 3
    sel <- input$mv_kproto_vars %||% character(0)
    d <- mv_data()
    nq <- sum(sapply(sel, function(v) v %in% names(d) && is.numeric(d[[v]])))
    nc <- length(sel) - nq
    n_st <- if (n >= 10*k) "ok" else if (n >= 2*k) "warn" else "err"
    mix_st <- if (nq >= 1 && nc >= 1) "ok" else "err"
    msgs <- list()
    if (n_st == "err") msgs <- c(msgs, paste0("Effectif critique : n=", n, " < 2k=", 2*k, "."))
    else if (n_st == "warn") msgs <- c(msgs, paste0("Effectif faible : n=", n, " (recommande 10k=", 10*k, ")."))
    if (mix_st == "err") msgs <- c(msgs, "k-prototypes requiert au moins 1 variable quantitative ET 1 qualitative.")
    if (!mv_has("clustMixType")) msgs <- c(msgs, "Package 'clustMixType' indisponible -- installation requise.")
    lvl <- if ("err" %in% c(n_st,mix_st) || !mv_has("clustMixType")) "err"
           else if (n_st == "warn") "warn" else "ok"
    .mv_cond_render("Conditions -- k-prototypes",
      shiny::tagList(.mv_badge(n_st, paste0("n = ", n)),
              .mv_badge(mix_st, paste0(nq, " quanti / ", nc, " quali")),
              .mv_badge(if (mv_has("clustMixType")) "ok" else "err", "clustMixType")), msgs, lvl)
  })


  # ====================== MOTEURS D'ANALYSE (QUANTI) ==========================

  # ---- k-means -------------------------------------------------------------
  shiny::observeEvent(input$mv_kmeans_run, {
    mv_res[["kmeans"]] <- local({
      tryCatch({
        d <- mv_data(); vars <- input$mv_kmeans_vars
        if (is.null(vars) || length(vars) < 2)
          return(list(ok = FALSE, error = "Sélectionnez au moins 2 variables numériques."))
        k <- input$mv_kmeans_k %||% 3
        X <- d[, vars, drop = FALSE]
        # Ne garder que les colonnes numeriques et remplacer Inf par NA
        X <- X[, vapply(X, is.numeric, logical(1)), drop = FALSE]
        X[] <- lapply(X, function(x) { x[!is.finite(x)] <- NA; x })
        X <- X[stats::complete.cases(X), , drop = FALSE]
        # Retirer les colonnes a variance nulle (sinon scale() -> NaN -> erreur kmeans)
        keep <- vapply(X, function(x) stats::var(x) > 0, logical(1))
        if (sum(keep) < 2)
          return(list(ok = FALSE, error = "Moins de 2 variables numériques a variance non nulle après nettoyage."))
        if (any(!keep)) {
          shiny::showNotification(paste("Variables a variance nulle ignorees :",
            paste(names(X)[!keep], collapse = ", ")), type = "warning", duration = 5)
          X <- X[, keep, drop = FALSE]; vars <- names(X)
        }
        if (nrow(X) < 2*k)
          return(list(ok = FALSE, error = "Effectif insuffisant après suppression des valeurs manquantes."))
        Xs <- if (isTRUE(input$mv_kmeans_scale)) scale(X) else as.matrix(X)
        # Securite : eliminer toute colonne devenue non finie apres scale
        Xs <- Xs[, apply(Xs, 2, function(c) all(is.finite(c))), drop = FALSE]
        if (ncol(Xs) < 2)
          return(list(ok = FALSE, error = "Données non exploitables après standardisation."))
        hstat_set_seed(input$globalSeed)
        Xc <- mv_subsample(as.data.frame(Xs), 8000)
        km <- stats::kmeans(Xc, centers = k, nstart = input$mv_kmeans_nstart %||% 25, iter.max = 50)
        bss <- km$betweenss / km$totss
        sil <- NA
        if (mv_has("cluster") && nrow(Xc) <= 5000)
          sil <- hstat_silhouette_mean(Xc, km$cluster)
        ch <- (km$betweenss/(k-1)) / (km$tot.withinss/(nrow(Xc)-k))
        st_bss <- if (bss >= .5) "ok" else if (bss >= .3) "warn" else "err"
        st_sil <- if (is.na(sil)) "info" else if (sil >= .5) "ok" else if (sil >= .25) "warn" else "err"
        st_bal <- if (min(km$size) >= .05*nrow(Xc)) "ok" else "warn"
        metrics <- rbind(
          mv_row("Inertie inter/totale (R2)", paste0(round(100*bss,1)," %"),
                 ">= 50 % nette ; 30-50 % modérée ; < 30 % faible",
                 if (st_bss=="ok") "Partition nette" else if (st_bss=="warn") "Partition modérée" else "Partition peu séparée",
                 st_bss),
          mv_row("Silhouette moyenne", if (is.na(sil)) "n/d" else round(sil,3),
                 "> 0,50 forte ; 0,25-0,50 raisonnable ; < 0,25 faible",
                 if (st_sil=="ok") "Structure forte" else if (st_sil=="warn") "Structure raisonnable"
                   else if (st_sil=="err") "Structure faible" else "Non calculée (n>5000)",
                 st_sil),
          mv_row("Calinski-Harabasz", round(ch,1),
                 "Plus élevé = meilleure separation (comparatif)",
                 "A maximiser entre solutions k", "info"),
          mv_row("Equilibre des clusters", paste(km$size, collapse=" / "),
                 "Eviter clusters vides ou très minoritaires",
                 if (st_bal=="ok") "Répartition acceptable" else "Cluster très minoritaire",
                 st_bal))
        pc <- stats::prcomp(Xc)
        coord <- as.data.frame(pc$x[, 1:2]); names(coord) <- c("Dim1","Dim2")
        coord$Cluster <- factor(km$cluster)
        coord$label <- rownames(Xc)
        if (is.null(coord$label)) coord$label <- as.character(seq_len(nrow(coord)))
        cen <- as.data.frame(stats::predict(pc, km$centers)[, 1:2]); names(cen) <- c("Dim1","Dim2")
        cen$Cluster <- factor(seq_len(k))
        var_pc <- round(100*pc$sdev^2/sum(pc$sdev^2), 1)
        render <- shiny::tagList(
          mv_card(border_color = "#4a7fa5",
            mv_section_header("Qualite de la partition", "#4a7fa5", "object-group"),
            mv_info_note("Chaque métrique de separation des clusters est confrontee a son seuil de référence."),
            mv_metrics_table(metrics, "#4a7fa5"),
            mv_interp_bar(paste0("Solution a ", k, " clusters sur ", nrow(Xc),
              " individus. Inertie inter-classes = ", round(100*bss,1), " %."),
              mv_col(st_bss))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Centres des clusters (variables d'origine)", "#6c757d", "crosshairs"),
            mv_data_table(data.frame(Cluster = seq_len(k), round(km$centers,3),
                                     check.names = FALSE), "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("k-means : ", k, " clusters sur ", nrow(Xc), " individus."),
          summary = c("=== Classification k-means ===",
            paste0("Variables : ", paste(vars, collapse=", ")),
            paste0("k = ", k, " | nstart = ", input$mv_kmeans_nstart),
            paste0("Inertie inter/totale = ", round(100*bss,2), " %"),
            "", "Centres :", paste(utils::capture.output(round(km$centers,3)), collapse="\n")),
          plotfn = function() {
            p <- ggplot2::ggplot(coord, ggplot2::aes(Dim1, Dim2, color = Cluster)) +
              ggplot2::geom_point(size = mv_pt_size(), alpha = .75) +
              ggplot2::geom_point(data = cen, ggplot2::aes(Dim1, Dim2, fill = Cluster),
                         shape = 23, size = mv_pt_size() + 2.6, color = "black", stroke = 1.1, show.legend = FALSE) +
              labs(title = "Classification k-means -- projection ACP",
                   subtitle = paste0(k, " clusters | inertie inter = ", round(100*bss,1), " %"),
                   x = paste0("Dim 1 (", var_pc[1], " %)"),
                   y = paste0("Dim 2 (", var_pc[2], " %)"),
                   caption = "Les losanges noirs marquent les centres de cluster.") +
              mv_gg_theme() + mv_color_scale(discrete = TRUE)
            mv_add_labels(p, coord, "Dim1", "Dim2", "label")
          },
          exports = list(Metriques = metrics,
            Centres = data.frame(Cluster = seq_len(k), round(km$centers,4), check.names = FALSE),
            Effectifs = data.frame(Cluster = seq_len(k), Effectif = km$size)))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- AFE -----------------------------------------------------------------
  shiny::observeEvent(input$mv_efa_run, {
    mv_res[["efa"]] <- local({
      tryCatch({
        d <- mv_data(); vars <- input$mv_efa_vars
        if (is.null(vars) || length(vars) < 3)
          return(list(ok = FALSE, error = "Sélectionnez au moins 3 variables."))
        X <- d[, vars, drop = FALSE]
        X <- X[, vapply(X, is.numeric, logical(1)), drop = FALSE]
        X[] <- lapply(X, function(x) { x[!is.finite(x)] <- NA; x })
        X <- X[stats::complete.cases(X), , drop = FALSE]
        keep <- vapply(X, function(x) stats::var(x) > 0, logical(1))
        if (sum(keep) < 3)
          return(list(ok = FALSE, error = "Moins de 3 variables numériques exploitables (variance non nulle, sans NA)."))
        if (any(!keep)) {
          shiny::showNotification(paste("Variables ignorees (variance nulle ou NA) :",
            paste(names(X)[!keep], collapse = ", ")), type = "warning", duration = 5)
          X <- X[, keep, drop = FALSE]
        }
        nf <- input$mv_efa_nf %||% 2
        if (nrow(X) < 3*ncol(X))
          return(list(ok = FALSE, error = "Effectif trop faible pour une AFE fiable."))
        R <- stats::cor(X)
        kmo <- psych::KMO(R)$MSA
        bart <- psych::cortest.bartlett(R, n = nrow(X))
        fa <- psych::fa(X, nfactors = nf, rotate = input$mv_efa_rot %||% "varimax",
                        fm = input$mv_efa_fm %||% "ml")
        load <- unclass(fa$loadings)
        var_exp <- sum(fa$Vaccounted["Proportion Var", ])
        comm <- fa$communality
        n_low <- sum(comm < .40)
        cross <- sum(apply(abs(load), 1, function(r) sum(r > .30) > 1))
        st_kmo <- if (kmo >= .80) "ok" else if (kmo >= .60) "warn" else "err"
        # Matrice de correlations singuliere -> p = NA : on signale l'echec du
        # test plutot que de faire tomber le tableau de metriques.
        st_bar <- switch(hstat_p_verdict(bart$p.value),
                         "significatif" = "ok", "non significatif" = "err", "warn")
        st_var <- if (var_exp >= .60) "ok" else if (var_exp >= .50) "warn" else "err"
        st_com <- if (n_low == 0) "ok" else "warn"
        st_cro <- if (cross == 0) "ok" else "warn"
        metrics <- rbind(
          mv_row("Indice KMO", round(kmo,3),
                 "> 0,80 très bon ; 0,60-0,80 acceptable ; < 0,60 inadequat",
                 if (st_kmo=="ok") "Données très adaptees" else if (st_kmo=="warn") "Adequation acceptable" else "Données peu factorisables",
                 st_kmo),
          mv_row("Test de Bartlett (p)", format.pval(bart$p.value, digits=3),
                 "p < 0,05 : corrélations exploitables",
                 if (st_bar=="ok") "Factorisation justifiee" else "Matrice proche de l'identite",
                 st_bar),
          mv_row("Variance cumulee expliquee", paste0(round(100*var_exp,1)," %"),
                 ">= 60 % bon ; 50-60 % acceptable ; < 50 % faible",
                 if (st_var=="ok") "Bonne restitution" else if (st_var=="warn") "Restitution acceptable" else "Restitution insuffisante",
                 st_var),
          mv_row("Communautes < 0,40", n_low,
                 "0 ideal : chaque variable communaute >= 0,40",
                 if (st_com=="ok") "Variables bien restituees" else paste0(n_low, " variable(s) mal restituee(s)"),
                 st_com),
          mv_row("Cross-loadings (|charge|>0,30)", cross,
                 "0 souhaite : structure simple",
                 if (st_cro=="ok") "Structure factorielle simple" else paste0(cross, " variable(s) ambigue(s)"),
                 st_cro))
        ld <- as.data.frame(load); ld$Variable <- rownames(load)
        ldl <- stats::reshape(ld, direction = "long",
                 varying = list(colnames(load)), v.names = "Saturation",
                 timevar = "Facteur", times = colnames(load), idvar = "Variable")
        comm_df <- data.frame(Variable = names(comm), Communaute = round(comm,3))
        render <- shiny::tagList(
          mv_card(border_color = "#1565c0",
            mv_section_header("Adequation & qualité factorielle", "#1565c0", "sliders"),
            mv_info_note("Vérification de la factorisabilite (KMO, Bartlett) et de la qualité de restitution."),
            mv_metrics_table(metrics, "#1565c0"),
            mv_interp_bar(paste0(nf, " facteurs extraits, rotation ", input$mv_efa_rot,
              ". Variance expliquee = ", round(100*var_exp,1), " %."), mv_col(st_var))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Saturations (loadings)", "#6c757d", "table"),
            mv_data_table(data.frame(Variable = rownames(load), round(load,3),
                                     check.names = FALSE), "#6c757d")),
          mv_card(border_color = "#6c757d",
            mv_section_header("Communautes", "#6c757d", "percentage"),
            mv_data_table(comm_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("AFE : ", nf, " facteurs, rotation ", input$mv_efa_rot, "."),
          summary = c("=== Analyse Factorielle Exploratoire ===",
            paste0("KMO = ", round(kmo,3), " | Bartlett p = ", format.pval(bart$p.value,digits=3)),
            "", "Saturations :", paste(utils::capture.output(round(load,3)), collapse="\n"),
            "", "Communautes :", paste(utils::capture.output(round(comm,3)), collapse="\n")),
          plotfn = function() {
            if (identical(input$mv_efa_plot %||% "heat", "path")) {
              # ---- Diagramme des variables latentes (path diagram) ----
              # Facteurs latents (ellipses, a droite) relies aux variables
              # observees (rectangles, a gauche) par des fleches dont l'epaisseur
              # est proportionnelle a |saturation|. Seuls les liens |x| >= 0,30
              # sont traces pour garder le schema lisible.
              thr <- 0.30
              vars_n <- rownames(load); facs_n <- colnames(load)
              vdf <- data.frame(name = vars_n, x = 0,
                                y = rev(seq_along(vars_n)), stringsAsFactors = FALSE)
              fy <- if (length(facs_n) == 1) mean(vdf$y) else
                seq(max(vdf$y) - 0.5, min(vdf$y) + 0.5, length.out = length(facs_n))
              fdf <- data.frame(name = facs_n, x = 1, y = fy, stringsAsFactors = FALSE)
              ed <- do.call(rbind, lapply(seq_along(facs_n), function(j) {
                lam <- load[, j]
                keep <- abs(lam) >= thr
                if (!any(keep)) return(NULL)
                data.frame(x = fdf$x[j], y = fdf$y[j],
                           xend = vdf$x[match(vars_n[keep], vdf$name)],
                           yend = vdf$y[match(vars_n[keep], vdf$name)],
                           lambda = lam[keep], stringsAsFactors = FALSE)
              }))
              g <- ggplot2::ggplot() +
                { if (!is.null(ed) && nrow(ed))
                    ggplot2::geom_segment(data = ed,
                      ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                          linewidth = abs(lambda), color = lambda > 0),
                      arrow = grid::arrow(length = grid::unit(7, "pt"),
                                          type = "closed"),
                      alpha = 0.85, lineend = "round") } +
                { if (!is.null(ed) && nrow(ed))
                    ggplot2::geom_label(data = transform(ed, xm = x + 0.62*(xend - x),
                                                ym = y + 0.62*(yend - y)),
                      ggplot2::aes(x = xm, y = ym, label = sprintf("%.2f", lambda)),
                      size = 3.1, label.size = 0, alpha = 0.85,
                      color = "#2c3e50") } +
                ggplot2::geom_label(data = vdf, ggplot2::aes(x = x, y = y, label = name),
                           hjust = 1, fill = "#eaf2f8", color = "#1a5276",
                           label.size = 0.3, size = mv_lbl_size_var(),
                           fontface = "bold",
                           label.padding = grid::unit(0.35, "lines")) +
                ggplot2::geom_point(data = fdf, ggplot2::aes(x = x, y = y), shape = 21,
                           size = 24, fill = "#fdebd0", color = "#b9770e",
                           stroke = 1) +
                ggplot2::geom_text(data = fdf, ggplot2::aes(x = x, y = y, label = name),
                          size = mv_lbl_size_var(), fontface = "bold",
                          color = "#7e5109") +
                ggplot2::scale_linewidth(range = c(0.4, 2.2), guide = "none") +
                ggplot2::scale_color_manual(values = c("TRUE" = "#1565c0", "FALSE" = "#c0392b"),
                                   labels = c("TRUE" = "positive", "FALSE" = "négative"),
                                   name = "Saturation") +
                ggplot2::scale_x_continuous(limits = c(-0.65, 1.25)) +
                labs(title = "AFE -- diagramme des variables latentes",
                     subtitle = paste0(nf, " facteurs | rotation ", input$mv_efa_rot),
                     caption = paste0("Fleches : saturations |x| >= ", format(thr, decimal.mark = ","),
                                      " ; epaisseur proportionnelle a |saturation| ; ",
                                      "bleu = positive, rouge = negative.")) +
                ggplot2::theme_void(base_size = 13) +
                ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15),
                      plot.subtitle = ggplot2::element_text(color = "#7f8c8d"),
                      plot.caption = ggplot2::element_text(color = "#7f8c8d", size = 9),
                      legend.position = "bottom",
                      plot.margin = ggplot2::margin(10, 20, 10, 20))
              return(g)
            }
            ggplot2::ggplot(ldl, ggplot2::aes(Facteur, Variable, fill = Saturation)) +
              ggplot2::geom_tile(color = "white", linewidth = .6) +
              ggplot2::geom_text(ggplot2::aes(label = round(Saturation, 2)), size = 3.4,
                        color = ifelse(abs(ldl$Saturation) > .5, "white", "#2c3e50")) +
              ggplot2::scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#1565c0",
                                   midpoint = 0, limits = c(-1, 1)) +
              labs(title = "AFE -- carte des saturations",
                   subtitle = paste0(nf, " facteurs | rotation ", input$mv_efa_rot),
                   x = "Facteur", y = "Variable",
                   caption = "Saturation |x| > 0,40 = variable bien associée au facteur.") +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics,
            Saturations = data.frame(Variable = rownames(load), round(load,4), check.names = FALSE),
            Communautes = comm_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- AFC confirmatoire ---------------------------------------------------
  shiny::observeEvent(input$mv_cfa_run, {
    mv_res[["cfa"]] <- local({
      tryCatch({
        if (!mv_has("lavaan"))
          return(list(ok = FALSE, error = "Package 'lavaan' indisponible."))
        d <- mv_data(); mdl <- input$mv_cfa_model
        if (is.null(mdl) || !nzchar(trimws(mdl)))
          return(list(ok = FALSE, error = "Renseignez la syntaxe du modèle de mesure."))
        # lavaan ne supporte pas les noms avec espaces/parentheses. On remplace
        # chaque nom de variable par un alias syntaxique (V1, V2, ...) dans le
        # modele ET dans les donnees, en commencant par les noms les plus longs.
        orig_names <- names(d)
        alias <- stats::setNames(paste0("V", seq_along(orig_names)), orig_names)
        d_safe <- d
        names(d_safe) <- unname(alias[names(d_safe)])
        mdl_safe <- mdl
        for (nm in orig_names[order(nchar(orig_names), decreasing = TRUE)]) {
          mdl_safe <- gsub(nm, alias[[nm]], mdl_safe, fixed = TRUE)
        }
        fit <- lavaan::cfa(mdl_safe, data = d_safe, estimator = input$mv_cfa_est %||% "MLR",
                           missing = "fiml")
        # ---- Garde de convergence -------------------------------------------
        # fitMeasures() echoue avec "fit measures not available if model did not
        # converge" : on verifie donc la convergence AVANT, on retente une fois
        # avec une identification par la variance des facteurs (std.lv = TRUE,
        # souvent suffisante pour les modeles a 2 indicateurs), et sinon on
        # renvoie un diagnostic actionnable plutot que l'erreur brute.
        cfa_note <- ""
        if (!isTRUE(lavaan::lavInspect(fit, "converged"))) {
          fit2 <- tryCatch(lavaan::cfa(mdl_safe, data = d_safe,
                                       estimator = input$mv_cfa_est %||% "MLR",
                                       missing = "fiml", std.lv = TRUE),
                           error = function(e) NULL)
          if (!is.null(fit2) && isTRUE(lavaan::lavInspect(fit2, "converged"))) {
            fit <- fit2
            cfa_note <- " Identification par la variance des facteurs (std.lv = TRUE) appliquée automatiquement pour obtenir la convergence."
          } else {
            # Diagnostic : nombre d'indicateurs par facteur
            pt <- tryCatch(lavaan::lavaanify(mdl_safe), error = function(e) NULL)
            small_fac <- character(0)
            if (!is.null(pt)) {
              ld <- pt[pt$op == "=~", , drop = FALSE]
              cnt <- table(ld$lhs)
              small_fac <- names(cnt)[cnt < 3]
            }
            msg <- paste0(
              "Le modèle n'a pas convergé : les indices d'ajustement ne peuvent pas être calculés. ",
              "Causes probables et pistes : ",
              if (length(small_fac))
                paste0("(1) le(s) facteur(s) ", paste(small_fac, collapse = ", "),
                       " n'ont que 1-2 indicateurs -- un facteur latent nécessite idéalement >= 3 indicateurs ; ")
              else "",
              "(2) des indicateurs quasi-colinéaires (ex. une variable qui est le ratio ou la somme d'autres indicateurs du modèle) ; ",
              "(3) des échelles très hétérogènes -- essayez de standardiser les variables ; ",
              "(4) un effectif insuffisant pour la complexité du modèle. ",
              "Reformulez le modèle (>= 3 indicateurs par facteur, indicateurs non redondants) puis relancez.")
            return(list(ok = FALSE, error = msg))
          }
        }
        fm <- lavaan::fitMeasures(fit)
        gv <- function(a,b) if (a %in% names(fm)) fm[[a]] else if (b %in% names(fm)) fm[[b]] else NA
        cfi <- gv("cfi.robust","cfi"); tli <- gv("tli.robust","tli")
        rmsea <- gv("rmsea.robust","rmsea"); srmr <- gv("srmr","srmr")
        chisq <- gv("chisq.scaled","chisq"); df <- gv("df.scaled","df")
        ratio <- if (!is.na(df) && df > 0) chisq/df else NA
        st_r <- if (!is.na(ratio) && ratio < 2) "ok" else if (!is.na(ratio) && ratio < 3) "warn" else "err"
        st_c <- if (cfi >= .95) "ok" else if (cfi >= .90) "warn" else "err"
        st_t <- if (tli >= .95) "ok" else if (tli >= .90) "warn" else "err"
        st_m <- if (rmsea < .05) "ok" else if (rmsea < .08) "warn" else "err"
        st_s <- if (srmr < .08) "ok" else "warn"
        metrics <- rbind(
          mv_row("Chi2 / ddl", if (is.na(ratio)) "n/d" else round(ratio,2),
                 "< 2 bon ; < 3 acceptable ; >= 3 mediocre",
                 if (st_r=="ok") "Tres bon ajustement" else if (st_r=="warn") "Ajustement acceptable" else "Ajustement mediocre",
                 st_r),
          mv_row("CFI", round(cfi,3),
                 "> 0,95 bon ; 0,90-0,95 acceptable ; < 0,90 insuffisant",
                 if (st_c=="ok") "Bon ajustement" else if (st_c=="warn") "Acceptable" else "Insuffisant", st_c),
          mv_row("TLI", round(tli,3),
                 "> 0,95 bon ; 0,90-0,95 acceptable ; < 0,90 insuffisant",
                 if (st_t=="ok") "Bon ajustement" else if (st_t=="warn") "Acceptable" else "Insuffisant", st_t),
          mv_row("RMSEA", round(rmsea,3),
                 "< 0,05 bon ; 0,05-0,08 acceptable ; > 0,10 mediocre",
                 if (st_m=="ok") "Erreur d'approximation faible" else if (st_m=="warn") "Erreur acceptable" else "Erreur trop élevée",
                 st_m),
          mv_row("SRMR", round(srmr,3), "< 0,08 ajustement acceptable",
                 if (st_s=="ok") "Residus standardises faibles" else "Residus élevés", st_s))
        std <- lavaan::standardizedSolution(fit)
        lt <- std[std$op == "=~", c("lhs","rhs","est.std","pvalue")]
        names(lt) <- c("Facteur","Indicateur","Charge_std","p_value")
        # Retraduire les alias (V1, V2, ...) vers les noms d'origine
        rev_alias <- stats::setNames(names(alias), unname(alias))
        lt$Indicateur <- ifelse(lt$Indicateur %in% names(rev_alias),
                                rev_alias[lt$Indicateur], lt$Indicateur)
        lt$Lien <- paste(lt$Facteur, lt$Indicateur, sep = " <- ")
        render <- shiny::tagList(
          mv_card(border_color = "#1565c0",
            mv_section_header("Indices d'ajustement du modèle", "#1565c0", "check-double"),
            mv_info_note("Le modèle de mesure pre-specifie est confronte aux seuils d'ajustement usuels."),
            mv_metrics_table(metrics, "#1565c0"),
            mv_interp_bar(paste0("Estimateur ", input$mv_cfa_est,
              ". CFI = ", round(cfi,3), " | RMSEA = ", round(rmsea,3), "."),
              mv_col(st_c))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Saturations standardisees", "#6c757d", "table"),
            mv_data_table(data.frame(Facteur = lt$Facteur, Indicateur = lt$Indicateur,
              Charge_std = round(lt$Charge_std,3), p_value = round(lt$p_value,4)), "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("AFC confirmatoire estimée.", cfa_note),
          summary = c("=== Analyse Factorielle Confirmatoire ===",
            paste(utils::capture.output(lavaan::summary(fit, fit.measures = TRUE,
                  standardized = TRUE)), collapse = "\n")),
          plotfn = function() {
            ggplot2::ggplot(lt, ggplot2::aes(stats::reorder(Lien, Charge_std), Charge_std,
                           fill = Charge_std >= .7)) +
              ggplot2::geom_col(width = .65) +
              ggplot2::geom_hline(yintercept = c(.5,.7), linetype = "dashed",
                         color = c("#f39c12","#27ae60")) +
              ggplot2::geom_text(ggplot2::aes(label = round(Charge_std,2)), hjust = -0.2, size = 3.4) +
              ggplot2::scale_fill_manual(values = c("TRUE"="#27ae60","FALSE"="#e67e22"),
                                labels = c("TRUE"=">= 0,70","FALSE"="< 0,70"),
                                name = "Charge std") +
              ggplot2::coord_flip(ylim = c(0, 1.05)) +
              labs(title = "AFC — saturations standardisées",
                   x = NULL, y = "Charge standardisee",
                   caption = "Lignes : seuils 0,50 (orange) et 0,70 (vert).") +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics,
            Saturations = data.frame(Facteur = lt$Facteur, Indicateur = lt$Indicateur,
              Charge_std = round(lt$Charge_std,4), p_value = round(lt$p_value,4))))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })


  # ---- MTMM (Multi-Trait Multi-Method, Campbell & Fiske 1959) ---------------
  shiny::observeEvent(input$mv_mtmm_run, {
    mv_res[["mtmm"]] <- local({
      tryCatch({
        d <- mv_data(); vars <- input$mv_mtmm_vars
        if (is.null(vars) || length(vars) < 4)
          return(list(ok = FALSE, error = "S\u00e9lectionnez au moins 4 variables (2 traits \u00d7 2 m\u00e9thodes)."))
        X <- d[, vars, drop = FALSE]
        X <- X[, vapply(X, is.numeric, logical(1)), drop = FALSE]
        if (ncol(X) < 4)
          return(list(ok = FALSE, error = "Au moins 4 variables num\u00e9riques sont requises."))
        X[] <- lapply(X, function(x) { x[!is.finite(x)] <- NA; x })
        X <- X[stats::complete.cases(X), , drop = FALSE]
        n <- nrow(X)
        if (n < 10)
          return(list(ok = FALSE, error = "Effectif exploitable insuffisant apr\u00e8s retrait des valeurs manquantes."))
        # ---- Affectation trait / methode de chaque variable -----------------
        all_cols <- names(d)
        if (isTRUE(input$mv_mtmm_auto %||% TRUE)) {
          sep <- input$mv_mtmm_sep %||% "_"
          asg <- t(vapply(colnames(X), function(v) .mtmm_parse_name(v, sep), character(2)))
          bad <- rownames(asg)[is.na(asg[, 1]) | is.na(asg[, 2])]
          if (length(bad))
            return(list(ok = FALSE, error = paste0(
              "Impossible de d\u00e9duire trait/m\u00e9thode pour : ", paste(bad, collapse = ", "),
              ". Attendu : Trait", sep, "M\u00e9thode (coupe sur le dernier '", sep,
              "'), ou d\u00e9cochez la d\u00e9duction automatique pour une affectation manuelle.")))
          traits <- asg[, 1]; methods <- asg[, 2]
        } else {
          traits <- vapply(colnames(X), function(v)
            trimws(input[[paste0("mv_mtmm_t_", match(v, all_cols))]] %||% ""), character(1))
          methods <- vapply(colnames(X), function(v)
            trimws(input[[paste0("mv_mtmm_m_", match(v, all_cols))]] %||% ""), character(1))
          miss <- colnames(X)[!nzchar(traits) | !nzchar(methods)]
          if (length(miss))
            return(list(ok = FALSE, error = paste0(
              "Affectation trait/m\u00e9thode manquante pour : ", paste(miss, collapse = ", "), ".")))
        }
        names(traits) <- names(methods) <- colnames(X)
        Tn <- sort(unique(traits)); Mn <- sort(unique(methods))
        if (length(Tn) < 2 || length(Mn) < 2)
          return(list(ok = FALSE, error = paste0(
            "Le MTMM requiert au moins 2 traits et 2 m\u00e9thodes (d\u00e9tect\u00e9 : ",
            length(Tn), " trait(s), ", length(Mn), " m\u00e9thode(s)).")))
        combo <- paste(traits, methods, sep = " || ")
        dup <- unique(combo[duplicated(combo)])
        if (length(dup))
          return(list(ok = FALSE, error = paste0(
            "Chaque combinaison trait \u00d7 m\u00e9thode doit correspondre \u00e0 UNE seule variable. En double : ",
            paste(gsub(" \\|\\| ", " \u00d7 ", dup), collapse = " ; "), ".")))
        # ---- Matrice MTMM ordonnee par blocs de methode ----------------------
        ord <- order(match(methods, Mn), match(traits, Tn))
        X <- X[, ord, drop = FALSE]; traits <- traits[ord]; methods <- methods[ord]
        R <- suppressWarnings(stats::cor(X, use = "pairwise.complete.obs"))
        p_of <- function(r) 2 * stats::pt(abs(r) * sqrt(n - 2) / sqrt(pmax(1e-12, 1 - r^2)),
                                          df = n - 2, lower.tail = FALSE)
        # ---- Classification des paires ---------------------------------------
        pr <- which(upper.tri(R), arr.ind = TRUE)
        cls <- data.frame(
          Var1 = colnames(R)[pr[, 1]], Var2 = colnames(R)[pr[, 2]],
          Trait1 = traits[pr[, 1]], Trait2 = traits[pr[, 2]],
          Methode1 = methods[pr[, 1]], Methode2 = methods[pr[, 2]],
          r = R[pr], stringsAsFactors = FALSE)
        cls$Zone <- ifelse(cls$Trait1 == cls$Trait2 & cls$Methode1 != cls$Methode2, "validite",
                    ifelse(cls$Trait1 != cls$Trait2 & cls$Methode1 == cls$Methode2, "HT-monoM",
                    ifelse(cls$Trait1 != cls$Trait2, "HT-heteroM", "fiabilite")))
        val  <- cls[cls$Zone == "validite", , drop = FALSE]
        htmm <- cls[cls$Zone == "HT-monoM", , drop = FALSE]
        hthm <- cls[cls$Zone == "HT-heteroM", , drop = FALSE]
        if (!nrow(val))
          return(list(ok = FALSE, error = "Aucun coefficient de validit\u00e9 : aucun trait n'est mesur\u00e9 par plusieurs m\u00e9thodes."))
        val$p <- p_of(val$r)
        mean_val <- mean(val$r); mean_htmm <- if (nrow(htmm)) mean(abs(htmm$r)) else NA
        mean_hthm <- if (nrow(hthm)) mean(abs(hthm$r)) else NA
        # ---- Criteres de Campbell & Fiske ------------------------------------
        # C1 : validites significatives et suffisamment grandes.
        c1_sig <- mean(val$p < .05) * 100
        # all() sur un vecteur contenant NA rend NA, et `if (NA && ...)` echoue :
        # une seule variable constante suffisait a faire tomber la matrice MTMM.
        val_tous_sig <- isTRUE(all(is.finite(val$p)) && all(val$p < .05))
        st_c1 <- if (val_tous_sig && isTRUE(mean_val >= .50)) "ok"
                 else if (isTRUE(mean_val >= .30)) "warn" else "err"
        # C2 : chaque validite > correlations HT-heteroM partageant une variable.
        # C3 : chaque validite > correlations HT-monoM partageant une variable.
        cmp_pct <- function(ref_df) {
          if (!nrow(ref_df)) return(NA_real_)
          tot <- 0; okc <- 0
          for (i in seq_len(nrow(val))) {
            v1 <- val$Var1[i]; v2 <- val$Var2[i]
            comp <- ref_df[ref_df$Var1 %in% c(v1, v2) | ref_df$Var2 %in% c(v1, v2), "r"]
            if (length(comp)) { tot <- tot + length(comp)
                                okc <- okc + sum(val$r[i] > abs(comp)) }
          }
          if (tot == 0) NA_real_ else 100 * okc / tot
        }
        c2 <- cmp_pct(hthm); c3 <- cmp_pct(htmm)
        st_c2 <- if (is.na(c2)) "warn" else if (c2 >= 95) "ok" else if (c2 >= 75) "warn" else "err"
        st_c3 <- if (is.na(c3)) "warn" else if (c3 >= 95) "ok" else if (c3 >= 75) "warn" else "err"
        # C4 : effet de methode = ecart HT-monoM vs HT-heteroM.
        meth_eff <- if (!is.na(mean_htmm) && !is.na(mean_hthm)) mean_htmm - mean_hthm else NA
        st_c4 <- if (is.na(meth_eff)) "warn" else if (meth_eff < .10) "ok"
                 else if (meth_eff < .25) "warn" else "err"
        fr <- function(x, d = 2) format(round(x, d), decimal.mark = ",")
        metrics <- rbind(
          mv_row("Validit\u00e9 convergente (r moyen)", fr(mean_val),
                 ">= 0,50 bon ; 0,30-0,50 mod\u00e9r\u00e9 ; < 0,30 faible",
                 if (st_c1=="ok") "Les m\u00e9thodes convergent sur les m\u00eames traits"
                 else if (st_c1=="warn") "Convergence mod\u00e9r\u00e9e" else "Convergence insuffisante", st_c1),
          mv_row("Validit\u00e9s significatives (p < 0,05)", paste0(fr(c1_sig, 0), " %"),
                 "100 % attendu (crit\u00e8re 1 de Campbell & Fiske)",
                 if (c1_sig >= 100) "Toutes les validit\u00e9s sont significatives"
                 else "Certaines validit\u00e9s ne diff\u00e8rent pas de 0",
                 if (c1_sig >= 100) "ok" else "warn"),
          mv_row("Crit\u00e8re 2 : validit\u00e9 > h\u00e9t\u00e9rotrait-h\u00e9t\u00e9rom\u00e9thode",
                 if (is.na(c2)) "n/d" else paste0(fr(c2, 0), " %"),
                 ">= 95 % bon ; 75-95 % acceptable ; < 75 % insuffisant",
                 if (st_c2=="ok") "Validit\u00e9 discriminante \u00e9tablie"
                 else if (st_c2=="warn") "Discrimination partielle" else "Traits mal discrimin\u00e9s", st_c2),
          mv_row("Crit\u00e8re 3 : validit\u00e9 > h\u00e9t\u00e9rotrait-monom\u00e9thode",
                 if (is.na(c3)) "n/d" else paste0(fr(c3, 0), " %"),
                 ">= 95 % bon ; 75-95 % acceptable ; < 75 % insuffisant",
                 if (st_c3=="ok") "Les traits dominent l'effet de m\u00e9thode"
                 else if (st_c3=="warn") "Effet de m\u00e9thode non n\u00e9gligeable" else "L'effet de m\u00e9thode domine", st_c3),
          mv_row("Effet de m\u00e9thode (|r| monoM - |r| h\u00e9t\u00e9roM)",
                 if (is.na(meth_eff)) "n/d" else fr(meth_eff),
                 "< 0,10 faible ; 0,10-0,25 mod\u00e9r\u00e9 ; >= 0,25 fort",
                 if (st_c4=="ok") "Variance de m\u00e9thode faible"
                 else if (st_c4=="warn") "Variance de m\u00e9thode mod\u00e9r\u00e9e" else "Forte variance de m\u00e9thode", st_c4))
        lvl_glob <- if ("err" %in% c(st_c1, st_c2, st_c3)) "err"
                    else if ("warn" %in% c(st_c1, st_c2, st_c3, st_c4)) "warn" else "ok"
        Rd <- as.data.frame(round(R, 3)); Rd <- cbind(Variable = rownames(R), Rd)
        asg_df <- data.frame(Variable = colnames(X), Trait = unname(traits),
                             Methode = unname(methods), stringsAsFactors = FALSE)
        val_df <- data.frame(Trait = val$Trait1,
                             Methodes = paste(val$Methode1, val$Methode2, sep = " vs "),
                             r = round(val$r, 3), p_value = round(val$p, 4))
        render <- shiny::tagList(
          mv_card(border_color = "#1565c0",
            mv_section_header("Crit\u00e8res de Campbell & Fiske", "#1565c0", "check-double"),
            mv_info_note("Validit\u00e9 convergente (m\u00eame trait, m\u00e9thodes diff\u00e9rentes) confront\u00e9e aux zones h\u00e9t\u00e9rotraits ; effet de m\u00e9thode quantifi\u00e9."),
            mv_metrics_table(metrics, "#1565c0"),
            mv_interp_bar(paste0(length(Tn), " traits \u00d7 ", length(Mn), " m\u00e9thodes | n = ", n,
              " | validit\u00e9 moyenne r = ", fr(mean_val), "."), mv_col(lvl_glob))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Coefficients de validit\u00e9 (monotrait-h\u00e9t\u00e9rom\u00e9thode)", "#6c757d", "bullseye"),
            mv_data_table(val_df, "#6c757d")),
          mv_card(border_color = "#6c757d",
            mv_section_header("Matrice MTMM (ordonn\u00e9e par blocs de m\u00e9thode)", "#6c757d", "table"),
            mv_data_table(Rd, "#6c757d")),
          mv_card(border_color = "#6c757d",
            mv_section_header("Affectations traits / m\u00e9thodes", "#6c757d", "tags"),
            mv_data_table(asg_df, "#6c757d")))
        # ---- Donnees du graphique (heatmap zonee) -----------------------------
        vn <- colnames(R)
        hm <- expand.grid(Vx = vn, Vy = vn, stringsAsFactors = FALSE)
        hm$r <- as.vector(R[cbind(match(hm$Vy, vn), match(hm$Vx, vn))])
        hm$Vx <- factor(hm$Vx, levels = vn); hm$Vy <- factor(hm$Vy, levels = rev(vn))
        is_val_cell <- function(a, b) traits[a] == traits[b] & methods[a] != methods[b]
        vcells <- hm[is_val_cell(as.character(hm$Vx), as.character(hm$Vy)), , drop = FALSE]
        blk <- data.frame(m = Mn,
          i0 = vapply(Mn, function(m) min(which(methods == m)), numeric(1)),
          i1 = vapply(Mn, function(m) max(which(methods == m)), numeric(1)))
        nv <- length(vn)
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("MTMM : ", length(Tn), " traits \u00d7 ", length(Mn),
                        " m\u00e9thodes, ", nrow(val), " coefficients de validit\u00e9."),
          summary = c("=== Multi-Trait Multi-Method (Campbell & Fiske) ===",
            paste0("Traits : ", paste(Tn, collapse = ", ")),
            paste0("M\u00e9thodes : ", paste(Mn, collapse = ", ")),
            paste0("n = ", n),
            "", "Matrice MTMM :",
            paste(utils::capture.output(round(R, 3)), collapse = "\n"),
            "", "Coefficients de validit\u00e9 :",
            paste(utils::capture.output(val_df), collapse = "\n")),
          plotfn = function() {
            g <- ggplot2::ggplot(hm, ggplot2::aes(Vx, Vy, fill = r)) +
              ggplot2::geom_tile(color = "white", linewidth = .4) +
              ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", r)), size = 3,
                        color = ifelse(abs(hm$r) > .5, "white", "#2c3e50")) +
              ggplot2::scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#1565c0",
                                   midpoint = 0, limits = c(-1, 1)) +
              # Cellules de validite (meme trait, methodes differentes) : contour dore
              ggplot2::geom_tile(data = vcells, fill = NA, color = "#f39c12", linewidth = 1.1) +
              # Blocs monomethode : contour noir epais
              ggplot2::annotate("rect",
                       xmin = blk$i0 - 0.5, xmax = blk$i1 + 0.5,
                       ymin = nv - blk$i1 + 0.5, ymax = nv - blk$i0 + 1.5,
                       fill = NA, color = "black", linewidth = 0.9) +
              labs(title = "Matrice Multi-Trait Multi-Method",
                   subtitle = paste0(length(Tn), " traits \u00d7 ", length(Mn),
                                     " m\u00e9thodes | n = ", n),
                   x = NULL, y = NULL,
                   caption = paste0("Blocs noirs = monom\u00e9thode ; cadres or\u00e9s = diagonale de validit\u00e9 ",
                                    "(m\u00eame trait, m\u00e9thodes diff\u00e9rentes).")) +
              mv_gg_theme() +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
            g
          },
          exports = list(Metriques = metrics,
            Matrice_MTMM = Rd,
            Coefficients_validite = val_df,
            Affectations = asg_df,
            Classification_paires = data.frame(cls[, c("Var1","Var2","Trait1","Trait2",
                                                       "Methode1","Methode2","Zone")],
                                               r = round(cls$r, 4))))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- PLS / PLS-DA --------------------------------------------------------
  shiny::observeEvent(input$mv_pls_run, {
    mv_res[["pls"]] <- local({
      tryCatch({
        if (!mv_has("pls"))
          return(list(ok = FALSE, error = "Package 'pls' indisponible."))
        d <- mv_data(); yv <- input$mv_pls_y; xv <- input$mv_pls_x
        if (is.null(yv) || is.null(xv) || length(xv) < 2)
          return(list(ok = FALSE, error = "Definissez Y et au moins 2 prédicteurs X."))
        sub <- d[, c(yv, xv), drop = FALSE]
        # Predicteurs numeriques uniquement, sans Inf
        num_x <- xv[vapply(xv, function(v) is.numeric(sub[[v]]), logical(1))]
        if (length(num_x) < 2)
          return(list(ok = FALSE, error = "Au moins 2 prédicteurs numériques sont requis."))
        sub[num_x] <- lapply(sub[num_x], function(x) { x[!is.finite(x)] <- NA; x })
        sub <- sub[stats::complete.cases(sub[, c(yv, num_x), drop = FALSE]), , drop = FALSE]
        # Retirer predicteurs a variance nulle
        keep <- vapply(num_x, function(v) stats::var(sub[[v]]) > 0, logical(1))
        num_x <- num_x[keep]
        if (length(num_x) < 2)
          return(list(ok = FALSE, error = "Moins de 2 prédicteurs exploitables (variance non nulle)."))
        xv <- num_x
        is_da <- !is.numeric(sub[[yv]])
        # Cap des composantes : <= nb predicteurs et marge pour la validation croisee
        max_comp <- max(1, min(length(xv), nrow(sub) - 1L))
        if (isTRUE(input$mv_pls_cv)) max_comp <- max(1, min(max_comp, nrow(sub) - 11L))
        ncomp <- max(1, min(input$mv_pls_ncomp %||% 3, max_comp))
        X <- as.matrix(sub[, xv, drop = FALSE])
        valid <- if (isTRUE(input$mv_pls_cv) && nrow(sub) >= 20) "CV" else "none"

        if (is_da) {
          yf <- factor(sub[[yv]])
          Ydum <- stats::model.matrix(~ yf - 1)
          dat <- data.frame(Y = I(Ydum), X = I(scale(X)))
          hstat_set_seed(input$globalSeed)
          fit <- pls::plsr(Y ~ X, ncomp = ncomp, data = dat, validation = valid)
          pred <- stats::predict(fit, ncomp = ncomp)
          cls <- factor(levels(yf)[apply(pred[,,1], 1, which.max)], levels = levels(yf))
          acc <- mean(cls == yf)
          st_acc <- if (acc >= .80) "ok" else if (acc >= .60) "warn" else "err"
          metrics <- rbind(
            mv_row("Taux de bon classement", paste0(round(100*acc,1)," %"),
                   ">= 80 % bon ; 60-80 % modéré ; < 60 % faible",
                   if (st_acc=="ok") "Bonne discrimination" else if (st_acc=="warn") "Discrimination modérée" else "Discrimination faible",
                   st_acc),
            mv_row("Composantes latentes", ncomp,
                   "Choisir le minimum optimisant la prédiction",
                   "Modèle PLS-DA", "info"),
            mv_row("Classes de Y", paste(levels(yf), collapse=" / "),
                   "Vérifier l'equilibre des effectifs", "Réponse catégorielle", "info"))
          sc <- as.data.frame(fit$scores[, 1:min(2,ncomp), drop = FALSE])
          if (ncol(sc) < 2) sc$Comp2 <- 0
          names(sc)[1:2] <- c("Comp1","Comp2"); sc$Classe <- yf
          sc$label <- if (!is.null(rownames(sub))) rownames(sub) else as.character(seq_len(nrow(sc)))
          plotfn <- function() {
            p <- ggplot2::ggplot(sc, ggplot2::aes(Comp1, Comp2, color = Classe)) +
              ggplot2::geom_point(size = mv_pt_size(), alpha = .8) +
              ggplot2::stat_ellipse(level = .9, linewidth = mv_ln_width()) +
              labs(title = "PLS-DA -- scores des individus",
                   subtitle = paste0(ncomp, " composantes | ", nlevels(yf), " classes"),
                   x = "Composante 1", y = "Composante 2") +
              mv_gg_theme() + mv_color_scale(discrete = TRUE)
            mv_add_labels(p, sc, "Comp1", "Comp2", "label")
          }
          exports <- list(Metriques = metrics,
            Predictions = data.frame(Observe = yf, Predit = cls))
          note <- paste0("PLS-DA : ", ncomp, " composantes, ", nlevels(yf), " classes.")
          extra_card <- NULL
        } else {
          y <- sub[[yv]]
          dat <- data.frame(Y = y, X = I(scale(X)))
          hstat_set_seed(input$globalSeed)
          fit <- pls::plsr(Y ~ X, ncomp = ncomp, data = dat, validation = valid)
          r2y <- pls::R2(fit)$val[1,1,ncomp+1]
          q2 <- if (valid == "CV") {
            press <- fit$validation$PRESS[1, ncomp]
            1 - press/sum((y-mean(y))^2)
          } else NA
          st_r2 <- if (!is.na(r2y) && r2y >= .5) "ok" else if (!is.na(r2y) && r2y >= .3) "warn" else "err"
          st_q2 <- if (is.na(q2)) "info" else if (q2 >= .5) "ok" else if (q2 >= 0) "warn" else "err"
          gap <- if (!is.na(q2)) abs(r2y-q2) else NA
          st_g <- if (is.na(gap)) "info" else if (gap < .3) "ok" else "warn"
          metrics <- rbind(
            mv_row("R2Y (variance Y expliquee)", round(r2y,3),
                   ">= 0,50 bon ; 0,30-0,50 modéré ; < 0,30 faible",
                   if (st_r2=="ok") "Bonne explication" else if (st_r2=="warn") "Explication modérée" else "Explication faible",
                   st_r2),
            mv_row("Q2 (predictivite, CV)", if (is.na(q2)) "n/d" else round(q2,3),
                   "> 0,50 bon ; > 0 acceptable ; < 0 nul",
                   if (st_q2=="ok") "Bon pouvoir prédictif" else if (st_q2=="warn") "Predictivite limitee"
                     else if (st_q2=="err") "Aucun pouvoir prédictif" else "Validation croisée desactivee",
                   st_q2),
            mv_row("Écart R2Y - Q2", if (is.na(gap)) "n/d" else round(gap,3),
                   "< 0,30 : pas de sur-ajustement",
                   if (st_g=="ok") "Modèle non sur-ajuste" else if (st_g=="warn") "Risque de sur-ajustement" else "Validation croisée desactivee",
                   st_g),
            mv_row("Composantes latentes", ncomp,
                   "Choisir le minimum optimisant Q2", "Modèle PLS", "info"))
          pr <- stats::predict(fit, ncomp = ncomp)[,1,1]
          pdf <- data.frame(Observe = y, Predit = pr)
          plotfn <- function() {
            ggplot2::ggplot(pdf, ggplot2::aes(Observe, Predit)) +
              ggplot2::geom_point(size = mv_pt_size(), alpha = .75, color = "#1565c0") +
              ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#c0392b") +
              labs(title = "PLS — valeurs prédites vs observées",
                   subtitle = paste0("R2Y = ", round(r2y,3),
                     if (!is.na(q2)) paste0(" | Q2 = ", round(q2,3)) else ""),
                   x = "Y observé", y = "Y predit") +
              mv_gg_theme()
          }
          exports <- list(Metriques = metrics)
          note <- paste0("PLS : ", ncomp, " composantes, réponse continue.")
          extra_card <- NULL
        }
        vip <- tryCatch({
          W <- fit$loading.weights[, 1:ncomp, drop = FALSE]
          SS <- colSums(fit$Yloadings[, 1:ncomp, drop = FALSE]^2) *
                colSums(fit$scores[, 1:ncomp, drop = FALSE]^2)
          sqrt(nrow(W) * rowSums(sweep(W^2, 2, SS, "*")) / sum(SS))
        }, error = function(e) NULL)
        if (!is.null(vip)) {
          n_imp <- sum(vip > 1)
          metrics <- rbind(metrics,
            mv_row("Variables VIP > 1", n_imp,
                   "VIP > 1 : variable importante ; 0,8-1 zone grise",
                   paste0(n_imp, " prédicteur(s) influent(s)"), "info"))
          exports$VIP <- data.frame(Variable = names(vip), VIP = round(vip,4))
          extra_card <- mv_card(border_color = "#6c757d",
            mv_section_header("Importance des variables (VIP)", "#6c757d", "ranking-star"),
            mv_data_table(data.frame(Variable = names(vip), VIP = round(vip,3)), "#6c757d"))
        }
        render <- shiny::tagList(
          mv_card(border_color = "#1565c0",
            mv_section_header("Qualite du modèle PLS", "#1565c0", "diagram-project"),
            mv_info_note("Pouvoir explicatif (R2Y) et prédictif (Q2) du modèle a composantes latentes."),
            mv_metrics_table(metrics, "#1565c0"),
            mv_interp_bar(note, "#1565c0")),
          extra_card)
        list(ok = TRUE, render = render, metrics = metrics, note = note,
          summary = c("=== Regression PLS ===",
            paste(utils::capture.output(summary(fit)), collapse="\n")),
          plotfn = plotfn, exports = exports)
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- Regression lineaire multiple ----------------------------------------
  shiny::observeEvent(input$mv_regmult_run, {
    mv_res[["regmult"]] <- local({
      tryCatch({
        d <- mv_data(); yv <- input$mv_regmult_y; xv <- input$mv_regmult_x
        if (is.null(yv) || is.null(xv) || length(xv) < 1)
          return(list(ok = FALSE, error = "Definissez Y et au moins 1 prédicteur X."))
        sub <- d[, c(yv, xv), drop = FALSE]
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        fml <- stats::as.formula(paste0("`", yv, "` ~ ",
          paste0("`", xv, "`", collapse = " + ")))
        fit <- stats::lm(fml, data = sub)
        s <- summary(fit)
        r2a <- s$adj.r.squared
        fs <- s$fstatistic
        fp <- stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
        vif_max <- tryCatch(if (length(xv) >= 2) max(car::vif(fit)) else 1,
                            error = function(e) NA)
        bp <- tryCatch(lmtest::bptest(fit)$p.value, error = function(e) NA)
        sw <- tryCatch(hstat_shapiro(stats::residuals(fit))$p.value,
                       error = function(e) NA)
        dw <- tryCatch(lmtest::dwtest(fit)$statistic, error = function(e) NA)
        st_r2 <- if (r2a >= .5) "ok" else if (r2a >= .3) "warn" else "err"
        st_f  <- if (fp < .05) "ok" else "err"
        st_v  <- if (is.na(vif_max)) "info" else if (vif_max < 5) "ok" else if (vif_max < 10) "warn" else "err"
        st_bp <- if (is.na(bp)) "info" else if (bp > .05) "ok" else "warn"
        st_sw <- if (is.na(sw)) "info" else if (sw > .05) "ok" else "warn"
        st_dw <- if (is.na(dw)) "info" else if (dw > 1.5 && dw < 2.5) "ok" else "warn"
        metrics <- rbind(
          mv_row("R2 ajuste", round(r2a,3),
                 "Plus élevé = meilleur (>= 0,50 bon, selon domaine)",
                 if (st_r2=="ok") "Bon pouvoir explicatif" else if (st_r2=="warn") "Pouvoir explicatif modéré" else "Pouvoir explicatif faible",
                 st_r2),
          mv_row("Test F global (p)", format.pval(fp, digits=3),
                 "p < 0,05 : modèle globalement significatif",
                 if (st_f=="ok") "Modèle significatif" else "Modèle non significatif", st_f),
          mv_row("VIF maximal", if (is.na(vif_max)) "n/d" else round(vif_max,2),
                 "< 5 OK ; 5-10 a surveiller ; > 10 multicolinéarité forte",
                 if (st_v=="ok") "Pas de multicolinéarité" else if (st_v=="warn") "Multicolinéarité modérée"
                   else if (st_v=="err") "Multicolinéarité problematique" else "Non applicable (1 prédicteur)",
                 st_v),
          mv_row("Breusch-Pagan (p)", if (is.na(bp)) "n/d" else format.pval(bp,digits=3),
                 "p > 0,05 : homoscedasticite respectee",
                 if (st_bp=="ok") "Variance des residus constante" else "Heteroscedasticite détectée",
                 st_bp),
          mv_row("Shapiro-Wilk residus (p)", if (is.na(sw)) "n/d" else format.pval(sw,digits=3),
                 "p > 0,05 : normalité des residus",
                 if (st_sw=="ok") "Residus normaux" else "Écart a la normalité", st_sw),
          mv_row("Durbin-Watson", if (is.na(dw)) "n/d" else round(dw,2),
                 "Proche de 2 : indépendance des residus",
                 if (st_dw=="ok") "Residus independants" else "Autocorrelation possible", st_dw))
        coefs <- as.data.frame(s$coefficients)
        coef_df <- data.frame(Terme = rownames(coefs),
          Estimation = round(coefs[,1],4), Err_std = round(coefs[,2],4),
          t = round(coefs[,3],3), p_value = round(coefs[,4],4))
        res_df <- data.frame(Ajuste = stats::fitted(fit), Residu = stats::residuals(fit))
        render <- shiny::tagList(
          mv_card(border_color = "#1565c0",
            mv_section_header("Qualite d'ajustement & validite", "#1565c0", "chart-line"),
            mv_info_note("Pouvoir explicatif du modèle et vérification des hypotheses sur les residus."),
            mv_metrics_table(metrics, "#1565c0"),
            mv_interp_bar(paste0("R2 ajuste = ", round(r2a,3),
              ". Modèle ", if (fp < .05) "globalement significatif." else "non significatif."),
              mv_col(st_r2))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Coefficients estimes", "#6c757d", "table"),
            mv_data_table(coef_df, "#6c757d", digits = 4)))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("Regression linéaire : R2 ajuste = ", round(r2a,3), "."),
          summary = c("=== Regression linéaire multiple ===",
            paste(utils::capture.output(s), collapse = "\n")),
          plotfn = function() {
            ggplot2::ggplot(res_df, ggplot2::aes(Ajuste, Residu)) +
              ggplot2::geom_point(size = mv_pt_size(), alpha = .7, color = "#1565c0") +
              ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#c0392b") +
              ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#f39c12", linewidth = .8) +
              labs(title = "Régression — résidus vs valeurs ajustées",
                   subtitle = "Un nuage sans tendance confirme l'homoscedasticite",
                   x = "Valeurs ajustees", y = "Residus") +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics, Coefficients = coef_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })


  # ====================== MOTEURS D'ANALYSE (QUALI) ==========================

  # ---- AFC (correspondances) ----------------------------------------------
  shiny::observeEvent(input$mv_afc_run, {
    mv_res[["afc"]] <- local({
      tryCatch({
        d <- mv_data(); rv <- input$mv_afc_row; cv <- input$mv_afc_col
        if (is.null(rv) || is.null(cv) || rv == cv)
          return(list(ok = FALSE, error = "Choisissez deux variables qualitatives differentes."))
        tab <- table(d[[rv]], d[[cv]])
        tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
        if (nrow(tab) < 2 || ncol(tab) < 2)
          return(list(ok = FALSE, error = "Table de contingence insuffisante (>=2x2 requise)."))
        chi <- suppressWarnings(stats::chisq.test(tab))
        ca <- FactoMineR::CA(tab, graph = FALSE)
        eig <- ca$eig
        inertia <- sum(eig[,1])
        cramer <- sqrt(chi$statistic / (sum(tab) * (min(dim(tab)) - 1)))
        exp_low <- mean(chi$expected < 5) * 100
        dim12 <- sum(eig[1:min(2,nrow(eig)),2])
        # Table de contingence degeneree -> p = NA.
        st_chi <- switch(hstat_p_verdict(chi$p.value),
                         "significatif" = "ok", "non significatif" = "err", "warn")
        st_exp <- if (exp_low <= 20) "ok" else "warn"
        st_dim <- if (dim12 >= 70) "ok" else if (dim12 >= 50) "warn" else "err"
        st_cr  <- if (cramer >= .3) "ok" else if (cramer >= .1) "warn" else "err"
        metrics <- rbind(
          mv_row("Test du Chi2 (p)", format.pval(chi$p.value, digits=3),
                 "p < 0,05 : association significative",
                 if (st_chi=="ok") "Association significative" else "Pas d'association détectée",
                 st_chi),
          mv_row("Inertie totale (Chi2/n)", round(inertia,4),
                 "Mesure l'intensité globale d'association",
                 "Intensite de l'association lignes/colonnes", "info"),
          mv_row("V de Cramér", round(cramer,3),
                 ">= 0,30 forte ; 0,10-0,30 modérée ; < 0,10 faible",
                 if (st_cr=="ok") "Association forte" else if (st_cr=="warn") "Association modérée" else "Association faible",
                 st_cr),
          mv_row("Inertie axes 1-2", paste0(round(dim12,1)," %"),
                 ">= 70 % bonne restitution ; 50-70 % acceptable",
                 if (st_dim=="ok") "Plan factoriel representatif" else if (st_dim=="warn") "Restitution acceptable" else "Restitution limitee",
                 st_dim),
          mv_row("Cases effectif théorique < 5", paste0(round(exp_low,1)," %"),
                 "<= 20 % conseille pour la validite du Chi2",
                 if (st_exp=="ok") "Validite du Chi2 satisfaisante" else "Trop de cases a faible effectif",
                 st_exp))
        row_co <- hstat_coord_mat(ca$row$coord); col_co <- hstat_coord_mat(ca$col$coord)
        rc <- as.data.frame(row_co[, 1:min(2, ncol(row_co)), drop = FALSE])
        if (ncol(rc) < 2) rc$D2 <- 0
        names(rc)[1:2] <- c("Dim1","Dim2"); rc$Label <- rownames(row_co); rc$Type <- "Ligne"
        cc <- as.data.frame(col_co[, 1:min(2, ncol(col_co)), drop = FALSE])
        if (ncol(cc) < 2) cc$D2 <- 0
        names(cc)[1:2] <- c("Dim1","Dim2"); cc$Label <- rownames(col_co); cc$Type <- "Colonne"
        biplot <- rbind(rc[,c("Dim1","Dim2","Label","Type")], cc[,c("Dim1","Dim2","Label","Type")])
        eig_df <- data.frame(Axe = rownames(eig), Valeur_propre = round(eig[,1],4),
                             Variance = round(eig[,2],2), Cumul = round(eig[,3],2))
        render <- shiny::tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Association & qualité factorielle", "#6a1b9a", "shapes"),
            mv_info_note("Significativite et intensité de l'association entre les deux variables qualitatives."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(paste0("Table ", nrow(tab), "x", ncol(tab),
              ". V de Cramér = ", round(cramer,3), "."), mv_col(st_cr))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Valeurs propres par axe", "#6c757d", "layer-group"),
            mv_data_table(eig_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("AFC : table ", nrow(tab), "x", ncol(tab), "."),
          summary = c("=== Analyse Factorielle des Correspondances ===",
            paste0("Variables : ", rv, " (lignes) x ", cv, " (colonnes)"),
            "", "Valeurs propres :", paste(utils::capture.output(round(eig,4)), collapse="\n")),
          plotfn = function() {
            ggplot2::ggplot(biplot, ggplot2::aes(Dim1, Dim2, color = Type, label = Label)) +
              ggplot2::geom_hline(yintercept = 0, color = "#bbb", linewidth = .4) +
              ggplot2::geom_vline(xintercept = 0, color = "#bbb", linewidth = .4) +
              ggplot2::geom_point(ggplot2::aes(shape = Type), size = mv_pt_size()) +
              # Lignes = individus, colonnes = variables : chaque famille suit
              # son propre reglage de taille de label (en points).
              ggplot2::geom_text(ggplot2::aes(size = Type), vjust = -0.8, show.legend = FALSE) +
              ggplot2::scale_size_manual(values = c("Ligne" = mv_lbl_size(),
                                           "Colonne" = mv_lbl_size_var()),
                                guide = "none") +
              ggplot2::scale_color_manual(values = c("Ligne"="#6a1b9a","Colonne"="#e67e22")) +
              labs(title = paste0("AFC : ", rv, " x ", cv),
                   subtitle = paste0("Inertie axes 1-2 = ", round(dim12,1), " %"),
                   x = paste0("Dim 1 (", round(eig[1,2],1), " %)"),
                   y = paste0("Dim 2 (", round(eig[min(2,nrow(eig)),2],1), " %)")) +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics, Valeurs_propres = eig_df,
            Table = as.data.frame.matrix(tab)))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- ACM -----------------------------------------------------------------
  shiny::observeEvent(input$mv_mca_run, {
    mv_res[["mca"]] <- local({
      tryCatch({
        d <- mv_data(); vars <- input$mv_mca_vars
        if (is.null(vars) || length(vars) < 2)
          return(list(ok = FALSE, error = "Sélectionnez au moins 2 variables qualitatives."))
        # Éléments supplémentaires (optionnels)
        quali_sup <- setdiff(intersect(input$mv_mca_quali_sup %||% character(0), names(d)), vars)
        quanti_sup <- setdiff(intersect(input$mv_mca_quanti_sup %||% character(0), names(d)), c(vars, quali_sup))
        all_cols <- c(vars, quali_sup, quanti_sup)
        sub <- d[, all_cols, drop = FALSE]
        for (v in c(vars, quali_sup)) sub[[v]] <- droplevels(factor(sub[[v]]))
        # Lignes complètes sur les variables ACTIVES uniquement (les sup peuvent être NA)
        keep_rows <- stats::complete.cases(sub[, vars, drop = FALSE])
        sub <- sub[keep_rows, , drop = FALSE]
        ls <- input$mv_mca_labelsource %||% "rownames"
        if (!identical(ls, "rownames") && ls %in% names(d)) {
          rownames(sub) <- make.unique(as.character(d[[ls]][keep_rows]))
        }
        ind_sup <- mv_parse_ind_sup(input$mv_mca_ind_sup, nrow(sub))
        # Positions des colonnes supplémentaires dans 'sub'
        quali_sup_idx <- if (length(quali_sup)) match(quali_sup, names(sub)) else NULL
        quanti_sup_idx <- if (length(quanti_sup)) match(quanti_sup, names(sub)) else NULL
        n_active_mod <- sum(sapply(sub[, vars, drop = FALSE], nlevels))
        ncp <- min(input$mv_mca_ncp %||% 5, n_active_mod - length(vars))
        mca <- FactoMineR::MCA(sub, ncp = ncp, graph = FALSE,
                               quali.sup = quali_sup_idx, quanti.sup = quanti_sup_idx,
                               ind.sup = if (length(ind_sup)) ind_sup else NULL)
        eig <- mca$eig
        n_mod <- sum(sapply(sub[, vars, drop = FALSE], nlevels)); Q <- length(vars)
        raw <- eig[,1]; adj <- raw[raw > 1/Q]
        adj_b <- (Q/(Q-1))^2 * (adj - 1/Q)^2
        adj_pct <- 100 * adj_b / sum(adj_b)
        dim12_raw <- sum(eig[1:min(2,nrow(eig)),2])
        dim12_adj <- if (length(adj_pct) >= 2) sum(adj_pct[1:2]) else sum(adj_pct)
        seuil_ctr <- 100 / n_mod
        st_dim <- if (dim12_adj >= 70) "ok" else if (dim12_adj >= 50) "warn" else "err"
        metrics <- rbind(
          mv_row("Inertie brute axes 1-2", paste0(round(dim12_raw,1)," %"),
                 "Sous-estimé la structure (a corriger)",
                 "Valeur brute -- privilegier l'inertie ajustee", "info"),
          mv_row("Inertie ajustee axes 1-2 (Benzecri)", paste0(round(dim12_adj,1)," %"),
                 ">= 70 % bonne restitution ; 50-70 % acceptable",
                 if (st_dim=="ok") "Plan factoriel representatif" else if (st_dim=="warn") "Restitution acceptable" else "Restitution limitee",
                 st_dim),
          mv_row("Nombre de modalités", n_mod,
                 "Regrouper les modalités rares (< 5 %)",
                 "Total des modalités actives", "info"),
          mv_row("Seuil de contribution moyen", paste0(round(seuil_ctr,2)," %"),
                 "Une modalité contribue si CTR > 100/nb modalités",
                 "Référence pour reperer les modalités structurantes", "info"))
        vc <- as.data.frame(hstat_coord_mat(mca$var$coord)[, 1:min(2, ncol(hstat_coord_mat(mca$var$coord))), drop = FALSE])
        if (ncol(vc) < 2) vc$D2 <- 0
        names(vc)[1:2] <- c("Dim1","Dim2"); vc$Modalite <- rownames(mca$var$coord)
        eig_df <- data.frame(Axe = rownames(eig), Valeur_propre = round(eig[,1],4),
                             Variance = round(eig[,2],2), Cumul = round(eig[,3],2))
        render <- shiny::tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Structure factorielle", "#6a1b9a", "shapes"),
            mv_info_note("L'inertie ajustee de Benzecri corrige la sous-estimation de l'inertie brute en ACM."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(paste0(length(vars), " variables, ", n_mod,
              " modalités. Inertie ajustee axes 1-2 = ", round(dim12_adj,1), " %."),
              mv_col(st_dim))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Valeurs propres par axe", "#6c757d", "layer-group"),
            mv_data_table(eig_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("ACM : ", length(vars), " variables, ", n_mod, " modalités."),
          summary = c("=== Analyse des Correspondances Multiples ===",
            "Valeurs propres :", paste(utils::capture.output(round(eig,4)), collapse="\n")),
          plotfn = function() {
            sub_t <- paste0("Inertie ajustee axes 1-2 = ", round(dim12_adj,1), " %")
            pt <- input$mv_mca_plottype %||% "biplot"
            # Coloration par groupe (variable qualitative) facon 'explor'
            gvar <- input$mv_mca_groupvar %||% "__none__"
            grp_vals <- NULL
            if (!identical(gvar, "__none__") && gvar %in% names(d)) {
              grp_vals <- as.character(d[[gvar]][keep_rows])
              if (length(ind_sup)) grp_vals <- grp_vals[-ind_sup]
            }
            pp <- mv_factor_viz(mca, "mca", pt, input$mv_mca_colorby,
                                "ACM -- analyse des correspondances multiples", sub_t,
                                ptsz = input$mv_mca_ptsz %||% mv_pt_size(),
                                arrsz = input$mv_mca_arrsz %||% mv_ln_width(),
                                lblsz = hstat_lbl_pt2gg(input$mv_mca_lblsz %||% mv_lbl_pt_ind()),
                                lblszvar = hstat_lbl_pt2gg(input$mv_mca_lblszvar %||% mv_lbl_pt_var()),
                                show_lab = isTRUE(input$mv_mca_showlab),
                                axes = c(as.integer(input$mv_mca_axisx %||% 1), as.integer(input$mv_mca_axisy %||% 2)),
                                group_values = grp_vals)
            if (!is.null(pp)) {
              # Ellipses de concentration autour des individus (optionnel)
              if (pt %in% c("ind", "biplot")) {
                gv_name <- input$mv_mca_ellipsevar %||% "__none__"
                gv <- NULL
                if (!identical(gv_name, "__none__") && gv_name %in% names(d)) {
                  gv <- as.character(d[[gv_name]][keep_rows])
                  if (length(ind_sup)) gv <- gv[-ind_sup]
                }
                pp <- mv_add_ind_ellipses(pp, mca, "mv_mca", gv)
              }
              # Modalités supplémentaires en vert (sur var/biplot)
              if (!is.null(mca$quali.sup) && pt %in% c("var","biplot")) {
                qs_co <- hstat_coord_mat(mca$quali.sup$coord)
                qs <- as.data.frame(qs_co[, 1:min(2, ncol(qs_co)), drop = FALSE])
                names(qs) <- c("Dim1","Dim2"); qs$lab <- rownames(mca$quali.sup$coord)
                pp <- pp + ggplot2::geom_point(data = qs, ggplot2::aes(Dim1, Dim2),
                             inherit.aes = FALSE, color = "#16a085", shape = 17,
                             size = mv_pt_size() + 0.6) +
                  ggplot2::geom_text(data = qs, ggplot2::aes(Dim1, Dim2, label = lab),
                             inherit.aes = FALSE, color = "#16a085", vjust = -0.8,
                             size = hstat_lbl_pt2gg(input$mv_mca_lblszvar %||% mv_lbl_pt_var()))
              }
              return(pp)
            }
            # Repli sans factoextra : plan des modalités
            ggplot2::ggplot(vc, ggplot2::aes(Dim1, Dim2, label = Modalite)) +
              ggplot2::geom_hline(yintercept = 0, color = "#bbb", linewidth = .4) +
              ggplot2::geom_vline(xintercept = 0, color = "#bbb", linewidth = .4) +
              ggplot2::geom_point(size = mv_pt_size(), color = "#6a1b9a") +
              ggplot2::geom_text(vjust = -0.8, color = "#2c3e50",
                        size = hstat_lbl_pt2gg(input$mv_mca_lblszvar %||% mv_lbl_pt_var())) +
              labs(title = "ACM -- plan des modalités", subtitle = sub_t,
                   x = paste0("Dim 1 (", round(eig[1,2],1), " %)"),
                   y = paste0("Dim 2 (", round(eig[min(2,nrow(eig)),2],1), " %)")) +
              mv_gg_theme()
          },
          exports = list(Metriques = metrics, Valeurs_propres = eig_df,
            Modalites = data.frame(Modalite = rownames(mca$var$contrib),
                                   round(mca$var$contrib,3), check.names = FALSE)))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- k-modes -------------------------------------------------------------
  shiny::observeEvent(input$mv_kmodes_run, {
    mv_res[["kmodes"]] <- local({
      tryCatch({
        if (!mv_has("klaR"))
          return(list(ok = FALSE,
                      error = hstat_pkg_manquant("klaR", "Classification k-modes")))
        d <- mv_data(); vars <- input$mv_kmodes_vars
        if (is.null(vars) || length(vars) < 2)
          return(list(ok = FALSE, error = "Sélectionnez au moins 2 variables qualitatives."))
        k <- input$mv_kmodes_k %||% 3
        sub <- d[, vars, drop = FALSE]
        for (v in vars) sub[[v]] <- factor(sub[[v]])
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        hstat_set_seed(input$globalSeed)
        sub <- mv_subsample(sub, 8000)
        km <- klaR::kmodes(sub, modes = k, iter.max = input$mv_kmodes_iter %||% 20)
        sizes <- km$size
        # Calcul sorti dans Utils.R : klaR est absent de beaucoup de machines,
        # la qualite de la partition doit rester testable sans lui.
        q <- hstat_kmodes_pseudo_r2(km$withindiff, sub)
        tot_diff <- q$dissimilarite_intra
        pr2 <- q$pseudo_r2
        st_r2 <- q$verdict
        st_bal <- hstat_part_equilibre(sizes)$verdict
        metrics <- rbind(
          mv_row("Dissimilarite intra totale", tot_diff,
                 "Plus faible = clusters plus homogenes",
                 "A minimiser entre solutions", "info"),
          mv_row("Pseudo-R2 (separation)", if (is.na(pr2)) "n/d" else round(pr2,3),
                 ">= 0,50 nette ; 0,30-0,50 modérée ; < 0,30 faible",
                 switch(st_r2, ok = "Partition nette", warn = "Partition modérée",
                        err = "Partition peu séparée",
                        "Non calculable : les variables retenues ne varient pas"),
                 st_r2),
          mv_row("Equilibre des clusters", paste(sizes, collapse=" / "),
                 "Eviter clusters vides ou très minoritaires",
                 switch(st_bal, ok = "Répartition acceptable",
                        warn = "Cluster très minoritaire", "Effectifs non calculables"),
                 st_bal),
          mv_row("Nombre de clusters", k,
                 "Fixe a priori -- comparer plusieurs k",
                 "Paramètre du partitionnement", "info"))
        sz_df <- data.frame(Cluster = factor(seq_len(k)), Effectif = sizes)
        render <- shiny::tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Qualite de la partition", "#6a1b9a", "object-group"),
            mv_info_note("Separation des clusters catégoriels evaluee par le pseudo-R2."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(paste0("Solution a ", k, " clusters sur ", nrow(sub), " individus."),
              mv_col(st_r2))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Modes (centres) des clusters", "#6c757d", "crosshairs"),
            mv_data_table(data.frame(Cluster = seq_len(k), km$modes, check.names = FALSE), "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("k-modes : ", k, " clusters sur ", nrow(sub), " individus."),
          summary = c("=== Classification k-modes ===",
            paste0("Variables : ", paste(vars, collapse=", ")),
            "", "Modes :", paste(utils::capture.output(km$modes), collapse="\n")),
          plotfn = function() {
            df_plot <- sz_df
            df_plot$Cluster <- factor(df_plot$Cluster)
            df_plot$Effectif <- as.numeric(df_plot$Effectif)
            ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[["Cluster"]], y = .data[["Effectif"]],
                                fill = .data[["Cluster"]], text = paste0("Cluster ", .data[["Cluster"]], " : ", .data[["Effectif"]]))) +
              ggplot2::geom_col(width = .65) +
              ggplot2::geom_text(ggplot2::aes(label = .data[["Effectif"]]), vjust = -0.5, size = 3.6, fontface = "bold") +
              labs(title = "k-modes -- effectifs par cluster",
                   subtitle = paste0(k, " clusters"),
                   x = "Cluster", y = "Nombre d'individus") +
              mv_gg_theme() + ggplot2::theme(legend.position = "none")
          },
          exports = list(Metriques = metrics,
            Modes = data.frame(Cluster = seq_len(k), km$modes, check.names = FALSE),
            Effectifs = sz_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })


  # ---- LCA -----------------------------------------------------------------
  shiny::observeEvent(input$mv_lca_run, {
    mv_res[["lca"]] <- local({
      tryCatch({
        if (!mv_has("poLCA"))
          return(list(ok = FALSE,
                      error = hstat_pkg_manquant("poLCA", "Analyse en classes latentes")))
        d <- mv_data(); vars <- input$mv_lca_vars
        if (is.null(vars) || length(vars) < 3)
          return(list(ok = FALSE, error = "Sélectionnez au moins 3 indicateurs catégoriels."))
        sub <- d[, vars, drop = FALSE]
        for (v in vars) sub[[v]] <- as.integer(factor(sub[[v]]))
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        names(sub) <- vars
        nclass <- input$mv_lca_nclass %||% 2
        fml <- stats::as.formula(paste0("cbind(",
          paste0("`", vars, "`", collapse = ","), ") ~ 1"))
        hstat_set_seed(input$globalSeed)
        fit <- poLCA::poLCA(fml, data = sub, nclass = nclass,
                            nrep = input$mv_lca_rep %||% 5, verbose = FALSE)
        post <- fit$posterior
        e <- hstat_lca_entropie(post)
        ent_rel <- e$entropie_relative
        st_ent <- e$verdict
        eq <- hstat_part_equilibre(table(fit$predclass))
        min_sz <- eq$part_min
        st_sz  <- eq$verdict
        metrics <- rbind(
          mv_row("BIC", round(fit$bic,1),
                 "Plus bas = meilleur (comparer entre nb de classes)",
                 "Critere de sélection du nombre de classes", "info"),
          mv_row("AIC", round(fit$aic,1),
                 "Plus bas = meilleur (penalise moins que le BIC)",
                 "Critere complementaire de sélection", "info"),
          mv_row("Entropie relative", round(ent_rel,3),
                 ">= 0,80 bonne separation ; 0,60-0,80 modérée ; < 0,60 faible",
                 switch(st_ent, ok = "Classes bien séparées", warn = "Separation modérée",
                        err = "Classes mal séparées",
                        "Non calculable : une seule classe estimée"),
                 st_ent),
          mv_row("G2 (rapport de vraisemblance)", round(fit$Gsq,1),
                 "Plus faible = meilleur ajustement du modèle",
                 "Qualite d'ajustement", "info"),
          mv_row("Plus petite classe", paste0(round(100*min_sz,1)," %"),
                 ">= 5 % conseille pour une classe interpretable",
                 switch(st_sz, ok = "Classes de taille suffisante",
                        warn = "Classe très minoritaire", "Effectifs non calculables"),
                 st_sz))
        cl_df <- as.data.frame(table(Classe = fit$predclass))
        cl_df$Classe <- factor(cl_df$Classe)
        render <- shiny::tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Sélection & qualité du modèle", "#6a1b9a", "shapes"),
            mv_info_note("Criteres d'information (AIC/BIC) et separation des classes latentes (entropie)."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(paste0(nclass, " classes latentes. Entropie = ", round(ent_rel,3), "."),
              mv_col(st_ent))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Effectifs par classe latente", "#6c757d", "users"),
            mv_data_table(cl_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("LCA : ", nclass, " classes latentes."),
          summary = c("=== Analyse en Classes Latentes ===",
            paste(utils::capture.output(print(fit)), collapse = "\n")),
          plotfn = function() {
            ggplot2::ggplot(cl_df, ggplot2::aes(x = .data[["Classe"]], y = .data[["Freq"]], fill = .data[["Classe"]])) +
              ggplot2::geom_col(width = .65) +
              ggplot2::geom_text(ggplot2::aes(label = .data[["Freq"]]), vjust = -0.5, size = 3.6, fontface = "bold") +
              labs(title = "LCA -- effectifs par classe latente",
                   subtitle = paste0(nclass, " classes | entropie = ", round(ent_rel,3)),
                   x = "Classe latente", y = "Nombre d'individus") +
              mv_gg_theme() + ggplot2::theme(legend.position = "none")
          },
          exports = list(Metriques = metrics, Effectifs = cl_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- Regression logistique ----------------------------------------------
  shiny::observeEvent(input$mv_logit_run, {
    mv_res[["logit"]] <- local({
      tryCatch({
        d <- mv_data(); yv <- input$mv_logit_y; xv <- input$mv_logit_x
        if (is.null(yv) || is.null(xv) || length(xv) < 1)
          return(list(ok = FALSE, error = "Definissez Y et au moins 1 prédicteur X."))
        sub <- d[, c(yv, xv), drop = FALSE]
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        sub[[yv]] <- droplevels(factor(sub[[yv]]))
        nlev <- nlevels(sub[[yv]])
        if (nlev < 2)
          return(list(ok = FALSE, error = "La réponse doit avoir au moins 2 modalités."))
        fml <- stats::as.formula(paste0("`", yv, "` ~ ",
          paste0("`", xv, "`", collapse = " + ")))

        if (nlev == 2) {
          gl  <- hstat_glm_fit(fml, data = sub, family = stats::binomial())
          fit <- gl$fit
          ll0 <- hstat_glm_fit(stats::as.formula(paste0("`",yv,"` ~ 1")),
                               data = sub, family = stats::binomial())$fit
          mcf <- as.numeric(1 - stats::logLik(fit)/stats::logLik(ll0))
          pp <- stats::predict(fit, type = "response")
          pc <- factor(ifelse(pp > .5, levels(sub[[yv]])[2], levels(sub[[yv]])[1]),
                       levels = levels(sub[[yv]]))
          acc <- mean(pc == sub[[yv]])
          yb <- as.integer(sub[[yv]]) - 1
          auc <- tryCatch({
            np <- sum(yb); nn <- sum(1-yb); r <- rank(pp)
            (sum(r[yb == 1]) - np*(np+1)/2) / (np*nn)
          }, error = function(e) NA)
          hl_p <- tryCatch({
            g <- 10
            grp <- cut(pp, stats::quantile(pp, probs = seq(0,1,1/g)), include.lowest = TRUE)
            obs <- tapply(yb, grp, sum); exp <- tapply(pp, grp, sum); cnt <- tapply(pp, grp, length)
            chi <- sum((obs-exp)^2 / (exp*(1-exp/cnt)), na.rm = TRUE)
            stats::pchisq(chi, g-2, lower.tail = FALSE)
          }, error = function(e) NA)
          aic <- stats::AIC(fit)
          st_m <- if (mcf >= .2 && mcf <= .4) "ok" else if (mcf > .4 || mcf >= .1) "warn" else "err"
          st_a <- if (is.na(auc)) "info" else if (auc >= .8) "ok" else if (auc >= .7) "warn" else "err"
          st_h <- if (is.na(hl_p)) "info" else if (hl_p > .05) "ok" else "warn"
          metrics <- rbind(
            mv_row("Pseudo-R2 McFadden", round(mcf,3),
                   "0,20-0,40 bon ajustement (échelle propre)",
                   if (st_m=="ok") "Bon ajustement" else if (st_m=="warn") "Ajustement a nuancer" else "Ajustement faible",
                   st_m),
            mv_row("AUC (courbe ROC)", if (is.na(auc)) "n/d" else round(auc,3),
                   ">= 0,80 bon ; 0,70-0,80 acceptable ; < 0,70 faible",
                   if (st_a=="ok") "Bon pouvoir discriminant" else if (st_a=="warn") "Discrimination acceptable" else "Discrimination faible",
                   st_a),
            mv_row("Taux de bon classement", paste0(round(100*acc,1)," %"),
                   "Plus élevé = meilleur (a comparer au hasard)",
                   "Precision globale du modèle", "info"),
            mv_row("Hosmer-Lemeshow (p)", if (is.na(hl_p)) "n/d" else format.pval(hl_p,digits=3),
                   "p > 0,05 : ajustement acceptable",
                   if (st_h=="ok") "Calibration acceptable" else "Mauvaise calibration", st_h),
            mv_row("AIC", round(aic,1),
                   "Plus bas = meilleur (comparaison de modèles)",
                   "Critere de comparaison", "info"))
          ors <- exp(stats::coef(fit))
          or_df <- data.frame(Terme = names(ors), Odds_ratio = round(ors,4))
          o <- order(pp); yo <- yb[o]
          roc_df <- data.frame(
            FPR = c(0, cumsum(rev(1-yo))/sum(1-yo)),
            TPR = c(0, cumsum(rev(yo))/sum(yo)))
          plotfn <- function() {
            ggplot2::ggplot(roc_df, ggplot2::aes(FPR, TPR)) +
              ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#999") +
              ggplot2::geom_line(color = "#6a1b9a", linewidth = 1.1) +
              labs(title = "Régression logistique — courbe ROC",
                   subtitle = paste0("AUC = ", if (is.na(auc)) "n/d" else round(auc,3)),
                   x = "Taux de faux positifs", y = "Taux de vrais positifs") +
              mv_gg_theme()
          }
          exports <- list(Metriques = metrics, Odds_ratios = or_df)
          extra <- mv_card(border_color = "#6c757d",
            mv_section_header("Rapports de cotes (odds ratios)", "#6c757d", "table"),
            mv_data_table(or_df, "#6c757d", digits = 4))
          summ <- utils::capture.output(summary(fit))
          # Non-convergence / separation : on remonte le diagnostic dans la
          # note affichee plutot que de laisser l'avertissement en console.
          note <- if (!is.null(gl$note))
            paste("Regression logistique binaire estimée. ATTENTION :", gl$note)
          else "Regression logistique binaire estimée."
        } else {
          if (!mv_has("nnet"))
            return(list(ok = FALSE, error = "Package 'nnet' requis pour la logistique multinomiale."))
          fit <- nnet::multinom(fml, data = sub, trace = FALSE)
          ll0 <- nnet::multinom(stats::as.formula(paste0("`",yv,"` ~ 1")),
                                data = sub, trace = FALSE)
          mcf <- 1 - as.numeric(stats::logLik(fit))/as.numeric(stats::logLik(ll0))
          pc <- stats::predict(fit)
          acc <- mean(pc == sub[[yv]])
          aic <- stats::AIC(fit)
          st_m <- if (mcf >= .2 && mcf <= .4) "ok" else if (mcf > .4 || mcf >= .1) "warn" else "err"
          st_a <- if (acc >= .7) "ok" else if (acc >= .5) "warn" else "err"
          metrics <- rbind(
            mv_row("Pseudo-R2 McFadden", round(mcf,3),
                   "0,20-0,40 bon ajustement (échelle propre)",
                   if (st_m=="ok") "Bon ajustement" else if (st_m=="warn") "Ajustement a nuancer" else "Ajustement faible",
                   st_m),
            mv_row("Taux de bon classement", paste0(round(100*acc,1)," %"),
                   ">= 70 % bon ; 50-70 % modéré",
                   if (st_a=="ok") "Bonne prédiction" else if (st_a=="warn") "Prédiction modérée" else "Prédiction faible",
                   st_a),
            mv_row("AIC", round(aic,1),
                   "Plus bas = meilleur (comparaison de modèles)",
                   "Critere de comparaison", "info"),
            mv_row("Modalités de Y", paste(levels(sub[[yv]]), collapse=" / "),
                   "Vérifier l'equilibre des effectifs", "Réponse multinomiale", "info"))
          cm <- as.data.frame(table(Observe = sub[[yv]], Predit = pc))
          plotfn <- function() {
            ggplot2::ggplot(cm, ggplot2::aes(Predit, Observe, fill = Freq)) +
              ggplot2::geom_tile(color = "white", linewidth = .6) +
              ggplot2::geom_text(ggplot2::aes(label = Freq), size = 4,
                        color = ifelse(cm$Freq > max(cm$Freq)/2, "white", "#2c3e50")) +
              ggplot2::scale_fill_gradient(low = "#f3e5f5", high = "#6a1b9a") +
              labs(title = "Logistique multinomiale -- matrice de confusion",
                   subtitle = paste0("Taux de bon classement = ", round(100*acc,1), " %"),
                   x = "Classe predite", y = "Classe observée") +
              mv_gg_theme()
          }
          exports <- list(Metriques = metrics)
          extra <- NULL
          summ <- utils::capture.output(summary(fit))
          note <- "Regression logistique multinomiale estimée."
        }
        render <- shiny::tagList(
          mv_card(border_color = "#6a1b9a",
            mv_section_header("Qualite d'ajustement & discrimination", "#6a1b9a", "shapes"),
            mv_info_note("Ajustement (pseudo-R2), discrimination (AUC) et calibration du modèle logistique."),
            mv_metrics_table(metrics, "#6a1b9a"),
            mv_interp_bar(note, "#6a1b9a")),
          extra)
        list(ok = TRUE, render = render, metrics = metrics, note = note,
          summary = c("=== Regression logistique ===", summ),
          plotfn = plotfn, exports = exports)
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ====================== MOTEURS D'ANALYSE (MIXTES) ========================

  # ---- AFDM ----------------------------------------------------------------
  shiny::observeEvent(input$mv_famd_run, {
    mv_res[["famd"]] <- local({
      tryCatch({
        d <- mv_data(); vars <- input$mv_famd_vars
        if (is.null(vars) || length(vars) < 3)
          return(list(ok = FALSE, error = "Sélectionnez au moins 3 variables."))
        quali_sup <- setdiff(intersect(input$mv_famd_quali_sup %||% character(0), names(d)), vars)
        quanti_sup <- setdiff(intersect(input$mv_famd_quanti_sup %||% character(0), names(d)), c(vars, quali_sup))
        all_cols <- c(vars, quali_sup, quanti_sup)
        sub <- d[, all_cols, drop = FALSE]
        for (v in vars) if (!is.numeric(sub[[v]])) sub[[v]] <- droplevels(factor(sub[[v]]))
        for (v in quali_sup) sub[[v]] <- droplevels(factor(sub[[v]]))
        keep_rows <- stats::complete.cases(sub[, vars, drop = FALSE])
        sub <- sub[keep_rows, , drop = FALSE]
        # Source des labels pour individus (optionnel)
        ls <- input$mv_famd_labelsource %||% "rownames"
        if (!identical(ls, "rownames") && ls %in% names(d)) {
          rownames(sub) <- make.unique(as.character(d[[ls]][keep_rows]))
        }
        ind_sup <- mv_parse_ind_sup(input$mv_famd_ind_sup, nrow(sub))
        sup_var_idx <- if (length(c(quali_sup, quanti_sup)))
          match(c(quali_sup, quanti_sup), names(sub)) else NULL
        nq <- sum(sapply(sub[, vars, drop = FALSE], is.numeric)); nc <- length(vars) - nq
        if (nq < 1 || nc < 1)
          return(list(ok = FALSE, error = "L'AFDM exige au moins 1 variable quantitative ET 1 qualitative."))
        ncp <- input$mv_famd_ncp %||% 5
        famd <- FactoMineR::FAMD(sub, ncp = ncp, graph = FALSE,
                                 sup.var = sup_var_idx,
                                 ind.sup = if (length(ind_sup)) ind_sup else NULL)
        eig <- famd$eig
        dim12 <- sum(eig[1:min(2,nrow(eig)),2])
        kaiser <- sum(eig[,1] > 1)
        cum_ncp <- eig[min(ncp,nrow(eig)),3]
        st_dim <- if (dim12 >= 70) "ok" else if (dim12 >= 50) "warn" else "err"
        st_cum <- if (cum_ncp >= 70) "ok" else "warn"
        metrics <- rbind(
          mv_row("Inertie axes 1-2", paste0(round(dim12,1)," %"),
                 ">= 70 % bonne restitution ; 50-70 % acceptable",
                 if (st_dim=="ok") "Plan factoriel representatif" else if (st_dim=="warn") "Restitution acceptable" else "Restitution limitee",
                 st_dim),
          mv_row("Axes a valeur propre > 1", kaiser,
                 "Critere de Kaiser : axes informatifs",
                 paste0(kaiser, " axe(s) informatif(s)"), "info"),
          mv_row("Composition du tableau", paste0(nq, " quanti / ", nc, " quali"),
                 "Les deux types doivent être presents",
                 "Tableau mixte equilibre par l'AFDM", "info"),
          mv_row("Inertie cumulee (ncp axes)", paste0(round(cum_ncp,1)," %"),
                 ">= 70-80 % conseille",
                 if (st_cum=="ok") "Bonne restitution cumulee" else "Restitution cumulee limitee",
                 st_cum))
        ic <- as.data.frame(hstat_coord_mat(famd$ind$coord)[, 1:min(2, ncol(hstat_coord_mat(famd$ind$coord))), drop = FALSE])
        if (ncol(ic) < 2) ic$D2 <- 0
        names(ic)[1:2] <- c("Dim1","Dim2")
        ic$label <- if (!is.null(rownames(famd$ind$coord))) rownames(famd$ind$coord) else as.character(seq_len(nrow(ic)))
        eig_df <- data.frame(Axe = rownames(eig), Valeur_propre = round(eig[,1],4),
                             Variance = round(eig[,2],2), Cumul = round(eig[,3],2))
        render <- shiny::tagList(
          mv_card(border_color = "#00695c",
            mv_section_header("Structure factorielle (données mixtes)", "#00695c", "layer-group"),
            mv_info_note("L'AFDM equilibre l'influence des variables quantitatives et qualitatives."),
            mv_metrics_table(metrics, "#00695c"),
            mv_interp_bar(paste0(nq, " quanti + ", nc, " quali. Inertie axes 1-2 = ",
              round(dim12,1), " %."), mv_col(st_dim))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Valeurs propres par axe", "#6c757d", "chart-simple"),
            mv_data_table(eig_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("AFDM : ", nq, " quanti + ", nc, " quali."),
          summary = c("=== Analyse Factorielle de Données Mixtes ===",
            "Valeurs propres :", paste(utils::capture.output(round(eig,4)), collapse="\n")),
          plotfn = function() {
            sub_t <- paste0("Inertie axes 1-2 = ", round(dim12,1), " %")
            gvar <- input$mv_famd_groupvar %||% "__none__"
            grp_vals <- NULL
            if (!identical(gvar, "__none__") && gvar %in% names(d)) {
              grp_vals <- as.character(d[[gvar]][keep_rows])
              if (length(ind_sup)) grp_vals <- grp_vals[-ind_sup]
            }
            pp <- mv_factor_viz(famd, "famd", input$mv_famd_plottype %||% "biplot",
                                input$mv_famd_colorby, "AFDM -- données mixtes", sub_t,
                                ptsz = input$mv_famd_ptsz %||% mv_pt_size(),
                                arrsz = input$mv_famd_arrsz %||% mv_ln_width(),
                                lblsz = hstat_lbl_pt2gg(input$mv_famd_lblsz %||% mv_lbl_pt_ind()),
                                lblszvar = hstat_lbl_pt2gg(input$mv_famd_lblszvar %||% mv_lbl_pt_var()),
                                show_lab = isTRUE(input$mv_famd_showlab),
                                axes = c(as.integer(input$mv_famd_axisx %||% 1), as.integer(input$mv_famd_axisy %||% 2)),
                                group_values = grp_vals)
            if (!is.null(pp)) {
              pt_famd <- input$mv_famd_plottype %||% "biplot"
              if (pt_famd %in% c("ind", "biplot")) {
                gv_name <- input$mv_famd_ellipsevar %||% "__none__"
                gv <- NULL
                if (!identical(gv_name, "__none__") && gv_name %in% names(d)) {
                  gv <- as.character(d[[gv_name]][keep_rows])
                  if (length(ind_sup)) gv <- gv[-ind_sup]
                }
                pp <- mv_add_ind_ellipses(pp, famd, "mv_famd", gv)
              }
              return(pp)
            }
            p <- ggplot2::ggplot(ic, ggplot2::aes(Dim1, Dim2)) +
              ggplot2::geom_hline(yintercept = 0, color = "#bbb", linewidth = .4) +
              ggplot2::geom_vline(xintercept = 0, color = "#bbb", linewidth = .4) +
              ggplot2::geom_point(size = mv_pt_size(), alpha = .7, color = "#00695c") +
              labs(title = "AFDM -- projection des individus", subtitle = sub_t,
                   x = paste0("Dim 1 (", round(eig[1,2],1), " %)"),
                   y = paste0("Dim 2 (", round(eig[min(2,nrow(eig)),2],1), " %)")) +
              mv_gg_theme()
            mv_add_labels(p, ic, "Dim1", "Dim2", "label")
          },
          exports = list(Metriques = metrics, Valeurs_propres = eig_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- AFM -----------------------------------------------------------------
  shiny::observeEvent(input$mv_mfa_run, {
    mv_res[["mfa"]] <- local({
      tryCatch({
        d <- mv_data(); qv <- input$mv_mfa_quanti; cv <- input$mv_mfa_quali
        if (is.null(qv) || length(qv) < 1 || is.null(cv) || length(cv) < 1)
          return(list(ok = FALSE, error = "Definissez au moins 1 variable dans chaque bloc."))
        # Bloc supplémentaire qualitatif (optionnel) -> 3e groupe marqué supplémentaire
        quali_sup <- setdiff(intersect(input$mv_mfa_quali_sup %||% character(0), names(d)), c(qv, cv))
        cols <- c(qv, cv, quali_sup)
        sub <- d[, cols, drop = FALSE]
        for (v in c(cv, quali_sup)) sub[[v]] <- droplevels(factor(sub[[v]]))
        keep_rows <- stats::complete.cases(sub[, c(qv, cv), drop = FALSE])
        sub <- sub[keep_rows, , drop = FALSE]
        ls <- input$mv_mfa_labelsource %||% "rownames"
        if (!identical(ls, "rownames") && ls %in% names(d)) {
          rownames(sub) <- make.unique(as.character(d[[ls]][keep_rows]))
        }
        ind_sup <- mv_parse_ind_sup(input$mv_mfa_ind_sup, nrow(sub))
        ncp <- input$mv_mfa_ncp %||% 5
        grp <- c(length(qv), length(cv)); typ <- c("s","n")
        gname <- c("Quantitatif","Qualitatif"); gsup <- NULL
        if (length(quali_sup) > 0) {
          grp <- c(grp, length(quali_sup)); typ <- c(typ, "n")
          gname <- c(gname, "Quali. sup."); gsup <- 3
        }
        mfa <- FactoMineR::MFA(sub, group = grp,
          type = typ, name.group = gname, num.group.sup = gsup,
          ind.sup = if (length(ind_sup)) ind_sup else NULL,
          ncp = ncp, graph = FALSE)
        eig <- mfa$eig
        dim12 <- sum(eig[1:min(2,nrow(eig)),2])
        rv <- tryCatch(mfa$group$RV[1,2], error = function(e) NA)
        st_dim <- if (dim12 >= 70) "ok" else if (dim12 >= 50) "warn" else "err"
        st_rv  <- if (is.na(rv)) "info" else if (rv >= .5) "ok" else if (rv >= .3) "warn" else "err"
        metrics <- rbind(
          mv_row("Inertie axes 1-2", paste0(round(dim12,1)," %"),
                 ">= 70 % bonne restitution ; 50-70 % acceptable",
                 if (st_dim=="ok") "Plan factoriel representatif" else if (st_dim=="warn") "Restitution acceptable" else "Restitution limitee",
                 st_dim),
          mv_row("Coefficient RV entre blocs", if (is.na(rv)) "n/d" else round(rv,3),
                 ">= 0,50 forte concordance ; 0,30-0,50 modérée ; < 0,30 faible",
                 if (st_rv=="ok") "Blocs très concordants" else if (st_rv=="warn") "Concordance modérée"
                   else if (st_rv=="err") "Blocs peu concordants" else "Non disponible",
                 st_rv),
          mv_row("Blocs definis", paste0(length(qv), " quanti / ", length(cv), " quali"),
                 "Chaque bloc equilibre par sa 1re valeur propre",
                 "Structure en groupes de variables", "info"))
        ic <- as.data.frame(hstat_coord_mat(mfa$ind$coord)[, 1:min(2, ncol(hstat_coord_mat(mfa$ind$coord))), drop = FALSE])
        if (ncol(ic) < 2) ic$D2 <- 0
        names(ic)[1:2] <- c("Dim1","Dim2")
        ic$label <- if (!is.null(rownames(mfa$ind$coord))) rownames(mfa$ind$coord) else as.character(seq_len(nrow(ic)))
        eig_df <- data.frame(Axe = rownames(eig), Valeur_propre = round(eig[,1],4),
                             Variance = round(eig[,2],2), Cumul = round(eig[,3],2))
        render <- shiny::tagList(
          mv_card(border_color = "#00695c",
            mv_section_header("Integration des blocs de variables", "#00695c", "layer-group"),
            mv_info_note("L'AFM compare un bloc quantitatif et un bloc qualitatif sur un même plan."),
            mv_metrics_table(metrics, "#00695c"),
            mv_interp_bar(paste0("Bloc quanti (", length(qv), ") + bloc quali (", length(cv),
              "). Inertie axes 1-2 = ", round(dim12,1), " %."), mv_col(st_dim))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Valeurs propres par axe", "#6c757d", "chart-simple"),
            mv_data_table(eig_df, "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("AFM : bloc quanti (", length(qv), ") + bloc quali (", length(cv), ")."),
          summary = c("=== Analyse Factorielle Multiple ===",
            "Valeurs propres :", paste(utils::capture.output(round(eig,4)), collapse="\n")),
          plotfn = function() {
            sub_t <- paste0("Inertie axes 1-2 = ", round(dim12,1), " %")
            gvar <- input$mv_mfa_groupvar %||% "__none__"
            grp_vals <- NULL
            if (!identical(gvar, "__none__") && gvar %in% names(d)) {
              grp_vals <- as.character(d[[gvar]][keep_rows])
              if (length(ind_sup)) grp_vals <- grp_vals[-ind_sup]
            }
            pp <- mv_factor_viz(mfa, "mfa", input$mv_mfa_plottype %||% "biplot",
                                input$mv_mfa_colorby, "AFM -- analyse factorielle multiple", sub_t,
                                ptsz = input$mv_mfa_ptsz %||% mv_pt_size(),
                                arrsz = input$mv_mfa_arrsz %||% mv_ln_width(),
                                lblsz = hstat_lbl_pt2gg(input$mv_mfa_lblsz %||% mv_lbl_pt_ind()),
                                lblszvar = hstat_lbl_pt2gg(input$mv_mfa_lblszvar %||% mv_lbl_pt_var()),
                                show_lab = isTRUE(input$mv_mfa_showlab),
                                axes = c(as.integer(input$mv_mfa_axisx %||% 1), as.integer(input$mv_mfa_axisy %||% 2)),
                                group_values = grp_vals)
            if (!is.null(pp)) return(pp)
            p <- ggplot2::ggplot(ic, ggplot2::aes(Dim1, Dim2)) +
              ggplot2::geom_hline(yintercept = 0, color = "#bbb", linewidth = .4) +
              ggplot2::geom_vline(xintercept = 0, color = "#bbb", linewidth = .4) +
              ggplot2::geom_point(size = mv_pt_size(), alpha = .7, color = "#00695c") +
              labs(title = "AFM -- projection des individus", subtitle = sub_t,
                   x = paste0("Dim 1 (", round(eig[1,2],1), " %)"),
                   y = paste0("Dim 2 (", round(eig[min(2,nrow(eig)),2],1), " %)")) +
              mv_gg_theme()
            mv_add_labels(p, ic, "Dim1", "Dim2", "label")
          },
          exports = list(Metriques = metrics, Valeurs_propres = eig_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  # ---- k-prototypes --------------------------------------------------------
  shiny::observeEvent(input$mv_kproto_run, {
    mv_res[["kproto"]] <- local({
      tryCatch({
        if (!mv_has("clustMixType"))
          return(list(ok = FALSE,
                      error = hstat_pkg_manquant("clustMixType", "Classification k-prototypes")))
        d <- mv_data(); vars <- input$mv_kproto_vars
        if (is.null(vars) || length(vars) < 2)
          return(list(ok = FALSE, error = "Sélectionnez au moins 2 variables."))
        k <- input$mv_kproto_k %||% 3
        sub <- d[, vars, drop = FALSE]
        for (v in vars) if (!is.numeric(sub[[v]])) sub[[v]] <- droplevels(factor(sub[[v]]))
        sub <- sub[stats::complete.cases(sub), , drop = FALSE]
        nq <- sum(sapply(sub, is.numeric)); nc <- ncol(sub) - nq
        if (nq < 1 || nc < 1)
          return(list(ok = FALSE, error = "k-prototypes exige au moins 1 variable quantitative ET 1 qualitative."))
        for (v in names(sub)) if (is.numeric(sub[[v]])) sub[[v]] <- as.numeric(scale(sub[[v]]))
        hstat_set_seed(input$globalSeed)
        sub <- mv_subsample(sub, 8000)
        kp <- clustMixType::kproto(sub, k = k, iter.max = input$mv_kproto_iter %||% 20,
                                   verbose = FALSE)
        sizes <- as.integer(table(kp$cluster))
        tot_cost <- kp$tot.withinss
        sil <- tryCatch({
          v <- clustMixType::validation_kproto(method = "silhouette", object = kp, verbose = FALSE)
          as.numeric(v$index)
        }, error = function(e) NA)
        st_sil <- if (is.na(sil)) "info" else if (sil >= .5) "ok" else if (sil >= .25) "warn" else "err"
        st_bal <- hstat_part_equilibre(sizes)$verdict
        metrics <- rbind(
          mv_row("Cout total intra (within SS)", round(tot_cost,1),
                 "Plus faible = clusters plus compacts",
                 "A minimiser entre solutions k", "info"),
          mv_row("Silhouette mixte", if (is.na(sil)) "n/d" else round(sil,3),
                 "> 0,50 forte ; 0,25-0,50 raisonnable ; < 0,25 faible",
                 if (st_sil=="ok") "Structure forte" else if (st_sil=="warn") "Structure raisonnable"
                   else if (st_sil=="err") "Structure faible" else "Non calculée",
                 st_sil),
          mv_row("Equilibre des clusters", paste(sizes, collapse=" / "),
                 "Eviter clusters vides ou très minoritaires",
                 switch(st_bal, ok = "Répartition acceptable",
                        warn = "Cluster très minoritaire", "Effectifs non calculables"),
                 st_bal),
          mv_row("Composition du tableau", paste0(nq, " quanti / ", nc, " quali"),
                 "Les deux types doivent être presents",
                 "Partitionnement mixte", "info"))
        sz_df <- data.frame(Cluster = factor(seq_len(k)), Effectif = sizes)
        render <- shiny::tagList(
          mv_card(border_color = "#00695c",
            mv_section_header("Qualite de la partition mixte", "#00695c", "object-group"),
            mv_info_note("k-prototypes combine distance euclidienne (quanti) et appariement (quali)."),
            mv_metrics_table(metrics, "#00695c"),
            mv_interp_bar(paste0("Solution a ", k, " clusters sur ", nrow(sub), " individus."),
              if (is.na(sil)) "#2980b9" else mv_col(st_sil))),
          mv_card(border_color = "#6c757d",
            mv_section_header("Prototypes des clusters", "#6c757d", "crosshairs"),
            mv_data_table(data.frame(Cluster = seq_len(k), kp$centers, check.names = FALSE),
                          "#6c757d")))
        list(ok = TRUE, render = render, metrics = metrics,
          note = paste0("k-prototypes : ", k, " clusters sur ", nrow(sub), " individus."),
          summary = c("=== Classification k-prototypes ===",
            paste0("Variables : ", paste(vars, collapse=", ")),
            "", "Prototypes :", paste(utils::capture.output(kp$centers), collapse="\n")),
          plotfn = function() {
            df_plot <- sz_df
            ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[["Cluster"]], y = .data[["Effectif"]], fill = .data[["Cluster"]])) +
              ggplot2::geom_col(width = .65) +
              ggplot2::geom_text(ggplot2::aes(label = .data[["Effectif"]]), vjust = -0.5, size = 3.6, fontface = "bold") +
              labs(title = "k-prototypes -- effectifs par cluster",
                   subtitle = paste0(k, " clusters | ", nq, " quanti + ", nc, " quali"),
                   x = "Cluster", y = "Nombre d'individus") +
              mv_gg_theme() + ggplot2::theme(legend.position = "none")
          },
          exports = list(Metriques = metrics, Effectifs = sz_df))
      }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  invisible(NULL)

  # ---- Seuils d'efficacite (module Shiny) ----
  mod_threshold_server("threshold", values)

  # ---- Citer HStat ----
  cite_text <- shiny::reactive({
    hstat_citation(input$citeStyle %||% "text")
  })
  output$citeOutput <- shiny::renderText({ cite_text() })

  shiny::observeEvent(input$citeCopy, {
    txt <- cite_text()
    # Copie via l'API navigateur (repli execCommand pour les contextes non securises)
    session$sendCustomMessage("hstat_copy_clip", list(text = txt))
    shiny::showNotification(shiny::tagList(shiny::icon("check"), " Citation copiée dans le presse-papiers."),
                     type = "message", duration = 3)
  })

  output$citeDownload <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$citeStyle %||% "text",
                    bibtex = "bib", ris = "ris", markdown = "md", "txt")
      paste0("citation_HStat.", ext)
    },
    content = function(file) writeLines(cite_text(), file, useBytes = TRUE)
  )
}
