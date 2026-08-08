#  Module Shiny : Deep Learning
#  Deux moteurs 100 % R (aucune dépendance Python) :
#   - neuralnet : perceptron multi-couches (MLP) à architecture libre,
#     toujours disponible (régression & classification) ;
#   - torch (optionnel) : MLP profond avec courbe d'apprentissage détaillée et
#     LSTM pour la prévision de séquences temporelles.
#  Métriques interprétées, architecture du réseau, courbe d'apprentissage,
#  simulateur de prédictions (saisie manuelle ou import de nouveaux cas),
#  exports tableaux (CSV/Excel) et images (PNG/JPG/TIFF/BMP/PDF/SVG, <= 20 000 DPI).

mod_dl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(id = ns("dlTabs"),
      # ================= MLP (tabulaire) =================
      tabPanel(tagList(icon("brain"), " Réseau de neurones (MLP)"),
        div(style = "padding-top:12px;"),
        fluidRow(
          box(title = tagList(icon("sliders"), " Configuration"), status = "primary",
              width = 4, solidHeader = TRUE, collapsible = TRUE,
              selectInput(ns("dlEngine"), "Moteur",
                choices = c("neuralnet (R natif, toujours disponible)" = "neuralnet",
                            "torch (profond, si installé)" = "torch")),
              uiOutput(ns("dlEngineNote")),
              uiOutput(ns("dlDoc")),
              selectInput(ns("dlTarget"), "Variable cible (à prédire)", choices = NULL),
              uiOutput(ns("dlTaskInfo")),
              selectizeInput(ns("dlPreds"), "Variables explicatives", choices = NULL,
                             multiple = TRUE),
              textInput(ns("dlHidden"), "Couches cachées (neurones, séparés par des virgules)",
                        value = "16,8"),
              fluidRow(
                column(6, sliderInput(ns("dlSplit"), "Part d'entraînement (%)",
                                      min = 50, max = 90, value = 75, step = 5)),
                column(6, numericInput(ns("dlSeed"), "Graine aléatoire",
                                       value = 123, min = 1, step = 1))),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'torch'", ns("dlEngine")),
                fluidRow(
                  column(4, numericInput(ns("dlEpochs"), "Époques",
                                         value = 100, min = 10, max = 2000, step = 10)),
                  column(4, numericInput(ns("dlLr"), "Taux d'apprentissage",
                                         value = 0.01, min = 0.0001, max = 1, step = 0.005)),
                  column(4, numericInput(ns("dlBatch"), "Taille de lot",
                                         value = 64, min = 8, max = 4096, step = 8)))),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'neuralnet'", ns("dlEngine")),
                fluidRow(
                  column(6, selectInput(ns("dlAct"), "Activation",
                           choices = c("Logistique" = "logistic", "Tangente hyp." = "tanh"))),
                  column(6, numericInput(ns("dlStepmax"), "Itérations max",
                                         value = 100000, min = 10000, step = 10000)))),
              actionButton(ns("dlRun"), "Entraîner le réseau",
                           icon = icon("play"), class = "btn-primary"),
              tags$small(style = "color:#6b7280; display:block; margin-top:8px;",
                sprintf("Prédicteurs standardisés automatiquement (indispensable aux réseaux de neurones). Au-delà de %s lignes, entraînement sur échantillon (HSTAT_ML_MAX_N).",
                        format(HSTAT_ML_MAX_N, big.mark = " ")))),
          box(title = tagList(icon("chart-area"), " Diagnostic du réseau"),
              status = "success", width = 8, solidHeader = TRUE,
              shinycssloaders::withSpinner(plotOutput(ns("dlPlot"), height = "420px")),
              tabsetPanel(
                tabPanel("Téléchargement", div(style = "padding-top:10px;",
                         hstat_export_plot_ui(ns, "dlPl"))),
                tabPanel("Apparence", div(style = "padding-top:10px;",
                         hstat_plot_opts_ui(ns, "dlO")))))),
        fluidRow(
          box(title = tagList(icon("table"), " Métriques & interprétation"),
              status = "info", width = 6, solidHeader = TRUE,
              DT::dataTableOutput(ns("dlMetrics")),
              hstat_export_table_ui(ns, "dlMet"),
              uiOutput(ns("dlInterp"))),
          box(title = tagList(icon("diagram-project"), " Architecture & apprentissage"),
              status = "warning", width = 6, solidHeader = TRUE,
              uiOutput(ns("dlArch")),
              plotOutput(ns("dlLoss"), height = "260px"),
              hstat_export_plot_ui(ns, "dlLo", width = 9, height = 5))),
        fluidRow(
          box(title = tagList(icon("magic"), " Simulateur de prédictions"),
              status = "success", width = 12, solidHeader = TRUE,
              fluidRow(
                column(5,
                  h5(strong("1. Saisie manuelle d'un cas")),
                  fluidRow(uiOutput(ns("dlSimForm"))),
                  actionButton(ns("dlSimOne"), "Prédire ce cas",
                               icon = icon("bullseye"), class = "btn-success"),
                  uiOutput(ns("dlSimOneOut"))),
                column(7,
                  h5(strong("2. Import d'un fichier de nouveaux cas (CSV/Excel)")),
                  fileInput(ns("dlSimFile"), NULL, accept = c(".csv", ".xlsx")),
                  tags$small(style = "color:#6b7280;",
                    "Mêmes colonnes explicatives que le modèle ; la cible est inutile."),
                  DT::dataTableOutput(ns("dlSimBatch")),
                  hstat_export_table_ui(ns, "dlSimB"),
                  uiOutput(ns("dlSimBatchInterp"))))))),
      # ================= LSTM (séquences) =================
      tabPanel(tagList(icon("wave-square"), " LSTM (séquences, torch)"),
        div(style = "padding-top:12px;"),
        fluidRow(
          box(title = tagList(icon("sliders"), " Configuration"), status = "primary",
              width = 4, solidHeader = TRUE,
              uiOutput(ns("lstmAvail")),
              uiOutput(ns("lstmDoc")),
              selectInput(ns("lstmVar"), "Série à prévoir (numérique)", choices = NULL),
              fluidRow(
                column(6, numericInput(ns("lstmLook"), "Fenêtre (pas passés)",
                                       value = 12, min = 2, max = 200, step = 1)),
                column(6, numericInput(ns("lstmH"), "Horizon futur (h)",
                                       value = 12, min = 1, max = 500, step = 1))),
              fluidRow(
                column(6, numericInput(ns("lstmHidden"), "Neurones LSTM",
                                       value = 32, min = 4, max = 256, step = 4)),
                column(6, numericInput(ns("lstmEpochs"), "Époques",
                                       value = 80, min = 10, max = 1000, step = 10))),
              fluidRow(
                column(6, numericInput(ns("lstmLr"), "Taux d'apprentissage",
                                       value = 0.005, min = 0.0001, max = 0.5, step = 0.001)),
                column(6, numericInput(ns("lstmTestN"), "Taille du jeu de test",
                                       value = 12, min = 2, step = 1))),
              actionButton(ns("lstmRun"), "Entraîner le LSTM",
                           icon = icon("play"), class = "btn-primary")),
          box(title = tagList(icon("chart-line"), " Prévision LSTM"),
              status = "success", width = 8, solidHeader = TRUE,
              shinycssloaders::withSpinner(plotOutput(ns("lstmPlot"), height = "420px")),
              hstat_export_plot_ui(ns, "lstmPl"))),
        fluidRow(
          box(title = tagList(icon("table"), " Métriques, prévisions & interprétation"),
              status = "info", width = 8, solidHeader = TRUE,
              DT::dataTableOutput(ns("lstmMetrics")),
              hstat_export_table_ui(ns, "lstmMet"),
              DT::dataTableOutput(ns("lstmFuture")),
              hstat_export_table_ui(ns, "lstmFut"),
              uiOutput(ns("lstmInterp"))),
          box(title = tagList(icon("chart-area"), " Courbe d'apprentissage"),
              status = "warning", width = 4, solidHeader = TRUE,
              plotOutput(ns("lstmLoss"), height = "300px"),
              hstat_export_plot_ui(ns, "lstmLo", width = 8, height = 5)))))
  )
}

mod_dl_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Incremente apres une installation reussie du backend torch, pour
    # invalider les interfaces qui dependent de sa disponibilite.
    torch_tick <- reactiveVal(0)
    has_torch <- function() {
      requireNamespace("torch", quietly = TRUE) &&
        tryCatch(torch::torch_is_installed(), error = function(e) FALSE)
    }
    # Etat du backend : "ready", "package_missing" ou "backend_missing"
    torch_state <- function() {
      if (!requireNamespace("torch", quietly = TRUE)) return("package_missing")
      ok <- tryCatch(torch::torch_is_installed(), error = function(e) FALSE)
      if (isTRUE(ok)) "ready" else "backend_missing"
    }
    torch_install_ui <- function(btn_id) {
      st <- torch_state()
      if (st == "ready") return(NULL)
      if (st == "package_missing")
        return(div(class = "callout callout-warning", icon("exclamation-triangle"),
          " Le package torch n'est pas installé : exécutez install.packages(\"torch\") ",
          "puis relancez l'application. Les modèles neuralnet restent disponibles."))
      div(class = "callout callout-warning", icon("download"),
          strong(" Bibliothèques torch non installées. "),
          "Un téléchargement unique (~600 Mo) est nécessaire pour les modèles torch/LSTM. ",
          "Il n'est jamais lancé au démarrage pour ne pas bloquer l'application.",
          div(style = "margin-top:8px;",
              actionButton(ns(btn_id),
                           "Télécharger et installer les bibliothèques torch",
                           icon = icon("download"), class = "btn-warning")),
          tags$small(style = "color:#6b7280; display:block; margin-top:6px;",
            "Pendant le téléchargement, l'application reste ouverte mais cette ",
            "session est occupée ; les modèles neuralnet sont utilisables sans torch."))
    }
    do_torch_install <- function() {
      if (torch_state() != "backend_missing") return(invisible(NULL))
      ok <- FALSE
      withProgress(message = "Installation des bibliothèques torch",
                   detail = "Téléchargement (~600 Mo), une seule fois...",
                   value = 0.2, {
        ok <- tryCatch({ torch::install_torch(); TRUE },
                       error = function(e) {
                         showNotification(hstat_err_fr(e, "Échec de l'installation de torch"),
                                          type = "error", duration = 12)
                         FALSE
                       })
        incProgress(0.8)
      })
      if (ok) {
        usable <- tryCatch({ invisible(torch::torch_tensor(1)); TRUE },
                           error = function(e) FALSE)
        if (usable) showNotification("Bibliothèques torch installées : les moteurs torch et LSTM sont disponibles.",
                                     type = "message", duration = 8)
        else showNotification("torch est installé mais nécessite un redémarrage de R pour être utilisé (relancez run_hstat()).",
                              type = "warning", duration = 12)
        torch_tick(torch_tick() + 1)
      }
      invisible(NULL)
    }
    observeEvent(input$torchInstall,  do_torch_install())
    observeEvent(input$torchInstall2, do_torch_install())

    observe({
      df <- values$cleanData
      req(df)
      updateSelectInput(session, "dlTarget", choices = names(df))
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      updateSelectInput(session, "lstmVar", choices = num,
                        selected = if (length(num)) num[1] else NULL)
    })
    observeEvent(input$dlTarget, {
      df <- values$cleanData; req(df, nzchar(input$dlTarget %||% ""))
      updateSelectizeInput(session, "dlPreds",
        choices = setdiff(names(df), input$dlTarget),
        selected = setdiff(names(df), input$dlTarget))
    })

    output$dlEngineNote <- renderUI({
      torch_tick()
      if (identical(input$dlEngine, "torch")) torch_install_ui("torchInstall")
    })
    output$lstmAvail <- renderUI({
      torch_tick()
      torch_install_ui("torchInstall2")
    })
    output$dlDoc <- renderUI(
      hstat_model_doc_ui(if (identical(input$dlEngine, "torch")) "dl_torch"
                         else "dl_neuralnet"))
    output$lstmDoc <- renderUI(hstat_model_doc_ui("lstm"))

    task_of <- function(x)
      if (is.numeric(x) && length(unique(x[!is.na(x)])) > 10) "regression" else "classification"

    output$dlTaskInfo <- renderUI({
      df <- values$cleanData; req(df, nzchar(input$dlTarget %||% ""))
      tk <- task_of(df[[input$dlTarget]])
      div(class = "callout callout-info", icon("circle-info"),
          if (tk == "regression") " Tâche de RÉGRESSION (prédire une valeur numérique)."
          else " Tâche de CLASSIFICATION (prédire une classe).")
    })

    parse_hidden <- function(txt) {
      v <- suppressWarnings(as.integer(strsplit(gsub("\\s", "", txt %||% "16,8"), ",")[[1]]))
      v <- v[is.finite(v) & v >= 1 & v <= 512]
      if (length(v) == 0) c(16L, 8L) else utils::head(v, 6)
    }

    # ---- Préparation : standardisation + encodage one-hot ----------------------
    prepare_dl <- function() {
      df <- values$cleanData
      validate(need(!is.null(df), "Chargez d'abord des données."),
               need(nzchar(input$dlTarget %||% ""), "Choisissez la variable cible."),
               need(length(input$dlPreds %||% character(0)) > 0,
                    "Choisissez au moins une variable explicative."))
      target <- input$dlTarget
      preds  <- setdiff(input$dlPreds, target)
      d <- df[, c(target, preds), drop = FALSE]
      d <- d[stats::complete.cases(d), , drop = FALSE]
      validate(need(nrow(d) >= 30, "Au moins 30 lignes complètes sont requises."))
      d <- hstat_cap_df_rows(d, max_n = HSTAT_ML_MAX_N, what = "Deep learning")
      task <- task_of(d[[target]])
      if (task == "classification") {
        d[[target]] <- factor(d[[target]])
        validate(need(nlevels(d[[target]]) >= 2, "La cible doit avoir au moins 2 classes."),
                 need(min(table(d[[target]])) >= 2,
                      "Chaque classe doit compter au moins 2 observations."))
      } else d[[target]] <- as.numeric(d[[target]])
      for (v in preds) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
      # Matrice de design one-hot des prédicteurs, puis standardisation
      xlev <- lapply(d[preds], function(x) if (is.factor(x)) levels(x) else NULL)
      xlev <- xlev[!vapply(xlev, is.null, logical(1))]
      tt <- stats::terms(stats::as.formula(
        paste("~", paste(sprintf("`%s`", preds), collapse = "+"))))
      make_x <- function(nd) {
        mm <- stats::model.matrix(tt, data = nd, xlev = xlev)
        mm[, colnames(mm) != "(Intercept)", drop = FALSE]
      }
      x_all <- make_x(d)
      ctr <- colMeans(x_all)
      scl <- apply(x_all, 2, stats::sd); scl[!is.finite(scl) | scl == 0] <- 1
      std <- function(m) sweep(sweep(m, 2, ctr, "-"), 2, scl, "/")
      set.seed(as.integer(hstat_finite(input$dlSeed, 123)))
      n <- nrow(d)
      idx <- sample.int(n, floor(n * hstat_finite(input$dlSplit, 75) / 100))
      if (task == "classification") {
        for (lv in levels(d[[target]])) {
          w <- which(d[[target]] == lv)
          if (!any(w %in% idx)) idx <- c(idx, w[1])
        }
      }
      # Cible : standardisée en régression (dé-standardisée à la prédiction)
      y_all <- d[[target]]
      y_ctr <- if (task == "regression") mean(y_all[idx]) else 0
      y_scl <- if (task == "regression") max(stats::sd(y_all[idx]), 1e-9) else 1
      list(d = d, task = task, target = target, preds = preds,
           make_x = function(nd) std(make_x(nd)),
           x_train = std(x_all[idx, , drop = FALSE]),
           x_test  = std(x_all[-idx, , drop = FALSE]),
           y_train = y_all[idx], y_test = y_all[-idx],
           y_ctr = y_ctr, y_scl = y_scl,
           n_train = length(idx), n_test = n - length(idx))
    }

    # ---- Entraînement neuralnet -------------------------------------------------
    fit_neuralnet <- function(p, hidden) {
      if (!requireNamespace("neuralnet", quietly = TRUE))
        stop("Le package 'neuralnet' est requis (install.packages(\"neuralnet\")).")
      xt <- as.data.frame(p$x_train)
      names(xt) <- paste0("X", seq_len(ncol(xt)))
      cls <- p$task == "classification"
      if (cls) {
        lv <- levels(p$y_train)
        yt <- stats::model.matrix(~ y - 1, data.frame(y = p$y_train))
        colnames(yt) <- paste0("Y", seq_along(lv))
        dat <- cbind(as.data.frame(yt), xt)
        f <- stats::as.formula(paste(paste(colnames(yt), collapse = "+"), "~",
                                     paste(names(xt), collapse = "+")))
      } else {
        dat <- cbind(data.frame(Y1 = (p$y_train - p$y_ctr) / p$y_scl), xt)
        f <- stats::as.formula(paste("Y1 ~", paste(names(xt), collapse = "+")))
      }
      m <- neuralnet::neuralnet(
        f, data = dat, hidden = hidden,
        act.fct = input$dlAct %||% "logistic",
        linear.output = !cls,
        stepmax = max(1e4, hstat_finite(input$dlStepmax, 1e5)),
        rep = 1)
      if (is.null(m$weights))
        stop("Le réseau n'a pas convergé : augmenter les itérations max, réduire les couches ou simplifier les variables.")
      pf <- function(nd_x) {
        xn <- as.data.frame(nd_x); names(xn) <- names(xt)
        pr <- stats::predict(m, xn)
        if (!cls) list(pred = as.numeric(pr) * p$y_scl + p$y_ctr, prob = NULL)
        else {
          lv <- levels(p$y_train)
          pm <- pr / pmax(rowSums(pr), 1e-9); colnames(pm) <- lv
          list(pred = factor(lv[max.col(pm)], levels = lv), prob = pm)
        }
      }
      err <- tryCatch(as.numeric(m$result.matrix["error", 1]), error = function(e) NA_real_)
      list(predict_fun = pf, losses = NULL, final_err = err,
           n_params = sum(vapply(m$weights[[1]], length, numeric(1))),
           engine = "neuralnet")
    }

    # ---- Entraînement torch (MLP) -------------------------------------------------
    fit_torch <- function(p, hidden) {
      if (!has_torch())
        stop("Bibliothèques torch non installées : utilisez le bouton de téléchargement ci-dessus (ou basculez sur le moteur neuralnet).")
      cls <- p$task == "classification"
      lv  <- if (cls) levels(p$y_train) else NULL
      n_in  <- ncol(p$x_train)
      n_out <- if (cls) length(lv) else 1L
      torch::torch_manual_seed(as.integer(hstat_finite(input$dlSeed, 123)))
      layers <- list()
      sizes <- c(n_in, hidden)
      for (i in seq_along(hidden)) {
        layers[[length(layers) + 1]] <- torch::nn_linear(sizes[i], sizes[i + 1])
        layers[[length(layers) + 1]] <- torch::nn_relu()
      }
      layers[[length(layers) + 1]] <- torch::nn_linear(sizes[length(sizes)], n_out)
      model <- do.call(torch::nn_sequential, layers)
      xt <- torch::torch_tensor(p$x_train, dtype = torch::torch_float())
      yt <- if (cls)
        torch::torch_tensor(as.integer(p$y_train), dtype = torch::torch_long())
      else torch::torch_tensor(matrix((p$y_train - p$y_ctr) / p$y_scl, ncol = 1),
                               dtype = torch::torch_float())
      opt <- torch::optim_adam(model$parameters, lr = hstat_finite(input$dlLr, 0.01))
      epochs <- as.integer(max(10, hstat_finite(input$dlEpochs, 100)))
      bs <- as.integer(max(8, hstat_finite(input$dlBatch, 64)))
      ntr <- nrow(p$x_train)
      losses <- numeric(epochs)
      withProgress(message = "Entraînement torch", value = 0, {
        for (e in seq_len(epochs)) {
          ord <- sample.int(ntr)
          tot <- 0; nb <- 0
          for (s in seq(1, ntr, by = bs)) {
            ix <- ord[s:min(s + bs - 1, ntr)]
            opt$zero_grad()
            out <- model(xt[ix, , drop = FALSE])
            l <- if (cls) torch::nnf_cross_entropy(out, yt[ix])
                 else torch::nnf_mse_loss(out, yt[ix, , drop = FALSE])
            l$backward(); opt$step()
            tot <- tot + l$item(); nb <- nb + 1
          }
          losses[e] <- tot / max(nb, 1)
          incProgress(1 / epochs, detail = sprintf("époque %d — perte %.4f", e, losses[e]))
        }
      })
      model$eval()
      pf <- function(nd_x) {
        xn <- torch::torch_tensor(as.matrix(nd_x), dtype = torch::torch_float())
        out <- torch::with_no_grad(model(xn))
        if (!cls) list(pred = as.numeric(out) * p$y_scl + p$y_ctr, prob = NULL)
        else {
          pm <- as.matrix(torch::nnf_softmax(out, dim = 2)); colnames(pm) <- lv
          list(pred = factor(lv[max.col(pm)], levels = lv), prob = pm)
        }
      }
      list(predict_fun = pf, losses = losses, final_err = utils::tail(losses, 1),
           n_params = sum(vapply(model$parameters, function(w) w$numel(), numeric(1))),
           engine = "torch")
    }

    # Depot du resultat du reseau de neurones pour l'aide a la decision.
    observeEvent(dlfit(), {
      f <- tryCatch(dlfit(), error = function(e) NULL)
      if (is.null(f) || is.null(f$metrics)) return()
      hstat_ai_capture(values, "Deep Learning",
        sprintf("Reseau de neurones (%s)",
                if (!is.null(f$p) && identical(f$p$task, "classification"))
                  "classification" else "regression"),
        tables = list("Metriques du modele" = hstat_ai_as_table(f$metrics)),
        meta = list(variables = c(input$dlTarget, input$dlPredictors),
                    `variable cible` = input$dlTarget,
                    `couches cachees` = f$hidden))
    }, ignoreInit = TRUE)

    dlfit <- eventReactive(input$dlRun, {
      p <- prepare_dl()
      hidden <- parse_hidden(input$dlHidden)
      set.seed(as.integer(hstat_finite(input$dlSeed, 123)))
      r <- tryCatch({
        if (identical(input$dlEngine, "torch")) fit_torch(p, hidden)
        else fit_neuralnet(p, hidden)
      }, error = function(e) e)
      validate(need(!inherits(r, "error"),
                    if (inherits(r, "error")) conditionMessage(r) else ""))
      out <- r$predict_fun(p$x_test)
      mets <- if (p$task == "classification")
        hstat_metrics_cls(p$y_test, out$pred, out$prob)
      else hstat_metrics_reg(p$y_test, out$pred)
      list(p = p, fit = r, pred = out$pred, prob = out$prob,
           metrics = mets, hidden = hidden)
    })

    # ---- Diagnostic principal -----------------------------------------------------
    dl_gg <- reactive({
      f <- dlfit(); req(f)
      col <- hstat_plot_opt(input, "dlO", "Col", "#8e44ad")
      if (f$p$task == "regression") {
        d <- data.frame(obs = f$p$y_test, pred = as.numeric(f$pred))
        g1 <- ggplot2::ggplot(d, ggplot2::aes(obs, pred)) +
          ggplot2::geom_point(color = col, alpha = 0.55, size = 1.6) +
          ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                               color = "#e74c3c") +
          ggplot2::labs(title = sprintf("MLP (%s) — observé vs prédit (test)", f$fit$engine),
                        x = "Valeur observée", y = "Valeur prédite")
        # Courbe de prédiction : valeurs observées et prédites le long du test
        dc <- rbind(data.frame(i = seq_len(nrow(d)), v = d$obs, quoi = "Observé"),
                    data.frame(i = seq_len(nrow(d)), v = d$pred, quoi = "Prédit"))
        g2 <- ggplot2::ggplot(dc, ggplot2::aes(i, v, color = quoi)) +
          ggplot2::geom_line(linewidth = 0.6, alpha = 0.9) +
          ggplot2::scale_color_manual(values = c("Observé" = "#37474f",
                                                 "Prédit" = col)) +
          ggplot2::labs(title = "Courbe de prédiction (jeu de test)",
                        x = "Observation (ordre du jeu de test)", y = "Valeur",
                        color = NULL) +
          ggplot2::theme(legend.position = "bottom")
        g1 <- hstat_apply_plot_opts(g1, input, "dlO")
        g2 <- hstat_apply_plot_opts(g2, input, "dlO") +
          ggplot2::theme(legend.position = "bottom")
        patchwork::wrap_plots(g1, g2, ncol = 2)
      } else {
        d <- as.data.frame(table(Observe = f$p$y_test, Predit = f$pred))
        tot <- stats::ave(d$Freq, d$Observe, FUN = sum)
        d$Pct <- ifelse(tot > 0, 100 * d$Freq / tot, 0)
        g <- ggplot2::ggplot(d, ggplot2::aes(Predit, Observe, fill = Freq)) +
          ggplot2::geom_tile() +
          ggplot2::geom_text(ggplot2::aes(label = sprintf("%d\n(%.0f %%)", Freq, Pct)),
                             color = "white", lineheight = 0.9) +
          ggplot2::scale_fill_gradient(low = "#90a4ae", high = col) +
          ggplot2::labs(title = sprintf("MLP (%s) — matrice de confusion (test)", f$fit$engine),
                        caption = "Pourcentages par ligne : part de chaque classe observée. Diagonale = bien classés.")
        hstat_apply_plot_opts(g, input, "dlO")
      }
    })
    output$dlPlot <- renderPlot(dl_gg())
    output$dlPlDl <- hstat_export_plot_handler(input, "dlPl",
                       function() dl_gg(), "dl_diagnostic")

    # ---- Métriques + interprétation --------------------------------------------------
    output$dlMetrics <- DT::renderDataTable(
      DT::datatable(dlfit()$metrics, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE)))
    hstat_export_table_handlers(output, "dlMet",
      function() dlfit()$metrics, "dl_metriques")

    output$dlInterp <- renderUI({
      f <- dlfit()
      div(class = "callout callout-info", style = "margin-top:10px;",
          icon("lightbulb"), strong(" Interprétation : "),
          hstat_model_interpretation(
            f$p$task, f$metrics,
            sprintf("réseau de neurones %s (couches cachées : %s)",
                    f$fit$engine, paste(f$hidden, collapse = "-")),
            f$p$n_train, f$p$n_test,
            notes = "Un écart important entre performance d'entraînement et de test signale du surapprentissage : réduire les couches, augmenter les données ou la régularisation."))
    })

    # ---- Architecture + courbe d'apprentissage -----------------------------------------
    output$dlArch <- renderUI({
      f <- dlfit()
      div(class = "callout callout-info",
          icon("diagram-project"), strong(" Architecture : "),
          sprintf("%d entrées → %s → %s ; %s paramètres entraînés (moteur %s).",
                  ncol(f$p$x_train), paste(f$hidden, collapse = " → "),
                  if (f$p$task == "classification")
                    sprintf("%d sorties (classes)", nlevels(f$p$y_train))
                  else "1 sortie (valeur)",
                  format(round(f$fit$n_params), big.mark = " "), f$fit$engine))
    })

    dl_loss_gg <- reactive({
      f <- dlfit()
      validate(need(!is.null(f$fit$losses),
        "Courbe d'apprentissage détaillée disponible avec le moteur torch (neuralnet ne renvoie que l'erreur finale)."))
      d <- data.frame(epoque = seq_along(f$fit$losses), perte = f$fit$losses)
      ggplot2::ggplot(d, ggplot2::aes(epoque, perte)) +
        ggplot2::geom_line(color = hstat_plot_opt(input, "dlO", "Col", "#8e44ad"),
                           linewidth = 0.8) +
        ggplot2::labs(title = "Courbe d'apprentissage",
                      x = "Époque", y = "Perte moyenne") +
        ggplot2::theme_minimal(base_size = 12)
    })
    output$dlLoss <- renderPlot(dl_loss_gg())
    output$dlLoDl <- hstat_export_plot_handler(input, "dlLo",
                       function() dl_loss_gg(), "courbe_apprentissage")

    # ---- Simulateur -----------------------------------------------------------------
    output$dlSimForm <- renderUI({
      f <- tryCatch(dlfit(), error = function(e) NULL)
      if (is.null(f)) return(tags$em("Entraînez d'abord le réseau."))
      hstat_sim_inputs_ui(ns, f$p$d, f$p$preds, "dlsv")
    })

    predict_original <- function(f, nd_original) {
      al <- hstat_align_newdata(nd_original, f$p$d, f$p$preds)
      if (is.null(al$data)) return(al)
      keep <- stats::complete.cases(al$data)
      if (!any(keep)) return(list(data = NULL, warn = "Aucune ligne complète exploitable."))
      out <- f$fit$predict_fun(f$p$make_x(al$data[keep, , drop = FALSE]))
      list(data = out, keep = keep, warn = al$warn)
    }

    observeEvent(input$dlSimOne, {
      output$dlSimOneOut <- renderUI({
        f <- dlfit()
        nd0 <- hstat_sim_collect(input, f$p$d, f$p$preds, "dlsv")
        r <- predict_original(f, nd0)
        validate(need(!is.null(r$data), r$warn %||% "Saisie invalide."))
        out <- r$data
        val <- if (f$p$task == "regression")
          format(round(as.numeric(out$pred)[1], 4), big.mark = " ")
          else as.character(out$pred[1])
        conf <- if (f$p$task == "classification" && !is.null(out$prob))
          sprintf(" (confiance : %.1f %%)", 100 * max(out$prob[1, ])) else ""
        div(class = "callout callout-info", style = "margin-top:10px;",
            icon("bullseye"),
            strong(sprintf(" Prédiction de « %s » : %s%s.", f$p$target, val, conf)))
      })
    })

    dl_sim_batch <- reactive({
      req(input$dlSimFile$datapath)
      f <- dlfit()
      nd0 <- tryCatch({
        if (grepl("\\.xlsx$", input$dlSimFile$name, ignore.case = TRUE))
          as.data.frame(readxl::read_excel(input$dlSimFile$datapath))
        else utils::read.csv(input$dlSimFile$datapath, check.names = FALSE)
      }, error = function(e) NULL)
      validate(need(!is.null(nd0), "Fichier importé illisible."))
      r <- predict_original(f, nd0)
      validate(need(!is.null(r$data), r$warn %||% "Colonnes incompatibles."))
      if (!is.null(r$warn)) showNotification(r$warn, type = "warning", duration = 8)
      res <- nd0[r$keep, , drop = FALSE]
      res[[paste0("Prediction_", f$p$target)]] <-
        if (f$p$task == "regression") round(as.numeric(r$data$pred), 4)
        else as.character(r$data$pred)
      if (f$p$task == "classification" && !is.null(r$data$prob))
        res$Confiance <- round(apply(r$data$prob, 1, max), 4)
      res
    })
    output$dlSimBatch <- DT::renderDataTable(
      DT::datatable(dl_sim_batch(), rownames = FALSE,
                    options = list(pageLength = 8, scrollX = TRUE)))
    hstat_export_table_handlers(output, "dlSimB", function() dl_sim_batch(),
                                "dl_predictions_nouveaux_cas")
    output$dlSimBatchInterp <- renderUI({
      d <- tryCatch(dl_sim_batch(), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      f <- dlfit()
      pc <- d[[paste0("Prediction_", f$p$target)]]
      div(class = "callout callout-info", style = "margin-top:8px;", icon("lightbulb"),
          if (f$p$task == "regression")
            sprintf(" %d cas prédits (moyenne = %s).", nrow(d),
                    format(round(mean(as.numeric(pc)), 3), big.mark = " "))
          else sprintf(" %d cas prédits ; classe majoritaire : « %s ».",
                       nrow(d), names(sort(table(pc), decreasing = TRUE))[1]))
    })

    # ================= LSTM =================
    lstm_res <- eventReactive(input$lstmRun, {
      validate(need(has_torch(),
        "Bibliothèques torch non installées : utilisez le bouton de téléchargement ci-dessus."))
      df <- values$cleanData
      validate(need(!is.null(df), "Chargez d'abord des données."),
               need(nzchar(input$lstmVar %||% ""), "Choisissez une série numérique."))
      y <- suppressWarnings(as.numeric(df[[input$lstmVar]]))
      y <- y[is.finite(y)]
      L <- as.integer(max(2, hstat_finite(input$lstmLook, 12)))
      n_test <- as.integer(max(2, hstat_finite(input$lstmTestN, 12)))
      validate(need(length(y) >= L + n_test + 10,
        sprintf("Série trop courte : au moins %d observations requises.", L + n_test + 10)))
      ctr <- mean(y); scl <- max(stats::sd(y), 1e-9)
      z <- (y - ctr) / scl
      # Séquences glissantes (fenêtre L -> valeur suivante)
      n_seq <- length(z) - L
      X <- t(vapply(seq_len(n_seq), function(i) z[i:(i + L - 1)], numeric(L)))
      Y <- z[(L + 1):length(z)]
      tr_ix <- seq_len(n_seq - n_test)
      torch::torch_manual_seed(123)
      H <- as.integer(max(4, hstat_finite(input$lstmHidden, 32)))
      net <- torch::nn_module(
        initialize = function(L, H) {
          self$lstm <- torch::nn_lstm(input_size = 1, hidden_size = H, batch_first = TRUE)
          self$fc <- torch::nn_linear(H, 1)
        },
        forward = function(x) {
          o <- self$lstm(x)[[1]]
          self$fc(o[, dim(o)[2], ])
        })
      model <- net(L, H)
      xt <- torch::torch_tensor(array(X[tr_ix, , drop = FALSE],
                                      dim = c(length(tr_ix), L, 1)),
                                dtype = torch::torch_float())
      yt <- torch::torch_tensor(matrix(Y[tr_ix], ncol = 1),
                                dtype = torch::torch_float())
      opt <- torch::optim_adam(model$parameters, lr = hstat_finite(input$lstmLr, 0.005))
      epochs <- as.integer(max(10, hstat_finite(input$lstmEpochs, 80)))
      losses <- numeric(epochs)
      withProgress(message = "Entraînement du LSTM", value = 0, {
        for (e in seq_len(epochs)) {
          opt$zero_grad()
          l <- torch::nnf_mse_loss(model(xt), yt)
          l$backward(); opt$step()
          losses[e] <- l$item()
          incProgress(1 / epochs, detail = sprintf("époque %d — perte %.5f", e, losses[e]))
        }
      })
      model$eval()
      pred1 <- function(win) { # une fenêtre -> valeur suivante (échelle z)
        xn <- torch::torch_tensor(array(win, dim = c(1, L, 1)),
                                  dtype = torch::torch_float())
        as.numeric(torch::with_no_grad(model(xn)))
      }
      # Test : prédiction 1 pas sur les fenêtres réelles
      te_ix <- (n_seq - n_test + 1):n_seq
      pred_test_z <- vapply(te_ix, function(i) pred1(X[i, ]), numeric(1))
      pred_test <- pred_test_z * scl + ctr
      obs_test  <- Y[te_ix] * scl + ctr
      # Futur : prévision récursive multi-pas
      h <- as.integer(max(1, hstat_finite(input$lstmH, 12)))
      win <- utils::tail(z, L); fut_z <- numeric(h)
      for (i in seq_len(h)) {
        fut_z[i] <- pred1(win)
        win <- c(win[-1], fut_z[i])
      }
      list(y = y, obs_test = obs_test, pred_test = pred_test,
           future = fut_z * scl + ctr, losses = losses, L = L, H = H, h = h,
           metrics = hstat_metrics_reg(obs_test, pred_test), n_test = n_test)
    })

    lstm_gg <- reactive({
      r <- lstm_res(); req(r)
      n <- length(r$y)
      d_hist <- data.frame(t = seq_len(n), y = r$y, quoi = "Série observée")
      d_test <- data.frame(t = (n - r$n_test + 1):n, y = r$pred_test,
                           quoi = "Prédiction (test)")
      d_fut  <- data.frame(t = n + seq_len(r$h), y = r$future,
                           quoi = "Prévision future")
      d <- rbind(d_hist, d_test, d_fut)
      ggplot2::ggplot(d, ggplot2::aes(t, y, color = quoi)) +
        ggplot2::geom_line(linewidth = 0.7) +
        ggplot2::scale_color_manual(values = c("Série observée" = "#37474f",
                                               "Prédiction (test)" = "#e67e22",
                                               "Prévision future" = "#8e44ad")) +
        ggplot2::labs(title = sprintf("LSTM (fenêtre %d, %d neurones) — test + futur",
                                      r$L, r$H),
                      x = "Temps", y = "Valeur", color = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    })
    output$lstmPlot <- renderPlot(lstm_gg())
    output$lstmPlDl <- hstat_export_plot_handler(input, "lstmPl",
                         function() lstm_gg(), "lstm_prevision")

    output$lstmMetrics <- DT::renderDataTable(
      DT::datatable(lstm_res()$metrics, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE)))
    hstat_export_table_handlers(output, "lstmMet",
      function() lstm_res()$metrics, "lstm_metriques")

    lstm_future_df <- reactive({
      r <- lstm_res()
      data.frame(Periode = seq_len(r$h), Prevision = round(r$future, 4))
    })
    output$lstmFuture <- DT::renderDataTable(
      DT::datatable(lstm_future_df(), rownames = FALSE,
                    options = list(pageLength = 8, dom = "tp")))
    hstat_export_table_handlers(output, "lstmFut",
      function() lstm_future_df(), "lstm_previsions_futures")

    lstm_loss_gg <- reactive({
      r <- lstm_res()
      d <- data.frame(epoque = seq_along(r$losses), perte = r$losses)
      ggplot2::ggplot(d, ggplot2::aes(epoque, perte)) +
        ggplot2::geom_line(color = "#8e44ad", linewidth = 0.8) +
        ggplot2::labs(title = "Courbe d'apprentissage (LSTM)",
                      x = "Époque", y = "Perte (MSE)") +
        ggplot2::theme_minimal(base_size = 12)
    })
    output$lstmLoss <- renderPlot(lstm_loss_gg())
    output$lstmLoDl <- hstat_export_plot_handler(input, "lstmLo",
                         function() lstm_loss_gg(), "lstm_courbe_apprentissage")

    output$lstmInterp <- renderUI({
      r <- lstm_res()
      v <- function(m) { i <- match(m, r$metrics$Metrique)
                         if (is.na(i)) NA else r$metrics$Valeur[i] }
      converge <- length(r$losses) > 5 &&
        utils::tail(r$losses, 1) < r$losses[1] * 0.9
      div(class = "callout callout-info", style = "margin-top:10px;",
          icon("lightbulb"), strong(" Interprétation : "),
          sprintf("Le LSTM apprend à prédire chaque valeur à partir des %d précédentes. Sur le jeu de test, la MAPE vaut %s %% : %s ",
                  r$L, v("MAPE (%)"), .hstat_interp_mape(v("MAPE (%)"))),
          if (converge) "La courbe d'apprentissage décroît nettement : l'entraînement a convergé. "
          else "La perte décroît peu : augmenter les époques, le taux d'apprentissage ou la fenêtre. ",
          "Les prévisions futures étant récursives (chaque prédiction nourrit la suivante), leur fiabilité décroît avec l'horizon : accorder plus de crédit aux premières périodes.")
    })
  })
}
