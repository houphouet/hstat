#  Module Shiny : Modélisation en séries temporelles
#  Naïf, dérive, moyennes, lissages exponentiels (SES/Holt/Holt-Winters), ETS,
#  ARIMA/SARIMA (auto & manuel), TBATS, Thêta, STL+ETS, NNAR (réseau de
#  neurones autorégressif) et Prophet (optionnel). Comparaison automatique,
#  diagnostics des résidus, interprétations, simulateur de prévisions,
#  exports tableaux (CSV/Excel) et images (PNG/JPG/TIFF/BMP/PDF/SVG, <= 20 000 DPI).

# -- Catalogue des modèles ------------------------------------------------------
.ts_catalog <- function() c(
  "Naïf (dernière valeur)"                    = "naive",
  "Naïf saisonnier"                           = "snaive",
  "Moyenne historique"                        = "meanf",
  "Marche aléatoire avec dérive"              = "drift",
  "Lissage exponentiel simple (SES)"          = "ses",
  "Holt (tendance)"                           = "holt",
  "Holt amorti (tendance amortie)"            = "holtd",
  "Holt-Winters additif"                      = "hwadd",
  "Holt-Winters multiplicatif"                = "hwmul",
  "ÉTS (sélection automatique)"               = "ets",
  "ARIMA automatique (auto.arima)"            = "arima",
  "SARIMA manuel (p,d,q)(P,D,Q)"              = "sarima",
  "TBATS (saisonnalités complexes)"           = "tbats",
  "Méthode Thêta"                             = "theta",
  "STL + ÉTS (décomposition)"                 = "stlf",
  "NNAR (réseau de neurones AR)"              = "nnetar",
  "DLM — modèle linéaire dynamique (Kalman)"  = "dlmts",
  "DLNM — retards distribués non linéaires"   = "dlnm",
  "Prophet (nécessite des dates)"             = "prophet")

mod_timeseries_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(title = shiny::tagList(shiny::icon("sliders"), " Configuration"), status = "primary",
          width = 4, solidHeader = TRUE, collapsible = TRUE,
          shiny::selectInput(ns("tsVar"), "Variable à prévoir (numérique)", choices = NULL),
          shiny::selectInput(ns("tsDate"), "Colonne de dates (optionnelle)",
                      choices = c("— Aucune (ordre des lignes) —" = "")),
          shiny::fluidRow(
            shiny::column(6, shiny::selectInput(ns("tsFreq"), "Fréquence saisonnière",
                     choices = c("Aucune (1)" = 1, "Trimestrielle (4)" = 4,
                                 "Mensuelle (12)" = 12, "Hebdo — jours (7)" = 7,
                                 "Journalière — heures (24)" = 24,
                                 "Annuelle — semaines (52)" = 52,
                                 "Annuelle — jours (365)" = 365),
                     selected = 12)),
            shiny::column(6, shiny::numericInput(ns("tsFreqCustom"),
                     "…ou fréquence libre", value = NA, min = 1, step = 1))),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("tsTestN"), "Taille du jeu de test",
                                   value = 12, min = 2, step = 1)),
            shiny::column(6, shiny::numericInput(ns("tsHorizon"), "Horizon futur (h)",
                                   value = 12, min = 1, step = 1))),
          shiny::checkboxGroupInput(ns("tsModels"), "Modèles à comparer",
            choices = .ts_catalog(),
            selected = c("naive", "ses", "ets", "arima", "theta")),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] && input['%s'].indexOf('dlnm') > -1",
                                ns("tsModels"), ns("tsModels")),
            shiny::selectInput(ns("dlnmExpo"),
                        "DLNM : variable d'exposition (ex. température, pollution)",
                        choices = NULL),
            shiny::numericInput(ns("dlnmLag"), "DLNM : décalage maximal (lags)",
                         value = 14, min = 1, max = 60, step = 1),
            shiny::tags$small(style = "color:#6b7280;",
              "Le DLNM modélise l'effet retardé et non linéaire de l'exposition ",
              "sur la série (usage classique en épidémiologie environnementale).")),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] && input['%s'].indexOf('sarima') > -1",
                                ns("tsModels"), ns("tsModels")),
            shiny::fluidRow(
              shiny::column(4, shiny::numericInput(ns("sar_p"), "p", 1, min = 0, max = 5)),
              shiny::column(4, shiny::numericInput(ns("sar_d"), "d", 1, min = 0, max = 2)),
              shiny::column(4, shiny::numericInput(ns("sar_q"), "q", 1, min = 0, max = 5))),
            shiny::fluidRow(
              shiny::column(4, shiny::numericInput(ns("sar_P"), "P", 0, min = 0, max = 2)),
              shiny::column(4, shiny::numericInput(ns("sar_D"), "D", 0, min = 0, max = 1)),
              shiny::column(4, shiny::numericInput(ns("sar_Q"), "Q", 0, min = 0, max = 2)))),
          shiny::actionButton(ns("tsRun"), "Entraîner et comparer",
                       icon = shiny::icon("play"), class = "btn-primary"),
          shiny::tags$small(style = "color:#6b7280; display:block; margin-top:8px;",
            "Les valeurs manquantes internes sont interpolées ; chaque modèle est ",
            "évalué sur les dernières observations mises de côté (jeu de test).")),
      shinydashboard::box(title = shiny::tagList(shiny::icon("trophy"), " Comparaison des modèles"),
          status = "success", width = 8, solidHeader = TRUE,
          withSpinner(DT::dataTableOutput(ns("tsCompare"))),
          hstat_export_table_ui(ns, "tsComp"),
          shiny::uiOutput(ns("tsBest")))),
    shiny::fluidRow(
      shinydashboard::box(title = shiny::tagList(shiny::icon("chart-line"), " Prévisions du modèle sélectionné"),
          status = "primary", width = 8, solidHeader = TRUE,
          shiny::selectInput(ns("tsShow"), "Modèle affiché", choices = NULL),
          shiny::uiOutput(ns("tsDoc")),
          withSpinner(shiny::plotOutput(ns("tsPlot"), height = "430px")),
          shiny::tabsetPanel(
            shiny::tabPanel("Téléchargement", shiny::div(style = "padding-top:10px;",
                     hstat_export_plot_ui(ns, "tsPl", theme = FALSE))),
            shiny::tabPanel("Apparence", shiny::div(style = "padding-top:10px;",
                     hstat_plot_opts_ui(ns, "tsO"))))),
      shinydashboard::box(title = shiny::tagList(shiny::icon("table"), " Métriques & interprétation"),
          status = "info", width = 4, solidHeader = TRUE,
          DT::dataTableOutput(ns("tsMetrics")),
          hstat_export_table_ui(ns, "tsMet"),
          shiny::uiOutput(ns("tsInterp")))),
    shiny::fluidRow(
      shinydashboard::box(title = shiny::tagList(shiny::icon("stethoscope"), " Diagnostics des résidus"),
          status = "warning", width = 6, solidHeader = TRUE,
          shiny::plotOutput(ns("tsResid"), height = "330px"),
          hstat_export_plot_ui(ns, "tsRe", width = 9, height = 5, theme = FALSE),
          shiny::uiOutput(ns("tsLjung"))),
      shinydashboard::box(title = shiny::tagList(shiny::icon("layer-group"), " Décomposition (STL)"),
          status = "warning", width = 6, solidHeader = TRUE,
          shiny::plotOutput(ns("tsDecomp"), height = "330px"),
          hstat_export_plot_ui(ns, "tsDe", width = 9, height = 6, theme = FALSE),
          shiny::tags$small(style = "color:#6b7280; display:block; margin-top:8px;",
            shiny::strong("Conditions : "), "la décomposition STL porte sur la série ",
            "elle-même (quel que soit le modèle affiché) et n'est disponible que si ",
            "la fréquence saisonnière est > 1 avec au moins 2 saisons complètes ",
            "(ex. 24 points en mensuel). Elle éclaire directement les modèles ",
            "saisonniers : naïf saisonnier, Holt-Winters, SARIMA, TBATS et STL+ÉTS ",
            "(qui l'utilise en interne). Pour une série de fréquence 1 (naïf, ",
            "dérive, SES, Holt, Thêta, DLM sans saison, DLNM...), elle est sans objet."))),
    shiny::fluidRow(
      shinydashboard::box(title = shiny::tagList(shiny::icon("magic"), " Simulateur de prévisions"),
          status = "success", width = 12, solidHeader = TRUE,
          shiny::fluidRow(
            shiny::column(4,
              shiny::numericInput(ns("simH"), "Horizon à simuler (périodes futures)",
                           value = 12, min = 1, step = 1),
              shiny::textAreaInput(ns("simManual"),
                "Saisir de nouvelles valeurs (séparées par des virgules, espaces ou retours à la ligne)",
                placeholder = "ex. : 152,3  148,9  151,2  ou  152.3 148.9 151.2",
                rows = 2),
              shiny::fileInput(ns("simFile"),
                        "Ou importer un fichier (CSV/Excel)",
                        accept = c(".csv", ".xlsx")),
              shiny::tags$small(style = "color:#6b7280; display:block;",
                "Pour les modèles classiques, ces valeurs sont ajoutées à la fin de ",
                "la série avant ré-entraînement. Pour le DLNM, elles sont interprétées ",
                "comme les expositions futures. La saisie manuelle prime sur le fichier."),
              shiny::tags$small(style = "color:#6b7280;",
                "Le fichier doit contenir une colonne portant le même nom que la ",
                "variable prévue ; ses valeurs sont ajoutées à la fin de la série ",
                "avant ré-entraînement."),
              shiny::actionButton(ns("simRun"), "Simuler les prévisions",
                           icon = shiny::icon("forward"), class = "btn-success")),
            shiny::column(8,
              withSpinner(shiny::plotOutput(ns("simPlot"), height = "330px")),
              hstat_export_plot_ui(ns, "simPl", theme = FALSE))),
          shiny::fluidRow(shiny::column(12,
            DT::dataTableOutput(ns("simTable")),
            hstat_export_table_ui(ns, "simTab"),
            shiny::uiOutput(ns("simInterp"))))))
  )
}

mod_timeseries_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Choix de variables synchronisés avec les données -------------------
    shiny::observe({
      df <- values$cleanData
      shiny::req(df)
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      shiny::updateSelectInput(session, "tsVar", choices = num,
                        selected = if (length(num)) num[1] else NULL)
      dt_like <- names(df)[vapply(df, function(x)
        inherits(x, c("Date", "POSIXct")) || is.character(x) || is.factor(x),
        logical(1))]
      shiny::updateSelectInput(session, "tsDate",
        choices = c("— Aucune (ordre des lignes) —" = "", dt_like))
      shiny::updateSelectInput(session, "dlnmExpo", choices = num,
                        selected = if (length(num) > 1) num[2] else NULL)
    })

    # ---- Construction de la série ts ----------------------------------------
    ts_freq <- shiny::reactive({
      f <- hstat_finite(input$tsFreqCustom, NA)
      if (is.finite(f) && f >= 1) as.integer(round(f))
      else as.integer(input$tsFreq %||% 1)
    })

    build_series <- function(df) {
      y <- suppressWarnings(as.numeric(df[[input$tsVar]]))
      row_idx <- seq_along(y)
      dts <- NULL
      if (nzchar(input$tsDate %||% "")) {
        raw <- df[[input$tsDate]]
        dts <- if (inherits(raw, c("Date", "POSIXct"))) as.Date(raw)
               else suppressWarnings(as.Date(as.character(raw)))
        if (all(is.na(dts))) dts <- NULL
        if (!is.null(dts)) { o <- order(dts); y <- y[o]; dts <- dts[o]
                             row_idx <- row_idx[o] }
      }
      keep <- which(is.finite(y))
      if (length(keep) < 8) stop("Au moins 8 observations valides sont requises.")
      rng <- seq(min(keep), max(keep))
      y <- y[rng]; row_idx <- row_idx[rng]
      if (!is.null(dts)) dts <- dts[rng]
      # Interpolation des trous internes (forecast::na.interp si dispo)
      if (anyNA(y)) {
        y <- if (requireNamespace("forecast", quietly = TRUE))
               as.numeric(forecast::na.interp(stats::ts(y, frequency = max(1, ts_freq()))))
             else stats::approx(seq_along(y), y, xout = seq_along(y), rule = 2)$y
      }
      list(y = stats::ts(y, frequency = max(1, ts_freq())), dates = dts,
           row_idx = row_idx)
    }

    # Objet 'forecast' minimal a partir d'une moyenne et d'un ecart-type ------
    .ts_fc_obj <- function(mu, se, train, method) {
      fq <- stats::frequency(train); tsp <- stats::tsp(train)
      z80 <- stats::qnorm(0.90); z95 <- stats::qnorm(0.975)
      fc <- list(mean  = stats::ts(mu, start = tsp[2] + 1 / fq, frequency = fq),
                 lower = cbind(`80%` = mu - z80 * se, `95%` = mu - z95 * se),
                 upper = cbind(`80%` = mu + z80 * se, `95%` = mu + z95 * se),
                 x = train, fitted = rep(NA_real_, length(train)),
                 residuals = rep(NA_real_, length(train)), method = method)
      class(fc) <- "forecast"
      fc
    }

    # ---- Ajustement d'un modèle du catalogue --------------------------------
    fit_one <- function(idm, train, h, dates_train = NULL,
                        expo = NULL, expo_future = NULL) {
      if (!requireNamespace("forecast", quietly = TRUE) && idm != "prophet")
        stop("Le package 'forecast' est requis (install.packages(\"forecast\")).")
      f <- stats::frequency(train)
      season_ok <- f > 1 && length(train) >= 2 * f
      as_fc <- function(fc, aic = NA_real_) list(fc = fc, aic = aic)
      switch(idm,
        naive  = as_fc(forecast::naive(train, h = h)),
        snaive = { if (!season_ok) stop("Naïf saisonnier : fréquence > 1 et au moins 2 saisons requises.")
                   as_fc(forecast::snaive(train, h = h)) },
        meanf  = as_fc(forecast::meanf(train, h = h)),
        drift  = as_fc(forecast::rwf(train, h = h, drift = TRUE)),
        ses    = { m <- forecast::ses(train, h = h);  as_fc(m, m$model$aic %||% NA_real_) },
        holt   = { m <- forecast::holt(train, h = h); as_fc(m, m$model$aic %||% NA_real_) },
        holtd  = { m <- forecast::holt(train, h = h, damped = TRUE)
                   as_fc(m, m$model$aic %||% NA_real_) },
        hwadd  = { if (!season_ok) stop("Holt-Winters : série saisonnière requise.")
                   m <- forecast::hw(train, h = h, seasonal = "additive")
                   as_fc(m, m$model$aic %||% NA_real_) },
        hwmul  = { if (!season_ok) stop("Holt-Winters : série saisonnière requise.")
                   if (any(train <= 0)) stop("Holt-Winters multiplicatif : valeurs strictement positives requises.")
                   m <- forecast::hw(train, h = h, seasonal = "multiplicative")
                   as_fc(m, m$model$aic %||% NA_real_) },
        ets    = { m <- forecast::ets(train); as_fc(forecast::forecast(m, h = h), m$aic) },
        arima  = { m <- forecast::auto.arima(train)
                   as_fc(forecast::forecast(m, h = h), stats::AIC(m)) },
        sarima = { m <- forecast::Arima(train,
                     order    = c(hstat_finite(input$sar_p, 1), hstat_finite(input$sar_d, 1),
                                  hstat_finite(input$sar_q, 1)),
                     seasonal = c(hstat_finite(input$sar_P, 0), hstat_finite(input$sar_D, 0),
                                  hstat_finite(input$sar_Q, 0)))
                   as_fc(forecast::forecast(m, h = h), stats::AIC(m)) },
        tbats  = { m <- forecast::tbats(train)
                   as_fc(forecast::forecast(m, h = h), m$AIC %||% NA_real_) },
        theta  = as_fc(forecast::thetaf(train, h = h)),
        stlf   = { if (!season_ok) stop("STL : série saisonnière requise.")
                   as_fc(forecast::stlf(train, h = h)) },
        nnetar = { m <- forecast::nnetar(train)
                   as_fc(forecast::forecast(m, h = h, PI = FALSE)) },
        dlmts  = {
          if (!requireNamespace("dlm", quietly = TRUE))
            stop("Le package 'dlm' est requis (install.packages(\"dlm\")).")
          seas <- season_ok
          build <- function(par) {
            m <- dlm::dlmModPoly(order = 2, dV = exp(par[1]), dW = exp(par[2:3]))
            if (seas) m <- m + dlm::dlmModSeas(f, dV = 0,
                                               dW = c(exp(par[4]), rep(0, f - 2)))
            m
          }
          npar <- if (seas) 4L else 3L
          est <- dlm::dlmMLE(as.numeric(train), parm = rep(-2, npar), build = build)
          if (!est$convergence %in% c(0L, 1L))
            stop("DLM : l'estimation du maximum de vraisemblance n'a pas convergé.")
          filt <- dlm::dlmFilter(as.numeric(train), build(est$par))
          fc0 <- dlm::dlmForecast(filt, nAhead = h)
          mu <- as.numeric(fc0$f)
          se <- sqrt(vapply(fc0$Q, function(q) q[1, 1], numeric(1)))
          as_fc(.ts_fc_obj(mu, se, train, "DLM"), 2 * npar + 2 * est$value)
        },
        dlnm   = {
          if (!requireNamespace("dlnm", quietly = TRUE))
            stop("Le package 'dlnm' est requis (install.packages(\"dlnm\")).")
          if (is.null(expo) || is.null(expo_future))
            stop("DLNM : choisissez une variable d'exposition dans la configuration.")
          L <- as.integer(max(1, min(60, hstat_finite(input$dlnmLag, 14))))
          x_all <- c(expo, expo_future)
          if (length(train) <= L + 10)
            stop("DLNM : série trop courte pour ce décalage maximal (réduire les lags).")
          cb <- dlnm::crossbasis(x_all, lag = L,
                                 argvar = list(fun = "ns", df = 3),
                                 arglag = list(fun = "ns", df = 3))
          n_tr <- length(train)
          X <- cbind(1, unclass(cb), seq_along(x_all))
          y_tr <- as.numeric(train)
          is_count <- all(y_tr >= 0) && all(abs(y_tr - round(y_tr)) < 1e-8)
          fam <- if (is_count) stats::quasipoisson() else stats::gaussian()
          rows <- which(seq_along(x_all) <= n_tr & stats::complete.cases(X))
          fit <- suppressWarnings(stats::glm.fit(X[rows, , drop = FALSE],
                                                 y_tr[rows], family = fam))
          beta <- fit$coefficients
          beta[!is.finite(beta)] <- 0
          mu_all <- fam$linkinv(as.numeric(X %*% beta))
          res_tr <- rep(NA_real_, n_tr)
          res_tr[rows] <- y_tr[rows] - mu_all[rows]
          fc <- .ts_fc_obj(mu_all[(n_tr + 1):(n_tr + h)],
                           rep(stats::sd(res_tr, na.rm = TRUE), h), train,
                           if (is_count) "DLNM (quasi-Poisson)" else "DLNM (gaussien)")
          fc$residuals <- res_tr
          as_fc(fc)
        },
        prophet = {
          if (!requireNamespace("prophet", quietly = TRUE))
            stop("Prophet n'est pas installé (install.packages(\"prophet\")).")
          if (is.null(dates_train)) stop("Prophet nécessite une colonne de dates.")
          dfp <- data.frame(ds = dates_train, y = as.numeric(train))
          m <- suppressMessages(prophet::prophet(dfp))
          step <- if (length(dates_train) > 1)
            stats::median(as.numeric(diff(dates_train))) else 1
          fut <- data.frame(ds = max(dates_train) + step * seq_len(h))
          pr <- stats::predict(m, fut)
          fc <- list(mean  = stats::ts(pr$yhat, start = stats::tsp(train)[2] + 1 / stats::frequency(train),
                                       frequency = stats::frequency(train)),
                     lower = cbind(`80%` = pr$yhat_lower, `95%` = pr$yhat_lower),
                     upper = cbind(`80%` = pr$yhat_upper, `95%` = pr$yhat_upper),
                     x = train, fitted = rep(NA_real_, length(train)),
                     residuals = rep(NA_real_, length(train)),
                     method = "Prophet")
          class(fc) <- "forecast"
          as_fc(fc)
        },
        stop("Modèle inconnu : ", idm))
    }

    # ---- Entraînement + comparaison ------------------------------------------
    # Depot du resultat de prevision pour l'aide a la decision.
    shiny::observeEvent(fits(), {
      f <- tryCatch(fits(), error = function(e) NULL)
      if (is.null(f)) return()
      mets <- tryCatch(do.call(rbind, lapply(names(f), function(nm) {
        r <- f[[nm]]
        if (is.null(r) || isFALSE(r$ok) || is.null(r$metrics)) return(NULL)
        cbind(Modele = nm, r$metrics)
      })), error = function(e) NULL)
      if (is.null(mets) || !NROW(mets)) return()
      hstat_ai_capture(values, "Séries temporelles",
        "Prévision et comparaison de modèles",
        tables = list("Qualité de prévision" = as.data.frame(mets)),
        meta = list(variables = input$tsVar, `horizon` = input$tsHorizon))
    }, ignoreInit = TRUE)

    fits <- shiny::eventReactive(input$tsRun, {
      df <- values$cleanData
      shiny::validate(shiny::need(!is.null(df), "Chargez d'abord des données."),
               shiny::need(nzchar(input$tsVar %||% ""), "Choisissez une variable numérique."))
      ser <- tryCatch(build_series(df), error = function(e) e)
      shiny::validate(shiny::need(!inherits(ser, "error"),
                    if (inherits(ser, "error")) conditionMessage(ser) else ""))
      y <- ser$y
      n <- length(y)
      n_test <- max(2, min(hstat_finite(input$tsTestN, 12), floor(n / 3)))
      train <- stats::window(y, end = stats::time(y)[n - n_test])
      test  <- as.numeric(utils::tail(as.numeric(y), n_test))
      dtr   <- if (!is.null(ser$dates)) utils::head(ser$dates, n - n_test) else NULL
      cat_all <- .ts_catalog()
      ids <- input$tsModels %||% character(0)
      shiny::validate(shiny::need(length(ids) > 0, "Sélectionnez au moins un modèle."))
      expo_ser <- NULL
      if ("dlnm" %in% ids) {
        shiny::validate(shiny::need(nzchar(input$dlnmExpo %||% ""),
                      "DLNM : choisissez la variable d'exposition."),
                 shiny::need(!identical(input$dlnmExpo, input$tsVar),
                      "DLNM : l'exposition doit différer de la variable prévue."))
        ex <- suppressWarnings(as.numeric(df[[input$dlnmExpo]]))[ser$row_idx]
        if (anyNA(ex))
          ex <- stats::approx(seq_along(ex), ex, xout = seq_along(ex), rule = 2)$y
        shiny::validate(shiny::need(all(is.finite(ex)),
                      "DLNM : exposition invalide sur la plage de la série."))
        expo_ser <- ex
      }
      res <- list()
      shiny::withProgress(message = "Entraînement des modèles", value = 0, {
        for (i in seq_along(ids)) {
          idm <- ids[i]
          shiny::incProgress(1 / length(ids), detail = names(cat_all)[match(idm, cat_all)])
          res[[idm]] <- tryCatch({
            r <- fit_one(idm, train, h = n_test, dates_train = dtr,
                         expo = if (is.null(expo_ser)) NULL
                                else utils::head(expo_ser, n - n_test),
                         expo_future = if (is.null(expo_ser)) NULL
                                       else utils::tail(expo_ser, n_test))
            pred <- as.numeric(r$fc$mean)
            mets <- hstat_metrics_reg(test, pred)
            # MASE : MAE / MAE du naïf (saisonnier si possible) sur le train
            lag  <- if (stats::frequency(train) > 1 &&
                        length(train) > stats::frequency(train)) stats::frequency(train) else 1
            scale <- mean(abs(diff(as.numeric(train), lag = lag)))
            mase <- if (is.finite(scale) && scale > 0)
              mean(abs(test - pred)) / scale else NA_real_
            list(ok = TRUE, fc = r$fc, aic = r$aic, metrics = mets,
                 mase = mase, pred = pred)
          }, error = function(e) list(ok = FALSE, err = conditionMessage(e)))
        }
      })
      list(res = res, y = y, train = train, test = test, n_test = n_test,
           dates = ser$dates, labels = cat_all, expo = expo_ser)
    })

    # Table comparative -------------------------------------------------------
    comp_df <- shiny::reactive({
      f <- fits(); shiny::req(f)
      rows <- lapply(names(f$res), function(idm) {
        r <- f$res[[idm]]
        lab <- names(f$labels)[match(idm, f$labels)]
        if (!isTRUE(r$ok))
          return(data.frame(Modele = lab, RMSE = NA, MAE = NA, `MAPE (%)` = NA,
                            MASE = NA, AIC = NA, Statut = trf("Échec : %s", r$err),
                            check.names = FALSE))
        v <- function(m) { i <- match(m, r$metrics$Metrique)
                           if (is.na(i)) NA else r$metrics$Valeur[i] }
        data.frame(Modele = lab, RMSE = v("RMSE"), MAE = v("MAE"),
                   `MAPE (%)` = v("MAPE (%)"), MASE = round(r$mase, 4),
                   AIC = if (is.finite(r$aic)) round(r$aic, 1) else NA,
                   Statut = "OK", check.names = FALSE)
      })
      out <- do.call(rbind, rows)
      out[order(out$RMSE), , drop = FALSE]
    })

    output$tsCompare <- DT::renderDataTable({
      DT::datatable(comp_df(), rownames = FALSE,
                    options = list(dom = "t", pageLength = 25, scrollX = TRUE))
    })
    hstat_export_table_handlers(output, "tsComp", function() comp_df(),
                                "series_temporelles_comparaison")

    output$tsBest <- shiny::renderUI({
      d <- comp_df(); d <- d[d$Statut == "OK" & is.finite(d$RMSE), , drop = FALSE]
      if (nrow(d) == 0) return(NULL)
      shiny::div(class = "callout callout-info", style = "margin-top:10px;",
          shiny::icon("trophy"),
          shiny::strong(trf(" Meilleur modèle sur le jeu de test : %s ", d$Modele[1])),
          trf("(RMSE = %s, MAPE = %s %%). Le classement repose sur l'erreur de ",
                  format(d$RMSE[1], big.mark = " "), d$`MAPE (%)`[1]),
          "prévision hors échantillon, le critère le plus honnête pour comparer des modèles.")
    })

    shiny::observeEvent(fits(), {
      f <- fits()
      ok <- names(f$res)[vapply(f$res, function(r) isTRUE(r$ok), logical(1))]
      labs <- names(f$labels)[match(ok, f$labels)]
      shiny::updateSelectInput(session, "tsShow",
                        choices = stats::setNames(ok, labs),
                        selected = if (length(ok)) ok[1] else NULL)
    })

    output$tsDoc <- shiny::renderUI({
      shiny::req(nzchar(input$tsShow %||% ""))
      hstat_model_doc_ui(input$tsShow)
    })

    # Valeurs saisies a la main dans le simulateur ("152,3 148,9" ou "152.3 148.9")
    parse_manual <- function(txt) {
      if (is.null(txt) || !nzchar(trimws(txt))) return(numeric(0))
      t <- gsub("[;\n\t]", " ", txt)
      # virgule decimale francaise : 152,3 -> 152.3 (si pas deja un separateur)
      t <- gsub("(?<=[0-9]),(?=[0-9])", ".", t, perl = TRUE)
      t <- gsub(",", " ", t)
      v <- suppressWarnings(as.numeric(strsplit(trimws(t), "[[:space:]]+")[[1]]))
      v[is.finite(v)]
    }

    cur <- shiny::reactive({
      f <- fits(); shiny::req(f, nzchar(input$tsShow %||% ""))
      r <- f$res[[input$tsShow]]
      shiny::validate(shiny::need(isTRUE(r$ok), "Ce modèle a échoué sur ces données."))
      list(r = r, f = f,
           label = names(f$labels)[match(input$tsShow, f$labels)])
    })

    # Graphique prévision -------------------------------------------------------
    fc_plot <- function(fc, y, col, lwd, title_def) {
      hist_df <- data.frame(t = as.numeric(stats::time(y)), y = as.numeric(y))
      tm <- as.numeric(stats::time(fc$mean))
      fdf <- data.frame(t = tm, y = as.numeric(fc$mean))
      has_pi <- !is.null(fc$lower) && length(fc$lower) > 0
      if (has_pi) {
        # `as.matrix()` sur les intervalles de `forecast` NE RETIRE PAS la classe
        # `ts` des colonnes extraites : `class(lo[, 1])` vaut bien « ts ». ggplot
        # ne sait pas choisir d'echelle pour ce type et le signalait a chaque
        # trace de prevision (« Don't know how to automatically pick scale for
        # object of type <ts> »). La courbe sortait quand meme, mais un
        # avertissement permanent en console masque les vrais.
        lo <- as.matrix(fc$lower); up <- as.matrix(fc$upper)
        fdf$lo80 <- as.numeric(lo[, 1]); fdf$hi80 <- as.numeric(up[, 1])
        fdf$lo95 <- as.numeric(lo[, ncol(lo)])
        fdf$hi95 <- as.numeric(up[, ncol(up)])
      }
      g <- ggplot2::ggplot()
      if (has_pi) g <- g +
        ggplot2::geom_ribbon(data = fdf, ggplot2::aes(x = t, ymin = lo95, ymax = hi95),
                             fill = col, alpha = 0.15) +
        ggplot2::geom_ribbon(data = fdf, ggplot2::aes(x = t, ymin = lo80, ymax = hi80),
                             fill = col, alpha = 0.28)
      g +
        ggplot2::geom_line(data = hist_df, ggplot2::aes(x = t, y = y),
                           color = "#37474f", linewidth = lwd) +
        ggplot2::geom_line(data = fdf, ggplot2::aes(x = t, y = y),
                           color = col, linewidth = lwd * 1.15) +
        ggplot2::labs(title = title_def, x = "Temps", y = "Valeur")
    }

    ts_plot_gg <- shiny::reactive({
      c0 <- cur()
      g <- fc_plot(c0$r$fc, c0$f$y,
                   col = hstat_plot_opt(input, "tsO", "Col", "#2c7fb8"),
                   lwd = hstat_finite(input$tsOLwd, 0.9),
                   title_def = trf("%s — prévision sur le jeu de test", c0$label))
      hstat_apply_plot_opts(g, input, "tsO")
    })
    output$tsPlot <- shiny::renderPlot(ts_plot_gg())
    output$tsPlDl <- hstat_export_plot_handler(input, "tsPl",
                       function() ts_plot_gg(), "prevision_serie")

    # Métriques + interprétation ------------------------------------------------
    output$tsMetrics <- DT::renderDataTable({
      DT::datatable(cur()$r$metrics, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE))
    })
    hstat_export_table_handlers(output, "tsMet",
      function() cur()$r$metrics, "series_temporelles_metriques")

    output$tsInterp <- shiny::renderUI({
      c0 <- cur()
      txt <- hstat_model_interpretation(
        "regression", c0$r$metrics, c0$label,
        n_train = length(c0$f$train), n_test = c0$f$n_test,
        notes = trf("La MASE vaut %s : %s",
          round(c0$r$mase, 3),
          if (!is.finite(c0$r$mase)) "non calculable."
          else if (c0$r$mase < 1) tr("le modèle bat la prévision naïve — il apporte une vraie valeur ajoutée.")
          else tr("le modèle ne fait pas mieux qu'une prévision naïve — à reconsidérer.")))
      shiny::div(class = "callout callout-info", style = "margin-top:10px;",
          shiny::icon("lightbulb"), shiny::strong(" Interprétation : "), txt)
    })

    # Diagnostics des résidus ----------------------------------------------------
    resid_gg <- shiny::reactive({
      c0 <- cur()
      res <- as.numeric(stats::residuals(c0$r$fc) %||% c0$r$fc$residuals)
      shiny::validate(shiny::need(length(res[is.finite(res)]) > 5,
                    "Résidus indisponibles pour ce modèle."))
      d <- data.frame(t = seq_along(res), r = res)
      acf_v <- stats::acf(res[is.finite(res)], plot = FALSE,
                          lag.max = min(30, length(res) - 1))
      a <- data.frame(lag = as.numeric(acf_v$lag)[-1], acf = as.numeric(acf_v$acf)[-1])
      ci <- 1.96 / sqrt(sum(is.finite(res)))
      col <- hstat_plot_opt(input, "tsO", "Col", "#2c7fb8")
      g1 <- ggplot2::ggplot(d, ggplot2::aes(t, r)) +
        ggplot2::geom_line(color = "#37474f", linewidth = 0.5) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::labs(title = "Résidus dans le temps", x = "Temps", y = "Résidu") +
        ggplot2::theme_minimal(base_size = 12)
      g2 <- ggplot2::ggplot(a, ggplot2::aes(lag, acf)) +
        ggplot2::geom_col(fill = col, width = 0.25) +
        ggplot2::geom_hline(yintercept = c(-ci, ci), linetype = "dashed",
                            color = "#e74c3c") +
        ggplot2::labs(title = "ACF des résidus", x = "Décalage", y = "Autocorrélation") +
        ggplot2::theme_minimal(base_size = 12)
      patchwork::wrap_plots(g1, g2, ncol = 2)
    })
    output$tsResid <- shiny::renderPlot(resid_gg())
    output$tsReDl <- hstat_export_plot_handler(input, "tsRe",
                       function() resid_gg(), "diagnostic_residus")

    output$tsLjung <- shiny::renderUI({
      c0 <- cur()
      res <- as.numeric(stats::residuals(c0$r$fc) %||% c0$r$fc$residuals)
      res <- res[is.finite(res)]
      if (length(res) < 8) return(NULL)
      lb <- stats::Box.test(res, lag = min(24, max(4, length(res) %/% 5)),
                            type = "Ljung-Box")
      # Des residus de variance nulle (modele qui reproduit exactement le train)
      # donnent p = NaN : `if (p > 0.05)` levait alors « missing value where
      # TRUE/FALSE needed » et faisait disparaitre tout le bloc de diagnostic.
      verdict <- hstat_p_verdict(lb$p.value)
      if (identical(verdict, "indeterminable"))
        return(shiny::div(class = "callout callout-warning", style = "margin-top:8px;",
          shiny::icon("circle-question"), shiny::strong(" Test de Ljung-Box : "),
          "non calculable — les résidus n'ont pas de variance exploitable ",
          "(le modèle reproduit la série d'apprentissage à l'identique). ",
          "L'absence d'autocorrélation résiduelle ne peut être ni confirmée ni infirmée."))
      ok <- identical(verdict, "non significatif")
      shiny::div(class = paste("callout", if (ok) "callout-info" else "callout-warning"),
          style = "margin-top:8px;",
          shiny::icon(if (ok) "check-circle" else "exclamation-triangle"),
          shiny::strong(" Test de Ljung-Box : "),
          sprintf("p = %.4f. ", lb$p.value),
          if (ok) "Les résidus se comportent comme un bruit blanc : le modèle a capté la structure temporelle de la série."
          else "Il reste de l'autocorrélation dans les résidus : de l'information temporelle n'est pas captée (essayer ARIMA/SARIMA, augmenter les ordres, ou vérifier la fréquence saisonnière).")
    })

    # Décomposition STL -----------------------------------------------------------
    decomp_gg <- shiny::reactive({
      f <- fits(); shiny::req(f)
      y <- f$y
      shiny::validate(shiny::need(stats::frequency(y) > 1 && length(y) >= 2 * stats::frequency(y),
        "Décomposition STL indisponible : série non saisonnière (fréquence = 1) ou trop courte."))
      st <- stats::stl(y, s.window = "periodic")
      d <- as.data.frame(st$time.series)
      d$t <- as.numeric(stats::time(y)); d$obs <- as.numeric(y)
      long <- rbind(
        data.frame(t = d$t, v = d$obs,       comp = "Série observée"),
        data.frame(t = d$t, v = d$trend,     comp = "Tendance"),
        data.frame(t = d$t, v = d$seasonal,  comp = "Saisonnalité"),
        data.frame(t = d$t, v = d$remainder, comp = "Résidu"))
      long$comp <- factor(long$comp, levels = unique(long$comp))
      ggplot2::ggplot(long, ggplot2::aes(t, v)) +
        ggplot2::geom_line(color = hstat_plot_opt(input, "tsO", "Col", "#2c7fb8"),
                           linewidth = 0.6) +
        ggplot2::facet_wrap(~comp, ncol = 1, scales = "free_y") +
        ggplot2::labs(title = "Décomposition STL", x = "Temps", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    })
    output$tsDecomp <- shiny::renderPlot(decomp_gg())
    output$tsDeDl <- hstat_export_plot_handler(input, "tsDe",
                       function() decomp_gg(), "decomposition_stl")

    # Simulateur ------------------------------------------------------------------
    sim <- shiny::eventReactive(input$simRun, {
      c0 <- cur()
      y <- c0$f$y
      h <- max(1, hstat_finite(input$simH, 12))
      # --- Cas DLNM : le fichier importé fournit les EXPOSITIONS futures -----
      if (identical(input$tsShow, "dlnm")) {
        expo_full <- c0$f$expo
        shiny::validate(shiny::need(!is.null(expo_full),
                      "DLNM : relancez l'entraînement avec une exposition."))
        L <- as.integer(max(1, hstat_finite(input$dlnmLag, 14)))
        note <- NULL
        manual <- parse_manual(input$simManual)
        if (length(manual) > 0) {
          if (length(manual) < h) h <- length(manual)
          ef <- manual[seq_len(h)]
          note <- trf("Expositions futures saisies manuellement (%d valeur(s)).", h)
        } else if (!is.null(input$simFile$datapath)) {
          nd <- tryCatch({
            if (grepl("\\.xlsx$", input$simFile$name, ignore.case = TRUE))
              as.data.frame(readxl::read_excel(input$simFile$datapath))
            else utils::read.csv(input$simFile$datapath, check.names = FALSE)
          }, error = function(e) NULL)
          shiny::validate(shiny::need(!is.null(nd), "Fichier importé illisible."),
                   shiny::need(input$dlnmExpo %in% names(nd),
                        trf("Pour le DLNM, le fichier doit contenir la colonne d'exposition '%s' (valeurs futures).",
                                input$dlnmExpo)))
          ef <- suppressWarnings(as.numeric(nd[[input$dlnmExpo]]))
          ef <- ef[is.finite(ef)]
          shiny::validate(shiny::need(length(ef) > 0, "Aucune exposition future valide dans le fichier."))
          if (length(ef) < h) h <- length(ef)
          ef <- ef[seq_len(h)]
        } else {
          ef <- rep(mean(utils::tail(expo_full, L)), h)
          note <- "Aucune exposition future fournie : l'exposition a été maintenue à sa moyenne récente (importez un fichier avec la colonne d'exposition pour un scénario réel)."
        }
        r <- tryCatch(fit_one("dlnm", y, h = h, expo = expo_full,
                              expo_future = ef),
                      error = function(e) e)
        shiny::validate(shiny::need(!inherits(r, "error"),
                      if (inherits(r, "error")) conditionMessage(r) else ""))
        return(list(fc = r$fc, y = y, h = h, appended = 0L,
                    label = c0$label, note = note))
      }
      # --- Cas général : saisie manuelle ou fichier AJOUTENT des observations --
      appended <- 0L
      manual <- parse_manual(input$simManual)
      if (length(manual) > 0) {
        y <- stats::ts(c(as.numeric(y), manual), frequency = stats::frequency(y))
        appended <- length(manual)
      } else if (!is.null(input$simFile$datapath)) {
        nd <- tryCatch({
          if (grepl("\\.xlsx$", input$simFile$name, ignore.case = TRUE))
            as.data.frame(readxl::read_excel(input$simFile$datapath))
          else utils::read.csv(input$simFile$datapath, check.names = FALSE)
        }, error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(nd), "Fichier importé illisible."),
                 shiny::need(input$tsVar %in% names(nd),
                      trf("Le fichier doit contenir une colonne '%s'.", input$tsVar)))
        add <- suppressWarnings(as.numeric(nd[[input$tsVar]]))
        add <- add[is.finite(add)]
        shiny::validate(shiny::need(length(add) > 0, "Aucune valeur numérique valide dans le fichier."))
        y <- stats::ts(c(as.numeric(y), add), frequency = stats::frequency(y))
        appended <- length(add)
      }
      r <- tryCatch(fit_one(input$tsShow, y, h = h,
                            dates_train = c0$f$dates),
                    error = function(e) e)
      shiny::validate(shiny::need(!inherits(r, "error"),
                    if (inherits(r, "error")) conditionMessage(r) else ""))
      list(fc = r$fc, y = y, h = h, appended = appended, label = c0$label,
           note = NULL)
    })

    sim_table <- shiny::reactive({
      s <- sim(); shiny::req(s)
      fc <- s$fc
      d <- data.frame(Periode = seq_len(s$h),
                      Prevision = round(as.numeric(fc$mean), 4))
      if (!is.null(fc$lower) && length(fc$lower) > 0) {
        lo <- as.matrix(fc$lower); up <- as.matrix(fc$upper)
        d$`Borne basse 80%` <- round(lo[, 1], 4)
        d$`Borne haute 80%` <- round(up[, 1], 4)
        d$`Borne basse 95%` <- round(lo[, ncol(lo)], 4)
        d$`Borne haute 95%` <- round(up[, ncol(up)], 4)
      }
      d
    })
    output$simTable <- DT::renderDataTable({
      DT::datatable(sim_table(), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    hstat_export_table_handlers(output, "simTab",
      function() sim_table(), "previsions_simulees")

    sim_gg <- shiny::reactive({
      s <- sim(); shiny::req(s)
      g <- fc_plot(s$fc, s$y,
                   col = hstat_plot_opt(input, "tsO", "Col", "#27ae60"),
                   lwd = hstat_finite(input$tsOLwd, 0.9),
                   title_def = trf("%s — prévisions futures (h = %d)", s$label, s$h))
      hstat_apply_plot_opts(g, input, "tsO")
    })
    output$simPlot <- shiny::renderPlot(sim_gg())
    output$simPlDl <- hstat_export_plot_handler(input, "simPl",
                        function() sim_gg(), "previsions_futures")

    output$simInterp <- shiny::renderUI({
      s <- sim(); d <- sim_table()
      first <- d$Prevision[1]; last <- d$Prevision[nrow(d)]
      trend <- if (!is.finite(first) || !is.finite(last)) "indéterminée"
        else if (last > first * 1.02) "orientée à la hausse"
        else if (last < first * 0.98) "orientée à la baisse" else "globalement stable"
      shiny::div(class = "callout callout-info", style = "margin-top:10px;",
          shiny::icon("lightbulb"), shiny::strong(" Interprétation des prévisions : "),
          trf("Le modèle %s, ré-entraîné sur l'intégralité de la série%s, projette une trajectoire %s sur les %d prochaines périodes (de %s à %s). ",
                  s$label,
                  if (s$appended > 0) trf(" (dont %d nouvelles observations importées)", s$appended) else "",
                  trend, s$h,
                  format(first, big.mark = " "), format(last, big.mark = " ")),
          if (!is.null(d$`Borne haute 95%`))
            "Les bornes à 95 % encadrent la valeur future avec 95 % de confiance : plus elles s'écartent, plus l'incertitude croît avec l'horizon — les premières périodes sont toujours les plus fiables."
          else "Ce modèle ne fournit pas d'intervalles de prévision (point uniquement).",
          if (!is.null(s$note)) shiny::tags$p(shiny::tags$em(s$note)))
    })
  })
}
