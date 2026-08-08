# ===========================================================================
# HStat - mod_ai.R
# Assistance par IA : moteur d'inference partage + aide a la decision
# ---------------------------------------------------------------------------
# Deux roles, et deux seulement :
#
#   1. INTERPRETER des resultats deja obtenus, pour que l'utilisateur reparte
#      avec un texte pret a l'emploi plutot qu'un tableau brut.
#
#   2. RECOMMANDER l'analyse qui aurait convenu aux donnees, a la fin d'une
#      analyse, pour eclairer le choix suivant.
#
# Ce que l'assistance ne fait PAS, volontairement : choisir l'analyse a la
# place de l'utilisateur, ni la lancer. Le choix de la methode engage
# l'interpretation scientifique du travail : il reste a l'analyste, qui en
# demeure responsable. L'assistance eclaire, elle ne decide pas.
#
# La recommandation (hstat_reco_*) est entierement DETERMINISTE et hors ligne :
# elle applique des regles statistiques classiques au profil des variables. Un
# modele de langue n'y intervient jamais — on ne veut pas qu'un test statistique
# soit conseille par generation de texte. Le modele, lui, redige l'interpretation.
# ===========================================================================

# ---------------------------------------------------------------------------
# 10. ASSISTANT DE CODAGE - TROIS MOTEURS
# ---------------------------------------------------------------------------
# L'assistance au codage doit pouvoir tourner GRATUITEMENT, EN LOCAL et SANS
# CONNEXION INTERNET. Trois moteurs sont donc proposes, du plus autonome au
# moins autonome :
#
#   "auto"   Thematisation statistique, sans aucun modele de langue. Tourne
#            dans le processus R, instantanement, sans rien installer et sans
#            reseau. C'est le seul moteur garanti disponible partout.
#
#   "local"  Modele de langue execute SUR LA MACHINE de l'utilisateur, servi
#            par Ollama ou par n'importe quel serveur d'inference compatible
#            OpenAI (llama.cpp, LM Studio, vLLM, Jan...). Gratuit, hors ligne
#            une fois le modele telecharge, et de loin le plus performant des
#            trois sur la comprehension du sens. C'est le moteur par defaut.
#
#   "claude" API Claude en ligne. Payante et necessitant une connexion : elle
#            n'est proposee qu'en dernier recours, jamais par defaut.
#
# Les appels HTTP restent locaux dans les deux premiers cas (127.0.0.1) : rien
# ne sort de la machine, ce qui compte aussi pour la confidentialite de donnees
# d'enquete. R n'ayant de SDK ni pour Ollama ni pour Anthropic, tout passe par
# {httr} + {jsonlite}, gardes en Suggests : sans eux, le moteur "auto" reste
# pleinement fonctionnel.
# ---------------------------------------------------------------------------

HSTAT_AI_ENGINES <- c(
  "Modele local (Ollama / llama.cpp) - gratuit, hors ligne" = "local",
  "Thematisation automatique (statistique, sans modele)"    = "auto",
  "API Claude (en ligne, payante)"                          = "claude")

HSTAT_AI_BACKENDS <- c(
  "Ollama"                                            = "ollama",
  "Serveur compatible OpenAI (llama.cpp, LM Studio...)" = "openai")

HSTAT_AI_DEFAULT_URL <- c(ollama = "http://127.0.0.1:11434",
                          openai = "http://127.0.0.1:8080")

HSTAT_AI_MODEL <- "claude-opus-5"   # utilise uniquement par le moteur "claude"

hstat_ai_key <- function(explicit = NULL) {
  k <- if (is.null(explicit)) "" else trimws(as.character(explicit)[1])
  if (is.na(k) || !nzchar(k)) k <- trimws(Sys.getenv("ANTHROPIC_API_KEY", ""))
  k
}

hstat_ai_url <- function(backend = "ollama", url = NULL) {
  u <- if (is.null(url)) "" else trimws(as.character(url)[1])
  if (is.na(u) || !nzchar(u))
    u <- unname(HSTAT_AI_DEFAULT_URL[[match.arg(backend, c("ollama", "openai"))]])
  sub("/+$", "", u)
}

.hstat_ai_http_ok <- function() {
  requireNamespace("httr", quietly = TRUE) &&
    requireNamespace("jsonlite", quietly = TRUE)
}

# Modeles reellement installes dans Ollama. Vecteur vide si le serveur n'est
# pas joignable : l'interface saura alors dire quoi faire plutot que d'offrir
# une liste vide sans explication.
hstat_ai_ollama_models <- function(url = NULL, timeout = 5) {
  if (!.hstat_ai_http_ok()) return(character(0))
  res <- tryCatch(
    httr::GET(paste0(hstat_ai_url("ollama", url), "/api/tags"), httr::timeout(timeout)),
    error = function(e) NULL)
  if (is.null(res) || httr::status_code(res) >= 300) return(character(0))
  p <- tryCatch(jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                                   simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(p$models)) return(character(0))
  nm <- vapply(p$models, function(m) if (is.null(m$name)) "" else as.character(m$name)[1],
               character(1))
  sort(nm[nzchar(nm)])
}

# Modeles annonces par un serveur compatible OpenAI (GET /v1/models).
hstat_ai_openai_models <- function(url = NULL, timeout = 5) {
  if (!.hstat_ai_http_ok()) return(character(0))
  res <- tryCatch(
    httr::GET(paste0(hstat_ai_url("openai", url), "/v1/models"), httr::timeout(timeout)),
    error = function(e) NULL)
  if (is.null(res) || httr::status_code(res) >= 300) return(character(0))
  p <- tryCatch(jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                                   simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(p$data)) return(character(0))
  nm <- vapply(p$data, function(m) if (is.null(m$id)) "" else as.character(m$id)[1],
               character(1))
  sort(nm[nzchar(nm)])
}

hstat_ai_models <- function(backend = "ollama", url = NULL, timeout = 5) {
  if (identical(backend, "openai")) hstat_ai_openai_models(url, timeout)
  else hstat_ai_ollama_models(url, timeout)
}

# Diagnostic lisible, propre a chaque moteur. Toujours actionnable : on dit ce
# qui manque ET comment y remedier, plutot qu'un simple « indisponible ».
hstat_ai_status <- function(engine = "local", backend = "ollama", url = NULL,
                            model = NULL, api_key = NULL) {
  if (identical(engine, "auto"))
    return(list(ok = TRUE,
                message = paste0("Thematisation automatique disponible : elle tourne ",
                                 "dans R, sans modele, sans installation et sans reseau.")))

  if (!.hstat_ai_http_ok()) {
    miss <- c(if (!requireNamespace("httr", quietly = TRUE)) "{httr}",
              if (!requireNamespace("jsonlite", quietly = TRUE)) "{jsonlite}")
    return(list(ok = FALSE,
                message = sprintf(
                  "Moteur indisponible : le(s) paquet(s) %s manquent. Installez-les avec install.packages(c(%s)), ou basculez sur la thematisation automatique, qui n'en a pas besoin.",
                  paste(miss, collapse = " et "),
                  paste(sprintf('"%s"', gsub("[{}]", "", miss)), collapse = ", "))))
  }

  if (identical(engine, "claude")) {
    if (!nzchar(hstat_ai_key(api_key)))
      return(list(ok = FALSE,
                  message = paste0("API Claude indisponible : renseignez une cle d'API ",
                                   "(champ ci-dessous ou variable ANTHROPIC_API_KEY). ",
                                   "Cette option est payante et necessite une connexion ",
                                   "Internet ; les deux autres moteurs sont gratuits et locaux.")))
    return(list(ok = TRUE, message = sprintf("API Claude disponible (modele %s).",
                                             HSTAT_AI_MODEL)))
  }

  # engine == "local"
  u <- hstat_ai_url(backend, url)
  mods <- hstat_ai_models(backend, u)
  if (!length(mods)) {
    aide <- if (identical(backend, "ollama"))
      paste0("Installez Ollama (ollama.com, gratuit), lancez-le, puis telechargez ",
             "un modele une seule fois : `ollama pull qwen2.5` par exemple. ",
             "Le telechargement fait, tout fonctionne hors ligne.")
    else
      paste0("Demarrez votre serveur d'inference (llama.cpp `llama-server`, ",
             "LM Studio, vLLM, Jan...) et verifiez son adresse ci-dessous.")
    return(list(ok = FALSE, models = character(0),
                message = sprintf("Aucun modele local joignable sur %s. %s", u, aide)))
  }
  if (!is.null(model) && nzchar(model) && !(model %in% mods))
    return(list(ok = FALSE, models = mods,
                message = sprintf("Le modele « %s » n'est pas installe sur %s. Modeles disponibles : %s.",
                                  model, u, paste(mods, collapse = ", "))))
  list(ok = TRUE, models = mods,
       message = sprintf("Modele local disponible sur %s (%d modele(s) installe(s)). Gratuit, hors ligne, aucune donnee ne quitte la machine.",
                         u, length(mods)))
}

# Retro-compatibilite : l'ancienne signature ne connaissait que Claude.
hstat_ai_available <- function(explicit = NULL) {
  isTRUE(hstat_ai_status("claude", api_key = explicit)$ok)
}

# --- Appels HTTP, un par moteur ---------------------------------------------
# Tous renvoient list(ok, text, error) : aucune panne reseau, aucun serveur
# absent ne doit faire tomber l'application.

.hstat_ai_post <- function(url, body, headers = NULL, timeout = 600) {
  args <- list(url,
               body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
               encode = "raw", httr::timeout(timeout))
  hdr <- c(`content-type` = "application/json", headers)
  args <- append(args, list(do.call(httr::add_headers, as.list(hdr))), after = 1)
  tryCatch(do.call(httr::POST, args), error = function(e) e)
}

# Corps de requete, isole du reseau : c'est la partie qui casse en silence si
# un champ change de nom, et c'est donc celle qu'il faut pouvoir tester.
.hstat_ai_messages <- function(prompt, system = NULL) {
  msgs <- list()
  if (!is.null(system) && nzchar(system))
    msgs <- c(msgs, list(list(role = "system", content = system)))
  c(msgs, list(list(role = "user", content = prompt)))
}

.hstat_ai_body_ollama <- function(prompt, system = NULL, model = "", json = TRUE) {
  body <- list(model = model,
               messages = .hstat_ai_messages(prompt, system),
               stream = FALSE,
               # Temperature basse : on veut une thematisation stable et
               # reproductible, pas de la creativite.
               options = list(temperature = 0.2))
  # `format: "json"` contraint Ollama a produire du JSON valide, ce qui evite
  # l'essentiel des reponses inexploitables des petits modeles.
  if (isTRUE(json)) body$format <- "json"
  body
}

.hstat_ai_body_openai <- function(prompt, system = NULL, model = "",
                                  max_tokens = 4096L, json = TRUE) {
  body <- list(model = model,
               messages = .hstat_ai_messages(prompt, system),
               stream = FALSE, temperature = 0.2,
               max_tokens = as.integer(max_tokens))
  if (isTRUE(json)) body$response_format <- list(type = "json_object")
  body
}

.hstat_ai_call_ollama <- function(prompt, system = NULL, url = NULL,
                                  model = NULL, json = TRUE, timeout = 600) {
  u <- hstat_ai_url("ollama", url)
  if (is.null(model) || !nzchar(model)) {
    mods <- hstat_ai_ollama_models(u)
    if (!length(mods))
      return(list(ok = FALSE, text = "",
                  error = sprintf("Aucun modele Ollama joignable sur %s.", u)))
    model <- mods[1]
  }
  body <- .hstat_ai_body_ollama(prompt, system, model, json)

  res <- .hstat_ai_post(paste0(u, "/api/chat"), body, timeout = timeout)
  if (inherits(res, "error"))
    return(list(ok = FALSE, text = "",
                error = sprintf("Serveur local injoignable sur %s (%s). Ollama est-il demarre ?",
                                u, conditionMessage(res))))
  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  p <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (httr::status_code(res) >= 300)
    return(list(ok = FALSE, text = "",
                error = sprintf("Erreur du serveur local (HTTP %d) : %s",
                                httr::status_code(res),
                                substr(if (!is.null(p$error)) p$error else raw, 1, 400))))
  txt <- if (!is.null(p$message$content)) p$message$content else p$response
  if (is.null(txt))
    return(list(ok = FALSE, text = "", error = "Reponse illisible du serveur local."))
  list(ok = TRUE, text = as.character(txt)[1], error = NULL, model = model)
}

.hstat_ai_call_openai <- function(prompt, system = NULL, url = NULL, model = NULL,
                                  api_key = NULL, max_tokens = 4096L,
                                  json = TRUE, timeout = 600) {
  u <- hstat_ai_url("openai", url)
  if (is.null(model) || !nzchar(model)) {
    mods <- hstat_ai_openai_models(u)
    model <- if (length(mods)) mods[1] else "local-model"
  }
  mk <- function(with_json)
    .hstat_ai_body_openai(prompt, system, model, max_tokens, with_json)
  hdr <- if (!is.null(api_key) && nzchar(api_key))
    c(Authorization = paste("Bearer", api_key)) else NULL

  send <- function(with_json)
    .hstat_ai_post(paste0(u, "/v1/chat/completions"), mk(with_json), hdr, timeout)

  res <- send(isTRUE(json))
  # Tous les serveurs locaux n'acceptent pas response_format : une requete
  # rejetee pour ce seul motif est rejouee sans lui plutot que d'echouer.
  if (!inherits(res, "error") && isTRUE(json) && httr::status_code(res) == 400)
    res <- send(FALSE)

  if (inherits(res, "error"))
    return(list(ok = FALSE, text = "",
                error = sprintf("Serveur local injoignable sur %s (%s).",
                                u, conditionMessage(res))))
  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  p <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (httr::status_code(res) >= 300) {
    msg <- if (!is.null(p$error$message)) p$error$message else raw
    return(list(ok = FALSE, text = "",
                error = sprintf("Erreur du serveur local (HTTP %d) : %s",
                                httr::status_code(res), substr(msg, 1, 400))))
  }
  txt <- tryCatch(p$choices[[1]]$message$content, error = function(e) NULL)
  if (is.null(txt))
    return(list(ok = FALSE, text = "", error = "Reponse illisible du serveur local."))
  list(ok = TRUE, text = as.character(txt)[1], error = NULL, model = model)
}

.hstat_ai_call_claude <- function(prompt, system = NULL, api_key = NULL,
                                  model = HSTAT_AI_MODEL, max_tokens = 8000L,
                                  thinking = TRUE, timeout = 300) {
  key <- hstat_ai_key(api_key)
  if (!nzchar(key)) return(list(ok = FALSE, text = "", error = "Cle d'API absente."))

  body <- list(model = model, max_tokens = as.integer(max_tokens),
               messages = list(list(role = "user", content = prompt)))
  if (!is.null(system) && nzchar(system)) body$system <- system
  # La reflexion adaptative est le mode par defaut des modeles Claude recents ;
  # `budget_tokens` y est rejete, on ne l'envoie donc pas.
  if (isTRUE(thinking)) body$thinking <- list(type = "adaptive")

  res <- .hstat_ai_post("https://api.anthropic.com/v1/messages", body,
                        c(`x-api-key` = key, `anthropic-version` = "2023-06-01"),
                        timeout)
  if (inherits(res, "error"))
    return(list(ok = FALSE, text = "",
                error = paste("Echec de la connexion :", conditionMessage(res))))

  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (httr::status_code(res) >= 300) {
    msg <- if (!is.null(parsed$error$message)) parsed$error$message else raw
    return(list(ok = FALSE, text = "",
                error = sprintf("Erreur API (HTTP %d) : %s",
                                httr::status_code(res), substr(msg, 1, 500))))
  }
  if (is.null(parsed) || is.null(parsed$content))
    return(list(ok = FALSE, text = "", error = "Reponse illisible de l'API."))

  # Le contenu est une liste de blocs ; avec la reflexion adaptative, les blocs
  # `thinking` precedent le bloc `text` : on ne garde que le texte.
  txt <- vapply(parsed$content, function(b)
    if (identical(b$type, "text") && !is.null(b$text)) b$text else "", character(1))
  list(ok = TRUE, text = paste(txt[nzchar(txt)], collapse = "\n"), error = NULL,
       model = model)
}

# Aiguillage. `engine = "local"` par defaut : gratuit et hors ligne.
hstat_ai_call <- function(prompt, system = NULL, engine = "local",
                          backend = "ollama", url = NULL, model = NULL,
                          api_key = NULL, max_tokens = 8000L, json = TRUE,
                          timeout = NULL) {
  if (!.hstat_ai_http_ok())
    return(list(ok = FALSE, text = "",
                error = "Les paquets {httr} et {jsonlite} sont requis pour ce moteur."))
  if (identical(engine, "claude"))
    return(.hstat_ai_call_claude(prompt, system, api_key, HSTAT_AI_MODEL,
                                 max_tokens, TRUE, timeout %||% 300))
  # Les modeles locaux tournent sur le processeur de l'utilisateur : le delai
  # d'attente par defaut est bien plus large que pour une API distante.
  tmo <- timeout %||% 900
  if (identical(backend, "openai"))
    .hstat_ai_call_openai(prompt, system, url, model, api_key, max_tokens, json, tmo)
  else
    .hstat_ai_call_ollama(prompt, system, url, model, json, tmo)
}


# Extraction tolerante du JSON : le modele peut encadrer sa reponse de texte
# ou de balises ```json. On isole le premier objet/tableau complet.
hstat_ai_extract_json <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  s <- gsub("```[a-zA-Z]*", "", txt, perl = TRUE)
  s <- gsub("```", "", s, fixed = TRUE)
  i <- regexpr("[\\{\\[]", s, perl = TRUE)
  if (i < 0) return(NULL)
  j <- max(gregexpr("[\\}\\]]", s, perl = TRUE)[[1]])
  if (j < i) return(NULL)
  tryCatch(jsonlite::fromJSON(substr(s, i, j), simplifyVector = FALSE),
           error = function(e) NULL)
}



# ---------------------------------------------------------------------------
# REGISTRE DES ANALYSES
# ---------------------------------------------------------------------------
# Chaque module depose ici ce qu'il vient de produire. L'onglet d'aide a la
# decision n'a alors plus qu'a lire `values$aiContext` : aucun module n'a
# besoin de connaitre l'assistance, et l'assistance n'a besoin de connaitre
# aucun module.
# ---------------------------------------------------------------------------

# Bornes de l'historique : au-dela de HSTAT_HIST_DETAIL analyses, les plus
# anciennes perdent leurs tableaux et leurs figures (le journal, lui, n'en a
# pas besoin) ; au-dela de HSTAT_HIST_MAX, elles sortent de l'historique.
HSTAT_HIST_DETAIL <- 30L
HSTAT_HIST_MAX    <- 200L

# `tables` : liste nommee de data.frame ; `text` : sortie R brute eventuelle ;
# `meta` : variables utilisees, groupe, options — ce qui permet de recommander.
# `plot` : fonction sans argument qui trace le graphique de l'analyse, quand le
# module en possede une. Le rapport s'en sert pour reproduire la figure ; les
# modules qui n'en ont pas passent simplement NULL.
hstat_ai_capture <- function(values, module, title, tables = list(),
                             text = NULL, meta = list(), plot = NULL) {
  if (is.null(values)) return(invisible(NULL))
  # Normalisation A L'ENTREE du registre. Les modules deposent ce qu'ils ont
  # sous la main : un data.frame, une matrice, mais aussi un objet `htest` brut
  # renvoye par t.test() ou shapiro.test(). `as.data.frame()` echoue dessus
  # (« cannot coerce class "htest" to a data.frame ») et faisait tomber toute
  # l'analyse. Coercer ici, une fois, plutot que dans chacun des lecteurs
  # (historique, rapport, onglet « Resultats captures »).
  tables <- lapply(tables, function(x)
    tryCatch(hstat_ai_as_table(x), error = function(e) NULL))
  tables <- Filter(function(x) !is.null(x) && NROW(x) > 0, tables)
  ctx <- list(
    module = as.character(module)[1],
    title  = as.character(title)[1],
    tables = tables,
    text   = if (is.null(text)) NULL else paste(as.character(text), collapse = "\n"),
    meta   = meta,
    plot   = if (is.function(plot)) plot else NULL,
    time   = Sys.time())
  values$aiContext <- ctx

  # Historique : le journal de reproductibilite a besoin de TOUTES les analyses
  # menees, pas seulement de la derniere. On ecarte la repetition immediate a
  # l'identique (un curseur deplace re-declenche la meme analyse) pour que le
  # script reste lisible.
  h <- values$aiHistory %||% list()
  precedent <- if (length(h)) h[[length(h)]] else NULL
  meme <- !is.null(precedent) &&
    identical(precedent$module, ctx$module) &&
    identical(precedent$title, ctx$title) &&
    identical(precedent$meta, ctx$meta)
  if (!meme) {
    # Le rapport a besoin des tableaux et des figures ; le journal, seulement
    # des metadonnees. On garde donc le detail des analyses RECENTES et on
    # l'allege sur les anciennes : la memoire reste bornee quelle que soit la
    # duree de la session.
    entree <- ctx[c("module", "title", "meta", "time", "plot")]
    entree$tables <- lapply(tables, function(x) utils::head(x, 100L))
    h[[length(h) + 1L]] <- entree
    if (length(h) > HSTAT_HIST_DETAIL) {
      a_alleger <- seq_len(length(h) - HSTAT_HIST_DETAIL)
      for (i in a_alleger) { h[[i]]$tables <- NULL; h[[i]]$plot <- NULL }
    }
    if (length(h) > HSTAT_HIST_MAX) h <- utils::tail(h, HSTAT_HIST_MAX)
    values$aiHistory <- h
  }
  invisible(ctx)
}

# Coercition en data.frame de ce qu'une analyse renvoie : les modules
# produisent tantot un tableau, tantot une liste de valeurs nommees (puissance,
# effectif requis...). Le registre n'a pas a s'en soucier.
hstat_ai_as_table <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.matrix(x)) return(as.data.frame(x))
  if (is.list(x)) {
    plats <- vapply(x, function(e) is.atomic(e) && length(e) == 1L, logical(1))
    x <- x[plats]
    if (!length(x)) return(NULL)
    return(data.frame(Grandeur = names(x),
                      Valeur = vapply(x, function(e) {
                        if (is.numeric(e)) format(signif(e, 5), trim = TRUE)
                        else as.character(e)
                      }, character(1)),
                      stringsAsFactors = FALSE, row.names = NULL))
  }
  if (is.atomic(x) && length(x))
    return(data.frame(Grandeur = names(x) %||% seq_along(x),
                      Valeur = as.character(x), stringsAsFactors = FALSE))
  NULL
}

# Mise en texte compacte d'un contexte, pour l'invite du modele. Les tableaux
# sont tronques : une invite de 200 lignes de chiffres degrade la reponse d'un
# modele local bien plus qu'elle ne l'informe.
hstat_ai_context_text <- function(ctx, max_rows = 25, max_chars = 6000) {
  if (is.null(ctx)) return("")
  out <- c(sprintf("ANALYSE REALISEE : %s", ctx$title))
  if (length(ctx$meta)) {
    m <- vapply(names(ctx$meta), function(k) {
      v <- ctx$meta[[k]]
      if (is.null(v) || !length(v)) return("")
      sprintf("- %s : %s", k, paste(utils::head(as.character(v), 12), collapse = ", "))
    }, character(1))
    m <- m[nzchar(m)]
    if (length(m)) out <- c(out, "", "PARAMETRES :", m)
  }
  for (nm in names(ctx$tables)) {
    tb <- ctx$tables[[nm]]
    if (is.null(tb) || !NROW(tb)) next
    trunc <- NROW(tb) > max_rows
    tb <- utils::head(as.data.frame(tb), max_rows)
    out <- c(out, "", sprintf("TABLEAU - %s :", nm),
             paste(utils::capture.output(print(tb, row.names = FALSE)), collapse = "\n"),
             if (trunc) sprintf("(... %d lignes au total)", NROW(ctx$tables[[nm]])) else NULL)
  }
  if (!is.null(ctx$text) && nzchar(ctx$text))
    out <- c(out, "", "SORTIE R :", substr(ctx$text, 1, 2500))
  substr(paste(out, collapse = "\n"), 1, max_chars)
}


# ---------------------------------------------------------------------------
# PROFIL DES VARIABLES
# ---------------------------------------------------------------------------
# Ce que la recommandation regarde : nature des variables, effectifs, normalite,
# homogeneite des variances, appariement. Tout est calcule ici, une fois, et
# reste consultable par l'utilisateur : une recommandation dont on ne voit pas
# les raisons ne vaut rien.
# ---------------------------------------------------------------------------

.hstat_reco_type <- function(x) {
  # Une variable sans AUCUNE valeur observee n'a pas de type. Sans ce garde-fou,
  # `unique(na.omit(x))` etait vide, donc de longueur 0 <= 2, et la variable
  # etait typee « binaire » : le moteur recommandait alors un chi-deux
  # d'independance sur une colonne entierement vide. Conseiller avec aplomb une
  # analyse impossible est pire que ne rien conseiller — et c'est exactement ce
  # qu'un utilisateur suivrait sans se mefier.
  if (is.null(x) || !length(x) || all(is.na(x))) return("indeterminable")
  if (is.numeric(x)) {
    u <- length(unique(stats::na.omit(x)))
    # Un entier a deux valeurs est un facteur deguise (0/1, 1/2...).
    if (u <= 2) return("binaire")
    if (u <= 7 && all(stats::na.omit(x) == round(stats::na.omit(x)))) return("ordinale")
    return("quantitative")
  }
  if (is.logical(x)) return("binaire")
  if (is.ordered(x)) return("ordinale")
  u <- length(unique(stats::na.omit(as.character(x))))
  if (u <= 2) return("binaire")
  "categorielle"
}

# Normalite : Shapiro-Wilk quand l'effectif le permet. Au-dela de 5000, le test
# rejette pour des ecarts sans consequence pratique ; on se rabat alors sur
# l'asymetrie et l'aplatissement, plus honnetes a grand n.
.hstat_reco_normal <- function(x) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  if (n < 3) return(list(ok = NA, methode = "effectif insuffisant", p = NA_real_))
  if (n <= 5000) {
    p <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
    return(list(ok = if (is.na(p)) NA else p > 0.05, methode = "Shapiro-Wilk", p = p))
  }
  m <- mean(x); s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(list(ok = FALSE, methode = "variance nulle", p = NA_real_))
  skew <- mean(((x - m) / s)^3)
  kurt <- mean(((x - m) / s)^4) - 3
  list(ok = abs(skew) < 1 && abs(kurt) < 2,
       methode = sprintf("asymetrie %.2f / aplatissement %.2f (n > 5000)", skew, kurt),
       p = NA_real_)
}

# Normalite EVALUEE DANS CHAQUE GROUPE, et non sur la variable groupee.
# La distinction n'est pas un detail : deux groupes parfaitement normaux mais
# bien separes forment ensemble une distribution bimodale que Shapiro rejette
# (p ~ 1e-6 la ou chaque groupe donne p ~ 0.8). Tester le melange conduirait a
# deconseiller l'ANOVA precisement quand elle convient. Ce que l'ANOVA et le
# test t supposent, c'est la normalite INTRA-groupe (celle des residus).
.hstat_reco_normal_by <- function(x, g) {
  ok <- stats::complete.cases(x, g)
  x <- as.numeric(x)[ok]; g <- as.character(g)[ok]
  lev <- unique(g)
  if (!length(lev)) return(list(ok = NA, methode = "aucune observation complete",
                                p = NA_real_, portee = "par groupe"))
  det <- do.call(rbind, lapply(lev, function(l) {
    r <- .hstat_reco_normal(x[g == l])
    data.frame(Groupe = l, n = sum(g == l), p = r$p, Normale = r$ok,
               stringsAsFactors = FALSE)
  }))
  testables <- !is.na(det$Normale)
  list(ok = if (!any(testables)) NA else all(det$Normale[testables]),
       methode = sprintf("Shapiro-Wilk dans chacun des %d groupes", length(lev)),
       p = if (any(testables)) min(det$p[testables], na.rm = TRUE) else NA_real_,
       portee = "par groupe",
       detail = det)
}

hstat_data_profile <- function(df, vars = NULL, group = NULL, paired = FALSE) {
  if (is.null(df) || !NROW(df)) return(NULL)
  df <- as.data.frame(df)
  vars <- intersect(vars %||% names(df), names(df))
  if (!length(vars)) return(NULL)

  has_g <- !is.null(group) && nzchar(group) && group %in% names(df) &&
           !identical(group, "") && !(group %in% vars && length(vars) == 1)
  v <- lapply(vars, function(nm) {
    x <- df[[nm]]
    ty <- .hstat_reco_type(x)
    n_ok <- sum(!is.na(x))
    norm <- if (ty != "quantitative") NULL
            else if (has_g && !identical(nm, group))
              .hstat_reco_normal_by(x, df[[group]])
            else c(.hstat_reco_normal(x), list(portee = "globale"))
    list(nom = nm, type = ty, n = n_ok,
         n_manquant = sum(is.na(x)),
         modalites = if (ty %in% c("categorielle", "ordinale", "binaire"))
                       length(unique(stats::na.omit(as.character(x)))) else NA_integer_,
         normale = norm)
  })
  names(v) <- vars

  g <- NULL
  if (!is.null(group) && nzchar(group) && group %in% names(df)) {
    gv <- as.character(df[[group]])
    tb <- table(gv[!is.na(gv) & nzchar(gv)])
    quanti <- vars[vapply(v, function(e) e$type == "quantitative", logical(1))]
    # Homogeneite des variances : Bartlett suppose la normalite, on ne l'utilise
    # donc que si toutes les variables quantitatives la respectent.
    homo <- NA
    if (length(quanti) && length(tb) >= 2) {
      normales <- all(vapply(quanti, function(nm) isTRUE(v[[nm]]$normale$ok), logical(1)))
      p <- tryCatch({
        f <- stats::as.formula(sprintf("`%s` ~ `%s`", quanti[1], group))
        if (normales) stats::bartlett.test(f, data = df)$p.value
        else if (requireNamespace("car", quietly = TRUE))
          stats::na.omit(car::leveneTest(f, data = df)$`Pr(>F)`)[1]
        else NA_real_
      }, error = function(e) NA_real_)
      homo <- if (is.na(p)) NA else p > 0.05
    }
    g <- list(nom = group, k = length(tb), effectifs = as.integer(tb),
              modalites = names(tb),
              equilibre = if (length(tb) >= 2) max(tb) / max(1, min(tb)) <= 1.5 else NA,
              petits_effectifs = any(tb < 5),
              variances_homogenes = homo)
  }

  list(n = nrow(df), variables = v, groupe = g, apparie = isTRUE(paired),
       n_quanti = sum(vapply(v, function(e) e$type == "quantitative", logical(1))),
       n_quali  = sum(vapply(v, function(e) e$type %in% c("categorielle", "binaire"), logical(1))),
       n_ordinal = sum(vapply(v, function(e) e$type == "ordinale", logical(1))))
}


# ---------------------------------------------------------------------------
# JOURNAL DE REPRODUCTIBILITE
# ---------------------------------------------------------------------------
# Restitue, sous forme de SCRIPT R executable, les analyses menees pendant la
# session. C'est ce qui separe un outil clic-bouton d'un outil dont les
# resultats sont publiables : un relecteur doit pouvoir refaire le chemin.
#
# Regle de conduite : quand le code exact ne peut pas etre reconstitue
# fidelement (reglages interactifs d'un module, modele de ML avec ses
# hyperparametres), on ecrit un COMMENTAIRE qui dit ce qui a ete fait, jamais
# du code plausible mais faux. Un script qui differe en silence de ce que
# l'application a calcule serait pire que pas de script du tout.
# ---------------------------------------------------------------------------

# Nom de variable protege : `ma variable` si l'identifiant n'est pas syntaxique.
.hstat_rlog_nom <- function(x) {
  x <- as.character(x)
  # Un nom non syntaxique se cite entre accents graves — mais le nom peut LUI
  # MEME en contenir, et le fermait alors prematurement : une colonne nommee
  # « a`b » produisait un script que R refusait d'analyser, alors que le journal
  # a precisement pour promesse d'etre executable. R accepte l'accent grave
  # echappe par une barre oblique inverse a l'interieur des accents graves ; la
  # barre elle-meme doit donc etre echappee d'abord.
  cite <- function(s) {
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    paste0("`", gsub("`", "\\`", s, fixed = TRUE), "`")
  }
  ifelse(grepl("^[A-Za-z.][A-Za-z0-9._]*$", x) & !grepl("^\\.[0-9]", x),
         x, vapply(x, cite, character(1), USE.NAMES = FALSE))
}

.hstat_rlog_vec <- function(x) {
  if (!length(x)) return("NULL")
  paste0("c(", paste(sprintf('"%s"', x), collapse = ", "), ")")
}

# Code R correspondant a UNE analyse capturee. NULL si rien de fidele n'est
# reconstituable — l'appelant ecrira alors un commentaire.
hstat_rlog_code <- function(ctx, donnees = "donnees") {
  if (is.null(ctx)) return(NULL)
  v <- intersect(ctx$meta$variables %||% character(0), ctx$meta$variables %||% character(0))
  v <- as.character(v[nzchar(v)])
  g <- as.character((ctx$meta$groupe %||% character(0)))
  g <- g[nzchar(g)]
  t <- tolower(chartr("àâäéèêëîïôöùûüç", "aaaeeeeiioouuuc", ctx$title %||% ""))
  y <- if (length(v)) .hstat_rlog_nom(v[1]) else NULL
  f <- if (length(g)) .hstat_rlog_nom(g[1]) else NULL
  fml <- if (!is.null(y) && !is.null(f)) sprintf("%s ~ %s", y, f) else NULL

  switch(ctx$module %||% "",

    "Exploration" = c(sprintf("str(%s)", donnees), sprintf("summary(%s)", donnees)),

    "Analyses descriptives" = if (length(v))
      c(sprintf("summary(%s[, %s, drop = FALSE])", donnees, .hstat_rlog_vec(v)),
        if (!is.null(fml)) sprintf("aggregate(%s, data = %s, FUN = mean)", fml, donnees)),

    "Correlations" = if (length(v) >= 2)
      c(sprintf('cor(%s[, %s], use = "pairwise.complete.obs")', donnees, .hstat_rlog_vec(v)),
        sprintf("cor.test(%s$%s, %s$%s)", donnees, .hstat_rlog_nom(v[1]),
                donnees, .hstat_rlog_nom(v[2]))),

    "Tests statistiques" = {
      if (grepl("normalite", t)) {
        if (length(v)) sprintf("shapiro.test(%s$%s)", donnees, y) else NULL
      } else if (grepl("homogeneite|levene", t)) {
        if (!is.null(fml)) sprintf("car::leveneTest(%s, data = %s)", fml, donnees) else NULL
      } else if (grepl("kruskal", t)) {
        if (!is.null(fml)) sprintf("kruskal.test(%s, data = %s)", fml, donnees) else NULL
      } else if (grepl("wilcoxon|mann", t)) {
        if (!is.null(fml)) sprintf("wilcox.test(%s, data = %s)", fml, donnees) else NULL
      } else if (grepl("anova", t)) {
        if (!is.null(fml)) c(sprintf("modele <- aov(%s, data = %s)", fml, donnees),
                             "summary(modele)") else NULL
      } else if (grepl("test t|student|welch", t)) {
        if (!is.null(fml)) sprintf("t.test(%s, data = %s)", fml, donnees) else NULL
      } else if (grepl("regression lineaire", t)) {
        if (!is.null(fml)) c(sprintf("modele <- lm(%s, data = %s)", fml, donnees),
                             "summary(modele)") else NULL
      } else if (grepl("chi", t)) {
        if (length(v) >= 1 && !is.null(f))
          sprintf("chisq.test(table(%s$%s, %s$%s))", donnees, y, donnees, f) else NULL
      } else if (grepl("1 echantillon|conformite|norme|equivalence|signe", t)) {
        # Les tests a une reference dependent de la valeur cible saisie :
        # elle figure dans les parametres, on la reporte telle quelle.
        NULL
      } else NULL
    },

    "Comparaisons multiples" = if (!is.null(fml))
      c(sprintf("modele <- aov(%s, data = %s)", fml, donnees), "TukeyHSD(modele)"),

    "Analyses multivariees" = {
      if (grepl("composantes principales|\\bacp\\b", t) && length(v))
        c(sprintf("acp <- FactoMineR::PCA(%s[, %s], graph = FALSE)", donnees, .hstat_rlog_vec(v)),
          "summary(acp)")
      else if (grepl("k-means", t) && length(v))
        sprintf("stats::kmeans(scale(%s[, %s]), centers = 3)", donnees, .hstat_rlog_vec(v))
      else if (grepl("correspondances multiples|\\bacm\\b", t) && length(v))
        sprintf("FactoMineR::MCA(%s[, %s], graph = FALSE)", donnees, .hstat_rlog_vec(v))
      else if (grepl("correspondances", t) && length(v) >= 2)
        sprintf("FactoMineR::CA(table(%s$%s, %s$%s), graph = FALSE)",
                donnees, .hstat_rlog_nom(v[1]), donnees, .hstat_rlog_nom(v[2]))
      else if (grepl("donnees mixtes|afdm", t) && length(v))
        sprintf("FactoMineR::FAMD(%s[, %s], graph = FALSE)", donnees, .hstat_rlog_vec(v))
      else if (grepl("factorielle exploratoire|\\bafe\\b", t) && length(v))
        sprintf("psych::fa(%s[, %s], nfactors = 2, rotate = \"varimax\")",
                donnees, .hstat_rlog_vec(v))
      else if (grepl("regression lineaire multiple", t) && !is.null(fml))
        c(sprintf("modele <- lm(%s, data = %s)", fml, donnees), "summary(modele)")
      else NULL
    },

    "Analyses qualitatives" = if (length(v) >= 2)
      sprintf("chisq.test(table(%s$%s, %s$%s))", donnees, .hstat_rlog_nom(v[1]),
              donnees, .hstat_rlog_nom(v[2]))
      else if (length(v) == 1) sprintf("table(%s$%s)", donnees, y),

    "Series temporelles" = if (length(v))
      c(sprintf("serie <- ts(%s$%s)", donnees, y),
        "modele <- forecast::auto.arima(serie)",
        "forecast::forecast(modele, h = 12)"),

    NULL)
}

# Script complet de la session.
hstat_rlog_script <- function(history, source = NULL, version = NULL,
                              donnees = "donnees") {
  entete <- c(
    "# =============================================================================",
    sprintf("# Journal de session HStat%s",
            if (!is.null(version)) paste0(" ", version) else ""),
    sprintf("# Genere le %s", format(Sys.time(), "%Y-%m-%d a %H:%M:%S")),
    "# -----------------------------------------------------------------------------",
    "# Script reconstitue a partir des analyses menees dans l'application.",
    "#",
    "# A LIRE AVANT DE L'EXECUTER :",
    "#  - Verifiez le chargement des donnees ci-dessous : le chemin, le separateur",
    "#    et l'encodage dependent de votre fichier.",
    "#  - Les etapes marquees « NON RECONSTITUE » ont ete faites de facon",
    "#    interactive. Leurs parametres sont rappeles en commentaire, mais le code",
    "#    n'est pas ecrit : mieux vaut une lacune signalee qu'un code plausible et",
    "#    faux, qui donnerait un resultat different de celui que vous avez lu.",
    "#  - Les analyses apparaissent dans l'ordre ou vous les avez menees.",
    "# =============================================================================",
    "")

  chargement <- c(
    "# ---- Donnees ----------------------------------------------------------------",
    if (!is.null(source) && nzchar(source))
      sprintf('# Fichier d\'origine : %s', source) else NULL,
    sprintf('%s <- read.csv("mon_fichier.csv", stringsAsFactors = FALSE)', donnees),
    "")

  if (is.null(history) || !length(history))
    return(paste(c(entete, chargement,
                   "# Aucune analyse enregistree pour cette session."),
                 collapse = "\n"))

  corps <- unlist(lapply(seq_along(history), function(i) {
    ctx <- history[[i]]
    code <- hstat_rlog_code(ctx, donnees)
    params <- unlist(lapply(names(ctx$meta), function(k) {
      val <- ctx$meta[[k]]
      if (is.null(val) || !length(val)) return(NULL)
      sprintf("#   %s : %s", k, paste(utils::head(as.character(val), 10), collapse = ", "))
    }))
    titre <- sprintf("# ---- %d. %s : %s ", i, ctx$module, ctx$title)
    c(paste0(titre, strrep("-", max(3L, 79L - nchar(titre)))),
      if (!is.null(ctx$time)) sprintf("#   (%s)", format(ctx$time, "%H:%M:%S")),
      params,
      if (is.null(code))
        c("#   NON RECONSTITUE : etape realisee de facon interactive.", "")
      else c(code, ""))
  }))

  paste(c(entete, chargement, corps), collapse = "\n")
}


# ---------------------------------------------------------------------------
# DIAGNOSTIC DE QUALITE DES DONNEES
# ---------------------------------------------------------------------------
# Ce que les logiciels du marche appellent « data health check ». Entierement
# deterministe et hors ligne : chaque constat porte sa GRAVITE et une
# SUGGESTION concrete, pas un simple pourcentage. L'utilisateur decide ; le
# diagnostic lui donne de quoi decider.
#
# L'analyse porte sur un echantillon au-dela de 20 000 lignes : les proportions
# restent fiables et le diagnostic reste instantane sur un gros fichier.
# ---------------------------------------------------------------------------

HSTAT_QUALITE_GRAVITES <- c("bloquant", "important", "a surveiller")

.hstat_q_row <- function(variable, constat, gravite, suggestion) {
  data.frame(Variable = variable, Constat = constat, Gravite = gravite,
             Suggestion = suggestion, stringsAsFactors = FALSE)
}

hstat_data_quality <- function(df, seuil_na = 0.20, seuil_modalite = 0.95,
                               seuil_cor = 0.95, max_modalites = 30,
                               n_echantillon = 20000L) {
  if (is.null(df) || !NROW(df) || !NCOL(df)) return(NULL)
  df <- as.data.frame(df)
  n_total <- nrow(df)
  if (n_total > n_echantillon) {
    set.seed(1L)
    df <- df[sort(sample.int(n_total, n_echantillon)), , drop = FALSE]
  }
  n <- nrow(df); p <- ncol(df)
  out <- list()

  for (nm in names(df)) {
    x <- df[[nm]]
    chr <- is.character(x) || is.factor(x)
    vide <- if (chr) is.na(x) | !nzchar(trimws(as.character(x))) else is.na(x)
    taux_na <- mean(vide)

    # --- Valeurs manquantes -------------------------------------------------
    if (taux_na >= 0.90)
      out <- c(out, list(.hstat_q_row(nm,
        sprintf("%.0f %% de valeurs manquantes", 100 * taux_na), "bloquant",
        "Variable quasi vide : l'exclure des analyses, ou retrouver la source des donnees manquantes.")))
    else if (taux_na >= 0.50)
      out <- c(out, list(.hstat_q_row(nm,
        sprintf("%.0f %% de valeurs manquantes", 100 * taux_na), "important",
        "Au-dela de la moitie, l'imputation invente plus qu'elle ne restitue. Preferer l'exclusion, ou une analyse sur cas complets en le declarant.")))
    else if (taux_na >= seuil_na)
      out <- c(out, list(.hstat_q_row(nm,
        sprintf("%.0f %% de valeurs manquantes", 100 * taux_na), "a surveiller",
        "Onglet Nettoyage : imputation par la mediane/le mode, ou par kNN / missForest si le mecanisme n'est pas aleatoire.")))

    vals <- x[!vide]
    if (!length(vals)) next
    u <- length(unique(vals))

    # --- Variables sans information ----------------------------------------
    if (u == 1L) {
      out <- c(out, list(.hstat_q_row(nm,
        sprintf("une seule valeur (« %s »)", substr(as.character(vals[1]), 1, 30)), "important",
        "Variable constante : elle ne peut expliquer aucune variation. La retirer des modeles (elle fait aussi echouer l'ACP et la standardisation).")))
      next
    }
    if (chr && u >= 0.95 * length(vals) && u > 20)
      out <- c(out, list(.hstat_q_row(nm,
        sprintf("%d valeurs distinctes sur %d observations", u, length(vals)), "a surveiller",
        "Ressemble a un identifiant ou a du texte libre. Comme identifiant : l'exclure des analyses. Comme texte : l'onglet Analyses qualitatives sait le coder et le thematiser.")))

    if (chr) {
      tb <- sort(table(as.character(vals)), decreasing = TRUE)
      # --- Modalite ecrasante ---------------------------------------------
      if (tb[1] / length(vals) >= seuil_modalite)
        out <- c(out, list(.hstat_q_row(nm,
          sprintf("la modalite « %s » couvre %.0f %% des reponses", names(tb)[1],
                  100 * tb[1] / length(vals)), "important",
          "Variable quasi constante : aucun test ne detectera de difference. Regrouper les modalites, ou renoncer a l'utiliser comme facteur.")))
      # --- Modalites trop rares --------------------------------------------
      rares <- names(tb)[tb < 5]
      if (length(rares) && u <= max_modalites)
        out <- c(out, list(.hstat_q_row(nm,
          sprintf("%d modalite(s) sous 5 observations (%s)", length(rares),
                  paste(utils::head(rares, 4), collapse = ", ")), "a surveiller",
          "Les tests du Chi2 et les approximations asymptotiques y perdent leur validite. Regrouper ces modalites, ou passer au test exact de Fisher.")))
      if (u > max_modalites && u < 0.95 * length(vals))
        out <- c(out, list(.hstat_q_row(nm,
          sprintf("%d modalites distinctes", u), "a surveiller",
          sprintf("Au-dela de %d modalites, les tableaux croises deviennent illisibles et les effectifs trop faibles. Regrouper en categories plus larges.", max_modalites))))

      # --- Nombres stockes en texte ----------------------------------------
      num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(vals))))
      if (mean(!is.na(num)) >= 0.95 && u > 10)
        out <- c(out, list(.hstat_q_row(nm,
          "des nombres stockes comme du texte", "important",
          "Convertir en numerique (onglet Nettoyage). En l'etat, moyennes, correlations et tests quantitatifs sont impossibles sur cette variable.")))
    } else if (is.numeric(vals)) {
      v <- as.numeric(vals)
      # --- Valeurs extremes -------------------------------------------------
      qs <- stats::quantile(v, c(.25, .75), na.rm = TRUE)
      iqr <- qs[2] - qs[1]
      if (is.finite(iqr) && iqr > 0) {
        ext <- sum(v < qs[1] - 3 * iqr | v > qs[2] + 3 * iqr)
        if (ext > 0)
          out <- c(out, list(.hstat_q_row(nm,
            sprintf("%d valeur(s) extreme(s) (au-dela de 3 ecarts interquartiles)", ext), "a surveiller",
            "Verifier s'il s'agit d'erreurs de saisie ou de vraies observations. Si elles sont reelles, preferer les tests de rangs, qui n'en dependent pas.")))
      }
      if (any(!is.finite(v)))
        out <- c(out, list(.hstat_q_row(nm,
          sprintf("%d valeur(s) infinie(s) ou non numerique(s)", sum(!is.finite(v))), "bloquant",
          "Ces valeurs font echouer la plupart des calculs. Les remplacer ou les retirer dans l'onglet Nettoyage.")))
    }
  }

  # --- Redondance entre variables quantitatives ------------------------------
  quanti <- names(df)[vapply(df, function(z) is.numeric(z) &&
                             length(unique(stats::na.omit(z))) > 2, logical(1))]
  if (length(quanti) >= 2) {
    M <- suppressWarnings(stats::cor(df[, quanti, drop = FALSE], use = "pairwise.complete.obs"))
    idx <- which(upper.tri(M) & abs(M) >= seuil_cor & is.finite(M), arr.ind = TRUE)
    if (nrow(idx))
      for (k in seq_len(min(nrow(idx), 8L)))
        out <- c(out, list(.hstat_q_row(
          paste(quanti[idx[k, 1]], "/", quanti[idx[k, 2]]),
          sprintf("correlation de %.2f entre ces deux variables", M[idx[k, 1], idx[k, 2]]), "important",
          "Redondance quasi parfaite : en garder une seule. Ensemble, elles rendent une regression instable (colinearite) et faussent la lecture des coefficients.")))
  }

  # --- Lignes en double -------------------------------------------------------
  dup <- sum(duplicated(df))
  if (dup > 0)
    out <- c(out, list(.hstat_q_row("(jeu de donnees)",
      sprintf("%d ligne(s) strictement identique(s)", dup), "important",
      "Doublons probables de saisie ou d'import. Les supprimer, sinon ils gonflent artificiellement les effectifs et resserrent a tort les intervalles de confiance.")))

  # --- Effectif au regard du nombre de variables -------------------------------
  if (n < 5 * p)
    out <- c(out, list(.hstat_q_row("(jeu de donnees)",
      sprintf("%d observations pour %d variables", n, p), "important",
      "Trop peu d'observations par variable : les modeles multivaries surapprendront. Reduire le nombre de variables, ou se limiter a des analyses bivariees.")))
  if (n < 30)
    out <- c(out, list(.hstat_q_row("(jeu de donnees)",
      sprintf("effectif total de %d observations", n), "a surveiller",
      "Sous 30 observations, les approximations normales sont fragiles : preferer les tests exacts et les tests de rangs.")))

  if (!length(out)) return(.hstat_q_row("(jeu de donnees)",
    "aucun probleme detecte", "a surveiller",
    "Structure saine : valeurs manquantes, modalites, valeurs extremes et redondances sont dans les clous."))

  res <- do.call(rbind, out)
  rang <- c("bloquant" = 1, "important" = 2, "a surveiller" = 3)
  res[order(rang[res$Gravite], res$Variable), , drop = FALSE]
}

# Resume d'une ligne, pour un bandeau ou une notification.
hstat_data_quality_resume <- function(dq) {
  if (is.null(dq) || !nrow(dq)) return(NULL)
  if (nrow(dq) == 1L && grepl("aucun probleme", dq$Constat[1]))
    return("Aucun probleme de qualite detecte sur ce jeu de donnees.")
  n <- table(factor(dq$Gravite, levels = HSTAT_QUALITE_GRAVITES))
  parts <- c(if (n[["bloquant"]]) sprintf("%d bloquant(s)", n[["bloquant"]]),
             if (n[["important"]]) sprintf("%d important(s)", n[["important"]]),
             if (n[["a surveiller"]]) sprintf("%d a surveiller", n[["a surveiller"]]))
  sprintf("%d constat(s) de qualite : %s.", nrow(dq), paste(parts, collapse = ", "))
}


# ---------------------------------------------------------------------------
# RECOMMANDATION D'ANALYSE - DETERMINISTE, HORS LIGNE
# ---------------------------------------------------------------------------
# Regles statistiques classiques appliquees au profil. Renvoie un tableau
# ordonne : l'analyse la plus adaptee d'abord, avec le MOTIF de chaque
# recommandation — c'est le motif qui permet a l'utilisateur de trancher, pas
# le classement.
# ---------------------------------------------------------------------------

.hstat_reco_row <- function(analyse, pertinence, pourquoi, conditions = "",
                            alternative = "") {
  data.frame(Analyse = analyse, Pertinence = pertinence, Pourquoi = pourquoi,
             `Conditions a verifier` = conditions, `Si non remplies` = alternative,
             check.names = FALSE, stringsAsFactors = FALSE)
}

hstat_reco_analyses <- function(profile) {
  if (is.null(profile)) return(NULL)
  v <- profile$variables
  g <- profile$groupe
  types <- vapply(v, function(e) e$type, character(1))
  quanti <- names(types)[types == "quantitative"]
  quali  <- names(types)[types %in% c("categorielle", "binaire")]
  ordin  <- names(types)[types == "ordinale"]
  normal_tous <- length(quanti) > 0 &&
    all(vapply(quanti, function(nm) isTRUE(v[[nm]]$normale$ok), logical(1)))
  petit <- profile$n < 30
  out <- list()

  # Variables sans aucune valeur observee. Elles sont deja exclues des listes
  # ci-dessus (leur type vaut « indeterminable »), mais le silence serait
  # trompeur : l'utilisateur les a choisies, il doit savoir pourquoi elles ne
  # donnent lieu a aucune proposition.
  vides <- names(types)[types == "indeterminable"]
  if (length(vides)) {
    out <- c(out, list(.hstat_reco_row(
      "Aucune analyse possible en l'etat", "Bloquant",
      sprintf("%s ne comporte aucune valeur observee : il n'y a rien a analyser.",
              paste(sprintf("« %s »", vides), collapse = ", ")),
      "Au moins quelques observations non manquantes par variable.",
      paste("Verifiez l'import (separateur, colonne decalee) et les filtres",
            "actifs, ou traitez les valeurs manquantes dans l'onglet Nettoyage."))))
  }

  # ---- Une quantitative, un facteur ----
  if (length(quanti) >= 1 && !is.null(g)) {
    homo <- g$variances_homogenes
    if (g$k == 2) {
      if (profile$apparie) {
        out <- c(out, list(
          if (normal_tous)
            .hstat_reco_row("Test t apparie", "Recommandee",
              "Une variable quantitative mesuree deux fois sur les memes sujets, distribution compatible avec la normalite.",
              "Normalite des DIFFERENCES entre les deux mesures.",
              "Test des rangs signes de Wilcoxon.")
          else
            .hstat_reco_row("Wilcoxon apparie (rangs signes)", "Recommandee",
              "Mesures appariees et distribution qui s'ecarte de la normalite.",
              "Symetrie approximative des differences.",
              "Test des signes, qui ne suppose rien de plus.")))
      } else {
        out <- c(out, list(
          if (normal_tous && !isFALSE(homo) && !isTRUE(g$petits_effectifs))
            .hstat_reco_row("Test t de Student (deux echantillons)", "Recommandee",
              sprintf("Une quantitative comparee entre %d groupes independants, normalite intra-groupe acceptable%s.",
                      g$k, if (isTRUE(homo)) " et variances homogenes" else ""),
              "Independance des observations ; normalite dans chaque groupe.",
              "Test t de Welch si les variances different, Mann-Whitney sinon.")
          else if (normal_tous && !isTRUE(g$petits_effectifs))
            .hstat_reco_row("Test t de Welch", "Recommandee",
              "Normalite intra-groupe acceptable mais variances inegales entre les deux groupes : Welch ne les suppose pas egales.",
              "Independance des observations.",
              "Mann-Whitney.")
          else
            .hstat_reco_row("Mann-Whitney (Wilcoxon)", "Recommandee",
              sprintf("Deux groupes independants%s.",
                      if (isTRUE(g$petits_effectifs))
                        " dont au moins un compte moins de 5 observations : la normalite n'y est pas verifiable"
                      else " et distribution non normale dans au moins un groupe"),
              "Independance ; formes de distribution comparables si l'on conclut sur les medianes.",
              "Comparaison des rangs seule, sans conclure sur la mediane.")))
      }
    } else if (g$k > 2) {
      out <- c(out, list(
        if (normal_tous && !isFALSE(homo) && !isTRUE(g$petits_effectifs))
          .hstat_reco_row("ANOVA a un facteur", "Recommandee",
            sprintf("Une quantitative comparee entre %d groupes, normalite intra-groupe et homogeneite des variances acceptables.", g$k),
            "Independance ; normalite des residus ; homogeneite des variances.",
            "ANOVA de Welch, ou Kruskal-Wallis.")
        else if (normal_tous && !isTRUE(g$petits_effectifs))
          .hstat_reco_row("ANOVA de Welch", "Recommandee",
            "Normalite intra-groupe acceptable mais variances heterogenes entre groupes.",
            "Independance des observations.",
            "Kruskal-Wallis.")
        else if (isTRUE(g$petits_effectifs))
          .hstat_reco_row("Kruskal-Wallis", "Recommandee",
            sprintf("%d groupes dont au moins un compte moins de 5 observations : la normalite n'y est pas verifiable, un test de rangs ne la suppose pas.", g$k),
            "Independance des observations.",
            "Test de permutation, si meme les rangs sont trop peu nombreux.")
        else
          .hstat_reco_row("Kruskal-Wallis", "Recommandee",
            sprintf("%d groupes independants et distribution non normale dans au moins un groupe.", g$k),
            "Independance ; formes comparables pour conclure sur les medianes.",
            "Comparaison des rangs seule.")))
      out <- c(out, list(.hstat_reco_row(
        "Comparaisons post-hoc", "A enchainer",
        "Un test global significatif dit qu'au moins deux groupes different, jamais lesquels : le post-hoc le precise.",
        "Correction pour comparaisons multiples (Tukey, Holm, Bonferroni...).",
        "Sans correction, le risque d'erreur de premiere espece s'accumule.")))
    }
    if (isTRUE(g$petits_effectifs))
      out <- c(out, list(.hstat_reco_row(
        "Test exact / permutation", "A envisager",
        "Au moins un groupe compte moins de 5 observations : les approximations asymptotiques deviennent peu fiables.",
        "-", "-")))
  }

  # ---- Deux quantitatives, aucun facteur ----
  if (length(quanti) >= 2 && is.null(g)) {
    out <- c(out, list(
      if (normal_tous)
        .hstat_reco_row("Correlation de Pearson", "Recommandee",
          "Deux variables quantitatives dont les distributions sont compatibles avec la normalite.",
          "Relation lineaire ; absence de valeurs extremes influentes.",
          "Correlation de Spearman, qui ne suppose que la monotonie.")
      else
        .hstat_reco_row("Correlation de Spearman", "Recommandee",
          "Deux quantitatives dont au moins une s'ecarte de la normalite : la correlation des rangs ne la suppose pas.",
          "Relation monotone.",
          "Tau de Kendall, plus robuste sur petits effectifs.")))
    out <- c(out, list(.hstat_reco_row(
      "Regression lineaire", "A envisager",
      "Si l'une des deux variables est expliquee par l'autre, la regression quantifie l'effet la ou la correlation ne mesure que l'association.",
      "Linearite, independance, homoscedasticite, normalite des residus.",
      "Regression robuste, ou transformation de la reponse.")))
    if (length(quanti) >= 3)
      out <- c(out, list(.hstat_reco_row(
        "ACP (analyse en composantes principales)", "A envisager",
        sprintf("%d variables quantitatives : l'ACP resume leur structure commune et revele les redondances.", length(quanti)),
        "Variables correlees entre elles ; effectif superieur au nombre de variables.",
        "Matrice de correlations seule si les variables sont independantes.")))
  }

  # ---- Deux qualitatives ----
  if (length(quali) >= 2 || (length(quali) >= 1 && !is.null(g) && length(quanti) == 0)) {
    petits <- isTRUE(g$petits_effectifs)
    out <- c(out, list(
      if (petits)
        .hstat_reco_row("Test exact de Fisher", "Recommandee",
          "Tableau croise dont certains effectifs attendus sont faibles : le chi-deux n'y est plus valide.",
          "Effectifs attendus < 5 dans plus de 20 % des cases.",
          "Regroupement de modalites, puis chi-deux.")
      else
        .hstat_reco_row("Test du chi-deux d'independance", "Recommandee",
          "Deux variables categorielles : le chi-deux teste leur independance.",
          "Effectifs attendus >= 5 dans au moins 80 % des cases.",
          "Test exact de Fisher.")))
    out <- c(out, list(.hstat_reco_row(
      "V de Cramer / Odds Ratio", "A enchainer",
      "Un test dit s'il y a association, jamais sa force : la taille d'effet le dit.",
      "-", "-")))
    if (length(quali) >= 3)
      out <- c(out, list(.hstat_reco_row(
        "ACM (analyse des correspondances multiples)", "A envisager",
        sprintf("%d variables categorielles : l'ACM positionne individus et modalites dans un meme plan.", length(quali)),
        "Modalites suffisamment representees (regrouper les plus rares).", "-")))
  }

  # ---- Ordinales ----
  if (length(ordin) >= 1) {
    out <- c(out, list(.hstat_reco_row(
      "Analyse ordinale (Likert, rangs)", if (length(ordin) >= 2) "Recommandee" else "A envisager",
      "Variable a modalites ordonnees : la traiter comme quantitative suppose des ecarts egaux entre echelons, ce qui est rarement vrai.",
      "Ordre des modalites correctement declare.",
      "Traitement categoriel simple si l'ordre n'a pas de sens.")))
    if (length(ordin) >= 2)
      out <- c(out, list(.hstat_reco_row(
        "Correlation de Spearman / Kendall", "A envisager",
        "Deux variables ordinales : la correlation des rangs respecte leur nature.",
        "Relation monotone.", "-")))
  }

  # ---- Reponse binaire : modelisation ----
  bin <- names(types)[types == "binaire"]
  if (length(bin) >= 1 && (length(quanti) >= 1 || length(quali) >= 1))
    out <- c(out, list(.hstat_reco_row(
      "Regression logistique", "A envisager",
      sprintf("« %s » ne prend que deux valeurs : la regression logistique modelise sa probabilite a partir des autres variables.", bin[1]),
      "Effectif suffisant par modalite (au moins 10 evenements par predicteur).",
      "Test exact ou regression penalisee si les effectifs sont faibles.")))

  if (!length(out)) return(NULL)
  res <- do.call(rbind, out)
  # Le rang tient a la pertinence, pas a l'ordre d'ecriture des regles.
  ordre <- c("Recommandee" = 1, "A enchainer" = 2, "A envisager" = 3)
  res[order(ordre[res$Pertinence], seq_len(nrow(res))), , drop = FALSE]
}

# Verdict lisible sur l'analyse effectivement realisee : elle figure ou non
# parmi les recommandations. On ne dit jamais « vous avez eu tort » — le
# contexte d'une enquete justifie parfois un choix que les regles ignorent.
# Modules dont la sortie n'est pas un test : les comparer aux recommandations
# n'aurait pas de sens — une statistique descriptive n'est pas un mauvais test,
# c'est une etape anterieure. On y propose la suite plutot qu'un verdict.
HSTAT_RECO_EXPLORATOIRE <- c("Analyses descriptives", "Visualisation",
                             "Exploration", "Analyses qualitatives",
                             "Nettoyage", "Filtrage", "Seuils d'efficacite")

hstat_reco_verdict <- function(reco, titre_analyse, module = NULL) {
  if (is.null(reco) || !nrow(reco) || is.null(titre_analyse) || !nzchar(titre_analyse))
    return(NULL)
  if (!is.null(module) && module %in% HSTAT_RECO_EXPLORATOIRE) {
    reco_1 <- reco$Analyse[reco$Pertinence == "Recommandee"]
    return(list(
      coherent = TRUE, exploratoire = TRUE,
      message = sprintf(
        "« %s » decrit vos donnees : c'est une etape preliminaire, pas un test, il n'y a donc rien a valider ici. Pour aller plus loin, le profil de vos variables appelle %s. A vous de decider si cette suite a du sens pour votre question de recherche.",
        titre_analyse,
        if (length(reco_1)) paste(reco_1, collapse = " ou ") else "une analyse inferentielle")))
  }
  cle <- function(x) {
    x <- tolower(chartr("àâäéèêëîïôöùûüç", "aaaeeeeiioouuuc", x))
    gsub("[^a-z]+", " ", x)
  }
  t <- cle(titre_analyse)
  mots <- lapply(cle(reco$Analyse), function(a) strsplit(a, " ")[[1]])
  hit <- vapply(mots, function(w) {
    w <- w[nchar(w) >= 4]
    length(w) > 0 && any(vapply(w, function(z) grepl(z, t, fixed = TRUE), logical(1)))
  }, logical(1))
  reco_1 <- reco$Analyse[reco$Pertinence == "Recommandee"]
  if (any(hit))
    list(coherent = TRUE, exploratoire = FALSE,
         message = sprintf(
           "L'analyse que vous avez menee figure parmi celles que le profil de vos donnees appelle (%s).",
           paste(reco$Analyse[hit], collapse = ", ")))
  else
    list(coherent = FALSE, exploratoire = FALSE,
         message = sprintf(
           "Au vu du profil des variables, %s aurait ete le choix le plus direct. Cela ne disqualifie pas votre analyse : un objectif de recherche ou une contrainte de terrain peut la justifier. A vous de trancher.",
           if (length(reco_1)) paste(reco_1, collapse = " ou ") else "une autre approche"))
}


# ---------------------------------------------------------------------------
# INTERPRETATION DES RESULTATS
# ---------------------------------------------------------------------------

# Reperage des p-values dans un tableau de resultats, quel que soit le module
# qui l'a produit : les intitules varient (`p`, `p.value`, `Pr(>F)`, `p ajustee`...)
# mais la colonne existe presque toujours.
.hstat_ai_pcols <- function(tb) {
  nm <- names(tb)
  hit <- grepl("^p([._ ]?(value|valeur))?$|p[-_. ]?value|^pr\\(|p[-_. ]?ajust",
               tolower(nm), perl = TRUE)
  nm[hit & vapply(tb, is.numeric, logical(1))]
}

.hstat_ai_signif <- function(p, alpha = 0.05) {
  if (is.na(p)) return("indeterminee")
  if (p < alpha) "significatif" else "non significatif"
}

# Lecture automatique, DETERMINISTE, des resultats captures. Sert de reponse
# quand aucun modele n'est joignable, et de garde-fou quand il l'est : ces
# chiffres-la ne sont pas generes, ils sont lus.
hstat_ai_interpret_offline <- function(ctx, profile = NULL, reco = NULL,
                                       verdict = NULL, alpha = 0.05) {
  if (is.null(ctx)) return(NULL)
  L <- c(sprintf("## %s", ctx$title), "")

  if (!is.null(profile)) {
    ty <- vapply(profile$variables, function(e) e$type, character(1))
    L <- c(L, "### Donnees analysees", "",
           sprintf("- %d observations ; %s.", profile$n,
                   paste(sprintf("%d variable(s) %s", table(ty), names(table(ty))),
                         collapse = ", ")))
    if (!is.null(profile$groupe))
      L <- c(L, sprintf("- Facteur « %s » a %d modalites (effectifs : %s)%s.",
                        profile$groupe$nom, profile$groupe$k,
                        paste(profile$groupe$effectifs, collapse = ", "),
                        if (isTRUE(profile$groupe$petits_effectifs))
                          " — au moins un groupe sous 5 observations" else ""))
    for (nm in names(profile$variables)) {
      e <- profile$variables[[nm]]
      if (is.null(e$normale) || is.na(e$normale$ok)) next
      L <- c(L, sprintf("- « %s » : distribution %s la normalite (%s%s).", nm,
                        if (isTRUE(e$normale$ok)) "compatible avec" else "incompatible avec",
                        e$normale$methode,
                        if (!is.na(e$normale$p)) sprintf(", p = %.4g", e$normale$p) else ""))
    }
    L <- c(L, "")
  }

  # --- Lecture des p-values ---
  lues <- 0L
  for (nm in names(ctx$tables)) {
    tb <- as.data.frame(ctx$tables[[nm]])
    pc <- .hstat_ai_pcols(tb)
    if (!length(pc)) next
    if (lues == 0L) L <- c(L, "### Ce que disent les tests", "")
    lab_col <- names(tb)[vapply(tb, function(x) is.character(x) || is.factor(x), logical(1))]
    for (i in seq_len(min(nrow(tb), 20L))) {
      p <- suppressWarnings(as.numeric(tb[[pc[1]]][i]))
      etiq <- if (length(lab_col)) as.character(tb[[lab_col[1]]][i]) else sprintf("ligne %d", i)
      L <- c(L, sprintf("- **%s** : p = %s -> %s au seuil de %.0f %%.",
                        etiq,
                        if (is.na(p)) "n.d." else format(signif(p, 3), scientific = p < 1e-4),
                        .hstat_ai_signif(p, alpha), 100 * alpha))
      lues <- lues + 1L
    }
    if (nrow(tb) > 20L) L <- c(L, sprintf("- (... %d lignes supplementaires)", nrow(tb) - 20L))
  }
  if (lues == 0L)
    L <- c(L, "### Resultats", "",
           "Aucune p-value reperee automatiquement dans les tableaux : reportez-vous au detail ci-dessous.")
  L <- c(L, "")

  if (!is.null(verdict))
    L <- c(L, "### Coherence du choix d'analyse", "", verdict$message, "")

  if (!is.null(reco) && nrow(reco)) {
    L <- c(L, "### Analyses appelees par vos donnees", "")
    for (i in seq_len(nrow(reco)))
      L <- c(L, sprintf("- **%s** *(%s)* — %s", reco$Analyse[i], reco$Pertinence[i],
                        reco$Pourquoi[i]))
    L <- c(L, "",
           paste0("*Ces propositions decoulent du profil de vos variables. ",
                  "Le choix de l'analyse, lui, vous appartient : lui seul engage ",
                  "l'interpretation scientifique de votre travail.*"))
  }
  paste(L, collapse = "\n")
}

HSTAT_AI_NIVEAUX <- c(
  "Rapport scientifique (concis, normalise)" = "scientifique",
  "Vulgarise (sans jargon)"                  = "vulgarise",
  "Detaille (methode + limites)"             = "detaille")

hstat_ai_interpret_prompt <- function(ctx, profile = NULL, reco = NULL,
                                      verdict = NULL, niveau = "scientifique",
                                      contexte = "", alpha = 0.05) {
  style <- switch(niveau,
    vulgarise = paste0("Ecris pour un lecteur sans formation statistique : pas de jargon, ",
                       "pas de symboles, des phrases courtes. Explique ce que le resultat ",
                       "signifie concretement."),
    detaille  = paste0("Redige un texte complet : rappel de la methode et de ses conditions ",
                       "d'application, resultats chiffres, taille d'effet, limites de ",
                       "l'analyse et portee des conclusions."),
    paste0("Redige comme une section « Resultats » d'article scientifique : concis, ",
           "impersonnel, chiffres entre parentheses selon l'usage (statistique, ddl, ",
           "p, taille d'effet)."))

  bloc_reco <- if (!is.null(reco) && nrow(reco))
    paste0("\n\nANALYSES QUE LE PROFIL DES DONNEES APPELLE (calculees par HStat, ",
           "deterministes — reprends-les telles quelles, n'en invente pas d'autres) :\n",
           paste(sprintf("- %s (%s) : %s", reco$Analyse, reco$Pertinence, reco$Pourquoi),
                 collapse = "\n")) else ""

  bloc_profil <- if (!is.null(profile)) {
    ty <- vapply(profile$variables, function(e) e$type, character(1))
    nrm <- vapply(profile$variables, function(e)
      if (is.null(e$normale) || is.na(e$normale$ok)) "non evaluee"
      else if (isTRUE(e$normale$ok)) "compatible avec la normalite"
      else "non normale", character(1))
    paste0("\n\nPROFIL DES DONNEES :\n",
           sprintf("- %d observations\n", profile$n),
           paste(sprintf("- %s : %s (%s)", names(ty), ty, nrm), collapse = "\n"),
           if (!is.null(profile$groupe))
             sprintf("\n- Facteur %s : %d modalites, effectifs %s",
                     profile$groupe$nom, profile$groupe$k,
                     paste(profile$groupe$effectifs, collapse = "/")) else "")
  } else ""

  paste0(
    "Tu es statisticien. On te donne les RESULTATS d'une analyse deja realisee ",
    "par l'utilisateur. Ta mission est de les INTERPRETER, en francais.\n\n",
    "REGLES ABSOLUES :\n",
    "1. N'invente aucun chiffre. N'utilise que les valeurs presentes ci-dessous. ",
    "Si une valeur manque, dis-le au lieu de la deviner.\n",
    "2. Ne recalcule rien et ne propose pas de relancer l'analyse a ta facon.\n",
    "3. Le choix de l'analyse appartient a l'utilisateur : tu peux signaler ",
    "qu'une autre methode aurait mieux convenu, jamais affirmer qu'il a eu tort.\n",
    sprintf("4. Seuil de signification retenu : %.0f %%.\n", 100 * alpha),
    "\nSTYLE : ", style,
    if (nzchar(trimws(contexte))) paste0("\n\nCONTEXTE DE L'ETUDE : ", contexte) else "",
    bloc_profil, bloc_reco,
    "\n\n", hstat_ai_context_text(ctx),
    "\n\nStructure ta reponse en markdown avec exactement ces sections :\n",
    "## Lecture des resultats\n",
    "## Ce que cela signifie\n",
    "## Precautions et limites\n",
    "## Analyse recommandee pour la suite\n",
    "Dans la derniere section, appuie-toi sur les analyses appelees par le profil ",
    "des donnees, et rappelle que la decision revient a l'utilisateur.")
}


# ---------------------------------------------------------------------------
# GUIDAGE A LA FIN DE L'ANALYSE
# ---------------------------------------------------------------------------
# Une recommandation qui n'arrive que si l'utilisateur pense a changer d'onglet
# n'aide personne. Des qu'une analyse depose son resultat, un bandeau discret
# annonce ce que le profil des donnees appelle, avec un lien vers le detail.
# Le lien bascule d'onglet cote navigateur : pas d'aller-retour serveur, et
# aucun couplage entre les modules d'analyse et l'assistance.
# ---------------------------------------------------------------------------

# Un identifiant de sortie par onglet d'analyse. Declares ici plutot que
# disperses : ajouter un onglet, c'est ajouter une ligne a cette liste et poser
# hstat_ai_hint_slot() dans son interface.
HSTAT_AI_HINT_IDS <- c(
  "aihint_descriptive", "aihint_viz", "aihint_correlation", "aihint_tests",
  "aihint_multiple", "aihint_multivariate", "aihint_qualitative",
  "aihint_timeseries", "aihint_ml", "aihint_dl", "aihint_design",
  "aihint_threshold")

# Emplacement a poser en bas d'un onglet d'analyse.
hstat_ai_hint_slot <- function(id) {
  stopifnot(id %in% HSTAT_AI_HINT_IDS)
  shiny::uiOutput(id)
}

# Greffe l'emplacement a la fin d'un tabItem deja construit. Permet d'ajouter
# le bandeau aux onglets dont l'interface est produite par un module, sans
# toucher au module : `tabItem()` renvoie un simple div, on lui ajoute un
# enfant. Un onglet introuvable ou d'une autre forme est renvoye tel quel
# plutot que de casser la construction de l'interface.
hstat_ai_with_hint <- function(tab, id) {
  if (!inherits(tab, "shiny.tag") || is.null(tab$children)) return(tab)
  tab$children <- c(tab$children, list(hstat_ai_hint_slot(id)))
  tab
}

hstat_ai_hint_ui <- function(ctx, reco, verdict = NULL) {
  if (is.null(ctx)) return(NULL)
  top <- if (!is.null(reco) && nrow(reco))
    reco$Analyse[reco$Pertinence == "Recommandee"] else character(0)
  suite <- if (!is.null(reco) && nrow(reco))
    reco$Analyse[reco$Pertinence == "A enchainer"] else character(0)

  lien <- shiny::tags$a(
    href = "#", style = "font-weight:bold;color:#1b6f8c;",
    onclick = paste0(
      "$('a[href=\"#shiny-tab-aidecision\"]').click();",
      "$(window).scrollTop(0);return false;"),
    shiny::icon("compass-drafting"), " Interpreter ces resultats")

  shiny::div(
    style = paste0("background:#eaf4fb;border-left:5px solid #2e86c1;",
                   "padding:12px 16px;border-radius:6px;margin-top:14px;font-size:13px;"),
    shiny::tags$strong(shiny::icon("lightbulb"), " Aide a la decision"),
    shiny::tags$span(style = "color:#7f8c8d;", sprintf(" - a la suite de : %s", ctx$title)),
    shiny::br(),
    if (length(top))
      shiny::tags$span("Le profil de vos donnees appelle ",
                       shiny::tags$b(paste(top, collapse = " ou ")), ".")
    else
      shiny::tags$span("Choisissez les variables analysees dans l'onglet dedie pour obtenir une recommandation."),
    if (length(suite))
      shiny::tags$span(" A enchainer : ", shiny::tags$b(paste(suite, collapse = ", ")), "."),
    if (!is.null(verdict) && !isTRUE(verdict$coherent))
      shiny::tagList(shiny::br(), shiny::tags$span(style = "color:#b9770e;",
        shiny::icon("circle-question"), " ", verdict$message)),
    shiny::br(),
    lien,
    shiny::tags$span(style = "color:#7f8c8d;",
      " - la methode reste votre choix : l'assistance eclaire, elle ne decide pas."))
}

# Notification a la fin d'une analyse. Volontairement breve : le detail vit
# dans l'onglet, ceci n'est qu'un rappel que l'aide existe et qu'elle a
# quelque chose a dire sur CE resultat.
hstat_ai_hint_text <- function(ctx, reco) {
  if (is.null(ctx)) return(NULL)
  top <- if (!is.null(reco) && nrow(reco))
    reco$Analyse[reco$Pertinence == "Recommandee"] else character(0)
  if (!length(top)) return(NULL)
  sprintf("%s enregistree. Le profil de vos donnees appelle %s.",
          ctx$title, paste(top, collapse = " ou "))
}


# ---------------------------------------------------------------------------
# RENDU
# ---------------------------------------------------------------------------

# Tableau HTML statique. Utilisable dans un renderUI, contrairement a
# shiny::renderTable() qui est une fonction de rendu et non un composant.
.hstat_html_table <- function(df) {
  df <- as.data.frame(df)
  if (!nrow(df)) return(shiny::tags$em(style = "color:#95a5a6;", "(tableau vide)"))
  fmt <- function(x) {
    if (is.numeric(x)) ifelse(is.na(x), "", format(signif(x, 5), trim = TRUE))
    else ifelse(is.na(x), "", as.character(x))
  }
  shiny::tags$div(style = "overflow-x:auto;",
    shiny::tags$table(class = "table table-condensed table-striped",
      shiny::tags$thead(shiny::tags$tr(lapply(names(df), shiny::tags$th))),
      shiny::tags$tbody(lapply(seq_len(nrow(df)), function(i)
        shiny::tags$tr(lapply(seq_along(df), function(j)
          shiny::tags$td(fmt(df[[j]])[i])))))))
}

# Conversion markdown -> HTML des seules constructions demandees au modele
# (titres, gras, italique, listes). Ecrite a la main plutot que confiee a un
# paquet : c'est trois expressions regulieres, et surtout le texte est ECHAPPE
# d'abord — une reponse de modele reste du contenu non fiable qu'on n'injecte
# pas tel quel dans la page.
.hstat_md_to_html <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return("")
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  inline <- function(x) {
    x <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", x, perl = TRUE)
    x <- gsub("(?<!\\*)\\*([^*]+?)\\*(?!\\*)", "<em>\\1</em>", x, perl = TRUE)
    gsub("`([^`]+)`", "<code>\\1</code>", x, perl = TRUE)
  }
  lines <- strsplit(esc(txt), "\n", fixed = TRUE)[[1]]
  out <- character(0); in_ul <- FALSE
  close_ul <- function() { if (in_ul) { out <<- c(out, "</ul>"); in_ul <<- FALSE } }
  for (l in lines) {
    t <- trimws(l)
    if (!nzchar(t)) { close_ul(); next }
    if (grepl("^#{1,6} ", t)) {
      close_ul()
      n <- min(6L, nchar(sub("^(#+).*$", "\\1", t)))
      out <- c(out, sprintf("<h%d style='margin-top:18px;'>%s</h%d>",
                            n + 2L, inline(sub("^#+ +", "", t)), n + 2L))
    } else if (grepl("^[-*+] ", t)) {
      if (!in_ul) { out <- c(out, "<ul>"); in_ul <- TRUE }
      out <- c(out, sprintf("<li>%s</li>", inline(sub("^[-*+] +", "", t))))
    } else {
      close_ul()
      out <- c(out, sprintf("<p>%s</p>", inline(t)))
    }
  }
  close_ul()
  paste(out, collapse = "\n")
}


# ===========================================================================
# ONGLET « INTERPRETATION & AIDE A LA DECISION »
# ===========================================================================
mod_ai_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = "aidecision",
    shiny::fluidRow(
      shinydashboard::box(width = 12, status = "primary", solidHeader = FALSE,
        background = "navy",
        shiny::h3(shiny::icon("compass-drafting"),
                  " Interpretation des resultats & aide a la decision",
                  style = "margin:0;color:white;"),
        shiny::p(style = "margin:8px 0 0 0;color:#d6e4f0;font-size:13px;",
          "L'assistance interprete les resultats que vous venez d'obtenir et vous ",
          "indique quelles analyses le profil de vos donnees appelle. ",
          shiny::tags$b("Elle ne choisit ni ne lance aucune analyse"),
          " : la methode reste votre decision, et votre responsabilite."))
    ),
    shiny::fluidRow(
      shiny::column(4,
        shinydashboard::box(width = 12, status = "primary", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("clipboard-check"), " Analyse a interpreter"),
          shiny::uiOutput(ns("ctx_box")),
          shiny::hr(style = "margin:10px 0;"),
          shiny::h5(shiny::icon("table-columns"), " Profil des donnees"),
          shiny::tags$small(style = "color:#7f8c8d;display:block;margin-bottom:8px;",
            "Sert a la recommandation. Pre-rempli depuis la derniere analyse ; ",
            "ajustez-le si vous voulez explorer un autre scenario."),
          shiny::uiOutput(ns("vars_ui")),
          shiny::uiOutput(ns("group_ui")),
          shiny::checkboxInput(ns("paired"), "Mesures appariees (memes sujets)", FALSE),
          shiny::sliderInput(ns("alpha"), "Seuil de signification", min = 0.001,
                             max = 0.10, value = 0.05, step = 0.001),
          shiny::hr(style = "margin:10px 0;"),
          shiny::selectInput(ns("niveau"), "Niveau de redaction",
                             choices = HSTAT_AI_NIVEAUX, selected = "scientifique"),
          shiny::textAreaInput(ns("contexte"), "Contexte de l'etude (facultatif)",
                               rows = 2,
                               placeholder = "Ex. essai clinique, 3 bras, critere principal"),
          shiny::hr(style = "margin:10px 0;"),
          shiny::actionButton(ns("go"), "Interpreter avec le modele",
                              icon = shiny::icon("wand-magic-sparkles"),
                              class = "btn-primary btn-block"),
          shiny::br(),
          shiny::actionButton(ns("go_offline"), "Lecture automatique (sans modele)",
                              icon = shiny::icon("calculator"),
                              class = "btn-default btn-block btn-sm"),
          shiny::tags$small(style = "color:#7f8c8d;display:block;margin-top:8px;",
            shiny::icon("circle-info"),
            " La lecture automatique ne genere rien : elle relit vos tableaux et ",
            "en enonce les p-values. Elle fonctionne toujours, sans modele.")
        ),
        shinydashboard::box(width = 12, status = "warning", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          title = shiny::tagList(shiny::icon("microchip"), " Moteur d'inference"),
          shiny::radioButtons(ns("engine"), NULL, choices = HSTAT_AI_ENGINES,
                              selected = "local"),
          shiny::conditionalPanel(sprintf("input['%s'] == 'local'", ns("engine")),
            shiny::radioButtons(ns("backend"), "Serveur", choices = HSTAT_AI_BACKENDS,
                                selected = "ollama", inline = TRUE),
            shiny::textInput(ns("url"), "Adresse",
                             value = unname(HSTAT_AI_DEFAULT_URL[["ollama"]])),
            shiny::uiOutput(ns("model_ui")),
            shiny::actionButton(ns("ping"), "Tester la connexion",
                                icon = shiny::icon("plug-circle-check"),
                                class = "btn-default btn-sm btn-block")),
          shiny::conditionalPanel(sprintf("input['%s'] == 'claude'", ns("engine")),
            shiny::passwordInput(ns("key"), "Cle d'API Anthropic",
                                 placeholder = "sk-ant-... (ou ANTHROPIC_API_KEY)")),
          shiny::uiOutput(ns("status")))
      ),
      shiny::column(8,
        shinydashboard::box(width = 12, status = "primary", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("lightbulb"), " Aide a la decision"),
          shiny::tabsetPanel(id = ns("tabs"),
            shiny::tabPanel(shiny::tagList(shiny::icon("file-lines"), " Interpretation"),
              shiny::br(),
              shiny::uiOutput(ns("interp_note")),
              shiny::div(style = "background:#ffffff;border:1px solid #e0e0e0;border-radius:6px;padding:16px 20px;min-height:280px;",
                         shiny::uiOutput(ns("interp"))),
              shiny::br(),
              shiny::fluidRow(
                shiny::column(4, shiny::downloadButton(ns("dl_md"), "Markdown",
                                                       class = "btn-info btn-sm btn-block")),
                shiny::column(4, shiny::downloadButton(ns("dl_txt"), "Texte brut",
                                                       class = "btn-info btn-sm btn-block")))),
            shiny::tabPanel(shiny::tagList(shiny::icon("route"), " Analyse recommandee"),
              shiny::br(),
              shiny::uiOutput(ns("verdict")),
              DT::DTOutput(ns("reco_table")),
              shiny::br(),
              shiny::div(style = "background:#fdf3e3;border-left:5px solid #e67e22;padding:12px 16px;border-radius:6px;font-size:13px;",
                shiny::icon("triangle-exclamation"),
                shiny::tags$b(" Ces propositions ne decident pas a votre place."),
                " Elles decoulent mecaniquement du type de vos variables, de leurs ",
                "effectifs et de tests de normalite et d'homogeneite. Une question de ",
                "recherche, un plan d'experience ou une contrainte de terrain peuvent ",
                "justifier un autre choix — que vous seul etes en mesure de faire.")),
            shiny::tabPanel(shiny::tagList(shiny::icon("magnifying-glass-chart"), " Profil des donnees"),
              shiny::br(),
              DT::DTOutput(ns("profile_table")),
              shiny::br(),
              shiny::uiOutput(ns("profile_notes"))),
            shiny::tabPanel(shiny::tagList(shiny::icon("code"), " Journal & reproductibilite"),
              shiny::br(),
              shiny::div(style = "background:#eafaf1;border-left:5px solid #27ae60;padding:12px 16px;border-radius:6px;font-size:13px;",
                shiny::tags$strong(shiny::icon("scroll"), " Script R de votre session"),
                shiny::tags$p(style = "margin:6px 0 0 0;",
                  "Les analyses que vous avez menees, dans l'ordre, sous forme de code R ",
                  "executable. C'est ce qu'un relecteur attend pour refaire le chemin. ",
                  shiny::tags$b("Les etapes purement interactives sont signalees, pas inventees"),
                  " : un script qui differerait en silence de ce que vous avez lu serait ",
                  "pire que pas de script.")),
              shiny::br(),
              shiny::fluidRow(
                shiny::column(6, shiny::uiOutput(ns("rlog_resume"))),
                shiny::column(3, shiny::textInput(ns("rlog_objet"), "Nom de l'objet de donnees",
                                                  value = "donnees")),
                shiny::column(3, shiny::br(),
                  shiny::downloadButton(ns("dl_rlog"), "Telecharger le script (.R)",
                                        class = "btn-success btn-block"))),
              shiny::tags$pre(style = "background:#f8f9fa;border:1px solid #e0e0e0;border-radius:6px;padding:14px;max-height:560px;overflow:auto;font-size:12px;line-height:1.5;",
                              shiny::textOutput(ns("rlog_script"), container = shiny::tags$code)),
              shiny::br(),
              DT::DTOutput(ns("rlog_table"))),
            shiny::tabPanel(shiny::tagList(shiny::icon("file-word"), " Rapport"),
              shiny::br(),
              shiny::div(style = "background:#eaf4fb;border-left:5px solid #2e86c1;padding:12px 16px;border-radius:6px;font-size:13px;",
                shiny::tags$strong(shiny::icon("file-lines"), " Un document, transmissible tel quel"),
                shiny::tags$p(style = "margin:6px 0 0 0;",
                  "Vos analyses, vos figures et vos interpretations reunies en un ",
                  "rapport redige. ",
                  shiny::tags$b("Le rapport ne calcule rien"),
                  " : il met en forme ce que vous avez deja obtenu. A relire avant ",
                  "diffusion — les interpretations eclairent la lecture, elles ne la valident pas.")),
              shiny::br(),
              shiny::fluidRow(
                shiny::column(5,
                  shiny::textInput(ns("rep_titre"), "Titre du rapport",
                                   value = "Rapport d'analyse statistique"),
                  shiny::textInput(ns("rep_auteur"), "Auteur (facultatif)",
                                   placeholder = "Nom, laboratoire, service"),
                  shiny::radioButtons(ns("rep_format"), "Format",
                                      choices = HSTAT_REPORT_FORMATS,
                                      selected = "html"),
                  shiny::selectInput(ns("rep_dpi"), "Resolution des figures",
                                     choices = HSTAT_REPORT_DPI,
                                     selected = "1000"),
                  shiny::tags$small(style = "color:#7f8c8d;display:block;margin-top:-8px;",
                    shiny::icon("circle-info"),
                    " Les figures sont tracees pour l'impression, pas pour l'ecran : ",
                    "a 150 dpi elles paraissent nettes a l'affichage et sortent ",
                    "floues sur papier, et le defaut ne se voit qu'une fois le ",
                    "document remis. Comptez quelques secondes par figure."),
                  shiny::uiOutput(ns("rep_dispo"))),
                shiny::column(4,
                  shiny::checkboxGroupInput(ns("rep_sections"), "Sections a inclure",
                                            choices = HSTAT_REPORT_SECTIONS,
                                            selected = unname(HSTAT_REPORT_SECTIONS))),
                shiny::column(3,
                  shiny::uiOutput(ns("rep_resume")),
                  shiny::br(),
                  shiny::downloadButton(ns("dl_rapport"), "Produire le rapport",
                                        class = "btn-success btn-block"),
                  shiny::br(),
                  shiny::actionButton(ns("rep_apercu_go"), "Apercu",
                                      icon = shiny::icon("eye"),
                                      class = "btn-default btn-block btn-sm"))),
              shiny::hr(),
              shiny::div(style = "background:#ffffff;border:1px solid #e0e0e0;border-radius:6px;padding:16px 20px;max-height:620px;overflow:auto;",
                         shiny::uiOutput(ns("rep_apercu")))),
            shiny::tabPanel(shiny::tagList(shiny::icon("stethoscope"), " Qualite des donnees"),
              shiny::br(),
              shiny::uiOutput(ns("dq_resume")),
              DT::DTOutput(ns("dq_table")),
              shiny::br(),
              shiny::div(style = "background:#eaf4fb;border-left:5px solid #2e86c1;padding:12px 16px;border-radius:6px;font-size:13px;",
                shiny::icon("circle-info"),
                shiny::tags$b(" Comment lire ce diagnostic."),
                " Chaque constat porte sa gravite et une suggestion concrete. ",
                shiny::tags$b("Bloquant"), " : l'analyse echouera ou n'aura pas de sens en l'etat. ",
                shiny::tags$b("Important"), " : le resultat sera trompeur si rien n'est fait. ",
                shiny::tags$b("A surveiller"), " : a connaitre avant d'interpreter. ",
                "Le diagnostic est calcule dans R, sans modele et sans reseau ; ",
                "au-dela de 20 000 lignes il porte sur un echantillon."),
              shiny::br(),
              shiny::downloadButton(ns("dl_dq"), "Telecharger le diagnostic (CSV)",
                                    class = "btn-info btn-sm")),
            shiny::tabPanel(shiny::tagList(shiny::icon("table"), " Resultats captures"),
              shiny::br(),
              shiny::uiOutput(ns("ctx_tables")))
          ))
      )
    )
  )
}

mod_ai_server <- function(id, values) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(txt = NULL, source = NULL, err = NULL)

    ctx <- shiny::reactive(values$aiContext)

    get_data <- shiny::reactive({
      d <- values$data
      shiny::validate(shiny::need(!is.null(d) && nrow(d) > 0,
        "Chargez d'abord un jeu de donnees dans l'onglet Chargement."))
      as.data.frame(d)
    })

    output$ctx_box <- shiny::renderUI({
      c0 <- ctx()
      if (is.null(c0))
        return(shiny::div(class = "callout callout-warning", style = "padding:10px 14px;",
          shiny::icon("circle-info"),
          shiny::tags$b(" Aucune analyse enregistree."),
          shiny::br(),
          "Lancez une analyse dans l'un des onglets (tests, descriptives, ",
          "correlations, multivariees, qualitatives...), puis revenez ici. ",
          "Vous pouvez deja obtenir une recommandation en choisissant vos ",
          "variables ci-dessous."))
      shiny::div(class = "callout callout-success", style = "padding:10px 14px;",
        shiny::icon("circle-check"), shiny::tags$b(" ", c0$title), shiny::br(),
        shiny::tags$small(sprintf("Module : %s - %d tableau(x) - %s",
                                  c0$module, length(c0$tables),
                                  format(c0$time, "%H:%M:%S"))))
    })

    # Les selecteurs sont pre-remplis depuis l'analyse capturee : l'utilisateur
    # arrive sur un profil deja pertinent, qu'il reste libre de modifier.
    output$vars_ui <- shiny::renderUI({
      d <- get_data()
      pre <- intersect(ctx()$meta$variables %||% character(0), names(d))
      shiny::selectInput(ns("vars"), "Variables analysees", choices = names(d),
                         selected = if (length(pre)) pre else NULL, multiple = TRUE)
    })
    output$group_ui <- shiny::renderUI({
      d <- get_data()
      pre <- intersect(ctx()$meta$groupe %||% character(0), names(d))
      shiny::selectInput(ns("group"), "Variable de groupe (facteur)",
                         choices = c("(aucune)" = "", names(d)),
                         selected = if (length(pre)) pre[1] else "")
    })

    profile <- shiny::reactive({
      d <- get_data()
      shiny::req(length(input$vars) > 0)
      hstat_data_profile(d, input$vars, input$group, isTRUE(input$paired))
    })
    reco <- shiny::reactive(hstat_reco_analyses(profile()))
    verdict <- shiny::reactive({
      c0 <- ctx()
      if (is.null(c0)) return(NULL)
      hstat_reco_verdict(reco(), c0$title, c0$module)
    })

    # ---------------------------------------------------- moteur
    ai_models <- shiny::reactiveVal(character(0))
    shiny::observeEvent(input$backend, {
      shiny::updateTextInput(session, "url",
                             value = unname(HSTAT_AI_DEFAULT_URL[[input$backend]]))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$ping, {
      st <- hstat_ai_status("local", input$backend %||% "ollama", input$url)
      ai_models(if (is.null(st$models)) character(0) else st$models)
      shiny::showNotification(st$message,
                              type = if (isTRUE(st$ok)) "message" else "warning",
                              duration = 10)
    })
    output$model_ui <- shiny::renderUI({
      m <- ai_models()
      if (!length(m))
        return(shiny::textInput(ns("model"), "Modele",
                                placeholder = "ex. qwen2.5 - testez la connexion"))
      shiny::selectInput(ns("model"), "Modele", choices = m,
                         selected = shiny::isolate(input$model) %||% m[1])
    })
    output$status <- shiny::renderUI({
      st <- hstat_ai_status(input$engine %||% "local", input$backend %||% "ollama",
                            input$url, input$model, input$key)
      shiny::div(class = if (isTRUE(st$ok)) "callout callout-success" else "callout callout-warning",
                 style = "padding:8px 12px;font-size:12px;",
                 shiny::icon(if (isTRUE(st$ok)) "circle-check" else "triangle-exclamation"),
                 " ", st$message)
    })

    # ---------------------------------------------------- interpretation
    .offline <- function() {
      c0 <- ctx()
      if (is.null(c0)) {
        shiny::showNotification(
          "Aucune analyse enregistree : lancez d'abord une analyse dans un autre onglet.",
          type = "warning", duration = 7)
        return(invisible(NULL))
      }
      pr <- tryCatch(profile(), error = function(e) NULL)
      rv$txt <- hstat_ai_interpret_offline(c0, pr, tryCatch(reco(), error = function(e) NULL),
                                           tryCatch(verdict(), error = function(e) NULL),
                                           input$alpha %||% 0.05)
      rv$source <- "Lecture automatique (aucun modele, aucun reseau)"
      rv$err <- NULL
    }

    shiny::observeEvent(input$go_offline, .offline())

    shiny::observeEvent(input$go, {
      c0 <- ctx()
      if (is.null(c0)) {
        shiny::showNotification(
          "Aucune analyse enregistree : lancez d'abord une analyse dans un autre onglet.",
          type = "warning", duration = 7); return()
      }
      eng <- input$engine %||% "local"
      if (identical(eng, "auto")) {
        # Le moteur « sans modele » de l'assistance, c'est la lecture automatique.
        .offline(); return()
      }
      st <- hstat_ai_status(eng, input$backend %||% "ollama", input$url,
                            input$model, input$key)
      if (!isTRUE(st$ok)) {
        shiny::showNotification(paste0(st$message,
          " La lecture automatique, elle, reste disponible."),
          type = "error", duration = 12)
        return()
      }
      pr <- tryCatch(profile(), error = function(e) NULL)
      rc <- tryCatch(reco(), error = function(e) NULL)
      vd <- tryCatch(verdict(), error = function(e) NULL)

      shiny::withProgress(message = "Redaction de l'interpretation...", value = 0.4, {
        res <- hstat_ai_call(
          hstat_ai_interpret_prompt(c0, pr, rc, vd, input$niveau %||% "scientifique",
                                    input$contexte %||% "", input$alpha %||% 0.05),
          system = paste0("Tu es statisticien. Tu interpretes des resultats deja ",
                          "obtenus, sans jamais inventer de chiffre ni relancer ",
                          "d'analyse. Tu reponds en francais, en markdown."),
          engine = eng, backend = input$backend %||% "ollama", url = input$url,
          model = input$model, api_key = input$key, json = FALSE)
        shiny::incProgress(0.5)
        if (!isTRUE(res$ok)) {
          rv$err <- res$error
          shiny::showNotification(paste0(res$error,
            " Repli sur la lecture automatique."), type = "error", duration = 12)
          .offline(); return()
        }
        rv$txt <- res$text
        rv$source <- sprintf("%s%s", if (identical(eng, "claude")) "API Claude" else "Modele local",
                             if (!is.null(res$model)) sprintf(" (%s)", res$model) else "")
        rv$err <- NULL
      })
    })

    output$interp_note <- shiny::renderUI({
      if (is.null(rv$txt))
        return(shiny::div(class = "callout callout-info", style = "padding:10px 14px;",
          shiny::icon("circle-info"),
          " Lancez une analyse dans un autre onglet, puis demandez son interpretation ici."))
      shiny::div(class = "callout callout-success", style = "padding:8px 12px;font-size:12px;",
        shiny::icon("pen-nib"), shiny::tags$b(" Source : "), rv$source,
        shiny::br(),
        shiny::tags$small(
          "Texte a relire avant publication : l'assistance interprete, elle ne valide pas."))
    })

    output$interp <- shiny::renderUI({
      if (is.null(rv$txt))
        return(shiny::tags$em(style = "color:#95a5a6;", "Aucune interpretation pour l'instant."))
      shiny::HTML(.hstat_md_to_html(rv$txt))
    })

    output$dl_md <- shiny::downloadHandler(
      filename = function() sprintf("hstat_interpretation_%s.md", format(Sys.Date(), "%Y%m%d")),
      content = function(file) writeLines(rv$txt %||% "", file, useBytes = TRUE))
    output$dl_txt <- shiny::downloadHandler(
      filename = function() sprintf("hstat_interpretation_%s.txt", format(Sys.Date(), "%Y%m%d")),
      content = function(file)
        writeLines(gsub("[*#`]", "", rv$txt %||% ""), file, useBytes = TRUE))

    # ---------------------------------------------------- recommandation
    output$verdict <- shiny::renderUI({
      v <- verdict()
      if (is.null(v)) return(NULL)
      shiny::div(class = if (isTRUE(v$exploratoire)) "callout callout-info"
                        else if (isTRUE(v$coherent)) "callout callout-success"
                        else "callout callout-warning",
        style = "padding:10px 14px;",
        shiny::icon(if (isTRUE(v$exploratoire)) "compass"
                    else if (isTRUE(v$coherent)) "circle-check" else "circle-question"),
        " ", v$message)
    })

    output$reco_table <- DT::renderDT({
      r <- reco()
      shiny::validate(shiny::need(!is.null(r) && nrow(r) > 0,
        "Choisissez au moins une variable analysee, a gauche."))
      DT::datatable(r, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE, dom = "t"))
    })

    output$profile_table <- DT::renderDT({
      p <- profile()
      shiny::validate(shiny::need(!is.null(p), "Choisissez au moins une variable."))
      tb <- do.call(rbind, lapply(p$variables, function(e) data.frame(
        Variable = e$nom, Type = e$type, `n valide` = e$n, Manquants = e$n_manquant,
        Modalites = e$modalites,
        Normalite = if (is.null(e$normale)) "-"
                    else if (is.na(e$normale$ok)) "non evaluable"
                    else if (isTRUE(e$normale$ok)) "compatible" else "ecart a la normalite",
        `Test de normalite` = if (is.null(e$normale)) "-" else e$normale$methode,
        p = if (is.null(e$normale) || is.na(e$normale$p)) NA_real_
            else signif(e$normale$p, 4),
        check.names = FALSE, stringsAsFactors = FALSE)))
      DT::datatable(tb, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$profile_notes <- shiny::renderUI({
      p <- profile()
      if (is.null(p)) return(NULL)
      el <- list(shiny::tags$li(sprintf("%d observations dans le jeu de donnees.", p$n)))
      if (!is.null(p$groupe)) {
        g <- p$groupe
        el <- c(el, list(shiny::tags$li(sprintf(
          "Facteur « %s » : %d modalites (%s), effectifs %s.", g$nom, g$k,
          paste(g$modalites, collapse = ", "), paste(g$effectifs, collapse = " / ")))))
        if (isTRUE(g$petits_effectifs))
          el <- c(el, list(shiny::tags$li(shiny::tags$b("Au moins un groupe compte moins de 5 observations"),
            " : les approximations asymptotiques y sont peu fiables.")))
        if (!is.na(g$variances_homogenes))
          el <- c(el, list(shiny::tags$li(sprintf("Variances %s entre groupes.",
            if (isTRUE(g$variances_homogenes)) "homogenes" else "heterogenes"))))
        if (!is.na(g$equilibre) && !isTRUE(g$equilibre))
          el <- c(el, list(shiny::tags$li("Groupes desequilibres (rapport des effectifs > 1,5).")))
      }
      # Le detail par groupe justifie la recommandation : sans lui, on demande
      # a l'utilisateur de croire sur parole.
      for (nm in names(p$variables)) {
        e <- p$variables[[nm]]
        if (is.null(e$normale) || is.null(e$normale$detail)) next
        d <- e$normale$detail
        el <- c(el, list(shiny::tags$li(sprintf(
          "Normalite de « %s » testee DANS CHAQUE GROUPE : %s.", nm,
          paste(sprintf("%s (n=%d, p=%s)", d$Groupe, d$n,
                        ifelse(is.na(d$p), "n.d.", format(signif(d$p, 3)))),
                collapse = " ; ")))))
      }
      shiny::div(class = "callout callout-info", style = "padding:10px 14px;",
        shiny::tags$b(shiny::icon("list-check"), " Ce sur quoi repose la recommandation"),
        shiny::tags$ul(el))
    })

    # ---------------------------------------------------- journal de session
    rlog_texte <- shiny::reactive({
      hstat_rlog_script(values$aiHistory,
                        source = values$sourceKind,
                        version = hstat_version(),
                        donnees = {
                          o <- trimws(input$rlog_objet %||% "donnees")
                          if (nzchar(o)) o else "donnees"
                        })
    })

    output$rlog_script <- shiny::renderText(rlog_texte())

    output$rlog_resume <- shiny::renderUI({
      h <- values$aiHistory
      if (is.null(h) || !length(h))
        return(shiny::div(class = "callout callout-info", style = "padding:10px 14px;",
          shiny::icon("circle-info"),
          " Aucune analyse enregistree. Le journal se remplit a mesure que vous travaillez."))
      reconstitues <- sum(vapply(h, function(c0)
        !is.null(hstat_rlog_code(c0)), logical(1)))
      shiny::div(class = "callout callout-success", style = "padding:10px 14px;",
        shiny::icon("circle-check"),
        sprintf(" %d analyse(s) enregistree(s), dont %d avec leur code R.",
                length(h), reconstitues),
        if (reconstitues < length(h))
          shiny::tags$small(style = "display:block;color:#7f8c8d;",
            sprintf("Les %d autres sont documentees en commentaire : leurs reglages etaient interactifs.",
                    length(h) - reconstitues)))
    })

    output$rlog_table <- DT::renderDT({
      h <- values$aiHistory
      shiny::validate(shiny::need(!is.null(h) && length(h) > 0,
        "Le journal se remplit a mesure que vous menez des analyses."))
      DT::datatable(
        data.frame(
          `#` = seq_along(h),
          Heure = vapply(h, function(c0) format(c0$time, "%H:%M:%S"), character(1)),
          Module = vapply(h, function(c0) c0$module %||% "", character(1)),
          Analyse = vapply(h, function(c0) c0$title %||% "", character(1)),
          `Code R` = vapply(h, function(c0)
            if (is.null(hstat_rlog_code(c0))) "commentaire" else "reconstitue", character(1)),
          check.names = FALSE, stringsAsFactors = FALSE),
        rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, order = list()))
    })

    output$dl_rlog <- shiny::downloadHandler(
      filename = function() sprintf("hstat_session_%s.R", format(Sys.Date(), "%Y%m%d")),
      content = function(file) writeLines(rlog_texte(), file, useBytes = TRUE))

    data_quality <- shiny::reactive({
      d <- values$filteredData %||% values$cleanData %||% values$data
      shiny::validate(shiny::need(!is.null(d) && NROW(d) > 0,
        "Chargez d'abord un jeu de donnees dans l'onglet Chargement."))
      hstat_data_quality(as.data.frame(d))
    })

    output$dq_resume <- shiny::renderUI({
      dq <- tryCatch(data_quality(), error = function(e) NULL)
      if (is.null(dq)) return(NULL)
      msg <- hstat_data_quality_resume(dq)
      grave <- sum(dq$Gravite == "bloquant")
      shiny::div(class = if (grave > 0) "callout callout-danger"
                        else if (any(dq$Gravite == "important")) "callout callout-warning"
                        else "callout callout-success",
        style = "padding:10px 14px;",
        shiny::icon(if (grave > 0) "triangle-exclamation" else "circle-check"),
        " ", msg)
    })

    output$dq_table <- DT::renderDT({
      dq <- data_quality()
      shiny::validate(shiny::need(!is.null(dq) && nrow(dq) > 0, "Aucun constat."))
      DT::datatable(dq, rownames = FALSE, filter = "top",
                    options = list(pageLength = 15, scrollX = TRUE)) |>
        DT::formatStyle("Gravite", target = "row",
          backgroundColor = DT::styleEqual(HSTAT_QUALITE_GRAVITES,
                                           c("#fdecea", "#fef5e7", "#f4f6f7")))
    })

    output$dl_dq <- shiny::downloadHandler(
      filename = function() sprintf("hstat_qualite_donnees_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file)
        utils::write.csv(tryCatch(data_quality(), error = function(e) NULL),
                         file, row.names = FALSE, fileEncoding = "UTF-8"))

    # ---------------------------------------------------- rapport automatique
    rep_dispo <- shiny::reactive(hstat_report_formats_dispo())

    output$rep_dispo <- shiny::renderUI({
      msg <- hstat_report_message_dispo(rep_dispo())
      if (is.null(msg))
        return(shiny::div(class = "callout callout-success", style = "padding:8px 12px;font-size:12px;",
          shiny::icon("circle-check"), " Les trois formats sont disponibles sur cette machine."))
      shiny::div(class = "callout callout-warning", style = "padding:8px 12px;font-size:12px;",
                 shiny::icon("triangle-exclamation"), " ", msg)
    })

    # Le format REELLEMENT produit : demander du PDF sans LaTeX donne du HTML.
    # Le calculer ici plutot que dans le seul `content` permet de nommer le
    # fichier correctement — un .pdf contenant du HTML ne s'ouvrirait pas.
    rep_format <- shiny::reactive({
      f <- input$rep_format %||% "html"
      if (isTRUE(rep_dispo()[[f]])) f else "html"
    })

    # `apercu = TRUE` degrade volontairement la resolution : l'apercu sert a
    # verifier la mise en page, incorporer 9000 px en base64 dans un onglet
    # n'ajouterait rien de visible et le rendrait poussif. Le document
    # telecharge, lui, passe toujours par le plancher d'impression.
    rep_figures <- function(apercu = FALSE) {
      d <- file.path(tempdir(),
                     paste0("hstat_fig_", session$token, if (apercu) "_ap" else ""))
      unlink(d, recursive = TRUE)
      tryCatch(
        hstat_report_figures(
          values$aiHistory, dossier = d, apercu = apercu,
          dpi = suppressWarnings(as.numeric(input$rep_dpi %||% HSTAT_REPORT_DPI_MIN)),
          progres = if (apercu) NULL else function(i, n, titre)
            shiny::setProgress(value = i / max(n, 1),
                               detail = sprintf("Figure %d sur %d — %s", i, n,
                                                substr(titre, 1, 60)))),
        error = function(e) NULL)
    }

    rep_markdown <- function(figures = NULL) {
      d <- values$filteredData %||% values$cleanData %||% values$data
      sections <- input$rep_sections %||% unname(HSTAT_REPORT_SECTIONS)
      hstat_report_markdown(
        history        = values$aiHistory,
        titre          = if (nzchar(trimws(input$rep_titre %||% "")))
                           trimws(input$rep_titre) else "Rapport d'analyse",
        auteur         = trimws(input$rep_auteur %||% ""),
        contexte       = trimws(input$contexte %||% ""),
        sections       = sections,
        donnees_resume = if ("donnees" %in% sections)
                           tryCatch(hstat_report_resume_donnees(d), error = function(e) NULL),
        qualite        = if ("qualite" %in% sections)
                           tryCatch(data_quality(), error = function(e) NULL),
        interpretation = rv$txt,
        reco           = if ("reco" %in% sections)
                           tryCatch(reco(), error = function(e) NULL),
        script         = if ("script" %in% sections)
                           tryCatch(rlog_texte(), error = function(e) NULL),
        version        = hstat_version(),
        figures        = figures)
    }

    output$rep_resume <- shiny::renderUI({
      h <- values$aiHistory %||% list()
      nfig <- sum(vapply(h, function(x) is.function(x$plot), logical(1)))
      shiny::div(style = "font-size:12px;color:#566573;line-height:1.7;",
        shiny::icon("list-check"), shiny::tags$b(" Le rapport contiendra"), shiny::br(),
        sprintf("%d analyse(s)", length(h)), shiny::br(),
        sprintf("%d figure(s) disponible(s)", nfig), shiny::br(),
        if (!is.null(rv$txt) && nzchar(rv$txt)) "une interpretation redigee"
        else shiny::tags$span(style = "color:#b9770e;",
          "aucune interpretation (lancez-en une dans l'onglet Interpretation)"))
    })

    rep_apercu <- shiny::eventReactive(input$rep_apercu_go, {
      figs <- if ("figures" %in% (input$rep_sections %||% character(0)))
        rep_figures(apercu = TRUE) else NULL
      .hstat_rep_images_html(.hstat_rep_md_to_html(rep_markdown(figs)))
    })

    output$rep_apercu <- shiny::renderUI({
      if (is.null(input$rep_apercu_go) || input$rep_apercu_go == 0)
        return(shiny::tags$em(style = "color:#95a5a6;",
          "Cliquez sur « Apercu » pour voir le rapport avant de le produire."))
      shiny::HTML(rep_apercu())
    })

    output$dl_rapport <- shiny::downloadHandler(
      filename = function()
        sprintf("hstat_rapport_%s.%s", format(Sys.Date(), "%Y%m%d"), rep_format()),
      content = function(file) {
        # Le trace a 1000 dpi prend quelques secondes par figure : sans
        # progression, l'utilisateur ne saurait pas si son clic a porte.
        figs <- if ("figures" %in% (input$rep_sections %||% character(0)))
          shiny::withProgress(
            message = "Trace des figures pour l'impression...",
            value = 0, rep_figures()) else NULL
        res <- shiny::withProgress(
          message = "Assemblage du document...", value = 0.9,
          hstat_report_render(rep_markdown(figs), file,
                              format = rep_format(),
                              titre = trimws(input$rep_titre %||% "Rapport d'analyse"),
                              dispo = rep_dispo()))
        # Le repli doit se DIRE : un utilisateur qui a demande du Word et
        # recoit du HTML sans explication croit a un bug.
        if (nzchar(res$message %||% ""))
          shiny::showNotification(res$message, type = "warning", duration = 12)
      })

    output$ctx_tables <- shiny::renderUI({
      c0 <- ctx()
      if (is.null(c0)) return(shiny::tags$em(style = "color:#95a5a6;",
        "Aucune analyse enregistree."))
      shiny::tagList(
        lapply(names(c0$tables), function(nm) shiny::tagList(
          shiny::h5(shiny::icon("table"), " ", nm),
          # Table statique construite a la main : une fonction de rendu Shiny
          # attend la session et le nom de sortie, elle ne s'appelle pas
          # directement depuis un renderUI.
          .hstat_html_table(utils::head(as.data.frame(c0$tables[[nm]]), 30)),
          shiny::br())),
        if (!is.null(c0$text) && nzchar(c0$text))
          shiny::tagList(shiny::h5(shiny::icon("terminal"), " Sortie R"),
                         shiny::tags$pre(style = "max-height:320px;overflow:auto;", c0$text)))
    })
  })
}
