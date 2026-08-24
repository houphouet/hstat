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
# 10. ASSISTANT DE CODAGE - UNE TABLE DE FOURNISSEURS, TROIS PROTOCOLES
# ---------------------------------------------------------------------------
# L'assistance doit pouvoir tourner GRATUITEMENT et SANS CONNEXION : c'est la
# raison d'etre du moteur "auto", et c'est pourquoi il reste le defaut. Les
# services en ligne sont ensuite proposes sur un pied d'egalite, Claude n'ayant
# aucun privilege.
#
#   "auto"   Thematisation statistique, sans aucun modele de langue. Tourne
#            dans le processus R, instantanement, sans rien installer et sans
#            reseau. Seul moteur garanti disponible partout, donc seul defaut
#            acceptable : une fonctionnalite facturee a l'usage ne doit jamais
#            devenir le chemin par defaut d'un utilisateur qui n'a rien demande.
#
#   "local"  Serveur d'inference sur la machine de l'utilisateur (llama.cpp,
#            LM Studio, vLLM, Jan...), parlant le protocole d'OpenAI. Gratuit,
#            hors ligne, aucune donnee ne quitte la machine.
#
#   les API  Claude, ChatGPT, DeepSeek, Gemini, GitHub Models (Copilot), Kimi.
#            Payantes, en ligne, et jamais choisies d'office.
#
# TOUT le reste du code derive de HSTAT_AI_FOURNISSEURS : la liste de choix,
# le diagnostic, l'aiguillage, l'interface. Ajouter un service, c'est ajouter
# une ligne -- et un service qui parle le protocole d'OpenAI (la plupart) ne
# demande aucun code.
#
# Le support d'Ollama a ete retire. Son protocole lui etait propre (/api/chat,
# /api/tags, `format: "json"`), il portait son propre constructeur de corps de
# requete et son propre lecteur de reponse ; un serveur local compatible OpenAI
# rend le meme service par le chemin commun.
#
# R n'ayant de SDK pour aucun de ces services, tout passe par {httr} +
# {jsonlite}, gardes en Suggests : sans eux, le moteur "auto" reste pleinement
# fonctionnel.
#
# Les adresses et les modeles sont des VALEURS PAR DEFAUT, modifiables dans
# l'interface : un service qui change d'adresse ou de modele ne doit pas
# obliger a rouvrir le code.
# ---------------------------------------------------------------------------

HSTAT_AI_FOURNISSEURS <- list(
  auto = list(
    label = "Thématisation automatique (statistique, sans modèle)",
    protocole = "auto", cle_env = "", url = "", modele = "", paye = FALSE),
  local = list(
    label = "Serveur local compatible OpenAI (llama.cpp, LM Studio, vLLM...)",
    protocole = "openai", cle_env = "", url = "http://127.0.0.1:8080",
    modele = "", paye = FALSE,
    aide = "Démarrez votre serveur d'inférence et vérifiez son adresse ci-dessous."),
  claude = list(
    label = "API Claude (Anthropic)", protocole = "anthropic",
    cle_env = "ANTHROPIC_API_KEY", url = "https://api.anthropic.com",
    modele = "claude-opus-5", paye = TRUE, cle_url = "console.anthropic.com"),
  chatgpt = list(
    label = "API ChatGPT (OpenAI)", protocole = "openai",
    cle_env = "OPENAI_API_KEY", url = "https://api.openai.com/v1",
    modele = "gpt-4o", paye = TRUE, cle_url = "platform.openai.com/api-keys"),
  deepseek = list(
    label = "API DeepSeek", protocole = "openai",
    cle_env = "DEEPSEEK_API_KEY", url = "https://api.deepseek.com/v1",
    modele = "deepseek-chat", paye = TRUE, cle_url = "platform.deepseek.com"),
  gemini = list(
    label = "API Gemini (Google)", protocole = "gemini",
    cle_env = "GEMINI_API_KEY",
    url = "https://generativelanguage.googleapis.com/v1beta",
    modele = "gemini-2.0-flash", paye = TRUE, cle_url = "aistudio.google.com/apikey"),
  copilot = list(
    # Variable DEDIEE, et surtout pas GITHUB_TOKEN : celle-ci est presente sur
    # quantite de postes et dans toutes les integrations continues. La lire
    # d'office enverrait un jeton ambiant chez un tiers sans que l'utilisateur
    # l'ait voulu -- constate ici meme, ou le moteur s'annoncait « disponible »
    # avec le jeton du conteneur. Une cle ne doit servir qu'a ce qu'on a
    # explicitement demande.
    label = "GitHub Models (Copilot)", protocole = "openai",
    cle_env = "GITHUB_MODELS_TOKEN", url = "https://models.github.ai/inference",
    modele = "openai/gpt-4o", paye = TRUE, cle_url = "github.com/settings/tokens"),
  kimi = list(
    label = "API Kimi (Moonshot)", protocole = "openai",
    cle_env = "MOONSHOT_API_KEY", url = "https://api.moonshot.ai/v1",
    modele = "moonshot-v1-8k", paye = TRUE, cle_url = "platform.moonshot.ai"))

# Liste de choix, dans l'ordre de la table : le gratuit d'abord, le paye
# ensuite. Un test garde cet ordre.
HSTAT_AI_ENGINES <- stats::setNames(
  names(HSTAT_AI_FOURNISSEURS),
  vapply(HSTAT_AI_FOURNISSEURS, function(f) f$label, character(1)))

# Fournisseur d'un moteur. Un identifiant inconnu retombe sur "auto", qui
# fonctionne toujours -- jamais sur une API payante.
hstat_ai_fournisseur <- function(engine = "auto") {
  id <- if (is.null(engine)) "" else as.character(engine)[1]
  f <- HSTAT_AI_FOURNISSEURS[[id]]
  if (is.null(f)) HSTAT_AI_FOURNISSEURS[["auto"]] else f
}

HSTAT_AI_MODEL <- HSTAT_AI_FOURNISSEURS$claude$modele   # retro-compatibilite

# Cle d'API : celle saisie dans l'interface, sinon la variable d'environnement
# PROPRE AU SERVICE. Une cle OpenAI ne doit pas servir a appeler DeepSeek.
hstat_ai_key <- function(engine = "claude", explicit = NULL) {
  k <- if (is.null(explicit)) "" else trimws(as.character(explicit)[1])
  if (is.na(k) || !nzchar(k)) {
    env <- hstat_ai_fournisseur(engine)$cle_env
    if (nzchar(env)) k <- trimws(Sys.getenv(env, ""))
  }
  if (is.na(k)) "" else k
}

# Adresse de base : celle saisie, sinon celle du fournisseur.
hstat_ai_url <- function(engine = "local", url = NULL) {
  u <- if (is.null(url)) "" else trimws(as.character(url)[1])
  if (is.na(u) || !nzchar(u)) u <- hstat_ai_fournisseur(engine)$url
  sub("/+$", "", u)
}

# Modele : celui saisi, sinon celui du fournisseur.
hstat_ai_modele <- function(engine = "local", model = NULL) {
  m <- if (is.null(model)) "" else trimws(as.character(model)[1])
  if (is.na(m) || !nzchar(m)) m <- hstat_ai_fournisseur(engine)$modele
  if (is.na(m)) "" else m
}

.hstat_ai_http_ok <- function() {
  requireNamespace("httr", quietly = TRUE) &&
    requireNamespace("jsonlite", quietly = TRUE)
}

# Modeles annonces par un service parlant le protocole d'OpenAI (GET /models).
# Vecteur vide si le serveur n'est pas joignable : l'interface saura alors dire
# quoi faire plutot que d'offrir une liste vide sans explication.
hstat_ai_openai_models <- function(url = NULL, api_key = NULL, timeout = 5,
                                   engine = "local") {
  if (!.hstat_ai_http_ok()) return(character(0))
  hdr <- if (!is.null(api_key) && nzchar(api_key))
    httr::add_headers(Authorization = paste("Bearer", api_key)) else NULL
  args <- list(paste0(hstat_ai_url(engine, url), "/models"), httr::timeout(timeout))
  if (!is.null(hdr)) args <- append(args, list(hdr), after = 1)
  res <- tryCatch(do.call(httr::GET, args), error = function(e) NULL)
  if (is.null(res) || httr::status_code(res) >= 300) return(character(0))
  p <- tryCatch(jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                                   simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(p$data)) return(character(0))
  nm <- vapply(p$data, function(m) if (is.null(m$id)) "" else as.character(m$id)[1],
               character(1))
  sort(nm[nzchar(nm)])
}

hstat_ai_models <- function(engine = "local", url = NULL, api_key = NULL,
                            timeout = 5) {
  if (!identical(hstat_ai_fournisseur(engine)$protocole, "openai"))
    return(character(0))
  hstat_ai_openai_models(url, api_key, timeout, engine)
}

# Diagnostic lisible, propre a chaque moteur. Toujours actionnable : on dit ce
# qui manque ET comment y remedier, plutot qu'un simple « indisponible ».
hstat_ai_status <- function(engine = "auto", url = NULL, model = NULL,
                            api_key = NULL) {
  f <- hstat_ai_fournisseur(engine)

  if (identical(f$protocole, "auto"))
    return(list(ok = TRUE,
                message = paste0("Thématisation automatique disponible : elle tourne ",
                                 "dans R, sans modèle, sans installation et sans réseau.")))

  if (!.hstat_ai_http_ok()) {
    miss <- c(if (!requireNamespace("httr", quietly = TRUE)) "{httr}",
              if (!requireNamespace("jsonlite", quietly = TRUE)) "{jsonlite}")
    return(list(ok = FALSE,
                message = trf(
                  "Moteur indisponible : le(s) paquet(s) %s manquent. Installez-les avec install.packages(c(%s)), ou basculez sur la thématisation automatique, qui n'en a pas besoin.",
                  paste(miss, collapse = " et "),
                  paste(sprintf('"%s"', gsub("[{}]", "", miss)), collapse = ", "))))
  }

  # Un service en ligne exige une cle. On NOMME le service, sa variable
  # d'environnement et l'endroit ou l'obtenir : « cle absente » tout court
  # laisse l'utilisateur sans geste a faire.
  if (nzchar(f$cle_env)) {
    if (!nzchar(hstat_ai_key(engine, api_key)))
      return(list(ok = FALSE,
                  message = trf("%s indisponible : renseignez une clé d'API (champ ci-dessous ou variable %s). Clé à créer sur %s. Ce service est payant et nécessite une connexion ; la thématisation automatique, elle, est gratuite et hors ligne.",
                                f$label, f$cle_env, f$cle_url %||% "le site du fournisseur")))
    return(list(ok = TRUE,
                message = trf("%s disponible (modèle %s).", f$label,
                              hstat_ai_modele(engine, model))))
  }

  # Serveur local : pas de cle, mais il faut qu'il reponde.
  u <- hstat_ai_url(engine, url)
  mods <- hstat_ai_models(engine, u)
  if (!length(mods))
    return(list(ok = FALSE, models = character(0),
                message = trf("Aucun modèle joignable sur %s. %s", u,
                              f$aide %||% "Vérifiez l'adresse ci-dessous.")))
  m <- hstat_ai_modele(engine, model)
  if (nzchar(m) && !(m %in% mods))
    return(list(ok = FALSE, models = mods,
                message = trf("Le modèle « %s » n'est pas disponible sur %s. Modèles annonces : %s.",
                                  m, u, paste(mods, collapse = ", "))))
  list(ok = TRUE, models = mods,
       message = trf("Serveur local disponible sur %s (%d modèle(s)). Gratuit, hors ligne, aucune donnée ne quitte la machine.",
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

.hstat_ai_body_openai <- function(prompt, system = NULL, model = "",
                                  max_tokens = 4096L, json = TRUE) {
  body <- list(model = model,
               messages = .hstat_ai_messages(prompt, system),
               stream = FALSE, temperature = 0.2,
               max_tokens = as.integer(max_tokens))
  if (isTRUE(json)) body$response_format <- list(type = "json_object")
  body
}

.hstat_ai_call_openai <- function(prompt, system = NULL, url = NULL, model = NULL,
                                  api_key = NULL, max_tokens = 4096L,
                                  json = TRUE, timeout = 600, engine = "local") {
  f <- hstat_ai_fournisseur(engine)
  u <- hstat_ai_url(engine, url)
  model <- hstat_ai_modele(engine, model)
  if (!nzchar(model)) {
    mods <- hstat_ai_openai_models(u, api_key, engine = engine)
    model <- if (length(mods)) mods[1] else "local-model"
  }
  mk <- function(with_json)
    .hstat_ai_body_openai(prompt, system, model, max_tokens, with_json)
  hdr <- if (!is.null(api_key) && nzchar(api_key))
    c(Authorization = paste("Bearer", api_key)) else NULL

  # L'adresse porte deja le prefixe de version quand le service en a un
  # (/v1 chez OpenAI, DeepSeek, Kimi ; rien chez GitHub Models). Le chemin
  # ajoute ici est donc le seul point commun : /chat/completions.
  send <- function(with_json)
    .hstat_ai_post(paste0(u, "/chat/completions"), mk(with_json), hdr, timeout)

  res <- send(isTRUE(json))
  # Tous les serveurs n'acceptent pas response_format : une requete rejetee
  # pour ce seul motif est rejouee sans lui plutot que d'echouer.
  if (!inherits(res, "error") && isTRUE(json) && httr::status_code(res) == 400)
    res <- send(FALSE)

  if (inherits(res, "error"))
    return(list(ok = FALSE, text = "",
                error = trf("%s injoignable sur %s (%s).", f$label, u,
                            conditionMessage(res))))
  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  p <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (httr::status_code(res) >= 300) {
    msg <- if (!is.null(p$error$message)) p$error$message else raw
    return(list(ok = FALSE, text = "",
                error = trf("Erreur de %s (HTTP %d) : %s", f$label,
                                httr::status_code(res), substr(msg, 1, 400))))
  }
  txt <- tryCatch(p$choices[[1]]$message$content, error = function(e) NULL)
  if (is.null(txt))
    return(list(ok = FALSE, text = "",
                error = trf("Réponse illisible de %s.", f$label)))
  list(ok = TRUE, text = as.character(txt)[1], error = NULL, model = model)
}

# --- Gemini : le seul service qui ne parle ni OpenAI ni Anthropic ------------
# Le corps est construit a part, comme les deux autres : c'est la piece qui
# casse en silence quand un champ est renomme, elle doit rester testable sans
# serveur.
.hstat_ai_body_gemini <- function(prompt, system = NULL, json = TRUE) {
  body <- list(
    contents = list(list(role = "user", parts = list(list(text = prompt)))),
    generationConfig = list(temperature = 0.2))
  if (!is.null(system) && nzchar(system))
    body$systemInstruction <- list(parts = list(list(text = system)))
  if (isTRUE(json)) body$generationConfig$responseMimeType <- "application/json"
  body
}

.hstat_ai_call_gemini <- function(prompt, system = NULL, url = NULL, model = NULL,
                                  api_key = NULL, json = TRUE, timeout = 300) {
  f <- hstat_ai_fournisseur("gemini")
  key <- hstat_ai_key("gemini", api_key)
  if (!nzchar(key)) return(list(ok = FALSE, text = "", error = "Clé d'API absente."))
  u <- hstat_ai_url("gemini", url)
  model <- hstat_ai_modele("gemini", model)

  res <- .hstat_ai_post(sprintf("%s/models/%s:generateContent", u, model),
                        .hstat_ai_body_gemini(prompt, system, json),
                        c(`x-goog-api-key` = key), timeout)
  if (inherits(res, "error"))
    return(list(ok = FALSE, text = "",
                error = trf("%s injoignable (%s).", f$label, conditionMessage(res))))
  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  p <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (httr::status_code(res) >= 300) {
    msg <- if (!is.null(p$error$message)) p$error$message else raw
    return(list(ok = FALSE, text = "",
                error = trf("Erreur de %s (HTTP %d) : %s", f$label,
                                httr::status_code(res), substr(msg, 1, 400))))
  }
  # Une reponse Gemini porte une liste de candidats, chacun une liste de parts.
  txt <- tryCatch(
    paste(vapply(p$candidates[[1]]$content$parts,
                 function(x) if (is.null(x$text)) "" else as.character(x$text)[1],
                 character(1)), collapse = ""),
    error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt))
    return(list(ok = FALSE, text = "",
                error = trf("Réponse illisible de %s.", f$label)))
  list(ok = TRUE, text = txt, error = NULL, model = model)
}

.hstat_ai_call_claude <- function(prompt, system = NULL, api_key = NULL,
                                  model = NULL, max_tokens = 8000L,
                                  thinking = TRUE, timeout = 300, url = NULL) {
  key <- hstat_ai_key("claude", api_key)
  if (!nzchar(key)) return(list(ok = FALSE, text = "", error = "Clé d'API absente."))
  model <- hstat_ai_modele("claude", model)

  body <- list(model = model, max_tokens = as.integer(max_tokens),
               messages = list(list(role = "user", content = prompt)))
  if (!is.null(system) && nzchar(system)) body$system <- system
  # La reflexion adaptative est le mode par defaut des modeles Claude recents ;
  # `budget_tokens` y est rejete, on ne l'envoie donc pas.
  if (isTRUE(thinking)) body$thinking <- list(type = "adaptive")

  res <- .hstat_ai_post(paste0(hstat_ai_url("claude", url), "/v1/messages"), body,
                        c(`x-api-key` = key, `anthropic-version` = "2023-06-01"),
                        timeout)
  if (inherits(res, "error"))
    return(list(ok = FALSE, text = "",
                error = paste("Échec de la connexion :", conditionMessage(res))))

  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (httr::status_code(res) >= 300) {
    msg <- if (!is.null(parsed$error$message)) parsed$error$message else raw
    return(list(ok = FALSE, text = "",
                error = trf("Erreur API (HTTP %d) : %s",
                                httr::status_code(res), substr(msg, 1, 500))))
  }
  if (is.null(parsed) || is.null(parsed$content))
    return(list(ok = FALSE, text = "", error = "Réponse illisible de l'API."))

  # Le contenu est une liste de blocs ; avec la reflexion adaptative, les blocs
  # `thinking` precedent le bloc `text` : on ne garde que le texte.
  txt <- vapply(parsed$content, function(b)
    if (identical(b$type, "text") && !is.null(b$text)) b$text else "", character(1))
  list(ok = TRUE, text = paste(txt[nzchar(txt)], collapse = "\n"), error = NULL,
       model = model)
}

# Reglages du moteur choisi, construits a partir de la table : l'interface n'a
# plus a connaitre les services un par un. Sept `conditionalPanel` en dur
# devenaient faux des qu'on ajoutait une ligne a la table -- ce qui est
# precisement ce qu'on veut pouvoir faire.
#
# Les identifiants sont prefixes parce que les deux onglets qui s'en servent
# nomment leurs champs differemment (`url` ici, `ai_url` dans l'atelier de
# codage) : le prefixe evite d'avoir a renommer les champs existants.
hstat_ai_reglages_ui <- function(ns, engine, prefixe = "") {
  f  <- hstat_ai_fournisseur(engine)
  id <- function(x) ns(paste0(prefixe, x))
  if (identical(f$protocole, "auto"))
    return(shiny::div(
      style = "background:#eafaf1;border-left:3px solid #27ae60;padding:8px 12px;font-size:12px;",
      shiny::icon("circle-check"),
      " Aucun réglage : la thématisation tourne dans R, sans modèle, sans clé et sans réseau."))

  shiny::tagList(
    if (nzchar(f$cle_env))
      shiny::tagList(
        shiny::passwordInput(id("key"), trf("Clé d'API - %s", f$label),
                             placeholder = trf("laisser vide pour utiliser %s", f$cle_env)),
        shiny::tags$small(style = "color:#7f8c8d;display:block;margin-top:-8px;",
          shiny::icon("key"), " ",
          trf("Clé à créer sur %s. Service payant, en ligne.",
              f$cle_url %||% "le site du fournisseur"))),
    shiny::textInput(id("url"), "Adresse du service", value = f$url),
    shiny::textInput(id("model"), "Modèle",
                     value = f$modele,
                     placeholder = paste("ex.", .hstat_ai_ex(f))),
    shiny::actionButton(id("ping"), "Tester la connexion",
                        icon = shiny::icon("plug-circle-check"),
                        class = "btn-default btn-sm btn-block"))
}

# Exemple de modele affiche en filigrane, quand le fournisseur n'en impose pas.
.hstat_ai_ex <- function(f) if (nzchar(f$modele)) f$modele else "le nom du modèle servi"

# Aiguillage sur le PROTOCOLE, pas sur le nom du service : ajouter un service
# qui parle celui d'OpenAI ne demande alors aucune ligne ici.
#
# `engine = "auto"` par defaut. C'est le seul moteur garanti disponible, et
# surtout le seul gratuit : une API facturee a l'usage ne doit jamais devenir
# le chemin par defaut d'un utilisateur qui n'a rien demande. Un test le garde.
hstat_ai_call <- function(prompt, system = NULL, engine = "auto",
                          url = NULL, model = NULL,
                          api_key = NULL, max_tokens = 8000L, json = TRUE,
                          timeout = NULL) {
  f <- hstat_ai_fournisseur(engine)
  if (identical(f$protocole, "auto"))
    return(list(ok = FALSE, text = "",
                error = paste0("La thématisation automatique ne passe pas par un ",
                               "modèle de langue : elle est calculée directement ",
                               "dans R. Choisissez un service en ligne ou un ",
                               "serveur local pour faire rédiger un texte.")))
  if (!.hstat_ai_http_ok())
    return(list(ok = FALSE, text = "",
                error = "Les paquets {httr} et {jsonlite} sont requis pour ce moteur."))

  if (identical(f$protocole, "anthropic"))
    return(.hstat_ai_call_claude(prompt, system, api_key, model, max_tokens,
                                 TRUE, timeout %||% 300, url))
  if (identical(f$protocole, "gemini"))
    return(.hstat_ai_call_gemini(prompt, system, url, model, api_key, json,
                                 timeout %||% 300))
  # Un modele qui tourne sur le processeur de l'utilisateur est bien plus lent
  # qu'une API distante : le delai d'attente par defaut en tient compte.
  tmo <- timeout %||% (if (nzchar(f$cle_env)) 300 else 900)
  .hstat_ai_call_openai(prompt, system, url, model,
                        hstat_ai_key(engine, api_key), max_tokens, json, tmo,
                        engine = engine)
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
  out <- c(trf("ANALYSE RÉALISÉE : %s", ctx$title))
  if (length(ctx$meta)) {
    m <- vapply(names(ctx$meta), function(k) {
      v <- ctx$meta[[k]]
      if (is.null(v) || !length(v)) return("")
      sprintf("- %s : %s", k, paste(utils::head(as.character(v), 12), collapse = ", "))
    }, character(1))
    m <- m[nzchar(m)]
    if (length(m)) out <- c(out, "", "PARAMÈTRES :", m)
  }
  for (nm in names(ctx$tables)) {
    tb <- ctx$tables[[nm]]
    if (is.null(tb) || !NROW(tb)) next
    trunc <- NROW(tb) > max_rows
    tb <- utils::head(as.data.frame(tb), max_rows)
    out <- c(out, "", trf("TABLEAU - %s :", nm),
             paste(utils::capture.output(print(tb, row.names = FALSE)), collapse = "\n"),
             if (trunc) trf("(... %d lignes au total)", NROW(ctx$tables[[nm]])) else NULL)
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
       methode = trf("asymétrie %.2f / aplatissement %.2f (n > 5000)", skew, kurt),
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
  if (!length(lev)) return(list(ok = NA, methode = "aucune observation complète",
                                p = NA_real_, portee = "par groupe"))
  det <- do.call(rbind, lapply(lev, function(l) {
    r <- .hstat_reco_normal(x[g == l])
    data.frame(Groupe = l, n = sum(g == l), p = r$p, Normale = r$ok,
               stringsAsFactors = FALSE)
  }))
  testables <- !is.na(det$Normale)
  list(ok = if (!any(testables)) NA else all(det$Normale[testables]),
       methode = trf("Shapiro-Wilk dans chacun des %d groupes", length(lev)),
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

# Un NOMBRE dans du code R engendre : quinze chiffres significatifs, jamais la
# mise en forme de l'affichage. `format()` respecte `options(digits)` et
# `OutDec` : une locale francaise ecrirait « 0,00063 », que R refuserait
# d'analyser -- le journal a precisement pour promesse d'etre executable.
.hstat_rlog_num <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  # `trimws` : formatC aligne sur une largeur commune, ce qui semait le code
  # engendre de colonnes d'espaces.
  if (!length(x) || !is.finite(x)) "NA"
  else trimws(formatC(x, digits = 15, format = "g"))
}

.hstat_rlog_vec_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x)) return("numeric(0)")
  paste0("c(", paste(vapply(x, .hstat_rlog_num, character(1)), collapse = ", "), ")")
}

# Code R correspondant a UNE analyse capturee. NULL si rien de fidele n'est
# reconstituable — l'appelant ecrira alors un commentaire.
hstat_rlog_code <- function(ctx, donnees = "donnees") {
  if (is.null(ctx)) return(NULL)
  v <- intersect(ctx$meta$variables %||% character(0), ctx$meta$variables %||% character(0))
  v <- as.character(v[nzchar(v)])
  g <- as.character((ctx$meta$groupe %||% character(0)))
  g <- g[nzchar(g)]
  t <- tolower(hstat_sans_accents(ctx$title %||% ""))
  y <- if (length(v)) .hstat_rlog_nom(v[1]) else NULL
  f <- if (length(g)) .hstat_rlog_nom(g[1]) else NULL
  fml <- if (!is.null(y) && !is.null(f)) sprintf("%s ~ %s", y, f) else NULL

  switch(ctx$module %||% "",

    "Exploration" = c(sprintf("str(%s)", donnees), sprintf("summary(%s)", donnees)),

    "Analyses descriptives" = if (length(v))
      c(sprintf("summary(%s[, %s, drop = FALSE])", donnees, .hstat_rlog_vec(v)),
        if (!is.null(fml)) sprintf("aggregate(%s, data = %s, FUN = mean)", fml, donnees)),

    "Corrélations" = if (length(v) >= 2)
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

    "Analyses multivariées" = {
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

    # LE BIOESSAI EST LA SEULE ANALYSE DONT LES DONNEES NE VIENNENT PAS DU
    # FICHIER : elles sont saisies dans le module, ou lues d'un fichier WIN DL.
    # Le script les REPORTE donc, ce qui le rend autonome -- il s'execute sans
    # `mon_fichier.csv`, et il est exactement reproductible.
    #
    # Et il n'est ecrit QUE quand il est fidele. A mortalite naturelle declaree
    # nulle, le modele de Finney EST un GLM binomial a lien probit sur le
    # log10 de la dose : les deux ajustements coincident a 1e-7, c'est verifie
    # par un test. Des que `c` est estimee (EM) ou fixee par Abbott, ce n'est
    # plus un GLM -- `glm()` ne sait pas ajuster c dans
    # p = c + (1 - c).F(a + b.log d) -- et un `glm()` ecrit la quand meme
    # rendrait un script plausible qui ne refait PAS ce que l'application a
    # calcule. On ecrit alors le commentaire, et les parametres obtenus.
    "DL50 / CL50" = {
      m <- ctx$meta %||% list()
      dz <- suppressWarnings(as.numeric(m$doses %||% numeric(0)))
      dn <- suppressWarnings(as.numeric(m$effectifs %||% numeric(0)))
      dx <- suppressWarnings(as.numeric(m$morts %||% numeric(0)))
      ok <- length(dz) >= 3L && length(dn) == length(dz) && length(dx) == length(dz)
      temoin <- if (length(m$temoin_n) && is.finite(m$temoin_n) && m$temoin_n > 0)
        sprintf("# Témoin : %s individus, %s mort(s).",
                .hstat_rlog_num(m$temoin_n), .hstat_rlog_num(m$temoin_x %||% 0))
      if (!ok) NULL
      else if (identical(m$methode %||% "", "nulle"))
        c("# Bioessai : les doses sont saisies dans le module, le script les reporte.",
          temoin,
          sprintf("dl50_dose  <- %s", .hstat_rlog_vec_num(dz)),
          sprintf("dl50_n     <- %s", .hstat_rlog_vec_num(dn)),
          sprintf("dl50_morts <- %s", .hstat_rlog_vec_num(dx)),
          "dl50_modele <- glm(cbind(dl50_morts, dl50_n - dl50_morts) ~ log10(dl50_dose),",
          "                   family = binomial(link = \"probit\"))",
          "summary(dl50_modele)",
          "# L'inverse normale est celle de WIN DL — l'approximation polynomiale",
          "# de Hastings (Abramowitz & Stegun 26.2.23), que le logiciel emploie.",
          "# `qnorm()` exact donnerait des doses létales différentes dès le",
          "# quatrième chiffre : le script cesserait de refaire ce que HStat a",
          "# calculé, ce qui est toute sa raison d'être.",
          "dl50_qnorm <- function(p) {",
          "  q <- pmin(pmax(ifelse(p > 0.5, 1 - p, p), 1e-300), 0.5)",
          "  t <- sqrt(log(1 / q^2))",
          "  v <- t - (2.515517 + 0.802853 * t + 0.010328 * t^2) /",
          "           (1 + 1.432788 * t + 0.189269 * t^2 + 0.001308 * t^3)",
          "  ifelse(p > 0.5, v, -v)",
          "}",
          "dl50_ab <- coef(dl50_modele)",
          "10^((dl50_qnorm(c(0.1, 0.5, 0.9)) - dl50_ab[1]) / dl50_ab[2])")
      else
        c("# NON RECONSTITUÉ : la mortalité naturelle n'est pas nulle dans cet",
          "# ajustement, et le modèle de Finney n'est alors pas un GLM binomial --",
          "# glm() ne sait pas ajuster c dans p = c + (1 - c) . F(a + b . log d).",
          "# Écrire un glm() ici rendrait un script plausible et faux.",
          sprintf("# Paramètres obtenus par HStat : a = %s, b = %s, c = %s.",
                  .hstat_rlog_num(m$a), .hstat_rlog_num(m$b), .hstat_rlog_num(m$c)),
          temoin)
    },

    "Séries temporelles" = if (length(v))
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
    sprintf("# Généré le %s", format(Sys.time(), "%Y-%m-%d a %H:%M:%S")),
    "# -----------------------------------------------------------------------------",
    "# Script reconstitue à partir des analyses menées dans l'application.",
    "#",
    "# À LIRE AVANT DE L'EXÉCUTER :",
    "#  - Vérifiez le chargement des données ci-dessous : le chemin, le séparateur",
    "#    et l'encodage dépendent de votre fichier.",
    "#  - Les étapes marquées « NON RECONSTITUE » ont été faites de façon",
    "#    interactive. Leurs paramètres sont rappelés en commentaire, mais le code",
    "#    n'est pas écrit : mieux vaut une lacune signalée qu'un code plausible et",
    "#    faux, qui donnerait un résultat différent de celui que vous avez lu.",
    "#  - Les analyses apparaissent dans l'ordre ou vous les avez menées.",
    "# =============================================================================",
    "")

  chargement <- c(
    "# ---- Données ----------------------------------------------------------------",
    if (!is.null(source) && nzchar(source))
      sprintf('# Fichier d\'origine : %s', source) else NULL,
    sprintf('%s <- read.csv("mon_fichier.csv", stringsAsFactors = FALSE)', donnees),
    "")

  if (is.null(history) || !length(history))
    return(paste(c(entete, chargement,
                   "# Aucune analyse enregistrée pour cette session."),
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
        c("#   NON RECONSTITUE : étape réalisée de façon interactive.", "")
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

HSTAT_QUALITE_GRAVITES <- c("bloquant", "important", "à surveiller")

# Le tableau de qualite est rendu tel quel : ses NOMS DE COLONNE sont donc
# ce que l'utilisateur lit. Ils restent sans accent -- le code y accede par
# `dq$Gravite`, et un nom accentue casserait cet acces sur une machine dont
# la locale n'est pas UTF-8. On relabellise donc a l'AFFICHAGE seulement.
HSTAT_QUALITE_LIBELLES <- c(Variable = "Variable", Constat = "Constat",
                            Gravite = "Gravit\u00e9", Suggestion = "Suggestion")

hstat_dq_affichage <- function(dq) {
  if (!is.data.frame(dq) || !nrow(dq)) return(dq)
  nm <- names(dq)
  vus <- nm %in% names(HSTAT_QUALITE_LIBELLES)
  nm[vus] <- unname(HSTAT_QUALITE_LIBELLES[nm[vus]])
  names(dq) <- nm
  dq
}

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
        trf("%.0f %% de valeurs manquantes", 100 * taux_na), "bloquant",
        "Variable quasi vide : l'exclure des analyses, ou retrouver la source des données manquantes.")))
    else if (taux_na >= 0.50)
      out <- c(out, list(.hstat_q_row(nm,
        trf("%.0f %% de valeurs manquantes", 100 * taux_na), "important",
        "Au-delà de la moitie, l'imputation invente plus qu'elle ne restitue. Préférer l'exclusion, ou une analyse sur cas complets en le déclarant.")))
    else if (taux_na >= seuil_na)
      out <- c(out, list(.hstat_q_row(nm,
        trf("%.0f %% de valeurs manquantes", 100 * taux_na), "à surveiller",
        "Onglet Nettoyage : imputation par la médiane/le mode, ou par kNN / missForest si le mécanisme n'est pas aléatoire.")))

    vals <- x[!vide]
    if (!length(vals)) next
    u <- length(unique(vals))

    # --- Variables sans information ----------------------------------------
    if (u == 1L) {
      out <- c(out, list(.hstat_q_row(nm,
        trf("une seule valeur (« %s »)", substr(as.character(vals[1]), 1, 30)), "important",
        "Variable constante : elle ne peut expliquer aucune variation. La retirer des modèles (elle fait aussi échouer l'ACP et la standardisation).")))
      next
    }
    if (chr && u >= 0.95 * length(vals) && u > 20)
      out <- c(out, list(.hstat_q_row(nm,
        trf("%d valeurs distinctes sur %d observations", u, length(vals)), "à surveiller",
        "Ressemble à un identifiant ou à du texte libre. Comme identifiant : l'exclure des analyses. Comme texte : l'onglet Analyses qualitatives sait le coder et le thématiser.")))

    if (chr) {
      tb <- sort(table(as.character(vals)), decreasing = TRUE)
      # --- Modalite ecrasante ---------------------------------------------
      if (tb[1] / length(vals) >= seuil_modalite)
        out <- c(out, list(.hstat_q_row(nm,
          trf("la modalité « %s » couvre %.0f %% des réponses", names(tb)[1],
                  100 * tb[1] / length(vals)), "important",
          "Variable quasi constante : aucun test ne détectera de différence. Regrouper les modalités, ou renoncer à l'utiliser comme facteur.")))
      # --- Modalites trop rares --------------------------------------------
      rares <- names(tb)[tb < 5]
      if (length(rares) && u <= max_modalites)
        out <- c(out, list(.hstat_q_row(nm,
          trf("%d modalité(s) sous 5 observations (%s)", length(rares),
                  paste(utils::head(rares, 4), collapse = ", ")), "à surveiller",
          "Les tests du Chi2 et les approximations asymptotiques y perdent leur validité. Regrouper ces modalités, ou passer au test exact de Fisher.")))
      if (u > max_modalites && u < 0.95 * length(vals))
        out <- c(out, list(.hstat_q_row(nm,
          trf("%d modalités distinctes", u), "à surveiller",
          trf("Au-delà de %d modalités, les tableaux croises deviennent illisibles et les effectifs trop faibles. Regrouper en catégories plus larges.", max_modalites))))

      # --- Nombres stockes en texte ----------------------------------------
      num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(vals))))
      if (mean(!is.na(num)) >= 0.95 && u > 10)
        out <- c(out, list(.hstat_q_row(nm,
          "des nombres stockes comme du texte", "important",
          "Convertir en numérique (onglet Nettoyage). En l'état, moyennes, corrélations et tests quantitatifs sont impossibles sur cette variable.")))
    } else if (is.numeric(vals)) {
      v <- as.numeric(vals)
      # --- Valeurs extremes -------------------------------------------------
      qs <- stats::quantile(v, c(.25, .75), na.rm = TRUE)
      iqr <- qs[2] - qs[1]
      if (is.finite(iqr) && iqr > 0) {
        ext <- sum(v < qs[1] - 3 * iqr | v > qs[2] + 3 * iqr)
        if (ext > 0)
          out <- c(out, list(.hstat_q_row(nm,
            trf("%d valeur(s) extrême(s) (au-delà de 3 écarts interquartiles)", ext), "à surveiller",
            "Vérifier s'il s'agit d'erreurs de saisie ou de vraies observations. Si elles sont réelles, préférer les tests de rangs, qui n'en dépendent pas.")))
      }
      if (any(!is.finite(v)))
        out <- c(out, list(.hstat_q_row(nm,
          trf("%d valeur(s) infinie(s) ou non numérique(s)", sum(!is.finite(v))), "bloquant",
          "Ces valeurs font échouer la plupart des calculs. Les remplacer ou les retirer dans l'onglet Nettoyage.")))
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
          trf("corrélation de %.2f entre ces deux variables", M[idx[k, 1], idx[k, 2]]), "important",
          "Redondance quasi parfaite : en garder une seule. Ensemble, elles rendent une régression instable (colinéarité) et faussent la lecture des coefficients.")))
  }

  # --- Lignes en double -------------------------------------------------------
  dup <- sum(duplicated(df))
  if (dup > 0)
    out <- c(out, list(.hstat_q_row("(jeu de données)",
      trf("%d ligne(s) strictement identique(s)", dup), "important",
      "Doublons probables de saisie ou d'import. Les supprimer, sinon ils gonflent artificiellement les effectifs et resserrent à tort les intervalles de confiance.")))

  # --- Effectif au regard du nombre de variables -------------------------------
  if (n < 5 * p)
    out <- c(out, list(.hstat_q_row("(jeu de données)",
      trf("%d observations pour %d variables", n, p), "important",
      "Trop peu d'observations par variable : les modèles multivariés surapprendront. Réduire le nombre de variables, ou se limiter à des analyses bivariées.")))
  if (n < 30)
    out <- c(out, list(.hstat_q_row("(jeu de données)",
      trf("effectif total de %d observations", n), "à surveiller",
      "Sous 30 observations, les approximations normales sont fragiles : préférer les tests exacts et les tests de rangs.")))

  if (!length(out)) return(.hstat_q_row("(jeu de données)",
    "aucun problème détecté", "à surveiller",
    "Structure saine : valeurs manquantes, modalités, valeurs extrêmes et redondances sont dans les clous."))

  res <- do.call(rbind, out)
  rang <- c("bloquant" = 1, "important" = 2, "à surveiller" = 3)
  res[order(rang[res$Gravite], res$Variable), , drop = FALSE]
}

# Resume d'une ligne, pour un bandeau ou une notification.
hstat_data_quality_resume <- function(dq) {
  if (is.null(dq) || !nrow(dq)) return(NULL)
  if (nrow(dq) == 1L && grepl("aucun problème", dq$Constat[1]))
    return("Aucun problème de qualité détecte sur ce jeu de données.")
  n <- table(factor(dq$Gravite, levels = HSTAT_QUALITE_GRAVITES))
  parts <- c(if (n[["bloquant"]]) sprintf("%d bloquant(s)", n[["bloquant"]]),
             if (n[["important"]]) sprintf("%d important(s)", n[["important"]]),
             if (n[["à surveiller"]]) trf("%d à surveiller", n[["à surveiller"]]))
  trf("%d constat(s) de qualité : %s.", nrow(dq), paste(parts, collapse = ", "))
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
      "Aucune analyse possible en l'état", "Bloquant",
      trf("%s ne comporte aucune valeur observée : il n'y a rien à analyser.",
              paste(sprintf("« %s »", vides), collapse = ", ")),
      "Au moins quelques observations non manquantes par variable.",
      paste("Vérifiez l'import (séparateur, colonne décalée) et les filtres",
            "actifs, ou traitez les valeurs manquantes dans l'onglet Nettoyage."))))
  }

  # ---- Une quantitative, un facteur ----
  if (length(quanti) >= 1 && !is.null(g)) {
    homo <- g$variances_homogenes
    if (g$k == 2) {
      if (profile$apparie) {
        out <- c(out, list(
          if (normal_tous)
            .hstat_reco_row("Test t apparie", "Recommandée",
              "Une variable quantitative mesurée deux fois sur les mêmes sujets, distribution compatible avec la normalité.",
              "Normalité des DIFFÉRENCES entre les deux mesures.",
              "Test des rangs signes de Wilcoxon.")
          else
            .hstat_reco_row("Wilcoxon apparie (rangs signes)", "Recommandée",
              "Mesures appariées et distribution qui s'écarte de la normalité.",
              "Symétrie approximative des différences.",
              "Test des signes, qui ne suppose rien de plus.")))
      } else {
        out <- c(out, list(
          if (normal_tous && !isFALSE(homo) && !isTRUE(g$petits_effectifs))
            .hstat_reco_row("Test t de Student (deux échantillons)", "Recommandée",
              trf("Une quantitative comparée entre %d groupes indépendants, normalité intra-groupe acceptable%s.",
                      g$k, if (isTRUE(homo)) " et variances homogènes" else ""),
              "Indépendance des observations ; normalité dans chaque groupe.",
              "Test t de Welch si les variances différent, Mann-Whitney sinon.")
          else if (normal_tous && !isTRUE(g$petits_effectifs))
            .hstat_reco_row("Test t de Welch", "Recommandée",
              "Normalité intra-groupe acceptable mais variances inégales entre les deux groupes : Welch ne les suppose pas égales.",
              "Indépendance des observations.",
              "Mann-Whitney.")
          else
            .hstat_reco_row("Mann-Whitney (Wilcoxon)", "Recommandée",
              trf("Deux groupes indépendants%s.",
                      if (isTRUE(g$petits_effectifs))
                        " dont au moins un compte moins de 5 observations : la normalité n'y est pas vérifiable"
                      else " et distribution non normale dans au moins un groupe"),
              "Indépendance ; formes de distribution comparables si l'on conclut sur les médianes.",
              "Comparaison des rangs seule, sans conclure sur la médiane.")))
      }
    } else if (g$k > 2) {
      out <- c(out, list(
        if (normal_tous && !isFALSE(homo) && !isTRUE(g$petits_effectifs))
          .hstat_reco_row("ANOVA à un facteur", "Recommandée",
            trf("Une quantitative comparée entre %d groupes, normalité intra-groupe et homogénéité des variances acceptables.", g$k),
            "Indépendance ; normalité des résidus ; homogénéité des variances.",
            "ANOVA de Welch, ou Kruskal-Wallis.")
        else if (normal_tous && !isTRUE(g$petits_effectifs))
          .hstat_reco_row("ANOVA de Welch", "Recommandée",
            "Normalité intra-groupe acceptable mais variances hétérogènes entre groupes.",
            "Indépendance des observations.",
            "Kruskal-Wallis.")
        else if (isTRUE(g$petits_effectifs))
          .hstat_reco_row("Kruskal-Wallis", "Recommandée",
            trf("%d groupes dont au moins un compte moins de 5 observations : la normalité n'y est pas vérifiable, un test de rangs ne la suppose pas.", g$k),
            "Indépendance des observations.",
            "Test de permutation, si même les rangs sont trop peu nombreux.")
        else
          .hstat_reco_row("Kruskal-Wallis", "Recommandée",
            trf("%d groupes indépendants et distribution non normale dans au moins un groupe.", g$k),
            "Indépendance ; formes comparables pour conclure sur les médianes.",
            "Comparaison des rangs seule.")))
      out <- c(out, list(.hstat_reco_row(
        "Comparaisons post-hoc", "À enchaîner",
        "Un test global significatif dit qu'au moins deux groupes différent, jamais lesquels : le post-hoc le précise.",
        "Correction pour comparaisons multiples (Tukey, Holm, Bonferroni...).",
        "Sans correction, le risque d'erreur de première espèce s'accumule.")))
    }
    if (isTRUE(g$petits_effectifs))
      out <- c(out, list(.hstat_reco_row(
        "Test exact / permutation", "À envisager",
        "Au moins un groupe compte moins de 5 observations : les approximations asymptotiques deviennent peu fiables.",
        "-", "-")))
  }

  # ---- Deux quantitatives, aucun facteur ----
  if (length(quanti) >= 2 && is.null(g)) {
    out <- c(out, list(
      if (normal_tous)
        .hstat_reco_row("Corrélation de Pearson", "Recommandée",
          "Deux variables quantitatives dont les distributions sont compatibles avec la normalité.",
          "Relation linéaire ; absence de valeurs extrêmes influentes.",
          "Corrélation de Spearman, qui ne suppose que la monotonie.")
      else
        .hstat_reco_row("Corrélation de Spearman", "Recommandée",
          "Deux quantitatives dont au moins une s'écarte de la normalité : la corrélation des rangs ne la suppose pas.",
          "Relation monotone.",
          "Tau de Kendall, plus robuste sur petits effectifs.")))
    out <- c(out, list(.hstat_reco_row(
      "Régression linéaire", "À envisager",
      "Si l'une des deux variables est expliquée par l'autre, la régression quantifie l'effet la ou la corrélation ne mesure que l'association.",
      "Linéarité, indépendance, homoscédasticité, normalité des résidus.",
      "Régression robuste, ou transformation de la réponse.")))
    if (length(quanti) >= 3)
      out <- c(out, list(.hstat_reco_row(
        "ACP (analyse en composantes principales)", "À envisager",
        trf("%d variables quantitatives : l'ACP résume leur structure commune et révèle les redondances.", length(quanti)),
        "Variables corrélées entre elles ; effectif supérieur au nombre de variables.",
        "Matrice de corrélations seule si les variables sont indépendantes.")))
  }

  # ---- Deux qualitatives ----
  if (length(quali) >= 2 || (length(quali) >= 1 && !is.null(g) && length(quanti) == 0)) {
    petits <- isTRUE(g$petits_effectifs)
    out <- c(out, list(
      if (petits)
        .hstat_reco_row("Test exact de Fisher", "Recommandée",
          "Tableau croise dont certains effectifs attendus sont faibles : le chi-deux n'y est plus valide.",
          "Effectifs attendus < 5 dans plus de 20 % des cases.",
          "Regroupement de modalités, puis chi-deux.")
      else
        .hstat_reco_row("Test du chi-deux d'indépendance", "Recommandée",
          "Deux variables catégorielles : le chi-deux teste leur indépendance.",
          "Effectifs attendus >= 5 dans au moins 80 % des cases.",
          "Test exact de Fisher.")))
    out <- c(out, list(.hstat_reco_row(
      "V de Cramer / Odds Ratio", "À enchaîner",
      "Un test dit s'il y a association, jamais sa force : la taille d'effet le dit.",
      "-", "-")))
    if (length(quali) >= 3)
      out <- c(out, list(.hstat_reco_row(
        "ACM (analyse des correspondances multiples)", "À envisager",
        trf("%d variables catégorielles : l'ACM positionne individus et modalités dans un même plan.", length(quali)),
        "Modalités suffisamment représentées (regrouper les plus rares).", "-")))
  }

  # ---- Ordinales ----
  if (length(ordin) >= 1) {
    out <- c(out, list(.hstat_reco_row(
      "Analyse ordinale (Likert, rangs)", if (length(ordin) >= 2) "Recommandée" else "À envisager",
      "Variable à modalités ordonnées : la traiter comme quantitative suppose des écarts égaux entre échelons, ce qui est rarement vrai.",
      "Ordre des modalités correctement déclaré.",
      "Traitement catégoriel simple si l'ordre n'a pas de sens.")))
    if (length(ordin) >= 2)
      out <- c(out, list(.hstat_reco_row(
        "Corrélation de Spearman / Kendall", "À envisager",
        "Deux variables ordinales : la corrélation des rangs respecte leur nature.",
        "Relation monotone.", "-")))
  }

  # ---- Reponse binaire : modelisation ----
  bin <- names(types)[types == "binaire"]
  if (length(bin) >= 1 && (length(quanti) >= 1 || length(quali) >= 1))
    out <- c(out, list(.hstat_reco_row(
      "Régression logistique", "À envisager",
      trf("« %s » ne prend que deux valeurs : la régression logistique modélise sa probabilité à partir des autres variables.", bin[1]),
      "Effectif suffisant par modalité (au moins 10 événements par prédicteur).",
      "Test exact ou régression pénalisée si les effectifs sont faibles.")))

  if (!length(out)) return(NULL)
  res <- do.call(rbind, out)
  # Le rang tient a la pertinence, pas a l'ordre d'ecriture des regles.
  ordre <- c("Recommandée" = 1, "À enchaîner" = 2, "À envisager" = 3)
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
                             "Nettoyage", "Filtrage", "Seuils d'efficacité")

hstat_reco_verdict <- function(reco, titre_analyse, module = NULL) {
  if (is.null(reco) || !nrow(reco) || is.null(titre_analyse) || !nzchar(titre_analyse))
    return(NULL)
  if (!is.null(module) && module %in% HSTAT_RECO_EXPLORATOIRE) {
    reco_1 <- reco$Analyse[reco$Pertinence == "Recommandée"]
    return(list(
      coherent = TRUE, exploratoire = TRUE,
      message = trf(
        "« %s » décrit vos données : c'est une étape préliminaire, pas un test, il n'y a donc rien à valider ici. Pour aller plus loin, le profil de vos variables appelle %s. À vous de décider si cette suite a du sens pour votre question de recherche.",
        titre_analyse,
        if (length(reco_1)) paste(reco_1, collapse = " ou ") else "une analyse inférentielle")))
  }
  cle <- function(x) {
    x <- tolower(hstat_sans_accents(x))
    gsub("[^a-z]+", " ", x)
  }
  t <- cle(titre_analyse)
  mots <- lapply(cle(reco$Analyse), function(a) strsplit(a, " ")[[1]])
  hit <- vapply(mots, function(w) {
    w <- w[nchar(w) >= 4]
    length(w) > 0 && any(vapply(w, function(z) grepl(z, t, fixed = TRUE), logical(1)))
  }, logical(1))
  reco_1 <- reco$Analyse[reco$Pertinence == "Recommandée"]
  if (any(hit))
    list(coherent = TRUE, exploratoire = FALSE,
         message = trf(
           "L'analyse que vous avez menée figure parmi celles que le profil de vos données appelle (%s).",
           paste(reco$Analyse[hit], collapse = ", ")))
  else
    list(coherent = FALSE, exploratoire = FALSE,
         message = trf(
           "Au vu du profil des variables, %s aurait été le choix le plus direct. Cela ne disqualifie pas votre analyse : un objectif de recherche ou une contrainte de terrain peut la justifier. À vous de trancher.",
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
    L <- c(L, "### Données analysées", "",
           sprintf("- %d observations ; %s.", profile$n,
                   paste(trf("%d variable(s) %s", table(ty), names(table(ty))),
                         collapse = ", ")))
    if (!is.null(profile$groupe))
      L <- c(L, trf("- Facteur « %s » a %d modalités (effectifs : %s)%s.",
                        profile$groupe$nom, profile$groupe$k,
                        paste(profile$groupe$effectifs, collapse = ", "),
                        if (isTRUE(profile$groupe$petits_effectifs))
                          " — au moins un groupe sous 5 observations" else ""))
    for (nm in names(profile$variables)) {
      e <- profile$variables[[nm]]
      if (is.null(e$normale) || is.na(e$normale$ok)) next
      L <- c(L, trf("- « %s » : distribution %s la normalité (%s%s).", nm,
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
      etiq <- if (length(lab_col)) as.character(tb[[lab_col[1]]][i]) else trf("ligne %d", i)
      L <- c(L, trf("- **%s** : p = %s -> %s au seuil de %.0f %%.",
                        etiq,
                        if (is.na(p)) "n.d." else format(signif(p, 3), scientific = p < 1e-4),
                        .hstat_ai_signif(p, alpha), 100 * alpha))
      lues <- lues + 1L
    }
    if (nrow(tb) > 20L) L <- c(L, trf("- (... %d lignes supplémentaires)", nrow(tb) - 20L))
  }
  if (lues == 0L)
    L <- c(L, "### Résultats", "",
           "Aucune p-value repérée automatiquement dans les tableaux : reportez-vous au détail ci-dessous.")
  L <- c(L, "")

  if (!is.null(verdict))
    L <- c(L, "### Cohérence du choix d'analyse", "", verdict$message, "")

  if (!is.null(reco) && nrow(reco)) {
    L <- c(L, "### Analyses appelées par vos données", "")
    for (i in seq_len(nrow(reco)))
      L <- c(L, sprintf("- **%s** *(%s)* — %s", reco$Analyse[i], reco$Pertinence[i],
                        reco$Pourquoi[i]))
    L <- c(L, "",
           paste0("*Ces propositions découlent du profil de vos variables. ",
                  "Le choix de l'analyse, lui, vous appartient : lui seul engage ",
                  "l'interprétation scientifique de votre travail.*"))
  }
  paste(L, collapse = "\n")
}

HSTAT_AI_NIVEAUX <- c(
  "Rapport scientifique (concis, normalise)" = "scientifique",
  "Vulgarise (sans jargon)"                  = "vulgarise",
  "Détaille (méthode + limites)"             = "detaille")

hstat_ai_interpret_prompt <- function(ctx, profile = NULL, reco = NULL,
                                      verdict = NULL, niveau = "scientifique",
                                      contexte = "", alpha = 0.05,
                                      lang = hstat_langue_session()) {
  style <- switch(niveau,
    vulgarise = paste0("Écris pour un lecteur sans formation statistique : pas de jargon, ",
                       "pas de symboles, des phrases courtes. Explique ce que le résultat ",
                       "signifie concrètement."),
    detaille  = paste0("Rédige un texte complet : rappel de la méthode et de ses conditions ",
                       "d'application, résultats chiffrés, taille d'effet, limites de ",
                       "l'analyse et portée des conclusions."),
    paste0("Rédige comme une section « Résultats » d'article scientifique : concis, ",
           "impersonnel, chiffres entre parenthèses selon l'usage (statistique, ddl, ",
           "p, taille d'effet)."))

  bloc_reco <- if (!is.null(reco) && nrow(reco))
    paste0("\n\nANALYSES QUE LE PROFIL DES DONNÉES APPELLE (calculées par HStat, ",
           "déterministes — reprends-les telles quelles, n'en invente pas d'autres) :\n",
           paste(sprintf("- %s (%s) : %s", reco$Analyse, reco$Pertinence, reco$Pourquoi),
                 collapse = "\n")) else ""

  bloc_profil <- if (!is.null(profile)) {
    ty <- vapply(profile$variables, function(e) e$type, character(1))
    nrm <- vapply(profile$variables, function(e)
      if (is.null(e$normale) || is.na(e$normale$ok)) "non évaluée"
      else if (isTRUE(e$normale$ok)) "compatible avec la normalité"
      else "non normale", character(1))
    paste0("\n\nPROFIL DES DONNÉES :\n",
           sprintf("- %d observations\n", profile$n),
           paste(sprintf("- %s : %s (%s)", names(ty), ty, nrm), collapse = "\n"),
           if (!is.null(profile$groupe))
             sprintf("\n- Facteur %s : %d modalités, effectifs %s",
                     profile$groupe$nom, profile$groupe$k,
                     paste(profile$groupe$effectifs, collapse = "/")) else "")
  } else ""

  paste0(
    "Tu es statisticien. On te donne les RÉSULTATS d'une analyse déjà réalisée ",
    "par l'utilisateur. Ta mission est de les INTERPRÉTER, ",
    hstat_ai_consigne_langue(lang), ".\n\n",
    "RÈGLES ABSOLUES :\n",
    "1. N'invente aucun chiffre. N'utilise que les valeurs présentes ci-dessous. ",
    "Si une valeur manque, dis-le au lieu de la deviner.\n",
    "2. Ne recalcule rien et ne propose pas de relancer l'analyse à ta façon.\n",
    "3. Le choix de l'analyse appartient à l'utilisateur : tu peux signaler ",
    "qu'une autre méthode aurait mieux convenu, jamais affirmer qu'il a eu tort.\n",
    trf("4. Seuil de signification retenu : %.0f %%.\n", 100 * alpha),
    "\nSTYLE : ", style,
    if (nzchar(trimws(contexte))) paste0("\n\nCONTEXTE DE L'ÉTUDE : ", contexte) else "",
    bloc_profil, bloc_reco,
    "\n\n", hstat_ai_context_text(ctx),
    "\n\nStructure ta réponse en markdown avec exactement ces sections :\n",
    "## Lecture des résultats\n",
    "## Ce que cela signifie\n",
    "## Précautions et limites\n",
    "## Analyse recommandée pour la suite\n",
    "Dans la dernière section, appuie-toi sur les analyses appelées par le profil ",
    "des données, et rappelle que la décision revient à l'utilisateur.")
}


# ---------------------------------------------------------------------------
# GUIDAGE A LA FIN DE L'ANALYSE : RETIRE
# ---------------------------------------------------------------------------
# Un bandeau greffe sur les douze onglets d'analyse et une notification
# repetaient a chaque resultat depose ce que le profil des donnees appelle.
# La meme recommandation vit en entier dans l'onglet « Interpretation & aide a
# la decision », ou l'utilisateur la demande. Repetee a chaque calcul, elle
# recouvrait les resultats au lieu de les eclairer.
#
# Ont disparu avec elle : HSTAT_AI_HINT_IDS, hstat_ai_hint_slot(),
# hstat_ai_with_hint(), hstat_ai_hint_ui() et hstat_ai_hint_text(). Le registre
# de capture (hstat_ai_capture) est intact : c'est lui qui alimente l'onglet,
# le journal de reproductibilite et le rapport.

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
                  " Interprétation des résultats & aide à la décision",
                  style = "margin:0;color:white;"),
        shiny::p(style = "margin:8px 0 0 0;color:#d6e4f0;font-size:13px;",
          "L'assistance interprète les résultats que vous venez d'obtenir et vous ",
          "indique quelles analyses le profil de vos données appelle. ",
          shiny::tags$b("Elle ne choisit ni ne lance aucune analyse"),
          " : la méthode reste votre décision, et votre responsabilité."))
    ),
    shiny::fluidRow(
      shiny::column(4,
        shinydashboard::box(width = 12, status = "primary", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("clipboard-check"), " Analyse à interpréter"),
          shiny::uiOutput(ns("ctx_box")),
          shiny::hr(style = "margin:10px 0;"),
          shiny::h5(shiny::icon("table-columns"), " Profil des données"),
          shiny::tags$small(style = "color:#7f8c8d;display:block;margin-bottom:8px;",
            "Sert à la recommandation. Pre-rempli depuis la dernière analyse ; ",
            "ajustez-le si vous voulez explorer un autre scenario."),
          shiny::uiOutput(ns("vars_ui")),
          shiny::uiOutput(ns("group_ui")),
          shiny::checkboxInput(ns("paired"), "Mesures appariées (mêmes sujets)", FALSE),
          shiny::sliderInput(ns("alpha"), "Seuil de signification", min = 0.001,
                             max = 0.10, value = 0.05, step = 0.001),
          shiny::hr(style = "margin:10px 0;"),
          shiny::selectInput(ns("niveau"), "Niveau de rédaction",
                             choices = HSTAT_AI_NIVEAUX, selected = "scientifique"),
          shiny::textAreaInput(ns("contexte"), "Contexte de l'étude (facultatif)",
                               rows = 2,
                               placeholder = "Ex. essai clinique, 3 bras, critère principal"),
          shiny::hr(style = "margin:10px 0;"),
          shiny::actionButton(ns("go"), "Interpréter avec le modèle",
                              icon = shiny::icon("wand-magic-sparkles"),
                              class = "btn-primary btn-block"),
          shiny::br(),
          shiny::actionButton(ns("go_offline"), "Lecture automatique (sans modèle)",
                              icon = shiny::icon("calculator"),
                              class = "btn-default btn-block btn-sm"),
          shiny::tags$small(style = "color:#7f8c8d;display:block;margin-top:8px;",
            shiny::icon("circle-info"),
            " La lecture automatique ne génère rien : elle relit vos tableaux et ",
            "en énonce les p-values. Elle fonctionne toujours, sans modèle.")
        ),
        shinydashboard::box(width = 12, status = "warning", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          title = shiny::tagList(shiny::icon("microchip"), " Moteur d'inférence"),
          shiny::radioButtons(ns("engine"), NULL, choices = HSTAT_AI_ENGINES,
                              selected = "auto"),
          shiny::uiOutput(ns("reglages")),
          shiny::uiOutput(ns("status")))
      ),
      shiny::column(8,
        shinydashboard::box(width = 12, status = "primary", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("lightbulb"), " Aide à la décision"),
          shiny::tabsetPanel(id = ns("tabs"),
            shiny::tabPanel(shiny::tagList(shiny::icon("file-lines"), " Interprétation"),
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
            shiny::tabPanel(shiny::tagList(shiny::icon("route"), " Analyse recommandée"),
              shiny::br(),
              shiny::uiOutput(ns("verdict")),
              DT::DTOutput(ns("reco_table")),
              shiny::br(),
              shiny::div(style = "background:#fdf3e3;border-left:5px solid #e67e22;padding:12px 16px;border-radius:6px;font-size:13px;",
                shiny::icon("triangle-exclamation"),
                shiny::tags$b(" Ces propositions ne décident pas à votre place."),
                " Elles découlent mécaniquement du type de vos variables, de leurs ",
                "effectifs et de tests de normalité et d'homogénéité. Une question de ",
                "recherche, un plan d'expérience ou une contrainte de terrain peuvent ",
                "justifier un autre choix — que vous seul êtes en mesure de faire.")),
            shiny::tabPanel(shiny::tagList(shiny::icon("magnifying-glass-chart"), " Profil des données"),
              shiny::br(),
              DT::DTOutput(ns("profile_table")),
              shiny::br(),
              shiny::uiOutput(ns("profile_notes"))),
            shiny::tabPanel(shiny::tagList(shiny::icon("code"), " Journal & reproductibilité"),
              shiny::br(),
              shiny::div(style = "background:#eafaf1;border-left:5px solid #27ae60;padding:12px 16px;border-radius:6px;font-size:13px;",
                shiny::tags$strong(shiny::icon("scroll"), " Script R de votre session"),
                shiny::tags$p(style = "margin:6px 0 0 0;",
                  "Les analyses que vous avez menées, dans l'ordre, sous forme de code R ",
                  "exécutable. C'est ce qu'un relecteur attend pour refaire le chemin. ",
                  shiny::tags$b("Les étapes purement interactives sont signalées, pas inventées"),
                  " : un script qui différerait en silence de ce que vous avez lu serait ",
                  "pire que pas de script.")),
              shiny::br(),
              shiny::fluidRow(
                shiny::column(6, shiny::uiOutput(ns("rlog_resume"))),
                shiny::column(3, shiny::textInput(ns("rlog_objet"), "Nom de l'objet de données",
                                                  value = "donnees")),
                shiny::column(3, shiny::br(),
                  shiny::downloadButton(ns("dl_rlog"), "Télécharger le script (.R)",
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
                  "Vos analyses, vos figures et vos interprétations réunies en un ",
                  "rapport rédige. ",
                  shiny::tags$b("Le rapport ne calcule rien"),
                  " : il met en forme ce que vous avez déjà obtenu. À relire avant ",
                  "diffusion — les interprétations éclairent la lecture, elles ne la valident pas.")),
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
                  shiny::selectInput(ns("rep_dpi"), "Résolution des figures",
                                     choices = HSTAT_REPORT_DPI,
                                     selected = "1000"),
                  shiny::tags$small(style = "color:#7f8c8d;display:block;margin-top:-8px;",
                    shiny::icon("circle-info"),
                    " Les figures sont tracées pour l'impression, pas pour l'écran : ",
                    "à 150 dpi elles paraissent nettes à l'affichage et sortent ",
                    "floues sur papier, et le défaut ne se voit qu'une fois le ",
                    "document remis. Comptez quelques secondes par figure."),
                  shiny::uiOutput(ns("rep_dispo"))),
                shiny::column(4,
                  shiny::checkboxGroupInput(ns("rep_sections"), "Sections à inclure",
                                            choices = HSTAT_REPORT_SECTIONS,
                                            selected = unname(HSTAT_REPORT_SECTIONS))),
                shiny::column(3,
                  shiny::uiOutput(ns("rep_resume")),
                  shiny::br(),
                  shiny::downloadButton(ns("dl_rapport"), "Produire le rapport",
                                        class = "btn-success btn-block"),
                  shiny::br(),
                  shiny::actionButton(ns("rep_apercu_go"), "Aperçu",
                                      icon = shiny::icon("eye"),
                                      class = "btn-default btn-block btn-sm"))),
              shiny::hr(),
              shiny::div(style = "background:#ffffff;border:1px solid #e0e0e0;border-radius:6px;padding:16px 20px;max-height:620px;overflow:auto;",
                         shiny::uiOutput(ns("rep_apercu")))),
            shiny::tabPanel(shiny::tagList(shiny::icon("stethoscope"), " Qualité des données"),
              shiny::br(),
              shiny::uiOutput(ns("dq_resume")),
              DT::DTOutput(ns("dq_table")),
              shiny::br(),
              shiny::div(style = "background:#eaf4fb;border-left:5px solid #2e86c1;padding:12px 16px;border-radius:6px;font-size:13px;",
                shiny::icon("circle-info"),
                shiny::tags$b(" Comment lire ce diagnostic."),
                " Chaque constat porte sa gravite et une suggestion concrète. ",
                shiny::tags$b("Bloquant"), " : l'analyse échouera ou n'aura pas de sens en l'état. ",
                shiny::tags$b("Important"), " : le résultat sera trompeur si rien n'est fait. ",
                shiny::tags$b("À surveiller"), " : à connaître avant d'interpréter. ",
                "Le diagnostic est calculé dans R, sans modèle et sans réseau ; ",
                "au-delà de 20 000 lignes il porte sur un échantillon."),
              shiny::br(),
              shiny::downloadButton(ns("dl_dq"), "Télécharger le diagnostic (CSV)",
                                    class = "btn-info btn-sm")),
            shiny::tabPanel(shiny::tagList(shiny::icon("table"), " Résultats captures"),
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
        "Chargez d'abord un jeu de données dans l'onglet Chargement."))
      as.data.frame(d)
    })

    output$ctx_box <- shiny::renderUI({
      c0 <- ctx()
      if (is.null(c0))
        return(shiny::div(class = "callout callout-warning", style = "padding:10px 14px;",
          shiny::icon("circle-info"),
          shiny::tags$b(" Aucune analyse enregistrée."),
          shiny::br(),
          "Lancez une analyse dans l'un des onglets (tests, descriptives, ",
          "corrélations, multivariées, qualitatives...), puis revenez ici. ",
          "Vous pouvez déjà obtenir une recommandation en choisissant vos ",
          "variables ci-dessous."))
      shiny::div(class = "callout callout-success", style = "padding:10px 14px;",
        shiny::icon("circle-check"), shiny::tags$b(" ", c0$title), shiny::br(),
        shiny::tags$small(trf("Module : %s - %d tableau(x) - %s",
                                  c0$module, length(c0$tables),
                                  format(c0$time, "%H:%M:%S"))))
    })

    # Les selecteurs sont pre-remplis depuis l'analyse capturee : l'utilisateur
    # arrive sur un profil deja pertinent, qu'il reste libre de modifier.
    output$vars_ui <- shiny::renderUI({
      d <- get_data()
      pre <- intersect(ctx()$meta$variables %||% character(0), names(d))
      shiny::selectInput(ns("vars"), "Variables analysées", choices = names(d),
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
    # Les reglages suivent le moteur choisi : la table dit ce qu'il faut
    # demander (cle ou non, adresse, modele).
    output$reglages <- shiny::renderUI(
      hstat_ai_reglages_ui(ns, input$engine %||% "auto"))

    shiny::observeEvent(input$ping, {
      st <- hstat_ai_status(input$engine %||% "auto", input$url, input$model,
                            input$key)
      shiny::showNotification(st$message,
                              type = if (isTRUE(st$ok)) "message" else "warning",
                              duration = 10)
    })
    output$status <- shiny::renderUI({
      st <- hstat_ai_status(input$engine %||% "auto", input$url, input$model,
                            input$key)
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
          "Aucune analyse enregistrée : lancez d'abord une analyse dans un autre onglet.",
          type = "warning", duration = 7)
        return(invisible(NULL))
      }
      pr <- tryCatch(profile(), error = function(e) NULL)
      rv$txt <- hstat_ai_interpret_offline(c0, pr, tryCatch(reco(), error = function(e) NULL),
                                           tryCatch(verdict(), error = function(e) NULL),
                                           input$alpha %||% 0.05)
      rv$source <- "Lecture automatique (aucun modèle, aucun réseau)"
      rv$err <- NULL
    }

    shiny::observeEvent(input$go_offline, .offline())

    shiny::observeEvent(input$go, {
      c0 <- ctx()
      if (is.null(c0)) {
        shiny::showNotification(
          "Aucune analyse enregistrée : lancez d'abord une analyse dans un autre onglet.",
          type = "warning", duration = 7); return()
      }
      eng <- input$engine %||% "auto"
      if (identical(eng, "auto")) {
        # Le moteur « sans modele » de l'assistance, c'est la lecture automatique.
        .offline(); return()
      }
      st <- hstat_ai_status(eng, input$url, input$model, input$key)
      if (!isTRUE(st$ok)) {
        shiny::showNotification(trf("%s La lecture automatique, elle, reste disponible.", st$message),
          type = "error", duration = 12)
        return()
      }
      pr <- tryCatch(profile(), error = function(e) NULL)
      rc <- tryCatch(reco(), error = function(e) NULL)
      vd <- tryCatch(verdict(), error = function(e) NULL)

      shiny::withProgress(message = "Rédaction de l'interprétation...", value = 0.4, {
        res <- hstat_ai_call(
          hstat_ai_interpret_prompt(c0, pr, rc, vd, input$niveau %||% "scientifique",
                                    input$contexte %||% "", input$alpha %||% 0.05),
          system = paste0("Tu es statisticien. Tu interprètes des résultats déjà ",
                          "obtenus, sans jamais inventer de chiffre ni relancer ",
                          "d'analyse. Tu réponds ", hstat_ai_consigne_langue(),
                          ", en markdown."),
          engine = eng, url = input$url,
          model = input$model, api_key = input$key, json = FALSE)
        shiny::incProgress(0.5)
        if (!isTRUE(res$ok)) {
          rv$err <- res$error
          shiny::showNotification(trf("%s Repli sur la lecture automatique.", res$error), type = "error", duration = 12)
          .offline(); return()
        }
        rv$txt <- res$text
        # Le service employe est nomme tel quel : avec sept moteurs possibles,
        # « Modele local » pour tout ce qui n'est pas Claude serait faux.
        rv$source <- sprintf("%s%s", hstat_ai_fournisseur(eng)$label,
                             if (!is.null(res$model)) sprintf(" (%s)", res$model) else "")
        rv$err <- NULL
      })
    })

    output$interp_note <- shiny::renderUI({
      if (is.null(rv$txt))
        return(shiny::div(class = "callout callout-info", style = "padding:10px 14px;",
          shiny::icon("circle-info"),
          " Lancez une analyse dans un autre onglet, puis demandez son interprétation ici."))
      shiny::div(class = "callout callout-success", style = "padding:8px 12px;font-size:12px;",
        shiny::icon("pen-nib"), shiny::tags$b(" Source : "), rv$source,
        shiny::br(),
        shiny::tags$small(
          "Texte à relire avant publication : l'assistance interprète, elle ne valide pas."))
    })

    output$interp <- shiny::renderUI({
      if (is.null(rv$txt))
        return(shiny::tags$em(style = "color:#95a5a6;", "Aucune interprétation pour l'instant."))
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
        "Choisissez au moins une variable analysée, à gauche."))
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
                    else if (is.na(e$normale$ok)) "non évaluable"
                    else if (isTRUE(e$normale$ok)) "compatible" else "écart à la normalité",
        `Test de normalite` = if (is.null(e$normale)) "-" else e$normale$methode,
        p = if (is.null(e$normale) || is.na(e$normale$p)) NA_real_
            else signif(e$normale$p, 4),
        check.names = FALSE, stringsAsFactors = FALSE)))
      DT::datatable(tb, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$profile_notes <- shiny::renderUI({
      p <- profile()
      if (is.null(p)) return(NULL)
      el <- list(shiny::tags$li(trf("%d observations dans le jeu de données.", p$n)))
      if (!is.null(p$groupe)) {
        g <- p$groupe
        el <- c(el, list(shiny::tags$li(sprintf(
          "Facteur « %s » : %d modalités (%s), effectifs %s.", g$nom, g$k,
          paste(g$modalites, collapse = ", "), paste(g$effectifs, collapse = " / ")))))
        if (isTRUE(g$petits_effectifs))
          el <- c(el, list(shiny::tags$li(shiny::tags$b("Au moins un groupe compte moins de 5 observations"),
            " : les approximations asymptotiques y sont peu fiables.")))
        if (!is.na(g$variances_homogenes))
          el <- c(el, list(shiny::tags$li(trf("Variances %s entre groupes.",
            if (isTRUE(g$variances_homogenes)) "homogènes" else "hétérogènes"))))
        if (!is.na(g$equilibre) && !isTRUE(g$equilibre))
          el <- c(el, list(shiny::tags$li("Groupes déséquilibres (rapport des effectifs > 1,5).")))
      }
      # Le detail par groupe justifie la recommandation : sans lui, on demande
      # a l'utilisateur de croire sur parole.
      for (nm in names(p$variables)) {
        e <- p$variables[[nm]]
        if (is.null(e$normale) || is.null(e$normale$detail)) next
        d <- e$normale$detail
        el <- c(el, list(shiny::tags$li(sprintf(
          "Normalité de « %s » testée DANS CHAQUE GROUPE : %s.", nm,
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
          " Aucune analyse enregistrée. Le journal se remplit à mesure que vous travaillez."))
      reconstitues <- sum(vapply(h, function(c0)
        !is.null(hstat_rlog_code(c0)), logical(1)))
      shiny::div(class = "callout callout-success", style = "padding:10px 14px;",
        shiny::icon("circle-check"),
        sprintf(" %d analyse(s) enregistrée(s), dont %d avec leur code R.",
                length(h), reconstitues),
        if (reconstitues < length(h))
          shiny::tags$small(style = "display:block;color:#7f8c8d;",
            trf("Les %d autres sont documentées en commentaire : leurs réglages étaient interactifs.",
                    length(h) - reconstitues)))
    })

    output$rlog_table <- DT::renderDT({
      h <- values$aiHistory
      shiny::validate(shiny::need(!is.null(h) && length(h) > 0,
        "Le journal se remplit à mesure que vous menez des analyses."))
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
        "Chargez d'abord un jeu de données dans l'onglet Chargement."))
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
      DT::datatable(hstat_dq_affichage(dq), rownames = FALSE, filter = "top",
                    options = list(pageLength = 15, scrollX = TRUE)) |>
        DT::formatStyle(HSTAT_QUALITE_LIBELLES[["Gravite"]], target = "row",
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
                               detail = trf("Figure %d sur %d — %s", i, n,
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
                           tryCatch(hstat_dq_affichage(data_quality()),
                                    error = function(e) NULL),
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
        trf("%d analyse(s)", length(h)), shiny::br(),
        trf("%d figure(s) disponible(s)", nfig), shiny::br(),
        if (!is.null(rv$txt) && nzchar(rv$txt)) "une interprétation rédigée"
        else shiny::tags$span(style = "color:#b9770e;",
          "aucune interprétation (lancez-en une dans l'onglet Interprétation)"))
    })

    rep_apercu <- shiny::eventReactive(input$rep_apercu_go, {
      figs <- if ("figures" %in% (input$rep_sections %||% character(0)))
        rep_figures(apercu = TRUE) else NULL
      .hstat_rep_images_html(.hstat_rep_md_to_html(rep_markdown(figs)))
    })

    output$rep_apercu <- shiny::renderUI({
      if (is.null(input$rep_apercu_go) || input$rep_apercu_go == 0)
        return(shiny::tags$em(style = "color:#95a5a6;",
          "Cliquez sur « Aperçu » pour voir le rapport avant de le produire."))
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
        "Aucune analyse enregistrée."))
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
