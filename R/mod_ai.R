# ===========================================================================
# HStat - mod_ai.R
# Moteur d'inference partage
# ---------------------------------------------------------------------------
# Un seul role : parler aux fournisseurs de modeles de langue, pour les modules
# qui en ont besoin. Aujourd'hui c'est l'atelier de codage CAQDAS
# (`mod_coding.R`), qui propose de pre-remplir un livre de codes et de
# pre-coder un corpus.
#
# Ce fichier ne DECIDE rien et n'INTERPRETE rien : il transporte une invite et
# rend une reponse. Le choix de la methode, la lecture des resultats et leur
# redaction restent a l'analyste, qui en demeure responsable.
#
# L'onglet « Interpretation & aide a la decision » a ete retire (voir
# CLAUDE.md) : avec lui sont partis le registre de capture, le profil des
# variables, la recommandation deterministe, le diagnostic de qualite, le
# journal de reproductibilite et le rapport automatique. Ce qui reste ici est
# exactement ce dont `mod_coding.R` se sert.
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
#
# UNE CLE D'AMBIANCE NE SUIT PAS UNE ADRESSE CHANGEE. C'est le prolongement de
# la regle deja ecrite pour GITHUB_TOKEN -- « une cle n'est jamais ambiante » --
# et il fallait l'etendre, parce que deux decisions justes se combinaient mal :
# l'adresse du service est un champ TEXTE LIBRE (un service qui demenage ne doit
# pas obliger a rouvrir le code), et le champ de cle INVITE a rester vide pour
# que la variable d'environnement serve. Les deux se resolvaient
# independamment.
#
# Sur un deploiement partage -- shinyapps.io, Posit Connect, Shiny Server, ceux
# que le README prevoit -- l'exploitant pose la cle dans l'environnement du
# serveur. N'importe quel visiteur choisissait alors le moteur, laissait la cle
# vide, remplacait l'adresse par la sienne, et recevait la cle du serveur dans
# l'en-tete `x-api-key`. Constate ici meme, avec un serveur de capture.
#
# La cle SAISIE, elle, part toujours ou son proprietaire l'envoie : c'est la
# sienne, et le pouvoir de deplacer l'adresse reste entier. Seul le repli
# ambiant est retire quand l'adresse quitte celle du fournisseur.
hstat_ai_key <- function(engine = "claude", explicit = NULL, url = NULL) {
  k <- if (is.null(explicit)) "" else trimws(as.character(explicit)[1])
  if (is.na(k) || !nzchar(k)) {
    env <- hstat_ai_fournisseur(engine)$cle_env
    if (nzchar(env) && hstat_ai_url_attendue(engine, url))
      k <- trimws(Sys.getenv(env, ""))
  }
  if (is.na(k)) "" else k
}

# L'adresse retenue est-elle celle que le fournisseur declare ? La comparaison
# porte sur l'adresse RESOLUE des deux cotes, pour qu'une barre finale ou un
# champ vide ne comptent pas comme un changement -- sinon le repli sauterait
# dans le cas ordinaire, qui est justement celui qu'il sert.
hstat_ai_url_attendue <- function(engine = "claude", url = NULL) {
  identical(hstat_ai_url(engine, url),
            hstat_ai_url(engine, NULL))
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
                  paste(miss, collapse = tr(" et ")),
                  paste(sprintf('"%s"', gsub("[{}]", "", miss)), collapse = ", "))))
  }

  # Un service en ligne exige une cle. On NOMME le service, sa variable
  # d'environnement et l'endroit ou l'obtenir : « cle absente » tout court
  # laisse l'utilisateur sans geste a faire.
  if (nzchar(f$cle_env)) {
    if (!nzchar(hstat_ai_key(engine, api_key, url)))
      return(list(ok = FALSE,
                  message = trf("%s indisponible : renseignez une clé d'API (champ ci-dessous ou variable %s). Clé à créer sur %s. Ce service est payant et nécessite une connexion ; la thématisation automatique, elle, est gratuite et hors ligne.",
                                f$label, f$cle_env, f$cle_url %||% tr("le site du fournisseur"))))
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
                              f$aide %||% tr("Vérifiez l'adresse ci-dessous."))))
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
                error = hstat_ai_err_http(httr::status_code(res), msg, f$label)))
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
  key <- hstat_ai_key("gemini", api_key, url)
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
                error = hstat_ai_err_http(httr::status_code(res), msg, f$label)))
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
  key <- hstat_ai_key("claude", api_key, url)
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
                error = trf("Échec de la connexion : %s", conditionMessage(res))))

  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (httr::status_code(res) >= 300) {
    msg <- if (!is.null(parsed$error$message)) parsed$error$message else raw
    return(list(ok = FALSE, text = "",
                error = hstat_ai_err_http(httr::status_code(res), msg,
                                          hstat_ai_fournisseur("claude")$label)))
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
              f$cle_url %||% tr("le site du fournisseur")))),
    shiny::textInput(id("url"), "Adresse du service", value = f$url),
    # CE QUI SORT DE LA MACHINE SE DIT, ET AU MOMENT DU CHOIX.
    #
    # Le moteur « auto » annonce en vert qu'il ne transmet rien. Les autres ne
    # disaient rien du tout : l'utilisateur lisait « service payant, en ligne »
    # -- ce qui parle du COUT -- et devait deviner ce qui part. L'asymetrie
    # etait le probleme : le cas rassurant etait nomme, le cas engageant non.
    #
    # Le texte vaut pour les deux adresses possibles, celle d'un tiers comme
    # celle d'un serveur local, parce que le champ juste au-dessus est
    # librement modifiable : une note qui ne parlerait que du tiers mentirait
    # des que l'adresse change.
    shiny::div(
      style = "background:#fef9e7;border-left:3px solid #f39c12;padding:8px 12px;font-size:12px;margin-bottom:10px;",
      shiny::icon("triangle-exclamation"), " ",
      shiny::strong(tr("Ce qui est transmis à cette adresse :")), " ",
      tr(paste("les résultats de l'analyse en cours (au plus 25 lignes par tableau),",
               "les noms de vos variables et de leurs modalités, et le contexte",
               "d'étude que vous saisissez.")), " ",
      shiny::strong(tr("Votre fichier de données n'est jamais envoyé.")), " ",
      tr("Sur une adresse locale (127.0.0.1), rien ne quitte la machine.")),
    shiny::textInput(id("model"), "Modèle",
                     value = f$modele,
                     placeholder = paste("ex.", .hstat_ai_ex(f))))
}

# LE BOUTON « TESTER LA CONNEXION » N'EST PLUS POSE ICI. Il l'etait sous le nom
# `<prefixe>ping`, et l'onglet d'aide a la decision -- le seul appelant a
# prefixe vide -- a disparu : `input$ping` n'etait plus lu par personne.
#
# Le retirer ferme aussi un defaut latent que l'orphelin cachait. L'atelier de
# codage declare DEJA son propre `ai_ping` dans son interface statique ; le
# helper, appele avec le prefixe « ai_ », en posait un SECOND du meme
# identifiant. Deux elements pour un meme id, c'est un HTML invalide et un
# `updateActionButton()` qui n'en atteindrait qu'un.

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
                        hstat_ai_key(engine, api_key, url), max_tokens, json, tmo,
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

