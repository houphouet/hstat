/* ===========================================================================
   HStat - persistance de la session
   ---------------------------------------------------------------------------
   Le probleme : quand l'ordinateur se verrouille ou se met en veille, le
   navigateur suspend ses connexions. Le websocket de Shiny tombe, et
   l'application se recouvre du voile gris « Disconnected from the server ».
   L'utilisateur a alors PERDU sa session : ses donnees chargees, ses filtres,
   ses analyses. Rien ne l'a prevenu, et rien ne lui permet de revenir.

   La regle voulue : SEUL L'UTILISATEUR FERME L'APPLICATION. Une veille, un
   verrouillage, une coupure reseau passagere ne doivent pas y suffire.

   Trois mecanismes, dans cet ordre d'importance :

   1. RECONNEXION. Le serveur autorise la reprise (`session$allowReconnect`) ;
      ici on la declenche, on la retente, et on DIT ce qui se passe. Le voile
      gris de Shiny est masque au profit d'un bandeau francais qui annonce
      l'etat et laisse la main.

   2. MAINTIEN. Un signal periodique empeche les coupures pour inactivite —
      celles des serveurs mandataires comme celles des hebergeurs. Il ne
      declenche aucun calcul : c'est une simple valeur d'entree.

   3. GARDE-FOU A LA FERMETURE. Fermer l'onglet par megarde perd le travail au
      meme titre qu'une deconnexion. Le navigateur demande confirmation, mais
      uniquement quand des donnees sont chargees : une confirmation qui
      s'affiche toujours n'est plus lue.

   Ecrit en JavaScript nu, sans dependance : ce fichier doit fonctionner meme
   quand le reseau est coupe, c'est precisement son objet.
   =========================================================================== */

(function () {
  "use strict";

  var ID_BANDEAU   = "hstat-bandeau-session";
  var DELAIS       = [1000, 2000, 4000, 8000, 15000, 30000]; // recul progressif
  var PING_MS      = 25000;

  var essai        = 0;
  var minuterie    = null;
  var deconnecte   = false;
  var travailEnCours = false;

  // ---------------------------------------------------------------- bandeau
  function bandeau() {
    var d = document.getElementById(ID_BANDEAU);
    if (d) return d;
    d = document.createElement("div");
    d.id = ID_BANDEAU;
    d.setAttribute("role", "status");
    d.setAttribute("aria-live", "polite");
    d.style.cssText = [
      "position:fixed", "left:0", "right:0", "top:0", "z-index:100000",
      "display:none", "padding:12px 18px", "font-size:14px",
      "font-family:inherit", "color:#fff", "background:#b9770e",
      "box-shadow:0 2px 10px rgba(0,0,0,.25)", "text-align:center"
    ].join(";");
    document.body.appendChild(d);
    return d;
  }

  function afficher(html, couleur) {
    var d = bandeau();
    d.style.background = couleur;
    d.innerHTML = html;
    d.style.display = "block";
    var b = d.querySelector("[data-hstat-action='reconnecter']");
    if (b) b.onclick = function () { essai = 0; reconnecter(); };
  }

  function masquer() {
    var d = document.getElementById(ID_BANDEAU);
    if (d) d.style.display = "none";
  }

  function boutonHtml(libelle) {
    return "<button data-hstat-action='reconnecter' style=\"margin-left:14px;" +
           "padding:3px 12px;border:1px solid rgba(255,255,255,.7);" +
           "border-radius:4px;background:rgba(255,255,255,.15);color:#fff;" +
           "cursor:pointer;font-size:13px;\">" + libelle + "</button>";
  }

  // ------------------------------------------------------------ reconnexion
  function reconnecter() {
    if (!deconnecte) return;
    if (minuterie) { clearTimeout(minuterie); minuterie = null; }

    var delai = DELAIS[Math.min(essai, DELAIS.length - 1)];
    essai += 1;

    afficher("<b>Connexion interrompue</b> &mdash; l'application est toujours " +
             "ouverte, votre travail n'est pas perdu. Tentative de reprise en " +
             "cours (essai " + essai + ")." + boutonHtml("Reprendre maintenant"),
             "#b9770e");

    try {
      // Shiny sait reprendre la session laissee ouverte cote serveur.
      if (window.Shiny && Shiny.shinyapp &&
          typeof Shiny.shinyapp.reconnect === "function") {
        Shiny.shinyapp.reconnect();
      }
    } catch (e) { /* la tentative suivante s'en chargera */ }

    minuterie = setTimeout(function () {
      if (deconnecte) reconnecter();
    }, delai);
  }

  // ------------------------------------------------------------- evenements
  // Les evenements de Shiny (shiny:disconnected, shiny:connected) sont emis
  // par jQuery, PAS par le DOM natif : `document.addEventListener` ne les voit
  // jamais. Il faut passer par `$(document).on()`. Piege verifie a l'ecran —
  // le bandeau ne s'affichait pas du tout avant cette correction.
  function surEvenementShiny(nom, fn) {
    if (window.jQuery) window.jQuery(document).on(nom, fn);
    else document.addEventListener(nom, fn);
  }

  surEvenementShiny("shiny:disconnected", function () {
    deconnecte = true;
    essai = 0;
    reconnecter();
  });

  surEvenementShiny("shiny:connected", function () {
    var reprise = deconnecte;
    deconnecte = false;
    essai = 0;
    if (minuterie) { clearTimeout(minuterie); minuterie = null; }
    if (!reprise) { masquer(); return; }
    afficher("<b>Connexion retablie.</b> Vous pouvez reprendre votre travail.",
             "#1e8449");
    setTimeout(masquer, 5000);
  });

  // Le cas qui motive tout ce fichier : au deverrouillage de l'ordinateur, la
  // page redevient visible. On ne laisse pas courir le calendrier des essais,
  // on retente immediatement — l'utilisateur est la, il regarde.
  document.addEventListener("visibilitychange", function () {
    if (!document.hidden && deconnecte) { essai = 0; reconnecter(); }
  });
  window.addEventListener("focus", function () {
    if (deconnecte) { essai = 0; reconnecter(); }
  });
  window.addEventListener("online", function () {
    if (deconnecte) { essai = 0; reconnecter(); }
  });

  // ---------------------------------------------------------------- maintien
  setInterval(function () {
    try {
      if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected &&
          Shiny.shinyapp.isConnected() && Shiny.setInputValue) {
        // priority:"event" : la valeur n'est pas conservee et ne redeclenche
        // aucun observateur en aval. Un signal, pas une entree.
        Shiny.setInputValue("hstat_keepalive", Date.now(), { priority: "event" });
      }
    } catch (e) { /* sans consequence */ }
  }, PING_MS);

  // ------------------------------------------------- garde-fou a la fermeture
  // Ce fichier est charge dans l'en-tete, donc parfois AVANT que Shiny ne soit
  // pret : `Shiny.addCustomMessageHandler` n'existe pas encore et le
  // gestionnaire serait perdu en silence — le garde-fou ne s'armait jamais.
  // On reessaie jusqu'a ce que Shiny reponde.
  function brancherMessage() {
    if (!(window.Shiny && Shiny.addCustomMessageHandler)) return false;
    Shiny.addCustomMessageHandler("hstat_travail", function (m) {
      travailEnCours = !!(m && m.actif);
    });
    return true;
  }
  if (!brancherMessage()) {
    var attente = setInterval(function () {
      if (brancherMessage()) clearInterval(attente);
    }, 200);
  }

  window.addEventListener("beforeunload", function (e) {
    if (!travailEnCours) return undefined;
    // Les navigateurs imposent leur propre libelle ; seul le fait de renvoyer
    // une valeur declenche la demande de confirmation.
    e.preventDefault();
    e.returnValue = "";
    return "";
  });
})();
