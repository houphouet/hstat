/* ===========================================================================
   HStat - separateur decimal des fichiers telecharges
   ---------------------------------------------------------------------------
   AVANT chaque telechargement, une fenetre demande : point (1.5) ou virgule
   (1,5) ? Le choix vaut pour tous les fichiers produits -- tableaux CSV, textes
   des classeurs Excel, et nombres ecrits dans les figures.

   Deux chemins de telechargement, et ils ne se traitent pas pareil :

   1. LES LIENS SHINY (`a.shiny-download-link`) : le fichier est ecrit par le
      serveur, qui lit le choix dans la session. Le choix part par le websocket,
      le fichier par une requete HTTP : rien ne garantit l'ordre d'arrivee. On
      attend donc l'accuse du serveur (`hstat-decimale-ok`) avant de lancer le
      telechargement, sans quoi il porterait le separateur d'AVANT.

   2. LES BOUTONS DE DataTables (`.buttons-csv`, `.buttons-excel`) : le fichier
      est ecrit dans le navigateur, sans repasser par le serveur. La conversion
      se fait ici, cellule par cellule (`hstatDecimaleCellule`), selon la meme
      regle que `hstat_decimale_texte()` cote R.

   La fenetre est le SEUL endroit ou le choix se fait : ni bandeau, ni
   selecteur dans les blocs d'export. Elle revient donc a chaque
   telechargement, pre-cochee sur le dernier choix (localStorage) : un clic, ou
   Entree, suffit a confirmer.
   =========================================================================== */

(function () {
  "use strict";

  var CLE_CHOIX    = "hstat.decimale";
  var ATTENTE_MS   = 3000;

  function lire(stock, cle, defaut) {
    try { var v = window[stock].getItem(cle); return v === null ? defaut : v; }
    catch (e) { return defaut; }
  }
  function ecrire(stock, cle, v) {
    try { window[stock].setItem(cle, v); } catch (e) { /* stockage refuse */ }
  }

  var choix   = lire("localStorage", CLE_CHOIX, ".") === "," ? "," : ".";
  var serveur = null;     // dernier choix accuse par le serveur
  var suite   = null;     // telechargement en attente de l'accuse

  // ------------------------------------------------------------ conversion
  // Un nombre ECRIT dans un texte. Meme regle que HSTAT_DECIMALE_MOTIF (R) :
  // precede d'une lettre, d'un chiffre, d'un `_` ou d'un point, c'est un
  // identifiant (« T1.2 ») ; suivi d'un second « .chiffre », une date ou une
  // version (« 04.08.2026 »). Ecrit sans lookbehind : un navigateur qui ne le
  // connait pas refuserait le fichier ENTIER a l'analyse.
  var LETTRE;
  try { LETTRE = new RegExp("[\\p{L}\\p{N}_.]", "u"); }
  catch (e) { LETTRE = /[A-Za-z0-9_.À-ɏ]/; }

  function convertirTexte(s) {
    return s.replace(/(\d+)\.(\d+)/g, function (m, a, b, pos, tout) {
      var avant = pos > 0 ? tout.charAt(pos - 1) : "";
      var apres = tout.substr(pos + m.length, 2);
      if (avant && LETTRE.test(avant)) return m;
      if (/^\d/.test(apres) || /^\.\d/.test(apres)) return m;
      return a + "," + b;
    });
  }

  var NOMBRE_NU = /^\s*[-+]?\d+(\.\d+)?([eE][-+]?\d+)?\s*$/;

  // Le mode "excel" ne fait plus d'exception : un nombre laisse NU dans un
  // classeur s'affiche selon les reglages regionaux d'Excel, donc avec un point
  // sur un poste regle au point, quel que soit le choix fait ici -- c'est le
  // defaut signale. Avec la virgule, il part ecrit avec elle, comme dans les
  // classeurs du serveur (hstat_decimale_classeur, R/utils.R).
  window.hstatDecimaleCellule = function (d, mode) {
    if (choix !== "," || d === null || d === undefined) return d;
    if (typeof d === "number") return convertirTexte(String(d));
    if (typeof d !== "string") return d;
    if (NOMBRE_NU.test(d)) return d.replace(".", ",");
    return convertirTexte(d);
  };
  window.hstatDecimaleTexte = convertirTexte;
  window.hstatDecimale = function () { return choix; };

  // -------------------------------------------------------------- serveur
  function envoyer() {
    if (window.Shiny && typeof window.Shiny.setInputValue === "function")
      window.Shiny.setInputValue("hstat_decimale", choix, { priority: "event" });
  }

  function brancher() {
    if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === "function") {
      window.Shiny.addCustomMessageHandler("hstat-decimale-ok", function (v) {
        serveur = v === "," ? "," : ".";
        if (suite && serveur === choix) { var s = suite; suite = null; s(); }
      });
    } else {
      setTimeout(brancher, 200);
    }
  }
  brancher();

  if (window.jQuery) {
    // Les evenements de Shiny sont emis par jQuery : `addEventListener` ne les
    // voit jamais (constate sur le bandeau de session).
    window.jQuery(document).on("shiny:connected", function () {
      serveur = null;
      envoyer();
    });
  }

  window.hstatSetDecimale = function (v) {
    choix = v === "," ? "," : ".";
    ecrire("localStorage", CLE_CHOIX, choix);
    envoyer();
  };

  // Lance `faire` une fois le serveur au courant du choix. Au-dela du delai, on
  // telecharge quand meme : un fichier au mauvais separateur vaut mieux qu'un
  // clic sans effet.
  function quandServeurPret(faire) {
    if (serveur === choix || !window.Shiny) { faire(); return; }
    suite = faire;
    envoyer();
    setTimeout(function () {
      if (suite === faire) { suite = null; faire(); }
    }, ATTENTE_MS);
  }

  // -------------------------------------------------------------- fenetre
  function fermer(fond) {
    if (fond && fond.parentNode) fond.parentNode.removeChild(fond);
    document.removeEventListener("keydown", fond.__echap, true);
  }

  function option(nom, valeur, libelle, exemple) {
    var l = document.createElement("label");
    l.className = "hstat-dec-option";
    var r = document.createElement("input");
    r.type = "radio"; r.name = nom; r.value = valeur;
    r.checked = valeur === choix;
    var t = document.createElement("span");
    t.textContent = libelle;
    var x = document.createElement("span");
    x.className = "hstat-dec-exemple";
    x.setAttribute("data-hstat-notranslate", "");
    x.textContent = exemple;
    l.appendChild(r); l.appendChild(t); l.appendChild(x);
    return l;
  }

  function demander(lancer) {
    var fond = document.createElement("div");
    fond.className = "hstat-dec-fond";
    var boite = document.createElement("div");
    boite.className = "hstat-dec-boite";
    boite.setAttribute("role", "dialog");
    boite.setAttribute("aria-modal", "true");
    boite.setAttribute("aria-labelledby", "hstatDecTitre");

    var h = document.createElement("h4");
    h.id = "hstatDecTitre";
    h.textContent = "Séparateur décimal";
    var p = document.createElement("p");
    p.className = "hstat-dec-aide";
    p.textContent = "Il s'applique aux nombres des tableaux et des figures téléchargés.";

    var nom = "hstatDec" + Date.now();
    var opts = document.createElement("div");
    opts.className = "hstat-dec-options";
    opts.appendChild(option(nom, ",", "Virgule décimale", "1,5"));
    opts.appendChild(option(nom, ".", "Point décimal", "1.5"));

    var note = document.createElement("p");
    note.className = "hstat-dec-note";
    note.textContent = "Avec la virgule, les colonnes d'un CSV sont séparées par un point-virgule, et les nombres d'un classeur Excel sont écrits avec la virgule quels que soient les réglages d'Excel. Ils y entrent comme du texte : « Convertir en nombre » les rend calculables.";

    var actions = document.createElement("div");
    actions.className = "hstat-dec-actions";
    var annuler = document.createElement("button");
    annuler.type = "button"; annuler.className = "btn btn-default";
    annuler.textContent = "Annuler";
    var ok = document.createElement("button");
    ok.type = "button"; ok.className = "btn btn-primary";
    ok.textContent = "Télécharger";
    actions.appendChild(annuler); actions.appendChild(ok);

    boite.appendChild(h); boite.appendChild(p); boite.appendChild(opts);
    boite.appendChild(note); boite.appendChild(actions);
    fond.appendChild(boite);
    document.body.appendChild(fond);

    annuler.onclick = function () { fermer(fond); };
    fond.addEventListener("click", function (ev) { if (ev.target === fond) fermer(fond); });
    fond.__echap = function (ev) {
      if (ev.key === "Escape") fermer(fond);
      else if (ev.key === "Enter" && document.activeElement !== annuler) {
        ev.preventDefault(); ok.onclick();
      }
    };
    document.addEventListener("keydown", fond.__echap, true);

    ok.onclick = function () {
      var r = boite.querySelector("input[type=radio]:checked");
      window.hstatSetDecimale(r ? r.value : choix);
      fermer(fond);
      lancer();
    };
    ok.focus();
  }

  // ------------------------------------------------------- telechargement
  // UN LIEN SHINY NE TELECHARGE PAS DIRECTEMENT. Le navigateur enregistre ce
  // que le serveur renvoie, quoi que ce soit : une session expiree (veille,
  // serveur redemarre) repond « 404 Not Found » en HTML, et l'utilisateur
  // recevait `dl50-gDl.htm` -- le nom du lien, l'extension de la page
  // d'erreur -- en croyant tenir son graphique. La fenetre Shiny de RStudio
  // ignore par ailleurs le nom de fichier envoye par le serveur.
  //
  // Le fichier est donc demande ICI (fetch), et n'est enregistre que si le
  // serveur l'a bien produit : reponse 200 ET nom de fichier annonce. Sinon on
  // dit pourquoi, au lieu d'enregistrer une page d'erreur.
  function nomAnnonce(res) {
    var cd = res.headers.get("Content-Disposition") || "";
    var m = /filename\*\s*=\s*[^']*''([^;]+)/i.exec(cd);
    if (m) { try { return decodeURIComponent(m[1].trim().replace(/^"|"$/g, "")); } catch (e) {} }
    m = /filename\s*=\s*"((?:[^"\\]|\\.)*)"/i.exec(cd) || /filename\s*=\s*([^;]+)/i.exec(cd);
    if (m) return m[1].replace(/\\(.)/g, "$1").trim();
    // Un nom non ASCII passe par une redirection : il est alors le dernier
    // segment de l'adresse (`.../download/<sortie>/<nom>`), et l'en-tete ne
    // dit que « attachment ».
    if (/^attachment/i.test(cd)) {
      var seg = (res.url || "").split("?")[0].split("/");
      if (seg.length > 1 && seg[seg.length - 2] !== "download") {
        try { return decodeURIComponent(seg[seg.length - 1]); } catch (e) {}
      }
    }
    return null;
  }

  function signaler(msg) {
    var n = window.Shiny && window.Shiny.notifications;
    if (n && typeof n.show === "function") {
      var t = document.createElement("span");
      t.textContent = msg;               // echappe : `html` est insere tel quel
      n.show({ html: t.outerHTML, type: "error", duration: 12000 });
      return;
    }
    window.alert(msg);
  }

  function enregistrer(blob, nom) {
    var url = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = url; a.download = nom; a.style.display = "none";
    document.body.appendChild(a);
    a.click();
    setTimeout(function () { URL.revokeObjectURL(url); a.parentNode && a.parentNode.removeChild(a); }, 60000);
  }

  var MSG_SESSION = "Le téléchargement a échoué : la session du serveur n'existe plus (veille de l'ordinateur ou serveur redémarré). Rechargez la page, puis relancez l'analyse.";
  var MSG_CONNEXION = "Le téléchargement a échoué : la connexion au serveur est interrompue. Attendez la reconnexion ou rechargez la page.";
  var MSG_SERVEUR = "Le téléchargement a échoué : le serveur n'a pas pu produire le fichier. Vérifiez que l'analyse a bien été lancée.";

  function telecharger(el) {
    var href = el.getAttribute("href") || "";
    if (!window.fetch || !window.Blob || !window.URL || !URL.createObjectURL) {
      relancer(el); return;
    }
    // Un lien encore vide n'a pas recu son adresse : la sortie n'est pas
    // rendue, ou la session n'est pas (re)connectee.
    if (!href || href === "#") { signaler(MSG_CONNEXION); return; }
    var url;
    try { url = new URL(href, document.baseURI).href; } catch (e) { relancer(el); return; }
    el.classList.add("hstat-dl-encours");
    fetch(url, { credentials: "same-origin" }).then(function (res) {
      var nom = res.ok ? nomAnnonce(res) : null;
      if (!nom) {
        signaler(res.status === 404 ? MSG_SESSION : MSG_SERVEUR);
        return null;
      }
      return res.blob().then(function (b) { enregistrer(b, nom); });
    }).catch(function () {
      signaler(MSG_CONNEXION);
    }).then(function () {
      el.classList.remove("hstat-dl-encours");
    });
  }
  window.hstatTelecharger = telecharger;

  // ------------------------------------------------------------ interception
  function relancer(el) { el.__hstatDecOk = true; el.click(); }

  document.addEventListener("click", function (ev) {
    var cible = ev.target && ev.target.closest ? ev.target : null;
    if (!cible) return;
    var lien = cible.closest("a.shiny-download-link");
    var bouton = lien ? null : cible.closest(".buttons-csv, .buttons-excel");
    var el = lien || bouton;
    if (!el) return;
    if (el.__hstatDecOk) { el.__hstatDecOk = false; return; }
    if (el.classList.contains("disabled") || el.hasAttribute("disabled")) return;

    // Le lien Shiny doit attendre que le serveur connaisse le choix ; le bouton
    // DataTables lit `choix` au clic, il n'attend rien.
    var lancer = lien
      ? function () { quandServeurPret(function () { telecharger(el); }); }
      : function () { relancer(el); };

    ev.preventDefault();
    ev.stopPropagation();
    ev.stopImmediatePropagation();
    demander(lancer);
  }, true);
})();
