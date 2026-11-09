// HStat - bascule francais / anglais
// L'application compte ~9 700 chaines destinees a l'utilisateur, reparties sur
// 24 fichiers R.

(function () {
  "use strict";

  var DICT = window.HSTAT_I18N || {};          // francais -> anglais

  // LA RECHERCHE NE TRAVERSE PAS LA CHAINE DE PROTOTYPES.
  var aCle = Object.prototype.hasOwnProperty;
  function lire(d, k) { return aCle.call(d, k) ? d[k] : undefined; }
  var dictPret = !!window.HSTAT_I18N;          // deja incorpore ? (repli)
  var CLE_STOCKAGE = "hstat-langue";
  var langue = "fr";
  var enCours = false;

  // Balises dont le contenu n'est pas du texte d'interface.
  var IGNORE = { SCRIPT: 1, STYLE: 1, TEXTAREA: 1, CODE: 1, PRE: 1, SVG: 1 };
  var ATTRS = ["placeholder", "title", "aria-label", "alt"];

  // LES DONNEES DE L'UTILISATEUR NE SE TRADUISENT PAS.
  var LONGUEUR_CELLULE = 25;

  // LES TERMES DU FICHIER DE L'UTILISATEUR NE SE TRADUISENT JAMAIS.
  var TERMES = Object.create(null);

  function estDonnee(net) { return TERMES[net] === true; }

  function dansCellule(el) {
    while (el && el.nodeType === 1) {
      if (el.tagName === "TD") return true;
      if (el.tagName === "TABLE" || el.tagName === "BODY") return false;
      el = el.parentNode;
    }
    return false;
  }

  function ignorable(el) {
    while (el) {
      if (el.nodeType === 1) {
        if (IGNORE[el.tagName]) return true;
        if (el.hasAttribute && el.hasAttribute("data-hstat-notranslate")) return true;
      }
      el = el.parentNode;
    }
    return false;
  }

  // UN ATTRIBUT N'EST PAS DU CONTENU. La liste IGNORE protege ce qu'une balise
  // CONTIENT -- le texte tape dans une zone de saisie, un extrait de code --
  // mais le `placeholder` d'un <textarea> est un LIBELLE, pose par
  // l'application.
  function ignorableAttr(el) {
    while (el) {
      if (el.nodeType === 1 && el.hasAttribute &&
          el.hasAttribute("data-hstat-notranslate")) return true;
      el = el.parentNode;
    }
    return false;
  }

  // phrases coupees par une balise
  // LE TRADUCTEUR REMPLACE DES NOEUDS DE TEXTE.
  var DICT_HTML = null;                 // innerHTML normalise -> traduction
  var HTML_MAX  = 0;

  // Meme normalisation des blancs que pour les noeuds de texte, et pour la
  // meme raison : Shiny ecrit chaque enfant d'une balise sur sa propre ligne.
  function blancs(x) { return String(x).replace(/\s+/g, " ").trim(); }

  function indexerHtml() {
    DICT_HTML = Object.create(null);
    var tmp;
    try { tmp = document.createElement("div"); } catch (e) { return; }
    if (!tmp) return;
    for (var cle in DICT) {
      if (cle.indexOf("<") < 0) continue;
      var norm;
      try { tmp.innerHTML = cle; norm = blancs(tmp.innerHTML); }
      catch (e) { continue; }
      if (!norm) continue;
      DICT_HTML[norm] = DICT[cle];
      if (norm.length > HTML_MAX) HTML_MAX = norm.length;
    }
  }

  function traduireHtml(racine) {
    if (DICT_HTML === null) indexerHtml();
    if (!HTML_MAX) return;
    var els = racine.querySelectorAll ? racine.querySelectorAll("*") : [];
    for (var i = 0; i < els.length; i++) {
      var el = els[i];
      if (langue === "en") {
        if (el.__hstatFrHtml !== undefined) continue;   // deja remplace
        if (ignorable(el)) continue;
        // Un element sans enfant ELEMENT ne porte qu'un noeud de texte : la
        // passe suivante s'en charge.
        if (!el.children || !el.children.length) continue;
        var brut = el.innerHTML;
        if (!brut || brut.length > HTML_MAX * 2 + 64) continue;
        var cible = DICT_HTML[blancs(brut)];
        if (cible === undefined) continue;
        el.__hstatFrHtml = brut;
        el.innerHTML = cible;
      } else if (el.__hstatFrHtml !== undefined) {
        // Le retour au francais restitue le balisage d'origine EXACTEMENT : les
        // noeuds de texte anglais sont remplaces d'un bloc, ils ne portent donc
        // aucun __hstatFr a defaire.
        el.innerHTML = el.__hstatFrHtml;
        delete el.__hstatFrHtml;
      }
    }
  }

  // noeuds de texte
  function traduireTexte(racine) {
    var w = document.createTreeWalker(racine, NodeFilter.SHOW_TEXT, null, false);
    var n, lot = [];
    while ((n = w.nextNode())) lot.push(n);
    for (var i = 0; i < lot.length; i++) {
      var t = lot[i];
      if (ignorable(t.parentNode)) continue;
      var brut = t.nodeValue;
      // LES BLANCS INTERNES SONT NORMALISES AVANT LA RECHERCHE.
      var net = brut.replace(/\s+/g, " ").trim();
      if (!net) continue;

      if (langue === "en") {
        if (t.__hstatFr !== undefined) continue;   // deja traduit
        var cible = lire(DICT, net);
        if (cible === undefined) continue;
        // Protection des donnees de l'utilisateur, deux barrieres : le terme
        // vient-il du fichier charge (exact), et la regle de longueur en cellule
        // (heuristique, pour ce que le serveur n'a pas pu annoncer).
        if (estDonnee(net)) continue;
        if (net.length <= LONGUEUR_CELLULE && dansCellule(t.parentNode)) continue;
        t.__hstatFr = brut;
        // Les espaces qui ENTOURENT le texte sont conserves : les retirer
        // collerait un libelle a l'icone qui le precede.
        t.nodeValue = brut.match(/^\s*/)[0] + cible + brut.match(/\s*$/)[0];
      } else if (t.__hstatFr !== undefined) {
        t.nodeValue = t.__hstatFr;
        delete t.__hstatFr;
      }
    }
  }

  // attributs
  function traduireAttributs(racine) {
    var els = racine.querySelectorAll ? racine.querySelectorAll("*") : [];
    for (var i = 0; i < els.length; i++) {
      var el = els[i];
      if (ignorableAttr(el)) continue;
      for (var a = 0; a < ATTRS.length; a++) {
        var nom = ATTRS[a];
        if (!el.hasAttribute(nom)) continue;
        var memo = "__hstatAttr_" + nom;
        if (langue === "en") {
          if (el[memo] !== undefined) continue;
          var v = el.getAttribute(nom).trim();
          var cible = lire(DICT, v);
          if (cible === undefined || estDonnee(v)) continue;
          el[memo] = el.getAttribute(nom);
          el.setAttribute(nom, cible);
        } else if (el[memo] !== undefined) {
          el.setAttribute(nom, el[memo]);
          delete el[memo];
        }
      }
    }
  }

  function appliquer(racine) {
    racine = racine || document.body;
    if (!racine || enCours) return;
    // L'observateur est suspendu pendant l'application : sans cela, chaque
    // remplacement declencherait une nouvelle passe, indefiniment.
    enCours = true;
    try {
      // L'ORDRE COMPTE : le remplacement en bloc d'abord. Les noeuds de texte
      // qu'il installe sont deja anglais, donc invisibles pour la passe suivante
      // (les cles du dictionnaire sont francaises).
      traduireHtml(racine);
      traduireTexte(racine);
      traduireAttributs(racine);
    } catch (e) { /* une page a moitie traduite vaut mieux qu'une page cassee */ }
    enCours = false;
  }

  // contenu rendu plus tard
  // Notifications, tableaux, sorties Shiny : tout arrive apres le chargement.
  var enAttente = null;
  function planifier(cible) {
    if (langue !== "en") return;
    if (enAttente) clearTimeout(enAttente);
    enAttente = setTimeout(function () { appliquer(cible || document.body); }, 60);
  }

  if (window.MutationObserver) {
    new MutationObserver(function (mutations) {
      if (enCours || langue !== "en") return;
      for (var i = 0; i < mutations.length; i++)
        if (mutations[i].addedNodes.length) { planifier(); return; }
    }).observe(document.documentElement, { childList: true, subtree: true });
  }

  // bascule
  // LE DICTIONNAIRE SE CHARGE A LA DEMANDE, au premier passage en anglais.
  var enAttenteDict = null;
  function chargerDictionnaire(suite) {
    if (dictPret) return suite(true);
    if (window.HSTAT_I18N) {
      DICT = window.HSTAT_I18N; dictPret = true; DICT_HTML = null;
      return suite(true);
    }
    var src = window.HSTAT_I18N_SRC;
    if (!src) { dictPret = true; return suite(false); }
    if (enAttenteDict) { enAttenteDict.push(suite); return; }
    enAttenteDict = [suite];
    var fin = function (ok) {
      dictPret = true;
      DICT = window.HSTAT_I18N || {};
      DICT_HTML = null;            // l'index HTML se rebatit sur le dictionnaire arrive
      var f = enAttenteDict; enAttenteDict = null;
      for (var i = 0; i < f.length; i++) f[i](ok);
    };
    var s = document.createElement("script");
    s.src = src;
    s.onload  = function () { fin(true); };
    // UNE TRADUCTION QUI N'ARRIVE PAS NE DOIT PAS FIGER L'INTERFACE : on reste
    // sur le francais, qui marche, plutot que de laisser la bascule sans effet
    // et sans un mot.
    s.onerror = function () { fin(false); };
    document.head.appendChild(s);
  }

  function definirLangue(l) {
    l = (l === "en") ? "en" : "fr";
    if (l === langue) return;
    // Revenir au francais ne demande AUCUN dictionnaire : le texte d'origine
    // est conserve sur les noeuds (`__hstatFr`).
    if (l === "en") { chargerDictionnaire(function () { poserLangue(l); }); return; }
    poserLangue(l);
  }

  function poserLangue(l) {
    langue = l;
    try { localStorage.setItem(CLE_STOCKAGE, l); } catch (e) {}
    document.documentElement.setAttribute("lang", l);
    marquerSegment(l);
    // Le SERVEUR doit connaitre la langue : les messages d'erreur et les
    // interpretations sont composes en R, phrase par phrase, et n'existent donc
    // pas comme chaine entiere dans le dictionnaire du navigateur.
    signalerAuServeur(l);
    // Repasser en francais doit restituer le texte d'origine : on parcourt
    // donc la page dans les deux sens de bascule.
    enCours = true;
    try {
      traduireHtml(document.body);
      traduireTexte(document.body);
      traduireAttributs(document.body);
    } catch (e) {} finally { enCours = false; }
  }

  // Shiny n'est pas forcement pret quand ce fichier s'execute (il est charge
  // dans l'en-tete) : on reessaie jusqu'a ce qu'il reponde, sinon la langue
  // choisie avant la connexion ne parviendrait jamais au serveur.
  function signalerAuServeur(l) {
    var essais = 0;
    (function envoyer() {
      if (window.Shiny && Shiny.setInputValue) {
        try { Shiny.setInputValue("hstat_langue", l, { priority: "event" }); }
        catch (e) {}
        return;
      }
      if (++essais < 100) setTimeout(envoyer, 200);
    })();
  }

  window.hstatSetLangue = definirLangue;

  // Reception de la liste des termes du fichier. Shiny n'est pas forcement
  // pret quand ce fichier s'execute : on reessaie, sinon le gestionnaire serait
  // perdu en silence et les donnees ne seraient plus protegees.
  function recevoirTermes() {
    if (!(window.Shiny && Shiny.addCustomMessageHandler)) {
      setTimeout(recevoirTermes, 200);
      return;
    }
    Shiny.addCustomMessageHandler("hstat-termes-donnees", function (json) {
      var liste;
      try { liste = (typeof json === "string") ? JSON.parse(json) : json; }
      catch (e) { return; }
      if (!liste || !liste.length) { TERMES = Object.create(null); }
      else {
        var t = Object.create(null);
        for (var i = 0; i < liste.length; i++) {
          var s = String(liste[i]).trim();
          if (s) t[s] = true;
        }
        TERMES = t;
      }
      // Un fichier charge ALORS QUE la page est deja en anglais aurait pu voir
      // ses valeurs traduites avant l'arrivee de cette liste : on defait puis on
      // refait la passe, sinon la protection n'agirait qu'a partir du prochain
      // rendu.
      if (langue === "en") {
        enCours = true;
        try {
          langue = "fr"; traduireTexte(document.body); traduireAttributs(document.body);
          langue = "en"; traduireTexte(document.body); traduireAttributs(document.body);
        } catch (e) { langue = "en"; } finally { enCours = false; }
      }
    });
  }
  recevoirTermes();

  // Le temoin visuel doit suivre la langue reellement appliquee.
  function marquerSegment(l) {
    var fr = document.getElementById("hstatLangFr");
    var en = document.getElementById("hstatLangEn");
    if (!fr || !en) return;
    (l === "en" ? en : fr).classList.add("active");
    (l === "en" ? fr : en).classList.remove("active");
  }

  function initialiser() {
    var voulue = "fr";
    try { voulue = localStorage.getItem(CLE_STOCKAGE) || "fr"; } catch (e) {}
    if (voulue === "en") definirLangue("en");
    else signalerAuServeur("fr");
    marquerSegment(voulue);
  }

  if (document.readyState === "loading")
    document.addEventListener("DOMContentLoaded", initialiser);
  else initialiser();

  // Shiny remplace des pans entiers de page a la reconnexion : on repasse.
  if (window.jQuery) jQuery(document).on("shiny:connected shiny:value", function () {
    planifier();
  });
})();
