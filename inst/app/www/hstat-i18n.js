/* ===========================================================================
   HStat - bascule francais / anglais
   ---------------------------------------------------------------------------
   L'application compte ~9 700 chaines destinees a l'utilisateur, reparties sur
   24 fichiers R. Les envelopper une a une dans un appel de traduction serait un
   chantier de plusieurs semaines qui toucherait chaque ligne de code.
   La traduction est donc appliquee AU TEXTE AFFICHE : l'interface construite
   par UX.R, celle des modules, les notifications et les tableaux rendus passent
   tous par le meme filtre, sans qu'aucun appel de code soit modifie.

   TROIS PROPRIETES QUI COMPTENT

   1. HORS LIGNE. Le dictionnaire est incorpore dans la page (window.HSTAT_I18N)
      au moment ou l'interface est construite. Aucune requete, jamais.

   2. DEGRADATION DOUCE. La cle est la chaine FRANCAISE elle-meme : une chaine
      absente du dictionnaire reste en francais, au lieu d'afficher un
      identifiant technique. Une traduction incomplete n'abime rien.

   3. REVERSIBLE EXACTEMENT. Le texte francais d'origine est conserve sur le
      noeud (__hstatFr). Repasser en francais restitue la chaine exacte plutot
      que de retraduire en sens inverse — une traduction inverse par
      dictionnaire perdrait les accents et les doublons de sens.

   PRUDENCE SUR LES DONNEES DE L'UTILISATEUR
   Seules les correspondances EXACTES et completes avec une entree du
   dictionnaire sont remplacees : un texte libre, un nom de variable ou une
   valeur de cellule n'est jamais traduit par morceaux. Un element portant
   l'attribut `data-hstat-notranslate` est ignore, lui et sa descendance.
   =========================================================================== */

(function () {
  "use strict";

  var DICT = window.HSTAT_I18N || {};          // francais -> anglais
  var CLE_STOCKAGE = "hstat-langue";
  var langue = "fr";
  var enCours = false;

  // Balises dont le contenu n'est pas du texte d'interface.
  var IGNORE = { SCRIPT: 1, STYLE: 1, TEXTAREA: 1, CODE: 1, PRE: 1, SVG: 1 };
  var ATTRS = ["placeholder", "title", "aria-label", "alt"];

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

  // --------------------------------------------------------- noeuds de texte
  function traduireTexte(racine) {
    var w = document.createTreeWalker(racine, NodeFilter.SHOW_TEXT, null, false);
    var n, lot = [];
    while ((n = w.nextNode())) lot.push(n);
    for (var i = 0; i < lot.length; i++) {
      var t = lot[i];
      if (ignorable(t.parentNode)) continue;
      var brut = t.nodeValue;
      var net = brut.trim();
      if (!net) continue;

      if (langue === "en") {
        if (t.__hstatFr !== undefined) continue;   // deja traduit
        var cible = DICT[net];
        if (cible === undefined) continue;
        t.__hstatFr = brut;
        // Les espaces qui entourent le texte sont conserves : les retirer
        // collerait un libelle a l'icone qui le precede.
        t.nodeValue = brut.replace(net, cible);
      } else if (t.__hstatFr !== undefined) {
        t.nodeValue = t.__hstatFr;
        delete t.__hstatFr;
      }
    }
  }

  // ----------------------------------------------------------- attributs
  function traduireAttributs(racine) {
    var els = racine.querySelectorAll ? racine.querySelectorAll("*") : [];
    for (var i = 0; i < els.length; i++) {
      var el = els[i];
      if (ignorable(el)) continue;
      for (var a = 0; a < ATTRS.length; a++) {
        var nom = ATTRS[a];
        if (!el.hasAttribute(nom)) continue;
        var memo = "__hstatAttr_" + nom;
        if (langue === "en") {
          if (el[memo] !== undefined) continue;
          var v = el.getAttribute(nom).trim();
          var cible = DICT[v];
          if (cible === undefined) continue;
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
      traduireTexte(racine);
      traduireAttributs(racine);
    } catch (e) { /* une page a moitie traduite vaut mieux qu'une page cassee */ }
    enCours = false;
  }

  // ------------------------------------------------- contenu rendu plus tard
  // Notifications, tableaux, sorties Shiny : tout arrive apres le chargement.
  // Sans observateur, seule l'interface initiale serait traduite.
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

  // --------------------------------------------------------------- bascule
  function definirLangue(l) {
    l = (l === "en") ? "en" : "fr";
    if (l === langue) return;
    langue = l;
    try { localStorage.setItem(CLE_STOCKAGE, l); } catch (e) {}
    document.documentElement.setAttribute("lang", l);
    marquerSegment(l);
    // Le SERVEUR doit connaitre la langue : les messages d'erreur et les
    // interpretations sont composes en R, phrase par phrase, et n'existent
    // donc pas comme chaine entiere dans le dictionnaire du navigateur.
    signalerAuServeur(l);
    // Repasser en francais doit restituer le texte d'origine : on parcourt
    // donc la page dans les deux sens de bascule.
    enCours = true;
    try { traduireTexte(document.body); traduireAttributs(document.body); }
    catch (e) {} finally { enCours = false; }
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

  // Le temoin visuel doit suivre la langue reellement appliquee. Sans cela,
  // apres un rechargement, la page s'affichait en anglais avec le segment
  // « FR » marque actif : le temoin mentait.
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
