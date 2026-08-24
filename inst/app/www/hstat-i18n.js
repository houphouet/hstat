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

  // LES DONNEES DE L'UTILISATEUR NE SE TRADUISENT PAS.
  // Une cellule de tableau peut contenir « Oui », « Non », « Total »,
  // « Normal » — des valeurs de SES donnees qui coincident mot pour mot avec
  // des libelles d'interface. Traduites, elles alteraient ce que l'utilisateur
  // lit de son propre fichier : le pire defaut possible pour un outil
  // statistique. Constate a l'ecran : « Oui » devenait « Yes » dans l'apercu.
  //
  // Regle : dans une cellule <td>, on ne traduit qu'au-dela de LONGUEUR_CELLULE
  // caracteres. Une valeur de donnees n'est presque jamais identique a une
  // phrase longue, alors qu'une interpretation l'est toujours. Les en-tetes
  // <th>, eux, restent traduits — ce sont des libelles, pas des donnees.
  var LONGUEUR_CELLULE = 25;

  // LES TERMES DU FICHIER DE L'UTILISATEUR NE SE TRADUISENT JAMAIS.
  // La regle de longueur ci-dessus est une heuristique : elle protege « Oui »
  // dans une cellule, mais pas un EN-TETE de colonne (un <th> est un libelle,
  // sauf quand c'est le nom d'une variable du fichier), et un tableau ajoute
  // demain echapperait a toute annotation posee a la main.
  //
  // Le serveur envoie donc la liste des termes qui viennent du fichier -- noms
  // de colonnes et modalites qualitatives. Rien de ce qui figure dans cette
  // liste n'est traduit, ou que ce soit dans la page.
  //
  // Prix assume : si une colonne s'appelle « Total », le libelle d'interface
  // « Total » cesse d'etre traduit lui aussi. C'est la degradation douce de la
  // conception ; alterer une donnee n'en est pas une.
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

  // ------------------------------------------- phrases coupees par une balise
  // LE TRADUCTEUR REMPLACE DES NOEUDS DE TEXTE. Une phrase mise en forme --
  // « <b>Toutes fermees</b> — <code>[a, b]</code>, … » -- n'existe donc nulle
  // part comme noeud entier : le DOM la coupe en autant de morceaux qu'il y a
  // de balises. Les 73 entrees du dictionnaire de cette forme -- les encadres
  // d'aide, ceux qui portent le plus d'explications -- ne pouvaient JAMAIS
  // s'appliquer, et restaient en francais quoi qu'on fasse.
  //
  // Elles sont donc traitees a part, au niveau de l'ELEMENT qui les porte.
  //
  // Traduire plutot chaque morceau serait pire : les morceaux courts (« La »,
  // « ou », « Lancez ») sont ceux qui ressemblent le plus a une valeur de
  // donnees, et les ecarter par prudence rendrait une phrase a moitie anglaise.
  //
  // LA CLE EST PASSEE PAR L'ANALYSEUR DU NAVIGATEUR avant d'etre comparee :
  // `innerHTML` est RENORMALISE a la lecture -- guillemets simples devenus
  // doubles, entites resolues, attributs reordonnes. Comparer la chaine du CSV
  // telle qu'elle y est ecrite n'aurait presque jamais colle.
  var DICT_HTML = null;                 // innerHTML normalise -> traduction
  var HTML_MAX  = 0;

  function indexerHtml() {
    DICT_HTML = Object.create(null);
    var tmp;
    try { tmp = document.createElement("div"); } catch (e) { return; }
    if (!tmp) return;
    for (var cle in DICT) {
      if (cle.indexOf("<") < 0) continue;
      var norm;
      try { tmp.innerHTML = cle; norm = String(tmp.innerHTML).trim(); }
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
        // passe suivante s'en charge. Ne pas lire son innerHTML evite de
        // serialiser chaque cellule d'un grand tableau pour rien.
        if (!el.children || !el.children.length) continue;
        var brut = el.innerHTML;
        if (!brut || brut.length > HTML_MAX + 8) continue;
        var cible = DICT_HTML[String(brut).trim()];
        if (cible === undefined) continue;
        el.__hstatFrHtml = brut;
        el.innerHTML = cible;
      } else if (el.__hstatFrHtml !== undefined) {
        // Le retour au francais restitue le balisage d'origine EXACTEMENT :
        // les noeuds de texte anglais sont remplaces d'un bloc, ils ne portent
        // donc aucun __hstatFr a defaire.
        el.innerHTML = el.__hstatFrHtml;
        delete el.__hstatFrHtml;
      }
    }
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
        // Protection des donnees de l'utilisateur, deux barrieres : le terme
        // vient-il du fichier charge (exact), et la regle de longueur en
        // cellule (heuristique, pour ce que le serveur n'a pas pu annoncer).
        if (estDonnee(net)) continue;
        if (net.length <= LONGUEUR_CELLULE && dansCellule(t.parentNode)) continue;
        t.__hstatFr = brut;
        // Les espaces qui entourent le texte sont conserves : les retirer
        // collerait un libelle a l'icone qui le precede.
        // Le remplacement passe par une FONCTION : avec une chaine, « $& » et
        // « $\u0060 » dans une traduction seraient interpretes comme des
        // references au texte trouve et la corrompraient.
        t.nodeValue = brut.replace(net, function () { return cible; });
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
      // qu'il installe sont deja anglais, donc invisibles pour la passe
      // suivante (les cles du dictionnaire sont francaises).
      traduireHtml(racine);
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
  // pret quand ce fichier s'execute : on reessaie, sinon le gestionnaire
  // serait perdu en silence et les donnees ne seraient plus protegees.
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
      // ses valeurs traduites avant l'arrivee de cette liste : on defait puis
      // on refait la passe, sinon la protection n'agirait qu'a partir du
      // prochain rendu.
      //
      // Seules les passes qui CONSULTENT la liste sont rejouees : un element
      // dont le balisage entier coincide avec une entree du dictionnaire est
      // un libelle par construction, une donnee ne peut pas le reproduire.
      // Le defaire et le refaire serait 73 remplacements pour rien.
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
