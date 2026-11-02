/* HStat — sections repliables de la barre laterale.
 *
 * Chaque titre de section portant `data-hstat-repli` regroupe les entrees qui
 * le suivent, jusqu'au titre suivant. Les entrees sont masquees par defaut :
 *   - le SURVOL du titre (ou de ses entrees) les deroule le temps du passage ;
 *   - le CLIC sur le titre les garde ouvertes, un second clic les replie.
 * L'etat epingle survit au rechargement (localStorage, par onglet de
 * navigateur). Un titre sans l'attribut (« 6. A propos ») reste deplie.
 *
 * Le masquage est pose par ce script et non par la feuille de style : si le
 * script ne se charge pas, le menu reste entierement visible au lieu de
 * disparaitre. Les evenements passent par jQuery (regle du depot).
 */
(function () {
  "use strict";
  var CLE = "hstat-menu-epingle";
  var DELAI_FERMETURE = 250;   // ms : laisse le temps de passer du titre aux entrees
  var DUREE = 150;             // ms : deroulement

  function lireEpingles() {
    try {
      var v = JSON.parse(window.localStorage.getItem(CLE) || "[]");
      return Array.isArray(v) ? v : [];
    } catch (e) { return []; }
  }
  function ecrireEpingles(liste) {
    try { window.localStorage.setItem(CLE, JSON.stringify(liste)); } catch (e) {}
  }

  function init() {
    if (typeof jQuery === "undefined") return;
    var $ = jQuery;
    var $titres = $(".sidebar-menu > li.header[data-hstat-repli]");
    if (!$titres.length) return;
    var epingles = lireEpingles();

    $titres.each(function (i) {
      var $t = $(this);
      if ($t.hasClass("hstat-repli-pret")) return;   // deja initialise (pas .data() : jQuery y lirait l attribut data-hstat-repli)
      var $items = $t.nextUntil("li.header").not("li.header");
      var cle = String(i + 1);
      var minuterie = null;
      var ouvert = false;

      $t.attr({ role: "button", tabindex: "0" });
      $items.addClass("hstat-repli-item");

      function afficher(anime) {
        if (ouvert) return;
        ouvert = true;
        $t.addClass("hstat-ouvert").attr("aria-expanded", "true");
        if (anime) $items.stop(true, true).slideDown(DUREE); else $items.show();
      }
      function masquer(anime) {
        if (!ouvert || $t.hasClass("hstat-epingle")) return;
        ouvert = false;
        $t.removeClass("hstat-ouvert").attr("aria-expanded", "false");
        if (anime) $items.stop(true, true).slideUp(DUREE); else $items.hide();
      }
      function entrer() {
        if (minuterie) { clearTimeout(minuterie); minuterie = null; }
        afficher(true);
      }
      function sortir() {
        if (minuterie) clearTimeout(minuterie);
        minuterie = setTimeout(function () { minuterie = null; masquer(true); },
                               DELAI_FERMETURE);
      }
      function basculer() {
        var liste = lireEpingles().filter(function (k) { return k !== cle; });
        if ($t.hasClass("hstat-epingle")) {
          $t.removeClass("hstat-epingle");
          masquer(true);
        } else {
          $t.addClass("hstat-epingle");
          afficher(true);
          liste.push(cle);
        }
        ecrireEpingles(liste);
      }

      $t.add($items).on("mouseenter", entrer).on("mouseleave", sortir);
      $t.on("click", function (e) { e.preventDefault(); basculer(); });
      $t.on("keydown", function (e) {
        if (e.key === "Enter" || e.key === " ") { e.preventDefault(); basculer(); }
      });

      if (epingles.indexOf(cle) >= 0) {
        $t.addClass("hstat-epingle");
        ouvert = false;
        afficher(false);
      } else {
        $t.attr("aria-expanded", "false");
        $items.hide();
      }
      $t.addClass("hstat-repli-pret");
    });
  }

  if (typeof jQuery !== "undefined") jQuery(init);
  else document.addEventListener("DOMContentLoaded", init);
})();
