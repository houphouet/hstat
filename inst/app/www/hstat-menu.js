// HStat — sections repliables de la barre laterale.
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

      // Le role de bouton se pose sur un <span> interieur : un <li> d'une
      // liste ne peut pas en porter un autre que listitem.
      $t.wrapInner('<span class="hstat-repli-btn" role="button" tabindex="0"></span>');
      var $b = $t.children(".hstat-repli-btn");
      $items.addClass("hstat-repli-item");

      function afficher(anime) {
        if (ouvert) return;
        ouvert = true;
        $t.addClass("hstat-ouvert"); $b.attr("aria-expanded", "true");
        if (anime) $items.stop(true, true).slideDown(DUREE); else $items.show();
      }
      function masquer(anime) {
        if (!ouvert || $t.hasClass("hstat-epingle")) return;
        ouvert = false;
        $t.removeClass("hstat-ouvert"); $b.attr("aria-expanded", "false");
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
      $b.on("keydown", function (e) {
        if (e.key === "Enter" || e.key === " ") { e.preventDefault(); basculer(); }
      });

      if (epingles.indexOf(cle) >= 0) {
        $t.addClass("hstat-epingle");
        ouvert = false;
        afficher(false);
      } else {
        $b.attr("aria-expanded", "false");
        $items.hide();
      }
      $t.addClass("hstat-repli-pret");
    });
  }

  if (typeof jQuery !== "undefined") jQuery(init);
  else document.addEventListener("DOMContentLoaded", init);
})();
