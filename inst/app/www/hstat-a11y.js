// Accessibilite et ergonomie communes a toute l'application.
(function () {
  "use strict";

  function etiquetteDe(id) {
    if (!id) return null;
    var lab = document.querySelector('label[for="' + CSS.escape(id) + '"]');
    if (!lab) return null;
    if (!lab.id) lab.id = id + "-label";
    return lab;
  }

  function nomIcone(i) {
    var c = (i.className || "").toString().match(/fa-([a-z0-9-]+)/g) || [];
    for (var k = 0; k < c.length; k++) {
      var n = c[k].slice(3);
      if (["solid", "regular", "fw", "lg", "2x", "spin"].indexOf(n) < 0)
        return n.replace(/-/g, " ");
    }
    return "";
  }

  function corrigerIcones(r) {
    var ic = r.querySelectorAll('i[role="presentation"][aria-label], i.fa, i.fas, i.far, i.fab, i.glyphicon');
    for (var k = 0; k < ic.length; k++) {
      var i = ic[k];
      if (i.getAttribute("aria-hidden") === "true") continue;
      var hote = i.closest("button, a, .btn");
      if (hote && !hote.getAttribute("aria-label") &&
          !(hote.textContent || "").trim()) {
        var nom = hote.getAttribute("title") || nomIcone(i);
        if (nom) hote.setAttribute("aria-label", nom);
      }
      i.removeAttribute("aria-label");
      i.setAttribute("aria-hidden", "true");
    }
  }

  function relierEtiquettes(r) {
    // selectize : le <select> d'origine est cache, le champ visible est neuf.
    var sz = r.querySelectorAll(".selectize-control");
    for (var k = 0; k < sz.length; k++) {
      var champ = sz[k].querySelector(".selectize-input input");
      if (!champ || champ.hasAttribute("aria-labelledby")) continue;
      var sel = sz[k].previousElementSibling;
      var lab = sel && etiquetteDe(sel.id);
      if (!lab) {
        var grp = sz[k].closest(".form-group, .shiny-input-container");
        lab = grp && grp.querySelector("label");
        if (lab && !lab.id) lab.id = ((sel && sel.id) || "hstat-sel-" + k) + "-label";
      }
      if (lab && (lab.textContent || "").trim()) champ.setAttribute("aria-labelledby", lab.id);
      else champ.setAttribute("aria-label", (sel && sel.getAttribute("name")) || "Liste de choix");
    }
    // selecteurs de couleur et curseurs : l'etiquette du groupe.
    var ch = r.querySelectorAll("input.shiny-colour-input, input.js-range-slider");
    for (var j = 0; j < ch.length; j++) {
      var c = ch[j];
      if (c.hasAttribute("aria-labelledby") || c.hasAttribute("aria-label")) continue;
      var l = etiquetteDe(c.id) ||
              (c.closest(".form-group") || {}).querySelector &&
              c.closest(".form-group").querySelector("label");
      if (l) { if (!l.id) l.id = (c.id || "hstat") + "-label"; c.setAttribute("aria-labelledby", l.id); }
    }
  }

  function divers(r) {
    var pre = r.querySelectorAll("pre.shiny-text-output:not([tabindex])");
    for (var k = 0; k < pre.length; k++) pre[k].setAttribute("tabindex", "0");
    var bt = r.querySelectorAll('.btn-box-tool[data-widget="collapse"]');
    for (var j = 0; j < bt.length; j++) {
      var b = bt[j];
      b.setAttribute("aria-label", "Replier ou déplier la boîte");
      var box = b.closest(".box");
      b.setAttribute("aria-expanded",
                     box && box.classList.contains("collapsed-box") ? "false" : "true");
    }
  }

  function traiter(r) {
    if (!r || !r.querySelectorAll) return;
    try { corrigerIcones(r); relierEtiquettes(r); divers(r); } catch (e) {}
  }

  function marquerActif() {
    var liens = document.querySelectorAll(".sidebar-menu a[data-value]");
    for (var k = 0; k < liens.length; k++) {
      var a = liens[k];
      if (a.parentElement && a.parentElement.classList.contains("active"))
        a.setAttribute("aria-current", "page");
      else a.removeAttribute("aria-current");
    }
  }

  function lienEvitement() {
    var cont = document.querySelector(".content-wrapper");
    if (!cont || document.getElementById("hstat-evitement")) return;
    if (!cont.id) cont.id = "hstat-contenu";
    cont.setAttribute("tabindex", "-1");
    cont.setAttribute("role", "main");
    var a = document.createElement("a");
    a.id = "hstat-evitement";
    a.className = "hstat-evitement";
    a.href = "#" + cont.id;
    a.textContent = "Aller au contenu principal";
    a.addEventListener("click", function (e) {
      e.preventDefault(); cont.focus(); cont.scrollIntoView();
    });
    document.body.insertBefore(a, document.body.firstChild);
  }

  function boutonHaut() {
    if (document.getElementById("hstat-haut")) return;
    var b = document.createElement("button");
    b.id = "hstat-haut";
    b.type = "button";
    b.className = "hstat-haut";
    b.setAttribute("aria-label", "Retour en haut de la page");
    b.setAttribute("title", "Retour en haut de la page");
    b.innerHTML = '<i class="fas fa-arrow-up" aria-hidden="true"></i>';
    b.addEventListener("click", function () {
      window.scrollTo({ top: 0, behavior: "smooth" });
      var cont = document.querySelector(".content-wrapper");
      if (cont) cont.focus({ preventScroll: true });
    });
    document.body.appendChild(b);
    var maj = function () { b.classList.toggle("hstat-visible", window.scrollY > 600); };
    window.addEventListener("scroll", maj, { passive: true });
    maj();
  }

  function init() {
    if (typeof jQuery === "undefined") return;
    var $ = jQuery;
    lienEvitement();
    boutonHaut();
    traiter(document.body);
    marquerActif();
    var attente = null, racines = [];
    new MutationObserver(function (mut) {
      for (var k = 0; k < mut.length; k++)
        for (var j = 0; j < mut[k].addedNodes.length; j++)
          if (mut[k].addedNodes[j].nodeType === 1) racines.push(mut[k].addedNodes[j]);
      if (attente) return;
      attente = requestAnimationFrame(function () {
        attente = null;
        var rs = racines; racines = [];
        for (var i = 0; i < rs.length; i++)
          if (rs[i].isConnected) traiter(rs[i].parentElement || rs[i]);
      });
    }).observe(document.body, { childList: true, subtree: true });

    // Changer d'onglet : on remonte en haut, sinon le nouvel onglet s'ouvre a
    // la hauteur ou l'on lisait l'ancien.
    $(document).on("shown.bs.tab", ".sidebar-menu a[data-toggle='tab']", function () {
      marquerActif();
      window.scrollTo(0, 0);
    });
    $(document).on("click", '.btn-box-tool[data-widget="collapse"]', function () {
      var b = this;
      setTimeout(function () {
        var box = b.closest(".box");
        b.setAttribute("aria-expanded",
                       box && box.classList.contains("collapsed-box") ? "false" : "true");
      }, 600);
    });
  }

  if (typeof jQuery !== "undefined") jQuery(init);
  else document.addEventListener("DOMContentLoaded", init);
})();
