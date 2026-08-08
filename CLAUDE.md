# HStat — conventions du dépôt

## Version : à faire évoluer à CHAQUE modification

Toute modification du code entraîne une montée de `Version:` dans `DESCRIPTION`,
dans le même commit que le changement. Jamais de commit de code sans bump.

Règle appliquée depuis le début du projet (cf. historique de `DESCRIPTION`) :

| Nature du changement | Incrément | Exemple |
|---|---|---|
| Nouvelle fonctionnalité, nouvelle analyse, nouveau module (`feat:`) | **mineur** | `0.6.0` → `0.7.0` |
| Correction, refactorisation, nettoyage, doc (`fix:`, `chore:`, `docs:`) | **correctif** | `0.5.1` → `0.5.2` |

### Le numéro n'est écrit qu'à un seul endroit

`DESCRIPTION` est la source unique de vérité. **Ne jamais recopier un numéro de
version ailleurs**, pas même en repli d'un `tryCatch` : un repli codé en dur ne
se met pas à jour et finit par mentir (la citation est restée bloquée sur
`0.2.3` alors que le paquet était en `0.7.1`).

Tout affichage de la version passe par `hstat_version()` (`Utils.R`), qui
résout dans cet ordre :

1. `utils::packageVersion("HStat")` — quand HStat est installé comme paquet ;
2. le champ `Version:` de `DESCRIPTION` lu sur disque — quand l'application
   tourne **depuis les sources** (`runApp("inst/app")`, déploiement du dossier
   sur shinyapps.io…), cas où `packageVersion()` échoue ;
3. `"0.0.0"` en dernier recours — volontairement invalide, pour qu'un numéro
   absent se voie au lieu de passer pour une vraie version.

`hstat_pkg_year()` suit la même logique pour l'année de citation.
Dans `inst/CITATION`, utiliser l'objet `meta` fourni par R (`meta$Version`).

**Seule exception : `README.md`.** C'est du markdown statique, il ne peut pas
lire `DESCRIPTION` : son bloc de citation porte donc le numéro en dur et doit
être mis à jour à la main à chaque montée de version. Un test échoue si les deux
divergent — le README était resté bloqué sur `0.6.0` alors que le paquet était
en `0.7.4`.

Note : `packageVersion()` lève une **erreur** quand le paquet est absent, mais
`packageDate()` un **avertissement** — il faut `suppressWarnings()` en plus du
`tryCatch()`, sinon l'onglet de citation pollue la console à chaque rendu.

## Structure

L'application vit dans `inst/app/` :

- `HStat.R` — point d'entrée : source les modules dans l'ordre, puis `shinyApp(ui, server)`
- `Utils.R` — fonctions de calcul et utilitaires partagés ; **sourcé en premier**,
  donc disponible pour l'UI comme pour le serveur
- `UX.R` — définit `ui` (tous les onglets)
- `app_server.R` — définit `server` ; contient les analyses multivariées
- `mod_*.R` — modules Shiny (tests, visualisation, ML, DL, qualitatif, etc.)

Corollaire : une fonction utilisée à la fois par l'UI et le serveur doit être
définie dans `Utils.R`, pas dans le corps de `server`.

### Assistance IA : deux roles, et deux seulement

`mod_ai.R` porte le moteur d'inference **partagé par toute l'application**
(c'est pourquoi `HStat.R` le source juste après `Utils.R` — un test garde cet
ordre) et l'onglet d'aide à la décision.

L'assistance **interprète** des résultats et **recommande** une analyse. Elle ne
choisit ni ne lance jamais une analyse : le choix de la méthode engage
l'interprétation scientifique du travail, il reste à l'analyste. L'invite
adressée au modèle l'interdit explicitement, et un test le vérifie.

**La recommandation ne passe jamais par un modèle de langue.** `hstat_reco_*`
applique des règles statistiques classiques au profil des variables : c'est
déterministe, hors ligne, et explicable. Un test statistique ne doit pas être
conseillé par génération de texte.

Piège corrigé, à ne pas réintroduire : la normalité se teste **dans chaque
groupe**, jamais sur la variable regroupée. Deux groupes parfaitement normaux
mais bien séparés forment un mélange bimodal que Shapiro rejette (p ≈ 1e-6 là
où chaque groupe donne p ≈ 0,8) — tester le mélange déconseillerait l'ANOVA
précisément quand elle convient.

### Capture des résultats : observer, ne pas instrumenter

Les modules déposent déjà leurs résultats dans `values` ; l'assistance les y
**observe** (`hstat_ai_capture()` appelé depuis un `observeEvent`). Aucun module
n'appelle l'assistance, aucune analyse n'est modifiée, et une analyse ajoutée
plus tard est captée sans rien changer du moment qu'elle alimente les mêmes
emplacements.

Corollaire : l'observateur doit vivre **là où ses `input$` existent**. Un module
namespacé (`mod_descriptive`, `mod_ml`…) porte le sien dans son propre
`moduleServer` — `input$numVars` n'a aucun sens dans `app_server.R`.

Un test vérifie qu'aucune famille d'analyse n'est oubliée : ajouter un onglet
d'analyse sans y poser de `hstat_ai_capture()` le fait échouer.

### Bandeau de guidage : greffé, pas inséré

Le bandeau de fin d'analyse (`aihint_*`) s'ajoute aux onglets **sans toucher
aux modules** : `hstat_ai_with_hint()` ajoute un enfant au `tabItem` que le
module a déjà construit. Les identifiants sont déclarés une seule fois dans
`HSTAT_AI_HINT_IDS` ; un test vérifie que déclarés et posés coïncident
exactement — un identifiant déclaré mais jamais posé ne s'afficherait nulle
part, l'inverse ferait échouer `hstat_ai_hint_slot()` au démarrage.

Une sortie Shiny ne peut apparaître qu'une fois dans le DOM : chaque onglet a
donc son propre identifiant, et `app_server.R` les rend en boucle.

### Ne jamais brancher sur une statistique non calculable

Un test statistique rend `NA` ou `NaN` dès que ses données sont dégénérées
(variance nulle, matrice singulière, effectifs vides). `if (p < 0.05)` lève
alors « missing value where TRUE/FALSE needed » et fait tomber **toute** la
sortie, pas seulement la ligne concernée. Passer par `hstat_p_verdict()`
(`Utils.R`), qui rend trois états — `significatif` / `non significatif` /
`indeterminable` — et traiter le troisième explicitement. Un test barre la
route à toute nouvelle condition `if (... $p.value < ...)` non gardée.

### FactoMineR : les coordonnées peuvent n'être qu'un vecteur

Dès qu'un résultat ne comporte qu'un seul axe, FactoMineR renvoie ses
coordonnées comme un **vecteur nu** et non une matrice : `ncol()` vaut `NULL`
et `coord[, 1:2]` échoue sur « incorrect number of dimensions ». Le cas est
courant, pas exotique : toute AFC croisant une variable **binaire** (sexe,
oui/non, avant/après) produit une table dont la plus petite dimension vaut 2,
donc un seul axe. Passer systématiquement par `hstat_coord_mat()` (`Utils.R`)
avant d'indexer. Un test le vérifie sur l'ensemble du dépôt.

### Ne jamais appeler une fonction de rendu à la main

`DT::renderDT(...)()`, `shiny::renderTable(...)()` depuis un `renderUI` échouent
avec « argument "name" is missing » : une fonction de rendu attend la session et
le nom de sortie que Shiny lui passe. Deux fois le piège dans ce dépôt. Il faut
soit une sortie dédiée (`DTOutput` + `renderDT`), soit un composant statique
(`.hstat_html_table()` dans `mod_ai.R`).

### Assistance au codage : local et gratuit d'abord

`mod_coding.R` propose trois moteurs d'assistance (`HSTAT_AI_ENGINES`). L'ordre
n'est pas cosmétique : **le moteur local est le premier choix et le défaut de
`hstat_ai_call()` / `hstat_ai_status()`**, l'API payante vient en dernier. Un
test garde cet ordre — une fonctionnalité facturée à l'usage ne doit jamais
devenir le chemin par défaut d'un utilisateur qui n'a rien demandé.

Le moteur `"auto"` (thématisation statistique) ne dépend d'aucun paquet
optionnel : c'est le seul dont la disponibilité est garantie, et c'est vers lui
que renvoient les messages d'erreur des deux autres. `httr` et `jsonlite`
restent donc en **Suggests**, jamais en Imports.

Les corps de requête sont construits par `.hstat_ai_body_ollama()` et
`.hstat_ai_body_openai()`, séparés de l'envoi réseau : c'est la partie qui casse
en silence quand un serveur renomme un champ, elle doit rester testable sans
serveur.

### Modules imbriqués : l'ordre de `source()` compte

`mod_qualitative_ui()` appelle `mod_coding_ui()` (l'atelier de codage CAQDAS
vit dans son propre fichier plutôt que d'alourdir les ~2900 lignes de
`mod_qualitative.R`). `HStat.R` doit donc sourcer `mod_coding.R` **avant**
`mod_qualitative.R` — un test garde cette contrainte.

## Tests

`tests/testthat/test-hstat.R` est la suite de référence (celle qu'exécute
`R CMD check` via `tests/testthat.R`). Elle source `Utils.R` et
`mod_qualitative.R` dans un environnement isolé, sans démarrer Shiny : seules
les **fonctions de calcul pures** y sont testables.

Placer la logique statistique dans `Utils.R` plutôt que dans un `observeEvent`
la rend testable — c'est le motif suivi par `hstat_ref_test()`,
`hstat_metrics_*()`, `hstat_q_*()`, etc.

Lancer, depuis la racine du dépôt : **`testthat::test_dir("tests/testthat")`**.

Viser `tests/testthat` et non `tests` : testthat considère comme suite tout
fichier dont le nom commence par `test`, y compris `tests/testthat.R`, dont le
`library(HStat)` échoue tant que le paquet n'est pas installé. `R CMD check`,
lui, passe bien par `tests/testthat.R` une fois l'installation faite.

### Environnement de test sans accès à CRAN

CRAN peut être injoignable ; les paquets R sont alors disponibles via apt
(`apt-cache search '^r-cran-'`, ~1130 paquets) :

```sh
apt-get install -y r-cran-testthat r-cran-shiny r-cran-ggplot2 r-cran-dplyr \
                   r-cran-svglite r-cran-openxlsx r-cran-dt
```

`svglite` est indispensable au test d'export image (`ggsave(device = "svg")`) :
sans lui la suite tombe en erreur, alors que rien n'est cassé dans le code.

## Déploiement : `app.R` à la racine

`app.R` (racine) est le pont vers `inst/app/` pour shinyapps.io, Posit Connect
et Shiny Server. Il doit se contenter de :

```r
shiny::shinyAppDir(app_dir)
```

**Ne jamais y faire `setwd()` puis `source()`.** Shiny résout le dossier de
l'application — et donc l'emplacement de `www/` — *avant* d'évaluer `app.R` :
un `setwd()` arrive trop tard. L'application déployée répondait alors 404 sur
`hstat-theme.css`, `Sortable.min.js` et toutes les polices, et s'affichait sans
son thème ni le glisser-déposer. Un test de non-régression garde cette règle.

`R/_disable_autoload.R` empêche par ailleurs Shiny de sourcer le dossier `R/`
du paquet dans l'environnement de l'application (`run_hstat` y était injecté).
Shiny cherche ce fichier dans `R/`, pas à la racine ; l'avertissement au
démarrage subsiste car Shiny l'émet avant de tester le fichier.

## Fins de ligne

Attention : le dépôt est **mixte**, et bien plus qu'il n'y paraît. Sont en
**CRLF** : `Utils.R`, `HStat.R`, `inst/app/app.R`, `mod_clean.R`,
`mod_descriptive.R`, `mod_design.R`, `mod_explore.R`, `mod_filter.R`,
`mod_qualitative.R`, `mod_tests.R`, `mod_threshold.R`, `mod_viz.R`, ainsi que
`README.md` et le `app.R` racine. Sont en **LF** : `UX.R`, `app_server.R`,
`mod_ai.R`, `mod_coding.R`, `mod_dl.R`, `mod_ml.R`, `mod_timeseries.R`, le
dossier `R/`, `CLAUDE.md` et la suite de tests.

Préserver les fins de ligne existantes lors d'une édition — un fichier réécrit
intégralement en LF produit un diff de plusieurs milliers de lignes qui masque
le changement réel.

Vérifier avant d'éditer plutôt que se fier à cette liste :

```sh
file inst/app/mon_fichier.R   # « with CRLF line terminators » ou non
```

Vérification rapide avant de committer :

```sh
git diff --numstat   # le nombre de lignes doit correspondre au changement réel
```

## Langue

Interface, messages, commentaires de code et interprétations statistiques sont
en **français**. Les identifiants (noms de fonctions, d'inputs, de variables)
restent en anglais ou en abrégé technique.
