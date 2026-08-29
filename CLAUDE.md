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

Même problème, même remède pour la section **« Project structure »** : rien ne
la met à jour quand un fichier arrive ou disparaît, et elle avait dérivé —
cinq fichiers réels manquants (le workflow de CI, `CLAUDE.md`,
`R/_disable_autoload.R`, `mod_report.R`, `hstat-session.js`) et un fichier
inexistant listé (`tests/test-hstat.R`). Une documentation qui **invente** un
fichier est pire qu'une documentation absente : on le cherche. Un test
reconstitue l'arbre du README et le compare à `git ls-files` dans les deux
sens ; il a été vérifié comme échouant sur chacune des deux dérives.

Note : `packageVersion()` lève une **erreur** quand le paquet est absent, mais
`packageDate()` un **avertissement** — il faut `suppressWarnings()` en plus du
`tryCatch()`, sinon l'onglet de citation pollue la console à chaque rendu.

## Structure

Le code vit dans `R/` — c'est le code du **paquet**. `inst/app/` ne garde que ce
qui **agit** au démarrage :

- `R/utils.R` — le socle : fonctions de calcul et utilitaires partagés
- `R/mod_*.R` — les **17 modules** Shiny (tests, visualisation, ML, DL,
  qualitatif, etc.), UI et serveur
- `inst/app/Utils.R` — le pont : charge le socle, puis effets de bord de
  démarrage (locale, installation des paquets, aiguillages d'interface)
- `inst/app/UX.R` — définit `ui` (tous les onglets)
- `inst/app/app_server.R` — définit `server` ; contient les analyses multivariées
- `inst/app/HStat.R` — point d'entrée : le pont, `UX.R`, `app_server.R`, puis
  `shinyApp(ui, server)`

Corollaire : une fonction utilisée à la fois par l'UI et le serveur doit être
définie dans `R/utils.R`, pas dans le corps de `server`.

### L'ordre de `source()` n'existe plus, et c'était le but

`HStat.R` sourçait quinze modules **dans un ordre qui devait être tenu à la
main** : `mod_ai.R` avant ceux qui appellent `hstat_ai_*`, `mod_coding.R` avant
`mod_qualitative.R` (dont l'UI appelle `mod_coding_ui()`). Deux tests gardaient
ces rangs. Ils gardaient mal : rien n'empêchait un module ajouté plus tard de se
glisser au mauvais endroit, et l'erreur — « could not find function
mod_coding_ui » — serait tombée au démarrage, loin de sa cause.

Les modules étant dans `R/`, ce sont des **définitions** : toutes en place avant
qu'aucune ne soit appelée. Les deux tests gardent désormais l'inverse de ce
qu'ils gardaient — qu'aucune ligne `source("mod_*.R")` ne revienne dans
`HStat.R`. En réintroduire une remettrait la contrainte sans le garde-fou.

Le chargeur (`.hstat_charger_socle`) **balaie le dossier** : `utils.R` d'abord
par lisibilité, le reste par ordre alphabétique. Un module ajouté demain est
chargé sans qu'on y pense — et le banc de tests, qui balaie le même dossier, le
prend de même. Avant, il en nommait quatre ; un cinquième portant une fonction
de calcul serait resté invisible, et ses tests auraient échoué sur « could not
find function », loin de la cause.

### Qualifier les appels : quatre pièges, tous constatés

Le code du paquet appelle **toujours** `pkg::fn()`, ou passe par un
**aiguillage** de l'application. Sans cela, il dépend de ce que `library()` a
attaché — et un paquet installé mais **non attaché** le fait tomber (constaté en
intégration continue sur « could not find function updatePickerInput »).

La réécriture est mécanique, faite aux positions rendues par l'analyseur de R.
Elle a produit quatre défauts, chacun d'une famille différente, chacun gardé par
un test :

1. **Un nom de paquet peut recouvrir une fonction locale.**
   `hstat_ai_reglages_ui()` définit chez elle `id <- function(s) ns(...)`. Le
   balayage n'a vu qu'un appel inconnu, l'a trouvé exporté par dplyr, et a écrit
   `dplyr::id("url")`. Rien ne lève au chargement : le défaut n'apparaît qu'à
   **l'affichage de l'onglet**, sur « id() is defunct ». Deux autres du même
   genre (`VIM::prepare()`, `mclust::sim()`), tous deux des réactifs.
   Le critère est la **portée**, pas la coïncidence de nom : `httr::timeout(timeout)`
   est juste — la locale y porte une valeur, pas une fonction.

2. **Un nom de paquet de base peut être masqué par un paquet d'interface.**
   L'envers du précédent. `box` est dans `graphics` : tenu pour « connu », il
   reste non qualifié, et sans `library(shinydashboard)` attaché
   `box(title = ..., status = ...)` appelle `graphics::box` et lève « plot.new
   has not been called yet ». **Toute** l'interface cesse de se construire, pour
   un nom de trois lettres.

3. **Un nom exporté n'est pas qualifiable pour autant.** `.data` est bien
   exporté par ggplot2, mais c'est un **pronom**, remplacé par le masque de
   données à l'évaluation. Écrit `ggplot2::.data[[x]]`, il est évalué tout de
   suite et lève « Can't subset `.data` outside of a data mask context » — donc
   tout graphique bâti sur un nom de colonne variable, c'est-à-dire le cas
   général.

4. **Une fonction passée en argument n'est pas un appel.**
   `do.call(tagList, els)` porte un simple `SYMBOL` : la qualification des
   appels ne le voit pas, et le nom résout par le chemin de recherche. Même
   chose pour `tags$div(...)` — `tags` est un objet de shiny, pas une fonction.

Bénéfice mesurable : `UX.R` construit désormais `ui` avec **shiny seul**
attaché. Le test des identifiants dupliqués, qui se *skippait* depuis toujours
faute de pouvoir bâtir l'interface, s'exécute enfin — et un test sauté ressemble
à un test qui passe.

### Un paquet optionnel s'appelle par son aiguillage, jamais par son nom

`hstat_installer_replis_ui()` (`R/utils.R`) pose `withSpinner`, `plotlyOutput`,
`ggplotly`, `layout`, `config`, `renderPlotly`, `colourInput`, `pickerInput`,
`radioGroupButtons`, `updatePickerInput` et `rank_list` : **toujours définis**,
soit vers le paquet, soit vers un équivalent de base.

Treize appels écrivaient pourtant `shinycssloaders::withSpinner(...)`,
`colourpicker::colourInput(...)` ou `shinyWidgets::pickerInput(...)` en dur. Ces
paquets sont **optionnels** : absents, l'appel lève, l'UI du module ne se
construit pas, et `HStat.R` remplace **toute** l'application par sa page de
secours — pour un indicateur d'attente manquant. Le repli existait sous le même
nom, à un préfixe près. Un test balaie le dépôt ; `R/utils.R` en est exclu,
c'est lui qui **pose** les aiguillages.

### Assistance IA : deux roles, et deux seulement

`R/mod_ai.R` porte le moteur d'inference **partagé par toute l'application**
et l'onglet d'aide à la décision. Il n'a plus de rang à tenir dans un
`source()` ; un test vérifie seulement qu'il est bien du côté paquet, l'y
oublier ferait revenir la question de l'ordre par la fenêtre.

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
d'analyse sans y poser de `hstat_ai_capture()` le fait échouer. Les **15**
familles sont couvertes, exploration et nettoyage compris.

Le registre n'a qu'un emplacement : la dernière analyse gagne. Un module ne doit
donc revendiquer le contexte que s'il a **réellement agi** — `mod_filter` se tait
tant qu'aucun filtre n'a retiré d'observation, `mod_clean` tant qu'aucune
transformation n'a été appliquée. Sans cette réserve, trois modules se
déclenchaient au chargement et s'écrasaient l'un l'autre, affichant un libellé
faux.

### Bandeau de guidage : retiré, et il ne revient pas

Un bandeau (`aihint_*`) greffé sur les douze onglets d'analyse et une
notification annonçaient, **à chaque résultat déposé**, l'analyse que le profil
des données appelle. La même recommandation vit en entier dans l'onglet
« Interprétation & aide à la décision », où l'utilisateur va la chercher.
Répétée à chaque calcul, elle recouvrait les résultats qu'on venait de
demander au lieu de les éclairer — signalé à l'écran.

`HSTAT_AI_HINT_IDS`, `hstat_ai_hint_slot()`, `hstat_ai_with_hint()`,
`hstat_ai_hint_ui()` et `hstat_ai_hint_text()` ont disparu avec elle. Un test
balaie `inst/app/` et échoue sur leur réintroduction, identifiant `aihint_*`
compris.

Le registre de capture (`hstat_ai_capture()`) est **intact** : c'est lui qui
alimente l'onglet d'interprétation, le journal de reproductibilité et le
rapport. Ce qui a été supprimé, c'est l'affichage non sollicité, pas la
collecte.

### Ne jamais brancher sur une statistique non calculable

Un test statistique rend `NA` ou `NaN` dès que ses données sont dégénérées
(variance nulle, matrice singulière, effectifs vides). `if (p < 0.05)` lève
alors « missing value where TRUE/FALSE needed » et fait tomber **toute** la
sortie, pas seulement la ligne concernée. Passer par `hstat_p_verdict()`
(`Utils.R`), qui rend trois états — `significatif` / `non significatif` /
`indeterminable` — et traiter le troisième explicitement. Un test barre la
route à toute nouvelle condition `if (... $p.value < ...)` non gardée.

### Un `tryCatch` autour d'une boucle emporte toute la sortie

Le principe est le même que pour les statistiques non calculables, mais la
portée est plus large. Dans `mod_tests.R`, le `tryCatch` de l'ANOVA enveloppe
**toute la boucle sur les variables** : une seule variable à résidus constants
faisait tomber l'ANOVA de *toutes* les autres.

Deux appels y étaient exposés — `shapiro.test()` lève « all 'x' values are
identical », et `car::leveneTest()` « contrasts can be applied only to factors
with 2 or more levels » quand les valeurs ajustées ne donnent qu'un niveau.
Chaque diagnostic est donc gardé séparément, comme le fait déjà l'onglet des
résidus (`sd(...) < 1e-10`) : l'ANOVA survit, seul le diagnostic manquant
disparaît.

Règle : quand un `tryCatch` couvre une boucle, tout ce qui peut lever à
l'intérieur doit être gardé au niveau de l'itération.

### FactoMineR : les coordonnées peuvent n'être qu'un vecteur

Dès qu'un résultat ne comporte qu'un seul axe, FactoMineR renvoie ses
coordonnées comme un **vecteur nu** et non une matrice : `ncol()` vaut `NULL`
et `coord[, 1:2]` échoue sur « incorrect number of dimensions ». Le cas est
courant, pas exotique : toute AFC croisant une variable **binaire** (sexe,
oui/non, avant/après) produit une table dont la plus petite dimension vaut 2,
donc un seul axe. Passer systématiquement par `hstat_coord_mat()` (`Utils.R`)
avant d'indexer. Un test le vérifie sur l'ensemble du dépôt.

### Un réglage déclaré mais masqué n'existe pas

Le panneau « Options du graphique » des comparaisons post-hoc portait **treize**
réglages dans un `div(style = "display:none;")` : largeur et hauteur d'export,
limites de l'axe X, taille et style du sous-titre et sa position, styles des
graduations X et Y, styles de la légende, taille des clés. Le serveur les
lisait, ils agissaient sur le graphique — et l'utilisateur ne pouvait pas les
atteindre. Un test échoue désormais sur tout `display:none` dans `mod_tests.R`,
et vérifie que chaque réglage lu par le serveur est bien déclaré dans
l'interface.

Un réglage était même déclaré **et** masqué **et** jamais lu (`legendKeySize`) :
il est maintenant branché sur `legend.key.height`.

Le panneau est organisé en sept cartes colorées (`.hstat_opt_section()`), une
couleur par famille. Ce n'est pas de la décoration : quarante contrôles gris
d'affilée ne se parcourent pas.

**Les listes de choix sont déclarées une fois** — `HSTAT_FONT_STYLES`,
`HSTAT_THEMES_GG`, `HSTAT_PALETTES_QUALI`, `HSTAT_PALETTES_DEGRADE`. La liste
des styles était recopiée treize fois. Deux tests gardent ces listes : un thème
absent du `switch` de `viz_get_theme()` retomberait en silence sur « minimal »,
et un nom de palette inconnu de RColorBrewer ferait tomber **tout** le
graphique — pour le seul utilisateur qui aurait choisi cette entrée.

Les palettes qualitatives viennent en premier et sont vérifiées comme telles
(`brewer.pal.info$category == "qual"`) : un dégradé sur des groupes sans ordre
naturel suggère une progression qui n'existe pas.

### Un téléchargement d'image ne renvoie jamais de HTML

Un `content =` de `downloadHandler` qui **lève**, ou qui se termine **sans
avoir écrit** le fichier, fait renvoyer à Shiny sa page d'erreur HTML. Le
navigateur l'enregistre sous le nom demandé : on croit tenir un PNG, on ouvre
du HTML. Signalé à l'écran.

Quatre causes réelles, toutes trouvées dans le dépôt :

1. **Une fonction hors de portée.** `mod_descriptive.R` appelait
   `calculate_dimensions_from_dpi()`, définie dans le corps de `server` — donc
   **jamais visible depuis un module**. Le téléchargement du graphique
   descriptif n'a jamais pu produire autre chose qu'une page d'erreur.
2. **Un `return()` avant d'écrire** (export multivarié générique, sans analyse
   lancée).
3. **Un `validate(need(...))` dans le `content`** (nuage de mots, carte
   conceptuelle) : il interrompt le contenu comme une erreur.
4. **Un handler qui n'écrit rien du tout** (« rapport complet » des post-hoc).

`hstat_ecrire_image()` (`Utils.R`) est le seul chemin d'écriture : elle ouvre
le périphérique du format demandé, trace, et **garantit qu'un fichier valide
existe au retour** — à défaut de graphique, une image portant le motif. Une
image qui explique vaut mieux qu'un fichier qu'aucun logiciel n'ouvre.

`hstat_img_fmt()` normalise le format (`jpg` → `jpeg`, `html` → `png`) et sert
aussi à composer le **nom** du fichier : c'est son extension que Shiny traduit
en type MIME. Ne pas fixer `contentType` à la main — il serait évalué à la
construction du handler, hors contexte réactif.

### Une ellipse de confiance suppose une covariance inversible

`stat_conf_ellipse()` (ggpubr, appelé par `fviz_*` avec
`ellipse.type = "confidence"`) s'arrête sur « valeur manquante là où
TRUE/FALSE est requis » — un facteur d'échelle `NA` — dans trois cas, et tous
se rencontrent : **moins de trois individus** dans le groupe, une **coordonnée
constante**, ou des points **parfaitement alignés**. Le message ne nomme ni le
groupe ni la cause.

`hstat_ellipse_ok()` vérifie **avant** de demander les ellipses et **nomme** les
groupes fautifs. Là où c'est possible (couche `stat_ellipse` maison), seuls les
groupes exploitables sont tracés au lieu de perdre la couche entière.

### Analyses multivariées : ggplot2 d'abord, puis les réglages

Les graphiques d'individus, de variables et les biplots imposaient leur propre
habillage — `ggtheme = theme_minimal()` en dur à chaque appel de `fviz_*`,
`theme_minimal(base_size = 12)` dans les graphiques maison, points à 2,4,
tracés à 0,7, texte à 12 — de sorte que **le rendu d'origine de ggplot2 était
inatteignable**. On devait défaire avant de faire.

Les valeurs de départ sont désormais celles de ggplot2 (`HSTAT_GG_POINT_SIZE`
1,5 ; `HSTAT_GG_LINEWIDTH` 0,5 ; `HSTAT_GG_BASE_SIZE` 11 ; `HSTAT_GG_LABEL_PT`
11), et le thème se choisit — `"gg"` par défaut, l'ancien habillage restant
disponible sous `"hstat"`. Corollaire : `HSTAT_LBL_PT_MIN` descend de 12 à 11,
sans quoi le défaut de ggplot2 serait hors du domaine du curseur.

### La résolution commande la taille, et il faut dire dans quel sens

Deux modèles cohabitent, et les confondre produit exactement le défaut que
chacun évite :

| Module | Les pixels saisis sont… | Effet du DPI |
|---|---|---|
| Seuils d'efficacité (`hstat_export_dims`) | une **mise en page**, lue à 96 ppp | multiplie la finesse, les champs ne bougent pas |
| Multivarié (`hstat_px_apres_dpi`) | les **pixels produits**, recalculés | recalcule les champs, taille physique constante |

Dans le second, `pouces = pixels / DPI` est **exact** — ce sont les pixels qui
dérivent de la taille physique, pas l'inverse. C'est la même formule qui était
fautive côté seuils, où l'utilisateur saisit les pixels à la main.

`calculate_dimensions_from_dpi()` a disparu : elle dérivait la taille du seul
DPI, par paliers, **ignorait** les champs de largeur/hauteur affichés à côté —
les régler ne faisait rien — et son palier « au-delà de 300 DPI » *réduisait*
la figure de 10 %.

L'observateur `mv_lier_dpi()` doit tourner avec `ignoreInit = FALSE` : le
premier passage sert à **retenir** la résolution de départ. Sans lui, le
premier changement n'a pas de « avant » à comparer et ne recalcule rien —
il fallait changer le DPI deux fois. Constaté à l'écran.

Les **23 exports** (14 analyses génériques, 9 historiques) sont déclarés dans
`MV_EXPORTS_DPI` ; un test échoue si l'un d'eux manque. L'export générique
sortait par ailleurs toujours **carré, neuf pouces de côté**, quels que soient
les champs.

Enfin, `createPlotDownloadHandler()` a été supprimée : jamais appelée, elle
portait un calcul de dimensions **différent** de celui réellement employé. Un
helper mort qu'on corrige en croyant corriger l'export est pire qu'absent.

### Seuils d'efficacité : la mise en forme, au complet

Le module traçait des barres avec un thème figé, une opacité figée à 0,8 en six
endroits, aucun sous-titre, aucun style de titre ni de graduations, et **aucune
valeur portée sur les barres** — sur un graphique en barres, c'est le premier
manque qu'on remarque.

Vingt-quatre réglages ont été ajoutés : thème (`viz_get_theme()`), sous-titre
avec son style et sa position, style et position du titre, styles des
graduations X et Y, valeurs sur les barres (affichage, décimales, taille,
style, couleur, position), opacité et contour des barres, pas des graduations
Y, étiquette de la ligne de seuil (affichage, position, style, taille), taille
du texte de légende **distincte** de celle du titre. Un test vérifie que chacun
est déclaré, lu, **et observé par le réactif du graphique** — un réglage que le
réactif n'observe pas se change sans que l'image bouge.

**L'étiquette d'une valeur dépend du signe.** Une efficacité négative — la
modalité fait moins bien que le témoin, c'est un résultat — descend sous l'axe :
un `vjust` figé écrirait son étiquette du mauvais côté de la barre.
`hstat_valeur_pos()` rend l'ordonnée **et** le calage ensemble, parce qu'ils ne
se choisissent pas séparément ; la position « au pied » ramène l'ordonnée à
zéro, seul endroit toujours visible.

**Un réglage se place là où vit ce qu'il règle.** La taille de l'étiquette
« Seuil : x % » vivait dans « Apparence & options », deux onglets plus loin que
la valeur du seuil qu'elle affiche : on la cherchait à côté de la valeur.
Affichage, taille, position et style de cette étiquette sont désormais dans
« Paramètres du seuil », avec la valeur, la couleur et le type de ligne.

**Une barre hors des limites de l'axe disparaît, avec son étiquette.** Le cas
est courant : le minimum vaut 0 par défaut. Le module compte désormais les
valeurs hors cadre et le dit, plutôt que de les escamoter.

**`colour = NA` n'est pas l'absence d'argument** : il *efface* le contour que
la géométrie dessinerait. `hstat_barre_style()` monte donc une **liste**
d'arguments à la demande, et n'y met la couleur que si un contour est demandé.

**Le sous-titre ne survit pas à `ggplotly`** — la conversion le laisse tomber.
Il est réinjecté en seconde ligne du titre plotly, échappé comme les étiquettes
d'axe. Même famille de piège que le plotmath : ce que ggplot sait rendre, la
conversion interactive ne le sait pas toujours.

### Une efficacité négative doit tenir dans le cadre

L'axe Y valait **0 à 100 par défaut**, et son champ « Minimum » portait
`min = 0`. Deux conséquences, dans le même sens :

1. toute efficacité négative — la modalité fait **moins bien** que le témoin,
   c'est un résultat — sortait du cadre et disparaissait, elle et son
   étiquette ;
2. l'utilisateur ne **pouvait pas** saisir la borne qui l'aurait ramenée.

Mesuré sur un essai à cinq modalités dont deux négatives : **2 barres sur 5
disparaissaient**, avec pour seule trace un « Removed 2 rows » dans la console.

Les deux bornes sont désormais **vides par défaut** = automatiques (`NA` sur
une borne, ggplot suit les données de ce côté), et le champ accepte le négatif.
Zéro est **toujours** inclus dans l'étendue : c'est la référence de la formule
d'Abbott, un cadre qui l'exclurait serait illisible.

`hstat_pas_debut()` aligne les graduations sur le pas demandé : partir de la
borne brute donnait −37, −17, **3**, 23… et **zéro n'était pas gradué**, alors
que c'est la seule graduation qui compte quand des valeurs sont négatives.
Une ligne de référence à zéro s'ajoute quand des négatives existent.

Le décompte « hors des limites » ne porte plus que sur les bornes
**réellement fixées** : une borne automatique ne peut, par construction, rien
exclure.

### Efficacités : une colonne par variable mesurée

`hstat_efficacite()` empile les variables mesurées — quinze variables sur onze
modalités font 165 lignes portant toutes la même colonne `Efficacite`. Le
sélecteur « Variable Y » n'avait donc qu'un seul choix, et le graphique
superposait quinze séries sur les mêmes onze positions : on croyait lire une
variable, on en lisait quinze. Constaté à l'écran.

`hstat_eff_large()` donne **une colonne d'efficacité par variable mesurée**,
nommée d'après elle. C'est ce tableau qui alimente le graphique, le sélecteur Y
et « Utiliser comme jeu de données » ; le tableau détaillé reste accessible
(bouton de présentation, et deuxième feuille du classeur Excel).

Le préfixe `Efficacite_` est délibéré : une colonne nommée comme la variable
d'origine contiendrait des **pourcentages** et non la mesure, et se confondrait
avec elle dès qu'on relit le tableau ou qu'on le réinjecte dans l'application.

### Le thème se déclare au kit, comme le format et le DPI

Quatre graphiques — descriptif, plan expérimental, distribution, valeurs
manquantes — n'offraient **aucun** choix de thème, quand les treize autres en
avaient un. Le thème rejoint donc le format et le DPI dans
`hstat_export_plot_ui()` : un bloc d'export ajouté demain en hérite sans qu'on
y pense.

`theme = FALSE` reste légitime pour les modules qui portent déjà un sélecteur
global (`hstat_plot_opts_ui()` — ML, DL, séries temporelles) : deux sélecteurs
pour un même graphique, c'est un réglage qui en contredit un autre. Un test
vérifie que `theme = FALSE` n'apparaît que là.

Les constructeurs de `mod_design.R` vivent **hors** du `moduleServer` : `input`
n'y est pas visible, le thème leur est donc **passé en argument**
(`theme_gg`). Le lire depuis `input` y aurait été impossible, et l'appliquer
après coup aurait effacé leurs réglages fins — un thème complet remplace tout
ce qui précède.

#### Un seul choisisseur de thème

Le test qui garde cette règle a lui-même dû être corrigé, et l'erreur mérite
d'être notée : il comparait deux propriétés **choisies à la main**
(`panel.background`, `panel.grid.major`) pour décider si un thème « valait
minimal ». Il passait en local et **échouait en intégration continue** sur
« Sans décor » — selon la version de ggplot2, ces deux propriétés-là
coïncident avec celles de `theme_minimal()` sans que les thèmes soient pour
autant les mêmes.

Un test ne doit pas dépendre de **la propriété par laquelle** deux objets se
distinguent. La branche par défaut du `switch` rend exactement
`theme_minimal(base_size)` : c'est donc l'**objet complet** qu'on compare, et
un nom inconnu est vérifié comme retombant bien sur minimal.

`hstat_apply_plot_opts()` portait un **second** `switch` sur le nom du thème,
et il avait dérivé : cinq thèmes connus sur les huit du catalogue. « Gris »,
« Traits fins » et « Sans décor » retombaient **en silence** sur « Minimal » —
l'utilisateur changeait le réglage et l'image ne bougeait pas, sur treize blocs
d'export. Il passe désormais par `viz_get_theme()`, et un test échoue sur tout
nouveau `switch` dont les étiquettes sont des noms de thème.

### Un repère qu'on ne peut pas saisir est un repère qui n'existe pas

« Valeur du seuil (%) » portait `min = 0, max = 100`. On ne pouvait donc pas
poser de repère sur une efficacité **négative** — la modalité fait moins bien
que le témoin, c'est précisément le résultat qu'on cherche à lire — ni au-delà
de 100. Les bornes de l'axe portaient de même un `max` arbitraire (100 et 200).
Toutes ont sauté : un axe doit aller aussi loin dans le négatif que dans le
positif.

**L'étendue automatique inclut le seuil.** Avec des limites automatiques,
ggplot entraîne bien son échelle sur les couches — un `geom_hline` étend la
plage. Mais les **graduations** calculées à la main (`seq(min, max, pas)`)
s'arrêtent, elles, à l'étendue des *données* : la ligne était tracée et aucune
graduation ne disait à quelle hauteur elle passait. `hstat_etendue_axe()` prend
donc les données **et** les repères à faire tenir dans le cadre.

Et quand l'utilisateur fixe lui-même des limites qui excluent le seuil, on le
dit : une ligne de seuil absente est plus trompeuse qu'une barre manquante — on
croit lire un graphique sans seuil alors qu'on en a demandé un.

Un test balaie les champs de bornes d'axe et de valeur de repère
(`thresholdValue`, `thresholdYMin/Max`, `yAxisMin/Max`, `xAxisMin/Max`,
`refValue`) et échoue sur toute borne `min`. Vérifié au passage :
`refSigma` et `refMargin` gardent la leur, un écart-type et une marge
d'équivalence étant positifs par définition.

### Un titre d'axe plus long que son axe doit revenir à la ligne

`element_text()` et `element_markdown()` ne reviennent **jamais** à la ligne :
le titre sort du cadre et se fait rogner à l'export. Sur un intitulé explicite
— « Rendement moyen par parcelle en t/ha » — c'est le cas normal, pas le cas
rare.

`hstat_axe_titre()` passe par `ggtext::element_textbox_simple()`, qui enveloppe
le texte dans une boîte de la **largeur réelle de l'axe** : le retour à la
ligne se fait tout seul, et non à un nombre de caractères deviné. `halign` cale
les lignes **entre elles** — c'est ce que l'utilisateur appelle centrer ou
aligner ; le `hjust` d'`element_text()` ne fait pas la même chose, il déplace
un texte d'un seul tenant.

**Pas d'entrée « Justifié »** : gridtext ne sait pas répartir le texte entre
les marges. L'offrir donnerait un réglage que l'image ignore — le défaut même
que ce dépôt traque ailleurs. Trois calages, tous les trois réels.

Repli sur `element_markdown()` quand le retour à la ligne n'est pas demandé ou
que ggtext manque : le style (gras, italique, taille, couleur) survit dans les
deux cas.

#### Les deux réglages passent par un helper, jamais par recopie

`hstat_axe_titre_ui()` pose la case et le sélecteur, `hstat_axe_titre_lire()`
les relit. **Sept** modules les portent désormais — Descriptives, Exploration
(distribution et valeurs manquantes), Visualisation, Seuils d'efficacité,
comparaisons post-hoc, Qualitatif.

Les deux premiers modules équipés avaient leurs widgets **recopiés à la main**,
avec des identifiants propres (`axisTitleWrap`, `thresholdAxisTitleWrap`). Ils
sont passés au helper : deux copies d'un même réglage, c'est le point de départ
de la dérive qu'on vient de corriger sur les thèmes.

Dans `mod_explore.R`, le graphique est construit **ailleurs que là où `input`
existe** (chemin du téléchargement) : les deux réglages y voyagent en
paramètres, comme les tailles de police. Même contrainte que pour le thème de
`mod_design.R`.

### Export d'image : les pixels saisis sont une mise en page, pas la sortie

`ggsave` raisonne en **pouces** ; le fichier fait pouces × DPI. Diviser les
pixels demandés par le DPI (1200 / 300 = 4 pouces) rendait bien 1200 px de
large — mais sur une toile de **quatre pouces**, où onze étiquettes de
traitement s'écrasent et où le texte, dimensionné en points, occupe une place
énorme.

Le défaut s'aggravait dans le sens même où l'utilisateur cherchait à
l'éviter : 1200 px à 600 DPI font 2 pouces, donc **demander plus de qualité
rétrécissait la figure**. Signalé à l'écran.

`hstat_export_dims()` (`Utils.R`) lit les pixels saisis à la résolution de
référence de l'écran (**96 ppp**, la convention CSS) : ils fixent la mise en
page, celle que l'utilisateur voit. Le DPI multiplie ensuite la finesse.
1200 × 800 à 300 DPI donnent 12,5 × 8,33 pouces rendus en 3750 × 2500 px.

Au-delà de `HSTAT_EXPORT_MAX_PX` (20 000 px de côté), le DPI est abaissé **et
annoncé** : `ggsave` échouerait sur l'allocation du bitmap, et un export
silencieusement dégradé serait pire qu'un refus. Les tests lisent les pixels
**réellement produits** (en-tête IHDR du PNG), et vérifient l'invariant qui
manquait : monter le DPI ne change pas la mise en page et augmente les pixels.

Même conversion pour l'export des analyses multivariées (`app_server.R`), qui
portait le même calcul.

### La taille physique est l'état, les pixels n'en sont que l'affichage

Côté multivarié, les champs de largeur et de hauteur se recalculent quand la
résolution change. Ils le faisaient **en chaînant les pixels** :
`nouveaux = précédents × neuf / ancien`. C'est juste, et c'est fragile — il faut
deux états exacts en même temps : l'ancien DPI retenu par le serveur, et les
pixels **tels que le navigateur les a déjà renvoyés**. Un panneau d'analyse
reconstruit remet les champs à leur valeur d'origine sans que l'ancien DPI
bouge ; deux changements plus rapprochés que l'aller-retour font repartir le
calcul de pixels périmés. Dans les deux cas la taille cesse de suivre la
résolution — le défaut signalé, deux fois.

La taille physique, elle, ne dépend d'aucun des deux. `.mv_pouces` la retient
par bloc d'export ; `hstat_px_pour_dpi()` en déduit les pixels à chaque
changement. Trois conséquences, chacune testée :

1. **L'ancre est relevée sur l'interface, jamais codée en dur** : le premier
   passage lit les champs déclarés, si bien qu'un bloc dont les valeurs par
   défaut changeraient suit tout seul.
2. **Nos propres écritures ne la redéfinissent pas** (`.mv_ecrit`). Sans ce
   garde-fou, un écho arrivé en retard divise d'anciens pixels par la résolution
   *déjà* changée, et la figure rétrécit à chaque cran.
3. **L'export lit l'ancre, pas les champs** (`mv_pouces_export()`) : le fichier
   fait pouces × DPI même si l'affichage n'a pas suivi. Monter la résolution
   monte donc la finesse dans tous les cas, ce qui est la promesse faite.

Un champ de DPI **vidé en cours de saisie** ne recalcule rien. Retomber sur une
valeur par défaut redimensionnerait la figure sous les doigts de l'utilisateur.

Enfin le lien ne se voyait nulle part : il ne restait qu'à le croire. Une note
(`hstat_mv_dim_note_ui()`) écrit sous les trois champs ce que le fichier
contiendra — « 6000 × 4500 px, soit 25,4 × 19,0 cm à 600 DPI ». Les centimètres
sont l'invariant : ils ne bougent pas d'une résolution à l'autre, et c'est
précisément ce qu'il faut comprendre.

### Le DPI monte jusqu'à 20 000, et ne touche jamais à la taille

Une seule règle, pour toute l'application : **augmenter la résolution ne change
ni la largeur, ni la hauteur, ni la mise en page**. Deux exports faisaient
l'inverse — ils multipliaient les pouces par un facteur de réduction, si bien
que demander plus de finesse rendait l'image plus *petite* sur le papier.

`HSTAT_DPI_MAX` (20 000) est le plafond du **champ**, déclaré une fois : trente
et un champs de DPI existent dans l'application, neuf plafonnaient à 1200 ou
2000 et quatre à 600, sans raison. Quatre d'entre eux s'écrivent `DPI` en
majuscules (`distDPI`, `missingDPI`, `corrDPI`, `plotDPIVisible`) et échappaient
à toute recherche de `dpi` — c'est le décompte des champs **dans la page rendue**
qui les a trouvés, pas la lecture du code.

`HSTAT_RASTER_MAX_PX` (20 000 px de côté) est autre chose : la limite du
**format**. Au-delà, le périphérique graphique échoue sur l'allocation du bitmap
et l'utilisateur n'obtient aucun fichier. `hstat_dpi_effectif()` ramène alors la
**résolution** — jamais la taille — et l'annonce, parce qu'un export
silencieusement dégradé est pire qu'un refus.

Conséquence assumée : sur une figure de 12 pouces, la finesse cesse de progresser
vers 1666 DPI. C'est une limite physique du matriciel, pas un choix. **Les
formats vectoriels (PDF, SVG, EPS) ne sont donc pas plafonnés** : leur résolution
est infinie et le DPI n'y veut rien dire — c'est la vraie réponse à « je veux
20 000 DPI sans rien perdre ».

Le plafond vit chez `hstat_ecrire_image()`, l'écrivain commun : vingt exports en
héritent sans que chacun ait à y penser, et aucun ne peut l'oublier.

Deux invariants testés : la taille physique est **constante** quel que soit le
DPI, et le nombre de pixels ne **décroît jamais** quand on demande davantage.

### Onglet Visualisation : la résolution ne rétrécit pas la figure

Un escalier y réduisait la taille physique à mesure que le DPI montait — 12 × 8
pouces jusqu'à 600 DPI, mais **6 × 4 au-delà de 5000**. Demander plus de finesse
rendait donc l'image plus *petite* sur le papier : le défaut s'aggravait dans le
sens où l'utilisateur cherchait à l'éviter. Le même escalier avait déjà été
retiré des analyses multivariées (`calculate_dimensions_from_dpi`) ; il avait
survécu ici, dans l'onglet le plus utilisé pour les graphiques.

La taille physique est fixe, le DPI ne multiplie que la finesse. Mesuré sur les
pixels réellement produits : 3600 × 2400 à 300 DPI, 7200 × 4800 à 600,
14 400 × 9600 à 1200 — la mise en page ne bouge pas.

Seul `HSTAT_VIZ_MAX_PX` (16 000 px de côté) peut encore la réduire, là où
`ggsave` échouerait sur l'allocation du bitmap et où l'utilisateur n'obtiendrait
**aucun** fichier. Cela ne joue qu'au-delà de 1200 DPI, quand l'escalier agissait
dès 600.

**Le calcul vivait en deux exemplaires** : le téléchargement, et le panneau qui
*annonce* ce que le fichier contiendra. Corriger l'un sans l'autre aurait fait
annoncer 6 × 4 pouces pour un fichier de 12 × 8 — c'est un test qui l'a
rattrapé. `hstat_viz_export_dims()` est désormais la seule source, appelée des
deux côtés.

La **qualité JPEG** et la **compression TIFF** étaient déclarées dans l'interface
et lues nulle part : deux réglages que l'utilisateur déplaçait sans effet. Ils
sont branchés.

### Chercher du code mort : `nom(` ne suffit pas

Un balayage qui ne cherche que les *appels* croit mortes les fonctions passées
**en valeur** (`sapply(df, is_categorical)`, `mapply(interpret_manova_effect, …)`,
`breaks = .hstat_code_breaks3`) et celles utilisées comme **argument par défaut**
(`path = hstat_i18n_path()`, qui vit sur la ligne de définition d'une *autre*
fonction). Cinq modules ont été cassés ainsi avant que la vérification ne le
signale.

Le critère juste est celui du nom nu : une fonction est morte si son nom
n'apparaît **qu'une seule fois** dans tout le dépôt — sa propre définition. Douze
l'étaient, elles ont été retirées ; un test empêche leur retour **et** garde les
cinq fausses mortes.

Même critère pour les sorties : vingt sorties `chiSq*` et trois téléchargements
étaient calculés sans être affichés nulle part, alors que le chi-deux réellement
accessible vit ailleurs dans le même fichier. Le risque n'était pas le poids,
c'était de corriger la copie morte en croyant corriger l'analyse — la leçon déjà
tirée de `createPlotDownloadHandler`.

Attention au faux positif symétrique : une sortie **placée dynamiquement**
(`uiOutput(paste0("mv_", key, "_controls"))`) n'apparaît qu'une fois elle aussi.
Il faut vérifier que le module ne construit aucun identifiant — c'est le cas de
`mod_tests.R`, ce n'est pas celui de `UX.R`.

### Plotmath ne survit pas à `ggplotly`

Les étiquettes d'axe en gras passent par `bquote(bold(...))` : `ggsave` les
rend, `ggplotly` les **déparse**, et l'axe affichait `bold("2SP(0,5)&2PV")` en
toutes lettres. Le style est donc retenu à part (`x_label_styles`) et rejoué en
HTML sur l'objet plotly (`hstat_html_style_label()`), que plotly comprend. Le
texte est **échappé** avant d'entrer dans la balise — un « & » dans un nom de
traitement la casserait.

### Multi-courbes : les Y sont empilées, elles doivent partager un type

`pivot_longer` met toutes les Y dans **une** colonne. Choisir la variable d'axe
X aussi en Y — une date, sur un fichier de suivi — levait « Can't combine
`Semaine` <date> and `ch_Hel` <double> », erreur qui tombait dans
l'observateur et emportait **tout** le graphique.

`hstat_y_multi_valides()` écarte l'axe X (l'empiler comme mesure n'a pas de
sens) et, en cas de types mélangés, garde le quantitatif — c'est ce qu'une
courbe représente. Ce qui est écarté est **nommé** ; le pivot reste malgré tout
sous `tryCatch`, un type exotique devant rendre un message et non faire tomber
l'onglet.

### Un tableau à une colonne n'est plus un tableau après `df[cond, ]`

`plotData()` (`mod_viz.R`) ne garde que les colonnes utiles au graphique : il
n'en reste **qu'une** quand X et Y désignent la même variable, ou quand
l'agrégation a renommé Y. Le premier filtrage de lignes ramenait alors le
tableau à un vecteur — `[.data.frame` simplifie par défaut — et ggplot rendait
« `dim(data)` must return an `<integer>` of length 2 », message que personne ne
peut relier à son choix de variables.

Tout sous-ensemble de lignes du chemin graphique porte donc `drop = FALSE`, et
`createPlot()` nomme le cas restant (variable absente des données préparées)
plutôt que de laisser passer le message de ggplot. Un test balaie `mod_viz.R`.

### Ne jamais appeler une fonction de rendu à la main

`DT::renderDT(...)()`, `shiny::renderTable(...)()` depuis un `renderUI` échouent
avec « argument "name" is missing » : une fonction de rendu attend la session et
le nom de sortie que Shiny lui passe. Deux fois le piège dans ce dépôt. Il faut
soit une sortie dédiée (`DTOutput` + `renderDT`), soit un composant statique
(`.hstat_html_table()` dans `mod_ai.R`).

### Assistance IA : une table de fournisseurs, trois protocoles

`mod_ai.R` porte `HSTAT_AI_FOURNISSEURS` — **une ligne par service**. La liste
de choix, le diagnostic, l'aiguillage et l'interface en dérivent tous. Ajouter
un service, c'est ajouter une ligne ; et un service qui parle le protocole
d'OpenAI (la plupart) ne demande **aucun code**.

Trois protocoles seulement : `openai` (ChatGPT, DeepSeek, Kimi, GitHub Models,
serveur local), `anthropic` (Claude), `gemini` (Google, seul à ne parler ni
l'un ni l'autre). L'aiguillage se fait sur le **protocole**, jamais sur le nom
du service.

**Le défaut est `"auto"`**, la thématisation statistique : gratuite, hors ligne,
sans clé, et le seul moteur garanti disponible partout. Une fonctionnalité
facturée à l'usage ne doit jamais devenir le chemin par défaut d'un utilisateur
qui n'a rien demandé — un test garde l'ordre de la liste (le gratuit avant le
payant) et le défaut des deux fonctions.

**Ollama a été retiré.** Son protocole lui était propre (`/api/chat`, la route
des modèles installés, `format: "json"`) : il portait son constructeur de corps
et son lecteur de réponse. Un serveur local compatible OpenAI (llama.cpp,
LM Studio, vLLM, Jan) rend le même service par le chemin commun, et reste
proposé — il ne dépend pas d'Ollama, il ne coûte rien, et rien n'obligeait à le
supprimer avec lui.

#### Une clé n'est jamais ambiante

Chaque service lit **sa propre** variable d'environnement : une clé OpenAI ne
doit pas servir à appeler DeepSeek. Et surtout, aucune variable **courante** :
`GITHUB_TOKEN` existe sur quantité de postes et dans toutes les intégrations
continues. La lire d'office enverrait un jeton chez un tiers sans acte de
l'utilisateur — constaté à l'écran, le moteur GitHub Models s'annonçait
« disponible » tout seul, avec le jeton du conteneur. D'où
`GITHUB_MODELS_TOKEN`, que l'on ne pose que pour cela. Un test l'exige.

#### Adresses et modèles sont des valeurs par défaut, pas des constantes

Un service qui déménage ou renomme son modèle ne doit pas obliger à rouvrir le
code : les deux sont éditables dans l'interface, et la valeur saisie l'emporte
toujours.

#### Les corps de requête restent testables sans serveur

`.hstat_ai_body_openai()` et `.hstat_ai_body_gemini()` sont séparés de l'envoi
réseau : c'est la partie qui casse **en silence** quand un fournisseur renomme
un champ. Gemini range la consigne système dans `systemInstruction` et le texte
dans `contents[].parts[]` — rien de commun avec les deux autres, donc un
constructeur et un lecteur à lui.

`httr` et `jsonlite` restent en **Suggests** : sans eux, `"auto"` fonctionne
toujours.

### Modules imbriqués : la dépendance reste, la contrainte a disparu

`mod_qualitative_ui()` appelle `mod_coding_ui()` (l'atelier de codage CAQDAS
vit dans son propre fichier plutôt que d'alourdir les ~2900 lignes de
`mod_qualitative.R`). C'était la contrainte d'ordre la plus visible : `HStat.R`
devait sourcer `mod_coding.R` avant `mod_qualitative.R`, et un test le gardait.

Les deux fichiers étant dans `R/`, ce sont des définitions : l'appel n'a lieu
qu'au rendu de l'onglet, bien après. La dépendance existe toujours ; elle est
devenue **sans objet**.

### Atelier de codage : arbre de codes et mémos

`mod_coding.R` porte deux structures que MAXQDA a et qu'un livre de codes plat
ne peut pas remplacer.

**La hiérarchie** (`parent_id`, `""` à la racine). Sans elle, l'arborescence
finit encodée dans les libellés (« Prix - trop cher »), ce qui interdit toute
agrégation : `hstat_code_counts()` rend donc `n_seg` (le code seul) **et**
`n_seg_cumul` (toute la branche). Un total qui ignorerait les sous-codes
afficherait zéro sur un code parent pourtant abondamment documenté.

Trois invariants, chacun testé :

1. **Le même libellé est permis sous deux parents différents**, interdit deux
   fois sous le même. « Prix > Qualité » et « Service > Qualité » sont deux
   codes légitimes ; c'est même l'intérêt de la hiérarchie.
2. **Supprimer un code ne supprime pas le codage de ses enfants** : ils
   remontent d'un niveau, sauf `avec_descendants = TRUE`.
3. **Un code ne peut devenir son propre descendant.** `hstat_code_update()`
   refuse le déplacement — la branche se détacherait de l'arbre et
   disparaîtrait de l'affichage.

`hstat_code_migrate_codebook()` répare au chargement : colonne absente, parent
introuvable (remonté à la racine), et **cycle** (un fichier édité à la main peut
en contenir un ; `hstat_code_tree()` boucrerait indéfiniment).

### Les quatre analyses croisées : chacune a son piège

**`hstat_code_query()` — la portée change le sens, elle doit être annoncée.**
« Même document » (les deux thèmes coexistent chez la personne), « même
passage » (le même extrait porte les deux étiquettes) et « à proximité » (les
idées se suivent sans se superposer) donnent des effectifs très différents pour
la même question — 23, 16 et 3 sur le corpus d'essai. Le résultat porte donc la
portée employée en attribut, et l'interface l'affiche. `OU` ne croise rien : sa
portée vaut `NA`, l'afficher laisserait croire le contraire.

**`hstat_code_kwic()` — le motif de l'utilisateur est échappé par défaut.**
Taper « prix (cher) » ne doit ni lever « unmatched parenthesis » ni chercher un
groupe de capture. En mode `regex = TRUE`, une expression invalide rend zéro
ligne : elle lève tantôt une **erreur**, tantôt un simple **avertissement**, et
ne rattraper que l'erreur laissait passer le second dans la console.

**`hstat_code_codeline()` — la position est en pourcentage, pas en caractères.**
C'est tout l'intérêt : deux réponses de longueurs très différentes deviennent
comparables. Un document vide garde des positions finies au lieu de produire
des `Inf` silencieux par division par zéro.

**`hstat_code_accord()` — l'unité est le couple document × code.** Deux codeurs
ne découpent jamais aux mêmes bornes ; comparer des segments exigerait un seuil
de recouvrement arbitraire qui ferait varier le résultat plus que le désaccord
réel. Seuls les documents que **les deux** ont vus comptent — sinon l'absence
de codage d'un document jamais ouvert passerait pour un désaccord.

Kappa n'est **pas toujours défini** : si les deux codeurs posent (ou omettent)
tout partout, l'accord attendu par hasard vaut 1, le dénominateur s'annule et
kappa rend `NaN`. Brancher dessus lèverait « missing value where TRUE/FALSE
needed ». D'où le quatrième état `indeterminable`, et l'affichage du
pourcentage d'accord, lui toujours calculable.

### Le codeur fait partie de l'identité d'un segment

`hstat_seg_add()` refusait un doublon sur (document, code, bornes) **sans le
codeur**. Deux codeurs qui étiquettent le même passage à l'identique — c'est-à-
dire l'accord parfait, le cas le plus courant — voyaient le second codage
silencieusement écarté, et l'accord inter-codeurs portait sur un corpus amputé.
Le test de doublon inclut donc `source`, ce qui préserve l'intention d'origine :
un double-dépôt accidentel du **même** codeur ne gonfle toujours pas les
effectifs.

**Les mémos** (`hstat_memo_*`) portent sur un code, un document, un segment, ou
rien (mémo libre). C'est la pièce qui transforme un codage en analyse. Le mémo
de code existait déjà comme colonne du livre de codes : il y reste, et
`hstat_memo_sync_codes()` le reprend dans le registre — idempotent, pour qu'un
projet ancien ne perde rien et ne duplique rien.

Le texte d'un mémo est **échappé** avant affichage (`hstat_html_escape`) : c'est
de la saisie utilisateur, elle n'entre pas telle quelle dans le DOM.

## Intégration continue

`.github/workflows/tests.yml` — deux travaux : `syntaxe` (analyse de tous les
fichiers R **sans installer une seule dépendance**, plus la cohérence des
versions) puis `tests` (la suite testthat).

Les paquets sont installés depuis une **liste explicite**, jamais depuis les 107
`Imports` de `DESCRIPTION` : la suite ne source que quatre fichiers, et
installer tout ferait durer la CI plus d'une heure sans rien tester de plus.
La liste sépare l'indispensable (`testthat`, `shiny`, `shinydashboard` — sans
eux la suite ne démarre pas) du souhaitable (dont l'absence transforme des tests
réels en tests sautés, **en silence**). Un test qui exige un nouveau paquet doit
l'ajouter à cette liste.

## Journal de reproductibilité

`hstat_rlog_*` (`mod_ai.R`) reconstitue le script R de la session à partir de
`values$aiHistory`, alimenté par `hstat_ai_capture()`.

Règle de conduite : quand le code exact n'est pas reconstituable fidèlement
(réglages interactifs, hyperparamètres de ML), écrire un **commentaire**
`NON RECONSTITUÉ`, jamais du code plausible. Un script qui différerait en
silence de ce que l'application a calculé serait pire que pas de script. Un test
vérifie que le script généré **s'analyse et s'exécute** réellement.

### Le bioessai est la seule analyse dont les données ne viennent pas du fichier

Elles sont saisies dans le module, ou lues d'un fichier WIN DL. Le journal les
**reporte** donc dans le script : il s'exécute sans `mon_fichier.csv`, et il est
exactement reproductible.

Et il n'est écrit **que quand il est fidèle**. À mortalité naturelle déclarée
nulle, le modèle de Finney est un GLM binomial à lien probit : le script sort un
`glm()` et un `MASS::dose.p()`, et un test vérifie qu'il rend **les mêmes
chiffres** — pas seulement qu'il s'exécute. Dès que `c` est estimée (EM) ou fixée
par Abbott, ce n'en est plus un — `glm()` ne sait pas ajuster `c` dans
`p = c + (1 − c)·F(a + b·log d)` — et le journal écrit `NON RECONSTITUÉ` avec les
paramètres obtenus, plutôt qu'un `glm()` plausible et faux.

`.hstat_rlog_num()` écrit les nombres à quinze chiffres par `formatC` : `format()`
respecte `OutDec`, et une locale française produirait « 0,00063 » que R refuserait
d'analyser — sur le seul artefact dont la promesse est d'être exécutable.

## Rapport automatique

`mod_report.R` assemble en un document ce que la session a produit. Il ne
calcule rien : il met en forme `values$aiHistory`, comme le journal.

Le corps est écrit en **markdown**, parce que c'est le seul format que pandoc
convertit vers Word *et* PDF, et qu'on sait aussi rendre en HTML sans lui.

**Le HTML est le format de référence, et il ne peut pas échouer** : il est
assemblé en R, sans dépendance externe. Word et PDF passent par `rmarkdown`,
donc par pandoc (et LaTeX pour le PDF) — outils absents de beaucoup de postes.
Quand ils manquent, `hstat_report_render()` rend le HTML et **renvoie le motif
du repli** dans `res$message` ; l'appelant doit l'afficher. Un utilisateur qui
demande du Word et reçoit du HTML sans explication croit à un bug. Même règle
pour le nom du fichier : `rep_format()` calcule le format *effectivement*
produit avant de nommer le téléchargement — un `.pdf` contenant du HTML ne
s'ouvrirait pas.

**Deux convertisseurs markdown → HTML, et c'est voulu.** Celui de `mod_ai.R`
(`.hstat_md_to_html`) ne connaît que titres, gras et listes : il suffit à une
réponse de modèle. Le rapport, lui, est fait de **tableaux**, de blocs de code
et de figures — passé au premier, un tableau ressortait en barres verticales au
milieu d'un paragraphe. D'où `.hstat_rep_md_to_html()`, propre au rapport.

**Les figures sont déposées en fonction, pas en objet** :
`hstat_ai_capture(..., plot = function() shiny::isolate(createPlot()))`. Elles
ne sont dessinées qu'au moment où un rapport les réclame, et une figure devenue
indessinable (variable supprimée entre-temps) disparaît du document au lieu de
le faire tomber. `isolate()` est indispensable : le téléchargement d'un rapport
n'est pas un contexte réactif, et un réactif ne s'y lit pas.

**Les figures sont tracées pour l'impression : 1000 dpi au minimum.**
`HSTAT_REPORT_DPI_MIN` est un **plancher**, pas une valeur par défaut — une
résolution inférieure passée par un appelant est remontée. Les revues exigent
couramment 300 à 600 dpi ; à 150 dpi une figure est nette à l'écran et floue sur
papier, et le défaut ne se voit qu'une fois le document remis.

Seul l'aperçu à l'écran y échappe (`apercu = TRUE`, 150 dpi) : il sert à
vérifier la mise en page, et incorporer 9000 px en base64 dans un onglet
rendrait la page poussive sans rien ajouter de visible.

Le coût a été mesuré, pas supposé : à 9 × 5,5 pouces, un nuage de points pèse
0,36 Mo à 1000 dpi (0,04 Mo à 150) pour 2,3 s de tracé. D'où le rappel
`progres` — sans lui, vingt figures font une minute de silence après le clic.

Les tests vérifient les **pixels réellement produits** (en-tête IHDR du PNG),
jamais l'argument passé : un `ggsave` qui ignorerait le `dpi` passerait
autrement inaperçu.

Le HTML **incorpore ses images en base64**. Un rapport qui pointerait vers
`/tmp` s'afficherait sans ses figures dès qu'on l'envoie à un relecteur.

Corollaire sur l'historique : `hstat_ai_capture()` conserve désormais les
tableaux (100 lignes) et les figures des `HSTAT_HIST_DETAIL` dernières analyses,
puis les allège, et borne l'historique à `HSTAT_HIST_MAX`. La mémoire reste
bornée quelle que soit la durée de la session.

Piège corrigé : `hstat_report_markdown(sections = ...)` attend les **valeurs**
de `HSTAT_REPORT_SECTIONS` (`"donnees"`, `"qualite"`…), pas ses noms, qui sont
les libellés affichés. Passer les noms vidait le rapport **en silence** — seul
l'en-tête sortait.

## Seuils d'efficacité : comparer chaque modalité au témoin

`hstat_efficacite()` applique la formule d'Abbott — celle de l'agronomie, de la
phytopharmacie et de l'entomologie :

```
efficacité (%) = (témoin − traitement) × 100 / témoin
```

Le module ne faisait que **tracer** des courbes à partir d'une variable
d'efficacité calculée ailleurs ; le calcul se fait désormais dans
l'application, en boucle sur toutes les modalités.

Quatre décisions, chacune testée :

1. **Le témoin vaut zéro par définition, et on l'écrit.** La formule le donne
   bien… sauf si sa valeur est nulle, où elle rend `NaN` (0/0). On pose donc 0
   explicitement : le témoin ne se compare pas à lui-même.
2. **Un témoin nul rend l'efficacité indéfinie pour tout le monde.** Diviser
   par zéro produirait des `Inf` silencieux, qui ressortiraient en graphique
   comme des barres démesurées. On rend `NA` **et on le dit**
   (`attr(res, "message")`).
3. **Une efficacité négative est un résultat, pas une erreur.** Elle signifie
   que la modalité fait moins bien que le témoin ; la borner à zéro masquerait
   précisément ce qu'il faut voir.
4. **Le groupement facultatif est ce qui rend la suite possible.** Sans lui, il
   n'y a qu'une ligne par modalité et plus rien à tester. En calculant
   l'efficacité **dans** chaque répétition (bloc, essai, site), on obtient une
   vraie variable, analysable ensuite par ANOVA ou comparaisons multiples.

Les colonnes `Groupe` et `Variable` **disparaissent** quand elles n'apportent
rien (pas de groupement, une seule variable mesurée) : une colonne vide fait
croire à une information absente.

Le tableau peut remplacer le jeu de travail (`values$data` / `cleanData` /
`filteredData`), ce qui le rend analysable par les autres onglets. Le geste est
annoncé sans détour : remplacer les données de quelqu'un sans le prévenir
serait le pire des services.

### Deux façons de tenir compte des répétitions, deux questions

- **« En commun »** (`mode = "cumul"`) — la moyenne ou la somme de la modalité
  porte sur **toutes ses répétitions** : une efficacité par modalité. C'est le
  chiffre du rapport.
- **« Par répétition »** (`mode = "par_repetition"`) — l'efficacité est calculée
  **dans** chaque répétition : autant de valeurs que de répétitions, donc une
  variable analysable par ANOVA ou comparaisons multiples.

La variable de répétition se déclare dans les deux cas ; c'est le `mode` qui dit
ce qu'on en fait. Le décompte réel apparaît en colonne `Repetitions`.

`var_groupe`, l'ancien argument, **garde son sens d'origine** (découpage par
groupe) : lui donner le nouveau ferait passer un appel existant de 12 lignes à
4, en silence.

### La somme n'est comparable qu'à répétitions égales

C'est le piège de ce module, et il est massif. Avec un nombre de répétitions
inégal, la modalité la plus répétée accumule mécaniquement davantage et
ressort **artificiellement moins efficace** — un artefact de plan pris pour un
résultat. Constaté à l'écran sur un essai où T1 n'a qu'une répétition contre
trois :

| Résumé | Efficacité de T1 |
|---|---|
| moyenne | **60 %** (juste) |
| somme | **86,7 %** (artefact) |

La moyenne n'en souffre pas : le rapport est invariant par changement
d'échelle, et à répétitions équilibrées les deux donnent exactement le même
chiffre — un test le vérifie. L'application signale le déséquilibre plutôt que
de laisser publier le second chiffre.

### Un groupe sans témoin est un défaut de plan, pas de mesure

« Témoin sans valeur mesurable » couvrait aussi le cas où le témoin est
simplement **absent** du groupe. Les deux causes sont différentes et appellent
des gestes différents ; le message nomme désormais le groupe fautif et renvoie
à la variable de groupement.

Constaté en groupant par une colonne qui compte une modalité par ligne — le cas
qu'un utilisateur produit sans y penser en choisissant la mauvaise colonne.

### `<-` ou `<<-` : le miroir du piège de `mod_tests.R`

Dans le **corps** d'une fonction, une boucle `for` ne crée pas de cadre : `<<-`
y écrit dans l'environnement **englobant** et saute la variable locale, qui
reste vide. C'est l'exact opposé du cas des gestionnaires d'erreur, où `<<-`
est indispensable.

Le balayage du dépôt ne couvre que les gestionnaires ; celui-ci s'est vu parce
que le message attendu ne sortait pas. Un test vérifie que la liste se remplit.

### Le graphique lit une source, pas `values$filteredData`

Tracer les efficacités calculées obligeait à **remplacer** le jeu de travail
puis à re-choisir X et Y — un détour qui fait perdre le fichier d'origine pour
un simple graphique. Le module passe donc par `source_data()`, qui rend soit le
fichier chargé, soit le tableau d'efficacités. Le bouton « Utiliser comme jeu
de données » reste, mais pour les **autres** onglets ; ici il n'est plus requis.

Quand la source est le tableau calculé, `Modalite` et `Efficacite` sont
présélectionnés : ses colonnes portent des noms connus, offrir la première
colonne venue serait gratuit.

### Un réactif ne s'appelle jamais lui-même

La bascule ci-dessus a été posée par substitution mécanique de
`values$filteredData` en `source_data()` — qui a touché **le corps de
`source_data` lui-même**. R s'arrête alors sur « C stack usage is too close to
the limit » et **l'application ne démarre plus du tout** : une panne totale
pour une ligne, invisible à l'analyse syntaxique puisque le code est
parfaitement valide.

Un test balaie les `X <- reactive({...})` et échoue si le corps appelle `X()`.
Il **retire les commentaires par l'analyseur de R**, pas par une heuristique :
il s'était signalé lui-même sur le commentaire documentant la correction, et un
faux positif permanent finit toujours par faire désactiver le test.

## Doses et dilutions : chaque résultat porte sa formule

`mod_dosage.R` refait les trois calculs qu'un essai phytosanitaire pose sur un
coin de table à chaque campagne. Ils reposent sur deux égalités, et sur rien
d'autre :

```
grammage = dose × concentration              (conservation de la matière)
Vi × Ci  = Vf × Cf                           (conservation du soluté)
```

Règle de conduite du fichier : **chaque résultat est accompagné de la formule
qui l'a produit**, à l'écran comme dans le fichier exporté. Un chiffre de dose
qu'on ne peut pas refaire à la main ne sera pas appliqué au champ — il sera
recalculé, et c'est le recalcul qui fera foi. `HSTAT_DOSE_FORMULES` les écrit
une seule fois : affichage et export lisent la même chaîne, elles ne peuvent
donc pas diverger.

### Une unité inconnue ne vaut jamais 1

`.hstat_dose_facteur()` rend `NA`, jamais 1. C'est la faute la plus coûteuse
que ce module puisse commettre : un facteur muet rendrait un résultat mille
fois trop grand **sans le moindre signe**, et la dose serait appliquée telle
quelle. Les deux sens (dose → grammage et grammage → dose) sont vérifiés comme
exactement réciproques.

### Le prélèvement se fait toujours dans la solution mère

La gamme reste géométrique — `C_n = C_(n-1) / k`, donc `C_n = C_mère / k^n` —
mais **Vi se lit sur la mère**, pas sur la fille précédente :

```
Vi_n × C_mère = Vf × C_n     d'où     Vi_n = Vf / k^n
```

Confondre les deux donne des volumes justes au premier étage et faux partout
ensuite, ce qui ne se voit pas. Le volume final `Vf` est celui que
l'utilisateur déclare, pour **chaque** fille ; l'eau à ajouter en est la
différence, `Vf − Vi`, à chaque fois.

Conséquence assumée : le prélèvement est divisé par `k` à chaque étage et
devient vite impipetable — 100 mL au 1/10 demandent 0,001 mL au cinquième
étage. Le module **le signale** (`attr(res, "avertissement")`) au lieu de
rendre un nombre que personne ne peut mesurer, et il calcule quand même :
c'est un plan de gamme à refaire, pas une erreur de calcul.

### Ce qui appartient au produit ne se répète pas sur ses matières actives

L'unité de saisie est le couple (produit, matière active) : c'est ce qui permet
de détailler la concentration fille de **chaque** matière active tout en
donnant la concentration totale de la solution. Mais le coefficient, le nombre
de filles et le volume final décrivent la **préparation** : deux valeurs
différentes sous le même nom de produit décrivent deux préparations, et le
module le refuse en nommant le produit plutôt que d'en retenir une au hasard.

De même, le volume à prélever, l'eau à ajouter et la concentration totale ne
figurent que sur la **première ligne** du couple (produit, rang) : les répéter
sur chaque matière active ferait croire qu'il faut prélever autant de fois.

Les concentrations sont ramenées à g/L avant d'être sommées : additionner des
pour-cent et des mg/L donnerait un total qui ne veut rien dire.

## DL50 / CL50 : reproduire WIN DL, au chiffre près

`mod_dl50.R` refait ce que faisait **WIN DL** (CIRAD — Giner & Joly 1993 pour
la version DOS, MABIS et le Programme Coton pour la version Windows) :
régression probit dose-mortalité, doses létales, intervalles, comparaison
d'essais.

Le modèle est celui de Finney (1971), et rien d'autre :

```
P'(d) = c + (1 − c) · F(a + b · log10(d))        DLp = 10^((F⁻¹(p) − a) / b)
```

**La conformité numérique est vérifiée contre le logiciel lui-même**, pas
contre une réimplémentation. Le fichier de résultats `CL94AC1.PRN` (7 doses,
25 insectes, témoin 25/0) est reproduit à 5 ou 6 chiffres significatifs sur
`a`, `b`, `c`, les deux log-vraisemblances, le Chi-2, les six termes de
variance et les DL10/50/90 avec leurs bornes. Ces valeurs sont **inscrites en
dur dans la suite de tests** : elles ne dépendent d'aucun fichier extérieur,
sans quoi le test qui garde le noyau ne tournerait pas en intégration
continue.

### L'inverse normale de WIN DL est celle de Hastings, pas celle de R

C'est la troisième convention, et la plus coûteuse à ignorer. Le manuel l'écrit
— « algorithme d'approximation polynomiale de la distribution normale inverse de
HASTINGS » — et les chiffres publiés le confirment. C'est la formule 26.2.23
d'Abramowitz & Stegun, dont l'erreur absolue est bornée par **4,5 × 10⁻⁴** :

```
F⁻¹(p) ≈ t − (c₀ + c₁t + c₂t²) / (1 + d₁t + d₂t² + d₃t³),   t = √(ln(1/q²))
```

Cette erreur **est visible à la précision d'impression du logiciel**. Sur l'essai
de référence, la DL90 en log-dose :

| normale exacte | Hastings | WIN DL imprime |
|---|---|---|
| −0,9412818 | **−0,9410997** | −9,41099e-01 |

L'écart passe de 1,8 × 10⁻⁴ à 7 × 10⁻⁷. C'est ce qui obligeait la suite de tests
à tolérer 1 % sur des bornes que le logiciel imprime à six chiffres.

**Le prix est nul, et il a été mesuré** : 1,8 × 10⁻⁴ en probit fait 0,04 % sur la
dose — quatre ordres de grandeur sous l'intervalle de confiance de la DL50
elle-même, qui couvre un facteur 20 sur cet essai. On ne perd aucune précision
utile, et on gagne la comparabilité chiffre pour chiffre, qui est la promesse du
module.

**La réciproque n'est pas reprise.** L'approximation de Hastings pour `F` a une
erreur de 7,5 × 10⁻⁸, invisible à la précision d'impression : l'employer
dégraderait la vraisemblance sans rien rapprocher. Le **quantile de confiance**
non plus — les bornes publiées de la DL50 donnent t = 1,95999, contre 1,959964
pour la normale exacte et 1,960395 pour Hastings. C'est la première qui colle.
La frontière est mesurée, pas choisie.

Corollaire pour le journal de reproductibilité : le script émet **la fonction de
Hastings**, pas `qnorm()`. Un script qui rendrait des doses létales différentes
dès le quatrième chiffre cesserait de refaire ce que l'application a calculé.

#### Les valeurs extrêmes sont affectées, pas calculées

Le manuel pose « **6 pour 100 % de mortalité et −6 pour 0 %** ». Une mortalité
corrigée de 0 ou de 1 n'a pas de probit ; la borne à `1e-12` employée jusqu'ici
rendait ±7,0345, un nombre qui ne dépendait que d'elle.

### Le bilan de la confrontation

Sur le fichier de résultats du logiciel (`CL94AC1.PRN`, méthode EM) :
**27 grandeurs, écart relatif maximal 2,5 × 10⁻⁴** — et cette valeur-là est le
Chi-2, que WIN DL n'imprime qu'à trois décimales. Sur les vingt-six autres :
**5,3 × 10⁻⁵**.

Les six essais livrés avec le logiciel portent en fin de fichier les résultats
qu'il a calculés. Ils couvrent deux cas que le fichier de référence ne couvre
pas : un essai dont une dose **tue tout** (CL94AEN) et un essai dont le **témoin
compte des morts** (CL97CY03), donc la correction d'Abbott. Terme constant,
pente et DL50 sont retrouvés à 10⁻⁶–2 × 10⁻⁴ près.

**Ces résultats-là datent du moteur MS-DOS** : leurs bornes sont symétriques en
log, donc issues de la delta-méthode seule — Fieller n'est arrivé qu'avec la
version Windows, et le manuel le décrit. On confronte donc les estimations
ponctuelles et l'**erreur-type**, qui n'en dépendent pas.

Et c'est cette erreur-type qui tranche la question laissée ouverte plus haut :
sous Abbott, elle vaut celle du logiciel **à 0,1 % près** avec l'inversion à deux
paramètres, et s'en écarte d'un facteur 5 avec celle à trois. Le manuel le disait
déjà en toutes lettres — l'estimation par EM « permet de prendre en compte
l'incertitude sur l'estimation de cette mortalité, **ce que la formule d'ABBOTT
ne fait pas** » — les chiffres du logiciel le confirment.

### Saisie en pourcentage : l'arrondi est le sujet, pas la conversion

Beaucoup d'opérateurs notent « 40 % » plutôt que « 12 sur 30 ». La conversion
est triviale ; ce qui ne l'est pas, c'est que le modèle binomial a besoin d'un
**entier**. Trois pièges, tous silencieux, tous testés :

1. **L'arrondi change le pourcentage, et il faut le dire.** 40 % de 7 individus
   font 2,8 : on enregistre 3, soit 42,86 %. Rendre la valeur sans le signaler
   laisse croire que l'essai porte le chiffre saisi.
2. **Deux pourcentages différents donnent le même effectif.** Sur n = 7, 40 % et
   43 % rendent tous deux 3 morts. Une saisie plus fine que l'essai ne
   l'autorise n'ajoute pas d'information, elle en promet une qui n'existe pas.
3. **`round()` arrondit au pair en R** : `round(2.5)` vaut 2, pas 3. « 50 % de 5
   individus » rendrait donc 2, ce que personne n'attend — et le défaut ne se
   verrait que sur les effectifs impairs. On arrondit moitiés vers le haut.

**C'est l'effectif qui est conservé**, le pourcentage n'en est qu'une lecture :
garder le pourcentage comme source obligerait à reconvertir à chaque calcul,
donc à arrondir plusieurs fois, ce qui ne rend pas toujours le même nombre.

Piège d'implémentation attrapé par le test : **les opérateurs vectoriels
recyclent, l'indexation non.** `n[ok]` sur un effectif de longueur 1 et un
masque de longueur 4 rend `20 NA NA NA` — trois lignes sur quatre ressortaient
vides, et seulement quand un seul effectif sert à plusieurs doses, c'est-à-dire
dans le cas le plus courant. D'où le `rep_len()` explicite.

### `innerText` d'une cellule en édition ment, et j'ai construit dessus

J'ai cru **deux fois** qu'une cellule fraîchement modifiée s'affichait **vide**,
et que la note d'arrondi restait sur l'état précédent. Les deux constats étaient
faux, et j'ai fini par bâtir un contournement — sauter la mise à jour du proxy
quand le changement naissait dans la table — pour un défaut qui n'existe pas.

**La mesure était fausse, pas l'affichage.** `innerText` d'une cellule en cours
d'édition rend la chaîne vide, parce que DT y a placé son éditeur
`<input type="number">` et que le texte d'un champ de saisie n'est pas du texte
de nœud. Le pilote Playwright pressait Entrée sans quitter la cellule ; dès que
le focus en sort, elle montre sa valeur. Même chose pour la note : elle se
rafraîchit, on la lisait pendant que l'éditeur tenait encore le focus.

Vérifié en lisant l'`innerHTML` : avant `"3"`, pendant
`"<input type=\"number\">"`, après le blur `"5"`.

**Et le contournement apportait, lui, une vraie régression.** En pourcentage, la
cellule aurait gardé le chiffre **tapé** (« 50 ») alors que l'arrondi range 4
morts sur 7, soit 57,14 % : l'écran aurait cessé de dire la vérité pour éviter
un défaut inexistant. Un test barre désormais la route au retour du drapeau.

Leçon générale, qui vaut au-delà de DT : **quand une mesure automatisée dit
qu'un affichage est vide, vérifier le DOM avant de conclure.** Un champ de
saisie, un `<svg>`, un pseudo-élément CSS ne rendent rien à `innerText` et sont
pourtant parfaitement visibles.

### Le chemin de convergence de l'EM est un choix, et il a été mesuré

HStat part de la mortalité du témoin. Quand celle-ci vaut **zéro** — le cas de
l'essai de référence — l'étape [E] rend des poids nuls et `c` ne bouge plus :
l'ajustement converge en **trois** boucles, sur la vraisemblance la plus haute.

WIN DL, lui, en annonce **75**. La cause est identifiée : son `c` de départ
n'est pas nul, l'EM rampe alors vers la borne, et **il s'arrête avant le
maximum** — sa log-vraisemblance imprimée (−105,63592) est plus basse que celle
que HStat atteint (−105,63582).

Reproduire ce chemin **rapproche** de WIN DL. Sur les 26 grandeurs publiées il
gagne sur **18**, divise l'écart médian par **3,2** (1,3 × 10⁻⁵ → 4,1 × 10⁻⁶),
et `a` sort à 2,19760 comme le logiciel l'imprime au lieu de 2,19759.

Mais il **coûte** : sur un essai à réponse plate, il ne converge plus en cent
itérations là où le départ nul aboutit en trois. C'est pourquoi il est
**proposé, et non imposé** — le défaut reste le chemin rapide, qui atteint un
optimum meilleur sur tous les essais essayés. Le réglage « Chemin de
convergence » ouvre l'autre à qui veut comparer chiffre pour chiffre.

Les deux restent d'ailleurs dans l'enveloppe de conformité : l'écart porte sur
le sixième chiffre, pas sur un résultat.

### Le plafond d'itérations est un réglage, pas une constante

Il valait 500 sans que personne puisse le voir ni le changer. Il vaut désormais
**100 par défaut** et se règle à l'écran — l'essai de référence converge en trois
boucles, et un plafond qu'on peut lever est ce qu'il faut le jour où un essai
difficile n'aboutit pas. Un garde-fou le borne à 5 000, et toute saisie aberrante
(vide, zéro, négative, texte) retombe sur le défaut.

Le plafond de **Newton-Raphson emboîté** dans l'EM reste à **50**, la valeur du
manuel : c'est une convention du logiciel, pas un réglage de confort. Quand
Newton-Raphson est l'ajustement lui-même — Abbott, mortalité nulle — c'est le
plafond choisi par l'utilisateur qui s'applique.

### Les chiffres significatifs sont un réglage d'affichage, et rien de plus

Ils valaient 5 ou 6 selon les tables, en dur. Un seul réglage les gouverne
désormais toutes, borné entre 1 et 15. **Les exports Excel et CSV gardent la
précision complète** : arrondir une donnée exportée la ferait diverger du calcul
dont elle sort.

### Relire des assertions ne suffit pas : il faut les faire mordre

Une assertion peut figer un mauvais comportement au lieu de garder un bon — le
repli sur le premier essai en était un — et cela ne se voit pas à la lecture,
parce qu'une assertion qui fige une tolérance ressemble **exactement** à une
assertion qui garde une règle.

La méthode qui le voit est la **mutation** : on abîme le code d'une régression
plausible, et on regarde si la suite s'en aperçoit. **Une mutation qui passe est
un trou.** `tools/mutation.R` est le banc ; il prend un filtre sur les noms de
tests, ce qui ramène une passe à une vingtaine de secondes au lieu de trois
minutes — c'est ce qui rend la méthode praticable, puisqu'il en faut une par
mutation.

Le banc **source la vraie suite**, il ne réimplémente rien. Une première version
qui refaisait le chargement rapportait 24 échecs sur du code sain : un banc de
mesure qui ment est pire que pas de banc.

Trois pièges de la méthode, tous rencontrés :

1. **Un filtre trop étroit fabrique de faux trous.** Des mutations sont
   ressorties « non attrapées » parce que le filtre excluait les tests qui les
   gardaient. Le banc annonce donc `TESTS: n / total` : un `n` plus petit que
   prévu se voit **avant** d'être lu comme un résultat, et un filtre qui ne
   retient **rien** lève désormais une erreur au lieu de rendre « 0 échec » —
   une faute de frappe dans le motif se lit sinon exactement comme une mutation
   non détectée.
2. **Un mutant équivalent n'est pas un trou** — mais il faut le prouver. Retirer
   la garde de `hstat_coord_mat()` laisse `as.matrix()` rendre la même matrice…
   à une différence près : la colonne perd son nom. Là, c'était bien un trou.
3. **Une mutation peut interrompre le chargement** et faire chuter le nombre
   d'assertions jouées. Un décompte anormalement bas se vérifie avant d'être lu
   comme un succès.

#### Le banc se gardait mal lui-même, et il mentait dans le sens rassurant

Le piège 1 avait une **cause**, pas seulement des manifestations.
`testthat::test_that()` évalue le corps du test **dans le cadre de son
appelant** — ici le filtre du banc, dont l'environnement englobant était
`globalenv()`. Le banc y rangeait son motif sous le nom `motif` ; le balayage
des p-values écrit `motif <- "if\\s*\\([^)]*\\$p\\.value…"` dans son corps,
et **l'écrasait en cours de passe**. Le filtre comparait alors les descriptions
suivantes à une expression régulière de code R, qui ne colle à aucune : tous les
tests d'après étaient **sautés en silence**.

Une passe filtrée pouvait donc rendre « 0 échec » sans avoir joué le test qui
gardait la règle — le résultat exact d'une mutation non détectée. Le banc
annonçait un trou dans les assertions là où il n'y avait qu'un trou dans le
banc, et c'est arrivé trois fois avant d'être compris. **Une erreur d'outil de
mesure va toujours dans ce sens** : un test non joué ne peut pas échouer.

L'état du banc vit désormais dans `.banc`, un environnement que le code testé ne
peut pas nommer. Un test le garde — il fait tourner le banc sur une suite
**miniature** dont le premier test écrit une variable nommée `motif`, et exige
que le second soit quand même joué. Vérifié comme échouant sur les trois
assertions contre la version d'avant correction.

#### Ce que la méthode a trouvé

**Le refus de pente négative portait sur la mauvaise valeur.** Il existait en
double — valeur de départ et valeur ajustée — et aucun test ne les distinguait,
si bien qu'en retirer un laissait l'autre attraper le cas d'essai. Le contrôle
sur la valeur de départ **refusait à tort** : elle vient d'une régression *non
pondérée* sur les seules doses de mortalité intermédiaire, et sur un essai
bruité elle sort négative alors que l'ajustement rend une pente franchement
positive. Mesuré sur quatre mille essais tirés au sort, **23 étaient refusés à
tort** — l'un d'eux passait de −0,36 au départ à **+2,49** ajusté, avec un
message qui accusait l'utilisateur d'avoir inversé ses colonnes.

**Un témoin nul rendait `−Inf`, et rien ne le voyait.** La règle est écrite ici
même — « on rend `NA` et on le dit » — et le **message** était bien vérifié.
Les **valeurs** ne l'étaient pas : retirer le garde-fou ne faisait échouer
aucune assertion des 987 de la suite, et les efficacités sortaient à `−Inf`
avec l'alerte affichée à côté. C'est la forme la plus coûteuse : le tableau
paraît sain parce que le message est là.

**La colonne d'une coordonnée réduite à un vecteur perdait son nom.** La
branche « matrice » du test vérifiait `colnames`, la branche « vecteur » l'avait
oublié — or c'est ce nom qui étiquette l'axe du graphique.

Vingt-trois mutations passées en tout, sur le module DL50 et sur le socle.
Vingt étaient attrapées ; les trois ci-dessus ne l'étaient pas, et toutes le
sont désormais.

#### La méthode appliquée aux modules

Quinze mutations de plus, sur `mod_coding.R`, `mod_report.R`,
`mod_qualitative.R` et `mod_tests.R`. `mod_report.R` les a **toutes** attrapées.
Les autres ont livré cinq trous, et un défaut.

**Le défaut : le niveau de confiance annoncé n'était pas celui qui était
calculé.** Le curseur d'OR/RR va de 0,80 à 0,99 ; la phrase d'interprétation
écrivait `100 * 0.95` en dur. À 99 %, le tableau des mesures affichait
« IC99% » et la phrase juste en dessous « IC95% » — **sur les mêmes bornes**.
C'est la phrase faite pour être recopiée dans un rapport.

**Les échelles accentuées perdaient leur ordre.** Les motifs de détection
étaient dépliés d'accents, pas les modalités : « Élevé / Énormément » — le
français correct — ne rencontrait aucun motif et la variable passait pour
**nominale**, donc sans médiane, sans quartiles et sans test de tendance. Rien
ne signale un type qui aurait pu être meilleur. Le seuil de **deux** mots-clés
est ce qui rend l'inverse sûr : le dépliage rapproche « Élève » (l'écolier) de
« eleve » (le niveau), et une colonne de professions rencontre donc un motif —
un seul. À un seul, l'application ordonnerait des métiers.

**Une case nulle sans correction de Haldane** donnait un OR de 0 et une borne
haute `NaN` — et `corrected` annonçait quand même la correction.

**Un document vide donnait 100 %.** L'assertion existante vérifiait que les
positions restent **finies** ; `100 / 0` vaut `Inf`, que `pmin(100, .)` ramène
à 100. Le segment ressortait donc comme occupant tout le document : fini, et
faux. « Fini » ne suffisait pas.

**Et trois balayages ne lisaient plus aucun module.** Ceux des p-values non
gardées, des coordonnées FactoMineR et des messages R bruts étaient restés
pointés sur `inst/app/` après le déménagement des vingt et un modules dans
`R/` : **cinq fichiers balayés au lieu de vingt-trois**. Les trois défauts
glissés ensemble dans `R/mod_tests.R` passaient tous les trois. Ils passent
désormais par `.hstat_sources_app()`, qui est traversé par tous — et les
modules se sont révélés propres sur les trois comptes, l'élargissement n'est
donc que du gain.

`mod_tests.R` n'a **pas** de fonction de calcul au premier niveau : tout y vit
dans les serveurs. Le banc ne peut donc pas le muter directement, et ce sont ses
balayages qui le gardent. C'est une raison de plus de placer la logique dans
`R/utils.R`.

`mod_viz.R`, `mod_clean.R` et `mod_explore.R` sont dans le même cas — **aucune**
fonction de calcul au premier niveau. La surface réellement mutable de
l'application, c'est `R/utils.R`.

#### Les métriques de modélisation : les noms étaient gardés, pas les nombres

Neuf mutations sur `hstat_metrics_reg()`, `hstat_metrics_cls()` et
`hstat_pair_agreement()`. **Six passaient.** C'est le plus gros gisement trouvé
jusqu'ici, et toujours la même forme : le tableau garde ses lignes, ses libellés
et ses interprétations non vides, et affiche un chiffre faux sous le bon nom.

L'assertion existante vérifiait `expect_setequal(m$Metrique, c("RMSE", "MAE",
"MAPE (%)", "R2"))`, le R² au-dessus d'un seuil, et
`all(nzchar(m$Interpretation))`. Rien sur les trois autres valeurs. Donc :

| Mutation | Ce qui sortait |
|---|---|
| `rmse <- mean(abs(err))` | RMSE = MAE, sous l'étiquette RMSE |
| précision ↔ rappel | les deux échangés, invisible sur matrice équilibrée |
| F1 en moyenne **arithmétique** | 0,657 au lieu de 0,642 — plausible |
| garde de kappa retirée | `NaN` là où `NA` était rendu |
| seuils de `.hstat_interp_r2` déplacés | une phrase française plausible, et fausse |
| seuils de `.hstat_interp_mape` déplacés | idem |

Trois règles en sont sorties :

1. **Vérifier la valeur, jamais seulement le nom.** C'est le même défaut que le
   témoin nul qui rendait `−Inf` avec son alerte affichée à côté : ce qui est
   contrôlé est le décor, pas le chiffre.
2. **La donnée d'essai doit rendre les formules discernables.** Précision et
   rappel coïncident sur une matrice équilibrée : l'échange y est invisible. Le
   test pose donc une matrice **asymétrique**, et vérifie en plus que les deux
   diffèrent — sinon l'assertion se viderait de son sens au premier
   réajustement des données.
3. **Un seuil se teste des deux côtés de sa frontière.** Un palier déplacé
   laisse une phrase parfaitement lisible ; seule la paire (juste avant / juste
   après) le voit.

`hstat_pair_agreement()` — l'indice de Rand qui porte le chiffre de stabilité
par bootstrap de la CAH — n'était appelé par **aucun** test. Sa propriété
essentielle est l'invariance au **renommage des classes** : un k-means relancé
rend les mêmes groupes sous d'autres numéros, et c'est précisément pourquoi on
compare des paires plutôt que des étiquettes. Elle est désormais testée comme
telle.

#### Deux mutants équivalents, et pourquoi ils le sont

Le piège 2 s'est présenté deux fois, et dans les deux cas la garde s'est révélée
**redondante**, non pas mal testée :

- `if (is.logical(x)) next` dans `hstat_vars_zero()` — une colonne booléenne est
  de toute façon écartée en aval, `as.numeric("FALSE")` valant `NA` ;
- `if (n < min_n)` en tête de `hstat_ellipse_ok()` — le contrôle par groupe
  couvre déjà le cas, et le message du mutant **nomme** le groupe trop petit là
  où la garde rend « Aucun groupe exploitable ».

Les deux gardes restent : elles disent l'intention. Mais aucun test n'a été
écrit pour elles — figer un comportement qu'aucune règle n'exige, c'est
exactement l'assertion-tolérance que la méthode cherche à éviter.

Sur 116 des 211 fonctions de `R/utils.R`, **aucun test n'appelle la fonction
directement**. Beaucoup sont des aides d'interface, mais pas toutes :
`multivariate_normality_mardia`, `box_m_test`, `permdisp_test`,
`hstat_silhouette_mean`, `hstat_cophenetic_corr` portent de vraies statistiques.

#### Une taille d'effet de 1,00 tirée d'un degré de liberté nul

Première prise dans ce gisement. `manova_effect_sizes()` calcule
`eta² = 1 - Wilks^(1/s)` avec `s = min(p, ddl_num)`, sans garde sur `s`. À
`s = 0`, `Wilks^(1/0)` vaut `Wilks^Inf`, donc **0**, donc `eta² = 1` — la taille
d'effet **maximale**, que `interpret_manova_effect()` qualifie d'« important ».
`Pillai / 0` sort en `Inf` dans la même ligne.

Le cas n'est **pas atteignable** par le chemin de l'application :
`manova_format_all_stats()` écarte la ligne « Residuals » et ne rend que des
effets à `ddl >= 1`. Le défaut est donc latent — et corrigé quand même, parce
que le mode de défaillance est le pire qui soit : pas une erreur, pas un vide,
mais **le chiffre le plus péremptoire possible**. C'est la même famille que le
témoin nul à `−Inf` et que le kappa à `NaN` ; la règle « ne jamais brancher sur
une statistique non calculable » vaut aussi pour ce qu'on affiche.

#### Les garde-fous des tests multivariés

`box_m_test()`, `permdisp_test()`, `multivariate_normality_mardia()` et
`hstat_silhouette_mean()` portent **onze** sorties anticipées, et aucune
n'était testée. Elles protègent les appels à `heplots`, `vegan`, `psych` et
`cluster`, qui lèvent ou rendent des `NaN` sur données dégénérées — et
l'onglet tombe avec eux. Toutes rendent `NA` **et une phrase** : « Test
impossible » se lit, un `NA` nu ne se lit pas.

Deux détails qui valaient d'être figés :

- **Le message de Box's M nomme les deux nombres** (`min n=2 < p+1=4`). Sans
  eux, l'utilisateur sait que c'est refusé mais pas de combien il manque.
- **Le rang, pas le déterminant.** La décision est écrite dans le corps de la
  fonction ; elle se vérifie. À l'échelle `1e-6`, le déterminant de la
  covariance vaut ~2e-37 — tout seuil sur le déterminant crierait à la
  singularité — alors que le rang reste plein. Des variables mesurées en
  microgrammes ne sont pas colinéaires pour autant.

Et la silhouette d'un groupe unique rend `NA`, **pas zéro** : zéro se lirait
comme « partition indifférente », ce qui est un résultat.

Ces tests ne demandent **aucun** paquet optionnel : chaque garde retourne avant
l'appel au paquet. C'est ce qui les rend réels en CI plutôt que sautés.

#### Le filtre du banc : sous-inclure fabrique de faux trous

Deux fautes du même genre dans le pilote, toutes deux découvertes en lisant un
« aucun test ne l'appelle » invraisemblable :

1. `\b` ne colle pas devant un point — `.hstat_interp_r2` ressortait sans
   couverture ;
2. exiger la parenthèse ouvrante rate `do.call(permdisp_test, cas)`.

Le risque est **asymétrique** : un filtre trop large ne fait que jouer des
tests en trop, un filtre trop étroit annonce un trou qui n'existe pas. Le choix
se fait de ce côté-là.

#### Un nom de colonne préfixe d'un autre cassait la formule

`auto_quote_colnames()` cite entre accents graves les noms portant un caractère
spécial. Le tri par longueur devait suffire — il ne suffit pas. Quand un nom est
le **préfixe** d'un autre et que les deux demandent des accents graves, le court
se réinsère **dans** les accents du long :

```
"A-1-bis + A-1"  →  ``A-1`-bis` + `A-1`
```

R refuse de l'analyser : « attempt to use zero-length variable name ». Le
garde-fou existant cherchait la forme citée **exacte** — `` `A-1` `` ne figure
pas dans `` `A-1-bis` ``, il ne se déclenchait donc jamais.

Le cas n'a rien d'exotique : `Rdt-2023` et `Rdt-2023-corrigé` suffisent, et le
calculateur de variables tombait alors sur un message que personne ne peut
relier à ses colonnes.

Règle : **ce qui est cité sort du jeu.** La fonction masque par un jeton ce que
l'utilisateur a déjà cité *et* chaque citation qu'elle vient de poser, puis
restitue en fin de passe. La réécriture `mean()` → `rowMeans()` opère sur la
chaîne masquée, donc les deux passes se composent sans se gêner.

#### Les transformations : l'aller-retour est l'invariant fort

`apply_variable_transformation()` et `back_transform_values()` n'étaient
appelées par aucun test, alors que les comparaisons post-hoc **affichent des
moyennes rétro-transformées** : une inverse fausse ne lève pas, elle rend des
nombres dans la bonne unité et du mauvais ordre de grandeur.

Sept méthodes, sept allers-retours vérifiés à 1e-10 — plus quelques valeurs
posées, pour que la propriété ne puisse pas être satisfaite par deux fonctions
fausses qui s'annulent. La racine cubique doit accepter les négatifs (c'est sa
raison d'être, et le message d'erreur de `sqrt` y renvoie) : `x^(1/3)` nu rendrait
`NaN`.

Second invariant, entre deux fonctions : **le contrôle de faisabilité doit dire
exactement ce que l'application fera.** Les deux listes de conditions vivent
dans des `switch()` distincts et peuvent donc diverger — une dérive laisserait
soit un bouton actif qui fait tomber la sortie, soit un refus incompréhensible
sur des données valides. Le test croise cinq jeux de données et sept méthodes.

### Trois défauts trouvés par l'audit, tous du même genre

Aucun ne lève, aucun ne laisse un vide : tous les trois rendent un résultat
**plausible et faux**, ce qui est la forme la plus coûteuse.

1. **Un seuil de dose létale hors de `]0 ; 100[`** rendait une ligne de `NaN`.
   Une dose létale à 0 % ou à 100 % n'existe pas — l'inverse normale y vaut
   l'infini — et au-delà de 100 % c'est une faute de frappe. Les seuils écartés
   sont maintenant **nommés** sous le tableau.
2. **Une liste de seuils vide levait** « invalid argument to unary operator » :
   le champ qu'on efface pour le retaper faisait tomber tout le tableau.
   L'interface filtrait avant d'appeler — mais elle n'est pas le seul appelant,
   et c'est justement ce filtre qui empêchait de nommer ce qui était écarté. Le
   filtre est descendu dans la fonction, où il appartient.
3. **Un essai de référence hors bornes retombait sur le premier.** Demander le
   rapport de puissance par rapport à l'essai 99 sur un jeu qui en compte deux
   rendait le tableau du premier, sa colonne « Référence » cochée sur lui.
   C'est le défaut le plus difficile à voir : rien n'est vide, rien ne lève.
   Un test **exigeait** ce repli — je l'avais écrit ainsi, et c'était un mauvais
   choix ; il exige désormais le refus.

Plus une gêne : une **palette inconnue** de RColorBrewer faisait avertir ggplot
à chaque tracé et rendait un graphique gris. L'interface n'offre que des noms
valides, mais la fonction est publique et un avertissement par tracé s'accumule
dans la console d'un serveur partagé.

### Le mode guidé, et les limites héritées

La **fiche de l'essai** est repliée par défaut : vingt champs d'identification
s'ouvraient au-dessus du tableau de doses alors qu'aucun ne change un calcul, et
poussaient hors de l'écran la seule chose qu'il faut vraiment saisir. Un encart
dit désormais le minimum — doses, effectifs, morts, témoin, et **trois doses au
moins** de mortalité corrigée intermédiaire — avant la saisie plutôt qu'en refus
après coup.

Les deux limites (**6 essais**, **100 doses**) sont celles de WIN DL, reprises
pour rester comparable. Elles étaient appliquées en silence : on les rencontrait
sous forme de refus, sans savoir d'où elles venaient ni si elles tenaient à
HStat. Elles sont maintenant nommées et attribuées.

### Le compte d'itérations ne se compare pas

Les deux algorithmes atteignent le même optimum — a, b et c coïncident à six
chiffres — mais pas au même rythme : sur l'essai de référence, HStat converge en
**3 boucles EM** (6 itérations de Newton-Raphson cumulées) là où le logiciel en
annonce **75**.

La cause est mesurée : la mortalité naturelle y vaut exactement zéro, et HStat
part de la valeur du témoin — ici 0/25, donc zéro. L'étape [E] rend alors des
poids nuls et `c` ne bouge plus. WIN DL, lui, rampe vers cette borne.
Reproduire son compte demanderait de **ralentir délibérément** l'algorithme pour
une valeur d'affichage. Le libellé dit donc de qui est le compte, plutôt que de
laisser croire à un désaccord.

### Les quatre tests de comparaison : confrontés au manuel, pas à des chiffres

Le manuel donne **H0 et H1 de chacun des quatre tests, sous chacun des trois
scénarios** — douze couples de modèles, écrits en toutes lettres. HStat les
reproduit exactement, et un test le vérifie en reconstruisant les douze couples
à la main puis en exigeant que le Chi-2 rendu vaille `2·(ll(H1) − ll(H0))` sur
ces couples-là.

C'est la seule confrontation possible : **aucun fichier de sortie livré avec le
logiciel n'exerce ces tests.** Elle porte donc sur la structure — quels modèles
sont opposés — et non sur des chiffres publiés. Une inversion de couple serait
invisible autrement : le test rendrait un Chi-2 parfaitement plausible, et faux.

Le point le plus délicat est le **troisième test**, dont l'hypothèse nulle change
avec le scénario : « c commun » contre « c libres » sous les deux premiers,
« c = 0 » contre « c libres » sous le troisième. Prendre le modèle de base dans
les trois cas comparerait un modèle à lui-même sous l'hétérogène — zéro degré de
liberté, un test qui ne teste rien.

Deux règles du manuel étaient déjà en place et se trouvent confirmées : le
scénario 3 n'est **pas effectué** si un essai a une mortalité observée dans son
témoin, et les quatre tests sont **indépendants** — deux non-significatifs pris
séparément ne concluent pas sur leur conjonction.

### Le Chi-2 est la déviance, et les fichiers d'exemple disent le contraire

Le choix se tranche sur le seul fichier de sortie du logiciel Windows, qui
imprime « Chi2 calculé : 0.672 ». La **déviance** vaut 0,67217 et s'arrondit à
0,672 ; le Chi-2 de **Pearson** vaut 0,66768 et s'arrondirait à 0,668. Le `.PRN`
décide.

Mais les six essais livrés stockent, eux, un **Pearson** — et c'est la même
explication que pour leurs bornes sans Fieller : ces valeurs viennent du moteur
MS-DOS. La différence crève les yeux sur l'essai dont une dose tue **tout** :

| CL94AEN | déviance | Pearson | stocké |
|---|---|---|---|
| Chi-2 | 1,579 | **1,212** | 1,20967 |

Aligner HStat sur ces fichiers-là le désalignerait du logiciel Windows, qui est
celui auquel on se compare. Un test épingle les deux valeurs pour que le choix
ne se reprenne pas par inadvertance.

### La limite de cent doses vaut aussi pour le total

Le manuel l'écrit à part de la limite par essai : « dans le module de
comparaison, le regroupement de doses ne peut pas excéder 100 au total ». Six
essais de quatre-vingts doses passaient donc un à un et dépassaient ensemble
sans un mot — la comparaison partait, et elle n'aurait eu aucun équivalent dans
le logiciel.

### Ce que le manuel confirme sans qu'il y ait rien à changer

- La **fusion** exige neuf champs strictement identiques (espèce, stade, durée,
  température, matières actives 1 et 2, ratio, méthode, unité), bloque sur un
  test significatif ou une estimation impossible, et **retombe sur `c = 0`**
  quand l'estimation échoue et que la mortalité naturelle globale est nulle.
  Les quatre règles étaient déjà en place.
- Le **degré de liberté** vaut le nombre de doses moins deux, le facteur
  d'hétérogénéité est `Chi2/ddl`, et le quantile devient celui de Student.
- La méthode par défaut à l'ouverture d'un fichier est **EM**.
- Le refus sous trois doses de mortalité corrigée intermédiaire est bien celui
  du logiciel.

### « Mortalité nulle » n'est pas une méthode de WIN DL

Le logiciel n'en propose que **deux** pour un essai isolé : Newton-Raphson avec
Abbott, et EM. La mortalité naturelle fixée à zéro n'y existe que comme l'un des
trois **scénarios de comparaison** entre essais. C'est donc un ajout de HStat,
utile — il donne l'ajustement probit nu, celui que `glm(binomial(probit))`
produit — mais il n'y a rien à comparer côté WIN DL. Le dire évite qu'on cherche
un désaccord là où il n'y a pas de vis-à-vis.

### Deux conventions du logiciel, qu'il a fallu retrouver

Aucune des deux ne se devine, et chacune se voit sur les nombres publiés.

1. **La log-vraisemblance est écrite sans les coefficients binomiaux.** WIN DL
   rend −105,636 là où R rend −12,559 : l'écart vaut exactement
   `sum(log(choose(n, x)))`. Il ne dépend pas des paramètres, donc tout
   *rapport* de vraisemblance est identique — mais la valeur affichée, elle,
   ne l'est pas, et c'est elle que les rapports de bioessai recopient.
2. **La matrice d'information de Fisher est assemblée sur les doses seules.**
   Le lot témoin apporte pourtant de l'information sur `c` — mais sa
   contribution vaut `n0/(c(1−c))`, donc **infinie dès que c vaut 0**, ce qui
   rendrait un écart-type nul sur la mortalité naturelle. WIN DL affiche
   ET(c) = 0,4387 sur un essai où `c` vaut exactement zéro : le témoin n'y est
   pas. C'est aussi le choix prudent — l'incertitude sur `c` y est plus grande,
   jamais plus petite.

Même remarque pour le **degré de liberté** du test d'ajustement : le manuel
écrit `nombre de doses − 2`, et le fichier de référence le confirme (7 doses,
ddl = 5) **y compris quand `c` est estimée**, où la théorie voudrait retirer un
paramètre de plus. C'est ce ddl qui fixe le seuil au-delà duquel le facteur
d'hétérogénéité s'applique : le changer déplacerait ce seuil.

### Ce qui est estimé et ce qui est déclaré ne se comptent pas pareil

Deux défauts silencieux, de la même famille, et tous deux découverts en
comparant HStat à `glm()` plutôt qu'en relisant le code.

**La matrice à inverser est celle des paramètres réellement estimés.** Sous
« Abbott » et « mortalité nulle », `c` est **déclarée** — lue sur le témoin, ou
posée à zéro — et pourtant la matrice d'information à trois lignes était
inversée comme si elle avait été estimée. Le bloc (a, b) payait alors une
incertitude sur `c` que l'hypothèse exclut :

| Essai de référence, `c` fixée à zéro | var(a) | var(b) | ET(DL50) |
|---|---|---|---|
| inversion 3×3 (faux) | 0,5893 | 0,3554 | **0,6716** |
| bloc 2×2 (juste) | 0,1841 | 0,0329 | **0,1032** |

Soit une erreur-type **6,5 fois trop grande**, des intervalles absurdement
larges, et un `g = t²·var(b)/b²` gonflé au point de franchir 1 — ce qui faisait
abandonner Fieller pour la delta-méthode sans raison.

L'incohérence était **interne** : `npar` vaut déjà 2 pour ces deux méthodes,
c'est lui qui décide si le Chi-2 garde un degré de liberté résiduel. Le même
ajustement comptait donc deux paramètres pour le test d'ajustement et trois
pour les variances.

La vérification qui tranche : à `c = 0`, le modèle de Finney **est** un GLM
binomial à lien probit sur le log10 de la dose. `glm()` et `MASS::dose.p()`
sont la référence universelle, et les deux coïncident désormais à 1e-7 — pas
« approchent ». `c` n'étant pas estimée, son écart-type et ses covariances
valent `NA` : zéro dirait « connue exactement », ce qui n'est pas la question
posée.

**Le témoin informe `c`, il n'est pas une dose.** Quand `c` est estimée (EM), le
témoin entre dans la vraisemblance — c'est ce chemin que WIN DL reproduit au
chiffre près. Quand `c` est déclarée, il n'informe plus rien : le modèle est
fixé avant qu'on le regarde, et l'ajustement se juge sur la série de doses
seule. C'est déjà la convention du degré de liberté (doses − 2) et celle de la
matrice d'information ; l'y ranger rend les trois cohérentes.

Ce n'est pas une question de doctrine. Sous « mortalité nulle » avec un témoin
qui compte des morts, le modèle affirme `p = 0` là où l'on a observé des décès :
la vraisemblance vaut moins l'infini. `hstat_dl50_logvrais()` borne la
probabilité à 1e-12 pour ne pas rendre l'infini, et le Chi-2 ressortait **fini
mais entièrement déterminé par cette borne** :

| epsilon | 1e-10 | 1e-12 | 1e-15 | 1e-20 |
|---|---|---|---|---|
| Chi-2 | 119,8 | 147,4 | 188,9 | 258,0 |

Un nombre qui change avec une constante d'implémentation n'est pas une
statistique de test. Il pilotait pourtant le facteur d'hétérogénéité, donc la
largeur de **tous** les intervalles publiés. Sur l'essai d'exemple, il valait
148 et multipliait les intervalles par 5,4.

La contradiction, elle, se dit maintenant par son nom : déclarer une mortalité
naturelle nulle devant un témoin qui compte des morts est un défaut de saisie,
pas un résultat.

**L'essai de référence de WIN DL n'est touché par aucun des deux** : il est
ajusté par EM, où `c` est estimée, et son témoin est 25/0 — le terme valait déjà
zéro des deux côtés. Les vingt valeurs vérifiées ne bougent pas d'un chiffre.

#### Deux vraisemblances ne se soustraient que sur les mêmes données

Onde de choc du point précédent, et attrapée avant de partir. Le test
Abbott/EM calculait `2·(em$ll0 − ab$ll0)` : depuis que le témoin sort de la
vraisemblance quand `c` est déclarée, celui d'EM le contient et celui d'Abbott
non. La différence chargeait le terme du témoin, ressortait **négative**, et le
`max(0, ·)` la ramenait à zéro — p = 1, « les deux estimations concordent »,
**quelles que soient les données**.

Or le témoin est précisément la donnée qui sépare les deux modèles : c'est lui
qui dit où est la mortalité naturelle. Il entre donc des deux côtés, Abbott
étant EM contraint à `c = x0/n0`. La différence est alors positive par
construction — EM maximise exactement cet objectif. Mesuré : 0,002 (p = 0,96)
sur un essai concordant, 28,3 (p = 1e-7) sur un essai où le témoin contredit
les doses.

### Fieller cède la place à la delta-méthode, et cela se voit

L'intervalle d'une dose létale est un rapport, `(F⁻¹(p) − a) / b`. Le théorème
de Fieller le donne exactement tant que

```
g = t² · var(b) / b²  <  1
```

Au-delà, l'ensemble des valeurs compatibles n'est plus borné. **Ce n'est pas un
détail d'implémentation** : l'essai de référence est à g = 1,44, et WIN DL y
publie bien des bornes *symétriques* en log-dose, celles de la delta-méthode
(Morgan, 1992).

Piège corrigé, à ne pas réintroduire : une écriture approchée du terme sous la
racine — `v(1−g) + g(...)` au lieu de `b²v − g(V_aa − V_ab²/V_bb)` — rendait
des bornes qui **n'encadraient même pas l'estimation** (DL50 = 0,0176 pour un
intervalle [0,00055 ; 0,0071]). Le test compare désormais le résultat aux
**racines du polynôme** qui définit Fieller, à 1e-8 ; et il vérifie la limite
qui trahit la faute : quand `g` tend vers 0, la demi-largeur doit tendre vers
`t · ET(m)`, celle de la delta-méthode.

### `doses_letales()` trie ses lignes — n'y recollez rien dans l'ordre de saisie

La fonction rend ses seuils par ordre **décroissant**. `hstat_dl50_dose_pour()`
y recollait la mortalité demandée dans l'ordre de saisie : toutes les colonnes
se décalaient, et l'on lisait la dose de la DL25 sur la ligne de la DL95 — un
tableau parfaitement plausible, entièrement faux. Le calcul se fait donc **une
ligne à la fois**.

### La table de saisie passe par un proxy DT

Relire `saisie()` dans `renderDT` reconstruit la table à **chaque cellule
modifiée** : celle qu'on est en train d'éditer est détruite sous le curseur, et
deux saisies rapprochées se perdent l'une l'autre. La table n'est donc
construite qu'une fois (`isolate()`), puis mise à jour par
`DT::replaceData()` sur son proxy. C'est l'idiome prévu par DT, et le seul qui
rende la saisie utilisable.

### La dose zéro est le témoin, elle n'est pas écartée

Son logarithme n'existe pas : elle ne peut pas entrer dans la régression. Mais
l'écarter en silence perdrait la **mortalité naturelle** de l'essai — celle qui
change toute la courbe de réponse. `hstat_dl50_depuis_donnees()` l'extrait et
en fait le témoin.

### Le rapport de puissance n'existe qu'à pente commune

C'est le chiffre que publie la surveillance des résistances : combien de fois
faut-il plus de produit pour tuer la souche étudiée que la souche de référence.

```
R = DL50(essai) / DL50(référence) = 10^((a_réf − a_essai) / b)
```

**Et c'est tout le sujet : ce rapport n'a de sens que si les droites sont
parallèles.** Sinon il change avec le niveau de mortalité — il vaut 3 à la DL50
et 12 à la DL90 — et publier « R = 3 » revient à choisir un chiffre parmi
d'autres sans le dire. Le test de parallélisme est donc calculé **avant** et
rendu **avec** : pas en option, pas plus bas dans la page. Quand il est rejeté,
`hstat_dl50_puissance()` renvoie l'avertissement qui dit de comparer les doses
létales seuil par seuil plutôt que par un rapport unique.

Le numérateur et le dénominateur viennent du **même** ajustement à pente
commune, d'où leur covariance — qu'un calcul essai par essai ignorerait. C'est
encore un rapport de deux estimateurs normaux : l'intervalle passe par Fieller,
avec le même repli sur la delta-méthode quand `g ≥ 1`.

La matrice de variances vient du **hessien** rendu par `optim(hessian = TRUE)` :
`nll` étant l'opposé de la log-vraisemblance, son hessien est directement
l'information observée. La demander coûte une évaluation de plus et évite de
réécrire l'information de Fisher pour le cas multi-essais.

Deux vérifications que le test porte, parce qu'elles attrapent des fautes
plausibles :

1. **Sur deux droites parallèles simulées de rapport connu 5**, l'estimation
   rend 5,09 avec un intervalle qui contient 5, et la pente commune retrouve la
   pente simulée.
2. **Inverser la référence inverse le rapport.** C'est la vérification qui
   attrape un signe pris à l'envers dans `(a_réf − a_i) / b` — une erreur qui
   rendrait un ratio de résistance parfaitement plausible, et faux.

### Les quatre silences du bioessai

Un audit du module a trouvé quatre situations où il rendait un chiffre faux ou
trompeur **sans un mot**. Chacune est maintenant refusée ou nommée ; aucune ne
se devine à la lecture des résultats.

1. **Une pente négative n'est pas un ajustement, c'est une saisie à l'envers.**
   Deux colonnes inversées, et le module ajustait, convergeait et rendait un
   rapport complet — équation, intervalles, graphique — où la DL10 valait mille
   fois la DL90. C'est le résultat faux le plus facile à publier de bonne foi.
   Le refus porte sur la valeur **initiale** (pour ne pas itérer en vain) et
   sur la valeur **finale** (si l'algorithme y arrive en route).

2. **Une dose létale extrapolée n'est pas une dose létale mesurée.** La droite
   de Henry se prolonge à l'infini, la population testée non. Sur l'essai de
   référence lui-même, *deux des trois* doses létales publiées tombent hors de
   l'étendue testée — la DL90 vaut près de quatre fois la dose la plus forte
   appliquée. La colonne `Position` les nomme, et l'avertissement porte le
   `DL` de chaque seuil : la liste brute « DL90, 10 » se lisait comme une dose
   de 10.

3. **Un Chi-2 sans degré de liberté résiduel ne teste rien.** Avec autant de
   doses que de paramètres estimés, le modèle passe exactement par les points
   et le Chi-2 vaut zéro par construction. Le plancher `max(1L, …)` — qui
   existe pour éviter une division par zéro — le transformait en « p = 1,0000,
   ajustement probit légitime ». `informatif` distingue les deux, et le facteur
   d'hétérogénéité ne s'applique plus à un test qui n'a pas eu lieu.

4. **Un modèle contraint ne peut pas dépasser le modèle libre.** `optim()`
   rendait `ok = TRUE` sans qu'on lise son code de convergence. Si le modèle
   emboîté ressortait au-dessus, le Chi-2 négatif était écrasé à zéro par
   `max(0, …)` et le test concluait « non significatif » : **l'échec se
   présentait comme une absence de différence**, la conclusion inverse.

Cinquième point, moins grave et de même nature : au-delà de
`HSTAT_DL50_TEMOIN_MAX` (20 %) de mortalité dans le témoin, la correction
d'Abbott devient peu fiable. C'est un défaut de conduite d'essai, pas de
saisie — on le dit, on ne bloque pas.

### On ouvre un module de DL50 pour lire une DL50

Le bloc de résumé annonçait l'équation, la mortalité naturelle, le test
d'ajustement et le nombre d'itérations. **Pas la dose létale.** Elle vivait au
bas d'un tableau de dix colonnes, et **sans son unité** — pourtant demandée
dans la fiche d'essai, donc connue. Une dose sans unité n'est pas un résultat.

`hstat_dl50_verdict()` rend la phrase qu'on vient chercher, et
`hstat_dl50_unite()` est la porte unique de l'unité : elle suit la dose dans
les tableaux, sur l'axe du graphique et dans les exports. Le titre d'axe reste
réglable — mais son **défaut** porte l'unité, plutôt que d'obliger à la retaper
là où elle figure déjà.

### La virgule a un seul rôle, et c'est le collage qui le décide

`hstat_dl50_coller()` accepte trois colonnes venues d'un tableur — les
comptages y existent déjà, les retaper cellule par cellule est la friction la
plus quotidienne du module.

Le piège est le même que dans le module de nettoyage, mais il se tranche
autrement : un tableur français copie `0,00063` séparé par des **tabulations**
(la virgule est une décimale) ; un CSV anglais copie `0.00063,25,5` (la virgule
sépare). On ne peut pas lui donner les deux rôles, alors **le texte décide** :
s'il existe un séparateur non ambigu — tabulation ou point-virgule — la virgule
est une décimale ; sinon seulement, elle sépare. Le rôle retenu est annoncé
après le collage.

Une ligne d'en-tête se reconnaît à ce qu'elle **ne porte aucun nombre**. La
jeter en silence vaut mieux que de rendre une première dose absurde ; demander
« votre collage a-t-il un en-tête ? » serait poser une question dont le texte
porte déjà la réponse.

### Erreur-type et écart-type ne mesurent pas la même chose

Les confondre est l'erreur classique du bioessai, et elle change la conclusion :

- l'**erreur-type** mesure la **précision de l'estimation**. Elle diminue quand
  on teste plus d'individus ; c'est elle qui fonde les intervalles de
  confiance, et c'est elle que WIN DL imprime sous le nom « Écart-type ».
- l'**écart-type** mesure la **dispersion des sensibilités** dans la
  population. Dans le modèle probit, les log-tolérances suivent une loi normale
  de moyenne `−a/b` et d'écart-type `1/b` (Finney, 1971) : il vaut donc `1/b`,
  il est **le même pour toutes les doses létales**, et il ne diminue **pas**
  quand on teste plus d'individus. Une population hétérogène garde un grand
  écart-type même mesurée parfaitement.

La vérification qui les sépare, et que le test porte : `10^(log DL50 ± 1/b)`
rend exactement la **DL84** et la **DL16**. L'écart-type décrit la courbe, pas
l'essai.

Piège posé par intuition, et faux : l'erreur-type **n'est pas minimale à la
DL50**. `var(m)` est un polynôme du second degré en `m`, minimal en
`m* = −Vab/Vbb`, qui ne coïncide avec la DL50 que si la covariance de `a` et
`b` l'y met. Sur l'essai de référence, la DL90 est estimée plus précisément que
la DL50.

### Deux graphiques, parce qu'ils ne montrent pas la même chose

La **droite de Henry** montre le *modèle* : le probit de la mortalité corrigée
est linéaire en log-dose, et c'est ce qui se vérifie à l'œil. La **courbe
dose-réponse** montre la *réponse mesurée* — une sigmoïde qui part de la
mortalité naturelle et monte vers 100 %. C'est celle qu'un rapport d'essai
publie, parce qu'elle se lit sans savoir ce qu'est un probit.

Trois pièges, tous silencieux, tous testés.

**Le point tracé n'est pas le même.** La droite porte le probit de la mortalité
**corrigée** ; la courbe porte la mortalité **observée**, parce que la courbe
ajustée inclut déjà `c`. Y poser les points corrigés les descendrait tous de la
valeur de `c`, et l'écart passerait pour un défaut d'ajustement.

**Une mortalité corrigée de 0 % ou de 100 % n'a pas de probit.**
`hstat_dl50_probit()` ramène la proportion dans `[1e-12 ; 1 − 1e-12]` pour ne
pas rendre l'infini : le point ressort à **±7,03**, une valeur qui ne mesure
rien — elle dépend de l'epsilon. Et comme l'étendue de l'axe se calcule sur les
points, **deux artefacts suffisent à étirer l'axe de −7,7 à +7,7** : mesuré sur
un essai à six doses dont une à 0 % et une à 100 %, les quatre points réels
occupaient **17 %** de la hauteur, la droite et sa bande écrasées au milieu.

Ces points sont donc écartés de la droite de Henry — et **nommés**, un point
qui disparaît sans un mot étant le défaut que ce dépôt traque ailleurs. La
courbe dose-réponse les porte tous : 0 % et 100 % sont des observations comme
les autres, et ce sont les doses qui **bornent** l'essai.

**La DL50 n'est pas à 50 % de mortalité observée.** Elle est définie sur la
mortalité corrigée : la dose à laquelle le produit tue la moitié des individus
que le témoin aurait laissés vivants. Sur une courbe de mortalités observées, le
repère passe donc à `c + (1 − c)·s`, pas à `s`. Avec un témoin nul les deux
coïncident — ce qui rend l'erreur invisible précisément sur les essais les plus
propres. Mesuré sur un témoin à 10 % : le repère DL50 est à **52,8 %**.

Deux conséquences de construction. L'intervalle se bâtit **sur le probit puis se
transporte** par F, qui est monotone : les bornes restent dans `[0 ; 100]` et
l'asymétrie de la sigmoïde est respectée — le bâtir sur le pourcentage le ferait
sortir du cadre aux extrêmes, là où l'on veut justement lire. Et le **second axe
en probit est masqué** sur la courbe dose-réponse : il placerait l'infini à 0 %
et à 100 %, c'est-à-dire aux deux graduations que cette courbe existe pour
montrer. Masqué côté interface, pas ignoré en silence.

Plusieurs essais de mortalités naturelles différentes n'ont pas de repère
commun : un seul trait serait faux pour tous sauf un. On s'abstient plutôt que
d'en tracer un au hasard.

### Un tableau de résultats se recopie, un paragraphe non

Les paramètres statistiques vivent dans **un tableau**, pas seulement dans le
résumé en prose : un chiffre lu dans un paragraphe ne se recopie pas dans un
rapport et ne s'exporte pas. Le résumé reste au-dessus pour la lecture rapide ;
le tableau porte les vingt valeurs, et c'est lui qui part en CSV et en Excel.

Détail de mise en forme qui a son importance : le tableau mêle des grandeurs
(2,19759), des probabilités (0,05) et des **entiers** (5 degrés de liberté).
Un format à décimales fixes écrivait « 5.00000 » degrés de liberté — on doute
d'un chiffre affiché comme s'il avait cinq décimales. La mise en forme est donc
faite à l'affichage seulement ; le tableau exporté reste numérique.

### Les listes existent pour que la sélection fonctionne

Ce sont deux pièces de WIN DL qui n'ont l'air de rien et qui se tiennent l'une
l'autre. Le manuel le dit sans détour : « il est conseillé pour les ajouts dans
les listes de ne pas utiliser des orthographes différentes pour décrire un même
élément **car la sélection de fichiers serait inefficace** ». Deux essais notés
« Cyfluthrine » et « cyfluthrine » ne se retrouvent jamais ensemble ; le
vocabulaire contrôlé est ce qui l'évite.

D'où trois décisions :

1. **Le doublon est refusé casse comprise**, et il est *annoncé*. Le taire
   ferait croire à un ajout.
2. **Un critère vide ne filtre pas.** C'est la différence entre « je ne demande
   rien sur l'espèce » et « je demande une espèce qui n'existe pas » : traiter
   le premier comme le second ne rendrait jamais aucun essai, et l'utilisateur
   conclurait que son fonds est vide. `selection()` distingue donc `NULL`
   (aucune sélection active) de `character(0)` (sélection vide) — et l'écran
   les distingue aussi.
3. **La seconde matière active et le ratio ne servent que si l'on trie sur les
   deux**, la case « (MA1) ou (MA1 et MA2) » du logiciel. Sans elle, un essai à
   une seule matière active serait écarté par un critère qui ne le concerne
   pas.

La température suit le manuel : une valeur → égalité, deux → intervalle. Les
bornes sont **remises dans l'ordre** plutôt que de rendre zéro essai sur une
inversion de saisie, qui n'apprendrait rien à personne.

### Les fichiers natifs se lisent en octets, pas en lignes

Les séparateurs de champ de la première ligne d'un fichier WIN DL sont les
octets **0x00 à 0x09**. `readLines()` s'arrête sur le premier zéro
(« nul character not allowed ») et perdrait l'en-tête entier. Le découpage en
lignes se fait donc aussi sur les octets : `rawToChar()` sur l'ensemble
rendrait une chaîne invalide dans la locale courante — les accents du CP437 ne
sont pas de l'UTF-8 — et `strsplit()` refuserait de travailler dessus.

L'écriture est symétrique, et pour la même raison : le zéro ne peut pas exister
dans une chaîne de R, l'en-tête se monte donc en `raw`.

## Variables à valeurs nulles : trois frontières, trois pièges

`hstat_vars_zero()` liste les colonnes dont **toutes les valeurs observées**
valent zéro — variance nulle, donc ni corrélation ni test. Le module de
nettoyage (étape 7) en propose quatre gestes : déclarer les zéros manquants,
les remplacer par une valeur, ressaisir les valeurs, supprimer les variables.

Trois cas limites, chacun constaté à l'écran et chacun fautif **dans le sens
silencieux** — la colonne disparaissait du diagnostic au lieu d'y figurer :

1. **Une colonne vide n'est pas une colonne de zéros.** Sans observation, il
   n'y a rien à comparer à zéro. Elle ressort dans `attr(res, "vides")`, que
   l'interface nomme à part. Et elle se présente sous **trois** formes : typée
   `logical` par les lecteurs de CSV, numérique tout-`NA`, ou remplie de
   chaînes vides (Excel, exports SPSS). Tester `is.na()` seul en laissait
   passer une, et placer l'exclusion des booléens avant en supprimait une
   autre.
2. **Les manquants ne comptent pas comme des zéros.** Une colonne `0/0/NA/0`
   est nulle sur ce qu'elle montre ; la colonne `Manquants` le dit à côté,
   plutôt que de mélanger « mesuré à zéro » et « pas de mesure ».
3. **Un booléen renseigné est écarté.** `FALSE` vaut bien 0 en arithmétique,
   mais une colonne de « non » est une réponse, pas une mesure : la ranger ici
   ferait proposer d'en « corriger les valeurs ».

### La virgule est une décimale, pas un séparateur

`hstat_zero_valeurs_parse()` ne peut pas lui donner les deux rôles : dans une
application française, « 2,5 » est la façon normale d'écrire deux et demi, et
la traiter en séparateur en faisait **deux** valeurs — donc un décompte faux,
donc un refus incompréhensible. Les séparateurs sont le retour à la ligne et
le point-virgule, exactement la convention du CSV français, qui existe pour
cette raison.

Le décompte est **vérifié** : une liste plus courte ou plus longue que la
colonne décalerait silencieusement toutes les observations. Refuser la saisie
vaut mieux.

### Ne pas écrire dans `transformationLog`

C'est un registre **typé** : ses entrées sont relues champ par champ
(`method`, `lambda`) pour inverser les transformations, et `entry$label` sur
une chaîne lève « $ operator is invalid for atomic vectors ». Un geste de
nettoyage se dépose donc par `hstat_ai_capture()`, pas là. Un test le garde.

## Réinitialisation : exhaustive par construction, jamais par énumération

`hstat_valeurs_initiales()` (`Utils.R`) est la **source unique** de l'état de
session : `reactiveValues` est construit depuis elle, et la réinitialisation la
reparcourt. Un champ ajouté là est donc créé au démarrage **et** effacé à la
remise à zéro, sans qu'on ait à y penser deux fois.

Avant, les deux listes étaient distinctes et avaient dérivé : `aiContext`,
`aiHistory`, `cahClusters`, `y2Vars`… survivaient à la réinitialisation.
La remise à zéro vide en plus tout champ créé en cours de session par un module
(`setdiff` sur `reactiveValuesToList`).

**`shinyjs::reset("file")` ne vide pas `input$file`.** Il remet le widget à
blanc, mais la valeur reste : la feuille Excel choisie et le bloc de combinaison
de feuilles survivaient donc à la réinitialisation. D'où `fichierNeutralise` et
le réactif `fichier_actif()` — **seule porte d'accès au fichier**, un test barre
tout accès direct à `input$file`.

**Aucune fonction essentielle ne doit dépendre d'un paquet optionnel.**
`shinyalert` est facultatif ; sans repli, `shinyalert()` levait une erreur avalée
par l'observateur et le bouton « Réinitialiser » ne faisait **rien, en silence**.
Le repli passe par `modalDialog`, qui fait partie de Shiny.

### Charger de nouvelles données EST une réinitialisation

Le bouton « Réinitialiser » n'est pas le seul geste qui rend l'état de session
caduc : **choisir un autre fichier le fait aussi**, et ne le faisait pas.
Constaté à l'écran — après un second chargement, les anciennes variables
restaient proposées et les résultats du premier fichier restaient affichés.

Quarante-huit champs créés par les modules portent des noms de colonnes ou des
niveaux de facteur (`yVarNames`, `selected_y_vars`, `x_label_levels`,
`legendLabels`, `customXLevels`…) ; aucun n'était effacé. On lisait donc des
tableaux, des graphiques et des recommandations portant sur des colonnes qui
n'existent plus — **pire qu'un écran vide, parce que ça ressemble à un
résultat**.

`hstat_reinitialiser_valeurs()` (`Utils.R`) porte donc la remise à zéro en un
seul exemplaire, et les **quatre** gestes l'appellent : le bouton, le chargement
d'un fichier, la combinaison de feuilles d'un classeur, la fusion de plusieurs
fichiers. Ce qui leur reste en propre est ce qui les distingue vraiment — le
bouton neutralise le fichier et revient à l'onglet de chargement ; un chargement
affecte ensuite les nouvelles données, **après** la remise à zéro, sinon elle
les effacerait.

**Le compteur `resetSignal` est monotone.** Les modules l'observent pour vider
leurs propres contrôles, et `observeEvent` ne réagit qu'à un *changement* de
valeur. Le remettre à son initiale (0) puis l'incrémenter rendrait toujours 1 :
le deuxième chargement ne signalerait plus rien. Il est donc lu avant la remise
à zéro et réécrit après.

**`reactiveValuesToList` sous `isolate()`.** Lire tous les champs prend une
dépendance sur chacun ; appelée depuis un observateur ordinaire, la fonction se
redéclencherait sur ses propres écritures, indéfiniment. Les appels actuels
passent par `observeEvent`, dont le corps est déjà isolé — l'`isolate()` protège
le prochain point d'appel, pas ceux d'aujourd'hui.

Deux remplacements du jeu de travail **n'y passent pas**, délibérément :
`mod_clean` (typage, découpage en classes) modifie les colonnes du fichier
courant, et « Utiliser comme jeu de données » (`mod_threshold`) dérive son
tableau de l'analyse en cours — purger effacerait l'analyse qui vient de le
produire. Un test vérifie que les trois portes d'*import*, elles, appellent bien
la remise à zéro : un quatrième chemin ajouté demain sans elle réintroduirait le
défaut en silence.

## Bilingue : traduire l'AFFICHAGE, pas les 9 700 appels

L'application compte ~9 700 chaînes destinées à l'utilisateur sur 24 fichiers.
Les envelopper une à une dans un appel de traduction toucherait chaque ligne de
code. La traduction est donc appliquée **au texte affiché**, dans le navigateur
(`www/hstat-i18n.js`), à partir d'un dictionnaire incorporé dans la page.
L'interface de `UX.R`, celle des modules, les notifications et les tableaux
rendus passent par le même filtre sans qu'aucun appel soit modifié.

**La clé est la chaîne française elle-même.** Conséquence voulue : une chaîne
absente du dictionnaire reste en français au lieu d'afficher un identifiant
technique. Une traduction incomplète dégrade doucement.

Quatre points à ne pas défaire :

1. **Le dictionnaire est incorporé** (`window.HSTAT_I18N`), jamais chargé par
   requête : le bilingue doit fonctionner hors ligne. Aucune API de traduction —
   elle exigerait une connexion, coûterait à l'usage, et traduirait mal le
   vocabulaire statistique.
2. **Un `MutationObserver` rattrape le contenu rendu après la bascule.** Sans
   lui, seule l'interface initiale serait traduite : notifications, tableaux et
   sorties Shiny resteraient en français.
3. **Le retour au français restitue le texte d'origine**, conservé sur le nœud
   (`__hstatFr`). Une traduction inverse par dictionnaire perdrait les accents
   et confondrait deux termes traduits pareil.
4. **Seules les correspondances exactes et complètes sont remplacées**, et un
   élément portant `data-hstat-notranslate` est ignoré : les données de
   l'utilisateur ne doivent jamais être traduites par morceaux.

#### Ce qui est traduisible, et ce qui ne l'est pas

La couverture se mesure sur les **libellés atteignables** : ceux qui existent
dans le DOM comme **texte entier d'un nœud** — libellé de widget, titre
d'onglet, titre de boîte, bouton, notification. Ce sont les seuls que le
traducteur du navigateur peut remplacer.

**904 sur 904, soit 100 %** (contre 580 auparavant).

Le dépôt compte par ailleurs ~3 300 autres chaînes françaises. Les traduire une
à une serait du travail perdu : ce sont des **fragments assemblés à
l'exécution**, qui n'apparaissent jamais comme chaîne complète dans la page. Ils
relèvent de `tr()` et `trf()`, côté R. Mesurer la couverture sur eux donnerait
un chiffre juste et sans signification.

Le plafond de poids vit désormais dans `HSTAT_I18N_KO_MAX` : il était écrit en
dur dans **deux** tests, même valeur recopiée — deux chiffres qui ne se parlent
pas finissent par diverger. Il est passé de 60 à 120 Ko, la couverture complète
portant le dictionnaire de ~53 à 79 Ko (26 Ko compressés, ce qui transite
réellement).

#### Le dictionnaire élague ses clés — la recherche doit le faire aussi

`hstat_i18n_load()` applique `trimws()` à ses clés, et c'est juste : une entrée
de CSV ne doit pas dépendre d'un blanc invisible. Mais `tr()` cherchait la
chaîne **telle quelle**. Conséquence non voulue : **toute chaîne bordée
d'espaces était intraduisible**, sans un mot.

Trente-huit gabarits étaient dans ce cas. Ce sont des fragments de phrase
assemblés par `paste0` — l'espace y sépare deux morceaux, il relève de la mise
en forme et jamais du texte. Ils restaient en français au milieu d'une interface
anglaise, **et le dictionnaire les contenait pourtant**.

`tr()` cherche donc sur la chaîne élaguée et rend l'espacement d'origine autour
de la traduction. L'espacement est de la présentation : on le retire pour
chercher, on le remet pour rendre. Un test vérifie qu'il ressort **à
l'identique**, jamais normalisé, et qu'une chaîne inconnue ressort intacte.

Détail d'implémentation : l'espacement **final** se prend par mesure
(`nchar(sub("[[:space:]]+$", "", x))`), pas par expression régulière — un `sub`
gourmand rendrait la chaîne entière.

Corollaire sur la **mesure** : compter la couverture en comparant les chaînes
brutes aux clés élaguées annonçait 38 manquantes que l'application traduit. Une
métrique doit modéliser le mécanisme qu'elle mesure, sinon elle décrit un autre
programme. Couverture réelle des chaînes passées à `tr()`/`trf()` : **263 sur
263**.

#### Corriger le français avant de le traduire

Une faute traduite se fige : elle devient une clé du dictionnaire, et la
corriger ensuite casse la correspondance. Les 55 corrections de français —
accents manquants dans les modules récents (« Memo enregistre », « etiquettes
deja posees »), et typographie (« Erreur: » pour « Erreur : », « succès! » pour
« succès ! ») — ont donc précédé la traduction.

Prudence nécessaire : le balayage des mots sans accent remonte aussi des
**identifiants** (`acp_resultats`, `donnees mixtes|afdm`) et des commentaires du
script de reproductibilité. Les corriger casserait des clés. Seuls les libellés
**affichés** ont été touchés, un par un.

#### `getParseData()` compte en octets, `substr()` en caractères

Le piège le plus coûteux de tout le chantier de traduction, et le plus
silencieux. L'outil qui convertit `paste0("texte ", x)` en `trf("texte %s", x)`
localise l'appel par l'analyseur de R, puis découpe la ligne à ces colonnes.

Or `getParseData()` rend des colonnes en **octets**, et `substr()` découpe en
**caractères**. Le décalage vaut le nombre d'octets surnuméraires de la ligne —
donc **nul tant qu'elle est en ASCII**. L'étendue extraite débordait sur la
virgule suivante, `str2lang()` levait « unexpected `,` », et l'appel était
sauté sans un mot.

Conséquence : l'outil échouait sur **toute ligne accentuée**, c'est-à-dire
exactement sur le français qu'il était censé traiter. Il annonçait 15
conversions là où il y en avait 146, et rien ne le disait — un outil de mesure
qui ment sur ce qu'il n'a pas vu. Corrigé, il en trouve **131** de plus, et la
couverture passe de 94,4 % à 96,6 %.

Règle : dès qu'on découpe une ligne source aux positions de `getParseData()`,
passer par `charToRaw()` / `rawToChar()`. Le `nchar()` d'un fichier source
français n'est jamais son nombre d'octets.

Deuxième piège du même outil : `strsplit("", "\n")` rend `character(0)`,
qu'`unlist()` fait disparaître. Redécouper les lignes réécrites supprimait
ainsi **toutes les lignes vides** des douze fichiers — un diff de plusieurs
milliers de lignes pour 131 conversions réelles. C'est `git diff --numstat` qui
l'a montré, avant tout test.

#### Ce que le modèle écrit, aucun dictionnaire ne le traduira

La réponse d'un modèle de langue est du **texte affiché** — mais elle n'existe
pas quand la page se construit, et le traducteur du navigateur ne remplace que
des correspondances connues. Trois invites imposaient « en français » **en
dur** : un utilisateur anglophone recevait une interprétation entière en
français, sans un mot d'avertissement.

C'est la plus grosse trace de français qui restait, et la seule qu'une mesure
de couverture du dictionnaire **ne peut pas voir** — elle ne mesure que ce qui
est écrit dans les sources.

`hstat_ai_consigne_langue()` (`Utils.R`) rend la consigne à insérer dans
l'invite, d'après `hstat_langue_session()`. Trois sites la prennent :
l'interprétation des résultats (invite et message système) et le livre de codes
de l'atelier CAQDAS — dont les libellés deviennent des **données** du projet, et
doivent donc suivre la langue de qui code.

Hors session Shiny la valeur par défaut reste le français : les fonctions
d'invite restent pures et testables, et aucun appel existant ne change de
comportement.

Même famille pour les **info-bulles de plotly** (`hovertemplate`) : elles ne
sont ni un nœud de texte ni un attribut HTML, le traducteur du navigateur ne
les voit pas. Elles passent par `tr()` côté serveur.

#### Une phrase coupée par une balise n'est plus une chaîne

Le traducteur remplace des **nœuds de texte**. Une phrase mise en forme —
`<b>Toutes fermées</b> — <code>[a ; b]</code>` — n'existe donc nulle part comme
nœud entier : le DOM la coupe en autant de morceaux qu'il y a de balises.
**73 entrées du dictionnaire étaient dans ce cas**, et ce sont précisément les
encadrés d'aide, ceux qui portent le plus d'explications. Elles ne pouvaient
jamais s'appliquer, et restaient en français quoi qu'on fasse.

`traduireHtml()` les traite au niveau de l'**élément** : quand le balisage
entier d'un élément coïncide avec une entrée, il est remplacé d'un bloc.

Traduire plutôt chaque morceau serait pire. Les morceaux courts (« La »,
« ou », « Lancez ») sont ceux qui ressemblent le plus à une valeur de données,
et les écarter par prudence rendrait une phrase à moitié anglaise — « La trial
record opposite is optional ».

**La clé passe par l'analyseur du navigateur avant d'être comparée.**
`innerHTML` est renormalisé à la lecture : guillemets simples devenus doubles,
entités résolues, attributs réordonnés. Comparer la chaîne du CSV telle qu'elle
y est écrite n'aurait presque jamais collé. Le banc d'essai du test normalise de
la même façon, sinon il testerait autre chose que la réalité.

L'ordre compte : le remplacement en bloc vient **avant** la passe sur les nœuds
de texte, dont les clés sont françaises et qui ne voit donc plus rien à faire
dans le sous-arbre remplacé. Le retour au français restitue le balisage
d'origine, conservé sur l'élément (`__hstatFrHtml`).

Cette passe ne consulte pas la liste des termes du fichier, et n'est donc pas
rejouée à son arrivée : un élément dont le balisage entier coïncide avec une
entrée du dictionnaire est un libellé par construction — une donnée ne peut pas
le reproduire.

#### Une classe posée dans le code mais absente de la feuille de style

Piège trouvé à l'audit, et parfaitement silencieux. La classe de l'encadré
d'interprétation était posée sur **dix-sept** `div` de `mod_tests.R` et définie
**nulle part** : l'encadré que le code construisait ne s'est jamais affiché,
l'interprétation sortait au fil du texte, sans rien qui la distingue des
résultats bruts qu'elle commente.

Elle portait en plus un accent (`interprétation-box`) — un identifiant reste
technique, sinon il dépend de l'encodage du fichier de style. Renommée
`hstat-interpretation`, définie dans `hstat-theme.css`.

Un test balaie les classes du préfixe du projet et exige une règle pour
chacune. Le contrôle ne porte **que** sur `hstat-*` : `btn`, `box`,
`col-md-6` viennent de Bootstrap et de shinydashboard, les exiger dans notre
feuille de style ferait échouer le test sur du code sain.

#### La comparaison des clés se fait des deux côtés élagués

`hstat_i18n_load()` élague ses clés **et** en retire les doublons ; `tr()`
cherche sur la chaîne élaguée. Une clé du CSV bordée d'espaces et un appel sans
espace sont donc la **même** entrée.

Conséquence pour tout contrôle : comparer les chaînes brutes signale des
gabarits « absents » qui sont en fait présents. L'audit s'y est laissé prendre
et a failli ajouter quatre doublons. Le test qui garde la couverture des appels
`tr()`/`trf()` élague les deux côtés.

#### Une cellule de tableau n'est pas un libellé

La correspondance exacte ne suffisait pas : une colonne du fichier chargé
valant « Oui »/« Non » — ou « Total », « Normal », « Moyenne » — coïncide **mot
pour mot** avec des libellés d'interface. Constaté à l'écran : passer en anglais
transformait les « Oui » de l'aperçu en « Yes ». L'application réécrivait les
données que l'utilisateur était venu lire ; c'est le pire défaut possible pour
un outil statistique, et il est silencieux.

Règle : dans une cellule `<td>`, on ne traduit **qu'au-delà de
`LONGUEUR_CELLULE` (25) caractères**. Une valeur de données n'est presque jamais
une phrase entière, alors qu'une interprétation l'est toujours. Les en-têtes
`<th>` restent traduits — ce sont des libellés, pas des données.

Le prix est assumé : une interprétation **courte** placée dans une cellule
(« Test exact de Fisher. », 21 caractères) reste en français. C'est la
dégradation douce du point 2 ; l'inverse — altérer une donnée — n'en est pas
une.

#### Le remplacement passe par une fonction, jamais par une chaîne

`String.replace(trouvé, chaîne)` interprète `$&`, `` $` `` et `$'` dans le
**remplacement** comme des références au texte trouvé. Une traduction contenant
ces suites ressortait corrompue. La forme fonction (`replace(net, function () {
return cible; })`) les rend littéralement.

Les deux défauts sont gardés par un test qui **exécute** le traducteur sous
`node` (banc d'essai fournissant le minimum de DOM), pas par une recherche de
chaîne dans le fichier : un test textuel passerait encore si le code changeait
de forme en gardant le défaut. Il a été vérifié comme échouant sur la version
d'avant correction, sur les deux points à la fois.

#### Les termes du fichier ne se traduisent jamais

La règle de longueur ci-dessus est une **heuristique** : elle protège « Oui »
dans une cellule, mais pas un **en-tête** — un `<th>` est un libellé, sauf
quand c'est le nom d'une variable du fichier. Et un tableau ajouté demain
échapperait à toute annotation posée à la main.

On procède donc à l'envers. Le serveur **envoie au navigateur** la liste des
termes qui viennent du fichier — noms de colonnes et modalités qualitatives
(`hstat_i18n_termes_donnees()`, message `hstat-termes-donnees`). Rien de ce qui
figure dans cette liste n'est traduit, où que ce soit dans la page. Un tableau
ajouté plus tard est protégé sans qu'on y pense.

Prix assumé : si une colonne s'appelle « Total », le libellé d'interface
« Total » cesse d'être traduit lui aussi. C'est la dégradation douce ; altérer
une donnée n'en est pas une.

La liste est **bornée** : une colonne de texte libre porte autant de modalités
que de lignes, l'envoyer entière alourdirait la page sans rien protéger d'utile
(une phrase entière ne coïncide pas avec un libellé). D'où `max_modalites` par
colonne et `max_termes` au total.

Un fichier chargé **alors que** la page est déjà en anglais aurait pu voir ses
valeurs traduites avant l'arrivée de la liste : la réception défait puis refait
la passe, sinon la protection n'agirait qu'au rendu suivant.

#### Phrases composées : traduire le gabarit, jamais les arguments

Les ~217 phrases construites par `sprintf()` n'existent nulle part dans le DOM
comme chaîne entière : le traducteur du navigateur, qui ne remplace que des
correspondances complètes, ne peut rien en faire.

`trf(fmt, ...)` (`Utils.R`) traduit **le gabarit** — « %s : %d valeur(s)
modifiée(s) » entre au dictionnaire avec ses marqueurs intacts — puis applique
`sprintf`. Conséquence voulue : **les arguments ne sont jamais traduits**. Ce
sont eux qui portent les données de l'utilisateur. La règle de protection est
ici obtenue **par construction**, pas par précaution.

Deux garde-fous :

- **Les gabarits ne partent pas au navigateur.** `hstat_i18n_json()` les écarte
  (`%[-0-9.]*[sdfgeix%]`) : traduits dans R avant d'exister, ils ne pourraient
  rien remplacer dans le DOM. 30,8 Ko retombent à 20,5. Le motif vise les
  marqueurs de `sprintf`, pas le caractère `%` seul — « % colonne » part
  toujours.
- **Le journal de reproductibilité est exclu.** `hstat_rlog_*` construit du
  **code R** ; traduire ses gabarits produirait un script que R refuserait
  d'analyser, alors que le journal a précisément pour promesse d'être
  exécutable. Un test balaie ses fonctions et échoue sur tout `trf()` qui s'y
  glisserait.

Une traduction fautive peut avoir perdu un marqueur : `sprintf` lèverait alors
« too few arguments » et ferait tomber toute la sortie pour une simple erreur de
dictionnaire. `trf()` retombe sur le français, qui marche.

#### Un libellé se déclare de trois façons, le balayage doit les connaître

Un titre d'onglet ou de boîte s'écrit `tabPanel("Titre", …)`,
`tabPanel(tagList(icon(…), " Titre"), …)` **ou** `title = tagList(icon(…),
" Titre")`. La troisième forme porte à elle seule 76 titres, et l'avoir
oubliée laissait « Supprimer Variable » en français au milieu d'une interface
anglaise — visible à l'écran, invisible à la mesure.

`hstat_i18n_coverage()` ne dit la vérité que si l'on sait quoi lui donner : le
test qui garde la couverture balaie donc les trois formes, plus les libellés
posés sur les widgets (deuxième argument des `*Input`).

Détail : une chaîne écrite `"\u03b1 (err prob)"` dans la source R ressort de
l'extraction **textuelle** avec l'échappement littéral, alors que R affiche
`α`. Le test les écarte — les compter le ferait échouer sur une différence qui
n'existe pas pour l'utilisateur.

#### Un mot ambigu n'entre pas seul au dictionnaire

La clé étant la chaîne française elle-même, un mot qui a **deux sens** ne peut
pas y figurer : « moyenne » vaut *medium* pour une taille d'effet et *mean* en
statistique. Une entrée pour l'un corromprait l'autre — le défaut symétrique de
celui que la restitution du texte d'origine évite déjà.

La nuance est donc portée par la **phrase entière** : quatre phrases complètes
plutôt qu'un gabarit et un adjectif. Un test vérifie que les adjectifs nus
(`moyenne`, `grande`, `petit`…) restent absents du dictionnaire.

En revanche, un mot **non ambigu choisi par le code** (« équiprobables »,
« blocs égaux ») passe explicitement par `tr()` au point d'appel. C'est la
distinction qui compte : `trf()` ne traduit jamais ses arguments *de lui-même*
— c'est ce qui protège les valeurs de l'utilisateur — mais le développeur peut
déclarer qu'un argument est un libellé, pas une donnée.

#### `tr()` doit suivre la session, sinon elle ne fait rien

Le défaut de `tr()` était `lang = "fr"`. Or **aucun** de ses ~250 points d'appel
ne passe `lang` : la fonction rendait donc le français quoi qu'il arrive. Elle
existait, le dictionnaire la servait, les tests la couvraient — en lui passant
`lang` explicitement, ce qui masquait exactement le défaut. Un défaut qui
neutralise sa propre fonction ne se voit nulle part ailleurs qu'à l'usage.

`lang = hstat_langue_session()`, comme `trf()` et `hstat_err_fr()` : la langue
vient de la **session**, jamais d'une option globale — sur un serveur partagé,
une option ferait basculer la langue de tous les utilisateurs. Un test compare
les deux valeurs par défaut, elles ne doivent plus diverger.

#### « faible » : la phrase porte la nuance, l'adjectif ne le peut pas

Même piège que « moyenne », rencontré à l'inverse. « faible » vaut *small* pour
une **taille d'effet** et *weak* pour une **force d'association** ; le mot ne
peut donc pas entrer seul au dictionnaire. Ce sont les deux sites de taille
d'effet (`interpret_manova_effect`, `interpret_permanova_effect`) qui portent
désormais la phrase entière — quatre gabarits chacun au lieu d'un adjectif
interpolé —, ce qui libère « faible » pour son sens d'association.

Le hasard des accords a résolu le cas voisin : la force d'association dit
« modérée » (accord avec *force*), la taille d'effet « modéré ». Deux chaînes
différentes, donc deux entrées légitimes.

#### Les enfants texte adjacents ne font qu'un seul nœud

Le piège le plus coûteux de la traduction des **alertes**, parce qu'il ressemble
à un succès. Dans

```r
showNotification(tagList(icon("x"), " Le résultat a ", n, " valeur(s)"))
```

le navigateur ne voit pas trois morceaux : il fond toute suite de caractères en
**un** nœud, « Le résultat a 3 valeur(s) » — une chaîne qui dépend des données
et qu'aucune clé ne peut couvrir. Mettre « valeur(s) » ou « au lieu de » au
dictionnaire ne sert donc à *rien* : ces morceaux n'existent nulle part comme
nœud. Trois entrées de ce genre ont été ajoutées puis retirées.

Une **balise** coupe le nœud : `tagList(icon(...), " texte fixe")` reste
traduisible par le dictionnaire, et c'est la forme la plus courante. La règle :

> dès qu'une valeur est interpolée **entre** deux morceaux de texte, l'unité
> affichée passe par `trf()`.

Un test balaie `showNotification`, `showModal`, `shinyalert` et `modalDialog` :
il reconstitue l'unité affichée (en distinguant `paste` de `paste0`, dont les
séparateurs diffèrent), la coupe sur les balises, et échoue sur tout morceau
français portant encore un `%s`.

Ce test ne vérifie **pas** que le gabarit est au dictionnaire — c'est le rôle
d'un autre — mais qu'il passe par `trf()`. Un `paste()` qui reconstituerait par
hasard une clé existante passerait sinon pour correct alors qu'il n'est jamais
traduit à l'exécution : le défaut exact recherché.

#### Un attribut n'est pas du contenu

`IGNORE` (SCRIPT, STYLE, TEXTAREA, CODE, PRE, SVG) protège ce qu'une balise
**contient** : le texte tapé dans une zone de saisie, un extrait de code. Le
`placeholder` d'un `<textarea>`, lui, est un **libellé** posé par
l'application. L'écarter avec le contenu laissait en français tous les exemples
de saisie — précisément les endroits où l'exemple *est* l'aide.

D'où `ignorableAttr()`, qui n'honore que l'exclusion explicite
(`data-hstat-notranslate`). Un test le vérifie sur un `placeholder` de
`<textarea>`.

Corollaire sur le contenu d'un `<code>`, lui bel et bien exclu : un exemple de
syntaxe qui y figure ne sera **jamais** traduit par le navigateur. Les blocs
d'aide qui en contiennent passent donc par `tr()`/`trf()` **côté serveur**, où
l'échantillon suit la langue.

#### Une syntaxe montrée est une syntaxe acceptée

Les exemples de sélection de lignes deviennent « 1 to 10 » en anglais. Les deux
analyseurs (`mod_filter.R`, `mod_clean.R`) acceptent donc `to` au même titre que
`à` : sans cela, la traduction enseignerait à l'utilisateur anglophone une
syntaxe que l'application refuse. Un test extrait `parseRowSelection` de l'arbre
syntaxique des deux modules et lui donne les trois écritures.

#### Une seule définition de ce qu'est un gabarit

`HSTAT_I18N_MARQUEUR` sert au filtre du dictionnaire **et** au test. Deux motifs
distincts finiraient par diverger, et l'un des deux mentirait.

Il ne tolère pas l'indicateur d'espace (`% d`), délibérément : « 100 % **de**
valeurs manquantes » n'est pas un gabarit, c'est un libellé où le pour-cent est
suivi du mot « de ». Un motif plus permissif y voyait un marqueur, écartait la
phrase du dictionnaire du navigateur, et la faisait passer pour une traduction
fautive.

Trois familles de texte, trois chemins :

| Texte | Chemin | Pourquoi |
|---|---|---|
| Interface (libellés, onglets, boutons) | dictionnaire côté navigateur | chaîne entière présente dans le DOM |
| Messages d'erreur, verdicts | `tr()` côté **serveur**, même CSV | composés phrase par phrase, donc absents du DOM comme chaîne entière |
| Interprétations **composées** (`sprintf`) | `trf()` côté **serveur**, gabarit dans le même CSV | la chaîne entière n'existe jamais dans le DOM ; seule l'armature est traduite, les arguments passent intacts |

`hstat_langue_session()` lit `session$userData` — **pas une option globale** :
sur un serveur partagé, une option ferait basculer la langue de tous les
utilisateurs dès que l'un change la sienne. La valeur par défaut de
`hstat_err_fr()` l'appelle, ce qui évite de toucher ses ~70 points d'appel.

Détail de typographie : le français met une espace avant les deux-points,
l'anglais non. Un test le vérifie — garder la ponctuation française dans une
phrase anglaise trahirait la traduction.

Un terme identique dans les deux langues (« Exploration ») est une **décision de
traduction**, pas un déchet : il compte dans la couverture, mais
`hstat_i18n_json()` ne l'envoie pas au navigateur où il ne ferait rien.

Piège corrigé : une clé de cache vide levait « attempt to use zero-length
variable name ». Un dictionnaire introuvable est un cas **normal** (paquet non
installé, exécution depuis un dossier quelconque) et ne doit pas empêcher le
démarrage.

`hstat_i18n_coverage()` dit ce qui est couvert et nomme ce qui manque ; un test
échoue si une entrée du menu latéral cesse d'être traduite.

## Dispositifs de malherbologie : du contenu, pas un moteur de plus

`hstat_malherbo_catalog()` (`mod_design.R`) porte cinq dispositifs de la
spécialité — densités croisées, série additive, méthodes intégrées de
désherbage, dose-réponse, efficacité et sélectivité d'un herbicide.

**Ce ne sont pas de nouveaux plans.** Ce sont des **structures de traitements**
posées sur les plans qui existent déjà (factoriel, blocs de Fisher) :
`hstat_design_base()` ramène le dispositif à son plan de base *une seule fois*,
en tête de `hstat_agri_design()`, et tout le reste du moteur l'ignore —
randomisation, carte, export, conseils d'analyse. Écrire un second moteur pour
la malherbologie ferait diverger les deux à la première correction, exactement
comme un second moteur de fusion pour les feuilles Excel. Un test l'interdit.

Ce que la spécialité apporte, c'est le **contenu** : les modalités usuelles, le
modèle à ajuster, et surtout **le piège propre à chaque dispositif**. Un
catalogue qui ne dirait que des noms n'apprendrait rien à qui sait déjà nommer
son essai.

### Le témoin est l'invariant, et il est testé

Chaque dispositif doit porter sa référence, faute de quoi il ne répond pas à sa
propre question :

| Dispositif | Témoin exigé | Sans lui |
|---|---|---|
| Série additive | A0, sans adventice | aucune perte de rendement n'est calculable |
| Dose-réponse | D0, dose nulle | la courbe n'est plus ancrée à l'origine |
| Efficacité / sélectivité | enherbé **et** propre | l'efficacité n'a plus de dénominateur, ou la baisse de rendement ne se distingue plus de la concurrence résiduelle |
| Désherbage intégré | T0 non désherbé | l'efficacité ne se mesure contre rien |
| Période critique | ET **et** PT permanents | le propre porte le dénominateur de toute perte, l'enherbé en borne le maximum ; sans eux, aucune des deux courbes n'est ancrée |
| Date et fréquence | T0 **et** TP | « aussi bien que le propre » ne se teste pas sans le propre |
| Parcelles appariées, bandes traitées | T0 non traité | rien à quoi comparer |
| Série substitutive | les deux peuplements **purs** | ils sont les dénominateurs de RY<sub>c</sub> et RY<sub>a</sub> ; sans eux, RYT n'existe pas |

Trois dispositifs reposent sur un plan que HStat possédait déjà — parcelles
appariées (`paired`) et bandes croisées (`strip`) — et n'en créent aucun : ils
lui apportent le contenu de la spécialité. Un test le vérifie.

Deux autres pièges sont écrits dans le catalogue et affichés à l'écran : une
dose-réponse doit **encadrer** la réponse (dose nulle *et* dose saturante),
sinon ED90 est extrapolé et non estimé ; et les densités croisées font varier
la densité **totale**, si bien qu'un effet attribué à la compétition peut
n'être qu'un effet de peuplement — c'est la raison d'être de la série additive.

### Les modalités ne contiennent pas de virgule

Le champ de saisie sépare les modalités par des virgules. Une virgule dans un
libellé du catalogue scinderait silencieusement une modalité en deux, et le
plan compterait un traitement de plus. Un test balaie les cinq dispositifs.

Corollaire : le générateur automatique « lettre + début..fin » est **désactivé**
sur ces dispositifs. Leurs modalités ne sont pas une suite (`A0, A1, A2`) mais
des doses et des stratégies ; il les écrasait dès le premier passage.

## Classeur Excel : les feuilles sont des fichiers comme les autres

Un classeur d'enquête porte souvent une feuille par année, par site ou par
vague. `hstat_excel_read_sheets()` les lit en une **liste de tableaux**, qui
part ensuite dans `hstat_merge_frames()` — celui-là même qui fusionne plusieurs
fichiers.

**Ne pas écrire un second moteur de fusion.** Toutes les jointures existantes
(empilement, jointure par clé, intersection…) deviennent disponibles sur les
feuilles sans une ligne de logique supplémentaire, et une correction faite dans
le moteur profite aux deux chemins.

`hstat_excel_sheets()` rend `character(0)` — jamais une erreur — pour tout ce
qui n'est pas un classeur lisible : elle alimente une sortie Shiny, où une
erreur ferait tomber tout le panneau de chargement.

Une feuille vide ou illisible est **écartée et nommée**, pas fatale : sur un
classeur de douze feuilles, une seule mal formée ne doit pas bloquer les onze
autres.

`hstat_excel_compat()` compare les colonnes et **conseille** : mêmes colonnes →
empilement, colonnes communes partielles → jointure par clé, aucune colonne
commune → ni l'un ni l'autre (et l'on suggère de vérifier les en-têtes).
L'utilisateur n'a pas à deviner ce que sa structure de données appelle.

Détail de langue : le moteur parle de « fichiers », c'est son vocabulaire
d'origine. Le message est retraduit en « feuilles » à l'affichage — lire
« 3 fichiers » après avoir combiné trois feuilles d'un même classeur est
déroutant.

## Un fichier statique servi sous un nom inchangé reste en cache

`hstat_asset()` estampille la feuille de style et les scripts de la version :
`hstat-theme.css?v=0.36.1`. Sans cela, le navigateur garde la version qu'il a
déjà — l'application est mise à jour sur le serveur, et l'utilisateur continue
de voir **l'ancienne mise en page** sans qu'aucun message ne le lui dise. Le cas
s'est présenté sur téléphone, où l'on ne sait même pas comment forcer un
rechargement.

L'estampille est la **version**, qui monte à chaque modification (règle du
dépôt) : le fichier est donc retéléchargé exactement quand il le faut, et mis en
cache le reste du temps. Un horodatage le ferait retélécharger à chaque
démarrage ; une valeur figée, jamais.

Un test barre tout appel direct à `"hstat-theme.css"`, `"hstat-session.js"` ou
`"hstat-i18n.js"` : c'est celui qu'on oublie qui garde l'ancien fichier.

## Responsive : rien ne disparaît, rien n'est coupé

Règle de conduite : ce qui ne tient pas en largeur se **replie** (colonnes
empilées) ou **défile dans son propre conteneur** (tableaux larges) — jamais
hors de la page. `.wrapper { overflow: hidden }` fait qu'un débordement n'est
pas rattrapable : la colonne est perdue, et **rien ne le signale**.

Toutes les règles vivent dans `www/hstat-theme.css`, section « RESPONSIVE ».
Elles étaient auparavant réparties entre la feuille de thème et un
`tags$style()` de `UX.R` ; un test échoue désormais sur tout `@media` réintroduit
dans `UX.R`, parce que deux endroits finissent par se contredire — et c'est
exactement ce qui s'était produit.

### Le défaut : 70 px de barre latérale posés sur le contenu

L'escamotage de la barre était écrit `translate(-230px, 0)`, la valeur
d'AdminLTE, alors que HStat déclare `dashboardSidebar(width = 300)`. Les 70 px
de différence restaient **par-dessus** le contenu sur tout écran de moins de
991 px : « Charger données » s'affichait « er données », sur **tous** les
onglets. Constaté sur un iPhone SE.

`translate(-100%, 0)` ne dépend d'aucun chiffre : la largeur peut changer,
l'escamotage suit. Un test barre le retour de tout pixel en dur.

Corollaire : ouverte, la barre se **pose** sur le contenu au lieu de le pousser.
Pousser de 230 px un contenu qu'elle recouvre sur 300 le décalait sans le
dégager — on perdait le bord gauche *et* le bord droit.

### Escamoter avec `transform` seul ne suffit pas

Trois règles déplacent la barre latérale, chacune avec **sa** valeur : celle
d'AdminLTE (−230 px, la largeur de *sa* barre), celle que shinydashboard injecte
pour `width = 300`, et celle du mode replié. Il suffit qu'une seule l'emporte
pour que la barre revienne **à moitié** sur le contenu — signalé à l'écran deux
fois, dont une dans un contexte (iframe SyGNAR sur téléphone) que la mesure
locale ne reproduisait pas.

D'où **deux mécanismes indépendants** : `left: -100%` *et*
`transform: translate(-100%, 0)`. `left` ne dépend d'aucun `transform` ; un
`transform` résiduel ne peut alors qu'éloigner la barre davantage, jamais la
ramener. Le défaut change de nature : au pire elle est trop cachée, jamais à
moitié posée sur le texte.

L'état **replié** (`sidebar-collapse`) vaut à **toute largeur**, hors de toute
media query : le repli ne connaît pas de largeur. Et la variante `sidebar-mini`
d'AdminLTE laisse un rail d'icônes de 50 px — le menu de HStat n'a pas d'icônes
seules, ce rail poserait des puces muettes sur le texte.

### Sur téléphone, le menu est un tiroir

Ouvert, il recouvre le contenu — c'est le comportement voulu d'un tiroir. Encore
faut-il qu'il se **referme** : on ouvre le menu, on choisit un onglet, et sans
cela il reste posé sur les résultats qu'on venait lire, le bouton qui le
fermerait étant lui-même recouvert. Il se referme donc au choix d'une entrée et
au premier contact avec le contenu.

Les événements passent par **jQuery** (`$(document).on`), jamais par
`addEventListener` : c'est la règle déjà apprise sur les événements de Shiny.

### Un conteneur flex en colonne réduit ses enfants à leur contenu

`.mv-layout` passait en `flex-direction: column` sous 1100 px, en gardant
`align-items: flex-start` : les deux colonnes se réduisaient alors à la largeur
de leur **contenu**. Le catalogue tombait à 285 px dans une fenêtre de 800, et à
**24 px** sur téléphone — un mot par ligne. Sous 1100 px l'empilement se fait
donc en **blocs**, dont la largeur ne se discute pas.

### Trois détails qui ne se voient qu'à l'usage

1. **`word-break: break-word` casse à l'intérieur des mots.** Posé sur `.btn`
   pour éviter qu'un libellé long ne déborde, il écrivait « Browse… » **une
   lettre par ligne**. `white-space: normal` seul suffit : le repli se fait aux
   espaces.
2. **Les marges négatives des `row` de Bootstrap** (−15 px) supposent un parent
   qui les rattrape par 15 px de rembourrage. Le rembourrage resserré du
   téléphone ne les rattrape plus : chaque rangée dépassait sa boîte de 7 px, et
   la boîte se mettait à défiler pour rien. Sur téléphone les colonnes sont de
   toute façon empilées : la gouttière négative n'a plus d'objet.
3. **Les champs sont à 16 px sur téléphone.** En dessous, Safari iOS **zoome**
   dès qu'on touche un champ, et la page reste zoomée : l'utilisateur se
   retrouve avec une interface coupée sans avoir rien demandé.

L'en-tête, lui, n'est pas amputé : ce sont des commandes, pas de la décoration.
Il est resserré (100 px de haut au lieu de 300 sur 667), et tout y reste.

## Messages d'erreur : jamais du R brut

`hstat_err_fr()` (`Utils.R`) traduit les erreurs de R en français. Toute erreur
montrée à l'utilisateur passe par là — `showNotification`, `validate(need())`,
colonne « Interprétation » d'un tableau de résultats, texte tracé dans un
graphique. Un test balaie le dépôt et échoue sur tout nouveau
`showNotification(paste("…", e$message))`.

Deux règles dans les traductions de `HSTAT_ERR_FR` :

1. **Cause puis geste.** « Variance nulle » ne suffit pas ; il faut « toutes les
   valeurs sont identiques, choisissez une autre variable ». Un test vérifie que
   chaque traduction contient un verbe d'action.
2. **Le message R d'origine survit**, entre parenthèses. Sans lui, un
   utilisateur qui demande de l'aide n'a plus rien à montrer, et une traduction
   fautive devient indébuggable.

Une erreur non reconnue n'est pas maquillée : elle est annoncée comme *non
traduite*. Présenter un message anglais comme une phrase française serait pire
que de dire qu'il ne l'est pas.

Même logique pour les paquets absents : `hstat_pkg_manquant()` donne la
commande d'installation **et** une analyse de repli déjà disponible.
« Package 'klaR' indisponible. » était une impasse.

### `<<-` dans un gestionnaire d'erreur, sinon la ligne est perdue

Six analyses (normalité, homogénéité, t-test, Wilcoxon, Kruskal-Wallis,
Scheirer-Ray-Hare) construisaient une ligne de résultat portant
`hstat_err_fr(e)` — puis la **jetaient** :

```r
results_list <- list()
for (var in vars) tryCatch({ ... }, error = function(e) {
  results_list[[var]] <- data.frame(...)      # affectation LOCALE au gestionnaire
})
```

Le `<-` crée une copie dans le cadre du gestionnaire ; la liste de
l'observateur n'est pas touchée. À l'écran, la variable en échec **disparaît
du tableau sans un mot** — et si c'était la seule, l'utilisateur ne reçoit
qu'un « Aucun résultat généré » qui masque la vraie cause (« toutes les
observations portent la même valeur… »). Tout le travail de traduction des
messages était annulé à l'endroit même où il devait servir.

À ne pas confondre avec le cas légitime : `gdf[[fvar]] <- ...` dans les
gestionnaires de comparaisons multiples porte sur une variable **créée dans le
corps du gestionnaire**, que celui-ci renvoie. D'où la règle du balayage : dans
un `error =`/`warning = function(e)`, une affectation à un nom que le
gestionnaire n'a pas lui-même défini doit passer par `<<-`. Un test balaie le
dépôt ; il a été vérifié comme signalant exactement les six sites et aucun des
trois `gdf`. `values` est exclu — `reactiveValues` est un objet à référence,
y écrire depuis un gestionnaire a bien un effet au dehors.

## Persistance de la session

Un verrouillage d'écran ne doit pas fermer l'application. Trois pièces :

1. `session$allowReconnect("force")` dans `app_server.R`. **`"force"` et non
   `TRUE`** : sans lui, Shiny n'active la reprise que derrière un serveur qui la
   gère (Connect, Shiny Server Pro), or HStat tourne le plus souvent en local —
   cas où le besoin est le plus fort.
2. `www/hstat-session.js` : bandeau français, reconnexion à recul progressif,
   reprise immédiate au retour de visibilité / de focus / du réseau, signal de
   maintien toutes les 25 s, confirmation avant fermeture quand des données sont
   chargées.
3. Le voile gris de Shiny est masqué en CSS, sinon il recouvrirait le bandeau.

**Piège vérifié à l'écran : les événements de Shiny sont émis par jQuery.**
`document.addEventListener("shiny:disconnected", …)` ne les voit **jamais** —
le bandeau ne s'affichait pas du tout. Il faut `$(document).on(…)`.

Second piège : le fichier est chargé dans l'en-tête, donc parfois **avant** que
Shiny soit prêt. `Shiny.addCustomMessageHandler` n'existe pas encore et le
gestionnaire est perdu en silence ; on réessaie jusqu'à ce que Shiny réponde.

Le signal de maintien passe en `priority: "event"` : c'est un signal, pas une
entrée. Sans cela il resterait dans `input` et pourrait redéclencher des
observateurs en aval.

## Classifications sur paquets optionnels

`klaR` (k-modes), `poLCA` (LCA) et `clustMixType` (k-prototypes) ne sont pas
installés par défaut : ce sont les seules analyses qu'on ne peut ni exécuter ni
tester sur une machine minimale.

D'où la règle : **sortir la statistique de qualité du paquet**.
`hstat_kmodes_pseudo_r2()`, `hstat_lca_entropie()` et `hstat_part_equilibre()`
vivent dans `Utils.R` et sont testées sur des valeurs posées à la main. C'est le
seul moyen de garder ces trois analyses sous contrôle sans pouvoir les lancer.

Elles rendent toutes un verdict à quatre états dont `indeterminable`
(`hstat_seuil_verdict`), et les `switch()` de `app_server.R` traitent ce
quatrième cas explicitement — sinon une partition non calculable serait
affichée comme « peu séparée », ce qui est faux.

## Mise en forme : les données de l'utilisateur cassent le markdown

Un tableau markdown vit sur **une ligne par enregistrement**, et la barre
verticale y sépare les colonnes. Deux caractères le détruisent donc, et tous
deux viennent de vraies données :

- le **retour à la ligne**, omniprésent dans les réponses libres du module
  qualitatif — la ligne se scinde et tout le tableau part de travers ;
- la **barre verticale**, qu'un en-tête de CSV peut porter (`Rendement|t/ha`).

`.hstat_rep_tableau_md()` protège désormais les deux, **y compris dans les noms
de colonnes** — ils ne l'étaient pas, et l'en-tête annonçait alors une colonne
de plus que le séparateur : le tableau ne se rendait plus du tout.

Corollaire : `.hstat_rep_cellules()` doit découper sur les barres **non
échappées** seulement, sinon le rendu HTML réinvente la colonne fantôme que
l'échappement venait d'éviter.

Même logique pour le texte alternatif des figures (`.hstat_rep_alt()`) : un
crochet dans un titre d'analyse fermait le `![...]` trop tôt, l'image n'était
plus reconnue et le markdown brut ressortait dans le document.

## Le journal doit rester exécutable, quel que soit le nom de colonne

`.hstat_rlog_nom()` cite les noms non syntaxiques entre accents graves — mais un
nom peut **lui-même** en contenir. Une colonne `` a`b `` fermait la citation
trop tôt et produisait un script que R refusait d'analyser, alors que le journal
a précisément pour promesse d'être exécutable. R accepte l'accent grave échappé
par une barre oblique inverse ; la barre elle-même doit donc être échappée
d'abord. Un test balaie une batterie de noms hostiles.

## Un curseur qui commence au défaut ne permet que d'agrandir

`HSTAT_LBL_PT_MIN` valait 12, puis 11 — le défaut de ggplot2. Le curseur de
taille des étiquettes partait donc **du** défaut : impossible de réduire. Or
c'est le besoin le plus courant, un nuage de plusieurs dizaines d'individus
voyant ses étiquettes se recouvrir. Le plancher est à **8 pt**, qui reste
lisible sur une figure exportée à 300 DPI.

Le **défaut**, lui, ne bouge pas : c'est toujours celui de ggplot2, l'état
d'origine qu'on doit pouvoir retrouver sans le chercher — et il doit rester
atteignable par le curseur. Un test vérifie les trois à la fois.

## Ne jamais recommander une analyse sur une variable vide

`.hstat_reco_type()` rend `"indeterminable"` quand la variable ne comporte
**aucune** valeur observée. Sans ce garde-fou, `unique(na.omit(x))` était vide,
donc de longueur 0 ≤ 2, et la variable était typée **binaire** : le moteur
recommandait un chi-deux d'indépendance sur une colonne entièrement vide.

Conseiller avec aplomb une analyse impossible est **pire que ne rien
conseiller** — c'est ce qu'un utilisateur suit sans se méfier. Les listes
`quanti` / `quali` / `ordin` excluent naturellement ce type, et
`hstat_reco_analyses()` émet en plus une ligne `Bloquant` qui nomme la variable
et renvoie à l'onglet Nettoyage : le silence seul serait trompeur.

Même exigence de langue dans `hstat_report_resume_donnees()` : une colonne vide
est lue comme `logical` par les lecteurs de CSV, et le nom de classe R sortait
tel quel au milieu d'un tableau français.

## Graphiques interactifs : le polyfill obsolète de plotly

plotly attache une dépendance `typedarray`, polyfill destiné aux navigateurs
sans tableaux typés (IE9). Son code référence `GLOBAL`, variable de Node.js
inexistante dans un navigateur : une `ReferenceError` était levée sur **chaque**
page portant un graphique interactif. Rien ne cassait, mais une erreur
permanente en console masque les vraies.

`hstat_plotly_clean()` retire la dépendance. Le nettoyage est posé sur
**`renderPlotly` lui-même**, pas sur chacun des dix appels : leurs corps
comportent des `return()` qui sauteraient purement et simplement un habillage de
l'expression. `shiny::exprToFunction()` transforme le bloc en fonction — le
`return()` en sort alors normalement, et la valeur passe bien par le nettoyage.

### Un attribut que le type de trace refuse

Même famille, même point de passage. Une trace `bar` n'a pas de `mode` — c'est
un attribut des nuages de points. Quand la conversion en dépose un,
`plotly_build()` avertit puis le jette : le graphique est juste, mais
l'avertissement revient à **chaque** rendu.

`HSTAT_PLOTLY_INTERDITS` liste les couples type → attribut refusé, et
`.hstat_plotly_attrs()` les retire **avant** la construction. C'est la
différence qui compte : un `suppressWarnings()` global étoufferait aussi les
avertissements qui signalent de vrais défauts. Un `mode` légitime, sur un nuage
de points, n'est pas touché — un test le vérifie, et vérifie aussi que
l'avertissement apparaît bien **sans** le nettoyage.

### Une barre absente se nomme

Une efficacité vaut `NA` dès que le témoin est nul : la formule d'Abbott n'est
pas définie. `geom_col()` retire alors la ligne, l'axe garde la place de la
modalité — vide — et le seul signal partait dans la console de R (« Removed 1
row containing missing values »). L'utilisateur voyait un trou.

Le décompte des valeurs hors cadre ne suffisait pas : il ne portait que sur les
valeurs **finies** sortant de bornes fixées à la main. Les modalités sans
valeur calculable sont donc comptées à part et **nommées** — « une valeur
manquante » n'aide pas à la retrouver parmi onze traitements. Chaque `geom_col`
du module porte en conséquence `na.rm = TRUE` : l'interface le dit déjà, la
console n'a pas à le répéter.

## Tests

`tests/testthat/test-hstat.R` est la suite de référence (celle qu'exécute
`R CMD check` via `tests/testthat.R`). Elle **balaie `R/`** — le socle puis
tous les modules — dans un environnement isolé, sans démarrer Shiny. Les
fonctions de calcul pures y sont testables directement ; les serveurs de module
le sont par `shiny::testServer()`, ce que la migration a rendu possible.

Le dossier est balayé, les fichiers ne sont **pas nommés un à un** : quatre
modules l'étaient, et un cinquième portant une fonction de calcul serait resté
invisible — ses tests auraient échoué sur « could not find function », loin de
la cause.

Placer la logique statistique dans `R/utils.R` plutôt que dans un `observeEvent`
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

## Le paquet s'installe, et c'est l'installation qui a trouvé les deux défauts

`R CMD INSTALL` réussit, et `HStat::run_hstat()` sert l'application depuis le
paquet installé — interface complète, `www/` servi, les 19 analyses parcourues
sans une erreur, exactement comme depuis les sources.

### `Imports` ou `Suggests` : la frontière se mesure

`DESCRIPTION` déclarait **102 `Imports`** : `install.packages()` échouait si
**un seul** manquait — alors que l'application est bâtie pour tourner sans, et
le dit (`hstat_pkg_manquant()` donne la commande d'installation *et* une
analyse de repli). Le fichier contredisait le code.

> **`Imports`** = ce sans quoi l'espace de noms ne se charge pas ou l'interface
> ne se construit pas. **`Suggests`** = ce dont une *analyse* a besoin, et dont
> l'absence est déjà annoncée et surmontée.

Mesure, pas estimation : on relève `loadedNamespaces()` en bâtissant `ui` **et
les dix-sept UI de module** — 19 paquets. Réunis aux 22 que le socle importe
par `importFrom` (sans eux la `library()` échoue) et à `stats`, cela fait
**33 `Imports`** et **72 `Suggests`**. `run_hstat()` continue d'installer
*tout* au démarrage : pour l'utilisateur ordinaire, rien ne change.

### Un `importFrom` ne doit jamais nommer un aiguillage

Un nom importé vit dans `imports:HStat`, **cherché avant l'environnement
global** où `hstat_installer_replis_ui()` pose les aiguillages : l'aiguillage
ne pouvait donc jamais gagner. `importFrom(shinyjs, colourInput)` traînait dans
`NAMESPACE`, et shinyjs **ré-exporte** un ersatz devenu caduc — d'où
« colourInput() has been moved to the 'colourpicker' package », qui faisait
tomber **toute** la construction de l'interface.

Le défaut n'existe **que dans le paquet installé** : depuis les sources il n'y
a pas d'environnement d'imports, et l'aiguillage l'emportait. C'est la raison
d'être de cette étape — aucun parcours depuis le dépôt ne pouvait le voir.

### Le compilateur d'octets voit ce que la lecture ne voit pas

`mod_coding.R` appelait `hstat_q_apply_palette(p, palette, low = , high = )`
alors que les paramètres s'appellent `col_low` / `col_high`. R lève « unused
arguments » **à l'appel** : le nuage de mots tombait entièrement dès qu'une
palette autre que « default » était choisie. Jamais au chargement, jamais à la
lecture — seulement sous les doigts de l'utilisateur.

Un test balaie désormais tout appel à une fonction **maison** dont un argument
nommé ne correspond à aucun paramètre (les fonctions à `...` sont hors de
portée, elles absorbent tout). Il a été vérifié comme signalant exactement ce
site avant correction.

### `R CMD check` passe, et c'est lui qui a trouvé sept fonctions fantômes

Lancer, depuis la racine :

```sh
R CMD build --no-build-vignettes --no-manual .
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual HStat_<version>.tar.gz
```

**`_R_CHECK_FORCE_SUGGESTS_=false` est la clé** : sans lui, le check exige que
les 72 `Suggests` soient installés et refuse de démarrer. C'est ce que font
rhub et CRAN ; le croire bloquant a longtemps fait passer le check pour
impossible ici.

État : **1 avertissement, 2 notes**, tous les trois expliqués plus bas. Point de
départ : 1 **erreur**, 5 avertissements, 3 notes.

#### Sept appels vers des fonctions qui n'existent pas

`glmmTMB::gaussian()`, `binomial()`, `poisson()`, `Gamma()`,
`inverse.gaussian()` — glmmTMB porte ces noms dans son espace de noms **sans
les exporter**, si bien qu'une attribution par `getNamespaceExports()` pouvait
les croire siens. Ce sont les familles de `stats`. Le sélecteur de loi du GLMM
levait « 'gaussian' is not an exported object ».

`emmeans::cld` n'existe pas davantage : enfermé dans un `tryCatch`, il rendait
`NULL` au lieu de lever — **un repli mort**, qui donnait l'illusion d'un filet
de sécurité.

`factoextra::fviz_mfa_biplot` n'existe pas non plus, et l'appel était dans un
`tryCatch(error = NULL)` : **le biplot AFM n'a jamais rien affiché**, sans un
message. Le module traitait pourtant déjà le cas pour l'AFDM — « factoextra n'a
pas de `fviz_famd_biplot`, on superpose » ; l'AFM appelait une fonction
fantôme. Il est composé de la même façon.

Un test balaie désormais **tous** les `pkg::objet` du dépôt et vérifie que
chacun existe. Un paquet absent de la machine est ignoré : le dire quand même
ferait échouer le test sur l'environnement, pas sur le code.

#### Un test qui ne trouve rien doit sauter, pas échouer

48 échecs pour **une seule** cause : sous `R CMD check`, le paquet est
*installé* — `inst/app/` y est aplati en `app/`, `R/` a disparu. Les tests qui
**balaient les sources** n'avaient plus rien à lire, concluaient à l'absence de
ce qu'ils cherchaient, et échouaient.

Vingt et un des quatre-vingts tests concernés n'avaient pas de garde. Le saut
est donc posé dans `.hstat_repo_root()` — **le localisateur, que tous
traversent** — et non dans chacun d'eux : une liste tenue à la main aurait
dérivé au premier test ajouté.

#### Deux défauts que seule l'intégration continue pouvait voir

**`PMCMRplus::dunnTest` n'existe pas.** `dunnTest` est le nom **de FSA**, et le
socle l'appelle correctement ainsi ; deux sites de `mod_tests.R` l'attribuaient
à PMCMRplus, dont la famille s'appelle `kwAllPairsDunnTest` — c'est d'ailleurs
la forme qu'emploient ses voisines immédiates (Conover, Nemenyi), qui lisent
comme elle `mc$p.value`. Le post-hoc de Dunn levait « is not an exported
object ».

Le test qui balaie les `pkg::objet` **saute les paquets absents de la machine**
— sinon il échouerait sur l'environnement et non sur le code. PMCMRplus n'étant
pas installé ici, seule la CI pouvait le voir. C'est le test qui fonctionne
comme prévu, pas une lacune.

**Le test des identifiants dupliqués était instable, à 6,1 % par rendu.**
`tabsetPanel()` numérote ses onglets `tab-<entier au hasard>-<n>`, l'entier
étant tiré entre 1000 et 10000 **à chaque construction**. L'application en rend
trente-quatre : la probabilité qu'au moins deux partagent le même tirage est de
6,1 %. Le test échouait donc environ une fois sur seize, sur un code
parfaitement correct.

Vérifié avant de conclure : passer un `id` explicite à `tabsetPanel()` **ne
change pas** ces ancres — l'`id` nomme la liaison d'entrée, pas les cibles.
J'avais commencé par ajouter huit identifiants sur cette hypothèse ; la mesure
l'a démentie, et ils ont été retirés. Le tirage est interne à Shiny.

Les ancres sont donc écartées du décompte. Ce que le test cherche — les
identifiants que **l'application** déclare deux fois, comme les 108 boutons
homonymes qu'il a trouvés — reste entièrement couvert : aucun d'eux ne porte
cette forme.

#### Ce qui reste, et pourquoi

| Reste | Raison |
|---|---|
| **WARNING** caractères non-ASCII | L'interface est en français : ~9 700 chaînes accentuées. Les échapper en `\uxxxx` rendrait la source illisible. Compromis assumé, pas un défaut. |
| **NOTE** `Suggests` indisponibles | Le conteneur est hors ligne ; 28 des 72 ne s'installent pas. Disparaît sur une machine reliée à CRAN. |
| **NOTE** « no visible global function definition » | Exactement les **neuf aiguillages** (`%>%`, `withSpinner`, `colourInput`, `pickerInput`, `ggplotly`, `renderPlotly`, `plotlyOutput`, `config`, `updatePickerInput`). Ils vivent dans l'environnement global par contrat : le compilateur ne peut pas les y voir. |

Le troisième point se supprimerait en définissant les neuf au premier niveau du
paquet, comme `layout` l'a été. `layout` a dû l'être parce que son nom existe
aussi dans `graphics` et que le compilateur le résolvait là — sept
avertissements « unused argument ». Les autres n'ont pas d'homonyme de base :
la note est cosmétique, la fragilité ne l'était pas.

#### Trois nettoyages que le check a rendus visibles

- **`exportPattern(".")` ne servait à rien.** Le pont recopie depuis l'**espace
  de noms** (`ls(asNamespace("HStat"), all.names = TRUE)`), ce qui atteint les
  aides `.hstat_*` exportées ou non. Le prix, lui, était réel : une fiche de
  documentation réclamée pour chacun des ~240 objets exportés. Seul
  `run_hstat` est exporté désormais — rien n'est caché à l'application,
  seulement à `library(HStat)`.
- **Le socle appelait shiny sans préfixe** et dépendait donc de `importFrom` —
  or `checkboxInput` n'y figurait pas : `hstat_axe_titre_ui()` aurait échoué
  dans le paquet installé sans shiny attaché. 57 appels qualifiés.
- **`.hstat_scope_banner()` était défini dans `UX.R`** et appelé par des UI de
  **module**, donc par du code du paquet. Cela ne cassait rien — `UX.R` est
  sourcé avant qu'aucune UI ne soit construite — mais la dépendance allait à
  l'envers. Il a rejoint le socle.

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

## Un seul écrivain d'image, et il est le seul à ouvrir un périphérique

`hstat_ecrire_image()` (`Utils.R`) écrit **toutes** les images de l'application ;
`.hstat_img_device()` est le **seul** endroit qui ouvre un périphérique
graphique. Un test balaie `inst/app/` et échoue sur tout `ggsave()`, `png()`,
`jpeg()`, `tiff()`, `bmp()`, `pdf()`, `postscript()`, `svg()` posé ailleurs.

Ce n'est pas une préférence de style. Ce que l'écrivain garantit — plafond de
résolution, fichier valide du format demandé, image de secours portant le motif
— ne profite qu'à ce qui passe par lui. Douze écritures brutes vivaient dehors
et n'en avaient rien : le rapport (deux), les tests statistiques (cinq), le
module qualitatif, la visualisation, et **le kit d'export partagé lui-même**.

Trois défauts en sont sortis, tous silencieux :

1. **Un `stop()` dans un `content =` ne laisse aucun fichier**, et Shiny renvoie
   sa page d'erreur HTML sous le nom `.png` demandé. Treize exports du kit
   partagé et l'export qualitatif étaient dans ce cas — on croyait tenir une
   image, on ouvrait du HTML.
2. **`on.exit()` s'accroche à un cadre de fonction, pas au bloc d'un
   `tryCatch`.** La fermeture du périphérique était donc repoussée à la sortie
   de `hstat_ecrire_image()` : le contrôle final lisait un fichier encore vide
   et croyait l'export perdu, et sur erreur l'image de secours était tracée sur
   un second périphérique **puis écrasée** par la fermeture du premier —
   l'utilisateur recevait une image vide au lieu du motif. D'où la fonction
   anonyme qui enferme le tracé.
3. **Le rapport plafonnait ses figures nulle part** : à 2400 dpi (choix proposé
   dans son interface) une figure de 9 pouces demande 21 600 px de côté, que le
   matriciel ne peut pas allouer.

`secours = FALSE` est l'unique dérogation, et elle est réservée au **rapport** :
là, une figure indessinable doit disparaître du document, une image d'erreur au
milieu d'un rapport remis serait pire que son absence. Partout ailleurs le filet
est indispensable — c'est lui qui empêche le HTML déguisé en PNG.

**Les réglages de format appartiennent à l'écrivain.** `qualite` (JPEG) et
`compression` (TIFF) y sont passés : c'est parce qu'ils vivaient dans le seul
module qui les propose que ce module gardait son propre `ggsave`. Un test vérifie
qu'ils **agissent** (qualité 5 pèse moins que qualité 100) et qu'une valeur
aberrante retombe sur 95 au lieu de faire tomber l'export.

### Le format et le DPI ne se déclarent qu'au catalogue

Dix-sept listes de formats et vingt champs de DPI étaient écrits à la main, et
ils avaient **divergé** : les neuf exports des analyses multivariées n'offraient
que quatre formats sur les sept que l'écrivain sait produire — et sans libellé —
tandis que deux modules écrivaient `20000` en clair là où les autres lisaient
`HSTAT_DPI_MAX`. C'est exactement ce qui s'était déjà produit : une montée du
plafond en avait laissé plusieurs en arrière.

`HSTAT_FORMATS_IMG`, `hstat_format_input()` et `hstat_dpi_input()` (`Utils.R`)
remplacent les trente-sept déclarations. Deux choix de construction :

1. **`hstat_format_input()` n'a pas d'argument `choices`**, et
   **`hstat_dpi_input()` n'a pas d'argument `max`.** C'est précisément ce qui
   permettait à chaque appel d'inventer sa propre liste et son propre plafond.
   Ce qui varie légitimement d'un site à l'autre — le libellé, le minimum, le
   pas, la largeur — reste paramétrable.
2. **Le catalogue est dérivé de ce que l'écrivain sait écrire.** Offrir un
   format qu'il ignore ne lève rien : `hstat_img_fmt()` retombe sur PNG, et
   l'utilisateur reçoit un PNG portant l'extension demandée. Un test vérifie
   les deux sens — chaque valeur du catalogue traverse `hstat_img_fmt()` sans
   changer, **et** produit un fichier dont les premiers octets sont ceux du
   format annoncé.

Mesuré dans la page rendue après correction : 31 champs de DPI, tous à 20 000 ;
29 sélecteurs de format, tous offrant les 7 formats.

Le balayage de garde vise le **champ de DPI**, pas le nombre 20 000 : les champs
de largeur et de hauteur en **pixels** portent le même plafond sans être des
résolutions. Il remonte donc à la tête de l'appel `numericInput(`, seule ligne
qui porte l'identifiant et le libellé — s'arrêter aux lignes voisines confondait
un champ de pixels avec le champ de DPI déclaré juste au-dessus.

### Le préfixe est le contrat

`hstat_export_plot_ui(ns, prefix)` déclare les quatre réglages et le bouton ;
`hstat_export_plot_handler(input, prefix, plot_fun)` les lit et écrit. **Le
module ne fournit plus que la fonction qui rend le graphique** — il n'écrit plus
une ligne de fichier, ne nomme plus de format, ne borne plus de résolution.

Quatre exports vivaient encore à côté du kit, et chacun y perdait quelque chose
que l'utilisateur voyait :

| Export | Ce qui manquait |
|---|---|
| Distribution (exploration) | PNG imposé, taille figée : ni format ni dimensions |
| Valeurs manquantes (exploration) | idem |
| Graphique descriptif | taille figée à 9,8 × 7,1 po — les curseurs voisins ne règlent que l'aperçu |
| Plan expérimental | taille figée à 11 × 7 po ; un dispositif à vingt blocs y écrase ses étiquettes |

Corollaire découvert en migrant : **l'aperçu et le téléchargement construisaient
chacun leur exemplaire du graphique**, avec les mêmes huit arguments recopiés.
Deux copies à tenir d'accord, et rien pour signaler qu'elles avaient divergé. Le
graphique passe donc par un `reactive()` unique, que `renderPlot` et l'export
lisent tous deux — c'est ce que faisaient déjà les modules bâtis sur le kit.

## Un seul écrivain de tableaux, comme il n'y a qu'un écrivain d'image

Vingt téléchargements de tableaux montaient chacun leur classeur : même boucle
sur les feuilles, même archive ZIP de CSV, même `tryCatch`, même notification.
Ce qui leur appartenait vraiment, c'est la **liste nommée de tableaux**.

`hstat_ecrire_classeur()`, `hstat_ecrire_csv_zip()`, `hstat_classeur_handler()`,
`hstat_csv_handler()` et `hstat_export_tables_handlers()` (`Utils.R`) portent le
reste. Bilan : −460 lignes dans `app_server.R`, −79 dans `mod_tests.R`.

Ils partageaient aussi le défaut des images : **un `req()` ou un `return(NULL)`
sans avoir écrit le fichier** fait renvoyer à Shiny sa page d'erreur HTML, que le
navigateur enregistre en `.xlsx` — Excel refuse alors de l'ouvrir, sans dire
pourquoi. Tout chemin écrit donc un classeur valide, portant le motif s'il n'y a
rien à exporter.

Trois pièges que la mise en commun a fermés :

1. **Un nom de feuille vient parfois d'une variable de l'utilisateur.** Les
   post-hoc nomment leurs feuilles `Lettres_<variable>_<facteur>`, tronqués à 31
   caractères mais **jamais nettoyés** : `addWorksheet()` lève sur `[ ] : * ? / \`,
   et l'export entier tombait pour un nom de colonne. `hstat_feuille_nom()`
   nettoie et tronque ; deux noms devenus identiques après troncature sont
   distingués, sinon Excel refuse le doublon.
2. **Le ZIP de CSV vidait `tempdir()`** de tous ses `.csv` avant d'écrire — un
   dossier partagé par toute la session. L'écriture se fait dans un sous-dossier
   dédié, supprimé à la sortie, et `zip` est appelé en mode silencieux (chaque
   téléchargement écrivait sa liste de fichiers dans la console du serveur).
3. **Un tableau seul n'a pas besoin d'une archive.** `hstat_csv_handler()` rend
   un `.csv` quand il n'y a qu'un tableau, un `.zip` au-delà — et l'extension
   annoncée suit, parce qu'un `.zip` contenant un CSV nu ne s'ouvre pas comme on
   s'y attend.

**Ce qui n'a pas été migré, et pourquoi.** L'export Excel du Khi² applique des
styles (en-têtes colorés, remplissage selon la significativité, largeurs
automatiques) : ce n'est plus une liste de tableaux, et le faire entrer de force
dans l'écrivain commun lui ferait perdre sa mise en forme. Le module de seuils
garde de même son modèle de dimensions en **pixels** lus à 96 ppp, documenté
plus haut : c'est une décision, pas un oubli.

### Le prix du contrat par préfixe, et le garde-fou qui le paie

Tant que chaque export déclarait son `output$<identifiant>`, une faute de frappe
se voyait à la lecture. Avec un préfixe, l'identifiant est **construit**
(`<préfixe>Xlsx`) : une lettre de travers débranche le bouton en silence — il
reste affiché, il ne fait rien, et rien dans le code ne le signale.

Un test refait donc la construction des deux côtés — boutons déclarés d'un côté,
producteurs de l'autre, kits compris — et échoue sur tout bouton sans
producteur. **108 boutons** sont couverts ; il a été vérifié comme signalant
exactement les deux boutons orphelins d'un préfixe volontairement mal
orthographié. Les identifiants construits en boucle (`paste0("mv_", key, …)`)
sortent des deux listes à la fois : ils ne sont pas couverts, et le test ne
prétend pas l'inverse.

## Audit : cinq défauts silencieux et les mesures qui les ont trouvés

Aucun ne se voyait à la lecture du code, et aucun ne faisait tomber quoi que ce
soit — c'est ce qui les rendait durables.

### Un observateur branché sur un champ que personne n'écrit

`observeEvent(values$multiResults, …)` déposait le contexte des **comparaisons
post-hoc**. Or rien n'écrit `multiResults` : le module écrit
`multiResultsMain`. L'observateur ne s'est donc jamais déclenché, et cette
famille manquait à l'onglet d'interprétation, au journal de reproductibilité et
au rapport.

Le test de couverture des familles ne pouvait pas le voir : il cherche l'**appel**
`hstat_ai_capture(values, "…")` dans le source, et le déclarait donc couvert. Un
second test vérifie désormais le **déclencheur** — tout `observeEvent(values$X)`
contenant une capture doit guetter un champ réellement écrit quelque part.

Corollaire : les métadonnées de cette capture lisaient `values$multiGroups`, lui
non plus jamais écrit. Les variables comparées se lisent dans le tableau
lui-même.

### Deux boutons, un seul identifiant

Les deux boutons « Diagnostiquer mes données » portaient
`runManovaDiagnostic`. Cliquer marchait — la liaison de Shiny lit l'id de
l'élément cliqué — mais `updateActionButton()` ou `shinyjs::disable()` n'en
auraient atteint qu'un, et le HTML était invalide.

La mesure porte sur la **page rendue**, pas sur le source : un identifiant écrit
deux fois dans deux branches qui ne s'affichent jamais ensemble n'est pas un
doublon. Détail qui a failli fausser la mesure : chercher `id="…"` sans l'espace
qui précède fait correspondre `data-grid="true"`, et annonce 108 doublons
imaginaires.

### `install.packages()` dans le corps du serveur

Six paquets des analyses multivariées étaient installés **à chaque session**.
Hors ligne, chaque ouverture attendait l'expiration de la requête CRAN (sept
tentatives relevées pour sept sessions d'essai) ; sur un serveur partagé, la
bibliothèque est le plus souvent en lecture seule, et deux sessions simultanées
pouvaient y écrire ensemble.

L'installation appartient au **démarrage** : `hstat_model_packages` (`Utils.R`),
traité une fois au source. Un test balaie les dix-sept fichiers et échoue sur
tout appel — en passant par l'analyseur de R, parce que plusieurs messages
citent `install.packages(...)` comme **aide à l'utilisateur** et qu'un balayage
textuel les compterait.

### Les intervalles de prévision gardaient la classe `ts`

`as.matrix()` sur une série multiple **ne retire pas** la classe `ts` des
colonnes extraites : `class(lo[, 1])` vaut « ts ». ggplot ne sait pas choisir
d'échelle pour ce type et le signalait à chaque tracé de prévision. La courbe
sortait quand même — mais un avertissement permanent en console masque les
vrais. Mesuré dans le journal du serveur : **1 avertissement avant, 0 après**.

### `sample(x, n)` pioche dans `1:x` quand `x` est un seul nombre

Quatre randomisations de plans d'expérience étaient dans ce cas. Un essai à un
seul traitement codé numériquement aurait vu apparaître un traitement qui
n'existe pas, **dans un plan d'expérience** et sans un mot. La forme employée
est désormais `x[sample.int(n)]`, qui tire sur les indices.

### Ce que l'audit a écarté, et pourquoi

- Les cinq réglages de style du **second axe** de la visualisation étaient lus
  dans la liste de dépendances du graphique mais déclarés nulle part. Ce n'est
  pas un défaut fonctionnel : l'axe Y2 est **volontairement calqué** sur Y1 et
  reprend ses styles. Les lectures mortes sont retirées — elles laissaient
  croire à des réglages absents.
- L'export Excel du Khi², les dimensions en pixels du module de seuils : des
  décisions documentées, pas des oublis.

### Une période répétée est presque toujours une date

Le sélecteur « Période (facteur intra-sujet, répété) » ne retenait que les
facteurs, les chaînes et les numériques à peu de modalités. Une colonne `Date`
n'est aucun des trois : elle n'apparaissait **jamais** dans la liste — alors que
l'exemple affiché sous le champ annonce « Ex. : temps, date, stade… ». Les
mesures répétées les plus courantes, celles datées, étaient donc inaccessibles.

`Date`, `POSIXct` et `POSIXlt` sont désormais éligibles comme facteur intra- et
inter-sujet.

**L'ordre des niveaux est celui du temps, et c'est le vrai piège.** `factor()`
appliqué à une `Date` classe sur la valeur sous-jacente, donc
chronologiquement ; passer d'abord par `as.character()` ou par un format
français trierait **alphabétiquement**, et le 5 avril viendrait avant le 19
mars. L'écart ne se voit qu'en traversant un changement de mois — à l'intérieur
d'un même mois les deux ordres coïncident, et un exemple mal choisi ferait
croire que le piège n'existe pas. Le test le vérifie sur des dates qui changent
de mois.

Deux messages accompagnent les cas dégénérés, qu'une colonne de dates rend
faciles à produire : un facteur à **une seule modalité** n'est pas répété, et un
facteur comptant **autant de modalités que d'observations** ne répète rien du
tout (le message invite alors à regrouper les dates par mois ou par stade).

## Le socle vit dans `R/`, l'application le charge par un pont

Première étape de la conversion en vrai paquet R, et elle est structurelle :
les ~5 700 lignes de définitions qui vivaient dans `inst/app/Utils.R` sont
passées dans **`R/utils.R`**, où elles forment le code du paquet.
`inst/app/Utils.R` reste en place — `HStat.R` le source toujours en premier —
mais il n'a plus que deux rôles : **charger le socle**, puis **faire ce qui doit
l'être au démarrage**.

### Pourquoi ce partage précisément

Dans un paquet R, le code de premier niveau est évalué **à l'installation**, pas
au chargement. Trois familles d'expressions ne peuvent donc pas rejoindre `R/` :

1. les **réglages de session** (`options(encoding=)`, `shiny.maxRequestSize`,
   `shiny.plot.res`) et la **locale** — ils appartiennent au démarrage ;
2. l'**installation des paquets** (`install_and_load(required_packages)`,
   `hstat_load_model_packages()`) — la liste est une définition, l'appel non ;
3. les **replis conditionnels** des paquets d'interface optionnels
   (`withSpinner`, `plotlyOutput`, `colourInput`, `pickerInput`, `rank_list`,
   `renderPlotly`). C'est le point le plus subtil : `if (!.hstat_has("plotly"))`
   placé dans `R/` figerait sur la machine de **construction** une décision qui
   appartient à la machine d'**exécution**.

S'y ajoutent les deux alias anti-masquage (`em <- shiny::em`,
`margin <- ggplot2::margin`) : ils protègent l'environnement de l'application,
pas le paquet.

Décompte réel du découpage : **256** expressions de premier niveau, dont **16**
restent au pont (139 lignes) et **240** partent au socle (5 740 lignes).

### Deux invariants, chacun testé

- **Le socle ne fait rien** : aucune expression de premier niveau autre qu'une
  affectation (`"_PACKAGE"`, support de la documentation, excepté). Un
  `options()` qui s'y glisserait serait posé à l'installation.
- **Le pont ne définit rien** d'autre que son chargeur : les seuls noms qu'il
  affecte sont `.hstat_charger_socle`, `.hstat_max_mb`, `em` et `margin`. Une
  fonction utilitaire qui y réapparaîtrait serait une **seconde source de
  vérité** — celle que l'application verrait, sans que le paquet ni les tests en
  sachent rien.

### Les sources priment sur le paquet installé

`.hstat_charger_socle()` cherche `R/utils.R` en remontant depuis le dossier de
l'application **avant** de se rabattre sur l'espace de noms. Sur un poste où
HStat est aussi installé, l'ordre inverse ferait travailler l'application sur une
version antérieure à celle qu'on est en train d'éditer — défaut particulièrement
pénible parce qu'il ne se voit pas. Un test vérifie l'ordre.

Quand le paquet est installé (`run_hstat()` sur `system.file("app")`), il n'y a
plus de fichier `R/utils.R` : le pont **recopie** alors les objets de l'espace de
noms dans l'environnement de l'application. La recopie est nécessaire parce que
l'application appelle aussi des aides internes `.hstat_*`, qu'un `library()` ne
rendrait pas visibles — d'où `exportPattern(".")` dans `NAMESPACE`, tenu à la
main et redéclaré en roxygen dans `R/utils.R` pour qu'un futur `document()` le
reproduise au lieu de l'effacer.

### Ce que cette étape ne prouve pas encore

`R CMD check` et `pkgload::load_all()` **n'ont pas pu être exécutés** ici : 22
des 107 `Imports` sont indisponibles hors ligne. Ce qui est vérifié, c'est que le
socle se charge **seul** (241 objets, `sys.source()` suffit), que la suite de
tests s'appuie désormais sur lui, et que l'application démarre et fonctionne
inchangée par le pont — 19 analyses parcourues, aucune erreur. La validation de
l'installation appartient à une machine disposant du réseau.

### Étape 2 : un module dans le paquet, et il devient testable

`mod_tests.R` est le module pilote : il vit désormais dans **`R/mod_tests.R`**.
Sa ligne a disparu de `HStat.R` — un module migré n'a plus de place dans l'ordre
de `source()`, qui était la contrainte à lever. Le pont charge `R/utils.R` puis
**tous les `R/mod_*.R`**, sans ordre : les modules ne font que définir, mesuré
sur les dix-sept fichiers (0 expression agissante).

**Le gain n'est pas théorique, il est vérifié** : `shiny::testServer()` exécute
`mod_tests_server` hors application. Le test lance la normalité, l'ANOVA, puis un
t de Student sur trois groupes, et observe ce que le module **écrit** dans
`values` — pas ce à quoi son code ressemble. Jusqu'ici c'était impossible : un
module n'existait que comme effet de bord d'un `source()` séquentiel.

Deux obstacles rencontrés, tous deux instructifs :

1. **Les replis d'interface vivaient dans le pont.** Tester le module échouait
   sur « could not find function `updatePickerInput` ». Leur **définition**
   appartient au paquet ; seule la **décision de les installer** appartient au
   démarrage. D'où `hstat_installer_replis_ui(envir)`, appelée par le pont — et
   appelable par un test. Le bloc y est évalué **tel quel**
   (`eval(quote({...}), envir)`) : une première tentative de le réécrire en
   `assign()` avait coupé une définition dont le corps tenait sur la ligne
   suivante.
2. **Le socle appelle sans préfixe.** Il vient de l'application, où `shiny` et
   `ggplot2` sont *attachés* ; dans un paquet, rien ne l'est. Mesure : **75
   symboles de 20 paquets**. Ils sont désormais déclarés en `importFrom` exacts
   — et non en `import()` entier, qui ferait s'entre-masquer `count`, `layout`
   ou `dataTableOutput` avec une résolution dépendant de l'ordre des directives.

Reste à faire, dit franchement : les **modules** appellent encore `renderDT`,
`renderPlotly` et consorts sans préfixe. Le test du module attache donc `DT` et
`ggplot2` explicitement. Qualifier ces appels est le chantier suivant, module par
module — c'est la convention que le dépôt s'est déjà donnée pour le reste du
code.

### Les balayages doivent voir les deux dossiers

Un test qui n'énumérerait que `inst/app/` **cesserait de voir** le code migré —
et passerait au vert en ne regardant plus rien. Tous les balayages passent donc
par `.hstat_sources_app()`, et la lecture d'un module par `.hstat_module_path()`.

Piège rencontré en posant ce localisateur : la substitution mécanique qui a
recalé les balayages a touché **le corps de la fonction elle-même**, qui s'est
mise à s'appeler récursivement — « C stack usage is too close to the limit ».
C'est exactement le défaut déjà documenté pour les réactifs de l'application ;
il s'attrape à l'exécution, jamais à la lecture.

### Étape 3 : un module migré n'appelle plus rien sans préfixe

`mod_tests.R` qualifie désormais ses **1 888** appels de fonctions d'autres
paquets (`shiny::`, `ggplot2::`, `DT::`, `dplyr::`, `shinydashboard::`,
`ggtext::`). Preuve mesurée : son serveur s'exécute sous `testServer()` avec
**shiny seul attaché** — plus de `library(DT)` ni `library(ggplot2)` dans le
test, qui devait auparavant reproduire le contrat de démarrage.

**Les replis deviennent des aiguillages permanents.**
`hstat_installer_replis_ui()` ne définissait un nom que si le paquet était
**absent** ; un paquet **installé mais non attaché** ne donnait donc ni l'un ni
l'autre — l'échec constaté en intégration continue (« could not find function
`updatePickerInput` »). Les onze noms concernés sont maintenant *toujours*
définis : ils pointent sur la fonction du paquet quand il est là, sur
l'équivalent de base sinon. L'application ne dépend plus de ce que `library()` a
attaché.

Ces onze noms ne se qualifient **jamais** : ce sont des fonctions de
l'application. Qualifier `renderPlotly` ferait sauter le nettoyage du polyfill
plotly ; qualifier `plotlyOutput` ferait sauter le repli quand plotly est
absent.

### Trois pièges de la réécriture mécanique, tous rencontrés

1. **Les colonnes de `getParseData()` sont en OCTETS, `substr()` en
   caractères.** Sur un fichier accentué, découper aux colonnes annoncées avec
   `substr()` corrompt le texte. Le remplacement se fait donc en octets, avec
   vérification systématique que les octets trouvés à la position sont bien le
   nom attendu.
2. **`tags$code(...)` est étiqueté `SYMBOL_FUNCTION_CALL`.** Une première passe
   a produit `tags$shiny::code(...)`, que R refuse d'analyser. Il faut écarter
   les appels précédés de `$` et `@`, pas seulement de `::`.
3. **Un paquet qui en ré-exporte un autre fausse l'attribution.** `plotly`
   ré-exporte `mutate`, `group_by`, `summarise` de dplyr, et `shinyjs`
   ré-exporte `colourInput` de colourpicker : prendre « le premier paquet qui
   exporte le symbole » donne une provenance vraie techniquement mais fausse
   sur le fond.

Un test garde l'invariant : dans un module **migré**, aucun appel non qualifié
n'appartient à un paquet des `Imports`. Il ne s'applique qu'aux modules déjà
dans `R/` — les autres l'atteindront à leur tour.

## Fins de ligne

Attention : le dépôt est **mixte**, et bien plus qu'il n'y paraît. La fin de
ligne suit le fichier, pas le dossier — le déplacement des modules dans `R/` ne
l'a pas changée. Sont en **CRLF** : `R/utils.R`, `R/mod_clean.R`,
`R/mod_descriptive.R`, `R/mod_design.R`, `R/mod_explore.R`, `R/mod_filter.R`,
`R/mod_qualitative.R`, `R/mod_tests.R`, `R/mod_threshold.R`, `R/mod_viz.R`,
`inst/app/Utils.R`, `inst/app/HStat.R`, `inst/app/app.R` et `README.md`.
Sont en **LF** : `R/mod_ai.R`, `R/mod_coding.R`, `R/mod_dl.R`, `R/mod_ml.R`,
`R/mod_report.R`, `R/mod_timeseries.R`, `R/run_hstat.R`, `R/zzz.R`,
`R/_disable_autoload.R`, `inst/app/UX.R`, `inst/app/app_server.R`, le `app.R`
racine, `CLAUDE.md` et la suite de tests.

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


### Orthographe : corriger en masse sans rien casser

Le français sans accent est une faute, et elle était partout. La correction se
fait **à la position exacte donnée par l'analyseur de R** (`getParseData`,
positions en **octets** — `substr()` compte en caractères), sur les seules
chaînes de prose, à partir des mots que hunspell rejette et dont **une seule**
variante accentuée est un mot français.

Trois garde-fous, appliqués par construction. Chacun répare un dégât constaté :

1. **Une chaîne sans espace n'est pas corrigée.** C'est peut-être un nom de
   colonne (`Modalite`), une valeur de `switch` (`indeterminable`) ou une
   classe CSS : un accent l'y rend introuvable. Les cas sont examinés un par un.
2. **Une chaîne qui sert de motif à `grepl`/`sub`/`gsub` n'est corrigée nulle
   part.** Ces motifs se comparent à du texte déjà déplié par
   `hstat_sans_accents()`.
3. **Ni CSS, ni JavaScript, ni code R engendré.** Le premier balayage a produit
   `text-décoration` (propriété inexistante), `{priority: 'évent'}` (le message
   change de nature) et `use = "pairwise.complète.obs"` (le script du journal
   ne s'exécutait plus). Aucun des trois ne se voit à l'analyse syntaxique.

Quand plusieurs formes accentuées sont valides, **la finale est conservée** :
« separe » donne « sépare », jamais « séparé ». Le participe passé se rétablit
ensuite sur le contexte grammatical (auxiliaire, négation, nom qui précède) ;
l'inverse ne serait pas rattrapable, un verbe conjugué n'offrant aucun indice.

Le correcteur ne peut pas tout : « traite », « installe », « demande » sont des
mots valides, et « homoscedasticite » n'existe ni nu ni accentué dans son
dictionnaire. Ces deux familles se traitent à part, par le contexte et à la
main.

### Un motif comparé à du texte déplié reste sans accent

`hstat_sans_accents()` (`Utils.R`) est la **seule** définition du repli : le
même `chartr` était recopié sept fois, et une copie oubliée c'est un
rapprochement qui échoue en silence.

Corollaire, et c'est le piège : `.hstat_rlog_code()` compare un titre **déplié**
à ses motifs. Accentuer `grepl("regression lineaire", t)` en
`grepl("régression linéaire", t)` fait que la condition n'est **plus jamais**
vraie — le journal de reproductibilité restait vide pour la régression
linéaire, l'AFDM et les tests à une référence, sans erreur ni avertissement.
Un test balaie les variables affectées depuis `hstat_sans_accents()` et refuse
tout motif accentué qui les vise.

Même famille, trouvée du même coup : le détecteur d'échelle ordinale comparait
des libellés **non dépliés** à une liste où « énormément » côtoyait
« enormement » ; et le lexique de sentiment contenait « qualité » et
« problème », qui ne peuvent jamais correspondre à un jeton déjà déplié.

### Un nom de colonne se relabellise, il ne s'accentue pas

`dq$Gravite` casserait sur une machine dont la locale n'est pas UTF-8 : le nom
interne reste sans accent. Mais un tableau rendu tel quel affiche ses **noms de
colonne**, et « Gravite » est une faute à l'écran. D'où `hstat_dq_affichage()`,
appelée au rendu **et** à la construction du rapport — les deux, sinon le
document exporté garde l'orthographe fautive que l'écran a corrigée.

Le piège symétrique est plus grave : la colonne créée s'appelait `Ecart_type`
et quatorze lectures la demandaient sous `Écart_type`. La colonne existait, la
lecture rendait `NULL`, la sortie partait sans un mot. Un test rapproche les
chaînes accentuées des **symboles** du même fichier.

### Traduction anglaise : un terme, un mot

- « graphique » se dit **plot** — celui de R et de ggplot2 — jamais « chart ».
- Une répétition de bloc est un **replicate** ; « repetition » désigne la redite.
- **dataset**, en un mot.
- Le seuil alpha est un **significance level**.
- L'anglais ne met pas d'espace avant « : ; ! ? » ni avant le pour-cent, mais
  **les deux-points ne disparaissent pas** : « Seuil : » rendu « Threshold »
  perd le signe qui annonce le champ.

Un test garde ces règles, plus l'égalité des marqueurs de `sprintf` des deux
côtés — une traduction qui en perd un fait tomber toute la sortie.

### Le recalage du dictionnaire ne fusionne jamais deux clés

La clé **est** la chaîne française : corriger une faute dans le code sans
corriger la clé rend l'entrée inutile, en silence. Le recalage rapproche donc
les clés orphelines des chaînes réellement produites — y compris celles qui
sont **assemblées à l'exécution** (`HSTAT_ERR_FR`, les recommandations), et qui
n'existent nulle part comme chaîne entière dans le code.

Mais deux clés distinctes ne doivent jamais converger vers la même : « Code »
absorbé par « code », et l'une des deux traductions disparaît.

### Une dépendance web annoncée sous un nom qui n'existe pas

`shiny::sliderInput()` attache la dépendance « strftime » en annonçant
`strftime-min.js`, alors que le paquet livre `strftime.min.js` : 404 sur
**chaque** page. Rien ne casse, mais une erreur permanente en console masque
les vraies — exactement le polyfill `typedarray` de plotly.

`hstat_reparer_deps()` republie la dépendance corrigée sous une version
supérieure : `resolveDependencies()` garde la plus haute, la déclaration
réparée l'emporte donc sur celles que chaque curseur pose de son côté.

### Ce que la suite de tests ne peut pas voir

Elle ne démarre jamais Shiny. Les défauts d'interface — un onglet qui ne se
rend pas, une exception JavaScript, un rapport vide — se trouvent en **lançant
l'application** et en la pilotant dans un navigateur, sur un jeu normal *et* sur
un jeu hostile (colonne vide, colonne constante, valeurs manquantes, retour à
la ligne dans une cellule, barre verticale et accent grave dans un nom de
colonne).

Note d'environnement, à ne pas confondre avec un défaut d'HStat : sur les
paquets R de Debian, `jquery.nouislider.min.js` est un lien vers la version
autonome de noUiSlider, qui n'installe pas le greffon jQuery. Les filtres
numériques de DT y sont donc inertes et la console affiche
`$x.noUiSlider is not a function`. Le paquet CRAN n'a pas ce défaut.
