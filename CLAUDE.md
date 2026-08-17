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

### Modules imbriqués : l'ordre de `source()` compte

`mod_qualitative_ui()` appelle `mod_coding_ui()` (l'atelier de codage CAQDAS
vit dans son propre fichier plutôt que d'alourdir les ~2900 lignes de
`mod_qualitative.R`). `HStat.R` doit donc sourcer `mod_coding.R` **avant**
`mod_qualitative.R` — un test garde cette contrainte.

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
