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

Trois familles de texte, trois chemins :

| Texte | Chemin | Pourquoi |
|---|---|---|
| Interface (libellés, onglets, boutons) | dictionnaire côté navigateur | chaîne entière présente dans le DOM |
| Messages d'erreur, verdicts | `tr()` côté **serveur**, même CSV | composés phrase par phrase, donc absents du DOM comme chaîne entière |
| Interprétations **composées** (`sprintf`) | non traduites à ce jour | demandent une réécriture par gabarit, pas une substitution |

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
