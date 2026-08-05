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

## Tests

`tests/testthat/test-hstat.R` est la suite de référence (celle qu'exécute
`R CMD check` via `tests/testthat.R`). Elle source `Utils.R` et
`mod_qualitative.R` dans un environnement isolé, sans démarrer Shiny : seules
les **fonctions de calcul pures** y sont testables.

Placer la logique statistique dans `Utils.R` plutôt que dans un `observeEvent`
la rend testable — c'est le motif suivi par `hstat_ref_test()`,
`hstat_metrics_*()`, `hstat_q_*()`, etc.

Lancer : `testthat::test_dir("tests")`.

## Fins de ligne

Attention : le dépôt est **mixte**. `Utils.R` et `mod_tests.R` sont en **CRLF**,
les autres fichiers R en LF. Préserver les fins de ligne existantes lors d'une
édition — un fichier réécrit intégralement en LF produit un diff de plusieurs
milliers de lignes qui masque le changement réel.

Vérification rapide avant de committer :

```sh
git diff --numstat   # le nombre de lignes doit correspondre au changement réel
```

## Langue

Interface, messages, commentaires de code et interprétations statistiques sont
en **français**. Les identifiants (noms de fonctions, d'inputs, de variables)
restent en anglais ou en abrégé technique.
