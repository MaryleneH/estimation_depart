# Spec 05 — Restitution et arrondis

## Intention métier

L'arrondi est une opération de présentation. Il ne modifie jamais le calcul,
les taux ni les contrôles. Une seule convention, définie dans
`00g_format_restitution.R`, s'applique à tous les CSV, HTML, fiches, graphiques,
cartes et tableaux de bord.

## Convention

| Grandeur | Règle | Exemples |
|---|---|---|
| Nombre attendu de personnes (`departs_*`, `dep_*`, `dont_*`, bornes de fourchette) | entier, **demi vers le haut** : `floor(x + 0,5)` | 0,5 → 1 ; 2,5 → 3 ; 27,5 → 28 ; 27,49 → 27 |
| Taux et parts (`*_pct`, `taux_*`, `part_*`) | une décimale, calculés sur les valeurs exactes | 23,74 → 23,7 |
| Effectifs observés, codes, libellés | jamais modifiés | « 01 », « 2A », 84 |
| NA (secret, non calculable) | conservé | NA, jamais 0 |
| Total et composantes affichés ensemble | composantes par plus forts restes, somme = total affiché | 24,4 + 1,4 + 1,4 → 25 + 1 + 1 = 27 ; 10,6 + 2,6 + 1,6 → 11 + 3 + 1 = 15 |

`round()` de R arrondit au pair (2,5 → 2) et n'est pas utilisé pour les
personnes ; il reste acceptable pour les taux à une décimale.

```gherkin
Fonctionnalité: Restituer les résultats avec une convention d'arrondi unique

  Scénario: [SPEC-REST-001] Arrondir les nombres attendus de personnes
    Étant donné une valeur exacte de 27,5 personnes attendues
    Quand la valeur est préparée pour restitution
    Alors elle doit être affichée comme 28
    Et la règle utilisée doit être "demi vers le haut"

  Scénario: [SPEC-REST-002] Ne pas utiliser l'arrondi au pair de R pour les personnes
    Étant donné une valeur exacte de 2,5 personnes attendues
    Quand la valeur est préparée pour restitution
    Alors elle doit être affichée comme 3, et non 2
    Et la même règle doit s'appliquer dans le JavaScript des cartes (arrondiPers)

  Scénario: [SPEC-REST-003] Calculer les taux sur les valeurs exactes
    Étant donné un numérateur et un dénominateur non arrondis
    Quand un taux est calculé
    Alors il doit être obtenu à partir des valeurs exactes
    Puis arrondi à une décimale pour l'affichage
    Et jamais recalculé à partir de personnes déjà arrondies

  Scénario: [SPEC-REST-004] Préserver les valeurs observées
    Étant donné un code géographique, un libellé ou un effectif observé (entier)
    Quand la restitution est formatée
    Alors ces valeurs ne doivent pas être modifiées
    Et le type entier des effectifs doit être conservé

  Scénario: [SPEC-REST-005] Conserver NA
    Étant donné une cellule masquée ou non calculable
    Quand la restitution est formatée
    Alors NA doit rester NA et ne jamais devenir 0
    Et un NaN ou un infini doit aussi devenir NA

  Scénario: [SPEC-REST-006] Garantir l'additivité d'un total affiché
    Étant donné un total exact et ses composantes exactes affichés ensemble
    (départs = retraite + invalidité + décès ; « Ensemble » = Σ catégories d'une fiche)
    Quand ils sont arrondis
    Alors les composantes doivent être arrondies par la méthode des plus forts restes
    Et leur somme affichée doit être exactement égale au total affiché
    Et les restes égaux doivent être départagés par la plus grande composante, puis l'ordre
    Et si une composante ou le total est NA, chaque composante est arrondie seule

  Scénario: [SPEC-REST-007] Arrondir indépendamment les territoires
    Étant donné des résultats exacts par département et par région
    Quand les restitutions territoriales sont arrondies
    Alors chaque territoire doit être arrondi indépendamment
    Et aucune correction ne doit forcer la somme des départements affichés à égaler la région affichée

  Scénario: [SPEC-REST-008] Respecter l'ordre de production
    Étant donné les résultats exacts du modèle
    Quand les sorties sont produites
    Alors l'ordre doit être : calcul exact → contrôles exacts → secret statistique → arrondi de restitution → écriture
    Et aucun arrondi ne doit précéder un group_by / summarise

  Scénario: [SPEC-REST-009] Laisser intacts les objets analytiques
    Étant donné un objet analytique en session
    Quand formater_restitution est appliqué pour écrire un fichier
    Alors l'objet d'origine ne doit pas être modifié
    Et une relance de la chaîne doit produire les mêmes objets exacts

  Scénario: [SPEC-REST-010] Formater les nombres en français
    Étant donné un nombre de personnes à afficher dans une page HTML
    Quand il est formaté
    Alors les milliers doivent être séparés par une espace
    Et NA doit s'afficher « n.d. » dans les fiches et « Résultat non diffusé » dans les cartes
```

## Traçabilité

Code :

- `R/00_config_fonctions/00g_format_restitution.R` —
  `arrondir_nombre_personnes()`, `arrondir_taux()`,
  `arrondir_composantes_avec_total()`, `formater_restitution()`,
  `fmt_personnes()`, `COLONNES_PERSONNES`, `DECOMPOSITIONS_PERSONNES`
- `R/00_config_fonctions/00f_fonctions_cartes.R` — JavaScript `arrondiPers`,
  `plusFortsRestes`
- Appels : `05`, `07`, `07b`, `07c`, `08`, `08b`, `08c`, `08d`, `09`, `00d`
- `R/README.md` — section « Arrondi des restitutions »

Tests :

- `tests/testthat/test-format-restitution.R` — « arrondir_nombre_personnes :
  cas limites documentés, demi vers le haut, NA jamais 0 » ;
  « arrondir_composantes_avec_total : la somme affichée des composantes = le
  total affiché » ; « formater_restitution : personnes -> entier, taux -> 1
  décimale, codes intacts, NA conservés, décompositions par ligne ; l'objet
  d'origine est intact » ; « NON-RÉGRESSION numérique : la chaîne produit des
  objets analytiques EXACTS, identiques avant/après restitution ; CSV entiers,
  taux exacts »
- `tests/testthat/test-fiches.R` — « cohérence interne : total = somme des
  CS ; 55+ = sous-population »
