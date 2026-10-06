# Spec 04 — Secret statistique primaire et secondaire

## Statut

Référentiel fonctionnel du secret statistique du projet. La règle primaire est
la règle de diffusion de l'Insee pour la Base Tous Salariés ; le secret
secondaire est une protection complémentaire par blocs, propre au projet.

Cette spécification distingue :

- le **secret primaire**, qui décide si une cellule est directement non
  diffusable ;
- le **secret secondaire**, qui empêche de retrouver une cellule primaire par
  différence avec une marge publiée.

La table analytique interne reste complète. Le masquage concerne la diffusion.

## Règles de référence

| Règle | Paramètre | Valeur courante |
|---|---|---|
| Minimum de salariés | `SECRET_MIN_SALARIES` | 5 |
| Minimum d'entreprises (SIREN distincts) | `SECRET_MIN_ENTREPRISES` | 3 |
| Seuil de dominance | `SECRET_DOMINANCE_PCT` | 85 % |

Ces valeurs sont paramétrables ; `regles_secret()` les lit. `regles = NULL`
signifie **usage interne** : aucun secret (fiches `_interne`, seuil 0).

## 1. Secret primaire

```gherkin
Fonctionnalité: Déterminer le secret primaire d'une cellule

  Scénario: [SPEC-SEC-001] Masquer une cellule contenant moins de 5 salariés
    Étant donné une cellule contenant 4 salariés
    Et au moins 3 entreprises
    Et aucune entreprise ne dépassant 85 % de dominance
    Quand le secret primaire est évalué
    Alors la cellule doit être masquée
    Et son motif de masquage doit être "primaire"

  Scénario: [SPEC-SEC-002] Autoriser exactement 5 salariés au titre du seul critère d'effectif
    Étant donné une cellule contenant exactement 5 salariés
    Et au moins 3 entreprises
    Et aucune entreprise ne dépassant 85 % de dominance
    Quand le secret primaire est évalué
    Alors le critère "nombre de salariés" ne doit pas masquer la cellule

  Scénario: [SPEC-SEC-003] Masquer une cellule contenant moins de 3 entreprises
    Étant donné une cellule contenant 2 entreprises distinctes
    Et au moins 5 salariés
    Et aucune entreprise ne dépassant 85 % de dominance
    Quand le secret primaire est évalué
    Alors la cellule doit être masquée
    Et son motif de masquage doit être "primaire"

  Scénario: [SPEC-SEC-004] Autoriser exactement 3 entreprises au titre du seul critère entreprises
    Étant donné une cellule contenant exactement 3 entreprises distinctes
    Et au moins 5 salariés
    Et aucune entreprise ne dépassant 85 % de dominance
    Quand le secret primaire est évalué
    Alors le critère "nombre d'entreprises" ne doit pas masquer la cellule

  Scénario: [SPEC-SEC-005] Masquer une cellule dominée à plus de 85 pour cent
    Étant donné une cellule ayant suffisamment de salariés et d'entreprises
    Et une entreprise représentant 85,1 % de la grandeur protégée
    Quand le secret primaire est évalué
    Alors la cellule doit être masquée
    Et son motif de masquage doit être "primaire"

  Scénario: [SPEC-SEC-006] Ne pas masquer une cellule à exactement 85 pour cent pour le seul critère de dominance
    Étant donné une cellule ayant suffisamment de salariés et d'entreprises
    Et une entreprise représentant exactement 85 % de la grandeur protégée
    Quand le secret primaire est évalué
    Alors le critère de dominance ne doit pas masquer la cellule

  Scénario: [SPEC-SEC-007] Calculer la dominance sur les effectifs ou les départs attendus
    Étant donné plusieurs entreprises dans une cellule
    Quand la part dominante est calculée
    Alors elle doit considérer la part maximale d'une entreprise dans l'effectif de la cellule
    Et la part maximale d'une entreprise dans les départs attendus (somme de p_central) de la cellule
    Et retenir la plus forte de ces deux parts
    Et si la somme des départs attendus est nulle, seule la part dans l'effectif compte

  Scénario: [SPEC-SEC-008] Appliquer une logique OU entre les trois critères
    Étant donné une cellule
    Quand au moins un des critères salariés, entreprises ou dominance est violé
    Alors la cellule doit être couverte par le secret primaire

  Scénario: [SPEC-SEC-009] Ne pas confondre zéro statistique et secret
    Étant donné une cellule soumise au secret
    Quand la table de diffusion est produite
    Alors ses mesures protégées doivent devenir NA
    Et elles ne doivent jamais devenir 0
    Et un résultat réellement nul d'une cellule diffusable doit rester 0
```

### Formulation fonctionnelle

Une cellule est en secret primaire si :

```
effectif_champ < SECRET_MIN_SALARIES
OU n_entreprises < SECRET_MIN_ENTREPRISES
OU part_dominante_pct > SECRET_DOMINANCE_PCT
```

Avec la configuration courante : `effectif_champ < 5 OU n_entreprises < 3 OU
part_dominante_pct > 85`. Les bornes sont strictes dans le sens de la
diffusion : 5 salariés, 3 entreprises et 85 % passent ; 4, 2 et 85,1 % ne
passent pas.

`part_dominante_pct = 100 × max( max_i n_i / Σ n , max_i d_i / Σ d )`, où
`n_i` est l'effectif et `d_i` la somme des `p_central` de l'entreprise `i` dans
la cellule.

## 2. Secret secondaire

### Principe

Le secret secondaire protège une cellule déjà masquée contre une reconstitution
arithmétique :

```
Total publié = 100
Cellule A = secret
Cellule B = 60
Cellule C = 25
→ A = 100 − 60 − 25 = 15
```

Le secret primaire de A serait inefficace : une autre cellule du même bloc doit
être masquée.

```gherkin
Fonctionnalité: Empêcher la reconstitution d'une cellule primaire

  Scénario: [SPEC-SEC-010] Ajouter un masque lorsqu'une seule cellule d'un bloc est masquée
    Étant donné un bloc comportant au moins 3 cellules
    Et exactement une cellule déjà masquée
    Et la marge du bloc publiée
    Quand le secret secondaire est appliqué
    Alors une deuxième cellule du bloc doit être masquée
    Afin que la cellule primaire ne soit pas retrouvable par différence

  Scénario: [SPEC-SEC-011] Choisir la plus petite cellule encore visible
    Étant donné un bloc avec une seule cellule masquée et plusieurs cellules visibles
    Quand le secret secondaire choisit une cellule complémentaire
    Alors il doit masquer la cellule visible ayant le plus petit effectif salarié (effectif_champ)
    Et en cas d'égalité, la première dans l'ordre de la table

  Scénario: [SPEC-SEC-012] Ne rien ajouter si au moins deux cellules sont déjà masquées
    Étant donné un bloc comportant déjà au moins deux cellules masquées
    Quand le secret secondaire est évalué
    Alors aucune cellule supplémentaire ne doit être masquée pour ce seul bloc

  Scénario: [SPEC-SEC-013] Ne rien ajouter s'il n'existe qu'une seule autre cellule
    Étant donné un bloc contenant une cellule masquée et une seule cellule visible
    Quand le secret secondaire est évalué
    Alors aucun masque secondaire supplémentaire ne doit être ajouté par cette règle
    Car masquer la dernière cellule visible reviendrait à masquer la marge elle-même

  Scénario: [SPEC-SEC-014] Identifier la nature du masque
    Étant donné une cellule masquée par une règle primaire
    Alors motif_masque doit valoir "primaire"
    Mais étant donné une cellule masquée uniquement pour protéger une autre cellule
    Alors motif_masque doit valoir "secondaire"
    Et une cellule non masquée doit avoir motif_masque NA

  Scénario: [SPEC-SEC-015] Réévaluer les blocs jusqu'à stabilité
    Étant donné plusieurs blocs de marges qui se croisent
    Et qu'un masque secondaire créé dans un bloc peut laisser une cellule isolée dans un autre
    Quand la protection secondaire est appliquée
    Alors chaque bloc doit être itéré jusqu'à ce que son masque ne change plus
    Puis tous les blocs doivent être repassés ensemble jusqu'à stabilité
    Et le nombre d'itérations doit être borné (max_iter = 20)
```

## 3. Blocs de secret secondaire — dimension PCS

Les blocs sont définis par les marges réellement publiées ailleurs dans les
sorties.

| Niveau diffusé | Bloc protégé | Marge publiée qui justifie le bloc |
|---|---|---|
| France × PCS | `cs1` | total national par grande CS (07, 07b) |
| Région × PCS | `pcs` | France × PCS |
| Région × PCS | `region_code × cs1` | région × grande CS (Σ 08b) |
| Département × PCS | `region_code × pcs` (ou `pcs` si la région n'est pas disponible) | région × PCS |
| Département × PCS | `geo_code × cs1` | département × grande CS (08b) |

```gherkin
Fonctionnalité: Appliquer le secret secondaire aux sorties PCS

  Scénario: [SPEC-SEC-016] Protéger les PCS au niveau France
    Étant donné la table France par PCS
    Quand le secret secondaire est appliqué
    Alors les cellules doivent être examinées par bloc de grande CS
    Car le total national de chaque grande CS est publié

  Scénario: [SPEC-SEC-017] Protéger les PCS au niveau région
    Étant donné la table région par PCS
    Quand le secret secondaire est appliqué
    Alors un premier bloc doit regrouper les cellules d'une même PCS entre régions
    Et un second bloc doit regrouper les cellules d'une même région et grande CS

  Scénario: [SPEC-SEC-018] Protéger les PCS au niveau département
    Étant donné la table département par PCS
    Quand le secret secondaire est appliqué
    Alors un bloc doit regrouper les départements d'une même région et PCS
    Et un autre bloc doit regrouper les PCS d'un même département et grande CS
```

## 4. Blocs de secret secondaire — dimension grande CS

| Niveau diffusé | Bloc protégé | Marge publiée qui justifie le bloc |
|---|---|---|
| France × CS1 | table entière | total national (05, 07) |
| Région × CS1 | `cs1` | France × CS1 |
| Région × CS1 | `region_code` | total régional (Σ 08 par département) |
| Département × CS1 | `region_code × cs1` (ou `cs1`) | région × CS1 |
| Département × CS1 | `geo_code` | total départemental (08, fiches) |

```gherkin
Fonctionnalité: Appliquer le secret secondaire aux sorties par grande CS

  Scénario: [SPEC-SEC-019] Protéger les grandes CS au niveau France
    Étant donné la table France par grande CS
    Quand le secret secondaire est appliqué
    Alors la table entière doit constituer un bloc
    Car le total national est publié

  Scénario: [SPEC-SEC-020] Protéger les grandes CS au niveau région
    Étant donné la table région par grande CS
    Quand le secret secondaire est appliqué
    Alors un bloc doit regrouper une même grande CS entre régions
    Et un autre bloc doit regrouper toutes les grandes CS d'une même région

  Scénario: [SPEC-SEC-021] Protéger les grandes CS au niveau département
    Étant donné la table département par grande CS
    Quand le secret secondaire est appliqué
    Alors un bloc doit regrouper les départements d'une même région et grande CS
    Et un autre bloc doit regrouper toutes les grandes CS d'un même département

  Scénario: [SPEC-SEC-022] Appliquer la même implémentation aux deux dimensions
    Étant donné les tables PCS et grande CS d'un même niveau
    Quand le secret est appliqué
    Alors la même fonction doit servir aux deux dimensions
    Et seule la définition des blocs doit différer
```

## 5. Séparation interne / diffusion

```gherkin
Fonctionnalité: Séparer les données nécessaires au contrôle des données diffusables

  Scénario: [SPEC-SEC-023] Conserver une table analytique complète
    Étant donné des cellules primaires ou secondaires
    Quand la table interne est écrite (sorties/departs_<dim>/interne/)
    Alors aucune valeur analytique ne doit être supprimée
    Et n_entreprises et part_dominante_pct doivent rester disponibles
    Et les contrôles doivent pouvoir être réalisés sur les valeurs exactes
    Et ce dossier ne doit pas être versionné (.gitignore)

  Scénario: [SPEC-SEC-024] Retirer les indicateurs sensibles de la diffusion
    Étant donné une table de diffusion
    Quand elle est écrite
    Alors n_entreprises et part_dominante_pct ne doivent pas y figurer
    Car ces indicateurs révèlent eux-mêmes la structure d'une cellule

  Scénario: [SPEC-SEC-025] Conserver toutes les lignes
    Étant donné une cellule masquée
    Quand la table de diffusion est produite
    Alors sa ligne doit rester présente avec masque = TRUE et son motif
    Et ses mesures (effectif compris) doivent être NA

  Scénario: [SPEC-SEC-026] Ne jamais laisser visible une cellule primaire
    Étant donné une table de diffusion terminée
    Quand chaque cellule visible est contrôlée contre la table interne
    Alors aucune cellule visible ne doit satisfaire secret_primaire
    Et le script doit s'arrêter si c'est le cas

  Scénario: [SPEC-SEC-027] Ne jamais laisser un bloc avec une seule cellule masquée
    Étant donné un bloc de plus de deux cellules couvert par une marge publiée
    Quand la table de diffusion est finalisée
    Alors le bloc ne doit pas contenir exactement une cellule masquée

  Scénario: [SPEC-SEC-028] Appliquer les mêmes critères aux fiches et au 08b
    Étant donné un territoire (fiche) ou une cellule territoire × grande CS (08b)
    Quand la diffusabilité est décidée
    Alors les mêmes trois critères doivent s'appliquer, suivis du secondaire dans le bloc du territoire
    Et un territoire non diffusable ne doit pas avoir de fiche, le motif étant consigné dans le journal

  Scénario: [SPEC-SEC-029] Produire une version à usage interne clairement marquée
    Étant donné FICHES_SECRET faux
    Quand les fiches sont produites
    Alors aucun secret ne doit être appliqué
    Et le dossier doit être suffixé _interne
    Et chaque fiche doit porter un bandeau d'avertissement
    Et les sorties diffusables ne doivent pas être modifiées
```

### Variables dérivées (typologie, spec 10)

Une variable dérivée d'un ensemble de cellules (« CS principale », « CS au plus
gros volume », « départs concentrés sur X », un profil qui dépend de la
concentration) peut borner ou révéler la valeur d'une cellule masquée. La règle
est fixée par la spec 10 (SPEC-TYPO-030 à 034) : dès qu'une cellule du
département est masquée, ces variables sont NA dans la diffusion et les profils
qui en dépendent ne sont pas diffusés ; la table interne reste complète. Aucune
règle de la présente spec n'est affaiblie.

## 6. Ordre des opérations

```gherkin
Fonctionnalité: Appliquer le secret au bon moment

  Scénario: [SPEC-SEC-030] Contrôler avant de masquer
    Étant donné les résultats exacts du modèle
    Quand la chaîne prépare une restitution
    Alors les contrôles de cohérence doivent être exécutés sur les valeurs exactes
    Puis le secret primaire doit être appliqué
    Puis le secret secondaire doit être appliqué
    Puis l'arrondi de restitution doit être appliqué

  Scénario: [SPEC-SEC-031] Appliquer le secret avant toute sortie publique
    Étant donné une table destinée à un CSV, une fiche ou une carte publique
    Quand la sortie est générée
    Alors elle doit utiliser une version déjà protégée
    Et aucune valeur masquée ne doit être récupérable depuis le HTML, le JavaScript, le JSON embarqué ou un attribut
```

## 7. Exemple d'acceptation complet

```gherkin
  Scénario: [SPEC-SEC-032] Primaire puis secondaire sur quatre cellules
    Étant donné un bloc contenant les effectifs 4, 30, 40 et 50
    Et que les critères entreprises et dominance sont satisfaits pour les quatre cellules
    Quand le secret primaire est évalué avec un seuil de 5 salariés
    Alors seule la cellule de 4 salariés doit être masquée en primaire
    Quand le secret secondaire est ensuite appliqué
    Alors la cellule de 30 salariés doit également être masquée
    Et son motif doit être "secondaire"
    Et les cellules de 40 et 50 salariés doivent rester visibles
```

## Invariants à tester automatiquement

- toute cellule primaire est masquée ;
- 5 salariés, 3 entreprises, 85 % ne sont pas primaires pour ce seul motif ;
  4 salariés, 2 entreprises, 85,1 % le sont ;
- les valeurs d'une cellule masquée sont NA, jamais 0 ;
- les tables internes restent complètes ;
- `n_entreprises` et `part_dominante_pct` ne figurent pas dans les tables de
  diffusion ;
- dans chaque bloc de plus de deux cellules, il n'existe pas exactement une
  seule cellule masquée ;
- un masque créé uniquement par protection complémentaire porte le motif
  « secondaire » ;
- l'application itérative des blocs converge ;
- la même implémentation sert aux dimensions PCS et grande CS, aux fiches et
  au 08b.

## Traçabilité

Code :

- `R/00_config_fonctions/00_config.R` — `SECRET_MIN_SALARIES`,
  `SECRET_MIN_ENTREPRISES`, `SECRET_DOMINANCE_PCT`, `FICHES_SECRET`,
  `DEPARTS_PCS_SECRET`, `DEPARTS_CS1_SECRET`
- `R/00_config_fonctions/00d_fonctions_fiches.R` — `regles_secret()`,
  `texte_regle_secret()`, `indicateurs_secret()`, `secret_primaire()`,
  `secret_secondaire()`, `masquer_cellules()`,
  `calculer_indicateurs_territoire()`, `selectionner_territoires()`
- `R/00_config_fonctions/00e_fonctions_departs_pcs.R` — `blocs_secret()`,
  `appliquer_secret_pcs()`
- `R/08_territoires/08b_departs_geo_cs.R`, `08c_departs_pcs.R`,
  `08d_departs_cs1.R`, `09_fiches_territoriales.R`
- `.gitignore` — `sorties/departs_pcs/interne/`, `sorties/departs_cs1/interne/`

Tests :

- `tests/testthat/test-fiches.R` — « secret statistique, règle Insee BTS : < 5
  salariés OU < 3 entreprises OU une entreprise > 85 %, puis secondaire » ;
  « usage interne (seuil 0) : toutes les fiches, aucun masquage, bloc
  entreprises par SIREN, bandeau ; diffusable inchangé » ; « FICHES_SECRET =
  FALSE sur la chaîne : dossier _interne, tous les territoires, journal sans
  écarté »
- `tests/testthat/test-departs-geo-cs.R` — « 5. secret statistique : règle
  Insee (salariés, entreprises, dominance) + secondaire, NA + masque, ligne
  conservée »
- `tests/testthat/test-departs-pcs.R` — « secret : règle Insee (primaire) +
  secondaire minimale sur la diffusion ; tables internes complètes ; NA jamais
  0 » ; « fichiers : interne/ toujours, diffusion/ seulement avec
  DEPARTS_PCS_SECRET »
- `tests/testthat/test-departs-cs1.R` — « H. secret : MÊME implémentation que
  departs_pcs (règle Insee primaire, secondaire par blocs), conventions
  inchangées »
- `tests/testthat/test-cartes-departs.R` — « SECRET : aucune valeur masquée
  dans la structure, le JSON, le HTML ni les infobulles ; table interne
  refusée »
- SPEC-SEC-032 (exemple 4 / 30 / 40 / 50) correspond au cas vérifié dans
  `test-departs-pcs.R` et `test-fiches.R`.
