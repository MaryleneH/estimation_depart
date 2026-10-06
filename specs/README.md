# Spécifications métier — estimation des départs

Ce répertoire décrit le comportement attendu de la chaîne `estimation_depart`
sous forme de spécifications lisibles par le métier, en Markdown avec scénarios
Gherkin (Étant donné / Quand / Alors). Chaque règle a été confrontée au code et
aux tests du dépôt : les valeurs, les noms de paramètres et les comportements
cités sont ceux réellement implémentés à la date de rédaction.

## Principes

- Les spécifications décrivent **ce que le système garantit**, pas l'implémentation
  R ligne à ligne.
- Elles complètent les tests `testthat` : les specs rendent les règles métier
  explicites, les tests vérifient techniquement leur respect.
- Toute évolution du modèle ou d'une règle conduit à vérifier si une spec doit
  être modifiée **avant** de modifier le code (voir la doctrine de modification
  dans `09_matrice_tracabilite.md`).
- **Le modèle statistique n'est pas modifié sans décision explicite** : les
  probabilités `p_central`, `p_bas`, `p_haut`, `p_cal_*`, `p_inval`, `p_deces`,
  les parts par cause, les scripts 02 à 04 et les paramètres de 00 qui les
  pilotent (`SCENARIO`, `HORIZON`, `SIGMA_SORTIE`, `AGE_CONJ_TOUS_REGIMES_2023`,
  `MONTEE_RESIDUELLE_2030`, `COEF_CSP_INVALIDITE`, `AGE_SENIOR`…) relèvent d'une
  décision métier, jamais d'un effet de bord d'une évolution de restitution.
- **Un paramètre a une seule source** : `R/00_config_fonctions/00_config.R`.
  Quand une spec cite une valeur courante (par exemple 45 ans), c'est un cas
  d'acceptation, pas une constante à recopier dans le code.

## Organisation

| Spec | Objet principal |
|---|---|
| `01_perimetre_et_champ.md` | Champ, âge minimum, horizon, périmètre, définition d'un départ |
| `02_preparation_donnees.md` | Préparation BTS, PCS / grande CS, contrat géographique, sources externes et replis |
| `03_projection_2030.md` | Projection individuelle, scénarios, causes de départ, fourchette |
| `04_secret_statistique.md` | Secret primaire (règle Insee BTS) et secondaire par blocs |
| `05_restitution_arrondis.md` | Convention d'arrondi et cohérence des restitutions |
| `06_restitutions_territoriales.md` | France, région, département, zonages ; 08, 08b, 08c, 08d |
| `07_cartes_et_fiches.md` | Cartes, tableaux de bord, fiches territoriales, langage public |
| `08_orchestration_et_controles.md` | Ordre d'exécution, étapes facultatives, contrôles et robustesse |
| `09_matrice_tracabilite.md` | Correspondance specs → code → tests, doctrine de modification |

## Convention Gherkin

Les blocs utilisent les mots-clés français :

```gherkin
Fonctionnalité: ...

  Contexte:
    Étant donné ...

  Scénario: [SPEC-DOMAINE-NNN] ...
    Étant donné ...
    Quand ...
    Alors ...
    Et ...
    Mais ...
```

Les valeurs paramétrables restent liées à la configuration réelle. Lorsqu'une
valeur courante est mentionnée, elle ne doit pas être dupliquée en dur dans le
code métier si une variable de configuration existe déjà ; le test
`test-age-min.R` vérifie ce point pour l'âge minimum.

## Identifiants stables

Chaque scénario porte un identifiant `SPEC-<DOMAINE>-NNN`, destiné à être
réutilisé dans les tests `testthat`, les revues de code et les demandes
d'évolution. Domaines : `CHAMP`, `PREP`, `PROJ`, `SEC`, `REST`, `GEO`, `PUB`,
`ORCH`. Un identifiant n'est jamais réattribué : un scénario retiré est marqué
« retiré », son numéro n'est pas réutilisé.

Exemple : `SPEC-SEC-001` désigne la règle de secret primaire liée au seuil
minimal de salariés.

## Traçabilité

Chaque fichier comporte une section **Traçabilité** reliant la règle aux scripts
et, lorsqu'ils existent, aux tests `testthat` (titre exact du `test_that`).
Quand une règle n'est couverte par aucun test, la section le dit explicitement :
c'est une dette identifiée, pas une omission.

## Glossaire

| Terme | Définition dans ce projet |
|---|---|
| BTS | Base Tous Salariés (Insee) : photo 2024 des salariés ; une extraction Parquet ou une table test simulée (`SOURCE_BTS`). |
| Périmètre BITD | Entreprises (SIREN) de la base industrielle et technologique de défense, listées dans `FICHIER_SIREN`. |
| Champ | Salariés du périmètre âgés d'au moins `AGE_MIN_BTS` en 2024, dont la PCS se rattache à une grande CS du modèle. |
| Départ | Sortie définitive de l'emploi d'ici l'horizon : retraite ou fin de carrière, invalidité, décès. Un changement d'employeur n'est pas un départ. |
| Estimation centrale / basse / haute | Trois jeux de probabilités (`p_central`, `p_bas`, `p_haut`) selon l'hypothèse sur l'âge de départ (correctif δ). |
| Grande CS (`cs1`) | Cadres, Professions intermédiaires, Employés, Ouvriers ; obtenue depuis la PCS par `PCS_VERS_CS1`. |
| PCS fine (`pcs`) | Code PCS à 4 caractères tel que lu dans la BTS, conservé jusqu'aux résultats. |
| Cellule | Croisement territoire × catégorie (grande CS ou PCS) d'une table de résultats. |
| Table interne / de diffusion | Même table avant / après application du secret statistique ; l'interne conserve tout, la diffusion masque. |
| Secret primaire / secondaire | Masque décidé par les trois critères Insee / masque ajouté pour empêcher une reconstitution par différence. |
| Stock tous âges | Effectifs actuels de tous les salariés (y compris sous `AGE_MIN_BTS`) par territoire × grande CS, dénominateur de la part à remplacer. |
| Zonage | Maille territoriale d'analyse (`GEO_ANALYSE`) : département, zone d'emploi, région… |
