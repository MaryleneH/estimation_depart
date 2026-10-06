# Spec 01 — Périmètre et champ d'étude

## Intention métier

La chaîne estime les sorties définitives de l'emploi à l'horizon 2030 pour les
salariés des entreprises du périmètre BITD, à partir d'un âge minimum
configurable. Le champ est défini une seule fois, dans la configuration, et
toutes les restitutions en dérivent.

## Paramètres de référence

| Règle | Paramètre | Valeur courante | Où |
|---|---|---|---|
| Âge minimum du champ | `AGE_MIN_BTS` | 45 ans | `00_config.R` |
| Année de référence (photo BTS) | `ANNEE_REF_GRAPHIQUE` | 2024 | `00_config.R` |
| Horizon de projection | `HORIZON` | 6 ans (2024 → 2030) | `00_config.R` |
| Périmètre d'entreprises | `FICHIER_SIREN` | `data/liste_entreprises_fictives.txt` (1 SIREN par ligne) | `00_config.R` |
| PCS rattachables aux grandes CS | `PCS_VERS_CS1` | premier caractère 3, 4, 5, 6 | `00_config.R` |
| PCS hors champ | `PCS_HORS_CHAMP` | premier caractère 1, 2 (agriculteurs ; artisans, commerçants, chefs d'entreprise) | `00_config.R` |
| Libellés dérivés | `LIB_CHAMP`, `LIB_CHAMP_LONG`, `LIB_CHAMP_COURT`, `LABELS_TRANCHES` | « 45 ans et + », « 45 ans et plus », « 45+ », tranches à partir de 45 | `00_config.R` |

```gherkin
Fonctionnalité: Définir le champ de l'estimation des départs

  Contexte:
    Étant donné que l'âge minimum du champ est défini par AGE_MIN_BTS
    Et que sa valeur courante est 45 ans
    Et que l'année de référence des données est 2024
    Et que l'horizon d'étude est 2030

  Scénario: [SPEC-CHAMP-001] Inclure un salarié appartenant au champ
    Étant donné un salarié d'une entreprise du périmètre BITD âgé de 45 ans en 2024
    Et dont la PCS se rattache à une grande catégorie du modèle
    Quand la base d'étude est constituée
    Alors ce salarié doit appartenir au champ de projection

  Scénario: [SPEC-CHAMP-002] Exclure un salarié trop jeune du champ de projection
    Étant donné un salarié du périmètre BITD âgé de 44 ans en 2024
    Quand la base d'étude est constituée avec AGE_MIN_BTS égal à 45
    Alors ce salarié ne doit pas appartenir au champ de projection
    Mais il doit compter dans le stock tous âges utilisé comme dénominateur

  Scénario: [SPEC-CHAMP-003] Exclure un salarié hors périmètre d'entreprises
    Étant donné un salarié dont le SIREN n'est pas dans FICHIER_SIREN
    Quand la base d'étude est constituée
    Alors ce salarié ne doit apparaître ni dans le champ ni dans le stock tous âges

  Scénario: [SPEC-CHAMP-004] Faire dériver toute la chaîne d'un changement d'âge minimum
    Étant donné que AGE_MIN_BTS est modifié de 45 à 44
    Quand la chaîne complète est relancée
    Alors le filtrage de la BTS doit utiliser 44 ans comme borne basse, y compris le filtre Arrow poussé au disque
    Et la table test doit être simulée à partir de 44 ans
    Et les tranches d'âge de restitution doivent commencer à 44 ans
    Et les libellés publics doivent mentionner « 44 ans ou plus »
    Et les contrôles associés doivent utiliser la même borne
    Et aucune règle métier ne doit conserver 45 en dur

  Scénario: [SPEC-CHAMP-005] Refuser une configuration d'âge incohérente
    Étant donné une valeur de AGE_MIN_BTS non entière, négative ou incompatible avec les tranches d'âge
    Quand la configuration est chargée
    Alors la chaîne doit s'arrêter avec un message explicite
    Et en mode test, un AGE_MAX_TEST inférieur ou égal à AGE_MIN_BTS doit être refusé par le script 01

  Scénario: [SPEC-CHAMP-006] Distinguer stock projeté et stock tous âges
    Étant donné que le modèle projette uniquement les salariés du champ
    Quand une part à remplacer est calculée
    Alors le numérateur doit provenir des départs attendus du champ
    Et le dénominateur doit provenir du stock actuel tous âges
    Et ces deux populations ne doivent jamais être confondues ni additionnées

  Scénario: [SPEC-CHAMP-007] Définir un départ
    Étant donné un salarié appartenant au champ
    Quand sa probabilité de sortie est calculée
    Alors les sorties définitives considérées doivent couvrir la retraite ou fin de carrière
    Et l'invalidité
    Et le décès
    Mais une mobilité vers un autre employeur ne doit pas être comptée comme départ

  Scénario: [SPEC-CHAMP-008] Rappeler le champ dans toute restitution publique
    Étant donné une carte, un tableau de bord ou une fiche
    Quand la page est produite
    Alors elle doit rappeler l'âge minimum, le périmètre d'entreprises, les catégories couvertes et les professions exclues
    Et ce rappel doit être construit depuis la configuration réelle, jamais écrit en dur
```

## Critères d'acceptation

- `AGE_MIN_BTS` est une source unique de vérité : le test « même code, trois
  exécutions » relance la chaîne avec 43, 44 et 45 ans et vérifie que seule
  cette ligne de configuration diffère.
- Aucun « 43 », « 44 » ou « 45 » de champ n'est écrit en dur dans le code ;
  les occurrences légitimes (constantes géographiques, années…) sont inscrites
  dans une liste blanche nominative du test.
- Le champ public, les tableaux, les cartes et les fiches reflètent la
  configuration réelle (voir `07_cartes_et_fiches.md`, SPEC-PUB-006).
- Le stock tous âges reste distinct du stock projeté.

## Traçabilité

Code :

- `R/00_config_fonctions/00_config.R` — `AGE_MIN_BTS`, `HORIZON`,
  `ANNEE_REF_GRAPHIQUE`, `FICHIER_SIREN`, `PCS_VERS_CS1`, `PCS_HORS_CHAMP`,
  libellés et tranches dérivés
- `R/01_preparation/01_fabriquer_donnees_test.R` — filtre d'âge, périmètre SIREN
- `R/01_preparation/01b_agreger_pcs.R` — PCS rattachables / hors champ
- `R/01_preparation/01c_stock_tous_ages.R` — stock tous âges
- `R/00_config_fonctions/00f_fonctions_cartes.R` — `construire_libelle_champ()`

Tests :

- `tests/testthat/test-age-min.R` — « même code, trois exécutions : seule la
  ligne AGE_MIN_BTS diffère du dépôt » ; « configuration : 43, 44, 45 acceptés ;
  libellés et tranches dérivés » ; « configuration : valeurs refusées avec un
  message explicite » ; « mode test : AGE_MAX_TEST <= AGE_MIN_BTS refusé par le
  01 lui-même » ; « aucun « 43 » / « 44 » / « 45 » de champ en dur dans le code
  (liste blanche nominative) »
- `tests/testthat/test-cartes-departs.R` — « CHAMP : source unique construite
  depuis la configuration (AGE_MIN_BTS, PCS_VERS_CS1), jamais codée en dur »
- SPEC-CHAMP-003 (SIREN hors périmètre) : couvert indirectement par les
  instantanés (`test-snapshot.R`) ; pas de test unitaire dédié.
