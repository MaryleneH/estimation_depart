# Spec 03 — Projection des départs à l'horizon 2030

## Intention métier

Le modèle calcule, pour chaque salarié du champ, des probabilités de sortie
définitive de l'emploi d'ici l'horizon, puis les agrège par cellule. Les causes
restent interprétables (retraite ou fin de carrière, invalidité, décès) et
l'incertitude est encadrée par une fourchette. Les calculs conservent leur
précision complète : aucun arrondi n'intervient avant la restitution.

## Construction du modèle (ce que le métier doit savoir)

Deux niveaux, au choix par `SCENARIO` (valeur courante « B ») ; les deux sont
toujours calculés pour le tableau de contribution « en pelures » :

- **A — législation seule** : départ par le seul âge conjoncturel de
  liquidation `age_conj` (DREES, lissé sur `ANNEES_LISSAGE`), sans sas ni
  événement de vie.
- **B — A + événements de vie** : le sas fait passer de l'âge de liquidation à
  l'âge de sortie de l'emploi `mu_sortie` (corrigé de la part d'invalidité pour
  éviter un double compte), puis l'invalidité (flux par CSP × âge, EACR) et le
  décès (quotients Insee) sont combinés : `p = 1 − (1 − p_cal) × (1 − p_frott)`.

Le correctif réglementaire δ (ancré sur `AGE_CONJ_TOUS_REGIMES_2023` = 63,6 ans
et `MONTEE_RESIDUELLE_2030`) décale les âges de sortie ; ses trois valeurs
(δ bas, central, haut) donnent les trois estimations. La dispersion des âges de
sortie est `SIGMA_SORTIE` (2,5 ans).

```gherkin
Fonctionnalité: Estimer les départs attendus d'ici 2030

  Contexte:
    Étant donné un salarié appartenant au champ
    Et ses caractéristiques utiles au modèle : âge en 2024, sexe, grande CS
    Et les paramètres de sortie de sa CSP (age_conj, mu_sortie)

  Scénario: [SPEC-PROJ-001] Projeter l'âge à l'horizon
    Étant donné un salarié âgé de age_2024 en 2024
    Quand la projection est réalisée
    Alors son âge projeté age_2030 doit valoir age_2024 + HORIZON

  Scénario: [SPEC-PROJ-002] Produire une estimation centrale bornée
    Étant donné les paramètres applicables au salarié
    Quand la projection est calculée
    Alors p_central doit exister et être compris entre 0 et 1
    Et aucun p_central ne doit être NA ; un paramètre non apparié doit arrêter la chaîne avec le détail des cas

  Scénario: [SPEC-PROJ-003] Produire une fourchette ordonnée
    Étant donné les trois valeurs du correctif δ
    Quand p_bas, p_central et p_haut sont calculés
    Alors p_bas ne doit pas dépasser p_central
    Et p_central ne doit pas dépasser p_haut
    Et un δ plus élevé doit retarder les sorties, donc baisser les départs

  Scénario: [SPEC-PROJ-004] Ventiler les causes sans double compte
    Étant donné la probabilité de sortie p_central d'un salarié
    Quand la décomposition par cause est construite (risques concurrents)
    Alors la part retraite ou fin de carrière, la part invalidité et la part décès doivent être identifiables
    Et leur somme exacte doit être égale à p_central
    Et la part d'invalidité déjà contenue dans la durée hors emploi DREES ne doit pas être comptée deux fois

  Scénario: [SPEC-PROJ-005] Conserver les deux scénarios pour la lecture en pelures
    Étant donné SCENARIO fixé à "B"
    Quand la projection est calculée
    Alors les probabilités du scénario A doivent aussi être disponibles
    Et le tableau de contribution doit montrer l'effet de chaque niveau ajouté

  Scénario: [SPEC-PROJ-006] Agréger des probabilités individuelles
    Étant donné plusieurs salariés d'une même cellule
    Quand le nombre attendu de départs est calculé
    Alors il doit être la somme des probabilités individuelles de la cellule
    Et il doit conserver sa précision complète jusqu'à la restitution

  Scénario: [SPEC-PROJ-007] Contrôler sur les valeurs exactes
    Étant donné des valeurs non arrondies
    Quand les contrôles de cohérence sont réalisés (totaux, Σ régions = France, PCS regroupées = grande CS)
    Alors ils doivent porter sur les valeurs exactes avec une tolérance numérique
    Et aucun arrondi de présentation ne doit intervenir avant eux

  Scénario: [SPEC-PROJ-008] Qualifier un départ pour les graphiques
    Étant donné p_central
    Quand une classe de lecture est attribuée
    Alors p ≥ SEUIL_CERTAIN (0,75) doit être lu « départ certain d'ici 2030 »
    Et SEUIL_PROBABLE (0,25) ≤ p < SEUIL_CERTAIN « départ probable ou envisageable »
    Et ces seuils ne servent qu'à la lecture, jamais au calcul des nombres attendus

  Scénario: [SPEC-PROJ-009] Modifier le modèle uniquement par décision explicite
    Étant donné une évolution de restitution, de secret ou d'arrondi
    Quand le code est modifié
    Alors aucune probabilité ni aucun paramètre du modèle ne doit changer
    Et les instantanés de référence doivent rester identiques à la tolérance près
```

## Traçabilité

Code :

- `R/03_modelisation/03_parametres_csp.R` — `param_cs`, `param_ensemble`
- `R/03_modelisation/04_projection_2030.R` — `p_sortie_horizon()`,
  `retrait_mu_invalidite()`, `p_invalidite_h()`, `p_deces_h()`,
  `frottement_h()`, objet `bts_projete`, tableau `contribution`
- `R/00_config_fonctions/00d_fonctions_fiches.R` — `ajouter_parts_causes()`
  (risques concurrents : `part_ret + part_inv + part_dec = p_central`)
- `R/00_config_fonctions/00_config.R` — `SCENARIO`, `HORIZON`, `SIGMA_SORTIE`,
  `AGE_CONJ_TOUS_REGIMES_2023`, `MONTEE_RESIDUELLE_2030`, `COEF_CSP_INVALIDITE`,
  `AGE_PLEIN_INVALIDITE`, `SEUIL_CERTAIN`, `SEUIL_PROBABLE`

Tests :

- `tests/testthat/test-snapshot.R` — « instantané <zonage> : aucun écart avec
  la référence figée » (probabilités et effectifs figés, tolérance 1e-9)
- `tests/testthat/test-non-regression-ze.R` — « mode ZE : résultats du 08
  identiques à la référence figée (champ par défaut) »
- `tests/testthat/test-format-restitution.R` — « NON-RÉGRESSION numérique : la
  chaîne produit des objets analytiques EXACTS, identiques avant/après
  restitution »
- `tests/testthat/test-departs-geo-cs.R` — « 2. totaux cohérents avec 04
  (p_central), 08 (effectifs par cellule) et 09 (départs par territoire) »
- `tests/testthat/test-geo.R` — « 04 : un sexe non apparié produit un arrêt
  explicite, pas des p_central NA »
- SPEC-PROJ-003 (ordre bas ≤ central ≤ haut) et SPEC-PROJ-004 (somme des parts
  = p_central) : vérifiés par les contrôles de 08c/08d sur les 7 mesures, pas
  de test unitaire dédié sur les probabilités individuelles.
