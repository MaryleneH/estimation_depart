# Spec 06 — Restitutions territoriales

## Intention métier

Les résultats territoriaux sont cohérents entre niveaux géographiques et
produits par la même méthode pour les PCS fines et les grandes CS. La France
entière est la référence des contrôles et n'est jamais restreinte.

## Sorties concernées

| Script | Objet | Sorties |
|---|---|---|
| 08 | seniors (`AGE_SENIOR`, 55 ans) par territoire, quadrant de vulnérabilité | `sorties/analyse_55plus_par_<zonage>.csv`, `criticite_55plus_<zonage>_cs.csv`, `quadrant_55plus_<zonage>.png`, tableau 55+ CSV + HTML |
| 08b | territoire × grande CS, part à remplacer | `sorties/departs_par_<zonage>_cs.csv` |
| 08c | PCS fine : France, région, département | `sorties/departs_pcs/{interne,diffusion}/departs_pcs_<niveau>.csv`, `pcs_hors_champ.csv` |
| 08d | grande CS : mêmes niveaux, même fonction | `sorties/departs_cs1/{interne,diffusion}/departs_cs1_<niveau>.csv` |
| 08f | typologie des départements, **en aval du 08d** (spec 10) : aucun départ recalculé | `sorties/typologie_territoriale/{interne,diffusion}/` |

La typologie (spec 10) lit la table département × grande CS exacte du 08d et le
stock tous âges du 01c ; elle n'ajoute aucune mesure de départ. Département et
bassin sont deux mailles distinctes : aucune sortie de cette spec ne descend au
bassin, aucune information de bassin ne qualifie un département (spec 10, §5).

Mesures communes des tables 08c / 08d : `effectif_champ`, `departs_central`,
`departs_bas`, `departs_haut`, `dep_retraite`, `dep_invalidite`, `dep_deces`,
`taux_depart_central_pct`.

```gherkin
Fonctionnalité: Produire des estimations territoriales cohérentes

  Scénario: [SPEC-GEO-001] Produire le niveau France entière sans restriction
    Étant donné la totalité de bts_projete
    Quand les départs par PCS ou grande CS sont calculés au niveau France
    Alors la base doit contenir toutes les lignes de bts_projete
    Et aucune sélection GEO_INTERET ou FICHES_SELECTION ne doit réduire ce total
    Et la France doit toujours être calculée, même si seuls d'autres niveaux sont demandés

  Scénario: [SPEC-GEO-002] Produire les niveaux région et département
    Étant donné une base dont geo_type est "departement"
    Quand les tables territoriales sont produites
    Alors les résultats départementaux doivent être calculés sur geo_code
    Et les résultats régionaux doivent être obtenus par la table de passage département → région
    Et un département absent de la table de passage doit être affecté à "inconnu" et provoquer un arrêt explicite

  Scénario: [SPEC-GEO-003] Ignorer explicitement une maille incompatible
    Étant donné une base dont geo_type n'est pas "departement" (par exemple zone d'emploi)
    Quand les niveaux région ou département sont demandés
    Alors ils ne doivent pas être fabriqués à partir d'une géographie erronée
    Et un message doit indiquer qu'ils sont ignorés
    Et seule la France doit être produite

  Scénario: [SPEC-GEO-004] Contrôler les totaux avant diffusion
    Étant donné une table analytique territoriale
    Quand les contrôles bloquants sont exécutés
    Alors les sept mesures de chaque niveau doivent sommer aux valeurs de la base (tolérance 1e-6)
    Et la somme des régions doit égaler la France
    Et la somme des départements doit égaler la France
    Et la somme des départements d'une région doit égaler cette région
    Et ces contrôles doivent précéder le secret et l'arrondi

  Scénario: [SPEC-GEO-005] Utiliser la même méthode pour PCS et grande CS
    Étant donné une dimension PCS fine et une dimension grande CS
    Quand les départs sont calculés pour chacune
    Alors la même fonction calculer_departs doit servir, seule la dimension changeant
    Et la table grande CS doit être exactement la table PCS regroupée par cs1 (sept mesures)
    Et les mêmes règles de secret doivent s'appliquer

  Scénario: [SPEC-GEO-006] Produire deux versions d'une sortie
    Étant donné une table analytique calculée
    Quand les sorties sont écrites
    Alors interne/ doit toujours être écrit, complet, avec les indicateurs de secret
    Et diffusion/ doit être écrit seulement si DEPARTS_PCS_SECRET (ou DEPARTS_CS1_SECRET) est vrai, avec le secret appliqué

  Scénario: [SPEC-GEO-007] Conserver la PCS fine jusqu'aux résultats
    Étant donné plusieurs PCS d'une même grande CS
    Quand les tables PCS sont produites
    Alors chaque PCS doit rester distincte
    Et le code PCS doit rester en texte

  Scénario: [SPEC-GEO-008] Analyser les seniors par territoire
    Étant donné AGE_SENIOR (55 ans)
    Quand le script 08 est exécuté
    Alors chaque territoire doit avoir son effectif senior, ses départs seniors et leur part
    Et le quadrant doit être construit autour des médianes du périmètre, sans seuil absolu
    Et les tableaux doivent appliquer le secret puis l'arrondi

  Scénario: [SPEC-GEO-009] Calculer la part à remplacer
    Étant donné stock_tous_ages disponible
    Quand le 08b est produit
    Alors part_a_remplacer_pct doit valoir 100 × départs 2030 / effectif tous âges, sur les valeurs exactes
    Et les colonnes tous âges doivent être absentes si le stock n'est pas disponible

  Scénario: [SPEC-GEO-010] Produire les mêmes sorties pour un autre zonage
    Étant donné GEO_ANALYSE fixé à un autre zonage de GEO_ZONAGES (zone d'emploi, région…)
    Quand la chaîne est exécutée
    Alors les fichiers 08 et 08b doivent être suffixés par ce zonage
    Et aucun autre script ne doit être modifié
    Et les résultats du mode zone d'emploi doivent rester identiques à la référence figée

  Scénario: [SPEC-GEO-011] Ne pas altérer la session
    Étant donné les objets de session avant le 08b
    Quand le 08b est sourcé
    Alors aucun objet existant ne doit être modifié, supprimé ni regroupé
    Et les indicateurs calculés doivent être identiques à ceux du 04 et du 08
```

## Traçabilité

Code :

- `R/00_config_fonctions/00e_fonctions_departs_pcs.R` —
  `preparer_base_departs()`, `calculer_departs()`, `controler_departs()`,
  `controler_cs1_vs_pcs()`, `blocs_secret()`, `appliquer_secret_pcs()`,
  `NIVEAUX_DEPARTS_PCS`, `MESURES_DEPARTS_PCS`
- `R/08_territoires/08_analyse_55plus_geo.R`, `08b_departs_geo_cs.R`,
  `08c_departs_pcs.R`, `08d_departs_cs1.R`
- `R/00_config_fonctions/00c_fonctions_geo.R` — `ajouter_region()`,
  `filtrer_geo_interet()`
- `R/00_config_fonctions/00_config.R` — `GEO_ANALYSE`, `GEO_ZONAGES`,
  `GEO_PASSAGES`, `GEO_INTERET`, `DEPARTS_PCS_NIVEAUX`, `DEPARTS_CS1_NIVEAUX`,
  `AGE_SENIOR`

Tests :

- `tests/testthat/test-departs-pcs.R` — « pcs : conservée jusqu'aux résultats,
  plusieurs PCS d'une même cs1 restent distinctes » ; « CONTRÔLE FONDAMENTAL :
  pcs regroupées par cs1 = résultats par grande CS, exactement (7 mesures) » ;
  « cohérence arithmétique (avant arrondi, avant secret) : France, Σ régions,
  Σ départements, Σ départements d'une région » ; « France entière :
  indépendante de GEO_INTERET et de toute sélection de restitution » ;
  « géographie : codes en texte (01, 2A, 2B, DROM), région correcte,
  département inconnu = arrêt » ; « zonage zone d'emploi : région et
  département ignorés avec message, France seule » ; « NON-RÉGRESSION : les 6
  fichiers PCS (interne + diffusion) sont IDENTIQUES aux références figées »
- `tests/testthat/test-departs-cs1.R` — « A. France x cs1 … » ; « B. CONTRÔLE
  FONDAMENTAL PCS -> cs1 … » ; « C/D/E. cohérence géographique par cs1 … » ;
  « G. comparaison 08b … » ; « fichiers : interne/ toujours, diffusion/ selon
  DEPARTS_CS1_SECRET ; 08d indépendant du 08c ; zonage ZE = France seule »
- `tests/testthat/test-departs-geo-cs.R` — « 1. aucun doublon territoire x
  CS ; tri code puis ordre métier des CS » ; « 2. totaux cohérents avec 04
  (p_central), 08 (effectifs par cellule) et 09 (départs par territoire) » ;
  « 3. tous les territoires du périmètre ; 4. aucune CS
  inattendue » ; « périmètre : GEO_INTERET (liste de départements) respecté sans
  duplication du paramétrage » ; « zonage : le même script produit
  departs_par_ze_cs.csv en mode ZE » ; « 8. sourcer 08b ne modifie, ne supprime
  ni ne regroupe aucun objet existant »
- `tests/testthat/test-non-regression-ze.R` — « mode ZE : résultats du 08
  identiques à la référence figée (champ par défaut) »
- `tests/testthat/test-snapshot.R` — instantanés par zonage
