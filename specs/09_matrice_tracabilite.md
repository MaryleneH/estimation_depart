# Matrice de traçabilité des spécifications

Cette matrice relie besoin métier → spécification → code R → test automatique.
Elle ne recopie pas les scénarios : elle sert de point d'entrée pour l'audit et
la maintenance. Les titres de tests sont ceux des `test_that()` du dépôt.

## Vue par domaine

| Domaine | Identifiants | Code principal | Tests principaux |
|---|---|---|---|
| Champ d'étude | SPEC-CHAMP-001 à 008 | `00_config.R`, `01_fabriquer_donnees_test.R`, `01b`, `01c`, `00f` (`construire_libelle_champ`) | `test-age-min.R`, `test-cartes-departs.R` (CHAMP) |
| Préparation | SPEC-PREP-001 à 012 | `01_*`, `02_*`, `00c_fonctions_geo.R` | `test-geo.R`, `test-departs-pcs.R` (passage, hors champ), `test-fiches.R` (stock tous âges) |
| Projection | SPEC-PROJ-001 à 009 | `03_parametres_csp.R`, `04_projection_2030.R`, `00d` (`ajouter_parts_causes`) | `test-snapshot.R`, `test-non-regression-ze.R`, `test-format-restitution.R` (non-régression numérique) |
| Secret statistique | SPEC-SEC-001 à 032 | `00d_fonctions_fiches.R`, `00e_fonctions_departs_pcs.R`, `08b`, `08c`, `08d`, `09` | `test-fiches.R`, `test-departs-geo-cs.R`, `test-departs-pcs.R`, `test-departs-cs1.R`, `test-cartes-departs.R` (SECRET) |
| Restitution / arrondi | SPEC-REST-001 à 010 | `00g_format_restitution.R`, JavaScript de `00f` | `test-format-restitution.R`, `test-fiches.R` (cohérence interne) |
| Restitutions territoriales | SPEC-GEO-001 à 011 | `00e`, `08`, `08b`, `08c`, `08d`, `00c` | `test-departs-pcs.R`, `test-departs-cs1.R`, `test-departs-geo-cs.R`, `test-non-regression-ze.R` |
| Cartes et fiches | SPEC-PUB-001 à 020 | `00f_fonctions_cartes.R`, `08e`, `00d`, `09` | `test-cartes-departs.R`, `test-fiches.R` |
| Orchestration | SPEC-ORCH-001 à 012 | `main.R`, `99b_normaliser_csv_excel.R` | `test-snapshot.R`, `test-normaliser-csv-excel.R`, `test-fiches.R` (chaîne) |

## Vue par exigence (règles structurantes)

| Identifiant | Règle | Fonction / script | Test (titre exact) |
|---|---|---|---|
| SPEC-CHAMP-004 | `AGE_MIN_BTS` source unique | `00_config.R` | `test-age-min.R` « même code, trois exécutions : seule la ligne AGE_MIN_BTS diffère du dépôt » |
| SPEC-CHAMP-005 | âge incohérent refusé | `00_config.R`, `01` | « configuration : valeurs refusées avec un message explicite » ; « mode test : AGE_MAX_TEST <= AGE_MIN_BTS refusé par le 01 lui-même » |
| SPEC-PREP-002 | codes géographiques en texte | `normaliser_geo()` | `test-geo.R` « codes : toujours en texte, « 01 », « 2A », « 2B », « 33 » intacts » |
| SPEC-PREP-006 | PCS hors champ tracées | `01b`, `08c` | `test-departs-pcs.R` « fichiers : … pcs_hors_champ agrégée avec total = exclusions du 01b » |
| SPEC-PREP-008 | stock tous âges indisponible non bloquant | `01c`, `00d` | `test-fiches.R` « STOCK_TOUS_AGES = FALSE : chaîne inchangée, fiches en repli, CSV 08b sans colonnes tous âges » |
| SPEC-PROJ-009 | modèle inchangé par les évolutions de restitution | `04`, références figées | `test-snapshot.R` ; `test-format-restitution.R` « NON-RÉGRESSION numérique … » |
| SPEC-SEC-001 à 008 | règle primaire Insee, bornes | `secret_primaire()`, `indicateurs_secret()` | `test-fiches.R` « secret statistique, règle Insee BTS : < 5 salariés OU < 3 entreprises OU une entreprise > 85 %, puis secondaire » |
| SPEC-SEC-010 à 015 | secondaire : plus petite cellule, itération | `secret_secondaire()`, `appliquer_secret_pcs()` | `test-departs-pcs.R` « secret : règle Insee (primaire) + secondaire minimale sur la diffusion … » |
| SPEC-SEC-016 à 022 | blocs par niveau et dimension | `blocs_secret()` | `test-departs-cs1.R` « H. secret : MÊME implémentation que departs_pcs … » |
| SPEC-SEC-023 à 027 | interne complet / diffusion masquée, contrôle final | `08c`, `08d` | `test-departs-pcs.R` « fichiers : interne/ toujours, diffusion/ seulement avec DEPARTS_PCS_SECRET » |
| SPEC-SEC-031 | aucune valeur masquée dans une page | `preparer_carte_departs()`, `donnees_json_carte()` | `test-cartes-departs.R` « SECRET : aucune valeur masquée dans la structure, le JSON, le HTML ni les infobulles ; table interne refusée » |
| SPEC-REST-001, 002, 005 | demi vers le haut, NA jamais 0 | `arrondir_nombre_personnes()` | `test-format-restitution.R` « arrondir_nombre_personnes : cas limites documentés, demi vers le haut, NA jamais 0 » |
| SPEC-REST-006 | additivité total / composantes | `arrondir_composantes_avec_total()` | « arrondir_composantes_avec_total : la somme affichée des composantes = le total affiché » |
| SPEC-GEO-001 | France jamais restreinte | `preparer_base_departs()` | `test-departs-pcs.R` « France entière : indépendante de GEO_INTERET et de toute sélection de restitution » |
| SPEC-GEO-003 | maille incompatible ignorée avec message | `preparer_base_departs()` | « zonage zone d'emploi : région et département ignorés avec message, France seule » |
| SPEC-GEO-004 | contrôles exacts par niveau | `controler_departs()` | « cohérence arithmétique (avant arrondi, avant secret) : France, Σ régions, Σ départements, Σ départements d'une région » |
| SPEC-GEO-005 | PCS regroupées = grande CS | `controler_cs1_vs_pcs()` | `test-departs-cs1.R` « B. CONTRÔLE FONDAMENTAL PCS -> cs1 … » |
| SPEC-PUB-006 | libellés dérivés de la configuration | `construire_libelles_public()` | `test-cartes-departs.R` « CHAMP : source unique construite depuis la configuration … » |
| SPEC-PUB-009 | frontières lisibles, source unique | `STYLE_CONTOURS_CARTE` | « CONTOURS : source unique STYLE_CONTOURS_CARTE, trait gris (jamais blanc) … » |
| SPEC-PUB-010 | libellé officiel des PCS | `charger_libelles_pcs()` | « LIBELLÉS PCS : nomenclature xlsx lue (codes normalisés) … » |
| SPEC-PUB-011 | liste des PCS par départs décroissants | `preparer_carte_departs()` | « ORDRE des PCS : départs estimés diffusés décroissants … » |
| SPEC-PUB-016 | « À retenir » factuel | `00d` | `test-fiches.R` « règles textuelles : position, « À retenir » en 2-3 phrases factuelles, sans répéter le chiffre clé » |
| SPEC-ORCH-009 | BOM sans changement de contenu | `ajouter_bom_utf8_csv()` | `test-normaliser-csv-excel.R` (A à H, non-régression) |

## Règles sans test automatique dédié (dette identifiée)

| Identifiant | Règle | Garantie actuelle |
|---|---|---|
| SPEC-CHAMP-003 | SIREN hors périmètre exclu partout | instantanés figés |
| SPEC-PREP-009, 010 | sources externes et replis documentés | messages forts dans 02b / 02c ; exécution avec les fichiers livrés |
| SPEC-PROJ-003, 004 | ordre bas ≤ central ≤ haut ; Σ parts = p_central par individu | contrôles agrégés 08c / 08d (7 mesures) |
| SPEC-PUB-012 | bornes de classes rondes | logique JavaScript, contrôle visuel |
| SPEC-ORCH-005, 007, 008 | gt facultatif, PNG non bloquant, hors ligne | construction du code (`requireNamespace`, `tryCatch`, fond local) |

Ajouter un test pour l'une de ces lignes est une évolution bienvenue ; elle
doit citer l'identifiant.

## Convention recommandée dans les tests

Lorsqu'un test matérialise directement une règle fonctionnelle, son intitulé
peut reprendre l'identifiant :

```r
test_that("SPEC-SEC-001 — moins de 5 salariés => secret primaire", {
  ...
})
```

Pour une règle vérifiée dans plusieurs tests, un commentaire suffit :

```r
# SPEC-SEC-015 — secret secondaire itéré jusqu'à stabilité
```

Recherche de toutes les vérifications d'une exigence :

```bash
grep -R "SPEC-SEC-015" tests/
```

Les tests existants n'ont pas été renommés : la matrice ci-dessus fait le lien
par leur titre actuel. La convention s'applique aux tests créés ou modifiés à
partir de maintenant.

## Doctrine de modification

Lorsqu'une règle métier évolue :

1. modifier d'abord la spécification concernée (ou en créer une, avec un nouvel
   identifiant, jamais réattribué) ;
2. identifier les tests associés dans cette matrice ;
3. modifier ou ajouter les tests ;
4. seulement ensuite modifier le code ;
5. vérifier la non-régression des autres `SPEC-*` : suite complète, instantanés,
   comparaison des sorties ;
6. si des références figées changent, les refiger dans le même changement en
   expliquant pourquoi.

Ainsi, une modification de seuil ou de doctrine n'est plus une simple
modification technique : elle devient une évolution explicite du contrat
fonctionnel. Les paramètres du modèle statistique (spec 03, SPEC-PROJ-009) ne
changent que par une décision métier tracée de cette façon.
