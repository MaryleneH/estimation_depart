# Organisation du code R

Le point d’entrée reste `main.R` à la racine : `source("main.R")` lance toute la chaîne. Les scripts communiquent par les objets de la session, pas par des fichiers intermédiaires.

Règle de lecture : **le préfixe d’un répertoire est le numéro du premier script qu’il contient**, donc l’ordre alphabétique des répertoires est l’ordre d’exécution. Les noms de scripts, avec leur numéro, n’ont pas changé lors de la mise en sous-répertoires.

| Répertoire | Rôle | Scripts (dans l’ordre d’exécution) |
|---|---|---|
| `00_config_fonctions/` | Paramètres et fonctions pures, rien n’est calculé ici | `00_config.R` (tous les réglages), `00b_fonts.R`, `00c_fonctions_geo.R` (contrat géographique), `00d_fonctions_fiches.R` (fiches), `00e_fonctions_departs_pcs.R` (départs par PCS fine et par grande CS : une fonction, deux dimensions), `00f_fonctions_cartes.R` (cartes des résultats diffusables : fond local, projection, HTML autonome, PNG) |
| `01_preparation/` | Lecture de la BTS et des sources externes, recodages | `01_fabriquer_donnees_test.R` (BTS parquet ou table test, géographie), `01b_agreger_pcs.R` (PCS → grande CS `cs1`, PCS hors champ tracées), `01c_stock_tous_ages.R` (effectifs tous âges), `02_importer_nettoyer_drees.R`, `02b_importer_mortalite_insee.R`, `02c_importer_invalidite_eacr.R` |
| `03_modelisation/` | Le modèle : paramètres par CS puis probabilités individuelles de départ | `03_parametres_csp.R`, `04_projection_2030.R` (→ `bts_projete`) |
| `05_restitutions/` | Résultats nationaux : entreprises, graphiques, tableaux de contribution | `05_resultats_entreprises.R`, `06_graphique_repartition.R`, `06b_graphique_repartition_cs.R`, `06c_graphique_sas_retraite_cs.R`, `06d_graphique_repartition_cs_regroupe.R`, `06e_graphique_repartition_age_regroupe.R`, `07_tableau_contribution.R`, `07b_tableau_contibution_sans_gt.R`, `07c_tableau_contribution_55plus_sans_gt.R` |
| `08_territoires/` | Analyses et fiches par territoire, départs par PCS fine | `08_analyse_55plus_geo.R` (55 ans et + par territoire, quadrant), `08b_departs_geo_cs.R` (territoire × grande CS), `08c_departs_pcs.R` (PCS fine : France, région, département), `08d_departs_cs1.R` (grande CS `cs1` : mêmes mailles, même fonction, même secret que 08c), `08e_cartes_departs.R` (cartes HTML des six tables de diffusion), `09_fiches_territoriales.R` (fiches chiffres clés) |
| `99_controles/` | Contrôles de la chaîne et finalisation technique des sorties | `99_controle.R` (tableaux de contrôle, à lancer à part), `99b_normaliser_csv_excel.R` (fin de `main.R`) |

`99b_normaliser_csv_excel.R` : ajoute un BOM UTF-8 aux CSV produits dans `sorties/` afin de garantir une ouverture correcte dans Excel Windows, sans modifier leur contenu. Pour relire ces CSV dans R : `read.csv2(f, fileEncoding = "UTF-8-BOM")`.

Ordre d’exécution de `main.R` : 00 → 01, 01b, 01c → 02, 02b, 02c → 03 → 04 → 05 → 06, 06b, 06d, 06e → 07 (si `gt` est installé) → 08 → 08b → 08c → 08d → 08e (si `GENERER_CARTES_DEPARTS`) → 09 → 99b (si `NORMALISER_CSV_EXCEL`).

## Cartographie des départs

`08e_cartes_departs.R` lit uniquement les six tables de **diffusion** (`sorties/departs_pcs/diffusion/`, `sorties/departs_cs1/diffusion/`) et écrit `sorties/cartes_departs/{pcs,cs1}/carte_<dimension>_<niveau>.html` (+ `.png` de la vue initiale) pour France entière, région et département. Chaque carte est une page HTML autonome (SVG inline, JavaScript natif, aucune ressource externe) avec un sélecteur de PCS ou de grande CS, le choix de l’indicateur (taux de départ par défaut, départs estimés central / bas / haut, effectif du champ) et une vue « couverture de diffusion » (part des catégories diffusables par territoire). **Gris = non diffusé, secret statistique** ; **blanc pointillé = pas de donnée observée** ; une cellule masquée n’embarque que son statut, jamais sa valeur. Fond local `data/cartographie/` (IGN Admin Express via france-geojson, voir `data/LISEZMOI.txt`), métropole en Lambert-93, DROM en encarts. Relançable seul après la chaîne : `source("R/08_territoires/08e_cartes_departs.R")`. Les scripts 06c, 07b, 07c et 99 se lancent à part, après `main.R`, dans la même session.

Où régler quoi : tout est dans `00_config_fonctions/00_config.R` (source, champ `AGE_MIN_BTS`, secret statistique `SECRET_*` selon la règle Insee de la Base Tous salariés : moins de 5 salariés, moins de 3 entreprises ou une entreprise à plus de 85 % d’une cellule, puis secret secondaire, géographie `GEO_*`, fiches, départs par PCS). Les fonctions de secret sont dans `00d_fonctions_fiches.R` et servent à 08, 08b, 08c et 09. Les utilitaires ponctuels sont dans `utils/`, les tests dans `tests/testthat/`, qui retrouvent chaque script par son nom.
