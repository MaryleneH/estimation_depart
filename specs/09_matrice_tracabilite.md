# Matrice de traçabilité des spécifications

Cette matrice relie besoin métier → spécification → code R → test automatique.
Elle ne recopie pas les scénarios : elle sert de point d'entrée pour l'audit et
la maintenance. Les titres de tests sont ceux des `test_that()` du dépôt.

## Vue par domaine

| Domaine | Identifiants | Code principal | Tests principaux |
|---|---|---|---|
| Champ d'étude | SPEC-CHAMP-001 à 008 | `00_config.R`, `01_fabriquer_donnees_test.R`, `01b`, `01c`, `00f` (`construire_libelle_champ`) | `test-age-min.R`, `test-cartes-departs.R` (CHAMP) |
| Préparation | SPEC-PREP-001 à 014 | `01_*`, `02_*`, `00c_fonctions_geo.R` | `test-geo.R`, `test-departs-pcs.R` (passage, hors champ), `test-fiches.R` (stock tous âges) |
| Projection | SPEC-PROJ-001 à 009 | `03_parametres_csp.R`, `04_projection_2030.R`, `00d` (`ajouter_parts_causes`) | `test-snapshot.R`, `test-non-regression-ze.R`, `test-format-restitution.R` (non-régression numérique) |
| Secret statistique | SPEC-SEC-001 à 034 | `00d_fonctions_fiches.R`, `00e_fonctions_departs_pcs.R`, `08b`, `08c`, `08d`, `09` | `test-fiches.R`, `test-departs-geo-cs.R`, `test-departs-pcs.R`, `test-departs-cs1.R`, `test-cartes-departs.R` (SECRET) |
| Restitution / arrondi | SPEC-REST-001 à 010 | `00g_format_restitution.R`, JavaScript de `00f` | `test-format-restitution.R`, `test-fiches.R` (cohérence interne) |
| Restitutions territoriales | SPEC-GEO-001 à 011 | `00e`, `08`, `08b`, `08c`, `08d`, `00c` | `test-departs-pcs.R`, `test-departs-cs1.R`, `test-departs-geo-cs.R`, `test-non-regression-ze.R` |
| Cartes et fiches | SPEC-PUB-001 à 020 | `00f_fonctions_cartes.R`, `08e`, `00d`, `09` | `test-cartes-departs.R`, `test-fiches.R` |
| Orchestration | SPEC-ORCH-001 à 012 | `main.R`, `99b_normaliser_csv_excel.R` | `test-snapshot.R`, `test-normaliser-csv-excel.R`, `test-fiches.R` (chaîne) |
| Typologie territoriale | SPEC-TYPO-001 à 107 | `00h_fonctions_typologie.R`, `08f_typologie_departements.R`, `01c` (pyramide), `00k_fonctions_tension.R`, `08i_exposition_tension.R`, `00_config.R` (TYPO_*, AGE_BORNE_JEUNES, TENSION_*) | `test-typologie-territoriale.R`, `test-typologie-pyramide.R`, `test-exposition-tension.R` |

## Typologie territoriale (spec 10) : besoin → règle → implémentation → test → sortie

| Identifiant | Règle | Fonction / script | Test (titre exact) | Sortie |
|---|---|---|---|---|
| SPEC-TYPO-001 | département seul ; ignoré hors zonage département | `construire_typologie_departements()`, `08f` | « SPEC-TYPO-001/002/004 — niveau département seul… » ; « SPEC-TYPO-040 — chaîne réelle… » (mode ZE) | `interne/typologie_departements.csv` |
| SPEC-TYPO-002 | réutilisation des mesures du 08d, base du poids | `construire_typologie_departements()` | « SPEC-TYPO-001/002/004 … » | `parametres_typologie.csv` (`base_poids`) |
| SPEC-TYPO-003 | dominance / Mixte, borne, égalité | `identifier_dominance_cs()` | « SPEC-TYPO-003 — structure : 31/30/25/14 … » | `structure_emploi`, `cs_principale` |
| SPEC-TYPO-004 | fourchette conservée, volume ≠ taux | `construire_typologie_departements()` | « SPEC-TYPO-001/002/004 … » | `departs_bas/central/haut`, `classe_volume_departs` |
| SPEC-TYPO-005 | intensité : définition et repli | `construire_typologie_departements()` | « SPEC-TYPO-001/002/004 … » (sans stock) ; « SPEC-TYPO-005/006 … » | `intensite_renouvellement_pct`, `indicateur_intensite` |
| SPEC-TYPO-006 | volume ≠ intensité (A 1 000 / 20 000, B 500 / 2 000) | `construire_typologie_departements()` | « SPEC-TYPO-005/006 — volume ≠ intensité … » | idem |
| SPEC-TYPO-007 | CS volume max ≠ CS intensité max ; rangs | `identifier_cs_volume_max()`, `identifier_cs_taux_max()` | « SPEC-TYPO-007 — la CS au plus gros volume … » | `cs_volume_departs_max`, `cs_taux_renouvellement_max`, `typologie_departement_cs.csv` |
| SPEC-TYPO-008 / 009 | concentré (60 % / 50) ; diffus ; borne | `calculer_concentration_departs()` | « SPEC-TYPO-008/009 — concentration … » | `type_concentration` |
| SPEC-TYPO-010 | classes à trois niveaux, borne incluse en haut | `classer_par_seuils()` | « SPEC-TYPO-010/011/012 — classes et frontières … » | `classe_*` |
| SPEC-TYPO-011 | plancher d'effectif | `classer_poids_base()` | idem | `classe_poids_base` |
| SPEC-TYPO-012 | terciles = règle provisoire, bornes écrites | `seuils_classes()` | idem ; « SPEC-TYPO-040 … » | `parametres_typologie.csv` |
| SPEC-TYPO-013 à 017, 019 | profils ordonnés à titres parlants, faible poids ≠ pôle majeur, situation à expertiser avec motif, pôle majeur au renouvellement modéré, frontière sur les trois axes seulement | `attribuer_profil_typologie()` | « SPEC-TYPO-013 à 019 — profils … » ; « SPEC-TYPO-014 — faible poids … » | `profil_typologie`, `motif_expertise` |
| SPEC-TYPO-018 | justification par règles | `construire_justification_profil()` | « SPEC-TYPO-013 à 019 … » | `justification_profil` |
| SPEC-TYPO-050 / 051 / 052 | situation rédigée par règles ; motif d'expertise ; points d'attention | `rediger_situation_departement()`, `points_attention_typologie()` | « SPEC-TYPO-050 à 053 — rédaction … » | `situation_texte`, `motif_expertise`, `points_attention` |
| SPEC-TYPO-053 | synthèse rédigée par profil, définitions publiées, motifs en console | `rediger_synthese_profils()`, `profils_definitions()`, `08f` | « SPEC-TYPO-050 à 053 … » ; « SPEC-TYPO-040 … » | `synthese_profils.csv` (`texte`), `profils_definitions.csv` |
| SPEC-TYPO-054 | textes et motifs jamais révélateurs (territoire masqué, cellule CS masquée, profil non diffusé) | `appliquer_secret_typologie()` | « SPEC-TYPO-030 à 034 … » ; « SPEC-TYPO-054 — secret des textes … » | `diffusion/typologie_departements.csv` |
| SPEC-TYPO-060 | carte alimentée par la seule diffusion ; interne refusée ; code sans géométrie = arrêt ; profil inconnu = arrêt ; fichier absent = message | `lire_typologie_diffusion()`, `preparer_carte_typologie()`, `08g` | `test-carte-typologie.R` « SPEC-TYPO-060 — source … » ; « SPEC-TYPO-060/061/063 — 08g sur la chaîne … » | `cartes/` |
| SPEC-TYPO-061 | couleur par profil, gris pour le secret (département absent de la table compris), légende avec comptes et significations, lisible sans script | `generer_carte_typologie()`, `html_legende_typologie()` | « SPEC-TYPO-061/062 — page interactive … » | `cartes/carte_typologie.html` |
| SPEC-TYPO-062 | survol = infobulle en rubriques + panneau en rubriques et puces (`blocs_detail_typologie()`) ; clic = épingler ; légende cliquable | `blocs_detail_typologie()`, `donnees_json_typologie()`, `js_carte_typologie()` | idem | idem |
| SPEC-TYPO-063 | version courriel sans aucun contenu actif, `<title>` natifs, tableau et définitions ouverts | `generer_carte_typologie_courriel()` | « SPEC-TYPO-063/064 — version courriel … » | `cartes/carte_typologie_courriel.html` |
| SPEC-TYPO-064 | rien d'un département masqué ; profil non diffusé sans catégorie | `preparer_carte_typologie()`, `controler_carte_typologie()` | idem | les deux pages |
| SPEC-TYPO-080 / 081 | top des PCS diffusables (classement, N lignes, compte des masquées) ; garde-fou de différenciation (une seule cellule masquée = top retenu) | `preparer_top_pcs()` | `test-cartes-typologie-pcs.R` « SPEC-TYPO-080/081 … » | `cartes/carte_typologie_pcs.html` |
| SPEC-TYPO-082 | région = profil le plus fréquent de ses départements, comptes, égalité ; clic = niveau département | `preparer_regions_typologie()`, `js_cartes_typologie_pcs()` | « SPEC-TYPO-082/083 … » | idem |
| SPEC-TYPO-083 | panneau département (profil, rubriques et puces, top en barres, spécificité face à la France), coloration par PCS | `donnees_json_typo_pcs()`, `js_cartes_typologie_pcs()` | idem | idem |
| SPEC-TYPO-084 | version courriel sans contenu actif, deux cartes, blocs dépliables natifs | `generer_cartes_typologie_pcs_courriel()` | « SPEC-TYPO-084 … » | `cartes/carte_typologie_pcs_courriel.html` |
| SPEC-TYPO-085 | 08h sur la chaîne et seul ; table absente = message | `08h_cartes_typologie_pcs.R` | « SPEC-TYPO-085 … » | les deux pages |
| SPEC-TYPO-070 | bornes effectives en console (99c) : cinq indicateurs, méthode, plancher, repli d'intensité, répartition, fichiers absents | `99c_afficher_bornes_typologie.R` | `test-afficher-bornes-typologie.R` (quatre blocs) | console |
| SPEC-TYPO-090 | tranches de la pyramide tirées de AGE_BORNE_JEUNES, AGE_MIN_BTS, AGE_SENIOR ; borne des jeunes contrôlée | `tranches_stock_ages()`, `affecter_tranche_stock()`, `00_config.R` | `test-typologie-pyramide.R` « SPEC-TYPO-090 — tranches … » | `pyramide_libelles` |
| SPEC-TYPO-091 | parts par tranche, relève (jeunes / seniors), vague suivante ; aucun senior = NA | `preparer_pyramide_departements()`, `indicateurs_pyramide()` | « SPEC-TYPO-091 — pyramide complète … » | `pyramide_<clé>_pct`, `ratio_releve`, `ratio_vague_seniors` |
| SPEC-TYPO-092 | repli sur le champ (`bts_projete`) ; sans source, rien de créé | `preparer_pyramide_departements()`, `phrase_pyramide()`, `08f` | « SPEC-TYPO-092 — repli sur le champ … » | `pyramide_source` |
| SPEC-TYPO-093 | classes de relève et de vague suivante, bornes écrites ; profils inchangés | `construire_typologie_departements()` | « SPEC-TYPO-093 — classes … » | `classe_releve`, `classe_vague_suivante`, `parametres_typologie.csv`, 99c (bloc 6) |
| SPEC-TYPO-094 | points d'attention relève fragile / vague après 2030 ; phrase de relève unique (« 1,4 salarié de moins de 30 ans pour 1 salarié de 55 ans et plus ») | `phrase_releve()`, `points_attention_typologie()`, `phrase_pyramide()`, `ligne_pyramide_typologie()` | « SPEC-TYPO-094 — points d'attention … » | `points_attention`, `situation_texte` |
| SPEC-TYPO-095 | secret des tranches (bloc département), pyramide entière masquée, effectifs par tranche internes ; cartes muettes | `appliquer_secret_typologie()`, `ligne_pyramide_typologie()`, `html_barres_pyramide()`, `js_cartes_typologie_pcs()` | « SPEC-TYPO-095 / SPEC-SEC-033 — secret … » ; « SPEC-TYPO-095 — cartes … » | `diffusion/typologie_departements.csv` (`pyramide_masquee`), cartes 08g / 08h, `interne/pyramide_ages_departement_cs.csv` |
| SPEC-TYPO-100 | lecture contrôlée du classeur PCS → FAP (feuille, colonnes, doublons, rangs avec sous-niveaux 5.1 / 5.2 ramenés à une décimale, colonne interdite, rang manquant) ; composante dominante, levier, faible confiance | `lire_pcs_fap_tension()`, `tension_metiers()` | `test-exposition-tension.R` « SPEC-TYPO-100 — lecture contrôlée … » ; « SPEC-TYPO-100 — composante dominante … » | `interne/metiers_strategiques_tension.csv` |
| SPEC-TYPO-101 / 102 | tension propriété nationale du métier ; exposition = A / (A + B), couverture, repère France, couverture insuffisante | `exposition_departements()`, `rediger_exposition_departement()` | « SPEC-TYPO-101 / 102 — exposition … » | `exposition_tension_departements.csv` |
| SPEC-TYPO-103 | classes sur les mêmes bornes, robustesse (hypothèse de départ, seuil de tension, rattachement PCS → FAP) | `exposition_departements()` | « SPEC-TYPO-103 — classes … » | `parametres_exposition_tension.csv` |
| SPEC-TYPO-104 / 105 | quatre situations à libellés factuels, cellule masquée, PCS absente ; cumul renouvellement rapide et exposition forte, trois messages ; fiche sans script : tableau des métiers par situation, matrice SVG en annexe repliée (libellés au survol, aucun métier non diffusable) | `matrice_metiers_tension()`, `cumul_renouvellement_tension()`, `messages_fiche_tension()`, `html_tableau_metiers_tension()`, `png_matrice_metiers_tension()`, `svg_matrice_metiers_tension()`, `generer_fiche_exposition_tension()` | « SPEC-TYPO-104 / 105 — matrice nationale … » | `matrice_metiers_tension.png`, `fiche_exposition_tension.html` |
| SPEC-TYPO-106 | secret tout ou rien, part sous le seuil, différenciation, département masqué, aucun effectif | `appliquer_secret_exposition()`, `controler_parts_secret()` | « SPEC-TYPO-106 / SPEC-SEC-034 — secret … » | `diffusion/exposition_tension_departements.csv` |
| SPEC-TYPO-107 | hachures (exposition forte, jamais un département masqué), légende, ligne et phrase du panneau, courriel sans script ; 08i sur la chaîne ; classeur absent = message | `svg_hachures_typo()`, `html_legende_hachures()`, `ligne_tension_typologie()`, `lire_typologie_diffusion()`, `08i` | « SPEC-TYPO-107 — carte des profils … » ; « SPEC-TYPO-107 — 08i sur la chaîne … » | cartes 08g |
| SPEC-TYPO-108 | classeur Excel : quatre feuilles (Lisez-moi, Départements, Métiers, Paramètres) issues des tables de diffusion, aucun effectif, non diffusés sans valeur, mise en forme openxlsx, repli writexl, désactivable ; échec = message | `tables_classeur_tension()`, `ecrire_classeur_tension()`, `08i` | « SPEC-TYPO-108 — classeur Excel … » ; « SPEC-TYPO-107 — 08i sur la chaîne … » | `diffusion/exposition_tension.xlsx` |
| SPEC-TYPO-109 | métiers de la liste territoire par territoire (08j) : table longue métier × territoire issue des seules tables de diffusion du 08c, lecture de l'écart à la France au seuil `TENSION_ECART_POINTS`, page sans script (bloc par métier, vue par territoire), classeur (table longue, vue par territoire), contrôle bloquant cellule visible = diffusion 08c, absence = message ; ancienne `fiche_territoires_tension.html` retirée par le 08i si elle subsiste ; libellé public `TENSION_LIBELLE_LISTE` via `texte_liste()` ; contrôle 99d | `table_metiers_territoires()`, `lecture_ecart_liste()`, `generer_fiche_metiers_liste()`, `tables_classeur_metiers_liste()`, `texte_liste()`, `08j_metiers_liste_territoires.R`, `99d_controler_tension_cartes.R` | `test-metiers-liste.R` ; `test-exposition-tension.R` « SPEC-TYPO-107 — 08i sur la chaîne … » ; `test-controler-tension.R` | `diffusion/metiers_liste_territoires.{csv,xlsx}`, `diffusion/fiche_metiers_liste.html` |
| SPEC-TYPO-110 | lecture en un écran : grille métiers (ordre de la liste) × régions (France en repère), couleur par lecture, non diffusé hachuré sans valeur, title sans script ; panneau de détail au clic par gabarits fixes (phrase de lecture, cellule, départements de la région classés, carte d'identité du métier, métiers d'une région) ; page autonome (JS natif, données embarquées sans valeur masquée, sans on*, sans lien) ; libellé public | `generer_tableau_metiers_liste()`, `donnees_json_metiers_liste()`, `08j_metiers_liste_territoires.R` | `test-metiers-liste.R` « SPEC-TYPO-110 … » ; `test-exposition-tension.R` « SPEC-TYPO-107 — 08i sur la chaîne … » | `diffusion/tableau_metiers_liste.html` |
| SPEC-ANA-001 … 008 | analyse « hypothèses de départ » (specs/11) : classeur Excel prêt pour Word, une feuille par tableau (hypothèses lues dans la config, volume total / âge / seniors / catégorie, métiers volume / part / liste / noyau dur, régions / départements enrichis de la typologie et de l'exposition / liste x régions / seniors par territoire, priorités avec tension, dimension principale et levier), feuille « Définitions » et intitulés explicites, seules tables de diffusion, secret par cellule, libellé public, jamais d'arrêt | `R/09_analyse/09a_fonctions_analyse_hypotheses.R` (toutes les fonctions `table_*_analyse`, `table_definitions_analyse()`, `ecrire_classeur_analyse()`, `controler_tables_analyse()`), `R/09_analyse/09b_analyse_hypotheses_depart.R` | `test-analyse-hypotheses.R` ; `test-exposition-tension.R` « SPEC-TYPO-107 — 08i sur la chaîne … » | `analyse_hypotheses_depart/<zonage>/analyse_hypotheses_depart.xlsx`, `*.csv` |
| SPEC-ANA-009 | deux cartes épurées pour la note sur la typologie : profils (une couleur par profil, légende avec comptes) et intensité du renouvellement (trois classes, pôles majeurs cernés) ; non diffusé blanc cerné de pointillés ; jamais d'arrêt ; relançable seul | `R/09_analyse/09c_cartes_typologie_note.R` (`preparer_cartes_note()`, `dessiner_carte_note()`, `cartes_note_typologie()`) | `test-cartes-note.R` ; `test-exposition-tension.R` « SPEC-TYPO-107 — 08i sur la chaîne … » | `analyse_hypotheses_depart/cartes/carte_{profils,intensite}_note.png` |
| SPEC-TYPO-065 | premier écran tenant dans la fenêtre (en-tête compact, carte entière, panneau à défilement interne, légende compacte ; écran étroit : carte au-dessus, défilement vers le panneau) ; survol et défilement stables (infobulle fixe, contenu reconstruit au changement seul, panneau à hauteur fixe, masquage au défilement) ; légende sans entrée « absent » à zéro | `css_carte_typologie()`, `js_carte_typologie()`, `generer_carte_typologie()`, `html_entete_typologie()`, `html_legende_typologie()`, `generer_cartes_typologie_pcs()`, `png_carte_typologie()` | « SPEC-TYPO-065 — stabilité du survol et légende … » ; « SPEC-TYPO-065 — premier écran … » ; test navigateur (Playwright, hors suite, contrôle « tout dans l'écran ») | `cartes/carte_typologie.html` |
| SPEC-TYPO-020 à 023 | tension de bassin jamais attribuée au département, jamais de départs au bassin, pas de FAP → CS, nommage | `joindre_signal_tension_localise()`, `controler_typologie()` | « SPEC-TYPO-020 à 023 — tensions … » | `nb_bassins_signales`, `presence_signal_tension_localise` (non produits par défaut) |
| SPEC-TYPO-030 | département non diffusable, secondaire sur le bloc national | `appliquer_secret_typologie()` | « SPEC-TYPO-030 à 034 — secret … » | `diffusion/typologie_departements.csv` (`masque`, `motif_masque`) |
| SPEC-TYPO-031 | colonnes par CS suivent le masque de la cellule | idem | idem | idem |
| SPEC-TYPO-032 / 033 | variables dérivées et profils dépendants masqués | idem | idem | idem |
| SPEC-TYPO-034 | interne complet, indicateurs techniques hors diffusion, `.gitignore` | `08f`, `.gitignore` | idem ; « SPEC-TYPO-040 … » | `interne/`, `diffusion/` |
| SPEC-TYPO-040 | sorties, arrondi, matrice, objets amont inchangés | `08f`, `png_matrice_typologie()` | « SPEC-TYPO-040 — chaîne réelle en mode département … » | les sept fichiers |
| SPEC-TYPO-042 | proposer des seuils fixes arrondis à partir des données, sans rien appliquer | `proposer_seuils_typologie()`, `arrondir_pas()` | « SPEC-TYPO-042 — proposer_seuils_typologie : terciles arrondis au pas … » | bloc console à coller dans `00_config.R` |
| SPEC-TYPO-041 | contrôles bloquants (total altéré, profil inconnu, colonne interdite = arrêt) | `controler_typologie()` | « SPEC-TYPO-001/002/004/041 … » ; « SPEC-TYPO-040 … » | — |

## Vue par exigence (règles structurantes)

| Identifiant | Règle | Fonction / script | Test (titre exact) |
|---|---|---|---|
| SPEC-CHAMP-004 | `AGE_MIN_BTS` source unique | `00_config.R` | `test-age-min.R` « même code, trois exécutions : seule la ligne AGE_MIN_BTS diffère du dépôt » |
| SPEC-CHAMP-005 | âge incohérent refusé | `00_config.R`, `01` | « configuration : valeurs refusées avec un message explicite » ; « mode test : AGE_MAX_TEST <= AGE_MIN_BTS refusé par le 01 lui-même » |
| SPEC-PREP-002 | codes géographiques en texte | `normaliser_geo()` | `test-geo.R` « codes : toujours en texte, « 01 », « 2A », « 2B », « 33 » intacts » |
| SPEC-PREP-006 | PCS hors champ tracées | `01b`, `08c` | `test-departs-pcs.R` « fichiers : … pcs_hors_champ agrégée avec total = exclusions du 01b » |
| SPEC-PREP-013 | département sans région dans la table de passage → « inconnu », tracé, sans arrêt | `01d_recoder_geo_hors_passage.R` | `test-recoder-geo-hors-passage.R` « SPEC-PREP-013 — le code « 99 » sans région est recodé « inconnu », compté, tracé ; ajouter_region ne s'arrête plus » |
| SPEC-PREP-014 | pyramide tous âges (01c) : tranches du champ = champ, total = stock tous âges, mode test déterministe, désactivable ; recodage 01d prudent | `01c_stock_tous_ages.R`, `01d_recoder_geo_hors_passage.R`, `indicateurs_secret_effectif()` | `test-typologie-pyramide.R` « SPEC-PREP-014 — 01c (mode test) … » ; « SPEC-PREP-014 / SPEC-SEC-033 — 01d … » |
| SPEC-PREP-008 | stock tous âges indisponible non bloquant | `01c`, `00d` | `test-fiches.R` « STOCK_TOUS_AGES = FALSE : chaîne inchangée, fiches en repli, CSV 08b sans colonnes tous âges » |
| SPEC-PROJ-009 | modèle inchangé par les évolutions de restitution | `04`, références figées | `test-snapshot.R` ; `test-format-restitution.R` « NON-RÉGRESSION numérique … » |
| SPEC-SEC-001 à 008 | règle primaire Insee, bornes | `secret_primaire()`, `indicateurs_secret()` | `test-fiches.R` « secret statistique, règle Insee BTS : < 5 salariés OU < 3 entreprises OU une entreprise > 85 %, puis secondaire » |
| SPEC-SEC-010 à 015 | secondaire : plus petite cellule, itération | `secret_secondaire()`, `appliquer_secret_pcs()` | `test-departs-pcs.R` « secret : règle Insee (primaire) + secondaire minimale sur la diffusion … » |
| SPEC-SEC-016 à 022 | blocs par niveau et dimension | `blocs_secret()` | `test-departs-cs1.R` « H. secret : MÊME implémentation que departs_pcs … » |
| SPEC-SEC-023 à 027 | interne complet / diffusion masquée, contrôle final | `08c`, `08d` | `test-departs-pcs.R` « fichiers : interne/ toujours, diffusion/ seulement avec DEPARTS_PCS_SECRET » |
| SPEC-SEC-031 | aucune valeur masquée dans une page | `preparer_carte_departs()`, `donnees_json_carte()` | `test-cartes-departs.R` « SECRET : aucune valeur masquée dans la structure, le JSON, le HTML ni les infobulles ; table interne refusée » |
| SPEC-SEC-033 | cellules de la pyramide des âges : primaire + secondaire par département, pyramide entière masquée, regroupement 01d prudent | `appliquer_secret_typologie()`, `01d` | `test-typologie-pyramide.R` « SPEC-TYPO-095 / SPEC-SEC-033 — secret … » ; « SPEC-PREP-014 / SPEC-SEC-033 — 01d … » |
| SPEC-SEC-034 | agrégat d'une liste de PCS : parts A / B / C à la règle primaire, aucune part avec exactement une cellule PCS masquée, pourcentages et départs attendus de la part A seulement (jamais les effectifs des parts) | `appliquer_secret_exposition()`, `controler_parts_secret()` | `test-exposition-tension.R` « SPEC-TYPO-106 / SPEC-SEC-034 — secret … » |
| SPEC-SEC-035 | tables de diffusion complétées sur l'univers des territoires (référentiel, passage région ; GEO_INTERET) x catégories observées en France : lignes masquées, NA, après le secret secondaire, tables internes intactes ; sans référentiel, message | `univers_territoires()` (00c), `completer_univers_diffusion()` (00g), `appliquer_secret_pcs()`, `appliquer_secret_typologie()`, `appliquer_secret_exposition()`, 08, 08b | `test-departs-pcs.R` « SPEC-SEC-035 … » ; `test-departs-cs1.R` « SPEC-SEC-035 … » ; `test-departs-geo-cs.R` « 3. tous les territoires … » ; `test-typologie-territoriale.R` « SPEC-TYPO-040 … » ; `test-exposition-tension.R` « SPEC-TYPO-107 — 08i sur la chaîne … » | `departs_*/diffusion/*.csv`, `departs_par_<zonage>_cs.csv`, `criticite_55plus_<zonage>_cs.csv`, `typologie_territoriale/diffusion/*.csv` |
| SPEC-GEO-012 | synthèse 08 par territoire, tableau des seniors et quadrant construits sur une version diffusable : territoire = cellule (primaire + secondaire, bloc = univers), bloc des seniors à part (masque_55plus), univers à zéro, total exact conservé, arrêt si une cellule sous le seuil reste visible ; synthese_geo exact inchangé | `08_analyse_55plus_geo.R` (synthese_geo_diffusable), `masquer_cellules()`, `indicateurs_secret()`, `univers_territoires()` | `test-departs-geo-cs.R` « SPEC-GEO-012 … » ; `test-format-restitution.R` | `analyse_55plus_par_<zonage>.csv`, `tableau_departs_55plus_<zonage>.csv/.html`, `quadrant_55plus_<zonage>.png` |
| SPEC-REST-001, 002, 005 | demi vers le haut, NA jamais 0 | `arrondir_nombre_personnes()` | `test-format-restitution.R` « arrondir_nombre_personnes : cas limites documentés, demi vers le haut, NA jamais 0 » |
| SPEC-REST-006 | additivité total / composantes | `arrondir_composantes_avec_total()` | « arrondir_composantes_avec_total : la somme affichée des composantes = le total affiché » |
| SPEC-GEO-001 | France jamais restreinte | `preparer_base_departs()` | `test-departs-pcs.R` « France entière : indépendante de GEO_INTERET et de toute sélection de restitution » |
| SPEC-GEO-003 | maille incompatible ignorée avec message | `preparer_base_departs()` | « zonage zone d'emploi : région et département ignorés avec message, France seule » |
| SPEC-GEO-004 | contrôles exacts par niveau | `controler_departs()` | « cohérence arithmétique (avant arrondi, avant secret) : France, Σ régions, Σ départements, Σ départements d'une région » |
| SPEC-GEO-005 | PCS regroupées = grande CS | `controler_cs1_vs_pcs()` | `test-departs-cs1.R` « B. CONTRÔLE FONDAMENTAL PCS -> cs1 … » |
| SPEC-PUB-006 | libellés dérivés de la configuration | `construire_libelles_public()` | `test-cartes-departs.R` « CHAMP : source unique construite depuis la configuration … » |
| SPEC-PUB-007 | absence de donnée = secret : territoire ou cellule absents de la table rendus comme masqués (gris, contour, libellé, comptage), aucune entrée « Aucun salarié observé » / « absent » dans les cartes 08e, 08g, 08h ; garde-fou sur R/ | `preparer_carte_departs()`, `vue_initiale_carte()`, `preparer_carte_typologie()`, `preparer_regions_typologie()` | `test-cartes-departs.R` « SPEC-PUB-007 — absence de donnée = secret … » (garde-fou sur R/ inclus) ; `test-carte-typologie.R` « SPEC-TYPO-060 … », « SPEC-TYPO-061/062 … », « SPEC-TYPO-065 — stabilité … » ; `test-cartes-typologie-pcs.R` « SPEC-TYPO-082 … », « SPEC-TYPO-084 … » | `cartes/**` |
| SPEC-PUB-009 | frontières lisibles, source unique | `STYLE_CONTOURS_CARTE` | « CONTOURS : source unique STYLE_CONTOURS_CARTE, trait gris (jamais blanc) … » |
| SPEC-PUB-021 | fiche : bloc des seniors (nombre, part, départs, taux) sous la règle primaire de sa cellule ; masqué = NA + « non diffusé (secret statistique) », jamais « aucun » ; secret secondaire entre tranches senior et entre tranches non senior | `calculer_indicateurs_territoire()`, `phrases_a_retenir()`, `generer_html_fiche()` | `test-fiches.R` « SPEC-PUB-021 … » | `fiches_<zonage>/*.html`, `journal` |
| SPEC-PUB-022 | chaque page HTML vit seule : aucun hyperlien vers un autre fichier, seules les ancres internes (`href="#…"`) ; index des fiches avec nom de fichier en texte ; garde-fou sur R/ | `generer_html_index()`, `generer_fiche_exposition_tension()`, `generer_fiche_metiers_liste()` | `test-fiches.R` « SPEC-PUB-022 … » (garde-fou sur R/ inclus) ; `test-metiers-liste.R` « SPEC-TYPO-109 — fiche … » | `fiches_<zonage>/index.html`, `typologie_territoriale/diffusion/*.html` |
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
