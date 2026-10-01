# ==============================================================================
# DÉPARTS DES SALARIÉS DU CHAMP (AGE_MIN_BTS ans et +, réglé dans 00_config.R) À L'HORIZON 2030 — ORCHESTRATEUR
# ------------------------------------------------------------------------------
# Exécute la chaîne dans l'ordre ; les scripts communiquent par les OBJETS de
# la session (pas d'intermédiaires sur disque). À lancer depuis la racine du
# projet, de préférence dans une session R FRAÎCHE (Ctrl+Maj+F10 dans RStudio)
# plutôt qu'avec rm(list = ls()) : c'est la seule vraie remise à zéro.
# ==============================================================================
source("R/00_config_fonctions/00_config.R")
source("R/00_config_fonctions/00c_fonctions_geo.R")           # -> fonctions géographiques génériques
source("R/00_config_fonctions/00d_fonctions_fiches.R")        # -> fonctions des fiches territoriales (09)
source("R/00_config_fonctions/00e_fonctions_departs_pcs.R")   # -> fonctions départs par PCS fine (08c)
source("R/00_config_fonctions/00f_fonctions_cartes.R")        # -> fonctions cartes des résultats diffusables (08e)
source("R/01_preparation/01_fabriquer_donnees_test.R")   # -> objet : bts (contrat geo_code/geo_nom/geo_type)
source("R/01_preparation/01b_agreger_pcs.R")             # -> cs1 agrégée (si AGGREGER_PCS)
source("R/01_preparation/01c_stock_tous_ages.R")         # -> stock_tous_ages : effectifs actuels TOUS ÂGES par
                                          #    territoire x CS (agrégation Arrow ; dénominateur des fiches)
source("R/01_preparation/02_importer_nettoyer_drees.R")  # -> objets : fdc, fdc_salaries
source("R/01_preparation/02b_importer_mortalite_insee.R")# -> objet : table_mortalite
source("R/01_preparation/02c_importer_invalidite_eacr.R")# -> objet : inval_base
source("R/03_modelisation/03_parametres_csp.R")           # -> objets : param_cs, param_ensemble
source("R/03_modelisation/04_projection_2030.R")          # -> objet : bts_projete
source("R/05_restitutions/05_resultats_entreprises.R")    # -> sorties/ (csv + png)  [facultatif]
source("R/05_restitutions/06_graphique_repartition.R")    # -> sorties/repartition_departs_par_age.png
source("R/05_restitutions/06b_graphique_repartition_cs.R")# -> sorties/repartition_par_cs.png (design par CS)
source("R/05_restitutions/06d_graphique_repartition_cs_regroupe.R") # -> sorties/repartition_par_cs_regroupe.png
                                          #    (06b avec invalidité + décès regroupés, note de lecture)
source("R/05_restitutions/06e_graphique_repartition_age_regroupe.R") # -> sorties/repartition_par_age_regroupe.png
                                          #    + repartition_55plus_regroupe.png (regroupé par âge)
if (requireNamespace("gt", quietly = TRUE)) {   # 07 = mise en forme gt, facultative :
  source("R/05_restitutions/07_tableau_contribution.R")   # -> sorties/tableau_contribution.html (+ .png)
} else message("07 IGNORÉ : package 'gt' absent (install.packages(\"gt\")) — ",
               "le même tableau sans gt est produit par R/07b (à sourcer à part).")
source("R/08_territoires/08_analyse_55plus_geo.R")       # -> sorties/analyse_55plus_par_<zonage>.csv,
                                          #    criticite_55plus_<zonage>_cs.csv,
                                          #    quadrant_55plus_<zonage>.png,
                                          #    tableau_departs_55plus_<zonage>.{csv,html}
                                          #    (<zonage> = GEO_ANALYSE : ze, departement...)
source("R/08_territoires/08b_departs_geo_cs.R")          # -> sorties/departs_par_<zonage>_cs.csv
                                          #    (territoire x grande CS, tout le champ, secret appliqué)
source("R/08_territoires/08c_departs_pcs.R")             # -> sorties/departs_pcs/{interne,diffusion}/departs_pcs_<niveau>.csv
                                          #    (PCS fine : France, région, département) + pcs_hors_champ.csv
source("R/08_territoires/08d_departs_cs1.R")             # -> sorties/departs_cs1/{interne,diffusion}/departs_cs1_<niveau>.csv
                                          #    (grande CS : mêmes mailles, même méthode, même secret que 08c)
source("R/08_territoires/08e_cartes_departs.R")          # -> sorties/cartes_departs/{pcs,cs1}/carte_<dim>_<niveau>.html (+ .png)
                                          #    (cartes des tables diffusion/ seules ; si GENERER_CARTES_DEPARTS)
source("R/08_territoires/09_fiches_territoriales.R")     # -> sorties/fiches_<zonage>/<code>_<nom>.html + index.html
                                          #    (si GENERER_FICHES ; mode tous / selection)
if (isTRUE(get0("NORMALISER_CSV_EXCEL", ifnotfound = FALSE)))
  source("R/99_controles/99b_normaliser_csv_excel.R")    # -> BOM UTF-8 sur tous les CSV de sorties/ (Excel),
                                          #    contenu inchangé ; objet controle_csv_excel
message("Chaîne exécutée.")
