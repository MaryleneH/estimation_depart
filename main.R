# ==============================================================================
# DÉPARTS DES SALARIÉS 43+ À L'HORIZON 2030 — ORCHESTRATEUR
# ------------------------------------------------------------------------------
# Exécute la chaîne dans l'ordre ; les scripts communiquent par les OBJETS de
# la session (pas d'intermédiaires sur disque). À lancer depuis la racine du
# projet, de préférence dans une session R FRAÎCHE (Ctrl+Maj+F10 dans RStudio)
# plutôt qu'avec rm(list = ls()) : c'est la seule vraie remise à zéro.
# ==============================================================================
source("R/00_config.R")
source("R/01_fabriquer_donnees_test.R")   # -> objet : bts
source("R/01b_agreger_pcs.R")             # -> cs1 agrégée (si AGGREGER_PCS)
source("R/02_importer_nettoyer_drees.R")  # -> objets : fdc, fdc_salaries
source("R/02b_importer_mortalite_insee.R")# -> objet : table_mortalite
source("R/02c_importer_invalidite_eacr.R")# -> objet : inval_base
source("R/03_parametres_csp.R")           # -> objets : param_cs, param_ensemble
source("R/04_projection_2030.R")          # -> objet : bts_projete
source("R/05_resultats_entreprises.R")    # -> sorties/ (csv + png)  [facultatif]
source("R/06_graphique_repartition.R")    # -> sorties/repartition_departs_par_age.png
source("R/06b_graphique_repartition_cs.R")# -> sorties/repartition_par_cs.png (design par CS)
source("R/07_tableau_contribution.R")     # -> sorties/tableau_contribution.html (+ .png)
message("Chaîne exécutée.")
