# ==============================================================================
# utils/utils_parquet.R — Inspecter le schéma d'une extraction BTS AVANT de la
# brancher : colonnes, types Arrow, exemples de valeurs.
# ------------------------------------------------------------------------------
# Usage (racine du projet) :
#   source("R/00_config.R"); source("R/00c_fonctions_geo.R")
#   source("utils/utils_parquet.R")            # inspecte FICHIER_BTS
#   inspecter_schema("chemin/vers/autre.parquet")
# Puis renseigner COL_BTS / COL_GEO / GEO_SOURCE / GEO_ANALYSE dans 00_config.R.
# Aucun choix automatique : inspection -> configuration explicite -> traitement.
# ==============================================================================
if (!exists("inspecter_schema")) {
  source("R/00_config.R"); source("R/00c_fonctions_geo.R")
}
schema_bts <- inspecter_schema(FICHIER_BTS)
