# ==============================================================================
# utils/utils_departs_par_cs.R — Nombre de départs attendus d'ici 2030 par CS
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (exécuter R/04 ou main.R au préalable)
# PRODUIT   : objet `departs_par_cs` + affichage console
# Principe  : départs = somme des probabilités individuelles (linéarité de
#             l'espérance, comme le script 05) ; fourchette = scénarios δ.
# Usage     : source("utils/utils_departs_par_cs.R")
# ==============================================================================

if (!exists("bts_projete")) {
  stop(
    "Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R)."
  )
}

library(dplyr)

departs_par_cs <- bts_projete |>
  group_by(cs1) |>
  summarise(
    effectif      = n(),
    departs       = round(sum(p_central)),
    departs_bas   = round(sum(p_bas)),
    departs_haut  = round(sum(p_haut)),
    taux_pct      = round(100 * mean(p_central), 1),
    .groups = "drop"
  ) |>
  arrange(desc(departs)) |>
  bind_rows(
    bts_projete |>
      summarise(
        cs1          = "Ensemble",
        effectif     = n(),
        departs      = round(sum(p_central)),
        departs_bas  = round(sum(p_bas)),
        departs_haut = round(sum(p_haut)),
        taux_pct     = round(100 * mean(p_central), 1)
      )
  )

cat("\n--- Départs attendus d'ici 2030 par CS (scénario central, fourchette δ) ---\n")
print(as.data.frame(departs_par_cs), row.names = FALSE)
