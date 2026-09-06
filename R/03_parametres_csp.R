# ==============================================================================
# 03_parametres_csp.R — Paramètres de sortie de l'emploi par CSP
# ------------------------------------------------------------------------------
# PRÉREQUIS : objets `fdc` et `fdc_salaries` (script 02), ANNEES_LISSAGE (00)
# PRODUIT   : objets `param_cs` (une ligne par CSP salariée) et
#             `param_ensemble` (ancrage pour le futur correctif δ)
# Concepts : age_conj = âge conjoncturel de départ (génération fictive) ;
#            d_horsempl = « sas » (années sans emploi ni retraite après 50 ans) ;
#            mu_sortie = age_conj - d_horsempl = âge moyen de SORTIE DE L'EMPLOI.
# ==============================================================================
if (!exists("fdc_salaries")) stop("Objet 'fdc_salaries' introuvable : exécutez R/02 (ou main.R).")

library(dplyr)
library(tidyr)

# Stabilité de la série (choix de la fenêtre de lissage ; Covid en 2020) :
print(fdc_salaries |> select(annee, csp, age_conj) |>
        pivot_wider(names_from = annee, values_from = age_conj, names_sort = TRUE))

param_cs <- fdc_salaries |>
  filter(annee %in% ANNEES_LISSAGE) |>
  group_by(csp) |>
  summarise(age_conj   = mean(age_conj,   na.rm = TRUE),
            d_emploi   = mean(d_emploi,   na.rm = TRUE),
            d_horsempl = mean(d_horsempl, na.rm = TRUE), .groups = "drop") |>
  mutate(verif_identite = round(50 + d_emploi + d_horsempl - age_conj, 2),
         mu_sortie      = age_conj - d_horsempl)
print(param_cs)
# Lecture attendue : age_conj plus élevé chez les cadres, d_horsempl plus long
# chez employés/ouvriers, mu_sortie cadres > ouvriers de 2 à 3 ans.

param_ensemble <- fdc |>
  # ligne d'agrégat : "9 - Toutes CSP confondues" (code 9 = hors classification)
  filter(grepl("^9", csp), annee %in% ANNEES_LISSAGE) |>
  summarise(age_conj_ensemble = mean(age_conj, na.rm = TRUE))
if (!is.finite(param_ensemble$age_conj_ensemble))
  stop("Ligne d'ensemble (code 9) introuvable : inspectez fdc |> distinct(csp).")
print(param_ensemble)
# Série pré-réforme (s'arrête en 2020) ; âge conjoncturel 2024 au régime
# général ~63a7m -> justifie le correctif réglementaire δ (script 04).
message("03 OK -> objets param_cs, param_ensemble")
