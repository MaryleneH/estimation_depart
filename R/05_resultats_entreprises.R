# ==============================================================================
# 05_resultats_entreprises.R — Agrégation par entreprise, fourchette, exports
# ------------------------------------------------------------------------------
# PRÉREQUIS : objets `bts_projete`, `delta_bas`, `delta_haut` (script 04) ;
#             DIR_SORTIES (00)
# PRODUIT   : objet `resultats_entreprises` ;
#             sorties/departs_2030_par_entreprise.csv (format tableur FR) ;
#             sorties/departs_2030_par_entreprise.png
# Principe  : le nombre de départs attendu d'une entreprise est la SOMME des
#             probabilités individuelles (linéarité de l'espérance) — pas
#             besoin de simulation pour l'espérance ; la fourchette vient des
#             scénarios réglementaires δ, pas de l'aléa.
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")

library(dplyr)
library(ggplot2)

resultats_entreprises <- bts_projete |>
  group_by(siren) |>
  summarise(effectif_43plus_2024 = n(),
            departs_2030 = sum(p_central),
            departs_bas  = sum(p_bas),
            departs_haut = sum(p_haut),
            .groups = "drop") |>
  mutate(part_departs_pct = 100 * departs_2030 / effectif_43plus_2024) |>
  arrange(desc(part_departs_pct))

total <- resultats_entreprises |>
  summarise(across(c(effectif_43plus_2024, departs_2030,
                     departs_bas, departs_haut), sum))

print(resultats_entreprises |> mutate(across(where(is.numeric), ~ round(.x, 1))))
cat("--- Total périmètre ---\n")
print(total |> mutate(across(everything(), round)))

# Export tableur FR (';' + décimale ',') — arrondis pour la diffusion
write.csv2(resultats_entreprises |> mutate(across(where(is.numeric), ~ round(.x, 1))),
           file.path(DIR_SORTIES, "departs_2030_par_entreprise.csv"),
           row.names = FALSE)

# Graphique : départs attendus par entreprise + fourchette réglementaire
g <- resultats_entreprises |>
  ggplot(aes(x = reorder(siren, departs_2030), y = departs_2030)) +
  geom_col(fill = "#2c7fb8", width = 0.65) +
  geom_errorbar(aes(ymin = departs_bas, ymax = departs_haut),
                width = 0.2, linewidth = 0.6, color = "grey25") +
  coord_flip() +
  labs(title    = "Départs définitifs attendus d'ici 2030, par entreprise",
       subtitle = sprintf("Salariés de 43 ans et + en 2024 — scénario central ; fourchette réglementaire δ ∈ [%.2f ; %.2f] an",
                          delta_bas, delta_haut),
       caption  = "Sources : DREES, jeu departretraite_parcsp (Insee, enquête Emploi) — calculs propres. Données individuelles : table test.",
       x = NULL, y = "Nombre de départs attendus") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0),
        plot.subtitle = element_text(color = "grey40", size = 10),
        plot.caption = element_text(color = "grey60", size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
ggsave(file.path(DIR_SORTIES, "departs_2030_par_entreprise.png"),
       g, width = 9, height = 5.5, dpi = 300)
message("05 OK -> objet resultats_entreprises + exports dans ", DIR_SORTIES, "/")
