# ==============================================================================
# 06_graphique_repartition.R — Répartition par tranche d'âge (deux modes)
# ------------------------------------------------------------------------------
# PRÉREQUIS : objet `bts_projete` (04) ; paramètres de restitution (00)
# PRODUIT   : objet `repartition` ; sorties/repartition_departs_par_age.png
# MODES (MODE_GRAPHIQUE, 00_config) :
#   "attendu" : parts ESPÉRÉES par cause dans chaque tranche — maintien /
#               sortie hors calendrier (invalidité, décès) / départ retraite-
#               fin de carrière. Répond à « quelle part de la tranche sort ? ».
#   "classes" : classement des INDIVIDUS par seuils de probabilité — répond à
#               « qui est individuellement quasi certain de partir ? ».
# Les décomptes officiels restent la somme des probabilités continues (05).
# ==============================================================================
if (!exists("bts_projete")) stop("Objet 'bts_projete' introuvable : exécutez R/04 (ou main.R).")

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

decalage <- ANNEE_REF_GRAPHIQUE - 2024
base_g <- bts_projete |>
  mutate(age_affiche = age_2024 + decalage,
         tranche = cut(age_affiche, breaks = BREAKS_TRANCHES,
                       labels = LABELS_TRANCHES))

if (MODE_GRAPHIQUE == "attendu") {
  # 4 postes qui somment à 1. On répartit les risques concurrents en parts
  # espérées additives : chaque sortie est pondérée pour que retraite,
  # invalidité et décès ne se chevauchent pas (produit des survies).
  niveaux <- c("Maintien en emploi",
               "Sortie de l'emploi (retraite ou sas)",
               "Sortie invalidité",
               "Décès")
  repartition <- base_g |>
    mutate(
      # part retraite : liquidation calendaire, pondérée par survie aux 2 risques
      `Sortie de l'emploi (retraite ou sas)` = p_cal_central * (1 - p_inval) * (1 - p_deces),
      # part invalidité : devient invalide sans être déjà parti en retraite ni décédé
      `Sortie invalidité`                 = p_inval * (1 - p_cal_central) * (1 - p_deces),
      # part décès : décède sans être déjà parti ni invalide
      `Décès`                             = p_deces * (1 - p_cal_central) * (1 - p_inval),
      # maintien : le complément
      `Maintien en emploi` = 1 - (`Sortie de l'emploi (retraite ou sas)` +
                                  `Sortie invalidité` + `Décès`)
    ) |>
    group_by(tranche) |>
    summarise(across(all_of(niveaux), mean), .groups = "drop") |>
    pivot_longer(-tranche, names_to = "classe", values_to = "part") |>
    mutate(classe = factor(classe, levels = niveaux))
  couleurs <- c("Sortie de l'emploi (retraite ou sas)" = "#2f6da4",
                "Sortie invalidité"                 = "#d69a3c",
                "Décès"                             = "#7a5195",
                "Maintien en emploi"                = "#59a14f")
  sous_titre <- sprintf("Parts espérées par cause — scénario central (δ = %.2f an) ; invalidité en flux propre par CSP × âge",
                        delta_central)
} else {
  niveaux <- c("Aucun départ",
               "Départ probable / envisageable",
               "Départ certain d'ici 2030")
  repartition <- base_g |>
    mutate(classe = cut(p_central,
                        breaks = c(-Inf, SEUIL_PROBABLE, SEUIL_CERTAIN, Inf),
                        labels = niveaux)) |>
    count(tranche, classe) |>
    group_by(tranche) |> mutate(part = n / sum(n)) |> ungroup()
  couleurs <- c("Départ certain d'ici 2030"      = "#2f6da4",
                "Départ probable / envisageable" = "#d69a3c",
                "Aucun départ"                   = "#59a14f")
  sous_titre <- sprintf("Classes individuelles — « certain » p ≥ %.0f %%, « probable » %.0f-%.0f %% (δ = %.2f an)",
                        100 * SEUIL_CERTAIN, 100 * SEUIL_PROBABLE,
                        100 * SEUIL_CERTAIN, delta_central)
}
print(repartition |> mutate(part = round(100 * part, 1)))

g <- repartition |>
  ggplot(aes(x = tranche, y = part, fill = classe)) +
  geom_col(position = "fill", width = 0.62) +
  geom_text(data = ~ filter(.x, part >= 0.025),
            aes(label = percent(part, accuracy = 1)),
            position = position_fill(vjust = 0.5), color = "white", size = 3.6) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = couleurs) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = sprintf("Répartition des salariés de 43 ans et + (en %d)\nselon leur situation attendue à 2030",
                       ANNEE_REF_GRAPHIQUE),
       subtitle = sous_titre,
       caption  = "Sources : DREES, jeu departretraite_parcsp (Insee, enquête Emploi) ; quotients de mortalité Insee — calculs propres. Données individuelles : table test.",
       x = sprintf("Tranche d'âge en %d", ANNEE_REF_GRAPHIQUE),
       y = "Proportion", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 13.5, hjust = 0),
        plot.subtitle = element_text(color = "grey40", size = 9.5),
        plot.caption  = element_text(color = "grey60", size = 8),
        legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_line(linetype = "dashed", color = "grey82"),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
ggsave(file.path(DIR_SORTIES, "repartition_departs_par_age.png"),
       g, width = 10, height = 6.2, dpi = 300)
message("06 OK (mode ", MODE_GRAPHIQUE, ") -> sorties/repartition_departs_par_age.png")
